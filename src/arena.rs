//! Arena-backed hashconsing: the counterpart of [`crate::HConsed`] / [`crate::HConsign`].
//!
//! Values live in a caller-owned [`bumpalo::Bump`] arena, and handles ([`BHConsed`]) are `Copy`:
//! a `&'bump T` plus a `u64` uid. Cloning a term is thus a register copy instead of an atomic
//! increment, and the consign ([`BHConsign`]) needs **no `T: Clone`** bound — the `Arc` consign
//! clones each element to use it as its table key, this one keys the table on the interned
//! reference itself.
//!
//! # Destructors never run
//!
//! Values are bump-allocated with [`bumpalo::Bump::alloc`], which never runs destructors, so
//! **`T`'s destructor never runs**. Any heap owned by `T` (`String`, `Vec<_>`, ...) is leaked for
//! as long as the arena lives.
//!
//! This is forced, not incidental: handles are `Copy` and carry the arena lifetime `'bump`, so no
//! owner could drop a value earlier without leaving handles dangling. Prefer arena-friendly
//! payloads: allocate `&'bump str` / `&'bump [T]` in the same arena, reachable through
//! [`BHConsign::arena`]. If you genuinely need destructors, use the `Arc`-backed
//! [`crate::HConsign`] instead.
//!
//! # Differences with the `Arc` path
//!
//! - no weak references, hence no `collect`/`collect_to_fit`: arena memory is reclaimed only when
//!   the [`bumpalo::Bump`] is dropped or reset;
//! - no [`crate::consign!`] analogue: [`bumpalo::Bump`] is `Send` but **not** `Sync`, so a lazy
//!   static arena consign is impossible. [`BHConsign`] is `!Send`; individual [`BHConsed`]
//!   handles are `Send + Sync` whenever `T: Sync`;
//! - [`crate::hash_coll`]'s `HConSet`/`HConMap` are specific to [`crate::HConsed`] and do not
//!   accept [`BHConsed`]. Use `std`'s `HashSet`/`HashMap`/`BTreeSet`/`BTreeMap` directly.
//!   [`BHConsed::hash`](BHConsed#impl-Hash-for-BHConsed<'_,+T>) writes exactly one `u64`, so this
//!   crate's fast hasher applies:
//!   `HashSet<Term, hashconsing::hash_coll::hashers::p_hash::Builder>`.
//!
//! # Examples
//!
//! Lambda calculus, mirroring the crate-level example. Note that the term type does **not** derive
//! `Clone`, and that handles are copied around without any `.clone()`.
//!
//! ```rust
//! use hashconsing::arena::{bumpalo::Bump, BHConsed, BHConsign};
//!
//! type Term<'b> = BHConsed<'b, ActualTerm<'b>>;
//!
//! #[derive(Debug, Hash, PartialEq, Eq)]
//! enum ActualTerm<'b> {
//!     Var(usize),
//!     Lam(Term<'b>),
//!     App(Term<'b>, Term<'b>),
//! }
//! use ActualTerm::*;
//!
//! let arena = Bump::new();
//! let mut factory: BHConsign<'_, ActualTerm<'_>> = BHConsign::new(&arena);
//! assert_eq!(factory.len(), 0);
//!
//! let v = factory.mk(Var(0));
//! assert_eq!(factory.len(), 1);
//!
//! let v2 = factory.mk(Var(3));
//! assert_eq!(factory.len(), 2);
//!
//! let lam = factory.mk(Lam(v2));
//! assert_eq!(factory.len(), 3);
//!
//! let v3 = factory.mk(Var(3));
//! // `v2` and `v3` are the same term: nothing new was allocated.
//! assert_eq!(factory.len(), 3);
//! assert_eq!(v2.uid(), v3.uid());
//! assert_eq!(v2, v3);
//! assert!(std::ptr::eq(v2.get(), v3.get()));
//!
//! let lam2 = factory.mk(Lam(v3));
//! assert_eq!(factory.len(), 3);
//! assert_eq!(lam, lam2);
//!
//! let app = factory.mk(App(lam2, v));
//! assert_eq!(factory.len(), 4);
//! ```
//!
//! Sets of handles, using this crate's uid-keyed hasher:
//!
//! ```rust
//! use std::collections::HashSet;
//! use hashconsing::{arena::{bumpalo::Bump, BHConsed, BHConsign}, hash_coll::hashers::p_hash};
//!
//! #[derive(Hash, PartialEq, Eq)]
//! struct Var(usize);
//!
//! let arena = Bump::new();
//! let mut factory: BHConsign<'_, Var> = BHConsign::new(&arena);
//! let a = factory.mk(Var(0));
//! let b = factory.mk(Var(0));
//!
//! let mut set: HashSet<BHConsed<'_, Var>, p_hash::Builder> = HashSet::default();
//! assert!(set.insert(a));
//! assert!(!set.insert(b));
//! assert_eq!(set.len(), 1);
//! ```

use std::{
    borrow::Borrow,
    cmp::Ordering,
    collections::{hash_map::RandomState, HashMap},
    fmt,
    hash::{BuildHasher, Hash, Hasher},
    ops::Deref,
};

use bumpalo::Bump;

use crate::HashConsed;

pub use bumpalo;

/// A hashconsed value allocated in a [`Bump`] arena.
///
/// This is a `Copy` handle: cloning it copies a reference and a uid, no atomic operation is
/// involved. There is no weak-reference counterpart, since the arena — not the handles — owns the
/// value.
pub struct BHConsed<'bump, T> {
    /// The actual element, allocated in the arena.
    elm: &'bump T,
    /// Unique identifier of the element.
    uid: u64,
}
impl<T> HashConsed for BHConsed<'_, T> {
    type Inner = T;
}

impl<'bump, T> BHConsed<'bump, T> {
    /// The inner element. Can also be accessed *via* dereferencing.
    ///
    /// The result borrows the arena, not `self`: interned values outlive any handle borrow.
    #[inline]
    pub fn get(&self) -> &'bump T {
        self.elm
    }
    /// The unique identifier of the element.
    #[inline]
    pub fn uid(&self) -> u64 {
        self.uid
    }
}

impl<T> Clone for BHConsed<'_, T> {
    fn clone(&self) -> Self {
        *self
    }
}
impl<T> Copy for BHConsed<'_, T> {}

impl<T> PartialEq for BHConsed<'_, T> {
    #[inline]
    fn eq(&self, rhs: &Self) -> bool {
        self.uid == rhs.uid
    }
}
impl<T> Eq for BHConsed<'_, T> {}
impl<T> PartialOrd for BHConsed<'_, T> {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
impl<T> Ord for BHConsed<'_, T> {
    #[inline]
    fn cmp(&self, other: &Self) -> Ordering {
        self.uid.cmp(&other.uid)
    }
}
impl<T> Hash for BHConsed<'_, T> {
    #[inline]
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.uid.hash(state)
    }
}

impl<T> Deref for BHConsed<'_, T> {
    type Target = T;
    #[inline]
    fn deref(&self) -> &T {
        self.elm
    }
}
impl<T> Borrow<T> for BHConsed<'_, T> {
    fn borrow(&self) -> &T {
        self.elm
    }
}

impl<T: fmt::Debug> fmt::Debug for BHConsed<'_, T> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "{:?}", self.elm)
    }
}
impl<T: fmt::Display> fmt::Display for BHConsed<'_, T> {
    #[inline]
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        self.elm.fmt(fmt)
    }
}

/// The consign storing arena-allocated hashconsed elements.
///
/// Elements are allocated in the [`Bump`] the consign was created with, and never freed before
/// that arena is dropped or reset. See the [module-level documentation](self) for the destructor
/// caveat.
pub struct BHConsign<'bump, T: Hash + Eq, S = RandomState> {
    /// Arena the elements are allocated in.
    arena: &'bump Bump,
    /// Maps interned elements to their uid. Keys point into `arena`.
    table: HashMap<&'bump T, u64, S>,
    /// Counter for uids.
    count: u64,
}

impl<'bump, T: Hash + Eq> BHConsign<'bump, T, RandomState> {
    /// Creates an empty consign over `arena`.
    #[inline]
    pub fn new(arena: &'bump Bump) -> Self {
        BHConsign {
            arena,
            table: HashMap::new(),
            count: 0,
        }
    }

    /// Creates an empty consign over `arena`, with a capacity.
    #[inline]
    pub fn with_capacity(arena: &'bump Bump, capacity: usize) -> Self {
        BHConsign {
            arena,
            table: HashMap::with_capacity(capacity),
            count: 0,
        }
    }
}

impl<'bump, T: Hash + Eq, S> BHConsign<'bump, T, S> {
    /// The arena the elements are allocated in.
    ///
    /// Handy to allocate arena-friendly payloads (`&'bump str`, `&'bump [_]`, ...) for the values
    /// about to be interned.
    #[inline]
    pub fn arena(&self) -> &'bump Bump {
        self.arena
    }

    /// The number of elements stored.
    #[inline]
    pub fn len(&self) -> usize {
        self.table.len()
    }

    /// True if the consign is empty.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.table.is_empty()
    }

    /// Capacity of the underlying lookup table.
    #[inline]
    pub fn capacity(&self) -> usize {
        self.table.capacity()
    }

    /// Iterator over the interned elements.
    #[inline]
    pub fn iter(&self) -> impl ExactSizeIterator<Item = &'bump T> + '_ {
        self.table.keys().copied()
    }

    /// Iterator over the interned elements, as hashconsed handles.
    #[inline]
    pub fn consed_iter(&self) -> impl ExactSizeIterator<Item = BHConsed<'bump, T>> + '_ {
        self.table.iter().map(|(elm, uid)| BHConsed {
            elm: *elm,
            uid: *uid,
        })
    }
}

impl<'bump, T: Hash + Eq, S: BuildHasher> BHConsign<'bump, T, S> {
    /// Creates an empty consign over `arena`, with a custom hash.
    #[inline]
    pub fn with_hasher(arena: &'bump Bump, build_hasher: S) -> Self {
        BHConsign {
            arena,
            table: HashMap::with_hasher(build_hasher),
            count: 0,
        }
    }

    /// Creates an empty consign over `arena`, with a capacity and a custom hash.
    #[inline]
    pub fn with_capacity_and_hasher(arena: &'bump Bump, capacity: usize, build_hasher: S) -> Self {
        BHConsign {
            arena,
            table: HashMap::with_capacity_and_hasher(capacity, build_hasher),
            count: 0,
        }
    }

    /// Hashconses `elm` and returns the hashconsed version.
    ///
    /// The boolean is `true` iff `elm` was not in the consign, meaning it was just allocated in
    /// the arena.
    #[inline]
    pub fn mk_is_new(&mut self, elm: T) -> (BHConsed<'bump, T>, bool) {
        if let Some((elm, uid)) = self.table.get_key_value(&elm) {
            return (
                BHConsed {
                    elm: *elm,
                    uid: *uid,
                },
                false,
            );
        }
        // The arena, not the consign, owns the value, so handles can carry `'bump`. `T`'s
        // destructor never runs, see module doc.
        let elm: &'bump T = self.arena.alloc(elm);
        let uid = self.count;
        self.count += 1;
        self.table.insert(elm, uid);
        (BHConsed { elm, uid }, true)
    }

    /// Creates a hashconsed element.
    #[inline]
    pub fn mk(&mut self, elm: T) -> BHConsed<'bump, T> {
        self.mk_is_new(elm).0
    }

    /// True if the consign contains `elm`.
    #[inline]
    pub fn contains(&self, elm: &T) -> bool {
        self.table.contains_key(elm)
    }

    /// Reserves capacity for at least `additional` more elements.
    ///
    /// Only affects the lookup table; the arena is untouched.
    #[inline]
    pub fn reserve(&mut self, additional: usize) {
        self.table.reserve(additional)
    }

    /// Shrinks the capacity of the lookup table as much as possible.
    ///
    /// Only affects the lookup table; arena memory is never reclaimed this way.
    #[inline]
    pub fn shrink_to_fit(&mut self) {
        self.table.shrink_to_fit()
    }
}

impl<T: Hash + Eq + fmt::Display, S> fmt::Display for BHConsign<'_, T, S> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "consign:")?;
        for e in self.table.keys() {
            write!(fmt, "\n  | {e}")?;
        }
        Ok(())
    }
}
