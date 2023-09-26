
pub trait Arc<T, W>: std::ops::Deref<Target=T> + core::borrow::Borrow<T> + Clone
{
	fn downgrade(&self) -> W;
	fn strong_count(&self) -> usize;
}
pub trait Weak<T, S>: Clone
{
	fn upgrade(&self) -> Option<S>;
	fn strong_count(&self) -> usize;
}

impl<T> Arc<T, std::sync::Weak<T>> for std::sync::Arc<T>
{
	fn downgrade(&self) -> std::sync::Weak<T>
	{
		std::sync::Arc::downgrade(&self)
	}
	fn strong_count(&self) -> usize {
		std::sync::Arc::strong_count(&self)
	}
}
impl<T> Weak<T, std::sync::Arc<T>> for std::sync::Weak<T>
{
	fn upgrade(&self) -> Option<std::sync::Arc<T>>
	{
		std::sync::Weak::upgrade(&self)
	}
	fn strong_count(&self) -> usize {
		std::sync::Weak::strong_count(&self)
	}
}


pub trait Allocator<T>: Default
{
	/// Strong `std::sync::Arc`-like pointer
	type Strong: Arc<T, Self::Weak>;

	/// `std::sync::Weak`-like pointer
	type Weak: Weak<T, Self::Strong>;

	fn alloc(&self, data: T) -> Self::Strong;
}

#[derive(Default)]
pub struct DefaultAllocator {}

impl<T> Allocator<T> for DefaultAllocator
{
	type Strong = std::sync::Arc<T>;
	type Weak = std::sync::Weak<T>;

	fn alloc(&self, data: T) -> Self::Strong
	{
		Self::Strong::new(data)
	}
}

/*
#[cfg(feature = "shared_arena")]
use shared_arena::ArenaArc;

#[cfg(feature = "shared_arena")]
impl<T> Allocator<T> for shared_arena::SharedArena<T>
{
	type P = ArenaArc<T>;

	fn alloc(data: T) -> Self::P
	{
		self.alloc_arc(data)
	}
}
*/
