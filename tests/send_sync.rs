//! Makes sure hconsed elements can be sent/shared across threads.

use hashconsing::{HConsed, HConsign, HashConsign};

use rayon::prelude::*;

type Tree<T> = HConsed<RawTree<T>>;

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
enum RawTree<T> {
    Node(Tree<T>, T, Tree<T>),
    Leaf(T),
}

#[test]
fn rayon() {
    let mut consign = HConsign::empty();
    macro_rules! tree {
        ($val:expr) => {
            consign.mk(RawTree::Leaf($val))
        };
        ($lft: expr, $val:expr, $rgt:expr) => {
            consign.mk(RawTree::Node($lft.clone(), $val, $rgt.clone()))
        };
    }

    let t_7 = tree!(7);
    let t_5 = tree!(5);
    let t_3 = tree!(3);
    let t_2 = tree!(2);
    let t_1 = tree!(1);

    let t_10 = tree!(t_1, 3, t_2);
    let t_11 = tree!(t_10, 6, t_3);
    let t_12 = tree!(t_11, 11, t_5);
    let t_13 = tree!(t_12, 18, t_7);

    let forest = vec![t_7, t_5, t_3, t_2, t_1, t_10, t_11, t_12, t_13];
    let verbose = false;

    forest
        .par_iter()
        .map(|tree| {
            if verbose {
                println!("{:?}", tree)
            }
        })
        .collect::<()>();

    forest
        .into_par_iter()
        .map(|tree| {
            if verbose {
                println!("{:?}", tree)
            }
        })
        .collect::<()>();
}
