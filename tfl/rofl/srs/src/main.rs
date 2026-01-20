#![allow(dead_code)]

mod srs;
mod utils;

use std::cmp::Ordering;
use std::collections::{HashMap, HashSet};

use crate::srs::base::StringRewritingSystem;

// fn lexic(a: &str, b: &str) -> Ordering {
//     match a.len().cmp(&b.len()) {
//         Ordering::Equal => a.cmp(b),
//         x => x,
//     }
// }

macro_rules! map {
    ($key:ty, $val:ty) => {
        {
            let map: HashMap<$key, $val> = HashMap::new();
            map
        }
    };
    [$($key:expr => $val:expr),*] => {
        {
            let mut map = HashMap::new();
            $( map.insert($key, $val); )*
            map
        }
    }
}

fn order(a: &str, b: &str) -> Ordering {
    let a = a.chars().rev().collect::<Vec<_>>();
    let b = b.chars().rev().collect::<Vec<_>>();

    if a == b {
        return Ordering::Equal;
    }

    if a.starts_with(&b) {
        return Ordering::Greater;
    }

    if b.starts_with(&a) {
        return Ordering::Less;
    }

    let convert = map!['a' => 3, 'b' => 1, 'd' => 2, 'r' => 0];

    for (x, y) in a.iter().zip(b.iter()) {
        if x == y {
            continue;
        }

        return if convert[x] > convert[y] {
            Ordering::Greater
        } else {
            Ordering::Less
        };
    }

    Ordering::Equal
}

fn main() {
    // let order = lexic;

    let mut srs = StringRewritingSystem::from([
        ("ab", "br"),
        ("raa", "dr"),
        ("ara", "ad"),
        ("da", "aad"),
        ("dd", "abdar"),
    ]);

    println!("{srs:?}");

    srs.reorder(order);

    println!("{srs:?}");

    for (r1, r2) in srs.critical_pairs() {
        let r1 = srs.any_normal_form(r1);
        let r2 = srs.any_normal_form(r2);
        let (t1, t2) = match order(&r1, &r2) {
            Ordering::Less => (r2, r1),
            _ => (r1, r2),
        };
        println!("{t1} -> {t2}");
    }

    // println!("{:?}", srs.random_word(10..30));

    // let mut queue: Vec<_> = vec![(srs.random_word(10..30), 0)];
    // let mut visited = HashSet::new();
    //
    // while let Some((u, d)) = queue.pop() {
    //     if visited.contains(&u) || u.len() > 50 {
    //         continue;
    //     }
    //
    //     let rewrites = srs.rewrite(&u).collect::<Vec<_>>();
    //
    //     assert!(rewrites.iter().all(|v| order(&u, v) == Ordering::Greater));
    //
    //     queue.extend(rewrites.into_iter().map(|v| (v, d + 1)));
    //
    //     println!("rewriting: {u:?}");
    //     println!("queue size: {:?}", queue.len());
    //
    //     visited.insert(u);
    // }
}
