use rand::random_range as rng;
use std::ops::Range;

use crate::srs::base::StringRewritingSystem;

const STRING_TO_CHECK: usize = 1e5 as usize;

impl StringRewritingSystem {
    pub fn trivial_cycle(&self, w: &str, max_depth: usize) -> bool {
        let mut queue: Vec<_> = vec![(w.to_string(), 0)];

        while let Some((u, d)) = queue.pop() {
            if d != 0 && u.contains(w) {
                return true;
            }

            if d == max_depth {
                continue;
            }

            queue.extend(self.rewrite(u).map(|v| (v, d + 1)));
        }

        false
    }

    pub fn has_cycle(&self, length: Range<usize>, max_depth: usize) -> Option<String> {
        let alphabet: Vec<_> = self.alphabet().collect();

        let min = alphabet.len().pow(length.start as u32);
        let max = alphabet.len().pow(length.end as u32);

        for _ in min..max {
            let w = (0..rng(length.clone()))
                .map(|_| alphabet[rng(0..alphabet.len())])
                .collect::<String>();

            if self.trivial_cycle(&w, max_depth) {
                return Some(w);
            }
        }

        None
    }
}
