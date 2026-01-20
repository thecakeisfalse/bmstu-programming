use std::cmp::{Ordering, min};
use std::collections::{HashSet, VecDeque};

use crate::srs::base::{Rule, StringRewritingSystem};

///
/// Knuth-Bendix completion
///
impl StringRewritingSystem {
    fn critical_pairs_for(&self, (l_i, r_i): &Rule) -> impl Iterator<Item = (String, String)> {
        self.iter().flat_map(|(l_j, r_j)| {
            (1..min(l_i.len(), l_j.len()))
                .map(|i| (l_i.len() - i, i))
                .filter(|(i, j)| l_i[*i..] == l_j[..*j])
                .map(|(i, j)| (l_i[..i].to_owned() + r_j, r_i.clone() + &l_j[j..]))
        })
    }

    pub fn critical_pairs(&self) -> impl Iterator<Item = (String, String)> {
        self.iter()
            .flat_map(|rule_i| self.critical_pairs_for(rule_i))
    }

    pub fn any_normal_form(&self, mut term: String) -> String {
        while let Some(new_term) = self.rewrite(term.clone()).next() {
            term = new_term;
        }

        term
    }

    // Не осилил. Literally skill issue
    fn interreduce<F>(&mut self, order: F)
    where
        F: Fn(&str, &str) -> Ordering,
    {
        let mut rules = self.rules().clone();
        let mut changed = true;

        println!("{rules:?}");

        while changed {
            let mut new_rules = vec![];
            let mut set = HashSet::new();
            changed = false;

            for (l, r) in rules {
                let new_l = self
                    .rewrite_without(l.clone(), &l)
                    .min_by(|l, r| order(l, r))
                    .inspect(|x| println!("{l:?} -> {x:?}"))
                    .unwrap_or(l);

                match order(&new_l, &r) {
                    Ordering::Greater if !set.contains(&new_l) => {
                        new_rules.push((new_l.clone(), r));
                        set.insert(new_l);
                    }
                    _ => {
                        changed = true;
                    }
                }
            }

            rules = new_rules;
        }

        println!("{rules:?}");

        *self = rules.into();
    }

    pub fn knuth_bendix<F>(&self, order: F) -> Self
    where
        F: Fn(&str, &str) -> Ordering,
    {
        let mut result = self.clone();
        let mut pairs: VecDeque<_> = result.critical_pairs().collect();

        while let Some((t1, t2)) = pairs.pop_front() {
            let (t1, t2) = (result.any_normal_form(t1), result.any_normal_form(t2));

            if t1 == t2 {
                continue;
            }

            let (l, r) = match order(&t1, &t2) {
                Ordering::Equal => panic!("not equal but same measure: {t1} {t2}"),
                Ordering::Less => (t2, t1),
                Ordering::Greater => (t1, t2),
            };

            result.interreduce(&order); // TODO:
            result.add_rule(&l, &r);
            pairs.extend(result.critical_pairs_for(&(l, r)));
        }

        result
    }
}
