mod automaton;

use automaton::Dfa;
use automaton::reduce::PartialReduce;
use std::{
    collections::{HashMap, HashSet, VecDeque},
    hash::Hash,
};

use crate::automaton::{Automaton, Nfa};

macro_rules! map {
    [$($key:expr => $val:expr),*] => {
        {
            let mut map = HashMap::new();
            $( map.insert($key, $val); )*
            map
        }
    };
    ($key:ty, $val:ty) => {
        {
            let map: HashMap<$key, $val> = HashMap::new();
            map
        }
    };
}

fn canonical<T>(nfa: &Nfa<usize, T>) -> Nfa<usize, T>
where
    T: Eq + Hash + Default + Copy,
{
    let mut rename = HashMap::new();
    let mut queue = VecDeque::new();

    for &state in nfa.initial_states() {
        queue.push_back(state);
    }

    while let Some(state) = queue.pop_front() {
        if rename.contains_key(&state) {
            continue;
        }

        if nfa.get_trap_state().is_some_and(|x| x == state) {
            continue;
        }

        rename.insert(state, rename.len());

        for &symbol in nfa.alphabet() {
            if let Some(transition) = nfa.transition(state, symbol) {
                for &to in transition {
                    queue.push_back(to);
                }
            }
        }
    }

    if let Some(state) = nfa.get_trap_state() {
        rename.insert(state, rename.len());
    }

    let mut ans = Nfa::new(
        (0..rename.len()).collect(),
        nfa.alphabet().clone(),
        nfa.final_states()
            .iter()
            .map(|state| *rename.get(state).unwrap())
            .collect(),
        nfa.initial_states()
            .iter()
            .map(|state| *rename.get(state).unwrap())
            .collect(),
    );

    if let Some(state) = nfa.get_trap_state() {
        ans.set_trap_state(*rename.get(&state).unwrap());
    }

    for &from in nfa.states() {
        for &symbol in nfa.alphabet() {
            if let Some(transition) = nfa.transition(from, symbol) {
                for &to in transition {
                    let from = *rename.get(&from).unwrap();
                    let to = *rename.get(&to).unwrap();
                    ans.add_transition(((from, symbol), to));
                }
            }
        }
    }

    ans
}

fn main() {
    let mut dfa = Dfa::<usize, _>::with_transitions(
        HashSet::from([0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16]),
        HashSet::from_iter("ab".chars()),
        HashSet::from([0, 7, 8, 9, 10, 11, 12, 15, 16]),
        0,
        map![
            (0, 'a') => 1,
            (0, 'b') => 3,
            (1, 'b') => 2,
            (1, 'a') => 5,
            (2, 'a') => 0,
            (3, 'a') => 4,
            (4, 'b') => 5,
            (5, 'a') => 6,
            (6, 'b') => 7,
            (7, 'b') => 3,
            (7, 'a') => 8,
            (8, 'b') => 3,
            (8, 'a') => 9,
            (9, 'b') => 3,
            (9, 'a') => 10,
            (10, 'a') => 10,
            (10, 'b') => 11,
            (11, 'b') => 3,
            (11, 'a') => 12,
            (12, 'a') => 9,
            (12, 'b') => 13,
            (13, 'a') => 14,
            (14, 'b') => 15,
            (15, 'b') => 3,
            (15, 'a') => 16,
            (16, 'a') => 9,
            (16, 'b') => 11
        ],
    );

    _ = dfa.add_trap_state(17);

    let states = dfa.states();

    let mut color: HashMap<(usize, usize), isize> = HashMap::new();

    let mut a = VecDeque::new();

    for p in states {
        for q in states {
            if p == q {
                continue;
            }

            if dfa.is_final_state(*p) != dfa.is_final_state(*q) {
                color.insert((*p, *q), 0);
                color.insert((*q, *p), 0);
            } else {
                a.push_back((p, q));
            }
        }
    }

    while let Some((p, q)) = a.pop_front() {
        let mut any = false;
        for symbol in dfa.alphabet() {
            if let Some(u) = dfa.transition(*p, *symbol) {
                if let Some(v) = dfa.transition(*q, *symbol) {
                    let u = u.iter().next().unwrap();
                    let v = v.iter().next().unwrap();
                    if color.contains_key(&(*u, *v)) {
                        let key = color[&(*u, *v)];
                        color.insert((*p, *q), key + 1);
                        color.insert((*q, *p), key + 1);
                        any = true;
                        break;
                    }
                }
            }
        }

        if !any {
            a.push_back((p, q));
        }
    }

    for i in 0..18 {
        for j in 0..18 {
            if i > j {
                print!("{} ", color[&(i, j)]);
            }
        }
        println!();
    }

    // ---

    let mut nfa = Nfa::with_transitions(
        HashSet::from([0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]),
        HashSet::from_iter("ab".chars()),
        HashSet::from([0, 3, 10, 11]),
        HashSet::from([0]),
        map![
        (0, 'a') => HashSet::from([1, 4]),
        (0, 'b') => HashSet::from([6]),
        (1, 'b') => HashSet::from([2]),
        (2, 'a') => HashSet::from([3]),
        (3, 'a') => HashSet::from([1, 4]),
        (3, 'b') => HashSet::from([6]),
        (4, 'a') => HashSet::from([5]),
        (5, 'a') => HashSet::from([9]),
        (6, 'a') => HashSet::from([7]),
        (7, 'b') => HashSet::from([8]),
        (8, 'a') => HashSet::from([9]),
        (9, 'b') => HashSet::from([10]),
        (10, 'a') => HashSet::from([4, 11]),
        (10, 'b') => HashSet::from([6]),
        (11, 'a') => HashSet::from([4, 11]),
        (11, 'b') => HashSet::from([6])
        ],
    );

    _ = nfa.add_trap_state(42);

    let nfa2: Nfa<_, _> = nfa.reduce();

    let nfa3 = canonical(&nfa2);

    println!("{nfa3}");
}
