use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
    ops::{Deref, DerefMut},
};

use crate::automaton::{Automaton, Transitions};

#[derive(Debug)]
pub struct Partition<'a, A: Automaton> {
    automaton: &'a A,
    blocks: Vec<HashSet<A::State>>,
}

impl<'a, A> Clone for Partition<'a, A>
where
    A: Automaton,
{
    fn clone(&self) -> Self {
        Self {
            automaton: self.automaton,
            blocks: self.blocks.clone(),
        }
    }
}

impl<'a, A: Automaton> Deref for Partition<'a, A> {
    type Target = Vec<HashSet<A::State>>;

    fn deref(&self) -> &Self::Target {
        &self.blocks
    }
}

impl<'a, A: Automaton> DerefMut for Partition<'a, A> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.blocks
    }
}

impl<'a, A: Automaton> Partition<'a, A> {
    pub fn new(automaton: &'a A) -> Self {
        Self {
            automaton,
            blocks: vec![],
        }
    }
}

impl<'a, A, S> Display for Partition<'a, A>
where
    A: Automaton<State = S> + Display,
    S: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut msg = String::new();

        msg.push_str("Automaton:\n\n");
        msg.push_str(&format!("{}", self.automaton));

        msg.push('\n');

        msg.push_str("Classes of equivalence\n");

        for (n, block) in self.blocks.iter().enumerate() {
            msg.push_str(&format!("{n}: "));
            let block: Vec<String> = block.iter().map(|s| s.to_string()).collect();
            msg.push_str(&block.join(", "));
            msg.push('\n');
        }

        write!(f, "{msg}")
    }
}

pub trait PartialReduce<'a>
where
    Self: Sized + Automaton,
{
    fn split(&'a self, partition: &Partition<'a, Self>) -> Option<Partition<'a, Self>> {
        self.alphabet().iter().find_map(|symbol| {
            partition.iter().enumerate().find_map(|(i, a1)| {
                partition.iter().find_map(|a2| {
                    let (b1, b2): (HashSet<_>, HashSet<_>) = a1.iter().partition(|state| {
                        let transition = self
                            .transition(**state, *symbol)
                            .expect("all transitions must be defined");

                        transition.is_disjoint(a2)
                    });

                    (!b1.is_empty() && !b2.is_empty()).then(|| {
                        let mut new_partition = partition.clone();
                        new_partition.swap_remove(i);
                        new_partition.push(b1);
                        new_partition.push(b2);
                        new_partition
                    })
                })
            })
        })
    }

    fn language_partition(&'a self) -> Partition<'a, Self> {
        let mut p = Partition::new(self);

        if self.final_states().is_empty() || self.states() == self.final_states() {
            p.push(self.states().clone());
        } else {
            p.push(self.final_states().clone());
            p.push(
                self.states()
                    .difference(self.final_states())
                    .copied()
                    .collect::<HashSet<_>>(),
            );
        }

        while let Some(q) = self.split(&p) {
            p = q;
        }

        p
    }

    fn reduce<A>(&'a self) -> A
    where
        A: Automaton<State = usize, Symbol = Self::Symbol>,
    {
        let blocks = self.language_partition().blocks;
        let mut new_automaton = A::default();

        let state_to_color: HashMap<Self::State, usize> = blocks
            .iter()
            .enumerate()
            .flat_map(|(color, block)| block.iter().map(move |state| (*state, color)))
            .collect();

        self.initial_states()
            .iter()
            .map(|state| state_to_color.get(state).expect("wrong language partition"))
            .for_each(|state| new_automaton.add_initial_state(*state));

        self.final_states()
            .iter()
            .map(|state| state_to_color.get(state).expect("wrong language partition"))
            .for_each(|state| new_automaton.add_final_state(*state));

        new_automaton.set_alphabet(self.alphabet().clone());

        (0..blocks.len()).for_each(|state| new_automaton.add_state(state));

        for (color, block) in blocks.iter().enumerate() {
            for symbol in self.alphabet() {
                for state in block {
                    if let Some(transitions) = self.transition(*state, *symbol) {
                        for next_state in transitions {
                            let next_color = state_to_color
                                .get(next_state)
                                .expect("wrong language partition");

                            new_automaton.add_transition(((color, *symbol), *next_color));
                        }
                    }
                }
            }
        }

        if let Some(trap_state) = self.get_trap_state() {
            new_automaton.set_trap_state(*state_to_color.get(&trap_state).unwrap());
        }

        println!("{:?}", new_automaton.get_trap_state());

        new_automaton
    }
}

impl<'a, S, T, U: Automaton<State = S, Symbol = T>> PartialReduce<'a> for U {}

pub trait Reduce<'a>: PartialReduce<'a> + Sized + Automaton {}
