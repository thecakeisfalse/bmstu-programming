use std::{
    fmt::{Debug, Display},
    hash::Hash,
};

use crate::automaton::{Automaton, States, Symbols, Transition, Transitions};

#[derive(Debug, Default)]
pub struct Nfa<S, T> {
    states: States<S>,
    alphabet: Symbols<T>,
    transition: Transitions<S, T>,
    finals: States<S>,
    initials: States<S>,
    trap_state: Option<S>,
}

impl<S, T> Nfa<S, T>
where
    S: Eq + Hash + Copy + Default,
    T: Eq + Hash + Copy + Default,
{
    pub fn new(
        states: States<S>,
        alphabet: Symbols<T>,
        finals: States<S>,
        initials: States<S>,
    ) -> Self {
        Self {
            states,
            alphabet,
            finals,
            initials,
            transition: Transitions::default(),
            trap_state: None,
        }
    }

    pub fn with_transitions<U>(
        states: States<S>,
        alphabet: Symbols<T>,
        finals: States<S>,
        initials: States<S>,
        transition: U,
    ) -> Self
    where
        U: Into<Transitions<S, T>>,
    {
        Self {
            transition: transition.into(),
            ..Self::new(states, alphabet, finals, initials)
        }
    }
}

impl<S, T> Automaton for Nfa<S, T>
where
    S: Eq + Hash + Copy + Default,
    T: Eq + Hash + Copy + Default,
{
    type State = S;
    type Symbol = T;

    #[inline]
    fn initial_states(&self) -> &States<Self::State> {
        &self.initials
    }

    #[inline]
    fn final_states(&self) -> &States<Self::State> {
        &self.finals
    }

    #[inline]
    fn states(&self) -> &States<Self::State> {
        &self.states
    }

    #[inline]
    fn alphabet(&self) -> &Symbols<Self::Symbol> {
        &self.alphabet
    }

    #[inline]
    fn transition(&self, from: Self::State, symbol: Self::Symbol) -> Option<&States<Self::State>> {
        self.transition.get(&(from, symbol))
    }

    #[inline]
    fn add_state(&mut self, state: Self::State) {
        self.states.insert(state);
    }

    #[inline]
    fn set_alphabet(&mut self, symbols: Symbols<Self::Symbol>) {
        self.alphabet = symbols;
    }

    #[inline]
    fn add_transition<U>(&mut self, u: U)
    where
        U: Into<Transition<S, T>>,
    {
        self.transition.add(u);
    }

    #[inline]
    fn add_initial_state(&mut self, state: Self::State) {
        self.initials.insert(state);
    }

    #[inline]
    fn add_final_state(&mut self, state: Self::State) {
        self.finals.insert(state);
    }

    #[inline]
    fn set_trap_state(&mut self, trap: Self::State) {
        self.trap_state = Some(trap);
    }

    #[inline]
    fn get_trap_state(&self) -> Option<Self::State> {
        self.trap_state
    }
}

impl<S, T> Display for Nfa<S, T>
where
    S: Display + Eq + Hash + Copy,
    T: Display + Eq + Hash + Copy,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut dot = String::new();

        dot.push_str("digraph NFA {\n");
        dot.push_str("  rankdir=LR;\n");

        if !self.finals.is_empty() {
            dot.push_str("  node [shape = doublecircle]; ");
            let finals: Vec<String> = self.finals.iter().map(|s| s.to_string()).collect();
            dot.push_str(&finals.join(", "));
            dot.push_str(";\n");
        }

        dot.push_str("  node [shape = circle];\n\n");

        for initial in &self.initials {
            dot.push_str("  start [shape = point];\n");
            dot.push_str(&format!("  start -> \"{initial}\";\n"));
        }

        dot.push('\n');

        for (&(from, symbol), transitions) in self.transition.iter() {
            for &to in transitions {
                if !self.trap_state.is_some_and(|state| state == from)
                    && !self.trap_state.is_some_and(|state| state == to)
                {
                    dot.push_str(&format!(
                        "  \"{from}\" -> \"{to}\" [label = \"{symbol}\"];\n",
                    ));
                }
            }
        }

        dot.push_str("}\n");

        write!(f, "{dot}")
    }
}
