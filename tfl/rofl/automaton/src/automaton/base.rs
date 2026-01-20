use std::{
    collections::{HashMap, HashSet},
    fmt::Debug,
    hash::Hash,
    ops::{Deref, DerefMut},
};

pub type States<S> = HashSet<S>;
pub type Symbols<T> = HashSet<T>;

pub trait Automaton: Default {
    type State: Eq + Hash + Copy;
    type Symbol: Eq + Hash + Copy;

    /// Get all states
    fn states(&self) -> &States<Self::State>;

    /// Add new state
    fn add_state(&mut self, state: Self::State);

    /// Get alphabet
    fn alphabet(&self) -> &Symbols<Self::Symbol>;

    /// Set alphabet
    fn set_alphabet(&mut self, symbols: Symbols<Self::Symbol>);

    /// Get all transitions by one symbol
    fn transition(&self, from: Self::State, symbol: Self::Symbol) -> Option<&States<Self::State>>;

    /// Add new transition
    fn add_transition<U>(&mut self, value: U)
    where
        U: Into<Transition<Self::State, Self::Symbol>>;

    /// Get final states
    fn final_states(&self) -> &States<Self::State>;

    /// Add final state
    fn add_final_state(&mut self, state: Self::State);

    /// Get initial states
    fn initial_states(&self) -> &States<Self::State>;

    /// Add initial state
    fn add_initial_state(&mut self, state: Self::State);

    /// Check if state is final
    fn is_final_state(&self, state: Self::State) -> bool {
        self.final_states().contains(&state)
    }

    /// Add trap state
    fn add_trap_state(&mut self, trap: Self::State) -> bool {
        assert!(!self.states().contains(&trap));

        self.set_trap_state(trap);

        let mut new_transitions = vec![];

        for state in self.states() {
            for symbol in self.alphabet() {
                if self.transition(*state, *symbol).is_none() {
                    new_transitions.push(((*state, *symbol), trap));
                }
            }
        }

        if new_transitions.is_empty() {
            return false;
        }

        self.add_state(trap);

        for symbol in self.alphabet() {
            new_transitions.push(((trap, *symbol), trap));
        }

        for v in new_transitions {
            self.add_transition(v);
        }

        true
    }

    /// Set trap state
    fn set_trap_state(&mut self, trap: Self::State);

    /// Get trap state
    fn get_trap_state(&self) -> Option<Self::State>;
}

#[derive(Default, Debug)]
pub struct Transition<S, T>((S, T), States<S>);

impl<S, T> From<((S, T), S)> for Transition<S, T>
where
    S: Eq + Hash,
    T: Eq + Hash,
{
    #[inline]
    fn from((key, value): ((S, T), S)) -> Self {
        Self(key, HashSet::from([value]))
    }
}

impl<S, T> From<((S, T), HashSet<S>)> for Transition<S, T>
where
    S: Eq + Hash,
    T: Eq + Hash,
{
    #[inline]
    fn from(value: ((S, T), HashSet<S>)) -> Self {
        Self(value.0, value.1)
    }
}

#[derive(Default, Debug, Clone)]
pub struct Transitions<S, T>(HashMap<(S, T), States<S>>);

impl<S, T> Transitions<S, T>
where
    S: Eq + Hash,
    T: Eq + Hash,
{
    pub fn add<U: Into<Transition<S, T>>>(&mut self, value: U) {
        let value: Transition<_, _> = value.into();
        self.0.entry(value.0).or_default().extend(value.1);
    }
}

impl<S, T> Deref for Transitions<S, T> {
    type Target = HashMap<(S, T), States<S>>;

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<S, T> DerefMut for Transitions<S, T> {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<S, T> From<HashMap<(S, T), S>> for Transitions<S, T>
where
    S: Eq + Hash,
    T: Eq + Hash,
{
    fn from(value: HashMap<(S, T), S>) -> Self {
        Self(
            value
                .into_iter()
                .map(|(k, v)| (k, HashSet::from([v])))
                .collect(),
        )
    }
}

impl<S, T> From<HashMap<(S, T), HashSet<S>>> for Transitions<S, T>
where
    S: Eq + Hash,
    T: Eq + Hash,
{
    #[inline]
    fn from(value: HashMap<(S, T), HashSet<S>>) -> Self {
        Self(value)
    }
}
