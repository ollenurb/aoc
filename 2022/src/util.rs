pub trait BiFunctor {
    type Item;

    fn bimap<F, B>(self, f: F) -> (B, B)
    where
        F: FnMut(Self::Item) -> B;
}

// BiMap a Tuple (A, A) -> (B, B)
impl<A> BiFunctor for (A, A) {
    type Item = A;

    fn bimap<F, B>(self, mut f: F) -> (B, B)
    where
        F: FnMut(Self::Item) -> B {
            (f(self.0), f(self.1))
    }
}

