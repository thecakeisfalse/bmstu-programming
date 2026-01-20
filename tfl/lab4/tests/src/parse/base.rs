pub trait Parse {
    fn parse<S: AsRef<[u8]> + ?Sized>(s: &S) -> bool;
}

pub trait TryParse {
    fn try_parse<S: AsRef<[u8]> + ?Sized>(s: &S) -> Option<bool>;
}

impl<T: Parse> TryParse for T {
    fn try_parse<S: AsRef<[u8]> + ?Sized>(s: &S) -> Option<bool> {
        Some(Self::parse(s))
    }
}
