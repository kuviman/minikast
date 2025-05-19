pub trait HasVariant<T>: Sized {
    fn from_variant(variant: T) -> Self;
    fn into_variant(self) -> Option<T>;
    fn as_variant(&self) -> Option<&T>;
    fn as_variant_mut(&mut self) -> Option<&mut T>;
}

pub trait EnumVariant: Sized {
    fn into_enum<E: HasVariant<Self>>(self) -> E {
        E::from_variant(self)
    }
    fn into_enum_box<E: HasVariant<Self>>(self) -> Box<E> {
        Box::new(self.into_enum())
    }
}

impl<T> EnumVariant for T {}

pub trait HasField<T> {
    fn get_field(&self) -> &T;
    fn get_field_mut(&mut self) -> &mut T;
}

/// just syntactic sugar
pub trait IntoBox: Sized {
    fn boxed(self) -> Box<Self> {
        Box::new(self)
    }
}

impl<T> IntoBox for T {}
