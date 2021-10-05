module AltRep where

    data Music attr a = Prim a
                 | Music attr a :+: Music attr a
                 | Music attr a :*: Music attr a
                 | Atrr attr (Music attr a)
        deriving (Show, Eq)

    infix 5 :+:
    infix 4 :*:

    newtype Line attr a = Line {unLine :: Music attr a}

    newtype Chord attr a = Chord {unChord :: Music attr a}

    class Duration a where
        toRational  :: Fractional b => a -> b

    instance Semigroup (Line attr a) where
        x <> y = Line $ unLine x :+: unLine y

    instance Semigroup (Chord attr a) where
        x <> y = Chord $ unChord x :*: unChord y

    instance Monoid a => Monoid (Line attr a) where
        mempty = Line $ Prim mempty
        mappend = (<>)

    instance Monoid a => Monoid (Chord attr a) where
        mempty = Chord $ Prim mempty
        mappend = (<>)

    line, chord :: Monoid a => [Music attr a] -> Music attr a
    line = unLine.mconcat.map Line
    chord = unChord.mconcat.map Chord


    class Durational a where
        duration :: Fractional b => a -> b

    type Pitch = (PitchClass, Octave)

    type Octave = Int 

    type PitchClass = (Note, Accidental)

    data Note = A | B | C | D | E | F | G
    data Accidental = Natural | Flat | Sharp

    class PitchLike a where
        toPitch :: a -> Pitch

    instance Functor (Music attr) where
        fmap f (Prim a) = Prim $ f a
        fmap f (a :+: b) = fmap f a :+: fmap f b
        fmap f (a:*:b) = fmap f a :*: fmap f b

    forever :: Music a b -> Music a b
    forever a = a :+: forever a



