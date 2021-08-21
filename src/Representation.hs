module Representation where

import Algebra
import Data.List

data Note = Note

data TonalitySystem
  = ET Int
  | Collection [Rational]

data Music a b = Music
  { edges :: [((a, a), b)],
    -- nodes :: [a],
    source :: a,
    sink :: a
  }
  deriving (Show)

nil :: Monoid a => Music a b
nil = mempty

-- map :: (a -> b) -> Music a c -> Music b c
-- map = fmap

lift :: (Monoid a, Show a) => a -> b -> Music a b
lift x y =
  Music
    { -- nodes = [mempty, x]
      edges = [((mempty, x), y)],
      source = mempty,
      sink = x
    }

m1 :: b -> Music Int b
m1 = lift 1 

(+>) :: Semigroup a => Music a b -> Music a b -> Music a b
x +> y =
  disambiguate $
    x
      { --  nodes = nodes a ++ map (base <>) (nodes b)
        edges = edges x ++ edges (first (<> base) y),
        sink = sink y
      }
  where
    base = sink x

nodes :: Ord a => Music a b -> [a]
nodes m =
  let ns = map fst $ edges m
   in map head . group . sort $ foldr (\(x, y) res -> res ++ [x, y]) [] ns


disambiguate :: Music a b -> Music a b
disambiguate = error "not implemented"

inv :: Music a b -> Music a b
inv a = a {source = sink a, sink = source a}

abs :: Group a => Music a b -> a
abs a = sink a ~~ source a

-- reset(t) = t +> inv t
reset :: Music a b -> Music a b
reset a = a {sink = source a}

-- coreset(t) = inv t +> t
coreset :: Music a b -> Music a b
coreset a = a {source = sink a}

-- reset a +> a == a +> coreset a

instance (Eq a, Eq b) => Eq (Music a b) where
  -- HOW ARE THEY EQUAL!!!????
  a == b = edges a == edges b

instance Semigroup a => Semigroup (Music a b) where
  (<>) = (+>)

instance Monoid a => Monoid (Music a b) where
  mempty = Music {edges = [], source = mempty, sink = mempty}

instance Bifunctor Music where
  -- bimap :: (a->b) -> (c->d) -> Music a c -> Music b d
  bimap f g m =
    m
      { edges = map k (edges m),
        sink = f . sink $ m,
        source = f . source $ m
      }
    where
      --k :: ((a, a), c) -> ((b,b), d)
      k ((x, y), z) = ((f x, f y), g z)
