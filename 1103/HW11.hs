module HW11 where

import Prelude hiding (Maybe(..))

-- Problem #1: Maybe, Foldable and Traversable
data Maybe a = Nothing | Just a
  deriving (Show, Eq, Ord)

instance Functor Maybe where
  -- fmap :: (a -> b) -> Maybe a -> Maybe b
  fmap _ Nothing  = Nothing
  fmap f (Just x) = Just $ f x

instance Foldable Maybe where
  -- foldMap :: Monoid b => (a -> b) -> Maybe a -> b
  foldMap _ Nothing  = mempty
  foldMap f (Just x) = f x
  -- foldl :: (b -> a -> b) -> b -> Maybe a -> b
  foldl f y Nothing  = y
  foldl f y (Just x) = f y x
  -- foldr :: (a -> b -> b) -> b -> Maybe a -> b
  foldr f y Nothing  = y
  foldr f y (Just x) = f x y

foldMaybe :: Monoid a => Maybe a -> a
foldMaybe Nothing  = mempty
foldMaybe (Just x) = x

instance Traversable Maybe where
  traverse f Nothing  = pure Nothing
  traverse f (Just x) = Just <$> f x
-- End Problem #1

-- Problem #2: Tree, Foldable and Traversable
data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving Show

instance Functor Tree where
  fmap f Leaf         = Leaf
  fmap f (Node l x r) = Node l' x' r'
                        where l' = fmap f l
                              x' = f x
                              r' = fmap f r

instance Foldable Tree where
  foldMap _ Leaf         = mempty
  foldMap f (Node l x r) = l' <> x' <> r'
                           where l' = foldMap f l
                                 x' = f x
                                 r' = foldMap f r

  foldl _ y Leaf = y
  foldl f y (Node l x r) = foldl f (f (foldl f y l) x) r

  foldr _ y Leaf = y
  foldr f y (Node l x r) = foldr f (f x (foldr f y r)) l

foldTree :: Monoid a => Tree a -> a
foldTree Leaf = mempty
foldTree (Node l x r) = foldTree l <> x <> foldTree r

instance Traversable Tree where
  traverse f Leaf         = pure Leaf
  traverse f (Node l x r) = Node <$> traverse f l <*> f x <*> traverse f r
-- End Problem #2

-- Problem #3: fibonacci using zip/tail/list-comprehension
fibs :: [Integer]
fibs = [0, 1] ++ [x + y | (x, y) <- zip fibs $ tail fibs]
-- End Problem #3

-- Problem #4: Newton's square root
sqroot :: Double -> Double
sqroot n = snd . head . dropWhile (\(a, b) -> abs(a - b) > eps) . zip as $ tail as
           where eps = 0.00001
                 as = iterate (\a -> (a + n/a) / 2) 1
-- End Problem #4
