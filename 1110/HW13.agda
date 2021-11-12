module HW13 where

module problem-1 where

  open import Data.Nat using (ℕ; suc; zero)
  open import Data.Bool using (Bool; true; false)

  _<_ : ℕ → ℕ → Bool
  zero  < zero  = false
  zero  < suc b = true
  suc a < zero  = false
  suc a < suc b = a < b

module problem-2 where

  open import Data.List using (List; []; _∷_)
  open import Data.Bool using (Bool; true; false)
  
  if_then_else_ : {A : Set} → Bool → A → A → A
  if true  then x else y = x
  if false then x else y = y
  
  filter : {A : Set} → (A → Bool) → List A → List A
  filter p [] = []
  filter p (x ∷ xs) = if p x then
                         x ∷ filter p xs
                      else
                         filter p xs

module problem-3 where

  open import Data.Nat using (ℕ; suc; zero)
  open import Data.Vec using (Vec; []; _∷_; zipWith)

  Matrix : Set -> ℕ -> ℕ -> Set
  Matrix A n m = Vec (Vec A n) m
  
  rep : {A : Set} {n : ℕ} → A → Vec A n
  rep {n = zero}  a = []
  rep {n = suc m} a = a ∷ rep {n = m} a
  
  transpose
    : {A : Set}
      {n m : ℕ}
    → Matrix A n m
      ------------
    → Matrix A m n
  transpose [] = rep []
  transpose (v ∷ mat) = zipWith _∷_ v (transpose mat)
 