module HW15 where

import Relation.Binary.PropositionalEquality as Eq
open Eq using (_≡_; refl; trans; sym; cong; cong-app; subst)
open Eq.≡-Reasoning using (begin_; _≡⟨⟩_; _≡⟨_⟩_; _∎)

open import Data.List using (List; []; _∷_; _++_; foldr)

module problem-1 where

  ++-assoc : ∀ {A : Set}
      (xs ys zs : List A)
      -----------------------------------
    → (xs ++ ys) ++ zs ≡ xs ++ (ys ++ zs)
  ++-assoc [] ys zs       = refl
  ++-assoc (x ∷ xs) ys zs =
    begin
      x ∷ (xs ++ ys) ++ zs
    ≡⟨ cong (x ∷_) (++-assoc xs ys zs) ⟩
      x ∷ xs ++ ys ++ zs
    ∎

  -- tips: to input the superscript l (ˡ), type \^l and use your
  --       arrow keys to select it (should be the fourth candidate).
  ++-identityˡ : ∀ {A : Set}
      (xs : List A)
      -------------
    → [] ++ xs ≡ xs
  ++-identityˡ = λ xs → refl

  -- you might have already guessed it: type \^r for superscript r (ʳ)
  -- again, use your arrow keys to select it (the fourth candidate). 
  ++-identityʳ : ∀ {A : Set}
      (xs : List A)
    → xs ++ [] ≡ xs
  ++-identityʳ []       = refl
  ++-identityʳ (x ∷ xs) =
    begin
      x ∷ xs ++ []
    ≡⟨ cong (x ∷_) (++-identityʳ xs) ⟩
      x ∷ xs
    ∎

module problem-2 where

  -- tips: to input ⊗, type \otimes
  foldr-++ : ∀ {A : Set}
      (_⊗_ : A → A → A)
      (e : A)
      (xs ys : List A)
      -----------------------------
    → foldr _⊗_ e (xs ++ ys)
    ≡ foldr _⊗_ (foldr _⊗_ e ys) xs
  foldr-++ _⊗_ e []       ys = refl
  foldr-++ _⊗_ e (x ∷ xs) ys =
    begin
      foldr _⊗_ e ((x ∷ xs) ++ ys)
    ≡⟨ cong (x ⊗_) (foldr-++ _⊗_ e xs ys) ⟩
      foldr _⊗_ (foldr _⊗_ e ys) (x ∷ xs)
    ∎
      

module problem-3 (
    extensionality : ∀ {A : Set} {B : A → Set}
        {f g : (x : A) → B x}
      → ((x : A) → f x ≡ g x)
        ---------------------
      → f ≡ g
  ) where

  open import Function using (id; _∘_)

  reverse : ∀ {A : Set} → List A → List A
  reverse []       = []
  reverse (x ∷ xs) = reverse xs ++ (x ∷ [])

  -- hint: you might want to introduce an extra lemma to prove this.
  reverse-involutive : ∀ {A : Set} → reverse {A} ∘ reverse {A} ≡ id
  reverse-involutive {A} = extensionality lem
    where
      reverse-tail : (x : A) → (xs : List A) → reverse (xs ++ (x ∷ [])) ≡ x ∷ reverse xs
      reverse-tail x []        = refl
      reverse-tail x (x₁ ∷ xs) = 
        begin
          reverse ((x₁ ∷ xs) ++ (x ∷ []))
        ≡⟨⟩
          reverse (x₁ ∷ (xs ++ (x ∷ [])))
        ≡⟨⟩
          (reverse (xs ++ (x ∷ []))) ++ (x₁ ∷ [])
        ≡⟨ cong (_++ (x₁ ∷ [])) (reverse-tail x xs) ⟩
          (x ∷ reverse xs) ++ (x₁ ∷ [])
        ≡⟨⟩
          x ∷ (reverse xs ++ (x₁ ∷ []))
        ≡⟨⟩
          x ∷ reverse (x₁ ∷ xs)
        ∎
      lem : ∀ (xs : List A) → (reverse ∘ reverse) xs ≡ id xs
      lem []       = refl
      lem (x ∷ xs) =
        begin
          (reverse ∘ reverse) (x ∷ xs)
        ≡⟨⟩
          reverse (reverse (x ∷ xs))
        ≡⟨⟩
          reverse ((reverse xs) ++ (x ∷ []))
        ≡⟨ reverse-tail x (reverse xs) ⟩
          x ∷ reverse (reverse xs)
        ≡⟨⟩
          x ∷ ((reverse ∘ reverse) xs)
        ≡⟨ cong (x ∷_) (lem xs) ⟩
          x ∷ xs
        ∎

  -- bonus: fast-reverse-involutive
  -- this is only for practice, not part of the homework this week

  -- fast-reverse : ∀ {A : Set} → List A → List A
  -- fast-reverse = helper []
  --   module FastReverse where
  --   helper : ∀ {A : Set} → List A → List A → List A
  --   helper res []       = res
  --   helper res (x ∷ xs) = helper (x ∷ res) xs

  -- fast-reverse-involutive : ∀ {A : Set}
  --   → fast-reverse {A} ∘ fast-reverse {A} ≡ id
  -- fast-reverse-involutive = ?
