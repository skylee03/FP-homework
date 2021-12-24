module HW16 where

-- How to type those Unicode characters:
-- →   \->
-- ≡   \==
-- ≢   \==n
-- ⟨   \<
-- ⟩   \>
-- ∎   \qed
-- ∘   \o
-- ∷   \::
-- ℕ   \bN
-- ⊕   \oplus
-- ˡ   \^l       (4th candidate, use your right arrow key to select)
-- ʳ   \^r       (4th candidate, use your right arrow key to select)
-- ₁   \_1
-- ×   \x
-- ∀   \all
-- Σ   \Sigma
-- ∃   \ex
-- ⊆   \subseteq
-- ≤   \le
-- ⊔   \sqcup
-- ¬   \neg
-- ⊥   \bot
-- ∈   \in

import Relation.Binary.PropositionalEquality as Eq
open Eq using (_≡_; _≢_; refl; trans; sym; cong; cong-app; subst)
open Eq.≡-Reasoning using (begin_; _≡⟨⟩_; _≡⟨_⟩_; _∎)

open import Function using (_∘_)

module monoid where
  record IsMonoid {A : Set} (e : A) (_⊕_ : A → A → A) : Set where
    field
      assoc : ∀ x y z → (x ⊕ y) ⊕ z ≡ x ⊕ (y ⊕ z)
      identityˡ : ∀ x → e ⊕ x ≡ x
      identityʳ : ∀ x → x ⊕ e ≡ x

  open IsMonoid public

  open import Data.Nat using (_+_)
  open import Data.Nat.Properties using (+-assoc; +-identityˡ; +-identityʳ)
  ℕ-add-is-monoid : IsMonoid 0 _+_
  ℕ-add-is-monoid .assoc = +-assoc
  ℕ-add-is-monoid .identityˡ = +-identityˡ
  ℕ-add-is-monoid .identityʳ = +-identityʳ

  open import Data.Nat using (_⊔_)
  open import Data.Nat.Properties using (⊔-assoc; ⊔-identityˡ; ⊔-identityʳ)
  ℕ-⊔-is-monoid : IsMonoid 0 _⊔_
  ℕ-⊔-is-monoid .assoc = ⊔-assoc
  ℕ-⊔-is-monoid .identityˡ = ⊔-identityˡ
  ℕ-⊔-is-monoid .identityʳ = ⊔-identityʳ

  open import Data.List using (List; _++_; [])
  open import Data.List.Properties using (++-assoc; ++-identityˡ; ++-identityʳ)
  List-++-is-monoid : ∀ {A : Set} → IsMonoid {List A} [] _++_
  List-++-is-monoid .assoc = ++-assoc
  List-++-is-monoid .identityˡ = ++-identityˡ
  List-++-is-monoid .identityʳ = ++-identityʳ

open monoid

module MSS (
    extensionality : ∀ {A : Set} {B : A → Set}
        {f g : (x : A) → B x}
      → ((x : A) → f x ≡ g x)
        ---------------------
      → f ≡ g
  ) where

  open import Data.Nat using (ℕ; _+_; zero; suc; _⊔_)
  open import Data.List using (List; []; _∷_; [_]; _++_; foldl; foldr; map; scanl; scanr)

  inits : ∀ {A : Set} → List A → List (List A)
  inits = scanl _++_ [] ∘ map [_]

  tails : ∀ {A : Set} → List A → List (List A)
  tails = scanr _++_ [] ∘ map [_]

  concat : ∀ {A : Set} → List (List A) → List A
  concat = foldr _++_ []

  segs : ∀ {A : Set} → List A → List (List A)
  segs = concat ∘ map tails ∘ inits

  sum : List ℕ → ℕ
  sum = foldr _+_ 0

  maximum : List ℕ → ℕ
  maximum = foldr _⊔_ 0

  mss : List ℕ → ℕ
  mss = maximum ∘ map sum ∘ segs

  -- Did you know there are plenty of useful theorems in the standard library?
  open import Data.Nat.Properties using (+-distribˡ-⊔; +-distribʳ-⊔)
  -- +-distribˡ-⊔ : ∀ x y z → x + (y ⊔ z) ≡ (x + y) ⊔ (x + z)
  -- +-distribˡ-⊔ : ∀ x y z → (x ⊔ y) + z ≡ (x + z) ⊔ (y + z)

  _⊙_   : ℕ → ℕ → ℕ
  a ⊙ b = (a + b) ⊔ 0

  mss-fast : List ℕ → ℕ
  mss-fast =  maximum ∘ (scanl _⊙_ 0)

  -- P 2.11
  map-distrib-++ : ∀ {A B : Set} (f : A → B) (xs ys : List A) → map f (xs ++ ys) ≡ map f xs ++ map f ys
  map-distrib-++ f []       ys = refl
  map-distrib-++ f (x ∷ xs) ys = cong ((f x) ∷_) (map-distrib-++ f xs ys)

  -- P 2.13
  map-promotion : ∀ {A B : Set} (f : A → B) → map f ∘ concat ≡ concat ∘ map (map f)
  map-promotion = extensionality ∘ lem
    where
      lem : ∀ {A B : Set} (f : A → B) (xss : List (List A))
          → (map f ∘ concat) xss ≡ (concat ∘ map (map f)) xss
      lem f []         = refl
      lem f (xs ∷ xss) = 
        begin
          (map f ∘ concat) (xs ∷ xss)
        ≡⟨⟩
          map f (xs ++ concat xss)
        ≡⟨ map-distrib-++ f xs (concat xss) ⟩
          (map f xs) ++ (map f (concat xss))
        ≡⟨ cong ((map f xs) ++_) (lem f xss) ⟩
          (map f xs) ++ ((concat ∘ map (map f)) xss)
        ≡⟨⟩
          (concat ∘ map (map f)) (xs ∷ xss)
        ∎

  maximum-++ : (xs ys : List ℕ) → maximum (xs ++ ys) ≡ maximum xs ⊔ maximum ys
  maximum-++ [] ys = refl
  maximum-++ (x ∷ xs) ys = {!!}
  
  maximum-promotion : maximum ∘ concat ≡ maximum ∘ map maximum
  maximum-promotion = extensionality lem
    where
      lem : ∀ (xss : List (List ℕ)) → (maximum ∘ concat) xss ≡ (maximum ∘ map maximum) xss
      lem []         = refl
      lem (xs ∷ xss) =
        begin
          (maximum ∘ concat) (xs ∷ xss)
        ≡⟨⟩
          maximum (xs ++ concat xss)
        ≡⟨ maximum-++ xs (concat xss) ⟩
          maximum xs ⊔ maximum (concat xss)
        ≡⟨ cong (maximum xs ⊔_) (lem xss) ⟩
          maximum xs ⊔ maximum (map maximum xss)
        ≡⟨⟩
          maximum (maximum xs ∷ map maximum xss)
        ≡⟨⟩
          maximum (map maximum (xs ∷ xss))
        ≡⟨⟩
          (maximum ∘ map maximum) (xs ∷ xss)
        ∎

  map-fusion : ∀ {A B C : Set} (f : B → C) (g : A → B)
             → map f ∘ map g ≡ map (f ∘ g)
  map-fusion f g = extensionality (lem f g)
   where
      lem : ∀ {A B C : Set} (f : B → C) (g : A → B) (xs : List A)
          → (map f ∘ map g) xs ≡ (map (f ∘ g)) xs
      lem f g []       = refl
      lem f g (x ∷ xs) = cong (_∷_ (f (g x))) (lem f g xs)
  
  R-Dist : ∀ {A : Set} (_⊕_ : A → A → A) (_⊗_ : A → A → A) → Set
  R-Dist {A} _⊕_ _⊗_ = ∀ (a b c : A) → (a ⊕ b) ⊗ c ≡ (a ⊗ c) ⊕ (b ⊕ c)
  
  reduce : ∀ {A : Set} → (_⊕_ : A → A → A) → (e : A)
           → IsMonoid e _⊕_ → List A → A
  reduce _⊕_ e _ []       = e
  reduce _⊕_ e p (x ∷ xs) = x ⊕ reduce _⊕_ e p xs
  
  horner's-rule : ∀ {A : Set} (_⊕_ : A → A → A) (e-⊕ : A) (_⊗_ : A → A → A) (e-⊗ : A)
                → (p : IsMonoid e-⊕ _⊕_) → (q : IsMonoid e-⊗ _⊗_)
                → (rdist : R-Dist _⊕_ _⊗_)
                →   reduce _⊕_ e-⊕ p ∘ map (reduce _⊗_ e-⊗ q) ∘ tails
                  ≡ foldl (λ a b → (a ⊗ b) ⊕ e-⊗) e-⊗
  horner's-rule = {!   !}
  
  acc-lemma : ∀{A : Set} (_⊕_ : A → A → A) (e : A)
            → scanl _⊕_ e ≡ map (foldl _⊕_ e) ∘ inits
  acc-lemma = {!   !}
  
  derivation : mss ≡ mss-fast
  derivation =
    begin
      mss
    ≡⟨⟩
      maximum ∘ map sum ∘ segs
    ≡⟨⟩
      maximum ∘ map sum ∘ concat ∘ map tails ∘ inits
    ≡⟨ cong (maximum ∘_) (cong (_∘ map tails ∘ inits) (map-promotion sum)) ⟩
      maximum ∘ concat ∘ map (map sum) ∘ map tails ∘ inits
    ≡⟨ cong (_∘ map (map sum) ∘ map tails ∘ inits) maximum-promotion ⟩
      maximum ∘ map maximum ∘ map (map sum) ∘ map tails ∘ inits
    ≡⟨ cong (maximum ∘_) (cong (_∘ map tails ∘ inits) (map-fusion maximum (map sum))) ⟩
      maximum ∘ map (maximum ∘ map sum) ∘ map tails ∘ inits
    ≡⟨ cong (maximum ∘_) (cong (_∘ inits) (map-fusion (maximum ∘ map sum) tails)) ⟩
      maximum ∘ map (maximum ∘ map sum ∘ tails) ∘ inits
    ≡⟨ {!!} ⟩
      maximum ∘ map (foldl _⊙_ 0) ∘ inits
    ≡⟨ cong (maximum ∘_) (sym (acc-lemma _⊙_ 0)) ⟩
      maximum ∘ (scanl _⊙_ 0)
    ≡⟨⟩
      mss-fast
    ∎

  -- note: it is possible to avoid extensionality and instead prove the following
  --
  -- derivation-alt : ∀ xs → mss xs ≡ mss-fast xs
  -- derivation-alt = ?
  --
  -- in fact, this version should be slightly easier to write, since it (generally)
  -- produces better error messages. If you want to follow this route, go ahead and
  -- prove the above 'derivation-alt', and uncomment the following:
  --
  -- derivation : mss ≡ mss-fast
  -- derivation = extensionality derivation-alt

  -- bonus(hard): try to prove the correctness of 'mss' and 'mss-fast'
  --
  -- We have this "segment" relation (you may come up with better definitions):
  --   open import Data.List using (take; drop)
  --   infix 4 _⊆_
  --   data _⊆_ {A : Set} (xs : List A) (ys : List A) : Set where
  --     segment : ∀ m n → take m (drop n ys) ≡ xs → xs ⊆ ys
  -- We also have the "less than" relation:
  --   open import Data.Nat using (_≤_)
  -- which is defined as follows in the standard library:
  --   infix 4 _≤_
  --   data _≤_ : ℕ → ℕ → Set where
  --     z≤n : ∀ {n}                 → zero  ≤ n
  --     s≤s : ∀ {m n} (m≤n : m ≤ n) → suc m ≤ suc n
  -- 'mss' is proven to be correct if we can prove the following two theorems:
  --   open import Data.Product using (_×_; ∃-syntax)
  --   mss-is-max : ∀ {xs ys} → ys ⊆ xs → sum ys ≤ mss xs
  --   mss-exists : ∀ {xs} → ∃[ ys ] ys ⊆ xs × sum ys ≡ mss xs

{-module BMF2-1 where

  open import Data.Product using (_×_; _,_; Σ-syntax; proj₁)
  open import Data.Nat using (ℕ; _+_; zero; suc)
  open import Data.List using (List; []; _∷_; [_]; _++_)
  import Data.List using (map)
  open import Relation.Nullary using (¬_)

  -- remark: 'Σ[ xs ∈ List A ] xs ≢ []' means
  --   those 'xs ∈ List A' such that 'xs ≢ []'
  NList : (A : Set) → Set
  NList A = Σ[ xs ∈ List A ] xs ≢ []

  -- this reduce works on non-empty lists
  reduce : ∀ {A : Set} → (_⊕_ : A → A → A) → NList A → A
  reduce {A} _⊕_ = λ (xs , N) → helper xs N
    module Reduce where
    helper : (xs : List A) → xs ≢ [] → A
    helper [] N with () ← N refl
    helper (x ∷ []) _ = x
    helper (x ∷ xs@(_ ∷ _)) _ = x ⊕ helper xs (λ())

  -- this map works on non-empty lists
  -- and it produces non-empty lists
  map : ∀ {A B : Set} → (f : A → B) → NList A → NList B
  map f ([] , N) with () ← N refl
  map f (x ∷ xs , _) = f x ∷ Data.List.map f xs , λ()

  -- 1. prove 'split' is a homomorphism
  split : ∀ {A : Set} → NList A → List A × A
  split = reduce ? ∘ map ?

  -- to verify your 'split' is correct. after defining 'split', proving the following
  -- should be as easy as filling in 'refl'.
  split-is-correct : split (1 ∷ 2 ∷ 3 ∷ 4 ∷ [] , λ()) ≡ (1 ∷ 2 ∷ 3 ∷ [] , 4)
  split-is-correct = ?

  -- bonus: find a proper way to prove your split is indeed correct:
  -- split-is-indeed-correct : ∀ {A} xs
  --   → let (ys , z) = split {A} xs
  --     in proj₁ xs ≡ ys ++ [ z ]

  -- 2. prove 'init' is not a homomorphism
  --    let's pretend 'init [] ≡ []' to make the termination checker happy
  init : ∀ {A : Set} → List A → List A
  init [] = []
  init (x ∷ []) = []
  init (x ∷ xs) = x ∷ init xs

  -- This part might be too hard for you to prove in Agda, so you can choose
  -- to write this part in natural language. If so, comment out (or remove)
  -- the following code, and write your proof in the comments.
  --
  -- Anyway, below are some key points if you want to try to prove in Agda:
  -- (1) inequality 'x ≢ y' is negation of equality: '¬ (x ≡ y)'
  -- (2) negation '¬ x' is implication to falsity: 'x → ⊥'
  -- (3) falsity '⊥' is an empty data type, it has no constructors ...
  -- (4) ... which means we can pattern match with absurd pattern '()'

  record IsHomomorphism
    {A : Set} {a : A} {_⊕_ : A → A → A} (m₁ : IsMonoid a _⊕_)
    {B : Set} {b : B} {_⊗_ : B → B → B} (m₂ : IsMonoid b _⊗_)
    (f : A → B) : Set where
    field
      distrib : (x y : A) → f (x ⊕ y) ≡ f x ⊗ f y

  open IsHomomorphism

  init-is-not-homomorphism :
    ∀ {e : List ℕ} {_⊗_} (m : IsMonoid e _⊗_)
    → ¬ IsHomomorphism List-++-is-monoid m init
  init-is-not-homomorphism = ?

  -- Hint: you might want to follow this guideline below if you get stuck.
  --
  -- Step 1: interpret the theorem
  --   ¬ IsHomomorphism List-++-is-monoid m init
  -- is just another way of saying
  --   IsHomomorphism List-++-is-monoid m init → ⊥
  -- (proof by contradiction)
  --
  -- Step 2: get your premise
  -- You want to derive contradiction from the premise, so the first thing
  -- to do is get the premise (add it as an argument):
  --   init-is-not-homomorphism {e} {_⊗_} m H = ?
  -- Now we have the following premises:
  --   m : IsMonoid e _⊗_
  --   H : IsHomomorphism List-++-is-monoid m init
  --
  -- Step 3: derive absurd results
  -- Pass in some example to your premises, and try to get some absurd
  -- results such as 'K : [ 0 ] ≡ [ 42 ]'.
  --
  -- Step 4: show the absurdity by proving the negation
  -- e.g. for 'K : [ 0 ] ≡ [ 42 ]', write the following:
  --   ¬K : [ 0 ] ≢ [ 42 ]
  --   ¬K ()
  --
  -- Step 5: make use of that absurd result
  -- Use the result 'K' from Step 3, apply it to '¬K':
  --   ¬K K
  -- Just use this expression as the return value.
-}
