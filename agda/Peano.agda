module Peano where
  data ℕ : Set where
    zero : ℕ
    suc : ℕ → ℕ

  _+_ : ℕ → ℕ → ℕ
  zero + zero = zero
  zero + n    = n
  (suc n) + m = suc (n + m)

  data _even : ℕ → Set where
    ZERO : zero even
    STEP : ∀ x → x even → suc (suc x) even

  proof₁ : suc (suc (suc (suc zero))) even
  proof₁ = STEP (suc (suc zero)) (STEP zero ZERO)
