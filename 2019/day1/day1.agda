module day1 where

open import IO using (IO; putStrLn; run; _>>_)
import IO.Primitive as Prim
open import IO.Primitive using (_>>=_)
open import Data.Unit using (⊤)
open import Data.Nat using (ℕ; _≤?_; zero; suc; _∸_; _+_)
open import Data.String
open import Util
open import Relation.Nullary using (Dec; yes; no)
open import Function using (_∘_; _$_)

data List (A : Set) : Set where
  [] : List A
  _∷_ : A -> List A -> List A
{-# COMPILE GHC List = data List ([] | (:)) #-}

div3 : (t : ℕ) -> ℕ
div3 tot = go tot zero 
  where
  go : ℕ -> ℕ -> ℕ
  go zero count = count
  go (suc zero) count = count
  go (suc (suc zero)) count = count
  go (suc (suc (suc t))) count = go t (suc count)

fuel-req : ℕ -> ℕ
fuel-req t = (div3 t) ∸ 2

fuel-req-iter : ℕ -> ℕ
fuel-req-iter i = go i i zero
  where
  go : ℕ -> ℕ -> ℕ -> ℕ
  go zero c acc = acc
  go (suc i) zero acc = acc
  go (suc i) (suc c) acc = go (fuel-req (suc i)) c (acc + fuel-req (suc i))


sum : List ℕ -> ℕ
sum [] = zero
sum (n ∷ ns) = _+_ n (sum ns)

map : ∀ {a b : Set} -> (a -> b) -> List a -> List b
map f [] = []
map f (a ∷ as) = (f a) ∷ (map f as)

total-fuel : List ℕ -> ℕ
total-fuel = sum ∘ map (fuel-req)

total-fuel-iter : List ℕ -> ℕ
total-fuel-iter = sum ∘ map (fuel-req-iter)

postulate
  showNat  : ℕ -> String
  ex : List ℕ
  readInput : Prim.IO (List ℕ)
{-# COMPILE GHC showNat  = showNat     #-}
{-# COMPILE GHC ex        = test       #-}
{-# COMPILE GHC readInput = readInput  #-}

-- puzzle 1 solution
printResult1 : List ℕ -> IO ⊤
printResult1 ns = (putStrLn (showNat (total-fuel ns)))

-- puzzle 2 solution
printResult2 : List ℕ -> IO ⊤
printResult2 ns = (putStrLn (showNat (total-fuel-iter ns)))

main : Prim.IO ⊤
main = readInput Prim.>>= λ i -> run $ printResult2 i


{-# FOREIGN GHC
  import Data.Text as Text

  type List a = [a]

  test :: [Integer]
  test = [1,2,3]

  showNat :: Integer -> Text
  showNat = Text.pack . show

  readInput :: IO [Integer]
  readInput =
    do
      inp <- readFile "input.txt"
      let inpLines = Prelude.lines inp
      pure $ read <$> inpLines
#-}

