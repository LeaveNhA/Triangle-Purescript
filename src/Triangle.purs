module Triangle
  ( Triangle(Equilateral, Isosceles, Scalene)
  , triangleKind
  , degenerateCheck
  )
       where

import Prelude

import Data.List (length, fromFoldable)
import Data.Either(Either(..))
import Data.Array (nub)

data Triangle = Equilateral | Isosceles | Scalene

derive instance eqTriangle :: Eq Triangle
instance showTriangle :: Show Triangle where
  show Equilateral = "Equilateral"
  show Isosceles = "Isosceles"
  show Scalene = "Scalene"

degenerateCheck :: Array Int -> Either String (Array Int)
degenerateCheck [a,b,c] | (a <= 0 || b <= 0 || c <= 0) = Left "Invalid lengths"
degenerateCheck [a,b,c] |  (b + c) > a,
                           (a + c) > b,
                           (a + b) > c = Right [a,b,c]
degenerateCheck _ = Left "Violates inequality"

triangleKind :: Int -> Int -> Int -> Either String Triangle
triangleKind a b c =
  case degenerateCheck [a, b, c] of
    (Left err) -> (Left err)
    (Right triangle) -> case length $ fromFoldable $ nub triangle of
      1 -> Right Equilateral
      2 -> Right Isosceles
      otherwise -> Right Scalene
