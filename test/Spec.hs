module Main
  ( main
  ) where

import           Anubisat
import           Control.Lens
import           Data.Algebra.Boolean
import           Prelude                 hiding ( (&&)
                                                , not
                                                , (||)
                                                )
import           Test.Hspec


formula :: Formula
formula =
  (Symbol "p" <--> Not (Symbol "q"))
    && (Symbol "r" --> (Symbol "s" || Symbol "t"))

model :: Model
model =
  fromList [("p", True), ("q", True), ("r", True), ("s", True), ("t", True)]

formula' :: Formula
formula' = (T <--> Not T) && (T --> (T || T))

f :: Formula
f =
  Not (Symbol "p" && (Symbol "q" || Symbol "r")) && (Symbol "s" || Symbol "t")

main :: IO ()
main = hspec $ do
  describe "AST manipulation" $ do
    it "grabs all vars" $ do
      variableNames formula `shouldBe` ["p", "q", "r", "s", "t"]

    it "given a model and a formula, substitutes the values" $ do
      applyModel model formula `shouldBe` formula'

    it "simplifies a formula" $ do
      simplify (Not (Not (Not (Not (Symbol "a"))))) `shouldBe` Symbol "a"

    it "simplify evaluates when all variables have been filled" $ do
      simplify formula' `shouldBe` F

    it "evaluates a formula, given a model" $ do
      evaluate model formula `shouldBe` False

    -- it "transforms an arbitrary formula to NNF" $ do 
    --   toNNF f `shouldBe` f' 
    --   where 
    --     f = (Symbol "p" <--> Symbol "q") <--> (Symbol "r" <--> Symbol "s")
    --     f' =  
    --       ((Symbol "p" && Not (Symbol "q")) || (Not (Symbol "p") && Symbol "q") || ()) 

