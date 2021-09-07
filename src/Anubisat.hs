{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveAnyClass #-}


{- |
 Copyright: (c) 2021 Yuri Carvalho
 SPDX-License-Identifier: MIT
 Maintainer: Yuri Carvalho <yuri.carvalho@alumni.usp.br>

  a noob's SAT solver
-}
module Anubisat where

import           Control.Lens
import           Data.Algebra.Boolean
import           Data.Data
-- import           Data.Generics.Uniplate.Operations
import           Prelude                 hiding ( (&&)
                                                , not
                                                , (||)
                                                )
import           Relude.Extra.Map
-- | A formula in Propositional Logic
data Formula
  -- | constants 
  = T
  | F
  -- | variables 
  | Symbol Text
  -- | the negation unary connective 
  | Not Formula
  -- | and binary connectives
  | And Formula Formula
  | Or Formula Formula
  | Implies Formula Formula
  | Iff Formula Formula
  deriving (Eq, Show, Data)

deriving instance Plated Formula

makePrisms ''Formula

instance Boolean Formula where
  true   = T
  false  = F
  not    = Not
  (&&)   = And
  (||)   = Or
  (-->)  = Implies
  (<-->) = Iff

type Model = Map Text Bool

data Polarity
  = Pos
  | Neg
  deriving (Eq, Show)

data Atom
  = T'
  | F'
  | Var Text
  deriving (Eq, Show)

data Lit = Lit
  { _polarity :: Polarity
  , _atom     :: Atom
  }
  deriving (Show, Eq)

makeLenses ''Lit

type Clause = Set Lit

type CNF = [Clause]

variableNames :: Formula -> [Text]
variableNames f = f ^.. cosmos . _Symbol

variableNamesIx :: Formula -> [(Int, Text)]
variableNamesIx f = universe f ^@.. folded . _Symbol

toBool' :: Bool -> Formula
toBool' True  = T
toBool' False = F

countTerms :: Formula -> Sum Integer
countTerms f = Sum 1 <> foldMapOf plate countTerms f

getSize :: Formula -> Integer
getSize = getSum . countTerms

applyModel :: Model -> Formula -> Formula
applyModel model = rewrite $ \case
  Symbol name -> Just . toBool' $ lookupDefault False name model
  _           -> Nothing

simplify :: Formula -> Formula
simplify = rewrite $ \case
  -- remove double negations
  (Not (Not p)) -> Just p
  -- simplify when values are available
  Not T         -> Just F
  Not F         -> Just T
  And     T T   -> Just T
  And     F _   -> Just F
  And     _ F   -> Just F
  Or      T _   -> Just T
  Or      _ T   -> Just T
  Implies F _   -> Just T
  Implies T T   -> Just T
  Implies T F   -> Just F
  Iff     T T   -> Just T
  Iff     T F   -> Just F
  Iff     F T   -> Just F
  Iff     F F   -> Just T
  _             -> Nothing

evaluate :: Model -> Formula -> Bool
evaluate m = \case
  T           -> True
  F           -> False
  Symbol name -> lookupDefault False name m
  Not    p    -> not $ evaluate m p
  And     p q -> evaluate m p && evaluate m q
  Or      p q -> evaluate m p || evaluate m q
  Implies p q -> evaluate m p --> evaluate m q
  Iff     p q -> evaluate m p == evaluate m q


useDeMorgan :: Formula -> Maybe Formula
useDeMorgan = \case
  Not (p `And` q) -> Just (Not p || Not q)
  Not (p `Or`  q) -> Just (Not p && Not q)
  _               -> Nothing

distributeConjunctions :: Formula -> Maybe Formula
distributeConjunctions = \case
  (p `And` q) `Or` r           -> Just ((p || r) && (q || r))
  r           `Or` (p `And` q) -> Just ((p || r) && (q || r))
  _                            -> Nothing

noDoubleNegations :: Formula -> Maybe Formula
noDoubleNegations = \case
  Not (Not p) -> Just p
  _           -> Nothing

onlyNNFConnectives :: Formula -> Maybe Formula
onlyNNFConnectives = \case
  p `Implies` q -> Just (Not p || q)
  p `Iff`     q -> Just ((Not p || q) && (p || q))
  _             -> Nothing

toNNF :: Formula -> Formula
toNNF = rewrite $ \x -> noDoubleNegations x <|> onlyNNFConnectives x

-- may increase the formula exponentially
-- use tseiting instead
toCNF' :: Formula -> Formula
toCNF' = rewrite $ \x ->
  noDoubleNegations x
    <|> onlyNNFConnectives x
    <|> useDeMorgan x
    <|> distributeConjunctions x


-- tseiting :: MonadState Int m => Formula -> m CNF

-- TODO 
-- convert to cnf via tseitin's transformation  
-- implement dpll
-- implement basic cdcl

