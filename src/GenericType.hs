{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GenericType () where

import Data.Kind (Constraint)
import Data.Proxy (Proxy (..))

class Prod0 impl return | impl -> return where
  ini :: Proxy impl -> return

class Prod typ impl return | impl -> return where
  prod ::  Proxy impl -> typ -> return -> return

class CoProd impl return | impl -> return where
  coProd :: Proxy impl -> String -> return -> return


class Gen input where
  type Rep impl input return :: Constraint
  gto :: input -> Proxy impl -> forall a. (Rep impl input a) => a



data Expr 
  = C Int 
  | Add Expr Expr
  | Minus Expr Expr

instance Gen Expr where
  type Rep impl Expr r = (Prod0 impl r, Prod Int impl r, CoProd impl r, Prod Expr impl r)
  gto :: Expr -> Proxy impl -> forall a. (Rep impl Expr a) => a
  gto (C a) impl = coProd impl "C" $ prod impl a $ ini impl
  gto (Add l r) impl = coProd impl "Add" $ prod impl l $ prod impl r $ ini impl
  gto (Minus l r) impl = coProd impl "Minus" $ prod impl l $ prod impl r $ ini impl

data Stringify

instance Prod0 Stringify String where
  ini ::Proxy Stringify -> String
  ini _ = ""

instance Prod Int Stringify String where
  prod :: Proxy Stringify -> Int -> String -> String
  prod _ i thunk = show i <> thunk

instance CoProd Stringify String where
  coProd :: Proxy Stringify -> String -> String -> String
  coProd _ dataName str = dataName  ++ " " ++ str

instance Prod Expr Stringify String where
  prod :: Proxy Stringify -> Expr -> String -> String
  prod impl expr str = gto expr impl ++ " " ++ str



testExpr = gto (Add (C 5) (C 3)) (Proxy @Stringify)
testWithUndefined = gto (Add (C 5) (C 3)) (undefined :: Proxy Stringify)

