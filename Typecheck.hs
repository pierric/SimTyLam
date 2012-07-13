module Typecheck where

import Basic
import Lam
import Text.PrettyPrint
import Text.Show.Pretty

builtinOps = fromListB [ makeB "+" (TyArrow TyInt (TyArrow TyInt TyInt))
                       , makeB "-" (TyArrow TyInt (TyArrow TyInt TyInt))
                       , makeB "*" (TyArrow TyInt (TyArrow TyInt TyInt))
                       , makeB "/" (TyArrow TyInt (TyArrow TyInt TyInt)) ]

typeCheck :: Expr -> ExprT (Either Typ TyErr)
typeCheck e = fst $ sem_AGItf (AGItf_AGItf e) builtinOps

typeCheckIO e = putStrLn $ ppShow e

pretty :: Expr -> Doc
pretty e = snd $ sem_AGItf (AGItf_AGItf e) builtinOps

