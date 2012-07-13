{-# LANGUAGE FlexibleContexts, Rank2Types #-}
module Parser where

import Text.Parsec
import Text.Parsec.Language
import Text.Parsec.Token
import Text.Parsec.String
import Text.Parsec.Expr
import Control.Applicative hiding ((<|>), many)
import Basic
import Lam hiding (angles)

pNatural     = fromInteger <$> natural haskell
pInteger     = fromInteger <$> integer haskell
pCharLiteral = charLiteral haskell  
pIdentifier  = identifier haskell
pParens      = parens haskell
pAngles      = angles haskell
pKeyword     = reserved haskell
pComma       = comma haskell
pKeyOp       = reservedOp haskell

pInt  = Expr_IConst       <$> pNatural
pChar = Expr_CConst       <$> pCharLiteral
pVar  = Expr_Var          <$> pIdentifier
pParensExpr = Expr_Parens <$> pParens pExpr

pLet = Expr_Let <$   pKeyword "let" 
                <*>  pDecls
                <*>  pPattern
                <*   pKeyOp "="
                <*>  pExpr
                <*   pKeyword "in"
                <*>  pExpr

pLam = Expr_Lam <$   pKeyOp "\\"
                <*>  pPattern 
                <*   pKeyOp "::"
                <*>  pTyExpr
                <*   pKeyOp "->"
                <*>  pExpr

pProd = do (e1,e2) <- pParens ((,) <$> pExpr 
                                   <*  pComma 
                                   <*> pExpr)
           return $ Expr_Prod e1 e2

pSum  = do (e1,e2) <- pAngles ((,) <$> pExpr
                                   <*  pComma
                                   <*> pExpr)
           return $ Expr_Sum e1 e2 

pAtomicExpr = pInt        <|> 
              pChar       <|>
              pVar        <|>
              pLet        <|> 
              pLam        <|> 
              try pProd   <|>
              pSum        <|>
              pParensExpr    

pAppExpr = foldl1 Expr_App <$> many1 pAtomicExpr


builtinOps   = [ [makePre  "-", makePre  "+" ]
               , [makeBinL "*", makeBinL "/" ]
               , [makeBinL "+", makeBinL "-" ] ]
  where makePre  op = Prefix (pKeyOp op >> return (\e -> Expr_App (Expr_OpPre op) e))
        makeBinL op = Infix  (pKeyOp op >> return (\e1 e2 -> Expr_App (Expr_App (Expr_OpBin op) e1) e2)) AssocLeft

pExpr = buildExpressionParser builtinOps pAppExpr

pPattern = pPatInt <|> pPatChar <|> pPatVar <|> pPatVarAs <|> pPatProd <|> pParensPattern

pPatInt  = PatExpr_IConst <$> pInteger
pPatChar = PatExpr_CConst <$> pCharLiteral
pPatVar  = PatExpr_Var    <$> pIdentifier
pPatVarAs= PatExpr_VarAs  <$> pIdentifier
                          <*  pKeyOp "@"
                          <*> pPattern
pPatProd = do (p1,p2) <- pParens ((,) <$> pPattern
                                      <*  pComma
                                      <*> pPattern)
              return $ PatExpr_Prod p1 p2
pPatSum  = do (p1,p2) <- pAngles ((,) <$> pPattern
                                      <*  pComma
                                      <*> pPattern)
              return $ PatExpr_Sum p1 p2
pParensPattern = PatExpr_Parens <$> pParens pPattern

                    
pDecls = many1 (try pDecl)
  where pDecl = Decl_Decl <$> pIdentifier
                          <*  pKeyOp "::"
                          <*> pTyExpr

pTyExpr  = pTyInt <|> pTyChar <|> pTyProd <|> pTySum <|> pTyArrow <|> pParensTyExpr
pTyInt   = TyExpr_Int   <$  pKeyword "Int"
pTyChar  = TyExpr_Char  <$  pKeyword "Char"
pTyProd  = do (t1,t2) <- pParens ((,) <$> pTyExpr
                                      <*  pComma
                                      <*> pTyExpr)
              return $ TyExpr_Prod t1 t2
pTySum   = do (t1,t2) <- pAngles ((,) <$> pTyExpr
                                      <*  pComma
                                      <*> pTyExpr)
              return $ TyExpr_Sum  t1 t2 
pTyArrow = TyExpr_Arrow <$> pTyExpr
                        <*  pKeyOp "->"
                        <*> pTyExpr
pParensTyExpr = TyExpr_Parens <$> pParens pTyExpr

parseBuffer = parse pExpr "<BUFFER>"
parse' c    = parse c "<BUFFER USER>"
