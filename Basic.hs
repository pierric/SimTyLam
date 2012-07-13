module Basic where

type HsName = String

data Typ = TyInt 
         | TyChar 
         | TyProd Typ Typ 
         | TySum Typ Typ 
         | TyArrow Typ Typ 
         | TyAny
         deriving (Eq, Show)

tyEq TyAny _ = True
tyEq _ TyAny = True
tyEq a b     = a == b

data TyPatErr = TyPatNoSig
              | TyPatClash
              | TyPatOK
              deriving (Eq, Show)
tyPatErrPlus a b = a
tyPatErrToTyErr  = TyPatErr

data TyErr = TyErrUnbounded 
           | TyErrNoBinding
           | TyErrMismatch
           | TyPatErr TyPatErr
           deriving (Eq, Show)

type Binding  = (HsName, Typ)
type Bindings = [Binding]

makeB :: HsName   -> Typ -> Binding
makeB = (,)

typeB :: Binding  -> Typ
typeB = snd

emptyB :: Bindings
emptyB = []

pushB :: Binding  -> Bindings -> Bindings
pushB = (:)

popB  :: Bindings -> HsName   -> Maybe (Binding, Bindings)
popB binds nam = go binds []
  where
    go [] _                          = Nothing
    go (b:bs) visited | fst b == nam = Just (b, reverse visited ++ bs)
                      | otherwise    = go bs (b:visited)

singletonB :: Binding  -> Bindings
singletonB = (:[])

concatB    :: Bindings -> Bindings -> Bindings
concatB = (++)

fromListB  :: [Binding] -> Bindings
fromListB = id

lookupB    :: HsName -> Bindings -> Maybe Typ
lookupB = lookup

