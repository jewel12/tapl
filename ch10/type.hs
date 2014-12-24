module Type where

data Ty = TyArr Ty Ty | TyBool deriving (Eq)
data Binding = NameBind | VarBind Ty deriving (Eq)
type Context = [(Name, Binding)]

type Index = Integer
type CtxMaxLength = Integer
type Name = String

data Term = TmVar Index CtxMaxLength |
            TmAbs Name Ty Term |
            TmApp Term Term |
            TmTrue | TmFalse | TmIf Term Term Term

typeof :: Context -> Term -> Either String Ty
typeof ctx t =
    case t of
      TmVar i _ -> getTypeFromContext ctx i
      TmAbs name tyT1 t2 -> do
             let ctx' = addbinding ctx name (VarBind tyT1)
             tyT2 <- typeof ctx' t2
             return $ TyArr tyT1 tyT2
      TmApp t1 t2 -> do
            tyT1 <- typeof ctx t1
            tyT2 <- typeof ctx t2
            case tyT1 of
              TyArr tyT11 tyT12
                  | tyT2 == tyT11 -> return tyT12
                  | otherwise -> fail "parameter type mismatch"
              otherwise -> fail "arrow type expected"
      TmTrue -> return TyBool
      TmFalse -> return TyBool
      TmIf t1 t2 t3 -> do
             tyT1 <- typeof ctx t1
             tyT2 <- typeof ctx t2
             tyT3 <- typeof ctx t3
             case tyT1 of
               TyBool
                   | tyT2 == tyT3 -> return tyT2
                   | otherwise -> fail "arms of conditional have different types"
               otherwise -> fail "guard of conditional not a boolean"

addbinding :: Context -> Name -> Binding -> Context
addbinding ctx name bind = (name, bind) : ctx

getTypeFromContext :: Context -> Index -> Either String Ty
getTypeFromContext ctx i =
    case (getbinding ctx i) of
      VarBind tyT -> return tyT
      otherwise -> fail "Wrong kind of binding for variable"
    where getbinding ctx' i' = snd $ ctx' !! (fromIntegral i')

instance Show Ty where
    show TyBool = "Bool"
    show (TyArr ty1@(TyArr _ _) ty2) = "(" ++ show ty1 ++ ")" ++ "->" ++ show ty2
    show (TyArr ty1 ty2@(TyArr _ _)) = show ty1 ++ "->(" ++ show ty2 ++ ")"
    show (TyArr ty1 ty2) = show ty1 ++ "->" ++ show ty2
