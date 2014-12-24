module Eval where

import Type
import qualified Type as T
import Parser

printtm :: Context -> Term -> String
printtm ctx t =
    case t of
      TmAbs name tyT1 t1 ->
          let (ctx', name') = pickfreshname ctx name
          in "(^" ++ name' ++ ":" ++ show tyT1 ++ ". " ++ printtm ctx' t1 ++ ") : " ++ (showTypeOfTerm t)
      TmApp t1 t2 ->
          "(" ++ printtm ctx t1 ++ " " ++ printtm ctx t2 ++ ")"
      TmVar x n
          | ctxlength ctx == n -> fst $ index2name ctx x
          | otherwise -> "[bad index]"
      TmTrue -> "true"
      TmFalse -> "false"
      TmIf t1 t2 t3 ->
          "if " ++ printtm ctx t1 ++ " then " ++ printtm ctx t2 ++ " else " ++ printtm ctx t3
      where
        index2name ctx i = ctx !! (fromIntegral i)
        ctxlength ctx = fromIntegral $ length ctx
        showTypeOfTerm t = case (T.typeof [] t) of
                             Right ty -> show ty
                             otherwise -> "typeof error" -- 型検査をしていれば otherwise になることはない

pickfreshname :: Context -> Name -> (Context, Name)
pickfreshname ctx name = pick ctx names where
    pick ctx (n:ns) =
        case (elem (n, NameBind) ctx) of
          False -> ((n, NameBind):ctx, n)
          otherwise -> pick ctx ns
    names = name : map ((name ++) . show) [1..]
            
termShift :: Integer -> Term -> Term
termShift d t =
    let
        walk c t =
            case t of
              TmVar i n
                  | i >= c -> TmVar (i + d) (n + d)
                  | otherwise -> TmVar i (n + d)
              TmAbs name tyT1 t1 ->
                  TmAbs name tyT1 (walk (c + 1) t1)
              TmTrue -> TmTrue
              TmFalse -> TmFalse
              TmIf t1 t2 t3 -> TmIf (walk c t1) (walk c t2) (walk c t3)
              TmApp t1 t2 ->
                  TmApp (walk c t1) (walk c t2)
    in walk 0 t

termSubst :: Integer -> Term -> Term -> Term
termSubst j s t =
    let
        walk c t =
            case t of
              TmVar i n
                  | i == (j + c) -> termShift c s
                  | otherwise -> TmVar i n
              TmAbs name tyT1 t1 -> TmAbs name tyT1 (walk (c + 1) t1)
              TmTrue -> TmTrue
              TmFalse -> TmFalse
              TmIf t1 t2 t3 -> TmIf (walk c t1) (walk c t2) (walk c t3)
              TmApp t1 t2 -> TmApp (walk c t1) (walk c t2)
    in walk 0 t              
       
-- 代入の場合は代入後の文脈でシフトして、変数がひとつ消費されるため -1 のシフトをする
termSubstTop :: Term -> Term -> Term
termSubstTop s t =
    termShift (-1) (termSubst 0 (termShift 1 s) t)

isval :: Context -> Term -> Bool
isval ctx t =
    case t of
      TmAbs _ _ _ -> True
      TmTrue -> True
      TmFalse -> True
      otherwise -> False

eval1 :: Context -> Term -> Maybe Term
eval1 ctx t =
    case t of
      TmApp (TmAbs x ty t12) v2
          | isval ctx v2 -> Just (termSubstTop v2 t12)
      TmApp v1 t2
          | isval ctx v1 -> do
              t2' <- eval1 ctx t2
              return (TmApp v1 t2')
      TmApp t1 t2 -> do
          t1' <- eval1 ctx t1
          return (TmApp t1' t2)
      otherwise -> Nothing

eval :: Context -> Term -> Term
eval ctx t =
    case (eval1 ctx t) of 
      Just t -> eval ctx t
      Nothing -> t
