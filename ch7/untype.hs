module Untype where
    
type Index = Integer
type CtxMaxLength = Integer
type Name = String
type Context = [Name]

data Term = TmVar Index CtxMaxLength | TmAbs Name Term | TmApp Term Term deriving (Show, Read)

printtm :: Context -> Term -> String
printtm ctx t =
    case t of
      TmAbs name t1 ->
          let (ctx', name') = pickfreshname ctx name
          in "(lambda " ++ name' ++ ". " ++ printtm ctx' t1 ++ ")"
      TmApp t1 t2 ->
          "(" ++ printtm ctx t1 ++ " " ++ printtm ctx t2 ++ ")"
      TmVar x n
          | ctxlength ctx == n -> index2name ctx x
          | otherwise -> "[bad index]"
      where
        index2name ctx i = ctx !! (fromIntegral i)
        ctxlength ctx = fromIntegral $ length ctx

-- | Context に無いが近い名前を生成して追加する
--
-- 名前が Context に含まれている場合:
-- >>> pickfreshname ["x"] "x"
-- (["x1","x"],"x1")
-- >>> pickfreshname ["x1","x"] "x"
-- (["x2","x1","x"],"x2")
--
-- 名前が Context に含まれていない場合:
-- >>> pickfreshname ["x"] "y"
-- (["y","x"],"y")
pickfreshname :: Context -> Name -> (Context, Name)
pickfreshname ctx name = pick ctx names where
    pick ctx (n:ns) =
        case (elem n ctx) of
          False -> (n:ctx, n)
          otherwise -> pick ctx ns
    names = name : map ((name ++) . show) [1..]
            
-- |
-- TmVar:
-- >>> termShift 1 (TmVar 0 0)
-- TmVar 1 1
-- >>> termShift 2 (TmAbs "x" (TmAbs "y" (TmApp (TmVar 1 2) (TmApp (TmVar 0 2) (TmVar 2 2)))))
-- TmAbs "x" (TmAbs "y" (TmApp (TmVar 1 4) (TmApp (TmVar 0 4) (TmVar 4 4))))
termShift :: Integer -> Term -> Term
termShift d t =
    let
        walk c t =
            case t of
              TmVar i n
                  | i >= c -> TmVar (i+d) (n+d)
                  | otherwise -> TmVar i (n+d)
              TmAbs name t1 ->
                  TmAbs name (walk (c+1) t1)
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
              TmAbs name t1 -> TmAbs name (walk (c + 1) t1)
              TmApp t1 t2 -> TmApp (walk c t1) (walk c t2)
    in walk 0 t              

-- 代入の場合は代入後の文脈でシフトして、変数がひとつ消費されるため -1 のシフトをする
termSubstTop :: Term -> Term -> Term
termSubstTop s t =
    termShift (-1) (termSubst 0 (termShift 1 s) t)

isval :: Context -> Term -> Bool
isval ctx t =
    case t of
      TmAbs _ _ -> True
      otherwise -> False

eval1 :: Context -> Term -> Maybe Term
eval1 ctx t =
    case t of
      TmApp (TmAbs x t12) v2
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
