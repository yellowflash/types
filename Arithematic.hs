import Parser

data Term = TmTrue 
          | TmFalse
          | TmZero
          | TmIf Term Term Term
          | TmSucc Term
          | TmPred Term
          | TmIsZero Term
            deriving Show

isnumeric (TmSucc t) = isnumeric t
isnumeric TmZero = True
isnumeric _ = False

isval TmTrue = True
isval TmFalse = True
isval t
  | isnumeric t = True
  | otherwise = False                       


eval1 (TmIf TmTrue t1 _) = t1
eval1 (TmIf TmFalse _ t2) = t2
eval1 (TmIf t1 t2 t3) = TmIf (eval1 t1) t2 t3
eval1 (TmIsZero TmZero) = TmTrue
eval1 (TmIsZero (TmSucc t))
  | isnumeric t = TmFalse
eval1 (TmIsZero t) = (TmIsZero (eval1 t))
eval1 (TmSucc t) = (TmSucc (eval1 t))
eval1 (TmPred (TmSucc t))
  | isnumeric t = t
eval1 (TmPred TmZero) = TmZero
eval1 (TmPred t) = (TmPred (eval1 t))
eval1 _ = error "No Rule matches"


evaluate t
  | isval t = t
  | otherwise = evaluate (eval1 t)

match p v = do
  x <- p
  return v

term = token ((match (string "true") TmTrue) +++ (match (string "false") TmFalse) +++ (match (string "0") TmZero))
       +++ ifexpression
       +++ succexp
       +++ predexp
       +++ iszeroexp

ifexpression = do
  token (string "if")
  t1 <- term
  token (string "then")
  t2 <- term
  token (string "else")
  t3 <- term
  return (TmIf t1 t2 t3)

succexp = do
  token (string "succ")
  t <- term
  return (TmSucc t)

predexp = do
  token (string "pred")
  t <- term
  return (TmPred t)

iszeroexp = do
  token (string "iszero")
  t <- term
  return (TmZero)


run s = case (parse term s) of
  (Just (t,[])) -> evaluate t
  _ -> error "Parsing failed"
  
