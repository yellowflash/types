module Parser where

data Parser a = Parser (String ->  (Maybe (a,String)))

parse (Parser p) xs = p xs

instance Monad Parser where
  p >>= n = Parser (\inp -> case (parse p inp) of
                                 Nothing -> Nothing
                                 Just (x,xs) -> parse (n x) xs)
  return a = Parser (\inp -> Just (a,inp))


p +++ p1 = (Parser (\inp -> case (parse p inp) of
               Nothing -> (parse p1 inp)
               t -> t))

failure = Parser (\inp -> Nothing)
item = Parser (\inp -> case inp of
                  [] -> Nothing
                  (x:xs) -> Just (x,xs))

sat p = Parser (\inp -> case (parse item inp) of
                   Just (x,xs) | (p x) -> Just (x,xs)
                               | otherwise -> Nothing
                   Nothing -> Nothing)

many1 p = do
  v <- p
  vs <- many p
  return (v:vs)


many p = (many1 p +++ return [])

token p = do
  space
  v <- p
  space
  return v

space = many (char ' ')
char c = (sat ((==) c))

string [] = return []
string (t:ts) = do
  c <- char t
  cs <- (string ts)
  return (c:cs)
