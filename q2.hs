newtype Parser a = Parser { executarParser :: String -> [(a, String)] }

elemento :: Parser Char
elemento = Parser $ \entrada -> case entrada of
    []     -> []  
    (x:xs) -> [(x, xs)]  

aplicar :: Parser a -> String -> [(a, String)]
aplicar (Parser p) entrada = p entrada

retornar :: a -> Parser a
retornar v = Parser $ \entrada -> [(v, entrada)]

falha :: Parser a
falha = Parser $ \_ -> []

ou :: Parser a -> Parser a -> Parser a
p1 ou p2 = Parser $ \entrada -> case aplicar p1 entrada of
    [] -> aplicar p2 entrada  
    resultado -> resultado


satisfaz :: (Char -> Bool) -> Parser Char
satisfaz condicao = elemento >>= \x ->
    if condicao x then retornar x else falha


caractere :: Char -> Parser Char
caractere c = satisfaz (== c)


digito :: Parser Char
digito = satisfaz (elem ['0'..'9'])

letra :: Parser Char
letra = satisfaz (elem ['a'..'z'] ++ ['A'..'Z'])

cadeia :: String -> Parser String
cadeia [] = retornar []
cadeia (x:xs) = caractere x >>= \_ ->
                cadeia xs >>= \_ ->
                retornar (x:xs)

muitos :: Parser a -> Parser [a]
muitos p = aoMenosUmaVez p ou retornar []

aoMenosUmaVez :: Parser a -> Parser [a]
aoMenosUmaVez p = p >>= \v ->
                  muitos p >>= \vs ->
                  retornar (v:vs)

instance Functor Parser where
    fmap f p = Parser $ \entrada -> [(f a, resto) | (a, resto) <- aplicar p entrada]

instance Applicative Parser where
    pure = retornar
    pf <*> px = Parser $ \entrada -> [(f x, resto2) | (f, resto1) <- aplicar pf entrada, (x, resto2) <- aplicar px resto1]

instance Monad Parser where
    return = retornar
    p >>= f = Parser $ \entrada -> concat [aplicar (f a) resto | (a, resto) <- aplicar p entrada]

data Expressao = Soma Expressao Expressao
               | Multiplica Expressao Expressao
               | Valor Int
               deriving Show

expressao :: Parser Expressao
expressao = termo >>= \t ->
            (caractere '+' >> expressao >>= \e ->
             retornar (Soma t e)) ou retornar t

termo :: Parser Expressao
termo = fator >>= \f ->
        (caractere '*' >> termo >>= \t ->
         retornar (Multiplica f t)) ou retornar f

fator :: Parser Expressao
fator = (digito >>= \d ->
         retornar (Valor (read [d]))) ou
        (caractere '(' >> expressao >>= \e ->
         caractere ')' >> retornar e)

--exemplo de teste
main :: IO ()
main = do
    let entrada = "3+5*2"
    print $ aplicar expressao entrada