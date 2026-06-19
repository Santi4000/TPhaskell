data Token = Num Float | Op Char 
    deriving (Show, Eq) 
data Arbol a = Vacio | Nodo a (Arbol a) (Arbol a) 
    deriving (Show, Eq)

esNumero :: Char -> Bool --agarra al numero con el decimal :v
esNumero x = isDigit x || x == '.'


tokenizar :: String -> [Token]
tokenizar [] = []
tokenizar (x:xs)
    | isSpace x = tokenizar xs --esto ignora los espacios
    | isDigit x || x = '.' =  -- '.' por si es decimal
        let (y:ys)  = span esNumero (x:xs)
        in Num (read y :: Float) : tokenizar ys
    | otherwise Op x : tokenizar xs --podriamos definir una auxiliar que tome solos los operadores que usemos, pero lo dejo a tu criterio, dsp vemos

shuntingYard :: String -> Arbol Token
