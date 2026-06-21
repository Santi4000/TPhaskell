import Data.Char (isDigit, isSpace)
import Stack

data Token = Num Float | Op Char 
    deriving (Show, Eq) 
data Arbol a = Vacio | Nodo a (Arbol a) (Arbol a) 
    deriving (Show, Eq)

esNumero :: Char -> Bool
esNumero x = isDigit x || x == '.'

tokenizar :: String -> [Token]
tokenizar [] = []
tokenizar (x:xs)
    | isSpace x = tokenizar xs
    | isDigit x || x == '.' =
        let (y, ys) = span esNumero (x:xs)
        in Num (read y :: Float) : tokenizar ys
    | otherwise = Op x : tokenizar xs


precedencia :: Char -> Int
precedencia '+' = 1
precedencia '-' = 1
precedencia '*' = 2
precedencia '/' = 2
precedencia '^' = 3
precedencia _   = 0  


esAsociativaIzquierda :: Char -> Bool
esAsociativaIzquierda '+' = True
esAsociativaIzquierda '-' = True
esAsociativaIzquierda '*' = True
esAsociativaIzquierda '/' = True
esAsociativaIzquierda '^' = False
esAsociativaIzquierda _ = False


esOperador :: Token -> Bool
esOperador (Op _) = True
esOperador _ = False


aplicarOperador :: Char -> Arbol Token -> Arbol Token -> Arbol Token
aplicarOperador oper izq der = Nodo (Op oper) izq der


aplicarTope :: Pila Char -> Pila (Arbol Token) -> (Pila Char, Pila (Arbol Token))
aplicarTope (PTop op ops) (PTop der (PTop izq salida)) = (ops, PTop (aplicarOperador op izq der) salida)


procesarTokens :: [Token] -> Pila Char -> Pila (Arbol Token) -> (Pila Char, Pila (Arbol Token))
procesarTokens [] ops salida = (ops, salida)
procesarTokens (t:ts) ops salida =
    let (ops', salida') = procesarToken t ops salida
    in procesarTokens ts ops' salida'


procesarToken :: Token -> Pila Char -> Pila (Arbol Token) -> (Pila Char, Pila (Arbol Token))
procesarToken (Num n) ops salida = (ops, PTop (Nodo (Num n) Vacio Vacio) salida)
procesarToken (Op '(') ops salida = (push '(' ops, salida)
procesarToken (Op ')') ops salida =
    let (ops', salida') = vaciarMientras (/= '(') ops salida    
    in case ops' of
        PTop '(' ops'' -> (ops'', salida')
        _ -> error "Paréntesis desbalanceados"
procesarToken (Op op) ops salida =
    let (ops', salida') = vaciarMientras (\o -> esOperador (Op o) && (precedencia o > precedencia op || (precedencia o == precedencia op && esAsociativaIzquierda op))) ops salida
    in (push op ops', salida')


vaciarMientras :: (Char -> Bool) -> Pila Char -> Pila (Arbol Token) -> (Pila Char, Pila (Arbol Token))
vaciarMientras cond (PTop op ops) salida
    | cond op =
        let (ops', salida') = aplicarTope (PTop op ops) salida
        in vaciarMientras cond ops' salida'
vaciarMientras _ ops salida = (ops, salida)


vaciarOperadores :: Pila Char -> Pila (Arbol Token) -> Arbol Token
vaciarOperadores (PTop op ops) salida =
    let (ops', salida') = aplicarTope (PTop op ops) salida
    in vaciarOperadores ops' salida'
vaciarOperadores _ (PTop arbol _) = arbol


shuntingYard :: String -> Arbol Token
shuntingYard entrada =
    let tokens = tokenizar entrada
        (opsFinal, salidaFinal) = procesarTokens tokens empty empty
    in vaciarOperadores opsFinal salidaFinal
