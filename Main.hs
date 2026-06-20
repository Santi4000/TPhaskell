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

-- Devuelve la precedencia de un operador (mayor número = mayor precedencia)
precedencia :: Char -> Int

-- Indica si un operador es asociativo por izquierda (relevante para desempate de precedencia igual)
esAsociativaIzquierda :: Char -> Bool

-- Distingue si un Token es un operador o un número (útil en el procesamiento)
esOperador :: Token -> Bool

-- Dado un operador y dos árboles (operando derecho e izquierdo, en ese orden de pop),
-- construye el Nodo correspondiente: Nodo (Op c) izq der
aplicarOperador :: Char -> Arbol Token -> Arbol Token -> Arbol Token

-- Toma la pila de operadores y la pila de árboles (salida), y aplica el operador
-- en el tope de la pila de operadores sobre los dos árboles en el tope de la pila de salida,
-- devolviendo las pilas actualizadas (la de operadores con uno menos, la de salida con el nuevo árbol)
aplicarTope :: Pila Char -> Pila (Arbol Token) -> (Pila Char, Pila (Arbol Token))

-- Procesa un solo Token actualizando ambas pilas según las reglas de shunting yard:
-- - si es Num, lo empuja como hoja (Nodo) en la pila de salida
-- - si es Op '(' , lo empuja en la pila de operadores
-- - si es Op ')' , va aplicando operadores hasta encontrar el '(' y lo descarta
-- - si es otro operador, aplica los operadores de mayor o igual precedencia 
--   (según asociatividad) antes de empujarlo
procesarToken :: Token -> Pila Char -> Pila (Arbol Token) -> (Pila Char, Pila (Arbol Token))

-- Recorre la lista de tokens aplicando procesarToken, acumulando el estado de ambas pilas
procesarTokens :: [Token] -> Pila Char -> Pila (Arbol Token) -> (Pila Char, Pila (Arbol Token))

-- Al terminar de procesar todos los tokens, vacía la pila de operadores restante,
-- aplicando cada uno sobre la pila de salida, hasta dejar un único árbol final
vaciarOperadores :: Pila Char -> Pila (Arbol Token) -> Arbol Token