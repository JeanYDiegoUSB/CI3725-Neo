{
{-
	LexNeo
	Description: Lexer generator file for language Neo
	
	Author: Br. Jean Paul Alexander Lacour 12-10848
			Br. Diego Daniel Pedroza Perez 12-11281
	
	Last modification date: 03-05-16
-}
module Main (main) where
import System.Environment
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-
    $white+             ;
    (\%\{([^\}]|[\n]|(\{+([^\}\%]|[\n])))*\}+\%)|(\%\%.*)    ;
    with				{ \p s -> TkWith p }
    begin				{ \p s -> TkBegin p }
    end					{ \p s -> TkEnd p }
    if					{ \p s -> TkIf p }
    var					{ \p s -> TkVar p }
    int					{ \p s -> TkInt p }
    bool				{ \p s -> TkBool p }
    char				{ \p s -> TkChar p }
    matrix				{ \p s -> TkMatrix p }
    of					{ \p s -> TkOf p }
    for					{ \p s -> TkFor p }
    from				{ \p s -> TkFrom p }
    to					{ \p s -> TkTo p }
    print				{ \p s -> TkPrint p }
    otherwise				{ \p s -> TkOtherwise p }
    step				{ \p s -> TkStep p }
    while				{ \p s -> TkWhile p }
    read				{ \p s -> TkRead p }
    False				{ \p s -> TkFalse p }
    True				{ \p s -> TkTrue p }
    \,					{ \p s -> TkComa p }
    \.					{ \p s -> TkPunto p }
    \:					{ \p s -> TkDosPuntos p }
    \(					{ \p s -> TkParAbre p }
    \)					{ \p s -> TkParCierra p }
    \[					{ \p s -> TkCorcheteAbre p }
    \]					{ \p s -> TkCorcheteCierra p }
    \{					{ \p s -> TkLlaveAbre p }
    \}					{ \p s -> TkLlaveCierra p }
    "->"				{ \p s -> TkHacer p }
    "<-"				{ \p s -> TkAsignacion p }
    \+					{ \p s -> TkSuma p }
    \-					{ \p s -> TkResta p }
    \*					{ \p s -> TkMult p }
    \/					{ \p s -> TkDiv p }
    \%					{ \p s -> TkMod p }
    "/\"				{ \p s -> TkConjuncion p }
    "\/"				{ \p s -> TkDisyuncion p }
    not					{ \p s -> TkNegacion p }
    \<					{ \p s -> TkMenor p }
    "<="				{ \p s -> TkMenorIgual p }
    \>					{ \p s -> TkMayor p }
    ">="				{ \p s -> TkMayorIgual p }
    \=					{ \p s -> TkIgual p }
    "++"				{ \p s -> TkSiguienteCar p }
    "--"				{ \p s -> TkAnteriorCar p }
    \#					{ \p s -> TkValorAscii p }
    "::"				{ \p s -> TkConcatenacion p }
    \$					{ \p s -> TkRotacion p }
    \?					{ \p s -> TkTrasposicion p }
    \'[\ a-zA-Z0-9]\'			{ \p s -> TkCaracter p s}
    $digit+				{ \p s -> TkNum p (read s) }
    $alpha [$alpha $digit \_]*		{ \p s -> TkId p s }
    (\%\{([^\}]|[\n]|(\{+([^\}\%]|[\n])))*)|(\}\%)|(.)    { \p s -> TkError p s}

{
-- Each action has type :: AlexPosn -> String -> Token

-- The token type:
data Token =
-- Neo's Keywords
	TkWith AlexPosn			|
	TkBegin AlexPosn		|
	TkEnd AlexPosn			|
	TkIf AlexPosn			|
	TkVar AlexPosn			|
	TkInt AlexPosn			|
	TkBool AlexPosn			|
	TkChar AlexPosn			|
	TkMatrix AlexPosn		|
	TkOf AlexPosn			|
	TkFor AlexPosn			|
	TkFrom AlexPosn			|
	TkTo AlexPosn			|
	TkPrint AlexPosn		|
	TkOtherwise AlexPosn		|
	TkStep AlexPosn			|
	TkWhile AlexPosn		|
	TkRead AlexPosn			|
	TkFalse AlexPosn		|
	TkTrue AlexPosn			|

-- Neo's separators
	TkComa AlexPosn			|
	TkPunto AlexPosn		|
	TkDosPuntos AlexPosn		|
	TkParAbre AlexPosn		|
	TkParCierra AlexPosn		|
	TkCorcheteAbre AlexPosn		|
	TkCorcheteCierra AlexPosn	|
	TkLlaveAbre AlexPosn		|
	TkLlaveCierra AlexPosn		|
	TkHacer AlexPosn		|
	TkAsignacion AlexPosn		|

-- Neo's operators
	TkSuma AlexPosn			|
	TkResta AlexPosn		|
	TkMult AlexPosn			|
	TkDiv AlexPosn			|
	TkMod AlexPosn			|
	TkConjuncion AlexPosn		|
	TkDisyuncion AlexPosn		|
	TkNegacion AlexPosn		|
	TkMenor AlexPosn		|
	TkMenorIgual AlexPosn		|
	TkMayor AlexPosn		|
	TkMayorIgual AlexPosn		|
	TkIgual AlexPosn		|
	TkSiguienteCar AlexPosn		|
	TkAnteriorCar AlexPosn		|
	TkValorAscii AlexPosn		|
	TkConcatenacion AlexPosn	|
	TkRotacion AlexPosn		|
	TkTrasposicion AlexPosn		|
	TkCaracter AlexPosn String	|
	TkNum AlexPosn Int		|
	TkId AlexPosn String		|

-- Error Token
	TkError AlexPosn String
    	deriving (Eq)

-- Token type as an instance of Show type class
instance Show Token where
	show (TkWith (AlexPn _ line column)) = "TkWith" ++ " " ++ show line ++ " " ++ show column
	show (TkBegin (AlexPn _ line column)) = "TkBegin" ++ " " ++ show line ++ " " ++ show column
	show (TkEnd (AlexPn _ line column)) = "TkEnd" ++ " " ++ show line ++ " " ++ show column
	show (TkIf (AlexPn _ line column)) = "TkIf" ++ " " ++ show line ++ " " ++ show column
	show (TkVar (AlexPn _ line column)) = "TkVar" ++ " " ++ show line ++ " " ++ show column
	show (TkInt (AlexPn _ line column)) = "TkInt" ++ " " ++ show line ++ " " ++ show column
	show (TkBool (AlexPn _ line column)) = "TkBool" ++ " " ++ show line ++ " " ++ show column
	show (TkChar (AlexPn _ line column)) = "TkChar" ++ " " ++ show line ++ " " ++ show column
	show (TkMatrix (AlexPn _ line column)) = "TkMatrix" ++ " " ++ show line ++ " " ++ show column
	show (TkOf (AlexPn _ line column)) = "TkOf" ++ " " ++ show line ++ " " ++ show column
	show (TkFor (AlexPn _ line column)) = "TkFor" ++ " " ++ show line ++ " " ++ show column
	show (TkFrom (AlexPn _ line column)) = "TkFrom" ++ " " ++ show line ++ " " ++ show column
	show (TkTo (AlexPn _ line column)) = "TkTo" ++ " " ++ show line ++ " " ++ show column
	show (TkPrint (AlexPn _ line column)) = "TkPrint" ++ " " ++ show line ++ " " ++ show column
	show (TkOtherwise (AlexPn _ line column)) = "TkOtherwise" ++ " " ++ show line ++ " " ++ show column
	show (TkStep (AlexPn _ line column)) = "TkStep" ++ " " ++ show line ++ " " ++ show column
	show (TkWhile (AlexPn _ line column)) = "TkWhile" ++ " " ++ show line ++ " " ++ show column
	show (TkRead (AlexPn _ line column)) = "TkRead" ++ " " ++ show line ++ " " ++ show column
	show (TkFalse (AlexPn _ line column)) = "TkFalse" ++ " " ++ show line ++ " " ++ show column
	show (TkTrue (AlexPn _ line column)) = "TkTrue" ++ " " ++ show line ++ " " ++ show column
	show (TkComa (AlexPn _ line column)) = "TkComa" ++ " " ++ show line ++ " " ++ show column
	show (TkPunto (AlexPn _ line column)) = "TkPunto" ++ " " ++ show line ++ " " ++ show column
	show (TkDosPuntos (AlexPn _ line column)) = "TkDosPuntos" ++ " " ++ show line ++ " " ++ show column
	show (TkParAbre (AlexPn _ line column)) = "TkParAbre" ++ " " ++ show line ++ " " ++ show column
	show (TkParCierra (AlexPn _ line column)) = "TkParCierra" ++ " " ++ show line ++ " " ++ show column
	show (TkCorcheteAbre (AlexPn _ line column)) = "TkCorcheteAbre" ++ " " ++ show line ++ " " ++ show column
	show (TkCorcheteCierra (AlexPn _ line column)) = "TkCorcheteCierra" ++ " " ++ show line ++ " " ++ show column
	show (TkLlaveAbre (AlexPn _ line column)) = "TkLlaveAbre" ++ " " ++ show line ++ " " ++ show column
	show (TkLlaveCierra (AlexPn _ line column)) = "TkLlaveCierra" ++ " " ++ show line ++ " " ++ show column
	show (TkHacer (AlexPn _ line column)) = "TkHacer" ++ " " ++ show line ++ " " ++ show column
	show (TkAsignacion (AlexPn _ line column)) = "TkAsignacion" ++ " " ++ show line ++ " " ++ show column
	show (TkSuma (AlexPn _ line column)) = "TkSuma" ++ " " ++ show line ++ " " ++ show column
	show (TkResta (AlexPn _ line column)) = "TkResta" ++ " " ++ show line ++ " " ++ show column
	show (TkMult (AlexPn _ line column)) = "TkMult" ++ " " ++ show line ++ " " ++ show column
	show (TkDiv (AlexPn _ line column)) = "TkDiv" ++ " " ++ show line ++ " " ++ show column
	show (TkMod (AlexPn _ line column)) = "TkMod" ++ " " ++ show line ++ " " ++ show column
	show (TkConjuncion (AlexPn _ line column)) = "TkConjuncion" ++ " " ++ show line ++ " " ++ show column
	show (TkDisyuncion (AlexPn _ line column)) = "TkDisyuncion" ++ " " ++ show line ++ " " ++ show column
	show (TkNegacion (AlexPn _ line column)) = "TkNegacion" ++ " " ++ show line ++ " " ++ show column
	show (TkMenor (AlexPn _ line column)) = "TkMenor" ++ " " ++ show line ++ " " ++ show column
	show (TkMenorIgual (AlexPn _ line column)) = "TkMenorIgual" ++ " " ++ show line ++ " " ++ show column
	show (TkMayor (AlexPn _ line column)) = "TkMayor" ++ " " ++ show line ++ " " ++ show column
	show (TkMayorIgual (AlexPn _ line column)) = "TkMayorIgual" ++ " " ++ show line ++ " " ++ show column
	show (TkIgual (AlexPn _ line column)) = "TkIgual" ++ " " ++ show line ++ " " ++ show column
	show (TkSiguienteCar (AlexPn _ line column)) = "TkSiguienteCar" ++ " " ++ show line ++ " " ++ show column
	show (TkAnteriorCar (AlexPn _ line column)) = "TkAnteriorCar" ++ " " ++ show line ++ " " ++ show column
	show (TkValorAscii (AlexPn _ line column)) = "TkValorAscii" ++ " " ++ show line ++ " " ++ show column
	show (TkConcatenacion (AlexPn _ line column)) = "TkConcatenacion" ++ " " ++ show line ++ " " ++ show column
	show (TkRotacion (AlexPn _ line column)) = "TkRotacion" ++ " " ++ show line ++ " " ++ show column
	show (TkTrasposicion (AlexPn _ line column)) = "TkTrasposicion" ++ " " ++ show line ++ " " ++ show column
	show (TkCaracter (AlexPn _ line column) s) = "TkCaracter("++ (init . tail $ show s) ++ ") " ++ show line ++ " " ++ show column
	show (TkNum (AlexPn _ line column) i )= "TkNum(" ++ show i ++ ") " ++ show line ++ " " ++ show column
	show (TkId (AlexPn _ line column) s) = "TkId(" ++ show s ++ ") " ++ show line ++ " " ++ show column
	show (TkError (AlexPn _ line column) s) = if (length s) <= 2
                                                	then "Error: Caracter inesperado " ++ show s ++ " en la fila " ++ show line ++ ", columna " ++ show column
                                                	else "Error: Comentario que comienza en la fila " ++ show line ++ ", columna " ++ show column ++ " no termina en }%"
-- show for List of Token
myShow :: [Token] -> String
myShow [] = ""
myShow (x:xs) = show x ++ "\n" ++ myShow xs

-- print for List of Token
myPrint :: [Token] -> IO()
myPrint a = putStr $ myShow a

-- Main program
main :: IO()
main = do
    args <- getArgs -- command line arguments
    let handle = head args -- first argument (file.neo)
    s <- readFile handle -- reading the file 
    let tokens = alexScanTokens s -- List of Token
    let result = filter f tokens -- courtesy of Ricardo Monascal
                    where
                        f :: Token -> Bool
                        f (TkError _ _) = True
                        f _ = False
    if result /= []
        then myPrint result
        else myPrint tokens
}
