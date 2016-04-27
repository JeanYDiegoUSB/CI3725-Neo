{
module Main (main) where
import Control.Monad
}

%wrapper "posn"

tokens :-
	$white+	;
	\,		{ \p s -> TkComa p }
	\.		{ \p s -> TkPunto p }
	\:		{ \p s -> TkDosPuntos p }
	\(		{ \p s -> TkParAbre p }
	\)		{ \p s -> TkParCierra p }
	\[		{ \p s -> TkCorcheteAbre p }
	\]		{ \p s -> TkCorcheteCierra p }
	\{		{ \p s -> TkLlaveAbre p }
	\}		{ \p s -> TkLlaveCierra p }
	"->"	{ \p s -> TkHacer p }
	"<-"	{ \p s -> TkAsignacion p }
{
-- Each action has type :: AlexPosn -> String -> Token

-- The Token type:
data Token = 
	TkComa AlexPosn	|
	TkPunto AlexPosn |
	TkDosPuntos AlexPosn |
	TkParAbre AlexPosn |
	TkParCierra AlexPosn |
	TkCorcheteAbre AlexPosn |
	TkCorcheteCierra AlexPosn |
	TkLlaveAbre AlexPosn |
	TkLlaveCierra AlexPosn |
	TkHacer AlexPosn |
	TkAsignacion AlexPosn

instance Show Token where
	show (TkComa (AlexPn _ line column)) = "TkComa" ++ " " ++ show line ++ " " ++ show column
	show (TkPunto (AlexPn _ line column)) = "TkPunto" ++ " " ++ show line ++ " " ++ show column
	show (TkDosPuntos (AlexPn _ line column)) = "TkDosPuntos" ++ " " ++ show line ++ " " ++ show column
	show (TkParAbre (AlexPn _ line column)) = "TkParAbre" ++ " " ++ show line ++ " " ++ show column
	show (TkParCierra (AlexPn _ line column)) = "TkParCierra" ++ " " ++ show line ++ " " ++ show column
	show (TkCorcheteAbre (AlexPn _ line column)) = "TkCorcheteAbre" ++ " " ++ show line ++ " " ++ show column
	show (TkCorcheteCierra (AlexPn _ line column)) = "TkCorcheteCierra" ++ " " ++ show line ++ " " ++ show column
	show (TkLlaveAbre (AlexPn _ line column))= "TkLlaveAbre" ++ " " ++ show line ++ " " ++ show column
	show (TkLlaveCierra (AlexPn _ line column))= "TkLlaveCierra" ++ " " ++ show line ++ " " ++ show column
	show (TkHacer (AlexPn _ line column))= "TkHacer" ++ " " ++ show line ++ " " ++ show column
	show (TkAsignacion (AlexPn _ line column))= "TkAsignacion" ++ " " ++ show line ++ " " ++ show column

myShow :: Show a => [a] -> String
myShow [] = ""
myShow (x:xs) = show x ++ ", " ++ myShow xs

main = do
	s <- liftM lines getContents
	(putStrLn . unlines) $ map myShow (mapM alexScanTokens s)
}