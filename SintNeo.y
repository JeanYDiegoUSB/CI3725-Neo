{
{-
	SintNeo
	Description: Parser generator file for language Neo
	
	Author: Br. Jean Paul Alexander Lacour 12-10848
			Br. Diego Daniel Pedroza Perez 12-11281
	
	Last modification date: 22-05-16
-}
module SintNeo where

import LexNeo
}

%name parse
%tokentype { Token }
%error { parseError }

%token
    with                { TkWith (AlexPn _ _ _) }
    begin               { TkBegin (AlexPn _ _ _) }
    end                 { TkEnd (AlexPn _ _ _) }
    if                  { TkIf (AlexPn _ _ _) }
    var                 { TkVar (AlexPn _ _ _) }
    INT                 { TkInt (AlexPn _ _ _) }
    BOOL                { TkBool (AlexPn _ _ _) }
    CHAR                { TkChar (AlexPn _ _ _) }
    matrix              { TkMatrix (AlexPn _ _ _) }
    of                  { TkOf (AlexPn _ _ _) }
    for                 { TkFor (AlexPn _ _ _) }
    from                { TkFrom (AlexPn _ _ _) }
    to                  { TkTo (AlexPn _ _ _) }
    print               { TkPrint (AlexPn _ _ _) }
    otherwise           { TkOtherwise (AlexPn _ _ _) }
    step                { TkStep (AlexPn _ _ _) }
    while               { TkWhile (AlexPn _ _ _) }
    read                { TkRead (AlexPn _ _ _) }
    ident               { TkId (AlexPn _ _ _) $$ }
    num                 { TkNum (AlexPn _ _ _) $$ }
    car                 { TkCaracter (AlexPn _ _ _) $$ }
    false               { TkFalse (AlexPn _ _ _) }
    true                { TkTrue (AlexPn _ _ _) }
    ','                 { TkComa (AlexPn _ _ _) }
    '.'                 { TkPunto (AlexPn _ _ _) }
    ':'                 { TkDosPuntos (AlexPn _ _ _) }
    '('                 { TkParAbre (AlexPn _ _ _) }
    ')'                 { TkParCierra (AlexPn _ _ _) }
    '['                 { TkCorcheteAbre (AlexPn _ _ _) }
    ']'                 { TkCorcheteCierra (AlexPn _ _ _) }
    '{'                 { TkLlaveAbre (AlexPn _ _ _) }
    '}'                 { TkLlaveCierra (AlexPn _ _ _) }
    '->'		{ TkHacer (AlexPn _ _ _) }
    '<-'		{ TkAsignacion (AlexPn _ _ _) }
    '+'                 { TkSuma (AlexPn _ _ _) }
    '-'                 { TkResta (AlexPn _ _ _) }
    '*'                 { TkMult (AlexPn _ _ _) }
    '/'			{ TkDiv (AlexPn _ _ _) }
    '%'			{ TkMod (AlexPn _ _ _) }
    '/\\'		{ TkConjuncion (AlexPn _ _ _) }
    '\\/'		{ TkDisyuncion (AlexPn _ _ _) }
    not			{ TkNegacion (AlexPn _ _ _) }
    '<'			{ TkMenor (AlexPn _ _ _) }
    '<='		{ TkMenorIgual (AlexPn _ _ _) }
    '>'			{ TkMayor (AlexPn _ _ _) }
    '>='		{ TkMayorIgual (AlexPn _ _ _) }
    '='			{ TkIgual (AlexPn _ _ _) }
    '++'		{ TkSiguienteCar (AlexPn _ _ _) }
    '--'		{ TkAnteriorCar (AlexPn _ _ _) }
    '#'			{ TkValorAscii (AlexPn _ _ _) }
    '::'		{ TkConcatenacion (AlexPn _ _ _) }
    '$'			{ TkRotacion (AlexPn _ _ _) }
    '?'			{ TkTrasposicion (AlexPn _ _ _) }

%%

S : with var ID ':' Tipo begin INSTR end { ProgAlc $3 $5 $7 }
    | begin INSTR end { Prog $2 }

INSTR : with var ID ':' Tipo begin INSTR end { Alcance $3 $5 $7 }
    | begin INSTR end { AlcanceSD $2 }
    | ident '<-' EXP '.' { Asignacion $1 $3 }
    | INSTR INSTR { Secuenciacion $1 $2 }
    | if EXP '->' INSTR end { Condicional $2 $4 }
    | if EXP '->' INSTR otherwise '->' INSTR end { CondicionalO $2 $4 $7 }
    | for ident from EXP to EXP '->' INSTR end { RepeticionDet $2 $4 $6 $8 }
    | for ident from EXP to EXP step EXP '->' INSTR end { RepeticionDetS $2 $4 $6 $8 $10 }
    | while EXP '->' INSTR end { RepeticionInd $2 $4 }
    | read ident '.' { Leer $2 }
    | print EXP '.' { Imprimir $2 }

EXP : EXP '+' EXP { Suma $1 $3 }
    | EXP '-' EXP { Resta $1 $3 }
    | EXP '*' EXP { Mult $1 $3 }
    | EXP '/' EXP { Div $1 $3 }
    | EXP '%' EXP { Mod $1 $3 }
    | '-' EXP { Negativo $2 }
    | EXP '\\/' EXP { Disyuncion $1 $3 }
    | EXP '/\\' EXP { Conjuncion $1 $3 }
    | not EXP { Negacion $2 }
    | EXP '++' { CarSig $1 }
    | EXP '--' { CarAnt $1 }
    | '#' EXP { ValorAscii $2 }
    | EXP '::' EXP { Concatenacion $1 $3 }
    | '$' EXP { Rotacion $2 }
    | EXP '?' { Transposicion $1 }
    | EXP '<' EXP { Menor $1 $3 }
    | EXP '<=' EXP { MenorIgual $1 $3 }
    | EXP '>' EXP { Mayor $1 $3 }
    | EXP '>=' EXP { MayorIgual $1 $3 }
    | EXP '=' EXP { Igual $1 $3 }
    | EXP '/''=' EXP { Desigual $1 $4 }
    | '(' EXP ')' { Parent $2 }
    | ident { IDENT $1 }
    | car { CAR $1 }
    | num { NUM $1 }
    | false { Booleano False }
    | true { Booleano True }
    | '{'EXP'}' { Matriz $2 }
    | EXP'['EXP']' { Index $1 $3 }
    | EXP ',' EXP { Separacion $1 $3 }

ID : ID ',' ident { ListIdent ((Ident_ $3) : (getIdent $1)) }
    | ident { ListIdent [Ident_ $1] }

Tipo : CHAR { TipoChar }
    | INT { TipoInt }
    | BOOL { TipoBool }
    | matrix EXP of Tipo { TipoMatrix $2 $4 }
{
parseError :: [Token] -> a
parseError _ = error "Parse error"

data S = ProgAlc ID Tipo INSTR
    | Prog INSTR
    deriving(Eq,Show)

data INSTR = Alcance ID Tipo INSTR
    | AlcanceSD INSTR
    | Asignacion String EXP
    | Secuenciacion INSTR INSTR
    | Condicional EXP INSTR
    | CondicionalO EXP INSTR INSTR
    | RepeticionDet String EXP EXP INSTR
    | RepeticionDetS String EXP EXP EXP INSTR
    | RepeticionInd EXP INSTR
    | Leer String
    | Imprimir EXP
    deriving(Eq,Show)

data EXP = Suma EXP EXP
    | Resta EXP EXP
    | Mult EXP EXP
    | Div EXP EXP
    | Mod EXP EXP
    | Negativo EXP
    | Disyuncion EXP EXP
    | Conjuncion EXP EXP
    | Negacion EXP
    | CarSig EXP
    | CarAnt EXP
    | ValorAscii EXP
    | Concatenacion EXP EXP
    | Rotacion EXP
    | Transposicion EXP
    | Menor EXP EXP
    | MenorIgual EXP EXP
    | Mayor EXP EXP
    | MayorIgual EXP EXP
    | Igual EXP EXP
    | Desigual EXP EXP
    | Parent EXP
    | IDENT String
    | CAR String
    | NUM Int
    | Booleano Bool
    | Matriz EXP
    | Index EXP EXP
    | Separacion EXP EXP
    deriving(Eq,Show)

data ID = ListIdent { getIdent :: [Ident] }
    deriving (Eq,Show)

data Ident = Ident_ String
    deriving (Eq,Show)

data Tipo = TipoChar
    | TipoInt
    | TipoBool
    | TipoMatrix EXP Tipo
    deriving (Eq,Show)
}