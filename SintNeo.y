{
{-
	SintNeo
	Description: Parser generator file for language Neo
	
	Author: Br. Jean Paul Alexander Lacour 12-10848
			Br. Diego Daniel Pedroza Perez 12-11281
	
	Last modification date: 27-05-16
-}
module SintNeo where

import LexNeo
}

%name parse
%tokentype { Token }
%error { parseError }

%nonassoc '<' "<=" '>' ">=" '='
%left "++" "--"
%left "::"
%left "\\/"
%left "/\\"
%left '+' '-'
%left '$'
%left '*' '/' '%'
%left not
%left Neg
%left '#'
%left '?' '[' ']' 
%nonassoc ','

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
    "->"		{ TkHacer (AlexPn _ _ _) }
    "<-"		{ TkAsignacion (AlexPn _ _ _) }
    '+'                 { TkSuma (AlexPn _ _ _) }
    '-'                 { TkResta (AlexPn _ _ _) }
    '*'                 { TkMult (AlexPn _ _ _) }
    '/'			{ TkDiv (AlexPn _ _ _) }
    '%'			{ TkMod (AlexPn _ _ _) }
    "/\\"		{ TkConjuncion (AlexPn _ _ _) }
    "\\/"		{ TkDisyuncion (AlexPn _ _ _) }
    not			{ TkNegacion (AlexPn _ _ _) }
    '<'			{ TkMenor (AlexPn _ _ _) }
    "<="		{ TkMenorIgual (AlexPn _ _ _) }
    '>'			{ TkMayor (AlexPn _ _ _) }
    ">="		{ TkMayorIgual (AlexPn _ _ _) }
    '='			{ TkIgual (AlexPn _ _ _) }
    "++"		{ TkSiguienteCar (AlexPn _ _ _) }
    "--"		{ TkAnteriorCar (AlexPn _ _ _) }
    '#'			{ TkValorAscii (AlexPn _ _ _) }
    "::"		{ TkConcatenacion (AlexPn _ _ _) }
    '$'			{ TkRotacion (AlexPn _ _ _) }
    '?'			{ TkTrasposicion (AlexPn _ _ _) }

%%

S : with Declaracion begin Secuenciacion end { ProgAlc $2 $4 }
    | begin Secuenciacion end { Prog $2 }

Secuenciacion : Secuenciacion INSTR { Secuencia ($2 : (getSecuencia $1)) }
    | INSTR { Secuencia [$1] }

Declaracion : Declaracion Declr { Declara ($2 : (getDeclara $1)) }
    | Declr { Declara [$1] }

Declr : var ID ':' Tipo { Dec $2 $4 }
    
INSTR : with Declaracion begin Secuenciacion end { Alcance $2 $4 }
    | begin Secuenciacion end { AlcanceSD $2 }
    | ident "<-" EXP '.' { Asignacion $1 $3 }
    | if EXP "->" Secuenciacion end { Condicional $2 $4 }
    | if EXP "->" Secuenciacion otherwise "->" Secuenciacion end { CondicionalO $2 $4 $7 }
    | for ident from EXP to EXP "->" Secuenciacion end { RepeticionDet $2 $4 $6 $8 }
    | for ident from EXP to EXP step EXP "->" Secuenciacion end { RepeticionDetS $2 $4 $6 $8 $10 }
    | while EXP "->" Secuenciacion end { RepeticionInd $2 $4 }
    | read ident '.' { Leer $2 }
    | print EXP '.' { Imprimir $2 }

EXP : EXP '+' EXP { Suma $1 $3 }
    | EXP '-' EXP { Resta $1 $3 }
    | EXP '*' EXP { Mult $1 $3 }
    | EXP '/' EXP { Div $1 $3 }
    | EXP '%' EXP { Mod $1 $3 }
    | '-' EXP %prec Neg { Negativo $2 }
    | EXP "\\/" EXP { Disyuncion $1 $3 }
    | EXP "/\\" EXP { Conjuncion $1 $3 }
    | not EXP { Negacion $2 }
    | EXP "++" { CarSig $1 }
    | EXP "--" { CarAnt $1 }
    | '#' EXP { ValorAscii $2 }
    | EXP "::" EXP { Concatenacion $1 $3 }
    | '$' EXP { Rotacion $2 }
    | EXP '?' { Transposicion $1 }
    | EXP '<' EXP { Menor $1 $3 }
    | EXP "<=" EXP { MenorIgual $1 $3 }
    | EXP '>' EXP { Mayor $1 $3 }
    | EXP ">=" EXP { MayorIgual $1 $3 }
    | EXP '=' EXP { Igual $1 $3 }
    | EXP '/''=' EXP { Desigual $1 $4 }
    | '(' EXP ')' { Parent $2 }
    | ident { IDENT $1 }
    | car { CAR $1 }
    | num { NUM $1 }
    | false { Booleano False }
    | true { Booleano True }
    | '{' EXP1 '}' { Matriz $2 }
    | EXP '['EXP1']' { IndexConId $1 $3 }

DIM : '['EXP1']' { Dimen $2 }

EXP1 : EXP1 ',' EXP { Separacion $1 $3 }
    | EXP { EXP $1 }

ID : ID ',' ident { ListIdent ((Ident_ $3) : (getIdent $1)) }
    | ident { ListIdent [Ident_ $1] }

Tipo : CHAR { TipoChar }
    | INT { TipoInt }
    | BOOL { TipoBool }
    | matrix DIM of Tipo { TipoMatrix $2 $4 }
{
showS :: [INSTR] -> String
showS [] = ""
showS (x:xs) = show x ++ showS xs

parseError :: [Token] -> a
parseError (x:xs) = error ("\tParse error: " ++ show x )

data S = ProgAlc Declaracion Secuenciacion
    | Prog Secuenciacion
    deriving(Eq)

instance Show S where
    show (ProgAlc d s) = show s
    show (Prog s) = show s

data Secuenciacion = Secuencia { getSecuencia :: [INSTR] }
    deriving (Eq)

instance Show Secuenciacion where
    show (Secuencia sec) = if length sec > 1 then "\nSecuenciacion" ++ showS sec
                                            else "\n" ++ showS sec

data Declaracion = Declara { getDeclara :: [Declr] }
    deriving (Eq)
    
instance Show Declaracion where
    show (Declara dec) = ""

data Declr = Dec ID Tipo
    deriving (Eq)

instance Show Declr where
    show (Dec i t) = ""
    
data INSTR = Alcance Declaracion Secuenciacion
    | AlcanceSD Secuenciacion
    | Asignacion String EXP
    | Condicional EXP Secuenciacion
    | CondicionalO EXP Secuenciacion Secuenciacion
    | RepeticionDet String EXP EXP Secuenciacion
    | RepeticionDetS String EXP EXP EXP Secuenciacion
    | RepeticionInd EXP Secuenciacion
    | Leer String
    | Imprimir EXP
    deriving(Eq)

instance Show INSTR where
    show (Alcance dec s) = show s
    show (AlcanceSD s) = show s
    show (Asignacion s e) = "\nAsignacion\n- contenedor: Variable\n- identificador: " ++ s ++ "\n- expresion: " ++ show e
    show (Condicional e s ) = "\nCondicional" ++ show e ++ "\n- exito:"++ show s
    show (CondicionalO e s1 s2) = "\nCondicional" ++ show e ++ "\n- exito:" ++ show s1 ++ "\n- fallo:" ++ show s2
    show (RepeticionDet s e1 e2 sec) = "\nRepeticion Determinada" ++ show s ++ show e1 ++ show e2 ++ show sec
    show (RepeticionDetS s e1 e2 e3 sec) = "\nRepeticion Determinada" ++ show s ++ show e1 ++ show e2 ++ show e3 ++ show sec
    show (RepeticionInd e sec) = "\nRepeticion Indeterminada" ++ show e ++ show sec
    show (Leer s) = "\nLeer\n- identificador: " ++ show s
    show (Imprimir e) = "\nImprimir" ++ show e

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
    | Matriz EXP1
    | IndexConId EXP EXP1
    deriving (Eq)
    
instance Show EXP where
    show (Suma e1 e2) = "\n- operacion: 'Suma'\n" ++ "- operador izquierdo: " ++ show e1 ++ "\n- operador derecho: " ++ show e2 
    show (Resta e1 e2) = "\n- operacion: 'Resta'\n" ++ "- operador izquierdo: " ++ show e1 ++ "\n- operador derecho: " ++ show e2
    show (Mult e1 e2) = "\n- operacion: 'Multiplicacion'\n" ++"- operador izquierdo: " ++ show e1 ++ "\n- operador derecho: " ++ show e2
    show (Div e1 e2) = "\n- operacion: 'Division'\n" ++ "- operador izquierdo: " ++ show e1 ++ "\n- operador derecho: " ++ show e2
    show (Mod e1 e2) = "\n- operacion: 'Modulo'\n" ++ "- operador izquierdo: " ++ show e1 ++ "\n- operador derecho: " ++ show e2
    show (Negativo e) = "\n- operacion: 'Negativo'\n" ++ show e
    show (Disyuncion e1 e2) = "\n- operacion: 'Disyuncion'\n" ++ "- operador izquierdo: " ++ show e1 ++ "\n- operador derecho: " ++ show e2
    show (Conjuncion e1 e2) = "\n- operacion: 'Conjuncion'\n" ++ "- operador izquierdo: " ++ show e1 ++ "\n- operador derecho: " ++ show e2
    show (Negacion e) = "\n- operacion: 'Negacion'\n- operador: " ++ show e
    show (CarSig e) = "\n- operacion: 'Caracter Siguiente'\n- operador: " ++ show e
    show (CarAnt e) = "\n- operacion: 'Caracter Anterior'\n- operador: " ++ show e
    show (ValorAscii e) = "\n- operacion: 'Valor Ascii'\n- operador: " ++ show e
    show (Concatenacion e1 e2) = "\n- operacion: 'Concatenacion'\n" ++ "- operador izquierdo: " ++ show e1 ++ "\n- operador derecho: " ++ show e2
    show (Rotacion e) = "\n- operacion: 'Rotacion'\n- operador: " ++ show e
    show (Transposicion e) = "\n- operacion: 'Transposicion'\n- operador: " ++ show e
    show (Menor e1 e2) = "\n- operacion: 'Menor que'\n" ++ "- operador izquierdo: " ++ show e1 ++ "\n- operador derecho: " ++ show e2
    show (MenorIgual e1 e2) = "\n- operacion: 'Menor igual que'\n" ++ "- operador izquierdo: " ++ show e1 ++ "\n- operador derecho: " ++ show e2
    show (Mayor e1 e2) = "\n- operacion: 'Mayor que'\n" ++ "- operador izquierdo: " ++ show e1 ++ "\n- operador derecho: " ++ show e2
    show (MayorIgual e1 e2) = "\n- operacion: 'Mayor igual que'\n" ++ "- operador izquierdo: " ++ show e1 ++ "\n- operador derecho: " ++ show e2
    show (Igual e1 e2) = "\n- operacion: 'Igual'\n" ++ "- operador izquierdo: " ++ show e1 ++ "\n- operador derecho: " ++ show e2
    show (Desigual e1 e2) = "\n- operacion: 'Desigual'\n" ++ "- operador izquierdo: " ++ show e1 ++ "\n- operador derecho: " ++ show e2
    show (Parent e) = show e
    show (IDENT s) = "\n- identificador: " ++ s
    show (CAR s) = "Literal Caracter\n - valor: " ++ s
    show (NUM i) = "Literal Entero\n - valor: " ++ show i
    show (Booleano b) = "Literal Booleano\n - valor: " ++ show b
    show (Matriz exp) = "Literal Matriz\n - valor: " ++ show exp
    show (IndexConId e exp) = show e ++ show exp

data DIM = Dimen EXP1
    deriving (Eq)

instance Show DIM where
    show (Dimen exp) = ""
    
data EXP1 = Separacion EXP1 EXP
    | EXP EXP
    deriving (Eq)
    
instance Show EXP1 where
    show (Separacion exp e) = show exp ++ show e
    show (EXP e) = show e

data ID = ListIdent { getIdent :: [Ident] }
    deriving (Eq)
    
instance Show ID where
    show (ListIdent l) = ""

data Ident = Ident_ String
    deriving (Eq)

instance Show Ident where
        show (Ident_ s) = ""

data Tipo = TipoChar
    | TipoInt
    | TipoBool
    | TipoMatrix DIM Tipo
    deriving (Eq)

instance Show Tipo where
    show _ = ""
    show (TipoMatrix d t) = ""
}
