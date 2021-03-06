# ------
# Neo.py
# @author:	Jean Alexander
# @author:	Diego Pedroza
# -----

from ply import lex


# Lista de nombre de Tokens
tokens = (
	"TkWith",
	"TkBegin",
	"TkEnd",
	"TkIf",
	"TkVar",
	"TkInt",
	"TkBool",
	"TkChar",
	"TkMatrix",
	"TkOf",
	"TkFor",
	"TkFrom",
	"TkTo",
	"TkPrint",
	"TkOtherwise",
	"TkStep",
	"TkWhile",
	"TkRead",
	"TkFalse",
	"TkTrue",
	"TkComa",
	"TkPunto",
	"TkDosPuntos",
	"TkParAbre",
	"TkParCierra",
	"TkCorcheteAbre",
	"TkCorcheteCierra",
	"TkLlaveAbre",
	"TkLlaveCierra",
	"TkHacer",
	"TkAsignacion",
	"TkSuma",
	"TkResta",
	"TkMult",
	"TkDiv",
	"TkMod",
	"TkConjuncion",
	"TkDisyuncion",
	"TkNegacion",
	"TkMenor",
	"TkMenorIgual",
	"TkMayor",
	"TkMayorIgual",
	"TkIgual",
	"TkSiguienteCar",
	"TkAnteriorCar",
	"TkValorAscii",
	"TkConcatenacion",
	"TkRotacion",
	"TkTrasposicion",
	"TkCaracter",
	"TkNum",
	"TkId"
)

# Expresiones Regulares
t_TkWith = r'with'
t_TkBegin =r'begin'
t_TkEnd  = r'end'
t_TkIf = r'if'
t_TkVar = r'var'
t_TkInt = r'int'
t_TkBool = r'bool'
t_TkChar = r'char'
t_TkMatrix = r'matrix'
t_TkOf = r'of'
t_TkFor = r'for'
t_TkFrom = r'from'
t_TkTo = r'to'
t_TkPrint = r'print'
t_TkOtherwise = r'otherwise'
t_TkStep = r'step'
t_TkWhile = r'while'
t_TkRead = r'read'
t_TkFalse = r'[fF]alse'
t_TkTrue = r'[Tt]rue'
t_TkComa = r'\,'
t_TkPunto = r'\.'
t_TkDosPuntos = r'\:'
t_TkParAbre = r'\)'
t_TkParCierra = r'\('
t_TkCorcheteAbre = r'\]'
t_TkCorcheteCierra = r'\['
t_TkLlaveAbre = r'\}'
t_TkLlaveCierra = r'\{'
t_TkHacer = r'\->'
t_TkAsignacion = r'\<-'
t_TkSuma = r'\+'
t_TkResta = r'\-'
t_TkMult = r'\*'
t_TkDiv = r'\/'
t_TkMod = r'\%'
t_TkConjuncion = r'/\\'
t_TkDisyuncion = r'\\/'
t_TkNegacion = r'not'
t_TkMenor = r'\<'
t_TkMenorIgual = r'<='
t_TkMayor = r'\>'
t_TkMayorIgual = r'\>='
t_TkIgual = r'\='
t_TkSiguienteCar = r'\+\+'
t_TkAnteriorCar = r'--'
t_TkValorAscii = r'\#'
t_TkConcatenacion = r'::'
t_TkRotacion = r'\$'
t_TkTrasposicion= r'\?'
t_TkCaracter = r'\w'
t_TkNum = r'\d+'
t_TkId = r'([a-zA-z]*([a-zA-z][0-9]*))'

# Define a rule so we can track line numbers
def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

# A string containing ignored characters (spaces and tabs)
t_ignore  = ' \t'

# Error handling rule
def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)

# Build the lexer
lexer = lex.lex()

# Test it out
data = '''
!int 3 + 4 * 10
  + -20 *2
'''

# Give the lexer some input
lexer.input(data)

# Tokenize
while True:
    tok = lexer.token()
    if not tok: 
        break      # No more input
    print(tok)