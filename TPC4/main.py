import ply.lex as lex

# Lista de tokens
tokens = (
    'SELECT', 'WHERE', 'LIMIT',
    'IDENTIFIER',  # variaveis ?nome, ?desc, ?s, ?w
    'PREFIX_NAME',  # nomes prefixados foaf:name, dbo:MusicalArtist, a (rdf:type)
    'STRING',  # strings "Chuck Berry"
    'LANG_TAG',  # language tag  @en
    'NUMBER',  # numeros
    'LBRACE', 'RBRACE',  # { and }
    'DOT',  # .
    'COMMENT', 
)

t_SELECT = r'select'
t_WHERE = r'where'
t_LIMIT = r'LIMIT'
t_LBRACE = r'\{'
t_RBRACE = r'\}'
t_DOT = r'\.'


def t_COMMENT(t):
    r'\#.*'
    return t  

def t_PREFIX_NAME(t):
    r'[a-zA-Z_][a-zA-Z0-9_-]*:[a-zA-Z_][a-zA-Z0-9_-]*|a'
    return t

def t_IDENTIFIER(t):
    r'\?[a-zA-Z_][a-zA-Z0-9_]*'
    return t

def t_STRING(t):
    r'"[^"]*"'  
    return t

def t_LANG_TAG(t):
    r'@[a-zA-Z]+'
    return t

def t_NUMBER(t):
    r'\d+'
    t.value = int(t.value) 
    return t

t_ignore = ' \t\n'

def t_error(t):
    print(f"Illegal character '{t.value[0]}' at position {t.lexpos}")
    t.lexer.skip(1)


lexer = lex.lex()


query = '''
# DBPedia: obras de Chuck Berry
select ?nome ?desc where {
    ?s a dbo:MusicalArtist.
    ?s foaf:name "Chuck Berry"@en .
    ?w dbo:artist ?s.
    ?w foaf:name ?nome.
    ?w dbo:abstract ?desc
} LIMIT 1000
'''

lexer.input(query)
for tok in lexer:
    print(tok)
