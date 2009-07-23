from pygments import lexers

lexers._mapping.LEXERS['TartLexer'] = (
    'tartlexer',
    'Tart',
    ('tart',),
    ('*.tart'),
    ('text/x-tart')
)

def setup(app):
  pass

