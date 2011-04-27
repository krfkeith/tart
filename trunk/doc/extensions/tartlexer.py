from pygments.lexer import RegexLexer, bygroups, include
from pygments.token import *

class TartLexer(RegexLexer):
    name = 'Tart'
    aliases = ['tart']
    filenames = ['*.tart']
    
    re_ident = '[_a-zA-Z][a-zA-Z0-9_]*'

    tokens = {
        'root': [
            (r'.*// ERROR.*\n', Generic.Error),  # For error examples.
            (r'/\*', Comment, 'comment'),
            (r'//.*?$', Comment),
            (r'/', Operator),
            (r'\n', Text),
            (r'\s+', Text),
            (r'[\{\}]', Keyword),
            include('operators'),
            (r'[\[\]:(),;]', Punctuation),
            ('@', Name.Attribute, 'attribute'),
            (r'(abstract|as|base|break|case|catch|continue|default|'
             r'do|else|explicit|finally|for|if|in|'
             r'internal|is|match|out|override|private|protected|public|'
             r'repeat|require|return|static|switch|throw|try|typeof|typealias|'
             r'virtual|void|with|while|get|set)\b', Keyword),
            (r'(false|null|self|true)\b', Name.Builtin.Pseudo),
            (r'(bool|byte|char|double|float|int|long|object|'
             r'short|String|ubyte|uint|ulong|ushort)\b', Keyword.Type),
            (r'(import)(\s+)(?:(namespace)(\s+))?\b',
                bygroups(Keyword, Text, Keyword, Text), 'import'),
            (r'(class|struct|interface|enum|friend|protocol|extend)\b(\s+)',
                bygroups(Keyword, Text), 'class'),
            (r'(namespace)\b(\s+)',
                bygroups(Keyword, Text), 'namespace'),
            (r'(let|var)\b(\s*)', bygroups(Keyword, Text), 'var'),
            (r'(def|macro|fn)\b(\s*)', bygroups(Keyword, Text), 'def'),
            (r'!\[', Punctuation, 'typeparams'),
            ('"', String, 'dqs'),
            ("'", String, 'sqs'),
            (re_ident, Name.Variable),
            include('numbers'),
            (r'[^/]+', Generic.Error),
        ],
        'operators': [
            ("(->|>=|<=|!=|==|=|<:|\*|\+|\?|-|\.|<|>|\||\&|\$)", Operator),
            ("(and|or|not)\b", Operator.Word),
        ],
        'numbers': [
            (r'(\d+\.?\d*|\d*\.\d+)([eE][+-]?[0-9]+)?', Number.Float),
            (r'0[xX][a-fA-F0-9]+', Number.Hex),
            (r'\d+L', Number.Integer.Long),
            (r'\d+u?', Number.Integer)
        ],
        'comment': [
            (r'[^*/]', Comment),
            (r'/\*', Comment, '#push'),
            (r'\*/', Comment, '#pop'),
            (r'[*/]', Comment),
        ],
        'class': [
            (re_ident, Name.Class),
            (r'\[', Punctuation, 'typeparams'),
            (r'\s+', Text),
            (r'\:', Punctuation, 'baselist'),
            (r'', Text, '#pop'),
        ],
        'namespace': [
            (re_ident, Name.Class),
            (r'', Text, '#pop'),
        ],
        'import': [
            (re_ident, Name.Class),
            (r'as\b', Keyword),
            (r'\s+', Text),
            (r'\.', Punctuation),
            (r';', Punctuation, '#pop'),
            (r'', Text, '#pop'),
        ],
        'var': [
            (re_ident, Name.Variable),
            (r'\s+', Text),
            (r':', Punctuation, 'type'),
            (r',', Punctuation),
            (r'', Text, '#pop'),
        ],
        'attribute': [
            (re_ident, Name.Attribute),
            (r'', Text, '#pop'),
        ],
        'def': [
            (re_ident, Name.Function),
            (r'\[', Punctuation, 'typeparams'),
            (r'\(', Punctuation, 'arglist'),
            (r'\s+', Text),
            (r'', Text, '#pop'),
        ],
        'baselist': [
            (r'\s+', Text),
            (re_ident, Name.Class),
            (r'\!\[', Punctuation, 'typeparams'),
            (r',', Punctuation),
            (r'', Text, '#pop'),
        ],
        'arglist': [
            (r'\)', Punctuation, '#pop'),
            (r'\s+', Text),
            (re_ident, Name.Variable),
            (r':', Punctuation, 'type'),
            (r',', Punctuation),
            (r';', Punctuation),
            (r'=[\s\d\w]+', Punctuation),
            (r'[^);]+', Generic.Error),
        ],
        'typeparams': [
            (r']', Punctuation, '#pop'),
            (re_ident, Name.Class),
            (r'\s+', Text),
            (r'%', Name.Class, 'patternvar'),
            (r':', Punctuation, 'type'),
            (r'(require)\b', Keyword),
            include('operators'),
            include('numbers'),
            (r',', Punctuation),
            (r'!\[', Punctuation, 'typeparams'),
            (r'[^>]+', Generic.Error),
        ],
        'type': [
            (r'\s+', Text),
            (r'(fn)(\s*)(\()', bygroups(Keyword, Text, Punctuation), 'arglist'),
            (r'(bool|byte|char|double|float|int|long|object|'
             r'short|String|ubyte|uint|ulong|ushort)\b', Keyword.Type),
            (re_ident, Name.Class),
            (r'\[\]', Punctuation),
            (r'', Text, '#pop'),
        ],
        'patternvar': [
            (re_ident, Name.Class, '#pop'),
            (r'', Text, '#pop'),
        ],
        'strings': [
            (r'[^\\\'"%\n]+', String),
            # quotes, percents and backslashes must be parsed one at a time
            (r'[\'"\\]', String),
            # newlines are an error (use "nl" state)
        ],
        'dqs': [
            (r'"', String, '#pop'),
            include('strings')
        ],
        'sqs': [
            (r"'", String, '#pop'),
            include('strings')
        ],
    }

__all__ = ['TartLexer']
