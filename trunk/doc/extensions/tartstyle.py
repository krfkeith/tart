from pygments.style import Style
from pygments.token import Keyword, Name, Comment, String, Error, \
     Number, Operator, Generic

class TartStyle(Style):
  default_style = ""
  styles = {
    Comment:                'italic #360',
    Keyword:                'bold #059',
    Keyword.Type:           'bold #575',
    Name:                   '#000',
    Name.Function:          'bold #770',
    Name.Class:             'bold #074',
    Name.Variable:          '#000',
    Name.Builtin.Pseudo:    'bold #80c',
    String:                 '#551',
    Generic.Error:          'bold #c00',
  }

#Operator
#Operator.Word
#Number.Float
#Number.Hex
#Number.Integer.Long
#Number.Integer
