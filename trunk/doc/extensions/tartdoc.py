from docutils import nodes
from sphinx.util.compat import Directive

class TartDocDirective(Directive):

    def run(self):
        return [todolist('')]

def setup(app):
    #app.add_config_value('tartdoc_include_todos', False, False)

    app.add_directive('tartdoc', TartDocDirective)
