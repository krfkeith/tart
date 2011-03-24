#!/usr/bin/python3

# TODO: Type params in classes
# TODO: Type params in methods
# TODO: Type params in type expressions
# TODO: Template classes
# TODO: Template methods
# TODO: Variables
# TODO: Enums
# TODO: Name index
# TODO: Base list
# TODO: Sub list
# TODO: Hierarchy
# TODO: Frames
# TODO: Publish
# TODO: Deprecated
# TODO: Attributes (well, some)

import sys, os
from optparse import OptionParser
from xml.etree import ElementTree
from collections import defaultdict

# Options
parser = OptionParser()
parser.add_option("-i", "--input", dest="infile",
                  help="input XML documentation file", metavar="FILE")
parser.add_option("-o", "--output-dir", dest="outdir",
                  help="output directory", metavar="DIR")
parser.add_option("-t", "--template-dir", dest="template",
                  help="directory for source templates", metavar="DIR")
parser.add_option("-p", "--path", dest="path", action="append",
                  help="additional path directories", metavar="DIR")
parser.add_option("-c", "--custom", dest="cust", action="append",
                  help="load additional templates for customization", metavar="FILE")

# Parse options
(options, args) = parser.parse_args()

# Add the path argument to sys.path
sys.path.extend(options.path)
from genshi.template import TemplateLoader
from genshi.core import Markup

# Create the template loader
loader = TemplateLoader(os.path.join(os.path.dirname(__file__), 'templates'))

# The set of tag names representing definitions
DEFN_TAGS = set([
  "namespace",
  "typedef",
  "method",
  "var",
  "let",
  "property",
  "indexer"])

class Filter(object):
  "Filter predicates for selecting members of a scope"
  @staticmethod
  def group_eq(group):
    "A filter that accepts definitions in a particular group, such as exceptions or attributes"
    def filter(el):
      return el.attrib.get('group') == group
    return filter
  
  @staticmethod
  def group_nil(el):
    "A filter that only accepts definitions with no group attribute"
    def filter(el):
      return 'group' not in el.attrib
    return filter

  @staticmethod
  def type_eq(type):
    "A filter that accepts elements with a given metatype, such as 'class' or 'enun'"
    def filter(el):
      return el.attrib.get('type') == type
    return filter
  
  @staticmethod
  def is_static():
    "A filter that accepts only static elements"
    def filter(el):
      return el.attrib.get('static') == "true"
    return filter
  
  @staticmethod
  def is_not_static():
    "A filter that accepts only non-static elements"
    def filter(el):
      return el.attrib.get('static') != "true"
    return filter
  
  @staticmethod
  def accept(el, filters):
    "Helper method that runs a list of filters against an object and returns True if all succeed"
    for filter in filters:
      if not filter(el): return False
    return True

class Definition(object):
  "A wrapper around an XML element for a definition"
  def __init__(self, helper, el):
    assert el.attrib['name']
    assert el.attrib['qualified-name']
    assert el.attrib['package']
    self.el = el
    self.helper = helper
    
  def name(self):
    return self.el.attrib['name']
  
  def qualified_name(self):
    return self.el.attrib['qualified-name']
  
  def package(self):
    return self.el.attrib['package']
  
  def url(self):
    return self.helper.url(self.el)
    
  def summary(self):
    summary = self.el.find(".//summary")
    if summary is not None:
      return inner_html(summary)

    first_para = self.el.find(".//description/p")
    if first_para is not None:
      return inner_html(first_para)
    
    return ""

  def has_description(self):
    return self.el.find(".//description")
    
  def description(self):
    desc = self.el.find(".//description")
    if desc is not None:
      return inner_html(desc)
    
    return ""
  
  def params(self):
    return self.el.findall(".//parameter")
    
  def returns(self):
    return self.el.find(".//returns")
    
  def throws(self):
    return self.el.findall(".//exception")

  def compressed_text(self, el):
    result = []
    first = True
    for child in el:
      if first and child.tag == "p":
        result.append(inner_html(child))
        first = False
      else:
        result.append(outer_html(child))
    return Markup('').join(result)
  
  def typename(self):
    if self.el.tag == "typedef":
      return self.el.attrib['type']
    else:
      return self.el.tag
    
  def visibility(self):
    return self.el.attrib.get('visibility')
  
  def storage(self):
    "Returns the storage-class of this definition"
    return 'static' if self.el.attrib.get('static') == 'true' else ''
  
  def declarator(self):
    "Returns the declaring keyword of this definition"
    if self.el.tag == "method": return "def"
    return self.typename()
  
  def sigpart1(self, link_name):
    tag = self.el.tag
    result = []
    result.append(self.visibility() + ' ')
    if self.el.attrib.get('static') == 'true':
      result.append('static ')
    result.append(self.declarator() + ' ')
    name = self.name()
    if name != "$call":
      if link_name:
        result.append(Markup('<a class="member-table-link symbol" href="#%s">' % name))
      else:
        result.append(Markup('<span class="symbol">'))
      result.append(name)
      if link_name:
        result.append(Markup('</a>'))
      else:
        result.append(Markup('</span>'))
    if tag == 'method':
      # TODO: Type params
      result.append("(")
    elif tag == 'typedef':
      # TODO: Type params
      pass
    elif tag == 'namespace':
      pass # return 'namespace ' + self.name()
    elif tag == 'let' or tag == 'var' or tag == 'property':
      result.append(":")
      # TODO: Type
    
    return Markup('').join(result)

  def sigpart2(self):
    result = []
    tag = self.el.tag
    if tag == 'method' or tag == 'typedef':
      if self.el.find("type-arg/*") is not None:
        result.append(self.format_type_args())
    if tag == 'method':
      params = []
      for param in self.el.findall('param'):
        psig = Markup('').join(
            [Markup('<span class="symbol">'), param.attrib['name'], Markup('</span>')])
        param_type = param.find('type/*')
        if param_type is not None:
          psig = Markup(':').join([psig, self.helper.format_type(param_type)])
        params.append(psig)
      result.append(Markup(", ").join(params))
      result.append(Markup('<span style="white-space: nowrap">'))
      result.append(")")
      ret = self.el.find('return-type/*')
      if ret is not None:
        result.append(" -> ")
        result.append(self.helper.format_type(ret))
      result.append(Markup('</span>'))
    elif tag == 'let' or tag == 'var' or tag == 'property':
      ty = self.el.find('type/*')
      result.append(self.helper.format_type(ty))
    
    return Markup('').join(result)
  
  def format_type_args(self, *options):
    options = set(options)
    args = self.el.findall("type-arg/*")
    if args:
      type_args = [self.helper.format_type(arg_type, options) for arg_type in args]
      return Markup('').join(['[', Markup(", ").join(type_args), ']'])
    else:
      return ''
    
  def supertypes(self, type):
    if 'bases' in self.el.attrib:
      return []
    return []
    
  def subtypes(self, type):
    return []
    
  def members(self, tag=None, *filters):
    result = []
    for member in self.el.findall(tag):
      if member.tag in DEFN_TAGS \
        and Filter.accept(member, filters) \
        and member.attrib['visibility'] != 'private':
        result.append(Definition(self.helper, member))
    result.sort(key=lambda x: x.el.attrib['name'])
    return result
  
class Package(object):
  "An object representing a package."
  def __init__(self, helper, name, modules):
    self.helper = helper
    self.name = name
    self.modules = modules

  def members(self, tag=None, *filters):
    "Return the members of this package which match the given query criteria"
    result = []
    for module in self.modules:
      for defn in module.findall(tag):
        if defn.tag in DEFN_TAGS and Filter.accept(defn, filters):
          result.append(Definition(self.helper, defn))
    return result

# Helper class called from the template
class TemplateHelper(object):
  def __init__(self, doc):
    # Strip off XML namespaces
    strip_xml_namespace(doc.getroot())

    # Divide into package
    self.packages = defaultdict(list)
    for mod in doc.getroot():
      qname = mod.attrib['name']
      pkname, name = qname.rsplit('.', 1)
      mod.attrib['package'] = pkname
      mod.attrib['name'] = name
      mod.attrib['qualified-name'] = qname
      self.packages[pkname].append(mod)
        
      for defn in mod:
        self.process_defn(defn, pkname, pkname)
    
    # Sort the list of modules    
    for pkname, modules in self.packages.items():
      modules.sort(key=lambda x: x.attrib['name'])

    # Custom templates    
    self.custom_templates = options.cust
    
  def process_defn(self, defn, parent_qname, pkname):
    qname = parent_qname + '.' + defn.attrib['name']
    defn.attrib['package'] = pkname
    defn.attrib['qualified-name'] = qname
    for child in defn:
      if child.tag in DEFN_TAGS:
        self.process_defn(child, parent_qname, pkname)
    
  def url(self, el):
    # Check to see if package is in packages.
    assert el.attrib['package']
    if el.attrib['package'] not in self.packages:
      return None

    if el.tag == "typedef": kind = el.attrib['type']
    else: kind = el.tag
    return self.make_url(kind, el.attrib['qualified-name'])

  def package_url(self, qname):
    return self.make_url('package', qname)

  def make_url(self, kind, qname):
    parts = [kind] + qname.split('.')
    return "-".join(parts) + ".html"
  
  def format_type(self, el, options=set()):
    result = []
    self.format_type_impl(el, result, options)
    return Markup('').join(result)
  
  def format_type_impl(self, el, result, options):
    if el.tag == 'typename':
      result.append(Markup('<span class="type-name">'))
      name = el.text
      if name.startswith("tart.core."):
        name = name[10:]
      result.append(name)
      result.append(Markup('</span>'))
    elif el.tag == 'type-variable':
      if 'tsig' in options:
        result.append('%')
        result.append(Markup('<span class="type-variable-name">'))
      else:
        result.append(Markup('<span class="type-name">'))
      result.append(el.attrib['name'])
      result.append(Markup('</span>'))
    elif el.tag == 'array':
      self.format_type_impl(list(el)[0], result, options)
      result.append("[]")
    elif el.tag == 'variadic':
      self.format_type_impl(list(el)[0], result, options)
      result.append("...")
    elif el.tag == 'address':
      result.append('Address[')
      if el:
        self.format_type_impl(list(el)[0], result, options)
      result.append("]")
    elif el.tag == 'tuple':
      result.append('(')
      types = []
      for child in el.findall("*"):
        types.append(self.format_type(child, options))
      result.append(Markup(', ').join(types))
      result.append(')')
    else:
      result.append(el.tag)
      result.append("??")
  
  def resetcounter(self):
    self.counter = 0
    
  def evenodd(self):
    self.counter += 1
    return 'row-even' if (self.counter & 1) == 0 else ''
  
  def write_index(self):
    self.write_template("index.xml", "index.html", data=self.packages)
    
  def write_packages(self):
    for pkname, modules in self.packages.items():
      url = self.package_url(pkname)
      self.write_template("package.xml", url, pkg=Package(self, pkname, modules), filter=Filter)

  def write_definitions(self):
    for pkname, modules in self.packages.items():
      package = Package(self, pkname, modules)
      for defn in package.members("*"):
        url = defn.url()
        self.write_template("defn.xml", url, d=defn, filter=Filter)

  def write_types(self):
    types = []
    for pkname, modules in self.packages.items():
      package = Package(self, pkname, modules)
      for defn in package.members("typedef"):
          types.append(defn)
          for inner in defn.members("typedef"):
            types.append(inner)
 
    types.sort(key = lambda x: x.qualified_name())   
    self.write_template("types.xml", "types.html", data=types)

  def write_template(self, template, outputfile, **kwargs):
    if not os.path.exists(options.outdir):
      os.makedirs(options.outdir)
    outfile = os.path.join(options.outdir, outputfile)
    #print "Generating:", outfile
    template = loader.load(template)
    stream = template.generate(th=self, **kwargs)
    content = stream.render('html', doctype='html5', encoding="UTF-8", strip_whitespace=True)
    fh = open(outfile, "w")
    fh.write(content)
    fh.close()
    
# Strip the namespace off of all XML elements. This makes them much easier to work with.
def strip_xml_namespace(el):
  _, el.tag = el.tag.split('}')
  for child in el:
    strip_xml_namespace(child)

# Return the 'outerHTML' of an element, with appropriate escaping.  
def outer_html(el):
  result = []
  flatten(el, result)
  return Markup('').join(result)

# Return the 'innerHTML' of an element, with appropriate escaping.  
def inner_html(el):
  result = []
  flatten_children(el, result)
  return Markup('').join(result)

def flatten_children(el, result):
  if el.text: result.append(el.text)
  for child in el:
    flatten(child, result)
    if child.tail: result.append(child.tail)
  
def flatten(el, result):
  attrs = ''
  for key in el.attrib:
    attrs += ' %s="%s"' % (key, el.attrib[key])
  result.append(Markup("<%s%s>" % (el.tag, attrs)))
  flatten_children(el, result)
  result.append(Markup("</%s>" % el.tag))
  
# Load the input XML file
th=TemplateHelper(ElementTree.parse(options.infile))
th.write_index()
th.write_packages()
th.write_definitions()
th.write_types()
