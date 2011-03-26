#/bin/python

# We need to be able to create a unique key for each reference of a symbol name,
# even if that name is incomplete.
#
# TODO: Type params in classes
# TODO: Type params in methods
# TODO: Type params in type expressions
# TODO: Template classes
# TODO: Template methods
# TODO: Variables
# TODO: Name index
# TODO: Base list
# TODO: Sub list
# TODO: Hierarchy
# TODO: Frames
# TODO: Deprecated
# TODO: Attributes (well, some) Unsafe for sure.
# TODO: Search
# TODO: Accordion Index
# TODO: InheritDoc
# TODO: link to a specific overload.

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
  "macro",
  "var",
  "let",
  "property",
  "indexer",
  "econst",
  "override",
  "undef"])

CALLABLES = set([
  "method",
  "macro",
  "override",
  "undef"])

class Filter(object):
  "Filter predicates for selecting members of a scope"
  @staticmethod
  def group_eq(group):
    "A filter that accepts definitions in a particular group, such as exceptions or attributes"
    def filter(decl):
      return decl.el.attrib.get('group') == group
    return filter
  
  @staticmethod
  def group_nil(el):
    "A filter that only accepts definitions with no group attribute"
    def filter(decl):
      return 'group' not in decl.el.attrib
    return filter

  @staticmethod
  def type_eq(type):
    "A filter that accepts elements with a given metatype, such as 'class' or 'enun'"
    def filter(decl):
      return decl.el.attrib.get('type') == type
    return filter
  
  @staticmethod
  def is_static():
    "A filter that accepts only static elements"
    def filter(decl):
      return decl.el.attrib.get('static') == "true"
    return filter
  
  @staticmethod
  def is_not_static():
    "A filter that accepts only non-static elements"
    def filter(decl):
      return decl.el.attrib.get('static') != "true"
    return filter
  
  @staticmethod
  def accept(el, filters):
    "Helper method that runs a list of filters against an object and returns True if all succeed"
    for filter in filters:
      if not filter(el): return False
    return True

#class Definition(object):
#  def compressed_text(self, el):
#    result = []
#    first = True
#    for child in el:
#      if first and child.tag == "p":
#        result.append(inner_html(child))
#        first = False
#      else:
#        result.append(outer_html(child))
#    return Markup('').join(result)
#  
#  def supertypes(self, type):
#    if 'bases' in self.el.attrib:
#      return []
#    return []
#    
#  def subtypes(self, type):
#    return []
    
class Declaration(object):
  "A wrapper around an XML element for a definition"
  def __init__(self, index, package, qualifiedName, name, el, uri):
    self.index = index
    self.package = package
    self.qualifiedName = qualifiedName
    self.name = name
    self.el = el
    self.tag = el.tag
    self.uri = uri
    self._members = []
    
  def __lt__(self, other):
    return self.qualifiedName.__lt__(other.qualifiedName)
  
  def __str__(self):
    return self.qualifiedName
    
  def __repr__(self):
    return self.tag + ':' + self.qualifiedName
    
  def addMember(self, m):
    "Add a child member to this member's scope"
    assert m
    self._members.append(m)

  def declarator(self):
    "Returns the declaring keyword of this definition"
    return self.el.tag

  def visibility(self):
    return self.el.attrib.get('visibility')
  
  def storage(self):
    "Returns the storage-class of this definition"
    return 'static' if self.el.attrib.get('static') == 'true' else ''
  
  def members(self, tag=None, *filters):
    result = []
    for member in self._members:
      if member.tag == tag \
        and Filter.accept(member, filters) \
        and member.el.attrib.get('visibility') != 'private':
        result.append(member)
    result.sort()
    return result

  def inherits(self):
    "Return the list of all base types"
    return []

  def implements(self):
    "Return the list of all implemented interfaces"
    return []

  def params(self):
    return self.el.findall("./doc/parameter")
    
  def returns(self):
    return self.el.find("./doc/returns")
    
  def throws(self):
    return self.el.findall("./doc/exception")

  def summary(self):
    summary = self.el.find("./doc/summary")
    if summary is not None:
      return inner_html(summary)

    first_para = self.el.find("./doc/description/p")
    if first_para is not None:
      return inner_html(first_para)
    
    return ""

  def hasDescription(self):
    return self.el.find("./doc/description")
    
  def description(self):
    desc = self.el.find("./doc/description")
    if desc is not None:
      return inner_html(desc)
    return ""
  
  def typeArgs(self):
    return self.el.findall("type-arg/*")

  def formatDeclarator(self):
    mods = []
    if self.visibility() != 'public':
      mods.append(self.visibility())
    if self.el.attrib.get('static') == 'true':
      mods.append('static')
    if self.el.attrib.get('abstract') == 'true':
      mods.append('abstract')
    if self.el.attrib.get('final') == 'true':
      mods.append('final')
    mods.append(self.declarator())
    mods.append(' ')
    return Markup(' ').join(mods)
  
  def formatName(self, makeLink):
    if self.name == "$call": return ''
    if makeLink:
      return Markup('').join([
          Markup('<a class="member-table-link symbol" href="%s">' % self.uri),
          self.name,
          Markup('</a>')])
    else:
      return Markup('').join([
          Markup('<span class="symbol">'),
          self.name,
          Markup('</span>')])
      
  def formatTypeSignature(self):
    return ''
    tag = self.el.tag
    if tag == 'let' or tag == 'var' or tag == 'property':
      return Markup('').join([':', self.index.formatType(self.el.find('type/*'))])
    return ''
  
  def formatDeclaration(self, makeLink):
    return Markup('').join([
        self.formatName(makeLink),
        self.index.formatTypeParamList(self.typeArgs()),
        self.formatTypeSignature()])
    
  def compressedText(self, el):
    result = []
    first = True
    for child in el:
      if first and child.tag == "p":
        result.append(inner_html(child))
        first = False
      else:
        result.append(outer_html(child))
    return Markup('').join(result)
  
class Module(Declaration):
  "Represents a Tart module."
  def __init__(self, index, package, qualifiedName, name, el, uri):
    super(Module, self).__init__(index, package, qualifiedName, name, el, uri)

class Typedef(Declaration):
  "Represents a Tart type definition."
  def __init__(self, index, package, qualifiedName, name, el, uri):
    super(Typedef, self).__init__(index, package, qualifiedName, name, el, uri)

    # Preprocess list of bases
    self.baseInherits = []
    self.baseImplements = []
    kind = el.attrib['type']
    for base in el.findall('./base-type/*'):
      baseKind = typeKind(base)
      if kind == baseKind:
        self.baseInherits.append(base)
      else:
        self.baseImplements.append(base)

  def declarator(self):
    return self.el.attrib['type']

  def inherits(self):
    return self.baseInherits

  def implements(self):
    return self.baseImplements
  
class Method(Declaration):
  "Represents a Tart method."
  def declarator(self):
    if self.el.tag == 'method': return "def"
    return self.el.tag

  def formatTypeSignature(self):
    params = []
    for param in self.el.findall('param'):
      psig = Markup('').join(
          [Markup('<span class="symbol">'), param.attrib['name'], Markup('</span>')])
      param_type = param.find('type/*')
      if param_type is not None:
        psig = Markup(':').join([psig, self.index.formatType(param_type)])
      params.append(psig)
    result = [
        "(",
        Markup(", ").join(params),
        Markup('<span style="white-space: nowrap">'),
        ")"]
    ret = self.el.find('return-type/*')
    if ret is not None:
      result.append(" -> ")
      result.append(self.index.formatType(ret))
    result.append(Markup('</span>'))
    return Markup('').join(result)

class Field(Declaration):
  "Represents a Tart variable or property."
  def formatTypeSignature(self):
    return Markup('').join([':', self.index.formatType(self.el.find('type/*'))])
  
class Package(object):
  "Represents a package."
  def __init__(self, index, name, url):
    self.index = index
    self.name = name
    self.url = url
    self._modules = []
    self._members = []
    
  def __lt__(self, other):
    return self.name.__lt__(other.name)
    
  def addModule(self, m):
    self._modules.append(m)

  def addMember(self, m):
    self._members.append(m)

  def modules(self, tag=None, *filters):
    "Return the members of this package which match the given query criteria"
    result = []
    for module in self._modules:
      result.extend(module.members(tag, *filters))
    result.sort()
    return result
  
  def members(self, tag=None, *filters):
    "Return the members of this package which match the given query criteria"
    result = []
    for member in self._members:
      if (tag == member.tag or (tag is None and member.tag in DEFN_TAGS)) \
        and Filter.accept(member, filters) \
        and member.el.attrib.get('visibility') != 'private':
        result.append(member)
    result.sort()
    return result
  
class SymbolIndex(object):
  "Index of all declared symbols"
  def __init__(self, document):
    self.nameIndex = defaultdict(list)
    self.packageIndex = {}
    self.buildModuleIndex(document.getroot())
    
  def makeUri(self, prefix, qualifiedName, relativeTo=None):
    # The 'outer' path is the path to the top-level symbol within a module.
    # The 'inner' path is the relative path from the top-level symbol to the declaration.
    outerPath = qualifiedName.split('.')
    innerPath = []
    while outerPath:
      testPath = tuple(outerPath[:-1])
      if testPath in self.packageIndex: break
      innerPath.insert(0, outerPath.pop())

    # If we never found a package, then restore the outer path.
    if not outerPath:
      outerPath, innerPath = innerPath, outerPath
      
    # TODO: What if we never find a matching package?
    result = "-".join([prefix] + outerPath)
    if result == relativeTo:
      result = ''
    result += ".html"
    if innerPath:
      result += '#' + ".".join(innerPath)
    return result
  
  def buildModuleIndex(self, rootEl):
    for el in rootEl:
      module = self.module(el)

  def module(self, el):
    path = tuple(el.attrib['name'].split('.'))
    qualifiedName = ".".join(path)
    packagePath = path[:-1]
    name = path[-1]
    package = self.getOrCreatePackage(packagePath)
    uri = self.makeUri("module", qualifiedName)
    module = Module(self, package, qualifiedName, name, el, uri)
    package.addModule(module)
    for childEl in el:
      if childEl.tag in DEFN_TAGS:
        decl = self.declaration(childEl, module, True, None)
        module.addMember(decl)
        if decl.name == module.name:
          package.addMember(decl)

  def declaration(self, el, parent, topLevel, uriBase):
    name = el.attrib['name']
    uriPrefix = el.attrib['type'] if el.tag == 'typedef' else el.tag
    if topLevel:
      qualifiedName = parent.package.name + '.' + name;
    else:
      qualifiedName = parent.qualifiedName + '.' + name;
    path = tuple(qualifiedName.split('.'))

    if uriBase:
      uri = uriBase
      if '#' in uri:
        uri += '-' + name
      else:
        uri += '#' + name
    else:
      uri = self.makeUri(uriPrefix, qualifiedName)

    if el.tag in CALLABLES:
      decl = Method(self, parent.package, qualifiedName, name, el, uri)
    elif el.tag == 'typedef':
      decl = Typedef(self, parent.package, qualifiedName, name, el, uri)
    elif el.tag == 'let' or el.tag == 'var' or el.tag == 'property':
      decl = Field(self, parent.package, qualifiedName, name, el, uri)
    else:
      decl = Declaration(self, parent.package, qualifiedName, name, el, uri)
    parent.addMember(decl)
    self.nameIndex[path].append(decl)
    for childEl in el:
      if childEl.tag in DEFN_TAGS:
        parent.addMember(self.declaration(childEl, decl, False, uri))
    return decl
    
  def getOrCreatePackage(self, path):
    if path not in self.packageIndex:
      name = ".".join(path)
      uri = 'package-' + "-".join(path) + ".html"
      self.packageIndex[path] = package = Package(self, name, uri)
      return package
    else:
      return self.packageIndex[path]
    
  def packages(self):
    return self.packageIndex.values()
  
  def formatTypeParamList(self, types, *options):
    "Format 'types' as a list of template arguments. Returns a Markup object"
    return TypeFormatter(self, options).typeParamList(types)

  def formatTypeList(self, types, *options):
    "Format 'types' as a comma-separated list. Returns a Markup object"
    return TypeFormatter(self, options).typeList(types)

  def formatType(self, ty, *options):
    "Format 'type' as text. Returns a Markup object"
    return TypeFormatter(self, options).type(ty)

class TypeFormatter(object):
  "Helper class that formats type expressions as markup objects"
  def __init__(self, index, options):
    self.index = index
    self.options = set(options)

  def typeParamList(self, types):
    if types:
      return Markup('').join(['[', self.typeList(types), ']'])
    return ''

  def typeList(self, types):
    return Markup(", ").join(self.type(ty) for ty in types)

  def typeContent(self, el):
    return self.type(list(el)[0])
    
  def type(self, el):
    "Format 'types' and transform into a template argument list"
    M = Markup
    if el.tag == 'typename':
      name = el.text
      if name.startswith("tart.core."): name = name[10:]
      if 'hlink' in self.options and el.attrib['type'] != 'primitive':
        uri = self.index.makeUri(el.attrib['type'], el.text)
        return self.concat(M('<a href="%s" class="type-name-link">' % uri), name, M('</a>'))
      else:
        return self.span(name, 'type-name')
    elif el.tag == 'type-variable':
      name = el.attrib['name']
      if 'tsig' in self.options: return self.concat('%', self.span(name, 'type-variable-name'))
      else: return self.span(name, 'type-name')
    elif el.tag == 'array':
      return self.concat(self.typeContent(el), "[]")
    elif el.tag == 'variadic':
      return self.concat(self.typeContent(el), "...")
    elif el.tag == 'address':
      return self.concat('Address[', self.typeContent(el), ']')
    elif el.tag == 'tuple':
      return self.concat('(', self.typeList(el.findall("*")), ')')
    elif el.tag == 'template-instance':
      return self.concat(
          self.typeContent(el),
          self.typeParamList(el.findall("template-arg/*")))
    else:
      return self.concat(el.tag, "??")

  def concat(self, *args):
    return Markup('').join(args)
  
  def span(self, content, cls):
    return self.concat(
        Markup('<span class="%s">' % cls), content, Markup('</span>'))

class DocGenerator(object):
  "The main driver class"
  def __init__(self, index, custom=[]):
    # Create the template loader
    self.loader = TemplateLoader(os.path.join(os.path.dirname(__file__), 'templates'))
    
    # Symbol index
    self.index = index

    # Custom templates    
    self.custom = custom
   
  def generate(self):
    self.writePackageIndex()
    self.writePackages()
    self.writeDefinitions()
    self.writeTypes()

  def resetcounter(self):
    self.counter = 0
    
  def evenodd(self):
    self.counter += 1
    return 'row-even' if (self.counter & 1) == 0 else ''
  
  def writeTemplate(self, template, outputfile, **kwargs):
    if not os.path.exists(options.outdir):
      os.makedirs(options.outdir)
    outfile = os.path.join(options.outdir, outputfile)
    # print "Generating:", outfile
    template = loader.load(template)
    stream = template.generate(gen=self, si=self.index, **kwargs)
    content = stream.render('html', doctype='html5', encoding="UTF-8", strip_whitespace=True)
    fh = open(outfile, "w")
    fh.write(content)
    fh.close()

  def writePackageIndex(self):
    self.writeTemplate("index.xml", "index.html", data=self.index.packages())
    
  def writePackages(self):
    for pkg in self.index.packages():
      self.writeTemplate("package.xml", pkg.url, pkg=pkg, filter=Filter)

  def writeDefinitions(self):
    for pkg in self.index.packages():
      for defn in pkg.members():
        self.writeTemplate("defn.xml", defn.uri, d=defn, filter=Filter)

  def writeTypes(self):
    types = []
    for package in self.index.packages():
      for defn in package.members("typedef"):
          types.append(defn)
          for inner in defn.members("typedef"):
            types.append(inner)
 
    types.sort()   
    self.writeTemplate("types.xml", "types.html", data=types)
  
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
  
def typeKind(el):
  if el.tag == 'typename':
    return el.attrib.get('type')
  elif el.tag == 'template-instance':
    return typeKind(el.find('typename'))
  else:
    return None
  
# Load the input XML file
doc = ElementTree.parse(options.infile)
strip_xml_namespace(doc.getroot())

# Generate the index of all symbols
si = SymbolIndex(doc)

# Generate all output files
DocGenerator(si, options.cust).generate()
