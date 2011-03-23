/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_DOCEXPORTER_H
#define TART_DOCEXPORTER_H

#ifndef TART_XMLWRITER_H
#include "XmlWriter.h"
#endif

#ifndef TART_COMMON_ABSTRACTCOMPILER_H
#include "tart/Common/AbstractCompiler.h"
#endif

namespace tart {

class Module;
class Defn;
class NamespaceDefn;
class CompositeType;
class EnumType;
class FunctionDefn;
class VariableDefn;
class PropertyDefn;
class IndexerDefn;
class IterableScope;
class Type;

namespace Doc {
  class Node;
}

/// -------------------------------------------------------------------
/// Writes out the contents of a module as XML.
class DocExporter : public AbstractCompiler {
public:
  DocExporter(llvm::raw_ostream & out) : xml_(out, true) {}

  void begin();
  void end();

  void exportModule(const Module * mod);
  void exportNamespace(const NamespaceDefn * ns);
  void exportCompositeType(const CompositeType * ctype);
  void exportEnumType(const EnumType * ctype);
  void exportMethod(const FunctionDefn * method);
  void exportVariable(const VariableDefn * var);
  void exportProperty(const PropertyDefn * prop);
  void exportIndexer(const IndexerDefn * idx);
  void writeAttributes(const Defn * de);

protected:
  void generate(Module * mod);

private:
  void writeModifiers(const Defn * de);
  void writeMembers(const IterableScope * scope);
  void writeTypeExpression(llvm::StringRef tagName, const Type * ty);
  void writeTypeRef(const Type * ty);
  void writeDocComment(const Defn * de);
  void writeDocCommentNode(const Doc::Node * node);
  void writeDocCommentNodeList(const Doc::Node * node);
  void writeElement(llvm::StringRef elName, llvm::StringRef content);

  XmlWriter xml_;
};

} // namespace tart

#endif // DOCEXPORTER
