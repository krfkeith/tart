/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_META_MDWRITER_H
#define TART_META_MDWRITER_H

#ifndef TART_META_TAGS_H
#include "tart/Meta/Tags.h"
#endif

#ifndef LLVM_SUPPORT_IRBUILDER_H
#include "llvm/Support/IRBuilder.h"
#endif

namespace llvm {
class NamedMDNode;
class MDNode;
class MDString;
}

namespace tart {

class Module;
class Scope;
class Defn;
class ExplicitImportDefn;
class TypeDefn;
class VariableDefn;
class FunctionDefn;
class ParameterDefn;
class PropertyDefn;
class IndexerDefn;
class Type;
class IterableScope;
class CodeGenerator;
class ASTWriter;
class MDNodeBuilder;

/// -------------------------------------------------------------------
/// Metadata writer.

class MDWriter {
public:
  MDWriter(CodeGenerator & cg);

  bool needImports() const { return needImports_; }

  llvm::MDNode * scopeMembers(const IterableScope * scope);

  /** Return a node containing all the explicit imports for this module. */
  llvm::MDNode * moduleImports();

  /** The LLVM IR builder. */
  llvm::IRBuilder<true> & builder() { return builder_; }

  /** The LLVM context. */
  llvm::LLVMContext & context() const { return context_; }

private:
  llvm::MDNode * typeDefn(const TypeDefn * td);
  llvm::MDNode * namespaceDefn(const NamespaceDefn * td);
  llvm::MDNode * variableDefn(const VariableDefn * td);
  llvm::MDNode * propertyDefn(const PropertyDefn * td);
  llvm::MDNode * indexerDefn(const IndexerDefn * idx);
  llvm::MDNode * functionDefn(const FunctionDefn * td);
  llvm::MDNode * parameterList(const ParameterList & params);
  llvm::MDNode * parameterDefn(const ParameterDefn * param);

  llvm::Value * templateSignature(const Defn * de);

  llvm::MDNode * location(const Defn * de);
  llvm::MDNode * attributeList(const Defn * de);

  llvm::Value * modifiers(const Defn * de);
  llvm::Value * expression(const Expr * e);
  llvm::Value * serializeAst(const ASTNode * ast);
  llvm::Value * serializeType(const Type * ty);
  llvm::Value * serializeType(QualifiedType ty);

  llvm::MDNode * imports(const ASTNodeList & imports);

  void putDefnHeader(MDNodeBuilder & builder, const Defn * de, meta::Defn::Tag tag);
  bool attrNeedsSerialization(const CompositeType * attrClass);

  llvm::ConstantInt * tagValue(unsigned tag);
  llvm::Value * nullValue();

  CodeGenerator & cg_;
  llvm::IRBuilder<true> & builder_;
  llvm::LLVMContext & context_;
  bool needImports_;
};

} // namespace tart

#endif
