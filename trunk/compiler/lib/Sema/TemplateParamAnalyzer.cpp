/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Defn/Template.h"

#include "tart/Sema/TemplateParamAnalyzer.h"

#include "tart/Type/TypeRelation.h"

namespace tart {

Type * TemplateParamAnalyzer::reduceTypeVariable(const ASTTypeVariable * ast) {
  DefnList defs;
  TypeVariable * tvar = NULL;
  Defn * def = tsig_->paramScope().lookupSingleMember(ast->name());
  if (def != NULL) {
    if (TypeDefn * tdef = dyn_cast<TypeDefn>(defs.front())) {
      tvar = dyn_cast<TypeVariable>(tdef->mutableTypePtr());
    }

    if (tvar == NULL) {
      diag.error(ast) << "Conflicting type declaration for type variable '" << ast->name() << "'";
      return NULL;
    }
  }

  TypeVariable::Target target = TypeVariable::TYPE_EXPRESSION;
  if (ast->constraint() == ASTTypeVariable::IS_QUALIFIER) {
    target = TypeVariable::TYPE_QUALIFIER;
  } else if (ast->constraint() == ASTTypeVariable::IS_TYPE_CTOR) {
    target = TypeVariable::TYPE_CONSTRUCTOR;
  }

  if (tvar == NULL) {
    tvar = new TypeVariable(ast->location(), ast->name(), target);
    tvar->setIsVariadic(ast->isVariadic());
    TypeDefn * tdef = new TypeDefn(module_, ast->name(), tvar);
    tsig_->paramScope().addMember(tdef);
  }

  // See if the type variable has constraints
  if (ast->type() != NULL) {
    QualifiedType type = typeFromAST(ast->type());
    if (type) {
      if (ast->constraint() == ASTTypeVariable::IS_SUBTYPE) {
        // Add a subclass test
        tvar->upperBounds().push_back(type);
      } else if (ast->constraint() == ASTTypeVariable::IS_SUPERTYPE) {
        // Add a subclass test - reversed.
        tvar->lowerBounds().push_back(type);
      } else if (ast->constraint() == ASTTypeVariable::IS_INSTANCE) {
        if (!tvar->metaType()) {
          tvar->setMetaType(type);
        } else if (!TypeRelation::isEqual(tvar->metaType(), type)) {
          diag.error(ast) << "Conflicting type declaration for pattern variable '" <<
              ast->name() << "'";
        }
      } else if (ast->constraint() == ASTTypeVariable::IS_QUALIFIER) {
        if (tvar->target() != TypeVariable::TYPE_QUALIFIER) {
          diag.error() << "Conflicting usage of type variable " << tvar;
        }
      } else if (ast->constraint() == ASTTypeVariable::IS_TYPE_CTOR) {
        if (tvar->target() != TypeVariable::TYPE_CONSTRUCTOR) {
          diag.error() << "Conflicting usage of type variable " << tvar;
        }
      } else {
        DFAIL("Invalid constraint");
      }
    }
  }

  return tvar;
}

}
