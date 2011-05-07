/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Defn/Template.h"
#include "tart/Defn/TemplateConditions.h"

#include "tart/Sema/TemplateParamAnalyzer.h"

namespace tart {

Type * TemplateParamAnalyzer::reduceTypeVariable(const ASTTypeVariable * ast) {
  DefnList defs;
  TypeVariable * tvar = NULL;
  Defn * def = tsig_->paramScope().lookupSingleMember(ast->name());
  if (def != NULL) {
    if (TypeDefn * tdef = dyn_cast<TypeDefn>(defs.front())) {
      tvar = dyn_cast<TypeVariable>(tdef->typeValue());
    }

    if (tvar == NULL) {
      diag.error(ast) << "Conflicting type declaration for type variable '" << ast->name() << "'";
      return NULL;
    }
  }

  if (tvar == NULL) {
    tvar = new TypeVariable(ast->location(), ast->name(), NULL);
    tvar->setIsVariadic(ast->isVariadic());
    TypeDefn * tdef = new TypeDefn(module_, ast->name(), tvar);
    tsig_->paramScope().addMember(tdef);
  }

  // See if the type variable has constraints
  if (ast->type() != NULL) {
    Type * type = typeFromAST(ast->type());
    if (type != NULL) {
      if (ast->constraint() == ASTTypeVariable::IS_SUBTYPE) {
        // Add a subclass test
        TemplateCondition * condition = new IsSubtypeCondition(tvar, type);
        tsig_->conditions().push_back(condition);
      } else if (ast->constraint() == ASTTypeVariable::IS_SUPERTYPE) {
        // Add a subclass test - reversed.
        TemplateCondition * condition = new IsSubtypeCondition(type, tvar);
        tsig_->conditions().push_back(condition);
      } else if (ast->constraint() == ASTTypeVariable::IS_INSTANCE) {
        if (tvar->valueType() == NULL) {
          tvar->setValueType(type);
        } else if (!tvar->valueType()->isEqual(type)) {
          diag.error(ast) << "Conflicting type declaration for pattern variable '" <<
              ast->name() << "'";
        }
      } else {
        DFAIL("Invalid constraint");
      }
    }
  }

  return tvar;
}

}
