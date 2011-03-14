/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Type/Attribute.h"
#include "tart/Defn/TypeDefn.h"
#include "tart/Defn/FunctionDefn.h"

namespace tart {

/// -------------------------------------------------------------------
/// AttributeInfo

bool AttributeInfo::canAttachTo(const Defn * de) const {
  switch (de->defnType()) {
    case Defn::Typedef: {
      const TypeDefn * type = static_cast<const TypeDefn *>(de);
      switch (type->typeValue()->typeClass()) {
        case Type::Class:
          return (target_ & CLASS) != 0;

        case Type::Struct:
          return (target_ & STRUCT) != 0;

        case Type::Interface:
          return (target_ & INTERFACE) != 0;

        case Type::Enum:
          return (target_ & ENUM) != 0;

        default:
          return false;
      }
    }

    case Defn::Namespace:
      return (target_ & NAMESPACE) != 0;

    case Defn::Var:
    case Defn::Let:
      return (target_ & VARIABLE) != 0;

    case Defn::Property:
    case Defn::Indexer:
      return (target_ & PROPERTY) != 0;

    case Defn::Function:
      if (static_cast<const FunctionDefn *>(de)->isCtor()) {
        return (target_ & (CONSTRUCTOR|FUNCTION)) != 0;
      } else {
        return (target_ & FUNCTION) != 0;
      }

    case Defn::Macro:
      return (target_ & MACRO) != 0;

    case Defn::Parameter:
      return (target_ & PARAMETER) != 0;

    case Defn::Mod:
    case Defn::ExplicitImport:
    case Defn::MacroArg:
      return false;

    case Defn::DefnTypeCount:
      break;
  }

  return false;
}

} // namespace tart
