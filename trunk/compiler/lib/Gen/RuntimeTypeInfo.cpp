/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Gen/RuntimeTypeInfo.h"
#include "tart/Type/CompositeType.h"
#include "tart/Defn/Defn.h"
#include "tart/Defn/TypeDefn.h"

namespace tart {

using namespace llvm;

RuntimeTypeInfo::RuntimeTypeInfo(const CompositeType * ty, Module * m)
  : type(ty)
  , linkageType_(GlobalValue::ExternalLinkage)
  , typeInfoBlock_(NULL)
  , typeAllocator_(NULL)
{
  external_ = type->typeDefn()->module() != m;
  if (type->typeDefn()->isSynthetic()) {
    external_ = false;
    linkageType_ = GlobalValue::LinkOnceODRLinkage;
  }
}

} // namespace tart
