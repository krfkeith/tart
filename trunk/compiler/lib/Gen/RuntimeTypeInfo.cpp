/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */
 
#include "tart/Gen/RuntimeTypeInfo.h"
#include "tart/CFG/CompositeType.h"
#include "tart/CFG/Defn.h"
#include "tart/CFG/TypeDefn.h"

namespace tart {
  
using namespace llvm;

RuntimeTypeInfo::RuntimeTypeInfo(const CompositeType * ty, Module * m)
  : type(ty)
  , linkageType(GlobalValue::ExternalLinkage)
  , typeObjectPtr(NULL)
  , typeInfoBlock(NULL)
  , typeInfoBlockType(llvm::OpaqueType::get())
  , typeInfoPtr(NULL)
  , typeAllocator(NULL)
{
  external = type->typeDefn()->module() != m;
  if (type->typeDefn()->isSynthetic()) {
    external = false;
    linkageType = GlobalValue::LinkOnceAnyLinkage;
  }
}

} // namespace tart
