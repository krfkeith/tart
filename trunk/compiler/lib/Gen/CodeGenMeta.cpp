/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Defn/Module.h"

#include "tart/Gen/CodeGenerator.h"

#include "tart/Meta/Tags.h"
#include "tart/Meta/MDWriter.h"

#include "llvm/Module.h"

namespace tart {

using namespace llvm;

void CodeGenerator::genModuleMetadata() {
  MDWriter writer(*this);
  SmallString<128> mdName("tart.xdef.");
  mdName += module_->qualifiedName();
  NamedMDNode * md = irModule_->getOrInsertNamedMetadata(mdName);
  MDNode * members = writer.scopeMembers(module_);
  md->addOperand(getFormatVersion());
  md->addOperand(getModuleSource());
  md->addOperand(getModuleDeps());
  md->addOperand(getModuleTimestamp());
  md->addOperand(writer.moduleImports());
  md->addOperand(members);
}

MDNode * CodeGenerator::getFormatVersion() {
  ValueList values;
  values.push_back(ConstantInt::get(builder_.getInt32Ty(), TART_OBJECT_FORMAT_VERSION));
  return MDNode::get(context_, values);
}

MDNode * CodeGenerator::getModuleSource() {
  ValueList values;
  values.push_back(MDString::get(context_, module_->moduleSource()->filePath()));
  return MDNode::get(context_, values);
}

MDNode * CodeGenerator::getModuleDeps() {
  ValueList values;
  const ModuleSet & modules = module_->importModules();
  for (ModuleSet::const_iterator it = modules.begin(); it != modules.end(); ++it) {
    Module * m = *it;
    if (!m->moduleSource()->filePath().empty()) {
      Value * modAttrs[2];
      modAttrs[0] = MDString::get(context_, m->moduleSource()->filePath());
      modAttrs[1] = MDString::get(context_, m->qualifiedName());
      values.push_back(MDNode::get(context_, modAttrs));
    }
  }

  return MDNode::get(context_, values);
}

MDNode * CodeGenerator::getModuleTimestamp() {
  Value * modTimestamp[2];
  modTimestamp[0] = ConstantInt::get(builder_.getInt64Ty(), module_->timestamp().seconds());
  modTimestamp[1] = ConstantInt::get(builder_.getInt32Ty(), module_->timestamp().nanoseconds());
  return MDNode::get(context_, modTimestamp);
}

} // namespace tart
