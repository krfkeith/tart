/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_GC_GCSTRATEGY_H
#define TART_GC_GCSTRATEGY_H

#ifndef TART_OBJECTS_TRACING_H
#include "tart/Objects/Tracing.h"
#endif

#include "llvm/CodeGen/GCStrategy.h"
#include "llvm/CodeGen/GCMetadata.h"
#include "llvm/CodeGen/GCMetadataPrinter.h"
#include "llvm/CodeGen/AsmPrinter.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/ADT/FoldingSet.h"

namespace tart {

using namespace llvm;

class StackTraceEntry {
public:
  StackTraceEntry(intptr_t offset) {
    offset_ = offset;
    meta_ = NULL;
  }

  intptr_t offset() const { return offset_; }
  llvm::Constant * meta() const { return meta_; }

private:
  intptr_t offset_;
  llvm::Constant * meta_;
};

struct StackTraceTable : public llvm::FoldingSetNode {
  typedef llvm::SmallVector<StackTraceEntry, 16> EntryList;

  StackTraceTable() {}
  StackTraceTable(const EntryList & srcEntries)
    : entries(srcEntries)
  {}

  StackTraceTable(const StackTraceTable & src)
    : entries(src.entries)
    , label(src.label)
  {}

  void operator=(const StackTraceTable & src) {
    entries = src.entries;
    label = src.label;
  }

  static void ProfileEntries(FoldingSetNodeID &ID, const EntryList & entries) {
    for (EntryList::const_iterator it = entries.begin(); it != entries.end(); ++it) {
      ID.AddInteger(it->offset());
      if ((it->offset() & TRACE_ENTRY_MASK) == TRACE_ENTRY_STRUCT) {
        ID.AddPointer(it->meta());
      }
    }
  }

  void Profile(FoldingSetNodeID &ID) const {
    ProfileEntries(ID, entries);
  }

  EntryList entries;
  llvm::MCSymbol * label;
  //unsigned label;
};

class TartGCStrategy : public GCStrategy {
public:
  TartGCStrategy() {
    //InitRoots = true;
    UsesMetadata = true;
    NeededSafePoints = 1 << GC::PostCall;
  }
};

class TartGCPrinter : public llvm::GCMetadataPrinter {
public:
  virtual void beginAssembly(AsmPrinter &AP);
  virtual void finishAssembly(AsmPrinter &AP);

private:
  llvm::FoldingSet<StackTraceTable> traceTables;
};

} // namespace tart

#endif // TART_GC_GCSTRATEGY_H
