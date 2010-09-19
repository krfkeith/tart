/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_GC_GCSTRATEGY_H
#define TART_GC_GCSTRATEGY_H

#include "llvm/CodeGen/GCStrategy.h"
#include "llvm/CodeGen/GCMetadata.h"
#include "llvm/CodeGen/GCMetadataPrinter.h"
#include "llvm/CodeGen/AsmPrinter.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/ADT/FoldingSet.h"

namespace tart {

using namespace llvm;

struct TraceMethodEntry {
public:
  TraceMethodEntry(int64_t offset, const llvm::Constant * method)
    : offset_(offset), method_(method) {}
  TraceMethodEntry(const TraceMethodEntry & src) {
    offset_ = src.offset_;
    method_ = src.method_;
  }

  TraceMethodEntry & operator=(const TraceMethodEntry & src) {
    offset_ = src.offset_;
    method_ = src.method_;
    return *this;
  }

  int64_t offset() const { return offset_; }
  const llvm::Constant * method() const { return method_; }

private:
  int64_t offset_;
  const llvm::Constant * method_;
};

struct StackTraceTable : public llvm::FoldingSetNode {
  typedef llvm::SmallVector<int64_t, 16> FieldOffsetList;
  typedef llvm::SmallVector<TraceMethodEntry, 8> TraceMethodList;

  StackTraceTable() {}
  StackTraceTable(const FieldOffsetList & offsets, const TraceMethodList & methods)
    : fieldOffsets(offsets)
    , traceMethods(methods)
    , fieldOffsetsLabel(NULL)
    , traceTableLabel(NULL)
  {}

  StackTraceTable(const StackTraceTable & src)
    : llvm::FoldingSetNode(src)
    , fieldOffsets(src.fieldOffsets)
    , traceMethods(src.traceMethods)
    , fieldOffsetsLabel(NULL)
    , traceTableLabel(NULL)
  {}

  void operator=(const StackTraceTable & src) {
    llvm::FoldingSetNode::operator=(src);
    fieldOffsets = src.fieldOffsets;
    traceMethods = src.traceMethods;
    fieldOffsetsLabel = src.fieldOffsetsLabel;
    traceTableLabel = src.traceTableLabel;
  }

  static void ProfileEntries(FoldingSetNodeID &ID, const FieldOffsetList & fieldOffsets,
      const TraceMethodList & traceMethods) {
    for (FieldOffsetList::const_iterator it = fieldOffsets.begin();
        it != fieldOffsets.end(); ++it) {
      ID.AddInteger(*it);
    }
    for (TraceMethodList::const_iterator it = traceMethods.begin();
        it != traceMethods.end(); ++it) {
      ID.AddInteger(it->offset());
      ID.AddPointer(it->method());
    }
  }

  void Profile(FoldingSetNodeID &ID) const {
    ProfileEntries(ID, fieldOffsets, traceMethods);
  }

  FieldOffsetList fieldOffsets;
  TraceMethodList traceMethods;
  llvm::MCSymbol * fieldOffsetsLabel;
  llvm::MCSymbol * traceTableLabel;
};

class TartGCStrategy : public GCStrategy {
public:
  TartGCStrategy() {
    InitRoots = false;
    UsesMetadata = true;
    NeededSafePoints = 1 << GC::PostCall;
  }
};

class TartGCPrinter : public llvm::GCMetadataPrinter {
public:
  void beginAssembly(AsmPrinter &AP);
  void finishAssembly(AsmPrinter &AP);

private:
  int64_t toInt(llvm::Constant * c, TargetMachine & tm);

  llvm::FoldingSet<StackTraceTable> traceTables;
};

} // namespace tart

#endif // TART_GC_GCSTRATEGY_H
