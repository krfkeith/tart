/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/GC/GCStrategy.h"
#include "llvm/Function.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetData.h"
#include "llvm/Target/TargetLoweringObjectFile.h"
#include "llvm/MC/MCAsmInfo.h"
#include "llvm/MC/MCStreamer.h"

#include <stdio.h>

namespace tart {

using namespace llvm;

namespace {

struct OffsetComparator {
  bool operator()(const StackTraceEntry & a0, const StackTraceEntry & a1) {
    return a1.offset() < a0.offset();
  }
};

}

GCRegistry::Add<TartGCStrategy>
AddTartGC("tart-gc", "Tart garbage collector.");

GCMetadataPrinterRegistry::Add<TartGCPrinter>
AddTartGCPrinter("tart-gc", "Tart garbage collector.");

void addTartGC() {}

void TartGCPrinter::beginAssembly(AsmPrinter &AP) {
  // Nothing to do.
}

typedef llvm::SmallVector<std::pair<MCSymbol *, MCSymbol *>, 64> SafePointList;

void TartGCPrinter::finishAssembly(AsmPrinter &AP) {
  unsigned nextLabel = 1;
  SafePointList safePoints;

  // Set up for emitting addresses.
  int pointerSize = AP.TM.getTargetData()->getPointerSize();
  int addressAlignLog;
  if (pointerSize == sizeof(int32_t)) {
    addressAlignLog = 2;
  } else {
    addressAlignLog = 3;
  }

  MCStreamer & outStream = AP.OutStreamer;

  // Put this in the data section.
  outStream.SwitchSection(AP.getObjFileLowering().getDataSection());
  AP.EmitAlignment(addressAlignLog);

  // For each function...
  for (iterator FI = begin(), FE = end(); FI != FE; ++FI) {
    GCFunctionInfo & gcFn = **FI;

    // And for each live root...
    // And each safe point...
    for (GCFunctionInfo::iterator sp = gcFn.begin(); sp != gcFn.end(); ++sp) {
      StackTraceTable::EntryList entries;
      for (GCFunctionInfo::live_iterator rt = gcFn.live_begin(sp); rt != gcFn.live_end(sp); ++rt) {
        intptr_t offset = rt->StackOffset;
        const Constant * meta = rt->Metadata;
        entries.push_back(StackTraceEntry(offset));
      }

      if (entries.empty()) {
        continue;
      }

      std::sort(entries.begin(), entries.end(), OffsetComparator());

      llvm::FoldingSetNodeID id;
      StackTraceTable::ProfileEntries(id, entries);

      void * insertPos;
      StackTraceTable * sTable = traceTables.FindNodeOrInsertPos(id, insertPos);
      if (sTable == NULL) {
        sTable = new StackTraceTable(entries);
        sTable->label = AP.GetTempSymbol("gc_stack", nextLabel++);

        traceTables.InsertNode(sTable, insertPos);

        outStream.AddBlankLine();
        outStream.EmitLabel(sTable->label);
        for (StackTraceTable::EntryList::const_iterator it = entries.begin();
            it != entries.end(); ++it) {
          if (it->meta() == NULL) {
            outStream.EmitIntValue(it->offset(), pointerSize, 0);
          }
        }
        outStream.EmitIntValue(TRACE_ENTRY_END, pointerSize, 0);
      }

      safePoints.push_back(std::pair<MCSymbol *, MCSymbol *>(sp->Label, sTable->label));
    }
  }

  outStream.AddBlankLine();
  outStream.EmitLabel(AP.GetExternalSymbolSymbol("gc_safepoint_map"));
  outStream.EmitIntValue(safePoints.size(), pointerSize, 0);
  for (SafePointList::const_iterator it = safePoints.begin(); it != safePoints.end(); ++it) {
    outStream.EmitSymbolValue(it->first, pointerSize, 0);
    outStream.EmitSymbolValue(it->second, pointerSize, 0);
  }
}

}
