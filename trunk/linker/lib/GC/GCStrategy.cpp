/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/GC/GCStrategy.h"
#include "llvm/Function.h"
#include "llvm/Target/Mangler.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetData.h"
#include "llvm/Target/TargetLoweringObjectFile.h"
#include "llvm/MC/MCAsmInfo.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/GlobalVariable.h"
#include "llvm/Constants.h"
#include "llvm/Analysis/ConstantFolding.h"

#include <stdio.h>

namespace tart {

using namespace llvm;

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

  // For each function...
  for (iterator FI = begin(), FE = end(); FI != FE; ++FI) {
    GCFunctionInfo & gcFn = **FI;

    // And for each live root...
    // And each safe point...
    for (GCFunctionInfo::iterator sp = gcFn.begin(); sp != gcFn.end(); ++sp) {
      StackTraceTable::FieldOffsetList fieldOffsets;
      StackTraceTable::TraceMethodList traceMethods;
      for (GCFunctionInfo::live_iterator rt = gcFn.live_begin(sp); rt != gcFn.live_end(sp); ++rt) {
        int64_t offset = rt->StackOffset;
        const Constant * meta = rt->Metadata;

        if (meta != NULL && !meta->isNullValue()) {
          const GlobalVariable * traceTable = dyn_cast<GlobalVariable>(meta);
          if (traceTable == NULL) {
            traceTable = cast<GlobalVariable>(meta->getOperand(0));
          }
          const ConstantArray * traceArray = cast<ConstantArray>(traceTable->getInitializer());
          for (ConstantArray::const_op_iterator it = traceArray->op_begin();
              it != traceArray->op_end(); ++it) {
            ConstantStruct * descriptor = cast<ConstantStruct>(*it);
            ConstantInt * fieldCount = cast<ConstantInt>(descriptor->getOperand(1));
            int64_t dscOffset = toInt(descriptor->getOperand(2), AP.TM);
            if (fieldCount->isZero()) {
              const Constant * traceMethod = descriptor->getOperand(3);
              traceMethods.push_back(TraceMethodEntry(offset + dscOffset, traceMethod));
            } else {
              const GlobalVariable * fieldOffsetsVar = cast<GlobalVariable>(
                  descriptor->getOperand(3)->getOperand(0));
              if (const ConstantAggregateZero * zero =
                  dyn_cast<ConstantAggregateZero>(fieldOffsetsVar->getInitializer())) {
                assert(fieldCount->isOne());
                fieldOffsets.push_back(offset + dscOffset);
              } else {
                const ConstantArray * fieldOffsetArray = cast<ConstantArray>(
                    fieldOffsetsVar->getInitializer());
                for (ConstantArray::const_op_iterator el = fieldOffsetArray->op_begin();
                    el != fieldOffsetArray->op_end(); ++el) {
                  fieldOffsets.push_back(
                      offset + dscOffset + toInt(cast<llvm::Constant>(*el), AP.TM));
                }
              }
            }
          }
        } else {
          fieldOffsets.push_back(offset);
        }
      }

      if (fieldOffsets.empty() && traceMethods.empty()) {
        continue;
      }

      //std::sort(fieldOffsets.begin(), fieldOffsets.end(), OffsetComparator());
      std::sort(fieldOffsets.begin(), fieldOffsets.end());

      llvm::FoldingSetNodeID id;
      StackTraceTable::ProfileEntries(id, fieldOffsets, traceMethods);

      void * insertPos;
      StackTraceTable * sTable = traceTables.FindNodeOrInsertPos(id, insertPos);
      if (sTable == NULL) {
        sTable = new StackTraceTable(fieldOffsets, traceMethods);
        sTable->fieldOffsetsLabel = AP.GetTempSymbol("gc_stack_offsets", nextLabel);
        sTable->traceTableLabel = AP.GetTempSymbol("gc_stack", nextLabel++);

        traceTables.InsertNode(sTable, insertPos);

        outStream.AddBlankLine();
        AP.EmitAlignment(addressAlignLog);

        outStream.EmitLabel(sTable->traceTableLabel);
        size_t traceMethodCount = sTable->traceMethods.size();
        if (!sTable->fieldOffsets.empty()) {
          outStream.EmitIntValue(traceMethodCount == 0 ? 1 : 0, 2, 0);
          outStream.EmitIntValue(sTable->fieldOffsets.size(), 2, 0);
          outStream.EmitIntValue(0, 4, 0);
          outStream.EmitSymbolValue(sTable->fieldOffsetsLabel, pointerSize, 0);
        }

        for (size_t i = 0; i < traceMethodCount; ++i) {
          const TraceMethodEntry * tm = &sTable->traceMethods[i];
          const Function * method = dyn_cast<Function>(tm->method());
          if (method == NULL) {
            method = cast<Function>(tm->method()->getOperand(0));
          }

          outStream.EmitIntValue((i + 1 == traceMethodCount ? 1 : 0), 2, 0);
          outStream.EmitIntValue(0, 2, 0);
          outStream.EmitIntValue(tm->offset(), 4, 0);
          MCSymbol * methodSym = AP.Mang->getSymbol(method);
          outStream.EmitSymbolValue(methodSym, pointerSize, 0);
        }

        outStream.AddBlankLine();
        AP.EmitAlignment(addressAlignLog);

        outStream.EmitLabel(sTable->fieldOffsetsLabel);
        for (StackTraceTable::FieldOffsetList::const_iterator it = fieldOffsets.begin();
            it != fieldOffsets.end(); ++it) {
          outStream.EmitIntValue(*it, pointerSize, 0);
        }
      }

      safePoints.push_back(std::pair<MCSymbol *, MCSymbol *>(sp->Label, sTable->traceTableLabel));
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

int64_t TartGCPrinter::toInt(llvm::Constant * c, TargetMachine & tm) {
  if (llvm::ConstantExpr * ce = dyn_cast<llvm::ConstantExpr>(c)) {
    c = ConstantFoldConstantExpression(ce, tm.getTargetData());
    if (c == ce) {
      ce->dump();
      assert(false && "Constant could not be folded");
    }

    return toInt(c, tm);
  } else if (ConstantInt * ci = dyn_cast<ConstantInt>(c)) {
    return ci->getValue().getSExtValue();
  } else {
    c->dump();
    assert(false && "Constant offset is not an integer expression");
  }
}

}
