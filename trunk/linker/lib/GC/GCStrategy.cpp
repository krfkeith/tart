/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/GC/GCStrategy.h"
#include "llvm/ADT/SmallPtrSet.h"
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

typedef llvm::SmallVector<std::pair<MCSymbol *, MCSymbol *>, 64> SafePointList;

GCRegistry::Add<TartGCStrategy>
AddTartGC("tart-gc", "Tart garbage collector.");

GCMetadataPrinterRegistry::Add<TartGCPrinter>
AddTartGCPrinter("tart-gc", "Tart garbage collector.");

void addTartGC() {}

// -------------------------------------------------------------------
// TartGCStrategy

bool TartGCStrategy::performCustomLowering(Function & fn) {
  bool madeChange = false;
  SmallVector<AllocaInst*, 32> roots;

  for (Function::iterator bb = fn.begin(), bbEnd = fn.end(); bb != bbEnd; ++bb) {
    for (BasicBlock::iterator II = bb->begin(), E = bb->end(); II != E; ) {
      if (IntrinsicInst * CI = dyn_cast<IntrinsicInst>(II++)) {
        if (Function * F = CI->getCalledFunction()) {
          switch (F->getIntrinsicID()) {
          case Intrinsic::gcwrite:
            // Handle llvm.gcwrite.
            //CI->eraseFromParent();
            //MadeChange = true;
            break;
          case Intrinsic::gcread:
            // Handle llvm.gcread.
            //CI->eraseFromParent();
            //MadeChange = true;
            break;
          case Intrinsic::gcroot:
            // Initialize the GC root, but do not delete the intrinsic. The
            // backend needs the intrinsic to flag the stack slot.
            roots.push_back(cast<AllocaInst>(
                CI->getArgOperand(0)->stripPointerCasts()));
            madeChange = true;
            break;
          }
        }
      }
    }
  }

  if (roots.size()) {
    madeChange |= insertRootInitializers(fn, roots.begin(), roots.size());
  }

  return madeChange;
}

bool TartGCStrategy::insertRootInitializers(Function & fn, AllocaInst ** roots, unsigned count) {
  // Scroll past alloca instructions.
  BasicBlock::iterator ip = fn.getEntryBlock().begin();
  while (isa<AllocaInst>(ip)) {
    ++ip;
  }

  // Search for initializers in the initial BB.
  SmallPtrSet<AllocaInst*,16> initedRoots;
  for (; !couldBecomeSafePoint(ip); ++ip) {
    if (StoreInst * si = dyn_cast<StoreInst>(ip)) {
      if (AllocaInst * ai = dyn_cast<AllocaInst>(si->getOperand(1)->stripPointerCasts())) {
        initedRoots.insert(ai);
      }
    }
  }

  // Add root initializers.
  bool madeChange = false;

  // Initialize each root to null.
  for (AllocaInst ** ai = roots, ** E = roots + count; ai != E; ++ai) {
    if (!initedRoots.count(*ai)) {
      const Type * type = cast<PointerType>((*ai)->getType())->getElementType();
      StoreInst * storeInst;
      if (const PointerType * ptype = dyn_cast<PointerType>(type)) {
        storeInst = new StoreInst(ConstantPointerNull::get(ptype), *ai);
      } else {
        storeInst = new StoreInst(ConstantAggregateZero::get(type), *ai);
      }
      storeInst->insertAfter(*ai);
      madeChange = true;
    }
  }

  return madeChange;
}

// CouldBecomeSafePoint - Predicate to conservatively determine whether the
// instruction could introduce a safe point.
bool TartGCStrategy::couldBecomeSafePoint(Instruction * inst) {
  // The natural definition of instructions which could introduce safe points
  // are:
  //
  //   - call, invoke (AfterCall, BeforeCall)
  //   - phis (Loops)
  //   - invoke, ret, unwind (Exit)
  //
  // However, instructions as seemingly inoccuous as arithmetic can become
  // libcalls upon lowering (e.g., div i64 on a 32-bit platform), so instead
  // it is necessary to take a conservative approach.

  if (isa<AllocaInst>(inst) || isa<GetElementPtrInst>(inst) ||
      isa<StoreInst>(inst) || isa<LoadInst>(inst))
    return false;

  // llvm.gcroot is safe because it doesn't do anything at runtime.
  if (CallInst * callInst = dyn_cast<CallInst>(inst)) {
    if (Function * calledFn = callInst->getCalledFunction()) {
      if (unsigned iid = calledFn->getIntrinsicID()) {
        if (iid == Intrinsic::gcroot) {
          return false;
        }
      }
    }
  }

  return true;
}

void TartGCPrinter::beginAssembly(AsmPrinter &AP) {
  // Nothing to do.
}

// -------------------------------------------------------------------
// TartGCPrinter

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

    // And each safe point...
    for (GCFunctionInfo::iterator sp = gcFn.begin(); sp != gcFn.end(); ++sp) {
      StackTraceTable::FieldOffsetList fieldOffsets;
      StackTraceTable::TraceMethodList traceMethods;

      // And for each live root...
      for (GCFunctionInfo::live_iterator rt = gcFn.live_begin(sp); rt != gcFn.live_end(sp); ++rt) {
        int64_t offset = rt->StackOffset;
        const Constant * meta = rt->Metadata;

        if (meta != NULL && !meta->isNullValue()) {
          // Meta is non-null, so it's a value type.
          const ConstantArray * traceArray = cast<ConstantArray>(getGlobalValue(meta));

          // For each trace descriptor in thre meta array...
          for (ConstantArray::const_op_iterator it = traceArray->op_begin();
              it != traceArray->op_end(); ++it) {
            ConstantStruct * descriptor = cast<ConstantStruct>(*it);
            ConstantInt * fieldCount = cast<ConstantInt>(descriptor->getOperand(1));
            int64_t dscOffset = toInt(descriptor->getOperand(2), AP.TM);

            if (fieldCount->isZero()) {
              // A zero field count means that this is a trace method descriptor.
              const Constant * traceMethod = descriptor->getOperand(3);
              assert(offset > -1000 && offset < 1000);
              assert(dscOffset > -1000 && dscOffset < 1000);
              traceMethods.push_back(TraceMethodEntry(offset + dscOffset, traceMethod));
            } else {
              // Otherwise it's a field offset descriptor.
              const GlobalVariable * fieldOffsetsVar = cast<GlobalVariable>(
                  descriptor->getOperand(3)->getOperand(0));

              // Handle case where the array value is just a ConstantAggregateZero, which
              // can be generated by llvm::ConstantArray::get() if the array values
              // are all zero.
              if (const ConstantAggregateZero * zero =
                  dyn_cast<ConstantAggregateZero>(fieldOffsetsVar->getInitializer())) {
                // Array should never contain duplicate offsets, so an all-zero array
                // can only have one entry.
                (void)zero;
                assert(fieldCount->isOne());
                fieldOffsets.push_back(offset + dscOffset);
              } else {
                // Get the field offset array and add to field offsets for this
                // safe point.
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
          // No metadata, so it's an object reference - just add the field offset.
          fieldOffsets.push_back(offset);
        }
      }

      // Nothing to trace? Then we're done.
      if (fieldOffsets.empty() && traceMethods.empty()) {
        continue;
      }

      // Create a folding set node and merge with any identical trace tables.
      std::sort(fieldOffsets.begin(), fieldOffsets.end());
      llvm::FoldingSetNodeID id;
      StackTraceTable::ProfileEntries(id, fieldOffsets, traceMethods);

      void * insertPos;
      StackTraceTable * sTable = traceTables.FindNodeOrInsertPos(id, insertPos);
      if (sTable == NULL) {
        sTable = new StackTraceTable(fieldOffsets, traceMethods);

        // Generate the labels for the trace table and field offset table.
        sTable->fieldOffsetsLabel = AP.GetTempSymbol("gc_stack_offsets", nextLabel);
        sTable->traceTableLabel = AP.GetTempSymbol("gc_stack", nextLabel++);

        // Add to folding set
        traceTables.InsertNode(sTable, insertPos);

        // Generate the trace table
        outStream.AddBlankLine();
        AP.EmitAlignment(addressAlignLog);

        // First the field offset descriptor
        outStream.EmitLabel(sTable->traceTableLabel);
        size_t traceMethodCount = sTable->traceMethods.size();
        if (!sTable->fieldOffsets.empty()) {
          outStream.EmitIntValue(traceMethodCount == 0 ? 1 : 0, 2, 0);
          outStream.EmitIntValue(sTable->fieldOffsets.size(), 2, 0);
          outStream.EmitIntValue(0, 4, 0);
          outStream.EmitSymbolValue(sTable->fieldOffsetsLabel, pointerSize, 0);
        }

        // Next the trace method descriptors
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

        // Now emit the field offset array
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

  // Finally, generate the safe point map.
  outStream.AddBlankLine();
  MCSymbol * gcSafepointSymbol = AP.GetExternalSymbolSymbol("GC_safepoint_map");
  outStream.EmitSymbolAttribute(gcSafepointSymbol, MCSA_Global);
  outStream.EmitLabel(gcSafepointSymbol);
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

llvm::Constant * TartGCPrinter::getGlobalValue(const llvm::Constant * c) {
  const GlobalVariable * var = dyn_cast<GlobalVariable>(c);
  if (var == NULL) {
    var = cast<GlobalVariable>(c->getOperand(0));
  }
  return var->getInitializer();
}

}
