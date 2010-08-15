/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

/** Object tracing. */

#include "tart/Objects/Tracing.h"

namespace tart {

class TracerBase {
public:
  // Can't use pure virtual because we're not linking with the cpp lib.
  virtual void execute(uint8_t * basePtr, intptr_t * traceTable) {
    (void)basePtr;
    (void)traceTable;
  };
};

template <class TraceAction> class Tracer : public TracerBase {
private:
  TraceAction action;

  void trace(uint8_t * basePtr, intptr_t * traceTable) {
    for (;;) {
      intptr_t entry = *traceTable++;
      // It's a processing instruction
      switch (entry & TRACE_ENTRY_MASK) {
        case 0:
          // It's a simple offset from the base pointer
          action(reinterpret_cast<tart_object **>(basePtr + entry));
          break;

        case TRACE_ENTRY_END:
          // We're done with this trace table.
          return;

        case TRACE_ENTRY_STRUCT: {
          intptr_t offset = entry & ~TRACE_ENTRY_MASK;
          intptr_t * structTable = reinterpret_cast<intptr_t *>(*traceTable++);
          uint8_t ** structAddr = reinterpret_cast<uint8_t **>(basePtr + offset);
          trace(*structAddr, structTable);
          break;
        }

        default:
          fprintf(stderr, "Invalid trace table entry: %d", int(entry));
          break;
      }
    }
  }

public:
  void execute(uint8_t * basePtr, intptr_t * traceTable) {
    trace(basePtr, traceTable);
  }
};

} // namespace tart
