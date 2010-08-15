/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_OBJECTS_TRACING_H
#define TART_OBJECTS_TRACING_H

#include "config.h"

#ifdef HAVE_STDINT_H
#include <stdint.h>
#endif

namespace tart {

/// ---------------------------------------------------------------
/// Various definitions for object tracing that are shared between
/// the compiler, linker and runtime.

/// A trace offset table consists of a list of offsets from a base
/// address that contain references to objects. The offsets will
/// normally be multiples of 4. Entries in the table which are
/// not multiples of 4 are special entries which tell the tracer
/// where additional references can be found.
enum TraceEntry {
  /// Indicates the end of the trace offset list.
  TRACE_ENTRY_END = 1,

  /// Indicates that the offset is not an object reference, but
  /// instead indicates the location of a pointer to a structure
  /// which contains object references. The offset to the pointer
  /// is stored in the high bits of this entry; Immediately
  /// following this entry is a pointer to the trace table for
  /// that structure.
  TRACE_ENTRY_STRUCT = 2,

  /// Mask bits that determines whether a trace entry should be
  /// interpreted as an offset or as a special entry.
  TRACE_ENTRY_MASK = 3,
};

inline bool isSpecialTraceEntry(intptr_t offset) {
  return (offset & TRACE_ENTRY_MASK) != 0;
}

} // namespace tart

#endif // TART_OBJECTS_TRACING_H
