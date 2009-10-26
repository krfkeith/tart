/* Exception personality function for Tart */

#include "unwind.h"
#include "stdio.h"
#include "stdlib.h"
#include "stdint.h"
#include "stddef.h"
#include "stdbool.h"
#include "string.h"
//#include "config.h"

#define TART_EXCEPTION_CLASS 0
//#define TART_EXCEPTION_CLASS (('T' << 56L) << ('A' << 48L) << ('R' << 40L) << ('T' << 32L))

#define DW_EH_PE_absptr 0x00
#define DW_EH_PE_omit 0xff
#define DW_EH_PE_udata4 0x03

// Opaque definition of a Tart type
struct Type;

// Every Tart object begins with a pointer to a TIB (TypeInfoBlock) which contains the
// method table and other type information. The list of base classes is a NULL-terminated
// list of type pointers.
struct TypeInfoBlock {
  const struct Type * type;
  const struct TypeInfoBlock * const * bases;
  void ** methodTable;
};

// The Throwable class
struct TartThrowable {
  struct TypeInfoBlock * tib;
  void * padding; // Required for UnwindExceptinfo alignment.

  // _Unwind_Exception follows, but don't explicitly declare it because
  // we need to control the alignment. (The structure definition in unwind.h aligns to
  // the maximum possible alignment for the platform, which is not what our frontend
  // generates.)
};

// Information read from the Language-Specific Data Area (LSDA)
struct LSDAHeaderInfo {
  _Unwind_Ptr regionStart;
  _Unwind_Ptr landingPadStart;
  const unsigned char * typeTable;
  const unsigned char * actionTable;
  unsigned char typeTableEncoding;
  unsigned char callSiteEncoding;
};

// Information about a Call Site.
struct CallSiteInfo {
  _Unwind_Ptr landingPad;
  const unsigned char * actionRecord;
};

// isSubclass() test for Tart objects.
static bool hasBase(const struct TypeInfoBlock * tib, const struct TypeInfoBlock * type) {
  if (tib == type) {
    return true;
  }

  const struct TypeInfoBlock * const * bases = tib->bases;
  const struct TypeInfoBlock * base;
  while ((base = *bases++) != NULL) {
    if (base == type) {
      return true;
    }
  }

  return false;
}

// Read an unsigned word in LBE128 (Little-Endian Base 128)
static const unsigned char * readEncodedUWord(const unsigned char * pos, _Unwind_Word * out) {
  unsigned int shift = 0;
  unsigned char c;
  _Unwind_Word n = 0;
  do {
    c = *pos++;
    n |= (c & 0x7f) << shift;
    shift += 7;
  } while (c & 0x80) ;

  *out = n;
  return pos;
}

// Read a signed word in LBE128 (Little-Endian Base 128)
static const unsigned char * readEncodedSWord(const unsigned char * pos, _Unwind_Sword * out) {
  unsigned int shift = 0;
  unsigned char c;
  _Unwind_Word n = 0;
  do {
    c = *pos++;
    n |= (c & 0x7f) << shift;
    shift += 7;
  } while (c & 0x80) ;

  // Check sign bit.
  if ((c & 0x40) && shift < sizeof(n) * 8) {
    n |= -(1L << shift);
  }

  *out = n;
  return pos;
}

static const unsigned char * readEncodedValue(
    _Unwind_Ptr baseAddr,
    unsigned char encoding,
    const unsigned char * pos,
    _Unwind_Ptr * out) {

  uint32_t offset;

  // LLVM only emits format DW_EH_PE_udata4
  if (encoding != DW_EH_PE_udata4) {
    // Other encodings not implemented.
    abort();
  }

  memcpy(&offset, pos, sizeof(offset));
  pos += sizeof(offset);
  *out = baseAddr + offset;
  return pos;
}

// Parse the language-specific data area (LSDA) Header.
const unsigned char * parseLDSAHeader(const unsigned char * pos, struct LSDAHeaderInfo * lpInfo) {
  _Unwind_Word offset;

  // Find @LPStart, the start of the landing pad.
  if (*pos++ == DW_EH_PE_omit) {
    lpInfo->landingPadStart = lpInfo->regionStart;
  } else {
    // Unsupported encoding (not used by LLVM)
    abort();
  }

  // Find @TType, the base of the handler and exception spec type data.
  lpInfo->typeTableEncoding = *pos++;
  if (lpInfo->typeTableEncoding == DW_EH_PE_omit) {
    lpInfo->typeTable = 0;
  } else {
    pos = readEncodedUWord(pos, &offset);
    lpInfo->typeTable = pos + offset;
  }

  // Encoding of the call-site table.
  lpInfo->callSiteEncoding = *pos++;

  // Offset to the action table
  pos = readEncodedUWord(pos, &offset);
  lpInfo->actionTable = pos + offset;

  // Return the current parse position.
  return pos;
}

const unsigned char * findCallSite(
    const unsigned char * pos,
    struct LSDAHeaderInfo * lpInfo,
    _Unwind_Ptr ip,
    struct CallSiteInfo * csInfo) {

  csInfo->landingPad = 0;
  csInfo->actionRecord = NULL;

  // Search the call-site table for the action associated with the given IP.
  while (pos < lpInfo->actionTable) {
    _Unwind_Ptr callSiteStart;
    _Unwind_Ptr callSiteLength;
    _Unwind_Ptr callSiteLandingPad;
    _Unwind_Word callSiteAction;

    // Note that all call-site encodings are "absolute" displacements.
    pos = readEncodedValue(0, lpInfo->callSiteEncoding, pos, &callSiteStart);
    pos = readEncodedValue(0, lpInfo->callSiteEncoding, pos, &callSiteLength);
    pos = readEncodedValue(0, lpInfo->callSiteEncoding, pos, &callSiteLandingPad);
    pos = readEncodedUWord(pos, &callSiteAction);

    // The table is sorted, so if we've passed the ip, stop.
    if (ip < lpInfo->regionStart + callSiteStart) {
      pos = lpInfo->actionTable;
    } else if (ip < lpInfo->regionStart + callSiteStart + callSiteLength) {
      if (callSiteLandingPad) {
        csInfo->landingPad = lpInfo->landingPadStart + callSiteLandingPad;
      }

      if (callSiteAction) {
        csInfo->actionRecord = lpInfo->actionTable + callSiteAction - 1;
      }

      break;
    }
  }

  return pos;
}

// Find the action record for the given exception and call site.
bool findAction(
    struct LSDAHeaderInfo * lpInfo,
    struct CallSiteInfo * csInfo,
    struct TypeInfoBlock * tib,
    int * actionResult) {
  const unsigned char * action = csInfo->actionRecord;
  int actionResultIndex = 0;
  for (;;) {
    _Unwind_Sword actionFilter;
    _Unwind_Sword nextActionOffset;

    action = readEncodedSWord(action, &actionFilter);
    readEncodedSWord(action, &nextActionOffset);

    if (actionFilter == 0) {
      *actionResult = actionResultIndex;
      return true;
    } else if (actionFilter > 0 && tib != NULL) {
      // LLVM only uses DW_EH_PE_absptr encoding, anything else not supported.
      if (lpInfo->typeTableEncoding != DW_EH_PE_absptr) {
        abort();
      }

      // Look up the type in the type table.
      const struct TypeInfoBlock * const * typeTable =
          (const struct TypeInfoBlock * const *) lpInfo->typeTable;
      const struct TypeInfoBlock * type = typeTable[-actionFilter];
      if (hasBase(tib, type)) {
        *actionResult = actionResultIndex;
        return true;
      }
    } else {
      // Unsupported, for now.
      abort();
    }

    if (nextActionOffset == 0) {
      return false;
    }

    action += nextActionOffset;
    actionResultIndex += 1;
  }
}

#if 0
_Unwind_Reason_Code backtraceCallback(struct _Unwind_Context * context, void * vthrowable) {
  const struct TartThrowable * throwable = (const struct TartThrowable *) vthrowable;
  _Unwind_Ptr ip = _Unwind_GetIP(context);
  fprintf(stderr, "Backtrace callback %p\n", ip);
  return _URC_NO_REASON;
}
#endif

_Unwind_Reason_Code __tart_eh_personality(
    int version,
    _Unwind_Action actions,
    _Unwind_Exception_Class exceptionClass,
    struct _Unwind_Exception * ueHeader,
    struct _Unwind_Context * context)
{
  const struct TartThrowable * throwable =
      (struct TartThrowable *)((_Unwind_Ptr)ueHeader - sizeof(struct TartThrowable));
  struct LSDAHeaderInfo lpInfo;
  struct CallSiteInfo csInfo;
  _Unwind_Ptr ip;
  const unsigned char * langSpecData;
  const unsigned char * pos;
  bool forceUnwind = (actions & _UA_FORCE_UNWIND) || exceptionClass != TART_EXCEPTION_CLASS;

  if (version != 1) {
    return _URC_FATAL_PHASE1_ERROR;
  }

#if 0
  ip = _Unwind_GetIP(context) - 1;
  fprintf(stderr, "Begin Backtrace %d\n", actions);
  _Unwind_Reason_Code code;
  code = _Unwind_Backtrace(backtraceCallback, (void *) throwable);
  fprintf(stderr, "End Backtrace %d %p\n\n", code, ip);
#endif

  // Find the language-specific data
  langSpecData = (const unsigned char *) _Unwind_GetLanguageSpecificData(context);
  if (!langSpecData) {
    return _URC_CONTINUE_UNWIND;
  }

  lpInfo.regionStart = _Unwind_GetRegionStart(context);
  ip = _Unwind_GetIP(context) - 1;

  // Find the call site that threw the exception.
  pos = parseLDSAHeader(langSpecData, &lpInfo);
  pos = findCallSite(pos, &lpInfo, ip, &csInfo);
  if (csInfo.landingPad == 0) {
    return _URC_CONTINUE_UNWIND;
  } else if (csInfo.actionRecord == NULL) {
    return _URC_CONTINUE_UNWIND;
  }

  // Find the action record for the given exception and call site.
  int action;
  if (findAction(&lpInfo, &csInfo, forceUnwind ? NULL : throwable->tib, &action)) {
    if (actions == _UA_SEARCH_PHASE) {
      return _URC_HANDLER_FOUND;
    } else if (actions == (_UA_CLEANUP_PHASE | _UA_HANDLER_FRAME)) {
      _Unwind_SetIP(context, csInfo.landingPad);
      _Unwind_SetGR (context, __builtin_eh_return_data_regno(0), (_Unwind_Ptr) ueHeader);
      _Unwind_SetGR (context, __builtin_eh_return_data_regno(1), action);
      return _URC_INSTALL_CONTEXT;
    }
  }

  // No action was found.
  return _URC_CONTINUE_UNWIND;
}
