/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "config.h"
#include "tart/Common/Diagnostics.h"
#include "tart/Common/SourceFile.h"
#include "llvm/Support/CommandLine.h"
#include <stdio.h>

#if HAVE_EXECINFO_H
#include <execinfo.h>         // For backtrace().
#endif

#if HAVE_CXXABI_H
#include <cxxabi.h>
#endif

#if HAVE_DLFCN_H && __GNUG__
#include <dlfcn.h>
#endif

static llvm::cl::opt<bool>
DebugErrors("debug-errors",
    llvm::cl::desc("Print compiler stack trace on fatal error"));

namespace tart {

Diagnostics::StdErrWriter Diagnostics::StdErrWriter::instance;

namespace {
  const char * severityNames[Diagnostics::Severity_Levels] = {
    "",
    "info: ",
    "warning: ",
    "error: ",
    "error: ",
  };

  static const char * INDENTATION = "                                ";
  static int MAX_INDENT = 16;
}

Diagnostics diag;

Diagnostics::Diagnostics()
    : writer_(&StdErrWriter::instance)
    , recovery(Open)
    , indentLevel(0)
    , minSeverity(Debug) {
  reset();
}

void Diagnostics::reset() {
  // Clear message counts
  for (int i = 0; i < Severity_Levels; i++) {
    messageCount[i] = 0;
  }
  recovery = Open;
}

// -------------------------------------------------------------------
// Error reporting functions
// -------------------------------------------------------------------

void Diagnostics::write(const SourceLocation & loc, Severity sev,
    StringRef msg) {

  switch (sev) {
    case Fatal:
    case Error:
      // If we are recovering from another error, don't show this one.
      if (recovery == Closed || recovery == Gated) {
        recovery = Closed;
        return;
      }

      recovery = Gated;
      break;

    case Warning:
    case Info:
      // Allow info messages to follow-up a fatal, but not
      // if the fatal was suppressed from an earlier fatal.
      if (recovery == Closed) {
        return;
      }
      break;

    default:
      break;
  }

  if (writer_ && sev >= minSeverity) {
    writer_->write(loc, sev, msg);
    if (sev == Fatal && DebugErrors) {
      printStackTrace(4);
      abort();
    }
  }

  messageCount[(int)sev] += 1;
}

void Diagnostics::debug(char * msg, ...) {
  va_list ap;
  va_start(ap, msg);
  vfprintf(stderr, msg, ap);
  va_end(ap);
}

void Diagnostics::printContextStack(SourceContext * source) {
  while (source) {
    if (source->expression()) {
      diag.info(source) << FormatOptions(source->formatOptions()) << source->expression();
      source = source->parent();
    }
  }
}

void Diagnostics::indent() {
  ++indentLevel;
}

void Diagnostics::unindent() {
  --indentLevel;
}

int Diagnostics::getIndentLevel() {
  return indentLevel;
}

void Diagnostics::writeIndent(int level) {
  fprintf(stderr, "%.*s", std::min(level, MAX_INDENT) * 2, INDENTATION);
}

void Diagnostics::writeIndent(FormatStream & out, int level) {
  for (int i = 0; i < level; ++i) {
    out << " ";
  }
}

int Diagnostics::setIndentLevel(int level) {
  int result = indentLevel;
  indentLevel = level;
  return result;
}

void Diagnostics::writeLnIndent(StringRef str) {
  writeIndent(indentLevel);
  fprintf(stderr, "%s\n", str.data());
}

void Diagnostics::writeLnIndent(const char * msg, ...) {
  va_list ap;
  va_start(ap, msg);
  writeIndent(indentLevel);
  fprintf(stderr, "\n");
  vfprintf(stderr, msg, ap);
  va_end(ap);
}

void Diagnostics::__fail(StringRef msg, const char * filename, unsigned lineno) {
  llvm::errs() << filename << ":" << lineno << ": Fatal error (" << msg << ")\n";
  debugBreak();
  printStackTrace(2);
  abort();
}

void Diagnostics::debugBreak() {
  fflush(stderr);
}

void Diagnostics::printStackTrace(int skipFrames) {
#if HAVE_BACKTRACE
  static void * stackTrace[256];

  // Use backtrace() to output a backtrace on Linux systems with glibc.
  int depth = backtrace(stackTrace,
      static_cast<int>(sizeof(stackTrace) / sizeof(stackTrace[0])));

#if HAVE_DLFCN_H && __GNUG__
  for (int i = skipFrames; i < depth; ++i) {
    Dl_info dlinfo;
    dladdr(stackTrace[i], &dlinfo);
    if (dlinfo.dli_sname != NULL) {
      int status;
      fputs("   ", stderr);
      char* d = abi::__cxa_demangle(dlinfo.dli_sname, NULL, NULL, &status);
      if (d == NULL) fputs(dlinfo.dli_sname, stderr);
      else           fputs(d, stderr);
      free(d);

      fprintf(stderr, " + %tu",(char*)stackTrace[i]-(char*)dlinfo.dli_saddr);
    }
    fputc('\n', stderr);
  }
#elif HAVE_CXXABI_H
  if (char ** symbols = backtrace_symbols(stackTrace, depth)) {

    // Name buffer used to contain demangling result.
    size_t sz = 256;
    char * buffer = (char *)malloc(sz);

    for (int i = 0; i < depth; ++i) {
      if (i >= skipFrames) {
        char * symbol = symbols[i];
        // TODO: This is a very cheesy way to extract the symbol name,
        // need to come up with something that will work on various platforms.
        // fprintf(outstream, "%s\n", symbol);
        char * begin = strchr(symbol, '_');
        char * demangled_name = NULL;
        if (begin) {
          char * end = strchr(begin, ' ');
          if (end) {
            *end = 0;
            int status;
            demangled_name = abi::__cxa_demangle(begin, buffer, &sz, &status);
          }
        }

        if (demangled_name != NULL) {
          fprintf(stderr, "    %s\n", demangled_name);

          // Result may be a realloc of input buffer.
          buffer = demangled_name;
        } else if (begin != NULL){
          fprintf(stderr, "    %s\n", begin);
        }
      }
    }

    free(symbols);
    free(buffer);
  }
#else
  backtrace_symbols_fd(stackTrace, depth, STDERR_FILENO);
#endif
#endif
}

template<>
void Diagnostics::DiagnosticAction<Diagnostics::Fatal>::write(
    const SourceLocation & loc, StringRef msg) {
  diag.write(loc, Fatal, msg);
  debugBreak();
}

template<>
void Diagnostics::DiagnosticAction<Diagnostics::Error>::write(
    const SourceLocation & loc, StringRef msg) {
  diag.write(loc, Error, msg);
  debugBreak();
}

template<>
void Diagnostics::DiagnosticAction<Diagnostics::Warning>::write(
    const SourceLocation & loc, StringRef msg) {
  diag.write(loc, Warning, msg);
  debugBreak();
}

template<>
void Diagnostics::DiagnosticAction<Diagnostics::Info>::write(
    const SourceLocation & loc, StringRef msg) {
  diag.write(loc, Info, msg);
}

template<>
void Diagnostics::DiagnosticAction<Diagnostics::Debug>::write(
    const SourceLocation & loc, StringRef msg) {
  diag.write(loc, Debug, msg);
}

void Diagnostics::FailAction::write(const SourceLocation & loc, StringRef msg) {
  diag.write(loc, Debug, msg);
  debugBreak();
  diag.printStackTrace(2);
  abort();
}

void Diagnostics::AssertAction::write(const SourceLocation & loc, StringRef msg) {
  diag.write(loc, Debug, msg);
  debugBreak();
}

void Diagnostics::StdErrWriter::write(const SourceLocation & loc, Severity sev,
    StringRef msg) {
  bool colorChanged = false;
  if (llvm::errs().is_displayed()) {
    if (sev >= Error) {
      llvm::errs().changeColor(llvm::raw_ostream::RED, true);
      colorChanged = true;
    } else if (sev == Warning) {
      llvm::errs().changeColor(llvm::raw_ostream::YELLOW, true);
      colorChanged = true;
    } else if (sev == Info) {
      llvm::errs().changeColor(llvm::raw_ostream::CYAN, true);
      colorChanged = true;
    }
  }
  if (loc.file != NULL && !loc.file->filePath().empty()) {
    // The TextMate error parser is fairly strict
    TokenPosition tokLoc = loc.file->tokenPosition(loc);
    llvm::errs() << loc.file->filePath() << ":" << tokLoc.beginLine << ": ";
  }

  llvm::errs() << severityNames[(int)sev];
  llvm::errs().indent(diag.indentLevel * 2);
  llvm::errs() << msg << "\n";
  llvm::errs().flush();
  if (colorChanged) {
    llvm::errs().resetColor();
  }
}

void Diagnostics::StringWriter::write(const SourceLocation & loc, Severity sev,
    StringRef msg) {
  llvm::raw_svector_ostream strm(str_);
  if (loc.file != NULL && !loc.file->filePath().empty()) {
    // The TextMate error parser is fairly strict
    TokenPosition tokLoc = loc.file->tokenPosition(loc);
    strm << loc.file->filePath() << ":" << tokLoc.beginLine << ": ";
  }

  strm << severityNames[(int)sev];
  strm.indent(diag.indentLevel * 2);
  strm << msg << "\n";
  strm.flush();
}

Diagnostics::FailStream::~FailStream() {
  flush();
  diag.__fail(str(), fname_, lineno_);
}

Diagnostics::AssertionFailureStream::~AssertionFailureStream() {
  if (str().empty()) {
    (*this) << msg_;
  }
  llvm::errs() << fname_ << ":" << lineno_ << ": Assertion failed: " << str() << "\n";
  debugBreak();
  diag.printStackTrace(2);
  abort();
}

}
