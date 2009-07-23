# Find LLVM - Find the LLVM includes and library.
#
# This module defines
#  LLVM_INCLUDE_DIR, where to find LLVM headers.
#  LLVM_LIBRARY_DIR, where to find LLVM libraries.
#  LLVM_FOUND, If false, do not try to use LLVM.
#  LLVM_<libname> path to each LLVM static library.
#  LLVM_<target>_O path to each LLVM object module.

# Look for the include dir
if (NOT LLVM_INCLUDE_DIR)
  find_path(LLVM_INCLUDE_DIR llvm/LinkAllVMCore.h DOC "Path to LLVM headers")
else (NOT LLVM_INCLUDE_DIR)
  set(LLVM_INCLUDE_DIR ${LLVM_INCLUDE_DIR} CACHE PATH "Path to LLVM headers")
endif (NOT LLVM_INCLUDE_DIR)

# Look for the library dir
if (NOT LLVM_LIBRARY_DIR)
  find_library(LLVM_CORE LLVMCore)
  get_filename_component(LLVM_LIB_DIR ${LLVM_CORE} PATH)
  set(LLVM_LIBRARY_DIR ${LLVM_LIB_DIR} CACHE PATH "Path to LLVM Libraries")
else (NOT LLVM_LIBRARY_DIR)
  find_library(LLVM_CORE LLVMCore PATHS ${LLVM_LIBRARY_DIR} NO_DEFAULT_PATH)
  set(LLVM_LIBRARY_DIR ${LLVM_LIBRARY_DIR} CACHE PATH "Path to LLVM Libraries")
endif (NOT LLVM_LIBRARY_DIR)

# Find an object file (.o) in the library path.
macro(find_llvm_module Name ObjectFile)
  find_file("LLVM_${Name}" "${ObjectFile}${CMAKE_CXX_OUTPUT_EXTENSION}"
      PATHS ${LLVM_LIBRARY_DIR} NO_DEFAULT_PATH)
  mark_as_advanced("LLVM_${Name}")
endmacro(find_llvm_module)

macro(find_llvm_library Name LibName)
  find_library("LLVM_${Name}_O" ${LibName} PATHS ${LLVM_LIBRARY_DIR} NO_DEFAULT_PATH)
  mark_as_advanced("LLVM_${Name}_O")
endmacro(find_llvm_library)

# Find LLVM libraries
find_llvm_library(IPO LLVMipo)
find_llvm_library(IPA LLVMipa)
find_llvm_library(SCALAR_OPTS LLVMScalarOpts)
find_llvm_library(ANALYSIS LLVMAnalysis)
find_llvm_library(TRANSFORM_UTILS LLVMTransformUtils)
find_llvm_library(CODE_GEN LLVMCodeGen)
find_llvm_library(BIT_READER LLVMBitReader)
find_llvm_library(BIT_WRITER LLVMBitWriter)
find_llvm_library(ASM_PRINTER LLVMAsmPrinter)
find_llvm_library(ASM_PARSER LLVMAsmParser)
find_llvm_library(LINKER LLVMLinker)
find_llvm_library(SELECTION_DAG LLVMSelectionDAG)
find_llvm_library(TARGET LLVMTarget)
find_llvm_library(SYSTEM LLVMSystem)
find_llvm_library(SUPPORT LLVMSupport)
find_llvm_library(ARCHIVE LLVMArchive)
find_llvm_library(DEBUGGER LLVMDebugger)
find_llvm_library(INSTRUMENTATION LLVMInstrumentation)

# Find target modules
find_llvm_module(ARM_ASM_PRINTER LLVMARMAsmPrinter)
find_llvm_module(ARM_CODE_GEN LLVMARMCodeGen)
find_llvm_module(ALPHA_ASM_PRINTER LLVMAlphaAsmPrinter)
find_llvm_module(ALPHA_CODE_GEN LLVMAlphaCodeGen)
find_llvm_module(C_BACKEND LLVMCBackend)
find_llvm_module(CELL_SPU_ASM_PRINTER LLVMCellSPUAsmPrinter)
find_llvm_module(CELL_SPU_CODE_GEN LLVMCellSPUCodeGen)
find_llvm_module(CPP_BACKEND LLVMCppBackend)
find_llvm_module(IA64_ASM_PRINTER LLVMIA64AsmPrinter)
find_llvm_module(IA64_CODE_GEN LLVMIA64CodeGen)
find_llvm_module(MSIL LLVMMSIL)
find_llvm_module(MIPS_ASM_PRINTER LLVMMipsAsmPrinter)
find_llvm_module(MIPS_CODE_GEN LLVMMipsCodeGen)
find_llvm_module(PIC16 LLVMPIC16)
find_llvm_module(POWER_PC_ASM_PRINTER LLVMPowerPCAsmPrinter)
find_llvm_module(POWER_PC_CODE_GEN LLVMPowerPCCodeGen)
find_llvm_module(SPARC_CODE_GEN LLVMSparcCodeGen)
find_llvm_module(SPARC_ASM_PRINTER LLVMSparcAsmPrinter)
find_llvm_module(X86_ASM_PRINTER LLVMX86AsmPrinter)
find_llvm_module(X86_CODE_GEN LLVMX86CodeGen)

# handle the QUIETLY and REQUIRED arguments and set LLVM_FOUND to TRUE if 
# all listed variables are TRUE
include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(LLVM DEFAULT_MSG LLVM_INCLUDE_DIR LLVM_LIBRARY_DIR)
