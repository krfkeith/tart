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

# Find an LLVM libraries
macro(find_llvm_library Name LibName)
  find_library(LLVM_${Name} ${LibName} PATHS ${LLVM_LIBRARY_DIR} NO_DEFAULT_PATH)
  mark_as_advanced(LLVM_${Name})
endmacro(find_llvm_library)

# LLVM Libraries
find_llvm_library(ANALYSIS LLVMAnalysis)
find_llvm_library(ARCHIVE LLVMArchive)
find_llvm_library(ASM_PARSER LLVMAsmParser)
find_llvm_library(ASM_PRINTER LLVMAsmPrinter)
find_llvm_library(BIT_READER LLVMBitReader)
find_llvm_library(BIT_WRITER LLVMBitWriter)
find_llvm_library(CODE_GEN LLVMCodeGen)
find_llvm_library(CORE LLVMCore)
find_llvm_library(DEBUGGER LLVMDebugger)
find_llvm_library(EXECUTION_ENGINE LLVMExecutionEngine)
find_llvm_library(HELLO LLVMHello)
find_llvm_library(INSTRUMENTATION LLVMInstrumentation)
find_llvm_library(INTERPRETER LLVMInterpreter)
find_llvm_library(IPA LLVMipa)
find_llvm_library(IPO LLVMipo)
find_llvm_library(JIT LLVMJIT)
find_llvm_library(LINKER LLVMLinker)
find_llvm_library(MC LLVMMC)
find_llvm_library(SCALAR_OPTS LLVMScalarOpts)
find_llvm_library(SELECTION_DAG LLVMSelectionDAG)
find_llvm_library(SUPPORT LLVMSupport)
find_llvm_library(SYSTEM LLVMSystem)
find_llvm_library(TARGET LLVMTarget)
find_llvm_library(TRANSFORM_UTILS LLVMTransformUtils)

# Architecture: ARM
find_llvm_library(ARM_ASM_PRINTER LLVMARMAsmPrinter)
find_llvm_library(ARM_CODE_GEN LLVMARMCodeGen)
find_llvm_library(ARM_INFO LLVMARMInfo)

# Architecture: Alpha
find_llvm_library(ALPHA_ASM_PRINTER LLVMAlphaAsmPrinter)
find_llvm_library(ALPHA_CODE_GEN LLVMAlphaCodeGen)
find_llvm_library(ALPHA_INFO LLVMAlphaInfo)

# Architecture: Blackfin
find_llvm_library(BLACKFIN_ASM_PRINTER LLVMBlackfinAsmPrinter)
find_llvm_library(BLACKFIN_CODE_GEN LLVMBlackfinCodeGen)
find_llvm_library(BLACKFIN_INFO LLVMBlackfinInfo)

# Architecture: CBackend
find_llvm_library(CBACKEND LLVMCBackend)
find_llvm_library(CBACKEND_INFO LLVMCBackendInfo)

# Architecture: CellSPU
find_llvm_library(CELLSPU_ASM_PRINTER LLVMCellSPUAsmPrinter)
find_llvm_library(CELLSPU_CODE_GEN LLVMCellSPUCodeGen)
find_llvm_library(CELLSPU_INFO LLVMCellSPUInfo)

# Architecture: CppBackend
find_llvm_library(CPPBACKEND LLVMCppBackend)
find_llvm_library(CPPBACKEND_INFO LLVMCppBackendInfo)

# Architecture: MSIL
find_llvm_library(MSIL LLVMMSIL)
find_llvm_library(MSIL_INFO LLVMMSILInfo)

# Architecture: MSP430
find_llvm_library(MSP430_ASM_PRINTER LLVMMSP430AsmPrinter)
find_llvm_library(MSP430_CODE_GEN LLVMMSP430CodeGen)
find_llvm_library(MSP430_INFO LLVMMSP430Info)

# Architecture: Mips
find_llvm_library(MIPS_ASM_PRINTER LLVMMipsAsmPrinter)
find_llvm_library(MIPS_CODE_GEN LLVMMipsCodeGen)
find_llvm_library(MIPS_INFO LLVMMipsInfo)

# Architecture: PIC16
find_llvm_library(PIC16 LLVMPIC16)
find_llvm_library(PIC16_ASM_PRINTER LLVMPIC16AsmPrinter)
find_llvm_library(PIC16_INFO LLVMPIC16Info)

# Architecture: PowerPC
find_llvm_library(POWERPC_ASM_PRINTER LLVMPowerPCAsmPrinter)
find_llvm_library(POWERPC_CODE_GEN LLVMPowerPCCodeGen)
find_llvm_library(POWERPC_INFO LLVMPowerPCInfo)

# Architecture: Sparc
find_llvm_library(SPARC_ASM_PRINTER LLVMSparcAsmPrinter)
find_llvm_library(SPARC_CODE_GEN LLVMSparcCodeGen)
find_llvm_library(SPARC_INFO LLVMSparcInfo)

# Architecture: SystemZ
find_llvm_library(SYSTEMZ_ASM_PRINTER LLVMSystemZAsmPrinter)
find_llvm_library(SYSTEMZ_CODE_GEN LLVMSystemZCodeGen)
find_llvm_library(SYSTEMZ_INFO LLVMSystemZInfo)

# Architecture: X86
find_llvm_library(X86_ASM_PARSER LLVMX86AsmParser)
find_llvm_library(X86_ASM_PRINTER LLVMX86AsmPrinter)
find_llvm_library(X86_CODE_GEN LLVMX86CodeGen)
find_llvm_library(X86_INFO LLVMX86Info)

# Architecture: XCore
find_llvm_library(XCORE LLVMXCore)
find_llvm_library(XCORE_ASM_PRINTER LLVMXCoreAsmPrinter)
find_llvm_library(XCORE_INFO LLVMXCoreInfo)

# handle the QUIETLY and REQUIRED arguments and set LLVM_FOUND to TRUE if 
# all listed variables are TRUE
include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(LLVM DEFAULT_MSG LLVM_INCLUDE_DIR LLVM_LIBRARY_DIR)
