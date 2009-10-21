# CMake build file for Tart

cmake_minimum_required(VERSION 2.6) 
project(TART)

set(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake_modules/")

# Macros we'll need
include(CheckIncludeFile)
include(CheckIncludeFileCXX)
include(CheckFunctionExists)

# Check for the existence of include files.
check_include_file(stdio.h HAVE_STDIO_H)
check_include_file(stdint.h HAVE_STDINT_H)
check_include_file(stddef.h HAVE_STDDEF_H)
check_include_file(stdlib.h HAVE_STDLIB_H)
check_include_file(string.h HAVE_STRING_H)
check_include_file(unistd.h HAVE_UNISTD_H)
check_include_file(errno.h HAVE_ERRNO_H)
check_include_file(execinfo.h HAVE_EXECINFO_H)
check_include_file(sys/time.h HAVE_SYS_TIME_H)
check_include_file(sys/types.h HAVE_SYS_TYPES_H)
check_include_file(sys/resource.h HAVE_SYS_RESOURCE_H)
check_include_file(libkern/OSAtomic.h HAVE_LIBKERN_OSATOMIC_H)
check_include_file_cxx(new HAVE_NEW)
check_include_file_cxx(ctime HAVE_CTIME)
check_include_file_cxx(cxxabi.h HAVE_CXXABI_H)
check_include_file_cxx(unwind.h HAVE_UNWIND_H)

# Check for library functions we need.
check_function_exists(backtrace HAVE_BACKTRACE)

# Check for LLVM
find_package(LLVM REQUIRED)

# Check for dsymutil
find_program(DSYMUTIL dsymutil)

# Generate the config.h file
configure_file(
    ${CMAKE_CURRENT_SOURCE_DIR}/config.h.in
    ${CMAKE_CURRENT_BINARY_DIR}/config.h)

# Generate the config.h file
configure_file(
    ${CMAKE_CURRENT_SOURCE_DIR}/runtime/include/runtime_config.h.in
    ${CMAKE_CURRENT_BINARY_DIR}/runtime/include/runtime_config.h)

# Include directory paths
include_directories(${CMAKE_CURRENT_BINARY_DIR})
include_directories(${TART_SOURCE_DIR}/collector/include)
include_directories(${TART_SOURCE_DIR}/compiler/include)
include_directories(${LLVM_INCLUDE_DIR})

# Definitions needed by LLVM headers
add_definitions(
    -D__STDC_LIMIT_MACROS
    -D__STDC_CONSTANT_MACROS
    -D_GNU_SOURCE)

# Extra flags for GCC (C++ only)  
if (CMAKE_COMPILER_IS_GNUCXX)
  set(GCC_CXX_FLAGS
      -pipe
      -Wall -Wextra -Werror -Wcast-align -Wpointer-arith
      -Wno-deprecated -Wno-unused
      -Woverloaded-virtual
      -fno-operator-names -ffor-scope
      -fconst-strings
      -fmessage-length=0)
endif (CMAKE_COMPILER_IS_GNUCXX)

# Add the check-build target
add_custom_target(check)

# Subdirs to compile
add_subdirectory(collector)
add_subdirectory(compiler)
add_subdirectory(tools)
add_subdirectory(runtime)
add_subdirectory(third-party)
add_subdirectory(test/unit)
add_subdirectory(stdlib)
add_subdirectory(test/nolib)
add_subdirectory(test/stdlib)
add_subdirectory(test/fail)

#enable_testing()
#add_test(UnitTest ${CMAKE_CURRENT_BINARY_DIR}/test-unit/unittest)
