# CMake build file for Tart/linker

# Extra flags for GCC (C++ only)
if (CMAKE_COMPILER_IS_GNUCXX)
  add_definitions(
      -Woverloaded-virtual
      -fno-operator-names -ffor-scope
      -fPIC
      )
   if ($PROFILE_TARTLN)
     add_definitions(-pg)
     set(CMAKE_EXE_LINKER_FLAGS -pg)
   endif ($PROFILE_TARTLN)
endif (CMAKE_COMPILER_IS_GNUCXX)

file(GLOB COMMON_SOURCES lib/Common/*.cpp)
file(GLOB COMMON_HEADERS include/Common/*.h)

file(GLOB GC_SOURCES lib/GC/*.cpp)
file(GLOB GC_HEADERS include/GC/*.h)

file(GLOB REFLECT_SOURCES lib/Reflect/*.cpp)
file(GLOB REFLECT_HEADERS include/Reflect/*.h)

source_group(GC REGULAR_EXPRESSION lib/GC/*)

add_library(linker_common STATIC
  ${COMMON_SOURCES} ${COMMON_HEADERS})
add_library(gcstrategy STATIC
  ${GC_SOURCES} ${GC_HEADERS})
add_library(linker_reflect STATIC
  ${REFLECT_SOURCES} ${COMMON_HEADERS} ${REFLECT_HEADERS})

add_library(gc SHARED ${GC_SOURCES} ${GC_HEADERS})
target_link_libraries(gc
    ${LLVM_ANALYSIS}
    ${LLVM_TARGET}
    ${LLVM_CODE_GEN}
    ${LLVM_ASM_PRINTER}
    ${LLVM_MC}
    ${LLVM_MC_PARSER}
    ${LLVM_CORE}
    ${LLVM_SUPPORT}
    ${LLVM_SYSTEM}
    )
add_library(reflector SHARED ${REFLECT_SOURCES} ${COMMON_HEADERS} ${REFLECT_HEADERS})
target_link_libraries(reflector
    linker_common
    ${LLVM_TARGET}
    ${LLVM_CORE}
    ${LLVM_SUPPORT}
    ${LLVM_SYSTEM}
    )
