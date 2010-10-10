# CMake build file for Tart/test-unit

# Extra flags for GCC (C++ only)
if (CMAKE_COMPILER_IS_GNUCXX)
  add_definitions(
      -Woverloaded-virtual
      -fno-operator-names -ffor-scope
      )
endif (CMAKE_COMPILER_IS_GNUCXX)

include_directories(${TART_SOURCE_DIR}/third-party/gtest-1.3.0/include)
include_directories(${TART_SOURCE_DIR}/third-party/gmock-1.1.0/include)

# Unit test executable. We specify the sources explicitly because we want
# the tests to run in that order.
add_executable(unittest
  main.cpp
  FakeSourceFile.h
  SmallEnumSetTest.cpp
  LexerTest.cpp
  ParserTest.cpp
  ScopeTest.cpp
  TypeTest.cpp
  PrimitiveTypeTest.cpp
  DefnTest.cpp
  ScopeBuilderTest.cpp
  DeclAnalyzerTest.cpp
  ParameterAssignmentsTest.cpp
  TypeInferenceTest.cpp
  UnionTest.cpp
  UnifyTest.cpp
  NameTableTest.cpp)
target_link_libraries(unittest
    gtest gmock compiler
    ${LLVM_BIT_WRITER}
    ${LLVM_ANALYSIS} 
    ${LLVM_TARGET}
    ${LLVM_MMC}
    ${LLVM_CORE}
    ${LLVM_SUPPORT}
    ${LLVM_SYSTEM}
    dl
    )
set_target_properties(unittest PROPERTIES LINK_FLAGS "${LLVM_LD_FLAGS}")

add_custom_target(unittest.run DEPENDS unittest COMMAND unittest)
add_dependencies(check unittest.run)
