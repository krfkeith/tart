# CMake build file for tart/unittest

# Extra flags for GCC (C++ only)
if (CMAKE_COMPILER_IS_GNUCXX)
  add_definitions(
      -Woverloaded-virtual
      -fno-operator-names -ffor-scope
      )
endif (CMAKE_COMPILER_IS_GNUCXX)

if (CMAKE_COMPILER_IS_CLANG)
  add_definitions(
      -Woverloaded-virtual
      -fno-rtti
      -DGTEST_HAS_RTTI=0
      )
endif (CMAKE_COMPILER_IS_CLANG)

execute_process(
  COMMAND ${LLVM_CONFIG} --libs bitwriter bitreader asmparser ${LLVM_TARGETS}
  OUTPUT_VARIABLE LLVM_TESTRUNNER_LIBS
  OUTPUT_STRIP_TRAILING_WHITESPACE
)

include_directories(${TART_SOURCE_DIR}/third-party/gmock-1.6.0/include)
include_directories(${TART_SOURCE_DIR}/third-party/gmock-1.6.0/gtest/include)

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
  TypeConversionTest.cpp
  DefnTest.cpp
  ScopeBuilderTest.cpp
  DeclAnalyzerTest.cpp
  ParameterAssignmentsTest.cpp
  TypeInferenceTest.cpp
  UnionTest.cpp
  UnifyTest.cpp
  NameTableTest.cpp
  ASTSerializationTest.cpp
  ConstraintTest.cpp
  BindingEnvTest.cpp
  )
target_link_libraries(unittest
    gtest gmock compiler
    ${LLVM_TESTRUNNER_LIBS}
    )
if (LIB_DL)
  target_link_libraries(unittest dl)
endif (LIB_DL)
set_target_properties(unittest PROPERTIES LINK_FLAGS "${LLVM_LD_FLAGS}")

add_custom_target(unittest.run DEPENDS unittest COMMAND unittest)
add_dependencies(check unittest.run)
