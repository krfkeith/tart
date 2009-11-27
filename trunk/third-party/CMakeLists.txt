# CMake build file for Tart/third-party

# Google test include dirs
include_directories(${TART_SOURCE_DIR}/third-party/gtest-1.3.0/include)
include_directories(${TART_SOURCE_DIR}/third-party/gtest-1.3.0)
include_directories(${TART_SOURCE_DIR}/third-party/gmock-1.1.0/include)
include_directories(${TART_SOURCE_DIR}/third-party/gmock-1.1.0)

# Unit test framework
add_library(gtest STATIC
    ${TART_SOURCE_DIR}/third-party/gtest-1.3.0/src/gtest-all.cc)
if (CMAKE_COMPILER_IS_GNUCXX)
  set_target_properties(gtest PROPERTIES COMPILE_FLAGS "-Wno-extra")
endif (CMAKE_COMPILER_IS_GNUCXX)

# Mock framework
add_library(gmock STATIC
    ${TART_SOURCE_DIR}/third-party/gmock-1.1.0/src/gmock-all.cc)

#add_subdirectory(scarcity)
