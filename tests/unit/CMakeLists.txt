# C++ unit tests for zust_core internals -- doctest input/output pairs that
# don't need the full compile/assemble/link/run pipeline the pytest golden
# suite drives. Opt-in via -DZUST_BUILD_TESTS=ON (see docs/PRD-ZIR.md Wave 1).

include(FetchContent)
FetchContent_Declare(
    doctest
    GIT_REPOSITORY https://github.com/doctest/doctest.git
    GIT_TAG        v2.4.11
)
# doctest 2.4.11's own CMakeLists.txt declares cmake_minimum_required(3.x) with
# a policy floor recent CMake rejects outright; this only relaxes that check
# for doctest's subdirectory, not this project's.
set(CMAKE_POLICY_VERSION_MINIMUM 3.5)
FetchContent_MakeAvailable(doctest)
unset(CMAKE_POLICY_VERSION_MINIMUM)

file(GLOB_RECURSE ZUST_UNIT_TEST_SRCS CONFIGURE_DEPENDS
    ${CMAKE_CURRENT_SOURCE_DIR}/*.cpp)

add_executable(zust_unit_tests ${ZUST_UNIT_TEST_SRCS})
target_link_libraries(zust_unit_tests PRIVATE zust_core doctest::doctest)
target_include_directories(zust_unit_tests PRIVATE ${PROJECT_SOURCE_DIR}/include)

include(${doctest_SOURCE_DIR}/scripts/cmake/doctest.cmake)
doctest_discover_tests(zust_unit_tests)
