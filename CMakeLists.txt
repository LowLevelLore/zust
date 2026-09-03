cmake_minimum_required(VERSION 3.16)
project(zpiler LANGUAGES CXX)

# ---------------------------------------------------------------------------
# Options
# ---------------------------------------------------------------------------
option(ZUST_WERROR       "Treat compiler warnings as errors"        ON)
option(ZUST_ENABLE_LTO   "Enable link-time optimization (Release)"  OFF)
option(ZUST_BUILD_TESTS  "Build C++ unit tests"                     OFF)
option(ZUST_ASSERTIONS   "Enable internal assertions/verifiers"     OFF)
set(ZUST_SANITIZE "" CACHE STRING
    "Comma-separated sanitizers, e.g. address,undefined (GCC/Clang only)")

# ---------------------------------------------------------------------------
# Language settings
# ---------------------------------------------------------------------------
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)   # for clangd / clang-tidy

if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
    set(CMAKE_BUILD_TYPE RelWithDebInfo CACHE STRING "Build type" FORCE)
    message(STATUS "CMAKE_BUILD_TYPE not set; defaulting to RelWithDebInfo")
endif()

# ccache, when available, without forcing it on anyone
find_program(CCACHE_PROGRAM ccache)
if(CCACHE_PROGRAM)
    set(CMAKE_CXX_COMPILER_LAUNCHER "${CCACHE_PROGRAM}")
    message(STATUS "Using ccache: ${CCACHE_PROGRAM}")
endif()

# ---------------------------------------------------------------------------
# Warnings and hardening
# ---------------------------------------------------------------------------
add_library(zust_flags INTERFACE)

if(MSVC)
    target_compile_options(zust_flags INTERFACE
        /W4 /permissive- /Zi
        /wd4244   # conversion, possible loss of data
        /wd4456   # declaration hides previous local
        /wd4100   # unreferenced formal parameter
    )
    if(ZUST_WERROR)
        target_compile_options(zust_flags INTERFACE /WX)
    endif()
else()
    target_compile_options(zust_flags INTERFACE
        -g
        -Wall -Wextra -Wpedantic -pedantic
        -Wreorder
        -Wno-unused-variable
        -Wno-unused-parameter
        -fstack-protector-strong
    )
    # -fstack-clash-protection is x86/ARM GCC/Clang only
    include(CheckCXXCompilerFlag)
    check_cxx_compiler_flag(-fstack-clash-protection ZUST_HAS_STACK_CLASH)
    if(ZUST_HAS_STACK_CLASH)
        target_compile_options(zust_flags INTERFACE -fstack-clash-protection)
    endif()

    # _FORTIFY_SOURCE requires optimization; enabling it at -O0 is a noisy no-op.
    target_compile_definitions(zust_flags INTERFACE
        $<$<NOT:$<CONFIG:Debug>>:_FORTIFY_SOURCE=2>
        _GLIBCXX_ASSERTIONS
    )

    if(ZUST_WERROR)
        target_compile_options(zust_flags INTERFACE -Werror)
    endif()

    if(ZUST_SANITIZE)
        message(STATUS "Sanitizers enabled: ${ZUST_SANITIZE}")
        target_compile_options(zust_flags INTERFACE
            -fsanitize=${ZUST_SANITIZE} -fno-omit-frame-pointer)
        target_link_options(zust_flags INTERFACE -fsanitize=${ZUST_SANITIZE})
    endif()
endif()

if(ZUST_ASSERTIONS)
    target_compile_definitions(zust_flags INTERFACE ZUST_ASSERTIONS=1)
endif()

# ---------------------------------------------------------------------------
# zust_core — the compiler as a library, so tests can link it directly
# ---------------------------------------------------------------------------
file(GLOB_RECURSE ZUST_CORE_SRCS CONFIGURE_DEPENDS
    ${PROJECT_SOURCE_DIR}/src/*.cpp)

add_library(zust_core STATIC ${ZUST_CORE_SRCS})
target_include_directories(zust_core PUBLIC ${PROJECT_SOURCE_DIR}/include)
target_link_libraries(zust_core PUBLIC zust_flags)

# ---------------------------------------------------------------------------
# zpiler — the driver
# ---------------------------------------------------------------------------
add_executable(zpiler ${PROJECT_SOURCE_DIR}/main.cpp)
target_link_libraries(zpiler PRIVATE zust_core)
set_target_properties(zpiler PROPERTIES OUTPUT_NAME "zpiler")

# The pytest harness looks for build/zpiler (and build/Debug/zpiler.exe);
# keep the binary at the top of the build tree for multi-config generators too.
set_target_properties(zpiler PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}")

if(ZUST_ENABLE_LTO)
    include(CheckIPOSupported)
    check_ipo_supported(RESULT ZUST_IPO_OK OUTPUT ZUST_IPO_MSG)
    if(ZUST_IPO_OK)
        set_property(TARGET zust_core zpiler
                     PROPERTY INTERPROCEDURAL_OPTIMIZATION_RELEASE TRUE)
    else()
        message(WARNING "LTO requested but unsupported: ${ZUST_IPO_MSG}")
    endif()
endif()

# ---------------------------------------------------------------------------
# Unit tests (opt-in). The golden/end-to-end suite lives in pytest.
# ---------------------------------------------------------------------------
if(ZUST_BUILD_TESTS)
    enable_testing()
    if(EXISTS ${PROJECT_SOURCE_DIR}/tests/unit/CMakeLists.txt)
        add_subdirectory(tests/unit)
    else()
        message(STATUS "ZUST_BUILD_TESTS=ON but tests/unit/ does not exist yet")
    endif()
endif()

install(TARGETS zpiler RUNTIME DESTINATION bin)
