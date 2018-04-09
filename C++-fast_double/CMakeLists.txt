cmake_minimum_required(VERSION 3.0.0)
project (benchmark)

option(USE_CLANG "build application with clang" ON)

add_executable(benchmark 
    benchmark.cpp
    fft.cpp
)

add_definitions(
    -DLOG2FFTSIZE=12
    -DFFT_REPEAT=1000

    # improve speed
    -DMOD_SPEED
)

target_include_directories(benchmark PUBLIC 
    "." 
)

install (TARGETS benchmark DESTINATION bin)

set(CPACK_PACKAGE_VERSION 1.0.0)
set(CPACK_GENERATOR "RPM")
set(CPACK_PACKAGE_NAME "fft-benchmark")
set(CPACK_PACKAGE_RELEASE 1)
set(CPACK_PACKAGE_CONTACT "Krüpl Zsolt, HG2ECZ")
set(CPACK_PACKAGE_VENDOR "Krüpl Zsolt, HG2ECZ")
set(CPACK_PACKAGING_INSTALL_PREFIX /opt/fft-benchmark)
set(CPACK_PACKAGE_FILE_NAME "${CPACK_PACKAGE_NAME}-${CPACK_PACKAGE_VERSION}-${CPACK_PACKAGE_RELEASE}.${CMAKE_SYSTEM_PROCESSOR}")
include(CPack)
