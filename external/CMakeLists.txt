add_subdirectory(cnpy)
add_library(fort_test "fort_test/src/fort_test.f90")
set_target_properties(fort_test PROPERTIES Fortran_MODULE_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/modules")
target_include_directories(fort_test PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/modules>)