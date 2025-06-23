function(add_cpp_test name sources library_dependencies)
    add_executable(${name} ${sources})
    target_link_libraries(${name} PUBLIC ${library_dependencies})
    add_test(
            NAME ${name}
            COMMAND ${name} --gtest_filter=*
    )
endfunction()