project(Z80_test)

add_executable(${PROJECT_NAME} driver.cpp
        ../include/z80.h)
target_link_libraries(${PROJECT_NAME})
target_include_directories(${PROJECT_NAME} PRIVATE ${CMAKE_SOURCE_DIR}/include)