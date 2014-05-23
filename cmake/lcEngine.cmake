# macro (add_sources)
#     file (RELATIVE_PATH _relPath "${CMAKE_SOURCE_DIR}" "${CMAKE_CURRENT_SOURCE_DIR}")
#     foreach (_src ${ARGN})
#         if (_relPath)
#             list (APPEND SRCS "${_relPath}/${_src}")
#         else()
#             list (APPEND SRCS "${_src}")
#         endif()
#     endforeach()
#     if (_relPath)
#         # propagate SRCS to parent directory
#         set (SRCS ${SRCS} PARENT_SCOPE)
#     endif()
# endmacro()

# generate .lcEnginePrj in top level directory of source tree
set(lcEnginePrj ${CMAKE_SOURCE_DIR}/.lcEnginePrj)
file(REMOVE ${lcEnginePrj})
# file(APPEND ${lcEnginePrj} "\# lcEngine generated file: DO NOT EDIT!\n")

function(lcEngine_output line)
  file(APPEND ${lcEnginePrj} ${line}\n)
endfunction()

function(lcEngine_add_target target)
  lcEngine_output("<target>")
  lcEngine_output("<name>${target}</name>")

  #must be after _add_library
  get_target_property(srcfilelist ${target} SOURCES) # short path

  foreach(srcfile ${srcfilelist})
    lcEngine_output("<source>")
    get_source_file_property(srcpath ${srcfile} LOCATION) # full path
    lcEngine_output("<file>${srcpath}</file>")

    message(STATUS ${CMAKE_CXX_FLAGS})
    get_directory_property(lint_include_directories INCLUDE_DIRECTORIES)
    get_directory_property(lint_defines COMPILE_DEFINITIONS)
    foreach(include_dir ${lint_include_directories})
	message(${include_dir})
    endforeach(include_dir)
    foreach(definition ${lint_defines})
	message(${definition})
    endforeach(definition)

    # get_source_file_property(srcd ${srcfile} COMPILE_DEFINITIONS)
    # message(${srcd})
    # get_source_file_property(srcf ${srcfile} COMPILE_FLAGS)
    # message(${srcf})

    # clang++.exe $(CXX_DEFINES) $(CXX_FLAGS) -o lcEngine.cpp.obj -c lcEngine.cpp
    # CXX_FLAGS =  -fpermissive
    # CXX_DEFINES = -DlcEngine_EXPORTS

    lcEngine_output("<CXX_FLAGS></CXX_FLAGS>")
    lcEngine_output("<CXX_DEFINES></CXX_DEFINES>")
    lcEngine_output("</source>")
  endforeach(srcfile)

  lcEngine_output("</target>")
endfunction()

function (add_library name)
  _add_library (${name} ${ARGN})
  lcEngine_add_target(${name})
endfunction (add_library)

function (add_executable name)
  _add_executable (${name} ${ARGN})
  lcEngine_add_target(${name})
endfunction (add_executable)

#从target获取更多信息参考"Properties on Targets"
#可能用到如INCLUDE_DIRECTORIES COMPILE_FLAGS COMPILE_OPTIONS

#get_source_file_property 从源码得到位置 LOCATION，更多参考："Properties on Source Files"一节，其实cmake文档结构挺好的

#get_directory_property INCLUDE_DIRECTORIES COMPILE_DEFINITIONS

# search keywords: lint, cppcheck, doxygen
# http://www.cmake.org/Wiki/PC-Lint
# 
# function(add_pc_lint target)
#     get_directory_property(lint_include_directories INCLUDE_DIRECTORIES)
#     get_directory_property(lint_defines COMPILE_DEFINITIONS)

#     # let's get those elephants across the alps
#     # prepend each include directory with "-i"; also quotes the directory
#     set(lint_include_directories_transformed)
#     foreach(include_dir ${lint_include_directories})
#         list(APPEND lint_include_directories_transformed -i"${include_dir}")
#     endforeach(include_dir)

#     # prepend each definition with "-d"
#     set(lint_defines_transformed)
#     foreach(definition ${lint_defines})
#         list(APPEND lint_defines_transformed -d${definition})
#     endforeach(definition)
        
#     # list of all commands, one for each given source file
#     set(pc_lint_commands)

#     foreach(sourcefile ${ARGN})
#         # only include c and cpp files
#         if( sourcefile MATCHES \\.c$|\\.cxx$|\\.cpp$ )
#             # make filename absolute
#             get_filename_component(sourcefile_abs ${sourcefile} ABSOLUTE)
#             # create command line for linting one source file and add it to the list of commands
#             list(APPEND pc_lint_commands
#                 COMMAND ${PC_LINT_EXECUTABLE}
#                 -i"${PC_LINT_CONFIG_DIR}" std.lnt
#                 "-u" ${PC_LINT_USER_FLAGS}
#                 ${lint_include_directories_transformed}
#                 ${lint_defines_transformed}
#                 ${sourcefile_abs})
#         endif()
#     endforeach(sourcefile)

#     # add a custom target consisting of all the commands generated above
#     add_custom_target(${target}_LINT ${pc_lint_commands} VERBATIM)
#     # make the ALL_LINT target depend on each and every *_LINT target
#     add_dependencies(ALL_LINT ${target}_LINT)

# endfunction(add_pc_lint)
