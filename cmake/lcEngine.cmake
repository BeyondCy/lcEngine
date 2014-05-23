# Copyright (C) 2011-2014, lynnux (lynnux.cn@gmail.com)
# generated date : 2014Äê5ÔÂ23ÈÕ
# Time-stamp: <2014-05-23 15:16:05 lynnux>

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

  get_target_property(srcfilelist ${target} SOURCES) # short path

  foreach(srcfile ${srcfilelist})
    get_source_file_property(srcLang ${srcfile} LANGUAGE)
    get_source_file_property(srcIsObj ${srcfile} EXTERNAL_OBJECT)

    # check source
    if((NOT srcIsObj) AND (srcLang STREQUAL "C" OR srcLang STREQUAL "CXX"))
      lcEngine_output("<source>")

      # output file path
      get_source_file_property(srcpath ${srcfile} LOCATION) # full path
      lcEngine_output("<file>${srcpath}</file>")

      # message(STATUS ${CMAKE_CXX_FLAGS})
      
      get_directory_property(src_include_directories INCLUDE_DIRECTORIES)
      foreach(include_dir ${src_include_directories})
	lcEngine_output("<include>${include_dir}</include>")
      endforeach(include_dir)

      get_directory_property(src_defines COMPILE_DEFINITIONS)
      foreach(definition ${src_defines})
	lcEngine_output("<definition>${definition}</definition>")
      endforeach(definition)

      # always null?
      # get_source_file_property(srcd ${srcfile} COMPILE_DEFINITIONS)
      # message(${srcd})
      # get_source_file_property(srcf ${srcfile} COMPILE_FLAGS)
      # message(${srcf})

      # clang++.exe $(CXX_DEFINES) $(CXX_FLAGS) -o lcEngine.cpp.obj -c lcEngine.cpp
      # CXX_FLAGS =  -fpermissive
      # CXX_DEFINES = -DlcEngine_EXPORTS

      # lcEngine_output("<CXX_FLAGS></CXX_FLAGS>")
      # lcEngine_output("<CXX_DEFINES></CXX_DEFINES>")
      lcEngine_output("</source>")
    endif()

  endforeach(srcfile)

  lcEngine_output("</target>")
endfunction()

# hook add_library
function (add_library name)
  _add_library (${name} ${ARGN})
  lcEngine_add_target(${name})
endfunction (add_library)

# hook add_executable
function (add_executable name)
  _add_executable (${name} ${ARGN})
  lcEngine_add_target(${name})
endfunction (add_executable)
