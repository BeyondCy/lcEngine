// date   : 2011/09/07
// author : lynnux.cn@gmail.com
#ifndef ENGINE_H_9_7_14_04
#define ENGINE_H_9_7_14_04

#if defined(_MSC_VER) && (_MSC_VER >= 1020)
#pragma once
#endif

#include <windows.h>
#include <map>
#include <string>
#include <vector>
#include "Index.h"

class CProject;

class CEngine
{
public:
    CEngine();
    ~CEngine();

    // hook CreateProcessA, when emacs call "call-procss", we will catch it, and handle it by process parameter.
    // finally we send string of result by lpStartupInfo->hStdOutput, which emacs can recognize
    BOOL WINAPI CreateProcessA(
        __in_opt    LPCSTR lpApplicationName,
        __inout_opt LPSTR lpCommandLine,
        __in_opt    LPSECURITY_ATTRIBUTES lpProcessAttributes,
        __in_opt    LPSECURITY_ATTRIBUTES lpThreadAttributes,
        __in        BOOL bInheritHandles,
        __in        DWORD dwCreationFlags,
        __in_opt    LPVOID lpEnvironment,
        __in_opt    LPCSTR lpCurrentDirectory,
        __in        LPSTARTUPINFOA lpStartupInfo,
        __out       LPPROCESS_INFORMATION lpProcessInformation
        );

    // return true if handled, only when proc equal to proc_
    // log the buf and len, as the parameters 'CXUnsavedFile' for clang_codeCompleteAt if necessary
    bool __cdecl send_process (void *proc, char* buf, int len, void* object);

    // return true if we care about the result of start_process, only when buffer name equal to"*lcEngine*"
    bool __cdecl start_process_before(int nargs, void** args);

    // log the Lisp_Object which represents the process
    void __cdecl start_process_after(int ret);

protected:

    // find the project by index, if there doesn't exist, create one and return
    CProject* get_project(unsigned int index);
private:

    typedef std::vector<std::string> CMD_LIST;
    std::string GetCmdResult(const CMD_LIST& cmdList);
	
	static std::string wstr2str(const std::wstring& wstr);

    int proc_; // start-process, send_process
    // buffer memory address in emacs, no copy, we should call clang_codeCompleteAt immediately, otherwise it will change
    const char* buf_; 
    int len_;

    typedef std::map<unsigned int, CProject*> MAP_PROJECT;
    MAP_PROJECT projects_;

    BOOL   bCreateProcessed_;
    HANDLE hProcess_;
    HANDLE hThread_;
    DWORD  dwProcessId_;
    DWORD  dwThreadId_;
};

// class CXTranslationUnit
// class CFileBuffer

// some refered to llvm\tools\clang\bindings\python\clang\cindex.py
class CProject
{
public:
    CProject(int excludeDeclarationsFromPCH = 0, int displayDiagnostics = 0);
    CXTranslationUnit getCXTranslationUnit(const std::string& filename, bool update = false);
protected:
private:
    CXIndex CIdx_; // one project, one CXIndex
    typedef std::map<std::string, CXTranslationUnit> MAP_TU; // one file, one CXTranslationUnit
    MAP_TU tu_list;
};

extern CEngine g_Engine;

#endif // ENGINE_H_9_7_14_04