// Copyright (C) 2011-2013, lynnux (lynnux.cn@gmail.com)

#include <shlwapi.h>
#include <Shellapi.h>
#include <string.h>
#include <set>
#include <utility>
#include <algorithm>
#include <vector>
#include <fstream>
#include "lcEngine.h"

extern "C" __declspec(dllexport) void stub(void){}

CEngine g_Engine;

typedef
BOOL
(WINAPI*
 CREATEPROCESSA)(
     LPCSTR lpApplicationName,
     LPSTR lpCommandLine,
     LPSECURITY_ATTRIBUTES lpProcessAttributes,
     LPSECURITY_ATTRIBUTES lpThreadAttributes,
     BOOL bInheritHandles,
     DWORD dwCreationFlags,
     LPVOID lpEnvironment,
     LPCSTR lpCurrentDirectory,
     LPSTARTUPINFOA lpStartupInfo,
     LPPROCESS_INFORMATION lpProcessInformation
                 );
extern CREATEPROCESSA CreateProcessA_Org;
// emacs subr function "call-process" will call this API on win32 platform
BOOL WINAPI CEngine::CreateProcessA(  LPCSTR lpApplicationName
                                      ,  LPSTR lpCommandLine
                                      ,  LPSECURITY_ATTRIBUTES lpProcessAttributes
                                      ,  LPSECURITY_ATTRIBUTES lpThreadAttributes
                                      ,  BOOL bInheritHandles
                                      ,  DWORD dwCreationFlags
                                      ,  LPVOID lpEnvironment
                                      ,  LPCSTR lpCurrentDirectory
                                      ,  LPSTARTUPINFOA lpStartupInfo
                                      ,  LPPROCESS_INFORMATION lpProcessInformation )
{
    if (lpApplicationName) // should always be not NULL
    {
        LPSTR lpFileName = PathFindFileNameA(lpApplicationName);
        LPSTR lpFind = StrStrIA(lpFileName, "runemacs.exe"); // I think runemacs.exe must be called merely
        if (lpFind && (lpFind == lpFileName))
        {
            int Length = MultiByteToWideChar(0, 0, lpCommandLine, -1, 0, 0) + 1;
            PWCHAR pCmd = (PWCHAR)malloc(sizeof(WCHAR) * Length);
            if (pCmd)
            {
                LPWSTR *szArglist;
                int nArgs;
                MultiByteToWideChar(0, 0, lpCommandLine, -1, pCmd, Length);
                szArglist = CommandLineToArgvW(pCmd, &nArgs);
                if (szArglist)
                {
                    if (lpStartupInfo && lpStartupInfo->hStdOutput)
                    {
                        CMD_LIST cl;
                        for (int i = 1; i < nArgs; ++i) // szArglist[0] is the process path
                        {
                            cl.push_back(wstr2str(szArglist[i]));
                        }
                        std::string Result = GetCmdResult(cl);
                        if (!Result.empty())
                        {
                            DWORD dwWrite;
                            const size_t BUFFER_SIZE = 0x1000;

                            //size_t count = Result.size() / BUFFER_SIZE;
                            //size_t loop = 0;
                            //do 
                            //{
                            //    WriteFile(lpStartupInfo->hStdOutput
                            //        , Result.c_str() + BUFFER_SIZE * loop
                            //        , (loop == count) ? (Result.size() % BUFFER_SIZE) : BUFFER_SIZE
                            //        , &dwWrite, NULL); 
                            //} while (loop++ < count);

                            // WriteConsoleA won't deliver anything to emacs immediately, we must using WriteFile
                            WriteFile(lpStartupInfo->hStdOutput
                                      , Result.c_str()
                                      // emacs will hang if content size greater than 0x1000
                                      , (Result.size() > BUFFER_SIZE) ? BUFFER_SIZE : Result.size()
                                      , &dwWrite, NULL); 
                        }
                    }
                    LocalFree(szArglist);
                }
                free(pCmd);
            }
            // trick : if we don't create a real process, emacs will create many suspended threads.
            if (!bCreateProcessed_)
            {
                bCreateProcessed_ = TRUE;
                STARTUPINFOA si;
                memset(&si, 0, sizeof(si));
                si.cb = sizeof(si);
                PROCESS_INFORMATION pi;
                memset(&pi, 0, sizeof(pi));
                if(CreateProcessA_Org( NULL, "cmd.exe", NULL, NULL, FALSE, CREATE_NO_WINDOW, NULL, NULL, &si, &pi))
                {
                    hProcess_   = pi.hProcess;
                    hThread_    = pi.hThread;
                    dwProcessId_ = pi.dwProcessId;
                    dwThreadId_  = pi.dwThreadId;
                    TerminateProcess(hProcess_, 0);
                }
                else
                {
                    bCreateProcessed_ = FALSE;
                }
            }
            if (bCreateProcessed_)
            {
                HANDLE hProcessAddRef, hThreadAddRef;
                // duplicate the handle, so the caller of CreateProcessA could call CloseHandle.
                DuplicateHandle(GetCurrentProcess(), hProcess_, GetCurrentProcess(), &hProcessAddRef, 0, FALSE, DUPLICATE_SAME_ACCESS);
                DuplicateHandle(GetCurrentProcess(), hThread_, GetCurrentProcess(), &hThreadAddRef, 0, FALSE, DUPLICATE_SAME_ACCESS);
                lpProcessInformation->hProcess  = hProcessAddRef;
                lpProcessInformation->hThread   = hThreadAddRef;
                lpProcessInformation->dwProcessId   = dwProcessId_;
                lpProcessInformation->dwThreadId    = dwThreadId_;
                return TRUE;
            }
        }
    }
    return FALSE;
}

static std::string completion_printAllCompletionTerms(
    CXCompletionResult* cxc
    )
{
    CXCompletionString completion_string = cxc->CompletionString;// , FILE *fp
    // char temp[10];
    // sprintf(temp, "kind:%d\n", cxc->CursorKind);
    // OutputDebugStringA(temp);
    bool bFiled = cxc->CursorKind == CXCursor_FieldDecl; // 成员变量不加()
    
    std::string ret;
    
    int i_chunk  = 0;
    int n_chunks = clang_getNumCompletionChunks(completion_string);

    CXString chk_text;
    enum CXCompletionChunkKind chk_kind;

    int placeholder_count = 1;
    
    for ( ; i_chunk < n_chunks; i_chunk++)
    {
        /* get the type and completion text of this chunk */
        chk_kind = clang_getCompletionChunkKind(completion_string, i_chunk);
        chk_text = clang_getCompletionChunkText(completion_string, i_chunk);
        
        // OutputDebugStringA(clang_getCString(chk_text));
        
        /* differenct kinds of chunks has various output formats */
        switch (chk_kind)
        {
                
        case CXCompletionChunk_ResultType:
            ret += "$$r:";
            ret += clang_getCString(chk_text);
          break;

        case CXCompletionChunk_TypedText:
            ret += "$$t:";
            ret += clang_getCString(chk_text);
            ret += "$$p:";
            if(!bFiled) ret += "(";
            break;

        case CXCompletionChunk_Placeholder:
        {
            if(bFiled) continue;
            char tmp[10] = {0};
            if(placeholder_count != 1)
            {
                ret += ", ";
            }
            sprintf(tmp, "%d", placeholder_count++);
            ret += "${";
            ret += tmp;
            ret += ":";
            ret += clang_getCString(chk_text);
            ret += "}";
        }
        break;
        
        default:
            break;
        }

        clang_disposeString(chk_text);
    }
    if(!ret.empty())
    {
        if(!bFiled) ret+=")";
        ret += "$0";        
    }
//    OutputDebugStringA(ret.c_str());
    
    return ret;
}

static std::string get_complete_str(CXCompletionString completion_string)
{
    std::string ret;
    int I, N;
    N = clang_getNumCompletionChunks(completion_string);
    for (I = 0; I != N; ++I) 
    {
        CXString text;
        enum CXCompletionChunkKind Kind
                = clang_getCompletionChunkKind(completion_string, I);

        if (Kind != CXCompletionChunk_TypedText) // we only care about this
        {
            continue;
        }
        text = clang_getCompletionChunkText(completion_string, I);
        ret = clang_getCString(text);
        clang_disposeString(text);
        return ret;
    }
    return ret;
}

static void parse_command(const std::string& str, std::vector<std::string>& ret)
{
    ret.clear();
    char buf[512] = {0}; // enough
    memcpy(buf, str.c_str(), str.size());
    char *pch;
    pch = strtok (buf, "|");
    while (pch != NULL)
    {
        ret.push_back(pch);
        pch = strtok (NULL, "|");
    }
}

// in the future, we will establish a command system for calling libclang's useful functions
std::string CEngine::GetCmdResult( const CMD_LIST& cmdList )
{
    std::string retStr = "";
    size_t Length = cmdList.size();
    for (size_t i = 0; i < Length; ++i)
    {
        std::vector<std::string> str_parsed;
        parse_command(cmdList[i], str_parsed);
        if(!str_parsed.empty())
        {
            // code completion, -C|file|line|column|[n|y]
            if ("-C" == str_parsed[0] && str_parsed.size() == 5)
            {
                CProject* np = get_project(1); // now only support one project
                if (np)
                {
                    std::string fileName = str_parsed[1];
                    unsigned line = atoi(str_parsed[2].c_str());
                    unsigned column = atoi(str_parsed[3].c_str());
                    bool bNoUnsaved = (*(str_parsed[4].begin()) == 'n');
                    CXUnsavedFile unsaved;

                    if (!bNoUnsaved && buf_ && len_)
                    {
                        unsaved.Contents = buf_;
                        unsaved.Length = len_;
                        unsaved.Filename = fileName.c_str();
                    }
                    CXTranslationUnit tu = np->getCXTranslationUnit(fileName);
                    if (tu)
                    {
                        // SetCurrentDirectoryA("E:\\src\\mine\\libclang_test"
                        //   /*(fileName.substr(0, fileName.find_last_of('/'))).c_str()*/);
#ifdef _DEBUG
                        DWORD dwTick = GetTickCount(); 
#endif
                        CXCodeCompleteResults* res = clang_codeCompleteAt(tu, fileName.c_str(), line, column, 
                                                                          bNoUnsaved ? 0 : &unsaved, 
                                                                          bNoUnsaved ? 0 : 1, 0/*clang_defaultCodeCompleteOptions()*/);
#ifdef _DEBUG
                        CHAR szDebug[MAX_PATH];
                        wsprintfA(szDebug, "lcEngine : clang_codeCompleteAt spend tick time: %d \r\n", GetTickCount() - dwTick);
                        OutputDebugStringA(szDebug);
#endif
                        if (res)
                        {
                            if (res->NumResults)
                            {
                                clang_sortCodeCompletionResults(res->Results, res->NumResults);

                                // we often get duplicated elements, although emacs can remove, but the processing time is more than c++
                                std::set<std::string> set_result;
                                for (unsigned i = 0; i < res->NumResults; ++i)
                                {
                                    CXCursorKind kind = (res->Results + i)->CursorKind;
                                    if(kind == CXCursor_ClassDecl || kind == CXCursor_Destructor)
                                        continue;
                                    // std::set automaticly ignore duplicated elements
                                    set_result.insert(// get_complete_str((res->Results + i)->CompletionString) + 
                                        completion_printAllCompletionTerms(res->Results + i));
                                }
                            
                                for (std::set<std::string>::iterator ubegin = set_result.begin();
                                     ubegin != set_result.end(); ++ubegin)
                                {
                                    retStr += *ubegin;
                                    retStr += "\n";
                                }
                            
                                if (*(retStr.rbegin()) == '\n')
                                {
                                    retStr.erase(retStr.end() - 1);
                                }
                            }
                            clang_disposeCodeCompleteResults(res);
                        }
                    }
                }
            }
            // -disp|filepath,在保存文件之前需要删除
            if ("-disp" == str_parsed[0] && str_parsed.size() == 2)
            {
                CProject* np = get_project(1);
                if(np)
                    np->disposeTranslationUnit(str_parsed[1]);
            }
            // flymake, -flymake|file|[y|n]
            if ("-flymake" == str_parsed[0] && str_parsed.size() == 3)
            {
                std::string fileName = str_parsed[1];
                bool bsaved = (*(str_parsed[2].begin()) == 'n');
                CProject* np = get_project(1);
                if(np)
                {
                    CXTranslationUnit t = np->getCXTranslationUnit(fileName);
                
                    // if not saved, reparse it
                    if (!bsaved && buf_ && len_)
                    {
                        CXUnsavedFile unsaved;
                        unsaved.Contents = buf_;
                        unsaved.Length = len_;
                        unsaved.Filename = fileName.c_str();
                        clang_reparseTranslationUnit(t, 1, &unsaved, CXTranslationUnit_PrecompiledPreamble);
                    }
                    unsigned int i_diag = 0, n_diag;
                    CXDiagnostic diag;
                    CXString     dmsg;
                    n_diag = clang_getNumDiagnostics(t);
                    for ( ; i_diag < n_diag; i_diag++)
                    {
                        diag = clang_getDiagnostic(t, i_diag);
                        dmsg = clang_formatDiagnostic(diag, clang_defaultDiagnosticDisplayOptions());
                        //fprintf(stdout, "%s\n", clang_getCString(dmsg));
                        retStr += clang_getCString(dmsg);
                        retStr += "\n";
                        clang_disposeString(dmsg);
                        clang_disposeDiagnostic(diag);
                    }
                }
            }
            // goto definition, -def|filename|line|colum|[y|n]
            if("-def" == str_parsed[0] && str_parsed.size() == 5)
            {
                std::string fileName = str_parsed[1];
                unsigned line = atoi(str_parsed[2].c_str());
                unsigned colum = atoi(str_parsed[3].c_str());
                bool bsaved = (*(str_parsed[4].begin()) == 'n');
                CProject* np = get_project(1);
                if(np)
                {
                    CXTranslationUnit tu = np->getCXTranslationUnit(fileName);
                    // if not saved, reparse it
                    if (!bsaved && buf_ && len_)
                    {
                        CXUnsavedFile unsaved;
                        unsaved.Contents = buf_;
                        unsaved.Length = len_;
                        unsaved.Filename = fileName.c_str();
                        clang_reparseTranslationUnit(tu, 1, &unsaved, CXTranslationUnit_PrecompiledPreamble);
                    }

                    // refer to translationunitcache.py::get_definition
                    CXFile cf = clang_getFile(tu, fileName.c_str());
                    if(cf)
                    {
                        CXSourceLocation cs = clang_getLocation(tu, cf, line, colum);
                        CXCursor cc = clang_getCursor(tu, cs);
                        CXCursor ccf = clang_getCursorReferenced(cc); //
                        if(!clang_Cursor_isNull(ccf))
                        {
                            CXSourceLocation csl = clang_getCursorLocation(ccf);
                            CXSourceLocation cslNull = clang_getNullLocation();
                            if(!clang_equalLocations(csl, cslNull))
                            {
                                CXFile cfi;
                                unsigned line;
                                unsigned column;
                                unsigned offset;
                                clang_getExpansionLocation(csl, &cfi, &line, &column, &offset);
                                CXString strFile = clang_getFileName(cfi);
                                char buff[MAX_PATH*2];
                                sprintf(buff, "$$f:%s$$l:%d$$c:%d", clang_getCString(strFile), line, column);
                                retStr = buff;
                                clang_disposeString(strFile);
                            }
                        }
                        // 应该是光标在include的位置，返回的是文件路径，而没有行号和列号
                        // 但是测试无法命中
                        else if(cc.kind == CXCursor_InclusionDirective)
                        {
                            CXFile cfi = clang_getIncludedFile(cc);
                            CXString strFile = clang_getFileName(cfi);
                            retStr = "$$f:";
                            retStr += clang_getCString(strFile);
                            clang_disposeString(strFile);
                        }
                    }
                    
                }
            }
        }
    }
    return retStr;
}

std::string CEngine::wstr2str( const std::wstring& wstr )
{
    int Length = WideCharToMultiByte(0, 0, wstr.c_str(), -1, 0, 0, 0, 0) + 1;
    PCHAR pStr = (PCHAR)malloc(Length);
    std::string strRet = "";
    if (pStr)
    {
        Length = WideCharToMultiByte(0, 0, wstr.c_str(), -1, pStr, Length, 0, 0);
        pStr[Length] = 0; 
        strRet = pStr;
        free(pStr);
    }
    return strRet;
}

bool __cdecl CEngine::send_process( void *proc, char* buf, int len, void* object )
{
    if ((int)proc == proc_)
    {
        // now, only support one file, and we should call clang_codeCompleteAt immediately, otherwise buffer address will change
        // if want to support more files, we should alloc memory buffer and copy
        buf_ = buf; 
        len_ = len;
        return true;
    }
    return false;
}

bool __cdecl CEngine::start_process_before( int nargs, void** args )
{
    // get string from Lisp_Object, reversed from binary bin
    PCHAR pName = (PCHAR)args[0];
    pName = (PCHAR)((DWORD)pName & 0xFFFFFFF8);
    if (!IsBadReadPtr(pName, 0xC + 4))
    {
        pName = *((PCHAR*)(pName + 0xC));
        static const char* Name = "*lcEngine*";
        if (!IsBadStringPtrA(pName, lstrlenA(Name))) // sometimes, it's not a string
        {
            if (0 == _strnicmp(pName, Name, lstrlenA(Name)))
            {
                return true;
            }
        }
    }

    return false;
}

void __cdecl CEngine::start_process_after( int ret )
{
    proc_ = ret;
}

CEngine::~CEngine()
{
    if (!projects_.empty())
    {
        MAP_PROJECT::const_iterator mi = projects_.begin();
        for (; mi != projects_.end(); ++mi)
        {
            delete mi->second;
        }
        projects_.clear();
    }
    if (hProcess_)
        CloseHandle(hProcess_);
    if (hThread_)
        CloseHandle(hThread_);
}

CProject* CEngine::get_project( unsigned int index )
{
    MAP_PROJECT::iterator mi = projects_.find(index); // now only support one project
    if (mi == projects_.end())
    {
        CProject* np = new CProject();
        projects_.insert(std::make_pair(index, np));
    }
    mi = projects_.find(index);
    if (mi != projects_.end())
    {
        return mi->second;
    }
    return NULL;
}

CEngine::CEngine()
        : buf_(0)
        , len_(0)
        , bCreateProcessed_(FALSE)
        , hProcess_(0)
        , hThread_(0)
        , dwProcessId_(0)
        , dwThreadId_(0)
{
}

CXTranslationUnit CProject::getCXTranslationUnit( const std::string& filename, bool update)
{
    CXTranslationUnit ret = NULL;
    MAP_TU::const_iterator mi = tu_list.find(filename);
    if (mi != tu_list.end())
    {
        ret = mi->second;
        if (update)
        {
            //if(0 != clang_reparseTranslationUnit(ret, ))
            //{
            //   clang_disposeTranslationUnit(ret);
            //}
        }
    }
    else
    {
        //std::locale::global(std::locale("")); // it will crash, but work in msvc
        const char ** command_argv = 0;
        size_t command_args = 0;
        std::vector<std::string> vs;

        std::string strLcEngine = filename;
        strLcEngine = strLcEngine.substr(0, strLcEngine.find_last_of('/')); // emacs 传来的/
        strLcEngine.append("/lcEngine.txt");
        std::ifstream read(strLcEngine.c_str());
        if (!read.bad())
        {
            std::string line;
            while(getline(read, line)) // c++ iso 在win平台应该是把\r\n换成\n了的
            {
                if(*line.rbegin() == 0x0D) // gcc only remove 0x0A
                    vs.push_back(line.substr(0, line.size()-1));
                else
                    vs.push_back(line);    // msvc remove both 0x0D and 0x0A
            }

            // libclang补全很快，不需要pch
            //"-D__MSVCRT__", // for msvc

            if (!vs.empty())
            {
                command_argv = new const char*[vs.size()];
                for (size_t i = 0; i < vs.size(); ++i)
                {
                    command_argv[i] = vs[i].c_str();
                    ++command_args;
                }
            }
        }

        // refer to libclang.py
        ret = clang_parseTranslationUnit(CIdx_, filename.c_str(), command_argv, command_args, 0, 0, 
                                         /*CXTranslationUnit_Incomplete*/CXTranslationUnit_PrecompiledPreamble // | CXTranslationUnit_CXXPrecompiledPreamble
                                         );
        //clang_reparseTranslationUnit(ret, 0, 0, clang_defaultReparseOptions(ret));
 
        if (command_argv)
        {
            delete [] command_argv;
        }
        //ret = clang_createTranslationUnit(CIdx_, "pch.pch");
        if (ret)
        {
            tu_list.insert(std::make_pair(filename, ret));
        }
    }
    return ret; 
}

void CProject::disposeTranslationUnit(const std::string& filename)
{
    MAP_TU::iterator mi = tu_list.find(filename);
    if (mi != tu_list.end())
    {
        clang_disposeTranslationUnit(mi->second);
        tu_list.erase(mi);
    }
}

CProject::CProject( int excludeDeclarationsFromPCH /*= 0*/, int displayDiagnostics /*= 0*/ )
{
    CIdx_ = clang_createIndex(excludeDeclarationsFromPCH, displayDiagnostics);
}
