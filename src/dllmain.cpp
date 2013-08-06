// Copyright (C) 2011-2013, lynnux (lynnux.cn@gmail.com)

#include <shlwapi.h>
#include "lcEngine.h"
#include "mhook-lib/mhook.h"

typedef BOOL (WINAPI* CREATEPROCESSA)(
     LPCSTR lpApplicationName,
     LPSTR lpCommandLine,
     LPSECURITY_ATTRIBUTES lpProcessAttributes,
     LPSECURITY_ATTRIBUTES lpThreadAttributes,
     BOOL bInheritHandles,
     DWORD dwCreationFlags,
     LPVOID lpEnvironment,
     LPCSTR lpCurrentDirectory,
     LPSTARTUPINFOA lpStartupInfo,
     LPPROCESS_INFORMATION lpProcessInformation);
typedef void (__cdecl* SEND_PROCESS)(void *proc, char* buf, int len, void* object);
typedef void (__attribute__((regparm(3)))* SEND_PROCESS_REGPARM)(void *proc, char* buf, int len, void* object);
typedef int (__cdecl* START_PROCESS)(int nargs, void** args);

CREATEPROCESSA CreateProcessA_Org = CreateProcessA;
static SEND_PROCESS send_process_Org = 0;
static SEND_PROCESS_REGPARM send_process_Org_regparm = 0;
static START_PROCESS start_process_Org = 0;

static BOOL WINAPI CreateProcessA_Hook(
    LPCSTR lpApplicationName,
    LPSTR lpCommandLine,
    LPSECURITY_ATTRIBUTES lpProcessAttributes,
    LPSECURITY_ATTRIBUTES lpThreadAttributes,
    BOOL bInheritHandles,
    DWORD dwCreationFlags,
    LPVOID lpEnvironment,
    LPCSTR lpCurrentDirectory,
    LPSTARTUPINFOA lpStartupInfo,
    LPPROCESS_INFORMATION lpProcessInformation)
{
    if (g_Engine.CreateProcessA(lpApplicationName, lpCommandLine, lpProcessAttributes, lpThreadAttributes
                                , bInheritHandles, dwCreationFlags, lpEnvironment, lpCurrentDirectory, lpStartupInfo, lpProcessInformation))
    {
        return TRUE;
    }
    return CreateProcessA_Org(lpApplicationName, lpCommandLine, lpProcessAttributes, lpThreadAttributes
                              , bInheritHandles, dwCreationFlags, lpEnvironment, lpCurrentDirectory, lpStartupInfo, lpProcessInformation);
}

static void __cdecl send_process_Hook(void *proc, char* buf, int len, void* object)
{
    if (!g_Engine.send_process(proc, buf, len, object)) //
        send_process_Org(proc, buf, len, object);
}

// if vesion of emacs > 24, send_process pass parameters by eax, edx, ecx
// it's gcc regparm attribute: http://blog.csdn.net/unix21/article/details/8450198
static void __attribute__((regparm(3))) send_process_Hook_regparm(void *proc, char* buf, int len, void* object)
{
    if (!g_Engine.send_process(proc, buf, len, object)) //
        send_process_Org_regparm(proc, buf, len, object);
}
static int __cdecl start_process_Hook(int nargs, void** args)
{
    int iRet = 0;
    bool rem = false;
    if(g_Engine.start_process_before(nargs, args))// test if is "*lcEngine*"
        rem = true;
    iRet = start_process_Org(nargs, args);
    if (rem)
    {
        g_Engine.start_process_after(iRet); // remember the "*lcEngine*" proc id
    }
    return iRet;
}

extern "C" // mingw
BOOL APIENTRY DllMain( HMODULE hModule,
                       DWORD  ul_reason_for_call,
                       LPVOID lpReserved
                       )
{
	switch (ul_reason_for_call)
	{
        case DLL_PROCESS_ATTACH:
            // putenv("LC_ALL=C"); // fix "what(): locale::facet::_S_create_c_locale name not valid" // 该句不能随便加，否则拖一个org进来时emacs会崩溃
            TCHAR szIniFile[MAX_PATH];
            GetModuleFileName(NULL, szIniFile, MAX_PATH);
            PathRemoveFileSpec(szIniFile);
            PathAppend(szIniFile, TEXT("lcEngine.ini"));
            send_process_Org = (SEND_PROCESS)::GetPrivateProfileInt(TEXT("subr"), TEXT("send_process"), 0, szIniFile);
            start_process_Org = (START_PROCESS)::GetPrivateProfileInt(TEXT("subr"), TEXT("start_process"), 0, szIniFile);
            if (send_process_Org && start_process_Org)
            {
                // check the emacs version
                TCHAR szEmacs[MAX_PATH];
                GetModuleFileName(NULL, szEmacs, MAX_PATH);
                DWORD hVerHandle;
                DWORD dwVerInfoSize = GetFileVersionInfoSize(szEmacs, &hVerHandle);
                if(!dwVerInfoSize)
                {
                    MessageBox(NULL, TEXT("can't get version info !"), TEXT("error"), MB_OK | MB_ICONERROR);
                    return -1;
                }
                HANDLE hMem; 
                LPVOID lpvMem;  
                unsigned int uInfoSize = 0;  
                VS_FIXEDFILEINFO * pFileInfo;  
                hMem = ::GlobalAlloc(GMEM_MOVEABLE, dwVerInfoSize);  
                lpvMem = ::GlobalLock(hMem);  
                ::GetFileVersionInfo(szEmacs, hVerHandle, dwVerInfoSize, lpvMem);  
                ::VerQueryValue(lpvMem,(LPTSTR)TEXT("\\"), (void**)&pFileInfo, &uInfoSize);
                DWORD dwProductVersionMS = pFileInfo->dwProductVersionMS;
                ::GlobalUnlock(hMem);  
                ::GlobalFree(hMem);  
                if(dwProductVersionMS > 0x00180000) // vesion above 24
                {
                    send_process_Org_regparm = send_process_Org;
                    Mhook_SetHook(&(PVOID&)send_process_Org_regparm, send_process_Hook_regparm);
                }
                else
                {
                    Mhook_SetHook(&(PVOID&)send_process_Org, send_process_Hook);
                }
                Mhook_SetHook(&(PVOID&)start_process_Org, start_process_Hook);
            }
            Mhook_SetHook(&(PVOID&)CreateProcessA_Org, CreateProcessA_Hook);
            break;
        case DLL_THREAD_ATTACH:
            break;
        case DLL_THREAD_DETACH:
            break;
        case DLL_PROCESS_DETACH:
            if (send_process_Org && start_process_Org)
            {
                Mhook_Unhook(&(PVOID&)send_process_Org);
                Mhook_Unhook(&(PVOID&)start_process_Org);
            }
            Mhook_Unhook(&(PVOID&)CreateProcessA_Org);
            break;
	}
	return TRUE;
}

