// dllmain.cpp : 定义 DLL 应用程序的入口点。
#include "stdafx.h"
#include "detours/detours.h"
#pragma comment(lib, "detours\\detours_x86.lib")
#include <shlwapi.h>
#pragma comment(lib, "shlwapi.lib")
#include "Engine.h"

typedef
BOOL
(WINAPI*
CREATEPROCESSA)(
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

typedef void (__cdecl* SEND_PROCESS)(void *proc, char* buf, int len, void* object);
typedef int (__cdecl* START_PROCESS)(int nargs, void** args);

CREATEPROCESSA CreateProcessA_Org = CreateProcessA;
SEND_PROCESS send_process_Org = 0;
START_PROCESS start_process_Org = 0;

BOOL
WINAPI
CreateProcessA_Hook(
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
 )
{
    if (g_Engine.CreateProcessA(lpApplicationName, lpCommandLine, lpProcessAttributes, lpThreadAttributes
        , bInheritHandles, dwCreationFlags, lpEnvironment, lpCurrentDirectory, lpStartupInfo, lpProcessInformation))
    {
        return TRUE;
    }
    return CreateProcessA_Org(lpApplicationName, lpCommandLine, lpProcessAttributes, lpThreadAttributes
        , bInheritHandles, dwCreationFlags, lpEnvironment, lpCurrentDirectory, lpStartupInfo, lpProcessInformation);
}

void __cdecl send_process_Hook(void *proc, char* buf, int len, void* object)
{
    //g_Engine.send_process(proc, buf, len, object);
    if (!g_Engine.send_process(proc, buf, len, object)) //
        send_process_Org(proc, buf, len, object);
}

int __cdecl start_process_Hook(int nargs, void** args)
{
    int iRet = 0;
    bool rem = false;
    if(g_Engine.start_process_before(nargs, args))
        rem = true;
    iRet = start_process_Org(nargs, args);
    if (rem)
    {
        g_Engine.start_process_after(iRet);
    }
    return iRet;
}
BOOL APIENTRY DllMain( HMODULE hModule,
                       DWORD  ul_reason_for_call,
                       LPVOID lpReserved
					 )
{
	switch (ul_reason_for_call)
	{
	case DLL_PROCESS_ATTACH:
        DetourTransactionBegin();
        DetourUpdateThread(GetCurrentThread());
        TCHAR szIniFile[MAX_PATH];
        GetModuleFileName(NULL, szIniFile, MAX_PATH);
        PathRemoveFileSpec(szIniFile);
        PathAppend(szIniFile, TEXT("lcEngine.ini"));
        send_process_Org = (SEND_PROCESS)::GetPrivateProfileInt(TEXT("subr"), TEXT("send_process"), 0, szIniFile);
        start_process_Org = (START_PROCESS)::GetPrivateProfileInt(TEXT("subr"), TEXT("start_process"), 0, szIniFile);
        if (send_process_Org && start_process_Org)
        {
            DetourAttach(&(PVOID&)send_process_Org, send_process_Hook);
            DetourAttach(&(PVOID&)start_process_Org, start_process_Hook);
        }
        DetourAttach(&(PVOID&)CreateProcessA_Org, CreateProcessA_Hook);
        DetourTransactionCommit();
        break;
	case DLL_THREAD_ATTACH:
        break;
	case DLL_THREAD_DETACH:
        break;
	case DLL_PROCESS_DETACH:
        DetourTransactionBegin();
        DetourUpdateThread(GetCurrentThread());
        if (send_process_Org && start_process_Org)
        {
            DetourDetach(&(PVOID&)send_process_Org, send_process_Hook);
            DetourDetach(&(PVOID&)start_process_Org, start_process_Hook);
        }
        DetourDetach(&(PVOID&)CreateProcessA_Org, CreateProcessA_Hook);
        DetourTransactionCommit();
		break;
	}
	return TRUE;
}

