// Copyright (C) 2011-2013, lynnux (lynnux.cn@gmail.com)

#include <windows.h>
#include <stdlib.h>
#include <malloc.h>
#include <memory.h>
#include <tchar.h>
#include <shlwapi.h>
#include "disasm-lib/disasm.h"

void kmp_init(const char *patn, int len, int *next)
{
    int i, j;

    /*	assert(patn != NULL && len > 0 && next != NULL);
    */
    next[0] = 0;
    for (i = 1, j = 0; i < len; i++) {
        while (j > 0 && patn[j] != patn[i])
            j = next[j - 1];
        if (patn[j] == patn[i])
            j ++;
        next[i] = j;
    }
}

int kmp_find(const char *text, int text_len, const char *patn,
             int patn_len, int *next)
{
    int i, j;

    /*	assert(text != NULL && text_len > 0 && patn != NULL && patn_len > 0
    && next != NULL);
    */
    for (i = 0, j = 0; i < text_len; i ++ ) {
        while (j > 0 && text[i] != patn[j])
            j = next[j - 1];
        if (text[i] == patn[j])
            j ++;
        if (j == patn_len)
            return (i + 1 - patn_len);
    }

    return -1; // 
}

class CSection
{
public:
    CSection(const char* bBegin): m_Begin(bBegin){
        pinh = (PIMAGE_NT_HEADERS32)(m_Begin + ((PIMAGE_DOS_HEADER)m_Begin)->e_lfanew);
        pifh = &pinh->FileHeader;
        pish = (PIMAGE_SECTION_HEADER)((PBYTE)pinh + sizeof (IMAGE_NT_HEADERS32));
    }
    virtual ~CSection(){}
    virtual bool RegionAndSize(/*in*/const char* SecName, /*out*/const char** begin, /*out*/size_t* size) const
    {
        for (size_t i = 0; i < pifh->NumberOfSections; ++i)
        {
            if (_strnicmp((const char*)pish[i].Name, SecName, 8) == 0)
            {
                *begin = m_Begin + pish[i].PointerToRawData;
                *size  = pish[i].SizeOfRawData;
                return true;
            }
        }
        return false;
    }
    DWORD VirtualAddress(const char* SecName) const
    {
        for (size_t i = 0; i < pifh->NumberOfSections; ++i)
        {
            if (_strnicmp((const char*)pish[i].Name, SecName, 8) == 0)
            {
                return pish[i].VirtualAddress;
            }
        }
        return 0;
    }
    DWORD ImageBase() const
    {
        return pinh->OptionalHeader.ImageBase;
    }
protected:
private:
    const char* m_Begin;
    PIMAGE_NT_HEADERS32 pinh;
    PIMAGE_FILE_HEADER pifh;
    PIMAGE_SECTION_HEADER pish;
};

class CFindBase
{
public:
    CFindBase() : next(0){};
    virtual ~CFindBase(){delete next;}
protected:
    void Init(const char* search, size_t size)
    {
        next = new int[size];
        kmp_init(search, size, next);
    }
    int Find(const char* region_start, size_t region_size, const char* search, size_t size) const{
        return kmp_find(region_start, region_size, search, size, next);
    }
    int* next;
};
class CSubrStrFind : public CFindBase
{
public:
    CSubrStrFind(const char* search)
    {
        size_t len = lstrlenA(search);
        size_ = len + 2; // two blank char
        search_ = new char[size_];
        memset(search_, 0, size_);
        memcpy(search_ + 1, search, len);

        Init(search_, size_);
    }
    virtual ~CSubrStrFind(){
        delete search_;
    }
    int Find(const char* region_start, size_t region_size)
    {
        return CFindBase::Find(region_start, region_size, search_, size_);
    }

private:
    char* search_;
    size_t size_;
};

class CSubrAddrFind : public CFindBase
{
public:
    CSubrAddrFind(DWORD Address) : Address_(Address){
        Init((const char*)&Address_, sizeof(Address_));
    }
    int Find(const char* region_start, size_t region_size)
    {
        return CFindBase::Find(region_start, region_size, (const char*)&Address_, sizeof(Address_));
    }

private:
    DWORD Address_;
};

// DEFUN ("process-send-region", Fprocess_send_region, 3, 4, 0,(process, start, end, buffer))
// get the Fprocess_send_region
class Cprocess_send_region
{
public:
    Cprocess_send_region(const CSection& Sec1, DWORD dwEmacsVer) : Sec(Sec1), dwEmacsVer_(dwEmacsVer){}
    /*
        search subr string from .rdata section, and code in .data section
    */
    DWORD GetAddr()
    {
        const char* pSearchBegin = 0;
        size_t size = 0;
        if(Sec.RegionAndSize(".rdata", &pSearchBegin, &size))
        {
            CSubrStrFind process_send_region("process-send-region");
            int iFind = process_send_region.Find(pSearchBegin, size);
            if (iFind != -1)
            {
                iFind += 1;
                DWORD targAddr = Sec.ImageBase() + iFind + Sec.VirtualAddress(".rdata");

                if(Sec.RegionAndSize(".data", &pSearchBegin, &size))
                {
                    CSubrAddrFind Adr(targAddr);
                    iFind = Adr.Find(pSearchBegin, size);
                    if (iFind != -1)
                    {
                        if (dwEmacsVer_ >= 0x00170002)  // start-process不需要第二次！
                        {
                            pSearchBegin = pSearchBegin + iFind + 4;
                            iFind = Adr.Find(pSearchBegin, size - iFind - 4);
                        }
                    }

                    if (iFind != -1)
                    {
                        // get the "process-send-region" function address
                        return *((DWORD*)(pSearchBegin + iFind - 8));
                    }
                }
            }
        }
        return 0;
    }
private:
    const CSection& Sec;
    DWORD dwEmacsVer_;
};

class Cstart_process
{
public:
    Cstart_process(const CSection& Sec1) : Sec(Sec1){}
    DWORD GetAddr()
    {
        const char* pSearchBegin = 0;
        size_t size = 0;
        if(Sec.RegionAndSize(".rdata", &pSearchBegin, &size))
        {
            CSubrStrFind process_send_region("start-process");
            int iFind = process_send_region.Find(pSearchBegin, size);
            if (iFind != -1)
            {
                iFind += 1;
                DWORD targAddr = Sec.ImageBase() + iFind + Sec.VirtualAddress(".rdata");

                if(Sec.RegionAndSize(".data", &pSearchBegin, &size))
                {
                    CSubrAddrFind Adr(targAddr);
                    iFind = Adr.Find(pSearchBegin, size);

                    // start-process only search once
                    if (iFind != -1)
                    {
                        return *((DWORD*)(pSearchBegin + iFind - 8));
                    }
                }
            }
        }
        return 0;
    }
private:
    const CSection& Sec;
};

// make new dll import of lcEngine.dll
BOOL AddNewImprortDll(LPCTSTR exe/*, LPCSTR Dll, LPCSTR Function*/)
{
    // make bakeup
    TCHAR szBakFile[MAX_PATH];
    lstrcpy(szBakFile, exe);
    LPTSTR pExtension = PathFindExtension(szBakFile);
    lstrcpy(pExtension, TEXT(".bak.exe"));
    CopyFile(exe, szBakFile, TRUE); // will fail if exists

    BOOL bRet = FALSE;
    HANDLE hFile = CreateFile(exe, GENERIC_READ | GENERIC_WRITE, FILE_SHARE_READ | FILE_SHARE_WRITE, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
    if (hFile != INVALID_HANDLE_VALUE)
    {    
        HANDLE hMap = CreateFileMapping(hFile, NULL, PAGE_READWRITE, 0, 0, NULL);
        if (hMap)
        {
            const unsigned char* pBegin = (const unsigned char*)MapViewOfFile(hMap, FILE_MAP_READ | FILE_MAP_WRITE, 0, 0, 0);
            if (pBegin)
            {            
                PIMAGE_NT_HEADERS32 pinh = (PIMAGE_NT_HEADERS32)(pBegin + ((PIMAGE_DOS_HEADER)pBegin)->e_lfanew);
                PIMAGE_OPTIONAL_HEADER32 pifh = &pinh->OptionalHeader;
                PIMAGE_DATA_DIRECTORY pImportDir = &pifh->DataDirectory[IMAGE_DIRECTORY_ENTRY_IMPORT];
                PIMAGE_IMPORT_DESCRIPTOR pStart = (PIMAGE_IMPORT_DESCRIPTOR)(pImportDir->VirtualAddress + pBegin);
                PIMAGE_IMPORT_DESCRIPTOR pid = pStart;
                
                unsigned char gap[50];
                memset(gap, 0, sizeof(gap));

                // check if patched
                do 
                {
                    if (0 == memcmp(pid->Name + pBegin, "lcEngine.dll", lstrlenA("lcEngine.dll")))
                    {
                        PIMAGE_IMPORT_BY_NAME pibn = (PIMAGE_IMPORT_BY_NAME)(*((PDWORD)(pid->FirstThunk + pBegin)) + pBegin);
                        if (0 == memcmp(pibn->Name, "stub", 4))
                        {
                            UnmapViewOfFile(pBegin);
                            CloseHandle(hMap);
                            CloseHandle(hFile);
                            return TRUE;
                        }
                    }
                    ++pid;
                } while (0 != memcmp(pid, gap, sizeof(IMAGE_IMPORT_DESCRIPTOR)));
                
                pid = pStart;
                do
                {
                    ++pid;
                }while (0 != memcmp(pid, gap, sizeof(IMAGE_IMPORT_DESCRIPTOR)));
                DWORD imp_size = (PUCHAR)pid - (PUCHAR)pStart;

                PUCHAR pBlank = (PUCHAR)pStart;
                do
                {
                    pBlank += 4;
                }while (0 != memcmp(pBlank, gap, 32));
                pBlank += 4; // last name take one blank char
                /*
                OriginalFirstThunk  |             | FirstThunk      |             | 
                00 00 00 00         | 00 00 00 00 | 00 00 00 00     | 00 00 00 00 | 

                AddressOfData
                hint  | Name[]     | dll name
                00 00 | "stub" 00  | "lcEngine.dll" 00
                */
                DWORD   ThunkSize   = 8;
                //pStart -= pImportDir->VirtualAddress;
                PUCHAR  pOriginalFirstThunk = pBlank;
                DWORD   OriginalFirstThunk  = pBlank - (PUCHAR)pStart + pImportDir->VirtualAddress;
                
                PUCHAR  pFirstThunk = pOriginalFirstThunk + ThunkSize;
                DWORD   FirstThunk  = pFirstThunk - (PUCHAR)pStart + pImportDir->VirtualAddress;

                PUCHAR pAddressOfData   = pFirstThunk + ThunkSize;
                DWORD  AddressOfData    = pAddressOfData - (PUCHAR)pStart + pImportDir->VirtualAddress;

                PUCHAR pDllName = pAddressOfData + 7;
                DWORD  DllName = pDllName - (PUCHAR)pStart + pImportDir->VirtualAddress;

                // now we can't fill
                *((PDWORD)pOriginalFirstThunk) = AddressOfData;
                *((PDWORD)pFirstThunk) = AddressOfData;
                memcpy(pAddressOfData + 2, "stub", 4);
                memcpy(pDllName, "lcEngine.dll", lstrlenA("lcEngine.dll"));

                pBlank += 36;
                memcpy(pBlank, pStart, imp_size);
        
                // 
                pImportDir->VirtualAddress = pBlank - pBegin;

                /*
                OriginalFirstThunk | 00 00 00 00 |00 00 00 00 | DllName | FirstThunk |
                */
                pBlank += imp_size - 20;
                memset(pBlank, 0, 20);
                *((PDWORD)pBlank) = OriginalFirstThunk;
                *((PDWORD)(pBlank + 0xC)) = DllName;
                *((PDWORD)(pBlank + 0x10)) = FirstThunk;
                
                bRet = TRUE;
                UnmapViewOfFile(pBegin);
            }
            CloseHandle(hMap);
        }
        CloseHandle(hFile);
    }
    else
    {
        MessageBox(NULL, TEXT("can't patch emacs.exe, emacs maybe running, please shutdown emacs"), TEXT("error"), MB_ICONERROR | MB_OK);
    }
    
    return bRet;
}
U32 GetAddrDisLength(U8* addr)
{
    U32 ret = 0;
	ARCHITECTURE_TYPE arch = ARCH_X86;
	DISASSEMBLER dis;
	if (InitDisassembler(&dis, arch))
    {
		INSTRUCTION* pins = NULL;
		DWORD dwFlags = DISASM_DECODE | DISASM_DISASSEMBLE | DISASM_ALIGNOUTPUT;
		pins = GetInstruction(&dis, 0, addr, dwFlags);
        if(pins)
        {
            ret = pins->Length;
        }
        CloseDisassembler(&dis);
    }
    return ret;
}
int APIENTRY _tWinMain(HINSTANCE hInstance,
                     HINSTANCE hPrevInstance,
                     LPTSTR    lpCmdLine,
                     int       nCmdShow)
{
	UNREFERENCED_PARAMETER(hPrevInstance);
	UNREFERENCED_PARAMETER(lpCmdLine);

    TCHAR szEmacs[MAX_PATH];
    GetModuleFileName(NULL, szEmacs, MAX_PATH);
    PathRemoveFileSpec(szEmacs);
    PathAppend(szEmacs, TEXT("emacs.exe"));
    if (!PathFileExists(szEmacs))
    {
        MessageBox(NULL, TEXT("please put this program in the emacs bin directory !"), TEXT("error"), MB_OK | MB_ICONERROR);
        return -1;
    }

    // check the emacs version
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
    ::VerQueryValue(lpvMem,(LPTSTR)_T("\\"), (void**)&pFileInfo, &uInfoSize);
    DWORD dwProductVersionMS = pFileInfo->dwProductVersionMS;
    ::GlobalUnlock(hMem);  
    ::GlobalFree(hMem);  
    if(dwProductVersionMS < 0x00170002) //  only surpport >= 23.2
    {
        MessageBox(NULL, TEXT("only surpport the version of emacs >= 23.2 !"), TEXT("error"), MB_OK | MB_ICONERROR);
        return -1;
    }

    if (IDCANCEL == MessageBox(NULL, TEXT("Warning ! I will patch emacs.exe, and backup emacs.exe to emacs.bak.exe, continue ?")
        , TEXT("warning"), MB_OKCANCEL | MB_ICONWARNING))
    {
        return 0;
    }

    // get send-process(lookup in emacs source), start-process(elisp function) address, which we will hook in lcEngine
    DWORD send_process = 0, start_process = 0;
    HANDLE hFile = CreateFile(szEmacs, GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
    if (hFile != INVALID_HANDLE_VALUE)
    {
        HANDLE hMap = CreateFileMapping(hFile, NULL, PAGE_READONLY, 0, 0, NULL);
        if (hMap)
        {
            const unsigned char* pBegin = (const unsigned char*)MapViewOfFile(hMap, FILE_MAP_READ, 0, 0, 0);
            if (pBegin)
            {

                CSection Sec((const char*)pBegin);
                Cprocess_send_region process_send_region(Sec, dwProductVersionMS);
                send_process = process_send_region.GetAddr();
                Cstart_process start_process1(Sec);
                start_process = start_process1.GetAddr();

                // get the 'send-process' function address
                if (send_process)
                {
                    const unsigned char* pSea2 = pBegin + send_process - Sec.ImageBase();

                    // look for ret
                    do
                    {
                        pSea2 += GetAddrDisLength((U8*)pSea2);
                    }while(*pSea2 != 0xC3);
                    // look for call xxxx
                    do
                    {
                        pSea2 -= GetAddrDisLength((U8*)pSea2);
                    }while(*pSea2 != 0xE8);
                    int iDelta = *((int*)(pSea2 + 1));
                    unsigned int pCurAdr = pSea2 - pBegin + Sec.ImageBase();
                    send_process = pCurAdr + iDelta + 5;
                }
                UnmapViewOfFile(pBegin);
            }
            CloseHandle(hMap);
        }
        CloseHandle(hFile);
    }  

    if (send_process != 0 && start_process != 0)
    {
        TCHAR szIniFile[MAX_PATH];
        GetModuleFileName(NULL, szIniFile, MAX_PATH);
        PathRemoveFileSpec(szIniFile);
        PathAppend(szIniFile, TEXT("lcEngine.ini"));

         TCHAR szWrite[20];
         _itot(send_process, szWrite, 10);

        ::WritePrivateProfileString(TEXT("subr"), TEXT("send_process"), szWrite, szIniFile);

        _itot(start_process, szWrite, 10);
        ::WritePrivateProfileString(TEXT("subr"), TEXT("start_process"), szWrite, szIniFile);

        if(!AddNewImprortDll(szEmacs/*, "lcEngine.dll", "nullfunciton"*/))
        {
            MessageBox(NULL , TEXT("sorry, can't change the import"), TEXT("error"), MB_OK | MB_ICONERROR);
            return -1;
        }
        MessageBox(NULL, TEXT("Congratulations, it seems all OK. If you want to uninstall, just delete emacs.exe, and rename the emacs.bak.exe to emacs.exe")
            , TEXT("Congratulations"), MB_OK);
    }
    else
    {
        MessageBox(NULL , TEXT("sorry, can't get enough informations"), TEXT("error"), MB_OK | MB_ICONERROR);
    }
    return 0;
}
