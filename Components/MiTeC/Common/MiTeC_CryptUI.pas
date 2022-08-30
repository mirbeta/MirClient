{*******************************************************}
{                                                       }
{         Windows CryptUI API interface                 }
{                                                       }
{       Copyright (c) 2013-2018 Michal Mutl             }
{                                                       }
{*******************************************************}

{$I Compilers.Inc}

unit MiTeC_CryptUI;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils, System.Classes,
     {$ELSE}
     Windows, SysUtils, Classes,
     {$ENDIF}
     MiTeC_Windows, MiTeC_WinCrypt;

type
  CRYPTUI_VIEWSIGNERINFO_STRUCT_A = record
    dwSize: DWORD;
    hwndParent: HWND;
    dwFlags: DWORD;
    szTitle: LPCSTR;
    pSignerInfo: PCMSG_SIGNER_INFO;
    hMsg: HCRYPTMSG;
    pszOID: LPCSTR;
    dwReserved: PDWORD;
    cStores: DWORD;
    rghStores: PHCERTSTORE;
    cPropSheetPages: DWORD;
    rgPropSheetPages: Pointer;//LPCPROPSHEETPAGE;
  end;
  PCRYPTUI_VIEWSIGNERINFO_STRUCT_A = ^CRYPTUI_VIEWSIGNERINFO_STRUCT_A;

  CRYPTUI_VIEWSIGNERINFO_STRUCT_W = record
    dwSize: DWORD;
    hwndParent: HWND;
    dwFlags: DWORD;
    szTitle: LPCWSTR;
    pSignerInfo: PCMSG_SIGNER_INFO;
    hMsg: HCRYPTMSG;
    pszOID: LPCSTR;
    dwReserved: PDWORD;
    cStores: DWORD;
    rghStores: PHCERTSTORE;
    cPropSheetPages: DWORD;
    rgPropSheetPages: Pointer;//LPCPROPSHEETPAGE;
  end;
  PCRYPTUI_VIEWSIGNERINFO_STRUCT_W = ^CRYPTUI_VIEWSIGNERINFO_STRUCT_W;

  {$IFDEF UNICODE}
  CRYPTUI_VIEWSIGNERINFO_STRUCT = CRYPTUI_VIEWSIGNERINFO_STRUCT_W;
  PCRYPTUI_VIEWSIGNERINFO_STRUCT = PCRYPTUI_VIEWSIGNERINFO_STRUCT_W;
  {$ELSE}
  CRYPTUI_VIEWSIGNERINFO_STRUCT = CRYPTUI_VIEWSIGNERINFO_STRUCT_A;
  PCRYPTUI_VIEWSIGNERINFO_STRUCT = PCRYPTUI_VIEWSIGNERINFO_STRUCT_A;
  {$ENDIF}

  PCRYPTUI_VIEWCERTIFICATE_STRUCT_A = ^CRYPTUI_VIEWCERTIFICATE_STRUCT_A;
  CRYPTUI_VIEWCERTIFICATE_STRUCT_A = record
    dwSize: DWORD;
    hwndParent: HWND;
    dwFlags: DWORD;
    szTitle: LPCSTR;
    pCertContext: PCCERT_CONTEXT;
    rgszPurposes: LPCSTR;
    cPurposes: DWORD;
    pCryptProviderData: Pointer;
    fpCryptProviderDataTrustedUsage: BOOL;
    idxSigner: DWORD;
    idxCert: DWORD;
    fCounterSigner: BOOL;
    idxCounterSigner: DWORD;
    cStores: DWORD;
    rghStores: Pointer;
    cPropSheetPages: DWORD;
    rgPropSheetPages: Pointer;
    nStartPage: DWORD;
  end;

  PCRYPTUI_VIEWCERTIFICATE_STRUCT_W = ^CRYPTUI_VIEWCERTIFICATE_STRUCT_W;
  CRYPTUI_VIEWCERTIFICATE_STRUCT_W = record
    dwSize: DWORD;
    hwndParent: HWND;
    dwFlags: DWORD;
    szTitle: LPCWSTR;
    pCertContext: PCCERT_CONTEXT;
    rgszPurposes: LPCWSTR;
    cPurposes: DWORD;
    pCryptProviderData: Pointer;
    fpCryptProviderDataTrustedUsage: BOOL;
    idxSigner: DWORD;
    idxCert: DWORD;
    fCounterSigner: BOOL;
    idxCounterSigner: DWORD;
    cStores: DWORD;
    rghStores: Pointer;
    cPropSheetPages: DWORD;
    rgPropSheetPages: Pointer;
    nStartPage: DWORD;
  end;

  {$IFDEF UNICODE}
  CRYPTUI_VIEWCERTIFICATE_STRUCT = CRYPTUI_VIEWCERTIFICATE_STRUCT_W;
  PCRYPTUI_VIEWCERTIFICATE_STRUCT = PCRYPTUI_VIEWCERTIFICATE_STRUCT_W;
  {$ELSE}
  CRYPTUI_VIEWCERTIFICATE_STRUCT = CRYPTUI_VIEWCERTIFICATE_STRUCT_A;
  PCRYPTUI_VIEWCERTIFICATE_STRUCT = PCRYPTUI_VIEWCERTIFICATE_STRUCT_A;
  {$ENDIF}


function CryptUIDlgViewSignerInfoA(pcvsi: PCRYPTUI_VIEWSIGNERINFO_STRUCT_A): bool; stdcall;
function CryptUIDlgViewSignerInfoW(pcvsi: PCRYPTUI_VIEWSIGNERINFO_STRUCT_W): bool; stdcall;
function CryptUIDlgViewSignerInfo(pcvsi: PCRYPTUI_VIEWSIGNERINFO_STRUCT): bool; stdcall;
function CryptUIDlgViewCertificateA(pCertInfo: PCRYPTUI_VIEWCERTIFICATE_STRUCT_A; var pfPropertiesChanged: Bool): Bool; stdcall;
function CryptUIDlgViewCertificateW(pCertInfo: PCRYPTUI_VIEWCERTIFICATE_STRUCT_W; var pfPropertiesChanged: Bool): Bool; stdcall;
function CryptUIDlgViewCertificate(pCertInfo: PCRYPTUI_VIEWCERTIFICATE_STRUCT; var pfPropertiesChanged: Bool): Bool; stdcall;

implementation

const
  cryptuidll = 'Cryptui.dll';

procedure GetProcedureAddress(var P: Pointer; const ModuleName, ProcName: string);
var
  ModuleHandle: HMODULE;
begin
  if not Assigned(P) then begin
    ModuleHandle:=GetModuleHandle(PChar(ModuleName));
    if ModuleHandle=0 then begin
      ModuleHandle:=LoadLibrary(PChar(ModuleName));
      if ModuleHandle=0 then
        Exit;
    end;
    P:=GetProcAddress(ModuleHandle,PChar(ProcName));
    if not Assigned(P) then
      Exit;
  end;
end;

type
  TCryptUIDlgViewSignerInfoA = function(pcvsi: PCRYPTUI_VIEWSIGNERINFO_STRUCT_A): bool; stdcall;
var
  _CryptUIDlgViewSignerInfoA: TCryptUIDlgViewSignerInfoA = nil;

function CryptUIDlgViewSignerInfoA;
begin
  if not Assigned(_CryptUIDlgViewSignerInfoA) then
    GetProcedureAddress(Pointer(@_CryptUIDlgViewSignerInfoA),cryptuidll,'CryptUIDlgViewSignerInfoA');
  if Assigned(_CryptUIDlgViewSignerInfoA) then
    Result:=_CryptUIDlgViewSignerInfoA(pcvsi)
  else
    Result:=False;
end;

type
  TCryptUIDlgViewSignerInfoW = function(pcvsi: PCRYPTUI_VIEWSIGNERINFO_STRUCT_W): bool; stdcall;
var
  _CryptUIDlgViewSignerInfoW: TCryptUIDlgViewSignerInfoW = nil;

function CryptUIDlgViewSignerInfoW;
begin
  if not Assigned(_CryptUIDlgViewSignerInfoW) then
    GetProcedureAddress(Pointer(@_CryptUIDlgViewSignerInfoW),cryptuidll,'CryptUIDlgViewSignerInfoW');
  if Assigned(_CryptUIDlgViewSignerInfoW) then
    Result:=_CryptUIDlgViewSignerInfoW(pcvsi)
  else
    Result:=False;
end;

function CryptUIDlgViewSignerInfo;
begin
  {$IFDEF UNICODE}
  Result:=CryptUIDlgViewSignerInfoW(pcvsi);
  {$ELSE}
  Result:=CryptUIDlgViewSignerInfoA(pcvsi);
  {$ENDIF}
end;

type
  TCryptUIDlgViewCertificateA = function(pCertInfo: PCRYPTUI_VIEWCERTIFICATE_STRUCT_A; var pfPropertiesChanged: Bool): bool; stdcall;
var
  _CryptUIDlgViewCertificateA: TCryptUIDlgViewCertificateA = nil;

function CryptUIDlgViewCertificateA;
begin
  if not Assigned(_CryptUIDlgViewCertificateA) then
    GetProcedureAddress(Pointer(@_CryptUIDlgViewCertificateA),cryptuidll,'CryptUIDlgViewCertificateA');
  if Assigned(_CryptUIDlgViewCertificateA) then
    Result:=_CryptUIDlgViewCertificateA(pCertInfo,pfPropertiesChanged)
  else
    Result:=False;
end;

type
  TCryptUIDlgViewCertificateW = function(pCertInfo: PCRYPTUI_VIEWCERTIFICATE_STRUCT_W; var pfPropertiesChanged: Bool): bool; stdcall;
var
  _CryptUIDlgViewCertificateW: TCryptUIDlgViewCertificateW = nil;

function CryptUIDlgViewCertificateW;
begin
  if not Assigned(_CryptUIDlgViewCertificateW) then
    GetProcedureAddress(Pointer(@_CryptUIDlgViewCertificateW),cryptuidll,'CryptUIDlgViewCertificateW');
  if Assigned(_CryptUIDlgViewCertificateW) then
    Result:=_CryptUIDlgViewCertificateW(pCertInfo,pfPropertiesChanged)
  else
    Result:=False;
end;

function CryptUIDlgViewCertificate;
begin
  {$IFDEF UNICODE}
  Result:=CryptUIDlgViewCertificateW(pCertInfo,pfPropertiesChanged);
  {$ELSE}
  Result:=CryptUIDlgViewCertificateA(pCertInfo,pfPropertiesChanged);
  {$ENDIF}
end;

end.
