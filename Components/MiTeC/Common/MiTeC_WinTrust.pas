{*******************************************************}
{                                                       }
{           Windows Trust API interface                 }
{                                                       }
{       Copyright (c) 2013-2018 Michal Mutl             }
{                                                       }
{*******************************************************}

{$I Compilers.Inc}

unit MiTeC_WinTrust;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils, System.Classes,
     {$ELSE}
     Windows, SysUtils, Classes,
     {$ENDIF}
     MiTeC_Windows, MiTeC_WinCrypt;

type
  CATALOG_INFO = record
    cbStruct: DWORD;
    wszCatalogFile: array[0..MAX_PATH] of WCHAR;
  end;
  PCATALOG_INFO = ^CATALOG_INFO;
  TCatalogInfo = CATALOG_INFO;
  PCatalogInfo = PCATALOG_INFO;

  PWintrustFileInfo = ^TWintrustFileInfo;
  {$EXTERNALSYM PWINTRUST_FILE_INFO}
  PWINTRUST_FILE_INFO = ^WINTRUST_FILE_INFO_;
  {$EXTERNALSYM WINTRUST_FILE_INFO_}
  WINTRUST_FILE_INFO_ = record
    cbStruct: DWORD;
    pcwszFilePath: LPCWSTR;
    hFile: THandle;
    pgKnownSubject: PGUID;
  end;
  {$EXTERNALSYM WINTRUST_FILE_INFO}
  WINTRUST_FILE_INFO = WINTRUST_FILE_INFO_;
  TWintrustFileInfo = WINTRUST_FILE_INFO_;

  PWintrustCatalogInfo = ^TWintrustCatalogInfo;
  {$EXTERNALSYM PWINTRUST_CATALOG_INFO}
  PWINTRUST_CATALOG_INFO = ^WINTRUST_CATALOG_INFO_;
  {$EXTERNALSYM WINTRUST_CATALOG_INFO_}
  WINTRUST_CATALOG_INFO_ = record
    cbStruct: DWORD;
    dwCatalogVersion: DWORD;
    pcwszCatalogFilePath: LPCWSTR;
    pcwszMemberTag: LPCWSTR;
    pcwszMemberFilePath: LPCWSTR;
    hMemberFile: THandle;
    pbCalculatedFileHash: PByte;
    cbCalculatedFileHash: DWORD;
    pcCatalogContext: PCCTL_CONTEXT;
  end;
  {$EXTERNALSYM WINTRUST_CATALOG_INFO}
  WINTRUST_CATALOG_INFO = WINTRUST_CATALOG_INFO_;
  TWintrustCatalogInfo = WINTRUST_CATALOG_INFO_;

  PWintrustBlobInfo = ^TWintrustBlobInfo;
  {$EXTERNALSYM PWINTRUST_BLOB_INFO}
  PWINTRUST_BLOB_INFO = ^WINTRUST_BLOB_INFO_;
  {$EXTERNALSYM WINTRUST_BLOB_INFO_}
  WINTRUST_BLOB_INFO_ = record
    cbStruct: DWORD;
    gSubject: TGUID;
    pcwszDisplayName: LPCWSTR;
    cbMemObject: DWORD;
    pbMemObject: PByte;
    cbMemSignedMsg: DWORD;
    pbMemSignedMsg: PByte;
  end;
  {$EXTERNALSYM WINTRUST_BLOB_INFO}
  WINTRUST_BLOB_INFO = WINTRUST_BLOB_INFO_;
  TWintrustBlobInfo = WINTRUST_BLOB_INFO_;

  PWintrustSgnrInfo = ^TWintrustSgnrInfo;
  {$EXTERNALSYM PWINTRUST_SGNR_INFO}
  PWINTRUST_SGNR_INFO = ^WINTRUST_SGNR_INFO_;
  {$EXTERNALSYM WINTRUST_SGNR_INFO_}
  WINTRUST_SGNR_INFO_ = record
    cbStruct: DWORD;
    pcwszDisplayName: LPCWSTR;
    psSignerInfo: PCMSG_SIGNER_INFO;
    chStores: DWORD;
    pahStores: PHCERTSTORE;
  end;
  {$EXTERNALSYM WINTRUST_SGNR_INFO}
  WINTRUST_SGNR_INFO = WINTRUST_SGNR_INFO_;
  TWintrustSgnrInfo = WINTRUST_SGNR_INFO_;

  PWintrustCertInfo = ^TWintrustCertInfo;
  {$EXTERNALSYM PWINTRUST_CERT_INFO}
  PWINTRUST_CERT_INFO = ^WINTRUST_CERT_INFO_;
  {$EXTERNALSYM WINTRUST_CERT_INFO_}
  WINTRUST_CERT_INFO_ = record
    cbStruct: DWORD;
    pcwszDisplayName: LPCWSTR;
    psCertContext: PCERT_CONTEXT;
    chStores: DWORD;
    pahStores: PHCERTSTORE;
    dwFlags: DWORD;
    psftVerifyAsOf: PFILETIME;
  end;
  {$EXTERNALSYM WINTRUST_CERT_INFO}
  WINTRUST_CERT_INFO = WINTRUST_CERT_INFO_;
  TWintrustCertInfo = WINTRUST_CERT_INFO_;

const
  {$EXTERNALSYM WTCI_DONT_OPEN_STORES}
  WTCI_DONT_OPEN_STORES = 1;
  {$EXTERNALSYM WTCI_OPEN_ONLY_ROOT}
  WTCI_OPEN_ONLY_ROOT   = 2;
type
  PWintrustData = ^TWintrustData;
  {$EXTERNALSYM PWINTRUST_DATA}
  PWINTRUST_DATA = ^WINTRUST_DATA;
  {$EXTERNALSYM _WINTRUST_DATA}
  _WINTRUST_DATA = record
    cbStruct: DWORD;
    pPolicyCallbackData: Pointer;
    pSIPClientData: Pointer;
    dwUIChoice: DWORD;
    fdwRevocationChecks: DWORD;
    dwUnionChoice: DWORD;
    InfoUnion: record
      case Integer of
        0: (pFile: PWintrustFileInfo);
        1: (pCatalog: PWintrustCatalogInfo);
        2: (pBlob: PWintrustBlobInfo);
        3: (pSgnr: PWintrustSgnrInfo);
        4: (pCert: PWintrustCertInfo);
    end;
    dwStateAction: DWORD;
    hWVTStateData: THandle;
    pwszUrlReference: LPCWSTR;
    dwProvFlags: DWORD;
    dwUIContext: DWORD;
  end;
  {$EXTERNALSYM WINTRUST_DATA}
  WINTRUST_DATA = _WINTRUST_DATA;
  TWintrustData = _WINTRUST_DATA;

const
  {$EXTERNALSYM WTD_UI_ALL}
  WTD_UI_ALL    = 1;
  {$EXTERNALSYM WTD_UI_NONE}
  WTD_UI_NONE   = 2;
  {$EXTERNALSYM WTD_UI_NOBAD}
  WTD_UI_NOBAD  = 3;
  {$EXTERNALSYM WTD_UI_NOGOOD}
  WTD_UI_NOGOOD = 4;

  {$EXTERNALSYM WTD_REVOKE_NONE}
  WTD_REVOKE_NONE       = 0;
  {$EXTERNALSYM WTD_REVOKE_WHOLECHAIN}
  WTD_REVOKE_WHOLECHAIN = 1;

  {$EXTERNALSYM WTD_CHOICE_FILE}
  WTD_CHOICE_FILE    = 1;
  {$EXTERNALSYM WTD_CHOICE_CATALOG}
  WTD_CHOICE_CATALOG = 2;
  {$EXTERNALSYM WTD_CHOICE_BLOB}
  WTD_CHOICE_BLOB    = 3;
  {$EXTERNALSYM WTD_CHOICE_SIGNER}
  WTD_CHOICE_SIGNER  = 4;
  {$EXTERNALSYM WTD_CHOICE_CERT}
  WTD_CHOICE_CERT    = 5;

  {$EXTERNALSYM WTD_STATEACTION_IGNORE}
  WTD_STATEACTION_IGNORE           = 0;
  {$EXTERNALSYM WTD_STATEACTION_VERIFY}
  WTD_STATEACTION_VERIFY           = 1;
  {$EXTERNALSYM WTD_STATEACTION_CLOSE}
  WTD_STATEACTION_CLOSE            = 2;
  {$EXTERNALSYM WTD_STATEACTION_AUTO_CACHE}
  WTD_STATEACTION_AUTO_CACHE       = 3;
  {$EXTERNALSYM WTD_STATEACTION_AUTO_CACHE_FLUSH}
  WTD_STATEACTION_AUTO_CACHE_FLUSH = 4;

  {$EXTERNALSYM WTD_PROV_FLAGS_MASK}
  WTD_PROV_FLAGS_MASK = $FFFF;
  {$EXTERNALSYM WTD_USE_IE4_TRUST_FLAG}
  WTD_USE_IE4_TRUST_FLAG   = 1;
  {$EXTERNALSYM WTD_NO_IE4_CHAIN_FLAG}
  WTD_NO_IE4_CHAIN_FLAG    = 2;
  {$EXTERNALSYM WTD_NO_POLICY_USAGE_FLAG}
  WTD_NO_POLICY_USAGE_FLAG = 4;
  {$EXTERNALSYM WTD_REVOCATION_CHECK_NONE}
  WTD_REVOCATION_CHECK_NONE               = $10;
  {$EXTERNALSYM WTD_REVOCATION_CHECK_END_CERT}
  WTD_REVOCATION_CHECK_END_CERT           = $20;
  {$EXTERNALSYM WTD_REVOCATION_CHECK_CHAIN}
  WTD_REVOCATION_CHECK_CHAIN              = $40;
  {$EXTERNALSYM WTD_REVOCATION_CHECK_CHAIN_EXCLUDE_ROOT}
  WTD_REVOCATION_CHECK_CHAIN_EXCLUDE_ROOT = $80;
  {$EXTERNALSYM WTD_SAFER_FLAG}
  WTD_SAFER_FLAG              = $100;
  {$EXTERNALSYM WTD_HASH_ONLY_FLAG}
  WTD_HASH_ONLY_FLAG          = $200;
  {$EXTERNALSYM WTD_USE_DEFAULT_OSVER_CHECK}
  WTD_USE_DEFAULT_OSVER_CHECK = $400;
  {$EXTERNALSYM WTD_LIFETIME_SIGNING_CHECK}
  WTD_LIFETIME_SIGNING_CHECK  = $800;
  {$EXTERNALSYM WTD_CACHE_ONLY_URL_RETRIEVAL}
  WTD_CACHE_ONLY_URL_RETRIEVAL = $1000;

  {$EXTERNALSYM WTD_UI_CONTEXT_EXECUTE}
  WTD_UI_CONTEXT_EXECUTE = 0;
  {$EXTERNALSYM WTD_UI_CONTEXT_INSTALL}
  WTD_UI_CONTEXT_INSTALL = 1;


type
  PCRYPT_PROVIDER_PRIVDATA = ^CRYPT_PROVIDER_PRIVDATA;
  _CRYPT_PROVIDER_PRIVDATA = record
    cbStruct: DWORD;
    gProviderID: GUID;
    cbProvData: DWORD;
    pvProvData: Pointer;
  end;
  CRYPT_PROVIDER_PRIVDATA = _CRYPT_PROVIDER_PRIVDATA;

  PPROVDATA_SIP = ^PROVDATA_SIP;
  _PROVDATA_SIP = record
    cbStruct: DWORD;
    gSubject: GUID;

    //The following members are actually typed pointers. The
    //corresponding structures are defined in mssip.h.
    pSip: Pointer;
    pCATSip: Pointer;
    psSipSubjectInfo: Pointer;
    psSipCATSubjectInfo: Pointer;
    psIndirectData: Pointer;
  end;
  PROVDATA_SIP = _PROVDATA_SIP;

  PCRYPT_PROVIDER_CERT = ^CRYPT_PROVIDER_CERT;
  _CRYPT_PROVIDER_CERT = record
    cbStruct: DWORD;
    pCert: PCCERT_CONTEXT;
    fCommercial: BOOL;
    fTrustedRoot: BOOL;
    fSelfSigned: BOOL;
    fTestCert: BOOL;
    dwRevokedReason: DWORD;
    dwConfidence: DWORD;
    dwError: DWORD;
    pTrustListContext: PCTL_CONTEXT;
    fTrustListSignerCert: BOOL;
    pCtlContext: PCCTL_CONTEXT;
    dwCtlError: DWORD;
    fIsCyclic: BOOL;
    pChainElement: PCERT_CHAIN_ELEMENT;
  end;
  CRYPT_PROVIDER_CERT = _CRYPT_PROVIDER_CERT;

  PCRYPT_PROVIDER_SGNR = ^CRYPT_PROVIDER_SGNR;
  _CRYPT_PROVIDER_SGNR = record
    cbStruct: DWORD;
    sftVerifyAsOf: FILETIME;
    csCertChain: DWORD;
    pasCertChain: PCRYPT_PROVIDER_CERT;
    dwSignerType: DWORD;
    psSigner: PCMSG_SIGNER_INFO;
    dwError: DWORD;
    csCounterSigners: DWORD;
    pasCounterSigners: PCRYPT_PROVIDER_SGNR;
    pChainContext: PCCERT_CHAIN_CONTEXT;
  end;
  CRYPT_PROVIDER_SGNR = _CRYPT_PROVIDER_SGNR;

  PCryptProviderData = ^TCryptProviderData;
  PCRYPT_PROVIDER_DATA = ^CRYPT_PROVIDER_DATA;
  _CRYPT_PROVIDER_DATA = record
    cbStruct: DWORD;
    pWintrustData: PWINTRUST_DATA;
    fOpenedFile: BOOL;
    hWndParent: HWND;
    pgActionId: PGUID;
    hProv: HCRYPTPROV;
    dwError: DWORD;
    dwRegSecuritySettings: DWORD;
    dwRegPolicySettings: DWORD;
    psPfns: Pointer; //actually a pointer to a _CRYPT_PROVIDER_FUNCTIONS
    cdwTrustStepErrors: DWORD;
    padwTrustStepErrors: PDWORD;
    chStores: DWORD;
    pahStores: PHCERTSTORE;
    dwEncoding: DWORD;
    hMsg: HCRYPTMSG;
    csSigners: DWORD;
    pasSigners: PCRYPT_PROVIDER_SGNR;
    csProvPrivData: DWORD;
    pasProvPrivData: PCRYPT_PROVIDER_PRIVDATA;
    dwSubjectChoice: DWORD;
    pPDSIP: PPROVDATA_SIP; //in C a union with one member
    pszUsageOID: PAnsiChar;
    fRecallWithState: BOOL;
    sftSystemTime: FILETIME;
    pszCTLSignerUsageOID: PAnsiChar;
    dwProvFlags: DWORD;
    dwFinalError: DWORD;
    pRequestUsage: PCERT_USAGE_MATCH;
    dwTrustPubSettings: DWORD;
    dwUIStateFlags: DWORD;
    //pSigState: Pointer; //PCRYPT_PROVIDER_SIGSTATE;
    //pSigSettings: Pointer; //PWINTRUST_SIGNATURE_SETTINGS
  end;
  CRYPT_PROVIDER_DATA = _CRYPT_PROVIDER_DATA;
  TCryptProviderData = _CRYPT_PROVIDER_DATA;

const
  WINTRUST_ACTION_GENERIC_VERIFY_V2: TGUID = '{00aac56b-cd44-11d0-8cc2-00c04fc295ee}';
  CRYPT_E_SECURITY_SETTINGS = HRESULT($80092026);

function WinVerifyTrust(hwnd: HWND; const pgActionID: TGUID; var pWinTrustData: _WINTRUST_DATA): LONG; stdcall;
function WinVerifyTrustEx(hwnd: HWND; const pgActionID: TGUID; pWinTrustData: PWINTRUST_DATA): LONG; stdcall;
function WTHelperGetProvSignerFromChain(pProvData: PCRYPT_PROVIDER_DATA; idxSigner: DWORD; fCounterSigner: BOOL; idxCounterSigner: DWORD): PCRYPT_PROVIDER_SGNR; stdcall;
function WTHelperGetProvCertFromChain(pSgnr: PCRYPT_PROVIDER_SGNR; idxCert: DWORD): PCRYPT_PROVIDER_CERT; stdcall;
function WTHelperProvDataFromStateData(hStateData: THandle): PCRYPT_PROVIDER_DATA; stdcall;
function WTHelperGetProvPrivateDataFromChain(pProvData: PCRYPT_PROVIDER_DATA; const pgProviderId: TGUID): PCRYPT_PROVIDER_PRIVDATA; stdcall;
function WTHelperCertIsSelfSigned(dwEncoding: DWORD; pCert: PCERT_INFO): BOOL; stdcall;
function WTHelperCertCheckValidSignature(pProvData: PCRYPT_PROVIDER_DATA): HRESULT; stdcall;

function CryptCATAdminAcquireContext(var hCATAdmin: THandle; pgSubsystem: PGUID; dwFlags: DWORD): BOOL; stdcall;
function CryptCATAdminReleaseContext(hCATAdmin: THANDLE; dwFlags: DWORD): BOOL; stdcall;
function CryptCATAdminCalcHashFromFileHandle(hFile: THANDLE; var dwSize: DWORD; buf: PByte; dwFlags: DWORD): BOOL; stdcall;
function CryptCATAdminEnumCatalogFromHash(hCATAdmin: THANDLE; pbHash: PByte; pHashSize: DWORD; dwFlags: DWORD; phPrevCatInfo: PHandle): THandle;
function CryptCATCatalogInfoFromContext(hCatInfo: THANDLE; psCatInfo: PWintrustCatalogInfo; dwFlags: DWORD): BOOL; stdcall;
function CryptCATAdminReleaseCatalogContext(hCATAdmin: THANDLE; hCatInfo: THANDLE; dwFlags: DWORD): BOOL; stdcall;

implementation

const
  wintrustdll = 'wintrust.dll';

type
  TWinVerifyTrust = function(hwnd: HWND; const pgActionID: TGUID; var pWinTrustData: _WINTRUST_DATA): LONG; stdcall;
  TWinVerifyTrustEx = function(hwnd: HWND; const pgActionID: TGUID; pWinTrustData: PWINTRUST_DATA): LONG; stdcall;
  TWTHelperGetProvSignerFromChain = function(pProvData: PCRYPT_PROVIDER_DATA; idxSigner: DWORD; fCounterSigner: BOOL; idxCounterSigner: DWORD): PCRYPT_PROVIDER_SGNR; stdcall;
  TWTHelperGetProvCertFromChain = function(pSgnr: PCRYPT_PROVIDER_SGNR; idxCert: DWORD): PCRYPT_PROVIDER_CERT; stdcall;
  TWTHelperProvDataFromStateData = function(hStateData: THandle): PCRYPT_PROVIDER_DATA; stdcall;
  TWTHelperGetProvPrivateDataFromChain = function(pProvData: PCRYPT_PROVIDER_DATA; const pgProviderId: TGUID): PCRYPT_PROVIDER_PRIVDATA; stdcall;
  TWTHelperCertIsSelfSigned = function(dwEncoding: DWORD; pCert: PCERT_INFO): BOOL; stdcall;
  TWTHelperCertCheckValidSignature = function(pProvData: PCRYPT_PROVIDER_DATA): HRESULT; stdcall;
  TCryptCATAdminAcquireContext = function(var HCatAdmin: THandle; pgSubsystem: PGUID; dwFlags: DWORD): BOOL; stdcall;
  TCryptCATAdminReleaseContext = function(hAdmin: THANDLE; dwFlags: DWORD): BOOL; stdcall;
  TCryptCATAdminCalcHashFromFileHandle = function(hFile: THANDLE; var dwSize: DWORD; buf: PByte; dwFlags: DWORD): BOOL; stdcall;
  TCryptCATAdminEnumCatalogFromHash = function(hAdmin: THANDLE; pbHash: PByte; pHashSize: DWORD; dwFlags: DWORD; phPrevCatInfo: PHandle): THandle;
  TCryptCATCatalogInfoFromContext = function(hCatInfo: THANDLE; psCatInfo: PWintrustCatalogInfo; dwFlags: DWORD): BOOL; stdcall;
  TCryptCATAdminReleaseCatalogContext = function(hAdmin: THANDLE; hCatInfo: THANDLE; dwFlags: DWORD): BOOL; stdcall;

var
  _WinVerifyTrust: TWinVerifyTrust = nil;
  _WinVerifyTrustEx: TWinVerifyTrustEx = nil;
  _WTHelperGetProvSignerFromChain: TWTHelperGetProvSignerFromChain = nil;
  _WTHelperGetProvCertFromChain: TWTHelperGetProvCertFromChain = nil;
  _WTHelperProvDataFromStateData: TWTHelperProvDataFromStateData = nil;
  _WTHelperGetProvPrivateDataFromChain: TWTHelperGetProvPrivateDataFromChain = nil;
  _WTHelperCertIsSelfSigned: TWTHelperCertIsSelfSigned = nil;
  _WTHelperCertCheckValidSignature: TWTHelperCertCheckValidSignature = nil;
  _CryptCATAdminAcquireContext: TCryptCATAdminAcquireContext = nil;
  _CryptCATAdminReleaseContext: TCryptCATAdminReleaseContext = nil;
  _CryptCATAdminCalcHashFromFileHandle: TCryptCATAdminCalcHashFromFileHandle = nil;
  _CryptCATAdminEnumCatalogFromHash: TCryptCATAdminEnumCatalogFromHash = nil;
  _CryptCATCatalogInfoFromContext: TCryptCATCatalogInfoFromContext = nil;
  _CryptCATAdminReleaseCatalogContext: TCryptCATAdminReleaseCatalogContext = nil;

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

function WinVerifyTrust;
begin
  if not Assigned(_WinVerifyTrust) then
    GetProcedureAddress(Pointer(@_WinVerifyTrust),wintrustdll,'WinVerifyTrust');
  if Assigned(_WinVerifyTrust) then
    Result:=_WinVerifyTrust(hwnd,pgActionID,pWinTrustData)
  else
    Result:=TRUST_E_PROVIDER_UNKNOWN;
end;

function WinVerifyTrustEx;
begin
  if not Assigned(_WinVerifyTrustEx) then
    GetProcedureAddress(Pointer(@_WinVerifyTrustEx),wintrustdll,'WinVerifyTrustEx');
  if Assigned(_WinVerifyTrustEx) then
    Result:=_WinVerifyTrustEx(hwnd,pgActionID,pWinTrustData)
  else
    Result:=TRUST_E_PROVIDER_UNKNOWN;
end;

function WTHelperGetProvSignerFromChain;
begin
  if not Assigned(_WTHelperGetProvSignerFromChain) then
    GetProcedureAddress(Pointer(@_WTHelperGetProvSignerFromChain),wintrustdll,'WTHelperGetProvSignerFromChain');
  if Assigned(_WTHelperGetProvSignerFromChain) then
    Result:=_WTHelperGetProvSignerFromChain(pProvData,idxSigner,fCounterSigner,idxCounterSigner)
  else
    Result:=nil;
end;

function WTHelperGetProvCertFromChain;
begin
  if not Assigned(_WTHelperGetProvCertFromChain) then
    GetProcedureAddress(Pointer(@_WTHelperGetProvCertFromChain),wintrustdll,'WTHelperGetProvCertFromChain');
  if Assigned(_WTHelperGetProvCertFromChain) then
    Result:=_WTHelperGetProvCertFromChain(pSgnr,idxCert)
  else
    Result:=nil;
end;

function WTHelperProvDataFromStateData;
begin
  if not Assigned(_WTHelperProvDataFromStateData) then
    GetProcedureAddress(Pointer(@_WTHelperProvDataFromStateData),wintrustdll,'WTHelperProvDataFromStateData');
  if Assigned(_WTHelperProvDataFromStateData) then
    Result:=_WTHelperProvDataFromStateData(hStateData)
  else
    Result:=nil;
end;

function WTHelperGetProvPrivateDataFromChain;
begin
  if not Assigned(_WTHelperGetProvPrivateDataFromChain) then
    GetProcedureAddress(Pointer(@_WTHelperGetProvPrivateDataFromChain),wintrustdll,'WTHelperGetProvPrivateDataFromChain');
  if Assigned(_WTHelperGetProvPrivateDataFromChain) then
    Result:=_WTHelperGetProvPrivateDataFromChain(pProvData,pgProviderId)
  else
    Result:=nil;
end;

function WTHelperCertIsSelfSigned;
begin
  if not Assigned(_WTHelperCertIsSelfSigned) then
    GetProcedureAddress(Pointer(@_WTHelperCertIsSelfSigned),wintrustdll,'WTHelperCertIsSelfSigned');
  if Assigned(_WTHelperCertIsSelfSigned) then
    Result:=_WTHelperCertIsSelfSigned(dwEncoding,pCert)
  else
    Result:=False;
end;

function WTHelperCertCheckValidSignature;
begin
  if not Assigned(_WTHelperCertCheckValidSignature) then
    GetProcedureAddress(Pointer(@_WTHelperCertCheckValidSignature),wintrustdll,'WTHelperCertCheckValidSignature');
  if Assigned(_WTHelperCertCheckValidSignature) then
    Result:=_WTHelperCertCheckValidSignature(pProvData)
  else
    Result:=TRUST_E_PROVIDER_UNKNOWN;
end;

function CryptCATAdminAcquireContext;
begin
  if not Assigned(_CryptCATAdminAcquireContext) then
    GetProcedureAddress(Pointer(@_CryptCATAdminAcquireContext),wintrustdll,'CryptCATAdminAcquireContext');
  if Assigned(_CryptCATAdminAcquireContext) then
    Result:=_CryptCATAdminAcquireContext(hCATAdmin,pgSubsystem,dwFlags)
  else
    Result:=False;
end;

function CryptCATAdminReleaseContext;
begin
  if not Assigned(_CryptCATAdminReleaseContext) then
    GetProcedureAddress(Pointer(@_CryptCATAdminReleaseContext),wintrustdll,'CryptCATAdminReleaseContext');
  if Assigned(_CryptCATAdminReleaseContext) then
    Result:=_CryptCATAdminReleaseContext(hCATAdmin,dwFlags)
  else
    Result:=False;
end;

function CryptCATAdminCalcHashFromFileHandle;
begin
  if not Assigned(_CryptCATAdminCalcHashFromFileHandle) then
    GetProcedureAddress(Pointer(@_CryptCATAdminCalcHashFromFileHandle),wintrustdll,'CryptCATAdminCalcHashFromFileHandle');
  if Assigned(_CryptCATAdminCalcHashFromFileHandle) then
    Result:=_CryptCATAdminCalcHashFromFileHandle(hFile,dwSize,buf,dwFlags)
  else
    Result:=False;
end;

function CryptCATAdminEnumCatalogFromHash;
begin
  if not Assigned(_CryptCATAdminEnumCatalogFromHash) then
    GetProcedureAddress(Pointer(@_CryptCATAdminEnumCatalogFromHash),wintrustdll,'CryptCATAdminEnumCatalogFromHash');
  if Assigned(_CryptCATAdminEnumCatalogFromHash) then
    Result:=_CryptCATAdminEnumCatalogFromHash(hCATAdmin,pbHash,pHashSize,dwFlags,phPrevCatInfo)
  else
    Result:=0;
end;

function CryptCATCatalogInfoFromContext;
begin
  if not Assigned(_CryptCATCatalogInfoFromContext) then
    GetProcedureAddress(Pointer(@_CryptCATCatalogInfoFromContext),wintrustdll,'CryptCATCatalogInfoFromContext');
  if Assigned(_CryptCATCatalogInfoFromContext) then
    Result:=_CryptCATCatalogInfoFromContext(hCatInfo,psCatInfo,dwFlags)
  else
    Result:=False;
end;

function CryptCATAdminReleaseCatalogContext;
begin
  if not Assigned(_CryptCATAdminReleaseCatalogContext) then
    GetProcedureAddress(Pointer(@_CryptCATAdminReleaseCatalogContext),wintrustdll,'CryptCATAdminReleaseCatalogContext');
  if Assigned(_CryptCATAdminReleaseCatalogContext) then
    Result:=_CryptCATAdminReleaseCatalogContext(hCATAdmin,hCATInfo,dwFlags)
  else
    Result:=False;
end;

end.
