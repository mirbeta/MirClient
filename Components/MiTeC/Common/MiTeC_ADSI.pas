{*******************************************************}
{               MiTeC Common Routines                   }
{             Active Directory Services Interface       }
{                                                       }
{        Copyright (c) 1997-2019 Michal Mutl            }
{                                                       }
{*******************************************************}

{$INCLUDE Compilers.inc}

unit MiTeC_ADSI;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils, System.Classes, WinAPI.ActiveX, VCL.OleServer,
     System.Variants,
     {$ELSE}
     Windows, SysUtils, Classes, Variants, OLEServer, ActiveX,
     {$ENDIF}
     {$IFDEF FPC}MiTeC_FPC_ActiveDs_TLB{$ELSE}MiTeC_ActiveDs_TLB{$ENDIF};

const
  S_ADS_NOMORE_ROWS  = HRESULT($00005012);
  S_ADS_NOMORE_COLUMNS = HRESULT($00005013);
  MAX_ADS_ENUM_COUNT = 100;

type
  TADsEnumCallBack = procedure(ADisp: IADs) of object;

  TADsGetObject = function (lpszPathName: WideString; const riid: TGUID; out ppObject): HRESULT; safecall;
  TADsBuildEnumerator = function (const pADsContainer: IADsContainer; out ppEnumVariant: IEnumVARIANT): HRESULT; safecall;
  TADsFreeEnumerator = function (pEnumVariant: IEnumVARIANT): HRESULT; safecall;
  TADsEnumerateNext = function (pEnumVariant: IEnumVARIANT; cElements: ULONG; var pvar: OleVARIANT; var pcElementsFetched: ULONG): HRESULT; safecall;
  TADsBuildVarArrayStr = function (lppPathNames: PWideChar; dwPathNames: DWORD; var pVar: VARIANT): HRESULT; safecall;
  TADsBuildVarArrayInt = function (lpdwObjectTypes: LPDWORD; dwObjectTypes: DWORD; var pVar: VARIANT): HRESULT; safecall;
  TADsOpenObject = function (lpszPathName, lpszUserName, lpszPassword: WideString; dwReserved: DWORD; const riid: TGUID; out ppObject): HRESULT; safecall;
  TADsGetLastError = function (var pError: DWORD; lpErrorBuf: PWideChar; dwErrorBufLen: DWORD; lpNameBuf: PWideChar; dwNameBufLen: DWORD): HRESULT; stdcall;
  TADsSetLastError = procedure (dwErr: DWORD; pszErro, pszProvider: LPWSTR); stdcall;
  TAllocADsMem = procedure (cb: DWORD); stdcall;
  TFreeADsMem = function (pMem: Pointer): BOOL; stdcall;
  TReallocADsMem = function (pOldMem: Pointer; cbOld, cbNew: DWORD): Pointer; stdcall;
  TAllocADsStr = function (pStr: LPWSTR): LPWSTR; stdcall;
  TFreeADsStr = function (pStr: LPWSTR): BOOL; stdcall;
  TReallocADsStr = function (ppStr: LPWSTR; pStr: LPWSTR): BOOL; stdcall;
  TADsEncodeBinaryData = function (pbSrcData: PBYTE; dwSrcLen: DWORD; ppszDestData: LPWSTR): HRESULT; stdcall;
  TPropVariantToAdsType = function (pVariant: Variant; dwNumVariant: DWORD; ppAdsValues: Pointer; pdwNumValues: PDWORD): HRESULT; stdcall;
  TAdsTypeToPropVariant = function (pAdsValues: Pointer; dwNumValues: DWORD; pVariant: variant): HRESULT; stdcall;
  TAdsFreeAdsValues = procedure (pAdsValues: Pointer; dwNumValues: DWORD); stdcall;

function GetObject(APath: string): IDispatch;
procedure ADsEnumerateObjects(APath: string; AEnumClbk: TADsEnumCallback); overload;
procedure ADsEnumerateObjects(AContainer: IADsContainer; AEnumClbk: TADsEnumCallback); overload;
procedure ADsEnumerateObjectsRecursive(AContainer: IADsContainer; AEnumClbk: TADsEnumCallback);
procedure ADsEnumerateMembers(AMembers: IADsMembers; AEnumClbk: TADsEnumCallback);

var
  ADsGetObject: TADsGetObject = nil;
  ADsBuildEnumerator: TADsBuildEnumerator = nil;
  ADsFreeEnumerator: TADsFreeEnumerator = nil;
  ADsEnumerateNext: TADsEnumerateNext = nil;
  ADsBuildVarArrayStr: TADsBuildVarArrayStr = nil;
  ADsBuildVarArrayInt: TADsBuildVarArrayInt = nil;
  ADsOpenObject: TADsOpenObject = nil;
  ADsGetLastError: TADsGetLastError = nil;
  ADsSetLastError: TADsSetLastError = nil;
  AllocADsMem: TAllocADsMem = nil;
  FreeADsMem: TFreeADsMem = nil;
  ReallocADsMem: TReallocADsMem = nil;
  AllocADsStr: TAllocADsStr = nil;
  FreeADsStr: TFreeADsStr = nil;
  ReallocADsStr: TReallocADsStr = nil;
  ADsEncodeBinaryData: TADsEncodeBinaryData = nil;
  PropVariantToAdsType: TPropVariantToAdsType = nil;
  AdsTypeToPropVariant: TAdsTypeToPropVariant = nil;
  AdsFreeAdsValues: TAdsFreeAdsValues = nil;

  ADSIHandle, ADSLDPCHandle : THandle;
  ADSILoaded: Boolean;

implementation

const
  ADSI = 'activeds.dll';
  ADSLDPC = 'adsldpc.dll';

function GetObject;
begin
  ADsGetObject(APath,IDispatch,Result);
end;

procedure ADsEnumerateObjects(AContainer: IADsContainer; AEnumClbk: TADsEnumCallback);
var
  enum: IEnumVARIANT;
  va: OleVariant;
  n: Cardinal;
  obj: IADs;
  hr: integer;
begin
  hr:=ADsBuildEnumerator(AContainer,enum);
  while Succeeded(hr) do begin
    hr:=ADsEnumerateNext(enum,1,va,n);
    if n=0 then
      Break;
    IDispatch(va).QueryInterface(IADs,obj);
    if Assigned(obj) then
      AEnumClbk(obj);
    Finalize(va);
    va:=null;
  end;
end;

procedure ADsEnumerateObjects(APath :string; AEnumClbk: TADsEnumCallback);
var
  c: IADsContainer;
begin
  try
    ADsGetObject(APath,IADsContainer,c);
  except
    raise Exception.Create('IADsContainer not supported.');
  end;
  ADsEnumerateObjects(c,AEnumClbk);
end;

procedure ADsEnumerateObjectsRecursive;
var
  enum: IEnumVARIANT;
  va: OleVariant;
  n: Cardinal;
  obj: IADs;
  cont: IADsContainer;
  hr: integer;
begin
  hr:=ADsBuildEnumerator(AContainer,enum);
  while Succeeded(hr) do begin
    hr:=ADsEnumerateNext(enum,1,va,n);
    if n=0 then
      Break;
    IDispatch(va).QueryInterface(IADs,obj);
    if Assigned(obj) then
      AEnumClbk(obj);
    IDispatch(va).QueryInterface(IADsContainer,cont);
    ADsEnumerateObjectsRecursive(cont,AEnumClbk);
    Finalize(va);
    va:=null;
  end;
end;

procedure ADsEnumerateMembers;
var
  enum: IEnumVARIANT;
  va: OleVariant;
  n: Cardinal;
  obj: IADs;
  hr: integer;
begin
  hr:=S_OK;
  enum:=AMembers._NewEnum as IEnumVariant;
  while Succeeded(hr) do begin
    hr:=ADsEnumerateNext(enum,1,va,n);
    if n=0 then
      Break;
    IDispatch(va).QueryInterface(IADs,obj);
    if Assigned(obj) then
      AEnumClbk(obj);
    Finalize(va);
    va:=null;
  end;
end;

function InitAD: Boolean;
begin
  ADSIHandle:=GetModuleHandle(ADSI);
  ADSIHandle:=LoadLibrary(ADSI);
  if ADSIHandle<>0 then begin
    @ADsGetObject:=getprocaddress(ADSIHandle,PAnsiChar('ADsGetObject'));
    @ADsBuildEnumerator:=getprocaddress(ADSIHandle,PAnsiChar('ADsBuildEnumerator'));
    @ADsEnumerateNext:=getprocaddress(ADSIHandle,PAnsiChar('ADsEnumerateNext'));
    @ADsFreeEnumerator:=getprocaddress(ADSIHandle,PAnsiChar('ADsFreeEnumerator'));
    @ADsBuildVarArrayStr:=getprocaddress(ADSIHandle,PAnsiChar('ADsBuildVarArrayStr'));
    @ADsBuildVarArrayInt:=getprocaddress(ADSIHandle,PAnsiChar('ADsBuildVarArrayInt'));
    @ADsOpenObject:=getprocaddress(ADSIHandle,PAnsiChar('ADsOpenObject'));
    @PropVariantToAdsType:=Getprocaddress(ADSIHandle,PAnsiChar('PropVariantToAdsType'));
    @AdsTypeToPropVariant:=getprocaddress(ADSIHandle,PAnsiChar('AdsTypeToPropVariant'));
    @AdsFreeAdsValues:=getprocaddress(ADSIHandle,PAnsiChar('AdsFreeAdsValues'));
  end;
  result:=(ADSIHandle<>0) and Assigned(ADsGetObject);

  ADSLDPCHandle:=GetModuleHandle(ADSLDPC);
  ADSLDPCHandle:=LoadLibrary(ADSLDPC);
  if ADSLDPCHandle<>0 then begin
    @ADsGetLastError:=getprocaddress(ADSLDPCHandle,PAnsiChar('ADsGetLastError'));
    @ADsSetLastError:=getprocaddress(ADSLDPCHandle,PAnsiChar('ADsSetLastError'));
    @AllocADsMem:=getprocaddress(ADSLDPCHandle,PAnsiChar('AllocADsMem'));
    @FreeADsMem:=getprocaddress(ADSLDPCHandle,PAnsiChar('FreeADsMem'));
    @ReallocADsMem:=getprocaddress(ADSLDPCHandle,PAnsiChar('ReallocADsMem'));
    @AllocADsStr:=getprocaddress(ADSLDPCHandle,PAnsiChar('AllocADsStr'));
    @FreeADsStr:=getprocaddress(ADSLDPCHandle,PAnsiChar('FreeADsStr'));
    @ReallocADsStr:=getprocaddress(ADSLDPCHandle,PAnsiChar('ReallocADsStr'));
    @ADsEncodeBinaryData:=getprocaddress(ADSLDPCHandle,PAnsiChar('ADsEncodeBinaryData'));
  end;
  result:=Result and (ADSLDPCHandle<>0) and Assigned(ADsEncodeBinaryData);
end;

procedure FreeAD;
begin
  if (ADSIHandle<>0) then begin
    if not FreeLibrary(ADSIHandle) then
      raise Exception.Create(Format('Unload Error: %s - 0x%x',[ADSI,GetModuleHandle(ADSI)]))
    else
      ADSIHandle:=0;
  end;
  if (ADSLDPCHandle<>0) then begin
    if not FreeLibrary(ADSLDPCHandle) then
      raise Exception.Create(Format('Unload Error: %s - 0x%x',[ADSLDPC,GetModuleHandle(ADSLDPC)]))
    else
      ADSLDPCHandle:=0;
  end;
  ADSILoaded:=False;
end;

initialization
  ADSILoaded:=InitAD;
finalization
  FreeAD;

end.
