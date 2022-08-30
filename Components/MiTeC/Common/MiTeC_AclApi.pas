{*******************************************************}
{               MiTeC Common Routines                   }
{                Access Control API                     }
{                                                       }
{          Copyright (c) 2009-2013 Michal Mutl          }
{                                                       }
{*******************************************************}

{$INCLUDE Compilers.inc}

unit MiTeC_AclApi;

interface

uses{$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils;
     {$ELSE}
     Windows, SysUtils;
     {$ENDIF}

type
  _SE_OBJECT_TYPE = (
    SE_UNKNOWN_OBJECT_TYPE,
    SE_FILE_OBJECT,
    SE_SERVICE,
    SE_PRINTER,
    SE_REGISTRY_KEY,
    SE_LMSHARE,
    SE_KERNEL_OBJECT,
    SE_WINDOW_OBJECT,
    SE_DS_OBJECT,
    SE_DS_OBJECT_ALL,
    SE_PROVIDER_DEFINED_OBJECT,
    SE_WMIGUID_OBJECT,
    SE_REGISTRY_WOW64_32KEY);
  SE_OBJECT_TYPE = _SE_OBJECT_TYPE;
  TSeObjectType = SE_OBJECT_TYPE;

  PPSID = ^PSID;

  {PACL = ^ACL;
  _ACL = record
    AclRevision: Byte;
    Sbz1: Byte;
    AclSize: Word;
    AceCount: Word;
    Sbz2: Word;
  end;
  ACL = _ACL;
  TAcl = ACL;}

  PPACL = ^PAcl;

  TGetNamedSecurityInfo = function(pObjectName: LPTSTR; ObjectType: SE_OBJECT_TYPE;
    SecurityInfo: SECURITY_INFORMATION; ppsidOwner, ppsidGroup: PPSID; ppDacl,
    ppSacl: PPACL; var ppSecurityDescriptor: PSECURITY_DESCRIPTOR): DWORD; stdcall;

  TGetSecurityInfo = function(handle: THANDLE; ObjectType: SE_OBJECT_TYPE;
    SecurityInfo: SECURITY_INFORMATION; ppsidOwner: PPSID; ppsidGroup: PPSID;
    ppDacl, ppSacl: PPACL; var ppSecurityDescriptor: PSECURITY_DESCRIPTOR): DWORD; stdcall;

var
  GetNamedSecurityInfo: TGetNamedSecurityInfo;
  GetSecurityInfo: TGetSecurityInfo;

implementation

const
  AclAPI_DLL = 'advapi32.dll';

var
  AclAPIHandle: THandle;
  UnloadAclAPI: Boolean;

function InitAclAPI: Boolean;
begin
  AclAPIHandle:=GetModuleHandle(AclAPI_DLL);
  UnloadAclAPI:=AclAPIHandle=0;
  if AclAPIHandle=0 then
    AclAPIHandle:=loadlibrary(AclAPI_DLL);
  if AclAPIHandle<>0 then begin
    @GetNamedSecurityInfo:=GetProcAddress(AclAPIHandle,PChar('GetNamedSecurityInfoA'));
    @GetSecurityInfo:=GetProcAddress(AclAPIHandle,PChar('GetSecurityInfo'));
  end;
  result:=(AclAPIHandle<>0) and Assigned(GetNamedSecurityInfo);
end;

procedure FreeAclAPI;
begin
  if (AclAPIHandle<>0) and UnloadAclAPI then begin
    if not FreeLibrary(AclAPIHandle) then
      raise Exception.Create(Format('Unload Error: %s - 0x%x',[AclAPI_DLL,GetModuleHandle(AclAPI_DLL)]))
    else
      AclAPIHandle:=0;
  end;
end;

initialization
  InitACLAPI;
end.


