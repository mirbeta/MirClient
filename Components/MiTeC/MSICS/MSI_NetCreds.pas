{*******************************************************}
{                                                       }
{       MiTeC System Information Component Suite        }
{               Network Credentials                     }
{                version 13.4.0                         }
{                                                       }
{        Copyright (c) 1997-2019 Michal Mutl            }
{                                                       }
{*******************************************************}

{$INCLUDE Compilers.Inc}

unit MSI_NetCreds;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils, System.Classes,
     WinAPI.ActiveX, System.Win.ComObj, MiTeC_SS,
     {$ELSE}
     Windows, SysUtils, Classes, ActiveX, ComObj, MiTeC_SS,
     {$ENDIF}
     MSI_Common, MiTeC_Windows, MSI_Defs;

const
  StorageFolderName = 'NetCredentials';

type
  TCredRecord = record
    Typ: Cardinal;
    Timestamp: TDateTime;
    Target,
    UserName,
    Password: string;
  end;

  TCredData = array of TCredRecord;

  TMiTeC_NetCreds = class(TMiTeC_Component)
  private
    FData: TCredData;
    function GetCount: Cardinal;
    function GetRecord(Index: Cardinal): TCredRecord;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SaveToStorage(const AFilename: string; var AWriteHeader: Boolean; AFormat: integer = 0; const AComment: string=''; ACodeStream: TCodeStreamProcedure = nil); override;
    function LoadFromStorage(const AFilename: string; var AReadHeader: boolean; ACodeStream: TCodeStreamProcedure = nil): boolean; override;
    procedure RefreshData(AScanObjects: TScanObjects = soAll); override;
    procedure Clear; override;

    property RecordCount: Cardinal read GetCount;
    property Records[Index: Cardinal]: TCredRecord read GetRecord;
  published
  end;

implementation

uses MiTeC_WinCrypt, MiTeC_Datetime;

const
  pwd1 = '82BD0E67-9FEA-4748-8672-D5EFE5B779B0';
  pwd2 = 'abe2869f-9b47-4cd9-a358-c22904dba7f7';

type
  PCREDENTIAL_ATTRIBUTEA = ^CREDENTIAL_ATTRIBUTEA;
  _CREDENTIAL_ATTRIBUTEA = record
    Keyword: LPSTR;
    Flags: DWORD;
    ValueSize: DWORD;
    Value: LPBYTE;
  end;
  CREDENTIAL_ATTRIBUTEA = _CREDENTIAL_ATTRIBUTEA;
  TCredentialAttributeA = CREDENTIAL_ATTRIBUTEA;
  PCredentialAttributeA = PCREDENTIAL_ATTRIBUTEA;

  PCREDENTIAL_ATTRIBUTEW = ^CREDENTIAL_ATTRIBUTEW;
  _CREDENTIAL_ATTRIBUTEW = record
    Keyword: LPWSTR;
    Flags: DWORD;
    ValueSize: DWORD;
    Value: LPBYTE;
  end;
  CREDENTIAL_ATTRIBUTEW = _CREDENTIAL_ATTRIBUTEW;
  TCredentialAttributeW = CREDENTIAL_ATTRIBUTEW;
  PCredentialAttributeW = PCREDENTIAL_ATTRIBUTEW;

  {$IFDEF UNICODE}
  CREDENTIAL_ATTRIBUTE = CREDENTIAL_ATTRIBUTEW;
  PCREDENTIAL_ATTRIBUTE = PCREDENTIAL_ATTRIBUTEW;
  TCredentialAttribute = TCredentialAttributeW;
  PCredentialAttribute = PCredentialAttributeW;
  {$ELSE}
  CREDENTIAL_ATTRIBUTE = CREDENTIAL_ATTRIBUTEA;
  PCREDENTIAL_ATTRIBUTE = PCREDENTIAL_ATTRIBUTEA;
  TCredentialAttribute = TCredentialAttributeA;
  PCredentialAttribute = PCredentialAttributeA;
  {$ENDIF UNICODE}

  PCREDENTIALA = ^CREDENTIALA;
  _CREDENTIALA = record
    Flags: DWORD;
    Type_: DWORD;
    TargetName: LPSTR;
    Comment: LPSTR;
    LastWritten: FILETIME;
    CredentialBlobSize: DWORD;
    CredentialBlob: LPBYTE;
    Persist: DWORD;
    AttributeCount: DWORD;
    Attributes: PCREDENTIAL_ATTRIBUTEA;
    TargetAlias: LPSTR;
    UserName: LPSTR;
  end;
  CREDENTIALA = _CREDENTIALA;
  TCredentialA = CREDENTIALA;

  PCREDENTIALW = ^CREDENTIALW;
  _CREDENTIALW = record
    Flags: DWORD;
    Type_: DWORD;
    TargetName: LPWSTR;
    Comment: LPWSTR;
    LastWritten: FILETIME;
    CredentialBlobSize: DWORD;
    CredentialBlob: LPBYTE;
    Persist: DWORD;
    AttributeCount: DWORD;
    Attributes: PCREDENTIAL_ATTRIBUTEW;
    TargetAlias: LPWSTR;
    UserName: LPWSTR;
  end;
  CREDENTIALW = _CREDENTIALW;
  TCredentialW = CREDENTIALW;

  {$IFDEF UNICODE}
  CREDENTIAL = CREDENTIALW;
  PCREDENTIAL = PCREDENTIALW;
  TCredential = TCredentialW;
  {$ELSE}
  CREDENTIAL = CREDENTIALA;
  PCREDENTIAL = PCREDENTIALA;
  TCredential = TCredentialA;
  {$ENDIF UNICODE}

  {$IFDEF UNICODE}
  PCREDENTIALS = array of PCREDENTIALW;
  {$ELSE}
  PCREDENTIALS = array of PCREDENTIALA;
  {$ENDIF}

  TCredEnumerateW = function(Filter: LPCWSTR; Flags: DWORD; var Count: DWORD; var Credential: PCREDENTIALW): BOOL; stdcall;
  TCredEnumerateA = function(Filter: LPCSTR; Flags: DWORD; var Count: DWORD; var Credential: PCREDENTIALA): BOOL; stdcall;
  TCredFree = procedure(Buffer: PVOID); stdcall;

const
  credapi = 'advapi32.dll';

var
  _CredEnumerateW: TCredEnumerateW;
  _CredEnumerateA: TCredEnumerateA;
  _CredFree: TCredFree;


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

function CredEnumerateW(Filter: LPCWSTR; Flags: DWORD; var Count: DWORD; var Credential: PCREDENTIALW): BOOL; stdcall;
begin
  GetProcedureAddress(Pointer(@_CredEnumerateW), credapi, 'CredEnumerateW');
  if Assigned(_CredEnumerateW) then
    Result:=_CredEnumerateW(Filter,Flags,Count,Credential)
  else
    Result:=False;
end;

function CredEnumerateA(Filter: LPCSTR; Flags: DWORD; var Count: DWORD; var Credential: PCREDENTIALA): BOOL; stdcall;
begin
  GetProcedureAddress(Pointer(@_CredEnumerateA), credapi, 'CredEnumerateA');
  if Assigned(_CredEnumerateA) then
    Result:=_CredEnumerateA(Filter,Flags,Count,Credential)
  else
    Result:=False;
end;

{$IFDEF UNICODE}
function CredEnumerate(Filter: LPCWSTR; Flags: DWORD; var Count: DWORD; var Credential: PCREDENTIALW): BOOL; stdcall;
begin
  Result:=CredEnumerateW(Filter,Flags,Count,Credential)
end;
{$ELSE}
function CredEnumerate(Filter: LPCSTR; Flags: DWORD; var Count: DWORD; var Credential: PCREDENTIALA): BOOL; stdcall;
begin
  Result:=CredEnumerateA(Filter,Flags,Count,Credential)
end;
{$ENDIF}

procedure CredFree(Buffer: PVOID); stdcall;
begin
  GetProcedureAddress(Pointer(@_CredFree), credapi, 'CredFree');
  if Assigned(_CredFree) then
    _CredFree(Buffer);
end;

{ TMiTeC_NetCreds }

procedure TMiTeC_NetCreds.Clear;
begin
  Finalize(FData);
end;

constructor TMiTeC_NetCreds.Create(AOwner: TComponent);
begin
  inherited;

end;

destructor TMiTeC_NetCreds.Destroy;
begin
  Finalize(FData);
  inherited;
end;

function TMiTeC_NetCreds.GetCount: Cardinal;
begin
  Result:=Length(FData);
end;

function TMiTeC_NetCreds.GetRecord(Index: Cardinal): TCredRecord;
begin
  Result:=FData[Index];
end;

function TMiTeC_NetCreds.LoadFromStorage;

procedure ParseRecord(ASource: string; var ARecord: TCredRecord);
var
  p: Integer;
begin
  Finalize(Arecord);
  ZeroMemory(@ARecord,SizeOf(ARecord));
  p:=Pos(';',ASource);
  if p=0 then
    Exit;
  ARecord.Typ:=StrToIntDef(Copy(ASource,1,p-1),0);
  Delete(ASource,1,p);
  p:=Pos(';',ASource);
  ARecord.Timestamp:=StrToIntdef(Copy(ASource,1,p-1),0);
  Delete(ASource,1,p);
  p:=Pos(';',ASource);
  ARecord.Target:=Copy(ASource,1,p-1);
  Delete(ASource,1,p);
  p:=Pos(';',ASource);
  ARecord.Username:=Copy(ASource,1,p-1);
  Delete(ASource,1,p);
  ARecord.Password:=ASource;
end;

var
  stg: IStorage;
  SS: TStructuredStorage;
  Sub: TStructuredStorage;
  strm: TStorageStream;
  i: Integer;
  sl: TStringList;
  r: TCredRecord;
  ds: char;
begin
  Sub:=nil;
  ds:={$IFDEF RAD8PLUS}FormatSettings.{$ENDIF}DecimalSeparator;
  {$IFDEF RAD8PLUS}FormatSettings.{$ENDIF}DecimalSeparator:='.';
  Finalize(FData);
  Result:=inherited LoadFromStorage(AFilename,AReadHeader,ACodeStream);
  if StgIsStorageFile(PWideChar(WideString(AFileName)))<>S_OK then
    Exit;
  OleCheck(StgOpenStorage(PWideChar(WideString(AFileName)),nil,STG_READ_INSTORAGE,nil,LongInt(nil),stg));
  SS:=TStructuredStorage.Create(nil,stg);
  sl:=TStringList.Create;
  try
    Result:=False;
    try
      Sub:=SS.OpenSubStorage(StorageFolderName,STG_READ_INSTORAGE,False);
    except
      Sub:=nil;
    end;
    if Sub<>nil then begin
      strm:=Sub.OpenStream(strm_Data,STG_READ_INSTORAGE,False);
      if strm=nil then
        Exit;
      try
        LoadFromEncodedStream(strm,sl,ACodeStream);
        for i:=0 to sl.Count-1 do begin
          ParseRecord(sl[i],r);
          SetLength(FData,Length(FData)+1);
          FData[High(FData)]:=r;
        end;
        SetDataAvail(True);
      finally
        strm.Free;
      end;
    end;
  finally
    {$IFDEF RAD8PLUS}FormatSettings.{$ENDIF}DecimalSeparator:=ds;
    sl.Free;
    if Sub<>nil then
      Sub.Free;
    SS.Free;
  end;
end;

procedure TMiTeC_NetCreds.RefreshData(AScanObjects: TScanObjects = soAll);
var
  Cred: PCREDENTIAL;
  ca: PCredentials;
  DataIn, DataOE, DataOut: DATA_BLOB;
  c: Cardinal;
  v,i: Integer;
  ed1,ed2: array[0..36] of byte;
begin
  Finalize(FData);
  Cred:=nil;
  if not CredEnumerate(nil,0,c,Cred) then
    if (GetLastError<>0) then
      Exit;

  for i:=1 to Length(pwd1) do begin
    v:=Ord(pwd1[i]) shl 2;
    if v>255 then
      v:=255;
    ed1[i-1]:=v;
    v:=Round(Ord(pwd2[i])*4);
    if v>255 then
      v:=255;
    ed2[i-1]:=v;
  end;

  DataOE.cbData:=74;
  DataOE.pbData:=PByte(@ed1);

  SetLength(ca,c);
  Move(Cred^,ca[0],c*sizeof(PCREDENTIAL));
  try
    for i:=0 to c-1 do begin
      SetLength(FData,Length(FData)+1);
      with FData[High(FData)] do begin
        Typ:=ca[i].Type_;
        try Timestamp:=FileTimeToDatetime(FILETIME(ca[i].LastWritten),ConvertTimeToLocal); except end;
        Target:=ca[i].TargetName;
        UserName:=ca[i].UserName;

        DataIn.pbData:=ca[i].CredentialBlob;
        DataIn.cbData:=ca[i].CredentialBlobSize;
        if CryptUnprotectData(@DataIn,nil,@DataOE,nil,nil,0,@DataOut) then
          Password:=PWideChar(DataOut.pbData)
        else if CryptUnprotectData(@DataIn,nil,nil,nil,nil,CRYPTPROTECT_UI_FORBIDDEN,@DataOut) then begin
          Password:=string(PAnsiChar(DataOut.pbData));
          if Length(Password)=1 then
            Password:=string(PWideChar(DataOut.pbData));
        end else begin
          Password:=string(PAnsiChar(ca[i].CredentialBlob));
          if Length(Password)=1 then
            Password:=PWideChar(ca[i].CredentialBlob);
        end;
      end;
    end;
  finally
    CredFree(Cred);
  end;
end;

procedure TMiTeC_NetCreds.SaveToStorage;
var
  stg: IStorage;
  SS: TStructuredStorage;
  Sub: TStructuredStorage;
  strm: TStorageStream;
  sl: TStringList;
  i: Integer;
  ds: char;
begin
  inherited SaveToStorage(AFilename,AWriteHeader,AFormat,AComment,ACodeStream);
  if StgIsStorageFile(PWideChar(WideString(AFileName)))<>S_OK then
    OleCheck(StgCreateDocFile(PWideChar(WideString(AFileName)),STG_CREATE_OPEN,0,stg))
  else
    OleCheck(StgOpenStorage(PWideChar(WideString(AFileName)),nil,STG_OPEN,nil,LongInt(nil),stg));
  SS:=TStructuredStorage.Create(nil,stg);
  Sub:=SS.OpenSubStorage(StorageFolderName,STG_OPEN,True);
  try
    Sub.DeleteElement(strm_Data);
    strm:=Sub.OpenStream(strm_Data,STG_OPEN,True);
    try
      sl:=TStringList.Create;
      ds:={$IFDEF RAD8PLUS}FormatSettings.{$ENDIF}DecimalSeparator;
      {$IFDEF RAD8PLUS}FormatSettings.{$ENDIF}DecimalSeparator:='.';
      try
        for i:=0 to High(FData) do
          sl.Add(Format('%d;%1.10f;%s;%s;%s',[FData[i].Typ,
                                   FData[i].Timestamp,
                                   FData[i].Target,
                                   FData[i].Username,
                                   FData[i].Password
                                  ]));
        SaveToEncodedStream(sl,strm,ACodeStream);
      finally
        {$IFDEF RAD8PLUS}FormatSettings.{$ENDIF}DecimalSeparator:=ds;
        sl.Free;
      end;
    finally
      strm.Free;
    end;
  finally
    Sub.Free;
    try
      SS.Free;
    except
    end;
  end;
end;

end.

