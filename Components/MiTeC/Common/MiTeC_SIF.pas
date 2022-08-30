{*******************************************************}
{                MiTeC Common Routines                  }
{               System Information File                 }
{                                                       }
{        Copyright (c) 1997-2016 Michal Mutl            }
{                                                       }
{*******************************************************}

{$I Compilers.inc}

unit MiTeC_SIF;

interface

uses {$IFDEF RAD9PLUS}
     WinApi.Windows, System.SysUtils;
     {$ELSE}
     Windows, SysUtils;
     {$ENDIF}

const
  cSIFVersion = 2;
  cSIFExt = '.sif';

type
  PStorageInfoRecord = ^TStorageInfoRecord;

  TStorageInfoRecord = record
    SIFVersion: Integer;
    SIFFormat: Integer;
    Machine: string;
    Title,
    Copyright,
    Application: string;
    OS,OSName,OSEdition: string;
    OSMajorVersion: Integer;
    OSMinorversion: Integer;
    OSBuildNumber: Integer;
    OSCSDVersion: string;
    Timestamp: TDateTime;
    Security: string;
    LoggedUser: string;
    LoggedUserSID: string;
    Account: string;
    Session: string;
    SessionID: Cardinal;
    Comment: string;
    IP: string;
    FileName: string;
    MD5: string;
    CSVersion: string;
    CSName: string;
    EXEBits: integer;
  end;

  TStorageData = array of TStorageInfoRecord;

procedure DefaultHeaderReader(const AFilename: string; var AHeader: TStorageInfoRecord);
procedure DefaultHeaderWriter(const AFilename: string; AHeader: TStorageInfoRecord);
function FindFileInStorageData(AData: TStoragedata; AFilename: string): integer;
function FindFileInStorageDataByMD5(AData: TStoragedata; AMD5: string): integer;
procedure DeleteFileRecord(var AData: TStoragedata; AIndex: Integer);
procedure AddFileRecord(var AData: TStorageData; AHeader: TStorageInfoRecord);

implementation

uses {$IFDEF RAD9PLUS}
     System.Win.ComObj, WinAPI.ActiveX,
     {$ELSE}
     ComObj, ActiveX,
     {$ENDIF}
     MiTeC_SS, MiTeC_Routines;

function FindFileInStorageData;
var
  i: Integer;
begin
  Result:=-1;
  for i:=0 to High(Adata) do
    if SameText(Adata[i].FileName,AFilename) then begin
      Result:=i;
      Break;
    end;
end;

function FindFileInStorageDataByMD5;
var
  i: Integer;
begin
  Result:=-1;
  for i:=0 to High(Adata) do
    if SameText(Adata[i].MD5,AMD5) then begin
      Result:=i;
      Break;
    end;
end;

procedure DeleteFileRecord;
var
  i: Integer;
begin
  for i:=AIndex+1 to High(AData) do
    AData[i-1]:=AData[i];
  SetLength(Adata,Length(AData)-1);
end;

procedure AddFileRecord;
begin
  SetLength(AData,Length(Adata)+1);
  AData[High(AData)]:=AHeader;
end;

procedure DefaultHeaderReader(const AFilename: string; var AHeader: TStorageInfoRecord);
var
  stg: IStorage;
  SS: TStructuredStorage;
  SPS: TStoragePropertySet;
  ps: IPropertyStorage;
begin
  if StgIsStorageFile(PWideChar(WideString(AFileName)))<>S_OK then
    Exit;
  OleCheck(StgOpenStorage(PWideChar(WideString(AFileName)),nil,STG_READ_INSTORAGE,nil,LongInt(nil),stg));
  SS:=TStructuredStorage.Create(nil,stg);
  try
    SPS:=nil;
    ps:=nil;
    if SS<>nil then
      SPS:=SS.OpenPropertySet(StringToGUID(FMTID_SummaryInformation),STG_READ_INSTORAGE,False);
    if SPS<>nil then
      ps:=SPS._IPropertyStorage;
    try
      try
        AHeader.SIFVersion:=ReadIntegerProperty(ps,'SIFVersion');
        AHeader.SIFFormat:=ReadIntegerProperty(ps,'SIFFormat');
        AHeader.Machine:=ReadStringProperty(ps,PIDSI_SUBJECT);
        AHeader.Application:=ReadStringProperty(ps,PIDSI_APPNAME);
        AHeader.Copyright:=ReadStringProperty(ps,PIDSI_AUTHOR);
        AHeader.Title:=ReadStringProperty(ps,PIDSI_TITLE);
        AHeader.OS:=ReadStringProperty(ps,PIDSI_KEYWORDS);
        AHeader.Timestamp:=ReadDatetimeProperty(ps,PIDSI_CREATE_DTM);
        AHeader.Comment:=ReadStringProperty(ps,PIDSI_COMMENTS);
        AHeader.LoggedUser:=ReadStringProperty(ps,'OS.LoggedUser');
        AHeader.LoggedUserSID:=ReadStringProperty(ps,'OS.LoggedUserSID');
        AHeader.Account:=ReadStringProperty(ps,'OS.Account');
        AHeader.OSMajorVersion:=ReadIntegerProperty(ps,'OS.MajorVersion');
        AHeader.OSMinorversion:=ReadIntegerProperty(ps,'OS.MinorVersion');
        AHeader.OSBuildNumber:=ReadIntegerProperty(ps,'OS.BuildNumber');
        AHeader.OSCSDVersion:=ReadStringProperty(ps,'OS.CSD');
        AHeader.OSName:=ReadStringProperty(ps,'OS.Name');
        AHeader.OSEdition:=ReadStringProperty(ps,'OS.Edition');
        AHeader.Session:=ReadStringProperty(ps,'Session');
        AHeader.SessionID:=ReadIntegerProperty(ps,'SessionID');
        AHeader.IP:=ReadStringProperty(ps,'IP');
        AHeader.Security:=ReadStringProperty(ps,PIDSI_SECURITY);
        AHeader.CSVersion:=ReadStringProperty(ps,'CS.Version');
        AHeader.CSName:=ReadStringProperty(ps,'CS.Name');
        AHeader.EXEBits:=ReadIntegerProperty(ps,'EXE.Bits');
      except
      end;
    finally
      if SPS<>nil then
        SPS.Free;
    end;
  finally
    SS.Free;
  end;
end;

procedure DefaultHeaderWriter(const AFilename: string; AHeader: TStorageInfoRecord);
var
  stg: IStorage;
  SS: TStructuredStorage;
  SPS: TStoragePropertySet;
begin
  if StgIsStorageFile(PWideChar(WideString(AFileName)))<>S_OK then
    OleCheck(StgCreateDocFile(PWideChar(WideString(AFileName)),STG_CREATE_OPEN,0,stg))
  else
    OleCheck(StgOpenStorage(PWideChar(WideString(AFileName)),nil,STG_OPEN,nil,LongInt(nil),stg));
  SS:=TStructuredStorage.Create(nil,stg);
  try
    try (SS._IStorage as IPropertySetStorage).Delete(StringToGUID(FMTID_SummaryInformation)) except end;
    SPS:=SS.OpenPropertySet(StringToGUID(FMTID_SummaryInformation),STG_OPEN,True);
    try
      WriteStringProperty(SPS._IPropertyStorage,PIDSI_TITLE,ModuleInfo.ProductName);
      WriteStringProperty(SPS._IPropertyStorage,PIDSI_APPNAME,Format('%s %s',[ModuleInfo.ProductName,ModuleInfo.FileVersion]));
      WriteStringProperty(SPS._IPropertyStorage,PIDSI_AUTHOR,Format('%s',[ModuleInfo.Copyright]));
      WriteStringProperty(SPS._IPropertyStorage,PIDSI_COMMENTS,AHeader.Comment);
      WriteStringProperty(SPS._IPropertyStorage,PIDSI_KEYWORDS,Format('%s %d.%d.%d %s',[OSName,OSVIX.dwMajorVersion,OSVIX.dwMinorVersion,OSVIX.dwBuildNumber,OSEdition]));
      WriteStringProperty(SPS._IPropertyStorage,PIDSI_SUBJECT,Format('%s',[MachineName]));
      WriteDateTimeProperty(SPS._IPropertyStorage,PIDSI_CREATE_DTM,now);
      if IsAdmin then
        WriteStringProperty(SPS._IPropertyStorage,PIDSI_SECURITY,'Admin')
      else
        WriteStringProperty(SPS._IPropertyStorage,PIDSI_SECURITY,'');

      WriteIntegerProperty(SPS._IPropertyStorage,'SIFVersion',cSIFVersion);
      WriteIntegerProperty(SPS._IPropertyStorage,'SIFFormat',AHeader.SIFFormat);
      WriteStringProperty(SPS._IPropertyStorage,'OS.LoggedUser',AHeader.LoggedUser);
      WriteStringProperty(SPS._IPropertyStorage,'OS.LoggedUserSID',AHeader.LoggedUserSID);
      WriteStringProperty(SPS._IPropertyStorage,'OS.Account',GetUser);
      WriteIntegerProperty(SPS._IPropertyStorage,'OS.Platform',Win32Platform);
      WriteIntegerProperty(SPS._IPropertyStorage,'OS.MajorVersion',OSVIX.dwMajorVersion);
      WriteIntegerProperty(SPS._IPropertyStorage,'OS.MinorVersion',OSVIX.dwMinorVersion);
      WriteIntegerProperty(SPS._IPropertyStorage,'OS.BuildNumber',OSVIX.dwBuildNumber);
      WriteStringProperty(SPS._IPropertyStorage,'OS.Name',AHeader.OSName);
      WriteStringProperty(SPS._IPropertyStorage,'OS.Edition',AHeader.OSEdition);
      WriteStringProperty(SPS._IPropertyStorage,'OS.CSD',Trim(OSVIX.szCSDVersion));
      WriteIntegerProperty(SPS._IPropertyStorage,'ParentProcessPID',GetCurrentProcessId);
      WriteStringProperty(SPS._IPropertyStorage,'Session',AHeader.Session);
      WriteIntegerProperty(SPS._IPropertyStorage,'SessionID',AHeader.SessionID);
      WriteStringProperty(SPS._IPropertyStorage,'IP',AHeader.IP);
      WriteStringProperty(SPS._IPropertyStorage,'CS.Name',AHeader.CSName);
      WriteStringProperty(SPS._IPropertyStorage,'CS.Version',AHeader.CSVersion);
      {$IFDEF WIN64}
      WriteIntegerProperty(SPS._IPropertyStorage,'EXE.Bits',64);
      {$ELSE}
      WriteIntegerProperty(SPS._IPropertyStorage,'EXE.Bits',32);
      {$ENDIF}
    finally
      SPS.Free;
    end;
  finally
    SS.Free;
  end;
end;


end.
