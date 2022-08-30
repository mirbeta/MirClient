{*******************************************************}
{       MiTeC System Information Component Suite        }
{          Common Routines, Definitions & Types         }
{                version 13.4.0                         }
{                                                       }
{        Copyright (c) 1997-2019 Michal Mutl            }
{                                                       }
{*******************************************************}

{$INCLUDE Compilers.inc}


unit MSI_Common;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils, System.Classes,
     {$ELSE}
     Windows, SysUtils, Classes,
     {$ENDIF}
     MiTeC_SS, MiTeC_SIF,
     MiTeC_Routines, MSI_Defs;

const
  strm_Props = 'Properties';
  strm_Item = 'Item_%d';
  strm_IP = 'IP_%d';
  strm_Data = 'Data';

type
  TCodeStreamProcedure = procedure(InStream, OutStream: TStream);

  TMiTeC_Component = class(TComponent)
  private
    FAbout: string;
    FRefresh: string;
    FFilename: string;
    FLiveData: Boolean;
    FSIR: TStorageInfoRecord;
    FHeaderReader: THeaderReader;
    FHeaderWriter: THeaderWriter;
    FDA: Boolean;
    FSID: string;
    FLU: string;
    FST: TSessionTypes;
    FSessionID: Cardinal;
    FCS: TCodeStreamProcedure;
    FCTTL: Boolean;
  protected
    procedure SetStorageName(AValue: string);
    procedure SetLiveData(AValue: Boolean);
    procedure SetDataAvail(AValue: Boolean);
    procedure SetHeaderReader(const Value: THeaderReader); virtual;
    procedure SetHeaderWriter(const Value: THeaderWriter); virtual;
    procedure SetStorageInfo(ARecord: TStorageInfoRecord);
  public
    constructor Create(AOwner: TComponent); override;
    procedure RefreshData(AScanObjects: TScanObjects = soAll); virtual;
    procedure Clear; virtual; abstract;

    procedure SetSession(ASessionID: Cardinal);

    procedure SaveToStorage(const AFilename: string; var AWriteHeader: Boolean; AFormat: integer = 0; const AComment: string=''; ACodeStream: TCodeStreamProcedure = nil); virtual;
    function LoadFromStorage(const AFilename: string; var AReadHeader: boolean; ACodeStream: TCodeStreamProcedure = nil): Boolean; virtual;
    procedure LoadStorageInfo(const AFilename: string; AHeader: Pointer); virtual;

    property StorageFilename: string read FFilename;
    property StorageInfo: TStorageInfoRecord read FSIR;

    property StreamCodeProc: TCodeStreamProcedure read FCS;

    property ConvertTimeToLocal: Boolean read FCTTL write FCTTL;
    property LoggedUser: string read FLU;
    property SID: string read FSID;
    property SessionID: Cardinal read FSessionID;
    property Session: TSessionTypes read FST;

    property DataAvailable: Boolean read FDA;
  published
    property _About :string read FAbout stored False;
    {$IFNDEF FPC}
    property _Refresh :string read FRefresh stored False;
    {$ENDIF}
    property LiveData: Boolean read FLiveData;

    property OnWriteHeader: THeaderWriter read FHeaderWriter write SetHeaderWriter;
    property OnReadHeader: THeaderReader read FHeaderReader write SetHeaderReader;
  end;

implementation

uses {$IFDEF RAD9PLUS}
     WinAPI.ActiveX, System.Win.ComObj,
     {$IFDEF TRIAL}VCL.Dialogs,{$ENDIF}
     {$ELSE}
     ActiveX, ComObj,
     {$IFDEF TRIAL}Dialogs,{$ENDIF}
     {$ENDIF}
     MiTeC_SysUtils;

{ TMiTeC_Component }

procedure TMiTeC_Component.SetDataAvail(AValue: Boolean);
begin
  FDA:=AValue;
end;

procedure TMiTeC_Component.SetHeaderReader(const Value: THeaderReader);
begin
  FHeaderReader:=Value;
end;

procedure TMiTeC_Component.SetHeaderWriter(const Value: THeaderWriter);
begin
  FHeaderWriter:=Value;
end;

procedure TMiTeC_Component.SaveToStorage;
begin
  if not LiveData then
    Abort;
  {$IFDEF TRIAL}
  if not RunFromIDE then
    MessageDlg(Self.ClassName+sLineBreak+cCompName+sLineBreak+cCopyright,mtInformation,[mbOK],0);
  {$ENDIF}
  if AWriteHeader then begin
    if Assigned(OnWriteHeader) then
      OnWriteHeader(AFilename,AFormat,AComment)
    else begin
      FSIR.SIFFormat:=AFormat;
      FSIR.Comment:=AComment;
      FSIR.LoggedUser:=GetLoggedUser;
      FSIR.LoggedUserSID:=GetSIDFromAccount('',FSIR.LoggedUser);
      DefaultHeaderWriter(AFilename,FSIR);
    end;
    AWriteHeader:=False;
  end;
end;

procedure TMiTeC_Component.SetSession(ASessionID: Cardinal);
begin
  FSessionID:=ASessionID;
  FLU:=GetLoggedUser(ASessionID);
  FSID:=GetSIDFromAccount('',FLU);

  FST:=GetSession;
end;

procedure TMiTeC_Component.SetStorageInfo(ARecord: TStorageInfoRecord);
begin
  FSIR:=ARecord;
end;

procedure TMiTeC_Component.SetStorageName(AValue: string);
begin
  FFilename:=AValue;
end;

procedure TMiTeC_Component.SetLiveData(AValue: Boolean);
begin
  FLiveData:=AValue;
end;


constructor TMiTeC_Component.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCTTL:=True;
  FAbout:=cCompName+' '+cVersion+{$IFDEF TRIAL}' TRIAL'+{$ENDIF}' - '+cCopyright;
  FRefresh:='Double-click to get data';
  FLiveData:=True;
  FFileName:='';
  FDA:=False;
  FLU:=GetLoggedUser;
  FSessionID:=Cardinal(-1);
  FSID:=GetSIDFromAccount('',FLU);
  FST:=GetSession;

  Self.FSIR.OSMajorVersion:=Win32MajorVersion;
  Self.FSIR.OSMinorversion:=Win32MinorVersion;
  Self.FSIR.OSBuildNumber:=Win32BuildNumber;
  Self.FSIR.OSCSDVersion:=Win32CSDVersion;
  Self.FSIR.CSVersion:=cVersion;
  Self.FSIR.CSName:=cCompName;
  Self.FSIR.OSName:=OSName;
  Self.FSIR.OSEdition:=OSEdition;
  Self.FSIR.SessionID:=FSessionID;
  Self.FSIR.Session:=GetSessionStr(FST);
  {$IFDEF WIN64}
  Self.FSIR.EXEBits:=64;
  {$ELSE}
  Self.FSIR.EXEBits:=32;
  {$ENDIF}
  FHeaderWriter:=nil;
  FHeaderReader:=nil;
end;

function TMiTeC_Component.LoadFromStorage;
begin
  {$IFDEF TRIAL}
  if not RunFromIDE then
    MessageDlg(Self.ClassName+sLineBreak+cCompName+sLineBreak+cCopyright,mtInformation,[mbOK],0);
  {$ENDIF}
  FCS:=ACodeSTream;
  FDA:=False;
  FFilename:=AFilename;
  FLiveData:=False;
  if AReadHeader then begin
    LoadStorageInfo(AFilename,Pointer(@FSIR));
    AReadHeader:=False;
  end;
  Result:=True;
end;

procedure TMiTeC_Component.LoadStorageInfo(const AFilename: string; AHeader: Pointer);
begin
  if Assigned(OnReadHeader) then
    OnReadHeader(AFilename,AHeader)
  else
    DefaultHeaderReader(AFilename,PStorageInfoRecord(AHeader)^);
end;

procedure TMiTeC_Component.RefreshData;
begin
  SetLiveData(True);
  Self.FSIR.OSMajorVersion:=Win32MajorVersion;
  Self.FSIR.OSMinorversion:=Win32MinorVersion;
  Self.FSIR.OSBuildNumber:=Win32BuildNumber;
  Self.FSIR.OSCSDVersion:=Win32CSDVersion;
  Self.FSIR.CSVersion:=cVersion;
  Self.FSIR.CSName:=cCompName;
  FSIR.IP:=Trim(GetLocalIP);
  {$IFDEF WIN64}
  Self.FSIR.EXEBits:=64;
  {$ELSE}
  Self.FSIR.EXEBits:=32;
  {$ENDIF}
  {$IFDEF TRIAL}
  if (csDesigning in ComponentState) or RunFromIDE then
    Exit;
  MessageDlg(Self.ClassName+sLineBreak+cCompName+sLineBreak+cCopyright,mtInformation,[mbOK],0);
  {$ENDIF}
end;

end.






