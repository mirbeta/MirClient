{*******************************************************}
{                                                       }
{           System Information Component                }
{             Drive Content Component                   }
{                version 13.4.0                         }
{                                                       }
{        Copyright (c) 1997-2019 Michal Mutl            }
{                                                       }
{*******************************************************}

{$INCLUDE Compilers.inc}


unit MSI_DriveContent;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils, System.Classes,
     WinAPI.ActiveX, System.Win.ComObj,
     {$ELSE}
     Windows, SysUtils, Classes, ActiveX, ComObj,
     {$ENDIF}
     MiTeC_SS, MSI_Common, MSI_Defs, MiTeC_Routines;

const
  StorageFolderName = 'DriveContent';

type
  TFileRecord = record
    ObjectName: string;
    Accessed,
    Created,
    Modified: TDateTime;
    Size: Int64;
    Attrs: integer;
    MD5: string;
    Metadata: string;
  end;

  TOnFileEvent = procedure (Sender: TObject; var FileInfo: TFileRecord; var Accept: boolean) of object;

  TMiTeC_DriveContent = class(TMiTeC_Component)
  private
    //FResult: TScanResult;
    FContent: TStrings;
    FAvailDisks: string;
    FDrive: string;
    FMD5: TStringList;
    FOF: TOnFileEvent;
    FSD: string;
    FOD: TOnFileEvent;
    FLFI: TFileRecord;
    procedure SetMD5(const Value: string);
    function GetMD5: string;
    procedure DoScanPath(APath: string);
    function AddRecord(AName: string; FileInfo: TFileRecord): Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear; override;
    procedure RefreshData(AScanObjects: TScanObjects = soAll); override;
    procedure SaveToStorage(const AFilename: string; var AWriteHeader: Boolean; AFormat: integer = 0; const AComment: string=''; ACodeStream: TCodeStreamProcedure = nil); override;
    function LoadFromStorage(const AFilename: string; var AReadHeader: boolean; ACodeStream: TCodeStreamProcedure = nil): boolean; override;
    function ParseRecord(ARecord: string): TFileRecord;
    procedure Scan(ADisk: string; AClear: Boolean = True);
    procedure ScanPath(APath: string; AClear: Boolean = False);
    property ScannedDrives: string read FSD;
  published
    property CurrentDrive: string read FDrive;
    property AvailableDisks: string read FAvailDisks stored False;
    property Content: TStrings read FContent stored False;
    property MD5Extensions: string read GetMD5 write SetMD5;
    property OnFile: TOnFileEvent read FOF write FOF;
    property OnFolder: TOnFileEvent read FOD write FOD;
  end;

function FormatAttributes(AAttr: Integer): string;

implementation

uses
  MiTeC_StrUtils, MiTeC_Datetime, MiTeC_WinCrypt;

function FormatAttributes(AAttr: Integer): string;
begin
  Result:='-----';
  if AAttr and faDirectory = faDirectory then
    Result[5]:='d';
  if AAttr and faReadOnly = faReadOnly then
    Result[1]:='r';
  if AAttr and faArchive = faArchive then
    Result[2]:='a';
  if AAttr and faHidden = faHidden then
    Result[3]:='h';
  if AAttr and faSysFile = faSysFile then
    Result[4]:='s';
end;

{ TMiTeC_DriveContent }

procedure TMiTeC_DriveContent.Clear;
begin
  FContent.Clear;
  FAvailDisks:='';
  FSD:='';
end;

constructor TMiTeC_DriveContent.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FContent:=TStringList.Create;
  FMD5:=TStringList.Create;
end;

destructor TMiTeC_DriveContent.Destroy;
begin
  FContent.Free;
  FMD5.Free;
  //Finalize(FResult);
  inherited;
end;

procedure TMiTeC_DriveContent.SetMD5(const Value: string);
begin
  FMD5.CommaText:=Value;
end;

function TMiTeC_DriveContent.LoadFromStorage;
var
  i: Integer;
  stg: IStorage;
  SS: TStructuredStorage;
  Sub: TStructuredStorage;
  strm: TStorageStream;
  s: string;
begin
  strm:=nil;
  Sub:=nil;
  FContent.Clear;
  Result:=inherited LoadFromStorage(AFilename,AReadHeader,ACodeStream);
  if StgIsStorageFile(PWideChar(WideString(AFileName)))<>S_OK then
    Exit;
  OleCheck(StgOpenStorage(PWideChar(WideString(AFileName)),nil,STG_READ_INSTORAGE,nil,LongInt(nil),stg));
  SS:=TStructuredStorage.Create(nil,stg);
  try
    Result:=False;
    try
      Sub:=SS.OpenSubStorage(StorageFolderName,STG_READ_INSTORAGE,False);
    except
      Sub:=nil;
    end;
    if Sub<>nil then begin
      FAvailDisks:='';
      for i:=0 to Sub.ElementCount-1 do
        if (Sub.Elements[i].Props.dwType=STGTY_STREAM) and (Length(Sub.Elements[i].Name)=1) then
          Self.FAvailDisks:=Self.FAvailDisks+Sub.Elements[i].Name;
      s:=Copy(FDrive,1,1);
      if PosText(s,Self.FAvailDisks)>0 then
        strm:=Sub.OpenStream(s,STG_READ_INSTORAGE,False);
      if strm=nil then
        Exit;
      try
        LoadFromEncodedStream(strm,FContent,ACodeStream);
        SetDataAvail(True);
      finally
        strm.Free;
      end;
    end;
  finally
    if Sub<>nil then
      Sub.Free;
    SS.Free;
  end;
end;

function TMiTeC_DriveContent.ParseRecord(ARecord: string): TFileRecord;
var
  ds: Char;
begin
  ds:={$IFDEF RAD8PLUS}FormatSettings.{$ENDIF}DecimalSeparator;
  {$IFDEF RAD8PLUS}FormatSettings.{$ENDIF}DecimalSeparator:='.';
  try
    Result.ObjectName:=GetCSVData(ARecord,0);
    Result.Size:=StrToIntDef(GetCSVData(ARecord,4),0);
    Result.Attrs:=StrToIntDef(GetCSVData(ARecord,5),0);
    Result.Created:=StrToFloatDef(GetCSVData(ARecord,1),0);
    Result.Modified:=StrToFloatDef(GetCSVData(ARecord,2),0);
    Result.Accessed:=StrToFloatDef(GetCSVData(ARecord,3),0);
    Result.MD5:=GetCSVData(ARecord,6);
  finally
    {$IFDEF RAD8PLUS}FormatSettings.{$ENDIF}DecimalSeparator:=ds;
  end;
end;

procedure TMiTeC_DriveContent.RefreshData;
var
  i,n :integer;
  buf :PChar;
begin
  inherited;
  Clear;
  buf:=stralloc(255);
  n:=GetLogicalDriveStrings(255,buf);
  for i:=0 to n do
    if buf[i]<>#0 then begin
      if (ord(buf[i]) in [$41..$5a]) or (ord(buf[i]) in [$61..$7a]) then
        FAvailDisks:=FAvailDisks+upcase(buf[i])
    end else
      if buf[i+1]=#0 then
        break;
  strdispose(buf);
  SetDataAvail(True);
end;

procedure TMiTeC_DriveContent.SaveToStorage;
var
  stg: IStorage;
  SS: TStructuredStorage;
  Sub: TStructuredStorage;
  strm: TStorageStream;
begin
  Sub:=nil;
  inherited SaveToStorage(AFilename,AWriteHeader,AFormat,AComment,ACodeStream);
  if StgIsStorageFile(PWideChar(WideString(AFileName)))<>S_OK then
    OleCheck(StgCreateDocFile(PWideChar(WideString(AFileName)),STG_CREATE_OPEN,0,stg))
  else
    OleCheck(StgOpenStorage(PWideChar(WideString(AFileName)),nil,STG_OPEN,nil,LongInt(nil),stg));
  SS:=TStructuredStorage.Create(nil,stg);
  try
    Sub:=SS.OpenSubStorage(StorageFolderName,STG_OPEN,True);
    Sub.DeleteElement(FDrive);
    strm:=Sub.OpenStream(Copy(FDrive,1,1),STG_OPEN,True);
    try
      SaveToEncodedStream(FContent,strm,ACodeStream);
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

function TMiTeC_DriveContent.AddRecord;
var
  A: Boolean;
begin
  A:=True;
  Result:=not SameText('.',AName) and not SameText('..',AName);
  if Result then begin
    if Assigned(FOF) and (FileInfo.Attrs and faDirectory=0) then
      FOF(Self,FileInfo,A);
    if A then
      FContent.Add(Format('%s;%1.10f;%1.10f;%1.10f;%d;%d;%s',[
                             FileInfo.ObjectName,
                             FileInfo.Created,
                             FileInfo.Accessed,
                             FileInfo.Modified,
                             FileInfo.Size,
                             FileInfo.Attrs,
                             FileInfo.MD5]));
  end;
end;

procedure TMiTeC_DriveContent.DoScanPath;
var
  c: Integer;
  A: Boolean;
  dr: TFileRecord;
  SR: TSearchRec;
  e: string;
begin
  APath:=IncludeTrailingPathDelimiter(APath);
  try
    c:=FindFirst(APath+'*.*',faAnyFile,SR);
    while c=0 do begin
      A:=True;
      dr.ObjectName:=APath+SR.Name;
      dr.Accessed:=FileTimeToDatetime(SR.FindData.ftLastAccessTime);
      dr.Created:=FileTimeToDatetime(SR.FindData.ftCreationTime);
      dr.Modified:=FileTimeToDatetime(SR.FindData.ftLastWriteTime);
      dr.Size:=SR.Size;
      dr.Attrs:=SR.Attr;
      dr.MD5:='';
      if (dr.Attrs and faDirectory=0) then begin
        e:=ExtractFileExt(dr.ObjectName);
        if e<>'' then
          Delete(e,1,1);
        if FMD5.IndexOf(e)>-1 then
          dr.MD5:=BytesToHEX(CreateFilehash(dr.ObjectName,CALG_MD5));
      end;
      if AddRecord(SR.Name,dr) and ((SR.Attr and faDirectory)=faDirectory) then begin
        if Assigned(FOD) then
          FOD(Self,dr,A);
        if A then begin
          FLFI:=dr;
          ScanPath(APath+SR.Name);
        end;
      end;
      c:=FindNext(SR);
    end;
  finally
    FindClose(SR);
  end;
end;

procedure TMiTeC_DriveContent.Scan;
var
  ds: char;
  rh,A: Boolean;
begin
  FDrive:=ADisk;
  if AClear then
    FContent.Clear;
  if LiveData then begin
    if GetMediaPresent(FDrive) then begin
      FSD:=FSD+FDrive[1];
      ds:={$IFDEF RAD8PLUS}FormatSettings.{$ENDIF}DecimalSeparator;
      {$IFDEF RAD8PLUS}FormatSettings.{$ENDIF}DecimalSeparator:='.';
      try
        DoScanPath(FDrive);
        if Assigned(FOD) then
          FOD(Self,FLFI,A);
      finally
        {$IFDEF RAD8PLUS}FormatSettings.{$ENDIF}DecimalSeparator:=ds;
      end;
    end;
  end else begin
    rh:=True;
    LoadFromStorage(StorageFileName,rh,StreamCodeProc);
  end;
end;

procedure TMiTeC_DriveContent.ScanPath(APath: string; AClear: Boolean);
var
  ds: char;
  A: Boolean;
begin
  if AClear then
    FContent.Clear;
  if LiveData then begin
    ds:={$IFDEF RAD8PLUS}FormatSettings.{$ENDIF}DecimalSeparator;
    {$IFDEF RAD8PLUS}FormatSettings.{$ENDIF}DecimalSeparator:='.';
    try
      FLFI.ObjectName:=APath;
      if Assigned(FOD) then
        FOD(Self,FLFI,A);
      DoScanPath(APath);
      if Assigned(FOD) then
        FOD(Self,FLFI,A);
    finally
      {$IFDEF RAD8PLUS}FormatSettings.{$ENDIF}DecimalSeparator:=ds;
    end;
  end;
end;

function TMiTeC_DriveContent.GetMD5: string;
begin
  Result:=FMD5.CommaText;
end;

end.
