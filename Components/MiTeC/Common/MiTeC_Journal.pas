{*******************************************************}
{                 MiTeC Common Routines                 }
{                    Journal Object                     }
{                                                       }
{                                                       }
{           Copyright (c) 1997-2019 Michal Mutl         }
{                                                       }
{*******************************************************}

{$INCLUDE Compilers.inc}

unit MiTeC_Journal;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils, System.Classes, System.Variants,
     {$ELSE}
     Variants, Windows, SysUtils, Classes,
     {$ENDIF}
     MiTeC_Windows;

type
  TJournalEvent = (jeNormal, jeSystem, jeInformation, jeWarning, jeError, jeData, jeAction, jeExecute, jeReturn, jeMessage);

  TEventLevel = (elNormal, elStart, elBegin, elEnd, elFinish);

  TRecordDataProperty = record
    PropName,
    Value: String;
  end;

  TRecordData = array of TRecordDataProperty;

  TJournalRecord = record
    Event: TJournalEvent;
    Level: TEventLevel;
    Timestamp: TDateTime;
    TimestampStr: String;
    Text: String;
    Data: TRecordData;
  end;

  TJournalBuffer = array of TJournalRecord;

  TJournal = class
  private
    FProcessHandle: THandle;
    FFile: TFileStream;
    FBuffer: TJournalBuffer;
    FInternalSave: Boolean;
    FFilename, FMachine, FUser: string;
    FOverwrite: Boolean;
    FStartTime,FStopTime: Int64;
    FInternalTime: array of Int64;
    FModuleName: string;
    FModuleVersion: string;
    function GetRecord(Index: DWORD): TJournalRecord;
    function GetRecordCount: DWORD;
    procedure SetRecord(Index: DWORD; const Value: TJournalRecord);
    procedure AddRecord(ATimestamp: TDateTime; AText: string; AEvent: TJournalEvent; ALevel: TEventLevel; AData: TRecordData); overload;
    //procedure AddRecord(ATimestamp: string; AText: string; AEvent: TJournalEvent; ALevel: TEventLevel; AData: TRecordData); overload;
    procedure AddRecord(ARecord: TJournalRecord); overload;
    procedure CreateFile;
    procedure PushTime(Time: Int64);
    function PopTime: UInt64;
    procedure WriteToFile(ARecord: TJournalRecord);
    procedure WriteSpace;
  public
    constructor Create(ADir,AFileNamePrefix: string; AInternalSave,AOverwrite,ASaveOnDisk: boolean);
    destructor Destroy; override;

    procedure WriteSimpleEvent(AText: string; AEvent: TJournalEvent; ALevel: TEventLevel = elNormal);
    procedure WriteEvent(AText: string; AEvent: TJournalEvent; ALevel: TEventLevel; AData: TRecordData);
    procedure WritePropertyEvent(AText: string; AEvent: TJournalEvent; ALevel: TEventLevel; APropNames: array of string; AValues: array of Variant);
    procedure LoadFromFile(AFilename: string);
    procedure SaveToFile(AFilename: string);
    procedure SaveToCSV(AFilename: string);
    procedure Clear;
    procedure StartTimer;
    function StopTimer: Extended;

    procedure AddDataProperty(var AData: TRecordData; AName: string; AValue: Variant);

    property FileName: string read FFilename;
    property InternalSave: Boolean read FInternalSave write FInternalSave;
    property Overwrite: Boolean read FOverwrite write FOverwrite;
    property Records[Index: DWORD]: TJournalRecord read GetRecord write SetRecord;
    property RecordCount: DWORD read GetRecordCount;

    property ModuleName: string read FModuleName;
    property ModuleVersion: string read FModuleVersion;
  end;

function FormatTimer(ATime: Int64): string;

const
  JournalEvents: array[TJournalEvent] of string = (
                                               'Normal ',
                                               'System ',
                                               'Info   ',
                                               'Warning',
                                               'Error  ',
                                               'Data   ',
                                               'Action ',
                                               'Execute',
                                               'Return ',
                                               'Message'
                                               );
  EventLevels: array[TEventLevel] of string = (
                                               'Normal ',
                                               'Start  ',
                                               'Begin  ',
                                               'End    ',
                                               'Finish '
                                               );
  extMJF = '.mjf';

resourcestring
  rsJournalStartedInEXE = 'Process started';
  rsJournalFinishedInEXE = 'Process terminated';
  rsJournalStartedInModule = 'Module started';
  rsJournalFinishedInModule = 'Module terminated';
  rsJournalInternalFree = 'Freeing internal timer leak';
  rsExitCode = 'ExitCode';
  rsElapsedTime = 'ElapsedTime';
  rsName = 'Name';
  rsVersion = 'Version';
  rsMachine = 'Machine';
  rsUser = 'Username';
  rsTimestamp = 'Timestamp';
  rsRecNo = 'RecNo';

implementation

uses {$IFDEF RAD9PLUS}
     System.Win.Registry,
     {$else}
     Registry,
     {$ENDIF}
     MiTeC_Routines, MiTeC_StrUtils, MiTeC_Datetime;

type
  TVersionInfo = record
    FileName,
    Version,
    ProductName,
    CompanyName,
    Description,
    Comments,
    Copyright: string;
    Major,
    Minor,
    Release,
    Build: DWORD;
  end;

function FormatTimer;
begin
  Result:=FormatMilliSeconds(ATime,False);
end;

function GetFileVerInfo(const fn :string; var VI:TVersionInfo): Boolean;
var
  VersionHandle,VersionSize :dword;
  PItem,PVersionInfo :pointer;
  FixedFileInfo :PVSFixedFileInfo;
  il :uint;
  translation: string;
begin
  Result:=False;
  if fn<>'' then begin
    VI.FileName:=fn;
    versionsize:=getfileversioninfosize(PChar(fn),versionhandle);
    Result:=False;
    if versionsize=0 then
      exit;
    getMem(pversioninfo,versionsize);
    try
      if getfileversioninfo(PChar(fn),versionhandle,versionsize,pversioninfo) then begin
        Result:=True;
        if verqueryvalue(pversioninfo,'\',pointer(fixedfileinfo),il) then begin
          VI.version:=inttostr(hiword(fixedfileinfo^.dwfileversionms))+
                   '.'+inttostr(loword(fixedfileinfo^.dwfileversionms))+
                   '.'+inttostr(hiword(fixedfileinfo^.dwfileversionls))+
                   '.'+inttostr(loword(fixedfileinfo^.dwfileversionls));
          VI.Major:=hiword(fixedfileinfo^.dwfileversionms);
          VI.Minor:=loword(fixedfileinfo^.dwfileversionms);
          VI.Release:=hiword(fixedfileinfo^.dwfileversionls);
          VI.Build:=loword(fixedfileinfo^.dwfileversionls);

          if verqueryvalue(pversioninfo,pchar('\VarFileInfo\Translation'),pitem,il) then begin
            translation:=IntToHex(PDWORD(pitem)^,8);
            translation:=Copy(translation,5,4)+Copy(translation,1,4);
          end;
          if verqueryvalue(pversioninfo,pchar('\StringFileInfo\'+translation+'\FileDescription'),pitem,il) then
            VI.description:=pchar(pitem);

          if verqueryvalue(pversioninfo,pchar('\StringFileInfo\'+translation+'\LegalCopyright'),pitem,il) then
            VI.Copyright:=pchar(pitem);

          if verqueryvalue(pversioninfo,pchar('\StringFileInfo\'+translation+'\Comments'),pitem,il) then
            VI.Comments:=pchar(pitem);

          if verqueryvalue(pversioninfo,pchar('\StringFileInfo\'+translation+'\ProductName'),pitem,il) then
            VI.ProductName:=pchar(pitem);

          if verqueryvalue(pversioninfo,pchar('\StringFileInfo\'+translation+'\CompanyName'),pitem,il) then
            VI.CompanyName:=pchar(pitem);

        end;
      end;
    finally
      freeMem(pversioninfo,versionsize);
    end;
  end;
end;

function GetUserAndDomainName(hProcess :THandle; var UserName, DomainName :string) :boolean;
const
  RTN_OK = 0;
  RTN_ERROR = 13;
  MY_BUFSIZE = 512;
var
  hToken :THandle;
  InfoBuffer :array[0..MY_BUFSIZE] of byte;
  snu :SID_NAME_USE;
  cchUserName,cchDomainName :dword;
  cbInfoBuffer :DWORD;
begin
  cbInfoBuffer:=MY_BUFSIZE;
  result:=false;
  if OpenProcessToken(hProcess,TOKEN_QUERY,hToken) then begin
    if GetTokenInformation(hToken,TokenUser,@InfoBuffer,cbInfoBuffer,cbInfoBuffer) then
      result:=LookupAccountSid(nil,PSID(@InfoBuffer),PChar(@UserName),
                          cchUserName,PChar(@DomainName),cchDomainName,snu);
    CloseHandle(hToken);
  end;
end;

function GetMachine :string;
var
  n :dword;
  buf :pchar;
const
  rkMachine = {HKEY_LOCAL_MACHINE}'\SYSTEM\CurrentControlSet\Control\ComputerName\ComputerName';
    rvMachine = 'ComputerName';
begin
  n:=255;
  buf:=stralloc(n);
  GetComputerName(buf,n);
  result:=buf;
  strdispose(buf);
  with TRegistry.Create do begin
    rootkey:=HKEY_LOCAL_MACHINE;
    if OpenKeyReadOnly(rkMachine) then begin
      if ValueExists(rvMachine) then
        result:=ReadString(rvMachine);
      closekey;
    end;
    free;
  end;
end;

function GetUser :string;
var
  n :dword;
  buf :pchar;
begin
  n:=255;
  buf:=stralloc(n);
  GetUserName(buf,n);
  result:=buf;
  strdispose(buf);
end;

{ TJournal }

procedure TJournal.AddDataProperty;
begin
  SetLength(AData,Length(AData)+1);
  with AData[High(AData)] do begin
    PropName:=AName;
    try
      Value:=VarToStr(AValue);
    except
      Value:='';
    end;
  end;
end;

procedure TJournal.AddRecord(ARecord: TJournalRecord);
begin
  SetLength(FBuffer,Length(FBuffer)+1);
  FBuffer[High(FBuffer)]:=ARecord;
end;

procedure TJournal.AddRecord(ATimestamp: TDateTime; AText: string;
  AEvent: TJournalEvent; ALevel: TEventLevel; AData: TRecordData);
begin
  SetLength(FBuffer,Length(FBuffer)+1);
  with FBuffer[High(FBuffer)] do begin
    Event:=AEvent;
    Level:=ALevel;
    Timestamp:=ATimestamp;
    TimestampStr:=FormatDateTime('yyy-mm-dd hh:mm:ss',ATimestamp);
    Text:=AText;
    Data:=AData;
  end;
end;

{procedure TJournal.AddRecord(ATimestamp, AText: string;
  AEvent: TJournalEvent; ALevel: TEventLevel; AData: TRecordData);
begin
  SetLength(FBuffer,Length(FBuffer)+1);
  with FBuffer[High(FBuffer)] do begin
    Event:=AEvent;
    Level:=ALevel;
    Timestamp:=0;
    TimeStampStr:=ATimestamp;
    Text:=AText;
    Data:=AData;
  end;
end;}

procedure TJournal.Clear;
var
  i: Integer;
begin
  for i:=0 to High(FBuffer) do
    Finalize(FBuffer[i].Data);
  Finalize(FBuffer);
  if Assigned(FFile) then begin
    FlushFileBuffers(FFile.Handle);
    FFile.Free;
  end;
  DeleteFile(FFilename);
  CreateFile;
end;

constructor TJournal.Create;
var
  p: PChar;
  VIM: TVersionInfo;
  s: string;
begin
  FMachine:=GetMachine;
  FUser:=GetUser;
  GetUserAndDomainName(GetCurrentProcess,FUser,FMachine);
  p:=Allocmem(256);
  GetModuleFileName(hInstance,p,255);
  FModulename:=p;
  GetFileVerInfo(p,VIM);
  FModuleVersion:=VIM.Version;
  FreeMem(p);
  FInternalSave:=AInternalSave;
  FOverwrite:=AOverwrite;
  SetLength(FBuffer,0);
  if not ASaveOnDisk then begin
    s:=ExtractFileExt(AFileNamePrefix);
    if s='' then
      s:=extMJF;
    AFileNamePrefix:=Trim(ChangeFileExt(ExtractFilename(AFileNamePrefix),''));
    if AFileNamePrefix<>'' then
      AFileNamePrefix:=AFilenamePrefix+'_';
    FFilename:=IncludeTrailingPathDelimiter(ADir)+AFilenamePrefix+FormatDateTime('yyyy-mm-dd',Date)+s;
    CreateFile;
  end;
end;

procedure TJournal.CreateFile;
var
  VIM,VIP: TVersionInfo;
  p: PChar;
  rd: TRecordData;
begin
  Finalize(rd);
  if Assigned(FFile) then begin
    FlushFileBuffers(FFile.Handle);
    FFile.Free;
  end;
  try
    if FOverwrite or not FileExists(FFilename) then begin
      FFile:=TFileStream.Create(FFileName,fmCreate or fmShareDenyWrite);
      FFile.Free;
    end;
    FFile:=TFileStream.Create(FFileName,fmOpenWrite or fmShareDenyWrite);
    if FFile.Size>0 then begin
      FFile.Position:=FFile.Size;
      WriteSpace;
    end;
    FProcessHandle:=GetModuleHandle(nil);
    GetFileVerInfo(ParamStr(0),VIP);
    if FProcessHandle<>hInstance then begin
      p:=Allocmem(256);
      GetModuleFileName(hInstance,p,255);
      GetFileVerInfo(p,VIM);
      AddDataProperty(rd,rsName,string(p));
      AddDataProperty(rd,rsVersion,VIM.Version);
      WriteEvent(rsJournalStartedInModule,jeSystem,elBegin,rd);
      Freemem(p);
    end else begin
      AddDataProperty(rd,rsName,ParamStr(0));
      AddDataProperty(rd,rsVersion,VIP.Version);
      AddDataProperty(rd,rsMachine,FMachine);
      AddDataProperty(rd,rsUser,FUser);
      WriteEvent(rsJournalStartedInEXE,jeSystem,elStart,rd);
    end;
  except
    on e: Exception do begin
      FFile:=nil;
      FFilename:='';
    end;
  end;
end;

destructor TJournal.Destroy;
var
  i: Integer;
  rd: TRecordData;
begin
  for i:=0 to High(FInternalTime) do
    WriteEvent(rsJournalInternalFree,jeSystem,elFinish,nil);
  if FProcessHandle<>hInstance then
    WriteEvent(rsJournalFinishedInModule,jeSystem,elEnd,nil)
  else begin
    AddDataProperty(rd,rsExitCode,ExitCode);
    WriteEvent(rsJournalFinishedInEXE,jeSystem,elFinish,rd);
  end;
  SetLength(FBuffer,0);
  if Assigned(FFile) then begin
    FlushFileBuffers(FFile.Handle);
    FFile.Free;
  end;
  inherited;
end;

function TJournal.GetRecord(Index: DWORD): TJournalRecord;
begin
  Finalize(Result);
  try
    Result:=FBuffer[Index];
  except
    ZeroMemory(@Result,SizeOf(TJournalRecord));
  end;
end;

function TJournal.GetRecordCount: DWORD;
begin
  Result:=Length(FBuffer);
end;

procedure TJournal.LoadFromFile(AFilename: string);
var
  fs: TFileStream;
  sl: TStringList;
  i,p,l: Integer;
  j: TJournalEvent;
  k: TEventLevel;
  s,v: string;
  r: TJournalRecord;
begin
  Clear;
  sl:=TStringList.Create;
  fs:=TFileStream.Create(AFileName,fmOpenRead or fmShareDenyNone);
  try
    sl.LoadFromStream(fs);
    for i:=0 to sl.Count-1 do begin
      Finalize(r);
      ZeroMemory(@r,SizeOf(r));
      s:=sl[i];
      if Pos('[',s)=1 then begin
        p:=Pos(']',s);
        r.TimestampStr:=Copy(s,2,p-2);
        Delete(s,1,p);
        p:=Pos(']',s);
        v:=Trim(Copy(s,2,p-2));
        r.Event:=jeNormal;
        for j:=Low(JournalEvents) to High(JournalEvents) do
          if CompareText(v,Trim(JournalEvents[j]))=0 then begin
            r.Event:=j;
            Break;
          end;
        Delete(s,1,p);
        p:=Pos(']',s);
        v:=Trim(Copy(s,2,p-2));
        r.Level:=elNormal;
        for k:=Low(EventLevels) to High(EventLevels) do
          if CompareText(v,Trim(EventLevels[k]))=0 then begin
            r.Level:=k;
            Break;
          end;
        Delete(s,1,p+1);
        p:=Pos('{',s);
        if p>0 then begin
          r.Text:=Copy(s,1,p-2);
          Delete(s,1,p-1);
          while p>0 do begin
            p:=Pos('}',s);
            v:=Copy(s,2,p-2);
            l:=Pos('=',v);
            if l>0 then
              AddDataProperty(r.Data,Copy(v,1,l-1),Copy(v,l+1,Length(v)));
            Delete(s,1,p);
            p:=Pos('{',s);
          end;
        end else
          r.Text:=s;
        AddRecord(r);
      end;
    end;
  finally
    fs.Free;
    sl.Free;
  end;
end;

function TJournal.PopTime: UInt64;
begin
  try
    Result:=FInternalTime[High(FInternalTime)];
    SetLength(FInternalTime,High(FInternalTime));
  except
    Result:=GetTickCount64;
  end;
end;

procedure TJournal.PushTime(Time: Int64);
begin
  try
    SetLength(FInternalTime,Length(FInternalTime)+1);
    FInternalTime[High(FInternalTime)]:=Time;
  except
  end;  
end;

procedure TJournal.SaveToCSV(AFilename: string);
var
  i: Integer;
  sl: TStringList;
begin
  sl:=TStringList.Create;
  try
    sl.Add('TS;MSG');
    for i:=0 to High(FBuffer) do
      with FBuffer[i] do
        sl.Add(Format('%s;%s',[TimeStampStr,Text]));
    sl.SaveToFile(AFilename);
  finally
    sl.Free;
  end;
end;

procedure TJournal.SaveToFile(AFilename: string);
var
  i: Integer;
  sl: TStringList;
begin
  sl:=TStringList.Create;
  try
    for i:=0 to High(FBuffer) do
      WriteToFile(FBuffer[i]);
    sl.SaveToFile(AFilename);
  finally
    sl.Free;
  end;
end;

procedure TJournal.SetRecord(Index: DWORD; const Value: TJournalRecord);
begin
  FBuffer[Index]:=Value;
end;

procedure TJournal.StartTimer;
begin
  FStartTime:=GetTickCount64;
  FStopTime:=FStartTime;
end;

function TJournal.StopTimer: Extended;
begin
  FStopTime:=GetTickCount64;
  Result:=FStopTime-FStartTime;
end;

procedure TJournal.WriteEvent(AText: string; AEvent: TJournalEvent; ALevel: TEventLevel; AData: TRecordData);
var
  i: Integer;
  t: Int64;
  rd: TRecordData;
  r: TJournalRecord;
begin
  Finalize(rd);
  AText:=StringReplace(AText,#10#13,' ',[rfReplaceAll,rfIgnoreCase]);
  AText:=StringReplace(AText,#13#10,' ',[rfReplaceAll,rfIgnoreCase]);
  AText:=StringReplace(AText,#10,' ',[rfReplaceAll,rfIgnoreCase]);
  AText:=StringReplace(AText,#13,' ',[rfReplaceAll,rfIgnoreCase]);
  if ALevel=elBegin then
    PushTime(GetTickCount64);
  if ALevel=elEnd then begin
    t:=GetTickCount64-PopTime;
    AddDataProperty(rd,rsElapsedTime,FormatTimer(t));
  end;
  Finalize(r);
  Zeromemory(@r,SizeOf(r));
  r.Timestamp:=now;
  r.TimestampStr:=FormatDateTime('yyyy-mm-dd hh:mm:ss',r.Timestamp);
  r.Text:=AText;
  r.Event:=AEvent;
  r.Level:=ALevel;
  if Assigned(AData) then
    for i:=0 to High(AData) do
      AddDataProperty(rd,AData[i].PropName,AData[i].Value);
  r.Data:=rd;
  WriteToFile(r);
  if FInternalSave then
    AddRecord(r);
end;

procedure TJournal.WritePropertyEvent;
var
  rd: TRecordData;
  i: Integer;
begin
  Finalize(rd);
  for i:=0 to High(APropnames) do
    try
      AddDataProperty(rd,APropnames[i],VarToStr(AValues[i]));
    except
    end;  
  WriteEvent(AText,AEvent,ALevel,rd);
end;

procedure TJournal.WriteSimpleEvent(AText: string; AEvent: TJournalEvent;
  ALevel: TEventLevel);
begin
  WriteEvent(AText,AEvent,ALevel,nil);
end;

procedure TJournal.WriteSpace;
var
  s: string;
begin
  if Assigned(FFile) then begin
    s:=#13#10;
    FFile.WriteBuffer(PChar(s)^,Length(s));
    FlushFileBuffers(FFile.Handle);
  end;
end;

procedure TJournal.WriteToFile(ARecord: TJournalRecord);
var
  s: string;
  i: Integer;
begin
  if Assigned(FFile) then
    with ARecord do begin
      s:=Format('[%s][%s][%s] %s ',[TimeStampStr,
                                   JournalEvents[Event],
                                   EventLevels[Level],
                                   Text]);
      if Assigned(Data) then
        for i:=0 to High(Data) do
          s:=s+Format('{%s = %s}',[Data[i].Propname,Data[i].Value]); 
      s:=s+#13#10;
      FFile.WriteBuffer(PChar(s)^,Length(s));
      FlushFileBuffers(FFile.Handle);
    end;
end;

end.
