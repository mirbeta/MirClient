{*******************************************************}
{               MiTeC Common Routines                   }
{          Windows NT Event Log Enumeration             }
{                                                       }
{        Copyright (c) 1997-2019 Michal Mutl            }
{                                                       }
{*******************************************************}

{$INCLUDE Compilers.inc}


unit MiTeC_EventLogNT;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils, System.Classes;
     {$ELSE}
     Windows, SysUtils, Classes;
     {$ENDIF}

const
  BUFFER_SIZE = 4096;

// Defines for the READ flags for Eventlogging
  EVENTLOG_SEQUENTIAL_READ =  $0001;
  EVENTLOG_SEEK_READ       =  $0002;
  EVENTLOG_FORWARDS_READ   =  $0004;
  EVENTLOG_BACKWARDS_READ  =  $0008;

// The types of events that can be logged.

  EVENTLOG_SUCCESS          = $0000;
  EVENTLOG_ERROR_TYPE       = $0001;
  EVENTLOG_WARNING_TYPE     = $0002;
  EVENTLOG_INFORMATION_TYPE = $0004;
  EVENTLOG_AUDIT_SUCCESS    = $0008;
  EVENTLOG_AUDIT_FAILURE    = $0010;


// Defines for the WRITE flags used by Auditing for paired events
// These are not implemented in Product 1

  EVENTLOG_START_PAIRED_EVENT    = $0001;
  EVENTLOG_END_PAIRED_EVENT      = $0002;
  EVENTLOG_END_ALL_PAIRED_EVENTS = $0004;
  EVENTLOG_PAIRED_EVENT_ACTIVE   = $0008;
  EVENTLOG_PAIRED_EVENT_INACTIVE = $0010;

type
  PSID = Pointer;

  _EVENTLOGRECORD = record
    Length: Cardinal;
    Reserved: Cardinal;
    RecordNumber: Cardinal;
    TimeGenerated: Cardinal;
    TimeWritten: Cardinal;
    EventID: Cardinal;
    EventType: WORD;
    NumStrings: WORD;
    EventCategory: WORD;
    ReservedFlags: WORD;
    ClosingRecordNumber: Cardinal;
    StringOffset: Cardinal;
    UserSidLength: Cardinal;
    UserSidOffset: Cardinal;
    DataLength: Cardinal;
    DataOffset: Cardinal;
    {SourceName: PAnsiChar;
    Computername: PAnsiChar;
    UserSid: PSID;
    Strings: PAnsiChar;
    Data: PAnsiChar;
    Pad: PAnsiChar;
    Length: Cardinal;}
  end;

  PEVENTLOGRECORD = ^EVENTLOGRECORD;
  EVENTLOGRECORD = _EVENTLOGRECORD;

  TEventType = (etError, etWarning, etInformation, etAuditSuccess, etAuditFailure);

  TLogRecord = record
    EventType: TEventType;
    DateTime: TDateTime;
    Source: string;
    Category: string;
    EventID: Cardinal;
    Username: string;
    Domain: string;
    Computer: string;
    Description: string;
    BinaryData: string;
    CharData: string;
  end;

  TLogRecords = array of TLogRecord;

  TLogContainer = record
    Name,
    Filename: string;
  end;

  TLogContainers = array of TLogContainer;

  TOnReadEventLog = procedure(Sender: TObject; ARecord: TLogRecord; var Cancel: Boolean) of object;

  TEventLog = class(TPersistent)
  private
    FLC: TLogContainers;
    FRecords: TLogRecords;
    FMachine: string;
    FSourceName: string;
    FExpand: Boolean;
    hkLM: HKEY;
    FLookupSID: Boolean;
    FSourceFilter: string;
    FCTTL: Boolean;
    FOnReadEventLog: TOnReadEventLog;
    FSL: TStringList;
    FRO: boolean;

    function RetrieveLog(AMachine: string; ASourceName,ASourceFilter: string): Cardinal;
    function RefreshContainers: Cardinal;

    function GetRecCount: Cardinal;
    function GetRecord(Index: Integer): TLogRecord;
    function GetCont(Index: Integer): TLogContainer;
    function GetContCount: Cardinal;
    procedure SetMachine(const Value: string);
  public
    constructor Create(NoContainers: Boolean = False);
    destructor Destroy; override;
    function RefreshData(NoContainers: Boolean = False): Cardinal;
    procedure ClearContainers;
    procedure ClearRecords;
    procedure AddRecord(ARecord: TLogRecord);
    procedure AddContainer(AContainer: TLogContainer);
    procedure Sort;

    property ContainerCount: Cardinal read GetContCount;
    property Containers[Index: Integer]: TLogContainer read GetCont;
    property Machine: string read FMachine write SetMachine;
    property SourceName: string read FSourceName write FSourceName;
    property RecordCount: Cardinal read GetRecCount;
    property Records[Index: Integer]: TLogRecord read GetRecord;
    property ExpandMessages: Boolean read FExpand write FExpand;
    property LookupSID: Boolean read FLookupSID write FLookupSID;
    property SourceFilter: string read FSourceFilter write FSourceFilter;
    property ConvertTimeToLocal: Boolean read FCTTL write FCTTL;
    property ReverseOrder: boolean read FRO write FRO;

    property OnReadEventLog: TOnReadEventLog read FOnReadEventLog write FOnReadEventLog;
  end;

const
  EventTypes: array[etError..etAuditFailure] of string = ('Error', 'Warning', 'Information', 'AuditSuccess', 'AuditFailure');

  rkEventLog = {HKEY_LOCAL_MACHINE\}'SYSTEM\CurrentControlSet\Services\EventLog';

  rvEventMessageFile = 'EventMessageFile'; // Path to the message resource file that contains the event format strings.
  rvTypesSupported = 'TypesSupported'; //The types of events this source can generate.
  rvCategoryMessageFile = 'CategoryMessageFile'; //Path to the message resource file that has the descriptive strings for the source categories.
  rvCategoryCount = 'CategoryCount'; // The number of categories described in the CategoryMessageFile.
  rvParameterMessageFile = 'ParameterMessageFile'; //Insert parameter descriptive strings.


implementation

uses {$IFDEF RAD9PLUS}
     System.DateUtils,
     {$ELSE}
     DateUtils,
     {$ENDIF}
     MiTeC_Routines, MiTeC_StrUtils, MiTeC_Datetime;

function MAKELANGID(PrimaryLang, SubLang: Word): Word;
begin
  Result:=(SubLang shl 10) or PrimaryLang;
end;

function GetMessageInfo(AFilename: string; AID: Cardinal; AArgs: string): string;
var
  i: Integer;
  hLib: THandle;
  lpMsgBuf: PChar;
  LangID,c: Cardinal;
  Args,PArgs: ^PChar;
begin
  Result:='';
  if AArgs='' then
    c:=0
  else
    c:=GetWordCount(AArgs,[#13])+1;
  GetMem(Args,c*SizeOf(PChar));
  PArgs:=Args;
  for i:=0 to c-1 do begin
    PArgs^:=PChar(ExtractWord(i+1,AArgs,[#13]));
    Inc(PArgs);
  end;
  lpMsgBuf:=StrAlloc(BUFFER_SIZE);
  LangID:=MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT);
  if FileExists(AFilename) then begin
    hLib:=LoadLibraryEx(PChar(AFilename),0,LOAD_LIBRARY_AS_DATAFILE);
    if hLib<>0 then begin
      c:=FormatMessage(FORMAT_MESSAGE_FROM_HMODULE or FORMAT_MESSAGE_FROM_SYSTEM or FORMAT_MESSAGE_ARGUMENT_ARRAY or FORMAT_MESSAGE_IGNORE_INSERTS,
                    Pointer(hLib),AID,LangID,lpMsgBuf,BUFFER_SIZE,{$IFDEF FPC}@{$ENDIF}Args);
      if c>0 then
        Result:=Trim(string(lpMsgBuf));
      FreeLibrary(hLib);
    end;
  end;
  StrDispose(lpMsgBuf);
  FreeMem(Args)
end;

function ExpandMessage(AMessage, AParams: string): string;
var
  sl: TStringList;
  i,p: integer;
  s: string;
begin
  if AMessage='' then
    Result:=AParams
  else begin
    sl:=TStringList.Create;
    SetDelimitedText(AParams,#13,sl);
    for i:=0 to sl.Count-1 do begin
      s:='%'+IntToStr(i+1);
      p:=Pos(s,AMessage);
      if p>0 then
        AMessage:=Copy(AMessage,1,p-1)+sl[i]+Copy(AMessage,p+Length(s),1024);
    end;
    Result:=StringReplace(AMessage,#13#10#13#10,#13#10,[rfReplaceAll,rfIgnoreCase]);
    sl.Free;
  end;
end;

{ TEventLog }

procedure TEventLog.AddContainer(AContainer: TLogContainer);
begin
  SetLength(FLC,Length(FLC)+1);
  FLC[High(FLC)]:=AContainer;
end;

procedure TEventLog.AddRecord(ARecord: TLogRecord);
begin
  SetLength(FRecords,Length(FRecords)+1);
  FRecords[High(FRecords)]:=ARecord;
end;

procedure TEventLog.ClearContainers;
var
  i: Integer;
begin
  for i:=0 to High(FLC) do
    Finalize(FLC[i]);
  Finalize(FLC);
end;

procedure TEventLog.ClearRecords;
var
  i: Integer;
begin
  for i:=0 to High(FRecords) do
    Finalize(FRecords[i]);
  Finalize(FRecords);
end;

constructor TEventLog.Create;
begin
  hkLM:=0;
  FRO:=True;
  FSourceName:='';
  FSourceFilter:='';
  FExpand:=False;
  FLookupSID:=False;
  FOnReadEventLog:=nil;
  FSL:=TStringList.Create;
  if not NoContainers then
    RefreshContainers;
end;

destructor TEventLog.Destroy;
begin
  ClearContainers;
  ClearRecords;
  FSL.Free;
  inherited;
end;

function TEventLog.GetCont(Index: Integer): TLogContainer;
begin
  Result:=FLC[Index];
end;

function TEventLog.GetContCount: Cardinal;
begin
  Result:=Length(FLC);
end;

function TEventLog.GetRecCount: Cardinal;
begin
  Result:=Length(Frecords);
end;

function TEventLog.GetRecord(Index: Integer): TLogRecord;
begin
  ResetMemory(Result,sizeof(Result));
  if Index<Length(FRecords) then
    Result:=FRecords[Index];
end;

function TEventLog.RefreshContainers: Cardinal;
var
  hk,hk1: HKEY;
  Buffer: array[0..255] of Char;
  i,n: Cardinal;
  m: string;
begin
  Result:=0;
  m:='';
  if FMachine<>'' then begin
    if Pos('\\',FMachine)=0 then
      m:='\\'+FMachine
    else
      m:=FMachine;
  end;
  Finalize(FLC);
  n:=SizeOf(Buffer);
  if hkLM=0 then begin
    Result:=RegConnectRegistry(PChar(m),HKEY_LOCAL_MACHINE,hkLM);
    if Result<>ERROR_SUCCESS then begin
      SetLength(FLC,3);
      FLC[0].Name:='Application';
      FLC[1].Name:='Security';
      FLC[2].Name:='System';
      Exit;
    end;
  end;
  if (RegOpenKeyEx(hkLM,'SYSTEM\CurrentControlSet\Services\EventLog',0,KEY_READ,hk)=ERROR_SUCCESS) then begin
    i:=0;
    while (RegEnumKeyEx(hk,i,@Buffer,n,nil,nil,nil,nil)=ERROR_SUCCESS) do begin
      if RegOpenKeyEx(hk,PChar(@Buffer),0,KEY_READ,hk1)=ERROR_SUCCESS then begin
        SetLength(FLC,Length(FLC)+1);
        FLC[High(FLC)].Name:=string(Buffer);
        Buffer[0]:=#0;
        n:=SizeOf(Buffer);
        RegQueryValueEx(hk1,'File',nil,nil,PBYTE(@Buffer),@n);
        FLC[High(FLC)].FileName:=string(Buffer);
      end;
      RegCloseKey(hk1);
      Inc(i);
    end;
    RegCloseKey(hk);
  end;
end;

function TEventLog.RefreshData(NoContainers: Boolean = False): Cardinal;
begin
  Result:=0;
  if not NoContainers then
    Result:=RefreshContainers;
  if SourceName<>'' then
    Result:=RetrieveLog(Machine,SourceName,SourceFilter);
end;


function TEventLog.RetrieveLog(AMachine: string; ASourceName,ASourceFilter: string): Cardinal;
var
  h: THANDLE;
  pevlr,spevlr: PEVENTLOGRECORD;
  dwRead,dwNeeded,dwNameSize,dwDomainSize,i,l,dwr: Cardinal;
  dwSIDType: SID_NAME_USE;
  SID: PSID;
  sids,s,Strings,EventMessageFile,CategoryMessageFile,ParameterMessageFile: string;
  szNameBuf,szDomainBuf: array[0..BUFFER_SIZE-1] of char;
  elr: TLogRecord;
  b: Byte;
  rf,si,sz,n: Cardinal;
  ec,j,idx,p: Integer;
  EOF: Boolean;
  sl: TStringList;
  hk: HKEY;
  ACancel: Boolean;
begin
  Result:=0;
  dwRead:=0;
  dwNeeded:=0;
  si:=0;
  FSL.Clear;
  ACancel:=False;
  sz:=$10000;
  if FRO then
    rf:=EVENTLOG_SEQUENTIAL_READ or EVENTLOG_BACKWARDS_READ
  else
    rf:=EVENTLOG_SEQUENTIAL_READ or EVENTLOG_FORWARDS_READ;
  ClearRecords;
  h:=OpenEventLog(PChar(AMachine),PChar(ASourceName));
  if h=0 then begin
    Result:=GetLastError;
    Exit;
  end;
  sl:=TStringList.Create;
  {$IFDEF BDS3PLUS}
  sl.Delimiter:=';';
  sl.StrictDelimiter:=True;
  {$ENDIF}
  try
    //GetNumberOfEventLogRecords(h,{$IFDEF FPC}@{$ENDIF}nr);
    //GetOldestEventLogRecord(h,{$IFDEF FPC}@{$ENDIF}onr);
    GetMem(spevlr,sz);
    EOF:=False;
    repeat
      if not ReadEventLog(h,rf,0,spevlr,sz,dwRead,dwNeeded) then begin
        ec:=GetLastError;
        if ec=ERROR_INSUFFICIENT_BUFFER then begin
          sz:=dwNeeded;
          Reallocmem(spevlr,dwNeeded);
          ReadEventLog(h,rf,si,spevlr,sz,dwRead,dwNeeded);
        end else
          EOF:=ec<>1500;
      end;
      pevlr:=spevlr;
      dwr:=dwRead;
      while (dwr>0) do begin
        ResetMemory(elr,SizeOf(elr));
        elr.Source:=string(PChar(PAnsiChar(pevlr)+sizeof(_EVENTLOGRECORD)));
        if (ASourceFilter='') or (PosText(elr.Source+',',ASourceFilter+',')>0) then begin
          l:=Length(elr.Source)+1;
          {$IFDEF UNICODE}
          l:=l*2;
          {$ENDIF}
          elr.Computer:=string(PChar(PAnsiChar(pevlr)+sizeof(_EVENTLOGRECORD)+l));
          SID:=PByte(PAnsiChar(pevlr)+pevlr^.UserSidOffset);
          dwNameSize:=BUFFER_SIZE;
          dwDomainSize:=BUFFER_SIZE;
          elr.UserName:='';
          elr.Domain:='';
          if IsValidSID(SID) then begin
            sids:=ConvertSIDToString(SID);
            elr.UserName:=sids;
            if LookupSID then begin
              idx:=FSL.IndexOfName(sids);
              if idx<>-1 then begin
                s:=FSL.ValueFromIndex[idx];
                if s<>'' then begin
                  p:=Pos('\',s);
                  elr.Domain:=Copy(s,1,p-1);
                  elr.Username:=Copy(s,p+1,255);
                end;
              end else if LookupAccountSID(PChar(elr.Computer),SID,szNameBuf,dwNameSize,szDomainBuf,dwDomainSize,dwSIDType) then begin
                elr.UserName:=StrPas(sznameBuf);
                elr.Domain:=StrPas(szDomainBuf);
                FSL.Add(Format('%s=%s\%s',[sids,elr.Domain,elr.Username]));
              end else
                FSL.Add(Format('%s=',[sids]));
            end;
          end;
          elr.BinaryData:='';
          elr.CharData:='';
          i:=0;
          while i<pevlr^.DataLength do begin
            b:=PByte(PAnsiChar(PAnsiChar(pevlr)+pevlr^.DataOffset+i))^;
            s:=Format('%0.2x',[b]);
            elr.BinaryData:=elr.BinaryData+s+',';
            if not(b in [0..31,44]) then
              s:=char(b)
            else
              s:='.';
            elr.CharData:=elr.CharData+s+',';
            Inc(i);
          end;
          elr.BinaryData:=Copy(elr.BinaryData,1,Length(elr.BinaryData)-1);
          elr.CharData:=Copy(elr.CharData,1,Length(elr.CharData)-1);
          i:=0;
          l:=0;
          Strings:='';
          while i<pevlr^.NumStrings do begin
            s:=string(PChar(PAnsiChar(pevlr)+pevlr^.StringOffset+l));
            Strings:=Strings+s;
            l:=Length(Strings);
            {$IFDEF UNICODE}
            l:=l*2+2;
            {$else}
            l:=l+1;
            {$ENDIF}
            Strings:=Strings+#13;
            Inc(i);
          end;
          SetLength(Strings,Length(Strings)-1);

          elr.EventID:=Word(pevlr^.EventID and $FFFF);
          case pevlr^.EventType of
            EVENTLOG_ERROR_TYPE       :elr.EventType:=etError;
            EVENTLOG_WARNING_TYPE     :elr.EventType:=etWarning;
            EVENTLOG_INFORMATION_TYPE :elr.EventType:=etInformation;
            EVENTLOG_AUDIT_SUCCESS    :elr.EventType:=etAuditSuccess;
            EVENTLOG_AUDIT_FAILURE    :elr.EventType:=etAuditFailure;
            else elr.EventType:=etInformation;
          end;


          elr.DateTime:=UnixToDateTime(pevlr^.TimeGenerated{$IFDEF RAD14PLUS},not FCTTL{$ENDIF});
          {$IFNDEF RAD14PLUS}
          if FCTTL then
            elr.DateTime:=UTCToLocalDatetime(elr.DateTime);
          {$ENDIF}
          if FExpand then begin
            if hkLM=0 then begin
              if FMachine='' then
                RegConnectRegistry(nil,HKEY_LOCAL_MACHINE,hkLM)
              else
                RegConnectRegistry(PChar('\\'+FMachine),HKEY_LOCAL_MACHINE,hkLM);
            end;
            if (hkLM<>0) and (RegOpenKeyEx(hkLM,PChar(rkEventLog+'\'+SourceName+'\'+elr.Source),0,KEY_READ,hk)=ERROR_SUCCESS) then begin
              szNameBuf[0]:=#0;
              n:=SizeOf(szNameBuf);
              if RegQueryValueEx(hk,rvEventMessageFile,nil,nil,PBYTE(@szNameBuf),@n)=ERROR_SUCCESS then
                EventMessageFile:=ExpandEnvVars(string(szNameBuf))
              else
                EventMessageFile:='';
              szNameBuf[0]:=#0;
              n:=SizeOf(szNameBuf);
              if RegQueryValueEx(hk,rvCategoryMessageFile,nil,nil,PBYTE(@szNameBuf),@n)=ERROR_SUCCESS then
                CategoryMessageFile:=ExpandEnvVars(string(szNameBuf))
              else
                CategoryMessageFile:='';
              szNameBuf[0]:=#0;
              n:=SizeOf(szNameBuf);
              if RegQueryValueEx(hk,rvParameterMessageFile,nil,nil,PBYTE(@szNameBuf),@n)=ERROR_SUCCESS then
                ParameterMessageFile:=ExpandEnvVars(string(szNameBuf))
              else
                ParameterMessageFile:='';
              RegCloseKey(hk);
            end;
            sl.DelimitedText:=CategoryMessageFile;
            for j:=0 to sl.Count-1 do begin
              try s:=Trim(GetMessageInfo(sl[j],pevlr^.EventCategory,Strings)); except end;
              if s<>''  then
                elr.Category:=s;
            end;
            sl.DelimitedText:=EventMessageFile;
            for j:=0 to sl.Count-1 do begin
              try s:=ExpandMessage(GetMessageInfo(sl[j],pevlr^.EventID,Strings),Strings); except end;
              if s<>'' then
                elr.Description:=s;
            end;
          end;
          if elr.Category='' then
            elr.Category:=IntToStr(pevlr^.EventCategory);
          if elr.Description='' then
            elr.Description:=Trim(Strings);
          AddRecord(elr);
          if Assigned(FOnReadEventLog) then
            FOnReadEventLog(Self,elr,ACancel);
          if ACancel then
            Break;
        end;
        dwr:=dwr-pevlr^.Length;
        pevlr:=PEVENTLOGRECORD(PAnsiChar(pevlr)+pevlr^.Length);
      end;
      if ACancel then
        Break;
    until EOF;
  finally
    FSL.Clear;
    Freemem(spevlr);
    CloseEventLog(h);
    sl.Free;
  end;
end;

procedure TEventLog.SetMachine(const Value: string);
begin
  if not SameText(FMachine,Value) then begin
    RegCloseKey(hkLM);
    hkLM:=0;
  end;
  FMachine:=Value;
end;

procedure TEventLog.Sort;

  procedure QuickSort(ALo, AHi: integer);
  var
    Lo,Hi,Mid: Integer;
    r: TLogRecord;
  begin
    repeat
      Lo:=ALo;
      Hi:=AHi;
      Mid:=(Lo+Hi) div 2;
      repeat
        while CompareDatetime(FRecords[Lo].DateTime,FRecords[Mid].DateTime)<0 do
          Inc(Lo);
        while CompareDatetime(FRecords[Hi].DateTime,FRecords[Mid].DateTime)>0 do
          Dec(Hi);
        if Lo<=Hi then begin
          if Lo<>Hi then begin
            r:=FRecords[Lo];
            FRecords[Lo]:=FRecords[Hi];
            FRecords[Hi]:=r;
            if Mid=Lo then
              Mid:=Hi
            else if Mid=Hi then
              Mid:=Lo;
          end;
          Inc(Lo);
          Dec(Hi);
        end;
      until Lo>Hi;
      if ALo<Hi then
        QuickSort(ALo,Hi);
      ALo:=Lo;
    until Lo>=AHi;
  end;

begin
  if Length(FRecords)>0 then
    QuickSort(0,High(FRecords));
end;



end.
