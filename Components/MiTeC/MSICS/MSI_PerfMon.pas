{*******************************************************}
{                                                       }
{       MiTeC System Information Component Suite        }
{           Performance Counter Monitor                 }
{                version 13.4.0                         }
{                                                       }
{        Copyright (c) 1997-2019 Michal Mutl            }
{                                                       }
{*******************************************************}

{$INCLUDE Compilers.inc}

unit MSI_PerfMon;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils, System.Classes, System.SyncObjs,
     {$ELSE}
     Windows, SysUtils, Classes, ExtCtrls, SyncObjs,
     {$ENDIF}
     MSI_Defs, MiTeC_Windows, MiTeC_WinPerf, MiTeC_Pdh;

type
  PPerformanceCounter = ^TPerformanceCounter;
  TPerformanceCounter = record
    Path: string;
    Handle: PDH_HCOUNTER;
    Value: Double;
  end;

  TPerfMonThread = class;

  TPerfMonNotifyEvent = procedure(Sender: TPerfMonThread) of object;

  TPerfMonThread = class(TThread)
  private
    FLock: TCriticalSection;
    FOnInterval: TPerfMonNotifyEvent;
    FInterval: Cardinal;
    FQuery: PDH_HQUERY;
    FList: TList;
    function GetInterval: Cardinal;
    function GetOnInterval: TPerfMonNotifyEvent;
    procedure SetInterval(const Value: Cardinal);
    procedure SetOnInterval(const Value: TPerfMonNotifyEvent);
    function GetCounterCount: Integer;
    procedure DoSync;
  protected
    procedure Execute; override;
  public
    class procedure ReadPerfTexts(ALocalized, AHelp: Boolean; AList: TStringList);
    class function GetLocalizedPerfCounterName(const APath: string): string;
    class function GetEnglishPerfCounterName(const APath: string): string;
    class function CounterExists(APath: string): Boolean;
    class procedure GetCounterInstances(const APath: string; AList: TStringList);
    class procedure ParseCounterName(const APath: string; var AMachine,AObject,AInstance,AParentInstance,ACounter: string; var AInstanceIndex: Cardinal);
    class function MakeCounterName(const AMachine,AObject,AInstance,AParentInstance,ACounter: string; AInstanceIndex: Cardinal): string;

    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function FindCounter(APath: string): Integer;
    function AddCounter(const APath: string): Cardinal;
    procedure RemoveCounter(APath: string);
    function ReadCounter(APath: string): Double;
    procedure GetCounter(AIndex: Integer; out ACounter: TPerformanceCounter);
    property CounterCount: Integer read GetCounterCount;
    property Interval: Cardinal read GetInterval write SetInterval;
    property OnInterval: TPerfMonNotifyEvent read GetOnInterval write SetOnInterval;
  end;

implementation

uses {$IFDEF RAD9PLUS}
     {$IFDEF TRIAL}VCL.Dialogs,{$ENDIF}System.Math,
     {$ELSE}
     {$IFDEF TRIAL}Dialogs,{$ENDIF}Math,
     {$ENDIF}
     {$IFDEF RAD5PLUS}MiTeC_Helpers,{$ENDIF}
     MiTeC_StrUtils, MiTeC_Routines, MiTeC_RegUtils;

var
  LocalNames,EngNames: TStringList;

{ TPerfMonThread }

function TPerfMonThread.AddCounter(const APath: string): Cardinal;
var
  r: PPerformanceCounter;
  s,l: string;
  p: PChar;
  n: Cardinal;
begin
  Result:=0;
  if not Assigned(PdhExpandWildCardPath) or not Assigned(PdhAddCounter) then
    Exit;
  FLock.Enter;
  try
    l:=GetLocalizedPerfCounterName(APath);
    if Pos('*',APath)>0 then begin
      n:=0;
      PdhExpandWildCardPath(nil,PChar(l),nil,n,0);
      Inc(n);
      SetLength(s,n);
      if (PdhExpandWildCardPath(nil,PChar(l),PChar(s),n,0)=ERROR_SUCCESS) and (s<>'') then begin
        p:=@s[1];
        while p^<>#0 do begin
          new(r);
          r.Path:=string(p);
          if Assigned(PdhAddEnglishCounter) and SameText(l,APath) then begin
            Result:=PdhAddEnglishCounter(FQuery,PChar(r.Path),0,r.Handle);
            if (Result=ERROR_SUCCESS) then
              FList.Add(r)
            else
              Dispose(r);
          end else begin
            Result:=PdhAddCounter(FQuery,PChar(r.Path),0,r.Handle);
            if (Result=ERROR_SUCCESS) then
              FList.Add(r)
            else
              Dispose(r);
          end;
          Inc(p,Length(p)+1);
        end;
      end;
    end else begin
      new(r);
      r.Path:=l;
      if Pos('\\',r.Path)=0 then
        r.Path:='\\'+MachineName+r.Path;
      if Assigned(PdhAddEnglishCounter) and SameText(l,APath) then begin
        Result:=PdhAddEnglishCounter(FQuery,PChar(r.Path),0,r.Handle);
        if (Result=ERROR_SUCCESS) then
          FList.Add(r)
        else
          Dispose(r);
      end else begin
        Result:=PdhAddCounter(FQuery,PChar(r.Path),0,r.Handle);
        if (Result=ERROR_SUCCESS) then
          FList.Add(r)
        else
          Dispose(r);
      end;
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TPerfMonThread.Clear;
var
  i: integer;
begin
  FLock.Enter;
  try
    for i:=0 to FList.Count-1 do begin
      PdhRemoveCounter(PPerformanceCounter(FList[i])^.Handle);
      Dispose(PPerformanceCounter(FList[i]));
    end;
    FList.Clear;
  finally
    FLock.Leave;
  end;
end;

class function TPerfMonThread.CounterExists(APath: string): Boolean;
var
  idx: Integer;
  nc,ni: Cardinal;
  sc,si,s: string;
begin
  try
    s:=GetLocalizedPerfCounterName(APath);
    if Pos('\\',s)=1 then begin
      Delete(s,1,2);
      idx:=Pos('\',s);
      if idx>0 then
        Delete(s,1,idx);
    end;
    idx:=Pos('(',s);
    if idx>0 then
      s:=Copy(s,1,idx-1);
    if s[1]='\' then
     Delete(s,1,1);
    nc:=0;
    ni:=0;
    PdhEnumObjectItems(nil,nil,PChar(s),nil,nc,nil,ni,PERF_DETAIL_WIZARD,0);
    SetLength(sc,nc);
    SetLength(si,ni);
    Result:=PdhEnumObjectItems(nil,nil,PChar(s),PChar(sc),nc,PChar(si),ni,PERF_DETAIL_WIZARD,0)=ERROR_SUCCESS;
  except
    Result:=False;
  end;
end;

constructor TPerfMonThread.Create;
begin
  {$IFDEF TRIAL}
  if not RunFromIDE then
    MessageDlg('TPerfMonThread'+sLineBreak+cCompName+sLineBreak+cCopyright,mtInformation,[mbOK],0);
  {$ENDIF}
  inherited Create(True);
  FreeOnTerminate:=False;

  FLock:=TCriticalSection.Create;
  FOnInterval:=nil;
  FInterval:=1000;

  FList:=TList.Create;
  FQuery:=0;

  if Assigned(PdhOpenQuery) then
    PdhOpenQuery(nil,0,FQuery);
end;

destructor TPerfMonThread.Destroy;
begin
  FOnInterval:=nil;
  FList.Free;
  FLock.Free;
  if FQuery<>0 then
    PdhCloseQuery(FQuery);
  inherited;
end;

procedure TPerfMonThread.DoSync;
begin
  if Assigned(FOnInterval) then
    FOnInterval(Self);
end;

procedure TPerfMonThread.Execute;
var
  se: TSimpleEvent;
  i: Integer;
  ct: Cardinal;
  cv: TPdhFmtCounterValue;
begin
  if not Assigned(PdhOpenQuery) then
    Exit;
  se:=TSimpleEvent.Create{$IFDEF BDS35PLUS}(nil,False,False,''){$ENDIF};
  try
    while not Terminated do begin
      FLock.Enter;
      try
        if PdhCollectQueryData(FQuery)=ERROR_SUCCESS then
          for i:=0 to FList.Count-1 do begin
            PPerformanceCounter(FList[i])^.Value:=0;
            if (PdhGetFormattedCounterValue(PPerformanceCounter(FList[i])^.Handle,PDH_FMT_DOUBLE,@ct,cv)=ERROR_SUCCESS) and (cv.CStatus<>PDH_CSTATUS_INVALID_DATA) then
              PPerformanceCounter(FList[i])^.Value:=cv.doubleValue;
          end;
      finally
        FLock.Leave;
      end;

      if Assigned(FOnInterval) then
        Synchronize(DoSync);

      se.WaitFor(FInterval);
    end;
  finally
    se.Free;
  end;
end;

function TPerfMonThread.FindCounter(APath: string): Integer;
var
  i: Integer;
  l: string;
begin
  FLock.Enter;
  try
    Result:=-1;
    if Pos('\\',APath)=0 then
      APath:='\\'+MachineName+APath;
    l:=GetLocalizedPerfCounterName(APath);
    for i:=0 to FList.Count-1 do
      if SameText(PPerformanceCounter(FList[i])^.Path,l) then begin
        Result:=i;
        Break;
      end;
  finally
    FLock.Leave;
  end;
end;

procedure TPerfMonThread.GetCounter(AIndex: Integer;
  out ACounter: TPerformanceCounter);
begin
  FLock.Enter;
  try
    if AIndex<FList.Count then
      ACounter:=PPerformanceCounter(FList[AIndex])^;
  finally
    FLock.Leave;
  end;
end;

function TPerfMonThread.GetCounterCount: Integer;
begin
  FLock.Enter;
  try
    Result:=FList.Count;
  finally
    FLock.Leave;
  end;
end;

class procedure TPerfMonThread.GetCounterInstances(const APath: string; AList: TStringList);
var
  idx: Integer;
  s,sc,si: string;
  nc,ni: Cardinal;
  p: PChar;
begin
  AList.Clear;
  s:=GetLocalizedPerfCounterName(APath);
  if Pos('\\',s)=1 then begin
    Delete(s,1,2);
    idx:=Pos('\',s);
    if idx>0 then
      Delete(s,1,idx);
  end;
  idx:=Pos('(',s);
  if idx>0 then
    s:=Copy(s,1,idx-1);
  if s[1]='\' then
   Delete(s,1,1);
  nc:=0;
  ni:=0;
  PdhEnumObjectItems(nil,nil,PChar(s),nil,nc,nil,ni,PERF_DETAIL_WIZARD,0);
  SetLength(sc,nc);
  SetLength(si,ni);
  if PdhEnumObjectItems(nil,nil,PChar(s),PChar(sc),nc,PChar(si),ni,PERF_DETAIL_WIZARD,0)=ERROR_SUCCESS then begin
    if ni>0 then begin
      p:=@si[1];
      while p^<>#0 do begin
        AList.Add(string(p));
        Inc(p,Length(p)+1);
      end;
    end;
  end;
end;

class function TPerfMonThread.GetEnglishPerfCounterName(
  const APath: string): string;
var
  idx,p: Integer;
  m,o,c,i,pi: string;
  ii: Cardinal;
begin
  p:=Pos('\',APath);
  if p=0 then
    o:=APath;
  ParseCounterName(APath,m,o,i,pi,c,ii);
  {$IFDEF RAD5PLUS}
  idx:=LocalNames.IndexOfValue(o);
  {$ELSE}
  idx:=ListIndexOfValue(LocalNames,o);
  {$ENDIF}
  if (idx>-1) and (EngNames.Count>0) then
    o:=EngNames.Values[LocalNames.Names[idx]];
  if p>0 then begin
    {$IFDEF RAD5PLUS}
    idx:=LocalNames.IndexOfValue(c);
    {$ELSE}
    idx:=ListIndexOfValue(LocalNames,c);
    {$ENDIF}
    if (idx>-1) and (EngNames.Count>0) then
      c:=EngNames.Values[LocalNames.Names[idx]];
  end;
  if p=0 then
    Result:=o
  else
    Result:=MakeCounterName(m,o,i,pi,c,ii);
end;

function TPerfMonThread.GetInterval: Cardinal;
begin
  FLock.Enter;
  try
    Result:=FInterval;
  finally
    FLock.Leave;
  end;
end;

class function TPerfMonThread.GetLocalizedPerfCounterName(const APath: string): string;
var
  idx,p: Integer;
  m,o,c,i,pi: string;
  ii: Cardinal;
begin
  p:=Pos('\',APath);
  if p=0 then
    o:=APath;
  ParseCounterName(APath,m,o,i,pi,c,ii);
  {$IFDEF RAD5PLUS}
  idx:=EngNames.IndexOfValue(o);
  {$ELSE}
  idx:=ListIndexOfValue(EngNames,o);
  {$ENDIF}
  if (idx>-1) and (LocalNames.Count>0) then
    o:=LocalNames.Values[EngNames.Names[idx]];
  if p>0 then begin
    {$IFDEF RAD5PLUS}
    idx:=EngNames.IndexOfValue(c);
    {$ELSE}
    idx:=ListIndexOfValue(EngNames,c);
    {$ENDIF}
    if (idx>-1) and (LocalNames.Count>0) then
      c:=LocalNames.Values[EngNames.Names[idx]];
  end;
  if p=0 then
    Result:=o
  else
    Result:=MakeCounterName(m,o,i,pi,c,ii);
end;

function TPerfMonThread.GetOnInterval: TPerfMonNotifyEvent;
begin
  FLock.Enter;
  try
    Result:=FOnInterval;
  finally
    FLock.Leave;
  end;
end;

class function TPerfMonThread.MakeCounterName(const AMachine, AObject,
  AInstance, AParentInstance, ACounter: string;
  AInstanceIndex: Cardinal): string;
{var
  n: Cardinal;
  cpe: TPdhCounterPathElements;
  p: PChar;
  r: PDH_STATUS;
begin
  Result:='';
  if not Assigned(PdhMakeCounterPath) then
    Exit;
  if AMachine<>'' then
    cpe.szMachineName:=PChar(AMachine)
  else
    cpe.szMachineName:=nil;
  cpe.szObjectName:=PChar(AObject);
  if AInstance<>'' then
    cpe.szInstanceName:=PChar(AInstance)
  else
    cpe.szInstanceName:=nil;
  if AParentInstance<>'' then
    cpe.szParentInstance:=PChar(AParentInstance)
  else
    cpe.szParentInstance:=nil;
  cpe.dwInstanceIndex:=AInstanceIndex;
  cpe.szCounterName:=PChar(ACounter);
  n:=0;
  PdhMakeCounterPath(@cpe,nil,n,0);
  p:=Allocmem(n);
  try
    r:=PdhMakeCounterPath(@cpe,p,n,0);
    if r=ERROR_SUCCESS then
      Result:=p;
  finally
    FreeMem(p);
  end;
end;}
begin
  if AInstance<>'' then
    Result:=AMachine+'\'+AObject+'('+AInstance+')'+'\'+ACounter
  else
    Result:=AMachine+'\'+AObject+'\'+ACounter;
end;

class procedure TPerfMonThread.ParseCounterName(const APath: string; var AMachine,
  AObject, AInstance, AParentInstance, ACounter: string;
  var AInstanceIndex: Cardinal);
{var
  n: Cardinal;
  cpe: PPdhCounterPathElements;
begin
  AMachine:='';
  AObject:='';
  AInstance:='';
  AParentInstance:='';
  ACounter:='';
  AInstanceIndex:=0;
  if not Assigned(PdhParseCounterPath) then
    Exit;
  n:=0;
  PdhParseCounterPath(PChar(APath),nil,n,0);
  cpe:=Allocmem(n);
  try
    if PdhParseCounterPath(PChar(APath),cpe,n,0)=ERROR_SUCCESS then begin
      AMachine:=cpe.szMachineName;
      AObject:=cpe.szObjectName;
      AInstance:=cpe.szInstanceName;
      AParentInstance:=cpe.szParentInstance;
      ACounter:=cpe.szCounterName;
      AInstanceIndex:=cpe.dwInstanceIndex;
    end;
  finally
    FreeMem(cpe);
  end;
end;}
var
  s: string;
  idx: Integer;
begin
  AMachine:='';
  AInstanceIndex:=0;
  AParentInstance:='';
  AInstance:='';
  s:=APath;
  idx:=PosLast('\',s);
  ACounter:=Trim(Copy(s,idx+1,Length(s)));
  Delete(s,idx,Length(s));
  if Pos('\\',s)=1 then begin
    Delete(s,1,2);
    idx:=Pos('\',s);
    if idx>0 then begin
      AMachine:='\\'+Copy(s,1,idx-1);
      Delete(s,1,idx);
    end;
  end;

  idx:=Pos('(',s);
  if idx>0 then begin
    AInstance:=Trim(Copy(s,idx+1,Length(s)));
    SetLength(AInstance,Length(AInstance)-1);
    s:=Copy(s,1,idx-1);
  end;
  if s[1]='\' then
    Delete(s,1,1);
  AObject:=s;
end;

function TPerfMonThread.ReadCounter(APath: string): Double;
var
  i: Integer;
  l: string;
begin
  FLock.Enter;
  try
    if Pos('\\',APath)=0 then
      APath:='\\'+MachineName+APath;
    l:=GetLocalizedPerfCounterName(APath);
    Result:=0;
    for i:=0 to FList.Count-1 do
      if SameText(PPerformanceCounter(FList[i])^.Path,l) then begin
        Result:=PPerformanceCounter(FList[i])^.Value;
        Break;
      end;
  finally
    FLock.Leave;
  end;
end;

class procedure TPerfMonThread.ReadPerfTexts(ALocalized, AHelp: Boolean; AList: TStringList);
var
  Data: PByte;
  i,ds,dt: Cardinal;
  n,k,s,v: string;
  idx: Integer;
begin
  AList.Clear;
  with OpenRegistryReadOnly do
    try
      Rootkey:=HKEY_LOCAL_MACHINE;
      if ALocalized then begin
        k:='\SOFTWARE\Microsoft\Windows NT\CurrentVersion\Perflib\';
        if OpenKey(k,False) then begin
          GetKeyNames(AList);
          idx:=AList.IndexOf('CurrentLanguage');
          if idx=-1 then begin
            idx:=AList.IndexOf('009');
            if idx>-1 then
              AList.Delete(idx);
            if AList.Count>0 then
              s:=AList[0];
          end else
            s:='CurrentLanguage';
          k:=k+s;
        end;
      end else
        k:='\SOFTWARE\Microsoft\Windows NT\CurrentVersion\Perflib\009';
      if AHelp then
        n:='Help'
      else
        n:='Counter';
      if OpenKey(k,False) then begin
        if ValueExists(n) then begin
          RegQueryValueEx(CurrentKey,PChar(n),nil,@dt,nil,@ds);
          if (dt=REG_MULTI_SZ) and (ds>0) then begin
            Data:=Allocmem(ds);
            try
              RegQueryValueEx(CurrentKey,PChar(n),nil,nil,PBYTE(Data),@ds);
              s:='';
              v:='';
              i:=0;
              repeat
                if Byte(PAnsiChar(Data)[i])=0 then begin
                  if s='' then
                    s:=v
                  else begin
                    AList.Add(Format('%s=%s',[s,v]));
                    s:='';
                  end;
                  v:='';
                end else
                  v:=v+Chr(MakeWord(Byte(PAnsiChar(Data)[i]),Byte(PAnsiChar(Data)[i+1])));
                Inc(i,2);
              until i>=ds;
            finally
              Freemem(Data);
            end;
          end;
        end;
        CloseKey;
      end;
    finally
      Free;
    end;
end;

procedure TPerfMonThread.RemoveCounter(APath: string);
var
  s,l: string;
  p: PChar;
  n: Cardinal;
  i: Integer;
begin
  FLock.Enter;
  try
    if Pos('\\',APath)=0 then
      APath:='\\'+MachineName+APath;
    l:=GetLocalizedPerfCounterName(APath);
    if Pos('*',l)>0 then begin
      n:=0;
      PdhExpandWildCardPath(nil,PChar(l),nil,n,0);
      Inc(n);
      SetLength(s,n);
      if PdhExpandWildCardPath(nil,PChar(l),PChar(s),n,0)=ERROR_SUCCESS then begin
        p:=@s[1];
        while p^<>#0 do begin
          for i:=0 to FList.Count-1 do
            if SameText(PPerformanceCounter(FList[i])^.Path,string(p)) then begin
              PdhRemoveCounter(PPerformanceCounter(FList[i])^.Handle);
              Dispose(PPerformanceCounter(FList[i]));
              FList.Delete(i);
              Break;
            end;
          Inc(p,Length(p)+1);
        end;
      end;
    end else begin
      for i:=0 to FList.Count-1 do
        if SameText(PPerformanceCounter(FList[i])^.Path,l) then begin
          PdhRemoveCounter(PPerformanceCounter(FList[i])^.Handle);
          Dispose(PPerformanceCounter(FList[i]));
          FList.Delete(i);
          Break;
        end;
    end;
  finally
    FLock.Leave;
  end;
end;


procedure TPerfMonThread.SetInterval(const Value: Cardinal);
begin
  FLock.Enter;
  try
    FInterval:=Max(Value,250);
  finally
    FLock.Leave;
  end;
end;

procedure TPerfMonThread.SetOnInterval(const Value: TPerfMonNotifyEvent);
begin
  FLock.Enter;
  try
    FOnInterval:=Value;
  finally
    FLock.Leave;
  end;
end;

initialization
  LocalNames:=TStringList.Create;
  EngNames:=TStringList.Create;
  TPerfMonThread.ReadPerfTexts(True,False,LocalNames);
  TPerfMonThread.ReadPerfTexts(False,False,EngNames);
finalization
  LocalNames.Free;
  EngNames.Free;
end.
