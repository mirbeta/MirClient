unit uLog;

interface

uses Windows, Classes, Sysutils, SyncObjs, DateUtils, Forms,uSyncObj;

const
  _LOG_INTERVAL = 3000;

type
  TLogLevel = (llMessage, llFatal, llError, llWarning, llDebug);

  TLogger = class
  private
    FLogDate: TDateTime;
    FPrefixFileName,
    FPath,
    FLogFile: String;
    FLogs: TFixedThreadList;
    FList: TList;
    FLogFileStream: TFileStream;
    FWritePrefix: Boolean;
    function GetLogFileName: String;
    procedure Save;
  public
    constructor Create(const APath, APrefixFileName: String);
    destructor Destroy; override;
    procedure Log(const AMsg: string; ALevel: TLogLevel);
    class procedure AddLog(const AMsg: string; ALevel: TLogLevel = llMessage);
    property WritePrefix: Boolean read FWritePrefix write FWritePrefix;
  end;

{$IFDEF LOGIN}
procedure InitLogger();
procedure CleanUpLogger();
{$ENDIF}
implementation

type
  TLogItem = record
    Date: TDate;
    Level: TLogLevel;
    Length: Integer;
    Value: array[0..0] of AnsiChar;
  end;
  PTLogItem = ^TLogItem;

  TLoggerThread = class(TThread)
  private
    class var FThread: TLoggerThread;
    {$IFNDEF LOGIN}
    class constructor Create;
    class destructor Destroy;
    {$ENDIF}
  private
    FLogs: TFixedThreadList;
    FEvent: TEvent;
    FDefault: TLogger;
    constructor Create;
    destructor Destroy; override;
    procedure Save;
    procedure ClearAll;
    procedure ClearHistory(const APath: String);
  protected
    procedure Execute; override;
  end;
{$IFDEF LOGIN}
procedure InitLogger();
begin
  TLoggerThread.FThread := TLoggerThread.Create;
end;

procedure CleanUpLogger();
begin
  TLoggerThread.FThread.Terminate;
  FreeAndNil(TLoggerThread.FThread);
end;
{$ENDIF}

constructor TLogger.Create(const APath, APrefixFileName: String);
begin
  FPrefixFileName := APrefixFileName;
  FPath := APath;
  if not DirectoryExists(FPath) then
    ForceDirectories(FPath);

  FLogs := TFixedThreadList.Create;
  FList := TList.Create;
  FLogDate := Date;
  FLogFile := GetLogFileName;
  FLogFileStream := nil;
  FWritePrefix := True;
  if TLoggerThread.FThread <> nil then
    TLoggerThread.FThread.FLogs.Add(Self);
end;

destructor TLogger.Destroy;
begin
  if TLoggerThread.FThread <> nil then
    TLoggerThread.FThread.FLogs.Remove(Self);
  FreeAndNil(FLogs);
  FreeAndNil(FList);
  //ÄÚ´æÐ¹Â¶  ËæÔÆ¼ÓÉÏ
  if FLogFileStream <> nil then
    FreeAndNil(FLogFileStream);
  inherited;
end;

function TLogger.GetLogFileName: String;
begin
  Result := FPath + FPrefixFileName + FormatDateTime('YYYY-MM-DD', FLogDate) + '(' + IntToStr(Application.Handle) +').log';
end;

procedure TLogger.Save;
var
  AList: TList;
  I: Integer;
  ALogItem: PTLogItem;
begin
  AList := FLogs.LockList;
  try
    FList.Assign(AList);
    AList.Clear;
  finally
    FLogs.UnlockList;
  end;

  for I := 0 to FList.Count - 1 do
  begin
    ALogItem := FList[I];
    try
      if ALogItem.Date <> Date then
      begin
        FLogDate := Date;
        FLogFile := GetLogFileName;
        if FLogFileStream <> nil then
          FreeAndNil(FLogFileStream);
      end;
      if FLogFileStream = nil then
      begin
        if not FileExists(FLogFile) then
        begin
          FLogFileStream := TFileStream.Create(FLogFile, fmCreate);
          FLogFileStream.WriteBuffer(#$FF#$FE, 2);
          FreeAndNil(FLogFileStream);
        end;
        FLogFileStream := TFileStream.Create(FLogFile, fmOpenWrite or fmShareDenyWrite);
        FLogFileStream.Position := FLogFileStream.Size;
      end;
      FLogFileStream.WriteBuffer(ALogItem.Value[0], ALogItem.Length);
      FreeMem(ALogItem, SizeOf(TLogItem) + ALogItem.Length);
    except
    end;
  end;
  FList.Clear;
end;

class procedure TLogger.AddLog(const AMsg: string; ALevel: TLogLevel);
begin
  TLoggerThread.FThread.FDefault.Log(AMsg, ALevel);
end;

procedure TLogger.Log(const AMsg: string; ALevel: TLogLevel);
var
  ALogItem: PTLogItem;
  ALogText: string;
  ALength: Integer;
begin
  ALogText := AMsg + #$D#$A;
  if FWritePrefix then
  begin
    case ALevel of
      llMessage: ALogText := FormatDateTime('[hh:mm:ss] ', Time) + ALogText;
      llFatal: ALogText := FormatDateTime('[hh:mm:ss](ÖÂÃü) ', Time) + ALogText;
      llError: ALogText := FormatDateTime('[hh:mm:ss](´íÎó) ', Time) + ALogText;
      llWarning: ALogText := FormatDateTime('[hh:mm:ss](¾¯¸æ) ', Time) + ALogText;
      llDebug: ALogText := FormatDateTime('[hh:mm:ss](µ÷ÊÔ) ', Time) + ALogText;
    end;
  end;
  ALength := Length(ALogText) shl 1;
  GetMem(ALogItem, SizeOf(TLogItem) + ALength);
  ALogItem^.Date := Date;
  ALogItem^.Level := ALevel;
  ALogItem^.Length := ALength;
  Move(ALogText[1], ALogItem^.Value[0], ALength);
  FLogs.Add(ALogItem);
end;

{ TLoggerThread }
{$IFNDEF LOGIN}
class constructor TLoggerThread.Create;
begin
  TLoggerThread.Create;
end;

class destructor TLoggerThread.Destroy;
begin
  FThread.Terminate;
  FreeAndNil(FThread);
end;
{$ENDIF}

constructor TLoggerThread.Create;
begin
  FLogs := TFixedThreadList.Create;
  FEvent := TEvent.Create(nil, true, False, '');
  inherited Create(False);
  FreeOnTerminate := False;
  FThread := Self;
  FDefault := TLogger.Create(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'Log\', '');
  ClearHistory(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'Log\');
end;

destructor TLoggerThread.Destroy;
begin
  FEvent.SetEvent;
  while not Finished do
    Sleep(10);
  FThread := nil;
  ClearAll;
  FreeAndNil(FLogs);
  FreeAndNil(FEvent);
  inherited;
end;

procedure TLoggerThread.ClearAll;
var
  AList: TList;
  I: Integer;
begin
  AList := FLogs.LockList;
  try
    for I := 0 to AList.Count - 1 do
      TLogger(AList[I]).Free;
    AList.Clear;
  finally
    FLogs.UnlockList;
  end;
end;

procedure TLoggerThread.ClearHistory(const APath: String);

  procedure DoSearchFile(ALogs: TStrings);
  var
    Found: Integer;
    SearchRec: TSearchRec;
  begin
    Found := FindFirst(APath + '*.log', faAnyFile, SearchRec);
    while Found = 0 do
    begin
      ALogs.Add(SearchRec.Name);
      Found := FindNext(SearchRec);
    end;
    FindClose(SearchRec);
  end;

var
  AFiles: TStrings;
  I: Integer;
  S: String;
  D: TDateTime;
  AFormatSettings: TFormatSettings;
begin
  AFormatSettings := TFormatSettings.Create;
  AFormatSettings.ShortDateFormat := 'yyyy-MM-dd';
  AFormatSettings.LongDateFormat := 'yyyy-MM-dd hh:mm:ss';
  AFormatSettings.DateSeparator := '-';
  AFiles := TStringList.Create;
  try
    DoSearchFile(AFiles);
    for I := 0 to AFiles.Count - 1 do
    begin
      try
        S := Copy(AFiles[I], 1, 10);
        D := StrToDateTimeDef(S, 0, AFormatSettings);
        if (D > 0) and (DateuTILS.DaysBetween(D, Now) > 7) then
          DeleteFile(APath + AFiles[I]);
      except
      end;
    end;
  finally
    AFiles.Free;
  end;
end;

procedure TLoggerThread.Execute;
begin
  while not Terminated do
  begin
    FEvent.WaitFor(_LOG_INTERVAL);
    Save;
  end;
end;

procedure TLoggerThread.Save;
var
  AList: TList;
  I: Integer;
begin
  AList := FLogs.LockList;
  try
    for I := 0 to AList.Count - 1 do
      TLogger(AList[I]).Save;
  finally
    FLogs.UnlockList;
  end;
end;

end.
