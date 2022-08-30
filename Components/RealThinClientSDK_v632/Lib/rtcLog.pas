{
  @html(<b>)
  Log File Creation
  @html(</b>)
  - Copyright 2004-2014 (c) RealThinClient.com (http://www.realthinclient.com)
  @html(<br><br>)

  This unit gives you thread-safe Log writing support.
}
unit rtcLog;

{$INCLUDE rtcDefs.inc}

interface

uses
{$IFDEF WINDOWS}
  Windows, // FileCreate + FileClose
{$ENDIF}
{$IFDEF POSIX}
  Posix.Unistd, // FileClose
  Posix.PThread, // GetCurrentThreadID
{$ENDIF}

  SysUtils,

{$IFDEF IDE_1}
  FileCtrl,
{$ENDIF}

  rtcTypes,
  rtcSyncObjs;

var
  { Write Logged exception into the Log file?
    Dafault=True. By changing this to False will remove any
    Connection component exceptions from the Log file. }
  LOG_EXCEPTIONS:boolean=True;

  { The RTC SDK can silently handle most exceptions which
    would otherwise cause the components to stop working.
    This is a safety-net which ensures that even bugs in
    the RTC SDK do not cause your apps to crash, but an
    exception getting that far down to the RTC SDK usually
    means something is wrong in the RTC SDK.
    When debugging the RTC SDK, LOG_AV_ERRORS should be
    TRUE in order for all abnormal exceptions to be logged. }
  LOG_AV_ERRORS:boolean={$IFDEF RTC_DEBUG}True{$ELSE}False{$ENDIF};

  { If you want old log files to be deleted after several days,
    you can specify how long (in days) files should be kept.
    If this variable is 0 (default), log files will NOT be deleted. }
  RTC_LOGS_LIVE_DAYS:integer=0;

  { Sub-Folder inside AppFileName's directory where all LOG files will be stored.
    If you want LOG files to be created in the same folder as AppFile (EXE/DLL),
    set LOG_FOLDER to an empty String before calling "StartLog".
    For this value to have any effect, you need to set it before calling "StartLog". }
  LOG_FOLDER:String='LOG';

  { Full path to the LOG folder. If you leave this variable empty (default),
    it will be initialized automatically by using the AppFileName and LOG_FOLDER
    variables, immediately before the first log entry needs to be written.
    If you want your LOG files written to a specific folder by using full path,
    you can do it by setting this variable before the first Log entry is written.
    RTC_LOG_FOLDER should ALWAYS end with '\' on Windows and '/' on other platforms. }
  RTC_LOG_FOLDER:String='';

  { String used to format Date/Time output in RTC LOG. For more information on valid Data/Time
    format strings, please refer to Delphi help about the "FormatDataTime" function. }
  RTC_LOG_DATETIMEFORMAT:String='yyyy-mm-dd hh:nn:ss.zzz; ';

  { Include CurrentThreadID in every LOG entry? }
  RTC_LOG_THREADID:boolean=False;

{$IFDEF RTC_BYTESTRING}
{ Write exception with a short description into the Global App Log file.
  This procedure will have no effect if Log writer not started
  (by calling StartLog) or LOG_EXCEPTIONS is @false }
procedure Log(const s:RtcString; E:Exception; const name:String=''); overload;

{ Write message into the Global App Log file.
  This procedure will have no effect if Log writer not started. }
procedure Log(const s:RtcString; const name:String=''); overload;

{ Write message into the Log file for the current date.
  This procedure will have no effect if Log writer not started. }
procedure XLog(const s:RtcString; const name:String=''); overload;
{$ENDIF}

{ Write exception with a short description into the Global App Log file.
  This procedure will have no effect if Log writer not started
  (by calling StartLog) or LOG_EXCEPTIONS is @false }
procedure Log(const s:RtcWideString; E:Exception; const name:String=''); overload;

{ Write message into the Global App Log file.
  This procedure will have no effect if Log writer not started. }
procedure Log(const s:RtcWideString; const name:String=''); overload;

{ Write message into the Log file for the current date.
  This procedure will have no effect if Log writer not started. }
procedure XLog(const s:RtcWideString; const name:String=''); overload;

{ Before Log() procedures will have any effect,
  you have to call this procedure to start the Log writer.
  Without it, no Log file. }
procedure StartLog;

{ To stop Log file creation, simply call this procedure.
  To continue log writing, call StartLog. }
procedure StopLog;

{ Start using Buffers for Logging, which makes logging a lot faster.
  "MaxSize" is the maximum size (in bytes) the LOG may occupy
  in memory before it has to be dumped to files. @html(<br><br>)

  IMPORTANT!!! When using Buffers for logging, the "name" parameter is case-sensitive,
  which means that a separte Buffer will be created for 'XName' than for 'xname', but
  both buffers will at the end be dumbed into the same file, so you have to be careful
  when using the "name" parameter to always use the exact same value for all LOG entries
  which need to go to the same file, or the order of log entries could get mixed up. }
procedure StartLogBuffers(MaxSize:longint);

{ Stop using Buffers for Logging. }
procedure StopLogBuffers;

{ Dump current Log Buffers to files and release log buffer memory. }
procedure DumpLogBuffers;

implementation

uses
  rtcInfo,
  rtcFastStrings,
  memStringObjList;

var
  ThrCS:TRtcCritSec=nil;
  doLog:boolean=False;
  doBuffers:boolean=False;
  LogMaxBuff:longint;
  LogCurBuff:longint;
  LogBuff:TStringObjList;

procedure StartLog;
  begin
  doLog:=assigned(ThrCS);
  end;

procedure StopLog;
  begin
  doLog:=False;
  end;

procedure Delete_old_logs;
  var
    vdate      :TDatetime;
    sr         :TSearchRec;
    intFileAge :LongInt;
    myfileage  :TDatetime;
  begin
  try
    vdate:= Now - RTC_LOGS_LIVE_DAYS;
    if FindFirst(RTC_LOG_FOLDER + '*.log', faAnyFile - faDirectory, sr) = 0 then
      repeat
        intFileAge := FileAge(RTC_LOG_FOLDER + sr.name);
        if intFileAge > -1 then
          begin
          myfileage:= FileDateToDateTime(intFileAge);
          if myfileage < vdate then
            Delete_File(RTC_LOG_FOLDER + sr.name);
          end;
        until (FindNext(sr) <> 0);
  finally
    FindClose(sr);
    end;
  end;

procedure File_AppendEx(const fname:String; const Data:RtcByteArray);
  var
    f:TRtcFileHdl;
  begin
  f:=FileOpen(fname,fmOpenReadWrite+fmShareDenyNone);
  if f=RTC_INVALID_FILE_HDL then
    begin
    try
      if RTC_LOGS_LIVE_DAYS > 0 then
        Delete_old_logs;
    except
      // ignore problems with file deletion
      end;
    f:=FileCreate(fname);
    end;
  if f<>RTC_INVALID_FILE_HDL then
    try
      if FileSeek(f,0,2)>=0 then
        FileWrite(f,data[0],length(data));
    finally
      FileClose(f);
      end;
  end;

procedure File_Append(const fname:String; const Data:RtcString);
  var
    f:TRtcFileHdl;
  begin
  f:=FileOpen(fname,fmOpenReadWrite+fmShareDenyNone);
  if f=RTC_INVALID_FILE_HDL then
    begin
    try
      if RTC_LOGS_LIVE_DAYS > 0 then
        Delete_old_logs;
    except
      // ignore problems with file deletion
      end;
    f:=FileCreate(fname);
    end;
  if f<>RTC_INVALID_FILE_HDL then
    try
      if FileSeek(f,0,2)>=0 then
        {$IFDEF RTC_BYTESTRING}
        FileWrite(f,data[1],length(data));
        {$ELSE}
        FileWrite(f,RtcStringToBytes(data)[0],length(data));
        {$ENDIF}
    finally
      FileClose(f);
      end;
  end;

procedure PrepareLogFolder;
  begin
  if RTC_LOG_FOLDER='' then
    begin
    if AppFileName='' then
      AppFileName:=ExpandUNCFileName(ParamStr(0));

    RTC_LOG_FOLDER:=ExtractFilePath(AppFileName);
    if Copy(RTC_LOG_FOLDER,length(RTC_LOG_FOLDER),1)<>FOLDER_DELIMITER then
      RTC_LOG_FOLDER:=RTC_LOG_FOLDER+FOLDER_DELIMITER;

    if LOG_FOLDER<>'' then
      begin
      RTC_LOG_FOLDER:=RTC_LOG_FOLDER+LOG_FOLDER;
      if Copy(RTC_LOG_FOLDER,length(RTC_LOG_FOLDER),1)<>FOLDER_DELIMITER then
        RTC_LOG_FOLDER:=RTC_LOG_FOLDER+FOLDER_DELIMITER;
      end;
    end;

  if not DirectoryExists(RTC_LOG_FOLDER) then
    if not CreateDir(RTC_LOG_FOLDER) then
      begin
      RTC_LOG_FOLDER:=GetTempDirectory;
      if Copy(RTC_LOG_FOLDER,length(RTC_LOG_FOLDER),1)<>FOLDER_DELIMITER then
        RTC_LOG_FOLDER:=RTC_LOG_FOLDER+FOLDER_DELIMITER;

      RTC_LOG_FOLDER:=RTC_LOG_FOLDER+LOG_FOLDER;
      if not DirectoryExists(RTC_LOG_FOLDER) then
        CreateDir(RTC_LOG_FOLDER);

      if Copy(RTC_LOG_FOLDER,length(RTC_LOG_FOLDER),1)<>FOLDER_DELIMITER then
        RTC_LOG_FOLDER:=RTC_LOG_FOLDER+FOLDER_DELIMITER;
      end;
  end;

procedure WriteToLogEx(const ext:String; const text:RtcByteArray);
  begin
  PrepareLogFolder;
  File_AppendEx(RTC_LOG_FOLDER+ExtractFileName(AppFileName)+'.'+ext, text);
  end;

procedure WriteToLog(const ext:String; const text:RtcString);
  begin
  PrepareLogFolder;
  File_Append(RTC_LOG_FOLDER+ExtractFileName(AppFileName)+'.'+ext, text);
  end;

procedure WriteToBuffEx(const ext:String; const text:RtcByteArray);
  var
    obj:TObject;
    data:TRtcHugeByteArray;
  begin
  obj:=LogBuff.search(ext);
  if not assigned(obj) then
    begin
    data:=TRtcHugeByteArray.Create;
    LogBuff.insert(ext,data);
    end
  else
    data:=TRtcHugeByteArray(obj);
  data.AddEx(text);
  Inc(LogCurBuff,length(text));
  if LogCurBuff>LogMaxBuff then
    DumpLogBuffers;
  end;

procedure WriteToBuff(const ext:String; const text:RtcString);
  var
    obj:TObject;
    data:TRtcHugeByteArray;
  begin
  obj:=LogBuff.search(ext);
  if not assigned(obj) then
    begin
    data:=TRtcHugeByteArray.Create;
    LogBuff.insert(ext,data);
    end
  else
    data:=TRtcHugeByteArray(obj);
  data.Add(text);
  Inc(LogCurBuff,length(text));
  if LogCurBuff>LogMaxBuff then
    DumpLogBuffers;
  end;

procedure StartLogBuffers(MaxSize:longint);
  begin
  ThrCS.Acquire;
  try
    doBuffers:=True;
    if assigned(LogBuff) then
      DumpLogBuffers
    else
      LogBuff:=tStringObjList.Create(128);
    LogMaxBuff:=MaxSize;
    LogCurBuff:=0;
  finally
    ThrCS.Release;
    end;
  end;

procedure DumpLogBuffers;
  var
    s:String;
    obj:TObject;
    data:TRtcHugeByteArray;
  begin
  ThrCS.Acquire;
  try
    if assigned(LogBuff) then
      begin
      while not LogBuff.Empty do
        begin
        s:=LogBuff.search_min(obj);
        LogBuff.remove(s);
        if assigned(obj) then
          begin
          data:=TRtcHugeByteArray(obj);
          try
            WriteToLogEx(s,data.GetEx);
          except
            end;
          data.Free;
          end;
        end;
      LogCurBuff:=0;
      end;
  finally
    ThrCS.Release;
    end;
  end;

procedure StopLogBuffers;
  begin
  ThrCS.Acquire;
  try
    doBuffers:=False;
    DumpLogBuffers;
    RtcFreeAndNil(LogBuff);
  finally
    ThrCS.Release;
    end;
  end;

procedure XLog(const s:RtcWideString; const name:String='');
  var
    d:TDateTime;
    fname:String;
    s2:RtcString;
  begin
  if not doLog then Exit; // Exit here !!!!

  d:=Now;
  if RTC_LOG_DATETIMEFORMAT<>'' then
    begin
    if RTC_LOG_THREADID then
      s2:= Utf8Encode(IntToStr(longword(GetCurrentThreadId))+'#'+FormatDateTime(RTC_LOG_DATETIMEFORMAT,d))
    else
      s2:= Utf8Encode(FormatDateTime(RTC_LOG_DATETIMEFORMAT,d));
    end
  else if RTC_LOG_THREADID then
    s2:= Int2Str(longword(GetCurrentThreadId))+'#'
  else
    s2:= '';

  if name<>'' then
    fname:=FormatDateTime('yyyy_mm_dd',d)+'.'+name+'.log'
  else
    fname:=FormatDateTime('yyyy_mm_dd',d)+'.log';

  ThrCS.Acquire;
  try
    if doBuffers then
      WriteToBuff(fname, s2+Utf8Encode(s)+#13#10 )
    else
      WriteToLog(fname, s2+Utf8Encode(s)+#13#10 );
  except
    end;
  ThrCS.Release;
  end;

procedure Log(const s:RtcWideString; const name:String='');
  var
    d:TDateTime;
    fname:String;
    s2:RtcString;
  begin
  if not doLog then Exit; // Exit here !!!!

  d:=Now;
  if RTC_LOG_DATETIMEFORMAT<>'' then
    begin
    if RTC_LOG_THREADID then
      s2:=Utf8Encode(IntToStr(longword(GetCurrentThreadId))+'#'+FormatDateTime(RTC_LOG_DATETIMEFORMAT,d))
    else
      s2:=Utf8Encode(FormatDateTime(RTC_LOG_DATETIMEFORMAT,d));
    end
  else if RTC_LOG_THREADID then
    s2:=Int2Str(longword(GetCurrentThreadId))+'#'
  else
    s2:='';

  if name<>'' then
    fname:=name+'.log'
  else
    fname:='log';

  ThrCS.Acquire;
  try
    if doBuffers then
      WriteToBuff(fname, s2+Utf8Encode(s)+#13#10 )
    else
      WriteToLog(fname, s2+Utf8Encode(s)+#13#10 );
  except
    end;
  ThrCS.Release;
  end;

procedure Log(const s:RtcWideString; E:Exception; const name:String='');
  begin
  if LOG_EXCEPTIONS then
    Log(s+' Exception! '+E.ClassName+': '+E.Message, name);
  end;

{$IFDEF RTC_BYTESTRING}

procedure XLog(const s:RtcString; const name:String='');
  var
    d:TDateTime;
    fname:String;
    s2:RtcString;
  begin
  if not doLog then Exit; // Exit here !!!!

  d:=Now;
  if RTC_LOG_DATETIMEFORMAT<>'' then
    begin
    if RTC_LOG_THREADID then
      s2:= Utf8Encode(IntToStr(longword(GetCurrentThreadId))+'#'+FormatDateTime(RTC_LOG_DATETIMEFORMAT,d))
    else
      s2:= Utf8Encode(FormatDateTime(RTC_LOG_DATETIMEFORMAT,d));
    end
  else if RTC_LOG_THREADID then
    s2:= Int2Str(longword(GetCurrentThreadId))+'#'
  else
    s2:= '';

  if name<>'' then
    fname:=FormatDateTime('yyyy_mm_dd',d)+'.'+name+'.log'
  else
    fname:=FormatDateTime('yyyy_mm_dd',d)+'.log';

  ThrCS.Acquire;
  try
    if doBuffers then
      WriteToBuff(fname, s2+s+#13#10 )
    else
      WriteToLog(fname, s2+s+#13#10 );
  except
    end;
  ThrCS.Release;
  end;

procedure Log(const s:RtcString; const name:String='');
  var
    d:TDateTime;
    fname:String;
    s2:RtcString;
  begin
  if not doLog then Exit; // Exit here !!!!

  d:=Now;
  if RTC_LOG_DATETIMEFORMAT<>'' then
    begin
    if RTC_LOG_THREADID then
      s2:=Utf8Encode(IntToStr(longword(GetCurrentThreadId))+'#'+FormatDateTime(RTC_LOG_DATETIMEFORMAT,d))
    else
      s2:=Utf8Encode(FormatDateTime(RTC_LOG_DATETIMEFORMAT,d));
    end
  else if RTC_LOG_THREADID then
    s2:=Int2Str(longword(GetCurrentThreadId))+'#'
  else
    s2:='';

  if name<>'' then
    fname:=name+'.log'
  else
    fname:='log';

  ThrCS.Acquire;
  try
    if doBuffers then
      WriteToBuff(fname, s2+s+#13#10 )
    else
      WriteToLog(fname, s2+s+#13#10 );
  except
    end;
  ThrCS.Release;
  end;

procedure Log(const s:RtcString; E:Exception; const name:String='');
  begin
  if LOG_EXCEPTIONS then
    Log(s+' Exception! '+RtcString(E.ClassName)+': '+RtcString(E.Message), name);
  end;
{$ENDIF}

initialization
ThrCS:=TRtcCritSec.Create;
{$IFDEF RTC_DEBUG}StartLog;{$ENDIF}
finalization
StopLog;
StopLogBuffers;
RtcFreeAndNil(ThrCS);
end.
