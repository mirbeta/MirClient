unit uJclExceptionLog;

// JclDebug 输出记录 一路随云
interface

uses
  Classes, JclDebug, SysUtils, DateUtils, SyncObjs,uSyncObj;

Procedure LogException(Sender: TObject; E: Exception);
procedure LogAdd(const Text:String);
implementation

uses Windows;

const
  LogPath = '.\91ExceptionLog.txt';

var
  ExcetionLogFile: TStringList;
  Lock : TFixedCriticalSection;
procedure LogAdd(const Text:String);
var
  i: integer;
  H: TextFile;
  BoFileOpened:Boolean;
begin
  Lock.Enter;
  Try
    Try
      BoFileOpened := False;
      if FileExists(LogPath) then
      begin
        AssignFile(H, LogPath);
        Append(H);
      end
      else
      begin
        AssignFile(H, LogPath);
        Rewrite(H);
      end;
      BoFileOpened := True;
    Except
      on E:Exception do
      begin
        BoFileOpened := False;
      end;
    End;

    if BoFileOpened then
    begin
      Try
        Writeln(H, Text );
      Finally
        CloseFile(H);
      End;
    end;
  Finally
    Lock.Leave;
  End;

end;

Procedure LogException(Sender: TObject; E: Exception);
var
  i: integer;
  H: TextFile;
  BoFileOpened:Boolean;
begin
  Lock.Enter;
  Try
    Try
      BoFileOpened := False;
      if FileExists(LogPath) then
      begin
        AssignFile(H, LogPath);
        Append(H);
      end
      else
      begin
        AssignFile(H, LogPath);
        Rewrite(H);
      end;
      BoFileOpened := True;
    Except
      on E:Exception do
      begin
        BoFileOpened := False;
      end;
    End;

    if BoFileOpened then
    begin
      Try
        Writeln(H, '===Start,Time  ' + DateTimeToStr(Now()) + '==========');
        Writeln(H, 'Exception Message:' + E.Message);
        Writeln(H, '--------------------------');
        Writeln(H, 'Stack list:');
        JclLastExceptStackListToStrings(ExcetionLogFile, false, false, True, True);
        for i := 0 to ExcetionLogFile.Count - 1 do
        begin
          Writeln(H, ExcetionLogFile[i]);
        end;
      Finally
        ExcetionLogFile.Clear;
        CloseFile(H);
      End;
    end;
  Finally
    Lock.Leave;
  End;

end;

initialization

begin
  Lock := TFixedCriticalSection.Create;
  ExcetionLogFile := TStringList.Create;
  Include(JclStackTrackingOptions, stRawMode);
  Include(JclStackTrackingOptions, stStaticModuleList);
  JclStartExceptionTracking;
end

finalization

begin
  JclStopExceptionTracking;
  ExcetionLogFile.Free;
  Lock.Free;
end

end.
