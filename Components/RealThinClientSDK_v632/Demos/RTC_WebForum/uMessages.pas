unit uMessages;

interface

function GetMsg(msg_id : string) : string;

implementation

uses
  rtcInfo, IniFiles, SysUtils;

var
  Messages : TMemIniFile = nil;

function GetMsg(msg_id : string) : string;
  begin
  Result := Trim(Messages.ReadString('Messages', msg_id, msg_id));
  end;

initialization
Messages := TMemIniFile.Create(ExtractFilePath(AppFileName) + 'messages.ini');
finalization
FreeAndNil(Messages);
end.
