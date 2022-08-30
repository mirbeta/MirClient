unit Module;

interface

uses
  SDK;

procedure MainOutMessasge(Msg: string; nMode: Integer);

function GetProcAddr(sProcName: string): Pointer;

function SetProcAddr(ProcAddr: Pointer; sProcName: string): Boolean;

function GetObjAddr(sObjName: string): TObject;

var
  OutMessage: TMsgProc;
  FindProcTable: TFindProc;
  FindObjTable: TFindObj;
  SetProcTable: TSetProc;

implementation

procedure MainOutMessasge(Msg: string; nMode: Integer);
begin
  if Assigned(OutMessage) then
  begin
    OutMessage(PChar(Msg), Length(Msg), nMode);
  end;
end;

function GetProcAddr(sProcName: string): Pointer;
begin
  Result := nil;
  if Assigned(FindProcTable) then
  begin
    Result := FindProcTable(PChar(sProcName), length(sProcName));
  end;
end;

function GetObjAddr(sObjName: string): TObject;
begin
  Result := nil;
  if Assigned(FindObjTable) then
  begin
    Result := FindObjTable(PChar(sObjName), length(sObjName));
  end;
end;

function SetProcAddr(ProcAddr: Pointer; sProcName: string): Boolean;
begin
  Result := False;
  if Assigned(SetProcTable) then
  begin
    Result := SetProcTable(ProcAddr, PChar(sProcName), length(sProcName));
  end;
end;

end.

