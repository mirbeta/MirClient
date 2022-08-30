unit ClassModule;

interface

uses
  SysUtils, Grobal2, SDK, EDcode;

type
  TRunSocket = class
  private
    procedure RunA;
  public
    procedure Run();
  end;

  TUserEngine = class
  private
  public
    procedure Run();
  end;

procedure CopyCodeToFile();
function GetCodeBlock(var Code: PChar): Integer;
procedure CopyCodeToBuffer(Buffer: PChar);

var
  RunSocket                 : TRunSocket;
  SocketRun                 : TClassProc;
  UserEngine                : TUserEngine;
  EngineRun                 : TClassProc;
  ProcBlock                 : PChar;
  ProcArray                 : array[0..100] of Pointer;

implementation

uses Module, Share;

procedure CopyCodeToFile();
var
  StartPoint                : PChar;
  nCodeSize                 : Integer;
  FileHandle                : Integer;
  sFileName                 : string;
begin
  StartPoint := @TRunSocket.RunA;
  nCodeSize := 0;
  while (nCodeSize < 1000) do begin
    if StartPoint[nCodeSize] = Char($C3) then begin
      Inc(nCodeSize);
      Break;
    end;
    Inc(nCodeSize);
  end;
  sFileName := 'Code.bin'; {Code.bin}
  if FileExists(sFileName) then
    FileHandle := FileOpen(sFileName, fmOpenWrite or fmShareDenyNone)
  else
    FileHandle := FileCreate(sFileName);

  if FileHandle > 0 then
    FileWrite(FileHandle, StartPoint^, nCodeSize);

  FileClose(FileHandle);
  //GetMem(ProcBlock, nCodeSize);
  //Move(StartPoint^, ProcBlock^, nCodeSize);
end;

procedure CopyCodeToBuffer(Buffer: PChar);
var
  StartPoint                : PChar;
  nCodeSize                 : Integer;
  FileHandle                : Integer;
  sFileName                 : string;
begin
  StartPoint := @TRunSocket.RunA;
  nCodeSize := 0;
  while (nCodeSize < 1000) do begin
    if StartPoint[nCodeSize] = Char($C3) then begin
      Inc(nCodeSize);
      Break;
    end;
    Inc(nCodeSize);
  end;
  //GetMem(ProcBlock, nCodeSize);
  Move(StartPoint^, Buffer^, nCodeSize);
end;

function GetCodeBlock(var Code: PChar): Integer;
var
  nCodeSize                 : Integer;
  boGetOK                   : Boolean;
begin
  Result := -1;
  boGetOK := False;
  nCodeSize := 0;
  Code := @TRunSocket.RunA;
  while (nCodeSize < 1000) do begin
    if Code[nCodeSize] = Char($C3) then begin
      Inc(nCodeSize);
      boGetOK := True;
      Break;
    end;
    Inc(nCodeSize);
  end;
  if boGetOK then Result := nCodeSize;
end;

procedure TRunSocket.RunA;
begin
  if Assigned(SocketRun) then
    SocketRun(Self);
end;

procedure TRunSocket.Run;
begin
  if not g_DConfig.boExpired and Assigned(SocketRun) then
    SocketRun(Self);
end;

{ TUserEngine }

procedure TUserEngine.Run;
begin
  if not g_DConfig.boExpired and Assigned(EngineRun) then
    EngineRun(Self);
end;

initialization
  begin
    FillChar(ProcArray, SizeOf(ProcArray), #0);
    ProcArray[0] := @GetCodeBlock;
  end;

finalization
  begin

  end;
end.

