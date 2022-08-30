unit SDK;

interface

uses
  Windows;

type
  TLocal = array of Char;

  TMsgProc = procedure(Msg: PChar; nMsgLen: Integer; nMode: Integer); stdcall;

  TFindProc = function(ProcName: PChar; nNameLen: Integer): Pointer; stdcall;

  TSetProc = function(ProcAddr: Pointer; ProcName: PChar; nNameLen: Integer): Boolean; stdcall;

  TFindObj = function(ObjName: PChar; nNameLen: Integer): TObject; stdcall;

  TGetFunAddr = function(nIndex: Integer): Pointer; stdcall; //20080729 增加

procedure GetIPLocal(sIPaddr: PChar; sLocal: PChar; nLocalLen: Integer); stdcall;

procedure DeCryptString(Src: PChar; Dest: PChar; nSrc: Integer); stdcall;

function Init(AppHandle: HWnd; MsgProc: TMsgProc; FindProc: TFindProc; SetProc: TSetProc; {FindOBj: TFindObj}GetFunAddr: TGetFunAddr): PChar; stdcall;//20080729 修改

procedure UnInit(); stdcall;

implementation

uses
  Module, PlugMain, Share;

procedure DeCryptString(Src: PChar; Dest: PChar; nSrc: Integer);
var
  sEncode: string;
  sDecode: string;
begin
  try
    SetLength(sEncode, nSrc);
    Move(Src^, sEncode[1], nSrc);
    sDecode := DeCodeText(sEncode);
    Move(sDecode[1], Dest^, Length(sDecode) * SizeOf(Char));
  except
  end;
end;

procedure GetIPLocal(sIPaddr: PChar; sLocal: PChar; nLocalLen: Integer);
var
  sIpLocal, sIPaddress: string;
begin
  try
    SetLength(sIPaddress, 15);
    Move(sIPaddr^, sIPaddress[1], 15 * SizeOf(Char));
    sIpLocal := SearchIPLocal(sIPaddress);
    Move(sIpLocal[1], sLocal^, nLocalLen);
  except
  end;
end;
//20080729 修改

function Init(AppHandle: HWnd; MsgProc: TMsgProc; FindProc: TFindProc; SetProc: TSetProc; {FindOBj: TFindObj}GetFunAddr: TGetFunAddr): PChar;
begin
  Result := #0;
  try
    OutMessage := MsgProc;
    FindProcTable := FindProc;
  //FindObjTable := FindOBj;//20080729 注释
    SetProcTable := SetProc;
    if HookDeCodeText = 1 then
    begin
      SetProcAddr(@DeCryptString, 'DeCryptString');
    end;
    if HookSearchIPLocal = 1 then
    begin
      SetProcAddr(@GetIPLocal, 'GetIPLocal');
    end;
    InitPlug(AppHandle);
    Result := PChar(sPlugName);
  except
  end;
end;

procedure UnInit();
begin
  MainOutMessasge(sUnLoadPlug, 0);
end;

end.

