unit Module;

interface

uses
  Windows, SysUtils, Grobal2, SDK, Share, HUtil32, WinSock, DESUnit;

procedure OutMessage(Msg: PChar; nMsgLen: Integer; nMode: Integer); stdcall;
function GetProcAddr(sProcName: string): Pointer;
function SetProcAddr(ProcAddr: Pointer; sProcName: string): Boolean;
function GetObjAddr(sObjName: string): TObject;
function GetResourceString(StrType: TStrType): PChar; stdcall;
function GetNextDirection(sX, sY, dx, dy: Integer): Byte; stdcall;
function GetExVersionNO(nVersionDate: Integer; var nOldVerstionDate: Integer): Integer; stdcall;
function GetGoldShape(nGold: Integer): Word; stdcall;
function IsExpired(): Boolean;
function GetValNameNo(sText: string): Integer; stdcall;
function CheckUserItems(nIdx: Integer; StdItem: pTStdItem): Boolean; stdcall;
function GetItemNumber(): Integer; stdcall;
function GetItemNumberEx(): Integer; stdcall;
function FilterShowName(sName: string): string; stdcall;
function CheckGuildName(sGuildName: string): Boolean; stdcall;
procedure GetLocalIP(var IP: array of Char; IPSize: Integer); stdcall;

var
  MainOutMessage            : TMsgProc;
  FindProcTable             : TFindProc;
  FindObjTable              : TFindObj;
  SetProcTable              : TSetProc;

implementation

procedure OutMessage(Msg: PChar; nMsgLen: Integer; nMode: Integer);
begin

end;

function GetProcAddr(sProcName: string): Pointer;
begin
  Result := nil;
  if Assigned(FindProcTable) then
    Result := FindProcTable(PChar(sProcName), Length(sProcName));
end;

function GetObjAddr(sObjName: string): TObject;
begin
  Result := nil;
  if Assigned(FindObjTable) then begin
    Result := FindObjTable(PChar(sObjName), Length(sObjName));
  end;
end;

function SetProcAddr(ProcAddr: Pointer; sProcName: string): Boolean;
begin
  Result := False;
  if Assigned(SetProcTable) then begin
    Result := SetProcTable(ProcAddr, PChar(sProcName), Length(sProcName));
  end;
end;

function GetResourceString(StrType: TStrType): PChar;
begin
 ChangeLabelVerColor(2);
 //显示在M2上面的更新日期 2018-12-17 修改
 case StrType of
  t_sUpDateTime: Result := PChar(g_DConfig.sUpDateTime);
 end;

end;

function GetNextDirection(sX, sY, dx, dy: Integer): Byte;
var
  flagx, flagy              : Integer;
begin
  Result := DR_DOWN;

  if sX < dx then
    flagx := 1
  else if sX = dx then
    flagx := 0
  else
    flagx := -1;
  if abs(sY - dy) > 2 then
    if (sX >= dx - 1) and (sX <= dx + 1) then
      flagx := 0;
  if sY < dy then
    flagy := 1
  else if sY = dy then
    flagy := 0
  else
    flagy := -1;
  if abs(sX - dx) > 2 then
    if (sY > dy - 1) and (sY <= dy + 1) then
      flagy := 0;
  if (flagx = 0) and (flagy = -1) then
    Result := DR_UP;
  if (flagx = 1) and (flagy = -1) then
    Result := DR_UPRIGHT;
  if (flagx = 1) and (flagy = 0) then
    Result := DR_RIGHT;
  if (flagx = 1) and (flagy = 1) then
    Result := DR_DOWNRIGHT;
  if (flagx = 0) and (flagy = 1) then
    Result := DR_DOWN;
  if (flagx = -1) and (flagy = 1) then
    Result := DR_DOWNLEFT;
  if (flagx = -1) and (flagy = 0) then
    Result := DR_LEFT;
  if (flagx = -1) and (flagy = -1) then
    Result := DR_UPLEFT;
end;

function GetExVersionNO(nVersionDate: Integer; var nOldVerstionDate: Integer): Integer;
begin
  Result := 0;
  nOldVerstionDate := 0;
  nOldVerstionDate := nVersionDate;
end;

function GetGoldShape(nGold: Integer): Word;
begin
  Result := 0;
end;

function IsExpired(): Boolean;
var
  Year, Month, Day          : Word;
begin
  Result := False;
  DecodeDate(Date, Year, Month, Day);
  if ((Year >= ENDYEAR^) and ((Month * 30 + Day) > ENDMONTH^ * 30 + ENDDAY^)) or
    ((Year <= ENDYEARMIN^) and ((Month * 30 + Day) < ENDMONTHMIN^ * 30 + ENDDAYMIN^)) then begin
    g_DConfig.boExpired := True;
    Result := True;
  end;
end;

function GetValNameNo(sText: string): Integer;
var
  nValNo                    : Integer;
begin
  Result := -1;
  if Length(sText) >= 2 then begin
    if UpCase(sText[1]) = 'P' then begin
      nValNo := Str_ToInt(sText[2], -1);
      if nValNo < 10 then
        Result := nValNo;
    end;

    if UpCase(sText[1]) = 'G' then begin
      if Length(sText) = 3 then begin
        nValNo := Str_ToInt(Copy(sText, 2, 2), -1);
        if nValNo < 100 then
          Result := nValNo + 100;
      end
      else begin
        nValNo := Str_ToInt(sText[2], -1);
        if nValNo < 10 then
          Result := nValNo + 100;
      end;
    end;

    if UpCase(sText[1]) = 'D' then begin
      nValNo := Str_ToInt(sText[2], -1);
      if nValNo < 10 then
        Result := nValNo + 200;
    end;

    if UpCase(sText[1]) = 'M' then begin
      if Length(sText) = 3 then begin
        nValNo := Str_ToInt(Copy(sText, 2, 2), -1);
        if nValNo < 100 then
          Result := nValNo + 300;
      end
      else begin
        nValNo := Str_ToInt(sText[2], -1);
        if nValNo < 10 then
          Result := nValNo + 300;
      end;
    end;
    if UpCase(sText[1]) = 'I' then begin
      if Length(sText) = 3 then begin
        nValNo := Str_ToInt(Copy(sText, 2, 2), -1);
        if nValNo < 100 then
          Result := nValNo + 400;
      end
      else begin
        nValNo := Str_ToInt(sText[2], -1);
        if nValNo < 10 then
          Result := nValNo + 400;
      end;
    end;

    if UpCase(sText[1]) = 'A' then begin
      if Length(sText) = 3 then begin
        nValNo := Str_ToInt(Copy(sText, 2, 2), -1);
        if nValNo < 100 then
          Result := nValNo + 500;
      end
      else begin
        nValNo := Str_ToInt(sText[2], -1);
        if nValNo < 10 then
          Result := nValNo + 500;
      end;
    end;

    if UpCase(sText[1]) = 'S' then begin
      if Length(sText) = 3 then begin
        nValNo := Str_ToInt(Copy(sText, 2, 2), -1);
        if nValNo < 100 then
          Result := nValNo + 600;
      end
      else begin
        nValNo := Str_ToInt(sText[2], -1);
        if nValNo < 10 then
          Result := nValNo + 600;
      end;
    end;
  end;
end;

function CheckUserItems(nIdx: Integer; StdItem: pTStdItem): Boolean;
begin
  Result := False;
  case nIdx of
    U_DRESS: if StdItem.StdMode in [10, 11] then
        Result := True;
    U_WEAPON: if (StdItem.StdMode = 5) or (StdItem.StdMode = 6) then
        Result := True;
    U_RIGHTHAND: if (StdItem.StdMode = 29) or (StdItem.StdMode = 30) or (StdItem.StdMode = 28) then
        Result := True;
    U_NECKLACE: if (StdItem.StdMode = 19) or (StdItem.StdMode = 20) or (StdItem.StdMode = 21) then
        Result := True;
    U_HELMET: if StdItem.StdMode = 15 then
        Result := True;
    U_ARMRINGL: if (StdItem.StdMode = 24) or (StdItem.StdMode = 25) or (StdItem.StdMode = 26) then
        Result := True;
    U_ARMRINGR: if (StdItem.StdMode = 24) or (StdItem.StdMode = 26) then
        Result := True;
    U_RINGL, U_RINGR: if (StdItem.StdMode = 22) or (StdItem.StdMode = 23) then
        Result := True;
{$IF not VER_176}
    U_BUJUK: if (StdItem.StdMode = 25) or (StdItem.StdMode = 51) then
        Result := True;
    U_BELT: if (StdItem.StdMode = 54) or (StdItem.StdMode = 64) then
        Result := True;
    U_BOOTS: if (StdItem.StdMode = 52) or (StdItem.StdMode = 62) then
        Result := True;
    U_CHARM: if (StdItem.StdMode = 53) or (StdItem.StdMode = 63) then
        Result := True;
{$IFEND VER_176}
  end;
end;

function GetItemNumber(): Integer;
begin
  Inc(g_Config.nItemNumber);
  if g_Config.nItemNumber > (High(Integer) div 2 - 1) then
    g_Config.nItemNumber := 1;
  Result := g_Config.nItemNumber;
end;

function GetItemNumberEx(): Integer;
begin
  Inc(g_Config.nItemNumberEx);
  if g_Config.nItemNumberEx < High(Integer) div 2 then
    g_Config.nItemNumberEx := High(Integer) div 2;
  if g_Config.nItemNumberEx > (High(Integer) - 1) then
    g_Config.nItemNumberEx := High(Integer) div 2;
  Result := g_Config.nItemNumberEx;
end;

function FilterShowName(sName: string): string;
var
  i                         : Integer;
  SC                        : string;
  boCheckOK                 : Boolean;
begin
  Result := '';
  SC := '';
  boCheckOK := False;
  for i := 1 to Length(sName) do begin
    if ((sName[i] >= '0') and (sName[i] <= '9')) or (sName[i] = '-') then begin
      Result := Copy(sName, 1, i - 1);
      SC := Copy(sName, i, Length(sName));
      boCheckOK := True;
      Break;
    end;
  end;
  if not boCheckOK then
    Result := sName;
end;

function CheckGuildName(sGuildName: string): Boolean;
var
  i                         : Integer;
begin
  Result := True;
  if Length(sGuildName) > g_Config.nGuildNameLen then begin
    Result := False;
    Exit;
  end;
  for i := 1 to Length(sGuildName) do begin
    if (sGuildName[i] < '0' {30}) or
      (sGuildName[i] = '/' {2F}) or
      (sGuildName[i] = '\' {5C}) or
      (sGuildName[i] = ':' {3A}) or
      (sGuildName[i] = '*') or
      (sGuildName[i] = ' ') or
      (sGuildName[i] = '"') or
      (sGuildName[i] = '''') or
      (sGuildName[i] = '<' {3C}) or
      (sGuildName[i] = '|' {7C}) or
      (sGuildName[i] = '?' {3F}) or
      (sGuildName[i] = '>' {3E}) then begin
      Result := False;
    end;
  end;
end;

procedure GetLocalIP(var IP: array of Char; IPSize: Integer); stdcall;
type
  TaPInAddr = array[0..10] of PInAddr;
  PaPInAddr = ^TaPInAddr;
var
  phe                       : PHostEnt;
  pptr                      : PaPInAddr;
  Buffer                    : array[0..63] of Char;
  i                         : Integer;
  GInitData                 : TWSADATA;
  sIP                       : string;
resourcestring
  sKey                      = 'Time@))&)!!%';
begin
  try
    WSAStartup($101, GInitData);
    FillChar(IP, IPSize, #0);
    GetHostName(Buffer, SizeOf(Buffer));
    phe := GetHostByName(Buffer);
    if phe = nil then Exit;
    pptr := PaPInAddr(phe^.h_addr_list);
    i := 0;
    while pptr^[i] <> nil do begin
      sIP := StrPas(inet_ntoa(pptr^[i]^));
      sIP := MyEnString(sIP, sKey);
      Move(sIP[1], IP, Length(sIP));
      Inc(i);
    end;
    WSACleanup;
  except
  end;
end;


end.

