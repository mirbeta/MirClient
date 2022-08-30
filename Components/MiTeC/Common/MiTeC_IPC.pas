{$INCLUDE Compilers.inc}

{*******************************************************}
{      MiTeC Inter-Process Communication Routines       }
{                                                       }
{          Copyright (c) 2014-2017 Michal Mutl          }
{                                                       }
{*******************************************************}


unit MiTeC_IPC;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils, WinAPI.Messages;
     {$ELSE}
     Windows, SysUtils, Messages;
     {$ENDIF}


const
  cNull: AnsiChar = #0;
  cStrType: AnsiChar = {$IFDEF UNICODE}#2{$ELSE}#1{$ENDIF};

type
  TIPCType = (ipcUnknown,ipcParams,ipcData);

  TIPCMessage = record
    UID: Int64;
    Typ: TIPCType;
    IntData: Int64;
    StrData: string;
  end;

  {$IFDEF FPC}
  TWMCopyData = record
    Msg: Cardinal;
    MsgFiller: TDWordFiller;
    From: HWND;
    CopyDataStruct: PCopyDataStruct;
    Result: LRESULT;
  end;
  {$ENDIF}

procedure DeactivateInstanceCheck;
procedure ActivateInstanceCheck(const AAppID: string; AGlobal: Boolean = False);

function InstanceExists(const AAppID,AWindowClass: string; AActivateRunningInstance: Boolean = True; ASendParams: Boolean = False; ASender: THandle = 0; AGlobal: Boolean = False): Boolean;

procedure SendIPCMessage(ASender,AReceiver: THandle; ATyp: TIPCType; AStrData: string; AIntData: Int64=0);
procedure ReadIPCMessage(AMessage: TWMCopyData; var AIPCMessage: TIPCMessage);

implementation

var
  UID: Int64 = 0;
  InstanceHandle: THandle = 0;

procedure ActivateInstanceCheck(const AAppID: string; AGlobal: Boolean = False);
var
  s: string;
begin
  if AGlobal then
    s:='Global\'
  else
    s:='Local\';
  InstanceHandle:=CreateMutex(nil,False,PChar(s+AAppId));
end;


function InstanceExists;
var
  h: HWND;
  i: Integer;
  s: string;
begin
  Result:=False;
  ActivateInstanceCheck(AAppID,AGlobal);
  if ((InstanceHandle>0) and (GetLastError=ERROR_ALREADY_EXISTS)) or
     ((InstanceHandle=0) and (GetLastError=ERROR_ACCESS_DENIED)) then begin
    if AActivateRunningInstance then begin
      h:=FindWindow(PChar(AWindowClass),nil);
      if h>0 then begin
        if IsIconic(h) then
          ShowWindow(h,SW_RESTORE)
        else if not IsWindowVisible(h) then
          ShowWindow(GetWindow(h,GW_OWNER),SW_RESTORE);
        SetForegroundWindow(h);
        if ASendParams and (ParamCount>0) then begin
          s:='';
          for i:=1 to ParamCount do
            s:=s+ParamStr(i)+#13#10;
          SendIPCMessage(ASender,h,ipcParams,Trim(s));
        end;
      end;
    end;
    Result:=True;
  end;
end;

function WideToAnsi(const ws: WideString; codePage: Word = CP_ACP): AnsiString;
var
  l: integer;
  f: Cardinal;
begin
  f:=WC_COMPOSITECHECK or WC_DISCARDNS or WC_SEPCHARS or WC_DEFAULTCHAR;
  if codepage=CP_UTF8 then
    f:=0;
  if ws = '' then
    Result := ''
  else begin
    l := WideCharToMultiByte(codePage,f,@ws[1],-1,nil,0,nil,nil);
    SetLength(Result,l-1);
    if l>1 then
      WideCharToMultiByte(codePage,f,@ws[1],-1,@Result[1],l-1,nil,nil);
  end;
end;

function AnsiToWide(const s: AnsiString; codePage: Word = CP_ACP): WideString;
var
  l: integer;
  f: Cardinal;
begin
  f:=MB_PRECOMPOSED;
  if codepage=CP_UTF8 then
    f:=0;
  if s = '' then
    Result := ''
  else begin
    l:=MultiByteToWideChar(codePage,f,PAnsiChar(@s[1]),-1,nil,0);
    SetLength(Result,l-1);
    if l>1 then
      MultiByteToWideChar(CodePage,f,PAnsiChar(@s[1]),-1,PWideChar(@Result[1]),l-1);
  end;
end;

procedure SendIPCMessage;
var
  CDS: TCopyDataStruct;
  MsgBytes: array of Byte;
  ByteIndex: Integer;
  Typ: AnsiChar;

  procedure AddByte(B: Byte);
  begin
    MsgBytes[ByteIndex]:=B;
    Inc(ByteIndex);
  end;

  procedure AddInt64(AValue: Int64);
  begin
    Move(AValue,MsgBytes[ByteIndex],SizeOf(AValue));
    Inc(ByteIndex,SizeOf(AValue));
  end;

  procedure AddStringBytes(AValue: string);
  var
    BPointer: {$IFDEF UNICODE} PByte {$ELSE} PAnsiChar {$ENDIF};
    i: Integer;
  begin
    BPointer:=Pointer(AValue);
    for i:=0 to ((Length(AValue))*SizeOf(Char))-1 do
      AddByte(Byte(BPointer[i]));
  end;

begin
  ByteIndex:=0;
  SetLength(MsgBytes,SizeOf(UID)+SizeOf(Int64)+2+(Length(AStrData)*SizeOf(Char))+1);
  CDS.cbData:=Length(MsgBytes);
  CDS.dwData:=0;
  Typ:=AnsiChar(Ord(ATyp));
  AddInt64(UID);
  AddByte(Byte(Typ));
  AddInt64(AIntData);
  AddByte(Byte(cStrType));
  AddStringBytes(AStrData);
  AddByte(Byte(cNull));
  CDS.lpData:=Pointer(MsgBytes);
  SendMessage(AReceiver,WM_COPYDATA,WPARAM(ASender),LPARAM(@CDS));
end;

procedure ReadIPCMessage(AMessage: TWMCopyData; var AIPCMessage: TIPCMessage);
var
  CData: TCopyDataStruct;
  MessageContent: PAnsiChar;
  i: Integer;
  CharWord: Word;
  UChar: WideChar;
  ct: Byte;
  sa: AnsiString;
  sw: WideString;
begin
  CData:=AMessage.CopyDataStruct^;
  MessageContent:=CData.lpData;
  i:=0;
  Move(MessageContent[i],AIPCMessage.UID,SizeOf(AIPCMessage.UID));
  Inc(i,SizeOf(AIPCMessage.UID));
  AIPCMessage.Typ:=TIPCType(Ord(MessageContent[i]));
  Inc(i);
  Move(MessageContent[i],AIPCMessage.IntData,SizeOf(AIPCMessage.IntData));
  Inc(i,SizeOf(AIPCMessage.IntData));
  ct:=Ord(MessageContent[i]);
  Inc(i);
  sa:='';
  sw:='';
  while MessageContent[i] <> #0 do begin
    if ct=2 then begin
      WordRec(CharWord).Lo:=Byte(MessageContent[i]);
      WordRec(CharWord).Hi:=Byte(MessageContent[i+1]);
      UChar:=WideChar(CharWord);
      sw:=sw+UChar;
      Inc(i,2);
    end else begin
      sa:=sa+MessageContent[i];
      Inc(i);
    end;
  end;
  if ct=1 then
    AIPCMessage.StrData:={$IFDEF UNICODE}AnsiToWide{$ENDIF}(sa)
  else if ct=2 then
    AIPCMessage.StrData:={$IFNDEF UNICODE}WideToAnsi{$ENDIF}(sw);
end;

procedure DeactivateInstanceCheck;
begin
  CloseHandle(Instancehandle);
end;

initialization
  UID:=GetCurrentProcessId;
end.
