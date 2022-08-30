{
  @html(<b>)
  RTC Gateway Constants
  @html(</b>)
  - Copyright 2004-2014 (c) RealThinClient.com (http://www.realthinclient.com)
  @html(<br><br>)

  Global parameters required by the Gateway and Gate Client components.
}
unit rtcGateConst;

interface

{$include rtcDefs.inc}

uses
  rtcTypes,
  SysUtils,
  Classes,

  rtcConn,
  rtcSockBase,
  rtcThrPool;

type
  TGateUID = LongWord;

  ERtcOutOfBounds = class(Exception);

const
  // GetAppRunTime Timer "ticks per second"
  RUN_TIMER_PRECISION = 20;

var
  LOG_GATEWAY_TIMEOUTS : boolean = {$IFDEF RTC_DEBUG} True {$ELSE} False {$ENDIF};
  LOG_GATEWAY_ERRORS : boolean = {$IFDEF RTC_DEBUG} True {$ELSE} False {$ENDIF};
  LOG_GATEWAY_STATUS : boolean = {$IFDEF RTC_DEBUG} True {$ELSE} False {$ENDIF};

  LOG_GATECLIENT_TIMEOUTS : boolean = {$IFDEF RTC_DEBUG} True {$ELSE} False {$ENDIF};

  // Ping interval in milliseconds
  PING_INTERVAL_inMS:cardinal = 1000;
  // Send PING interval in AppRunTime ticks
  SENDPING_INTERVAL:cardinal = 8 * RUN_TIMER_PRECISION;

  // You can split packets above this size ( 8 KB = 0,1% overhead )
  MIN_SPLITPACKET_SIZE:longword = 8*1024;
  // Maximum size for outgoing TCP/IP packets
  MAX_OUTPACKET_SIZE:longword = 16*1024;
  // Maximum size for incoming TCP/IP packets
  MAX_INPACKET_SIZE:longword = 32*1024;

  // Default size for the Output Stream, if not explicitly set by Client (2 GB)
  DEF_OUTSTREAM_SIZE:int64 = int64(2000)*1000*1000;

  GATECHECKLOGIN_TIMEOUT:cardinal = 60 * RUN_TIMER_PRECISION;
  GATECHECKIN_TIMEOUT:cardinal    = 60 * RUN_TIMER_PRECISION;
  GATECHECKOUT_TIMEOUT:cardinal   = 80 * RUN_TIMER_PRECISION;
  GATECHECKMAXIN_TIMEOUT:cardinal  = 90 * RUN_TIMER_PRECISION;
  GATECHECKMAXOUT_TIMEOUT:cardinal = 120 * RUN_TIMER_PRECISION;

  GATECHECKRECV_TIMEOUT:cardinal   = 240 * RUN_TIMER_PRECISION;
  GATECHECKSEND_TIMEOUT:cardinal   = 240 * RUN_TIMER_PRECISION;
  GATECHECKDONE_TIMEOUT:cardinal   = 900 * RUN_TIMER_PRECISION;

  CLIENTCHECKLOGIN_TIMEOUT:cardinal = 30 * RUN_TIMER_PRECISION;
  CLIENTCHECKIN_TIMEOUT:cardinal    = 60 * RUN_TIMER_PRECISION;
  CLIENTCHECKOUT_TIMEOUT:cardinal   = 80 * RUN_TIMER_PRECISION;
  CLIENTCHECKMAXIN_TIMEOUT:cardinal = 90 * RUN_TIMER_PRECISION;
  CLIENTCHECKMAXOUT_TIMEOUT:cardinal = 120 * RUN_TIMER_PRECISION;

  CLIENTCHECKRECV_TIMEOUT:cardinal   = 240 * RUN_TIMER_PRECISION;
  CLIENTCHECKSEND_TIMEOUT:cardinal   = 240 * RUN_TIMER_PRECISION;
  CLIENTCHECKLOGOUT_TIMEOUT:cardinal = 600 * RUN_TIMER_PRECISION;

const
  GATE_LOG:String='GATE';

  GATE_OK_CODE:integer=200;

  GATE_ERROR_CODE:integer=409;
  GATE_ERROR_TEXT:String='Conflict';

  GATE_FATALERROR_CODE:integer=412;
  GATE_FATALERROR_TEXT:String='Precondition Failed';

  GATEURI_STATUS:String='check';
  GATEURI_PING:String='ping';
  GATEURI_LOGIN:String='login';
  GATEURI_INPUT:String='datain';
  GATEURI_OUTPUT:String='dataout';
  GATEURI_INPUTRESET:String='resin';
  GATEURI_OUTPUTRESET:String='resout';

const
  MinLowID=TGateUID(1);
  MinHigID=TGateUID(111111);
  MaxHigID=TGateUID(999999);

  MinUserID = MinLowID;
  MaxUserID = MaxHigID;

  CntLowIDs=MinHigID-MinLowID;
  CntHigIDs=MaxHigID-MinHigID;

  UserIDShift=12;
  UserIDMask =longword($FFFFF) shl UserIDShift;

  GroupIDBits=8;
  GroupNrMask=(1 shl GroupIDBits) - 1;
  CntMaxGroups=GroupNrMask;
  // GroupID = UserID + ($01 .. $FF)
  GroupIDMask=GroupNrMask or UserIDMask;

  // Command = UserID/GroupID + ($0 .. $F)
  CommandMask=word($F) shl GroupIDBits;

  // Client -> Gateway
  Cmd_SendData            = 0;
  Cmd_AddUserToGroup      = word(1) shl GroupIDBits;
  Cmd_RemoveUserFromGroup = word(2) shl GroupIDBits;
  Cmd_RemoveGroup         = word(3) shl GroupIDBits;

  // Gateway -> Client
  Cmd_SendAll   = 0;
  Cmd_SendFirst = word(1) shl GroupIDBits;
  Cmd_SendMore  = word(2) shl GroupIDBits;
  Cmd_SendLast  = word(3) shl GroupIDBits;

  // Client <=> Gateway
  Cmd_UserIn    = word(4) shl GroupIDBits;
  Cmd_UserOut   = word(5) shl GroupIDBits;
  Cmd_UserOn    = word(6) shl GroupIDBits;
  Cmd_UserOff   = word(7) shl GroupIDBits;

  // Commands $8 .. $F are left unused for possible future updates
  Cmd_Mask=word($F) shl GroupIDBits;

function Int2Bytes(UID:LongWord):RtcByteArray;
function Bytes2Int(const BAR:RtcByteArray; loc:integer=0):LongWord;

function Word2Bytes(UID:Word):RtcByteArray;
function Bytes2Word(const BAR:RtcByteArray; loc:integer=0):Word;

function Int2BytesCRC(UID:LongWord):RtcByteArray;
function Bytes2IntCRC(const BAR:RtcByteArray; loc:integer=0):LongWord;

function OneByte2Bytes(UID:Byte):RtcByteArray;
function Bytes2OneByte(const BAR:RtcByteArray; loc:integer=0):Byte;

function GetAppRunTime:Cardinal;

// Set min. required connection Speed in KBits
procedure SetupConnectionSpeed(ReceiveSpeed, SendSpeed:integer);

{ Set connection timeouts based on global GATE TIMEOUT values.
  Used internally by TRtcGateway and TRtcHttpGateClient components. }
procedure SetupConnectionTimeouts(Sender:TRtcConnection);

implementation

var
  AppStartTime:TDateTime;

procedure SetupConnectionSpeed(ReceiveSpeed, SendSpeed:integer);
  begin
{$IFDEF WINDOWS}
  RTC_THREAD_PRIORITY:=tpLower;
{$ENDIF}

  MAX_OUTPACKET_SIZE:=trunc(SendSpeed/8 * 1024);
  MAX_INPACKET_SIZE:=trunc(ReceiveSpeed/8 * 1024);

  MIN_SPLITPACKET_SIZE:=trunc(ReceiveSpeed/10 * 1024);
  if MIN_SPLITPACKET_SIZE>8*1024 then
    MIN_SPLITPACKET_SIZE:=8*1024;

  SOCK_SEND_BUFFER_SIZE:=MAX_OUTPACKET_SIZE;
  SOCK_READ_BUFFER_SIZE:=MAX_INPACKET_SIZE;

  SOCK_MAX_SEND_SIZE:=MAX_OUTPACKET_SIZE;
  SOCK_MAX_READ_SIZE:=MAX_INPACKET_SIZE;
  end;

procedure SetupConnectionTimeouts(Sender:TRtcConnection);
  begin
  if Sender is TRtcServer then
    begin
    Sender.TimeoutsOfAPI.ReceiveTimeout:=GATECHECKRECV_TIMEOUT div RUN_TIMER_PRECISION;
    Sender.TimeoutsOfAPI.SendTimeout:=GATECHECKSEND_TIMEOUT div RUN_TIMER_PRECISION;
    end
  else
    begin
    Sender.TimeoutsOfAPI.ResolveTimeout :=CLIENTCHECKLOGIN_TIMEOUT div RUN_TIMER_PRECISION;
    Sender.TimeoutsOfAPI.ConnectTimeout :=CLIENTCHECKLOGIN_TIMEOUT div RUN_TIMER_PRECISION;

    Sender.TimeoutsOfAPI.ResponseTimeout:=CLIENTCHECKRECV_TIMEOUT div RUN_TIMER_PRECISION;
    Sender.TimeoutsOfAPI.ReceiveTimeout :=CLIENTCHECKRECV_TIMEOUT div RUN_TIMER_PRECISION;

    Sender.TimeoutsOfAPI.SendTimeout:=CLIENTCHECKSEND_TIMEOUT div RUN_TIMER_PRECISION;
    end;
  end;

function GetAppRunTime:Cardinal;
  begin
  Result:=Cardinal(trunc((Now-AppStartTime)*24*60*60*RUN_TIMER_PRECISION));
  end;

function Int2Bytes(UID:LongWord):RtcByteArray;
  begin
  SetLength(Result,4);
  Result[0]:=UID and $FF;
  Result[1]:=(UID shr 8) and $FF;
  Result[2]:=(UID shr 16) and $FF;
  Result[3]:=(UID shr 24) and $FF;
  end;

function Bytes2Int(const BAR:RtcByteArray; loc:integer=0):LongWord;
  begin
  if (loc<0) or (loc+3>=length(BAR)) then
    raise ERtcOutOfBounds.Create('Bytes2Int: LOC ('+IntToStr(loc)+') out of ByteArray bounds ('+IntToStr(length(BAR))+')');
  Result:=BAR[loc] or
         (BAR[loc+1] shl 8) or
         (BAR[loc+2] shl 16) or
         (BAR[loc+3] shl 24);
  end;

function Int2BytesCRC(UID:LongWord):RtcByteArray;
  var
    a,b,c,crc:byte;
  begin
  if UID>$FFFFFF then
    raise ERtcOutOfBounds.Create('Int2BytesCRC: Value ('+IntToStr(UID)+') exceeds allowed range ($FFFFFF).');
  SetLength(Result,4);
  a:=UID and $FF;
  b:=(UID shr 8) and $FF;
  c:=(UID shr 16) and $FF;
  crc:=255 xor (((a+b+c) and $1F) + ((a+b+c) shr 5));
  Result[0]:=a;
  Result[1]:=b;
  Result[2]:=c;
  Result[3]:=crc;
  end;

function Bytes2IntCRC(const BAR:RtcByteArray; loc:integer=0):LongWord;
  var
    a,b,c,crc:byte;
  begin
  if (loc<0) or (loc+3>=length(BAR)) then
    raise ERtcOutOfBounds.Create('Bytes2IntCRC: LOC ('+IntToStr(loc)+') out of ByteArray bounds ('+IntToStr(length(BAR))+')');
  a:=BAR[loc];
  b:=BAR[loc+1];
  c:=BAR[loc+2];
  crc:=255 xor (((a+b+c) and $1F) + ((a+b+c) shr 5));
  if BAR[loc+3]<>crc then
    raise ERtcOutOfBounds.Create('Bytes2IntCRC: CRC Error at LOC ('+IntToStr(loc)+') in ByteArray ('+IntToStr(length(BAR))+')');

  Result:=a or (b shl 8) or (c shl 16);
  end;

function Word2Bytes(UID:Word):RtcByteArray;
  begin
  SetLength(Result,2);
  Result[0]:=UID and $FF;
  Result[1]:=(UID shr 8) and $FF;
  end;

function Bytes2Word(const BAR:RtcByteArray; loc:integer=0):Word;
  begin
  if (loc<0) or (loc+1>=length(BAR)) then
    raise ERtcOutOfBounds.Create('Bytes2Word: LOC ('+IntToStr(loc)+') out of ByteArray bounds ('+IntToStr(length(BAR))+')');
  Result:=BAR[loc] or
         (BAR[loc+1] shl 8);
  end;

function OneByte2Bytes(UID:Byte):RtcByteArray;
  begin
  SetLength(Result,1);
  Result[0]:=UID;
  end;

function Bytes2OneByte(const BAR:RtcByteArray; loc:integer=0):Byte;
  begin
  if (loc<0) or (loc>=length(BAR)) then
    raise ERtcOutOfBounds.Create('Bytes2Byte: LOC ('+IntToStr(loc)+') out of ByteArray bounds ('+IntToStr(length(BAR))+')');
  Result:=BAR[loc];
  end;

initialization
AppStartTime:=Now;
end.
