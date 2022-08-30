{*******************************************************}
{               MiTeC Common Routines                   }
{          Network Basic Input/Output System            }
{                                                       }
{                                                       }
{        Copyright (c) 1997-2016 Michal Mutl            }
{                                                       }
{*******************************************************}


{$INCLUDE Compilers.inc}

unit MiTeC_NetBIOS;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils, System.Classes;
     {$ELSE}
     Windows, SysUtils, Classes;
     {$ENDIF}

const
  NBNAMESIZE    = 16;    { absolute length of a net name }
  MAX_LANA      = 254;   { lana's in range 0 to MAX_LANA inclusive }
  NCB_ASYNC     = $80;   { asynch command bit to be or-ed into command }
  NCB_CALL      = $10;   { open a session }
  NCB_LISTEN    = $11;   { wait for a call }
  NCB_HANGUP    = $12;   { end session }
  NCB_SEND      = $14;   { send data }
  NCB_RECV      = $15;   { receive data }
  NCB_RECVANY   = $16;   { receive data on any session }
  NCB_CHAINSEND = $17;   { chain send data }
  NCB_DGSEND    = $20;   { send a datagram }
  NCB_DGRECV    = $21;   { receive datagram }
  NCB_DGSENDBC  = $22;   { send broadcast datagram }
  NCB_DGREVCBC  = $23;   { receive broadcast datagram }
  NCB_ADDNAME   = $30;   { add unique name to local table }
  NCB_DELNAME   = $31;   { delete name from local table }
  NCB_RESET     = $32;   { reset adapter }
  NCB_ADPSTAT   = $33;   { adapter status }
  NCB_SSTAT     = $34;   { session status }
  NCB_CANCEL    = $35;   { cancel NCB request }
  NCB_ADDGRPNAME= $36;   { add group name to local table }
  NCB_ENUM      = $37;   { enum adapters }
  NCB_UNLINK    = $70;   { unlink remote boot code }
  NCB_SENDNA    = $71;   { send, don't wait for ACK }
  NCB_CHAINSENDNA=$72;   { chain send, but don't wait for ACK }
  NCB_LANSTALERT= $73;   { lan status alert }
  NCB_ACTION    = $77;   { enable extensions }
  NCB_FINDNAME  = $78;   { search for name on the network }
  NCB_TRACE     = $79;   { activate / stop tracing }
  NRC_GOODRET     = $00;    { good return   }
  NRC_BUFLEN      = $01;    { illegal buffer length                      }
  NRC_ILLCMD      = $03;    { illegal command                            }
  NRC_CMDTMO      = $05;    { command timed out                          }
  NRC_INCOMP      = $06;    { message incomplete, issue another command  }
  NRC_BADDR       = $07;    { illegal buffer address                     }
  NRC_SNUMOUT     = $08;    { session number out of range                }
  NRC_NORES       = $09;    { no resource available                      }
  NRC_SCLOSED     = $0a;    { session closed                             }
  NRC_CMDCAN      = $0b;    { command cancelled                          }
  NRC_DUPNAME     = $0d;    { duplicate name                             }
  NRC_NAMTFUL     = $0e;    { name table full                            }
  NRC_ACTSES      = $0f;    { no deletions, name has active sessions     }
  NRC_LOCTFUL     = $11;    { local session table full                   }
  NRC_REMTFUL     = $12;    { remote session table full                  }
  NRC_ILLNN       = $13;    { illegal name number                        }
  NRC_NOCALL      = $14;    { no callname                                }
  NRC_NOWILD      = $15;    { cannot put * in NCB_NAME                   }
  NRC_INUSE       = $16;    { name in use on remote adapter              }
  NRC_NAMERR      = $17;    { name deleted                               }
  NRC_SABORT      = $18;    { session ended abnormally                   }
  NRC_NAMCONF     = $19;    { name conflict detected                     }
  NRC_IFBUSY      = $21;    { interface busy, IRET before retrying       }
  NRC_TOOMANY     = $22;    { too many commands outstanding, retry later }
  NRC_BRIDGE      = $23;    { ncb_lana_num field invalid                 }
  NRC_CANOCCR     = $24;    { command completed while cancel occurring   }
  NRC_CANCEL      = $26;    { command not valid to cancel                }
  NRC_DUPENV      = $30;    { name defined by anther local process       }
  NRC_ENVNOTDEF   = $34;    { environment undefined. RESET required      }
  NRC_OSRESNOTAV  = $35;    { required OS resources exhausted            }
  NRC_MAXAPPS     = $36;    { max number of applications exceeded        }
  NRC_NOSAPS      = $37;    { no saps available for netbios              }
  NRC_NORESOURCES = $38;    { requested resources are not available      }
  NRC_INVADDRESS  = $39;    { invalid ncb address or length > segment    }
  NRC_INVDDID     = $3B;    { invalid NCB DDID                           }
  NRC_LOCKFAIL    = $3C;    { lock of user area failed                   }
  NRC_OPENERR     = $3f;    { NETBIOS not loaded                         }
  NRC_SYSTEM      = $40;    { system error                               }
  NRC_PENDING     = $ff;    { asynchronous command is not yet finished   }

  ALL_TRANSPORTS = 'M'#$00#$00#$00;
  MS_NBF         = 'MNBF';

  NAME_FLAGS_MASK = $87;

  GROUP_NAME      = $80;
  UNIQUE_NAME     = $00;
  DUPLICATE       = $06;

type
{ Netbios Name }
  TNBName = array[0..(NBNAMESIZE - 1)] of byte;

 { MAC address }
  TMacAddress = array[0..5] of byte;

  PNCB = ^TNCB;

 { Netbios Control Block }

  TNcbPost = procedure (P: PNCB); stdcall;

  TNCB = record        { Netbios Control Block }
    ncb_command: Byte;  // command code
    ncb_retcode: Byte;  // return code
    ncb_lsn: Byte;      // local session number
    ncb_num: Byte;      // number of our network name
    ncb_buffer: PByte;  // address of message buffer
    ncb_length: Word;    // size of message buffer
    ncb_callname: array [0..NBNAMESIZE - 1] of Byte; // blank-padded name of remote
    ncb_name: array [0..NBNAMESIZE - 1] of Byte;     // our blank-padded netname
    ncb_rto: Byte;      // rcv timeout/retry count
    ncb_sto: Byte;      // send timeout/sys timeout
    ncb_post: TNcbPost;  // POST routine address
    ncb_lana_num: Byte; // lana (adapter) number
    ncb_cmd_cplt: Byte; // 0xff => commmand pending
    {$IFDEF WIN64}
    ncb_reserve: array [0..17] of Byte; // reserved, used by BIOS
    {$ELSE}
    ncb_reserve: array [0..9] of Byte;  // reserved, used by BIOS
    {$ENDIF}
    ncb_event: THandle;   // HANDLE to Win32 event which
                         // will be set to the signalled
                         // state when an ASYNCH command
                         // completes
  end;


{ Netbios Name Info record }
  PNameInfo = ^TNameInfo;
  TNameInfo = record  { name info record }
    Name:   TNBName;       { netbios name }
    NameNum: byte;          { name number  }
    NameSt: byte;          { name status  }
  end;

{ Netbios adapter status }
  PAdapterStatus = ^TAdapterStatus;
  TAdapterStatus = record    { adapter status record}
    adapter_address: TMACAddress;
    rev_major: Byte;
    reserved0: Byte;
    adapter_type: Byte;
    rev_minor: Byte;
    duration: WORD;
    frmr_recv: WORD;
    frmr_xmit: WORD;
    iframe_recv_err: WORD;
    xmit_aborts: WORD;
    xmit_success: DWORD;
    recv_success: DWORD;
    iframe_xmit_err: WORD;
    recv_buff_unavail: WORD;
    t1_timeouts: WORD;
    ti_timeouts: WORD;
    reserved1: DWORD;
    free_ncbs: WORD;
    max_cfg_ncbs: WORD;
    max_ncbs: WORD;
    xmit_buf_unavail: WORD;
    max_dgram_size: WORD;
    pending_sess: WORD;
    max_cfg_sess: WORD;
    max_sess: WORD;
    max_sess_pkt_size: WORD;
    name_count: WORD;
  end;

  TAdapterFullStatus = packed record
    Adapter: TAdapterStatus;
    Names: array [0..15] of TNameInfo;
  end;

{
   Structure returned to the NCB command NCBSSTAT is SESSION_HEADER followed
   by an array of SESSION_BUFFER structures. If the NCB_NAME starts with an
   asterisk then an array of these structures is returned containing the
   status for all names.
}

{ session header }
  PSession_Header = ^TSession_Header;
  TSession_Header = record
    sess_name: Byte;
    num_sess: Byte;
    rcv_dg_outstanding: Byte;
    rcv_any_outstanding: Byte;
  end;

{ session buffer }
  PSession_Buffer = ^TSession_Buffer;
  TSession_Buffer = packed record
    lsn: Byte;
    state: Byte;
    local_name: TNBName;
    remote_name: TNBName;
    rcvs_outstanding: Byte;
    sends_outstanding: Byte;
  end;

{
   Structure returned to the NCB command NCBENUM.

   On a system containing lana's 0, 2 and 3, a structure with
   length =3, lana[0]=0, lana[1]=2 and lana[2]=3 will be returned.
}

  _LANA_ENUM = record
    length: Byte; // Number of valid entries in lana[]
    lana: array [0..MAX_LANA] of Byte;
  end;
  LANA_ENUM = _LANA_ENUM;
  PLANA_ENUM = ^LANA_ENUM;
  TLanaEnum = LANA_ENUM;
  PLanaEnum = PLANA_ENUM;

{
   Structure returned to the NCB command NCBFINDNAME is FIND_NAME_HEADER followed
   by an array of FIND_NAME_BUFFER structures.
 }

  PFind_Name_Header = ^TFind_Name_Header;
  TFind_Name_Header = record
    node_count: WORD;
    reserved: Byte;
    unique_group: Byte;
  end;

  PFind_Name_Buffer = ^TFind_Name_Buffer;
  TFind_Name_Buffer = record
    length: UCHAR;
    access_control: UCHAR;
    frame_control: UCHAR;
    destination_addr: TMACAddress;
    source_addr: TMACAddress;
    routing_info: array [0..17] of UCHAR;
  end;

{
   Structure provided with NCBACTION. The purpose of NCBACTION is to provide
   transport specific extensions to netbios.
 }

  PAction_Header = ^TAction_Header;
  TAction_Header = record
    transport_id: Cardinal;
    action_code: Word;
    reserved: word;
  end;

function NetbiosCmd(var NCB: TNCB): byte;

function NB_Reset(Lana_Num: Byte): Word;
function NB_LANAEnum: TLanaEnum;
function NB_GetAdapterStatus(const Machine: string): TAdapterFullStatus;
function NB_GetMacAddresses(const Machine: string; Addresses: TStrings): Integer;

function AdapterToString(MAC: TMACAddress): string;
function GetStrFromBuf(var Buffer: array of Byte; Len: Byte): string;

implementation

uses {$IFDEF UNICODE}MiTeC_StrUtils,{$ENDIF}
     {$IFDEF RAD9PLUS}
     System.SyncObjs;
     {$ELSE}
     SyncObjs;
     {$ENDIF}

var
  NBLock: TCriticalSection;

procedure ResetMemory(out P; Size: Longint);
begin
  if Size>0 then begin
    Byte(P):=0;
    FillChar(P,Size,0);
  end;
end;

function AdapterToString(MAC: TMACAddress): string;
begin
  Result:=Format('%2.2x-%2.2x-%2.2x-%2.2x-%2.2x-%2.2x',[
      Integer(MAC[0]), Integer(MAC[1]),
      Integer(MAC[2]), Integer(MAC[3]),
      Integer(MAC[4]), Integer(MAC[5])]);
end;

function GetStrFromBuf(var Buffer: array of Byte; Len: Byte): string;
var
  i,j :integer;
begin
  result:='';
  j:=0;
  i:=0;
  repeat
    if (buffer[i]<>0) and (i<Len) then begin
      result:=result+Chr(buffer[i]);
      j:=0;
    end else
      inc(j);
    inc(i);
  until j>1;
end;

function NetBIOS(P: PNCB): Byte; stdcall; external 'netapi32.dll' name 'Netbios';

function NetBIOSCmd(var NCB: TNCB): Byte;
begin
  try
    Result:=Netbios(PNCB(@NCB));
  except
    Result:=0;
  end;
end;

function NB_LANAEnum: TLanaEnum;
var
  NCB: TNCB;
  L_Enum: TLanaEnum;
  RetCode: byte;
begin
  ResetMemory(NCB,SizeOf(NCB));
  resetmemory(L_Enum,SizeOf(TLanaEnum));
  NCB.ncb_command:=NCB_ENUM;
  NCB.ncb_buffer:=PByte(@L_Enum);
  NCB.ncb_length:=Sizeof(L_Enum);
  RetCode:=NetBiosCmd(NCB);
  if RetCode<>NRC_GOODRET then begin
    L_Enum.Length:=0;
    L_Enum.Lana[0]:=Byte(RetCode);
  end;
  Result:=L_Enum;
end;

function NB_Reset;
var
  NCB: TNCB;
begin
  ResetMemory(NCB,SizeOf(NCB));
  NCB.ncb_command:=NCB_RESET;
  NCB.ncb_lana_num:=Lana_Num;
  Result:=NetBiosCmd(NCB);
end;

function NB_GetAdapterStatus(const Machine: string): TAdapterFullStatus;
var
  NCB: TNCB;
  LANA_Enum: TLanaEnum;
  Adapter: TAdapterFullStatus;
  MachineName: ansistring;
  LN,r,n: Byte;
  s: string;
begin
  if Assigned(NBLock) then
    NBLock.Enter;
  try
    ResetMemory(Result,SizeOf(Result));
    s:=UpperCase(Machine);
    if s='' then
      s:='*';
    s:=Copy(s,1,NBNAMESIZE)+StringOfChar(' ',NBNAMESIZE-Length(s));
    MachineName:={$IFDEF UNICODE}WideToAnsi{$ENDIF}(s);
    MachineName[NBNAMESIZE]:=#0;
    LANA_Enum:=NB_LANAEnum;
    for n:=0 to LANA_Enum.Length-1 do begin
      LN:=LANA_Enum.LANA[n];
      if NB_Reset(LN)=NRC_GOODRET then begin
        Resetmemory(NCB,SizeOf(TNCB));
        NCB.ncb_command:=NCB_ADPSTAT;
        NCB.ncb_lana_num:=LN;
        //Move(MachineName[1],NCB.ncb_callname,SizeOf(NCB.ncb_callname));
        NCB.ncb_buffer:=PByte(@Adapter);
        NCB.ncb_length:=SizeOf(Adapter);
        r:=NetBiosCmd(NCB);
        if r=NRC_GOODRET then begin
          Result:=Adapter;
          Break;
        end;
      end;
    end;
  finally
    if Assigned(NBLock) then
      NBLock.Leave;
  end;
end;

function NB_GetMacAddresses(const Machine: string; Addresses: TStrings): Integer;
var
  NCB: TNCB;
  Enum: TLanaEnum;
  ln,i: byte;
  AdapterStat: TAdapterFullStatus;
  MachineName: ansistring;
  r: Word;
  s: string;
begin
  if Assigned(NBLock) then
    NBLock.Enter;
  try
    Addresses.Clear;
    s:=UpperCase(Machine);
    if s='' then
      s:='*';
    s:=Copy(s,1,NBNAMESIZE)+StringOfChar(' ',NBNAMESIZE-Length(s));
    MachineName:={$IFDEF UNICODE}WideToAnsi{$ENDIF}(s);
    MachineName[NBNAMESIZE]:=#0;
    Enum:=NB_LANAEnum;
    ResetMemory(NCB,SizeOf(NCB));
    Result:=Enum.Length;
    for i:=0 to Enum.length-1 do begin
      ln:=Enum.lana[i];
      if NB_Reset(ln)=NRC_GOODRET then begin
        FillChar(NCB,SizeOf(TNCB),#0);
        NCB.ncb_command:=NCB_ADPSTAT;
        NCB.ncb_lana_num:=ln;
        Move(MachineName[1],NCB.ncb_callname,SizeOf(NCB.ncb_callname));
        NCB.ncb_buffer:=PByte(@AdapterStat);
        NCB.ncb_length:=SizeOf(AdapterStat);
        r:=NetBiosCmd(NCB);
        if (r=NRC_GOODRET) and (AdapterStat.Adapter.rev_major>0) then
          Addresses.Add(AdapterToString(AdapterStat.Adapter.adapter_address));
      end;
    end;
  finally
    if Assigned(NBLock) then
      NBLock.Leave;
  end;
end;

initialization
  NBLock:=nil;//TCriticalSection.Create;
finalization
  if Assigned(NBLock) then
    NBLock.Free;
end.
