unit Share;

interface

uses
  Windows, Classes, SysUtils, MudUtil, RSA, Grobal2;

const
  DLLVersion                = 1;

type
  TCheckStatus = (c_Idle, c_Connect, c_Checking, c_CheckError, c_CheckFail, c_CheckOK);
  TCheckStep = (c_None, c_SendClinetKey, c_CheckOver);
  TSessionStatus = (s_NoUse, s_Used, s_GetLic, s_SendLic, s_Finished);

  TXORItem = packed record
    SoftType: ShortInt;                 //1    1=M2Server 2=RunGate 3=DBServer
    MainVersion: Single;                //4    ������汾��
    DLLVersion: Single;                 //4    DLL�汾��
    IniRegIP: Integer;                  //4    ��INI����д��Reg IP
    Param1: Integer;                    //4    �����Ժ���
    param2: string[15];                 //4    �����Ժ���
  end;

  TXORRecord = packed record
    XORKey: Integer;                    //��Ҫ�ֶΣ������ж��Ƿ������ǵ���֤������
    XORItemLen: Integer;
    XORItem: string[33];                //���Ⱥ� TXORItemһ����
  end;

  TRSARecord = packed record
    RSAKey: Int64;
    RSAItemLen: Integer;
    RSAItem: TInt64Item;                //����TXORRecord RSA���ܺ�Ľ��
  end;
  pRSARecord = ^TRSARecord;


  TDLLConfig = packed record
    sWellCome: string;
    sTitleName: string;
    sProductName: string;
    sVersion: string;
    sUpDateTime: string;
    sProgram: string;
    sWebSite: string;
    sBbsSite: string;
    s_LoadPlugOk: string;
    s_PluginName: string;
    g_dwM2Crc: string;
    dwLocalTick: LongWord;
    dwServerTick: LongWord;
    nRemainDays: Integer;               //ʣ������
    nUserCount: Integer;                //�û���
    fVersion: Single;                   //������汾��
    sRegIniIP: string;                  //Ini Reg IP
    boExpired: Boolean;
  end;
  pTDLLConfig = ^TDLLConfig;

function MakeIPToStr(IPAddr: TIPAddr): string;
function MakeIPToInt(sIPaddr: string): Integer;

var
  g_Config                  : pTConfig;
  g_DConfig                 : pTDLLConfig;

  ENDYEARMIN                : PInteger;
  ENDMONTHMIN               : PInteger;
  ENDDAYMIN                 : PInteger;

  ENDYEAR                   : PInteger;
  ENDMONTH                  : PInteger;
  ENDDAY                    : PInteger;

  nLocalXORKey              : Integer;
  nRemoteXORKey             : Integer;
  ChangeLabelVerColor       : procedure(nColor: Integer); stdcall;
  ChangeCaptionText         : procedure(Msg: PChar; nLen: Integer); stdcall;
  PlugRunOver               : procedure(nUserCount, nDay: Integer);
  SetRemoteXORKey           : procedure(nRemoXORKey: Integer; XORStr: PChar); stdcall;
  //GetVersion                : function(): Single; stdcall;
  //GetXORKey                 : function(): Integer; stdcall;

implementation

uses HUtil32;

function MakeIPToStr(IPAddr: TIPAddr): string;
begin
  Result := IntToStr(IPAddr.A) + '.' + IntToStr(IPAddr.B) + '.' + IntToStr(IPAddr.C) + '.' + IntToStr(IPAddr.D);
end;

function MakeIPToInt(sIPaddr: string): Integer;
var
  sA, sB, SC, sD            : string;
  A, B, C, D                : Byte;
begin
  Result := -1;
  sIPaddr := Trim(GetValidStr3(sIPaddr, sA, ['.']));
  sIPaddr := Trim(GetValidStr3(sIPaddr, sB, ['.']));
  sD := Trim(GetValidStr3(sIPaddr, SC, ['.']));
  if (sA <> '') and (sB <> '') and (SC <> '') and (sD <> '') then begin
    A := Str_ToInt(sA, 0);
    B := Str_ToInt(sB, 0);
    C := Str_ToInt(SC, 0);
    D := Str_ToInt(sD, 0);
    Result := MakeLong(MakeWord(A, B), MakeWord(C, D));
  end;
end;

initialization

  New(ENDYEARMIN);
  New(ENDMONTHMIN);
  New(ENDDAYMIN);

  New(ENDYEAR);
  New(ENDMONTH);
  New(ENDDAY);

  ENDYEARMIN^ := 2010;
  ENDMONTHMIN^ := 05;
  ENDDAYMIN^ := 15;

  ENDYEAR^ := 2228;
  ENDMONTH^ := 12;
  ENDDAY^ := 15;

  New(g_DConfig);
  with g_DConfig^ do begin
    sUpDateTime  := ('��������:2019/02/03');
    s_LoadPlugOk := ('������Ϸ�����ʼ��ģ��ɹ�...');
    s_PluginName := ('��Ϸ�����ʼ��ģ��');
    boExpired := false;
  end;


finalization

  Dispose(ENDYEARMIN);
  Dispose(ENDMONTHMIN);
  Dispose(ENDDAYMIN);

  Dispose(ENDYEAR);
  Dispose(ENDMONTH);
  Dispose(ENDDAY);

  Dispose(g_DConfig);


end.

