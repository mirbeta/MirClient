{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     �й����Լ��Ŀ���Դ�������������                         }
{                   (C)Copyright 2001-2010 CnPack ������                       }
{                   ------------------------------------                       }
{                                                                              }
{            ���������ǿ�Դ��������������������� CnPack �ķ���Э������        }
{        �ĺ����·�����һ����                                                }
{                                                                              }
{            ������һ��������Ŀ����ϣ�������ã���û���κε���������û��        }
{        �ʺ��ض�Ŀ�Ķ������ĵ���������ϸ���������� CnPack ����Э�顣        }
{                                                                              }
{            ��Ӧ���Ѿ��Ϳ�����һ���յ�һ�� CnPack ����Э��ĸ��������        }
{        ��û�У��ɷ������ǵ���վ��                                            }
{                                                                              }
{            ��վ��ַ��http://www.cnpack.org                                   }
{            �����ʼ���master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

{******************************************************************************}
{        �õ�Ԫ�������ݻ���Dennis D. Spreen��UTBASE64.pas��д��                }
{        ������UTBASE64.pas������:                                             }
{ -----------------------------------------------------------------------------}
{ uTBase64 v1.0 - Simple Base64 encoding/decoding class                        }
{ Base64 described in RFC2045, Page 24, (w) 1996 Freed & Borenstein            }
{ Delphi implementation (w) 1999 Dennis D. Spreen (dennis@spreendigital.de)    }
{ This unit is freeware. Just drop me a line if this unit is useful for you.   }
{ -----------------------------------------------------------------------------}

unit CnBase64;
{* |<PRE>
================================================================================
* ������ƣ�������������
* ��Ԫ���ƣ�Base64�����㷨��Ԫ
* ��Ԫ���ߣ�ղ����Solin�� solin@21cn.com; http://www.ilovezhuzhu.net
*           wr960204
* ��    ע���õ�Ԫ�������汾��Base64ʵ�֣��ֱ�����ֲ�Ľ�������
* ����ƽ̨��PWin2003Std + Delphi 6.0
* ���ݲ��ԣ���δ����
* �� �� �����õ�Ԫ���豾�ػ�����
* �޸ļ�¼��2006.10.25 V1.1
*               ���� wr960204 ���Ż��汾
*           2003.10.14 V1.0
*               ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes;

function Base64Encode(InputData: TStream; var OutputData: string): Byte; overload;
function Base64Encode(const InputData: AnsiString; var OutputData: string): Byte; overload;
{* �����ݽ���BASE64���룬�����ɹ�����Base64_OK
|<PRE>
  InputData:AnsiString        - Ҫ���������
  var OutputData: AnsiString  - ����������
|</PRE>}

function Base64Decode(const InputData: AnsiString; var OutputData: AnsiString): Byte; overload;
function Base64Decode(const InputData: AnsiString; OutputData: TStream): Byte; overload;
{* �����ݽ���BASE64���룬�����ɹ�����Base64_OK
|<PRE>
  InputData:AnsiString        - Ҫ���������
  var OutputData: AnsiString  - ����������
|</PRE>}

// ԭʼ��ֲ�İ汾���Ƚ���
function Base64Encode_Slow(const InputData: AnsiString; var OutputData: AnsiString): Byte;

// ԭʼ��ֲ�İ汾���Ƚ���
function Base64Decode_Slow(const InputData: AnsiString; var OutputData: AnsiString): Byte;

const
  BASE64_OK       = 0; // ת���ɹ�
  BASE64_ERROR    = 1; // ת������δ֪���� (e.g. can't encode octet in input stream) -> error in implementation
  BASE64_INVALID  = 2; // ������ַ������зǷ��ַ� (�� FilterDecodeInput=False ʱ���ܳ���)
  BASE64_LENGTH   = 3; // ���ݳ��ȷǷ�
  BASE64_DATALEFT = 4; // too much input data left (receveived 'end of encoded data' but not end of input string)
  BASE64_PADDING  = 5; // ���������δ������ȷ������ַ�����

implementation

var
  FilterDecodeInput: Boolean = True;

const
  Base64TableLength = 64;
  Base64Table:string[Base64TableLength]='ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
  Pad = '=';

//------------------------------------------------------------------------------
// ����Ĳο���
//------------------------------------------------------------------------------

  EnCodeTab: array[0..64] of AnsiChar =
  (
    'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H',
    'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P',
    'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X',
    'Y', 'Z', 'a', 'b', 'c', 'd', 'e', 'f',
    'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n',
    'o', 'p', 'q', 'r', 's', 't', 'u', 'v',
    'w', 'x', 'y', 'z', '0', '1', '2', '3',
    '4', '5', '6', '7', '8', '9', '+', '/',
    '=');

//------------------------------------------------------------------------------
// ����Ĳο���
//------------------------------------------------------------------------------

  { �������� Base64 ������ַ�ֱ�Ӹ���, ����Ҳȡ����}
  DecodeTable: array[#0..#127] of Byte =
  (
    Byte('='), 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00,
    00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00,
    00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 62, 00, 00, 00, 63,
    52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 00, 00, 00, 00, 00, 00,
    00, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14,
    15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 00, 00, 00, 00, 00,
    00, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40,
    41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 00, 00, 00, 00, 00
    );  

// ԭʼ��ֲ�İ汾���Ƚ���
function Base64Encode_Slow(const InputData: AnsiString; var OutputData:AnsiString): Byte;
var
  i: Integer;
  CurrentB,PrevB: Byte;
  c: Byte;
  s: AnsiChar;
  InputLength: Integer;

  function ValueToCharacter(Value: Byte; var Character: AnsiChar): Boolean;
  //******************************************************************
  // ��һ����0..Base64TableLength-1�����ڵ�ֵ��ת��Ϊ��Base64�������Ӧ
  // ���ַ�����ʾ�����ת���ɹ��򷵻�True
  //******************************************************************
  begin
    Result := True;
    if (Value > Base64TableLength - 1) then
      Result := False
    else
      Character := AnsiChar(Base64Table[Value + 1]);
  end;

begin
  OutPutData := '';
  InputLength := Length(InputData);
  i:=1;
  if (InputLength = 0) then
  begin
    Result := BASE64_OK;
    Exit;
  end;

  repeat
    // ��һ��ת��
    CurrentB := Ord(InputData[i]);
    Inc(i);
    InputLength := InputLength-1;
    c := (CurrentB shr 2);
    if not ValueToCharacter(c, s) then
    begin
      Result := BASE64_ERROR;
      Exit;
    end;
    OutPutData := OutPutData + s;
    PrevB := CurrentB;

    // �ڶ���ת��
    if InputLength = 0 then
      CurrentB := 0
    else
    begin
      CurrentB := Ord(InputData[i]);
      Inc(i);
    end;

    InputLength := InputLength-1;
    c:=(PrevB and $03) shl 4 + (CurrentB shr 4);  //ȡ��XX��4λ����������4λ��XX����4λ�ϲ�����λ
    if not ValueToCharacter(c,s) then             //���ȡ�õ��ַ��Ƿ���Base64Table��
    begin
      Result := BASE64_ERROR;
      Exit;
    end;
    OutPutData := OutPutData+s;
    PrevB := CurrentB;

    // ������ת��
    if InputLength<0 then
      s := pad
    else
    begin
      if InputLength = 0 then
        CurrentB := 0
      else
      begin
        CurrentB := Ord(InputData[i]);
        Inc(i);
      end;
      InputLength := InputLength - 1;
      c := (PrevB and $0F) shl 2 + (CurrentB shr 6);//ȡ��XX��4λ����������2λ��XX����6λ�ϲ�����λ
      if not ValueToCharacter(c, s) then           //���ȡ�õ��ַ��Ƿ���Base64Table��
      begin
        Result := BASE64_ERROR;
        Exit;
      end;
    end;
    OutPutData:=OutPutData+s;

    // ���Ĵ�ת��
    if InputLength < 0 then
      s := pad
    else
    begin
      c := (CurrentB and $3F);                      //ȡ��XX��6λ
      if not ValueToCharacter(c, s) then           //���ȡ�õ��ַ��Ƿ���Base64Table��
      begin
        Result := BASE64_ERROR;
        Exit;
      end;
    end;
    OutPutData := OutPutData + s;
  until InputLength <= 0;

  Result:=BASE64_OK;
end;

// ԭʼ��ֲ�İ汾���Ƚ���
function Base64Decode_Slow(const InputData: AnsiString; var OutputData: AnsiString): Byte;
var
  i: Integer;
  InputLength: Integer;
  CurrentB, PrevB: Byte;
  c: Byte;
  s: AnsiChar;
  Data: AnsiString;

  function CharacterToValue(Character: AnsiChar; var Value: Byte): Boolean;
  //******************************************************************
  // ת���ַ�Ϊһ��0..Base64TableLength-1�����е�ֵ�����ת���ɹ���
  // ��True(���ַ���Base64Table��)
  //******************************************************************
  begin
    Result := True;
    Value := Pos(Character, Base64Table);
    if Value=0 then
      Result := False
    else
      Value := Value - 1;
  end;

  function FilterLine(const InputData: AnsiString): AnsiString;
  //******************************************************************
  // �������в���Base64Table�е��ַ�������ֵΪ���˺���ַ�
  //******************************************************************
  var
    f: Byte;
    i: Integer;
  begin
    Result := '';
    for i := 1 to Length(InputData) do
      if CharacterToValue(inputData[i], f) or (InputData[i] = Pad) then
        Result:=Result + InputData[i];
  end;

begin
  if (InputData = '') then
  begin
    Result := BASE64_OK;
    Exit;
  end;
  OutPutData := '';

  if FilterDecodeInput then
    Data := FilterLine(InputData)
  else
    Data := InputData;

  InputLength := Length(Data);
  if InputLength mod 4 <> 0 then
  begin
    Result := BASE64_LENGTH;
    Exit;
  end;

  i := 0;
  repeat
    // ��һ��ת��
    Inc(i);
    s := Data[i];
    if not CharacterToValue(s, CurrentB) then
    begin
      Result := BASE64_INVALID;
      Exit;
    end;

    Inc(i);
    s := Data[i];
    if not CharacterToValue(s, PrevB) then
    begin
      Result := BASE64_INVALID;
      Exit;
    end;

    c := (CurrentB shl 2) + (PrevB shr 4);
    OutPutData := {$IFDEF DELPHI12_UP}AnsiString{$ENDIF}(OutPutData + {$IFDEF DELPHI12_UP}AnsiString{$ENDIF}(Chr(c)));

    // �ڶ���ת��
    Inc(i);
    s := Data[i];
    if s = pad then
    begin
      if (i <> InputLength-1) then
      begin
        Result := BASE64_DATALEFT;
        Exit;
      end
      else
      if Data[i + 1] <> pad then
      begin
        Result := BASE64_PADDING;
        Exit;
      end;
    end
    else
    begin
      if not CharacterToValue(s,CurrentB) then
      begin
        Result:=BASE64_INVALID;
        Exit;
      end;
      c:=(PrevB shl 4) + (CurrentB shr 2);
      OutPutData := OutPutData+{$IFDEF DELPHI12_UP}AnsiString{$ENDIF}(chr(c));
    end;

    // ������ת��
    Inc(i);
    s := Data[i];
    if s = pad then
    begin
      if (i <> InputLength) then
      begin
        Result := BASE64_DATALEFT;
        Exit;
      end;
    end
    else
    begin
     if not CharacterToValue(s, PrevB) then
     begin
       Result := BASE64_INVALID;
       Exit;
     end;
     c := (CurrentB shl 6) + (PrevB);
     OutPutData := OutPutData + {$IFDEF DELPHI12_UP}AnsiString{$ENDIF}(Chr(c));
    end;
  until (i >= InputLength);

  Result:=BASE64_OK;
end;

// ����Ϊ wr960204 �Ľ��Ŀ��� Base64 ������㷨
function Base64Encode(InputData: TStream; var OutputData: string): Byte; overload;
var
  Str: TStringStream;
begin
  Str := TStringStream.Create('');
  try
    Str.CopyFrom(InputData, InputData.Size);
    Result := Base64Encode({$IFDEF DELPHI12_UP}AnsiString{$ENDIF}(Str.DataString), OutputData);
  finally
    Str.Free;
  end;
end;

function Base64Encode(const InputData: AnsiString; var OutputData: string): Byte; overload;
var
  Times, SrcLen, i: Integer;
  x1, x2, x3, x4: AnsiChar;
  xt: byte;
begin
  SrcLen := Length(InputData);
  if SrcLen mod 3 = 0 then
    Times := SrcLen div 3
  else
    Times := SrcLen div 3 + 1;
  SetLength(OutputData, Times * 4);   //һ�η��������ڴ�,����һ�δ��ַ������,һ�δ��ͷŷ����ڴ�

  for i := 0 to times - 1 do
  begin
    if SrcLen >= (3 + i * 3) then
    begin
      x1 := EnCodeTab[(ord(InputData[1 + i * 3]) shr 2)];
      xt := (ord(InputData[1 + i * 3]) shl 4) and 48;
      xt := xt or (ord(InputData[2 + i * 3]) shr 4);
      x2 := EnCodeTab[xt];
      xt := (Ord(InputData[2 + i * 3]) shl 2) and 60;
      xt := xt or (ord(InputData[3 + i * 3]) shr 6);
      x3 := EnCodeTab[xt];
      xt := (ord(InputData[3 + i * 3]) and 63);
      x4 := EnCodeTab[xt];
    end
    else if SrcLen >= (2 + i * 3) then
    begin
      x1 := EnCodeTab[(ord(InputData[1 + i * 3]) shr 2)];
      xt := (ord(InputData[1 + i * 3]) shl 4) and 48;
      xt := xt or (ord(InputData[2 + i * 3]) shr 4);
      x2 := EnCodeTab[xt];
      xt := (ord(InputData[2 + i * 3]) shl 2) and 60;
      x3 := EnCodeTab[xt ];
      x4 := '=';
    end
    else
    begin
      x1 := EnCodeTab[(ord(InputData[1 + i * 3]) shr 2)];
      xt := (ord(InputData[1 + i * 3]) shl 4) and 48;
      x2 := EnCodeTab[xt];
      x3 := '=';
      x4 := '=';
    end;
    OutputData[I shl 2 + 1] := Char(X1);
    OutputData[I shl 2 + 2] := Char(X2);
    OutputData[I shl 2 + 3] := Char(X3);
    OutputData[I shl 2 + 4] := Char(X4);
  end;
  OutputData := Trim(OutputData);
  Result := BASE64_OK;
end;

function Base64Decode(const InputData: AnsiString; OutputData: TStream): Byte; overload;
var
  Str: AnsiString;
begin
  Result := Base64Decode(InputData, Str);
  OutputData.Size := Length(Str);
  OutputData.Position := 0;
  if Str <> '' then
    OutputData.Write(Str[1], Length(Str));
end;

function Base64Decode(const InputData: AnsiString; var OutputData: AnsiString): Byte;
var
  SrcLen, Times, i: Integer;
  x1, x2, x3, x4, xt: Byte;
  C: Integer;
  Data: AnsiString;

  function FilterLine(const Source: AnsiString): AnsiString;
  var
    P, PP: PAnsiChar;
    I: Integer;
  begin
    SrcLen := Length(Source);
    GetMem(P, Srclen);                   //һ�η��������ڴ�,����һ�δ��ַ������,һ�δ��ͷŷ����ڴ�
    PP := P;
    FillChar(P^, Srclen, 0);
    for I := 1 to SrcLen do
    begin
      if Source[I] in ['0'..'9', 'A'..'Z', 'a'..'z', '+', '/', '='] then
      begin
        PP^ := Source[I];
        Inc(PP);
      end;
    end;
    SetString(Result, P, PP - P);        //��ȡ��Ч����
    FreeMem(P, SrcLen);
  end;

begin
  if (InputData = '') then
  begin
    Result := BASE64_OK;
    Exit;
  end;
  OutPutData:='';

  if FilterDecodeInput then
    Data := FilterLine(InputData)
  else
    Data := InputData;

  SrcLen := Length(Data);
  SetLength(OutputData, (SrcLen * 3 div 4));  //һ�η��������ڴ�,����һ�δ��ַ������,һ�δ��ͷŷ����ڴ�
  Times := SrcLen div 4;
  C := 1;

  for i := 0 to Times - 1 do
  begin
    x1 := DecodeTable[Data[1 + i shl 2]];
    x2 := DecodeTable[Data[2 + i shl 2]];
    x3 := DecodeTable[Data[3 + i shl 2]];
    x4 := DecodeTable[Data[4 + i shl 2]];
    x1 := x1 shl 2;
    xt := x2 shr 4;
    x1 := x1 or xt;
    x2 := x2 shl 4;
    OutputData[C] := AnsiChar(Chr(x1));
    Inc(C);
    if x3 = 64 then
      Break;
    xt := x3 shr 2;
    x2 := x2 or xt;
    x3 := x3 shl 6;
    OutputData[C] := AnsiChar(Chr(x2));
    Inc(C);
    if x4 = 64 then
      Break;
    x3 := x3 or x4;
    OutputData[C] := AnsiChar(Chr(x3));
    Inc(C);
  end;
  OutputData := {$IFDEF DELPHI12_UP}AnsiString{$ENDIF}({$IFDEF DELPHI12_UP}String{$ENDIF}(OutputData));
  Result := BASE64_OK;
end;

end.
