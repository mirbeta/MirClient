(*
  unit Owner: D10.Mofen, qdac.swish
  welcome to report bug: 185511468(qq), 185511468@qq.com
  Web site   : https://github.com/ymofen/msgpack-delphi

  * Delphi 2007 (tested)
  * XE5, XE7 (tested)

  + first release
  2014-08-15 13:05:13

  + add array support
  2014-08-19 12:18:47

  + add andriod support
  2014-09-08 00:45:27

  * fixed int32, int64 parse bug< integer, int64 parse zero>
  2014-11-09 22:35:27

  + add EncodeToFile/DecodeFromFile
  2014-11-13 12:30:58

  * fix  asVariant = null (thanks for cyw(26890954))
  2014-11-14 09:05:52

  * fix AsInteger = -1 bug (thanks for cyw(26890954))
  2014-11-14 12:15:52

  * fix AsInteger = -127 bug
  check int64/integer/cardinal/word/shortint/smallint/byte assign, encode,decode, read
  2014-11-14 12:30:38

  * fix AsFloat = 2.507182 bug
  thanks fo [珠海]-芒果  1939331207
  2014-11-21 12:37:04

  * add AddArrayChild func
  2015-03-25 17:47:28

  * add remove/removeFromParent/Delete function
  2015-08-29 22:37:48


  samples:
  lvMsgPack:=TSimpleMsgPack.Create;
  lvMsgPack.S['root.child01'] := 'abc';

  //save to stream
  lvMsgPack.EncodeToStream(pvStream);


  Copyright (c) 2014, ymofen, swish
  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are met:

  * Redistributions of source code must retain the above copyright notice, this
  list of conditions and the following disclaimer.

  * Redistributions in binary form must reproduce the above copyright notice,
  this list of conditions and the following disclaimer in the documentation
  and/or other materials provided with the distribution.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
  FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
  SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
  OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.



*)
unit SimpleMsgPack;

interface

uses
  classes, SysUtils
{$IFDEF UNICODE}, Generics.Collections{$ELSE}, Contnrs{$ENDIF}
{$IFDEF MSWINDOWS}, Windows{$ENDIF}
    , Variants;

type
{$IF RTLVersion<25}
  IntPtr = Integer;
{$IFEND IntPtr}
{$IF CompilerVersion < 18} // before delphi 2007
  TBytes = array of Byte;
{$IFEND}
  TMsgPackType = (mptUnknown, mptNull, mptMap, mptArray, mptString, mptInteger,
    mptBoolean, mptFloat, mptSingle, mptDateTime, mptBinary);

  // reserved
  IMsgPack = interface
    ['{37D3E479-7A46-435A-914D-08FBDA75B50E}']
  end;

  // copy from qmsgPack
  TMsgPackValue = packed record
    ValueType: Byte;
    case Integer of
      0:
        (U8Val: Byte);
      1:
        (I8Val: Shortint);
      2:
        (U16Val: Word);
      3:
        (I16Val: Smallint);
      4:
        (U32Val: Cardinal);
      5:
        (I32Val: Integer);
      6:
        (U64Val: UInt64);
      7:
        (I64Val: Int64);
      // 8:(F32Val:Single);
      // 9:(F64Val:Double);
      10:
        (BArray: array [0 .. 16] of Byte);
  end;

  TMsgPackSetting = class(TObject)
  private
    FCaseSensitive: Boolean;
  public
    property CaseSensitive: Boolean read FCaseSensitive write FCaseSensitive;
  end;

  TSimpleMsgPack = class(TObject)
  private

    FParent: TSimpleMsgPack;

    FLowerName: String;

    FName: String;

    FValue: TBytes;

    FDataType: TMsgPackType;

{$IFDEF UNICODE}
    FChildren: TList<TSimpleMsgPack>;
{$ELSE}
    FChildren: TList;
{$ENDIF}
    procedure InnerAddToChildren(pvDataType: TMsgPackType; obj: TSimpleMsgPack);
    function InnerAdd(pvDataType: TMsgPackType): TSimpleMsgPack; overload;
    function InnerAdd(): TSimpleMsgPack; overload;
    function GetCount: Integer;
    procedure InnerEncodeToStream(pvStream: TStream);
    procedure InnerParseFromStream(pvStream: TStream);

    procedure setName(pvName: string);
  private
    function getAsString: String;
    procedure setAsString(pvValue: string);

    function getAsInteger: Int64;
    procedure setAsInteger(pvValue: Int64);
    function GetAsBoolean: Boolean;
    procedure SetAsBoolean(const Value: Boolean);

    procedure SetAsFloat(const Value: Double);
    function GetAsFloat: Double;

    procedure SetAsDateTime(const Value: TDateTime);
    function GetAsDateTime: TDateTime;

    function GetAsVariant: Variant;
    procedure SetAsVariant(const Value: Variant);

    procedure SetAsSingle(const Value: Single);
    function GetAsSingle: Single;

    procedure SetAsBytes(const Value: TBytes);
    function GetAsBytes: TBytes;

    procedure checkObjectDataType(ANewType: TMsgPackType);

    function findObj(pvName: string): TSimpleMsgPack;
    function indexOf(pvName: string): Integer;
    function indexOfCaseSensitive(pvName: string): Integer;
    function indexOfIgnoreSensitive(pvLowerCaseName: string): Integer;

  private

    /// <summary>
    /// 通过路径查找子对象
    /// </summary>
    /// <param name="pvPath">要查找的子对象路径 比如: 'p1.age' </param>
    /// <param name="vParent">查找到的子对象的父对象</param>
    /// <param name="vIndex">查找到的子对象所在父对象的索引值</param>
    /// <returns>返回找到的子对象</returns>
    function InnerFindPathObject(pvPath: string; var vParent: TSimpleMsgPack;
      var vIndex: Integer): TSimpleMsgPack;

    function GetO(pvPath: String): TSimpleMsgPack;
    procedure SetO(pvPath: String; const Value: TSimpleMsgPack);

    function GetS(pvPath: String): string;
    procedure SetS(pvPath: String; const Value: string);

    function GetI(pvPath: String): Int64;
    procedure SetI(pvPath: String; const Value: Int64);

    function GetB(pvPath: String): Boolean;
    procedure SetB(pvPath: String; const Value: Boolean);

    function GetD(pvPath: String): Double;

    procedure SetD(pvPath: String; const Value: Double);

    function GetItems(AIndex: Integer): TSimpleMsgPack;

    /// <summary>
    /// 释放所有子对象，并清空子对象列表
    /// </summary>
    procedure ClearAndFreeAllChildren;
    function GetDataType: TMsgPackType;
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    /// 清空子对象以及本身值(null)
    /// </summary>
    procedure Clear;

    property Count: Integer read GetCount;

    procedure LoadBinaryFromStream(pvStream: TStream; pvLen: Cardinal = 0);
    procedure SaveBinaryToStream(pvStream: TStream);

    procedure LoadBinaryFromFile(pvFileName: String);
    procedure SaveBinaryToFile(pvFileName: String);

    procedure EncodeToStream(pvStream: TStream);
    procedure EncodeToFile(pvFileName: string);

    procedure DecodeFromStream(pvStream: TStream);
    procedure DecodeFromFile(pvFileName: string);

    function EncodeToBytes: TBytes;
    procedure DecodeFromBytes(pvBytes: TBytes);

    function Add(pvNameKey, pvValue: string): TSimpleMsgPack; overload;
    function Add(pvNameKey: string; pvValue: Int64): TSimpleMsgPack; overload;
    function Add(pvNameKey: string; pvValue: TBytes): TSimpleMsgPack; overload;
    function Add(pvNameKey: String): TSimpleMsgPack; overload;
    function Add(): TSimpleMsgPack; overload;

    /// <summary>
    /// 添加一个子对象,并负责对象的生命周期
    /// </summary>
    function Add(pvNameKey: string; pvValue: TSimpleMsgPack)
      : TSimpleMsgPack; overload;

    function AddArrayChild(): TSimpleMsgPack; overload;

    /// <summary>
    /// 添加一个子对象,并负责对象的生命周期
    /// </summary>
    function AddArrayChild(pvValue: TSimpleMsgPack): TSimpleMsgPack; overload;

    function ForcePathObject(pvPath: string): TSimpleMsgPack;

    /// <summary>
    /// 移除并释放对象
    /// </summary>
    /// <param name="pvPath">要移除的对象路径, 比如: 'p1.age' </param>
    /// <returns>如果移除成功返回true, 否则返回false(对象不存在)</returns>
    function DeleteObject(pvPath: String): Boolean;

    /// <summary>
    /// 根据索引值删除并释放对象
    /// </summary>
    /// <param name="pvIndex">子对象的索引值</param>
    /// <returns>如果移除成功返回true, 否则返回false(超出索引范围)</returns>
    function Delete(pvIndex: Integer): Boolean;

    /// <summary>
    /// 移除子对象,并不释放子对象
    /// </summary>
    /// <param name="pvPath">要移除的对象路径, 比如: 'p1.age' </param>
    /// <returns>如果移除成功返回移除的子对象, 否则返回nil(对象不存在)</returns>
    function Remove(pvPath: string): TSimpleMsgPack; overload;

    /// <summary>
    /// 移除子对象,并不释放子对象
    /// </summary>
    /// <param name="pvIndex">子对象的索引值</param>
    /// <returns>如果移除成功返回移除的子对象, 否则返回nil(索引超出范围)</returns>
    function Remove(pvIndex: Integer): TSimpleMsgPack; overload;

    /// <summary>
    /// 移除子对象,并不释放子对象
    /// </summary>
    /// <param name="pvChild">要移除的对象</param>
    /// <returns>如果移除成功返回true, 否则返回false(对象不是该子对象)</returns>
    function Remove(pvChild: TSimpleMsgPack): Boolean; overload;

    /// <summary>
    /// 从父对象中移除
    /// </summary>
    procedure RemoveFromParent();

    property AsInteger: Int64 read getAsInteger write setAsInteger;
    property AsString: string read getAsString write setAsString;
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsFloat: Double read GetAsFloat write SetAsFloat;
    property AsSingle: Single read GetAsSingle write SetAsSingle;
    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
    property AsVariant: Variant read GetAsVariant write SetAsVariant;

    property AsBytes: TBytes read GetAsBytes write SetAsBytes;

    property O[pvPath: String]: TSimpleMsgPack read GetO write SetO;
    property S[pvPath: String]: string read GetS write SetS;
    property I[pvPath: String]: Int64 read GetI write SetI;
    property B[pvPath: String]: Boolean read GetB write SetB;
    property D[pvPath: String]: Double read GetD write SetD;

    property Items[AIndex: Integer]: TSimpleMsgPack read GetItems; default;
    property DataType: TMsgPackType read GetDataType;
  end;

implementation

resourcestring
  SVariantConvertNotSupport = 'type to convert not support!。';
  SCannotAddChild = 'Can''t add child in this node!';

function swap16(const v): Word;
begin
  // FF, EE : EE->1, FF->2
  PByte(@result)^ := PByte(IntPtr(@v) + 1)^;
  PByte(IntPtr(@result) + 1)^ := PByte(@v)^;
end;

function swap32(const v): Cardinal;
begin
  // FF, EE, DD, CC : CC->1, DD->2, EE->3, FF->4
  PByte(@result)^ := PByte(IntPtr(@v) + 3)^;
  PByte(IntPtr(@result) + 1)^ := PByte(IntPtr(@v) + 2)^;
  PByte(IntPtr(@result) + 2)^ := PByte(IntPtr(@v) + 1)^;
  PByte(IntPtr(@result) + 3)^ := PByte(@v)^;
end;

function swap64(const v): Int64;
begin
  // FF, EE, DD, CC, BB, AA, 99, 88 : 88->1 ,99->2 ....
  PByte(@result)^ := PByte(IntPtr(@v) + 7)^;
  PByte(IntPtr(@result) + 1)^ := PByte(IntPtr(@v) + 6)^;
  PByte(IntPtr(@result) + 2)^ := PByte(IntPtr(@v) + 5)^;
  PByte(IntPtr(@result) + 3)^ := PByte(IntPtr(@v) + 4)^;
  PByte(IntPtr(@result) + 4)^ := PByte(IntPtr(@v) + 3)^;
  PByte(IntPtr(@result) + 5)^ := PByte(IntPtr(@v) + 2)^;
  PByte(IntPtr(@result) + 6)^ := PByte(IntPtr(@v) + 1)^;
  PByte(IntPtr(@result) + 7)^ := PByte(@v)^;
end;

// v and outVal is can't the same value
procedure swap64Ex(const v; out outVal);
begin
  // FF, EE, DD, CC, BB, AA, 99, 88 : 88->1 ,99->2 ....
  PByte(@outVal)^ := PByte(IntPtr(@v) + 7)^;
  PByte(IntPtr(@outVal) + 1)^ := PByte(IntPtr(@v) + 6)^;
  PByte(IntPtr(@outVal) + 2)^ := PByte(IntPtr(@v) + 5)^;
  PByte(IntPtr(@outVal) + 3)^ := PByte(IntPtr(@v) + 4)^;
  PByte(IntPtr(@outVal) + 4)^ := PByte(IntPtr(@v) + 3)^;
  PByte(IntPtr(@outVal) + 5)^ := PByte(IntPtr(@v) + 2)^;
  PByte(IntPtr(@outVal) + 6)^ := PByte(IntPtr(@v) + 1)^;
  PByte(IntPtr(@outVal) + 7)^ := PByte(@v)^;
end;

// v and outVal is can't the same value
procedure swap32Ex(const v; out outVal);
begin
  // FF, EE, DD, CC : CC->1, DD->2, EE->3, FF->4
  PByte(@outVal)^ := PByte(IntPtr(@v) + 3)^;
  PByte(IntPtr(@outVal) + 1)^ := PByte(IntPtr(@v) + 2)^;
  PByte(IntPtr(@outVal) + 2)^ := PByte(IntPtr(@v) + 1)^;
  PByte(IntPtr(@outVal) + 3)^ := PByte(@v)^;
end;

// v and outVal is can't the same value
procedure swap16Ex(const v; out outVal);
begin
  // FF, EE : EE->1, FF->2
  PByte(@outVal)^ := PByte(IntPtr(@v) + 1)^;
  PByte(IntPtr(@outVal) + 1)^ := PByte(@v)^;
end;

// overload swap, result type is integer, because single maybe NaN
function swap(v: Single): Integer; overload;
begin
  swap32Ex(v, result);
end;

// overload swap
function swap(v: Word): Word; overload;
begin
  swap16Ex(v, result);
end;

// overload swap
function swap(v: Cardinal): Cardinal; overload;
begin
  swap32Ex(v, result);
end;

// swap , result type is Int64, because Double maybe NaN
function swap(v: Double): Int64; overload;
begin
  swap64Ex(v, result);
end;

// copy from qstring
function BinToHex(p: Pointer; l: Integer; ALowerCase: Boolean): string;
const
  B2HConvert: array [0 .. 15] of Char = ('0', '1', '2', '3', '4', '5', '6', '7',
    '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');
  B2HConvertL: array [0 .. 15] of Char = ('0', '1', '2', '3', '4', '5', '6',
    '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f');
var
  pd: PChar;
  pb: PByte;
begin
  if SizeOf(Char) = 2 then
  begin
    SetLength(result, l shl 1);
  end
  else
  begin
    SetLength(result, l);
  end;
  pd := PChar(result);
  pb := p;
  if ALowerCase then
  begin
    while l > 0 do
    begin
      pd^ := B2HConvertL[pb^ shr 4];
      Inc(pd);
      pd^ := B2HConvertL[pb^ and $0F];
      Inc(pd);
      Inc(pb);
      Dec(l);
    end;
  end
  else
  begin
    while l > 0 do
    begin
      pd^ := B2HConvert[pb^ shr 4];
      Inc(pd);
      pd^ := B2HConvert[pb^ and $0F];
      Inc(pd);
      Inc(pb);
      Dec(l);
    end;
  end;
end;

function getFirst(var strPtr: PChar; splitChars: TSysCharSet): string;
var
  oPtr: PChar;
  l: Cardinal;
begin
  oPtr := strPtr;
  result := '';
  while True do
  begin
    if (strPtr^ in splitChars) then
    begin
      l := strPtr - oPtr;
      if l > 0 then
      begin
{$IFDEF UNICODE}
        SetLength(result, l);
        Move(oPtr^, PChar(result)^, l shl 1);
{$ELSE}
        SetLength(result, l);
        Move(oPtr^, PChar(result)^, l);
{$ENDIF}
        break;
      end;
    end
    else if (strPtr^ = #0) then
    begin
      l := strPtr - oPtr;
      if l > 0 then
      begin
{$IFDEF UNICODE}
        SetLength(result, l);
        Move(oPtr^, PChar(result)^, l shl 1);
{$ELSE}
        SetLength(result, l);
        Move(oPtr^, PChar(result)^, l);
{$ENDIF}
      end;
      break;
    end;
    Inc(strPtr);
  end;
end;

function Utf8DecodeEx(pvValue:
{$IFDEF UNICODE}TBytes{$ELSE}AnsiString{$ENDIF}; len: Cardinal): string;
{$IFDEF UNICODE}
var
  lvBytes: TBytes;
{$ENDIF}
begin
{$IFDEF UNICODE}
  lvBytes := TEncoding.Convert(TEncoding.UTF8, TEncoding.Unicode, pvValue);
  SetLength(result, Length(lvBytes) shr 1);
  Move(lvBytes[0], PChar(result)^, Length(lvBytes));
{$ELSE}
  result := UTF8Decode(pvValue);
{$ENDIF}
end;

function Utf8EncodeEx(pvValue: string):
{$IFDEF UNICODE}TBytes{$ELSE}AnsiString{$ENDIF};
{$IFDEF UNICODE}
var
  lvBytes: TBytes;
  len: Cardinal;
{$ENDIF}
begin
{$IFDEF UNICODE}
  len := Length(pvValue) shl 1;
  SetLength(lvBytes, len);
  Move(PChar(pvValue)^, lvBytes[0], len);
  result := TEncoding.Convert(TEncoding.Unicode, TEncoding.UTF8, lvBytes);
{$ELSE}
  result := UTF8Encode(pvValue);
{$ENDIF}
end;

// copy from qmsgPack
procedure writeString(pvValue: string; pvStream: TStream);
var

  lvRawData: {$IFDEF UNICODE}TBytes{$ELSE}AnsiString{$ENDIF};
  l: Integer;
  lvValue: TMsgPackValue;
begin
  lvRawData := Utf8EncodeEx(pvValue);
  l := Length(lvRawData);

  //
  // fixstr stores a byte array whose length is upto 31 bytes:
  // +--------+========+
  // |101XXXXX|  data  |
  // +--------+========+
  //
  // str 8 stores a byte array whose length is upto (2^8)-1 bytes:
  // +--------+--------+========+
  // |  0xd9  |YYYYYYYY|  data  |
  // +--------+--------+========+
  //
  // str 16 stores a byte array whose length is upto (2^16)-1 bytes:
  // +--------+--------+--------+========+
  // |  0xda  |ZZZZZZZZ|ZZZZZZZZ|  data  |
  // +--------+--------+--------+========+
  //
  // str 32 stores a byte array whose length is upto (2^32)-1 bytes:
  // +--------+--------+--------+--------+--------+========+
  // |  0xdb  |AAAAAAAA|AAAAAAAA|AAAAAAAA|AAAAAAAA|  data  |
  // +--------+--------+--------+--------+--------+========+
  //
  // where
  // * XXXXX is a 5-bit unsigned integer which represents N
  // * YYYYYYYY is a 8-bit unsigned integer which represents N
  // * ZZZZZZZZ_ZZZZZZZZ is a 16-bit big-endian unsigned integer which represents N
  // * AAAAAAAA_AAAAAAAA_AAAAAAAA_AAAAAAAA is a 32-bit big-endian unsigned integer which represents N
  // * N is the length of data

  if l <= 31 then
  begin
    lvValue.ValueType := $A0 + Byte(l);
    pvStream.WriteBuffer(lvValue.ValueType, 1);
  end
  else if l <= 255 then
  begin
    lvValue.ValueType := $D9;
    lvValue.U8Val := Byte(l);
    pvStream.WriteBuffer(lvValue, 2);
  end
  else if l <= 65535 then
  begin
    lvValue.ValueType := $DA;
    lvValue.U16Val := ((l shr 8) and $FF) or ((l shl 8) and $FF00);
    pvStream.Write(lvValue, 3);
  end
  else
  begin
    lvValue.ValueType := $DB;
    lvValue.BArray[0] := (l shr 24) and $FF;
    lvValue.BArray[1] := (l shr 16) and $FF;
    lvValue.BArray[2] := (l shr 8) and $FF;
    lvValue.BArray[3] := l and $FF;
    pvStream.WriteBuffer(lvValue, 5);
  end;

{$IFDEF UNICODE}
  pvStream.Write(PByte(@lvRawData[0])^, l);
{$ELSE}
  pvStream.Write(PByte(lvRawData)^, l);
{$ENDIF};
end;

procedure WriteBinary(p: PByte; l: Integer; pvStream: TStream);
var
  lvValue: TMsgPackValue;
begin
  if l <= 255 then
  begin
    lvValue.ValueType := $C4;
    lvValue.U8Val := Byte(l);
    pvStream.WriteBuffer(lvValue, 2);
  end
  else if l <= 65535 then
  begin
    lvValue.ValueType := $C5;
    lvValue.BArray[0] := (l shr 8) and $FF;
    lvValue.BArray[1] := l and $FF;
    pvStream.WriteBuffer(lvValue, 3);
  end
  else
  begin
    lvValue.ValueType := $C6;
    lvValue.BArray[0] := (l shr 24) and $FF;
    lvValue.BArray[1] := (l shr 16) and $FF;
    lvValue.BArray[2] := (l shr 8) and $FF;
    lvValue.BArray[3] := l and $FF;
    pvStream.WriteBuffer(lvValue, 5);
  end;
  pvStream.WriteBuffer(p^, l);
end;

// copy from qmsgPack
procedure WriteInt(const iVal: Int64; AStream: TStream);
var
  lvValue: TMsgPackValue;
begin
  if iVal >= 0 then
  begin
    if iVal <= 127 then
    begin
      lvValue.U8Val := Byte(iVal);
      AStream.WriteBuffer(lvValue.U8Val, 1);
    end
    else if iVal <= 255 then // UInt8
    begin
      lvValue.ValueType := $CC;
      lvValue.U8Val := Byte(iVal);
      AStream.WriteBuffer(lvValue, 2);
    end
    else if iVal <= 65535 then
    begin
      lvValue.ValueType := $CD;
      lvValue.BArray[0] := (iVal shr 8);
      lvValue.BArray[1] := (iVal and $FF);
      AStream.WriteBuffer(lvValue, 3);
    end
    else if iVal <= Cardinal($FFFFFFFF) then
    begin
      lvValue.ValueType := $CE;
      lvValue.BArray[0] := (iVal shr 24) and $FF;
      lvValue.BArray[1] := (iVal shr 16) and $FF;
      lvValue.BArray[2] := (iVal shr 8) and $FF;
      lvValue.BArray[3] := iVal and $FF;
      AStream.WriteBuffer(lvValue, 5);
    end
    else
    begin
      lvValue.ValueType := $CF;
      lvValue.BArray[0] := (iVal shr 56) and $FF;
      lvValue.BArray[1] := (iVal shr 48) and $FF;
      lvValue.BArray[2] := (iVal shr 40) and $FF;
      lvValue.BArray[3] := (iVal shr 32) and $FF;
      lvValue.BArray[4] := (iVal shr 24) and $FF;
      lvValue.BArray[5] := (iVal shr 16) and $FF;
      lvValue.BArray[6] := (iVal shr 8) and $FF;
      lvValue.BArray[7] := iVal and $FF;
      AStream.WriteBuffer(lvValue, 9);
    end;
  end
  else // <0
  begin
    if iVal <= Low(Integer) then // -2147483648  // 64 bit
    begin
      lvValue.ValueType := $D3;
      lvValue.BArray[0] := (iVal shr 56) and $FF;
      lvValue.BArray[1] := (iVal shr 48) and $FF;
      lvValue.BArray[2] := (iVal shr 40) and $FF;
      lvValue.BArray[3] := (iVal shr 32) and $FF;
      lvValue.BArray[4] := (iVal shr 24) and $FF;
      lvValue.BArray[5] := (iVal shr 16) and $FF;
      lvValue.BArray[6] := (iVal shr 8) and $FF;
      lvValue.BArray[7] := iVal and $FF;
      AStream.WriteBuffer(lvValue, 9);
    end
    else if iVal <= Low(Smallint) then // -32768    // 32 bit
    begin
      lvValue.ValueType := $D2;
      lvValue.BArray[0] := (iVal shr 24) and $FF;
      lvValue.BArray[1] := (iVal shr 16) and $FF;
      lvValue.BArray[2] := (iVal shr 8) and $FF;
      lvValue.BArray[3] := iVal and $FF;
      AStream.WriteBuffer(lvValue, 5);
    end
    else if iVal <= -128 then
    begin
      lvValue.ValueType := $D1;
      lvValue.BArray[0] := (iVal shr 8);
      lvValue.BArray[1] := (iVal and $FF);
      AStream.WriteBuffer(lvValue, 3);
    end
    else if iVal < -32 then
    begin
      lvValue.ValueType := $D0;
      lvValue.I8Val := iVal;
      AStream.WriteBuffer(lvValue, 2);
    end
    else
    begin
      lvValue.I8Val := iVal;
      AStream.Write(lvValue.I8Val, 1);
    end;
  end; // End <0
end;

procedure WriteFloat(pvVal: Double; AStream: TStream);
var
  lvValue: TMsgPackValue;
begin

  lvValue.I64Val := swap(pvVal);
  lvValue.ValueType := $CB;
  AStream.WriteBuffer(lvValue, 9);
end;

procedure WriteSingle(pvVal: Single; AStream: TStream);
var
  lvValue: TMsgPackValue;
begin
  lvValue.I32Val := swap(pvVal);
  lvValue.ValueType := $CA;
  AStream.WriteBuffer(lvValue, 5);
end;

procedure WriteNull(pvStream: TStream);
var
  lvByte: Byte;
begin
  lvByte := $C0;
  pvStream.Write(lvByte, 1);
end;

procedure WriteBoolean(pvValue: Boolean; pvStream: TStream);
var
  lvByte: Byte;
begin
  if pvValue then
    lvByte := $C3
  else
    lvByte := $C2;
  pvStream.Write(lvByte, 1);
end;

/// <summary>
/// copy from qmsgpack
/// </summary>
procedure writeArray(obj: TSimpleMsgPack; pvStream: TStream);
var
  c, I: Integer;
  lvValue: TMsgPackValue;
  lvNode: TSimpleMsgPack;
begin
  c := obj.Count;

  if c <= 15 then
  begin
    lvValue.ValueType := $90 + c;
    pvStream.WriteBuffer(lvValue.ValueType, 1);
  end
  else if c <= 65535 then
  begin
    lvValue.ValueType := $DC;
    lvValue.BArray[0] := (c shr 8) and $FF;
    lvValue.BArray[1] := c and $FF;
    pvStream.WriteBuffer(lvValue, 3);
  end
  else
  begin
    lvValue.ValueType := $DD;
    lvValue.BArray[0] := (c shr 24) and $FF;
    lvValue.BArray[1] := (c shr 16) and $FF;
    lvValue.BArray[2] := (c shr 8) and $FF;
    lvValue.BArray[3] := c and $FF;
    pvStream.WriteBuffer(lvValue, 5);
  end;

  for I := 0 to c - 1 do
  begin
    lvNode := TSimpleMsgPack(obj.FChildren[I]);
    lvNode.InnerEncodeToStream(pvStream);
  end;
end;

procedure writeMap(obj: TSimpleMsgPack; pvStream: TStream);
var
  c, I: Integer;
  lvValue: TMsgPackValue;
  lvNode: TSimpleMsgPack;
begin
  c := obj.Count;
  if c <= 15 then
  begin
    lvValue.ValueType := $80 + c;
    pvStream.WriteBuffer(lvValue.ValueType, 1);
  end
  else if c <= 65535 then
  begin
    lvValue.ValueType := $DE;
    lvValue.BArray[0] := (c shr 8) and $FF;
    lvValue.BArray[1] := c and $FF;
    pvStream.WriteBuffer(lvValue, 3);
  end
  else
  begin
    lvValue.ValueType := $DF;
    lvValue.BArray[0] := (c shr 24) and $FF;
    lvValue.BArray[1] := (c shr 16) and $FF;
    lvValue.BArray[2] := (c shr 8) and $FF;
    lvValue.BArray[3] := c and $FF;
    pvStream.WriteBuffer(lvValue, 5);
  end;
  for I := 0 to c - 1 do
  begin
    lvNode := TSimpleMsgPack(obj.FChildren[I]);
    writeString(lvNode.FName, pvStream);
    lvNode.InnerEncodeToStream(pvStream);
  end;
end;

function EncodeDateTime(pvVal: TDateTime): string;
var
  AValue: TDateTime;
begin
  AValue := pvVal;
  if AValue - Trunc(AValue) = 0 then // Date
    result := FormatDateTime('yyyy-MM-dd', AValue)
  else
  begin
    if Trunc(AValue) = 0 then
      result := FormatDateTime('hh:nn:ss.zzz', AValue)
    else
      result := FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', AValue);
  end;
end;

constructor TSimpleMsgPack.Create;
begin
  inherited Create;
{$IFDEF UNICODE}
  FChildren := TList<TSimpleMsgPack>.Create();
{$ELSE}
  FChildren := TList.Create();
{$ENDIF}
end;

procedure TSimpleMsgPack.DecodeFromBytes(pvBytes: TBytes);
var
  lvStream: TStream;
begin
  lvStream := TMemoryStream.Create;
  try
    lvStream.Write(pvBytes[0], Length(pvBytes));
    lvStream.Position := 0;
    DecodeFromStream(lvStream);
  finally
    lvStream.Free;
  end;

end;

procedure TSimpleMsgPack.DecodeFromFile(pvFileName: string);
var
  lvFileStream: TFileStream;
begin
  if FileExists(pvFileName) then
  begin
    lvFileStream := TFileStream.Create(pvFileName, fmOpenRead);
    try
      DecodeFromStream(lvFileStream);
    finally
      lvFileStream.Free;
    end;
  end;
end;

procedure TSimpleMsgPack.DecodeFromStream(pvStream: TStream);
begin
  InnerParseFromStream(pvStream);
end;

function TSimpleMsgPack.Delete(pvIndex: Integer): Boolean;
begin
  if (pvIndex < 0) or (pvIndex >= Count) then
  begin
    result := false;
  end
  else
  begin
    TObject(FChildren[pvIndex]).Free;
    FChildren.Delete(pvIndex);
    result := True;
  end;
end;

function TSimpleMsgPack.DeleteObject(pvPath: String): Boolean;
var
  lvParent, lvObj: TSimpleMsgPack;
  j: Integer;
begin
  lvObj := InnerFindPathObject(pvPath, lvParent, j);
  result := lvObj <> nil;
  if result then
  begin
    lvParent.FChildren.Delete(j);
    lvObj.Free;
  end;
end;

destructor TSimpleMsgPack.Destroy;
begin
  ClearAndFreeAllChildren;
  FChildren.Free;
  FChildren := nil;
  inherited Destroy;
end;

function TSimpleMsgPack.Add(pvNameKey, pvValue: string): TSimpleMsgPack;
begin
  result := InnerAdd(mptMap);
  result.setName(pvNameKey);
  result.AsString := pvValue;
end;

function TSimpleMsgPack.Add(pvNameKey: string; pvValue: Int64): TSimpleMsgPack;
begin
  result := InnerAdd(mptMap);
  result.setName(pvNameKey);
  result.AsInteger := pvValue;
end;

function TSimpleMsgPack.Add: TSimpleMsgPack;
begin
  result := InnerAdd(mptMap);
end;

function TSimpleMsgPack.AddArrayChild: TSimpleMsgPack;
begin
  if FDataType <> mptArray then
  begin
    Clear();
    FDataType := mptArray;
  end;
  result := InnerAdd;
end;

function TSimpleMsgPack.Add(pvNameKey: string; pvValue: TBytes): TSimpleMsgPack;
begin
  result := InnerAdd(mptMap);
  result.setName(pvNameKey);
  result.FDataType := mptBinary;
  result.FValue := pvValue;
end;

function TSimpleMsgPack.Add(pvNameKey: String): TSimpleMsgPack;
begin
  result := InnerAdd(mptMap);
  result.setName(pvNameKey);
end;

function TSimpleMsgPack.Add(pvNameKey: string; pvValue: TSimpleMsgPack)
  : TSimpleMsgPack;
begin
  InnerAddToChildren(mptMap, pvValue);
  pvValue.FName := pvNameKey;
  result := pvValue;
end;

function TSimpleMsgPack.AddArrayChild(pvValue: TSimpleMsgPack): TSimpleMsgPack;
begin
  InnerAddToChildren(mptArray, pvValue);
  result := pvValue;
end;

procedure TSimpleMsgPack.checkObjectDataType(ANewType: TMsgPackType);
begin
  if (FDataType <> ANewType) then
  begin
    FDataType := ANewType;
  end;
end;

procedure TSimpleMsgPack.Clear;
begin
  ClearAndFreeAllChildren;
  FDataType := mptNull;
  SetLength(FValue, 0);
end;

function TSimpleMsgPack.EncodeToBytes: TBytes;
var
  lvStream: TStream;
begin
  lvStream := TMemoryStream.Create;
  try
    EncodeToStream(lvStream);
    lvStream.Position := 0;
    SetLength(result, lvStream.size);
    lvStream.Read(result[0], lvStream.size);
  finally
    lvStream.Free;
  end;
end;

procedure TSimpleMsgPack.EncodeToFile(pvFileName: string);
var
  lvFileStream: TFileStream;
begin
  if FileExists(pvFileName) then
    lvFileStream := TFileStream.Create(pvFileName, fmOpenWrite)
  else
    lvFileStream := TFileStream.Create(pvFileName, fmCreate);
  try
    lvFileStream.size := 0;
    EncodeToStream(lvFileStream);
  finally
    lvFileStream.Free;
  end;
end;

procedure TSimpleMsgPack.EncodeToStream(pvStream: TStream);
begin
  InnerEncodeToStream(pvStream);
end;

function TSimpleMsgPack.findObj(pvName: string): TSimpleMsgPack;
var
  I: Integer;
begin
  I := indexOfCaseSensitive(pvName);
  if I <> -1 then
  begin
    result := TSimpleMsgPack(FChildren[I]);
  end
  else
  begin
    result := nil;
  end;
end;

function TSimpleMsgPack.ForcePathObject(pvPath: string): TSimpleMsgPack;
var
  lvName: string;
  S: string;
  sPtr: PChar;
  lvTempObj, lvParent: TSimpleMsgPack;
  j: Integer;
begin
  result := nil;
  S := pvPath;

  lvParent := Self;
  sPtr := PChar(S);
  while sPtr^ <> #0 do
  begin
    lvName := getFirst(sPtr, ['.', '/', '\']);
    if lvName = '' then
    begin
      break;
    end
    else
    begin
      if sPtr^ = #0 then
      begin // end
        j := lvParent.indexOf(lvName);
        if j <> -1 then
        begin
          result := TSimpleMsgPack(lvParent.FChildren[j]);
        end
        else
        begin
          result := lvParent.Add(lvName);
        end;
      end
      else
      begin
        // find childrean
        lvTempObj := lvParent.findObj(lvName);
        if lvTempObj = nil then
        begin
          lvParent := lvParent.Add(lvName);
        end
        else
        begin
          lvParent := lvTempObj;
        end;
      end;
    end;
    if sPtr^ = #0 then
      break;
    Inc(sPtr);
  end;
end;

function TSimpleMsgPack.GetDataType: TMsgPackType;
begin
  if Self.FParent = nil then
    result := mptUnknown
  else
    result := FDataType;

end;

procedure TSimpleMsgPack.ClearAndFreeAllChildren;
var
  I: Integer;
begin
  for I := 0 to FChildren.Count - 1 do
  begin
    TObject(FChildren[I]).Free;
  end;
  FChildren.Clear;
end;

function TSimpleMsgPack.GetAsBoolean: Boolean;
begin
  if FDataType = mptBoolean then
    result := PBoolean(FValue)^
  else if FDataType = mptString then
    result := StrToBoolDef(AsString, false)
  else if FDataType = mptInteger then
    result := (AsInteger <> 0)
  else if FDataType in [mptNull, mptUnknown] then
    result := false
  else
    result := false;

end;

function TSimpleMsgPack.GetAsBytes: TBytes;
begin
  result := FValue;
end;

function TSimpleMsgPack.GetAsDateTime: TDateTime;
begin
  if FDataType in [mptDateTime, mptFloat] then
    result := PDouble(FValue)^
  else if FDataType = mptSingle then
    result := PSingle(FValue)^
  else if FDataType = mptString then
  begin
    result := StrToDateTimeDef(getAsString, 0);
  end
  else if FDataType in [mptInteger] then
    result := AsInteger
  else
    result := 0;
end;

function TSimpleMsgPack.GetAsFloat: Double;
begin
  if FDataType in [mptFloat, mptDateTime] then
    result := PDouble(FValue)^
  else if FDataType = mptSingle then
    result := PSingle(FValue)^
  else if FDataType = mptBoolean then
    result := Integer(AsBoolean)
  else if FDataType = mptString then
    result := StrToFloatDef(AsString, 0)
  else if FDataType = mptInteger then
    result := AsInteger
  else
    result := 0;
end;

function TSimpleMsgPack.getAsInteger: Int64;
begin
  case FDataType of
    mptInteger:
      result := PInt64(FValue)^;
  else
    result := 0;
  end;
end;

function TSimpleMsgPack.GetAsSingle: Single;
begin
  if FDataType in [mptFloat, mptDateTime] then
    result := PDouble(FValue)^
  else if FDataType = mptSingle then
    result := PSingle(FValue)^
  else if FDataType = mptBoolean then
    result := Integer(AsBoolean)
  else if FDataType = mptString then
    result := StrToFloatDef(AsString, 0)
  else if FDataType = mptInteger then
    result := AsInteger
  else
    result := 0;
end;

function TSimpleMsgPack.getAsString: String;
var
  l: Cardinal;
begin
  result := '';
  if FDataType = mptString then
  begin
    l := Length(FValue);
    if l = 0 then
    begin
      result := '';
    end
    else if SizeOf(Char) = 2 then
    begin
      SetLength(result, l shr 1);
      Move(FValue[0], PChar(result)^, l);
    end
    else
    begin
      SetLength(result, l);
      Move(FValue[0], PChar(result)^, l);
    end;
  end
  else
  begin
    case FDataType of
      mptUnknown, mptNull:
        result := '';
      mptInteger:
        result := IntToStr(AsInteger);
      mptBoolean:
        result := BoolToStr(AsBoolean, True);
      mptFloat:
        result := FloatToStrF(AsFloat, ffGeneral, 15, 0);
      mptSingle:
        result := FloatToStrF(AsSingle, ffGeneral, 7, 0);
      mptBinary:
        result := BinToHex(@FValue[0], Length(FValue), false);
      mptDateTime:
        result := EncodeDateTime(AsDateTime);
      // mptArray:
      // Result := EncodeArray;
      // mptMap:
      // Result := EncodeMap;
      // mptExtended:
      // Result := EncodeExtended;
    else
      result := '';
    end;
  end;
  // showMessage(Result);
end;

/// <summary>
/// copy from qdac3
/// </summary>
function TSimpleMsgPack.GetAsVariant: Variant;
var
  I: Integer;
  procedure BytesAsVariant;
  var
    l: Integer;
    p: PByte;
  begin
    l := Length(FValue);
    result := VarArrayCreate([0, l - 1], varByte);
    p := VarArrayLock(result);
    Move(FValue[0], p^, l);
    VarArrayUnlock(result);
  end;

begin
  case FDataType of
    mptNull:
      result := null;
    mptString:
      result := AsString;
    mptInteger:
      result := AsInteger;
    mptFloat:
      result := AsFloat;
    mptSingle:
      result := AsSingle;
    mptDateTime:
      result := AsDateTime;
    mptBoolean:
      result := AsBoolean;
    mptArray, mptMap:
      begin
        result := VarArrayCreate([0, Count - 1], varVariant);
        for I := 0 to Count - 1 do
          result[I] := TSimpleMsgPack(FChildren[I]).AsVariant;
      end;
    mptBinary:
      BytesAsVariant;
  else
    raise Exception.Create(SVariantConvertNotSupport);
  end;
end;

function TSimpleMsgPack.GetB(pvPath: String): Boolean;
var
  lvObj: TSimpleMsgPack;
begin
  lvObj := GetO(pvPath);
  if lvObj = nil then
  begin
    result := false;
  end
  else
  begin
    result := lvObj.AsBoolean;
  end;
end;

function TSimpleMsgPack.GetCount: Integer;
begin
  result := FChildren.Count;
end;

function TSimpleMsgPack.GetD(pvPath: String): Double;
var
  lvObj: TSimpleMsgPack;
begin
  lvObj := GetO(pvPath);
  if lvObj = nil then
  begin
    result := 0;
  end
  else
  begin
    result := lvObj.AsFloat;
  end;
end;

function TSimpleMsgPack.GetI(pvPath: String): Int64;
var
  lvObj: TSimpleMsgPack;
begin
  lvObj := GetO(pvPath);
  if lvObj = nil then
  begin
    result := 0;
  end
  else
  begin
    result := lvObj.AsInteger;
  end;
end;

function TSimpleMsgPack.GetItems(AIndex: Integer): TSimpleMsgPack;
begin
  result := TSimpleMsgPack(FChildren[AIndex]);
end;

function TSimpleMsgPack.GetO(pvPath: String): TSimpleMsgPack;
var
  lvParent: TSimpleMsgPack;
  j: Integer;
begin
  result := InnerFindPathObject(pvPath, lvParent, j);
end;

function TSimpleMsgPack.GetS(pvPath: String): string;
var
  lvObj: TSimpleMsgPack;
begin
  lvObj := GetO(pvPath);
  if lvObj = nil then
  begin
    result := '';
  end
  else
  begin
    result := lvObj.AsString;
  end;
end;

function TSimpleMsgPack.indexOf(pvName: string): Integer;
begin
  result := indexOfIgnoreSensitive(LowerCase(pvName));
end;

function TSimpleMsgPack.indexOfCaseSensitive(pvName: string): Integer;
var
  I, l: Integer;
  lvObj: TSimpleMsgPack;
begin
  result := -1;
  l := Length(pvName);
  if l = 0 then
    exit;
  for I := 0 to FChildren.Count - 1 do
  begin
    lvObj := TSimpleMsgPack(FChildren[I]);
    if Length(lvObj.FName) = l then
    begin
      if lvObj.FName = pvName then
      begin
        result := I;
        break;
      end;
    end;
  end;
end;

function TSimpleMsgPack.indexOfIgnoreSensitive(pvLowerCaseName: string)
  : Integer;
var
  I, l: Integer;
  lvObj: TSimpleMsgPack;
begin
  result := -1;
  l := Length(pvLowerCaseName);
  if l = 0 then
    exit;
  for I := 0 to FChildren.Count - 1 do
  begin
    lvObj := TSimpleMsgPack(FChildren[I]);
    if Length(lvObj.FLowerName) = l then
    begin
      if lvObj.FLowerName = pvLowerCaseName then
      begin
        result := I;
        break;
      end;
    end;
  end;
end;

function TSimpleMsgPack.InnerAdd(pvDataType: TMsgPackType): TSimpleMsgPack;
begin
  result := TSimpleMsgPack.Create;
  result.FDataType := mptUnknown;
  InnerAddToChildren(pvDataType, result);
end;

function TSimpleMsgPack.InnerAdd: TSimpleMsgPack;
begin
  if Self.FDataType in [mptMap, mptArray] then
  begin
    result := TSimpleMsgPack.Create;
    result.FDataType := mptUnknown;
    result.FParent := Self;
    FChildren.Add(result);
  end
  else
  begin
    raise Exception.Create(SCannotAddChild);
  end;

end;

procedure TSimpleMsgPack.InnerAddToChildren(pvDataType: TMsgPackType;
  obj: TSimpleMsgPack);
begin
  checkObjectDataType(pvDataType);
  obj.FParent := Self;
  FChildren.Add(obj);
end;

procedure TSimpleMsgPack.InnerEncodeToStream(pvStream: TStream);
begin
  case FDataType of
    mptUnknown, mptNull:
      WriteNull(pvStream);
    mptMap:
      writeMap(Self, pvStream);
    mptArray:
      writeArray(Self, pvStream);
    mptString:
      writeString(Self.getAsString, pvStream);
    mptInteger:
      WriteInt(Self.getAsInteger, pvStream);
    mptBoolean:
      WriteBoolean(Self.GetAsBoolean, pvStream);
    mptDateTime, mptFloat:
      WriteFloat(GetAsFloat, pvStream);
    mptSingle:
      WriteSingle(GetAsSingle, pvStream);
    mptBinary:
      WriteBinary(PByte(@FValue[0]), Length(FValue), pvStream);
  end;
end;

function TSimpleMsgPack.InnerFindPathObject(pvPath: string;
  var vParent: TSimpleMsgPack; var vIndex: Integer): TSimpleMsgPack;
var
  lvName: string;
  S: string;
  sPtr: PChar;
  lvTempObj, lvParent: TSimpleMsgPack;
  j: Integer;
begin
  S := pvPath;

  result := nil;

  lvParent := Self;
  sPtr := PChar(S);
  while sPtr^ <> #0 do
  begin
    lvName := getFirst(sPtr, ['.', '/', '\']);
    if lvName = '' then
    begin
      break;
    end
    else
    begin
      if sPtr^ = #0 then
      begin // end
        j := lvParent.indexOf(lvName);
        if j <> -1 then
        begin
          result := TSimpleMsgPack(lvParent.FChildren[j]);
          vIndex := j;
          vParent := lvParent;
        end
        else
        begin
          break;
        end;
      end
      else
      begin
        // find childrean
        lvTempObj := lvParent.findObj(lvName);
        if lvTempObj = nil then
        begin
          break;
        end
        else
        begin
          lvParent := lvTempObj;
        end;
      end;
    end;
    if sPtr^ = #0 then
      break;
    Inc(sPtr);
  end;
end;

procedure TSimpleMsgPack.InnerParseFromStream(pvStream: TStream);
var
  lvByte: Byte;
  lvBData: array [0 .. 15] of Byte;
  lvSwapData: array [0 .. 7] of Byte;
  lvAnsiStr: {$IFDEF UNICODE}TBytes{$ELSE}AnsiString{$ENDIF};
  l, I: Cardinal;
  i64: Int64;
  lvObj: TSimpleMsgPack;
begin
  pvStream.Read(lvByte, 1);
  if lvByte in [$00 .. $7F] then // positive fixint	0xxxxxxx	0x00 - 0x7f
  begin
    // +--------+
    // |0XXXXXXX|
    // +--------+
    setAsInteger(lvByte);
  end
  else if lvByte in [$80 .. $8F] then // fixmap	1000xxxx	0x80 - 0x8f
  begin
    FDataType := mptMap;
    SetLength(FValue, 0);
    ClearAndFreeAllChildren;
    l := lvByte - $80;
    if l > 0 then // check is empty ele
    begin
      for I := 0 to l - 1 do
      begin
        lvObj := InnerAdd(mptMap);

        // map key
        lvObj.InnerParseFromStream(pvStream);
        lvObj.setName(lvObj.getAsString);

        // value
        lvObj.InnerParseFromStream(pvStream);
      end;
    end;
  end
  else if lvByte in [$90 .. $9F] then // fixarray	1001xxxx	0x90 - 0x9f
  begin
    FDataType := mptArray;
    SetLength(FValue, 0);
    ClearAndFreeAllChildren;

    l := lvByte - $90;
    if l > 0 then // check is empty ele
    begin
      for I := 0 to l - 1 do
      begin
        lvObj := InnerAdd(mptArray);
        // value
        lvObj.InnerParseFromStream(pvStream);
      end;
    end;
  end
  else if lvByte in [$A0 .. $BF] then // fixstr	101xxxxx	0xa0 - 0xbf
  begin
    l := lvByte - $A0; // str len
    if l > 0 then
    begin

      SetLength(lvAnsiStr, l);
      pvStream.Read(PByte(lvAnsiStr)^, l);
      setAsString(Utf8DecodeEx(lvAnsiStr, l));

      // SetLength(lvBytes, l + 1);
      // lvBytes[l] := 0;
      // pvStream.Read(lvBytes[0], l);
      // setAsString(UTF8Decode(PAnsiChar(@lvBytes[0])));
    end
    else
    begin
      setAsString('');
    end;
  end
  else if lvByte in [$E0 .. $FF] then
  begin
    // negative fixnum stores 5-bit negative integer
    // +--------+
    // |111YYYYY|
    // +--------+
    setAsInteger(Shortint(lvByte));
  end
  else
  begin
    case lvByte of
      $C0: // null
        begin
          FDataType := mptNull;
          SetLength(FValue, 0);
        end;
      $C1: // (never used)
        raise Exception.Create('(never used) type $c1');
      $C2: // False
        begin
          SetAsBoolean(false);
        end;
      $C3: // True
        begin
          SetAsBoolean(True);
        end;
      $C4: // 短二进制，最长255字节
        begin
          FDataType := mptBinary;

          l := 0; // fill zero
          pvStream.Read(l, 1);

          SetLength(FValue, l);
          pvStream.Read(FValue[0], l);
        end;
      $C5: // 二进制，16位，最长65535B
        begin
          FDataType := mptBinary;

          l := 0; // fill zero
          pvStream.Read(l, 2);
          l := swap16(l);

          SetLength(FValue, l);
          pvStream.Read(FValue[0], l);
        end;
      $C6: // 二进制，32位，最长2^32-1
        begin
          FDataType := mptBinary;

          l := 0; // fill zero
          pvStream.Read(l, 4);
          l := swap32(l);

          SetLength(FValue, l);
          pvStream.Read(FValue[0], l);
        end;
      $C7, $C8, $C9:
        // ext 8	11000111	0xc7, ext 16	11001000	0xc8, ext 32	11001001	0xc9
        begin
          raise Exception.Create('(ext8,ext16,ex32) type $c7,$c8,$c9');
        end;
      $CA: // float 32
        begin
          pvStream.Read(lvBData[0], 4);

          swap32Ex(lvBData[0], lvSwapData[0]);

          AsSingle := PSingle(@lvSwapData[0])^;
        end;
      $CB: // Float 64
        begin

          pvStream.Read(lvBData[0], 8);

          // swap to int64, and lvBData is not valid double value (for IEEE)
          i64 := swap64(lvBData[0]);

          //
          AsFloat := PDouble(@i64)^;

          // AsFloat := swap(PDouble(@lvBData[0])^);
        end;
      $CC: // UInt8
        begin
          // uint 8 stores a 8-bit unsigned integer
          // +--------+--------+
          // |  0xcc  |ZZZZZZZZ|
          // +--------+--------+
          l := 0;
          pvStream.Read(l, 1);
          setAsInteger(l);
        end;
      $CD:
        begin
          // uint 16 stores a 16-bit big-endian unsigned integer
          // +--------+--------+--------+
          // |  0xcd  |ZZZZZZZZ|ZZZZZZZZ|
          // +--------+--------+--------+
          l := 0;
          pvStream.Read(l, 2);
          l := swap16(l);
          setAsInteger(Word(l));
        end;
      $CE:
        begin
          // uint 32 stores a 32-bit big-endian unsigned integer
          // +--------+--------+--------+--------+--------+
          // |  0xce  |ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ
          // +--------+--------+--------+--------+--------+
          l := 0;
          pvStream.Read(l, 4);
          l := swap32(l);
          setAsInteger(Cardinal(l));
        end;
      $CF:
        begin
          // uint 64 stores a 64-bit big-endian unsigned integer
          // +--------+--------+--------+--------+--------+--------+--------+--------+--------+
          // |  0xcf  |ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|
          // +--------+--------+--------+--------+--------+--------+--------+--------+--------+
          i64 := 0;
          pvStream.Read(i64, 8);
          i64 := swap64(i64);
          setAsInteger(i64);
        end;
      $DC: // array 16
        begin
          // +--------+--------+--------+~~~~~~~~~~~~~~~~~+
          // |  0xdc  |YYYYYYYY|YYYYYYYY|    N objects    |
          // +--------+--------+--------+~~~~~~~~~~~~~~~~~+
          FDataType := mptArray;
          SetLength(FValue, 0);
          ClearAndFreeAllChildren;

          l := 0; // fill zero
          pvStream.Read(l, 2);

          l := swap16(l);
          if l > 0 then // check is empty ele
          begin
            for I := 0 to l - 1 do
            begin
              lvObj := InnerAdd(mptArray);
              // value
              lvObj.InnerParseFromStream(pvStream);
            end;
          end;
        end;
      $DD: // Array 32
        begin
          // +--------+--------+--------+--------+--------+~~~~~~~~~~~~~~~~~+
          // |  0xdd  |ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|    N objects    |
          // +--------+--------+--------+--------+--------+~~~~~~~~~~~~~~~~~+
          FDataType := mptArray;
          SetLength(FValue, 0);
          ClearAndFreeAllChildren;

          l := 0; // fill zero
          pvStream.Read(l, 4);

          l := swap32(l);
          if l > 0 then // check is empty ele
          begin
            for I := 0 to l - 1 do
            begin
              lvObj := InnerAdd(mptArray);
              // value
              lvObj.InnerParseFromStream(pvStream);
            end;
          end;
        end;
      $D9: // str 8 , 255
        begin
          // str 8 stores a byte array whose length is upto (2^8)-1 bytes:
          // +--------+--------+========+
          // |  0xd9  |YYYYYYYY|  data  |
          // +--------+--------+========+
          l := 0;
          pvStream.Read(l, 1);
          if l > 0 then // check is empty ele
          begin
            SetLength(lvAnsiStr, l);
            pvStream.Read(PByte(lvAnsiStr)^, l);
            setAsString(Utf8DecodeEx(lvAnsiStr, l));
          end
          else
          begin
            setAsString('');
          end;
          // SetLength(lvBytes, l + 1);
          // lvBytes[l] := 0;
          // pvStream.Read(lvBytes[0], l);
          // setAsString(UTF8Decode(PAnsiChar(@lvBytes[0])));
        end;
      $DE: // Object map 16
        begin
          // +--------+--------+--------+~~~~~~~~~~~~~~~~~+
          // |  0xde  |YYYYYYYY|YYYYYYYY|   N*2 objects   |
          // +--------+--------+--------+~~~~~~~~~~~~~~~~~+
          FDataType := mptMap;
          SetLength(FValue, 0);
          ClearAndFreeAllChildren;

          l := 0; // fill zero
          pvStream.Read(l, 2);
          l := swap16(l);
          if l > 0 then // check is empty ele
          begin
            for I := 0 to l - 1 do
            begin
              lvObj := InnerAdd(mptMap);
              // map key
              lvObj.InnerParseFromStream(pvStream);
              lvObj.setName(lvObj.getAsString);

              // value
              lvObj.InnerParseFromStream(pvStream);
            end;
          end;
        end;
      $DF: // Object map 32
        begin
          // +--------+--------+--------+--------+--------+~~~~~~~~~~~~~~~~~+
          // |  0xdf  |ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|   N*2 objects   |
          // +--------+--------+--------+--------+--------+~~~~~~~~~~~~~~~~~+
          FDataType := mptMap;
          SetLength(FValue, 0);
          ClearAndFreeAllChildren;

          l := 0; // fill zero
          pvStream.Read(l, 4);

          l := swap32(l);
          if l > 0 then // check is empty ele
          begin
            for I := 0 to l - 1 do
            begin
              lvObj := InnerAdd(mptMap);

              // map key
              lvObj.InnerParseFromStream(pvStream);
              lvObj.setName(lvObj.getAsString);

              // value
              lvObj.InnerParseFromStream(pvStream);
            end;
          end;
        end;
      $DA: // str 16
        begin
          // str 16 stores a byte array whose length is upto (2^16)-1 bytes:
          // +--------+--------+--------+========+
          // |  0xda  |ZZZZZZZZ|ZZZZZZZZ|  data  |
          // +--------+--------+--------+========+

          l := 0; // fill zero
          pvStream.Read(l, 2);
          l := swap16(l);
          if l > 0 then // check is empty ele
          begin
            SetLength(lvAnsiStr, l);
            pvStream.Read(PByte(lvAnsiStr)^, l);
            setAsString(Utf8DecodeEx(lvAnsiStr, l));
          end
          else
          begin
            setAsString('');
          end;

          // SetLength(lvBytes, l + 1);
          // lvBytes[l] := 0;
          // pvStream.Read(lvBytes[0], l);
          // setAsString(UTF8Decode(PAnsiChar(@lvBytes[0])));
        end;
      $DB: // str 16
        begin
          // str 32 stores a byte array whose length is upto (2^32)-1 bytes:
          // +--------+--------+--------+--------+--------+========+
          // |  0xdb  |AAAAAAAA|AAAAAAAA|AAAAAAAA|AAAAAAAA|  data  |
          // +--------+--------+--------+--------+--------+========+

          l := 0; // fill zero
          pvStream.Read(l, 4);
          l := swap32(l);
          if l > 0 then // check is empty ele
          begin
            SetLength(lvAnsiStr, l);
            pvStream.Read(PByte(lvAnsiStr)^, l);
            setAsString(Utf8DecodeEx(lvAnsiStr, l));
          end
          else
          begin
            setAsString('');
          end;

          // SetLength(lvBytes, l + 1);
          // lvBytes[l] := 0;
          // pvStream.Read(lvBytes[0], l);
          // setAsString(UTF8Decode(PAnsiChar(@lvBytes[0])));
        end;
      $D0: // int 8
        begin
          // int 8 stores a 8-bit signed integer
          // +--------+--------+
          // |  0xd0  |ZZZZZZZZ|
          // +--------+--------+

          l := 0;
          pvStream.Read(l, 1);
          setAsInteger(Shortint(l));
        end;
      $D1:
        begin
          // int 16 stores a 16-bit big-endian signed integer
          // +--------+--------+--------+
          // |  0xd1  |ZZZZZZZZ|ZZZZZZZZ|
          // +--------+--------+--------+

          l := 0;
          pvStream.Read(l, 2);
          l := swap16(l);
          setAsInteger(Smallint(l));
        end;

      $D2:
        begin
          // int 32 stores a 32-bit big-endian signed integer
          // +--------+--------+--------+--------+--------+
          // |  0xd2  |ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|
          // +--------+--------+--------+--------+--------+
          l := 0;
          pvStream.Read(l, 4);
          l := swap32(l);
          setAsInteger(Integer(l));
        end;
      $D3:
        begin
          // int 64 stores a 64-bit big-endian signed integer
          // +--------+--------+--------+--------+--------+--------+--------+--------+--------+
          // |  0xd3  |ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|ZZZZZZZZ|
          // +--------+--------+--------+--------+--------+--------+--------+--------+--------+
          i64 := 0;
          pvStream.Read(i64, 8);
          i64 := swap64(i64);
          setAsInteger(Int64(i64));
        end;
    end;
  end;
end;

procedure TSimpleMsgPack.LoadBinaryFromFile(pvFileName: String);
var
  lvFileStream: TFileStream;
begin
  if FileExists(pvFileName) then
  begin
    lvFileStream := TFileStream.Create(pvFileName, fmOpenRead);
    try
      LoadBinaryFromStream(lvFileStream);
    finally
      lvFileStream.Free;
    end;
  end;
end;

procedure TSimpleMsgPack.LoadBinaryFromStream(pvStream: TStream;
  pvLen: Cardinal = 0);
begin
  FDataType := mptBinary;
  if pvLen = 0 then
  begin
    pvStream.Position := 0;
    SetLength(FValue, pvStream.size);
    pvStream.Read(FValue[0], pvStream.size);
  end
  else
  begin
    SetLength(FValue, pvLen);
    pvStream.ReadBuffer(FValue[0], pvLen);
  end;
end;

function TSimpleMsgPack.Remove(pvPath: string): TSimpleMsgPack;
var
  lvParent: TSimpleMsgPack;
  j: Integer;
begin
  result := InnerFindPathObject(pvPath, lvParent, j);
  if result <> nil then
  begin
    lvParent.FChildren.Delete(j);
  end;
end;

function TSimpleMsgPack.Remove(pvIndex: Integer): TSimpleMsgPack;
begin
  if (pvIndex < 0) or (pvIndex >= Count) then
  begin
    result := nil;
  end
  else
  begin
    result := TSimpleMsgPack(FChildren[pvIndex]);
    FChildren.Delete(pvIndex);
  end;
end;

function TSimpleMsgPack.Remove(pvChild: TSimpleMsgPack): Boolean;
begin
  result := FChildren.Remove(pvChild) <> -1;
end;

procedure TSimpleMsgPack.RemoveFromParent;
begin
  if FParent <> nil then
  begin
    FParent.FChildren.Remove(Self);
  end;
end;

procedure TSimpleMsgPack.SaveBinaryToFile(pvFileName: String);
var
  lvFileStream: TFileStream;
begin
  if FileExists(pvFileName) then
  begin
    if not DeleteFile(PChar(pvFileName)) then
      RaiseLastOSError;
  end;
  lvFileStream := TFileStream.Create(pvFileName, fmCreate);
  try
    lvFileStream.WriteBuffer(FValue[0], Length(FValue));
  finally
    lvFileStream.Free;
  end;
end;

procedure TSimpleMsgPack.SaveBinaryToStream(pvStream: TStream);
begin
  pvStream.WriteBuffer(FValue[0], Length(FValue));
end;

procedure TSimpleMsgPack.SetAsBoolean(const Value: Boolean);
begin
  FDataType := mptBoolean;
  SetLength(FValue, 1);
  PBoolean(@FValue[0])^ := Value;
end;

procedure TSimpleMsgPack.SetAsBytes(const Value: TBytes);
begin
  FDataType := mptBinary;
  FValue := Value;
end;

procedure TSimpleMsgPack.SetAsDateTime(const Value: TDateTime);
begin
  FDataType := mptDateTime;
  SetLength(FValue, SizeOf(TDateTime));
  PDouble(@FValue[0])^ := Value;
end;

procedure TSimpleMsgPack.SetAsFloat(const Value: Double);
begin
  FDataType := mptFloat;
  SetLength(FValue, SizeOf(Double));
  PDouble(@FValue[0])^ := Value;
end;

procedure TSimpleMsgPack.setAsInteger(pvValue: Int64);
begin
  FDataType := mptInteger;
  SetLength(FValue, SizeOf(Int64));
  PInt64(@FValue[0])^ := pvValue;
end;

procedure TSimpleMsgPack.SetAsSingle(const Value: Single);
begin
  FDataType := mptSingle;
  SetLength(FValue, SizeOf(Single));
  PSingle(FValue)^ := Value;
end;

procedure TSimpleMsgPack.setAsString(pvValue: string);
begin
  FDataType := mptString;
  if SizeOf(Char) = 2 then
  begin
    SetLength(FValue, Length(pvValue) shl 1);
    Move(PChar(pvValue)^, FValue[0], Length(FValue));
  end
  else
  begin
    SetLength(FValue, Length(pvValue));
    Move(PChar(pvValue)^, FValue[0], Length(FValue));
  end;
end;

/// <summary>
/// copy from qdac3
/// </summary>
procedure TSimpleMsgPack.SetAsVariant(const Value: Variant);
var
  I: Integer;
  AType: TVarType;
  procedure VarAsBytes;
  var
    l: Integer;
    p: PByte;
  begin
    FDataType := mptBinary;
    l := VarArrayHighBound(Value, 1) + 1;
    SetLength(FValue, l);
    p := VarArrayLock(Value);
    Move(p^, FValue[0], l);
    VarArrayUnlock(Value);
  end;

begin
  if VarIsArray(Value) then
  begin
    AType := VarType(Value);
    if (AType and varTypeMask) = varByte then
      VarAsBytes
    else
    begin
      checkObjectDataType(mptArray);
      ClearAndFreeAllChildren;
      for I := VarArrayLowBound(Value, VarArrayDimCount(Value))
        to VarArrayHighBound(Value, VarArrayDimCount(Value)) do
        Add.AsVariant := Value[I];
    end;
  end
  else
  begin
    case VarType(Value) of
      varSmallInt, varInteger, varByte, varShortInt, varWord,
        varLongWord, varInt64:
        AsInteger := Value;
      varSingle, varDouble, varCurrency:
        AsFloat := Value;
      varDate:
        AsDateTime := Value;
      varOleStr, varString{$IFDEF UNICODE}, varUString{$ENDIF}:
        AsString := Value;
      varBoolean:
        AsBoolean := Value;
      varNull, varEmpty, varUnknown:
        begin
          FDataType := mptNull;
          SetLength(FValue, 0);
        end;
{$IF RtlVersion>=26}
      varUInt64:
        AsInteger := Value;
{$IFEND}
    else
      // null
      ; // raise Exception.Create(SVariantConvertNotSupport);
    end;
  end;
end;

procedure TSimpleMsgPack.SetB(pvPath: String; const Value: Boolean);
var
  lvObj: TSimpleMsgPack;
begin
  lvObj := ForcePathObject(pvPath);
  lvObj.AsBoolean := Value;
end;

procedure TSimpleMsgPack.SetD(pvPath: String; const Value: Double);
var
  lvObj: TSimpleMsgPack;
begin
  lvObj := ForcePathObject(pvPath);
  lvObj.AsFloat := Value;
end;

procedure TSimpleMsgPack.SetI(pvPath: String; const Value: Int64);
var
  lvObj: TSimpleMsgPack;
begin
  lvObj := ForcePathObject(pvPath);
  lvObj.AsInteger := Value;
end;

procedure TSimpleMsgPack.setName(pvName: string);
begin
  FName := pvName;
  FLowerName := LowerCase(FName);
end;

procedure TSimpleMsgPack.SetO(pvPath: String; const Value: TSimpleMsgPack);
var
  lvName: String;
  S: String;
  sPtr: PChar;
  lvTempObj, lvParent: TSimpleMsgPack;
  j: Integer;
begin
  S := pvPath;

  lvParent := Self;
  sPtr := PChar(S);
  while sPtr^ <> #0 do
  begin
    lvName := getFirst(sPtr, ['.', '/', '\']);
    if lvName = '' then
    begin
      break;
    end
    else
    begin
      if sPtr^ = #0 then
      begin // end
        j := lvParent.indexOf(lvName);
        if j <> -1 then
        begin
          lvTempObj := TSimpleMsgPack(lvParent.FChildren[j]);
          lvParent.FChildren[j] := Value;
          lvTempObj.Free; // free old
        end
        else
        begin
          Value.setName(lvName);
          lvParent.InnerAddToChildren(mptMap, Value);
        end;
      end
      else
      begin
        // find childrean
        lvTempObj := lvParent.findObj(lvName);
        if lvTempObj = nil then
        begin
          lvParent := lvParent.Add(lvName);
        end
        else
        begin
          lvParent := lvTempObj;
        end;
      end;
    end;
    if sPtr^ = #0 then
      break;
    Inc(sPtr);
  end;
end;

procedure TSimpleMsgPack.SetS(pvPath: String; const Value: string);
var
  lvObj: TSimpleMsgPack;
begin
  lvObj := ForcePathObject(pvPath);
  lvObj.AsString := Value;
end;

end.
