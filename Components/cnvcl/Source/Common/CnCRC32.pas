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

unit CnCRC32;
{* |<PRE>
================================================================================
* ������ƣ�������������
* ��Ԫ���ƣ�CRC32ѭ������У�鵥Ԫ
* ��Ԫ���ߣ��ܾ��� (zjy@cnpack.org)
* ��    ע��
* ����ƽ̨��PWin2000Pro + Delphi 5.0
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* ��Ԫ��ʶ��$Id: CnCRC32.pas 418 2010-02-08 04:53:54Z zhoujingyu $
* �޸ļ�¼��2009.08.21 V1.3
*               ����CRC64��֧��
*           2009.07.31 V1.2
*               ����������ļ�CRC32����ȷ�����⣬���ӶԴ���4G�ļ���֧��
*           2009.04.16 V1.1
*               ����һ���������������
*           2002.08.11 V1.0
*               ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, SysUtils;

function CRC32Calc(const OrgCRC32: DWORD; const Data; Len: DWORD): DWORD;
{* ����CRC32ֵ
 |<PRE>
   OrgCRC32: DWORD  - ��ʼCRC32ֵ��Ĭ�Ͽɴ� 0
   const Data       - Ҫ��������ݿ�
   Len: DWORD       - ���ݿ鳤��
   Result: DWORD    - ����CRC32������
 |</PRE>}

function StrCRC32(const OrgCRC32: DWORD; const Text: string): DWORD;
{* �����ַ�����CRC32ֵ }

function StrCRC32A(const OrgCRC32: DWORD; const Text: AnsiString): DWORD;
{* ���� AnsiString �ַ�����CRC32ֵ }

function FileCRC32(const FileName: string; var CRC: DWORD; StartPos: Int64 = 0;
  Len: Int64 = 0): Boolean;
{* �����ļ�CRC32ֵ��֧�ֳ���4G�Ĵ��ļ�
 |<PRE>
   const FileName: string   - Ŀ���ļ���
   var CRC: DWORD           - CRC32ֵ����������������ԭʼֵ��Ĭ�Ͽ�Ϊ 0���������ֵ
   StartPos: Int64 = 0      - �ļ���ʼλ�ã�Ĭ�ϴ�ͷ��ʼ
   Len: Int64 = 0           - ���㳤�ȣ�Ϊ��Ĭ��Ϊ�����ļ�
   Result: Boolean          - ���سɹ���־���ļ���ʧ�ܻ�ָ��������Чʱ���� False
 |</PRE>}

function CRC64Calc(const OrgCRC64: Int64; const Data; Len: DWORD): Int64;
{* ����CRC64ֵ
 |<PRE>
   OrgCRC64: Int64  - ��ʼCRC64ֵ��Ĭ�Ͽɴ� 0
   const Data       - Ҫ��������ݿ�
   Len: DWORD       - ���ݿ鳤��
   Result: Int64    - ����CRC64������
 |</PRE>}

function StrCRC64(const OrgCRC64: Int64; const Text: string): Int64;
{* �����ַ�����CRC64ֵ }

function StrCRC64A(const OrgCRC64: Int64; const Text: AnsiString): Int64;
{* ���� AnsiString �ַ�����CRC64ֵ }

function FileCRC64(const FileName: string; var CRC: Int64; StartPos: Int64 = 0;
  Len: Int64 = 0): Boolean;
{* �����ļ�CRC64ֵ��֧�ֳ���4G�Ĵ��ļ�
 |<PRE>
   const FileName: string   - Ŀ���ļ���
   var CRC: Int64           - CRC64ֵ����������������ԭʼֵ��Ĭ�Ͽ�Ϊ 0���������ֵ
   StartPos: Int64 = 0      - �ļ���ʼλ�ã�Ĭ�ϴ�ͷ��ʼ
   Len: Int64 = 0           - ���㳤�ȣ�Ϊ��Ĭ��Ϊ�����ļ�
   Result: Boolean          - ���سɹ���־���ļ���ʧ�ܻ�ָ��������Чʱ���� False
 |</PRE>}

implementation

const
  csBuff_Size = 4096;
  csCRC64 = $C96C5795D7870F42;
  
type
  // �ļ�������
  PBuff = ^TBuff;
  TBuff = array[0..csBuff_Size - 1] of Byte;

  // CRC32��
  TCRC32Table = array[0..255] of DWORD;
  
  // CRC64��
  TCRC64Table = array[0..255] of Int64;
  
var
  CRC32Table: TCRC32Table;
  
  CRC64Table: TCRC64Table;

// ����CRC32��
procedure Make_CRC32Table;
asm
        PUSH    EBX
        MOV     EDX, OFFSET CRC32Table

        XOR     EBX, EBX
@MakeCRC32Loop:
        CMP     EBX, $100
        JE      @MakeCRC32_Succ
        MOV     EAX, EBX
        MOV     ECX, 8
@MakeLoop:
        TEST    EAX, 1
        JZ      @MakeIsZero
        SHR     EAX, 1
        XOR     EAX, $EDB88320
        JMP     @MakeNext
@MakeIsZero:
        SHR     EAX, 1
@MakeNext:
        LOOP    @MakeLoop
        MOV     DWORD PTR [EDX], EAX
        ADD     EDX, 4
        INC     EBX
        JMP     @MakeCRC32Loop

@MakeCRC32_Succ:
        POP     EBX
        RET
end;

// ����CRC32ֵ
function DoCRC32Calc(const OrgCRC32: DWORD; const Data; Len: DWORD): DWORD;
asm
        OR      EDX, EDX   // Data = nil?
        JE      @Exit
        JECXZ   @Exit      // Len = 0?
        PUSH    ESI
        PUSH    EBX
        MOV     ESI, OFFSET CRC32Table
@Upd:
        MOVZX   EBX, AL    // CRC32
        XOR     BL, [EDX]
        SHR     EAX, 8
        AND     EAX, $00FFFFFF
        XOR     EAX, [EBX * 4 + ESI]
        INC     EDX
        LOOP    @Upd
        POP     EBX
        POP     ESI
@Exit:
        RET
end;

// ���� CRC32 ֵ
function CRC32Calc(const OrgCRC32: DWORD; const Data; Len: DWORD): DWORD;
begin
  Result := not OrgCRC32;
  Result := DoCRC32Calc(Result, Data, Len);
  Result := not Result;
end;

// �����ַ�����CRC32ֵ
function StrCRC32(const OrgCRC32: DWORD; const Text: string): DWORD;
begin
  Result := CRC32Calc(OrgCRC32, PChar(Text)^, Length(Text) * SizeOf(Char));
end;

// ���� AnsiString �ַ�����CRC32ֵ
function StrCRC32A(const OrgCRC32: DWORD; const Text: AnsiString): DWORD;
begin
  Result := CRC32Calc(OrgCRC32, PAnsiChar(Text)^, Length(Text));
end;

// �����ļ�CRCֵ�������ֱ�Ϊ���ļ�����CRCֵ����ʼ��ַ�����㳤��
function FileCRC32(const FileName: string; var CRC: DWORD; StartPos: Int64 = 0;
  Len: Int64 = 0): Boolean;
var
  Handle: THandle;
  ReadCount: Integer;
  Size: Int64;
  Count: Int64;
  Buff: TBuff;
begin
  // �Թ������ʽ���ļ�
  Handle := CreateFile(PChar(FileName), GENERIC_READ,
    FILE_SHARE_READ, nil, OPEN_EXISTING,
    FILE_ATTRIBUTE_NORMAL, 0);
  Result := Handle <> INVALID_HANDLE_VALUE;
  if Result then
  try
    Int64Rec(Size).Lo := GetFileSize(Handle, @Int64Rec(Size).Hi);
    if Size < StartPos + Len then
    begin
      Result := False;                  // �����ļ�����
      Exit;
    end;
    if Len > 0 then
      Count := Len
    else
      Count := Size - StartPos;         // ����Ϊ�㣬���㵽�ļ�β

    CRC := not CRC;
    SetFilePointer(Handle, Int64Rec(StartPos).Lo, @Int64Rec(StartPos).Hi, FILE_BEGIN);
    while Count > 0 do
    begin
      if Count > SizeOf(Buff) then
        ReadCount := SizeOf(Buff)
      else
        ReadCount := Count;
      ReadFile(Handle, Buff, ReadCount, LongWord(ReadCount), nil);
      CRC := DoCrc32Calc(CRC, Buff, ReadCount);
      Dec(Count, ReadCount);
    end;
    CRC := not CRC;
  finally
    CloseHandle(Handle);
  end;
end;

procedure Make_CRC64Table;
var
  I, J: Integer;
  Data: Int64;
begin
  for I := 0 to 255 do
  begin
    Data := I;
    for J := 0 to 7 do
    begin
      if (Data and 1) <> 0 then
        Data := Data shr 1 xor csCRC64
      else
        Data := Data shr 1;
      
      CRC64Table[I] := Data;   
    end;
  end;
end;

function DoCRC64Calc(const OrgCRC64: Int64; const Data; Len: DWORD): Int64;
var
  I: Integer;
  DataAddr: PByte;
begin
  DataAddr := @Data;
  Result := OrgCRC64;
  
  for I := 0 to Len - 1 do
  begin
    Result := Result shr 8 xor 
      CRC64Table[Cardinal(Result) and $FF xor DataAddr^]; 
    Inc(DataAddr);   
  end;
end;

// ���� CRC64 ֵ
function CRC64Calc(const OrgCRC64: Int64; const Data; Len: DWORD): Int64;
begin
  Result := not OrgCRC64;
  Result := DoCRC64Calc(Result, Data, Len);
  Result := not Result;
end;

// �����ַ�����CRC32ֵ
function StrCRC64(const OrgCRC64: Int64; const Text: string): Int64;
begin
  Result := CRC64Calc(OrgCRC64, PChar(Text)^, Length(Text) * SizeOf(Char));
end;

// ���� AnsiString �ַ�����CRC32ֵ
function StrCRC64A(const OrgCRC64: Int64; const Text: AnsiString): Int64;
begin
  Result := CRC64Calc(OrgCRC64, PAnsiChar(Text)^, Length(Text));
end;

// �����ļ�CRCֵ�������ֱ�Ϊ���ļ�����CRCֵ����ʼ��ַ�����㳤��
function FileCRC64(const FileName: string; var CRC: Int64; StartPos: Int64 = 0;
  Len: Int64 = 0): Boolean;
var
  Handle: THandle;
  ReadCount: Integer;
  Size: Int64;
  Count: Int64;
  Buff: TBuff;
begin
  // �Թ������ʽ���ļ�
  Handle := CreateFile(PChar(FileName), GENERIC_READ,
    FILE_SHARE_READ, nil, OPEN_EXISTING,
    FILE_ATTRIBUTE_NORMAL, 0);
  Result := Handle <> INVALID_HANDLE_VALUE;
  if Result then
  try
    Int64Rec(Size).Lo := GetFileSize(Handle, @Int64Rec(Size).Hi);
    if Size < StartPos + Len then
    begin
      Result := False;                  // �����ļ�����
      Exit;
    end;
    if Len > 0 then
      Count := Len
    else
      Count := Size - StartPos;         // ����Ϊ�㣬���㵽�ļ�β

    CRC := not CRC;
    SetFilePointer(Handle, Int64Rec(StartPos).Lo, @Int64Rec(StartPos).Hi, FILE_BEGIN);
    while Count > 0 do
    begin
      if Count > SizeOf(Buff) then
        ReadCount := SizeOf(Buff)
      else
        ReadCount := Count;
      ReadFile(Handle, Buff, ReadCount, LongWord(ReadCount), nil);
      CRC := DoCrc64Calc(CRC, Buff, ReadCount);
      Dec(Count, ReadCount);
    end;
    CRC := not CRC;
  finally
    CloseHandle(Handle);
  end;
end;

initialization
  Make_CRC32Table; // ��ʼ��CRC32��
  
  Make_CRC64Table; // ��ʼ��CRC64��

end.

