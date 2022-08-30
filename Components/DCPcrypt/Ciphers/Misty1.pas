{******************************************************************************}
{* DCPcrypt v2.0 written by David Barton (davebarton@bigfoot.com) *************}
{******************************************************************************}
{* A binary compatible implementation of Misty1 *******************************}
{******************************************************************************}
{* Copyright (c) 1999-2000 David Barton                                       *}
{* Permission is hereby granted, free of charge, to any person obtaining a    *}
{* copy of this software and associated documentation files (the "Software"), *}
{* to deal in the Software without restriction, including without limitation  *}
{* the rights to use, copy, modify, merge, publish, distribute, sublicense,   *}
{* and/or sell copies of the Software, and to permit persons to whom the      *}
{* Software is furnished to do so, subject to the following conditions:       *}
{*                                                                            *}
{* The above copyright notice and this permission notice shall be included in *}
{* all copies or substantial portions of the Software.                        *}
{*                                                                            *}
{* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *}
{* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *}
{* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *}
{* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *}
{* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *}
{* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *}
{* DEALINGS IN THE SOFTWARE.                                                  *}
{******************************************************************************}
unit Misty1;

interface
uses
  Classes, Sysutils, DCPcrypt, DCPconst;

const
  NUMROUNDS                 = 8;  

type
  TDCP_misty1 = class(TDCP_blockcipher)
  protected
    KeyData: array[0..31] of DWord;
    function FI(const FI_IN, FI_KEY: DWord): DWord;
    function FO(const FO_IN: DWord; const k: longint): DWord;
    function FL(const FL_IN: DWord; const k: longint): DWord;
    function FLINV(const FL_IN: DWord; const k: longint): DWord;
  public
    class function GetID: longint; override;
    class function GetAlgorithm: string; override;
    class function GetMaxKeySize: longint; override;
    class function GetBlockSize: longint; override;
    class function SelfTest: boolean; override;
    procedure Init(const Key; Size: longint; InitVector: pointer); override;
    procedure Burn; override;
    procedure EncryptECB(const InData; var OutData); override;
    procedure DecryptECB(const InData; var OutData); override;
  end;

  {******************************************************************************}
  {******************************************************************************}
implementation
{$R-}{$Q-}
{$I Misty1.inc}

class function TDCP_misty1.GetID: longint;
begin
  Result := DCP_misty1;
end;

class function TDCP_misty1.GetAlgorithm: string;
begin
  Result := 'Misty1';
end;

class function TDCP_misty1.GetMaxKeySize: longint;
begin
  Result := 128;
end;

class function TDCP_misty1.GetBlockSize: longint;
begin
  Result := 64;
end;

class function TDCP_misty1.SelfTest: boolean;
const
  Key                       : array[0..15] of byte =
    ($00, $11, $22, $33, $44, $55, $66, $77, $88, $99, $AA, $BB, $CC, $DD, $EE, $FF);
  Plain1                    : array[0..7] of byte = ($01, $23, $45, $67, $89, $AB, $CD, $EF);
  Plain2                    : array[0..7] of byte = ($FE, $DC, $BA, $98, $76, $54, $32, $10);
  Cipher1                   : array[0..7] of byte = ($8B, $1D, $A5, $F5, $6A, $B3, $D0, $7C);
  Cipher2                   : array[0..7] of byte = ($04, $B6, $82, $40, $B1, $3B, $E9, $5D);
var
  Cipher                    : TDCP_misty1;
  Block                     : array[0..7] of byte;
begin
  Cipher := TDCP_misty1.Create(nil);
  Cipher.Init(Key, SizeOf(Key) * 8, nil);
  Cipher.EncryptECB(Plain1, Block);
  Result := CompareMemory(@Cipher1, @Block, SizeOf(Block));
  Cipher.DecryptECB(Block, Block);
  Result := Result and CompareMemory(@Plain1, @Block, SizeOf(Block));
  Cipher.EncryptECB(Plain2, Block);
  Result := Result and CompareMemory(@Cipher2, @Block, SizeOf(Block));
  Cipher.DecryptECB(Block, Block);
  Result := Result and CompareMemory(@Plain2, @Block, SizeOf(Block));
  Cipher.Burn;
  Cipher.Free;
end;

function TDCP_misty1.FI(const FI_IN, FI_KEY: DWord): DWord;
var
  d7, d9                    : DWord;
begin
  d9 := (FI_IN shr 7) and $1FF;
  d7 := FI_IN and $7F;
  d9 := S9Table[d9] xor d7;
  d7 := (S7Table[d7] xor d9) and $7F;
  d7 := d7 xor ((FI_KEY shr 9) and $7F);
  d9 := d9 xor (FI_KEY and $1FF);
  d9 := S9Table[d9] xor d7;
  Result := (d7 shl 9) or d9;
end;

function TDCP_misty1.FO(const FO_IN: DWord; const k: longint): DWord;
var
  t0, t1                    : DWord;
begin
  t0 := FO_IN shr 16;
  t1 := FO_IN and $FFFF;
  t0 := t0 xor KeyData[k];
  t0 := FI(t0, KeyData[((k + 5) mod 8) + 8]);
  t0 := t0 xor t1;
  t1 := t1 xor KeyData[(k + 2) mod 8];
  t1 := FI(t1, KeyData[((k + 1) mod 8) + 8]);
  t1 := t1 xor t0;
  t0 := t0 xor KeyData[(k + 7) mod 8];
  t0 := FI(t0, KeyData[((k + 3) mod 8) + 8]);
  t0 := t0 xor t1;
  t1 := t1 xor KeyData[(k + 4) mod 8];
  Result := (t1 shl 16) or t0;
end;

function TDCP_misty1.FL(const FL_IN: DWord; const k: longint): DWord;
var
  d0, d1                    : DWord;
  t                         : byte;
begin
  d0 := FL_IN shr 16;
  d1 := FL_IN and $FFFF;
  if (k mod 2) <> 0 then
  begin
    t := (k - 1) div 2;
    d1 := d1 xor (d0 and KeyData[((t + 2) mod 8) + 8]);
    d0 := d0 xor (d1 or KeyData[(t + 4) mod 8]);
  end
  else
  begin
    t := k div 2;
    d1 := d1 xor (d0 and KeyData[t]);
    d0 := d0 xor (d1 or KeyData[((t + 6) mod 8) + 8]);
  end;
  Result := (d0 shl 16) or d1;
end;

function TDCP_misty1.FLINV(const FL_IN: DWord; const k: longint): DWord;
var
  d0, d1                    : DWord;
  t                         : byte;
begin
  d0 := FL_IN shr 16;
  d1 := FL_IN and $FFFF;
  if (k mod 2) <> 0 then
  begin
    t := (k - 1) div 2;
    d0 := d0 xor (d1 or KeyData[(t + 4) mod 8]);
    d1 := d1 xor (d0 and KeyData[((t + 2) mod 8) + 8]);
  end
  else
  begin
    t := k div 2;
    d0 := d0 xor (d1 or KeyData[((t + 6) mod 8) + 8]);
    d1 := d1 xor (d0 and KeyData[t]);
  end;
  Result := (d0 shl 16) or d1;
end;

procedure TDCP_misty1.Init(const Key; Size: longint; InitVector: pointer);
var
  KeyB                      : array[0..15] of byte;
  i                         : longint;
begin
  inherited Init(Key, Size, InitVector);

  FillChar(KeyB, SizeOf(KeyB), 0);
  Move(Key, KeyB, Size div 8);
  for i := 0 to 7 do
    KeyData[i] := (KeyB[i * 2] * 256) + KeyB[i * 2 + 1];
  for i := 0 to 7 do
  begin
    KeyData[i + 8] := FI(KeyData[i], KeyData[(i + 1) mod 8]);
    KeyData[i + 16] := KeyData[i + 8] and $1FF;
    KeyData[i + 24] := KeyData[i + 8] shr 9;
  end;

  { Generate a "random" IV }
  if InitVector = nil then
  begin
    FillChar(IV^, BS, 0);
    EncryptECB(IV^, IV^);
    Reset;
  end
  else
  begin
    Move(InitVector^, IV^, BS);
    Reset;
  end;
end;

procedure TDCP_misty1.Burn;
begin
  FillChar(KeyData, SizeOf(KeyData), 0);
  inherited Burn;
end;

procedure TDCP_misty1.EncryptECB(const InData; var OutData);
var
  d0, d1                    : DWord;
  i                         : longint;
begin
  if not fInitialized then
    raise EDCP_blockcipher.Create('Cipher not initialized');
  d0 := SwapDWord(PDWord(@InData)^);
  d1 := SwapDWord(PDWord(longint(@InData) + 4)^);
  for i := 0 to NUMROUNDS - 1 do
  begin
    if (i mod 2) = 0 then
    begin
      d0 := FL(d0, i);
      d1 := FL(d1, i + 1);
      d1 := d1 xor FO(d0, i);
    end
    else
      d0 := d0 xor FO(d1, i);
  end;
  d0 := FL(d0, NUMROUNDS);
  d1 := FL(d1, NUMROUNDS + 1);
  PDWord(@OutData)^ := SwapDWord(d1);
  PDWord(longint(@OutData) + 4)^ := SwapDWord(d0);
end;

procedure TDCP_misty1.DecryptECB(const InData; var OutData);
var
  d0, d1                    : DWord;
  i                         : longint;
begin
  if not fInitialized then
    raise EDCP_blockcipher.Create('Cipher not initialized');
  d1 := SwapDWord(PDWord(@InData)^);
  d0 := SwapDWord(PDWord(longint(@InData) + 4)^);
  d1 := FLINV(d1, NUMROUNDS + 1);
  d0 := FLINV(d0, NUMROUNDS);
  for i := NUMROUNDS - 1 downto 0 do
  begin
    if (i mod 2) = 0 then
    begin
      d1 := d1 xor FO(d0, i);
      d0 := FLINV(d0, i);
      d1 := FLINV(d1, i + 1);
    end
    else
      d0 := d0 xor FO(d1, i);
  end;
  PDWord(@OutData)^ := SwapDWord(d0);
  PDWord(longint(@OutData) + 4)^ := SwapDWord(d1);
end;

initialization
  RegisterClass(TDCP_misty1);
  DCPregcipher(TDCP_misty1, true);

end.

