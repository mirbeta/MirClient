{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressRichEditControl                                   }
{                                                                    }
{           Copyright (c) 2000-2019 Developer Express Inc.           }
{           ALL RIGHTS RESERVED                                      }
{                                                                    }
{   The entire contents of this file is protected by U.S. and        }
{   International Copyright Laws. Unauthorized reproduction,         }
{   reverse-engineering, and distribution of all or any portion of   }
{   the code contained in this file is strictly prohibited and may   }
{   result in severe civil and criminal penalties and will be        }
{   prosecuted to the maximum extent possible under the law.         }
{                                                                    }
{   RESTRICTIONS                                                     }
{                                                                    }
{   THIS SOURCE CODE AND ALL RESULTING INTERMEDIATE FILES            }
{   (DCU, OBJ, DLL, ETC.) ARE CONFIDENTIAL AND PROPRIETARY TRADE     }
{   SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER IS    }
{   LICENSED TO DISTRIBUTE THE EXPRESSRICHEDITCONTROL AND ALL        }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM       }
{   ONLY.                                                            }
{                                                                    }
{   THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED       }
{   FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE         }
{   COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE        }
{   AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT   }
{   AND PERMISSION FROM DEVELOPER EXPRESS INC.                       }
{                                                                    }
{   CONSULT THE END USER LICENSE AGREEMENT FOR INFORMATION ON        }
{   ADDITIONAL RESTRICTIONS.                                         }
{                                                                    }
{********************************************************************}

unit dxRichEdit.Utils.CheckSumStream;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Classes, Generics.Defaults, Generics.Collections;

type
  { TdxCrc32CheckSumCalculator }

  TdxCrc32CheckSumCalculator = class
  protected
    //IdxCheckSumCalculator
    function InitialCheckSumValue: Cardinal;
    function UpdateCheckSum(AValue: Cardinal; const ABuffer; ACount: Integer): Cardinal;
    function GetFinalCheckSum(AValue: Cardinal): Cardinal;
  end;

  { TdxCheckSumStream }

  TdxCheckSumStream = class abstract(TStream)
  strict private
    FStream: TStream;
    FCheckSumCalculator: TdxCrc32CheckSumCalculator;
    FReadCheckSum: Cardinal;
    FWriteCheckSum: Cardinal;
  protected
    function GetSize: Int64; override;
    procedure SetSize(const NewSize: Int64); override;

    function GetReadCheckSum: Cardinal;
    function GetWriteCheckSum: Cardinal;
  public
    constructor Create(AStream: TStream);
    destructor Destroy; override;

    procedure ResetCheckSum;

    procedure ReadToEnd;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    function Write(const Buffer; Count: Longint): Longint; override;

    procedure WriteByte(Value: Byte);

    property Stream: TStream read FStream;
    property ReadCheckSum: Cardinal read GetReadCheckSum;
    property WriteCheckSum: Cardinal read GetWriteCheckSum;
  end;

  { TdxCrc32Stream }

  TdxCrc32Stream = class(TdxCheckSumStream);

implementation

uses
  Math;

type

  { TdxCrc32CheckSum }

  TdxCrc32CheckSum = class
  private
    class var FTable: TArray<Cardinal>;
    class constructor Initialize;
    class destructor Finalize;
  protected
    class property Table: TArray<Cardinal> read FTable;
  public
    class function Update(ACheckSum: Cardinal; const ABuffer; ACount: Integer): Cardinal;
  end;

{ TdxCheckSumStream }

constructor TdxCheckSumStream.Create(AStream: TStream);
begin
  inherited Create;
  FStream := AStream;
  FCheckSumCalculator := TdxCrc32CheckSumCalculator.Create;
  ResetCheckSum;
end;

destructor TdxCheckSumStream.Destroy;
begin
  FreeAndNil(FCheckSumCalculator);
  inherited Destroy;
end;

procedure TdxCheckSumStream.ResetCheckSum;
begin
  FReadCheckSum := FCheckSumCalculator.InitialCheckSumValue;
  FWriteCheckSum := FCheckSumCalculator.InitialCheckSumValue;
end;

function TdxCheckSumStream.Seek(const Offset: Int64;
  Origin: TSeekOrigin): Int64;
begin
  Result := Stream.Seek(Offset, Origin);
end;

procedure TdxCheckSumStream.SetSize(const NewSize: Int64);
begin
  Stream.Size := NewSize;
end;

function TdxCheckSumStream.GetReadCheckSum: Cardinal;
begin
  Result := FCheckSumCalculator.GetFinalCheckSum(FReadCheckSum);
end;

function TdxCheckSumStream.GetSize: Int64;
begin
  Result := Stream.Size;
end;

function TdxCheckSumStream.GetWriteCheckSum: Cardinal;
begin
  Result := FCheckSumCalculator.GetFinalCheckSum(FWriteCheckSum);
end;

procedure TdxCheckSumStream.ReadToEnd;
const
  ABufferSize = 8192;
var
  ABuffer: TArray<Byte>;
  ABytesRead: Integer;
begin
  SetLength(ABuffer, ABufferSize);
  while True do
  begin
    ABytesRead := FStream.Read(ABuffer[0], ABufferSize);
    if ABytesRead < ABufferSize then
      Break;
  end;
end;

function TdxCheckSumStream.Read(var Buffer; Count: Longint): Longint;
var
  ACount: Integer;
begin
  ACount := Count;
  Result := Stream.Read(Buffer, Count);
  FReadCheckSum := FCheckSumCalculator.UpdateCheckSum(FReadCheckSum, Buffer, ACount);
end;

function TdxCheckSumStream.Write(const Buffer; Count: Longint): Longint;
begin
  Result := Stream.Write(Buffer, Count);
  FWriteCheckSum := FCheckSumCalculator.UpdateCheckSum(FWriteCheckSum, Buffer, Result);
end;

procedure TdxCheckSumStream.WriteByte(Value: Byte);
begin
  Write(Value, 1);
end;

{ TdxCrc32CheckSumCalculator }

function TdxCrc32CheckSumCalculator.GetFinalCheckSum(
  AValue: Cardinal): Cardinal;
begin
  Result := AValue xor $FFFFFFFF;
end;

function TdxCrc32CheckSumCalculator.InitialCheckSumValue: Cardinal;
begin
  Result := $FFFFFFFF;
end;

function TdxCrc32CheckSumCalculator.UpdateCheckSum(AValue: Cardinal;
  const ABuffer; ACount: Integer): Cardinal;
begin
  Result := TdxCrc32CheckSum.Update(AValue, ABuffer, ACount);
end;

{ TdxCrc32CheckSum }

class constructor TdxCrc32CheckSum.Initialize;
var
  N, C: Cardinal;
  K: Integer;
begin
  SetLength(FTable, 256);
  for N := 0 to 255 do
  begin
    C := N;
    for K := 0 to 7 do
    begin
      if C and 1 <> 0 then
        C := $EDB88320 xor (C shr 1)
      else
        C := C shr 1;
    end;
    FTable[N] := C;
  end;
end;

class destructor TdxCrc32CheckSum.Finalize;
begin
  SetLength(FTable, 0);
end;

class function TdxCrc32CheckSum.Update(ACheckSum: Cardinal;
  const ABuffer; ACount: Integer): Cardinal;
var
  B: PByte;
begin
  Result := ACheckSum;
  B := @ABuffer;
  while ACount > 0 do
  begin
    Result := (Result shr 8) xor Table[(Result xor B^) and $FF];
    Inc(B);
    Dec(ACount);
  end;
end;

end.
