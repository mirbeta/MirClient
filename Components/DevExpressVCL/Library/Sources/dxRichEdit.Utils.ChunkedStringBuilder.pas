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

unit dxRichEdit.Utils.ChunkedStringBuilder;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Math,
  Classes, SysUtils, Generics.Defaults, Generics.Collections, Contnrs,
  dxGenerics;

type

  { TdxChunkedStringBuilder }

  TdxChunkedStringBuilder = class
  protected const
    DefaultMaxBufferSize = 32 * 1024;
  protected
    type
      { TChunk }

      TChunk = class
      strict private
        FCapacity: Integer;
        FBuffer: TCharArray;
        FLength: Integer;
        function GetChar(AIndex: Integer): Char; inline;
        procedure SetCapacity(ANewCapacity: Integer); inline;
        procedure SetChar(AIndex: Integer; const AValue: Char); inline;
      public
        constructor Create(ACapacity: Integer = 0);
        destructor Destroy; override;
        procedure Append(const ACharacter: Char); overload; inline;
        procedure Append(AChars: Pointer; ACount: Integer); overload; inline;
        procedure Clear;
        function ToString(AStartIndex, ALength: Integer): string; reintroduce; overload;
        function ToString: string; reintroduce; overload;

        property Capacity: Integer read FCapacity write SetCapacity;
        property Chars[Index: Integer]: Char read GetChar write SetChar; default;
        property Length: Integer read FLength;
      end;
  strict private
    FMaxChunkSize: Integer;
    FChunks: TdxObjectList<TChunk>;
    FTotalLength: Integer;
    procedure ClearNotOwnedChunks;
    function GetChar(AIndex: Integer): Char; inline;
    procedure SetChar(AIndex: Integer; const AValue: Char); inline;
  protected
    procedure Initialize;
    property MaxChunkSize: Integer read FMaxChunkSize write FMaxChunkSize;
    property Chunks: TdxObjectList<TChunk> read FChunks;
  public
    constructor Create; overload;
    constructor Create(const AValue: string); overload;
    destructor Destroy; override;
    function Append(const AValue: string): TdxChunkedStringBuilder; overload;
    function Append(const AValue: TCharArray; AStartIndex, ACount: Integer): TdxChunkedStringBuilder; overload;
    function Append(const AValue: string; AStartIndex, ACount: Integer): TdxChunkedStringBuilder; overload;
    function Append(AValue: Char): TdxChunkedStringBuilder; overload;
    function Append(AValue: Byte): TdxChunkedStringBuilder; overload;
    function Append(AValue: Double): TdxChunkedStringBuilder; overload;
    function Append(AValue: Integer): TdxChunkedStringBuilder; overload;
    function Append(AValue: Int64): TdxChunkedStringBuilder; overload;
    function Append(AValue: Single): TdxChunkedStringBuilder; overload;
    function AppendLine: TdxChunkedStringBuilder; overload;
    function AppendLine(const AValue: string): TdxChunkedStringBuilder; overload;
    function GetBytes(AEncoding: TEncoding): TBytes;
    function ToString(AStartIndex, ALength: Integer): string; reintroduce; overload;
    function ToString: string; reintroduce; overload;
    procedure Clear;
    procedure AppendExistingBuffersUnsafe(AStringBuilder: TdxChunkedStringBuilder);

    property Chars[Index: Integer]: Char read GetChar write SetChar; default;
    property Length: Integer read FTotalLength;
  end;

  { TdxChunkedStringBuilderWriter }

  TdxChunkedStringBuilderWriter = class(TTextWriter)
  strict private
    FStringBuilder: TdxChunkedStringBuilder;
    FEncoding: TEncoding;
    FIsOpen: Boolean;
  public
    constructor Create(AStringBuilder: TdxChunkedStringBuilder);
    procedure Close; override;
    function GetStringBuilder: TdxChunkedStringBuilder; virtual;
    function ToString: string; override;
    procedure Write(AValue: Char); override;
    procedure Write(const AValue: string); override;
    procedure Write(const ABuffer: TArray<Char>; AIndex: Integer; ACount: Integer); override;
    procedure ThrowWriterClosedException;
    procedure Flush; override;
    procedure WriteLine; overload; override;
    procedure WriteLine(Value: Boolean); overload; override;
    procedure WriteLine(Value: Char); overload; override;
    procedure WriteLine(const Value: TCharArray); overload; override;
    procedure WriteLine(Value: Double); overload; override;
    procedure WriteLine(Value: Integer); overload; override;
    procedure WriteLine(Value: Int64); overload; override;
    procedure WriteLine(Value: TObject); overload; override;
    procedure WriteLine(Value: Single); overload; override;
    procedure WriteLine(const Value: string); overload; override;
    procedure WriteLine(Value: Cardinal); overload; override;
    procedure WriteLine(Value: UInt64); overload; override;
    procedure WriteLine(const Format: string; Args: array of const); overload; override;
    procedure WriteLine(const Value: TCharArray; Index, Count: Integer); overload; override;

    property Encoding: TEncoding read FEncoding write FEncoding;
  end;

implementation

uses
  dxRichEdit.Utils.Exceptions;

{ TChunk }

procedure TdxChunkedStringBuilder.TChunk.SetCapacity(ANewCapacity: Integer);
begin
  if ANewCapacity <> FCapacity then
  begin
    FCapacity := ANewCapacity;
    SetLength(FBuffer, FCapacity);
  end;
end;

constructor TdxChunkedStringBuilder.TChunk.Create(ACapacity: Integer);
begin
  inherited Create;
  Assert(ACapacity > 0);
  Capacity := ACapacity
end;

destructor TdxChunkedStringBuilder.TChunk.Destroy;
begin
  Capacity := 0;
  inherited Destroy;
end;

procedure TdxChunkedStringBuilder.TChunk.Append(const ACharacter: Char);
begin
  Assert(FLength < Capacity, 'Buffer overflow');
  FBuffer[FLength] := ACharacter;
  Inc(FLength);
end;

procedure TdxChunkedStringBuilder.TChunk.Append(AChars: Pointer; ACount: Integer);
begin
  Assert(ACount > 0, 'Bad ACount parameter: ACount <= 0');
  Assert(FLength + ACount <= Capacity, 'Buffer overflow');
  Move(AChars^, FBuffer[FLength], SizeOf(Char) * ACount);
  Inc(FLength, ACount);
end;

procedure TdxChunkedStringBuilder.TChunk.Clear;
begin
  FLength := 0;
end;

function TdxChunkedStringBuilder.TChunk.GetChar(AIndex: Integer): Char;
begin
  Assert((AIndex >= 0) and (AIndex < FLength), Format('Bad Index parameter: GetChar(Index = %d) where Self.Length = %d', [AIndex, FLength]));
  Result := FBuffer[AIndex];
end;

procedure TdxChunkedStringBuilder.TChunk.SetChar(AIndex: Integer; const AValue: Char);
begin
  Assert((AIndex >= 0) and (AIndex < FLength), Format('Bad Index parameter: SetChar(Index = %d) where Self.Length = %d', [AIndex, FLength]));
  FBuffer[AIndex] := AValue;
end;

function TdxChunkedStringBuilder.TChunk.ToString(AStartIndex, ALength: Integer): string;
begin
  Assert((AStartIndex >= 0) and (AStartIndex < FLength), Format('Bad StartIndex parameter: ToString(StartIndex = %d) where Self.Length = %d', [AStartIndex, FLength]));
  Assert(AStartIndex + ALength <= FLength, Format('Bad Length parameter: ToString(StartIndex = %d, Length = %d) where Self.Length = %d', [AStartIndex, ALength, FLength]));
  SetString(Result, PChar(@FBuffer[AStartIndex]), ALength);
end;

function TdxChunkedStringBuilder.TChunk.ToString: string;
begin
  SetString(Result, PChar(FBuffer), FLength);
end;

{ TdxChunkedStringBuilder }

constructor TdxChunkedStringBuilder.Create;
begin
  FMaxChunkSize := DefaultMaxBufferSize;
  FChunks := TdxObjectList<TChunk>.Create;
  Initialize;
end;

constructor TdxChunkedStringBuilder.Create(const AValue: string);
begin
  Create;
  Append(AValue);
end;

destructor TdxChunkedStringBuilder.Destroy;
begin
  FChunks.Free;
  inherited Destroy;
end;

function TdxChunkedStringBuilder.GetChar(AIndex: Integer): Char;
var
  AChunkIndex, AChunkOffset: Integer;
begin
  AChunkIndex := AIndex div MaxChunkSize;
  Assert((AIndex >= 0) and (AChunkIndex < FChunks.Count));
  AChunkOffset := AIndex mod MaxChunkSize;
  Result := FChunks[AChunkIndex].Chars[AChunkOffset];
end;

procedure TdxChunkedStringBuilder.SetChar(AIndex: Integer; const AValue: Char);
var
  AChunkIndex, AChunkOffset: Integer;
begin
  AChunkIndex := AIndex div MaxChunkSize;
  Assert((AIndex >= 0) and (AChunkIndex < FChunks.Count));
  AChunkOffset := AIndex mod MaxChunkSize;
  FChunks[AChunkIndex].Chars[AChunkOffset] := AValue;
end;

procedure TdxChunkedStringBuilder.Initialize;
var
  AChunkCount: Integer;
begin
  AChunkCount := FChunks.Count;
  if AChunkCount <= 0 then
    FChunks.Add(TChunk.Create(MaxChunkSize))
  else
  begin
    if AChunkCount > 1 then
      FChunks.DeleteRange(1, AChunkCount - 1);
    FChunks[0].Clear;
  end;
  FTotalLength := 0;
end;

function TdxChunkedStringBuilder.Append(const AValue: string): TdxChunkedStringBuilder;
begin
  if AValue = '' then
    Exit(Self);
  Result := Append(AValue, 0, System.Length(AValue));
end;

function TdxChunkedStringBuilder.Append(const AValue: TCharArray; AStartIndex, ACount: Integer): TdxChunkedStringBuilder;
var
  AChunk: TChunk;
  ASpace, AIndex, AEndIndex, ALength: Integer;
begin
  if ACount <= 0 then
    Exit(Self);

  AChunk := FChunks[FChunks.Count - 1];
  ASpace := MaxChunkSize - AChunk.Length;

  if ASpace >= ACount then
    AChunk.Append(@AValue[AStartIndex], ACount)
  else
  begin
    AIndex := AStartIndex;
    if ASpace > 0 then
    begin
      AChunk.Append(@AValue[AStartIndex], ASpace);
      Inc(AIndex, ASpace);
    end;
    AEndIndex := AStartIndex + ACount;
    while AIndex < AEndIndex do
    begin
      ALength := Min(AEndIndex - AIndex, MaxChunkSize);
      AChunk := TChunk.Create(MaxChunkSize);
      AChunk.Append(@AValue[AIndex], ALength);
      FChunks.Add(AChunk);

      Inc(AIndex, ALength);
    end;
  end;
  FTotalLength := FTotalLength + ACount;
  Result := Self;
end;

function TdxChunkedStringBuilder.Append(const AValue: string; AStartIndex, ACount: Integer): TdxChunkedStringBuilder;
var
  AChunk: TChunk;
  ASpace, AIndex, AEndIndex, ALength: Integer;
begin
  if ACount <= 0 then
    Exit(Self);

  AChunk := FChunks[FChunks.Count - 1];
  ASpace := MaxChunkSize - AChunk.Length;

  if ASpace >= ACount then
    AChunk.Append(@AValue[AStartIndex + {$IFDEF DELPHIXE3}Low(string){$ELSE}1{$ENDIF}], ACount)
  else
  begin
    AIndex := AStartIndex;
    if ASpace > 0 then
    begin
      AChunk.Append(@AValue[AStartIndex + {$IFDEF DELPHIXE3}Low(string){$ELSE}1{$ENDIF}], ASpace);
      Inc(AIndex, ASpace);
    end;
    AEndIndex := AStartIndex + ACount;
    while AIndex < AEndIndex do
    begin
      ALength := Min(AEndIndex - AIndex, MaxChunkSize);
      AChunk := TChunk.Create(MaxChunkSize);
      AChunk.Append(@AValue[AIndex + {$IFDEF DELPHIXE3}Low(string){$ELSE}1{$ENDIF}], ALength);
      FChunks.Add(AChunk);
      Inc(AIndex, ALength);
    end;
  end;
  FTotalLength := FTotalLength + ACount;
  Result := Self;
end;

function TdxChunkedStringBuilder.Append(AValue: Char): TdxChunkedStringBuilder;
var
  AChunk: TChunk;
  ASpace: Integer;
begin
  AChunk := FChunks[FChunks.Count - 1];
  ASpace := MaxChunkSize - AChunk.Length;
  if ASpace >= 1 then
    AChunk.Append(AValue)
  else
  begin
    AChunk := TChunk.Create(MaxChunkSize);
    AChunk.Append(AValue);
    FChunks.Add(AChunk);
  end;
  Inc(FTotalLength);
  Result := Self;
end;

function TdxChunkedStringBuilder.Append(AValue: Byte): TdxChunkedStringBuilder;
begin
  Result := Append(IntToStr(AValue));
end;

function TdxChunkedStringBuilder.Append(AValue: Double): TdxChunkedStringBuilder;
begin
  Result := Append(FloatToStr(AValue));
end;

function TdxChunkedStringBuilder.Append(AValue: Integer): TdxChunkedStringBuilder;
begin
  Result := Append(IntToStr(AValue));
end;

function TdxChunkedStringBuilder.Append(AValue: Int64): TdxChunkedStringBuilder;
begin
  Result := Append(IntToStr(AValue));
end;

function TdxChunkedStringBuilder.Append(AValue: Single): TdxChunkedStringBuilder;
begin
  Result := Append(FloatToStr(AValue));
end;

function TdxChunkedStringBuilder.AppendLine: TdxChunkedStringBuilder;
begin
  Result := Append(sLineBreak);
end;

function TdxChunkedStringBuilder.AppendLine(const AValue: string): TdxChunkedStringBuilder;
begin
  Append(AValue);
  Result := Append(sLineBreak);
end;

function TdxChunkedStringBuilder.GetBytes(AEncoding: TEncoding): TBytes;
begin
  Result := AEncoding.GetBytes(ToString);
end;

function TdxChunkedStringBuilder.ToString: string;
var
  AStringBuilder: TStringBuilder;
  ACount, I: Integer;
begin
  AStringBuilder := TStringBuilder.Create(Length);
  try
    ACount := FChunks.Count;
    for I := 0 to ACount - 1 do
      AStringBuilder.Append(FChunks[I].ToString);
    Result := AStringBuilder.ToString;
  finally
    AStringBuilder.Free;
  end;
end;

function TdxChunkedStringBuilder.ToString(AStartIndex: Integer; ALength: Integer): string;
var
  AFirstBufferIndex, AFirstBufferOffset, ALastBufferIndex, I, AEndIndex: Integer;
  AStringBuilder: TStringBuilder;
begin
  AFirstBufferIndex := AStartIndex div MaxChunkSize;
  AFirstBufferOffset := AStartIndex mod MaxChunkSize;
  ALastBufferIndex := (AStartIndex + ALength - 1) div MaxChunkSize;
  if AFirstBufferIndex = ALastBufferIndex then
    Exit(FChunks[AFirstBufferIndex].ToString(AFirstBufferOffset, ALength));

  AStringBuilder := TStringBuilder.Create(ALength);
  try
    AStringBuilder.Append(FChunks[AFirstBufferIndex].ToString(AFirstBufferOffset, MaxChunkSize - AFirstBufferOffset));
    for I := AFirstBufferIndex + 1 to ALastBufferIndex - 1 do
      AStringBuilder.Append(FChunks[I].ToString);

    AEndIndex := AStartIndex + ALength;
    AStringBuilder.Append(FChunks[ALastBufferIndex].ToString(0, AEndIndex - ALastBufferIndex * MaxChunkSize));

    Result := AStringBuilder.ToString;
  finally
    AStringBuilder.Free;
  end;
end;

procedure TdxChunkedStringBuilder.Clear;
begin
  Initialize;
end;

procedure TdxChunkedStringBuilder.AppendExistingBuffersUnsafe(AStringBuilder: TdxChunkedStringBuilder);
begin
  if (AStringBuilder = nil) or (AStringBuilder.Length <= 0) then
    Exit;

  if MaxChunkSize <> AStringBuilder.MaxChunkSize then
    raise EArgumentException.CreateFmt('stringBuilder.MaxBufferSize = %d', [AStringBuilder.MaxChunkSize]);

  Chunks.AddRange(AStringBuilder.Chunks);
  Inc(FTotalLength, AStringBuilder.Length);
  AStringBuilder.ClearNotOwnedChunks;
end;

procedure TdxChunkedStringBuilder.ClearNotOwnedChunks;
begin
  Chunks.OwnsObjects := False;
  Chunks.Clear;
  Chunks.OwnsObjects := True;
  Initialize;
end;

{ TdxChunkedStringBuilderWriter }

constructor TdxChunkedStringBuilderWriter.Create(AStringBuilder: TdxChunkedStringBuilder);
begin
  FEncoding := TEncoding.Unicode;
  FIsOpen := True;
  FStringBuilder := AStringBuilder;
end;

procedure TdxChunkedStringBuilderWriter.Flush;
begin
end;

procedure TdxChunkedStringBuilderWriter.Close;
begin
  FIsOpen := False;
end;

function TdxChunkedStringBuilderWriter.GetStringBuilder: TdxChunkedStringBuilder;
begin
  Result := FStringBuilder;
end;

function TdxChunkedStringBuilderWriter.ToString: string;
begin
  Result := FStringBuilder.ToString;
end;

procedure TdxChunkedStringBuilderWriter.Write(AValue: Char);
begin
  if FIsOpen then
    FStringBuilder.Append(AValue)
  else
    ThrowWriterClosedException;
end;

procedure TdxChunkedStringBuilderWriter.Write(const AValue: string);
begin
  if FIsOpen then
    FStringBuilder.Append(AValue)
  else
    ThrowWriterClosedException;
end;

procedure TdxChunkedStringBuilderWriter.Write(const ABuffer: TArray<Char>; AIndex: Integer; ACount: Integer);
begin
  if FIsOpen then
    FStringBuilder.Append(ABuffer, AIndex, ACount)
  else
    ThrowWriterClosedException;
end;

procedure TdxChunkedStringBuilderWriter.WriteLine(Value: Double);
begin
  if FIsOpen then
  begin
    FStringBuilder.Append(Value);
    FStringBuilder.AppendLine;
  end
  else
    ThrowWriterClosedException;
end;

procedure TdxChunkedStringBuilderWriter.WriteLine(Value: Integer);
begin
  if FIsOpen then
  begin
    FStringBuilder.Append(Value);
    FStringBuilder.AppendLine;
  end
  else
    ThrowWriterClosedException;
end;

procedure TdxChunkedStringBuilderWriter.WriteLine(Value: Int64);
begin
  if FIsOpen then
  begin
    FStringBuilder.Append(Value);
    FStringBuilder.AppendLine;
  end
  else
    ThrowWriterClosedException;
end;

procedure TdxChunkedStringBuilderWriter.WriteLine(const Value: TCharArray);
begin
  if FIsOpen then
  begin
    FStringBuilder.Append(Value, 0, Length(Value));
    FStringBuilder.AppendLine;
  end
  else
    ThrowWriterClosedException;
end;

procedure TdxChunkedStringBuilderWriter.WriteLine;
begin
  if FIsOpen then
    FStringBuilder.AppendLine
  else
    ThrowWriterClosedException;
end;

procedure TdxChunkedStringBuilderWriter.WriteLine(Value: Boolean);
begin
  if FIsOpen then
  begin
    FStringBuilder.Append(BoolToStr(Value, True));
    FStringBuilder.AppendLine;
  end
  else
    ThrowWriterClosedException;
end;

procedure TdxChunkedStringBuilderWriter.WriteLine(Value: Char);
begin
  if FIsOpen then
  begin
    FStringBuilder.Append(Value);
    FStringBuilder.AppendLine;
  end
  else
    ThrowWriterClosedException;
end;

procedure TdxChunkedStringBuilderWriter.WriteLine(Value: UInt64);
begin
  if FIsOpen then
  begin
    FStringBuilder.Append(Value);
    FStringBuilder.AppendLine;
  end
  else
    ThrowWriterClosedException;
end;

procedure TdxChunkedStringBuilderWriter.WriteLine(const Format: string; Args: array of const);
begin
  if FIsOpen then
  begin
    FStringBuilder.Append(SysUtils.Format(Format, Args));
    FStringBuilder.AppendLine;
  end
  else
    ThrowWriterClosedException;
end;

procedure TdxChunkedStringBuilderWriter.WriteLine(const Value: TCharArray; Index, Count: Integer);
begin
  if FIsOpen then
  begin
    FStringBuilder.Append(Value, Index, Count);
    FStringBuilder.AppendLine;
  end
  else
    ThrowWriterClosedException;
end;

procedure TdxChunkedStringBuilderWriter.WriteLine(Value: Cardinal);
begin
  if FIsOpen then
  begin
    FStringBuilder.Append(Value);
    FStringBuilder.AppendLine;
  end
  else
    ThrowWriterClosedException;
end;

procedure TdxChunkedStringBuilderWriter.WriteLine(Value: TObject);
begin
end;

procedure TdxChunkedStringBuilderWriter.WriteLine(Value: Single);
begin
  if FIsOpen then
  begin
    FStringBuilder.Append(Value);
    FStringBuilder.AppendLine;
  end
  else
    ThrowWriterClosedException;
end;

procedure TdxChunkedStringBuilderWriter.WriteLine(const Value: string);
begin
  if FIsOpen then
  begin
    FStringBuilder.Append(Value);
    FStringBuilder.AppendLine;
  end
  else
    ThrowWriterClosedException;
end;

procedure TdxChunkedStringBuilderWriter.ThrowWriterClosedException;
begin
  TdxRichEditExceptions.ThrowInvalidOperationException('writer is closed');
end;

end.
