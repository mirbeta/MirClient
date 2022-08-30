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
unit dxRichEdit.Import.Doc.FormattedDiskPage;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Classes, Generics.Defaults, Generics.Collections, dxCore, dxCoreClasses, dxCoreGraphics,
  dxGenerics,
  dxRichEdit.Doc.Utils,
  dxRichEdit.DocumentModel.FloatingObjectFormatting;

type

  { TdxFormattedDiskPageBase }

  TdxFormattedDiskPageBase = class abstract
  strict private
    FFc: TdxIntegerList;
    FInnerOffsets: TList<Byte>;
  protected
    procedure Read(AReader: TBinaryReader; AOffset: Integer); virtual;
    function CalculateIndex(AFc: Integer): Integer;
    procedure WriteFilePositions(AWriter: TBinaryWriter);
    procedure ReadInnerOffsets(AReader: TBinaryReader); virtual; abstract;
    procedure WriteInnerOffsets(AWriter: TBinaryWriter); virtual; abstract;
    procedure WriteByteProperties(AWriter: TBinaryWriter; AStartPosition: Int64); virtual; abstract;

    property FC: TdxIntegerList read FFc;
    property InnerOffsets: TList<Byte> read FInnerOffsets;
  public
    constructor Create;
    destructor Destroy; override;
    function GetFirstOffset: Integer;
    procedure AddLastPosition(AFilePosition: Integer);
    function GetInnerOffset(AFc: Integer): Byte;
    procedure Write(AWriter: TBinaryWriter); virtual;
  end;

  { TdxPAPXFormattedDiskPage }

  TdxPAPXFormattedDiskPage = class(TdxFormattedDiskPageBase)
  public const
    BasePageSize                       = Integer(510);
    StyleDescriptorSize                = Integer(2);
    PadByteSize                        = Integer(1);
    SizeOfGrpprlSizeAndStyleDescriptor = Integer(1);
    ParagraphHeightSize                = Integer(12);
    BxSize                             = Integer(13);
  strict private
    FStyleIndexes: TdxIntegerList;
    FGrppapx: TList<TBytes>;
  protected
    procedure ReadInnerOffsets(AReader: TBinaryReader); override;
    procedure WriteInnerOffsets(AWriter: TBinaryWriter); override;
    procedure WriteByteProperties(AWriter: TBinaryWriter; AStartPosition: Int64); override;
  public
    constructor Create;
    destructor Destroy; override;
    class function FromStream(AReader: TBinaryReader; AOffset: Integer): TdxPAPXFormattedDiskPage; static;
    function TryToAddGrpprlAndPosition(AFilePosition: Integer; AStyleIndex: Integer; const AGrpprl: TBytes): Boolean;
  end;

  { TdxCHPXFormattedDiskPage }

  TdxCHPXFormattedDiskPage = class(TdxFormattedDiskPageBase)
  public const
    FilePositionSize     = Integer(4);
    LastFilePositionSize = Integer(4);
    InnerOffsetSize      = Integer(1);
    SizeOfGrpprlSize     = Integer(1);
  strict private
    FGrpchpx: TList<TBytes>;
    FLastActiveInnerOffset: Integer;
  protected
    procedure ReadInnerOffsets(AReader: TBinaryReader); override;
    procedure WriteInnerOffsets(AWriter: TBinaryWriter); override;
    procedure WriteByteProperties(AWriter: TBinaryWriter; AStartPosition: Int64); override;

    property LastActiveInnerOffset: Integer read FLastActiveInnerOffset write FLastActiveInnerOffset;
  public
    constructor Create;
    destructor Destroy; override;
    class function FromStream(AReader: TBinaryReader; AOffset: Integer): TdxCHPXFormattedDiskPage; static;
    function TryToAddGrpprlAndPosition(AFilePosition: Integer; const AGrpprl: TBytes): Boolean;
  end;

  { TdxVirtualStreamBinaryReader }

  TdxVirtualStreamBinaryReader = class(TBinaryReader)
  public
  end;

implementation

uses
  Math;

{ TdxFormattedDiskPageBase }

constructor TdxFormattedDiskPageBase.Create;
begin
  FFc := TdxIntegerList.Create;
  FInnerOffsets := TList<Byte>.Create;
end;

destructor TdxFormattedDiskPageBase.Destroy;
begin
  FFc.Free;
  FInnerOffsets.Free;
  inherited Destroy;
end;

procedure TdxFormattedDiskPageBase.Read(AReader: TBinaryReader; AOffset: Integer);
begin
  Assert(AReader <> nil, 'reader');
  FFc.AddRange(TdxSectorHelper.GetBorders(AReader, AOffset));
  ReadInnerOffsets(AReader);
end;

function TdxFormattedDiskPageBase.GetFirstOffset: Integer;
begin
  Result := FFc[0];
end;

procedure TdxFormattedDiskPageBase.AddLastPosition(AFilePosition: Integer);
begin
  FFc.Add(AFilePosition);
end;

function TdxFormattedDiskPageBase.GetInnerOffset(AFc: Integer): Byte;
var
  AIndex: Integer;
begin
  if InnerOffsets.Count = 0 then
    Exit(0);
  AIndex := Min(CalculateIndex(AFc), InnerOffsets.Count - 1);
  Result := InnerOffsets[AIndex];
end;

function TdxFormattedDiskPageBase.CalculateIndex(AFc: Integer): Integer;
begin
  if not FC.BinarySearch(AFc, Result) then
    Result := Max(0, Result - 1);
end;

procedure TdxFormattedDiskPageBase.Write(AWriter: TBinaryWriter);
var
  AStartPosition: Int64;
begin
  Assert(AWriter <> nil, 'writer');
  AStartPosition := AWriter.BaseStream.Position;
  WriteFilePositions(AWriter);
  WriteInnerOffsets(AWriter);
  WriteByteProperties(AWriter, AStartPosition);
  AWriter.BaseStream.Seek(AStartPosition + TdxDocConstants.LastByteOffsetInSector, TSeekOrigin.soBeginning);
  AWriter.Write(Byte(InnerOffsets.Count));
end;

procedure TdxFormattedDiskPageBase.WriteFilePositions(AWriter: TBinaryWriter);
var
  ACount, I: Integer;
begin
  ACount := FC.Count;
  for I := 0 to ACount - 1 do
    AWriter.Write(FC[I]);
end;

{ TdxPAPXFormattedDiskPage }

constructor TdxPAPXFormattedDiskPage.Create;
begin
  inherited Create;
  FStyleIndexes := TdxIntegerList.Create;
  FGrppapx := TList<TBytes>.Create;
end;

destructor TdxPAPXFormattedDiskPage.Destroy;
begin
  FStyleIndexes.Free;
  FGrppapx.Free;
  inherited Destroy;
end;

class function TdxPAPXFormattedDiskPage.FromStream(AReader: TBinaryReader; AOffset: Integer): TdxPAPXFormattedDiskPage;
begin
  Result := TdxPAPXFormattedDiskPage.Create;
  Result.Read(AReader, AOffset);
end;

procedure TdxPAPXFormattedDiskPage.ReadInnerOffsets(AReader: TBinaryReader);
var
  ACount, I: Integer;
begin
  ACount := FC.Count - 1;
  for I := 0 to ACount - 1 do
  begin
    InnerOffsets.Add(AReader.ReadByte);
    AReader.BaseStream.Seek(ParagraphHeightSize, TSeekOrigin.soCurrent);
  end;
end;

function TdxPAPXFormattedDiskPage.TryToAddGrpprlAndPosition(AFilePosition: Integer; AStyleIndex: Integer; const AGrpprl: TBytes): Boolean;
var
  APapxLength, AGrpprlOffset: Integer;
begin
  APapxLength := Length(AGrpprl) + SizeOfGrpprlSizeAndStyleDescriptor + StyleDescriptorSize;
  if Length(AGrpprl) mod 2 = 0 then
    Inc(APapxLength, PadByteSize);

  if InnerOffsets.Count = 0 then
    AGrpprlOffset := BasePageSize - APapxLength
  else
    AGrpprlOffset := InnerOffsets[InnerOffsets.Count - 1] * 2 - APapxLength;

  if AGrpprlOffset < (TdxDocConstants.CharacterPositionSize + BxSize) * (InnerOffsets.Count + 1) + TdxDocConstants.CharacterPositionSize then
    Exit(False);
  FC.Add(AFilePosition);
  InnerOffsets.Add(Byte(AGrpprlOffset div 2));
  FStyleIndexes.Add(AStyleIndex);
  FGrppapx.Add(AGrpprl);
  Result := True;
end;

procedure TdxPAPXFormattedDiskPage.WriteInnerOffsets(AWriter: TBinaryWriter);
var
  ACount, I: Integer;
  AReserved: TBytes;
begin
  ACount := InnerOffsets.Count;
  SetLength(AReserved, ParagraphHeightSize);
  for I := 0 to ACount - 1 do
  begin
    AWriter.Write(InnerOffsets[I]);
    AWriter.Write(AReserved);
  end;
end;

procedure TdxPAPXFormattedDiskPage.WriteByteProperties(AWriter: TBinaryWriter; AStartPosition: Int64);
var
  ACount, I: Integer;
begin
  ACount := FGrppapx.Count;
  for I := 0 to ACount - 1 do
  begin
    AWriter.BaseStream.Seek(AStartPosition + InnerOffsets[I] * 2, TSeekOrigin.soBeginning);
    if Length(FGrppapx[I]) mod 2 = 0 then
    begin
      AWriter.Write(Byte(0));
      AWriter.Write(Byte((Length(FGrppapx[I]) + StyleDescriptorSize) div 2));
    end
    else
      AWriter.Write(Byte((Length(FGrppapx[I]) + StyleDescriptorSize + PadByteSize) div 2));
    AWriter.Write(Word(FStyleIndexes[I]));
    AWriter.Write(FGrppapx[I]);
  end;
end;

{ TdxCHPXFormattedDiskPage }

constructor TdxCHPXFormattedDiskPage.Create;
begin
  inherited Create;
  FLastActiveInnerOffset := TdxDocConstants.LastByteOffsetInSector;
  FGrpchpx := TList<TBytes>.Create;
end;

destructor TdxCHPXFormattedDiskPage.Destroy;
begin
  FGrpchpx.Free;
  inherited Destroy;
end;

class function TdxCHPXFormattedDiskPage.FromStream(AReader: TBinaryReader; AOffset: Integer): TdxCHPXFormattedDiskPage;
begin
  Result := TdxCHPXFormattedDiskPage.Create;
  Result.Read(AReader, AOffset);
end;

procedure TdxCHPXFormattedDiskPage.ReadInnerOffsets(AReader: TBinaryReader);
var
  ACount, I: Integer;
begin
  ACount := FC.Count - 1;
  for I := 0 to ACount - 1 do
    InnerOffsets.Add(AReader.ReadByte);
end;

function TdxCHPXFormattedDiskPage.TryToAddGrpprlAndPosition(AFilePosition: Integer; const AGrpprl: TBytes): Boolean;
var
  AChpxLength, AGrpprlOffset: Integer;
begin
  if Length(AGrpprl) = 0 then
    AChpxLength := 0
  else
    AChpxLength := Length(AGrpprl) + SizeOfGrpprlSize;

  if Length(AGrpprl) = 0 then
    AGrpprlOffset := 0
  else
    AGrpprlOffset := LastActiveInnerOffset - AChpxLength;

  if AGrpprlOffset mod 2 <> 0 then
    Dec(AGrpprlOffset);

  if AGrpprlOffset <> 0 then
    LastActiveInnerOffset := AGrpprlOffset;

  if LastActiveInnerOffset < (FilePositionSize + InnerOffsetSize) * (InnerOffsets.Count + 1) + LastFilePositionSize then
    Exit(False);
  FC.Add(AFilePosition);
  InnerOffsets.Add(Byte(AGrpprlOffset div 2));
  FGrpchpx.Add(AGrpprl);
  Result := True;
end;

procedure TdxCHPXFormattedDiskPage.WriteInnerOffsets(AWriter: TBinaryWriter);
var
  ACount, I: Integer;
begin
  ACount := InnerOffsets.Count;
  for I := 0 to ACount - 1 do
    AWriter.Write(InnerOffsets[I]);
end;

procedure TdxCHPXFormattedDiskPage.WriteByteProperties(AWriter: TBinaryWriter; AStartPosition: Int64);
var
  ACount, I: Integer;
begin
  ACount := FGrpchpx.Count;
  for I := 0 to ACount - 1 do
  begin
    AWriter.BaseStream.Seek(AStartPosition + InnerOffsets[I] * 2, TSeekOrigin.soBeginning);
    AWriter.Write(Byte(Length(FGrpchpx[I])));
    AWriter.Write(FGrpchpx[I]);
  end;
end;


end.
