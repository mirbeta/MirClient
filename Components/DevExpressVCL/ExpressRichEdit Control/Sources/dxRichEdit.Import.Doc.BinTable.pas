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
unit dxRichEdit.Import.Doc.BinTable;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Classes, Generics.Defaults, Generics.Collections, dxGenerics;

type

  { TdxBinTable }

  TdxBinTable = class
  public const
    PositionSize        = Integer(4);
    OffsetSize          = Integer(4);
    SectorOffsetBitMask = Integer($3FFFFF);
  strict private
    FPositions: TdxIntegerList;
    FSectorsOffsets: TdxIntegerList;
  protected
    procedure Read(AReader: TBinaryReader; AOffset: Integer; ASize: Integer; AStreamLength: Int64);
  public
    constructor Create;
    destructor Destroy; override;
    class function FromStream(AReader: TBinaryReader; AOffset: Integer; ASize: Integer; AStreamLength: Int64): TdxBinTable; static;
    procedure Write(AWriter: TBinaryWriter);
    function GetPositions(AReader: TBinaryReader; ACount: Integer): TArray<Integer>;
    function GetSectorsOffsets(AReader: TBinaryReader; ACount: Integer; AStreamLength: Int64): TArray<Integer>;
    function GetBorders(AReader: TBinaryReader): TdxIntegerList;
    function GetFKPOffset(AFc: Integer): Integer;
    procedure AddEntry(AFcFirst: Integer; AFkpPageNumber: Integer);
    procedure AddLastPosition(ALastPosition: Integer);
    procedure UpdateSectorsOffsets(AOffset: Integer);

    property Positions: TdxIntegerList read FPositions;
    property SectorsOffsets: TdxIntegerList read FSectorsOffsets;
  end;


implementation

uses
  Math,
  dxRichEdit.Doc.Utils;

{ TdxBinTable }

constructor TdxBinTable.Create;
begin
  FPositions := TdxIntegerList.Create;
  FSectorsOffsets := TdxIntegerList.Create;
end;

destructor TdxBinTable.Destroy;
begin
  FSectorsOffsets.Free;
  FPositions.Free;
  inherited Destroy;
end;

class function TdxBinTable.FromStream(AReader: TBinaryReader; AOffset: Integer; ASize: Integer; AStreamLength: Int64): TdxBinTable;
begin
  Result := TdxBinTable.Create;
  Result.Read(AReader, AOffset, ASize, AStreamLength);
end;

procedure TdxBinTable.Read(AReader: TBinaryReader; AOffset: Integer; ASize: Integer; AStreamLength: Int64);
var
  ABinEntriesCount: Integer;
begin
  Assert(AReader <> nil, 'reader');
  AReader.BaseStream.Seek(AOffset, TSeekOrigin.soBeginning);
  ABinEntriesCount := (ASize - PositionSize) div (PositionSize + OffsetSize);
  FPositions.AddRange(GetPositions(AReader, ABinEntriesCount + 1));
  FSectorsOffsets.AddRange(GetSectorsOffsets(AReader, ABinEntriesCount, AStreamLength));
end;

procedure TdxBinTable.Write(AWriter: TBinaryWriter);
var
  ACount, I: Integer;
begin
  Assert(AWriter <> nil, 'writer');
  ACount := FPositions.Count;
  for I := 0 to ACount - 1 do
    AWriter.Write(Cardinal(FPositions[I]));

  ACount := FSectorsOffsets.Count;
  for I := 0 to ACount - 1 do
    AWriter.Write(Cardinal(FSectorsOffsets[I] div TdxDocConstants.ContentBuilderSectorSize));
end;

function TdxBinTable.GetPositions(AReader: TBinaryReader; ACount: Integer): TArray<Integer>;
var
  I: Integer;
begin
  SetLength(Result, ACount);
  for I := 0 to ACount - 1 do
    Result[I] := AReader.ReadInt32;
end;

function TdxBinTable.GetSectorsOffsets(AReader: TBinaryReader; ACount: Integer; AStreamLength: Int64): TArray<Integer>;
var
  I, ACurrentOffset: Integer;
begin
  SetLength(Result, ACount);

  for I := 0 to ACount - 1 do
  begin
    ACurrentOffset := TdxDocConstants.ContentBuilderSectorSize * (AReader.ReadInt32 and SectorOffsetBitMask);
    if ACurrentOffset < AStreamLength then
    begin
      Result[I] := ACurrentOffset;
      Continue;
    end;
    if I > 0 then
      Result[I] := Result[I - 1] + TdxDocConstants.ContentBuilderSectorSize;
  end;
end;

function TdxBinTable.GetBorders(AReader: TBinaryReader): TdxIntegerList;
var
  ACount, I, ASectorOffset: Integer;
begin
  Result := TdxIntegerList.Create;
  ACount := FSectorsOffsets.Count;
  for I := 0 to ACount - 1 do
  begin
    ASectorOffset := FSectorsOffsets[I];
    Result.AddRange(TdxSectorHelper.GetBorders(AReader, ASectorOffset));
  end;
end;

function TdxBinTable.GetFKPOffset(AFc: Integer): Integer;
var
  APosition: Integer;
begin
  if not FPositions.BinarySearch(AFc, APosition) then
    Dec(APosition);
  APosition := Min(APosition, FSectorsOffsets.Count - 1);
  Result := FSectorsOffsets[APosition];
end;

procedure TdxBinTable.AddEntry(AFcFirst: Integer; AFkpPageNumber: Integer);
begin
  FPositions.Add(AFcFirst);
  FSectorsOffsets.Add(AFkpPageNumber);
end;

procedure TdxBinTable.AddLastPosition(ALastPosition: Integer);
begin
  FPositions.Add(ALastPosition + 2);
end;

procedure TdxBinTable.UpdateSectorsOffsets(AOffset: Integer);
var
  ACount, I: Integer;
begin
  ACount := FSectorsOffsets.Count;
  for I := 0 to ACount - 1 do
    FSectorsOffsets[I] := FSectorsOffsets[I] + AOffset * TdxDocConstants.ContentBuilderSectorSize;
end;

end.
