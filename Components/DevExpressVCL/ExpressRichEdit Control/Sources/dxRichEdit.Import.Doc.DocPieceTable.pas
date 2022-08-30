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
unit dxRichEdit.Import.Doc.DocPieceTable;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Classes, Generics.Defaults, Generics.Collections,
  dxGenerics,
  dxRichEdit.Doc.Utils,
  dxRichEdit.Import.Doc.PieceDescriptor;

type

  { TdxDocPieceTable }

  TdxDocPieceTable = class
  public const
    PieceDescriptorSize = 8;
  strict private
    FIsDefault: Boolean;
    FPcdCount: Integer;
    FCharacterPositions: TdxIntegerList;
    FPieceDescriptors: TdxObjectList<TdxPieceDescriptor>;
  protected
    procedure Read(const APieceTable: TBytes);
    function GetCharacterPositions(const APieceTable: TBytes): TArray<Integer>;
    function GetPieceDescriptors(const APieceTable: TBytes): TArray<TdxPieceDescriptor>;
  public
    constructor Create;
    destructor Destroy; override;
    class function FromByteArray(const APieceTable: TBytes): TdxDocPieceTable; static;
    class function CreateDefault(ATextStartOffset: Integer; ALastCharacterPosition: Integer): TdxDocPieceTable; static;
    function ToByteArray: TBytes;
    function GetEncoding(APcdIndex: Integer): TEncoding;
    function GetOffset(APcdIndex: Integer): Integer;
    function GetLength(APcdIndex: Integer): Integer;
    procedure AddEntry(ACharacterPosition: Integer; APieceDescriptor: TdxPieceDescriptor);
    procedure AddLastPosition(ACharacterPosition: Integer);

    property PcdCount: Integer read FPcdCount;
  end;

implementation

uses
  Contnrs,
  dxTypeHelpers,
  dxEncoding;

{ TdxDocPieceTable }

constructor TdxDocPieceTable.Create;
begin
  FCharacterPositions := TdxIntegerList.Create;
  FPieceDescriptors := TdxObjectList<TdxPieceDescriptor>.Create;
end;

destructor TdxDocPieceTable.Destroy;
begin
  FPieceDescriptors.Free;
  FCharacterPositions.Free;
  inherited Destroy;
end;

class function TdxDocPieceTable.FromByteArray(const APieceTable: TBytes): TdxDocPieceTable;
begin
  Result := TdxDocPieceTable.Create;
  Result.Read(APieceTable);
end;

class function TdxDocPieceTable.CreateDefault(ATextStartOffset: Integer; ALastCharacterPosition: Integer): TdxDocPieceTable;
var
  ADocPieceDescriptor: TdxPieceDescriptor;
begin
  ADocPieceDescriptor := TdxPieceDescriptor.FromFileOffset(ATextStartOffset);
  Result := TdxDocPieceTable.Create;
  Result.FIsDefault := True;
  Result.FPcdCount := 1;
  Result.AddEntry(0, ADocPieceDescriptor);
  Result.AddLastPosition(ALastCharacterPosition);
end;

procedure TdxDocPieceTable.Read(const APieceTable: TBytes);
begin
  FPcdCount := (Length(APieceTable) - TdxDocConstants.CharacterPositionSize) div (TdxDocConstants.CharacterPositionSize + PieceDescriptorSize);
  FCharacterPositions.AddRange(GetCharacterPositions(APieceTable));
  FPieceDescriptors.AddRange(GetPieceDescriptors(APieceTable));
end;

function TdxDocPieceTable.ToByteArray: TBytes;
var
  APositionsCount, APieceDescriptorsCount, I, APieceDescriptorsOffset, AValue: Integer;
begin
  APositionsCount := FCharacterPositions.Count;
  APieceDescriptorsCount := FPieceDescriptors.Count;
  SetLength(Result, APositionsCount * TdxDocConstants.CharacterPositionSize + APieceDescriptorsCount * PieceDescriptorSize);
  for I := 0 to APositionsCount - 1 do
  begin
    AValue := FCharacterPositions[I];
    Move(AValue, Result[I * TdxDocConstants.CharacterPositionSize], TdxDocConstants.CharacterPositionSize);
  end;
  APieceDescriptorsOffset := APositionsCount * TdxDocConstants.CharacterPositionSize;
  for I := 0 to APieceDescriptorsCount - 1 do
    TArray.Copy<Byte>(FPieceDescriptors[I].ToByteArray, Result, 0, APieceDescriptorsOffset + I * PieceDescriptorSize, PieceDescriptorSize);
end;

function TdxDocPieceTable.GetCharacterPositions(const APieceTable: TBytes): TArray<Integer>;
var
  I: Integer;
begin
  SetLength(Result, PcdCount + 1);
  for I := 0 to High(Result) do
    Result[I] := PInteger(@APieceTable[I shl 2])^;
end;

function TdxDocPieceTable.GetPieceDescriptors(const APieceTable: TBytes): TArray<TdxPieceDescriptor>;
var
  APcdOffset, ACount, I: Integer;
  APieceDescriptor: TBytes;
begin
  SetLength(Result, PcdCount);
  APcdOffset := (PcdCount + 1) * 4;
  ACount := Length(Result);
  for I := 0 to ACount - 1 do
  begin
    SetLength(APieceDescriptor, TdxPieceDescriptor.PieceDescriptorSize);
    TArray.Copy<Byte>(APieceTable, APieceDescriptor, (I shl 3) + APcdOffset, 0, TdxPieceDescriptor.PieceDescriptorSize);
    Result[I] := TdxPieceDescriptor.FromByteArray(APieceDescriptor);
  end;
end;

function TdxDocPieceTable.GetEncoding(APcdIndex: Integer): TEncoding;
begin
  if FIsDefault then
    Exit(TdxEncoding.GetEncoding(1252));
  Result := FPieceDescriptors[APcdIndex].GetEncoding;
end;

function TdxDocPieceTable.GetOffset(APcdIndex: Integer): Integer;
begin
  Result := FPieceDescriptors[APcdIndex].GetOffset;
end;

function TdxDocPieceTable.GetLength(APcdIndex: Integer): Integer;
var
  ALength, AFc: Integer;
begin
  ALength := FCharacterPositions[APcdIndex + 1] - FCharacterPositions[APcdIndex];
  AFc := FPieceDescriptors[APcdIndex].FC;
  if (AFc and $40000000) <> 0 then
    Exit(ALength);
  Result := ALength * 2;
end;

procedure TdxDocPieceTable.AddEntry(ACharacterPosition: Integer; APieceDescriptor: TdxPieceDescriptor);
begin
  FCharacterPositions.Add(ACharacterPosition);
  FPieceDescriptors.Add(APieceDescriptor);
end;

procedure TdxDocPieceTable.AddLastPosition(ACharacterPosition: Integer);
begin
  FCharacterPositions.Add(ACharacterPosition);
end;

end.
