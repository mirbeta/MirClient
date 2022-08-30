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
unit dxRichEdit.Import.Doc.SectionPropertiesHelper;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Classes, Generics.Defaults, Generics.Collections, dxGenerics,

  dxRichEdit.Doc.Utils,
  dxRichEdit.Import.Doc.DCO;

type

  { TdxSectionPropertiesHelper }

  TdxSectionPropertiesHelper = class
  public const
    SectionPropertyDescriptorSize = 12;
  strict private
    FCurrentSectionIndex: Integer;
    FSectionStartPositions: TdxIntegerList;
    FSectionPropertyDescriptorOffsets: TdxIntegerList;
  protected
    procedure Read(AMainStreamReader: TBinaryReader; ATableReader: TBinaryReader; AOffset, ASize: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    class function FromStream(AMainStreamReader: TBinaryReader; ATableStreamReader: TBinaryReader; AOffset, ASize: Integer): TdxSectionPropertiesHelper; static;
    procedure Write(AWriter: TBinaryWriter);
    procedure UpdateCurrentSectionProperties(AMainStreamReader: TBinaryReader; ADataStreamReader: TBinaryReader; APropertyContainer: TdxDocPropertyContainer);
    function GetSectionStartPositions(AReader: TBinaryReader; ACount: Integer): TdxIntegerList;
    function GetSectionPropertyDescriptorOffsets(AReader: TBinaryReader; ACount: Integer): TdxIntegerList;
    procedure AddEntry(ASectionStartPosition, ASepxOffset: Integer);
    procedure AddLastPosition(ALastPosition: Integer);
    procedure UpdateOffsets(AOffset: Integer);
  end;


implementation

uses
  dxRichEdit.Import.Doc.DocCommandHelper;

{ TdxSectionPropertiesHelper }

constructor TdxSectionPropertiesHelper.Create;
begin
  FSectionStartPositions := TdxIntegerList.Create;
  FSectionPropertyDescriptorOffsets := TdxIntegerList.Create;
end;

destructor TdxSectionPropertiesHelper.Destroy;
begin
  FSectionStartPositions.Free;
  FSectionPropertyDescriptorOffsets.Free;
  inherited Destroy;
end;

class function TdxSectionPropertiesHelper.FromStream(AMainStreamReader: TBinaryReader; ATableStreamReader: TBinaryReader;
   AOffset, ASize: Integer): TdxSectionPropertiesHelper;
begin
  Result := TdxSectionPropertiesHelper.Create;
  Result.Read(AMainStreamReader, ATableStreamReader, AOffset, ASize);
end;

procedure TdxSectionPropertiesHelper.Read(AMainStreamReader: TBinaryReader; ATableReader: TBinaryReader; AOffset, ASize: Integer);
var
  ASectionsCount: Integer;
begin
  Assert(AMainStreamReader <> nil, 'mainStreamReader');
  Assert(ATableReader <> nil, 'tableReader');
  ATableReader.BaseStream.Seek(AOffset, TSeekOrigin.soBeginning);
  ASectionsCount := (ASize - TdxDocConstants.CharacterPositionSize) div (TdxDocConstants.CharacterPositionSize + SectionPropertyDescriptorSize);
  FSectionStartPositions.Free;
  FSectionStartPositions := GetSectionStartPositions(ATableReader, ASectionsCount + 1);
  FSectionPropertyDescriptorOffsets.Free;
  FSectionPropertyDescriptorOffsets := GetSectionPropertyDescriptorOffsets(ATableReader, ASectionsCount);
end;

procedure TdxSectionPropertiesHelper.Write(AWriter: TBinaryWriter);
var
  I: Integer;
begin
  Assert(AWriter <> nil, 'writer');
  AWriter.Write(FSectionStartPositions[0]);
  for I := 1 to FSectionStartPositions.Count - 1 do
  begin
    if FSectionStartPositions[I] <> FSectionStartPositions[I - 1] then
      AWriter.Write(FSectionStartPositions[I]);
  end;
  for I := 0 to FSectionPropertyDescriptorOffsets.Count - 1 do
  begin
    AWriter.BaseStream.Seek(2, TSeekOrigin.soCurrent);
    AWriter.Write(FSectionPropertyDescriptorOffsets[I]);
    AWriter.Seek(6, TSeekOrigin.soCurrent);
  end;
end;

procedure TdxSectionPropertiesHelper.UpdateCurrentSectionProperties(AMainStreamReader: TBinaryReader;
  ADataStreamReader: TBinaryReader; APropertyContainer: TdxDocPropertyContainer);
var
  AGrpprlSize: Word;
  AGrpprl: TBytes;
begin
  if (FCurrentSectionIndex >= FSectionPropertyDescriptorOffsets.Count) or (FSectionPropertyDescriptorOffsets[FCurrentSectionIndex] = -1) then
    Exit;

  AMainStreamReader.BaseStream.Seek(FSectionPropertyDescriptorOffsets[FCurrentSectionIndex], TSeekOrigin.soBeginning);
  AGrpprlSize := AMainStreamReader.ReadUInt16;
  AGrpprl := AMainStreamReader.ReadBytes(AGrpprlSize);
  TdxDocCommandHelper.Traverse(AGrpprl, APropertyContainer, ADataStreamReader);
  Inc(FCurrentSectionIndex);
end;

function TdxSectionPropertiesHelper.GetSectionStartPositions(AReader: TBinaryReader; ACount: Integer): TdxIntegerList;
var
  I: Integer;
begin
  Result := TdxIntegerList.Create;
  Result.Capacity := ACount;
  for I := 0 to ACount - 1 do
    Result.Add(AReader.ReadInt32);
end;

function TdxSectionPropertiesHelper.GetSectionPropertyDescriptorOffsets(AReader: TBinaryReader; ACount: Integer): TdxIntegerList;
var
  I: Integer;
begin
  Result := TdxIntegerList.Create;
  Result.Capacity := ACount;
  for I := 0 to ACount - 1 do
  begin
    if FSectionStartPositions[I] = FSectionStartPositions[I + 1] then
      AReader.BaseStream.Seek(SectionPropertyDescriptorSize, TSeekOrigin.soCurrent)
    else
    begin
      AReader.BaseStream.Seek(2, TSeekOrigin.soCurrent);
      Result.Add(AReader.ReadInt32);
      AReader.BaseStream.Seek(6, TSeekOrigin.soCurrent);
    end;
  end;
end;

procedure TdxSectionPropertiesHelper.AddEntry(ASectionStartPosition: Integer; ASepxOffset: Integer);
begin
  FSectionStartPositions.Add(ASectionStartPosition);
  FSectionPropertyDescriptorOffsets.Add(ASepxOffset);
end;

procedure TdxSectionPropertiesHelper.AddLastPosition(ALastPosition: Integer);
begin
  FSectionStartPositions.Add(ALastPosition);
end;

procedure TdxSectionPropertiesHelper.UpdateOffsets(AOffset: Integer);
var
  ACount, I: Integer;
begin
  ACount := FSectionPropertyDescriptorOffsets.Count;
  for I := 0 to ACount - 1 do
  begin
    if FSectionPropertyDescriptorOffsets[I] <> -1 then
      FSectionPropertyDescriptorOffsets[I] := FSectionPropertyDescriptorOffsets[I] + AOffset;
  end;
end;

end.
