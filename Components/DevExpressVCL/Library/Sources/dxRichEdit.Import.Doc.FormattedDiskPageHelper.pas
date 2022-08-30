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
unit dxRichEdit.Import.Doc.FormattedDiskPageHelper;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Classes, Generics.Defaults, Generics.Collections, dxCore, dxCoreClasses, dxCoreGraphics,
  dxGenerics,
  dxRichEdit.Doc.Utils,
  dxRichEdit.Import.Doc.FormattedDiskPage,
  dxRichEdit.Import.Doc.BinTable,
  dxRichEdit.Import.Doc.DCO,
  dxRichEdit.Import.Doc.DocCommand;

type

  { TdxFormattedDiskPageHelper }

  TdxFormattedDiskPageHelper = class
  public const
    PadByte        = Integer(1);
    StyleIndexSize = Integer(2);
  strict private
    FCharacterFKPs: TObjectDictionary<Integer, TdxCHPXFormattedDiskPage>;
    FParagraphsFKPs: TObjectDictionary<Integer, TdxPAPXFormattedDiskPage>;
    FMainStreamReader: TdxVirtualStreamBinaryReader;
    FDataStreamReader: TBinaryReader;
    FParagraphsBinTable: TdxBinTable;
    FCharactersBinTable: TdxBinTable;
    FObjectsToDelete: TObjectList<TdxDocPropertyContainer>;
  protected
    property MainStreamReader: TdxVirtualStreamBinaryReader read FMainStreamReader;
    property DataStreamReader: TBinaryReader read FDataStreamReader;
    property CharacterFormattedDiskPages: TObjectDictionary<Integer, TdxCHPXFormattedDiskPage> read FCharacterFKPs;
    property ParagraphFormattedDiskPages: TObjectDictionary<Integer, TdxPAPXFormattedDiskPage> read FParagraphsFKPs;
  public
    constructor Create(AMainStreamReader: TdxVirtualStreamBinaryReader; ADataStreamReader: TBinaryReader;
      AParagraphsBinTable: TdxBinTable; ACharactersBinTable: TdxBinTable);
    destructor Destroy; override;
    function UpdateCharacterProperties(AFc: Integer; AFactory: TdxDocCommandFactory): TdxDocPropertyContainer;
    procedure UpdateParagraphProperties(AFc: Integer; APropertyContainer: TdxDocPropertyContainer);
    function GetTextRunBorders: TdxObjectList<TdxTextRunBorder>;
    procedure AddTextRunBorders(ABinTable: TdxBinTable; AReason: TdxTextRunStartReason; ATextRunBorders: TdxObjectList<TdxTextRunBorder>);
  end;

implementation

uses
  Math, Contnrs,
  dxRichEdit.Import.Doc.DocCommandHelper;

{ TdxFormattedDiskPageHelper }

constructor TdxFormattedDiskPageHelper.Create(AMainStreamReader: TdxVirtualStreamBinaryReader;
  ADataStreamReader: TBinaryReader; AParagraphsBinTable: TdxBinTable; ACharactersBinTable: TdxBinTable);
begin
  FMainStreamReader := AMainStreamReader;//.Clone;
  FDataStreamReader := ADataStreamReader;
  FParagraphsBinTable := AParagraphsBinTable;
  FCharactersBinTable := ACharactersBinTable;
  FCharacterFKPs := TObjectDictionary<Integer, TdxCHPXFormattedDiskPage>.Create([doOwnsValues]);
  FParagraphsFKPs := TObjectDictionary<Integer, TdxPAPXFormattedDiskPage>.Create([doOwnsValues]);
  FObjectsToDelete := TObjectList<TdxDocPropertyContainer>.Create;
end;

destructor TdxFormattedDiskPageHelper.Destroy;
begin
  FCharacterFKPs.Free;
  FParagraphsFKPs.Free;
  FObjectsToDelete.Free;

  FParagraphsBinTable.Free;
  FCharactersBinTable.Free;

  inherited Destroy;
end;

function TdxFormattedDiskPageHelper.UpdateCharacterProperties(AFc: Integer; AFactory: TdxDocCommandFactory): TdxDocPropertyContainer;
var
  AFkpOffset, AInnerOffset, APxOffset: Integer;
  AChFkp: TdxCHPXFormattedDiskPage;
  AGrpprl: TBytes;
  AGrpprlSize: Byte;
begin
  AFkpOffset := FCharactersBinTable.GetFKPOffset(AFc);
  if not CharacterFormattedDiskPages.TryGetValue(AFkpOffset, AChFkp) then
  begin
    AChFkp := TdxCHPXFormattedDiskPage.FromStream(MainStreamReader, AFkpOffset);
    FCharacterFKPs.Add(AFkpOffset, AChFkp);
  end;
  AInnerOffset := AChFkp.GetInnerOffset(AFc);

  if AInnerOffset = 0 then
    SetLength(AGrpprl, 0)
  else
  begin
    APxOffset := AFkpOffset + (AInnerOffset shl 1);
    MainStreamReader.BaseStream.Seek(APxOffset, TSeekOrigin.soBeginning);
    AGrpprlSize := MainStreamReader.ReadByte;
    AGrpprl := MainStreamReader.ReadBytes(AGrpprlSize);
  end;
  Result := TdxDocCommandHelper.Traverse(AGrpprl, AFactory, FDataStreamReader);
  FObjectsToDelete.Add(Result);
end;

procedure TdxFormattedDiskPageHelper.UpdateParagraphProperties(AFc: Integer; APropertyContainer: TdxDocPropertyContainer);
var
  AFkpOffset, APapInnerOffset, APxOffset, AGrpprlSize: Integer;
  APapFkp: TdxPAPXFormattedDiskPage;
  AGrpprl: TBytes;
begin
  AFkpOffset := FParagraphsBinTable.GetFKPOffset(AFc);
  if not ParagraphFormattedDiskPages.TryGetValue(AFkpOffset, APapFkp) then
  begin
    APapFkp := TdxPAPXFormattedDiskPage.FromStream(MainStreamReader, AFkpOffset);
    ParagraphFormattedDiskPages.Add(AFkpOffset, APapFkp);
  end;
  APapInnerOffset := APapFkp.GetInnerOffset(AFc);
  if APapInnerOffset = 0 then
    Exit;

  APxOffset := AFkpOffset + (APapInnerOffset shl 1);
  FMainStreamReader.BaseStream.Seek(APxOffset, TSeekOrigin.soBeginning);
  AGrpprlSize := MainStreamReader.ReadByte;
  if AGrpprlSize = 0 then
    AGrpprlSize := (MainStreamReader.ReadByte * 2) - StyleIndexSize
  else
    AGrpprlSize := (AGrpprlSize * 2) - PadByte - StyleIndexSize;
  APropertyContainer.Update([TdxChangeActionType.Paragraph]);
  APropertyContainer.ParagraphInfo.ParagraphStyleIndex := SmallInt(MainStreamReader.ReadUInt16);
  if (AGrpprlSize <= 0) or (MainStreamReader.BaseStream.Position + AGrpprlSize > MainStreamReader.BaseStream.Size) then
    Exit;
  AGrpprl := MainStreamReader.ReadBytes(AGrpprlSize);
  TdxDocCommandHelper.Traverse(AGrpprl, APropertyContainer, FDataStreamReader);
end;

function TdxFormattedDiskPageHelper.GetTextRunBorders: TdxObjectList<TdxTextRunBorder>;
begin
  Result := TdxObjectList<TdxTextRunBorder>.Create;
  AddTextRunBorders(FCharactersBinTable, TdxTextRunStartReason.TextRunMark, Result);
  AddTextRunBorders(FParagraphsBinTable, TdxTextRunStartReason.ParagraphMark, Result);
end;

procedure TdxFormattedDiskPageHelper.AddTextRunBorders(ABinTable: TdxBinTable; AReason: TdxTextRunStartReason;
  ATextRunBorders: TdxObjectList<TdxTextRunBorder>);
var
  ABorderOffsets: TdxIntegerList;
  ACount, I, ACurrentOffset, AIndex: Integer;
  ATextRunBorder: TdxTextRunBorder;
  AComparable: IdxComparable<TdxTextRunBorder>;
begin
  ABorderOffsets := ABinTable.GetBorders(MainStreamReader);
  try
    ACount := ABorderOffsets.Count;
    for I := 0 to ACount - 1 do
    begin
      ACurrentOffset := ABorderOffsets[I];
      AComparable := TdxTextRunBorderComparable.Create(ACurrentOffset);
      if TdxAlgorithms1<TdxTextRunBorder>.BinarySearch(ATextRunBorders, AComparable, AIndex) then
        ATextRunBorders[AIndex].Reason := ATextRunBorders[AIndex].Reason + [AReason]
      else
      begin
        ATextRunBorder := TdxTextRunBorder.Create(ACurrentOffset, [AReason]);
        ATextRunBorders.Insert(AIndex, ATextRunBorder);
      end;
    end;
  finally
    ABorderOffsets.Free;
  end;
end;

end.
