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

unit dxRichEdit.Import.Rtf.DestinationTableStyle;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, Classes, SysUtils, dxCoreClasses, dxRichEdit.Import.Rtf, dxRichEdit.Import.Rtf.DestinationPieceTable,
  dxRichEdit.DocumentModel.ParagraphFormatting, dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.Styles, dxRichEdit.DocumentModel.TableStyles,
  dxRichEdit.DocumentModel.TableFormatting, dxRichEdit.DocumentModel.CharacterFormatting;

type
  { TdxDestinationTableStyle }

  TdxDestinationTableStyle = class(TdxDestinationPieceTable)
  strict private
    class var FKeywordHT: TdxKeywordTranslatorTable;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateKeywordHT: TdxKeywordTranslatorTable; static;
  strict private
    FConditionalTableStyleFormattingType: TdxConditionalTableStyleFormattingType;
    FQFormat: Boolean;
    FRtfStyleIndex: Integer;
    FStyleName: string;
    function AddConditionalStyle: TdxTableConditionalStyle;
    function GetDefaultRtfProperties: TdxTableProperties;

    procedure SetCharacterFormattingOptions(AMask: TdxUsedCharacterFormattingOptions);

    class function GetThis(AImporter: TdxRtfImporter): TdxDestinationTableStyle;
    class procedure ParentStyleIndexHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure StyleQFormatKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure ConditionalStyleFirstRowHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure ConditionalStyleLastRowHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure ConditionalStyleFirstColumnHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure ConditionalStyleLastColumnHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure ConditionalStyleOddRowBandingHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure ConditionalStyleEvenRowBandingHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure ConditionalStyleOddColumnBandingHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure ConditionalStyleEvenColumnBandingHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure ConditionalStyleTopLeftCellHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure ConditionalStyleTopRightCellHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure ConditionalStyleBottomLeftCellHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure ConditionalStyleBottomRightCellHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
  protected
    function CanAppendText: Boolean; override;
    class function GetControlCharHT: TdxControlCharTranslatorTable; override;
    class function GetKeywordHT: TdxKeywordTranslatorTable; override;
    function CreateClone: TdxRichEditRtfDestinationBase; override;
    procedure ProcessCharCore(AChar: Char); override;

    function GetTableStyleByName(const AStyleName: string): TdxTableStyle;

    property QFormat: Boolean read FQFormat write FQFormat;
  public
    constructor Create(AImporter: TdxRtfImporter; AStyleIndex: Integer); reintroduce; overload;
    procedure BeforePopRtfState; override;
    procedure FinalizePieceTableCreation; override;
  end;

implementation

uses
  dxRichEdit.Utils.BatchUpdateHelper;

{ TdxDestinationTableStyle }

function TdxDestinationTableStyle.AddConditionalStyle: TdxTableConditionalStyle;
var
  ADocumentModel: TdxDocumentModel;
  AMainStyle: TdxTableStyle;
  ATableConditionalStyle: TdxTableConditionalStyle;
begin
  if not Importer.TableStyleCollectionIndex.ContainsKey(FRtfStyleIndex) then
    Exit(nil);

  ADocumentModel := Importer.DocumentModel;
  AMainStyle := ADocumentModel.TableStyles[Importer.GetTableStyleIndex(FRtfStyleIndex)];
  ATableConditionalStyle := AMainStyle.ConditionalStyleProperties.GetStyleSafe(FConditionalTableStyleFormattingType);
  Result := ATableConditionalStyle;
end;

function TdxDestinationTableStyle.GetDefaultRtfProperties: TdxTableProperties;
begin
  Result := DocumentModel.DefaultTableProperties;
end;

procedure TdxDestinationTableStyle.SetCharacterFormattingOptions(AMask: TdxUsedCharacterFormattingOptions);
var
  AOptions: TdxCharacterFormattingOptions;
begin
  AOptions := TdxCharacterFormattingOptions.Create(AMask);
  Importer.Position.CharacterFormatting.ReplaceInfo(Importer.Position.CharacterFormatting.Info, AOptions);
end;

procedure TdxDestinationTableStyle.BeforePopRtfState;
var
  AName: string;
  AStyle: IdxTableStyle;
  AParentStyleIndex: Integer;
  AIsConditionalStyle: Boolean;
  ATableStyle: TdxTableStyle;
  AParentTableStyle: TdxTableStyle;
  AParentCharacterProperties, AParentTableStyleCharacterProperties: TdxMergedCharacterProperties;
  AParentParagraphProperties, AParentTableStyleParagraphProperties: TdxMergedParagraphProperties;
  AParentTableProperties: TdxMergedTableProperties;
  AParentTableRowProperties, AParentTableStyleTableRowProperties: TdxMergedTableRowProperties;
  AParentTableCellProperties, AParentTableStyleTableCellProperties: TdxMergedTableCellProperties;
begin
  AName := Trim(FStyleName);
  AParentStyleIndex := Importer.Position.RtfFormattingInfo.ParentStyleIndex;
  AIsConditionalStyle := FConditionalTableStyleFormattingType <> TdxConditionalTableStyleFormattingType.WholeTable;
  if AIsConditionalStyle then
    AStyle := AddConditionalStyle
  else
  begin
    SetCharacterFormattingOptions(TdxCharacterFormattingOptions.MaskUseAll);
    if not Importer.TableStyleCollectionIndex.ContainsKey(FRtfStyleIndex) then
      AStyle := GetTableStyleByName(AName);
  end;

  if AStyle = nil then
    Exit;
  if AStyle is TdxTableStyle then
    ATableStyle := TdxTableStyle(AStyle)
  else
    ATableStyle := nil;
  if ATableStyle <> nil then
    ATableStyle.Primary := QFormat;
  if AName <> TdxTableStyleCollection.DefaultTableStyleName then
  begin
    if not AIsConditionalStyle then
      AParentTableStyle := DocumentModel.TableStyles[Importer.GetTableStyleIndex(AParentStyleIndex)]
    else
    begin
      AParentTableStyle := AStyle.Parent;
      if AParentTableStyle = nil then
        AParentTableStyle := DocumentModel.TableStyles[Importer.GetTableStyleIndex(-1)];
    end;
    if AIsConditionalStyle then
    begin
      AParentCharacterProperties := nil;
      AStyle.CharacterProperties.CopyFrom(Importer.Position.CharacterFormatting);
      AParentParagraphProperties := AParentTableStyle.GetMergedParagraphProperties(FConditionalTableStyleFormattingType);
      AParentTableRowProperties := AParentTableStyle.GetMergedTableRowProperties(FConditionalTableStyleFormattingType);
      AParentTableCellProperties := AParentTableStyle.GetMergedTableCellProperties(FConditionalTableStyleFormattingType);
    end
    else
    begin
      AParentTableStyleCharacterProperties := AParentTableStyle.GetMergedCharacterProperties;
      try
        AParentCharacterProperties := Importer.GetStyleMergedCharacterProperties(AParentTableStyleCharacterProperties);
        Importer.ApplyCharacterProperties(AStyle.CharacterProperties, Importer.Position.CharacterFormatting.Info, AParentCharacterProperties);
      finally
        AParentTableStyleCharacterProperties.Free;
      end;
      AParentTableStyleParagraphProperties := AParentTableStyle.GetMergedParagraphProperties;
      try
        AParentParagraphProperties := Importer.GetStyleMergedParagraphProperties(AParentTableStyleParagraphProperties);
      finally
        AParentTableStyleParagraphProperties.Free;
      end;
      AParentTableStyleTableRowProperties := AParentTableStyle.GetMergedTableRowProperties;
      try
        AParentTableRowProperties := Importer.GetStyleMergedTableRowProperties(AParentTableStyleTableRowProperties);
      finally
        AParentTableStyleTableRowProperties.Free;
      end;
      AParentTableStyleTableCellProperties := AParentTableStyle.GetMergedTableCellProperties;
      try
        AParentTableCellProperties := Importer.GetStyleMergedTableCellProperties(AParentTableStyleTableCellProperties);
      finally
        AParentTableStyleTableCellProperties.Free;
      end;
    end;
    AParentTableProperties := AParentTableStyle.GetMergedTableProperties;
    try
      AParentTableProperties.Merge(GetDefaultRtfProperties);
      Importer.ApplyLineSpacing(Importer.Position.ParagraphFormattingInfo);
      Importer.ApplyParagraphProperties(AStyle.ParagraphProperties, Importer.Position.ParagraphFormattingInfo, AParentParagraphProperties);
      Importer.ApplyTableProperties(Importer.TableReader.TableProperties, AParentTableProperties);
      AStyle.TableProperties.CopyFrom(Importer.TableReader.TableProperties);

      Importer.ApplyTableRowProperties(Importer.TableReader.RowProperties, AParentTableRowProperties);
      AStyle.TableRowProperties.CopyFrom(Importer.TableReader.RowProperties);

      Importer.ApplyTableCellProperties(Importer.TableReader.CellProperties, AParentTableCellProperties);
      AStyle.TableCellProperties.CopyFrom(Importer.TableReader.CellProperties);
    finally
      AParentCharacterProperties.Free;
      AParentParagraphProperties.Free;
      AParentTableRowProperties.Free;
      AParentTableCellProperties.Free;
      AParentTableProperties.Free;
    end;
  end;
  if not AIsConditionalStyle and Importer.TableStyleCollectionIndex.ContainsKey(AParentStyleIndex) then
    ATableStyle.Parent := DocumentModel.TableStyles[Importer.GetTableStyleIndex(AParentStyleIndex)];
end;

function TdxDestinationTableStyle.CanAppendText: Boolean;
begin
  Result := False;
end;

class function TdxDestinationTableStyle.GetControlCharHT: TdxControlCharTranslatorTable;
begin
  Result := nil;
end;

class function TdxDestinationTableStyle.GetKeywordHT: TdxKeywordTranslatorTable;
begin
  Result := FKeywordHT;
end;

constructor TdxDestinationTableStyle.Create(
  AImporter: TdxRtfImporter; AStyleIndex: Integer);
begin
  inherited Create(AImporter, AImporter.PieceTable);
  FConditionalTableStyleFormattingType := TdxConditionalTableStyleFormattingType.WholeTable;
  FRtfStyleIndex := AStyleIndex;
  SetCharacterFormattingOptions([]);
end;

class constructor TdxDestinationTableStyle.Initialize;
begin
  FKeywordHT := CreateKeywordHT;
end;

class destructor TdxDestinationTableStyle.Finalize;
begin
  FreeAndNil(FKeywordHT);
end;

function TdxDestinationTableStyle.CreateClone: TdxRichEditRtfDestinationBase;
begin
  Result := TdxDestinationTableStyle.Create(Importer, Importer.Position.CharacterStyleIndex);
end;

class function TdxDestinationTableStyle.CreateKeywordHT: TdxKeywordTranslatorTable;
begin
  Result := TdxKeywordTranslatorTable.Create;
  Result.Add('sbasedon', ParentStyleIndexHandler);
  Result.Add('sqformat', StyleQFormatKeywordHandler);

  Result.Add('tscfirstrow', ConditionalStyleFirstRowHandler);
  Result.Add('tsclastrow', ConditionalStyleLastRowHandler);
  Result.Add('tscfirstcol', ConditionalStyleFirstColumnHandler);
  Result.Add('tsclastcol', ConditionalStyleLastColumnHandler);
  Result.Add('tscbandhorzodd', ConditionalStyleOddRowBandingHandler);
  Result.Add('tscbandhorzeven', ConditionalStyleEvenRowBandingHandler);
  Result.Add('tscbandvertodd', ConditionalStyleOddColumnBandingHandler);
  Result.Add('tscbandverteven', ConditionalStyleEvenColumnBandingHandler);
  Result.Add('tscnwcell', ConditionalStyleTopLeftCellHandler);
  Result.Add('tscnecell', ConditionalStyleTopRightCellHandler);
  Result.Add('tscswcell', ConditionalStyleBottomLeftCellHandler);
  Result.Add('tscsecell', ConditionalStyleBottomRightCellHandler);
  AddCharacterPropertiesKeywords(Result);
  AddParagraphPropertiesKeywords(Result);
  AppendTablePropertiesKeywords(Result);
end;

procedure TdxDestinationTableStyle.FinalizePieceTableCreation;
begin
//do nothing
end;

function TdxDestinationTableStyle.GetTableStyleByName(
  const AStyleName: string): TdxTableStyle;
var
  ADocumentModel: TdxDocumentModel;
  ATableStyles: TdxTableStyleCollection;
  AStyleIndex: Integer;
begin
  ADocumentModel := Importer.DocumentModel;
  ATableStyles := ADocumentModel.TableStyles;
  AStyleIndex := ATableStyles.GetStyleIndexByName(AStyleName);
  if AStyleIndex >= 0 then
  begin
    Importer.TableStyleCollectionIndex.Add(FRtfStyleIndex, AStyleIndex);
    Result := ATableStyles[AStyleIndex];
  end
  else
  begin
    Result := TdxTableStyle.Create(ADocumentModel);
    Result.StyleName := AStyleName;
    AStyleIndex := ATableStyles.Add(Result);
    Importer.TableStyleCollectionIndex.Add(FRtfStyleIndex, AStyleIndex);
  end;
end;

class function TdxDestinationTableStyle.GetThis(
  AImporter: TdxRtfImporter): TdxDestinationTableStyle;
begin
  Result := AImporter.Destination as TdxDestinationTableStyle;
end;

procedure TdxDestinationTableStyle.ProcessCharCore(AChar: Char);
begin
  if AChar <> ';' then
    FStyleName := FStyleName + AChar;
end;

class procedure TdxDestinationTableStyle.ParentStyleIndexHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Position.RtfFormattingInfo.ParentStyleIndex := AParameterValue;
end;

class procedure TdxDestinationTableStyle.StyleQFormatKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  GetThis(AImporter).QFormat := True;
end;

class procedure TdxDestinationTableStyle.ConditionalStyleFirstRowHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  GetThis(AImporter).FConditionalTableStyleFormattingType := TdxConditionalTableStyleFormattingType.FirstRow;
end;

class procedure TdxDestinationTableStyle.ConditionalStyleLastRowHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  GetThis(AImporter).FConditionalTableStyleFormattingType := TdxConditionalTableStyleFormattingType.LastRow;
end;

class procedure TdxDestinationTableStyle.ConditionalStyleFirstColumnHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  GetThis(AImporter).FConditionalTableStyleFormattingType := TdxConditionalTableStyleFormattingType.FirstColumn;
end;

class procedure TdxDestinationTableStyle.ConditionalStyleLastColumnHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  GetThis(AImporter).FConditionalTableStyleFormattingType := TdxConditionalTableStyleFormattingType.LastColumn;
end;

class procedure TdxDestinationTableStyle.ConditionalStyleOddRowBandingHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  GetThis(AImporter).FConditionalTableStyleFormattingType := TdxConditionalTableStyleFormattingType.OddRowBanding;
end;

class procedure TdxDestinationTableStyle.ConditionalStyleEvenRowBandingHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  GetThis(AImporter).FConditionalTableStyleFormattingType := TdxConditionalTableStyleFormattingType.EvenRowBanding;
end;

class procedure TdxDestinationTableStyle.ConditionalStyleOddColumnBandingHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  GetThis(AImporter).FConditionalTableStyleFormattingType := TdxConditionalTableStyleFormattingType.OddColumnBanding;
end;

class procedure TdxDestinationTableStyle.ConditionalStyleEvenColumnBandingHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  GetThis(AImporter).FConditionalTableStyleFormattingType := TdxConditionalTableStyleFormattingType.EvenColumnBanding;
end;

class procedure TdxDestinationTableStyle.ConditionalStyleTopLeftCellHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  GetThis(AImporter).FConditionalTableStyleFormattingType := TdxConditionalTableStyleFormattingType.TopLeftCell;
end;

class procedure TdxDestinationTableStyle.ConditionalStyleTopRightCellHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  GetThis(AImporter).FConditionalTableStyleFormattingType := TdxConditionalTableStyleFormattingType.TopRightCell;
end;

class procedure TdxDestinationTableStyle.ConditionalStyleBottomLeftCellHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  GetThis(AImporter).FConditionalTableStyleFormattingType := TdxConditionalTableStyleFormattingType.BottomLeftCell;
end;

class procedure TdxDestinationTableStyle.ConditionalStyleBottomRightCellHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  GetThis(AImporter).FConditionalTableStyleFormattingType := TdxConditionalTableStyleFormattingType.BottomRightCell;
end;

end.
