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

unit dxRichEdit.Import.Html.TableTags;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, SysUtils, Classes, Generics.Defaults, Generics.Collections,
  dxCore, dxCoreGraphics,

  dxGenerics,
  dxRichEdit.Utils.Types,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.History.IndexChangedHistoryItem,
  dxRichEdit.DocumentModel.ParagraphFormatting,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.TableFormatting,
  dxRichEdit.DocumentModel.Tables,
  dxRichEdit.DocumentModel.UnitConverter,
  dxRichEdit.Import.CSSParser,
  dxRichEdit.Import.Html,
  dxRichEdit.Import.Html.TagBase;

type

  { TdxTableTag }

  TdxTableTag = class(TdxTagBase)
  strict private
    class var
      FAttributeTable: TdxAttributeKeywordTranslatorTable;
    class constructor Initialize;
    class destructor Finalize;
  strict private
    FTableProperties: TdxHtmlTableProperties;
    FTableNotCreated: Boolean;
    class function AddAttributes: TdxAttributeKeywordTranslatorTable; static;
    class procedure AlignmentKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase); static;
    class procedure BackgroundImageKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase); static;
    class procedure BackgroundColorKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase); static;
    class procedure BorderWidthKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase); static;
    class procedure BorderColorKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase); static;
    function GetTablesDisabled: Boolean;
  protected
    function GetAttributeTable: TdxAttributeKeywordTranslatorTable; override;
    procedure ApplyTagProperties; override;
    function ApplyCssProperties: TdxParagraphFormattingOptions; override;
    procedure OpenTagProcessCore; override;
    function ShouldIgnoreOpenTag: Boolean;
    procedure ClearParentProperties;
    function ShouldAddParagraph: Boolean; override;
    procedure CloseUnclosedTableRowTag(AIndex: Integer);
    procedure CloseUnclosedTableCellTag(AIndex: Integer);
    function ObtainParentCell: TdxTableCell;
  public
    constructor Create(AImporter: TdxCustomHtmlImporter);
    destructor Destroy; override;
    procedure BeforeDeleteTagFromStack(AIndexOfDeletedTag: Integer); override;
    procedure DeleteOldOpenTag; override;
    procedure FunctionalTagProcess; override;

    class procedure ImportBackgroundColor(AConverter: TdxDocumentModelUnitConverter; const AValue: string;
      ATableProperties: TdxHtmlTableProperties); static;
    class procedure CellPaddingKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase); static;
    class procedure CellSpacingKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase); static;
    class procedure ColumnsCountKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase); static;
    class procedure TableHeightKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase); static;
    class procedure TableWidthKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase); static;
    procedure SetDefaultTableProperties;

    property TablesDisabled: Boolean read GetTablesDisabled;
    property TableProperties: TdxHtmlTableProperties read FTableProperties;
    property TableNotCreated: Boolean read FTableNotCreated write FTableNotCreated;
  end;

  { TdxTableCellTag }

  TdxTableCellTag = class abstract(TdxTagBase)
  strict private
    FStartParagraphIndex: TdxParagraphIndex;
    function GetTablesDisabled: Boolean;
  protected
    function CreateTableCell: TdxTableCell; virtual; abstract;
    procedure OpenTagProcessCore; override;
    function GetStartParagraphIndex(AOpenTag: TdxTableCellTag): TdxParagraphIndex;
    function GetEndParagraphIndex(AOpenTag: TdxTableCellTag; AStartParagraphIndex: TdxParagraphIndex): TdxParagraphIndex;
    function GetEndParagraphIndexNonEmptyLastParagraphCase: TdxParagraphIndex;
    function GetEndParagraphIndexEmptyLastParagraphCase(AOpenTag: TdxTableCellTag;
      AStartParagraphIndex: TdxParagraphIndex): TdxParagraphIndex;
    function GetEndParagraphIndexForCellWithContent(AOpenTag: TdxTableCellTag): TdxParagraphIndex; virtual;
    function GetEndParagraphIndexForEmptyCell(AStartParagraphIndex: TdxParagraphIndex): TdxParagraphIndex;
  public
    procedure BeforeDeleteTagFromStack(AIndexOfDeletedTag: Integer); override;

    property StartParagraphIndex: TdxParagraphIndex read FStartParagraphIndex write FStartParagraphIndex;
    property TablesDisabled: Boolean read GetTablesDisabled;
  end;

  { TdxTrTag }

  TdxTrTag = class(TdxTagBase)
  strict private
    class var
      FAttributeTable: TdxAttributeKeywordTranslatorTable;
    class constructor Initialize;
    class destructor Finalize;
  strict private
    FRowProperties: TdxHtmlTableRowProperties;
    FStartParagraphIndex: TdxParagraphIndex;
    FAlignment: TdxHtmlParagraphAlignment;
    class function AddAttributes: TdxAttributeKeywordTranslatorTable; static;
    class procedure AlignmentKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase); static;
    class procedure BackgroundCellColorKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase); static;
    class procedure VerticalAlignmentKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase); static;
    class procedure BorderColorKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase); static;
    class procedure HeightKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase); static;
    function GetTablesDisabled: Boolean;
  protected
    function GetAttributeTable: TdxAttributeKeywordTranslatorTable; override;
    procedure ApplyTagProperties; override;
    procedure OpenTagProcessCore; override;
    procedure CreateCellWhenAfterOpenTrTagWasContent(ATrTag: TdxTrTag; ALastRow: TdxTableRow);
    procedure AdaptParagraphMarkHeightToRowHeight;
  public
    constructor Create(AImporter: TdxCustomHtmlImporter);
    destructor Destroy; override;
    procedure BeforeDeleteTagFromStack(AIndexOfDeletedTag: Integer); override;
    procedure DeleteOldOpenTag; override;
    procedure FunctionalTagProcess; override;

    property RowProperties: TdxHtmlTableRowProperties read FRowProperties;
    property StartParagraphIndex: TdxParagraphIndex read FStartParagraphIndex write FStartParagraphIndex;
    property Alignment: TdxHtmlParagraphAlignment read FAlignment write FAlignment;
    property TablesDisabled: Boolean read GetTablesDisabled;
  end;

  { TdxTdTag }

  TdxTdTag = class(TdxTableCellTag, IUnknown, IdxCellPropertiesOwner)
  strict private
    class var
      FAttributeTable: TdxAttributeKeywordTranslatorTable;
    class constructor Initialize;
    class destructor Finalize;
  private
    FCellProperties: TdxTableCellProperties;
    FTablesCountOnOpen: Integer;
    FTableCellRowSpan: Integer;
    FTdTagWithoutOpenedTable: Boolean;
    FAlignment: TdxHtmlParagraphAlignment;
    class function AddAttributes: TdxAttributeKeywordTranslatorTable; static;
    class procedure HeadersKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase); static;
    class procedure ScopeKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase); static;
    class procedure AbbrKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase); static;
    class procedure AxisKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase); static;
    class procedure RowspanKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase); static;
    class procedure ColspanKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase); static;
    class procedure NowrapKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase); static;
    class procedure WidthKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase); static;
    class function FindRowTag(AImporter: TdxCustomHtmlImporter): TdxTrTag; static;
    class procedure HeightKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase); static;
    class procedure AlignmentKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase); static;
    class procedure VerticalAlignmentKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase); static;
    class procedure BorderColorKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase); static;
  protected
    function GetAttributeTable: TdxAttributeKeywordTranslatorTable; override;
    procedure ApplyTagProperties; override;
    function ApplyCssProperties: TdxParagraphFormattingOptions; override;
    procedure OpenTagProcessCore; override;
    function CreateTableCell: TdxTableCell; override;
    procedure ApplyHeightToTableRow(ATableRow: TdxTableRow; ARowSpan: Integer);
    procedure ApplyAllCellProperties(ACell: TdxTableCell); virtual;
    function GetEndParagraphIndexForCellWithContent(AOpenTag: TdxTableCellTag): TdxParagraphIndex; override;
    function GetParagraphIndexForCellContainsNestedTable: TdxParagraphIndex;
    // IUnknown
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    // IdxCellPropertiesOwner
    function CreateCellPropertiesChangedHistoryItem(AProperties: TdxTableCellProperties): TdxIndexChangedHistoryItemCore;

    property TablesCountOnOpen: Integer read FTablesCountOnOpen;
    property TableCellRowSpan: Integer read FTableCellRowSpan;
  public
    constructor Create(AImporter: TdxCustomHtmlImporter);
    destructor Destroy; override;
    procedure BeforeDeleteTagFromStack(AIndexOfDeletedTag: Integer); override;
    procedure DeleteOldOpenTag; override;
    procedure FunctionalTagProcess; override;
    function GetStartIndexAllowedSearchScope: Integer; override;

    class procedure SetColorToAllBorders(AColor: TdxAlphaColor; ACellProperties: TdxTableCellProperties); static;
    class procedure ImportBorderWidth(AUnitConverter: TdxDocumentModelUnitConverter; const AValue: string;
      ABottomBorder: TdxHtmlBorderProperty); static;
    class procedure ImportBorderColor(AConverter: TdxDocumentModelUnitConverter; const AValue: string;
      ABorder: TdxHtmlBorderProperty); static;
    class procedure BackgroundColorKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase); static;
    class procedure BackgroundImageKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase); static;

    property CellProperties: TdxTableCellProperties read FCellProperties;
    property Alignment: TdxHtmlParagraphAlignment read FAlignment write FAlignment;
    property TdTagWithoutOpenedTable: Boolean read FTdTagWithoutOpenedTable write FTdTagWithoutOpenedTable;
  end;

  { TdxThTag }

  TdxThTag = class(TdxTdTag)
  strict private
    class var
      FAttributeTable: TdxAttributeKeywordTranslatorTable;
    class constructor Initialize;
    class destructor Finalize;
  strict private
    class function AddAttributes: TdxAttributeKeywordTranslatorTable; static;
  protected
    function GetAttributeTable: TdxAttributeKeywordTranslatorTable; override;
    procedure ApplyTagProperties; override;
  end;

  { TdxCaptionTag }

  TdxCaptionTag = class(TdxTableCellTag)
  strict private
    class var
      FAttributeTable: TdxAttributeKeywordTranslatorTable;
    class constructor Initialize;
    class destructor Finalize;
  strict private
    class function AddAttributes: TdxAttributeKeywordTranslatorTable; static;
    class procedure AlignmentKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase); static;
  protected
    function GetAttributeTable: TdxAttributeKeywordTranslatorTable; override;
    procedure ApplyTagProperties; override;
    function CreateTableCell: TdxTableCell; override;
  public
    procedure FunctionalTagProcess; override;
  end;

  { TBodyTag }

  TBodyTag = class(TdxTagBase)
  protected
    procedure ApplyTagProperties; override;
  end;

  { THeadTag }

  THeadTag = class(TdxTagBase)
  protected
    procedure ApplyTagProperties; override;
  end;

  { TFootTag }

  TFootTag = class(TdxTagBase)
  protected
    procedure ApplyTagProperties; override;
  end;

implementation

uses
  Contnrs, Math,
  dxRichEdit.NativeApi,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.DocumentModel.Borders,
  dxRichEdit.Platform.Font,
  dxRichEdit.Import.Html.CSSParser,
  dxRichEdit.Import.Html.HTMLParser,
  dxRichEdit.Utils.NumberParser,
  dxRichEdit.Import.Html.NumberingListTags, dxRichEdit.DocumentModel.CharacterFormatting;

type
  TdxTagBaseHelper = class helper for TdxTagBase
  private
    function GetImporter: TdxHtmlImporter; inline;
  public
    property Importer: TdxHtmlImporter read GetImporter;
  end;

{ TdxTagBaseHelper }

function TdxTagBaseHelper.GetImporter: TdxHtmlImporter;
begin
  Result := TdxHtmlImporter(inherited Importer);
end;

{ TdxTableTag }

class constructor TdxTableTag.Initialize;
begin
  FAttributeTable := AddAttributes;
end;

class destructor TdxTableTag.Finalize;
begin
  FAttributeTable.Free;
end;

constructor TdxTableTag.Create(AImporter: TdxCustomHtmlImporter);
begin
  inherited Create(AImporter);

  SetDefaultTableProperties;
  FTableNotCreated := False;
end;

destructor TdxTableTag.Destroy;
begin
  FTableProperties.Free;
  inherited Destroy;
end;

class function TdxTableTag.AddAttributes: TdxAttributeKeywordTranslatorTable;
begin
  Result := CreateAttributeTable;
  Result.Add(ConvertKeyToUpper('align'), AlignmentKeyword);
  Result.Add(ConvertKeyToUpper('background'), BackgroundImageKeyword);
  Result.Add(ConvertKeyToUpper('bgcolor'), BackgroundColorKeyword);
  Result.Add(ConvertKeyToUpper('border'), BorderWidthKeyword);
  Result.Add(ConvertKeyToUpper('bordercolor'), BorderColorKeyword);
  Result.Add(ConvertKeyToUpper('cellpadding'), CellPaddingKeyword);
  Result.Add(ConvertKeyToUpper('cellspacing'), CellSpacingKeyword);
  Result.Add(ConvertKeyToUpper('cols'), ColumnsCountKeyword);
  Result.Add(ConvertKeyToUpper('height'), TableHeightKeyword);
  Result.Add(ConvertKeyToUpper('width'), TableWidthKeyword);
end;


class procedure TdxTableTag.AlignmentKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase);
var
  ATableTag: TdxTableTag;
  ARowAlignment: TdxTableRowAlignment;
begin
  ATableTag := TdxTableTag(ATag);
  ARowAlignment := TdxTableRowAlignment.Left;
  if ImportAlignment(AValue, ARowAlignment) then
    ATableTag.TableProperties.TableAlignment := ARowAlignment;
end;

class procedure TdxTableTag.BackgroundImageKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase);
begin
end;

class procedure TdxTableTag.BackgroundColorKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase);
var
  ATableTag: TdxTableTag;
begin
  ATableTag := TdxTableTag(ATag);
  ImportBackgroundColor(AImporter.DocumentModel.UnitConverter, AValue, ATableTag.TableProperties);
end;

class procedure TdxTableTag.BorderWidthKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase);
var
  ATableTag: TdxTableTag;
begin
  ATableTag := TdxTableTag(ATag);
  ImportBordersWidths(AImporter.DocumentModel.UnitConverter, AValue, ATableTag.tableProperties.BordersProperties);
  if ATableTag.tableProperties.BordersProperties.TopBorder.Width > 0 then
  begin
    ATableTag.tableProperties.SetInnerBorders := True;
    if not ATableTag.tableProperties.BordersProperties.TopBorder.UseLineStyle then
    begin
      ATableTag.tableProperties.BordersProperties.TopBorder.LineStyle := TdxBorderLineStyle.Single;
      ATableTag.tableProperties.BordersProperties.LeftBorder.LineStyle := TdxBorderLineStyle.Single;
      ATableTag.tableProperties.BordersProperties.RightBorder.LineStyle := TdxBorderLineStyle.Single;
      ATableTag.tableProperties.BordersProperties.BottomBorder.LineStyle := TdxBorderLineStyle.Single;
    end;
  end;
end;

class procedure TdxTableTag.BorderColorKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase);
var
  ATableTag: TdxTableTag;
begin
  ATableTag := TdxTableTag(ATag);
  ImportBordersColors(AImporter.DocumentModel.UnitConverter, AValue, ATableTag.TableProperties.BordersProperties);
end;

class procedure TdxTableTag.ImportBackgroundColor(AConverter: TdxDocumentModelUnitConverter; const AValue: string;
  ATableProperties: TdxHtmlTableProperties);
var
  AColor: TdxAlphaColor;
begin
  AColor := ImportColor(AConverter, AValue);
  if AColor <> TdxAlphaColors.Empty then
    ATableProperties.BackgroundColor := AColor;
end;

class procedure TdxTableTag.CellPaddingKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase);
var
  ATableTag: TdxTableTag;
  ACellMargin: TdxWidthUnitInfo;
begin
  ATableTag := TdxTableTag(ATag);
  ACellMargin := ConvertPixelsValueToWidthUnitInfo(AImporter.DocumentModel.UnitConverter, AValue);
  try
    if (ACellMargin.&Type <> TdxWidthUnitType.&Nil) and (ACellMargin.Value >= 0) then
      ATableTag.TableProperties.CellMargin := ACellMargin;
  finally
    ACellMargin.Free;
  end;
end;

class procedure TdxTableTag.CellSpacingKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase);
var
  ATableTag: TdxTableTag;
  ACellSpacing: TdxWidthUnitInfo;
begin
  ATableTag := TdxTableTag(ATag);
  ACellSpacing := ConvertPixelsValueToWidthUnitInfo(AImporter.DocumentModel.UnitConverter, AValue);
  try
    if (ACellSpacing.&Type <> TdxWidthUnitType.&Nil) and (ACellSpacing.Value >= 0) then
      ATableTag.TableProperties.CellSpacing := ACellSpacing;
  finally
    ACellSpacing.Free;
  end;
end;

class procedure TdxTableTag.ColumnsCountKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase);
begin
end;

class procedure TdxTableTag.TableHeightKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase);
begin
end;

class procedure TdxTableTag.TableWidthKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase);
var
  ATableTag: TdxTableTag;
  AWidth: TdxWidthUnitInfo;
begin
  ATableTag := TdxTableTag(ATag);
  AWidth := ConvertPixelsValueToWidthUnitInfo(AImporter.DocumentModel.UnitConverter, AValue);
  try
    if (AWidth.&Type <> TdxWidthUnitType.&Nil) and (AWidth.Value >= 0) then
      ATableTag.TableProperties.Width := AWidth;
  finally
    AWidth.Free;
  end;
end;

function TdxTableTag.GetTablesDisabled: Boolean;
begin
  Result := not Importer.TablesImportHelper.TablesAllowed;
end;

procedure TdxTableTag.SetDefaultTableProperties;
const
  AAlignmentMap: array[TdxParagraphAlignment] of TdxRichEditTableRowAlignment =
    (TdxRichEditTableRowAlignment.Left,
    TdxRichEditTableRowAlignment.Right,
    TdxRichEditTableRowAlignment.Center,
    TdxRichEditTableRowAlignment.Both);

var
  ADefaultSpacing, ADefaultMargin: Integer;
  AWidth: TdxWidthUnitInfo;
begin
  FTableProperties := TdxHtmlTableProperties.Create;
  FTableProperties.BordersProperties.TopBorder.Width := 0;
  FTableProperties.BordersProperties.LeftBorder.Width := 0;
  FTableProperties.BordersProperties.RightBorder.Width := 0;
  FTableProperties.BordersProperties.BottomBorder.Width := 0;
  if Importer.Position.ParagraphFormatting.Alignment in [TdxParagraphAlignment.Right, TdxParagraphAlignment.Center] then
    FTableProperties.TableAlignment := AAlignmentMap[Importer.Position.ParagraphFormatting.Alignment];
  if Importer.Options.DefaultTableCellSpacing > 0 then
  begin
    ADefaultSpacing := Importer.DocumentModel.UnitConverter.TwipsToModelUnits(Importer.Options.DefaultTableCellSpacing);
    AWidth := TdxWidthUnitInfo.Create(TdxWidthUnitType.ModelUnits, ADefaultSpacing);
    try
      FTableProperties.CellSpacing := AWidth;
    finally
      AWidth.Free;
    end;
  end;
  if Importer.Options.DefaultTableCellMarging > 0 then
  begin
    ADefaultMargin := Importer.DocumentModel.UnitConverter.TwipsToModelUnits(Importer.Options.DefaultTableCellMarging);
    AWidth := TdxWidthUnitInfo.Create(TdxWidthUnitType.ModelUnits, ADefaultMargin);
    try
      FTableProperties.CellMargin := AWidth;
    finally
      AWidth.Free;
    end;
  end;
end;

function TdxTableTag.GetAttributeTable: TdxAttributeKeywordTranslatorTable;
begin
  Result := FAttributeTable;
end;

procedure TdxTableTag.ApplyTagProperties;
var
  AIndent: TdxWidthUnitInfo;
begin
  Importer.Position.TableProperties.CopyFrom(TableProperties);
  AIndent := TdxWidthUnitInfo.Create(TdxWidthUnitType.ModelUnits,
    Importer.Position.TableProperties.Indent.Value + Importer.Position.AdditionalIndent);
  try
    Importer.Position.TableProperties.Indent := AIndent;
  finally
    AIndent.Free;
  end;
  TableProperties.ApplyPropertiesToCharacter(Importer.Position.CharacterFormatting);
end;

function TdxTableTag.ApplyCssProperties: TdxParagraphFormattingOptions;
var
  AOptions: TdxParagraphFormattingOptions;
  ACount, ADelta: Integer;
  APrevTag: TdxOpenHtmlTag;
  ALevelTag: TdxLevelTag;
begin
  AOptions := inherited ApplyCssProperties;
  ACount := Importer.TagsStack.Count;
  if ACount < 2 then
    Exit(AOptions);
  APrevTag := Importer.TagsStack[ACount - 2];
  ALevelTag := Safe<TdxLevelTag>.Cast(APrevTag.Tag);
  if ALevelTag <> nil then
  begin
    if AOptions.UseLeftIndent then
      ADelta := Importer.Position.ParagraphFormatting.LeftIndent
    else
      ADelta := 0;
    Importer.Position.TableProperties.Indent.Value := Importer.TagsStack[ACount - 1].OldPosition.ParagraphFormatting.LeftIndent + ADelta;
  end;
  Result := AOptions;
end;

procedure TdxTableTag.FunctionalTagProcess;
begin
  ParagraphFunctionalProcess;
end;

procedure TdxTableTag.OpenTagProcessCore;
var
  ANewTable: TdxTable;
begin
  if TablesDisabled then
    Exit;
  if ShouldIgnoreOpenTag then
  begin
    TableNotCreated := True;
    Exit;
  end;
  ClearParentProperties;
  ANewTable := Importer.TablesImportHelper.CreateTable(ObtainParentCell);
  Importer.Position.TableProperties.ApplyPropertiesToTable(ANewTable.TableProperties);

  if Importer.IsEmptyLine then
  begin
    Importer.SetAppendObjectProperty;
    ParagraphFunctionalProcess;
  end;
end;

function TdxTableTag.ShouldIgnoreOpenTag: Boolean;
var
  I: Integer;
  ATag: TdxTagBase;
  ATagName: TdxHtmlTagNameID;
begin
  I := Importer.TagsStack.Count - 2; //ignore current Table tag
  while I >= 0 do
  begin
    ATag := Importer.TagsStack[I].Tag;
    ATagName := ATag.Name;
    if ATagName in [TdxHtmlTagNameID.TD, TdxHtmlTagNameID.TR, TdxHtmlTagNameID.TH] then
      Exit(False);
    if ATagName = TdxHtmlTagNameID.Table then
      Exit(True);
    Dec(I);
  end;
  Result := False;
end;

procedure TdxTableTag.ClearParentProperties;
begin
  Importer.DocumentModel.ResetParagraphFormatting(Importer.Position.ParagraphFormatting);
  Importer.Position.ParagraphTabs.Clear;
  Importer.Position.DefaultAlignment.ResetDefaultAlignment;
  Importer.Position.TableCellRowSpan := 1;

  if Importer.Position.CharacterFormatting.Options.UseBackColor then
    Importer.Position.CharacterFormatting.BackColor := TdxAlphaColors.Empty;

  Importer.Position.AdditionalIndent := 0;
end;

procedure TdxTableTag.BeforeDeleteTagFromStack(AIndexOfDeletedTag: Integer);
var
  AIsIgnored: Boolean;
begin
  AIsIgnored := (TdxTableTag((Importer.TagsStack[AIndexOfDeletedTag].Tag))).TableNotCreated;
  if TablesDisabled or AIsIgnored then
    Exit;
  Importer.TablesImportHelper.FinalizeTableCreation;
end;

function TdxTableTag.ShouldAddParagraph: Boolean;
begin
  Result := not Importer.IsEmptyParagraph;
end;

procedure TdxTableTag.DeleteOldOpenTag;
var
  ACount, I: Integer;
  ATag: TdxTagBase;
begin
  if Tag.ElementType = TdxHtmlElementType.OpenTag then
    Exit;
  ACount := Importer.TagsStack.Count;
  for I := ACount - 1 downto 0 do
  begin
    ATag := Importer.TagsStack[I].Tag;
    if (ATag is TdxTdTag) or (ATag is TdxThTag) then
      CloseUnclosedTableCellTag(I)
    else
      if ATag is TdxTrTag then
        CloseUnclosedTableRowTag(I)
      else
        if ATag is TdxTableTag then
          Exit;
    Continue;
  end;
end;

procedure TdxTableTag.CloseUnclosedTableRowTag(AIndex: Integer);
var
  ATag: TdxTrTag;
begin
  ATag := TdxTrTag(Importer.TagsStack[AIndex].Tag);
  Importer.CloseUnClosedTag(ATag, AIndex);
end;

procedure TdxTableTag.CloseUnclosedTableCellTag(AIndex: Integer);
var
  ATdTag: TdxTdTag;
begin
  ATdTag := TdxTdTag(Importer.TagsStack[AIndex].Tag);
  Importer.CloseUnClosedTag(ATdTag, AIndex);
end;

function TdxTableTag.ObtainParentCell: TdxTableCell;
var
  I: Integer;
  ATag: TdxTagBase;
  ATrTag: TdxTrTag;
  ACellTag: TdxTdTag;
  ANewtag: TdxOpenHtmlTag;
begin
  Result := nil;
  if Importer.TablesImportHelper.Table = nil then
    Exit;
  if Importer.TablesImportHelper.Table.LastRow = nil then
    Exit;
  I := Importer.TagsStack.Count - 1;
  while I >= 0 do
  begin
    ATag := Importer.TagsStack[I].Tag;
    if (ATag is TdxTdTag) or (ATag is TdxThTag) then
      Exit(Importer.TablesImportHelper.Table.LastRow.LastCell);
    ATrTag := Safe<TdxTrTag>.Cast(ATag);
    if (ATrTag <> nil) and (Importer.TablesImportHelper.Table <> nil) then
    begin
      ACellTag := TdxTdTag.Create(Importer);
      ANewtag := TdxOpenHtmlTag.Create(ACellTag, Importer.PieceTable);
      ANewtag.OldPosition.CopyFrom(Importer.Position);
      FunctionalTagProcess;
      Importer.TagsStack.Insert(I + 1, ANewtag);
      ACellTag.OpenTagProcessCore;
      Result := Importer.TablesImportHelper.Table.LastRow.LastCell;

      if Importer.TablesImportHelper.Table.LastRow.Cells.Count = 1 then
      begin
        ACellTag.StartParagraphIndex := ATrTag.StartParagraphIndex;
        Result.StartParagraphIndex := ACellTag.StartParagraphIndex;
      end;
      Exit;
    end;
    Dec(I);
  end;
end;

{ TdxTableCellTag }

function TdxTableCellTag.GetTablesDisabled: Boolean;
begin
  Result := (not Importer.TablesImportHelper.TablesAllowed) or (not Importer.TablesImportHelper.IsInTable);
end;

procedure TdxTableCellTag.OpenTagProcessCore;
var
  ACell: TdxTableCell;
begin
  if TablesDisabled then
    Exit;
  ACell := CreateTableCell;
  FStartParagraphIndex := Importer.TablesImportHelper.FindStartParagraphIndexForCell(Importer.Position.ParagraphIndex);
  ACell.StartParagraphIndex := StartParagraphIndex;
end;

procedure TdxTableCellTag.BeforeDeleteTagFromStack(AIndexOfDeletedTag: Integer);
var
  AOpenTag: TdxTableCellTag;
  AStartParagraphIndex, AEndParagraphIndex: TdxParagraphIndex;
begin
  if TablesDisabled then
    Exit;

  AOpenTag := TdxTableCellTag(Importer.TagsStack[AIndexOfDeletedTag].Tag);
  AStartParagraphIndex := GetStartParagraphIndex(AOpenTag);
  AEndParagraphIndex := GetEndParagraphIndex(AOpenTag, AStartParagraphIndex);
  Importer.TablesImportHelper.InitializeTableCell(Importer.TablesImportHelper.Table.LastRow.LastCell,
    AStartParagraphIndex, AEndParagraphIndex);
end;

function TdxTableCellTag.GetStartParagraphIndex(AOpenTag: TdxTableCellTag): TdxParagraphIndex;
begin
  Result := AOpenTag.StartParagraphIndex;
end;

function TdxTableCellTag.GetEndParagraphIndex(AOpenTag: TdxTableCellTag;
  AStartParagraphIndex: TdxParagraphIndex): TdxParagraphIndex;
begin
  if Importer.IsEmptyParagraph then
    Result := GetEndParagraphIndexEmptyLastParagraphCase(AOpenTag, AStartParagraphIndex)
  else
    Result := GetEndParagraphIndexNonEmptyLastParagraphCase;
end;

function TdxTableCellTag.GetEndParagraphIndexNonEmptyLastParagraphCase: TdxParagraphIndex;
begin
  Result := Importer.Position.ParagraphIndex;
end;

function TdxTableCellTag.GetEndParagraphIndexEmptyLastParagraphCase(AOpenTag: TdxTableCellTag;
  AStartParagraphIndex: TdxParagraphIndex): TdxParagraphIndex;
var
  AEndParagraphIndex: TdxParagraphIndex;
  AIndex: Integer;
begin
  AIndex := Importer.PieceTable.Paragraphs.Count - 2;
  if AStartParagraphIndex = AIndex then
    AEndParagraphIndex := GetEndParagraphIndexForEmptyCell(AStartParagraphIndex)
  else
    AEndParagraphIndex := GetEndParagraphIndexForCellWithContent(AOpenTag);
  Result := AEndParagraphIndex;
end;

function TdxTableCellTag.GetEndParagraphIndexForCellWithContent(AOpenTag: TdxTableCellTag): TdxParagraphIndex;
var
  AEndParagraphIndex, AParagraphIndex: TdxParagraphIndex;
begin
  AParagraphIndex := Importer.Position.ParagraphIndex;
  if DocumentModel.ActivePieceTable.Paragraphs[AParagraphIndex].IsEmpty then
    AEndParagraphIndex := AParagraphIndex - 1
  else
  begin
    AEndParagraphIndex := AParagraphIndex;

    Importer.SetAppendObjectProperty;
  end;
  Result := AEndParagraphIndex;
end;

function TdxTableCellTag.GetEndParagraphIndexForEmptyCell(AStartParagraphIndex: TdxParagraphIndex): TdxParagraphIndex;
var
  AEndParagraphIndex: TdxParagraphIndex;
begin
  Importer.SetAppendObjectProperty;
  AEndParagraphIndex := AStartParagraphIndex;
  Result := AEndParagraphIndex;
end;

{ TdxTdTag }

class constructor TdxTdTag.Initialize;
begin
  FAttributeTable := AddAttributes;
end;

class destructor TdxTdTag.Finalize;
begin
  FAttributeTable.Free;
end;

constructor TdxTdTag.Create(AImporter: TdxCustomHtmlImporter);
begin
  inherited Create(AImporter);

  FCellProperties := TdxTableCellProperties.Create(Importer.PieceTable, Self);
  FTableCellRowSpan := 1;
  FTdTagWithoutOpenedTable := False;
  FAlignment := TdxHtmlParagraphAlignment.Create;
end;

destructor TdxTdTag.Destroy;
begin
  FreeAndNil(FCellProperties);
  FreeAndNil(FAlignment);
  inherited Destroy;
end;

class function TdxTdTag.AddAttributes: TdxAttributeKeywordTranslatorTable;
begin
  Result := CreateAttributeTable;
  Result.Add(ConvertKeyToUpper('headers'), HeadersKeyword);
  Result.Add(ConvertKeyToUpper('scope'), ScopeKeyword);
  Result.Add(ConvertKeyToUpper('abbr'), AbbrKeyword);
  Result.Add(ConvertKeyToUpper('axis'), AxisKeyword);
  Result.Add(ConvertKeyToUpper('rowspan'), RowspanKeyword);
  Result.Add(ConvertKeyToUpper('colspan'), ColspanKeyword);
  Result.Add(ConvertKeyToUpper('nowrap'), NowrapKeyword);
  Result.Add(ConvertKeyToUpper('width'), WidthKeyword);
  Result.Add(ConvertKeyToUpper('height'), HeightKeyword);
  Result.Add(ConvertKeyToUpper('align'), AlignmentKeyword);
  Result.Add(ConvertKeyToUpper('valign'), VerticalAlignmentKeyword);
  Result.Add(ConvertKeyToUpper('bordercolor'), BorderColorKeyword);
  Result.Add(ConvertKeyToUpper('bgcolor'), BackgroundColorKeyword);
end;

class procedure TdxTdTag.HeadersKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase);
begin
end;

class procedure TdxTdTag.ScopeKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase);
begin
end;

class procedure TdxTdTag.AbbrKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase);
begin
end;

class procedure TdxTdTag.AxisKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase);
begin
end;

class procedure TdxTdTag.RowspanKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase);
var
  ARowsSpanned: Integer;
begin
  if TdxNumber.TryParse(AValue, ARowsSpanned) and (ARowsSpanned > 1) then
    TdxHtmlImporter(AImporter).Position.TableCellRowSpan := ARowsSpanned;
end;

class procedure TdxTdTag.ColspanKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase);
var
  ATdTag: TdxTdTag absolute ATag;
  AColumns: Integer;
begin
  if TdxNumber.TryParse(AValue, AColumns) and (AColumns > 1) then
    ATdTag.CellProperties.ColumnSpan := AColumns;
end;

class procedure TdxTdTag.NowrapKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase);
var
  ATdTag: TdxTdTag absolute ATag;
begin
  ATdTag.CellProperties.NoWrap := True;
end;

class procedure TdxTdTag.WidthKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase);
var
  ATdTag: TdxTdTag absolute ATag;
  AWidth: TdxWidthUnitInfo;
  ACellWidth: TdxPreferredWidth;
begin
  AWidth := ConvertPixelsValueToWidthUnitInfo(AImporter.DocumentModel.UnitConverter, AValue);
  try
    if AWidth.&Type <> TdxWidthUnitType.&Nil then
    begin
      ACellWidth := ATdTag.CellProperties.PreferredWidth;
      ACellWidth.Value := AWidth.Value;
      ACellWidth.&Type := AWidth.&Type;
    end;
  finally
    AWidth.Free;
  end;
end;

function TdxTdTag._AddRef: Integer;
begin
  Result := -1;
end;

function TdxTdTag._Release: Integer;
begin
  Result := -1;
end;

class function TdxTdTag.FindRowTag(AImporter: TdxCustomHtmlImporter): TdxTrTag;
var
  ATagStack: TdxList<TdxOpenHtmlTag>;
  AStart, I: Integer;
  ATag: TdxTagBase;
  ATrTag: TdxTrTag;
begin
  ATagStack := TdxHtmlImporter(AImporter).TagsStack;
  AStart := ATagStack.Count - 2;
  for I := AStart downto 0 + 1 do
  begin
    ATag := ATagStack[I].Tag;
    ATrTag := Safe<TdxTrTag>.Cast(ATag);
    if ATrTag <> nil then
      Exit(ATrTag);
  end;
  Result := nil;
end;

class procedure TdxTdTag.HeightKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase);
var
  ATrTag: TdxTrTag;
  AResult: Integer;
  AParsedValue: TdxLengthValueParser;
  AConverter: TdxDocumentModelUnitConverter;
  AHeight: TdxHeightUnitInfo;
begin
  ATrTag := FindRowTag(AImporter);
  if ATrTag = nil then
    Exit;

  AParsedValue := TdxLengthValueParser.Create(AValue + 'px', AImporter.DocumentModel.ScreenDpi);
  if AParsedValue.IsDigit and not AParsedValue.IsRelativeUnit then
  begin
    AConverter := AImporter.DocumentModel.UnitConverter;
    AResult := Max(0, Round(AConverter.PointsToModelUnitsF(AParsedValue.PointsValue)));

    AHeight := TdxHeightUnitInfo.Create;
    try
      AHeight.&Type := TdxHeightUnitType.Minimum;
      AHeight.Value := AResult;
      ATrTag.RowProperties.Height := AHeight;
      TdxHtmlImporter(AImporter).Position.RowProperties.Height := AHeight;
    finally
      AHeight.Free;
    end;
  end;
end;

class procedure TdxTdTag.AlignmentKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase);
var
  ATdTag: TdxTdTag;
  ATargetValue: TdxParagraphAlignment;
begin
  ATdTag := TdxTdTag(ATag);
  ATargetValue := TdxParagraphAlignment.Left;
  if ReadParagraphAlignment(AValue, ATargetValue) then
    ATdTag.Alignment.AlignmentValue := ATargetValue;
end;

class procedure TdxTdTag.VerticalAlignmentKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase);
var
  ATdTag: TdxTdTag;
begin
  ATdTag := TdxTdTag(ATag);
  ATdTag.CellProperties.VerticalAlignment := ReadVerticalAlignment(AValue);
end;

class procedure TdxTdTag.BorderColorKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase);
var
  ATdTag: TdxTdTag;
begin
  ATdTag := TdxTdTag(ATag);
  SetColorToAllBorders(TdxMarkupLanguageColorParser.ParseColor(AValue), ATdTag.CellProperties);
end;

class procedure TdxTdTag.SetColorToAllBorders(AColor: TdxAlphaColor; ACellProperties: TdxTableCellProperties);
var
  ABorders: TdxTableCellBorders;
begin
  ABorders := ACellProperties.Borders;
  ABorders.TopBorder.Color := AColor;
  ABorders.BottomBorder.Color := AColor;
  ABorders.RightBorder.Color := AColor;
  ABorders.LeftBorder.Color := AColor;
end;

class procedure TdxTdTag.ImportBorderWidth(AUnitConverter: TdxDocumentModelUnitConverter; const AValue: string;
  ABottomBorder: TdxHtmlBorderProperty);
var
  AWidth: TdxWidthUnitInfo;
begin
  AWidth := ImportBorderWidthCore(AUnitConverter, AValue);
  try
    if AWidth.&Type <> TdxWidthUnitType.&Nil then
      ABottomBorder.Width := AWidth.Value;
  finally
    AWidth.Free;
  end;
end;

class procedure TdxTdTag.ImportBorderColor(AConverter: TdxDocumentModelUnitConverter; const AValue: string;
  ABorder: TdxHtmlBorderProperty);
var
  AColor: TdxAlphaColor;
begin
  if AValue = '' then
    Exit;
  AColor := ImportColor(AConverter, AValue);
  if AColor <> TdxAlphaColors.Empty then
    ABorder.Color := AColor;
end;

class procedure TdxTdTag.BackgroundColorKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase);
var
  ATdTag: TdxTdTag;
begin
  ATdTag := TdxTdTag(ATag);
  ATdTag.CellProperties.BackgroundColor := TdxMarkupLanguageColorParser.ParseColor(AValue);
end;

class procedure TdxTdTag.BackgroundImageKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase);
begin
end;

function TdxTdTag.GetAttributeTable: TdxAttributeKeywordTranslatorTable;
begin
  Result := FAttributeTable;
end;

procedure TdxTdTag.ApplyTagProperties;
begin
  Importer.Position.CellProperties.CopyFrom(CellProperties);
  if Alignment.UseAlignment then
  begin
    Importer.Position.DefaultAlignment.AlignmentValue := Alignment.AlignmentValue;
    Importer.Position.ParagraphFormatting.Alignment := Alignment.AlignmentValue;
  end;
end;

function TdxTdTag.ApplyCssProperties: TdxParagraphFormattingOptions;
begin
  Result := inherited ApplyCssProperties;
end;

procedure TdxTdTag.FunctionalTagProcess;
begin
  ParagraphFunctionalProcess;
end;

procedure TdxTdTag.OpenTagProcessCore;
begin
  if TablesDisabled then
  begin
    TdTagWithoutOpenedTable := True;
    Exit;
  end;

  inherited OpenTagProcessCore;

  FTablesCountOnOpen := Importer.PieceTable.Tables.Count;
  FTableCellRowSpan := Importer.Position.TableCellRowSpan;
  Importer.IsEmptyParagraph := True;
end;

function TdxTdTag.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

function TdxTdTag.CreateTableCell: TdxTableCell;
var
  AHelper: TdxHtmlTablesImportHelper;
  ACell: TdxTableCell;
begin
  AHelper := Importer.TablesImportHelper;
  if AHelper.Table.LastRow = nil then
    AHelper.CreateNewRow;

  ACell := TdxTableCell.Create(AHelper.Table.LastRow);
  ACell.Row.Cells.AddInternal(ACell);
  ApplyAllCellProperties(ACell);

  ApplyHeightToTableRow(ACell.Row, Importer.Position.TableCellRowSpan);
  if AHelper.Table.Rows.Count = AHelper.TableInfo.TableCaption.Count + 1 then
  begin
    AHelper.AddCellToSpanCollection(Importer.Position.TableCellRowSpan, Importer.Position.CellProperties.ColumnSpan);
    AHelper.TableInfo.CaptionColSpan := AHelper.TableInfo.CaptionColSpan + Importer.Position.CellProperties.ColumnSpan;
  end
  else
    if AHelper.TableInfo.ColumnIndex + Importer.Position.CellProperties.ColumnSpan > AHelper.TableInfo.CellsRowSpanCollection.Count then
    begin
      AHelper.ExpandSpanCollection(Importer.Position.TableCellRowSpan, Importer.Position.CellProperties.ColumnSpan);
    end;
  Result := ACell;
end;

procedure TdxTdTag.ApplyHeightToTableRow(ATableRow: TdxTableRow; ARowSpan: Integer);
var
  ATrTag: TdxTrTag;
begin
  ATrTag := TdxTdTag.FindRowTag(Importer);
  if (ATrTag = nil) or (ARowSpan > 1) then
    Exit;

  Importer.Position.RowProperties.ApplyPropertiesToRow(ATableRow.Properties);
end;

procedure TdxTdTag.BeforeDeleteTagFromStack(AIndexOfDeletedTag: Integer);
var
  AOpenTag: TdxTdTag;
  ACount, I: Integer;
begin
  if (TablesDisabled) or (Importer.TablesImportHelper.Table.LastRow = nil) then
    Exit;
  AOpenTag := TdxTdTag(Importer.TagsStack[AIndexOfDeletedTag].Tag);
  if AOpenTag.tdTagWithoutOpenedTable then
    Exit;

  inherited BeforeDeleteTagFromStack(AIndexOfDeletedTag);
  Importer.TablesImportHelper.UpdateFirstRowSpanCollection(AOpenTag.TableCellRowSpan, AOpenTag.CellProperties.ColumnSpan);
  ACount := Importer.TagsStack.Count;
  for I := ACount - 1 downto AIndexOfDeletedTag + 1 do
    Importer.CloseUnClosedTag(Importer.TagsStack[I].Tag, I);
end;

procedure TdxTdTag.ApplyAllCellProperties(ACell: TdxTableCell);
begin
  ACell.Properties.BeginInit;
  try

    ACell.Properties.CopyFrom(Importer.Position.CellProperties);
    if Importer.Position.TableCellRowSpan > 1 then
      ACell.Properties.VerticalMerging := TdxMergingState.Restart;
    if not ACell.Properties.UseBackgroundColor then
    begin
      Importer.Position.TableProperties.ApplyBackgroundColorToCell(ACell.Properties);
      Importer.Position.RowProperties.ApplyBackgroundColorToCell(ACell.Properties);
    end;
    if not ACell.Properties.UseVerticalAlignment then
      Importer.Position.RowProperties.ApplyVerticalAlignmentToCell(ACell.Properties);
    if not ACell.Properties.UseVerticalAlignment then
      ACell.Properties.VerticalAlignment := TdxVerticalAlignment.Center;
    if not ACell.Properties.UsePreferredWidth then
      ACell.Properties.PreferredWidth.&Type := TdxWidthUnitType.Auto;
  finally
    ACell.Properties.EndInit;
  end;
end;

function TdxTdTag.GetEndParagraphIndexForCellWithContent(AOpenTag: TdxTableCellTag): TdxParagraphIndex;
var
  ATdOpenTag: TdxTdTag;
  ATablesCountOnClose: Integer;
begin
  ATdOpenTag := Safe<TdxTdTag>.Cast(AOpenTag);
  ATablesCountOnClose := Importer.PieceTable.Tables.Count;
  if (ATdOpenTag <> nil) and (ATdOpenTag.TablesCountOnOpen < ATablesCountOnClose) then
    Result := GetParagraphIndexForCellContainsNestedTable
  else
    Result := inherited GetEndParagraphIndexForCellWithContent(AOpenTag);
end;

function TdxTdTag.GetParagraphIndexForCellContainsNestedTable: TdxParagraphIndex;
var
  AEndParagraphIndex: TdxParagraphIndex;
begin
  AEndParagraphIndex := Importer.Position.ParagraphIndex - 1;
  if (AEndParagraphIndex < 0) or (Importer.PieceTable.Paragraphs[AEndParagraphIndex].IsInCell) then
  begin
    Inc(AEndParagraphIndex);
    Importer.SetAppendObjectProperty;
  end;

  Result := AEndParagraphIndex;
end;

procedure TdxTdTag.DeleteOldOpenTag;
var
  I: Integer;
  ATag: TdxTagBase;
  ATdTag: TdxTdTag;
begin
  if Tag.ElementType = TdxHtmlElementType.CloseTag then
  begin
    for I := Importer.TagsStack.Count - 1 downto 0 do
    begin
      ATag := Importer.TagsStack[I].Tag;
      if ATag is TdxTableTag then
      begin
        Importer.OpenTagIsFoundAndRemoved(Importer.TagsStack[I].Tag);
        Exit;
      end;
      if ATag is TdxTrTag then
        Exit;
      if (ATag is TdxTdTag) or (ATag is TdxThTag) then
        Exit;
    end;
    Exit;
  end;
  for I := Importer.TagsStack.Count - 1 downto 0 do
  begin
    ATag := Importer.TagsStack[I].Tag;

    if ATag is TdxTableTag then
      Exit; // do not close open cell from parent table
    if ATag is TdxTrTag then
      Exit;
    if (ATag is TdxTdTag) or (ATag is TdxThTag) then
    begin
      ATdTag := TdxTdTag(Importer.TagsStack[I].Tag);
      Importer.CloseUnClosedTag(ATdTag, I);
      Exit;
    end;
  end;
end;

function TdxTdTag.GetStartIndexAllowedSearchScope: Integer;
var
  ACount, I: Integer;
  ATag: TdxTagBase;
begin
  ACount := Importer.TagsStack.Count;
  for I := ACount - 1 downto 0 do
  begin
    ATag := Importer.TagsStack[I].Tag;
    if ATag is TdxTrTag then
      Exit(I);
  end;
  Result := inherited GetStartIndexAllowedSearchScope;
end;

function TdxTdTag.CreateCellPropertiesChangedHistoryItem(AProperties: TdxTableCellProperties): TdxIndexChangedHistoryItemCore;
begin
  Assert(AProperties = CellProperties);
  Result := TdxIndexChangedHistoryItem.Create(TdxPieceTable(AProperties.PieceTable), AProperties);
end;

{ TdxTrTag }

constructor TdxTrTag.Create(AImporter: TdxCustomHtmlImporter);
begin
  inherited Create(AImporter);
  FRowProperties := TdxHtmlTableRowProperties.Create;
  FStartParagraphIndex := 0;
  FAlignment := TdxHtmlParagraphAlignment.Create;
end;

destructor TdxTrTag.Destroy;
begin
  FRowProperties.Free;
  FAlignment.Free;
  inherited Destroy;
end;

class constructor TdxTrTag.Initialize;
begin
  FAttributeTable := AddAttributes;
end;

class destructor TdxTrTag.Finalize;
begin
  FAttributeTable.Free;
end;

class function TdxTrTag.AddAttributes: TdxAttributeKeywordTranslatorTable;
begin
  Result := CreateAttributeTable;
  Result.Add(ConvertKeyToUpper('align'), AlignmentKeyword);
  Result.Add(ConvertKeyToUpper('bgcolor'), BackgroundCellColorKeyword);
  Result.Add(ConvertKeyToUpper('bordercolor'), BorderColorKeyword);
  Result.Add(ConvertKeyToUpper('valign'), VerticalAlignmentKeyword);
  Result.Add(ConvertKeyToUpper('height'), HeightKeyword);
end;

class procedure TdxTrTag.AlignmentKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase);
var
  ATrTag: TdxTrTag;
  AResultAlignment: TdxParagraphAlignment;
begin
  ATrTag := TdxTrTag(ATag);
  AResultAlignment := TdxParagraphAlignment.Left;
  if ReadParagraphAlignment(AValue, AResultAlignment) then
    ATrTag.Alignment.AlignmentValue := AResultAlignment;
end;

class procedure TdxTrTag.BackgroundCellColorKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase);
var
  ATrTag: TdxTrTag;
begin
  ATrTag := TdxTrTag(ATag);

  ATrTag.RowProperties.BackgroundColor := TdxMarkupLanguageColorParser.ParseColor(AValue);
end;

class procedure TdxTrTag.VerticalAlignmentKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase);
var
  ATdTag: TdxTrTag;
begin
  ATdTag := TdxTrTag(ATag);
  ATdTag.RowProperties.VerticalAlignment := ReadVerticalAlignment(AValue);
end;

class procedure TdxTrTag.BorderColorKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase);
begin
end;

class procedure TdxTrTag.HeightKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase);
var
  ATrTag: TdxTrTag;
  AValueType: string;
  APar: TdxLengthValueParser;
  ANewHeight, AHeight: TdxHeightUnitInfo;
begin
  ATrTag := TdxTrTag(ATag);

  if AValue = '' then
    Exit;
  AValueType := AValue + 'px';
  APar := TdxLengthValueParser.Create(AValueType, AImporter.DocumentModel.ScreenDpi);
  if (not APar.IsDigit) or (APar.PointsValue <= 0) then
    Exit;
  ANewHeight := TdxHeightUnitInfo.Create;
  try
    ANewHeight.Value := Round(ATag.DocumentModel.UnitConverter.PointsToModelUnitsF(APar.PointsValue));
    ANewHeight.&Type := TdxHeightUnitType.Minimum;
    AHeight := ATrTag.RowProperties.Height;
    if AHeight.Value < ANewHeight.Value then
      ATrTag.RowProperties.Height := ANewHeight;
  finally
    ANewHeight.Free;
  end;
end;

function TdxTrTag.GetAttributeTable: TdxAttributeKeywordTranslatorTable;
begin
  Result := FAttributeTable;
end;

function TdxTrTag.GetTablesDisabled: Boolean;
begin
  Result := (not Importer.TablesImportHelper.TablesAllowed) or (not Importer.TablesImportHelper.IsInTable);
end;

procedure TdxTrTag.ApplyTagProperties;
begin
  Importer.Position.RowProperties.CopyFrom(RowProperties);

  if Alignment.UseAlignment then
  begin
    Importer.Position.DefaultAlignment.AlignmentValue := Alignment.AlignmentValue;
    Importer.Position.ParagraphFormatting.Alignment := Alignment.AlignmentValue;
  end;

  RowProperties.ApplyPropertiesToCharacter(Importer.Position.CharacterFormatting);
end;

procedure TdxTrTag.FunctionalTagProcess;
begin
  ParagraphFunctionalProcess;
end;

procedure TdxTrTag.DeleteOldOpenTag;
var
  ACount, I: Integer;
  ATag: TdxTagBase;
  ATdTag: TdxTdTag;
  ATrTag: TdxTrTag;
begin
  if Tag.ElementType = TdxHtmlElementType.CloseTag then
    Exit;
  ACount := Importer.TagsStack.Count;
  for I := ACount - 1 downto 0 do
  begin

    ATag := Importer.TagsStack[I].Tag;
    if ATag is TdxTableTag then
      Exit;
    if (ATag is TdxThTag) or (ATag is TdxTdTag) then
    begin
      ATdTag := TdxTdTag(Importer.TagsStack[I].Tag);
      Importer.CloseUnClosedTag(ATdTag, I);
      Continue;
    end;
    if ATag is TdxTrTag then
    begin
      ATrTag := TdxTrTag(Importer.TagsStack[I].Tag);
      Importer.CloseUnClosedTag(ATrTag, I);
      Exit;
    end;
  end;
end;

procedure TdxTrTag.OpenTagProcessCore;
var
  ARow: TdxTableRow;
begin
  if TablesDisabled then
    Exit;
  StartParagraphIndex := Importer.Position.ParagraphIndex;
  ARow := Importer.TablesImportHelper.CreateNewRowOrGetLastEmpty;
  Importer.Position.RowProperties.ApplyPropertiesToRow(ARow.Properties);
end;

procedure TdxTrTag.BeforeDeleteTagFromStack(AIndexOfDeletedTag: Integer);
var
  ALastRow: TdxTableRow;
  ATrTag: TdxTrTag;
begin
  if TablesDisabled then
    Exit;
  ALastRow := Importer.TablesImportHelper.Table.LastRow;
  if ALastRow = nil then
    Exit;
  ATrTag := (TdxTrTag(Importer.TagsStack[AIndexOfDeletedTag].Tag));
  Importer.Position.RowProperties.CopyFrom(ATrTag.RowProperties);

  CreateCellWhenAfterOpenTrTagWasContent(ATrTag, ALastRow);
  AdaptParagraphMarkHeightToRowHeight;
end;

procedure TdxTrTag.CreateCellWhenAfterOpenTrTagWasContent(ATrTag: TdxTrTag; ALastRow: TdxTableRow);
var
  ACell: TdxTableCell;
  AStartParagraphIndex: TdxParagraphIndex;
begin
  if not Importer.IsEmptyParagraph and (ALastRow.Cells.Count = 0) then
  begin

    ACell := TdxTableCell.Create(ALastRow);
    ACell.Row.Cells.AddInternal(ACell);
    AStartParagraphIndex := Importer.TablesImportHelper.FindStartParagraphIndexForCell(Importer.Position.ParagraphIndex);
    Importer.TablesImportHelper.InitializeTableCell(Importer.TablesImportHelper.Table.LastRow.LastCell,
      AStartParagraphIndex, Importer.Position.ParagraphIndex);
  end;
end;

procedure TdxTrTag.AdaptParagraphMarkHeightToRowHeight;
var
  ARow: TdxTableRow;
  ASizeInPoints, ACellsCount, ACellId: Integer;
  ACells: TdxTableCellCollection;
  ACell: TdxTableCell;
  AParagraph: TdxParagraph;
  ATextRun: TdxTextRunBase;
begin
  ARow := Importer.TablesImportHelper.Table.LastRow;
  if ARow.Properties.Height.&Type <> TdxHeightUnitType.Minimum then
    Exit;

  ASizeInPoints := Max(Trunc(DocumentModel.UnitConverter.ModelUnitsToPointsFRound(ARow.Properties.Height.Value)), 1);
  ACells := ARow.Cells;
  ACellsCount := ACells.Count;
  for ACellId := 0 to ACellsCount - 1 do
  begin
    ACell := ACells[ACellId];
    AParagraph := Importer.DocumentModel.MainPieceTable.Paragraphs[ACell.StartParagraphIndex];
    if (ACell.StartParagraphIndex = ACell.EndParagraphIndex) and (AParagraph.FirstRunIndex = AParagraph.LastRunIndex) then
    begin
      ATextRun := Importer.DocumentModel.MainPieceTable.Runs[AParagraph.FirstRunIndex];
      ATextRun.DoubleFontSize := Min(ATextRun.DoubleFontSize, ASizeInPoints * 2);
    end;
  end;
end;

{ TdxThTag }

class constructor TdxThTag.Initialize;
begin
  FAttributeTable := AddAttributes;
end;

class destructor TdxThTag.Finalize;
begin
  FAttributeTable.Free;
end;

class function TdxThTag.AddAttributes: TdxAttributeKeywordTranslatorTable;
begin
  Result := CreateAttributeTable;
  Result.Add(ConvertKeyToUpper('headers'), HeadersKeyword);
  Result.Add(ConvertKeyToUpper('scope'), ScopeKeyword);
  Result.Add(ConvertKeyToUpper('abbr'), AbbrKeyword);
  Result.Add(ConvertKeyToUpper('axis'), AxisKeyword);
  Result.Add(ConvertKeyToUpper('rowspan'), RowspanKeyword);
  Result.Add(ConvertKeyToUpper('colspan'), ColspanKeyword);
  Result.Add(ConvertKeyToUpper('nowrap'), NowrapKeyword);
  Result.Add(ConvertKeyToUpper('width'), WidthKeyword);
  Result.Add(ConvertKeyToUpper('height'), HeightKeyword);
  Result.Add(ConvertKeyToUpper('align'), AlignmentKeyword);
  Result.Add(ConvertKeyToUpper('valign'), VerticalAlignmentKeyword);
  Result.Add(ConvertKeyToUpper('bordercolor'), BorderColorKeyword);
  Result.Add(ConvertKeyToUpper('bgcolor'), BackgroundColorKeyword);
  Result.Add(ConvertKeyToUpper('background'), BackgroundImageKeyword);
end;

function TdxThTag.GetAttributeTable: TdxAttributeKeywordTranslatorTable;
begin
  Result := FAttributeTable;
end;

procedure TdxThTag.ApplyTagProperties;
begin
  inherited ApplyTagProperties;
  Importer.Position.CharacterFormatting.FontBold := True;
  Importer.Position.ParagraphFormatting.Alignment := TdxParagraphAlignment.Center;
  Importer.Position.DefaultAlignment.AlignmentValue := TdxParagraphAlignment.Center;
end;

{ TdxCaptionTag }

class constructor TdxCaptionTag.Initialize;
begin
  FAttributeTable := AddAttributes;
end;

class destructor TdxCaptionTag.Finalize;
begin
  FAttributeTable.Free;
end;

class function TdxCaptionTag.AddAttributes: TdxAttributeKeywordTranslatorTable;
begin
  Result := CreateAttributeTable;
  Result.Add(ConvertKeyToUpper('align'), AlignmentKeyword);
end;

class procedure TdxCaptionTag.AlignmentKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase);
begin
  // ignore it for the while
end;

function TdxCaptionTag.GetAttributeTable: TdxAttributeKeywordTranslatorTable;
begin
  Result := FAttributeTable;
end;

procedure TdxCaptionTag.ApplyTagProperties;
begin
  Importer.Position.CharacterFormatting.FontBold := True;
  Importer.Position.ParagraphFormatting.Alignment := TdxParagraphAlignment.Center;
end;

function TdxCaptionTag.CreateTableCell: TdxTableCell;
var
  AHelper: TdxHtmlTablesImportHelper;
  ARow: TdxTableRow;
  ACell: TdxTableCell;
  ABorders: TdxTableCellBorders;
begin
  AHelper := Importer.TablesImportHelper;
  ARow := AHelper.CreateNewRowOrGetLastEmpty;
  Importer.Position.RowProperties.ApplyPropertiesToRow(ARow.Properties);

  ARow.Cells.AddInternal(TdxTableCell.Create(ARow));
  ACell := ARow.LastCell;

  ABorders := ACell.Properties.Borders;
  ACell.Properties.BeginInit;
  ABorders.TopBorder.Style := TdxBorderLineStyle.None;
  ABorders.BottomBorder.Style := TdxBorderLineStyle.None;
  ABorders.LeftBorder.Style := TdxBorderLineStyle.None;
  ABorders.RightBorder.Style := TdxBorderLineStyle.None;
  ACell.Properties.EndInit;

  if AHelper.TableInfo.CaptionColSpan > 0 then
    ACell.ColumnSpan := AHelper.TableInfo.CaptionColSpan;
  AHelper.TableInfo.TableCaption.Add(ACell);
  Result := ACell;
end;

procedure TdxCaptionTag.FunctionalTagProcess;
begin
  ParagraphFunctionalProcess;
end;

{ TBodyTag }

procedure TBodyTag.ApplyTagProperties;
begin
end;

{ THeadTag }

procedure THeadTag.ApplyTagProperties;
begin
end;

{ TFootTag }

procedure TFootTag.ApplyTagProperties;
begin
end;

end.
