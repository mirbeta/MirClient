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

unit dxRichEdit.Import.Html.TagBase;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, SysUtils, Classes, Generics.Defaults, Generics.Collections, dxCore, dxCoreGraphics,
  dxGenerics,
  dxRichEdit.Utils.Types,
  dxRichEdit.NativeApi,
  dxRichEdit.Platform.Font,
  dxRichEdit.Options,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.DocumentModel.Borders,
  dxRichEdit.DocumentModel.History.IndexChangedHistoryItem,
  dxRichEdit.DocumentModel.CharacterFormatting,
  dxRichEdit.DocumentModel.ParagraphFormatting,
  dxRichEdit.DocumentModel.TabFormatting,
  dxRichEdit.DocumentModel.TableFormatting,
  dxRichEdit.DocumentModel.UnitConverter,
  dxRichEdit.Import.Core,
  dxRichEdit.Import,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.Import.CSSSelectorParser,
  dxRichEdit.Import.CSSParser,
  dxRichEdit.Import.Html.CSSParser,
  dxRichEdit.Import.Html.HTMLParser,
  dxUriRecord;

type
  { TdxCustomHtmlImporter }

  TdxCustomHtmlImporter = class(TdxDocumentModelImporter)
  strict private
    function GetDocumentModel: TdxDocumentModel; inline;
    function GetPieceTable: TdxPieceTable; inline;
  public
    property DocumentModel: TdxDocumentModel read GetDocumentModel;
    property PieceTable: TdxPieceTable read GetPieceTable;
  end;

  TdxTagBase = class;

  { TdxHtmlFontSize }

  TdxHtmlFontSize = record
  strict private const
    DefaultHtmlFontSize = 3;
    HtmlToDocumentFontSizeTable: array[1..7] of Single = (
      7.5,
      10,
      12,
      13.5,
      18,
      24,
      36);
  strict private
    FFontSize: Integer;
  public
    class function Create: TdxHtmlFontSize; static;

    function GetFontSize(AHtmlFontSize: Integer): Single;
    function GetDoubleFontSize(AHtmlFontSize: Integer): Integer;
    function GetLargerFontSize(ADoubleFontSize: Integer): Single; overload;
    function GetLargerFontSize: Single; overload;
    function GetLargerDoubleFontSize(ADoubleFontSize: Integer): Single; overload;
    function GetLargerDoubleFontSize: Integer; overload;
    function GetSmallerFontSize(ADoubleFontSize: Integer): Single; overload;
    function GetSmallerFontSize: Single; overload;
    function GetSmallerDoubleFontSize(ADoubleFontSize: Integer): Integer; overload;
    function GetSmallerDoubleFontSize: Integer; overload;
  end;

  { TdxHtmlParagraphAlignment }

  TdxHtmlParagraphAlignment = class
  strict private
    FAlignmentValue: TdxParagraphAlignment;
    FUseAlignment: Boolean;
    procedure SetAlignmentValue(const AValue: TdxParagraphAlignment);
  public
    constructor Create;
    procedure ResetDefaultAlignment;
    procedure CopyFrom(AValue: TdxHtmlParagraphAlignment);

    property AlignmentValue: TdxParagraphAlignment read FAlignmentValue write SetAlignmentValue;
    property UseAlignment: Boolean read FUseAlignment;
  end;


  { TdxHtmlInputPosition }

  TdxHtmlInputPosition = class(TdxInputPosition, IdxCellPropertiesOwner)
  strict private
    FParagraphFormatting: TdxParagraphFormattingBase;
    FListLevelProperties: TdxHtmlListLevelProperties;
    FLevelIndex: Integer;
    FAdditionalIndent: Integer;
    FDefaultAlignment: TdxHtmlParagraphAlignment;
    FTableProperties: TdxHtmlTableProperties;
    FCellProperties: TdxTableCellProperties;
    FRowProperties: TdxHtmlTableRowProperties;
    FParagraphTabs: TdxTabFormattingInfo;
    FTableCellRowSpan: Integer;
    FNextTableRowHeight: Integer;
    FImageProperties: TdxHtmlImageProperties;
    FCssFloat: TdxHtmlCssFloat;
    procedure SetLevelIndex(const AValue: Integer);
    procedure SetTableCellRowSpan(const AValue: Integer);
  protected
    // IUnknown
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function CreateCellPropertiesChangedHistoryItem(AProperties: TdxTableCellProperties):
      TdxIndexChangedHistoryItemCore;
  public
    constructor Create(APieceTable: TdxSimplePieceTable); override;
    destructor Destroy; override;
    procedure CopyFrom(AValue: TdxHtmlInputPosition);
    procedure CopyParagraphFormattingFrom(AValue: TdxHtmlInputPosition);

    property ParagraphFormatting: TdxParagraphFormattingBase read FParagraphFormatting;
    property DefaultAlignment: TdxHtmlParagraphAlignment read FDefaultAlignment write FDefaultAlignment;
    property ListLevelProperties: TdxHtmlListLevelProperties read FListLevelProperties;
    property AdditionalIndent: Integer read FAdditionalIndent write FAdditionalIndent;
    property NextTableRowHeight: Integer read FNextTableRowHeight write FNextTableRowHeight;
    property ImageProperties: TdxHtmlImageProperties read FImageProperties write FImageProperties;
    property CssFloat: TdxHtmlCssFloat read FCssFloat write FCssFloat;
    property LevelIndex: Integer read FLevelIndex write SetLevelIndex;
    property TableCellRowSpan: Integer read FTableCellRowSpan write SetTableCellRowSpan;
    property TableProperties: TdxHtmlTableProperties read FTableProperties;
    property CellProperties: TdxTableCellProperties read FCellProperties;
    property RowProperties: TdxHtmlTableRowProperties read FRowProperties;
    property ParagraphTabs: TdxTabFormattingInfo read FParagraphTabs;
  end;

  { TdxOpenHtmlTag }

  TdxOpenHtmlTag = class
  strict private
    FTag: TdxTagBase;
    FOldPosition: TdxHtmlInputPosition;
  public
    constructor Create(ATag: TdxTagBase; APieceTable: TdxPieceTable);
    destructor Destroy; override;
    function ToString: string; override;

    property Tag: TdxTagBase read FTag;
    property OldPosition: TdxHtmlInputPosition read FOldPosition;
  end;

  { TdxTranslationTableEntry }

  TdxTranslationTableEntry<T: record> = class
  strict private
    FKey: T;
    FValue: string;
  public
    constructor Create(const AKey: T; const AValue: string);

    property Key: T read FKey;
    property Value: string read FValue;
  end;

  { TdxTranslationTable }

  TdxTranslationTable<T: record> = class
  strict private
    FItems: TdxObjectList<TdxTranslationTableEntry<T>>;
    FEqualityComparer: IEqualityComparer<T>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(const AKey: T; const AValue: string);
    function GetEnumValue(const AStr: string; const ADefaultValue: T; ACaseSensitive: Boolean): T; overload;
    function GetEnumValue(const AStr: string; const ADefaultValue: T): T; overload;
    function GetStringValue(const AKey, ADefaultKey: T): string;
  end;

  TdxAttributeTranslateKeywordHandler = reference to procedure (AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase);
  TdxAttributeKeywordTranslatorTable = class(TdxNamedDelegateDictionary<TdxAttributeTranslateKeywordHandler>);

  { TdxTagBaseHelper }

  TdxTagBaseHelper = class
  strict private
    FImporter: TdxCustomHtmlImporter;
    FCurrentTagIndex: Integer;
    FTagBase: TdxTagBase;
    FPrevRelativeProperties: TdxCssProperties;
    function GetOldPosition: TdxHtmlInputPosition;
  protected
    function ApplyCssProperties(AStyleAttributeProperties: TdxCssProperties): TdxParagraphFormattingOptions;
    function IsFontSizeChanged: Boolean;
    function ApplyCssPropertiesCore(AStyleAttributeProperties: TdxCssProperties): TdxParagraphFormattingOptions;
    function CheckSelector(ASelector: TdxSelector): Boolean;
    function CheckSelectorCore(AElement: TdxSelectorElement; ACombinator: TdxCombinator): Boolean;
    function GetCurrentTagIndex: Integer;
    function CheckParentTag(AElement: TdxSelectorElement): Boolean;
    function CheckAncestorTag(AElement: TdxSelectorElement): Boolean;
    function IsTrueSelector(ASimpleSelector: TdxSimpleSelector): Boolean;
    function CheckSelectorName(ASimpleSelector: TdxSimpleSelector; ATagBase: TdxTagBase): Boolean;
    function CheckSelectorId(ASimpleSelector: TdxSimpleSelector; ATagBase: TdxTagBase): Boolean;
    function CheckSelectorClasses(ASimpleSelector: TdxSimpleSelector; ATagBase: TdxTagBase): Boolean;
    function CheckSelectorPseudoClasses(ASimpleSelector: TdxSimpleSelector; ATagBase: TdxTagBase): Boolean;
    function CheckSelectorAttributes(ASimpleSelector: TdxSimpleSelector; ATagBase: TdxTagBase): Boolean;
    function CheckSelectorAttribute(ASelectorAttribute: TdxSelectorAttribute; ATagBase: TdxTagBase): Boolean;
    function CheckSelectorAttributeCore(ASelectorAttributes: TdxSelectorAttribute; AAttr: TdxAttribute): Boolean;
    function GetRelativeProperties(AProperties: TdxCssProperties): TdxCssProperties;
    function IsRelative(const ARelativeUnit: string): Boolean;
    function GetRelativeIndentProperties(const ARelativeUnit: string; ACssProperties: Integer): Integer;
    function GetRelativeLineSpacingProperties(const ARelativeUnit: string; ACssProperties: TdxParagraphFormattingBase): Single;
    function GetRelativeWidthProperties(const ARelativeUnit: string; ARelativeValue: Single; AFontSize: Integer): TdxWidthUnitInfo;
    function OverrideProperties(ACssProperties: TdxCssProperties): TdxParagraphFormattingOptions;
    function GetOverriddenCharacterProperties(ACssCharacterProperties: TdxCharacterFormattingBase): TdxMergedCharacterProperties;
    function GetOverriddenParagraphProperties(ACssParagraphProperties: TdxParagraphFormattingBase): TdxMergedParagraphProperties;
    function GetOverriddenCellProperties(ATableCellProperties: TdxTableCellProperties): TdxMergedTableCellProperties;

    property OldPosition: TdxHtmlInputPosition read GetOldPosition;
    property Importer: TdxCustomHtmlImporter read FImporter;
  public
    constructor Create(AImporter: TdxCustomHtmlImporter; ATagBase: TdxTagBase);
    destructor Destroy; override;

    class function GetRelativeWidth(const AUnitConverter: TdxDocumentModelUnitConverter; const ARelativeUnit: string;
      ARelativeValue: Single; AFontSize: Integer; ARootDoubleFontSize: Integer): TdxWidthUnitInfo; static;
  end;

  { TdxTagBase }

  TdxTagBase = class abstract
  strict private
    class var
      FBorderLineStyleTable: TdxTranslationTable<TdxBorderLineStyle>;
      FAttributeTable: TdxAttributeKeywordTranslatorTable;
    class constructor Initialize;
    class destructor Finalize;
  strict private
    FImporter: TdxCustomHtmlImporter;
    FName: TdxHtmlTagNameID;
    FClasses: TdxStyleClasses;
    FCharacterFormatting: TdxCharacterFormattingBase;
    class function CreateBorderLineStyleTable: TdxTranslationTable<TdxBorderLineStyle>; static;
    function GetTag: TdxTag;
    function GetDocumentModel: TdxDocumentModel;
  protected
    FId: string;
    FStyleAttributeProperties: TdxCssProperties;
    function GetAttributeTable: TdxAttributeKeywordTranslatorTable; virtual;
    function GetCanAppendToTagStack: Boolean; virtual;
    function GetShouldBeIgnored: Boolean; virtual;
    function GetApplyStylesToInnerHtml: Boolean; virtual;
    function ApplyCssProperties: TdxParagraphFormattingOptions; virtual;
    procedure CopyActualCharacterProperties;
    procedure RemoveDuplicateAttributes;
    procedure FindKeywordInAttributeTable; virtual;
    procedure ApplyTagProperties; virtual; abstract;
    procedure OpenTagProcessCore; virtual;
    procedure ProcessIsEmptyLine;
    procedure ProcessIsEmptyParagraph;
    procedure ApplyCharacterProperties(ARunIndex: TdxRunIndex);
    function ShouldAddParagraph: Boolean; virtual;
    procedure CopyProperties;
    procedure AppendText(const AText: string); virtual;
    function CreateUri(const ABaseUriString: string; const AValue: string): TdxUri;
    procedure IgnoredMarginPropertiesFromBlockTags;

    property Importer: TdxCustomHtmlImporter read FImporter;
    property Tag: TdxTag read GetTag;
    property AttributeTable: TdxAttributeKeywordTranslatorTable read GetAttributeTable;
    property CharacterFormatting: TdxCharacterFormattingBase read FCharacterFormatting;
  public
    constructor Create(AImporter: TdxCustomHtmlImporter);
    destructor Destroy; override;
    procedure AppendContent(const AText: string; AUseRawText: Boolean); virtual;
    procedure ApplyProperties; virtual;
    procedure BeforeDeleteTagFromStack(AIndexOfDeletedTag: Integer); virtual;
    class function ConvertKeyToUpper(const AKey: string): string; static;
    class function CreateAttributeTable: TdxAttributeKeywordTranslatorTable; static;
    procedure DeleteOldOpenTag; virtual;
    procedure EmptyTagProcess; virtual;
    procedure FunctionalTagProcess; virtual;
    function GetAbsoluteUri(const ABaseUriString: string; const AValue: string): string; virtual;
    function GetStartIndexAllowedSearchScope: Integer; virtual;
    procedure OpenTagProcess;
    procedure ParagraphFunctionalProcess; virtual;
    class procedure StyleAttributeKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase); static;
    class procedure ClassAttributeKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase); static;
    class procedure AddClass(ATag: TdxTagBase; const AVal: string); static;
    class procedure IdAttributeKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase); static;
    class function GetColorValue(const AValue: string): TdxAlphaColor; static;
    class function GetColorByName(const AValue: string): TdxAlphaColor; static;
    class function GetColor(const AColorName: string; AStartIndex: Integer): Integer; static;
    class function GetColorByRgb(const AColorName: string): TdxAlphaColor; static;
    class function ParseRGB(const AValue: string): TdxAlphaColor; static;
    class function ParseMediaAttribute(const AValue: string): TdxStringList; static;
    class function ConvertPixelsValueToWidthUnitInfo(AUnitConverter: TdxDocumentModelUnitConverter;
      const AValue: string): TdxWidthUnitInfo; static;
    class function ReadParagraphAlignment(const AValue: string; var ATargetAlignment: TdxParagraphAlignment): Boolean; static;
    class function ReadVerticalAlignment(const AValue: string): TdxVerticalAlignment; static;
    class function ImportBorderWidthCore(AUnitConverter: TdxDocumentModelUnitConverter;
      const AValue: string): TdxWidthUnitInfo; static;
    class function ImportColor(AUnitConverter: TdxDocumentModelUnitConverter; const AValue: string): TdxAlphaColor; static;
    class procedure ImportBorderLineStyle(AUnitConverter: TdxDocumentModelUnitConverter; const AValue: string;
      ABorder: TdxHtmlBorderProperty); static;
    class procedure ImportBordersLineStyles(AUnitConverter: TdxDocumentModelUnitConverter; const AValue: string;
      ABordersProperties: TdxHtmlBordersProperties); static;
    class procedure ImportBordersColors(AUnitConverter: TdxDocumentModelUnitConverter; const AValue: string;
      ABordersProperties: TdxHtmlBordersProperties); static;
    class procedure ImportBordersWidths(AUnitConverter: TdxDocumentModelUnitConverter; const AValue: string;
      ABordersProperties: TdxHtmlBordersProperties); static;
    class function ImportAlignment(const AValue: string; var ATargetAlignment: TdxTableRowAlignment): Boolean; static;

    property ApplyStylesToInnerHtml: Boolean read GetApplyStylesToInnerHtml;
    class property BorderLineStyleTable: TdxTranslationTable<TdxBorderLineStyle> read FBorderLineStyleTable;
    property Name: TdxHtmlTagNameID read FName;
    property Id: string read FId;
    property CanAppendToTagStack: Boolean read GetCanAppendToTagStack;
    property Classes: TdxStyleClasses read FClasses;
    property DocumentModel: TdxDocumentModel read GetDocumentModel;
    property ShouldBeIgnored: Boolean read GetShouldBeIgnored;
  end;

implementation

uses
  Contnrs, Character, Math, IOUtils,
  dxRichEdit.DocumentLayout.UnitConverter,
  dxRichEdit.Import.Html,
  dxCharacters,
  dxRichEdit.Utils.DXUnit,
  dxRichEdit.Utils.Exceptions,
  dxRichEdit.Utils.NumberParser,
  dxStringHelper;

type
  TdxTagBaseHelperInnerHelper = class helper for TdxTagBaseHelper
  private
    function GetImporter: TdxHtmlImporter; inline;
  public
    property Importer: TdxHtmlImporter read GetImporter;
  end;

  TdxTagBaseInnerHelper = class helper for TdxTagBase
  private
    function GetImporter: TdxHtmlImporter; inline;
  public
    property Importer: TdxHtmlImporter read GetImporter;
  end;

{ TdxTagBaseHelperInnerHelper }

function TdxTagBaseHelperInnerHelper.GetImporter: TdxHtmlImporter;
begin
  Result := TdxHtmlImporter(inherited Importer);
end;

{ TdxTagBaseInnerHelper }

function TdxTagBaseInnerHelper.GetImporter: TdxHtmlImporter;
begin
  Result := TdxHtmlImporter(inherited Importer);
end;

{ TdxCustomHtmlImporter }

function TdxCustomHtmlImporter.GetDocumentModel: TdxDocumentModel;
begin
  Result := TdxDocumentModel(inherited DocumentModel);
end;

function TdxCustomHtmlImporter.GetPieceTable: TdxPieceTable;
begin
  Result := DocumentModel.MainPieceTable;
end;

{ TdxTranslationTableEntry }

constructor TdxTranslationTableEntry<T>.Create(const AKey: T; const AValue: string);
begin
  FKey := AKey;
  FValue := AValue;
end;

{ TdxHtmlFontSize }

class function TdxHtmlFontSize.Create: TdxHtmlFontSize;
begin
  Result.FFontSize := DefaultHtmlFontSize;
end;

function TdxHtmlFontSize.GetFontSize(AHtmlFontSize: Integer): Single;
begin
  if AHtmlFontSize >= 7 then
    AHtmlFontSize := 7
  else
    if AHtmlFontSize <= 1 then
      AHtmlFontSize := 1;
  Result := HtmlToDocumentFontSizeTable[AHtmlFontSize];
end;

function TdxHtmlFontSize.GetDoubleFontSize(AHtmlFontSize: Integer): Integer;
begin
  Result := Ceil(GetFontSize(AHtmlFontSize) * 2.0);
end;

function TdxHtmlFontSize.GetLargerFontSize(ADoubleFontSize: Integer): Single;
var
  ADocumentFontSize: Single;
  AHtmlFontSize: Integer;
begin
  ADocumentFontSize := ADoubleFontSize / 2.0;
  for AHtmlFontSize := Low(HtmlToDocumentFontSizeTable) to High(HtmlToDocumentFontSizeTable) do
    if ADocumentFontSize < HtmlToDocumentFontSizeTable[AHtmlFontSize] then
      Break;
  Result := GetFontSize(AHtmlFontSize);
end;

function TdxHtmlFontSize.GetLargerDoubleFontSize(ADoubleFontSize: Integer): Single;
begin
  Result := Ceil(GetLargerFontSize(ADoubleFontSize) * 2.0);
end;

function TdxHtmlFontSize.GetLargerFontSize: Single;
begin
  if FFontSize < 7 then
    Inc(FFontSize);
  Result := GetFontSize(FFontSize);
end;

function TdxHtmlFontSize.GetLargerDoubleFontSize: Integer;
begin
  Result := Ceil(GetLargerFontSize * 2.0);
end;

function TdxHtmlFontSize.GetSmallerFontSize(ADoubleFontSize: Integer): Single;
var
  ADocumentFontSize: Single;
  AHtmlFontSize: Integer;
begin
  ADocumentFontSize := ADoubleFontSize / 2.0;
  for AHtmlFontSize := Low(HtmlToDocumentFontSizeTable) to High(HtmlToDocumentFontSizeTable) do
    if ADocumentFontSize > HtmlToDocumentFontSizeTable[AHtmlFontSize] then
      Break;
  Result := GetFontSize(AHtmlFontSize);
end;

function TdxHtmlFontSize.GetSmallerDoubleFontSize(ADoubleFontSize: Integer): Integer;
begin
  Result := Ceil(GetSmallerFontSize(ADoubleFontSize) * 2.0);
end;

function TdxHtmlFontSize.GetSmallerFontSize: Single;
begin
  if FFontSize > 1 then
    Dec(FFontSize);
  Result := GetFontSize(FFontSize);
end;

function TdxHtmlFontSize.GetSmallerDoubleFontSize: Integer;
begin
  Result := Ceil(GetSmallerFontSize * 2.0);
end;

{ TdxHtmlParagraphAlignment }

constructor TdxHtmlParagraphAlignment.Create;
begin
  FUseAlignment := False;
  FAlignmentValue := TdxParagraphAlignment.Left;
end;

procedure TdxHtmlParagraphAlignment.SetAlignmentValue(const AValue: TdxParagraphAlignment);
begin
  FAlignmentValue := AValue;
  FUseAlignment := True;
end;

procedure TdxHtmlParagraphAlignment.ResetDefaultAlignment;
begin
  FAlignmentValue := TdxParagraphAlignment.Left;
  FUseAlignment := False;
end;

procedure TdxHtmlParagraphAlignment.CopyFrom(AValue: TdxHtmlParagraphAlignment);
begin
  FUseAlignment := AValue.UseAlignment;
  FAlignmentValue := AValue.AlignmentValue;
end;

{ TdxHtmlInputPosition }

constructor TdxHtmlInputPosition.Create(APieceTable: TdxSimplePieceTable);
begin
  inherited Create(APieceTable);
  CharacterFormatting.BeginUpdate;
  CharacterFormatting.FontName := 'Times New Roman';
  CharacterFormatting.DoubleFontSize := 24;
  CharacterFormatting.ResetUse(TdxCharacterFormattingOptions.MaskUseAll);
  CharacterFormatting.EndUpdate;

  FParagraphFormatting := APieceTable.DocumentModel.CreateEmptyParagraphFormatting;
  FDefaultAlignment := TdxHtmlParagraphAlignment.Create;
  FListLevelProperties := TdxHtmlListLevelProperties.Create;

  FLevelIndex := -1;
  FAdditionalIndent := 0;

  FTableProperties := TdxHtmlTableProperties.Create;
  FCellProperties := TdxTableCellProperties.Create(PieceTable, Self);
  FRowProperties := TdxHtmlTableRowProperties.Create;
  FParagraphTabs := TdxTabFormattingInfo.Create;
  FTableCellRowSpan := 1;
  FNextTableRowHeight := 0;
  FImageProperties := TdxHtmlImageProperties.Create;
  FCssFloat := TdxHtmlCssFloat.NotSet;
end;

destructor TdxHtmlInputPosition.Destroy;
begin
  FreeAndNil(FParagraphFormatting);
  FreeAndNil(FDefaultAlignment);
  FreeAndNil(FListLevelProperties);
  FreeAndNil(FTableProperties);
  FreeAndnIl(FCellProperties);
  FreeAndNil(FRowProperties);
  FreeAndNil(FParagraphTabs);
  FreeAndNil(FImageProperties);
  inherited Destroy;
end;

function TdxHtmlInputPosition.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

procedure TdxHtmlInputPosition.SetLevelIndex(const AValue: Integer);
begin
  if AValue >= 9 then
    Exit;
  FLevelIndex := AValue;
end;

procedure TdxHtmlInputPosition.SetTableCellRowSpan(const AValue: Integer);
begin
  if AValue < 0 then
    TdxRichEditExceptions.ThrowArgumentException('RowSpan', AValue);
  FTableCellRowSpan := AValue;
end;

function TdxHtmlInputPosition._AddRef: Integer;
begin
  Result := -1;
end;

function TdxHtmlInputPosition._Release: Integer;
begin
  Result := -1;
end;

procedure TdxHtmlInputPosition.CopyFrom(AValue: TdxHtmlInputPosition);
begin
  CharacterFormatting.CopyFrom(AValue.CharacterFormatting);
  FParagraphFormatting.CopyFrom(AValue.ParagraphFormatting);
  FListLevelProperties.CopyFrom(AValue.ListLevelProperties);
  FTableProperties.CopyFrom(AValue.TableProperties);
  FCellProperties.CopyFrom(AValue.CellProperties);
  FRowProperties.CopyFrom(AValue.RowProperties);
  FParagraphTabs.CopyFrom(AValue.ParagraphTabs);

  CharacterStyleIndex := AValue.CharacterStyleIndex;

  DefaultAlignment.CopyFrom(AValue.DefaultAlignment);
  AdditionalIndent := AValue.AdditionalIndent;
  FLevelIndex := AValue.LevelIndex;
  FTableCellRowSpan := AValue.TableCellRowSpan;

  FImageProperties.CopyFrom(AValue.ImageProperties);
  FCssFloat := AValue.CssFloat;
end;

procedure TdxHtmlInputPosition.CopyParagraphFormattingFrom(AValue: TdxHtmlInputPosition);
begin
  ParagraphFormatting.CopyFrom(AValue.ParagraphFormatting);
end;

function TdxHtmlInputPosition.CreateCellPropertiesChangedHistoryItem(AProperties: TdxTableCellProperties): TdxIndexChangedHistoryItemCore;
begin
  Assert(AProperties = CellProperties);
  Result := TdxIndexChangedHistoryItem.Create(TdxPieceTable(AProperties.PieceTable), AProperties);
end;


{ TdxOpenHtmlTag }

constructor TdxOpenHtmlTag.Create(ATag: TdxTagBase; APieceTable: TdxPieceTable);
begin
  inherited Create;
  FTag := ATag;
  FOldPosition := TdxHtmlInputPosition.Create(APieceTable);
end;

destructor TdxOpenHtmlTag.Destroy;
begin
  FreeAndNil(FOldPosition);
  inherited Destroy;
end;

function TdxOpenHtmlTag.ToString: string;
begin
  Result := Format('Tag: %s', [Tag.ToString]);
end;

{ TdxTranslationTable }

constructor TdxTranslationTable<T>.Create;
begin
  FItems := TdxObjectList<TdxTranslationTableEntry<T>>.Create;
  FEqualityComparer := TEqualityComparer<T>.Default;
end;

destructor TdxTranslationTable<T>.Destroy;
begin
  FEqualityComparer := nil;
  FItems.Free;
  inherited Destroy;
end;

procedure TdxTranslationTable<T>.Add(const AKey: T; const AValue: string);
var
  AEntry: TdxTranslationTableEntry<T>;
begin
  AEntry := TdxTranslationTableEntry<T>.Create(AKey, AValue);
  FItems.Add(AEntry);
end;

function TdxTranslationTable<T>.GetEnumValue(const AStr: string; const ADefaultValue: T; ACaseSensitive: Boolean): T;
var
  AEntry: TdxTranslationTableEntry<T>;
  S: string;
  I: Integer;
begin
  if AStr = '' then
    Exit(ADefaultValue);

  if not ACaseSensitive then
    S := LowerCase(AStr)
  else
    S := AStr;

  for I := 0 to FItems.Count - 1 do
  begin
    AEntry := FItems[I];
    if S = AEntry.Value then
      Exit(AEntry.Key);
  end;

  Result := ADefaultValue;
end;

function TdxTranslationTable<T>.GetEnumValue(const AStr: string; const ADefaultValue: T): T;
begin
  Result := GetEnumValue(AStr, ADefaultValue, False);
end;

function TdxTranslationTable<T>.GetStringValue(const AKey, ADefaultKey: T): string;
var
  ADefaultValue: string;
  AEntry: TdxTranslationTableEntry<T>;
  I: Integer;
begin
  Result := '';
  for I := 0 to FItems.Count - 1 do
  begin
    AEntry := FItems[I];
    if FEqualityComparer.Equals(AKey, AEntry.Key) then
      Exit(AEntry.Value);

    if (Result = '') and FEqualityComparer.Equals(ADefaultKey, AEntry.Key) then
      Result := AEntry.Value;
  end;
end;

{ TdxTagBaseHelper }

constructor TdxTagBaseHelper.Create(AImporter: TdxCustomHtmlImporter; ATagBase: TdxTagBase);
begin
  FImporter := AImporter;
  FTagBase := ATagBase;
end;

destructor TdxTagBaseHelper.Destroy;
begin
  FPrevRelativeProperties.Free;
  inherited Destroy;
end;

function TdxTagBaseHelper.GetOldPosition: TdxHtmlInputPosition;
var
  ACount: Integer;
begin
  ACount := Importer.TagsStack.Count;
  if ACount > 0 then
    Result := Importer.TagsStack[ACount - 1].OldPosition
  else
    Result := nil;
end;

function TdxTagBaseHelper.ApplyCssProperties(AStyleAttributeProperties: TdxCssProperties): TdxParagraphFormattingOptions;
begin
  Result := ApplyCssPropertiesCore(AStyleAttributeProperties);
  if IsFontSizeChanged then
    Result.Value := Result.Value + ApplyCssPropertiesCore(AStyleAttributeProperties).Value;
end;

function TdxTagBaseHelper.IsFontSizeChanged: Boolean;
begin
  if OldPosition = nil then
    Result := False
  else
    Result := OldPosition.CharacterFormatting.DoubleFontSize <> Importer.Position.CharacterFormatting.DoubleFontSize;
end;

function TdxTagBaseHelper.ApplyCssPropertiesCore(AStyleAttributeProperties: TdxCssProperties): TdxParagraphFormattingOptions;
var
  AStyleTagCollection: TdxCssElementCollection;
  ACount, I: Integer;
  AParagraphOptions, AOverridenOptions: TdxParagraphFormattingOptions;
  ACssProperties: TdxCssProperties;
begin
  AStyleTagCollection := Importer.StyleTagCollection;
  ACount := AStyleTagCollection.Count;
  AParagraphOptions := TdxParagraphFormattingOptions.Create(TdxParagraphFormattingOptions.MaskUseNone);
  for I := 0 to ACount - 1 do
  begin
    if CheckSelector(AStyleTagCollection[I].Selector) then
    begin
      ACssProperties := GetRelativeProperties(AStyleTagCollection[I].Properties);
      AOverridenOptions := OverrideProperties(ACssProperties);
      AParagraphOptions.Value := AParagraphOptions.Value + AOverridenOptions.Value;
    end;
  end;
  if AStyleAttributeProperties <> nil then
  begin
    ACssProperties := GetRelativeProperties(AStyleAttributeProperties);
    AOverridenOptions := OverrideProperties(ACssProperties);
    AParagraphOptions.Value := AParagraphOptions.Value + AOverridenOptions.Value;
  end;
  Result := AParagraphOptions;
end;

function TdxTagBaseHelper.CheckSelector(ASelector: TdxSelector): Boolean;
var
  ACombinator: TdxCombinator;
  AElements: TdxList<TdxSelectorElement>;
  I: Integer;
begin
  ACombinator := TdxCombinator.None;

  AElements := ASelector.Elements;
  for I := AElements.Count - 1 downto 0 do
  begin
    if CheckSelectorCore(AElements[I], ACombinator) then
    begin
      if AElements[I].Combinator = TdxCombinator.None then
        Exit(True);
      ACombinator := AElements[I].Combinator;
    end
    else
      Exit(False);
  end;
  Result := False;
end;

function TdxTagBaseHelper.CheckSelectorCore(AElement: TdxSelectorElement; ACombinator: TdxCombinator): Boolean;
begin
  case ACombinator of
    TdxCombinator.None:
      begin
        FCurrentTagIndex := GetCurrentTagIndex;
        Result := IsTrueSelector(AElement.SimpleSelector);
      end;
    TdxCombinator.WhiteSpace:
      Result := CheckAncestorTag(AElement);
    TdxCombinator.PlusSign:
      Result := True;
    TdxCombinator.RightAngle:
      Result := CheckParentTag(AElement);
    else
      Result := True;
  end;
end;

function TdxTagBaseHelper.GetCurrentTagIndex: Integer;
var
  ATags: TdxList<TdxOpenHtmlTag>;
  I: Integer;
begin
  ATags := Importer.TagsStack;
  for I := Importer.TagsStack.Count - 1 downto 0 do
    if FTagBase = ATags[I].Tag then
      Exit(I);
  Result := Importer.TagsStack.Count - 1;
end;

function TdxTagBaseHelper.CheckParentTag(AElement: TdxSelectorElement): Boolean;
var
  AIndex: Integer;
begin
  AIndex := FCurrentTagIndex - 1;
  if AIndex >= 0 then
  begin
    FCurrentTagIndex := AIndex;
    Result := IsTrueSelector(AElement.SimpleSelector);
  end
  else
    Result := False;
end;

function TdxTagBaseHelper.CheckAncestorTag(AElement: TdxSelectorElement): Boolean;
var
  AIndex, I: Integer;
begin
  AIndex := FCurrentTagIndex;
  for I := 0 to AIndex - 1 do
  begin
    FCurrentTagIndex := I;
    if IsTrueSelector(AElement.SimpleSelector) then
      Exit(True);
  end;
  Result := False;
end;

function TdxTagBaseHelper.IsTrueSelector(ASimpleSelector: TdxSimpleSelector): Boolean;
var
  ATagBase: TdxTagBase;
begin
  if (FCurrentTagIndex >= Importer.TagsStack.Count) or (FCurrentTagIndex < 0) then
    Exit(False);
  ATagBase := Importer.TagsStack[FCurrentTagIndex].Tag;
  Result := (((CheckSelectorName(ASimpleSelector, ATagBase) and CheckSelectorId(ASimpleSelector, ATagBase)) and
    CheckSelectorClasses(ASimpleSelector, ATagBase)) and CheckSelectorPseudoClasses(ASimpleSelector, ATagBase)) and
    CheckSelectorAttributes(ASimpleSelector, ATagBase);
end;

function TdxTagBaseHelper.CheckSelectorName(ASimpleSelector: TdxSimpleSelector; ATagBase: TdxTagBase): Boolean;
var
  ASimpleSelectorName: TdxHtmlTagNameID;
begin
  ASimpleSelectorName := Importer.GetTagNameID(ASimpleSelector.Name);
  Result := (ATagBase.Name = ASimpleSelectorName) or (ASimpleSelector.Name = '') or (ASimpleSelector.Name = '*');
end;

function TdxTagBaseHelper.CheckSelectorId(ASimpleSelector: TdxSimpleSelector; ATagBase: TdxTagBase): Boolean;
begin
  Result := (ATagBase.Id = ASimpleSelector.Id) or (ASimpleSelector.Id = '');
end;

function TdxTagBaseHelper.CheckSelectorClasses(ASimpleSelector: TdxSimpleSelector; ATagBase: TdxTagBase): Boolean;
var
  I: Integer;
begin
  for I := 0 to ASimpleSelector.Classes.Count - 1 do
  begin
    if not ATagBase.Classes.Contains(ASimpleSelector.Classes[I]) then
      Exit(False);
  end;
  Result := True;
end;

function TdxTagBaseHelper.CheckSelectorPseudoClasses(ASimpleSelector: TdxSimpleSelector; ATagBase: TdxTagBase): Boolean;
begin
  if ASimpleSelector.PseudoClasses.Count > 0 then
    Result := False
  else
    Result := True;
end;

function TdxTagBaseHelper.CheckSelectorAttributes(ASimpleSelector: TdxSimpleSelector; ATagBase: TdxTagBase): Boolean;
var
  I: Integer;
  ASelectorAttributes: TdxSelectorAttribute;
begin
  for I := 0 to ASimpleSelector.SelectorAttributes.Count - 1 do
  begin
    ASelectorAttributes := ASimpleSelector.SelectorAttributes[I];
    if not CheckSelectorAttribute(ASelectorAttributes, ATagBase) then
      Exit(False);
  end;
  Result := True;
end;

function TdxTagBaseHelper.CheckSelectorAttribute(ASelectorAttribute: TdxSelectorAttribute; ATagBase: TdxTagBase): Boolean;
var
  I: Integer;
  AAttr: TdxAttribute;
begin
  for I := 0 to ATagBase.Tag.Attributes.Count - 1 do
  begin
    AAttr := ATagBase.Tag.Attributes[I];
    if AAttr.Name = ASelectorAttribute.AttributeName then
    begin
      if CheckSelectorAttributeCore(ASelectorAttribute, AAttr) then
        Exit(True);
    end;
  end;
  Result := False;
end;

function TdxTagBaseHelper.CheckSelectorAttributeCore(ASelectorAttributes: TdxSelectorAttribute; AAttr: TdxAttribute): Boolean;
var
  ASelectorAttrValue: string;
begin
  ASelectorAttrValue := ASelectorAttributes.AttributeValue;
  if ASelectorAttributes.AttributeConnector = '=' then
    Result := AAttr.Value = ASelectorAttrValue
  else
  if ASelectorAttributes.AttributeConnector = '^=' then
    Result := TdxStringHelper.StartsWith(AAttr.Value, ASelectorAttrValue)
  else
  if ASelectorAttributes.AttributeConnector = '$=' then
    Result := TdxStringHelper.EndsWith(AAttr.Value, ASelectorAttrValue)
  else
  if ASelectorAttributes.AttributeConnector = '*=' then
    Result := TdxStringHelper.Contains(AAttr.Value, ASelectorAttrValue)
  else
  if ASelectorAttributes.AttributeConnector = '~=' then
    Result := AAttr.Value = ASelectorAttrValue
  else
  if ASelectorAttributes.AttributeConnector = '|=' then
    Result := (AAttr.Value = ASelectorAttrValue) or (TdxStringHelper.StartsWith(AAttr.Value, ASelectorAttrValue + '-'))
  else
    Result := True;
end;

function TdxTagBaseHelper.GetRelativeProperties(AProperties: TdxCssProperties): TdxCssProperties;
var
  ARelativeProperties, AResultRelativeProperties: PdxRelativeProperties;
  ACssParagraphProperties: TdxParagraphFormattingBase;
  AFontSize: Integer;
  AWidth, AHeight: TdxWidthUnitInfo;
begin
  ARelativeProperties := AProperties.RelativeProperties;
  if not ARelativeProperties.HasRelativeProperties then
    Exit(AProperties);

  Result := TdxCssProperties.Create(Importer.DocumentModel.MainPieceTable);
  Result.CopyFrom(AProperties);
  FPrevRelativeProperties.Free;
  FPrevRelativeProperties := Result;

  ACssParagraphProperties := Result.CssParagraphProperties;
  AResultRelativeProperties := Result.RelativeProperties;
  if IsRelative(ARelativeProperties.UnitRelativeFirstLineIndent) then
    ACssParagraphProperties.FirstLineIndent := GetRelativeIndentProperties(AResultRelativeProperties.UnitRelativeFirstLineIndent, ACssParagraphProperties.FirstLineIndent);
  if IsRelative(ARelativeProperties.UnitRelativeLeftIndent) then
    ACssParagraphProperties.LeftIndent := GetRelativeIndentProperties(AResultRelativeProperties.UnitRelativeLeftIndent, ACssParagraphProperties.LeftIndent);
  if IsRelative(ARelativeProperties.UnitRelativeRightIndent) then
    ACssParagraphProperties.RightIndent := GetRelativeIndentProperties(AResultRelativeProperties.UnitRelativeRightIndent, ACssParagraphProperties.RightIndent);
  if IsRelative(ARelativeProperties.UnitRelativeLineSpacing) then
    ACssParagraphProperties.LineSpacing := GetRelativeLineSpacingProperties(AResultRelativeProperties.UnitRelativeLineSpacing, ACssParagraphProperties);
  if IsRelative(ARelativeProperties.UnitRelativeSpacingAfter) then
    ACssParagraphProperties.SpacingAfter := GetRelativeIndentProperties(AResultRelativeProperties.UnitRelativeSpacingAfter, ACssParagraphProperties.SpacingAfter);
  if IsRelative(ARelativeProperties.UnitRelativeLineSpacing) then
    ACssParagraphProperties.SpacingBefore := GetRelativeIndentProperties(AResultRelativeProperties.UnitRelativeSpacingBefore, ACssParagraphProperties.SpacingBefore);
  if IsRelative(ARelativeProperties.UnitRelativeFontSize) then
  begin
    if ARelativeProperties.UnitRelativeFontSize = 'rem' then
      Result.CssCharacterProperties.DoubleFontSize := Max(1, Round(Result.CssCharacterProperties.DoubleFontSize * Importer.RootDoubleFontSize / 2.0 / 100.0))
    else
      Result.CssCharacterProperties.DoubleFontSize := Max(1, Round(Result.CssCharacterProperties.DoubleFontSize * OldPosition.CharacterFormatting.DoubleFontSize / 2.0 / 100.0));
  end;

  AFontSize := Importer.Position.CharacterFormatting.DoubleFontSize div 2;
  if IsRelative(ARelativeProperties.UnitRelativeWidth) then
  begin
    AWidth := GetRelativeWidthProperties(ARelativeProperties.UnitRelativeWidth, ARelativeProperties.RelativeWidth, AFontSize);
    AHeight := GetRelativeWidthProperties(ARelativeProperties.UnitRelativeHeight, ARelativeProperties.RelativeHeight, AFontSize);
    try
      Result.CellProperties.PreferredWidth.CopyFrom(AWidth);
      Result.TableProperties.Width := AWidth;
      Result.ImageProperties.Width := AWidth;
      Result.ImageProperties.Height := AHeight;
    finally
      AWidth.Free;
      AHeight.Free;
    end;
  end;
end;

function TdxTagBaseHelper.IsRelative(const ARelativeUnit: string): Boolean;
begin
  Result := TdxRelativeProperties.IsRelative(ARelativeUnit);
end;

function TdxTagBaseHelper.GetRelativeIndentProperties(const ARelativeUnit: string; ACssProperties: Integer): Integer;
var
  APageWidth, AValue: Integer;
begin
  APageWidth := Importer.DocumentModel.Sections.Last.Page.Width;
  if ARelativeUnit = '%' then
    Exit(Round(APageWidth * ACssProperties / 100.0));

  AValue := Round(Importer.Position.CharacterFormatting.DoubleFontSize / 2.0 * ACssProperties / 100.0);
  Result := Importer.UnitConverter.PointsToModelUnits(AValue);
end;

function TdxTagBaseHelper.GetRelativeLineSpacingProperties(const ARelativeUnit: string; ACssProperties: TdxParagraphFormattingBase): Single;
var
  ALineSpacing, AValue: Single;
begin
  if ARelativeUnit = '%' then
  begin
    ALineSpacing := ACssProperties.LineSpacing / 100.0;
    ACssProperties.SetMultipleLineSpacing(ALineSpacing);
    Exit(ALineSpacing);
  end;

  AValue := Importer.Position.CharacterFormatting.DoubleFontSize * ACssProperties.LineSpacing / 100.0 / 2;
  Result := Importer.UnitConverter.PointsToModelUnitsF(AValue);
end;

function TdxTagBaseHelper.GetRelativeWidthProperties(const ARelativeUnit: string; ARelativeValue: Single;
  AFontSize: Integer): TdxWidthUnitInfo;
begin
  Result := GetRelativeWidth(FImporter.UnitConverter, ARelativeUnit, ARelativeValue, AFontSize, Importer.RootDoubleFontSize);
end;

class function TdxTagBaseHelper.GetRelativeWidth(const AUnitConverter: TdxDocumentModelUnitConverter;
  const ARelativeUnit: string; ARelativeValue: Single; AFontSize: Integer; ARootDoubleFontSize: Integer): TdxWidthUnitInfo;
begin
  Result := TdxWidthUnitInfo.Create;
  if ARelativeUnit = 'em' then
  begin
    Result.Value := AUnitConverter.PointsToModelUnits(Trunc(ARelativeValue * AFontSize));
    Result.&Type := TdxWidthUnitType.ModelUnits;
  end
  else
    if ARelativeUnit = 'ex' then
    begin
      Result.Value := AUnitConverter.PointsToModelUnits(Trunc(ARelativeValue * AFontSize * 2));
      Result.&Type := TdxWidthUnitType.ModelUnits;
    end
    else
      if ARelativeUnit = '%' then
      begin
        Result.Value := Trunc(ARelativeValue * 50);
        Result.&Type := TdxWidthUnitType.FiftiethsOfPercent;
      end
      else
        if ARelativeUnit = 'rem' then
        begin
          Result.Value := AUnitConverter.PointsToModelUnits(Trunc(ARelativeValue * ARootDoubleFontSize / 100.0));
          Result.&Type := TdxWidthUnitType.ModelUnits;
        end;
end;

function TdxTagBaseHelper.OverrideProperties(ACssProperties: TdxCssProperties): TdxParagraphFormattingOptions;
var
  AImporterPosition: TdxHtmlInputPosition;
  AOverriddenCharacterProperties: TdxMergedCharacterProperties;
  AOverriddenParagraphProperties: TdxMergedParagraphProperties;
  AOverriddenCellProperties: TdxMergedTableCellProperties;
  AImporterPositionCellPropertiesBorders: TdxTableCellBorders;
  ACssBordersProperties: TdxHtmlBordersProperties;
  AListLevelProperties: TdxHtmlListLevelProperties;
  ATableProperties: TdxHtmlTableProperties;
  ABordersProperties: TdxHtmlBordersProperties;
  ATableRowProperties: TdxHtmlTableRowProperties;
  AImageProperties: TdxHtmlImageProperties;
begin
  AImporterPosition := Importer.Position;

  AOverriddenCharacterProperties := GetOverriddenCharacterProperties(ACssProperties.CssCharacterProperties);
  try
    AImporterPosition.CharacterFormatting.CopyFrom(AOverriddenCharacterProperties.Info, AOverriddenCharacterProperties.Options);
  finally
    AOverriddenCharacterProperties.Free;
  end;
  AOverriddenParagraphProperties := GetOverriddenParagraphProperties(ACssProperties.CssParagraphProperties);
  try
    Result := TdxParagraphFormattingOptions.Create(ACssProperties.CssParagraphProperties.Options.Value);
    AImporterPosition.ParagraphFormatting.CopyFrom(AOverriddenParagraphProperties.Info, AOverriddenParagraphProperties.Options);
  finally
    AOverriddenParagraphProperties.Free;
  end;
  AListLevelProperties := ACssProperties.ListLevelProperties.MergeWith(AImporterPosition.ListLevelProperties);
  try
    AImporterPosition.ListLevelProperties.CopyFrom(AListLevelProperties);
  finally
    AListLevelProperties.Free;
  end;
  ATableProperties := ACssProperties.TableProperties.MergeWith(AImporterPosition.TableProperties);
  try
    AImporterPosition.TableProperties.CopyFrom(ATableProperties);
  finally
    ATableProperties.Free;
  end;
  ABordersProperties := ACssProperties.BordersProperties.MergeWith(AImporterPosition.TableProperties.BordersProperties);
  try
    AImporterPosition.TableProperties.BordersProperties.CopyFrom(ABordersProperties);
  finally
    ABordersProperties.Free;
  end;
  ATableRowProperties := ACssProperties.RowProperties.MergeWith(AImporterPosition.RowProperties);
  try
    AImporterPosition.RowProperties.CopyFrom(ATableRowProperties);
  finally
    ATableRowProperties.Free;
  end;
  AImageProperties := ACssProperties.ImageProperties.MergeWith(AImporterPosition.ImageProperties);
  try
    AImporterPosition.ImageProperties.CopyFrom(AImageProperties);
  finally
    AImageProperties.Free;
  end;
  AOverriddenCellProperties := GetOverriddenCellProperties(ACssProperties.CellProperties);
  try
    AImporterPosition.CellProperties.CopyFrom(AOverriddenCellProperties);
  finally
    AOverriddenCellProperties.Free;
  end;
  AImporterPositionCellPropertiesBorders := AImporterPosition.CellProperties.Borders;
  ACssBordersProperties := ACssProperties.BordersProperties;
  ACssBordersProperties.TopBorder.Apply(AImporterPositionCellPropertiesBorders.TopBorder);
  ACssBordersProperties.LeftBorder.Apply(AImporterPositionCellPropertiesBorders.LeftBorder);
  ACssBordersProperties.RightBorder.Apply(AImporterPositionCellPropertiesBorders.RightBorder);
  ACssBordersProperties.BottomBorder.Apply(AImporterPositionCellPropertiesBorders.BottomBorder);
end;

function TdxTagBaseHelper.GetOverriddenCharacterProperties(ACssCharacterProperties: TdxCharacterFormattingBase): TdxMergedCharacterProperties;
var
  ANewMergedCharacterProperties: TdxMergedCharacterProperties;
begin
  Result := TdxMergedCharacterProperties.Create(ACssCharacterProperties.Info, ACssCharacterProperties.Options);
  ANewMergedCharacterProperties := TdxMergedCharacterProperties.Create(Importer.Position.CharacterFormatting.Info,
    Importer.Position.CharacterFormatting.Options);
  try
    Result.Merge(ANewMergedCharacterProperties);
  finally
    ANewMergedCharacterProperties.Free;
  end;
end;

function TdxTagBaseHelper.GetOverriddenParagraphProperties(ACssParagraphProperties: TdxParagraphFormattingBase): TdxMergedParagraphProperties;
var
  ANewMergedParagraphProperties: TdxMergedParagraphProperties;
begin
  ANewMergedParagraphProperties := TdxMergedParagraphProperties.Create(ACssParagraphProperties.Info, ACssParagraphProperties.Options);
  try
    Result := TdxMergedParagraphProperties.Create(ANewMergedParagraphProperties);
    Result.MergeCore(Importer.Position.ParagraphFormatting.Info, Importer.Position.ParagraphFormatting.Options);

    if (ANewMergedParagraphProperties.Options.UseSpacingBefore and not ANewMergedParagraphProperties.Options.UseBeforeAutoSpacing) and
      Result.Options.UseBeforeAutoSpacing then
      Result.Info.BeforeAutoSpacing := False;
    if (ANewMergedParagraphProperties.Options.UseSpacingAfter and not ANewMergedParagraphProperties.Options.UseAfterAutoSpacing) and
      Result.Options.UseAfterAutoSpacing then
      Result.Info.AfterAutoSpacing := False;
  finally
    ANewMergedParagraphProperties.Free;
  end;
end;

function TdxTagBaseHelper.GetOverriddenCellProperties(ATableCellProperties: TdxTableCellProperties): TdxMergedTableCellProperties;
var
  ACellPropertiesInfo: TdxCombinedCellPropertiesInfo;
begin
  ACellPropertiesInfo := TdxCombinedCellPropertiesInfo.Create(ATableCellProperties);
  try
    Result := TdxMergedTableCellProperties.Create(ACellPropertiesInfo, ATableCellProperties.Info);
    Result.Merge(Importer.Position.CellProperties);
    if Importer.Position.CellProperties.GeneralSettings.ColumnSpan > 0 then
      Result.Info.GeneralSettings.ColumnSpan := Importer.Position.CellProperties.GeneralSettings.ColumnSpan;
  finally
    ACellPropertiesInfo.Free;
  end;
end;

{ TdxTagBase }

constructor TdxTagBase.Create(AImporter: TdxCustomHtmlImporter);
begin
  inherited Create;
  FImporter := AImporter;
  FName := Tag.NameID;
  FId := '';
  FClasses := TdxStyleClasses.Create;
  FCharacterFormatting := AImporter.DocumentModel.CreateEmptyCharacterFormatting;
  FCharacterFormatting.BeginUpdate;
  FCharacterFormatting.FontName := 'Times New Roman';
  FCharacterFormatting.DoubleFontSize := 24;
  FCharacterFormatting.ResetUse(TdxCharacterFormattingOptions.MaskUseAll);
  FCharacterFormatting.EndUpdate;
end;

destructor TdxTagBase.Destroy;
begin
  FClasses.Free;
  FCharacterFormatting.Free;
  FStyleAttributeProperties.Free;
  inherited Destroy;
end;

class constructor TdxTagBase.Initialize;
begin
  FBorderLineStyleTable := CreateBorderLineStyleTable;
  FAttributeTable := CreateAttributeTable;
end;

class destructor TdxTagBase.Finalize;
begin
  FBorderLineStyleTable.Free;
  FAttributeTable.Free;
end;

class function TdxTagBase.CreateBorderLineStyleTable: TdxTranslationTable<TdxBorderLineStyle>;
begin
  Result := TdxTranslationTable<TdxBorderLineStyle>.Create;
  Result.Add(TdxBorderLineStyle.&Nil, 'none');
  Result.Add(TdxBorderLineStyle.Dotted, 'dotted');
  Result.Add(TdxBorderLineStyle.Dashed, 'dashed');
  Result.Add(TdxBorderLineStyle.Double, 'double');
  Result.Add(TdxBorderLineStyle.Inset, 'inset');
  Result.Add(TdxBorderLineStyle.Outset, 'outset');
  Result.Add(TdxBorderLineStyle.Single, 'solid');
end;

class function TdxTagBase.ConvertKeyToUpper(const AKey: string): string;
begin
  Result := UpperCase(AKey);
end;

class function TdxTagBase.CreateAttributeTable: TdxAttributeKeywordTranslatorTable;
begin
  Result := TdxAttributeKeywordTranslatorTable.Create;
  Result.Add(ConvertKeyToUpper('style'), StyleAttributeKeyword);
  Result.Add(ConvertKeyToUpper('class'), ClassAttributeKeyword);
  Result.Add(ConvertKeyToUpper('id'), IdAttributeKeyword);
end;

class procedure TdxTagBase.StyleAttributeKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase);
var
  ACssParser: TdxCssParser;
  ACssElementCollection: TdxCssElementCollection;
  AReader: TStringReader;
  I: Integer;
begin
  if AValue = '' then
    Exit;
  ACssParser := TdxCssParser.Create(AImporter.DocumentModel);
  try
    AReader := TStringReader.Create(AValue);
    try
      ACssElementCollection := ACssParser.ParseAttribute(AReader);
      if ACssElementCollection.Count > 0 then
      try
        ATag.FStyleAttributeProperties := TdxCssProperties.Create(AImporter.PieceTable);
        ATag.FStyleAttributeProperties.CopyFrom(ACssElementCollection[0].Properties);
      finally
        for I := 0 to ACssElementCollection.Count - 1 do
          ACssElementCollection[I].Free;
      end;
    finally
      AReader.Free;
    end;
  finally
    ACssParser.Free;
  end;
end;

class procedure TdxTagBase.ClassAttributeKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase);
var
  AText: TStringBuilder;
  I: Integer;
begin
  AText := TStringBuilder.Create;
  try
    for I := 1 to Length(AValue) do
    begin
    {$IFDEF DELPHIXE4}
      if not AValue[I].IsWhiteSpace then
    {$ELSE}
      if not TCharacter.IsWhiteSpace(AValue[I]) then
    {$ENDIF}
        AText.Append(AValue[I])
      else
      begin
        AddClass(ATag, AText.ToString);
        AText.Length := 0;
      end;
    end;
    AddClass(ATag, AText.ToString);
  finally
    AText.Free;
  end;
end;

class procedure TdxTagBase.AddClass(ATag: TdxTagBase; const AVal: string);
begin
  if AVal <> '' then
    ATag.Classes.Add(AVal);
end;

class procedure TdxTagBase.IdAttributeKeyword(AImporter: TdxCustomHtmlImporter; const AValue: string; ATag: TdxTagBase);
begin
  ATag.FId := UpperCase(AValue);
end;

class function TdxTagBase.GetColorValue(const AValue: string): TdxAlphaColor;
var
  AColorName: TStringBuilder;
begin
  if AValue[1] = '#' then
  begin
    AColorName := TStringBuilder.Create(AValue);
    try
      AColorName.Remove(0, 1);
      if AColorName.Length = 6 then
        Exit(GetColorByRgb(AColorName.ToString));
      if AColorName.Length = 3 then
      begin
        AColorName.Insert(0, AColorName[0]);
        AColorName.Insert(2, AColorName[2]);
        AColorName.Insert(4, AColorName[4]);
        Exit(GetColorByRgb(AColorName.ToString));
      end;
      Exit(GetColorByName(AValue));
    finally
      AColorName.Free;
    end;
  end;
  if Length(AValue) = 6 then
  begin
    Result := GetColorByRgb(AValue);
    if Result <> TdxAlphaColors.Empty then
      Exit(Result);
  end;
  if TdxStringHelper.StartsWith(AValue, 'rgb(') then
  begin
    Result := ParseRGB(TdxStringHelper.Substring(AValue, 4));
    if Result <> TdxAlphaColors.Empty then
      Exit(Result);
  end;
  Result := GetColorByName(AValue);
end;

class function TdxTagBase.GetColorByName(const AValue: string): TdxAlphaColor;
begin
  Result := TdxAlphaColors.FromName(AValue);

  if not TdxAlphaColors.IsKnownColor(Result) then
    Result := TdxAlphaColors.Empty;
end;

class function TdxTagBase.GetColor(const AColorName: string; AStartIndex: Integer): Integer;
var
  S: string;
begin
  S := TdxStringHelper.Substring(AColorName, AStartIndex, 2);
  TdxNumber.TryParse(S, TdxNumberStyles.HexNumber, Result);
  if (Result = 0) and (S <> '00') then
    Result := -1;
end;

class function TdxTagBase.GetColorByRgb(const AColorName: string): TdxAlphaColor;
var
  R, G, B: Integer;
begin
  R := GetColor(AColorName, 0);
  G := GetColor(AColorName, 2);
  B := GetColor(AColorName, 4);
  if ((R <> -1) and (G <> -1)) and (B <> -1) then
    Exit(TdxAlphaColors.FromArgb(255, Byte(R), Byte(G), Byte(B)));
  Result := TdxAlphaColors.Empty;
end;

class function TdxTagBase.ParseRGB(const AValue: string): TdxAlphaColor;
var
  ARgb: string;
  AColor, I: Integer;
  AColors: TdxIntegerList;
  AIsDigit: Boolean;
begin
  ARgb := '';
  AColors := TdxIntegerList.Create;
  try
    for I := 1 to Length(AValue) do
    begin
      if (AValue[I] <> ',') and (AValue[I] <> ')') then
      begin
      {$IFDEF DELPHIXE4}
        if not AValue[I].IsWhiteSpace then
      {$ELSE}
        if not TCharacter.IsWhiteSpace(AValue[I]) then
      {$ENDIF}
          ARgb := ARgb + AValue[I];
      end
      else
      begin
        AIsDigit := TdxNumber.TryParse(ARgb, AColor);
        if AIsDigit then
          AColors.Add(AColor);
        ARgb := '';
      end;
    end;
    if AColors.Count = 3 then
      Exit(TdxAlphaColors.FromArgb(255, Byte(AColors[0]), Byte(AColors[1]), Byte(AColors[2])));
  finally
    AColors.Free;
  end;

  Result := TdxAlphaColors.Empty;
end;

class function TdxTagBase.ParseMediaAttribute(const AValue: string): TdxStringList;
var
  ADescriptor: TStringBuilder;
  I: Integer;
begin
  Result := TdxStringList.Create;
  ADescriptor := TStringBuilder.Create;
  try
    for I := 1 to Length(AValue) do
    begin
      if AValue[I] <> ',' then
      begin
      {$IFDEF DELPHIXE4}
        if AValue[I].IsLetter then
      {$ELSE}
        if TCharacter.IsLetter(AValue[I]) then
      {$ENDIF}
          ADescriptor.Append(AValue[I]);
      end
      else
      begin
        if ADescriptor.Length > 0 then
          Result.Add(ADescriptor.ToString);
        ADescriptor.Length := 0;
      end;
    end;
    if ADescriptor.Length > 0 then
      Result.Add(ADescriptor.ToString);
  finally
    ADescriptor.Free;
  end;
end;

class function TdxTagBase.ConvertPixelsValueToWidthUnitInfo(AUnitConverter: TdxDocumentModelUnitConverter;
  const AValue: string): TdxWidthUnitInfo;
var
  AValueInPixels: Integer;
  AParsedValue: TdxLengthValueParser;
  AConverter: TdxDocumentModelUnitConverter;
begin
  Result := TdxWidthUnitInfo.Create;
  if TdxNumber.TryParse(AValue, AValueInPixels) then
    AParsedValue := TdxLengthValueParser.Create(AValue + 'px', AUnitConverter.ScreenDpi)
  else
    AParsedValue := TdxLengthValueParser.Create(AValue, AUnitConverter.ScreenDpi);

  if AParsedValue.IsRelativeUnit then
  begin
    Result.Value := Max(0, Round(AParsedValue.PointsValue) * 50);
    Result.&Type := TdxWidthUnitType.FiftiethsOfPercent;
  end
  else
    if AParsedValue.IsDigit then
    begin
      AConverter := AUnitConverter;
      Result.Value := Max(0, Round(AConverter.PointsToModelUnitsF(AParsedValue.PointsValue)));
      Result.&Type := TdxWidthUnitType.ModelUnits;
    end;
end;

class function TdxTagBase.ReadParagraphAlignment(const AValue: string;
  var ATargetAlignment: TdxParagraphAlignment): Boolean;
var
  S: string;
begin
  S := LowerCase(AValue);
  if S = 'right' then
  begin
    ATargetAlignment := TdxParagraphAlignment.Right;
    Exit(True);
  end;
  if S = 'center' then
  begin
    ATargetAlignment := TdxParagraphAlignment.Center;
    Exit(True);
  end;
  if S = 'justify' then
  begin
    ATargetAlignment := TdxParagraphAlignment.Justify;
    Exit(True);
  end;
  if S = 'left' then
  begin
    ATargetAlignment := TdxParagraphAlignment.Left;
    Exit(True);
  end;
  Result := False;
end;

class function TdxTagBase.ReadVerticalAlignment(const AValue: string): TdxVerticalAlignment;
var
  S: string;
begin
  S := LowerCase(AValue);
  if S = 'top' then
    Result := TdxVerticalAlignment.Top
  else
    if S = 'middle' then
      Result := TdxVerticalAlignment.Center
    else
      if S = 'bottom' then
        Result := TdxVerticalAlignment.Bottom
      else
        Result := TdxVerticalAlignment.Top;
end;

function TdxTagBase.GetTag: TdxTag;
begin
  Result := TdxTag(Importer.Element);
end;

function TdxTagBase.GetAttributeTable: TdxAttributeKeywordTranslatorTable;
begin
  Result := FAttributeTable;
end;

function TdxTagBase.GetDocumentModel: TdxDocumentModel;
begin
  Result := Importer.DocumentModel;
end;

function TdxTagBase.GetCanAppendToTagStack: Boolean;
begin
  Result := True;
end;

function TdxTagBase.GetShouldBeIgnored: Boolean;
begin
  Result := False;
end;

function TdxTagBase.GetApplyStylesToInnerHtml: Boolean;
begin
  Result := True;
end;

function TdxTagBase.ApplyCssProperties: TdxParagraphFormattingOptions;
var
  ATagBaseHelper: TdxTagBaseHelper;
begin
  ATagBaseHelper := TdxTagBaseHelper.Create(Importer, Self);
  try
    Result := ATagBaseHelper.ApplyCssProperties(FStyleAttributeProperties);
  finally
    ATagBaseHelper.Free;
  end;
end;

procedure TdxTagBase.OpenTagProcess;
begin
  RemoveDuplicateAttributes;
  FindKeywordInAttributeTable;
  CopyActualCharacterProperties;
  ApplyProperties;
  FunctionalTagProcess;
  CopyActualCharacterProperties;
  OpenTagProcessCore;
end;

procedure TdxTagBase.CopyActualCharacterProperties;
begin
  FCharacterFormatting.CopyFrom(Importer.Position.CharacterFormatting);
end;

procedure TdxTagBase.RemoveDuplicateAttributes;
var
  AAttributes: TdxList<TdxAttribute>;
  ACount, I, J: Integer;
  AAttrName: string;
begin
  AAttributes := Tag.Attributes;
  ACount := AAttributes.Count;
  for I := 1 to ACount - 1 do
  begin
    AAttrName := AAttributes[I].Name;
    for J := 0 to I - 1 do
    begin
      if AAttrName = AAttributes[J].Name then
      begin
        AAttributes[I].Name := '';
        Break;
      end;
    end;
  end;
end;

procedure TdxTagBase.EmptyTagProcess;
begin
  Importer.OpenProcess(Self);
end;

procedure TdxTagBase.FunctionalTagProcess;
begin
end;

procedure TdxTagBase.BeforeDeleteTagFromStack(AIndexOfDeletedTag: Integer);
var
  ADeletedTag: TdxTagBase;
begin
  ADeletedTag := Importer.TagsStack[AIndexOfDeletedTag].Tag;
  FCharacterFormatting.CopyFrom(ADeletedTag.CharacterFormatting);
end;

procedure TdxTagBase.FindKeywordInAttributeTable;
var
  AAttributes: TdxList<TdxAttribute>;
  I: Integer;
  ATranslator: TdxAttributeTranslateKeywordHandler;
begin
  if Tag.NameID = TdxHtmlTagNameID.Unknown then
    Exit;
  AAttributes := Tag.Attributes;
  for I := 0 to AAttributes.Count - 1 do
  begin
    if ((AAttributes[I].Value <> '') or (AAttributes[I].Name = 'NOWRAP')) and
        AttributeTable.TryGetValue(AAttributes[I].Name, ATranslator) then
      ATranslator(FImporter, Importer.DecodeStringContent(AAttributes[I].Value), Self);
  end;
end;

procedure TdxTagBase.ApplyProperties;
var
  APosition: TdxHtmlInputPosition;
begin
  APosition := Importer.Position;
  APosition.CharacterFormatting.BeginUpdate;
  APosition.ParagraphFormatting.BeginUpdate;
  APosition.CellProperties.BeginUpdate;
  ApplyTagProperties;
  ApplyCssProperties;

  if APosition.CharacterFormatting.Options.UseForeColor then
    APosition.CharacterFormatting.Options.UseUnderlineColor := False;

  APosition.CellProperties.EndUpdate;
  APosition.ParagraphFormatting.EndUpdate;
  APosition.CharacterFormatting.EndUpdate;
end;

procedure TdxTagBase.OpenTagProcessCore;
begin
end;

procedure TdxTagBase.DeleteOldOpenTag;
begin
end;

procedure TdxTagBase.ParagraphFunctionalProcess;
begin
  ProcessIsEmptyLine;
  ProcessIsEmptyParagraph;
  CopyProperties;
end;

procedure TdxTagBase.ProcessIsEmptyLine;
var
  AParagraphsAllowed, AOldValue: Boolean;
begin
  AParagraphsAllowed := Importer.DocumentModel.DocumentCapabilities.ParagraphsAllowed;
  if Importer.IsEmptyLine and not Importer.IsEmptyParagraph and AParagraphsAllowed then
  begin
    Importer.Position.LogPosition := Importer.Position.LogPosition - 1;
    AOldValue := DocumentModel.ForceNotifyStructureChanged;
    DocumentModel.ForceNotifyStructureChanged := True;
    Importer.PieceTable.DeleteContent(Importer.Position.LogPosition, 1, False);
    Importer.IsEmptyLine := False;
    DocumentModel.ForceNotifyStructureChanged := AOldValue;
  end;
end;

procedure TdxTagBase.ProcessIsEmptyParagraph;
var
  APieceTable: TdxPieceTable;
begin
  APieceTable := Importer.PieceTable;
  if ShouldAddParagraph then
  begin
    if not Importer.DocumentModel.DocumentCapabilities.ParagraphsAllowed then
      Importer.AppendText(' ')
    else
    begin
      ApplyCharacterProperties(APieceTable.Runs.Count - 2);
      Importer.Position.LogPosition := Importer.Position.LogPosition + 1;
      Importer.Position.ParagraphIndex := Importer.Position.ParagraphIndex + 1;
      Importer.AppendParagraph;
      Importer.Position.LogPosition := Importer.Position.LogPosition - 1;
      Importer.Position.ParagraphIndex := Importer.Position.ParagraphIndex - 1;
    end;
    Importer.IsEmptyParagraph := True;
    Importer.IsEmptyListItem := False;
  end;
end;

procedure TdxTagBase.ApplyCharacterProperties(ARunIndex: TdxRunIndex);
var
  ARun: TdxTextRunBase;
begin
  ARun := Importer.PieceTable.Runs[ARunIndex];
  ARun.CharacterProperties.CopyFrom(FCharacterFormatting);
end;

function TdxTagBase.ShouldAddParagraph: Boolean;
begin
  if Importer.IsEmptyListItem then
    Result := False
  else
    Result := not Importer.IsEmptyParagraph;
end;

procedure TdxTagBase.CopyProperties;
var
  APieceTable: TdxPieceTable;
  AParagraphIndex: TdxParagraphIndex;
  ARunIndex: TdxRunIndex;
  AParagraphProperties: TdxParagraphProperties;
begin
  APieceTable := Importer.PieceTable;
  AParagraphIndex := APieceTable.Paragraphs.Last.Index - 1;
  ARunIndex := APieceTable.Runs.Count - 2;

  AParagraphProperties := TdxParagraphProperties.Create(Importer.DocumentModel);
  try
    AParagraphProperties.BeginInit;
    AParagraphProperties.CopyFrom(Importer.Position.ParagraphFormatting);
    AParagraphProperties.LeftIndent := AParagraphProperties.LeftIndent + Importer.Position.AdditionalIndent;
    AParagraphProperties.RightIndent := AParagraphProperties.RightIndent + Importer.Position.AdditionalIndent;
    AParagraphProperties.EndInit;
    APieceTable.Paragraphs[AParagraphIndex].ParagraphProperties.CopyFrom(AParagraphProperties);
  finally
    AParagraphProperties.Free;
  end;

  APieceTable.Runs[ARunIndex].CharacterProperties.CopyFrom(Importer.Position.CharacterFormatting);

  if Importer.Position.ParagraphTabs.Count > 0 then
    APieceTable.Paragraphs[AParagraphIndex].Tabs.SetTabs(Importer.Position.ParagraphTabs);
end;

function TdxTagBase.GetAbsoluteUri(const ABaseUriString: string; const AValue: string): string;
var
  AUri: TdxUri;
  APath: string;
begin
  try
    AUri := CreateUri(ABaseUriString, AValue);
    if not AUri.IsAbsoluteUri then
      Exit(AValue);

    if AUri.IsFileScheme then
    begin
      APath := TPath.GetFullPath(AUri.LocalPath);
      TdxUri.TryCreateAbsoluteUri(APath, AUri);
    end;
    Result := AUri.AbsoluteUri;
  except
    Result := AValue;
  end;
end;

procedure TdxTagBase.AppendContent(const AText: string; AUseRawText: Boolean);
begin
  if AUseRawText then
    Importer.AppendPlainText(AText)
  else
    AppendText(AText);
end;

procedure TdxTagBase.AppendText(const AText: string);
begin
  Importer.AppendText(AText);
end;

function TdxTagBase.CreateUri(const ABaseUriString: string; const AValue: string): TdxUri;
var
  ABaseUri: TdxUri;
begin
  if TdxUri.TryCreateAbsoluteUri(ABaseUriString, ABaseUri) then
    Result := TdxUri.Create(ABaseUri, AValue)
  else
    Result := TdxUri.Create(AValue);
end;

class function TdxTagBase.ImportBorderWidthCore(AUnitConverter: TdxDocumentModelUnitConverter;
  const AValue: string): TdxWidthUnitInfo;
var
  ANamedValue: string;
begin
  ANamedValue := '';
  if AValue = 'thin' then
    ANamedValue := '2px'
  else
    if AValue = 'medium' then
      ANamedValue := '4px'
    else
      if AValue = 'thick' then
        ANamedValue := '6px';

  if ANamedValue = '' then
    ANamedValue := AValue;

  Result := ConvertPixelsValueToWidthUnitInfo(AUnitConverter, ANamedValue);
end;

class function TdxTagBase.ImportColor(AUnitConverter: TdxDocumentModelUnitConverter; const AValue: string): TdxAlphaColor;
begin
  Result := TdxMarkupLanguageColorParser.ParseColor(AValue);
end;

class procedure TdxTagBase.ImportBorderLineStyle(AUnitConverter: TdxDocumentModelUnitConverter; const AValue: string;
  ABorder: TdxHtmlBorderProperty);
var
  AResult: TdxBorderLineStyle;
begin
  AResult := BorderLineStyleTable.GetEnumValue(AValue, TdxBorderLineStyle.ThreeDEmboss);
  if AResult <> TdxBorderLineStyle.ThreeDEmboss then
    ABorder.LineStyle := AResult;
end;

class procedure TdxTagBase.ImportBordersLineStyles(AUnitConverter: TdxDocumentModelUnitConverter; const AValue: string;
  ABordersProperties: TdxHtmlBordersProperties);
var
  AValueNormalizedSpaces: string;
  ALineStyles: TArray<string>;
  ATop, ABottom, ALeft, ARight: TdxBorderLineStyle;
  ALength: Integer;
begin
  if AValue = '' then
    Exit;
  AValueNormalizedSpaces := TdxStringHelper.Replace(AValue, '  ', ' ');
  ALineStyles := TdxStringHelper.Split(AValueNormalizedSpaces, [' ']);
  ALength := Length(ALineStyles);

  case ALength of
    4:
      begin
        ATop := BorderLineStyleTable.GetEnumValue(ALineStyles[0], TdxBorderLineStyle.ThreeDEmboss);
        ARight := BorderLineStyleTable.GetEnumValue(ALineStyles[1], TdxBorderLineStyle.ThreeDEmboss);
        ABottom := BorderLineStyleTable.GetEnumValue(ALineStyles[2], TdxBorderLineStyle.ThreeDEmboss);
        ALeft := BorderLineStyleTable.GetEnumValue(ALineStyles[3], TdxBorderLineStyle.ThreeDEmboss);
      end;
    3:
      begin
        ATop := BorderLineStyleTable.GetEnumValue(ALineStyles[0], TdxBorderLineStyle.ThreeDEmboss);
        ALeft := BorderLineStyleTable.GetEnumValue(ALineStyles[1], TdxBorderLineStyle.ThreeDEmboss);
        ARight := ALeft;
        ABottom := BorderLineStyleTable.GetEnumValue(ALineStyles[2], TdxBorderLineStyle.ThreeDEmboss);
      end;
    2:
      begin
        ATop := BorderLineStyleTable.GetEnumValue(ALineStyles[0], TdxBorderLineStyle.ThreeDEmboss);
        ABottom := ATop;
        ALeft := BorderLineStyleTable.GetEnumValue(ALineStyles[1], TdxBorderLineStyle.ThreeDEmboss);
        ARight := ALeft;
      end;
    1:
      begin
        ATop := BorderLineStyleTable.GetEnumValue(ALineStyles[0], TdxBorderLineStyle.ThreeDEmboss);
        if ATop = TdxBorderLineStyle.ThreeDEmboss then
          Exit;
        ALeft := ATop;
        ARight := ATop;
        ABottom := ATop;
      end;
    else
      Exit;
  end;
  if ATop <> TdxBorderLineStyle.ThreeDEmboss then
    ABordersProperties.TopBorder.LineStyle := ATop;
  if ALeft <> TdxBorderLineStyle.ThreeDEmboss then
    ABordersProperties.LeftBorder.LineStyle := ALeft;
  if ARight <> TdxBorderLineStyle.ThreeDEmboss then
    ABordersProperties.RightBorder.LineStyle := ARight;
  if ABottom <> TdxBorderLineStyle.ThreeDEmboss then
    ABordersProperties.BottomBorder.LineStyle := ABottom;
end;

class procedure TdxTagBase.ImportBordersColors(AUnitConverter: TdxDocumentModelUnitConverter; const AValue: string;
  ABordersProperties: TdxHtmlBordersProperties);
var
  AValueNormalizedSpaces: string;
  ADifferentColors: TArray<string>;
  ATopColor, ABottomColor, ALeftColor, ARightColor: TdxAlphaColor;
  ALength: Integer;
begin
  if AValue = '' then
    Exit;
  AValueNormalizedSpaces := TdxStringHelper.Replace(AValue, '  ', ' ');
  ADifferentColors := TdxStringHelper.Split(AValueNormalizedSpaces, [' ']);
  ALength := Length(ADifferentColors);
  if (ALength = 0) or (ALength > 4) then
    Exit;

  case ALength of
    4:
      begin
        ATopColor := ImportColor(AUnitConverter, ADifferentColors[0]);
        ARightColor := ImportColor(AUnitConverter, ADifferentColors[1]);
        ABottomColor := ImportColor(AUnitConverter, ADifferentColors[2]);
        ALeftColor := ImportColor(AUnitConverter, ADifferentColors[3]);
      end;
    3:
      begin
        ATopColor := ImportColor(AUnitConverter, ADifferentColors[0]);
        ALeftColor := ImportColor(AUnitConverter, ADifferentColors[1]);
        ARightColor := ALeftColor;
        ABottomColor := ImportColor(AUnitConverter, ADifferentColors[2]);
      end;
    2:
      begin
        ATopColor := ImportColor(AUnitConverter, ADifferentColors[0]);
        ARightColor := ImportColor(AUnitConverter, ADifferentColors[1]);
        ABottomColor := ATopColor;
        ALeftColor := ARightColor;
      end
    else
      begin
        ATopColor := ImportColor(AUnitConverter, AValue);
        if ATopColor = TdxAlphaColors.Empty then
          Exit;
        ARightColor := ATopColor;
        ABottomColor := ATopColor;
        ALeftColor := ATopColor;
      end;
  end;
  if ATopColor <> TdxAlphaColors.Empty then
    ABordersProperties.TopBorder.Color := ATopColor;
  if ALeftColor <> TdxAlphaColors.Empty then
    ABordersProperties.LeftBorder.Color := ALeftColor;
  if ARightColor <> TdxAlphaColors.Empty then
    ABordersProperties.RightBorder.Color := ARightColor;
  if ABottomColor <> TdxAlphaColors.Empty then
    ABordersProperties.BottomBorder.Color := ABottomColor;
end;

class procedure TdxTagBase.ImportBordersWidths(AUnitConverter: TdxDocumentModelUnitConverter;
  const AValue: string; ABordersProperties: TdxHtmlBordersProperties);
var
  AValueNormalizedSpaces: string;
  ADifferentBorders: TArray<string>;
  ATopWidth, ARightWidth, ABottomWidth, ALeftWidth: TdxWidthUnitInfo;
  ALength: Integer;
begin
  if AValue = '' then
    Exit;
  AValueNormalizedSpaces := TdxStringHelper.Replace(AValue, '  ', ' ');
  ADifferentBorders := TdxStringHelper.Split(AValueNormalizedSpaces, [' ']);
  ALength := Length(ADifferentBorders);

  if (ALength = 0) or (ALength > 4) then
    Exit;

  case ALength of
    4:
      begin
        ATopWidth := ImportBorderWidthCore(AUnitConverter, ADifferentBorders[0]);
        ARightWidth := ImportBorderWidthCore(AUnitConverter, ADifferentBorders[1]);
        ABottomWidth := ImportBorderWidthCore(AUnitConverter, ADifferentBorders[2]);
        ALeftWidth := ImportBorderWidthCore(AUnitConverter, ADifferentBorders[3]);
      end;
    3:
      begin
        ATopWidth := ImportBorderWidthCore(AUnitConverter, ADifferentBorders[0]);
        ALeftWidth := ImportBorderWidthCore(AUnitConverter, ADifferentBorders[1]);
        ARightWidth := ALeftWidth;
        ABottomWidth := ImportBorderWidthCore(AUnitConverter, ADifferentBorders[2]);
      end;
    2:
      begin
        ATopWidth := ImportBorderWidthCore(AUnitConverter, ADifferentBorders[0]);
        ARightWidth := ImportBorderWidthCore(AUnitConverter, ADifferentBorders[1]);
        ABottomWidth := ATopWidth;
        ALeftWidth := ARightWidth;
      end;
    else
    begin
      ATopWidth := ImportBorderWidthCore(AUnitConverter, ADifferentBorders[0]);
      if ATopWidth.&Type = TdxWidthUnitType.&Nil then
      begin
        ATopWidth.Free;
        Exit;
      end;
      ARightWidth := ATopWidth;
      ABottomWidth := ATopWidth;
      ALeftWidth := ATopWidth;
    end;
  end;
  try
    if ATopWidth.&Type <> TdxWidthUnitType.&Nil then
      ABordersProperties.TopBorder.Width := ATopWidth.Value;
    if ALeftWidth.&Type <> TdxWidthUnitType.&Nil then
      ABordersProperties.LeftBorder.Width := ALeftWidth.Value;
    if ARightWidth.&Type <> TdxWidthUnitType.&Nil then
      ABordersProperties.RightBorder.Width := ARightWidth.Value;
    if ABottomWidth.&Type <> TdxWidthUnitType.&Nil then
      ABordersProperties.BottomBorder.Width := ABottomWidth.Value;
  finally
    case ALength of
      4:
        begin
          ATopWidth.Free;
          ARightWidth.Free;
          ABottomWidth.Free;
          ALeftWidth.Free;
        end;
      3:
        begin
          ATopWidth.Free;
          ALeftWidth.Free;
          ABottomWidth.Free;
        end;
      2:
        begin
          ATopWidth.Free;
          ARightWidth.Free;
        end;
      else
        ATopWidth.Free;
    end;
  end;
end;

function TdxTagBase.GetStartIndexAllowedSearchScope: Integer;
begin
  Result := 0;
end;

procedure TdxTagBase.IgnoredMarginPropertiesFromBlockTags;
var
  ATagsStack: TdxList<TdxOpenHtmlTag>;
  AParagraphFormatting: TdxParagraphFormattingBase;
begin
  ATagsStack := Importer.TagsStack;
  AParagraphFormatting := ATagsStack[ATagsStack.Count - 1].OldPosition.ParagraphFormatting;
  if ATagsStack.Count > 0 then
  begin
    Importer.Position.ParagraphFormatting.SpacingAfter := AParagraphFormatting.SpacingAfter;
    Importer.Position.ParagraphFormatting.SpacingBefore := AParagraphFormatting.SpacingBefore;
    Importer.Position.ParagraphFormatting.FirstLineIndent := AParagraphFormatting.FirstLineIndent;
    Importer.Position.ParagraphFormatting.LeftIndent := AParagraphFormatting.LeftIndent;
    Importer.Position.ParagraphFormatting.RightIndent := AParagraphFormatting.RightIndent;
  end;
end;

class function TdxTagBase.ImportAlignment(const AValue: string; var ATargetAlignment: TdxTableRowAlignment): Boolean;
var
  S: string;
begin
  S := LowerCase(AValue);
  Result := True;
  if S = 'right' then
    ATargetAlignment := TdxTableRowAlignment.Right
  else if S = 'center' then
    ATargetAlignment := TdxTableRowAlignment.Center
  else if S = 'justify' then
    ATargetAlignment := TdxTableRowAlignment.Distribute
  else if S = 'left' then
    ATargetAlignment := TdxTableRowAlignment.Left
  else
    Result := False;
end;

end.
