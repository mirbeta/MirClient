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

unit dxRichEdit.Import.CSSParser;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, SysUtils, Classes, Generics.Defaults, Generics.Collections,
  dxCoreClasses, dxCoreGraphics,
  dxRichEdit.NativeApi,

  dxGenerics,
  dxRichEdit.Utils.BatchUpdateHelper,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.History.IndexChangedHistoryItem,
  dxRichEdit.DocumentModel.IndexBasedObject,
  dxRichEdit.Utils.Types,
  dxRichEdit.Utils.ModelUnitConverter,
  dxRichEdit.DocumentModel.UnitConverter,

  dxRichEdit.DocumentModel.CharacterFormatting,
  dxRichEdit.DocumentModel.ParagraphFormatting,
  dxRichEdit.DocumentModel.TableFormatting,
  dxRichEdit.DocumentModel.Section,
  dxRichEdit.DocumentModel.Numbering,
  dxRichEdit.DocumentModel.NumberingFormatting,
  dxRichEdit.DocumentModel.Borders,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.Import.CSSSelectorParser,
  dxRichEdit.Utils.DXUnit,
  dxRichEdit.Platform.Font;

type

  { TdxBorderCollapse }

  TdxBorderCollapse = (
    Separate,
    Collapse
  );

  { TdxHtmlCssFloat }

  TdxHtmlCssFloat = (
    NotSet,
    Left,
    Right,
    None
  );

  { TdxHtmlImageAlignment }

  TdxHtmlImageAlignment = (
    None,
    Left,
    Right,
    Middle,
    Top,
    Bottom
  );

  { TdxHtmlListLevelProperties }

  TdxHtmlListLevelProperties = class
  strict private
    FStart: Integer;
    FLevelPositionIsOutside: Boolean;
    FFormat: TdxNumberingFormat;
    FBulletFontName: string;
    FUseStart: Boolean;
    FUseFormat: Boolean;
    FUseLevelPositionIsOutside: Boolean;
    FUseBulletFontName: Boolean;
    procedure SetStart(const AValue: Integer);
    procedure SetFormat(const AValue: TdxNumberingFormat);
    procedure SetLevelPositionIsOutside(const AValue: Boolean);
    procedure SetBulletFontName(const AValue: string);
  protected
    function IsDefaultProperties: Boolean;
  public
    constructor Create;
    procedure CopyFrom(AValue: TdxHtmlListLevelProperties);
    function ApplyPropertiesToListLevel(ALevel: TdxListLevel): TdxListLevel;
    function MergeWith(AProperties: TdxHtmlListLevelProperties): TdxHtmlListLevelProperties;

    property BulletFontName: string read FBulletFontName write SetBulletFontName;
    property Format: TdxNumberingFormat read FFormat write SetFormat;
    property Start: Integer read FStart write SetStart;
    property LevelPositionIsOutside: Boolean read FLevelPositionIsOutside write SetLevelPositionIsOutside;
    property UseBulletFontName: Boolean read FUseBulletFontName write FUseBulletFontName;
    property UseFormat: Boolean read FUseFormat write FUseFormat;
    property UseStart: Boolean read FUseStart;
  end;

  { TdxHtmlBorderProperty }

  TdxHtmlBorderProperty = class
  strict private
    FWidth: Integer;
    FColor: TdxAlphaColor;
    FLineStyle: TdxBorderLineStyle;
    FUseWidth: Boolean;
    FUseColor: Boolean;
    FUseLineStyle: Boolean;
    procedure SetWidth(const AValue: Integer);
    procedure SetColor(const AValue: TdxAlphaColor);
    procedure SetLineStyle(const AValue: TdxBorderLineStyle);
  public
    constructor Create;
    procedure CopyFrom(ASource: TdxHtmlBorderProperty);
    procedure Apply(ABorder: TdxBorderBase);
    function IsDefaultProperties: Boolean;
    function MergeWith(AOther: TdxHtmlBorderProperty): TdxHtmlBorderProperty;

    property Width: Integer read FWidth write SetWidth;
    property Color: TdxAlphaColor read FColor write SetColor;
    property LineStyle: TdxBorderLineStyle read FLineStyle write SetLineStyle;
    property UseColor: Boolean read FUseColor;
    property UseLineStyle: Boolean read FUseLineStyle;
    property UseWidth: Boolean read FUseWidth;
  end;

  { TdxHtmlBordersProperties }

  TdxHtmlBordersProperties = class
  strict private
    FTopBorder: TdxHtmlBorderProperty;
    FLeftBorder: TdxHtmlBorderProperty;
    FBottomBorder: TdxHtmlBorderProperty;
    FRightBorder: TdxHtmlBorderProperty;
  protected
    constructor Create(ATopBorder, ALeftBorder, ABottomBorder, ARightBorder: TdxHtmlBorderProperty); overload;
  public
    constructor Create; overload;
    destructor Destroy; override;
    procedure CopyFrom(ASource: TdxHtmlBordersProperties);
    function IsDefaultProperties: Boolean;
    function MergeWith(AOther: TdxHtmlBordersProperties): TdxHtmlBordersProperties;

    property TopBorder: TdxHtmlBorderProperty read FTopBorder write FTopBorder;
    property LeftBorder: TdxHtmlBorderProperty read FLeftBorder write FLeftBorder;
    property BottomBorder: TdxHtmlBorderProperty read FBottomBorder write FBottomBorder;
    property RightBorder: TdxHtmlBorderProperty read FRightBorder write FRightBorder;
  end;

  { TdxHtmlTableProperties }

  TdxHtmlTableProperties = class
  strict private
    FWidth: TdxWidthUnitInfo;
    FCellMargins: TdxWidthUnitInfo;
    FCellSpacing: TdxWidthUnitInfo;
    FBackgroundColor: TdxAlphaColor;
    FSetInnerBorders: Boolean;
    FUseWidth: Boolean;
    FUseCellMargins: Boolean;
    FUseCellSpacing: Boolean;
    FUseBackgroundColor: Boolean;
    FBordersProperties: TdxHtmlBordersProperties;
    FTableAlignment: TdxTableRowAlignment;
    FUseTableAlignment: Boolean;
    FIndent: TdxWidthUnitInfo;
    FUseIndent: Boolean;
    FBorderCollapse: TdxBorderCollapse;
    FUseBorderCollapse: Boolean;
    procedure SetWidth(const AValue: TdxWidthUnitInfo);
    procedure SetCellMargin(const AValue: TdxWidthUnitInfo);
    procedure SetCellSpacing(const AValue: TdxWidthUnitInfo);
    procedure SetIndent(const AValue: TdxWidthUnitInfo);
    procedure SetBackgroundColor(const AValue: TdxAlphaColor);
    procedure SetTableAlignment(const AValue: TdxTableRowAlignment);
    procedure SetBorderCollapse(const AValue: TdxBorderCollapse);
  private
    procedure SetBordersProperties(const Value: TdxHtmlBordersProperties);
  protected
    function IsDefaultProperties: Boolean;

  public
    constructor Create;
    destructor Destroy; override;
    procedure ApplyBackgroundColorToCell(AProperties: TdxTableCellProperties);
    procedure ApplyPropertiesToCharacter(AProperties: TdxCharacterFormattingBase);
    procedure ApplyPropertiesToTable(AProperties: TdxTableProperties);
    procedure CopyFrom(AValue: TdxHtmlTableProperties);
    function MergeWith(AProperties: TdxHtmlTableProperties): TdxHtmlTableProperties;

    property Width: TdxWidthUnitInfo read FWidth write SetWidth;
    property CellMargin: TdxWidthUnitInfo read FCellMargins write SetCellMargin;
    property CellSpacing: TdxWidthUnitInfo read FCellSpacing write SetCellSpacing;
    property Indent: TdxWidthUnitInfo read FIndent write SetIndent;
    property BordersProperties: TdxHtmlBordersProperties read FBordersProperties write SetBordersProperties;
    property BackgroundColor: TdxAlphaColor read FBackgroundColor write SetBackgroundColor;
    property UseBackgroundColor: Boolean read FUseBackgroundColor;
    property TableAlignment: TdxTableRowAlignment read FTableAlignment write SetTableAlignment;
    property BorderCollapse: TdxBorderCollapse read FBorderCollapse write SetBorderCollapse;
    property SetInnerBorders: Boolean read FSetInnerBorders write FSetInnerBorders;
  end;

  { TdxHtmlTableRowProperties }

  TdxHtmlTableRowProperties = class
  strict private
    FHeight: TdxHeightUnitInfo;
    FUseHeight: Boolean;
    FBackgroundColor: TdxAlphaColor;
    FUseBackgroundColor: Boolean;
    FVerticalAlignment: TdxVerticalAlignment;
    FUseVerticalAlignment: Boolean;
    procedure SetHeight(const AValue: TdxHeightUnitInfo);
    procedure SetBackgroundColor(const AValue: TdxAlphaColor);
    procedure SetVerticalAlignment(const AValue: TdxVerticalAlignment);
  protected
    function IsDefaultProperties: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ApplyBackgroundColorToCell(AProperties: TdxTableCellProperties);
    procedure ApplyPropertiesToCharacter(ACharacterFormattingBase: TdxCharacterFormattingBase);
    procedure ApplyPropertiesToRow(AProperties: TdxTableRowProperties);
    procedure ApplyVerticalAlignmentToCell(AProperties: TdxTableCellProperties);
    procedure CopyFrom(AValue: TdxHtmlTableRowProperties);
    function MergeWith(AProperties: TdxHtmlTableRowProperties): TdxHtmlTableRowProperties;

    property Height: TdxHeightUnitInfo read FHeight write SetHeight;
    property UseHeight: Boolean read FUseHeight;
    property BackgroundColor: TdxAlphaColor read FBackgroundColor write SetBackgroundColor;
    property VerticalAlignment: TdxVerticalAlignment read FVerticalAlignment write SetVerticalAlignment;
  end;

  { TdxHtmlImageProperties }

  TdxHtmlImageProperties = class
  strict private
    FWidth: TdxWidthUnitInfo;
    FHeight: TdxWidthUnitInfo;
    FCssFloat: TdxHtmlCssFloat;
    FUseWidth: Boolean;
    FUseHeight: Boolean;
    FUseCssFloat: Boolean;
    FAlignment: TdxHtmlImageAlignment;
    procedure SetWidth(const AValue: TdxWidthUnitInfo);
    procedure SetHeight(const AValue: TdxWidthUnitInfo);
    procedure SetCssFloat(const AValue: TdxHtmlCssFloat);
  protected
    function IsDefaultProperties: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure CopyFrom(AValue: TdxHtmlImageProperties);
    function MergeWith(AProperties: TdxHtmlImageProperties): TdxHtmlImageProperties;
    procedure ResetFloat;

    property Width: TdxWidthUnitInfo read FWidth write SetWidth;
    property UseWidth: Boolean read FUseWidth;
    property Height: TdxWidthUnitInfo read FHeight write SetHeight;
    property UseHeight: Boolean read FUseHeight;
    property CssFloat: TdxHtmlCssFloat read FCssFloat write SetCssFloat;
    property UseCssFloat: Boolean read FUseCssFloat;
    property Alignment: TdxHtmlImageAlignment read FAlignment write FAlignment;
  end;

  { TdxRelativeProperties }

  PdxRelativeProperties = ^TdxRelativeProperties;
  TdxRelativeProperties = record
  strict private
    FHasRelativeProperties: Boolean;
    FUnitRelativeFontSize: string;
    FUnitRelativeFirstLineIndent: string;
    FUnitRelativeLeftIndent: string;
    FUnitRelativeRightIndent: string;
    FUnitRelativeLineSpacing: string;
    FUnitRelativeSpacingAfter: string;
    FUnitRelativeSpacingBefore: string;
    FUnitRelativeWidth: string;
    FUnitRelativeHeight: string;
    FRelativeWidth: Single;
    FRelativeHeight: Single;
    procedure SetUnitRelativeFontSize(const AValue: string);
    procedure SetUnitRelativeFirstLineIndent(const AValue: string);
    procedure SetUnitRelativeLeftIndent(const AValue: string);
    procedure SetUnitRelativeRightIndent(const AValue: string);
    procedure SetUnitRelativeLineSpacing(const AValue: string);
    procedure SetUnitRelativeSpacingAfter(const AValue: string);
    procedure SetUnitRelativeSpacingBefore(const AValue: string);
    procedure SetUnitRelativeWidth(const AValue: string);
    procedure SetUnitRelativeHeight(const AValue: string);

    procedure OnPropertyChanged(const AValue: string);
  public
    procedure Clear;
    function IsDefaultProperties: Boolean;
    class function IsRelative(const ARelativeUnit: string): Boolean; static;

    property HasRelativeProperties: Boolean read FHasRelativeProperties;
    property UnitRelativeFontSize: string read FUnitRelativeFontSize write SetUnitRelativeFontSize;
    property UnitRelativeFirstLineIndent: string read FUnitRelativeFirstLineIndent write SetUnitRelativeFirstLineIndent;
    property UnitRelativeLeftIndent: string read FUnitRelativeLeftIndent write SetUnitRelativeLeftIndent;
    property UnitRelativeRightIndent: string read FUnitRelativeRightIndent write SetUnitRelativeRightIndent;
    property UnitRelativeLineSpacing: string read FUnitRelativeLineSpacing write SetUnitRelativeLineSpacing;
    property UnitRelativeSpacingAfter: string read FUnitRelativeSpacingAfter write SetUnitRelativeSpacingAfter;
    property UnitRelativeSpacingBefore: string read FUnitRelativeSpacingBefore write SetUnitRelativeSpacingBefore;
    property UnitRelativeWidth: string read FUnitRelativeWidth write SetUnitRelativeWidth;
    property UnitRelativeHeight: string read FUnitRelativeHeight write SetUnitRelativeHeight;
    property RelativeWidth: Single read FRelativeWidth write FRelativeWidth;
    property RelativeHeight: Single read FRelativeHeight write FRelativeHeight;
  end;

  { TdxUnitConversionParameters }

  TdxUnitConversionParameters = record
  strict private
    FOriginalValue: Integer;
    FEm: Single;
    FEx: Single;
    FDpi: Single;
    class function CreateEmptyUnitConversionParameters: TdxUnitConversionParameters; static;
  public

    property OriginalValue: Integer read FOriginalValue write FOriginalValue;
    property Em: Single read FEm write FEm;
    property Ex: Single read FEx write FEx;
    property Dpi: Single read FDpi write FDpi;
    class property Empty: TdxUnitConversionParameters read CreateEmptyUnitConversionParameters;
  end;

  { TdxCssProperties }

  TdxCssProperties = class(TcxIUnknownObject, IdxCellPropertiesOwner)
  strict private
    FParagraphProperties: TdxParagraphFormattingBase;
    FCharacterProperties: TdxCharacterFormattingBase;
    FListLevelProperties: TdxHtmlListLevelProperties;
    FTableProperties: TdxHtmlTableProperties;
    FCellProperties: TdxTableCellProperties;
    FRowProperties: TdxHtmlTableRowProperties;
    FRelativeProperties: TdxRelativeProperties;
    FBordersProperties: TdxHtmlBordersProperties;
    FImageProperties: TdxHtmlImageProperties;
    FCssFloat: TdxHtmlCssFloat;
    FPieceTable: TdxPieceTable;
    function GetRelativeProperties: PdxRelativeProperties;
    function GetUnitConverter: TdxDocumentModelUnitConverter;
  protected
    function IsDefaultProperties: Boolean;
    procedure Reset(ADocumentModel: TdxDocumentModel);
    function CreateCellPropertiesChangedHistoryItem(AProperties: TdxTableCellProperties):
      TdxIndexChangedHistoryItemCore;
  public
    constructor Create(APieceTable: TdxPieceTable);
    destructor Destroy; override;
    procedure CopyFrom(AValue: TdxCssProperties);

    property PieceTable: TdxPieceTable read FPieceTable;
    property CssParagraphProperties: TdxParagraphFormattingBase read FParagraphProperties;
    property CssCharacterProperties: TdxCharacterFormattingBase read FCharacterProperties;
    property ListLevelProperties: TdxHtmlListLevelProperties read FListLevelProperties;
    property TableProperties: TdxHtmlTableProperties read FTableProperties;
    property CellProperties: TdxTableCellProperties read FCellProperties;
    property RowProperties: TdxHtmlTableRowProperties read FRowProperties;
    property BordersProperties: TdxHtmlBordersProperties read FBordersProperties;
    property ImageProperties: TdxHtmlImageProperties read FImageProperties;
    property CssFloat: TdxHtmlCssFloat read FCssFloat write FCssFloat;
    property RelativeProperties: PdxRelativeProperties read GetRelativeProperties;

    property UnitConverter: TdxDocumentModelUnitConverter read GetUnitConverter;
  end;

  { TdxCssKeywordTranslatorTable }

  TdxCssPropertiesTranslateHandler = reference to procedure(ACssProperties: TdxCssProperties;
    const APropertiesValues: TArray<string>);
  TdxCssKeywordTranslatorTable = class(TdxNamedDelegateDictionary<TdxCssPropertiesTranslateHandler>);

  { TdxCssElement }

  TdxCssElement = class
  strict private
    FIndex: Integer;
    FSelector: TdxSelector;
    FProperties: TdxCssProperties;
    procedure SetSelector(const Value: TdxSelector);
  public
    constructor Create(APieceTable: TdxPieceTable; AIndex: Integer);
    destructor Destroy; override;

    property Index: Integer read FIndex;
    property Selector: TdxSelector read FSelector write SetSelector;
    property Properties: TdxCssProperties read FProperties;
  end;

  { TdxCssElementCollection }

  TdxCssElementCollection = TdxList<TdxCssElement>;

  { TdxCssParserState }

  TdxCssParserState = (
    ReadSelector,
    ReadPropertiesName,
    WaitColonSymbol,
    SkipProperties,
    WaitPropertiesValue,
    ReadPropertiesValueInQuotes,
    ReadPropertiesValueInApostrophe,
    ReadPropertiesValueInParentheses,
    ReadPropertiesValue,
    WaitPropertiesKeyword,
    ReadPropertiesKeyword,
    WaitStartComment,
    ReadComment,
    WaitEndComment,
    SkipNestedProperties
  );

  { TdxFontProperties }

  TdxFontProperties = (
    Style = 0,
    Variant,
    Weight,
    Size,
    Family
  );

  { TdxCustomCssParser }

  TdxCustomCssParser = class
  public type
    TUnitConverter = record
    strict private
      FUnitConverter: TdxDocumentModelUnitConverter;
    public
      constructor Create(AUnitConverter: TdxDocumentModelUnitConverter);
      function ToModelUnits(AValue: TdxUnit): Integer; overload;
      function DegreeToModelUnits(AValue: TdxRotationUnit): Integer;
      function FDToModelUnits(AValue: TdxRotationUnit): Integer;
      function ToModelUnits(AValue: TdxUnit; AParameters: TdxUnitConversionParameters): Integer; overload;
      function ToPoints(AValue: TdxUnit; AParameters: TdxUnitConversionParameters): Single;
    end;
  strict private
    FDocumentModel: TdxDocumentModel;
    FSelectorsText: string;
    FSelectors: TdxList<TdxSelector>;
    FCssElementCollection: TdxCssElementCollection;
    FState: TdxCssParserState;
    FParentState: TdxCssParserState;
    FProperties: TStringBuilder;
    FPropertiesValue: TStringBuilder;
    FPropertiesKeyword: TStringBuilder;
    FPropertiesValues: TdxStringList;
    FCssProperties: TdxCssProperties;
    FBracketCount: Integer;
    function GetIsUpdateLocked: Boolean;
  protected
    class function ConvertKeyToUpper(const AKey: string): string; static; inline;
    class function CompareNoCase(const AStr1: string; const AStr2: string): Boolean; static; inline;

    function GetCssKeywordTable: TdxCssKeywordTranslatorTable; virtual; abstract;
    procedure BeginUpdate;
    procedure EndUpdate;
    function ParseCssElements(AReader: TTextReader): TdxCssElementCollection;
    procedure SetCssParserState(ANewState: TdxCssParserState); virtual;
    procedure ParseSelectors(ACh: Char);
    procedure ReadPropertiesName(ACh: Char);
    procedure WaitPropertiesValue(ACh: Char);
    procedure ReadPropertiesValue(ACh: Char);
    procedure ReadPropertiesValueInQuotes(ACh: Char);
    procedure ReadPropertiesValueInApostrophe(ACh: Char);
    procedure ReadPropertiesValueInParentheses(ACh: Char);
    procedure WaitPropertiesKeyword(ACh: Char);
    procedure ReadPropertiesKeyword(ACh: Char);
    procedure WaitColonSymbol(ACh: Char);
    function CheckGeneralEvent(ACh: Char): Boolean;
    procedure SkipNestedProperties(ACh: Char);
    procedure EndProperty; virtual;
    procedure EndCssElement;
    procedure ClearProperties;
    procedure WaitStartComment(ACh: Char);
    procedure WaitEndComment(ACh: Char);
    procedure ReadComment(ACh: Char);
    procedure FindKeywordInTable; virtual;
    procedure AddCssElements;
    procedure AddCssElement(ASelector: TdxSelector);

    property CssKeywordTable: TdxCssKeywordTranslatorTable read GetCssKeywordTable;
    property IsUpdateLocked: Boolean read GetIsUpdateLocked;
  public
    constructor Create(ADocumentModel: TdxDocumentModel);
    destructor Destroy; override;

    function ParseAttribute(AReader: TTextReader): TdxCssElementCollection; virtual;
    function Parse(AReader: TTextReader): TdxCssElementCollection; virtual;
  end;

implementation

uses
  Contnrs, Character,
  dxRichEdit.Utils.Exceptions,
  dxStringHelper,
  dxMeasurementUnits;

{ TdxHtmlListLevelProperties }

constructor TdxHtmlListLevelProperties.Create;
begin
  inherited Create;
  FStart := 1;
  FLevelPositionIsOutside := False;
  FFormat := TdxNumberingFormat.Decimal;
end;

procedure TdxHtmlListLevelProperties.SetStart(const AValue: Integer);
begin
  if AValue < 0 then
    TdxRichEditExceptions.ThrowArgumentException('Start', AValue);
  FStart := AValue;
  FUseStart := True;
end;

procedure TdxHtmlListLevelProperties.SetFormat(const AValue: TdxNumberingFormat);
begin
  FFormat := AValue;
  FUseFormat := True;
end;

procedure TdxHtmlListLevelProperties.SetLevelPositionIsOutside(const AValue: Boolean);
begin
  FLevelPositionIsOutside := AValue;
  FUseLevelPositionIsOutside := True;
end;

procedure TdxHtmlListLevelProperties.SetBulletFontName(const AValue: string);
begin
  FBulletFontName := AValue;
  FUseBulletFontName := True;
end;

procedure TdxHtmlListLevelProperties.CopyFrom(AValue: TdxHtmlListLevelProperties);
begin
  FStart := AValue.Start;
  FLevelPositionIsOutside := AValue.LevelPositionIsOutside;
  FFormat := AValue.Format;
  FBulletFontName := AValue.FBulletFontName;
  FUseFormat := AValue.FUseFormat;
  FUseLevelPositionIsOutside := AValue.FUseLevelPositionIsOutside;
  FUseStart := AValue.FUseStart;
  FUseBulletFontName := AValue.FUseBulletFontName;
end;

function TdxHtmlListLevelProperties.MergeWith(AProperties: TdxHtmlListLevelProperties): TdxHtmlListLevelProperties;
begin
  Result := TdxHtmlListLevelProperties.Create;
  if FUseFormat then
    Result.Format := Format
  else
    if AProperties.FUseFormat then
      Result.Format := AProperties.Format;

  if FUseLevelPositionIsOutside then
    Result.LevelPositionIsOutside := LevelPositionIsOutside
  else
    if AProperties.FUseLevelPositionIsOutside then
      Result.LevelPositionIsOutside := AProperties.LevelPositionIsOutside;

  if FUseStart then
    Result.Start := Start
  else
    if AProperties.FUseStart then
      Result.Start := AProperties.Start;

  if FUseBulletFontName then
    Result.BulletFontName := BulletFontName
  else
    if AProperties.FUseBulletFontName then
      Result.BulletFontName := AProperties.BulletFontName;
end;

function TdxHtmlListLevelProperties.ApplyPropertiesToListLevel(ALevel: TdxListLevel): TdxListLevel;
begin
  if FUseBulletFontName then
    ALevel.CharacterProperties.FontName := BulletFontName;
  if FUseFormat then
    ALevel.ListLevelProperties.Format := Format;
  if FUseStart then
    ALevel.ListLevelProperties.Start := Start;
  Result := ALevel;
end;

function TdxHtmlListLevelProperties.IsDefaultProperties: Boolean;
begin
  Result := not FUseFormat and not FUseLevelPositionIsOutside;
end;

{ TdxHtmlBorderProperty }

constructor TdxHtmlBorderProperty.Create;
begin
  FColor := TdxAlphaColors.Empty;
end;

procedure TdxHtmlBorderProperty.SetWidth(const AValue: Integer);
begin
  FWidth := AValue;
  FUseWidth := True;
end;

procedure TdxHtmlBorderProperty.SetColor(const AValue: TdxAlphaColor);
begin
  FColor := AValue;
  FUseColor := True;
end;

procedure TdxHtmlBorderProperty.SetLineStyle(const AValue: TdxBorderLineStyle);
begin
  FLineStyle := AValue;
  FUseLineStyle := True;
end;

procedure TdxHtmlBorderProperty.CopyFrom(ASource: TdxHtmlBorderProperty);
begin
  FWidth := ASource.FWidth;
  FColor := ASource.FColor;
  FLineStyle := ASource.FLineStyle;
  FUseWidth := ASource.FUseWidth;
  FUseColor := ASource.FUseColor;
  FUseLineStyle := ASource.FUseLineStyle;
end;

procedure TdxHtmlBorderProperty.Apply(ABorder: TdxBorderBase);
begin
  if UseWidth then
    ABorder.Width := Width;
  if UseColor then
    ABorder.Color := Color;
  if UseLineStyle then
  begin
    if (not UseWidth) or (Width > 0) then
      ABorder.Style := LineStyle
    else
      ABorder.Style := TdxBorderLineStyle.None;
  end;
end;

function TdxHtmlBorderProperty.IsDefaultProperties: Boolean;
begin
  Result := (not UseWidth and not UseColor) and not UseLineStyle;
end;

function TdxHtmlBorderProperty.MergeWith(AOther: TdxHtmlBorderProperty): TdxHtmlBorderProperty;
begin
  Result := TdxHtmlBorderProperty.Create;
  if UseLineStyle then
    Result.LineStyle := LineStyle
  else
    if AOther.UseLineStyle then
      Result.LineStyle := AOther.LineStyle;

  if UseColor then
    Result.Color := Color
  else
    if AOther.UseColor then
      Result.Color := AOther.Color;

  if UseWidth then
    Result.Width := Width
  else
    if AOther.UseWidth then
      Result.Width := AOther.Width;
end;

{ TdxHtmlBordersProperties }

constructor TdxHtmlBordersProperties.Create;
begin
  inherited Create;
  FTopBorder := TdxHtmlBorderProperty.Create;
  FLeftBorder := TdxHtmlBorderProperty.Create;
  FBottomBorder := TdxHtmlBorderProperty.Create;
  FRightBorder := TdxHtmlBorderProperty.Create;
end;

constructor TdxHtmlBordersProperties.Create(ATopBorder, ALeftBorder, ABottomBorder,
  ARightBorder: TdxHtmlBorderProperty);
begin
  inherited Create;
  FTopBorder := ATopBorder;
  FLeftBorder := ALeftBorder;
  FBottomBorder := ABottomBorder;
  FRightBorder := ARightBorder;
end;

destructor TdxHtmlBordersProperties.Destroy;
begin
  FTopBorder.Free;
  FLeftBorder.Free;
  FBottomBorder.Free;
  FRightBorder.Free;
  inherited Destroy;
end;

procedure TdxHtmlBordersProperties.CopyFrom(ASource: TdxHtmlBordersProperties);
begin
  TopBorder.CopyFrom(ASource.TopBorder);
  LeftBorder.CopyFrom(ASource.LeftBorder);
  RightBorder.CopyFrom(ASource.RightBorder);
  BottomBorder.CopyFrom(ASource.BottomBorder);
end;

function TdxHtmlBordersProperties.IsDefaultProperties: Boolean;
begin
  Result := ((TopBorder.IsDefaultProperties and LeftBorder.IsDefaultProperties) and RightBorder.IsDefaultProperties) and
    BottomBorder.IsDefaultProperties;
end;

function TdxHtmlBordersProperties.MergeWith(AOther: TdxHtmlBordersProperties): TdxHtmlBordersProperties;
begin
  Result := TdxHtmlBordersProperties.Create(
    TopBorder.MergeWith(AOther.TopBorder),
    LeftBorder.MergeWith(AOther.LeftBorder),
    BottomBorder.MergeWith(AOther.BottomBorder),
    RightBorder.MergeWith(AOther.RightBorder));
end;

{ TdxHtmlTableProperties }

constructor TdxHtmlTableProperties.Create;
begin
  inherited Create;
  FWidth := TdxWidthUnitInfo.Create;
  FCellMargins := TdxWidthUnitInfo.Create;
  FCellSpacing := TdxWidthUnitInfo.Create;
  FIndent := TdxWidthUnitInfo.Create;
  FBordersProperties := TdxHtmlBordersProperties.Create;

  FBackgroundColor := TdxAlphaColors.Empty;
  FSetInnerBorders := False;
  FTableAlignment := TdxTableRowAlignment.Left;
end;

destructor TdxHtmlTableProperties.Destroy;
begin
  FWidth.Free;
  FCellMargins.Free;
  FCellSpacing.Free;
  FIndent.Free;
  FBordersProperties.Free;
  inherited Destroy;
end;

procedure TdxHtmlTableProperties.SetWidth(const AValue: TdxWidthUnitInfo);
begin
  if AValue.Value < 0 then
    TdxRichEditExceptions.ThrowArgumentException('Width', AValue.Value);
  FWidth.CopyFrom(AValue);
  FUseWidth := True;
end;

procedure TdxHtmlTableProperties.SetCellMargin(const AValue: TdxWidthUnitInfo);
begin
  if AValue.Value < 0 then
    TdxRichEditExceptions.ThrowArgumentException('CellMargins', AValue.Value);
  FCellMargins.CopyFrom(AValue);
  FUseCellMargins := True;
end;

procedure TdxHtmlTableProperties.SetCellSpacing(const AValue: TdxWidthUnitInfo);
begin
  if AValue.Value < 0 then
    TdxRichEditExceptions.ThrowArgumentException('CellSpacing', AValue.Value);
  FCellSpacing.CopyFrom(AValue);
  FUseCellSpacing := True;
end;

procedure TdxHtmlTableProperties.SetIndent(const AValue: TdxWidthUnitInfo);
begin
  FIndent.CopyFrom(AValue);
  FUseIndent := True;
end;

procedure TdxHtmlTableProperties.SetBackgroundColor(const AValue: TdxAlphaColor);
begin
  FBackgroundColor := AValue;
  FUseBackgroundColor := True;
end;

procedure TdxHtmlTableProperties.SetTableAlignment(const AValue: TdxTableRowAlignment);
begin
  FTableAlignment := AValue;
  FUseTableAlignment := True;
end;

procedure TdxHtmlTableProperties.SetBorderCollapse(const AValue: TdxBorderCollapse);
begin
  FBorderCollapse := AValue;
  FUseBorderCollapse := True;
end;

procedure TdxHtmlTableProperties.SetBordersProperties(const Value: TdxHtmlBordersProperties);
begin
  FBordersProperties.CopyFrom(Value);
end;

procedure TdxHtmlTableProperties.CopyFrom(AValue: TdxHtmlTableProperties);
begin
  FWidth.CopyFrom(AValue.FWidth);
  FCellMargins.CopyFrom(AValue.CellMargin);
  FCellSpacing.CopyFrom(AValue.CellSpacing);
  FBorderCollapse := AValue.FBorderCollapse;

  FBordersProperties.CopyFrom(AValue.BordersProperties);
  FBackgroundColor := AValue.FBackgroundColor;
  FTableAlignment := AValue.FTableAlignment;

  FUseWidth := AValue.FUseWidth;
  FUseCellMargins := AValue.FUseCellMargins;
  FUseCellSpacing := AValue.FUseCellSpacing and ((AValue.CellSpacing.Value > 0) or
    (AValue.CellSpacing.&Type = TdxWidthUnitType.ModelUnits));
  FUseBorderCollapse := AValue.FUseBorderCollapse;

  FUseBackgroundColor := AValue.FUseBackgroundColor;
  FSetInnerBorders := AValue.FSetInnerBorders;
  FUseTableAlignment := AValue.FUseTableAlignment;
  FIndent.CopyFrom(AValue.FIndent);
  FUseIndent := AValue.FUseIndent;
end;

function TdxHtmlTableProperties.MergeWith(AProperties: TdxHtmlTableProperties): TdxHtmlTableProperties;
var
  ABordersProperties: TdxHtmlBordersProperties;
begin
  Result := TdxHtmlTableProperties.Create;
  if FUseWidth then
    Result.Width := Width
  else
    if AProperties.FUseWidth then
      Result.Width := AProperties.Width;

  if FUseCellMargins then
    Result.CellMargin := CellMargin
  else
    if AProperties.FUseCellMargins then
      Result.CellMargin := AProperties.CellMargin;

  if FUseCellSpacing then
    Result.CellSpacing := CellSpacing
  else
    if AProperties.FUseCellSpacing then
      Result.CellSpacing := AProperties.cellSpacing;

  if FUseBorderCollapse then
    Result.BorderCollapse := BorderCollapse
  else
    if AProperties.FUseCellSpacing then
      Result.BorderCollapse := AProperties.BorderCollapse;

  ABordersProperties := BordersProperties.MergeWith(AProperties.BordersProperties);
  try
    Result.BordersProperties := ABordersProperties;
  finally
    ABordersProperties.Free;
  end;

  if FUseBackgroundColor then
    Result.BackgroundColor := BackgroundColor
  else
    if AProperties.UseBackgroundColor then
      Result.BackgroundColor := AProperties.BackgroundColor;

  if (FSetInnerBorders) or (AProperties.SetInnerBorders) then
    Result.SetInnerBorders := True;

  if FUseTableAlignment then
    Result.TableAlignment := TableAlignment
  else
    if AProperties.FUseTableAlignment then
      Result.TableAlignment := AProperties.tableAlignment;

  if FUseIndent then
    Result.Indent := Indent
  else
    if AProperties.FUseIndent then
      Result.Indent := AProperties.Indent;
end;

procedure TdxHtmlTableProperties.ApplyPropertiesToTable(AProperties: TdxTableProperties);
var
  AAutoWidth: TdxWidthUnitInfo;
  ASpacingBetweenCellsIsForbidden: Boolean;
  ABorders: TdxTableBorders;
  AInners: TdxBorderInfo;
begin
  if FUseWidth then
    AProperties.PreferredWidth.CopyFrom(Width)
  else
  begin
    AAutoWidth := TdxWidthUnitInfo.Create;
    try
      AAutoWidth.&Type := TdxWidthUnitType.Auto;
      AAutoWidth.Value := 0;
      AProperties.PreferredWidth.CopyFrom(AAutoWidth);
    finally
      AAutoWidth.Free;
    end;
  end;
  if FUseCellMargins then
  begin
    AProperties.CellMargins.Top.CopyFrom(CellMargin);
    AProperties.CellMargins.Left.CopyFrom(CellMargin);
    AProperties.CellMargins.Bottom.CopyFrom(CellMargin);
    AProperties.CellMargins.Right.CopyFrom(CellMargin);
  end;
  ASpacingBetweenCellsIsForbidden := FUseBorderCollapse and (BorderCollapse = TdxBorderCollapse.Collapse);
  if FUseCellSpacing and not ASpacingBetweenCellsIsForbidden then
    AProperties.CellSpacing.CopyFrom(CellSpacing);
  ABorders := AProperties.Borders;
  FBordersProperties.TopBorder.Apply(ABorders.TopBorder);
  FBordersProperties.LeftBorder.Apply(ABorders.LeftBorder);
  FBordersProperties.RightBorder.Apply(ABorders.RightBorder);
  FBordersProperties.BottomBorder.Apply(ABorders.BottomBorder);
  FBordersProperties.LeftBorder.Apply(ABorders.InsideVerticalBorder);
  FBordersProperties.TopBorder.Apply(ABorders.InsideHorizontalBorder);

  if FUseBackgroundColor then
    AProperties.BackgroundColor := BackgroundColor;
  if not SetInnerBorders then
  begin
    AInners := TdxBorderInfo.Create;
    try
      AInners.Width := 0;
      AInners.Style := TdxBorderLineStyle.None;
      AProperties.Borders.InsideHorizontalBorder.CopyFrom(AInners);
      AProperties.Borders.InsideVerticalBorder.CopyFrom(AInners);
    finally
      AInners.Free;
    end;
  end;
  if FUseTableAlignment then
    AProperties.TableAlignment := TableAlignment;

  if FUseIndent then
    AProperties.TableIndent.CopyFrom(Indent);
end;

procedure TdxHtmlTableProperties.ApplyBackgroundColorToCell(AProperties: TdxTableCellProperties);
begin
  if FUseBackgroundColor then
    AProperties.BackgroundColor := BackgroundColor;
end;

procedure TdxHtmlTableProperties.ApplyPropertiesToCharacter(AProperties: TdxCharacterFormattingBase);
begin
  if FUseBackgroundColor then
    AProperties.BackColor := BackgroundColor;
end;

function TdxHtmlTableProperties.IsDefaultProperties: Boolean;
begin
  Result := (((((not FUseWidth and not FUseCellMargins) and not FUseCellSpacing) and
    FBordersProperties.IsDefaultProperties) and not FUseTableAlignment) and not FUseIndent) and not FUseBorderCollapse;
end;

{ TdxHtmlTableRowProperties }

constructor TdxHtmlTableRowProperties.Create;
begin
  inherited Create;
  FHeight := TdxHeightUnitInfo.Create;
end;

destructor TdxHtmlTableRowProperties.Destroy;
begin
  FHeight.Free;
  inherited Destroy;
end;

procedure TdxHtmlTableRowProperties.SetHeight(const AValue: TdxHeightUnitInfo);
begin
  if AValue.Value < 0 then
    TdxRichEditExceptions.ThrowArgumentException('Height', AValue.Value);
  FHeight.CopyFrom(AValue);
  FUseHeight := True;
end;

procedure TdxHtmlTableRowProperties.SetBackgroundColor(const AValue: TdxAlphaColor);
begin
  FBackgroundColor := AValue;
  FUseBackgroundColor := True;
end;

procedure TdxHtmlTableRowProperties.SetVerticalAlignment(const AValue: TdxVerticalAlignment);
begin
  FVerticalAlignment := AValue;
  FUseVerticalAlignment := True;
end;

procedure TdxHtmlTableRowProperties.CopyFrom(AValue: TdxHtmlTableRowProperties);
begin
  FHeight.CopyFrom(AValue.Height);
  FUseHeight := AValue.FUseHeight;
  FBackgroundColor := AValue.BackgroundColor;
  FUseBackgroundColor := AValue.FUseBackgroundColor;
  FVerticalAlignment := AValue.VerticalAlignment;
  FUseVerticalAlignment := AValue.FUseVerticalAlignment;
end;

function TdxHtmlTableRowProperties.MergeWith(AProperties: TdxHtmlTableRowProperties): TdxHtmlTableRowProperties;
begin
  Result := TdxHtmlTableRowProperties.Create;
  if FUseHeight then
    Result.Height := Height
  else
    if AProperties.UseHeight then
      Result.Height := AProperties.Height;

  if FUseBackgroundColor then
    Result.BackgroundColor := BackgroundColor
  else
    if AProperties.FUseBackgroundColor then
      Result.BackgroundColor := AProperties.BackgroundColor;

  if FUseVerticalAlignment then
    Result.VerticalAlignment := VerticalAlignment
  else
    if AProperties.FUseVerticalAlignment then
      Result.VerticalAlignment := AProperties.VerticalAlignment;
end;

procedure TdxHtmlTableRowProperties.ApplyPropertiesToRow(AProperties: TdxTableRowProperties);
begin
  if FUseHeight then
    AProperties.Height.CopyFrom(Height);
end;

procedure TdxHtmlTableRowProperties.ApplyBackgroundColorToCell(AProperties: TdxTableCellProperties);
begin
  if FUseBackgroundColor then
    AProperties.BackgroundColor := BackgroundColor;
end;

procedure TdxHtmlTableRowProperties.ApplyVerticalAlignmentToCell(AProperties: TdxTableCellProperties);
begin
  if FUseVerticalAlignment then
    AProperties.VerticalAlignment := VerticalAlignment;
end;

function TdxHtmlTableRowProperties.IsDefaultProperties: Boolean;
begin
  Result := (not FUseHeight and not FUseBackgroundColor) and not FUseVerticalAlignment;
end;

procedure TdxHtmlTableRowProperties.ApplyPropertiesToCharacter(ACharacterFormattingBase: TdxCharacterFormattingBase);
begin
  if FUseBackgroundColor then
    ACharacterFormattingBase.BackColor := BackgroundColor;
end;

{ TdxHtmlImageProperties }

constructor TdxHtmlImageProperties.Create;
begin
  inherited Create;
  FHeight := TdxWidthUnitInfo.Create;
  FWidth := TdxWidthUnitInfo.Create;
  FAlignment := TdxHtmlImageAlignment.None;
  FCssFloat := TdxHtmlCssFloat.NotSet;
end;

destructor TdxHtmlImageProperties.Destroy;
begin
  FHeight.Free;
  FWidth.Free;
  inherited Destroy;
end;

procedure TdxHtmlImageProperties.SetWidth(const AValue: TdxWidthUnitInfo);
begin
  if AValue.Value < 0 then
    TdxRichEditExceptions.ThrowArgumentException('Width', AValue.Value);
  FWidth.CopyFrom(AValue);
  FUseWidth := True;
end;

procedure TdxHtmlImageProperties.SetHeight(const AValue: TdxWidthUnitInfo);
begin
  if AValue.Value < 0 then
    TdxRichEditExceptions.ThrowArgumentException('Height', AValue.Value);
  FHeight.CopyFrom(AValue);
  FUseHeight := True;
end;

procedure TdxHtmlImageProperties.SetCssFloat(const AValue: TdxHtmlCssFloat);
begin
  FCssFloat := AValue;
  FUseCssFloat := True;
end;

procedure TdxHtmlImageProperties.CopyFrom(AValue: TdxHtmlImageProperties);
begin
  FWidth.CopyFrom(AValue.FWidth);
  FUseWidth := AValue.FUseWidth;
  FHeight.CopyFrom(AValue.FHeight);
  FUseHeight := AValue.FUseHeight;
  FAlignment := AValue.FAlignment;
  FCssFloat := AValue.FCssFloat;
  FUseCssFloat := AValue.FUseCssFloat;
end;

function TdxHtmlImageProperties.MergeWith(AProperties: TdxHtmlImageProperties): TdxHtmlImageProperties;
begin
  Result := TdxHtmlImageProperties.Create;
  if FUseHeight then
    Result.Height := Height
  else
    if AProperties.FUseHeight then
      Result.Height := AProperties.Height;

  if FUseWidth then
    Result.Width := Width
  else
    if AProperties.FUseWidth then
      Result.Width := AProperties.Width;
  Result.Alignment := Alignment;
  if FUseCssFloat then
    Result.CssFloat := CssFloat
  else
    if AProperties.FUseCssFloat then
      Result.CssFloat := AProperties.CssFloat;
end;

function TdxHtmlImageProperties.IsDefaultProperties: Boolean;
begin
  Result := ((not FUseHeight and not FUseWidth) and (Alignment = TdxHtmlImageAlignment.None)) and not FUseCssFloat;
end;

procedure TdxHtmlImageProperties.ResetFloat;
begin
  FUseCssFloat := False;
  FCssFloat := TdxHtmlCssFloat.NotSet;
end;

{ TdxRelativeProperties }

procedure TdxRelativeProperties.Clear;
begin
  FHasRelativeProperties := False;
  FUnitRelativeFirstLineIndent := '';
  FUnitRelativeFontSize := '';
  FUnitRelativeLeftIndent := '';
  FUnitRelativeRightIndent := '';
  FUnitRelativeLineSpacing := '';
  FUnitRelativeSpacingAfter := '';
  FUnitRelativeSpacingBefore := '';
  FUnitRelativeWidth := '';
  FUnitRelativeHeight := '';
end;

procedure TdxRelativeProperties.SetUnitRelativeFontSize(const AValue: string);
begin
  FUnitRelativeFontSize := AValue;
  OnPropertyChanged(AValue);
end;

procedure TdxRelativeProperties.SetUnitRelativeFirstLineIndent(const AValue: string);
begin
  FUnitRelativeFirstLineIndent := AValue;
  OnPropertyChanged(AValue);
end;

procedure TdxRelativeProperties.SetUnitRelativeLeftIndent(const AValue: string);
begin
  FUnitRelativeLeftIndent := AValue;
  OnPropertyChanged(AValue);
end;

procedure TdxRelativeProperties.SetUnitRelativeRightIndent(const AValue: string);
begin
  FUnitRelativeRightIndent := AValue;
  OnPropertyChanged(AValue);
end;

procedure TdxRelativeProperties.SetUnitRelativeLineSpacing(const AValue: string);
begin
  FUnitRelativeLineSpacing := AValue;
  OnPropertyChanged(AValue);
end;

procedure TdxRelativeProperties.SetUnitRelativeSpacingAfter(const AValue: string);
begin
  FUnitRelativeSpacingAfter := AValue;
  OnPropertyChanged(AValue);
end;

procedure TdxRelativeProperties.SetUnitRelativeSpacingBefore(const AValue: string);
begin
  FUnitRelativeSpacingBefore := AValue;
  OnPropertyChanged(AValue);
end;

procedure TdxRelativeProperties.SetUnitRelativeWidth(const AValue: string);
begin
  FUnitRelativeWidth := AValue;
  OnPropertyChanged(AValue);
end;

procedure TdxRelativeProperties.SetUnitRelativeHeight(const AValue: string);
begin
  FUnitRelativeHeight := AValue;
  OnPropertyChanged(AValue);
end;


procedure TdxRelativeProperties.OnPropertyChanged(const AValue: string);
begin
  if not FHasRelativeProperties then
    FHasRelativeProperties := IsRelative(AValue);
end;

class function TdxRelativeProperties.IsRelative(const ARelativeUnit: string): Boolean;
begin
  if ARelativeUnit = '' then
    Result := False
  else
    Result := (ARelativeUnit = 'em') or (ARelativeUnit = 'rem') or (ARelativeUnit = 'ex') or (ARelativeUnit = '%');
end;

function TdxRelativeProperties.IsDefaultProperties: Boolean;
begin
  Result := ((((((((FUnitRelativeFirstLineIndent = '') and (FUnitRelativeFontSize = '')) and
    (FUnitRelativeLeftIndent = '')) and (FUnitRelativeRightIndent = '')) and
    (FUnitRelativeLineSpacing = '')) and (FUnitRelativeSpacingAfter = '')) and
    (FUnitRelativeSpacingBefore = '')) and (FUnitRelativeWidth = '')) and (FUnitRelativeHeight = '');
end;

{ TdxUnitConversionParameters }

class function TdxUnitConversionParameters.CreateEmptyUnitConversionParameters: TdxUnitConversionParameters;
var
  AParameters: TdxUnitConversionParameters;
begin
  AParameters.dpi := 96;
  AParameters.em := 1;
  AParameters.ex := 0.5;
  AParameters.originalValue := 0;
  Result := AParameters;
end;


{ TdxCssProperties }

constructor TdxCssProperties.Create(APieceTable: TdxPieceTable);
begin
  inherited Create;
  Assert(APieceTable <> nil, 'pieceTable');
  FPieceTable := APieceTable;
  FParagraphProperties := APieceTable.DocumentModel.CreateEmptyParagraphFormatting;
  FCharacterProperties := APieceTable.DocumentModel.CreateEmptyCharacterFormatting;
  FCharacterProperties.BeginUpdate;
  FCharacterProperties.FontName := 'Times New Roman';
  FCharacterProperties.DoubleFontSize := 24;
  FCharacterProperties.ResetUse(TdxCharacterFormattingOptions.MaskUseAll);
  FCharacterProperties.EndUpdate;
  FListLevelProperties := TdxHtmlListLevelProperties.Create;
  FTableProperties := TdxHtmlTableProperties.Create;
  FRowProperties := TdxHtmlTableRowProperties.Create;
  FCellProperties := TdxTableCellProperties.Create(APieceTable, Self);
  FRelativeProperties.Clear;
  FBordersProperties := TdxHtmlBordersProperties.Create;
  FImageProperties := TdxHtmlImageProperties.Create;
  FCssFloat := TdxHtmlCssFloat.NotSet;
end;

destructor TdxCssProperties.Destroy;
begin
  FParagraphProperties.Free;
  FCharacterProperties.Free;
  FListLevelProperties.Free;
  FTableProperties.Free;
  FRowProperties.Free;
  FCellProperties.Free;
  FBordersProperties.Free;
  FImageProperties.Free;
  inherited Destroy;
end;

function TdxCssProperties.GetRelativeProperties: PdxRelativeProperties;
begin
  Result := @FRelativeProperties;
end;

function TdxCssProperties.GetUnitConverter: TdxDocumentModelUnitConverter;
begin
  Result := FParagraphProperties.DocumentModel.UnitConverter;
end;

procedure TdxCssProperties.CopyFrom(AValue: TdxCssProperties);
begin
  FCharacterProperties.CopyFrom(AValue.FCharacterProperties);
  FParagraphProperties.CopyFrom(AValue.FParagraphProperties);
  FListLevelProperties.CopyFrom(AValue.FListLevelProperties);
  FTableProperties.CopyFrom(AValue.FTableProperties);
  FRowProperties.CopyFrom(AValue.FRowProperties);
  FCellProperties.CopyFrom(AValue.FCellProperties);
  FRelativeProperties := AValue.FRelativeProperties;
  FBordersProperties.CopyFrom(AValue.FBordersProperties);
  FImageProperties.CopyFrom(AValue.FImageProperties);
  FCssFloat := AValue.CssFloat;
end;

function TdxCssProperties.IsDefaultProperties: Boolean;
begin
  Result :=
    (CssCharacterProperties.Options.Value = TdxCharacterFormattingOptions.MaskUseNone) and
    (CssParagraphProperties.Options.Value = TdxParagraphFormattingOptions.MaskUseNone) and
    ListLevelProperties.IsDefaultProperties and
    TableProperties.IsDefaultProperties and
    RowProperties.IsDefaultProperties and
    BordersProperties.IsDefaultProperties and
    ImageProperties.IsDefaultProperties and
    RelativeProperties.IsDefaultProperties and
    (FCssFloat = TdxHtmlCssFloat.NotSet) and
    (TdxDocumentModel(CellProperties.DocumentModel).Cache.TableCellPropertiesOptionsCache.DefaultItem.Equals(CellProperties.Info));
end;

procedure TdxCssProperties.Reset(ADocumentModel: TdxDocumentModel);
begin
  ADocumentModel.ResetCharacterFormatting(CssCharacterProperties);
  CssCharacterProperties.DoubleFontSize := 24;
  CssCharacterProperties.FontName := 'Times New Roman';
  CssCharacterProperties.ResetUse(TdxCharacterFormattingOptions.MaskUseAll);

  ADocumentModel.ResetParagraphFormatting(CssParagraphProperties);

  FListLevelProperties.Free;
  FListLevelProperties := TdxHtmlListLevelProperties.Create;

  FTableProperties.Free;
  FTableProperties := TdxHtmlTableProperties.Create;

  FRowProperties.Free;
  FRowProperties := TdxHtmlTableRowProperties.Create;

  FBordersProperties.Free;
  FBordersProperties := TdxHtmlBordersProperties.Create;

  FImageProperties.Free;
  FImageProperties := TdxHtmlImageProperties.Create;

  FCellProperties.Reset;
  FRelativeProperties.Clear;
  FCssFloat := TdxHtmlCssFloat.NotSet;
end;

function TdxCssProperties.CreateCellPropertiesChangedHistoryItem(AProperties: TdxTableCellProperties): TdxIndexChangedHistoryItemCore;
begin
  Assert(AProperties = CellProperties);
  Result := TdxIndexChangedHistoryItem.Create(AProperties.PieceTable,
    AProperties);
end;

{ TdxCssElement }

constructor TdxCssElement.Create(APieceTable: TdxPieceTable; AIndex: Integer);
begin
  inherited Create;
  FProperties := TdxCssProperties.Create(APieceTable);
  FSelector := TdxSelector.Create;
  FIndex := AIndex;
end;

destructor TdxCssElement.Destroy;
begin
  FProperties.Free;
  FSelector.Free;
  inherited Destroy;
end;

procedure TdxCssElement.SetSelector(const Value: TdxSelector);
begin
  if FSelector = Value then
    Exit;
  FSelector.Free;
  FSelector := Value;
end;

{ TdxCustomCssParser.TUnitConverter }

constructor TdxCustomCssParser.TUnitConverter.Create(AUnitConverter: TdxDocumentModelUnitConverter);
begin
  Assert(AUnitConverter <> nil, 'unitConverter');
  FUnitConverter := AUnitConverter;
end;

function TdxCustomCssParser.TUnitConverter.ToModelUnits(AValue: TdxUnit): Integer;
begin
  Result := ToModelUnits(AValue, TdxUnitConversionParameters.Empty);
end;

function TdxCustomCssParser.TUnitConverter.DegreeToModelUnits(AValue: TdxRotationUnit): Integer;
begin
  Result := FUnitConverter.DegreeToModelUnits(Trunc(AValue.Value));
end;

function TdxCustomCssParser.TUnitConverter.FDToModelUnits(AValue: TdxRotationUnit): Integer;
begin
  Result := FUnitConverter.FDToModelUnits(Trunc(AValue.Value));
end;

function TdxCustomCssParser.TUnitConverter.ToModelUnits(AValue: TdxUnit; AParameters: TdxUnitConversionParameters): Integer;
begin
  Result := Trunc(FUnitConverter.PointsToModelUnitsF(ToPoints(AValue, AParameters)));
end;

function TdxCustomCssParser.TUnitConverter.ToPoints(AValue: TdxUnit; AParameters: TdxUnitConversionParameters): Single;
begin
  case AValue.&Type of
    TdxUnitType.Em:
      Result := AParameters.OriginalValue * AValue.Value * AParameters.Em;
    TdxUnitType.Ex:
      Result := AParameters.OriginalValue * AValue.Value * AParameters.Ex;
    TdxUnitType.Percentage:
      Result := AParameters.OriginalValue * AValue.Value / 100;
    TdxUnitType.Point:
      Result := AValue.Value;
    TdxUnitType.Pica:
      Result := AValue.Value * 12;
    TdxUnitType.Pixel:
      Result := PixelsToPointsF(AValue.Value, AParameters.Dpi);
    TdxUnitType.Inch:
      Result := AValue.Value * 72;
    TdxUnitType.Cm:
      Result := MillimetersToPointsF(AValue.Value * 10.0);
    TdxUnitType.Mm:
      Result := MillimetersToPointsF(AValue.Value);
    else
      Result := AValue.Value;
  end;
end;

{ TdxCustomCssParser }

constructor TdxCustomCssParser.Create(ADocumentModel: TdxDocumentModel);
begin
  FBracketCount := 0;
  FPropertiesValue := TStringBuilder.Create;
  FProperties := TStringBuilder.Create;
  FPropertiesKeyword := TStringBuilder.Create;
  FDocumentModel := ADocumentModel;
  FSelectors := TdxList<TdxSelector>.Create;
  FCssElementCollection := TdxCssElementCollection.Create;
  FState := TdxCssParserState.ReadSelector;
  FParentState := TdxCssParserState.ReadSelector;
  FPropertiesValues := TdxStringList.Create;
  FCssProperties := TdxCssProperties.Create(ADocumentModel.MainPieceTable);
  FSelectorsText := '';
end;

destructor TdxCustomCssParser.Destroy;
begin
  FPropertiesValue.Free;
  FProperties.Free;
  FPropertiesKeyword.Free;
  FSelectors.Free;
  FCssElementCollection.Free;
  FPropertiesValues.Free;
  FCssProperties.Free;
  inherited Destroy;
end;

class function TdxCustomCssParser.ConvertKeyToUpper(const AKey: string): string;
begin
  Result := UpperCase(AKey);
end;

class function TdxCustomCssParser.CompareNoCase(const AStr1: string; const AStr2: string): Boolean;
begin
  Result := CompareText(AStr1, AStr2) = 0;
end;

function TdxCustomCssParser.GetIsUpdateLocked: Boolean;
begin
  Result :=  FCssProperties.CellProperties.IsUpdateLocked;
end;

function TdxCustomCssParser.Parse(AReader: TTextReader): TdxCssElementCollection;
var
  AIntChar: Integer;
begin
  AIntChar := AReader.Peek;
  if AIntChar < 0 then
    Exit(nil);
  ParseCssElements(AReader);
  Result := FCssElementCollection;
end;

procedure TdxCustomCssParser.BeginUpdate;
begin
  FCssProperties.CssCharacterProperties.BeginUpdate;
  FCssProperties.CssParagraphProperties.BeginUpdate;
  FCssProperties.CellProperties.BeginUpdate;
end;

procedure TdxCustomCssParser.EndUpdate;
begin
  FCssProperties.CellProperties.EndUpdate;
  FCssProperties.CssParagraphProperties.EndUpdate;
  FCssProperties.CssCharacterProperties.EndUpdate;
end;

function TdxCustomCssParser.ParseAttribute(AReader: TTextReader): TdxCssElementCollection;
var
  AIntChar: Integer;
begin
  AIntChar := AReader.Peek;
  if AIntChar < 0 then
    Exit(nil);
  BeginUpdate;
  SetCssParserState(TdxCssParserState.ReadPropertiesName);

  ParseCssElements(AReader);
  EndProperty;
  AddCssElement(TdxSelector.Create);
  Result := FCssElementCollection;
end;

function TdxCustomCssParser.ParseCssElements(AReader: TTextReader): TdxCssElementCollection;
var
  AIntChar: Integer;
  ACh: Char;
begin
  while True do
  begin
    AIntChar := AReader.Read;
    if AIntChar < 0 then
      Break;
    ACh := Char(AIntChar);
    case FState of
      TdxCssParserState.ReadSelector:
        ParseSelectors(ACh);
      TdxCssParserState.ReadPropertiesName:
        ReadPropertiesName(ACh);
      TdxCssParserState.WaitColonSymbol:
        WaitColonSymbol(ACh);
      TdxCssParserState.WaitPropertiesValue:
        WaitPropertiesValue(ACh);
      TdxCssParserState.ReadPropertiesValue:
        ReadPropertiesValue(ACh);
      TdxCssParserState.ReadPropertiesValueInQuotes:
        ReadPropertiesValueInQuotes(ACh);
      TdxCssParserState.ReadPropertiesValueInApostrophe:
        ReadPropertiesValueInApostrophe(ACh);
      TdxCssParserState.ReadPropertiesValueInParentheses:
        ReadPropertiesValueInParentheses(ACh);
      TdxCssParserState.WaitPropertiesKeyword:
        WaitPropertiesKeyword(ACh);
      TdxCssParserState.ReadPropertiesKeyword:
        ReadPropertiesKeyword(ACh);
      TdxCssParserState.WaitStartComment:
        WaitStartComment(ACh);
      TdxCssParserState.ReadComment:
        ReadComment(ACh);
      TdxCssParserState.WaitEndComment:
        WaitEndComment(ACh);
      TdxCssParserState.SkipNestedProperties:
        SkipNestedProperties(ACh);
    end;
  end;
  if IsUpdateLocked then
    EndUpdate;
  Result := FCssElementCollection;
end;

procedure TdxCustomCssParser.SetCssParserState(ANewState: TdxCssParserState);
begin
  FParentState := FState;
  FState := ANewState;
end;

procedure TdxCustomCssParser.ParseSelectors(ACh: Char);
var
  ASelectorParser: TdxSelectorParser;
begin
  if ACh = '{' then
  begin
    ASelectorParser := TdxSelectorParser.Create(FSelectorsText);
    try
      FSelectorsText := '';
      FSelectors.Clear;
      FSelectors.AddRange(ASelectorParser.Parse);
    finally
      ASelectorParser.Free;
    end;
    FCssProperties.Reset(FDocumentModel);
    BeginUpdate;
    SetCssParserState(TdxCssParserState.ReadPropertiesName);
  end
  else
    if ACh = '/' then
      SetCssParserState(TdxCssParserState.WaitStartComment)
    else
      FSelectorsText := FSelectorsText + ACh;
end;

procedure TdxCustomCssParser.ReadPropertiesName(ACh: Char);
begin
  if CheckGeneralEvent(ACh) then
    Exit;
{$IFDEF DELPHIXE4}
  if ACh.IsWhiteSpace then
{$ELSE}
  if TCharacter.IsWhiteSpace(ACh) then
{$ENDIF}
  begin
    if FProperties.Length > 0 then
      SetCssParserState(TdxCssParserState.WaitColonSymbol);
    Exit;
  end;
  if ACh = ':' then
    SetCssParserState(TdxCssParserState.WaitPropertiesValue)
  else
    FProperties.Append(ACh);
end;

procedure TdxCustomCssParser.WaitPropertiesValue(ACh: Char);
begin
{$IFDEF DELPHIXE4}
  if CheckGeneralEvent(ACh) or ACh.IsWhiteSpace then
{$ELSE}
  if CheckGeneralEvent(ACh) or TCharacter.IsWhiteSpace(ACh) then
{$ENDIF}
    Exit;
  case ACh of
    #$27:
      SetCssParserState(TdxCssParserState.ReadPropertiesValueInApostrophe);
    '"':
      SetCssParserState(TdxCssParserState.ReadPropertiesValueInQuotes);
    else
    begin
      FPropertiesValue.Append(ACh);

      SetCssParserState(TdxCssParserState.ReadPropertiesValue);
    end;
  end;
end;

procedure TdxCustomCssParser.ReadPropertiesValue(ACh: Char);
begin
  if CheckGeneralEvent(ACh) then
    Exit;
  case ACh of
    '!':
      begin
        SetCssParserState(TdxCssParserState.WaitPropertiesKeyword);
      end;
    '(':
      begin
        FPropertiesValue.Append(ACh);
        SetCssParserState(TdxCssParserState.ReadPropertiesValueInParentheses);
      end;
    ',':
      begin
        FPropertiesValues.Add(FPropertiesValue.ToString);
        FPropertiesValue.Length := 0;
        SetCssParserState(TdxCssParserState.WaitPropertiesValue);
      end;
    else
      FPropertiesValue.Append(ACh);
  end;
end;

procedure TdxCustomCssParser.ReadPropertiesValueInQuotes(ACh: Char);
begin
  if ACh = '"' then
    SetCssParserState(TdxCssParserState.ReadPropertiesValue)
  else
    FPropertiesValue.Append(ACh);
end;

procedure TdxCustomCssParser.ReadPropertiesValueInApostrophe(ACh: Char);
begin
  if ACh = #$27 then
    SetCssParserState(TdxCssParserState.ReadPropertiesValue)
  else
    FPropertiesValue.Append(ACh);
end;

procedure TdxCustomCssParser.ReadPropertiesValueInParentheses(ACh: Char);
begin
  if ACh = ')' then
    SetCssParserState(TdxCssParserState.ReadPropertiesValue);
  FPropertiesValue.Append(ACh);
end;

procedure TdxCustomCssParser.WaitPropertiesKeyword(ACh: Char);
begin
{$IFDEF DELPHIXE4}
  if CheckGeneralEvent(ACh) or ACh.IsWhiteSpace then
{$ELSE}
  if CheckGeneralEvent(ACh) or TCharacter.IsWhiteSpace(ACh) then
{$ENDIF}
    Exit;
  FPropertiesKeyword.Clear;
  SetCssParserState(TdxCssParserState.ReadPropertiesKeyword);
  ReadPropertiesKeyword(ACh);
end;

procedure TdxCustomCssParser.ReadPropertiesKeyword(ACh: Char);
begin
  if CheckGeneralEvent(ACh) then
    Exit;
  FPropertiesKeyword.Append(ACh);
end;

procedure TdxCustomCssParser.WaitColonSymbol(ACh: Char);
begin
  if ACh = ':' then
  begin
    SetCssParserState(TdxCssParserState.WaitPropertiesValue);
    Exit;
  end;
{$IFDEF DELPHIXE4}
  if CheckGeneralEvent(ACh) or ACh.IsWhiteSpace then
{$ELSE}
  if CheckGeneralEvent(ACh) or TCharacter.IsWhiteSpace(ACh) then
{$ENDIF}
    Exit;
end;

function TdxCustomCssParser.CheckGeneralEvent(ACh: Char): Boolean;
begin
  case ACh of
    '/':
      SetCssParserState(TdxCssParserState.WaitStartComment);
    '}':
      EndCssElement;
    '{':
      SetCssParserState(TdxCssParserState.SkipNestedProperties);
    ';':
      begin
        EndProperty;
        SetCssParserState(TdxCssParserState.ReadPropertiesName);
      end;
    else
      Exit(False);
  end;
  Result := True;
end;

procedure TdxCustomCssParser.SkipNestedProperties(ACh: Char);
begin
  if ACh = '}' then
  begin
    if FBracketCount = 0 then
    begin
      ClearProperties;
      SetCssParserState(TdxCssParserState.ReadPropertiesName);
    end
    else
      Dec(FBracketCount);
  end;
  if ACh = '{' then
    Inc(FBracketCount);
end;

procedure TdxCustomCssParser.EndProperty;
begin
  if FPropertiesValue.Length > 0 then
    FPropertiesValues.Add(Trim(FPropertiesValue.ToString));

  BeginUpdate;
  if FPropertiesValues.Count > 0 then
    FindKeywordInTable;
  EndUpdate;
  ClearProperties;
end;

procedure TdxCustomCssParser.EndCssElement;
begin
  EndProperty;
  AddCssElements;
  EndUpdate;
  SetCssParserState(TdxCssParserState.ReadSelector);
end;

procedure TdxCustomCssParser.ClearProperties;
begin

  FProperties.Length := 0;
  FPropertiesValue.Length := 0;

  FPropertiesValues.Clear;
  FPropertiesKeyword.Clear;
end;

procedure TdxCustomCssParser.WaitStartComment(ACh: Char);
var
  AParentState1: TdxCssParserState;
begin
  if ACh = '*' then
    FState := TdxCssParserState.ReadComment
  else
  begin
    AParentState1 := FParentState;
    SetCssParserState(FParentState);
    if AParentState1 = TdxCssParserState.ReadPropertiesValue then
    begin
      FPropertiesValue.Append('/');
      ReadPropertiesValue(ACh);
    end;
  end;
end;

procedure TdxCustomCssParser.WaitEndComment(ACh: Char);
begin
  if ACh = '/' then
    SetCssParserState(FParentState)
  else
    FState := TdxCssParserState.ReadComment;
end;

procedure TdxCustomCssParser.ReadComment(ACh: Char);
begin
  if ACh = '*' then
    FState := TdxCssParserState.WaitEndComment;
end;

procedure TdxCustomCssParser.FindKeywordInTable;
var
  ATranslator: TdxCssPropertiesTranslateHandler;
  APropertiesName: string;
begin
  ATranslator := nil;
  APropertiesName := UpperCase(FProperties.ToString);
  if APropertiesName <> '' then
    CssKeywordTable.TryGetValue(APropertiesName, ATranslator);
  if Assigned(ATranslator) then
    ATranslator(FCssProperties, FPropertiesValues.ToArray);
end;

procedure TdxCustomCssParser.AddCssElements;
var
  ASelector: TdxSelector;
  I: Integer;
begin
  for I := 0 to FSelectors.Count - 1 do
  begin
    ASelector := FSelectors[I];
    if ASelector.Elements.Count <> 0 then
      AddCssElement(ASelector);
  end;
  FSelectors.Clear;
end;

procedure TdxCustomCssParser.AddCssElement(ASelector: TdxSelector);
var
  ACssElement: TdxCssElement;
begin
  if FCssProperties.IsDefaultProperties then
  begin
    ASelector.Free;
    Exit;
  end;
  ACssElement := TdxCssElement.Create(FDocumentModel.MainPieceTable, FCssElementCollection.Count);
  ACssElement.Selector := ASelector;
  ACssElement.Properties.CopyFrom(FCssProperties);
  FCssElementCollection.Add(ACssElement);
end;

end.
