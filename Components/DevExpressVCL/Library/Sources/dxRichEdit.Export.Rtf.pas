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

unit dxRichEdit.Export.Rtf;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, Classes, SysUtils, Graphics, Windows, Generics.Defaults, Generics.Collections,
  cxGeometry, dxCoreGraphics,
  dxProtectionUtils,
  dxRichEdit.NativeApi,
  dxRichEdit.Options,
  dxRichEdit.Utils.Types,
  dxCharacters,
  dxRichEdit.Utils.ChunkedStringBuilder,
  dxRichEdit.Utils.WidthsContentInfo,
  dxRichEdit.Utils.OfficeImage,
  dxRichEdit.Platform.Font,
  dxRichEdit.Export.Rtf.Keywords,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Styles.Core,
  dxRichEdit.DocumentModel.Styles,
  dxRichEdit.DocumentModel.TextRange,
  dxRichEdit.DocumentModel.InlineObjectRange,
  dxRichEdit.DocumentModel.FloatingObjectRange,
  dxRichEdit.DocumentModel.NotesRange,
  dxRichEdit.DocumentModel.Exporter,
  dxRichEdit.DocumentModel.CharacterFormatting,
  dxRichEdit.DocumentModel.ParagraphFormatting,
  dxRichEdit.DocumentModel.UnitConverter,
  dxRichEdit.DocumentModel.Section,
  dxRichEdit.DocumentModel.SectionFormatting,
  dxRichEdit.DocumentModel.Tables,
  dxRichEdit.DocumentModel.Intervals.Core,
  dxRichEdit.DocumentModel.Intervals,
  dxRichEdit.DocumentModel.Bookmarks,
  dxRichEdit.DocumentModel.TabFormatting,
  dxRichEdit.DocumentModel.FrameFormatting,
  dxRichEdit.DocumentModel.Numbering,
  dxRichEdit.DocumentModel.NumberingFormatting,
  dxRichEdit.DocumentModel.TableStyles,
  dxRichEdit.DocumentModel.TableCalculator,
  dxRichEdit.DocumentModel.UnitToLayoutUnitConverter,
  dxRichEdit.DocumentModel.TableFormatting,
  dxRichEdit.DocumentModel.FieldRange,
  dxRichEdit.DocumentModel.Borders,
  dxRichEdit.DocumentModel.Notes,
  dxRichEdit.DocumentModel.DocumentProperties,
  dxRichEdit.DocumentModel.FloatingObjectFormatting,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.ShapeFormatting,
  dxGenerics,
  dxRichEdit.Export.Core,
  dxRichEdit.Export.Formats;

type
  TdxRtfBuilder = class;
  TdxRtfContentExporter = class;

  IdxRtfExportHelper = interface
  ['{BB5A4345-2772-4F4C-8791-CA1DBCD5A4EA}']
    function GetDefaultCharacterProperties: string;
    function GetDefaultFontIndex: Integer;
    function GetDefaultParagraphProperties: string;
    function GetFontCharsetTable: TdxIntegersDictionary;
    function GetListOverrideCollectionIndex: TdxIntegersDictionary;

    function BlendColor(AColor: TdxAlphaColor): TdxAlphaColor;
    function GetCharacterStylesCollectionIndex: TdxNamedOrdinalDictionary<Integer>;
    function GetColorCollection: TdxAlphaColorList;
    function GetColorIndex(AColor: TdxAlphaColor): Integer;
    function GetFontNameIndex(const AName: string): Integer;
    function GetFontNamesCollection: TdxStringList;
    function GetListCollection: TDictionary<Integer, string>;
    function GetListOverrideCollection: TdxStringList;
    function GetParagraphStylesCollectionIndex: TdxNamedOrdinalDictionary<Integer>;
    function GetStylesCollection: TdxStringList;
    function GetSupportStyle: Boolean;
    function GetTableStylesCollectionIndex: TdxNamedOrdinalDictionary<Integer>;
    function GetUserCollection: TdxStringList;

    procedure SetDefaultCharacterProperties(const Value: string);
    procedure SetDefaultParagraphProperties(const Value: string);

    property CharacterStylesCollectionIndex: TdxNamedOrdinalDictionary<Integer> read GetCharacterStylesCollectionIndex;
    property ColorCollection: TdxAlphaColorList read GetColorCollection;
    property DefaultCharacterProperties: string read GetDefaultCharacterProperties write SetDefaultCharacterProperties;
    property DefaultFontIndex: Integer read GetDefaultFontIndex;
    property DefaultParagraphProperties: string read GetDefaultParagraphProperties write SetDefaultParagraphProperties;
    property FontCharsetTable: TdxIntegersDictionary read GetFontCharsetTable;
    property FontNamesCollection: TdxStringList read GetFontNamesCollection;
    property ListCollection: TDictionary<Integer, string> read GetListCollection;
    property ListOverrideCollection: TdxStringList read GetListOverrideCollection;
    property ListOverrideCollectionIndex: TdxIntegersDictionary read GetListOverrideCollectionIndex;
    property ParagraphStylesCollectionIndex: TdxNamedOrdinalDictionary<Integer> read GetParagraphStylesCollectionIndex;
    property StylesCollection: TdxStringList read GetStylesCollection;
    property SupportStyle: Boolean read GetSupportStyle;
    property TableStylesCollectionIndex: TdxNamedOrdinalDictionary<Integer> read GetTableStylesCollectionIndex;
    property UserCollection: TdxStringList read GetUserCollection;
  end;

  { TdxRtfExportHelper }

  TdxRtfExportHelper = class(TInterfacedObject, IdxRtfExportHelper)
  private
    FDefaultFontName: string;
    FDefaultFontIndex: Integer;
    FFontNamesCollection: TdxStringList;
    FListCollection: TDictionary<Integer, string>;
    FListOverrideCollectionIndex: TdxIntegersDictionary;
    FListOverrideCollection: TdxStringList;
    FColorCollection: TdxAlphaColorList;
    FParagraphStylesCollectionIndex: TdxNamedOrdinalDictionary<Integer>;
    FCharacterStylesCollectionIndex: TdxNamedOrdinalDictionary<Integer>;
    FTableStylesCollectionIndex: TdxNamedOrdinalDictionary<Integer>;
    FFontCharsetTable: TdxIntegersDictionary;
    FStylesCollection: TdxStringList;
    FUserCollection: TdxStringList;
    FDefaultCharacterProperties: string;
    FDefaultParagraphProperties: string;
    function GetListOverrideCollectionIndex: TdxIntegersDictionary;
  protected
    //IdxRtfExportHelper
    function BlendColor(AColor: TdxAlphaColor): TdxAlphaColor;
    function GetCharacterStylesCollectionIndex: TdxNamedOrdinalDictionary<Integer>;
    function GetColorCollection: TdxAlphaColorList;
    function GetColorIndex(AColor: TdxAlphaColor): Integer;
    function GetDefaultCharacterProperties: string;
    function GetDefaultFontIndex: Integer;
    function GetDefaultParagraphProperties: string;
    function GetFontCharsetTable: TdxIntegersDictionary;
    function GetFontNamesCollection: TdxStringList;
    function GetFontNameIndex(const AName: string): Integer;
    function GetListCollection: TDictionary<Integer, string>;
    function GetListOverrideCollection: TdxStringList;
    function GetParagraphStylesCollectionIndex: TdxNamedOrdinalDictionary<Integer>;
    function GetStylesCollection: TdxStringList;
    function GetSupportStyle: Boolean;
    function GetTableStylesCollectionIndex: TdxNamedOrdinalDictionary<Integer>;
    function GetUserCollection: TdxStringList;
    procedure SetDefaultCharacterProperties(const Value: string);
    procedure SetDefaultParagraphProperties(const Value: string);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function GetUserIndex(ARangePermission: TdxRangePermission): Integer;

    property ColorCollection: TdxAlphaColorList read FColorCollection;
    property FontCharsetTable: TdxIntegersDictionary read GetFontCharsetTable;
    property FontNamesCollection: TdxStringList read FFontNamesCollection;
    property ListCollection: TDictionary<Integer, string> read FListCollection;
    property ListOverrideCollectionIndex: TdxIntegersDictionary read GetListOverrideCollectionIndex;
    property ListOverrideCollection: TdxStringList read GetListOverrideCollection;
    property ParagraphStylesCollectionIndex: TdxNamedOrdinalDictionary<Integer> read GetParagraphStylesCollectionIndex;
    property CharacterStylesCollectionIndex: TdxNamedOrdinalDictionary<Integer> read GetCharacterStylesCollectionIndex;
    property TableStylesCollectionIndex: TdxNamedOrdinalDictionary<Integer> read GetTableStylesCollectionIndex;
    property StylesCollection: TdxStringList read GetStylesCollection;
    property UserCollection: TdxStringList read FUserCollection;
    property SupportStyle: Boolean read GetSupportStyle;
    property DefaultCharacterProperties: string read GetDefaultCharacterProperties write SetDefaultCharacterProperties;
    property DefaultParagraphProperties: string read GetDefaultParagraphProperties write SetDefaultParagraphProperties;
    property DefaultFontIndex: Integer read FDefaultFontIndex;
  end;

  { TdxRtfPropertiesExporter }

  TdxRtfPropertiesExporter = class abstract
  private
    FDocumentModel: TdxDocumentModel;
    FRtfBuilder: TdxRtfBuilder;
    FRtfExportHelper: IdxRtfExportHelper;
    function GetUnitConverter: TdxDocumentModelUnitConverter; inline;
    procedure WriteBorderProperties(ABorder: TdxBorderInfo);
    procedure WriteBorderStyle(AValue: TdxBorderLineStyle);
    procedure WriteBorderWidth(AValue, ADefaultValue: Integer);
  protected
    function ShouldExportCellMargin(AMarginUnit: TdxWidthUnitInfo): Boolean;
    procedure WriteBoolCommand(const ACommand: string; AValue: Boolean);
    procedure WriteWidthUnit(AUnit: TdxWidthUnitInfo; const ATypeKeyword, AValueKeyword: string; AWriteValueAnyway: Boolean = False);
    procedure WriteWidthUnitInTwips(AUnit: TdxWidthUnitInfo; const ATypeKeyword, AValueKeyword: string);
  public
    constructor Create(ADocumentModel: TdxDocumentModel; const ARtfExportHelper: IdxRtfExportHelper;
      ARtfBuilder: TdxRtfBuilder);

    property RtfExportHelper: IdxRtfExportHelper read FRtfExportHelper;
    property RtfBuilder: TdxRtfBuilder read FRtfBuilder;
    property DocumentModel: TdxDocumentModel read FDocumentModel;
    property UnitConverter: TdxDocumentModelUnitConverter read GetUnitConverter;
  end;

  TdxRtfParagraphPropertiesExporter = class(TdxRtfPropertiesExporter)
  public const
    DefaultParagraphFirstLineIndent = 0;
    DefaultParagraphLeftIndent = 0;
    DefaultParagraphRightIndent = 0;
    DefaultSuppressHyphenation = False;
    DefaultPageBreakBefore = False;
    DefaultBeforeAutoSpacing = False;
    DefaultAfterAutoSpacing = False;
    DefaultKeepWithNext = False;
    DefaultKeepLinesTogether = False;
    DefaultWidowOrphanControl = True;
    DoubleIntervalRtfLineSpacingValue = 480;
    SesquialteralIntervalRtfLineSpacingValue = 360;
    SingleIntervalRtfLineSpacingValue = 240;

    AtLeastLineSpacingMultiple = 0;
    ExactlyLineSpacingMultiple = 0;
    MultipleLineSpacing = 1;

    DefaultParagraphSpacingBefore = 0;
    DefaultParagraphSpacingAfter = 0;
  private
    function CalcRtfFirstLineIndent(AFirstLineIndentType: TdxParagraphFirstLineIndent;  AFirstLineIndent: Integer): Integer;
    function CalcRtfLeftIndent(AFirstLineIndentType: TdxParagraphFirstLineIndent; AFirstLineIndent, ALeftIndent: Integer): Integer;
    function CalcRtfRightIndent(ARightIndent: Integer): Integer;
  protected
    procedure ExportParagraphNumberingProperties(AParagraph: TdxParagraph);

    procedure WriteParagraphAlignment(Alignment: TdxParagraphAlignment);
    procedure WriteParagraphIndents(AMergedParagraphProperties: TdxMergedParagraphProperties);
    procedure WriteParagraphSuppressHyphenation(AParagraphSuppressHyphenation: Boolean);
    procedure WriteParagraphSuppressLineNumbers(AParagraphSuppressLineNumbers: Boolean);
    procedure WriteParagraphContextualSpacing(Value: Boolean);
    procedure WriteParagraphPageBreakBefore(Value: Boolean);
    procedure WriteParagraphBeforeAutoSpacing(Value: Boolean);
    procedure WriteParagraphAfterAutoSpacing(Value: Boolean);
    procedure WriteParagraphKeepWithNext(Value: Boolean);
    procedure WriteParagraphKeepLinesTogether(Value: Boolean);
    procedure WriteParagraphWidowOrphanControl(Value: Boolean);
    procedure WriteParagraphOutlineLevel(AOutlineLevel: Integer);
    procedure WriteParagraphBackColor(Value: TdxAlphaColor);
    procedure WriteParagraphLineSpacing(AParagraphLineSpacingType: TdxParagraphLineSpacing; AParagraphLineSpacing: Single);
    procedure WriteParagraphSpacingBefore(ASpacingBefore: Integer);
    procedure WriteParagraphSpacingAfter(ASpacingAfter: Integer);
    procedure WriteRtfLineSpacing(ARtfLineSpacingValue, ARtfLineSpacingMultiple: Integer);
    procedure WriteParagraphListIndex(AIndex: TdxNumberingListIndex);
    procedure WriteParagraphStyle(AParagraphStyle: TdxParagraphStyle);
    procedure WriteParagraphGroupPropertiesId(AParagraph: TdxParagraph);
    procedure WriteTabs(ATabFormattingInfo: TdxTabFormattingInfo);
    procedure WriteTabInfo(const ATabInfo: TdxTabInfo);
    procedure WriteTabKind(AlignmentType: TdxTabAlignmentType);
    procedure WriteTabLeader(ALeaderType: TdxTabLeaderType);
    procedure WriteParagraphTableProperties(AParagraph: TdxParagraph; ANestingLevel: Integer);
    procedure WriteTabPosition(APosition: Integer);

    procedure WriteParagraphBorder(ATopBorder: TdxBorderInfo; const ACommand: string);
    procedure WriteFrameProperties(AProperties: TdxFrameProperties);
    procedure WriteParagraphVerticalPositionType(APositionType: TdxParagraphFrameVerticalPositionType);
    procedure WriteParagraphHorizontalPositionType(APositionType: TdxParagraphFrameHorizontalPositionType);
    procedure WriteParagraphWrapType(AWrapType: TdxParagraphFrameTextWrapType); virtual;
  public
    procedure ExportParagraphPropertiesCore(AProperties: TdxMergedParagraphProperties;
      ACheckDefaultAlignment: Boolean = False);
    procedure ExportParagraphProperties(AParagraph: TdxParagraph; ATableNestingLevel: Integer);
  end;

  { TdxRtfCharacterPropertiesExporter }

  TdxRtfCharacterPropertiesExporter = class(TdxRtfPropertiesExporter)
  public const
    DefaultRtfFontSize = 24;
{$REGION 'DefaultUnderlineTypes'}
    DefaultUnderlineTypes: array[TdxUnderlineType] of string = (
      '',
      TdxRtfExportSR.FontUnderline,
      TdxRtfExportSR.FontUnderlineDotted,
      TdxRtfExportSR.FontUnderlineDashed,
      TdxRtfExportSR.FontUnderlineDashDotted,
      TdxRtfExportSR.FontUnderlineDashDotDotted,
      TdxRtfExportSR.FontUnderlineDouble,
      TdxRtfExportSR.FontUnderlineHeavyWave,
      TdxRtfExportSR.FontUnderlineLongDashed,
      TdxRtfExportSR.FontUnderlineThickSingle,
      TdxRtfExportSR.FontUnderlineThickDotted,
      TdxRtfExportSR.FontUnderlineThickDashed,
      TdxRtfExportSR.FontUnderlineThickDashDotted,
      TdxRtfExportSR.FontUnderlineThickDashDotDotted,
      TdxRtfExportSR.FontUnderlineThickLongDashed,
      TdxRtfExportSR.FontUnderlineDoubleWave,
      TdxRtfExportSR.FontUnderlineWave,
      '');
{$ENDREGION}
  private
    FOptions: TdxRtfDocumentExporterOptions;
  protected
    procedure ExportCharacterProperties(ACharacterProperties: TdxMergedCharacterProperties;
      ACheckDefaultColor: Boolean = False; ACheckUseFontSize: Boolean = False; ACheckUseFontName: Boolean = False);
    procedure ExportCharacterPropertiesCore(ACharacterProperties: TdxMergedCharacterProperties;
      ACheckUseFontSize, ACheckUseFontName: Boolean);
    procedure ExportParagraphCharacterProperties(ACharacterProperties: TdxMergedCharacterProperties);

    procedure RegisterFontCharset(AInfo: TdxCharacterFormattingInfo; AFontNameIndex: Integer);
    procedure WriteBackgroundColor(ABackColor: TdxAlphaColor);
    procedure WriteFontSize(ARtfFontSize: Integer);
    function WriteFontName(const AFontName: string): Integer;
    procedure WriteFontUnderline(AUnderlineType: TdxUnderlineType);
    procedure WriteForegroundColor(AForeColor: TdxAlphaColor; ACheckDefaultColor: Boolean = False);
    procedure WriteForegroundColorCore(AForeColor: TdxAlphaColor; ACheckDefaultColor: Boolean = False);
    procedure WriteUnderlineColor(AUnderlineColor: TdxAlphaColor);
  public
    constructor Create(ADocumentModel: TdxDocumentModel; const ARtfExportHelper: IdxRtfExportHelper;
      ARtfBuilder: TdxRtfBuilder; const AOptions: TdxRtfDocumentExporterOptions);
  end;

  { TdxRtfTablePropertiesExporter }

  TdxRtfTablePropertiesExporter = class(TdxRtfPropertiesExporter)
  protected
    function GetTableTopBorder: string; virtual;
    function GetTableLeftBorder: string; virtual;
    function GetTableBottomBorder: string; virtual;
    function GetTableRightBorder: string; virtual;
    function GetTableHorizontalBorder: string; virtual;
    function GetTableVerticalBorder: string; virtual;
    function GetTableCellMarginsLeftType: string; virtual;
    function GetTableCellMarginsLeft: string; virtual;
    function GetTableCellMarginsBottomType: string; virtual;
    function GetTableCellMarginsBottom: string; virtual;
    function GetTableCellMarginsRightType: string; virtual;
    function GetTableCellMarginsRight: string; virtual;
    function GetTableCellMarginsTopType: string; virtual;
    function GetTableCellMarginsTop: string; virtual;

    property TableTopBorder: string read GetTableTopBorder;
    property TableLeftBorder: string read GetTableLeftBorder;
    property TableBottomBorder: string read GetTableBottomBorder;
    property TableRightBorder: string read GetTableRightBorder;
    property TableHorizontalBorder: string read GetTableHorizontalBorder;
    property TableVerticalBorder: string read GetTableVerticalBorder;
    property TableCellMarginsLeftType: string read GetTableCellMarginsLeftType;
    property TableCellMarginsLeft: string read GetTableCellMarginsLeft;
    property TableCellMarginsBottomType: string read GetTableCellMarginsBottomType;
    property TableCellMarginsBottom: string read GetTableCellMarginsBottom;
    property TableCellMarginsRightType: string read GetTableCellMarginsRightType;
    property TableCellMarginsRight: string read GetTableCellMarginsRight;
    property TableCellMarginsTopType: string read GetTableCellMarginsTopType;
    property TableCellMarginsTop: string read GetTableCellMarginsTop;
  public
    procedure WriteBandSizes(AInfo: TdxTableGeneralSettingsInfo;
      AExportRowBand: Boolean; AExportColBand: Boolean);
    procedure WriteRowLeft(ALeft: Integer);
    procedure WriteTableBorders(ATopBorder, ALeftBorder, ABottomBorder, ARightBorder, AInnerHorizontalBorder, AInnerVerticalBorder: TdxBorderInfo);
    procedure WriteTableHorizontalAnchor(AValue: TdxHorizontalAnchorTypes);
    procedure WriteTableVerticalAnchor(AValue: TdxVerticalAnchorTypes);
    procedure WriteTableHorizontalAlign(AValue: TdxHorizontalAlignMode);
    procedure WriteTableVerticalAlign(AValue: TdxVerticalAlignMode);
    procedure WriteTableHorizontalPosition(AValue: Integer);
    procedure WriteTableVerticalPosition(AValue: Integer);
    procedure WriteTableWidth(APreferredWidth: TdxWidthUnitInfo);
    procedure WriteTableLayout(AValue: TdxTableLayoutType);
    procedure WriteTableCellMargins(ALeftMargin, ARightMargin, ABottomMargin, ATopMargin: TdxWidthUnitInfo);
    procedure WriteTableLook(AValue: TdxTableLookTypes);
    procedure WriteTableIndent(ATableIndent: TdxWidthUnitInfo);
    procedure WriteTableFloatingPosition(AFloatingPosition: TdxTableFloatingPositionInfo);
  end;

  { TdxRtfTableStyleTablePropertiesExporter }

  TdxRtfTableStyleTablePropertiesExporter = class(TdxRtfTablePropertiesExporter)
  private
    function GetTableStyleRowBandSize: string;
    function GetTableStyleColumnBandSize: string;
  protected
    function GetTableCellMarginsLeftType: string; override;
    function GetTableCellMarginsLeft: string; override;
    function GetTableCellMarginsBottomType: string; override;
    function GetTableCellMarginsBottom: string; override;
    function GetTableCellMarginsRightType: string; override;
    function GetTableCellMarginsRight: string; override;
    function GetTableCellMarginsTopType: string; override;
    function GetTableCellMarginsTop: string; override;
    procedure ExportTableProperties(AMergedTableProperties: TdxMergedTableProperties;
      AExportRowBand: Boolean; AExportColBand: Boolean);

    property TableStyleRowBandSize: string read GetTableStyleRowBandSize;
    property TableStyleColumnBandSize: string read GetTableStyleColumnBandSize;
  end;

  { TdxRtfTableRowPropertiesExporter }

  TdxRtfTableRowPropertiesExporter = class(TdxRtfPropertiesExporter)
  public
    procedure WriteLastRowMark;
    procedure WriteHalfSpaceBetweenCells(AVal: Integer);
    procedure WriteRowAlignment(AValue: TdxTableRowAlignment);
    procedure WriteRowHeader(AHeader: Boolean);
    procedure WriteRowCantSplit(ACantSplit: Boolean);
    procedure WriteRowHeight(AHeight: TdxHeightUnitInfo);
    procedure WriteWidthBefore(AWidthBefore: TdxWidthUnitInfo);
    procedure WriteWidthAfter(AWidthAfter: TdxWidthUnitInfo);
    procedure WriteRowCellSpacing(ACellSpacing: TdxWidthUnitInfo);
  end;

  { TdxRtfTableCellPropertiesExporter }

  TdxRtfTableCellPropertiesExporter = class(TdxRtfPropertiesExporter)
  protected
    function GetTableCellBackgroundColor: string; virtual;
    function GetTableCellForegroundColor: string; virtual;
    function GetTableCellShading: string; virtual;
    function GetTableCellNoWrap: string; virtual;
    function GetTableCellTextTopAlignment: string; virtual;
    function GetTableCellTextCenterAlignment: string; virtual;
    function GetTableCellTextBottomAlignment: string; virtual;
    function GetTableCellUpperLeftToLowerRightBorder: string; virtual;
    function GetTableCellUpperRightToLowerLeftBorder: string; virtual;
    function GetCellTopBorder: string; virtual;
    function GetCellLeftBorder: string; virtual;
    function GetCellBottomBorder: string; virtual;
    function GetCellRightBorder: string; virtual;

    property TableCellBackgroundColor: string read GetTableCellBackgroundColor;
    property TableCellForegroundColor: string read GetTableCellForegroundColor;
    property TableCellShading: string read GetTableCellShading;
    property TableCellNoWrap: string read GetTableCellNoWrap;
    property TableCellTextTopAlignment: string read GetTableCellTextTopAlignment;
    property TableCellTextCenterAlignment: string read GetTableCellTextCenterAlignment;
    property TableCellTextBottomAlignment: string read GetTableCellTextBottomAlignment;
    property TableCellUpperLeftToLowerRightBorder: string read GetTableCellUpperLeftToLowerRightBorder;
    property TableCellUpperRightToLowerLeftBorder: string read GetTableCellUpperRightToLowerLeftBorder;
    property CellTopBorder: string read GetCellTopBorder;
    property CellLeftBorder: string read GetCellLeftBorder;
    property CellBottomBorder: string read GetCellBottomBorder;
    property CellRightBorder: string read GetCellRightBorder;
  public
    procedure WriteCellBackgroundColor(ACell: TdxTableCell); overload;
    procedure WriteCellBackgroundColor(AColor: TdxAlphaColor; ATable: TdxTable = nil); overload;
    procedure WriteCellFitText(AFitText: Boolean);
    procedure WriteCellNoWrap(ANoWrap: Boolean);
    procedure WriteCellHideCellMark(AHideCellMark: Boolean);
    procedure WriteCellMerging(AMergingState: TdxMergingState);
    procedure WriteCellVerticalMerging(AValue: TdxMergingState; ADefaultValue: TdxMergingState);
    procedure WriteCellVerticalAlignment(AVerticalAlignment: TdxVerticalAlignment);
    procedure WriteCellTextDirection(AValue: TdxTextDirection);
    procedure WriteCellBasicBorders(ATopBorder, ALeftBorder, ARightBorder, ABottomBorder: TdxBorderInfo); virtual;
    procedure WriteCellPreferredWidth(APreferredWidth: TdxWidthUnitInfo);
    procedure WriteCellMargins(ACell: TdxTableCell); overload;
    procedure WriteCellMargins(ATopMargin, ALeftMargin, ARightMargin, ABottomMargin: TdxWidthUnitInfo); overload;
    procedure WriteCellRight(ACellRight: Integer);
    procedure WriteCellForegroundColor(ACell: TdxTableCell); overload;
    procedure WriteCellForegroundColor(AColor: TdxAlphaColor); overload;
    procedure WriteCellForegroundColor(AColor: TdxAlphaColor; ATable: TdxTable); overload;
    procedure WriteCellShading(ACell: TdxTableCell); overload;
    procedure WriteCellShading(AShading: TdxShadingPattern); overload;
  end;

  { TdxRtfTableStyleTableCellPropertiesExporter }

  TdxRtfTableStyleTableCellPropertiesExporter = class(TdxRtfTableCellPropertiesExporter)
  protected
    function GetTableCellBackgroundColor: string; override;
    function GetTableCellNoWrap: string; override;
    function GetTableCellTextTopAlignment: string; override;
    function GetTableCellTextCenterAlignment: string; override;
    function GetTableCellTextBottomAlignment: string; override;
    function GetTableCellUpperLeftToLowerRightBorder: string; override;
    function GetTableCellUpperRightToLowerLeftBorder: string; override;
    function GetCellTopBorder: string; override;
    function GetCellLeftBorder: string; override;
    function GetCellBottomBorder: string; override;
    function GetCellRightBorder: string; override;
  public
    procedure WriteCellBasicBorders(ATopBorder: TdxBorderInfo; ALeftBorder: TdxBorderInfo;
      ARightBorder: TdxBorderInfo; ABottomBorder: TdxBorderInfo); override;
  end;

  { TdxRtfStyleExporter }

  TdxRtfStyleExporter = class
  strict private
    FRtfExportHelper: IdxRtfExportHelper;
    FRtfBuilder: TdxRtfBuilder;
    FCharacterPropertiesExporter: TdxRtfCharacterPropertiesExporter;
    FParagraphPropertiesExporter: TdxRtfParagraphPropertiesExporter;
    FTablePropertiesExporter: TdxRtfTableStyleTablePropertiesExporter;
    FTableRowPropertiesExporter: TdxRtfTableRowPropertiesExporter;
    FTableCellPropertiesExporter: TdxRtfTableStyleTableCellPropertiesExporter;
    FDocumentModel: TdxDocumentModel;
    function ObtainStyleIndex(const AStyle: IdxStyle; ACollection: TdxNamedOrdinalDictionary<Integer>): Integer;
    function ObtainCharacterStyleIndex(AStyle: TdxCharacterStyle): Integer;
    function ObtainParagraphStyleIndex(AStyle: TdxParagraphStyle): Integer;
  protected
    procedure ExportCharacterProperties(ACharacterProperties: TdxMergedCharacterProperties);
    procedure ExportCharacterStyle(AStyle: TdxCharacterStyle);
    procedure ExportParagraphStyle(AStyle: TdxParagraphStyle; I: Integer);
    procedure ExportParagraphStyles(AParagraphStyles: TdxParagraphStyleCollection);
    procedure ExportParagraphProperties(AParagraphProperties: TdxParagraphProperties;
      AMergedParagraphProperties: TdxMergedParagraphProperties);
    procedure ExportCharacterStyles(ACharacterStyles: TdxCharacterStyleCollection);
    procedure ExportTableCellProperties(ATableCellProperties: TdxTableCellProperties;
      AMergedTableCellProperties: TdxMergedTableCellProperties);
    procedure ExportTableConditionalStyle(AConditionalStyle: TdxTableConditionalStyle;
      const AStyleName: string; AStyleIndex: Integer);
    procedure ExportTableProperties(ATableProperties: TdxTableProperties;
      AMergedTableProperties: TdxMergedTableProperties; AExportRowProperties, AExportColProperties: Boolean);
    procedure ExportTableRowProperties(ATableRowProperties: TdxTableRowProperties;
      AMergedTableRowProperties: TdxMergedTableRowProperties);
    procedure ExportTableStyle(AStyle: TdxTableStyle);
    procedure ExportTableStyles(ATableStyles: TdxTableStyleCollection);
    function GetListId(AIndex: TdxNumberingListIndex): Integer;
    function GetNextFreeStyleIndex: Integer;
    function ObtainTableStyleIndex(AStyle: TdxTableStyle): Integer;
    procedure WriteConditionalStyleType(AConditionType: TdxConditionalTableStyleFormattingType);
    procedure WriteStyleName(const AName: string);
  public
    constructor Create(ADocumentModel: TdxDocumentModel; ARtfBuilder: TdxRtfBuilder;
      const ARtfExportHelper: IdxRtfExportHelper; AOptions: TdxRtfDocumentExporterOptions);
    destructor Destroy; override;

    procedure ExportStyleSheet(AParagraphStyles: TdxParagraphStyleCollection;
      ACharacterStyles: TdxCharacterStyleCollection; ATableStyles: TdxTableStyleCollection);

    property RtfBuilder: TdxRtfBuilder read FRtfBuilder;
    property RtfExportHelper: IdxRtfExportHelper read FRtfExportHelper;
    property CharacterExporter: TdxRtfCharacterPropertiesExporter read FCharacterPropertiesExporter;
    property DocumentModel: TdxDocumentModel read FDocumentModel;
  end;

  { TdxRtfNumberingListExporter }

  TdxRtfNumberingListExporter = class
  strict private
    FRtfExporter: TdxRtfContentExporter;
    FRtfBuilder: TdxRtfBuilder;
    FText: string;
    FNumber: string;
    FCharacterPropertiesExporter: TdxRtfCharacterPropertiesExporter;
    FParagraphPropertiesExporter: TdxRtfParagraphPropertiesExporter;
    FTextLength: Integer;
  private
    function GetAbstractNumberingLists(ANumberingLists: TdxNumberingListCollection; AStartIndex: TdxNumberingListIndex;
      ACount: Integer): TdxAbstractNumberingListCollection;
  protected
    procedure ExportAbstractNumberingList(AList: TdxAbstractNumberingList); virtual;
    procedure ExportListLevels(AListLevelCollection: TdxListLevelCollection); virtual;
    procedure ExportListLevel(const AListLevel: IdxListLevel);
    function GetNumberingListFormat(ANumberingFormat: TdxNumberingFormat): Integer;
    function GetListLevelSeparator(ASeparator: Char): Integer;
    procedure ExportListLevelTextAndNumber(const ADisplayFormatString: string; ALevelTemplateId: Integer); virtual;
    procedure GetTextAndNumber(const ADisplayFormatString: string);
    function AddLevelNumber(const ADisplayFormatString: string; I: Integer): Integer;
    function AddLevelNumberCore(const ADisplayFormatString: string; I: Integer): Integer;
    class function DoubleBrackets(const ADisplayFormatString: string; I: Integer): Boolean;
    function AddChar(ACh: Char; I: Integer): Integer;
    function AddEscapedChar(ACh: Char; I: Integer): Integer;
    procedure ExportListLevelCharacterAndParagraphProperties(const AListLevel: IdxListLevel);
    procedure ExportAbstractListLevelParagraphStyleIndex(AAbstractListLevel: TdxListLevel);
    procedure ExportListOverride(ANumberingList: TdxNumberingList); virtual;
    procedure WriteListOverrideId(ANumberingList: TdxNumberingList); virtual;
    function GetListOverrideCount(ANumberingList: TdxNumberingList): Integer;
    function IsOverrideLevel(AListLevel: TdxAbstractListLevel): Boolean;
    procedure ExportListOverrideLevels(ANumberingList: TdxNumberingList);
    procedure ExportListOverrideLevel(ALevel: TdxAbstractListLevel);

    property RtfBuilder: TdxRtfBuilder read FRtfBuilder;
    property RtfExporter: TdxRtfContentExporter read FRtfExporter;
  public
    constructor Create(ARtfExporter: TdxRtfContentExporter);
    destructor Destroy; override;

    procedure Export(ANumberingLists: TdxNumberingListCollection; AStartIndex, ACount: Integer);
    procedure ExportNumberingListTable(AAbstractNumberingLists: TdxAbstractNumberingListCollection); virtual;
    procedure ExportAbstractNumberingLists(AAbstractNumberingLists: TdxAbstractNumberingListCollection); virtual;
    procedure ExportListOverrideTable(ANumberingLists: TdxNumberingListCollection;
      AStartIndex: TdxNumberingListIndex; ACount: Integer); overload; virtual;
    procedure ExportListOverrideTable(ANumberingLists: TdxNumberingListCollection); overload; virtual;

    property Text: string read FText write FText;
    property Number: string read FNumber write FNumber;
    property TextLength: Integer read FTextLength write FTextLength;
  end;

  { TdxRtfContentExporter }

  TdxRtfContentExporter = class(TdxDocumentModelExporter)
  public const
{$REGION 'VerticalAlignmentTypes'}
    VerticalAlignmentTypes: array[TdxVerticalAlignment] of string = (
      TdxRtfExportSR.VerticalAlignmentTop,
      TdxRtfExportSR.VerticalAlignmentJustify,
      TdxRtfExportSR.VerticalAlignmentCenter,
      TdxRtfExportSR.VerticalAlignmentBottom);
{$ENDREGION}
{$REGION 'SectionStartTypes'}
    SectionStartTypes: array [TdxSectionStartType] of string = (
      TdxRtfExportSR.SectionBreakTypeNextPage,
      TdxRtfExportSR.SectionBreakTypeOddPage,
      TdxRtfExportSR.SectionBreakTypeEvenPage,
      TdxRtfExportSR.SectionBreakTypeContinuous,
      TdxRtfExportSR.SectionBreakTypeColumn
    );
{$ENDREGION}
{$REGION 'ConditionalStylesTypes'}
{$IFDEF DELPHIXE2}
    ConditionalStylesTypes: array[TdxConditionalTableStyleFormattingType.BottomLeftCell..TdxConditionalTableStyleFormattingType.FirstRow] of string = (
{$ELSE}
    ConditionalStylesTypes: array[Ord(TdxConditionalTableStyleFormattingType.BottomLeftCell)..Ord(TdxConditionalTableStyleFormattingType.FirstRow)] of string = (
{$ENDIF}
      TdxRtfExportSR.TableConditionalStyleBottomLeftCell,
      TdxRtfExportSR.TableConditionalStyleBottomRightCell,
      TdxRtfExportSR.TableConditionalStyleTopLeftCell,
      TdxRtfExportSR.TableConditionalStyleTopRightCell,
      TdxRtfExportSR.TableConditionalStyleEvenRowBanding,
      TdxRtfExportSR.TableConditionalStyleOddRowBanding,
      TdxRtfExportSR.TableConditionalStyleEvenColumnBanding,
      TdxRtfExportSR.TableConditionalStyleOddColumnBanding,
      TdxRtfExportSR.TableConditionalStyleLastColumn,
      TdxRtfExportSR.TableConditionalStyleFirstColumn,
      TdxRtfExportSR.TableConditionalStyleLastRow,
      TdxRtfExportSR.TableConditionalStyleFirstRow
    );
{$ENDREGION}
{$REGION 'BorderLineStyles'}
{$IFDEF DELPHIXE2}
    BorderLineStyles: array[TdxBorderLineStyle.&Nil..TdxBorderLineStyle.Inset] of string = (
{$ELSE}
    BorderLineStyles: array[Ord(TdxBorderLineStyle.&Nil)..Ord(TdxBorderLineStyle.Inset)] of string = (
{$ENDIF}
      TdxRtfExportSR.NoBorder,
      TdxRtfExportSR.BorderNone,
      TdxRtfExportSR.BorderSingle,
      '',
      TdxRtfExportSR.BorderDouble,
      TdxRtfExportSR.BorderDotted,
      TdxRtfExportSR.BorderDashed,
      TdxRtfExportSR.BorderDotDashed,
      TdxRtfExportSR.BorderDotDotDashed,
      TdxRtfExportSR.BorderTriple,
      TdxRtfExportSR.BorderThinThickSmall,
      TdxRtfExportSR.BorderThickThinSmall,
      TdxRtfExportSR.BorderThinThickThinSmall,
      TdxRtfExportSR.BorderThinThickMedium,
      TdxRtfExportSR.BorderThickThinMedium,
      TdxRtfExportSR.BorderThinThickThinMedium,
      TdxRtfExportSR.BorderThinThickLarge,
      TdxRtfExportSR.BorderThickThinLarge,
      TdxRtfExportSR.BorderThinThickThinLarge,
      TdxRtfExportSR.BorderWavy,
      TdxRtfExportSR.BorderDoubleWavy,
      TdxRtfExportSR.BorderDashedSmall,
      TdxRtfExportSR.BorderDashDotStroked,
      TdxRtfExportSR.BorderThreeDEmboss,
      TdxRtfExportSR.BorderThreeDEngrave,
      TdxRtfExportSR.BorderOutset,
      TdxRtfExportSR.BorderInset
    );
{$ENDREGION}
  strict private
    class var
      FChapterSeparatorTypes: TdxCharStringDictionary;
      FPageNumberingTypes: TdxEnumeratedDictionary<TdxNumberingFormat, string>;
      FSectionFootNoteNumberingTypes: TdxEnumeratedDictionary<TdxNumberingFormat, string>;
      FSectionEndNoteNumberingTypes: TdxEnumeratedDictionary<TdxNumberingFormat, string>;
      FFootNoteNumberingTypes: TdxEnumeratedDictionary<TdxNumberingFormat, string>;
      FEndNoteNumberingTypes: TdxEnumeratedDictionary<TdxNumberingFormat, string>;
      FPredefinedUserGroups: TDictionary<Integer, string>;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateChapterSeparatorTypesTable: TdxCharStringDictionary; static;
    class function CreatePageNumberingTypesTable: TdxEnumeratedDictionary<TdxNumberingFormat, string>; static;
    class function CreateSectionFootNoteNumberingTypesTable: TdxEnumeratedDictionary<TdxNumberingFormat, string>; static;
    class function CreateSectionEndNoteNumberingTypesTable: TdxEnumeratedDictionary<TdxNumberingFormat, string>; static;
    class function CreateFootNoteNumberingTypesTable: TdxEnumeratedDictionary<TdxNumberingFormat, string>; static;
    class function CreateEndNoteNumberingTypesTable: TdxEnumeratedDictionary<TdxNumberingFormat, string>; static;
    class function CreatePredefinedUserGroups: TDictionary<Integer, string>; static;
  strict private
    FLastParagraphRunNotSelected: Boolean;
    FRtfExportHelper: IdxRtfExportHelper;
    FRtfBuilder: TdxRtfBuilder;
    FParagraphPropertiesExporter: TdxRtfParagraphPropertiesExporter;
    FCharacterPropertiesExporter: TdxRtfCharacterPropertiesExporter;
    FOptions: TdxRtfDocumentExporterOptions;
    FGetMergedCharacterPropertiesCachedResult: TdxRunMergedCharacterPropertiesCachedResult;
    function CalculateFirstExportedNumberingListsIndex(ANumberingLists: TdxNumberingListCollection): Integer;
    function CalculateFirstExportedNumberingListsIndexForStyles(AParagraphStyleCollection: TdxParagraphStyleCollection): Integer;
    function CalculateFirstExportedNumberingListsIndexForNumberingListStyles(ANumberingListStyleCollection: TdxNumberingListStyleCollection): TdxNumberingListIndex;
    function GetUnitConverter: TdxDocumentModelUnitConverter;
    function IsLastParagraph(AParagraph: TdxParagraph): Boolean;
  protected
    class function ConditionalStylesTypesTryGetValue(AKey: TdxConditionalTableStyleFormattingType;
      var AResult: string): Boolean; static; inline;
    class function BorderLineStylesTryGetValue(AKey: TdxBorderLineStyle; var AResult: string): Boolean; static; inline;
    procedure ExportSection(const ASection: TdxSection); override;
    function ShouldExportHiddenText: Boolean; override;

    function CreateNumberingListExporter(AExporter: TdxRtfContentExporter): TdxRtfNumberingListExporter;

    function CreateRtfBuilder: TdxRtfBuilder; virtual;
    function CreateParagraphPropertiesExporter: TdxRtfParagraphPropertiesExporter; virtual;
    function CreateStyleExporter: TdxRtfStyleExporter;
    procedure EnsureValidFirstColor(AColor: TdxAlphaColor);
    procedure FinishParagraph(AParagraph: TdxParagraph);
    procedure PopulateUserTable;
    procedure PopulateUserList(APieceTable: TdxPieceTable; AUsers: TdxStringList); virtual;
    procedure StartNewParagraph(AParagraph: TdxParagraph; ATableNestingLevel: Integer);
    procedure StartNewInnerParagraph(AParagraph: TdxParagraph; ATableNestingLevel: Integer);
    function ShouldWriteRunCharacterStyle(ARun: TdxTextRunBase): Boolean;
    procedure StartNewSection(ASection: TdxSection);
    function SuppressExportLastParagraph(AParagraph: TdxParagraph): Boolean;
    procedure WriteAlternativeText(AParagraph: TdxParagraph);
    procedure WriteEnumValueCommand<T>(ATable: TDictionary<T, string>; const AValue: T; const ADefaultCommand: string); overload;
    procedure WriteEnumValueCommand<T>(ATable: TdxEnumeratedDictionary<T, string>; const AValue: T; const ADefaultCommand: string); overload;
    procedure WriteText(const AText: string);
    procedure WriteSectionBreakType(ABreakType: TdxSectionStartType);
    procedure WriteChapterSeparator(ASeparator: Char);
    procedure WritePageNumberingFormat(ANumberingFormat: TdxNumberingFormat);
    procedure WriteAlternativeAndPlainText(AParagraph: TdxParagraph);
    procedure WriteRunCharacterStyle(ARun: TdxTextRunBase);
    procedure WriteCharacterStyle(ACharacterStyle: TdxCharacterStyle);
    procedure WriteParagraphStyle(AParagraphStyle: TdxParagraphStyle);
    procedure WriteVerticalAlignment(AAlignment: TdxVerticalAlignment);
    procedure WriteFieldInstructionStart(ARun: TdxFieldCodeStartRun);
    procedure WriteFieldInstructionEnd;
    procedure WriteFieldResultStart;
    procedure WriteFieldResultEnd;

    procedure ExportBookmarkEnd(ABookmark: TdxBookmark); override;
    procedure ExportBookmarkStart(ABookmark: TdxBookmark); override;

    procedure ExportRangePermissionStart(ARangePermission: TdxRangePermission); override;
    procedure ExportRangePermissionEnd(ARangePermission: TdxRangePermission); override;
    function GenerateRangePermissionData(ARangePermission: TdxRangePermission): string;
    function IntToShortString(AValue: Integer): string;

    procedure ExportFloatingObjectAnchorRun(ARun: TdxFloatingObjectAnchorRun); override;
    procedure ExportShapeInstanceProperties(AFloatingObjectProperties: TdxFloatingObjectProperties; ARotation: Integer);
    procedure ExportFloatingObjectShape(AShape: TdxShape);
    procedure ExportFloatingObjectHorizontalPositionAlignment(AFloatingObjectProperties: TdxFloatingObjectProperties);
    procedure ExportFloatingObjectHorizontalPositionTypeProperty(AFloatingObjectProperties: TdxFloatingObjectProperties);
    procedure ExportFloatingObjectVerticalPositionAlignment(AFloatingObjectProperties: TdxFloatingObjectProperties);
    procedure ExportFloatingObjectVerticalPositionTypeProperty(AFloatingObjectProperties: TdxFloatingObjectProperties);
    procedure ExportFloatingObjectLayoutInTableCell(AFloatingObjectProperties: TdxFloatingObjectProperties);
    procedure ExportFloatingObjectAllowOverlap(AFloatingObjectProperties: TdxFloatingObjectProperties);
    procedure ExportFloatingObjectLockAspectRatio(AFloatingObjectProperties: TdxFloatingObjectProperties);
    procedure ExportFloatingObjectHidden(AFloatingObjectProperties: TdxFloatingObjectProperties);
    procedure ExportFloatingObjectBehindDocument(AFloatingObjectProperties: TdxFloatingObjectProperties);
    procedure ExportFloatingObjectLeftDistance(AFloatingObjectProperties: TdxFloatingObjectProperties);
    procedure ExportFloatingObjectRightDistance(AFloatingObjectProperties: TdxFloatingObjectProperties);
    procedure ExportFloatingObjectTopDistance(AFloatingObjectProperties: TdxFloatingObjectProperties);
    procedure ExportFloatingObjectBottomDistance(AFloatingObjectProperties: TdxFloatingObjectProperties);
    procedure ExportFloatingObjectRelativeSize(AFloatingObjectProperties: TdxFloatingObjectProperties);
    procedure ExportPicture(ARun: TdxFloatingObjectAnchorRun; AContent: TdxPictureFloatingObjectContent);
    procedure ExportTextBoxContent(ARun: TdxFloatingObjectAnchorRun; AContent: TdxTextBoxFloatingObjectContent);
    procedure ExportFloatingObjectTextWrapType(AFloatingObjectProperties: TdxFloatingObjectProperties);
    procedure ExportFloatingObjectHorizontalPositionType(AFloatingObjectProperties: TdxFloatingObjectProperties);
    procedure ExportFloatingObjectVerticalPositionType(AFloatingObjectProperties: TdxFloatingObjectProperties);
    procedure ExportTextBoxProperties(AProperties: TdxTextBoxProperties);
    procedure ExportFloatingObjectRotation(AShape: TdxShape);

    procedure ExportFootNoteRunCore(ARun: TdxFootNoteRunBase; const ASuffixKeyword: string);
    procedure ExportFootNoteRun(ARun: TdxFootNoteRun); override;
    procedure ExportEndNoteRun(ARun: TdxEndNoteRun); override;

    function AreFootNotesPresent: Boolean;
    function AreEndNotesPresent: Boolean;
    procedure ExportDocumentLevelFootNotePresenceProperties;
    procedure ExportDocumentLevelFootNoteProperties;
    procedure ExportDocumentLevelEndNoteProperties;

    procedure ExportDocumentInformation; virtual;
    procedure ExportDocumentProtectionProperties; virtual;
    procedure ExportDocumentProtectionPasswordHash; virtual;
    procedure ExportPageBackground(ADocumentProperties: TdxDocumentProperties); virtual;
    function ShouldExportDocumentInformation: Boolean; virtual;
    function ShouldExportDocumentProtectionPasswordHash: Boolean; virtual;
    function ShouldExportDocumentProtectionPasswordHashInWord2007Format: Boolean; virtual;
    procedure ExportDocumentProtectionPasswordHashWord2007; virtual;
    function GetDocumentProtectionPasswordHashWord2007Bytes: TArray<Byte>;
    procedure ExportDocumentProtectionPasswordHashWord2003; virtual;

    procedure ExportFieldCodeStartRun(ARun: TdxFieldCodeStartRun); override;
    procedure ExportFieldCodeEndRun(ARun: TdxFieldCodeEndRun); override;
    procedure ExportFieldResultEndRun(ARun: TdxFieldResultEndRun); override;
    procedure ExportNumberingListTable;
    procedure ExportDefaultCharacterProperties;
    procedure ExportDefaultParagraphProperties;
    procedure ExportInlinePictureRun(ARun: TdxInlinePictureRun); override;
    procedure ExportStyleTable;
    procedure ExportDocumentProperties;
    procedure ExportSectionProperties(ASection: TdxSection);
    procedure ExportSectionMargins(AMargins: TdxSectionMargins);
    procedure ExportSectionPage(APage: TdxSectionPage);
    procedure ExportSectionGeneralSettings(ASettings: TdxSectionGeneralSettings);
    procedure ExportSectionPageNumbering(APageNumbering: TdxSectionPageNumbering);
    procedure ExportSectionLineNumbering(ALineNumbering: TdxSectionLineNumbering);
    procedure ExportSectionColumns(AColumns: TdxSectionColumns);
    procedure ExportSectionFootNote(ANote: TdxSectionFootNote);
    procedure ExportSectionEndNote(ANote: TdxSectionFootNote);
    procedure ExportSectionColumnsDetails(AColumns: TdxColumnInfoCollection);
    procedure ExportSectionColumn(AColumn: TdxColumnInfo; AColumnIndex: Integer);
    procedure ExportLegacyPageProperties(ASection: TdxSection; ASectionCount: Integer);
    function ExportParagraph(AParagraph: TdxParagraph): TdxParagraphIndex; override;
    procedure ExportSingleParagraph(AParagraph: TdxParagraph);
    procedure ExportParagraphTableStyleProperties(ACondTypes: TdxConditionalTableStyleFormattingTypes;
      ATableStyleIndex: Integer);
    procedure ExportParagraphCharacterProperties(AParagraph: TdxParagraph);
    procedure ExportTextRun(ARun: TdxTextRun); override;
    procedure ExportFormattingFlags;
    function ExportRtfTable(ATable: TdxTable): TdxParagraphIndex; virtual;
    function ShouldUseCustomSaveTableMethod: Boolean; override;

    property RtfOptions: TdxRtfDocumentExporterOptions read FOptions;
  public
    constructor Create(ADocumentModel: TdxDocumentModel; const AOptions: IdxExporterOptions;
      const ARtfExportHelper: IdxRtfExportHelper); reintroduce;
    destructor Destroy; override;

    procedure Export; override;
    procedure Export(AOutputStream: TStream); override;

    procedure ExportFirstPageHeader(ASectionHeader: TdxSectionHeader; ALinkedToPrevious: Boolean); override;
    procedure ExportOddPageHeader(ASectionHeader: TdxSectionHeader; ALinkedToPrevious: Boolean); override;
    procedure ExportEvenPageHeader(ASectionHeader: TdxSectionHeader; ALinkedToPrevious: Boolean); override;
    procedure ExportFirstPageFooter(ASectionFooter: TdxSectionFooter; ALinkedToPrevious: Boolean); override;
    procedure ExportOddPageFooter(ASectionFooter: TdxSectionFooter; ALinkedToPrevious: Boolean); override;
    procedure ExportEvenPageFooter(ASectionFooter: TdxSectionFooter; ALinkedToPrevious: Boolean); override;

    procedure ExportParagraphCore(AParagraph: TdxParagraph; ATableNestingLevel: Integer;
      ACondTypes: TdxConditionalTableStyleFormattingTypes; ATableStyleIndex: Integer);

    property RtfBuilder: TdxRtfBuilder read FRtfBuilder;
    property UnitConverter: TdxDocumentModelUnitConverter read GetUnitConverter;

    class property ChapterSeparatorTypes: TdxCharStringDictionary read FChapterSeparatorTypes;
    class property PageNumberingTypes: TdxEnumeratedDictionary<TdxNumberingFormat, string> read FPageNumberingTypes;
    class property SectionFootNoteNumberingTypes: TdxEnumeratedDictionary<TdxNumberingFormat, string> read FSectionFootNoteNumberingTypes;
    class property SectionEndNoteNumberingTypes: TdxEnumeratedDictionary<TdxNumberingFormat, string> read FSectionEndNoteNumberingTypes;
    class property FootNoteNumberingTypes: TdxEnumeratedDictionary<TdxNumberingFormat, string> read FFootNoteNumberingTypes;
    class property EndNoteNumberingTypes: TdxEnumeratedDictionary<TdxNumberingFormat, string> read FEndNoteNumberingTypes;
    class property PredefinedUserGroups: TDictionary<Integer, string> read FPredefinedUserGroups;

    property RtfExportHelper: IdxRtfExportHelper read FRtfExportHelper;
  end;

  { TdxRtfBuilder }

  TdxRtfBuilder = class
  strict private
    class var
      FSpecialMarks: TdxCharStringDictionary;
      FSpecialPCDataMarks: TdxCharStringDictionary;
    class constructor Initialize;
    class destructor Finalize;
    class procedure AddHexToMarkTable(ATable: TdxCharStringDictionary; ACh: Char);
    class function CreateSpecialMarksTable: TdxCharStringDictionary;
    class function CreateSpecialPCDataMarksTable: TdxCharStringDictionary;
  private
    FEncoding: TEncoding;
    FHexMode: Boolean;
    FRowLength: Integer;
    FRtfContent: TdxChunkedStringBuilder;
    FIsPreviousWriteCommand: Boolean;
    FUnicodeTextBuilder: TStringBuilder;
    function IsSpecialSymbol(ACh: Char): Boolean;
    function GetBoolParameterValue(AValue: Boolean): string;
  protected
    procedure AppendUnicodeCompatibleCharCore(ACode: Integer; ACh: Char); virtual;
    procedure IncreaseRowLength(ADelta: Integer);
    function GetUnicodeCompatibleString(ACh: Char): string;
    function GetUnicodeCompatibleStringDirect(const AText: string): string;

    procedure WriteTextDirectUnsafe(AText: TdxChunkedStringBuilder);
    procedure WriteTextDirect(const AText: string; AMakeStringUnicodeCompatible: Boolean = False);
    procedure WriteTextCore(const AText: string; ASpecialMarks: TdxCharStringDictionary);
    procedure WriteStreamAsHex(AStream: TStream);
    procedure WriteStreamAsHexCore(AStream: TStream);
    procedure WriteByteArrayAsHex(ABuffer: TBytes; AOffset: Integer = 0; ALength: Integer = -1);
    procedure WriteMemoryStreamAsHex(AStream: TBytesStream);

    property Encoding: TEncoding read FEncoding;
    property UnicodeTextBuilder: TStringBuilder read FUnicodeTextBuilder;
    class property SpecialMarks: TdxCharStringDictionary read FSpecialMarks;
    class property SpecialPCDataMarks: TdxCharStringDictionary read FSpecialPCDataMarks;
  public
    constructor Create(AEncoding: TEncoding); virtual;
    destructor Destroy; override;

    procedure Clear;

    procedure CloseGroup;
    procedure OpenGroup;

    procedure WriteChar(ACh: Char);
    procedure WriteCommand(const ACommand: string); overload;
    procedure WriteCommand(const ACommand: string; AParam: Integer); overload;
    procedure WriteCommand(const ACommand, AParam: string); overload;
    procedure WriteText(const AText: string);
    procedure WritePCData(const AText: string);
    procedure WriteShapeProperty(const APropertyName, APropertyValue: string);
    procedure WriteShapeBoolProperty(const APropertyName: string; APropertyValue: Boolean);
    procedure WriteShapeIntegerProperty(const APropertyName: string; APropertyValue: Integer);
    procedure WriteShapeColorProperty(const APropertyName: string; APropertyValue: TdxAlphaColor);
    procedure WriteShapePropertyName(const APropertyName: string);
    procedure WriteShapePropertyValue(const APropertyValue: string);

    function RowLengthBound: Integer; virtual;
    function ExtractRtfContent: TdxChunkedStringBuilder;

    property RtfContent: TdxChunkedStringBuilder read FRtfContent;
  end;

  { TdxDBCSRtfBuilder }

  TdxDBCSRtfBuilder = class(TdxRtfBuilder)
  protected
    procedure AppendUnicodeCompatibleCharCore(ACode: Integer; ACh: Char); override;
  public
    constructor Create(AEncoding: TEncoding); override;
  end;

  { TdxRtfExporter}

  TdxRtfExporter = class(TdxCustomDocumentModelExporter)
  strict private
    FContentExporter: TdxRtfContentExporter;
    FRtfBuilder: TdxRtfBuilder;
    FRtfExportHelper: IdxRtfExportHelper;
    FFontNameStringBuilder: TStringBuilder;
    function GetDocumentModel: TdxDocumentModel;
  private
    function EscapeSpecialCharacters(const AFontName: string): string;
    function GetLastParagraphRunNotSelected: Boolean;
    procedure SetLastParagraphRunNotSelected(const Value: Boolean);
    function GetKeepFieldCodeViewState: Boolean;
    procedure SetKeepFieldCodeViewState(const Value: Boolean);
  protected
    function CreateContentExporter(ADocumentModel: TdxDocumentModel; const AOptions: IdxExporterOptions;
      const ARtfExportHelper: IdxRtfExportHelper): TdxRtfContentExporter; virtual;

    function CalculateFontNameEncoding(const AFontName: string; AFontIndex: Integer): TEncoding;
    function EscapeFontName(const AFontName: string; AEncoding: TEncoding): string;

    procedure ExportCore;

    procedure ExportColorTable;
    procedure ExportColorTableEntry(const AColor: TdxAlphaColor);
    procedure ExportDefaultCharacterProperties;
    procedure ExportDefaultParagraphProperties;
    procedure ExportDefaultProperties;
    procedure ExportDocumentVariables;
    procedure ExportFontTable;
    procedure ExportFontTableEntry(const AFontName: string; AFontIndex: Integer);
    procedure ExportListOverrideTable;
    procedure ExportListTable;
    procedure ExportParagraphGroupProperties;
    procedure ExportStyleTable;
    procedure ExportUsersTable;
    property ContentExporter: TdxRtfContentExporter read FContentExporter;
    property RtfBuilder: TdxRtfBuilder read FRtfBuilder;
    property RtfExportHelper: IdxRtfExportHelper read FRtfExportHelper;
  public
    constructor Create(ADocumentModel: TdxCustomDocumentModel; const AOptions: IdxExporterOptions); override;
    destructor Destroy; override;

    // IdxRichEditExporter
    function Export: string;

    function ExportSaveMemory: TdxChunkedStringBuilder; override;

    property DocumentModel: TdxDocumentModel read GetDocumentModel;
    property LastParagraphRunNotSelected: Boolean read GetLastParagraphRunNotSelected write SetLastParagraphRunNotSelected;
    property KeepFieldCodeViewState: Boolean read GetKeepFieldCodeViewState write SetKeepFieldCodeViewState;
  end;

  { TdxRtfArtBorderConverter }

  TdxRtfArtBorderConverter = class
  private const
{$REGION 'LineStyles'}
    LineStyles: array[0..163] of TdxBorderLineStyle = (
      TdxBorderLineStyle.Apples,
      TdxBorderLineStyle.ArchedScallops,
      TdxBorderLineStyle.BabyPacifier,
      TdxBorderLineStyle.BabyRattle,
      TdxBorderLineStyle.Balloons3Colors,
      TdxBorderLineStyle.BalloonsHotAir,
      TdxBorderLineStyle.BasicBlackDashes,
      TdxBorderLineStyle.BasicBlackDots,
      TdxBorderLineStyle.BasicBlackSquares,
      TdxBorderLineStyle.BasicThinLines,
      TdxBorderLineStyle.BasicWhiteDashes,
      TdxBorderLineStyle.BasicWhiteDots,
      TdxBorderLineStyle.BasicWhiteSquares,
      TdxBorderLineStyle.BasicWideInline,
      TdxBorderLineStyle.BasicWideMidline,
      TdxBorderLineStyle.BasicWideOutline,
      TdxBorderLineStyle.Bats,
      TdxBorderLineStyle.Birds,
      TdxBorderLineStyle.BirdsFlight,
      TdxBorderLineStyle.Cabins,
      TdxBorderLineStyle.CakeSlice,
      TdxBorderLineStyle.CandyCorn,
      TdxBorderLineStyle.CelticKnotwork,
      TdxBorderLineStyle.CertificateBanner,
      TdxBorderLineStyle.ChainLink,
      TdxBorderLineStyle.ChampagneBottle,
      TdxBorderLineStyle.CheckedBarBlack,
      TdxBorderLineStyle.CheckedBarColor,
      TdxBorderLineStyle.Checkered,
      TdxBorderLineStyle.ChristmasTree,
      TdxBorderLineStyle.CirclesLines,
      TdxBorderLineStyle.CirclesRectangles,
      TdxBorderLineStyle.ClassicalWave,
      TdxBorderLineStyle.Clocks,
      TdxBorderLineStyle.Compass,
      TdxBorderLineStyle.Confetti,
      TdxBorderLineStyle.ConfettiGrays,
      TdxBorderLineStyle.ConfettiOutline,
      TdxBorderLineStyle.ConfettiStreamers,
      TdxBorderLineStyle.ConfettiWhite,
      TdxBorderLineStyle.CornerTriangles,
      TdxBorderLineStyle.CouponCutoutDashes,
      TdxBorderLineStyle.CouponCutoutDots,
      TdxBorderLineStyle.CrazyMaze,
      TdxBorderLineStyle.CreaturesButterfly,
      TdxBorderLineStyle.CreaturesFish,
      TdxBorderLineStyle.CreaturesInsects,
      TdxBorderLineStyle.CreaturesLadyBug,
      TdxBorderLineStyle.CrossStitch,
      TdxBorderLineStyle.Cup,
      TdxBorderLineStyle.DecoArch,
      TdxBorderLineStyle.DecoArchColor,
      TdxBorderLineStyle.DecoBlocks,
      TdxBorderLineStyle.DiamondsGray,
      TdxBorderLineStyle.DoubleD,
      TdxBorderLineStyle.DoubleDiamonds,
      TdxBorderLineStyle.Earth1,
      TdxBorderLineStyle.Earth2,
      TdxBorderLineStyle.EclipsingSquares1,
      TdxBorderLineStyle.EclipsingSquares2,
      TdxBorderLineStyle.EggsBlack,
      TdxBorderLineStyle.Fans,
      TdxBorderLineStyle.Film,
      TdxBorderLineStyle.Firecrackers,
      TdxBorderLineStyle.FlowersBlockPrint,
      TdxBorderLineStyle.FlowersDaisies,
      TdxBorderLineStyle.FlowersModern1,
      TdxBorderLineStyle.FlowersModern2,
      TdxBorderLineStyle.FlowersPansy,
      TdxBorderLineStyle.FlowersRedRose,
      TdxBorderLineStyle.FlowersRoses,
      TdxBorderLineStyle.FlowersTeacup,
      TdxBorderLineStyle.FlowersTiny,
      TdxBorderLineStyle.Gems,
      TdxBorderLineStyle.GingerbreadMan,
      TdxBorderLineStyle.Gradient,
      TdxBorderLineStyle.Handmade1,
      TdxBorderLineStyle.Handmade2,
      TdxBorderLineStyle.HeartBalloon,
      TdxBorderLineStyle.HeartGray,
      TdxBorderLineStyle.Hearts,
      TdxBorderLineStyle.HeebieJeebies,
      TdxBorderLineStyle.Holly,
      TdxBorderLineStyle.HouseFunky,
      TdxBorderLineStyle.Hypnotic,
      TdxBorderLineStyle.IceCreamCones,
      TdxBorderLineStyle.LightBulb,
      TdxBorderLineStyle.Lightning1,
      TdxBorderLineStyle.Lightning2,
      TdxBorderLineStyle.MapPins,
      TdxBorderLineStyle.MapleLeaf,
      TdxBorderLineStyle.MapleMuffins,
      TdxBorderLineStyle.Marquee,
      TdxBorderLineStyle.MarqueeToothed,
      TdxBorderLineStyle.Moons,
      TdxBorderLineStyle.Mosaic,
      TdxBorderLineStyle.MusicNotes,
      TdxBorderLineStyle.Northwest,
      TdxBorderLineStyle.Ovals,
      TdxBorderLineStyle.Packages,
      TdxBorderLineStyle.PalmsBlack,
      TdxBorderLineStyle.PalmsColor,
      TdxBorderLineStyle.PaperClips,
      TdxBorderLineStyle.Papyrus,
      TdxBorderLineStyle.PartyFavor,
      TdxBorderLineStyle.PartyGlass,
      TdxBorderLineStyle.Pencils,
      TdxBorderLineStyle.People,
      TdxBorderLineStyle.PeopleWaving,
      TdxBorderLineStyle.PeopleHats,
      TdxBorderLineStyle.Poinsettias,
      TdxBorderLineStyle.PostageStamp,
      TdxBorderLineStyle.Pumpkin1,
      TdxBorderLineStyle.PushPinNote2,
      TdxBorderLineStyle.PushPinNote1,
      TdxBorderLineStyle.Pyramids,
      TdxBorderLineStyle.PyramidsAbove,
      TdxBorderLineStyle.Quadrants,
      TdxBorderLineStyle.Rings,
      TdxBorderLineStyle.Safari,
      TdxBorderLineStyle.Sawtooth,
      TdxBorderLineStyle.SawtoothGray,
      TdxBorderLineStyle.ScaredCat,
      TdxBorderLineStyle.Seattle,
      TdxBorderLineStyle.ShadowedSquares,
      TdxBorderLineStyle.SharksTeeth,
      TdxBorderLineStyle.ShorebirdTracks,
      TdxBorderLineStyle.Skyrocket,
      TdxBorderLineStyle.SnowflakeFancy,
      TdxBorderLineStyle.Snowflakes,
      TdxBorderLineStyle.Sombrero,
      TdxBorderLineStyle.Southwest,
      TdxBorderLineStyle.Stars,
      TdxBorderLineStyle.StarsTop,
      TdxBorderLineStyle.Stars3d,
      TdxBorderLineStyle.StarsBlack,
      TdxBorderLineStyle.StarsShadowed,
      TdxBorderLineStyle.Sun,
      TdxBorderLineStyle.Swirligig,
      TdxBorderLineStyle.TornPaper,
      TdxBorderLineStyle.TornPaperBlack,
      TdxBorderLineStyle.Trees,
      TdxBorderLineStyle.TriangleParty,
      TdxBorderLineStyle.Triangles,
      TdxBorderLineStyle.Tribal1,
      TdxBorderLineStyle.Tribal2,
      TdxBorderLineStyle.Tribal3,
      TdxBorderLineStyle.Tribal4,
      TdxBorderLineStyle.Tribal5,
      TdxBorderLineStyle.Tribal6,
      TdxBorderLineStyle.TwistedLines1,
      TdxBorderLineStyle.TwistedLines2,
      TdxBorderLineStyle.Vine,
      TdxBorderLineStyle.Waveline,
      TdxBorderLineStyle.WeavingAngles,
      TdxBorderLineStyle.WeavingBraid,
      TdxBorderLineStyle.WeavingRibbon,
      TdxBorderLineStyle.WeavingStrips,
      TdxBorderLineStyle.WhiteFlowers,
      TdxBorderLineStyle.Woodwork,
      TdxBorderLineStyle.XIllusions,
      TdxBorderLineStyle.ZanyTriangles,
      TdxBorderLineStyle.ZigZag,
      TdxBorderLineStyle.ZigZagStitch);
{$ENDREGION}
  public
    class function GetBorderArtIndex(ABorderLineStyle: TdxBorderLineStyle): Integer; static; inline;
    class function GetBorderLineStyle(ABorderArtIndex: Integer): TdxBorderLineStyle; static; inline;
  end;

  { TdxRtfPictureExporter }

  TdxRtfPictureExporter = class abstract
  private
    FRtfBuilder: TdxRtfBuilder;
    FRun: IdxPictureContainerRun;
    function GetImage: TdxOfficeImage;
    function GetUnitConverter: TdxDocumentModelUnitConverter;
  protected
    function GetPictureSize: TSize; virtual; abstract;
    function GetDesiredPictureSize: TSize; virtual; abstract;
    function GetPictureScale: TdxSizeF; virtual; abstract;
    function RtfPictureType: string; virtual; abstract;
    function GetImageBytesStream: TStream; virtual; abstract;

    procedure WritePictureHeader;

    property UnitConverter: TdxDocumentModelUnitConverter read GetUnitConverter;
    property Image: TdxOfficeImage read GetImage;
    property RtfBuilder: TdxRtfBuilder read FRtfBuilder;
    property Run: IdxPictureContainerRun read FRun;
  public
    constructor Create(ARtfBuilder: TdxRtfBuilder; const ARun: IdxPictureContainerRun);
    class function CreateRtfPictureExporter(ARtfBuilder: TdxRtfBuilder; const ARun: IdxPictureContainerRun;
      AImageFormat: TdxOfficeImageFormat): TdxRtfPictureExporter;
    procedure Export;
  end;

  { TdxRtfMetafilePictureExporter }

  TdxRtfMetafilePictureExporter = class abstract(TdxRtfPictureExporter)
  protected
    function GetDesiredPictureSize: TSize; override;
    function GetPictureScale: TdxSizeF; override;
    function GetPictureSize: TSize; override;
  end;

  { TdxRtfWmfPictureExporter }

  TdxRtfWmfPictureExporter = class(TdxRtfMetafilePictureExporter)
  protected
    function RtfPictureType: string; override;
    function GetImageBytesStream: TStream; override;
  end;

  { TdxRtfEmfPictureExporter }

  TdxRtfEmfPictureExporter = class(TdxRtfMetafilePictureExporter)
  protected
    function RtfPictureType: string; override;
    function GetImageBytesStream: TStream; override;
  end;

  { TdxRtfBitmapPictureExporter }

  TdxRtfBitmapPictureExporter = class abstract(TdxRtfPictureExporter)
  protected
    function GetImageBytesStream: TStream; override;
    function GetDesiredPictureSize: tagSIZE; override;
    function GetPictureScale: TdxSizeF; override;
    function GetPictureSize: tagSIZE; override;

    function GetImageFormat: TdxOfficeImageFormat; virtual; abstract;

    property ImageFormat: TdxOfficeImageFormat read GetImageFormat;
  end;

  { TdxRtfJpegPictureExporter }

  TdxRtfJpegPictureExporter = class(TdxRtfBitmapPictureExporter)
  protected
    function RtfPictureType: string; override;
    function GetImageFormat: TdxOfficeImageFormat; override;
  end;

  { TdxRtfPngPictureExporter }

  TdxRtfPngPictureExporter = class(TdxRtfBitmapPictureExporter)
  protected
    function RtfPictureType: string; override;
    function GetImageFormat: TdxOfficeImageFormat; override;
  end;

  { TdxExportRtfFormat }

  TdxExportRtfFormat = class(TdxExportFileFormat)
  public
    class function GetDocumentFormat: TdxRichEditDocumentFormat; override;
    function GetExporter(ADocumentModel: TdxCustomDocumentModel;
      const AOptions: IdxExporterOptions): TdxCustomDocumentModelExporter; override;
    function GetDocumentExporter: IdxExporter; override;
  end;

implementation

uses
{$IFDEF DELPHIXE2}
  System.UITypes,
{$ENDIF}
  Contnrs, RTLConsts, dxRichEdit.Utils.BatchUpdateHelper,
  TypInfo, Math, StrUtils, dxCore, dxCoreClasses,
  dxTypeHelpers,
  dxEncoding,
  dxRichEdit.Utils.Exceptions,
  dxStringHelper,
  dxRichEdit.Import.Rtf.DestinationListLevel,
  dxRichEdit.Import.Rtf.DestinationShape,
  dxRichEdit.Import.Rtf.DestinationPieceTable,
  dxRichEdit.Export.Rtf.TableExporter,
  dxRichEdit.DocumentModel.Fields.Core,
  dxRichEdit.DocumentModel.Fields,
  dxRichEdit.DocumentModel.SectionRange,
  dxRichEdit.DocumentModel.ProtectionFormatting,
  dxRichEdit.Export.Rtf.DocumentExporter;

type
  TdxRtfDocumentExporterCompatibilityOptionsAccess = class(TdxRtfDocumentExporterCompatibilityOptions);

  { TdxRtfPictureExportStrategy }

  TdxRtfPictureExportStrategy = class abstract
  protected
    procedure ExportShapePicturePrefix(ARtfBuilder: TdxRtfBuilder); virtual; abstract;
    procedure ExportShapePicturePostfix(ARtfBuilder: TdxRtfBuilder); virtual; abstract;
    procedure ExportNonShapePicturePrefix(ARtfBuilder: TdxRtfBuilder); virtual; abstract;
    procedure ExportNonShapePicturePostfix(ARtfBuilder: TdxRtfBuilder); virtual; abstract;
  public
    procedure Export(ARtfBuilder: TdxRtfBuilder; const ARun: IdxPictureContainerRun; ADuplicateObjectAsMetafile: Boolean);
  end;

  { TdxRtfFloatingObjectPictureExportStrategy }

  TdxRtfFloatingObjectPictureExportStrategy = class(TdxRtfPictureExportStrategy)
  protected
    procedure ExportShapePicturePrefix(ARtfBuilder: TdxRtfBuilder); override;
    procedure ExportShapePicturePostfix(ARtfBuilder: TdxRtfBuilder); override;
    procedure ExportNonShapePicturePrefix(ARtfBuilder: TdxRtfBuilder); override;
    procedure ExportNonShapePicturePostfix(ARtfBuilder: TdxRtfBuilder); override;
  end;

  { TdxRtfInlinePictureExportStrategy }

  TdxRtfInlinePictureExportStrategy = class(TdxRtfPictureExportStrategy)
  protected
    procedure ExportShapePicturePrefix(ARtfBuilder: TdxRtfBuilder); override;
    procedure ExportShapePicturePostfix(ARtfBuilder: TdxRtfBuilder); override;
    procedure ExportNonShapePicturePrefix(ARtfBuilder: TdxRtfBuilder); override;
    procedure ExportNonShapePicturePostfix(ARtfBuilder: TdxRtfBuilder); override;
  end;

{ TdxRtfPictureExportStrategy }

procedure TdxRtfPictureExportStrategy.Export(ARtfBuilder: TdxRtfBuilder; const ARun: IdxPictureContainerRun;
  ADuplicateObjectAsMetafile: Boolean);
var
  AFormat: TdxOfficeImageFormat;
  AExporter: TdxRtfPictureExporter;
  AWmfExporter: TdxRtfPictureExporter;
begin
  AFormat := ARun.PictureContent.Image.RawFormat;
  if AFormat <> TdxOfficeImageFormat.Wmf then
  begin
    ExportShapePicturePrefix(ARtfBuilder);
    AExporter := TdxRtfPictureExporter.CreateRtfPictureExporter(ARtfBuilder, ARun, AFormat);
    try
      AExporter.Export;
      ExportShapePicturePostfix(ARtfBuilder);
      if ADuplicateObjectAsMetafile and ARun.PictureContent.Image.CanGetImageBytes(TdxOfficeImageFormat.Wmf) then
      begin
        AWMfExporter := TdxRtfPictureExporter.CreateRtfPictureExporter(ARtfBuilder, ARun, TdxOfficeImageFormat.Wmf);
        try
          if AWmfExporter <> nil then
          begin
            ExportNonShapePicturePrefix(ARtfBuilder);
            AWmfExporter.Export;
            ExportNonShapePicturePostfix(ARtfBuilder);
          end;
        finally
          AWMfExporter.Free;
        end;
      end;
    finally
      AExporter.Free;
    end;
  end
  else
  begin
    AWmfExporter := TdxRtfPictureExporter.CreateRtfPictureExporter(ARtfBuilder, ARun, TdxOfficeImageFormat.Wmf);
    try
      if AWmfExporter <> nil then
        AWmfExporter.Export;
    finally
      AWmfExporter.Free;
    end;
  end;
end;

{ TdxRtfFloatingObjectPictureExportStrategy }

procedure TdxRtfFloatingObjectPictureExportStrategy.ExportShapePicturePrefix(ARtfBuilder: TdxRtfBuilder);
begin
  ARtfBuilder.OpenGroup;
  ARtfBuilder.WriteCommand(TdxRtfExportSR.ShapeProperty);
  ARtfBuilder.WriteShapePropertyName('pib');
  ARtfBuilder.OpenGroup;
  ARtfBuilder.WriteCommand(TdxRtfExportSR.ShapePropertyValue);
end;

procedure TdxRtfFloatingObjectPictureExportStrategy.ExportShapePicturePostfix(ARtfBuilder: TdxRtfBuilder);
begin
  ARtfBuilder.CloseGroup;
  ARtfBuilder.CloseGroup;
end;

procedure TdxRtfFloatingObjectPictureExportStrategy.ExportNonShapePicturePrefix(ARtfBuilder: TdxRtfBuilder);
begin
  ARtfBuilder.OpenGroup;
  ARtfBuilder.WriteCommand(TdxRtfExportSR.ShapeResult);
  ARtfBuilder.WriteCommand(TdxRtfExportSR.EndOfParagraph);
  ARtfBuilder.WriteCommand(TdxRtfExportSR.ResetParagraphProperties);
end;

procedure TdxRtfFloatingObjectPictureExportStrategy.ExportNonShapePicturePostfix(ARtfBuilder: TdxRtfBuilder);
begin
  ARtfBuilder.CloseGroup;
end;

{ TdxRtfInlinePictureExportStrategy }

procedure TdxRtfInlinePictureExportStrategy.ExportShapePicturePrefix(ARtfBuilder: TdxRtfBuilder);
begin
  ARtfBuilder.OpenGroup;
  ARtfBuilder.WriteCommand(TdxRtfExportSR.ShapePicture);
end;

procedure TdxRtfInlinePictureExportStrategy.ExportShapePicturePostfix(ARtfBuilder: TdxRtfBuilder);
begin
  ARtfBuilder.CloseGroup;
end;

procedure TdxRtfInlinePictureExportStrategy.ExportNonShapePicturePrefix(ARtfBuilder: TdxRtfBuilder);
begin
  ARtfBuilder.OpenGroup;
  ARtfBuilder.WriteCommand(TdxRtfExportSR.NonShapePicture);
end;

procedure TdxRtfInlinePictureExportStrategy.ExportNonShapePicturePostfix(ARtfBuilder: TdxRtfBuilder);
begin
  ARtfBuilder.CloseGroup;
end;

{ TdxRtfPictureExporter }

constructor TdxRtfPictureExporter.Create(ARtfBuilder: TdxRtfBuilder;
  const ARun: IdxPictureContainerRun);
begin
  inherited Create;
  FRtfBuilder := ARtfBuilder;
  FRun := ARun;
end;

class function TdxRtfPictureExporter.CreateRtfPictureExporter(ARtfBuilder: TdxRtfBuilder;
  const ARun: IdxPictureContainerRun; AImageFormat: TdxOfficeImageFormat): TdxRtfPictureExporter;
begin
  if not ARun.PictureContent.Image.IsExportSupported(AImageFormat) then
    Result := nil
  else
  if AImageFormat = TdxOfficeImageFormat.Wmf then
    Result := TdxRtfWmfPictureExporter.Create(ARtfBuilder, ARun)
  else
  if AImageFormat = TdxOfficeImageFormat.Emf then
    Result := TdxRtfEmfPictureExporter.Create(ARtfBuilder, ARun)
  else
  if AImageFormat = TdxOfficeImageFormat.Jpeg then
    Result := TdxRtfJpegPictureExporter.Create(ARtfBuilder, ARun)
  else
    Result := TdxRtfPngPictureExporter.Create(ARtfBuilder, ARun);
end;

procedure TdxRtfPictureExporter.Export;
var
  AStream: TStream;
begin
  RtfBuilder.OpenGroup;
  WritePictureHeader;
  try
    AStream := GetImageBytesStream;
    try
      RtfBuilder.WriteStreamAsHex(AStream);
    finally
      AStream.Free;
    end;
  finally
    RtfBuilder.CloseGroup;
  end;
end;

function TdxRtfPictureExporter.GetImage: TdxOfficeImage;
begin
  Result := Run.PictureContent.Image.Image;
end;

function TdxRtfPictureExporter.GetUnitConverter: TdxDocumentModelUnitConverter;
begin
  Result := Run.PictureContent.Paragraph.DocumentModel.UnitConverter;
end;

procedure TdxRtfPictureExporter.WritePictureHeader;
var
  APirctureSize, ADesiredPictureSize: TSize;
  APictureScale: TdxSizeF;
begin
  APirctureSize := GetPictureSize;
  ADesiredPictureSize := GetDesiredPictureSize;
  APictureScale := GetPictureScale;
  while (ADesiredPictureSize.cx > $7FFF) or (ADesiredPictureSize.cy > $7FFF) do
  begin
    ADesiredPictureSize.cx := ADesiredPictureSize.cx div 2;
    ADesiredPictureSize.cy := ADesiredPictureSize.cy div 2;
    APictureScale.cx := APictureScale.cx * 2;
    APictureScale.cy := APictureScale.cy * 2;
    APirctureSize.cx := APirctureSize.cx div 2;
    APirctureSize.cy := APirctureSize.cy div 2;
  end;

  RtfBuilder.WriteCommand(TdxRtfExportSR.Picture);
  RtfBuilder.WriteCommand(RtfPictureType);

  RtfBuilder.WriteCommand(TdxRtfExportSR.PictureWidth, Math.Max(APirctureSize.cx, 1));
  RtfBuilder.WriteCommand(TdxRtfExportSR.PictureHeight, Math.Max(APirctureSize.cy, 1));

  RtfBuilder.WriteCommand(TdxRtfExportSR.PictureDesiredWidth, Math.Max(ADesiredPictureSize.cx, 1));
  RtfBuilder.WriteCommand(TdxRtfExportSR.PictureDesiredHeight, Math.Max(ADesiredPictureSize.cy, 1));

  RtfBuilder.WriteCommand(TdxRtfExportSR.PictureScaleX, Math.Max(Round(APictureScale.cx), 1));
  RtfBuilder.WriteCommand(TdxRtfExportSR.PictureScaleY, Math.Max(Round(APictureScale.cy), 1));

  if Image.Uri <> '' then
  begin
    RtfBuilder.OpenGroup;
    try
      RtfBuilder.WriteCommand(TdxRtfExportSR.DxImageUri);
      RtfBuilder.WriteText(Image.Uri);
    finally
      RtfBuilder.CloseGroup;
    end;
  end;
end;

{ TdxRtfMetafilePictureExporter }

function TdxRtfMetafilePictureExporter.GetDesiredPictureSize: TSize;
begin
  Result := UnitConverter.ModelUnitsToTwips(Run.PictureContent.OriginalSize);
end;

function TdxRtfMetafilePictureExporter.GetPictureScale: TdxSizeF;
begin
  Result := dxSizeF(Run.ScaleX, Run.ScaleY);
end;

function TdxRtfMetafilePictureExporter.GetPictureSize: TSize;
begin
  Result := UnitConverter.ModelUnitsToHundredthsOfMillimeter(Run.PictureContent.OriginalSize);
end;

{ TdxRtfWmfPictureExporter }

function TdxRtfWmfPictureExporter.GetImageBytesStream: TStream;
begin
  Result := Image.GetImageBytesStream(TdxOfficeImageFormat.Wmf)
end;

function TdxRtfWmfPictureExporter.RtfPictureType: string;
begin
  Result := '\wmetafile8';
end;

{ TdxRtfEmfPictureExporter }

function TdxRtfEmfPictureExporter.GetImageBytesStream: TStream;
begin
  Result := Image.GetImageBytesStream(TdxOfficeImageFormat.Emf);
end;

function TdxRtfEmfPictureExporter.RtfPictureType: string;
begin
  Result := '\emfblip';
end;

{ TdxRtfBitmapPictureExporter }

function TdxRtfBitmapPictureExporter.GetImageBytesStream: TStream;
begin
  Result := Image.GetImageBytesStream(ImageFormat);
end;

function TdxRtfBitmapPictureExporter.GetDesiredPictureSize: tagSIZE;
begin
  Result := Image.SizeInTwips;
end;

function TdxRtfBitmapPictureExporter.GetPictureScale: TdxSizeF;
begin
  Result := dxSizeF(Run.ScaleX, Run.ScaleY);
end;

function TdxRtfBitmapPictureExporter.GetPictureSize: tagSIZE;
begin
  Result := Image.SizeInHundredthsOfMillimeter;
end;

{ TdxRtfJpegPictureExporter }

function TdxRtfJpegPictureExporter.RtfPictureType: string;
begin
  Result := '\jpegblip';
end;

function TdxRtfJpegPictureExporter.GetImageFormat: TdxOfficeImageFormat;
begin
  Result := TdxOfficeImageFormat.Jpeg;
end;

{ TdxRtfPngPictureExporter }

function TdxRtfPngPictureExporter.RtfPictureType: string;
begin
  Result := '\pngblip';
end;

function TdxRtfPngPictureExporter.GetImageFormat: TdxOfficeImageFormat;
begin
  Result := TdxOfficeImageFormat.Png;
end;

{ TdxRtfArtBorderConverter }

class function TdxRtfArtBorderConverter.GetBorderArtIndex(ABorderLineStyle: TdxBorderLineStyle): Integer;
begin
  if ABorderLineStyle in [TdxBorderLineStyle.Apples..TdxBorderLineStyle.ZigZagStitch] then
    Result := Ord(ABorderLineStyle) - Ord(TdxBorderLineStyle.Apples) + 1
  else
    Result := 0;
end;

class function TdxRtfArtBorderConverter.GetBorderLineStyle(ABorderArtIndex: Integer): TdxBorderLineStyle;
begin
  Dec(ABorderArtIndex);
  if (ABorderArtIndex >= Low(LineStyles)) and (ABorderArtIndex <= High(LineStyles)) then
    Result := LineStyles[ABorderArtIndex]
  else
    Result := TdxBorderLineStyle.None;
end;

{ TdxRtfExporter }

constructor TdxRtfExporter.Create(ADocumentModel: TdxCustomDocumentModel;
  const AOptions: IdxExporterOptions);
begin
  inherited Create(ADocumentModel, AOptions);
  FRtfExportHelper := TdxRtfExportHelper.Create;
  FContentExporter := CreateContentExporter(DocumentModel, AOptions, RtfExportHelper);
  FRtfBuilder := ContentExporter.CreateRtfBuilder;
  FFontNameStringBuilder := TStringBuilder.Create;
end;

destructor TdxRtfExporter.Destroy;
begin
  FreeAndNil(FFontNameStringBuilder);
  FreeAndNil(FRtfBuilder);
  FreeAndNil(FContentExporter);
  FRtfExportHelper := nil;
  inherited Destroy;
end;

function TdxRtfExporter.CalculateFontNameEncoding(const AFontName: string;
  AFontIndex: Integer): TEncoding;
var
  AFontCharset, ACodePage, I: Integer;
  AEncoding: TEncoding;
  AEncodings: TArray<TEncoding>;
begin
  if TdxEncoding.ASCII.CanBeLosslesslyEncoded(AFontName) then
    Exit(TdxEncoding.ASCII);

  if not RtfExportHelper.FontCharsetTable.TryGetValue(AFontIndex, AFontCharset) then
    AFontCharset := -1;

  if AFontCharset >= 0 then
  begin
    ACodePage := TdxEncoding.CodePageFromCharset(AFontCharset);
    try
      AEncoding := TdxEncoding.GetEncoding(ACodePage);
      if AEncoding.CanBeLosslesslyEncoded(AFontName) then
        Exit(AEncoding);
    except
    end;
  end;

  if FContentExporter.Options.ActualEncoding.CanBeLosslesslyEncoded(AFontName) then
    Exit(FContentExporter.Options.ActualEncoding);

  AEncodings := TdxEncoding.Encodings;
  for I := Low(AEncodings) to High(AEncodings) do
  begin
    try
      AEncoding := AEncodings[I];
      if (AEncoding.CodePage <> 65000) and // Utf-7
         (AEncoding.CodePage <> 65001) and // Utf-8
         (AEncoding.CodePage <> 1200)  and // Unicode
         (AEncoding.CodePage <> 1201) then // Unicode (BigEndian)
      begin
        if AEncoding.CanBeLosslesslyEncoded(AFontName) then
          Exit(AEncoding);
      end;
    except
    end;
  end;
  Result := nil;
end;

function TdxRtfExporter.CreateContentExporter(ADocumentModel: TdxDocumentModel;
  const AOptions: IdxExporterOptions;
  const ARtfExportHelper: IdxRtfExportHelper): TdxRtfContentExporter;
begin
  Result := TdxRtfContentExporter.Create(ADocumentModel, AOptions, ARtfExportHelper);
end;

function TdxRtfExporter.EscapeFontName(const AFontName: string;
  AEncoding: TEncoding): string;
var
  ABytes: TArray<Byte>;
  ACount, I: Integer;
begin
  ABytes := AEncoding.GetBytes(AFontName);
  ACount := Length(ABytes);
  FFontNameStringBuilder.Clear;
  for I := 0 to ACount - 1 do
  begin
    FFontNameStringBuilder.Append('\'#$27);
    FFontNameStringBuilder.Append(TdxStringHelper.ToHex(ABytes[I]));
  end;
  Result := FFontNameStringBuilder.ToString;
end;

function TdxRtfExporter.Export: string;
begin
  ExportCore;
  Result := RtfBuilder.RtfContent.ToString;
end;

procedure TdxRtfExporter.ExportColorTable;
var
  I: Integer;
  AColorCollection: TdxAlphaColorList;
begin
  RtfBuilder.OpenGroup;
  RtfBuilder.WriteCommand(TdxRtfExportSR.ColorTable);
  AColorCollection := RtfExportHelper.ColorCollection;
  for I := 0 to AColorCollection.Count - 1 do
    ExportColorTableEntry(AColorCollection[I]);
  RtfBuilder.CloseGroup;
end;

procedure TdxRtfExporter.ExportColorTableEntry(const AColor: TdxAlphaColor);
begin
  if not TdxAlphaColors.IsEmpty(AColor) then
  begin
    RtfBuilder.WriteCommand(TdxRtfExportSR.ColorRed, TdxAlphaColors.R(AColor));
    RtfBuilder.WriteCommand(TdxRtfExportSR.ColorGreen, TdxAlphaColors.G(AColor));
    RtfBuilder.WriteCommand(TdxRtfExportSR.ColorBlue, TdxAlphaColors.B(AColor));
  end;
  RtfBuilder.WriteTextDirect(';');
end;

procedure TdxRtfExporter.ExportCore;
var
  AContent: TdxChunkedStringBuilder;
begin
  ContentExporter.Export;
  AContent := ContentExporter.RtfBuilder.RtfContent;

  RtfBuilder.OpenGroup;
  RtfBuilder.WriteCommand(TdxRtfExportSR.RtfSignature);
  RtfBuilder.WriteCommand(TdxRtfExportSR.DefaultFontIndex, RtfExportHelper.DefaultFontIndex);

  ExportFontTable;
  ExportColorTable;
  ExportDefaultProperties;
  ExportStyleTable;
  ExportListTable;
  ExportListOverrideTable;
  ExportParagraphGroupProperties;
  ExportUsersTable;
  ExportDocumentVariables;

  RtfBuilder.WriteTextDirectUnsafe(AContent);
  RtfBuilder.CloseGroup;
end;

procedure TdxRtfExporter.ExportDefaultCharacterProperties;
begin
  if Length(RtfExportHelper.DefaultCharacterProperties) = 0 then
    Exit;
  RtfBuilder.OpenGroup;
  RtfBuilder.WriteCommand(TdxRtfExportSR.DefaultCharacterProperties);
  RtfBuilder.WriteTextDirect(RtfExportHelper.DefaultCharacterProperties);
  RtfBuilder.CloseGroup;
end;

procedure TdxRtfExporter.ExportDefaultParagraphProperties;
begin
  if Length(RtfExportHelper.DefaultParagraphProperties) = 0 then
    Exit;
  RtfBuilder.OpenGroup;
  RtfBuilder.WriteCommand(TdxRtfExportSR.DefaultParagraphProperties);
  RtfBuilder.WriteTextDirect(RtfExportHelper.DefaultParagraphProperties);
  RtfBuilder.CloseGroup;
end;

procedure TdxRtfExporter.ExportDefaultProperties;
begin
  ExportDefaultCharacterProperties;
  ExportDefaultParagraphProperties;
end;

procedure TdxRtfExporter.ExportDocumentVariables;
var
  AVariables: TdxDocumentVariableCollection;
  AName: string;
  AUpdateDocVariablesBeforePrint: TdxUpdateDocVariablesBeforePrint;
begin
  AVariables := ContentExporter.DocumentModel.Variables;
  AUpdateDocVariablesBeforePrint := ContentExporter.DocumentModel.DocumentProperties.UpdateDocVariablesBeforePrint;
  if (AVariables.Count = 0) and
      (AUpdateDocVariablesBeforePrint = TdxUpdateDocVariablesBeforePrint.Auto) then
    Exit;

  for AName in AVariables.GetVariableNames do
  begin
    RtfBuilder.OpenGroup;
    RtfBuilder.WriteCommand(TdxRtfExportSR.DocumentVariable);

    RtfBuilder.OpenGroup;
    RtfBuilder.WriteText(AName);
    RtfBuilder.CloseGroup;

    RtfBuilder.OpenGroup;
    RtfBuilder.WritePCData(AVariables[AName].ToString);
    RtfBuilder.CloseGroup;

    RtfBuilder.CloseGroup;
  end;
  if AUpdateDocVariablesBeforePrint <> TdxUpdateDocVariablesBeforePrint.Auto then
  begin
    RtfBuilder.OpenGroup;
    RtfBuilder.WriteCommand(TdxRtfExportSR.DocumentVariable);

    RtfBuilder.OpenGroup;
    RtfBuilder.WriteText(TdxDocumentProperties.UpdateDocVariableFieldsBeforePrintDocVarName);
    RtfBuilder.CloseGroup;

    RtfBuilder.OpenGroup;
    RtfBuilder.WritePCData(ContentExporter.DocumentModel.DocumentProperties.GetUpdateFieldsBeforePrintDocVarValue);
    RtfBuilder.CloseGroup;

    RtfBuilder.CloseGroup;
  end;
end;

procedure TdxRtfExporter.ExportFontTable;
var
  AFontNames: TdxStringList;
  I: Integer;
begin
  RtfBuilder.OpenGroup;
  RtfBuilder.WriteCommand(TdxRtfExportSR.FontTable);

  AFontNames := RtfExportHelper.FontNamesCollection;
  for I := 0 to AFontNames.Count - 1 do
    ExportFontTableEntry(AFontNames[I], I);

  RtfBuilder.CloseGroup;
end;

procedure TdxRtfExporter.ExportFontTableEntry(const AFontName: string;
  AFontIndex: Integer);
var
  AEncoding: TEncoding;
  AFontCharset: Integer;
  S: string;
begin
  RtfBuilder.OpenGroup;
  RtfBuilder.WriteCommand(TdxRtfExportSR.FontNumber, AFontIndex);

  AEncoding := CalculateFontNameEncoding(AFontName, AFontIndex);
  if AEncoding <> nil then
  begin
    AFontCharset := TdxEncoding.CharsetFromCodePage(AEncoding.CodePage);
    if AFontCharset = 0 then
      if not RtfExportHelper.FontCharsetTable.TryGetValue(AFontIndex, AFontCharset) then
        AFontCharset := 0;
    if AFontCharset > 1 then
      RtfBuilder.WriteCommand(TdxRtfExportSR.FontCharset, AFontCharset);
  end
  else
    AEncoding := FContentExporter.Options.ActualEncoding;
  S := EscapeSpecialCharacters(AFontName);
  if {(AEncoding = ContentExporter.Options.ActualEncoding) or }TdxEncoding.ASCII.CanBeLosslesslyEncoded(S) then
    RtfBuilder.WriteTextDirect(S)
  else
    RtfBuilder.WriteTextDirect(EscapeFontName(S, AEncoding));
  RtfBuilder.WriteTextDirect(';');
  RtfBuilder.CloseGroup;
end;

procedure TdxRtfExporter.ExportListOverrideTable;
var
  AOverrides: TdxStringList;
  AValue: string;
begin
  RtfBuilder.OpenGroup;
  RtfBuilder.WriteCommand(TdxRtfExportSR.ListOverrideTable);

  AOverrides := RtfExportHelper.ListOverrideCollection;
  for AValue in AOverrides do
    RtfBuilder.WriteTextDirect(AValue);
  RtfBuilder.CloseGroup;
end;

procedure TdxRtfExporter.ExportListTable;
var
  AListCollection: TDictionary<Integer, string>;
  AValue: string;
begin
  AListCollection := RtfExportHelper.ListCollection;
  if AListCollection.Count = 0 then
    Exit;
  RtfBuilder.OpenGroup;
  RtfBuilder.WriteCommand(TdxRtfExportSR.NumberingListTable);
  for AValue in AListCollection.Values do
    RtfBuilder.WriteTextDirect(AValue);
  RtfBuilder.CloseGroup;
end;

procedure TdxRtfExporter.ExportParagraphGroupProperties;
begin
end;

function TdxRtfExporter.ExportSaveMemory: TdxChunkedStringBuilder;
begin
  ExportCore;
  Result := RtfBuilder.ExtractRtfContent;
end;

procedure TdxRtfExporter.ExportStyleTable;
var
  AStyles: TdxStringList;
  AValue: string;
begin
  RtfBuilder.OpenGroup;
  RtfBuilder.WriteCommand(TdxRtfExportSR.StyleTable);

  AStyles := RtfExportHelper.StylesCollection;
  for AValue in AStyles do
    RtfBuilder.WriteTextDirect(AValue);
  RtfBuilder.CloseGroup;
end;

procedure TdxRtfExporter.ExportUsersTable;
var
  AUsers: TdxStringList;
  AValue: string;
begin
  AUsers := RtfExportHelper.UserCollection;
  if AUsers.Count = 0 then
    Exit;

  RtfBuilder.OpenGroup;
  RtfBuilder.WriteCommand(TdxRtfExportSR.UserTable);
  for AValue in AUsers do
  begin
    RtfBuilder.OpenGroup;
    RtfBuilder.WriteText(AValue);
    RtfBuilder.CloseGroup;
  end;
  RtfBuilder.CloseGroup;
end;

function TdxRtfExporter.EscapeSpecialCharacters(const AFontName: string): string;
var
  AContainsSpecialSymbol: Boolean;
  ALength, I: Integer;
  ACh: Char;
begin
  AContainsSpecialSymbol := False;

  ALength := Length(AFontName);
  for I := 1 to ALength do
  begin
    ACh := AFontName[I];
    if (((ACh <> ';') and (ACh <> '\')) and (ACh <> '{')) and (ACh <> '}') then
    begin
      if AContainsSpecialSymbol then
        FFontNameStringBuilder.Append(ACh);
      Continue;
    end;
    if not AContainsSpecialSymbol then
    begin
      FFontNameStringBuilder.Clear;
      if I > 0 then
        FFontNameStringBuilder.Append(TdxStringHelper.Substring(AFontName, 0, I - 1));
      AContainsSpecialSymbol := True;
    end;
    FFontNameStringBuilder.Append('\'#$27);
    FFontNameStringBuilder.Append(TdxStringHelper.ToHex(Byte(Ord(ACh))));
  end;
  if AContainsSpecialSymbol then
    Result := FFontNameStringBuilder.ToString
  else
    Result := AFontName;
end;

function TdxRtfExporter.GetKeepFieldCodeViewState: Boolean;
begin
  Result := ContentExporter.KeepFieldCodeViewState;
end;

function TdxRtfExporter.GetLastParagraphRunNotSelected: Boolean;
begin
  Result := ContentExporter.LastParagraphRunNotSelected;
end;

procedure TdxRtfExporter.SetKeepFieldCodeViewState(const Value: Boolean);
begin
  ContentExporter.KeepFieldCodeViewState := Value;
end;

procedure TdxRtfExporter.SetLastParagraphRunNotSelected(const Value: Boolean);
begin
  ContentExporter.LastParagraphRunNotSelected := Value;
end;

function TdxRtfExporter.GetDocumentModel: TdxDocumentModel;
begin
  Result := TdxDocumentModel(inherited DocumentModel);
end;

{ TdxRtfContentExporter }

function TdxRtfContentExporter.CalculateFirstExportedNumberingListsIndex(
  ANumberingLists: TdxNumberingListCollection): Integer;
var
  AFirstIndex: Integer;
  ALastIndex: Integer;
begin
  AFirstIndex := 0;
  ALastIndex := ANumberingLists.Count - 1;
  while AFirstIndex <= ALastIndex do
  begin
    if not ANumberingLists[AFirstIndex].CanRemove then
      Break;
    Inc(AFirstIndex);
  end;
  Result := AFirstIndex;
end;

function TdxRtfContentExporter.CalculateFirstExportedNumberingListsIndexForNumberingListStyles(
  ANumberingListStyleCollection: TdxNumberingListStyleCollection): TdxNumberingListIndex;
var
  AStyle: TdxNumberingListStyle;
  AStyleListIndex: TdxNumberingListIndex;
  I: Integer;
begin
  Result := NumberingListIndexMaxValue;
  for I := 0 to ANumberingListStyleCollection.Count - 1 do
  begin
    AStyle := ANumberingListStyleCollection[I];
    AStyleListIndex := AStyle.NumberingListIndex;
    if AStyleListIndex >= NumberingListIndexMinValue then
      Result := Min(Result, AStyleListIndex);
  end;
end;

function TdxRtfContentExporter.CalculateFirstExportedNumberingListsIndexForStyles(
  AParagraphStyleCollection: TdxParagraphStyleCollection): Integer;
var
  I: Integer;
  AStyle: TdxParagraphStyle;
  AStyleListIndex: Integer;
begin
  Result := MaxInt;
  for I := 0 to AParagraphStyleCollection.Count - 1 do
  begin
    AStyle := AParagraphStyleCollection[I];
    AStyleListIndex := AStyle.GetOwnNumberingListIndex;
    if AStyleListIndex >= 0 then
      Result := Min(Result, AStyleListIndex);
  end;
end;

class function TdxRtfContentExporter.ConditionalStylesTypesTryGetValue(AKey: TdxConditionalTableStyleFormattingType;
  var AResult: string): Boolean;
begin
{$IFDEF DELPHIXE2}
  if AKey in [TdxConditionalTableStyleFormattingType.BottomLeftCell..TdxConditionalTableStyleFormattingType.FirstRow] then
    AResult := ConditionalStylesTypes[AKey]
{$ELSE}
  if Ord(AKey) in [Ord(TdxConditionalTableStyleFormattingType.BottomLeftCell)..Ord(TdxConditionalTableStyleFormattingType.FirstRow)] then
    AResult := ConditionalStylesTypes[Ord(AKey)]
{$ENDIF}
  else
    AResult := '';
  Result := AResult <> '';
end;

constructor TdxRtfContentExporter.Create(ADocumentModel: TdxDocumentModel;
  const AOptions: IdxExporterOptions;
  const ARtfExportHelper: IdxRtfExportHelper);
begin
  inherited Create(ADocumentModel, AOptions);
  FRtfExportHelper := ARtfExportHelper;
  FOptions := TdxRtfDocumentExporterOptions(Options);
  FRtfBuilder := CreateRtfBuilder;
  FParagraphPropertiesExporter := CreateParagraphPropertiesExporter;
  FCharacterPropertiesExporter := TdxRtfCharacterPropertiesExporter.Create(ADocumentModel, ARtfExportHelper, RtfBuilder, FOptions);
  FGetMergedCharacterPropertiesCachedResult := TdxRunMergedCharacterPropertiesCachedResult.Create;
end;

class function TdxRtfContentExporter.CreateChapterSeparatorTypesTable: TdxCharStringDictionary;
begin
  Result := TdxCharStringDictionary.Create;
  Result.Add(TdxCharacters.Hyphen, TdxRtfExportSR.SectionChapterSeparatorHyphen);
  Result.Add('.', TdxRtfExportSR.SectionChapterSeparatorPeriod);
  Result.Add(':', TdxRtfExportSR.SectionChapterSeparatorColon);
  Result.Add(TdxCharacters.EmDash, TdxRtfExportSR.SectionChapterSeparatorEmDash);
  Result.Add(TdxCharacters.EnDash, TdxRtfExportSR.SectionChapterSeparatorEnDash);
end;

class function TdxRtfContentExporter.CreateEndNoteNumberingTypesTable: TdxEnumeratedDictionary<TdxNumberingFormat, string>;
begin
  Result := TdxEnumeratedDictionary<TdxNumberingFormat, string>.Create;
  Result.Add(TdxNumberingFormat.Decimal, TdxRtfExportSR.EndNoteNumberingFormatDecimal);
  Result.Add(TdxNumberingFormat.UpperRoman, TdxRtfExportSR.EndNoteNumberingFormatUpperRoman);
  Result.Add(TdxNumberingFormat.LowerRoman, TdxRtfExportSR.EndNoteNumberingFormatLowerRoman);
  Result.Add(TdxNumberingFormat.UpperLetter, TdxRtfExportSR.EndNoteNumberingFormatUpperLetter);
  Result.Add(TdxNumberingFormat.LowerLetter, TdxRtfExportSR.EndNoteNumberingFormatLowerLetter);
  Result.Add(TdxNumberingFormat.Chicago, TdxRtfExportSR.EndNoteNumberingFormatChicago);
  Result.Add(TdxNumberingFormat.Chosung, TdxRtfExportSR.EndNoteNumberingFormatChosung);
  Result.Add(TdxNumberingFormat.DecimalEnclosedCircle, TdxRtfExportSR.EndNoteNumberingFormatDecimalEnclosedCircle);
  Result.Add(TdxNumberingFormat.DecimalFullWidth, TdxRtfExportSR.EndNoteNumberingFormatDecimalFullWidth);
  Result.Add(TdxNumberingFormat.Ganada, TdxRtfExportSR.EndNoteNumberingFormatGanada);
end;

class function TdxRtfContentExporter.CreateFootNoteNumberingTypesTable: TdxEnumeratedDictionary<TdxNumberingFormat, string>;
begin
  Result := TdxEnumeratedDictionary<TdxNumberingFormat, string>.Create;
  Result.Add(TdxNumberingFormat.Decimal, TdxRtfExportSR.FootNoteNumberingFormatDecimal);
  Result.Add(TdxNumberingFormat.UpperRoman, TdxRtfExportSR.FootNoteNumberingFormatUpperRoman);
  Result.Add(TdxNumberingFormat.LowerRoman, TdxRtfExportSR.FootNoteNumberingFormatLowerRoman);
  Result.Add(TdxNumberingFormat.UpperLetter, TdxRtfExportSR.FootNoteNumberingFormatUpperLetter);
  Result.Add(TdxNumberingFormat.LowerLetter, TdxRtfExportSR.FootNoteNumberingFormatLowerLetter);
  Result.Add(TdxNumberingFormat.Chicago, TdxRtfExportSR.FootNoteNumberingFormatChicago);
  Result.Add(TdxNumberingFormat.Chosung, TdxRtfExportSR.FootNoteNumberingFormatChosung);
  Result.Add(TdxNumberingFormat.DecimalEnclosedCircle, TdxRtfExportSR.FootNoteNumberingFormatDecimalEnclosedCircle);
  Result.Add(TdxNumberingFormat.DecimalFullWidth, TdxRtfExportSR.FootNoteNumberingFormatDecimalFullWidth);
  Result.Add(TdxNumberingFormat.Ganada, TdxRtfExportSR.FootNoteNumberingFormatGanada);
end;

destructor TdxRtfContentExporter.Destroy;
begin
  FRtfExportHelper := nil;
  FreeAndNil(FGetMergedCharacterPropertiesCachedResult);
  FreeAndNil(FCharacterPropertiesExporter);
  FreeAndNil(FParagraphPropertiesExporter);
  FreeAndNil(FRtfBuilder);
  inherited Destroy;
end;

procedure TdxRtfContentExporter.EnsureValidFirstColor(AColor: TdxAlphaColor);
begin
//do nothing
end;

procedure TdxRtfContentExporter.Export;
begin
  ExportDefaultCharacterProperties;
  ExportDefaultParagraphProperties;
  ExportStyleTable;
  if FOptions.ListExportFormat = TdxRtfNumberingListExportFormat.RtfFormat then
    ExportNumberingListTable;
  PopulateUserTable;
  FRtfBuilder.Clear;
  ExportDocumentProperties;
  if FOptions.WrapContentInGroup then
    FRtfBuilder.OpenGroup;
  inherited Export;
  if FOptions.WrapContentInGroup then
    FRtfBuilder.CloseGroup;
end;

procedure TdxRtfContentExporter.ExportFirstPageHeader(ASectionHeader: TdxSectionHeader; ALinkedToPrevious: Boolean);
begin
  if ALinkedToPrevious then
    Exit;
  if not DocumentModel.DocumentCapabilities.HeadersFootersAllowed then
    Exit;
  RtfBuilder.OpenGroup;
  try
    RtfBuilder.WriteCommand(TdxRtfExportSR.SectionFirstPageHeader);
    inherited ExportFirstPageHeader(ASectionHeader, ALinkedToPrevious);
  finally
    RtfBuilder.CloseGroup;
  end;
end;

procedure TdxRtfContentExporter.ExportOddPageHeader(ASectionHeader: TdxSectionHeader; ALinkedToPrevious: Boolean);
begin
  if ALinkedToPrevious then
    Exit;
  if not DocumentModel.DocumentCapabilities.HeadersFootersAllowed then
    Exit;
  RtfBuilder.OpenGroup;
  try
    RtfBuilder.WriteCommand(TdxRtfExportSR.SectionOddPageHeader);
    inherited ExportOddPageHeader(ASectionHeader, ALinkedToPrevious);
  finally
    RtfBuilder.CloseGroup;
  end;
end;

procedure TdxRtfContentExporter.ExportEvenPageHeader(ASectionHeader: TdxSectionHeader; ALinkedToPrevious: Boolean);
begin
  if ALinkedToPrevious then
    Exit;
  if not DocumentModel.DocumentCapabilities.HeadersFootersAllowed then
    Exit;
  RtfBuilder.OpenGroup;
  try
    RtfBuilder.WriteCommand(TdxRtfExportSR.SectionEvenPageHeader);
    inherited ExportEvenPageHeader(ASectionHeader, ALinkedToPrevious);
  finally
    RtfBuilder.CloseGroup;
  end;
end;

procedure TdxRtfContentExporter.ExportFirstPageFooter(ASectionFooter: TdxSectionFooter; ALinkedToPrevious: Boolean);
begin
  if ALinkedToPrevious then
    Exit;
  if not DocumentModel.DocumentCapabilities.HeadersFootersAllowed then
    Exit;
  RtfBuilder.OpenGroup;
  try
    RtfBuilder.WriteCommand(TdxRtfExportSR.SectionFirstPageFooter);
    inherited ExportFirstPageFooter(ASectionFooter, ALinkedToPrevious);
  finally
    RtfBuilder.CloseGroup;
  end;
end;

procedure TdxRtfContentExporter.ExportOddPageFooter(ASectionFooter: TdxSectionFooter; ALinkedToPrevious: Boolean);
begin
  if ALinkedToPrevious then
    Exit;
  if not DocumentModel.DocumentCapabilities.HeadersFootersAllowed then
    Exit;
  RtfBuilder.OpenGroup;
  try
    RtfBuilder.WriteCommand(TdxRtfExportSR.SectionOddPageFooter);
    inherited ExportOddPageFooter(ASectionFooter, ALinkedToPrevious);
  finally
    RtfBuilder.CloseGroup;
  end;
end;

procedure TdxRtfContentExporter.ExportEvenPageFooter(ASectionFooter: TdxSectionFooter; ALinkedToPrevious: Boolean);
begin
  if ALinkedToPrevious then
    Exit;
  if not DocumentModel.DocumentCapabilities.HeadersFootersAllowed then
    Exit;
  RtfBuilder.OpenGroup;
  try
    RtfBuilder.WriteCommand(TdxRtfExportSR.SectionEvenPageFooter);
    inherited ExportEvenPageFooter(ASectionFooter, ALinkedToPrevious);
  finally
    RtfBuilder.CloseGroup;
  end;
end;

procedure TdxRtfContentExporter.ExportDefaultCharacterProperties;
var
  ACharacterPropertiesExporter: TdxRtfCharacterPropertiesExporter;
  ACharacterFormatting: TdxCharacterFormattingBase;
  AMergedCharacterProperties: TdxMergedCharacterProperties;
begin
  if RtfExportHelper.DefaultCharacterProperties <> '' then
    Exit;
  RtfBuilder.Clear;
  ACharacterPropertiesExporter := TdxRtfCharacterPropertiesExporter.Create(DocumentModel,
    RtfExportHelper, RtfBuilder, FOptions);
  try
    ACharacterFormatting := DocumentModel.DefaultCharacterProperties.Info;
    if not TdxAlphaColors.IsTransparentOrEmpty(ACharacterFormatting.ForeColor) and (ACharacterFormatting.ForeColor <> TdxAlphaColors.Black) then
      RtfExportHelper.GetColorIndex(TdxAlphaColors.Black);

    EnsureValidFirstColor(ACharacterFormatting.ForeColor);
    AMergedCharacterProperties :=  TdxMergedCharacterProperties.Create(ACharacterFormatting.Info, ACharacterFormatting.Options);
    try
      ACharacterPropertiesExporter.ExportCharacterProperties(AMergedCharacterProperties, True, False, False);
      RtfExportHelper.DefaultCharacterProperties := RtfBuilder.RtfContent.ToString;
    finally
      AMergedCharacterProperties.Free;
    end;
  finally
    ACharacterPropertiesExporter.Free;
  end;
end;

procedure TdxRtfContentExporter.ExportDefaultParagraphProperties;
var
  AParagraphPropertiesExporter: TdxRtfParagraphPropertiesExporter;
  AParagraphFormatting: TdxParagraphFormattingBase;
  AMergedParagraphProperties: TdxMergedParagraphProperties;
begin
  if RtfExportHelper.DefaultParagraphProperties <> '' then
    Exit;
  RtfBuilder.Clear;
  AParagraphPropertiesExporter := TdxRtfParagraphPropertiesExporter.Create(DocumentModel, RtfExportHelper, RtfBuilder);
  try
    AParagraphFormatting := DocumentModel.DefaultParagraphProperties.Info;
    AMergedParagraphProperties := TdxMergedParagraphProperties.Create(AParagraphFormatting.Info,
      AParagraphFormatting.Options);
    try
      AParagraphPropertiesExporter.ExportParagraphPropertiesCore(AMergedParagraphProperties, True);
      RtfExportHelper.DefaultParagraphProperties := RtfBuilder.RtfContent.ToString;
    finally
      AMergedParagraphProperties.Free;
    end;
  finally
    AParagraphPropertiesExporter.Free;
  end;
end;

procedure TdxRtfContentExporter.ExportDocumentProperties;
var
  ADefaultDocumentInfo: TdxDocumentInfo;
  ADocumentProperties: TdxDocumentProperties;
  AValue: string;
begin
  ExportDocumentInformation;

  ADefaultDocumentInfo := DocumentModel.Cache.DocumentInfoCache.DefaultItem;
  ADocumentProperties := DocumentModel.DocumentProperties;
  ExportFormattingFlags;

  if ADocumentProperties.DefaultTabWidth <> ADefaultDocumentInfo.DefaultTabWidth then
    RtfBuilder.WriteCommand(TdxRtfExportSR.DefaultTabWidth, UnitConverter.ModelUnitsToTwips(ADocumentProperties.DefaultTabWidth));
  if ADocumentProperties.HyphenateDocument <> TdxDocumentInfo.HyphenateDocumentDefaultValue then
  begin
    if ADocumentProperties.HyphenateDocument then
      AValue := '1'
    else
      AValue := '0';
    RtfBuilder.WriteCommand(TdxRtfExportSR.HyphenateDocument,  AValue);
  end;
  if ADocumentProperties.DifferentOddAndEvenPages <> ADefaultDocumentInfo.DifferentOddAndEvenPages then
    RtfBuilder.WriteCommand(TdxRtfExportSR.PageFacing);
  if ADocumentProperties.DisplayBackgroundShape then
    RtfBuilder.WriteCommand(TdxRtfExportSR.DisplayBackgroundShape, '1');
  if not TdxAlphaColors.IsTransparentOrEmpty(ADocumentProperties.PageBackColor) then
    ExportPageBackground(ADocumentProperties);

  ExportDocumentProtectionProperties;
  ExportDocumentLevelFootNotePresenceProperties;
  ExportDocumentLevelFootNoteProperties;
  ExportDocumentLevelEndNoteProperties;
end;

procedure TdxRtfContentExporter.ExportFieldCodeEndRun(ARun: TdxFieldCodeEndRun);
begin
  WriteFieldInstructionEnd;
  WriteFieldResultStart;
end;

procedure TdxRtfContentExporter.ExportFieldCodeStartRun(
  ARun: TdxFieldCodeStartRun);
begin
  WriteFieldInstructionStart(ARun);
end;

procedure TdxRtfContentExporter.ExportFieldResultEndRun(
  ARun: TdxFieldResultEndRun);
begin
  WriteFieldResultEnd;
end;

procedure TdxRtfContentExporter.ExportFormattingFlags;
begin
  RtfBuilder.WriteCommand(TdxRtfExportSR.NoUICompatible);
  RtfBuilder.WriteCommand(TdxRtfExportSR.ShapeDoNotLay);
  RtfBuilder.WriteCommand(TdxRtfExportSR.HtmlAutoSpacing);
end;

procedure TdxRtfContentExporter.ExportInlinePictureRun(
  ARun: TdxInlinePictureRun);
var
  ACharacterProperties: TdxMergedCharacterProperties;
  AShouldExportCharacterProperties: Boolean;
  AExportStrategy: TdxRtfInlinePictureExportStrategy;
begin
  ACharacterProperties := ARun.GetMergedCharacterProperties(FGetMergedCharacterPropertiesCachedResult);
  try
    AShouldExportCharacterProperties := TdxInlinePictureRun.ShouldExportInlinePictureRunCharacterProperties(ACharacterProperties.Info, ACharacterProperties.Options);
    if AShouldExportCharacterProperties then
    begin
      RtfBuilder.OpenGroup;
      FCharacterPropertiesExporter.ExportCharacterProperties(ACharacterProperties);
    end;
  finally
  end;
  AExportStrategy := TdxRtfInlinePictureExportStrategy.Create;
  try
    AExportStrategy.Export(FRtfBuilder, ARun, FOptions.Compatibility.DuplicateObjectAsMetafile);
  finally
    AExportStrategy.Free;
  end;
  if AShouldExportCharacterProperties then
    RtfBuilder.CloseGroup;
end;

procedure TdxRtfContentExporter.ExportNumberingListTable;
var
  AListExporter: TdxRtfNumberingListExporter;
  ANumberingLists: TdxNumberingListCollection;
  AStartIndex: Integer;
  ACount: Integer;
begin
  AListExporter := CreateNumberingListExporter(Self);
  try
    ANumberingLists := DocumentModel.NumberingLists;
    AStartIndex := CalculateFirstExportedNumberingListsIndex(ANumberingLists);
    AStartIndex := Min(AStartIndex,
      CalculateFirstExportedNumberingListsIndexForStyles(DocumentModel.ParagraphStyles));
    AStartIndex := Min(AStartIndex, CalculateFirstExportedNumberingListsIndexForNumberingListStyles(DocumentModel.NumberingListStyles));
    ACount := ANumberingLists.Count - AStartIndex;
    AListExporter.Export(ANumberingLists, AStartIndex, ACount);
  finally
    AListExporter.Free;
  end;
end;

function TdxRtfContentExporter.ExportParagraph(
  AParagraph: TdxParagraph): TdxParagraphIndex;
var
  ATablesAllowed: Boolean;
  AParagraphCell: TdxTableCell;
begin
  ATablesAllowed := DocumentModel.DocumentCapabilities.TablesAllowed;
  if ATablesAllowed then
    AParagraphCell := AParagraph.GetCell
  else
    AParagraphCell := nil;

  if AParagraphCell = nil then
  begin
    ExportSingleParagraph(AParagraph);
    Result := AParagraph.Index;
  end
  else
    Result := ExportRtfTable(AParagraphCell.Row.Table);
end;

procedure TdxRtfContentExporter.ExportParagraphCharacterProperties(
  AParagraph: TdxParagraph);
var
  ARun: TdxTextRunBase;
  AProperties: TdxMergedCharacterProperties;
begin
  ARun := PieceTable.Runs[AParagraph.LastRunIndex];
  AProperties := ARun.GetMergedCharacterProperties(FGetMergedCharacterPropertiesCachedResult);
  try
    FCharacterPropertiesExporter.ExportParagraphCharacterProperties(AProperties);
  finally
  end;
end;

procedure TdxRtfContentExporter.ExportParagraphCore(AParagraph: TdxParagraph;
  ATableNestingLevel: Integer;
  ACondTypes: TdxConditionalTableStyleFormattingTypes;
  ATableStyleIndex: Integer);
begin
  StartNewParagraph(AParagraph, ATableNestingLevel);
  ExportParagraphTableStyleProperties(ACondTypes, ATableStyleIndex);
  ExportParagraphRuns(AParagraph);
  ExportParagraphCharacterProperties(AParagraph);
end;

procedure TdxRtfContentExporter.ExportParagraphTableStyleProperties(
  ACondTypes: TdxConditionalTableStyleFormattingTypes;
  ATableStyleIndex: Integer);
var
  ACondType: TdxConditionalTableStyleFormattingType;
  AKeyWord: string;
begin
  if ATableStyleIndex < 0 then
    Exit;

  for ACondType := Low(TdxConditionalTableStyleFormattingType) to High(TdxConditionalTableStyleFormattingType) do
  begin
    if (ACondType in ACondTypes) and ConditionalStylesTypesTryGetValue(ACondType, AKeyword) then
      RtfBuilder.WriteCommand(AKeyWord);
  end;
  RtfBuilder.WriteCommand(TdxRtfExportSR.TableStyleCellIndex, ATableStyleIndex);
end;

function TdxRtfContentExporter.ExportRtfTable(
  ATable: TdxTable): TdxParagraphIndex;
var
  AExporter: TdxRtfTableExporter;
begin
  AExporter := TdxRtfTableExporter.Create(Self);
  try
    Result := AExporter.Export(ATable);
  finally
    AExporter.Free;
  end;
end;

procedure TdxRtfContentExporter.ExportSection(const ASection: TdxSection);
begin
  StartNewSection(ASection);
  inherited ExportSection(ASection);
end;

procedure TdxRtfContentExporter.ExportSectionColumn(AColumn: TdxColumnInfo;
  AColumnIndex: Integer);
begin
  RtfBuilder.WriteCommand(TdxRtfExportSR.SectionColumnNumber, AColumnIndex + 1);
  RtfBuilder.WriteCommand(TdxRtfExportSR.SectionColumnWidth, UnitConverter.ModelUnitsToTwips(AColumn.Width));
  RtfBuilder.WriteCommand(TdxRtfExportSR.SectionColumnSpace, UnitConverter.ModelUnitsToTwips(AColumn.Space));
end;

procedure TdxRtfContentExporter.ExportLegacyPageProperties(ASection: TdxSection; ASectionCount: Integer);
var
  ADefaultPage: TdxPageInfo;
  ADefaultMargins: TdxMarginsInfo;
begin
  ADefaultPage := DocumentModel.Cache.PageInfoCache.DefaultItem;
  ADefaultMargins := DocumentModel.Cache.MarginsInfoCache.DefaultItem;

  if ASection.Page.Width <> ADefaultPage.Width then
    FRtfBuilder.WriteCommand(TdxRtfExportSR.LegacyPaperWidth, UnitConverter.ModelUnitsToTwips(ASection.Page.Width));
  if ASection.Page.Height <> ADefaultPage.Height then
    FRtfBuilder.WriteCommand(TdxRtfExportSR.LegacyPaperHeight, UnitConverter.ModelUnitsToTwips(ASection.Page.Height));
  if (ASection.Page.PaperKind <> ADefaultPage.PaperKind) and (ASection.Page.PaperKind <> TdxPaperKind.Custom) then
    FRtfBuilder.WriteCommand(TdxRtfExportSR.PaperKind, Integer(ASection.Page.PaperKind));
  if (ASectionCount <= 1) and ASection.Page.Landscape then
    FRtfBuilder.WriteCommand(TdxRtfExportSR.LegacyLandscape);

  if not ASection.PageNumbering.ContinueNumbering then
    FRtfBuilder.WriteCommand(TdxRtfExportSR.LegacyPageNumberingStart, ASection.PageNumbering.FirstPageNumber);

  if ASection.Margins.Left <> ADefaultMargins.Left then
    FRtfBuilder.WriteCommand(TdxRtfExportSR.LegacyMarginsLeft, UnitConverter.ModelUnitsToTwips(ASection.Margins.Left));
  if ASection.Margins.Right <> ADefaultMargins.Right then
    FRtfBuilder.WriteCommand(TdxRtfExportSR.LegacyMarginsRight, UnitConverter.ModelUnitsToTwips(ASection.Margins.Right));
  if ASection.Margins.Top <> ADefaultMargins.Top then
    FRtfBuilder.WriteCommand(TdxRtfExportSR.LegacyMarginsTop, UnitConverter.ModelUnitsToTwips(ASection.Margins.Top));
  if ASection.Margins.Bottom <> ADefaultMargins.Bottom then
    FRtfBuilder.WriteCommand(TdxRtfExportSR.LegacyMarginsBottom, UnitConverter.ModelUnitsToTwips(ASection.Margins.Bottom));
  if ASection.Margins.Gutter <> ADefaultMargins.Gutter then
    FRtfBuilder.WriteCommand(TdxRtfExportSR.LegacyMarginsGutter, UnitConverter.ModelUnitsToTwips(ASection.Margins.Gutter));
  if ASection.Margins.GutterAlignment = TdxSectionGutterAlignment.Right then
    FRtfBuilder.WriteCommand(TdxRtfExportSR.LegacyMarginsGutterAtRight);
end;

procedure TdxRtfContentExporter.ExportSectionColumns(
  AColumns: TdxSectionColumns);
var
  ADefaultColumns: TdxColumnsInfo;
  AColumnsForExport: TdxColumnInfoCollection;
begin
  ADefaultColumns := PieceTable.DocumentModel.Cache.ColumnsInfoCache.DefaultItem;
  if AColumns.EqualWidthColumns then
  begin
    if AColumns.ColumnCount <> ADefaultColumns.ColumnCount then
      RtfBuilder.WriteCommand(TdxRtfExportSR.SectionColumnsCount, AColumns.ColumnCount);
    if AColumns.Space <> ADefaultColumns.Space then
      RtfBuilder.WriteCommand(TdxRtfExportSR.SectionSpaceBetweenColumns, UnitConverter.ModelUnitsToTwips(AColumns.Space));
  end
  else
  begin
    AColumnsForExport := AColumns.GetColumns;
    try
      ExportSectionColumnsDetails(AColumnsForExport);
    finally
      AColumnsForExport.Free;
    end;
  end;

  if AColumns.DrawVerticalSeparator then
    RtfBuilder.WriteCommand(TdxRtfExportSR.SectionColumnsDrawVerticalSeparator);
end;

procedure TdxRtfContentExporter.ExportSectionColumnsDetails(
  AColumns: TdxColumnInfoCollection);
var
  ACount: Integer;
  I: Integer;
begin
  ACount := AColumns.Count;
  if ACount > 0 then
    RtfBuilder.WriteCommand(TdxRtfExportSR.SectionColumnsCount, ACount);
  for I := 0 to ACount - 1 do
    ExportSectionColumn(AColumns[I], I);
end;

procedure TdxRtfContentExporter.ExportSectionEndNote(ANote: TdxSectionFootNote);
var
  ADefaultInfo: TdxFootNoteInfo;
begin
  if not DocumentModel.DocumentCapabilities.EndNotesAllowed then
    Exit;

  ADefaultInfo := DocumentModel.Cache.FootNoteInfoCache[TdxFootNoteInfoCache.DefaultEndNoteItemIndex];

  if ANote.StartingNumber <> ADefaultInfo.StartingNumber then
    FRtfBuilder.WriteCommand(TdxRtfExportSR.SectionEndNoteNumberingStart, ANote.StartingNumber);

  if ANote.NumberingFormat <> ADefaultInfo.NumberingFormat then
    WriteEnumValueCommand<TdxNumberingFormat>(FSectionEndNoteNumberingTypes, ANote.NumberingFormat, TdxRtfExportSR.SectionEndNoteNumberingFormatLowerRoman);

  if ANote.NumberingRestartType <> ADefaultInfo.NumberingRestartType then
  begin
    if ANote.NumberingRestartType = TdxLineNumberingRestart.Continuous then
      RtfBuilder.WriteCommand(TdxRtfExportSR.SectionEndNoteNumberingRestartContinuous)
    else
      RtfBuilder.WriteCommand(TdxRtfExportSR.SectionEndNoteNumberingRestartEachSection);
  end;
end;

procedure TdxRtfContentExporter.ExportSectionFootNote(
  ANote: TdxSectionFootNote);
var
  ADefaultInfo: TdxFootNoteInfo;
begin
  if not DocumentModel.DocumentCapabilities.FootNotesAllowed then
    Exit;

  ADefaultInfo := DocumentModel.Cache.FootNoteInfoCache[TdxFootNoteInfoCache.DefaultFootNoteItemIndex];
  if ANote.Position <> ADefaultInfo.Position then
  begin
    if ANote.Position = TdxFootNotePosition.BelowText then
      RtfBuilder.WriteCommand(TdxRtfExportSR.SectionFootNotePlacementBelowText)
    else
      RtfBuilder.WriteCommand(TdxRtfExportSR.SectionFootNotePlacementPageBottom);
  end;

  if ANote.StartingNumber <> ADefaultInfo.StartingNumber then
    FRtfBuilder.WriteCommand(TdxRtfExportSR.SectionFootNoteNumberingStart, ANote.StartingNumber);

  if ANote.NumberingFormat <> ADefaultInfo.NumberingFormat then
    WriteEnumValueCommand<TdxNumberingFormat>(FSectionFootNoteNumberingTypes, ANote.NumberingFormat, TdxRtfExportSR.SectionFootNoteNumberingFormatDecimal);

  if ANote.NumberingRestartType <> ADefaultInfo.NumberingRestartType then
  begin
    if ANote.NumberingRestartType = TdxLineNumberingRestart.Continuous then
      RtfBuilder.WriteCommand(TdxRtfExportSR.SectionFootNoteNumberingRestartContinuous)
    else
      if ANote.NumberingRestartType = TdxLineNumberingRestart.NewSection then
        RtfBuilder.WriteCommand(TdxRtfExportSR.SectionFootNoteNumberingRestartEachSection)
      else
        RtfBuilder.WriteCommand(TdxRtfExportSR.SectionFootNoteNumberingRestartEachPage);
  end;
end;

procedure TdxRtfContentExporter.ExportSectionGeneralSettings(
  ASettings: TdxSectionGeneralSettings);
var
  ADefaultSettings: TdxGeneralSectionInfo;
begin
  ADefaultSettings := PieceTable.DocumentModel.Cache.GeneralSectionInfoCache.DefaultItem;
  if ASettings.StartType <> ADefaultSettings.StartType then
    WriteSectionBreakType(ASettings.StartType);
  if ASettings.FirstPagePaperSource <> ADefaultSettings.FirstPagePaperSource then
    RtfBuilder.WriteCommand(TdxRtfExportSR.SectionFirstPagePaperSource, ASettings.FirstPagePaperSource);
  if ASettings.OtherPagePaperSource <> ADefaultSettings.OtherPagePaperSource then
    RtfBuilder.WriteCommand(TdxRtfExportSR.SectionOtherPagePaperSource, ASettings.OtherPagePaperSource);
  if ASettings.OnlyAllowEditingOfFormFields then
    RtfBuilder.WriteCommand(TdxRtfExportSR.SectionOnlyAllowEditingOfFormFields);
  if ASettings.TextDirection <> ADefaultSettings.TextDirection then
    RtfBuilder.WriteCommand(TdxRtfExportSR.SectionTextFlow, Ord(ASettings.TextDirection));
  if ASettings.VerticalTextAlignment <> ADefaultSettings.VerticalTextAlignment then
    WriteVerticalAlignment(ASettings.VerticalTextAlignment);
  if ASettings.DifferentFirstPage <> ADefaultSettings.DifferentFirstPage then
    RtfBuilder.WriteCommand(TdxRtfExportSR.SectionTitlePage);
end;

procedure TdxRtfContentExporter.ExportSectionLineNumbering(
  ALineNumbering: TdxSectionLineNumbering);
var
  ADefaultLineNumbering: TdxLineNumberingInfo;
begin
  ADefaultLineNumbering := PieceTable.DocumentModel.Cache.LineNumberingInfoCache.DefaultItem;
  if ALineNumbering.Distance <> ADefaultLineNumbering.Distance then
    RtfBuilder.WriteCommand(TdxRtfExportSR.SectionLineNumberingDistance, UnitConverter.ModelUnitsToTwips(ALineNumbering.Distance));
  if ALineNumbering.Step <> ADefaultLineNumbering.Step then
    RtfBuilder.WriteCommand(TdxRtfExportSR.SectionLineNumberingStep, ALineNumbering.Step);

  if ALineNumbering.NumberingRestartType <> TdxLineNumberingRestart.NewPage then
  begin
    RtfBuilder.WriteCommand(TdxRtfExportSR.SectionLineNumberingStartingLineNumber, ALineNumbering.StartingLineNumber);
    if ALineNumbering.NumberingRestartType = TdxLineNumberingRestart.Continuous then
      RtfBuilder.WriteCommand(TdxRtfExportSR.SectionLineNumberingContinuous)
    else
      RtfBuilder.WriteCommand(TdxRtfExportSR.SectionLineNumberingRestartNewSection);
  end;
end;

procedure TdxRtfContentExporter.ExportSectionMargins(
  AMargins: TdxSectionMargins);
var
  ADefaultMargins: TdxMarginsInfo;
begin
  ADefaultMargins := PieceTable.DocumentModel.Cache.MarginsInfoCache.DefaultItem;
  if AMargins.Left <> ADefaultMargins.Left then
    RtfBuilder.WriteCommand(TdxRtfExportSR.SectionMarginsLeft, UnitConverter.ModelUnitsToTwips(AMargins.Left));
  if AMargins.Right <> ADefaultMargins.Right then
    RtfBuilder.WriteCommand(TdxRtfExportSR.SectionMarginsRight, UnitConverter.ModelUnitsToTwips(AMargins.Right));
  if AMargins.Top <> ADefaultMargins.Top then
    RtfBuilder.WriteCommand(TdxRtfExportSR.SectionMarginsTop, UnitConverter.ModelUnitsToTwips(AMargins.Top));
  if AMargins.Bottom <> ADefaultMargins.Bottom then
    RtfBuilder.WriteCommand(TdxRtfExportSR.SectionMarginsBottom, UnitConverter.ModelUnitsToTwips(AMargins.Bottom));
  if AMargins.HeaderOffset <> ADefaultMargins.HeaderOffset then
    RtfBuilder.WriteCommand(TdxRtfExportSR.SectionMarginsHeaderOffset, UnitConverter.ModelUnitsToTwips(AMargins.HeaderOffset));
  if AMargins.FooterOffset <> ADefaultMargins.FooterOffset then
    RtfBuilder.WriteCommand(TdxRtfExportSR.SectionMarginsFooterOffset, UnitConverter.ModelUnitsToTwips(AMargins.FooterOffset));
  if AMargins.Gutter <> ADefaultMargins.Gutter then
    RtfBuilder.WriteCommand(TdxRtfExportSR.SectionMarginsGutter, UnitConverter.ModelUnitsToTwips(AMargins.Gutter));
end;

procedure TdxRtfContentExporter.ExportSectionPage(APage: TdxSectionPage);
var
  ADefaultPage: TdxPageInfo;
begin
  ADefaultPage := PieceTable.DocumentModel.Cache.PageInfoCache.DefaultItem;
  if APage.Width <> ADefaultPage.Width then
    RtfBuilder.WriteCommand(TdxRtfExportSR.SectionPageWidth, UnitConverter.ModelUnitsToTwips(APage.Width));
  if APage.Height <> ADefaultPage.Height then
    RtfBuilder.WriteCommand(TdxRtfExportSR.SectionPageHeight, UnitConverter.ModelUnitsToTwips(APage.Height));
  if APage.Landscape then
    RtfBuilder.WriteCommand(TdxRtfExportSR.SectionPageLandscape);
  if (APage.PaperKind <> ADefaultPage.PaperKind) and (APage.PaperKind <> TdxPaperKind.Custom) then
    RtfBuilder.WriteCommand(TdxRtfExportSR.PaperKind, Ord(APage.PaperKind));
end;

procedure TdxRtfContentExporter.ExportSectionPageNumbering(
  APageNumbering: TdxSectionPageNumbering);
var
  ADefaultPageNumbering: TdxPageNumberingInfo;
begin
  ADefaultPageNumbering := PieceTable.DocumentModel.Cache.PageNumberingInfoCache.DefaultItem;
  if APageNumbering.ChapterSeparator <> ADefaultPageNumbering.ChapterSeparator then
    WriteChapterSeparator(APageNumbering.ChapterSeparator);
  if APageNumbering.ChapterHeaderStyle <> ADefaultPageNumbering.ChapterHeaderStyle then
    RtfBuilder.WriteCommand(TdxRtfExportSR.SectionChapterHeaderStyle, APageNumbering.ChapterHeaderStyle);

  if not APageNumbering.ContinueNumbering then
  begin
    RtfBuilder.WriteCommand(TdxRtfExportSR.SectionPageNumberingStart, APageNumbering.FirstPageNumber);
    RtfBuilder.WriteCommand(TdxRtfExportSR.SectionPageNumberingRestart);
  end;

  if APageNumbering.NumberingFormat <> ADefaultPageNumbering.NumberingFormat then
    WritePageNumberingFormat(APageNumbering.NumberingFormat);
end;

procedure TdxRtfContentExporter.ExportSectionProperties(ASection: TdxSection);
begin
  ExportSectionMargins(ASection.Margins);
  ExportSectionPage(ASection.Page);
  ExportSectionGeneralSettings(ASection.GeneralSettings);
  ExportSectionPageNumbering(ASection.PageNumbering);
  ExportSectionLineNumbering(ASection.LineNumbering);
  ExportSectionColumns(ASection.Columns);
  ExportSectionFootNote(ASection.FootNote);
  ExportSectionEndNote(ASection.EndNote);
end;

procedure TdxRtfContentExporter.ExportSingleParagraph(AParagraph: TdxParagraph);
begin
  ExportParagraphCore(AParagraph, 0, [], -1);
  if not (IsLastParagraph(AParagraph) and SuppressExportLastParagraph(AParagraph)) then
    FinishParagraph(AParagraph);
end;

procedure TdxRtfContentExporter.ExportStyleTable;
var
  ADocumentModel: TdxDocumentModel;
  AStyleExporter: TdxRtfStyleExporter;
begin
  ADocumentModel := PieceTable.DocumentModel;
  AStyleExporter := CreateStyleExporter;
  try
    AStyleExporter.ExportStyleSheet(ADocumentModel.ParagraphStyles, ADocumentModel.CharacterStyles,
      ADocumentModel.TableStyles);
  finally
    AStyleExporter.Free;
  end;
end;

procedure TdxRtfContentExporter.ExportTextRun(ARun: TdxTextRun);
var
  AText: string;
  AProperties: TdxMergedCharacterProperties;
  AKerning: Boolean;
begin
  RtfBuilder.OpenGroup;
  AProperties := ARun.GetMergedCharacterProperties(FGetMergedCharacterPropertiesCachedResult);
  try
    FCharacterPropertiesExporter.ExportCharacterProperties(AProperties);
  finally
  end;
  WriteRunCharacterStyle(ARun);

  if not ARun.IsFieldSymbolRun then
  begin
    AKerning := TdxRtfDocumentExporterCompatibilityOptionsAccess(RtfOptions.Compatibility).Kerning;
    if AKerning then
      RtfBuilder.WriteCommand('\kerning', 1);
    AText := ARun.GetPlainText(PieceTable.TextBuffer);
    WriteText(AText);
    if AKerning then
      RtfBuilder.WriteCommand('\kerning', 0);
  end;
  RtfBuilder.CloseGroup;
end;

class destructor TdxRtfContentExporter.Finalize;
begin
  FreeAndNil(FChapterSeparatorTypes);
  FreeAndNil(FPageNumberingTypes);
  FreeAndNil(FSectionFootNoteNumberingTypes);
  FreeAndNil(FSectionEndNoteNumberingTypes);
  FreeAndNil(FFootNoteNumberingTypes);
  FreeAndNil(FEndNoteNumberingTypes);
  FreeAndNil(FPredefinedUserGroups);
end;

procedure TdxRtfContentExporter.FinishParagraph(AParagraph: TdxParagraph);
begin
  if (PieceTable.Runs[AParagraph.LastRunIndex] is TdxSectionRun) and
      DocumentModel.DocumentCapabilities.SectionsAllowed then
    RtfBuilder.WriteCommand(TdxRtfExportSR.SectionEndMark)
  else
    RtfBuilder.WriteCommand(TdxRtfExportSR.EndOfParagraph);
end;

function TdxRtfContentExporter.GetUnitConverter: TdxDocumentModelUnitConverter;
begin
  Result := DocumentModel.UnitConverter;
end;

class constructor TdxRtfContentExporter.Initialize;
begin
  FChapterSeparatorTypes := CreateChapterSeparatorTypesTable;
  FPageNumberingTypes := CreatePageNumberingTypesTable;
  FSectionFootNoteNumberingTypes := CreateSectionFootNoteNumberingTypesTable;
  FSectionEndNoteNumberingTypes := CreateSectionEndNoteNumberingTypesTable;
  FFootNoteNumberingTypes := CreateFootNoteNumberingTypesTable;
  FEndNoteNumberingTypes := CreateEndNoteNumberingTypesTable;
  FPredefinedUserGroups := CreatePredefinedUserGroups;
end;

function TdxRtfContentExporter.IsLastParagraph(
  AParagraph: TdxParagraph): Boolean;
begin
  Result := PieceTable.Paragraphs.Last = AParagraph;
end;

procedure TdxRtfContentExporter.PopulateUserTable;
var
  AHelper: TdxRtfExportHelper;
  AUsers: TdxStringList;
  APieceTables: TdxFastList;
  I: Integer;
begin
  AHelper := TdxRtfExportHelper(RtfExportHelper);
  if AHelper = nil then
    Exit;
  AUsers := AHelper.UserCollection;
  AUsers.Clear;
  APieceTables := DocumentModel.GetPieceTables(False);
  try
    for I := 0 to APieceTables.Count - 1 do
      PopulateUserList(TdxPieceTable(APieceTables[I]), AUsers);
  finally
    APieceTables.Free;
  end;
end;

procedure TdxRtfContentExporter.PopulateUserList(APieceTable: TdxPieceTable; AUsers: TdxStringList);
var
  ARangePermissions: TdxRangePermissionCollection;
  ACount, I: Integer;
  AUserName: string;
begin
  ARangePermissions := APieceTable.RangePermissions;
  ACount := ARangePermissions.Count;
  for I := 0 to ACount - 1 do
  begin
    AUserName := ARangePermissions[I].UserName;
    if (AUserName <> '') and not AUsers.Contains(AUserName) then
      AUsers.Add(AUserName);
  end;
end;

function TdxRtfContentExporter.ShouldExportHiddenText: Boolean;
begin
  Result := True;
end;

function TdxRtfContentExporter.ShouldUseCustomSaveTableMethod: Boolean;
begin
  Result := True;
end;

function TdxRtfContentExporter.ShouldWriteRunCharacterStyle(
  ARun: TdxTextRunBase): Boolean;
var
  AParagraphStyle: TdxParagraphStyle;
begin
  if (ARun.CharacterStyleIndex = TdxCharacterStyleCollection.EmptyCharacterStyleIndex) or
      not RtfExportHelper.SupportStyle then
    Result := False
  else
  begin
    if ARun.Paragraph.ParagraphStyleIndex = TdxParagraphStyleCollection.EmptyParagraphStyleIndex then
      Result := True
    else
    begin
      AParagraphStyle := ARun.Paragraph.ParagraphStyle;
      Result := not (AParagraphStyle.HasLinkedStyle and (AParagraphStyle.LinkedStyle = ARun.CharacterStyle));
    end;
  end;
end;

procedure TdxRtfContentExporter.StartNewInnerParagraph(AParagraph: TdxParagraph;
  ATableNestingLevel: Integer);
begin
  RtfBuilder.WriteCommand(TdxRtfExportSR.ResetCharacterFormatting);
  FParagraphPropertiesExporter.ExportParagraphProperties(AParagraph, ATableNestingLevel);
end;

procedure TdxRtfContentExporter.StartNewParagraph(AParagraph: TdxParagraph;
  ATableNestingLevel: Integer);
begin
  if AParagraph.IsInList then
    WriteAlternativeAndPlainText(AParagraph);
  RtfBuilder.WriteCommand(TdxRtfExportSR.ResetParagraphProperties);
  StartNewInnerParagraph(AParagraph, ATableNestingLevel);
end;

procedure TdxRtfContentExporter.StartNewSection(ASection: TdxSection);
begin
  if not DocumentModel.DocumentCapabilities.SectionsAllowed then
    Exit;
  RtfBuilder.WriteCommand(TdxRtfExportSR.ResetSectionProperties);
  ExportSectionProperties(ASection);
end;

function TdxRtfContentExporter.SuppressExportLastParagraph(AParagraph: TdxParagraph): Boolean;
begin
  if not AParagraph.PieceTable.IsMain then
    Exit(False);
  Result := (FOptions.ExportFinalParagraphMark = TdxExportFinalParagraphMark.Never) or
    ((FOptions.ExportFinalParagraphMark = TdxExportFinalParagraphMark.SelectedOnly) and FLastParagraphRunNotSelected);
end;

procedure TdxRtfContentExporter.WriteAlternativeAndPlainText(
  AParagraph: TdxParagraph);
begin
  WriteAlternativeText(AParagraph);
end;

procedure TdxRtfContentExporter.WriteAlternativeText(AParagraph: TdxParagraph);
var
  ASeparator: string;
  ANumberingList: TdxNumberingList;
  AProperties, AParagraphProperties: TdxMergedCharacterProperties;
  ARun: TdxTextRunBase;
begin
  RtfBuilder.OpenGroup;
  RtfBuilder.WriteCommand(TdxRtfExportSR.AlternativeText);
  RtfBuilder.WriteCommand(TdxRtfExportSR.ResetParagraphProperties);
  ANumberingList := DocumentModel.NumberingLists[AParagraph.GetNumberingListIndex];
  AProperties := TdxMergedCharacterProperties.Create(ANumberingList.Levels[AParagraph.GetListLevelIndex].CharacterProperties);
  try
    ARun := PieceTable.Runs[AParagraph.FirstRunIndex];
    AParagraphProperties := ARun.GetMergedCharacterProperties;
    try
      AProperties.Merge(AParagraphProperties);
      FCharacterPropertiesExporter.ExportCharacterPropertiesCore(AProperties, True, True);
    finally
      AParagraphProperties.Free;
    end;
  finally
    AProperties.Free;
  end;
  WriteText(GetNumberingListText(AParagraph));
  ASeparator := AParagraph.GetListLevelSeparator;
  if ASeparator <> '' then
    WriteText(ASeparator);
  RtfBuilder.CloseGroup;
end;

procedure TdxRtfContentExporter.WriteChapterSeparator(ASeparator: Char);
begin
  WriteEnumValueCommand<Char>(FChapterSeparatorTypes, ASeparator, TdxRtfExportSR.SectionChapterSeparatorHyphen);
end;

procedure TdxRtfContentExporter.WriteCharacterStyle(
  ACharacterStyle: TdxCharacterStyle);
var
  AStyleCollection: TdxNamedOrdinalDictionary<Integer>;
  AStyleName: string;
begin
  AStyleCollection := RtfExportHelper.CharacterStylesCollectionIndex;
  AStyleName := ACharacterStyle.StyleName;
  if AStyleCollection.ContainsKey(AStyleName) then
    RtfBuilder.WriteCommand(TdxRtfExportSR.CharacterStyleIndex, AStyleCollection[AStyleName]);
end;

procedure TdxRtfContentExporter.WriteEnumValueCommand<T>(
  ATable: TDictionary<T, string>; const AValue: T;
  const ADefaultCommand: string);
var
  ACommand: string;
begin
  if not ATable.TryGetValue(AValue, ACommand) then
    ACommand := ADefaultCommand;
  RtfBuilder.WriteCommand(ACommand);
end;

procedure TdxRtfContentExporter.WriteEnumValueCommand<T>(
  ATable: TdxEnumeratedDictionary<T, string>; const AValue: T; const ADefaultCommand: string);
var
  ACommand: string;
begin
  if not ATable.TryGetValue(AValue, ACommand) then
    ACommand := ADefaultCommand;
  RtfBuilder.WriteCommand(ACommand);
end;

procedure TdxRtfContentExporter.WriteFieldInstructionEnd;
begin
  RtfBuilder.CloseGroup;
  RtfBuilder.OpenGroup;
end;

procedure TdxRtfContentExporter.WriteFieldResultEnd;
begin
  RtfBuilder.CloseGroup;
  RtfBuilder.CloseGroup;
end;

procedure TdxRtfContentExporter.Export(AOutputStream: TStream);
begin
end;

procedure TdxRtfContentExporter.ExportBookmarkEnd(ABookmark: TdxBookmark);
begin
  RtfBuilder.OpenGroup;
  RtfBuilder.WriteCommand(TdxRtfExportSR.BookmarkEnd);
  RtfBuilder.WriteText(ABookmark.Name);
  RtfBuilder.CloseGroup;
end;

procedure TdxRtfContentExporter.ExportBookmarkStart(ABookmark: TdxBookmark);
begin
  RtfBuilder.OpenGroup;
  RtfBuilder.WriteCommand(TdxRtfExportSR.BookmarkStart);
  RtfBuilder.WriteText(ABookmark.Name);
  RtfBuilder.CloseGroup;
end;

procedure TdxRtfContentExporter.ExportRangePermissionStart(ARangePermission: TdxRangePermission);
var
  AData: string;
begin
  AData := GenerateRangePermissionData(ARangePermission);
  if AData = '' then
    Exit;

  RtfBuilder.OpenGroup;
  RtfBuilder.WriteCommand(TdxRtfExportSR.RangePermissionStart);
  RtfBuilder.WriteTextDirect(AData);
  RtfBuilder.CloseGroup;
end;

procedure TdxRtfContentExporter.ExportRangePermissionEnd(ARangePermission: TdxRangePermission);
var
  AData: string;
begin
  AData := GenerateRangePermissionData(ARangePermission);
  if AData = '' then
    Exit;

  RtfBuilder.OpenGroup;
  RtfBuilder.WriteCommand(TdxRtfExportSR.RangePermissionEnd);
  RtfBuilder.WriteTextDirect(AData);
  RtfBuilder.CloseGroup;
end;

function TdxRtfContentExporter.GenerateRangePermissionData(ARangePermission: TdxRangePermission): string;
var
  AHelper: TdxRtfExportHelper;
  AUserIndex, ARangeIndex: Integer;
begin
  AHelper := TdxRtfExportHelper(RtfExportHelper);
  if AHelper = nil then
    Exit('');

  AUserIndex := AHelper.GetUserIndex(ARangePermission);
  if AUserIndex = 0 then
    Exit('');

  ARangeIndex := PieceTable.RangePermissions.IndexOf(ARangePermission);
  if ARangeIndex < 0 then
    Exit('');

  Result := IntToShortString(AUserIndex) + '0100' + IntToShortString(ARangeIndex) + '0000';
end;

function TdxRtfContentExporter.IntToShortString(AValue: Integer): string;
var
  ALow, AHigh: Integer;
begin
  AValue := AValue and $0000FFFF;
  ALow := AValue and $000000FF;
  AHigh := AValue shr 8;
  Result := IntToHex(ALow, 2) + IntToHex(AHigh, 2);
end;

procedure TdxRtfContentExporter.ExportFloatingObjectAnchorRun(ARun: TdxFloatingObjectAnchorRun);
var
  AFloatingObjectProperties: TdxFloatingObjectProperties;
  APictureContent: TdxPictureFloatingObjectContent;
  ATextBoxContent: TdxTextBoxFloatingObjectContent;
begin
  AFloatingObjectProperties := ARun.FloatingObjectProperties;
  RtfBuilder.OpenGroup;
  try
    RtfBuilder.WriteCommand(TdxRtfExportSR.Shape);
    RtfBuilder.OpenGroup;
    try
      RtfBuilder.WriteCommand(TdxRtfExportSR.ShapeInstance);
      ExportShapeInstanceProperties(AFloatingObjectProperties, IfThen(ARun.Shape.UseRotation, ARun.Shape.Rotation, 0));
      ExportFloatingObjectShape(ARun.Shape);
      ExportFloatingObjectHorizontalPositionAlignment(AFloatingObjectProperties);
      ExportFloatingObjectHorizontalPositionTypeProperty(AFloatingObjectProperties);
      ExportFloatingObjectVerticalPositionAlignment(AFloatingObjectProperties);
      ExportFloatingObjectVerticalPositionTypeProperty(AFloatingObjectProperties);
      ExportFloatingObjectLayoutInTableCell(AFloatingObjectProperties);
      ExportFloatingObjectAllowOverlap(AFloatingObjectProperties);
      ExportFloatingObjectLockAspectRatio(AFloatingObjectProperties);
      ExportFloatingObjectHidden(AFloatingObjectProperties);
      ExportFloatingObjectBehindDocument(AFloatingObjectProperties);
      ExportFloatingObjectLeftDistance(AFloatingObjectProperties);
      ExportFloatingObjectRightDistance(AFloatingObjectProperties);
      ExportFloatingObjectTopDistance(AFloatingObjectProperties);
      ExportFloatingObjectBottomDistance(AFloatingObjectProperties);
      ExportFloatingObjectRelativeSize(AFloatingObjectProperties);

      if Trim(ARun.Name) <> '' then
        RtfBuilder.WriteShapeProperty('wzName', ARun.Name);

      APictureContent := Safe<TdxPictureFloatingObjectContent>.Cast(ARun.Content);
      if APictureContent <> nil then
        ExportPicture(ARun, APictureContent);

      ATextBoxContent := Safe<TdxTextBoxFloatingObjectContent>.Cast(ARun.Content);
      if ATextBoxContent <> nil then
        ExportTextBoxContent(ARun, ATextBoxContent);
    finally
      RtfBuilder.CloseGroup;
    end;
  finally
    RtfBuilder.CloseGroup;
  end;
end;

procedure TdxRtfContentExporter.ExportShapeInstanceProperties(AFloatingObjectProperties: TdxFloatingObjectProperties; ARotation: Integer);
var
  AOffsetX, AOffsetY, ARtfRoation, AWidth, AHeight, ATemp: Integer;
begin
  AOffsetX := UnitConverter.ModelUnitsToTwips(AFloatingObjectProperties.Offset.X);
  AOffsetY := UnitConverter.ModelUnitsToTwips(AFloatingObjectProperties.Offset.Y);
  ARtfRoation := UnitConverter.ModelUnitsToFD(ARotation);
  AWidth := UnitConverter.ModelUnitsToTwips(AFloatingObjectProperties.ActualSize.Width);
  AHeight := UnitConverter.ModelUnitsToTwips(AFloatingObjectProperties.ActualSize.Height);
  if TdxShapeInstanceDestination.ShouldSwapSize(ARtfRoation) then
  begin
    ATemp := AWidth;
    AWidth := AHeight;
    AHeight := ATemp;
    Inc(AOffsetX, AWidth);
    Dec(AOffsetY, AWidth);
  end;

  RtfBuilder.WriteCommand(TdxRtfExportSR.ShapeLeft, AOffsetX);
  RtfBuilder.WriteCommand(TdxRtfExportSR.ShapeTop, AOffsetY);
  RtfBuilder.WriteCommand(TdxRtfExportSR.ShapeRight, AOffsetX + AWidth);
  RtfBuilder.WriteCommand(TdxRtfExportSR.ShapeBottom, AOffsetY + AHeight);
  ExportFloatingObjectTextWrapType(AFloatingObjectProperties);
  ExportFloatingObjectHorizontalPositionType(AFloatingObjectProperties);
  ExportFloatingObjectVerticalPositionType(AFloatingObjectProperties);

  if AFloatingObjectProperties.ZOrder <> 0 then
    RtfBuilder.WriteCommand(TdxRtfExportSR.ShapeZOrder, AFloatingObjectProperties.ZOrder);
  if AFloatingObjectProperties.UseTextWrapSide then
    RtfBuilder.WriteCommand(TdxRtfExportSR.ShapeWrapTextSide, TdxRtfExportSR.FloatingObjectTextWrapSideTable[AFloatingObjectProperties.TextWrapSide]);
  if AFloatingObjectProperties.UseLocked and (AFloatingObjectProperties.Locked = True) then
    RtfBuilder.WriteCommand(TdxRtfExportSR.ShapeLocked);
end;

procedure TdxRtfContentExporter.ExportFloatingObjectShape(AShape: TdxShape);
begin
  if AShape.UseRotation then
    ExportFloatingObjectRotation(AShape);

  if TdxAlphaColors.IsTransparentOrEmpty(AShape.OutlineColor) then
    RtfBuilder.WriteShapeBoolProperty('fLine', False);

  if AShape.UseOutlineWidth and (AShape.OutlineWidth > 0) then
  begin
    RtfBuilder.WriteShapeIntegerProperty('lineWidth', UnitConverter.ModelUnitsToEmu(AShape.OutlineWidth));
    if AShape.UseOutlineColor and not TdxAlphaColors.IsTransparentOrEmpty(AShape.OutlineColor) then
      RtfBuilder.WriteShapeColorProperty('lineColor', AShape.OutlineColor);
  end;

  if AShape.UseFillColor and not TdxAlphaColors.IsTransparentOrEmpty(AShape.FillColor) then
  begin
    RtfBuilder.WriteShapeBoolProperty('fFilled', True);
    RtfBuilder.WriteShapeColorProperty('fillColor', AShape.FillColor);
  end
  else
    RtfBuilder.WriteShapeBoolProperty('fFilled', False);
end;

procedure TdxRtfContentExporter.ExportFloatingObjectHorizontalPositionAlignment(AFloatingObjectProperties: TdxFloatingObjectProperties);
begin
  if AFloatingObjectProperties.UseHorizontalPositionAlignment then
    RtfBuilder.WriteShapeIntegerProperty('posh', TdxRtfExportSR.FloatingObjectHorizontalPositionAlignmentTable[AFloatingObjectProperties.HorizontalPositionAlignment]);
end;

procedure TdxRtfContentExporter.ExportFloatingObjectHorizontalPositionTypeProperty(AFloatingObjectProperties: TdxFloatingObjectProperties);
begin
  if AFloatingObjectProperties.UseHorizontalPositionType then
    RtfBuilder.WriteShapeIntegerProperty('posrelh', TdxRtfExportSR.FloatingObjectHorizontalPositionTypeTable[AFloatingObjectProperties.HorizontalPositionType]);
end;

procedure TdxRtfContentExporter.ExportFloatingObjectVerticalPositionAlignment(AFloatingObjectProperties: TdxFloatingObjectProperties);
begin
  if AFloatingObjectProperties.UseVerticalPositionAlignment then
    RtfBuilder.WriteShapeIntegerProperty('posv', TdxRtfExportSR.FloatingObjectVerticalPositionAlignmentTable[AFloatingObjectProperties.VerticalPositionAlignment]);
end;

procedure TdxRtfContentExporter.ExportFloatingObjectVerticalPositionTypeProperty(AFloatingObjectProperties: TdxFloatingObjectProperties);
begin
  if AFloatingObjectProperties.UseVerticalPositionType then
    RtfBuilder.WriteShapeIntegerProperty('posrelv', TdxRtfExportSR.FloatingObjectVerticalPositionTypeTable[AFloatingObjectProperties.VerticalPositionType]);
end;

procedure TdxRtfContentExporter.ExportFloatingObjectLayoutInTableCell(AFloatingObjectProperties: TdxFloatingObjectProperties);
begin
  if AFloatingObjectProperties.UseLayoutInTableCell then
    RtfBuilder.WriteShapeBoolProperty('fLayoutInCell', AFloatingObjectProperties.LayoutInTableCell);
end;

procedure TdxRtfContentExporter.ExportFloatingObjectAllowOverlap(AFloatingObjectProperties: TdxFloatingObjectProperties);
begin
  if AFloatingObjectProperties.UseAllowOverlap then
    RtfBuilder.WriteShapeBoolProperty('fAllowOverlap', AFloatingObjectProperties.AllowOverlap);
end;

procedure TdxRtfContentExporter.ExportFloatingObjectLockAspectRatio(AFloatingObjectProperties: TdxFloatingObjectProperties);
begin
  if AFloatingObjectProperties.UseLockAspectRatio then
    RtfBuilder.WriteShapeBoolProperty('fLockAspectRatio', AFloatingObjectProperties.LockAspectRatio);
end;

procedure TdxRtfContentExporter.ExportFloatingObjectHidden(AFloatingObjectProperties: TdxFloatingObjectProperties);
begin
  if AFloatingObjectProperties.UseHidden then
    RtfBuilder.WriteShapeBoolProperty('fHidden', AFloatingObjectProperties.Hidden);
end;

procedure TdxRtfContentExporter.ExportFloatingObjectBehindDocument(AFloatingObjectProperties: TdxFloatingObjectProperties);
begin
  RtfBuilder.WriteShapeBoolProperty('fBehindDocument', AFloatingObjectProperties.IsBehindDoc);
end;

procedure TdxRtfContentExporter.ExportFloatingObjectLeftDistance(AFloatingObjectProperties: TdxFloatingObjectProperties);
begin
  if AFloatingObjectProperties.UseLeftDistance and
    (AFloatingObjectProperties.LeftDistance <> UnitConverter.EmuToModelUnits(TdxShapeInstanceDestination.DistanceFromText)) then
    RtfBuilder.WriteShapeIntegerProperty('dxWrapDistLeft', UnitConverter.ModelUnitsToEmu(AFloatingObjectProperties.LeftDistance));
end;

procedure TdxRtfContentExporter.ExportFloatingObjectRightDistance(AFloatingObjectProperties: TdxFloatingObjectProperties);
begin
  if AFloatingObjectProperties.UseRightDistance and
    (AFloatingObjectProperties.RightDistance <> UnitConverter.EmuToModelUnits(TdxShapeInstanceDestination.DistanceFromText)) then
    RtfBuilder.WriteShapeIntegerProperty('dxWrapDistRight', UnitConverter.ModelUnitsToEmu(AFloatingObjectProperties.RightDistance));
end;

procedure TdxRtfContentExporter.ExportFloatingObjectTopDistance(AFloatingObjectProperties: TdxFloatingObjectProperties);
begin
  if AFloatingObjectProperties.UseTopDistance then
    RtfBuilder.WriteShapeIntegerProperty('dyWrapDistTop', UnitConverter.ModelUnitsToEmu(AFloatingObjectProperties.TopDistance));
end;

procedure TdxRtfContentExporter.ExportFloatingObjectBottomDistance(AFloatingObjectProperties: TdxFloatingObjectProperties);
begin
  if AFloatingObjectProperties.UseBottomDistance then
    RtfBuilder.WriteShapeIntegerProperty('dyWrapDistBottom', UnitConverter.ModelUnitsToEmu(AFloatingObjectProperties.BottomDistance));
end;

procedure TdxRtfContentExporter.ExportFloatingObjectRelativeSize(AFloatingObjectProperties: TdxFloatingObjectProperties);
var
  AWidth: TdxFloatingObjectRelativeWidth;
  AHeight: TdxFloatingObjectRelativeHeight;
begin
  if AFloatingObjectProperties.UseRelativeWidth then
  begin
    AWidth := AFloatingObjectProperties.RelativeWidth;
    RtfBuilder.WriteShapeIntegerProperty('sizerelh', TdxRtfExportSR.FloatingObjectRelativeFromHorizontalTable[AWidth.From]);
    RtfBuilder.WriteShapeIntegerProperty('pctHoriz', AWidth.Width div 100);
  end;
  if AFloatingObjectProperties.UseRelativeHeight then
  begin
    AHeight := AFloatingObjectProperties.RelativeHeight;
    RtfBuilder.WriteShapeIntegerProperty('sizerelv', TdxRtfExportSR.FloatingObjectRelativeFromVerticalTable[AHeight.From]);
    RtfBuilder.WriteShapeIntegerProperty('pctVert', AHeight.Height div 100);
  end;
  if AFloatingObjectProperties.UsePercentOffset then
  begin
    if AFloatingObjectProperties.PercentOffsetX <> 0 then
      RtfBuilder.WriteShapeIntegerProperty('pctHorizPos', AFloatingObjectProperties.PercentOffsetX div 100);
    if AFloatingObjectProperties.PercentOffsetY <> 0 then
      RtfBuilder.WriteShapeIntegerProperty('pctVertPos', AFloatingObjectProperties.PercentOffsetY div 100);
  end;
end;

procedure TdxRtfContentExporter.ExportPicture(ARun: TdxFloatingObjectAnchorRun; AContent: TdxPictureFloatingObjectContent);
var
  AExportStrategy: TdxRtfFloatingObjectPictureExportStrategy;
begin
  AExportStrategy := TdxRtfFloatingObjectPictureExportStrategy.Create;
  try
    AExportStrategy.Export(FRtfBuilder, ARun, FOptions.Compatibility.DuplicateObjectAsMetafile);
  finally
    AExportStrategy.Free;
  end;
end;

procedure TdxRtfContentExporter.ExportTextBoxContent(ARun: TdxFloatingObjectAnchorRun; AContent: TdxTextBoxFloatingObjectContent);
begin
  ExportTextBoxProperties(AContent.TextBoxProperties);
  RtfBuilder.OpenGroup;
  try
    RtfBuilder.WriteCommand(TdxRtfExportSR.ShapeText);
    PerformExportPieceTable(TdxPieceTable(AContent.TextBox.PieceTable), ExportPieceTable);
  finally
    RtfBuilder.CloseGroup;
  end;
end;

procedure TdxRtfContentExporter.ExportFloatingObjectTextWrapType(AFloatingObjectProperties: TdxFloatingObjectProperties);
var
  ATextWrapType: TdxFloatingObjectTextWrapType;
  AType: string;
begin
  ATextWrapType := AFloatingObjectProperties.TextWrapType;
  if ATextWrapType = TdxFloatingObjectTextWrapType.None then
  begin
    RtfBuilder.WriteCommand(TdxRtfExportSR.ShapeWrapTextType, '3');
    if AFloatingObjectProperties.IsBehindDoc then
      AType := '1'
    else
      AType := '0';
    RtfBuilder.WriteCommand(TdxRtfExportSR.ShapeWrapTextTypeZOrder, AType);
  end
  else
    RtfBuilder.WriteCommand(TdxRtfExportSR.ShapeWrapTextType, TdxRtfExportSR.FloatingObjectTextWrapTypeTable[ATextWrapType]);
end;

procedure TdxRtfContentExporter.ExportFloatingObjectHorizontalPositionType(AFloatingObjectProperties: TdxFloatingObjectProperties);
var
  AHorizontalPositionType: TdxFloatingObjectHorizontalPositionType;
  AShapeHorizontalPositionType: string;
begin
  if AFloatingObjectProperties.UseHorizontalPositionType then
  begin
    AHorizontalPositionType := AFloatingObjectProperties.HorizontalPositionType;
    case AHorizontalPositionType of
      TdxFloatingObjectHorizontalPositionType.Page:
        AShapeHorizontalPositionType := TdxRtfExportSR.ShapeLegacyHorizontalPositionTypePage;
      TdxFloatingObjectHorizontalPositionType.Margin:
        AShapeHorizontalPositionType := TdxRtfExportSR.ShapeLegacyHorizontalPositionTypeMargin;
      TdxFloatingObjectHorizontalPositionType.Column:
        AShapeHorizontalPositionType := TdxRtfExportSR.ShapeLegacyHorizontalPositionTypeColumn;
      else
        AShapeHorizontalPositionType := TdxRtfExportSR.ShapeLegacyHorizontalPositionTypeMargin;
    end;
    RtfBuilder.WriteCommand(AShapeHorizontalPositionType);
    RtfBuilder.WriteCommand(TdxRtfExportSR.ShapeIgnoreLegacyHorizontalPositionType);
  end;
end;

procedure TdxRtfContentExporter.ExportFloatingObjectVerticalPositionType(AFloatingObjectProperties: TdxFloatingObjectProperties);
var
  AVerticalPositionType: TdxFloatingObjectVerticalPositionType;
  AShapeVerticalPositionType: string;
begin
  if AFloatingObjectProperties.UseVerticalPositionType then
  begin
    AVerticalPositionType := AFloatingObjectProperties.VerticalPositionType;

    case AVerticalPositionType of
      TdxFloatingObjectVerticalPositionType.Page:
        AShapeVerticalPositionType := TdxRtfExportSR.ShapeLegacyVerticalPositionTypePage;
      TdxFloatingObjectVerticalPositionType.Margin:
        AShapeVerticalPositionType := TdxRtfExportSR.ShapeLegacyVerticalPositionTypeMargin;
      TdxFloatingObjectVerticalPositionType.Paragraph:
        AShapeVerticalPositionType := TdxRtfExportSR.ShapeLegacyVerticalPositionTypeParagraph;
      else
        AShapeVerticalPositionType := TdxRtfExportSR.ShapeLegacyVerticalPositionTypeParagraph;
    end;
    RtfBuilder.WriteCommand(AShapeVerticalPositionType);
    RtfBuilder.WriteCommand(TdxRtfExportSR.ShapeIgnoreLegacyVerticalPositionType);
  end;
end;

procedure TdxRtfContentExporter.ExportTextBoxProperties(AProperties: TdxTextBoxProperties);
begin
  if AProperties.UseLeftMargin then
    RtfBuilder.WriteShapeIntegerProperty('dxTextLeft', UnitConverter.ModelUnitsToEmu(AProperties.LeftMargin));
  if AProperties.UseRightMargin then
    RtfBuilder.WriteShapeIntegerProperty('dxTextRight', UnitConverter.ModelUnitsToEmu(AProperties.RightMargin));
  if AProperties.UseTopMargin then
    RtfBuilder.WriteShapeIntegerProperty('dyTextTop', UnitConverter.ModelUnitsToEmu(AProperties.TopMargin));
  if AProperties.UseBottomMargin then
    RtfBuilder.WriteShapeIntegerProperty('dyTextBottom', UnitConverter.ModelUnitsToEmu(AProperties.BottomMargin));
  if AProperties.UseResizeShapeToFitText then
    RtfBuilder.WriteShapeBoolProperty('fFitShapeToText', AProperties.ResizeShapeToFitText);
  if AProperties.UseWrapText and not AProperties.WrapText then
    RtfBuilder.WriteShapeIntegerProperty('WrapText', 2);
end;

procedure TdxRtfContentExporter.ExportFloatingObjectRotation(AShape: TdxShape);
begin
  if AShape.UseRotation then
    RtfBuilder.WriteShapeIntegerProperty('rotation', UnitConverter.ModelUnitsToFD(AShape.Rotation));
end;

procedure TdxRtfContentExporter.ExportFootNoteRunCore(ARun: TdxFootNoteRunBase; const ASuffixKeyword: string);
var
  AOldExportFinalParagraphMark: TdxExportFinalParagraphMark;
begin
  RtfBuilder.OpenGroup;
  try
    FCharacterPropertiesExporter.ExportCharacterProperties(ARun.GetMergedCharacterProperties(FGetMergedCharacterPropertiesCachedResult));
    WriteRunCharacterStyle(ARun);

    RtfBuilder.WriteCommand(TdxRtfExportSR.FootNoteReference);

    if not ARun.Paragraph.PieceTable.IsMain then
      Exit;

    RtfBuilder.OpenGroup;
    RtfBuilder.WriteCommand(TdxRtfExportSR.FootNote);
    if ASuffixKeyword <> '' then
      RtfBuilder.WriteCommand(ASuffixKeyword);
    AOldExportFinalParagraphMark := FOptions.ExportFinalParagraphMark;
    FOptions.ExportFinalParagraphMark := TdxExportFinalParagraphMark.Never;
    try
      PerformExportPieceTable(TdxPieceTable(ARun.Note.PieceTable), ExportPieceTable);
    finally
      FOptions.ExportFinalParagraphMark := AOldExportFinalParagraphMark;
    end;
    RtfBuilder.CloseGroup;
  finally
    RtfBuilder.CloseGroup;
  end;
end;

procedure TdxRtfContentExporter.ExportFootNoteRun(ARun: TdxFootNoteRun);
begin
  ExportFootNoteRunCore(ARun, '');
end;

procedure TdxRtfContentExporter.ExportEndNoteRun(ARun: TdxEndNoteRun);
begin
  ExportFootNoteRunCore(ARun, TdxRtfExportSR.EndNote);
end;

function TdxRtfContentExporter.AreFootNotesPresent: Boolean;
var
  ANotes: TdxFootNoteCollection;
  ACount, I: Integer;
begin
  ANotes := TdxFootNoteCollection(DocumentModel.FootNotes);
  ACount := ANotes.Count;
  for I := 0 to ACount - 1 do
    if ANotes[I].IsReferenced then
      Exit(True);
  Result := False;
end;

class function TdxRtfContentExporter.BorderLineStylesTryGetValue(AKey: TdxBorderLineStyle;
  var AResult: string): Boolean;
begin
{$IFDEF DELPHIXE2}
  if AKey in [TdxBorderLineStyle.&Nil..TdxBorderLineStyle.Inset] then
    AResult := BorderLineStyles[AKey]
{$ELSE}
  if Ord(AKey) in [Ord(TdxBorderLineStyle.&Nil)..Ord(TdxBorderLineStyle.Inset)] then
    AResult := BorderLineStyles[Ord(AKey)]
{$ENDIF}
  else
    AResult := '';
  Result := AResult <> '';
end;

function TdxRtfContentExporter.AreEndNotesPresent: Boolean;
var
  ANotes: TdxEndNoteCollection;
  ACount, I: Integer;
begin
  ANotes := TdxEndNoteCollection(DocumentModel.EndNotes);
  ACount := ANotes.Count;
  for I := 0 to ACount - 1 do
    if ANotes[I].IsReferenced then
      Exit(True);
  Result := False;
end;

procedure TdxRtfContentExporter.ExportDocumentLevelFootNotePresenceProperties;
var
  AFootNotesPresent, AEndNotesPresent: Boolean;
begin
  if not DocumentModel.DocumentCapabilities.FootNotesAllowed then
    Exit;
  if not DocumentModel.DocumentCapabilities.EndNotesAllowed then
    Exit;

  AFootNotesPresent := AreFootNotesPresent;
  AEndNotesPresent := AreEndNotesPresent;
  if not AFootNotesPresent and not AEndNotesPresent then
    Exit;
  if AEndNotesPresent and not AFootNotesPresent then
    RtfBuilder.WriteCommand('\fet1')
  else
    RtfBuilder.WriteCommand('\fet2');
end;

procedure TdxRtfContentExporter.ExportDocumentLevelFootNoteProperties;
var
  ANote: TdxSectionFootNote;
  ADefaultInfo: TdxFootNoteInfo;
begin
  if not DocumentModel.DocumentCapabilities.FootNotesAllowed then
    Exit;

  ANote := DocumentModel.Sections.First.FootNote;
  ADefaultInfo := DocumentModel.Cache.FootNoteInfoCache[TdxFootNoteInfoCache.DefaultFootNoteItemIndex];
  if ANote.Position <> ADefaultInfo.Position then
  begin
    if ANote.Position = TdxFootNotePosition.BelowText then
      RtfBuilder.WriteCommand(TdxRtfExportSR.FootNotePlacementBelowText)
    else
      RtfBuilder.WriteCommand(TdxRtfExportSR.FootNotePlacementPageBottom);
  end;

  if ANote.StartingNumber <> ADefaultInfo.StartingNumber then
    FRtfBuilder.WriteCommand(TdxRtfExportSR.FootNoteNumberingStart, ANote.StartingNumber);

  if ANote.NumberingFormat <> ADefaultInfo.NumberingFormat then
    WriteEnumValueCommand<TdxNumberingFormat>(FFootNoteNumberingTypes, ANote.NumberingFormat, TdxRtfExportSR.FootNoteNumberingFormatDecimal);

  if ANote.NumberingRestartType <> ADefaultInfo.NumberingRestartType then
  begin
    if ANote.NumberingRestartType = TdxLineNumberingRestart.Continuous then
      RtfBuilder.WriteCommand(TdxRtfExportSR.FootNoteNumberingRestartContinuous)
    else
      if ANote.NumberingRestartType = TdxLineNumberingRestart.NewSection then
        RtfBuilder.WriteCommand(TdxRtfExportSR.FootNoteNumberingRestartEachSection)
      else
        RtfBuilder.WriteCommand(TdxRtfExportSR.FootNoteNumberingRestartEachPage);
  end;
end;

procedure TdxRtfContentExporter.ExportDocumentLevelEndNoteProperties;
var
  ANote: TdxSectionFootNote;
  ADefaultInfo: TdxFootNoteInfo;
begin
  if not DocumentModel.DocumentCapabilities.EndNotesAllowed then
    Exit;

  ANote := DocumentModel.Sections.First.EndNote;
  ADefaultInfo := DocumentModel.Cache.FootNoteInfoCache[TdxFootNoteInfoCache.DefaultEndNoteItemIndex];

  if ANote.Position <> ADefaultInfo.Position then
  begin
    if ANote.Position = TdxFootNotePosition.EndOfSection then
      RtfBuilder.WriteCommand(TdxRtfExportSR.EndNotePlacementEndOfSection)
    else
      RtfBuilder.WriteCommand(TdxRtfExportSR.EndNotePlacementEndOfDocument);
  end;

  if ANote.StartingNumber <> ADefaultInfo.StartingNumber then
    FRtfBuilder.WriteCommand(TdxRtfExportSR.EndNoteNumberingStart, ANote.StartingNumber);

  if ANote.NumberingFormat <> ADefaultInfo.NumberingFormat then
    WriteEnumValueCommand<TdxNumberingFormat>(FEndNoteNumberingTypes, ANote.NumberingFormat, TdxRtfExportSR.EndNoteNumberingFormatLowerRoman);

  if ANote.NumberingRestartType <> ADefaultInfo.NumberingRestartType then
  begin
    if ANote.NumberingRestartType = TdxLineNumberingRestart.Continuous then
      RtfBuilder.WriteCommand(TdxRtfExportSR.EndNoteNumberingRestartContinuous)
    else
      RtfBuilder.WriteCommand(TdxRtfExportSR.EndNoteNumberingRestartEachSection);
  end;
end;

procedure TdxRtfContentExporter.ExportDocumentInformation;
begin
  if not ShouldExportDocumentInformation then
    Exit;

  RtfBuilder.OpenGroup;
  RtfBuilder.WriteCommand(TdxRtfExportSR.DocumentInformation);
  ExportDocumentProtectionPasswordHash;
  RtfBuilder.CloseGroup;
end;

procedure TdxRtfContentExporter.ExportDocumentProtectionProperties;
var
  AProperties: TdxDocumentProtectionProperties;
begin
  AProperties := DocumentModel.ProtectionProperties;
  if not AProperties.EnforceProtection then
    Exit;

  RtfBuilder.WriteCommand(TdxRtfExportSR.EnforceProtection, 1);
  if AProperties.ProtectionType = TdxDocumentProtectionType.ReadOnly then
  begin
    RtfBuilder.WriteCommand(TdxRtfExportSR.AnnotationProtection);
    RtfBuilder.WriteCommand(TdxRtfExportSR.ReadOnlyProtection);
    RtfBuilder.WriteCommand(TdxRtfExportSR.ProtectionLevel, 3);
  end;
end;

procedure TdxRtfContentExporter.ExportDocumentProtectionPasswordHash;
begin
  if not ShouldExportDocumentProtectionPasswordHash then
    Exit;

  if ShouldExportDocumentProtectionPasswordHashInWord2007Format then
    ExportDocumentProtectionPasswordHashWord2007
  else
    ExportDocumentProtectionPasswordHashWord2003;
end;

procedure TdxRtfContentExporter.ExportPageBackground(ADocumentProperties: TdxDocumentProperties);
begin
  RtfBuilder.OpenGroup;
  try
    RtfBuilder.WriteCommand(TdxRtfExportSR.PageBackground);
    RtfBuilder.OpenGroup;
    try
      RtfBuilder.WriteCommand(TdxRtfExportSR.Shape);
      RtfBuilder.OpenGroup;
      try
        RtfBuilder.WriteCommand(TdxRtfExportSR.ShapeInstance);
        RtfBuilder.WriteShapeColorProperty('fillColor', ADocumentProperties.PageBackColor);
      finally
        RtfBuilder.CloseGroup;
      end;
    finally
      RtfBuilder.CloseGroup;
    end;
  finally
    RtfBuilder.CloseGroup;
  end;
end;

function TdxRtfContentExporter.ShouldExportDocumentInformation: Boolean;
begin
  Result := ShouldExportDocumentProtectionPasswordHash;
end;

function TdxRtfContentExporter.ShouldExportDocumentProtectionPasswordHash: Boolean;
var
  AProperties: TdxDocumentProtectionProperties;
begin
  AProperties := DocumentModel.ProtectionProperties;
  Result := AProperties.EnforceProtection and
    ((AProperties.PasswordHash <> nil) or (AProperties.Word2003PasswordHash <> nil));
end;

function TdxRtfContentExporter.ShouldExportDocumentProtectionPasswordHashInWord2007Format: Boolean;
var
  AProperties: TdxDocumentProtectionProperties;
begin
  AProperties := DocumentModel.ProtectionProperties;
  Result := (AProperties.PasswordHash <> nil) and (AProperties.HashAlgorithmType <> TdxHashAlgorithmType.None);
end;

procedure TdxRtfContentExporter.ExportDocumentProtectionPasswordHashWord2007;
var
  ABytes: TArray<Byte>;
begin
  RtfBuilder.OpenGroup;
  RtfBuilder.WriteCommand(TdxRtfExportSR.PasswordHash);
  ABytes := GetDocumentProtectionPasswordHashWord2007Bytes;
  RtfBuilder.WriteByteArrayAsHex(ABytes);
  RtfBuilder.CloseGroup;
end;

function TdxRtfContentExporter.GetDocumentProtectionPasswordHashWord2007Bytes: TArray<Byte>;
var
  AProperties: TdxDocumentProtectionProperties;
  APasswordHash, APasswordPrefix: TArray<Byte>;
  AStream: TdxMemoryStream;
  APasswordHashLength, APasswordPrefixLength: Integer;
begin
  AProperties := DocumentModel.ProtectionProperties;
  APasswordHash := AProperties.PasswordHash;
  APasswordHashLength := Length(APasswordHash);
  APasswordPrefix := AProperties.PasswordPrefix;
  APasswordPrefixLength := Length(APasswordPrefix);

  AStream := TdxMemoryStream.Create;
  try
    AStream.WriteInteger(1);
    AStream.WriteInteger(40 + APasswordHashLength + APasswordPrefixLength);
    AStream.WriteInteger(1);
    AStream.WriteInteger($8000 + Ord(AProperties.HashAlgorithmType));
    AStream.WriteInteger(AProperties.HashIterationCount);
    AStream.WriteInteger(APasswordHashLength);
    AStream.WriteInteger(APasswordPrefixLength);
    AStream.WriteInteger(0);
    AStream.WriteInteger(0);
    AStream.WriteInteger(0);

    AStream.WriteByteArray(APasswordHash);
    AStream.WriteByteArray(APasswordPrefix);
    Result := AStream.ToArray;
  finally
    AStream.Free;
  end;
end;

procedure TdxRtfContentExporter.ExportDocumentProtectionPasswordHashWord2003;
var
  AProperties: TdxDocumentProtectionProperties;
  AHash: TArray<Byte>;
  AValue: string;
  AHashValue: Integer;
begin
  AProperties := DocumentModel.ProtectionProperties;

  RtfBuilder.OpenGroup;
  RtfBuilder.WriteCommand(TdxRtfExportSR.Password);

  AHash := AProperties.Word2003PasswordHash;
  if AHash = nil then
    AValue := '00000000'
  else
  begin
    if Length(AHash) <> 4 then
      raise EArgumentOutOfRangeException.Create('Hash');
    Move(PByte(AHash)^, AHashValue, SizeOf(AHashValue));
    AValue := LowerCase(IntToHex(AHashValue, 8));
  end;
  RtfBuilder.WriteTextDirect(AValue);
  RtfBuilder.CloseGroup;
end;

procedure TdxRtfContentExporter.WriteFieldResultStart;
begin
  RtfBuilder.WriteCommand(TdxRtfExportSR.FieldResult);
end;

procedure TdxRtfContentExporter.WritePageNumberingFormat(
  ANumberingFormat: TdxNumberingFormat);
begin
  WriteEnumValueCommand<TdxNumberingFormat>(FPageNumberingTypes, ANumberingFormat, TdxRtfExportSR.SectionPageNumberingDecimal);
end;

procedure TdxRtfContentExporter.WriteParagraphStyle(
  AParagraphStyle: TdxParagraphStyle);
var
  AStyleCollection: TdxNamedOrdinalDictionary<Integer>;
  AStyleName: string;
begin
  AStyleCollection := RtfExportHelper.ParagraphStylesCollectionIndex;
  AStyleName := AParagraphStyle.StyleName;
  if AStyleCollection.ContainsKey(AStyleName) then
    RtfBuilder.WriteCommand(TdxRtfExportSR.ParagraphStyle, AStyleCollection[AStyleName]);
end;

procedure TdxRtfContentExporter.WriteRunCharacterStyle(ARun: TdxTextRunBase);
begin
  if ShouldWriteRunCharacterStyle(ARun) then
    WriteCharacterStyle(ARun.CharacterStyle);
end;

procedure TdxRtfContentExporter.WriteSectionBreakType(ABreakType: TdxSectionStartType);
begin
  RtfBuilder.WriteCommand(SectionStartTypes[ABreakType]);
end;

procedure TdxRtfContentExporter.WriteText(const AText: string);
begin
  RtfBuilder.WriteText(AText);
end;

procedure TdxRtfContentExporter.WriteVerticalAlignment(AAlignment: TdxVerticalAlignment);
begin
  if AAlignment in [Low(TdxVerticalAlignment)..High(TdxVerticalAlignment)] then
    RtfBuilder.WriteCommand(VerticalAlignmentTypes[AAlignment])
  else
    RtfBuilder.WriteCommand(TdxRtfExportSR.VerticalAlignmentBottom);
end;

procedure TdxRtfContentExporter.WriteFieldInstructionStart(ARun: TdxFieldCodeStartRun);
var
  AField: TdxField;
begin
  RtfBuilder.OpenGroup;
  RtfBuilder.WriteCommand(TdxRtfExportSR.Field);
  if not DocumentModel.FieldOptions.UpdateFieldsOnPaste and KeepFieldCodeViewState then
  begin
    AField := PieceTable.FindFieldByRunIndex(ARun.GetRunIndex);
    if AField.IsCodeView then
      RtfBuilder.WriteCommand(TdxRtfExportSR.FieldCodeView);
  end;
  RtfBuilder.OpenGroup;
  RtfBuilder.WriteCommand(TdxRtfExportSR.FieldInstructions);
end;

function TdxRtfContentExporter.CreateNumberingListExporter(
  AExporter: TdxRtfContentExporter): TdxRtfNumberingListExporter;
begin
  Result := TdxRtfNumberingListExporter.Create(AExporter);
end;

class function TdxRtfContentExporter.CreatePageNumberingTypesTable: TdxEnumeratedDictionary<TdxNumberingFormat, string>;
begin
  Result := TdxEnumeratedDictionary<TdxNumberingFormat, string>.Create;
  Result.Add(TdxNumberingFormat.Decimal, TdxRtfExportSR.SectionPageNumberingDecimal);
  Result.Add(TdxNumberingFormat.UpperRoman, TdxRtfExportSR.SectionPageNumberingUpperRoman);
  Result.Add(TdxNumberingFormat.LowerRoman, TdxRtfExportSR.SectionPageNumberingLowerRoman);
  Result.Add(TdxNumberingFormat.UpperLetter, TdxRtfExportSR.SectionPageNumberingUpperLetter);
  Result.Add(TdxNumberingFormat.LowerLetter, TdxRtfExportSR.SectionPageNumberingLowerLetter);
  Result.Add(TdxNumberingFormat.ArabicAbjad, TdxRtfExportSR.SectionPageNumberingArabicAbjad);
  Result.Add(TdxNumberingFormat.ArabicAlpha, TdxRtfExportSR.SectionPageNumberingArabicAlpha);
  Result.Add(TdxNumberingFormat.Chosung, TdxRtfExportSR.SectionPageNumberingChosung);
  Result.Add(TdxNumberingFormat.DecimalEnclosedCircle, TdxRtfExportSR.SectionPageNumberingDecimalEnclosedCircle);
  Result.Add(TdxNumberingFormat.DecimalFullWidth, TdxRtfExportSR.SectionPageNumberingDecimalFullWidth);
  result.Add(TdxNumberingFormat.Ganada, TdxRtfExportSR.SectionPageNumberingGanada);
  Result.Add(TdxNumberingFormat.HindiVowels, TdxRtfExportSR.SectionPageNumberingHindiVowels);
  Result.Add(TdxNumberingFormat.HindiConsonants, TdxRtfExportSR.SectionPageNumberingHindiConsonants);
  Result.Add(TdxNumberingFormat.HindiNumbers, TdxRtfExportSR.SectionPageNumberingHindiNumbers);
  Result.Add(TdxNumberingFormat.HindiDescriptive, TdxRtfExportSR.SectionPageNumberingHindiDescriptive);
  Result.Add(TdxNumberingFormat.ThaiLetters, TdxRtfExportSR.SectionPageNumberingThaiLetters);
  Result.Add(TdxNumberingFormat.ThaiNumbers, TdxRtfExportSR.SectionPageNumberingThaiNumbers);
  Result.Add(TdxNumberingFormat.ThaiDescriptive, TdxRtfExportSR.SectionPageNumberingThaiDescriptive);
  Result.Add(TdxNumberingFormat.VietnameseDescriptive, TdxRtfExportSR.SectionPageNumberingVietnameseDescriptive);
end;

function TdxRtfContentExporter.CreateParagraphPropertiesExporter: TdxRtfParagraphPropertiesExporter;
begin
  Result := TdxRtfParagraphPropertiesExporter.Create(DocumentModel, RtfExportHelper, RtfBuilder)
end;

class function TdxRtfContentExporter.CreatePredefinedUserGroups: TDictionary<Integer, string>;
begin
  Result := TDictionary<Integer, string>.Create;
  Result.Add($FFFA, 'Current User');
  Result.Add($FFFB, 'Editors');
  Result.Add($FFFC, 'Owners');
  Result.Add($FFFD, 'Contributors');
  Result.Add($FFFE, 'Administrators');
  Result.Add($FFFF, 'Everyone');
end;

function TdxRtfContentExporter.CreateRtfBuilder: TdxRtfBuilder;
var
  AEncoding: TEncoding;
  AIsSingleByte: Boolean;
begin
  AEncoding := Options.ActualEncoding;
  AIsSingleByte := AEncoding.IsSingleByte;
  if AIsSingleByte then
    Result := TdxRtfBuilder.Create(AEncoding)
  else
    Result := TdxDBCSRtfBuilder.Create(AEncoding);
end;

class function TdxRtfContentExporter.CreateSectionEndNoteNumberingTypesTable: TdxEnumeratedDictionary<TdxNumberingFormat, string>;
begin
  Result := TdxEnumeratedDictionary<TdxNumberingFormat, string>.Create;
  Result.Add(TdxNumberingFormat.Decimal, TdxRtfExportSR.SectionEndNoteNumberingFormatDecimal);
  Result.Add(TdxNumberingFormat.UpperRoman, TdxRtfExportSR.SectionEndNoteNumberingFormatUpperRoman);
  Result.Add(TdxNumberingFormat.LowerRoman, TdxRtfExportSR.SectionEndNoteNumberingFormatLowerRoman);
  Result.Add(TdxNumberingFormat.UpperLetter, TdxRtfExportSR.SectionEndNoteNumberingFormatUpperLetter);
  Result.Add(TdxNumberingFormat.LowerLetter, TdxRtfExportSR.SectionEndNoteNumberingFormatLowerLetter);
  Result.Add(TdxNumberingFormat.Chicago, TdxRtfExportSR.SectionEndNoteNumberingFormatChicago);
  Result.Add(TdxNumberingFormat.Chosung, TdxRtfExportSR.SectionEndNoteNumberingFormatChosung);
  Result.Add(TdxNumberingFormat.DecimalEnclosedCircle, TdxRtfExportSR.SectionEndNoteNumberingFormatDecimalEnclosedCircle);
  Result.Add(TdxNumberingFormat.DecimalFullWidth, TdxRtfExportSR.SectionEndNoteNumberingFormatDecimalFullWidth);
  Result.Add(TdxNumberingFormat.Ganada, TdxRtfExportSR.SectionEndNoteNumberingFormatGanada);
end;

class function TdxRtfContentExporter.CreateSectionFootNoteNumberingTypesTable: TdxEnumeratedDictionary<TdxNumberingFormat, string>;
begin
  Result := TdxEnumeratedDictionary<TdxNumberingFormat, string>.Create;
  Result.Add(TdxNumberingFormat.Decimal, TdxRtfExportSR.SectionFootNoteNumberingFormatDecimal);
  Result.Add(TdxNumberingFormat.UpperRoman, TdxRtfExportSR.SectionFootNoteNumberingFormatUpperRoman);
  Result.Add(TdxNumberingFormat.LowerRoman, TdxRtfExportSR.SectionFootNoteNumberingFormatLowerRoman);
  Result.Add(TdxNumberingFormat.UpperLetter, TdxRtfExportSR.SectionFootNoteNumberingFormatUpperLetter);
  Result.Add(TdxNumberingFormat.LowerLetter, TdxRtfExportSR.SectionFootNoteNumberingFormatLowerLetter);
  Result.Add(TdxNumberingFormat.Chicago, TdxRtfExportSR.SectionFootNoteNumberingFormatChicago);
  Result.Add(TdxNumberingFormat.Chosung, TdxRtfExportSR.SectionFootNoteNumberingFormatChosung);
  Result.Add(TdxNumberingFormat.DecimalEnclosedCircle, TdxRtfExportSR.SectionFootNoteNumberingFormatDecimalEnclosedCircle);
  Result.Add(TdxNumberingFormat.DecimalFullWidth, TdxRtfExportSR.SectionFootNoteNumberingFormatDecimalFullWidth);
  Result.Add(TdxNumberingFormat.Ganada, TdxRtfExportSR.SectionFootNoteNumberingFormatGanada);
end;

function TdxRtfContentExporter.CreateStyleExporter: TdxRtfStyleExporter;
begin
  Result := TdxRtfStyleExporter.Create(PieceTable.DocumentModel, CreateRtfBuilder, RtfExportHelper, FOptions);
end;

{ TdxRtfPropertiesExporter }

constructor TdxRtfPropertiesExporter.Create(ADocumentModel: TdxDocumentModel;
  const ARtfExportHelper: IdxRtfExportHelper; ARtfBuilder: TdxRtfBuilder);
begin
  inherited Create;
  FDocumentModel := ADocumentModel;
  FRtfExportHelper := ARtfExportHelper;
  FRtfBuilder := ARtfBuilder;
end;

function TdxRtfPropertiesExporter.GetUnitConverter: TdxDocumentModelUnitConverter;
begin
  Result := DocumentModel.UnitConverter;
end;

procedure TdxRtfPropertiesExporter.WriteWidthUnit(AUnit: TdxWidthUnitInfo; const ATypeKeyword, AValueKeyword: string; AWriteValueAnyway: Boolean = False);
var
  ADefaultWidthUnit: TdxWidthUnitInfo;
  AVal: Integer;
begin
  ADefaultWidthUnit := DocumentModel.Cache.UnitInfoCache.DefaultItem;
  if AUnit.&Type <> ADefaultWidthUnit.&Type then
  begin
    AVal := AUnit.Value;
    case AUnit.&Type of
      TdxWidthUnitType.ModelUnits:
        begin
          RtfBuilder.WriteCommand(ATypeKeyword, Ord(TdxWidthUnitType.ModelUnits));
          AVal := UnitConverter.ModelUnitsToTwips(AVal);
        end;
      TdxWidthUnitType.FiftiethsOfPercent:
          RtfBuilder.WriteCommand(ATypeKeyword, Ord(TdxWidthUnitType.FiftiethsOfPercent));
      TdxWidthUnitType.Auto:
        RtfBuilder.WriteCommand(ATypeKeyword, Ord(TdxWidthUnitType.Auto));
      TdxWidthUnitType.Nil:
        RtfBuilder.WriteCommand(ATypeKeyword, Ord(TdxWidthUnitType.Nil));
    else
      TdxRichEditExceptions.ThrowInternalException;
    end;
    if (AUnit.Value <> ADefaultWidthUnit.Value) or AWriteValueAnyway then
      RtfBuilder.WriteCommand(AValueKeyword, AVal);
  end;
end;

procedure TdxRtfPropertiesExporter.WriteWidthUnitInTwips(
  AUnit: TdxWidthUnitInfo; const ATypeKeyword, AValueKeyword: string);
var
  ADefaultWidthUnit: TdxWidthUnitInfo;
  AVal: Integer;
begin
  ADefaultWidthUnit := DocumentModel.Cache.UnitInfoCache.DefaultItem;
  if AUnit.&Type <> ADefaultWidthUnit.&Type then
  begin
    AVal := AUnit.Value;
    case AUnit.&Type of
      TdxWidthUnitType.ModelUnits:
        begin
          RtfBuilder.WriteCommand(ATypeKeyword, Ord(TdxWidthUnitType.ModelUnits));
          AVal := UnitConverter.ModelUnitsToTwips(AVal);
        end;
      TdxWidthUnitType.Nil:
        RtfBuilder.WriteCommand(ATypeKeyword, Ord(TdxWidthUnitType.Nil));
    else
      TdxRichEditExceptions.ThrowInternalException;
    end;
    if AUnit.Value <> ADefaultWidthUnit.Value then
      RtfBuilder.WriteCommand(AValueKeyword, AVal);
  end;
end;

function TdxRtfPropertiesExporter.ShouldExportCellMargin(
  AMarginUnit: TdxWidthUnitInfo): Boolean;
begin
  Result := (AMarginUnit.&Type <> TdxWidthUnitType.Nil) or (AMarginUnit.Value <> 0);
end;

procedure TdxRtfPropertiesExporter.WriteBoolCommand(const ACommand: string;
  AValue: Boolean);
begin
  if AValue then
    RtfBuilder.WriteCommand(ACommand)
  else
    RtfBuilder.WriteCommand(ACommand, 0);
end;

procedure TdxRtfPropertiesExporter.WriteBorderProperties(
  ABorder: TdxBorderInfo);
var
  ADefaultBorder: TdxBorderInfo;
  AColorIndex: Integer;
begin
  ADefaultBorder := DocumentModel.Cache.BorderInfoCache.DefaultItem;
  WriteBorderStyle(ABorder.Style);
  if ABorder.Style = TdxBorderLineStyle.Nil then
    Exit;
  WriteBorderWidth(ABorder.Width, ADefaultBorder.Width);
  if ABorder.Offset <> ADefaultBorder.Offset then
    RtfBuilder.WriteCommand(TdxRtfExportSR.BorderSpace, UnitConverter.ModelUnitsToTwips(ABorder.Offset));
  if (ABorder.Color <> ADefaultBorder.Color) and not TdxAlphaColors.IsEmpty(ABorder.Color) then
  begin
    AColorIndex := RtfExportHelper.GetColorIndex(ABorder.Color);
    RtfBuilder.WriteCommand(TdxRtfExportSR.BorderColor, AColorIndex);
  end;
  if ABorder.Frame <> ADefaultBorder.Frame then
    RtfBuilder.WriteCommand(TdxRtfExportSR.BorderFrame);
  if ABorder.Shadow <> ADefaultBorder.Shadow then
    RtfBuilder.WriteCommand(TdxRtfExportSR.BorderShadow);
end;

procedure TdxRtfPropertiesExporter.WriteBorderStyle(AValue: TdxBorderLineStyle);
var
  ABorderCommand: string;
  ABorderArtIndex: Integer;
begin
  if AValue <> TdxBorderLineStyle.Single then
  begin
    if TdxRtfContentExporter.BorderLineStylesTryGetValue(AValue, ABorderCommand) then
      RtfBuilder.WriteCommand(ABorderCommand)
    else
    begin
      ABorderArtIndex := TdxRtfArtBorderConverter.GetBorderArtIndex(AValue);
      if ABorderArtIndex > 0 then
        RtfBuilder.WriteCommand(TdxRtfExportSR.BorderArtIndex, ABorderArtIndex);
    end;
  end
  else
    RtfBuilder.WriteCommand(TdxRtfExportSR.BorderSingleWidth);
end;

procedure TdxRtfPropertiesExporter.WriteBorderWidth(AValue,
  ADefaultValue: Integer);
var
  AVal: Integer;
begin
  if AValue = ADefaultValue then
    Exit;
  AVal := UnitConverter.ModelUnitsToTwips(AValue);
  if AVal > 255 then
  begin
    AVal := Min(AVal div 2, 255);
    RtfBuilder.WriteCommand(TdxRtfExportSR.BorderWidth, AVal);
    RtfBuilder.WriteCommand(TdxRtfExportSR.BorderDoubleWidth);
  end
  else
    RtfBuilder.WriteCommand(TdxRtfExportSR.BorderWidth, AVal);
end;

{ TdxRtfCharacterPropertiesExporter }

constructor TdxRtfCharacterPropertiesExporter.Create(
  ADocumentModel: TdxDocumentModel; const ARtfExportHelper: IdxRtfExportHelper;
  ARtfBuilder: TdxRtfBuilder; const AOptions: TdxRtfDocumentExporterOptions);
begin
  inherited Create(ADocumentModel, ARtfExportHelper, ARtfBuilder);
  FOptions := AOptions;
end;

procedure TdxRtfCharacterPropertiesExporter.ExportCharacterProperties(
  ACharacterProperties: TdxMergedCharacterProperties;
  ACheckDefaultColor: Boolean = False;
  ACheckUseFontSize: Boolean = False;
  ACheckUseFontName: Boolean = False);
var
  AInfo: TdxCharacterFormattingInfo;
  AColor: TdxAlphaColor;
begin
  ExportCharacterPropertiesCore(ACharacterProperties, ACheckUseFontSize, ACheckUseFontName);
  AInfo := ACharacterProperties.Info;
  WriteForegroundColor(AInfo.ForeColor, ACheckDefaultColor);

  AColor := AInfo.BackColor;
  if not TdxAlphaColors.IsTransparentOrEmpty(AColor) then
    WriteBackgroundColor(AColor);

  AColor := AInfo.UnderlineColor;
  if not TdxAlphaColors.IsTransparentOrEmpty(AColor) then
    WriteUnderlineColor(AColor);
end;

procedure TdxRtfCharacterPropertiesExporter.ExportCharacterPropertiesCore(
  ACharacterProperties: TdxMergedCharacterProperties;
  ACheckUseFontSize, ACheckUseFontName: Boolean);
var
  AInfo: TdxCharacterFormattingInfo;
  AFontNameIndex: Integer;
begin
  AInfo := ACharacterProperties.Info;
  if AInfo.NoProof then
    RtfBuilder.WriteCommand(TdxRtfExportSR.NoProof);
  if AInfo.AllCaps then
    RtfBuilder.WriteCommand(TdxRtfExportSR.AllCapitals);
  if AInfo.Hidden then
    RtfBuilder.WriteCommand(TdxRtfExportSR.HiddenText);
  if AInfo.FontBold then
    RtfBuilder.WriteCommand(TdxRtfExportSR.FontBold);
  if AInfo.FontItalic then
    RtfBuilder.WriteCommand(TdxRtfExportSR.FontItalic);
  if AInfo.FontStrikeoutType <> TdxStrikeoutType.None then
  begin
    if AInfo.FontStrikeoutType = TdxStrikeoutType.Single then
      RtfBuilder.WriteCommand(TdxRtfExportSR.FontStrikeout)
    else
      RtfBuilder.WriteCommand(TdxRtfExportSR.FontDoubleStrikeout);
  end;
  if AInfo.Script <> TdxCharacterFormattingScript.Normal then
  begin
    if AInfo.Script = TdxCharacterFormattingScript.Subscript then
      RtfBuilder.WriteCommand(TdxRtfExportSR.RunSubScript)
    else
      RtfBuilder.WriteCommand(TdxRtfExportSR.RunSuperScript);
  end;
  if AInfo.UnderlineWordsOnly and (AInfo.FontUnderlineType = TdxUnderlineType.Single) then
    RtfBuilder.WriteCommand(TdxRtfExportSR.FontUnderlineWordsOnly)
  else
  begin
    if AInfo.FontUnderlineType <> TdxUnderlineType.None then
      WriteFontUnderline(AInfo.FontUnderlineType);
  end;
  if  not ACheckUseFontName or ACharacterProperties.Options.UseFontName then
  begin
    AFontNameIndex := WriteFontName(AInfo.FontName);
    if AFontNameIndex >= 0 then
      RegisterFontCharset(AInfo, AFontNameIndex);
  end;
  if not ACheckUseFontSize or ACharacterProperties.Options.UseDoubleFontSize then
    WriteFontSize(AInfo.DoubleFontSize);
end;

procedure TdxRtfCharacterPropertiesExporter.ExportParagraphCharacterProperties(
  ACharacterProperties: TdxMergedCharacterProperties);
var
  AForeColor: TdxAlphaColor;
begin
  ExportCharacterPropertiesCore(ACharacterProperties, False, False);
  AForeColor := ACharacterProperties.Info.ForeColor;
  if not TdxAlphaColors.IsEmpty(AForeColor) then
    WriteForegroundColorCore(AForeColor);
end;

procedure TdxRtfCharacterPropertiesExporter.RegisterFontCharset(
  AInfo: TdxCharacterFormattingInfo; AFontNameIndex: Integer);
var
  ACharset: Integer;
  AFontInfoIndex: Integer;
  AFontInfo: TdxFontInfo;
  AFontStyle: TFontStyles;
begin
  if not RtfExportHelper.FontCharsetTable.TryGetValue(AFontNameIndex, ACharset) then
  begin
    AFontStyle := [];
    if AInfo.FontBold then
      Include(AFontStyle, fsBold);
    if AInfo.FontItalic then
      Include(AFontStyle, fsItalic);

    AFontInfoIndex := DocumentModel.FontCache.CalcFontIndex(AInfo.FontName,
      AInfo.DoubleFontSize, AFontStyle, AInfo.Script);
    AFontInfo := DocumentModel.FontCache[AFontInfoIndex];
    RtfExportHelper.FontCharsetTable.Add(AFontNameIndex, AFontInfo.Charset);
  end;
end;

procedure TdxRtfCharacterPropertiesExporter.WriteBackgroundColor(
  ABackColor: TdxAlphaColor);
var
  AIndex: Integer;
  AMode: TdxRtfRunBackColorExportMode;
begin
  if not TdxAlphaColors.IsEmpty(ABackColor) then
    ABackColor := RtfExportHelper.BlendColor(ABackColor);
  AIndex := RtfExportHelper.GetColorIndex(ABackColor);
  AMode := FOptions.Compatibility.BackColorExportMode;
  if AMode = TdxRtfRunBackColorExportMode.Chcbpat then
    RtfBuilder.WriteCommand(TdxRtfExportSR.RunBackgroundColor, AIndex)
  else
    if AMode = TdxRtfRunBackColorExportMode.Highlight then
      RtfBuilder.WriteCommand(TdxRtfExportSR.RunBackgroundColor2, AIndex)
    else
    begin
      RtfBuilder.WriteCommand(TdxRtfExportSR.RunBackgroundColor2, AIndex);
      RtfBuilder.WriteCommand(TdxRtfExportSR.RunBackgroundColor, AIndex);
    end;
end;

function TdxRtfCharacterPropertiesExporter.WriteFontName(
  const AFontName: string): Integer;
begin
  Result := RtfExportHelper.GetFontNameIndex(AFontName);
  if Result = RtfExportHelper.DefaultFontIndex then
    Result := -1
  else
    RtfBuilder.WriteCommand(TdxRtfExportSR.FontNumber, Result);
end;

procedure TdxRtfCharacterPropertiesExporter.WriteFontSize(
  ARtfFontSize: Integer);
begin
  if ARtfFontSize = DefaultRtfFontSize then
    Exit;
  RtfBuilder.WriteCommand(TdxRtfExportSR.FontSize, ARtfFontSize);
end;

procedure TdxRtfCharacterPropertiesExporter.WriteFontUnderline(AUnderlineType: TdxUnderlineType);
var
  AKeyword: string;
begin
  if AUnderlineType = TdxUnderlineType.None then
    Exit;

  if AUnderlineType in [Low(TdxUnderlineType)..High(TdxUnderlineType)] then
    AKeyword := DefaultUnderlineTypes[AUnderlineType]
  else
    AKeyword := '';
  if AKeyword = '' then
    AKeyword := DefaultUnderlineTypes[TdxUnderlineType.Single];
  RtfBuilder.WriteCommand(AKeyword);
end;

procedure TdxRtfCharacterPropertiesExporter.WriteForegroundColor(
  AForeColor: TdxAlphaColor; ACheckDefaultColor: Boolean = False);
begin
  if AForeColor = TdxAlphaColors.Transparent then
    AForeColor := TdxAlphaColors.Empty;
  WriteForegroundColorCore(AForeColor, ACheckDefaultColor);
end;

procedure TdxRtfCharacterPropertiesExporter.WriteForegroundColorCore(
  AForeColor: TdxAlphaColor; ACheckDefaultColor: Boolean = False);
var
  AColorIndex: Integer;
begin
  if AForeColor <> TdxAlphaColors.Empty then
    AForeColor := RtfExportHelper.BlendColor(AForeColor);
  AColorIndex := RtfExportHelper.GetColorIndex(AForeColor);
  if not ACheckDefaultColor or (AColorIndex <> 0) then
    RtfBuilder.WriteCommand(TdxRtfExportSR.RunForegroundColor, AColorIndex);
end;

procedure TdxRtfCharacterPropertiesExporter.WriteUnderlineColor(
  AUnderlineColor: TdxAlphaColor);
var
  AIndex: Integer;
begin
  AUnderlineColor := RtfExportHelper.BlendColor(AUnderlineColor);
  AIndex := RtfExportHelper.GetColorIndex(AUnderlineColor);
  RtfBuilder.WriteCommand(TdxRtfExportSR.RunUnderlineColor, AIndex);
end;

{ TdxRtfTablePropertiesExporter }

function TdxRtfTablePropertiesExporter.GetTableTopBorder: string;
begin
  Result := TdxRtfExportSR.TableTopBorder;
end;

function TdxRtfTablePropertiesExporter.GetTableLeftBorder: string;
begin
  Result := TdxRtfExportSR.TableLeftBorder;
end;

function TdxRtfTablePropertiesExporter.GetTableBottomBorder: string;
begin
  Result := TdxRtfExportSR.TableBottomBorder;
end;

function TdxRtfTablePropertiesExporter.GetTableRightBorder: string;
begin
  Result := TdxRtfExportSR.TableRightBorder;
end;

function TdxRtfTablePropertiesExporter.GetTableHorizontalBorder: string;
begin
  Result := TdxRtfExportSR.TableHorizontalBorder;
end;

function TdxRtfTablePropertiesExporter.GetTableVerticalBorder: string;
begin
  Result := TdxRtfExportSR.TableVerticalBorder;
end;

function TdxRtfTablePropertiesExporter.GetTableCellMarginsLeftType: string;
begin
  Result := TdxRtfExportSR.TableCellMarginsLeftType;
end;

function TdxRtfTablePropertiesExporter.GetTableCellMarginsLeft: string;
begin
  Result := TdxRtfExportSR.TableCellMarginsLeft;
end;

function TdxRtfTablePropertiesExporter.GetTableCellMarginsBottomType: string;
begin
  Result := TdxRtfExportSR.TableCellMarginsBottomType;
end;

function TdxRtfTablePropertiesExporter.GetTableCellMarginsBottom: string;
begin
  Result := TdxRtfExportSR.TableCellMarginsBottom;
end;

function TdxRtfTablePropertiesExporter.GetTableCellMarginsRightType: string;
begin
  Result := TdxRtfExportSR.TableCellMarginsRightType;
end;

function TdxRtfTablePropertiesExporter.GetTableCellMarginsRight: string;
begin
  Result := TdxRtfExportSR.TableCellMarginsRight;
end;

function TdxRtfTablePropertiesExporter.GetTableCellMarginsTopType: string;
begin
  Result := TdxRtfExportSR.TableCellMarginsTopType;
end;

function TdxRtfTablePropertiesExporter.GetTableCellMarginsTop: string;
begin
  Result := TdxRtfExportSR.TableCellMarginsTop;
end;

procedure TdxRtfTablePropertiesExporter.WriteTableFloatingPosition(AFloatingPosition: TdxTableFloatingPositionInfo);
var
  ADefaultFloatingPosition: TdxTableFloatingPositionInfo;
begin
  if AFloatingPosition.TextWrapping <> TdxTextWrapping.Around then
    Exit;
  ADefaultFloatingPosition := DocumentModel.Cache.TableFloatingPositionInfoCache.DefaultItem;
  if AFloatingPosition.HorizontalAnchor <> TdxHorizontalAnchorTypes.Column then
    WriteTableHorizontalAnchor(AFloatingPosition.HorizontalAnchor);
  if AFloatingPosition.TableHorizontalPosition <> 0 then
    WriteTableHorizontalPosition(AFloatingPosition.TableHorizontalPosition)
  else
    WriteTableHorizontalPosition(1);
  if AFloatingPosition.HorizontalAlign <> ADefaultFloatingPosition.HorizontalAlign then
    WriteTableHorizontalAlign(AFloatingPosition.HorizontalAlign);
  if AFloatingPosition.VerticalAnchor <> TdxVerticalAnchorTypes.Margin then
    WriteTableVerticalAnchor(AFloatingPosition.VerticalAnchor);
  if AFloatingPosition.TableVerticalPosition <> 0 then
    WriteTableVerticalPosition(AFloatingPosition.TableVerticalPosition)
  else
    WriteTableVerticalPosition(1);
  if AFloatingPosition.VerticalAlign <> ADefaultFloatingPosition.VerticalAlign then
    WriteTableVerticalAlign(AFloatingPosition.VerticalAlign);
  if AFloatingPosition.LeftFromText <> ADefaultFloatingPosition.LeftFromText then
    RtfBuilder.WriteCommand(TdxRtfExportSR.TableRowLeftFromText, UnitConverter.ModelUnitsToTwips(AFloatingPosition.LeftFromText));
  if AFloatingPosition.BottomFromText <> ADefaultFloatingPosition.BottomFromText then
    RtfBuilder.WriteCommand(TdxRtfExportSR.TableRowBottomFromText, UnitConverter.ModelUnitsToTwips(AFloatingPosition.BottomFromText));
  if AFloatingPosition.RightFromText <> ADefaultFloatingPosition.RightFromText then
    RtfBuilder.WriteCommand(TdxRtfExportSR.TableRowRightFromText, UnitConverter.ModelUnitsToTwips(AFloatingPosition.RightFromText));
  if AFloatingPosition.TopFromText <> ADefaultFloatingPosition.TopFromText then
    RtfBuilder.WriteCommand(TdxRtfExportSR.TableRowTopFromText, UnitConverter.ModelUnitsToTwips(AFloatingPosition.TopFromText));
end;

procedure TdxRtfTablePropertiesExporter.WriteTableHorizontalAnchor(AValue: TdxHorizontalAnchorTypes);
begin
  case AValue of
    TdxHorizontalAnchorTypes.Column:
      RtfBuilder.WriteCommand(TdxRtfExportSR.TableRowHorizontalAnchorColumn);
    TdxHorizontalAnchorTypes.Margin:
      RtfBuilder.WriteCommand(TdxRtfExportSR.TableRowHorizontalAnchorMargin);
    TdxHorizontalAnchorTypes.Page:
      RtfBuilder.WriteCommand(TdxRtfExportSR.TableRowHorizontalAnchorPage);
    else
      TdxRichEditExceptions.ThrowInternalException;
  end;
end;

procedure TdxRtfTablePropertiesExporter.WriteTableVerticalAnchor(AValue: TdxVerticalAnchorTypes);
begin
  case AValue of
    TdxVerticalAnchorTypes.Margin:
      RtfBuilder.WriteCommand(TdxRtfExportSR.TableRowVerticalAnchorMargin);
    TdxVerticalAnchorTypes.Page:
      RtfBuilder.WriteCommand(TdxRtfExportSR.TableRowVerticalAnchorPage);
    TdxVerticalAnchorTypes.Paragraph:
      RtfBuilder.WriteCommand(TdxRtfExportSR.TableRowVerticalAnchorParagraph);
    else
      TdxRichEditExceptions.ThrowInternalException;
  end;
end;

procedure TdxRtfTablePropertiesExporter.WriteTableHorizontalAlign(AValue: TdxHorizontalAlignMode);
begin
  case AValue of
    TdxHorizontalAlignMode.Center:
      RtfBuilder.WriteCommand(TdxRtfExportSR.TableRowHorizontalAlignCenter);
    TdxHorizontalAlignMode.Inside:
      RtfBuilder.WriteCommand(TdxRtfExportSR.TableRowHorizontalAlignInside);
    TdxHorizontalAlignMode.Left:
      RtfBuilder.WriteCommand(TdxRtfExportSR.TableRowHorizontalAlignLeft);
    TdxHorizontalAlignMode.Outside:
      RtfBuilder.WriteCommand(TdxRtfExportSR.TableRowHorizontalAlignOutside);
    TdxHorizontalAlignMode.Right:
      RtfBuilder.WriteCommand(TdxRtfExportSR.TableRowHorizontalAlignRight);
    TdxHorizontalAlignMode.None:
      Exit;
    else
      TdxRichEditExceptions.ThrowInternalException;
  end;
end;

procedure TdxRtfTablePropertiesExporter.WriteTableVerticalAlign(AValue: TdxVerticalAlignMode);
begin
  case AValue of
    TdxVerticalAlignMode.Bottom:
      RtfBuilder.WriteCommand(TdxRtfExportSR.TableRowVerticalAlignBottom);
    TdxVerticalAlignMode.Center:
      RtfBuilder.WriteCommand(TdxRtfExportSR.TableRowVerticalAlignCenter);
    TdxVerticalAlignMode.Inline:
      RtfBuilder.WriteCommand(TdxRtfExportSR.TableRowVerticalAlignInline);
    TdxVerticalAlignMode.Inside:
      RtfBuilder.WriteCommand(TdxRtfExportSR.TableRowVerticalAlignInside);
    TdxVerticalAlignMode.Outside:
      RtfBuilder.WriteCommand(TdxRtfExportSR.TableRowVerticalAlignOutside);
    TdxVerticalAlignMode.Top:
      RtfBuilder.WriteCommand(TdxRtfExportSR.TableRowVerticalAlignTop);
    TdxVerticalAlignMode.None:
      Exit;
    else
      TdxRichEditExceptions.ThrowInternalException;
  end;
end;

procedure TdxRtfTablePropertiesExporter.WriteTableHorizontalPosition(AValue: Integer);
begin
  if AValue >= 0 then
    RtfBuilder.WriteCommand(TdxRtfExportSR.TableRowHorizontalPosition, UnitConverter.ModelUnitsToTwips(AValue))
  else
    RtfBuilder.WriteCommand(TdxRtfExportSR.TableRowHorizontalPositionNeg, UnitConverter.ModelUnitsToTwips(AValue));
end;

procedure TdxRtfTablePropertiesExporter.WriteTableVerticalPosition(AValue: Integer);
begin
  if AValue >= 0 then
    RtfBuilder.WriteCommand(TdxRtfExportSR.TableRowVerticalPosition, UnitConverter.ModelUnitsToTwips(AValue))
  else
    RtfBuilder.WriteCommand(TdxRtfExportSR.TableRowVerticalPositionNeg, UnitConverter.ModelUnitsToTwips(AValue));
end;

procedure TdxRtfTablePropertiesExporter.WriteRowLeft(ALeft: Integer);
begin
  RtfBuilder.WriteCommand(TdxRtfExportSR.TableRowLeft, UnitConverter.ModelUnitsToTwips(ALeft));
end;

procedure TdxRtfTablePropertiesExporter.WriteTableBorders(ATopBorder, ALeftBorder,
  ABottomBorder, ARightBorder, AInnerHorizontalBorder, AInnerVerticalBorder: TdxBorderInfo);
var
  ADefaultBorder: TdxBorderInfo;
begin
  ADefaultBorder := DocumentModel.Cache.BorderInfoCache.DefaultItem;
  if not ATopBorder.Equals(ADefaultBorder) then
  begin
    RtfBuilder.WriteCommand(TableTopBorder);
    WriteBorderProperties(ATopBorder);
  end;
  if not ALeftBorder.Equals(ADefaultBorder) then
  begin
    RtfBuilder.WriteCommand(TableLeftBorder);
    WriteBorderProperties(ALeftBorder);
  end;
  if not ABottomBorder.Equals(ADefaultBorder) then
  begin
    RtfBuilder.WriteCommand(TableBottomBorder);
    WriteBorderProperties(ABottomBorder);
  end;
  if not ARightBorder.Equals(ADefaultBorder) then
  begin
    RtfBuilder.WriteCommand(TableRightBorder);
    WriteBorderProperties(ARightBorder);
  end;
  if not AInnerHorizontalBorder.Equals(ADefaultBorder) then
  begin
    RtfBuilder.WriteCommand(TableHorizontalBorder);
    WriteBorderProperties(AInnerHorizontalBorder);
  end;
  if not AInnerVerticalBorder.Equals(ADefaultBorder) then
  begin
    RtfBuilder.WriteCommand(TableVerticalBorder);
    WriteBorderProperties(AInnerVerticalBorder);
  end;
end;

procedure TdxRtfTablePropertiesExporter.WriteTableWidth(APreferredWidth: TdxWidthUnitInfo);
begin
  WriteWidthUnit(APreferredWidth, TdxRtfExportSR.TablePreferredWidthType, TdxRtfExportSR.TablePreferredWidth);
end;

procedure TdxRtfTablePropertiesExporter.WriteTableLayout(AValue: TdxTableLayoutType);
begin
  if AValue <> TdxTableLayoutType.Fixed then
    RtfBuilder.WriteCommand(TdxRtfExportSR.TableLayout, Ord(AValue));
end;

procedure TdxRtfTablePropertiesExporter.WriteTableCellMargins(ALeftMargin: TdxWidthUnitInfo; ARightMargin: TdxWidthUnitInfo; ABottomMargin: TdxWidthUnitInfo; ATopMargin: TdxWidthUnitInfo);
begin
  if ShouldExportCellMargin(ALeftMargin) then
    WriteWidthUnitInTwips(ALeftMargin, TableCellMarginsLeftType, TableCellMarginsLeft);
  if ShouldExportCellMargin(ABottomMargin) then
    WriteWidthUnitInTwips(ABottomMargin, TableCellMarginsBottomType, TableCellMarginsBottom);
  if ShouldExportCellMargin(ARightMargin) then
    WriteWidthUnitInTwips(ARightMargin, TableCellMarginsRightType, TableCellMarginsRight);
  if ShouldExportCellMargin(ATopMargin) then
    WriteWidthUnitInTwips(ATopMargin, TableCellMarginsTopType, TableCellMarginsTop);
end;

procedure TdxRtfTablePropertiesExporter.WriteTableLook(AValue: TdxTableLookTypes);
var
  ADefaultGeneralSettings: TdxTableGeneralSettingsInfo;
begin
  ADefaultGeneralSettings := DocumentModel.Cache.TableGeneralSettingsInfoCache.DefaultItem;
  if AValue = ADefaultGeneralSettings.TableLook then
    Exit;
  if TdxTableLookType.ApplyFirstColumn in AValue then
    RtfBuilder.WriteCommand(TdxRtfExportSR.TableApplyFirstColumn);
  if TdxTableLookType.ApplyFirstRow in AValue then
    RtfBuilder.WriteCommand(TdxRtfExportSR.TableApplyFirstRow);
  if TdxTableLookType.ApplyLastColumn in AValue then
    RtfBuilder.WriteCommand(TdxRtfExportSR.TableApplyLastColumn);
  if TdxTableLookType.ApplyLastRow in AValue then
    RtfBuilder.WriteCommand(TdxRtfExportSR.TableApplyLastRow);
  if TdxTableLookType.DoNotApplyColumnBanding in AValue then
    RtfBuilder.WriteCommand(TdxRtfExportSR.TableDoNotApplyColumnBanding);
  if TdxTableLookType.DoNotApplyRowBanding in AValue then
    RtfBuilder.WriteCommand(TdxRtfExportSR.TableDoNotApplyRowBanding);
end;

procedure TdxRtfTablePropertiesExporter.WriteTableIndent(ATableIndent: TdxWidthUnitInfo);
begin
  WriteWidthUnit(ATableIndent, TdxRtfExportSR.TableIndentType, TdxRtfExportSR.TableIndent, True);
end;

procedure TdxRtfTablePropertiesExporter.WriteBandSizes(AInfo: TdxTableGeneralSettingsInfo; AExportRowBand: Boolean; AExportColBand: Boolean);
begin
  if (AInfo.TableStyleRowBandSize <> 0) and (AExportRowBand or (AInfo.TableStyleRowBandSize > 1)) then
    RtfBuilder.WriteCommand(TdxRtfExportSR.TableStyleRowBandSize, AInfo.TableStyleRowBandSize);
  if (AInfo.TableStyleColBandSize <> 0) and (AExportColBand or (AInfo.TableStyleColBandSize > 1)) then
    RtfBuilder.WriteCommand(TdxRtfExportSR.TableStyleColumnBandSize, AInfo.TableStyleColBandSize);
end;

{ TdxRtfTableStyleTablePropertiesExporter }

function TdxRtfTableStyleTablePropertiesExporter.GetTableStyleRowBandSize: string;
begin
  Result := TdxRtfExportSR.TableStyleRowBandSize;
end;

function TdxRtfTableStyleTablePropertiesExporter.GetTableStyleColumnBandSize: string;
begin
  Result := TdxRtfExportSR.TableStyleColumnBandSize;
end;

function TdxRtfTableStyleTablePropertiesExporter.GetTableCellMarginsLeftType: string;
begin
  Result := TdxRtfExportSR.TableStyleTableLeftCellMarginUnitType;
end;

function TdxRtfTableStyleTablePropertiesExporter.GetTableCellMarginsLeft: string;
begin
  Result := TdxRtfExportSR.TableStyleTableLeftCellMargin;
end;

function TdxRtfTableStyleTablePropertiesExporter.GetTableCellMarginsBottomType: string;
begin
  Result := TdxRtfExportSR.TableStyleTableBottomCellMarginUnitType;
end;

function TdxRtfTableStyleTablePropertiesExporter.GetTableCellMarginsBottom: string;
begin
  Result := TdxRtfExportSR.TableStyleTableBottomCellMargin;
end;

function TdxRtfTableStyleTablePropertiesExporter.GetTableCellMarginsRightType: string;
begin
  Result := TdxRtfExportSR.TableStyleTableRightCellMarginUnitType;
end;

function TdxRtfTableStyleTablePropertiesExporter.GetTableCellMarginsRight: string;
begin
  Result := TdxRtfExportSR.TableStyleTableRightCellMargin;
end;

function TdxRtfTableStyleTablePropertiesExporter.GetTableCellMarginsTopType: string;
begin
  Result := TdxRtfExportSR.TableStyleTableTopCellMarginUnitType;
end;

function TdxRtfTableStyleTablePropertiesExporter.GetTableCellMarginsTop: string;
begin
  Result := TdxRtfExportSR.TableStyleTableTopCellMargin;
end;

procedure TdxRtfTableStyleTablePropertiesExporter.ExportTableProperties(AMergedTableProperties: TdxMergedTableProperties; AExportRowBand: Boolean; AExportColBand: Boolean);
var
  AInfo: TdxCombinedTablePropertiesInfo;
begin
  AInfo := AMergedTableProperties.Info;
  WriteBandSizes(AInfo.GeneralSettings, AExportRowBand, AExportColBand);
  WriteTableBorders(AInfo.Borders.TopBorder, AInfo.Borders.LeftBorder, AInfo.Borders.BottomBorder, AInfo.Borders.RightBorder, AInfo.Borders.InsideHorizontalBorder, AInfo.Borders.InsideVerticalBorder);
  WriteTableFloatingPosition(AInfo.FloatingPosition);
  WriteTableWidth(AInfo.PreferredWidth);
  WriteTableCellMargins(AInfo.CellMargins.Left, AInfo.CellMargins.Right, AInfo.CellMargins.Bottom, AInfo.CellMargins.Top);
  WriteTableLook(AInfo.GeneralSettings.TableLook);
  WriteTableIndent(AInfo.TableIndent);
end;

{ TdxRtfTableRowPropertiesExporter }

procedure TdxRtfTableRowPropertiesExporter.WriteLastRowMark;
begin
  RtfBuilder.WriteCommand(TdxRtfExportSR.TableLastRow);
end;

procedure TdxRtfTableRowPropertiesExporter.WriteHalfSpaceBetweenCells(AVal: Integer);
begin
  if AVal > 0 then
    RtfBuilder.WriteCommand(TdxRtfExportSR.TableHalfSpaceBetweenCells, AVal);
end;

procedure TdxRtfTableRowPropertiesExporter.WriteRowAlignment(AValue: TdxTableRowAlignment);
var
  ADefaultGeneralSettings: TdxTableRowGeneralSettingsInfo;
begin
  ADefaultGeneralSettings := DocumentModel.Cache.TableRowGeneralSettingsInfoCache.DefaultItem;
  if AValue = ADefaultGeneralSettings.TableRowAlignment then
    Exit;
  case AValue of
    TdxTableRowAlignment.Center:
      RtfBuilder.WriteCommand(TdxRtfExportSR.TableRowCenterAlignment);
    TdxTableRowAlignment.Left:
      RtfBuilder.WriteCommand(TdxRtfExportSR.TableRowLeftAlignment);
    TdxTableRowAlignment.Right:
      RtfBuilder.WriteCommand(TdxRtfExportSR.TableRowRightAlignment);
  end;
end;

procedure TdxRtfTableRowPropertiesExporter.WriteRowHeader(AHeader: Boolean);
var
  ADefaultGeneralSettings: TdxTableRowGeneralSettingsInfo;
begin
  ADefaultGeneralSettings := DocumentModel.Cache.TableRowGeneralSettingsInfoCache.DefaultItem;
  if AHeader <> ADefaultGeneralSettings.Header then
    WriteBoolCommand(TdxRtfExportSR.TableRowHeader, AHeader);
end;

procedure TdxRtfTableRowPropertiesExporter.WriteRowCantSplit(ACantSplit: Boolean);
var
  ADefaultGeneralSettings: TdxTableRowGeneralSettingsInfo;
begin
  ADefaultGeneralSettings := DocumentModel.Cache.TableRowGeneralSettingsInfoCache.DefaultItem;
  if ACantSplit <> ADefaultGeneralSettings.CantSplit then
    WriteBoolCommand(TdxRtfExportSR.TableRowCantSplit, ACantSplit);
end;

procedure TdxRtfTableRowPropertiesExporter.WriteRowHeight(AHeight: TdxHeightUnitInfo);
var
  ADefaultHeight: TdxHeightUnitInfo;
begin
  ADefaultHeight := DocumentModel.Cache.HeightUnitInfoCache.DefaultItem;
  if AHeight.&Type = ADefaultHeight.&Type then
    Exit;
  case AHeight.&Type of
    TdxHeightUnitType.Auto:
      RtfBuilder.WriteCommand(TdxRtfExportSR.TableRowHeight, 0);
    TdxHeightUnitType.Minimum:
      RtfBuilder.WriteCommand(TdxRtfExportSR.TableRowHeight, UnitConverter.ModelUnitsToTwips(AHeight.Value));
    TdxHeightUnitType.Exact:
      RtfBuilder.WriteCommand(TdxRtfExportSR.TableRowHeight, -UnitConverter.ModelUnitsToTwips(AHeight.Value));
  end;
end;

procedure TdxRtfTableRowPropertiesExporter.WriteWidthBefore(AWidthBefore: TdxWidthUnitInfo);
begin
  WriteWidthUnit(AWidthBefore, TdxRtfExportSR.TableRowWidthBeforeType, TdxRtfExportSR.TableRowWidthBefore);
end;

procedure TdxRtfTableRowPropertiesExporter.WriteWidthAfter(AWidthAfter: TdxWidthUnitInfo);
begin
  WriteWidthUnit(AWidthAfter, TdxRtfExportSR.TableRowWidthAfterType, TdxRtfExportSR.TableRowWidthAfter);
end;

procedure TdxRtfTableRowPropertiesExporter.WriteRowCellSpacing(ACellSpacing: TdxWidthUnitInfo);
begin
  WriteWidthUnitInTwips(ACellSpacing, TdxRtfExportSR.TableCellSpacingLeftType, TdxRtfExportSR.TableCellSpacingLeft);
  WriteWidthUnitInTwips(ACellSpacing, TdxRtfExportSR.TableCellSpacingBottomType, TdxRtfExportSR.TableCellSpacingBottom);
  WriteWidthUnitInTwips(ACellSpacing, TdxRtfExportSR.TableCellSpacingRightType, TdxRtfExportSR.TableCellSpacingRight);
  WriteWidthUnitInTwips(ACellSpacing, TdxRtfExportSR.TableCellSpacingTopType, TdxRtfExportSR.TableCellSpacingTop);
end;

{ TdxRtfTableCellPropertiesExporter }

function TdxRtfTableCellPropertiesExporter.GetTableCellBackgroundColor: string;
begin
  Result := TdxRtfExportSR.TableCellBackgroundColor;
end;

function TdxRtfTableCellPropertiesExporter.GetTableCellForegroundColor: string;
begin
  Result := TdxRtfExportSR.TableCellForegroundColor;
end;

function TdxRtfTableCellPropertiesExporter.GetTableCellShading: string;
begin
  Result := TdxRtfExportSR.TableCellShading;
end;

function TdxRtfTableCellPropertiesExporter.GetTableCellNoWrap: string;
begin
  Result := TdxRtfExportSR.TableCellNoWrap;
end;

function TdxRtfTableCellPropertiesExporter.GetTableCellTextTopAlignment: string;
begin
  Result := TdxRtfExportSR.TableCellTextTopAlignment;
end;

function TdxRtfTableCellPropertiesExporter.GetTableCellTextCenterAlignment: string;
begin
  Result := TdxRtfExportSR.TableCellTextCenterAlignment;
end;

function TdxRtfTableCellPropertiesExporter.GetTableCellTextBottomAlignment: string;
begin
  Result := TdxRtfExportSR.TableCellTextBottomAlignment;
end;

function TdxRtfTableCellPropertiesExporter.GetTableCellUpperLeftToLowerRightBorder: string;
begin
  Result := TdxRtfExportSR.TableCellUpperLeftToLowerRightBorder;
end;

function TdxRtfTableCellPropertiesExporter.GetTableCellUpperRightToLowerLeftBorder: string;
begin
  Result := TdxRtfExportSR.TableCellUpperRightToLowerLeftBorder;
end;

function TdxRtfTableCellPropertiesExporter.GetCellTopBorder: string;
begin
  Result := TdxRtfExportSR.TableCellTopBorder;
end;

function TdxRtfTableCellPropertiesExporter.GetCellLeftBorder: string;
begin
  Result := TdxRtfExportSR.TableCellLeftBorder;
end;

function TdxRtfTableCellPropertiesExporter.GetCellBottomBorder: string;
begin
  Result := TdxRtfExportSR.TableCellBottomBorder;
end;

function TdxRtfTableCellPropertiesExporter.GetCellRightBorder: string;
begin
  Result := TdxRtfExportSR.TableCellRightBorder;
end;

procedure TdxRtfTableCellPropertiesExporter.WriteCellBackgroundColor(ACell: TdxTableCell);
begin
  WriteCellBackgroundColor(ACell.BackgroundColor, ACell.Table);
end;

procedure TdxRtfTableCellPropertiesExporter.WriteCellBackgroundColor(AColor: TdxAlphaColor; ATable: TdxTable);
var
  ADefaultGeneralSetting: TdxTableCellGeneralSettingsInfo;
  AColorIndex: Integer;
begin
  ADefaultGeneralSetting := DocumentModel.Cache.TableCellGeneralSettingsInfoCache.DefaultItem;
  if (AColor <> ADefaultGeneralSetting.BackgroundColor) and not TdxAlphaColors.IsTransparentOrEmpty(AColor) then
    AColor := RtfExportHelper.BlendColor(AColor)
  else
    if (ATable <> nil) and (ATable.BackgroundColor <> ADefaultGeneralSetting.BackgroundColor) and
        not TdxAlphaColors.IsTransparentOrEmpty(ATable.BackgroundColor) then
      AColor := RtfExportHelper.BlendColor(ATable.BackgroundColor)
    else
      Exit;
  AColorIndex := RtfExportHelper.GetColorIndex(AColor);
  RtfBuilder.WriteCommand(TableCellBackgroundColor, AColorIndex);
end;

procedure TdxRtfTableCellPropertiesExporter.WriteCellFitText(AFitText: Boolean);
var
  ADefaultGeneralSetting: TdxTableCellGeneralSettingsInfo;
begin
  ADefaultGeneralSetting := DocumentModel.Cache.TableCellGeneralSettingsInfoCache.DefaultItem;
  if AFitText <> ADefaultGeneralSetting.FitText then
    WriteBoolCommand(TdxRtfExportSR.TableCellFitText, AFitText);
end;

procedure TdxRtfTableCellPropertiesExporter.WriteCellNoWrap(ANoWrap: Boolean);
var
  ADefaultGeneralSetting: TdxTableCellGeneralSettingsInfo;
begin
  ADefaultGeneralSetting := DocumentModel.Cache.TableCellGeneralSettingsInfoCache.DefaultItem;
  if ANoWrap <> ADefaultGeneralSetting.NoWrap then
    WriteBoolCommand(TableCellNoWrap, ANoWrap);
end;

procedure TdxRtfTableCellPropertiesExporter.WriteCellHideCellMark(AHideCellMark: Boolean);
var
  ADefaultGeneralSetting: TdxTableCellGeneralSettingsInfo;
begin
  ADefaultGeneralSetting := DocumentModel.Cache.TableCellGeneralSettingsInfoCache.DefaultItem;
  if AHideCellMark <> ADefaultGeneralSetting.HideCellMark then
    WriteBoolCommand(TdxRtfExportSR.TableCellHideMark, AHideCellMark);
end;

procedure TdxRtfTableCellPropertiesExporter.WriteCellMerging(AMergingState: TdxMergingState);
var
  ADefaultGeneralSetting: TdxTableCellGeneralSettingsInfo;
begin
  ADefaultGeneralSetting := DocumentModel.Cache.TableCellGeneralSettingsInfoCache.DefaultItem;
  WriteCellVerticalMerging(AMergingState, ADefaultGeneralSetting.VerticalMerging);
end;

procedure TdxRtfTableCellPropertiesExporter.WriteCellVerticalMerging(AValue: TdxMergingState; ADefaultValue: TdxMergingState);
begin
  if AValue = ADefaultValue then
    Exit;
  if AValue = TdxMergingState.Restart then
    RtfBuilder.WriteCommand(TdxRtfExportSR.TableCellStartVerticalMerging)
  else
    if AValue = TdxMergingState.Continue then
      RtfBuilder.WriteCommand(TdxRtfExportSR.TableCellContinueVerticalMerging);
end;

procedure TdxRtfTableCellPropertiesExporter.WriteCellVerticalAlignment(AVerticalAlignment: TdxVerticalAlignment);
begin
  case AVerticalAlignment of
    TdxVerticalAlignment.Top:
      RtfBuilder.WriteCommand(TableCellTextTopAlignment);
    TdxVerticalAlignment.Center:
      RtfBuilder.WriteCommand(TableCellTextCenterAlignment);
    TdxVerticalAlignment.Bottom:
      RtfBuilder.WriteCommand(TableCellTextBottomAlignment);
    else
      TdxRichEditExceptions.ThrowInternalException;
  end;
end;

procedure TdxRtfTableCellPropertiesExporter.WriteCellTextDirection(AValue: TdxTextDirection);
begin
  case AValue of
    TdxTextDirection.LeftToRightTopToBottom:
      RtfBuilder.WriteCommand(TdxRtfExportSR.TableCellLeftToRightTopToBottomTextDirection);
    TdxTextDirection.TopToBottomRightToLeft:
      RtfBuilder.WriteCommand(TdxRtfExportSR.TableCellTopToBottomRightToLeftTextDirection);
    TdxTextDirection.BottomToTopLeftToRight:
      RtfBuilder.WriteCommand(TdxRtfExportSR.TableCellBottomToTopLeftToRightTextDirection);
    TdxTextDirection.LeftToRightTopToBottomRotated:
      RtfBuilder.WriteCommand(TdxRtfExportSR.TableCellLeftToRightTopToBottomVerticalTextDirection);
    TdxTextDirection.TopToBottomRightToLeftRotated:
      RtfBuilder.WriteCommand(TdxRtfExportSR.TableCellTopToBottomRightToLeftVerticalTextDirection);
  end;
end;

procedure TdxRtfTableCellPropertiesExporter.WriteCellBasicBorders(ATopBorder, ALeftBorder,
  ARightBorder, ABottomBorder: TdxBorderInfo);
begin
  RtfBuilder.WriteCommand(CellTopBorder);
  WriteBorderProperties(ATopBorder);
  RtfBuilder.WriteCommand(CellLeftBorder);
  WriteBorderProperties(ALeftBorder);
  RtfBuilder.WriteCommand(CellBottomBorder);
  WriteBorderProperties(ABottomBorder);
  RtfBuilder.WriteCommand(CellRightBorder);
  WriteBorderProperties(ARightBorder);
end;

procedure TdxRtfTableCellPropertiesExporter.WriteCellPreferredWidth(APreferredWidth: TdxWidthUnitInfo);
begin
  WriteWidthUnit(APreferredWidth, TdxRtfExportSR.TableCellPreferredWidthType, TdxRtfExportSR.TableCellPreferredWidth);
end;

procedure TdxRtfTableCellPropertiesExporter.WriteCellMargins(ACell: TdxTableCell);
begin
  WriteCellMargins(ACell.GetActualTopMargin.Info, ACell.GetActualLeftMargin.Info, ACell.GetActualRightMargin.Info, ACell.GetActualBottomMargin.Info);
end;

procedure TdxRtfTableCellPropertiesExporter.WriteCellMargins(ATopMargin, ALeftMargin,
  ARightMargin, ABottomMargin: TdxWidthUnitInfo);
begin
  if ShouldExportCellMargin(ABottomMargin) then
    WriteWidthUnitInTwips(ABottomMargin, TdxRtfExportSR.TableCellBottomMarginType, TdxRtfExportSR.TableCellBottomMargin);
  if ShouldExportCellMargin(ATopMargin) then
    WriteWidthUnitInTwips(ATopMargin, TdxRtfExportSR.TableCellLeftMarginType, TdxRtfExportSR.TableCellLeftMargin);
  if ShouldExportCellMargin(ARightMargin) then
    WriteWidthUnitInTwips(ARightMargin, TdxRtfExportSR.TableCellRightMarginType, TdxRtfExportSR.TableCellRightMargin);
  if ShouldExportCellMargin(ALeftMargin) then
    WriteWidthUnitInTwips(ALeftMargin, TdxRtfExportSR.TableCellTopMarginType, TdxRtfExportSR.TableCellTopMargin);
end;

procedure TdxRtfTableCellPropertiesExporter.WriteCellRight(ACellRight: Integer);
begin
  RtfBuilder.WriteCommand(TdxRtfExportSR.TableCellRight, UnitConverter.ModelUnitsToTwips(ACellRight));
end;

procedure TdxRtfTableCellPropertiesExporter.WriteCellForegroundColor(ACell: TdxTableCell);
begin
  WriteCellForegroundColor(ACell.ForegroundColor, ACell.Table);
end;

procedure TdxRtfTableCellPropertiesExporter.WriteCellForegroundColor(AColor: TdxAlphaColor);
begin
  WriteCellBackgroundColor(AColor, nil);
end;

procedure TdxRtfTableCellPropertiesExporter.WriteCellForegroundColor(AColor: TdxAlphaColor; ATable: TdxTable);
var
  ADefaultGeneralSetting: TdxTableCellGeneralSettingsInfo;
  AIndex: Integer;
begin
  ADefaultGeneralSetting := DocumentModel.Cache.TableCellGeneralSettingsInfoCache.DefaultItem;
  if (AColor <> ADefaultGeneralSetting.ForegroundColor) and
      (AColor <> TdxAlphaColors.Empty) and (AColor <> TdxAlphaColors.Transparent) then
    AColor := RtfExportHelper.BlendColor(AColor)
  else
    Exit;
  AIndex := RtfExportHelper.GetColorIndex(AColor);
  RtfBuilder.WriteCommand(TableCellForegroundColor, AIndex);
end;

procedure TdxRtfTableCellPropertiesExporter.WriteCellShading(ACell: TdxTableCell);
begin
  WriteCellShading(ACell.Shading);
end;

procedure TdxRtfTableCellPropertiesExporter.WriteCellShading(AShading: TdxShadingPattern);
var
  ADefaultGeneralSetting: TdxTableCellGeneralSettingsInfo;
  AValue: Integer;
begin
  ADefaultGeneralSetting := DocumentModel.Cache.TableCellGeneralSettingsInfoCache.DefaultItem;
  if AShading = ADefaultGeneralSetting.Shading then
    Exit;
  AValue := TdxDestinationPieceTable.GetShadingValue(AShading);
  if AValue = 0 then
    Exit;
  RtfBuilder.WriteCommand(TableCellShading, AValue);
end;

{ TdxRtfTableStyleTableCellPropertiesExporter }

function TdxRtfTableStyleTableCellPropertiesExporter.GetTableCellBackgroundColor: string;
begin
  Result := TdxRtfExportSR.TableStyleCellBackgroundColor;
end;

function TdxRtfTableStyleTableCellPropertiesExporter.GetTableCellNoWrap: string;
begin
  Result := TdxRtfExportSR.TableStyleCellNoWrap;
end;

function TdxRtfTableStyleTableCellPropertiesExporter.GetTableCellTextTopAlignment: string;
begin
  Result := TdxRtfExportSR.TableStyleCellVerticalAlignmentTop;
end;

function TdxRtfTableStyleTableCellPropertiesExporter.GetTableCellTextCenterAlignment: string;
begin
  Result := TdxRtfExportSR.TableStyleCellVerticalAlignmentCenter;
end;

function TdxRtfTableStyleTableCellPropertiesExporter.GetTableCellTextBottomAlignment: string;
begin
  Result := TdxRtfExportSR.TableStyleCellVerticalAlignmentBottom;
end;

function TdxRtfTableStyleTableCellPropertiesExporter.GetTableCellUpperLeftToLowerRightBorder: string;
begin
  Result := TdxRtfExportSR.TableStyleUpperLeftToLowerRightBorder;
end;

function TdxRtfTableStyleTableCellPropertiesExporter.GetTableCellUpperRightToLowerLeftBorder: string;
begin
  Result := TdxRtfExportSR.TableStyleUpperRightToLowerLeftBorder;
end;

function TdxRtfTableStyleTableCellPropertiesExporter.GetCellTopBorder: string;
begin
  Result := TdxRtfExportSR.TableStyleTopCellBorder;
end;

function TdxRtfTableStyleTableCellPropertiesExporter.GetCellLeftBorder: string;
begin
  Result := TdxRtfExportSR.TableStyleLeftCellBorder;
end;

function TdxRtfTableStyleTableCellPropertiesExporter.GetCellBottomBorder: string;
begin
  Result := TdxRtfExportSR.TableStyleBottomCellBorder;
end;

function TdxRtfTableStyleTableCellPropertiesExporter.GetCellRightBorder: string;
begin
  Result := TdxRtfExportSR.TableStyleRightCellBorder;
end;

procedure TdxRtfTableStyleTableCellPropertiesExporter.WriteCellBasicBorders(ATopBorder: TdxBorderInfo; ALeftBorder: TdxBorderInfo; ARightBorder: TdxBorderInfo; ABottomBorder: TdxBorderInfo);
var
  ADefaultBorder: TdxBorderInfo;
begin
  ADefaultBorder := DocumentModel.Cache.BorderInfoCache.DefaultItem;
  if not ATopBorder.Equals(ADefaultBorder) then
  begin
    RtfBuilder.WriteCommand(CellTopBorder);
    WriteBorderProperties(ATopBorder);
  end;
  if not ALeftBorder.Equals(ADefaultBorder) then
  begin
    RtfBuilder.WriteCommand(CellLeftBorder);
    WriteBorderProperties(ALeftBorder);
  end;
  if not ABottomBorder.Equals(ADefaultBorder) then
  begin
    RtfBuilder.WriteCommand(CellBottomBorder);
    WriteBorderProperties(ABottomBorder);
  end;
  if not ARightBorder.Equals(ADefaultBorder) then
  begin
    RtfBuilder.WriteCommand(CellRightBorder);
    WriteBorderProperties(ARightBorder);
  end;
end;

{ TdxRtfBuilder }

constructor TdxRtfBuilder.Create(AEncoding: TEncoding);
begin
  inherited Create;
  FEncoding := AEncoding;
  FRtfContent := TdxChunkedStringBuilder.Create;
  FUnicodeTextBuilder := TStringBuilder.Create;
end;

destructor TdxRtfBuilder.Destroy;
begin
  FreeAndNil(FRtfContent);
  FreeAndNil(FUnicodeTextBuilder);
  inherited Destroy;
end;

class constructor TdxRtfBuilder.Initialize;
begin
  FSpecialMarks := CreateSpecialMarksTable;
  FSpecialPCDataMarks := CreateSpecialPCDataMarksTable;
end;

class destructor TdxRtfBuilder.Finalize;
begin
  FreeAndNil(FSpecialPCDataMarks);
  FreeAndNil(FSpecialMarks);
end;

function TdxRtfBuilder.GetBoolParameterValue(AValue: Boolean): string;
begin
  if AValue then
    Result := '1'
  else
    Result := '0'
end;

function TdxRtfBuilder.GetUnicodeCompatibleString(ACh: Char): string;
var
  ACode: SmallInt;
begin
  FUnicodeTextBuilder.Length := 0;
  ACode := SmallInt(ACh);
  if (ACode >= 0) and (ACode <= 127) then
  begin
    if IsSpecialSymbol(ACh) then
      FUnicodeTextBuilder.Append('\');
    FUnicodeTextBuilder.Append(ACh);
  end
  else
    AppendUnicodeCompatibleCharCore(ACode, ACh);
  Result := FUnicodeTextBuilder.ToString;
end;

function TdxRtfBuilder.GetUnicodeCompatibleStringDirect(
  const AText: string): string;
var
  ACh: Char;
  I, ALength, ACode: Integer;
begin
  FUnicodeTextBuilder.Length := 0;
  ALength := Length(AText);
  for I := 1 to ALength do
  begin
    ACh := AText[I];
    ACode := Ord(ACh);
    if (ACode >= 0) and (ACode <= 127) then
      FUnicodeTextBuilder.Append(ACh)
    else
      AppendUnicodeCompatibleCharCore(ACode, ACh);
  end;
  Result := FUnicodeTextBuilder.ToString;
end;

class function TdxRtfBuilder.CreateSpecialMarksTable: TdxCharStringDictionary;
begin
  Result := TdxCharStringDictionary.Create;
  Result.Add(TdxCharacters.EmSpace, '\u8195\''3f');
  Result.Add(TdxCharacters.EnSpace, '\u8194\''3f');
  Result.Add(TdxCharacters.Hyphen, EmptyStr);
  Result.Add(TdxCharacters.LineBreak, '\line ');
  Result.Add(TdxCharacters.PageBreak, '\page ');
  Result.Add(TdxCharacters.ColumnBreak, '\column ');
  Result.Add(TdxCharacters.NonBreakingSpace, '\~');
  Result.Add(TdxCharacters.QmSpace, '\u8197\''3f');
  Result.Add(TdxCharacters.TabMark, '\tab ');
end;

class function TdxRtfBuilder.CreateSpecialPCDataMarksTable: TdxCharStringDictionary;
var
  I: Integer;
begin
  Result := TdxCharStringDictionary.Create;
  for I := 0 to 30 do
    AddHexToMarkTable(Result, Char(I));
  AddHexToMarkTable(result, '\');
  AddHexToMarkTable(result, '{');
  AddHexToMarkTable(result, '}');
end;

procedure TdxRtfBuilder.IncreaseRowLength(ADelta: Integer);
var
  ACLRF: string;
begin
  FRowLength := FRowLength + ADelta;
  if (FRowLength >= RowLengthBound) and FHexMode then
  begin
    ACLRF := TdxRtfExportSR.CLRF;
    RtfContent.Append(ACLRF);
    FIsPreviousWriteCommand := False;
    FRowLength := 0;
  end;
end;

function TdxRtfBuilder.IsSpecialSymbol(ACh: Char): Boolean;
begin
  Result := CharInSet(ACh, ['{', '}', '\']);
end;

procedure TdxRtfBuilder.OpenGroup;
begin
  RtfContent.Append(TdxRtfExportSR.OpenGroup);
  FIsPreviousWriteCommand := False;
  IncreaseRowLength(Length(TdxRtfExportSR.OpenGroup));
end;

function TdxRtfBuilder.RowLengthBound: Integer;
begin
  Result := 200;
end;

function TdxRtfBuilder.ExtractRtfContent: TdxChunkedStringBuilder;
begin
  Result := RtfContent;
  FRtfContent := nil;
end;

procedure TdxRtfBuilder.AppendUnicodeCompatibleCharCore(ACode: Integer;
  ACh: Char);
var
  ABytes: TBytes;
begin
  ABytes := Encoding.GetBytes(ACh);
  FUnicodeTextBuilder.AppendFormat('\u%d\''%s', [ACode, TdxStringHelper.ToHex(ABytes[0])]);
end;

procedure TdxRtfBuilder.Clear;
begin
  FRtfContent.Clear;
end;

procedure TdxRtfBuilder.CloseGroup;
begin
  RtfContent.Append(TdxRtfExportSR.CloseGroup);
  FIsPreviousWriteCommand := False;
  IncreaseRowLength(Length(TdxRtfExportSR.CloseGroup));
end;

procedure TdxRtfBuilder.WriteCommand(const ACommand: string);
begin
  RtfContent.Append(ACommand);
  FIsPreviousWriteCommand := True;
  IncreaseRowLength(Length(ACommand));
end;

procedure TdxRtfBuilder.WriteCommand(const ACommand: string; AParam: Integer);
begin
  RtfContent.Append(ACommand);
  RtfContent.Append(AParam);
  FIsPreviousWriteCommand := true;
  IncreaseRowLength(Length(ACommand));
end;

procedure TdxRtfBuilder.WriteByteArrayAsHex(ABuffer: TBytes; AOffset: Integer = 0;
  ALength: Integer = -1);
var
  ACount: Integer;
  I: Integer;
  AValue: Byte;
begin
  if ALength = -1 then
    ALength := Length(ABuffer);

  if FIsPreviousWriteCommand then
    FRtfContent.Append(TdxRtfExportSR.Space);
  FHexMode := True;
  ACount := AOffset + ALength;
  for I := AOffset to ACount - 1 do
  begin
    AValue := ABuffer[I];
    IncreaseRowLength(2);
    RtfContent.Append(TdxStringHelper.ToHex(AValue));
  end;
  FIsPreviousWriteCommand := True;
  FHexMode := False;
end;

procedure TdxRtfBuilder.WriteChar(ACh: Char);
var
  AStr: string;
begin
  if FIsPreviousWriteCommand then
    RtfContent.Append(TdxRtfExportSR.Space);
  AStr := GetUnicodeCompatibleString(ACh);
  RtfContent.Append(AStr);
  FIsPreviousWriteCommand := False;
  IncreaseRowLength(Length(AStr));
end;

procedure TdxRtfBuilder.WriteCommand(const ACommand, AParam: string);
begin
  FRtfContent.Append(ACommand);
  FRtfContent.Append(AParam);
  FIsPreviousWriteCommand := True;
  IncreaseRowLength(Length(ACommand));
end;

procedure TdxRtfBuilder.WriteMemoryStreamAsHex(AStream: TBytesStream);
begin
  if AStream.Size > 0 then
    WriteByteArrayAsHex(AStream.Bytes, 0, AStream.Size)
  else
    WriteStreamAsHexCore(AStream);
end;

procedure TdxRtfBuilder.WritePCData(const AText: string);
begin
  WriteTextCore(AText, FSpecialPCDataMarks);
end;

procedure TdxRtfBuilder.WriteText(const AText: string);
begin
  WriteTextCore(AText, FSpecialMarks);
end;

procedure TdxRtfBuilder.WriteTextCore(const AText: string;
  ASpecialMarks: TdxCharStringDictionary);
var
  I: Integer;
  ACh: Char;
  ASpecialMark: string;
begin
  for I := 1 to Length(AText) do
  begin
    ACh := AText[I];
    if ASpecialMarks.TryGetValue(ACh, ASpecialMark) then
      WriteTextDirect(ASpecialMark)
    else
      WriteChar(ACh);
  end;
end;

procedure TdxRtfBuilder.WriteShapeBoolProperty(const APropertyName: string;
  APropertyValue: Boolean);
begin
  WriteShapeProperty(APropertyName, GetBoolParameterValue(APropertyValue));
end;

procedure TdxRtfBuilder.WriteShapeColorProperty(const APropertyName: string;
  APropertyValue: TdxAlphaColor);
var
  AColor: string;
begin
  AColor := IntToStr(dxGetRed(APropertyValue) or dxGetGreen(APropertyValue) shl 8 or dxGetBlue(APropertyValue) shl 16);
  WriteShapeProperty(APropertyName, AColor);
end;

procedure TdxRtfBuilder.WriteShapeIntegerProperty(const APropertyName: string;
  APropertyValue: Integer);
begin
  WriteShapeProperty(APropertyName, IntToStr(APropertyValue));
end;

procedure TdxRtfBuilder.WriteShapeProperty(const APropertyName,
  APropertyValue: string);
begin
  OpenGroup;
  try
    WriteCommand(TdxRtfExportSR.ShapeProperty);
    WriteShapePropertyName(APropertyName);
    WriteShapePropertyValue(APropertyValue);
  finally
    CloseGroup;
  end;
end;

procedure TdxRtfBuilder.WriteShapePropertyName(const APropertyName: string);
begin
  OpenGroup;
  try
    WriteCommand(TdxRtfExportSR.ShapePropertyName);
    WriteTextDirect(APropertyName);
  finally
    CloseGroup;
  end;
end;

procedure TdxRtfBuilder.WriteShapePropertyValue(const APropertyValue: string);
begin
  OpenGroup;
  try
    WriteCommand(TdxRtfExportSR.ShapePropertyValue);
    WriteTextDirect(APropertyValue);
  finally
    CloseGroup;
  end;
end;

procedure TdxRtfBuilder.WriteStreamAsHex(AStream: TStream);
begin
  if AStream is TBytesStream then
    WriteMemoryStreamAsHex(TBytesStream(AStream))
  else
    WriteStreamAsHexCore(AStream);
end;

procedure TdxRtfBuilder.WriteStreamAsHexCore(AStream: TStream);
var
  ABuffer: TBytes;
  ABufferLength, ALength: Integer;
begin
  SetLength(ABuffer, 4096);
  try
    ABufferLength := 4096;
    ALength := AStream.Size - AStream.Position;
    while ALength >= ABufferLength do
    begin
      AStream.Read(ABuffer, ABufferLength);
      Dec(ALength, ABufferLength);
      WriteByteArrayAsHex(ABuffer);
      FIsPreviousWriteCommand := False;
    end;
    if ALength > 0 then
    begin
      AStream.Read(ABuffer, ALength);
      WriteByteArrayAsHex(ABuffer, 0, ALength);
    end;
    FIsPreviousWriteCommand := True;
  finally
    SetLength(ABuffer, 0);
  end;
end;

procedure TdxRtfBuilder.WriteTextDirect(const AText: string;
  AMakeStringUnicodeCompatible: Boolean);
var
  S: string;
begin
  if FIsPreviousWriteCommand then
    RtfContent.Append(TdxRtfExportSR.Space);
  if AMakeStringUnicodeCompatible then
    S := GetUnicodeCompatibleStringDirect(AText)
  else
    S := AText;
  if S <> '' then
    RtfContent.Append(S);
  FIsPreviousWriteCommand := False;
  IncreaseRowLength(Length(AText));
end;

procedure TdxRtfBuilder.WriteTextDirectUnsafe(AText: TdxChunkedStringBuilder);
var
  ATextLength: Integer;
begin
  if FIsPreviousWriteCommand then
    RtfContent.Append(TdxRtfExportSR.Space);
  ATextLength := AText.Length;
  RtfContent.AppendExistingBuffersUnsafe(AText);
  FIsPreviousWriteCommand := False;
  IncreaseRowLength(ATextLength);
end;

class procedure TdxRtfBuilder.AddHexToMarkTable(ATable: TdxCharStringDictionary;
  ACh: Char);
begin
  ATable.Add(ACh, Format('\''%s', [LowerCase(IntToHex(Ord(ACh), 2))]));
end;

{ TdxDBCSRtfBuilder }

constructor TdxDBCSRtfBuilder.Create(AEncoding: TEncoding);
begin
  inherited Create(AEncoding);
  Assert(not Encoding.IsSingleByte);
end;

procedure TdxDBCSRtfBuilder.AppendUnicodeCompatibleCharCore(ACode: Integer; ACh: Char);
begin
  if Encoding.GetByteCount(ACh) > 1 then
    UnicodeTextBuilder.AppendFormat('\u%d?', [ACode])
  else
    inherited AppendUnicodeCompatibleCharCore(ACode, ACh);
end;

{ TdxRtfExportHelper }

function TdxRtfExportHelper.BlendColor(AColor: TdxAlphaColor): TdxAlphaColor;
begin
  Result := TdxAlphaColors.Blend(AColor, TdxAlphaColors.White);
end;

constructor TdxRtfExportHelper.Create;
begin
  inherited Create;
  FColorCollection := TdxAlphaColorList.Create;
  FColorCollection.Add(TdxAlphaColors.Empty);
  FFontNamesCollection := TdxStringList.Create;
  FListCollection := TDictionary<Integer, string>.Create;
  FListOverrideCollectionIndex := TdxIntegersDictionary.Create;
  FListOverrideCollection := TdxStringList.Create;
  FDefaultFontName := TdxRichEditControlCompatibility.DefaultFontName;
  FDefaultFontIndex := GetFontNameIndex(FDefaultFontName);
  FParagraphStylesCollectionIndex := TdxNamedOrdinalDictionary<Integer>.Create;
  FCharacterStylesCollectionIndex := TdxNamedOrdinalDictionary<Integer>.Create;
  FTableStylesCollectionIndex := TdxNamedOrdinalDictionary<Integer>.Create;
  FFontCharsetTable := TdxIntegersDictionary.Create;
  FStylesCollection := TdxStringList.Create;
  FUserCollection := TdxStringList.Create;
end;

destructor TdxRtfExportHelper.Destroy;
begin
  FreeAndNil(FColorCollection);
  FreeAndNil(FFontNamesCollection);
  FreeAndNil(FListCollection);
  FreeAndNil(FListOverrideCollectionIndex);
  FreeAndNil(FListOverrideCollection);
  FreeAndNil(FParagraphStylesCollectionIndex);
  FreeAndNil(FCharacterStylesCollectionIndex);
  FreeAndNil(FTableStylesCollectionIndex);
  FreeAndNil(FFontCharsetTable);
  FreeAndNil(FStylesCollection);
  FreeAndNil(FUserCollection);
  inherited Destroy;
end;

function TdxRtfExportHelper.GetUserIndex(ARangePermission: TdxRangePermission): Integer;
var
  AIndex, AId: Integer;
  APredefinedUserGroups: TDictionary<Integer, string>;
begin
  AIndex := UserCollection.IndexOf(ARangePermission.UserName);
  if AIndex >= 0 then
    Exit(AIndex + 1);

  APredefinedUserGroups := TdxRtfContentExporter.PredefinedUserGroups;
  for AId in APredefinedUserGroups.Keys do
  begin
    if APredefinedUserGroups[AId] = ARangePermission.Group then
      Exit(AId);
  end;
  Result := 0;
end;

function TdxRtfExportHelper.GetCharacterStylesCollectionIndex: TdxNamedOrdinalDictionary<Integer>;
begin
  Result := FCharacterStylesCollectionIndex;
end;

function TdxRtfExportHelper.GetColorCollection: TdxAlphaColorList;
begin
  Result := FColorCollection;
end;

function TdxRtfExportHelper.GetColorIndex(AColor: TdxAlphaColor): Integer;
begin
  if TdxAlphaColors.IsTransparentOrEmpty(AColor) then
    Result := 0
  else
  begin
    Result := FColorCollection.IndexOf(AColor);
    if Result < 0 then
      Result := FColorCollection.Add(AColor);
  end;
end;

function TdxRtfExportHelper.GetDefaultCharacterProperties: string;
begin
  Result := FDefaultCharacterProperties;
end;

function TdxRtfExportHelper.GetDefaultFontIndex: Integer;
begin
  Result := FDefaultFontIndex;
end;

function TdxRtfExportHelper.GetDefaultParagraphProperties: string;
begin
  Result := FDefaultParagraphProperties;
end;

function TdxRtfExportHelper.GetFontCharsetTable: TdxIntegersDictionary;
begin
  Result := FFontCharsetTable;
end;

function TdxRtfExportHelper.GetFontNamesCollection: TdxStringList;
begin
  Result := FFontNamesCollection;
end;

function TdxRtfExportHelper.GetFontNameIndex(const AName: string): Integer;
begin
  if AName = '' then
    Result := DefaultFontIndex
  else
  begin
    Result := FFontNamesCollection.IndexOf(AName);
    if Result < 0 then
      Result := FFontNamesCollection.Add(AName);
  end;
end;

function TdxRtfExportHelper.GetListCollection: TDictionary<Integer, string>;
begin
  Result := FListCollection;
end;

function TdxRtfExportHelper.GetListOverrideCollection: TdxStringList;
begin
  Result := FListOverrideCollection;
end;

function TdxRtfExportHelper.GetListOverrideCollectionIndex: TdxIntegersDictionary;
begin
  Result := FListOverrideCollectionIndex;
end;

function TdxRtfExportHelper.GetParagraphStylesCollectionIndex: TdxNamedOrdinalDictionary<Integer>;
begin
  Result := FParagraphStylesCollectionIndex;
end;

function TdxRtfExportHelper.GetStylesCollection: TdxStringList;
begin
  Result := FStylesCollection;
end;

function TdxRtfExportHelper.GetSupportStyle: Boolean;
begin
  Result := True;
end;

function TdxRtfExportHelper.GetTableStylesCollectionIndex: TdxNamedOrdinalDictionary<Integer>;
begin
  Result := FTableStylesCollectionIndex;
end;

function TdxRtfExportHelper.GetUserCollection: TdxStringList;
begin
  Result := FUserCollection;
end;

procedure TdxRtfExportHelper.SetDefaultCharacterProperties(const Value: string);
begin
  FDefaultCharacterProperties := Value;
end;

procedure TdxRtfExportHelper.SetDefaultParagraphProperties(const Value: string);
begin
  FDefaultParagraphProperties := Value;
end;

{ TdxRtfNumberingListExporter }

constructor TdxRtfNumberingListExporter.Create(
  ARtfExporter: TdxRtfContentExporter);
begin
  inherited Create;
  FRtfExporter := ARtfExporter;
  FRtfBuilder := FRtfExporter.CreateRtfBuilder;
  FCharacterPropertiesExporter := TdxRtfCharacterPropertiesExporter.Create(FRtfExporter.DocumentModel,
    FRtfExporter.RtfExportHelper, FRtfBuilder, FRtfExporter.RtfOptions);
  FParagraphPropertiesExporter := TdxRtfParagraphPropertiesExporter.Create(FRtfExporter.DocumentModel,
    FRtfExporter.RtfExportHelper, FRtfBuilder);
end;

destructor TdxRtfNumberingListExporter.Destroy;
begin
  FreeAndNil(FRtfBuilder);
  FreeAndNil(FCharacterPropertiesExporter);
  FreeAndNil(FParagraphPropertiesExporter);
  inherited Destroy;
end;

function TdxRtfNumberingListExporter.AddChar(ACh: Char; I: Integer): Integer;
begin
  Text := Text + ACh;
  Inc(FTextLength);
  Result := I + 1;
end;

function TdxRtfNumberingListExporter.AddEscapedChar(ACh: Char; I: Integer): Integer;
begin
  Text := Format('%s\''%s', [Text, TdxStringHelper.ToHex(Byte(Ord(ACh)))]);
  Inc(FTextLength);
  Result := I + 1;
end;

function TdxRtfNumberingListExporter.AddLevelNumber(const ADisplayFormatString: string; I: Integer): Integer;
begin
  if DoubleBrackets(ADisplayFormatString, I) then
    Exit(AddEscapedChar(ADisplayFormatString[I], I) + 1);
  if ADisplayFormatString[I] = '\' then
    Exit(AddEscapedChar(ADisplayFormatString[I], I));

  Result := AddLevelNumberCore(ADisplayFormatString, I);
end;

function TdxRtfNumberingListExporter.AddLevelNumberCore(const ADisplayFormatString: string; I: Integer): Integer;
var
  AValue: string;
  AIndex: Integer;
begin
  AValue := '';
  AIndex := TdxStringHelper.IndexOfAny(ADisplayFormatString, ['s', ':'], I);
  AValue := TdxStringHelper.Substring(ADisplayFormatString, I, AIndex - I);
  Text := Text + '\'#$27 + Format('%s', [LowerCase(IntToHex(StrToInt(AValue), 2))]);
  TextLength := TextLength + Length(AValue);
  Number := Number + '\'#$27 + Format('%s', [LowerCase(IntToHex(TextLength, 2))]);
  Result := I + Length(AValue) + 1 + 1;
  if ADisplayFormatString[AIndex + 1] = ':' then
    Inc(Result);
end;

class function TdxRtfNumberingListExporter.DoubleBrackets(const ADisplayFormatString: string; I: Integer): Boolean;
begin
  Result := ((ADisplayFormatString[I] = '%') and (ADisplayFormatString[I + 1] = '%')) or
    ((ADisplayFormatString[I] = '}') and (ADisplayFormatString[I + 1] = '}')) or
    ((ADisplayFormatString[I] = '{') and (ADisplayFormatString[I + 1] = '{'));
end;

procedure TdxRtfNumberingListExporter.Export(
  ANumberingLists: TdxNumberingListCollection; AStartIndex, ACount: Integer);
var
  AAbstractNumberingLists: TdxAbstractNumberingListCollection;
begin
  if ANumberingLists.Count <= 0 then
    Exit;
  AAbstractNumberingLists := GetAbstractNumberingLists(ANumberingLists, AStartIndex, ACount);
  try
    ExportNumberingListTable(AAbstractNumberingLists);
    ExportListOverrideTable(ANumberingLists, AStartIndex, ACount);
  finally
    AAbstractNumberingLists.Free;
  end;
end;

procedure TdxRtfNumberingListExporter.ExportAbstractListLevelParagraphStyleIndex(AAbstractListLevel: TdxListLevel);
var
  AParagraphStyleIndex: Integer;
  AParagraphStyles: TdxParagraphStyleCollection;
begin
  AParagraphStyles := FRtfExporter.DocumentModel.ParagraphStyles;
  AParagraphStyleIndex := AAbstractListLevel.ParagraphStyleIndex;
  if (AParagraphStyleIndex < 0) or (AParagraphStyleIndex >= AParagraphStyles.Count) then
    Exit;
  FRtfExporter.WriteParagraphStyle(AParagraphStyles[AAbstractListLevel.ParagraphStyleIndex]);
end;

procedure TdxRtfNumberingListExporter.ExportAbstractNumberingList(AList: TdxAbstractNumberingList);
begin
  if AList.Id = -2 then
    Exit;

  FRtfBuilder.OpenGroup();
  FRtfBuilder.WriteCommand(TdxRtfExportSR.NumberingList);
  FRtfBuilder.WriteCommand(TdxRtfExportSR.NumberingListTemplateId, -1);
  if TdxNumberingListHelper.IsHybridList(AList) then
    FRtfBuilder.WriteCommand(TdxRtfExportSR.NumberingListHybrid);
  ExportListLevels(AList.Levels);
  FRtfBuilder.OpenGroup;
  FRtfBuilder.WriteCommand(TdxRtfExportSR.NumberingListName);
  FRtfBuilder.CloseGroup;
  FRtfBuilder.WriteCommand(TdxRtfExportSR.NumberingListId, AList.Id);
  if AList.StyleLinkIndex >= 0 then
  begin
    FRtfBuilder.OpenGroup;
    FRtfBuilder.WriteCommand(TdxRtfExportSR.NumberingListStyleName, AList.StyleLink.StyleName);
    FRtfBuilder.CloseGroup;
  end
  else
  begin
    if AList.NumberingStyleReferenceIndex >= 0 then
      FRtfBuilder.WriteCommand(TdxRtfExportSR.NumberingListStyleId, AList.NumberingStyleReference.NumberingList.AbstractNumberingList.Id);
  end;
  FRtfBuilder.CloseGroup;
  if not FRtfExporter.RtfExportHelper.ListCollection.ContainsKey(AList.Id) then
    FRtfExporter.RtfExportHelper.ListCollection.Add(AList.Id, FRtfBuilder.RtfContent.ToString);
end;

procedure TdxRtfNumberingListExporter.ExportAbstractNumberingLists(
  AAbstractNumberingLists: TdxAbstractNumberingListCollection);
var
  I: Integer;
  ACount: TdxAbstractNumberingListIndex;
begin
  ACount := AAbstractNumberingLists.Count;
  for I := 0 to ACount - 1 do
  begin
    FRtfBuilder.Clear;
    ExportAbstractNumberingList(AAbstractNumberingLists[I]);
  end;
end;

procedure TdxRtfNumberingListExporter.ExportListLevel(const AListLevel: IdxListLevel);
var
  ANumberingListFormat: Integer;
begin
  ANumberingListFormat := GetNumberingListFormat(AListLevel.ListLevelProperties.Format);
  FRtfBuilder.OpenGroup;
  FRtfBuilder.WriteCommand(TdxRtfExportSR.ListLevel);
  FRtfBuilder.WriteCommand(TdxRtfExportSR.ListLevelNumberingFormat, ANumberingListFormat);
  FRtfBuilder.WriteCommand(TdxRtfExportSR.ListLevelNumberingFormatN, ANumberingListFormat);
  FRtfBuilder.WriteCommand(TdxRtfExportSR.ListLevelAlignment, Ord(AListLevel.ListLevelProperties.Alignment));
  FRtfBuilder.WriteCommand(TdxRtfExportSR.ListLevelAlignmentN, Ord(AListLevel.ListLevelProperties.Alignment));
  FRtfBuilder.WriteCommand(TdxRtfExportSR.ListLevelFollow, GetListLevelSeparator(AListLevel.ListLevelProperties.Separator));
  FRtfBuilder.WriteCommand(TdxRtfExportSR.ListLevelStart, AListLevel.ListLevelProperties.Start);
  if AListLevel.ListLevelProperties.Legacy then
  begin
    FRtfBuilder.WriteCommand(TdxRtfExportSR.ListLevelLegacy);
    FRtfBuilder.WriteCommand(TdxRtfExportSR.ListLevelLegacySpace,
      AListLevel.DocumentModel.UnitConverter.ModelUnitsToTwips(AListLevel.ListLevelProperties.LegacySpace));
    FRtfBuilder.WriteCommand(TdxRtfExportSR.ListLevelLegacyIndent,
      AListLevel.DocumentModel.UnitConverter.ModelUnitsToTwips(AListLevel.ListLevelProperties.LegacyIndent));
  end;
  ExportListLevelTextAndNumber(AListLevel.ListLevelProperties.DisplayFormatString, AListLevel.ListLevelProperties.TemplateCode);

  if AListLevel.ListLevelProperties.ConvertPreviousLevelNumberingToDecimal then
    FRtfBuilder.WriteCommand(TdxRtfExportSR.ListLevelLegal, 1)
  else
    FRtfBuilder.WriteCommand(TdxRtfExportSR.ListLevelLegal, 0);

  if AListLevel.ListLevelProperties.SuppressRestart then
    FRtfBuilder.WriteCommand(TdxRtfExportSR.ListLevelNoRestart, 1)
  else
    FRtfBuilder.WriteCommand(TdxRtfExportSR.ListLevelNoRestart, 0);

  ExportListLevelCharacterAndParagraphProperties(AListLevel);
  FRtfBuilder.CloseGroup;
end;

procedure TdxRtfNumberingListExporter.ExportListLevelCharacterAndParagraphProperties(const AListLevel: IdxListLevel);
var
  AMergedCharacterProperties: TdxMergedCharacterProperties;
  AMergedParagraphProperties: TdxMergedParagraphProperties;
begin
  AMergedCharacterProperties := TdxMergedCharacterProperties.Create(AListLevel.CharacterProperties);
  try
    AMergedCharacterProperties.Merge(FRtfExporter.DocumentModel.DefaultCharacterProperties);
    AMergedCharacterProperties.Options.UseDoubleFontSize := AListLevel.CharacterProperties.UseDoubleFontSize;
    AMergedCharacterProperties.Options.UseFontName := AListLevel.CharacterProperties.UseFontName;
    FCharacterPropertiesExporter.ExportCharacterProperties(AMergedCharacterProperties, True, True, True);
  finally
    AMergedCharacterProperties.Free;
  end;
  if AListLevel is TdxListLevel then
    ExportAbstractListLevelParagraphStyleIndex(TdxListLevel(AListLevel));
  AMergedParagraphProperties := TdxMergedParagraphProperties.Create(AListLevel.ParagraphProperties);
  try
    AMergedParagraphProperties.Merge(FRtfExporter.DocumentModel.DefaultParagraphProperties);
    FParagraphPropertiesExporter.WriteParagraphIndents(AMergedParagraphProperties);
  finally
    AMergedParagraphProperties.Free;
  end;
end;

procedure TdxRtfNumberingListExporter.ExportListLevels(AListLevelCollection: TdxListLevelCollection);
var
  I: Integer;
begin
  for I := 0 to AListLevelCollection.Count - 1 do
    ExportListLevel(AListLevelCollection[I]);
end;

procedure TdxRtfNumberingListExporter.ExportListLevelTextAndNumber(const ADisplayFormatString: string;
  ALevelTemplateId: Integer);
begin
  FText := EmptyStr;
  FNumber := EmptyStr;
  GetTextAndNumber(ADisplayFormatString);
  FRtfBuilder.OpenGroup;
  FRtfBuilder.WriteCommand(TdxRtfExportSR.ListLevelText);
  if ALevelTemplateId <> 0 then
    FRtfBuilder.WriteCommand(TdxRtfExportSR.LevelTemplateId, ALevelTemplateId);
  FRtfBuilder.WriteTextDirect(Format('\''%s', [LowerCase(IntToHex(FTextLength, 2))]));
  FRtfBuilder.WriteTextDirect(FText, True);
  FRtfBuilder.WriteChar(';');
  FRtfBuilder.CloseGroup;
  FRtfBuilder.OpenGroup;
  FRtfBuilder.WriteCommand(TdxRtfExportSR.ListLevelNumbers, FNumber + ';');
  FRtfBuilder.CloseGroup;
end;

procedure TdxRtfNumberingListExporter.ExportListOverride(ANumberingList: TdxNumberingList);
var
  AListOverrideCount, AIndex: Integer;
begin
  FRtfBuilder.OpenGroup;
  FRtfBuilder.WriteCommand(TdxRtfExportSR.ListOverride);
  FRtfBuilder.WriteCommand(TdxRtfExportSR.NumberingListId, ANumberingList.AbstractNumberingList.Id);
  AListOverrideCount := GetListOverrideCount(ANumberingList);
  FRtfBuilder.WriteCommand(TdxRtfExportSR.ListOverrideCount, AListOverrideCount);
  WriteListOverrideId(ANumberingList);
  ExportListOverrideLevels(ANumberingList);
  FRtfBuilder.CloseGroup;

  AIndex := FRtfExporter.RtfExportHelper.ListOverrideCollection.Count;
  FRtfExporter.RtfExportHelper.ListOverrideCollection.Add(FRtfBuilder.RtfContent.ToString);
  if FRtfExporter.RtfExportHelper.ListOverrideCollectionIndex.ContainsKey(ANumberingList.Id) then
    FRtfExporter.RtfExportHelper.ListOverrideCollectionIndex.Add(ANumberingList.Id, AIndex);
end;

procedure TdxRtfNumberingListExporter.ExportListOverrideLevel(ALevel: TdxAbstractListLevel);
begin
  FRtfBuilder.OpenGroup;
  FRtfBuilder.WriteCommand(TdxRtfExportSR.ListOverrideLevel);
  if ALevel.OverrideStart then
    FRtfBuilder.WriteCommand(TdxRtfExportSR.ListOverrideStart);
  if ALevel is TdxOverrideListLevel then
  begin
    FRtfBuilder.WriteCommand(TdxRtfExportSR.ListOverrideFormat);
    ExportListLevel(ALevel);
  end
  else
    FRtfBuilder.WriteCommand(TdxRtfExportSR.ListLevelStart, ALevel.NewStart);
  FRtfBuilder.CloseGroup;
end;

procedure TdxRtfNumberingListExporter.ExportListOverrideLevels(ANumberingList: TdxNumberingList);
var
  I: Integer;
  ALevels: TdxListLevelCollection;
begin
  ALevels := ANumberingList.Levels;
  for I := 0 to ALevels.Count - 1 do
    if IsOverrideLevel(ALevels[I]) then
      ExportListOverrideLevel(ALevels[I]);
end;

procedure TdxRtfNumberingListExporter.ExportListOverrideTable(ANumberingLists: TdxNumberingListCollection;
  AStartIndex: TdxNumberingListIndex; ACount: Integer);
var
  I: Integer;
  ALastIndex: TdxNumberingListIndex;
begin
  if ANumberingLists.Count <= 0 then
    Exit;

  ALastIndex := AStartIndex + (ACount - 1);
  for I := AStartIndex to ALastIndex do
  begin
    FRtfBuilder.Clear;
    ExportListOverride(ANumberingLists[I]);
  end;
end;

procedure TdxRtfNumberingListExporter.ExportListOverrideTable(ANumberingLists: TdxNumberingListCollection);
var
  I, ACount: TdxNumberingListIndex;
begin
  if ANumberingLists.Count <= 0 then
    Exit;

  ACount := ANumberingLists.Count;
  for I := 0 to ACount - 1 do
  begin
    RtfBuilder.Clear;
    ExportListOverride(ANumberingLists[I]);
  end;
end;

procedure TdxRtfNumberingListExporter.ExportNumberingListTable(
  AAbstractNumberingLists: TdxAbstractNumberingListCollection);
begin
  if AAbstractNumberingLists.Count > 0 then
  begin
    FRtfBuilder.Clear;
    ExportAbstractNumberingLists(AAbstractNumberingLists);
  end;
end;

function TdxRtfNumberingListExporter.GetAbstractNumberingLists(ANumberingLists: TdxNumberingListCollection;
  AStartIndex: TdxNumberingListIndex; ACount: Integer): TdxAbstractNumberingListCollection;
var
  I: Integer;
  AList: TdxAbstractNumberingList;
  ALastIndex: TdxNumberingListIndex;
begin
  Result := TdxAbstractNumberingListCollection.Create(False);
  ALastIndex := AStartIndex + (ACount - 1);
  for I := AStartIndex to ALastIndex do
  begin
    AList := ANumberingLists[I].AbstractNumberingList;
    if Result.IndexOf(AList) < 0 then
      Result.Add(AList);
  end;
end;

function TdxRtfNumberingListExporter.GetListLevelSeparator(ASeparator: Char): Integer;
begin
  case ASeparator of
    TdxCharacters.TabMark:
      Result := 0;
    ' ':
      Result := 1;
    else
      Result := 2;
  end;
end;

function TdxRtfNumberingListExporter.GetListOverrideCount(ANumberingList: TdxNumberingList): Integer;
var
  I: Integer;
  ALevels: TdxListLevelCollection;
begin
  ALevels := ANumberingList.Levels;
  Result := 0;
  for I := 0 to ALevels.Count - 1 do
    if IsOverrideLevel(ALevels[I]) then
      Inc(Result);
end;

function TdxRtfNumberingListExporter.GetNumberingListFormat(ANumberingFormat: TdxNumberingFormat): Integer;
begin
  Result := TdxListLevelDestination.NumberingFormats.IndexOf(ANumberingFormat);
  if Result < 0 then
    Result := 0;
end;

procedure TdxRtfNumberingListExporter.GetTextAndNumber(const ADisplayFormatString: string);
var
  ACh: Char;
  I, ACount: Integer;
begin
  TextLength := 0;
  ACount := Length(ADisplayFormatString);
  I := 1;
  while I <= ACount do
  begin
    ACh := ADisplayFormatString[I];
    if not RtfBuilder.IsSpecialSymbol(ACh) and (ACh <> '%') then
      I := AddChar(ACh, I)
    else
      I := AddLevelNumber(ADisplayFormatString, I);
  end;
end;

function TdxRtfNumberingListExporter.IsOverrideLevel(AListLevel: TdxAbstractListLevel): Boolean;
begin
  if AListLevel is TdxOverrideListLevel then
    Exit(True);
  Result := AListLevel.OverrideStart;
end;

procedure TdxRtfNumberingListExporter.WriteListOverrideId(ANumberingList: TdxNumberingList);
begin
  FRtfBuilder.WriteCommand(TdxRtfExportSR.ListIndex, ANumberingList.Id);
end;

{ TdxRtfParagraphPropertiesExporter }

procedure TdxRtfParagraphPropertiesExporter.WriteParagraphKeepLinesTogether(
  Value: Boolean);
begin
  if Value <> DefaultKeepLinesTogether then
    RtfBuilder.WriteCommand(TdxRtfExportSR.KeepLinesTogether, IfThen(Value, 1, 0));
end;

function TdxRtfParagraphPropertiesExporter.CalcRtfFirstLineIndent(
  AFirstLineIndentType: TdxParagraphFirstLineIndent;
  AFirstLineIndent: Integer): Integer;
begin
  case AFirstLineIndentType of
    TdxParagraphFirstLineIndent.Indented: Result := UnitConverter.ModelUnitsToTwips(AFirstLineIndent);
    TdxParagraphFirstLineIndent.Hanging: Result := -UnitConverter.ModelUnitsToTwips(AFirstLineIndent);
  else
    Result := 0;
  end;
end;

function TdxRtfParagraphPropertiesExporter.CalcRtfLeftIndent(
  AFirstLineIndentType: TdxParagraphFirstLineIndent; AFirstLineIndent,
  ALeftIndent: Integer): Integer;
begin
  Result := UnitConverter.ModelUnitsToTwips(ALeftIndent);
end;

function TdxRtfParagraphPropertiesExporter.CalcRtfRightIndent(
  ARightIndent: Integer): Integer;
begin
  Result := UnitConverter.ModelUnitsToTwips(ARightIndent);
end;

procedure TdxRtfParagraphPropertiesExporter.ExportParagraphNumberingProperties(
  AParagraph: TdxParagraph);
begin
  if not AParagraph.ShouldExportNumbering then
    Exit;
  RtfBuilder.WriteCommand(TdxRtfExportSR.LevelIndex, AParagraph.GetListLevelIndex);
  WriteParagraphListIndex(AParagraph.GetNumberingListIndex);
end;

procedure TdxRtfParagraphPropertiesExporter.ExportParagraphProperties(
  AParagraph: TdxParagraph; ATableNestingLevel: Integer);
var
  AProperties: TdxMergedParagraphProperties;
  ATabs: TdxTabFormattingInfo;
begin
  if AParagraph.IsInList then
    ExportParagraphNumberingProperties(AParagraph);
  if RtfExportHelper.SupportStyle and (AParagraph.ParagraphStyleIndex <> TdxParagraphStyleCollection.EmptyParagraphStyleIndex) then
    WriteParagraphStyle(AParagraph.ParagraphStyle);
  if AParagraph.TopBorder.Style <> TdxBorderLineStyle.None then
    WriteParagraphBorder(AParagraph.TopBorder, TdxRtfExportSR.TopParagraphBorder);
  if AParagraph.LeftBorder.Style <> TdxBorderLineStyle.None then
    WriteParagraphBorder(AParagraph.LeftBorder, TdxRtfExportSR.LeftParagraphBorder);
  if AParagraph.BottomBorder.Style <> TdxBorderLineStyle.None then
    WriteParagraphBorder(AParagraph.BottomBorder, TdxRtfExportSR.BottomParagraphBorder);
  if AParagraph.RightBorder.Style <> TdxBorderLineStyle.None then
    WriteParagraphBorder(AParagraph.RightBorder, TdxRtfExportSR.RightParagraphBorder);
  if AParagraph.FrameProperties <> nil then
    WriteFrameProperties(AParagraph.FrameProperties);
  WriteParagraphAlignment(AParagraph.Alignment);
  WriteParagraphTableProperties(AParagraph, ATableNestingLevel);
  WriteParagraphGroupPropertiesId(AParagraph);
  AProperties := AParagraph.GetMergedParagraphProperties;
  try
    WriteParagraphIndents(AProperties);
  finally
    AProperties.Free;
  end;
  WriteParagraphSuppressHyphenation(AParagraph.SuppressHyphenation);
  WriteParagraphSuppressLineNumbers(AParagraph.SuppressLineNumbers);
  WriteParagraphContextualSpacing(AParagraph.ContextualSpacing);
  WriteParagraphPageBreakBefore(AParagraph.PageBreakBefore);
  WriteParagraphBeforeAutoSpacing(AParagraph.BeforeAutoSpacing);
  WriteParagraphAfterAutoSpacing(AParagraph.AfterAutoSpacing);
  WriteParagraphKeepWithNext(AParagraph.KeepWithNext);
  WriteParagraphKeepLinesTogether(AParagraph.KeepLinesTogether);
  WriteParagraphWidowOrphanControl(AParagraph.WidowOrphanControl);
  WriteParagraphOutlineLevel(AParagraph.OutlineLevel);
  WriteParagraphBackColor(AParagraph.BackColor);
  WriteParagraphLineSpacing(AParagraph.LineSpacingType, AParagraph.LineSpacing);
  WriteParagraphSpacingBefore(AParagraph.SpacingBefore);
  WriteParagraphSpacingAfter(AParagraph.SpacingAfter);
  ATabs := AParagraph.GetTabs;
  try
    WriteTabs(ATabs);
  finally
    ATabs.Free;
  end;
end;

procedure TdxRtfParagraphPropertiesExporter.ExportParagraphPropertiesCore(
  AProperties: TdxMergedParagraphProperties; ACheckDefaultAlignment: Boolean);
var
  AInfo: TdxParagraphFormattingInfo;
begin
  AInfo := AProperties.Info;
  if AInfo.TopBorder.Style <> TdxBorderLineStyle.None then
    WriteParagraphBorder(AInfo.TopBorder, TdxRtfExportSR.TopParagraphBorder);
  if AInfo.LeftBorder.Style <> TdxBorderLineStyle.None then
    WriteParagraphBorder(AInfo.LeftBorder, TdxRtfExportSR.LeftParagraphBorder);
  if AInfo.BottomBorder.Style <> TdxBorderLineStyle.None then
    WriteParagraphBorder(AInfo.BottomBorder, TdxRtfExportSR.BottomParagraphBorder);
  if AInfo.RightBorder.Style <> TdxBorderLineStyle.None then
    WriteParagraphBorder(AInfo.RightBorder, TdxRtfExportSR.RightParagraphBorder);
  if not ACheckDefaultAlignment or (AInfo.Alignment <> TdxParagraphAlignment.Left) then
    WriteParagraphAlignment(AInfo.Alignment);
  WriteParagraphIndents(AProperties);
  WriteParagraphSuppressHyphenation(AInfo.SuppressHyphenation);
  WriteParagraphSuppressLineNumbers(AInfo.SuppressLineNumbers);
  WriteParagraphContextualSpacing(AInfo.ContextualSpacing);
  WriteParagraphPageBreakBefore(AInfo.PageBreakBefore);
  WriteParagraphBeforeAutoSpacing(AInfo.BeforeAutoSpacing);
  WriteParagraphAfterAutoSpacing(AInfo.AfterAutoSpacing);
  WriteParagraphKeepWithNext(AInfo.KeepWithNext);
  WriteParagraphKeepLinesTogether(AInfo.KeepLinesTogether);
  WriteParagraphWidowOrphanControl(AInfo.WidowOrphanControl);
  WriteParagraphOutlineLevel(AInfo.OutlineLevel);
  WriteParagraphBackColor(AInfo.BackColor);
  WriteParagraphLineSpacing(AInfo.LineSpacingType, AInfo.LineSpacing);
  WriteParagraphSpacingBefore(AInfo.SpacingBefore);
  WriteParagraphSpacingAfter(AInfo.SpacingAfter);
end;

procedure TdxRtfParagraphPropertiesExporter.WriteParagraphAfterAutoSpacing(
  Value: Boolean);
begin
  if Value <> DefaultAfterAutoSpacing then
    RtfBuilder.WriteCommand(TdxRtfExportSR.AfterAutoSpacing, IfThen(Value, 1, 0));
end;

procedure TdxRtfParagraphPropertiesExporter.WriteParagraphAlignment(
  Alignment: TdxParagraphAlignment);
begin
  case Alignment of
    TdxParagraphAlignment.Left: RtfBuilder.WriteCommand(TdxRtfExportSR.LeftAlignment);
    TdxParagraphAlignment.Center: RtfBuilder.WriteCommand(TdxRtfExportSR.CenterAlignment);
    TdxParagraphAlignment.Justify: RtfBuilder.WriteCommand(TdxRtfExportSR.JustifyAlignment);
    TdxParagraphAlignment.Right: RtfBuilder.WriteCommand(TdxRtfExportSR.RightAlignment);
  end;
end;

procedure TdxRtfParagraphPropertiesExporter.WriteParagraphBackColor(
  Value: TdxAlphaColor);
var
  AIndex: Integer;
begin
  if TdxAlphaColors.IsTransparentOrEmpty(Value) then
    Exit;
  AIndex := RtfExportHelper.GetColorIndex(Value);
  RtfBuilder.WriteCommand(TdxRtfExportSR.ParagraphBackgroundColor, AIndex);
end;

procedure TdxRtfParagraphPropertiesExporter.WriteParagraphBeforeAutoSpacing(
  Value: Boolean);
begin
  if Value <> DefaultBeforeAutoSpacing then
    RtfBuilder.WriteCommand(TdxRtfExportSR.BeforeAutoSpacing, IfThen(Value, 1 , 0));
end;

procedure TdxRtfParagraphPropertiesExporter.WriteParagraphContextualSpacing(
  Value: Boolean);
begin
  if Value then
    RtfBuilder.WriteCommand(TdxRtfExportSR.ContextualSpacing);
end;

procedure TdxRtfParagraphPropertiesExporter.WriteParagraphGroupPropertiesId(
  AParagraph: TdxParagraph);
begin
end;

procedure TdxRtfParagraphPropertiesExporter.WriteParagraphIndents(
  AMergedParagraphProperties: TdxMergedParagraphProperties);
var
  AParagraphPropertiesInfo: TdxParagraphFormattingInfo;
  AFirstLineIndent: Integer;
  ALeftIndent: Integer;
  ARightIndent: Integer;
begin
  AParagraphPropertiesInfo := AMergedParagraphProperties.Info;

  AFirstLineIndent := CalcRtfFirstLineIndent(AParagraphPropertiesInfo.FirstLineIndentType,
    AParagraphPropertiesInfo.FirstLineIndent);
  if AFirstLineIndent <> DefaultParagraphFirstLineIndent then
    RtfBuilder.WriteCommand(TdxRtfExportSR.FirstLineIndentInTwips, AFirstLineIndent);

  ALeftIndent := CalcRtfLeftIndent(AParagraphPropertiesInfo.FirstLineIndentType,
    AParagraphPropertiesInfo.FirstLineIndent,
    AParagraphPropertiesInfo.LeftIndent);
  if ALeftIndent <> DefaultParagraphLeftIndent then
  begin
    RtfBuilder.WriteCommand(TdxRtfExportSR.LeftIndentInTwips, ALeftIndent);
    RtfBuilder.WriteCommand(TdxRtfExportSR.LeftIndentInTwips_Lin, ALeftIndent);
  end;
  ARightIndent := CalcRtfRightIndent(AParagraphPropertiesInfo.RightIndent);
  if ARightIndent <> DefaultParagraphRightIndent then
  begin
    RtfBuilder.WriteCommand(TdxRtfExportSR.RightIndentInTwips, ARightIndent);
    RtfBuilder.WriteCommand(TdxRtfExportSR.RightIndentInTwips_Rin, ARightIndent);
  end;
end;

procedure TdxRtfParagraphPropertiesExporter.WriteParagraphKeepWithNext(
  Value: Boolean);
begin
  if Value <> DefaultKeepWithNext then
    RtfBuilder.WriteCommand(TdxRtfExportSR.KeepWithNext, IfThen(Value, 1, 0));
end;

procedure TdxRtfParagraphPropertiesExporter.WriteParagraphLineSpacing(
  AParagraphLineSpacingType: TdxParagraphLineSpacing;
  AParagraphLineSpacing: Single);
begin
  case AParagraphLineSpacingType of
    TdxParagraphLineSpacing.AtLeast:
      WriteRtfLineSpacing(UnitConverter.ModelUnitsToTwips(Trunc(AParagraphLineSpacing)), AtLeastLineSpacingMultiple);
    TdxParagraphLineSpacing.Exactly:
      WriteRtfLineSpacing(-UnitConverter.ModelUnitsToTwips(Trunc(AParagraphLineSpacing)), ExactlyLineSpacingMultiple);
    TdxParagraphLineSpacing.Double:
      WriteRtfLineSpacing(DoubleIntervalRtfLineSpacingValue, MultipleLineSpacing);
    TdxParagraphLineSpacing.Sesquialteral:
      WriteRtfLineSpacing(SesquialteralIntervalRtfLineSpacingValue, MultipleLineSpacing);
    TdxParagraphLineSpacing.Multiple:
      WriteRtfLineSpacing(Trunc(AParagraphLineSpacing * SingleIntervalRtfLineSpacingValue), MultipleLineSpacing);
  end;
end;

procedure TdxRtfParagraphPropertiesExporter.WriteParagraphListIndex(
  AIndex: TdxNumberingListIndex);
begin
  RtfBuilder.WriteCommand(TdxRtfExportSR.ListIndex, DocumentModel.NumberingLists[AIndex].Id);
end;

procedure TdxRtfParagraphPropertiesExporter.WriteParagraphOutlineLevel(
  AOutlineLevel: Integer);
begin
  if (AOutlineLevel < 0) or (AOutlineLevel >= 10) then
    Exit;

  if AOutlineLevel > 0 then
    RtfBuilder.WriteCommand(TdxRtfExportSR.OutlineLevel, AOutlineLevel - 1);
end;

procedure TdxRtfParagraphPropertiesExporter.WriteParagraphPageBreakBefore(
  Value: Boolean);
begin
  if Value <> DefaultPageBreakBefore then
    RtfBuilder.WriteCommand(TdxRtfExportSR.PageBreakBefore, IfThen(Value, 1 , 0));
end;

procedure TdxRtfParagraphPropertiesExporter.WriteParagraphSpacingAfter(
  ASpacingAfter: Integer);
begin
  if ASpacingAfter <> DefaultParagraphSpacingAfter then
    RtfBuilder.WriteCommand(TdxRtfExportSR.SpaceAfter, UnitConverter.ModelUnitsToTwips(ASpacingAfter));
end;

procedure TdxRtfParagraphPropertiesExporter.WriteParagraphSpacingBefore(
  ASpacingBefore: Integer);
begin
  if ASpacingBefore <> DefaultParagraphSpacingBefore then
    RtfBuilder.WriteCommand(TdxRtfExportSR.SpaceBefore, UnitConverter.ModelUnitsToTwips(ASpacingBefore));
end;

procedure TdxRtfParagraphPropertiesExporter.WriteParagraphStyle(
  AParagraphStyle: TdxParagraphStyle);
var
  AStyleName: string;
  AStyleCollection: TdxNamedOrdinalDictionary<Integer>;
begin
  AStyleName := AParagraphStyle.StyleName;
  AStyleCollection := RtfExportHelper.ParagraphStylesCollectionIndex;
  if AStyleCollection.ContainsKey(AStyleName) then
    RtfBuilder.WriteCommand(TdxRtfExportSR.ParagraphStyle, AStyleCollection[AStyleName]);
end;

procedure TdxRtfParagraphPropertiesExporter.WriteParagraphSuppressHyphenation(
  AParagraphSuppressHyphenation: Boolean);
begin
  if AParagraphSuppressHyphenation <> DefaultSuppressHyphenation then
    RtfBuilder.WriteCommand(TdxRtfExportSR.AutomaticParagraphHyphenation, IfThen(AParagraphSuppressHyphenation, 0, 1));
end;

procedure TdxRtfParagraphPropertiesExporter.WriteParagraphSuppressLineNumbers(
  AParagraphSuppressLineNumbers: Boolean);
begin
  if AParagraphSuppressLineNumbers then
    RtfBuilder.WriteCommand(TdxRtfExportSR.SuppressLineNumbering);
end;

procedure TdxRtfParagraphPropertiesExporter.WriteTabInfo(const ATabInfo: TdxTabInfo);
begin
  WriteTabKind(ATabInfo.Alignment);
  WriteTabLeader(ATabInfo.Leader);
  WriteTabPosition(ATabInfo.Position);
end;

procedure TdxRtfParagraphPropertiesExporter.WriteParagraphTableProperties(
  AParagraph: TdxParagraph; ANestingLevel: Integer);
begin
  if not AParagraph.DocumentModel.DocumentCapabilities.TablesAllowed then
    Exit;
  if ANestingLevel > 0 then
    RtfBuilder.WriteCommand(TdxRtfExportSR.InTableParagraph);
  if ANestingLevel > 1 then
    RtfBuilder.WriteCommand(TdxRtfExportSR.ParagraphNestingLevel, ANestingLevel);
end;

procedure TdxRtfParagraphPropertiesExporter.WriteTabs(ATabFormattingInfo: TdxTabFormattingInfo);
var
  ACount: Integer;
  I: Integer;
begin
  ACount := ATabFormattingInfo.Count;
  for I := 0 to ACount - 1 do
    WriteTabInfo(ATabFormattingInfo[I]);
end;

procedure TdxRtfParagraphPropertiesExporter.WriteParagraphWidowOrphanControl(
  Value: Boolean);
begin
  if Value = DefaultWidowOrphanControl then
    Exit;
  if Value then
    RtfBuilder.WriteCommand(TdxRtfExportSR.WidowOrphanControlOn)
  else
    RtfBuilder.WriteCommand(TdxRtfExportSR.WidowOrphanControlOff);
end;

procedure TdxRtfParagraphPropertiesExporter.WriteRtfLineSpacing(
  ARtfLineSpacingValue, ARtfLineSpacingMultiple: Integer);
begin
  RtfBuilder.WriteCommand(TdxRtfExportSR.RtfLineSpacingValue, ARtfLineSpacingValue);
  RtfBuilder.WriteCommand(TdxRtfExportSR.RtfLineSpacingMultiple, ARtfLineSpacingMultiple);
end;

procedure TdxRtfParagraphPropertiesExporter.WriteTabKind(
  AlignmentType: TdxTabAlignmentType);
begin
  case AlignmentType of
    TdxTabAlignmentType.Center:
      RtfBuilder.WriteCommand(TdxRtfExportSR.CenteredTab);
    TdxTabAlignmentType.Right:
      RtfBuilder.WriteCommand(TdxRtfExportSR.FlushRightTab);
    TdxTabAlignmentType.Decimal:
      RtfBuilder.WriteCommand(TdxRtfExportSR.DecimalTab);
  end;
end;

procedure TdxRtfParagraphPropertiesExporter.WriteTabLeader(
  ALeaderType: TdxTabLeaderType);
begin
  case ALeaderType of
    TdxTabLeaderType.Dots:
      RtfBuilder.WriteCommand(TdxRtfExportSR.TabLeaderDots);
    TdxTabLeaderType.EqualSign:
      RtfBuilder.WriteCommand(TdxRtfExportSR.TabLeaderEqualSign);
    TdxTabLeaderType.Hyphens:
      RtfBuilder.WriteCommand(TdxRtfExportSR.TabLeaderHyphens);
    TdxTabLeaderType.MiddleDots:
      RtfBuilder.WriteCommand(TdxRtfExportSR.TabLeaderMiddleDots);
    TdxTabLeaderType.ThickLine:
      RtfBuilder.WriteCommand(TdxRtfExportSR.TabLeaderThickLine);
    TdxTabLeaderType.Underline:
      RtfBuilder.WriteCommand(TdxRtfExportSR.TabLeaderUnderline);
  end;
end;

procedure TdxRtfParagraphPropertiesExporter.WriteTabPosition(
  APosition: Integer);
begin
  RtfBuilder.WriteCommand(TdxRtfExportSR.TabPosition, UnitConverter.ModelUnitsToTwips(APosition));
end;

procedure TdxRtfParagraphPropertiesExporter.WriteParagraphBorder(ATopBorder: TdxBorderInfo; const ACommand: string);
var
  ADefaultBorder: TdxBorderInfo;
begin
  ADefaultBorder := DocumentModel.Cache.BorderInfoCache.DefaultItem;
  if ATopBorder <> ADefaultBorder then
  begin
    RtfBuilder.WriteCommand(ACommand);
    WriteBorderProperties(ATopBorder);
  end;
end;

procedure TdxRtfParagraphPropertiesExporter.WriteFrameProperties(AProperties: TdxFrameProperties);
begin
  if AProperties.UseVerticalPositionType then
    WriteParagraphVerticalPositionType(AProperties.VerticalPositionType);
  if AProperties.UseHorizontalPositionType then
    WriteParagraphHorizontalPositionType(AProperties.HorizontalPositionType);
  if AProperties.UseHorizontalPosition then
    RtfBuilder.WriteCommand(TdxRtfExportSR.FrameHorizontalPosition, AProperties.HorizontalPosition);
  if AProperties.UseVerticalPosition then
    RtfBuilder.WriteCommand(TdxRtfExportSR.FrameVerticalPosition, AProperties.VerticalPosition);
  if AProperties.UseHeight then
  begin
    if AProperties.HorizontalRule = TdxParagraphFrameHorizontalRule.Exact then
      RtfBuilder.WriteCommand(TdxRtfExportSR.FrameHeight, -AProperties.Height)
    else
      RtfBuilder.WriteCommand(TdxRtfExportSR.FrameHeight, AProperties.Height);
  end;
  if AProperties.UseWidth then
    RtfBuilder.WriteCommand(TdxRtfExportSR.FrameWidth, AProperties.Width);
  if AProperties.UseTextWrapType then
    WriteParagraphWrapType(AProperties.TextWrapType);
end;

procedure TdxRtfParagraphPropertiesExporter.WriteParagraphVerticalPositionType(APositionType: TdxParagraphFrameVerticalPositionType);
begin
  case APositionType of
    TdxParagraphFrameVerticalPositionType.Margin:
      RtfBuilder.WriteCommand(TdxRtfExportSR.ParagraphVerticalPositionTypeMargin);
    TdxParagraphFrameVerticalPositionType.Page:
      RtfBuilder.WriteCommand(TdxRtfExportSR.ParagraphVerticalPositionTypePage);
    TdxParagraphFrameVerticalPositionType.Paragraph:
      RtfBuilder.WriteCommand(TdxRtfExportSR.ParagraphVerticalPositionTypeLine);
  end;
end;

procedure TdxRtfParagraphPropertiesExporter.WriteParagraphHorizontalPositionType(APositionType: TdxParagraphFrameHorizontalPositionType);
begin
  case APositionType of
    TdxParagraphFrameHorizontalPositionType.Margin:
      RtfBuilder.WriteCommand(TdxRtfExportSR.ParagraphHorizontalPositionTypeMargin);
    TdxParagraphFrameHorizontalPositionType.Page:
      RtfBuilder.WriteCommand(TdxRtfExportSR.ParagraphHorizontalPositionTypePage);
    TdxParagraphFrameHorizontalPositionType.Column:
      RtfBuilder.WriteCommand(TdxRtfExportSR.ParagraphHorizontalPositionTypeColumn);
  end;
end;

procedure TdxRtfParagraphPropertiesExporter.WriteParagraphWrapType(AWrapType: TdxParagraphFrameTextWrapType);
begin
  case AWrapType of
    TdxParagraphFrameTextWrapType.Around:
      RtfBuilder.WriteCommand(TdxRtfExportSR.FrameWrapAround);
    TdxParagraphFrameTextWrapType.None:
      RtfBuilder.WriteCommand(TdxRtfExportSR.FrameWrapOverlay);
    TdxParagraphFrameTextWrapType.NotBeside:
      RtfBuilder.WriteCommand(TdxRtfExportSR.FrameNoWrap);
    TdxParagraphFrameTextWrapType.Through:
      RtfBuilder.WriteCommand(TdxRtfExportSR.FrameWrapThrough);
    TdxParagraphFrameTextWrapType.Tight:
      RtfBuilder.WriteCommand(TdxRtfExportSR.FrameWrapTight);
  end;
end;

{ TdxRtfStyleExporter }

constructor TdxRtfStyleExporter.Create(ADocumentModel: TdxDocumentModel;
  ARtfBuilder: TdxRtfBuilder; const ARtfExportHelper: IdxRtfExportHelper;
  AOptions: TdxRtfDocumentExporterOptions);
begin
  inherited Create;
  FDocumentModel := ADocumentModel;
  FRtfExportHelper := ARtfExportHelper;
  FRtfBuilder := ARtfBuilder;
  FCharacterPropertiesExporter := TdxRtfCharacterPropertiesExporter.Create(ADocumentModel,
    RtfExportHelper, RtfBuilder, AOptions);
  FParagraphPropertiesExporter := TdxRtfParagraphPropertiesExporter.Create(ADocumentModel,
    RtfExportHelper, RtfBuilder);
end;

destructor TdxRtfStyleExporter.Destroy;
begin
  FreeAndNil(FRtfBuilder);
  FreeAndNil(FCharacterPropertiesExporter);
  FreeAndNil(FParagraphPropertiesExporter);
  inherited Destroy;
end;

procedure TdxRtfStyleExporter.ExportCharacterProperties(
  ACharacterProperties: TdxMergedCharacterProperties);
var
  AMergedProperties: TdxMergedCharacterProperties;
begin
  AMergedProperties := TdxMergedCharacterProperties.Create(ACharacterProperties);
  try
    AMergedProperties.Merge(DocumentModel.DefaultCharacterProperties);
    CharacterExporter.ExportCharacterProperties(AMergedProperties, True, False, False);
  finally
    AMergedProperties.Free;
  end;
end;

procedure TdxRtfStyleExporter.ExportCharacterStyle(AStyle: TdxCharacterStyle);
var
  AStyleIndex: Integer;
  AParentStyleIndex: Integer;
  ALinkedStyleIndex: Integer;
  AMergedCharacterProperties: TdxMergedCharacterProperties;
begin
  if AStyle.Deleted or RtfExportHelper.CharacterStylesCollectionIndex.ContainsKey(AStyle.StyleName) then
    Exit;

  AStyleIndex := GetNextFreeStyleIndex;
  RtfExportHelper.CharacterStylesCollectionIndex.Add(AStyle.StyleName, AStyleIndex);
  RtfBuilder.OpenGroup;
  RtfBuilder.WriteCommand(TdxRtfExportSR.CharacterStyle, AStyleIndex);
  AParentStyleIndex := ObtainCharacterStyleIndex(AStyle.Parent);
  if AParentStyleIndex >= 0 then
    RtfBuilder.WriteCommand(TdxRtfExportSR.ParentStyle, AParentStyleIndex);
  if AStyle.HasLinkedStyle then
  begin
    ALinkedStyleIndex := ObtainParagraphStyleIndex(AStyle.LinkedStyle);
    if ALinkedStyleIndex >= 0 then
      RtfBuilder.WriteCommand(TdxRtfExportSR.LinkedStyle, ALinkedStyleIndex);
  end;

  if AStyle.Primary then
    RtfBuilder.WriteCommand(TdxRtfExportSR.QuickFormatStyle);

  AMergedCharacterProperties := AStyle.GetMergedCharacterProperties;
  try
    ExportCharacterProperties(AMergedCharacterProperties);
  finally
    AMergedCharacterProperties.Free;
  end;
  WriteStyleName(AStyle.StyleName);
  RtfBuilder.CloseGroup;
end;

procedure TdxRtfStyleExporter.ExportCharacterStyles(
  ACharacterStyles: TdxCharacterStyleCollection);
var
  ACount: Integer;
  I: Integer;
begin
  RtfBuilder.Clear;
  ACount := ACharacterStyles.Count;

  for I := 0 to ACount - 1 do
    ExportCharacterStyle(ACharacterStyles[I]);

  RtfExportHelper.StylesCollection.Add(RtfBuilder.RtfContent.ToString);
end;

procedure TdxRtfStyleExporter.ExportParagraphProperties(
  AParagraphProperties: TdxParagraphProperties;
  AMergedParagraphProperties: TdxMergedParagraphProperties);
var
  AMergedProperties: TdxMergedParagraphProperties;
  AInfo: TdxParagraphFormattingInfo;
begin
  AMergedProperties := TdxMergedParagraphProperties.Create(AMergedParagraphProperties);
  try
    AMergedProperties.Merge(DocumentModel.DefaultParagraphProperties);
    AInfo := AMergedProperties.Info;
    if AInfo.TopBorder.Style <> TdxBorderLineStyle.None then
      FParagraphPropertiesExporter.WriteParagraphBorder(AInfo.TopBorder, TdxRtfExportSR.TopParagraphBorder);
    if AInfo.LeftBorder.Style <> TdxBorderLineStyle.None then
      FParagraphPropertiesExporter.WriteParagraphBorder(AInfo.LeftBorder, TdxRtfExportSR.LeftParagraphBorder);
    if AInfo.BottomBorder.Style <> TdxBorderLineStyle.None then
      FParagraphPropertiesExporter.WriteParagraphBorder(AInfo.BottomBorder, TdxRtfExportSR.BottomParagraphBorder);
    if AInfo.RightBorder.Style <> TdxBorderLineStyle.None then
      FParagraphPropertiesExporter.WriteParagraphBorder(AInfo.RightBorder, TdxRtfExportSR.RightParagraphBorder);
    FParagraphPropertiesExporter.WriteParagraphAlignment(AInfo.Alignment);
    FParagraphPropertiesExporter.WriteParagraphIndents(AMergedParagraphProperties);
    FParagraphPropertiesExporter.WriteParagraphSuppressHyphenation(AInfo.SuppressHyphenation);
    FParagraphPropertiesExporter.WriteParagraphSuppressLineNumbers(AInfo.SuppressLineNumbers);
    FParagraphPropertiesExporter.WriteParagraphContextualSpacing(AInfo.ContextualSpacing);
    FParagraphPropertiesExporter.WriteParagraphPageBreakBefore(AInfo.PageBreakBefore);
    FParagraphPropertiesExporter.WriteParagraphOutlineLevel(AInfo.OutlineLevel);
    FParagraphPropertiesExporter.WriteParagraphBackColor(AInfo.BackColor);
    FParagraphPropertiesExporter.WriteParagraphLineSpacing(AInfo.LineSpacingType, AInfo.LineSpacing);
    FParagraphPropertiesExporter.WriteParagraphSpacingBefore(AParagraphProperties.SpacingBefore);
    FParagraphPropertiesExporter.WriteParagraphSpacingAfter(AParagraphProperties.SpacingAfter);
  finally
    AMergedProperties.Free;
  end;
end;

procedure TdxRtfStyleExporter.ExportParagraphStyle(AStyle: TdxParagraphStyle; I: Integer);
var
  AStyleIndex: Integer;
  AParentStyleIndex: Integer;
  ALinkedStyleIndex: Integer;
  ANextStyleIndex: Integer;
  AListLevelIndex: Integer;
  AMergedParagraphProperties: TdxMergedParagraphProperties;
  AMergedCharacterProperties: TdxMergedCharacterProperties;
  ATabs: TdxTabFormattingInfo;
begin
  AStyleIndex := ObtainParagraphStyleIndex(AStyle);
  if AStyleIndex < 0 then
    Exit;

  RtfBuilder.OpenGroup;
  if I > 0 then
  begin
    RtfBuilder.WriteCommand(TdxRtfExportSR.ParagraphStyle, AStyleIndex);
    AParentStyleIndex := ObtainParagraphStyleIndex(AStyle.Parent);
    if AParentStyleIndex >= 0 then
      RtfBuilder.WriteCommand(TdxRtfExportSR.ParentStyle, AParentStyleIndex);
  end;
  if AStyle.HasLinkedStyle then
  begin
    ALinkedStyleIndex := ObtainCharacterStyleIndex(AStyle.LinkedStyle);
    if ALinkedStyleIndex >= 0 then
      RtfBuilder.WriteCommand(TdxRtfExportSR.LinkedStyle, ALinkedStyleIndex);
  end;
  if AStyle.NextParagraphStyle <> nil then
  begin
    ANextStyleIndex := ObtainParagraphStyleIndex(AStyle.NextParagraphStyle);
    if ANextStyleIndex >= 0 then
      RtfBuilder.WriteCommand(TdxRtfExportSR.NextStyle, ANextStyleIndex);
  end;

  if AStyle.Primary then
    RtfBuilder.WriteCommand(TdxRtfExportSR.QuickFormatStyle);

  AMergedParagraphProperties := AStyle.GetMergedParagraphProperties;
  try
    ExportParagraphProperties(AStyle.ParagraphProperties, AMergedParagraphProperties);
  finally
    AMergedParagraphProperties.Free;
  end;

  ATabs := AStyle.GetTabs;
  try
    FParagraphPropertiesExporter.WriteTabs(ATabs);
  finally
    ATabs.Free;
  end;

  AMergedCharacterProperties := AStyle.GetMergedCharacterProperties;
  try
    ExportCharacterProperties(AMergedCharacterProperties);
  finally
    AMergedCharacterProperties.Free;
  end;

  if AStyle.GetNumberingListIndex >= 0 then
  begin
    RtfBuilder.WriteCommand(TdxRtfExportSR.ListIndex, GetListId(AStyle.GetNumberingListIndex));
    AListLevelIndex := AStyle.GetListLevelIndex;
    if AListLevelIndex > 0 then
      RtfBuilder.WriteCommand(TdxRtfExportSR.LevelIndex, AListLevelIndex);
  end;
  WriteStyleName(AStyle.StyleName);
  RtfBuilder.CloseGroup;
end;

procedure TdxRtfStyleExporter.ExportParagraphStyles(
  AParagraphStyles: TdxParagraphStyleCollection);
var
  AStyles: TdxList<TdxParagraphStyle>;
  AList: TdxList<TdxParagraphStyle>;
  I: Integer;
  AStylesToWrite: TdxList<TdxParagraphStyle>;
  ACount: Integer;
  AStyle: TdxParagraphStyle;
  AStyleIndex: Integer;
begin
  RtfBuilder.Clear;
  AList := TdxList<TdxParagraphStyle>.Create;
  try
    for I := 0 to AParagraphStyles.Count - 1 do
      AList.Add(AParagraphStyles[I]);
    AStyles := TdxTopologicalSorter<TdxParagraphStyle>.Sort(AList, TdxStyleTopologicalComparer<TdxParagraphStyle>.Create);
    try
      AStylesToWrite := TdxList<TdxParagraphStyle>.Create;
      try
        ACount := AStyles.Count;
        for I := 0 to ACount - 1 do
        begin
          AStyle := AStyles[I];
          if not AStyle.Deleted and not RtfExportHelper.ParagraphStylesCollectionIndex.ContainsKey(AStyle.StyleName) then
          begin
            AStylesToWrite.Add(AStyle);
            AStyleIndex := GetNextFreeStyleIndex;
            RtfExportHelper.ParagraphStylesCollectionIndex.Add(AStyle.StyleName, AStyleIndex);
          end;
        end;
        ACount := AStylesToWrite.Count;
        for I := 0 to ACount - 1 do
          ExportParagraphStyle(AStylesToWrite[I], I);
        RtfExportHelper.StylesCollection.Add(RtfBuilder.RtfContent.ToString);
      finally
        AStylesToWrite.Free;
      end;
    finally
      AStyles.Free;
    end;
  finally
    AList.Free;
  end;
end;

procedure TdxRtfStyleExporter.ExportStyleSheet(
  AParagraphStyles: TdxParagraphStyleCollection;
  ACharacterStyles: TdxCharacterStyleCollection;
  ATableStyles: TdxTableStyleCollection);
begin
  if AParagraphStyles.Count > 0 then
    ExportParagraphStyles(AParagraphStyles);
  if ACharacterStyles.Count > 0 then
    ExportCharacterStyles(ACharacterStyles);
  if ATableStyles.Count > 0 then
    ExportTableStyles(ATableStyles);
end;

procedure TdxRtfStyleExporter.ExportTableCellProperties(
  ATableCellProperties: TdxTableCellProperties;
  AMergedTableCellProperties: TdxMergedTableCellProperties);
var
  AMergedProperties: TdxMergedTableCellProperties;
  AInfo: TdxCombinedCellPropertiesInfo;
begin
  AMergedProperties := TdxMergedTableCellProperties.Create(AMergedTableCellProperties);
  try
    AMergedProperties.Merge(DocumentModel.DefaultTableCellProperties);
    AInfo := AMergedProperties.Info;

    FTableCellPropertiesExporter.WriteCellMerging(ATableCellProperties.VerticalMerging);

    FTableCellPropertiesExporter.WriteCellVerticalAlignment(AInfo.GeneralSettings.VerticalAlignment);
    FTableCellPropertiesExporter.WriteCellBackgroundColor(AInfo.GeneralSettings.BackgroundColor);
    FTableCellPropertiesExporter.WriteCellForegroundColor(AInfo.GeneralSettings.ForegroundColor);
    FTableCellPropertiesExporter.WriteCellShading(AInfo.GeneralSettings.Shading);
    FTableCellPropertiesExporter.WriteCellBasicBorders(AInfo.Borders.TopBorder,
      AInfo.Borders.LeftBorder, AInfo.Borders.RightBorder, AInfo.Borders.BottomBorder);
    FTableCellPropertiesExporter.WriteCellTextDirection(AInfo.GeneralSettings.TextDirection);
    FTableCellPropertiesExporter.WriteCellFitText(AInfo.GeneralSettings.FitText);
    FTableCellPropertiesExporter.WriteCellNoWrap(AInfo.GeneralSettings.NoWrap);
    FTableCellPropertiesExporter.WriteCellHideCellMark(AInfo.GeneralSettings.HideCellMark);
    FTableCellPropertiesExporter.WriteCellPreferredWidth(AInfo.PreferredWidth);
    FTableCellPropertiesExporter.WriteCellMargins(AInfo.CellMargins.Top, AInfo.CellMargins.Left,
      AInfo.CellMargins.Right, AInfo.CellMargins.Bottom);
  finally
    AMergedProperties.Free;
  end;
end;

procedure TdxRtfStyleExporter.ExportTableConditionalStyle(
  AConditionalStyle: TdxTableConditionalStyle; const AStyleName: string;
  AStyleIndex: Integer);
var
  AMergedCharacterProperties: TdxMergedCharacterProperties;
  AMergedParagraphProperties: TdxMergedParagraphProperties;
  AMergedTableRowProperties: TdxMergedTableRowProperties;
  AMergedTableCellProperties: TdxMergedTableCellProperties;
begin
  if AConditionalStyle = nil then
    Exit;

  RtfBuilder.OpenGroup;
  RtfBuilder.WriteCommand(TdxRtfExportSR.TableStyle, AStyleIndex);
  RtfBuilder.WriteCommand(TdxRtfExportSR.TableStyleResetTableProperties);

  AMergedCharacterProperties := AConditionalStyle.GetMergedCharacterProperties;
  try
    ExportCharacterProperties(AMergedCharacterProperties);
  finally
    AMergedCharacterProperties.Free;
  end;
  AMergedParagraphProperties := AConditionalStyle.GetMergedParagraphProperties;
  try
    ExportParagraphProperties(AConditionalStyle.ParagraphProperties, AMergedParagraphProperties);
  finally
    AMergedParagraphProperties.Free;
  end;
  AMergedTableRowProperties := AConditionalStyle.GetMergedTableRowProperties;
  try
    ExportTableRowProperties(AConditionalStyle.TableRowProperties, AMergedTableRowProperties);
  finally
    AMergedTableRowProperties.Free;
  end;
  AMergedTableCellProperties := AConditionalStyle.GetMergedTableCellProperties;
  try
    ExportTableCellProperties(AConditionalStyle.TableCellProperties, AMergedTableCellProperties);
  finally
    AMergedTableCellProperties.Free;
  end;
  WriteConditionalStyleType(AConditionalStyle.ConditionType);
  WriteStyleName(AStyleName);
  RtfBuilder.CloseGroup;
end;

procedure TdxRtfStyleExporter.ExportTableProperties(
  ATableProperties: TdxTableProperties;
  AMergedTableProperties: TdxMergedTableProperties; AExportRowProperties,
  AExportColProperties: Boolean);
var
  AMergedProperties: TdxMergedTableProperties;
begin
  AMergedProperties := TdxMergedTableProperties.Create(AMergedTableProperties);
  try
    AMergedProperties.Merge(DocumentModel.DefaultTableProperties);
    FTablePropertiesExporter.ExportTableProperties(AMergedProperties, AExportRowProperties, AExportColProperties);
  finally
    AMergedProperties.Free;
  end;
end;

procedure TdxRtfStyleExporter.ExportTableRowProperties(
  ATableRowProperties: TdxTableRowProperties;
  AMergedTableRowProperties: TdxMergedTableRowProperties);
var
  AMergedProperties: TdxMergedTableRowProperties;
  AProp: TdxCombinedTableRowPropertiesInfo;
begin
  AMergedProperties := TdxMergedTableRowProperties.Create(AMergedTableRowProperties);
  try
    AMergedProperties.Merge(DocumentModel.DefaultTableRowProperties);
    AProp := AMergedProperties.Info;

    FTableRowPropertiesExporter.WriteRowAlignment(AProp.GeneralSettings.TableRowAlignment);
    FTableRowPropertiesExporter.WriteRowHeight(AProp.Height);
    FTableRowPropertiesExporter.WriteRowHeader(AProp.GeneralSettings.Header);
    FTableRowPropertiesExporter.WriteRowCantSplit(ATableRowProperties.CantSplit);
    FTableRowPropertiesExporter.WriteWidthBefore(AProp.WidthBefore);
    FTableRowPropertiesExporter.WriteWidthAfter(AProp.WidthAfter);
    FTableRowPropertiesExporter.WriteRowCellSpacing(AProp.CellSpacing);
  finally
    AMergedProperties.Free;
  end;
end;

procedure TdxRtfStyleExporter.ExportTableStyle(AStyle: TdxTableStyle);
var
  AStyleIndex: Integer;
  AParentStyleIndex: Integer;
  AMergedCharacterProperties: TdxMergedCharacterProperties;
  AMergedParagraphProperties: TdxMergedParagraphProperties;
  AMergedTableProperties: TdxMergedTableProperties;
  AMergedTableRowProperties: TdxMergedTableRowProperties;
  AMergedTableCellProperties: TdxMergedTableCellProperties;
begin
  if AStyle.Deleted then
    Exit;
  if RtfExportHelper.TableStylesCollectionIndex.ContainsKey(AStyle.StyleName) then
    Exit;
  AStyleIndex := GetNextFreeStyleIndex;
  RtfExportHelper.TableStylesCollectionIndex.Add(AStyle.StyleName, AStyleIndex);
  RtfBuilder.OpenGroup;
  RtfBuilder.WriteCommand(TdxRtfExportSR.TableStyle, AStyleIndex);
  RtfBuilder.WriteCommand(TdxRtfExportSR.TableStyleResetTableProperties);
  AParentStyleIndex := ObtainTableStyleIndex(AStyle.Parent);
  if AParentStyleIndex >= 0 then
    RtfBuilder.WriteCommand(TdxRtfExportSR.ParentStyle, AParentStyleIndex);
  if AStyle.Primary then
    RtfBuilder.WriteCommand(TdxRtfExportSR.QuickFormatStyle);

  AMergedCharacterProperties := AStyle.GetMergedCharacterProperties;
  try
    ExportCharacterProperties(AMergedCharacterProperties);
  finally
    AMergedCharacterProperties.Free;
  end;

  AMergedParagraphProperties := AStyle.GetMergedParagraphProperties;
  try
    ExportParagraphProperties(AStyle.ParagraphProperties, AMergedParagraphProperties);
  finally
    AMergedParagraphProperties.Free;
  end;

  AMergedTableProperties := AStyle.GetMergedTableProperties;
  try
    ExportTableProperties(AStyle.TableProperties, AMergedTableProperties, AStyle.HasRowBandingStyleProperties, AStyle.HasColumnBandingStyleProperties);
  finally
    AMergedTableProperties.Free;
  end;

  AMergedTableRowProperties := AStyle.GetMergedTableRowProperties;
  try
    ExportTableRowProperties(AStyle.TableRowProperties, AMergedTableRowProperties);
  finally
    AMergedTableRowProperties.Free;
  end;

  AMergedTableCellProperties := AStyle.GetMergedTableCellProperties;
  try
    ExportTableCellProperties(AStyle.TableCellProperties, AMergedTableCellProperties);
  finally
    AMergedTableCellProperties.Free;
  end;
  WriteStyleName(AStyle.StyleName);
  RtfBuilder.CloseGroup;

  if AStyle.HasConditionalStyleProperties then
  begin
    AStyle.ConditionalStyleProperties.ForEachStyle(procedure (const Sender: TdxTableConditionalStyle)
      begin
        ExportTableConditionalStyle(Sender, AStyle.StyleName, AStyleIndex);
      end);
  end;
end;

procedure TdxRtfStyleExporter.ExportTableStyles(
  ATableStyles: TdxTableStyleCollection);
var
  ACount, I: Integer;
begin
  RtfBuilder.Clear;
  FTablePropertiesExporter := TdxRtfTableStyleTablePropertiesExporter.Create(DocumentModel, RtfExportHelper, RtfBuilder);
  FTableRowPropertiesExporter := TdxRtfTableRowPropertiesExporter.Create(DocumentModel, RtfExportHelper, RtfBuilder);
  FTableCellPropertiesExporter := TdxRtfTableStyleTableCellPropertiesExporter.Create(DocumentModel, RtfExportHelper, RtfBuilder);
  try
    ACount := ATableStyles.Count;
    for I := 0 to ACount -1 do
      ExportTableStyle(ATableStyles[I]);

    RtfExportHelper.StylesCollection.Add(RtfBuilder.RtfContent.ToString);
  finally
    FreeAndNil(FTablePropertiesExporter);
    FreeAndNil(FTableRowPropertiesExporter);
    FreeAndNil(FTableCellPropertiesExporter);
  end;
end;

function TdxRtfStyleExporter.GetListId(AIndex: TdxNumberingListIndex): Integer;
begin
  Result := DocumentModel.NumberingLists[AIndex].Id;
end;

function TdxRtfStyleExporter.GetNextFreeStyleIndex: Integer;
begin
  Result := 0;
  while RtfExportHelper.CharacterStylesCollectionIndex.ContainsValue(Result) or
      RtfExportHelper.ParagraphStylesCollectionIndex.ContainsValue(Result) or
      RtfExportHelper.TableStylesCollectionIndex.ContainsValue(Result) do
    Inc(Result);
end;

function TdxRtfStyleExporter.ObtainCharacterStyleIndex(
  AStyle: TdxCharacterStyle): Integer;
begin
  Result := ObtainStyleIndex(AStyle, RtfExportHelper.CharacterStylesCollectionIndex);
end;

function TdxRtfStyleExporter.ObtainParagraphStyleIndex(
  AStyle: TdxParagraphStyle): Integer;
begin
  Result := ObtainStyleIndex(AStyle, RtfExportHelper.ParagraphStylesCollectionIndex);
end;

function TdxRtfStyleExporter.ObtainStyleIndex(const AStyle: IdxStyle;
  ACollection: TdxNamedOrdinalDictionary<Integer>): Integer;
begin
  if AStyle = nil then
    Result := -1
  else
  begin
    if not ACollection.TryGetValue(AStyle.StyleName, Result) then
      Result := -1;
  end;
end;

function TdxRtfStyleExporter.ObtainTableStyleIndex(AStyle: TdxTableStyle): Integer;
begin
  Result := ObtainStyleIndex(AStyle, RtfExportHelper.TableStylesCollectionIndex);
end;

procedure TdxRtfStyleExporter.WriteConditionalStyleType(AConditionType: TdxConditionalTableStyleFormattingType);
var
  AKeyword: string;
begin
  if TdxRtfContentExporter.ConditionalStylesTypesTryGetValue(AConditionType, AKeyword) then
    RtfBuilder.WriteCommand(AKeyword);
end;

procedure TdxRtfStyleExporter.WriteStyleName(const AName: string);
var
  ACount: Integer;
  I: Integer;
begin
  ACount := Length(AName);
  for I := 1 to ACount do
    RtfBuilder.WriteChar(AName[I]);
  RtfBuilder.WriteChar(';');
end;

{ TdxExportRtfFormat }

function TdxExportRtfFormat.GetDocumentExporter: IdxExporter;
begin
  Result := TdxRtfDocumentExporter.Create;
end;

class function TdxExportRtfFormat.GetDocumentFormat: TdxRichEditDocumentFormat;
begin
  Result := TdxRichEditDocumentFormat.Rtf;
end;

function TdxExportRtfFormat.GetExporter(ADocumentModel: TdxCustomDocumentModel;
  const AOptions: IdxExporterOptions): TdxCustomDocumentModelExporter;
begin
  Result := TdxRtfExporter.Create(ADocumentModel, AOptions);
end;

end.

