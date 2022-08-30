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

unit dxRichEdit.DocumentLayout;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Windows, Types, Classes, SysUtils, Graphics, Generics.Defaults, Generics.Collections, dxCore, dxCoreClasses, dxGDIPlusClasses,
  dxCoreGraphics, cxGeometry,
  dxRichEdit.DocumentModel.PieceTable,

  dxGDIPlusAPI,

  dxRichEdit.NativeApi,
  dxGenerics,
  dxRichEdit.Utils.Types,
  dxRichEdit.Utils.OfficeImage,
  dxRichEdit.Types,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Borders,
  dxRichEdit.DocumentModel.Boxes.Core,
  dxRichEdit.DocumentModel.Boxes.Simple,
  dxRichEdit.DocumentModel.Boxes,
  dxRichEdit.DocumentModel.CharacterFormatting,
  dxRichEdit.DocumentModel.ParagraphFormatting,
  dxRichEdit.DocumentModel.TabFormatting,
  dxRichEdit.DocumentModel.ShapeFormatting,
  dxRichEdit.DocumentModel.PatternLine,
  dxRichEdit.DocumentModel.InlineObjectFormatting,
  dxRichEdit.DocumentModel.InlineObjectRange,
  dxRichEdit.DocumentModel.FloatingObjectRange,
  dxRichEdit.DocumentModel.Tables,
  dxRichEdit.LayoutEngine.Formatter,
  dxRichEdit.LayoutEngine.BoxMeasurer,
  dxRichEdit.Platform.Win.Painter,
  dxRichEdit.Platform.PatternLinePainter,
  dxRichEdit.Options,
  dxRichEdit.DocumentModel.UnitToLayoutUnitConverter,
  dxRichEdit.Utils.TextColors,
  dxRichEdit.DocumentModel.Hyperlink;

type
  { TdxDocumentLayoutExporterContext }

  TdxDocumentLayoutExporterContext = class
  strict private
    FBackColor: TdxAlphaColor;
  public
    constructor Create;

    procedure CopyFrom(ASource: TdxDocumentLayoutExporterContext);
    function Clone: TdxDocumentLayoutExporterContext;

    property BackColor: TdxAlphaColor read FBackColor write FBackColor;
  end;

  { TdxBackgroundItem }

  TdxBackgroundItem = record
  public
    Color: TdxAlphaColor;
    Bounds: TRect;
    constructor Create(AColor: TdxAlphaColor; const ABounds: TRect);
  end;

  { TdxBackgroundLayer }

  TdxBackgroundLayer = class
  strict private
    FBackgroundItems: TList<TdxBackgroundItem>;
    FLayerBounds: TRect;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetBackColor(AColor: TdxAlphaColor; const ABounds: TRect);
    procedure UpdateBounds(const ABounds: TRect);
    function GetBackColor(const ABounds: TRect): TdxNullableValue<TdxAlphaColor>;
  end;

  { TdxDocumentLayoutExporter }

  TdxDocumentLayoutExporter = class abstract (TcxIUnknownObject,
    IdxSimpleDocumentLayoutExporter,
    IdxDocumentLayoutExporter)
  strict private
    FColumnClipBounds: TdxRectF;
    FPieceTable: TdxPieceTable;
    FMinReadableTextHeight: Integer;
    FCurrentBackColor: TdxAlphaColor;
    FCurrentCellBackColor: TdxAlphaColor;
    FCurrentRow: TdxRow;
    FCurrentCell: TdxTableCellViewInfo;
    FDocumentModel: TdxDocumentModel;
    FShowWhitespace: Boolean;
    FReadOnly: Boolean;
    FHyperlinkCalculators: TObjectDictionary<TdxPieceTable, TdxHyperlinkCalculator>;
    FContextStack: TdxObjectStack<TdxDocumentLayoutExporterContext>;
    FBackgroundLayers: TdxObjectList<TdxBackgroundLayer>;
  protected
    function CalcRowContentBounds(ARow: TdxRow): TRect; virtual;
    procedure SetCurrentCell(ACell: TdxTableCellViewInfo); virtual;
    function GetBoxText(ABox: TdxBox): string;
    function GetLastExportBoxInRowIndex(ARow: TdxRow): Integer; virtual;
    function GetShowWhitespace: Boolean; virtual;
    procedure SetShowWhitespace(const Value: Boolean); virtual;
    function GetActualBackColor(const ABounds: TRect): TdxAlphaColor;
    procedure PushBackgroundLayer;
    procedure PopBackgroundLayer;
    function GetBackColor(const ABounds: TRect): TdxAlphaColor;

    function GetPainter: TdxPainter; virtual; abstract;
    function ApplyClipBounds(const AClipBounds: TdxRectF): TdxRectF;
    procedure RestoreClipBounds(const AClipBounds: TdxRectF); virtual;
    procedure ApplyClipBoundsCore(const ABounds: TdxRectF); virtual;
    procedure RestoreClipBoundsCore(const ABounds: TdxRectF); virtual;
    function GetClipBounds: TdxRectF; virtual;

    procedure SetClipBounds(const AClipBounds: TdxRectF); virtual;
    function IntersectClipBounds(const AOldClipBounds, ABounds: TdxRectF): TdxRectF; virtual;
    procedure FinishExport; virtual;
    function IsValidBounds(const ABoxBounds: TRect): Boolean; virtual;

    procedure AfterExportPageArea; virtual;
    procedure ExportRowBookmarkBoxes; virtual;
    function BeforeExportRotatedContent(ABox: TdxFloatingObjectBox): Boolean; virtual;
    procedure AfterExportRotatedContent(ATransformApplied: Boolean); virtual;
    procedure ExportRows(AColumn: TdxColumn); virtual;
    procedure ExportParagraphFrames(AColumn: TdxColumn; const APredicate: TdxFunc<TdxParagraphFrameBox, Boolean>); overload; virtual;
    procedure ExportParagraphFrames(AParagraphFrames: TdxParagraphFrameBoxList; APieceTable: TdxCustomPieceTable); overload; virtual;
    procedure ExportParagraphBackgroundFrame(AParagraphFrameBox: TdxParagraphFrameBox); virtual;
    procedure ExportParagraphFrame(AParagraphFrameBox: TdxParagraphFrameBox); virtual;
    procedure SetCurrentRow(ARow: TdxRow); virtual;
    function ShouldExportRow(ARow: TdxRow): Boolean; virtual;
    procedure BeginExportTableCellContent(const AClipBounds: TdxRectF); virtual;
    procedure EndExportTableContent(const AOldClipBounds: TdxRectF); virtual;
    function BeginExportTableCell(ACell: TdxTableCellViewInfo): TdxRectF;
    procedure EndExportTableCell(const AOldClipBounds: TdxRectF);
    procedure ExportRowCore; virtual;
    procedure ExportRowContentBoxes; virtual;
    procedure ExportRowUnderlineBoxes; virtual;
    procedure ExportRowStrikeoutBoxes; virtual;
    function BeginHeaderFooterExport(const AClipBounds: TdxRectF): TdxRectF; virtual;
    procedure EndHeaderFooterExport(const AOldClipBounds: TdxRectF); virtual;
    procedure ExportPageHeaderFooter(AArea: TdxHeaderFooterPageAreaBase); virtual;
    function CalculateHeaderFooterClipBounds(const AContentBounds: TdxRectF): TdxRectF;
    procedure ExportTables(AColumn: TdxColumn); virtual;
    procedure ExportTablesBackground(AColumn: TdxColumn); virtual;
    procedure ExportNotRotatedContent(ABox: TdxFloatingObjectBox); overload; virtual;
    function ExportNotRotatedContent(ABox: TdxParagraphFrameBox): Boolean; overload; virtual;
    function ShouldExportComments(APage: TdxPage): Boolean; virtual;
    procedure BeforeExportComments(APage: TdxPage); virtual;
    procedure BeforeExportComment(ACommentViewInfo: TdxCommentViewInfo); virtual;
    function ShouldExportComment(ACommentViewInfo: TdxCommentViewInfo): Boolean; virtual;
    procedure AfterExportComment(ACommentViewInfo: TdxCommentViewInfo; const APageCommentBounds: TRect); virtual;
    function GetHyperlinkCalculator(APieceTable: TdxPieceTable): TdxHyperlinkCalculator;
    function GetUrl(ABox: TdxBox): string;
    function GetAnchor(ABox: TdxBox): string;
  public
    constructor Create(ADocumentModel: TdxDocumentModel);
    destructor Destroy; override;
    procedure ExportParagraphFrameShape(ABox: TdxParagraphFrameBox; AShape: TdxShape); virtual;
    procedure ExportParagraphFramePicture(ABox: TdxParagraphFrameBox; APictureContent: TdxPictureFloatingObjectContent); virtual;
    procedure ExportParagraphFrameTextBox(ABox: TdxParagraphFrameBox; ATextBoxDocumentLayout: TdxDocumentLayout); virtual;
    procedure ExportComment(ACommentViewInfo: TdxCommentViewInfo); virtual;
    procedure ExportCommentStartBox(ABox: TdxVisitableDocumentIntervalBox); virtual;
    procedure ExportCommentEndBox(ABox: TdxVisitableDocumentIntervalBox); virtual;
    procedure ExportPageAreaCore(APage: TdxPage; APieceTable: TdxCustomPieceTable; AAction: TdxExportPageAction);
    procedure ApplyActivePieceTable(APageArea: TdxPageArea);
    procedure RectorePieceTable;
    procedure ExportPageHeaderFooterBase(APageArea: TdxHeaderFooterPageAreaBase);
    procedure ExportPageHeader(APage: TdxPage); virtual;
    procedure ExportPageMainAreas(APage: TdxPage); virtual;
    procedure ExportPageFooter(APage: TdxPage); virtual;
    procedure ExportFloatingObjects(AFloatingObjects: TdxFloatingObjectBoxList; APieceTable: TdxCustomPieceTable);
    procedure ExportPage(APage: TdxPage); overload; virtual;
    procedure ExportPage(APage: TdxPage; const AAction: TdxAction); overload; virtual;
    procedure ExportParagraphFramePage(APage: TdxPage; const APageClipBounds: TdxRectF; AExportContent: Boolean); virtual;
    procedure ExportPageArea(APageArea: TdxPageArea); virtual;
    procedure ExportColumn(AColumn: TdxColumn); virtual;
    procedure ExportSimpleRow(ARow: TdxSimpleRow); virtual;
    procedure ExportRow(ARow: TdxRow); virtual;
    procedure ApplyCurrentRowTableCellClipping(ARow: TdxRow);
    procedure ExportTextBox(ABox: TdxTextBox); virtual;
    procedure ExportSpecialTextBox(ABox: TdxSpecialTextBox); overload; virtual;
    procedure ExportLayoutDependentTextBox(ABox: TdxLayoutDependentTextBox); virtual;
    procedure ExportHyphenBox(ABox: TdxHyphenBox); virtual;
    procedure ExportInlinePictureBox(ABox: TdxInlinePictureBox); virtual;
    function BeginExportCompositeObject(const ABounds: TRect; APieceTable: TdxPieceTable; var AOldClipRect, AClipRect: TdxRectF;
      var AOldPieceTable: TdxPieceTable): Boolean; virtual;
    procedure EndExportCompositeObject(const AOldClipRect: TdxRectF; AOldPieceTable: TdxPieceTable); virtual;
    procedure ExportFloatingObjectBox(ABox: TdxFloatingObjectBox); virtual;
    procedure ExportFloatingObjectShape(TdxBox: TdxFloatingObjectBox; AShape: TdxShape); virtual;
    procedure ExportFloatingObjectPicture(ABox: TdxFloatingObjectBox; APictureContent: TdxPictureFloatingObjectContent); virtual;
    procedure ExportFloatingObjectTextBox(ABox: TdxFloatingObjectBox; ATextBoxContent: TdxTextBoxFloatingObjectContent; ATextBoxDocumentLayout: TdxDocumentLayout); virtual;
    function ExportRotatedContent(ABox: TdxFloatingObjectBox): Boolean; overload; virtual;
    function ExportRotatedContent(ABox: TdxParagraphFrameBox): Boolean; overload; virtual;
    procedure ExportSpaceBox(ABox: TdxBox); virtual;
    procedure ExportTabSpaceBox(ABox: TdxTabSpaceBox); virtual;
    procedure ExportLineBreakBox(ABox: TdxLineBreakBox); virtual;
    procedure ExportParagraphMarkBox(ABox: TdxParagraphMarkBox); virtual;
    procedure ExportSectionMarkBox(ABox: TdxSectionMarkBox); virtual;
    procedure ExportUnderlineBox(ARow: TdxSimpleRow; AUnderlineBox: TdxUnderlineBox); virtual;
    procedure ExportStrikeoutBox(ARow: TdxSimpleRow; AStrikeoutBox: TdxUnderlineBox); virtual;
    procedure ExportErrorBox(AErrorBox: TdxErrorBox); virtual;
    procedure ExportBookmarkStartBox(ABox: TdxVisitableDocumentIntervalBox); virtual;
    procedure ExportBookmarkEndBox(ABox: TdxVisitableDocumentIntervalBox); virtual;
    procedure ExportCustomMarkBox(ABox: TdxCustomMarkBox); virtual;
    procedure ExportSeparatorBox(ABox: TdxSeparatorBox); virtual;
    procedure ExportPageBreakBox(ABox: TdxPageBreakBox); virtual;
    procedure ExportColumnBreakBox(ABox: TdxColumnBreakBox); virtual;
    procedure ExportNumberingListBox(ABox: TdxNumberingListBox); virtual;
    procedure ExportLineNumberBox(ABox: TdxLineNumberBox); virtual;
    procedure ExportTableBorder(ABorder: TdxTableBorderViewInfoBase; const ACellBounds: TRect); virtual;
    procedure ExportTableBorderCorner(ACorner: TdxCornerViewInfoBase; X, Y: Integer); virtual;
    procedure ExportTableCell(ACell: TdxTableCellViewInfo); virtual;
    procedure ExportTableRow(ARow: TdxTableRowViewInfoBase); virtual;
    procedure ExportParagraphFrameBox(ABox: TdxParagraphFrameBox); virtual;
    procedure ExportHighlightArea(const AArea: TdxHighlightArea); virtual;
    procedure SetBackColor(AColor: TdxAlphaColor; const ABounds: TRect);

    function GetDrawingBounds(const ABounds: TRect): TRect; virtual;
    function IsAnchorVisible(const AAnchor: TdxTableCellVerticalAnchor): Boolean; virtual;
    function IsTableRowVisible(ARow: TdxTableRowViewInfoBase): Boolean; virtual;

    property DocumentModel: TdxDocumentModel read FDocumentModel;
    property PieceTable: TdxPieceTable read FPieceTable write FPieceTable;
    property ShowWhitespace: Boolean read GetShowWhitespace write SetShowWhitespace;
    property MinReadableTextHeight: Integer read FMinReadableTextHeight write FMinReadableTextHeight;
    property CurrentBackColor: TdxAlphaColor read FCurrentBackColor write FCurrentBackColor;
    property CurrentCellBackColor: TdxAlphaColor read FCurrentCellBackColor write FCurrentCellBackColor;

    property CurrentRow: TdxRow read FCurrentRow;
    property CurrentCell: TdxTableCellViewInfo read FCurrentCell;
    property Painter: TdxPainter read GetPainter;
    property ReadOnly: Boolean read FReadOnly write FReadOnly;
    property ColumnClipBounds: TdxRectF read FColumnClipBounds write FColumnClipBounds;
  end;

  { TdxBoundedDocumentLayoutExporter }

  TdxBoundedDocumentLayoutExporter = class abstract(TdxDocumentLayoutExporter,
    IdxPatternLinePainter,
    IdxUnderlinePainter,
    IdxStrikeoutPainter,
    IdxCharacterLinePainter)
  private
    FBounds: TRect;
    FVisibleBounds: TRect;
    FHorizontalLinePainter: TdxRichEditPatternLinePainter;
    FVerticalLinePainter: TdxRichEditPatternLinePainter;
    FTextColors: TdxTextColors;
  protected
    function CreateHorizontalLinePainter(const ALinePaintingSupport: IdxPatternLinePaintingSupport): TdxRichEditPatternLinePainter; virtual; abstract;
    function CreateVerticalLinePainter(const ALinePaintingSupport: IdxPatternLinePaintingSupport ): TdxRichEditPatternLinePainter; virtual; abstract;

    function GetCharacterLineBoxByIndex(ARow: TdxSimpleRow; AIndex: Integer): TdxBox;
    function GetOffset: TPoint; virtual;
    function IsValidBounds(const ABoxBounds: TRect): Boolean; override;
    function GetTabLeaderCharacter(ALeaderType: TdxTabLeaderType): Char; virtual;
    function GetTabLeaderCharacterWidth(ABox: TdxTabSpaceBox): Integer; virtual;
    function ShouldCenterTabLeaderLineVertically(ABox: TdxTabSpaceBox): Boolean; virtual;
    function GetTabLeaderUnderlineType(ABox: TdxTabSpaceBox): TdxUnderlineType; virtual;
    procedure ExportRows(AColumn: TdxColumn); override;
    procedure ExportRowBackground(ARow: TdxRow); virtual;
    function ShouldExportRowBackground(ARow: TdxRow): Boolean; virtual;
    function GetTabLeaderText(ABox: TdxTabSpaceBox; const ATextBounds: TRect): string; virtual;
    procedure ExportBackground; virtual;
    procedure ExportTextHighlighting;
    procedure ExportHighlighting(AHighlightAreas: TdxHighlightAreaCollection);
    procedure DrawDoubleSolidLine(const ABounds: TdxRectF; AColor: TdxAlphaColor);
    procedure DrawPatternLine(const ABounds: TdxRectF; AColor: TdxAlphaColor; const APattern: TArray<Single>);
  public
    constructor Create(ADocumentModel: TdxDocumentModel; const ABounds: TRect;
      const ALinePaintingSupport: IdxPatternLinePaintingSupport; ATextColors: TdxTextColors);
    destructor Destroy; override;

    procedure DrawUnderline(AUnderline: TdxUnderlineSingle; const ABounds: TdxRectF; AColor: TdxAlphaColor); overload;
    procedure DrawUnderline(AUnderline: TdxUnderlineDotted; const ABounds: TdxRectF; AColor: TdxAlphaColor); overload;
    procedure DrawUnderline(AUnderline: TdxUnderlineDashed; const ABounds: TdxRectF; AColor: TdxAlphaColor); overload;
    procedure DrawUnderline(AUnderline: TdxUnderlineDashSmallGap; const ABounds: TdxRectF; AColor: TdxAlphaColor); overload;
    procedure DrawUnderline(AUnderline: TdxUnderlineDashDotted; const ABounds: TdxRectF; AColor: TdxAlphaColor); overload;
    procedure DrawUnderline(AUnderline: TdxUnderlineDashDotDotted; const ABounds: TdxRectF; AColor: TdxAlphaColor); overload;
    procedure DrawUnderline(AUnderline: TdxUnderlineDouble; const ABounds: TdxRectF; AColor: TdxAlphaColor); overload;
    procedure DrawUnderline(AUnderline: TdxUnderlineHeavyWave; const ABounds: TdxRectF; AColor: TdxAlphaColor); overload;
    procedure DrawUnderline(AUnderline: TdxUnderlineLongDashed; const ABounds: TdxRectF; AColor: TdxAlphaColor); overload;
    procedure DrawUnderline(AUnderline: TdxUnderlineThickSingle; const ABounds: TdxRectF; AColor: TdxAlphaColor); overload;
    procedure DrawUnderline(AUnderline: TdxUnderlineThickDotted; const ABounds: TdxRectF; AColor: TdxAlphaColor); overload;
    procedure DrawUnderline(AUnderline: TdxUnderlineThickDashed; const ABounds: TdxRectF; AColor: TdxAlphaColor); overload;
    procedure DrawUnderline(AUnderline: TdxUnderlineThickDashDotted; const ABounds: TdxRectF; AColor: TdxAlphaColor); overload;
    procedure DrawUnderline(AUnderline: TdxUnderlineThickDashDotDotted; const ABounds: TdxRectF; AColor: TdxAlphaColor); overload;
    procedure DrawUnderline(AUnderline: TdxUnderlineThickLongDashed; const ABounds: TdxRectF; AColor: TdxAlphaColor); overload;
    procedure DrawUnderline(AUnderline: TdxUnderlineDoubleWave; const ABounds: TdxRectF; AColor: TdxAlphaColor); overload;
    procedure DrawUnderline(AUnderline: TdxUnderlineWave; const ABounds: TdxRectF; AColor: TdxAlphaColor); overload;
    procedure DrawStrikeout(AStrikeout: TdxStrikeoutSingle; const ABounds: TdxRectF; AColor: TdxAlphaColor); overload;
    procedure DrawStrikeout(AStrikeout: TdxStrikeoutDouble; const ABounds: TdxRectF; AColor: TdxAlphaColor); overload;

    function GetDrawingBounds(const ABounds: TRect): TRect; override;
    function GetTableBorderLine(ABorderLineStyle: TdxBorderLineStyle): TdxUnderline; virtual;
    function GetShapeOutlinePenWidth(ARun: TdxFloatingObjectAnchorRun; ABox: TdxFloatingObjectBox): Integer;

    property TextColors: TdxTextColors read FTextColors;
    property Bounds: TRect read FBounds write FBounds;
    property VisibleBounds: TRect read FVisibleBounds write FVisibleBounds;
    property HorizontalLinePainter: TdxRichEditPatternLinePainter read FHorizontalLinePainter;
    property VerticalLinePainter: TdxRichEditPatternLinePainter read FVerticalLinePainter;
    property Offset: TPoint read GetOffset;
  end;

  TdxGraphicsDocumentLayoutExporter = class;

  { TdxGraphicsDocumentLayoutExporterWhitespaceStrategy }

  TdxGraphicsDocumentLayoutExporterWhitespaceStrategy = class abstract
  private
    FExporter: TdxGraphicsDocumentLayoutExporter;
  public
    constructor Create(AExporter: TdxGraphicsDocumentLayoutExporter);

    procedure ExportSpaceBox(ABox: TdxBox); virtual; abstract;
    procedure ExportParagraphMarkBox(ABox: TdxParagraphMarkBox); virtual; abstract;
    procedure ExportSectionMarkBox(ABox: TdxSectionMarkBox); virtual; abstract;
    procedure ExportColumnBreakBox(ABox: TdxColumnBreakBox); virtual; abstract;
    procedure ExportPageBreakBox(ABox: TdxPageBreakBox); virtual; abstract;
    procedure ExportTabSpaceBox(ABox: TdxTabSpaceBox); virtual; abstract;
    procedure ExportLineBreakBox(ABox: TdxLineBreakBox); virtual; abstract;
    procedure ExportSeparatorBox(ABox: TdxSeparatorBox); virtual; abstract;
    procedure ExportNumberingListBoxSeparator(ANumberingListBoxWithSeparator: TdxNumberingListBoxWithSeparator); virtual; abstract;

    property Exporter: TdxGraphicsDocumentLayoutExporter read FExporter;
  end;

  { TdxGraphicsDocumentLayoutExporterShowWhitespaceStrategy }

  TdxGraphicsDocumentLayoutExporterShowWhitespaceStrategy = class(TdxGraphicsDocumentLayoutExporterWhitespaceStrategy)
  public
    procedure ExportSpaceBox(ASpaceBox: TdxBox); override;
    procedure ExportParagraphMarkBox(ABox: TdxParagraphMarkBox); override;
    procedure ExportSectionMarkBox(ABox: TdxSectionMarkBox); override;
    procedure ExportColumnBreakBox(ABox: TdxColumnBreakBox); override;
    procedure ExportPageBreakBox(ABox: TdxPageBreakBox); override;
    procedure ExportTabSpaceBox(ABox: TdxTabSpaceBox); override;
    procedure ExportLineBreakBox(ABox: TdxLineBreakBox); override;
    procedure ExportSeparatorBox(ABox: TdxSeparatorBox); override;
    procedure ExportNumberingListBoxSeparator(ANumberingListBoxWithSeparator: TdxNumberingListBoxWithSeparator); override;
  end;

  { TdxGraphicsDocumentLayoutExporterHideWhitespaceStrategy }

  TdxGraphicsDocumentLayoutExporterHideWhitespaceStrategy = class(TdxGraphicsDocumentLayoutExporterShowWhitespaceStrategy)
  private
    function GetFormattingMarkVisibility: TdxFormattingMarkVisibilityOptions;
  public
    procedure ExportSpaceBox(ABox: TdxBox); override;
    procedure ExportParagraphMarkBox(ABox: TdxParagraphMarkBox); override;
    procedure ExportSectionMarkBox(ABox: TdxSectionMarkBox); override;
    procedure ExportColumnBreakBox(ABox: TdxColumnBreakBox); override;
    procedure ExportPageBreakBox(ABox: TdxPageBreakBox); override;
    procedure ExportTabSpaceBox(ABox: TdxTabSpaceBox); override;
    procedure ExportSeparatorBox(ABox: TdxSeparatorBox); override;
    procedure ExportLineBreakBox(ABox: TdxLineBreakBox); override;

    property FormattingMarkVisibility: TdxFormattingMarkVisibilityOptions read GetFormattingMarkVisibility;
  end;

  { TdxGraphicsDocumentLayoutExporterAdapter }

  TdxGraphicsDocumentLayoutExporterAdapter = class abstract
  public
    procedure ExportInlinePictureBox(AExporter: TdxGraphicsDocumentLayoutExporter; ABox: TdxInlinePictureBox); virtual; abstract;
    procedure ExportTabSpaceBoxCore(AExporter: TdxGraphicsDocumentLayoutExporter; ABox: TdxTabSpaceBox); virtual; abstract;
    procedure ExportSeparatorBoxCore(AExporter: TdxGraphicsDocumentLayoutExporter; ABox: TdxSeparatorBox); virtual; abstract;
    procedure ExportLineBreakBoxCore(AExporter: TdxGraphicsDocumentLayoutExporter; ABox: TdxLineBreakBox); virtual; abstract;
    procedure ExportTabLeader(AExporter: TdxGraphicsDocumentLayoutExporter; ABox: TdxTabSpaceBox); virtual; abstract;
    procedure ExportFloatingObjectPicture(AExporter: TdxGraphicsDocumentLayoutExporter; ABox: TdxFloatingObjectBox;
      APictureContent: TdxPictureFloatingObjectContent); virtual; abstract;
  end;

  { TdxMarkBoxExporterBase }

  TdxMarkBoxExporterBase = class abstract
  public const
    PaddingsWidth = 60;
    SpacingWidth = 6;
  strict private
    FPaddingsWidth: Integer;
    FSpacingWidth: Integer;
    FExporter: TdxGraphicsDocumentLayoutExporter;
    function GetDocumentModel: TdxDocumentModel;
    function GetPieceTable: TdxPieceTable;
  protected
    function GetText: string; virtual; abstract;
    procedure DrawLine(const ALineBounds: TRect; AForeColor: TdxAlphaColor); virtual; abstract;
  public
    constructor Create(AExporter: TdxGraphicsDocumentLayoutExporter);
    procedure Export(ABox: TdxBox; const ABoxBounds: TRect);

    property Exporter: TdxGraphicsDocumentLayoutExporter read FExporter;
    property DocumentModel: TdxDocumentModel read GetDocumentModel;
    property PieceTable: TdxPieceTable read GetPieceTable;
    property Text: string read GetText;
  end;

  { TdxGraphicsDocumentLayoutExporter }

  TdxGraphicsDocumentLayoutExporter = class(TdxBoundedDocumentLayoutExporter)
  protected type
    TAction = reference to procedure(const AOldClipRect, AClipRect: TdxRectF);
  strict private
    FPainter: TdxPainter;
    FAdapter: TdxGraphicsDocumentLayoutExporterAdapter;
    FWhitespaceStrategy: TdxGraphicsDocumentLayoutExporterWhitespaceStrategy;
    FHidePartiallyVisibleRow: Boolean;
    FDrawInactivePieceTableWithDifferentColor: Boolean;
  protected
    function GetPainter: TdxPainter; override;
    function GetShowWhitespace: Boolean; override;
    procedure SetShowWhitespace(const AValue: Boolean); override;
    function CreateHorizontalLinePainter(const ALinePaintingSupport: IdxPatternLinePaintingSupport): TdxRichEditPatternLinePainter; override;
    function CreateVerticalLinePainter(const ALinePaintingSupport: IdxPatternLinePaintingSupport): TdxRichEditPatternLinePainter; override;
    function GetActualColor(AColor: TdxAlphaColor): TdxAlphaColor; virtual;
    function GetEffectivePieceTable(APieceTable: TdxPieceTable): TdxPieceTable; virtual;
    function ShouldGrayContent: Boolean; virtual;
    function ShouldExportRowBackground(ARow: TdxRow): Boolean; override;
    function ShouldExportRow(ARow: TdxRow): Boolean; override;
    function IsRowContainsVerticalMergingCell(ARow: TdxTableRow): Boolean; virtual;
    procedure ExportBackground; override;
    procedure ExportFieldsHighlighting; virtual;
    function BeforeExportRotatedContent(ABox: TdxFloatingObjectBox): Boolean; override;
    procedure AfterExportRotatedContent(ATransformApplied: Boolean); override;
    procedure ApplyClipBoundsCore(const ABounds: TdxRectF); override;
    procedure RestoreClipBoundsCore(const ABounds: TdxRectF); override;
    procedure ExportCompositeObject(const ABounds: TRect; APieceTable: TdxPieceTable;
      const AAction: TdxGraphicsDocumentLayoutExporter.TAction);
    procedure ExportTextBoxCore(ABox: TdxBox); overload; virtual;
    procedure ExportTextBoxCoreNoCheckBoundsValidity(ABox: TdxBox); virtual;
    procedure ExportNumberingListBoxCore(ABox: TdxBox); virtual;
    procedure ExportTextBoxCore(ABox: TdxBox; const ATextBounds: TRect; const AText: string); overload; virtual;
    procedure ExportSpaceBoxCore(ABox: TdxBox; const ATextBounds: TRect; const AText: string); virtual;
    procedure ExportTabLeaderAsCharacterSequence(ABox: TdxTabSpaceBox); virtual;
    procedure ExportTabLeaderAsUnderline(ABox: TdxTabSpaceBox); virtual;
    procedure ExportParagraphMarkBoxCore(ABox: TdxParagraphMarkBox); virtual;
    procedure ExportSectionMarkBoxCore(ABox: TdxSectionMarkBox); virtual;
    procedure ExportColumnBreakBoxCore(ABox: TdxColumnBreakBox); virtual;
    procedure ExportPageBreakBoxCore(ABox: TdxPageBreakBox); virtual;
    procedure ExportMarkBoxCore(ABox: TdxBox; AExporter: TdxMarkBoxExporterBase); virtual;
    function GetBorderPainter(ACorner: TdxCornerViewInfoBase): TdxTableCornerPainter; virtual;
    function GetClipBounds: TdxRectF; override;
    procedure SetClipBounds(const AClipBounds: TdxRectF); override;
    function BeginHeaderFooterExport(const AClipBounds: TdxRectF): TdxRectF; override;
    procedure EndHeaderFooterExport(const AOldClipBounds: TdxRectF); override;
    procedure BeginExportTableCellContent(const AClipBounds: TdxRectF); override;
    procedure EndExportTableContent(const AOldClipBounds: TdxRectF); override;
  public
    constructor Create(ADocumentModel: TdxDocumentModel; APainter: TdxPainter; AAdapter: TdxGraphicsDocumentLayoutExporterAdapter;
      const ABounds: TRect; ATextColors: TdxTextColors); overload;
    constructor Create(ADocumentModel: TdxDocumentModel; APainter: TdxPainter; AAdapter: TdxGraphicsDocumentLayoutExporterAdapter;
      const ABounds: TRect; AHidePartiallyVisibleRow: Boolean; ATextColors: TdxTextColors); overload;
    destructor Destroy; override;
    procedure ExportRow(ARow: TdxRow); override;
    function GetActualForeColor(ABox: TdxBox; const ABounds: TRect): TdxAlphaColor;
    function IsTableRowVisible(ARow: TdxTableRowViewInfoBase): Boolean; override;
    function IsAnchorVisible(const AAnchor: TdxTableCellVerticalAnchor): Boolean; override;
    procedure ExportHighlightArea(const AArea: TdxHighlightArea); override;
    procedure ExportTextBox(ABox: TdxTextBox); override;
    procedure ExportLayoutDependentTextBox(ABox: TdxLayoutDependentTextBox); override;
    procedure ExportNumberingListBox(ABox: TdxNumberingListBox); override;
    procedure ExportLineNumberBox(ABox: TdxLineNumberBox); override;
    procedure ExportSpaceBox(ABox: TdxBox); override;
    procedure ExportSeparatorBox(ABox: TdxSeparatorBox); override;
    procedure ExportInlinePictureBox(ABox: TdxInlinePictureBox); override;
    procedure ExportFloatingObjectShape(ABox: TdxFloatingObjectBox; AShape: TdxShape); override;
    procedure ExportParagraphFrameShape(ABox: TdxParagraphFrameBox; AShape: TdxShape); override;
    procedure FillRectangle(AFillColor: TdxAlphaColor; const AActualContentBounds: TRect);
    procedure DrawParagraphBordersWithCorners(const AActualContentBounds: TRect; ABoxParagraphProperties: TdxParagraphProperties);
    class function GetActualBoxHeight(const AActualBounds: TRect; ARows: TdxRowCollection; ACurrentRow: TdxRow; const ARowBounds: TRect): Integer; static;
    procedure DrawParagraphBackground(const AActualBounds: TRect; ARows: TdxRowCollection);
    procedure DrawParagraphBordersWithoutTableBounds(const AActualBounds: TRect; ABoxParagraphProperties: TdxParagraphProperties; ARows: TdxRowCollection);
    procedure DrawParagraphBorders(const AContentBounds: TRect; AParagraphProperties: TdxParagraphProperties);
    procedure DrawParagraphBordersCorners(const AContentBounds: TRect; AParagraphProperties: TdxParagraphProperties);
    procedure ExportFloatingObjectPicture(ABox: TdxFloatingObjectBox; APictureContent: TdxPictureFloatingObjectContent); override;
    procedure ExportFloatingObjectTextBox(ABox: TdxFloatingObjectBox; ATextBoxContent: TdxTextBoxFloatingObjectContent; ATextBoxDocumentLayout: TdxDocumentLayout); override;
    procedure ExportParagraphFrameTextBox(ABox: TdxParagraphFrameBox; ATextBoxDocumentLayout: TdxDocumentLayout); override;
    function BeginExportCompositeObject(const ABounds: TRect; APieceTable: TdxPieceTable; var AOldClipRect, AClipRect: TdxRectF;
      var AOldPieceTable: TdxPieceTable): Boolean; override;
    procedure EndExportCompositeObject(const AOldClipRect: TdxRectF; AOldPieceTable: TdxPieceTable); override;
    procedure ExportComment(ACommentViewInfo: TdxCommentViewInfo); override;
    procedure ExportHyphenBox(ABox: TdxHyphenBox); override;
    procedure ExportTabSpaceBox(ABox: TdxTabSpaceBox); override;
    procedure ExportLineBreakBox(ABox: TdxLineBreakBox); override;
    procedure ExportParagraphMarkBox(ABox: TdxParagraphMarkBox); override;
    function ExportRotatedContent(ABox: TdxFloatingObjectBox): Boolean; override;
    procedure ExportSectionMarkBox(ABox: TdxSectionMarkBox); override;
    procedure ExportColumnBreakBox(ABox: TdxColumnBreakBox); override;
    procedure ExportPageBreakBox(ABox: TdxPageBreakBox); override;
    procedure ExportTableBorder(ABorder: TdxTableBorderViewInfoBase; const ACellBounds: TRect); override;
    procedure ExportTableBorderCorner(ACorner: TdxCornerViewInfoBase; X: Integer; Y: Integer); override;
    procedure ExportTableCell(ACell: TdxTableCellViewInfo); override;
    procedure ExportTableRow(ARow: TdxTableRowViewInfoBase); override;
    procedure ExcludeCellBounds(ACells: TdxTableCellViewInfoCollection; const ARowBounds: TRect);
    function GetActualBackgroundColor(ARow: TdxTableRowViewInfoBase): TdxAlphaColor;
    procedure ExportUnderlineBox(ARow: TdxSimpleRow; AUnderlineBox: TdxUnderlineBox); override;
    procedure ExportStrikeoutBox(ARow: TdxSimpleRow; AStrikeoutBox: TdxUnderlineBox); override;

    property Adapter: TdxGraphicsDocumentLayoutExporterAdapter read FAdapter;
    property DrawInactivePieceTableWithDifferentColor: Boolean read FDrawInactivePieceTableWithDifferentColor write FDrawInactivePieceTableWithDifferentColor;
    property HidePartiallyVisibleRow: Boolean read FHidePartiallyVisibleRow write FHidePartiallyVisibleRow;
  end;

  { TdxScreenOptimizedGraphicsDocumentLayoutExporter }

  TdxScreenOptimizedGraphicsDocumentLayoutExporter = class(TdxGraphicsDocumentLayoutExporter)
  strict private
    FPixel: Integer;
    FErrorWavyLinePen: TdxGPPen;
    FScaleFactor: Single;
    function CreateErrorWavyLinePen: TdxGPPen;
    function CreatePlaceholderBrush(AForeColor: TdxAlphaColor): TdxGPHatchBrush;
    function GetActualBounds(const ABounds: TRect): TdxRectF;
  public
    constructor Create(ADocumentModel: TdxDocumentModel; APainter: TdxPainter; AAdapter: TdxGraphicsDocumentLayoutExporterAdapter;
      const ABounds: TRect; ATextColors: TdxTextColors; AScaleFactor: Single);
    destructor Destroy; override;
    procedure FinishExport; override;
    function ShouldOptimizeBox(ABox: TdxBox): Boolean; virtual;
    procedure ExportRowStrikeoutBoxes; override;
    procedure ExportRowUnderlineBoxes; override;
    procedure ExportHyphenBox(ABox: TdxHyphenBox); override;
    procedure ExportParagraphMarkBox(ABox: TdxParagraphMarkBox); override;
    procedure ExportSpaceBox(ABox: TdxBox); override;
    procedure ExportSeparatorBox(ABox: TdxSeparatorBox); override;
    procedure ExportTextBox(ABox: TdxTextBox); override;
    procedure ExportLayoutDependentTextBox(ABox: TdxLayoutDependentTextBox); override;
    procedure ExportNumberingListBox(ABox: TdxNumberingListBox); override;
    procedure ExportLineNumberBox(ABox: TdxLineNumberBox); override;
    procedure ExportBoxOptimized(ABox: TdxBox); virtual;
    procedure ExportErrorBox(AErrorBox: TdxErrorBox); override;

    property ScaleFactor: Single read FScaleFactor;
  end;

  { TdxScreenOptimizedGraphicsDocumentLayoutExporterNoLineNumbers }

  TdxScreenOptimizedGraphicsDocumentLayoutExporterNoLineNumbers = class(TdxScreenOptimizedGraphicsDocumentLayoutExporter)
  public
    procedure ExportLineNumberBox(ABox: TdxLineNumberBox); override;
  end;

  { TdxWinFormsGraphicsDocumentLayoutExporterAdapter }

  TdxWinFormsGraphicsDocumentLayoutExporterAdapter = class(TdxGraphicsDocumentLayoutExporterAdapter)
  strict private
    procedure ExportInlinePictureBoxCore(AExporter: TdxGraphicsDocumentLayoutExporter; ABox: TdxInlinePictureBox;
      AImg: TdxOfficeImage; const AImgBounds: TRect; const ASize: TSize; ASizing: TdxImageSizeMode);
  public
    destructor Destroy; override;
    procedure ExportLineBoxCore<T>(AExporter: TdxGraphicsDocumentLayoutExporter;
      const ALinePainter: IdxPatternLinePainter; ALineBox: TdxUnderlineBox; ALine: TdxPatternLine<T>;
      ALineColor: TdxAlphaColor);
    procedure ExportInlinePictureBox(AExporter: TdxGraphicsDocumentLayoutExporter; ABox: TdxInlinePictureBox); override;
    procedure ExportFloatingObjectPicture(AExporter: TdxGraphicsDocumentLayoutExporter; ABox: TdxFloatingObjectBox;
      APictureContent: TdxPictureFloatingObjectContent); override;
    procedure ExportTabSpaceBoxCore(AExporter: TdxGraphicsDocumentLayoutExporter; ABox: TdxTabSpaceBox); override;
    procedure ExportSeparatorBoxCore(AExporter: TdxGraphicsDocumentLayoutExporter; ABox: TdxSeparatorBox); override;
    procedure ExportSingleCharacterMarkBoxCore(AExporter: TdxGraphicsDocumentLayoutExporter; ABox: TdxSingleCharacterMarkBox);
    procedure ExportLineBreakBoxCore(AExporter: TdxGraphicsDocumentLayoutExporter; ABox: TdxLineBreakBox); override;
    procedure DrawLineBreakArrow(AExporter: TdxGraphicsDocumentLayoutExporter; const AGlyphBounds: TRect; AForeColor: TdxAlphaColor); virtual;
    procedure ExportTabLeader(AExporter: TdxGraphicsDocumentLayoutExporter; ABox: TdxTabSpaceBox); override;
  end;

  { TdxCharacterBoxLevelDocumentLayoutExporter }

  TdxCharacterBoxLevelDocumentLayoutExporter = class(TdxDocumentLayoutExporter)
  private
    FChars: TdxCharacterBoxCollection;
    FMeasurer: TdxBoxMeasurer;
    FRowBounds: TRect;
    procedure AppendCharacterBox(const AStartPos: TdxFormatterPosition;
      AOffset: Integer; const ABounds, ATightBounds: TRect);
    function CalculateCharacterBounds(ABox: TdxTextBox): TArray<TRect>;
    function GetActualCharacterBounds(const ABounds: TRect): TRect;
    procedure ExportMultiCharacterBox(ABox: TdxBox; ACharacterBounds: TArray<TRect>);
  protected
    function GetPainter: TdxPainter; override;
    procedure ExportRowCore; override;
    procedure ExportSingleCharacterBox(ABox: TdxBox);
  public
    constructor Create(ADocumentModel: TdxDocumentModel;
      AChars: TdxCharacterBoxCollection; AMeasurer: TdxBoxMeasurer);
    procedure ExportTextBox(ABox: TdxTextBox); override;
    procedure ExportLayoutDependentTextBox(ABox: TdxLayoutDependentTextBox);
      override;
    procedure ExportSpaceBox(ABox: TdxBox); override;
    procedure ExportHyphenBox(ABox: TdxHyphenBox); override;
    procedure ExportInlinePictureBox(ABox: TdxInlinePictureBox); override;
    procedure ExportTabSpaceBox(ABox: TdxTabSpaceBox); override;
    procedure ExportSeparatorBox(ABox: TdxSeparatorBox); override;
    procedure ExportParagraphMarkBox(ABox: TdxParagraphMarkBox); override;
    procedure ExportRowBox(ARow: TdxRow; ABox: TdxBox); virtual;
    procedure ExportSectionMarkBox(ABox: TdxSectionMarkBox); override;
    procedure ExportLineBreakBox(ABox: TdxLineBreakBox); override;
    procedure ExportPageBreakBox(ABox: TdxPageBreakBox); override;
    procedure ExportColumnBreakBox(ABox: TdxColumnBreakBox); override;
    procedure ExportNumberingListBox(ABox: TdxNumberingListBox); override;

    property Chars: TdxCharacterBoxCollection read FChars;
    property Measurer: TdxBoxMeasurer read FMeasurer;
  end;

  { TdxGraphicsPainterWrapper }

  TdxGraphicsPainterWrapper = class(TcxIUnknownObject, IdxPainterWrapper)
  private
    FPainter: TdxPainter;
    FHorizontalLinePainter: IdxCharacterLinePainter;
    FVerticalLinePainter: IdxCharacterLinePainter;
    function GetHorizontalLinePainter: IdxCharacterLinePainter;
    function GetVerticalLinePainter: IdxCharacterLinePainter;
  public
    constructor Create(APainter: TdxPainter; const AHorizontalLinePainter, AVerticalLinePainter: IdxCharacterLinePainter);
    procedure SnapWidths(var AWidths: TArray<Single>);
    procedure SnapHeights(var AHeights: TArray<Single>);
    procedure FillRectangle(AColor: TdxAlphaColor; const ABounds: TdxRectF);
    function GetSnappedPoint(const APoint: TdxPointF): TdxPointF;

    property HorizontalLinePainter: IdxCharacterLinePainter read GetHorizontalLinePainter;
    property VerticalLinePainter: IdxCharacterLinePainter read GetVerticalLinePainter;
  end;

  { TdxGraphicsDocumentLayoutExporterTableBorder }

  TdxGraphicsDocumentLayoutExporterTableBorder = class
  private
    FPainter: TdxPainter;
    FHorizontalLinePainter: TdxRichEditPatternLinePainter;
    FVerticalLinePainter: TdxRichEditPatternLinePainter;
    FDocumentModel: TdxDocumentModel;
    FBounds: TRect;
  protected
    function GetOffset: TPoint; virtual;
    function GetBorderPainter(ABorder: TdxBorderInfo; AConverter: TdxDocumentModelUnitToLayoutUnitConverter;
      APainterWrapper: TdxGraphicsPainterWrapper): TdxTableBorderPainter; overload; virtual;
    function GetTableBorderLine(ABorderLineStyle: TdxBorderLineStyle; ADocumentModel: TdxDocumentModel): TdxUnderline; overload; virtual;
    function GetBorderPainter(ACorner: TdxCornerViewInfoBase): TdxTableCornerPainter; overload; virtual;

    property Offset: TPoint read GetOffset;
    property HorizontalLinePainter: TdxRichEditPatternLinePainter read FHorizontalLinePainter;
    property VerticalLinePainter: TdxRichEditPatternLinePainter read FVerticalLinePainter;
  public
    constructor Create(ADocumentModel: TdxDocumentModel; APainter: TdxPainter;
      AHorizontalLinePainter: TdxRichEditPatternLinePainter; AVerticalLinePainter: TdxRichEditPatternLinePainter;
      const ABounds: TRect); overload;
    constructor Create(ADocumentModel: TdxDocumentModel; APainter: TdxPainter;
      AHorizontalLinePainter: TdxRichEditPatternLinePainter; AVerticalLinePainter: TdxRichEditPatternLinePainter); overload;
    procedure ExportTableBorder(ABorder: TdxBorderInfo; const ADrawingBounds: TRect;
      AConverter: TdxDocumentModelUnitToLayoutUnitConverter; const AViewInfo: TdxTableBorderViewInfoBase); virtual;
    procedure ExportTableBorderCorner(ACorner: TdxCornerViewInfoBase; X, Y: Integer); virtual;

    property Painter: TdxPainter read FPainter;
  end;

  { TdxColumnBreakMarkBoxExporter }

  TdxColumnBreakMarkBoxExporter = class(TdxMarkBoxExporterBase)
  protected
    function GetText: string; override;
    procedure DrawLine(const ALineBounds: TRect; AForeColor: TdxAlphaColor); override;
  end;

  { TdxPageBreakMarkBoxExporter }

  TdxPageBreakMarkBoxExporter = class(TdxMarkBoxExporterBase)
  protected
    function GetText: string; override;
    procedure DrawLine(const ALineBounds: TRect; AForeColor: TdxAlphaColor); override;
  end;

  { TdxSectionBreakMarkBoxExporter }

  TdxSectionBreakMarkBoxExporter = class(TdxMarkBoxExporterBase)
  protected
    function GetText: string; override;
    procedure DrawLine(const ALineBounds: TRect; AForeColor: TdxAlphaColor); override;
  end;

implementation

uses
{$IFDEF DELPHIXE2}
  System.UITypes,
{$ENDIF}
  Math, StrUtils, RTLConsts, Contnrs,
  dxTypeHelpers, cxGraphics, dxSpellCheckerCore,
  dxCharacters,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.DocumentModel.ParagraphRange,
  dxRichEdit.DocumentModel.TableFormatting,
  dxRichEdit.DocumentModel.Tables.Core,
  dxRichEdit.Platform.Font,
  dxRichEdit.Utils.Exceptions,
  dxRichEdit.Utils.Graphics;

function RectSplitHorizontally(const R: TRect; ACount: Integer): TArray<TRect>;
var
  AWidth: Integer;
  I: Integer;
  ALeft: Integer;
begin
  SetLength(Result, ACount);
  ALeft := R.Left;
  for I := 0 to ACount - 2 do
  begin
    AWidth := (R.Right - ALeft) div (ACount - I);
    Result[I] := TRect.Create(ALeft, R.Top, ALeft + AWidth, R.Bottom);
    ALeft := ALeft + AWidth;
  end;
  Result[ACount - 1] := TRect.Create(ALeft, R.Top, R.Right, R.Bottom);
end;

{ TdxDocumentLayoutExporterContext }

constructor TdxDocumentLayoutExporterContext.Create;
begin
  inherited Create;
  FBackColor := TdxAlphaColors.Empty;
end;

procedure TdxDocumentLayoutExporterContext.CopyFrom(ASource: TdxDocumentLayoutExporterContext);
begin
  BackColor := ASource.BackColor;
end;

function TdxDocumentLayoutExporterContext.Clone: TdxDocumentLayoutExporterContext;
begin
  Result := TdxDocumentLayoutExporterContext.Create;
  Result.BackColor := BackColor;
end;

{ TdxBackgroundItem }

constructor TdxBackgroundItem.Create(AColor: TdxAlphaColor; const ABounds: TRect);
begin
  Color := AColor;
  Bounds := ABounds;
end;

{ TdxBackgroundLayer }

constructor TdxBackgroundLayer.Create;
begin
  inherited Create;
  FBackgroundItems := TList<TdxBackgroundItem>.Create;
end;

destructor TdxBackgroundLayer.Destroy;
begin
  FBackgroundItems.Free;
  inherited Destroy;
end;

procedure TdxBackgroundLayer.SetBackColor(AColor: TdxAlphaColor; const ABounds: TRect);
begin
  FBackgroundItems.Add(TdxBackgroundItem.Create(AColor, ABounds));
  UpdateBounds(ABounds);
end;

procedure TdxBackgroundLayer.UpdateBounds(const ABounds: TRect);
var
  ALeft, ATop, ARight, ABottom: Integer;
begin
  if FLayerBounds.IsEmpty then
    FLayerBounds := ABounds
  else
  begin
    ALeft := Min(FLayerBounds.Left, ABounds.Left);
    ATop := Min(FLayerBounds.Top, ABounds.Top);
    ARight := Max(FLayerBounds.Right, ABounds.Right);
    ABottom := Max(FLayerBounds.Bottom, ABounds.Bottom);
    FLayerBounds.Init(ALeft, ATop, ARight, ABottom);
  end;
end;

function TdxBackgroundLayer.GetBackColor(const ABounds: TRect): TdxNullableValue<TdxAlphaColor>;
var
  I: Integer;
  AItem: TdxBackgroundItem;
begin
  if FLayerBounds.IntersectsWith(ABounds) then
  begin
    for I := FBackgroundItems.Count - 1 downto 0 do
    begin
      AItem := FBackgroundItems[I];
      if AItem.Bounds.IntersectsWith(ABounds) then
        Exit(AItem.Color);
    end;
  end;
  Result.Reset
end;

{ TdxDocumentLayoutExporter }

constructor TdxDocumentLayoutExporter.Create(ADocumentModel: TdxDocumentModel);
begin
  inherited Create;
  FCurrentBackColor := TdxAlphaColors.Empty;
  FCurrentCellBackColor := TdxAlphaColors.Empty;
  Assert(ADocumentModel <> nil);
  FDocumentModel := ADocumentModel;
  FPieceTable := ADocumentModel.MainPieceTable;
  FHyperlinkCalculators := TObjectDictionary<TdxPieceTable, TdxHyperlinkCalculator>.Create([doOwnsValues]);
  FContextStack := TdxObjectStack<TdxDocumentLayoutExporterContext>.Create(False);
  FBackgroundLayers := TdxObjectList<TdxBackgroundLayer>.Create;
  PushBackgroundLayer;
end;

destructor TdxDocumentLayoutExporter.Destroy;
begin
  SetCurrentRow(nil);
  FContextStack.Free;
  FBackgroundLayers.Free;
  FHyperlinkCalculators.Free;
  inherited Destroy;
end;

procedure TdxDocumentLayoutExporter.ExportParagraphFrameShape(ABox: TdxParagraphFrameBox; AShape: TdxShape);
begin
end;

procedure TdxDocumentLayoutExporter.ExportParagraphFramePicture(ABox: TdxParagraphFrameBox; APictureContent: TdxPictureFloatingObjectContent);
begin
end;

procedure TdxDocumentLayoutExporter.ExportParagraphFrameTextBox(ABox: TdxParagraphFrameBox; ATextBoxDocumentLayout: TdxDocumentLayout);
begin
end;

procedure TdxDocumentLayoutExporter.ExportComment(ACommentViewInfo: TdxCommentViewInfo);
begin
end;

procedure TdxDocumentLayoutExporter.ExportCommentStartBox(ABox: TdxVisitableDocumentIntervalBox);
begin
end;

procedure TdxDocumentLayoutExporter.ExportCommentEndBox(ABox: TdxVisitableDocumentIntervalBox);
begin
end;

function TdxDocumentLayoutExporter.ApplyClipBounds(const AClipBounds: TdxRectF): TdxRectF;
var
  AActualClipBounds: TdxRectF;
begin
  Result := GetClipBounds;
  AActualClipBounds := Result;
  if (AActualClipBounds.Left = NegInfinity) and (AActualClipBounds.Top = NegInfinity) and
    (AActualClipBounds.Width = Infinity) and (AActualClipBounds.Height = Infinity) then
    AActualClipBounds := AClipBounds
  else
    AActualClipBounds.Intersect(AClipBounds);
  ApplyClipBoundsCore(AActualClipBounds);
end;

procedure TdxDocumentLayoutExporter.ApplyCurrentRowTableCellClipping(ARow: TdxRow);
var
  ACellRow: TdxTableCellRow;
  ACell: TdxTableCellViewInfo;
  AClipBounds: TdxRectF;
begin
  ACellRow := Safe<TdxTableCellRow>.Cast(ARow);
  if ACellRow <> nil then
  begin
    ACell := ACellRow.CellViewInfo;
    if ACell <> FCurrentCell then
    begin
      AClipBounds := GetDrawingBounds(ACell.TableViewInfo.GetCellBounds(ACell)).ToRectF;
      RestoreClipBounds(ColumnClipBounds);
      BeginExportTableCellContent(AClipBounds);
      FCurrentCell := ACell;
      FCurrentCellBackColor := ACell.Cell.Properties.BackgroundColor;
    end;
  end
  else
  begin
    if FCurrentCell <> nil then
    begin
      SetClipBounds(ColumnClipBounds);
      FCurrentCell := nil;
      FCurrentCellBackColor := TdxAlphaColors.Empty;
    end;
  end;
end;

function TdxDocumentLayoutExporter.CalcRowContentBounds(ARow: TdxRow): TRect;
begin
  Result := ARow.Bounds;
end;

procedure TdxDocumentLayoutExporter.ExportColumn(AColumn: TdxColumn);
begin
  ColumnClipBounds := GetClipBounds;
  PushBackgroundLayer;
  ExportParagraphFrames(AColumn,
    function (const AParagraphFrameBox: TdxParagraphFrameBox): Boolean
    begin
      Result := not AParagraphFrameBox.IsInCell;
    end);
  ExportTablesBackground(AColumn);
  ExportParagraphFrames(AColumn,
    function (const AParagraphFrameBox: TdxParagraphFrameBox): Boolean
    begin
      Result := AParagraphFrameBox.IsInCell;
    end);
  ExportRows(AColumn);
  RestoreClipBounds(ColumnClipBounds);

  ApplyClipBounds(ColumnClipBounds);
  ExportTables(AColumn);
  PopBackgroundLayer;
  RestoreClipBounds(ColumnClipBounds);
end;

function TdxDocumentLayoutExporter.BeginExportCompositeObject(const ABounds: TRect; APieceTable: TdxPieceTable;
  var AOldClipRect, AClipRect: TdxRectF; var AOldPieceTable: TdxPieceTable): Boolean;
begin
  Result := True;
end;

procedure TdxDocumentLayoutExporter.EndExportCompositeObject(const AOldClipRect: TdxRectF; AOldPieceTable: TdxPieceTable);
begin
end;

procedure TdxDocumentLayoutExporter.ExportFloatingObjectBox(ABox: TdxFloatingObjectBox);
begin
  if not ExportRotatedContent(ABox) then
    ExportNotRotatedContent(ABox);
end;

procedure TdxDocumentLayoutExporter.ExportHyphenBox(ABox: TdxHyphenBox);
begin
end;

procedure TdxDocumentLayoutExporter.ExportInlinePictureBox(ABox: TdxInlinePictureBox);
begin
end;

procedure TdxDocumentLayoutExporter.ExportLayoutDependentTextBox(ABox: TdxLayoutDependentTextBox);
begin
end;

procedure TdxDocumentLayoutExporter.ExportFloatingObjectShape(TdxBox: TdxFloatingObjectBox; AShape: TdxShape);
begin
end;

procedure TdxDocumentLayoutExporter.ExportFloatingObjectPicture(ABox: TdxFloatingObjectBox; APictureContent: TdxPictureFloatingObjectContent);
begin
end;

procedure TdxDocumentLayoutExporter.ExportFloatingObjects(AFloatingObjects: TdxFloatingObjectBoxList;
  APieceTable: TdxCustomPieceTable);
var
  ACount, I: Integer;
  AFloatingObject: TdxFloatingObjectBox;
begin
  if AFloatingObjects = nil then
    Exit;

  ACount := AFloatingObjects.Count;
  for I := 0 to ACount - 1 do
  begin
    AFloatingObject := AFloatingObjects[I];
    if AFloatingObject.PieceTable = APieceTable then
      ExportFloatingObjectBox(AFloatingObject);
  end;
end;

procedure TdxDocumentLayoutExporter.ExportFloatingObjectTextBox(ABox: TdxFloatingObjectBox; ATextBoxContent: TdxTextBoxFloatingObjectContent; ATextBoxDocumentLayout: TdxDocumentLayout);
begin
end;

procedure TdxDocumentLayoutExporter.ExportSpaceBox(ABox: TdxBox);
begin
end;

procedure TdxDocumentLayoutExporter.ExportTabSpaceBox(ABox: TdxTabSpaceBox);
begin
end;

procedure TdxDocumentLayoutExporter.ExportLineBreakBox(ABox: TdxLineBreakBox);
begin
end;

procedure TdxDocumentLayoutExporter.ExportParagraphMarkBox(ABox: TdxParagraphMarkBox);
begin
end;

procedure TdxDocumentLayoutExporter.ExportSectionMarkBox(ABox: TdxSectionMarkBox);
begin
end;

procedure TdxDocumentLayoutExporter.ExportUnderlineBox(ARow: TdxSimpleRow; AUnderlineBox: TdxUnderlineBox);
begin
end;

procedure TdxDocumentLayoutExporter.ExportStrikeoutBox(ARow: TdxSimpleRow; AStrikeoutBox: TdxUnderlineBox);
begin
end;

procedure TdxDocumentLayoutExporter.ExportErrorBox(AErrorBox: TdxErrorBox);
begin
end;

procedure TdxDocumentLayoutExporter.ExportBookmarkStartBox(ABox: TdxVisitableDocumentIntervalBox);
begin
end;

procedure TdxDocumentLayoutExporter.ExportBookmarkEndBox(ABox: TdxVisitableDocumentIntervalBox);
begin
end;

procedure TdxDocumentLayoutExporter.ExportCustomMarkBox(ABox: TdxCustomMarkBox);
begin
end;

procedure TdxDocumentLayoutExporter.ExportSeparatorBox(ABox: TdxSeparatorBox);
begin
end;

procedure TdxDocumentLayoutExporter.ExportPageBreakBox(ABox: TdxPageBreakBox);
begin
end;

procedure TdxDocumentLayoutExporter.ExportColumnBreakBox(ABox: TdxColumnBreakBox);
begin
end;

procedure TdxDocumentLayoutExporter.ExportNumberingListBox(ABox: TdxNumberingListBox);
begin
end;

procedure TdxDocumentLayoutExporter.ExportLineNumberBox(ABox: TdxLineNumberBox);
begin
end;

procedure TdxDocumentLayoutExporter.ExportTableBorder(ABorder: TdxTableBorderViewInfoBase; const ACellBounds: TRect);
begin
end;

procedure TdxDocumentLayoutExporter.ExportTableBorderCorner(ACorner: TdxCornerViewInfoBase; X, Y: Integer);
begin
end;

procedure TdxDocumentLayoutExporter.ExportTableCell(ACell: TdxTableCellViewInfo);
begin
end;

procedure TdxDocumentLayoutExporter.ExportTableRow(ARow: TdxTableRowViewInfoBase);
begin
end;

procedure TdxDocumentLayoutExporter.ExportParagraphFrameBox(ABox: TdxParagraphFrameBox);
begin
  if not ExportRotatedContent(ABox) then
    ExportNotRotatedContent(ABox);
end;

procedure TdxDocumentLayoutExporter.ExportHighlightArea(const AArea: TdxHighlightArea);
begin
end;

procedure TdxDocumentLayoutExporter.ExportPage(APage: TdxPage);
begin
  if APage.Header <> nil then
    ExportPageAreaCore(APage, APage.Header.PieceTable, ExportPageHeader);
  if APage.Footer <> nil then
    ExportPageAreaCore(APage, APage.Footer.PieceTable, ExportPageFooter);
  ExportPageAreaCore(APage, FDocumentModel.MainPieceTable, ExportPageMainAreas);
end;

procedure TdxDocumentLayoutExporter.ExportPage(APage: TdxPage; const AAction: TdxAction);
begin
  AAction;
end;

procedure TdxDocumentLayoutExporter.ExportParagraphFramePage(APage: TdxPage; const APageClipBounds: TdxRectF;
  AExportContent: Boolean);
var
  AOldClipBounds: TdxRectF;
  AFloatingObjects: TdxFloatingObjectBoxList;
  AParagraphFrames: TdxParagraphFrameBoxList;
begin
  AOldClipBounds := GetClipBounds;
  SetClipBounds(APageClipBounds);
  try
    ExportFloatingObjects(APage.BackgroundFloatingObjects, FPieceTable);
  finally
    SetClipBounds(AOldClipBounds);
  end;
  if not AExportContent then
    Exit;

  ExportPageMainAreas(APage);

  AFloatingObjects := APage.GetSortedNonBackgroundFloatingObjects;
  try
    ExportFloatingObjects(AFloatingObjects, FDocumentModel.MainPieceTable);
  finally
    AFloatingObjects.Free;
  end;

  AParagraphFrames := APage.GetSortedParagraphFrames;
  try
    ExportParagraphFrames(AParagraphFrames, FDocumentModel.MainPieceTable);
  finally
    AParagraphFrames.Free;
  end;
end;

procedure TdxDocumentLayoutExporter.ExportPageArea(APageArea: TdxPageArea);
begin
  APageArea.Columns.ExportTo(Self);
  APageArea.LineNumbers.ExportTo(Self);
  AfterExportPageArea;
end;

procedure TdxDocumentLayoutExporter.ExportPageAreaCore(APage: TdxPage; APieceTable: TdxCustomPieceTable;
  AAction: TdxExportPageAction);
var
  AFloatingObjects: TdxFloatingObjectBoxList;
  AParagraphFrames: TdxParagraphFrameBoxList;
begin
  ExportFloatingObjects(APage.BackgroundFloatingObjects, APieceTable);

  AAction(APage);

  AFloatingObjects := APage.GetSortedNonBackgroundFloatingObjects;
  try
    ExportFloatingObjects(AFloatingObjects, APieceTable);
  finally
    AFloatingObjects.Free;
  end;

  AParagraphFrames := APage.GetSortedParagraphFrames;
  try
    ExportParagraphFrames(AParagraphFrames, APieceTable);
  finally
    AParagraphFrames.Free;
  end;
end;

procedure TdxDocumentLayoutExporter.ApplyActivePieceTable(APageArea: TdxPageArea);
begin
  FPieceTable := TdxPieceTable(APageArea.PieceTable);
end;

procedure TdxDocumentLayoutExporter.RectorePieceTable;
begin
  FPieceTable := FDocumentModel.MainPieceTable;
end;

procedure TdxDocumentLayoutExporter.ExportPageHeaderFooterBase(APageArea: TdxHeaderFooterPageAreaBase);
var
  AClipBounds, AOldClipBounds: TdxRectF;
begin
  if APageArea = nil then
    Exit;

  AClipBounds := CalculateHeaderFooterClipBounds(APageArea.ContentBounds.ToRectF);
  if AClipBounds = TdxRectF.Null then
    Exit;

  ApplyActivePieceTable(APageArea);
  AOldClipBounds := BeginHeaderFooterExport(AClipBounds);
  try
    ExportPageHeaderFooter(APageArea);
  finally
    EndHeaderFooterExport(AOldClipBounds);
    RectorePieceTable;
  end;
end;

procedure TdxDocumentLayoutExporter.ExportPageFooter(APage: TdxPage);
begin
  ExportPageHeaderFooterBase(APage.Footer);
end;

procedure TdxDocumentLayoutExporter.ExportPageHeader(APage: TdxPage);
begin
  ExportPageHeaderFooterBase(APage.Header);
end;

procedure TdxDocumentLayoutExporter.ExportPageMainAreas(APage: TdxPage);
begin
  APage.Areas.ExportTo(Self);
end;

procedure TdxDocumentLayoutExporter.ExportParagraphBackgroundFrame(AParagraphFrameBox: TdxParagraphFrameBox);
begin
  if AParagraphFrameBox.DocumentLayout = nil then
    ExportParagraphFrame(AParagraphFrameBox);
end;

procedure TdxDocumentLayoutExporter.ExportParagraphFrame(AParagraphFrameBox: TdxParagraphFrameBox);
var
  ARow, AOldRow: TdxRow;
begin
  ARow := AParagraphFrameBox.FirstRow;
  if ARow = nil then
    Exit;

  AOldRow := FCurrentRow;
  TdxRow.AddReference(AOldRow);
  try
    SetCurrentRow(ARow);
    ApplyCurrentRowTableCellClipping(ARow);
    ExportParagraphFrameBox(AParagraphFrameBox);
    SetCurrentRow(AOldRow);
  finally
    TdxRow.Release(AOldRow);
  end;
end;

procedure TdxDocumentLayoutExporter.ExportParagraphFrames(AColumn: TdxColumn;
  const APredicate: TdxFunc<TdxParagraphFrameBox, Boolean>);
var
  AParagraphFrames: TdxParagraphFrameBoxCollection;
  I: Integer;
begin
  AParagraphFrames := AColumn.InnerParagraphFrames;
  if AParagraphFrames = nil then
    Exit;
  for I := 0 to AParagraphFrames.Count - 1 do
    if APredicate(AParagraphFrames[I]) then
      ExportParagraphBackgroundFrame(AParagraphFrames[I]);
end;

procedure TdxDocumentLayoutExporter.ExportParagraphFrames(AParagraphFrames: TdxParagraphFrameBoxList; APieceTable: TdxCustomPieceTable);
var
  ACount, I: Integer;
  AParagraphFrame: TdxParagraphFrameBox;
begin
  if AParagraphFrames = nil then
    Exit;

  ACount := AParagraphFrames.Count;
  for I := 0 to ACount - 1 do
  begin
    AParagraphFrame := AParagraphFrames[I];
    if AParagraphFrame.PieceTable = APieceTable then
      ExportParagraphFrameBox(AParagraphFrame);
  end;
end;

procedure TdxDocumentLayoutExporter.ExportSimpleRow(ARow: TdxSimpleRow);
begin
NotImplemented;
end;

procedure TdxDocumentLayoutExporter.ExportRow(ARow: TdxRow);
begin
  SetCurrentRow(ARow);
  ApplyCurrentRowTableCellClipping(ARow);
  ExportRowCore;
  SetCurrentRow(nil);
end;

procedure TdxDocumentLayoutExporter.ExportRowContentBoxes;
var
  ABoxes: TdxBoxCollection;
  ALastIndex, I: Integer;
begin
  if CurrentRow.NumberingListBox <> nil then
    CurrentRow.NumberingListBox.ExportTo(Self);
  ABoxes := CurrentRow.Boxes;
  ALastIndex := GetLastExportBoxInRowIndex(CurrentRow);
  for I := 0 to ALastIndex do
    ABoxes[I].ExportTo(Self);
end;

procedure TdxDocumentLayoutExporter.ExportRowCore;
begin
  ExportRowContentBoxes;
  ExportRowUnderlineBoxes;
  ExportRowStrikeoutBoxes;
  ExportRowBookmarkBoxes;
end;

function TdxDocumentLayoutExporter.CalculateHeaderFooterClipBounds(const AContentBounds: TdxRectF): TdxRectF;
begin
  Result := GetClipBounds;
  Result.Height := Math.Max(0, AContentBounds.Bottom - Result.Top);
end;

function TdxDocumentLayoutExporter.BeginHeaderFooterExport(const AClipBounds: TdxRectF): TdxRectF;
begin
  Result := ApplyClipBounds(AClipBounds);
end;

procedure TdxDocumentLayoutExporter.EndHeaderFooterExport(const AOldClipBounds: TdxRectF);
begin
  RestoreClipBounds(AOldClipBounds);
end;

procedure TdxDocumentLayoutExporter.ExportPageHeaderFooter(AArea: TdxHeaderFooterPageAreaBase);
begin
  AArea.ExportTo(Self);
end;

procedure TdxDocumentLayoutExporter.ExportTables(AColumn: TdxColumn);
var
  AColumnTables: TdxTableViewInfoCollection;
begin
  AColumnTables := AColumn.InnerTables;
  if AColumnTables <> nil then
  begin
    AColumnTables.ExportTo(Self);
  end;
end;

procedure TdxDocumentLayoutExporter.ExportTablesBackground(AColumn: TdxColumn);
var
  AColumnTables: TdxTableViewInfoCollection;
begin
  AColumnTables := AColumn.InnerTables;
  if AColumnTables <> nil then
    AColumnTables.ExportBackground(Self);
end;

function TdxDocumentLayoutExporter.ExportRotatedContent(ABox: TdxFloatingObjectBox): Boolean;
var
  ARun: TdxFloatingObjectAnchorRun;
  APreviousPieceTable: TdxPieceTable;
  APictureContent: TdxPictureFloatingObjectContent;
  ATextBoxContent: TdxTextBoxFloatingObjectContent;
begin
  ARun := ABox.GetFloatingObjectRun;
  APreviousPieceTable := PieceTable;
  PieceTable := TdxPieceTable(ARun.PieceTable);
  try
    ExportFloatingObjectShape(ABox, ARun.Shape);

    APictureContent := Safe<TdxPictureFloatingObjectContent>.Cast(ARun.Content);
    if APictureContent <> nil then
    begin
      ExportFloatingObjectPicture(ABox, APictureContent);
      Exit(True);
    end;
  finally
    PieceTable := APreviousPieceTable;
  end;
  ATextBoxContent := Safe<TdxTextBoxFloatingObjectContent>.Cast(ARun.Content);
  if (ABox.DocumentLayout <> nil) and (ATextBoxContent <> nil) then
    if not ATextBoxContent.TextBoxProperties.Upright then
    begin
      ExportFloatingObjectTextBox(ABox, ATextBoxContent, ABox.DocumentLayout);
      Exit(True);
    end;
  Result := False;
end;

procedure TdxDocumentLayoutExporter.ExportNotRotatedContent(ABox: TdxFloatingObjectBox);
var
  ARun: TdxFloatingObjectAnchorRun;
  ATextBoxContent: TdxTextBoxFloatingObjectContent;
begin
  ARun := ABox.GetFloatingObjectRun;
  ATextBoxContent := Safe<TdxTextBoxFloatingObjectContent>.Cast(ARun.Content);
  if (ABox.DocumentLayout <> nil) and (ATextBoxContent <> nil) then
    if ATextBoxContent.TextBoxProperties.Upright then
      ExportFloatingObjectTextBox(ABox, ATextBoxContent, ABox.DocumentLayout);
end;

function TdxDocumentLayoutExporter.ExportRotatedContent(ABox: TdxParagraphFrameBox): Boolean;
var
  APreviousPieceTable: TdxPieceTable;
begin
  APreviousPieceTable := PieceTable;
  PieceTable := TdxPieceTable(ABox.PieceTable);
  try
    ExportParagraphFrameShape(ABox, nil);
    Result := ExportNotRotatedContent(ABox);
  finally
    PieceTable := APreviousPieceTable;
  end;
end;

function TdxDocumentLayoutExporter.ExportNotRotatedContent(ABox: TdxParagraphFrameBox): Boolean;
begin
  if ABox.DocumentLayout <> nil then
  begin
    ExportParagraphFrameTextBox(ABox, ABox.DocumentLayout);
    Result := True;
  end
  else
    Result := False;
end;

function TdxDocumentLayoutExporter.ShouldExportComments(APage: TdxPage): Boolean;
begin
  Result := True;
end;

procedure TdxDocumentLayoutExporter.BeforeExportComments(APage: TdxPage);
begin
end;

procedure TdxDocumentLayoutExporter.BeforeExportComment(ACommentViewInfo: TdxCommentViewInfo);
begin
end;

function TdxDocumentLayoutExporter.ShouldExportComment(ACommentViewInfo: TdxCommentViewInfo): Boolean;
begin
  Result := True;
end;

procedure TdxDocumentLayoutExporter.AfterExportComment(ACommentViewInfo: TdxCommentViewInfo; const APageCommentBounds: TRect);
begin
end;

function TdxDocumentLayoutExporter.GetHyperlinkCalculator(APieceTable: TdxPieceTable): TdxHyperlinkCalculator;
begin
  if FHyperlinkCalculators.TryGetValue(APieceTable, Result) then
    Exit;
  Result := TdxHyperlinkCalculator.Create(APieceTable);
  FHyperlinkCalculators.Add(APieceTable, Result);
end;

function TdxDocumentLayoutExporter.GetUrl(ABox: TdxBox): string;
begin
  Result := GetHyperlinkCalculator(PieceTable).GetUrl(ABox.GetFirstPosition(PieceTable).RunIndex);
end;

function TdxDocumentLayoutExporter.GetAnchor(ABox: TdxBox): string;
begin
  Result := GetHyperlinkCalculator(PieceTable).GetAnchor(ABox.GetFirstPosition(PieceTable).RunIndex);
end;

procedure TdxDocumentLayoutExporter.AfterExportPageArea;
begin
end;

procedure TdxDocumentLayoutExporter.ExportRowBookmarkBoxes;
begin
end;

function TdxDocumentLayoutExporter.BeforeExportRotatedContent(ABox: TdxFloatingObjectBox): Boolean;
begin
  Result := True;
end;

procedure TdxDocumentLayoutExporter.AfterExportRotatedContent(ATransformApplied: Boolean);
begin
end;

procedure TdxDocumentLayoutExporter.ExportRows(AColumn: TdxColumn);
begin
  AColumn.Rows.ExportTo(Self);
end;

procedure TdxDocumentLayoutExporter.ExportRowStrikeoutBoxes;
var
  AStrikeouts: TdxUnderlineBoxCollection;
  I: Integer;
begin
  AStrikeouts := CurrentRow.InnerStrikeouts;
  if AStrikeouts = nil then
    Exit;
  for I := 0 to AStrikeouts.Count - 1 do
    ExportStrikeoutBox(CurrentRow, AStrikeouts[I]);
end;

procedure TdxDocumentLayoutExporter.ExportRowUnderlineBoxes;
var
  AUnderlines: TdxUnderlineBoxCollection;
  I: Integer;
begin
  AUnderlines := CurrentRow.InnerUnderlines;
  if AUnderlines = nil then
    Exit;
  for I := 0 to AUnderlines.Count - 1 do
    ExportUnderlineBox(CurrentRow, AUnderlines[I]);
end;

procedure TdxDocumentLayoutExporter.ExportSpecialTextBox(ABox: TdxSpecialTextBox);
begin
  ExportTextBox(ABox);
end;

procedure TdxDocumentLayoutExporter.ExportTextBox(ABox: TdxTextBox);
begin
end;

procedure TdxDocumentLayoutExporter.FinishExport;
begin
end;

function TdxDocumentLayoutExporter.GetBoxText(ABox: TdxBox): string;
begin
  Result := ABox.GetText(PieceTable);
end;

function TdxDocumentLayoutExporter.GetClipBounds: TdxRectF;
begin
  Result := TdxRectF.Null;
end;

function TdxDocumentLayoutExporter.GetDrawingBounds(const ABounds: TRect): TRect;
begin
  Result := ABounds;
end;

function TdxDocumentLayoutExporter.GetLastExportBoxInRowIndex(ARow: TdxRow): Integer;
begin
  Result := ARow.Boxes.Count - 1;
end;

function TdxDocumentLayoutExporter.GetShowWhitespace: Boolean;
begin
  Result := FShowWhitespace;
end;

function TdxDocumentLayoutExporter.IntersectClipBounds(const AOldClipBounds, ABounds: TdxRectF): TdxRectF;
begin
  if IsInfinite(AOldClipBounds.Left) and IsInfinite(AOldClipBounds.Top) and
    IsInfinite(AOldClipBounds.Width) and IsInfinite(AOldClipBounds.Height) then
    Result := ABounds
  else
  begin
    Result := AOldClipBounds;
    Result.Intersect(ABounds);
  end;
end;

function TdxDocumentLayoutExporter.IsAnchorVisible(const AAnchor: TdxTableCellVerticalAnchor): Boolean;
begin
  Result := True;
end;

function TdxDocumentLayoutExporter.IsTableRowVisible(ARow: TdxTableRowViewInfoBase): Boolean;
begin
  Result := True;
end;

function TdxDocumentLayoutExporter.IsValidBounds(const ABoxBounds: TRect): Boolean;
begin
  Result := True;
end;

procedure TdxDocumentLayoutExporter.RestoreClipBounds(const AClipBounds: TdxRectF);
begin
  RestoreClipBoundsCore(AClipBounds);
end;

procedure TdxDocumentLayoutExporter.ApplyClipBoundsCore(const ABounds: TdxRectF);
begin
  SetClipBounds(ABounds);
end;

procedure TdxDocumentLayoutExporter.RestoreClipBoundsCore(const ABounds: TdxRectF);
begin
  SetClipBounds(ABounds);
end;

procedure TdxDocumentLayoutExporter.SetClipBounds(const AClipBounds: TdxRectF);
begin
end;

procedure TdxDocumentLayoutExporter.SetCurrentCell(ACell: TdxTableCellViewInfo);
begin
  FCurrentCell := ACell;
end;

procedure TdxDocumentLayoutExporter.SetCurrentRow(ARow: TdxRow);
begin
  if FCurrentRow <> ARow then
  begin
    TdxRow.Release(FCurrentRow);
    FCurrentRow := ARow;
    TdxRow.AddReference(FCurrentRow);
  end;
end;

function TdxDocumentLayoutExporter.ShouldExportRow(ARow: TdxRow): Boolean;
begin
  Result := True;
end;

procedure TdxDocumentLayoutExporter.BeginExportTableCellContent(const AClipBounds: TdxRectF);
begin
  SetClipBounds(AClipBounds);
end;

procedure TdxDocumentLayoutExporter.EndExportTableContent(const AOldClipBounds: TdxRectF);
begin
  RestoreClipBounds(AOldClipBounds);
end;

function TdxDocumentLayoutExporter.BeginExportTableCell(ACell: TdxTableCellViewInfo): TdxRectF;
var
  AOldClipBounds: TdxRectF;
begin
  AOldClipBounds := GetClipBounds;
  ApplyClipBounds(GetDrawingBounds(ACell.TableViewInfo.GetCellBounds(ACell)).ToRectF);
  FCurrentCell := ACell;
  FCurrentCellBackColor := ACell.Cell.Properties.BackgroundColor;
  Result := AOldClipBounds;
end;

procedure TdxDocumentLayoutExporter.EndExportTableCell(const AOldClipBounds: TdxRectF);
begin
  RestoreClipBounds(AOldClipBounds);
  FCurrentCell := nil;
  FCurrentCellBackColor := TdxAlphaColors.Empty;
end;

procedure TdxDocumentLayoutExporter.SetShowWhitespace(const Value: Boolean);
begin
  FShowWhitespace := Value;
end;

function TdxDocumentLayoutExporter.GetActualBackColor(const ABounds: TRect): TdxAlphaColor;
begin
  if not TdxAlphaColors.IsTransparentOrEmpty(CurrentCellBackColor) then
    Result := CurrentCellBackColor
  else
    Result := GetBackColor(ABounds);
end;

procedure TdxDocumentLayoutExporter.PushBackgroundLayer;
begin
  FBackgroundLayers.Add(TdxBackgroundLayer.Create);
end;

procedure TdxDocumentLayoutExporter.PopBackgroundLayer;
begin
  FBackgroundLayers.Delete(FBackgroundLayers.Count - 1);
end;

function TdxDocumentLayoutExporter.GetBackColor(const ABounds: TRect): TdxAlphaColor;
var
  ALayerIndex: Integer;
  AColor: TdxNullableValue<TdxAlphaColor>;
begin
  AColor.Reset;
  ALayerIndex := FBackgroundLayers.Count - 1;
  while AColor.IsNull and (ALayerIndex >= 0) do
  begin
    AColor := FBackgroundLayers[ALayerIndex].GetBackColor(ABounds);
    Dec(ALayerIndex);
  end;
  if AColor.IsNull then
    Result := TdxAlphaColors.Empty
  else
    Result := AColor.Value;
end;

procedure TdxDocumentLayoutExporter.SetBackColor(AColor: TdxAlphaColor; const ABounds: TRect);
begin
  FBackgroundLayers[FBackgroundLayers.Count - 1].SetBackColor(AColor, ABounds);
end;

{ TdxBoundedDocumentLayoutExporter }

constructor TdxBoundedDocumentLayoutExporter.Create(ADocumentModel: TdxDocumentModel; const ABounds: TRect;
  const ALinePaintingSupport: IdxPatternLinePaintingSupport; ATextColors: TdxTextColors);
begin
  inherited Create(ADocumentModel);
  Assert(ALinePaintingSupport <> nil);
  Assert(ATextColors <> nil);
  FHorizontalLinePainter := CreateHorizontalLinePainter(ALinePaintingSupport);
  FVerticalLinePainter := CreateVerticalLinePainter(ALinePaintingSupport);
  FBounds := ABounds;
  FTextColors := ATextColors;
end;

destructor TdxBoundedDocumentLayoutExporter.Destroy;
begin
  FreeAndNil(FVerticalLinePainter);
  FreeAndNil(FHorizontalLinePainter);
  inherited Destroy;
end;

procedure TdxBoundedDocumentLayoutExporter.DrawDoubleSolidLine(const ABounds: TdxRectF; AColor: TdxAlphaColor);
begin
  HorizontalLinePainter.DrawDoubleSolidLine(ABounds, AColor);
end;

procedure TdxBoundedDocumentLayoutExporter.DrawPatternLine(const ABounds: TdxRectF; AColor: TdxAlphaColor;
  const APattern: TArray<Single>);
begin
  HorizontalLinePainter.DrawPatternLine(ABounds, AColor, APattern);
end;

procedure TdxBoundedDocumentLayoutExporter.DrawStrikeout(AStrikeout: TdxStrikeoutDouble;
  const ABounds: TdxRectF; AColor: TdxAlphaColor);
begin
  HorizontalLinePainter.DrawStrikeout(AStrikeout, ABounds, AColor);
end;

procedure TdxBoundedDocumentLayoutExporter.DrawUnderline(AUnderline: TdxUnderlineDashed;
  const ABounds: TdxRectF; AColor: TdxAlphaColor);
begin
  HorizontalLinePainter.DrawUnderline(AUnderline, ABounds, AColor);
end;

procedure TdxBoundedDocumentLayoutExporter.DrawUnderline(AUnderline: TdxUnderlineDotted;
  const ABounds: TdxRectF; AColor: TdxAlphaColor);
begin
  HorizontalLinePainter.DrawUnderline(AUnderline, ABounds, AColor);
end;

procedure TdxBoundedDocumentLayoutExporter.DrawUnderline(AUnderline: TdxUnderlineSingle;
  const ABounds: TdxRectF; AColor: TdxAlphaColor);
begin
  HorizontalLinePainter.DrawUnderline(AUnderline, ABounds, AColor);
end;

procedure TdxBoundedDocumentLayoutExporter.DrawUnderline(AUnderline: TdxUnderlineDouble;
  const ABounds: TdxRectF; AColor: TdxAlphaColor);
begin
  HorizontalLinePainter.DrawUnderline(AUnderline, ABounds, AColor);
end;

procedure TdxBoundedDocumentLayoutExporter.DrawUnderline(AUnderline: TdxUnderlineThickDashDotted;
  const ABounds: TdxRectF; AColor: TdxAlphaColor);
begin
  HorizontalLinePainter.DrawUnderline(AUnderline, ABounds, AColor);
end;

procedure TdxBoundedDocumentLayoutExporter.DrawUnderline(AUnderline: TdxUnderlineThickDashed;
  const ABounds: TdxRectF; AColor: TdxAlphaColor);
begin
  HorizontalLinePainter.DrawUnderline(AUnderline, ABounds, AColor);
end;

procedure TdxBoundedDocumentLayoutExporter.DrawUnderline(AUnderline: TdxUnderlineThickDotted;
  const ABounds: TdxRectF; AColor: TdxAlphaColor);
begin
  HorizontalLinePainter.DrawUnderline(AUnderline, ABounds, AColor);
end;

procedure TdxBoundedDocumentLayoutExporter.DrawUnderline(AUnderline: TdxUnderlineThickSingle;
  const ABounds: TdxRectF; AColor: TdxAlphaColor);
begin
  HorizontalLinePainter.DrawUnderline(AUnderline, ABounds, AColor);
end;

procedure TdxBoundedDocumentLayoutExporter.DrawUnderline(AUnderline: TdxUnderlineLongDashed;
  const ABounds: TdxRectF; AColor: TdxAlphaColor);
begin
  HorizontalLinePainter.DrawUnderline(AUnderline, ABounds, AColor);
end;

procedure TdxBoundedDocumentLayoutExporter.DrawUnderline(AUnderline: TdxUnderlineHeavyWave;
  const ABounds: TdxRectF; AColor: TdxAlphaColor);
begin
  HorizontalLinePainter.DrawUnderline(AUnderline, ABounds, AColor);
end;

procedure TdxBoundedDocumentLayoutExporter.DrawUnderline(AUnderline: TdxUnderlineDashDotDotted;
  const ABounds: TdxRectF; AColor: TdxAlphaColor);
begin
  HorizontalLinePainter.DrawUnderline(AUnderline, ABounds, AColor);
end;

procedure TdxBoundedDocumentLayoutExporter.DrawUnderline(AUnderline: TdxUnderlineDashDotted;
  const ABounds: TdxRectF; AColor: TdxAlphaColor);
begin
  HorizontalLinePainter.DrawUnderline(AUnderline, ABounds, AColor);
end;

procedure TdxBoundedDocumentLayoutExporter.DrawUnderline(AUnderline: TdxUnderlineDashSmallGap;
  const ABounds: TdxRectF; AColor: TdxAlphaColor);
begin
  HorizontalLinePainter.DrawUnderline(AUnderline, ABounds, AColor);
end;

procedure TdxBoundedDocumentLayoutExporter.DrawStrikeout(AStrikeout: TdxStrikeoutSingle;
  const ABounds: TdxRectF; AColor: TdxAlphaColor);
begin
  HorizontalLinePainter.DrawStrikeout(AStrikeout, ABounds, AColor);
end;

procedure TdxBoundedDocumentLayoutExporter.ExportBackground;
begin
  ExportTextHighlighting;
end;

procedure TdxBoundedDocumentLayoutExporter.ExportHighlighting(AHighlightAreas: TdxHighlightAreaCollection);
var
  I: Integer;
begin
  if AHighlightAreas = nil then
    Exit;
  for I := 0 to AHighlightAreas.Count - 1 do
    ExportHighlightArea(AHighlightAreas[I]);
end;

procedure TdxBoundedDocumentLayoutExporter.ExportRowBackground(ARow: TdxRow);
begin
  if not ShouldExportRowBackground(ARow) then
    Exit;
  if not ShouldExportRow(ARow) then
    Exit;
  SetCurrentRow(ARow);
  ApplyCurrentRowTableCellClipping(ARow);
  ExportBackground;
  SetCurrentRow(nil);
end;

procedure TdxBoundedDocumentLayoutExporter.ExportRows(AColumn: TdxColumn);
var
  I: Integer;
begin
  for I := 0 to AColumn.Rows.Count - 1 do
    ExportRowBackground(AColumn.Rows[I]);
  inherited ExportRows(AColumn);
end;

procedure TdxBoundedDocumentLayoutExporter.ExportTextHighlighting;
begin
  ExportHighlighting(CurrentRow.HighlightAreas);
  ExportHighlighting(CurrentRow.InnerCommentHighlightAreas);
end;

function TdxBoundedDocumentLayoutExporter.GetDrawingBounds(const ABounds: TRect): TRect;
begin
  Result := ABounds;
  Result.Offset(Offset);
end;

function TdxBoundedDocumentLayoutExporter.GetOffset: TPoint;
begin
  Result := FBounds.TopLeft;
end;

function TdxBoundedDocumentLayoutExporter.GetShapeOutlinePenWidth(ARun: TdxFloatingObjectAnchorRun;
  ABox: TdxFloatingObjectBox): Integer;
begin
  if not cxRectIsEqual(ABox.Bounds, ABox.ContentBounds) then
    Result := Math.Max(1, DocumentModel.ToDocumentLayoutUnitConverter.ToLayoutUnits(ARun.Shape.OutlineWidth))
  else
    if ARun.Shape.UseOutlineColor and not TdxAlphaColors.IsTransparentOrEmpty(ARun.Shape.OutlineColor) and
       ARun.Shape.UseOutlineWidth and (ARun.Shape.OutlineWidth = 0) then
      Result := 0
    else
      Result := -1;
end;

function TdxBoundedDocumentLayoutExporter.GetTabLeaderCharacter(ALeaderType: TdxTabLeaderType): Char;
begin
  Result := TdxTabsController.GetTabLeaderCharacter(ALeaderType);
end;

function TdxBoundedDocumentLayoutExporter.GetTabLeaderCharacterWidth(ABox: TdxTabSpaceBox): Integer;
begin
  Result := TdxTabsController.GetTabLeaderCharacterWidth(ABox, PieceTable);
end;

function TdxBoundedDocumentLayoutExporter.GetTabLeaderText(ABox: TdxTabSpaceBox; const ATextBounds: TRect): string;
var
  ACharacter: Char;
  ACount: Integer;
begin
  Result := '';
  if ABox.TabInfo.Leader <> TdxTabLeaderType.None then
  begin
    ACharacter := GetTabLeaderCharacter(ABox.TabInfo.Leader);
    ACount := ABox.LeaderCount;
    Result := StringOfChar(ACharacter, ACount);
  end;
end;

function TdxBoundedDocumentLayoutExporter.GetTabLeaderUnderlineType(ABox: TdxTabSpaceBox): TdxUnderlineType;
begin
  case ABox.TabInfo.Leader of
    TdxTabLeaderType.MiddleDots:
      Result := TdxUnderlineType.Dotted;
    TdxTabLeaderType.Hyphens:
      Result := TdxUnderlineType.Dashed;
    TdxTabLeaderType.Underline, TdxTabLeaderType.ThickLine:
      Result := TdxUnderlineType.Single;
    TdxTabLeaderType.EqualSign:
      Result := TdxUnderlineType.Double;
    else
      Result := TdxUnderlineType.Dotted;
  end;
end;

function TdxBoundedDocumentLayoutExporter.GetTableBorderLine(ABorderLineStyle: TdxBorderLineStyle): TdxUnderline;
var
  ARepository: TdxBorderLineRepository;
begin
  if ABorderLineStyle = TdxBorderLineStyle.Single then
    Exit(nil);
  ARepository := DocumentModel.BorderLineRepository;
  Result := ARepository.GetCharacterLineByType(ABorderLineStyle);
end;

function TdxBoundedDocumentLayoutExporter.GetCharacterLineBoxByIndex(ARow: TdxSimpleRow; AIndex: Integer): TdxBox;
begin
  if (ARow is TdxRow) and (TdxRow(ARow).NumberingListBox <> nil) then
  begin
    if AIndex = 0 then
      Result := TdxRow(ARow).NumberingListBox
    else
      Result := ARow.Boxes[AIndex - 1];
  end
  else
    Result := ARow.Boxes[AIndex];
end;

function TdxBoundedDocumentLayoutExporter.IsValidBounds(const ABoxBounds: TRect): Boolean;
begin
  Result := not cxRectIsEmpty(ABoxBounds) and (Painter.HasTransform or cxRectIntersect(FBounds, ABoxBounds));
end;

function TdxBoundedDocumentLayoutExporter.ShouldCenterTabLeaderLineVertically(ABox: TdxTabSpaceBox): Boolean;
begin
  case ABox.TabInfo.Leader of
    TdxTabLeaderType.MiddleDots:
      Result := True;
    TdxTabLeaderType.Hyphens:
      Result := True;
    TdxTabLeaderType.EqualSign:
      Result := True;
    TdxTabLeaderType.Underline, TdxTabLeaderType.ThickLine:
      Result := False;
    else
      Result := False;
  end;
end;

function TdxBoundedDocumentLayoutExporter.ShouldExportRowBackground(ARow: TdxRow): Boolean;
begin
  Result := (ARow.InnerHighlightAreas <> nil) and (ARow.InnerHighlightAreas.Count > 0) or
    (ARow.InnerCommentHighlightAreas <> nil) and (ARow.InnerCommentHighlightAreas.Count > 0);
end;

procedure TdxBoundedDocumentLayoutExporter.DrawUnderline(AUnderline: TdxUnderlineThickDashDotDotted;
  const ABounds: TdxRectF; AColor: TdxAlphaColor);
begin
  HorizontalLinePainter.DrawUnderline(AUnderline, ABounds, AColor);
end;

procedure TdxBoundedDocumentLayoutExporter.DrawUnderline(AUnderline: TdxUnderlineThickLongDashed;
  const ABounds: TdxRectF; AColor: TdxAlphaColor);
begin
  HorizontalLinePainter.DrawUnderline(AUnderline, ABounds, AColor);
end;

procedure TdxBoundedDocumentLayoutExporter.DrawUnderline(AUnderline: TdxUnderlineDoubleWave;
  const ABounds: TdxRectF; AColor: TdxAlphaColor);
begin
  HorizontalLinePainter.DrawUnderline(AUnderline, ABounds, AColor);
end;

procedure TdxBoundedDocumentLayoutExporter.DrawUnderline(AUnderline: TdxUnderlineWave;
  const ABounds: TdxRectF; AColor: TdxAlphaColor);
begin
  HorizontalLinePainter.DrawUnderline(AUnderline, ABounds, AColor);
end;

{ TdxMarkBoxExporterBase }

constructor TdxMarkBoxExporterBase.Create(AExporter: TdxGraphicsDocumentLayoutExporter);
begin
  inherited Create;
  Assert(AExporter <> nil);
  FExporter := AExporter;
end;

function TdxMarkBoxExporterBase.GetDocumentModel: TdxDocumentModel;
begin
  Result := Exporter.DocumentModel;
end;


function TdxMarkBoxExporterBase.GetPieceTable: TdxPieceTable;
begin
  Result := Exporter.PieceTable;
end;

procedure TdxMarkBoxExporterBase.Export(ABox: TdxBox; const ABoxBounds: TRect);
var
  AForeColor: TdxAlphaColor;
  AFontIndex: Integer;
  AFontInfo: TdxFontInfo;
  ATextSize: TSize;
  ATextBounds, ALineBounds, ALeftLineBounds, ARightLineBounds: TRect;
begin
  AFontIndex := DocumentModel.FontCache.CalcFontIndex('Arial', 16, [], TdxCharacterFormattingScript.Normal);
  AFontInfo := DocumentModel.FontCache[AFontIndex];

  ATextSize := DocumentModel.FontCache.Measurer.MeasureString(Text, AFontInfo);
  ATextBounds := ABoxBounds;
  ATextBounds.X := Trunc(ATextBounds.X + (ATextBounds.Width - ATextSize.Width) / 2);
  ATextBounds.Width := ATextSize.Width;
  ATextBounds.Y := Trunc(ATextBounds.Y + (ATextBounds.Height - ATextSize.Height) / 2);
  AForeColor := Exporter.GetActualColor(ABox.GetActualForeColor(PieceTable, Exporter.TextColors, FExporter.GetBackColor(ATextBounds)));
  if ATextBounds.Width + FPaddingsWidth < ABoxBounds.Width then
  begin
    Exporter.Painter.DrawString(Text, AFontInfo, AForeColor, ATextBounds);

    ALineBounds := ABoxBounds;
    ALineBounds.Y := Trunc(ALineBounds.Y + ALineBounds.Height / 2);
    ALineBounds.Height := 1;

    ALeftLineBounds := ALineBounds;
    ALeftLineBounds.Width := ATextBounds.Left - ALineBounds.Left - FSpacingWidth;
    DrawLine(ALeftLineBounds, AForeColor);

    ARightLineBounds := ALineBounds;
    ARightLineBounds.X := ATextBounds.Right + FSpacingWidth;
    ARightLineBounds.Width := ABoxBounds.Right - ATextBounds.Right - FSpacingWidth;
    DrawLine(ARightLineBounds, AForeColor);
  end
  else
  begin
    ALineBounds := ABoxBounds;
    ALineBounds.Y := Trunc(ALineBounds.Y + ALineBounds.Height / 2);
    ALineBounds.Height := 1;
    DrawLine(ALineBounds, AForeColor);
  end;
end;

{ TdxGraphicsDocumentLayoutExporter }

constructor TdxGraphicsDocumentLayoutExporter.Create(ADocumentModel: TdxDocumentModel; APainter: TdxPainter;
  AAdapter: TdxGraphicsDocumentLayoutExporterAdapter; const ABounds: TRect; AHidePartiallyVisibleRow: Boolean; ATextColors: TdxTextColors);
begin
  inherited Create(ADocumentModel, ABounds, APainter, ATextColors);
  FDrawInactivePieceTableWithDifferentColor := True;
  Assert(APainter <> nil);
  Assert(AAdapter <> nil);
  FPainter := APainter;
  FAdapter := AAdapter;
  FHidePartiallyVisibleRow := AHidePartiallyVisibleRow;
  ShowWhitespace := False;
end;

constructor TdxGraphicsDocumentLayoutExporter.Create(ADocumentModel: TdxDocumentModel; APainter: TdxPainter;
  AAdapter: TdxGraphicsDocumentLayoutExporterAdapter; const ABounds: TRect; ATextColors: TdxTextColors);
begin
  Create(ADocumentModel, APainter, AAdapter, ABounds, False, ATextColors);
end;

destructor TdxGraphicsDocumentLayoutExporter.Destroy;
begin
  FreeAndNil(FWhitespaceStrategy);
  FreeAndNil(FAdapter);
  inherited Destroy;
end;

function TdxGraphicsDocumentLayoutExporter.GetPainter: TdxPainter;
begin
  Result := FPainter;
end;

function TdxGraphicsDocumentLayoutExporter.GetShowWhitespace: Boolean;
begin
  Result := inherited ShowWhitespace;
end;

procedure TdxGraphicsDocumentLayoutExporter.SetShowWhitespace(const AValue: Boolean);
begin
  inherited SetShowWhitespace(AValue);
  FreeAndNil(FWhitespaceStrategy);
  if AValue then
    FWhitespaceStrategy := TdxGraphicsDocumentLayoutExporterShowWhitespaceStrategy.Create(Self)
  else
    FWhitespaceStrategy := TdxGraphicsDocumentLayoutExporterHideWhitespaceStrategy.Create(Self);
end;

function TdxGraphicsDocumentLayoutExporter.CreateHorizontalLinePainter(const ALinePaintingSupport: IdxPatternLinePaintingSupport): TdxRichEditPatternLinePainter;
begin
  Result := TdxRichEditHorizontalPatternLinePainter.Create(ALinePaintingSupport, DocumentModel.LayoutUnitConverter);
end;

function TdxGraphicsDocumentLayoutExporter.CreateVerticalLinePainter(const ALinePaintingSupport: IdxPatternLinePaintingSupport): TdxRichEditPatternLinePainter;
begin
  Result := TdxRichEditVerticalPatternLinePainter.Create(ALinePaintingSupport, DocumentModel.LayoutUnitConverter);
end;

function TdxGraphicsDocumentLayoutExporter.GetActualColor(AColor: TdxAlphaColor): TdxAlphaColor;
begin
  if not ShouldGrayContent then
    Result := AColor
  else
    Result := TdxAlphaColors.Blend(TdxAlphaColors.FromArgb(128, AColor), TdxAlphaColors.White);
end;

function TdxGraphicsDocumentLayoutExporter.GetEffectivePieceTable(APieceTable: TdxPieceTable): TdxPieceTable;
var
  AContentType: TdxTextBoxContentType;
begin
  if APieceTable.IsTextBox then
  begin
    AContentType := TdxTextBoxContentType(APieceTable.ContentType);
    if AContentType.AnchorRun = nil then
      Exit(APieceTable);
    Exit(TdxPieceTable(AContentType.AnchorRun.PieceTable));
  end
  ;
  Result := APieceTable;
end;

function TdxGraphicsDocumentLayoutExporter.ShouldGrayContent: Boolean;
var
  APieceTable, AActivePieceTable: TdxPieceTable;
begin
  APieceTable := GetEffectivePieceTable(PieceTable);
  AActivePieceTable := GetEffectivePieceTable(DocumentModel.ActivePieceTable);
  Result := (APieceTable <> AActivePieceTable) and DrawInactivePieceTableWithDifferentColor;
end;

procedure TdxGraphicsDocumentLayoutExporter.ExportRow(ARow: TdxRow);
begin
  if ShouldExportRow(ARow) then
    inherited ExportRow(ARow);
end;

function TdxGraphicsDocumentLayoutExporter.ShouldExportRowBackground(ARow: TdxRow): Boolean;
begin
  if (ARow.InnerHighlightAreas <> nil) and (ARow.InnerHighlightAreas.Count > 0) then
    Exit(True);
  if (ARow.InnerFieldHighlightAreas <> nil) and (ARow.InnerFieldHighlightAreas.Count > 0) then
    Exit(True);
  if (ARow.InnerRangePermissionHighlightAreas <> nil) and (ARow.InnerRangePermissionHighlightAreas.Count > 0) then
    Exit(True);
  if (ARow.InnerCommentHighlightAreas <> nil) and (ARow.InnerCommentHighlightAreas.Count > 0) then
    Exit(True);
  Result := False;
end;

function TdxGraphicsDocumentLayoutExporter.ShouldExportRow(ARow: TdxRow): Boolean;
var
  ARowBounds, ADrawingBounds: TRect;
begin
  if FHidePartiallyVisibleRow then
  begin
    ARowBounds := CalcRowContentBounds(ARow);
    if GetDrawingBounds(ARowBounds).Bottom > Bounds.Bottom then
      Exit(False);
  end;
  if not VisibleBounds.IsEmpty then
  begin
    ARowBounds := CalcRowContentBounds(ARow);
    ADrawingBounds := GetDrawingBounds(ARowBounds);
    if (ADrawingBounds.Bottom < VisibleBounds.Top) or (ADrawingBounds.Top > VisibleBounds.Bottom) then
      Exit(False);
  end;
  Result := True;
end;

function TdxGraphicsDocumentLayoutExporter.IsTableRowVisible(ARow: TdxTableRowViewInfoBase): Boolean;
var
  ATopAnchor, ABottomAnchor: TdxTableCellVerticalAnchor;
  AMaxTop, AMinBottom: Integer;
begin
  if VisibleBounds.IsEmpty then
    Exit(True);

  if IsRowContainsVerticalMergingCell(ARow.Row) then
    Exit(True);

  ATopAnchor := ARow.TopAnchor;
  ABottomAnchor := ARow.BottomAnchor;
  AMaxTop := Math.Max(ATopAnchor.VerticalPosition, VisibleBounds.Top);
  AMinBottom := Math.Min(ABottomAnchor.VerticalPosition + ABottomAnchor.BottomTextIndent, VisibleBounds.Bottom);

  Result := AMaxTop <= AMinBottom;
end;

function TdxGraphicsDocumentLayoutExporter.IsRowContainsVerticalMergingCell(ARow: TdxTableRow): Boolean;
var
  AModelCells: TdxTableCellCollection;
  AModelCellCount, I: Integer;
begin
  AModelCells := ARow.Cells;
  AModelCellCount := AModelCells.Count;
  for I := 0 to AModelCellCount - 1 do
  begin
    if AModelCells[I].VerticalMerging = TdxMergingState.Restart then
      Exit(True);
  end;
  Result := False;
end;

function TdxGraphicsDocumentLayoutExporter.IsAnchorVisible(const AAnchor: TdxTableCellVerticalAnchor): Boolean;
begin
  if VisibleBounds.IsEmpty then
    Exit(True);

  if AAnchor.VerticalPosition + AAnchor.BottomTextIndent < VisibleBounds.Top then
    Exit(False);
  if AAnchor.VerticalPosition > VisibleBounds.Bottom then
    Exit(False);

  Result := True;
end;

procedure TdxGraphicsDocumentLayoutExporter.ExportBackground;
begin
  ExportFieldsHighlighting;
  inherited ExportBackground;
end;

procedure TdxGraphicsDocumentLayoutExporter.ExportFieldsHighlighting;
begin
  ExportHighlighting(CurrentRow.InnerFieldHighlightAreas);
  ExportHighlighting(CurrentRow.InnerRangePermissionHighlightAreas);
  ExportHighlighting(CurrentRow.InnerCommentHighlightAreas);
end;

procedure TdxGraphicsDocumentLayoutExporter.ExportHighlightArea(const AArea: TdxHighlightArea);
begin
  Painter.FillRectangle(GetActualColor(AArea.Color), GetDrawingBounds(AArea.Bounds));
end;

procedure TdxGraphicsDocumentLayoutExporter.ExportTextBox(ABox: TdxTextBox);
begin
  ExportTextBoxCore(ABox);
end;

procedure TdxGraphicsDocumentLayoutExporter.ExportLayoutDependentTextBox(ABox: TdxLayoutDependentTextBox);
begin
  ExportTextBoxCore(ABox);
end;

procedure TdxGraphicsDocumentLayoutExporter.ExportNumberingListBox(ABox: TdxNumberingListBox);
begin
  ExportNumberingListBoxCore(ABox);
end;

procedure TdxGraphicsDocumentLayoutExporter.ExportLineNumberBox(ABox: TdxLineNumberBox);
var
  ARun: TdxTextRunBase;
  AArea: TdxHighlightArea;
begin
  ARun := TdxTextRunBase(ABox.GetRun(PieceTable.DocumentModel.MainPieceTable));
  if not TdxAlphaColors.IsTransparentOrEmpty(ARun.BackColor) then
  begin
    AArea := TdxHighlightArea.Create(ARun.BackColor, ABox.Bounds);
    ExportHighlightArea(AArea);
  end;
  ExportTextBoxCoreNoCheckBoundsValidity(ABox);
end;

procedure TdxGraphicsDocumentLayoutExporter.ExportSpaceBox(ABox: TdxBox);
begin
  FWhitespaceStrategy.ExportSpaceBox(ABox);
end;

procedure TdxGraphicsDocumentLayoutExporter.ExportSeparatorBox(ABox: TdxSeparatorBox);
begin
  FWhitespaceStrategy.ExportSeparatorBox(ABox);
end;

procedure TdxGraphicsDocumentLayoutExporter.ExportInlinePictureBox(ABox: TdxInlinePictureBox);
begin
  FAdapter.ExportInlinePictureBox(Self, ABox);
end;

function TdxGraphicsDocumentLayoutExporter.BeforeExportRotatedContent(ABox: TdxFloatingObjectBox): Boolean;
var
  ABounds: TRect;
  ACenter: TPoint;
  ATransformApplied: Boolean;
begin
  ABounds := GetDrawingBounds(ABox.Bounds);
  ACenter := TdxRectangleUtils.CenterPoint(ABounds);
  ATransformApplied := Painter.TryPushRotationTransform(ACenter, DocumentModel.GetBoxEffectiveRotationAngleInDegrees(ABox));
  Painter.PushSmoothingMode(ATransformApplied);
  Result := ATransformApplied;
end;

procedure TdxGraphicsDocumentLayoutExporter.AfterExportRotatedContent(ATransformApplied: Boolean);
begin
  Painter.PopSmoothingMode;
  if ATransformApplied then
    Painter.PopTransform;
end;

function TdxGraphicsDocumentLayoutExporter.ExportRotatedContent(ABox: TdxFloatingObjectBox): Boolean;
var
  ATransformApplied: Boolean;
begin
  ATransformApplied := BeforeExportRotatedContent(ABox);

  try
    Exit(inherited ExportRotatedContent(ABox));
  finally
    AfterExportRotatedContent(ATransformApplied);
  end;
end;

procedure TdxGraphicsDocumentLayoutExporter.ExportFloatingObjectShape(ABox: TdxFloatingObjectBox; AShape: TdxShape);
var
  AContentBounds, AShapeBounds: TRect;
  ARun: TdxFloatingObjectAnchorRun;
  AFillColor, AOutlineColor: TdxAlphaColor;
  APenWidth: Integer;
  APen: TdxGPColorPen;
begin
  AContentBounds := GetDrawingBounds(ABox.ContentBounds);
  if not IsValidBounds(AContentBounds) then
    Exit;

  ARun := ABox.GetFloatingObjectRun;
  AFillColor := ARun.Shape.FillColor;
  if not TdxAlphaColors.IsTransparentOrEmpty(AFillColor) then
    Painter.FillRectangle(GetActualColor(AFillColor), AContentBounds);
  APenWidth := GetShapeOutlinePenWidth(ARun, ABox);
  if APenWidth >= 0 then
  begin
    AOutlineColor := GetActualColor(ARun.Shape.OutlineColor);

    APen := TdxGPColorPen.Create(AOutlineColor, APenWidth);
    try
      AShapeBounds := GetDrawingBounds(ABox.Bounds);
      APen.Alignment := TdxGpPenAlignment.PenAlignmentInset;
      Painter.DrawRectangle(APen, AShapeBounds);
    finally
      APen.Free;
    end;
  end;
end;

procedure TdxGraphicsDocumentLayoutExporter.ExportParagraphFrameShape(ABox: TdxParagraphFrameBox; AShape: TdxShape);
var
  AContentBounds, AActualBounds: TRect;
  AFirstParagraph: TdxSimpleParagraph;
  ABoxParagraphProperties: TdxParagraphProperties;
  ABackColor: TdxAlphaColor;
  ARows: TdxRowCollection;
  AIsContainsTable: Boolean;
begin
  AContentBounds := ABox.ContentBounds;
  AActualBounds := ABox.ActualSizeBounds;
  AFirstParagraph := ABox.GetParagraph;
  ABoxParagraphProperties := AFirstParagraph.ParagraphProperties;

  if not ABox.HasFrameProperties or (ABox.DocumentLayout = nil) then
  begin
    ABackColor := AFirstParagraph.BackColor;
    SetBackColor(ABackColor, AContentBounds);
    FillRectangle(ABackColor, AContentBounds);
    DrawParagraphBordersWithCorners(AContentBounds, ABoxParagraphProperties);
    Exit;
  end;

  ARows := ABox.DocumentLayout.Pages.Last.Areas.Last.Columns.Last.Rows;
  AIsContainsTable := ABox.DocumentLayout.Pages.Last.Areas.Last.Columns.Last.Tables.Count > 0;

  DrawParagraphBackground(AActualBounds, ARows);

  if AIsContainsTable then
    DrawParagraphBordersWithoutTableBounds(AActualBounds, ABoxParagraphProperties, ARows)
  else
    DrawParagraphBordersWithCorners(AActualBounds, ABoxParagraphProperties);
end;

procedure TdxGraphicsDocumentLayoutExporter.FillRectangle(AFillColor: TdxAlphaColor; const AActualContentBounds: TRect);
begin
  if not TdxAlphaColors.IsTransparentOrEmpty(AFillColor) then
    Painter.FillRectangle(GetActualColor(AFillColor), AActualContentBounds);
end;

procedure TdxGraphicsDocumentLayoutExporter.DrawParagraphBordersWithCorners(const AActualContentBounds: TRect;
  ABoxParagraphProperties: TdxParagraphProperties);
begin
  DrawParagraphBorders(AActualContentBounds, ABoxParagraphProperties);
  DrawParagraphBordersCorners(AActualContentBounds, ABoxParagraphProperties);
end;

class function TdxGraphicsDocumentLayoutExporter.GetActualBoxHeight(const AActualBounds: TRect; ARows: TdxRowCollection;
  ACurrentRow: TdxRow; const ARowBounds: TRect): Integer;
begin
  if (ACurrentRow = ARows.First) and (ACurrentRow = ARows.Last) then
    Exit(AActualBounds.Height);

  if ACurrentRow = ARows.First then
    Exit(ARowBounds.Height + ARowBounds.Y - AActualBounds.Y);

  if ACurrentRow = ARows.Last then
    Exit(AActualBounds.Bottom - ARowBounds.Y);

  Result := ARowBounds.Height;
end;

procedure TdxGraphicsDocumentLayoutExporter.DrawParagraphBackground(const AActualBounds: TRect; ARows: TdxRowCollection);
var
  AIndex, Y, AHeight: Integer;
  ACurrentRow: TdxRow;
  ARowBounds, AActualParagraphBounds: TRect;
  AParagraph: TdxSimpleParagraph;
begin
  for AIndex := 0 to ARows.Count - 1 do
  begin
    ACurrentRow := ARows[AIndex];
    AParagraph := ACurrentRow.Paragraph;

    if not AParagraph.IsInCell then
    begin
      ARowBounds := ACurrentRow.Bounds;

      if ACurrentRow <> ARows.First then
        Y := ARowBounds.Y
      else
        Y := AActualBounds.Y;

      AHeight := GetActualBoxHeight(AActualBounds, ARows, ACurrentRow, ARowBounds);

      AActualParagraphBounds.InitSize(AActualBounds.X, Y, AActualBounds.Width, AHeight);
      FillRectangle(AParagraph.BackColor, AActualParagraphBounds);
    end;
  end;
end;

procedure TdxGraphicsDocumentLayoutExporter.DrawParagraphBordersWithoutTableBounds(const AActualBounds: TRect;
  ABoxParagraphProperties: TdxParagraphProperties; ARows: TdxRowCollection);
var
  AIndex, ATop, AHeight: Integer;
  ACurrentRow: TdxRow;
  ARowBounds, AActualParagraphBounds: TRect;
begin
  AIndex := 0;
  while AIndex < ARows.Count do
  begin
    ACurrentRow := ARows[AIndex];

    if not ACurrentRow.Paragraph.IsInCell then
    begin
      ARowBounds := ACurrentRow.Bounds;

      if ACurrentRow <> ARows.First then
        ATop := ARowBounds.Top
      else
        ATop := AActualBounds.Top;

      AHeight := 0;
      repeat
        Inc(AHeight, GetActualBoxHeight(AActualBounds, ARows, ARows[AIndex], ARows[AIndex].Bounds));
        if AIndex < ARows.Count - 1 then
          Inc(AIndex);
      until not ((AIndex < ARows.Count - 1) and not ARows[AIndex].Paragraph.IsInCell);

      AActualParagraphBounds.InitSize(AActualBounds.X, ATop, AActualBounds.Width, AHeight);
      DrawParagraphBordersWithCorners(AActualParagraphBounds, ABoxParagraphProperties);
    end;
    Inc(AIndex);
  end;
end;

procedure TdxGraphicsDocumentLayoutExporter.DrawParagraphBorders(const AContentBounds: TRect; AParagraphProperties: TdxParagraphProperties);
var
  AConverter: TdxDocumentModelUnitToLayoutUnitConverter;
  ALeftBorder, ARightBorder, ATopBorder, ABottomBorder: TdxBorderInfo;
  ABorderViewInfo: TdxTableCellVerticalBorderViewInfo;
  ARightBorderBounds, ALeftBorderBounds, ATopBorderBounds, ABottomBorderBounds: TRect;
  ALeftCorner, ARightCorner: TdxSingleLineCornerViewInfo;
  AHorizontalBorderViewInfo: TdxParagraphHorizontalBorderViewInfo;
begin
  AConverter := DocumentModel.ToDocumentLayoutUnitConverter;

  ALeftBorder := AParagraphProperties.LeftBorder;
  ARightBorder := AParagraphProperties.RightBorder;
  ATopBorder := AParagraphProperties.TopBorder;
  ABottomBorder := AParagraphProperties.BottomBorder;

  ABorderViewInfo := TdxTableCellVerticalBorderViewInfo.Create(nil, ARightBorder, 0, 0, AConverter);
  try
    ARightBorderBounds.InitSize(AContentBounds.Right, AContentBounds.Y, 0, AContentBounds.Height);
    ExportTableBorder(ABorderViewInfo, ARightBorderBounds);
  finally
    ABorderViewInfo.Free;
  end;

  ABorderViewInfo := TdxTableCellVerticalBorderViewInfo.Create(nil, ALeftBorder, 0, 0, AConverter);
  try
    ALeftBorderBounds.InitSize(AContentBounds.Left, AContentBounds.Y, 0, AContentBounds.Height);
    ExportTableBorder(ABorderViewInfo, ALeftBorderBounds);
  finally
    ABorderViewInfo.Free;
  end;

  ALeftCorner := TdxSingleLineCornerViewInfo.Create(ALeftBorder, ARightBorder, ATopBorder, ABottomBorder, 0.0, 0.0, TdxCornerViewInfoType.OuterVerticalStart);
  try
    ARightCorner := TdxSingleLineCornerViewInfo.Create(ALeftBorder, ARightBorder, ATopBorder, ABottomBorder, 0.0, 0.0, TdxCornerViewInfoType.OuterVerticalEnd);
    try
      AHorizontalBorderViewInfo := TdxParagraphHorizontalBorderViewInfo.Create(ATopBorder, AConverter, ALeftCorner, ARightCorner);
      try
        ATopBorderBounds.InitSize(AContentBounds.X, AContentBounds.Top, AContentBounds.Width, 0);
        ExportTableBorder(AHorizontalBorderViewInfo, ATopBorderBounds);
      finally
        AHorizontalBorderViewInfo.Free;
      end;

      AHorizontalBorderViewInfo := TdxParagraphHorizontalBorderViewInfo.Create(ABottomBorder, AConverter, ALeftCorner, ARightCorner);
      try
        ABottomBorderBounds.InitSize(AContentBounds.X, AContentBounds.Bottom, AContentBounds.Width, 0);
        ExportTableBorder(AHorizontalBorderViewInfo, ABottomBorderBounds);
      finally
        AHorizontalBorderViewInfo.Free;
      end;
    finally
      ARightCorner.Free;
    end;
  finally
    ALeftCorner.Free;
  end;
end;

procedure TdxGraphicsDocumentLayoutExporter.DrawParagraphBordersCorners(const AContentBounds: TRect; AParagraphProperties: TdxParagraphProperties);
var
  AConverter: TdxDocumentModelUnitToLayoutUnitConverter;
  ALeftBorder, ARightBorder, ATopBorder, ABottomBorder: TdxBorderInfo;
  ATopLeftCorner, ATopRightCorner, ABottomLeftCorner, ABottomRightCorner: TdxCornerViewInfoBase;
begin
  AConverter := DocumentModel.ToDocumentLayoutUnitConverter;

  ALeftBorder := AParagraphProperties.LeftBorder;
  ARightBorder := AParagraphProperties.RightBorder;
  ATopBorder := AParagraphProperties.TopBorder;
  ABottomBorder := AParagraphProperties.BottomBorder;

  ATopLeftCorner := TdxCornerViewInfoBase.CreateCorner(TdxCornerViewInfoType.OuterHorizontalEnd, AConverter, nil, nil, ATopBorder, ALeftBorder, 0);
  try
    ExportTableBorderCorner(ATopLeftCorner, AContentBounds.Left, AContentBounds.Top);
  finally
    ATopLeftCorner.Free;
  end;

  ATopRightCorner := TdxCornerViewInfoBase.CreateCorner(TdxCornerViewInfoType.OuterHorizontalEnd, AConverter, ATopBorder, nil, nil, ARightBorder, 0);
  try
    ExportTableBorderCorner(ATopRightCorner, AContentBounds.Right, AContentBounds.Top);
  finally
    ATopRightCorner.Free;
  end;

  ABottomLeftCorner := TdxCornerViewInfoBase.CreateCorner(TdxCornerViewInfoType.OuterHorizontalEnd, AConverter, nil, ALeftBorder, ABottomBorder, nil, 0);
  try
    ExportTableBorderCorner(ABottomLeftCorner, AContentBounds.Left, AContentBounds.Bottom);
  finally
    ABottomLeftCorner.Free;
  end;

  ABottomRightCorner := TdxCornerViewInfoBase.CreateCorner(TdxCornerViewInfoType.OuterHorizontalEnd, AConverter, ABottomBorder, ARightBorder, nil, nil, 0);
  try
    ExportTableBorderCorner(ABottomRightCorner, AContentBounds.Right, AContentBounds.Bottom);
  finally
    ABottomRightCorner.Free;
  end;
end;

procedure TdxGraphicsDocumentLayoutExporter.ExportFloatingObjectPicture(ABox: TdxFloatingObjectBox; APictureContent: TdxPictureFloatingObjectContent);
begin
  FAdapter.ExportFloatingObjectPicture(Self, ABox, APictureContent);
end;

procedure TdxGraphicsDocumentLayoutExporter.ExportFloatingObjectTextBox(ABox: TdxFloatingObjectBox;
  ATextBoxContent: TdxTextBoxFloatingObjectContent; ATextBoxDocumentLayout: TdxDocumentLayout);
var
  AContentBounds: TRect;
  ARun: TdxFloatingObjectAnchorRun;
begin
  PushBackgroundLayer;
  AContentBounds := GetDrawingBounds(ABox.ContentBounds);
  ARun := ABox.GetFloatingObjectRun;
  SetBackColor(ARun.Shape.FillColor, AContentBounds);
  ExportCompositeObject(AContentBounds, TdxPieceTable(ATextBoxContent.TextBox.PieceTable),
    procedure(const AOldClipRect, AClipRect: TdxRectF)
    begin
      ATextBoxDocumentLayout.Pages.First.ExportTo(Self);
    end);
  PopBackgroundLayer;
end;

procedure TdxGraphicsDocumentLayoutExporter.ExportParagraphFrameTextBox(ABox: TdxParagraphFrameBox;
  ATextBoxDocumentLayout: TdxDocumentLayout);
begin
  ExportCompositeObject(GetDrawingBounds(ABox.ContentBounds), PieceTable,
    procedure(const AOldClipRect, AClipRect: TdxRectF)
    begin
      ExportParagraphFramePage(ATextBoxDocumentLayout.Pages.First, AOldClipRect, AClipRect <> TdxRectF.Null);
    end);
end;

function TdxGraphicsDocumentLayoutExporter.BeginExportCompositeObject(const ABounds: TRect; APieceTable: TdxPieceTable;
  var AOldClipRect, AClipRect: TdxRectF; var AOldPieceTable: TdxPieceTable): Boolean;
begin
  if not IsValidBounds(ABounds) then
    Exit(False);

  AOldClipRect := GetClipBounds;
  AClipRect := IntersectClipBounds(AOldClipRect, ABounds.ToRectF);
  if AClipRect = TdxRectF.Null then
    Exit(False);

  ApplyClipBounds(AClipRect);
  AOldPieceTable := PieceTable;
  PieceTable := APieceTable;
  Result := True;
end;

procedure TdxGraphicsDocumentLayoutExporter.EndExportCompositeObject(const AOldClipRect: TdxRectF; AOldPieceTable: TdxPieceTable);
begin
  PieceTable := AOldPieceTable;
  RestoreClipBounds(AOldClipRect);
end;

procedure TdxGraphicsDocumentLayoutExporter.ApplyClipBoundsCore(const ABounds: TdxRectF);
begin
  Painter.ApplyClipBounds(ABounds);
end;

procedure TdxGraphicsDocumentLayoutExporter.RestoreClipBoundsCore(const ABounds: TdxRectF);
begin
  Painter.RestoreClipBounds(ABounds);
end;

procedure TdxGraphicsDocumentLayoutExporter.ExportCompositeObject(const ABounds: TRect; APieceTable: TdxPieceTable;
  const AAction: TdxGraphicsDocumentLayoutExporter.TAction);
var
  AOldClipRect, AClipRect: TdxRectF;
  AOldPieceTable: TdxPieceTable;
begin
  AOldClipRect.Empty;
  AClipRect.Empty;
  AOldPieceTable := nil;
  if not BeginExportCompositeObject(ABounds, APieceTable, AOldClipRect, AClipRect, AOldPieceTable) then
    Exit;
  try
    AAction(AOldClipRect, AClipRect);
  finally
    EndExportCompositeObject(AOldClipRect, AOldPieceTable);
  end;
end;

procedure TdxGraphicsDocumentLayoutExporter.ExportComment(ACommentViewInfo: TdxCommentViewInfo);
begin
  NotImplemented;
end;

procedure TdxGraphicsDocumentLayoutExporter.ExportTextBoxCore(ABox: TdxBox);
var
  ATextBounds: TRect;
  AText: string;
begin
  ATextBounds := GetDrawingBounds(ABox.Bounds);
  if IsValidBounds(ATextBounds) then
  begin
    AText := GetBoxText(ABox);
    ExportTextBoxCore(ABox, ATextBounds, AText);
  end;
end;

procedure TdxGraphicsDocumentLayoutExporter.ExportTextBoxCoreNoCheckBoundsValidity(ABox: TdxBox);
var
  ATextBounds: TRect;
  AText: string;
begin
  ATextBounds := GetDrawingBounds(ABox.Bounds);

  AText := GetBoxText(ABox);
  ExportTextBoxCore(ABox, ATextBounds, AText);
end;

procedure TdxGraphicsDocumentLayoutExporter.ExportNumberingListBoxCore(ABox: TdxBox);
var
  ATextBounds: TRect;
  AText: string;
  ANumberingListBoxWithSeparator: TdxNumberingListBoxWithSeparator;
begin
  ATextBounds := GetDrawingBounds(ABox.Bounds);
  if IsValidBounds(ATextBounds) then
  begin
    AText := GetBoxText(ABox);
    ExportTextBoxCore(ABox, ATextBounds, AText);
    ANumberingListBoxWithSeparator := Safe<TdxNumberingListBoxWithSeparator>.Cast(ABox);
    if ANumberingListBoxWithSeparator <> nil then
      FWhitespaceStrategy.ExportNumberingListBoxSeparator(ANumberingListBoxWithSeparator);
  end;
end;

procedure TdxGraphicsDocumentLayoutExporter.ExportTextBoxCore(ABox: TdxBox; const ATextBounds: TRect; const AText: string);
var
  AFontInfo: TdxFontInfo;
  AForeColor: TdxAlphaColor;
begin
  AFontInfo := ABox.GetFontInfo(PieceTable);
  AForeColor := GetActualForeColor(ABox, ATextBounds);
  Painter.DrawString(AText, AFontInfo, AForeColor, ATextBounds);
end;

function TdxGraphicsDocumentLayoutExporter.GetActualForeColor(ABox: TdxBox; const ABounds: TRect): TdxAlphaColor;
var
  ABackColor: TdxAlphaColor;
begin
  ABackColor := GetActualBackColor(ABounds);
  Result := GetActualColor(ABox.GetActualForeColor(PieceTable, TextColors, ABackColor));
end;

procedure TdxGraphicsDocumentLayoutExporter.ExportSpaceBoxCore(ABox: TdxBox; const ATextBounds: TRect; const AText: string);
var
  AFontInfo: TdxFontInfo;
  AForeColor: TdxAlphaColor;
begin
  AFontInfo := ABox.GetFontInfo(PieceTable);
  AForeColor := GetActualColor(ABox.GetActualForeColor(PieceTable, TextColors, GetBackColor(ATextBounds)));
  Painter.DrawSpacesString(AText, AFontInfo, AForeColor, ATextBounds);
end;

procedure TdxGraphicsDocumentLayoutExporter.ExportHyphenBox(ABox: TdxHyphenBox);
begin
end;

procedure TdxGraphicsDocumentLayoutExporter.ExportTabSpaceBox(ABox: TdxTabSpaceBox);
var
  ALeaderType: TdxTabLeaderType;
begin
  FWhitespaceStrategy.ExportTabSpaceBox(ABox);

  ALeaderType := ABox.TabInfo.Leader;
  if ALeaderType <> TdxTabLeaderType.None then
    FAdapter.ExportTabLeader(Self, ABox);
end;

procedure TdxGraphicsDocumentLayoutExporter.ExportTabLeaderAsCharacterSequence(ABox: TdxTabSpaceBox);
var
  ATextBounds: TRect;
begin
  ATextBounds := GetDrawingBounds(ABox.Bounds);
  if not IsValidBounds(ATextBounds) then
    Exit;

  ExportTextBoxCore(ABox, ATextBounds, GetTabLeaderText(ABox, ATextBounds));
end;

procedure TdxGraphicsDocumentLayoutExporter.ExportTabLeaderAsUnderline(ABox: TdxTabSpaceBox);
var
  ATextBounds: TRect;
  AUnderline: TdxUnderline;
  AUnderlineCalculator: TdxUnderlineCalculator;
  AUnderlineBox: TdxUnderlineBox;
  AColor: TdxAlphaColor;
begin
  ATextBounds := GetDrawingBounds(ABox.Bounds);
  if not IsValidBounds(ATextBounds) then
    Exit;

  AUnderline := DocumentModel.UnderlineRepository.GetPatternLineByType(GetTabLeaderUnderlineType(ABox));
  AUnderlineCalculator := TdxUnderlineCalculator.Create(PieceTable);
  try
    AUnderlineBox := AUnderlineCalculator.CreateTabLeaderUnderlineBox(ABox, ATextBounds, CurrentRow);
    try
      if ShouldCenterTabLeaderLineVertically(ABox) then
        AUnderlineCalculator.CenterTabLeaderUnderlineBoxVertically(AUnderlineBox, ATextBounds);
      AColor := GetActualColor(ABox.GetActualUnderlineColor(PieceTable, TextColors, CurrentBackColor));
      TdxWinFormsGraphicsDocumentLayoutExporterAdapter(FAdapter).ExportLineBoxCore<TdxUnderlineType>(Self,
        Self, AUnderlineBox, AUnderline, AColor);
    finally
      AUnderlineBox.Free;
    end;
  finally
    AUnderlineCalculator.Free;
  end;
end;

procedure TdxGraphicsDocumentLayoutExporter.ExportLineBreakBox(ABox: TdxLineBreakBox);
begin
  FWhitespaceStrategy.ExportLineBreakBox(ABox);
end;

procedure TdxGraphicsDocumentLayoutExporter.ExportParagraphMarkBox(ABox: TdxParagraphMarkBox);
begin
  FWhitespaceStrategy.ExportParagraphMarkBox(ABox);
end;

procedure TdxGraphicsDocumentLayoutExporter.ExportParagraphMarkBoxCore(ABox: TdxParagraphMarkBox);
var
  ATextBounds: TRect;
  AText: string;
  ARun: TdxTextRunBase;
  ACell: TdxCustomTableCell;
begin
  ATextBounds := GetDrawingBounds(ABox.Bounds);
  if IsValidBounds(ATextBounds) then
  begin
    AText := GetBoxText(ABox);
    ARun := TdxTextRunBase(ABox.GetRun(PieceTable));
    ACell := ARun.Paragraph.GetCellCore;
    if (ACell <> nil) and (ARun.Paragraph.Index = ACell.GetEndParagraphIndexCore) then
      AText := StringOfChar(TdxCharacters.CurrencySign, Length(AText))
    else
      AText := StringOfChar(TdxCharacters.PilcrowSign, Length(AText));
    ExportTextBoxCore(ABox, ATextBounds, AText);
  end;
end;

procedure TdxGraphicsDocumentLayoutExporter.ExportSectionMarkBox(ABox: TdxSectionMarkBox);
begin
  FWhitespaceStrategy.ExportSectionMarkBox(ABox);
end;

procedure TdxGraphicsDocumentLayoutExporter.ExportSectionMarkBoxCore(ABox: TdxSectionMarkBox);
var
  AExporter: TdxSectionBreakMarkBoxExporter;
begin
  AExporter := TdxSectionBreakMarkBoxExporter.Create(Self);
  try
    ExportMarkBoxCore(ABox, AExporter);
  finally
    AExporter.Free;
  end;
end;

procedure TdxGraphicsDocumentLayoutExporter.ExportColumnBreakBox(ABox: TdxColumnBreakBox);
begin
  FWhitespaceStrategy.ExportColumnBreakBox(ABox);
end;

procedure TdxGraphicsDocumentLayoutExporter.ExportColumnBreakBoxCore(ABox: TdxColumnBreakBox);
var
  AExporter: TdxColumnBreakMarkBoxExporter;
begin
  AExporter := TdxColumnBreakMarkBoxExporter.Create(Self);
  try
    ExportMarkBoxCore(ABox, AExporter);
  finally
    AExporter.Free;
  end;
end;

procedure TdxGraphicsDocumentLayoutExporter.ExportPageBreakBox(ABox: TdxPageBreakBox);
begin
  FWhitespaceStrategy.ExportPageBreakBox(ABox);
end;

procedure TdxGraphicsDocumentLayoutExporter.ExportPageBreakBoxCore(ABox: TdxPageBreakBox);
var
  AExporter: TdxPageBreakMarkBoxExporter;
begin
  AExporter := TdxPageBreakMarkBoxExporter.Create(Self);
  try
    ExportMarkBoxCore(ABox, AExporter);
  finally
    AExporter.Free;
  end;
end;

procedure TdxGraphicsDocumentLayoutExporter.ExportMarkBoxCore(ABox: TdxBox; AExporter: TdxMarkBoxExporterBase);
var
  ABounds, ABoxBounds: TRect;
begin
  ABounds := GetDrawingBounds(ABox.Bounds);
  if not IsValidBounds(ABounds) then
    Exit;

  ABoxBounds.InitSize(ABounds.Left, CurrentRow.Bounds.Top, ABounds.Width, CurrentRow.Height);
  AExporter.Export(ABox, ABoxBounds);
end;

procedure TdxGraphicsDocumentLayoutExporter.ExportTableBorder(ABorder: TdxTableBorderViewInfoBase; const ACellBounds: TRect);
var
  ATableBorderExporter: TdxGraphicsDocumentLayoutExporterTableBorder;
  ABorderBase: TdxBorderInfo;
  AConverter: TdxDocumentModelUnitToLayoutUnitConverter;
begin
  ATableBorderExporter := TdxGraphicsDocumentLayoutExporterTableBorder.Create(DocumentModel, Painter, HorizontalLinePainter, VerticalLinePainter, ACellBounds);
  try
    ABorderBase := ABorder.Border;
    AConverter := ABorder.Converter;
    ATableBorderExporter.ExportTableBorder(ABorderBase, GetDrawingBounds(ACellBounds), AConverter, TdxTableBorderViewInfoBase(ABorder));
  finally
    ATableBorderExporter.Free;
  end;
end;

procedure TdxGraphicsDocumentLayoutExporter.ExportTableBorderCorner(ACorner: TdxCornerViewInfoBase; X: Integer; Y: Integer);
var
  ATableBorderExporter: TdxGraphicsDocumentLayoutExporterTableBorder;
begin
  ATableBorderExporter := TdxGraphicsDocumentLayoutExporterTableBorder.Create(DocumentModel, Painter, HorizontalLinePainter, VerticalLinePainter);
  try
    ATableBorderExporter.ExportTableBorderCorner(ACorner, X, Y);
  finally
    ATableBorderExporter.Free;
  end;
end;

procedure TdxGraphicsDocumentLayoutExporter.ExportTableCell(ACell: TdxTableCellViewInfo);
var
  ARect: TRect;
  AColor: TdxAlphaColor;
  ACellInnerTablesCount, I: Integer;
begin
  ARect := ACell.GetBackgroundBounds;
  AColor := ACell.Cell.GetActualBackgroundColor;
  if not TdxAlphaColors.IsEmpty(AColor) then
    Painter.FillRectangle(AColor, GetDrawingBounds(ARect));
  ACellInnerTablesCount := ACell.InnerTables.Count;
  if ACellInnerTablesCount > 0 then
    for I := 0 to ACellInnerTablesCount - 1 do
      ACell.InnerTables[I].ExportBackground(Self);
end;

procedure TdxGraphicsDocumentLayoutExporter.ExportTableRow(ARow: TdxTableRowViewInfoBase);
var
  AOldClip: TdxRectF;
  ADrawingBounds: TRect;
begin
  AOldClip := Painter.ClipBounds;
  try
    ADrawingBounds := GetDrawingBounds(ARow.GetBounds);
    ExcludeCellBounds(ARow.Cells, ADrawingBounds);
    Painter.FillRectangle(GetActualBackgroundColor(ARow), ADrawingBounds);
  finally
    Painter.ClipBounds := AOldClip;
    Painter.ResetCellBoundsClip;
  end;
end;

procedure TdxGraphicsDocumentLayoutExporter.ExcludeCellBounds(ACells: TdxTableCellViewInfoCollection; const ARowBounds: TRect);
var
  ACellsCount, I: Integer;
  ACurrentCellBounds: TRect;
begin
  ACellsCount := ACells.Count;
  for I := 0 to ACellsCount - 1 do
  begin
    ACurrentCellBounds := ACells[I].GetBackgroundBounds;
    Painter.ExcludeCellBounds(ACurrentCellBounds, ARowBounds);
  end;
end;

function TdxGraphicsDocumentLayoutExporter.GetActualBackgroundColor(ARow: TdxTableRowViewInfoBase): TdxAlphaColor;
var
  ATablePropertiesException: TdxTableProperties;
begin
  ATablePropertiesException := ARow.Row.TablePropertiesException;
  if ATablePropertiesException.GetUse(TdxTablePropertiesOptions.MaskUseBackgroundColor) then
    Result := ATablePropertiesException.BackgroundColor
  else
    Result := ARow.TableViewInfo.Table.BackgroundColor;
end;

function TdxGraphicsDocumentLayoutExporter.GetBorderPainter(ACorner: TdxCornerViewInfoBase): TdxTableCornerPainter;
begin
  if ACorner is TdxNoneLineCornerViewInfo then
    Result := nil
  else
    Result := TdxTableCornerPainter.Create;
end;

procedure TdxGraphicsDocumentLayoutExporter.ExportUnderlineBox(ARow: TdxSimpleRow; AUnderlineBox: TdxUnderlineBox);
var
  ABox: TdxBox;
  AColor: TdxAlphaColor;
  AUnderline: TdxUnderline;
begin
  ABox := GetCharacterLineBoxByIndex(ARow, AUnderlineBox.StartAnchorIndex);
  AColor := GetActualColor(ABox.GetActualUnderlineColor(PieceTable, TextColors, GetBackColor(GetDrawingBounds(AUnderlineBox.UnderlineBounds))));
  AUnderline := DocumentModel.UnderlineRepository.GetPatternLineByType(ABox.GetFontUnderlineType(PieceTable));
  TdxWinFormsGraphicsDocumentLayoutExporterAdapter(FAdapter).ExportLineBoxCore<TdxUnderlineType>(Self,
    Self as IdxPatternLinePainter, AUnderlineBox, AUnderline, AColor);
end;

procedure TdxGraphicsDocumentLayoutExporter.ExportStrikeoutBox(ARow: TdxSimpleRow; AStrikeoutBox: TdxUnderlineBox);
var
  ABox: TdxBox;
  AColor: TdxAlphaColor;
  AStrikeout: TdxStrikeout;
begin
  ABox := GetCharacterLineBoxByIndex(ARow, AStrikeoutBox.StartAnchorIndex);
  AColor := GetActualColor(ABox.GetActualStrikeoutColor(PieceTable, TextColors, GetBackColor(GetDrawingBounds(AStrikeoutBox.UnderlineBounds))));
  AStrikeout := DocumentModel.StrikeoutRepository.GetPatternLineByType(ABox.GetFontStrikeoutType(PieceTable));
  TdxWinFormsGraphicsDocumentLayoutExporterAdapter(FAdapter).ExportLineBoxCore<TdxStrikeoutType>(Self,
    Self as IdxPatternLinePainter, AStrikeoutBox, AStrikeout, AColor);
end;

function TdxGraphicsDocumentLayoutExporter.GetClipBounds: TdxRectF;
begin
  Result := Painter.ClipBounds;
end;

procedure TdxGraphicsDocumentLayoutExporter.SetClipBounds(const AClipBounds: TdxRectF);
begin
  Painter.ClipBounds := AClipBounds;
end;

function TdxGraphicsDocumentLayoutExporter.BeginHeaderFooterExport(const AClipBounds: TdxRectF): TdxRectF;
begin
  Result := Painter.ApplyClipBounds(AClipBounds);
end;

procedure TdxGraphicsDocumentLayoutExporter.EndHeaderFooterExport(const AOldClipBounds: TdxRectF);
begin
  Painter.RestoreClipBounds(AOldClipBounds);
end;

procedure TdxGraphicsDocumentLayoutExporter.BeginExportTableCellContent(const AClipBounds: TdxRectF);
begin

  ApplyClipBounds(AClipBounds);
end;

procedure TdxGraphicsDocumentLayoutExporter.EndExportTableContent(const AOldClipBounds: TdxRectF);
begin
  Painter.RestoreClipBounds(AOldClipBounds);
end;

{ TdxScreenOptimizedGraphicsDocumentLayoutExporter }

constructor TdxScreenOptimizedGraphicsDocumentLayoutExporter.Create(ADocumentModel: TdxDocumentModel;
  APainter: TdxPainter; AAdapter: TdxGraphicsDocumentLayoutExporterAdapter; const ABounds: TRect;
  ATextColors: TdxTextColors; AScaleFactor: Single);
begin
  inherited Create(ADocumentModel, APainter, AAdapter, ABounds, False, ATextColors);
  FPixel := Math.Ceil(ADocumentModel.LayoutUnitConverter.PixelsToLayoutUnits(2, APainter.DpiY));
  FScaleFactor := AScaleFactor;
  FErrorWavyLinePen := CreateErrorWavyLinePen;
end;

destructor TdxScreenOptimizedGraphicsDocumentLayoutExporter.Destroy;
begin
  Painter.ReleasePen(FErrorWavyLinePen);
  inherited Destroy;
end;

function TdxScreenOptimizedGraphicsDocumentLayoutExporter.CreatePlaceholderBrush(AForeColor: TdxAlphaColor): TdxGPHatchBrush;
begin
  Result := TdxGPHatchBrush.Create;
  Result.ForegroundColor := AForeColor;
  Result.BackgroundColor := TdxAlphaColors.Empty;
  Result.Style := TdxGpHatchStyle.HatchStyle50Percent;
end;

procedure TdxScreenOptimizedGraphicsDocumentLayoutExporter.ExportBoxOptimized(ABox: TdxBox);
var
  AActualBounds: TRect;
  ABrush: TdxGPHatchBrush;
begin
  AActualBounds := ABox.Bounds;
  AActualBounds.Inflate(0, -FPixel);
  if AActualBounds.Height <= 0 then
    AActualBounds.Height := 1;
  ABrush := CreatePlaceholderBrush(GetActualColor(ABox.GetActualForeColor(PieceTable, TextColors, CurrentBackColor)));
  try
    Painter.Graphics.FillRectangle(AActualBounds, ABrush);
  finally
    ABrush.Free;
  end;
end;

function TdxScreenOptimizedGraphicsDocumentLayoutExporter.GetActualBounds(const ABounds: TRect): TdxRectF;
begin

  Result := ABounds.ToRectF;
end;

procedure TdxScreenOptimizedGraphicsDocumentLayoutExporter.ExportErrorBox(AErrorBox: TdxErrorBox);
var
  ABounds: TdxRectF;
begin
  ABounds := GetActualBounds(AErrorBox.ClipBounds);

  if (TdxSpellCheckerInstance.ISpellChecker3 = nil) or
      (TdxSpellCheckerInstance.ISpellChecker3.CheckAsYouTypeOptions.UnderlineStyle <> usLine) then
    HorizontalLinePainter.DrawWaveUnderline(ABounds, FErrorWavyLinePen, ABounds.Height)
  else
    HorizontalLinePainter.DrawLine(FErrorWavyLinePen, ABounds);
end;

function TdxScreenOptimizedGraphicsDocumentLayoutExporter.CreateErrorWavyLinePen: TdxGPPen;
var
  AColor: TdxAlphaColor;
begin
  if TdxSpellCheckerInstance.ISpellChecker3 = nil then
    AColor := TdxAlphaColors.Red
  else
    AColor := TdxAlphaColors.FromColor(TdxSpellCheckerInstance.ISpellChecker3.CheckAsYouTypeOptions.UnderlineColor);
  Result := Painter.GetPen(AColor);
end;

procedure TdxScreenOptimizedGraphicsDocumentLayoutExporter.ExportHyphenBox(ABox: TdxHyphenBox);
begin
  if ShouldOptimizeBox(ABox) then
    ExportBoxOptimized(ABox)
  else
    inherited ExportHyphenBox(ABox);
end;

procedure TdxScreenOptimizedGraphicsDocumentLayoutExporter.ExportLayoutDependentTextBox(
  ABox: TdxLayoutDependentTextBox);
begin
  if ShouldOptimizeBox(ABox) then
    ExportBoxOptimized(ABox)
  else
    inherited ExportTextBox(ABox);
end;

procedure TdxScreenOptimizedGraphicsDocumentLayoutExporter.ExportLineNumberBox(ABox: TdxLineNumberBox);
begin
  if ShouldOptimizeBox(ABox) then
    ExportBoxOptimized(ABox)
  else
    inherited ExportLineNumberBox(ABox);
end;

procedure TdxScreenOptimizedGraphicsDocumentLayoutExporter.ExportNumberingListBox(ABox: TdxNumberingListBox);
begin
  if ShouldOptimizeBox(ABox) then
    ExportBoxOptimized(ABox)
  else
    inherited ExportNumberingListBox(ABox);
end;

procedure TdxScreenOptimizedGraphicsDocumentLayoutExporter.ExportParagraphMarkBox(ABox: TdxParagraphMarkBox);
begin
  if not ShouldOptimizeBox(ABox) then
    inherited ExportParagraphMarkBox(ABox);
end;

procedure TdxScreenOptimizedGraphicsDocumentLayoutExporter.ExportRowStrikeoutBoxes;
begin
    inherited ExportRowStrikeoutBoxes;
end;

procedure TdxScreenOptimizedGraphicsDocumentLayoutExporter.ExportRowUnderlineBoxes;
begin
    inherited ExportRowUnderlineBoxes;
end;

procedure TdxScreenOptimizedGraphicsDocumentLayoutExporter.ExportSeparatorBox(ABox: TdxSeparatorBox);
begin
  if ShouldOptimizeBox(ABox) then
    ExportBoxOptimized(ABox)
  else
    inherited ExportSeparatorBox(ABox);
end;

procedure TdxScreenOptimizedGraphicsDocumentLayoutExporter.ExportSpaceBox(ABox: TdxBox);
begin
  if not ShouldOptimizeBox(ABox) then
    inherited ExportSpaceBox(ABox);
end;

procedure TdxScreenOptimizedGraphicsDocumentLayoutExporter.ExportTextBox(ABox: TdxTextBox);
begin
  if ShouldOptimizeBox(ABox) then
    ExportBoxOptimized(ABox)
  else
    inherited ExportTextBox(ABox);
end;

procedure TdxScreenOptimizedGraphicsDocumentLayoutExporter.FinishExport;
begin
  Painter.FinishPaint;
end;

function TdxScreenOptimizedGraphicsDocumentLayoutExporter.ShouldOptimizeBox(ABox: TdxBox): Boolean;
begin
  Result := not IsWin10FallCreatorsUpdate and (ABox.Bounds.Height < MinReadableTextHeight);
end;

{ TdxScreenOptimizedGraphicsDocumentLayoutExporterNoLineNumbers }

procedure TdxScreenOptimizedGraphicsDocumentLayoutExporterNoLineNumbers.ExportLineNumberBox(ABox: TdxLineNumberBox);
begin
end;

{ TdxWinFormsGraphicsDocumentLayoutExporterAdapter }

procedure TdxWinFormsGraphicsDocumentLayoutExporterAdapter.ExportLineBreakBoxCore(
  AExporter: TdxGraphicsDocumentLayoutExporter; ABox: TdxLineBreakBox);
var
  AForeColor: TdxAlphaColor;
  ACharacterBounds, AGlyphBounds: TRect;
begin
  ACharacterBounds := AExporter.GetDrawingBounds(ABox.Bounds);
  AGlyphBounds := ACharacterBounds;
  AGlyphBounds.Height := ACharacterBounds.Width;
  AGlyphBounds.Offset(0, (ACharacterBounds.Height - AGlyphBounds.Height) div 2);
  AForeColor := AExporter.GetActualColor(ABox.GetActualForeColor(AExporter.PieceTable, AExporter.TextColors, AExporter.GetBackColor(AGlyphBounds)));
  DrawLineBreakArrow(AExporter, AGlyphBounds, AForeColor);
end;

procedure TdxWinFormsGraphicsDocumentLayoutExporterAdapter.ExportSeparatorBoxCore(
  AExporter: TdxGraphicsDocumentLayoutExporter; ABox: TdxSeparatorBox);
begin
  ExportSingleCharacterMarkBoxCore(AExporter, ABox);
end;

procedure TdxWinFormsGraphicsDocumentLayoutExporterAdapter.ExportSingleCharacterMarkBoxCore(
  AExporter: TdxGraphicsDocumentLayoutExporter; ABox: TdxSingleCharacterMarkBox);
var
  ABoxBounds, ATextBounds: TRect;
  AForeColor: TdxAlphaColor;
  AFontInfo: TdxFontInfo;
  AText: string;
  ATextSize: TSize;
begin
  ABoxBounds := AExporter.GetDrawingBounds(ABox.Bounds);
  if not AExporter.IsValidBounds(ABoxBounds) then
    Exit;
  AForeColor := AExporter.GetActualColor(ABox.GetActualForeColor(AExporter.PieceTable, AExporter.TextColors, AExporter.GetBackColor(ABoxBounds)));
  AFontInfo := ABox.GetFontInfo(AExporter.PieceTable);
  AText := ABox.MarkCharacter;
  ATextSize := AExporter.DocumentModel.FontCache.Measurer.MeasureString(AText, AFontInfo);
  ATextBounds := ABoxBounds;
  ATextBounds := cxRectSetLeft(ATextBounds, ATextBounds.Left + (ATextBounds.Width - ATextSize.cx) div 2);
  ATextBounds.Width := ATextSize.cx;
  ATextBounds := cxRectSetTop(ATextBounds, ATextBounds.Top + (ATextBounds.Height - ATextSize.cy) div 2);
  AExporter.Painter.DrawString(AText, AFontInfo, AForeColor, ATextBounds);
end;

procedure TdxWinFormsGraphicsDocumentLayoutExporterAdapter.ExportTabLeader(AExporter: TdxGraphicsDocumentLayoutExporter;
  ABox: TdxTabSpaceBox);
begin
  AExporter.ExportTabLeaderAsCharacterSequence(ABox);
end;

procedure TdxWinFormsGraphicsDocumentLayoutExporterAdapter.ExportTabSpaceBoxCore(
  AExporter: TdxGraphicsDocumentLayoutExporter; ABox: TdxTabSpaceBox);
begin
  ExportSingleCharacterMarkBoxCore(AExporter, ABox);
end;

destructor TdxWinFormsGraphicsDocumentLayoutExporterAdapter.Destroy;
begin

  inherited;
end;

procedure TdxWinFormsGraphicsDocumentLayoutExporterAdapter.DrawLineBreakArrow(
  AExporter: TdxGraphicsDocumentLayoutExporter; const AGlyphBounds: TRect; AForeColor: TdxAlphaColor);
const
  ArrowPointsTemplate: array[0..8] of TdxPointF = (
    (X: 0.00; Y: 0.60),
    (X: 0.35; Y: 0.35),
    (X: 0.30; Y: 0.55),
    (X: 0.80; Y: 0.55),
    (X: 0.80; Y: 0.10),
    (X: 0.90; Y: 0.10),
    (X: 0.90; Y: 0.65),
    (X: 0.30; Y: 0.65),
    (X: 0.35; Y: 0.85)
  );
var
  APoint: TdxPointF;
  ABrush: TdxGPBrush;
  I, ACount: Integer;
  AArrowPoints: TArray<TdxPointF>;
begin
  ACount := Length(ArrowPointsTemplate);
  SetLength(AArrowPoints, ACount);
  for I := 0 to ACount - 1 do
  begin
    APoint := ArrowPointsTemplate[I];
    AArrowPoints[I] := dxPointF(AGlyphBounds.Left + AGlyphBounds.Width * APoint.X,
      AGlyphBounds.Top + AGlyphBounds.Height * APoint.Y);
  end;

  ABrush := TdxGPBrush.Create;
  try
    ABrush.Color := AForeColor;
    AExporter.Painter.FillPolygon(ABrush, AArrowPoints);
  finally
    ABrush.Free;
  end;
end;

procedure TdxWinFormsGraphicsDocumentLayoutExporterAdapter.ExportFloatingObjectPicture(
  AExporter: TdxGraphicsDocumentLayoutExporter; ABox: TdxFloatingObjectBox;
  APictureContent: TdxPictureFloatingObjectContent);
var
  AImgBounds: TRect;
begin
  AImgBounds := AExporter.GetDrawingBounds(ABox.ContentBounds);
  if AExporter.IsValidBounds(AImgBounds) then
  begin
    AExporter.Painter.DrawImage(APictureContent.Image.Image, AImgBounds);
    if AExporter.ShouldGrayContent then
      AExporter.Painter.FillRectangle(TdxAlphaColors.FromArgb($80, $FF, $FF, $FF), AImgBounds);
  end;
end;

procedure TdxWinFormsGraphicsDocumentLayoutExporterAdapter.ExportInlinePictureBox(
  AExporter: TdxGraphicsDocumentLayoutExporter; ABox: TdxInlinePictureBox);
var
  AImg: TdxOfficeImage;
  APainter: TdxPainter;
  ASize: TSize;
  AImgBounds, ARowBounds: TRect;
  ASizing: TdxImageSizeMode;
  AOldClip, AClipRect: TdxRectF;
begin
  AImg := ABox.GetImage(AExporter.PieceTable, AExporter.ReadOnly);
  AImgBounds := AExporter.GetDrawingBounds(ABox.Bounds);
  ASize := ABox.GetImageActualSizeInLayoutUnits(AExporter.PieceTable);
  APainter := AExporter.Painter;
  ASizing := ABox.GetSizing(AExporter.PieceTable);
  if AExporter.IsValidBounds(AImgBounds) then
  begin
      ARowBounds := AExporter.CurrentRow.Bounds;
      if ABox.Bounds.Height > ARowBounds.Height then
      begin
          AOldClip := APainter.ClipBounds;
          AClipRect := AExporter.GetDrawingBounds(ARowBounds).ToRectF;
          try
            AClipRect.Intersect(AOldClip);
            APainter.ClipBounds := AClipRect;
            ExportInlinePictureBoxCore(AExporter, ABox, AImg, AImgBounds, ASize, ASizing);
          finally
            APainter.ClipBounds := AOldClip;
          end;
      end
      else
        ExportInlinePictureBoxCore(AExporter, ABox, AImg, AImgBounds, ASize, ASizing);
  end;
end;

procedure TdxWinFormsGraphicsDocumentLayoutExporterAdapter.ExportInlinePictureBoxCore(
  AExporter: TdxGraphicsDocumentLayoutExporter; ABox: TdxInlinePictureBox; AImg: TdxOfficeImage; const AImgBounds: TRect;
  const ASize: TSize; ASizing: TdxImageSizeMode);
var
  APainter: TdxPainter;
  APictureRun: TdxInlinePictureRun;
begin
  APainter := AExporter.Painter;
  APictureRun := TdxInlinePictureRun(ABox.GetRun(AExporter.PieceTable));
  if APictureRun.Properties.FillColor <> TdxAlphaColors.Empty then
    APainter.FillRectangle(APictureRun.Properties.FillColor, AImgBounds);
  APainter.DrawImage(AImg, AImgBounds, ASize, ASizing);
  ABox.ExportHotZones(APainter);
  if AExporter.ShouldGrayContent then
    APainter.FillRectangle(TdxAlphaColors.FromArgb($80, $ff, $ff, $ff), AImgBounds);
end;

procedure TdxWinFormsGraphicsDocumentLayoutExporterAdapter.ExportLineBoxCore<T>(
  AExporter: TdxGraphicsDocumentLayoutExporter; const ALinePainter: IdxPatternLinePainter;
  ALineBox: TdxUnderlineBox; ALine: TdxPatternLine<T>; ALineColor: TdxAlphaColor);
var
  AUnderlineClipBounds, AActualUnderlineBounds: TRect;
  APainter: TdxPainter;
  AOldClip, AClipRect: TdxRectF;
begin
  AUnderlineClipBounds := ALine.CalcLineBounds(ALineBox.ClipBounds, ALineBox.UnderlineThickness);
  AUnderlineClipBounds := AExporter.GetDrawingBounds(AUnderlineClipBounds);

  APainter := AExporter.Painter;
  AOldClip := APainter.ClipBounds;
  AClipRect.InitSize(AUnderlineClipBounds.Left, AOldClip.Top, AUnderlineClipBounds.Width, AOldClip.Height);
  try
    AClipRect.Intersect(AOldClip);
    APainter.ClipBounds := AClipRect;

    AActualUnderlineBounds := ALine.CalcLineBounds(ALineBox.UnderlineBounds, ALineBox.UnderlineThickness);
    AActualUnderlineBounds.Y := AActualUnderlineBounds.Top +
      Trunc(ALine.CalcLinePenVerticalOffset(AActualUnderlineBounds.ToRectF));
    AActualUnderlineBounds := AExporter.GetDrawingBounds(AActualUnderlineBounds);
    ALine.Draw(ALinePainter, AActualUnderlineBounds.ToRectF, ALineColor);
  finally
    APainter.ClipBounds := AOldClip;
  end;
end;

{ TdxGraphicsDocumentLayoutExporterWhitespaceStrategy }

constructor TdxGraphicsDocumentLayoutExporterWhitespaceStrategy.Create(AExporter: TdxGraphicsDocumentLayoutExporter);
begin
  inherited Create;
  Assert(AExporter <> nil);
  FExporter := AExporter;
end;

{ TdxGraphicsDocumentLayoutExporterHideWhitespaceStrategy }

procedure TdxGraphicsDocumentLayoutExporterHideWhitespaceStrategy.ExportColumnBreakBox(ABox: TdxColumnBreakBox);
begin
end;

procedure TdxGraphicsDocumentLayoutExporterHideWhitespaceStrategy.ExportLineBreakBox(ABox: TdxLineBreakBox);
begin
end;

procedure TdxGraphicsDocumentLayoutExporterHideWhitespaceStrategy.ExportPageBreakBox(ABox: TdxPageBreakBox);
begin
end;

procedure TdxGraphicsDocumentLayoutExporterHideWhitespaceStrategy.ExportParagraphMarkBox(ABox: TdxParagraphMarkBox);
begin
  if FormattingMarkVisibility.ParagraphMark = TdxRichEditFormattingMarkVisibility.Visible then
    inherited ExportParagraphMarkBox(ABox);
end;

procedure TdxGraphicsDocumentLayoutExporterHideWhitespaceStrategy.ExportSectionMarkBox(ABox: TdxSectionMarkBox);
begin
end;

procedure TdxGraphicsDocumentLayoutExporterHideWhitespaceStrategy.ExportSeparatorBox(ABox: TdxSeparatorBox);
begin
  if FormattingMarkVisibility.Separator = TdxRichEditFormattingMarkVisibility.Visible then
    inherited ExportSeparatorBox(ABox);
end;

procedure TdxGraphicsDocumentLayoutExporterHideWhitespaceStrategy.ExportSpaceBox(ABox: TdxBox);
begin
  if not (FormattingMarkVisibility.Space <> TdxRichEditFormattingMarkVisibility.Visible) then
    inherited ExportSpaceBox(ABox);
end;

procedure TdxGraphicsDocumentLayoutExporterHideWhitespaceStrategy.ExportTabSpaceBox(ABox: TdxTabSpaceBox);
begin
  if FormattingMarkVisibility.TabCharacter = TdxRichEditFormattingMarkVisibility.Visible then
    inherited ExportTabSpaceBox(ABox);
end;

function TdxGraphicsDocumentLayoutExporterHideWhitespaceStrategy.GetFormattingMarkVisibility: TdxFormattingMarkVisibilityOptions;
begin
  Result := Exporter.DocumentModel.FormattingMarkVisibilityOptions;
end;

{ TdxGraphicsDocumentLayoutExporterShowWhitespaceStrategy }

procedure TdxGraphicsDocumentLayoutExporterShowWhitespaceStrategy.ExportColumnBreakBox(ABox: TdxColumnBreakBox);
begin
  Exporter.ExportColumnBreakBoxCore(ABox);
end;

procedure TdxGraphicsDocumentLayoutExporterShowWhitespaceStrategy.ExportLineBreakBox(ABox: TdxLineBreakBox);
begin
  Exporter.Adapter.ExportLineBreakBoxCore(Exporter, ABox);
end;

procedure TdxGraphicsDocumentLayoutExporterShowWhitespaceStrategy.ExportNumberingListBoxSeparator(
  ANumberingListBoxWithSeparator: TdxNumberingListBoxWithSeparator);
var
  ATabSpaceBox: TdxTabSpaceBox;
begin
  ATabSpaceBox := Safe<TdxTabSpaceBox>.Cast(ANumberingListBoxWithSeparator.SeparatorBox);
  if (ATabSpaceBox <> nil) and not ATabSpaceBox.Bounds.IsEmpty then
    ExportTabSpaceBox(ATabSpaceBox);
end;

procedure TdxGraphicsDocumentLayoutExporterShowWhitespaceStrategy.ExportPageBreakBox(ABox: TdxPageBreakBox);
begin
  Exporter.ExportPageBreakBoxCore(ABox);
end;

procedure TdxGraphicsDocumentLayoutExporterShowWhitespaceStrategy.ExportParagraphMarkBox(ABox: TdxParagraphMarkBox);
begin
  Exporter.ExportParagraphMarkBoxCore(ABox);
end;

procedure TdxGraphicsDocumentLayoutExporterShowWhitespaceStrategy.ExportSectionMarkBox(ABox: TdxSectionMarkBox);
begin
  Exporter.ExportSectionMarkBoxCore(ABox);
end;

procedure TdxGraphicsDocumentLayoutExporterShowWhitespaceStrategy.ExportSeparatorBox(ABox: TdxSeparatorBox);
begin
  Exporter.Adapter.ExportSeparatorBoxCore(Exporter, ABox);
end;

procedure TdxGraphicsDocumentLayoutExporterShowWhitespaceStrategy.ExportSpaceBox(ASpaceBox: TdxBox);
var
  ATextBounds: TRect;
  AText: string;
begin
  ATextBounds := Exporter.GetDrawingBounds(ASpaceBox.Bounds);
  if Exporter.IsValidBounds(ATextBounds) then
  begin
    AText := Exporter.GetBoxText(ASpaceBox);
    AText := StringOfChar(TdxCharacters.MiddleDot, Length(AText));
    Exporter.ExportSpaceBoxCore(ASpaceBox, ATextBounds, AText);
  end;
end;

procedure TdxGraphicsDocumentLayoutExporterShowWhitespaceStrategy.ExportTabSpaceBox(ABox: TdxTabSpaceBox);
begin
  Exporter.Adapter.ExportTabSpaceBoxCore(Exporter, ABox);
end;

{ TdxCharacterBoxLevelDocumentLayoutExporter }

constructor TdxCharacterBoxLevelDocumentLayoutExporter.Create(
  ADocumentModel: TdxDocumentModel; AChars: TdxCharacterBoxCollection;
  AMeasurer: TdxBoxMeasurer);
begin
  inherited Create(ADocumentModel);
  FChars := AChars;
  FMeasurer := AMeasurer;
end;

procedure TdxCharacterBoxLevelDocumentLayoutExporter.ExportColumnBreakBox(
  ABox: TdxColumnBreakBox);
begin
  ExportSingleCharacterBox(ABox);
end;

procedure TdxCharacterBoxLevelDocumentLayoutExporter.ExportHyphenBox(
  ABox: TdxHyphenBox);
begin
  ExportSingleCharacterBox(ABox);
end;

procedure TdxCharacterBoxLevelDocumentLayoutExporter.ExportInlinePictureBox(
  ABox: TdxInlinePictureBox);
begin
  ExportSingleCharacterBox(ABox);
end;

procedure TdxCharacterBoxLevelDocumentLayoutExporter.ExportLayoutDependentTextBox(
  ABox: TdxLayoutDependentTextBox);
begin
  ExportSingleCharacterBox(ABox);
end;

procedure TdxCharacterBoxLevelDocumentLayoutExporter.ExportLineBreakBox(
  ABox: TdxLineBreakBox);
begin
  ExportSingleCharacterBox(ABox);
end;

procedure TdxCharacterBoxLevelDocumentLayoutExporter.ExportNumberingListBox(
  ABox: TdxNumberingListBox);
begin
  ExportSingleCharacterBox(ABox);
end;

procedure TdxCharacterBoxLevelDocumentLayoutExporter.ExportPageBreakBox(
  ABox: TdxPageBreakBox);
begin
  ExportSingleCharacterBox(ABox);
end;

procedure TdxCharacterBoxLevelDocumentLayoutExporter.ExportParagraphMarkBox(
  ABox: TdxParagraphMarkBox);
begin
  ExportSingleCharacterBox(ABox);
end;

procedure TdxCharacterBoxLevelDocumentLayoutExporter.ExportRowBox(ARow: TdxRow; ABox: TdxBox);
var
  APreviousPieceTable: TdxPieceTable;
begin
  FRowBounds := ARow.Bounds;
  APreviousPieceTable := PieceTable;
  try
    PieceTable := TdxPieceTable(ARow.Paragraph.PieceTable);
    ABox.ExportTo(Self);
  finally
    PieceTable := APreviousPieceTable;
  end;
end;

procedure TdxCharacterBoxLevelDocumentLayoutExporter.ExportRowCore;
var
  APreviousPieceTable: TdxPieceTable;
begin
  FRowBounds := CurrentRow.Bounds;
  APreviousPieceTable := PieceTable;
  try
    PieceTable := TdxPieceTable(CurrentRow.Paragraph.PieceTable);
    inherited ExportRowCore;
  finally
    PieceTable := APreviousPieceTable;
  end;
end;

procedure TdxCharacterBoxLevelDocumentLayoutExporter.ExportSingleCharacterBox(ABox: TdxBox);
begin
  AppendCharacterBox(ABox.StartPos, 0, GetActualCharacterBounds(ABox.Bounds), ABox.Bounds);
end;

procedure TdxCharacterBoxLevelDocumentLayoutExporter.ExportSectionMarkBox(
  ABox: TdxSectionMarkBox);
begin
  ExportSingleCharacterBox(ABox);
end;

procedure TdxCharacterBoxLevelDocumentLayoutExporter.ExportSeparatorBox(
  ABox: TdxSeparatorBox);
begin
  ExportSingleCharacterBox(ABox);
end;

procedure TdxCharacterBoxLevelDocumentLayoutExporter.ExportSpaceBox(ABox: TdxBox);
var
  ASpaceCount: Integer;
  ACharacterBounds: TArray<TRect>;
begin
  ASpaceCount := ABox.EndPos.Offset - ABox.StartPos.Offset + 1;
  ACharacterBounds := RectSplitHorizontally(ABox.Bounds, ASpaceCount);
  ExportMultiCharacterBox(ABox, ACharacterBounds);
end;

procedure TdxCharacterBoxLevelDocumentLayoutExporter.ExportTabSpaceBox(
  ABox: TdxTabSpaceBox);
begin
  ExportSingleCharacterBox(ABox);
end;

procedure TdxCharacterBoxLevelDocumentLayoutExporter.ExportTextBox(
  ABox: TdxTextBox);
var
  ACharacterBounds: TArray<TRect>;
begin
  ACharacterBounds := CalculateCharacterBounds(ABox);
  ExportMultiCharacterBox(ABox, ACharacterBounds);
end;

function TdxCharacterBoxLevelDocumentLayoutExporter.GetPainter: TdxPainter;
begin
  raise TdxNotImplementedException.Create;
end;

procedure TdxCharacterBoxLevelDocumentLayoutExporter.AppendCharacterBox(
  const AStartPos: TdxFormatterPosition;
  AOffset: Integer; const ABounds, ATightBounds: TRect);
var
  ACharacterBox: TdxCharacterBox;
  ANewStartPos: TdxFormatterPosition;
begin
  ACharacterBox := TdxCharacterBox.Create;
  ACharacterBox.Bounds := ABounds;
  ACharacterBox.TightBounds := ATightBounds;
  ANewStartPos := TdxFormatterPosition.Create(AStartPos.RunIndex, AStartPos.Offset + AOffset, AStartPos.BoxIndex);
  ACharacterBox.StartPos := ANewStartPos;
  ACharacterBox.EndPos := ANewStartPos;
  Chars.Add(ACharacterBox);
end;

function TdxCharacterBoxLevelDocumentLayoutExporter.CalculateCharacterBounds(ABox: TdxTextBox): TArray<TRect>;
begin
  Result := Measurer.MeasureCharactersBounds(GetBoxText(ABox), ABox.GetFontInfo(PieceTable), ABox.Bounds);
end;

function TdxCharacterBoxLevelDocumentLayoutExporter.GetActualCharacterBounds(const ABounds: TRect): TRect;
begin
  Result.Init(ABounds.Left, FRowBounds.Top, ABounds.Right, FRowBounds.Bottom);
end;

procedure TdxCharacterBoxLevelDocumentLayoutExporter.ExportMultiCharacterBox(ABox: TdxBox; ACharacterBounds: TArray<TRect>);
var
  AStartPos: TdxFormatterPosition;
  ACount: Integer;
  I: Integer;
begin
  AStartPos := ABox.StartPos;
  ACount := ABox.EndPos.Offset - AStartPos.Offset + 1;
  Assert(Length(ACharacterBounds) = ACount);
  for I := 0 to ACount - 1 do
    AppendCharacterBox(AStartPos, I, GetActualCharacterBounds(ACharacterBounds[I]), ACharacterBounds[I]);
end;

{ TdxGraphicsPainterWrapper }

constructor TdxGraphicsPainterWrapper.Create(APainter: TdxPainter; const AHorizontalLinePainter,
  AVerticalLinePainter: IdxCharacterLinePainter);
begin
  inherited Create;
  FPainter := APainter;
  FHorizontalLinePainter := AHorizontalLinePainter;
  FVerticalLinePainter := AVerticalLinePainter;
end;

procedure TdxGraphicsPainterWrapper.FillRectangle(AColor: TdxAlphaColor; const ABounds: TdxRectF);
begin
  FPainter.FillRectangle(AColor, ABounds);
end;

function TdxGraphicsPainterWrapper.GetHorizontalLinePainter: IdxCharacterLinePainter;
begin
  Result := FHorizontalLinePainter;
end;

function TdxGraphicsPainterWrapper.GetSnappedPoint(const APoint: TdxPointF): TdxPointF;
begin
  Result := FPainter.GetSnappedPoint(APoint);
end;

function TdxGraphicsPainterWrapper.GetVerticalLinePainter: IdxCharacterLinePainter;
begin
  Result := FVerticalLinePainter;
end;

procedure TdxGraphicsPainterWrapper.SnapHeights(var AHeights: TArray<Single>);
begin
  FPainter.SnapHeights(AHeights);
end;

procedure TdxGraphicsPainterWrapper.SnapWidths(var AWidths: TArray<Single>);
begin
  FPainter.SnapWidths(AWidths);
end;

{ TdxGraphicsDocumentLayoutExporterTableBorder }

constructor TdxGraphicsDocumentLayoutExporterTableBorder.Create(ADocumentModel: TdxDocumentModel; APainter: TdxPainter;
  AHorizontalLinePainter, AVerticalLinePainter: TdxRichEditPatternLinePainter; const ABounds: TRect);
begin
  inherited Create;
  Assert(APainter <> nil);
  Assert(AHorizontalLinePainter <> nil);
  Assert(AVerticalLinePainter <> nil);
  Assert(ADocumentModel<> nil);
  FPainter := APainter;
  FHorizontalLinePainter := AHorizontalLinePainter;
  FVerticalLinePainter := AVerticalLinePainter;
  FDocumentModel := ADocumentModel;
  FBounds := ABounds;
end;

constructor TdxGraphicsDocumentLayoutExporterTableBorder.Create(ADocumentModel: TdxDocumentModel; APainter: TdxPainter;
  AHorizontalLinePainter, AVerticalLinePainter: TdxRichEditPatternLinePainter);
begin
  inherited Create;
  Assert(APainter <> nil);
  Assert(AHorizontalLinePainter <> nil);
  Assert(AVerticalLinePainter <> nil);
  Assert(ADocumentModel<> nil);
  FPainter := APainter;
  FHorizontalLinePainter := AHorizontalLinePainter;
  FVerticalLinePainter := AVerticalLinePainter;
  FDocumentModel := ADocumentModel;
end;

procedure TdxGraphicsDocumentLayoutExporterTableBorder.ExportTableBorder(ABorder: TdxBorderInfo;
  const ADrawingBounds: TRect; AConverter: TdxDocumentModelUnitToLayoutUnitConverter;
  const AViewInfo: TdxTableBorderViewInfoBase);
var
  ABorderPainter: TdxTableBorderPainter;
  APainterWrapper: TdxGraphicsPainterWrapper;
begin
  APainterWrapper := TdxGraphicsPainterWrapper.Create(Painter, HorizontalLinePainter, VerticalLinePainter);
  try
    ABorderPainter := GetBorderPainter(ABorder, AConverter, APainterWrapper);
    if ABorderPainter <> nil then
      try
        ABorderPainter.DrawBorder(AViewInfo, ADrawingBounds);
      finally
        ABorderPainter.Free;
      end;
  finally
    APainterWrapper.Free;
  end;
end;

procedure TdxGraphicsDocumentLayoutExporterTableBorder.ExportTableBorderCorner(ACorner: TdxCornerViewInfoBase; X, Y: Integer);
var
  ALocation: TPoint;
  ABorderPainter: TdxTableCornerPainter;
  APainterWrapper: TdxGraphicsPainterWrapper;
begin
  ABorderPainter := GetBorderPainter(ACorner);
  if ABorderPainter <> nil then
    try
      ALocation.Init(X, Y);
      ALocation.Offset(Offset);
      APainterWrapper := TdxGraphicsPainterWrapper.Create(Painter, HorizontalLinePainter, VerticalLinePainter);
      try
        ABorderPainter.DrawCorner(APainterWrapper, ALocation.X, ALocation.Y, ACorner);
      finally
        APainterWrapper.Free;
      end;
    finally
      ABorderPainter.Free;
    end;
end;

function TdxGraphicsDocumentLayoutExporterTableBorder.GetBorderPainter(
  ACorner: TdxCornerViewInfoBase): TdxTableCornerPainter;
begin
  if ACorner is TdxNoneLineCornerViewInfo then
    Result := nil
  else
    Result := TdxTableCornerPainter.Create;
end;

function TdxGraphicsDocumentLayoutExporterTableBorder.GetBorderPainter(ABorder: TdxBorderInfo;
  AConverter: TdxDocumentModelUnitToLayoutUnitConverter; APainterWrapper: TdxGraphicsPainterWrapper): TdxTableBorderPainter;
var
  AWidth: Single;
  AActualWidth: Single;
  ACompoundArray: TArray<Single>;
  ABorderStyle: TdxBorderLineStyle;
  ABorderCalculator: TdxTableBorderCalculator;
begin
  ABorderStyle := ABorder.Style;
  if (ABorderStyle = TdxBorderLineStyle.None) or (ABorderStyle = TdxBorderLineStyle.Nil) or
    (ABorderStyle = TdxBorderLineStyle.Disabled) then
    Exit(nil);
  ABorderCalculator := TdxTableBorderCalculator.Create;
  try
    AActualWidth := ABorderCalculator.GetActualWidth(ABorder);
    AWidth := AConverter.ToLayoutUnits(AActualWidth);
    ACompoundArray := ABorderCalculator.GetDrawingCompoundArray(ABorder);
  finally
    ABorderCalculator.Free;
  end;
  if Length(ACompoundArray) = 4 then
    Result := TdxDoubleBorderPainter.Create(APainterWrapper, ACompoundArray, AWidth)
  else
    if Length(ACompoundArray) = 6 then
      Result := TdxTripleBorderPainter.Create(APainterWrapper, ACompoundArray, AWidth)
    else
      Result := TdxSingleBorderPainter.Create(APainterWrapper, AWidth, GetTableBorderLine(ABorder.Style, FDocumentModel));
end;

function TdxGraphicsDocumentLayoutExporterTableBorder.GetOffset: TPoint;
begin
  Result := FBounds.Location;
end;

function TdxGraphicsDocumentLayoutExporterTableBorder.GetTableBorderLine(ABorderLineStyle: TdxBorderLineStyle;
  ADocumentModel: TdxDocumentModel): TdxUnderline;
var
  ARepository: TdxBorderLineRepository;
begin
  if ABorderLineStyle = TdxBorderLineStyle.Single then
    Exit(nil);
  ARepository := ADocumentModel.BorderLineRepository;
  Result := ARepository.GetCharacterLineByType(ABorderLineStyle);
end;

{ TdxColumnBreakMarkBoxExporter }

function TdxColumnBreakMarkBoxExporter.GetText: string;
begin
  Result := 'Column Break';
end;

procedure TdxColumnBreakMarkBoxExporter.DrawLine(const ALineBounds: TRect; AForeColor: TdxAlphaColor);
begin
  Exporter.DrawPatternLine(ALineBounds.ToRectF, AForeColor, Exporter.HorizontalLinePainter.ColumnBreakPattern);
end;

{ TdxPageBreakMarkBoxExporter }

function TdxPageBreakMarkBoxExporter.GetText: string;
begin
  Result := 'Page Break';
end;

procedure TdxPageBreakMarkBoxExporter.DrawLine(const ALineBounds: TRect; AForeColor: TdxAlphaColor);
begin
  Exporter.DrawPatternLine(ALineBounds.ToRectF, AForeColor, Exporter.HorizontalLinePainter.PageBreakPattern);
end;

{ TdxSectionBreakMarkBoxExporter }

function TdxSectionBreakMarkBoxExporter.GetText: string;
begin
  Result := 'Section Break';
end;

procedure TdxSectionBreakMarkBoxExporter.DrawLine(const ALineBounds: TRect; AForeColor: TdxAlphaColor);
var
  ABounds: TRect;
begin
  ABounds := ALineBounds;
  ABounds.Offset(0, -3);
  Exporter.DrawPatternLine(ABounds.ToRectF, AForeColor, Exporter.HorizontalLinePainter.SectionStartPattern);
  ABounds.Offset(0, 6);
  Exporter.DrawPatternLine(ABounds.ToRectF, AForeColor, Exporter.HorizontalLinePainter.SectionStartPattern);
end;

end.
