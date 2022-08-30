{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressPrinting System                                   }
{                                                                    }
{           Copyright (C) 1998-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSPRINTING SYSTEM AND            }
{   ALL ACCOMPANYING VCL CONTROLS AS PART OF AN                      }
{   EXECUTABLE PROGRAM ONLY.                                         }
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

unit dxPSRichEditControlLnk.DocumentExporter;

{$I cxVer.inc}

{.$DEFINE DXLOGGING}

interface

uses
{$IFDEF DELPHIXE2}
  System.UITypes,
{$ENDIF}
  Types, Windows, Messages, Classes, SysUtils, Graphics, Generics.Defaults, Generics.Collections, Controls,
  StdCtrls, Forms, ActiveX, dxCoreClasses, cxGeometry, cxLookAndFeels, cxGraphics, cxControls, cxClasses,
  dxMessages, dxGDIPlusAPI, dxGDIPlusClasses, cxLookAndFeelPainters, dxCoreGraphics, dxPSCore,

  dxRichEdit.Types,
  dxRichEdit.Utils.Types,
  dxRichEdit.Utils.Graphics,
  dxRichEdit.Utils.OfficeImage,
  dxRichEdit.Utils.Properties,
  dxRichEdit.Utils.TextColors,
  dxRichEdit.InnerControl,
  dxRichEdit.View.Core,
  dxRichEdit.View.ViewInfo,
  dxRichEdit.DocumentModel.ShapeFormatting,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Boxes.Core,
  dxRichEdit.DocumentModel.Boxes.Simple,
  dxRichEdit.DocumentModel.Boxes,
  dxRichEdit.DocumentModel.Bookmarks,
  dxRichEdit.DocumentModel.Paragraphs,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.PatternLine,
  dxRichEdit.DocumentModel.Section,
  dxRichEdit.DocumentModel.SectionFormatting,
  dxRichEdit.DocumentModel.CharacterFormatting,
  dxRichEdit.DocumentModel.ParagraphFormatting,
  dxRichEdit.DocumentModel.TabFormatting,
  dxRichEdit.DocumentModel.InlineObjectRange,
  dxRichEdit.DocumentModel.FloatingObjectRange,
  dxRichEdit.Platform.PatternLinePainter,
  dxPSRichEditControlLnk.Bricks,
  dxRichEdit.DocumentLayout,
  dxRichEdit.DocumentLayout.UnitConverter,
  dxRichEdit.DocumentLayout.Painters,
  dxRichEdit.Platform.Win.Painter,
  dxRichEdit.DocumentLayout.CommentPadding,
  dxRichEdit.View.PrintLayout;

type
  TdxMargins = TRect;

  TdxPaperKindInfo = record
  public
    PaperKind: TdxPaperKind;
    Landscape: Boolean;
    constructor Create(APaperKind: TdxPaperKind; ALandscape: Boolean);
    class function Default: TdxPaperKindInfo; static;
  end;

  TdxBrick = TdxReportCell;
  TdxLineType = (lTHorizontal, lTUp, lTDown);

  TdxPrintingDocumentExporter = class;

  { TdxEmptyPatternLinePaintingSupport }

  TdxEmptyPatternLinePaintingSupport = class(TInterfacedObject, IdxPatternLinePaintingSupport)
  public
    // IdxPatternLinePaintingSupport
    procedure DrawLine(APen: TdxGPPen; AX1: Single; AY1: Single; AX2: Single; AY2: Single);
    procedure DrawLines(APen: TdxGPPen; const APoints: TArray<TdxPointF>);
    function GetBrush(AColor: TdxAlphaColor): TdxGPBrush;
    function GetPen(AColor: TdxAlphaColor; AThickness: Single): TdxGPColorPen; overload;
    function GetPen(AColor: TdxAlphaColor): TdxGPColorPen; overload;
    procedure ReleaseBrush(ABrush: TdxGPBrush);
    procedure ReleasePen(APen: TdxGPPen);
  end;

  { TdxHorizontalPrinterCharacterLinePainter }

  TdxHorizontalPrinterCharacterLinePainter = class(TcxIUnknownObject,
    IdxUnderlinePainter,
    IdxCharacterLinePainter)
  strict private
    FExporter: TdxPrintingDocumentExporter;
    function GetDocumentModel: TdxDocumentModel;
  protected
    function IsHorizontal: Boolean; virtual;
    procedure DrawCore(ALine: TdxUnderline; const ABoundsF: TdxRectF; AColor: TdxAlphaColor);

    property DocumentModel: TdxDocumentModel read GetDocumentModel;
    property Exporter: TdxPrintingDocumentExporter read FExporter;
  public
    constructor Create(AExporter: TdxPrintingDocumentExporter);

    procedure DrawStrikeout(AUnderline: TdxStrikeoutDouble; const ABounds: TdxRectF; AColor: TdxAlphaColor); overload;
    procedure DrawStrikeout(AUnderline: TdxStrikeoutSingle; const ABounds: TdxRectF; AColor: TdxAlphaColor); overload;

    procedure DrawUnderline(AUnderline: TdxUnderlineWave; const ABounds: TdxRectF; AColor: TdxAlphaColor); overload;
    procedure DrawUnderline(AUnderline: TdxUnderlineDoubleWave; const ABounds: TdxRectF; AColor: TdxAlphaColor); overload;
    procedure DrawUnderline(AUnderline: TdxUnderlineThickLongDashed; const ABounds: TdxRectF; AColor: TdxAlphaColor); overload;
    procedure DrawUnderline(AUnderline: TdxUnderlineThickDashDotDotted; const ABounds: TdxRectF; AColor: TdxAlphaColor); overload;
    procedure DrawUnderline(AUnderline: TdxUnderlineThickDashDotted; const ABounds: TdxRectF; AColor: TdxAlphaColor); overload;
    procedure DrawUnderline(AUnderline: TdxUnderlineThickDashed; const ABounds: TdxRectF; AColor: TdxAlphaColor); overload;
    procedure DrawUnderline(AUnderline: TdxUnderlineThickDotted; const ABounds: TdxRectF; AColor: TdxAlphaColor); overload;
    procedure DrawUnderline(AUnderline: TdxUnderlineThickSingle; const ABounds: TdxRectF; AColor: TdxAlphaColor); overload;
    procedure DrawUnderline(AUnderline: TdxUnderlineLongDashed; const ABounds: TdxRectF; AColor: TdxAlphaColor); overload;
    procedure DrawUnderline(AUnderline: TdxUnderlineHeavyWave; const ABounds: TdxRectF; AColor: TdxAlphaColor); overload;
    procedure DrawUnderline(AUnderline: TdxUnderlineDouble; const ABounds: TdxRectF; AColor: TdxAlphaColor); overload;
    procedure DrawUnderline(AUnderline: TdxUnderlineDashDotDotted; const ABounds: TdxRectF; AColor: TdxAlphaColor); overload;
    procedure DrawUnderline(AUnderline: TdxUnderlineDashDotted; const ABounds: TdxRectF; AColor: TdxAlphaColor); overload;
    procedure DrawUnderline(AUnderline: TdxUnderlineDashSmallGap; const ABounds: TdxRectF; AColor: TdxAlphaColor); overload;
    procedure DrawUnderline(AUnderline: TdxUnderlineDashed; const ABounds: TdxRectF; AColor: TdxAlphaColor); overload;
    procedure DrawUnderline(AUnderline: TdxUnderlineDotted; const ABounds: TdxRectF; AColor: TdxAlphaColor); overload;
    procedure DrawUnderline(AUnderline: TdxUnderlineSingle; const ABounds: TdxRectF; AColor: TdxAlphaColor); overload;
  end;

  { TdxVerticalPrinterCharacterLinePainter }

  TdxVerticalPrinterCharacterLinePainter = class(TdxHorizontalPrinterCharacterLinePainter)
  protected
    function IsHorizontal: Boolean; override;
  end;

  { TdxPrinterPainterWrapper }

  TdxPrinterPainterWrapper = class(TInterfacedObject, IdxPainterWrapper)
  strict private
    FExporter: TdxPrintingDocumentExporter;
    function GetHorizontalLinePainter: IdxCharacterLinePainter;
    function GetVerticalLinePainter: IdxCharacterLinePainter;
  public
    constructor Create(AExporter: TdxPrintingDocumentExporter);
    procedure FillRectangle(AColor: TdxAlphaColor; const ABoundsF: TdxRectF);
    function GetSnappedPoint(const APoint: TdxPointF): TdxPointF;
    procedure SnapWidths(var AWidths: TArray<Single>);
    procedure SnapHeights(var AHeights: TArray<Single>);

    property HorizontalLinePainter: IdxCharacterLinePainter read GetHorizontalLinePainter;
    property VerticalLinePainter: IdxCharacterLinePainter read GetVerticalLinePainter;
  end;

  { TdxPrintingDocumentExporter }

  TdxPrintingDocumentExporter = class(TdxBoundedDocumentLayoutExporter)
  strict private
    FPageCell: TdxOfficeBrick;
    FContainerCell: TdxOfficeBrick;
    FMeasurer: TdxBoxMeasurer;
    FOffset: TPoint;
    FRunningTopPage: Integer;
    FTextBoxOffset: TdxPointF;
    FHorizontalPrinterLinePainter: TdxHorizontalPrinterCharacterLinePainter;
    FVerticalPrinterLinePainter: TdxVerticalPrinterCharacterLinePainter;
    FSuppressPutIntoTableCellBrick: Boolean;
    FStartFromNotFirstPage: Boolean;
  protected
    FLink: TBasedxReportLink;
    FParentCell: TdxReportCell;
    FPrevPageSkipCount: Integer;
    function GetOffset: TPoint; override;
    function GetPainter: TdxPainter; override;
    function CreateHorizontalLinePainter(const ALinePaintingSupport: IdxPatternLinePaintingSupport): TdxRichEditPatternLinePainter; override;
    function CreateVerticalLinePainter(const ALinePaintingSupport: IdxPatternLinePaintingSupport): TdxRichEditPatternLinePainter; override;
    procedure ExportRowBookmarkBoxes; override;
    procedure SetPageBackColor; virtual;
    procedure SetupDefaultPageSettings; virtual;
    function CalculateScaleFactor(APage: TdxPage; const APageSize: TSize): Double;
    function CalculateScaleMargins(AMargins: TdxMargins; AScaleFactor: Double): TdxMargins;
    function CalculateMargins(APage: TdxPage): TdxMargins;
    procedure ExportPageCore(APage: TdxPage; AMargins: TdxMargins; APaperKindInfo: TdxPaperKindInfo;
      const APageSize: TSize; AExportEmptyPage: Boolean; ABounds: TdxNullableValue<TRect>; AScaleFactor: Single; AAction: TdxAction);
    procedure ExportPageBase(APage: TdxPage); virtual;
    function CalculateActualPageRectangle(const ABounds: TRect; ALandscape: Boolean): TRect;
    function CalculateActualPaperKind(APage: TdxPage): TdxPaperKindInfo; overload; virtual;
    function CalculateActualPaperKind(ASectionPage: TdxSectionPage): TdxPaperKindInfo; overload; virtual;
    procedure ExportPictureCore(ABox: TdxBox; AImage: TdxOfficeImage; const ABounds: TRect; ASizeMode: TdxImageSizeMode; ATransparent: Boolean); overload;
    procedure ExportBoxCore(ABox: TdxBox); overload;
    procedure ExportBoxCore(ABox: TdxBox; const AText: string); overload;
    procedure AddBrickToCurrentPage(ABrick: TdxBrick); overload; virtual;
    procedure AddBrickToCurrentPage(ABrick: TdxBrick; ACell: TdxTableCellViewInfo); overload; virtual;
    procedure AddBrickToCurrentContainer(ABrick: TdxBrick); virtual;
    procedure ExportInnerTables(ACell: TdxTableCellViewInfo); virtual;
    procedure ExportTablesBackground(AColumn: TdxColumn); override;
    procedure ExportTopLevelTables(AColumnTables: TdxTableViewInfoCollection);
    function GetBorderPainter(AViewInfo: TdxTableBorderViewInfoBase): TdxTableBorderPainter; overload; virtual;
    function GetBorderPainter(ACorner: TdxCornerViewInfoBase): TdxTableCornerPainter; overload; virtual;

    property PageCell: TdxOfficeBrick read FPageCell;
    property ContainerCell: TdxOfficeBrick read FContainerCell;
    property TextBoxOffset: TdxPointF read FTextBoxOffset write FTextBoxOffset;
    property StartFromNotFirstPage: Boolean read FStartFromNotFirstPage write FStartFromNotFirstPage;

    property HorizontalPrinterLinePainter: TdxHorizontalPrinterCharacterLinePainter read FHorizontalPrinterLinePainter;
    property VerticalPrinterLinePainter: TdxVerticalPrinterCharacterLinePainter read FVerticalPrinterLinePainter;
  public
    constructor Create(ADocumentModel: TdxDocumentModel; ATextColors: TdxTextColors); reintroduce; virtual;
    destructor Destroy; override;

    procedure Export(ADocumentLayout: TdxDocumentLayout; ALink: TBasedxReportLink);

    function CreateRotatedFloatingObjectBoxImage(ABox: TdxBox; ATransform: TdxTransformMatrix): TdxOfficeImage;
    class function GetActualBoxHeight(const AActualBounds: TRect; ARows: TdxRowCollection; ACurrentRow: TdxRow; const ARowBounds: TRect): Integer; static;
    function GetNativeImage(ABox: TdxInlinePictureBox): TdxOfficeImage; overload;
    function GetNativeImage(APictureContent: TdxPictureFloatingObjectContent): TdxOfficeImage; overload;

    procedure DrawParagagraphBackground(const AActualBounds: TRect; ARows: TdxRowCollection);
    procedure DrawParagraphBorders(const AContentBounds: TRect; AParagraphProperties: TdxParagraphProperties);
    procedure DrawParagraphBordersCorners(const AContentBounds: TRect; AParagraphProperties: TdxParagraphProperties);
    procedure DrawParagraphBordersWithCorners(const AActualContentBounds: TRect; ABoxParagraphProperties: TdxParagraphProperties);
    procedure DrawParagraphBordersWithoutTableBounds(const AActualBounds: TRect; ABoxParagraphProperties: TdxParagraphProperties; ARows: TdxRowCollection);
    procedure ExportBookmarkStartBox(ABox: TdxVisitableDocumentIntervalBox); override;
    procedure ExportColumnBreakBox(ABox: TdxColumnBreakBox); override;
    procedure ExportFloatingObjectBox(ABox: TdxFloatingObjectBox); override;
    procedure ExportFloatingObjectPicture(ABox: TdxFloatingObjectBox; APictureContent: TdxPictureFloatingObjectContent); override;
    procedure ExportFloatingObjectShape(ABox: TdxFloatingObjectBox; AShape: TdxShape); override;
    procedure ExportFloatingObjectTextBox(ABox: TdxFloatingObjectBox; ATextBoxContent: TdxTextBoxFloatingObjectContent; ATextBoxDocumentLayout: TdxDocumentLayout); override;
    procedure ExportHighlightArea(const AArea: TdxHighlightArea); override;
    procedure ExportHyphenBox(ABox: TdxHyphenBox); override;
    procedure ExportInlinePictureBox(ABox: TdxInlinePictureBox); override;
    procedure ExportLayoutDependentTextBox(ABox: TdxLayoutDependentTextBox); override;
    procedure ExportLineBoxCore<T>(ALineBox: TdxUnderlineBox; ALine: TdxPatternLine<T>; ABrick: TdxLineBrick; ALineColor: TdxAlphaColor);
    procedure ExportLineBreakBox(ABox: TdxLineBreakBox); override;
    procedure ExportLineNumberBox(ABox: TdxLineNumberBox); override;
    procedure ExportNumberingListBox(ABox: TdxNumberingListBox); override;
    procedure ExportPage(APage: TdxPage); override;
    procedure ExportPageBreakBox(ABox: TdxPageBreakBox); override;
    procedure ExportParagraphFrameBox(ABox: TdxParagraphFrameBox); override;
    procedure ExportParagraphFrameShape(ABox: TdxParagraphFrameBox; AShape: TdxShape); override;
    procedure ExportParagraphFrameTextBox(ABox: TdxParagraphFrameBox; ATextBoxDocumentLayout: TdxDocumentLayout); override;
    procedure ExportParagraphMarkBox(ABox: TdxParagraphMarkBox); override;
    procedure ExportRotatedFloatingObjectBox(ABox: TdxFloatingObjectBox; ATransform: TdxTransformMatrix);
    procedure ExportRotatedParagraphFrameBox(ABox: TdxParagraphFrameBox; ATransform: TdxTransformMatrix);
    procedure ExportRow(ARow: TdxRow); override;
    procedure ExportSectionMarkBox(ABox: TdxSectionMarkBox); override;
    procedure ExportSpaceBox(ABox: TdxBox); override;
    procedure ExportStrikeoutBox(ARow: TdxSimpleRow; AStrikeoutBox: TdxUnderlineBox); override;
    procedure ExportTableBorder(ABorder: TdxTableBorderViewInfoBase; const ACellBounds: TRect); override;
    procedure ExportTableBorderCorner(ACorner: TdxCornerViewInfoBase; X: Integer; Y: Integer); override;
    procedure ExportTableCell(ACell: TdxTableCellViewInfo); override;
    procedure ExportTableRow(ARow: TdxTableRowViewInfoBase); override;
    procedure ExportTabSpaceBox(ABox: TdxTabSpaceBox); override;
    procedure ExportTextBox(ABox: TdxTextBox); override;
    procedure ExportUnderlineBox(ARow: TdxSimpleRow; AUnderlineBox: TdxUnderlineBox); override;
    procedure FillRectangle(AFillColor: TdxAlphaColor; const AActualContentBounds: TRect);
    procedure PrintRectangle(AFillColor: TdxAlphaColor; const ABounds: TRect);

    function CorrectTextDrawingBounds(ABox: TdxBox; const ATextBounds: TRect): TRect;
    function GetDrawingBounds(const ABounds: TRect): TRect; override;
  end;

implementation

uses
  Math, Dialogs, ShellAPI, ComObj, ShlObj, UxTheme, DwmApi, dxTypeHelpers, dxCore,

  dxRichEdit.LayoutEngine.BoxMeasurer,
  dxRichEdit.Utils.Mouse,
  dxRichEdit.Platform.Font,
  dxRichEdit.Platform.Win.Font,
  dxRichEdit.Platform.Win.FontCache,
  dxRichEdit.DocumentModel.Borders,
  dxRichEdit.DocumentModel.DocumentProperties,
  dxRichEdit.DocumentModel.Fields,
  dxRichEdit.DocumentModel.UnitConverter,
  dxRichEdit.DocumentModel.UnitToLayoutUnitConverter,
  dxRichEdit.Options,
  dxPSRichEditControlLnk, dxBkGnd;

type
  TdxPrintingDocumentExporterHelper = class helper for TdxPrintingDocumentExporter
  private
    function GetReportLink: TdxRichEditControlReportLink;
  public
    property ReportLink: TdxRichEditControlReportLink read GetReportLink;
  end;

  TdxRichEditControlReportLinkHelper = class helper for TdxRichEditControlReportLink
  public
    function GetFontSize(AModelSize: Single): Integer;
  end;

{ TdxPrintingDocumentExporterHelper }

function TdxPrintingDocumentExporterHelper.GetReportLink: TdxRichEditControlReportLink;
begin
  Result := TdxRichEditControlReportLink(FLink);
end;

{ TdxRichEditControlReportLinkHelper }

function TdxRichEditControlReportLinkHelper.GetFontSize(AModelSize: Single): Integer;
begin
  Result := Round(AModelSize * Printable.FontScaleFactor);
end;

{ TdxPaperKindInfo }

constructor TdxPaperKindInfo.Create(APaperKind: TdxPaperKind; ALandscape: Boolean);
begin
  Landscape := ALandscape;
  PaperKind := APaperKind;
end;

class function TdxPaperKindInfo.Default: TdxPaperKindInfo;
begin
  Result := TdxPaperKindInfo.Create(TdxPaperKind.Custom, False);
end;

{ TdxEmptyPatternLinePaintingSupport }

procedure TdxEmptyPatternLinePaintingSupport.DrawLine(APen: TdxGPPen; AX1: Single; AY1: Single; AX2: Single; AY2: Single);
begin
end;

procedure TdxEmptyPatternLinePaintingSupport.DrawLines(APen: TdxGPPen; const APoints: TArray<TdxPointF>);
begin
end;

function TdxEmptyPatternLinePaintingSupport.GetBrush(AColor: TdxAlphaColor): TdxGPBrush;
begin
  Result := TdxGPBrush.Create;
  Result.Color := AColor;
end;

function TdxEmptyPatternLinePaintingSupport.GetPen(AColor: TdxAlphaColor; AThickness: Single): TdxGPColorPen;
begin
  Result := TdxGPColorPen.Create(AColor, AThickness);
end;

function TdxEmptyPatternLinePaintingSupport.GetPen(AColor: TdxAlphaColor): TdxGPColorPen;
begin
  Result := TdxGPColorPen.Create(AColor);
end;

procedure TdxEmptyPatternLinePaintingSupport.ReleaseBrush(ABrush: TdxGPBrush);
begin
  ABrush.Free;
end;

procedure TdxEmptyPatternLinePaintingSupport.ReleasePen(APen: TdxGPPen);
begin
  APen.Free;
end;

{ TdxVerticalPrinterCharacterLinePainter }

function TdxVerticalPrinterCharacterLinePainter.IsHorizontal: Boolean;
begin
  Result := False;
end;

procedure TdxHorizontalPrinterCharacterLinePainter.DrawUnderline(AUnderline: TdxUnderlineSingle;
  const ABounds: TdxRectF; AColor: TdxAlphaColor);
begin
  DrawCore(AUnderline, ABounds, AColor);
end;

procedure TdxHorizontalPrinterCharacterLinePainter.DrawUnderline(AUnderline: TdxUnderlineDotted;
  const ABounds: TdxRectF; AColor: TdxAlphaColor);
begin
  DrawCore(AUnderline, ABounds, AColor);
end;

procedure TdxHorizontalPrinterCharacterLinePainter.DrawUnderline(AUnderline: TdxUnderlineDashed;
  const ABounds: TdxRectF; AColor: TdxAlphaColor);
begin
  DrawCore(AUnderline, ABounds, AColor);
end;

procedure TdxHorizontalPrinterCharacterLinePainter.DrawUnderline(AUnderline: TdxUnderlineDashSmallGap;
  const ABounds: TdxRectF; AColor: TdxAlphaColor);
begin
  DrawCore(AUnderline, ABounds, AColor);
end;

procedure TdxHorizontalPrinterCharacterLinePainter.DrawUnderline(AUnderline: TdxUnderlineDashDotted;
  const ABounds: TdxRectF; AColor: TdxAlphaColor);
begin
  DrawCore(AUnderline, ABounds, AColor);
end;

procedure TdxHorizontalPrinterCharacterLinePainter.DrawUnderline(AUnderline: TdxUnderlineDashDotDotted;
  const ABounds: TdxRectF; AColor: TdxAlphaColor);
begin
  DrawCore(AUnderline, ABounds, AColor);
end;

procedure TdxHorizontalPrinterCharacterLinePainter.DrawUnderline(AUnderline: TdxUnderlineDouble;
  const ABounds: TdxRectF; AColor: TdxAlphaColor);
begin
  DrawCore(AUnderline, ABounds, AColor);
end;

procedure TdxHorizontalPrinterCharacterLinePainter.DrawUnderline(AUnderline: TdxUnderlineHeavyWave;
  const ABounds: TdxRectF; AColor: TdxAlphaColor);
begin
  DrawCore(AUnderline, ABounds, AColor);
end;

procedure TdxHorizontalPrinterCharacterLinePainter.DrawUnderline(AUnderline: TdxUnderlineLongDashed;
  const ABounds: TdxRectF; AColor: TdxAlphaColor);
begin
  DrawCore(AUnderline, ABounds, AColor);
end;

procedure TdxHorizontalPrinterCharacterLinePainter.DrawUnderline(AUnderline: TdxUnderlineThickSingle;
  const ABounds: TdxRectF; AColor: TdxAlphaColor);
begin
  DrawCore(AUnderline, ABounds, AColor);
end;

procedure TdxHorizontalPrinterCharacterLinePainter.DrawUnderline(AUnderline: TdxUnderlineThickDotted;
  const ABounds: TdxRectF; AColor: TdxAlphaColor);
begin
  DrawCore(AUnderline, ABounds, AColor);
end;

procedure TdxHorizontalPrinterCharacterLinePainter.DrawUnderline(AUnderline: TdxUnderlineThickDashed;
  const ABounds: TdxRectF; AColor: TdxAlphaColor);
begin
  DrawCore(AUnderline, ABounds, AColor);
end;

procedure TdxHorizontalPrinterCharacterLinePainter.DrawUnderline(AUnderline: TdxUnderlineThickDashDotted;
  const ABounds: TdxRectF; AColor: TdxAlphaColor);
begin
  DrawCore(AUnderline, ABounds, AColor);
end;

procedure TdxHorizontalPrinterCharacterLinePainter.DrawUnderline(AUnderline: TdxUnderlineThickDashDotDotted;
  const ABounds: TdxRectF; AColor: TdxAlphaColor);
begin
  DrawCore(AUnderline, ABounds, AColor);
end;

procedure TdxHorizontalPrinterCharacterLinePainter.DrawUnderline(AUnderline: TdxUnderlineThickLongDashed;
  const ABounds: TdxRectF; AColor: TdxAlphaColor);
begin
  DrawCore(AUnderline, ABounds, AColor);
end;

procedure TdxHorizontalPrinterCharacterLinePainter.DrawUnderline(AUnderline: TdxUnderlineDoubleWave;
  const ABounds: TdxRectF; AColor: TdxAlphaColor);
begin
  DrawCore(AUnderline, ABounds, AColor);
end;

procedure TdxHorizontalPrinterCharacterLinePainter.DrawUnderline(AUnderline: TdxUnderlineWave;
  const ABounds: TdxRectF; AColor: TdxAlphaColor);
begin
  DrawCore(AUnderline, ABounds, AColor);
end;

procedure TdxHorizontalPrinterCharacterLinePainter.DrawStrikeout(AUnderline: TdxStrikeoutSingle;
  const ABounds: TdxRectF; AColor: TdxAlphaColor);
begin
end;

procedure TdxHorizontalPrinterCharacterLinePainter.DrawStrikeout(AUnderline: TdxStrikeoutDouble;
  const ABounds: TdxRectF; AColor: TdxAlphaColor);
begin
end;

procedure TdxHorizontalPrinterCharacterLinePainter.DrawCore(ALine: TdxUnderline; const ABoundsF: TdxRectF;
  AColor: TdxAlphaColor);
var
  ABrick: TdxLineBrick;
begin
  ABrick := TdxLineBrick.CreateUnderlineBrick(Exporter.ContainerCell, ALine.Id, IsHorizontal);
  ABrick.BoundsRect := TRect.Round(ABoundsF);
  ABrick.Color := TdxAlphaColors.ToColor(AColor);
end;

function TdxHorizontalPrinterCharacterLinePainter.IsHorizontal: Boolean;
begin
  Result := True;
end;

function TdxHorizontalPrinterCharacterLinePainter.GetDocumentModel: TdxDocumentModel;
begin
  Result := Exporter.DocumentModel;
end;

{ TdxHorizontalPrinterCharacterLinePainter }

constructor TdxHorizontalPrinterCharacterLinePainter.Create(AExporter: TdxPrintingDocumentExporter);
begin
  inherited Create;
  FExporter := AExporter;
end;

{ TdxPrinterPainterWrapper }

constructor TdxPrinterPainterWrapper.Create(AExporter: TdxPrintingDocumentExporter);
begin
  inherited Create;
  Assert(AExporter <> nil);
  FExporter := AExporter;
end;

function TdxPrinterPainterWrapper.GetHorizontalLinePainter: IdxCharacterLinePainter;
begin
  Result := FExporter.HorizontalPrinterLinePainter;
end;

function TdxPrinterPainterWrapper.GetVerticalLinePainter: IdxCharacterLinePainter;
begin
  Result := FExporter.VerticalPrinterLinePainter;
end;

procedure TdxPrinterPainterWrapper.FillRectangle(AColor: TdxAlphaColor; const ABoundsF: TdxRectF);
var
  ABrick: TdxOfficeRectBrick;
begin
  ABrick := TdxOfficeRectBrick.Create(FExporter.ContainerCell);
  ABrick.BoundsRect := TRect.Round(ABoundsF);
  ABrick.Color := TdxAlphaColors.ToColor(AColor);
end;

function TdxPrinterPainterWrapper.GetSnappedPoint(const APoint: TdxPointF): TdxPointF;
begin
  Result := TdxPointF.Create(Trunc(APoint.X), Trunc(APoint.Y));
end;

procedure TdxPrinterPainterWrapper.SnapWidths(var AWidths: TArray<Single>);
begin
end;

procedure TdxPrinterPainterWrapper.SnapHeights(var AHeights: TArray<Single>);
begin
end;

{ TdxPrintingDocumentExporter }

constructor TdxPrintingDocumentExporter.Create(ADocumentModel: TdxDocumentModel; ATextColors: TdxTextColors);
begin
  inherited Create(ADocumentModel, TRect.Null,
    (TdxEmptyPatternLinePaintingSupport.Create as IdxPatternLinePaintingSupport), ATextColors);
  FStartFromNotFirstPage := False;
  FHorizontalPrinterLinePainter := TdxHorizontalPrinterCharacterLinePainter.Create(Self);
  FVerticalPrinterLinePainter := TdxVerticalPrinterCharacterLinePainter.Create(Self);
end;

destructor TdxPrintingDocumentExporter.Destroy;
begin
  FHorizontalPrinterLinePainter.Free;
  FVerticalPrinterLinePainter.Free;
  inherited Destroy;
end;

function TdxPrintingDocumentExporter.GetOffset: TPoint;
begin
  Result := FOffset;
end;

function TdxPrintingDocumentExporter.GetPainter: TdxPainter;
begin
  raise ENotImplemented.Create('Painter');
end;

function TdxPrintingDocumentExporter.CreateHorizontalLinePainter(
  const ALinePaintingSupport: IdxPatternLinePaintingSupport): TdxRichEditPatternLinePainter;
begin
  Result := nil;
end;

function TdxPrintingDocumentExporter.CreateVerticalLinePainter(
  const ALinePaintingSupport: IdxPatternLinePaintingSupport): TdxRichEditPatternLinePainter;
begin
  Result := nil;
end;

procedure TdxPrintingDocumentExporter.ExportRowBookmarkBoxes;
var
  ABoxes: TdxVisitableDocumentIntervalBoxCollection;
  ALastIndex, I: Integer;
begin
  ABoxes := CurrentRow.InnerBookmarkBoxes;
  if ABoxes = nil then
    Exit;
  ALastIndex := ABoxes.Count - 1;
  for I := 0 to ALastIndex do
    ABoxes[I].ExportTo(Self);
end;

procedure TdxPrintingDocumentExporter.ExportBookmarkStartBox(ABox: TdxVisitableDocumentIntervalBox);
begin
end;

procedure TdxPrintingDocumentExporter.Export(ADocumentLayout: TdxDocumentLayout; ALink: TBasedxReportLink);
var
  ACurrentPage, APageCount: Integer;
begin
  FLink := ALink;
  FMeasurer := ADocumentLayout.Measurer;
  SetPageBackColor;
  SetupDefaultPageSettings;
  FRunningTopPage := 0;

  APageCount := ADocumentLayout.Pages.Count;
  for ACurrentPage := 0 to APageCount - 1 do
  begin
    ADocumentLayout.Pages[ACurrentPage].ExportTo(Self);
    ALink.ReportCells.DoProgress((ACurrentPage + 1) div APageCount);
  end;
end;

procedure TdxPrintingDocumentExporter.SetPageBackColor;
var
  ABackground: TdxBackground;
begin
  ABackground := ReportLink.PrinterPage.Background;
  if ReportLink.IsApplyBackgroundToEntirePage then
  begin
    ABackground.Mode := bmBrush;
    ABackground.Brush.Color := TdxAlphaColors.ToColor(DocumentModel.DocumentProperties.PageBackColor);
  end
  else
  begin
    ABackground.Mode := bmNone;
    ABackground.Brush.Color := TdxAlphaColors.ToColor(TdxAlphaColors.White);
  end
end;

procedure TdxPrintingDocumentExporter.SetupDefaultPageSettings;
var
  AUnitConverter: TdxDocumentModelUnitConverter;
  ASection: TdxSection;
  ASectionMargins: TdxSectionMargins;
  AMargins: TdxMargins;
  ASectionPage: TdxSectionPage;
  APageSize: TSize;
  ABounds: TRect;
  APaperKindInfo: TdxPaperKindInfo;
begin
  AUnitConverter := DocumentModel.UnitConverter;
  ASection := DocumentModel.Sections.First;

  ASectionMargins := ASection.Margins;
  AMargins := TdxMargins.Create(AUnitConverter.ModelUnitsToHundredthsOfInch(ASectionMargins.Left),
    AUnitConverter.ModelUnitsToHundredthsOfInch(ASectionMargins.Right),
    AUnitConverter.ModelUnitsToHundredthsOfInch(Abs(ASectionMargins.Top)),
    AUnitConverter.ModelUnitsToHundredthsOfInch(Abs(ASectionMargins.Bottom)));

  ASectionPage := ASection.Page;
  APageSize := TSize.Create(ASectionPage.Width, ASectionPage.Height);
  APageSize := AUnitConverter.ModelUnitsToHundredthsOfInch(APageSize);

  APaperKindInfo := CalculateActualPaperKind(ASectionPage);
  ABounds := CalculateActualPageRectangle(TRect.CreateSize(0, 0, APageSize.Width, APageSize.Height), APaperKindInfo.Landscape);
end;

function TdxPrintingDocumentExporter.CalculateScaleFactor(APage: TdxPage; const APageSize: TSize): Double;
begin
  Result := 1;
end;

function TdxPrintingDocumentExporter.CalculateScaleMargins(AMargins: TdxMargins; AScaleFactor: Double): TdxMargins;
begin
  Result := TdxMargins.Create(Trunc(AMargins.Left * AScaleFactor), Trunc(AMargins.Right * AScaleFactor),
    Trunc(AMargins.Top * AScaleFactor), Trunc(AMargins.Bottom * AScaleFactor));
end;

function TdxPrintingDocumentExporter.CalculateMargins(APage: TdxPage): TdxMargins;
var
  X, AOriginalPageBounds, AOriginalPageClientBounds: TRect;
begin
  AOriginalPageBounds := APage.Bounds;
  AOriginalPageClientBounds := APage.ClientBounds;

  X.Left := DocumentModel.LayoutUnitConverter.TwipsToLayoutUnits(AOriginalPageClientBounds.Left - AOriginalPageBounds.Left);
  X.Top := DocumentModel.LayoutUnitConverter.TwipsToLayoutUnits(AOriginalPageClientBounds.Top - AOriginalPageBounds.Top);
  X.Right := DocumentModel.LayoutUnitConverter.TwipsToLayoutUnits(AOriginalPageBounds.Right - AOriginalPageClientBounds.Right);
  X.Bottom := DocumentModel.LayoutUnitConverter.TwipsToLayoutUnits(AOriginalPageBounds.Bottom - AOriginalPageClientBounds.Bottom);

  Result := TdxMargins.Create(X.Left, X.Top, X.Right, X.Bottom);
end;

procedure TdxPrintingDocumentExporter.ExportPage(APage: TdxPage);
var
  APaperKindInfo: TdxPaperKindInfo;
  I: Integer;
  AMargins: TdxMargins;
  AScaleFactor: Single;
begin
  FPageCell := ReportLink.AddPage(APage);
  FContainerCell := FPageCell;

  APaperKindInfo := CalculateActualPaperKind(APage);

  for I := 0 to APage.NumSkippedPages - FPrevPageSkipCount do
  begin
    Bounds := FPageCell.BoundsRect;

    FOffset := TPoint.Create(
      -MulDiv(APage.ClientBounds.X, Round(DocumentModel.LayoutUnitConverter.Dpi), 1440),
      -MulDiv(APage.ClientBounds.Y, Round(DocumentModel.LayoutUnitConverter.Dpi), 1440));

    AScaleFactor := 1.0;
    AMargins := CalculateMargins(APage {, AScaleFactor});
    ExportPageCore(APage, AMargins, APaperKindInfo,
      DocumentModel.LayoutUnitConverter.LayoutUnitsToHundredthsOfInch(FPageCell.BoundsRect.Size),
      I < APage.NumSkippedPages - FPrevPageSkipCount, TRect.Null, AScaleFactor,
      procedure ()
      begin
        ExportPageBase(APage);
      end);
  end;

  FRunningTopPage := FPageCell.BoundsRect.Bottom;
end;

procedure TdxPrintingDocumentExporter.ExportPageCore(APage: TdxPage; AMargins: TdxMargins;
  APaperKindInfo: TdxPaperKindInfo; const APageSize: TSize; AExportEmptyPage: Boolean; ABounds: TdxNullableValue<TRect>;
  AScaleFactor: Single; AAction: TdxAction);
begin
    if not AExportEmptyPage then
//      ExportPageBase(APage);
      AAction;
end;

procedure TdxPrintingDocumentExporter.ExportRow(ARow: TdxRow);
var
  ACell: TdxTableCellViewInfo;
  AClipBounds: TRect;
  APrevContainerCell: TdxOfficeBrick;
begin
  if ARow is TdxTableCellRow then
  begin
    APrevContainerCell := FContainerCell;
    try
      FContainerCell := TdxOfficeBrick.Create(APrevContainerCell);
      FContainerCell.BoundsRect := APrevContainerCell.BoundsRect;

      inherited ExportRow(ARow);

      if FContainerCell.DataItemCount > 0 then
      begin
        ACell := TdxTableCellRow(ARow).CellViewInfo;
        AClipBounds := ACell.TableViewInfo.GetCellBounds(ACell);
        FContainerCell.Offset(-AClipBounds.Left, -AClipBounds.Top);
        FContainerCell.BoundsRect := AClipBounds;
        FContainerCell.ClipChildren := True;
      end;
    finally
      FContainerCell := APrevContainerCell;
    end;
  end
  else
    inherited ExportRow(ARow);
end;

procedure TdxPrintingDocumentExporter.ExportPageBase(APage: TdxPage);
begin
  inherited ExportPage(APage);
end;


function TdxPrintingDocumentExporter.CalculateActualPaperKind(APage: TdxPage): TdxPaperKindInfo;
var
  APageArea: TdxPageArea;
  AColumn: TdxColumn;
  ARow: TdxRow;
  AParagraph: TdxCustomParagraph;
  AParagraphIndex: TdxParagraphIndex;
  ASectionIndex: TdxSectionIndex;
  ASection: TdxSection;
  ASectionPage: TdxSectionPage;
begin
  APageArea := APage.Areas.First;
  if APageArea = nil then
    Exit(TdxPaperKindInfo.Default);

  AColumn := APageArea.Columns.First;
  if AColumn = nil then
    Exit(TdxPaperKindInfo.Default);

  ARow := AColumn.Rows.First;
  if ARow = nil then
    Exit(TdxPaperKindInfo.Default);

  AParagraph := ARow.Paragraph;
  if AParagraph = nil then
    Exit(TdxPaperKindInfo.Default);

  AParagraphIndex := AParagraph.Index;
  ASectionIndex := DocumentModel.MainPieceTable.LookupSectionIndexByParagraphIndex(AParagraphIndex);
  ASection := DocumentModel.Sections[ASectionIndex];

  ASectionPage := ASection.Page;
  Result := CalculateActualPaperKind(ASectionPage);
end;

function TdxPrintingDocumentExporter.CalculateActualPaperKind(ASectionPage: TdxSectionPage): TdxPaperKindInfo;

  function IfThen(AValue: Boolean; const ATrue, AFalse: Boolean): Boolean;
  begin
    if AValue then
      Result := ATrue
    else
      Result := AFalse;
  end;

var
  APaperKind: TdxPaperKind;
begin
  APaperKind := ASectionPage.PaperKind;
  if APaperKind <> TdxPaperKind.Custom then
    Result := TdxPaperKindInfo.Create(APaperKind, ASectionPage.Landscape)
  else
    Result := TdxPaperKindInfo.Create(APaperKind, IfThen(ASectionPage.Width > ASectionPage.Height, ASectionPage.Landscape, False));
end;

function TdxPrintingDocumentExporter.CalculateActualPageRectangle(const ABounds: TRect; ALandscape: Boolean): TRect;
begin
  if ALandscape then
    Result := cxRectRotate(ABounds)
  else
    Result := ABounds;
end;

procedure TdxPrintingDocumentExporter.ExportInlinePictureBox(ABox: TdxInlinePictureBox);
begin
  ExportPictureCore(ABox, GetNativeImage(ABox), ABox.Bounds, ABox.GetSizing(PieceTable), False);
end;

function TdxPrintingDocumentExporter.GetNativeImage(ABox: TdxInlinePictureBox): TdxOfficeImage;
begin
  Result := ABox.GetImage(PieceTable, ReadOnly);
  Result.EnsureLoadComplete;
end;

function TdxPrintingDocumentExporter.GetNativeImage(APictureContent: TdxPictureFloatingObjectContent): TdxOfficeImage;
begin
  Result := APictureContent.Image.Image;
  Result.EnsureLoadComplete;
end;

procedure TdxPrintingDocumentExporter.ExportPictureCore(ABox: TdxBox;
  AImage: TdxOfficeImage; const ABounds: TRect; ASizeMode: TdxImageSizeMode; ATransparent: Boolean);
var
  ABrick: TdxOfficeImageBrick;
begin
  ABrick := TdxOfficeImageBrick.Create(ContainerCell);
  ABrick.BoundsRect := ABounds;
  ABrick.Image := AImage;
  ABrick.Color := clWhite;
end;

procedure TdxPrintingDocumentExporter.ExportFloatingObjectBox(ABox: TdxFloatingObjectBox);
var
  ATransform: TdxTransformMatrix;
begin
  ATransform := ABox.CreateBackwardTransformUnsafe;
  if ATransform <> nil then
  begin
    ExportRotatedFloatingObjectBox(ABox, ATransform);
    ExportNotRotatedContent(ABox);
  end
  else
    inherited ExportFloatingObjectBox(ABox);
end;

procedure TdxPrintingDocumentExporter.ExportParagraphFrameBox(ABox: TdxParagraphFrameBox);
var
  ATransform: TdxTransformMatrix;
begin
  ATransform := ABox.CreateBackwardTransformUnsafe;
  if ATransform <> nil then
  begin
    ExportRotatedParagraphFrameBox(ABox, ATransform);
    ExportNotRotatedContent(ABox);
  end
  else
    inherited ExportParagraphFrameBox(ABox);
end;

procedure TdxPrintingDocumentExporter.ExportRotatedFloatingObjectBox(ABox: TdxFloatingObjectBox; ATransform: TdxTransformMatrix);
var
  AImage: TdxOfficeImage;
  ABounds: TRect;
begin
  AImage := CreateRotatedFloatingObjectBoxImage(ABox, ATransform);
  try
    ABounds := GetDrawingBounds(TdxRectangleUtils.BoundingRectangle(ABox.Bounds, ATransform));
    ExportPictureCore(ABox, AImage, ABounds, TdxImageSizeMode.StretchImage, True);
  finally
    AImage.Free;
  end;
end;

procedure TdxPrintingDocumentExporter.ExportRotatedParagraphFrameBox(ABox: TdxParagraphFrameBox; ATransform: TdxTransformMatrix);
var
  AImage: TdxOfficeImage;
  ABounds: TRect;
begin
  AImage := CreateRotatedFloatingObjectBoxImage(ABox, ATransform);
  ABounds := GetDrawingBounds(TdxRectangleUtils.BoundingRectangle(ABox.Bounds, ATransform));
  ExportPictureCore(ABox, AImage, ABounds, TdxImageSizeMode.StretchImage, True);
end;

function TdxPrintingDocumentExporter.CreateRotatedFloatingObjectBoxImage(ABox: TdxBox; ATransform: TdxTransformMatrix): TdxOfficeImage;
var
  ABoundingRect, ABounds: TRect;
  AExporter: TdxGraphicsDocumentLayoutExporter;
  AFloatingObjectBox: TdxFloatingObjectBox;
  AParagraphFrameBox: TdxParagraphFrameBox;
  AGraphics: TdxGraphics;
  APainter: TdxGdiPainter;
begin
  ABoundingRect := GetDrawingBounds(TdxRectangleUtils.BoundingRectangle(ABox.Bounds, ATransform));

  Result := TdxOfficeImage.CreateSize(ABoundingRect.Width, ABoundingRect.Height);
  AGraphics := TdxGraphics.CreateFromImage(Result);
  try
    APainter := TdxGdiPainter.Create(AGraphics, DocumentModel.LayoutUnitConverter);
    try
      AExporter := TdxGraphicsDocumentLayoutExporter.Create(DocumentModel, APainter, TdxWinFormsGraphicsDocumentLayoutExporterAdapter.Create, Bounds, TextColors);
      try
        AExporter.PieceTable := PieceTable;
        ABounds := AExporter.GetDrawingBounds(ABox.Bounds);
        AGraphics.TranslateWorldTransform(-ABounds.X + (ABoundingRect.Width - ABounds.Width) / 2, -ABounds.Y + (ABoundingRect.Height - ABounds.Height) / 2);
        AFloatingObjectBox := Safe<TdxFloatingObjectBox>.Cast(ABox);
        if AFloatingObjectBox <> nil then
          AExporter.ExportRotatedContent(AFloatingObjectBox);
        AParagraphFrameBox := Safe<TdxParagraphFrameBox>.Cast(ABox);
        if AParagraphFrameBox <> nil then
          AExporter.ExportRotatedContent(AParagraphFrameBox);
      finally
        AExporter.Free;
      end;
    finally
      APainter.Free;
    end;
  finally
    AGraphics.Free;
  end;
end;

procedure TdxPrintingDocumentExporter.ExportFloatingObjectPicture(ABox: TdxFloatingObjectBox;
  APictureContent: TdxPictureFloatingObjectContent);
var
  ABrick: TdxOfficeImageBrick;
  AImage: TdxOfficeImage;
  ABounds: TRect;
begin
  ABounds := GetDrawingBounds(ABox.Bounds);
  AImage := GetNativeImage(APictureContent);
  AImage.EnsureLoadComplete;
  ABrick := TdxOfficeImageBrick.Create(ContainerCell);
  ABrick.BoundsRect := ABounds;
  ABrick.Image := AImage;
end;

procedure TdxPrintingDocumentExporter.ExportFloatingObjectTextBox(ABox: TdxFloatingObjectBox;
  ATextBoxContent: TdxTextBoxFloatingObjectContent; ATextBoxDocumentLayout: TdxDocumentLayout);
var
  AOldPieceTable: TdxPieceTable;
  APage: TdxPage;
  ASectionPage: TdxSectionPage;
  APaperKind: TdxPaperKind;
  APaperKindInfo: TdxPaperKindInfo;
  AMargins: TdxMargins;
  APageBounds: TRect;
  APageSize: TSize;
  ALandscape: Boolean;
begin
  AOldPieceTable := PieceTable;
  PieceTable := TdxPieceTable(ATextBoxContent.TextBox.PieceTable);
  try
    APage := ATextBoxDocumentLayout.Pages.First;
    ASectionPage := DocumentModel.Sections.First.Page;

    APaperKind := ASectionPage.PaperKind;
    if APaperKind <> TdxPaperKind.Custom then
      ALandscape := ASectionPage.Landscape
    else
      ALandscape := False;

    APaperKindInfo := TdxPaperKindInfo.Create(APaperKind, ALandscape);
    AMargins := CalculateMargins(APage);
    APageBounds := CalculateActualPageRectangle(APage.Bounds, APaperKindInfo.Landscape);
    APageSize := DocumentModel.LayoutUnitConverter.LayoutUnitsToHundredthsOfInch(APageBounds.Size);
    ExportPageCore(APage, AMargins, APaperKindInfo, APageSize, False, ABox.Bounds, 1,
      procedure ()
      begin
        ExportPageBase(APage);
      end);
    TextBoxOffset := FContainerCell.AbsoluteOrigin.ToPointF;
  finally
    PieceTable := AOldPieceTable;
  end;
end;

procedure TdxPrintingDocumentExporter.ExportParagraphFrameTextBox(ABox: TdxParagraphFrameBox;
  ATextBoxDocumentLayout: TdxDocumentLayout);
var
  AOldPieceTable: TdxPieceTable;
  APage: TdxPage;
  ASectionPage: TdxSectionPage;
  APaperKind: TdxPaperKind;
  APaperKindInfo: TdxPaperKindInfo;
  AMargins: TdxMargins;
  APageBounds: TRect;
  APageSize: TSize;
  ALandscape: Boolean;
begin
  AOldPieceTable := PieceTable;
  PieceTable := TdxPieceTable(ABox.PieceTable);
  try
    APage := ATextBoxDocumentLayout.Pages.First;
    ASectionPage := DocumentModel.Sections.First.Page;

    APaperKind := ASectionPage.PaperKind;
    if APaperKind <> TdxPaperKind.Custom then
      ALandscape := ASectionPage.Landscape
    else
      ALandscape := False;
    APaperKindInfo := TdxPaperKindInfo.Create(APaperKind, ALandscape);
    AMargins := CalculateMargins(APage);
    APageBounds := CalculateActualPageRectangle(APage.Bounds, APaperKindInfo.Landscape);
    APageSize := DocumentModel.LayoutUnitConverter.LayoutUnitsToHundredthsOfInch(APageBounds.Size);
    ExportPageCore(APage, AMargins, APaperKindInfo, APageSize, False, ABox.Bounds, 1,
      procedure()
      begin
        ExportPageBase(APage);
      end);
    TextBoxOffset := FContainerCell.AbsoluteOrigin.ToPointF;
  finally
    PieceTable := AOldPieceTable;
  end;
end;

procedure TdxPrintingDocumentExporter.PrintRectangle(AFillColor: TdxAlphaColor; const ABounds: TRect);
var
  ABrick: TdxOfficeRectBrick;
begin
  ABrick := TdxOfficeRectBrick.Create(ContainerCell);
  ABrick.Color := TdxAlphaColors.ToColor(AFillColor);
  ABrick.BoundsRect := ABounds;
end;

procedure TdxPrintingDocumentExporter.ExportFloatingObjectShape(ABox: TdxFloatingObjectBox; AShape: TdxShape);
var
  AContentBounds, AShapeBounds, ABounds: TRect;
  ARun: TdxFloatingObjectAnchorRun;
  AFillColor, AOutlineColor: TdxAlphaColor;
  APenWidth: Integer;
begin
  AContentBounds := ABox.ContentBounds;

  ARun := ABox.GetFloatingObjectRun;
  AFillColor := ARun.Shape.FillColor;
  if not TdxAlphaColors.IsTransparentOrEmpty(AFillColor) then
    PrintRectangle(AFillColor, AContentBounds);
  APenWidth := GetShapeOutlinePenWidth(ARun, ABox);
  if APenWidth >= 0 then
  begin
    AOutlineColor := ARun.Shape.OutlineColor;
    AShapeBounds := GetDrawingBounds(ABox.Bounds);

    ABounds := TRect.CreateSize(AShapeBounds.X, AShapeBounds.Y, AContentBounds.X - AShapeBounds.X, AShapeBounds.Height);
    if (ABounds.Width = 0) and (APenWidth >= 0) then
      ABounds.Width := 1;
    PrintRectangle(AOutlineColor, ABounds);
    ABounds := TRect.CreateSize(AContentBounds.X, AShapeBounds.Y, AContentBounds.Width, AContentBounds.Y - AShapeBounds.Y);
    if (ABounds.Height = 0) and (APenWidth >= 0) then
      ABounds.Height := 1;
    PrintRectangle(AOutlineColor, ABounds);
    ABounds := TRect.CreateSize(AContentBounds.Right, AShapeBounds.Y, AShapeBounds.Right - AContentBounds.Right, AShapeBounds.Height);
    if (ABounds.Width = 0) and (APenWidth >= 0) then
      ABounds.Width := 1;
    PrintRectangle(AOutlineColor, ABounds);
    ABounds := TRect.CreateSize(AContentBounds.X, AContentBounds.Bottom, AContentBounds.Width, AShapeBounds.Bottom - AContentBounds.Bottom);
    if (ABounds.Height = 0) and (APenWidth >= 0) then
      ABounds.Height := 1;
    PrintRectangle(AOutlineColor, ABounds);
  end;
end;

procedure TdxPrintingDocumentExporter.ExportParagraphFrameShape(ABox: TdxParagraphFrameBox; AShape: TdxShape);
var
  AContentBounds, AParagraphBackgroundBounds, AActualBounds: TRect;
  ABackColor: TdxAlphaColor;
  ABoxParagraphProperties: TdxParagraphProperties;
  ARows: TdxRowCollection;
  AIsContainsTable: Boolean;
  ABoxParagraph: TdxParagraph;
begin
  AContentBounds := ABox.ContentBounds;
  ABoxParagraph := TdxParagraph(ABox.GetParagraph);
  if not ABox.HasFrameProperties or (ABox.DocumentLayout = nil) then
  begin
    ABackColor := ABoxParagraph.BackColor;
    SetBackColor(ABackColor, AContentBounds);
    PrintRectangle(ABackColor, ABox.Bounds);
    Exit;
  end;

  AParagraphBackgroundBounds := GetDrawingBounds(ABox.ActualSizeBounds);
  AActualBounds := ABox.ActualSizeBounds;
  ABoxParagraphProperties := ABoxParagraph.ParagraphProperties;
  ARows := ABox.DocumentLayout.Pages.Last.Areas.Last.Columns.Last.Rows;
  AIsContainsTable := ABox.DocumentLayout.Pages.Last.Areas.Last.Columns.Last.Tables.Count > 0;

  DrawParagagraphBackground(AParagraphBackgroundBounds, ARows);

  if AIsContainsTable then
    DrawParagraphBordersWithoutTableBounds(AActualBounds, ABoxParagraphProperties, ARows)
  else
    DrawParagraphBordersWithCorners(AActualBounds, ABoxParagraphProperties);
end;

procedure TdxPrintingDocumentExporter.FillRectangle(AFillColor: TdxAlphaColor; const AActualContentBounds: TRect);
begin
  PrintRectangle(AFillColor, AActualContentBounds);
end;

procedure TdxPrintingDocumentExporter.DrawParagraphBordersWithCorners(const AActualContentBounds: TRect; ABoxParagraphProperties: TdxParagraphProperties);
begin
  DrawParagraphBorders(AActualContentBounds, ABoxParagraphProperties);
  DrawParagraphBordersCorners(AActualContentBounds, ABoxParagraphProperties);
end;

class function TdxPrintingDocumentExporter.GetActualBoxHeight(const AActualBounds: TRect; ARows: TdxRowCollection; ACurrentRow: TdxRow; const ARowBounds: TRect): Integer;
begin
  if (ACurrentRow = ARows.First) and (ACurrentRow = ARows.Last) then
    Exit(AActualBounds.Height);

  if ACurrentRow = ARows.First then
    Exit(ARowBounds.Height + ARowBounds.Y - AActualBounds.Y);

  if ACurrentRow = ARows.Last then
    Exit(AActualBounds.Bottom - ARowBounds.Y);

  Result := ARowBounds.Height;
end;

procedure TdxPrintingDocumentExporter.DrawParagagraphBackground(const AActualBounds: TRect; ARows: TdxRowCollection);
var
  AIndex, Y, AHeight: Integer;
  ACurrentRow: TdxRow;
  ARowParagraph: TdxParagraph;
  ARowBounds, AActualParagraphBounds: TRect;
begin
  for AIndex := 0 to ARows.Count - 1 do
  begin
    ACurrentRow := ARows[AIndex];
    ARowParagraph := TdxParagraph(ACurrentRow.Paragraph);
    if not ARowParagraph.IsInCell then
    begin
      ARowBounds := GetDrawingBounds(ACurrentRow.Bounds);

      if ACurrentRow <> ARows.First then
        Y := ARowBounds.Top
      else
        Y := AActualBounds.Top;

      AHeight := GetActualBoxHeight(AActualBounds, ARows, ACurrentRow, ARowBounds);

      AActualParagraphBounds := TRect.CreateSize(AActualBounds.X, Y, AActualBounds.Width, AHeight);
      FillRectangle(ARowParagraph.BackColor, AActualParagraphBounds);
    end;
  end;
end;

procedure TdxPrintingDocumentExporter.DrawParagraphBordersWithoutTableBounds(const AActualBounds: TRect;
  ABoxParagraphProperties: TdxParagraphProperties; ARows: TdxRowCollection);
var
  AIndex, Y, AHeight: Integer;
  ACurrentRow: TdxRow;
  ARowBounds, AActualParagraphBounds: TRect;
begin
  AIndex := 0;
  while AIndex < ARows.Count do
  begin
    ACurrentRow := ARows[AIndex];
    if not TdxParagraph(ACurrentRow.Paragraph).IsInCell then
    begin
      ARowBounds := ACurrentRow.Bounds;

      if ACurrentRow <> ARows.First then
        Y := ARowBounds.Y
      else
        Y := AActualBounds.Y;

      AHeight := 0;
      repeat
        Inc(AHeight, GetActualBoxHeight(AActualBounds, ARows, ARows[AIndex], ARows[AIndex].Bounds));
        if AIndex < ARows.Count - 1 then
          Inc(AIndex);
      until not ((AIndex < ARows.Count - 1) and not TdxParagraph(ARows[AIndex].Paragraph).IsInCell);

      AActualParagraphBounds := TRect.CreateSize(AActualBounds.X, Y, AActualBounds.Width, AHeight);
      DrawParagraphBordersWithCorners(AActualParagraphBounds, ABoxParagraphProperties);
      Inc(AIndex);
    end;
  end;
end;

procedure TdxPrintingDocumentExporter.DrawParagraphBorders(const AContentBounds: TRect; AParagraphProperties: TdxParagraphProperties);
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
  ARightBorderBounds := TRect.CreateSize(AContentBounds.Right, AContentBounds.Y, 0, AContentBounds.Height);
  ExportTableBorder(ABorderViewInfo, ARightBorderBounds);

  ABorderViewInfo := TdxTableCellVerticalBorderViewInfo.Create(nil, ALeftBorder, 0, 0, AConverter);
  ALeftBorderBounds := TRect.CreateSize(AContentBounds.Left, AContentBounds.Y, 0, AContentBounds.Height);
  ExportTableBorder(ABorderViewInfo, ALeftBorderBounds);

  ALeftCorner := TdxSingleLineCornerViewInfo.Create(ALeftBorder, ARightBorder, ATopBorder, ABottomBorder, 0.0, 0.0, TdxCornerViewInfoType.OuterVerticalStart);
  ARightCorner := TdxSingleLineCornerViewInfo.Create(ALeftBorder, ARightBorder, ATopBorder, ABottomBorder, 0.0, 0.0, TdxCornerViewInfoType.OuterVerticalEnd);

  AHorizontalBorderViewInfo := TdxParagraphHorizontalBorderViewInfo.Create(ATopBorder, AConverter, ALeftCorner, ARightCorner);
  ATopBorderBounds := TRect.CreateSize(AContentBounds.X, AContentBounds.Top, AContentBounds.Width, 0);
  ExportTableBorder(AHorizontalBorderViewInfo, ATopBorderBounds);

  ABottomBorderBounds := TRect.CreateSize(AContentBounds.X, AContentBounds.Bottom, AContentBounds.Width, 0);
  AHorizontalBorderViewInfo := TdxParagraphHorizontalBorderViewInfo.Create(ABottomBorder, AConverter, ALeftCorner, ARightCorner);
  ExportTableBorder(AHorizontalBorderViewInfo, ABottomBorderBounds);
end;

procedure TdxPrintingDocumentExporter.DrawParagraphBordersCorners(const AContentBounds: TRect; AParagraphProperties: TdxParagraphProperties);
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
  ExportTableBorderCorner(ATopLeftCorner, AContentBounds.Left, AContentBounds.Top);

  ATopRightCorner := TdxCornerViewInfoBase.CreateCorner(TdxCornerViewInfoType.OuterHorizontalEnd, AConverter, ATopBorder, nil, nil, ARightBorder, 0);
  ExportTableBorderCorner(ATopRightCorner, AContentBounds.Right, AContentBounds.Top);

  ABottomLeftCorner := TdxCornerViewInfoBase.CreateCorner(TdxCornerViewInfoType.OuterHorizontalEnd, AConverter, nil, ALeftBorder, ABottomBorder, nil, 0);
  ExportTableBorderCorner(ABottomLeftCorner, AContentBounds.Left, AContentBounds.Bottom);

  ABottomRightCorner := TdxCornerViewInfoBase.CreateCorner(TdxCornerViewInfoType.OuterHorizontalEnd, AConverter, ABottomBorder, ARightBorder, nil, nil, 0);
  ExportTableBorderCorner(ABottomRightCorner, AContentBounds.Right, AContentBounds.Bottom);
end;

procedure TdxPrintingDocumentExporter.ExportNumberingListBox(ABox: TdxNumberingListBox);
begin
  ExportBoxCore(ABox);
end;

procedure TdxPrintingDocumentExporter.ExportLineNumberBox(ABox: TdxLineNumberBox);
begin
  ExportBoxCore(ABox);
end;

procedure TdxPrintingDocumentExporter.ExportTextBox(ABox: TdxTextBox);
begin
  ExportBoxCore(ABox);
end;

procedure TdxPrintingDocumentExporter.ExportLayoutDependentTextBox(ABox: TdxLayoutDependentTextBox);
begin
  ExportBoxCore(ABox);
end;

procedure TdxPrintingDocumentExporter.ExportHyphenBox(ABox: TdxHyphenBox);
begin
  ExportBoxCore(ABox);
end;

procedure TdxPrintingDocumentExporter.ExportSpaceBox(ABox: TdxBox);
begin
end;

procedure TdxPrintingDocumentExporter.ExportTabSpaceBox(ABox: TdxTabSpaceBox);
var
  ATabLeaderText: string;
begin
  if ABox.TabInfo.Leader = TdxTabLeaderType.None then
    Exit;
  ATabLeaderText := GetTabLeaderText(ABox, CorrectTextDrawingBounds(ABox, ABox.Bounds));
  ExportBoxCore(ABox, ATabLeaderText);
end;

procedure TdxPrintingDocumentExporter.ExportLineBreakBox(ABox: TdxLineBreakBox);
begin
end;

procedure TdxPrintingDocumentExporter.ExportPageBreakBox(ABox: TdxPageBreakBox);
begin
end;

procedure TdxPrintingDocumentExporter.ExportParagraphMarkBox(ABox: TdxParagraphMarkBox);
begin
end;

procedure TdxPrintingDocumentExporter.ExportColumnBreakBox(ABox: TdxColumnBreakBox);
begin
end;

procedure TdxPrintingDocumentExporter.ExportSectionMarkBox(ABox: TdxSectionMarkBox);
begin
end;

procedure TdxPrintingDocumentExporter.ExportBoxCore(ABox: TdxBox);
var
  AText: string;
begin
  AText := GetBoxText(ABox);
  ExportBoxCore(ABox, AText);
end;

procedure TdxPrintingDocumentExporter.ExportBoxCore(ABox: TdxBox; const AText: string);
var
  ABrick: TdxOfficeTextBrick;
  AFontInfo: TdxGdiFontInfo;
  ATextViewInfo: TdxGdiTextViewInfo;
  AForeColor: TColor;
  AFontSize: Integer;
begin
  AFontInfo := TdxGdiFontInfo(ABox.GetFontInfo(PieceTable));

  ABrick := TdxOfficeTextBrick.Create(ContainerCell);

  if AText = '' then
    ABrick.SetText(AText)
  else
  begin
    ATextViewInfo := TdxGdiTextViewInfo(FMeasurer.TextViewInfoCache.TryGetTextViewInfo(AText, AFontInfo));
    ABrick.SetText(AText, PWord(ATextViewInfo.Glyphs), PInteger(ATextViewInfo.CharacterWidths), ATextViewInfo.GlyphCount);
  end;
  ABrick.BoundsRect := CorrectTextDrawingBounds(ABox, ABox.Bounds);

  AForeColor := TdxAlphaColors.ToColor(ABox.GetActualForeColor(PieceTable, TextColors, GetActualBackColor(ABox.Bounds)));
  ABrick.Color := AForeColor;
  AFontSize := ReportLink.GetFontSize(AFontInfo.SizeInPoints);
  ABrick.FontIndex := ReportLink.AddFontToPool(AFontInfo.FamilyName, AForeColor, fpDefault, AFontInfo.Style, AFontSize);
end;

procedure TdxPrintingDocumentExporter.ExportHighlightArea(const AArea: TdxHighlightArea);
var
  ABrick: TdxOfficeRectBrick;
begin
  ABrick := TdxOfficeRectBrick.Create(ContainerCell);
  ABrick.BoundsRect := AArea.Bounds;
  ABrick.Color := TdxAlphaColors.ToColor(AArea.Color);
end;

function TdxPrintingDocumentExporter.CorrectTextDrawingBounds(ABox: TdxBox; const ATextBounds: TRect): TRect;
var
  AFontInfo: TdxFontInfo;
begin
  AFontInfo := ABox.GetFontInfo(PieceTable);
  Result := ATextBounds;
  Result.Y := Result.Y + AFontInfo._Free - AFontInfo.DrawingOffset;
end;

function TdxPrintingDocumentExporter.GetDrawingBounds(const ABounds: TRect): TRect;
begin
  Result := ABounds;
end;

procedure TdxPrintingDocumentExporter.ExportUnderlineBox(ARow: TdxSimpleRow; AUnderlineBox: TdxUnderlineBox);
var
  ABox: TdxBox;
  ALineType: TdxUnderlineType;
begin
  ABox := GetCharacterLineBoxByIndex(ARow, AUnderlineBox.StartAnchorIndex);
  ALineType := ABox.GetFontUnderlineType(PieceTable);
  ExportLineBoxCore<TdxUnderlineType>(AUnderlineBox,
    DocumentModel.UnderlineRepository.GetPatternLineByType(ALineType),
    TdxLineBrick.CreateUnderlineBrick(ContainerCell, ALineType, True),
    ABox.GetActualUnderlineColor(PieceTable, TextColors, GetBackColor(GetDrawingBounds(ABox.Bounds))));
end;

procedure TdxPrintingDocumentExporter.ExportStrikeoutBox(ARow: TdxSimpleRow; AStrikeoutBox: TdxUnderlineBox);
var
  ABox: TdxBox;
  ALineType: TdxStrikeoutType;
begin
  ABox := GetCharacterLineBoxByIndex(ARow, AStrikeoutBox.StartAnchorIndex);
  ALineType := ABox.GetFontStrikeoutType(PieceTable);
  ExportLineBoxCore<TdxStrikeoutType>(AStrikeoutBox,
    DocumentModel.StrikeoutRepository.GetPatternLineByType(ALineType),
    TdxLineBrick.CreateStrikeoutBrick(ContainerCell, ALineType),
    ABox.GetActualStrikeoutColor(PieceTable, TextColors, GetBackColor(GetDrawingBounds(ABox.Bounds))));
end;

procedure TdxPrintingDocumentExporter.ExportLineBoxCore<T>(ALineBox: TdxUnderlineBox;
  ALine: TdxPatternLine<T>; ABrick: TdxLineBrick; ALineColor: TdxAlphaColor);
var
  ABounds: TRect;
  ALineSize: Integer;
  AOffset: Integer;
begin
  ABrick.Transparent := True;
  ABrick.Color := TdxAlphaColors.ToColor(ALineColor);

  ABounds := ALine.CalcLineBounds(ALineBox.UnderlineBounds, ALineBox.UnderlineThickness);
  ABounds.Left := ALineBox.ClipBounds.Left;
  ABounds.Right := ALineBox.ClipBounds.Right;

  ALineSize := DocumentModel.LayoutUnitConverter.PixelsToLayoutUnits(1, Screen.PixelsPerInch);
  AOffset := Trunc(ALine.CalcLinePenVerticalOffset(ABounds.ToRectF));
  if AOffset <> 0 then
    AOffset := Max(AOffset, ALineSize);
  ABounds.Y := ABounds.Top + AOffset;
  ABounds.Height := Max(ABounds.Height, ABrick.PatternSize * ALineSize);

  ABrick.BoundsRect := ABounds;
end;

procedure TdxPrintingDocumentExporter.AddBrickToCurrentPage(ABrick: TdxBrick);
begin
  AddBrickToCurrentPage(ABrick, CurrentCell);
end;

procedure TdxPrintingDocumentExporter.AddBrickToCurrentPage(ABrick: TdxBrick; ACell: TdxTableCellViewInfo);
begin
  NotImplemented();
end;

procedure TdxPrintingDocumentExporter.AddBrickToCurrentContainer(ABrick: TdxBrick);
begin
    NotImplemented();
end;

procedure TdxPrintingDocumentExporter.ExportTableCell(ACell: TdxTableCellViewInfo);
var
  ARect, ABounds: TRect;
  ATop, ABottom: Integer;
  ABrick: TdxOfficeRectBrick;
begin
  ARect := ACell.TableViewInfo.GetCellBounds(ACell);
  ATop := ACell.TableViewInfo.Anchors[ACell.TopAnchorIndex].VerticalPosition;
  ABottom := ACell.TableViewInfo.Anchors[ACell.BottomAnchorIndex].VerticalPosition;
  ARect.Top := ATop;
  ARect.Height := ABottom - ATop;

  ABrick := TdxOfficeRectBrick.Create(ContainerCell);
  ABounds := GetDrawingBounds(ARect);
  ABrick.Color := TdxAlphaColors.ToColor(ACell.Cell.GetActualBackgroundColor);
  ABrick.BoundsRect := ARect;


  ExportInnerTables(ACell);
end;

procedure TdxPrintingDocumentExporter.ExportInnerTables(ACell: TdxTableCellViewInfo);
var
  AOldCurrentCell: TdxTableCellViewInfo;
  I: Integer;
begin
  AOldCurrentCell := CurrentCell;
  try
    SetCurrentCell(ACell);
    for I := 0 to ACell.InnerTables.Count - 1 do
      ACell.InnerTables[I].ExportBackground(Self);
  finally
    SetCurrentCell(AOldCurrentCell);
  end;
end;

procedure TdxPrintingDocumentExporter.ExportTableRow(ARow: TdxTableRowViewInfoBase);
var
  ABrick: TdxOfficeRectBrick;
begin
  ABrick := TdxOfficeRectBrick.Create(ContainerCell);
  ABrick.BoundsRect := GetDrawingBounds(ARow.GetBounds);
  ABrick.Color := TdxAlphaColors.ToColor(ARow.TableViewInfo.Table.BackgroundColor);
end;

procedure TdxPrintingDocumentExporter.ExportTableBorder(ABorder: TdxTableBorderViewInfoBase; const ACellBounds: TRect);
var
  AOldValue: Boolean;
  ABorderPainter: TdxTableBorderPainter;
begin
  AOldValue := FSuppressPutIntoTableCellBrick;
  FSuppressPutIntoTableCellBrick := True;
  try
    ABorderPainter := GetBorderPainter(ABorder);
    try
      if ABorderPainter <> nil then
        ABorderPainter.DrawBorder(ABorder, ACellBounds);
    finally
      ABorderPainter.Free;
    end;
  finally
    FSuppressPutIntoTableCellBrick := AOldValue;
  end;
end;

procedure TdxPrintingDocumentExporter.ExportTableBorderCorner(ACorner: TdxCornerViewInfoBase; X: Integer; Y: Integer);
var
  AOldValue: Boolean;
  ABorderPainter: TdxTableCornerPainter;
begin
  AOldValue := FSuppressPutIntoTableCellBrick;
  FSuppressPutIntoTableCellBrick := True;
  try
    ABorderPainter := GetBorderPainter(ACorner);
    try
      if ABorderPainter <> nil then
        ABorderPainter.DrawCorner(TdxPrinterPainterWrapper.Create(Self) as IdxPainterWrapper, X, Y, ACorner);
    finally
      ABorderPainter.Free;
    end;
  finally
    FSuppressPutIntoTableCellBrick := AOldValue;
  end;
end;

procedure TdxPrintingDocumentExporter.ExportTablesBackground(AColumn: TdxColumn);
var
  AColumnTables: TdxTableViewInfoCollection;
begin
  AColumnTables := AColumn.InnerTables;
  if AColumnTables <> nil then
    ExportTopLevelTables(AColumnTables);
end;

procedure TdxPrintingDocumentExporter.ExportTopLevelTables(AColumnTables: TdxTableViewInfoCollection);
var
  ACount, I: Integer;
begin
  ACount := AColumnTables.Count;
  for I := 0 to ACount - 1 do
    if AColumnTables[I].Table.NestedLevel = 0 then
      AColumnTables[I].ExportBackground(Self);
end;

function TdxPrintingDocumentExporter.GetBorderPainter(AViewInfo: TdxTableBorderViewInfoBase): TdxTableBorderPainter;
var
  ABorder: TdxBorderInfo;
  ABorderStyle: TdxBorderLineStyle;
  ABorderCalculator: TdxTableBorderCalculator;
  AWidth: Integer;
  ACompoundArray: TArray<Single>;
  APainterWrapper: IdxPainterWrapper;
begin
  ABorder := AViewInfo.Border;

  ABorderStyle := ABorder.Style;
  if (ABorderStyle = TdxBorderLineStyle.None) or (ABorderStyle = TdxBorderLineStyle.Disabled) or (ABorderStyle = TdxBorderLineStyle.Nil) then
    Exit(nil);

  ABorderCalculator := TdxTableBorderCalculator.Create;
  try
    AWidth := ABorderCalculator.GetActualWidth(ABorder);
    AWidth := AViewInfo.Converter.ToLayoutUnits(AWidth);
    ACompoundArray := ABorderCalculator.GetDrawingCompoundArray(ABorder);
    APainterWrapper := TdxPrinterPainterWrapper.Create(Self) as IdxPainterWrapper;
    if Length(ACompoundArray) = 4 then
      Result := TdxDoubleBorderPainter.Create(APainterWrapper, ACompoundArray, AWidth)
    else
      if Length(ACompoundArray) = 6 then
        Result := TdxTripleBorderPainter.Create(APainterWrapper, ACompoundArray, AWidth)
      else
        Result := TdxSingleBorderPainter.Create(APainterWrapper, AWidth, GetTableBorderLine(ABorder.Style));
  finally
    ABorderCalculator.Free;
  end;
end;

function TdxPrintingDocumentExporter.GetBorderPainter(ACorner: TdxCornerViewInfoBase): TdxTableCornerPainter;
begin
  if ACorner is TdxNoneLineCornerViewInfo then
    Exit(nil);
  Result := TdxTableCornerPainter.Create;
end;


end.
