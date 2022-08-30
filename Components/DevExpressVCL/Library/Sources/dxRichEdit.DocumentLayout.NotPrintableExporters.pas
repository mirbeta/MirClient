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

unit dxRichEdit.DocumentLayout.NotPrintableExporters;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Windows, Types, Classes, SysUtils, Graphics, Generics.Defaults, Generics.Collections, dxCore, dxCoreClasses, dxGDIPlusClasses,
  dxCoreGraphics, cxGraphics, cxGeometry,

  dxGDIPlusAPI,
  dxRichEdit.Types,
  dxRichEdit.Utils.Types,
  dxRichEdit.Utils.OfficeImage,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.Boxes.Simple,
  dxRichEdit.DocumentModel.Boxes,
  dxRichEdit.DocumentModel.FloatingObjectRange,
  dxRichEdit.DocumentLayout,
  dxRichEdit.DocumentLayout.Painters,
  dxRichEdit.DocumentLayout.UnitConverter,
  dxRichEdit.Platform.Win.Painter,
  dxRichEdit.Platform.PatternLinePainter,
  dxRichEdit.View.Core,
  dxRichEdit.View.ViewInfo;

type

  TdxNotPrintableGraphicsBoxExporter = class;

  { TdxTableNotPrintableBorderExporter }

  TdxTableNotPrintableBorderExporter = class
  strict private
    PenWidth: Integer;
    FExporter: TdxNotPrintableGraphicsBoxExporter;
  protected
    procedure DrawBorder(ABorder: TdxTableBorderViewInfoBase; const ACellBounds: TRect); virtual;
    function CreateDashPattern: TArray<Single>;
    function GetPoints(ABorder: TdxTableBorderViewInfoBase; X: Single; Y: Single; AHeight: Single; AWidth: Single): TArray<TdxPointF>;
  public
    constructor Create(AExporter: TdxNotPrintableGraphicsBoxExporter);
  end;

  { TdxBookmarkBoxExporter }

  TdxBookmarkBoxExporter = class
  public type
    TdxGetPointsDelegate = reference to function (const X, Y, AHeight: Single): TArray<TdxPointF>;
  strict private
    BookmarkBoxWidth: Integer;
    PenWidth: Integer;
    FExporter: TdxNotPrintableGraphicsBoxExporter;
  protected
    procedure DrawBookmarkBox(AColor: TdxAlphaColor; const APoints: TArray<TdxPointF>); virtual;
    function GetBookmarkStartPoints(const X, Y, AHeight: Single): TArray<TdxPointF>;
    function GetBookmarkEndPoints(const X, Y, AHeight: Single): TArray<TdxPointF>;
    function GetBookmarkPoints(const X, Y, AHeight, AWidth: Single): TArray<TdxPointF>; virtual;

    property Exporter: TdxNotPrintableGraphicsBoxExporter read FExporter;
  public
    constructor Create(AExporter: TdxNotPrintableGraphicsBoxExporter);
    procedure ExportBookmarkStartBox(ABookmarkBox: TdxVisitableDocumentIntervalBox); virtual;
    procedure ExportBookmarkEndBox(ABookmarkBox: TdxVisitableDocumentIntervalBox); virtual;
    procedure ExportBookmarkBoxCore(ABookmarkBox: TdxVisitableDocumentIntervalBox; const AGetPoints: TdxGetPointsDelegate);
  end;

  { TdxNotPrintableGraphicsBoxExporter }

  TdxNotPrintableGraphicsBoxExporter = class abstract(TdxDocumentLayoutExporter)
  strict private
    FView: TdxRichEditView;
    FPainter: TdxPainter;
    FBookmarkExporter: TdxBookmarkBoxExporter;
    FCustomMarkExporter: IdxCustomMarkExporter;
    FTableBorderExporter: TdxTableNotPrintableBorderExporter;
    FTableViewInfo: TdxTableViewInfo;
    function GetUnitConverter: TdxDocumentLayoutUnitConverter;
  protected
    FLineWidth: Single;
    FDotStep: Single;
    function GetPainter: TdxPainter; override;
    function CreateBookmarkBoxExporter: TdxBookmarkBoxExporter; virtual;
    function CreateTableBorderExporter: TdxTableNotPrintableBorderExporter;
    function GetActualBounds(const ABounds: TRect): TRect; virtual; abstract;
    function PixelsToDrawingUnits(AValue: Single): Single; virtual; abstract;
    procedure ExportTablesBackground(AColumn: TdxColumn); override;
    procedure ExportParagraphFrames(AColumn: TdxColumn; const APredicate: TdxFunc<TdxParagraphFrameBox, Boolean>); override;
    procedure ExportTables(AColumn: TdxColumn); override;
    procedure ExportRowCore; override;
    procedure ExportImeBoxes; virtual;
    procedure ExportBookmarkBoxes; virtual;
    procedure ExportRangePermissionBoxes; virtual;
    procedure ExportBookmarkBoxesCore(ABoxes: TdxVisitableDocumentIntervalBoxCollection); virtual;
    procedure ExportCustomMarkBoxesCore(ABoxes: TdxCustomMarkBoxCollection); virtual;
    procedure ExportHiddenTextBoxes; virtual;
    procedure ExportSpecialTextBoxes; virtual;
    procedure ExportHiddenTextBox(ABox: TdxHiddenTextUnderlineBox; APen: TdxGPPen);
    procedure DrawHiddenTextBox(ALeft: Integer; ARight: Integer; ABottom: Integer; APen: TdxGPPen);
    procedure DrawHorizontalDotLine(ALeft: Integer; ARight: Integer; Y: Integer; APen: TdxGPPen);
    procedure DrawVerticalDotLine(ATop: Integer; ABottom: Integer; X: Integer; APen: TdxGPPen);
    function GetActualCustomMarkBounds(const ABounds: TRect): TRect; virtual;
    procedure ExportCustomMarkBoxCore(ABox: TdxCustomMarkBox); virtual;
    procedure ExportSpecialTextBox(ABox: TdxSpecialTextBox; APen: TdxGPPen); overload; virtual;
    procedure SetClipBounds(const AClipBounds: TdxRectF); override;
    function GetClipBounds: TdxRectF; override;

    property UnitConverter: TdxDocumentLayoutUnitConverter read GetUnitConverter;
  public
    constructor Create(ADocumentModel: TdxDocumentModel; APainter: TdxPainter;
      AView: TdxRichEditView; const ACustomMarkExporter: IdxCustomMarkExporter);
    destructor Destroy; override;
    procedure ExportPageArea(APageArea: TdxPageArea); override;
    procedure ExportBookmarkStartBox(ABookmarkBox: TdxVisitableDocumentIntervalBox); override;
    procedure ExportBookmarkEndBox(ABookmarkBox: TdxVisitableDocumentIntervalBox); override;
    procedure ExportCommentStartBox(ACommentBox: TdxVisitableDocumentIntervalBox); override;
    procedure ExportCommentEndBox(ACommentBox: TdxVisitableDocumentIntervalBox); override;
    procedure ExportCustomMarkBox(ABox: TdxCustomMarkBox); override;
    procedure ExportFloatingObjectTextBox(ABox: TdxFloatingObjectBox; ATextBoxContent: TdxTextBoxFloatingObjectContent; ATextBoxDocumentLayout: TdxDocumentLayout); override;
    function ExportRotatedContent(ABox: TdxFloatingObjectBox): Boolean; override;
    procedure ExportTableBorder(ABorder: TdxTableBorderViewInfoBase; const ACellBounds: TRect); override;
    function IsTableBorderInvisible(ABorder: TdxTableBorderViewInfoBase): Boolean;
  end;

  { TdxWinFormsNotPrintableGraphicsBoxExporter }

  TdxWinFormsNotPrintableGraphicsBoxExporter = class(TdxNotPrintableGraphicsBoxExporter)
  strict private
    FCurrentPageInfo: TdxPageViewInfo;
    FView: TdxRichEditView;
  protected
    procedure ExportCurrentPage; virtual;
    function GetActualBounds(const ABounds: TRect): TRect; override;
    function GetActualCustomMarkBounds(const ABounds: TRect): TRect; override;
    function PixelsToDrawingUnits(AValue: Single): Single; override;
    procedure ExportImeBoxes; override;

    property CurrentPageInfo: TdxPageViewInfo read FCurrentPageInfo write FCurrentPageInfo;
  public
    constructor Create(ADocumentModel: TdxDocumentModel; APainter: TdxPainter; AView: TdxRichEditView; const ACustomMarkExporter: IdxCustomMarkExporter);
    procedure ExportPage(APage: TdxPageViewInfo); virtual;

    property View: TdxRichEditView read FView;
  end;

implementation

uses
{$IFDEF DELPHIXE2}
  System.UITypes,
{$ENDIF}
  Math, StrUtils, dxTypeHelpers, dxSpellCheckerCore,
  dxRichEdit.Utils.Graphics,
  dxStringHelper,
  dxRichEdit.Utils.Mouse,
  dxRichEdit.InnerControl.Mouse,
  dxRichEdit.DocumentModel.Bookmarks,
  dxRichEdit.DocumentModel.Borders;

{ TdxTableNotPrintableBorderExporter }

constructor TdxTableNotPrintableBorderExporter.Create(AExporter: TdxNotPrintableGraphicsBoxExporter);
begin
  inherited Create;
  PenWidth := 1;
  Assert(AExporter <> nil);
  FExporter := AExporter;
end;

procedure TdxTableNotPrintableBorderExporter.DrawBorder(ABorder: TdxTableBorderViewInfoBase; const ACellBounds: TRect);
var
  APainter: TdxPainter;
  APen: TdxGPColorPen;
  ADrawingBounds: TRect;
  APoints: TArray<TdxPointF>;
begin
  APainter := FExporter.Painter;
  APen := APainter.GetPen(TdxAlphaColors.FromArgb(255, 30, 144, 255), FExporter.PixelsToDrawingUnits(PenWidth));
  try
    APen.DashPattern := CreateDashPattern;
    ADrawingBounds := FExporter.GetActualBounds(ACellBounds);
    APoints := GetPoints(ABorder, ADrawingBounds.X, ADrawingBounds.Y, ADrawingBounds.Height, ADrawingBounds.Width);
    APainter.DrawLines(APen, APoints);
  finally
    APainter.ReleasePen(APen);
  end;
end;

function TdxTableNotPrintableBorderExporter.CreateDashPattern: TArray<Single>;
begin
  Result := TArray<Single>.Create(FExporter.PixelsToDrawingUnits(3), FExporter.PixelsToDrawingUnits(1));
end;

function TdxTableNotPrintableBorderExporter.GetPoints(ABorder: TdxTableBorderViewInfoBase; X: Single; Y: Single; AHeight: Single; AWidth: Single): TArray<TdxPointF>;
begin
  if ABorder.BorderType and TdxBorderTypes.Left <> 0 then
    Result := TArray<TdxPointF>.Create(TdxPointF.Create(X, Y), TdxPointF.Create(X, Y + AHeight))
  else
    if ABorder.BorderType and TdxBorderTypes.Right <> 0 then
      Result := TArray<TdxPointF>.Create(TdxPointF.Create(X + AWidth, Y), TdxPointF.Create(X + AWidth, Y + AHeight))
    else
      if ABorder.BorderType and TdxBorderTypes.Top <> 0 then
        Result := TArray<TdxPointF>.Create(TdxPointF.Create(X, Y), TdxPointF.Create(X + AWidth, Y))
      else
        if ABorder.BorderType and TdxBorderTypes.Bottom <> 0 then
          Result := TArray<TdxPointF>.Create(TdxPointF.Create(X, Y + AHeight), TdxPointF.Create(X + AWidth, Y + AHeight))
        else
          Result := nil;
end;

{ TdxBookmarkBoxExporter }

constructor TdxBookmarkBoxExporter.Create(AExporter: TdxNotPrintableGraphicsBoxExporter);
begin
  inherited Create;
  BookmarkBoxWidth := 4;
  PenWidth := 2;
  Assert(AExporter <> nil);
  FExporter := AExporter;
end;

procedure TdxBookmarkBoxExporter.ExportBookmarkStartBox(ABookmarkBox: TdxVisitableDocumentIntervalBox);
begin
  ExportBookmarkBoxCore(ABookmarkBox, GetBookmarkStartPoints);
end;

procedure TdxBookmarkBoxExporter.ExportBookmarkEndBox(ABookmarkBox: TdxVisitableDocumentIntervalBox);
begin
  ExportBookmarkBoxCore(ABookmarkBox, GetBookmarkEndPoints);
end;

procedure TdxBookmarkBoxExporter.ExportBookmarkBoxCore(ABookmarkBox: TdxVisitableDocumentIntervalBox;
  const AGetPoints: TdxGetPointsDelegate);
var
  ABookmark: TdxBookmark;
  AHorizontalPosition, ABottom, ATop: Integer;
  ARow: TdxRow;
  ABounds, ADrawingBounds: TRect;
begin
  ABookmark := Safe<TdxBookmark>.Cast(ABookmarkBox.Interval);
  if ABookmark = nil then
    Exit;
  AHorizontalPosition := ABookmarkBox.HorizontalPosition;
  ARow := FExporter.CurrentRow;
  ABottom := ARow.Boxes.First.Bounds.Bottom;
  ATop := ARow.Bounds.Top + ARow.SpacingBefore;
  ABounds := TRect.Create(AHorizontalPosition, ATop, AHorizontalPosition, ABottom);
  ADrawingBounds := FExporter.GetActualBounds(ABounds);
  DrawBookmarkBox(ABookmarkBox.Color, AGetPoints(ADrawingBounds.X, ADrawingBounds.Y, ADrawingBounds.Height));
end;

procedure TdxBookmarkBoxExporter.DrawBookmarkBox(AColor: TdxAlphaColor; const APoints: TArray<TdxPointF>);
var
  APainter: TdxPainter;
  APen: TdxGPPen;
begin
  APainter := FExporter.Painter;
  APen := APainter.GetPen(AColor, PenWidth);
  try

    APainter.DrawLines(APen, APoints);
  finally
    APainter.ReleasePen(APen);
  end;
end;

function TdxBookmarkBoxExporter.GetBookmarkStartPoints(const X, Y, AHeight: Single): TArray<TdxPointF>;
begin
  Result := GetBookmarkPoints(X, Y, AHeight, FExporter.PixelsToDrawingUnits(BookmarkBoxWidth));
end;

function TdxBookmarkBoxExporter.GetBookmarkEndPoints(const X, Y, AHeight: Single): TArray<TdxPointF>;
begin
  Result := GetBookmarkPoints(X, Y, AHeight, FExporter.PixelsToDrawingUnits(-BookmarkBoxWidth));
end;

function TdxBookmarkBoxExporter.GetBookmarkPoints(const X, Y, AHeight, AWidth: Single): TArray<TdxPointF>;
var
  AOffset, ALeft, ARight, ATop, ABottom: Single;
begin
  AOffset := FExporter.PixelsToDrawingUnits(PenWidth) / 2;
  ALeft := X - AOffset;
  ARight := X - AOffset + AWidth;
  ATop := Y + AOffset;
  ABottom := Y + AHeight - AOffset;
  Result := TArray<TdxPointF>.Create(TdxPointF.Create(ARight, ATop),
    TdxPointF.Create(ALeft, ATop), TdxPointF.Create(ALeft, ABottom), TdxPointF.Create(ARight, ABottom));
end;

{ TdxNotPrintableGraphicsBoxExporter }

constructor TdxNotPrintableGraphicsBoxExporter.Create(ADocumentModel: TdxDocumentModel; APainter: TdxPainter; AView: TdxRichEditView; const ACustomMarkExporter: IdxCustomMarkExporter);
begin
  inherited Create(ADocumentModel);
  Assert(APainter <> nil);
  FPainter := APainter;
  Assert(AView <> nil);
  FView := AView;

  FBookmarkExporter := CreateBookmarkBoxExporter;
  FCustomMarkExporter := ACustomMarkExporter;
  FTableBorderExporter := CreateTableBorderExporter;
end;

destructor TdxNotPrintableGraphicsBoxExporter.Destroy;
begin
  FreeAndNil(FBookmarkExporter);
  FCustomMarkExporter := nil;
  FreeAndNil(FTableBorderExporter);
  inherited Destroy;
end;

function TdxNotPrintableGraphicsBoxExporter.GetPainter: TdxPainter;
begin
  Result := FPainter;
end;

function TdxNotPrintableGraphicsBoxExporter.GetUnitConverter: TdxDocumentLayoutUnitConverter;
begin
  Result := DocumentModel.LayoutUnitConverter;
end;


function TdxNotPrintableGraphicsBoxExporter.CreateBookmarkBoxExporter: TdxBookmarkBoxExporter;
begin
  Result := TdxBookmarkBoxExporter.Create(Self);
end;

function TdxNotPrintableGraphicsBoxExporter.CreateTableBorderExporter: TdxTableNotPrintableBorderExporter;
begin
  Result := TdxTableNotPrintableBorderExporter.Create(Self);
end;

procedure TdxNotPrintableGraphicsBoxExporter.ExportPageArea(APageArea: TdxPageArea);
begin
  APageArea.Columns.ExportTo(Self);
end;

procedure TdxNotPrintableGraphicsBoxExporter.ExportTablesBackground(AColumn: TdxColumn);
begin
end;

procedure TdxNotPrintableGraphicsBoxExporter.ExportParagraphFrames(AColumn: TdxColumn;
  const APredicate: TdxFunc<TdxParagraphFrameBox, Boolean>);
begin
end;

procedure TdxNotPrintableGraphicsBoxExporter.ExportTables(AColumn: TdxColumn);
var
  ATables: TdxTableViewInfoCollection;
  ACount, I: Integer;
begin
  ATables := AColumn.InnerTables;
  if ATables <> nil then
  begin
    ACount := ATables.Count;
    for I := 0 to ACount - 1 do
    begin
      FTableViewInfo := ATables[I];
      ATables[I].ExportTo(Self);
    end;
  end;
end;

procedure TdxNotPrintableGraphicsBoxExporter.ExportRowCore;
begin
  ExportBookmarkBoxes;
  ExportRangePermissionBoxes;
  ExportImeBoxes;
  ExportHiddenTextBoxes;
  ExportSpecialTextBoxes;
end;

procedure TdxNotPrintableGraphicsBoxExporter.ExportImeBoxes;
begin
end;

procedure TdxNotPrintableGraphicsBoxExporter.ExportBookmarkBoxes;
var
  ABookmarkBoxes: TdxVisitableDocumentIntervalBoxCollection;
begin
  ABookmarkBoxes := CurrentRow.InnerBookmarkBoxes;
  if ABookmarkBoxes <> nil then
    ExportBookmarkBoxesCore(ABookmarkBoxes);
end;

procedure TdxNotPrintableGraphicsBoxExporter.ExportRangePermissionBoxes;
var
  ARangePermissionBoxes: TdxVisitableDocumentIntervalBoxCollection;
begin
  ARangePermissionBoxes := CurrentRow.InnerRangePermissionBoxes;
  if ARangePermissionBoxes <> nil then
    ExportBookmarkBoxesCore(ARangePermissionBoxes);
end;


procedure TdxNotPrintableGraphicsBoxExporter.ExportBookmarkBoxesCore(ABoxes: TdxVisitableDocumentIntervalBoxCollection);
var
  ACount, I: Integer;
begin
  ACount := ABoxes.Count;
  for I := 0 to ACount - 1 do
    ABoxes[I].ExportTo(Self);
end;

procedure TdxNotPrintableGraphicsBoxExporter.ExportCustomMarkBoxesCore(ABoxes: TdxCustomMarkBoxCollection);
var
  ACount, I: Integer;
begin
  ACount := ABoxes.Count;
  for I := 0 to ACount - 1 do
    ABoxes[I].ExportTo(Self);
end;

procedure TdxNotPrintableGraphicsBoxExporter.ExportHiddenTextBoxes;
var
  ABoxes: TdxHiddenTextUnderlineBoxCollection;
  APen: TdxGPPen;
  ACount, I: Integer;
begin
  ABoxes := CurrentRow.InnerHiddenTextBoxes;
  if (ABoxes = nil) or (ABoxes.Count <= 0) then
    Exit;
  FLineWidth := PixelsToDrawingUnits(0.5);
  FDotStep := PixelsToDrawingUnits(3);

  APen := Painter.GetPen(TdxAlphaColors.Black, 1);
  try
    ACount := ABoxes.Count;
    for I := 0 to ACount - 1 do
      ExportHiddenTextBox(ABoxes[I], APen);
  finally
    Painter.ReleasePen(APen);
  end;
end;

procedure TdxNotPrintableGraphicsBoxExporter.ExportSpecialTextBoxes;
var
  ABoxes: TdxSpecialTextBoxCollection;
  APen: TdxGPPen;
  ACount, I: Integer;
begin
  if not DocumentModel.FormattingMarkVisibilityOptions.ShowHiddenText then
    Exit;

  ABoxes := CurrentRow.InnerSpecialTextBoxes;
  if (ABoxes = nil) or (ABoxes.Count <= 0) then
    Exit;

  FLineWidth := PixelsToDrawingUnits(0.5);
  FDotStep := PixelsToDrawingUnits(3);
  APen := Painter.GetPen(TdxAlphaColors.Black, 1);
  try
    ACount := ABoxes.Count;
    for I := 0 to ACount - 1 do
      ExportSpecialTextBox(ABoxes[I], APen);
  finally
    Painter.ReleasePen(APen);
  end;
end;

procedure TdxNotPrintableGraphicsBoxExporter.ExportHiddenTextBox(ABox: TdxHiddenTextUnderlineBox; APen: TdxGPPen);
var
  ABottom: Integer;
  ABounds, ADrawBounds: TRect;
begin
  ABottom := CurrentRow.Bounds.Top + CurrentRow.BaseLineOffset + ABox.BottomOffset;
  ABounds := TRect.Create(ABox.Start, ABottom, ABox.&End, ABottom);
  ADrawBounds := GetActualBounds(ABounds);
  DrawHiddenTextBox(ADrawBounds.Left, ADrawBounds.Right, ADrawBounds.Bottom, APen);
end;

procedure TdxNotPrintableGraphicsBoxExporter.DrawHiddenTextBox(ALeft: Integer; ARight: Integer; ABottom: Integer; APen: TdxGPPen);
begin
  DrawHorizontalDotLine(ALeft, ARight, ABottom, APen);
end;

procedure TdxNotPrintableGraphicsBoxExporter.DrawHorizontalDotLine(ALeft: Integer; ARight: Integer; Y: Integer; APen: TdxGPPen);
var
  I: Single;
begin
  I := ALeft;
  while I < ARight do
  begin
    Painter.DrawLine(APen, I, Y, I + FLineWidth, Y);
    I := I + FDotStep;
  end;
end;

procedure TdxNotPrintableGraphicsBoxExporter.DrawVerticalDotLine(ATop: Integer; ABottom: Integer; X: Integer; APen: TdxGPPen);
var
  I: Single;
begin
  I := ATop;
  while I < ABottom do
  begin
    Painter.DrawLine(APen, X, I, X, I + FLineWidth);
    I := I + FDotStep;
  end;
end;

function TdxNotPrintableGraphicsBoxExporter.GetActualCustomMarkBounds(const ABounds: TRect): TRect;
begin
  Result := GetActualBounds(ABounds);
end;

procedure TdxNotPrintableGraphicsBoxExporter.ExportCustomMarkBoxCore(ABox: TdxCustomMarkBox);
var
  ADrawBounds: TRect;
begin

  ADrawBounds := GetActualCustomMarkBounds(ABox.Bounds);
end;

procedure TdxNotPrintableGraphicsBoxExporter.ExportSpecialTextBox(ABox: TdxSpecialTextBox; APen: TdxGPPen);
var
  ABounds: TRect;
begin
  ABounds := GetActualBounds(ABox.Bounds);
  DrawHorizontalDotLine(ABounds.Left, ABounds.Right, ABounds.Top, APen);
  DrawVerticalDotLine(ABounds.Top, ABounds.Bottom, ABounds.Right, APen);
  DrawHorizontalDotLine(ABounds.Left, ABounds.Right, ABounds.Bottom, APen);
  DrawVerticalDotLine(ABounds.Top, ABounds.Bottom, ABounds.Left, APen);
end;

procedure TdxNotPrintableGraphicsBoxExporter.ExportBookmarkStartBox(ABookmarkBox: TdxVisitableDocumentIntervalBox);
begin
  FBookmarkExporter.ExportBookmarkStartBox(ABookmarkBox);
end;

procedure TdxNotPrintableGraphicsBoxExporter.ExportBookmarkEndBox(ABookmarkBox: TdxVisitableDocumentIntervalBox);
begin
  FBookmarkExporter.ExportBookmarkEndBox(ABookmarkBox);
end;

procedure TdxNotPrintableGraphicsBoxExporter.ExportCommentStartBox(ACommentBox: TdxVisitableDocumentIntervalBox);
begin
end;

procedure TdxNotPrintableGraphicsBoxExporter.ExportCommentEndBox(ACommentBox: TdxVisitableDocumentIntervalBox);
begin
end;

procedure TdxNotPrintableGraphicsBoxExporter.ExportCustomMarkBox(ABox: TdxCustomMarkBox);
begin
  ExportCustomMarkBoxCore(ABox);
end;

function TdxNotPrintableGraphicsBoxExporter.ExportRotatedContent(ABox: TdxFloatingObjectBox): Boolean;
var
  ABounds: TRect;
  ACenter: TPoint;
  ATransformApplied: Boolean;
begin
  ABounds := GetDrawingBounds(ABox.Bounds);
  ACenter := ABounds.CenterPoint;
  ATransformApplied := Painter.TryPushRotationTransform(ACenter, DocumentModel.GetBoxEffectiveRotationAngleInDegrees(ABox));
  Painter.PushSmoothingMode(ATransformApplied);

  try
    Exit(inherited ExportRotatedContent(ABox));
  finally
    Painter.PopSmoothingMode;
    if ATransformApplied then
      Painter.PopTransform;
  end;
end;

procedure TdxNotPrintableGraphicsBoxExporter.ExportFloatingObjectTextBox(ABox: TdxFloatingObjectBox; ATextBoxContent: TdxTextBoxFloatingObjectContent; ATextBoxDocumentLayout: TdxDocumentLayout);
var
  ABounds: TRect;
  AOldClipRect, AClipRect: TdxRectF;
begin
  ABounds := GetDrawingBounds(ABox.ContentBounds);
  AOldClipRect := GetClipBounds;
  AClipRect := IntersectClipBounds(AOldClipRect, ABounds.ToRectF);
  if AClipRect = TdxRectF.Null then
    Exit;

  SetClipBounds(AClipRect);
  ATextBoxDocumentLayout.Pages.First.ExportTo(Self);
  SetClipBounds(AOldClipRect);
end;

procedure TdxNotPrintableGraphicsBoxExporter.ExportTableBorder(ABorder: TdxTableBorderViewInfoBase; const ACellBounds: TRect);
var
  AShouldDrawGridLinesForAllTable: Boolean;
begin
  AShouldDrawGridLinesForAllTable :=
    (DocumentModel.TableOptions.GridLines = TdxRichEditTableGridLinesVisibility.Visible) and
    IsTableBorderInvisible(ABorder);
  if AShouldDrawGridLinesForAllTable then
    FTableBorderExporter.DrawBorder(ABorder, ACellBounds);
end;

function TdxNotPrintableGraphicsBoxExporter.IsTableBorderInvisible(ABorder: TdxTableBorderViewInfoBase): Boolean;
var
  ABorderLineStyle: TdxBorderLineStyle;
begin
  ABorderLineStyle := ABorder.Border.Style;
  Result := (ABorderLineStyle = TdxBorderLineStyle.None) or (ABorderLineStyle = TdxBorderLineStyle.Nil)
    or (ABorderLineStyle = TdxBorderLineStyle.Disabled);
end;

procedure TdxNotPrintableGraphicsBoxExporter.SetClipBounds(const AClipBounds: TdxRectF);
var
  X, Y, AWidth, AHeight, AScaleFactor: Single;
begin
  X := UnitConverter.LayoutUnitsToPixelsF(AClipBounds.Left, DocumentModel.DpiX);
  Y := UnitConverter.LayoutUnitsToPixelsF(AClipBounds.Top, DocumentModel.DpiY);
  AWidth := UnitConverter.LayoutUnitsToPixelsF(AClipBounds.Width, DocumentModel.DpiX);
  AHeight := UnitConverter.LayoutUnitsToPixelsF(AClipBounds.Height, DocumentModel.DpiY);
  AScaleFactor := FView.ScaleFactor;
  Painter.ClipBounds := TdxRectF.CreateSize(X * AScaleFactor, Y * AScaleFactor, AWidth * AScaleFactor, AHeight * AScaleFactor);
end;

function TdxNotPrintableGraphicsBoxExporter.GetClipBounds: TdxRectF;
var
  AScaleFactor, X, Y, AWidth, AHeight: Single;
  AClipBounds: TdxRectF;
begin
  AScaleFactor := FView.ScaleFactor;
  AClipBounds := Painter.ClipBounds;
  X := UnitConverter.PixelsToLayoutUnitsF(AClipBounds.Left, DocumentModel.DpiX);
  Y := UnitConverter.PixelsToLayoutUnitsF(AClipBounds.Top, DocumentModel.DpiY);
  AWidth := UnitConverter.PixelsToLayoutUnitsF(AClipBounds.Width, DocumentModel.DpiX);
  AHeight := UnitConverter.PixelsToLayoutUnitsF(AClipBounds.Height, DocumentModel.DpiY);
  Result := TdxRectF.CreateSize(X / AScaleFactor, Y / AScaleFactor, AWidth / AScaleFactor, AHeight / AScaleFactor);
end;

{ TdxWinFormsNotPrintableGraphicsBoxExporter }

constructor TdxWinFormsNotPrintableGraphicsBoxExporter.Create(ADocumentModel: TdxDocumentModel; APainter: TdxPainter; AView: TdxRichEditView; const ACustomMarkExporter: IdxCustomMarkExporter);
begin
  inherited Create(ADocumentModel, APainter, AView, ACustomMarkExporter);
  Assert(AView <> nil);
  FView := AView;
end;

procedure TdxWinFormsNotPrintableGraphicsBoxExporter.ExportPage(APage: TdxPageViewInfo);
begin
  FCurrentPageInfo := APage;
  ExportCurrentPage;
  FCurrentPageInfo := nil;
end;

procedure TdxWinFormsNotPrintableGraphicsBoxExporter.ExportCurrentPage;
begin
  CurrentPageInfo.Page.ExportTo(Self);
end;

function TdxWinFormsNotPrintableGraphicsBoxExporter.GetActualBounds(const ABounds: TRect): TRect;
var
  AScaleFactor: Single;
  ARect: TRect;
  ADpi: Integer;
begin
  AScaleFactor := View.ScaleFactor;
  ARect := TRect.CreateSize(Trunc(ABounds.X * AScaleFactor), Trunc(ABounds.Y * AScaleFactor),
    Trunc(ABounds.Width * AScaleFactor), Trunc(ABounds.Height * AScaleFactor));
  ADpi := Painter.DpiY;
  Result := UnitConverter.LayoutUnitsToPixels(ARect, ADpi, ADpi);
end;

function TdxWinFormsNotPrintableGraphicsBoxExporter.GetActualCustomMarkBounds(const ABounds: TRect): TRect;
var
  AResult: TRect;
  ADpi: Integer;
begin
  AResult := View.CreatePhysicalRectangle(CurrentPageInfo, ABounds);
  ADpi := Painter.DpiY;
  Result := UnitConverter.LayoutUnitsToPixels(AResult, ADpi, ADpi);
end;

function TdxWinFormsNotPrintableGraphicsBoxExporter.PixelsToDrawingUnits(AValue: Single): Single;
begin
  Result := AValue;
end;

procedure TdxWinFormsNotPrintableGraphicsBoxExporter.ExportImeBoxes;
begin
end;

end.
