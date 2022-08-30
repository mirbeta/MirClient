unit CustomDrawDemoMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, cxGeometry,
  Dialogs, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, dxSpreadSheetCore, dxSpreadSheetFunctions,
  dxSpreadSheetGraphics, dxSpreadSheetClasses, dxSpreadSheetTypes, dxSpreadSheet, BaseForm, dxCore, dxCoreClasses,
  dxHashUtils, dxSpreadSheetCoreHistory, dxSpreadSheetCoreStyles, dxSpreadSheetCoreStrs,
  dxSpreadSheetConditionalFormatting, dxSpreadSheetConditionalFormattingRules, dxSpreadSheetContainers,
  dxSpreadSheetFormulas, dxSpreadSheetHyperlinks, dxSpreadSheetStyles, dxSpreadSheetPrinting, dxSpreadSheetUtils,
  cxSplitter, dxSpreadSheetFormulaBar;

type
  TfrmCustomDraw = class(TfmBaseForm)
    SpreadSheet: TdxSpreadSheet;
    FormulaBar: TdxSpreadSheetFormulaBar;
    FormulaBarSplitter: TcxSplitter;
    procedure FormCreate(Sender: TObject);
    procedure SpreadSheetCustomDrawTableViewCell(Sender: TdxSpreadSheetTableView; ACanvas: TcxCanvas;
      AViewInfo: TdxSpreadSheetTableViewCellViewInfo; var AHandled: Boolean);
  private
  protected
    function GetSpreadSheet: TdxSpreadSheet; override;
  public
    procedure DrawCallout(ACanvas: TcxCanvas; AViewInfo: TdxSpreadSheetTableViewCellViewInfo);
  end;

var
  frmCustomDraw: TfrmCustomDraw;

implementation

{$R *.dfm}

uses
  Types;

procedure TfrmCustomDraw.FormCreate(Sender: TObject);
begin
  SpreadSheet.LoadFromFile('..\..\Data\CalculatorOfArea.xlsx');
end;

function TfrmCustomDraw.GetSpreadSheet: TdxSpreadSheet;
begin
  Result := SpreadSheet;
end;

procedure TfrmCustomDraw.DrawCallout(ACanvas: TcxCanvas; AViewInfo: TdxSpreadSheetTableViewCellViewInfo);
var
  AClipRgn, AFrameRgn: TcxRegion;
  FramePoints: array[0..5] of TPoint;
begin
  FramePoints[0] := cxPointOffset(AViewInfo.Bounds.TopLeft, 0, 10);
  FramePoints[1] := cxPointOffset(FramePoints[0], 5, -10);
  FramePoints[2] := cxPointOffset(FramePoints[1], 200, 0);
  FramePoints[3] := cxPointOffset(FramePoints[2], 0, -20);
  FramePoints[4] := cxPointOffset(FramePoints[3], -205, 0);
  FramePoints[5] := FramePoints[0];
  AFrameRgn := TcxRegion.Create;
  AFrameRgn.Handle := CreatePolygonRgn(FramePoints, Length(FramePoints), WINDING);
  ACanvas.SaveState;
  AClipRgn := TcxRegion.Create(AViewInfo.ViewInfo.CellsArea);
  AClipRgn.Combine(AFrameRgn, roIntersect, False);
  ACanvas.SetClipRegion(AClipRgn, roSet, False);
  FillRegionByColor(ACanvas.Handle, AFrameRgn.Handle, clInfoBk);
  FrameRgn(ACanvas.Handle, AFrameRgn.Handle, GetStockObject(BLACK_BRUSH), 1, 1);
  ACanvas.Font.Name := 'Tahoma';
  ACanvas.Font.Size := 10;
  ACanvas.Font.Color := clBlack;
  ACanvas.Font.Style := [fsBold];
  ACanvas.Brush.Style := bsClear;
  ACanvas.DrawTexT('Incorrect parameter values', cxRect(FramePoints[4], FramePoints[2]),
    cxAlignVCenter or cxAlignHCenter or cxSingleLine);
  ACanvas.RestoreState;
  ACanvas.SetClipRegion(AClipRgn, roSubtract);
  AFrameRgn.Free;
end;

procedure TfrmCustomDraw.SpreadSheetCustomDrawTableViewCell(
  Sender: TdxSpreadSheetTableView; ACanvas: TcxCanvas;
  AViewInfo: TdxSpreadSheetTableViewCellViewInfo; var AHandled: Boolean);
begin
  if (AViewInfo.Cell = nil) or  (AViewInfo.Cell.DataType <> cdtFormula) then Exit;
  if  (AViewInfo.Cell.AsFormula.ErrorCode <> ecNone) or (AViewInfo.Cell.AsFormula.ResultValue.ErrorCode <> ecNone) or
    (not VarIsNumeric(AViewInfo.Cell.AsFormula.Value) or (AViewInfo.Cell.AsFormula.Value < 0)) then
  begin
    ACanvas.Brush.Color := clRed;
    ACanvas.Font.Color := clAqua;
    DrawCallout(ACanvas, AViewInfo);
  end;
end;

end.
