unit CustomDrawMain;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, DemoBasicMain, cxLookAndFeels, Menus, StdCtrls, cxControls,
  cxCustomPivotGrid, cxDBPivotGrid, DemoBasicDM, cxGraphics, dxOffice11,
  cxClasses, cxCustomData, cxStyles, cxLookAndFeelPainters, cxEdit,
  ExtCtrls;

type
  TfrmCustomDraw = class(TfrmDemoBasicMain)
    DBPivotGrid: TcxDBPivotGrid;
    pgfPaymentType: TcxDBPivotGridField;
    pgfQuantity: TcxDBPivotGridField;
    pgfCarName: TcxDBPivotGridField;
    pgfUnitPrice: TcxDBPivotGridField;
    pgfCompanyName: TcxDBPivotGridField;
    pgfPaymentAmount: TcxDBPivotGridField;
    Drawing1: TMenuItem;
    Content1: TMenuItem;
    GroupHeaders1: TMenuItem;
    N3: TMenuItem;
    miLimitValues: TMenuItem;
    tmrColorChange: TTimer;
    procedure DBPivotGridCustomDrawCell(Sender: TcxCustomPivotGrid;
      ACanvas: TcxCanvas; AViewInfo: TcxPivotGridDataCellViewInfo;
      var ADone: Boolean);
    procedure DBPivotGridCustomDrawColumnHeader(Sender: TcxCustomPivotGrid;
      ACanvas: TcxCanvas; AViewInfo: TcxPivotGridHeaderCellViewInfo;
      var ADone: Boolean);
    procedure DBPivotGridCustomDrawRowHeader(Sender: TcxCustomPivotGrid;
      ACanvas: TcxCanvas; AViewInfo: TcxPivotGridHeaderCellViewInfo;
      var ADone: Boolean);
    procedure DrawingClick(Sender: TObject);
    procedure DBPivotGridSelectionChanged(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tmrColorChangeTimer(Sender: TObject);
  private
    FCurrentColorIndex: Integer;
    FColors: array of TColor;
    FList: TcxPivotGridRecords;
    FSupportFont: TFont;
  protected
    function GetDefaultLookAndFeelKind: TcxLookAndFeelKind; override;
    function GetPivotGrid: TcxCustomPivotGrid; override;
  public
    { Public declarations }
  end;

var
  frmCustomDraw: TfrmCustomDraw;

implementation

{$R *.dfm}

uses
  DemoUtils, Math, Contnrs;

const
  cxSelectedFontStylesMap: array[Boolean] of TFontStyles = ([], [fsBold]);

type
  TcxPivotGridViewDataItemAccess = class(TcxPivotGridViewDataItem);
  TcxPivotGridHeaderCellViewInfoAccess = class(TcxPivotGridHeaderCellViewInfo);
  TcxCustomPivotGridAccess = class(TcxCustomPivotGrid);

function TfrmCustomDraw.GetDefaultLookAndFeelKind: TcxLookAndFeelKind;
begin
  Result := lfUltraFlat;
end;

function TfrmCustomDraw.GetPivotGrid: TcxCustomPivotGrid;
begin
  Result := DBPivotGrid;
end;

procedure TfrmCustomDraw.DBPivotGridCustomDrawCell(
  Sender: TcxCustomPivotGrid; ACanvas: TcxCanvas;
  AViewInfo: TcxPivotGridDataCellViewInfo; var ADone: Boolean);

  function GetCellPosition: Integer;
  begin
    Result := MakeLong(AViewInfo.ColumnIndex, AViewInfo.RowIndex);
  end;

begin
  if (lvtColumnMaximum in AViewInfo.LimitValueTypes) and dxDemoIsMenuChecked(dxDemoFindMenuItem(mmMain, 'miLimitValues')) then
    ACanvas.Font.Color := clRed
  else
    if (lvtColumnMinimum in AViewInfo.LimitValueTypes) and dxDemoIsMenuChecked(dxDemoFindMenuItem(mmMain, 'miLimitValues')) then
      ACanvas.Font.Color := clLime
    else
    begin
      if FCurrentColorIndex = -1 then
      begin
        if (FList.Count < 5) and (RandomRange(0, 20) = 1) then
          FList.Add(GetCellPosition);
      end
      else
        if FList.IndexOf(Pointer(GetCellPosition)) > -1 then
        begin
          ACanvas.Font.Style := [fsBold];
          ACanvas.Font.Color := FColors[FCurrentColorIndex];
        end;
    end;
  if (AViewInfo.RowIndex mod 2 = 0) or (AViewInfo.RowIndex mod 2 = 0) then
    ACanvas.Brush.Color := clSilver;
end;

procedure TfrmCustomDraw.DBPivotGridCustomDrawColumnHeader(
  Sender: TcxCustomPivotGrid; ACanvas: TcxCanvas;
  AViewInfo: TcxPivotGridHeaderCellViewInfo; var ADone: Boolean);
var
  P: TPoint;
  AMin, AMax: Integer;
  AViewParams: TcxViewParams;
begin
  P := Sender.ViewData.FocusedCell;
  with TcxPivotGridViewDataItemAccess(TcxPivotGridHeaderCellViewInfoAccess(AViewInfo).Data) do
  begin
    AMin := GetChildLeftVisibleIndex;
    AMax := GetChildRightVisibleIndex;
  end;
  FSupportFont.Style := cxSelectedFontStylesMap[(P.X >= AMin) and (P.X <= AMax)];
  AViewParams := AViewInfo.ViewParams;
  AViewParams.Font := FSupportFont;
  AViewInfo.ViewParams := AViewParams;
end;

procedure TfrmCustomDraw.DBPivotGridCustomDrawRowHeader(
  Sender: TcxCustomPivotGrid; ACanvas: TcxCanvas;
  AViewInfo: TcxPivotGridHeaderCellViewInfo; var ADone: Boolean);
var
  P: TPoint;
  AMin, AMax: Integer;
  AViewParams: TcxViewParams;
begin
  P := Sender.ViewData.FocusedCell;
  with TcxPivotGridViewDataItemAccess(TcxPivotGridHeaderCellViewInfoAccess(AViewInfo).Data) do
  begin
    AMin := GetChildLeftVisibleIndex;
    AMax := GetChildRightVisibleIndex;
  end;
  FSupportFont.Style := cxSelectedFontStylesMap[(P.Y >= AMin) and (P.Y <= AMax)];
  AViewParams := AViewInfo.ViewParams;
  AViewParams.Font := FSupportFont;
  AViewInfo.ViewParams := AViewParams;
end;

procedure TfrmCustomDraw.DrawingClick(Sender: TObject);
var
  AChecked: Boolean;
begin
  AChecked := dxDemoIsMenuChecked(Sender);
  case TComponent(Sender).Tag of
    1:
      if AChecked then
      begin
        DBPivotGrid.OnCustomDrawCell := DBPivotGridCustomDrawCell;
        dxDemoMenuItemSetChecked(dxDemoFindMenuItem(mmMain, 'miLimitValues'), True);
        dxDemoMenuItemSetEnabled(dxDemoFindMenuItem(mmMain, 'miLimitValues'), True);
      end
      else
      begin
        dxDemoMenuItemSetChecked(dxDemoFindMenuItem(mmMain, 'miLimitValues'), False);
        dxDemoMenuItemSetEnabled(dxDemoFindMenuItem(mmMain, 'miLimitValues'), False);
        DBPivotGrid.OnCustomDrawCell := nil;
      end;
    3:
      if AChecked then
      begin
        DBPivotGrid.OnCustomDrawColumnHeader := DBPivotGridCustomDrawColumnHeader;
        DBPivotGrid.OnCustomDrawRowHeader := DBPivotGridCustomDrawRowHeader;
      end
      else
      begin
        DBPivotGrid.OnCustomDrawColumnHeader := nil;
        DBPivotGrid.OnCustomDrawRowHeader := nil;
      end;
  end;
  PivotGrid.LayoutChanged;
end;

procedure TfrmCustomDraw.DBPivotGridSelectionChanged(Sender: TObject);
begin
  inherited;
  PivotGrid.Invalidate;
end;

procedure TfrmCustomDraw.FormCreate(Sender: TObject);
var
  I: Integer;
  R, G, B: Byte;
  ACount: Integer;
  ACurrent: Integer;
begin
  inherited;
  FSupportFont := TFont.Create;
  FList := TcxPivotGridRecords.Create;
  FCurrentColorIndex := -1;
  R := GetRValue(ColorToRGB(clWindowText));
  G := GetGValue(ColorToRGB(clWindowText));
  B := GetBValue(ColorToRGB(clWindowText));
  I := 0;
  while R < 255 do
  begin
    SetLength(FColors, I + 1);
    FColors[I] := RGB(R, G, B);
    R := Min(R + 50, 255);
    Inc(I);
  end;
  ACurrent := I;
  ACount := Length(FColors) * 2 - 1;
  SetLength(FColors, ACount);
  for I := ACurrent to ACount - 1 do
    FColors[I] := FColors[ACount - 1 - I];
  PivotGrid.DoubleBuffered := True;
end;

procedure TfrmCustomDraw.FormDestroy(Sender: TObject);
begin
  SetLength(FColors, 0);
  FreeAndNil(FList);
  FreeAndNil(FSupportFont);
  inherited;
end;

procedure TfrmCustomDraw.tmrColorChangeTimer(Sender: TObject);
begin
  tmrColorChange.Enabled := False;
  Inc(FCurrentColorIndex);
  if FCurrentColorIndex >= Length(FColors) then
    FCurrentColorIndex := -1;
  if FCurrentColorIndex = -1 then
  begin
    FList.Clear;
    tmrColorChange.Interval := 1000;
  end
  else
    tmrColorChange.Interval := 200;
  PivotGrid.Invalidate;
  tmrColorChange.Enabled := True;
end;

end.
