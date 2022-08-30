unit CustomDrawTableViewDemoMain;

{$I cxVer.inc}

interface

uses
  Variants, Jpeg, 
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, cxControls, cxLookupGrid, cxLookupDBGrid, cxGridCustomTableView,
  cxGridTableView, cxGridDBTableView, cxGridLevel, cxGridCustomView, cxGrid,
  cxMaskEdit, StdCtrls, ExtCtrls, cxContainer, cxEdit, cxTextEdit, cxDropDownEdit,
  cxDBEdit, cxStyles, Menus, ActnList, ImgList, ComCtrls, cxLookAndFeels,
  cxGraphics, ToolWin, cxGridCustomPopupMenu, cxGridPopupMenu,
  DB, cxBlobEdit, cxCustomData, cxFilter, cxData, cxDBData, cxClasses, cxLookAndFeelPainters,
  cxDataStorage, cxTimeEdit, cxImageComboBox, DemoUtils, BaseForm, CarsDataForGrid, cxNavigator, cxGridCardView,
  cxCurrencyEdit;

type
  TCustomDrawTableViewDemoMainForm = class(TfmBaseForm)
    miOptions: TMenuItem;
    OpenDialog: TOpenDialog;
    FontDialog: TFontDialog;
    cxgCars: TcxGrid;
    tvCars: TcxGridDBTableView;
    lvCars: TcxGridLevel;
    lvOrders: TcxGridLevel;
    tvOrders: TcxGridDBTableView;
    miCustomDrawStylesEditor: TMenuItem;
    tvCarsTrademark: TcxGridDBColumn;
    tvCarsModel: TcxGridDBColumn;
    tvCarsHP: TcxGridDBColumn;
    tvCarsTorque: TcxGridDBColumn;
    tvCarsCyl: TcxGridDBColumn;
    tvCarsTransmissSpeedCount: TcxGridDBColumn;
    tvCarsMPG_City: TcxGridDBColumn;
    tvCarsMPG_Highway: TcxGridDBColumn;
    tvCarsCategory: TcxGridDBColumn;
    tvCarsDescription: TcxGridDBColumn;
    tvCarsPicture: TcxGridDBColumn;
    tvCarsPrice: TcxGridDBColumn;
    tvOrdersPurchaseDate: TcxGridDBColumn;
    tvOrdersTime: TcxGridDBColumn;
    tvOrdersPaymentType: TcxGridDBColumn;
    tvOrdersPaymentAmount: TcxGridDBColumn;
    tvOrdersQuantity: TcxGridDBColumn;
    imPaymentType: TImageList;
    imIndicatorImages: TImageList;
    procedure tvCarsCustomDrawCell(Sender: TcxCustomGridTableView;
      ACanvas: TcxCanvas; AViewInfo: TcxGridTableDataCellViewInfo;
      var ADone: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure tvOrdersCustomDrawCell(Sender: TcxCustomGridTableView;
      ACanvas: TcxCanvas; AViewInfo: TcxGridTableDataCellViewInfo;
      var ADone: Boolean);
    procedure miCustomDrawEditorClick(Sender: TObject);
    procedure tvCarsCustomDrawColumnHeader(Sender: TcxGridTableView;
      ACanvas: TcxCanvas; AViewInfo: TcxGridColumnHeaderViewInfo;
      var ADone: Boolean);
    procedure tvCarsCustomDrawFooterCell(Sender: TcxGridTableView;
      ACanvas: TcxCanvas; AViewInfo: TcxGridColumnHeaderViewInfo;
      var ADone: Boolean);
    procedure tvCarsCustomDrawPartBackground(Sender: TcxCustomGridTableView;
      ACanvas: TcxCanvas; AViewInfo: TcxCustomGridCellViewInfo;
      var ADone: Boolean);
    procedure tvOrdersCustomDrawColumnHeader(Sender: TcxGridTableView;
      ACanvas: TcxCanvas; AViewInfo: TcxGridColumnHeaderViewInfo;
      var ADone: Boolean);
    procedure tvOrdersCustomDrawFooterCell(Sender: TcxGridTableView;
      ACanvas: TcxCanvas; AViewInfo: TcxGridColumnHeaderViewInfo;
      var ADone: Boolean);
    procedure tvOrdersCustomDrawPartBackground(Sender: TcxCustomGridTableView;
      ACanvas: TcxCanvas; AViewInfo: TcxCustomGridCellViewInfo;
      var ADone: Boolean);
    procedure tvCarsCustomDrawGroupCell(Sender: TcxCustomGridTableView;
      ACanvas: TcxCanvas; AViewInfo: TcxGridTableCellViewInfo;
      var ADone: Boolean);
    procedure tvOrdersCustomDrawGroupCell(Sender: TcxCustomGridTableView;
      ACanvas: TcxCanvas; AViewInfo: TcxGridTableCellViewInfo;
      var ADone: Boolean);
    procedure tvCarsCustomDrawIndicatorCell(Sender: TcxGridTableView;
      ACanvas: TcxCanvas; AViewInfo: TcxCustomGridIndicatorItemViewInfo;
      var ADone: Boolean);
    procedure tvOrdersCustomDrawIndicatorCell(Sender: TcxGridTableView;
      ACanvas: TcxCanvas; AViewInfo: TcxCustomGridIndicatorItemViewInfo;
      var ADone: Boolean);
  private
    FCustomDrawingStyle: TCustomDrawingStyleArr;
    FColorScheme: TColorSchemes;
    FSkyBitmap, FEgyptBitmap, FMyFaceBitmap, FTileBitmap: TBitMap;
    FGridBrushMasterCell,
    FGridBrushMasterGroupCell,
    FGridBrushMasterFooterCell,
    FGridBrushMasterColumnHeader,
    FGridBrushMasterIndicatorCell,
    FGridBrushMasterPartBackground,

    FGridBrushDetailCell,
    FGridBrushDetailGroupCell,
    FGridBrushDetailFooterCell,
    FGridBrushDetailColumnHeader,
    FGridBrushDetailIndicatorCell,
    FGridBrushDetailPartBackground: TBrush;
    FBitMap: TBitMap;
    FFonts: TFonts;
    FBkImages: TBkImages;
    FIndicatorImageIndex: array [0..1] of Integer;
    function GetIndicatorImageIndex(AViewType: TViewType): Integer;
    procedure SetIndicatorImageIndex(AViewType: TViewType;
      const Value: Integer);
  protected
    procedure SetCustomDrawingStyle(AViewType: TViewType; ACustomDrawArea: TCustomDrawArea; const Value: TCustomDrawingStyle);
    function GetCustomDrawingStyle(AViewType: TViewType; ACustomDrawArea: TCustomDrawArea): TCustomDrawingStyle;

    procedure SetCustomDrawingStyles;

    function GetCustomBkImage(AViewType: TViewType; ACustomDrawArea: TCustomDrawArea): TBkImage;
    procedure SetCustomBkImage(AViewType: TViewType; ACustomDrawArea: TCustomDrawArea; AValue: TBkImage);

    function GetCustomColorScheme(AViewType: TViewType; ACustomDrawArea: TCustomDrawArea): TColorScheme;
    procedure SetCustomColorScheme(AViewType: TViewType; ACustomDrawArea: TCustomDrawArea; const Value: TColorScheme);

    procedure SetUserDefineBitmap(AViewType: TViewType; ACustomDrawArea: TCustomDrawArea; const Value: TBitmap);
    function GetFont(AViewType: TViewType; ACustomDrawArea: TCustomDrawArea): TFont;
    procedure SetFont(AViewType: TViewType; ACustomDrawArea: TCustomDrawArea; const Value: TFont);
    procedure InitFonts;
  public
    property CustomDrawingStyle[AViewType: TViewType; ACustomDrawArea: TCustomDrawArea]: TCustomDrawingStyle read GetCustomDrawingStyle write SetCustomDrawingStyle;
    property CustomBkImage[AViewType: TViewType; ACustomDrawArea: TCustomDrawArea]: TBkImage read GetCustomBkImage write SetCustomBkImage;
    property CustomColorScheme[AViewType: TViewType; ACustomDrawArea: TCustomDrawArea]: TColorScheme read GetCustomColorScheme write SetCustomColorScheme;
    property UserDefindedBitmap[AViewType: TViewType; ACustomDrawArea: TCustomDrawArea]: TBitmap write SetUserDefineBitmap;
    property Fonts[AViewType: TViewType; ACustomDrawArea: TCustomDrawArea]: TFont read GetFont write SetFont;
    property IndicatorImageIndex[AViewType: TViewType]:Integer read GetIndicatorImageIndex write SetIndicatorImageIndex;
  end;

var
  CustomDrawTableViewDemoMainForm: TCustomDrawTableViewDemoMainForm;

implementation

{$R *.dfm}

uses
  cxGridCommon, CustomDrawTableViewDemoData, CustomDrawTableViewDemoStylesEditor,
  AboutDemoForm;

procedure TCustomDrawTableViewDemoMainForm.InitFonts;
var
  I, J: Integer;
begin
  for I:=0 to High(FFonts) do
    for J:=0 to High(FFonts[I]) do
      FFonts[I, J] := TFont.Create;
end;

procedure TCustomDrawTableViewDemoMainForm.FormCreate(Sender: TObject);
begin
  InitFonts;

  FSkyBitmap := TBitMap.Create;

  LoadImageFromRes(FSkyBitmap, 'SKY');

  FEgyptBitmap := TBitmap.Create;
  LoadImageFromRes(FEgyptBitmap, 'EGYPT');

  FMyFaceBitmap := TBitmap.Create;
  LoadImageFromRes(FMyFaceBitmap, 'MYFACE');

  FTileBitmap := TBitmap.Create;
  LoadImageFromRes(FTileBitmap, 'TILE');

  FBitMap := TBitMap.Create;
  SetCustomDrawingStyles;
end;

procedure TCustomDrawTableViewDemoMainForm.FormDestroy(Sender: TObject);
var
  i, J: Integer;
begin
  for I:=0 to High(FFonts) do
    for J:=0 to High(FFonts[I]) do
      FFonts[I, J].Free;
  FBitMap.Free;
  FEgyptBitmap.Free;
  FMyFaceBitmap.Free;
  FTileBitmap.Free;
  FSkyBitmap.Free;

  FGridBrushMasterCell.Free;
  FGridBrushMasterGroupCell.Free;
  FGridBrushMasterFooterCell.Free;
  FGridBrushMasterColumnHeader.Free;
  FGridBrushMasterIndicatorCell.Free;
  FGridBrushMasterPartBackground.Free;

  FGridBrushDetailCell.Free;
  FGridBrushDetailGroupCell.Free;
  FGridBrushDetailFooterCell.Free;
  FGridBrushDetailColumnHeader.Free;
  FGridBrushDetailIndicatorCell.Free;
  FGridBrushDetailPartBackground.Free;
end;

procedure TCustomDrawTableViewDemoMainForm.tvCarsCustomDrawCell(
  Sender: TcxCustomGridTableView; ACanvas: TcxCanvas;
  AViewInfo: TcxGridTableDataCellViewInfo; var ADone: Boolean);
var
  ARec: TRect;
  ATextToDraw, val: String;
  AIsVertical: Boolean;
begin
  ATextToDraw := AViewInfo.GridRecord.DisplayTexts[AViewInfo.Item.Index];
  ARec := AViewInfo.Bounds;
  ACanvas.Canvas.Font.Assign(FFonts[Integer(vtMaster), Integer(cdaCell)]);
  case FCustomDrawingStyle[Integer(vtMaster), Integer(cdaCell)] of
    cdsBkImage:
    begin
      ACanvas.Brush.Bitmap := nil;
      ACanvas.Brush.Assign(FGridBrushMasterCell);
      ACanvas.FillRect(ARec);
      ACanvas.Brush.Bitmap := nil;
    end;
    cdsGradient:
    begin
      AIsVertical := (FColorScheme[Integer(vtMaster), Integer(cdaCell)] = csGrey) or (FColorScheme[Integer(vtMaster), Integer(cdaCell)] = csGold);
      DrawGradient(ACanvas.Canvas, ARec, ColorScheme[Integer(FColorScheme[Integer(vtMaster), Integer(cdaCell)]), 1], ColorScheme[Integer(FColorScheme[Integer(vtMaster), Integer(cdaCell)]), 0], 40, AIsVertical)
    end;
    cdsDependOnData:
    begin
      ACanvas.Canvas.Brush.Style := bsSolid;
      ACanvas.Canvas.Brush.Color := clBlueLight;

      val := VarAsType(AViewInfo.GridRecord.DisplayTexts[tvCarsCategory.Index], varString);
      if val = 'SPORTS' then
        ACanvas.Canvas.Font.Color := clRed
      else
      if val = 'SALOON' then
        ACanvas.Canvas.Font.Color := clBlue
      else
      if val = 'TRUCK' then
        ACanvas.Canvas.Font.Color := clGreen;
      ACanvas.Canvas.FillRect(ARec);
    end;
  end;

  SetBkMode(ACanvas.Canvas.Handle, TRANSPARENT);
  ADone := not (FCustomDrawingStyle[Integer(vtMaster), Integer(cdaCell)] in [cdsDefaultDrawing, cdsDependOnData]);
  if ADone then
  begin
    InflateRect(ARec, -cxGridCellTextOffset, -cxGridCellTextOffset);
    ACanvas.DrawText(ATextToDraw, ARec, 0);
  end;
end;

procedure TCustomDrawTableViewDemoMainForm.tvCarsCustomDrawGroupCell(
  Sender: TcxCustomGridTableView; ACanvas: TcxCanvas;
  AViewInfo: TcxGridTableCellViewInfo; var ADone: Boolean);
var
  ARec: TRect;
  ATextToDraw: String;
  AIsVertical: Boolean;
begin
  ATextToDraw := AViewInfo.Text;
  ARec := AViewInfo.Bounds;
  ACanvas.Canvas.Font.Assign(FFonts[Integer(vtMaster), Integer(cdaGroupCell)]);
  case FCustomDrawingStyle[Integer(vtMaster), Integer(cdaGroupCell)] of
    cdsBkImage:
    begin
      ACanvas.Brush.Bitmap := nil;
      ACanvas.Brush.Assign(FGridBrushMasterGroupCell);
      ACanvas.FillRect(ARec);
      ACanvas.Brush.Bitmap := nil;
    end;
    cdsGradient:
    begin
      AIsVertical := (FColorScheme[Integer(vtMaster), Integer(cdaGroupCell)] = csGrey) or (FColorScheme[Integer(vtMaster), Integer(cdaGroupCell)] = csGold);
      DrawGradient(ACanvas.Canvas, ARec, ColorScheme[Integer(FColorScheme[Integer(vtMaster), Integer(cdaGroupCell)]), 1], ColorScheme[Integer(FColorScheme[Integer(vtMaster), Integer(cdaGroupCell)]), 0], 40, AIsVertical)
    end;
  end;

  ADone := not (FCustomDrawingStyle[Integer(vtMaster), Integer(cdaGroupCell)] in [cdsDefaultDrawing]);
  if ADone then
  begin
    SetBkMode(ACanvas.Canvas.Handle, TRANSPARENT);
    ARec.Left := ARec.Left + (AViewInfo.RecordViewInfo.ExpandButtonBounds.Right - AViewInfo.RecordViewInfo.ExpandButtonBounds.Left) + 10;
    ARec.Top := ARec.Top + cxGridCellTextOffset;
    ACanvas.DrawText(ATextToDraw, ARec, 0);
  end;
end;

procedure TCustomDrawTableViewDemoMainForm.tvCarsCustomDrawIndicatorCell(
  Sender: TcxGridTableView; ACanvas: TcxCanvas;
  AViewInfo: TcxCustomGridIndicatorItemViewInfo; var ADone: Boolean);
var
  ARec: TRect;
  AIsVertical: Boolean;
  AIndicatorKind: TcxIndicatorKind;
  X, Y: Integer;
begin
  ARec := AViewInfo.Bounds;
  ACanvas.Canvas.Font.Assign(FFonts[Integer(vtMaster), Integer(cdaIndicatorCell)]);

  if not (AViewInfo is TcxGridIndicatorRowItemViewInfo) then
    AIndicatorKind := ikNone
  else
    AIndicatorKind := TcxGridIndicatorRowItemViewInfo(AViewInfo).IndicatorKind;

  case FCustomDrawingStyle[Integer(vtMaster), Integer(cdaIndicatorCell)] of
    cdsBkImage:
    begin
      ACanvas.Brush.Bitmap := nil;
      ACanvas.Brush.Assign(FGridBrushMasterIndicatorCell);
      ACanvas.FillRect(ARec);
      ACanvas.Brush.Bitmap := nil;
    end;
    cdsGradient:
    begin
      AIsVertical := (FColorScheme[Integer(vtMaster), Integer(cdaIndicatorCell)] = csGrey) or (FColorScheme[Integer(vtMaster), Integer(cdaIndicatorCell)] = csGold);
      DrawGradient(ACanvas.Canvas, ARec, ColorScheme[Integer(FColorScheme[Integer(vtMaster), Integer(cdaGroupCell)]), 1], ColorScheme[Integer(FColorScheme[Integer(vtMaster), Integer(cdaIndicatorCell)]), 0], 40, AIsVertical)
    end;
  end;
  if AIndicatorKind <> ikNone then
  begin
    with imIndicatorImages, ARec do
    begin
      X := (Left + Right - Width) div 2;
      Y := (Top + Bottom - Height) div 2;
    end;
    imIndicatorImages.Draw(ACanvas.Canvas, X, Y, IndicatorImageIndex[vtMaster], True);
  end;

  ADone := not (FCustomDrawingStyle[Integer(vtMaster), Integer(cdaIndicatorCell)] in [cdsDefaultDrawing]);
end;

procedure TCustomDrawTableViewDemoMainForm.FormShow(Sender: TObject);
begin
  CustomDrawTableViewDemoStylesEditorForm.Show;
end;

procedure TCustomDrawTableViewDemoMainForm.tvOrdersCustomDrawCell(
  Sender: TcxCustomGridTableView; ACanvas: TcxCanvas;
  AViewInfo: TcxGridTableDataCellViewInfo; var ADone: Boolean);
var
  ARec: TRect;
  ATextToDraw: String;
  AIsVertical: Boolean;
  val: String;
begin
  ATextToDraw := AViewInfo.GridRecord.DisplayTexts[AViewInfo.Item.Index];
  ARec := AViewInfo.Bounds;
  ACanvas.Canvas.Font.Assign(FFonts[Integer(vtDetail), Integer(cdaCell)]);
  case FCustomDrawingStyle[Integer(vtDetail), Integer(cdaCell)] of
    cdsBkImage:
    begin
      ACanvas.Brush.Bitmap := nil;
      ACanvas.Brush.Assign(FGridBrushDetailCell);
      ACanvas.Canvas.FillRect(ARec);
      ACanvas.Brush.Bitmap := nil;
    end;
    cdsGradient:
    begin
      AIsVertical := (FColorScheme[Integer(vtDetail), Integer(cdaCell)] = csGrey) or (FColorScheme[Integer(vtDetail), Integer(cdaCell)] = csGold);
      DrawGradient(ACanvas.Canvas, ARec, ColorScheme[Integer(FColorScheme[Integer(vtDetail), Integer(cdaCell)]), 1], ColorScheme[Integer(FColorScheme[Integer(vtDetail), Integer(cdaCell)]), 0], 40, AIsVertical)
    end;
    cdsDependOnData:
    begin
      ACanvas.Canvas.Brush.Style := bsSolid;
      ACanvas.Canvas.Brush.Color := clBlueLight;

      val := VarAsType(AViewInfo.GridRecord.DisplayTexts[tvOrdersPaymentType.Index], varString);
      if val = 'Master Card' then
        ACanvas.Canvas.Font.Color := clRed
      else
      if val = 'American Express' then
        ACanvas.Canvas.Font.Color := clBlue
      else
      if val = 'Cash' then
        ACanvas.Canvas.Font.Color := clGreen;
      if val = 'Visa Card' then
        ACanvas.Canvas.Font.Color := clFuchsia;
      ACanvas.Canvas.FillRect(ARec);
    end;
  end;

  SetBkMode(ACanvas.Canvas.Handle, TRANSPARENT);

  ADone := not (FCustomDrawingStyle[Integer(vtDetail), Integer(cdaCell)] in [cdsDefaultDrawing, cdsDependOnData]);
  if ADone then
  begin
    InflateRect(ARec, -cxGridCellTextOffset, -cxGridCellTextOffset);
    ACanvas.DrawText(ATextToDraw, ARec, 0);
  end;
end;

procedure TCustomDrawTableViewDemoMainForm.tvOrdersCustomDrawGroupCell(
  Sender: TcxCustomGridTableView; ACanvas: TcxCanvas;
  AViewInfo: TcxGridTableCellViewInfo; var ADone: Boolean);
var
  ARec: TRect;
  ATextToDraw: String;
  AIsVertical: Boolean;
begin
  ATextToDraw := AViewInfo.Text;
  ARec := AViewInfo.Bounds;
  ACanvas.Canvas.Font.Assign(FFonts[Integer(vtDetail), Integer(cdaGroupCell)]);
  case FCustomDrawingStyle[Integer(vtDetail), Integer(cdaGroupCell)] of
    cdsBkImage:
    begin
      ACanvas.Brush.Bitmap := nil;
      ACanvas.Brush.Assign(FGridBrushDetailGroupCell);
      ACanvas.Canvas.FillRect(ARec);
      ACanvas.Brush.Bitmap := nil;
    end;
    cdsGradient:
    begin
      AIsVertical := (FColorScheme[Integer(vtDetail), Integer(cdaGroupCell)] = csGrey) or (FColorScheme[Integer(vtDetail), Integer(cdaGroupCell)] = csGold);
      DrawGradient(ACanvas.Canvas, ARec, ColorScheme[Integer(FColorScheme[Integer(vtDetail), Integer(cdaGroupCell)]), 1], ColorScheme[Integer(FColorScheme[Integer(vtDetail), Integer(cdaGroupCell)]), 0], 40, AIsVertical)
    end;
  end;

  SetBkMode(ACanvas.Canvas.Handle, TRANSPARENT);

  ADone := not (FCustomDrawingStyle[Integer(vtDetail), Integer(cdaGroupCell)] in [cdsDefaultDrawing]);
  if ADone then
  begin
    ARec.Left := ARec.Left + (AViewInfo.RecordViewInfo.ExpandButtonBounds.Right - AViewInfo.RecordViewInfo.ExpandButtonBounds.Left) + 10;
    ARec.Top := ARec.Top + cxGridCellTextOffset;
    ACanvas.DrawText(ATextToDraw, ARec, 0);
  end;
end;

procedure TCustomDrawTableViewDemoMainForm.tvOrdersCustomDrawIndicatorCell(
  Sender: TcxGridTableView; ACanvas: TcxCanvas;
  AViewInfo: TcxCustomGridIndicatorItemViewInfo; var ADone: Boolean);
var
  ARec: TRect;
  AIsVertical: Boolean;
  AIndicatorKind: TcxIndicatorKind;
  X, Y: Integer;
begin
  ARec := AViewInfo.Bounds;
  ACanvas.Canvas.Font.Assign(FFonts[Integer(vtDetail), Integer(cdaIndicatorCell)]);

  if not (AViewInfo is TcxGridIndicatorRowItemViewInfo) then
    AIndicatorKind := ikNone
  else
    AIndicatorKind := TcxGridIndicatorRowItemViewInfo(AViewInfo).IndicatorKind;

  case FCustomDrawingStyle[Integer(vtDetail), Integer(cdaIndicatorCell)] of
    cdsBkImage:
    begin
      ACanvas.Brush.Bitmap := nil;
      ACanvas.Brush.Assign(FGridBrushDetailIndicatorCell);
      ACanvas.FillRect(ARec);
      ACanvas.Brush.Bitmap := nil;
    end;
    cdsGradient:
    begin
      AIsVertical := (FColorScheme[Integer(vtDetail), Integer(cdaIndicatorCell)] = csGrey) or (FColorScheme[Integer(vtDetail), Integer(cdaIndicatorCell)] = csGold);
      DrawGradient(ACanvas.Canvas, ARec, ColorScheme[Integer(FColorScheme[Integer(vtDetail), Integer(cdaGroupCell)]), 1], ColorScheme[Integer(FColorScheme[Integer(vtDetail), Integer(cdaIndicatorCell)]), 0], 40, AIsVertical)
    end;
  end;
  if AIndicatorKind <> ikNone then
  begin
    with imIndicatorImages, ARec do
    begin
      X := (Left + Right - Width) div 2;
      Y := (Top + Bottom - Height) div 2;
    end;
    imIndicatorImages.Draw(ACanvas.Canvas, X, Y, IndicatorImageIndex[vtDetail], True);
  end;

  ADone := not (FCustomDrawingStyle[Integer(vtDetail), Integer(cdaIndicatorCell)] in [cdsDefaultDrawing]);
end;

procedure TCustomDrawTableViewDemoMainForm.miCustomDrawEditorClick(
  Sender: TObject);
begin
  CustomDrawTableViewDemoStylesEditorForm.Show;
end;

procedure TCustomDrawTableViewDemoMainForm.SetCustomDrawingStyle(
  AViewType: TViewType; ACustomDrawArea: TCustomDrawArea;
  const Value: TCustomDrawingStyle);
begin
  if FCustomDrawingStyle[Integer(AViewType), Integer(ACustomDrawArea)] <> Value then
  begin
    FCustomDrawingStyle[Integer(AViewType), Integer(ACustomDrawArea)] := Value;
    case AViewType of
      vtMaster:
      begin
        tvCars.LayoutChanged(False);
        tvCars.Painter.Invalidate;
      end;
      vtDetail:
      begin
        tvOrders.LayoutChanged(False);
        tvOrders.Painter.Invalidate;
      end;
    end;
  end
end;

function TCustomDrawTableViewDemoMainForm.GetCustomDrawingStyle(
  AViewType: TViewType;
  ACustomDrawArea: TCustomDrawArea): TCustomDrawingStyle;
begin
  Result := FCustomDrawingStyle[Integer(AViewType), Integer(ACustomDrawArea)];
end;

procedure SetStyleSolidColorBitmap(AStyle: TcxStyle);
var
  B: TBitmap;
begin
  B := TBitmap.Create;
  try
    B.Width  := 128;
    B.Height := 32;
    with AStyle do
    begin
      B.Canvas.Brush.Color := Color;
      B.Canvas.FillRect(Rect(0, 0, B.Width, B.Height));
      Bitmap := B;
    end;
  finally
    B.Free;
  end;
end;

procedure TCustomDrawTableViewDemoMainForm.SetCustomDrawingStyles;
var
  i, j: Integer;
begin
  FGridBrushMasterCell := TBrush.Create;
  FGridBrushMasterGroupCell := TBrush.Create;
  FGridBrushMasterFooterCell := TBrush.Create;
  FGridBrushMasterColumnHeader := TBrush.Create;
  FGridBrushMasterIndicatorCell := TBrush.Create;
  FGridBrushMasterPartBackground := TBrush.Create;

  FGridBrushDetailCell := TBrush.Create;
  FGridBrushDetailGroupCell := TBrush.Create;
  FGridBrushDetailFooterCell := TBrush.Create;
  FGridBrushDetailColumnHeader := TBrush.Create;
  FGridBrushDetailIndicatorCell := TBrush.Create;
  FGridBrushDetailPartBackground := TBrush.Create;
  FIndicatorImageIndex[Integer(vtMaster)] := 0;
  FIndicatorImageIndex[Integer(vtDetail)] := 0;
  for i:=0 to 1 do
    for j:=0 to 5 do
      FBkImages[1, j] := bkiTile;

  FGridBrushMasterCell.Bitmap := FTileBitmap;
  FGridBrushMasterGroupCell.Bitmap := FTileBitmap;
  FGridBrushMasterColumnHeader.Bitmap := FTileBitmap;
  FGridBrushMasterFooterCell.Bitmap := FTileBitmap;
  FGridBrushMasterIndicatorCell.Bitmap := FTileBitmap;
  FGridBrushMasterPartBackground.Bitmap := FTileBitmap;
  FGridBrushDetailCell.Bitmap := FTileBitmap;
  FGridBrushDetailGroupCell.Bitmap := FTileBitmap;
  FGridBrushDetailColumnHeader.Bitmap := FTileBitmap;
  FGridBrushDetailFooterCell.Bitmap := FTileBitmap;
  FGridBrushDetailIndicatorCell.Bitmap := FTileBitmap;
  FGridBrushDetailPartBackground.Bitmap := FTileBitmap;

  FColorScheme[Integer(vtMaster), 0] := csBlue;
  FColorScheme[Integer(vtDetail), 1] := csBlue;
  for i:=0 to 1 do
    for j:=0 to 5 do
      FCustomDrawingStyle[i, j] := cdsGradient;
      
  SetStyleSolidColorBitmap(tvOrders.Styles.Content);
  SetStyleSolidColorBitmap(tvOrders.Styles.Group);
  SetStyleSolidColorBitmap(tvOrders.Styles.Header);
  SetStyleSolidColorBitmap(tvOrders.Styles.Footer);
  SetStyleSolidColorBitmap(tvOrders.Styles.Indicator);
  SetStyleSolidColorBitmap(tvOrders.Styles.Background);
end;

procedure TCustomDrawTableViewDemoMainForm.tvCarsCustomDrawColumnHeader(
  Sender: TcxGridTableView; ACanvas: TcxCanvas;
  AViewInfo: TcxGridColumnHeaderViewInfo; var ADone: Boolean);
var
  ARec: TRect;
  ATextToDraw: String;
  AIsVertical: Boolean;
begin
  ATextToDraw := AViewInfo.Text;
  ARec := AViewInfo.Bounds;
  ACanvas.Canvas.Font.Assign(FFonts[Integer(vtMaster), Integer(cdaColumnHeader)]);
  case FCustomDrawingStyle[Integer(vtMaster), Integer(cdaColumnHeader)] of
    cdsBkImage:
    begin
      ACanvas.Brush.Bitmap := nil;
      ACanvas.Brush.Assign(FGridBrushMasterColumnHeader);
      ACanvas.FillRect(ARec);
      ACanvas.Brush.Bitmap := nil;
    end;
    cdsGradient:
    begin
      AIsVertical := (FColorScheme[Integer(vtMaster), Integer(cdaColumnHeader)] = csGrey) or (FColorScheme[Integer(vtMaster), Integer(cdaColumnHeader)] = csGold);
      DrawGradient(ACanvas.Canvas, ARec, ColorScheme[Integer(FColorScheme[Integer(vtMaster), Integer(cdaColumnHeader)]), 1], ColorScheme[Integer(FColorScheme[Integer(vtMaster), Integer(cdaColumnHeader)]), 0], 40, AIsVertical)
    end;
  end;

  InflateRect(ARec, -cxGridCellTextOffset, -cxGridCellTextOffset);
  SetBkMode(ACanvas.Canvas.Handle, TRANSPARENT);
  ACanvas.DrawText(ATextToDraw, ARec, 0);

  ADone := FCustomDrawingStyle[Integer(vtMaster), Integer(cdaColumnHeader)] <> cdsDefaultDrawing;
end;

procedure TCustomDrawTableViewDemoMainForm.tvCarsCustomDrawFooterCell(
  Sender: TcxGridTableView; ACanvas: TcxCanvas;
  AViewInfo: TcxGridColumnHeaderViewInfo; var ADone: Boolean);
var
  ARec: TRect;
  ATextToDraw: String;
  AIsVertical: Boolean;
begin
  ATextToDraw := AViewInfo.Text;
  ARec := AViewInfo.Bounds;
  ACanvas.Canvas.Font.Assign(FFonts[Integer(vtMaster), Integer(cdaFooterCell)]);
  case FCustomDrawingStyle[Integer(vtMaster), Integer(cdaFooterCell)] of
    cdsBkImage:
    begin
      ACanvas.Brush.Bitmap := nil;
      ACanvas.Brush.Assign(FGridBrushMasterFooterCell);
      ACanvas.Canvas.FillRect(ARec);
      ACanvas.Brush.Bitmap := nil;
    end;
    cdsGradient:
    begin
      AIsVertical := (FColorScheme[Integer(vtMaster), Integer(cdaFooterCell)] = csGrey) or (FColorScheme[Integer(vtMaster), Integer(cdaFooterCell)] = csGold);
      DrawGradient(ACanvas.Canvas, ARec, ColorScheme[Integer(FColorScheme[Integer(vtMaster),
        Integer(cdaFooterCell)]), 1], ColorScheme[Integer(FColorScheme[Integer(vtMaster), Integer(cdaFooterCell)]), 0], 40, AIsVertical)
    end;
  end;

  InflateRect(ARec, -cxGridCellTextOffset, -cxGridCellTextOffset);
  SetBkMode(ACanvas.Canvas.Handle, TRANSPARENT);
  ACanvas.DrawText(ATextToDraw, ARec, 0);

  ADone := FCustomDrawingStyle[Integer(vtMaster), Integer(cdaFooterCell)] <> cdsDefaultDrawing;
end;

procedure TCustomDrawTableViewDemoMainForm.tvCarsCustomDrawPartBackground(
  Sender: TcxCustomGridTableView; ACanvas: TcxCanvas;
  AViewInfo: TcxCustomGridCellViewInfo; var ADone: Boolean);
var
  ARec: TRect;
  AIsVertical: Boolean;
begin
  ARec := AViewInfo.Bounds;
  ACanvas.Canvas.Font.Assign(FFonts[Integer(vtMaster), Integer(cdaPartBackground)]);
  case FCustomDrawingStyle[Integer(vtMaster), Integer(cdaPartBackground)] of
    cdsBkImage:
    begin
      ACanvas.Brush.Bitmap := nil;
      ACanvas.Brush.Assign(FGridBrushMasterPartBackground);
      ACanvas.Canvas.FillRect(ARec);
      ACanvas.Brush.Bitmap := nil;
    end;
    cdsGradient:
    begin
      AIsVertical := (FColorScheme[Integer(vtMaster), Integer(cdaPartBackground)] = csGrey) or
        (FColorScheme[Integer(vtMaster), Integer(cdaPartBackground)] = csGold);
      DrawGradient(ACanvas.Canvas, ARec, ColorScheme[Integer(FColorScheme[Integer(vtMaster), Integer(cdaPartBackground)]), 1],
         ColorScheme[Integer(FColorScheme[Integer(vtMaster), Integer(cdaPartBackground)]), 0], 40, AIsVertical)
    end;
  end;

  ADone := FCustomDrawingStyle[Integer(vtMaster), Integer(cdaPartBackground)] <> cdsDefaultDrawing;
end;

procedure TCustomDrawTableViewDemoMainForm.tvOrdersCustomDrawColumnHeader(
  Sender: TcxGridTableView; ACanvas: TcxCanvas;
  AViewInfo: TcxGridColumnHeaderViewInfo; var ADone: Boolean);
var
  ARec: TRect;
  ATextToDraw: String;
  AIsVertical: Boolean;
begin
  ATextToDraw := AViewInfo.Text;
  ARec := AViewInfo.Bounds;
  ACanvas.Canvas.Font.Assign(FFonts[Integer(vtDetail), Integer(cdaColumnHeader)]);
  case FCustomDrawingStyle[Integer(vtDetail), Integer(cdaColumnHeader)] of
    cdsBkImage:
    begin
      ACanvas.Brush.Bitmap := nil;
      ACanvas.Brush.Assign(FGridBrushDetailColumnHeader);
      ACanvas.Canvas.FillRect(ARec);
      ACanvas.Brush.Bitmap := nil;
    end;
    cdsGradient:
    begin
      AIsVertical := (FColorScheme[Integer(vtDetail), Integer(cdaColumnHeader)] = csGrey) or
        (FColorScheme[Integer(vtDetail), Integer(cdaColumnHeader)] = csGold);
      DrawGradient(ACanvas.Canvas, ARec, ColorScheme[Integer(FColorScheme[Integer(vtDetail), Integer(cdaColumnHeader)]), 1],
        ColorScheme[Integer(FColorScheme[Integer(vtDetail), Integer(cdaColumnHeader)]), 0], 40, AIsVertical);
    end;
    cdsDependOnData:
    begin
      ACanvas.Canvas.Brush.Style := bsSolid;
      ACanvas.Canvas.Brush.Color := clBlueLight;
      ACanvas.Canvas.FillRect(ARec);
    end;
  end;

  InflateRect(ARec, -cxGridCellTextOffset, -cxGridCellTextOffset);
  SetBkMode(ACanvas.Canvas.Handle, TRANSPARENT);
  ACanvas.DrawText(ATextToDraw, ARec, 0);

  ADone := FCustomDrawingStyle[Integer(vtDetail), Integer(cdaColumnHeader)] <> cdsDefaultDrawing;
end;

procedure TCustomDrawTableViewDemoMainForm.tvOrdersCustomDrawFooterCell(
  Sender: TcxGridTableView; ACanvas: TcxCanvas;
  AViewInfo: TcxGridColumnHeaderViewInfo; var ADone: Boolean);
var
  ARec: TRect;
  ATextToDraw: String;
  AIsVertical: Boolean;
begin
  ATextToDraw := AViewInfo.Text;
  ARec := AViewInfo.Bounds;
  ACanvas.Canvas.Font.Assign(FFonts[Integer(vtDetail), Integer(cdaFooterCell)]);
  case FCustomDrawingStyle[Integer(vtDetail), Integer(cdaFooterCell)] of
    cdsBkImage:
    begin
      ACanvas.Brush.Bitmap := nil;
      ACanvas.Brush.Assign(FGridBrushDetailFooterCell);
      ACanvas.Canvas.FillRect(ARec);
      ACanvas.Brush.Bitmap := nil;
    end;
    cdsGradient:
    begin
      AIsVertical := (FColorScheme[Integer(vtDetail), Integer(cdaFooterCell)] = csGrey) or
        (FColorScheme[Integer(vtDetail), Integer(cdaFooterCell)] = csGold);
      DrawGradient(ACanvas.Canvas, ARec, ColorScheme[Integer(FColorScheme[Integer(vtDetail), Integer(cdaFooterCell)]), 1],
        ColorScheme[Integer(FColorScheme[Integer(vtDetail), Integer(cdaFooterCell)]), 0], 40, AIsVertical)
    end;
  end;

  InflateRect(ARec, -cxGridCellTextOffset, -cxGridCellTextOffset);
  SetBkMode(ACanvas.Canvas.Handle, TRANSPARENT);
  ACanvas.DrawText(ATextToDraw, ARec, 0);

  ADone := FCustomDrawingStyle[Integer(vtDetail), Integer(cdaFooterCell)] <> cdsDefaultDrawing;
end;

procedure TCustomDrawTableViewDemoMainForm.tvOrdersCustomDrawPartBackground(
  Sender: TcxCustomGridTableView; ACanvas: TcxCanvas;
  AViewInfo: TcxCustomGridCellViewInfo; var ADone: Boolean);
var
  ARec: TRect;
  AIsVertical: Boolean;
begin
  ARec := AViewInfo.Bounds;
  ACanvas.Canvas.Font.Assign(FFonts[Integer(vtDetail), Integer(cdaPartBackground)]);
  case FCustomDrawingStyle[Integer(vtDetail), Integer(cdaPartBackground)] of
    cdsBkImage:
    begin
      ACanvas.Brush.Bitmap := nil;
      ACanvas.Brush.Assign(FGridBrushDetailPartBackground);
      ACanvas.Canvas.FillRect(ARec);
      ACanvas.Brush.Bitmap := nil;
    end;
    cdsGradient:
    begin
      AIsVertical := (FColorScheme[Integer(vtDetail), Integer(cdaPartBackground)] = csGrey) or
        (FColorScheme[Integer(vtDetail), Integer(cdaPartBackground)] = csGold);
      DrawGradient(ACanvas.Canvas, ARec, ColorScheme[Integer(FColorScheme[Integer(vtDetail), Integer(cdaPartBackground)]),1],
        ColorScheme[Integer(FColorScheme[Integer(vtDetail), Integer(cdaPartBackground)]), 0], 40, AIsVertical)
    end;
  end;

  ADone := FCustomDrawingStyle[Integer(vtDetail), Integer(cdaPartBackground)] <> cdsDefaultDrawing;
end;

function TCustomDrawTableViewDemoMainForm.GetCustomBkImage(
  AViewType: TViewType; ACustomDrawArea: TCustomDrawArea): TBkImage;
begin
  Result := FBkImages[Integer(AViewType), Integer(ACustomDrawArea)];
end;

procedure TCustomDrawTableViewDemoMainForm.SetCustomBkImage(
  AViewType: TViewType; ACustomDrawArea: TCustomDrawArea; AValue: TBkImage);
  function GetImage: TBitMap;
  begin
    Result := nil;
    case AValue of
      bkiTile: Result := FTileBitmap;
      bkiSky: Result := FSkyBitmap;
      bkiEgypt: Result := FEgyptBitmap;
      bkiMyFace: Result := FMyFaceBitmap;
    end;
  end;
begin
  if AValue <> bkiUserDefined then
  begin
    if FBkImages[Integer(AViewType), Integer(ACustomDrawArea)] <> AValue then
    begin
      FBkImages[Integer(AViewType), Integer(ACustomDrawArea)] := AValue;
      case AViewType of
        vtMaster:
        begin
          case ACustomDrawArea of
            cdaCell: FGridBrushMasterCell.Bitmap := GetImage;
            cdaGroupCell: FGridBrushMasterGroupCell.Bitmap := GetImage;
            cdaColumnHeader: FGridBrushMasterColumnHeader.Bitmap := GetImage;
            cdaFooterCell: FGridBrushMasterFooterCell.Bitmap := GetImage;
            cdaIndicatorCell: FGridBrushMasterIndicatorCell.Bitmap := GetImage;
            cdaPartBackGround: FGridBrushMasterPartBackGround.Bitmap := GetImage;
          end;
          tvCars.LayoutChanged(False);
          tvCars.Painter.Invalidate;
        end;
        vtDetail:
        begin
          case ACustomDrawArea of
            cdaCell: FGridBrushDetailCell.Bitmap := GetImage;
            cdaGroupCell: FGridBrushDetailGroupCell.Bitmap := GetImage;
            cdaColumnHeader: FGridBrushDetailColumnHeader.Bitmap := GetImage;
            cdaFooterCell: FGridBrushDetailFooterCell.Bitmap := GetImage;
            cdaIndicatorCell: FGridBrushDetailIndicatorCell.Bitmap := GetImage;
            cdaPartBackGround: FGridBrushDetailPartBackGround.Bitmap := GetImage;
          end;
          tvOrders.LayoutChanged(False);
          tvOrders.Painter.Invalidate;
        end;
      end;
    end;
  end
  else
  begin
    FBkImages[Integer(AViewType), Integer(ACustomDrawArea)] := AValue;
    case AViewType of
      vtMaster:
      begin
        tvCars.LayoutChanged(False);
        tvCars.Painter.Invalidate;
      end;
      vtDetail:
      begin
        tvOrders.LayoutChanged(False);
        tvOrders.Painter.Invalidate;
      end;
    end;
  end;
end;

function TCustomDrawTableViewDemoMainForm.GetCustomColorScheme(
  AViewType: TViewType; ACustomDrawArea: TCustomDrawArea): TColorScheme;
begin
  Result := FColorScheme[Integer(AViewType), Integer(ACustomDrawArea)];
end;

procedure TCustomDrawTableViewDemoMainForm.SetCustomColorScheme(
  AViewType: TViewType; ACustomDrawArea: TCustomDrawArea;
  const Value: TColorScheme);
begin
  if FColorScheme[Integer(AViewType), Integer(ACustomDrawArea)] <> Value then
  begin
    FColorScheme[Integer(AViewType), Integer(ACustomDrawArea)] := Value;
    case AViewType of
      vtMaster:
      begin
        tvCars.LayoutChanged(False);
        tvCars.Painter.Invalidate;
      end;
      vtDetail:
      begin
        tvOrders.LayoutChanged(False);
        tvOrders.Painter.Invalidate;
      end;
    end;
  end;
end;

procedure TCustomDrawTableViewDemoMainForm.SetUserDefineBitmap(
  AViewType: TViewType; ACustomDrawArea: TCustomDrawArea;
  const Value: TBitmap);
begin
  case AViewType of
    vtMaster:
    begin
      case ACustomDrawArea of
        cdaCell: FGridBrushMasterCell.Bitmap := Value;
        cdaGroupCell: FGridBrushMasterGroupCell.Bitmap := Value;
        cdaColumnHeader: FGridBrushMasterColumnHeader.Bitmap := Value;
        cdaFooterCell: FGridBrushMasterFooterCell.Bitmap := Value;
        cdaIndicatorCell: FGridBrushMasterIndicatorCell.Bitmap := Value;
        cdaPartBackGround: FGridBrushMasterPartBackGround.Bitmap := Value;
      end;
    end;
    vtDetail:
    begin
      case ACustomDrawArea of
        cdaCell: FGridBrushDetailCell.Bitmap := Value;
        cdaGroupCell: FGridBrushDetailGroupCell.Bitmap := Value;
        cdaColumnHeader: FGridBrushDetailColumnHeader.Bitmap := Value;
        cdaFooterCell: FGridBrushDetailFooterCell.Bitmap := Value;
        cdaIndicatorCell: FGridBrushDetailIndicatorCell.Bitmap := Value;
        cdaPartBackGround: FGridBrushDetailPartBackGround.Bitmap := Value;
      end;
    end;
  end;
end;

function TCustomDrawTableViewDemoMainForm.GetFont(AViewType: TViewType;
  ACustomDrawArea: TCustomDrawArea): TFont;
begin
  Result := FFonts[Integer(AViewType), Integer(ACustomDrawArea)];
end;

procedure TCustomDrawTableViewDemoMainForm.SetFont(AViewType: TViewType;
  ACustomDrawArea: TCustomDrawArea; const Value: TFont);
begin
  FFonts[Integer(AViewType), Integer(ACustomDrawArea)].Assign(Value);
  case AViewType of
    vtMaster:
    begin
      tvCars.LayoutChanged(False);
      tvCars.Painter.Invalidate;
    end;
    vtDetail:
    begin
      tvOrders.LayoutChanged(False);
      tvOrders.Painter.Invalidate;
    end;
  end;
end;

function TCustomDrawTableViewDemoMainForm.GetIndicatorImageIndex(
  AViewType: TViewType): Integer;
begin
  Result := FIndicatorImageIndex[Integer(AViewType)];
end;

procedure TCustomDrawTableViewDemoMainForm.SetIndicatorImageIndex(
  AViewType: TViewType; const Value: Integer);
begin
  if FIndicatorImageIndex[Integer(AViewType)] <> Value then
  begin
    FIndicatorImageIndex[Integer(AViewType)] := Value;
    case AViewType of
      vtMaster:
      begin
        tvCars.LayoutChanged(False);
        tvCars.Painter.Invalidate;
      end;
      vtDetail:
      begin
        tvOrders.LayoutChanged(False);
        tvOrders.Painter.Invalidate;
      end;
    end;
  end
end;

end.

