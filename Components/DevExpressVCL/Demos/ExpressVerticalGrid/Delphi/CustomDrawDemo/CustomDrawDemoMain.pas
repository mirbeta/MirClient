unit CustomDrawDemoMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  cxStyles, cxMaskEdit, cxCurrencyEdit, cxMemo, cxCheckBox, Menus,
  ActnList, ImgList,  ComCtrls, StdCtrls, 
  cxLookAndFeels, cxLookAndFeelPainters, cxInplaceContainer, cxControls,
  cxClasses, cxGraphics, cxContainer, cxCustomData, 
  cxImageComboBox, cxDBLookupComboBox, cxEdit, cxEditRepositoryItems, cxTextEdit, 
  cxVGrid, cxDBVGrid, cxVGridViewInfo, dxOffice11, cxVGridUtils,
  cxHyperLinkEdit, cxBlobEdit, DemoBasicMain, CustomDrawDemoUtils;

type
  TcxItemCustomDrawInfo = class;

  TcxCustomDrawInfo = class (TObject)
  private
    FBitmaps: TList;
    FDefaultFont: TFont;
    FCustomDrawData: TList;
    FOwnerDrawText: Boolean;
    FControl: TControl;
    function GetBkBitmap(ABkImage: TBkImage): TBitmap;
    function GetCount: Integer;
    function GetItem(ADrawArea: TCustomDrawArea): TcxItemCustomDrawInfo;
  public
    constructor Create(AControl: TControl);
    destructor Destroy; override;
    procedure AddNewItem(ADrawArea: TCustomDrawArea; AItemType: TcxItemCustomDrawType);
    function GetItemByIndex(AIndex: Integer): TcxItemCustomDrawInfo;
    property Bitmaps[ABkImage: TBkImage]: TBitmap read GetBkBitmap;
    property Count: Integer read GetCount;
    property DefaultFont: TFont read FDefaultFont;
    property Items[ADrawArea: TCustomDrawArea]: TcxItemCustomDrawInfo read GetItem; default;
    property OwnerDrawText: Boolean read FOwnerDrawText write FOwnerDrawText;
    property Control: TControl read FControl;
  end;

  TFontChangedEvent = procedure (Sender: TObject; AFontSize: Integer) of object;
  TcxItemCustomDrawInfo = class
  private
    FOwner: TcxCustomDrawInfo;
    FBitmap: TBitmap;
    FBkImageType: TBkImage;
    FDrawArea: TCustomDrawArea;
    FDrawingStyle: TCustomDrawingStyle;
    FColorScheme: TColorScheme;
    FFont: TFont;
    FIsBitmapAssigned: Boolean;
    FIsFontAssigned: Boolean;
    FItemType: TcxItemCustomDrawType;
    FOwnerTextDraw: Boolean;
    FFontChangedEvent: TFontChangedEvent;
    function GetBitmap: TBitmap;
    function GetFont: TFont;
    procedure SetBitmap(const Value: TBitmap);
    procedure SetFont(const Value: TFont);
  protected
    procedure DoFontChanged(AFont: TFont); virtual;
  public
    constructor Create(AOwner: TcxCustomDrawInfo; ADrawArea: TCustomDrawArea;
      AItemType: TcxItemCustomDrawType);
    destructor Destroy; override;
    property Owner: TcxCustomDrawInfo read FOwner;
    property Bitmap: TBitmap read GetBitmap write SetBitmap;
    property BkImageType: TBkImage read FBkImageType write FBkImageType;
    property DrawArea: TCustomDrawArea read FDrawArea;
    property DrawingStyle: TCustomDrawingStyle read FDrawingStyle write FDrawingStyle;
    property ColorScheme: TColorScheme read FColorScheme write FColorScheme;
    property Font: TFont read GetFont write SetFont;
    property ItemType: TcxItemCustomDrawType read FItemType;
    property OwnerTextDraw: Boolean read FOwnerTextDraw write FOwnerTextDraw;
    property FontChanged: TFontChangedEvent read FFontChangedEvent write FFontChangedEvent;
  end;

  TCustomDrawDemoMainForm = class(TDemoBasicMainForm)
    cxEditRepository1: TcxEditRepository;
    eriTelephoneMaskEdit: TcxEditRepositoryMaskItem;
    actShowCustomizationForm: TAction;
    actShowCustomizationForm1: TMenuItem;
    miLayoutStyle: TMenuItem;
    miBandsView: TMenuItem;
    miMultiRecordView: TMenuItem;
    miSingleRecordView: TMenuItem;
    N1: TMenuItem;
    cxDBVerticalGrid: TcxDBVerticalGrid;
    cxDBVerticalGridOrderInfo: TcxCategoryRow;
    cxDBVerticalGridPurchaseDate: TcxDBEditorRow;
    cxDBVerticalGridTime: TcxDBEditorRow;
    cxDBVerticalGridPaymentType: TcxDBEditorRow;
    cxDBVerticalGridPaymentAmount: TcxDBEditorRow;
    cxDBVerticalGridQuantity: TcxDBEditorRow;
    cxDBVerticalGridCustomerInfo: TcxCategoryRow;
    cxDBVerticalGridCommonCustomerInfo: TcxCategoryRow;
    cxDBVerticalGridFirstName: TcxDBEditorRow;
    cxDBVerticalGridLastName: TcxDBEditorRow;
    cxDBVerticalGridSpouse: TcxDBEditorRow;
    cxDBVerticalGridPrefix: TcxDBEditorRow;
    cxDBVerticalGridTitle: TcxDBEditorRow;
    cxDBVerticalGridCustomerContacts: TcxCategoryRow;
    cxDBVerticalGridPhonesAndFaxes: TcxCategoryRow;
    cxDBVerticalGridFaxPhone: TcxDBEditorRow;
    cxDBVerticalGridHomePhone: TcxDBEditorRow;
    cxDBVerticalGridCategoryAddress: TcxCategoryRow;
    cxDBVerticalGridState: TcxDBEditorRow;
    cxDBVerticalGridCity: TcxDBEditorRow;
    cxDBVerticalGridAddress: TcxDBEditorRow;
    cxDBVerticalGridZipCode: TcxDBEditorRow;
    cxDBVerticalGridEmail: TcxDBEditorRow;
    cxDBVerticalGridOccupation: TcxDBEditorRow;
    cxDBVerticalGridCustomer: TcxDBEditorRow;
    cxDBVerticalGridCompany: TcxDBEditorRow;
    cxDBVerticalGridCarInfo: TcxCategoryRow;
    cxDBVerticalGridCar: TcxCategoryRow;
    cxDBVerticalGridTrademark: TcxDBEditorRow;
    cxDBVerticalGridModel: TcxDBEditorRow;
    cxDBVerticalGridMPG: TcxCategoryRow;
    cxDBVerticalGridMPG_City: TcxDBEditorRow;
    cxDBVerticalGridMPG_Highway: TcxDBEditorRow;
    cxDBVerticalGridEngine: TcxCategoryRow;
    cxDBVerticalGridHP: TcxDBEditorRow;
    cxDBVerticalGridLiter: TcxDBEditorRow;
    cxDBVerticalGridCyl: TcxDBEditorRow;
    cxDBVerticalGridNotes: TcxCategoryRow;
    cxDBVerticalGridCars_Description: TcxDBEditorRow;
    cxDBVerticalGridTransmission: TcxCategoryRow;
    cxDBVerticalGridTransmissSpeedCount: TcxDBEditorRow;
    cxDBVerticalGridTransmissAutomatic: TcxDBEditorRow;
    cxDBVerticalGridOthers: TcxCategoryRow;
    cxDBVerticalGridCategory: TcxDBEditorRow;
    cxDBVerticalGridHyperlink: TcxDBEditorRow;
    cxDBVerticalGridPrice: TcxDBEditorRow;
    cxDBVerticalGridPicture: TcxDBEditorRow;
    ImageList: TImageList;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure actCustomizationFormExecute(Sender: TObject);
    procedure cxDBVerticalGridDrawValue(Sender: TObject;
      ACanvas: TcxCanvas; APainter: TcxvgPainter;
      AValueInfo: TcxRowValueInfo; var Done: Boolean);
    procedure cxDBVerticalGridDrawRowHeader(Sender: TObject;
      ACanvas: TcxCanvas; APainter: TcxvgPainter;
      AHeaderViewInfo: TcxCustomRowHeaderInfo; var Done: Boolean);
    procedure cxDBVerticalGridDrawBackground(Sender: TObject;
      ACanvas: TcxCanvas; const R: TRect; const AViewParams: TcxViewParams;
      var Done: Boolean);
    procedure FormDestroy(Sender: TObject);
    procedure LayoutStyleClick(Sender: TObject);
  private
    FCustomDrawInfo: TcxCustomDrawInfo;
    function DrawCellItem(AItem: TcxItemCustomDrawInfo; ACanvas: TcxCanvas;
      AValueInfo: TcxRowValueInfo; APainter: TcxvgPainter; Sender: TObject): Boolean;
    function DrawHeaderItem(AItem: TcxItemCustomDrawInfo; ACanvas: TcxCanvas;
      AHeaderViewInfo: TcxCustomRowHeaderInfo; APainter: TcxvgPainter; Sender: TObject): Boolean;
    function DrawItem(AItem: TcxItemCustomDrawInfo; ACanvas: TcxCanvas;
      const R: TRect; AHorizontal: Boolean): Boolean;
    function DrawCategoryItem(AItem: TcxItemCustomDrawInfo; ACanvas: TcxCanvas;
      ACategoryViewInfo: TcxCategoryRowHeaderInfo; APainter: TcxvgPainter; Sender: TObject): Boolean;
    function DrawBackgroundItem(AItem: TcxItemCustomDrawInfo;
  ACanvas: TcxCanvas; const R: TRect; AViewParams: TcxViewParams; Sender: TObject): Boolean;
    procedure DrawIndents(AHeaderViewInfo: TcxCustomRowHeaderInfo; ACustomDrawArea: TCustomDrawArea; ACanvas: TcxCanvas; AViewParams: TcxViewParams;
  APainter: TcxvgPainter; AIndentInfoList: TIndentInfoList);
    procedure DrawCellsLines(ACanvas: TcxCanvas; AColor: TColor;
      ARect: TRect);
    procedure DrawDefaultLines(ACanvas: TcxCanvas; AColor: TColor;
      AHeaderViewInfo: TcxCustomRowHeaderInfo; ARect: TRect; ALineInfos: TLineInfos);
    procedure DrawRightLine(ACanvas: TcxCanvas; AColor: TColor;
      ARect: TRect);
    function GetAdditionalLines(Sender: TcxDBVerticalGrid; AHeaderViewInfo: TcxCustomRowHeaderInfo): TLineInfos;
    procedure DrawRowLeftBorder(ACanvas: TcxCanvas; const AHeaderRect: TRect; AColor: TColor);
  public
    property CustomDrawInfo: TcxCustomDrawInfo read FCustomDrawInfo;
  end;

var
  CustomDrawDemoMainForm: TCustomDrawDemoMainForm;

implementation

uses 
  CustomDrawDemoData, ShellAPI, DB, Dialogs, cxListBox, cxGeometry, CustomDrawDemoEditor;

{$R *.dfm}

type
  TControlAccess = class(TControl);

{ TcxCustomDrawInfo }

destructor TcxCustomDrawInfo.Destroy;
var
  I: Integer;
begin
  for I := 0 to FCustomDrawData.Count - 1 do
    TcxItemCustomDrawInfo(FCustomDrawData[I]).Free;
  for I := 0 to FBitmaps.Count - 1 do
    TBitmap(FBitmaps[I]).Free;
  FCustomDrawData.Free;  
  FBitmaps.Free;
  FDefaultFont.Free;
end;

constructor TcxCustomDrawInfo.Create(AControl: TControl);
  procedure LoadResourceBitmaps;
  var
    I: TBkImage;
    ABitmap: TBitmap;
  begin
    for I := Low(BkImageResNames) to High(BkImageResNames) do
    begin
      ABitmap := TBitmap.Create;
      LoadImageFromRes(ABitmap, BkImageResNames[I]);
      FBitmaps.Add(ABitmap);
    end;
  end;
begin
  FControl := AControl;
  FBitmaps := TList.Create;
  LoadResourceBitmaps;
  FDefaultFont := TFont.Create;
  FCustomDrawData := TList.Create;
  FOwnerDrawText := True;
end;

procedure TCustomDrawDemoMainForm.LayoutStyleClick(Sender: TObject);
begin
  if not TMenuItem(Sender).Checked then
  begin
    TMenuItem(Sender).Checked := True;
    cxDBVerticalGrid.LayoutStyle := TcxvgLayoutStyle(TMenuItem(Sender).Tag);
  end;
end;

procedure TcxCustomDrawInfo.AddNewItem(ADrawArea: TCustomDrawArea;
  AItemType: TcxItemCustomDrawType);
begin
  FCustomDrawData.Add(TcxItemCustomDrawInfo.Create(Self, ADrawArea, AItemType));
end;

function TcxCustomDrawInfo.GetItemByIndex(
  AIndex: Integer): TcxItemCustomDrawInfo;
begin
  Result := TcxItemCustomDrawInfo(FCustomDrawData[AIndex]);
end;

function TcxCustomDrawInfo.GetBkBitmap(ABkImage: TBkImage): TBitmap;
begin
  Result := TBitmap(FBitmaps[Integer(ABkImage)]);
end;

function TcxCustomDrawInfo.GetCount: Integer;
begin
  Result := FCustomDrawData.Count;
end;

function TcxCustomDrawInfo.GetItem(ADrawArea: TCustomDrawArea): TcxItemCustomDrawInfo;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FCustomDrawData.Count - 1 do
    if TcxItemCustomDrawInfo(FCustomDrawData[I]).DrawArea = ADrawArea then
    begin
      Result := TcxItemCustomDrawInfo(FCustomDrawData[I]);
      Break;
    end;
end;

{ TcxItemCustomDrawInfo }

constructor TcxItemCustomDrawInfo.Create(AOwner: TcxCustomDrawInfo;
  ADrawArea: TCustomDrawArea; AItemType: TcxItemCustomDrawType);
begin
  FOwner := AOwner;
  FDrawArea := ADrawArea;
  FItemType := AItemType;
  if FOwner <> nil then
    FBitmap := AOwner.Bitmaps[TBkImage(0)]
  else
    FBitmap := nil;
  if FOwner <> nil then
    FFont := AOwner.DefaultFont
  else
    FBitmap := nil;
  FBkImageType := TBkImage(0);
  FDrawingStyle := TCustomDrawingStyle(0);
  FColorScheme := TColorScheme(0);
  FIsBitmapAssigned := False;
  FIsFontAssigned := False;
  FOwnerTextDraw := False;
end;

destructor TcxItemCustomDrawInfo.Destroy;
begin
  if FIsBitmapAssigned then
    FBitmap.Free;
  if FIsFontAssigned then
    FFont.Free;
  inherited Destroy;
end;

procedure TcxItemCustomDrawInfo.DoFontChanged(AFont: TFont);
begin
  if Assigned(FFontChangedEvent) then
    FFontChangedEvent(Self, AFont.Size);
end;

function TcxItemCustomDrawInfo.GetBitmap: TBitmap;
begin
  if ((FBkImageType <> bkiUserDefined) or not FIsBitmapAssigned) and
    (FOwner <> nil) then
    Result := FOwner.Bitmaps[FBkImageType]
  else
    Result := FBitmap;
end;

function TcxItemCustomDrawInfo.GetFont: TFont;
begin
  Result := FFont;
end;

procedure TcxItemCustomDrawInfo.SetBitmap(const Value: TBitmap);
begin
  if FIsBitmapAssigned then
    FBitmap.Free;
  FBitmap := Value;
  FIsBitmapAssigned := True;
  FBkImageType := bkiUserDefined;
end;

procedure TcxItemCustomDrawInfo.SetFont(const Value: TFont);
begin
  TControlAccess(Owner.Control).Font := Value;
  if FIsFontAssigned then
    FFont.Free;
  FFont := Value;
  FIsFontAssigned := True;
end;

{ TCustomDrawDemoMainForm }

procedure TCustomDrawDemoMainForm.FormCreate(Sender: TObject);
  procedure AddCustomDrawInfos;
  begin
    FCustomDrawInfo.AddNewItem(cdaBackground, itNormal);
    FCustomDrawInfo.AddNewItem(cdaCategory, itNormal);
    FCustomDrawInfo.AddNewItem(cdaCell, itCell);
    FCustomDrawInfo.AddNewItem(cdaHeader, itNormal);
  end;
  procedure AdjustCustomDrawItems;
  begin
    FCustomDrawInfo[cdaBackground].BkImageType := bkiTile;
    FCustomDrawInfo[cdaCategory].DrawingStyle := cdsGradient;
    FCustomDrawInfo[cdaCategory].ColorScheme := csBlue;
    FCustomDrawInfo[cdaCell].DrawingStyle := cdsGradient;
    FCustomDrawInfo[cdaHeader].DrawingStyle := cdsDefaultDrawing;
  end;
begin
  inherited;
  FCustomDrawInfo := TcxCustomDrawInfo.Create(cxDBVerticalGrid);
  AddCustomDrawInfos;
  AdjustCustomDrawItems;
end;

procedure TCustomDrawDemoMainForm.FormDestroy(Sender: TObject);
begin
  FCustomDrawInfo.Free;
end;

procedure TCustomDrawDemoMainForm.FormShow(Sender: TObject);
begin
  cxDBVerticalGrid.FullExpand;
  CustomDrawDemoEditorForm.Show;
end;

procedure TCustomDrawDemoMainForm.actCustomizationFormExecute(Sender: TObject);
begin
  CustomDrawDemoEditorForm.Show;
end;

procedure TCustomDrawDemoMainForm.cxDBVerticalGridDrawBackground(
  Sender: TObject; ACanvas: TcxCanvas; const R: TRect;
  const AViewParams: TcxViewParams; var Done: Boolean);
begin
  Done := DrawBackgroundItem(FCustomDrawInfo[cdaBackground], ACanvas, R, AViewParams, Sender);
end;

function TCustomDrawDemoMainForm.DrawItem(AItem: TcxItemCustomDrawInfo;
  ACanvas: TcxCanvas; const R: TRect; AHorizontal: Boolean): Boolean;
begin
  case AItem.DrawingStyle of
    cdsBkImage:
       ACanvas.FillRect(R, AItem.Bitmap);
    cdsGradient:
      FillGradientRect(ACanvas.Canvas.Handle, R,
        ColorScheme[Integer(AItem.ColorScheme), 1],
        ColorScheme[Integer(AItem.ColorScheme), 0],
        AHorizontal);
  end;
  Result := (AItem.DrawingStyle <> cdsDefaultDrawing);
end;

function TCustomDrawDemoMainForm.DrawBackgroundItem(AItem: TcxItemCustomDrawInfo;
  ACanvas: TcxCanvas; const R: TRect; AViewParams: TcxViewParams; Sender: TObject): Boolean;
begin
  Result := False;
  if AItem.DrawingStyle = cdsDefaultDrawing then Exit;
  Result := DrawItem(AItem, ACanvas, R, Integer(AItem.ColorScheme) > 1);
end;

function TCustomDrawDemoMainForm.DrawCellItem(AItem: TcxItemCustomDrawInfo;
  ACanvas: TcxCanvas; AValueInfo: TcxRowValueInfo; APainter: TcxvgPainter;
  Sender: TObject): Boolean;

  procedure OwnerDrawText(ALineColor: TColor; AFont: TFont);
  var
    ARect: TRect;
  begin
    ACanvas.Pen.Color := ALineColor;
    ACanvas.Brush.Style := bsClear;
    ACanvas.Font := AFont;
    ARect := cxRectInflate(AValueInfo.BoundsRect, -1, 0, 0, -1);
    with ARect do
	    ACanvas.Canvas.Rectangle(Left, Top, Right, Bottom);
    if AValueInfo.EditViewInfo is TcxCustomCheckBoxViewInfo then
      with TcxCustomCheckBoxViewInfo(AValueInfo.EditViewInfo) do
      AValueInfo.LookAndFeelPainterClass.DrawCheckButton(ACanvas,
        cxRectOffset(CheckBoxRect, AValueInfo.BoundsRect.Left, AValueInfo.BoundsRect.Top),
        cxbsDefault, State = cbsChecked)
    else
      if AValueInfo.EditViewInfo is TcxCustomTextEditViewInfo then
        with TcxCustomTextEditViewInfo(AValueInfo.EditViewInfo) do
          ACanvas.DrawTexT(Text, cxRectInflate(AValueInfo.BoundsRect, -2, -2), 0);
  end;

  procedure AddLines(ALines: TLineInfoList; const R: TRect; AColor: TColor);
  begin
    ALines.Add(R.Left, R.Top, R.Right - R.Left, 1, AColor);
    ALines.Add(R.Left, R.Top, R.Bottom - R.Top, 1, AColor);
    ALines.Add(R.Right - 1, R.Top, R.Right - R.Left, 1, AColor);
    ALines.Add(R.Left, R.Bottom - 1, R.Right - R.Left, 1, AColor);
  end;

var
  AStyle: TcxStyle;
  I: Integer;
begin
  with TcxVerticalGrid(Sender).ViewInfo do
  begin
    LinesInfo.Clear;
    for I := 0 to ViewRects.BandRects.Count - 1 do
      AddLines(LinesInfo, ViewRects.BandRects[I], BandBorderColor);
  end;
  AValueInfo.Transparent := (AValueInfo.ViewParams.Bitmap <> nil) and
    (not AValueInfo.ViewParams.Bitmap.Empty);
  if AItem.DrawingStyle = cdsDefaultDrawing then
  begin
    APainter.DrawRowValueCell(AValueInfo);
    DrawCellsLines(ACanvas, TcxVerticalGrid(Sender).OptionsView.GridLineColor, AValueInfo.BoundsRect);
    DrawRightLine(ACanvas, TcxVerticalGrid(Sender).OptionsView.GridLineColor, AValueInfo.BoundsRect);
    Result := True;
    Exit;
  end;
  if AItem.DrawingStyle = cdsDependsOnData then
  begin
    if cxDBVerticalGridCustomer.Properties.Values[AValueInfo.RecordIndex] = 'Y' then
      AStyle := CustomDrawDemoDataDM.stCustomer
    else
      AStyle := CustomDrawDemoDataDM.stNoCustomer;
    ACanvas.Brush.Color := AStyle.Color;
    ACanvas.FillRect(AValueInfo.VisibleRect);
    OwnerDrawText(AStyle.TextColor, AStyle.Font);
    DrawCellsLines(ACanvas, TcxVerticalGrid(Sender).OptionsView.GridLineColor, AValueInfo.BoundsRect);
    Result := True;
  end
  else
  begin
    Result := DrawItem(AItem, ACanvas, cxRectInflate(AValueInfo.BoundsRect, 0, 0, 1, 1) , Integer(AItem.ColorScheme) > 1);
    if AItem.OwnerTextDraw then
      OwnerDrawText(clGray, AItem.Font)
    else
    begin
      AValueInfo.Transparent := True;
      Result := False;
    end;
  end;
end;

procedure TCustomDrawDemoMainForm.cxDBVerticalGridDrawValue(
  Sender: TObject; ACanvas: TcxCanvas; APainter: TcxvgPainter;
  AValueInfo: TcxRowValueInfo; var Done: Boolean);
begin
  Done := DrawCellItem(FCustomDrawInfo[cdaCell], ACanvas, AValueInfo, APainter, Sender);
end;

procedure TCustomDrawDemoMainForm.DrawIndents(AHeaderViewInfo: TcxCustomRowHeaderInfo; ACustomDrawArea: TCustomDrawArea; ACanvas: TcxCanvas; AViewParams: TcxViewParams;
  APainter: TcxvgPainter; AIndentInfoList: TIndentInfoList);
var
  i: Integer;
begin
  if FCustomDrawInfo[ACustomDrawArea].DrawingStyle = cdsDefaultDrawing then
    for I := 0 to AIndentInfoList.Count - 1 do
      with AIndentInfoList[I]^ do
      begin
        APainter.Canvas.Brush.Color := AViewParams.Color;
        APainter.Canvas.FillRect(cxRectInflate(Bounds, 0, 0, 0, 1), ViewParams.Bitmap);
      end
  else
  for I := 0 to AIndentInfoList.Count - 1 do
    DrawItem(FCustomDrawInfo[ACustomDrawArea], ACanvas, cxRectInflate(AIndentInfoList[I].Bounds, 0, 0, 0, 1),
        True);
end;

procedure TCustomDrawDemoMainForm.DrawCellsLines(ACanvas: TcxCanvas; AColor: TColor; ARect: TRect);
begin
  ACanvas.Brush.Color := AColor;
  with ARect do
  begin
    ACanvas.FillRect(cxRectBounds(Left, Top - 1, Right - Left + 1, 1));
    ACanvas.FillRect(cxRectBounds(Left, Bottom, Right - Left + 1, 1));
  end;
end;

procedure TCustomDrawDemoMainForm.DrawDefaultLines(ACanvas: TcxCanvas; AColor: TColor;
  AHeaderViewInfo: TcxCustomRowHeaderInfo; ARect: TRect;  ALineInfos: TLineInfos);
begin
  FillRects(ACanvas, AColor, GetIndents(AHeaderViewInfo, ALineInfos));
  DrawCellsLines(ACanvas, AColor, ARect);
end;

procedure TCustomDrawDemoMainForm.DrawRightLine(ACanvas: TcxCanvas; AColor: TColor; ARect: TRect);
begin
  ACanvas.Brush.Color := AColor;
  with ARect do
    ACanvas.FillRect(cxRectBounds(Right, Top, 1, Bottom - Top));
end;

function TCustomDrawDemoMainForm.GetAdditionalLines(Sender: TcxDBVerticalGrid; AHeaderViewInfo: TcxCustomRowHeaderInfo): TLineInfos;
var
  i: Integer;
begin
  Result := [];
  if Sender.LayoutStyle <> lsBandsView then Exit;
  for i := 0 to Sender.ViewInfo.BandInfo.Count - 1 do
    if Sender.ViewInfo.BandInfo[i].FirstRow = AHeaderViewInfo.Row then
    begin
      Result := [liTop];
      if Sender.ViewInfo.BandInfo[i].RowsCount = 1 then
        Result := Result + [liBottom];
    end;
  for i := 0 to Sender.ViewInfo.BandInfo.Count - 1 do
    if (Sender.PrevVisibleRow(Sender.ViewInfo.BandInfo[i].FirstRow)
        = AHeaderViewInfo.Row) or (Sender.LastVisibleRow = AHeaderViewInfo.Row) then
    begin
      Result := Result + [liBottom];
      Exit;
    end;
end;

procedure TCustomDrawDemoMainForm.cxDBVerticalGridDrawRowHeader(Sender: TObject; ACanvas: TcxCanvas;
  APainter: TcxvgPainter; AHeaderViewInfo: TcxCustomRowHeaderInfo; var Done: Boolean);
begin
  if AHeaderViewInfo is TcxEditorRowHeaderInfo then
    Done := DrawHeaderItem(FCustomDrawInfo[cdaHeader], ACanvas, AHeaderViewInfo, APainter, Sender) else
  if AHeaderViewInfo is TcxCategoryRowHeaderInfo then
    Done := DrawCategoryItem(FCustomDrawInfo[cdaCategory], ACanvas, TcxCategoryRowHeaderInfo(AHeaderViewInfo),
      APainter, Sender);
end;

function TCustomDrawDemoMainForm.DrawHeaderItem(AItem: TcxItemCustomDrawInfo; ACanvas: TcxCanvas;
  AHeaderViewInfo: TcxCustomRowHeaderInfo; APainter: TcxvgPainter; Sender: TObject): Boolean;
var
  ARect: TRect;
  ACategoryViewParams: TcxViewParams;
  ALineInfo: TLineInfos;
begin
  TcxVerticalGrid(Sender).ViewInfo.LinesInfo.Clear;
  DrawRowLeftBorder(ACanvas, AHeaderViewInfo.HeaderRect, TcxVerticalGrid(Sender).OptionsView.GridLineColor);
  if (AItem.DrawingStyle = cdsDefaultDrawing) then
  begin
    APainter.DrawRowHeader(AHeaderViewInfo);
    DrawIndents(AHeaderViewInfo, cdaHeader, ACanvas, AHeaderViewInfo.ViewParams, APainter, AHeaderViewInfo.RowIndents);
    Result := True;
  end else
  begin
    with AHeaderViewInfo do
    begin
      ARect := cxRectInflate(HeaderRect, 0, 0, 1, 1);
      ARect.Left := RowIndents[0].Bounds.Left;
      DrawItem(FCustomDrawInfo[cdaHeader], ACanvas, ARect, Integer(AItem.ColorScheme) > 1);
    end;
    APainter.Painter.DrawExpandButton(ACanvas, AHeaderViewInfo.ButtonRect, AHeaderViewInfo.Row.Expanded);
    ACanvas.ExcludeClipRect(AHeaderViewInfo.ButtonRect);
    AHeaderViewInfo.Transparent := True;
    Result := False;
  end;

  with ACategoryViewParams do
  begin
    Bitmap := cxDBVerticalGrid.Styles.Category.Bitmap;
    Color := cxDBVerticalGrid.Styles.Category.Color;
  end;
  DrawIndents(AHeaderViewInfo, cdaCategory, ACanvas, ACategoryViewParams, APainter, AHeaderViewInfo.CategoryIndents);
  if (FCustomDrawInfo[cdaHeader].DrawingStyle = cdsDefaultDrawing) or
     (FCustomDrawInfo[cdaCategory].DrawingStyle = cdsDefaultDrawing) then
  begin
    DrawRightLine(ACanvas, TcxVerticalGrid(Sender).OptionsView.GridLineColor, AHeaderViewInfo.HeaderRect);
    if cxRectIsNull(AHeaderViewInfo.HeaderCellsRect) then
      ARect := AHeaderViewInfo.RowIndents[AHeaderViewInfo.RowIndents.Count - 1].Bounds
    else
      ARect := AHeaderViewInfo.HeaderCellsRect;
    ALineInfo := GetAdditionalLines(TcxDBVerticalGrid(Sender), AHeaderViewInfo);
    DrawDefaultLines(ACanvas, TcxVerticalGrid(Sender).OptionsView.GridLineColor, AHeaderViewInfo, ARect, ALineInfo);
  end;
  AHeaderViewInfo.LinesInfo.Clear;
end;

function TCustomDrawDemoMainForm.DrawCategoryItem(AItem: TcxItemCustomDrawInfo;
  ACanvas: TcxCanvas; ACategoryViewInfo: TcxCategoryRowHeaderInfo; APainter: TcxvgPainter;
  Sender: TObject): Boolean;
var
  ARect: TRect;
  ARowViewParams: TcxViewParams;
  ADelta: Integer;
  ALineInfo: TLineInfos;
begin
  TcxVerticalGrid(Sender).ViewInfo.LinesInfo.Clear;
  DrawRowLeftBorder(ACanvas, ACategoryViewInfo.HeaderRect, TcxVerticalGrid(Sender).OptionsView.GridLineColor);
  if (ACategoryViewInfo.RowIndents.Count > 0) then
  begin
    if FCustomDrawInfo[cdaHeader].DrawingStyle <> cdsDefaultDrawing then
    begin
      ADelta := ACategoryViewInfo.RowIndents[0].Bounds.Left - ACategoryViewInfo.HeaderRect.Left;
      ARect := ACategoryViewInfo.HeaderRect;
      ARect.Left := ACategoryViewInfo.RowIndents[0].Bounds.Left;
      ARect.Right := ARect.Left + (TcxVerticalGrid(Sender).ViewInfo.FullHeaderWidth - ADelta);
      DrawItem(FCustomDrawInfo[cdaHeader], ACanvas, cxRectInflate(ARect, 0, 0, 0, 1), Integer(FCustomDrawInfo[cdaHeader].ColorScheme) > 1)
    end else
    begin
      with ARowViewParams do
      begin
        Bitmap := cxDBVerticalGrid.Styles.Header.Bitmap;
        Color := cxDBVerticalGrid.Styles.Header.Color;
      end;
      DrawIndents(ACategoryViewInfo, cdaHeader, ACanvas, ARowViewParams, APainter, ACategoryViewInfo.RowIndents);
    end;
  end;
  if (AItem.DrawingStyle = cdsDefaultDrawing) then
  begin
    APainter.DrawRowHeaderCell(ACategoryViewInfo.CaptionsInfo[0], False);
    Result := True;
  end else
  begin
    DrawItem(AItem, ACanvas, cxRectInflate(ACategoryViewInfo.HeaderCellsRect, 0, 0, 1, 1), Integer(AItem.ColorScheme) > 1);
    ACategoryViewInfo.Transparent := True;
    Result := False;
  end;
  APainter.Painter.DrawExpandButton(ACanvas, ACategoryViewInfo.ButtonRect, ACategoryViewInfo.Row.Expanded);
  ACanvas.ExcludeClipRect(ACategoryViewInfo.ButtonRect);
  DrawIndents(ACategoryViewInfo, cdaCategory, ACanvas, ACategoryViewInfo.ViewParams, APainter, ACategoryViewInfo.CategoryIndents);

  if (FCustomDrawInfo[cdaHeader].DrawingStyle = cdsDefaultDrawing) or
    (FCustomDrawInfo[cdaCategory].DrawingStyle = cdsDefaultDrawing) then
  begin
    DrawRightLine(ACanvas, TcxVerticalGrid(Sender).OptionsView.GridLineColor, ACategoryViewInfo.HeaderRect);
    ALineInfo := GetAdditionalLines(TcxDBVerticalGrid(Sender), ACategoryViewInfo);
    DrawDefaultLines(ACanvas, TcxVerticalGrid(Sender).OptionsView.GridLineColor, ACategoryViewInfo, ACategoryViewInfo.HeaderCellsRect, ALineInfo);
  end;
  ACategoryViewInfo.LinesInfo.Clear;
end;


procedure TCustomDrawDemoMainForm.DrawRowLeftBorder(ACanvas: TcxCanvas;
  const AHeaderRect: TRect; AColor: TColor);
var
  ARect: TRect;
begin
  ARect := AHeaderRect;
  ARect.Right := ARect.Left;
  Dec(ARect.Left);
  Inc(ARect.Bottom);
  ACanvas.FillRect(ARect, AColor);
  ACanvas.ExcludeClipRect(ARect);
end;

end.


