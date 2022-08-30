unit ViewChartDemoMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, cxStyles, cxCustomData, cxGraphics, cxFilter, cxData,
  cxDataStorage, cxEdit, DB, cxDBData, cxGridChartView, cxGridDBChartView,
  cxGridCustomTableView, cxGridTableView, cxGridDBTableView, 
  cxGridLevel, cxClasses, cxControls, cxGridCustomView, cxGrid, cxTimeEdit,
  cxImageComboBox, ImgList, cxLookAndFeels, Menus, StdCtrls,
  cxLookAndFeelPainters, BaseForm, DBClient, cxGridCardView, ComCtrls, MidasLib;

type
  TfrmMain = class(TfmBaseForm)
    tvData: TcxGridDBTableView;
    grMainLevel1: TcxGridLevel;
    grMain: TcxGrid;
    dsOrders: TDataSource;
    tblOrders: TClientDataSet;
    grMainLevel2: TcxGridLevel;
    chvSales: TcxGridDBChartView;
    dsSales: TDataSource;
    tblOrdersID: TAutoIncField;
    tblOrdersCustomerID: TIntegerField;
    tblOrdersProductID: TIntegerField;
    tblOrdersPurchaseDate: TDateTimeField;
    tblOrdersTime: TDateTimeField;
    tblOrdersPaymentType: TStringField;
    tblOrdersPaymentAmount: TCurrencyField;
    tblOrdersDescription: TMemoField;
    tblOrdersQuantity: TIntegerField;
    tblProducts: TClientDataSet;
    tblOrdersProductName: TStringField;
    tblCustomers: TClientDataSet;
    tblOrdersCustomer: TStringField;
    tvDataID: TcxGridDBColumn;
    tvDataPurchaseDate: TcxGridDBColumn;
    tvDataPaymentType: TcxGridDBColumn;
    tvDataPaymentAmount: TcxGridDBColumn;
    tvDataQuantity: TcxGridDBColumn;
    tvDataProductName: TcxGridDBColumn;
    tvDataCompany: TcxGridDBColumn;
    chvSalesByQuarter: TcxGridChartView;
    chvSalesByQuarterSeries1: TcxGridChartSeries;
    chvSalesByQuarterSeries2: TcxGridChartSeries;
    chvSalesByQuarterSeries3: TcxGridChartSeries;
    chvSalesByQuarterSeries4: TcxGridChartSeries;
    grMainLevel3: TcxGridLevel;
    chvSalesSeries1: TcxGridDBChartSeries;
    PaymentTypeImages: TImageList;
    miColumnDiagram: TMenuItem;
    miBarDiagram: TMenuItem;
    N1: TMenuItem;
    miView: TMenuItem;
    miTitlePosition: TMenuItem;
    miLegendPosition: TMenuItem;
    miLegendBorder: TMenuItem;
    miValueCaptionPosition: TMenuItem;
    miCategoryAxis: TMenuItem;
    miCategoryAxisVisible: TMenuItem;
    miCategoryAxisTickMarkLabels: TMenuItem;
    miCategoryAxisTickMarkKind: TMenuItem;
    miCategoryAxisGridLines: TMenuItem;
    miTitlePositionDefault: TMenuItem;
    miTitlePositionNone: TMenuItem;
    miTitlePositionLeft: TMenuItem;
    miTitlePositionTop: TMenuItem;
    miTitlePositionRight: TMenuItem;
    miTitlePositionBottom: TMenuItem;
    miLegendPositionDefault: TMenuItem;
    miLegendPositionNone: TMenuItem;
    miLegendPositionLeft: TMenuItem;
    miLegendPositionTop: TMenuItem;
    miLegendPositionRight: TMenuItem;
    miLegendPositionBottom: TMenuItem;
    miValueCaptionPositionNone: TMenuItem;
    miValueCaptionPositionInsideBase: TMenuItem;
    miValueCaptionPositionCenter: TMenuItem;
    miValueCaptionPositionInsideEnd: TMenuItem;
    miValueCaptionPositionOutsideEnd: TMenuItem;
    miCategoryAxisTickMarkKindNone: TMenuItem;
    miCategoryAxisTickMarkKindCross: TMenuItem;
    miCategoryAxisTickMarkKindInside: TMenuItem;
    miCategoryAxisTickMarkKindOutside: TMenuItem;
    miValueAxis: TMenuItem;
    miValueAxisTickMarkLabels: TMenuItem;
    miValueAxisTickMarkKind: TMenuItem;
    miValueAxisTickMarkKindOutside: TMenuItem;
    miValueAxisTickMarkKindInside: TMenuItem;
    miValueAxisTickMarkKindCross: TMenuItem;
    miValueAxisTickMarkKindNone: TMenuItem;
    miValueAxisGridLines: TMenuItem;
    miValueAxisVisible: TMenuItem;
    miCategoryAxisCategoriesInReverseOrder: TMenuItem;
    miCategoryAxisValueAxisAtMaxCategory: TMenuItem;
    miCategoryAxisValueAxisBetweenCategories: TMenuItem;
    N3: TMenuItem;
    miLineDiagram: TMenuItem;
    miAreaDiagram: TMenuItem;
    miDiagram: TMenuItem;
    miLineValueCaptionPosition: TMenuItem;
    miLineValueCaptionPositionNone: TMenuItem;
    miLineValueCaptionPositionLeft: TMenuItem;
    miLineValueCaptionPositionAbove: TMenuItem;
    miLineValueCaptionPositionRight: TMenuItem;
    miLineValueCaptionPositionBelow: TMenuItem;
    miLineValueCaptionPositionCenter: TMenuItem;
    miLineStyle: TMenuItem;
    miLineStyleNone: TMenuItem;
    miLineStyleSolid: TMenuItem;
    miLineStyleDash: TMenuItem;
    miLineStyleDot: TMenuItem;
    miLineStyleDashDot: TMenuItem;
    miLineStyleDashDotDot: TMenuItem;
    N2: TMenuItem;
    miMarkerStyle: TMenuItem;
    miMarkerStyleNone: TMenuItem;
    miMarkerStyleSquare: TMenuItem;
    miMarkerStyleTriangle: TMenuItem;
    miMarkerStyleDiamond: TMenuItem;
    miMarkerStyleCircle: TMenuItem;
    miValueStacking: TMenuItem;
    miValueStackingNone: TMenuItem;
    miValueStackingNormal: TMenuItem;
    miValueStacking100Percent: TMenuItem;
    miLineWidth: TMenuItem;
    miLineWidth1: TMenuItem;
    miLineWidth2: TMenuItem;
    miLineWidth3: TMenuItem;
    miLineWidth4: TMenuItem;
    miLineWidth5: TMenuItem;
    miMarkerSize: TMenuItem;
    miMarkerSize3: TMenuItem;
    miMarkerSize5: TMenuItem;
    miMarkerSize7: TMenuItem;
    miMarkerSize9: TMenuItem;
    miMarkerSize11: TMenuItem;
    miSeriesCaptions: TMenuItem;
    miSeriesColumnCount: TMenuItem;
    miSeriesSites: TMenuItem;
    miPieValueCaptionPosition: TMenuItem;
    miPieValueCaptionItems: TMenuItem;
    miSeriesColumns1: TMenuItem;
    miSeriesColumns2: TMenuItem;
    miSeriesColumns3: TMenuItem;
    miSeriesColumns4: TMenuItem;
    miPieValueCaptionPositionNone: TMenuItem;
    miPieValueCaptionPositionCenter: TMenuItem;
    miPieValueCaptionPositionInsideEnd: TMenuItem;
    miPieValueCaptionPositionOutsideEnd: TMenuItem;
    miPieValueCaptionPositionOutsideEndWithLeaderLines: TMenuItem;
    miPieValueCaptionItemCategory: TMenuItem;
    miPieValueCaptionItemValue: TMenuItem;
    miPieValueCaptionItemPercentage: TMenuItem;
    miToolBox: TMenuItem;
    N4: TMenuItem;
    miToolBoxPosition: TMenuItem;
    miToolBoxPositionTop: TMenuItem;
    miToolBoxPositionBottom: TMenuItem;
    miPieDiagram: TMenuItem;
    cvStyle1: TcxStyle;
    cvStyle2: TcxStyle;
    cvStyle3: TcxStyle;
    cvStyle4: TcxStyle;
    cvStyle5: TcxStyle;
    cvStyle6: TcxStyle;
    cvStyle7: TcxStyle;
    cdsQuarter2: TClientDataSet;
    cdsQuarter4: TClientDataSet;
    cdsQuarter3: TClientDataSet;
    cdsQuarter1: TClientDataSet;
    cdsSales: TClientDataSet;
    procedure chvSalesByQuarterGetValueHint(Sender: TcxGridChartView;
      ASeries: TcxGridChartSeries; AValueIndex: Integer; var AHint: String);
    procedure chvSalesSeries1CustomDrawValue(Sender: TcxGridChartSeries;
      ACanvas: TcxCanvas; AViewInfo: TcxGridChartDiagramValueViewInfo; var ADone: Boolean);
    procedure grMainActiveTabChanged(Sender: TcxCustomGrid; ALevel: TcxGridLevel);
    procedure miColumnDiagramClick(Sender: TObject);
    procedure miBarDiagramClick(Sender: TObject);
    procedure miTitlePositionItemClick(Sender: TObject);
    procedure miLegendPositionItemClick(Sender: TObject);
    procedure miLegendBorderClick(Sender: TObject);
    procedure miValueCaptionPositionItemClick(Sender: TObject);
    procedure chvSalesActiveDiagramChanged(Sender: TcxGridChartView; ADiagram: TcxGridChartDiagram);
    procedure miAxisVisibleClick(Sender: TObject);
    procedure miAxisGridLinesClick(Sender: TObject);
    procedure miAxisTickMarkKindItemClick(Sender: TObject);
    procedure miAxisTickMarkLabelsClick(Sender: TObject);
    procedure miCategoryAxisCategoriesInReverseOrderClick(Sender: TObject);
    procedure miCategoryAxisValueAxisAtMaxCategoryClick(Sender: TObject);
    procedure miCategoryAxisValueAxisBetweenCategoriesClick(Sender: TObject);
    procedure miLineDiagramClick(Sender: TObject);
    procedure miAreaDiagramClick(Sender: TObject);
    procedure miPieDiagramClick(Sender: TObject);
    procedure miLineValueCaptionPositionClick(Sender: TObject);
    procedure miLineStyleClick(Sender: TObject);
    procedure miMarkerStyleClick(Sender: TObject);
    procedure miValueStackingClick(Sender: TObject);
    procedure miLineWidthClick(Sender: TObject);
    procedure miMarkerSizeClick(Sender: TObject);
    procedure miSeriesCaptionsClick(Sender: TObject);
    procedure miSeriesSitesClick(Sender: TObject);
    procedure miSeriesColumnsClick(Sender: TObject);
    procedure miPieValueCaptionPositionClick(Sender: TObject);
    procedure miPieValueCaptionItemClick(Sender: TObject);
    procedure chvSalesDiagramAreaCustomDrawValueArea(
      Sender: TcxGridChartAreaDiagram; ACanvas: TcxCanvas;
      AViewInfo: TcxGridChartAreaDiagramValueViewInfo; var ADone: Boolean);
    procedure chvSalesValueClick(Sender: TcxGridChartView;
      ASeries: TcxGridChartSeries; AValueIndex: Integer; var AHandled: Boolean);
    procedure miToolBoxClick(Sender: TObject);
    procedure miToolBoxPositionClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    function GetActiveChart: TcxGridChartView;
    function GetActiveColumnDiagram: TcxGridChartColumnDiagram;
    function GetActiveHistogram: TcxGridChartHistogram;
    function GetActiveLineDiagram: TcxGridChartLineDiagram;
    function GetActivePieDiagram: TcxGridChartPieDiagram;
  protected
    procedure UpdateControls;
  public
    procedure CalculateSalesInfo;
    procedure CalculateSalesInfoForQuarter(AQuarter: Integer; AQuarterData: TClientDataSet);
    function GetActiveAxis(AMenuItem: TObject): TcxGridChartHistogramAxis;

    property ActiveChart: TcxGridChartView read GetActiveChart;
    property ActiveColumnDiagram: TcxGridChartColumnDiagram read GetActiveColumnDiagram;
    property ActiveHistogram: TcxGridChartHistogram read GetActiveHistogram;
    property ActiveLineDiagram: TcxGridChartLineDiagram read GetActiveLineDiagram;
    property ActivePieDiagram: TcxGridChartPieDiagram read GetActivePieDiagram;
  end;

var
  frmMain: TfrmMain;

implementation

uses
  dxOffice11, cxGridCommon, AboutDemoForm;

{$R *.dfm}

function TfrmMain.GetActiveChart: TcxGridChartView;
begin
  if grMain.ActiveView is TcxGridChartView then
    Result := TcxGridChartView(grMain.ActiveView)
  else
    Result := nil;
end;

function TfrmMain.GetActiveColumnDiagram: TcxGridChartColumnDiagram;
begin
  if ActiveChart.ActiveDiagram is TcxGridChartColumnDiagram then
    Result := TcxGridChartColumnDiagram(ActiveChart.ActiveDiagram)
  else
    Result := nil;
end;

function TfrmMain.GetActiveHistogram: TcxGridChartHistogram;
begin
  if ActiveChart.ActiveDiagram is TcxGridChartHistogram then
    Result := TcxGridChartHistogram(ActiveChart.ActiveDiagram)
  else
    Result := nil;
end;

function TfrmMain.GetActiveLineDiagram: TcxGridChartLineDiagram;
begin
  if ActiveChart.ActiveDiagram is TcxGridChartLineDiagram then
    Result := TcxGridChartLineDiagram(ActiveChart.ActiveDiagram)
  else
    Result := nil;
end;

function TfrmMain.GetActivePieDiagram: TcxGridChartPieDiagram;
begin
  if ActiveChart.ActiveDiagram is TcxGridChartPieDiagram then
    Result := TcxGridChartPieDiagram(ActiveChart.ActiveDiagram)
  else
    Result := nil;
end;

procedure TfrmMain.UpdateControls;
var
  AChart: TcxGridChartView;
  AColumnDiagram: TcxGridChartColumnDiagram;
  AHistogram: TcxGridChartHistogram;
  ALineDiagram: TcxGridChartLineDiagram;
  APieDiagram: TcxGridChartPieDiagram;
begin
  AChart := ActiveChart;
  MenuItemSetEnabled('miView', AChart <> nil);
  MenuItemSetEnabled('miDiagram', AChart <> nil);
  if AChart = nil then Exit;

  // View Menu
  MenuItemSetChecked('miAreaDiagram', AChart.DiagramArea.Active);
  MenuItemSetChecked('miBarDiagram', AChart.DiagramBar.Active);
  MenuItemSetChecked('miColumnDiagram', AChart.DiagramColumn.Active);
  MenuItemSetChecked('miLineDiagram', AChart.DiagramLine.Active);
  MenuItemSetChecked('miPieDiagram', AChart.DiagramPie.Active);
  MenuItemCheckSubItemWithTag('miTitlePosition', Ord(AChart.Title.Position));
  MenuItemCheckSubItemWithTag('miLegendPosition', Ord(AChart.ActiveDiagram.Legend.Position));
  MenuItemSetChecked('miLegendBorder', AChart.ActiveDiagram.Legend.Border = lbSingle);
  MenuItemSetChecked('miToolBox', AChart.ToolBox.Visible <> tvNever);
  MenuItemCheckSubItemWithTag('miToolBoxPosition', Ord(AChart.ToolBox.Position));

  { diagram }
  AHistogram := ActiveHistogram;
  MenuItemSetVisible('miCategoryAxis', AHistogram <> nil);
  MenuItemSetVisible('miValueAxis', AHistogram <> nil);
  if AHistogram <> nil then
  begin
    // category axis
    MenuItemSetChecked('miCategoryAxisVisible', AHistogram.AxisCategory.Visible);
    MenuItemSetChecked('miCategoryAxisGridLines', AHistogram.AxisCategory.GridLines);
    MenuItemCheckSubItemWithTag('miCategoryAxisTickMarkKind', Ord(AHistogram.AxisCategory.TickMarkKind));
    MenuItemSetChecked('miCategoryAxisTickMarkLabels', AHistogram.AxisCategory.TickMarkLabels);
    MenuItemSetChecked('miCategoryAxisCategoriesInReverseOrder', AHistogram.AxisCategory.CategoriesInReverseOrder);
    MenuItemSetChecked('miCategoryAxisValueAxisAtMaxCategory', AHistogram.AxisCategory.ValueAxisAtMaxCategory);
    MenuItemSetChecked('miCategoryAxisValueAxisBetweenCategories', AHistogram.AxisCategory.ValueAxisBetweenCategories);
    // value axis
    MenuItemSetChecked('miValueAxisVisible', AHistogram.AxisValue.Visible);
    MenuItemSetChecked('miValueAxisGridLines', AHistogram.AxisValue.GridLines);
    MenuItemSetChecked('miValueAxisTickMarkLabels', AHistogram.AxisValue.TickMarkLabels);
    MenuItemCheckSubItemWithTag('miValueAxisTickMarkKind', Ord(AHistogram.AxisValue.TickMarkKind));
  end;

  // column
  AColumnDiagram := ActiveColumnDiagram;
  MenuItemSetVisible('miValueCaptionPosition', AColumnDiagram <> nil);
  if AColumnDiagram <> nil then
    MenuItemCheckSubItemWithTag('miValueCaptionPosition', Ord(AColumnDiagram.Values.CaptionPosition));

  // line
  ALineDiagram := ActiveLineDiagram;
  MenuItemSetVisible('miLineValueCaptionPosition', ALineDiagram <> nil);
  MenuItemSetVisible('miLineStyle', ALineDiagram <> nil);
  MenuItemSetVisible('miLineWidth', ALineDiagram <> nil);
  MenuItemSetVisible('miMarkerStyle', ALineDiagram <> nil);
  MenuItemSetVisible('miMarkerSize', ALineDiagram <> nil);
  MenuItemSetVisible('miValueStacking', ALineDiagram <> nil);
  if ALineDiagram <> nil then
  begin
    MenuItemCheckSubItemWithTag('miLineValueCaptionPosition', Ord(ALineDiagram.Values.CaptionPosition));
    MenuItemCheckSubItemWithTag('miLineStyle', Ord(ALineDiagram.Values.LineStyle));
    MenuItemCheckSubItemWithTag('miLineWidth', ALineDiagram.Values.LineWidth);
    MenuItemCheckSubItemWithTag('miMarkerStyle', Ord(ALineDiagram.Values.MarkerStyle));
    MenuItemCheckSubItemWithTag('miMarkerSize', ALineDiagram.Values.MarkerSize);
    MenuItemCheckSubItemWithTag('miValueStacking', Ord(ALineDiagram.Values.Stacking));
  end;

  // pie
  APieDiagram := ActivePieDiagram;
  MenuItemSetVisible('miSeriesCaptions', APieDiagram <> nil);
  MenuItemSetVisible('miSeriesSites', APieDiagram <> nil);
  MenuItemSetVisible('miSeriesColumnCount', APieDiagram <> nil);
  MenuItemSetVisible('miPieValueCaptionPosition', APieDiagram <> nil);
  MenuItemSetVisible('miPieValueCaptionItems', APieDiagram <> nil);
  if APieDiagram <> nil then
  begin
    MenuItemSetChecked('miSeriesCaptions', APieDiagram.SeriesCaptions);
    MenuItemSetChecked('miSeriesSites', APieDiagram.SeriesSites);
    MenuItemSetEnabled('miSeriesColumnCount', APieDiagram.GetSeriesColumnCount > 0);
    if APieDiagram.GetSeriesColumnCount > 0 then
      MenuItemCheckSubItemWithTag('miSeriesColumnCount', APieDiagram.GetSeriesColumnCount);
    MenuItemCheckSubItemWithTag('miPieValueCaptionPosition', Ord(APieDiagram.Values.CaptionPosition));

    MenuItemSetChecked('miPieValueCaptionItemCategory', pdvciCategory in APieDiagram.Values.CaptionItems);
    MenuItemSetChecked('miPieValueCaptionItemValue', pdvciValue in APieDiagram.Values.CaptionItems);
    MenuItemSetChecked('miPieValueCaptionItemPercentage', pdvciPercentage in APieDiagram.Values.CaptionItems);
  end;
end;

procedure TfrmMain.miAreaDiagramClick(Sender: TObject);
begin
  ActiveChart.DiagramArea.Active := True;
end;

procedure TfrmMain.CalculateSalesInfo;
begin
  CalculateSalesInfoForQuarter(1, cdsQuarter1);
  CalculateSalesInfoForQuarter(2, cdsQuarter2);
  CalculateSalesInfoForQuarter(3, cdsQuarter3);
  CalculateSalesInfoForQuarter(4, cdsQuarter4);
end;

procedure TfrmMain.CalculateSalesInfoForQuarter(AQuarter: Integer; AQuarterData: TClientDataSet);
var
  I: Integer;
begin
  AQuarterData.Active := True;
  chvSalesByQuarter.ViewData.CategoryCount := AQuarterData.RecordCount;

  chvSalesByQuarter.BeginUpdate;
  try
    I := 0;
    AQuarterData.First;
    while not AQuarterData.Eof do
    begin
      chvSalesByQuarter.ViewData.Categories[I] := AQuarterData.FieldValues['Name'];
      chvSalesByQuarter.ViewData.Values[AQuarter - 1, I] := AQuarterData.FieldValues['Amount'];
      AQuarterData.Next;
      Inc(I);
    end;
  finally
    chvSalesByQuarter.EndUpdate;
  end;
end;

function TfrmMain.GetActiveAxis(AMenuItem: TObject): TcxGridChartHistogramAxis;

  function CheckPrefix(const APrefix: string): Boolean;
  begin
    Result := SameText(APrefix, Copy((AMenuItem as TComponent).Name, 1, Length(APrefix)));
  end;

begin
  if CheckPrefix('miCategoryAxis') then
    Result := ActiveHistogram.AxisCategory
  else
    if CheckPrefix('miValueAxis') then
      Result := ActiveHistogram.AxisValue
    else
      Result := nil;
end;

procedure TfrmMain.FormActivate(Sender: TObject);
begin
  // to update menu after chart view was changed via Customization Form
  UpdateControls;
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  inherited;
  grMainLevel3.Active := True;
  CalculateSalesInfo;
end;

procedure TfrmMain.chvSalesByQuarterGetValueHint(Sender: TcxGridChartView;
  ASeries: TcxGridChartSeries; AValueIndex: Integer; var AHint: String);
begin
  AHint := Format('%s sales for Q%d are %s',
    [Sender.ViewData.Categories[AValueIndex], ASeries.Index + 1, ASeries.VisibleDisplayTexts[AValueIndex]]);
end;

procedure TfrmMain.chvSalesDiagramAreaCustomDrawValueArea(
  Sender: TcxGridChartAreaDiagram; ACanvas: TcxCanvas;
  AViewInfo: TcxGridChartAreaDiagramValueViewInfo; var ADone: Boolean);
var
  AClipRegion, ARegion: TcxRegion;
begin
  AClipRegion := ACanvas.GetClipRegion;
  try
    ARegion := AViewInfo.CreateAreaRegion;
    try
      ACanvas.SetClipRegion(ARegion, roIntersect, False);
      FillGradientRect(ACanvas.Handle, AViewInfo.GetRealBounds, clWhite, clMedGray, True);
    finally
      ARegion.Free;
    end;
  finally
    ACanvas.SetClipRegion(AClipRegion, roSet);
  end;
  ADone := True;
end;

procedure TfrmMain.chvSalesSeries1CustomDrawValue(
  Sender: TcxGridChartSeries; ACanvas: TcxCanvas;
  AViewInfo: TcxGridChartDiagramValueViewInfo; var ADone: Boolean);
var
  AEndColor: TColor;
begin
  if Sender.GridView.DiagramColumn.Active or Sender.GridView.DiagramBar.Active then
  begin
    if AViewInfo.State = gcsNone then
      AEndColor := clBlack
    else
      AEndColor := clGray;
    FillGradientRect(ACanvas.Handle, AViewInfo.ContentBounds, clWhite, AEndColor,
      Sender.GridView.DiagramColumn.Active);
    ADone := True;
  end;
end;

procedure TfrmMain.chvSalesValueClick(Sender: TcxGridChartView;
  ASeries: TcxGridChartSeries; AValueIndex: Integer; var AHandled: Boolean);
begin
  cdsSales.Locate('Name', Sender.Categories.VisibleValues[AValueIndex], []);
  tblOrders.Locate('ProductID', cdsSales.FieldByName('ID').AsInteger, []);
  grMainLevel1.Active := True;
end;

procedure TfrmMain.grMainActiveTabChanged(Sender: TcxCustomGrid;
  ALevel: TcxGridLevel);
begin
  UpdateControls;
end;

procedure TfrmMain.miLineDiagramClick(Sender: TObject);
begin
  ActiveChart.DiagramLine.Active := True;
end;

procedure TfrmMain.miLineStyleClick(Sender: TObject);
begin
  ActiveLineDiagram.Values.LineStyle := TcxGridChartLineStyle((Sender as TComponent).Tag);
  UpdateControls;
end;

procedure TfrmMain.miLineValueCaptionPositionClick(Sender: TObject);
begin
  ActiveLineDiagram.Values.CaptionPosition :=
    TcxGridChartLineDiagramValueCaptionPosition((Sender as TComponent).Tag);
  UpdateControls;
end;

procedure TfrmMain.miLineWidthClick(Sender: TObject);
begin
  ActiveLineDiagram.Values.LineWidth := (Sender as TComponent).Tag;
  UpdateControls;
end;

procedure TfrmMain.miMarkerSizeClick(Sender: TObject);
begin
  ActiveLineDiagram.Values.MarkerSize := (Sender as TComponent).Tag;
  UpdateControls;
end;

procedure TfrmMain.miMarkerStyleClick(Sender: TObject);
begin
  ActiveLineDiagram.Values.MarkerStyle := TcxGridChartMarkerStyle((Sender as TComponent).Tag);
  UpdateControls;
end;

procedure TfrmMain.miColumnDiagramClick(Sender: TObject);
begin
  ActiveChart.DiagramColumn.Active := True;
end;

procedure TfrmMain.miBarDiagramClick(Sender: TObject);
begin
  ActiveChart.DiagramBar.Active := True;
end;

procedure TfrmMain.miTitlePositionItemClick(Sender: TObject);
begin
  ActiveChart.Title.Position := TcxGridChartPartPosition((Sender as TComponent).Tag);
  UpdateControls;  
end;

procedure TfrmMain.miToolBoxClick(Sender: TObject);
begin
  with ActiveChart.ToolBox do
    if Visible = tvNever then
      Visible := tvAlways
    else
      Visible := tvNever;
  UpdateControls;
end;

procedure TfrmMain.miToolBoxPositionClick(Sender: TObject);
begin
  ActiveChart.ToolBox.Position := TcxGridChartToolBoxPosition((Sender as TComponent).Tag);
  UpdateControls;
end;

procedure TfrmMain.miLegendPositionItemClick(Sender: TObject);
begin
  ActiveChart.ActiveDiagram.Legend.Position := TcxGridChartPartPosition((Sender as TComponent).Tag);
  UpdateControls;  
end;

procedure TfrmMain.miLegendBorderClick(Sender: TObject);
begin
  with ActiveChart.ActiveDiagram.Legend do
    if Border <> lbSingle then
      Border := lbSingle
    else
      Border := lbNone;
  UpdateControls;  
end;

procedure TfrmMain.miValueCaptionPositionItemClick(Sender: TObject);
begin
  ActiveColumnDiagram.Values.CaptionPosition :=
    TcxGridChartColumnDiagramValueCaptionPosition((Sender as TComponent).Tag);
  UpdateControls;
end;

procedure TfrmMain.miValueStackingClick(Sender: TObject);
begin
  ActiveLineDiagram.Values.Stacking := TcxGridChartValuesStacking((Sender as TComponent).Tag);
  UpdateControls;
end;

procedure TfrmMain.miPieValueCaptionItemClick(Sender: TObject);
var
  ACaptionItem: TcxGridChartPieDiagramValueCaptionItem;
begin
  ACaptionItem := TcxGridChartPieDiagramValueCaptionItem((Sender as TComponent).Tag);
  with ActivePieDiagram.Values do
    if ACaptionItem in CaptionItems then
      CaptionItems := CaptionItems - [ACaptionItem]
    else
      CaptionItems := CaptionItems + [ACaptionItem];
  UpdateControls;    
end;

procedure TfrmMain.miPieValueCaptionPositionClick(Sender: TObject);
begin
  ActivePieDiagram.Values.CaptionPosition :=
    TcxGridChartPieDiagramValueCaptionPosition((Sender as TComponent).Tag);
  UpdateControls;
end;

procedure TfrmMain.miPieDiagramClick(Sender: TObject);
begin
  ActiveChart.DiagramPie.Active := True;
end;

procedure TfrmMain.miSeriesCaptionsClick(Sender: TObject);
begin
  with ActivePieDiagram do
    SeriesCaptions := not SeriesCaptions;
  UpdateControls;
end;

procedure TfrmMain.miSeriesColumnsClick(Sender: TObject);
begin
  ActivePieDiagram.SeriesColumnCount := (Sender as TComponent).Tag;
  UpdateControls;
end;

procedure TfrmMain.miSeriesSitesClick(Sender: TObject);
begin
  with ActivePieDiagram do
    SeriesSites := not SeriesSites;
  UpdateControls;
end;

procedure TfrmMain.chvSalesActiveDiagramChanged(
  Sender: TcxGridChartView; ADiagram: TcxGridChartDiagram);
begin
  if Sender = ActiveChart then UpdateControls;
end;

procedure TfrmMain.miAxisVisibleClick(Sender: TObject);
begin
  with GetActiveAxis(Sender) do
    Visible := not Visible;
  UpdateControls;
end;

procedure TfrmMain.miAxisGridLinesClick(Sender: TObject);
begin
  with GetActiveAxis(Sender) do
    GridLines := not GridLines;
  UpdateControls;  
end;

procedure TfrmMain.miAxisTickMarkKindItemClick(Sender: TObject);
begin
  GetActiveAxis(Sender).TickMarkKind :=
    TcxGridChartHistogramTickMarkKind((Sender as TComponent).Tag);
  UpdateControls;  
end;

procedure TfrmMain.miAxisTickMarkLabelsClick(Sender: TObject);
begin
  with GetActiveAxis(Sender) do
    TickMarkLabels := not TickMarkLabels;
  UpdateControls;
end;

procedure TfrmMain.miCategoryAxisCategoriesInReverseOrderClick(
  Sender: TObject);
begin
  with GetActiveAxis(Sender) as TcxGridChartHistogramAxisCategory do
    CategoriesInReverseOrder := not CategoriesInReverseOrder;
  UpdateControls;  
end;

procedure TfrmMain.miCategoryAxisValueAxisAtMaxCategoryClick(
  Sender: TObject);
begin
  with GetActiveAxis(Sender) as TcxGridChartHistogramAxisCategory do
    ValueAxisAtMaxCategory := not ValueAxisAtMaxCategory;
  UpdateControls;
end;

procedure TfrmMain.miCategoryAxisValueAxisBetweenCategoriesClick(
  Sender: TObject);
begin
  with GetActiveAxis(Sender) as TcxGridChartHistogramAxisCategory do
    ValueAxisBetweenCategories := not ValueAxisBetweenCategories;
  UpdateControls;  
end;

procedure TfrmMain.FormCreate(Sender: TObject);
var
  APath: string;
begin
  APath := ExtractFilePath(Application.ExeName) + '..\..\Data\';
  tblCustomers.LoadFromFile(APath + 'Customers.xml');
  tblProducts.LoadFromFile(APath + 'Products.xml');
  tblOrders.LoadFromFile(APath + 'Orders.xml');
end;

end.
