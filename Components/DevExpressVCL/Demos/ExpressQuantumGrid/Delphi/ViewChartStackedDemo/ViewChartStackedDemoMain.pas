unit ViewChartStackedDemoMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  cxStyles, cxCustomData, cxGraphics, cxFilter, cxData, cxDataStorage, cxEdit,
  DB, cxDBData, cxGridChartView, cxGridDBChartView, cxGridCustomTableView,
  cxGridTableView, cxGridDBTableView, cxGridLevel, cxClasses,
  cxControls, cxGridCustomView, cxGrid, cxTimeEdit, cxImageComboBox, ImgList,
  cxLookAndFeels, Menus, StdCtrls, cxLookAndFeelPainters, dxmdaset, dxOffice11,
  cxGeometry, ComCtrls, BaseForm;

type
  TfrmMain = class(TfmBaseForm)
    PaymentTypeImages: TImageList;
    ChartViewStyles: TcxStyleRepository;
    cvStyle1: TcxStyle;
    cvStyle2: TcxStyle;
    cvStyle3: TcxStyle;
    cvStyle4: TcxStyle;
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
    miDiagram: TMenuItem;
    N2: TMenuItem;
    cvStyle5: TcxStyle;
    cvStyle6: TcxStyle;
    miToolBox: TMenuItem;
    N4: TMenuItem;
    miToolBoxPosition: TMenuItem;
    miToolBoxPositionTop: TMenuItem;
    miToolBoxPositionBottom: TMenuItem;
    cvStyle7: TcxStyle;
    grMain: TcxGrid;
    gvBarsChartView: TcxGridDBChartView;
    cxGridDBChartSeries1: TcxGridDBChartSeries;
    cxGridDBChartSeries2: TcxGridDBChartSeries;
    cxGridDBChartSeries3: TcxGridDBChartSeries;
    cxGridDBChartSeries4: TcxGridDBChartSeries;
    cxGridDBChartSeries5: TcxGridDBChartSeries;
    cxGridDBChartSeries6: TcxGridDBChartSeries;
    glBarsChart: TcxGridLevel;
    dsSales: TDataSource;
    dxMemData1: TdxMemData;
    dxMemData1Country: TStringField;
    dxMemData1Male14: TFloatField;
    dxMemData1Male64: TFloatField;
    dxMemData1Male65: TFloatField;
    dxMemData1Female14: TFloatField;
    dxMemData1Female64: TFloatField;
    dxMemData1Female65: TFloatField;
    glBarsTable: TcxGridLevel;
    gvBarsTableView: TcxGridDBTableView;
    gvBarsTableViewRecId: TcxGridDBColumn;
    gvBarsTableViewCountry: TcxGridDBColumn;
    gvBarsTableViewMale14: TcxGridDBColumn;
    gvBarsTableViewMale64: TcxGridDBColumn;
    gvBarsTableViewMale65: TcxGridDBColumn;
    gvBarsTableViewFemale14: TcxGridDBColumn;
    gvBarsTableViewFemale64: TcxGridDBColumn;
    gvBarsTableViewFemale65: TcxGridDBColumn;
    N1: TMenuItem;
    miStackedStyle: TMenuItem;
    Normal1: TMenuItem;
    SideBySide1: TMenuItem;
    N100Percent1: TMenuItem;
    N100PercentSideBySide1: TMenuItem;
    N5: TMenuItem;
    dxMemData2: TdxMemData;
    dxMemData2Politics: TIntegerField;
    dxMemData2Category: TDateTimeField;
    dxMemData2Entertainment: TIntegerField;
    dxMemData2Travel: TIntegerField;
    glAreaChart: TcxGridLevel;
    glAreaTable: TcxGridLevel;
    gvAreaChartView: TcxGridDBChartView;
    DataSource1: TDataSource;
    gvAreaChartViewSeries1: TcxGridDBChartSeries;
    gvAreaChartViewSeries2: TcxGridDBChartSeries;
    gvAreaChartViewSeries3: TcxGridDBChartSeries;
    gvAreaTableView: TcxGridDBTableView;
    gvAreaTableViewRecId: TcxGridDBColumn;
    gvAreaTableViewCategory: TcxGridDBColumn;
    gvAreaTableViewPolitics: TcxGridDBColumn;
    gvAreaTableViewEntertainment: TcxGridDBColumn;
    gvAreaTableViewTravel: TcxGridDBColumn;
    miEmptyPointsMode: TMenuItem;
    miepdmZero: TMenuItem;
    miepdmGap: TMenuItem;
    N6: TMenuItem;
    miTransparency: TMenuItem;
    mi0: TMenuItem;
    mi255: TMenuItem;
    mi45: TMenuItem;
    mi90: TMenuItem;
    mi135: TMenuItem;
    mi180: TMenuItem;
    mi225: TMenuItem;
    miAreaStackedStyle: TMenuItem;
    N100Percent2: TMenuItem;
    Default1: TMenuItem;
    miLineDiagramValueCaptionPosition: TMenuItem;
    mildvcpBelow: TMenuItem;
    mildvcpRight: TMenuItem;
    mildvcpAbove: TMenuItem;
    mildvcpLeft: TMenuItem;
    mildvcpNone: TMenuItem;
    mildvcpAboveRight: TMenuItem;
    mildvcpBelowRight: TMenuItem;
    mildvcpAboveLeft: TMenuItem;
    mildvcpBelowLeft: TMenuItem;
    N7: TMenuItem;
    mildvcpCenter: TMenuItem;
    procedure FormActivate(Sender: TObject);
    procedure grMainActiveTabChanged(Sender: TcxCustomGrid; ALevel: TcxGridLevel);
    procedure miAxisGridLinesClick(Sender: TObject);
    procedure miAxisTickMarkKindItemClick(Sender: TObject);
    procedure miAxisTickMarkLabelsClick(Sender: TObject);
    procedure miAxisVisibleClick(Sender: TObject);
    procedure miCategoryAxisCategoriesInReverseOrderClick(Sender: TObject);
    procedure miCategoryAxisValueAxisAtMaxCategoryClick(Sender: TObject);
    procedure miCategoryAxisValueAxisBetweenCategoriesClick(Sender: TObject);
    procedure miLegendBorderClick(Sender: TObject);
    procedure miLegendPositionItemClick(Sender: TObject);
    procedure miLineDiagramClick(Sender: TObject);
    procedure miPieDiagramClick(Sender: TObject);
    procedure miTitlePositionItemClick(Sender: TObject);
    procedure miToolBoxClick(Sender: TObject);
    procedure miToolBoxPositionClick(Sender: TObject);
    procedure gvChartViewDiagramStackedBarCustomDrawValue(
      Sender: TcxGridChartDiagram; ACanvas: TcxCanvas;
      AViewInfo: TcxGridChartDiagramValueViewInfo; var ADone: Boolean);
    procedure gvChartViewActiveDiagramChanged(Sender: TcxGridChartView; ADiagram: TcxGridChartDiagram);
    procedure miEmptyPointsDisplayModeClick(Sender: TObject);
    procedure miLineDiagramValueCaptionPositionItemClick(Sender: TObject);
    procedure miTransparencyClick(Sender: TObject);
    procedure miValueCaptionPositionItemClick(Sender: TObject);
    procedure StackedAreaStyleClick(Sender: TObject);
    procedure StackedStyleClick(Sender: TObject);
  private
    function GetActiveChart: TcxGridChartView;
    function GetActiveHistogram: TcxGridChartHistogram;
  protected
    procedure UpdateControls;
  public
    function GetActiveAxis(AMenuItem: TObject): TcxGridChartHistogramAxis;

    property ActiveChart: TcxGridChartView read GetActiveChart;
    property ActiveHistogram: TcxGridChartHistogram read GetActiveHistogram;
  end;

var
  frmMain: TfrmMain;

implementation

uses
  cxGridCommon, AboutDemoForm;

{$R *.dfm}

function TfrmMain.GetActiveChart: TcxGridChartView;
begin
  if grMain.ActiveView is TcxGridChartView then
    Result := TcxGridChartView(grMain.ActiveView)
  else
    Result := nil;
end;

function TfrmMain.GetActiveHistogram: TcxGridChartHistogram;
begin
  if (ActiveChart <> nil) and (ActiveChart.ActiveDiagram is TcxGridChartHistogram) then
    Result := TcxGridChartHistogram(ActiveChart.ActiveDiagram)
  else
    Result := nil;
end;

procedure TfrmMain.UpdateControls;

  procedure UpdateTitles;
  const
    Description: array[TcxGridChartStackedDiagramStyle] of string = (
      'Stacked %s Diagram', 'Full Stacked %s Diagram',
      'Side-By-Side Stacked %s Diagram', 'Side-By-Side Full Stacked %s Diagram'
    );
    Orientation: array[Boolean] of string = ('Bars', 'Columns');
  begin
    if gvBarsChartView.ActiveDiagram is TcxGridChartStackedColumnDiagram then
      glBarsChart.Caption := 'Population - ' +
        Format(Description[TcxGridChartStackedColumnDiagram(gvBarsChartView.ActiveDiagram).StackedStyle],
        [Orientation[gvBarsChartView.ActiveDiagram is TcxGridChartStackedColumnDiagram]])
    else
      glBarsChart.Caption := 'Population - ' + gvBarsChartView.ActiveDiagram.DisplayText;

    glAreaChart.Caption := 'Website Popularity - ' + gvAreaChartView.ActiveDiagram.DisplayText;
  end;

var
  AChart: TcxGridChartView;
begin
  UpdateTitles;
  AChart := ActiveChart;
  MenuItemSetEnabled('miView', AChart <> nil);
  MenuItemSetEnabled('miDiagram', AChart <> nil);
  if AChart = nil then Exit;

  MenuItemSetVisible('miStackedStyle', ActiveHistogram is TcxGridChartStackedColumnDiagram);
  if ActiveHistogram is TcxGridChartStackedColumnDiagram then
    MenuItemCheckSubItemWithTag('miStackedStyle', Ord(TcxGridChartStackedColumnDiagram(ActiveHistogram).StackedStyle));

  MenuItemSetVisible('miAreaStackedStyle', ActiveHistogram is TcxGridChartStackedAreaDiagram);
  if ActiveHistogram is TcxGridChartStackedAreaDiagram then
    MenuItemCheckSubItemWithTag('miAreaStackedStyle', Ord(TcxGridChartStackedAreaDiagram(ActiveHistogram).StackedStyle));

  MenuItemSetVisible('miTransparency', ActiveHistogram is TcxGridChartAreaDiagram);
  if ActiveHistogram is TcxGridChartAreaDiagram then
    MenuItemCheckSubItemWithTag('miTransparency', TcxGridChartAreaDiagram(ActiveHistogram).Transparency);

  MenuItemSetVisible('miEmptyPointsMode', ActiveHistogram is TcxGridChartLineDiagram);
  if ActiveHistogram is TcxGridChartLineDiagram then
    MenuItemCheckSubItemWithTag('miEmptyPointsMode', Ord(TcxGridChartLineDiagram(ActiveHistogram).EmptyPointsDisplayMode));

  // View Menu
  MenuItemCheckSubItemWithTag('miTitlePosition', Ord(AChart.Title.Position));
  MenuItemCheckSubItemWithTag('miLegendPosition', Ord(AChart.ActiveDiagram.Legend.Position));
  MenuItemSetChecked('miLegendBorder', AChart.ActiveDiagram.Legend.GetBorder = lbSingle);
  MenuItemSetChecked('miToolBox', AChart.ToolBox.Visible <> tvNever);
  MenuItemCheckSubItemWithTag('miToolBoxPosition', Ord(AChart.ToolBox.Position));

  MenuItemSetVisible('miLineDiagramValueCaptionPosition', ActiveHistogram is TcxGridChartLineDiagram);
  if ActiveHistogram is TcxGridChartLineDiagram then
    MenuItemCheckSubItemWithTag('miLineDiagramValueCaptionPosition',
      Ord(TcxGridChartLineDiagram(ActiveHistogram).Values.CaptionPosition));

  MenuItemSetVisible('miValueCaptionPosition', not (ActiveHistogram is TcxGridChartLineDiagram));
  if not (ActiveHistogram is TcxGridChartLineDiagram) then
    MenuItemCheckSubItemWithTag('miValueCaptionPosition',
      Ord(TcxGridChartStackedColumnDiagram(ActiveHistogram).Values.CaptionPosition));

  // histogram
  MenuItemSetVisible('miCategoryAxis', ActiveHistogram <> nil);
  MenuItemSetVisible('miValueAxis', ActiveHistogram <> nil);
  if ActiveHistogram <> nil then
  begin
    // category axis
    MenuItemSetChecked('miCategoryAxisVisible', ActiveHistogram.AxisCategory.Visible);
    MenuItemSetChecked('miCategoryAxisGridLines', ActiveHistogram.AxisCategory.GridLines);
    MenuItemSetChecked('miCategoryAxisTickMarkLabels', ActiveHistogram.AxisCategory.TickMarkLabels);
    MenuItemSetChecked('miCategoryAxisCategoriesInReverseOrder', ActiveHistogram.AxisCategory.CategoriesInReverseOrder);
    MenuItemSetChecked('miCategoryAxisValueAxisAtMaxCategory', ActiveHistogram.AxisCategory.ValueAxisAtMaxCategory);
    MenuItemSetChecked('miCategoryAxisValueAxisBetweenCategories', ActiveHistogram.AxisCategory.ValueAxisBetweenCategories);
    MenuItemCheckSubItemWithTag('miCategoryAxisTickMarkKind', Ord(ActiveHistogram.AxisCategory.TickMarkKind));
    // value axis
    MenuItemSetChecked('miValueAxisVisible', ActiveHistogram.AxisValue.Visible);
    MenuItemSetChecked('miValueAxisGridLines', ActiveHistogram.AxisValue.GridLines);
    MenuItemSetChecked('miValueAxisTickMarkLabels', ActiveHistogram.AxisValue.TickMarkLabels);
    MenuItemCheckSubItemWithTag('miValueAxisTickMarkKind', Ord(ActiveHistogram.AxisValue.TickMarkKind));
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
  UpdateControls;  // to update menu after chart view was changed via Customization Form
end;

procedure TfrmMain.grMainActiveTabChanged(Sender: TcxCustomGrid; ALevel: TcxGridLevel);
begin
  UpdateControls;
end;

procedure TfrmMain.miLineDiagramClick(Sender: TObject);
begin
  ActiveChart.DiagramLine.Active := True;
end;

procedure TfrmMain.miTitlePositionItemClick(Sender: TObject);
begin
  ActiveChart.Title.Position := TcxGridChartPartPosition((Sender as TComponent).Tag);
  UpdateControls;
end;

procedure TfrmMain.miToolBoxClick(Sender: TObject);
const
  AChangeVisible: array[TcxGridChartToolBoxVisible] of TcxGridChartToolBoxVisible =
    (tvAlways, tvNever, tvNever);
begin
  ActiveChart.ToolBox.Visible := AChangeVisible[ActiveChart.ToolBox.Visible];
  UpdateControls;
end;

procedure TfrmMain.miToolBoxPositionClick(Sender: TObject);
begin
  ActiveChart.ToolBox.Position := TcxGridChartToolBoxPosition((Sender as TComponent).Tag);
  UpdateControls;
end;

procedure TfrmMain.miValueCaptionPositionItemClick(Sender: TObject);
begin
  if ActiveHistogram is TcxGridChartColumnDiagram then
    TcxGridChartColumnDiagram(ActiveHistogram).Values.CaptionPosition :=
      TcxGridChartColumnDiagramValueCaptionPosition((Sender as TComponent).Tag);
  UpdateControls;
end;

procedure TfrmMain.miLineDiagramValueCaptionPositionItemClick(Sender: TObject);
begin
  if ActiveHistogram is TcxGridChartLineDiagram then
    TcxGridChartLineDiagram(ActiveHistogram).Values.CaptionPosition :=
      TcxGridChartLineDiagramValueCaptionPosition((Sender as TComponent).Tag);
  UpdateControls;
end;

procedure TfrmMain.miLegendPositionItemClick(Sender: TObject);
begin
  ActiveChart.ActiveDiagram.Legend.Position := TcxGridChartPartPosition((Sender as TComponent).Tag);
  UpdateControls;
end;

procedure TfrmMain.miLegendBorderClick(Sender: TObject);
begin
  if GetMenuItemChecked(Sender) then
    ActiveChart.ActiveDiagram.Legend.Border := lbSingle
  else
    ActiveChart.ActiveDiagram.Legend.Border := lbNone;
  UpdateControls;
end;

procedure TfrmMain.miPieDiagramClick(Sender: TObject);
begin
  ActiveChart.DiagramPie.Active := True;
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
  GetActiveAxis(Sender).TickMarkKind := TcxGridChartHistogramTickMarkKind((Sender as TComponent).Tag);
  UpdateControls;
end;

procedure TfrmMain.miAxisTickMarkLabelsClick(Sender: TObject);
begin
  with GetActiveAxis(Sender) do
    TickMarkLabels := not TickMarkLabels;
  UpdateControls;
end;

procedure TfrmMain.miCategoryAxisCategoriesInReverseOrderClick(Sender: TObject);
begin
  with GetActiveAxis(Sender) as TcxGridChartHistogramAxisCategory do
    CategoriesInReverseOrder := not CategoriesInReverseOrder;
  UpdateControls;
end;

procedure TfrmMain.miCategoryAxisValueAxisAtMaxCategoryClick(Sender: TObject);
begin
  with GetActiveAxis(Sender) as TcxGridChartHistogramAxisCategory do
    ValueAxisAtMaxCategory := not ValueAxisAtMaxCategory;
  UpdateControls;
end;

procedure TfrmMain.miCategoryAxisValueAxisBetweenCategoriesClick(Sender: TObject);
begin
  with GetActiveAxis(Sender) as TcxGridChartHistogramAxisCategory do
    ValueAxisBetweenCategories := not ValueAxisBetweenCategories;
  UpdateControls;
end;

procedure TfrmMain.gvChartViewDiagramStackedBarCustomDrawValue(
  Sender: TcxGridChartDiagram; ACanvas: TcxCanvas;
  AViewInfo: TcxGridChartDiagramValueViewInfo; var ADone: Boolean);
var
  C1: TColor;
begin
  C1 := dxGetLighterColor(AViewInfo.Params.Color, 75);
  FillGradientRect(ACanvas.Handle, AViewInfo.Bounds, C1,
    AViewInfo.Params.Color, Sender is TcxGridChartStackedBarDiagram);
  ADone := True;
end;

procedure TfrmMain.StackedStyleClick(Sender: TObject);
begin
  if ActiveHistogram is TcxGridChartStackedColumnDiagram then
  begin
    TcxGridChartStackedColumnDiagram(ActiveHistogram).StackedStyle :=
      TcxGridChartStackedDiagramStyle((Sender as TComponent).Tag);
  end;
  UpdateControls;
end;

procedure TfrmMain.StackedAreaStyleClick(Sender: TObject);
begin
  if ActiveHistogram is TcxGridChartStackedAreaDiagram then
  begin
    TcxGridChartStackedAreaDiagram(ActiveHistogram).StackedStyle :=
      TcxGridChartStackedAreaDiagramStyle((Sender as TComponent).Tag);
  end;
  UpdateControls;
end;

procedure TfrmMain.gvChartViewActiveDiagramChanged(
  Sender: TcxGridChartView; ADiagram: TcxGridChartDiagram);
begin
  UpdateControls;
end;

procedure TfrmMain.miEmptyPointsDisplayModeClick(Sender: TObject);
begin
  if ActiveHistogram is TcxGridChartLineDiagram then
  begin
    TcxGridChartLineDiagram(ActiveHistogram).EmptyPointsDisplayMode :=
      TcxGridChartEmptyPointsDisplayMode(TComponent(Sender).Tag);
  end;
  UpdateControls;
end;

procedure TfrmMain.miTransparencyClick(Sender: TObject);
begin
  if ActiveHistogram is TcxGridChartAreaDiagram then
    TcxGridChartAreaDiagram(ActiveHistogram).Transparency := (Sender as TComponent).Tag;
  UpdateControls;
end;

end.


