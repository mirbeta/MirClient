unit HybridAppFrameSales;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, HybridAppBaseFrame, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit,
  dxLayoutcxEditAdapters, dxLayoutContainer, cxClasses, dxGDIPlusClasses, cxImage, cxTextEdit, cxMaskEdit, cxButtonEdit,
  dxLayoutControl, cxStyles, cxCustomData, cxFilter, cxData, cxDataStorage, cxNavigator, DB, cxDBData, cxCalendar,
  cxDBLookupComboBox, cxCurrencyEdit, cxProgressBar, cxCustomPivotGrid, cxDBPivotGrid, cxGridLevel, cxGridChartView,
  cxGridDBChartView, cxGridCustomTableView, cxGridTableView, cxGridDBTableView, cxGridCustomView, cxGrid, cxDropDownEdit,
  HybridAppDM, dxLayoutControlAdapters, Menus, StdCtrls, cxButtons, cxGroupBox,
  cxDataControllerConditionalFormattingRulesManagerDialog, dxDateRanges, Generics.Defaults, Generics.Collections;

type
  TfrmSales = class(TfrmBase)
    dxLayoutGroup3: TdxLayoutGroup;
    dxLayoutItem2: TdxLayoutItem;
    cbxYears: TcxComboBox;
    dxLayoutItem3: TdxLayoutItem;
    cxGridChart: TcxGrid;
    cxGridChartDBTableView1: TcxGridDBTableView;
    cxGridChartDBTableView1RecId: TcxGridDBColumn;
    cxGridChartDBTableView1Year: TcxGridDBColumn;
    cxGridChartDBTableView1Category: TcxGridDBColumn;
    cxGridChartDBTableView1Total: TcxGridDBColumn;
    gvChartView: TcxGridDBChartView;
    gvChartViewSeries1: TcxGridDBChartSeries;
    cxGridChartLevel1: TcxGridLevel;
    liOpportunities: TdxLayoutItem;
    cxDBPivotGrid1: TcxDBPivotGrid;
    cxDBPivotGrid1Total: TcxDBPivotGridField;
    cxDBPivotGrid1Opportunity: TcxDBPivotGridField;
    cxDBPivotGrid1State: TcxDBPivotGridField;
    cxDBPivotGrid1City: TcxDBPivotGridField;
    dxLayoutItem5: TdxLayoutItem;
    cxGridSales: TcxGrid;
    gvSales: TcxGridDBTableView;
    gvSalesInvoiceNumber: TcxGridDBColumn;
    gvSalesOrderDate: TcxGridDBColumn;
    gvSalesName: TcxGridDBColumn;
    gvSalesCity: TcxGridDBColumn;
    gvSalesTotalAmount: TcxGridDBColumn;
    cxGridSalesLevel1: TcxGridLevel;
    dxLayoutItem1: TdxLayoutItem;
    btnView: TcxButton;
    dxLayoutItem8: TdxLayoutItem;
    btnPrint: TcxButton;
    dxLayoutSeparatorItem2: TdxLayoutSeparatorItem;
    procedure btnViewClick(Sender: TObject);
    procedure gvSalesNameGetFilterValues(Sender: TcxCustomGridTableItem; AValueList: TcxDataFilterValueList);
    procedure gvSalesCellDblClick(Sender: TcxCustomGridTableView; ACellViewInfo: TcxGridTableDataCellViewInfo;
      AButton: TMouseButton; AShift: TShiftState; var AHandled: Boolean);
    procedure cbxYearsPropertiesEditValueChanged(Sender: TObject);
  strict private
    FYears: TDictionary<Integer, Integer>;
  private
    procedure PopulateYears;
  protected
    procedure DoAfterActivate; override;
    procedure Translate; override;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

implementation

{$R *.dfm}

uses
  dxCore, MainUnit, DateUtils, LocalizationStrs;


procedure TfrmSales.btnViewClick(Sender: TObject);
begin
  DM.clOrderItems.Filter := Format('OrderId = %s', [DM.clOrders.FieldByName('Id').AsString]);
  DM.clOrderItems.Filtered := True;
  ShowEditPage(MainForm.tbiSaleView);
end;

procedure TfrmSales.cbxYearsPropertiesEditValueChanged(Sender: TObject);
var
  AYear: Integer;
begin
  if FYears.TryGetValue(cbxYears.ItemIndex, AYear) then
  begin
    ShowHourglassCursor;
    try
      DM.ApplyYearSalesFilter(AYear);
      DM.ApplySalesFilter(AYear);
      DM.ApplyQuotesFilter(AYear);
      gvSales.Controller.GoToFirst;
    finally
      HideHourglassCursor;
    end;
  end;
end;

procedure TfrmSales.AfterConstruction;
begin
  inherited AfterConstruction;
  FYears := TDictionary<Integer, Integer>.Create;
end;

procedure TfrmSales.BeforeDestruction;
begin
  FreeAndNil(FYears);
  inherited BeforeDestruction;
end;

procedure TfrmSales.DoAfterActivate;
begin
  inherited DoAfterActivate;
  if ActivatingCount = 1 then
    gvSales.Controller.GoToFirst;

  if cbxYears.ItemIndex < 0 then
    cbxYears.ItemIndex := 0;
end;

procedure TfrmSales.Translate;
begin
  inherited Translate;

  PopulateYears;

  gvSalesInvoiceNumber.Caption := cxGetResourceString(@sInvoiceColumn);
  gvSalesOrderDate.Caption := cxGetResourceString(@sOrderDateColumn);
  gvSalesName.Caption := cxGetResourceString(@sName);
  gvSalesCity.Caption := cxGetResourceString(@sCityColumn);
  gvSalesTotalAmount.Caption := cxGetResourceString(@sTotalAmountColumn);

  liOpportunities.Caption := cxGetResourceString(@sOpportunitiesLabel);
  cxDBPivotGrid1State.Caption := cxGetResourceString(@sStateColumn);
  cxDBPivotGrid1Opportunity.Caption := cxGetResourceString(@sOpportunitiesColumn);
  cxDBPivotGrid1City.Caption := cxGetResourceString(@sCityColumn);
  cxDBPivotGrid1Total.Caption := cxGetResourceString(@sGrandTotalLabel);

  btnView.Caption := cxGetResourceString(@sViewButton);
  btnPrint.Caption := cxGetResourceString(@sPrintButton);
end;

procedure TfrmSales.gvSalesCellDblClick(Sender: TcxCustomGridTableView; ACellViewInfo: TcxGridTableDataCellViewInfo;
  AButton: TMouseButton; AShift: TShiftState; var AHandled: Boolean);
begin
  btnView.Click;
end;

procedure TfrmSales.gvSalesNameGetFilterValues(Sender: TcxCustomGridTableItem; AValueList: TcxDataFilterValueList);
begin
  AssignGridFilterBoxFont(Sender, AValueList);
end;

procedure TfrmSales.PopulateYears;
var
  AFiltered: Boolean;
  AIndex, AYear: Integer;
begin
  FYears.Clear;
  cbxYears.Properties.BeginUpdate;
  AIndex := cbxYears.ItemIndex;
  cbxYears.Properties.Items.Clear;
  DM.clYearSales.DisableControls;
  try
    AFiltered := DM.clYearSales.Filtered;
    DM.clYearSales.Filtered := False;
    DM.clYearSales.First;
    while not DM.clYearSales.EOF do
    begin
      AYear := DM.clYearSales.FieldByName('Year').AsInteger;
      if not FYears.ContainsValue(AYear) then
      begin
        cbxYears.Properties.Items.Add(cxGetResourceString(@sSalesLabel) + ' ' + IntToStr(AYear));
        FYears.Add(cbxYears.Properties.Items.Count - 1, AYear);
      end;
      DM.clYearSales.Next;
    end;
    DM.clYearSales.Filtered := AFiltered;
  finally
    DM.clYearSales.EnableControls;
  end;
  cbxYears.Properties.EndUpdate;
  cbxYears.ItemIndex := AIndex;
end;

initialization
  RegisterFrame(IDSales, TfrmSales);

end.
