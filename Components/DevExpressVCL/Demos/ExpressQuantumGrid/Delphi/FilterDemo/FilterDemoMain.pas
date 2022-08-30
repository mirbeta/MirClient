unit FilterDemoMain;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, DB, StdCtrls, dxCore, cxStyles, cxCustomData, cxGraphics, cxFilter,
  cxData, cxDataStorage, cxEdit, cxDBData, cxGridLevel, cxClasses, cxControls,
  cxGridCustomView, cxGridCustomTableView, cxGridTableView, cxGridDBTableView,
  cxGrid, cxDBLookupComboBox, cxEditRepositoryItems, ExtCtrls, BaseForm,
  cxLookAndFeels, cxLookAndFeelPainters, ComCtrls, dxmdaset,
  DBClient, MidasLib;

type
  TfrmMain = class(TfmBaseForm)
    dsCompanies: TDataSource;
    GridLevel1: TcxGridLevel;
    Grid: TcxGrid;
    TableView: TcxGridTableView;
    TableViewCompany: TcxGridColumn;
    TableViewCountry: TcxGridColumn;
    TableViewOrderDate: TcxGridColumn;
    TableViewOrderID: TcxGridColumn;
    TableViewProduct: TcxGridColumn;
    TableViewQuantity: TcxGridColumn;
    miView: TMenuItem;
    miColumnFilterPopupMultiSelect: TMenuItem;
    miApplyMultiSelectChanges: TMenuItem;
    N1: TMenuItem;
    miColumnFilterPopupFilteredList: TMenuItem;
    N2: TMenuItem;
    miFilterRow: TMenuItem;
    miApplyMultiSelectChangesImmediately: TMenuItem;
    miApplyMultiSelectChangesOnButtonClick: TMenuItem;
    miDateTimeFilters: TMenuItem;
    dsCountries: TDataSource;
    erMain: TcxEditRepository;
    erMainFlag: TcxEditRepositoryImageItem;
    miApplyFilterRowChanges: TMenuItem;
    miApplyFilterRowChangesOnCellExit: TMenuItem;
    miApplyFilterRowChangesImmediately: TMenuItem;
    miDateTimeFilterRelativeDays: TMenuItem;
    miDateTimeFilterRelativeDayPeriods: TMenuItem;
    miDateTimeFilterRelativeWeeks: TMenuItem;
    miDateTimeFilterRelativeMonths: TMenuItem;
    miDateTimeFilterRelativeYears: TMenuItem;
    miDateTimeFilterPastFuture: TMenuItem;
    miDateTimeFilterMonths: TMenuItem;
    miDateTimeFilterYears: TMenuItem;
    Panel1: TPanel;
    pnlMaskInfo: TPanel;
    cdsCompanies: TClientDataSet;
    cdsCountries: TClientDataSet;
    cdsProducts: TClientDataSet;
    cdsCompaniesID: TAutoIncField;
    cdsCompaniesCOMPANYTYPEID: TIntegerField;
    cdsCompaniesCOUNTRYID: TIntegerField;
    cdsCompaniesCOMPANYNAME: TStringField;
    cdsCompaniesCOMPANYWEBSITE: TStringField;
    cdsCountriesID: TAutoIncField;
    cdsCountriesNAME: TStringField;
    cdsCountriesACRONYM: TStringField;
    cdsCountriesNATIONALFLAG: TBlobField;
    cdsProductsID: TAutoIncField;
    cdsProductsName: TStringField;
    cdsProductsDescription: TMemoField;
    cdsProductsPlatform: TStringField;
    cdsProductsLogo: TBlobField;
    cdsProductsLink: TMemoField;
    miAllowOperatorCustomization: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure miColumnFilterPopupMultiSelectClick(Sender: TObject);
    procedure miApplyMultiSelectChangesClick(Sender: TObject);
    procedure miColumnFilterPopupFilteredListClick(Sender: TObject);
    procedure miFilterRowClick(Sender: TObject);
    procedure miApplyFilterRowChangesClick(Sender: TObject);
    procedure miDateTimeFilterClick(Sender: TObject);
    procedure miAllowOperatorCustomizationClick(Sender: TObject);
  protected
    procedure GenerateData;
    function GetProductName(AID: Integer): string;
    procedure UpdateMenuValues;
  public
    procedure AfterConstruction; override;
  end;

var
  frmMain: TfrmMain;

implementation

uses
  Variants, AboutDemoForm;

{$R *.dfm}

procedure TfrmMain.AfterConstruction;
begin
  inherited AfterConstruction;
  UpdateMenuValues;
end;

procedure TfrmMain.GenerateData;
const
  OrdersPerCustomer = 25;
  DayRange = 3 * 365 div 2;
  MaxQuantity = 50;
var
  ADate: TDateTime;
  ACompanyName: string;
  ACountryID, I, ARecordIndex: Integer;
  ARow: TcxCustomGridRow;
begin
  ADate := Date;
  TableView.DataController.RecordCount := cdsCompanies.RecordCount * OrdersPerCustomer;

  TableView.BeginUpdate;
  try
    cdsCompanies.First;
    while not cdsCompanies.Eof do
    begin
      ACompanyName := cdsCompaniesCOMPANYNAME.AsString;
      ACountryID := cdsCompaniesCOUNTRYID.AsInteger;

      for I := 0 to OrdersPerCustomer - 1 do
      begin
        ARecordIndex := (cdsCompanies.RecNo - 1) * OrdersPerCustomer + I;
        ARow := TableView.ViewData.Rows[ARecordIndex];

        ARow.Values[TableViewCompany.Index] := ACompanyName;
        ARow.Values[TableViewCountry.Index] := ACountryID;
        ARow.Values[TableViewOrderDate.Index] := ADate + Random(DayRange) - DayRange div 2 + Random;
        ARow.Values[TableViewProduct.Index] := GetProductName(1 + Random(cdsProducts.RecordCount));
        ARow.Values[TableViewQuantity.Index] := 1 + Random(MaxQuantity);
      end;
      cdsCompanies.Next;
    end;
    TableViewOrderDate.SortOrder := soAscending;
  finally
    TableView.EndUpdate;
  end;

  TableView.BeginUpdate;
  try
    for I := 0 to TableView.ViewData.RowCount - 1 do
      TableView.ViewData.Rows[I].Values[TableViewOrderID.Index] := 1 + I;
  finally
    TableView.EndUpdate;
  end;
end;

function TfrmMain.GetProductName(AID: Integer): string;
begin
  if cdsProducts.Locate('ID', AID, []) then
    Result := cdsProductsName.AsString
  else
    Result := '';
end;

procedure TfrmMain.UpdateMenuValues;
const
  MenuItemsName: array[TcxGridDateTimeFilter] of string = (
    'miDateTimeFilterRelativeDays', 'miDateTimeFilterRelativeDayPeriods',
    'miDateTimeFilterRelativeWeeks', 'miDateTimeFilterRelativeMonths',
    'miDateTimeFilterRelativeYears', 'miDateTimeFilterPastFuture',
    'miDateTimeFilterMonths', 'miDateTimeFilterYears');
var
  ADateTimeFilter: TcxGridDateTimeFilter;
begin
  MenuItemSetChecked('miColumnFilterPopupMultiSelect', TableView.Filtering.ColumnPopup.MultiSelect);
  MenuItemSetEnabled('miApplyMultiSelectChanges', TableView.Filtering.ColumnPopup.MultiSelect);
  if TableView.Filtering.ColumnPopup.ApplyMultiSelectChanges = fpacImmediately then
    MenuItemSetChecked('miApplyMultiSelectChangesImmediately', True)
  else
    MenuItemSetChecked('miApplyMultiSelectChangesOnButtonClick', True);

  MenuItemSetChecked('miColumnFilterPopupFilteredList', TableView.Filtering.ColumnFilteredItemsList);
  MenuItemSetChecked('miFilterRow', TableView.FilterRow.Visible);
  MenuItemSetEnabled('miApplyFilterRowChanges', TableView.FilterRow.Visible);

  if TableView.FilterRow.ApplyChanges = fracOnCellExit then
    MenuItemSetChecked('miApplyFilterRowChangesOnCellExit', True)
  else
    MenuItemSetChecked('miApplyFilterRowChangesImmediately', True);

  MenuItemSetChecked('miFilterRow', TableView.FilterRow.Visible);
  MenuItemSetChecked('miAllowOperatorCustomization', TableView.FilterRow.OperatorCustomization);
  MenuItemSetEnabled('miAllowOperatorCustomization', TableView.FilterRow.Visible);
  for ADateTimeFilter := Low(ADateTimeFilter) to High(ADateTimeFilter) do
    MenuItemSetChecked(MenuItemsName[ADateTimeFilter],
      ADateTimeFilter in TableView.DateTimeHandling.Filters);
end;

procedure TfrmMain.miAllowOperatorCustomizationClick(Sender: TObject);
begin
  TableView.FilterRow.OperatorCustomization := not TableView.FilterRow.OperatorCustomization;
  UpdateMenuValues;
end;

procedure TfrmMain.miApplyFilterRowChangesClick(Sender: TObject);
begin
  TableView.FilterRow.ApplyChanges := TcxGridFilterRowApplyChangesMode(TComponent(Sender).Tag);
  UpdateMenuValues;
end;

procedure TfrmMain.miApplyMultiSelectChangesClick(Sender: TObject);
begin
  TableView.Filtering.ColumnPopup.ApplyMultiSelectChanges :=
    TcxGridItemFilterPopupApplyChangesMode(TComponent(Sender).Tag);
  UpdateMenuValues;  
end;

procedure TfrmMain.miColumnFilterPopupFilteredListClick(Sender: TObject);
begin
  with TableView.Filtering do
    ColumnFilteredItemsList := not ColumnFilteredItemsList;
  UpdateMenuValues;  
end;

procedure TfrmMain.miColumnFilterPopupMultiSelectClick(Sender: TObject);
begin
  with TableView.Filtering.ColumnPopup do
    MultiSelect := not MultiSelect;
  UpdateMenuValues;  
end;

procedure TfrmMain.miFilterRowClick(Sender: TObject);
begin
  with TableView.FilterRow do
    Visible := not Visible;
  UpdateMenuValues;
end;

procedure TfrmMain.miDateTimeFilterClick(Sender: TObject);
var
  AFilter: TcxGridDateTimeFilter;
begin
  AFilter := TcxGridDateTimeFilter(TComponent(Sender).Tag);
  with TableView.DateTimeHandling do
    if AFilter in Filters then
      Filters := Filters - [AFilter]
    else
      Filters := Filters + [AFilter];
  UpdateMenuValues;    
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  cdsCompanies.Open;
  cdsCountries.Open;
  cdsProducts.Open;

  GenerateData;

  TableViewProduct.DataBinding.AddToFilter(nil, foLike, 'Express*');
  TableViewOrderDate.DataBinding.AddToFilter(nil, foThisMonth, Null);
  TableView.DataController.Filter.Active := True;
end;

end.
