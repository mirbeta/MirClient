unit FindPanelDemoMain;

{$I cxVer.inc}

interface

uses
  SysUtils, Classes, Controls, Menus, DB, StdCtrls, BaseForm, ComCtrls, DBClient,
  dxCore, cxStyles, cxCustomData, cxGraphics, cxFilter, cxData, cxDataStorage,
  cxEdit, cxGridLevel, cxClasses, cxControls, cxGridCustomView, cxGridCustomTableView,
  cxGridTableView, cxGrid, cxEditRepositoryItems, cxLookAndFeels, cxLookAndFeelPainters,
  cxGridCardView, cxNavigator, cxContainer, cxGroupBox, ActnList, cxCheckBox, cxLabel,
  cxTextEdit, cxMaskEdit, cxSpinEdit, cxDropDownEdit, cxImageComboBox, cxDBData,
  cxGridDBTableView, XPMan, CarsDataForGrid, MidasLib;

type
  TfrmMain = class(TfmBaseForm)
    erMain: TcxEditRepository;
    erMainFlag: TcxEditRepositoryImageItem;
    cxGroupBox1: TcxGroupBox;
    cbClearFindOnClose: TcxCheckBox;
    cbShowClearButton: TcxCheckBox;
    cbShowCloseButton: TcxCheckBox;
    cbShowFindButton: TcxCheckBox;
    cbHighlightSearchResults: TcxCheckBox;
    alAction: TActionList;
    actClearFindOnClose: TAction;
    actShowClearButton: TAction;
    actShowCloseButton: TAction;
    actShowFindButton: TAction;
    actHighlightSearchResults: TAction;
    miFindPanelOptions: TMenuItem;
    ClearFindOnClose1: TMenuItem;
    HighlightFindResult1: TMenuItem;
    miVisibleButtons: TMenuItem;
    ShowClearButton2: TMenuItem;
    ShowCloseButton2: TMenuItem;
    ShowFindButton2: TMenuItem;
    seFindDelay: TcxSpinEdit;
    lbSearchDelay: TcxLabel;
    icbFindFilterColumns: TcxImageComboBox;
    lbSearchableColumns: TcxLabel;
    cbeFindPanelPosition: TcxComboBox;
    lbFindPanelPosition: TcxLabel;
    cbeDisplayMode: TcxComboBox;
    lbDisplayMode: TcxLabel;
    cbUseDelayedSearch: TcxCheckBox;
    actUseDelayedSearch: TAction;
    UseDelayedFind1: TMenuItem;
    dsOrders: TDataSource;
    cdsOrders: TClientDataSet;
    cdsOrdersID: TAutoIncField;
    cdsOrdersCustomerID: TIntegerField;
    cdsOrdersProductID: TIntegerField;
    cdsOrdersPurchaseDate: TDateTimeField;
    cdsOrdersPaymentType: TStringField;
    cdsOrdersQuantity: TIntegerField;
    cdsOrdersUnitPrice: TCurrencyField;
    cdsOrdersCompanyName: TStringField;
    cdsOrdersPaymentAmount: TCurrencyField;
    dsCustomers: TDataSource;
    cdsCustomers: TClientDataSet;
    cdsCustomersID: TIntegerField;
    cdsCustomersCompany: TStringField;
    actUseExtendedSyntax: TAction;
    cbUseExtendedSyntax: TcxCheckBox;
    UseExtendedSyntax1: TMenuItem;
    Grid: TcxGrid;
    TableView: TcxGridDBTableView;
    TableViewCompanyName: TcxGridDBColumn;
    TableViewCarName: TcxGridDBColumn;
    TableViewQuantity: TcxGridDBColumn;
    TableViewUnitPrice: TcxGridDBColumn;
    TableViewPaymentAmount: TcxGridDBColumn;
    TableViewPaymentType: TcxGridDBColumn;
    TableViewPurchaseDate: TcxGridDBColumn;
    GridLevel1: TcxGridLevel;
    cdsOrdersCarName: TStringField;
    procedure FormCreate(Sender: TObject);
    procedure actClearFindOnCloseChange(Sender: TObject);
    procedure actShowClearButtonChange(Sender: TObject);
    procedure actShowCloseButtonChange(Sender: TObject);
    procedure actShowFindButtonEChange(Sender: TObject);
    procedure actHighlightFindResultChange(Sender: TObject);
    procedure seFindDelayPropertiesChange(Sender: TObject);
    procedure icbFindFilterColumnsPropertiesChange(Sender: TObject);
    procedure cbFindPanelPositionPropertiesChange(Sender: TObject);
    procedure cbDisplayModePropertiesChange(Sender: TObject);
    procedure actUseDelayedSearchExecute(Sender: TObject);
    procedure cdsOrdersCalcFields(DataSet: TDataSet);
    procedure actUseExtendedSyntaxExecute(Sender: TObject);
  protected
    procedure UpdateFindFilterColumns;
  end;

var
  frmMain: TfrmMain;

implementation

uses
  cxFindPanel, Variants, AboutDemoForm;

{$R *.dfm}

procedure TfrmMain.icbFindFilterColumnsPropertiesChange(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to TableView.ColumnCount - 1 do
    if (icbFindFilterColumns.EditValue = 'All') or
      (Pos(TableView.Columns[I].Name, icbFindFilterColumns.EditValue) > 0) then
      TableView.Columns[I].Options.FilteringWithFindPanel := True
    else
      TableView.Columns[I].Options.FilteringWithFindPanel := False;
end;

procedure TfrmMain.UpdateFindFilterColumns;
var
  I, J: Integer;
  AFindFilterColumnsDescription: string;
  AFindFilterColumnsValue: string;
  AImageComboBoxItem: TcxImageComboBoxItem;
  AColumn: TcxGridColumn;
begin
  icbFindFilterColumns.Properties.Items.Clear;
  AImageComboBoxItem := icbFindFilterColumns.Properties.Items.Add;
  AImageComboBoxItem.Description := 'All';
  AImageComboBoxItem.Value := 'All';
  for I := 0 to TableView.ColumnCount - 1 do
  begin
    AColumn := TableView.Columns[I];
    if not AColumn.Visible then
      Continue;
    AFindFilterColumnsDescription := TableView.Columns[I].Caption;
    AFindFilterColumnsValue := TableView.Columns[I].Name;
    for J := I to TableView.ColumnCount - 1 do
    begin
      AColumn := TableView.Columns[J];
      if not AColumn.Visible then
        Continue;
      if J <> I then
      begin
        AFindFilterColumnsDescription := AFindFilterColumnsDescription + '; ' + TableView.Columns[J].Caption;
        AFindFilterColumnsValue := AFindFilterColumnsValue + ';' + TableView.Columns[J].Name;
      end;
      AImageComboBoxItem := icbFindFilterColumns.Properties.Items.Add;
      AImageComboBoxItem.Description := AFindFilterColumnsDescription;
      AImageComboBoxItem.Value := AFindFilterColumnsValue;
    end;
  end;
  icbFindFilterColumns.ItemIndex := 0;
end;

procedure TfrmMain.seFindDelayPropertiesChange(Sender: TObject);
begin
  TableView.FindPanel.ApplyInputDelay := seFindDelay.Value;
end;

procedure TfrmMain.actClearFindOnCloseChange(Sender: TObject);
begin
  TableView.FindPanel.ClearFindFilterTextOnClose := actClearFindOnClose.Checked;
end;

procedure TfrmMain.actHighlightFindResultChange(Sender: TObject);
begin
  TableView.FindPanel.HighlightSearchResults := actHighlightSearchResults.Checked;
end;

procedure TfrmMain.actShowClearButtonChange(Sender: TObject);
begin
  TableView.FindPanel.ShowClearButton := actShowClearButton.Checked;
end;

procedure TfrmMain.actShowCloseButtonChange(Sender: TObject);
begin
  TableView.FindPanel.ShowCloseButton := actShowCloseButton.Checked;
end;

procedure TfrmMain.actShowFindButtonEChange(Sender: TObject);
begin
  TableView.FindPanel.ShowFindButton := actShowFindButton.Checked;
end;

procedure TfrmMain.actUseDelayedSearchExecute(Sender: TObject);
begin
  TableView.FindPanel.UseDelayedFind := actUseDelayedSearch.Checked;
end;

procedure TfrmMain.actUseExtendedSyntaxExecute(Sender: TObject);
begin
  TableView.FindPanel.UseExtendedSyntax := actUseExtendedSyntax.Checked;
end;

procedure TfrmMain.cbDisplayModePropertiesChange(Sender: TObject);
begin
  if cbeDisplayMode.ItemIndex = 0 then
    TableView.FindPanel.DisplayMode := fpdmNever
  else
    if cbeDisplayMode.ItemIndex = 1 then
      TableView.FindPanel.DisplayMode := fpdmManual
    else
      TableView.FindPanel.DisplayMode := fpdmAlways;
  actShowCloseButton.Enabled := not (TableView.FindPanel.DisplayMode = fpdmAlways);
end;

procedure TfrmMain.cbFindPanelPositionPropertiesChange(Sender: TObject);
begin
  if cbeFindPanelPosition.Text = 'Top' then
    TableView.FindPanel.Position := fppTop
  else
    TableView.FindPanel.Position := fppBottom;
end;

procedure TfrmMain.cdsOrdersCalcFields(DataSet: TDataSet);
begin
  cdsOrdersPaymentAmount.Value := cdsOrdersQuantity.Value * cdsOrdersUnitPrice.Value;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  cdsCustomers.Open;
  cdsOrders.Open;

  UpdateFindFilterColumns;

  TableView.Controller.ShowFindPanel;
end;

end.
