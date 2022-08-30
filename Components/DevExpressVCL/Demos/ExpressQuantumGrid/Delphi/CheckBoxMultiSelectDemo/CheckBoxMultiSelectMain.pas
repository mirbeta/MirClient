unit CheckBoxMultiSelectMain;

{$I cxVer.inc}

interface

uses
  SysUtils, Classes, Controls, Menus, DB, StdCtrls, BaseForm, ComCtrls, DBClient,
  dxCore, cxStyles, cxCustomData, cxGraphics, cxFilter, cxData, cxDataStorage,
  cxEdit, cxGridLevel, cxClasses, cxControls, cxGridCustomView, cxGridCustomTableView,
  cxGridTableView, cxGrid, cxEditRepositoryItems, cxLookAndFeels, cxLookAndFeelPainters,
  cxGridCardView, cxNavigator, cxContainer, cxGroupBox, ActnList, cxCheckBox, cxLabel,
  cxTextEdit, cxMaskEdit, cxSpinEdit, cxDropDownEdit, cxImageComboBox, cxDBData,
  cxGridDBTableView, XPMan, CarsDataForGrid, MidasLib, cxRadioGroup;

type
  TfrmMain = class(TfmBaseForm)
    erMain: TcxEditRepository;
    erMainFlag: TcxEditRepositoryImageItem;
    cxGroupBox1: TcxGroupBox;
    alAction: TActionList;
    actClearFindOnClose: TAction;
    actShowClearButton: TAction;
    actShowCloseButton: TAction;
    actShowFindButton: TAction;
    actHighlightSearchResults: TAction;
    actUseDelayedSearch: TAction;
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
    gbCheckBoxPosition: TcxGroupBox;
    rbFirstColumn: TcxRadioButton;
    rbIndicator: TcxRadioButton;
    cbShowCheckBoxesDynamically: TcxCheckBox;
    cbClearSelectionOnClickOutsideSelection: TcxCheckBox;
    cxGroupBox2: TcxGroupBox;
    cbDataRowCheckBoxVisible: TcxCheckBox;
    cbGroupRowCheckBoxVisible: TcxCheckBox;
    cbColumnHeaderCheckBoxSelectorVisible: TcxCheckBox;
    cbPersistentSelection: TcxCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure cdsOrdersCalcFields(DataSet: TDataSet);
    procedure CheckBoxPositionChanged(Sender: TObject);
    procedure cbGroupRowCheckBoxVisibilityChanged(Sender: TObject);
    procedure cbColumnHeaderCheckBoxVisibilityChanged(Sender: TObject);
    procedure cbShowCheckBoxesDynamicallyChanged(Sender: TObject);
    procedure cbClearSelectionWithAClickOutsideChanged(Sender: TObject);
    procedure cbDataRowCheckBoxVisibilityChanged(Sender: TObject);
    procedure cbPersistentSelectionPropertiesEditValueChanged(Sender: TObject);
    procedure FormShow(Sender: TObject);
  protected
    procedure UpdateCheckBoxesVisibility(AOption: TcxGridCheckBoxVisibilityOption; AInclude: Boolean);
  end;

var
  frmMain: TfrmMain;

implementation

uses
  Variants, AboutDemoForm;

{$R *.dfm}

procedure TfrmMain.UpdateCheckBoxesVisibility(AOption: TcxGridCheckBoxVisibilityOption; AInclude: Boolean);
begin
  if AInclude then
    TableView.OptionsSelection.CheckBoxVisibility := TableView.OptionsSelection.CheckBoxVisibility + [AOption]
  else
    TableView.OptionsSelection.CheckBoxVisibility := TableView.OptionsSelection.CheckBoxVisibility - [AOption];
end;

procedure TfrmMain.cbShowCheckBoxesDynamicallyChanged(Sender: TObject);
begin
  TableView.OptionsSelection.ShowCheckBoxesDynamically := cbShowCheckBoxesDynamically.Checked;
end;

procedure TfrmMain.cbClearSelectionWithAClickOutsideChanged(Sender: TObject);
begin
  TableView.OptionsSelection.ClearPersistentSelectionOnOutsideClick := cbClearSelectionOnClickOutsideSelection.Checked;
end;

procedure TfrmMain.cbDataRowCheckBoxVisibilityChanged(Sender: TObject);
begin
  UpdateCheckBoxesVisibility(cbvDataRow, cbDataRowCheckBoxVisible.Checked);
end;

procedure TfrmMain.cbGroupRowCheckBoxVisibilityChanged(Sender: TObject);
begin
  UpdateCheckBoxesVisibility(cbvGroupRow, cbGroupRowCheckBoxVisible.Checked);
end;

procedure TfrmMain.cbPersistentSelectionPropertiesEditValueChanged(Sender: TObject);
begin
  if cbPersistentSelection.Checked then
    TableView.OptionsSelection.MultiSelectMode := msmPersistent
  else
    TableView.OptionsSelection.MultiSelectMode := msmStandard;
end;

procedure TfrmMain.cbColumnHeaderCheckBoxVisibilityChanged(Sender: TObject);
begin
  UpdateCheckBoxesVisibility(cbvColumnHeader, cbColumnHeaderCheckBoxSelectorVisible.Checked);
end;

procedure TfrmMain.cdsOrdersCalcFields(DataSet: TDataSet);
begin
  cdsOrdersPaymentAmount.Value := cdsOrdersQuantity.Value * cdsOrdersUnitPrice.Value;
end;

procedure TfrmMain.CheckBoxPositionChanged(Sender: TObject);
begin
  if rbFirstColumn.Checked then
    TableView.OptionsSelection.CheckBoxPosition := cbpFirstColumn
  else
    TableView.OptionsSelection.CheckBoxPosition := cbpIndicator;
  cbShowCheckBoxesDynamically.Enabled := TableView.OptionsSelection.CheckBoxPosition = cbpFirstColumn;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  cdsCustomers.Open;
  cdsOrders.Open;
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  TableView.Controller.FocusRecord(2, True);
  TableView.DataController.SelectRows(4, 6);
  TableView.DataController.SelectRows(8, 10);
end;

end.
