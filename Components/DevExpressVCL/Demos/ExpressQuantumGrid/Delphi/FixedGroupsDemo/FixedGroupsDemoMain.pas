unit FixedGroupsDemoMain;

{$I cxVer.inc}

interface

uses
  SysUtils, Classes, Controls, Menus, DB, StdCtrls, BaseForm, ComCtrls, DBClient,
  dxCore, cxStyles, cxCustomData, cxGraphics, cxFilter, cxData, cxDataStorage,
  cxEdit, cxGridLevel, cxClasses, cxControls, cxGridCustomView, cxGridCustomTableView,
  cxGridTableView, cxGrid, cxEditRepositoryItems, cxLookAndFeels, cxLookAndFeelPainters,
  cxGridCardView, cxNavigator, cxContainer, cxGroupBox, ActnList, cxCheckBox, cxLabel,
  cxTextEdit, cxMaskEdit, cxSpinEdit, cxDropDownEdit, cxImageComboBox, cxDBData,
  cxGridDBTableView, XPMan, CarsDataForGrid, dxToggleSwitch, MidasLib;

type
  TfrmMain = class(TfmBaseForm)
    erMain: TcxEditRepository;
    erMainFlag: TcxEditRepositoryImageItem;
    gbOptions: TcxGroupBox;
    alAction: TActionList;
    acFixedGroups: TAction;
    miFixedGroupsOptions: TMenuItem;
    miFixedGroups: TMenuItem;
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
    tsFixedGroups: TdxToggleSwitch;
    procedure FormCreate(Sender: TObject);
    procedure acFixedGroupsExecute(Sender: TObject);
    procedure cdsOrdersCalcFields(DataSet: TDataSet);
  end;

var
  frmMain: TfrmMain;

implementation

uses
  Variants, AboutDemoForm;

{$R *.dfm}

procedure TfrmMain.acFixedGroupsExecute(Sender: TObject);
begin
  TableView.OptionsBehavior.FixedGroups := not TableView.OptionsBehavior.FixedGroups;
end;

procedure TfrmMain.cdsOrdersCalcFields(DataSet: TDataSet);
begin
  cdsOrdersPaymentAmount.Value := cdsOrdersQuantity.Value * cdsOrdersUnitPrice.Value;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  cdsCustomers.Open;
  cdsOrders.Open;

  ActiveControl := Grid;
  TableView.Controller.FocusedRowIndex := 0;
  TableView.ViewData.Expand(True);
end;

end.
