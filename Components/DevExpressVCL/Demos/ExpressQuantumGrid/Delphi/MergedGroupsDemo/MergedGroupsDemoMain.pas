unit MergedGroupsDemoMain;

{$I cxVer.inc}

interface

uses
  SysUtils, Classes, Controls, Menus, DB, StdCtrls, BaseForm, ComCtrls, DBClient,
  dxCore, cxStyles, cxCustomData, cxGraphics, cxFilter, cxData, cxDataStorage,
  cxEdit, cxGridLevel, cxClasses, cxControls, cxGridCustomView, cxGridCustomTableView,
  cxGridTableView, cxGrid, cxEditRepositoryItems, cxLookAndFeels, cxLookAndFeelPainters,
  cxGridCardView, cxNavigator, cxContainer, cxGroupBox, ActnList, cxCheckBox, cxLabel,
  cxTextEdit, cxMaskEdit, cxSpinEdit, cxDropDownEdit, cxImageComboBox, cxDBData,
  cxGridDBTableView, XPMan, CarsDataForGrid, dxToggleSwitch, MidasLib,
  cxCheckGroup, dxmdaset, dxPSGlbl, dxPSUtl, dxPSEngn, dxPrnPg, dxBkgnd, dxWrap,
  dxPrnDev, dxPSCompsProvider, dxPSFillPatterns, dxPSEdgePatterns,
  dxPSPDFExportCore, dxPSPDFExport, cxDrawTextUtils, dxPSPrVwStd, dxPSPrVwAdv,
  dxPSPrVwRibbon, dxPScxPageControlProducer, dxPScxGridLnk,
  dxPScxGridLayoutViewLnk, dxPScxEditorProducers, dxPScxExtEditorProducers,
  dxPSCore, dxPScxCommon, cxButtons, Forms, Windows, cxDBLookupComboBox;

type
  TfrmMain = class(TfmBaseForm)
    erMain: TcxEditRepository;
    erMainFlag: TcxEditRepositoryImageItem;
    dsOrders: TDataSource;
    dsCustomers: TDataSource;
    Grid: TcxGrid;
    TableView: TcxGridDBTableView;
    TableViewCompanyName: TcxGridDBColumn;
    TableViewModel: TcxGridDBColumn;
    TableViewQuantity: TcxGridDBColumn;
    TableViewUnitPrice: TcxGridDBColumn;
    TableViewPaymentAmount: TcxGridDBColumn;
    TableViewPaymentType: TcxGridDBColumn;
    TableViewPurchaseDate: TcxGridDBColumn;
    GridLevel1: TcxGridLevel;
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
    cdsOrdersCarName: TStringField;
    cdsCustomers: TClientDataSet;
    cdsCustomersID: TIntegerField;
    cdsCustomersCompany: TStringField;
    TableViewTrademark: TcxGridDBColumn;
    procedure FormCreate(Sender: TObject);
    procedure TableViewDataControllerGroupingChanged(Sender: TObject);
    procedure cdsOrdersCalcFields(DataSet: TDataSet);
  end;

var
  frmMain: TfrmMain;

implementation

uses
  AboutDemoForm;

{$R *.dfm}

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

procedure TfrmMain.TableViewDataControllerGroupingChanged(Sender: TObject);
begin
  TableView.Controller.FocusedRowIndex := 0;
  TableView.ViewData.Expand(True);
end;

end.
