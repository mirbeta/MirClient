unit FixedDataRowsMain;

{$I cxVer.inc}

interface

uses
  SysUtils, Classes, Controls, Menus, DB, StdCtrls, BaseForm, ComCtrls, DBClient,
  dxCore, cxStyles, cxCustomData, cxGraphics, cxFilter, cxData, cxDataStorage,
  cxEdit, cxGridLevel, cxClasses, cxControls, cxGridCustomView, cxGridCustomTableView,
  cxGridTableView, cxGrid, cxEditRepositoryItems, cxLookAndFeels, cxLookAndFeelPainters,
  cxGridCardView, cxNavigator, cxContainer, cxGroupBox, ActnList, cxCheckBox, cxLabel,
  cxTextEdit, cxMaskEdit, cxSpinEdit, cxDropDownEdit, cxImageComboBox, cxDBData,
  cxGridDBTableView, XPMan, CarsDataForGrid, MidasLib, cxRadioGroup, cxTrackBar;

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
    gbPinClickAction: TcxGroupBox;
    rbShowPopup: TcxRadioButton;
    rbNone: TcxRadioButton;
    gbPinVisibility: TcxGroupBox;
    rbPinVisibilityNever: TcxRadioButton;
    rbPinVisibilityAlways: TcxRadioButton;
    rbPinVisibilityRowHover: TcxRadioButton;
    rbPinVisibilityHover: TcxRadioButton;
    rbFixRowToBottom: TcxRadioButton;
    rbFixRowToTop: TcxRadioButton;
    tbSeparatorWidth: TcxTrackBar;
    lbSeparatorWidth: TcxLabel;
    procedure FormCreate(Sender: TObject);
    procedure PinVisibilityChanged(Sender: TObject);
    procedure FixationCapabilityChanged(Sender: TObject);
    procedure tbSeparatorWidthPropertiesChange(Sender: TObject);
  protected
    procedure UpdateFixationCapability;
    procedure UpdatePinVisibility;
  end;

var
  frmMain: TfrmMain;

implementation

uses
  Variants, AboutDemoForm;

{$R *.dfm}

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  cdsCustomers.Open;
  cdsOrders.Open;
end;

procedure TfrmMain.FixationCapabilityChanged(Sender: TObject);
begin
  UpdateFixationCapability;
end;

procedure TfrmMain.PinVisibilityChanged(Sender: TObject);
begin
  UpdatePinVisibility;
end;

procedure TfrmMain.tbSeparatorWidthPropertiesChange(Sender: TObject);
begin
  TableView.FixedDataRows.SeparatorWidth := tbSeparatorWidth.Position;
end;

procedure TfrmMain.UpdateFixationCapability;
begin
  if rbShowPopup.Checked then
    TableView.FixedDataRows.PinClickAction := rpcaShowPopup
  else
    if rbFixRowToTop.Checked then
      TableView.FixedDataRows.PinClickAction := rpcaFixToTop
    else
      if rbFixRowToBottom.Checked then
        TableView.FixedDataRows.PinClickAction := rpcaFixToBottom
      else
        TableView.FixedDataRows.PinClickAction := rpcaNone;
end;

procedure TfrmMain.UpdatePinVisibility;
begin
  if rbPinVisibilityNever.Checked then
    TableView.FixedDataRows.PinVisibility := rpvNever
  else
    if rbPinVisibilityAlways.Checked then
      TableView.FixedDataRows.PinVisibility := rpvAlways
    else
      if rbPinVisibilityHover.Checked then
        TableView.FixedDataRows.PinVisibility := rpvHotTrack
      else
        TableView.FixedDataRows.PinVisibility := rpvRowHotTrack;
end;

end.
