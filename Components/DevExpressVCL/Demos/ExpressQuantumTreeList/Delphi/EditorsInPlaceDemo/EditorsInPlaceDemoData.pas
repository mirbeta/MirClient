unit EditorsInPlaceDemoData;

interface

uses
  SysUtils, Classes, DB, Forms, cxClasses, cxStyles, cxTL, ImgList,
  Controls, dxmdaset;

type
  TEditorsInPlaceDemoDataDM = class(TDataModule)
    dsOrders: TDataSource;
    StyleRepository: TcxStyleRepository;
    cxStyle1: TcxStyle;
    cxStyle2: TcxStyle;
    cxStyle3: TcxStyle;
    cxStyle4: TcxStyle;
    cxStyle5: TcxStyle;
    cxStyle6: TcxStyle;
    cxStyle7: TcxStyle;
    cxStyle8: TcxStyle;
    cxStyle9: TcxStyle;
    cxStyle10: TcxStyle;
    cxStyle11: TcxStyle;
    cxStyle12: TcxStyle;
    cxStyle13: TcxStyle;
    TreeListStyleSheetDevExpress: TcxTreeListStyleSheet;
    dsCustomers: TDataSource;
    PaymentTypeImages: TImageList;
    styCaption: TcxStyle;
    mdOrders: TdxMemData;
    mdOrdersID: TAutoIncField;
    mdOrdersCustomerID: TIntegerField;
    mdOrdersProductID: TIntegerField;
    mdOrdersPurchaseDate: TDateTimeField;
    mdOrdersTime: TDateTimeField;
    mdOrdersPaymentType: TStringField;
    mdOrdersPaymentAmount: TCurrencyField;
    mdOrdersDescription: TMemoField;
    mdOrdersQuantity: TIntegerField;
    mdCustomers: TdxMemData;
    mdCustomersID: TAutoIncField;
    mdCustomersFirstName: TStringField;
    mdCustomersLastName: TStringField;
    mdCustomersCompany: TStringField;
    mdCustomersPrefix: TStringField;
    mdCustomersTitle: TStringField;
    mdCustomersAddress: TStringField;
    mdCustomersCity: TStringField;
    mdCustomersState: TStringField;
    mdCustomersZipCode: TStringField;
    mdCustomersSource: TStringField;
    mdCustomersCustomer: TStringField;
    mdCustomersHomePhone: TStringField;
    mdCustomersFaxPhone: TStringField;
    mdCustomersSpouse: TStringField;
    mdCustomersOccupation: TStringField;
    mdCustomersDescription: TMemoField;
    mdCustomersEmail: TStringField;
    mdOrdersCustomerEmail: TStringField;
    procedure DataModuleCreate(Sender: TObject);
    procedure mdOrdersCalcFields(DataSet: TDataSet);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  EditorsInPlaceDemoDataDM: TEditorsInPlaceDemoDataDM;

implementation

{$R *.dfm}

procedure TEditorsInPlaceDemoDataDM.DataModuleCreate(Sender: TObject);
const
  DataPath = '..\..\Data\';
begin
  mdCustomers.LoadFromBinaryFile(DataPath + 'Customers.dat');
  mdOrders.LoadFromBinaryFile(DataPath + 'Orders.dat');
  mdCustomers.Open;
  mdOrders.Open;
end;

procedure TEditorsInPlaceDemoDataDM.mdOrdersCalcFields(DataSet: TDataSet);
begin
  mdCustomers.Open;
  mdOrdersCustomerEmail.AsString := mdCustomers.Lookup('ID', mdOrdersCustomerID.Value, 'email');
end;

end.
