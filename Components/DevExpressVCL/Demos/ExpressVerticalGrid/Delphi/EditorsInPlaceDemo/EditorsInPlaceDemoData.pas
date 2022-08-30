unit EditorsInPlaceDemoData;

interface

uses
  SysUtils, Classes, DB, Forms, cxClasses, cxStyles, ImgList,
  Controls, cxVGrid, dxmdaset;

type
  TEditorsInPlaceDemoDataDM = class(TDataModule)
    dsOrders: TDataSource;
    StyleRepository: TcxStyleRepository;
    dsCustomers: TDataSource;
    PaymentTypeImages: TImageList;
    styCaption: TcxStyle;
    cxVerticalGridStyleSheetDevExpress: TcxVerticalGridStyleSheet;
    cxStyle1: TcxStyle;
    cxStyle2: TcxStyle;
    cxStyle3: TcxStyle;
    cxStyle4: TcxStyle;
    cxStyle5: TcxStyle;
    cxStyle6: TcxStyle;
    cxStyle7: TcxStyle;
    mdOrders: TdxMemData;
    mdCustomers: TdxMemData;
    mdOrdersID: TAutoIncField;
    mdOrdersCustomerID: TIntegerField;
    mdOrdersProductID: TIntegerField;
    mdOrdersPurchaseDate: TDateTimeField;
    mdOrdersTime: TDateTimeField;
    mdOrdersPaymentType: TStringField;
    mdOrdersPaymentAmount: TFloatField;
    mdOrdersDescription: TMemoField;
    mdOrdersQuantity: TIntegerField;
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
begin
  mdCustomers.LoadFromBinaryFile('..\..\Data\Customers.dat');
  mdOrders.LoadFromBinaryFile('..\..\Data\Orders.dat');
  mdCustomers.Open;
  mdOrders.Open;
end;

end.
