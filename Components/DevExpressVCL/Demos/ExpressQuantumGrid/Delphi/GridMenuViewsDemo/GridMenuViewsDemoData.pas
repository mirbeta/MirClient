unit GridMenuViewsDemoData;

interface

uses

  Forms,

  SysUtils, Classes, DB, cxStyles, ImgList, Controls, cxClasses,
  cxGridTableView, DBClient, MidasLib;

type
  TGridMenuViewsDemoDataDM = class(TDataModule)
    dsOrders: TDataSource;
    dsCustomers: TDataSource;
    PaymentTypeImages: TImageList;
    cdsOrders: TClientDataSet;
    cdsCustomers: TClientDataSet;
    cdsOrdersID: TAutoIncField;
    cdsOrdersCustomerID: TIntegerField;
    cdsOrdersProductID: TIntegerField;
    cdsOrdersPurchaseDate: TDateTimeField;
    cdsOrdersTime: TDateTimeField;
    cdsOrdersPaymentType: TStringField;
    cdsOrdersDescription: TMemoField;
    cdsOrdersQuantity: TIntegerField;
    cdsOrdersPaymentAmount: TCurrencyField;
    cdsCustomersID: TAutoIncField;
    cdsCustomersFirstName: TStringField;
    cdsCustomersLastName: TStringField;
    cdsCustomersCompany: TStringField;
    cdsCustomersPrefix: TStringField;
    cdsCustomersTitle: TStringField;
    cdsCustomersAddress: TStringField;
    cdsCustomersCity: TStringField;
    cdsCustomersState: TStringField;
    cdsCustomersZipCode: TStringField;
    cdsCustomersSource: TStringField;
    cdsCustomersCustomer: TStringField;
    cdsCustomersHomePhone: TStringField;
    cdsCustomersFaxPhone: TStringField;
    cdsCustomersSpouse: TStringField;
    cdsCustomersOccupation: TStringField;
    cdsCustomersDescription: TMemoField;
    cdsCustomersEmail: TStringField;
    cdsOrdersPurchaseMonth: TStringField;
    procedure DataModuleCreate(Sender: TObject);
    procedure cdsOrdersCalcFields(DataSet: TDataSet);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  GridMenuViewsDemoDataDM: TGridMenuViewsDemoDataDM;

implementation

{$R *.dfm}

procedure TGridMenuViewsDemoDataDM.DataModuleCreate(Sender: TObject);
begin
  cdsOrders.LoadFromFile('..\..\Data\Orders.xml');
  cdsOrders.Open;
  cdsCustomers.LoadFromFile('..\..\Data\Customers.xml');
  cdsCustomers.Open;
end;

procedure TGridMenuViewsDemoDataDM.cdsOrdersCalcFields(DataSet: TDataSet);
var
  S: string;
begin
  DateTimeToString(S, 'mmmm', cdsOrdersPurchaseDate.Value);
  cdsOrdersPurchaseMonth.AsString := S;
end;

end.
