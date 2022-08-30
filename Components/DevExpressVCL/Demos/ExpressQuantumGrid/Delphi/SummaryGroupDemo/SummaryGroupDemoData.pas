unit SummaryGroupDemoData;

interface

uses
  Forms,
  SysUtils, Classes, DB, cxStyles, ImgList, Controls, DemoUtils,
  dxmdaset, cxClasses;

type
  TSummaryGroupDemoDataDM = class(TDataModule)
    dsCars: TDataSource;
    dsOrders: TDataSource;
    dsCustomers: TDataSource;
    StyleRepository: TcxStyleRepository;
    stBlueLight: TcxStyle;
    stGreyLight: TcxStyle;
    stBlueSky: TcxStyle;
    PaymentTypeImages: TImageList;
    stClear: TcxStyle;
    stRed: TcxStyle;
    mdCars: TdxMemData;
    mdOrders: TdxMemData;
    mdCustomers: TdxMemData;
    mdCarsID: TAutoIncField;
    mdCarsTrademark: TStringField;
    mdCarsModel: TStringField;
    mdCarsHP: TSmallintField;
    mdCarsLiter: TFloatField;
    mdCarsCyl: TSmallintField;
    mdCarsTransmissSpeedCount: TSmallintField;
    mdCarsTransmissAutomatic: TStringField;
    mdCarsMPG_City: TSmallintField;
    mdCarsMPG_Highway: TSmallintField;
    mdCarsCategory: TStringField;
    mdCarsDescription: TMemoField;
    mdCarsHyperlink: TStringField;
    mdCarsPicture: TBlobField;
    mdCarsPrice: TFloatField;
    mdOrdersID: TAutoIncField;
    mdOrdersCustomerID: TIntegerField;
    mdOrdersProductID: TIntegerField;
    mdOrdersPurchaseDate: TDateTimeField;
    mdOrdersTime: TDateTimeField;
    mdOrdersPaymentType: TStringField;
    mdOrdersPaymentAmount: TCurrencyField;
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
    mdCarsCarName: TStringField;
    mdOrdersPurchaseMonth: TStringField;
    procedure mdCarsCalcFields(DataSet: TDataSet);
    procedure mdOrdersCalcFields(DataSet: TDataSet);
    procedure DataModuleCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  SummaryGroupDemoDataDM: TSummaryGroupDemoDataDM;

implementation

{$R *.dfm}

procedure TSummaryGroupDemoDataDM.mdCarsCalcFields(DataSet: TDataSet);
begin
  SetStringFieldValue(mdCarsCarName,
    mdCarsTrademark.Value + ' ' +  mdCarsModel.Value);
end;

procedure TSummaryGroupDemoDataDM.mdOrdersCalcFields(DataSet: TDataSet);
var
  s: string;
begin
  DateTimeToString(s, 'mmmm', mdOrdersPurchaseDate.AsDateTime);
  SetStringFieldValue(mdOrdersPurchaseMonth, s);
end;

procedure TSummaryGroupDemoDataDM.DataModuleCreate(Sender: TObject);
begin
  mdCars.LoadFromBinaryFile('..\..\Data\CarsModel.dat');
  mdOrders.LoadFromBinaryFile('..\..\Data\Orders.dat');
  mdCustomers.LoadFromBinaryFile('..\..\Data\Customers.dat');
  mdCars.Open;
  mdOrders.Open;
  mdCustomers.Open;
end;

end.
