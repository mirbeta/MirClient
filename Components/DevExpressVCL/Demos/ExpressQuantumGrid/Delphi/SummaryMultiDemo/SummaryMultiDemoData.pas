unit SummaryMultiDemoData;

interface

uses
  Forms, SysUtils, Classes, DB, cxStyles, ImgList, Controls, DemoUtils, cxClasses, DBClient, MidasLib;

type
  TSummaryMultiDemoDataDM = class(TDataModule)
    dsOrders: TDataSource;
    dsCustomers: TDataSource;
    tblOrders: TClientDataSet;
    tblCustomers: TClientDataSet;
    tblCustomersID: TIntegerField;
    tblCustomersFirstName: TStringField;
    tblCustomersLastName: TStringField;
    tblCustomersCompany: TStringField;
    StyleRepository: TcxStyleRepository;
    styleGold: TcxStyle;
    styleBlueLight: TcxStyle;
    styleYellowLight: TcxStyle;
    PaymentTypeImages: TImageList;
    tblOrdersID: TAutoIncField;
    tblOrdersCustomerID: TIntegerField;
    tblOrdersProductID: TIntegerField;
    tblOrdersPurchaseDate: TDateTimeField;
    tblOrdersTime: TDateTimeField;
    tblOrdersPaymentType: TStringField;
    tblOrdersDescription: TMemoField;
    tblOrdersQuantity: TIntegerField;
    tblOrdersPaymentAmount: TCurrencyField;
    styleBlue: TcxStyle;
    styleRed: TcxStyle;
    styleNormal: TcxStyle;
    styleSortedSummary: TcxStyle;
    procedure DataModuleCreate(Sender: TObject);
  end;

var
  SummaryMultiDemoDataDM: TSummaryMultiDemoDataDM;

implementation

{$R *.dfm}

procedure TSummaryMultiDemoDataDM.DataModuleCreate(Sender: TObject);
var
  APath: string;
begin
  APath := ExtractFilePath(Application.ExeName) + '..\..\Data\';
  tblCustomers.LoadFromFile(APath + 'Customers.xml');
  tblOrders.LoadFromFile(APath + 'Orders.xml');
end;

end.
