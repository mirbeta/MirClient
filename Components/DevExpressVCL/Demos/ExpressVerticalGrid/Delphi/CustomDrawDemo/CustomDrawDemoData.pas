unit CustomDrawDemoData;

interface

uses
  SysUtils, cxClasses, cxStyles, DB, Classes, cxVGrid, Forms, dxmdaset;

type
  TCustomDrawDemoDataDM = class(TDataModule)
    StyleRepository: TcxStyleRepository;
    stCustomer: TcxStyle;
    stNoCustomer: TcxStyle;
    dsOrders: TDataSource;
    cxVerticalGridStyleSheetDevExpress: TcxVerticalGridStyleSheet;
    cxStyle1: TcxStyle;
    cxStyle2: TcxStyle;
    cxStyle3: TcxStyle;
    cxStyle4: TcxStyle;
    cxStyle5: TcxStyle;
    cxStyle6: TcxStyle;
    cxStyle7: TcxStyle;
    mdOrders: TdxMemData;
    mdOrdersPurchaseDate: TDateTimeField;
    mdOrdersPaymentType: TStringField;
    mdOrdersPaymentAmount: TFloatField;
    mdOrdersQuantity: TIntegerField;
    mdOrdersFirstName: TStringField;
    mdOrdersLastName: TStringField;
    mdOrdersCompany: TStringField;
    mdOrdersPrefix: TStringField;
    mdOrdersTitle: TStringField;
    mdOrdersAddress: TStringField;
    mdOrdersCity: TStringField;
    mdOrdersState: TStringField;
    mdOrdersZipCode: TStringField;
    mdOrdersSource: TStringField;
    mdOrdersCustomer: TStringField;
    mdOrdersHomePhone: TStringField;
    mdOrdersFaxPhone: TStringField;
    mdOrdersSpouse: TStringField;
    mdOrdersOccupation: TStringField;
    mdOrdersEmail: TStringField;
    mdOrdersTrademark: TStringField;
    mdOrdersModel: TStringField;
    mdOrdersHP: TSmallintField;
    mdOrdersLiter: TFloatField;
    mdOrdersCyl: TSmallintField;
    mdOrdersTransmissSpeedCount: TSmallintField;
    mdOrdersTransmissAutomatic: TStringField;
    mdOrdersMPG_City: TSmallintField;
    mdOrdersMPG_Highway: TSmallintField;
    mdOrdersCategory: TStringField;
    mdOrdersCars_Description: TMemoField;
    mdOrdersHyperlink: TStringField;
    mdOrdersPicture: TBlobField;
    mdOrdersPrice: TFloatField;
    mdOrdersCustomers_ID: TIntegerField;
    mdOrdersCustomerID: TIntegerField;
    mdOrdersOrders_ID: TIntegerField;
    mdOrdersProductID: TIntegerField;
    mdOrdersCars_ID: TIntegerField;
    mdOrdersOrders_Time: TDateTimeField;
    procedure CustomDrawDemoDataDMCreate(Sender: TObject);
  end;

var
  CustomDrawDemoDataDM: TCustomDrawDemoDataDM;

implementation

{$R *.dfm}

procedure TCustomDrawDemoDataDM.CustomDrawDemoDataDMCreate(
  Sender: TObject);
begin
  mdOrders.LoadFromBinaryFile('..\..\Data\OrdersCarsCustomers.dat');
  mdOrders.Open;
end;

end.
