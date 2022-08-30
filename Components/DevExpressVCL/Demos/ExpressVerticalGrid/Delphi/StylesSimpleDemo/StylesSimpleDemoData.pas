unit StylesSimpleDemoData;

interface

uses
  Forms, SysUtils, Classes, DB, cxStyles, ImgList, Controls,
  cxClasses, cxVGrid, dxmdaset;

type
  TStylesSimpleDemoDataDM = class(TDataModule)
    dsOrders: TDataSource;
    StyleRepository: TcxStyleRepository;
    Sunny: TcxStyle;
    Dark: TcxStyle;
    Golden: TcxStyle;
    Summer: TcxStyle;
    Autumn: TcxStyle;
    Bright: TcxStyle;
    Cold: TcxStyle;
    Spring: TcxStyle;
    Light: TcxStyle;
    Winter: TcxStyle;
    Depth: TcxStyle;
    UserStyleSheet: TcxVerticalGridStyleSheet;
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
    procedure StylesSimpleDemoDataDMCreate(Sender: TObject);
  end;

var
  StylesSimpleDemoDataDM: TStylesSimpleDemoDataDM;

implementation

{$R *.dfm}

procedure TStylesSimpleDemoDataDM.StylesSimpleDemoDataDMCreate(
  Sender: TObject);
begin
  mdOrders.LoadFromBinaryFile('..\..\Data\OrdersCarsCustomers.dat');
  mdOrders.Open;
end;

end.

