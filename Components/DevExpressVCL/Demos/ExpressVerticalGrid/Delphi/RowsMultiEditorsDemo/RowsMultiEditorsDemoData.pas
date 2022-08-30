unit RowsMultiEditorsDemoData;

interface

uses
  Forms, SysUtils, Classes, DB, cxStyles, ImgList, Controls,
  cxClasses, cxVGrid, dxmdaset;

type
  TRowsMultiEditorsDemoDataDM = class(TDataModule)
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
    mdOrdersOrders_Time: TDateTimeField;
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
    mdOrdersOrders_ID: TIntegerField;
    mdOrdersCars_ID: TIntegerField;
    procedure DataModuleCreate(Sender: TObject);
  end;

var
  RowsMultiEditorsDemoDataDM: TRowsMultiEditorsDemoDataDM;

implementation

{$R *.dfm}

procedure TRowsMultiEditorsDemoDataDM.DataModuleCreate(Sender: TObject);
begin
  mdOrders.LoadFromBinaryFile('..\..\Data\OrdersCarsCustomers.dat');
  mdOrders.Open;
end;

end.

