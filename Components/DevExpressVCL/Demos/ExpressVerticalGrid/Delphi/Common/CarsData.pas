unit CarsData;

{$I cxVer.inc}

interface

uses
  SysUtils, Classes, DB, dxmdaset, cxClasses, cxEdit, cxEditRepositoryItems, cxDBEditRepository, cxStyles,
  cxCustomData, cxGraphics, cxFilter, cxData, cxDataStorage, cxNavigator, cxControls, cxHyperLinkEdit, cxDBData, cxMemo;

type

  { TdmCars }

  TdmCars = class(TDataModule)
    dsBodyStyle: TDataSource;
    dsCategory: TDataSource;
    dsModels: TDataSource;
    dsTrademark: TDataSource;
    dsTransmissionType: TDataSource;
    EditRepository: TcxEditRepository;
    EditRepositoryBodyStyleLookup: TcxEditRepositoryLookupComboBoxItem;
    EditRepositoryCategoryLookup: TcxEditRepositoryLookupComboBoxItem;
    EditRepositoryImage: TcxEditRepositoryImageItem;
    EditRepositoryImageBlob: TcxEditRepositoryBlobItem;
    EditRepositoryMemo: TcxEditRepositoryMemoItem;
    EditRepositoryMemoBlob: TcxEditRepositoryBlobItem;
    EditRepositoryTransmissionTypeCheckBox: TcxEditRepositoryCheckBoxItem;
    EditRepositoryTransmissionTypeLookup: TcxEditRepositoryLookupComboBoxItem;
    mdBodyStyle: TdxMemData;
    mdBodyStyleID: TIntegerField;
    mdBodyStyleName: TWideStringField;
    mdCategory: TdxMemData;
    mdCategoryID: TIntegerField;
    mdCategoryName: TWideStringField;
    mdCategoryPicture: TBlobField;
    mdModels: TdxMemData;
    mdModelsBodyStyleID: TIntegerField;
    mdModelsCategoryID: TIntegerField;
    mdModelsCilinders: TIntegerField;
    mdModelsDelivery_Date: TDateTimeField;
    mdModelsDescription: TWideMemoField;
    mdModelsDoors: TIntegerField;
    mdModelsFullName: TWideStringField;
    mdModelsHorsepower: TWideStringField;
    mdModelsHyperlink: TStringField;
    mdModelsID: TIntegerField;
    mdModelsImage: TBlobField;
    mdModelsInStock: TBooleanField;
    mdModelsModification: TWideStringField;
    mdModelsMPG_City: TIntegerField;
    mdModelsMPG_Highway: TIntegerField;
    mdModelsName: TWideStringField;
    mdModelsPhoto: TBlobField;
    mdModelsPrice: TBCDField;
    mdModelsTorque: TWideStringField;
    mdModelsTrademark: TWideStringField;
    mdModelsTrademarkID: TIntegerField;
    mdModelsTransmission_Speeds: TWideStringField;
    mdModelsTransmission_Type: TIntegerField;
    mdModelsTransmissionTypeName: TStringField;
    mdTrademark: TdxMemData;
    mdTrademarkDescription: TWideMemoField;
    mdTrademarkID: TIntegerField;
    mdTrademarkLogo: TBlobField;
    mdTrademarkName: TWideStringField;
    mdTrademarkSite: TWideStringField;
    mdTransmissionType: TdxMemData;
    mdTransmissionTypeID: TIntegerField;
    mdTransmissionTypeName: TWideStringField;
    mdModelsCategory: TStringField;
    mdModelsBodyStyle: TStringField;
    mdCarOrders: TdxMemData;
    mdCarOrdersID: TIntegerField;
    mdCarOrdersTrademark: TStringField;
    mdCarOrdersName: TWideStringField;
    mdCarOrdersModification: TWideStringField;
    mdCarOrdersPrice: TBCDField;
    mdCarOrdersMPG_City: TIntegerField;
    mdCarOrdersMPG_Highway: TIntegerField;
    mdCarOrdersBodyStyleID: TIntegerField;
    mdCarOrdersCilinders: TIntegerField;
    mdCarOrdersSalesDate: TDateField;
    mdCarOrdersBodyStyle: TStringField;
    dsCarOrders: TDataSource;
    mdCarOrdersParentID: TIntegerField;

    procedure DataModuleCreate(Sender: TObject);
    procedure mdModelsCalcFields(DataSet: TDataSet);
  end;

var
  dmCars: TdmCars;

implementation

uses
  Forms;

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

procedure TdmCars.DataModuleCreate(Sender: TObject);
var
  APath: string;
begin
  APath := ExtractFilePath(Application.ExeName) + '..\..\Data\';
  mdBodyStyle.LoadFromBinaryFile(APath + 'CarsBodyStyle.dat');
  mdCategory.LoadFromBinaryFile(APath + 'CarsCategory.dat');
  mdModels.LoadFromBinaryFile(APath + 'CarsModel.dat');
  mdTrademark.LoadFromBinaryFile(APath + 'CarsTrademark.dat');
  mdTransmissionType.LoadFromBinaryFile(APath + 'CarsTransmissionType.dat');
  mdCarOrders.LoadFromBinaryFile(APath + 'CarOrders.dat');

  mdBodyStyle.Active := True;
  mdCategory.Active := True;
  mdTrademark.Active := True;
  mdTransmissionType.Active := True;
  mdModels.Active := True;
end;

procedure TdmCars.mdModelsCalcFields(DataSet: TDataSet);
begin
  mdModelsFullName.Value := mdModelsTrademark.Value + ' ' + mdModelsName.Value;
end;

end.
