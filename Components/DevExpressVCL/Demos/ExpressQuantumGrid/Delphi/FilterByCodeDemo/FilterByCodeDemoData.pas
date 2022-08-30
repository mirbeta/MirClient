unit FilterByCodeDemoData;

interface

uses
  Forms, Classes, SysUtils, DB, ImgList, Controls, cxDBEditRepository,
  cxEditRepositoryItems, cxClasses, cxStyles, cxGridTableView, cxEdit, DBClient, MidasLib;

type
  TFilterByCodeDemoMainDM = class(TDataModule)
    tblCustomers: TClientDataSet;
    dsCustomers: TDataSource;
    StyleRepository: TcxStyleRepository;
    GridTableViewStyleSheetDevExpress: TcxGridTableViewStyleSheet;
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
    cxStyle14: TcxStyle;
    tblProducts: TClientDataSet;
    edrepMain: TcxEditRepository;
    edrepDXPaymentTypeImageCombo: TcxEditRepositoryImageComboBoxItem;
    imPaytTypes: TImageList;
    dsProducts: TDataSource;
    edrepDXLookupProducts: TcxEditRepositoryLookupComboBoxItem;
    procedure DataModuleCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FilterByCodeDemoMainDM: TFilterByCodeDemoMainDM;

implementation

{$R *.dfm}

procedure TFilterByCodeDemoMainDM.DataModuleCreate(Sender: TObject);
var
  APath: string;
begin
  APath := ExtractFilePath(Application.ExeName) + '..\..\Data\';
  tblCustomers.LoadFromFile(APath + 'Customer.xml');
  tblProducts.LoadFromFile(APath + 'Products.xml');
end;

end.
