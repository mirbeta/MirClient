unit EditorsInPlaceValidationDemoData;

interface

uses
  SysUtils, Classes, DB, Forms, cxClasses, cxStyles, ImgList,
  Controls, cxVGrid, dxmdaset;

type
  TEditorsInPlaceValidationDemoDataDM = class(TDataModule)
    StyleRepository: TcxStyleRepository;
    styCaption: TcxStyle;
    cxVerticalGridStyleSheetDevExpress: TcxVerticalGridStyleSheet;
    cxStyle1: TcxStyle;
    cxStyle2: TcxStyle;
    cxStyle3: TcxStyle;
    cxStyle4: TcxStyle;
    cxStyle5: TcxStyle;
    cxStyle6: TcxStyle;
    cxStyle7: TcxStyle;
    dxMemData1: TdxMemData;
    DataSource: TDataSource;
    dxMemData1FirstName: TStringField;
    dxMemData1LastName: TStringField;
    dxMemData1PhoneNumber: TStringField;
    dxMemData1Email: TStringField;
    dxMemData1Address: TStringField;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  EditorsInPlaceValidationDemoDataDM: TEditorsInPlaceValidationDemoDataDM;

implementation

{$R *.dfm}

end.
