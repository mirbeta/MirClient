unit SummariesDemoData;

interface

uses
  SysUtils, Classes, DB, Forms, cxClasses, cxStyles, cxTL, ImgList,
  Controls, dxmdaset;

type
  TSummariesDemoDataDM = class(TDataModule)
    StyleRepository: TcxStyleRepository;
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
    stlGroupNode: TcxStyle;
    stlFixedBand: TcxStyle;
    TreeListStyleSheetDevExpress: TcxTreeListStyleSheet;
    dsDepartments: TDataSource;
    mdDepartments: TdxMemData;
    mdDepartmentsID: TAutoIncField;
    mdDepartmentsPARENTID: TIntegerField;
    mdDepartmentsMANAGERID: TIntegerField;
    mdDepartmentsNAME: TStringField;
    mdDepartmentsBUDGET: TFloatField;
    mdDepartmentsLOCATION: TStringField;
    mdDepartmentsPHONE: TStringField;
    mdDepartmentsFAX: TStringField;
    mdDepartmentsEMAIL: TStringField;
    mdDepartmentsVACANCY: TBooleanField;
    procedure DataModuleCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  SummariesDemoDataDM: TSummariesDemoDataDM;

implementation

{$R *.dfm}

procedure TSummariesDemoDataDM.DataModuleCreate(Sender: TObject);
const
  DataPath = '..\..\Data\';
begin
  mdDepartments.LoadFromBinaryFile(DataPath + 'Departments.dat');
  mdDepartments.Open;
end;

end.
