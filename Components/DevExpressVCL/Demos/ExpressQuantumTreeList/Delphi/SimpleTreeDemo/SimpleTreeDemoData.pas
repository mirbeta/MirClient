unit SimpleTreeDemoData;

interface

uses
  SysUtils, cxClasses, cxStyles, cxTL, DB, Classes, Forms, dxmdaset;

type
  TSimpleTreeDemoDataDM = class(TDataModule)
    dsDepartments: TDataSource;
    StyleRepository: TcxStyleRepository;
    TreeListStyleSheetDevExpress: TcxTreeListStyleSheet;
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
    procedure SetParentValue(AValue: Variant);
  end;

var
  SimpleTreeDemoDataDM: TSimpleTreeDemoDataDM;

implementation

{$R *.dfm}

procedure TSimpleTreeDemoDataDM.SetParentValue(AValue: Variant);
begin
  if mdDepartments.State in [dsEdit, dsInsert] then
    mdDepartments.FindField('ParentID').Value := AValue;
end;

procedure TSimpleTreeDemoDataDM.DataModuleCreate(Sender: TObject);
const
  DataPath = '..\..\Data\';
begin
  mdDepartments.LoadFromBinaryFile(DataPath + 'Departments.dat');
  mdDepartments.Open;
end;

end.
