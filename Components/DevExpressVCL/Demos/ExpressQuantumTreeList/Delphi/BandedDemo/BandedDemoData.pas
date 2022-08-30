unit BandedDemoData;

{$I cxVer.inc}

interface

uses
  SysUtils, cxClasses, cxStyles, cxTL, DB, Classes, Variants, Forms, dxmdaset;

type
  TBandedDemoDataDM = class(TDataModule)
    dsDepartments: TDataSource;
    dsPersons: TDataSource;
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
    TreeListStyleSheetDevExpress: TcxTreeListStyleSheet;
    mdDepartments: TdxMemData;
    mdPersons: TdxMemData;
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
    mdPersonsID: TAutoIncField;
    mdPersonsName: TStringField;
    mdPersonsCountry: TStringField;
    mdPersonsPostalCode: TStringField;
    mdPersonsCity: TStringField;
    mdPersonsAddress: TStringField;
    mdPersonsPhone: TStringField;
    mdPersonsFax: TStringField;
    mdPersonsEMAIL: TStringField;
    mdPersonsHOMEPAGE: TStringField;
    mdPersonsDepartmentID: TIntegerField;
    mdDepartmentsManager: TStringField;
    mdDepartmentsManagerPhone: TStringField;
    mdDepartmentsManagerEmail: TStringField;
    procedure DataModuleCreate(Sender: TObject);
  public
    { Public declarations }
    procedure SetParentValue(AValue: Variant);
  end;

var
  BandedDemoDataDM: TBandedDemoDataDM;

implementation

{$R *.dfm}

procedure TBandedDemoDataDM.SetParentValue(AValue: Variant);
begin
  if mdDepartments.State in [dsEdit, dsInsert] then
    mdDepartments.FindField('ParentID').Value := AValue;
end;

procedure TBandedDemoDataDM.DataModuleCreate(Sender: TObject);
const
  DataPath = '..\..\Data\';
begin
  mdDepartments.Close;
  mdPersons.Close;
  mdPersons.LoadFromBinaryFile(DataPath + 'Persons.dat');
  mdDepartments.LoadFromBinaryFile(DataPath + 'Departments.dat');
  mdPersons.Open;
  mdDepartments.Open;
end;

end.
