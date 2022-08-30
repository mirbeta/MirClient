unit BandedFixedDemoData;

{$I cxVer.inc}

interface

uses
  SysUtils, cxClasses, cxStyles, cxTL, DB, Classes, Variants, Forms, DBClient,
  dxmdaset;

type
  TBandedFixedDemoDataDM = class(TDataModule)
    dsSheduler: TDataSource;
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
    dsProjects: TDataSource;
    stlGroupNode: TcxStyle;
    stlFixedBand: TcxStyle;
    mdSheduler: TdxMemData;
    mdShedulerID: TAutoIncField;
    mdShedulerPROJECTID: TIntegerField;
    mdShedulerProjectManagerID: TIntegerField;
    mdShedulerUSERID: TIntegerField;
    mdShedulerSUNDAY: TSmallintField;
    mdShedulerMONDAY: TSmallintField;
    mdShedulerTUESDAY: TSmallintField;
    mdShedulerWEDNESDAY: TSmallintField;
    mdShedulerTHURSDAY: TSmallintField;
    mdShedulerFRIDAY: TSmallintField;
    mdShedulerSATURDAY: TSmallintField;
    mdShedulerWeekSum: TIntegerField;
    mdShedulerWeekAVG: TFloatField;
    mdPersons: TdxMemData;
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
    mdProjects: TdxMemData;
    mdProjectsID: TAutoIncField;
    mdProjectsNAME: TStringField;
    mdProjectsMANAGERID: TIntegerField;
    procedure DataModuleCreate(Sender: TObject);
    procedure mdShedulerCalcFields(DataSet: TDataSet);
  public
    { Public declarations }
    function GetProjectNameByID(AProjectID: Integer): string;
    function GetPersonNameByID(APersonID: Integer): string;
  end;

var
  BandedFixedDemoDataDM: TBandedFixedDemoDataDM;

implementation

{$R *.dfm}

{ TBandedFixedDemoDataDM }

function TBandedFixedDemoDataDM.GetPersonNameByID(APersonID: Integer): string;
begin
  Result := mdPersons.Lookup('ID', APersonID, 'Name');
end;

function TBandedFixedDemoDataDM.GetProjectNameByID(AProjectID: Integer): string;
begin
  Result := mdProjects.Lookup('ID', AProjectID, 'Name');
end;

procedure TBandedFixedDemoDataDM.DataModuleCreate(Sender: TObject);
const
  DataPath = '..\..\Data\';
begin
  mdProjects.LoadFromBinaryFile(DataPath + 'Projects.dat');
  mdPersons.LoadFromBinaryFile(DataPath + 'Persons.dat');
  mdSheduler.LoadFromBinaryFile(DataPath + 'Scheduler.dat');
  mdProjects.Open;
  mdPersons.Open;
  mdSheduler.Open;
end;

procedure TBandedFixedDemoDataDM.mdShedulerCalcFields(DataSet: TDataSet);
begin
  mdShedulerWeekSum.Value := mdShedulerSUNDAY.Value + mdShedulerMONDAY.Value +
    mdShedulerTUESDAY.Value + mdShedulerWEDNESDAY.Value + mdShedulerTHURSDAY.Value +
    mdShedulerFRIDAY.Value + mdShedulerSATURDAY.Value;
  mdShedulerWeekAVG.Value := mdShedulerWeekSum.Value/7;
end;

end.
