unit DragDropDemoData;

interface

uses
  SysUtils, cxClasses, cxStyles, cxTL, DB, Classes, Forms, DBClient, Provider, MidasLib;

type
  TDragDropDemoDataDM = class(TDataModule)
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
    stlGroupNode: TcxStyle;
    stlFixedBand: TcxStyle;
    TreeListStyleSheetDevExpress: TcxTreeListStyleSheet;
    dsDeptDict: TDataSource;
    dsPersDict: TDataSource;
    cdsDepartments: TClientDataSet;
    cdsDepartmentsID: TAutoIncField;
    cdsDepartmentsPARENTID: TIntegerField;
    cdsDepartmentsNAME: TStringField;
    cdsDepartmentsBUDGET: TFloatField;
    cdsDepartmentsPHONE: TStringField;
    cdsDepartmentsFAX: TStringField;
    cdsDepartmentsEMAIL: TStringField;
    cdsDepartmentsVACANCY: TBooleanField;
    cdsPersons: TClientDataSet;
    cdsPersonsID: TAutoIncField;
    cdsPersonsName: TStringField;
    cdsPersonsCountry: TStringField;
    cdsPersonsPostalCode: TStringField;
    cdsPersonsCity: TStringField;
    cdsPersonsAddress: TStringField;
    cdsPersonsPhone: TStringField;
    cdsPersonsFax: TStringField;
    cdsPersonsEMAIL: TStringField;
    cdsPersonsHOMEPAGE: TStringField;
    cdsPersonsDepartmentID: TIntegerField;
    cdsDeptDict: TClientDataSet;
    cdsPersDict: TClientDataSet;
    dspDepartments: TDataSetProvider;
    dspPersons: TDataSetProvider;
    cxStyle14: TcxStyle;
    cdsDeptData: TClientDataSet;
    cdsPersData: TClientDataSet;
    procedure DataModuleCreate(Sender: TObject);
    procedure cdsPersDictAfterPost(DataSet: TDataSet);
    procedure cdsDeptDictAfterPost(DataSet: TDataSet);
    procedure cdsDepartmentsAfterPost(DataSet: TDataSet);
    procedure cdsPersonsAfterPost(DataSet: TDataSet);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure SetParentValue(AValue: Variant);
  end;

var
  DragDropDemoDataDM: TDragDropDemoDataDM;

implementation

uses Math;

{$R *.dfm}

procedure TDragDropDemoDataDM.SetParentValue(AValue: Variant);
begin
  if cdsDepartments.State in [dsEdit, dsInsert] then
    cdsDepartments.FindField('ParentID').Value := AValue;
end;

procedure TDragDropDemoDataDM.DataModuleCreate(Sender: TObject);
begin
  cdsDeptData.Open;
  cdsPersData.Open;
end;

procedure TDragDropDemoDataDM.cdsPersDictAfterPost(DataSet: TDataSet);
begin
  cdsPersDict.ApplyUpdates(-1);
end;

procedure TDragDropDemoDataDM.cdsDeptDictAfterPost(DataSet: TDataSet);
begin
  cdsDeptDict.ApplyUpdates(-1);
end;

procedure TDragDropDemoDataDM.cdsDepartmentsAfterPost(DataSet: TDataSet);
begin
  cdsDepartments.ApplyUpdates(-1);
end;

procedure TDragDropDemoDataDM.cdsPersonsAfterPost(DataSet: TDataSet);
begin
  cdsPersons.ApplyUpdates(-1);
end;

end.
