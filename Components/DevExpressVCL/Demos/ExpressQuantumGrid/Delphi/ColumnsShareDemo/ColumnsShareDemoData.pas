unit ColumnsShareDemoData;

interface

uses
  SysUtils, Classes, DB, cxStyles, cxClasses, cxGridTableView, Forms,
  cxGridCardView, ImgList, Controls, DBClient, MidasLib;

type
  TColumnsShareDemoMainDM = class(TDataModule)
    tblProjects: TClientDataSet;
    dsProjects: TDataSource;
    dsItems: TDataSource;
    tblItems: TClientDataSet;
    tblUsers: TClientDataSet;
    dsUsers: TDataSource;
    dsProjectTeam: TDataSource;
    tblProjectTeam: TClientDataSet;
    dsDepartments: TDataSource;
    tblDepartments: TClientDataSet;
    tblUsersID: TAutoIncField;
    tblUsersFNAME: TStringField;
    tblUsersMNAME: TStringField;
    tblUsersLNAME: TStringField;
    tblUsersCOUNTRY: TStringField;
    tblUsersPOSTALCODE: TStringField;
    tblUsersCITY: TStringField;
    tblUsersADDRESS: TStringField;
    tblUsersPHONE: TStringField;
    tblUsersFAX: TStringField;
    tblUsersEMAIL: TStringField;
    tblUsersHOMEPAGE: TStringField;
    tblUsersDEPARTMENTID: TIntegerField;
    tblUsersUserName: TStringField;
    tblProjectTeamID: TAutoIncField;
    tblProjectTeamPROJECTID: TIntegerField;
    tblProjectTeamUSERID: TIntegerField;
    tblProjectTeamFUNCTION: TStringField;
    tblProjectsID: TAutoIncField;
    tblProjectsNAME: TStringField;
    tblProjectsMANAGERID: TIntegerField;
    tblItemsID: TAutoIncField;
    tblItemsNAME: TStringField;
    tblItemsTYPE: TBooleanField;
    tblItemsPROJECTID: TIntegerField;
    tblItemsPRIORITY: TSmallintField;
    tblItemsSTATUS: TSmallintField;
    tblItemsCREATORID: TIntegerField;
    tblItemsCREATEDDATE: TDateTimeField;
    tblItemsOWNERID: TIntegerField;
    tblItemsLASTMODIFIEDDATE: TDateTimeField;
    tblItemsFIXEDDATE: TDateTimeField;
    tblItemsDESCRIPTION: TMemoField;
    tblItemsRESOLUTION: TMemoField;
    imStat: TImageList;
    tblUsersDepartment: TStringField;
    procedure tblUsersCalcFields(DataSet: TDataSet);
    procedure DataModuleCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ColumnsShareDemoMainDM: TColumnsShareDemoMainDM;

implementation

{$R *.dfm}

procedure TColumnsShareDemoMainDM.tblUsersCalcFields(DataSet: TDataSet);
begin
  tblUsersUserName.AsString := tblUsersFNAME.AsString + ' ' + tblUsersLNAME.AsString;
end;

procedure TColumnsShareDemoMainDM.DataModuleCreate(Sender: TObject);
var
  APath: string;
begin
  APath := ExtractFilePath(Application.ExeName) + '..\..\Data\';
  tblDepartments.LoadFromFile(APath + 'Departments.xml');
  tblItems.LoadFromFile(APath + 'Items.xml');
  tblProjects.LoadFromFile(APath + 'Projects.xml');
  tblProjectTeam.LoadFromFile(APath + 'ProjectTeam.xml');
  tblUsers.LoadFromFile(APath + 'Users.xml');
end;

end.
