unit IssueListData;

interface

uses
  SysUtils, Classes, DB, Controls, ImgList, Forms,
  cxDBEditRepository, cxStyles, cxEdit, cxClasses, cxGridTableView,
  cxGridBandedTableView, cxLookAndFeels, cxContainer, cxEditRepositoryItems, DBClient, MidasLib;

type
  TdmMain = class(TDataModule)
    dsUsers: TDataSource;
    dsDepartments: TDataSource;
    dsItems: TDataSource;
    dsProjects: TDataSource;
    dsTeam: TDataSource;
    imStat: TImageList;
    ilMain: TImageList;
    edrepMain: TcxEditRepository;
    edrepUserLookup: TcxEditRepositoryLookupComboBoxItem;
    strepMain: TcxStyleRepository;
    stFixed: TcxStyle;
    stRejected: TcxStyle;
    stNew: TcxStyle;
    stPostponed: TcxStyle;
    stPreview: TcxStyle;
    ssTableStyles: TcxGridTableViewStyleSheet;
    stLightYellow: TcxStyle;
    stSelected: TcxStyle;
    dsScheduler: TDataSource;
    edrepUserFullName: TcxEditRepositoryLookupComboBoxItem;
    edrepDepartmentName: TcxEditRepositoryLookupComboBoxItem;
    edrepProjectName: TcxEditRepositoryLookupComboBoxItem;
    edstcMain: TcxEditStyleController;
    edrepItemStatus: TcxEditRepositoryImageComboBoxItem;
    edrepItemType: TcxEditRepositoryImageComboBoxItem;
    edrepItemPriority: TcxEditRepositoryImageComboBoxItem;
    stLightBlue: TcxStyle;
    stGold: TcxStyle;
    stBlue: TcxStyle;
    cdsUsers: TClientDataSet;
    cdsDepartments: TClientDataSet;
    cdsItems: TClientDataSet;
    cdsProjects: TClientDataSet;
    cdsTeam: TClientDataSet;
    cdsScheduler: TClientDataSet;
    cdsUsersID: TAutoIncField;
    cdsUsersUserName: TStringField;
    cdsUsersFNAME: TStringField;
    cdsUsersMNAME: TStringField;
    cdsUsersLNAME: TStringField;
    cdsUsersCOUNTRY: TStringField;
    cdsUsersPOSTALCODE: TStringField;
    cdsUsersCITY: TStringField;
    cdsUsersADDRESS: TStringField;
    cdsUsersPHONE: TStringField;
    cdsUsersFAX: TStringField;
    cdsUsersEMAIL: TStringField;
    cdsUsersHOMEPAGE: TStringField;
    cdsUsersDEPARTMENTID: TIntegerField;
    cdsUsersDepartment: TStringField;
    cdsDepartmentsID: TAutoIncField;
    cdsDepartmentsNAME: TStringField;
    cdsItemsID: TAutoIncField;
    cdsItemsNAME: TStringField;
    cdsItemsTYPE: TBooleanField;
    cdsItemsPROJECTID: TIntegerField;
    cdsItemsPRIORITY: TSmallintField;
    cdsItemsSTATUS: TSmallintField;
    cdsItemsCREATORID: TIntegerField;
    cdsItemsCREATEDDATE: TDateTimeField;
    cdsItemsOWNERID: TIntegerField;
    cdsItemsLASTMODIFIEDDATE: TDateTimeField;
    cdsItemsFIXEDDATE: TDateTimeField;
    cdsItemsDESCRIPTION: TMemoField;
    cdsItemsRESOLUTION: TMemoField;
    cdsTeamID: TAutoIncField;
    cdsTeamPROJECTID: TIntegerField;
    cdsTeamUSERID: TIntegerField;
    cdsTeamFUNCTION: TStringField;
    cdsProjectsID: TAutoIncField;
    cdsProjectsNAME: TStringField;
    cdsProjectsMANAGERID: TIntegerField;
    cdsSchedulerID: TAutoIncField;
    cdsSchedulerPROJECTID: TIntegerField;
    cdsSchedulerUSERID: TIntegerField;
    cdsSchedulerSUNDAY: TSmallintField;
    cdsSchedulerMONDAY: TSmallintField;
    cdsSchedulerTUESDAY: TSmallintField;
    cdsSchedulerWEDNESDAY: TSmallintField;
    cdsSchedulerTHURSDAY: TSmallintField;
    cdsSchedulerFRIDAY: TSmallintField;
    cdsSchedulerSATURDAY: TSmallintField;
    cdsSchedulerUserName: TStringField;
    cdsSchedulerFirstName: TStringField;
    cdsSchedulerMiddleName: TStringField;
    cdsSchedulerLastName: TStringField;
    cdsSchedulerRowSum: TFloatField;
    cdsSchedulerRowAvg: TFloatField;
    cdsUsersFullName: TStringField;
    procedure DataModuleCreate(Sender: TObject);
    procedure cdsSchedulerCalcFields(DataSet: TDataSet);
    procedure cdsUsersCalcFields(DataSet: TDataSet);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  dmMain: TdmMain;

implementation

{$R *.dfm}

uses
  DemoUtils;

procedure TdmMain.DataModuleCreate(Sender: TObject);
begin
  cdsDepartments.LoadFromFile('..\..\Data\Departments.xml');
  cdsDepartments.Open;
  cdsUsers.LoadFromFile('..\..\Data\Users.xml');
  cdsUsers.Open;
  cdsItems.LoadFromFile('..\..\Data\Items.xml');
  cdsItems.Open;
  cdsProjects.LoadFromFile('..\..\Data\Projects.xml');
  cdsProjects.Open;
  cdsTeam.LoadFromFile('..\..\Data\Team.xml');
  cdsTeam.Open;
  cdsScheduler.LoadFromFile('..\..\Data\Scheduler.xml');
  cdsScheduler.Open;
end;

procedure TdmMain.cdsSchedulerCalcFields(DataSet: TDataSet);
begin
 with DataSet do
 begin
  cdsSchedulerRowSum.AsFloat := cdsSchedulerSUNDAY.AsFloat + cdsSchedulerMONDAY.AsFloat +
    cdsSchedulerTUESDAY.AsFloat + cdsSchedulerWEDNESDAY.AsFloat +
    cdsSchedulerTHURSDAY.AsFloat + cdsSchedulerFRIDAY.AsFloat +
    cdsSchedulerSATURDAY.AsFloat;
  cdsSchedulerRowAvg.AsString := FormatFloat('0.00', cdsSchedulerRowSum.AsFloat / 7);
 end;
end;

procedure TdmMain.cdsUsersCalcFields(DataSet: TDataSet);
begin
  with DataSet do
    SetStringFieldValue(cdsUsersFullName, Format('%s %s %s', [cdsUsersFName.AsString,
      cdsUsersMName.AsString, cdsUsersLName.AsString]));
end;

end.
