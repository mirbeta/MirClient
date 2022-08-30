unit ViewBandedDemoData;

interface

uses
  SysUtils, Classes, DB, cxStyles, cxClasses, cxGridBandedTableView, Forms,
  DemoUtils, dxmdaset;

type
  TViewBandedDemoDataDM = class(TDataModule)
    dsUSERS: TDataSource;
    dsPROJECTS: TDataSource;
    dsITEMS: TDataSource;
    mdProjects: TdxMemData;
    mdUsers: TdxMemData;
    mdItems: TdxMemData;
    mdProjectsID: TAutoIncField;
    mdProjectsNAME: TStringField;
    mdProjectsMANAGERID: TIntegerField;
    mdUsersID: TAutoIncField;
    mdUsersFNAME: TStringField;
    mdUsersMNAME: TStringField;
    mdUsersLNAME: TStringField;
    mdUsersCOUNTRY: TStringField;
    mdUsersPOSTALCODE: TStringField;
    mdUsersCITY: TStringField;
    mdUsersADDRESS: TStringField;
    mdUsersPHONE: TStringField;
    mdUsersFAX: TStringField;
    mdUsersEMAIL: TStringField;
    mdUsersHOMEPAGE: TStringField;
    mdUsersDEPARTMENTID: TIntegerField;
    mdItemsID: TAutoIncField;
    mdItemsNAME: TStringField;
    mdItemsTYPE: TBooleanField;
    mdItemsPROJECTID: TIntegerField;
    mdItemsPRIORITY: TSmallintField;
    mdItemsSTATUS: TSmallintField;
    mdItemsCREATORID: TIntegerField;
    mdItemsCREATEDDATE: TDateTimeField;
    mdItemsOWNERID: TIntegerField;
    mdItemsLASTMODIFIEDDATE: TDateTimeField;
    mdItemsFIXEDDATE: TDateTimeField;
    mdItemsDESCRIPTION: TMemoField;
    mdItemsRESOLUTION: TMemoField;
    mdUsersName: TStringField;
    procedure DataModuleCreate(Sender: TObject);
    procedure mdUsersCalcFields(DataSet: TDataSet);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ViewBandedDemoDataDM: TViewBandedDemoDataDM;

implementation

{$R *.dfm}

procedure TViewBandedDemoDataDM.DataModuleCreate(Sender: TObject);
begin
  mdProjects.LoadFromBinaryFile('..\..\Data\Projects.dat');
  mdItems.LoadFromBinaryFile('..\..\Data\Items.dat');
  mdUsers.LoadFromBinaryFile('..\..\Data\Users.dat');
  mdProjects.Open;
  mdItems.Open;
  mdUsers.Open;
end;

procedure TViewBandedDemoDataDM.mdUsersCalcFields(DataSet: TDataSet);
begin
  SetStringFieldValue(mdUSERSName, mdUSERSFNAME.Value + ' ' + mdUSERSLNAME.Value);
end;

end.
