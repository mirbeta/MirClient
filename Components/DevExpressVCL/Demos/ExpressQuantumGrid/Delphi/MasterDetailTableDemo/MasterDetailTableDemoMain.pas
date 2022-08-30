unit MasterDetailTableDemoMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, cxGridLevel, cxControls, cxGrid, StdCtrls, ExtCtrls, Menus,
  ImgList, ActnList, cxGridCustomTableView, cxGridTableView,
  cxGridDBTableView, cxGridCustomView, ComCtrls, cxStyles, cxCustomData,
  cxGraphics, cxFilter, cxData, cxEdit, DB, cxDBData, cxClasses,
  cxDataStorage, cxDBLookupComboBox, cxBlobEdit, cxLookAndFeels,
  cxLookAndFeelPainters, BaseForm, cxContainer, cxLabel, cxGridCardView;

type
  TMasterDetailTableDemoMainForm = class(TfmBaseForm)
    pnlDetail: TPanel;
    lblDetail: TcxLabel;
    lblStyle: TcxLabel;
    lblMaster: TcxLabel;
    Bevel1: TBevel;
    GridDetail: TcxGrid;
    lvDetail: TcxGridLevel;
    tvFilms: TcxGridDBTableView;
    lvFilms: TcxGridLevel;
    Grid: TcxGrid;
    lvFilmsPersonsStaff: TcxGridLevel;
    tvFilmsCAPTION: TcxGridDBColumn;
    tvFilmsYEAR: TcxGridDBColumn;
    tvFilmsTAGLINE: TcxGridDBColumn;
    tvFilmsPLOTOUTLINE: TcxGridDBColumn;
    tvFilmsRUNTIME: TcxGridDBColumn;
    tvFilmsPHOTO: TcxGridDBColumn;
    miOptions: TMenuItem;
    miGrid: TMenuItem;
    Splitter: TSplitter;
    tvFilmsPersonsStaff: TcxGridDBTableView;
    tvFilmsPersonsStaffPERSONLINEID: TcxGridDBColumn;
    tvFilmsPersonsStaffPERSONID: TcxGridDBColumn;
    tvFilmsPersonsStaffDESCRIPTION: TcxGridDBColumn;
    procedure miGridClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure SetStandardMasterDetailStyle; // Standard two grid style
    procedure SetGridMasterDetailStyle;    // Grid single grid style
  protected
    procedure LookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues); override;
  end;

var
  MasterDetailTableDemoMainForm: TMasterDetailTableDemoMainForm;

implementation

{$R *.dfm}

uses
  FilmsDemoData, cxGridDBDataDefinitions, AboutDemoForm;

procedure TMasterDetailTableDemoMainForm.miGridClick;
begin
  if GetMenuItemChecked(Sender) then
    SetGridMasterDetailStyle
  else
    SetStandardMasterDetailStyle;
end;

procedure TMasterDetailTableDemoMainForm.LookAndFeelChanged(
  Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues);
begin
  inherited LookAndFeelChanged(Sender, AChangedValues);
  UpdateTableViewStyleSheet(tvFilms);
  UpdateTableViewStyleSheet(tvFilmsPersonsStaff);
end;

procedure TMasterDetailTableDemoMainForm.SetGridMasterDetailStyle;
// Display in Grid4 single grid style
var
  AView: TcxGridDBTableView;
  AData: TcxGridDBDataController;
begin
  // remove master/detail link in the data module
  FilmsDemoDM.cdsFilmsPersons.MasterSource := nil;

  // create view in the first grid (Grid)
  AView := TcxGridDBTableView(Grid.CreateView(TcxGridDBTableView));
  AView.Assign(lvDetail.GridView);
  AData := AView.DataController as TcxGridDBDataController;
  AData.KeyFieldNames := 'ID';
  AData.MasterKeyFieldNames := 'ID';
  AData.DetailKeyFieldNames := 'FilmID';
  AData.DataModeController.SmartRefresh := True;

  // hide the second grid (GridDetail)
  lvDetail.GridView.Free;
  pnlDetail.Visible := False;
  Splitter.Visible := False;
  // bind AView to first grid's detail level
  lvFilmsPersonsStaff.Visible := True;
  lvFilmsPersonsStaff.GridView := AView;
  tvFilmsPersonsStaff := AView;

  lblMaster.Visible := False;
  lblStyle.Caption := 'ExpressQuantumGrid master-detail style';
  UpdateTableViewStyleSheet(tvFilmsPersonsStaff);
end;

procedure TMasterDetailTableDemoMainForm.SetStandardMasterDetailStyle;
// Display in standard two grid style
var
  AView: TcxGridDBTableView;
  AData: TcxGridDBDataController;
begin
  // restore master/detail link in the data module
  FilmsDemoDM.cdsFilmsPersons.MasterSource := FilmsDemoDM.dsFilms;
  FilmsDemoDM.cdsFilmsPersons.IndexFieldNames := 'FILMID';
  FilmsDemoDM.cdsFilmsPersons.MasterFields := 'ID';

  // create view in the second grid (GridDetail)
  AView := TcxGridDBTableView(GridDetail.CreateView(TcxGridDBTableView));
  AView.Assign(lvFilmsPersonsStaff.GridView);
  AData := AView.DataController as TcxGridDBDataController;
  AData.KeyFieldNames := 'ID';
  AData.MasterKeyFieldNames := '';
  AData.DetailKeyFieldNames := '';
  AData.DataModeController.SmartRefresh := False;
  tvFilmsPersonsStaff := AView;

  // remove the detail level from the first grid (Grid)
  lvFilmsPersonsStaff.Visible := False;
  lvFilmsPersonsStaff.GridView.Free;

  // bind AView to second grid's level
  lvDetail.GridView := AView;
  pnlDetail.Visible := True;
  Splitter.Visible := True;

  lblMaster.Visible := True;
  lblStyle.Caption := 'Standard master-detail style';
  UpdateTableViewStyleSheet(tvFilmsPersonsStaff);
end;

procedure TMasterDetailTableDemoMainForm.FormCreate(Sender: TObject);
begin
  inherited;
  FilmsDemoDM.FilmsFiltered := False;
  FilmsDemoDM.cdsFilmsPersons.MasterSource := FilmsDemoDM.dsFilms;
  FilmsDemoDM.cdsFilmsPersons.IndexFieldNames := 'FILMID';
  FilmsDemoDM.cdsFilmsPersons.MasterFields := 'ID';
end;

end.
