unit MasterDetailDemoMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, cxGridCustomTableView, cxGridTableView, cxGridDBTableView, DB,
  cxGridLevel, cxControls, cxGridCommon, cxGridCustomView, cxGrid,
  cxGridCardView, cxGridDBCardView, StdCtrls, ExtCtrls,
  cxDBData, ComCtrls, ToolWin, Menus, cxStyles, ImgList, DBCtrls,
  cxDropDownEdit, cxLookupEdit, cxDBLookupComboBox, cxMaskEdit, cxCalendar,
  cxDBEdit, cxHyperLinkEdit, cxContainer, cxEdit, cxTextEdit, cxMemo,
  ActnList, cxCheckBox, cxCustomData, cxGraphics, cxFilter, cxData,
  cxClasses, cxDataStorage, cxBlobEdit, cxImage, cxLookAndFeels,
  cxLookAndFeelPainters, cxGridCustomLayoutView, BaseForm;

type
  TMasterDetailDemoMainForm = class(TfmBaseForm)
    miOptions: TMenuItem;
    miSeparator1: TMenuItem;
    miTabsPosition: TMenuItem;
    miTopTabsPosition: TMenuItem;
    miLeftTabsPosition: TMenuItem;
    miShowPreviewData: TMenuItem;
    tvFilms: TcxGridDBTableView;
    lvFilms: TcxGridLevel;
    Grid: TcxGrid;
    lvPeople: TcxGridLevel;
    lvCompanies: TcxGridLevel;
    lvPhotos: TcxGridLevel;
    cvPeople: TcxGridDBCardView;
    tvCompanies: TcxGridDBTableView;
    cvPhotos: TcxGridDBCardView;
    tvFilmsID: TcxGridDBColumn;
    tvFilmsCAPTION: TcxGridDBColumn;
    tvFilmsYEAR: TcxGridDBColumn;
    tvFilmsTAGLINE: TcxGridDBColumn;
    tvFilmsPLOTOUTLINE: TcxGridDBColumn;
    tvFilmsRUNTIME: TcxGridDBColumn;
    tvFilmsCOLOR: TcxGridDBColumn;
    tvFilmsPHOTO: TcxGridDBColumn;
    tvFilmsICON: TcxGridDBColumn;
    tvFilmsWEBSITE: TcxGridDBColumn;
    cvPeopleName: TcxGridDBCardViewRow;
    cvPeoplePersonLineID: TcxGridDBCardViewRow;
    cvPeopleFIRSTNAME: TcxGridDBCardViewRow;
    cvPeopleSECONDNAME: TcxGridDBCardViewRow;
    cvPeopleNICKNAME: TcxGridDBCardViewRow;
    cvPeopleBIRTHNAME: TcxGridDBCardViewRow;
    cvPeopleDATEOFBIRTH: TcxGridDBCardViewRow;
    cvPeopleLOCATIONOFBIRTH: TcxGridDBCardViewRow;
    cvPeopleBIOGRAPHY: TcxGridDBCardViewRow;
    cvPeopleHOMEPAGE: TcxGridDBCardViewRow;
    cvPeopleID: TcxGridDBCardViewRow;
    cvPeopleFilmID: TcxGridDBCardViewRow;
    cvPeopleBIRTHCOUNTRY: TcxGridDBCardViewRow;
    cvPeopleGender: TcxGridDBCardViewRow;
    tvCompaniesName: TcxGridDBColumn;
    tvCompaniesType: TcxGridDBColumn;
    tvCompaniesCountry: TcxGridDBColumn;
    tvCompaniesWebSite: TcxGridDBColumn;
    tvCompaniesID: TcxGridDBColumn;
    tvCompaniesFILMID: TcxGridDBColumn;
    cvPhotosID: TcxGridDBCardViewRow;
    cvPhotosFILMID: TcxGridDBCardViewRow;
    cvPhotosSCREEN: TcxGridDBCardViewRow;
    cvPhotosICON: TcxGridDBCardViewRow;
    miDetailViewsSynchronization: TMenuItem;
    procedure FormShow(Sender: TObject);
    procedure miTabsPositionClick(Sender: TObject);
    procedure miShowPreviewDataClick(Sender: TObject);
    procedure miDetailViewsSynchronizationClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  protected
    procedure LookAndFeelChanged(Sender: TcxLookAndFeel;
      AChangedValues: TcxLookAndFeelValues); override;
  end;

var
  MasterDetailDemoMainForm: TMasterDetailDemoMainForm;

implementation

{$R *.dfm}

uses
  FilmsDemoData, AboutDemoForm;

procedure TMasterDetailDemoMainForm.FormShow(Sender: TObject);
begin
  if FilmsDemoDM.cdsFilms.Active then
  begin
    FilmsDemoDM.cdsFilms.First;
    if Assigned(tvFilms.Controller.FocusedRecord) then
      tvFilms.Controller.FocusedRecord.Expanded := true;
  end;
end;

procedure TMasterDetailDemoMainForm.miTabsPositionClick(Sender: TObject);
begin
  lvFilms.Options.DetailTabsPosition :=
    TcxGridDetailTabsPosition(TComponent(Sender).Tag + 1);
end;

procedure TMasterDetailDemoMainForm.miShowPreviewDataClick(Sender: TObject);
begin
  if not Assigned(tvFilms.Preview.Column) then
    tvFilms.Preview.Column := tvFilmsPLOTOUTLINE;
  tvFilms.Preview.Visible := GetMenuItemChecked(Sender);
end;

procedure TMasterDetailDemoMainForm.miDetailViewsSynchronizationClick(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to Grid.ViewCount - 1 do
    Grid.Views[I].Synchronization := GetMenuItemChecked(Sender);
end;

procedure TMasterDetailDemoMainForm.LookAndFeelChanged(
  Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues);
begin
  inherited LookAndFeelChanged(Sender, AChangedValues);
  UpdateCardViewStyleSheet(cvPeople);
  UpdateCardViewStyleSheet(cvPhotos);
  UpdateTableViewStyleSheet(tvFilms);
  UpdateTableViewStyleSheet(tvCompanies);
end;

procedure TMasterDetailDemoMainForm.FormCreate(Sender: TObject);
begin
  inherited;
  FilmsDemoDM.FilmsFiltered := False;
end;

end.
