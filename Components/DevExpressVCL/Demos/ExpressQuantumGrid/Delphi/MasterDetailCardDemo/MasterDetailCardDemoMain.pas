unit MasterDetailCardDemoMain;

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
  cxClasses, cxNavigator, cxDBLookupEdit, cxDBNavigator, cxDataStorage,
  cxBlobEdit, cxLookAndFeels, cxLookAndFeelPainters, cxGridCustomLayoutView,
  BaseForm, cxLabel;

type
  TMasterDetailCardDemoMainForm = class(TfmBaseForm)
    Grid: TcxGrid;
    lvFilms: TcxGridLevel;
    pnlDetail: TPanel;
    tvFilms: TcxGridDBTableView;
    colFilmsCaption: TcxGridDBColumn;
    colFilmsYear: TcxGridDBColumn;
    colFilmsTagline: TcxGridDBColumn;
    colFilmsPlotOutline: TcxGridDBColumn;
    colFilmsRuntime: TcxGridDBColumn;
    colFilmsPhoto: TcxGridDBColumn;
    lblMaster: TcxLabel;
    lblDetail: TcxLabel;
    lblStyle: TcxLabel;
    Bevel1: TBevel;
    cvFilmsPersons: TcxGridDBCardView;
    cvFilmsPersonsPersonLineID: TcxGridDBCardViewRow;
    cvFilmsPersonsBIOGRAPHY: TcxGridDBCardViewRow;
    cvFilmsPersonsBIRTHNAME: TcxGridDBCardViewRow;
    cvFilmsPersonsDATEOFBIRTH: TcxGridDBCardViewRow;
    cvFilmsPersonsFIRSTNAME: TcxGridDBCardViewRow;
    cvFilmsPersonsLOCATIONOFBIRTH: TcxGridDBCardViewRow;
    cvFilmsPersonsNICKNAME: TcxGridDBCardViewRow;
    cvFilmsPersonsSECONDNAME: TcxGridDBCardViewRow;
    cvFilmsPersonsHOMEPAGE: TcxGridDBCardViewRow;
    miOptions: TMenuItem;
    miGrid: TMenuItem;
    cvFilmsPersonsName: TcxGridDBCardViewRow;
    Panel1: TPanel;
    Label1: TcxLabel;
    cbOccupation: TcxDBLookupComboBox;
    Label2: TcxLabel;
    edFirstName: TcxDBTextEdit;
    Label8: TcxLabel;
    edSecondName: TcxDBTextEdit;
    Label9: TcxLabel;
    edNickName: TcxDBTextEdit;
    Label7: TcxLabel;
    edHomePage: TcxDBHyperLinkEdit;
    DBNavigator1: TcxDBNavigator;
    edBirthName: TcxDBTextEdit;
    Label6: TcxLabel;
    edLocationOfBirth: TcxDBTextEdit;
    Label4: TcxLabel;
    deDateOfBirth: TcxDBDateEdit;
    Label3: TcxLabel;
    meBiography: TcxDBMemo;
    Label5: TcxLabel;
    lvFilmsPersons: TcxGridLevel;
    chbMale: TcxDBCheckBox;
    cvFilmsPersonsGender: TcxGridDBCardViewRow;
    cxEditStyleController1: TcxEditStyleController;
    procedure miGridClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure SetStandardMasterDetailStyle;
    procedure SetGridMasterDetailStyle;
  protected
    procedure LookAndFeelChanged(Sender: TcxLookAndFeel;
      AChangedValues: TcxLookAndFeelValues); override;
  end;

var
  MasterDetailCardDemoMainForm: TMasterDetailCardDemoMainForm;

implementation

{$R *.dfm}

uses
  cxGridDBDataDefinitions, FilmsDemoData, AboutDemoForm;

procedure TMasterDetailCardDemoMainForm.miGridClick(Sender: TObject);
begin
  tvFilms.Navigator.Visible := GetMenuItemChecked(Sender);
  if GetMenuItemChecked(Sender) then
    SetGridMasterDetailStyle
  else
    SetStandardMasterDetailStyle;
end;

procedure TMasterDetailCardDemoMainForm.LookAndFeelChanged(
  Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues);
begin
  inherited LookAndFeelChanged(Sender, AChangedValues);
  UpdateTableViewStyleSheet(tvFilms);
  UpdateCardViewStyleSheet(cvFilmsPersons);
  if CanUseStyleSheet then
    cxEditStyleController1.Style.Color := $00F7EAD9
  else
    cxEditStyleController1.Style.AssignedValues := [];
end;

procedure TMasterDetailCardDemoMainForm.SetGridMasterDetailStyle;
var
  AData: TcxGridDBDataController;
begin
  pnlDetail.Visible := False;
  AData := cvFilmsPersons.DataController;
  AData.DataModeController.SmartRefresh := True;
  lvFilmsPersons.Visible := True;
  FilmsDemoDM.cdsFilmsPersons.MasterSource := nil;
  lblMaster.Visible := False;
  lblStyle.Caption := 'ExpressQuantumGrid master-detail style';
end;

procedure TMasterDetailCardDemoMainForm.SetStandardMasterDetailStyle;
var
  AData: TcxGridDBDataController;
begin
  lvFilmsPersons.Visible := False;
  AData := cvFilmsPersons.DataController;
  AData.DataModeController.SmartRefresh := False;
  FilmsDemoDM.cdsFilmsPersons.MasterSource := FilmsDemoDM.dsFilms;
  pnlDetail.Visible := True;
  lblMaster.Visible := True;
  lblStyle.Caption := 'Standard master-detail style';
end;

procedure TMasterDetailCardDemoMainForm.FormCreate(Sender: TObject);
begin
  inherited;
  FilmsDemoDM.FilmsFiltered := False;
  FilmsDemoDM.cdsFilmsPersons.MasterSource := FilmsDemoDM.dsFilms;
  FilmsDemoDM.cdsFilmsPersons.IndexFieldNames := 'FILMID';
  FilmsDemoDM.cdsFilmsPersons.MasterFields := 'ID';
end;

end.
