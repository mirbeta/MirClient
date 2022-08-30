unit StylesMultiDemoMain;

interface

uses
  Windows, Messages, Forms, SysUtils, cxStyles, cxCustomData, cxGraphics, cxFilter,
  cxEdit, DB, cxDBData, Dialogs, cxGridCustomPopupMenu, cxGridPopupMenu,
  Classes, ActnList, ImgList, Controls, Menus, StdCtrls, ExtCtrls,
  cxButtons, cxGridLevel, cxGridCustomTableView, cxGridTableView, cxData,
  cxGridDBTableView, cxClasses, cxControls, cxGridCustomView, cxGrid,
  ComCtrls, cxContainer, cxRadioGroup, cxListBox, cxDataStorage,
  cxLookAndFeelPainters, cxGroupBox, cxLookAndFeels, cxGridStyleSheetsPreview,
  BaseForm, cxGridCardView, cxTextEdit, cxMaskEdit, cxDropDownEdit;

type
  TStylesMultiDemoMainForm = class(TfmBaseForm)
    tvProjects: TcxGridDBTableView;
    lvProjects: TcxGridLevel;
    cxGrid: TcxGrid;
    pnlLeft: TPanel;
    tvProjectsID: TcxGridDBColumn;
    tvProjectsNAME: TcxGridDBColumn;
    tvProjectsMANAGERID: TcxGridDBColumn;
    lvTeam: TcxGridLevel;
    tvTeam: TcxGridDBTableView;
    tvTeamPROJECTID: TcxGridDBColumn;
    tvTeamUSERID: TcxGridDBColumn;
    tvTeamFUNCTION: TcxGridDBColumn;
    Splitter: TSplitter;
    cxGridPopupMenu1: TcxGridPopupMenu;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    gbUserDefined: TcxGroupBox;
    RadioGroup: TcxRadioGroup;
    gbPredefined: TcxGroupBox;
    cbUserStyleSheets: TcxComboBox;
    btnLoad: TcxButton;
    btnSave: TcxButton;
    lbPredefinedStyleSheets: TcxListBox;
    btnEdit: TcxButton;
    pnlCurrentStyleSheet: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
    procedure RadioGroupClick(Sender: TObject);
    procedure cbUserStyleSheetsChange(Sender: TObject);
    procedure lbPredefinedStyleSheetsClick(Sender: TObject);
    procedure btnEditClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    function GetCurrentStyleSheet: TcxGridTableViewStyleSheet;
    procedure CreateUserStyleSheetsList;
    procedure CreatePredefinedStyleSheetsList;
    procedure UpdateGridStyleSheets(const AStyleSheet: TcxGridTableViewStyleSheet);
    procedure ChangeVisibility(AType: Integer);
    procedure SetPredefinedStyleSheets;
    procedure SetUserDefinedStyleSheets;
    procedure ClearUserDefinedStyleSheets;
    procedure LoadUserDefinedStyleSheets(AFileName: TFileName);
    procedure SaveUserDefinedStyleSheets(AFileName: TFileName);
  end;

var
  StylesMultiDemoMainForm: TStylesMultiDemoMainForm;

implementation

{$R *.dfm}

uses
  StylesMultiDemoData, cxStyleSheetEditor, AboutDemoForm;

const
  cNone = 0;
  cPredefined = 1;
  cUserDefined = 2;

procedure TStylesMultiDemoMainForm.FormCreate(Sender: TObject);
begin
  CreateUserStyleSheetsList;
  CreatePredefinedStyleSheetsList;
  SetPredefinedStyleSheets;
end;

procedure TStylesMultiDemoMainForm.CreateUserStyleSheetsList;
var
  I: Integer;
begin
  with StylesMultiDemoMainDM.strepUserDefined do
  begin
    cbUserStyleSheets.Properties.Items.Clear;
    for I := 0 to StyleSheetCount - 1 do
      cbUserStyleSheets.Properties.Items.AddObject(StyleSheets[I].Caption, StyleSheets[I]);
    cbUserStyleSheets.ItemIndex := 0;
  end;
end;

procedure TStylesMultiDemoMainForm.CreatePredefinedStyleSheetsList;
var
  I: Integer;
begin
  with StylesMultiDemoMainDM.strepPredefined do
  begin
    lbPredefinedStyleSheets.Clear;
    for I := 0 to StyleSheetCount - 1 do
      lbPredefinedStyleSheets.Items.AddObject(StyleSheets[I].Caption, StyleSheets[I]);
    lbPredefinedStyleSheets.ItemIndex := 0;
  end;
end;

procedure TStylesMultiDemoMainForm.UpdateGridStyleSheets(const AStyleSheet: TcxGridTableViewStyleSheet);

  procedure UpdateView(const AView: TcxGridDBTableView);
  begin
    with AView do
    begin
      BeginUpdate;
      Styles.StyleSheet := AStyleSheet;
      EndUpdate;
    end;
  end;

begin
  if GetCurrentStyleSheet = AStyleSheet then
    Exit;
  UpdateView(tvProjects);
  UpdateView(tvTeam);
  tvProjects.DataController.ClearDetails;  // refresh detail level

  if AStyleSheet <> nil then
    pnlCurrentStyleSheet.Caption := AStyleSheet.Caption
  else
    pnlCurrentStyleSheet.Caption := '';
end;

procedure TStylesMultiDemoMainForm.btnSaveClick(Sender: TObject);
begin
  with SaveDialog do
    if Execute then
      SaveUserDefinedStyleSheets(FileName);
end;

procedure TStylesMultiDemoMainForm.btnLoadClick(Sender: TObject);
begin
  with OpenDialog do
    if Execute then
      LoadUserDefinedStyleSheets(FileName);
end;

procedure TStylesMultiDemoMainForm.RadioGroupClick(Sender: TObject);
begin
  case TcxRadioGroup(Sender).ItemIndex of
    cNone:
      UpdateGridStyleSheets(nil);
    cPredefined:
      SetPredefinedStyleSheets;
    cUserDefined:
      SetUserDefinedStyleSheets;
  end;
  ChangeVisibility(TcxRadioGroup(Sender).ItemIndex);
end;

procedure TStylesMultiDemoMainForm.SetUserDefinedStyleSheets;
begin
  with cbUserStyleSheets.Properties do
    if Items.Count > 0 then
      UpdateGridStyleSheets(TcxGridTableViewStyleSheet(
        Items.Objects[cbUserStyleSheets.ItemIndex]));
end;

procedure TStylesMultiDemoMainForm.SetPredefinedStyleSheets;
begin
  with lbPredefinedStyleSheets do
    if Items.Count > 0 then
      UpdateGridStyleSheets(TcxGridTableViewStyleSheet(Items.Objects[ItemIndex]));
end;

procedure TStylesMultiDemoMainForm.ChangeVisibility(AType: Integer);
begin
  cbUserStyleSheets.Enabled := AType = cUserDefined;
  gbUserDefined.Enabled := AType = cUserDefined;
  btnEdit.Enabled := AType = cUserDefined;
  btnLoad.Enabled := AType = cUserDefined;
  btnSave.Enabled := AType = cUserDefined;

  lbPredefinedStyleSheets.Enabled := AType = cPredefined;
end;

procedure TStylesMultiDemoMainForm.cbUserStyleSheetsChange(
  Sender: TObject);
begin
  SetUserDefinedStyleSheets;
end;

procedure TStylesMultiDemoMainForm.lbPredefinedStyleSheetsClick(
  Sender: TObject);
begin
  SetPredefinedStyleSheets;
end;

procedure TStylesMultiDemoMainForm.btnEditClick(Sender: TObject);
begin
  with cbUserStyleSheets.Properties do
    ShowcxStyleSheetEditor(TcxGridTableViewStyleSheet(
      Items.Objects[cbUserStyleSheets.ItemIndex]), nil);
end;

function TStylesMultiDemoMainForm.GetCurrentStyleSheet: TcxGridTableViewStyleSheet;
begin
  Result := TcxGridTableViewStyleSheet(tvProjects.Styles.StyleSheet);
end;

procedure TStylesMultiDemoMainForm.LoadUserDefinedStyleSheets(AFileName: TFileName);
begin
  UpdateGridStyleSheets(nil);
  ClearUserDefinedStyleSheets;

  LoadStyleSheetsFromIniFile(AFileName, StylesMultiDemoMainDM.strepUserDefined,
    TcxGridTableViewStyleSheet);

  CreateUserStyleSheetsList;
  SetUserDefinedStyleSheets;
end;

procedure TStylesMultiDemoMainForm.SaveUserDefinedStyleSheets(AFileName: TFileName);
var
  AList: TList;
begin
  AList := TList.Create;
  try
    PopulateStyleSheetsList(AList);
    SaveStyleSheetsToIniFile(AFileName, AList);
  finally
    AList.Free;
  end;
end;

procedure TStylesMultiDemoMainForm.ClearUserDefinedStyleSheets;
begin
  with StylesMultiDemoMainDM.strepUserDefined do
  begin
    Clear;
    ClearStyleSheets;
  end;
end;

procedure TStylesMultiDemoMainForm.FormActivate(Sender: TObject);
begin
  OpenDialog.InitialDir := ExtractFileDir(Application.ExeName);
  SaveDialog.InitialDir := OpenDialog.InitialDir;
end;

end.
