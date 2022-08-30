unit FindPanelDemoMain;

{$I cxVer.inc}

interface

uses
  SysUtils, Classes, Forms, Controls, Menus, DB, StdCtrls, ComCtrls, DBClient,
  dxCore, cxStyles, cxCustomData, cxGraphics, cxFilter, cxData, cxDataStorage,
  cxEdit, cxClasses, cxControls,
  cxEditRepositoryItems, cxLookAndFeels, cxLookAndFeelPainters,
  cxNavigator, cxContainer, cxGroupBox, ActnList, cxCheckBox, cxLabel,
  cxTextEdit, cxMaskEdit, cxSpinEdit, cxDropDownEdit, cxImageComboBox, cxDBData,
  XPMan, DemoBasicMain, MidasLib,
  cxDataControllerConditionalFormattingRulesManagerDialog, cxGridLevel,
  cxGridCustomTableView, cxGridTableView, cxGridDBTableView, cxGridCustomView,
  cxGrid, ImgList, cxVGrid, cxDBVGrid, cxInplaceContainer, dxmdaset;

type
  TfrmMain = class(TDemoBasicMainForm)
    erMain: TcxEditRepository;
    erMainFlag: TcxEditRepositoryImageItem;
    cxGroupBox1: TcxGroupBox;
    cbClearFindOnClose: TcxCheckBox;
    cbShowClearButton: TcxCheckBox;
    cbShowCloseButton: TcxCheckBox;
    cbShowFindButton: TcxCheckBox;
    cbHighlightSearchResults: TcxCheckBox;
    alAction: TActionList;
    actClearFindOnClose: TAction;
    actShowClearButton: TAction;
    actShowCloseButton: TAction;
    actShowFindButton: TAction;
    actHighlightSearchResults: TAction;
    miFindPanelOptions: TMenuItem;
    ClearFindOnClose1: TMenuItem;
    HighlightFindResult1: TMenuItem;
    miVisibleButtons: TMenuItem;
    ShowClearButton2: TMenuItem;
    ShowCloseButton2: TMenuItem;
    ShowFindButton2: TMenuItem;
    seFindDelay: TcxSpinEdit;
    lbSearchDelay: TcxLabel;
    icbFindFilterColumns: TcxImageComboBox;
    lbSearchableColumns: TcxLabel;
    cbeFindPanelPosition: TcxComboBox;
    lbFindPanelPosition: TcxLabel;
    cbeDisplayMode: TcxComboBox;
    lbDisplayMode: TcxLabel;
    cbUseDelayedSearch: TcxCheckBox;
    actUseDelayedSearch: TAction;
    UseDelayedFind1: TMenuItem;
    actUseExtendedSyntax: TAction;
    cbUseExtendedSyntax: TcxCheckBox;
    UseExtendedSyntax1: TMenuItem;
    mdEmployeesGroups: TdxMemData;
    mdEmployeesGroupsId: TStringField;
    mdEmployeesGroupsParentId: TStringField;
    mdEmployeesGroupsJobTitle: TStringField;
    mdEmployeesGroupsFirstName: TStringField;
    mdEmployeesGroupsLastName: TStringField;
    mdEmployeesGroupsCity: TStringField;
    mdEmployeesGroupsStateProvinceName: TStringField;
    mdEmployeesGroupsPhone: TStringField;
    mdEmployeesGroupsEmailAddress: TStringField;
    mdEmployeesGroupsAddressLine1: TStringField;
    mdEmployeesGroupsPostalCode: TStringField;
    dsEmployeesGroups: TDataSource;
    VerticalGrid: TcxDBVerticalGrid;
    VerticalGridRecId: TcxDBEditorRow;
    VerticalGridId: TcxDBEditorRow;
    VerticalGridParentId: TcxDBEditorRow;
    VerticalGridJobTitle: TcxDBEditorRow;
    VerticalGridFirstName: TcxDBEditorRow;
    VerticalGridLastName: TcxDBEditorRow;
    VerticalGridCity: TcxDBEditorRow;
    VerticalGridStateProvinceName: TcxDBEditorRow;
    VerticalGridPhone: TcxDBEditorRow;
    VerticalGridEmailAddress: TcxDBEditorRow;
    VerticalGridAddressLine1: TcxDBEditorRow;
    VerticalGridPostalCode: TcxDBEditorRow;
    procedure FormCreate(Sender: TObject);
    procedure actClearFindOnCloseChange(Sender: TObject);
    procedure actShowClearButtonChange(Sender: TObject);
    procedure actShowCloseButtonChange(Sender: TObject);
    procedure actShowFindButtonEChange(Sender: TObject);
    procedure actHighlightFindResultChange(Sender: TObject);
    procedure seFindDelayPropertiesChange(Sender: TObject);
    procedure icbFindFilterColumnsPropertiesChange(Sender: TObject);
    procedure cbFindPanelPositionPropertiesChange(Sender: TObject);
    procedure cbDisplayModePropertiesChange(Sender: TObject);
    procedure actUseDelayedSearchExecute(Sender: TObject);
    procedure actUseExtendedSyntaxExecute(Sender: TObject);
  protected
    procedure UpdateFindFilterColumns;
  end;

var
  frmMain: TfrmMain;

implementation

uses
  cxFindPanel, Variants;

{$R *.dfm}

procedure TfrmMain.icbFindFilterColumnsPropertiesChange(Sender: TObject);
var
  I: Integer;
  ARow: TcxDBEditorRow;
begin
  for I := 0 to VerticalGrid.Rows.Count - 1 do
  begin
    ARow := TcxDBEditorRow(VerticalGrid.Rows[I]);
    if (icbFindFilterColumns.EditValue = 'All') or
      (Pos(ARow.Name, icbFindFilterColumns.EditValue) > 0) then
      ARow.Properties.Options.FilteringWithFindPanel := True
    else
      ARow.Properties.Options.FilteringWithFindPanel := False;
  end;
end;

procedure TfrmMain.UpdateFindFilterColumns;
var
  I, J: Integer;
  AFindFilterColumnsDescription: string;
  AFindFilterColumnsValue: string;
  AImageComboBoxItem: TcxImageComboBoxItem;
  ARow: TcxDBEditorRow;
begin
  icbFindFilterColumns.Properties.Items.Clear;
  AImageComboBoxItem := icbFindFilterColumns.Properties.Items.Add;
  AImageComboBoxItem.Description := 'All';
  AImageComboBoxItem.Value := 'All';
  for I := 0 to VerticalGrid.Rows.Count - 1 do
  begin
    ARow := TcxDBEditorRow(VerticalGrid.Rows[I]);
    if not ARow.Visible then
      Continue;
    AFindFilterColumnsDescription := ARow.Properties.Caption;
    AFindFilterColumnsValue := ARow.Name;
    for J := I to VerticalGrid.Rows.Count - 1 do
    begin
      ARow := TcxDBEditorRow(VerticalGrid.Rows[J]);
      if not ARow.Visible then
        Continue;
      if J <> I then
      begin
        AFindFilterColumnsDescription := AFindFilterColumnsDescription + '; ' + ARow.Properties.Caption;
        AFindFilterColumnsValue := AFindFilterColumnsValue + ';' + ARow.Name;
      end;
      AImageComboBoxItem := icbFindFilterColumns.Properties.Items.Add;
      AImageComboBoxItem.Description := AFindFilterColumnsDescription;
      AImageComboBoxItem.Value := AFindFilterColumnsValue;
    end;
  end;
  icbFindFilterColumns.ItemIndex := 0;
end;

procedure TfrmMain.seFindDelayPropertiesChange(Sender: TObject);
begin
  VerticalGrid.FindPanel.ApplyInputDelay := seFindDelay.Value;
end;

procedure TfrmMain.actClearFindOnCloseChange(Sender: TObject);
begin
  VerticalGrid.FindPanel.ClearFindFilterTextOnClose := actClearFindOnClose.Checked;
end;

procedure TfrmMain.actHighlightFindResultChange(Sender: TObject);
begin
  VerticalGrid.FindPanel.HighlightSearchResults := actHighlightSearchResults.Checked;
end;

procedure TfrmMain.actShowClearButtonChange(Sender: TObject);
begin
  VerticalGrid.FindPanel.ShowClearButton := actShowClearButton.Checked;
end;

procedure TfrmMain.actShowCloseButtonChange(Sender: TObject);
begin
  VerticalGrid.FindPanel.ShowCloseButton := actShowCloseButton.Checked;
end;

procedure TfrmMain.actShowFindButtonEChange(Sender: TObject);
begin
  VerticalGrid.FindPanel.ShowFindButton := actShowFindButton.Checked;
end;

procedure TfrmMain.actUseDelayedSearchExecute(Sender: TObject);
begin
  VerticalGrid.FindPanel.UseDelayedFind := actUseDelayedSearch.Checked;
end;

procedure TfrmMain.actUseExtendedSyntaxExecute(Sender: TObject);
begin
  VerticalGrid.FindPanel.UseExtendedSyntax := actUseExtendedSyntax.Checked;
end;

procedure TfrmMain.cbDisplayModePropertiesChange(Sender: TObject);
begin
  if cbeDisplayMode.ItemIndex = 0 then
    VerticalGrid.FindPanel.DisplayMode := fpdmNever
  else
    if cbeDisplayMode.ItemIndex = 1 then
      VerticalGrid.FindPanel.DisplayMode := fpdmManual
    else
      VerticalGrid.FindPanel.DisplayMode := fpdmAlways;
  actShowCloseButton.Enabled := not (VerticalGrid.FindPanel.DisplayMode = fpdmAlways);
end;

procedure TfrmMain.cbFindPanelPositionPropertiesChange(Sender: TObject);
begin
  if cbeFindPanelPosition.Text = 'Top' then
    VerticalGrid.FindPanel.Position := fppTop
  else
    VerticalGrid.FindPanel.Position := fppBottom;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
var
  APath: string;
begin
  APath := ExtractFilePath(Application.ExeName) + '..\..\Data\';
  mdEmployeesGroups.LoadFromBinaryFile(APath + 'EmployeesGroups.dat');
  mdEmployeesGroups.Open;

  VerticalGrid.ApplyFindFilterText('cali +manager');
  VerticalGrid.ShowFindPanel;
  UpdateFindFilterColumns;
end;

end.
