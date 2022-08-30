unit FindPanelDemoMain;

{$I cxVer.inc}

interface

uses
  SysUtils, Classes, Controls, Forms, Menus, DB, StdCtrls, ComCtrls, DBClient,
  dxCore, cxStyles, cxCustomData, cxGraphics, cxFilter, cxData, cxDataStorage,
  cxEdit, cxClasses, cxControls, cxEditRepositoryItems, cxLookAndFeels, cxLookAndFeelPainters,
  cxNavigator, cxContainer, cxGroupBox, ActnList, cxCheckBox, cxLabel,
  cxTextEdit, cxMaskEdit, cxSpinEdit, cxDropDownEdit, cxImageComboBox, cxDBData,
  cxGridDBTableView, XPMan, DemoBasicMain, MidasLib, Dialogs, ImgList, cxTL,
  cxTLdxBarBuiltInMenu, cxDataControllerConditionalFormattingRulesManagerDialog,
  cxInplaceContainer, cxDBTL, cxTLData, dxmdaset;

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
    dsEmployeesGroups: TDataSource;
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
    TreeList: TcxDBTreeList;
    tlDBRecId: TcxDBTreeListColumn;
    tlDBId: TcxDBTreeListColumn;
    tlDBParentId: TcxDBTreeListColumn;
    tlDBJobTitle: TcxDBTreeListColumn;
    tlDBFirstName: TcxDBTreeListColumn;
    tlDBLastName: TcxDBTreeListColumn;
    tlDBCity: TcxDBTreeListColumn;
    tlDBStateProvinceName: TcxDBTreeListColumn;
    tlDBPhone: TcxDBTreeListColumn;
    tlDBEmailAddress: TcxDBTreeListColumn;
    tlDBAddressLine1: TcxDBTreeListColumn;
    tlDBPostalCode: TcxDBTreeListColumn;
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
  cxFindPanel, Variants, AboutDemoForm;

{$R *.dfm}

procedure TfrmMain.icbFindFilterColumnsPropertiesChange(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to TreeList.ColumnCount - 1 do
    if (icbFindFilterColumns.EditValue = 'All') or
      (Pos(TreeList.Columns[I].Name, icbFindFilterColumns.EditValue) > 0) then
      TreeList.Columns[I].Options.FilteringWithFindPanel := True
    else
      TreeList.Columns[I].Options.FilteringWithFindPanel := False;
end;

procedure TfrmMain.UpdateFindFilterColumns;
var
  I, J: Integer;
  AFindFilterColumnsDescription: string;
  AFindFilterColumnsValue: string;
  AImageComboBoxItem: TcxImageComboBoxItem;
  AColumn: TcxTreeListColumn;
begin
  icbFindFilterColumns.Properties.Items.Clear;
  AImageComboBoxItem := icbFindFilterColumns.Properties.Items.Add;
  AImageComboBoxItem.Description := 'All';
  AImageComboBoxItem.Value := 'All';
  for I := 0 to TreeList.ColumnCount - 1 do
  begin
    AColumn := TreeList.Columns[I];
    if not AColumn.Visible then
      Continue;
    AFindFilterColumnsDescription := AColumn.Caption.Text;
    AFindFilterColumnsValue := AColumn.Name;
    for J := I to TreeList.ColumnCount - 1 do
    begin
      AColumn := TreeList.Columns[J];
      if not AColumn.Visible then
        Continue;
      if J <> I then
      begin
        AFindFilterColumnsDescription := AFindFilterColumnsDescription + '; ' + AColumn.Caption.Text;
        AFindFilterColumnsValue := AFindFilterColumnsValue + ';' + AColumn.Name;
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
  TreeList.FindPanel.ApplyInputDelay := seFindDelay.Value;
end;

procedure TfrmMain.actClearFindOnCloseChange(Sender: TObject);
begin
  TreeList.FindPanel.ClearFindFilterTextOnClose := actClearFindOnClose.Checked;
end;

procedure TfrmMain.actHighlightFindResultChange(Sender: TObject);
begin
  TreeList.FindPanel.HighlightSearchResults := actHighlightSearchResults.Checked;
end;

procedure TfrmMain.actShowClearButtonChange(Sender: TObject);
begin
  TreeList.FindPanel.ShowClearButton := actShowClearButton.Checked;
end;

procedure TfrmMain.actShowCloseButtonChange(Sender: TObject);
begin
  TreeList.FindPanel.ShowCloseButton := actShowCloseButton.Checked;
end;

procedure TfrmMain.actShowFindButtonEChange(Sender: TObject);
begin
  TreeList.FindPanel.ShowFindButton := actShowFindButton.Checked;
end;

procedure TfrmMain.actUseDelayedSearchExecute(Sender: TObject);
begin
  TreeList.FindPanel.UseDelayedFind := actUseDelayedSearch.Checked;
end;

procedure TfrmMain.actUseExtendedSyntaxExecute(Sender: TObject);
begin
  TreeList.FindPanel.UseExtendedSyntax := actUseExtendedSyntax.Checked;
end;

procedure TfrmMain.cbDisplayModePropertiesChange(Sender: TObject);
begin
  if cbeDisplayMode.ItemIndex = 0 then
    TreeList.FindPanel.DisplayMode := fpdmNever
  else
    if cbeDisplayMode.ItemIndex = 1 then
      TreeList.FindPanel.DisplayMode := fpdmManual
    else
      TreeList.FindPanel.DisplayMode := fpdmAlways;
  actShowCloseButton.Enabled := not (TreeList.FindPanel.DisplayMode = fpdmAlways);
end;

procedure TfrmMain.cbFindPanelPositionPropertiesChange(Sender: TObject);
begin
  if cbeFindPanelPosition.Text = 'Top' then
    TreeList.FindPanel.Position := fppTop
  else
    TreeList.FindPanel.Position := fppBottom;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
var
  APath: string;
begin
  APath := ExtractFilePath(Application.ExeName) + '..\..\Data\';
  mdEmployeesGroups.LoadFromBinaryFile(APath + 'EmployeesGroups.dat');
  mdEmployeesGroups.Open;
  TreeList.ApplyFindFilterText('cali +manager');
  TreeList.ShowFindPanel;
  TreeList.FullExpand;
  UpdateFindFilterColumns;
end;

end.
