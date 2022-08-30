unit BreadcrumbEditDemoMain;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Forms, ComCtrls, ShlObj, cxShellCommon, cxGraphics,
  cxControls, cxLookAndFeels, cxLookAndFeelPainters, Menus, cxClasses, cxContainer,
  cxEdit, ImgList, Controls, Classes, ActnList, dxBreadcrumbEdit, dxShellBreadcrumbEdit,
  Buttons, cxShellTreeView, cxSplitter, cxShellControls, cxShellListView,
  StdCtrls, ExtCtrls, dxGDIPlusClasses, ShellAPI, BaseForm, cxTreeView,
  cxGroupBox, cxLabel, cxTextEdit, cxMaskEdit, cxDropDownEdit, cxButtons,
  cxCheckBox, cxImageList;

type
  TdxBreadcrumbEditDemoForm = class(TfmBaseForm)
    beNavigation: TdxBreadcrumbEdit;
    btnProgressStart: TcxButton;
    btnProgressStop: TcxButton;
    cbCancelEffect: TcxComboBox;
    gbProgressBarOptions: TcxGroupBox;
    cxImageList1: TcxImageList;
    lbCancelEffect: TcxLabel;
    tmProgress: TTimer;
    tvTree: TcxTreeView;
    gbPathEditorOptions: TcxGroupBox;
    cbpeAutoComplete: TcxCheckBox;
    cbpeEnabled: TcxCheckBox;
    cbpeReadOnly: TcxCheckBox;
    gbpeRecents: TcxGroupBox;
    cbpeRecentsAutoPopulate: TcxCheckBox;
    btnEditRecentPath: TcxButton;
    gbMain: TcxGroupBox;
    bvSpacerLeft: TBevel;
    bvSpacerBottom: TBevel;
    bvSpacerTop: TBevel;
    bvSpacerRight: TBevel;
    Bevel1: TBevel;
    procedure beNavigationPathSelected(Sender: TObject);
    procedure beNavigationPopulateChildren(Sender: TObject; ANode: TdxBreadcrumbEditNode);
    procedure btnProgressStartClick(Sender: TObject);
    procedure btnProgressStopClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PathEditorOptionsChanged(Sender: TObject);
    procedure ProgressBarPropertiesChange(Sender: TObject);
    procedure tmProgressTimer(Sender: TObject);
    procedure tvTreeChange(Sender: TObject; Node: TTreeNode);
    procedure btnEditRecentPathClick(Sender: TObject);
  private
    FInitializingControls: Boolean;
    FWasSynchronization: Boolean;
    function GetPath(ATreeNode: TTreeNode): string;
    procedure EnableControls(AContainer: TWinControl; ASender: TcxCheckBox);
    procedure InitializeControls;
    procedure SynchronizeNodes(ANode: TdxBreadcrumbEditNode; ATreeNode: TTreeNode);
  end;

var
  dxBreadcrumbEditDemoForm: TdxBreadcrumbEditDemoForm;

implementation

uses
  BreadcrumbEditDemoRecentPaths;

{$R *.dfm}

{ TdxBreadcrumbEditDemoForm }

procedure TdxBreadcrumbEditDemoForm.EnableControls(AContainer: TWinControl; ASender: TcxCheckBox);
var
  AControl: TControl;
  I: Integer;
begin
  for I := 0 to AContainer.ControlCount - 1 do
  begin
    AControl := AContainer.Controls[I];
    if AControl <> ASender then
      AControl.Enabled := ASender.Checked;
    if AControl is TWinControl then
      EnableControls(TWinControl(AControl), ASender);
  end;
end;

procedure TdxBreadcrumbEditDemoForm.beNavigationPathSelected(Sender: TObject);
begin
  tvTree.Selected := TTreeNode(beNavigation.Selected.Data);
end;

procedure TdxBreadcrumbEditDemoForm.beNavigationPopulateChildren(
  Sender: TObject; ANode: TdxBreadcrumbEditNode);
var
  ATreeNode: TTreeNode;
  I: Integer;
begin
  ATreeNode := TTreeNode(ANode.Data);
  for I := 0 to ATreeNode.Count - 1 do
    SynchronizeNodes(ANode.AddChild, ATreeNode[I]);
end;

procedure TdxBreadcrumbEditDemoForm.btnEditRecentPathClick(Sender: TObject);
begin
  with TdxBreadcrumbEditDemoRecentPathsForm.Create(nil) do
  try
    LoadPaths(beNavigation.Properties.PathEditor.RecentPaths);
    if ShowModal = mrOk then
      SavePaths(beNavigation.Properties.PathEditor.RecentPaths);
  finally
    Free;
  end;
end;

procedure TdxBreadcrumbEditDemoForm.btnProgressStartClick(Sender: TObject);
begin
  btnProgressStop.Enabled := True;
  btnProgressStart.Enabled := False;
  tmProgress.Enabled := True;
end;

procedure TdxBreadcrumbEditDemoForm.btnProgressStopClick(Sender: TObject);
begin
  tmProgress.Enabled := False;
  btnProgressStop.Enabled := False;
  btnProgressStart.Enabled := True;
  beNavigation.Properties.ProgressBar.Position := 0;
end;

procedure TdxBreadcrumbEditDemoForm.PathEditorOptionsChanged(Sender: TObject);
begin
  EnableControls(gbPathEditorOptions, cbpeEnabled);
  if not FInitializingControls then
  begin
    beNavigation.Properties.PathEditor.AutoComplete := cbpeAutoComplete.Checked;
    beNavigation.Properties.PathEditor.Enabled := cbpeEnabled.Checked;
    beNavigation.Properties.PathEditor.ReadOnly := cbpeReadOnly.Checked;
    beNavigation.Properties.PathEditor.RecentPathsAutoPopulate := cbpeRecentsAutoPopulate.Checked;
  end;
end;

procedure TdxBreadcrumbEditDemoForm.ProgressBarPropertiesChange(Sender: TObject);
begin
  beNavigation.Properties.ProgressBar.CancelEffect :=
    TdxBreadcrumbEditProgressBarCancelEffect(cbCancelEffect.ItemIndex);
end;

procedure TdxBreadcrumbEditDemoForm.InitializeControls;
begin
  FInitializingControls := True;
  try
    cbCancelEffect.ItemIndex := Integer(beNavigation.Properties.ProgressBar.CancelEffect);
    cbpeAutoComplete.Checked := beNavigation.Properties.PathEditor.AutoComplete;
    cbpeEnabled.Checked := beNavigation.Properties.PathEditor.Enabled;
    cbpeReadOnly.Checked := beNavigation.Properties.PathEditor.ReadOnly;
    cbpeRecentsAutoPopulate.Checked := beNavigation.Properties.PathEditor.RecentPathsAutoPopulate;
  finally
    FInitializingControls := False;
  end;
end;

procedure TdxBreadcrumbEditDemoForm.FormCreate(Sender: TObject);
begin
  InitializeControls;
  tvTree.FullExpand;
  SynchronizeNodes(beNavigation.Root, tvTree.Items.Item[0]);
end;

procedure TdxBreadcrumbEditDemoForm.SynchronizeNodes(
  ANode: TdxBreadcrumbEditNode; ATreeNode: TTreeNode);
begin
  ANode.BeginUpdate;
  try
    ANode.ImageIndex := ATreeNode.ImageIndex;
    ANode.HasChildren := ATreeNode.HasChildren;
    ANode.Name := ATreeNode.Text;
    ANode.Data := ATreeNode;
  finally
    ANode.EndUpdate;
  end;
  FWasSynchronization := True;
end;

function TdxBreadcrumbEditDemoForm.GetPath(ATreeNode: TTreeNode): string;
begin
  if ATreeNode.Parent <> nil then
    Result := GetPath(ATreeNode.Parent)
  else
    Result := '';

  Result := Result + ATreeNode.Text + PathDelim;
end;

procedure TdxBreadcrumbEditDemoForm.tmProgressTimer(Sender: TObject);
begin
  beNavigation.Properties.ProgressBar.Position :=
    beNavigation.Properties.ProgressBar.Position + 1;
end;

procedure TdxBreadcrumbEditDemoForm.tvTreeChange(Sender: TObject; Node: TTreeNode);
begin
  if FWasSynchronization then
    beNavigation.SelectedPath := GetPath(Node);
end;

end.
