unit BreadcrumbEditDemoRecentPaths;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters,
  cxContainer, cxEdit, cxGroupBox, cxListBox, ComCtrls, cxListView, dxBevel,
  Menus, StdCtrls, cxButtons, cxTextEdit, cxMaskEdit, cxDropDownEdit,
  cxImageComboBox, BreadcrumbEditDemoMain, cxLabel, dxBreadCrumbEdit;

type
  TdxBreadcrumbEditDemoRecentPathsForm = class(TForm)
    btnAdd: TcxButton;
    btnCancel: TcxButton;
    btnDelete: TcxButton;
    btnOk: TcxButton;
    btnReplace: TcxButton;
    bvSeparator: TdxBevel;
    cbImage: TcxImageComboBox;
    lbRecentPath: TcxLabel;
    lvPaths: TcxListView;
    tePath: TcxTextEdit;
    procedure btnAddClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnReplaceClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lvPathsSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure tePathPropertiesChange(Sender: TObject);
  private
    procedure InitializeImageComboBox;
    procedure UpdateControlsState;
  public
    procedure LoadPaths(APaths: TdxBreadcrumbEditRecentPaths);
    procedure SavePaths(APaths: TdxBreadcrumbEditRecentPaths);
  end;

implementation

{$R *.dfm}

{ TdxBreadcrumbEditDemoRecentPathsForm }

procedure TdxBreadcrumbEditDemoRecentPathsForm.btnAddClick(Sender: TObject);
var
  AItem: TListItem;
begin
  AItem := lvPaths.Items.Add;
  AItem.Caption := tePath.Text;
  AItem.ImageIndex := cbImage.ItemIndex;
  lvPaths.Selected := AItem;
end;

procedure TdxBreadcrumbEditDemoRecentPathsForm.btnDeleteClick(Sender: TObject);
begin
  lvPaths.DeleteSelected;
end;

procedure TdxBreadcrumbEditDemoRecentPathsForm.btnReplaceClick(Sender: TObject);
begin
  lvPaths.Selected.ImageIndex := cbImage.ItemIndex;
  lvPaths.Selected.Caption := tePath.Text;
end;

procedure TdxBreadcrumbEditDemoRecentPathsForm.FormCreate(Sender: TObject);
begin
  InitializeImageComboBox;
  UpdateControlsState;
end;

procedure TdxBreadcrumbEditDemoRecentPathsForm.InitializeImageComboBox;
var
  AItem: TcxImageComboBoxItem;
  I: Integer;
begin
  cbImage.Properties.Items.BeginUpdate;
  try
    cbImage.Properties.Items.Clear;
    for I := 0 to cbImage.Properties.Images.Count - 1 do
    begin
      AItem := cbImage.Properties.Items.Add;
      AItem.ImageIndex := I;
      AItem.Value := I;
    end;
  finally
    cbImage.Properties.Items.EndUpdate;
  end;
end;

procedure TdxBreadcrumbEditDemoRecentPathsForm.LoadPaths(APaths: TdxBreadcrumbEditRecentPaths);
var
  AListItem: TListItem;
  APathItem: TdxBreadcrumbEditRecentPath;
  I: Integer;
begin
  lvPaths.Items.BeginUpdate;
  try
    lvPaths.Items.Clear;
    for I := 0 to APaths.Count - 1 do
    begin
      APathItem := APaths.Items[I];
      AListItem := lvPaths.Items.Add;
      AListItem.Caption := APathItem.Path;
      AListItem.ImageIndex := APathItem.ImageIndex;
    end;
  finally
    lvPaths.Items.EndUpdate;
  end;
end;

procedure TdxBreadcrumbEditDemoRecentPathsForm.SavePaths(APaths: TdxBreadcrumbEditRecentPaths);
var
  I: Integer;
begin
  APaths.BeginUpdate;
  try
    APaths.Clear;
    for I := 0 to lvPaths.Items.Count - 1 do
    begin
      with lvPaths.Items[I] do
        APaths.Add(Caption, ImageIndex);
    end;
  finally
    APaths.EndUpdate;
  end;
end;

procedure TdxBreadcrumbEditDemoRecentPathsForm.tePathPropertiesChange(Sender: TObject);
begin
  UpdateControlsState;
end;

procedure TdxBreadcrumbEditDemoRecentPathsForm.lvPathsSelectItem(
  Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  if lvPaths.Selected <> nil then
  begin
    cbImage.ItemIndex := lvPaths.Selected.ImageIndex;
    tePath.Text := lvPaths.Selected.Caption;
  end
  else
  begin
    cbImage.ItemIndex := -1;
    tePath.Text := '';
  end;
  UpdateControlsState;
end;

procedure TdxBreadcrumbEditDemoRecentPathsForm.UpdateControlsState;
begin
  btnAdd.Enabled := tePath.Text <> '';
  btnDelete.Enabled := lvPaths.Selected <> nil;
  btnReplace.Enabled := btnAdd.Enabled and (lvPaths.Selected <> nil);
end;

end.
