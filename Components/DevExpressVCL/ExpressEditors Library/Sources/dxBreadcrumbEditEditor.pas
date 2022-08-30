{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressEditors                                           }
{                                                                    }
{           Copyright (c) 1998-2019 Developer Express Inc.           }
{           ALL RIGHTS RESERVED                                      }
{                                                                    }
{   The entire contents of this file is protected by U.S. and        }
{   International Copyright Laws. Unauthorized reproduction,         }
{   reverse-engineering, and distribution of all or any portion of   }
{   the code contained in this file is strictly prohibited and may   }
{   result in severe civil and criminal penalties and will be        }
{   prosecuted to the maximum extent possible under the law.         }
{                                                                    }
{   RESTRICTIONS                                                     }
{                                                                    }
{   THIS SOURCE CODE AND ALL RESULTING INTERMEDIATE FILES            }
{   (DCU, OBJ, DLL, ETC.) ARE CONFIDENTIAL AND PROPRIETARY TRADE     }
{   SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER IS    }
{   LICENSED TO DISTRIBUTE THE EXPRESSEDITORS AND ALL                }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY. }
{                                                                    }
{   THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED       }
{   FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE         }
{   COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE        }
{   AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT   }
{   AND PERMISSION FROM DEVELOPER EXPRESS INC.                       }
{                                                                    }
{   CONSULT THE END USER LICENSE AGREEMENT FOR INFORMATION ON        }
{   ADDITIONAL RESTRICTIONS.                                         }
{                                                                    }
{********************************************************************}

unit dxBreadcrumbEditEditor;

{$I cxVer.inc}

interface

uses
  Windows, SysUtils, Classes, Controls, Dialogs, ExtCtrls, Forms, Graphics, cxGraphics,
  cxLookAndFeels, cxLookAndFeelPainters, Menus, StdCtrls, cxButtons, dxBreadCrumbEdit,
  ComCtrls, cxControls, cxContainer, cxEdit, cxTreeView, cxGroupBox, dxBevel,
  cxLabel, cxTextEdit, cxMaskEdit, cxDropDownEdit, cxImageComboBox, cxSpinEdit,
  cxCheckBox, dxForms;

type

  { TdxBreadcrumbEditEditorAdditionalData }

  TdxBreadcrumbEditEditorAdditionalData = class
  private
    FText: string;
  public
    constructor Create(const AText: string); virtual;
    //
    property Text: string read FText write FText;
  end;

  { TdxBreadcrumbEditEditorForm }

  TdxBreadcrumbEditEditorForm = class(TdxForm)
    btnDeleteItem: TcxButton;
    btnNewItem: TcxButton;
    btnNewSubItem: TcxButton;
    bvlLine: TdxBevel;
    cbSaveSelection: TcxCheckBox;
    cxBtnCancel: TcxButton;
    cxBtnOK: TcxButton;
    gbItemOptions: TcxGroupBox;
    lbItemDisplayName: TcxLabel;
    lbItemImageIndex: TcxLabel;
    lbItemText: TcxLabel;
    Panel2: TPanel;
    pnlBottomBar: TPanel;
    pnlClient: TPanel;
    seItemImageIndex: TcxSpinEdit;
    teItemDisplayName: TcxTextEdit;
    teItemText: TcxTextEdit;
    tvStruct: TcxTreeView;

    procedure btnDeleteItemClick(Sender: TObject);
    procedure btnNewItemClick(Sender: TObject);
    procedure btnNewSubItemClick(Sender: TObject);
    procedure teItemOptionsChange(Sender: TObject);
    procedure tvStructChange(Sender: TObject; Node: TTreeNode);
    procedure tvStructDeletion(Sender: TObject; Node: TTreeNode);
    procedure tvStructDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure tvStructDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
  protected
    FUpdatingItemOptionsData: Boolean;
    function GenerateSubItemName(ANode: TTreeNode): string;
    function GetAdditionalData(ANode: TTreeNode): TdxBreadcrumbEditEditorAdditionalData;
    function GetHasSelection: Boolean;
    function GetSelectedPath(APathDelimiter: Char): string;
    function IsTreeRoot(ANode: TTreeNode): Boolean;
    procedure EnableContainerControls(AContainer: TWinControl; AEnabled: Boolean);
  public
    procedure Load(ABreadcrumbEdit: TdxBreadcrumbEdit);
    procedure Save(ABreadcrumbEdit: TdxBreadcrumbEdit);
    procedure UpdateCaption(AComponent: TComponent);
    procedure UpdateControlsState;
    procedure UpdateItemOptionsData;
    //
    property HasSelection: Boolean read GetHasSelection;
  end;

procedure dxBreadcrumbEditShowEditor(ABreadcrumbEdit: TdxBreadcrumbEdit);
implementation

uses
  StrUtils, Variants;

{$R *.dfm}

const
  sdxBreadcrumbEditEditorCaption = '%s%s - BreadcrumbEdit Editor';
  sdxBreadcrumbEditEditorNewItem = 'New Item';

procedure dxBreadcrumbEditShowEditor(ABreadcrumbEdit: TdxBreadcrumbEdit);
begin
  with TdxBreadcrumbEditEditorForm.Create(Application) do
  try
    Load(ABreadcrumbEdit);
    UpdateCaption(ABreadcrumbEdit);
    tvStruct.Images := ABreadcrumbEdit.Properties.Images;
    tvStruct.FullExpand;
    if ShowModal = mrOk then
    begin
      Save(ABreadcrumbEdit);
      SetDesignerModified(ABreadcrumbEdit);
    end;
  finally
    Free;
  end;
end;

{ TdxBreadcrumbEditEditorAdditionalData }

constructor TdxBreadcrumbEditEditorAdditionalData.Create(const AText: string);
begin
  FText := AText;
end;

{ TdxBreadcrumbEditEditorForm }

procedure TdxBreadcrumbEditEditorForm.btnDeleteItemClick(Sender: TObject);
begin
  if HasSelection and not IsTreeRoot(tvStruct.Selected) then
    tvStruct.Items.Delete(tvStruct.Selected);
end;

procedure TdxBreadcrumbEditEditorForm.btnNewItemClick(Sender: TObject);
var
  ANode: TTreeNode;
begin
  if HasSelection and not IsTreeRoot(tvStruct.Selected) then
  begin
    ANode := tvStruct.Items.Add(tvStruct.Selected, GenerateSubItemName(tvStruct.Selected.Parent));
    ANode.Data := TdxBreadcrumbEditEditorAdditionalData.Create('');
    tvStruct.Selected := ANode;
  end;
end;

procedure TdxBreadcrumbEditEditorForm.btnNewSubItemClick(Sender: TObject);
var
  ANode: TTreeNode;
begin
  if HasSelection then
  begin
    ANode := tvStruct.Items.AddChild(tvStruct.Selected, GenerateSubItemName(tvStruct.Selected));
    ANode.Data := TdxBreadcrumbEditEditorAdditionalData.Create('');
    tvStruct.Selected := ANode;
  end;
end;

procedure TdxBreadcrumbEditEditorForm.EnableContainerControls(
  AContainer: TWinControl; AEnabled: Boolean);
var
  I: Integer;
begin
  for I := 0 to AContainer.ControlCount - 1 do
    AContainer.Controls[I].Enabled := AEnabled;
end;

function TdxBreadcrumbEditEditorForm.GenerateSubItemName(ANode: TTreeNode): string;

  function CheckName(const AName: string): Boolean;
  var
    I: Integer;
  begin
    Result := True;
    for I := 0 to ANode.Count - 1 do
      if SameText(ANode[I].Text, AName) then
      begin
        Result := False;
        Break;
      end;
  end;

var
  AIndex: Integer;
begin
  Result := sdxBreadcrumbEditEditorNewItem;
  if not CheckName(Result) then
  begin
    AIndex := 1;
    repeat
      Result := sdxBreadcrumbEditEditorNewItem + ' (' + IntToStr(AIndex) + ')';
      Inc(AIndex);
    until CheckName(Result);
  end;
end;

function TdxBreadcrumbEditEditorForm.GetAdditionalData(ANode: TTreeNode): TdxBreadcrumbEditEditorAdditionalData;
begin
  Result := TdxBreadcrumbEditEditorAdditionalData(ANode.Data);
end;

function TdxBreadcrumbEditEditorForm.GetHasSelection: Boolean;
begin
  Result := tvStruct.Selected <> nil;
end;

function TdxBreadcrumbEditEditorForm.GetSelectedPath(APathDelimiter: Char): string;

  function GetPath(ANode: TTreeNode): string;
  begin
    if ANode <> nil then
      Result := GetPath(ANode.Parent) + ANode.Text + APathDelimiter
    else
      Result := '';
  end;

begin
  Result := GetPath(tvStruct.Selected);
end;

function TdxBreadcrumbEditEditorForm.IsTreeRoot(ANode: TTreeNode): Boolean;
begin
  Result := tvStruct.Items[0] = ANode;
end;

procedure TdxBreadcrumbEditEditorForm.Load(ABreadcrumbEdit: TdxBreadcrumbEdit);

  procedure LoadNode(AParentTreeNode: TTreeNode; ANode, ASelectedNode: TdxBreadcrumbEditNode);
  var
    ATreeNode: TTreeNode;
    I: Integer;
  begin
    ATreeNode := tvStruct.Items.AddChild(AParentTreeNode, ANode.Name);
    ATreeNode.Data := TdxBreadcrumbEditEditorAdditionalData.Create(ANode.DisplayName);
    ATreeNode.ImageIndex := ANode.ImageIndex;
    ATreeNode.SelectedIndex := ANode.ImageIndex;
    if ANode = ASelectedNode then
      tvStruct.Selected := ATreeNode;
    for I := 0 to ANode.Count - 1 do
      LoadNode(ATreeNode, ANode[I], ASelectedNode);
  end;

begin
  tvStruct.Items.BeginUpdate;
  try
    tvStruct.Items.Clear;
    LoadNode(nil, ABreadcrumbEdit.Root, ABreadcrumbEdit.Selected);
  finally
    tvStruct.Items.EndUpdate;
  end;
end;

procedure TdxBreadcrumbEditEditorForm.Save(ABreadcrumbEdit: TdxBreadcrumbEdit);

  procedure SaveNode(ATreeNode, ASelectedTreeNode: TTreeNode; ANode: TdxBreadcrumbEditNode);
  var
    I: Integer;
  begin
    ANode.DisplayName := GetAdditionalData(ATreeNode).Text;
    ANode.ImageIndex := ATreeNode.ImageIndex;
    ANode.Name := ATreeNode.Text;
    for I := 0 to ATreeNode.Count - 1 do
      SaveNode(ATreeNode[I], ASelectedTreeNode, ANode.AddChild);
  end;

begin
  ABreadcrumbEdit.BeginUpdate;
  try
    ABreadcrumbEdit.Root.Clear;
    SaveNode(tvStruct.Items[0], tvStruct.Selected, ABreadcrumbEdit.Root);
    if cbSaveSelection.Checked then
      ABreadcrumbEdit.SelectedPath := GetSelectedPath(ABreadcrumbEdit.Properties.PathEditor.PathDelimiter);
  finally
    ABreadcrumbEdit.EndUpdate;
  end;
end;

procedure TdxBreadcrumbEditEditorForm.teItemOptionsChange(Sender: TObject);
begin
  if not FUpdatingItemOptionsData and (tvStruct.Selected <> nil) then
  begin
    tvStruct.Selected.Text := teItemText.EditingValue;
    if teItemDisplayName.EditingValue <> Null then
      GetAdditionalData(tvStruct.Selected).Text := teItemDisplayName.EditingValue
    else
      GetAdditionalData(tvStruct.Selected).Text := '';
    tvStruct.Selected.ImageIndex := seItemImageIndex.EditingValue;
    tvStruct.Selected.SelectedIndex := tvStruct.Selected.ImageIndex;
    tvStruct.Invalidate;
  end;
end;

procedure TdxBreadcrumbEditEditorForm.tvStructChange(Sender: TObject; Node: TTreeNode);
begin
  UpdateControlsState;
  UpdateItemOptionsData;
end;

procedure TdxBreadcrumbEditEditorForm.tvStructDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  AAddMode: TNodeAttachMode;
  AInsertAsChild: Boolean;
  ANode: TTreeNode;
begin
  if tvStruct.Selected <> nil then
  begin
    AInsertAsChild := GetKeyState(VK_SHIFT) < 0;
    if AInsertAsChild then
      AAddMode := naAddChild
    else
      AAddMode := naInsert;

    ANode := tvStruct.GetNodeAt(X, Y);
    if ANode = nil then
    begin
      ANode := tvStruct.Items[tvStruct.Items.Count - 1];
      while (ANode <> nil) and not ANode.IsVisible do
        ANode := ANode.GetPrev;
      if not AInsertAsChild then
        AAddMode := naAdd;
    end;

    if ANode <> nil then
    begin
      tvStruct.Selected.MoveTo(ANode, AAddMode);
      tvStruct.Selected.Selected := True;
    end;
  end;
end;

procedure TdxBreadcrumbEditEditorForm.tvStructDragOver(
  Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := True;
end;

procedure TdxBreadcrumbEditEditorForm.UpdateCaption(AComponent: TComponent);
var
  AOwnerCaption: string;
begin
  if AComponent.Owner <> nil then
    AOwnerCaption := AComponent.Owner.Name + '.'
  else
    AOwnerCaption := '';

  Caption := Format(sdxBreadcrumbEditEditorCaption, [AOwnerCaption, AComponent.Name]);
end;

procedure TdxBreadcrumbEditEditorForm.UpdateControlsState;
begin
  btnNewItem.Enabled := HasSelection and not IsTreeRoot(tvStruct.Selected);
  btnNewSubItem.Enabled := HasSelection;
  btnDeleteItem.Enabled := HasSelection and not IsTreeRoot(tvStruct.Selected);
  EnableContainerControls(gbItemOptions, HasSelection);
end;

procedure TdxBreadcrumbEditEditorForm.UpdateItemOptionsData;
begin
  FUpdatingItemOptionsData := True;
  try
    if tvStruct.Selected <> nil then
    begin
      seItemImageIndex.Value := tvStruct.Selected.ImageIndex;
      teItemText.Text := tvStruct.Selected.Text;
      teItemDisplayName.Text := GetAdditionalData(tvStruct.Selected).Text;
    end
    else
    begin
      seItemImageIndex.Value := -1;
      teItemText.Text := '';
      teItemDisplayName.Text := '';
    end;
  finally
    FUpdatingItemOptionsData := False;
  end;
end;

procedure TdxBreadcrumbEditEditorForm.tvStructDeletion(Sender: TObject; Node: TTreeNode);
begin
  GetAdditionalData(Node).Free;
end;

end.
