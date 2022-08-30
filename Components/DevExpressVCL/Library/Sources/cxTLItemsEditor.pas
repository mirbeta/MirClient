{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressQuantumTreeList                                   }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSQUANTUMTREELIST AND ALL        }
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

unit cxTLItemsEditor;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, cxStyles, cxLookAndFeelPainters, cxMaskEdit, cxSpinEdit,
  cxContainer, cxEdit, cxTextEdit, StdCtrls, cxButtons, cxControls,
  cxLookAndFeels, cxInplaceContainer, cxTL, cxDBTL, cxTLStrs, Menus,
  ExtCtrls, cxGraphics, cxCustomData, cxDropDownEdit, cxCheckBox, dxLayoutContainer, dxLayoutControlAdapters,
  dxLayoutcxEditAdapters, dxLayoutLookAndFeels, cxClasses, dxLayoutControl, dxForms;

type
  { TfrmItemsEditor }

  TfrmItemsEditor = class(TdxForm, IcxTreeListDesigner)
    mnuEditItems: TPopupMenu;
    NewItem1: TMenuItem;
    NewSubItem1: TMenuItem;
    Delete1: TMenuItem;
    N1: TMenuItem;
    Customize1: TMenuItem;
    btnCustomize: TcxButton;
    btnNewItem: TcxButton;
    btnNewSubItem: TcxButton;
    btnDelete: TcxButton;
    edtImage: TcxTextEdit;
    edtSelected: TcxTextEdit;
    edtState: TcxTextEdit;
    cxTLSite: TcxTreeList;
    cxTreeList1cxTreeListColumn1: TcxTreeListColumn;
    edtOverlay: TcxTextEdit;
    edtOverlayState: TcxTextEdit;
    edtDefImage: TcxTextEdit;
    edtDefSelected: TcxTextEdit;
    edtDefState: TcxTextEdit;
    edtDefOverlay: TcxTextEdit;
    edtDefOverlayState: TcxTextEdit;
    cbCheckGroupType: TcxComboBox;
    cbEnabled: TcxCheckBox;
    btnOk: TcxButton;
    btnCancel: TcxButton;
    lcMainGroup_Root: TdxLayoutGroup;
    lcMain: TdxLayoutControl;
    dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList;
    dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel;
    dxLayoutItem1: TdxLayoutItem;
    dxLayoutItem2: TdxLayoutItem;
    dxLayoutItem3: TdxLayoutItem;
    dxLayoutItem4: TdxLayoutItem;
    dxLayoutItem5: TdxLayoutItem;
    dxLayoutItem6: TdxLayoutItem;
    dxLayoutItem7: TdxLayoutItem;
    dxLayoutItem8: TdxLayoutItem;
    dxLayoutItem9: TdxLayoutItem;
    dxLayoutItem10: TdxLayoutItem;
    dxLayoutItem11: TdxLayoutItem;
    dxLayoutItem12: TdxLayoutItem;
    dxLayoutItem13: TdxLayoutItem;
    dxLayoutItem14: TdxLayoutItem;
    lbCheckType: TdxLayoutItem;
    dxLayoutItem16: TdxLayoutItem;
    dxLayoutItem17: TdxLayoutItem;
    dxLayoutItem18: TdxLayoutItem;
    dxLayoutItem19: TdxLayoutItem;
    dxLayoutGroup1: TdxLayoutGroup;
    dxLayoutGroup2: TdxLayoutGroup;
    dxLayoutGroup3: TdxLayoutGroup;
    dxLayoutGroup4: TdxLayoutGroup;
    dxLayoutGroup5: TdxLayoutGroup;
    gbIndexes: TdxLayoutGroup;
    dxLayoutGroup8: TdxLayoutGroup;
    dxLayoutGroup9: TdxLayoutGroup;
    dxLayoutGroup10: TdxLayoutGroup;
    dxLayoutSeparatorItem1: TdxLayoutSeparatorItem;
    dxLayoutLabeledItem1: TdxLayoutLabeledItem;
    dxLayoutLabeledItem2: TdxLayoutLabeledItem;
    procedure cxButtonPress(Sender: TObject);
    procedure edtImageExit(Sender: TObject);
    procedure edtImageKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure cxTLSiteDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure cbCheckGroupTypePropertiesEditValueChanged(Sender: TObject);
    procedure cxCheckBox1PropertiesEditValueChanged(Sender: TObject);
  private
    FDBTLSite: TcxCustomTreeList;
    FEditControl: TcxCustomTreeList;
    function GetFocusedNode: TcxTreeListNode;
    function GetTreeList: TcxCustomTreeList;
    procedure SetEditControl(AValue: TcxCustomTreeList);
  protected
    procedure CheckEditControl;
    procedure Loaded; override;
    procedure DoOnChangeTreeList(Sender: TObject);
    procedure DoSetImageIndex(Sender: TcxTextEdit);
    function GetValue(Sender: TcxTextEdit): Integer;
    // IcxTreeListDesigner
    procedure ComponentRemoved(Sender: TObject);
    procedure ComponentModified;
  public
    property EditControl: TcxCustomTreeList read FEditControl write SetEditControl;
    property FocusedNode: TcxTreeListNode read GetFocusedNode;
    property TreeList: TcxCustomTreeList read GetTreeList;
  end;

  function cxShowTreeListItemsEditor(AControl: TcxCustomTreeList): Boolean;

implementation

{$R *.dfm}

function cxShowTreeListItemsEditor(AControl: TcxCustomTreeList): Boolean;
var
  AEditor: TfrmItemsEditor;
begin
  AEditor := TfrmItemsEditor.Create(nil);
  try
    AEditor.EditControl := AControl;
    Result := AEditor.ShowModal = mrOk;
    if Result then
      AControl.Assign(AEditor.TreeList);
  finally
    AEditor.Free;
  end;
end;

procedure TfrmItemsEditor.CheckEditControl;
begin
  if EditControl.InheritsFrom(TcxCustomDBTreeList) then
  begin
    FDBTLSite := TcxDBTreeList.Create(Self);
    FDBTLSite.Parent := cxTLSite.Parent;
    TcxTreeList(FDBTLSite).DragMode := cxTLSite.DragMode;
    TcxTreeList(FDBTLSite).OnDragOver := cxTLSite.OnDragOver;
    FDBTLSite.BoundsRect := cxTLSite.BoundsRect;
    FDBTLSite.Anchors := cxTLSite.Anchors;
    FDBTLSite.Visible := True;
    FDBTLSite.BringToFront;
    gbIndexes.Enabled := False;
  end;
  TreeList.Assign(EditControl);
  TreeList.OptionsData.Editing := True;
  TreeList.OptionsSelection.CellSelect := True;
  cbCheckGroupType.Enabled := TreeList.OptionsView.CheckGroups;
  lbCheckType.Enabled := TreeList.OptionsView.CheckGroups;
  cbEnabled.Enabled := TreeList.OptionsView.CheckGroups;
  TcxTreeList(TreeList).OnChange := DoOnChangeTreeList;
  TcxTreeList(TreeList).OnSelectionChanged := DoOnChangeTreeList;
  SetControlLookAndFeel(Self, EditControl.LookAndFeel);
  if TreeList.AbsoluteCount > 0 then
    TreeList.AbsoluteItems[0].Focused := True;
end;

procedure TfrmItemsEditor.Loaded;
begin
  inherited Loaded;
  Constraints.MinHeight := Height;
  Constraints.MinWidth := Width;
end;

procedure TfrmItemsEditor.DoOnChangeTreeList(Sender: TObject);

  procedure SetValues(const I1, I2, I3, I4, I5: string);
  begin
    edtImage.Text := I1;
    edtSelected.Text := I2;
    edtState.Text := I3;
    edtOverlay.Text := I4;
    edtOverlayState.Text := I5;
  end;

begin
  if FocusedNode <> nil then
  begin
    SetValues(IntToStr(FocusedNode.ImageIndex), IntToStr(FocusedNode.SelectedIndex),
      IntToStr(FocusedNode.StateIndex), IntToStr(FocusedNode.OverlayIndex),
      IntToStr(FocusedNode.OverlayStateIndex));
    cbCheckGroupType.ItemIndex := Ord(FocusedNode.Parent.CheckGroupType);
    cbEnabled.Checked := FocusedNode.Enabled;
  end
  else
  begin
    SetValues(edtDefImage.Text, edtDefSelected.Text, edtDefState.Text, edtDefOverlay.Text,
      edtDefOverlayState.Text);
    cbCheckGroupType.ItemIndex := Ord(ncgNone);
    cbEnabled.Checked := False;
  end;
end;

procedure TfrmItemsEditor.DoSetImageIndex(Sender: TcxTextEdit);
var
  I: Integer;
  AList: TList;
  AIndex: Integer;
  ANode: TcxTreeListNode;
begin
  AIndex := GetValue(Sender);
  TreeList.BeginUpdate;
  try
    AList := TList.Create;
    try
      TreeList.GetSelections(AList);
      for I := 0 to AList.Count - 1 do
      begin
        ANode := TcxTreeListNode(AList.List[I]);
        case TcxTreeListImageIndexType(Sender.Tag) of
          tlitSelectedIndex:
            ANode.SelectedIndex := AIndex;
          tlitStateIndex:
            ANode.StateIndex := AIndex;
          tlitOverlayIndex:
            ANode.OverlayIndex := AIndex;
          tlitOverlayStateIndex:
            ANode.OverlayStateIndex := AIndex;
        else
          ANode.ImageIndex := AIndex;
        end;
      end;
    finally
      AList.Free;
    end;
  finally
    TreeList.EndUpdate;
  end;
end;

function TfrmItemsEditor.GetValue(Sender: TcxTextEdit): Integer;
var
  ACode: Integer;
begin
  Val(Sender.Text, Result, ACode);
  if ACode <> 0 then Result := -1;
  Sender.Text := IntToStr(Result);
end;

procedure TfrmItemsEditor.ComponentRemoved(Sender: TObject);
begin
  Hide;
end;

procedure TfrmItemsEditor.ComponentModified;
begin
end;

function TfrmItemsEditor.GetFocusedNode: TcxTreeListNode;
begin
  Result := TreeList.FocusedNode;
end;

function TfrmItemsEditor.GetTreeList: TcxCustomTreeList;
begin
  Result := FDBTLSite;
  if Result = nil then
    Result := cxTLSite;
end;

procedure TfrmItemsEditor.SetEditControl(AValue: TcxCustomTreeList);
begin
  FEditControl := AValue;
  CheckEditControl;
end;

procedure TfrmItemsEditor.cxButtonPress(Sender: TObject);

  procedure CheckAdd(ANode: TcxTreeListNode);
  begin
    if ANode <> nil then
    begin
      TreeList.BeginUpdate;
      try
        ANode.ImageIndex := GetValue(edtDefImage);
        ANode.SelectedIndex := GetValue(edtDefSelected);
        ANode.StateIndex := GetValue(edtDefState);
        ANode.OverlayIndex := GetValue(edtDefOverlay);
        ANode.OverlayStateIndex := GetValue(edtDefOverlayState);
        ANode.MakeVisible;
        ANode.Focused := True;
      finally
        TreeList.EndUpdate;
      end;
    end;
  end;

var
  ATag: Integer;
  AParentNode, ANode: TcxTreeListNode;

begin
  ATag := (Sender as TComponent).Tag;
  ANode := TreeList.FocusedNode;
  if ANode <> nil then
    AParentNode := ANode.Parent
  else
    AParentNode := nil;
  case ATag of
    0:
      TreeList.Customizing.Visible := True;
    3:;
    4:
      if AParentNode = nil then
        CheckAdd(TreeList.Root.AddChild)
      else
        CheckAdd(AParentNode.AddChild);
    5:
      if ANode = nil then
        CheckAdd(TreeList.Root.AddChild)
      else
        CheckAdd(ANode.AddChild);
    6:
      TreeList.DeleteSelection;
  end;
  if (ATag <> 0) and (TreeList.FocusedNode <> nil) then
    TreeList.FocusedNode.MakeVisible;
end;

procedure TfrmItemsEditor.edtImageExit(Sender: TObject);
begin
  DoSetImageIndex(Sender as TcxTextEdit);
end;

procedure TfrmItemsEditor.edtImageKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
    DoSetImageIndex(Sender as TcxTextEdit);
end;

procedure TfrmItemsEditor.cxTLSiteDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := TreeList.HitTest.HitAtNode;
end;

procedure TfrmItemsEditor.cbCheckGroupTypePropertiesEditValueChanged(
  Sender: TObject);
begin
  if FocusedNode <> nil then
    FocusedNode.Parent.CheckGroupType := TcxTreeListNodeCheckGroupType(cbCheckGroupType.ItemIndex);
end;

procedure TfrmItemsEditor.cxCheckBox1PropertiesEditValueChanged(
  Sender: TObject);
begin
  if FocusedNode <> nil then
    FocusedNode.Enabled := (Sender as TcxCheckBox).Checked;
end;

end.
