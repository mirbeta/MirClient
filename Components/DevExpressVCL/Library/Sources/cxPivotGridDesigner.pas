{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressPivotGrid                                         }
{                                                                    }
{           Copyright (c) 2005-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSPIVOTGRID AND ALL ACCOMPANYING }
{   VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY.              }
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
unit cxPivotGridDesigner;

interface

{$I cxVer.inc}

uses
  DesignIntf, Variants,
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, StdCtrls, ComCtrls,
  cxClasses, cxLookAndFeelPainters, cxButtons,
  cxControls, cxContainer, cxListBox, cxCustomPivotGrid, cxDesignWindows,
  cxTreeView, ExtCtrls, cxGraphics, cxLookAndFeels, cxEdit, dxLayoutContainer, dxLayoutControlAdapters,
  dxLayoutLookAndFeels, dxLayoutControl;

type
  TfrmPivotGridDesigner = class(TcxDesignFormEditor)
    btnClose: TcxButton;
    btnDelete: TcxButton;
    btnMoveUp: TcxButton;
    btnAdd: TcxButton;
    btnMoveDown: TcxButton;
    btnRetrieveFields: TcxButton;
    lbFields: TcxListBox;
    pmFields: TPopupMenu;
    miMoveUp: TMenuItem;
    miMoveDown: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    miSelectAll: TMenuItem;
    miAdd: TMenuItem;
    miDelete: TMenuItem;
    tvGroups: TcxTreeView;
    btnUnlink: TcxButton;
    btnLink: TcxButton;
    lbUnlinkedFields: TcxListBox;
    btnAddGroup: TcxButton;
    btnDeleteGroup: TcxButton;
    pmGroups: TPopupMenu;
    miAddGroup: TMenuItem;
    miDeleteGroup: TMenuItem;
    MenuItem3: TMenuItem;
    miMoveUpInGroup: TMenuItem;
    miMoveDownInGroup: TMenuItem;
    btnMoveUpGroup: TcxButton;
    btnMoveDownGroup: TcxButton;
    dxLayoutControl1Group_Root: TdxLayoutGroup;
    dxLayoutControl1: TdxLayoutControl;
    pcDesigner: TdxLayoutGroup;
    tbsFields: TdxLayoutGroup;
    dxLayoutItem1: TdxLayoutItem;
    dxLayoutGroup3: TdxLayoutGroup;
    dxLayoutItem2: TdxLayoutItem;
    dxLayoutItem3: TdxLayoutItem;
    dxLayoutItem4: TdxLayoutItem;
    dxLayoutItem5: TdxLayoutItem;
    liRetrieveFields: TdxLayoutItem;
    tbsGroups: TdxLayoutGroup;
    dxLayoutItem7: TdxLayoutItem;
    dxLayoutGroup5: TdxLayoutGroup;
    dxLayoutItem8: TdxLayoutItem;
    dxLayoutItem9: TdxLayoutItem;
    dxLayoutItem10: TdxLayoutItem;
    dxLayoutItem11: TdxLayoutItem;
    dxLayoutItem12: TdxLayoutItem;
    dxLayoutItem13: TdxLayoutItem;
    dxLayoutItem14: TdxLayoutItem;
    dxLayoutGroup6: TdxLayoutGroup;
    dxLayoutItem15: TdxLayoutItem;
    dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList;
    dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel;
    procedure btnFieldsPageClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure lbFieldsClick(Sender: TObject);
    procedure lbFieldsDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure lbFieldsDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure tbsFieldsStartDrag(Sender: TObject; var DragObject: TDragObject);
    procedure lbFieldsEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure tvGroupsEditing(Sender: TObject; Node: TTreeNode; var AllowEdit: Boolean);
    procedure btnGroupClick(Sender: TObject);
    procedure tvGroupsCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure tvGroupsClick(Sender: TObject);
    procedure lbUnlinkedFieldsClick(Sender: TObject);
    procedure tvGroupsDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure tvGroupsDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure tvGroupsMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure miMoveInGroupClick(Sender: TObject);
    procedure tvGroupsChange(Sender: TObject; Node: TTreeNode);
    procedure tvGroupsEdited(Sender: TObject; Node: TTreeNode; var S: String);
    procedure tvGroupsDblClick(Sender: TObject);
    procedure lbUnlinkedFieldsDblClick(Sender: TObject);
    procedure tvGroupsKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
  private
    function GetPivotIntf: IcxPivotGridDesignerIntf;
    function GetPivotGrid: TcxCustomPivotGrid;
  protected
    FIsLocked: Boolean;
    FPrevDragIndex: Integer;
    DeletedGroup: TObject;
    procedure AddField;
    procedure DeleteSelection;
    function GetFieldsFromDragSource(ASource: TObject): TList;
    function GetGroup(AGroupOnly: Boolean = False): TcxPivotGridFieldGroup;
    function GetGroupField: TcxPivotGridField;
    procedure InitFormEditor; override;
    procedure SelectionMoveUp;
    procedure SelectionMoveDown;
    procedure SelectItem(AItem: TPersistent);
    procedure ReindexFields(AList: TList; ANewIndex: Integer);
    procedure RetrieveFields;
    procedure SyncGroupSelection;

    procedure UpdateButtons;
    procedure UpdateDesigner;
    procedure UpdateFieldList;
    procedure UpdateFieldListBox(AListBox: TcxListBox; AUnlinkedOnly: Boolean);
    // groups
    procedure UpdateGroups;
    procedure UpdateGroupsButtons;
    procedure UpdateUnlinkedFieldList;

  public
    procedure DoItemsModified; override;
    procedure SelectionsChanged(const ASelection: TDesignerSelectionList); override;

    property PivotGrid: TcxCustomPivotGrid read GetPivotGrid;
    property PivotIntf: IcxPivotGridDesignerIntf read GetPivotIntf;
  end;

  procedure ShowPivotGridDesigner(ADesigner: IDesigner; APivotGrid: TcxCustomPivotGrid);

implementation

{$R *.dfm}

procedure ShowPivotGridDesigner(ADesigner: IDesigner; APivotGrid: TcxCustomPivotGrid);
begin
  ShowFormEditorClass(ADesigner, APivotGrid, TfrmPivotGridDesigner);
end;

procedure GetTreeViewSelections(ATreeView: TTreeView; ASelection: TList);
begin
  ATreeView.GetSelections(ASelection);
end;

procedure TfrmPivotGridDesigner.btnFieldsPageClick(Sender: TObject);
begin
  case TComponent(Sender).Tag of
    0:
      AddField;
    1:
      DeleteSelection;
    2:
      SelectionMoveUp;
    3:
      SelectionMoveDown;
    4:
      RetrieveFields;
    5:
      ListBoxSelectAll(lbFields.InnerListBox);
  end;
end;

procedure TfrmPivotGridDesigner.DoItemsModified;
begin
  UpdateFieldList;
end;

procedure TfrmPivotGridDesigner.SelectionsChanged(
  const ASelection: TDesignerSelectionList);
begin
  if (PivotGrid = nil) or PivotGrid.IsLoading or PivotGrid.IsDestroying or FIsLocked then Exit;
  ListBoxSynchronizeSelection(lbFields.InnerListBox);
  UpdateGroups;
  UpdateButtons;
end;

procedure TfrmPivotGridDesigner.AddField;
var
  AField: TcxPivotGridField;
  APrefix: string;
begin
  ListBoxClearSelection(lbFields.InnerListBox);
  AField := PivotIntf.CreateField;
  APrefix := AField.ClassName;
  APrefix := Copy(APrefix, 1, Pos('Field', APrefix) - 1);
  AField.Name := CreateUniqueName(PivotGrid.Owner, PivotGrid, AField, APrefix, '');
  UpdateFieldList;
  ListBoxSelectByObject(lbFields.InnerListBox, AField);
  lbFieldsClick(nil);
  AField.Visible := True;
  UpdateDesigner;
end;

procedure TfrmPivotGridDesigner.DeleteSelection;
begin
  if lbFields.SelCount > 0 then
  begin
    BeginUpdate;
    try
      PivotGrid.BeginUpdate;
      try
        ListBoxDeleteSelection(lbFields.InnerListBox, True);
      finally
        PivotGrid.EndUpdate;
      end;
    finally
      EndUpdate;
      UpdateFieldList;
      lbFieldsClick(nil);
      UpdateDesigner;
    end;
  end;
end;

function TfrmPivotGridDesigner.GetFieldsFromDragSource(ASource: TObject): TList;
var
  I: Integer;
begin
  Result := TList.Create;
  if ASource is TcxDragControlObject then
  begin
    if TcxDragControlObject(ASource).Control = lbUnlinkedFields then
    begin
      for I := 0 to lbUnlinkedFields.Items.Count - 1 do
        if lbUnlinkedFields.Selected[I] then
          Result.Add(lbUnlinkedFields.Items.Objects[I]);
    end
    else
      if TcxDragControlObject(ASource).Control = tvGroups then
      begin
        GetTreeViewSelections(tvGroups.InnerTreeView, Result);
        for I := 0 to Result.Count - 1 do
          Result[I] := TTreeNode(Result[I]).Data;
      end;
  end;
end;

function TfrmPivotGridDesigner.GetGroup(AGroupOnly: Boolean = False): TcxPivotGridFieldGroup;
begin
  Result := nil;
  if tvGroups.Selected = nil then Exit;
  if (tvGroups.Selected.Level = 0) then
    Result := TcxPivotGridFieldGroup(tvGroups.Selected.Data)
  else
    if not AGroupOnly then
      Result := TcxPivotGridFieldGroup(tvGroups.Selected.Parent.Data);
end;

function TfrmPivotGridDesigner.GetGroupField: TcxPivotGridField;
begin
  if (tvGroups.Selected <> nil) and (tvGroups.Selected.Level = 1) then
    Result := TcxPivotGridField(tvGroups.Selected.Data)
  else
    Result := nil;
end;

procedure TfrmPivotGridDesigner.InitFormEditor;
begin
  inherited InitFormEditor;
  FPrevDragIndex := -1;
  UpdateFieldList;
  UpdateSelection;
  UpdateButtons;
end;

procedure TfrmPivotGridDesigner.SelectionMoveUp;
begin
  PivotGrid.BeginUpdate;
  try
    ListBoxMoveUpItems(lbFields.InnerListBox, FPrevDragIndex, ReindexFields);
  finally
    PivotGrid.EndUpdate;
  end;
  UpdateDesigner;
end;

procedure TfrmPivotGridDesigner.SelectionMoveDown;
begin
  PivotGrid.BeginUpdate;
  try
    ListBoxMoveDownItems(lbFields.InnerListBox, FPrevDragIndex, ReindexFields);
  finally
    PivotGrid.EndUpdate;
  end;
  UpdateDesigner;
end;

procedure TfrmPivotGridDesigner.SelectItem(AItem: TPersistent);
begin
  if AItem = nil then
    Designer.SelectComponent(AItem)
  else
    Designer.SelectComponent(Component);
end;

procedure TfrmPivotGridDesigner.ReindexFields(
  AList: TList; ANewIndex: Integer);
var
  I: Integer;
begin
  if AList.Count = 0 then Exit;
  if TcxPivotGridField(AList[0]).Index < ANewIndex then
  begin
    for I := 0 to AList.Count - 1 do
      TcxPivotGridField(AList[I]).Index := ANewIndex;
  end
  else
  begin
    for I := AList.Count - 1 downto 0 do
      TcxPivotGridField(AList[I]).Index := ANewIndex;
  end;
  UpdateDesigner;
end;

procedure TfrmPivotGridDesigner.RetrieveFields;
const
  Flags = MB_YESNOCANCEL or MB_ICONWARNING;
  scxConfirm      = 'Confirm';
  scxDeleteFields = 'Delete existing fields?';
begin
  PivotGrid.BeginUpdate;
  try
    if PivotGrid.FieldCount > 0 then
    begin
      case MessageBox(0, scxDeleteFields, scxConfirm, Flags) of
        mrCancel:
          Exit;
        mrYes:
          PivotGrid.DeleteAllFields;
      end;
    end;
    PivotIntf.DoCreateAllFields;
    UpdateFieldList;
    lbFieldsClick(nil);
    UpdateDesigner;
  finally
    PivotGrid.EndUpdate;
  end;
end;

procedure TfrmPivotGridDesigner.SyncGroupSelection;
begin
  if pcDesigner.ItemIndex = tbsGroups.Index then
  begin
    FIsLocked := True;
    if tvGroups.Selected <> nil then
      SelectComponent(TPersistent(tvGroups.Selected.Data))
    else
      SelectComponent(PivotGrid);
    FIsLocked := False;
  end;
end;

function TfrmPivotGridDesigner.GetPivotIntf: IcxPivotGridDesignerIntf;
begin
  Result := PivotGrid as IcxPivotGridDesignerIntf;
end;

function TfrmPivotGridDesigner.GetPivotGrid: TcxCustomPivotGrid;
begin
  Result := Component as TcxCustomPivotGrid;
end;

procedure TfrmPivotGridDesigner.UpdateButtons;
var
  AIsActive: Boolean;
begin
  btnAdd.Enabled := CanAddComponent;
  btnDelete.Enabled := (lbFields.SelCount > 0) and CanChangeComponentList(PivotGrid);
  btnMoveUp.Enabled := lbFields.SelCount > 0;
  btnMoveDown.Enabled := lbFields.SelCount > 0;
  miMoveUp.Enabled := btnMoveUp.Enabled;
  miMoveDown.Enabled := btnMoveDown.Enabled;
  miDelete.Enabled := btnDelete.Enabled;
  miSelectAll.Enabled := lbFields.Count > 0;
  liRetrieveFields.Visible := PivotIntf.CanRetrieveFields(AIsActive);
  liRetrieveFields.Enabled := AIsActive;
end;

procedure TfrmPivotGridDesigner.UpdateDesigner;
begin
  Designer.Modified;
end;

procedure TfrmPivotGridDesigner.UpdateFieldList;
begin
  UpdateFieldListBox(lbFields, False);
  UpdateGroups;
end;

procedure TfrmPivotGridDesigner.UpdateFieldListBox(
  AListBox: TcxListBox; AUnlinkedOnly: Boolean);
var
  AField: TcxPivotGridField;
  ASelection: TStringList;
  I, AItemIndex, ATopIndex: Integer;
begin
//  if not AUnlinkedOnly or (pcDesigner.ActivePage = tbsGroups) then
  ListBoxSaveSelection(AListBox.InnerListBox, ASelection, AItemIndex, ATopIndex);
  try
    AListBox.Items.Clear;
    for I := 0 to PivotGrid.FieldCount - 1 do
    begin
      AField := PivotGrid.Fields[I];
      if not AUnlinkedOnly or (AField.Group = nil) then
        AListBox.Items.AddObject(AField.Name, AField);
    end;
  finally
//    if not AUnlinkedOnly or (pcDesigner.ActivePage = tbsGroups) then
    ListBoxRestoreSelection(AListBox.InnerListBox, ASelection, AItemIndex, ATopIndex);
  end;
  UpdateButtons;
end;

procedure TfrmPivotGridDesigner.btnCloseClick(Sender: TObject);
begin
  if not tvGroups.IsEditing then
    Hide
  else
  begin
    if tvGroups.Selected <> nil then
      tvGroups.Selected.EndEdit(False);
  end;
end;

procedure TfrmPivotGridDesigner.lbFieldsClick(Sender: TObject);
begin
  ListBoxApplySelection(lbFields.InnerListBox, PivotGrid);
end;

procedure TfrmPivotGridDesigner.lbFieldsDragOver(Sender, Source: TObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  ListBoxDragOver(lbFields.InnerListBox, Sender, Source,
    X, Y, State, Accept, FPrevDragIndex);
end;

procedure TfrmPivotGridDesigner.lbFieldsDragDrop(Sender, Source: TObject;
  X, Y: Integer);
begin
  ListBoxDragDrop(lbFields.InnerListBox, Sender, Source,
    X, Y, FPrevDragIndex, ReindexFields);
  UpdateDesigner;
end;

procedure TfrmPivotGridDesigner.tbsFieldsStartDrag(Sender: TObject;
  var DragObject: TDragObject);
begin
  FPrevDragIndex := -1;
end;

procedure TfrmPivotGridDesigner.lbFieldsEndDrag(Sender, Target: TObject; X,
  Y: Integer);
begin
  ListBoxEndDrag(lbFields.InnerListBox, Sender, Target, X, Y, FPrevDragIndex);
end;

// groups

procedure TreeViewSaveSelection(ATreeView: TTreeView; var ASelection: TList; var AItemIndex, ATopIndex: Integer);

  function GetIndexFrom(ANode: TTreeNode): Integer;
  begin
    Result := 0;
    if ANode <> nil then
      Result := ANode.AbsoluteIndex;
  end;

var
  I: Integer;
begin
  ASelection := TList.Create;
  try
    ATopIndex := GetIndexFrom(ATreeView.TopItem);
    AItemIndex := GetIndexFrom(ATreeView.Selected);

    GetTreeViewSelections(ATreeView, ASelection);
    for I := 0 to ASelection.Count - 1 do
      ASelection[I] := TTreeNode(ASelection[I]).Data;
    ATreeView.Items.BeginUpdate;
  except
    FreeAndNil(ASelection);
  end;
end;

procedure TreeViewRestoreSelection(ATreeView: TTreeView; var ASelection: TList;
  var AItemIndex, ATopIndex: Integer; ASyncSelection: Boolean);

  function CheckIndex(var AIndex: Integer): Boolean;
  begin
    while AIndex > ATreeView.Items.Count - 1 do Dec(AIndex);
    Result := AIndex >= 0;
  end;

var
  I: Integer;
  APrevOnClick: TNotifyEvent;
begin
  APrevOnClick := ATreeView.OnClick;
  try
    ATreeView.OnClick := nil;
    if ASyncSelection then
    begin
      for I := 0 to ATreeView.Items.Count - 1 do
        if ASelection.IndexOf(ATreeView.Items[I].Data) <> -1 then
           ATreeView.Items[I].Selected := True;
    end;
    if CheckIndex(ATopIndex) then
      ATreeView.TopItem := ATreeView.Items[ATopIndex];
    if ASyncSelection then
    begin
      if CheckIndex(AItemIndex) and (ATreeView.Selected = nil) then
        ATreeView.Items[AItemIndex].Selected := True;
      if ATreeView.Selected <> nil then
        ATreeView.Selected.MakeVisible;
    end;
  finally
    ATreeView.OnClick := APrevOnClick;
    ATreeView.Items.EndUpdate;
    FreeAndNil(ASelection);
  end;
end;

procedure TfrmPivotGridDesigner.UpdateGroups;
var
  I, J: Integer;
  ASelection: TList;
  AField: TcxPivotGridField;
  ACurSelectedNode, ANode, AGroupNode: TTreeNode;
  AGroup: TcxPivotGridFieldGroup;
  ATopIndex, AItemIndex: Integer;
  ASelList: TList;
begin
  if LockCount > 0 then Exit;
  ASelList := TList.Create;
  ACurSelectedNode := nil;
  TreeViewSaveSelection(tvGroups.InnerTreeView, ASelection, AItemIndex, ATopIndex);
  try
    LockWindowUpdate(tvGroups.Handle);
    tvGroups.Items.Clear;
    for I := 0 to PivotGrid.Groups.Count - 1 do
    begin
      AGroup := PivotGrid.Groups[I];
      GetSelectionList(ASelList);
      AGroupNode := tvGroups.Items.AddChildObject(nil, AGroup.Caption, AGroup);
      if ASelList.IndexOf(AGroup) >= 0 then
        ACurSelectedNode := AGroupNode;
      if AGroup <> DeletedGroup then
        for J := 0 to AGroup.FieldCount - 1 do
        begin
          AField :=  AGroup.Fields[J];
          ANode := tvGroups.Items.AddChildObject(AGroupNode, AField.Name, AField);
          if ASelList.IndexOf(AField) >= 0 then
            ACurSelectedNode := ANode;
        end;
    end;
    tvGroups.FullExpand;
  finally
    ASelList.Free;
    TreeViewRestoreSelection(tvGroups.InnerTreeView, ASelection, AItemIndex, ATopIndex, ACurSelectedNode = nil);
    tvGroups.SetScrollBarsParameters();
    LockWindowUpdate(0);
    tvGroups.Update;
  end;
  if ACurSelectedNode <> nil then
    tvGroups.Selected := ACurSelectedNode;
  UpdateUnlinkedFieldList;
  UpdateGroupsButtons;
end;

procedure TfrmPivotGridDesigner.UpdateGroupsButtons;

  function CanMoveNodeUp(ANode: TTreeNode): Boolean;
  begin
    Result := (ANode <> nil) and (ANode.AbsoluteIndex > 0);
    if Result then
    begin
      if ANode.Level = 0 then
        Result := (ANode.Index > 0)
      else
        Result := (ANode.Index > 0) or (ANode.Parent.Index > 0);
    end;
  end;

  function CanMoveNodeDown(ANode: TTreeNode): Boolean;
  begin
    Result := (ANode <> nil) and (ANode.AbsoluteIndex < ANode.Owner.Count);
    if Result then
    begin
      if ANode.Level = 0 then
        Result := ANode.getNextSibling <> nil
      else
        Result := (ANode.getNextSibling <> nil) or (ANode.Parent.getNextSibling <> nil);
    end;
  end;

begin
  btnDeleteGroup.Enabled := GetGroup(True) <> nil;
  btnUnlink.Enabled := GetGroupField <> nil;
  btnLink.Enabled := (GetGroup <> nil) and (lbUnlinkedFields.SelCount > 0);
  btnMoveUpGroup.Enabled := CanMoveNodeUp(tvGroups.Selected);
  btnMoveDownGroup.Enabled := CanMoveNodeDown(tvGroups.Selected);
  miMoveUpInGroup.Enabled := btnMoveUpGroup.Enabled;
  miMoveDownInGroup.Enabled := btnMoveDownGroup.Enabled;
end;

procedure TfrmPivotGridDesigner.UpdateUnlinkedFieldList;
begin
  UpdateFieldListBox(lbUnlinkedFields, True);
end;

procedure TfrmPivotGridDesigner.tvGroupsEditing(Sender: TObject;
  Node: TTreeNode; var AllowEdit: Boolean);
begin
  AllowEdit := Node.Level = 0;
end;

procedure TfrmPivotGridDesigner.btnGroupClick(Sender: TObject);
var
  I: Integer;
  ANode: TTreeNode;
  AField: TcxPivotGridField;
  AGroup: TcxPivotGridFieldGroup;
begin
  PivotGrid.BeginUpdate;
  try
    AGroup := GetGroup;
    case TButton(Sender).Tag of
      0:
      begin
        AGroup := PivotGrid.Groups.Add;
        AGroup.Caption := Format('FieldGroup %d', [PivotGrid.Groups.Count]);
        tvGroups.Items.AddChildObject(nil, AGroup.Caption, AGroup).Selected := True;
      end;
      1:
      begin
        DeletedGroup := AGroup;
        AGroup.Free;
      end;
      2:
      begin
        AField := TcxPivotGridField(tvGroups.Selected.Data);
        tvGroups.Selected := tvGroups.Selected.GetPrev;
        lbUnlinkedFields.ClearSelection;
        with lbUnlinkedFields do
          Selected[Items.AddObject(AField.Name, AField)] := True;
        AField.Group := nil;
      end;
      3:
        if (lbUnlinkedFields.SelCount > 0) and (AGroup <> nil) then
        begin
          ANode := tvGroups.Selected;
          if (ANode <> nil) and (ANode.Level = 1) then
            ANode := ANode.Parent;
          for I := 0 to lbUnlinkedFields.Items.Count - 1 do
            if lbUnlinkedFields.Selected[I] then
            begin
              AField := TcxPivotGridField(lbUnlinkedFields.Items.Objects[I]);
              AGroup.Add(AField);
              if ANode <> nil then
              begin
                tvGroups.Items.AddChildObject(ANode,
                  AField.Name, AField).Selected := True;
              end;
            end;
        end;
    end;
  finally
    PivotGrid.EndUpdate;
    UpdateDesigner;
    UpdateFieldList;
  end;
  SyncGroupSelection;
end;

procedure TfrmPivotGridDesigner.tvGroupsCustomDrawItem(
  Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState;
  var DefaultDraw: Boolean);
begin
  if Node.Level = 0 then
    Sender.Canvas.Font.Style := Sender.Canvas.Font.Style + [fsBold]
  else
    Sender.Canvas.Font.Style := Sender.Canvas.Font.Style - [fsBold];
  if (Node <> nil) and Node.Selected then
    Sender.Canvas.Brush.Color := clHighlight;
end;

procedure TfrmPivotGridDesigner.tvGroupsClick(Sender: TObject);
begin
  UpdateGroupsButtons;
  SyncGroupSelection;
end;

procedure TfrmPivotGridDesigner.lbUnlinkedFieldsClick(Sender: TObject);
begin
  UpdateGroupsButtons;
  SyncGroupSelection;
end;

procedure TfrmPivotGridDesigner.tvGroupsDragDrop(Sender, Source: TObject;
  X, Y: Integer);
var
  AFields: TList;
  ANode: TTreeNode;
  I, AIndex: Integer;
  AField: TcxPivotGridField;
  AGroup: TcxPivotGridFieldGroup;
begin
  ANode := tvGroups.GetNodeAt(X, Y);
  if ANode = nil then Exit;
  PivotGrid.BeginUpdate;
  try
    AFields := GetFieldsFromDragSource(Source);
    try
      if AFields.Count = 0 then Exit;
      AIndex := ANode.Count;
      if ANode.Level = 1 then
      begin
        AIndex := ANode.Index;
        ANode := ANode.Parent;
      end;
      AGroup := TcxPivotGridFieldGroup(ANode.Data);
      for I := 0 to AFields.Count - 1 do
      begin
        AField := TcxPivotGridField(AFields[I]);
        if TcxDragControlObject(Source).Control <> tvGroups then
          tvGroups.Items.AddChildObject(ANode, AField.Name, AField).Selected := True;
        AGroup.Insert(AIndex + I, AField);
      end;
    finally
      AFields.Free;
    end;
  finally
    PivotGrid.EndUpdate;
    UpdateDesigner;
  end;
end;

procedure TfrmPivotGridDesigner.tvGroupsDragOver(Sender, Source: TObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := (Source is TcxDragControlObject) and (tvGroups.GetNodeAt(X, Y) <> nil);
  if Accept then
  begin
    with TcxDragControlObject(Source) do
      Accept := (Control = lbUnlinkedFields) or (Control = tvGroups);
  end;
end;

procedure TfrmPivotGridDesigner.tvGroupsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) and (tvGroups.GetNodeAt(X, Y) <> nil) and
    (tvGroups.GetNodeAt(X, Y).Level = 1) then
    begin
      tvGroupsClick(nil);
      tvGroups.BeginDrag(False);
    end;
  UpdateGroupsButtons;
end;

procedure TfrmPivotGridDesigner.miMoveInGroupClick(Sender: TObject);

  procedure MoveUp(ANode: TTreeNode);
  var
    AField: TcxPivotGridField;
    AGroup: TcxPivotGridFieldGroup;
  begin
    if ANode.Level = 1 then
    begin
      AGroup := TcxPivotGridFieldGroup(ANode.Parent.Data);
      AField := TcxPivotGridField(ANode.Data);
      if ANode.Index > 0 then
      begin
        AGroup.Remove(AField);
        AGroup.Insert(ANode.Index - 1, AField);
      end
      else
        if AGroup.Index > 0 then
        begin
          AGroup := AGroup.Groups[AGroup.Index - 1];
          AGroup.Add(AField);
        end;
    end
    else
    begin
      AGroup := TcxPivotGridFieldGroup(ANode.Data);
      if AGroup.Index > 0 then
        AGroup.Index := AGroup.Index - 1;
    end;
  end;

  procedure MoveDown(ANode: TTreeNode);
  var
    AField: TcxPivotGridField;
    AGroup: TcxPivotGridFieldGroup;
  begin
    if ANode.Level = 1 then
    begin
      AGroup := TcxPivotGridFieldGroup(ANode.Parent.Data);
      AField := TcxPivotGridField(ANode.Data);
      if ANode.Index < (AGroup.FieldCount - 1) then
        AGroup.Insert(ANode.Index + 1, AField)
      else
        if AGroup.Index < (AGroup.Groups.Count - 1) then
        begin
          AGroup := AGroup.Groups[AGroup.Index + 1];
          AGroup.Insert(0, AField);
        end;
    end
    else
    begin
      AGroup := TcxPivotGridFieldGroup(ANode.Data);
      if AGroup.Index < (AGroup.Groups.Count - 1) then
        AGroup.Index := AGroup.Index + 1;
    end;
  end;

begin
  if tvGroups.Selected = nil then Exit;
  PivotGrid.BeginUpdate;
  try
    case TComponent(Sender).Tag of
      2:
        MoveUp(tvGroups.Selected);
      3:
        MoveDown(tvGroups.Selected);
    end;
  finally
    PivotGrid.EndUpdate;
  end;
  UpdateDesigner;
end;

procedure TfrmPivotGridDesigner.tvGroupsChange(Sender: TObject;
  Node: TTreeNode);
begin
  UpdateGroupsButtons;
end;

procedure TfrmPivotGridDesigner.tvGroupsEdited(Sender: TObject;
  Node: TTreeNode; var S: String);
begin
  TcxPivotGridFieldGroup(Node.Data).Caption := S;
  UpdateDesigner;
end;

procedure TfrmPivotGridDesigner.tvGroupsDblClick(Sender: TObject);
begin
  if (tvGroups.Selected <> nil) and (tvGroups.Selected.Level = 1) then
    btnUnlink.Click;
end;

procedure TfrmPivotGridDesigner.lbUnlinkedFieldsDblClick(Sender: TObject);
begin
  if lbUnlinkedFields.SelCount > 0 then
    btnLink.Click;
end;

procedure TfrmPivotGridDesigner.tvGroupsKeyUp(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  UpdateGroupsButtons;
  SyncGroupSelection;
end;

procedure TfrmPivotGridDesigner.FormCreate(Sender: TObject);
begin
  with Constraints do
  begin
    MinHeight := cxGetValueCurrentDPI(MinHeight);
    MinWidth := cxGetValueCurrentDPI(MinWidth);
  end;
end;

end.
