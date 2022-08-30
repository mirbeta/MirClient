{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           Express Cross Platform Library classes                   }
{                                                                    }
{           Copyright (c) 2000-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSCROSSPLATFORMLIBRARY AND ALL   }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM       }
{   ONLY.                                                            }
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

unit dxGalleryDesigner;

interface

uses
  DesignIntf, cxDesignWindows,
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, ComCtrls, StdCtrls, ToolWin, ImgList, Menus, ActnList, Variants,
  dxCore, cxGraphics, cxClasses, dxComCtrlsUtils, dxMessages, dxGallery;

type
  TfrmGalleryDesigner = class(TcxDesignFormEditor)
    ilButtons: TcxImageList;
    pmGalleryItems: TPopupMenu;
    pmiAddGroupItem: TMenuItem;
    pmiDelete: TMenuItem;
    pmDragDrop: TPopupMenu;
    ppmiCopy: TMenuItem;
    ppmiMove: TMenuItem;
    ilGroupState: TcxImageList;
    alMain: TActionList;
    actAddGroup: TAction;
    actRemove: TAction;
    actAddGroupItem: TAction;
    pmiAddGroup: TMenuItem;
    actMoveNodeUp: TAction;
    actMoveNodeDown: TAction;
    tvGalleryItems: TTreeView;
    ToolBar1: TToolBar;
    tbAddGroup: TToolButton;
    tbAddGroupItem: TToolButton;
    tbDelete: TToolButton;
    tbSeparator: TToolButton;
    tbMoveNodeUp: TToolButton;
    tbMoveNodeDown: TToolButton;
    actClose: TAction;
    ilHelper: TcxImageList;
    actSelectAll: TAction;
    procedure FormShow(Sender: TObject);
    procedure tvGalleryItemsDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure tvGalleryItemsDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure tvGalleryItemsChange(Sender: TObject; Node: TTreeNode);
    procedure tvGalleryItemsEdited(Sender: TObject; Node: TTreeNode; var S: string);
    procedure tvGalleryItemsEditing(Sender: TObject; Node: TTreeNode; var AllowEdit: Boolean);
    procedure ppmiCopyClick(Sender: TObject);
    procedure ppmiMoveClick(Sender: TObject);
    procedure pmDragDropPopup(Sender: TObject);
    procedure pmGalleryItemsPopup(Sender: TObject);
    procedure actAddGroupExecute(Sender: TObject);
    procedure actAddGroupItemExecute(Sender: TObject);
    procedure actCloseExecute(Sender: TObject);
    procedure actMoveNodeUpExecute(Sender: TObject);
    procedure actMoveNodeDownExecute(Sender: TObject);
    procedure actRemoveExecute(Sender: TObject);
    procedure actSelectAllExecute(Sender: TObject);
  private
    FDragDropTargetNode: TTreeNode;
    FGalleryItemsWndProc: TWndMethod;
    FLockSelectionChangedCount: Integer;

    FGallery: IdxGallery;

    function CanModify: Boolean;
    procedure InitializePopupMenuItems;
    procedure UpdatePopupMenuItemsVisibility(ASelectedNode: TTreeNode);
    procedure RefreshImages;

    function CalculateNextNodeIndex(ANode: TTreeNode; AIsMovingUp: Boolean): Integer;
    procedure AddGroupNode;
    procedure AddGroupItemNode(ANode: TTreeNode);
    procedure DeleteNodes(AList: TList);
    procedure MoveGroupNode(ANode: TTreeNode; AIsMovingUp: Boolean);
    procedure MoveNodeDown(ANode: TTreeNode);
    procedure MoveNodeUp(ANode: TTreeNode);
    procedure MoveGroupItemNode(ANode: TTreeNode; AIsMovingUp: Boolean);
    procedure PopulateTreeView;
    procedure RemoveSelectedNodes;
    procedure SelectNextNode(ADeletingNodes: TList);
    procedure SetGroupNodeGlyph(ANode: TTreeNode);
    procedure UpdateGroupNode(ANode: TTreeNode);
    procedure UpdateGroupItemNode(ANode: TTreeNode);
    procedure UpdateNodeText(ANode: TTreeNode);

    // dragdrop
    procedure DoDragDrop(ATargetNode: TTreeNode; AIsCoping: Boolean = False);
    procedure GroupItemNodeDragDrop(ASourceNode: TTreeNode; var ATargetNode: TTreeNode; AIsCoping: Boolean);
    procedure GroupNodeDragDrop(ASourceNode, ATargetNode: TTreeNode);

    function AddGalleryGroup(ANode: TTreeNode): IdxGalleryGroup;
    function AddGalleryGroupItem(ANode: TTreeNode): IdxGalleryItem;
    function GetGalleryGroup(ANode: TTreeNode): IdxGalleryGroup;
    function GetGalleryItem(ANode: TTreeNode): IdxGalleryItem;
    procedure SynchronizeTreeView;
    procedure SetGalleryGroupIndex(ANode: TTreeNode; AIndex: Integer);

    function IsGroup(ANode: TTreeNode): Boolean;
    function IsGroupIndexValid(AIndex: Integer): Boolean;
    function IsGroupItem(ANode: TTreeNode): Boolean;
    function GetObjectCaption(ANode: TTreeNode): string;
    function GetObjectIndex(ANode: TTreeNode): Integer;
    procedure SetObjectCaption(ANode: TTreeNode; var ACaption: string);

    procedure MoveItem(AItem: IdxGalleryItem; ATargetGroup: IdxGalleryGroup; AIndex: Integer);
    function CopyItem(AItem: IdxGalleryItem; ATargetGroup: IdxGalleryGroup; AIndex: Integer): IdxGalleryItem;

    // object inspector
    procedure SetDesignerModified;
    procedure SelectCorrespondingObjects;
    procedure SynchronizeSelection(const ASelection: TDesignerSelectionList);

    // TreeView wnd proc
    procedure GalleryItemsWndProc(var Message: TMessage);
    procedure RestoreTreeViewWndProc;
    procedure StoreTreeViewWndProc;
    function TreeViewWndProcHandler(ATreeView: TTreeView; var Message: TMessage): Boolean;
  protected
    procedure UpdateActions; override;
    procedure UpdateCaption; override;

    procedure SetComponent(AValue: TComponent); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure DoItemDeleted(AItem: TPersistent); override;
    procedure DoItemsModified; override;

    procedure SelectionsChanged(const ASelection: TDesignerSelectionList); override;
  end;

procedure EditGallery(AGalleryOwner: TComponent);

implementation

{$R *.dfm}

uses
  TypInfo, Math, CommCtrl, cxControls;

const
  dxcGroupLevel = 0;
  dxcGroupItemLevel = 1;
  sdxGroupDefaultCaption = 'Group';
  sdxGroupItemDefaultCaption = 'Item';

procedure EditGallery(AGalleryOwner: TComponent);
begin
  ShowFormEditorClass(GetObjectDesigner(AGalleryOwner), AGalleryOwner, TfrmGalleryDesigner);
end;

{ TfrmGalleryDesigner }

constructor TfrmGalleryDesigner.Create(AOwner: TComponent);
begin
  inherited;
  InitializePopupMenuItems;
  StoreTreeViewWndProc;
  AOwner.FreeNotification(Self);
  RefreshImages;
end;

destructor TfrmGalleryDesigner.Destroy;
begin
  RestoreTreeViewWndProc;
  inherited;
end;

procedure TfrmGalleryDesigner.DoItemDeleted(AItem: TPersistent);
var
  I: Integer;
begin
  for I := 0 to tvGalleryItems.Items.Count - 1 do
    if AItem = tvGalleryItems.Items[I].Data then
    begin
      tvGalleryItems.Items.Delete(tvGalleryItems.Items[I]);
      Break;
    end;
end;

procedure TfrmGalleryDesigner.DoItemsModified;
var
  ANode: TTreeNode;
  I: Integer;
begin
  inherited DoItemsModified;
  for I := 0 to tvGalleryItems.SelectionCount - 1 do
  begin
    ANode := tvGalleryItems.Selections[I];
    if ANode <> nil then
      if IsGroup(ANode) then
        UpdateGroupNode(ANode)
      else
        UpdateGroupItemNode(ANode);
  end;
end;

procedure TfrmGalleryDesigner.SelectionsChanged(const ASelection: TDesignerSelectionList);
begin
  SynchronizeSelection(ASelection);
end;

procedure TfrmGalleryDesigner.UpdateActions;
var
  ASelectedNode: TTreeNode;
begin
  inherited UpdateActions;
  ASelectedNode := tvGalleryItems.Selected;
  actAddGroup.Enabled := CanModify;
  actAddGroupItem.Enabled := (FGallery.Groups.Count > 0) and CanModify;
  actRemove.Enabled := (ASelectedNode <> nil) and CanModify;
  actMoveNodeUp.Enabled := (ASelectedNode <> nil) and (IsGroup(ASelectedNode) and
    (ASelectedNode.AbsoluteIndex > 0) or IsGroupItem(ASelectedNode) and
    (ASelectedNode.AbsoluteIndex > 1)) and (tvGalleryItems.SelectionCount <= 1) and CanModify;
  actMoveNodeDown.Enabled := (ASelectedNode <> nil) and
    not ((ASelectedNode.AbsoluteIndex = tvGalleryItems.Items.Count - 1) or IsGroup(ASelectedNode) and
    (ASelectedNode.getNextSibling = nil)) and (tvGalleryItems.SelectionCount <= 1) and CanModify;

  actAddGroup.Caption := 'Add &' + FGallery.Groups.GetDisplayName;
  actAddGroup.Hint := 'Add ' + FGallery.Groups.GetDisplayName;

  actAddGroupItem.Caption := actAddGroup.Hint + ' &Item';
  actAddGroupItem.Hint := actAddGroup.Hint + ' Item';
end;

procedure TfrmGalleryDesigner.UpdateCaption;
begin
  Caption := cxGetFullComponentName(Component) + ' - Designer';
end;

procedure TfrmGalleryDesigner.SetComponent(AValue: TComponent);
var
  AGalleryOwner: IdxGalleryOwner;
begin
  inherited;
  if AValue <> nil then
  begin
    if not Supports(AValue, IdxGallery, FGallery) then
      if Supports(AValue, IdxGalleryOwner, AGalleryOwner) then
        FGallery := AGalleryOwner.GetGallery
      else
        raise EdxException.Create('Gallery Designer fails: ' + AValue.ClassName);
  end
  else
    FGallery := nil;
end;

function TfrmGalleryDesigner.CanModify: Boolean;
begin
  Result := not Designer.IsSourceReadOnly;
end;

procedure TfrmGalleryDesigner.InitializePopupMenuItems;
begin
  pmiAddGroup.Tag := dxcGroupItemLevel;
  pmiAddGroupItem.Tag := dxcGroupItemLevel;
  pmiDelete.Tag := dxcGroupItemLevel;
end;

procedure TfrmGalleryDesigner.UpdatePopupMenuItemsVisibility(ASelectedNode: TTreeNode);
var
  I: Integer;
begin
  if ASelectedNode <> nil then
    for I := 0 to pmGalleryItems.Items.Count - 1 do
      pmGalleryItems.Items[I].Visible := IsGroup(ASelectedNode) or
        IsGroupItem(ASelectedNode) and (pmGalleryItems.Items[I].Tag = dxcGroupItemLevel);
end;

procedure TfrmGalleryDesigner.RefreshImages;
begin
  cxTransformImages(ilButtons, ilHelper, clWindow, False);
end;

function TfrmGalleryDesigner.CalculateNextNodeIndex(ANode: TTreeNode; AIsMovingUp: Boolean): Integer;
begin
  Result := ANode.Index + IfThen(AIsMovingUp, -1, 1);
end;

procedure TfrmGalleryDesigner.AddGroupNode;
var
  AAddedNode: TTreeNode;
begin
  tvGalleryItems.Items.BeginUpdate;
  try
    AAddedNode := tvGalleryItems.Items.Add(nil, FGallery.Groups.GetDisplayName + IntToStr(FGallery.Groups.Count));
    AAddedNode.Data := AddGalleryGroup(AAddedNode).Instance;
    SetGroupNodeGlyph(AAddedNode);
    AAddedNode.Selected := True;
  finally
    tvGalleryItems.Items.EndUpdate;
  end;
  SetDesignerModified;
end;

procedure TfrmGalleryDesigner.AddGroupItemNode(ANode: TTreeNode);

  function GetDefaultItemCaption(const APrefix: string; ANode: TTreeNode): string;
  var
    ACount: Integer;
  begin
    if IsGroup(ANode) then
      ACount := GetGalleryGroup(ANode).GetItems.Count
    else
      ACount := GetGalleryItem(ANode).Items.Count;
    Result := APrefix + IntToStr(ACount);
  end;

var
  AAddedNode: TTreeNode;
  ACaption: string;
begin
  if tvGalleryItems.Items.Count > 0 then
  begin
    if ANode = nil then
      ANode := tvGalleryItems.Items[0];
    tvGalleryItems.Items.BeginUpdate;
    try
      ACaption := GetDefaultItemCaption(sdxGroupItemDefaultCaption, ANode);
      if IsGroup(ANode) then
        AAddedNode := tvGalleryItems.Items.AddChild(ANode, ACaption)
      else
        AAddedNode := tvGalleryItems.Items.Add(ANode, ACaption);
      AAddedNode.Data := AddGalleryGroupItem(AAddedNode).Instance;
      AAddedNode.Parent.Expand(False);
      AAddedNode.Selected := True;
    finally
      tvGalleryItems.Items.EndUpdate;
    end;
    SetDesignerModified;
  end;
end;

procedure TfrmGalleryDesigner.DeleteNodes(AList: TList);
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := 0 to AList.Count - 1 do
      if IsGroupItem(TTreeNode(AList[I])) then
      begin
        TObject(TTreeNode(AList[I]).Data).Free;
        AList[I] := nil;
      end;
    for I := 0 to AList.Count - 1 do
      if IsGroup(TTreeNode(AList[I])) then
        TObject(TTreeNode(AList[I]).Data).Free;
  finally
    EndUpdate;
  end;
end;

procedure TfrmGalleryDesigner.MoveGroupNode(ANode: TTreeNode; AIsMovingUp: Boolean);
var
  AIndex: Integer;
begin
  AIndex := CalculateNextNodeIndex(ANode, AIsMovingUp);
  if IsGroupIndexValid(AIndex) then
  begin
    SetGalleryGroupIndex(ANode, AIndex);
    SetDesignerModified;
  end;
end;

procedure TfrmGalleryDesigner.MoveNodeDown(ANode: TTreeNode);
begin
  if IsGroup(ANode) then
    MoveGroupNode(ANode, False)
  else
    MoveGroupItemNode(ANode, False);
end;

procedure TfrmGalleryDesigner.MoveNodeUp(ANode: TTreeNode);
begin
  if IsGroup(ANode) then
    MoveGroupNode(ANode, True)
  else
    MoveGroupItemNode(ANode, True);
end;

procedure TfrmGalleryDesigner.MoveGroupItemNode(ANode: TTreeNode; AIsMovingUp: Boolean);
type
  TdxGroupChangeType = (gcNone, gcIncreased, gcDecreased);

  function CalculateGroupChangeType(AParentNode: TTreeNode; out ANodeIndex, AGroupIndex: Integer): TdxGroupChangeType;
  begin
    Result := gcNone;
    ANodeIndex := CalculateNextNodeIndex(ANode, AIsMovingUp);
    AGroupIndex := GetGalleryGroup(AParentNode).Index;
    if ANodeIndex < 0 then
    begin
      Dec(AGroupIndex);
      Result := gcDecreased;
      ANodeIndex := -1;
    end
    else
      if ANodeIndex > ANode.Parent.Count - 1 then
      begin
        Inc(AGroupIndex);
        Result := gcIncreased;
        ANodeIndex := 0;
      end;
  end;

  function CreateNode(AParentNode: TTreeNode; ANodeIndex: Integer; AGroup: IdxGalleryGroup;
    AChangeType: TdxGroupChangeType): TTreeNode;
  begin
    if (ANodeIndex = AGroup.GetItems.Count - 1) and not ((AChangeType = gcIncreased) and (AParentNode.Count = 1)) or
      (AParentNode.Count = 0) then
    begin
      Result := tvGalleryItems.Items.AddChild(AParentNode, ANode.Text);
      AParentNode.Expand(False);
    end
    else
      Result := tvGalleryItems.Items.Insert(
        AParentNode.Item[ANodeIndex + IfThen(AIsMovingUp or (AChangeType = gcIncreased), 0, 1)], ANode.Text);
  end;

var
  ANodeIndex, AGroupIndex: Integer;
  AGroup: IdxGalleryGroup;
  AGroupItem: IdxGalleryItem;
  AParentNode, ACreatedNode: TTreeNode;
  AGroupChangeType: TdxGroupChangeType;
begin
  Inc(FLockSelectionChangedCount);
  try
    AParentNode := ANode.Parent;
    AGroupChangeType := CalculateGroupChangeType(AParentNode, ANodeIndex, AGroupIndex);
    if IsGroupIndexValid(AGroupIndex) then
    begin
      case AGroupChangeType of
        gcIncreased: AParentNode := AParentNode.getNextSibling;
        gcDecreased: AParentNode := AParentNode.getPrevSibling;
      end;
      AGroup := FGallery.Groups.GetGroup(AGroupIndex);
      if ANodeIndex = -1 then
        ANodeIndex := AGroup.GetItems.Count - 1;
      tvGalleryItems.Items.BeginUpdate;
      try
        ACreatedNode := CreateNode(AParentNode, ANodeIndex, AGroup, AGroupChangeType);
        AGroupItem := GetGalleryItem(ANode);
        MoveItem(AGroupItem, AGroup, ANodeIndex + IfThen(AGroupChangeType = gcDecreased, 1, 0));
        ACreatedNode.Data := AGroupItem.Instance;
        UpdateNodeText(ACreatedNode);
        tvGalleryItems.Items.Delete(ANode);
        ACreatedNode.Selected := True;
      finally
        tvGalleryItems.Items.EndUpdate;
      end;
    end;
  finally
    Dec(FLockSelectionChangedCount);
  end;
  if AGroupChangeType <> gcNone then
    SetDesignerModified;
end;

procedure TfrmGalleryDesigner.PopulateTreeView;

  function AddNode(AParent: TTreeNode; AObject: TObject): TTreeNode;
  begin
    if AParent = nil then
      Result := tvGalleryItems.Items.AddObject(nil, '', AObject)
    else
      Result := tvGalleryItems.Items.AddChildObject(AParent, '', AObject);
    Result.Text := GetObjectCaption(Result);
  end;

var
  I, J: Integer;
  ANode: TTreeNode;
  AGroup: IdxGalleryGroup;
begin
  tvGalleryItems.Items.Clear;
  for I := 0 to FGallery.Groups.Count - 1 do
  begin
    AGroup := FGallery.Groups.GetGroup(I);
    ANode := AddNode(nil, AGroup.Instance);
    SetGroupNodeGlyph(ANode);
    for J := 0 to AGroup.GetItems.Count - 1 do
      AddNode(ANode, AGroup.GetItems.GetItem(J).Instance);
  end;
end;

procedure TfrmGalleryDesigner.RemoveSelectedNodes;

  procedure UpdateNodesTexts;
  var
    I: Integer;
  begin
    tvGalleryItems.Items.BeginUpdate;
    try
      for I := 0 to tvGalleryItems.Items.Count - 1 do
        UpdateNodeText(tvGalleryItems.Items[I]);
    finally
      tvGalleryItems.Items.EndUpdate;
    end;
  end;

var
  I: Integer;
  AList: TList;
begin
  if tvGalleryItems.SelectionCount > 0 then
  begin
    tvGalleryItems.Items.BeginUpdate;
    AList := TList.Create;
    try
      for I := 0 to tvGalleryItems.SelectionCount - 1 do
        AList.Add(tvGalleryItems.Selections[I]);
      SelectNextNode(AList);
      DeleteNodes(AList);
      UpdateNodesTexts;
    finally
      AList.Free;
      tvGalleryItems.Items.EndUpdate;
    end;
    SetDesignerModified;
  end;
end;

procedure TfrmGalleryDesigner.SelectNextNode(ADeletingNodes: TList);

  function NeedSearchNextNode(ACurrentNode: TTreeNode): Boolean;
  begin
    Result := (ACurrentNode <> nil) and (ADeletingNodes.IndexOf(ACurrentNode) <> -1);
  end;

var
  AFirstNode: TTreeNode;
  ANextNode: TTreeNode;
begin
  if ADeletingNodes.Count < tvGalleryItems.Items.Count then
  begin
    AFirstNode := TObject(ADeletingNodes[0]) as TTreeNode;
    ANextNode := AFirstNode.getPrevSibling;
    while NeedSearchNextNode(ANextNode) do
      ANextNode := ANextNode.getPrevSibling;
    if ANextNode = nil then
    begin
      ANextNode := AFirstNode.getNextSibling;
      while NeedSearchNextNode(ANextNode) do
        ANextNode := ANextNode.getNextSibling;
    end;
    if ANextNode <> nil then
      ANextNode.Selected := True
    else
      if AFirstNode.Parent <> nil then
        AFirstNode.Parent.Selected := True;
  end;
end;

procedure TfrmGalleryDesigner.SetGroupNodeGlyph(ANode: TTreeNode);

  function InternalGetVisible(AGroup: TObject): Boolean;
  begin
    if IsPublishedProp(AGroup, 'Visible') then
      Result := GetPropValue(AGroup, 'Visible')
    else
      Result := True;
  end;

begin
  if InternalGetVisible(GetGalleryGroup(ANode).Instance) then
    ANode.StateIndex := 2
  else
    ANode.StateIndex := 1;
end;

procedure TfrmGalleryDesigner.UpdateGroupNode(ANode: TTreeNode);
begin
  SetGroupNodeGlyph(ANode);
  UpdateNodeText(ANode);
end;

procedure TfrmGalleryDesigner.UpdateGroupItemNode(ANode: TTreeNode);
begin
  UpdateNodeText(ANode);
end;

procedure TfrmGalleryDesigner.UpdateNodeText(ANode: TTreeNode);
begin
  ANode.Text := GetObjectCaption(ANode);
end;

procedure TfrmGalleryDesigner.DoDragDrop(ATargetNode: TTreeNode; AIsCoping: Boolean = False);
var
  ASourceNode: TTreeNode;
  ASourceNodes: TList;
  I: Integer;
begin
  if (ATargetNode <> nil) and not ATargetNode.Selected then
  begin
    ASourceNode := tvGalleryItems.Selected;
    if IsGroupItem(ASourceNode) or (tvGalleryItems.SelectionCount > 1) then
    begin
      ASourceNodes := TList.Create;
      try
        for I := 0 to tvGalleryItems.Items.Count - 1 do
          if tvGalleryItems.Items[I].Selected and IsGroupItem(tvGalleryItems.Items[I]) then
            ASourceNodes.Add(tvGalleryItems.Items[I]);

        for I := 0 to ASourceNodes.Count - 1 do
          GroupItemNodeDragDrop(TTreeNode(ASourceNodes[I]), ATargetNode, AIsCoping);
      finally
        ASourceNodes.Free;
      end;
    end
    else
      GroupNodeDragDrop(ASourceNode, ATargetNode);
  end;
end;

procedure TfrmGalleryDesigner.GroupItemNodeDragDrop(ASourceNode: TTreeNode; var ATargetNode: TTreeNode; AIsCoping: Boolean);

  procedure ChangeGalleryItem(ACreatedNode: TTreeNode; AGroup: IdxGalleryGroup; AIndex: Integer; AIsCoping: Boolean);
  var
    AItem: IdxGalleryItem;
  begin
    AItem := GetGalleryItem(ASourceNode);
    if AIsCoping then
      ACreatedNode.Data := CopyItem(AItem, AGroup, AIndex).Instance
    else
    begin
      MoveItem(AItem, AGroup, AIndex);
      ACreatedNode.Data := AItem.Instance;
    end;
  end;

  function IsDragDropDown: Boolean;
  begin
    Result := IsGroupItem(ATargetNode) and ((ASourceNode.Parent.Index < ATargetNode.Parent.Index) or
      (ASourceNode.Parent.Index = ATargetNode.Parent.Index) and (ASourceNode.Index < ATargetNode.Index));
  end;

var
  ACreatedNode, ASibling: TTreeNode;
  AIndex: Integer;
  AGroup: IdxGalleryGroup;
begin
  tvGalleryItems.Items.BeginUpdate;
  try
    if IsGroupItem(ATargetNode) then
    begin
      AIndex := ATargetNode.Index;
      ASibling := nil;
      if IsDragDropDown then
      begin
        ASibling := ATargetNode.getNextSibling;
        if ASibling <> nil then
        begin
          ATargetNode := ASibling;
          Inc(AIndex);
        end;
      end;
      AGroup := GetGalleryGroup(ATargetNode.Parent);
      if IsDragDropDown and (ASibling = nil) then
      begin
        ATargetNode := ATargetNode.Parent;
        ACreatedNode := tvGalleryItems.Items.AddChild(ATargetNode, ASourceNode.Text);
      end
      else
      begin
        ACreatedNode := tvGalleryItems.Items.Insert(ATargetNode, ASourceNode.Text);
        if ATargetNode = ASibling then
          ATargetNode := ACreatedNode;
      end;
    end
    else
    begin
      ACreatedNode := tvGalleryItems.Items.AddChild(ATargetNode, ASourceNode.Text);
      ATargetNode.Expand(False);
      AIndex := ATargetNode.Count - 1 - IfThen((ASourceNode.Parent = ATargetNode) and not AIsCoping, 1, 0);
      AGroup := GetGalleryGroup(ATargetNode);
    end;
    ChangeGalleryItem(ACreatedNode, AGroup, AIndex, AIsCoping);
    if not AIsCoping then
      tvGalleryItems.Items.Delete(ASourceNode);
  finally
    tvGalleryItems.Items.EndUpdate;
  end;

  SetDesignerModified;
  ACreatedNode.Selected := True;
end;

procedure TfrmGalleryDesigner.GroupNodeDragDrop(ASourceNode, ATargetNode: TTreeNode);
var
  ATargetNodeIndex: Integer;
begin
  ATargetNodeIndex := GetGalleryGroup(ATargetNode).Index;
  SetGalleryGroupIndex(ASourceNode, ATargetNodeIndex);
end;

function TfrmGalleryDesigner.AddGalleryGroup(ANode: TTreeNode): IdxGalleryGroup;
begin
  Result := FGallery.Groups.Add;
  Result.SetCaption(ANode.Text);
end;

function TfrmGalleryDesigner.AddGalleryGroupItem(ANode: TTreeNode): IdxGalleryItem;
begin
  Result := GetGalleryGroup(ANode.Parent).GetItems.Add;
  Result.SetCaption(ANode.Text);
end;

function TfrmGalleryDesigner.GetGalleryGroup(ANode: TTreeNode): IdxGalleryGroup;
begin
  if not Supports(TObject(ANode.Data), IdxGalleryGroup, Result) then
    Result := nil;
end;

function TfrmGalleryDesigner.GetGalleryItem(ANode: TTreeNode): IdxGalleryItem;
begin
  if not Supports(TObject(ANode.Data), IdxGalleryItem, Result) then
    Result := nil;
end;

procedure TfrmGalleryDesigner.SynchronizeTreeView;
begin
  tvGalleryItems.Items.BeginUpdate;
  try
    PopulateTreeView;
    tvGalleryItems.FullExpand;
  finally
    tvGalleryItems.Items.EndUpdate;
  end;
end;

procedure TfrmGalleryDesigner.SynchronizeSelection(const ASelection: TDesignerSelectionList);
var
  AList: TList;
begin
  AList := TList.Create;
  try
    BeginUpdate;
    try
      ConvertSelectionToList(ASelection, AList);
      cxTreeViewSetSelection(tvGalleryItems, AList);
    finally
      EndUpdate(False);
    end;
  finally
    AList.Free;
  end;
end;

procedure TfrmGalleryDesigner.SetGalleryGroupIndex(ANode: TTreeNode; AIndex: Integer);

  procedure SelectGroup(AIndex: Integer);
  var
    I: Integer;
  begin
    ANode := tvGalleryItems.Items[0];
    for I := 1 to AIndex do
      ANode := ANode.GetNextSibling;
    ANode.Selected := True;
  end;

begin
  GetGalleryGroup(ANode).SetIndex(AIndex);

  SynchronizeTreeView;
  SelectGroup(AIndex);
end;

function TfrmGalleryDesigner.IsGroup(ANode: TTreeNode): Boolean;
begin
  Result := (ANode <> nil) and (ANode.Level = dxcGroupLevel);
end;

function TfrmGalleryDesigner.IsGroupIndexValid(AIndex: Integer): Boolean;
begin
  Result := (AIndex >= 0) and (AIndex < FGallery.Groups.Count);
end;

function TfrmGalleryDesigner.IsGroupItem(ANode: TTreeNode): Boolean;
begin
  Result := (ANode <> nil) and (ANode.Level = dxcGroupItemLevel);
end;

function TfrmGalleryDesigner.GetObjectCaption(ANode: TTreeNode): string;
begin
  if IsGroup(ANode) then
    Result := GetGalleryGroup(ANode).GetCaption
  else
    Result := GetGalleryItem(ANode).GetCaption;

  if Result = '' then
    Result := '<Item'+ IntToStr(GetObjectIndex(ANode)) + '>';
end;

function TfrmGalleryDesigner.GetObjectIndex(ANode: TTreeNode): Integer;
begin
  if IsGroup(ANode) then
    Result := GetGalleryGroup(ANode).Index
  else
    Result := GetGalleryItem(ANode).Index;
end;

procedure TfrmGalleryDesigner.SetObjectCaption(ANode: TTreeNode; var ACaption: string);
begin
  if IsGroup(ANode) then
    GetGalleryGroup(ANode).SetCaption(ACaption)
  else
    GetGalleryItem(ANode).SetCaption(ACaption);
end;

procedure TfrmGalleryDesigner.MoveItem(AItem: IdxGalleryItem; ATargetGroup: IdxGalleryGroup; AIndex: Integer);
begin
  AItem.Items := ATargetGroup.GetItems;
  AItem.Index := AIndex;
end;

function TfrmGalleryDesigner.CopyItem(AItem: IdxGalleryItem; ATargetGroup: IdxGalleryGroup; AIndex: Integer): IdxGalleryItem;
begin
  Result := ATargetGroup.GetItems.Insert(AIndex);
  (Result.Instance as TPersistent).Assign(AItem.Instance as TPersistent);
end;

procedure TfrmGalleryDesigner.SetDesignerModified;
begin
  Designer.Modified;
end;

procedure TfrmGalleryDesigner.SelectCorrespondingObjects;
var
  ASelection: TList;
begin
  ASelection := TList.Create;
  try
    if tvGalleryItems.SelectionCount > 0 then
      cxTreeViewGetSelection(tvGalleryItems, ASelection);
    if (ASelection.Count > 0) and (LockCount = 0) then
      SetSelectionList(ASelection);
  finally
    ASelection.Free;
  end;
end;

procedure TfrmGalleryDesigner.GalleryItemsWndProc(var Message: TMessage);
begin
  if not TreeViewWndProcHandler(tvGalleryItems, Message) then
    FGalleryItemsWndProc(Message);
end;

procedure TfrmGalleryDesigner.RestoreTreeViewWndProc;
begin
  if Assigned(FGalleryItemsWndProc) then
    tvGalleryItems.WindowProc := FGalleryItemsWndProc;
  FGalleryItemsWndProc := nil;
end;

procedure TfrmGalleryDesigner.StoreTreeViewWndProc;
begin
  FGalleryItemsWndProc := tvGalleryItems.WindowProc;
  tvGalleryItems.WindowProc := GalleryItemsWndProc;
end;

function TfrmGalleryDesigner.TreeViewWndProcHandler(ATreeView: TTreeView; var Message: TMessage): Boolean;

  procedure ShowContextMenu;
  var
    AHitTest: THitTests;
  begin
    AHitTest := ATreeView.GetHitTestInfoAt(Message.LParamLo, Message.LParamHi);
    if (htOnItem in AHitTest) then
      ATreeView.Perform(WM_CONTEXTMENU, ATreeView.Handle, dxPointToLParam(GetMouseCursorPos));
  end;

var
  ANode: TTreeNode;
  AShift: TShiftState;
begin
  Result := False;
  case Message.Msg of
    CN_NOTIFY:
      case TWMNotify(Message).NMHdr^.code of
        NM_RCLICK:
          begin
            ShowContextMenu;
            Message.Result := 1;
            Result := True;
          end;
      end;
    WM_KEYDOWN:
      if Message.WParam = VK_ESCAPE then
        CancelDrag;
    WM_RBUTTONDOWN:
      begin
        if ATreeView.Selected <> nil then
          ATreeView.Selected.EndEdit(False);
        ANode := ATreeView.GetNodeAt(Message.LParamLo, Message.LParamHi);
        if ANode <> nil then
        begin
          ANode.Focused := True;
          AShift := KeysToShiftState(Message.WParam);
          if not ANode.Selected then
          begin
            if [ssShift, ssCtrl] * AShift <> [] then
              AShift := [];
            BeginUpdate;
            try
              ATreeView.Select(ANode, AShift);
            finally
              CancelUpdate;
            end;
            SelectCorrespondingObjects;
          end;
          Message.Result := 1;
          Result := True;
        end;
      end;
  end;
end;

procedure TfrmGalleryDesigner.FormShow(Sender: TObject);
begin
  SynchronizeTreeView;
  tvGalleryItems.ReadOnly := not CanModify;
end;

procedure TfrmGalleryDesigner.tvGalleryItemsDragOver(Sender,
  Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
var
  ASelectedNode, ATargetNode: TTreeNode;
begin
  ASelectedNode := tvGalleryItems.Selected;
  ATargetNode := tvGalleryItems.GetNodeAt(X, Y);
  Accept := (Sender = tvGalleryItems) and (ASelectedNode <> nil) and (ATargetNode <> nil) and
    (IsGroupItem(ASelectedNode) or IsGroup(ATargetNode)) and CanModify;
end;

procedure TfrmGalleryDesigner.tvGalleryItemsDragDrop(Sender,
  Source: TObject; X, Y: Integer);
begin
  FDragDropTargetNode := tvGalleryItems.GetNodeAt(X, Y);
  DoDragDrop(FDragDropTargetNode, (GetKeyState(VK_CONTROL) and $10000000) <> 0);
end;

procedure TfrmGalleryDesigner.tvGalleryItemsChange(Sender: TObject; Node: TTreeNode);
begin
  SelectCorrespondingObjects;
end;

procedure TfrmGalleryDesigner.tvGalleryItemsEdited(Sender: TObject; Node: TTreeNode; var S: string);
begin
  SetObjectCaption(Node, S);
  S := GetObjectCaption(Node);
  SetDesignerModified;
end;

procedure TfrmGalleryDesigner.tvGalleryItemsEditing(Sender: TObject; Node: TTreeNode; var AllowEdit: Boolean);

  function IsActionAllowed(AAction: TdxGalleryCustomizationAction): Boolean;
  begin
    Result := AAction in FGallery.GetAllowedCustomizationActions;
  end;

var
  AAction: TdxGalleryCustomizationAction;
begin
  if IsGroup(Node) then
    AAction := gcaChangeGroupCaption
  else
    AAction := gcaChangeItemCaption;
  AllowEdit := IsActionAllowed(AAction);
end;

procedure TfrmGalleryDesigner.ppmiCopyClick(Sender: TObject);
begin
  DoDragDrop(FDragDropTargetNode, True);
end;

procedure TfrmGalleryDesigner.ppmiMoveClick(Sender: TObject);
begin
  DoDragDrop(FDragDropTargetNode);
end;

procedure TfrmGalleryDesigner.pmDragDropPopup(Sender: TObject);
begin
  ppmiCopy.Enabled := (tvGalleryItems.Selected <> nil) and IsGroupItem(tvGalleryItems.Selected);
end;

procedure TfrmGalleryDesigner.pmGalleryItemsPopup(Sender: TObject);
begin
  UpdatePopupMenuItemsVisibility(tvGalleryItems.Selected);
end;

procedure TfrmGalleryDesigner.actAddGroupExecute(Sender: TObject);
begin
  AddGroupNode;
end;

procedure TfrmGalleryDesigner.actRemoveExecute(Sender: TObject);
begin
  RemoveSelectedNodes;
end;

procedure TfrmGalleryDesigner.actSelectAllExecute(Sender: TObject);
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := 0 to tvGalleryItems.Items.Count - 1 do
      tvGalleryItems.Items[I].Selected := True;
  finally
    EndUpdate(False);
  end;
end;

procedure TfrmGalleryDesigner.actAddGroupItemExecute(Sender: TObject);
begin
  AddGroupItemNode(tvGalleryItems.Selected);
end;

procedure TfrmGalleryDesigner.actMoveNodeUpExecute(Sender: TObject);
begin
  MoveNodeUp(tvGalleryItems.Selected);
end;

procedure TfrmGalleryDesigner.actMoveNodeDownExecute(Sender: TObject);
begin
  MoveNodeDown(tvGalleryItems.Selected);
end;

procedure TfrmGalleryDesigner.actCloseExecute(Sender: TObject);
begin
  Close;
end;

end.

