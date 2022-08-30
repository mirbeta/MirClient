{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressNavBar                                            }
{                                                                    }
{           Copyright (c) 2002-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSNAVBAR AND ALL ACCOMPANYING    }
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

unit dxNavBarCustomization;

{$I cxVer.inc}

interface

uses
  Types, Windows, SysUtils, Classes, Controls, ActnList, Menus, StdCtrls, Messages,
  ImgList, RTLConsts, Math, CommCtrl, Generics.Defaults, Generics.Collections,
  dxCore, dxNavBar, cxControls, cxListBox, cxGeometry, dxCoreClasses,
  cxGraphics, cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit,
  ComCtrls, cxTreeView, cxGroupBox, cxSplitter, dxNavBarCollns, cxButtons;

type
  TdxNavBarCustomizationForm = class(TdxNavBarCustomCustomizationForm)
    cxGroupBox1: TcxGroupBox;
    gbGroups: TcxGroupBox;
    gbItems: TcxGroupBox;
    cxSplitter1: TcxSplitter;
    tvGroups: TcxTreeView;
    tvItems: TcxTreeView;
    ilItems: TcxImageList;
    ilActions: TcxImageList;
    gbToolBar: TcxGroupBox;
    gbGroupsIndent: TcxGroupBox;
    gbItemsIndent: TcxGroupBox;
    btnExpandAll: TcxButton;
    btnCollapseAll: TcxButton;
    btnAddGroup: TcxButton;
    btnDelete: TcxButton;
    alMain: TActionList;
    acAddGroup: TAction;
    acAddChildGroup: TAction;
    acDeleteGroupOrLink: TAction;
    acExpandAll: TAction;
    acCollapseAll: TAction;
    pmGroupAndLinks: TPopupMenu;
    acExpandAll1: TMenuItem;
    acCollapseAll1: TMenuItem;
    N1: TMenuItem;
    acAddGroup1: TMenuItem;
    AddChildGroup1: TMenuItem;
    acDelete1: TMenuItem;
    cxGroupBox2: TcxGroupBox;
    cxButton3: TcxButton;
    cxButton4: TcxButton;
    acAddItem: TAction;
    acAddSeparator: TAction;
    acDeleteItem: TAction;
    pmItems: TPopupMenu;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    pmAddItems: TPopupMenu;
    acAddItem1: TMenuItem;
    acAddSeparator1: TMenuItem;
    acClose: TAction;
    acRename: TAction;
    cxImageList1: TcxImageList;
    N2: TMenuItem;
    acRename1: TMenuItem;
    N3: TMenuItem;
    acRename2: TMenuItem;
    btnClose: TcxButton;
    pmAddGroups: TPopupMenu;
    acAddGroup2: TMenuItem;
    AddChildGroup2: TMenuItem;
    cxButton1: TcxButton;
    cxButton2: TcxButton;
    acMoveItemUp: TAction;
    acMoveItemDown: TAction;
    acMoveGroupOrLinkUp: TAction;
    acMoveGroupOrLinkDown: TAction;
    cxButton5: TcxButton;
    cxButton6: TcxButton;
    N4: TMenuItem;
    acMoveGroupOrLinkUp1: TMenuItem;
    acMoveGroupDown1: TMenuItem;
    N5: TMenuItem;
    acMoveItemUp1: TMenuItem;
    acMoveItemDown1: TMenuItem;
    cxGroupBox3: TcxGroupBox;
    procedure tvStartDrag(Sender: TObject;
      var DragObject: TDragObject);
    procedure tvEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure tvDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure tvGroupsEditing(Sender: TObject; Node: TTreeNode;
      var AllowEdit: Boolean);
    procedure tvEdited(Sender: TObject; Node: TTreeNode;
      var S: String);
    procedure acExpandAllExecute(Sender: TObject);
    procedure acCollapseAllExecute(Sender: TObject);
    procedure acDeleteGroupOrLinkExecute(Sender: TObject);
    procedure tvGroupsChange(Sender: TObject; Node: TTreeNode);
    procedure acAddGroupExecute(Sender: TObject);
    procedure FormShortCut(var Msg: TWMKey; var Handled: Boolean);
    procedure acAddChildGroupExecute(Sender: TObject);
    procedure tvDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure alMainUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure tvGroupsEnter(Sender: TObject);
    procedure tvItemsEnter(Sender: TObject);
    procedure tvItemsChange(Sender: TObject; Node: TTreeNode);
    procedure acAddItemExecute(Sender: TObject);
    procedure acAddSeparatorExecute(Sender: TObject);
    procedure acDeleteItemExecute(Sender: TObject);
    procedure acCloseExecute(Sender: TObject);
    procedure acRenameExecute(Sender: TObject);
    procedure acMoveItemUpExecute(Sender: TObject);
    procedure acMoveItemDownExecute(Sender: TObject);
    procedure acMoveGroupOrLinkUpExecute(Sender: TObject);
    procedure acMoveGroupOrLinkDownExecute(Sender: TObject);
  private
    FTargetPoint: TPoint;
    FDragObject: TDragControlObject;
    FIsRefreshing: Boolean;
    FIsSelectionChanging: Boolean;
    function AddNode(ATreeView: TcxTreeView; AParent: TTreeNode;
      const AText: string; AData: Pointer): TTreeNode;
    function CanDeleteItems(ATreeView: TcxTreeView): Boolean;
    procedure FreeSelectedItemsData(ATreeView: TcxTreeView);
    function GetItemText(AItem: TdxNavBarCustomItem): string;
    procedure MoveGroupsOrLinks(ADelta: Integer);
    procedure MoveItems(ADelta: Integer);
    procedure PopulateGroups;
    procedure PopulateItems;
    procedure SynchronizeGroupsSelection;
    procedure SynchronizeItemsSelection;
    procedure SynchronizeSelection(ATreeView: TcxTreeView);
    procedure SetItemText(AItem: TdxNavBarCustomItem; const AText: string);
    procedure SetNodeImageIndex(ANode: TTreeNode);
    procedure InternalAddGroup(ATargetGroup: TdxNavBarGroup = nil; APosition: Integer = -1);
    procedure InternalAddItem(AItemClass: TdxNavBarItemClass);
    procedure TreeViewWndProcHandler(ATreeView: TTreeView; var Message: TMessage; var ADone: Boolean);
    function UniqueName(Component: TComponent): string;
    procedure UpdateTargets(X, Y: Integer);
  protected
    procedure BeforeShow; override;
    procedure DesignSelectionChanged(ASelection: TList); override;
    procedure DoCreateControls; override;
    procedure DoRefreshItems; override;
    procedure Loaded; override;
    procedure Localize; override;

    procedure CalculateDropInfo(var ADropTargetInfo: TdxNavBarDropTargetInfo); override;
    function CanProcessDropItem(Target: TObject; X, Y: Integer): Boolean; override;
    procedure DoProcessDropItem(Target: TObject; X, Y: Integer); override;
    function IsDropTargetControl(AControl: TWinControl): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TdxNavBarCustomizationDropInfoCalculator = class(TdxNavBarDropInfoCalculator)
  private
    FForm: TdxNavBarCustomizationForm;
  protected
    procedure CheckIfGroupFreeSpaceAreaTarget(const APoint: TPoint;
      var ADropTargetInfo: TdxNavBarDropTargetInfo); override;
    function GetDropTargetPoint: TPoint; override;
    function IsGroupCaptionTarget(const APoint: TPoint; var AGroup: TdxNavBarGroup;
      var ARect: TRect): Boolean; override;
    function IsLinkTarget(const APoint: TPoint; var ALink: TdxNavBarItemLink;
      var ARect: TRect): Boolean; override;
    function IsTargetVerticalLayout(AGroup: TdxNavBarGroup): Boolean; override;
  public
    constructor Create(ANavBar: TdxCustomNavBar; ACustomizationForm: TdxNavBarCustomizationForm);
  end;

  TdxNavBarCustomizationDragControlObject = class(TdxNavBarDragControlObject)
  private
    FCustomizationForm: TdxNavBarCustomizationForm;
  protected
    function GetNavBar(AControl: TWinControl): TdxCustomNavBar; override;
  public
    constructor Create(ACustomizationForm: TdxNavBarCustomizationForm; AControl: TControl); reintroduce; virtual;
  end;

implementation

{$R *.dfm}

uses
  Forms, Graphics, cxClasses, dxComCtrlsUtils, dxNavBarGroupItems, dxNavBarConsts;

type
  TdxNavBarCustomItemAccess = class(TdxNavBarCustomItem);
  TdxNavBarAccess = class(TdxNavBar);

{ TdxNavBarCustomizationForm }

constructor TdxNavBarCustomizationForm.Create(AOwner: TComponent);
begin
  inherited;
  tvGroups.OnInnerTreeViewWndProc := TreeViewWndProcHandler;
  tvItems.OnInnerTreeViewWndProc := TreeViewWndProcHandler;
end;

procedure TdxNavBarCustomizationForm.BeforeShow;
begin
  tvGroups.FullExpand;
end;

procedure TdxNavBarCustomizationForm.DesignSelectionChanged(ASelection: TList);
begin
  if FIsSelectionChanging then
    Exit;
  FIsSelectionChanging := True;
  try
    cxTreeViewSetSelection(tvGroups.InnerTreeView, ASelection);
    cxTreeViewSetSelection(tvItems.InnerTreeView, ASelection);
  finally
    FIsSelectionChanging := False;
  end;
end;

procedure TdxNavBarCustomizationForm.DoCreateControls;
begin
  inherited;
  if not IsXPManifestEnabled then
    cxTransformImages(ilItems, clWindow);
end;

procedure TdxNavBarCustomizationForm.DoRefreshItems;
var
  AList: TList;
begin
  inherited;
  FIsRefreshing := True;
  try
    AList := TList.Create;
    try
      cxTreeViewGetSelection(tvGroups.InnerTreeView, AList);
      cxTreeViewGetSelection(tvItems.InnerTreeView, AList);
      PopulateGroups;
      PopulateItems;
      cxTreeViewSetSelection(tvItems.InnerTreeView, AList);
      cxTreeViewSetSelection(tvGroups.InnerTreeView, AList);
    finally
      AList.Free;
    end;
  finally
    FIsRefreshing := False;
  end;
end;

procedure TdxNavBarCustomizationForm.Loaded;
begin
  inherited Loaded;
  Constraints.MinHeight := Height div 2;
  Constraints.MinWidth := Width;
end;

procedure TdxNavBarCustomizationForm.Localize;
begin
  if NavBar.IsDesigning then
    Caption := Format(cxGetResourceString(@sdxNavBarItemsDesignerCaptionFormat),
      [cxGetFullComponentName(NavBar)])
  else
    Caption := cxGetResourceString(@sdxNavBarCustomizationCaption);
  gbGroups.Caption := cxGetResourceString(@sdxNavBarNewGroupsCaption);
  gbItems.Caption := cxGetResourceString(@sdxNavBarNewItemsCaption);
  acAddGroup.Caption := cxGetResourceString(@sdxNavBarAddGroup);
  acAddGroup.Hint := acAddGroup.Caption;
  acAddChildGroup.Caption := cxGetResourceString(@sdxNavBarAddChildGroup);
  acAddChildGroup.Hint := acAddChildGroup.Caption;
  acAddItem.Caption := cxGetResourceString(@sdxNavBarAddItem);
  acAddItem.Hint := acAddItem.Caption;
  acAddSeparator.Caption := cxGetResourceString(@sdxNavBarAddSeparator);
  acAddSeparator.Hint := acAddSeparator.Caption;
  acDeleteGroupOrLink.Caption := cxGetResourceString(@sdxNavBarDelete);
  acDeleteGroupOrLink.Hint := acDeleteGroupOrLink.Caption;
  acDeleteItem.Caption := cxGetResourceString(@sdxNavBarDelete);
  acDeleteItem.Hint := acDeleteItem.Caption;
  acExpandAll.Caption := cxGetResourceString(@sdxNavBarExpandAll);
  acExpandAll.Hint := acExpandAll.Caption;
  acCollapseAll.Caption := cxGetResourceString(@sdxNavBarCollapseAll);
  acCollapseAll.Hint := acCollapseAll.Caption;
  acMoveItemUp.Caption := cxGetResourceString(@sdxNavBarMoveUp);
  acMoveItemDown.Caption := cxGetResourceString(@sdxNavBarMoveDown);
  acMoveGroupOrLinkUp.Caption := acMoveItemUp.Caption;
  acMoveGroupOrLinkDown.Caption := acMoveItemDown.Caption;
  acMoveItemUp.Hint := acMoveItemUp.Caption;
  acMoveItemDown.Hint := acMoveItemDown.Caption;
  acMoveGroupOrLinkUp.Hint := acMoveGroupOrLinkUp.Caption;
  acMoveGroupOrLinkDown.Hint := acMoveGroupOrLinkDown.Caption;
  btnClose.Caption := cxGetResourceString(@sdxNavBarClose);
end;

function TdxNavBarCustomizationForm.CanProcessDropItem(Target: TObject; X, Y: Integer): Boolean;
begin
  Result := True;
end;

procedure TdxNavBarCustomizationForm.DoProcessDropItem(Target: TObject; X, Y: Integer);
begin
  inherited;
  BeginUpdate;
  try
    if Target = tvItems then
    begin
      if dxNavBarDragObject.SourceLink <> nil then
        dxNavBarDragObject.SourceLink.Group.RemoveLink(dxNavBarDragObject.SourceLink.Index);
    end
    else
      NavBar.DoDragDrop(False);
  finally
    EndUpdate;
  end;
end;

function TdxNavBarCustomizationForm.IsDropTargetControl(AControl: TWinControl): Boolean;
begin
  Result := (AControl = tvGroups.InnerTreeView);
end;

procedure TdxNavBarCustomizationForm.CalculateDropInfo(var ADropTargetInfo: TdxNavBarDropTargetInfo);
begin
  with TdxNavBarCustomizationDropInfoCalculator.Create(NavBar, Self) do
  begin
    Calculate(ADropTargetInfo);
    Free;
  end;
end;

function TdxNavBarCustomizationForm.AddNode(ATreeView: TcxTreeView;
  AParent: TTreeNode; const AText: string; AData: Pointer): TTreeNode;
begin
  Result := ATreeView.Items.AddChildObject(AParent, AText, AData);
  SetNodeImageIndex(Result);
end;

function TdxNavBarCustomizationForm.CanDeleteItems(
  ATreeView: TcxTreeView): Boolean;
var
  AList: TList;
  I: Integer;
  AObject: TObject;
begin
  Result := not ATreeView.IsEditing and ATreeView.Focused;
  if Result then
  begin
    AList := TList.Create;
    try
      cxTreeViewGetSelection(ATreeView.InnerTreeView, AList);
      Result := AList.Count > 0;
      if Result and (TdxNavBarAccess(NavBar).FDesignHelper <> nil) then
        for I := 0 to AList.Count - 1 do
        begin
          AObject := TObject(AList[I]);
          Result := not (AObject is TComponent) or
            TdxNavBarAccess(NavBar).FDesignHelper.CanDeleteComponent(NavBar, TComponent(AObject));
          if not Result then
            Break;
        end;
    finally
      AList.Free;
    end;
  end;
end;

procedure TdxNavBarCustomizationForm.FreeSelectedItemsData(ATreeView: TcxTreeView);

  procedure DestroyNodeData(ANode: TTreeNode);
  var
    AItem: TObject;
  begin
    AItem := TObject(ANode.Data);
    AItem.Free;
    ANode.Data := nil;
  end;

  procedure ResetChildData(ANode: TTreeNode);
  var
    AChildNode: TTreeNode;
  begin
    AChildNode := ANode.GetFirstChild;
    while AChildNode <> nil do
    begin
      if AChildNode.Data <> nil then
      begin
        AChildNode.Data := nil;
        ResetChildData(AChildNode);
      end;
      AChildNode := AChildNode.getNextSibling;
    end;
  end;

  procedure ChangeSelection(ATreeView: TcxTreeView);
  var
    ANode: TTreeNode;
  begin
    ANode := ATreeView.FindNextToSelect;
    if ANode <> nil then
      ANode.Selected := True;
  end;

  procedure DestroyNodeAndResetChildData(ANode: TTreeNode);
  begin
    if ANode.Data <> nil then
    begin
      ResetChildData(ANode);
      DestroyNodeData(ANode);
    end;
  end;

var
  AList: TList;
  ASelectedNode: TTreeNode;
  I: Integer;
begin
  AList := TList.Create;
  try
    ASelectedNode := ATreeView.GetSelections(AList);
    if ASelectedNode <> nil then
    begin
      BeginUpdate;
      try
        ChangeSelection(ATreeView);
        if AList.Count > 0 then
          for I := 0 to AList.Count - 1 do
          begin
            ASelectedNode := TObject(AList[I]) as TTreeNode;
            DestroyNodeAndResetChildData(ASelectedNode);
          end
        else
          DestroyNodeAndResetChildData(ASelectedNode);
      finally
        EndUpdate;
      end;
    end;
  finally
    AList.Free;
  end;
end;

function TdxNavBarCustomizationForm.GetItemText(AItem: TdxNavBarCustomItem): string;
begin
  if NavBar.IsDesigning then
    Result := AItem.Name
  else
    Result := TdxNavBarCustomItemAccess(AItem).Caption;
end;

type
  TdxNavBarGroupChildPosition = record
    Parent: TdxNavBarGroup;
    Position: Integer;
  end;

procedure TdxNavBarCustomizationForm.MoveGroupsOrLinks(ADelta: Integer);

  function GetSiblingCount(AGroup: TdxNavBarGroup): Integer;
  begin
    if AGroup.Parent <> nil then
      Result := AGroup.Parent.ChildCount - 1
    else
      Result := NavBar.RootGroupCount - 1;
  end;

  procedure MoveItem(ANode: TTreeNode; ADelta: Integer;
    AList: TList<TdxNavBarGroupChildPosition>; var AChanged: Boolean);
  var
    APosition: Integer;
    AGroup: TdxNavBarGroup;
    ALink: TdxNavBarItemLink;
    APos: TdxNavBarGroupChildPosition;
  begin
    if ANode.Selected then
    begin
      if TObject(ANode.Data) is TdxNavBarGroup  then
      begin
        AGroup := TObject(ANode.Data) as TdxNavBarGroup;
        ALink := nil;
      end
      else
      begin
        AGroup := nil;
        ALink := TObject(ANode.Data) as TdxNavBarItemLink;
      end;
      if AGroup <> nil then
      begin
        APosition := AGroup.Position + ADelta;
        APos.Parent := AGroup.Parent;
        APos.Position := APosition;
        if InRange(APosition, 0, GetSiblingCount(AGroup)) and
          (AList.IndexOf(APos) = -1) then
        begin
          AGroup.Position := APosition;
          AChanged := True;
        end;
        APos.Position := AGroup.Position;
        AList.Add(APos);
      end
      else
      begin
        APosition := ALink.Position + ADelta;
        APos.Parent := ALink.Group;
        APos.Position := APosition;
        if InRange(APosition, 0, ALink.Group.ChildCount - 1) and (AList.IndexOf(APos) = -1) then
        begin
          ALink.Position := APosition;
          AChanged := True;
        end;
        APos.Position := ALink.Position;
        AList.Add(APos);
      end;
    end;
  end;

  procedure MoveSiblings(ANodes: TTreeNodes; ADelta: Integer; var AChanged: Boolean);
  var
    AList: TList<TdxNavBarGroupChildPosition>;
    I: Integer;
  begin
    AList := TList<TdxNavBarGroupChildPosition>.Create;
    try
      if ADelta < 0 then
        for I := 0 to ANodes.Count - 1 do
          MoveItem(ANodes[I], ADelta, AList, AChanged)
      else
        if ADelta > 0 then
          for I := ANodes.Count - 1 downto 0 do
            MoveItem(ANodes[I], ADelta, AList, AChanged);
    finally
      AList.Free;
    end;
  end;

var
  AChanged: Boolean;
begin
  AChanged := False;
  BeginUpdate;
  try
    MoveSiblings(tvGroups.Items, ADelta, AChanged);
  finally
    if AChanged then
    begin
      EndUpdate;
      NavBar.DesignerModified;
    end
    else
      CancelUpdate;
  end;
end;

procedure TdxNavBarCustomizationForm.MoveItems(ADelta: Integer);

  procedure MoveItem(ANode: TTreeNode; ADelta: Integer; AList: TList<Integer>; var AChanged: Boolean);
  var
    AIndex: Integer;
    AItem: TdxNavBarItem;
  begin
    if ANode.Selected then
    begin
      AItem := TObject(ANode.Data) as TdxNavBarItem;
      AIndex := AItem.Index + ADelta;
      if InRange(AIndex, 0, NavBar.Items.Count - 1) and (AList.IndexOf(AIndex) = -1) then
      begin
        AItem.Index := AIndex;
        AChanged := True;
      end;
      AList.Add(AItem.Index);
    end;
  end;

var
  AList: TList<Integer>;
  I: Integer;
  AChanged: Boolean;
begin
  AList := TList<Integer>.Create;
  try
    AChanged := False;
    BeginUpdate;
    try
      if ADelta < 0 then
      begin
        for I := 0 to tvItems.Items.Count - 1 do
          MoveItem(tvItems.Items[I], ADelta, AList, AChanged)
      end
      else
        if ADelta > 0 then
          for I := tvItems.Items.Count - 1 downto 0 do
            MoveItem(tvItems.Items[I], ADelta, AList, AChanged);
    finally
      if AChanged then
      begin
        EndUpdate;
        NavBar.DesignerModified;
      end
      else
        CancelUpdate;
    end;
  finally
    AList.Free;
  end;
end;

procedure TdxNavBarCustomizationForm.PopulateGroups;

  procedure AddGroup(AGroup: TdxNavBarGroup; AParentNode: TTreeNode; AExpandedList: TList);
  var
    I: Integer;
    ANode: TTreeNode;
    ALink: TdxNavBarItemLink;
  begin
    ANode := AddNode(tvGroups, AParentNode, GetItemText(AGroup), AGroup);
    for I := 0 to AGroup.ChildCount - 1 do
    begin
      if AGroup.Children[I] is TdxNavBarItemLink then
      begin
        ALink := TdxNavBarItemLink(AGroup.Children[I]);
        if ALink.Item <> nil then
          AddNode(tvGroups, ANode, GetItemText(ALink.Item), ALink);
      end
      else
        AddGroup(AGroup.Children[I] as TdxNavBarGroup, ANode, AExpandedList);
    end;
    if AExpandedList.IndexOf(AGroup) <> -1 then
      ANode.Expand(False);
  end;

var
  I: Integer;
  AExpandedList: TList;
begin
  AExpandedList := TList.Create;
  try
    for I := 0 to tvGroups.Items.Count - 1 do
      if tvGroups.Items[I].Expanded then
        AExpandedList.Add(tvGroups.Items[I].Data);
    tvGroups.Items.BeginUpdate;
    try
      tvGroups.Items.Clear;
      for I := 0 to NavBar.RootGroupCount - 1 do
        AddGroup(NavBar.RootGroups[I], nil, AExpandedList);
    finally
      tvGroups.Items.EndUpdate;
    end;
  finally
    AExpandedList.Free;
  end;
end;

procedure TdxNavBarCustomizationForm.PopulateItems;
var
  I: Integer;
begin
  tvItems.Items.BeginUpdate;
  try
    tvItems.Items.Clear;
    for I := 0 to NavBar.Items.Count - 1 do
      AddNode(tvItems, nil, GetItemText(NavBar.Items[I]), NavBar.Items[I]);
  finally
    tvItems.Items.EndUpdate;
  end;
end;

procedure TdxNavBarCustomizationForm.SynchronizeGroupsSelection;
begin
  SynchronizeSelection(tvGroups);
end;

procedure TdxNavBarCustomizationForm.SynchronizeItemsSelection;
begin
  SynchronizeSelection(tvItems);
end;

procedure TdxNavBarCustomizationForm.SynchronizeSelection(ATreeView: TcxTreeView);
var
  AList: TList;
  I: Integer;
  AClearSelection: Boolean;
  ASelectedNode: TTreeNode;
  ASelectedObject: TObject;
begin
  if not FIsRefreshing and not FIsSelectionChanging and (TdxNavBarAccess(NavBar).FDesignHelper <> nil) then
  begin
    FIsSelectionChanging := True;
    try
      AList := TList.Create;
      try
        ASelectedNode := ATreeView.GetSelections(AList);
        if ASelectedNode <> nil then
        begin
          if AList.Count > 0 then
          begin
            AClearSelection := True;
            for I := 0 to AList.Count - 1 do
            begin
              ASelectedNode := TObject(AList[I]) as TTreeNode;
              ASelectedObject := TObject(ASelectedNode.Data);
              if ASelectedObject is TdxNavBarItemLink then
                ASelectedObject := TdxNavBarItemLink(ASelectedObject).Item;

              TdxNavBarAccess(NavBar).FDesignHelper.SelectObject(NavBar,
                ASelectedObject as TPersistent, AClearSelection, False);
              AClearSelection := False;
            end;
          end
          else
            NavBar.DesignerSelect(TObject(ASelectedNode.Data) as TPersistent);
        end
        else
          NavBar.DesignerSelect(NavBar);
      finally
        AList.Free;
      end;
    finally
      FIsSelectionChanging := False;
    end;
  end;
end;

procedure TdxNavBarCustomizationForm.SetItemText(AItem: TdxNavBarCustomItem; const AText: string);
begin
  if NavBar.IsDesigning then
    AItem.Name := AText
  else
    TdxNavBarCustomItemAccess(AItem).Caption := AText;
end;

procedure TdxNavBarCustomizationForm.SetNodeImageIndex(
  ANode: TTreeNode);
var
  AItem: TObject;
begin
  AItem := TObject(ANode.Data);
  if AItem is TdxNavBarGroup then
    ANode.ImageIndex := 0
  else
    if AItem is TdxNavBarItemLink then
      ANode.ImageIndex := 3
    else
      if AItem is TdxNavBarItem then
        if AItem is TdxNavBarSeparator then
          ANode.ImageIndex := 2
        else
          ANode.ImageIndex := 1;
  ANode.SelectedIndex := ANode.ImageIndex;
end;

procedure TdxNavBarCustomizationForm.InternalAddGroup(
  ATargetGroup: TdxNavBarGroup = nil; APosition: Integer = -1);
var
  ANewGroup: TdxNavBarGroup;
  ANode: TTreeNode;
begin
  tvGroups.SetFocus;
  BeginUpdate;
  try
    ANewGroup := NavBar.Groups.Add;
    if NavBar.IsDesigning then
      ANewGroup.Name := UniqueName(ANewGroup)
    else
      ANewGroup.Caption := cxGetResourceString(@sdxNavBarNewGroupCaption);
    if APosition <> -1 then
      ANewGroup.MoveTo(ATargetGroup, APosition);
  finally
    EndUpdate;
  end;
  dxTestCheck(cxTreeViewFindNodeByData(tvGroups.InnerTreeView, ANewGroup, ANode),
    'InternalAddGroup: not find new group node');
  ANode.Selected := True;
  NavBar.DesignerModified;
end;

procedure TdxNavBarCustomizationForm.InternalAddItem(AItemClass: TdxNavBarItemClass);
var
  ANewItem: TcxComponentCollectionItem;
  ANode: TTreeNode;
begin
  tvItems.SetFocus;
  BeginUpdate;
  try
    ANewItem := NavBar.Items.Add(AItemClass);
    if NavBar.IsDesigning then
      ANewItem.Name := UniqueName(ANewItem);
  finally
    EndUpdate;
  end;
  dxTestCheck(cxTreeViewFindNodeByData(tvItems.InnerTreeView, ANewItem, ANode),
    'InternalAddItem: not find new item node');
  ANode.Selected := True;
  NavBar.DesignerModified;
end;

procedure TdxNavBarCustomizationForm.TreeViewWndProcHandler(ATreeView: TTreeView;
  var Message: TMessage; var ADone: Boolean);
begin
  if Message.Msg <> WM_LBUTTONDOWN then
    ADone := CustomizationTreeViewWndProcHandler(ATreeView, Message)
  else
    ADone := False;
end;

function TdxNavBarCustomizationForm.UniqueName(Component: TComponent): string;
const
  sdxNavBarClassesPrefix = 'dxNavBar';
var
  AItemNamePrefix: string;
  AIndex: Integer;
begin
  AIndex := Pos(sdxNavBarClassesPrefix, Component.ClassName);
  AItemNamePrefix := 'Unknown';
  dxTestCheck(AIndex > 0, 'UniqueName fail:' + Component.ClassName);
  if AIndex > 0 then
    AItemNamePrefix := System.Copy(Component.ClassName, AIndex + Length(sdxNavBarClassesPrefix), MaxInt);
  Result := TdxNavBarAccess(NavBar).FDesignHelper.UniqueName(NavBar, NavBar.Name + AItemNamePrefix);
end;

procedure TdxNavBarCustomizationForm.UpdateTargets(X, Y: Integer);
begin
  FTargetPoint := Point(X, Y);
  NavBar.UpdateTargets(Self);
end;

procedure TdxNavBarCustomizationForm.tvStartDrag(Sender: TObject;
  var DragObject: TDragObject);
var
  ATreeView: TcxTreeView;
  ASelected: TObject;
begin
  ATreeView := Sender as TcxTreeView;
  FDragObject := TdxNavBarCustomizationDragControlObject.Create(Self, ATreeView);
  DragObject := FDragObject;
  if (ATreeView.Selected <> nil) then
  begin
    ASelected := TObject(ATreeView.Selected.Data);
    if ASelected is TdxNavBarItem then
      dxNavBarDragObject := TdxNavBarDragObject.Create(NavBar, DragObject, nil, nil,
        TdxNavBarItem(ASelected))
    else
      if ASelected is TdxNavBarGroup then
        dxNavBarDragObject := TdxNavBarDragObject.Create(NavBar, DragObject,
          TdxNavBarGroup(ASelected), nil, nil)
      else
        dxNavBarDragObject := TdxNavBarDragObject.Create(NavBar, DragObject, nil,
          TdxNavBarItemLink(ASelected), nil);
  end;
end;

procedure TdxNavBarCustomizationForm.tvEndDrag(Sender,
  Target: TObject; X, Y: Integer);
begin
  FreeAndNil(dxNavBarDragObject);
  FreeAndNil(FDragObject);
end;

procedure TdxNavBarCustomizationForm.tvDragOver(Sender,
  Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := (dxNavBarDragObject <> nil);
  if Accept then
  begin
    if Sender = tvItems then
      Accept := (dxNavBarDragObject.SourceGroup = nil) and (dxNavBarDragObject.SourceItem = nil)
    else
    begin
      UpdateTargets(X, Y);
      Accept := NavBar.IsTargetValid;
    end;
  end;
  if (Sender = tvGroups) and (State = dsDragLeave) and (WindowFromPoint(GetMouseCursorPos) <> (Sender as TcxCustomTreeView).InnerTreeView.Handle) then
    UpdateTargets(-1, -1);
end;

procedure TdxNavBarCustomizationForm.tvGroupsEditing(Sender: TObject;
  Node: TTreeNode; var AllowEdit: Boolean);
begin
  AllowEdit := not (TObject(Node.Data) is TdxNavBarItemLink);
end;

procedure TdxNavBarCustomizationForm.tvGroupsEnter(Sender: TObject);
begin
  SynchronizeGroupsSelection;
end;

procedure TdxNavBarCustomizationForm.tvItemsChange(Sender: TObject;
  Node: TTreeNode);
begin
  SynchronizeItemsSelection;
end;

procedure TdxNavBarCustomizationForm.tvItemsEnter(Sender: TObject);
begin
  SynchronizeItemsSelection;
end;

procedure TdxNavBarCustomizationForm.tvEdited(Sender: TObject;
  Node: TTreeNode; var S: String);
begin
  if TdxNavBarCustomItemAccess(Node.Data).Caption <> S then
  begin
    BeginUpdate;
    try
      SetItemText(TObject(Node.Data) as TdxNavBarCustomItem, S);
    finally
      CancelUpdate;
    end;
    if Sender = tvItems.InnerTreeView then
      PopulateGroups;
    NavBar.DesignerModified;
  end;
end;

procedure TdxNavBarCustomizationForm.acExpandAllExecute(Sender: TObject);
begin
  tvGroups.FullExpand;
end;

procedure TdxNavBarCustomizationForm.acMoveGroupOrLinkDownExecute(
  Sender: TObject);
begin
  MoveGroupsOrLinks(1);
end;

procedure TdxNavBarCustomizationForm.acMoveGroupOrLinkUpExecute(
  Sender: TObject);
begin
  MoveGroupsOrLinks(-1);
end;

procedure TdxNavBarCustomizationForm.acMoveItemDownExecute(Sender: TObject);
begin
  MoveItems(1);
end;

procedure TdxNavBarCustomizationForm.acMoveItemUpExecute(Sender: TObject);
begin
  MoveItems(-1);
end;

procedure TdxNavBarCustomizationForm.acRenameExecute(Sender: TObject);
begin
  if tvGroups.Focused then
    tvGroups.Selected.EditText
  else
    if tvItems.Focused then
      tvItems.Selected.EditText;
end;

procedure TdxNavBarCustomizationForm.alMainUpdate(Action: TBasicAction;
  var Handled: Boolean);
begin
  acAddChildGroup.Enabled := (tvGroups.Selected <> nil) and
    (TObject(tvGroups.Selected.Data) is TdxNavBarGroup) and NavBar.OptionsBehavior.Common.AllowChildGroups;
  acDeleteGroupOrLink.Enabled := CanDeleteItems(tvGroups);
  acDeleteItem.Enabled := CanDeleteItems(tvItems);
  acRename.Enabled := tvGroups.Focused and (tvGroups.Selected <> nil) and (TObject(tvGroups.Selected.Data) is TdxNavBarGroup) or
    tvItems.Focused and (tvItems.Selected <> nil);
  acMoveItemUp.Enabled := tvItems.Focused and (tvItems.Selected <> nil);
  acMoveItemDown.Enabled := tvItems.Focused and (tvItems.Selected <> nil);
  acMoveGroupOrLinkUp.Enabled := tvGroups.Focused and (tvGroups.Selected <> nil);
  acMoveGroupOrLinkDown.Enabled := tvGroups.Focused and (tvGroups.Selected <> nil);
end;

procedure TdxNavBarCustomizationForm.acCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TdxNavBarCustomizationForm.acCollapseAllExecute(Sender: TObject);
begin
  tvGroups.FullCollapse;
end;

procedure TdxNavBarCustomizationForm.acDeleteGroupOrLinkExecute(Sender: TObject);
begin
  FreeSelectedItemsData(tvGroups);
end;

procedure TdxNavBarCustomizationForm.acDeleteItemExecute(Sender: TObject);
begin
  FreeSelectedItemsData(tvItems);
end;

procedure TdxNavBarCustomizationForm.tvGroupsChange(Sender: TObject;
  Node: TTreeNode);
begin
  SynchronizeGroupsSelection;
end;

procedure TdxNavBarCustomizationForm.tvDragDrop(Sender, Source: TObject;
  X, Y: Integer);
begin
  ProcessDropItem(Sender, X, Y);
end;

procedure TdxNavBarCustomizationForm.acAddChildGroupExecute(
  Sender: TObject);
var
  ATargetGroup: TdxNavBarGroup;
  APosition: Integer;
begin
  ATargetGroup := nil;
  APosition := -1;
  if tvGroups.Selected <> nil then
  begin
    if TObject(tvGroups.Selected.Data) is TdxNavBarGroup then
      ATargetGroup := TdxNavBarGroup(tvGroups.Selected.Data)
    else
      ATargetGroup := TdxNavBarItemLink(tvGroups.Selected.Data).Group;
    APosition := ATargetGroup.ChildCount;
  end;
  InternalAddGroup(ATargetGroup, APosition);
end;

procedure TdxNavBarCustomizationForm.acAddGroupExecute(Sender: TObject);
begin
  InternalAddGroup;
end;

procedure TdxNavBarCustomizationForm.acAddItemExecute(Sender: TObject);
begin
  InternalAddItem(TdxNavBarItem);
end;

procedure TdxNavBarCustomizationForm.acAddSeparatorExecute(Sender: TObject);
begin
  InternalAddItem(TdxNavBarSeparator);
end;

procedure TdxNavBarCustomizationForm.FormShortCut(var Msg: TWMKey;
  var Handled: Boolean);
begin
  Handled := (Msg.CharCode in [VK_DELETE, VK_INSERT, VK_ESCAPE]) or
    (Msg.CharCode in [VK_UP, VK_DOWN]) and (GetKeyState(VK_CONTROL) < 0);
  if Handled then
    case Msg.CharCode of
      VK_DELETE:
        begin
          if tvGroups.Focused and not tvGroups.IsEditing then
            acDeleteGroupOrLink.Execute
          else
            if tvItems.Focused and not tvItems.IsEditing then
              acDeleteItem.Execute;
        end;
      VK_INSERT:
        begin
          if tvGroups.Focused and not tvGroups.IsEditing then
            acAddGroup.Execute
          else
            if tvItems.Focused and not tvItems.IsEditing then
              acAddItem.Execute;
        end;
      VK_ESCAPE:
        begin
          Handled := tvGroups.IsEditing or tvItems.IsEditing;
          if tvGroups.IsEditing and (tvGroups.Selected <> nil) then
            tvGroups.Selected.EndEdit(True);
          if tvItems.IsEditing and (tvItems.Selected <> nil) then
            tvItems.Selected.EndEdit(True);
        end;
      VK_UP:
        begin
          acMoveItemUp.Execute;
          acMoveGroupOrLinkUp.Execute;
        end;
      VK_DOWN:
        begin
          acMoveItemDown.Execute;
          acMoveGroupOrLinkDown.Execute;
        end;
    end;
end;

{ TdxNavBarCustomizationDropInfoCalculator }

constructor TdxNavBarCustomizationDropInfoCalculator.Create(
  ANavBar: TdxCustomNavBar; ACustomizationForm: TdxNavBarCustomizationForm);
begin
  inherited Create(ANavBar);
  FForm := ACustomizationForm;
end;

procedure TdxNavBarCustomizationDropInfoCalculator.CheckIfGroupFreeSpaceAreaTarget(
  const APoint: TPoint; var ADropTargetInfo: TdxNavBarDropTargetInfo);
begin
  // do nothing
end;

function TdxNavBarCustomizationDropInfoCalculator.GetDropTargetPoint: TPoint;
begin
  Result := FForm.FTargetPoint;
end;

function TdxNavBarCustomizationDropInfoCalculator.IsGroupCaptionTarget(
  const APoint: TPoint; var AGroup: TdxNavBarGroup; var ARect: TRect): Boolean;

  function GetGroup(ANode: TTreeNode): TdxNavBarGroup;
  begin
    if TObject(ANode.Data) is TdxNavBarGroup then
      Result := TdxNavBarGroup(ANode.Data)
    else
      Result := nil;
  end;

var
  ANode: TTreeNode;
begin
  ANode := FForm.tvGroups.GetNodeAt(APoint.X, APoint.Y);
  Result := ANode <> nil;
  if Result then
  begin
    AGroup := GetGroup(ANode);
    ARect := ANode.DisplayRect(False);
    Result := AGroup <> nil;
  end;
end;

function TdxNavBarCustomizationDropInfoCalculator.IsLinkTarget(
  const APoint: TPoint; var ALink: TdxNavBarItemLink;
  var ARect: TRect): Boolean;

  function GetLink(ANode: TTreeNode): TdxNavBarItemLink;
  begin
    if TObject(ANode.Data) is TdxNavBarItemLink then
      Result := TdxNavBarItemLink(ANode.Data)
    else
      Result := nil;
  end;

var
  ANode: TTreeNode;
begin
  ANode := FForm.tvGroups.GetNodeAt(APoint.X, APoint.Y);
  Result := ANode <> nil;
  if Result then
  begin
    ALink := GetLink(ANode);
    ARect := ANode.DisplayRect(False);
    Result := ALink <> nil;
  end;
end;

function TdxNavBarCustomizationDropInfoCalculator.IsTargetVerticalLayout(
  AGroup: TdxNavBarGroup): Boolean;
begin
  Result := True;
end;

{ TdxNavBarCustomizationDragControlObject }

constructor TdxNavBarCustomizationDragControlObject.Create(
  ACustomizationForm: TdxNavBarCustomizationForm; AControl: TControl);
begin
  inherited Create(AControl);
  FCustomizationForm := ACustomizationForm;
end;

function TdxNavBarCustomizationDragControlObject.GetNavBar(
  AControl: TWinControl): TdxCustomNavBar;
begin
  if AControl = FCustomizationForm.tvGroups.InnerTreeView then
    Result := FCustomizationForm.NavBar
  else
    Result := inherited GetNavBar(AControl);
end;

initialization
  dxNavBarRegisterCustomizationFormClass(TdxNavBarCustomizationForm);

finalization
  dxNavBarUnregisterCustomizationFormClass(TdxNavBarCustomizationForm);

end.
