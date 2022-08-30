{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressPrinting System                                   }
{                                                                    }
{           Copyright (C) 1998-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSPRINTING SYSTEM AND            }
{   ALL ACCOMPANYING VCL CONTROLS AS PART OF AN                      }
{   EXECUTABLE PROGRAM ONLY.                                         }
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

unit dxPSXplorerTreeView;

interface

{$I cxVer.inc}

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, Windows, Classes, Messages, CommCtrl, Graphics, Controls, ExtCtrls, ComCtrls, Forms,
  cxControls, dxPSCore, cxTreeView, ImgList;

type
  TcxTreeViewClass = class of TcxTreeView;

  TCustomdxPSExplorerTreeViewContainer = class(TCustomdxPSExplorerTreeContainer)
  private
    FStorageStream: TMemoryStream;
    function GetFocusedNode: TTreeNode;
    function GetRootNode: TTreeNode;
    function GetTreeView: TcxTreeView;
    procedure TreeCancelEdit(Sender: TObject; ANode: TTreeNode);
    procedure TreeChanged(Sender: TObject; ANode: TTreeNode);
    procedure TreeCollapsing(Sender: TObject; Node: TTreeNode; var AllowCollapse: Boolean);
    procedure TreeCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode;
      State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure TreeDblClick(Sender: TObject);
    procedure TreeDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure TreeDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure TreeEdited(Sender: TObject; Node: TTreeNode; var S: string);
    procedure TreeEditing(Sender: TObject; Node: TTreeNode; var AllowEdit: Boolean);
    procedure TreeExpanding(Sender: TObject; Node: TTreeNode; var AllowExpansion: Boolean);
    procedure TreeKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure TreeKeyPress(Sender: TObject; var Key: Char);
    procedure TreeMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

    procedure ReadTreeViewState;
    procedure WriteTreeViewState;
  protected
    procedure AddItem(AParent: TdxPSExplorerFolder; AnItem: TCustomdxPSExplorerItem); override;
    procedure Clear; override;
    procedure DeleteItem(AnItem: TCustomdxPSExplorerItem); override;
    procedure InvalidateItem(AnItem: TCustomdxPSExplorerItem); override;
    procedure MoveItem(AnItem: TCustomdxPSExplorerItem); override;
    procedure RenameItem(AnItem: TCustomdxPSExplorerItem); override;

    function GetCreationParent: TdxPSExplorerFolder; override;
    function GetFocusedItem: TCustomdxPSExplorerItem; override;
    function GetIsEditing: Boolean; override;
    function GetIsFolderSelected: Boolean; override;
    function GetIsItemSelected: Boolean; override;
    function GetIsRootSelected: Boolean; override;
    function GetSelectedFolder: TdxPSExplorerFolder; override;
    function GetSelectedItem: TCustomdxPSExplorerItem; override;
    function GetSelectedItemText: string; override;
    procedure SetFocusedItem(Value: TCustomdxPSExplorerItem); override;
    procedure SetSelectedItem(Value: TCustomdxPSExplorerItem); override;
    procedure SetSelectedItemText(const Value: string); override;

    procedure RestoreState; override;
    procedure SaveState; override;

    procedure InitializeTreeContainer; override;

    function AddNode(AParent: TTreeNode; AnItem: TCustomdxPSExplorerItem): TTreeNode; virtual;
    function CanDragNode(ANode: TTreeNode): Boolean; virtual;
    function GetHitTestInfoAt(X, Y: Integer): THitTests;
    function GetItemByNode(ANode: TTreeNode): TCustomdxPSExplorerItem;
    function GetNodeByItem(AnItem: TCustomdxPSExplorerItem): TTreeNode;

    property FocusedNode: TTreeNode read GetFocusedNode;
    property RootNode: TTreeNode read GetRootNode;
    property TreeView: TcxTreeView read GetTreeView;
  public
    function BeginEdit(AnImmediate: Boolean = True): Boolean; override;
    procedure EndEdit(ACancel: Boolean); override;

    class function ControlClass: TWinControlClass; override;
    class function TreeViewClass: TcxTreeViewClass; virtual;

    procedure CollapseItem(AnItem: TCustomdxPSExplorerItem; ARecursive: Boolean = False); override;
    procedure ExpandItem(AnItem: TCustomdxPSExplorerItem; ARecursive: Boolean = False); override;
    procedure ItemDataLoaded(AnItem: TCustomdxPSExplorerItem); override;
    procedure ItemDataUnloaded(AnItem: TCustomdxPSExplorerItem); override;
    procedure MakeItemVisible(AnItem: TCustomdxPSExplorerItem); override;

    procedure BeginUpdate; override;
    procedure EndUpdate; override;

    procedure RefreshSorting(ANode: TObject); override;
    procedure RefreshSorting(AFolder: TdxPSExplorerFolder); override;

    function GetDropTarget(X, Y: Integer): TdxPSExplorerFolder; override;
    function GetItemAt(X, Y: Integer): TCustomdxPSExplorerItem; override;
    function GetItemAtMousePos: TCustomdxPSExplorerItem;
  end;

  TdxPSExplorerTreeViewScrollArea = (etsaLeft, etsaTop, etsaRight, etsaBottom);
  TdxPSExplorerTreeViewScrollAreas = set of TdxPSExplorerTreeViewScrollArea;

  TdxPSExplorerTreeView = class(TcxTreeView)
  private
    FContainer: TCustomdxPSExplorerTreeViewContainer;
    FDragImage: TDragImageList;
    FExpandTimer: TTimer;
    FFlat: Boolean;
    FSavedDropTarget: TTreeNode;
    FSavedScrollActiveAreas: TdxPSExplorerTreeViewScrollAreas;
    FScrollTimer: TTimer;
    function GetIsMouseInScrollArea: Boolean;
    function GetIsMouseInScrollAreaBound(ScrollArea: TdxPSExplorerTreeViewScrollArea): Boolean;
    function GetScrollActiveAreas: TdxPSExplorerTreeViewScrollAreas;
    function GetScrollAreaBounds(ScrollArea: TdxPSExplorerTreeViewScrollArea): TRect;
    procedure SetFlat(Value: Boolean);
    procedure OnExpandTimer(Sender: TObject);
    procedure OnScrollTimer(Sender: TObject);
    procedure WMNCPaint(var Message: TWMNCPaint); message WM_NCPAINT;
    procedure CMHintShow(var Message: TCMHintShow); message CM_HINTSHOW;
  protected
    function CanEdit(Node: TTreeNode): Boolean; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DblClick; override;
    procedure DoChange(Sender: TObject; Node: TTreeNode);
    procedure DoEndDrag(Target: TObject; X, Y: Integer); override;
    procedure DoStartDrag(var DragObject: TDragObject); override;
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure WndProc(var Message: TMessage); override;

    class function GetTreeViewClass: TcxCustomInnerTreeViewClass; override;
    function CanDragNode(ANode: TTreeNode): Boolean; virtual;
    procedure DrawFlatEdge; virtual;
    function GetInfoTip(AnItem: HTREEITEM): string;
    function MakeDragBitmap(ANode: TTreeNode): Graphics.TBitmap; virtual;
    procedure UpdateDragMode(ANode: TTreeNode);

    property Container: TCustomdxPSExplorerTreeViewContainer read FContainer write FContainer;
    property IsMouseInScrollArea: Boolean read GetIsMouseInScrollArea;
    property IsMouseInScrollAreaBounds[ScrollArea: TdxPSExplorerTreeViewScrollArea]: Boolean read GetIsMouseInScrollAreaBound;
    property ScrollActiveAreas: TdxPSExplorerTreeViewScrollAreas read GetScrollActiveAreas;
    property ScrollAreaBounds[ScrollArea: TdxPSExplorerTreeViewScrollArea]: TRect read GetScrollAreaBounds;
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateEx(AOwner: TComponent; AContainer: TCustomdxPSExplorerTreeViewContainer); virtual;

    function GetDragImages: TDragImageList; override;
    procedure DragDrop(Source: TObject; X, Y: Integer); override;

    property Flat: Boolean read FFlat write SetFlat;
  end;

  { TdxPSExplorerTreeViewContainer }

  TdxPSExplorerTreeViewContainer = class(TCustomdxPSExplorerTreeViewContainer)
  private
    function GetTreeView: TdxPSExplorerTreeView;
  protected
    procedure InitializeTreeContainer; override;
  public
    class function TreeViewClass: TcxTreeViewClass; override;
    property TreeView: TdxPSExplorerTreeView read GetTreeView;
  end;

  { TcxPSExplorerInnerTreeView }

  TcxPSExplorerInnerTreeView = class(TcxCustomInnerTreeView)
  private
    function GetTreeViewContainer: TCustomdxPSExplorerTreeViewContainer;
  protected
    function CanCollapse(Node: TTreeNode): Boolean; override;
    function CanExpand(Node: TTreeNode): Boolean; override;
    //
    property TreeViewContainer: TCustomdxPSExplorerTreeViewContainer read GetTreeViewContainer;
  end;

implementation

uses
  SysUtils, cxClasses, dxPSUtl, dxPSGlbl, dxCore, dxPSEngn;

const
  ScrollAreaHeight = 30;
  ScrollAreaWidth = 30;

type
  TcxControlAccess = class(TcxControl);

{ TCustomdxPSExplorerTreeViewContainer }

function TCustomdxPSExplorerTreeViewContainer.BeginEdit(AnImmediate: Boolean = True): Boolean;
var
  SelectedNode: TTreeNode;
begin
  Result := CanRenameSelectedItem;
  if Result then
  begin
    SelectedNode := TreeView.Selected;
    if AnImmediate then
      Result := SelectedNode.EditText
    else
      Result := PostMessage(TreeView.Handle, TVM_EDITLABEL, 0, lParam(SelectedNode.ItemId));
    Host.UpdateState;
  end;
end;

procedure TCustomdxPSExplorerTreeViewContainer.EndEdit(ACancel: Boolean);
var
  Node: TTreeNode;
begin
  if TreeView.IsEditing then
  begin
    Node := TreeView.Selected;
    if Node <> nil then Node.EndEdit(ACancel);
  end;
end;

procedure TCustomdxPSExplorerTreeViewContainer.CollapseItem(AnItem: TCustomdxPSExplorerItem;
  ARecursive: Boolean = False);
var
  Node: TTreeNode;
begin
  Node := GetNodeByItem(AnItem);
  if Node <> nil then Node.Collapse(ARecursive);
end;

procedure TCustomdxPSExplorerTreeViewContainer.ExpandItem(AnItem: TCustomdxPSExplorerItem;
  ARecursive: Boolean = False);
var
  Node: TTreeNode;
begin
  Node := GetNodeByItem(AnItem);
  if Node <> nil then
  begin
    dxPSCore.dxPSStartWait;
    try
      Node.Expand(ARecursive);
    finally
      dxPSCore.dxPSStopWait;
    end;
  end;
end;

procedure TCustomdxPSExplorerTreeViewContainer.ItemDataLoaded(AnItem: TCustomdxPSExplorerItem);
begin
  TreeView.Refresh;
end;

procedure TCustomdxPSExplorerTreeViewContainer.ItemDataUnloaded(AnItem: TCustomdxPSExplorerItem);
begin
  TreeView.Refresh;
end;

procedure TCustomdxPSExplorerTreeViewContainer.MakeItemVisible(AnItem: TCustomdxPSExplorerItem);
var
  Node: TTreeNode;
begin
  Node := GetNodeByItem(AnItem);
  if Node <> nil then
  begin
    Node.MakeVisible;
    Node.Focused := True;
    Node.Selected := True;
  end;
end;

procedure TCustomdxPSExplorerTreeViewContainer.BeginUpdate;
begin
  inherited BeginUpdate;
  TreeView.Items.BeginUpdate;
end;

procedure TCustomdxPSExplorerTreeViewContainer.EndUpdate;
begin
  TreeView.Items.EndUpdate;
  inherited EndUpdate;
end;

function TreeSortFunc(lParam1, lParam2, lParamSort: LPARAM): Integer; stdcall;
begin
  if lParam2 <> 0 then
    Result := TCustomdxPSExplorerItemComparator.CompareItems(TTreeNode(lParam1).Data, TTreeNode(lParam2).Data)
  else
    Result := 0;
end;


procedure TCustomdxPSExplorerTreeViewContainer.RefreshSorting(ANode: TObject);
begin
  if Explorer.State * [esLoading{, esRefreshing}] = [] then
    if ANode <> nil then
      TTreeNode(ANode).CustomSort(TreeSortFunc, 0, False)
    else
      TreeView.CustomSort(TreeSortFunc, 0, True);
end;

procedure TCustomdxPSExplorerTreeViewContainer.RefreshSorting(AFolder: TdxPSExplorerFolder);
var
  Node: TObject;
begin
  if (AFolder = nil) or AFolder.IsRoot then
    Node := nil
  else
    Node := GetNodeByItem(AFolder);
  RefreshSorting(Node);
end;

class function TCustomdxPSExplorerTreeViewContainer.ControlClass: TWinControlClass;
begin
  Result := TreeViewClass;
end;

class function TCustomdxPSExplorerTreeViewContainer.TreeViewClass: TcxTreeViewClass;
begin
  Result := TcxTreeView;
end;

function TCustomdxPSExplorerTreeViewContainer.GetItemAt(X, Y: Integer): TCustomdxPSExplorerItem;
begin
  Result := GetItemByNode(TreeView.GetNodeAt(X, Y))
end;

function TCustomdxPSExplorerTreeViewContainer.GetItemAtMousePos: TCustomdxPSExplorerItem;
begin
  with Control.ScreenToClient(GetMouseCursorPos) do
    Result := GetItemAt(X, Y);
end;

function TCustomdxPSExplorerTreeViewContainer.GetDropTarget(X, Y: Integer): TdxPSExplorerFolder;
var
  Item: TCustomdxPSExplorerItem;
begin
  if TreeView.DropTarget <> nil then
    if [ComCtrls.htBelow, ComCtrls.htNowhere] * GetHitTestInfoAt(X, Y) = [] then
    begin
      Item := GetItemByNode(TreeView.DropTarget);
      if Item is TdxPSExplorerFolder then
        Result := TdxPSExplorerFolder(Item)
      else
        Result := nil;
    end
    else
      Result := Explorer.Root
  else
    Result := Explorer.Root;
end;

procedure TCustomdxPSExplorerTreeViewContainer.AddItem(AParent: TdxPSExplorerFolder;
  AnItem: TCustomdxPSExplorerItem);
var
  Node: TTreeNode;
begin
  Node := AddNode(GetNodeByItem(AParent), AnItem);
  if AnItem is TdxPSExplorerFolder then
    Node.HasChildren := TdxPSExplorerFolderHelper.GetHasChildren(TdxPSExplorerFolder(AnItem));
end;

procedure TCustomdxPSExplorerTreeViewContainer.Clear;
begin
  TreeView.Items.Clear;
end;

procedure TCustomdxPSExplorerTreeViewContainer.DeleteItem(AnItem: TCustomdxPSExplorerItem);
begin
  inherited;
  GetNodeByItem(AnItem).Delete;
end;

procedure TCustomdxPSExplorerTreeViewContainer.InvalidateItem(AnItem: TCustomdxPSExplorerItem);
var
  ANode: TTreeNode;
  R: TRect;
begin
  if TreeView.HandleAllocated then
  begin
    ANode := GetNodeByItem(AnItem);
    if Assigned(ANode) then
    begin
      R := ANode.DisplayRect(False);
      InvalidateRect(TreeView.Handle, @R, True);
    end;
  end;
end;

procedure TCustomdxPSExplorerTreeViewContainer.MoveItem(AnItem: TCustomdxPSExplorerItem);
var
  Node: TTreeNode;
begin
  Node := GetNodeByItem(AnItem);
  if Node <> nil then { We are not in creation phase of parent assigning }
    if AnItem is TdxPSExplorerFolder then
      Node.MoveTo(GetNodeByItem(AnItem.Parent), naAddChildFirst)
    else
      Node.MoveTo(GetNodeByItem(AnItem.Parent), naAddChild);
end;

procedure TCustomdxPSExplorerTreeViewContainer.RenameItem(AnItem: TCustomdxPSExplorerItem);
begin
  GetNodeByItem(AnItem).Text := AnItem.DisplayName;
end;

function TCustomdxPSExplorerTreeViewContainer.GetCreationParent: TdxPSExplorerFolder;
var
  Item: TCustomdxPSExplorerItem;
begin
  Item := SelectedItem;
  if Item = nil then
    Result := Explorer.Root
  else
    if not (Item is TdxPSExplorerFolder) then
      Result := Item.Parent
    else
      Result := TdxPSExplorerFolder(Item);
end;

function TCustomdxPSExplorerTreeViewContainer.GetFocusedItem: TCustomdxPSExplorerItem;
begin
  Result := GetItemByNode(FocusedNode);
end;

function TCustomdxPSExplorerTreeViewContainer.GetIsEditing: Boolean;
begin
  Result := TreeView.HandleAllocated and TreeView.IsEditing;
end;

function TCustomdxPSExplorerTreeViewContainer.GetIsFolderSelected: Boolean;
begin
  Result := SelectedItem is TdxPSExplorerFolder;
end;

function TCustomdxPSExplorerTreeViewContainer.GetIsItemSelected: Boolean;
begin
  Result := SelectedItem is TdxPSExplorerItem;
end;

function TCustomdxPSExplorerTreeViewContainer.GetIsRootSelected: Boolean;
begin
  Result := SelectedItem = Explorer.Root;
end;

function TCustomdxPSExplorerTreeViewContainer.GetSelectedFolder: TdxPSExplorerFolder;
var
  Item: TCustomdxPSExplorerItem;
begin
  Item := SelectedItem;
  if Item is TdxPSExplorerFolder then
    Result := TdxPSExplorerFolder(Item)
  else
    Result := nil;
end;

function TCustomdxPSExplorerTreeViewContainer.GetSelectedItem: TCustomdxPSExplorerItem;
begin
  Result := GetItemByNode(TreeView.Selected);
end;

function TCustomdxPSExplorerTreeViewContainer.GetSelectedItemText: string;
var
  Node: TTreeNode;
begin
  Node := GetNodeByItem(SelectedItem);
  if Node <> nil then
    Result := Node.Text
  else
    Result := '';
end;

procedure TCustomdxPSExplorerTreeViewContainer.SetFocusedItem(Value: TCustomdxPSExplorerItem);
var
  Node: TTreeNode;
begin
  Node := GetNodeByItem(Value);
  if Node <> nil then
    Node.Focused := True;
end;

procedure TCustomdxPSExplorerTreeViewContainer.SetSelectedItem(Value: TCustomdxPSExplorerItem);
var
  Node: TTreeNode;
begin
  Node := GetNodeByItem(Value);
  if Node <> nil then
    Node.Selected := True;
end;

procedure TCustomdxPSExplorerTreeViewContainer.SetSelectedItemText(const Value: string);
var
  Node: TTreeNode;
begin
  Node := GetNodeByItem(SelectedItem);
  if Node <> nil then
  begin
    Node.Text := Value;
    //RefreshSorting(Node.Parent); {.1}
  end;
end;

procedure TCustomdxPSExplorerTreeViewContainer.RestoreState;
begin
  FStorageStream.Position := 0;
  ReadTreeViewState;
  FreeAndNil(FStorageStream);
end;

procedure TCustomdxPSExplorerTreeViewContainer.SaveState;
begin
  FStorageStream := TMemoryStream.Create;
  WriteTreeViewState;
end;

procedure TCustomdxPSExplorerTreeViewContainer.InitializeTreeContainer;
begin
  inherited;
  TreeView.OnCustomDrawItem := TreeCustomDrawItem;
  TreeView.OnEdited := TreeEdited;
end;

function TCustomdxPSExplorerTreeViewContainer.AddNode(AParent: TTreeNode;
  AnItem: TCustomdxPSExplorerItem): TTreeNode;
begin
  Result := TreeView.Items.AddChildObject(AParent, AnItem.DisplayName, AnItem);
  Result.ImageIndex := TCustomdxPSExplorerItemHelper.GetImageIndex(AnItem);
  Result.SelectedIndex := TCustomdxPSExplorerItemHelper.GetSelectedIndex(AnItem);
//  RefreshSorting(AParent); //3.1
end;

function TCustomdxPSExplorerTreeViewContainer.CanDragNode(ANode: TTreeNode): Boolean;
var
  Item: TCustomdxPSExplorerItem;
begin
  Item := GetItemByNode(ANode);
  Result := (Item <> nil) and Item.CanMove;
end;

function TCustomdxPSExplorerTreeViewContainer.GetHitTestInfoAt(X, Y: Integer): THitTests;
begin
  Result := TreeView.GetHitTestInfoAt(X, Y);
end;

function TCustomdxPSExplorerTreeViewContainer.GetItemByNode(ANode: TTreeNode): TCustomdxPSExplorerItem;
begin
  if ANode <> nil then
    Result := TCustomdxPSExplorerItem(ANode.Data)
  else
    Result := nil;
end;

function TCustomdxPSExplorerTreeViewContainer.GetNodeByItem(AnItem: TCustomdxPSExplorerItem): TTreeNode;

  function FindInChildren(ANode: TTreeNode): TTreeNode;
  var
    I: Integer;
  begin
    for I := 0 to ANode.Count - 1 do
    begin
      Result := ANode[I];
      if GetItemByNode(Result) = AnItem then
        Exit;
      if Result.HasChildren then
      begin
        Result := FindInChildren(Result);
        if Result <> nil then Exit;
      end;
    end;
    Result := nil;
  end;

begin
  Result := RootNode;
  if (Result <> nil) and (GetItemByNode(Result) <> AnItem) then
    Result := FindInChildren(Result);
end;

function TCustomdxPSExplorerTreeViewContainer.GetFocusedNode: TTreeNode;
begin
  Result := TreeView.Selected;
end;

function TCustomdxPSExplorerTreeViewContainer.GetRootNode: TTreeNode;
begin
  Result := TreeView.Items.GetFirstNode;
end;

function TCustomdxPSExplorerTreeViewContainer.GetTreeView: TcxTreeView;
begin
  Result := TcxTreeView(Control);
end;

procedure TCustomdxPSExplorerTreeViewContainer.TreeCollapsing(Sender: TObject;
  Node: TTreeNode; var AllowCollapse: Boolean);
begin
  AllowCollapse := GetItemByNode(Node) <> Explorer.Root;
end;

procedure TCustomdxPSExplorerTreeViewContainer.TreeCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
var
  Item: TCustomdxPSExplorerItem;
begin
  Item := GetItemByNode(Node);
  if Item is TdxPSExplorerItem then
    with Sender.Canvas do
    begin
      if TdxPSExplorerItem(Item).IsCurrentlyLoaded then
        Font.Style := Font.Style + [fsBold];
      if TdxPSExplorerItem(Item).HasInvalidData then
        Font.Color := clGrayText;
    end;
end;

procedure TCustomdxPSExplorerTreeViewContainer.TreeDblClick(Sender: TObject);
begin
  if (SelectedItem <> nil) and (GetItemAtMousePos = SelectedItem) and CanLoadSelectedItemData then
    LoadSelectedItemData;
end;

procedure TCustomdxPSExplorerTreeViewContainer.TreeDragDrop(Sender, Source: TObject;
  X, Y: Integer);
begin
  SelectedItem.Parent := GetDropTarget(X, Y);
end;

procedure TCustomdxPSExplorerTreeViewContainer.TreeDragOver(Sender, Source: TObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := (Source = Sender) and SelectedItem.CanMoveTo(GetDropTarget(X, Y));
end;

procedure TCustomdxPSExplorerTreeViewContainer.TreeCancelEdit(Sender: TObject; ANode: TTreeNode);
begin
  Host.UpdateState;
end;

procedure TCustomdxPSExplorerTreeViewContainer.TreeChanged(Sender: TObject; ANode: TTreeNode);
begin
  Host.UpdateState;
end;

procedure TCustomdxPSExplorerTreeViewContainer.TreeEdited(Sender: TObject; Node: TTreeNode;
  var S: string);
var
  Item: TCustomdxPSExplorerItem;
  ErrorText: string;
begin
  if Node.Selected then Node.Focused := True;

  Item := GetItemByNode(Node);
  if not dxSameStr(Item.DisplayName, S) then
    if not Item.CanRenameTo(S) then
    begin
      ErrorText := Item.CannotRenameMessageText(Item.DisplayName, S);
      dxPSUtl.MessageError(ErrorText);
      BeginEdit(False);
    end
    else
      Item.Name := S;

  Host.UpdateState;
end;

procedure TCustomdxPSExplorerTreeViewContainer.TreeEditing(Sender: TObject; Node: TTreeNode;
  var AllowEdit: Boolean);
var
  Item: TCustomdxPSExplorerItem;
  Editor: HWND;
begin
  Item := GetItemByNode(Node);
  AllowEdit := (Item <> nil) and Item.CanRename;
  if AllowEdit then
  begin
    TreeView.Selected.Text := Item.DisplayName;
    Editor := TreeView_GetEditControl(TreeView.Handle);
    if IsWindow(Editor) then
      SendMessage(Editor, WM_SETTEXT, 0, LPARAM(PChar(Item.DisplayName)));
  end;
end;

procedure TCustomdxPSExplorerTreeViewContainer.TreeExpanding(Sender: TObject; Node: TTreeNode;
  var AllowExpansion: Boolean);
var
  Item: TCustomdxPSExplorerItem;
begin
  Item := GetItemByNode(Node);
  if Item is TdxPSExplorerFolder then
  begin
    TdxPSExplorerFolder(Item).Populate;
    //RefreshSorting(Item);
  end;
end;

procedure TCustomdxPSExplorerTreeViewContainer.TreeKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  ProcessKeyDown(Key, Shift);
end;

procedure TCustomdxPSExplorerTreeViewContainer.TreeKeyPress(Sender: TObject;
  var Key: Char);
begin
  ProcessKeyPress(Key);
end;

procedure TCustomdxPSExplorerTreeViewContainer.TreeMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Host.UpdateState;
end;

type
  TdxNodeInfo = record
    Expanded: Integer;
    Count: Integer;
    UniqueIDSize: Integer;
    //UniqueID: TBytes with UniqueIDSize length
  end;

function GetNodeInfoSize: Integer;
begin
  Result := SizeOf(TdxNodeInfo);
end;

procedure TCustomdxPSExplorerTreeViewContainer.ReadTreeViewState;

  function ReadNode(ARecursive: Boolean): TTreeNode;
  var
    NodeInfo: TdxNodeInfo;
    Bytes: TBytes;
    Item: TCustomdxPSExplorerItem;
    I: Integer;
  begin
    Result := nil;
    FStorageStream.ReadBuffer(NodeInfo, GetNodeInfoSize);
    if NodeInfo.UniqueIDSize <> 0 then
    begin
      SetLength(Bytes, NodeInfo.UniqueIDSize);
      FStorageStream.ReadBuffer(Pointer(Bytes)^, NodeInfo.UniqueIDSize);
      Item := Explorer.FindCustomItemByUniqueID(Bytes);
      if Item <> nil then
      begin
        Result := GetNodeByItem(Item);
        if Result <> nil then
          Result.Expanded := NodeInfo.Expanded = 1;
      end;
    end;
    if ARecursive then
      for I := 0 to NodeInfo.Count - 1 do
        ReadNode(ARecursive);
  end;

  procedure ReadSelection;
  var
    Buffer: Integer;
  begin
    FStorageStream.ReadBuffer(Buffer, SizeOf(Buffer));
    if Buffer <> 0 then
      TreeView.Selected := ReadNode(False);
  end;

begin
  BeginUpdate;
  try
    ReadNode(True);
    ReadSelection;
  finally
    EndUpdate;
  end;
end;

procedure TCustomdxPSExplorerTreeViewContainer.WriteTreeViewState;

  procedure WriteNode(ANode: TTreeNode; ARecursive: Boolean);
  var
    Item: TCustomdxPSExplorerItem;
    NodeInfo: TdxNodeInfo;
    Bytes: TBytes;
    I: Integer;
  begin
    Item := GetItemByNode(ANode);
    with NodeInfo do
    begin
      Expanded := Ord(ANode.Expanded);
      if Expanded = 1 then
        Count := ANode.Count
      else
        Count := 0;
      UniqueIDSize := Item.GetUniqueID(Bytes);
    end;
    FStorageStream.WriteBuffer(NodeInfo, GetNodeInfoSize);
    if NodeInfo.UniqueIDSize <> 0 then
      FStorageStream.WriteBuffer(Pointer(Bytes)^, NodeInfo.UniqueIDSize);

    for I := 0 to ANode.Count - 1 do
      WriteNode(ANode[I], ARecursive);
  end;

  procedure WriteSelection;
  var
    Flag: Integer;
  begin
    Flag := Ord(TreeView.Selected <> nil);
    FStorageStream.WriteBuffer(Flag , SizeOf(Flag));
    if TreeView.Selected <> nil then
      WriteNode(TreeView.Selected, False);
  end;

begin
  WriteNode(RootNode, True);
  WriteSelection;
end;

{ TdxPSExplorerTreeView }

constructor TdxPSExplorerTreeView.Create(AOwner: TComponent);

  function CreateTimer(AnInterval: Integer; AnOnTimer: TNotifyEvent): TTimer;
  begin
    Result := TTimer.Create(Self);
    Result.Enabled := False;
    Result.Interval := AnInterval;
    Result.OnTimer := AnOnTimer;
  end;

begin
  inherited;
  Align := alClient;
  DragMode := dmAutomatic;
  HideSelection := False;
  Images := dxPSExplorerImages;
  ShowHint := True;
  ShowRoot := False;
  OnChange := DoChange;
  if IsWinXPOrLater then
    ShowLines := False;

  FDragImage := TDragImageList.Create(Self);

  FExpandTimer := CreateTimer(500, OnExpandTimer);
  FScrollTimer := CreateTimer(80, OnScrollTimer);
end;

constructor TdxPSExplorerTreeView.CreateEx(AOwner: TComponent;
  AContainer: TCustomdxPSExplorerTreeViewContainer);
begin
  Create(AOwner);
  FContainer := AContainer;
end;

procedure TdxPSExplorerTreeView.DragDrop(Source: TObject; X, Y: Integer);
begin
  inherited;
  Container.TreeDragDrop(Self, Source, X, Y);
end;

function TdxPSExplorerTreeView.CanEdit(Node: TTreeNode): Boolean;
begin
  Result := inherited CanEdit(Node);
  Container.TreeEditing(Self, Node, Result);
end;

procedure TdxPSExplorerTreeView.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or TVS_INFOTIP;
end;

procedure TdxPSExplorerTreeView.DblClick;
begin
  inherited;
  Container.TreeDblClick(Self);
end;

procedure TdxPSExplorerTreeView.DoChange(Sender: TObject; Node: TTreeNode);
begin
  Container.TreeChanged(Self, Node);
end;

procedure TdxPSExplorerTreeView.DoEndDrag(Target: TObject; X, Y: Integer);
begin
  FDragImage.Clear;
  FSavedScrollActiveAreas := [];
  FSavedDropTarget := nil;
  FExpandTimer.Enabled := False;
  FScrollTimer.Enabled := False;
  inherited;
end;

procedure TdxPSExplorerTreeView.DoStartDrag(var DragObject: TDragObject);
var
  Pt: TPoint;
  DragNode: TTreeNode;
  Bitmap: Graphics.TBitmap;
begin
  inherited;
  Pt := GetMouseCursorClientPos;
  DragNode := GetNodeAt(Pt.X, Pt.Y);
  if DragNode <> nil then
  begin
    FDragImage.Handle := TreeView_CreateDragImage(Handle, DragNode.ItemId);
    if FDragImage.HandleAllocated then
    begin
      //Item := GetItemFromNode(DragNode);
      //Item.Unload;
      Bitmap := MakeDragBitmap(DragNode);
      try
        with FDragImage do
        begin
          Clear;
          Width := Bitmap.Width;
          Height := Bitmap.Height;
          AddMasked(Bitmap, clDefault);
          SetDragImage(0, 2, 2);
        end;
      finally
        Bitmap.Free;
      end;
    end;
  end;
end;

procedure TdxPSExplorerTreeView.DragOver(Source: TObject; X, Y: Integer; State: TDragState;
  var Accept: Boolean);
begin
  inherited;
  Container.TreeDragOver(Self, Source, X, Y, State, Accept);
  if Accept and (DropTarget <> nil) and not DropTarget.Expanded then
  begin
    FSavedDropTarget := DropTarget;
    FExpandTimer.Enabled := False;
    FExpandTimer.Enabled := True;
  end;
  if IsMouseInScrollArea then
  begin
    FSavedScrollActiveAreas := ScrollActiveAreas;
    FScrollTimer.Enabled := False;
    FScrollTimer.Enabled := True;
  end;
end;

function TdxPSExplorerTreeView.GetDragImages: TDragImageList;
begin
  if FDragImage.Count <> 0 then
    Result := FDragImage
  else
    Result := nil;
end;

procedure TdxPSExplorerTreeView.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  Container.TreeKeyDown(Self, Key, Shift);
end;

procedure TdxPSExplorerTreeView.KeyPress(var Key: Char);
begin
  inherited;
  Container.TreeKeyPress(Self, Key);
end;

procedure TdxPSExplorerTreeView.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  Container.TreeMouseDown(Self, Button, Shift, X, Y);
end;

procedure TdxPSExplorerTreeView.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Pt: TPoint;
  Node: TTreeNode;
begin
  if Button = mbRight then
  begin
    Pt := Point(X, Y);
    Node := GetNodeAt(Pt.X, Pt.Y);
    if Node <> nil then
    begin
      Node.Focused := True;
      Node.Selected := True;
    end;
  end;
  inherited;
end;

procedure TdxPSExplorerTreeView.WndProc(var Message: TMessage);
var
  Node: TTreeNode;
begin
  case Message.Msg of
    WM_LBUTTONDOWN, WM_LBUTTONDBLCLK:
      with TWMMouse(Message) do
        if Keys and MK_LBUTTON = MK_LBUTTON then
        begin
          Node := GetNodeAt(XPos, YPos);
          if Assigned(Node) then
            UpdateDragMode(Node);
        end;
  end;
  inherited WndProc(Message);
end;

function TdxPSExplorerTreeView.CanDragNode(ANode: TTreeNode): Boolean;
begin
  Result := (Container = nil) or Container.CanDragNode(ANode);
end;

class function TdxPSExplorerTreeView.GetTreeViewClass: TcxCustomInnerTreeViewClass;
begin
  Result := TcxPSExplorerInnerTreeView;
end;

procedure TdxPSExplorerTreeView.DrawFlatEdge;
var
  R: TRect;
  DC: HDC;
begin
  GetWindowRect(Handle, R);
  OffsetRect(R, -R.Left, -R.Top);

  DC := GetWindowDC(Handle);
  try
    FrameRect(DC, R, GetSysColorBrush(COLOR_BTNSHADOW));
    InflateRect(R, -1, -1);
    FrameRect(DC, R, GetSysColorBrush(COLOR_WINDOW));
  finally
    ReleaseDC(Handle, DC);
  end;
end;

function TdxPSExplorerTreeView.GetInfoTip(AnItem: HTREEITEM): string;
var
  ExplorerItem: TCustomdxPSExplorerItem;
begin
  ExplorerItem := TCustomdxPSExplorerItem(Items.GetNode(AnItem).Data);
  if ExplorerItem <> nil then
    Result := ExplorerItem.InfoTip
  else
    Result := '';
  if Result <> '' then
     Result := Result + ' ' + #0#0;
end;

function TdxPSExplorerTreeView.MakeDragBitmap(ANode: TTreeNode): Graphics.TBitmap;
const
  Format: UINT = DT_LEFT or DT_CENTER or DT_END_ELLIPSIS or DT_SINGLELINE;
var
  S: string;
  R: TRect;
begin
  S := ANode.Text;

  Result := Graphics.TBitmap.Create;
  with Result do
  begin
    Width := Images.Width + 2 + Self.Canvas.TextWidth(S) + 1;
    Height := TreeView_GetItemHeight(Self.Handle);
    R := Rect(0, 0, Width, Height);

    Canvas.Brush.Color := clWhite;
    Canvas.FillRect(R);

    Images.Draw(Canvas, 0, 0, ANode.ImageIndex);

    Canvas.Font.Assign(Self.Font);
    Canvas.Brush.Style := bsClear;
    Inc(R.Left, Images.Width + 2);
    Windows.DrawText(Canvas.Handle, PChar(S), Length(S), R, Format);
    Canvas.Brush.Style := bsSolid;
  end;
end;

procedure TdxPSExplorerTreeView.UpdateDragMode(ANode: TTreeNode);
const
  DragModesMap: array[Boolean] of TDragMode = (dmManual, dmAutomatic);
begin
  DragMode := DragModesMap[CanDragNode(ANode)];
end;

function TdxPSExplorerTreeView.GetIsMouseInScrollArea: Boolean;
var
  ScrollArea: TdxPSExplorerTreeViewScrollArea;
begin
  Result := True;
  for ScrollArea := Low(ScrollArea) to High(ScrollArea) do
    if IsMouseInScrollAreaBounds[ScrollArea] then Exit;
  Result := False;
end;

function TdxPSExplorerTreeView.GetIsMouseInScrollAreaBound(ScrollArea: TdxPSExplorerTreeViewScrollArea): Boolean;
begin
  Result := PtInRect(ScrollAreaBounds[ScrollArea], GetMouseCursorClientPos);
end;

function TdxPSExplorerTreeView.GetScrollActiveAreas: TdxPSExplorerTreeViewScrollAreas;
var
  ScrollArea: TdxPSExplorerTreeViewScrollArea;
begin
  Result := [];
  for ScrollArea := Low(ScrollArea) to High(ScrollArea) do
    if IsMouseInScrollAreaBounds[ScrollArea] then
      Include(Result, ScrollArea);
end;

function TdxPSExplorerTreeView.GetScrollAreaBounds(ScrollArea: TdxPSExplorerTreeViewScrollArea): TRect;
type
  TScrollBarKind = (sbkHorz, sbkVert);

  function HasScrollBar(AScrollBarKind: TScrollBarKind): Boolean;
  const
    ScrollBarKindMap: array[TScrollBarKind] of Integer = (WS_HSCROLL, WS_VSCROLL);
  begin
    Result := Windows.GetWindowLong(Handle, GWL_STYLE) and ScrollBarKindMap[AScrollBarKind] <> 0;
  end;

  function ScrollBarWidth(AScrollBarKind: TScrollBarKind): Integer;
  const
    ScrollBarWidthIndexMap: array[TScrollBarKind] of Integer = (SM_CYHSCROLL, SM_CXVSCROLL);
  begin
    Result := GetSystemMetrics(ScrollBarWidthIndexMap[AScrollBarKind]);
  end;

begin
  case ScrollArea of
    etsaLeft:
      Result := Rect(0, 0, ScrollAreaWidth, Height);
    etsaTop:
      Result := Rect(0, 0, Width, ScrollAreaHeight);
    etsaRight:
      begin
        Result := Rect(Width - ScrollAreaWidth, 0, Width, Height);
        if HasScrollBar(sbkVert) then
          Dec(Result.Left, ScrollBarWidth(sbkVert));
      end;
    else //etsaBottom
    begin
      Result := Rect(0, Height - ScrollAreaHeight, Width, Height);
      if HasScrollBar(sbkHorz) then
        Dec(Result.Top, ScrollBarWidth(sbkHorz));
    end;
  end;
end;

procedure TdxPSExplorerTreeView.SetFlat(Value: Boolean);
begin
  if FFlat <> Value then
  begin
    FFlat := Value;
    RecreateWnd;
  end;
end;

procedure TdxPSExplorerTreeView.OnExpandTimer(Sender: TObject);
begin
  FExpandTimer.Enabled := False;

  if DropTarget = FSavedDropTarget then
  begin
    ImageList_DragShowNolock(False);
    try
      DropTarget.Expand(False);
    finally
      Update;
      ImageList_DragShowNolock(True);
    end;
  end;
end;

procedure TdxPSExplorerTreeView.OnScrollTimer(Sender: TObject);

  procedure DoScroll(AMessage: Cardinal; AScrollCode: Integer);
  begin
    Perform(AMessage, AScrollCode, 0);
  end;

const
  ScrollMessages: array[TdxPSExplorerTreeViewScrollArea] of Cardinal = (WM_HSCROLL, WM_VSCROLL, WM_HSCROLL, WM_VSCROLL);
  ScrollCodes: array[TdxPSExplorerTreeViewScrollArea] of Integer = (SB_LINELEFT, SB_LINEUP, SB_LINERIGHT, SB_LINEDOWN);

var
  ScrollActiveAreas: TdxPSExplorerTreeViewScrollAreas;
  ScrollArea: TdxPSExplorerTreeViewScrollArea;
begin
  ScrollActiveAreas := Self.ScrollActiveAreas * FSavedScrollActiveAreas;
  if ScrollActiveAreas <> [] then
  begin
    ImageList_DragShowNolock(False);
    try
      for ScrollArea := Low(ScrollArea) to High(ScrollArea) do
        if ScrollArea in ScrollActiveAreas then
          DoScroll(ScrollMessages[ScrollArea], ScrollCodes[ScrollArea]);
    finally
      Update;
      ImageList_DragShowNolock(True);
    end;
  end;
  FSavedScrollActiveAreas := Self.ScrollActiveAreas;
end;

procedure TdxPSExplorerTreeView.WMNCPaint(var Message: TWMNCPaint);
begin
  inherited;
  if Flat then DrawFlatEdge;
end;

procedure TdxPSExplorerTreeView.CMHintShow(var Message: TCMHintShow);

  function HasInfoTip: Boolean;
  var
    Node: TTreeNode;
  begin
    Result := dxPSGlbl.CanShowHints;
    if Result then
    begin
      with Message.HintInfo^.CursorPos do
        Node := GetNodeAt(X, Y);
      Result := (Node <> nil) and (GetInfoTip(Node.ItemId) <> '');
    end;
  end;

var
  Node: TTreeNode;
begin
  if HasInfoTip then
  begin
    with Message.HintInfo^ do
    begin
      Node := GetNodeAt(CursorPos.X, CursorPos.Y);

      CursorRect := Node.DisplayRect(True);
      Windows.MapWindowPoints(Handle, 0, CursorRect, 2);
      HintStr := GetInfoTip(Node.ItemId);
      HintPos.Y := CursorRect.Top + Windows.GetSystemMetrics(SM_CYCURSOR);
      HintPos.X := CursorRect.Left + Windows.GetSystemMetrics(SM_CXCURSOR);
      HintMaxWidth := ClientWidth;
    end;
    Message.Result := 0;
  end
  else
    inherited;
end;

{ TdxPSExplorerTreeViewContainer }

class function TdxPSExplorerTreeViewContainer.TreeViewClass: TcxTreeViewClass;
begin
  Result := TdxPSExplorerTreeView;
end;

procedure TdxPSExplorerTreeViewContainer.InitializeTreeContainer;
begin
  inherited InitializeTreeContainer;
  TreeView.Container := Self;
  TreeView.Flat := Host.Flat;
  TreeView.OnCancelEdit := TreeCancelEdit;
  TreeView.Style.LookAndFeel.MasterLookAndFeel := TcxControlAccess(Host.TreeContainerParent).LookAndFeel;
end;

function TdxPSExplorerTreeViewContainer.GetTreeView: TdxPSExplorerTreeView;
begin
  Result := TdxPSExplorerTreeView(inherited TreeView)
end;

{ TcxPSExplorerInnerTreeView }

function TcxPSExplorerInnerTreeView.CanCollapse(Node: TTreeNode): Boolean;
begin
  Result := inherited CanCollapse(Node);
  TreeViewContainer.TreeCollapsing(Container, Node, Result);
end;

function TcxPSExplorerInnerTreeView.CanExpand(Node: TTreeNode): Boolean;
begin
  Result := inherited CanExpand(Node);
  TreeViewContainer.TreeExpanding(Container, Node, Result);
end;

function TcxPSExplorerInnerTreeView.GetTreeViewContainer: TCustomdxPSExplorerTreeViewContainer;
begin
  Result := TdxPSExplorerTreeView(Container).Container;
end;

initialization
  TCustomdxPSExplorerTreeViewContainer.Register;
  TdxPSExplorerTreeViewContainer.Register;

finalization
  TdxPSExplorerTreeViewContainer.Unregister;
  TCustomdxPSExplorerTreeViewContainer.Unregister;

end.

