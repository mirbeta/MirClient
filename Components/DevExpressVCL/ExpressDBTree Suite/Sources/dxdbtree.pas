{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           Express data-aware tree view                             }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSGRID AND ALL ACCOMPANYING VCL  }
{   CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY.                  }
{                                                                    }
{   THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED       }
{   FILES or ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE         }
{   COPIED, TRANSFERRED, SOLD, DISTRIBUTED, or OTHERWISE MADE        }
{   AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT   }
{   AND PERMISSION FROM DEVELOPER EXPRESS INC.                       }
{                                                                    }
{   CONSULT THE END USER LICENSE AGREEMENT FOR INFORMATION ON        }
{   ADDITIONAL RESTRICTIONS.                                         }
{                                                                    }
{********************************************************************}

unit dxdbtree;

interface

{$I cxVer.inc}

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Variants, ComCtrls, CommCtrl, DB, dxCore, dxMessages, cxGraphics, cxControls, dxtree;

type
  EDBTreeViewError = class(EdxException);

  TdxDBTreeView = class;

  TDataLinkTreeView = class(TDataLink)
  private
    FDBTreeView: TdxDBTreeView;
    Filter: string;
    Filtered: Boolean;
  protected
    procedure ActiveChanged; override;
    procedure DataSetChanged; override;
    procedure DataSetScrolled(Distance: Integer); override;
    procedure RecordChanged(Field: TField); override;
  public
    constructor Create;
  end;

  TdxDBTreeNodes = class;

  TdxDBTreeNode = class(TdxTreeNode)
  private
    IsInserting: Boolean;
    Refreshed: Boolean;
  protected
    DBTreeNodes: TdxDBTreeNodes;

    FKeyFieldValue: Variant;
    FParentFieldValue: Variant;
    FChildLoaded: Boolean;

    FIndex: Integer;
    FListText: string;

    FBkColor: TColor;
    FColor: TColor;
    FFontStyle: TFontStyles;
    FFontName: TFontName;
    FIsCustomDraw: Boolean;

    function GetParent: TdxDBTreeNode;
    procedure SetChildLoaded(Value: Boolean);
    procedure SetIsCustomDraw(Value: Boolean);
  protected
    procedure SetKeyFieldValue(Value: Variant);
    procedure SetParentFieldValue(Value: Variant);
    procedure UpdateImages;

    property Parent: TdxDBTreeNode read GetParent;
  public
    constructor Create(AOwner: TdxDBTreeNodes; Value: Variant); reintroduce; overload;
    destructor Destroy; override;
    procedure LoadChildren(ARecursive: Boolean);

    property BkColor: TColor read FBkColor write FBkColor;
    property Color: TColor read FColor write FColor;
    property FontStyle: TFontStyles read FFontStyle write FFontStyle;
    property FontName: TFontName read FFontName write FFontName;
    property IsCustomDraw: Boolean read FIsCustomDraw write SetIsCustomDraw;

    property ChildLoaded: Boolean read FChildLoaded write SetChildLoaded;
    property Index: Integer read FIndex;
    property KeyFieldValue: Variant read FKeyFieldValue;
    property ParentFieldValue: Variant read FParentFieldValue;
  end;

  TdxDBTreeNodes = class
  private
    FDBTreeView: TdxDBTreeView;
    FList: TList;
    FCustomDrawCount: Integer;
    Destroying: Boolean;

    function GetDBNodeFromIndex(Index: Integer): TdxDBTreeNode;
    function GetMaxKeyFieldValue: Variant;
  protected
    procedure BeginRefreshRecord;
    procedure EndRefreshRecord;
    function FindNearest(Value: Variant; var Index: Integer): Boolean;
    procedure Clear;
    procedure Delete(ADBTreeNode: TdxDBTreeNode);
    procedure NodeChangeParent(ATreeNode: TTreeNode; Value: Variant);
    function RefreshRecord: TdxDBTreeNode;
    procedure RefreshParents;
  public
    constructor Create(ADBTreeView: TdxDBTreeView);
    destructor Destroy; override;
    function Count: Integer;
    function GetKeyFieldValue(Value: TTreeNode): Variant;
    function GetParentValue(Value: TTreeNode): Variant;
    function GetDBTreeNode(Value: Variant): TdxDBTreeNode;
    function GetTreeNode(Value: Variant): TTreeNode;
    function IndexOf(Value: TTreeNode): Integer;
    property Items[Index: Integer]: TdxDBTreeNode read GetDBNodeFromIndex; default;
    property MaxKeyFieldValue: Variant read GetMaxKeyFieldValue;
  end;

  TAddNewDBTreeNodeEvent = procedure(Sender: TObject; var DBTreeNode: TdxDBTreeNode) of Object;
  TDBTreeRefreshNodeEvent = procedure(Sender: TObject; DBTreeNode: TdxDBTreeNode) of Object;
  TCreateNewKeyValue = procedure(Sender: TObject; var NewKeyValue: Variant) of Object;
  TSetDisplayItemText = procedure(Sender: TObject; var DisplayText: string) of Object;
  TdxDBTreeViewOption = (trDBCanDelete, trDBConfirmDelete, trCanDBNavigate,
                    trSmartRecordLoad, trSmartRecordCopy, trCheckHasChildren);
  TdxDBTreeViewOptions = set of TdxDBTreeViewOption;

  TdxDBTreeViewAction = (trDBInsert, trDBDelete, trDBChangeText, trDBChangeParent, trDBMove);
  TdxDBTreeViewActionEvent = procedure(Sender: TObject; DBNode1, DBNode2: TdxDBTreeNode;
                           Action: TdxDBTreeViewAction; var Automatic: Boolean) of Object;

  TdxDBTreeView = class(TCustomdxTreeView)
  private
    FIsRecordEditing: Boolean;
    FLockCount: Integer;

    CNNotifyFlag: Boolean;

    FDataLink: TDataLinkTreeView;
    FDBTreeNodes: TdxDBTreeNodes;
    FAddNewItem: TAddNewDBTreeNodeEvent;
    FCreateNewKeyValue: TCreateNewKeyValue;
    FOnSetDisplayItemText: TSetDisplayItemText;
    FOnInsertNodeError: TNotifyEvent;
    FRaiseOnError: Boolean;
    FOnDBAction: TdxDBTreeViewActionEvent;
    FOnRefreshNode: TDBTreeRefreshNodeEvent;

    FKeyFieldName: string;
    FListFieldName: string;
    FParentFieldName: string;
    FDisplayFieldName: string;
    FImageIndexFieldName: string;
    FStateIndexFieldName: string;
    FRootValue: Variant;
    FSeparatedSt: string;
    FOptions: TdxDBTreeViewOptions;

    FImageIndexField: TField;
    FKeyField: TField;
    FListField: TField;
    FDisplayFields: TList;
    FParentField: TField;
    FStateIndexField: TField;

    FEditInstance: Pointer;
    FDefEditProc: Pointer;
    FEditHandle: HWND;

    procedure AssignFields;
    procedure DataLinkActiveChanged;
    procedure DataLinkRecordChanged(Field: TField);
    procedure DataChanged;
    procedure RecordEdit(Field: TField);
    procedure RecordInsert(Field: TField);

    function GetDataSource: TDataSource;
    function GetSelectedDBTreeNode: TdxDBTreeNode;
    procedure Scroll;
    procedure SetDataSource(Value: TDataSource);
    procedure SetDisplayFieldName(const Value: string);
    procedure SetImageIndexFieldName(const Value: string);
    procedure SetKeyFieldName(const Value: string);
    procedure SetListFieldName(const Value: string);
    procedure SetRootValue(const Value: Variant);
    procedure SetStateIndexFieldName(const Value: string);
    procedure SetOptions(const Value: TdxDBTreeViewOptions);
    procedure SetParentFieldName(const Value: string);
    procedure SetSeparatedSt(const Value: string);

    function AddNode(AParentNode: TdxDBTreeNode; const AText: string): TdxDBTreeNode;
    procedure SetNodeParent(ANode, AParent: TdxDBTreeNode);
    function GetDisplayText: string;

    procedure DoCNNotify(var Message: TWMNotify);
    procedure DoWMPaint(var Message: TMessage);
    procedure DoVMInsertItem(var Message: TMessage);
    procedure DoVMSelectItem(var Message: TMessage);
    procedure DoVMSetItem(var Message: TMessage);
    procedure DoVMExpand(var Message: TMessage);
    procedure CNNotify(var Message: TWMNotify); message CN_NOTIFY;
    procedure VM_INSERTITEM(var Message: TMessage); message TVM_INSERTITEM;
    procedure VM_SELECTITEM(var Message: TMessage); message TVM_SELECTITEM;
    procedure VM_SETITEM(var Message: TMessage); message TVM_SETITEM;
    procedure VM_EXPAND(var Message: TMessage); message TVM_EXPAND;
    procedure WMDestroy(var Message: TWMDestroy); message WM_DESTROY;
    procedure WMPAINT(var Message: TMessage); message WM_PAINT;
    procedure WMUpdateImages(var Message: TMessage); message DXM_UPDATEIMAGES;
    procedure EditWndProc(var Message: TMessage);
    procedure FrameSelectedItem;
    procedure DeleteDBNode(AKeyValue: Variant);

    procedure InsertTreeNode(ANode, AParent: TdxDBTreeNode);
    procedure SetNewTreeNodeParent(ANode, ANewParent: TdxDBTreeNode);
  protected
    function CanExpand(Node: TTreeNode): Boolean; override;
    function DoDBAction(DBNode1, DBNode2: TdxDBTreeNode; Action: TdxDBTreeViewAction): Boolean; dynamic;

    procedure CreateWnd; override;
    procedure DestroyWnd; override;

    procedure CreateHandle; override;
    procedure Change(ANode: TTreeNode); override;
    procedure ClearItems;
    procedure DoCustomDraw(TreeNode: TTreeNode; AFont: TFont; var AColor, ABkColor: TColor); override;
    function GetListItemText(TreeNode: TTreeNode): string; override;
    procedure Edit(const Item: TTVItem); override;
    procedure InsertTreeNodeStructure(ListS, ListD: TList); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function GetNextMaxKeyValue: Variant;

    function IsUpdateLocked: Boolean;
    procedure BeginUpdate;
    procedure EndUpdate;
    function LockedLocate(const AFieldName: string; AValue: Variant; AOptions: TLocateOptions): Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CreateNode: TTreeNode; override;

    procedure GetNodeStructure(TreeNode: TTreeNode; List: TList); override;

    function GetImageIndexField: TField;
    function GetKeyField: TField;
    function GetListField: TField;
    function GetParentField: TField;
    function GetStateIndexField: TField;

    procedure GotoKeyFieldValue(Value: Variant);
    function IsCustomDraw: Boolean; override;
    procedure RefreshItems;

    property DBSelected: TdxDBTreeNode read GetSelectedDBTreeNode;
    property DBTreeNodes: TdxDBTreeNodes read FDBTreeNodes;
    property Items;
    property Selected;
  published
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DisplayField: string read FDisplayFieldName write SetDisplayFieldName;
    property ImageIndexField: string read FImageIndexFieldName write SetImageIndexFieldName;
    property KeyField: string read FKeyFieldName write SetKeyFieldName;
    property ListField: string read FListFieldName write SetListFieldName;
    property ParentField: string read FParentFieldName write SetParentFieldName;
    property RootValue: Variant read FRootValue write SetRootValue;
    property SeparatedSt: string read FSeparatedSt write SetSeparatedSt;
    property StateIndexField: string read FStateIndexFieldName write SetStateIndexFieldName;
    property RaiseOnError: Boolean read FRaiseOnError write FRaiseOnError;
    property ShowButtons;
    property BorderStyle;
    property DragCursor;
    property ShowLines;
    property ShowRoot;
    property ReadOnly;
    property DragMode;
    property HideSelection;
    property Indent;
    property OnEditing;
    property OnEdited;
    property OnExpanding;
    property OnExpanded;
    property OnCollapsing;
    property OnCompare;
    property OnCollapsed;
    property OnChanging;
    property OnChange;
    property OnDeletion;
    property OnGetImageIndex;
    property OnGetSelectedIndex;
    property Align;
    property Enabled;
    property Font;
    property Color;
    property ParentColor;
    property ParentCtl3D;
    property Ctl3D;
    property Options: TdxDBTreeViewOptions read FOptions write SetOptions default [];
    property SortType;
    property SelectedIndex;
    property TabOrder;
    property TabStop default True;
    property Visible;
    property OnAddNewItem: TAddNewDBTreeNodeEvent read FAddNewItem write FAddNewItem;
    property OnCreateNewKeyValue: TCreateNewKeyValue read FCreateNewKeyValue
                                 write FCreateNewKeyValue;
    property OnSetDisplayItemText: TSetDisplayItemText read FOnSetDisplayItemText
                                 write FOnSetDisplayItemText;
    property OnInsertNodeError: TNotifyEvent read FOnInsertNodeError write FOnInsertNodeError;
    property OnDBAction: TdxDBTreeViewActionEvent read FOnDBAction write FOnDBAction;
    property OnRefreshNode: TDBTreeRefreshNodeEvent read FOnRefreshNode write FOnRefreshNode;
    property OnClick;
    property OnEnter;
    property OnExit;
    property OnDragDrop;
    property OnDragOver;
    property OnStartDrag;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnDblClick;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property PopupMenu;
    property ParentFont;
    property ParentShowHint;
    property ShowHint;
    property Images;
    property StateImages;
    property OnContextPopup;
end;

implementation

uses
  TypInfo, Contnrs;

function VarEquals(const V1, V2: Variant): Boolean;
begin
  try
    Result := V1 = V2;
  except
    Result := False;
  end;
end;

function VarFirstMore(const V1, V2: Variant): Boolean;
begin
  try
    Result := V1 >= V2;
  except
    Result := False;
  end;
end;

function VarFirstMoreEx(const V1, V2: Variant): Boolean;
begin
  try
    Result := V1 > V2;
  except
    Result := False;
  end;
end;

{TDataLinkTreeView}
constructor TDataLinkTreeView.Create;
begin
  inherited Create;
  VisualControl := True;
end;

procedure TDataLinkTreeView.ActiveChanged;
begin
  if DataSet <> nil then
  begin
    Filter := DataSet.Filter;
    Filtered := DataSet.Filtered;
  end
  else
  begin
    Filter := '';
    Filtered := False;
  end;
  if FDBTreeView <> nil then
    FDBTreeView.DataLinkActiveChanged;
end;

procedure TDataLinkTreeView.DataSetChanged;
begin
  if (DataSet <> nil) and ((Filter <> DataSet.Filter)
    or (Filtered <> DataSet.Filtered)) then
  begin
    Filter := DataSet.Filter;
    Filtered := DataSet.Filtered;
    FDBTreeView.DataLinkActiveChanged;
  end
  else
    FDBTreeView.DataChanged;
end;

procedure TDataLinkTreeView.DataSetScrolled(Distance: Integer);
begin
  FDBTreeView.Scroll;
end;

procedure TDataLinkTreeView.RecordChanged(Field: TField);
begin
  FDBTreeView.DataLinkRecordChanged(Field);
end;

{TdxDBTreeNode}
constructor TdxDBTreeNode.Create(AOwner: TdxDBTreeNodes; Value: Variant);
var
  I: Integer;
begin
  inherited Create(AOwner.FDBTreeView.Items);
  DBTreeNodes := AOwner;

  FBkColor := clWindow;
  FColor  := DBTreeNodes.FDBTreeView.Font.Color;
  FFontStyle := DBTreeNodes.FDBTreeView.Font.Style;
  FFontName := DBTreeNodes.FDBTreeView.Font.Name;
  FIsCustomDraw := False;

  FKeyFieldValue := Value;
  DBTreeNodes.FindNearest(Value, FIndex);
  if FIndex < 0 then
    FIndex := 0;
  for I := FIndex to DBTreeNodes.FList.Count - 1 do
    Inc(DBTreeNodes[I].FIndex);
  DBTreeNodes.FList.Insert(FIndex, Self);
end;

destructor TdxDBTreeNode.Destroy;
var
  AKeyValue: Variant;
begin
  if not DBTreeNodes.Destroying and not (csDestroying in DBTreeNodes.FDBTreeView.ComponentState) then
  begin
    AKeyValue := FKeyFieldValue;
    with DBTreeNodes.FDBTreeView do
      if (FKeyField <> nil) and (FParentField <> nil) and not IsUpdateLocked
        and not CNNotifyFlag and not CopyTreeNodeStructFlag and HandleAllocated then
      begin
        if not DBTreeNodes.FDBTreeView.DoDBAction(Self, nil, trDBDelete) then
          AKeyValue := Null;
        if (trSmartRecordLoad in Options) then
          Self.LoadChildren(True);
      end;
    DBTreeNodes.FList.Remove(Self);
  end
  else
    AKeyValue := Null;
  inherited Destroy;
  if not VarIsNull(AKeyValue)then
    DBTreeNodes.FDBTreeView.DeleteDBNode(AKeyValue);
end;

procedure TdxDBTreeNode.SetKeyFieldValue(Value: Variant);
var
  ANewIndex, I: Integer;
begin
  if FKeyFieldValue <> Value then
  begin
    FKeyFieldValue := Value;
    DBTreeNodes.FindNearest(Value, ANewIndex);
    if ANewIndex <> FIndex then
    begin
      if ANewIndex > FIndex then
        for I := FIndex + 1 to ANewIndex do
          Inc(DBTreeNodes[I].FIndex)
      else
        for I := ANewIndex to FIndex - 1 do
          Dec(DBTreeNodes[I].FIndex);
      DBTreeNodes.FList.Delete(FIndex);
      FIndex := ANewIndex;
      DBTreeNodes.FList.Insert(FIndex, Self);
    end;
  end;
end;

procedure TdxDBTreeNode.SetParentFieldValue(Value: Variant);
var
  DBNode: TdxDBTreeNode;
begin
  if FParentFieldValue <> Value then
  begin
    FParentFieldValue := Value;
    DBNode := DBTreeNodes.GetDBTreeNode(Value);
    Self.MoveTo(DBNode, naAddChild);
  end;
end;

procedure TdxDBTreeNode.UpdateImages;
begin
  with DBTreeNodes.FDBTreeView do
  begin
    if FKeyFieldValue <> FKeyField.Value then Exit;
    if FImageIndexField <> nil then
    begin
      ImageIndex := 0;
      if not VarIsNull(FImageIndexField.Value) then
        ImageIndex := FImageIndexField.AsInteger;
    end;
    if FStateIndexField <> nil then
    begin
      StateIndex := -1;
      if not VarIsNull(FStateIndexField.Value) then
        StateIndex := FStateIndexField.AsInteger;
    end;
  end;
end;

procedure TdxDBTreeNode.LoadChildren(ARecursive: Boolean);
var
  ADataSet: TDataSet;
  ABookmark: TBookMark;
  ASortType: TSortType;
begin
  DBTreeNodes.FDBTreeView.BeginUpdate;
  try
    ADataSet := DBTreeNodes.FDBTreeView.FDataLink.DataSet;
    ADataSet.DisableControls;
    try
      ABookmark := ADataSet.GetBookmark;
      try
        HasChildren := DBTreeNodes.FDBTreeView.LockedLocate(DBTreeNodes.FDBTreeView.FParentFieldName, KeyFieldValue, []);
        if HasChildren then
          while not ADataSet.EOF
            and VarEquals(DBTreeNodes.FDBTreeView.FParentField.Value, KeyFieldValue) do
          begin
            if DBTreeNodes.GetDBTreeNode(DBTreeNodes.FDBTreeView.FKeyField.Value) = nil then
              DBTreeNodes.RefreshRecord;
            if ARecursive then
              DBTreeNodes.FDBTreeView.DBSelected.LoadChildren(True);
            ADataSet.Next;
          end;
        ADataSet.GotoBookmark(ABookmark);
      finally
        ADataSet.FreeBookmark(ABookmark);
      end;
    finally
      ADataSet.EnableControls;
    end;
  finally
    DBTreeNodes.FDBTreeView.EndUpdate;
  end;
  ASortType := DBTreeNodes.FDBTreeView.SortType;
  if ASortType in [stText, stData, stBoth] then
    AlphaSort;
end;

function TdxDBTreeNode.GetParent: TdxDBTreeNode;
begin
  Result := inherited Parent as TdxDBTreeNode;
end;

procedure TdxDBTreeNode.SetChildLoaded(Value: Boolean);
var
  flag: Boolean;
begin
  if Value <> FChildLoaded then
  begin
    FChildLoaded := Value;
    if Value then
      LoadChildren(False)
    else
    begin
      flag := HasChildren;
      DeleteChildren;
      HasChildren := flag;
    end;
  end;
end;

procedure TdxDBTreeNode.SetIsCustomDraw(Value: Boolean);
begin
  if FIsCustomDraw <> Value then
  begin
    FIsCustomDraw := Value;
    if Value then
      Inc(DBTreeNodes.FCustomDrawCount)
    else
      Dec(DBTreeNodes.FCustomDrawCount);
  end;
end;

{TdxDBTreeNodes}
constructor TdxDBTreeNodes.Create(ADBTreeView: TdxDBTreeView);
begin
  FDBTreeView := ADBTreeView;
  FList := TList.Create;
  FCustomDrawCount := 0;
end;

destructor TdxDBTreeNodes.Destroy;
begin
  Destroying := True;
  Clear;
  FList.Free;
  inherited Destroy;
end;

procedure TdxDBTreeNodes.BeginRefreshRecord;
Var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].Refreshed := False;
end;

procedure TdxDBTreeNodes.EndRefreshRecord;
Var
 i: Integer;
begin
  i := 0;
  while i < Count  do
    if not Items[i].Refreshed then
    begin
      Delete(Items[i]);
      i := 0;
    end
      else Inc(i);
end;

function TdxDBTreeNodes.FindNearest(Value: Variant; var Index: Integer): Boolean;
var
  Min, Max: LongInt;
begin
  Index := -1;
  Result := False;
  if (FList.Count = 0) or VarIsNull(Value)
    or VarFirstMoreEx(TdxDBTreeNode(FList.List[0]).FKeyFieldValue, Value) then
    Exit;

  if VarFirstMoreEx(Value, MaxKeyFieldValue) then
  begin
    Index := FList.Count;
    Exit;
  end;

  Min := 0;
  Max := FList.Count - 1;

  repeat
    if Max - Min = 1 then
    begin
      if Min = Index then Min := Max;
      if Max = Index then Max := Min;
    end;
    Index := Min + ((Max - Min) div 2);
    if VarEquals(Value, TdxDBTreeNode(FList.List[Index]).FKeyFieldValue) then
      Break;
    if VarFirstMore(Value, TdxDBTreeNode(FList.List[Index]).FKeyFieldValue) then
      Min := Index
    else
      Max := Index;
  until Min = Max;
  if VarEquals(Value, TdxDBTreeNode(FList.List[Index]).KeyFieldValue) then
    Result := True
  else
    if (Index < FList.Count - 1) and VarFirstMore(Value, TdxDBTreeNode(FList.List[Index]).FKeyFieldValue) then
      Inc(Index);
end;

function TdxDBTreeNodes.RefreshRecord: TdxDBTreeNode;

  procedure MoveNode(ANode, AParentNode: TdxDBTreeNode);
  begin
    if AParentNode <> nil then
    begin
      ANode.MoveTo(AParentNode, naAddChild);
      AParentNode.HasChildren := True;
    end
    else
      ANode.MoveTo(nil, naAdd);
  end;

  procedure CheckChildren(ANode: TdxDBTreeNode);
  var
    ABookmark: TBookMark;
  begin
    if trCheckHasChildren in FDBTreeView.Options then
    begin
      FDBTreeView.FDataLink.DataSet.DisableControls;
      try
        ABookmark := FDBTreeView.FDataLink.DataSet.GetBookmark;
        try
          ANode.HasChildren := FDBTreeView.LockedLocate(FDBTreeView.FParentFieldName, Result.FKeyFieldValue, []);
          FDBTreeView.FDataLink.DataSet.GotoBookmark(ABookmark);
        finally
          FDBTreeView.FDataLink.DataSet.FreeBookmark(ABookmark);
        end;
      finally
        FDBTreeView.FDataLink.DataSet.EnableControls;
      end;
    end
    else
      ANode.HasChildren := True;
  end;

var
  AParentNode: TdxDBTreeNode;
  AText: string;
begin
  Result := nil;
  if VarIsNull(FDBTreeView.FKeyField.Value) then
    Exit;

  FDBTreeView.BeginUpdate;
  try
    AText := FDBTreeView.GetDisplayText;
    Result := GetDBTreeNode(FDBTreeView.FKeyField.Value);
    AParentNode := GetDBTreeNode(FDBTreeView.FParentField.Value);
    if Result = nil then
      Result := FDBTreeView.AddNode(AParentNode, AText)
    else
    begin
      if (Result.Parent <> AParentNode) and (Result <> AParentNode) and (Result.Parent <> nil) then
        MoveNode(Result, AParentNode);
      Result.Text := AText;
    end;

    Result.FParentFieldValue := FDBTreeView.FParentField.Value;

    if trSmartRecordLoad in FDBTreeView.Options then
      CheckChildren(Result);

    Result.FListText := FDBTreeView.FListField.Text;
    Result.UpdateImages;
    Result.Refreshed := True;
    if Assigned(FDBTreeView.FOnRefreshNode) then
      FDBTreeView.FOnRefreshNode(FDBTreeView, Result);

    if (Count < 100) and (Count > FDBTreeView.FDataLink.BufferCount) then
      FDBTreeView.FDataLink.BufferCount := Count
    else
      FDBTreeView.FDataLink.BufferCount := 100;
  finally
    FDBTreeView.EndUpdate;
  end;
end;

procedure TdxDBTreeNodes.RefreshParents;
var
  i: Integer;
  ParentNode: TTreeNode;
  Node: TTreeNode;
  List: TList;
begin
  List := TList.Create;
  try
    Node := FDBTreeView.Items.GetFirstNode;
    while Node <> nil do
    begin
      List.Add(Node);
      Node := Node.GetNextSibling;
    end;

    for i := 0 to List.Count - 1  do
      if TdxDBTreeNode(List[i]).Refreshed then
      begin
        ParentNode := GetTreeNode(TdxDBTreeNode(List[i]).ParentFieldValue);
        if (ParentNode <> nil) and (ParentNode <> TdxDBTreeNode(List[i])) then
        begin
          ParentNode.HasChildren := True;
          TdxDBTreeNode(List[i]).MoveTo(ParentNode, naAddChild);
        end;
      end;
  finally
    List.Free;
  end;
end;

procedure TdxDBTreeNodes.Clear;
begin
  FDBTreeView.ClearItems;
  FList.Clear;
end;

procedure TdxDBTreeNodes.Delete(ADBTreeNode: TdxDBTreeNode);
begin
 if ADBTreeNode <> nil then
   ADBTreeNode.Free;
end;

function TdxDBTreeNodes.Count: Integer;
begin
  Result := FList.Count;
end;

function TdxDBTreeNodes.GetDBNodeFromIndex(Index: Integer): TdxDBTreeNode;
begin
  if (Index > - 1) and (Index < Count) then
    Result := TdxDBTreeNode(FList[Index])
  else
    Result := nil;
end;

function TdxDBTreeNodes.GetMaxKeyFieldValue: Variant;
begin
  Result := Null;
  if FList.Count > 0 then
    Result := Items[FList.Count - 1].KeyFieldValue;
end;

function TdxDBTreeNodes.GetKeyFieldValue(Value: TTreeNode): Variant;
begin
  if Value = nil then
    Result := Null
  else
    Result := TdxDBTreeNode(Value).KeyFieldValue;
end;

function TdxDBTreeNodes.GetParentValue(Value: TTreeNode): Variant;
begin
  if Value = nil then
    Result := Null
  else
    Result := TdxDBTreeNode(Value).ParentFieldValue;
end;

function TdxDBTreeNodes.GetDBTreeNode(Value: Variant): TdxDBTreeNode;
var
  i: Integer;
begin
  if FindNearest(Value, i) then
    Result := Items[i]
  else
    Result := nil;
end;

function TdxDBTreeNodes.GetTreeNode(Value: Variant): TTreeNode;
begin
  Result := GetDBTreeNode(Value);
end;

function TdxDBTreeNodes.IndexOf(Value: TTreeNode): Integer;
var
  i: Integer;
begin
  Result := -1;
  if Value <> nil then
    for i := 0 to Count - 1 do
      if Items[i] = Value then
      begin
        Result := i;
        Break;
      end;
end;

procedure TdxDBTreeNodes.NodeChangeParent(ATreeNode: TTreeNode; Value: Variant);
var
  AParentNode: TTreeNode;
  ANode: TdxDBTreeNode;
begin
  if ATreeNode = nil then
    Exit;
  ANode := ATreeNode as TdxDBTreeNode;
  if not VarEquals(ANode.ParentFieldValue, Value) then
    ANode.FParentFieldValue := Value;
  AParentNode := GetTreeNode(Value);
  if AParentNode = ANode.Parent then
    Exit;

  FDBTreeView.BeginUpdate;
  try
    if (AParentNode <> nil) and (ANode <> AParentNode) then
    begin
      ANode.Focused := False;
      if trSmartRecordLoad in FDBTreeView.Options then
        TdxDBTreeNode(AParentNode).ChildLoaded := True;
      AParentNode.HasChildren := True;
      ANode.MoveTo(AParentNode, naAddChild);
    end
    else
      ANode.MoveTo(nil, naAdd);

    ANode.Text := FDBTreeView.GetDisplayText;
  finally
    FDBTreeView.EndUpdate;
  end;
  FDBTreeView.Repaint;
end;

{TdxDBTreeView}
constructor TdxDBTreeView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRootValue := NULL;
  FDataLink := TDataLinkTreeView.Create;
  FDataLink.FDBTreeView := Self;
  FDBTreeNodes := TdxDBTreeNodes.Create(Self);
  FEditInstance := Classes.MakeObjectInstance(EditWndProc);
  CNNotifyFlag := False;

  FDisplayFields := TList.Create;
  FSeparatedSt := ' - ';
  Options := [trDBCanDelete, trDBConfirmDelete, trCanDBNavigate,
    trSmartRecordCopy, trCheckHasChildren];
  FRaiseOnError := True;
end;

destructor TdxDBTreeView.Destroy;
begin
  Classes.FreeObjectInstance(FEditInstance);
  FreeAndNil(FDisplayFields);
  FreeAndNil(FDBTreeNodes);
  FDataLink.FDBTreeView := nil;
  dxFreeAndNil(FDataLink);
  inherited Destroy;
end;

function TdxDBTreeView.CanExpand(Node: TTreeNode): Boolean;
begin
  Result := inherited CanExpand(Node);
  if Result and (Node is TdxDBTreeNode) and FDataLink.Active then
    (Node as TdxDBTreeNode).ChildLoaded := True;
end;

function TdxDBTreeView.DoDBAction(DBNode1, DBNode2: TdxDBTreeNode; Action: TdxDBTreeViewAction): Boolean;
begin
  Result := True;
  if Assigned(FOnDBAction) then
    FOnDBAction(Self, DBNode1, DBNode2, Action, Result);
end;

function TdxDBTreeView.GetImageIndexField: TField;
begin
  Result := FImageIndexField;
end;

function TdxDBTreeView.GetKeyField: TField;
begin
  Result := FKeyField;
end;

function TdxDBTreeView.GetListField: TField;
begin
  Result := FListField;
end;

function TdxDBTreeView.GetParentField: TField;
begin
  Result := FParentField;
end;

function TdxDBTreeView.GetStateIndexField: TField;
begin
  Result := FStateIndexField;
end;

procedure TdxDBTreeView.GotoKeyFieldValue(Value: Variant);
var
  ANode: TTreeNode;
  PValue: PVariant;
  AList: TList;
  ABookmark: TBookmark;
  i: Integer;
  AIsValueExist: Boolean;
begin
  if VarIsNull(Value) then
    Exit;

  ANode := FDBTreeNodes.GetTreeNode(Value);
  if ANode <> nil then
  begin
    ANode.MakeVisible;
    Selected := ANode;
  end
  else
    if (trSmartRecordLoad in Options) and not IsUpdateLocked then
    begin
      BeginUpdate;
      try
        if not (VarEquals(FKeyField.Value, Value)) then
          AIsValueExist := LockedLocate(FKeyFieldName, Value, [])
        else
          AIsValueExist := True;

        if AIsValueExist then
        begin
          FDataLink.DataSet.DisableControls;
          try
            ABookmark := FDataLink.DataSet.GetBookmark;
            try
              AList := TList.Create;
              try
                while (ANode = nil) do
                  if LockedLocate(FKeyFieldName, FParentField.Value, [])
                    and not (VarEquals(FKeyField.Value, FParentField.Value)) then
                  begin
                    New(PValue);
                    PValue^ := FKeyField.Value;
                    AList.Add(PValue);
                    ANode := FDBTreeNodes.GetTreeNode(PValue^);
                  end
                  else
                    Break;

                if ANode = nil then
                begin
                  DBTreeNodes.RefreshRecord;
                  ANode := DBSelected;
                  DBSelected.ChildLoaded := True;
                end;
                if ANode <> nil then
                  for i := AList.Count - 1 downto 0 do
                  begin
                    PValue := AList[i];
                    LockedLocate(FKeyFieldName, PValue^, []);
                    DBSelected.ChildLoaded := True;
                  end;
                GotoKeyFieldValue(Value);
              finally
                for i := 0 to AList.Count - 1 do
                  Dispose(PVariant(AList[i]));
                AList.Free;
              end;
              FDataLink.DataSet.GotoBookmark(ABookmark);
            finally
              FDataLink.DataSet.FreeBookmark(ABookmark);
            end;
          finally
            FDataLink.DataSet.EnableControls;
          end;
        end;
      finally
        EndUpdate;
      end;
    end;
end;

procedure TdxDBTreeView.CreateHandle;
begin
  BeginUpdate;
  try
    inherited CreateHandle;
  finally
    EndUpdate;
  end;
end;

procedure TdxDBTreeView.CreateWnd;
begin
  inherited CreateWnd;
  RefreshItems;
end;

procedure TdxDBTreeView.DestroyWnd;
begin
  BeginUpdate;
  try
    DBTreeNodes.Clear;
    inherited DestroyWnd;
  finally
    EndUpdate;
  end;
end;

procedure TdxDBTreeView.InsertTreeNode(ANode, AParent: TdxDBTreeNode);
begin
  PostMessage(Handle, DXM_UPDATEIMAGES, WPARAM(ANode), 0);

  ANode.IsInserting := False;
  if not DoDBAction(ANode, AParent, trDBInsert) then
    Exit;

  try
    BeginUpdate;
    try
      if (AParent <> nil) and (trSmartRecordLoad in FOptions) then
        AParent.ChildLoaded := True;
      SetNodeParent(ANode, AParent);

      FDataLink.DataSet.DisableControls;
      try
        FDataLink.DataSet.Append;
        FParentField.Value := ANode.FParentFieldValue;

        if (FListField <> FKeyField) and (FListField <> FParentField) then
          FListField.Text := ANode.Text;
        if FListField.IsNull then
          FListField.Text := ' ';

        if not (FKeyField is TAutoIncField) then
        begin
          if VarIsNull(FKeyField.Value) then
            FKeyField.Value := ANode.FKeyFieldValue
          else
            ANode.SetKeyFieldValue(FKeyField.Value);
        end;
        FDataLink.DataSet.Post;

        if FKeyField is TAutoIncField then
          ANode.SetKeyFieldValue(FKeyField.Value);

        ANode.Text := GetDisplayText;
      finally
        FDataLink.DataSet.EnableControls;
      end;
    finally
      EndUpdate;
    end;
    Selected := ANode;
    ANode.MakeVisible;
  except
    if Assigned(FOnInsertNodeError) then
       FOnInsertNodeError(Self)
     else
       if FRaiseOnError then
         raise;
  end;
end;

procedure TdxDBTreeView.SetNewTreeNodeParent(ANode, ANewParent: TdxDBTreeNode);
var
  AKeyValue: Variant;
begin
  if not DoDBAction(ANode, ANewParent, trDBChangeParent) then
    Exit;

  BeginUpdate;
  try
    FDataLink.DataSet.DisableControls;
    try
      if ANewParent <> nil then
        ANode.FParentFieldValue := ANewParent.FKeyFieldValue
      else
      begin
        if (trSmartRecordLoad in FOptions) and not VarIsNull(FRootValue) then
          ANode.FParentFieldValue := FRootValue
        else
          ANode.FParentFieldValue := ANode.FKeyFieldValue;
      end;
      if not VarEquals(FKeyField.Value, ANode.FKeyFieldValue) then
      begin
        AKeyValue := FKeyField.Value;
        LockedLocate(FKeyFieldName, ANode.FKeyFieldValue, []);
      end
      else
        AKeyValue := NULL;
      FDataLink.DataSet.Edit;
      FParentField.Value := ANode.FParentFieldValue;
      FDataLink.DataSet.Post;

      ANode.Text := GetDisplayText;

      if not VarIsNULL(AKeyValue) then
        LockedLocate(FKeyFieldName, AKeyValue, []);
    finally
      FDataLink.DataSet.EnableControls;
    end;
  finally
    EndUpdate;
  end;
end;

function TdxDBTreeView.CreateNode: TTreeNode;
begin
  if (FKeyField <> nil) and (FParentField <> nil) then
  begin
    if IsUpdateLocked then
      Result := TdxDBTreeNode.Create(FDBTreeNodes, FKeyField.Value)
    else
    begin
      Result := TdxDBTreeNode.Create(FDBTreeNodes, GetNextMaxKeyValue);
      TdxDBTreeNode(Result).IsInserting := True;
    end;
    if (Result <> nil) and (SelectedIndex <> -1) then
      Result.SelectedIndex := SelectedIndex;
  end
  else
    Result := nil;
end;

procedure TdxDBTreeView.Change(ANode: TTreeNode);
var
  V: Variant;
  ASelectedNode: TTreeNode;
begin
  if ANode = nil then
    Exit;
  if IsUpdateLocked then
  begin
    inherited;
    Exit;
  end;
  BeginUpdate;
  try
    if FKeyField <> nil then
    begin
      V := FDBTreeNodes.GetKeyFieldValue(ANode);
      if not VarIsNull(V) and (VarType(V) <> varEmpty)
        and not VarEquals(FKeyField.Value, V)
        and (dsInsert <> FKeyField.DataSet.State)
        and not ((FDataLink.DataSet.EOF) and (FDataLink.DataSet.BOF)) then
      begin
        ASelectedNode := nil;
        if FDataLink.DataSet.State = dsEdit then
        begin
          ANode := DBTreeNodes.GetTreeNode(FKeyField.Value);
          if ANode <> Selected then
          begin
            SendMessage(Handle, WM_SETREDRAW, WPARAM(False), 0);
            ASelectedNode := Selected;
            Selected := ANode;
          end
        end;
        LockedLocate(FKeyFieldName, V, []);
        if ASelectedNode <> nil then
        begin
          Selected := ASelectedNode;
          SendMessage(Handle, WM_SETREDRAW, WPARAM(True), 0);
        end;
      end;
    end;
  finally
    EndUpdate;
  end;
  inherited;
end;

procedure TdxDBTreeView.ClearItems;
begin
  if HandleAllocated then
    Items.BeginUpdate;
  try
    Items.Clear;
  finally
    if HandleAllocated then
      Items.EndUpdate;
  end;
end;

procedure TdxDBTreeView.Edit(const Item: TTVItem);
begin
  inherited;
  if (FListField <> nil) and (FDisplayFields.Count > 0) then
  begin
    BeginUpdate;
    try
      if (not ReadOnly) and (FListField.DataSet <> nil) and (Selected <> nil)
        and ((FListField.DataSet.State = dsEdit) or (FListField.DataSet.State = dsInsert)) and
        (FListField.Text <> Selected.Text) then
        FListField.Text := Selected.Text
      else
        if Selected.Text <> FListField.Text then
          Selected.Text := FListField.Text;
      if (Selected <> nil) and (Selected.Text <> GetDisplayText) then
        Selected.Text := GetDisplayText;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TdxDBTreeView.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and (AComponent = DataSource) then
    DataSource := nil;
end;

procedure TdxDBTreeView.RefreshItems;
var
  ABookmark: TBookMark;
  i: Integer;
  OldSortType: TSortType;
begin
  BeginUpdate;
  try
    if (FKeyField <> nil) and (FListField <> nil) and (FParentField <> nil) then
    begin
      Items.BeginUpdate;
      DBTreeNodes.BeginRefreshRecord;
      OldSortType := SortType;
      SortType := stNone;

      FDataLink.DataSet.DisableControls;
      try
        ABookmark := FDataLink.DataSet.GetBookmark;
        try
          if not (trSmartRecordLoad in Options) then
          begin
            i := mrNo;
            if (csDesigning in ComponentState) and (FDataLink.DataSet.RecordCount > 999) then
              i := MessageDlg(dxDBTreeViewSmartLoadS, mtConfirmation, [mbYes, mbNo], 0);
            if i = mrNo then
            begin
              FDataLink.DataSet.First;
              while not FDataLink.DataSet.EOF do
              begin
                FDBTreeNodes.RefreshRecord;
                FDataLink.DataSet.Next;
              end;
            end;
          end
          else
          begin
            if FDBTreeNodes.Count > 0 then
            begin
              for i := 0 to FDBTreeNodes.Count - 1 do
              begin
                FDBTreeNodes[i].FChildLoaded := False;
                if not (VarEquals(FKeyField.Value, FDBTreeNodes[i].FKeyFieldValue)) then
                  LockedLocate(FKeyFieldName, FDBTreeNodes[i].FKeyFieldValue, []);
                FDBTreeNodes.RefreshRecord;
                FDataLink.DataSet.Next;
              end;
            end
            else
              FDataLink.DataSet.First;
              if LockedLocate(FParentFieldName, FRootValue, []) then
                while (not FDataLink.DataSet.EOF) and VarEquals(FParentField.Value, FRootValue) do
                begin
                  FDBTreeNodes.RefreshRecord;
                  FDataLink.DataSet.Next;
               end;
          end;
          FDataLink.DataSet.GotoBookmark(ABookmark);
        finally
          FDataLink.DataSet.FreeBookmark(ABookmark);
        end;
      finally
        FDataLink.DataSet.EnableControls;
      end;
      Items.EndUpdate;   // Q526268
      Items.BeginUpdate; // Q526268
      FDBTreeNodes.RefreshParents;
      DBTreeNodes.EndRefreshRecord;
      Items.EndUpdate;
      SortType := OldSortType;
    end
    else
      DBTreeNodes.Clear;

    Scroll;
  finally
    EndUpdate;
  end;
end;

procedure TdxDBTreeView.AssignFields;

  function IsIntegerField(AField: TField): Boolean;
  begin
    Result := AField.DataType in [ftSmallint, ftInteger, ftWord, ftFloat, ftCurrency, ftAutoInc];
  end;

begin
  FDisplayFields.Clear;
  if FDataLink.Active then
  begin
    if FKeyFieldName <> '' then
      FKeyField := FDataLink.DataSet.FieldByName(FKeyFieldName);
    if FListFieldName <> '' then
      FListField := FDataLink.DataSet.FieldByName(FListFieldName);
    if FParentFieldName <> '' then
      FParentField := FDataLink.DataSet.FieldByName(FParentFieldName);
    if FImageIndexFieldName <> '' then
    begin
      FImageIndexField := FDataLink.DataSet.FieldByName(FImageIndexFieldName);
      if (FImageIndexField <> nil) and not IsIntegerField(FImageIndexField) then
        FImageIndexField := nil;
    end;
    if FStateIndexFieldName <> '' then
    begin
      FStateIndexField := FDataLink.DataSet.FieldByName(FStateIndexFieldName);
      if (FStateIndexField <> nil) and not IsIntegerField(FStateIndexField) then
        FStateIndexField := nil;
    end;
  {$WARNINGS OFF}
    FDataLink.DataSet.GetFieldList(FDisplayFields, FDisplayFieldName);
  {$WARNINGS ON}
  end;
end;

procedure TdxDBTreeView.DataLinkActiveChanged;
begin
  FKeyField := nil;
  FListField := nil;
  FParentField := nil;
  FImageIndexField := nil;
  FStateIndexField := nil;

  FDisplayFields.Clear;
  AssignFields;
  if (FKeyField <> nil) and not VarIsNull(FRootValue) then
    case FKeyField.DataType of
      ftSmallint: VarCast(FRootValue, FRootValue,  varSmallint);
      ftInteger, ftWord, ftAutoInc: VarCast(FRootValue, FRootValue,  varInteger);
      ftFloat, ftCurrency: VarCast(FRootValue, FRootValue,  varDouble);
    else
      VarCast(FRootValue, FRootValue,  varString);
    end;

  if HandleAllocated then
    RefreshItems;
end;

procedure TdxDBTreeView.DataLinkRecordChanged(Field: TField);
begin
  if (Field <> nil) and (Field.DataSet <> nil) then
    case Field.DataSet.State of
      dsEdit: RecordEdit(Field);
      dsInsert: RecordInsert(Field);
    end;
end;

function TdxDBTreeView.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

function TdxDBTreeView.GetSelectedDBTreeNode: TdxDBTreeNode;
begin
  Result := nil;
  if (FKeyField <> nil) and (FKeyField.Value <> NULL) then
    Result := FDBTreeNodes.GetDBTreeNode(FKeyField.Value);
end;

procedure TdxDBTreeView.Scroll;
begin
  if not (trCanDBNavigate in Options) then
    Exit;
  if FKeyField <> nil then
    GotoKeyFieldValue(FKeyField.Value);
end;

procedure TdxDBTreeView.SetDataSource(Value: TDataSource);
begin
  FDataLink.DataSource := Value;
  if Value <> nil then
    Value.FreeNotification(Self);
end;

procedure TdxDBTreeView.SetDisplayFieldName(const Value: string);
begin
  if FDisplayFieldName <> Value then
  begin
    FDisplayFieldName := Value;
    DataLinkActiveChanged;
  end;
end;

procedure TdxDBTreeView.SetImageIndexFieldName(const Value: string);
begin
  if FImageIndexFieldName <> Value then
  begin
    FImageIndexFieldName := Value;
    DataLinkActiveChanged;
  end;
end;

procedure TdxDBTreeView.SetKeyFieldName(const Value: string);
begin
  if FKeyFieldName <> Value then
  begin
    FKeyFieldName := Value;
    DataLinkActiveChanged;
  end;
end;

procedure TdxDBTreeView.SetListFieldName(const Value: string);
begin
  if FListFieldName <> Value then
  begin
    FListFieldName := Value;
    DataLinkActiveChanged;
  end;
end;

procedure TdxDBTreeView.SetStateIndexFieldName(const Value: string);
begin
  if FStateIndexFieldName <> Value then
  begin
    FStateIndexFieldName := Value;
    DataLinkActiveChanged;
  end;
end;

procedure TdxDBTreeView.SetOptions(const Value: TdxDBTreeViewOptions);
begin
  if Value <> FOptions then
  begin
    FOptions := Value;
    inherited Options := [];
    if trDBCanDelete in FOptions then
      inherited Options := inherited Options + [trCanDelete];
    if trDBConfirmDelete in FOptions then
      inherited Options := inherited Options + [trConfirmDelete];
    RefreshItems;
  end;
end;

procedure TdxDBTreeView.SetParentFieldName(const Value: string);
begin
  if FParentFieldName <> Value then
  begin
    FParentFieldName := Value;
    DataLinkActiveChanged;
  end;
end;

procedure TdxDBTreeView.SetRootValue(const Value: Variant);
begin
  FRootValue := Value;
  if trSmartRecordLoad in FOptions then
  begin
    BeginUpdate;
    try
      Items.Clear;
      DataLinkActiveChanged;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TdxDBTreeView.SetSeparatedSt(const Value: string);
begin
  if FSeparatedSt <> Value then
  begin
    FSeparatedSt := Value;
    if FDisplayFields.Count > 0 then
      DataLinkActiveChanged;
  end;
end;

procedure TdxDBTreeView.DataChanged;
begin
  if IsUpdateLocked or not HandleAllocated or (FDataLink.DataSet.State in dsEditModes) then
    Exit;
  if IsEditing and (Selected <> nil) then
    Selected.EndEdit(True);
  AssignFields;
  RefreshItems;
  Scroll;
end;

function TdxDBTreeView.AddNode(AParentNode: TdxDBTreeNode; const AText: string): TdxDBTreeNode;
begin
  if AParentNode = nil then
    Result := TdxDBTreeNode(Items.Add(nil, AText))
  else
    Result := TdxDBTreeNode(Items.AddChild(AParentNode, AText));
  if Assigned(FAddNewItem) and (FDataLink.DataSet.State = dsInsert) then
    FAddNewItem(Self, Result);
end;

procedure TdxDBTreeView.SetNodeParent(ANode, AParent: TdxDBTreeNode);
begin
  if AParent <> nil then
    ANode.FParentFieldValue := AParent.KeyFieldValue
  else
  begin
    if not VarIsNull(FRootValue) then
      ANode.FParentFieldValue := FRootValue
    else
      ANode.FParentFieldValue := ANode.FKeyFieldValue;
  end;
end;

function TdxDBTreeView.GetDisplayText: string;
var
  i: Integer;
begin
  Result := '';
  if FDisplayFields.Count > 0 then
  begin
    for i := 0 to FDisplayFields.Count - 1 do
    begin
      if i > 0 then
        Result := Result + FSeparatedSt;
      Result := Result + TField(FDisplayFields[i]).Text;
    end;
  end
  else
    if FListField <> nil then
      Result := FListField.Text;
  if Assigned(FOnSetDisplayItemText) then
    FOnSetDisplayItemText(Self, Result);
end;

procedure TdxDBTreeView.RecordEdit(Field: TField);

  procedure RepaintNode(ANode: TTreeNode);
  begin
    if HandleAllocated and (ANode <> nil) and ANode.IsVisible then
      cxInvalidateRect(Handle, ANode.DisplayRect(False), False);
  end;

var
  ANode: TTreeNode;
  V: Variant;
begin
  if not FIsRecordEditing and (FKeyField <> nil) then
  begin
    ANode := Selected;
    if ANode = nil then
      Exit;

    if (Field = FListField) and (ANode.Text <> Field.Text) then
      ANode.Text := Field.Text;

    if FImageIndexField = Field then
    begin
      if not VarIsNull(FImageIndexField.Value) then
        ANode.ImageIndex := FImageIndexField.AsInteger
      else
        ANode.ImageIndex := -1;
      RepaintNode(ANode);
    end;

    if FStateIndexField = Field then
    begin
      if not VarIsNull(FStateIndexField.Value) then
        ANode.StateIndex := FStateIndexField.AsInteger
      else
        ANode.StateIndex := -1;
      RepaintNode(ANode);
    end;

    if Field = FParentField then
    begin
      V := FDBTreeNodes.GetParentValue(ANode);
      if not VarIsNull(Field.Value) and not VarIsNull(V) and not VarEquals(Field.Value, V) then
      begin
        try
          if trSmartRecordLoad in FOptions then
          begin
            BeginUpdate;
            FIsRecordEditing := True;
            try
              FDataLink.DataSet.Post;
            finally
              FIsRecordEditing := False;
              EndUpdate;
            end;
          end;
          FDBTreeNodes.NodeChangeParent(ANode, Field.Value);
        except
          Field.Value := V;
        end;
      end;
    end;
  end;
end;

procedure TdxDBTreeView.RecordInsert(Field: TField);
var
  DBTreeNode: TdxDBTreeNode;
begin
  if (FKeyField <> nil) and (FKeyField.Value <> NULL) then
  begin
    if not IsUpdateLocked then
      FDBTreeNodes.RefreshRecord;
    DBTreeNode := DBSelected;
    if DBTreeNode <> nil then
    begin
      Selected := DBTreeNode;
      if Selected <> nil then
        Selected.MakeVisible;
    end;
  end;
end;

procedure TdxDBTreeView.FrameSelectedItem;
var
  ARect: TRect;
  AColor, ABkColor: TColor;
begin
  if not Focused and not IsUpdateLocked and (Selected <> nil) then
  begin
    if HandleAllocated then
      DoCustomDraw(Selected, Font, AColor, ABkColor);

    TreeView_GetItemRect(Selected.Handle, Selected.ItemId, ARect, True);

    cxPaintCanvas.BeginPaint(Canvas);
    try
      cxPaintCanvas.FrameRect(ARect, Font.Color);
    finally
      cxPaintCanvas.EndPaint;
    end;
  end;
end;

procedure TdxDBTreeView.DeleteDBNode(AKeyValue: Variant);
var
  AParentForm: TCustomForm;
  ALocate: Boolean;
begin
  if (FKeyField = nil) or (FParentField = nil) or IsUpdateLocked or CNNotifyFlag
    or CopyTreeNodeStructFlag or not HandleAllocated then
    Exit;
  AParentForm := GetParentForm(Self);
  if (AParentForm <> nil) and (csDestroying in AParentForm.ComponentState) then
    Exit;

  BeginUpdate;
  try
    FDataLink.DataSet.DisableControls;
    try
      if not (FDataLink.DataSet.EOF and FDataLink.DataSet.BOF) then
      begin
        try
          ALocate := FDataLink.DataSet.Locate(FKeyFieldName, AKeyValue, []);
        except
          ALocate := False;
        end;
        if ALocate and VarEquals(AKeyValue, FKeyField.Value) then
          FDataLink.DataSet.Delete;
      end;
    finally
      FDataLink.DataSet.EnableControls;
    end;
  finally
    EndUpdate;
  end;
  Change(Selected);
end;

procedure TdxDBTreeView.DoCustomDraw(TreeNode: TTreeNode; AFont: TFont;
  var AColor, ABkColor: TColor);
begin
  if Assigned(OnCustomDraw) then
    inherited DoCustomDraw(TreeNode, AFont, AColor, ABkColor)
  else
  begin
    if TdxDBTreeNode(TreeNode).IsCustomDraw then
      with TdxDBTreeNode(TreeNode) do
      begin
        AFont.Style := FontStyle;
        AFont.Name := FontName;
        AColor := Color;
        ABkColor := BkColor;
      end;
  end;
end;

procedure TdxDBTreeView.GetNodeStructure(TreeNode: TTreeNode; List: TList);
begin
  if (trSmartRecordLoad in FOptions) and CopyTreeNodeStructFlag then
    TdxDBTreeNode(TreeNode).LoadChildren(True);
  inherited GetNodeStructure(TreeNode, List);
end;

type
  TInsertFieldStruct = class
  private
    Buffer: Pointer;
    Stream: TStream;
    FieldName: string;
  public
    constructor Create(Size: Integer);
    destructor Destroy; override;
  end;

constructor TInsertFieldStruct.Create(Size: Integer);
begin
  inherited Create;
  GetMem(Buffer, Size);
end;

destructor TInsertFieldStruct.Destroy;
begin
  FreeMem(Buffer);
  FreeAndNil(Stream);
  inherited Destroy;
end;

function TdxDBTreeView.GetNextMaxKeyValue: Variant;
begin
  Result := DBTreeNodes.MaxKeyFieldValue;
  if Assigned(FCreateNewKeyValue) then
    FCreateNewKeyValue(nil, Result)
  else
    try
      if not VarIsNULL(Result) then
        Result := Result + 1
      else
        Result := 0;
    except
    end;
end;

function TdxDBTreeView.IsUpdateLocked: Boolean;
begin
  Result := FLockCount <> 0;
  if FLockCount < 0 then
    raise EDBTreeViewError.Create('TdxDBTreeView.IsUpdateLocked fails');
end;

procedure TdxDBTreeView.BeginUpdate;
begin
  Inc(FLockCount);
end;

procedure TdxDBTreeView.EndUpdate;
begin
  Dec(FLockCount);
end;

function TdxDBTreeView.LockedLocate(const AFieldName: string; AValue: Variant; AOptions: TLocateOptions): Boolean;
begin
  BeginUpdate;
  try
    Result := FDataLink.DataSet.Locate(AFieldName, AValue, AOptions)
  finally
    EndUpdate;
  end;
end;

procedure TdxDBTreeView.InsertTreeNodeStructure(ListS, ListD: TList);

  function IsBlobField(AField: TField): Boolean;
  begin
    Result := AField is TBlobField;
  end;

  procedure AddFieldStruct(AField: TField; AList: TList);
  var
    AFieldStruct: TInsertFieldStruct;
    AStream: TStream;
  begin
    AFieldStruct := TInsertFieldStruct.Create(AField.DataSize + 1);
    AList.Add(AFieldStruct);
    AFieldStruct.FieldName := AField.FieldName;
    if IsBlobField(AField) then
    begin
      AStream := AField.DataSet.CreateBlobStream(AField, bmRead);
      try
        AFieldStruct.Stream := TMemoryStream.Create;
        AFieldStruct.Stream.CopyFrom(AStream, AStream.Size);
      finally
        AStream.Free;
      end;
    end
    else
    {$WARNINGS OFF}
      if not AField.GetData(AFieldStruct.Buffer) then
    {$WARNINGS ON}
        AList.Remove(AFieldStruct);
  end;

  procedure UpdateFieldFromStruct(AFieldStruct: TInsertFieldStruct);
  var
    AStream: TStream;
  begin
    if AFieldStruct.Stream <> nil then
    begin
      AStream := FDataLink.DataSet.CreateBlobStream(FDataLink.DataSet.FindField(AFieldStruct.FieldName), bmWrite);
      try
        AFieldStruct.Stream.Position := 0;
        AStream.CopyFrom(AFieldStruct.Stream, AFieldStruct.Stream.Size);
      finally
        AStream.Free;
      end;
    end
    else
    {$WARNINGS OFF}
      FDataLink.DataSet.FindField(AFieldStruct.FieldName).SetData(AFieldStruct.Buffer);
    {$WARNINGS ON}
  end;

var
  i, AIndex: Integer;
  MaxKeyFieldValue: Variant;
  ADestNode, ASourceNode: TdxDBTreeNode;
  AList: TList;
  ASourceDBTreeView: TdxDBTreeView;
  ASourceDBTreeKeyValue: Variant;
  FCopiedFields: TList;
  AField: TField;
begin
  if not DoDBAction(TdxDBTreeNode(ListS[0]), TdxDBTreeNode(ListD[0]), trDBMove) then
    Exit;

  if (ListS.Count > 0) and (TTreeNode(ListS[0]).TreeView is TdxDBTreeView) then
    ASourceDBTreeView := TdxDBTreeView(TTreeNode(ListS[0]).TreeView)
  else
    ASourceDBTreeView := nil;
  BeginUpdate;
  try
    FDataLink.DataSet.DisableControls;
    try
      if (ListD.Count > 0) and (trSmartRecordLoad in FOptions) then
      begin
        ADestNode := TdxDBTreeNode(ListD[0]);
        if (ADestNode <> nil) and (ADestNode.Parent <> nil) then
          ADestNode.Parent.ChildLoaded := True;
      end;
      ASourceDBTreeKeyValue := varNull;
      FCopiedFields := TList.Create;
      if (trSmartRecordCopy in FOptions) and (ListS.Count > 0) and (ASourceDBTreeView <> nil) then
      begin
        ASourceDBTreeKeyValue := ASourceDBTreeView.FKeyField.Value;
        for AIndex := 0 to ASourceDBTreeView.FDataLink.DataSet.FieldCount - 1 do
        begin
          AField := ASourceDBTreeView.FDataLink.DataSet.Fields[AIndex];
          if (CompareText(AField.FieldName, FKeyFieldName) <> 0)
            and (CompareText(AField.FieldName, FParentFieldName) <> 0)
            and (FDataLink.DataSet.FindField(AField.FieldName) <> nil) then
            FCopiedFields.Add(AField);
        end;
      end;

      MaxKeyFieldValue := DBTreeNodes.MaxKeyFieldValue;
      for i := 0 to ListD.Count - 1 do
      begin
        ADestNode := TdxDBTreeNode(ListD[i]);
        ASourceNode := TdxDBTreeNode(ListS[i]);

        SetNodeParent(ADestNode, ADestNode.Parent);

        if (FDisplayFields.Count > 0) and (ASourceDBTreeView = Self) and (ASourceNode <> nil) then
        begin
          LockedLocate(FKeyFieldName, ASourceNode.FKeyFieldValue, []);
          ADestNode.Text := FListField.Text;
        end;

        FDataLink.DataSet.Append;
        FParentField.Value := ADestNode.FParentFieldValue;
        if (FListField <> FKeyField) and (FListField <> FParentField) then
          FListField.Value := ADestNode.Text;
        if FKeyField.DataType <> ftAutoInc then
          FKeyField.Value := ADestNode.FKeyFieldValue;
        FDataLink.DataSet.Post;
        if (FKeyField.DataType = ftAutoInc) and not FKeyField.IsNull then
          ADestNode.FKeyFieldValue := FKeyField.Value;

        if FCopiedFields.Count > 0 then
        begin
          AList := TObjectList.Create;
          try
            if ASourceNode <> nil then
            begin
              ASourceDBTreeView.LockedLocate(ASourceDBTreeView.FKeyFieldName, ASourceNode.FKeyFieldValue, []);
              for AIndex := 0 to FCopiedFields.Count - 1 do
                AddFieldStruct(TField(FCopiedFields[AIndex]), AList);

              if AList.Count > 0 then
                try
                  LockedLocate(FKeyFieldName, ADestNode.FKeyFieldValue, []);
                  FDataLink.DataSet.Edit;
                  for AIndex := 0 to AList.Count - 1 do
                    UpdateFieldFromStruct(TInsertFieldStruct(AList[AIndex]));
                  FDataLink.DataSet.Post;
                except
                end;
            end;
          finally
            AList.Free;
          end;
        end;

        ADestNode.Text := GetDisplayText;
      end;
      ADestNode := TdxDBTreeNode(ListD[0]);
      if (ADestNode <> nil) and (ADestNode.Parent <> nil) then
        LockedLocate(FKeyFieldName, ADestNode.Parent.FKeyFieldValue, []);

      if (ASourceDBTreeView <> nil) and (ASourceDBTreeView <> Self) then
        ASourceDBTreeView.LockedLocate(ASourceDBTreeView.FKeyFieldName, ASourceDBTreeKeyValue, []);

      FCopiedFields.Free;
    finally
      FDataLink.DataSet.EnableControls;
    end
  finally
    EndUpdate;
  end;
end;

function TdxDBTreeView.IsCustomDraw: Boolean;
begin
  Result := (inherited IsCustomDraw) or (FDBTreeNodes.FCustomDrawCount > 0);
end;

function TdxDBTreeView.GetListItemText(TreeNode: TTreeNode): string;
begin
  if FDisplayFields.Count > 0 then
    Result := TdxDBTreeNode(TreeNode).FListText
  else
    Result := inherited GetListItemText(TreeNode);
end;

procedure TdxDBTreeView.DoCNNotify(var Message: TWMNotify);
var
  TreeNode: TTreeNode;
  dbtr: TdxDBTreeNode;
  OldCNNotifyFlag: Boolean;
begin
  if trSmartRecordLoad in FOptions then
    with Message.NMHdr^, PNMTreeView(Pointer(Message.NMHdr))^do
    begin
      if (code = TVN_ITEMEXPANDING) and (Action = TVE_EXPAND) then
      begin
        dbtr := TdxDBTreeNode(GetNodeFromItem(ItemNew));
        if dbtr <> nil then
        begin
          if CanExpand(dbtr) then
            dbtr.ChildLoaded := True
          else
            Message.Result := 1;
        end;
        Exit;
      end;
      if (code = TVN_ITEMEXPANDED) and (action = TVE_COLLAPSE) then
      begin
        inherited;
        dbtr := TdxDBTreeNode(GetNodeFromItem(ItemNew));
        if dbtr <> nil then
        begin
          OldCNNotifyFlag := CNNotifyFlag;
          CNNotifyFlag := True;
          dbtr.ChildLoaded := False;
          CNNotifyFlag := OldCNNotifyFlag;
        end;
        Exit;
      end;
    end;

  with Message.NMHdr^ do
  begin
    if (code = TVN_BEGINLABELEDIT) and (FListField <> nil) then
    begin
      with PTVDispInfo(Pointer(Message.NMHdr))^do
        TreeNode := GetNodeFromItem(item);
      if TreeNode.Text <> FListField.Text then
      begin
        TreeNode.Text := FListField.Text;
        SendMessage(TreeView_GetEditControl(handle), WM_SETTEXT, 0, LPARAM(PChar(TreeNode.Text)));
      end;
      if FDataLink.DataSet.CanModify then
        FDataLink.Edit;
      FEditHandle := TreeView_GetEditControl(Handle);
      SendMessage(FEditHandle, EM_SETLIMITTEXT, FListField.DisplayWidth, 0);
      FDefEditProc := dxSetWindowProc(FEditHandle, FEditInstance);
    end;
  end;
end;

procedure TdxDBTreeView.DoVMInsertItem(var Message: TMessage);
var
  ANode, AOldNode, ANewNode: TTreeNode;
  AStructure: TTVInsertStruct;
begin
  if not IsUpdateLocked then
  begin
    ANode := Items.GetNode(HTreeItem(Message.Result));
    if ANode <> nil then
    begin
      AStructure := PTVInsertStruct(Message.lParam)^;
      ANewNode := Items.GetNode(AStructure.hParent);
      if TdxDBTreeNode(ANode).IsInserting then
      begin
        if not CopyTreeNodeStructFlag then
          InsertTreeNode(TdxDBTreeNode(ANode), TdxDBTreeNode(ANewNode));
        TdxDBTreeNode(ANode).IsInserting := False;
      end
      else
      begin
        AOldNode := DBTreeNodes.GetTreeNode(TdxDBTreeNode(ANode).FParentFieldValue);
        if ANewNode <> AOldNode then
          SetNewTreeNodeParent(TdxDBTreeNode(ANode), TdxDBTreeNode(ANewNode));
      end;
    end;
  end;
end;

procedure TdxDBTreeView.DoVMSelectItem(var Message: TMessage);
begin
  FrameSelectedItem;
end;

procedure TdxDBTreeView.DoVMSetItem(var Message: TMessage);
var
  ANode: TTreeNode;
begin
  if not IsUpdateLocked and not CopyTreeNodeStructFlag and not IsEditing
    and (PTVITEM(Message.LParam)^.mask and TVIF_TEXT = 1)
    and (PTVITEM(Message.LParam)^.hItem <> nil)
    and (not ReadOnly) and (FListField <> nil) and (FDisplayFields.Count = 0)
    and (FDataLink.DataSet.CanModify) and not FListField.ReadOnly then
  begin
    FDataLink.DataSet.DisableControls;
    try
      SendMessage(Handle, WM_SETREDRAW, WPARAM(False), 0);
      try
        ANode := Selected;
        TreeView_SelectItem(Handle, PTVITEM(Message.LParam)^.hItem);

        BeginUpdate;
        try
          if (FListField.Text <> Selected.Text) and DoDBAction(TdxDBTreeNode(selected), nil, trDBChangeText) then
          begin
            FDataLink.DataSet.Edit;
            FListField.Text := Selected.Text;
            FDataLink.DataSet.Post;
          end;
          Selected := ANode;
        finally
          EndUpdate;
        end;
      finally
        SendMessage(Handle, WM_SETREDRAW, WPARAM(True), 0);
      end;
    finally
      FDataLink.DataSet.EnableControls;
    end;
  end;
end;

procedure TdxDBTreeView.DoVMExpand(var Message: TMessage);
begin
  if (Message.wParam = TVE_COLLAPSE) and (Selected <> nil) then
    Change(Selected);
end;

procedure TdxDBTreeView.DoWMPaint(var Message: TMessage);
begin
  if not Focused and (Selected <> nil) and (Selected.IsVisible) and not IsEditing then
    FrameSelectedItem;
end;

procedure TdxDBTreeView.CNNotify(var Message: TWMNotify);
begin
  DoCNNotify(Message);
  inherited;
end;

procedure TdxDBTreeView.VM_INSERTITEM(var Message: TMessage);
begin
  inherited;
  DoVMInsertItem(Message);
end;

procedure TdxDBTreeView.VM_SELECTITEM(var Message: TMessage);
begin
  inherited;
  DoVMSelectItem(Message);
end;

procedure TdxDBTreeView.VM_SETITEM(var Message: TMessage);
begin
  inherited;
  DoVMSetItem(Message);
end;

procedure TdxDBTreeView.VM_EXPAND(var Message: TMessage);
begin
  inherited;
  DoVMExpand(Message);
end;

procedure TdxDBTreeView.WMDestroy(var Message: TWMDestroy);
begin
  BeginUpdate;
  try
    if FDBTreeNodes <> nil then
      FDBTreeNodes.Clear;
    inherited;
  finally
    EndUpdate;
  end;
end;

procedure TdxDBTreeView.WMPaint(var Message: TMessage);
begin
  inherited;
  DoWMPaint(Message);
end;

procedure TdxDBTreeView.WMUpdateImages(var Message: TMessage);
var
  ANode: TdxDBTreeNode;
begin
  ANode := TdxDBTreeNode(Message.WParam);
  if ANode is TdxDBTreeNode then
    ANode.UpdateImages;
end;

procedure TdxDBTreeView.EditWndProc(var Message: TMessage);
begin
  with Message do
  begin
    case Msg of
      WM_CHAR:
        if not (FDataLink.DataSet.CanModify) or FListField.ReadOnly then
          Exit
        else
          if not FListField.IsValidChar(Char(wParam)) then
          begin
            MessageBeep(0);
            Exit;
          end;
    end;
    Result := CallWindowProc(FDefEditProc, FEditHandle, Msg, WParam, LParam);
  end;
end;

end.

