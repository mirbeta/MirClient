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

unit cxShellTreeView;

{$I cxVer.inc}

interface

uses
  Windows, Messages, Classes, ComCtrls, CommCtrl, Controls, Forms, Graphics,
  ImgList, Menus, ShlObj, StdCtrls,
  cxContainer, cxDataUtils, cxEdit, cxTreeView, cxShellListView, cxShellCommon, cxShellControls;

type
  TcxCustomShellTreeView = class;

  { TcxInnerShellTreeView }

  TcxInnerShellTreeView = class(TcxCustomInnerShellTreeView)
  private
    FAbsolutePIDL: PItemIDList;
    FIndent: Integer;
    function GetAbsolutePIDL: PItemIDList;
    function GetContainer: TcxCustomShellTreeView;
    function GetIndent: Integer;
    function GetPath: string;
    procedure SaveAbsolutePIDL(AValue: PItemIDList);
    procedure SetAbsolutePIDL(AValue: PItemIDList);
    procedure SetIndent(const Value: Integer);
    procedure SetPath(AValue: string);
    procedure DSMShellChangeNotify(var Message: TMessage); message DSM_SHELLCHANGENOTIFY;
    procedure TVMEnsureVisible(var Message: TMessage); message TVM_ENSUREVISIBLE;
  protected
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    function IsDragDropEnabled: Boolean; override;
    function IsLoading: Boolean; override;
    procedure CreateWindowHandle(const Params: TCreateParams); override;

    property AbsolutePIDL: PItemIDList read GetAbsolutePIDL write SetAbsolutePIDL;
    property Container: TcxCustomShellTreeView read GetContainer;
    property Path: string read GetPath write SetPath;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;

    property Cursor;
    property OnAddFolder;
    property OnShellChange;
    property Indent: Integer read GetIndent write SetIndent;
  end;

  { TcxCustomShellTreeView }

  TcxCustomShellTreeView = class(TcxTreeViewContainer, IcxShellDependedControls, IcxShellRoot)
  private
    FIsExitProcessing: Boolean;
    FOnAddFolder: TcxShellAddFolderEvent;
    FOnChange: TTVChangedEvent;
    FOnChanging: TTVChangingEvent;
    FOnCollapsed: TTVExpandedEvent;
    FOnCollapsing: TTVCollapsingEvent;
    FOnEdited: TTVEditedEvent;
    FOnEditing: TTVEditingEvent;
    FOnExpanded: TTVExpandedEvent;
    FOnExpanding: TTVExpandingEvent;
    FOnShellChange: TcxShellChangeEvent;

    procedure AddFolderHandler(Sender: TObject; AFolder: TcxShellFolder;
      var ACanAdd: Boolean);
    procedure ChangeHandler(Sender: TObject; Node: TTreeNode);
    procedure ChangingHandler(Sender: TObject; Node: TTreeNode;
      var AllowChange: Boolean);
    procedure CollapsedHandler(Sender: TObject; Node: TTreeNode);
    procedure CollapsingHandler(Sender: TObject; Node: TTreeNode;
      var AllowCollapse: Boolean);
    procedure EditedHandler(Sender: TObject; Node: TTreeNode; var S: string);
    procedure EditingHandler(Sender: TObject; Node: TTreeNode;
      var AllowEdit: Boolean);
    procedure ExpandedHandler(Sender: TObject; Node: TTreeNode);
    procedure ExpandingHandler(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
    procedure ShellChangeHandler(Sender: TObject; AEventID: DWORD;
      APIDL1, APIDL2: PItemIDList);

    function GetAbsolutePIDL: PItemIDList;
    function GetDragDropSettings: TcxDragDropSettings;
    function GetFolder(AIndex: Integer): TcxShellFolder;
    function GetFolderCount: Integer;
    function GetIndent: Integer;
    function GetInnerTreeView: TcxInnerShellTreeView;
    function GetOptions: TcxShellTreeViewOptions;
    function GetPath: string;
    function GetRoot: TcxShellTreeRoot;
    function GetShellListView: TcxCustomShellListView;
    function GetShowInfoTips: Boolean;
    function GetTreeHotTrack: Boolean;
    procedure SetAbsolutePIDL(Value: PItemIDList);
    procedure SetDragDropSettings(Value: TcxDragDropSettings);
    procedure SetIndent(Value: Integer);
    procedure SetOptions(Value: TcxShellTreeViewOptions);
    procedure SetPath(const Value: string);
    procedure SetRoot(Value: TcxShellTreeRoot);
    procedure SetShellListView(Value: TcxCustomShellListView);
    procedure SetShowInfoTips(Value: Boolean);
    procedure SetTreeHotTrack(Value: Boolean);
  protected
    FDataBinding: TcxCustomDataBinding;
    procedure DoExit; override;
    procedure Loaded; override;
    procedure Scroll(AScrollBarKind: TScrollBarKind; AScrollCode: TScrollCode;
      var AScrollPos: Integer); override;
    procedure CurrentFolderChangedHandler(Sender: TObject; Root: TcxCustomShellRoot); virtual;
    function GetDataBindingClass: TcxCustomDataBindingClass; virtual;
    function GetInnerTreeViewClass: TcxInnerTreeViewClass; override;
    procedure InitializeInternalTreeView; override;
    function GetViewOptions(AForNavigation: Boolean = False): TcxShellViewOptions;
    function IsDragAndDropSupported: Boolean; virtual;
    procedure WndProc(var Message: TMessage); override;
    // IcxShellDependedControls
    function GetDependedControls: TcxShellDependedControls;
    // IcxShellRoot
    function IcxShellRoot.GetRoot = GetShellRoot;
    function GetShellRoot: TcxCustomShellRoot;

    property DataBinding: TcxCustomDataBinding read FDataBinding;
    property DragDropSettings: TcxDragDropSettings read GetDragDropSettings write SetDragDropSettings;
    property Indent: Integer read GetIndent write SetIndent;
    property Options: TcxShellTreeViewOptions read GetOptions write SetOptions;
    property Root: TcxShellTreeRoot read GetRoot write SetRoot;
    property ShellListView: TcxCustomShellListView read GetShellListView write SetShellListView;
    property ShowInfoTips: Boolean read GetShowInfoTips write SetShowInfoTips default False;
    property TreeHotTrack: Boolean read GetTreeHotTrack write SetTreeHotTrack default False;
    property OnAddFolder: TcxShellAddFolderEvent read FOnAddFolder write FOnAddFolder;
    property OnChange: TTVChangedEvent read FOnChange write FOnChange;
    property OnChanging: TTVChangingEvent read FOnChanging write FOnChanging;
    property OnCollapsed: TTVExpandedEvent read FOnCollapsed write FOnCollapsed;
    property OnCollapsing: TTVCollapsingEvent read FOnCollapsing write FOnCollapsing;
    property OnEdited: TTVEditedEvent read FOnEdited write FOnEdited;
    property OnEditing: TTVEditingEvent read FOnEditing write FOnEditing;
    property OnExpanded: TTVExpandedEvent read FOnExpanded write FOnExpanded;
    property OnExpanding: TTVExpandingEvent read FOnExpanding write FOnExpanding;
    property OnShellChange: TcxShellChangeEvent read FOnShellChange write FOnShellChange;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    procedure SetFocus; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    function GetNodeAbsolutePIDL(ANode: TTreeNode): PItemIDList;
    procedure UpdateContent;
    property AbsolutePath: string read GetPath write SetPath; // deprecated;
    property AbsolutePIDL: PItemIDList read GetAbsolutePIDL write SetAbsolutePIDL;
    property FolderCount: Integer read GetFolderCount;
    property Folders[AIndex: Integer]: TcxShellFolder read GetFolder;
    property InnerTreeView: TcxInnerShellTreeView read GetInnerTreeView;
    property Path: string read GetPath write SetPath;
//    property RelativePIDL: PItemIDList write SetRelativePIDL; // TODO
  end;

  { TcxShellTreeView }

  TcxShellTreeView = class(TcxCustomShellTreeView)
  published
    property Anchors;
    property AutoExpand;
    property BiDiMode;
    property ChangeDelay;
    property Constraints;
    property DragDropSettings;
    property Enabled;
    property HideSelection;
    property Indent;
    property Options;
    property ParentBiDiMode;
    property ParentColor default False;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property RightClickSelect;
    property Root;
    property ShellListView;
    property ShowButtons;
    property ShowHint;
    property ShowInfoTips;
    property ShowLines;
    property ShowRoot;
    property StateImages;
    property Style;
    property StyleDisabled;
    property StyleFocused;
    property StyleHot;
    property TabOrder;
    property TabStop;
    property TreeHotTrack;
    property Visible;
    property OnAddFolder;
    property OnChange;
    property OnChanging;
    property OnClick;
    property OnCollapsed;
    property OnCollapsing;
    property OnDblClick;
    property OnEdited;
    property OnEditing;
    property OnEnter;
    property OnExit;
    property OnExpanded;
    property OnExpanding;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnShellChange;
  end;

implementation

uses
  SysUtils, cxClasses, cxScrollBar;

type
  TcxContainerAccess = class(TcxContainer);
  TcxCustomDataBindingAccess = class(TcxCustomDataBinding);
  TcxInnerShellListViewAccess = class(TcxInnerShellListView);
  TcxShellTreeItemProducerAccess = class(TcxShellTreeItemProducer);

{ TcxInnerShellTreeView }

constructor TcxInnerShellTreeView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIndent := -1;
end;

destructor TcxInnerShellTreeView.Destroy;
begin
  SaveAbsolutePIDL(nil);
  inherited Destroy;
end;

function TcxInnerShellTreeView.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or
    TcxCustomDataBindingAccess(Container.FDataBinding).ExecuteAction(Action);
end;

function TcxInnerShellTreeView.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or
    TcxCustomDataBindingAccess(Container.FDataBinding).UpdateAction(Action);
end;

procedure TcxInnerShellTreeView.CreateWnd;
begin
  inherited CreateWnd;
  if FIndent <> -1 then
    Indent := FIndent;
  if FAbsolutePIDL <> nil then
    AbsolutePIDL := FAbsolutePIDL;
end;

procedure TcxInnerShellTreeView.DestroyWnd;
var
  ATempPIDL: PItemIDList;
begin
  ATempPIDL := AbsolutePIDL;
  try
    SaveAbsolutePIDL(ATempPIDL);
  finally
    DisposePidl(ATempPIDL);
  end;
  FIndent := Indent;
  inherited DestroyWnd;
end;

function TcxInnerShellTreeView.IsDragDropEnabled: Boolean;
begin
  Result := Container.IsDragAndDropSupported and inherited IsDragDropEnabled;
end;

function TcxInnerShellTreeView.IsLoading: Boolean;
begin
  Result := csLoading in Container.ComponentState;
end;

procedure TcxInnerShellTreeView.CreateWindowHandle(const Params: TCreateParams);
begin
  inherited CreateWindowHandle(Params);
  Container.SetScrollBarsParameters;
end;

function TcxInnerShellTreeView.GetAbsolutePIDL: PItemIDList;
begin
  if HandleAllocated then
  begin
    Result := nil;
    if Selected <> nil then
      Result := Container.GetNodeAbsolutePIDL(Selected);
  end
  else
    Result := GetPidlCopy(FAbsolutePIDL);
end;

function TcxInnerShellTreeView.GetContainer: TcxCustomShellTreeView;
begin
  Result := TcxCustomShellTreeView(FContainer);
end;

function TcxInnerShellTreeView.GetIndent: Integer;
begin
  if HandleAllocated then
    Result := inherited Indent
  else
    Result := FIndent;
end;

function TcxInnerShellTreeView.GetPath: string;
var
  ATempPIDL: PItemIDList;
begin
  ATempPIDL := AbsolutePIDL;
  try
    Result := GetPidlName(ATempPIDL);
  finally
    DisposePidl(ATempPIDL);
  end;
end;

procedure TcxInnerShellTreeView.SaveAbsolutePIDL(AValue: PItemIDList);
var
  ATempPIDL: PItemIDList;
begin
  ATempPIDL := GetPidlCopy(AValue);
  DisposePidl(FAbsolutePIDL);
  FAbsolutePIDL := ATempPIDL;
end;

procedure TcxInnerShellTreeView.SetAbsolutePIDL(AValue: PItemIDList);
begin
  if HandleAllocated then
  begin
    if CheckAbsolutePIDL(AValue, Root, True) then
    begin
      SendMessage(Handle, DSM_DONAVIGATE, WPARAM(AValue), 0);
      DoNavigateListView;
    end;
  end
  else
    SaveAbsolutePIDL(AValue);
end;

procedure TcxInnerShellTreeView.SetIndent(const Value: Integer);
begin
  if HandleAllocated then
    inherited Indent := Value
  else
    FIndent := Value;
end;

procedure TcxInnerShellTreeView.SetPath(AValue: string);
var
  APIDL: PItemIDList;
begin
  APIDL := PathToAbsolutePIDL(AValue, Root, Container.GetViewOptions(True));
  if APIDL <> nil then
    try
      AbsolutePIDL := APIDL;
    finally
      DisposePidl(APIDL);
    end;
end;

procedure TcxInnerShellTreeView.DSMShellChangeNotify(var Message: TMessage);
begin
  inherited;
  Container.SetScrollBarsParameters;
end;

procedure TcxInnerShellTreeView.TVMEnsureVisible(var Message: TMessage);
begin
  inherited;
  UpdateVisibleItems;
end;

{ TcxCustomShellTreeView }

constructor TcxCustomShellTreeView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataBinding := GetDataBindingClass.Create(Self, Self);
  with TcxCustomDataBindingAccess(FDataBinding) do
  begin
    OnDataChange := Self.DataChange;
    OnDataSetChange := Self.DataSetChange;
    OnUpdateData := Self.UpdateData;
  end;
  LookAndFeel.MasterLookAndFeel := Self.Style.LookAndFeel;
  HScrollBar.SmallChange := 1;
  VScrollBar.SmallChange := 1;
  Width := 121;
  Height := 97;
end;

destructor TcxCustomShellTreeView.Destroy;
begin
  FreeAndNil(FDataBinding);
  inherited Destroy;
end;

function TcxCustomShellTreeView.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or
    TcxCustomDataBindingAccess(FDataBinding).ExecuteAction(Action);
end;

procedure TcxCustomShellTreeView.SetFocus;
begin
  if not IsDesigning then
    inherited SetFocus;
end;

function TcxCustomShellTreeView.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or
    TcxCustomDataBindingAccess(FDataBinding).UpdateAction(Action);
end;

function TcxCustomShellTreeView.GetNodeAbsolutePIDL(ANode: TTreeNode): PItemIDList;
var
  AItemProducer: TcxShellTreeItemProducerAccess;
  AItem: TcxShellItemInfo;
begin
  AItemProducer := TcxShellTreeItemProducerAccess(ANode.Data);
  if AItemProducer.FolderPidl <> nil then
    Result := GetPidlCopy(AItemProducer.FolderPidl)
  else
  begin
    CheckShellRoot(Root);
    if ANode.Parent = nil then
      Result := GetPidlCopy(Root.Pidl)
    else
    begin
      AItemProducer := TcxShellTreeItemProducerAccess(ANode.Parent.Data);
      AItemProducer.LockRead;
      try
        AItem := AItemProducer.ItemInfo[ANode.Index];
        Result := GetPidlCopy(AItem.FullPIDL);
      finally
        AItemProducer.UnlockRead;
      end;
    end;
  end;
end;

procedure TcxCustomShellTreeView.UpdateContent;
begin
  InnerTreeView.UpdateContent;
end;

procedure TcxCustomShellTreeView.DoExit;
begin
  if IsDestroying or FIsExitProcessing then
    Exit;
  FIsExitProcessing := True;
  try
    try
      DataBinding.UpdateDataSource;
    except
      SetFocus;
      raise;
    end;
    inherited DoExit;
  finally
    FIsExitProcessing := False;
  end;
end;

procedure TcxCustomShellTreeView.Loaded;
begin
  inherited Loaded;
  InnerTreeView.Loaded;
  SetScrollBarsParameters;
end;

procedure TcxCustomShellTreeView.Scroll(AScrollBarKind: TScrollBarKind;
  AScrollCode: TScrollCode; var AScrollPos: Integer);
begin
  inherited Scroll(AScrollBarKind, AScrollCode, AScrollPos);
  InnerTreeView.UpdateVisibleItems;
end;

procedure TcxCustomShellTreeView.CurrentFolderChangedHandler(Sender: TObject;
  Root: TcxCustomShellRoot);
begin
  SetScrollBarsParameters;
end;

function TcxCustomShellTreeView.GetDataBindingClass: TcxCustomDataBindingClass;
begin
  Result := TcxDataBinding;
end;

function TcxCustomShellTreeView.GetInnerTreeViewClass: TcxInnerTreeViewClass;
begin
  Result := TcxInnerShellTreeView;
end;

procedure TcxCustomShellTreeView.InitializeInternalTreeView;
begin
  inherited InitializeInternalTreeView;
  with InnerTreeView do
  begin
    OnAddFolder := Self.AddFolderHandler;
    OnChange := Self.ChangeHandler;
    OnChanging := Self.ChangingHandler;
    OnCollapsed := Self.CollapsedHandler;
    OnCollapsing := Self.CollapsingHandler;
    OnEdited := Self.EditedHandler;
    OnEditing := Self.EditingHandler;
    OnExpanded := Self.ExpandedHandler;
    OnExpanding := Self.ExpandingHandler;
    OnRootChanged := Self.CurrentFolderChangedHandler;
    OnShellChange := Self.ShellChangeHandler;
  end;
end;

function TcxCustomShellTreeView.GetViewOptions(AForNavigation: Boolean = False): TcxShellViewOptions;
begin
  with InnerTreeView do
    begin
      Result := [];
      if Options.ShowNonFolders then
        Include(Result, svoShowFiles);
      if Options.ShowFolders then
        Include(Result, svoShowFolders);
      if AForNavigation or Options.ShowHidden then
        Include(Result, svoShowHidden);
    end;
end;

function TcxCustomShellTreeView.IsDragAndDropSupported: Boolean;
begin
  Result := True;
end;

function TcxCustomShellTreeView.GetDependedControls: TcxShellDependedControls;
var
  ADependedControls: IcxShellDependedControls;
begin
  if Supports(InnerControl, IcxShellDependedControls, ADependedControls) then
    Result := ADependedControls.GetDependedControls
  else
    Result := nil;
end;

function TcxCustomShellTreeView.GetShellRoot: TcxCustomShellRoot;
var
  ARoot: IcxShellRoot;
begin
  if Supports(InnerControl, IcxShellRoot, ARoot) then
    Result := ARoot.GetRoot
  else
    Result := nil;
end;

procedure TcxCustomShellTreeView.WndProc(var Message: TMessage);
begin
  case Message.Msg of
    DSM_FIRST..DSM_LAST:
      if (InnerControl <> nil) and InnerControl.HandleAllocated then
      begin
        Message.Result := SendMessage(InnerControl.Handle,
          Message.Msg, Message.WParam, Message.LParam);
        Exit;
      end;
  end;
  inherited WndProc(Message);
end;

procedure TcxCustomShellTreeView.AddFolderHandler(Sender: TObject;
  AFolder: TcxShellFolder; var ACanAdd: Boolean);
begin
  if Assigned(FOnAddFolder) then
    FOnAddFolder(Self, AFolder, ACanAdd);
end;

procedure TcxCustomShellTreeView.ChangeHandler(Sender: TObject; Node: TTreeNode);
begin
  try
    if Assigned(FOnChange) then
      FOnChange(Self, Node);
  finally
    SetScrollBarsParameters;
  end;
end;

procedure TcxCustomShellTreeView.ChangingHandler(Sender: TObject; Node: TTreeNode;
  var AllowChange: Boolean);
begin
  if Assigned(FOnChanging) then
    FOnChanging(Self, Node, AllowChange);
end;

procedure TcxCustomShellTreeView.CollapsedHandler(Sender: TObject;
  Node: TTreeNode);
begin
  try
    if Assigned(FOnCollapsed) then
      FOnCollapsed(Self, Node);
  finally
    SetScrollBarsParameters;
  end;
end;

procedure TcxCustomShellTreeView.CollapsingHandler(Sender: TObject;
  Node: TTreeNode; var AllowCollapse: Boolean);
begin
  if Assigned(FOnCollapsing) then
    FOnCollapsing(Self, Node, AllowCollapse);
end;

procedure TcxCustomShellTreeView.EditedHandler(Sender: TObject;
  Node: TTreeNode; var S: string);
begin
  try
    if Assigned(FOnEdited) then
      FOnEdited(Self, Node, S);
  finally
    SetScrollBarsParameters;
  end;
end;

procedure TcxCustomShellTreeView.EditingHandler(Sender: TObject; Node: TTreeNode;
  var AllowEdit: Boolean);
begin
  if Assigned(FOnEditing) then
    FOnEditing(Self, Node, AllowEdit);
end;

procedure TcxCustomShellTreeView.ExpandedHandler(Sender: TObject;
  Node: TTreeNode);
begin
  try
    if Assigned(FOnExpanded) then
      FOnExpanded(Self, Node);
  finally
    SetScrollBarsParameters;
  end;
end;

procedure TcxCustomShellTreeView.ExpandingHandler(Sender: TObject;
  Node: TTreeNode; var AllowExpansion: Boolean);
begin
  if Assigned(FOnExpanding) then
    FOnExpanding(Self, Node, AllowExpansion);
end;

procedure TcxCustomShellTreeView.ShellChangeHandler(Sender: TObject;
  AEventID: DWORD; APIDL1, APIDL2: PItemIDList);
begin
  if Assigned(FOnShellChange) then
    FOnShellChange(Self, AEventID, APIDL1, APIDL2);
end;

function TcxCustomShellTreeView.GetAbsolutePIDL: PItemIDList;
begin
  Result := nil;
  if not IsDestroying then
    Result := InnerTreeView.AbsolutePIDL;
end;

function TcxCustomShellTreeView.GetDragDropSettings: TcxDragDropSettings;
begin
  Result := TcxDragDropSettings(InnerTreeView.DragDropSettings);
end;

function TcxCustomShellTreeView.GetFolder(AIndex: Integer): TcxShellFolder;
begin
  Result := InnerTreeView.Folders[AIndex];
end;

function TcxCustomShellTreeView.GetFolderCount: Integer;
begin
  Result := InnerTreeView.FolderCount;
end;

function TcxCustomShellTreeView.GetIndent: Integer;
begin
  Result := InnerTreeView.Indent;
end;

function TcxCustomShellTreeView.GetInnerTreeView: TcxInnerShellTreeView;
begin
  Result := TcxInnerShellTreeView(inherited InnerTreeView);
end;

function TcxCustomShellTreeView.GetOptions: TcxShellTreeViewOptions;
begin
  Result := TcxShellTreeViewOptions(InnerTreeView.Options);
end;

function TcxCustomShellTreeView.GetPath: string;
begin
  Result := '';
  if not IsDestroying then
    Result := InnerTreeView.Path;
end;

function TcxCustomShellTreeView.GetRoot: TcxShellTreeRoot;
begin
  Result := TcxShellTreeRoot(InnerTreeView.Root)
end;

function TcxCustomShellTreeView.GetShellListView: TcxCustomShellListView;
begin
  if InnerTreeView.ListView is TcxInnerShellListView then
    Result := TcxInnerShellListViewAccess(InnerTreeView.ListView).Container
  else
    Result := nil;
end;

function TcxCustomShellTreeView.GetShowInfoTips: Boolean;
begin
  Result := InnerTreeView.ShowInfoTips;
end;

function TcxCustomShellTreeView.GetTreeHotTrack: Boolean;
begin
  Result := InnerTreeView.HotTrack;
end;

procedure TcxCustomShellTreeView.SetAbsolutePIDL(Value: PItemIDList);
begin
  if not IsDestroying then
    InnerTreeView.AbsolutePIDL := Value;
end;

procedure TcxCustomShellTreeView.SetDragDropSettings(Value: TcxDragDropSettings);
begin
  InnerTreeView.DragDropSettings := Value;
end;

procedure TcxCustomShellTreeView.SetIndent(Value: Integer);
var
  APrevIndent: Integer;
begin
  APrevIndent := InnerTreeView.Indent;
  InnerTreeView.Indent := Value;
  if APrevIndent <> InnerTreeView.Indent then
    SetScrollBarsParameters;
end;

procedure TcxCustomShellTreeView.SetOptions(Value: TcxShellTreeViewOptions);
begin
  InnerTreeView.Options.Assign(Value);
end;

procedure TcxCustomShellTreeView.SetPath(const Value: string);
begin
  if not IsDestroying then
    InnerTreeView.Path := Value;
end;

procedure TcxCustomShellTreeView.SetRoot(Value: TcxShellTreeRoot);
begin
  InnerTreeView.Root := Value;
end;

procedure TcxCustomShellTreeView.SetShellListView(Value: TcxCustomShellListView);
begin
  if Value = nil then
    InnerTreeView.ListView := nil
  else
    InnerTreeView.ListView := Value.InnerListView;
end;

procedure TcxCustomShellTreeView.SetShowInfoTips(Value: Boolean);
begin
  InnerTreeView.ShowInfoTips := Value;
end;

procedure TcxCustomShellTreeView.SetTreeHotTrack(Value: Boolean);
begin
  InnerTreeView.HotTrack := Value;
end;

end.
