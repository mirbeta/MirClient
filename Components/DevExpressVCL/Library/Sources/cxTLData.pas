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

unit cxTLData;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Classes, SysUtils, Windows, Messages, Variants, dxCore, cxVariants, cxControls,
  cxClasses, cxData, cxCustomData, cxInplaceContainer, cxTL, cxTLStrs;

type
  TcxCustomDataTreeList = class;
  TcxVirtualTreeListNode = class;
  TcxCustomVirtualTreeList = class;

  TcxTreeListStructureLoader = class;

  { TcxTreeListCustomDataSource }

  TcxTreeListCustomDataSource = class(TcxCustomDataSource)
  protected
    // Methods used in smart load mode
    function GetChildCount(AParentHandle: TcxDataRecordHandle): Integer; virtual;
    function GetChildRecordHandle(AParentHandle: TcxDataRecordHandle; AChildIndex: Integer): TcxDataRecordHandle; virtual;
    function GetRootRecordHandle: TcxDataRecordHandle; virtual;
    // Methods used in LoadAllRecords mode
    function GetParentRecordHandle(ARecordHandle: TcxDataRecordHandle): TcxDataRecordHandle; virtual;
    function GetRecordCount: Integer; override;
    function GetRecordHandle(ARecordIndex: Integer): TcxDataRecordHandle; override;
    // Common methods
    procedure DeleteChildItems(AParentHandle: TcxDataRecordHandle);
    procedure NodeMoveTo(ARecordHandle, AttachRecordHandle: TcxDataRecordHandle;
      AttachMode: TcxTreeListNodeAttachMode; IsCopy: Boolean); virtual;
  public
    procedure DataChanged; override;
  end;

  { TcxDataTreeListDataController }

  TcxDataTreeListDataController = class(TcxTreeListDataController)
  private
    FDataSource: TcxTreeListCustomDataSource;
    FIsDataLoaded: Boolean;
    FLoader: TcxTreeListStructureLoader;
    FLoadingInProcess: Boolean;
    FLoadingRefCount: Integer;
    function GetTreeList: TcxCustomDataTreeList;
    procedure SetCustomDataSource(AValue: TcxTreeListCustomDataSource);
  protected
    procedure AfterLoad; virtual;
    procedure CheckData; virtual;
    function CreateLoader: TcxTreeListStructureLoader; virtual; abstract;
    procedure DoInitInsertingNode(ANode: TcxTreeListNode); virtual;
    function GetCustomDataSource: TcxTreeListCustomDataSource; virtual;
    function IsLoading: Boolean; override;
    procedure Load(ANode: TcxTreeListNode); virtual;
    procedure NodesMoveTo(AttachNode: TcxVirtualTreeListNode;
      AttachMode: TcxTreeListNodeAttachMode; ANodes: TList; IsCopy: Boolean);

    property IsDataLoaded: Boolean read FIsDataLoaded write FIsDataLoaded;
    property Loader: TcxTreeListStructureLoader read FLoader;
    property LoadingInProcess: Boolean read FLoadingInProcess write FLoadingInProcess;
    property LoadingRefCount: Integer read FLoadingRefCount;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetNodeDisplayText(ANode: TcxTreeListNode; AIndex: Integer): Variant; override;

    property CustomDataSource: TcxTreeListCustomDataSource read GetCustomDataSource write SetCustomDataSource;
    property TreeList: TcxCustomDataTreeList read GetTreeList;
  end;

  { TcxVirtualTreeListDataController }

  TcxVirtualTreeListDataController = class(TcxDataTreeListDataController)
  private
    function GetTreeList: TcxCustomVirtualTreeList;
  protected
    procedure CheckLoaded(ANode: TcxTreeListNode);
    function CreateLoader: TcxTreeListStructureLoader; override;
    procedure DeleteNode(ANode: TcxTreeListNode); override;
  public
    function CompareNodesByColumns(ANode1, ANode2: TcxTreeListNode; AColumns: TList): Integer; override;
    function GetNodeDisplayText(ANode: TcxTreeListNode; AIndex: Integer): Variant; override;
    function GetNodeValue(ANode: TcxTreeListNode; AIndex: Integer): Variant; override;
    procedure SetNodeValue(ANode: TcxTreeListNode; AIndex: Integer; const AValue: Variant); override;

    property TreeList: TcxCustomVirtualTreeList read GetTreeList;
  end;

  { TcxVirtualTreeListNode }

  TcxVirtualTreeListNode = class(TcxTreeListNode)
  private
    FRecordHandle: TcxDataRecordHandle;
  public
    property RecordHandle: TcxDataRecordHandle read FRecordHandle;
  end;

  { TcxTreeListStructureLoader }

  TcxTreeListStructureLoader = class
  private
    FDataController: TcxDataTreeListDataController;
    FTreeList: TcxCustomDataTreeList;
    function GetRoot: TcxTreeListNode;
  protected
    Nodes, Loaded: TList;
    procedure AssignValues(ANode: TcxTreeListNode); virtual;
    function CloneList(ASource: TList): TList;
    procedure DoLoad(ALoadingNode: TcxTreeListNode); virtual; abstract;
    function GetImageIndex(ANode: TcxTreeListNode; AIndexType: TcxTreeListImageIndexType): Integer; virtual;
    procedure InternalDelete(ANodes: TList);
    procedure InternalMove(ANode, ADestNode: TcxTreeListNode; AMode: TcxTreeListNodeAttachMode);
    function IsChildrenExist(ANode: TcxTreeListNode): Boolean; virtual; abstract;
    function IsLoadAllRecords: Boolean; virtual;
    procedure RestorePos; virtual;
    procedure SavePos; virtual;
  public
    constructor Create(AOwner: TcxDataTreeListDataController); virtual;
    procedure Load(ANode: TcxTreeListNode); virtual;

    property DataController: TcxDataTreeListDataController read FDataController;
    property Root: TcxTreeListNode read GetRoot;
    property TreeList: TcxCustomDataTreeList read FTreeList;
  end;

  { TcxVirtualTreeListLoader }

  TcxVirtualTreeListLoader = class(TcxTreeListStructureLoader)
  private
    function GetDataController: TcxDataTreeListDataController;
    function GetDataSource: TcxTreeListCustomDataSource;
    function GetTreeList: TcxCustomVirtualTreeList;
  protected
    function AddChild(AParent: TcxTreeListNode; ARecordHandle: TcxDataRecordHandle): TcxVirtualTreeListNode;
    procedure AssignValues(ANode: TcxTreeListNode); override;
    procedure DoLoad(ALoadingNode: TcxTreeListNode); override;
    function ExtractNode(ARecordHandle: TcxDataRecordHandle;
      var ANode: TcxVirtualTreeListNode): Boolean;
    function FindParentNode(const AParentHandle: TcxDataRecordHandle; APrev: TcxVirtualTreeListNode;
      var ANode: TcxVirtualTreeListNode): Boolean;
    function IsChildrenExist(ANode: TcxTreeListNode): Boolean; override;
    procedure LoadAllRecords; virtual;
    procedure LoadLevel(AParent: TcxTreeListNode; AParentHandle: TcxDataRecordHandle); virtual;
    procedure LoadValues(ANode: TcxTreeListNode);
    procedure LoadVirtualLevel(ANode: TcxTreeListNode; ARecursive: Boolean); virtual;
    procedure RestoreNodeState(ANode: TcxTreeListNode; AData: Pointer);
    procedure SaveNodeState(ANode: TcxTreeListNode; AData: Pointer);
  public
    property DataController: TcxDataTreeListDataController read GetDataController;
    property DataSource: TcxTreeListCustomDataSource read GetDataSource;
    property TreeList: TcxCustomVirtualTreeList read GetTreeList;
  end;

  { TcxVirtualTreeListOptionsData }

  TcxVirtualTreeListOptionsData = class(TcxTreeListOptionsData)
  private
    FCheckHasChildren: Boolean;
    FSmartLoad: Boolean;
    procedure SetSmartLoad(AValue: Boolean);
  public
    constructor Create(AOwner: TPersistent); override;
    procedure Assign(Source: TPersistent); override;
  published
    property CheckHasChildren: Boolean read FCheckHasChildren write FCheckHasChildren default True;
    property SmartLoad: Boolean read FSmartLoad write SetSmartLoad default False;
  end;

  { TcxCustomDataTreeList }

  TcxDataTreeListChange = (dtcStructure, dtcData, dtcLayout, dtcFocused);
  TcxDataTreeListChanges = set of TcxDataTreeListChange;

  TcxCustomDataTreeList = class(TcxCustomTreeList)
  private
    FDataChanges: TcxDataTreeListChanges;
    FInternalCreationRefCount: Integer;
    FLoadingLockCount: Integer;
    FLockDataChanges: Integer;
    function GetDataController: TcxDataTreeListDataController;
    function GetLoader: TcxTreeListStructureLoader;
    function GetOptionsData: TcxVirtualTreeListOptionsData;
    procedure SetOptionsData(Value: TcxVirtualTreeListOptionsData);
  protected
    procedure AddDataChanges(AChanges: TcxDataTreeListChanges);
    function AddNode(ANode, ARelative: TcxTreeListNode; AData: Pointer;
      AttachMode: TcxTreeListNodeAttachMode): TcxTreeListNode; override;
    procedure CheckChanges; override;
    procedure CheckDataChanges; virtual;
    function CreateNode: TcxTreeListNode; override;
    procedure DataModeChanged; virtual;
    procedure DeleteNodes(AList: TList); override;
    procedure DoClear; override;
    procedure DoExpand(ANode: TcxTreeListNode); override;
    function GetOptionsDataClass: TcxControlOptionsDataClass; override;
    procedure InternalCreationAddRef;
    procedure InternalCreationRelease;
    function IsDataSettingsValid: Boolean; virtual;
    procedure LockLoading;
    procedure UnlockLoading;

    property DataController: TcxDataTreeListDataController read GetDataController;
    property DataChanges: TcxDataTreeListChanges read FDataChanges write FDataChanges;
    property InternalCreationRefCount: Integer read FInternalCreationRefCount;
    property Loader: TcxTreeListStructureLoader read GetLoader;
    property LoadingLockCount: Integer read FLoadingLockCount;
    property LockDataChanges: Integer read FLockDataChanges;
    property OptionsData: TcxVirtualTreeListOptionsData read GetOptionsData write SetOptionsData;
  public
    procedure FullRefresh; override;
  end;

  { TcxCustomVirtualTreeList }

  TcxVirtualTreeListGetChildCountEvent = procedure(Sender: TcxCustomTreeList;
     AParentNode: TcxTreeListNode; var ACount: Integer) of object;
  TcxVirtualTreeListNodeValueEvent = procedure(Sender: TcxCustomTreeList;
     ANode: TcxTreeListNode; AColumn: TcxTreeListColumn; var AValue: Variant) of object;

  TcxCustomVirtualTreeList = class(TcxCustomDataTreeList)
  private
    FUpdatingNode: TcxTreeListNode;
    FOnGetChildCount: TcxVirtualTreeListGetChildCountEvent;
    FOnGetNodeValue: TcxVirtualTreeListNodeValueEvent;
    FOnSetNodeValue: TcxVirtualTreeListNodeValueEvent;
    function GetDataController: TcxDataTreeListDataController;
    function GetDataSource: TcxTreeListCustomDataSource;
    procedure SetOnGetChildCount(AValue: TcxVirtualTreeListGetChildCountEvent);
    procedure SetOnGetNodeValue(AValue: TcxVirtualTreeListNodeValueEvent);
    procedure SetOnSetNodeValue(AValue: TcxVirtualTreeListNodeValueEvent);
    procedure SetDataSource(Value: TcxTreeListCustomDataSource);
  protected
    function CanUseMultiThreadedSorting: Boolean; override;
    procedure DataChanged; override;
    procedure DoDeleteNode(ANode: TcxTreeListNode); override;
    function DoGetChildCount(ANode: TcxTreeListNode): Integer; virtual;
    procedure DoLoadValues(ANode: TcxTreeListNode); virtual;
    procedure DoMoveTo(AttachNode: TcxTreeListNode;
      AttachMode: TcxTreeListNodeAttachMode; ANodes: TList; IsCopy: Boolean); override;
    procedure DoSetValue(ANode: TcxTreeListNode; AIndex: Integer; var AValue: Variant); virtual;
    function GetDataControllerClass: TcxCustomDataControllerClass; override;
    procedure InitInsertingNode(ANode: TcxTreeListNode); override;
    function InsertNode(ARelative: TcxTreeListNode; IsAppend: Boolean): Boolean; override;
    function IsCompletelyVirtual: Boolean; virtual;
    procedure Loaded; override;

    property DataController: TcxDataTreeListDataController read GetDataController;
    property UpdatingNode: TcxTreeListNode read FUpdatingNode write FUpdatingNode;
    property OnGetChildCount: TcxVirtualTreeListGetChildCountEvent read FOnGetChildCount write SetOnGetChildCount;
    property OnGetNodeValue: TcxVirtualTreeListNodeValueEvent read FOnGetNodeValue write SetOnGetNodeValue;
    property OnSetNodeValue: TcxVirtualTreeListNodeValueEvent read FOnSetNodeValue write SetOnSetNodeValue;
  public
    destructor Destroy; override;
    function HandleFromNode(ANode: TcxTreeListNode): TcxDataRecordHandle; virtual;
    function NodeFromHandle(ARecordHandle: TcxDataRecordHandle): TcxTreeListNode; virtual;

    property CustomDataSource: TcxTreeListCustomDataSource read GetDataSource write SetDataSource;
  end;

  { TcxVirtualTreeList }

  TcxVirtualTreeList = class(TcxCustomVirtualTreeList)
  public
    property ColumnCount;
    property Columns;
    property Customizing;
    property DataController;
    property FocusedColumn;
    property FocusedNode;
    property HitTest;
    property IsEditing;
    property SelectionCount;
    property Selections;
    property Sorted;
    property SortedColumnCount;
    property SortedColumns;
    property TopVisibleNode;
    property VisibleColumnCount;
    property VisibleColumns;
    property VisibleCount;
  published
    property Align;
    property Anchors;
    property Bands;
    property BorderStyle;
    property Constraints;
    property Cursor;
    property DateTimeHandling;
    property DefaultLayout;
    property DefaultRowHeight;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FilterBox;
    property Filtering;
    property FindPanel;
    property Font;
    property HelpContext;
    property HelpKeyword;
    property HelpType;
    property Hint;
    property Images;
    property LookAndFeel;
    property Navigator;
    property OptionsBehavior;
    property OptionsCustomizing;
    property OptionsData;
    property OptionsSelection;
    property OptionsView;
    property ParentColor;
    property ParentFont;
    property PopupMenu;
    property PopupMenus;
    property Preview;
    property StateImages;
    property Styles;
    property TabOrder;
    property TabStop;
    property Visible;
    property NavigatorEvents;
    property OnAfterSummary;
    property OnBandHeaderClick;
    property OnBandPosChanged;
    property OnBandSizeChanged;
    property OnBeginDragNode;
    property OnCanFocusNode;
    property OnCanResize;
    property OnCanSelectNode;
    property OnChange;
    property OnClick;
    property OnCollapsed;
    property OnCollapsing;
    property OnColumnHeaderClick;
    property OnColumnPosChanged;
    property OnColumnSizeChanged;
    property OnCompare;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnCustomDrawBackgroundCell;
    property OnCustomDrawBandCell;
    property OnCustomDrawBandHeaderCell;
    property OnCustomDrawDataCell;
    property OnCustomDrawFooterCell;
    property OnCustomDrawHeaderCell;
    property OnCustomDrawIndentCell;
    property OnCustomDrawIndicatorCell;
    property OnCustomDrawPreviewCell;
    property OnCustomizationFormVisibleChanged;
    property OnDataChanged;
    property OnDblClick;
    property OnDeletion;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEdited;
    property OnEditing;
    property OnEditValueChanged;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnExpanded;
    property OnExpanding;
    property OnFilterControlDialogShow;
    property OnFilterCustomization;
    property OnFilterDialogShow;
    property OnFilterNode;
    property OnFindPanelVisibilityChanged;
    property OnFocusedColumnChanged;
    property OnFocusedNodeChanged;
    property OnGetCellHint;
    property OnGetChildCount;
    property OnGetDragDropText;
    property OnGetLevelImages;
    property OnGetNodeHeight;
    property OnGetNodeImageIndex;
    property OnGetNodePreviewHeight;
    property OnGetNodeValue;
    property OnGetSiteInfo;
    property OnGetStoredProperties;
    property OnGetStoredPropertyValue;
    property OnHotTrackNode;
    property OnInitEdit;
    property OnInitEditValue;
    property OnInitFilteringDateRanges;
    property OnInitStoredObject;
    property OnIsGroupNode;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnLayoutChanged;
    property OnLeftPosChanged;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnMoveTo;
    property OnNodeChanged;
    property OnNodeCheckChanged;
    property OnResize;
    property OnSelectionChanged;
    property OnSetNodeValue;
    property OnSetStoredPropertyValue;
    property OnSorted;
    property OnSorting;
    property OnStartDock;
    property OnStartDrag;
    property OnSummary;
    property OnTopRecordIndexChanged;
    property OnUnDock;
    property PopupMenusEvents;
    property StylesEvents;
  end;

const
  cxNullRecordHandle: TcxDataRecordHandle = TcxDataRecordHandle(-1);

implementation

uses Math;

function cxCompareNodesByRecordHandle(
  ANode1, ANode2: TcxVirtualTreeListNode): Integer;
begin
  Result := dxCompareValues(ANode1.RecordHandle, ANode2.RecordHandle);
end;

{ TcxTreeListCustomDataSource }

procedure TcxTreeListCustomDataSource.DataChanged;
begin
  if DataController is TcxDataTreeListDataController then
    TcxDataTreeListDataController(DataController).Load(nil)
  else
    inherited DataChanged;
end;

procedure TcxTreeListCustomDataSource.DeleteChildItems(
  AParentHandle: TcxDataRecordHandle);
var
  I: Integer;
  AItemHandle: TcxDataRecordHandle;
begin
  for I := GetChildCount(AParentHandle) - 1 downto 0 do
  begin
    AItemHandle := GetChildRecordHandle(AParentHandle, I);
    try
      DeleteChildItems(AItemHandle);
    finally
      DeleteRecord(AItemHandle);
    end;
  end;
end;

function TcxTreeListCustomDataSource.GetChildCount(
  AParentHandle: TcxDataRecordHandle): Integer;
begin
  Result := 0;
end;

function TcxTreeListCustomDataSource.GetChildRecordHandle(
  AParentHandle: TcxDataRecordHandle; AChildIndex: Integer): TcxDataRecordHandle;
begin
  Result := cxNullRecordHandle;
end;

procedure TcxTreeListCustomDataSource.NodeMoveTo(
  ARecordHandle, AttachRecordHandle: TcxDataRecordHandle;
  AttachMode: TcxTreeListNodeAttachMode; IsCopy: Boolean);
begin
end;

function TcxTreeListCustomDataSource.GetParentRecordHandle(
  ARecordHandle: TcxDataRecordHandle): TcxDataRecordHandle;
begin
  Result := cxNullRecordHandle;
end;

function TcxTreeListCustomDataSource.GetRecordCount: Integer;
begin
  Result := 0;
end;

function TcxTreeListCustomDataSource.GetRecordHandle(
  ARecordIndex: Integer): TcxDataRecordHandle;
begin
  Result := TcxDataRecordHandle(ARecordIndex);
end;

function TcxTreeListCustomDataSource.GetRootRecordHandle: TcxDataRecordHandle;
begin
  Result := cxNullRecordHandle;
end;

{ TcxDataTreeListDataController }

constructor TcxDataTreeListDataController.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLoader := CreateLoader;
end;

destructor TcxDataTreeListDataController.Destroy;
begin
  FLoader.Free;
  inherited Destroy;
end;

function TcxDataTreeListDataController.GetNodeDisplayText(
  ANode: TcxTreeListNode; AIndex: Integer): Variant;
begin
  if (CustomDataSource = nil) or (ANode = Root) then
    Result := inherited GetNodeDisplayText(ANode, AIndex)
  else
    with CustomDataSource do
      Result := GetDisplayText(TcxVirtualTreeListNode(ANode).RecordHandle, GetItemHandle(AIndex));
end;

procedure TcxDataTreeListDataController.AfterLoad;
begin
end;

procedure TcxDataTreeListDataController.CheckData;
begin
  if IsDataLoaded or LoadingInProcess or
    TreeList.IsTreeListLocked or not TreeList.IsDataSettingsValid then Exit;
  Load(nil);
end;

procedure TcxDataTreeListDataController.DoInitInsertingNode(
  ANode: TcxTreeListNode);
begin
end;

function TcxDataTreeListDataController.GetCustomDataSource: TcxTreeListCustomDataSource;
begin
  Result := FDataSource;
end;

function TcxDataTreeListDataController.IsLoading: Boolean;
begin
  Result := (FLoadingRefCount > 0) or inherited IsLoading;
end;

procedure TcxDataTreeListDataController.Load(ANode: TcxTreeListNode);
var
  ALoaded: Boolean;
begin
  if IsDataLoading or (TreeList.LoadingLockCount > 0) or
    not TreeList.IsDataSettingsValid then Exit;
  TreeList.BeginUpdate;
  try
    FLoadingInProcess := True;
    if TreeList.IsDataSettingsValid then
    begin
      ALoaded := IsDataLoaded;
      IsDataLoaded := False;
      if not ALoaded then
        Loader.Load(nil)
      else
        Loader.Load(ANode)
    end;
    TreeList.AddChanges([tcData, tcStructure]);
  finally
    TreeList.EndUpdate;
    FLoadingInProcess := False;
    AfterLoad;
    IsDataLoaded := True;
  end;
end;

procedure TcxDataTreeListDataController.NodesMoveTo(AttachNode: TcxVirtualTreeListNode;
  AttachMode: TcxTreeListNodeAttachMode; ANodes: TList; IsCopy: Boolean);
var
  I: Integer;
begin
  if CustomDataSource = nil then Exit;
  CustomDataSource.CurrentProvider := Provider;
  for I := 0 to ANodes.Count - 1 do
  begin
    CustomDataSource.NodeMoveTo(TcxVirtualTreeListNode(ANodes[I]).RecordHandle,
      AttachNode.RecordHandle, AttachMode, IsCopy);
  end;
end;

function TcxDataTreeListDataController.GetTreeList: TcxCustomDataTreeList;
begin
  Result := TcxCustomDataTreeList(GetOwner);
end;

procedure TcxDataTreeListDataController.SetCustomDataSource(
  AValue: TcxTreeListCustomDataSource);
begin
  if FDataSource <> AValue then
  begin
    FDataSource := AValue;
    Load(nil);
  end;
end;

  { TcxVirtualTreeListDataController }

function TcxVirtualTreeListDataController.CompareNodesByColumns(
  ANode1, ANode2: TcxTreeListNode; AColumns: TList): Integer;
begin
  CheckLoaded(ANode1);
  CheckLoaded(ANode2);
  Result := inherited CompareNodesByColumns(ANode1, ANode2, AColumns);
end;

function TcxVirtualTreeListDataController.GetNodeDisplayText(
  ANode: TcxTreeListNode; AIndex: Integer): Variant;
begin
  CheckLoaded(ANode);
  Result := inherited GetNodeDisplayText(ANode, AIndex);
end;

function TcxVirtualTreeListDataController.GetNodeValue(
  ANode: TcxTreeListNode; AIndex: Integer): Variant;
begin
  CheckLoaded(ANode);
  Result := inherited GetNodeValue(ANode, AIndex);
end;

procedure TcxVirtualTreeListDataController.SetNodeValue(
  ANode: TcxTreeListNode; AIndex: Integer; const AValue: Variant);
var
  V: Variant;
begin
  CheckLoaded(ANode);
  V := AValue;
  if TreeList.IsCompletelyVirtual then
    TreeList.DoSetValue(ANode, AIndex, V);
  inherited SetNodeValue(ANode, AIndex, V);
  if not TreeList.IsCompletelyVirtual and (FLoadingRefCount = 0) and (CustomDataSource <> nil) then
    CustomDataSource.SetValue(TcxVirtualTreeListNode(ANode).RecordHandle, CustomDataSource.GetItemHandle(AIndex), V);
  IsValueChanged := True;
end;


procedure TcxVirtualTreeListDataController.CheckLoaded(ANode: TcxTreeListNode);
begin
  if not (nsValuesAssigned in TcxVirtualTreeListNode(ANode).State) and not TreeList.IsDestroying then
    TcxVirtualTreeListLoader(Loader).LoadValues(ANode);
end;

function TcxVirtualTreeListDataController.CreateLoader: TcxTreeListStructureLoader;
begin
  Result := TcxVirtualTreeListLoader.Create(Self);
end;

procedure TcxVirtualTreeListDataController.DeleteNode(ANode: TcxTreeListNode);
begin
  if not TreeList.IsCompletelyVirtual then
    CustomDataSource.DeleteRecord(TcxVirtualTreeListNode(ANode).RecordHandle)
  else
    inherited DeleteNode(ANode);
end;

function TcxVirtualTreeListDataController.GetTreeList: TcxCustomVirtualTreeList;
begin
  Result := TcxCustomVirtualTreeList(inherited TreeList);
end;

{ TcxTreeListStructureLoader }

constructor TcxTreeListStructureLoader.Create(AOwner: TcxDataTreeListDataController);
begin
  FDataController := AOwner;
  FTreeList := DataController.TreeList;
end;

procedure TcxTreeListStructureLoader.Load(ANode: TcxTreeListNode);
begin
  TreeList.SetGlassCursor;
  try
    TreeList.InternalCreationAddRef;
    SavePos;
    try
      DoLoad(ANode);
    finally
      RestorePos;
      TreeList.InternalCreationRelease;
    end;
  finally
    TreeList.RestoreCursor
  end;
end;

procedure TcxTreeListStructureLoader.AssignValues(ANode: TcxTreeListNode);
var
  AIndex: TcxTreeListImageIndexType;
begin
  if ANode = Root then Exit;
  for AIndex := Low(TcxTreeListImageIndexType) to High(TcxTreeListImageIndexType) do
    TcxVirtualTreeListNode(ANode).FImageIndexes[AIndex] := GetImageIndex(ANode, AIndex);
  DataController.IsValueChanged := not ANode.IsEditing;
end;

function TcxTreeListStructureLoader.CloneList(ASource: TList): TList;
begin
  Result := TList.Create;
  dxCopyList(ASource, Result);
end;

function TcxTreeListStructureLoader.GetImageIndex(
  ANode: TcxTreeListNode; AIndexType: TcxTreeListImageIndexType): Integer;
begin
  Result := TreeList.DoGetNodeImageIndex(ANode, AIndexType);
end;

procedure TcxTreeListStructureLoader.InternalDelete(ANodes: TList);
begin
  if not Assigned(ANodes) then Exit;
  TreeList.InternalDelete(ANodes);
end;

procedure TcxTreeListStructureLoader.InternalMove(
  ANode, ADestNode: TcxTreeListNode; AMode: TcxTreeListNodeAttachMode);
begin
  TcxVirtualTreeListNode(ANode).ExtractFromParent;
  TreeList.InternalMove(ANode, ADestNode, AMode);
end;

function TcxTreeListStructureLoader.IsLoadAllRecords: Boolean;
begin
  Result := not TreeList.OptionsData.SmartLoad;
end;

procedure TcxTreeListStructureLoader.RestorePos;
begin
  InterlockedDecrement(DataController.FLoadingRefCount);
  DataController.DataChangedBusy := False;
  DataController.IsValueChanged := DataController.EditingNode <> nil;
end;

procedure TcxTreeListStructureLoader.SavePos;
begin
  InterlockedIncrement(DataController.FLoadingRefCount);
  DataController.DataChangedBusy := True;
end;

function TcxTreeListStructureLoader.GetRoot: TcxTreeListNode;
begin
  Result := TreeList.Root;
end;

{ TcxVirtualTreeListLoader }

function TcxVirtualTreeListLoader.AddChild(AParent: TcxTreeListNode;
  ARecordHandle: TcxDataRecordHandle): TcxVirtualTreeListNode;
begin
  Result := TcxVirtualTreeListNode(AParent.AddChild);
  Result.FRecordHandle := ARecordHandle;
end;

procedure TcxVirtualTreeListLoader.AssignValues(ANode: TcxTreeListNode);
begin
  Exclude(TcxVirtualTreeListNode(ANode).State, nsValuesAssigned);
  DataController.FreeNodeRecord(ANode);
  DataController.IsValueChanged := not ANode.IsEditing;
end;

procedure TcxVirtualTreeListLoader.DoLoad(ALoadingNode: TcxTreeListNode);
begin
  if ALoadingNode = nil then
    DataController.ForEachNode(SaveNodeState, nil);
  try
    if DataSource = nil then
    begin
      if not TcxCustomVirtualTreeList(TreeList).IsCompletelyVirtual then
        TreeList.InternalClearAll;
      if ALoadingNode = nil then
        ALoadingNode := Root;
      if IsLoadAllRecords then
        LoadVirtualLevel(Root, True)
      else
        LoadVirtualLevel(ALoadingNode, False);
      Exit;
    end;
    if not IsLoadAllRecords and (ALoadingNode <> nil) and (ALoadingNode.Count > 0) then
      ALoadingNode := nil; // todo: refresh loaded nodes !!!
    if not IsLoadAllRecords and (ALoadingNode <> nil) then
      LoadLevel(ALoadingNode, TcxVirtualTreeListNode(ALoadingNode).RecordHandle)
    else
    begin
      Nodes := CloneList(TreeList.AbsoluteItemsList);
      try
        Nodes.Sort(@cxCompareNodesByRecordHandle);
        if IsLoadAllRecords then
          LoadAllRecords
        else
          if ALoadingNode = nil then
            LoadLevel(Root, DataSource.GetRootRecordHandle);
          InternalDelete(Nodes);
      finally
        FreeAndNil(Nodes);
      end;
    end;
  finally
    if ALoadingNode = nil then
      DataController.ForEachNode(RestoreNodeState, nil);
  end;
end;

function TcxVirtualTreeListLoader.ExtractNode(
  ARecordHandle: TcxDataRecordHandle; var ANode: TcxVirtualTreeListNode): Boolean;
var
  L, H, I, C: TdxNativeInt;
begin
  L := 0;
  Result := False;
  if Nodes = nil then Exit;
  H := Nodes.Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    ANode := TcxVirtualTreeListNode(Nodes.List[I]);
    C := TdxNativeInt(ANode.RecordHandle) - TdxNativeInt(ARecordHandle);
    if C = 0 then
    begin
      Nodes.Delete(I);
      Result := True;
      Break;
    end
    else
      if C < 0 then
        L := I + 1
      else
        H := I - 1;
  end;
end;

function TcxVirtualTreeListLoader.FindParentNode(
  const AParentHandle: TcxDataRecordHandle; APrev: TcxVirtualTreeListNode;
  var ANode: TcxVirtualTreeListNode): Boolean;
var
  L, H, I, C: TdxNativeInt;
begin
  Result := (APrev <> nil) and (APrev.RecordHandle = AParentHandle);
  if Result then
  begin
    ANode := APrev;
    Exit;
  end;
  L := 0;
  H := Loaded.Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    ANode := TcxVirtualTreeListNode(Loaded.List[I]);
    C := TdxNativeInt(ANode.RecordHandle) - TdxNativeInt(AParentHandle);
    if C < 0 then
      L := I + 1
    else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        Break;
      end;
    end;
  end;
end;

function TcxVirtualTreeListLoader.IsChildrenExist(ANode: TcxTreeListNode): Boolean;
begin
  Result := DataSource.GetChildCount(
    TcxVirtualTreeListNode(ANode).RecordHandle) <> 0;
end;

procedure TcxVirtualTreeListLoader.LoadAllRecords;
var
  I: Integer;
  AHandle: TcxDataRecordHandle;
  ANode: TcxVirtualTreeListNode;
  AOrder: TList;
begin
  AOrder := TList.Create;
  try
    Loaded := TList.Create();
    try
      Loaded.Capacity := DataSource.GetRecordCount;
      for I := 0 to DataSource.GetRecordCount - 1 do
      begin
        AHandle := DataSource.GetRecordHandle(I);
        if not ExtractNode(AHandle, ANode) then
          ANode := AddChild(Root, AHandle)
        else
          ANode.SaveStateBeforeRefresh;
        AssignValues(ANode);
        Loaded.Add(ANode);
      end;
      AOrder.Assign(Loaded);
      Loaded.Sort(@cxCompareNodesByRecordHandle);
      ANode := nil;
      for I := 0 to AOrder.Count - 1 do
      begin
        AHandle := DataSource.GetParentRecordHandle(
          TcxVirtualTreeListNode(AOrder.List[I]).RecordHandle);
        if FindParentNode(AHandle, ANode, ANode) then
          InternalMove(TcxVirtualTreeListNode(AOrder.List[I]), ANode, tlamAddChild)
        else
          InternalMove(TcxVirtualTreeListNode(AOrder.List[I]), Root, tlamAddChild);
      end;
      for I := 0 to Loaded.Count - 1 do
        TcxVirtualTreeListNode(Loaded.List[I]).RestoreStateAfterRefresh;
    finally
      Loaded.Free;
    end;
  finally
    AOrder.Free;
  end;
end;

procedure TcxVirtualTreeListLoader.LoadLevel(
  AParent: TcxTreeListNode; AParentHandle: TcxDataRecordHandle);
var
  I: Integer;
  ALoadSubItems: Boolean;
  ANewNode: TcxVirtualTreeListNode;
  ARecordHandle: TcxDataRecordHandle;
begin
  for I := 0 to DataSource.GetChildCount(AParentHandle) - 1 do
  begin
    ALoadSubItems := False;
    ARecordHandle := DataSource.GetChildRecordHandle(AParentHandle, I);
    if ExtractNode(ARecordHandle, ANewNode) then
    begin
      ALoadSubItems := ANewNode.Expanded or (ANewNode.Count > 0);
      InternalMove(ANewNode, AParent, tlamAddChild)
    end
    else
      ANewNode := AddChild(AParent, ARecordHandle);
    ANewNode.HasChildren := not TreeList.OptionsData.CheckHasChildren or
      (DataSource.GetChildCount(ARecordHandle) > 0);
    AssignValues(ANewNode);
    if ANewNode.HasChildren and ALoadSubItems then
      LoadLevel(ANewNode, ANewNode.RecordHandle);
  end;
end;

procedure TcxVirtualTreeListLoader.LoadValues(ANode: TcxTreeListNode);
var
  I: Integer;
begin
  if nsValuesAssigned in TcxVirtualTreeListNode(ANode).State then Exit;
  Include(TcxVirtualTreeListNode(ANode).State, nsValuesAssigned);
  inherited AssignValues(ANode);
  InterlockedIncrement(DataController.FLoadingRefCount);
  try
    if TreeList.IsCompletelyVirtual then
      TreeList.DoLoadValues(ANode)
    else
      for I := 0 to TreeList.ColumnCount - 1 do
        DataController.SetNodeValue(ANode, I, DataSource.GetValue(
          TcxVirtualTreeListNode(ANode).RecordHandle, DataSource.GetItemHandle(I)));
  finally
    InterlockedDecrement(DataController.FLoadingRefCount);
  end;
  DataController.IsValueChanged := not ANode.IsEditing;
end;

procedure TcxVirtualTreeListLoader.LoadVirtualLevel(
  ANode: TcxTreeListNode; ARecursive: Boolean);
var
  ACount, I: Integer;
  AChildNode: TcxTreeListNode;
begin
  ACount := TreeList.DoGetChildCount(ANode);
  while ANode.Count > ACount do
    ANode.GetLastChild.Delete;
  AChildNode := ANode.getFirstChild;
  for I := 0 to ACount - 1 do
  begin
    if AChildNode = nil then
      AChildNode := ANode.AddChild;
    AssignValues(AChildNode);
    if ARecursive then
      LoadVirtualLevel(AChildNode, ARecursive)
    else
      AChildNode.HasChildren := (AChildNode.Count <> 0) or
        (not TreeList.OptionsData.CheckHasChildren or (TreeList.DoGetChildCount(AChildNode) > 0));
    AChildNode := AChildNode.GetNextSibling;
  end;
end;

procedure TcxVirtualTreeListLoader.RestoreNodeState(
  ANode: TcxTreeListNode; AData: Pointer);
begin
  TcxVirtualTreeListNode(ANode).RestoreStateAfterRefresh;
end;

procedure TcxVirtualTreeListLoader.SaveNodeState(
  ANode: TcxTreeListNode; AData: Pointer);
begin
  TcxVirtualTreeListNode(ANode).SaveStateBeforeRefresh;
end;

function TcxVirtualTreeListLoader.GetDataController: TcxDataTreeListDataController;
begin
  Result := TcxDataTreeListDataController(inherited DataController);
end;

function TcxVirtualTreeListLoader.GetDataSource: TcxTreeListCustomDataSource;
begin
  Result := TreeList.CustomDataSource;
end;

function TcxVirtualTreeListLoader.GetTreeList: TcxCustomVirtualTreeList;
begin
  Result := TcxCustomVirtualTreeList(inherited TreeList);
end;

{ TcxVirtualTreeListOptionsData }

constructor TcxVirtualTreeListOptionsData.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FCheckHasChildren := True;
end;

procedure TcxVirtualTreeListOptionsData.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TcxVirtualTreeListOptionsData then
  begin
    CheckHasChildren := TcxVirtualTreeListOptionsData(Source).CheckHasChildren;
    SmartLoad := TcxVirtualTreeListOptionsData(Source).SmartLoad;
  end;
end;

procedure TcxVirtualTreeListOptionsData.SetSmartLoad(AValue: Boolean);
begin
  if FSmartLoad <> AValue then
  begin
    FSmartLoad := AValue;
    TcxCustomDataTreeList(GetOwner).DataModeChanged;
  end;
end;

{ TcxCustomDataTreeList }

procedure TcxCustomDataTreeList.AddDataChanges(AChanges: TcxDataTreeListChanges);
begin
  FDataChanges := FDataChanges + AChanges;
end;

function TcxCustomDataTreeList.AddNode(ANode, ARelative: TcxTreeListNode;
  AData: Pointer; AttachMode: TcxTreeListNodeAttachMode): TcxTreeListNode;
begin
  Result := inherited AddNode(ANode, ARelative, AData, AttachMode);
end;

procedure TcxCustomDataTreeList.CheckChanges;
begin
  CheckDataChanges;
  inherited CheckChanges;
end;

procedure TcxCustomDataTreeList.CheckDataChanges;
begin
  if DataController.IsLoading or (FLockDataChanges > 0) or (DataChanges = [])  then Exit;
  Inc(FLockDataChanges);
  try
    if dtcStructure in DataChanges then
    begin
      RefreshFields;
      Include(FDataChanges, dtcData);
      Exclude(FDataChanges, dtcStructure);
      DataController.IsDataLoaded := False;
    end;
    if not IsDataSettingsValid then
    begin
      if Root.Count > 0 then
      begin
        Exclude(FDataChanges, dtcData);
        DoClear;
      end;
      Exit;
    end;
    if ([dtcData, dtcStructure] * FDataChanges = [dtcData]) then
      DataController.CheckData;
    if DataController.IsDataLoaded then
      Exclude(FDataChanges, dtcData);
    //cancel changes if data loaded
    if not (dtcData in DataChanges) then
      FDataChanges := [];
  finally
    Dec(FLockDataChanges);
  end;
end;

function TcxCustomDataTreeList.CreateNode: TcxTreeListNode;
begin
  Result := TcxVirtualTreeListNode.Create(Self);
end;

procedure TcxCustomDataTreeList.DataModeChanged;
begin
  DoClear;
  FullRefresh;
end;

procedure TcxCustomDataTreeList.DeleteNodes(AList: TList);
var
  I: Integer;
begin
  DataController.DataChangedBusy := True;
  try
    for I := 0 to AList.Count - 1 do
      DataController.DeleteNode(TcxTreeListNode(AList[I]))
  finally
    DataController.DataChangedBusy := False;
  end;
end;

procedure TcxCustomDataTreeList.DoExpand(ANode: TcxTreeListNode);
begin
  if (ANode.Count = 0) and ANode.HasChildren then
  begin
    Loader.Load(TcxVirtualTreeListNode(ANode));
    ANode.HasChildren := ANode.Count > 0;
  end;
end;

procedure TcxCustomDataTreeList.DoClear;
begin
  DataController.CustomDataSource := nil;
  inherited DoClear;
end;

function TcxCustomDataTreeList.GetOptionsDataClass: TcxControlOptionsDataClass;
begin
  Result := TcxVirtualTreeListOptionsData;
end;

procedure TcxCustomDataTreeList.InternalCreationAddRef;
begin
  Inc(FInternalCreationRefCount);
end;

procedure TcxCustomDataTreeList.InternalCreationRelease;
begin
  Dec(FInternalCreationRefCount);
end;

function TcxCustomDataTreeList.IsDataSettingsValid: Boolean;
begin
  Result := not (IsLoading or IsDestroying);
end;

procedure TcxCustomDataTreeList.LockLoading;
begin
  Inc(FLoadingLockCount);
end;

procedure TcxCustomDataTreeList.UnlockLoading;
begin
  Dec(FLoadingLockCount);
end;

procedure TcxCustomDataTreeList.FullRefresh;
begin
  if DataController.IsDataLoading or (LoadingLockCount > 0) then Exit;
  AddDataChanges([dtcStructure..dtcFocused]);
  RefreshFields;
  if IsTreeListLocked or not IsDataSettingsValid then
  begin
    DataController.FIsDataLoaded := False;
    Exit;
  end;
  BeginUpdate;
  try
    InternalClearAll;
    DataController.Load(nil);
  finally
    EndUpdate;
  end;
end;

function TcxCustomDataTreeList.GetDataController: TcxDataTreeListDataController;
begin
  Result := TcxDataTreeListDataController(inherited DataController);
end;

function TcxCustomDataTreeList.GetLoader: TcxTreeListStructureLoader;
begin
  Result := DataController.Loader;
end;

function TcxCustomDataTreeList.GetOptionsData: TcxVirtualTreeListOptionsData;
begin
  Result := TcxVirtualTreeListOptionsData(inherited OptionsData);
end;

procedure TcxCustomDataTreeList.SetOptionsData(
  Value: TcxVirtualTreeListOptionsData);
begin
  OptionsData.Assign(Value);
end;

{ TcxCustomVirtualTreeList }

destructor TcxCustomVirtualTreeList.Destroy;
begin
  if (DataController.EditingNode <> nil) and DataController.EditingNode.Inserting then
    DataController.Cancel;
  OnGetChildCount := nil;
  CustomDataSource := nil;
  InternalClearAll;
  inherited Destroy;
end;

function TcxCustomVirtualTreeList.HandleFromNode(
  ANode: TcxTreeListNode): TcxDataRecordHandle;
begin
  if ANode = Root then
    Result := CustomDataSource.GetRootRecordHandle
  else
    Result := TcxVirtualTreeListNode(ANode).RecordHandle;
end;

function TcxCustomVirtualTreeList.NodeFromHandle(
  ARecordHandle: TcxDataRecordHandle): TcxTreeListNode;
var
  I: Integer;
begin
  Result := nil;
  if CustomDataSource = nil then Exit;
  for I := 0 to AbsoluteCount - 1 do
    if TcxVirtualTreeListNode(AbsoluteItems[I]).RecordHandle = ARecordHandle then
    begin
      Result := AbsoluteItems[I];
      Exit;
    end;
  if ARecordHandle = CustomDataSource.GetRootRecordHandle then
    Result := Root;
end;

function TcxCustomVirtualTreeList.CanUseMultiThreadedSorting: Boolean;
begin
  Result := inherited CanUseMultiThreadedSorting and (CustomDataSource <> nil) and
    CustomDataSource.IsMultiThreadingSupported;
end;

procedure TcxCustomVirtualTreeList.DataChanged;
begin
  if IsLocked then Exit;
  inherited DataChanged;
end;

procedure TcxCustomVirtualTreeList.DoDeleteNode(ANode: TcxTreeListNode);
begin
  if ANode = FUpdatingNode then
    FUpdatingNode := nil;
  inherited DoDeleteNode(ANode);
end;

function TcxCustomVirtualTreeList.DoGetChildCount(ANode: TcxTreeListNode): Integer;
begin
  Result := 0;
  if Assigned(FOnGetChildCount) then
    OnGetChildCount(Self, ANode, Result);
end;

procedure TcxCustomVirtualTreeList.DoLoadValues(ANode: TcxTreeListNode);
var
  I: Integer;
  AValue: Variant;
begin
  if not Assigned(FOnGetNodeValue) then Exit;
  for I := 0 to ColumnCount - 1 do
  begin
    AValue := Null;
    OnGetNodeValue(Self, ANode, Columns[I], AValue);
    DataController.SetNodeValue(ANode, I, AValue);
  end;
end;

procedure TcxCustomVirtualTreeList.DoMoveTo(AttachNode: TcxTreeListNode;
  AttachMode: TcxTreeListNodeAttachMode; ANodes: TList; IsCopy: Boolean);
begin
  if OptionsData.SmartLoad and (AttachMode = tlamAddChild) and
    (AttachNode <> nil) and AttachNode.HasChildren and (AttachNode.Count = 0) then
    Loader.Load(TcxVirtualTreeListNode(AttachNode));
  BeginUpdate;
  try
    inherited DoMoveTo(AttachNode, AttachMode, ANodes, IsCopy);
    if not IsCompletelyVirtual then
      DataController.NodesMoveTo(TcxVirtualTreeListNode(AttachNode), AttachMode, ANodes, IsCopy);
  finally
    EndUpdate;
  end;
end;

procedure TcxCustomVirtualTreeList.DoSetValue(
  ANode: TcxTreeListNode; AIndex: Integer; var AValue: Variant);
begin
  if not DataController.IsLoading and Assigned(FOnSetNodeValue) then
    FOnSetNodeValue(Self, ANode, Columns[AIndex], AValue);
end;

function TcxCustomVirtualTreeList.GetDataControllerClass: TcxCustomDataControllerClass;
begin
  Result := TcxVirtualTreeListDataController;
end;

procedure TcxCustomVirtualTreeList.InitInsertingNode(ANode: TcxTreeListNode);
var
  AHandle: TcxDataRecordHandle;
begin
  UpdatingNode := ANode;
  try
    if not IsCompletelyVirtual then
    begin
      BeginUpdate;
      try
        if ANode.Parent = Root then
          AHandle := CustomDataSource.AppendRecord
        else
          AHandle := CustomDataSource.InsertRecord(
            TcxVirtualTreeListNode(ANode.Parent).RecordHandle);
        if UpdatingNode <> nil then
          TcxVirtualTreeListNode(UpdatingNode).FRecordHandle := AHandle;
      finally
        EndUpdate;
      end;
    end
    else
      TcxVirtualTreeListLoader(Loader).LoadValues(ANode);
    if UpdatingNode <> nil then
      inherited InitInsertingNode(ANode);
  finally
    UpdatingNode := nil;
  end;
end;

function TcxCustomVirtualTreeList.InsertNode(
  ARelative: TcxTreeListNode; IsAppend: Boolean): Boolean;
begin
  Result := (Assigned(CustomDataSource) or IsCompletelyVirtual) and
    inherited InsertNode(ARelative, IsAppend);
end;

function TcxCustomVirtualTreeList.IsCompletelyVirtual: Boolean;
begin
  Result := Assigned(OnGetChildCount) and Assigned(OnGetNodeValue);
end;

procedure TcxCustomVirtualTreeList.Loaded;
begin
  inherited Loaded;
  DataController.CheckData;
end;

function TcxCustomVirtualTreeList.GetDataController: TcxDataTreeListDataController;
begin
  Result := TcxDataTreeListDataController(inherited DataController);
end;

function TcxCustomVirtualTreeList.GetDataSource: TcxTreeListCustomDataSource;
begin
  Result := DataController.CustomDataSource;
  if Result <> nil then
    Result.CurrentProvider := DataController.Provider;
end;

procedure TcxCustomVirtualTreeList.SetOnGetChildCount(
  AValue: TcxVirtualTreeListGetChildCountEvent);
begin
  FOnGetChildCount := AValue;
  DataChanged;
end;

procedure TcxCustomVirtualTreeList.SetOnGetNodeValue(
  AValue: TcxVirtualTreeListNodeValueEvent);
begin
  FOnGetNodeValue := AValue;
  DataChanged;
end;

procedure TcxCustomVirtualTreeList.SetOnSetNodeValue(
  AValue: TcxVirtualTreeListNodeValueEvent);
begin
  FOnSetNodeValue := AValue;
  DataChanged;
end;

procedure TcxCustomVirtualTreeList.SetDataSource(
  Value: TcxTreeListCustomDataSource);
begin
  SetGlassCursor;
  try
    DataController.CustomDataSource := Value;
  finally
    RestoreCursor;
  end;
end;

end.


