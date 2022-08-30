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

unit cxShellControls;

{$I cxVer.inc}

interface

uses
  Windows, ActiveX, Classes, ComCtrls, CommCtrl, ComObj, Controls, Dialogs,
  Menus, Messages, Types, AppEvnts, ShellApi, ShlObj, SysUtils, dxCore, cxControls,
  cxListView, cxTreeView, cxShellCommon, Graphics, cxGraphics, ImgList,
  dxThemeManager, dxUxTheme, dxThemeConsts, cxEdit, cxImageList;

type
  TcxCustomInnerShellListView = class;
  TcxCustomInnerShellTreeView = class;

  TcxListViewStyle=(lvsIcon, lvsSmallIcon, lvsList, lvsReport);
  // Custom listview styles added because D4 and D5 does not allow detect
  // the ViewStyle change. Also, we can add more styles to this component:
  // Tile/Thumbnails/Custom...

  TcxNavigationEvent = procedure (Sender: TcxCustomInnerShellListView; APIDL: PItemIDList;
    ADisplayName: WideString) of object;
  TcxShellExecuteItemEvent = procedure (Sender: TObject; APIDL: PItemIDList; var AHandled: Boolean) of object;
  TcxShellAddFolderEvent = procedure(Sender: TObject; AFolder: TcxShellFolder; var ACanAdd: Boolean) of object;
  TcxShellChangeEvent = procedure(Sender: TObject; AEventID: DWORD; APIDL1, APIDL2: PItemIDList) of object;
  TcxShellCompareEvent = procedure(Sender: TObject;
    AItem1, AItem2: TcxShellFolder; var ACompare: Integer) of object;

  TcxShellListViewProducer = class(TcxCustomItemProducer)
  private
    FThumbnails: TCustomImageList;
    procedure CheckThumnals;
    function GetListView: TcxCustomInnerShellListView;
  protected
    function CanAddFolder(AFolder: TcxShellFolder): Boolean; override;
    function DoCompareItems(AItem1, AItem2: TcxShellFolder;
      out ACompare: Integer): Boolean; override;
    procedure DoDestroy; override;
    procedure DoSlowInitialization(AItem: TcxShellItemInfo); override;
    function GetEnumFlags: Cardinal; override;
    function GetItemsInfoGatherer: TcxShellItemsInfoGatherer; override;
    function GetThumbnailIndex(AItem: TcxShellItemInfo): Integer; override;
    function GetShowToolTip: Boolean; override;
    function SlowInitializationDone(AItem: TcxShellItemInfo): Boolean; override;

    property ListView: TcxCustomInnerShellListView read GetListView;
  public
    constructor Create(AOwner: TWinControl); override;
    procedure ProcessItems(AIFolder: IShellFolder; AFolderPIDL: PItemIDList;
      APreloadItemCount: Integer); override;
    procedure ProcessDetails(ShellFolder: IShellFolder; CharWidth: Integer); override;
  end;

  { TcxShellListRoot }

  TcxShellListRoot = class(TcxCustomShellRoot);

  TDropTargetType = (dttNone, dttOpenFolder, dttItem);

  IcxDropTarget = interface(IDropTarget)
  ['{F688E250-96A6-4222-AF9D-049EB6E7D05B}']
  end;

  { TcxShellDependedControls }

  TcxShellDependedControls = class(TList)
  private
    function GetItem(Index: Integer): TWinControl;
  protected
    procedure DoNavigate(AControl: TWinControl; AItem: PItemIDList);
    procedure DoSynchronizeRoot(AControl: TWinControl; ARoot: TcxCustomShellRoot);
  public
    procedure Add(AControl: TWinControl);
    procedure Navigate(AItem: PItemIDList);
    procedure Remove(AControl: TWinControl);
    procedure SynchronizeRoot(ARoot: TcxCustomShellRoot);
    //
    property Items[Index: Integer]: TWinControl read GetItem; default;
  end;

  { IcxShellDependedControls }

  IcxShellDependedControls = interface
  ['{93CE18BE-90F7-4CD1-B6DF-B578E2776DCA}']
    function GetDependedControls: TcxShellDependedControls;
  end;

  { IcxShellRoot }

  IcxShellRoot = interface
  ['{C819D99E-4368-400C-B4C4-0021E6B5C260}']
    function GetRoot: TcxCustomShellRoot;
  end;

  { TcxShellListViewOptions }

  TcxShellListViewOptions = class(TcxShellOptions)
  private
    FAutoExecute: Boolean;
    FAutoNavigate: Boolean;
    FCurrentFolderContextMenu: Boolean;
  public
    constructor Create(AOwner: TWinControl); override;
    procedure Assign(Source: TPersistent); override;
  published
    property AutoExecute: Boolean read FAutoExecute write FAutoExecute default True;
    property AutoNavigate: Boolean read FAutoNavigate write FAutoNavigate default True;
    property CurrentFolderContextMenu: Boolean read FCurrentFolderContextMenu write FCurrentFolderContextMenu default False;
    property FileMask;
  end;

  IcxDataObject = interface(IDataObject)
  ['{9A9CDB78-150E-4469-A551-608EFF415145}']
  end;

  TcxShellChangeNotifierData = record
    Handle: THandle;
    PIDL: PItemIDList;
  end;

  TcxShellIconSize = (isDefault, isExtraLarge, isJumbo);

  TcxShellListViewContextMenu = class(TcxShellCustomContextMenu)
  private
    FListView: TcxCustomInnerShellListView;
  protected
    function GetWindowHandle: THandle; override;
    procedure Populate; override;
  public
    constructor Create(AListView: TcxCustomInnerShellListView);
    destructor Destroy; override;
    property ListView: TcxCustomInnerShellListView read FListView;
  end;
  TcxShellListViewContextMenuClass = class of TcxShellListViewContextMenu;

  TcxShellListViewCurrentFolderContextMenu = class(TcxShellListViewContextMenu)
  private
    FLastSortColumnIndexCommandId: Cardinal;
    procedure AddCheckItem(const ACaption: string; AId: Cardinal; AIsChecked: Boolean; AMenu: HMenu = 0);
    procedure AddItem(const ACaption: string; AId: Cardinal; AMenu: HMenu = 0);
    procedure AddRadioGroup(AItems: array of string; AStartId, AStartPos: Cardinal; AItemIndex: Integer;
      AMenu: HMenu = 0);
    procedure AddSeparator(AMenu: HMenu = 0);
    function AddSubitem(const ACaption: string; AMenu: HMenu = 0): HMenu;
  protected
    procedure ExecuteMenuItemCommand(ACommandId: Cardinal); override;
    procedure Populate; override;
  end;

  { TcxCustomInnerShellListView }

  TcxCustomInnerShellListView = class(TcxBaseInnerListView,
    IcxDropTarget, IcxShellDependedControls, IcxShellRoot)
  private
    FAppEvents: TApplicationEvents;
    FCurrentDropTarget: IcxDropTarget;
    FDependedControls: TcxShellDependedControls;
    FDragDropSettings: TcxDragDropSettings;
    FDraggedObject: IcxDataObject;
    FDropTargetItemIndex: Integer;
    FFakeThumbnailImages: TCustomImageList;
    FFirstUpdateItem: Integer;
    FInternalSmallImages: THandle;
    FIsThumbnailView: Boolean;
    FItemProducer: TcxShellListViewProducer;
    FItemsInfoGatherer: TcxShellItemsInfoGatherer;
    FLargeIconSize: TcxShellIconSize;
    FLargeImages: TCustomImageList;
    FLastUpdateItem: Integer;
    FListViewStyle: TcxListViewStyle;
    FNotificationLock: Boolean;
    FOptions: TcxShellListViewOptions;
    FRoot: TcxShellListRoot;
    FShellChangeNotifierData: TcxShellChangeNotifierData;
    FSorting: Boolean;
    FThumbnailOptions: TcxShellThumbnailOptions;
    FWasMouseRButtonPressed: Boolean;

    FAfterNavigation: TcxNavigationEvent;
    FBeforeNavigation: TcxNavigationEvent;
    FOnAddFolder: TcxShellAddFolderEvent;
    FOnCompare: TcxShellCompareEvent;
    FOnExecuteItem: TcxShellExecuteItemEvent;
    FOnRootChanged: TcxRootChangedEvent;
    FOnShellChange: TcxShellChangeEvent;

    procedure DestroySelectedPidlsList(ASelectedPidls: TList);
    function GetFolder(AIndex: Integer): TcxShellFolder;
    function GetFolderCount: Integer;
    function GetLargeImageListType: Integer;
    function GetRootField: TcxShellListRoot;
    function CreateSelectedPidlsList: TList;
    function IsThumbnailView: Boolean;
    procedure ResetSorting;
    procedure RootFolderChanged(Sender: TObject; Root: TcxCustomShellRoot);
    procedure RootSettingsChanged(Sender: TObject);
    procedure SetLargeIconSize(const Value: TcxShellIconSize);
    procedure SetListViewStyle(const Value: TcxListViewStyle);
    procedure SetDropTargetItemIndex(Value: Integer);
    procedure SetRootField(const Value: TcxShellListRoot);
    procedure SetSorting(const Value: Boolean);
    procedure ShellChangeNotify(AEventID: Longint; APidl1, APidl2: PItemIDList);
    procedure DSMSynchronizeRoot(var Message: TMessage); message DSM_SYNCHRONIZEROOT;
    procedure DsmSystemShellChangeNotify(var Message: TMessage); message DSM_SYSTEMSHELLCHANGENOTIFY;
  protected
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    function OwnerDataFetch(Item: TListItem; Request: TItemRequest): Boolean; override;
    function OwnerDataFind(Find: TItemFind; const FindString: string;
      const FindPosition: TPoint; FindData: Pointer; StartIndex: Integer;
      Direction: TSearchDirection; Wrap: Boolean): Integer; override;
    procedure ColClick(Column: TListColumn); override;
    procedure DblClick; override;
    procedure DoContextPopup(MousePos: TPoint; var Handled: Boolean); override;
    function CanEdit(Item: TListItem): Boolean; override;
    procedure Loaded; override;
    procedure Edit(const Item: TLVItem); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;

    procedure AdvancedDrawItem(Sender: TCustomListView; Item: TListItem;
      State: TCustomDrawState; Stage: TCustomDrawStage; var DefaultDraw: Boolean);
    procedure CheckLargeImages;
    procedure DisplayContextMenu(const APos: TPoint);
    function DoAddFolder(AFolder: TcxShellFolder): Boolean;
    procedure DoAfterNavigation;
    procedure DoBeforeNavigation(fqPidl: PItemIDList);
    procedure DoBeginDrag;
    function DoCompare(AItem1, AItem2: TcxShellFolder; out ACompare: Integer): Boolean; virtual;
    procedure DoNavigateTreeView;
    procedure DoOnIdle(Sender: TObject; var Done: Boolean);
    procedure DoProcessDefaultCommand(Item: TcxShellItemInfo); virtual;
    procedure DoProcessNavigation(Item: TcxShellItemInfo);
    function CheckFileMask(AFolder: TcxShellFolder): Boolean;
    procedure UpdateThumbnails;
    procedure CheckUpdateItems;
    procedure CreateColumns;
    procedure CreateDropTarget;
    procedure CreateChangeNotification;
    procedure GetDropTarget(pt: TPoint; out New: Boolean);
    function GetLargeIconSize: TSize;
    function GetPidlByItemIndex(AIndex: Integer): PItemIDList;
    function GetSortOrder(AColumnIndex: Integer): TdxSortOrder; override;
    function GetThumbnailSize: TSize;
    procedure Navigate(APIDL: PItemIDList); virtual;
    procedure RemoveChangeNotification;
    procedure RemoveColumns;
    procedure RemoveDropTarget;
    procedure SortColumnChanged; virtual;
    procedure ThumbnailOptionsChanged(Sender: TObject);
    function TryReleaseDropTarget: HResult;

    procedure CNNotify(var Message: TWMNotify); message CN_NOTIFY;
    procedure DsmDoNavigate(var Message: TMessage); message DSM_DONAVIGATE;
    procedure DsmNotifyUpdateContents(var Message: TMessage); message DSM_NOTIFYUPDATECONTENTS;
    procedure DsmNotifyUpdateItem(var Message: TMessage); message DSM_NOTIFYUPDATE;
    procedure DsmSetCount(var Message: TMessage); message DSM_SETCOUNT;
    procedure DsmShellChangeNotify(var Message: TMessage); message DSM_SHELLCHANGENOTIFY;

    // IcxDropTarget
    function DragEnter(const dataObj: IDataObject; grfKeyState: Longint;
      pt: TPoint; var dwEffect: Longint): HResult; stdcall;
    function IcxDropTarget.DragOver = IDropTargetDragOver;
    function IDropTargetDragOver(grfKeyState: Longint; pt: TPoint;
      var dwEffect: Longint): HResult; stdcall;
    function DragLeave: HResult; stdcall;
    function Drop(const dataObj: IDataObject; grfKeyState: Longint; pt: TPoint;
      var dwEffect: Longint): HResult; stdcall;
    // IcxShellDependedControls
    function GetDependedControls: TcxShellDependedControls;
    // IcxShellRoot
    function GetRoot: TcxCustomShellRoot;

    property CurrentDropTarget: IcxDropTarget read FCurrentDropTarget write FCurrentDropTarget;
    property DraggedObject: IcxDataObject read FDraggedObject write FDraggedObject;
    property DropTargetItemIndex: Integer read FDropTargetItemIndex write SetDropTargetItemIndex;
    property FirstUpdateItem: Integer read FFirstUpdateItem write FFirstUpdateItem;
    property ItemProducer: TcxShellListViewProducer read FItemProducer;
    property ItemsInfoGatherer: TcxShellItemsInfoGatherer read FItemsInfoGatherer;
    property LastUpdateItem: Integer read FLastUpdateItem write FLastUpdateItem;
    property Sorting: Boolean read FSorting write SetSorting default False;
    property OnAddFolder: TcxShellAddFolderEvent read FOnAddFolder write FOnAddFolder;
    property OnCompare: TcxShellCompareEvent read FOnCompare write FOnCompare;
    property OnExecuteItem: TcxShellExecuteItemEvent read FOnExecuteItem write FOnExecuteItem;
    property OnShellChange: TcxShellChangeEvent read FOnShellChange write FOnShellChange;
  public
    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;
    procedure BrowseParent;
    procedure ProcessTreeViewNavigate(APidl: PItemIDList);
    procedure Sort; overload;
    procedure Sort(AColumnIndex: Integer; AIsAscending: Boolean); overload;
    procedure UpdateContent;

    property DependedControls: TcxShellDependedControls read FDependedControls write FDependedControls;
    property DragDropSettings: TcxDragDropSettings read FDragDropSettings write FDragDropSettings;
    property FolderCount: Integer read GetFolderCount;
    property Folders[AIndex: Integer]: TcxShellFolder read GetFolder;
    property LargeIconSize: TcxShellIconSize read FLargeIconSize write SetLargeIconSize;
    property ListViewStyle: TcxListViewStyle read FListViewStyle write SetListViewStyle;
    property Options: TcxShellListViewOptions read FOptions write FOptions;
    property Root: TcxShellListRoot read GetRootField write SetRootField;
    property ThumbnailOptions: TcxShellThumbnailOptions read FThumbnailOptions write FThumbnailOptions;
    property AfterNavigation: TcxNavigationEvent read FAfterNavigation write FAfterNavigation;
    property BeforeNavigation: TcxNavigationEvent read FBeforeNavigation write FBeforeNavigation;
    property OnRootChanged: TcxRootChangedEvent read FOnRootChanged write FOnRootChanged;
  end;

  TcxShellTreeRoot = class(TcxCustomShellRoot);

  TcxShellTreeItemProducer = class(TcxCustomItemProducer)
  private
    FNode: TTreeNode;
    FShellItemInfo: TcxShellItemInfo;
    FOnDestroy: TNotifyEvent;
    function GetTreeView: TcxCustomInnerShellTreeView;
  protected
    function CanAddFolder(AFolder: TcxShellFolder): Boolean; override;
    procedure DoSlowInitialization(AItem: TcxShellItemInfo); override;
    function GetEnumFlags: Cardinal; override;
    function GetItemsInfoGatherer: TcxShellItemsInfoGatherer; override;
    function GetShowToolTip: Boolean; override;
    procedure InitializeItem(AItem: TcxShellItemInfo); override;
    procedure CheckForSubitems(AItem: TcxShellItemInfo); override;
    function SlowInitializationDone(AItem: TcxShellItemInfo): Boolean; override;

    property Node: TTreeNode read FNode write FNode;
    property ShellItemInfo: TcxShellItemInfo read FShellItemInfo write FShellItemInfo;
    property TreeView: TcxCustomInnerShellTreeView read GetTreeView;
  public
    constructor Create(AOwner: TWinControl); override;
    destructor Destroy; override;
    procedure SetItemsCount(Count: Integer); override;
    procedure NotifyRemoveItem(Index: Integer); override;
    procedure NotifyAddItem(Index: Integer); override;
    procedure ProcessItems(AIFolder: IShellFolder; APIDL: PItemIDList;
      ANode: TTreeNode; APreloadItemCount: Integer); reintroduce; overload;
    function CheckUpdates: Boolean;
    property OnDestroy: TNotifyEvent read FOnDestroy write FOnDestroy;
  end;

  PcxShellTreeItemProducer = ^TcxShellTreeItemProducer;

  { TcxShellTreeViewOptions }

  TcxShellTreeViewOptions = class(TcxShellOptions)
  protected
    procedure DoNotifyUpdateContents; override;
  published
    property FileMask;
  end;

  TcxShellTreeViewStateData = record
    CurrentPath: PItemIDList;
    ExpandedNodeList: TList;
    TopItemIndex: Integer;
  end;

  TcxCustomInnerShellTreeView = class(TcxBaseInnerTreeView,
    IcxDropTarget, IcxShellDependedControls, IcxShellRoot)
  private
    FAppEvents: TApplicationEvents;
    FContextPopupItemProducer: TcxShellTreeItemProducer;
    FCurrentDropTarget: IcxDropTarget;
    FDependedControls: TcxShellDependedControls;
    FDragDropSettings: TcxDragDropSettings;
    FDraggedObject: IcxDataObject;
    FDragSourceNode: TTreeNode;
    FDragSourceNodeParent: TTreeNode;
    FInternalSmallImages: THandle;
    FIsChangeNotificationCreationLocked: Boolean;
    FIsDragging: Boolean;
    FIsUpdating: Boolean;
    FItemProducersList: TThreadList;
    FItemsInfoGatherer: TcxShellItemsInfoGatherer;
    FListView: TcxCustomInnerShellListView;
    FLockVisibleUpdate: Integer;
    FLockChange: Integer;
    FMaxVisibleNodeCount: Integer;
    FNavigation: Boolean;
    FOptions: TcxShellTreeViewOptions;
    FPrevTargetNode: TTreeNode;
    FRoot: TcxShellTreeRoot;
    FShellChangeNotificationCreation: Boolean;
    FShellChangeNotifierData: TcxShellChangeNotifierData;
    FShowInfoTips: Boolean;
    FStateData: TcxShellTreeViewStateData;
    FTopNode: TTreeNode;
    FWasMouseRButtonPressed: Boolean;
    FOnAddFolder: TcxShellAddFolderEvent;
    FOnRootChanged: TcxRootChangedEvent;
    FOnShellChange: TcxShellChangeEvent;
    procedure CheckUpdates(ANode: TTreeNode);
    procedure ContextPopupItemProducerDestroyHandler(Sender: TObject);
    function CreateShellNode(AParentNode: TTreeNode;
      AShellItem: TcxShellItemInfo): TTreeNode;
    function GetFolder(AIndex: Integer): TcxShellFolder;
    function GetFolderCount: Integer;
    function GetNodeFromItem(const Item: TTVItem): TTreeNode;
    function GetItemProducer(ANode: TTreeNode): TcxShellTreeItemProducer;
    function GetRootField: TcxShellTreeRoot;
    function GetShellItemInfo(ANode: TTreeNode): TcxShellItemInfo;
    procedure LockUpdateVisibleInfo;
    procedure RestoreTreeState;
    procedure RootFolderChanged(Sender: TObject; Root: TcxCustomShellRoot);
    procedure RootSettingsChanged(Sender: TObject);
    procedure SaveTreeState;
    procedure SetListView(Value: TcxCustomInnerShellListView);
    procedure SetPrevTargetNode(const Value: TTreeNode);
    procedure SetRootField(const Value: TcxShellTreeRoot);
    procedure SetShowInfoTips(Value: Boolean);
    procedure ShellChangeNotify(AEventID: Longint; APidl1, APidl2: PItemIDList);
    procedure ShowToolTipChanged(Sender: TObject);
    procedure UnlockUpdateVisibleInfo;

    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure DSMShellTreeChangeNotify(var Message: TMessage); message DSM_SHELLTREECHANGENOTIFY;
    procedure DSMShellTreeRestoreCurrentPath(var Message: TMessage);
      message DSM_SHELLTREERESTORECURRENTPATH;
    procedure DSMSynchronizeRoot(var Message: TMessage); message DSM_SYNCHRONIZEROOT;
    procedure DSMSystemShellChangeNotify(var Message: TMessage); message DSM_SYSTEMSHELLCHANGENOTIFY;

    property CurrentDropTarget: IcxDropTarget read FCurrentDropTarget write FCurrentDropTarget;
    property DraggedObject: IcxDataObject read FDraggedObject write FDraggedObject;
    property ItemProducersList: TThreadList read FItemProducersList;
    property Navigation: Boolean read FNavigation write FNavigation;
    property PrevTargetNode: TTreeNode read FPrevTargetNode write SetPrevTargetNode;
  protected
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure Change(Node: TTreeNode); override;
    function CanEdit(Node: TTreeNode): Boolean; override;
    procedure Edit(const Item: TTVItem); override;
    function CanExpand(Node: TTreeNode): Boolean; override;
    procedure Delete(Node: TTreeNode); override;
    procedure Expand(Node: TTreeNode); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DoContextPopup(MousePos: TPoint; var Handled: Boolean); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Resize; override;
    // IcxShellDependedControls
    function GetDependedControls: TcxShellDependedControls;
    // IcxShellRoot
    function GetRoot: TcxCustomShellRoot;

    procedure AddItemProducer(Producer:TcxShellTreeItemProducer);
    procedure AdjustControlParams;
    function CheckFileMask(AFolder: TcxShellFolder): Boolean;
    procedure CreateChangeNotification(ANode: TTreeNode = nil);
    procedure CreateDropTarget;
    function DoAddFolder(AFolder: TcxShellFolder): Boolean;
    procedure DoBeginDrag;
    procedure DoNavigateListView;
    procedure DoOnIdle(Sender: TObject; var Done: Boolean);
    procedure DragDropSettingsChanged(Sender: TObject); virtual;
    procedure GetDropTarget(out ANew: Boolean; APoint: TPoint);
    function GetNodeByPIDL(APIDL: PItemIDList): TTreeNode;
    function IsDragDropEnabled: Boolean; virtual;
    function IsLoading: Boolean; virtual;
    procedure RemoveChangeNotification;
    procedure RemoveDropTarget;
    procedure RemoveItemProducer(AProducer: TcxShellTreeItemProducer);
    function TryReleaseDropTarget: HResult;
    procedure UpdateItem(AItem: TcxShellItemInfo);
    procedure UpdateVisibleItems;

    procedure DsmSetCount(var Message: TMessage); message DSM_SETCOUNT;
    procedure DsmNotifyUpdateItem(var Message: TMessage); message DSM_NOTIFYUPDATE;
    procedure DsmNotifyRemoveItem(var Message: TMessage); message DSM_NOTIFYREMOVEITEM;
    procedure DsmNotifyAddItem(var Message: TMessage); message DSM_NOTIFYADDITEM;
    procedure DsmNotifyUpdateContents(var Message: TMessage); message DSM_NOTIFYUPDATECONTENTS;
    procedure DsmShellChangeNotify(var Message: TMessage); message DSM_SHELLCHANGENOTIFY;
    procedure DsmDoNavigate(var Message: TMessage); message DSM_DONAVIGATE;
    procedure CNNotify(var Message: TWMNotify); message CN_NOTIFY;
    // IcxDropTarget methods
    function DragEnter(const dataObj: IDataObject; grfKeyState: Longint;
      pt: TPoint; var dwEffect: Longint): HResult; stdcall;
    function IcxDropTarget.DragOver = IDropTargetDragOver;
    function IDropTargetDragOver(grfKeyState: Longint; pt: TPoint;
      var dwEffect: Longint): HResult; stdcall;
    function DragLeave: HResult; stdcall;
    function Drop(const dataObj: IDataObject; grfKeyState: Longint; pt: TPoint;
      var dwEffect: Longint): HResult; stdcall;

    property ItemsInfoGatherer: TcxShellItemsInfoGatherer read FItemsInfoGatherer;
    property OnAddFolder: TcxShellAddFolderEvent read FOnAddFolder write FOnAddFolder;
    property OnShellChange: TcxShellChangeEvent read FOnShellChange write FOnShellChange;
  public
    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;
    procedure UpdateContent;
    procedure UpdateNode(ANode: TTreeNode; AFast: Boolean);

    property DependedControls: TcxShellDependedControls read FDependedControls;
    property DragDropSettings:TcxDragDropSettings read FDragDropSettings write FDragDropSettings;
    property FolderCount: Integer read GetFolderCount;
    property Folders[AIndex: Integer]: TcxShellFolder read GetFolder;
    property ListView: TcxCustomInnerShellListView read FListView write SetListView;
    property Options: TcxShellTreeViewOptions read FOptions write FOptions;
    property Root: TcxShellTreeRoot read GetRootField write SetRootField;
    property ShowInfoTips: Boolean read FShowInfoTips write SetShowInfoTips default False;
    property OnRootChanged: TcxRootChangedEvent read FOnRootChanged write FOnRootChanged;
  end;

  { TcxShellImageList }

  TcxShellImageList = class(TcxBaseImageList)
  public
    constructor Create(ASizeFlags: Integer); reintroduce;
  end;

function cxSetShellControl(AControl, AValue: TWinControl; var AFieldValue: TWinControl): Boolean;

function cxShellGetImageList(AFlags: Integer): THandle;
function cxShellIsZipFile(AFolder: TcxShellFolder): Boolean;
procedure cxShellGetNotifyParams(Message: TMessage; out AEventID: Integer; out APidl1, APidl2: PItemIDList);
procedure cxShellRegisterChangeNotifier(ANotifierPIDL: PItemIDList; AWnd: HWND;
  ANotificationMessage: Cardinal; AWatchSubtree: Boolean; var ANotifierData: TcxShellChangeNotifierData);
procedure cxShellUnregisterChangeNotifier(var ANotifierData: TcxShellChangeNotifierData);
implementation

uses
  Forms, Math, cxGeometry, dxDPIAwareUtils;

type
  TcxShellOptionsAccess = class(TcxShellOptions);
  TcxShellThumbnailOptionsAccess = class(TcxShellThumbnailOptions);
  PPItemIDList = ^PItemIDList;
  TFontAccess = class(TFont);

var
  NavigationLock: Boolean;

procedure cxShellInvokeContextMenuCommand(AShellFolder: IShellFolder; AItemPIDLList: TList; const ACommandStr: AnsiString);
var
  AIContextMenu : IContextMenu;
  AInvokeCommandInfo : TCmInvokeCommandInfo;
  APIDLs: PITEMIDLISTARRAY;
begin
  if AShellFolder <> nil then
  begin
    APIDLs := CreatePidlArrayFromList(AItemPIDLList);
    try
      if AShellFolder.GetUIObjectOf(0, AItemPIDLList.Count, APIDLs[0], IID_IContextMenu, nil, AIContextMenu) = S_OK then
      begin
        ZeroMemory(@AInvokeCommandInfo, SizeOf(AInvokeCommandInfo));
        AInvokeCommandInfo.cbSize := SizeOf(TCmInvokeCommandInfo);
        AInvokeCommandInfo.lpVerb := PAnsiChar(ACommandStr);
        AIContextMenu.InvokeCommand(AInvokeCommandInfo);
      end;
    finally
      DisposePidlArray(APIDLs);
    end;
  end;
end;

function cxShellIsClipboardCommandContextMenuShortCut(Key: Word; Shift: TShiftState): Boolean;
begin
  Result := (Shift = [ssCtrl]) and (Key in [Ord('C'), Ord('V'), Ord('X')]);
end;

function cxShellGetContextMenuCommandStrByShortCut(Key: Word; Shift: TShiftState): AnsiString;
begin
  Result := '';
  case Key of
    Ord('C'):
      if Shift = [ssCtrl] then
        Result := 'copy';
    Ord('V'):
      if Shift = [ssCtrl] then
        Result := 'paste';
    Ord('X'):
      if Shift = [ssCtrl] then
        Result := 'cut';
    VK_DELETE:
       Result := 'delete';
  end;
end;

function cxShellGetImageList(AFlags: Integer): THandle;
var
  AFileInfo: TSHFileInfo;
begin
  Result := SHGetFileInfo('C:\', 0, AFileInfo, SizeOf(AFileInfo), SHGFI_SYSICONINDEX or AFlags);
end;

function cxShellIsZipFile(AFolder: TcxShellFolder): Boolean;
begin
  Result := AFolder.StorageCapabilities * [sfscStream, sfscFolder] = [sfscStream, sfscFolder];
end;

procedure SetComCtlStyle(ACtl: TWinControl; AValue: Integer; AUseStyle: Boolean);
const
  AOperationMap: array[Boolean] of TdxSetOperation = (soSubtract, soAdd);
begin
  if ACtl.HandleAllocated then
    dxSetWindowStyle(ACtl.Handle, AValue, AOperationMap[AUseStyle]);
end;

procedure CheckDefaultDropEffect(grfKeyState: Integer; ADragDropSettings: TcxDragDropSettings; var dwEffect: Integer);
begin
  if grfKeyState and (MK_RBUTTON or MK_CONTROL or MK_SHIFT or MK_ALT) = 0 then
    dwEffect := ADragDropSettings.DefaultDropEffectAPI;
end;

procedure cxShellGetNotifyParams(Message: TMessage; out AEventID: Integer; out APidl1, APidl2: PItemIDList);
var
  APidls: PPidlList;
  ALock: THandle;
begin
  ALock := SHChangeNotification_Lock(Message.WParam, Message.LParam, APidls, AEventID);
  try
    APidl1 := GetPidlCopy(APidls^[0]);
    APidl2 := GetPidlCopy(APidls^[1]);
  finally
    SHChangeNotification_UnLock(ALock);
  end;
end;

procedure cxShellRegisterChangeNotifier(ANotifierPIDL: PItemIDList;
  AWnd: HWND; ANotificationMessage: Cardinal; AWatchSubtree: Boolean;
  var ANotifierData: TcxShellChangeNotifierData);
var
  AItems: PSHChangeNotifyEntry;
begin
  if not EqualPIDLs(ANotifierData.PIDL, ANotifierPIDL) then
  begin
    cxShellUnregisterChangeNotifier(ANotifierData);
    if ANotifierPIDL <> nil then
    begin
      ANotifierData.PIDL := GetPidlCopy(ANotifierPIDL);
      New(AItems);
      try
        AItems.pidlPath := ANotifierData.PIDL;
        AItems.bWatchSubtree := AWatchSubtree;
        ANotifierData.Handle := SHChangeNotifyRegister(AWnd,
          SHCNF_ACCEPT_INTERRUPTS or SHCNF_ACCEPT_NON_INTERRUPTS or SHCNF_NO_PROXY,
          SHCNE_RENAMEITEM or SHCNE_CREATE or SHCNE_DELETE or SHCNE_MKDIR or
          SHCNE_RMDIR or SHCNE_ATTRIBUTES or SHCNE_UPDATEDIR or SHCNE_UPDATEITEM or
          SHCNE_UPDATEIMAGE or SHCNE_RENAMEFOLDER, ANotificationMessage, 1, AItems);
      finally
        Dispose(AItems);
      end;
    end;
  end;
end;

procedure cxShellUnregisterChangeNotifier(var ANotifierData: TcxShellChangeNotifierData);
begin
  if ANotifierData.Handle <> 0 then
  begin
    SHChangeNotifyUnregister(ANotifierData.Handle);
    ANotifierData.Handle := 0;
    DisposePidl(ANotifierData.PIDL);
    ANotifierData.PIDL := nil;
  end;
end;

function cxSetShellControl(AControl, AValue: TWinControl; var AFieldValue: TWinControl): Boolean;

  procedure SynchronizeDependedControlsRoot(AOwner: TWinControl);
  var
    ADependedControls: IcxShellDependedControls;
    ARoot: IcxShellRoot;
  begin
    if Supports(AOwner, IcxShellDependedControls, ADependedControls) then
    begin
      if Supports(AOwner, IcxShellRoot, ARoot) then
        ADependedControls.GetDependedControls.SynchronizeRoot(ARoot.GetRoot);
    end;
  end;

  procedure UpdateDependencies(AOwner, AControl: TWinControl; AOperation: TOperation);
  var
    ADependedControls: IcxShellDependedControls;
  begin
    if Supports(AOwner, IcxShellDependedControls, ADependedControls) then
      case AOperation of
        opInsert:
          ADependedControls.GetDependedControls.Add(AControl);
        opRemove:
          ADependedControls.GetDependedControls.Remove(AControl);
      end;
  end;

begin
  Result := AValue <> AFieldValue;
  if Result then
  begin
    if AFieldValue <> nil then
    begin
      UpdateDependencies(AFieldValue, AControl, opRemove);
      UpdateDependencies(AControl, AFieldValue, opRemove);
      AFieldValue.RemoveFreeNotification(AControl);
      AFieldValue := nil;
    end;
    if AValue <> nil then
    begin
      AFieldValue := AValue;
      AFieldValue.FreeNotification(AControl);
      UpdateDependencies(AControl, AFieldValue, opInsert);
      UpdateDependencies(AFieldValue, AControl, opInsert);
      SynchronizeDependedControlsRoot(AControl);
    end;
  end;
end;

{ TcxShellListViewOptions }

constructor TcxShellListViewOptions.Create(AOwner: TWinControl);
begin
  inherited Create(AOwner);
  FAutoNavigate := True;
  FAutoExecute := True;
end;

procedure TcxShellListViewOptions.Assign(Source: TPersistent);
begin
  if Source is TcxShellListViewOptions then
  begin
    AutoExecute := TcxShellListViewOptions(Source).AutoExecute;
    AutoNavigate := TcxShellListViewOptions(Source).AutoNavigate;
    CurrentFolderContextMenu := TcxShellListViewOptions(Source).CurrentFolderContextMenu;
  end;
  inherited Assign(Source);
end;

{ TcxShellDependedControls }

procedure TcxShellDependedControls.Add(AControl: TWinControl);
begin
  inherited Add(AControl);
end;

procedure TcxShellDependedControls.DoNavigate(AControl: TWinControl; AItem: PItemIDList);
begin
  if AControl.Parent <> nil then
  begin
    AControl.HandleNeeded;
    SendMessage(AControl.Handle, DSM_DONAVIGATE, WPARAM(AItem), 0);
  end;
end;

procedure TcxShellDependedControls.DoSynchronizeRoot(AControl: TWinControl; ARoot: TcxCustomShellRoot);
begin
  if AControl.HandleAllocated then
    SendMessage(AControl.Handle, DSM_SYNCHRONIZEROOT, WPARAM(ARoot), 0);
end;

procedure TcxShellDependedControls.Navigate(AItem: PItemIDList);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    DoNavigate(Items[I], AItem);
end;

procedure TcxShellDependedControls.Remove(AControl: TWinControl);
begin
  if Self <> nil then
    inherited Remove(AControl);
end;

procedure TcxShellDependedControls.SynchronizeRoot(ARoot: TcxCustomShellRoot);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    DoSynchronizeRoot(Items[I], ARoot);
end;

function TcxShellDependedControls.GetItem(Index: Integer): TWinControl;
begin
  Result := TWinControl(inherited Items[Index]);
end;

{ TcxCustomInnerShellListView }

constructor TcxCustomInnerShellListView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDependedControls := TcxShellDependedControls.Create;
  FDragDropSettings := TcxDragDropSettings.Create;
  FDropTargetItemIndex := -1;
  FFirstUpdateItem := -1;
  FLargeImages := TImageList.Create(nil);
  FLargeImages.ShareImages := True;
  FFakeThumbnailImages := TImageList.Create(nil);

  CheckLargeImages;
  FInternalSmallImages := cxShellGetImageList(SHGFI_SMALLICON);

  FItemProducer := TcxShellListViewProducer.Create(Self);
  FItemsInfoGatherer := TcxShellItemsInfoGatherer.Create(Self);
  FLastUpdateItem := -1;
  FOptions := TcxShellListViewOptions.Create(Self);
  FThumbnailOptions := TcxShellThumbnailOptions.Create(Self);
  TcxShellThumbnailOptionsAccess(FThumbnailOptions).OnChange := ThumbnailOptionsChanged;
  FRoot := TcxShellListRoot.Create(Self, 0);
  FRoot.OnFolderChanged := RootFolderChanged;
  FRoot.OnSettingsChanged := RootSettingsChanged;

  DoubleBuffered := True;
  LargeImages := FLargeImages;
  DragMode := dmManual;
  HideSelection := False;
  OwnerData := True;
  FAppEvents := TApplicationEvents.Create(nil);
  FAppEvents.OnIdle := DoOnIdle;
end;

destructor TcxCustomInnerShellListView.Destroy;
begin
  FreeAndNil(FAppEvents);
  RemoveChangeNotification;
  FreeAndNil(FItemProducer);
  FreeAndNil(FItemsInfoGatherer);
  FreeAndNil(FRoot);
  FreeAndNil(FOptions);
  FreeAndNil(FThumbnailOptions);
  FreeAndNil(FFakeThumbnailImages);
  FreeAndNil(FLargeImages);
  FreeAndNil(FDragDropSettings);
  FreeAndNil(FDependedControls);
  inherited Destroy;
end;

procedure TcxCustomInnerShellListView.BrowseParent;
var
  APIDL: PItemIDList;
begin
  APIDL := GetPidlParent(ItemProducer.FolderPidl);
  try
    Navigate(APIDL);
  finally
    DisposePidl(APIDL);
  end;
end;

function TcxCustomInnerShellListView.CanEdit(Item: TListItem): Boolean;
begin
  Result := True;
  if Item = nil then
     Exit;
  if Item.Index > ItemProducer.Items.Count - 1 then
  begin
    Result := False;
    Exit;
  end;
  Result := ItemProducer.ItemInfo[Item.Index].CanRename and inherited CanEdit(Item);
end;

procedure TcxCustomInnerShellListView.CNNotify(var Message: TWMNotify);

  function GetOverlayIndex: Integer;
  var
    AItemData: TcxShellItemInfo;
  begin
    AItemData := ItemProducer.ItemInfo[PLVDispInfo(Message.NMHdr)^.item.iItem];
    AItemData.CheckUpdate(ItemProducer.ShellFolder, ItemProducer.FolderPidl, False);
    Result := AItemData.OverlayIndex;
  end;

begin
  if csDestroying in ComponentState then
    Exit;
  case Message.NMHdr^.code of
    LVN_BEGINDRAG, LVN_BEGINRDRAG:
      begin
        if not DragDropSettings.AllowDragObjects then
        begin
          inherited;
          Exit;
        end;
        if SelCount <= 0 then
          Exit;
        DoBeginDrag;
      end;
    LVN_BEGINLABELEDIT:
      begin
        with PLVDispInfo(Message.NMHdr)^.item do
          mask := mask and not LVIF_PARAM;  // because of GetItem bug
        inherited;
      end;
    LVN_GETINFOTIP:
      ItemProducer.DoGetInfoTip(Handle, PNMLVGetInfoTip(Message.NMHdr)^.iItem,
        PNMLVGetInfoTip(Message.NMHdr)^.pszText,
        PNMLVGetInfoTip(Message.NMHdr)^.cchTextMax);
    LVN_GETDISPINFO:
      begin
        inherited;
        with PLVDispInfo(Message.NMHdr)^.item do
          if InRange(iItem, 0, ItemProducer.Items.Count - 1) and
            (mask and LVIF_IMAGE <> 0) and (iSubItem = 0) then
          begin
            state := IndexToOverlayMask(GetOverlayIndex + 1);
            stateMask := ILD_OVERLAYMASK;
            mask := mask or LVIF_STATE;
          end;
      end;
    LVN_ODFINDITEM:
      begin
        if (PNMLVFindItem(Message.NMHdr)^.lvfi.flags and $4) <> 0 then {LVFI_SUBSTRING}
          PNMLVFindItem(Message.NMHdr)^.lvfi.flags := PNMLVFindItem(Message.NMHdr)^.lvfi.flags or LVFI_PARTIAL;
        inherited;
      end;
    else
      inherited;
  end;
end;

procedure TcxCustomInnerShellListView.DsmDoNavigate(var Message: TMessage);
begin
  ProcessTreeViewNavigate(PItemIDList(Message.WParam));
end;

procedure TcxCustomInnerShellListView.DsmNotifyUpdateContents(
  var Message: TMessage);
begin
  if not (csLoading in ComponentState) then
    CheckUpdateItems;
end;

procedure TcxCustomInnerShellListView.DsmNotifyUpdateItem(
  var Message: TMessage);
begin
  UpdateItems(Message.WParam, Message.WParam);
end;

procedure TcxCustomInnerShellListView.DsmSetCount(var Message: TMessage);
begin
  Items.Count := Message.WParam;
  ItemFocused := nil;
  Selected := nil;
end;

procedure TcxCustomInnerShellListView.DsmShellChangeNotify(
  var Message: TMessage);
begin
  ShellChangeNotify(Message.LParam, PPItemIDList(Message.WParam)^,
    PPItemIDList(Message.WParam + SizeOf(Pointer))^);
end;

// IcxDropTarget
function TcxCustomInnerShellListView.DragEnter(const dataObj: IDataObject;
  grfKeyState: Integer; pt: TPoint; var dwEffect: Integer): HResult;
var
  new: Boolean;
begin
  DraggedObject := IcxDataObject(dataObj);
  GetDropTarget(pt, new);
  if CurrentDropTarget = nil then
  begin
    dwEffect := DROPEFFECT_NONE;
    Result := S_OK;
  end
  else
  begin
    CheckDefaultDropEffect(grfKeyState, DragDropSettings, dwEffect);
    Result := CurrentDropTarget.DragEnter(dataObj, grfKeyState, pt, dwEffect);
  end;
end;

function TcxCustomInnerShellListView.DragLeave: HResult;
begin
  DraggedObject := nil;
  Result := TryReleaseDropTarget;
end;

function TcxCustomInnerShellListView.IDropTargetDragOver(grfKeyState: Integer; pt: TPoint;
  var dwEffect: Integer): HResult;
var
  New: Boolean;
begin
  GetDropTarget(pt, new);
  if CurrentDropTarget = nil then
  begin
    dwEffect := DROPEFFECT_NONE;
    Result := S_OK;
  end
  else
  begin
    FWasMouseRButtonPressed := grfKeyState and MK_RBUTTON <> 0;
    CheckDefaultDropEffect(grfKeyState, DragDropSettings, dwEffect);
    if New then
      Result := CurrentDropTarget.DragEnter(DraggedObject, grfKeyState, pt, dwEffect)
    else
      Result := S_OK;
    if Succeeded(Result) then
      Result := CurrentDropTarget.DragOver(grfKeyState, pt, dwEffect);
  end;
end;

function TcxCustomInnerShellListView.Drop(const dataObj: IDataObject;
  grfKeyState: Integer; pt: TPoint; var dwEffect: Integer): HResult;
var
  New: Boolean;
begin
  GetDropTarget(pt, new);
  if CurrentDropTarget = nil then
  begin
    dwEffect := DROPEFFECT_NONE;
    Result := S_OK;
  end
  else
  begin
    if not FWasMouseRButtonPressed then
      CheckDefaultDropEffect(grfKeyState, DragDropSettings, dwEffect);
    if New then
       Result := CurrentDropTarget.DragEnter(dataObj, grfKeyState, pt, dwEffect)
    else
       Result := S_OK;
    if Succeeded(Result) then
       Result := CurrentDropTarget.Drop(dataObj, grfKeyState, pt, dwEffect);
  end;
  DraggedObject := nil;
  TryReleaseDropTarget;
end;

procedure TcxCustomInnerShellListView.CreateWnd;
begin
  inherited CreateWnd;
  if HandleAllocated then
  begin
    if FInternalSmallImages <> 0 then
      SendMessage(Handle, LVM_SETIMAGELIST, LVSIL_SMALL, LParam(FInternalSmallImages));

    CreateDropTarget;
    if Root.Pidl = nil then
      Root.CheckRoot
    else
      CheckUpdateItems;
  end;
end;

procedure TcxCustomInnerShellListView.ColClick(Column: TListColumn);
begin
  if FSorting then
  begin
    if ItemProducer.SortColumn = Column.Index then
      ItemProducer.SortDescending := not ItemProducer.SortDescending
    else
      ItemProducer.SortColumn := Column.Index;
    SortColumnChanged;
  end;
  inherited ColClick(Column);
end;

procedure TcxCustomInnerShellListView.DblClick;
var
  AItem: TcxShellItemInfo;
begin
  if Options.AutoNavigate and (Selected <> nil) then
  begin
    ItemProducer.LockRead;
    try
      AItem := ItemProducer.ItemInfo[Selected.Index];
      if AItem.IsFolder then
      begin
        if (AItem.Folder.ShellFolder <> nil) then
          DoProcessNavigation(AItem);
      end
      else
        if Options.AutoExecute then
          DoProcessDefaultCommand(AItem);
    finally
      ItemProducer.UnlockRead;
    end;
  end;
  inherited DblClick;
end;

procedure TcxCustomInnerShellListView.DestroyWnd;
begin
  RemoveChangeNotification;
  RemoveColumns;
  RemoveDropTarget;
  inherited DestroyWnd;
end;

procedure TcxCustomInnerShellListView.DoContextPopup(MousePos: TPoint; var Handled: Boolean);
begin
  if Options.ContextMenus and (SelCount > 0) or (SelCount = 0) and Options.CurrentFolderContextMenu then
  begin
    Handled := True;
    ItemProducer.LockRead;
    try
      DisplayContextMenu(ClientToScreen(MousePos));
    finally
      ItemProducer.UnlockRead;
    end;
  end
  else
    inherited DoContextPopup(MousePos, Handled);
end;

procedure TcxCustomInnerShellListView.Edit(const Item: TLVItem);
var
  ATempItem: TcxShellItemInfo;
  ANewName: WideString;
  ANewPidl: PItemIDList;
  S: string;
begin
  if (ItemProducer.Items.Count - 1) < Item.iItem then
    Exit;
  S := Item.pszText;
  if Assigned(OnEdited) then OnEdited(Self, Items.Item[Item.iItem], S);
  ATempItem := ItemProducer.ItemInfo[Item.iItem];
  ANewName := S;
  ItemProducer.ShellFolder.SetNameOf(Handle, ATempItem.pidl, PWideChar(ANewName),
    SHGDN_INFOLDER or SHGDN_FORPARSING, ANewPidl);
  try
    ATempItem.SetNewPidl(ItemProducer.ShellFolder, ItemProducer.FolderPidl, ANewPidl);
  finally
    DisposePidl(ANewPidl);
  end;
end;

procedure TcxCustomInnerShellListView.KeyDown(var Key: Word;
  Shift: TShiftState);

  procedure InvokeContextMenuCommand(const ACommandStr: AnsiString);
  var
    AShellFolder : IShellFolder;
    AItemPIDLList: TList;
  begin
    AItemPIDLList := CreateSelectedPidlsList;
    try
      if Selected <> nil then
        AShellFolder := Root.ShellFolder
      else
      begin
        AShellFolder := Root.Folder.ParentShellFolder;
        AItemPIDLList.Add(GetPidlCopy(Root.Folder.RelativePIDL));
      end;
      cxShellInvokeContextMenuCommand(AShellFolder, AItemPIDLList, ACommandStr);
    finally
      DestroySelectedPidlsList(AItemPIDLList);
    end;
  end;

begin
  inherited KeyDown(Key, Shift);
  if not IsEditing then
    case Key of
      VK_RETURN:
        DblClick;
      VK_BACK:
        if Options.AutoNavigate then
          BrowseParent;
      VK_F5:
        UpdateContent;
    else
      if Options.ContextMenus and (cxShellIsClipboardCommandContextMenuShortCut(Key, Shift) or
        (Key = VK_DELETE) and (Selected <> nil)) then
        InvokeContextMenuCommand(cxShellGetContextMenuCommandStrByShortCut(Key, Shift));
    end;
end;

procedure TcxCustomInnerShellListView.AdvancedDrawItem(Sender: TCustomListView; Item: TListItem;
  State: TCustomDrawState; Stage: TCustomDrawStage; var DefaultDraw: Boolean);

  procedure CalculateRects(var ABounds, AIconRect, ATextRect: TRect);
  begin
    ABounds := Item.DisplayRect(drBounds);
    AIconRect := Item.DisplayRect(drIcon);
    ATextRect := Item.DisplayRect(drLabel);
  end;

  procedure DrawItemBackground(ACanvas: TcxCanvas; const ABounds: TRect);

    function GetItemState: Integer;
    begin
      if Focused then
        Result := MPI_HOT
      else
        if Item.Selected then
          if (cdsHot in State) or IsEditing then
            Result := MPI_HOT  // pressed
          else
            Result := MPI_DISABLEDHOT
        else
          Result := MPI_HOT;
    end;

  var
    ATheme: TdxTheme;
  begin
    ACanvas.FillRect(ABounds, Color);
    if (Item.Selected or (cdsHot in State)) and not IsWinXP and IsXPManifestEnabled and AreVisualStylesAvailable([totMenu]) then
    begin
      ATheme := OpenTheme(totMenu);
      if ATheme <> 0 then
        DrawThemeBackground(ATheme, ACanvas.Handle, MENU_POPUPITEM, GetItemState, @ABounds)
    end;
  end;

  procedure DrawThumbnailFrame(ACanvas: TcxCanvas; const AIconRect: TRect);
  var
    ARect: TRect;
    AColor: TColor;
  begin
    ARect := cxRectCenter(AIconRect, GetThumbnailSize);
    if Item.Selected then
    begin
      ACanvas.SaveClipRegion;
      ACanvas.ExcludeClipRect(ARect);
      if Focused then
        AColor := clMenuHighlight
      else
        AColor := clBtnFace;
      ACanvas.FillRect(cxRectInflate(ARect, 3, 3), AColor);
      ACanvas.RestoreClipRegion;
    end
    else
      ACanvas.FrameRect(ARect, clBtnFace);
  end;

  procedure DrawThumbnail(ACanvas: TcxCanvas; const AIconRect: TRect; AShellItem: TcxShellItemInfo);
  var
    ARect: TRect;
  begin
    ARect := cxRectCenter(AIconRect, GetThumbnailSize);
    if IsWinVistaOrLater and not cxIsVCLThemesEnabled then
      cxDrawImage(ACanvas.Handle, ARect, AIconRect, nil, ItemProducer.FThumbnails,  AShellItem.ThumbnailIndex, idmNormal)
    else
      ItemProducer.FThumbnails.Draw(ACanvas.Canvas, ARect.Left, ARect.Top, AShellItem.ThumbnailIndex);
  end;

  procedure DrawIcon(ACanvas: TcxCanvas; const AIconRect: TRect; AShellItem: TcxShellItemInfo);
  var
    ARect: TRect;
  begin
    ARect := cxRectCenter(AIconRect, GetLargeIconSize);
    FLargeImages.DrawOverlay(ACanvas.Canvas, ARect.Left, ARect.Top, AShellItem.IconIndex,
      AShellItem.OverlayIndex, dsTransparent, itImage);
  end;

  procedure DrawItemText(ACanvas: TcxCanvas; ATextRect: TRect; const AText: string);
  var
    AFlags: Integer;
  begin
    if Item.Selected and not AreVisualStylesAvailable([totButton]) then
    begin
      ACanvas.FillRect(ATextRect, clMenuHighlight);
      ACanvas.Font.Color := clHighlightText;
    end;
    Inc(ATextRect.Left, 2);
    Dec(ATextRect.Right, 2);
    AFlags := DT_CENTER or DT_WORDBREAK;
    if Item.Selected then
      AFlags := AFlags or DT_EDITCONTROL
    else
      AFlags := AFlags or DT_WORD_ELLIPSIS;

    cxDrawText(ACanvas.Handle, AText, ATextRect, AFlags);
  end;

  procedure InternalDrawItem(ACanvas: TcxCanvas; const ABounds, AIconRect, ATextRect: TRect; ADefaultDrawText: Boolean;
    AShellItem: TcxShellItemInfo);
  begin
    DrawItemBackground(ACanvas, ABounds);
    if IsWinXP then
      DrawThumbnailFrame(ACanvas, AIconRect);
    if AShellItem.HasThumbnail then
      DrawThumbnail(ACanvas, AIconRect, AShellItem)
    else
      DrawIcon(ACanvas, AIconRect, AShellItem);
    if not ADefaultDrawText and not (Item.Selected and IsEditing) then
      DrawItemText(ACanvas, ATextRect, Item.Caption);
  end;

var
  ABounds, AIconRect, ATextRect: TRect;
  AShellItem: TcxShellItemInfo;
  ABitmap: TcxBitmap32;
  ADefaultDrawText: Boolean;
begin
  if not InRange(Item.Index, 0, ItemProducer.Items.Count - 1) then
    Exit;

  if Stage = cdPrePaint then
  begin
    AShellItem := ItemProducer.ItemInfo[Item.Index];
    CalculateRects(ABounds, AIconRect, ATextRect);
    ADefaultDrawText := IsWinXP or not IsXPManifestEnabled;
    TFontAccess(Canvas.Font).Changed;
    ABitmap := TcxBitmap32.CreateSize(ABounds);
    ABitmap.Canvas.Lock;
    try
      ABitmap.cxCanvas.WindowOrg := ABounds.TopLeft;
      ABitmap.Canvas.Font.Assign(Font);
      if UseRightToLeftAlignment then
        SetLayout(ABitmap.Canvas.Handle, LAYOUT_RTL or LAYOUT_BITMAPORIENTATIONPRESERVED);
      InternalDrawItem(ABitmap.cxCanvas, ABounds, AIconRect, ATextRect, ADefaultDrawText, AShellItem);
      cxBitBlt(Canvas.Handle, ABitmap.Canvas.Handle, ABounds, ABounds.TopLeft, SRCCOPY);
    finally
      ABitmap.Canvas.Unlock;
      ABitmap.Free;
    end;
    DefaultDraw := ADefaultDrawText;
  end;
end;

procedure TcxCustomInnerShellListView.CheckLargeImages;
var
  AHImages: HIMAGELIST;
begin
  if IsWinXPOrLater then
  begin
    SHGetImageList(GetLargeImageListType, IID_IImageList, AHImages);
    FLargeImages.Handle := AHImages;
  end
  else
    FLargeImages.Handle := cxShellGetImageList(SHGFI_LARGEICON);
end;

procedure TcxCustomInnerShellListView.DisplayContextMenu(const APos: TPoint);

  function GetContextMenuClass: TcxShellListViewContextMenuClass;
  begin
    if SelCount <> 0 then
      Result := TcxShellListViewContextMenu
    else
      Result := TcxShellListViewCurrentFolderContextMenu;
  end;

begin
  with GetContextMenuClass.Create(Self) do
  begin
    Popup(APos);
    Free;
  end;
end;

function TcxCustomInnerShellListView.DoAddFolder(AFolder: TcxShellFolder): Boolean;
begin
  Result := AFolder.IsFolder and
    (Options.ShowNonFolders or Options.ShowZipFilesWithFolders or not cxShellIsZipFile(AFolder)) or
    not AFolder.IsFolder and CheckFileMask(AFolder);
  if Assigned(FOnAddFolder) then
    FOnAddFolder(Self, AFolder, Result);
end;

procedure TcxCustomInnerShellListView.DoAfterNavigation;
begin
  if Assigned(AfterNavigation) then
     AfterNavigation(Self, Root.Pidl, Root.CurrentPath);
end;

procedure TcxCustomInnerShellListView.DoBeforeNavigation(fqPidl: PItemIDList);
var
  Desktop: IShellFolder;
  tempPath: WideString;
  StrName: TStrRet;
begin
  if Failed(SHGetDesktopFolder(Desktop)) then
     Exit;
  if Succeeded(Desktop.GetDisplayNameOf(fqPidl, SHGDN_NORMAL or SHGDN_FORPARSING, StrName)) then
     tempPath := GetTextFromStrRet(StrName, fqPidl)
  else
     tempPath := '';
  if Assigned(BeforeNavigation) then
     BeforeNavigation(Self, fqPidl, tempPath);
end;

procedure TcxCustomInnerShellListView.DoBeginDrag;
var
  ASelectedPidls: TList;
  APidlList: PITEMIDLISTARRAY;
  pDataObject: IDataObject;
  pDropSource: IcxDropSource;
  dwEffect: Integer;
begin
  ASelectedPidls := CreateSelectedPidlsList;
  try
    APidlList := CreatePidlArrayFromList(ASelectedPidls);
    try
      if Failed(ItemProducer.ShellFolder.GetUIObjectOf(Handle, SelCount, APidlList[0],
        IDataObject, nil, Pointer(pDataObject))) then
         Exit;
      pDropSource := TcxDropSource.Create(Self);
      dwEffect := DragDropSettings.DropEffectAPI;
      DoDragDrop(pDataObject, pDropSource, dwEffect, dwEffect);
    finally
      DisposePidlArray(APidlList);
    end;
  finally
    DestroySelectedPidlsList(ASelectedPidls);
  end;
end;

function TcxCustomInnerShellListView.DoCompare(AItem1, AItem2: TcxShellFolder;
  out ACompare: Integer): Boolean;
begin
  Result := Assigned(FOnCompare);
  if Result then
    FOnCompare(Self, AItem1, AItem2, ACompare);
end;

procedure TcxCustomInnerShellListView.DoNavigateTreeView;
var
  ATempPidl: PItemIDList;
begin
  if not NavigationLock and (DependedControls.Count > 0) then
  begin
    ATempPidl := GetPidlCopy(Root.Pidl);
    try
      DependedControls.Navigate(ATempPidl);
    finally
      DisposePidl(ATempPidl);
    end;
  end;
end;

procedure TcxCustomInnerShellListView.DoOnIdle(Sender: TObject; var Done: Boolean);
var
  AList: TList;
  AItem: TcxShellItemInfo;
  I: Integer;
begin
  AList := FItemsInfoGatherer.ProcessedItems.LockList;
  try
//    if AList.Count > 0 then
//      I := ItemProducer.Items.IndexOf(TcxShellItemInfo(AList.First));
//    while (AList.Count > 0) and (ListView_IsItemVisible(Handle, I) = 0) do
//    begin
//      AList.Delete(0);
//      if AList.Count > 0 then
//        I := ItemProducer.Items.IndexOf(TcxShellItemInfo(AList.First));
//    end;
    if AList.Count > 0 then
      AItem := TcxShellItemInfo(AList.Extract(AList.First))
    else
      AItem := nil;
  finally
    FItemsInfoGatherer.ProcessedItems.UnlockList;
  end;
  if AItem <> nil then
  begin
    I := ItemProducer.Items.IndexOf(AItem);
    UpdateItems(I, I);
  end;
end;

procedure TcxCustomInnerShellListView.DoProcessDefaultCommand(Item: TcxShellItemInfo);
var
  fqPidl: PItemIDList;
  lpExecInfo: PShellExecuteInfo;
  AHandled: Boolean;
begin
  fqPidl := ConcatenatePidls(ItemProducer.FolderPidl, Item.pidl);
  AHandled := False;
  if Assigned(OnExecuteItem) then
    OnExecuteItem(Self, fqPidl, AHandled);
  if not AHandled then
    try
      New(lpExecInfo);
      try
        ZeroMemory(lpExecInfo, SizeOf(TShellExecuteInfo));
        lpExecInfo.cbSize := SizeOf(TShellExecuteInfo);
        lpExecInfo.fMask := SEE_MASK_INVOKEIDLIST;
        lpExecInfo.Wnd := Handle;
        lpExecInfo.lpIDList := fqPidl;
        lpExecInfo.nShow := SW_SHOW;
        ShellExecuteEx(lpExecInfo);
      finally
        Dispose(lpExecInfo);
      end;
    finally
      DisposePidl(fqPidl);
    end;
end;

procedure TcxCustomInnerShellListView.DoProcessNavigation(Item: TcxShellItemInfo);
var
  APIDL: PItemIDList;
begin
  if not Item.IsFolder then
    Exit;
  APIDL := ConcatenatePidls(ItemProducer.FolderPidl, Item.pidl);
  try
    Navigate(APIDL);
  finally
    DisposePidl(APIDL);
  end;
end;

function TcxCustomInnerShellListView.CheckFileMask(AFolder: TcxShellFolder): Boolean;
begin
  Result := Options.IsFileNameValid(ExtractFileName(AFolder.PathName));
end;

procedure TcxCustomInnerShellListView.UpdateThumbnails;
begin
  if IsThumbnailView then
  begin
    FIsThumbnailView := True;
    FFakeThumbnailImages.Width := ThumbnailOptions.Width;
    FFakeThumbnailImages.Height := ThumbnailOptions.Height;
    OnAdvancedCustomDrawItem := AdvancedDrawItem;
    DoubleBuffered := not IsWinXP;
    LargeImages := FFakeThumbnailImages;
  end
  else
  begin
    FIsThumbnailView := False;
    OnAdvancedCustomDrawItem := nil;
    DoubleBuffered := True;
    LargeImages := FLargeImages;
  end;
end;

procedure TcxCustomInnerShellListView.CheckUpdateItems;
var
  AEditingItemPidl: PItemIDList;
  AItemIndex: Integer;
begin
  if csLoading in ComponentState then
    Exit;
  AEditingItemPidl := nil;
  try
    if IsEditing then
      AEditingItemPidl := GetPidlCopy(GetPidlByItemIndex(Selected.Index));
    ItemProducer.ClearItems;
    if IsWindow(WindowHandle) then
    begin
      if not Root.IsValid then
        Items.Clear
      else
        if ItemProducer.Items.Count = 0 then
          ItemProducer.ProcessItems(Root.ShellFolder, Root.Pidl, PRELOAD_ITEMS_COUNT);
      CreateChangeNotification;
      Refresh;
    end;
    if AEditingItemPidl <> nil then
    begin
      AItemIndex := ItemProducer.GetItemIndexByPidl(AEditingItemPidl);
      if (AItemIndex >= 0) and (AItemIndex < Items.Count) then
        Items[AItemIndex].EditCaption;
    end;
  finally
    DisposePidl(AEditingItemPidl);
  end;
end;

procedure TcxCustomInnerShellListView.CreateColumns;
var
  i: Integer;
  Column: TListColumn;
begin
//  if ListViewStyle <> lvsReport then
//     Exit;
  Columns.BeginUpdate;
  try
    Columns.Clear;
    for i := 0 to ItemProducer.Details.Count - 1 do
    begin
      Column := Columns.Add;
      Column.Caption := ItemProducer.Details[i].Text;
      Column.Alignment := ItemProducer.Details[i].Alignment;
      Column.Width := ItemProducer.Details[i].Width;
    end;
  finally
    Columns.EndUpdate;
  end;
end;

procedure TcxCustomInnerShellListView.CreateDropTarget;
var
  AIDropTarget: IcxDropTarget;
begin
  GetInterface(IcxDropTarget, AIDropTarget);
  RegisterDragDrop(Handle, IDropTarget(AIDropTarget));
end;

procedure TcxCustomInnerShellListView.CreateChangeNotification;
begin
  if Options.TrackShellChanges then
    cxShellRegisterChangeNotifier(ItemProducer.FolderPidl, Handle, DSM_SYSTEMSHELLCHANGENOTIFY, False, FShellChangeNotifierData)
  else
    RemoveChangeNotification;
end;

procedure TcxCustomInnerShellListView.GetDropTarget(pt: TPoint; out New: Boolean);

  function GetDropTargetItemIndex: Integer;
  var
    AItem: TListItem;
    P: TPoint;
  begin
    Result := -1;
    P := dxMapWindowPoint(HWND_DESKTOP, Handle, P);
    AItem := GetItemAt(P.X, P.Y);
    if AItem <> nil then
      Result := AItem.Index;
  end;

var
  AItemIndex: Integer;
  tempDropTarget: IcxDropTarget;
  tempPidl: PItemIDList;
begin
  if not DragDropSettings.AllowDragObjects then
  begin
    CurrentDropTarget := nil;
    Exit;
  end;
  AItemIndex := GetDropTargetItemIndex;
  if AItemIndex = -1 then
  begin // There are no items selected, so drop target is current opened folder
    if (DropTargetItemIndex = -1) and (CurrentDropTarget <> nil) then
    begin
      New := False;
      Exit;
    end;
    TryReleaseDropTarget;
    New := True;
    if Failed(ItemProducer.ShellFolder.CreateViewObject(Handle,IDropTarget, tempDropTarget)) then
       Exit;
    CurrentDropTarget := tempDropTarget;
  end
  else
  begin // Use one of Items as Drop Target
    if AItemIndex = DropTargetItemIndex then
    begin
      New := False;
      Exit;
    end;
    TryReleaseDropTarget;
    New := True;
    tempPidl := GetPidlCopy(GetPidlByItemIndex(AItemIndex));
    try
      if Failed(ItemProducer.ShellFolder.GetUIObjectOf(Handle, 1, tempPidl, IDropTarget, nil, tempDropTarget)) then
         Exit;
    finally
      DisposePidl(tempPidl);
    end;
    CurrentDropTarget := tempDropTarget;
    DropTargetItemIndex := AItemIndex;
  end;
end;

function TcxCustomInnerShellListView.GetLargeIconSize: TSize;
begin
  Result := cxSize(FLargeImages.Width, FLargeImages.Height);
end;

function TcxCustomInnerShellListView.GetPidlByItemIndex(AIndex: Integer): PItemIDList;
begin
  Result := ItemProducer.ItemInfo[AIndex].pidl;
end;

function TcxCustomInnerShellListView.GetSortOrder(AColumnIndex: Integer): TdxSortOrder;
begin
  if FSorting and (AColumnIndex = FItemProducer.SortColumn) then
    if FItemProducer.SortDescending then
      Result := soDescending
    else
      Result := soAscending
  else
    Result := soNone;
end;

function TcxCustomInnerShellListView.GetThumbnailSize: TSize;
begin
  Result := cxSize(ThumbnailOptions.Width, ThumbnailOptions.Height);
end;

procedure TcxCustomInnerShellListView.Navigate(APIDL: PItemIDList);
begin
  if EqualPIDLs(APIDL, ItemProducer.FolderPidl) then
    Exit;
  Items.BeginUpdate;
  try
    DoBeforeNavigation(APIDL);
    Root.Pidl := APIDL;
    DoNavigateTreeView;
    DoAfterNavigation;
  finally
    Items.EndUpdate;
  end;
end;

procedure TcxCustomInnerShellListView.RemoveChangeNotification;
begin
  cxShellUnregisterChangeNotifier(FShellChangeNotifierData);
end;

procedure TcxCustomInnerShellListView.RemoveColumns;
begin
  Columns.Clear;
end;

procedure TcxCustomInnerShellListView.RemoveDropTarget;
begin
  RevokeDragDrop(Handle);
end;

procedure TcxCustomInnerShellListView.SortColumnChanged;
begin
  Sort;
  Refresh;
  HeaderInvalidate;
end;

procedure TcxCustomInnerShellListView.ThumbnailOptionsChanged(Sender: TObject);
begin
  UpdateThumbnails;
  CheckUpdateItems;
end;

procedure TcxCustomInnerShellListView.Loaded;
begin
  inherited Loaded;
  if csDesigning in ComponentState then
    Root.RootUpdated
  else
    CheckUpdateItems;
end;

function TcxCustomInnerShellListView.OwnerDataFetch(Item: TListItem; Request: TItemRequest): Boolean;
var
  AShellItem: TcxShellItemInfo;
  i: Integer;
begin
  Result := True;
  ItemProducer.LockRead;
  try
    if Item.Index >= ItemProducer.Items.Count then
      Exit;
    AShellItem := ItemProducer.ItemInfo[Item.Index];
    Item.Caption := AShellItem.Name;
    Item.ImageIndex := AShellItem.IconIndex;
    AShellItem.CheckUpdate(ItemProducer.ShellFolder, ItemProducer.FolderPidl, False);
    if not AShellItem.ThumbnailUpdated then
      FItemsInfoGatherer.RequestItemInfo(AShellItem);
    if ListViewStyle = lvsReport then
    begin
      if AShellItem.Details.Count = 0 then
        AShellItem.FetchDetails(Handle, ItemProducer.ShellFolder, ItemProducer.Details);
      for i := 0 to AShellItem.Details.Count - 1 do
          Item.SubItems.Add(AShellItem.Details[i]);
    end;
    Item.Cut := AShellItem.IsGhosted;
  finally
    ItemProducer.UnlockRead;
  end;
  Result := inherited OwnerDataFetch(Item, Request);
end;

function TcxCustomInnerShellListView.OwnerDataFind(Find: TItemFind; const FindString: string;
  const FindPosition: TPoint; FindData: Pointer; StartIndex: Integer;
  Direction: TSearchDirection; Wrap: Boolean): Integer;

  function IsItemSuitable(AIndex: Integer; AFind: TItemFind; const AFindString: string): Boolean;
  var
    ACaption: string;
  begin
    ACaption := ItemProducer.ItemInfo[AIndex].Name;
    if AFind = ifPartialString then
      ACaption := Copy(ACaption, 1, Length(FindString));
    Result := CompareText(FindString, ACaption) = 0;
  end;

  function FindItemByCaption(AFind: TItemFind; const AFindString: string;
    AStartIndex: Integer; AWrap: Boolean): Integer;
  var
    I: Integer;
  begin
    Result := -1;
    AStartIndex := EnsureRange(AStartIndex, 0, ItemProducer.Items.Count - 1);
    for I := AStartIndex to ItemProducer.Items.Count - 1 do
      if IsItemSuitable(I, AFind, AFindString) then
      begin
        Result := I;
        Break;
      end;
    if AWrap and (Result = -1) then
      for I := 0 to AStartIndex - 1 do
        if IsItemSuitable(I, AFind, AFindString) then
          begin
            Result := I;
            Break;
          end;
  end;

begin
  Result := inherited OwnerDataFind(Find, FindString, FindPosition, FindData,
    StartIndex, Direction, Wrap);
  if (Result = -1) and (Find in [ifPartialString, ifExactString]) then
    Result := FindItemByCaption(Find, FindString, StartIndex, Wrap);
end;

procedure TcxCustomInnerShellListView.SetDropTargetItemIndex(Value: Integer);
begin
  if FDropTargetItemIndex <> -1 then
    Items[FDropTargetItemIndex].DropTarget := False;
  FDropTargetItemIndex := Value;
  if FDropTargetItemIndex <> -1 then
    Items[FDropTargetItemIndex].DropTarget := True;
end;

procedure TcxCustomInnerShellListView.SetRootField(const Value: TcxShellListRoot);
begin
  FRoot.Assign(Value);
end;

procedure TcxCustomInnerShellListView.SetSorting(const Value: Boolean);
begin
  if FSorting <> Value then
  begin
    FSorting := Value;
    SortColumnChanged;
  end;
end;

procedure TcxCustomInnerShellListView.ShellChangeNotify(AEventID: Longint; APidl1, APidl2: PItemIDList);
begin
  if FNotificationLock then
     Exit;
  FNotificationLock := True;
  try
    CheckUpdateItems;
  finally
    FNotificationLock := False;
  end;
  if Assigned(FOnShellChange) then
    FOnShellChange(Self, AEventID, APidl1, APidl2);
end;

procedure TcxCustomInnerShellListView.DSMSynchronizeRoot(var Message: TMessage);
begin
  if not((Parent <> nil) and (csLoading in Parent.ComponentState)) then
    Root.Update(TcxCustomShellRoot(Message.WParam));
end;

procedure TcxCustomInnerShellListView.DsmSystemShellChangeNotify(
  var Message: TMessage);
var
  AEventID: Integer;
  APidl1, APidl2: PItemIDList;
begin
  cxShellGetNotifyParams(Message, AEventID, APidl1, APidl2);
  try
    ShellChangeNotify(AEventID, APidl1, APidl2);
  finally
    DisposePidl(APidl1);
    DisposePidl(APidl2);
  end;
end;

procedure TcxCustomInnerShellListView.DestroySelectedPidlsList(ASelectedPidls: TList);
var
  I: Integer;
begin
  try
    for I := 0 to ASelectedPidls.Count - 1 do
      DisposePidl(ASelectedPidls[I]);
  finally
    FreeAndNil(ASelectedPidls);
  end;
end;

function TcxCustomInnerShellListView.GetFolder(AIndex: Integer): TcxShellFolder;
begin
  Result := ItemProducer.ItemInfo[AIndex].Folder;
end;

function TcxCustomInnerShellListView.GetFolderCount: Integer;
begin
  Result := Items.Count;
end;

function TcxCustomInnerShellListView.GetLargeImageListType: Integer;
begin
  case FLargeIconSize of
    isDefault:
      Result := SHIL_LARGE;
    isExtraLarge:
      Result := SHIL_EXTRALARGE;
  else // isJumbo
    if IsWinVistaOrLater then
      Result := SHIL_JUMBO
    else
      Result := SHIL_EXTRALARGE;
  end;
end;

function TcxCustomInnerShellListView.GetRootField: TcxShellListRoot;
begin
  Result := FRoot;
end;

function TcxCustomInnerShellListView.GetDependedControls: TcxShellDependedControls;
begin
  Result := DependedControls;
end;

function TcxCustomInnerShellListView.GetRoot: TcxCustomShellRoot;
begin
  Result := FRoot;
end;

function TcxCustomInnerShellListView.CreateSelectedPidlsList: TList;
var
  AItem: TListItem;
  AItemPIDL: PItemIDList;
begin
  Result := TList.Create;
  AItem := Selected;
  while AItem <> nil do
  begin
    AItemPIDL := GetPidlByItemIndex(AItem.Index);
    if AItemPIDL <> nil then
      Result.Add(GetPidlCopy(AItemPIDL));
    AItem := GetNextItem(AItem, sdAll, [isSelected]);
  end;
end;

function TcxCustomInnerShellListView.IsThumbnailView: Boolean;
begin
  Result := IsWinXPOrLater and (ListViewStyle = lvsIcon) and
    ThumbnailOptions.ShowThumbnails;
end;

procedure TcxCustomInnerShellListView.ResetSorting;
begin
  FItemProducer.SortDescending := False;
  FItemProducer.SortColumn := 0;
  SortColumnChanged;
end;

procedure TcxCustomInnerShellListView.RootFolderChanged(
  Sender: TObject; Root: TcxCustomShellRoot);
begin
  CheckUpdateItems;
  ResetSorting;
  if Assigned(FOnRootChanged) then
     FOnRootChanged(Self, Root);
end;

procedure TcxCustomInnerShellListView.RootSettingsChanged(Sender: TObject);
begin
  if not ((Parent <> nil) and (csLoading in Parent.ComponentState)) then
    DependedControls.SynchronizeRoot(Root);
end;

procedure TcxCustomInnerShellListView.SetLargeIconSize(const Value: TcxShellIconSize);
begin
  if Value <> FLargeIconSize then
  begin
    FLargeIconSize := Value;
    CheckLargeImages;
    Refresh;
  end;
end;

procedure TcxCustomInnerShellListView.SetListViewStyle(
  const Value: TcxListViewStyle);
begin
  if FListViewStyle <> Value then
  begin
    FListViewStyle := Value;
    case FListViewStyle of
      lvsIcon:          ViewStyle := vsIcon;
      lvsSmallIcon:     ViewStyle := vsSmallIcon;
      lvsList:          ViewStyle := vsList;
      lvsReport:        ViewStyle := vsReport;
    end;
    if FIsThumbnailView <> IsThumbnailView then
      UpdateThumbnails;
    CheckUpdateItems;
  end;
end;

function TcxCustomInnerShellListView.TryReleaseDropTarget:HResult;
begin
  Result := S_OK;
  if CurrentDropTarget <> nil then
     Result := CurrentDropTarget.DragLeave;
  CurrentDropTarget := nil;
  DropTargetItemIndex := -1;
end;

procedure TcxCustomInnerShellListView.ProcessTreeViewNavigate(APidl: PItemIDList);

  function IsFolder(APIDL: PItemIDList): Boolean;
  const
    SHGFI_ATTR_SPECIFIED = $20000;
  var
    ASHFileInfo: TSHFileInfo;
  begin
    ASHFileInfo.dwAttributes := SFGAO_FOLDER;
    cxShellGetThreadSafeFileInfo(Pointer(APIDL), 0, ASHFileInfo, SizeOf(ASHFileInfo),
      SHGFI_PIDL or SHGFI_ATTR_SPECIFIED or SHGFI_ATTRIBUTES);
    Result := ASHFileInfo.dwAttributes and SFGAO_FOLDER <> 0;
  end;

  function GetNearestFolderPidl(APidl: PItemIDList; out ANewlyCreated: Boolean): PItemIDList;
  var
    AFolder: IShellFolder;
  begin
    Result := APidl;
    ANewlyCreated := False;
    if not IsFolder(Result) or
      not Succeeded(GetDesktopIShellFolder.BindToObject(APidl, nil, IID_IShellFolder, AFolder)) then
    begin
      Result := GetPidlParent(Result);
      ANewlyCreated := True;
    end;
  end;

var
  AFolderPidl: PItemIDList;
  ANewlyCreated: Boolean;
begin
  NavigationLock := True;
  try
    AFolderPidl := GetNearestFolderPidl(APidl, ANewlyCreated);
    if not EqualPIDLs(AFolderPidl, Root.Pidl) then
      Root.Pidl := AFolderPidl;
    if ANewlyCreated then
      DisposePidl(AFolderPidl);
  finally
    NavigationLock := False;
  end;
end;

procedure TcxCustomInnerShellListView.Sort;
begin
  ItemProducer.Sort;
end;

procedure TcxCustomInnerShellListView.Sort(AColumnIndex: Integer; AIsAscending: Boolean);
begin
    if (AColumnIndex <> ItemProducer.SortColumn) or
      (ItemProducer.SortDescending xor not AIsAscending) then
    begin
      ItemProducer.SortColumn := AColumnIndex;
      ItemProducer.SortDescending := not AIsAscending;
      SortColumnChanged;
    end;
end;

procedure TcxCustomInnerShellListView.UpdateContent;
var
  AItemIndex: Integer;
  ASelectedItemPID: PItemIDList;
begin
  ASelectedItemPID := nil;
  try
    if not MultiSelect and (Selected <> nil) then
      ASelectedItemPID := GetPidlCopy(GetPidlByItemIndex(Selected.Index));
    CheckUpdateItems;
    if ASelectedItemPID <> nil then
    begin
      AItemIndex := ItemProducer.GetItemIndexByPidl(ASelectedItemPID);
      if (AItemIndex >= 0) and (AItemIndex < Items.Count) then
        Items[AItemIndex].Selected := True;
    end;
  finally
    DisposePidl(ASelectedItemPID);
  end;
end;

{ TcxShellListViewContextMenu }

constructor TcxShellListViewContextMenu.Create(
  AListView: TcxCustomInnerShellListView);
begin
  inherited Create;
  FListView := AListView;
end;

destructor TcxShellListViewContextMenu.Destroy;
begin

  inherited;
end;

function TcxShellListViewContextMenu.GetWindowHandle: THandle;
begin
  Result := FListView.Handle;
end;

procedure TcxShellListViewContextMenu.Populate;
var
  AItemPIDLList: TList;
begin
  AItemPIDLList := FListView.CreateSelectedPidlsList;
  try
    AddDefaultShellItems(FListView.ItemProducer.ShellFolder, AItemPIDLList);
  finally
    FListView.DestroySelectedPidlsList(AItemPIDLList);
  end;
end;

{ TcxShellListViewCurrentFolderContextMenu }

const
  cmdExtraLargeId = 1;
  cmdLargeId = 2;
  cmdMediumId = 3;
  cmdSmallId = 4;
  cmdListId = 5;
  cmdDetailId = 6;
  cmdRefreshId = 7;
  cmdAscendingId = 8;
  cmdDescendingId = 9;
  cmdLastId = cmdDescendingId;

procedure TcxShellListViewCurrentFolderContextMenu.ExecuteMenuItemCommand(ACommandId: Cardinal);
begin
  case ACommandId of
    cmdExtraLargeId:
      begin
        ListView.ListViewStyle := lvsIcon;
        ListView.ThumbnailOptions.ShowThumbnails := True;
        ListView.ThumbnailOptions.Width := 128;
        ListView.ThumbnailOptions.Height := 128;
      end;
    cmdLargeId:
      begin
        ListView.ListViewStyle := lvsIcon;
        ListView.ThumbnailOptions.ShowThumbnails := True;
        ListView.ThumbnailOptions.Width := 64;
        ListView.ThumbnailOptions.Height := 64;
      end;
    cmdMediumId:
      begin
        ListView.ListViewStyle := lvsIcon;
        ListView.ThumbnailOptions.ShowThumbnails := False;
      end;
    cmdSmallId:
      begin
        ListView.ListViewStyle := lvsSmallIcon;
      end;
    cmdListId:
      begin
        ListView.ListViewStyle := lvsList;
      end;
    cmdDetailId:
      begin
        ListView.ListViewStyle := lvsReport;
      end;
    cmdRefreshId:
      ListView.UpdateContent;
    cmdDescendingId:
      ListView.Sort(ListView.ItemProducer.SortColumn, False);
    cmdAscendingId:
      ListView.Sort(ListView.ItemProducer.SortColumn, True);
  else
    if (ACommandId > cmdLastId) and (ACommandId <= FLastSortColumnIndexCommandId) then
      ListView.Sort(ACommandId - cmdLastId - 1, ListView.ItemProducer.SortDescending)
    else
      inherited ExecuteMenuItemCommand(ACommandId);
  end;
end;

procedure TcxShellListViewCurrentFolderContextMenu.Populate;

  function GetCheckedIconSizeMenuItemIndex: Integer;
  begin
    Result := -1;
    case ListView.ListViewStyle of
      lvsIcon:
        begin
          if not ListView.ThumbnailOptions.ShowThumbnails then
            Result := 2
          else
            if ListView.ThumbnailOptions.Width = 128 then
              Result := 0
            else
              if ListView.ThumbnailOptions.Width = 64 then
                Result := 1;
        end;
      lvsSmallIcon:
        Result := 3;
      lvsList:
        Result := 4;
      lvsReport:
        Result := 5;
    end;
  end;

const
  AIconSizeMenuItemsCount = 6;
  AIconSizeMenuItemCaptions: array [0..AIconSizeMenuItemsCount - 1] of string = (
    'Extra large icons', 'Large icons', 'Medium icons', 'Small icons',
    'List', 'Details');
  ASortDirectionMenuItemCaptions: array [0..1] of string = (
    'Ascending', 'Descending');
  SortDirectionMenuItemIndex: array [Boolean] of Integer = (0, 1);
var
  AViewSubmenu: HMENU;
  I: Integer;
  ASortColumnMenuItemCaptions: array of string;
begin
  AViewSubmenu := AddSubitem('View', 0);
  AddRadioGroup(AIconSizeMenuItemCaptions, cmdExtraLargeId, 0, GetCheckedIconSizeMenuItemIndex, AViewSubmenu);
  if ListView.Sorting then
  begin
    SetLength(ASortColumnMenuItemCaptions, ListView.Columns.Count);
    for I := 0 to ListView.Columns.Count - 1 do
      ASortColumnMenuItemCaptions[I] := ListView.Columns[I].Caption;
    AViewSubmenu := AddSubitem('Sort by', 0);
    AddRadioGroup(ASortColumnMenuItemCaptions, cmdLastId + 1, 0, ListView.ItemProducer.SortColumn, AViewSubmenu);
    AddSeparator(AViewSubmenu);
    AddRadioGroup(ASortDirectionMenuItemCaptions, cmdAscendingId,
      Length(ASortColumnMenuItemCaptions) + 1, SortDirectionMenuItemIndex[ListView.ItemProducer.SortDescending], AViewSubmenu);
    FLastSortColumnIndexCommandId := cmdLastId + Length(ASortColumnMenuItemCaptions);
  end
  else
    FLastSortColumnIndexCommandId := 0;
  AddItem('Refresh', cmdRefreshId);
  AddDefaultShellItems(FListView.ItemProducer.ShellFolder);
end;

procedure TcxShellListViewCurrentFolderContextMenu.AddCheckItem(const ACaption: string;
  AId: Cardinal; AIsChecked: Boolean; AMenu: HMenu = 0);
begin
  if AMenu = 0 then
    AMenu := Menu;
  AppendMenu(AMenu, MF_ENABLED or MF_STRING or IfThen(AIsChecked, MFS_CHECKED, MFS_UNCHECKED),
    AId, PChar(ACaption));
end;

procedure TcxShellListViewCurrentFolderContextMenu.AddItem(const ACaption: string;
  AId: Cardinal; AMenu: HMenu);
begin
  AddCheckItem(ACaption, AId, False, AMenu);
end;

procedure TcxShellListViewCurrentFolderContextMenu.AddRadioGroup(AItems: array of string;
  AStartId, AStartPos: Cardinal; AItemIndex: Integer; AMenu: HMenu = 0);
var
  I: Integer;
  AMenuItemInfo: TMenuItemInfo;
begin
  if AMenu = 0 then
    AMenu := Menu;
  for I := 0 to High(AItems) do
  begin
    cxZeroMemory(@AMenuItemInfo, SizeOf(AMenuItemInfo));
    AMenuItemInfo.cbSize := SizeOf(AMenuItemInfo);
    AMenuItemInfo.fMask := MIIM_FTYPE or MIIM_STATE or MIIM_STRING or MIIM_ID;
    AMenuItemInfo.fType := MFT_RADIOCHECK;
    AMenuItemInfo.fState := IfThen(I = AItemIndex, MFS_CHECKED, MFS_UNCHECKED);
    AMenuItemInfo.dwTypeData := PChar(AItems[I]);
    AMenuItemInfo.wID := AStartId + Cardinal(I);
    InsertMenuItem(AMenu, AStartPos + Cardinal(I), True, AMenuItemInfo);
  end;
end;

procedure TcxShellListViewCurrentFolderContextMenu.AddSeparator(AMenu: HMenu = 0);
begin
  if AMenu = 0 then
    AMenu := Menu;
  AppendMenu(AMenu, MF_SEPARATOR, 0, nil);
end;

function TcxShellListViewCurrentFolderContextMenu.AddSubitem(
  const ACaption: string; AMenu: HMenu): HMenu;
begin
  Result := CreatePopupMenu;
  if AMenu = 0 then
    AMenu := Menu;
  AppendMenu(AMenu, MF_ENABLED or MF_STRING or MF_POPUP, Result, PChar(ACaption));
end;

{ TcxShellListViewProducer }

constructor TcxShellListViewProducer.Create(AOwner: TWinControl);
begin
  inherited Create(AOwner);
  FThumbnails := TCustomImageList.Create(nil);
end;

procedure TcxShellListViewProducer.ProcessItems(AIFolder: IShellFolder; AFolderPIDL: PItemIDList;
  APreloadItemCount: Integer);
begin
  CheckThumnals;
  inherited ProcessItems(AIFolder, AFolderPIDL, APreloadItemCount);
end;

procedure TcxShellListViewProducer.ProcessDetails(ShellFolder: IShellFolder;
  CharWidth: Integer);
begin
  inherited ProcessDetails(ShellFolder, ListView.StringWidth('X'));
  ListView.CreateColumns;
end;

function TcxShellListViewProducer.CanAddFolder(AFolder: TcxShellFolder): Boolean;
begin
  Result := ListView.DoAddFolder(AFolder);
end;

function TcxShellListViewProducer.DoCompareItems(AItem1, AItem2: TcxShellFolder;
  out ACompare: Integer): Boolean;
begin
  Result := ListView.DoCompare(AItem1, AItem2, ACompare);
end;

procedure TcxShellListViewProducer.DoDestroy;
begin
  inherited DoDestroy;
  FreeAndNil(FThumbnails);
end;

procedure TcxShellListViewProducer.DoSlowInitialization(AItem: TcxShellItemInfo);
begin
  AItem.UpdateThumbnail;
end;

function TcxShellListViewProducer.GetEnumFlags: Cardinal;
const
	SHCONTF_ENABLE_ASYNC	= $8000;
begin
  Result := ListView.Options.GetEnumFlags;// or SHCONTF_ENABLE_ASYNC;
end;

function TcxShellListViewProducer.GetItemsInfoGatherer: TcxShellItemsInfoGatherer;
begin
  Result := ListView.ItemsInfoGatherer;
end;

function TcxShellListViewProducer.GetThumbnailIndex(AItem: TcxShellItemInfo): Integer;

  procedure AddThumbnailToImageList(AHBitmap: HBITMAP; ARequiredSize: TSize);
  var
    ABitmap: TcxBitmap;
    AThumbnail: TBitmap;
    ARect: TRect;
  begin
    ABitmap := TcxBitmap32.CreateSize(cxRect(ARequiredSize));
    ABitmap.Canvas.Lock;
    try
      AThumbnail := TBitmap.Create;
      AThumbnail.Handle := AHBitmap;
      AThumbnail.Canvas.Lock;
      try
        ARect := cxRectCenter(ABitmap.ClientRect, AThumbnail.Width, AThumbnail.Height);
        cxBitBlt(ABitmap.Canvas.Handle, AThumbnail.Canvas.Handle, ARect, cxNullPoint, SRCCOPY);
        Result := ImageList_Add(FThumbnails.Handle, ABitmap.Handle, 0);
      finally
        AThumbnail.Canvas.Unlock;
        AThumbnail.Free;
      end;
    finally
      ABitmap.Canvas.Unlock;
      ABitmap.Free;
    end;
  end;

var
  ATempPidl: PItemIDList;
  AFlags: Cardinal;
  AExtractor: IExtractImage;
  APathBuffer: array [0..1024] of WideChar;
  ASize: TSize;
  APriority: Cardinal;
  AHBitmap: HBITMAP;
  ARequiredSize: TSize;
  AItemImageFactory: IShellItemImageFactory;
  AShellItem: IShellItem;
begin
  Result := inherited GetThumbnailIndex(AItem);
  if ListView.IsThumbnailView then
  begin
    ARequiredSize := ListView.GetThumbnailSize;
    if IsWinVistaOrLater and (SHCreateItemFromIDList(AItem.FullPIDL, CLSID_ShellItem, AShellItem) = S_OK) and
      (AShellItem.QueryInterface(IID_IShellItemImageFactory, AItemImageFactory) = S_OK) and
      (AItemImageFactory.GetImage(ARequiredSize, 0, AHBitmap) = S_OK) then
        AddThumbnailToImageList(AHBitmap, ARequiredSize)
    else
    begin
      ATempPidl := GetPidlCopy(AItem.Pidl);
      try
        if ShellFolder.GetUIObjectOf(0, 1, ATempPidl, IID_IExtractImage, nil, AExtractor) = S_OK then
        begin
          ASize := ARequiredSize;
          APriority := IEIT_PRIORITY_NORMAL;
          AFlags := IEIFLAG_OFFLINE or IEIFLAG_QUALITY or IEIFLAG_ORIGSIZE;
          AExtractor.GetLocation(APathBuffer, 255, APriority, ASize, 32, AFlags);
          if AExtractor.Extract(AHBitmap) = 0 then
            AddThumbnailToImageList(AHBitmap, ARequiredSize);
        end;
      finally
        DisposePidl(ATempPidl);
      end;
    end;
  end;
end;

function TcxShellListViewProducer.GetShowToolTip: Boolean;
begin
  Result := ListView.Options.ShowToolTip;
end;

function TcxShellListViewProducer.SlowInitializationDone(AItem: TcxShellItemInfo): Boolean;
begin
  Result := AItem.ThumbnailUpdated;
end;

procedure TcxShellListViewProducer.CheckThumnals;
var
  ASize: TSize;
begin
  ASize := ListView.GetThumbnailSize;
  FThumbnails.Handle := ImageList_Create(ASize.cx, ASize.cy,
    ILC_COLOR32 or ILC_MASK, FThumbnails.AllocBy, FThumbnails.AllocBy);
end;

function TcxShellListViewProducer.GetListView: TcxCustomInnerShellListView;
begin
  Result := TcxCustomInnerShellListView(Owner);
end;

{ TcxCustomInnerShellTreeView }

procedure TcxCustomInnerShellTreeView.AddItemProducer(
  Producer: TcxShellTreeItemProducer);
var
  tempList: TList;
begin
  tempList := ItemProducersList.LockList;
  try
    tempList.Add(Producer);
  finally
    ItemProducersList.UnlockList;
  end;
end;

procedure TcxCustomInnerShellTreeView.AdjustControlParams;
var
  AStyle: Longint;
begin
  if HandleAllocated then
  begin
    AStyle := GetWindowLong(Handle, GWL_STYLE) and not(TVS_INFOTIP) or TVS_NOTOOLTIPS;
    if ShowInfoTips or Options.ShowToolTip then
      AStyle := AStyle and not TVS_NOTOOLTIPS;
    if ShowInfoTips then
      AStyle := AStyle or TVS_INFOTIP;
    SetWindowLong(Handle, GWL_STYLE, AStyle);
  end;
end;

function TcxCustomInnerShellTreeView.CheckFileMask(AFolder: TcxShellFolder): Boolean;
begin
  Result := Options.IsFileNameValid(ExtractFileName(AFolder.PathName));
end;

function TcxCustomInnerShellTreeView.CanEdit(Node: TTreeNode): Boolean;
var
  AItemProducer: TcxShellTreeItemProducer;
  AShellItemInfo: TcxShellItemInfo;
begin
  Result := False;
  if Node.Parent = nil then
     Exit;
  AItemProducer := GetItemProducer(Node.Parent);
  AShellItemInfo := GetShellItemInfo(Node);
  if AShellItemInfo <> nil then
  begin
    AItemProducer.LockWrite;
    try
      if not AItemProducer.SlowInitializationDone(AShellItemInfo) then
        AItemProducer.DoSlowInitialization(AShellItemInfo);
      Result := AShellItemInfo.CanRename;
    finally
      AItemProducer.UnlockWrite;
    end;
    Result := Result and inherited CanEdit(Node);
  end;
end;

function TcxCustomInnerShellTreeView.CanExpand(Node: TTreeNode): Boolean;

  function GetDataForProcessing(out AProcessingFolder: IShellFolder;
    out AProcessingPidl: PItemIDList): Boolean;
  var
    AItemProducer: TcxShellTreeItemProducer;
    AItemInfo: TcxShellItemInfo;
  begin
    if Node.Parent <> nil then
    begin
      AItemProducer := GetItemProducer(Node.Parent);
      AItemInfo := GetShellItemInfo(Node);
      Node.HasChildren := AItemInfo.IsFolder;
      Result := AItemInfo.IsFolder and
        Succeeded(AItemProducer.ShellFolder.BindToObject(AItemInfo.pidl, nil,
          IID_IShellFolder, AProcessingFolder));
      if Result then
        AProcessingPidl := ConcatenatePidls(AItemProducer.FolderPidl, AItemInfo.pidl);
    end
    else
    begin
      Result := True;
      AProcessingFolder := Root.ShellFolder;
      AProcessingPidl := GetPidlCopy(Root.Pidl);
    end;
  end;

  function InternalCanExpand: Boolean;
  var
    AProcessingPidl: PItemIDList;
    AProcessingFolder: IShellFolder;
    APreloadItemCount: Integer;
  begin
    Inc(FLockChange);
    try
      Result := GetDataForProcessing(AProcessingFolder, AProcessingPidl);
      if Result then
        try
          APreloadItemCount := IfThen(Options.ShowNonFolders,
            TreeView_GetVisibleCount(Handle));
          GetItemProducer(Node).ProcessItems(AProcessingFolder,
            AProcessingPidl, Node, APreloadItemCount);
        finally
          DisposePidl(AProcessingPidl);
        end;
    finally
      Dec(FLockChange);
    end;
  end;

begin
  Result := ((Node.GetFirstChild <> nil) or InternalCanExpand) and
    inherited CanExpand(Node);
end;

procedure TcxCustomInnerShellTreeView.CNNotify(var Message: TWMNotify);
var
  ANode: TTreeNode;
  AItemProducer: TcxShellTreeItemProducer;
begin
  case Message.NMHdr^.code of
    TVN_BEGINDRAG, TVN_BEGINRDRAG:
      begin
        with PNMTreeView(Message.NMHdr)^ do
          Selected := GetNodeFromItem(ItemNew);
        DoBeginDrag;
      end;
    TVN_GETINFOTIP:
      begin
        ANode := Items.GetNode(PNMTVGetInfoTip(Message.NMHdr)^.hItem);
        if (ANode <> nil) and (ANode.Parent <> nil) then
        begin
          AItemProducer := GetItemProducer(ANode.Parent);
          AItemProducer.DoGetInfoTip(Handle, ANode.Index,
            PNMTVGetInfoTip(Message.NMHdr)^.pszText,
            PNMTVGetInfoTip(Message.NMHdr)^.cchTextMax);
        end;
      end
  else
    inherited;
  end;
end;

{ TcxShellTreeViewOptions }

procedure TcxShellTreeViewOptions.DoNotifyUpdateContents;
begin
  (Owner as TcxCustomInnerShellTreeView).UpdateContent;
end;

constructor TcxCustomInnerShellTreeView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDependedControls := TcxShellDependedControls.Create;
  FItemsInfoGatherer := TcxShellItemsInfoGatherer.Create(Self);
  FRoot := TcxShellTreeRoot.Create(Self, 0);
  FRoot.OnFolderChanged := RootFolderChanged;
  FRoot.OnSettingsChanged := RootSettingsChanged;
  FDragDropSettings := TcxDragDropSettings.Create;
  FDragDropSettings.OnChange := DragDropSettingsChanged;
  FOptions := TcxShellTreeViewOptions.Create(Self);
  TcxShellOptionsAccess(FOptions).OnShowToolTipChanged := ShowToolTipChanged;
  FItemProducersList := TThreadList.Create;
  FInternalSmallImages := cxShellGetImageList(SHGFI_SMALLICON);
  ImageList_SetBkColor(FInternalSmallImages, CLR_NONE);
  DoubleBuffered := True;
  RightClickSelect := True;
  FAppEvents := TApplicationEvents.Create(nil);
  FAppEvents.OnIdle := DoOnIdle;
end;

destructor TcxCustomInnerShellTreeView.Destroy;
var
  AList: TList;
  I: Integer;
begin
  FreeAndNil(FAppEvents);
  RemoveChangeNotification;

  AList := FItemProducersList.LockList;
  try
    for I := 0 to AList.Count - 1 do
      TcxShellTreeItemProducer(AList[I]).ClearFetchQueue;
  finally
    FItemProducersList.UnlockList;
  end;

  ListView := nil;
  Items.Clear;
  FreeAndNil(FItemProducersList);
  FreeAndNil(FOptions);
  FreeAndNil(FDragDropSettings);
  FreeAndNil(FRoot);
  FreeAndNil(FItemsInfoGatherer);
  FreeAndNil(FDependedControls);
  inherited Destroy;
end;

procedure TcxCustomInnerShellTreeView.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  if ShowInfoTips then
    Params.Style := (Params.Style or TVS_INFOTIP) and not TVS_NOTOOLTIPS;
  if IsDragDropEnabled then
    Params.Style := Params.Style and not TVS_DISABLEDRAGDROP
  else
    Params.Style := Params.Style or TVS_DISABLEDRAGDROP;
end;

procedure TcxCustomInnerShellTreeView.CreateWnd;
begin
  inherited CreateWnd;
  if HandleAllocated then
  begin
    if FInternalSmallImages <> 0 then
      SendMessage(Handle, TVM_SETIMAGELIST, TVSIL_NORMAL, LParam(FInternalSmallImages));
    if not IsLoading and (Root.Pidl = nil) then
       Root.CheckRoot;
    UpdateNode(nil, False);
    CreateDropTarget;
  end;
end;

procedure TcxCustomInnerShellTreeView.Delete(Node: TTreeNode);
var
  ItemProducer: TcxShellTreeItemProducer;
begin
  ItemProducer := GetItemProducer(Node);
  if ItemProducer <> nil then
  begin
    ItemProducer.Free;
    Node.Data := nil;
  end;
  if Node = FTopNode then
    FTopNode := nil;
  if Node = FDragSourceNode then
    FDragSourceNode := nil;
  if Node = FDragSourceNodeParent then
    FDragSourceNodeParent := nil;
  inherited;
end;

procedure TcxCustomInnerShellTreeView.Expand(Node: TTreeNode);
begin
  inherited;
  UpdateVisibleItems;
end;

procedure TcxCustomInnerShellTreeView.UpdateContent;
var
  APidls: TPidlList;
begin
  if HandleAllocated then
  begin
    if Root.ShellFolder = nil then
      Root.CheckRoot;
    APidls[0] := Root.Pidl;
    APidls[1] := nil;
    SendMessage(Handle, DSM_SHELLTREECHANGENOTIFY, WPARAM(@APidls), 0);
  end;
end;

procedure TcxCustomInnerShellTreeView.DestroyWnd;
begin
  RemoveChangeNotification;
  RemoveDropTarget;
  CreateWndRestores := False;
  inherited DestroyWnd;
end;

procedure TcxCustomInnerShellTreeView.DoBeginDrag;
var
  ASourceItemProducer: TcxShellTreeItemProducer;
  ATempPidl: PItemIDList;
  ADataObject: IDataObject;
  ADropSource: IcxDropSource;
  AEffect: Integer;
begin
  FDragSourceNode := Selected;
  FDragSourceNodeParent := FDragSourceNode.Parent;
  if FDragSourceNodeParent = nil then
  begin
    FDragSourceNode := nil;
    Exit;
  end;
  ASourceItemProducer := GetItemProducer(FDragSourceNodeParent);
  FIsDragging := True;
  ASourceItemProducer.LockRead;
  try
    if FDragSourceNode.Index < ASourceItemProducer.Items.Count then
    begin
      ATempPidl := GetPidlCopy(GetShellItemInfo(FDragSourceNode).pidl);
      try
        if Succeeded(ASourceItemProducer.ShellFolder.GetUIObjectOf(Handle, 1, ATempPidl,
          IDataObject, nil, ADataObject)) then
        begin
          ADropSource := TcxDropSource.Create(Self);
          AEffect := DragDropSettings.DropEffectAPI;
          DoDragDrop(ADataObject, ADropSource, AEffect, AEffect);
          if FDragSourceNode <> nil then
            CheckUpdates(FDragSourceNodeParent);
        end;
      finally
        DisposePidl(ATempPidl);
      end;
    end;
  finally
    if FDragSourceNodeParent <> nil then
      ASourceItemProducer.UnlockRead;
    FIsDragging := False;
    FDragSourceNode := nil;
    FDragSourceNodeParent := nil;
  end;
end;

procedure TcxCustomInnerShellTreeView.DoContextPopup(MousePos: TPoint; var Handled: Boolean);
var
  AItem: TcxShellItemInfo;
  AItemPIDLList: TList;
  ANode: TTreeNode;
begin
  try
    ANode := GetNodeAt(MousePos.X, MousePos.Y);
    if not Options.ContextMenus or (ANode = nil) then
    begin
      inherited DoContextPopup(MousePos, Handled);
      Exit;
    end;
    Handled := True;
    if ANode.Parent = nil then
      Exit;

    FContextPopupItemProducer := GetItemProducer(ANode.Parent);
    FContextPopupItemProducer.OnDestroy := ContextPopupItemProducerDestroyHandler;
    FContextPopupItemProducer.LockRead;
    try
      CreateChangeNotification(ANode);
      AItem := FContextPopupItemProducer.ItemInfo[ANode.Index];
      FIsChangeNotificationCreationLocked := True;
      if AItem.pidl <> nil then
      begin
        AItemPIDLList := TList.Create;
        try
          AItemPIDLList.Add(GetPidlCopy(AItem.pidl));
          cxShellCommon.DisplayContextMenu(Handle,
            FContextPopupItemProducer.ShellFolder, AItemPIDLList, ClientToScreen(MousePos));
        finally
          DisposePidl(AItemPIDLList[0]);
          AItemPIDLList.Free;
        end;
      end;
    finally
      if FContextPopupItemProducer <> nil then
        FContextPopupItemProducer.UnlockRead;
    end;
  finally
    FIsChangeNotificationCreationLocked := False;
    if FContextPopupItemProducer <> nil then
    begin
      FContextPopupItemProducer.OnDestroy := nil;
      FContextPopupItemProducer := nil;
    end;
  end;
end;

procedure TcxCustomInnerShellTreeView.KeyDown(var Key: Word; Shift: TShiftState);

  procedure InvokeContextMenuCommand(const ACommandStr: AnsiString);
  var
    AShellFolder : IShellFolder;
    AItemPIDLList: TList;
    AItem: TcxShellItemInfo;
    AParentItemProducer: TcxShellTreeItemProducer;
  begin
    if (Selected <> nil) and (Selected.Parent <> nil) then
    begin
      AItemPIDLList := TList.Create;
      try
        AParentItemProducer := GetItemProducer(Selected.Parent);
        AShellFolder := AParentItemProducer.ShellFolder;
        AItem := AParentItemProducer.ItemInfo[Selected.Index];
        AItemPIDLList.Add(GetPidlCopy(AItem.pidl));
        cxShellInvokeContextMenuCommand(AShellFolder, AItemPIDLList, ACommandStr);
      finally
        if AItemPIDLList.Count > 0 then
          DisposePidl(AItemPIDLList[0]);
        AItemPIDLList.Free;
      end;
    end;
  end;

begin
  inherited KeyDown(Key, Shift);
  case Key of
    VK_F5:
      UpdateContent;
  else
    if not IsEditing and Options.ContextMenus and(cxShellIsClipboardCommandContextMenuShortCut(Key, Shift) or
      (Key = VK_DELETE) and (Selected <> nil)) then
        InvokeContextMenuCommand(cxShellGetContextMenuCommandStrByShortCut(Key, Shift));
  end;
end;

procedure TcxCustomInnerShellTreeView.Resize;
begin
  inherited;
  UpdateVisibleItems;
end;

function TcxCustomInnerShellTreeView.DragEnter(const dataObj: IDataObject;
  grfKeyState: Integer; pt: TPoint; var dwEffect: Integer): HResult;
var
  ANew: Boolean;
begin
  DraggedObject := IcxDataObject(dataObj);
  GetDropTarget(ANew, pt);
  if CurrentDropTarget = nil then
  begin
    dwEffect := DROPEFFECT_NONE;
    Result := S_OK;
  end
  else
  begin
    CheckDefaultDropEffect(grfKeyState, DragDropSettings, dwEffect);
    Result := CurrentDropTarget.DragEnter(dataObj, grfKeyState, pt, dwEffect);
  end;
end;

function TcxCustomInnerShellTreeView.DragLeave: HResult;
begin
  DraggedObject := nil;
  Result := TryReleaseDropTarget;
end;

function TcxCustomInnerShellTreeView.IDropTargetDragOver(grfKeyState: Integer; pt: TPoint;
  var dwEffect: Integer): HResult;
var
  ANew: Boolean;
begin
  GetDropTarget(ANew, pt);
  if CurrentDropTarget = nil then
  begin
    dwEffect := DROPEFFECT_NONE;
    Result := S_OK;
  end
  else
  begin
    FWasMouseRButtonPressed := grfKeyState and MK_RBUTTON <> 0;
    CheckDefaultDropEffect(grfKeyState, DragDropSettings, dwEffect);
    if ANew then
      Result := CurrentDropTarget.DragEnter(DraggedObject, grfKeyState, pt, dwEffect)
    else
      Result := S_OK;
    if Succeeded(Result) then
      Result := CurrentDropTarget.DragOver(grfKeyState, pt, dwEffect);
  end;
end;

function TcxCustomInnerShellTreeView.Drop(const dataObj: IDataObject;
  grfKeyState: Integer; pt: TPoint; var dwEffect: Integer): HResult;
var
  ANew: Boolean;
begin
  GetDropTarget(ANew, pt);
  if CurrentDropTarget = nil then
  begin
    dwEffect := DROPEFFECT_NONE;
    Result := S_OK;
  end
  else
  begin
    if not FWasMouseRButtonPressed then
      CheckDefaultDropEffect(grfKeyState, DragDropSettings, dwEffect);
    if ANew then
      Result := CurrentDropTarget.DragEnter(dataObj, grfKeyState, pt, dwEffect)
    else
      Result := S_OK;
    if Succeeded(Result) then
      Result := CurrentDropTarget.Drop(dataObj, grfKeyState, pt, dwEffect);
  end;
  PostMessage(Handle, DSM_SHELLCHANGENOTIFY, WPARAM(PrevTargetNode), 0);
  DraggedObject := nil;
  TryReleaseDropTarget;
end;

procedure TcxCustomInnerShellTreeView.DsmNotifyAddItem(var Message: TMessage);
var
  AParentProducer: TcxShellTreeItemProducer;
  ANode: TTreeNode;
begin
  ANode := TTreeNode(Message.LParam);
  AParentProducer := GetItemProducer(ANode);
  AParentProducer.LockRead;
  try
    CreateShellNode(ANode, AParentProducer.Items[Message.WParam]);
  finally
    AParentProducer.UnlockRead;
  end;
end;

procedure TcxCustomInnerShellTreeView.DsmNotifyRemoveItem(
  var Message: TMessage);
var
  ANode: TTreeNode;
begin
  ANode := TTreeNode(Message.LParam);
  if Message.WParam < WParam(ANode.Count) then
    ANode.Item[Message.WParam].Delete;
end;

procedure TcxCustomInnerShellTreeView.DsmNotifyUpdateContents(
  var Message: TMessage);
begin
  if not (csLoading in ComponentState) then
     UpdateNode(nil, False);
end;

procedure TcxCustomInnerShellTreeView.DsmNotifyUpdateItem(
  var Message: TMessage);

  function GetChildNode(ANode: TTreeNode; AIndex: Integer): TTreeNode;
  begin
    Result := ANode.getFirstChild;
    while (Result <> nil) and (AIndex > 0) do
    begin
      Result := ANode.GetNextChild(Result);
      Dec(AIndex);
    end;
  end;

var
  AItem: TcxShellItemInfo;
  AItemProducer: TcxShellTreeItemProducer;
  ANode, ATempNode: TTreeNode;
begin
  ANode := TTreeNode(Message.LParam);
  if ANode.getFirstChild = nil then
    Exit;
  ATempNode := GetChildNode(ANode, Message.WParam);
  if ATempNode = nil then
    Exit;

  AItemProducer := GetItemProducer(ANode);
  AItemProducer.LockRead;
  try
    AItem := AItemProducer.ItemInfo[Message.WParam];
    ATempNode.ImageIndex := AItem.IconIndex;
    ATempNode.SelectedIndex := AItem.OpenIconIndex;
    ATempNode.Text := AItem.Name;
    ATempNode.HasChildren := AItem.HasSubfolder;
    ATempNode.Cut := AItem.IsGhosted;
    ATempNode.OverlayIndex := AItem.GetOverlayIndex;
  finally
    AItemProducer.UnlockRead;
  end;
end;

procedure TcxCustomInnerShellTreeView.DsmSetCount(var Message: TMessage);
var
  ANode: TTreeNode;
  AParentProducer: TcxShellTreeItemProducer;
  I: Integer;
begin
  ANode := TTreeNode(Message.LParam);
  if Message.WParam = 0 then
  begin
    ANode.DeleteChildren;
    ANode.HasChildren := False;
    Exit;
  end;
  AParentProducer := GetItemProducer(ANode);
  AParentProducer.LockRead;
  try
    Items.BeginUpdate;
    try
      for I := 0 to AParentProducer.Items.Count - 1 do
        CreateShellNode(ANode, AParentProducer.Items[I]);
    finally
      Items.EndUpdate;
    end;
    if ANode.GetFirstChild = nil then
       ANode.HasChildren := False;
  finally
    AParentProducer.UnlockRead;
  end;
end;

procedure TcxCustomInnerShellTreeView.DsmShellChangeNotify(
  var Message: TMessage);
begin
  Sleep(100);
  CheckUpdates(TTreeNode(Message.WParam));
end;

procedure TcxCustomInnerShellTreeView.Edit(const Item: TTVItem);
var
  AItemInfo: TcxShellItemInfo;
  AItemProducer: TcxShellTreeItemProducer;
  ANode: TTreeNode;
  APIDL: PItemIDList;
  APrevNodeText: string;
begin
  ANode := GetNodeFromItem(Item);
  APrevNodeText := '';
  if ANode <> nil then
    APrevNodeText := ANode.Text;
  inherited Edit(Item);
  if (Item.pszText = nil) or (ANode = nil) or (ANode.Parent = nil) then
    Exit;
  AItemProducer := GetItemProducer(ANode.Parent);
  AItemInfo := GetShellItemInfo(ANode);
  RemoveChangeNotification;
  if AItemProducer.ShellFolder.SetNameOf(Handle, AItemInfo.pidl, PWideChar(WideString(ANode.Text)),
    SHGDN_INFOLDER or SHGDN_FORPARSING, APIDL) = S_OK then
    try
      AItemInfo.SetNewPidl(AItemProducer.ShellFolder, AItemProducer.FolderPidl, APIDL);
      UpdateNode(ANode, True);
    finally
      DisposePidl(APIDL);
    end
  else
    ANode.Text := APrevNodeText;
end;

procedure TcxCustomInnerShellTreeView.ContextPopupItemProducerDestroyHandler(
  Sender: TObject);
begin
  FContextPopupItemProducer.UnlockRead;
  FContextPopupItemProducer.OnDestroy := nil;
  FContextPopupItemProducer := nil;
end;

function TcxCustomInnerShellTreeView.CreateShellNode(AParentNode: TTreeNode;
  AShellItem: TcxShellItemInfo): TTreeNode;
var
  AItemProducer: TcxShellTreeItemProducer;
begin
  AItemProducer := TcxShellTreeItemProducer.Create(Self);
  AItemProducer.ShellItemInfo := AShellItem;
  Result := Items.AddChildObject(AParentNode, AShellItem.Name, AItemProducer);
  AShellItem.Data := Result;
  Result.ImageIndex := AShellItem.IconIndex;
  Result.SelectedIndex := AShellItem.OpenIconIndex;
  Result.HasChildren := AShellItem.HasSubfolder;
  Result.Cut := AShellItem.IsGhosted;
  Result.OverlayIndex := AShellItem.OverlayIndex;
end;

function TcxCustomInnerShellTreeView.GetFolder(AIndex: Integer): TcxShellFolder;
var
  ANode: TTreeNode;
begin
  ANode := Items[AIndex];
  if ANode.Parent = nil then
    Result := Root.Folder
  else
    Result := GetShellItemInfo(ANode).Folder;
end;

function TcxCustomInnerShellTreeView.GetFolderCount: Integer;
begin
  Result := Items.Count;
end;

function TcxCustomInnerShellTreeView.GetNodeFromItem(
  const Item: TTVItem): TTreeNode;
begin
  Result := nil;
  if Items <> nil then
    with Item do
      if (state and TVIF_PARAM) <> 0 then
        Result := Pointer(lParam)
      else
        Result := Items.GetNode(hItem);
end;

function TcxCustomInnerShellTreeView.GetRoot: TcxCustomShellRoot;
begin
  Result := FRoot;
end;

function TcxCustomInnerShellTreeView.GetItemProducer(ANode: TTreeNode): TcxShellTreeItemProducer;
begin
  Result := TcxShellTreeItemProducer(ANode.Data);
end;

function TcxCustomInnerShellTreeView.GetRootField: TcxShellTreeRoot;
begin
  Result := FRoot;
end;

function TcxCustomInnerShellTreeView.GetShellItemInfo(ANode: TTreeNode): TcxShellItemInfo;
begin
  Result := GetItemProducer(ANode).ShellItemInfo;
end;

function TcxCustomInnerShellTreeView.GetDependedControls: TcxShellDependedControls;
begin
  Result := DependedControls;
end;

procedure TcxCustomInnerShellTreeView.LockUpdateVisibleInfo;
begin
  Inc(FLockVisibleUpdate);
end;

procedure TcxCustomInnerShellTreeView.RestoreTreeState;

  procedure RestoreExpandedNodes;

    procedure ExpandNode(APIDL: PItemIDList);
    var
      ANode: TTreeNode;
    begin
      if Root.ShellFolder = nil then
        Root.CheckRoot;
      if APIDL = nil then
        APIDL := Root.Pidl;
      ANode := GetNodeByPIDL(APIDL);
      if ANode <> nil then
        ANode.Expand(False);
    end;

    procedure DestroyExpandedNodeList;
    var
      I: Integer;
    begin
      if FStateData.ExpandedNodeList = nil then
        Exit;
      for I := 0 to FStateData.ExpandedNodeList.Count - 1 do
        DisposePidl(PItemIDList(FStateData.ExpandedNodeList[I]));
      FreeAndNil(FStateData.ExpandedNodeList);
    end;

  var
    I: Integer;
  begin
    try
      for I := 0 to FStateData.ExpandedNodeList.Count - 1 do
        ExpandNode(PItemIDList(FStateData.ExpandedNodeList[I]));
    finally
      DestroyExpandedNodeList;
    end;
  end;

  procedure RestoreTopItemIndex;
  begin
    if (FStateData.TopItemIndex >= 0) and (FStateData.TopItemIndex < Items.Count) then
      TopItem := Items[FStateData.TopItemIndex];
  end;

  procedure RestoreCurrentPath;
  var
    ACurrentPath, ATempPIDL: PItemIDList;
  begin
    if FStateData.CurrentPath = nil then
      Exit;
    ACurrentPath := GetPidlCopy(FStateData.CurrentPath);
    try
      repeat
        if GetNodeByPIDL(ACurrentPath) <> nil then
        begin
          PostMessage(Handle, DSM_SHELLTREERESTORECURRENTPATH,
            WPARAM(GetPidlCopy(ACurrentPath)), 0);
          Break;
        end;
        ATempPIDL := ACurrentPath;
        ACurrentPath := GetPidlParent(ACurrentPath);
        DisposePidl(ATempPIDL);
      until False;
    finally
      DisposePidl(ACurrentPath);
    end;
  end;

begin
  try
    RestoreExpandedNodes;
    RestoreTopItemIndex;
    RestoreCurrentPath;
  finally
    DisposePidl(FStateData.CurrentPath);
    FStateData.CurrentPath := nil;
  end;
end;

procedure TcxCustomInnerShellTreeView.SaveTreeState;

  procedure SaveTopItemIndex;
  begin
    if TopItem <> nil then
      FStateData.TopItemIndex := TopItem.AbsoluteIndex
    else
      FStateData.TopItemIndex := -1;
  end;

  procedure SaveExpandedNodes;

    procedure SaveExpandedNode(ANode: TTreeNode);
    begin
      if ANode.Parent = nil then
        FStateData.ExpandedNodeList.Add(nil)
      else
        FStateData.ExpandedNodeList.Add(
          GetPidlCopy(GetShellItemInfo(ANode).FullPIDL));
    end;

  var
    ANode: TTreeNode;
  begin
    FStateData.ExpandedNodeList := TList.Create;
    ANode := Items.GetFirstNode;
    while ANode <> nil do
    begin
      if ANode.Expanded then
        SaveExpandedNode(ANode);
      ANode := ANode.GetNext;
    end;
  end;

  function GetSelectedNode: TTreeNode;
  begin
    if HandleAllocated then
      Result := Items.GetNode(TreeView_GetSelection(Handle))
    else
      Result := nil;
  end;

  procedure SaveCurrentPath;
  var
    ASelected: TTreeNode;
  begin
    ASelected := GetSelectedNode;
    if ASelected <> nil then
    begin
      if ASelected.Parent = nil then
        FStateData.CurrentPath := Root.Pidl
      else
        FStateData.CurrentPath := GetShellItemInfo(ASelected).FullPIDL;
      FStateData.CurrentPath := GetPidlCopy(FStateData.CurrentPath);
    end
    else
      FStateData.CurrentPath := nil;
  end;

begin
  SaveTopItemIndex;
  SaveExpandedNodes;
  SaveCurrentPath;
end;

procedure TcxCustomInnerShellTreeView.Loaded;
begin
  if Root.Pidl = nil then
    Root.CheckRoot;
  UpdateNode(nil, False);
end;

procedure TcxCustomInnerShellTreeView.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = ListView then
      ListView := nil;
  end;
end;

procedure TcxCustomInnerShellTreeView.CreateChangeNotification(
  ANode: TTreeNode = nil);

  function GetShellChangeNotifierPIDL: PItemIDList;
  begin
    if Root.ShellFolder = nil then
      Root.CheckRoot;
    if ANode = nil then
      if Selected = nil then
        ANode := Items[0]
      else
        ANode := Selected;
    if ANode.Parent = nil then
      Result := Root.Pidl
    else
      Result := GetShellItemInfo(ANode).FullPIDL;
  end;

begin
  if FIsChangeNotificationCreationLocked then
    Exit;
  FShellChangeNotificationCreation := True;
  try
    if not Options.TrackShellChanges or (Items.Count = 0) then
      RemoveChangeNotification
    else
      cxShellRegisterChangeNotifier(GetShellChangeNotifierPIDL, Handle,
        DSM_SYSTEMSHELLCHANGENOTIFY, True, FShellChangeNotifierData);
  finally
    FShellChangeNotificationCreation := False;
  end;
end;

procedure TcxCustomInnerShellTreeView.CreateDropTarget;
var
  AIDropTarget: IcxDropTarget;
begin
  GetInterface(IcxDropTarget, AIDropTarget);
  RegisterDragDrop(Handle, IDropTarget(AIDropTarget));
end;

function TcxCustomInnerShellTreeView.DoAddFolder(AFolder: TcxShellFolder): Boolean;

  function IsZipFile(AFolder: TcxShellFolder): Boolean;
  begin
    Result := AFolder.StorageCapabilities * [sfscStream, sfscFolder] = [sfscStream, sfscFolder];
  end;

begin
  Result := AFolder.IsFolder and
    (Options.ShowNonFolders or Options.ShowZipFilesWithFolders or not IsZipFile(AFolder)) or
    not AFolder.IsFolder and CheckFileMask(AFolder);
  if Assigned(FOnAddFolder) then
    FOnAddFolder(Self, AFolder, Result);
end;

procedure TcxCustomInnerShellTreeView.CheckUpdates(ANode: TTreeNode);
var
  AProducer: TcxShellTreeItemProducer;
begin
  LockUpdateVisibleInfo;
  ShowHourglassCursor;
  try
    AProducer := GetItemProducer(ANode);
    FItemsInfoGatherer.ClearFetchQueue(AProducer);
    if not AProducer.CheckUpdates then
      UpdateNode(ANode, False);
  finally
    HideHourglassCursor;
    UnlockUpdateVisibleInfo;
  end;
  UpdateVisibleItems;
end;

procedure TcxCustomInnerShellTreeView.SetPrevTargetNode(const Value: TTreeNode);
begin
  if FPrevTargetNode <> nil then
    FPrevTargetNode.DropTarget := False;
  FPrevTargetNode := Value;
  if FPrevTargetNode <> nil then
    FPrevTargetNode.DropTarget := True;
end;

procedure TcxCustomInnerShellTreeView.SetRootField(const Value: TcxShellTreeRoot);
begin
  FRoot.Assign(Value);
end;

function TcxCustomInnerShellTreeView.TryReleaseDropTarget: HResult;
begin
  Result := S_OK;
  if CurrentDropTarget <> nil then
     Result := CurrentDropTarget.DragLeave;
  CurrentDropTarget := nil;
  PrevTargetNode := nil;
end;

procedure TcxCustomInnerShellTreeView.UpdateItem(AItem: TcxShellItemInfo);
var
  AItemProducer: TcxShellTreeItemProducer;
  AUpdatingNode: TTreeNode;
begin
  AUpdatingNode := TTreeNode(AItem.Data);
  AItemProducer := TcxShellTreeItemProducer(AItem.ItemProducer);
  AItemProducer.LockRead;
  try
    AUpdatingNode.ImageIndex := AItem.IconIndex;
    AUpdatingNode.SelectedIndex := AItem.OpenIconIndex;
    AUpdatingNode.Text := AItem.Name;
    AUpdatingNode.HasChildren := AItem.HasSubfolder;
    AUpdatingNode.Cut := AItem.IsGhosted;
    AUpdatingNode.OverlayIndex := AItem.OverlayIndex;
    AItem.Processed := True;
  finally
    AItemProducer.UnlockRead;
  end;
end;

procedure TcxCustomInnerShellTreeView.UpdateVisibleItems;
var
  I: Integer;
  ANode: TTreeNode;
  AItems: TList;
  AVisibleCount: Integer;
  AItemInfo: TcxShellItemInfo;
begin
  if FLockVisibleUpdate > 0 then
    Exit;
  AVisibleCount := TreeView_GetVisibleCount(Handle);
  ANode := TopItem;
 // if (FTopNode <> ANode) or (AVisibleCount > FMaxVisibleNodeCount) then
  begin
    FTopNode := ANode;
    FMaxVisibleNodeCount := AVisibleCount;
    AItems := TList.Create;
    try
      I := 0;
      while (ANode <> nil) and (I <= AVisibleCount) do
      begin
        if ANode.Parent <> nil then
        begin
          AItemInfo := GetShellItemInfo(ANode);
          if not AItemInfo.Updated or not AItemInfo.Processed then
            AItems.Add(AItemInfo);
        end;
        ANode := ANode.GetNextVisible;
        Inc(I);
      end;
      FItemsInfoGatherer.ClearVisibleItems;
      FItemsInfoGatherer.RequestItems(AItems);
    finally
      AItems.Free;
    end;
  end;
end;

procedure TcxCustomInnerShellTreeView.UpdateNode(ANode: TTreeNode;
  AFast: Boolean);

  procedure InternalUpdateNode(AUpdatedNode: TTreeNode);
  var
    AFullPIDL: PITemIDList;
    AParentItemProducer: TcxShellTreeItemProducer;
    AShellItemInfo: TcxShellItemInfo;
  begin
    AShellItemInfo := GetShellItemInfo(AUpdatedNode);
    AParentItemProducer := GetItemProducer(AUpdatedNode.Parent);
    AFullPIDL := ConcatenatePidls(AParentItemProducer.FolderPidl,
      AShellItemInfo.pidl);
    try
      GetItemProducer(AUpdatedNode).FolderPidl := AFullPIDL;
      AUpdatedNode.HasChildren := HasSubItems(AParentItemProducer.ShellFolder,
        AFullPIDL, AParentItemProducer.GetEnumFlags);
    finally
      DisposePidl(AFullPIDL);
    end;
  end;

var
  ATempNode: TTreeNode;
begin
  if csLoading in ComponentState then
    Exit;
  if IsWindow(WindowHandle) and Root.IsValid then
  begin
    if ANode = nil then
    begin
      if (Items.Count > 0) and (Items[0].Data <> nil) then
        Items.Clear;
      if Items.Count = 0 then
        ATempNode := Items.AddFirst(nil, '')
      else
        ATempNode := Items[0];
      ATempNode.Data := TcxShellTreeItemProducer.Create(Self);
    end
    else
      ATempNode := ANode;
    if not AFast or (ATempNode.Parent = nil) then
    begin
      ATempNode.HasChildren := True;
      if not CanExpand(ATempNode) then
        InternalUpdateNode(ATempNode);
    end
    else
      InternalUpdateNode(ATempNode);
    CreateChangeNotification;
  end;
end;

procedure TcxCustomInnerShellTreeView.SetListView(Value: TcxCustomInnerShellListView);
begin
  if cxSetShellControl(Self, Value, TWinControl(FListView)) then
    DoNavigateListView;
end;

procedure TcxCustomInnerShellTreeView.RootFolderChanged(Sender: TObject; Root: TcxCustomShellRoot);
begin
  Items.Clear;
  UpdateNode(nil, False);
  if Assigned(FOnRootChanged) then
    FOnRootChanged(Self, FRoot);
end;

procedure TcxCustomInnerShellTreeView.RootSettingsChanged(Sender: TObject);
begin
  if (Parent <> nil) and (csLoading in Parent.ComponentState) then
    Exit;
  DependedControls.SynchronizeRoot(Root);
end;

procedure TcxCustomInnerShellTreeView.SetShowInfoTips(Value: Boolean);
begin
  if Value <> FShowInfoTips then
  begin
    FShowInfoTips := Value;
    AdjustControlParams;
  end;
end;

procedure TcxCustomInnerShellTreeView.ShellChangeNotify(AEventID: Longint; APidl1, APidl2: PItemIDList);

  function NeedProcessMessage(AEventID: Longint; APidl1, APidl2: PItemIDList): Boolean; // TODO more detailed selection
  begin
    Result := (AEventID <> SHCNE_UPDATEITEM) or
      (GetNodeByPIDL(APidl1) <> nil);
  end;

  procedure DoShellTreeChange;
  begin
    if (DraggedObject <> nil) or FIsDragging then
      Exit;
    Items.BeginUpdate;
    FIsUpdating := True;
    try
      SendMessage(Parent.Handle, WM_SETREDRAW, 0, 0);
      try
        SaveTreeState;
        try
          Items.Clear;
          UpdateNode(nil, False);
        finally
          RestoreTreeState;
        end;
      finally
        SendMessage(Parent.Handle, WM_SETREDRAW, 1, 0);
        Parent.Update;
      end;
    finally
      FIsUpdating := False;
      Items.EndUpdate;
    end;
  end;

begin
  if not FShellChangeNotificationCreation and not FIsUpdating and
    (FLockChange = 0) and NeedProcessMessage(AEventID, APidl1, APidl2) then
    try
      DoShellTreeChange;
    finally
      if Assigned(FOnShellChange) then
        FOnShellChange(Self, AEventID, APidl1, APidl2);
    end;
end;

procedure TcxCustomInnerShellTreeView.ShowToolTipChanged(Sender: TObject);
begin
  ToolTips := Options.ShowToolTip;
  AdjustControlParams;
end;

procedure TcxCustomInnerShellTreeView.UnlockUpdateVisibleInfo;
begin
  if FLockVisibleUpdate > 0 then
    Dec(FLockVisibleUpdate);
end;

procedure TcxCustomInnerShellTreeView.CMColorChanged(var Message: TMessage);
begin
  inherited;
end;

procedure TcxCustomInnerShellTreeView.CMFontChanged(var Message: TMessage);
begin
  inherited;
end;

procedure TcxCustomInnerShellTreeView.DSMShellTreeChangeNotify(var Message: TMessage);
begin
  ShellChangeNotify(Message.LParam, PPItemIDList(Message.WParam)^,
    PPItemIDList(Message.WParam + SizeOf(Pointer))^);
end;

procedure TcxCustomInnerShellTreeView.DSMShellTreeRestoreCurrentPath(var Message: TMessage);
var
  APrevAutoExpand: Boolean;
begin
  if FIsChangeNotificationCreationLocked then
    PostMessage(Handle, DSM_SHELLTREERESTORECURRENTPATH, Message.WPARAM, 0)
  else
    try
      APrevAutoExpand := AutoExpand;
      AutoExpand := False;
      try
        SendMessage(Handle, DSM_DONAVIGATE, Message.WPARAM, 0);
        DoNavigateListView;
      finally
        AutoExpand := APrevAutoExpand;
      end;
    finally
      DisposePidl(PItemIDList(Message.WPARAM));
    end;
end;

procedure TcxCustomInnerShellTreeView.DSMSynchronizeRoot(var Message: TMessage);
begin
  if not((Parent <> nil) and (csLoading in Parent.ComponentState)) then
    Root.Update(TcxCustomShellRoot(Message.WParam));
end;

procedure TcxCustomInnerShellTreeView.DSMSystemShellChangeNotify(var Message: TMessage);
var
  AEventID: Integer;
  APidl1, APidl2: PItemIDList;
begin
  cxShellGetNotifyParams(Message, AEventID, APidl1, APidl2);
  try
    ShellChangeNotify(AEventID, APidl1, APidl2);
  finally
    DisposePidl(APidl1);
    DisposePidl(APidl2);
  end;
end;

procedure TcxCustomInnerShellTreeView.DoNavigateListView;
var
  ATempPIDL: PItemIDList;
  ASelected: TTreeNode;
begin
  if not (csDestroying in ComponentState) then
  begin
    if (Items.Count > 0) and (DependedControls.Count > 0) then
    begin
      ASelected := Selected;
      if ASelected <> nil then
        ATempPIDL := GetItemProducer(ASelected).FolderPidl
      else
        ATempPIDL := GetItemProducer(Items[0]).FolderPidl;

      DependedControls.Navigate(ATempPIDL);
    end;
  end;
end;

procedure TcxCustomInnerShellTreeView.DoOnIdle(Sender: TObject; var Done: Boolean);
var
  AList: TList;
  AItem: TcxShellItemInfo;
begin
  AList := FItemsInfoGatherer.ProcessedItems.LockList;
  try
    if AList.Count > 0 then
      AItem := TcxShellItemInfo(AList.Extract(AList.First))
    else
      AItem := nil;
  finally
    FItemsInfoGatherer.ProcessedItems.UnlockList;
  end;
  if AItem <> nil then
    UpdateItem(AItem);
end;

procedure TcxCustomInnerShellTreeView.DragDropSettingsChanged(Sender: TObject);
begin
  SetComCtlStyle(Self, TVS_DISABLEDRAGDROP, not IsDragDropEnabled);
end;

procedure TcxCustomInnerShellTreeView.GetDropTarget(out ANew: Boolean; APoint: TPoint);

  function InternalGetDropTarget(ANode: TTreeNode; out ADropTarget: IcxDropTarget): Boolean;
  var
    AItemProducer: TcxShellTreeItemProducer;
    ATempShellItemInfo: TcxShellItemInfo;
    ATempPidl: PItemIDList;
    ATempShellFolder: IShellFolder;
  begin
    if ANode.Parent = nil then
    begin // Root object selected
      ATempShellFolder := GetItemProducer(ANode).ShellFolder;
      Result := (ATempShellFolder <> nil) and
        Succeeded(ATempShellFolder.CreateViewObject(Handle, IDropTarget, ADropTarget));
    end
    else
    begin // Non-root object selected
      AItemProducer := GetItemProducer(ANode.Parent);
      ATempShellItemInfo := GetShellItemInfo(ANode);
      ATempPidl := GetPidlCopy(ATempShellItemInfo.pidl);
      try
        if ATempShellItemInfo.IsFolder then
          Result := Succeeded(AItemProducer.ShellFolder.BindToObject(ATempPidl,
            nil, IID_IShellFolder, ATempShellFolder)) and
            Succeeded(ATempShellFolder.CreateViewObject(Handle, IDropTarget, ADropTarget))
        else
          Result := Succeeded(AItemProducer.ShellFolder.GetUIObjectOf(Handle, 1,
            ATempPidl, IDropTarget, nil, ADropTarget));
      finally
        DisposePidl(ATempPidl);
      end;
    end;
  end;

var
  ANode: TTreeNode;
  ADropTarget: IcxDropTarget;
begin
  if not DragDropSettings.AllowDragObjects then
  begin
    CurrentDropTarget := nil;
    Exit;
  end;
  APoint := dxMapWindowPoint(HWND_DESKTOP, Handle, APoint);
  ANode := GetNodeAt(APoint.X, APoint.Y);
  if ANode = nil then
    TryReleaseDropTarget;
  ANew := (ANode <> nil) and ((ANode <> PrevTargetNode) or (CurrentDropTarget = nil));
  if ANew then
  begin
    TryReleaseDropTarget;
    if InternalGetDropTarget(ANode, ADropTarget) then
    begin
      PrevTargetNode := ANode;
      CurrentDropTarget := ADropTarget;
    end;
  end;
end;

function TcxCustomInnerShellTreeView.GetNodeByPIDL(APIDL: PItemIDList): TTreeNode;
var
  AItemIndex, I: Integer;
  APID: PItemIDList;
  AItemProducer: TcxShellTreeItemProducer;
begin
  Result := nil;
  if APIDL = nil then
    Exit;

  if Root.ShellFolder = nil then
    Root.CheckRoot;
  if EqualPIDLs(Root.Pidl, APIDL) then
  begin
    Result := Items[0];
    Exit;
  end;

  if not IsSubPath(Root.Pidl, APIDL) then
    Exit;

  for I := 0 to GetPidlItemsCount(Root.Pidl) - 1 do
    APIDL := GetNextItemID(APIDL);
  Result := Items[0];
  for I := 0 to GetPidlItemsCount(APIDL) - 1 do
  begin
    APID := ExtractParticularPidl(APIDL);
    if APID = nil then
      Break;
    try
      AItemProducer := GetItemProducer(Result);
      AItemIndex := AItemProducer.GetItemIndexByPidl(APID);
      if not InRange(AItemIndex, 0, Result.Count - 1) then
      begin
        Result := nil;
        Break;
      end;
      Result := TTreeNode(AItemProducer.ItemInfo[AItemIndex].Data);
      APIDL := GetNextItemID(APIDL);
    finally
      DisposePidl(APID);
    end;
  end;
end;

function TcxCustomInnerShellTreeView.IsDragDropEnabled: Boolean;
begin
  Result := DragDropSettings.AllowDragObjects;
end;

function TcxCustomInnerShellTreeView.IsLoading: Boolean;
begin
  Result := csLoading in ComponentState;
end;

procedure TcxCustomInnerShellTreeView.RemoveChangeNotification;
begin
  cxShellUnregisterChangeNotifier(FShellChangeNotifierData);
end;

procedure TcxCustomInnerShellTreeView.RemoveDropTarget;
begin
  RevokeDragDrop(Handle);
end;

procedure TcxCustomInnerShellTreeView.RemoveItemProducer(
  AProducer: TcxShellTreeItemProducer);
var
  ATempList: TList;
begin
  ATempList := ItemProducersList.LockList;
  try
    ATempList.Remove(AProducer);
  finally
    ItemProducersList.UnlockList;
  end;
end;

procedure TcxCustomInnerShellTreeView.Change(Node: TTreeNode);
var
  ATopNode: TTreeNode;
begin
  inherited Change(Node);
  UpdateNode(Selected, not Navigation);
  ATopNode := TopItem;
  if FTopNode <> ATopNode then
  begin
    FTopNode := ATopNode;
    UpdateVisibleItems;
  end;
  if not Navigation then
    DoNavigateListView;
end;

procedure TcxCustomInnerShellTreeView.DsmDoNavigate(var Message: TMessage);
var
  ASourcePidl, ADestPidl: PItemIDList;
  AShellFolder: IShellFolder;
  APartDstPidl: PItemIDList;
  I: Integer;
  ATempProducer: TcxShellTreeItemProducer;
  ATempIndex: Integer;
begin
  if Failed(SHGetDesktopFolder(AShellFolder)) then
    Exit;
  Navigation := True;
  Items.BeginUpdate;
  try
    ASourcePidl := Root.Pidl;
    ADestPidl := PItemIDList(Message.WParam);
    if GetPidlItemsCount(ASourcePidl) > GetPidlItemsCount(ADestPidl) then
    begin
      Root.Pidl := ADestPidl;
      Items[0].Selected := True;
      Exit;
    end;
    for I := 0 to GetPidlItemsCount(ASourcePidl) - 1 do
      ADestPidl := GetNextItemID(ADestPidl);
    Selected := Items[0];
    for I := 0 to GetPidlItemsCount(ADestPidl) - 1 do
    begin
      ATempProducer := Selected.Data;
      APartDstPidl := ExtractParticularPidl(ADestPidl);
      ADestPidl := GetNextItemID(ADestPidl);
      if APartDstPidl = nil then
        Break;
      try
        ATempIndex := ATempProducer.GetItemIndexByPidl(APartDstPidl);
        if ATempIndex = -1 then
          Break;
        Selected := Selected.Item[ATempIndex];
      finally
        DisposePidl(APartDstPidl);
      end;
    end;
  finally
    Items.EndUpdate;
    Navigation := False;
  end;

  if Selected <> nil then
    PostMessage(Handle, TVM_ENSUREVISIBLE, 0, LPARAM(Selected.ItemId));
end;

{ TcxShellTreeItemProducer }

function TcxShellTreeItemProducer.GetItemsInfoGatherer: TcxShellItemsInfoGatherer;
begin
  Result := TreeView.ItemsInfoGatherer;
end;

procedure TcxShellTreeItemProducer.CheckForSubitems(AItem: TcxShellItemInfo);
begin
  inherited CheckForSubitems(AItem);
  if (AItem <> nil) and (not AItem.IsRemovable) then
    AItem.CheckSubitems(ShellFolder, GetEnumFlags);
end;

function TcxShellTreeItemProducer.SlowInitializationDone(AItem: TcxShellItemInfo): Boolean;
begin
  Result := AItem.Updated;
end;

function TcxShellTreeItemProducer.CheckUpdates: Boolean;

  procedure MergeItems(AFakeProducer: TcxShellTreeItemProducer);
  var
    I, J: Integer;
    AItem, ANewItem: TcxShellItemInfo;
    AFounded: Boolean;
    ANewItems: TList;
  begin
    ANewItems := AFakeProducer.Items;
    I := 0;
    while (I < Items.Count) do
    begin
      AItem := ItemInfo[I];
      AFounded := False;
      for J := 0 to ANewItems.Count - 1 do
      begin
        ANewItem := ANewItems[J];
        if SmallInt(ShellFolder.CompareIDs(0, AItem.pidl, ANewItem.pidl)) = 0 then
        begin
          ANewItems.Remove(ANewItem);
          ANewItem.Free;
          AFounded := True;
          Break;
        end;
      end;
      if not AFounded then
      begin
        NotifyRemoveItem(I);
        Items.Remove(AItem);
        AItem.Free;
      end
      else
        Inc(I);
    end;
    while ANewItems.Count > 0 do
    begin
      AItem := TObject(ANewItems.Extract(ANewItems.First)) as TcxShellItemInfo;
      if CanAddFolder(AItem.Folder) then
      begin
        ANewItem := TcxShellItemInfo.Create(Self, ShellFolder, FolderPidl,
          AItem.pidl, False);
        Items.Add(ANewItem);
        NotifyAddItem(Items.Count - 1);
      end;
      AItem.Free;
    end;
  end;

var
  AFakeProducer: TcxShellTreeItemProducer;
begin
  ShowHourglassCursor;
  AFakeProducer := TcxShellTreeItemProducer.Create(TreeView);
  try
    LockWrite;
    try
      AFakeProducer.Initialize(ShellFolder, FolderPidl);
      Result := AFakeProducer.EnumerateItems;
      MergeItems(AFakeProducer);
      if ShellItemInfo <> nil then
        ShellItemInfo.Updated := False;
    finally
      UnlockWrite;
    end;
  finally
    AFakeProducer.Free;
    HideHourglassCursor;
  end;
end;

constructor TcxShellTreeItemProducer.Create(AOwner: TWinControl);
begin
  inherited Create(AOwner);
  TreeView.AddItemProducer(Self);
end;

destructor TcxShellTreeItemProducer.Destroy;
begin
  if Assigned(FOnDestroy) then
    FOnDestroy(Self);
  TreeView.RemoveItemProducer(Self);
  inherited Destroy;
end;

function TcxShellTreeItemProducer.CanAddFolder(AFolder: TcxShellFolder): Boolean;
begin
  Result := TreeView.DoAddFolder(AFolder);
end;

procedure TcxShellTreeItemProducer.DoSlowInitialization(AItem: TcxShellItemInfo);
begin
  InitializeItem(AItem);
end;

function TcxShellTreeItemProducer.GetEnumFlags: Cardinal;
begin
  Result := TreeView.Options.GetEnumFlags;
end;

function TcxShellTreeItemProducer.GetShowToolTip: Boolean;
begin
  Result := TreeView.ShowInfoTips;
end;

procedure TcxShellTreeItemProducer.InitializeItem(AItem: TcxShellItemInfo);
begin
  inherited InitializeItem(AItem);
  CheckForSubitems(AItem);
end;

procedure TcxShellTreeItemProducer.NotifyAddItem(Index: Integer);
begin
  if (Owner.HandleAllocated) and (Node <> nil) then
    SendMessage(Owner.Handle, DSM_NOTIFYADDITEM, Index, LPARAM(Node));
end;

procedure TcxShellTreeItemProducer.NotifyRemoveItem(Index: Integer);
begin
  if (Owner.HandleAllocated) and (Node <> nil) then
    SendMessage(Owner.Handle, DSM_NOTIFYREMOVEITEM, Index, LPARAM(Node));
end;

procedure TcxShellTreeItemProducer.ProcessItems(AIFolder: IShellFolder;
  APIDL: PItemIDList; ANode: TTreeNode; APreloadItemCount: Integer);

  function SetNodeText: Boolean;
  var
    ATempPIDL: PItemIDList;
  begin
    Result := Node.Parent <> nil;
    if not Result then
      Exit;
    ATempPIDL := GetLastPidlItem(APIDL);
    Node.Text := GetShellItemDisplayName(
      TcxShellTreeItemProducer(ShellItemInfo.ItemProducer).ShellFolder,
      ATempPIDL, True);
  end;

var
  AFileInfo: TShFileInfo;
begin
  Node := ANode;
  cxShellGetThreadSafeFileInfo(PChar(APIDL), 0, AFileInfo, SizeOf(AFileInfo),
    SHGFI_PIDL or SHGFI_DISPLAYNAME or SHGFI_SYSICONINDEX);
  if not SetNodeText then
    Node.Text := StrPas(AFileInfo.szDisplayName);
  Node.ImageIndex := AFileInfo.iIcon;
  cxShellGetThreadSafeFileInfo(PChar(APIDL), 0, AFileInfo, SizeOf(AFileInfo),
    SHGFI_PIDL or SHGFI_SYSICONINDEX or SHGFI_OPENICON);
  Node.SelectedIndex := AFileInfo.iIcon;
  ProcessItems(AIFolder, APIDL, APreloadItemCount);
end;

procedure TcxShellTreeItemProducer.SetItemsCount(Count: Integer);
begin
  if (Owner.HandleAllocated) and (Node <> nil) then
     SendMessage(Owner.Handle, DSM_SETCOUNT, Count, LPARAM(Node));
end;

function TcxShellTreeItemProducer.GetTreeView: TcxCustomInnerShellTreeView;
begin
  Result := TcxCustomInnerShellTreeView(Owner);
end;

{ TcxShellImageList }

constructor TcxShellImageList.Create(ASizeFlags: Integer);
begin
  inherited Create(nil);
  SourceDPI := dxGetSystemDPI;
  ShareImages := True;
  Handle := cxShellGetImageList(ASizeFlags);
  BkColor := clNone;
end;

initialization
  NavigationLock := False;

end.
