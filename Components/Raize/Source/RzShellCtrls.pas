{===============================================================================
  RzShellCtrls Unit

  Raize Components - Component Source Unit

  Copyright © 1995-2015 by Raize Software, Inc.  All Rights Reserved.


  Components
  ------------------------------------------------------------------------------
  TRzShellTree
    Explorer-like tree view of the namespace.

  TRzShellList
    Explorer-like list view of folder contents.

  TRzShellCombo
    Explorer-like combobox of the namespace.


  Modification History
  ------------------------------------------------------------------------------
  6.2    (16 Jul 2015)
    * Fixed issue in TRzShellTree to allow non-folder items to appear in the
      shell tree view.
    * Fixed issue in TRzChangeHandlerThread in 64-bit applications.
    * The TRzShellCombo now correctly detects when drives are added or removed
      from the system (e.g. USB Drives).
    * Surfaced SelectionPen property in TRzShellTree.
  ------------------------------------------------------------------------------
  6.1.11 (08 Apr 2015)
    * When dragging a file from a TRzShellList and dropping it onto a different
      folder in the associated TRzShellTree, the shell tree no longer scrolls
      to the source folder represented by the shell list. That is, the shell
      tree maintains its current view.
  ------------------------------------------------------------------------------
  6.1.8  (16 Apr 2014)
    * Added stoFilesCanBeFolders option to the TRzShellTree .Options property.
  ------------------------------------------------------------------------------
  6.1.7  (07 Mar 2014)
    * Fixed potential sorting issue in TRzShellTree under 64-bit Windows.
  ------------------------------------------------------------------------------
  6.1.2  (22 Feb 2013)
    * Fixed issue in TRzShellCombo where selected items would always appear in
      the default font color rather than the highlight font color.
    * Added overloaded version of RefreshNodes method to TRzShellTree that takes
      a node parameter. The method will refresh all nodes below the specified
      node.
    * Surfaced the TRzChengeHandlerThread class.
  ------------------------------------------------------------------------------
  6.1    (20 Sep 2012)
    * Fixed issue in TRzCustomShellTree.DoSetSelectedIdList method where under
      certain situations, the correct information was not being correctly
      added to the new node.
    * Fixed auto-expand issue in TRzCustomShellTree when dragging files over
      a node that was previously expanded.
  ------------------------------------------------------------------------------
  6.0    (09 Oct 2011)
    * The TRzShellTree initializes the new ItemHeightMargin property inherited
      from the TRzTreeView component to a value of 3, which results in the
      TRzShellTree matching the spacing used by Windows for its shell tree in
      Explorer.
    * Fixed memory leak in TRzShellTree that could occur if the control's window
      handled needed to be recreated at runtime. For example, changing the
      BorderStyle property at runtime causes the control's window handle to be
      recreated. Very few properties caused the window to be recreated. However,
      the window does get recreated when changing VCL Styles at runtime.
    * The RefreshNodes and SortNode methods of TRzCustomShellTree are now
      dynamic methods.
    * Made necessary modifications to TRzShellTree, TRzShellList, and
      TRzShellCombo to fully support VCL Styles introduced in RAD Studio XE2.
    * Made necessary modifications to shell controls to support 64-bit.
  ------------------------------------------------------------------------------
  5.5    (06 Mar 2011)
    * Fixed AV that would occur when selecting the Homegroup item in a
      TRzShellTree or TRzShellList under Windows 7 and the Homegroup was
      currently empty.
  ------------------------------------------------------------------------------
  5.4    (14 Sep 2010)
    * Adjusted the default column widths of TRzShellList so that date/time stamp
      of file fits completely in the column.
  ------------------------------------------------------------------------------
  5.2    (05 Sep 2009)
    * The TRzShellTree now correctly raises the OnCancelEdit event when a node
      is edited and cancelled by pressing the Escape key.
    * For RAD Studio 2010, surfaced Touch property and OnGesture event in the
      TRzShellTree, TRzShellList, and TRzShellCombo controls.
  ------------------------------------------------------------------------------
  5.1.2  (11 Jun 2009)
    * Fixed scrolling issue in TRzShellTree when Mouse Wheel settings were set
      to scroll one screen at a time.
  ------------------------------------------------------------------------------
  5.1    (16 Mar 2009)
    * Fixed issue in TRzShellTree that prevented the stoDynamicRefresh option
      from working correctly in Delphi 2009.
    * Fixed issue that resulted in Access Violation when dragging and dropping
      in TRzShellTree and TRzShellList under Delphi 2009.
    * Fixed issue that caused Access Violation when calling the RefreshItems
      method of TRzShellList.
  ------------------------------------------------------------------------------
  5.0    (30 Sep 2008)
    * Fixed issue where the TRzShellList was unable to navigate to links in
      My Network Places.
    * Added the ItemHeight property to TRzShellTree, which can be used to
      specify the number of pixels to be used to display each node in the tree.
      The default value is 0, which instructs the tree view to use the default
      height as determined by the selected font.
  ------------------------------------------------------------------------------
  4.3.1  (24 Sep 2007)
    * Added Portugal Portuguese translations for Shell strings.
  ------------------------------------------------------------------------------
  4.2    (29 May 2007)
    * Fixed problem where setting sloHideFoldersWhenLinkedToTree option to True,
      zip files would get hidden.
  ------------------------------------------------------------------------------
  4.1.2  (17 Apr 2007)
    * The TRzShellList now correctly displays the appropriate sort indicator
      when in vsReport mode.
  ------------------------------------------------------------------------------
  4.1    (15 Dec 2006)
    * Added SFGAO_STREAM attribute bit in ShouldInclude method of TRzShellList.
    * Fixed some assignment issues of TRzShellLocator properties between
      TRzShellList, TRzShellTree, and TRzShellCombo.
  ------------------------------------------------------------------------------
  4.0    (23 Dec 2005)
    * Added new FrameControllerNotifications property to TRzShellTree,
      TRzShellList, and TRzShellCombo.
      The FrameControllerNotifications set property defines which
      TRzFrameController properties will be handled by the control.
      By default all TRzFrameController properties will be handled.
  ------------------------------------------------------------------------------
  3.1    (04 Aug 2005)
    * Fixed problem where TRzShellTree would not appear if placed on a
      TRzSplitter.  The problem was related to calling Items.EndUpdate in the
      DestroyWnd method.
  ------------------------------------------------------------------------------
  3.0.13 (15 May 2005)
    * Fixed TRzShellList.FileFilter when sloFilesCanBeFolders option is False.
  ------------------------------------------------------------------------------
  3.0.11 (12 Dec 2004)
    * Added sloFilesCanBeFolders to TRzShellList.Options.
    * Improved multi-language capability with non-English codepages.
    * Fixed declaration of OnAdvancedCustomDrawSubItem.
    * Improved auto-expand when dragging over TRzShellTree.
  ------------------------------------------------------------------------------
  3.0.10 (26 Dec 2003)
    * Fixed calls to SystemParametersInfo to obtain mouse hover times.
    * Fixed slow operation of TPTShellTree when mapped, disconnnected network
      drives were present.
  ------------------------------------------------------------------------------
  3.0.8  (29 Aug 2003)
    * Surfaced Align property in TRzShellCombo.
    * Fixed problem in TRzShellTree that would cause an AV when running under
      some versions of ComCtl32 in Windows 98.
  ------------------------------------------------------------------------------
  3.0.4  (04 Mar 2003)
    * Fixed flickering problem in TRzShellList caused by inherited
      FillLastColumn property.
  ------------------------------------------------------------------------------
  3.0.3  (21 Jan 2003)
    * Fixed problem in TRzCustomShellTree.DoSetSelectedIdList method where
      selected node wasn't being positioned correctly under certain
      circumstances.
  ------------------------------------------------------------------------------
  3.0    (20 Dec 2002)
    * Initial inclusion in Raize Components.
===============================================================================}

{$I RzComps.inc}

{$RANGECHECKS OFF}
{$WRITEABLECONST OFF}
{$TYPEDADDRESS ON}

unit RzShellCtrls;

{$IFDEF INTERNAL}
  // Define the symbol XDEBUG to turn on vigorous logfile debugging.
{$ENDIF}

interface

uses
  {$IFDEF USE_CS}
  CodeSiteLogging,
  {$ENDIF}
  Types,
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  ShellApi,
  StdCtrls,
  CommCtrl,
  ComCtrls,
  ExtCtrls,
  Menus,
  ShlObj,
  ActiveX,
  ImgList,
  RzShellIntf,
  RzTreeVw,
  RzListVw,
  RzShellUtils,
  RzCmboBx,
  RzCommon;

{ $DEFINE PTDEBUG}// <-- Define this symbol to turn on leak checking

// User messages
const
  RZSH_AM_CHANGE_NOTIFY = WM_USER + 101;
  RZSH_AM_DEFERRED_EDIT = WM_USER + 102;
  RZSH_AM_DEFERRED_FILL = WM_USER + 103;
  RZSH_AM_LAST = WM_USER + 150;

type
  TRzCustomShellCombo = class;
  TRzShellCombo = class;
  TRzShellComboData = class;
  TRzCustomShellTree = class;
  TRzShellTree = class;
  TRzShellTreeData = class;
  TRzCustomShellList = class;
  TRzShellList = class;
  TRzShellListData = class;

  TRzShellTreeOption =
  (
    stoAutoFill,
    stoVirtualFolders,
    stoDesignInteractive,
    stoDefaultKeyHandling,
    stoContextMenus,
    stoDynamicRefresh,
    stoIncludeNonFolders,
    stoOleDrag,                           {Allow automatic Ole drag operations}
    stoOleDrop,                           {Allow automatic Ole drop operations}
    stoShowHidden,
    stoFilesCanBeFolders
  );
  TRzShellTreeOptions = set of TRzShellTreeOption;

  TRzShellListOption =
  (
    sloAutoFill,
    sloNonFilesystemAncestors,
    sloDesignInteractive,
    sloDefaultKeyHandling,                {Backspace GoUp(1), Enter, Ctrl+A etc. - if folder open}
    sloContextMenus,                      {Allow context menus for items}
    sloDontChangeFolder,                  {Disallow GoUp and into folders}
    sloDontGoBelowBase,                   {Don't go below Folder}
    sloDynamicRefresh,                    {Automatic refresh when list items change}
    sloHideFoldersWhenLinkedToTree,       {What it says}
    sloOleDrag,                           {Allow automatic Ole drag operations}
    sloOleDrop,                           {Allow automatic Ole drop operations}
    sloFolderContextMenu,                 {Use default 'no items selected' context menu - not implemented yet}
    sloShowHidden,
    sloFilesCanBeFolders                  {Treat certain files (eg. .zip) as folders when supported by the OS}
  );
  TRzShellListOptions = set of TRzShellListOption;

  TRzShellComboOption =
  (
    scoAutofill,
    scoNonFilesystemAncestors
  );
  TRzShellComboOptions = set of TRzShellComboOption;


  TRzShAddItemEvent = procedure( Sender: TObject; ParentIShf: IShellFolder_NRC;
                                 ParentAbsIdList: PItemIdList; ItemRelIdList: PItemIdList;
                                 Attribs: Integer;                      // Attributes returned from IShellFolder::GetAttributesOf
                                 var AllowAdd: Bool ) of object;

  TRzShTreeInsertItemEvent = procedure( Sender: TObject; Node: TTreeNode ) of object;
  TRzShTreeDeleteItemEvent = procedure( Sender: TObject; Node: TTreeNode; TreeData: TRzShellTreeData ) of object;
  TRzShListDeleteItemEvent = procedure( Sender: TObject; Node: TListItem; ListData: TRzShellListData ) of object;
  TRzShComboDeleteItemEvent = procedure( Sender: TObject; Item: Integer; ComboData: TRzShellComboData ) of object;
  TRzShDblClickOpenEvent = procedure( Sender: TObject; var Handled: Boolean ) of object;
  TRzShPopupHintEvent = procedure( Sender: TObject; const Hint: string ) of object;

   {The TRzShellLocator class is effectively a Variant type for shell object locations.

    Use to reference a shell object. You can reference a shell object in one
    of three ways:
      1) By PIDL
      2) By pathname
      3) By CSIDL (indirect system reference)

    This is simplified by always storing PIDL or Pathname as PIDL (converting to/from pathname
    on read/write). Leaving two options:
      1) By PIDL (Pathname)
      2) By CSIDL

    You would want to use a CSIDL to locate such folders as the Start Menu, or Program Files folder.
    Navigating to your particular folder at design time and storing as a PIDL is not sufficient.
    Remember that a PIDL is just a binary pathname. For example, if you selected C:\WINDOWS\START MENU\
    as a PIDL on another machine the Start Menu might be at E:\WIN95\START MENU\ and your code will break.

    You should therefore use absolute PIDLs carefully. Items you are safe in referencing are anything that will
    appear on all computers (Desktop, My Computer, Network Neighbourhood, Recycle Bin). However, you could
    just as easily use a CSIDL to reference many of these objects as well.

    This class provides three properties for access in the three different ways. The mode is
    selected on write, but conversion is done on read (except for indirect system reference).

      mLocator.PIDL := pIdList;   // Selects PIDL/Pathname mode
      mLocator.Pathname := 'C:\'; // Selects PIDL/Pathname mode
      mLocator.CSIDL := csidlStartMenu;  // Selects sysref mode

    }
  TRzShellLocator_Which = ( usePidl, useCSIDL ); // C++Builder demands a formal type for enumerations.

  TRzShellLocator = class( TPersistent )
  protected
    FWhich: TRzShellLocator_Which;
    FIdList: PItemIdList;
    FCSIDL: TCSIDL;
    FOnChange: TNotifyEvent;

    procedure Changed; dynamic;

    function GetPathName: string;
    function GetIdList: PItemIdList;
    function GetCSIDL: TCSIDL;

    procedure SetIdList( Value: PItemIdList );
    procedure SetPathName( const Value: string );
    procedure SetCSIDL( Value: TCSIDL );

    procedure DefineProperties( Filer: TFiler ); override;
  public
    destructor Destroy; override;

    procedure Assign( Source: TPersistent ); override;
    function IsEqual( Value: TRzShellLocator ): Boolean;

    procedure Clear;

    procedure ReadData( Stream: TStream );
    procedure WriteData( Stream: TStream );

    property IdList: PItemIdList
      read GetIdList
      write SetIdList;

    property PathName: string
      read GetPathName
      write SetPathName;

    property CSIDL: TCSIDL
      read GetCSIDL
      write SetCSIDL;

    property OnChange: TNotifyEvent
      read FOnChange
      write FOnChange;
  end;


  {==========================================}
  {== TRzCustomShellTree Class Declaration ==}
  {==========================================}

  TRzCustomShellTree = class( TRzCustomTreeView )
  private
    {$IFDEF PTDEBUG}
    mdbgNodes: Integer;                     // Count of TRzShellTreeData nodes created. Used for debugging lifetime of TRzShellTreeData objects
    {$ENDIF}
    FBaseFolder: TRzShellLocator;
    FSelectedFolder: TRzShellLocator;

    FTimer: TTimer;

    FShellList: TRzCustomShellList;
    FOptions: TRzShellTreeOptions;
    FOnAddItem: TRzShAddItemEvent;
    FOnDeleteItem: TRzShTreeDeleteItemEvent;
    FOnInsertItem: TRzShTreeInsertItemEvent;
    FOnPopupHint: TRzShPopupHintEvent;

    FOnFillComplete: TNotifyEvent;
    FOnFillStart: TNotifyEvent;

    FShellCombo: TRzCustomShellCombo;            // If a TRzShellCombo component has its ShellTree property set
                                                 // then a backpointer is kept to the combo in this variable.

    FActiveContextMenu: IContextMenu_NRC;        // A valid IContextMenu interface when processing a right-click popup

    FQuickSelect: Boolean;             // Set True when you don't want a 500ms timeout before the select takes effect
    FIgnoreChanges: Integer;        // When >0 changes to selected PIDL should be ignored
    FIgnoreErrors: Integer;
    FEatExpand: Boolean;               // Set True around the inherited Loaded call to prevent TCustomTreeView's call to FullExpand from doing anything.

    FIgnoreNextChangeNotify: Boolean;    // When True the next change detected through AMChangeNotify is ignored.
                                         // Used when renaming items.
    FChangeHandlerThread: Pointer;
    FDeferRefresh: Boolean;
    FRefreshDeferred: Boolean;

    FLastNode: TTreeNode;

    FLoaded: Boolean;    // Set to True in Loaded method. This is to prevent unnecessary initialization if the
                         // window handle is created prematurely (like when making an ActiveX control)

    FDeletingNodes: Boolean;
    FSettingParent: Boolean;

  protected                                 // -- Ole drag/drop related members --
    FDragNode: TTreeNode;
    FIDataObject: IDataObject_NRC;          // Source IDataObject for a drag operation.
    FIDropTarget: IDropTarget_NRC;
    FILastDropDataObject: IDataObject_NRC;
    FInitialDropKeyState: Longint;
    FLastAutoScrollTick: Longint;
    FLastAutoOpenPos: TPoint;
    FLastAutoOpenTick: Longint;

    procedure OleBeginDrag( Button: TMouseButton );

    function CreateIDropSource( Button: TMouseButton; DataObject: IDataObject_NRC ): IDropSource_NRC; virtual;

    function OnDropTarget_DragEnter( const dataObj: IDataObject_NRC; grfKeyState: Longint; pt: TPoint;
                                     var dwEffect: Longint ): HResult; virtual;
    function OnDropTarget_DragOver( grfKeyState: Longint; pt: TPoint; var dwEffect: Longint ): HResult; virtual;
    function OnDropTarget_DragLeave: HResult; virtual;
    function OnDropTarget_Drop( const dataObj: IDataObject_NRC; grfKeyState: Longint; pt: TPoint;
                                var dwEffect: Longint ): HResult; virtual;

    procedure DoDropTargetAutoscroll( pt: TPoint ); virtual;

  private
    procedure InitImageList;
    procedure BaseFolderChanged( Sender: TObject );
    procedure SelectedFolderChanged( Sender: TObject );

    procedure TimerElapsed( Sender: TObject );

    function GetSelectedItem: TRzShellTreeData;
    function GetSelectedPathName: string;
    function GetShellTreeData( idx: Integer ): TRzShellTreeData;

    function GetParentHWND: HWND;

    procedure SetBaseFolder( Value: TRzShellLocator );
    procedure SetShellList( Value: TRzCustomShellList );
    procedure SetOptions( Value: TRzShellTreeOptions );
    procedure SetSelectedFolder( Value: TRzShellLocator );
    procedure DoSetSelectedIdList( Value: PItemIdList );

    procedure SetShellCombo( Value: TRzCustomShellCombo );

    procedure AMChangeNotify( var Msg: TMessage ); message RZSH_AM_CHANGE_NOTIFY;
    procedure AMDeferredFill( var Msg: TMessage ); message RZSH_AM_DEFERRED_FILL;

    procedure CMDesignHitTest( var Msg: TCMDesignHitTest ); message CM_DESIGNHITTEST;
    procedure CMWantSpecialKey( var Msg: TCMWantSpecialKey ); message CM_WANTSPECIALKEY;
    procedure CNNotify( var Msg: TWMNotify ); message CN_NOTIFY;

    procedure TVMDeleteItem( var Msg: TMessage ); message TVM_DELETEITEM;

    procedure WMMenuChar( var Msg: TWMMenuChar ); message WM_MENUCHAR;
    procedure WMDrawItem( var Msg: TWMDrawItem ); message WM_DRAWITEM;

    procedure WMNCDestroy( var Msg: TWMNCDestroy ); message WM_NCDESTROY;
    procedure WMNCHitTest( var Msg: TWMNCHitTest ); message WM_NCHITTEST;
    procedure WMMeasureItem( var Msg: TWMMeasureItem ); message WM_MEASUREITEM;
    procedure WMMenuSelect( var Msg: TWMMenuSelect ); message WM_MENUSELECT;
    procedure WMInitMenuPopup( var Msg: TWMInitMenuPopup ); message WM_INITMENUPOPUP;
    procedure WMWindowPosChanged( var Msg: TWMWindowPosChanged ); message WM_WINDOWPOSCHANGED;
    procedure WMPaint( var Msg: TWMPaint ); message WM_PAINT;

  protected
    procedure SetSelectedPathName( const Value: string );

    function AddNewShellNode( Parent: IShellFolder_NRC; ParentAbsIdList: PItemIdList; ParentNode: TTreeNode;
                              RelIdList: PItemIdList ): HResult; virtual;

    function CanAdd( ParentIShf: IShellFolder_NRC; ParentAbsPidl, ItemRelPidl: PItemIdList; Attribs: DWORD ): Bool; virtual;

    function CanExpand( Node: TTreeNode ): Boolean; override;
    function CanEdit( Node: TTreeNode ): Boolean; override;
    procedure Change( Node: TTreeNode ); override;

    procedure DeviceChangeDetected( Sender: TObject; var Msg: TMessage ); virtual;

    procedure DoOnInsertItem( Node: TTreeNode ); dynamic;

    procedure NodeContextMenu( Node: TTreeNode; var P: TPoint; var Menu: TPopupMenu ); override;
    function NodeHasData( Node: TTreeNode ): Boolean;

    procedure Delete( Node: TTreeNode ); override;
    procedure ExpandNode( Node: TTreeNode ); dynamic;
    procedure Edit( const Item: TTVItem ); override;
    procedure FillTree( ishf: IShellFolder_NRC; ABaseNode: TTreeNode ); virtual;
    procedure FillComplete; dynamic;
    procedure FillStart; dynamic;
    function GetFirstRootLevelShellNode: TTreeNode;
    procedure GetImageIndex( Node: TTreeNode ); override;
    procedure GetSelectedIndex( Node: TTreeNode ); override;
    procedure Loaded; override;
    procedure Notification( Component: TComponent; Operation: TOperation ); override;
    procedure CreateParams( var Params: TCreateParams ); override;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure SetParent( AParent: TWinControl ); override;
    procedure KeyDown( var Key: Word; ShiftState: TShiftState ); override;
    procedure ProcessSendTo( Node: TTreeNode; Index: Integer ); dynamic;
    function DoMouseWheelDown( Shift: TShiftState; MousePos: TPoint ): Boolean; override;
    function DoMouseWheelUp( Shift: TShiftState; MousePos: TPoint ): Boolean; override;

    procedure InstallChangeHandler( const PathName: string ); dynamic;
    procedure InstallChangeHandlersForAllLocalDrives;

    property SelectedItem: TRzShellTreeData
      read GetSelectedItem;

    property SelectedFolder: TRzShellLocator
      read FSelectedFolder
      write SetSelectedFolder;

    property SelectedPathName: string
      read GetSelectedPathName
      write SetSelectedPathName;

    property ShellTreeData[ Index: Integer ]: TRzShellTreeData
      read GetShellTreeData;

    property Items stored False;

    property BaseFolder: TRzShellLocator
      read FBaseFolder
      write SetBaseFolder;

    property ShellList: TRzCustomShellList
      read FShellList
      write SetShellList;

    property Options: TRzShellTreeOptions
      read FOptions
      write SetOptions
      default [ stoAutoFill, stoVirtualFolders, stoDefaultKeyHandling, stoContextMenus,
                stoDynamicRefresh, stoOleDrag, stoOleDrop, stoShowHidden ];

    property OnAddItem: TRzShAddItemEvent
      read FOnAddItem
      write FOnAddItem;

    property OnDeleteItem: TRzShTreeDeleteItemEvent
      read FOnDeleteItem
      write FOnDeleteItem;

    property OnInsertItem: TRzShTreeInsertItemEvent
      read FOnInsertItem
      write FOnInsertItem;

    property OnPopupHint: TRzShPopupHintEvent
      read FOnPopupHint
      write FOnPopupHint;

    property OnFillComplete: TNotifyEvent
      read FOnFillComplete
      write FOnFillComplete;

    property OnFillStart: TNotifyEvent
      read FOnFillStart
      write FOnFillStart;

  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

    procedure FillItems;
    procedure RefreshNodes; overload; dynamic;
    procedure RefreshNodes( Node: TTreeNode ); overload; dynamic;

    function GetDataFromNode( Node: TTreeNode ): TRzShellTreeData;
    procedure GoUp( Levels: Integer );

    function CreateNewFolder( EditNow: Boolean ): Boolean;
    function DoCommandForNode( Node: TTreeNode; Cmd: PAnsiChar ): HResult;
    function FindNodeWithIdList( BaseNode: TTreeNode; pidl: PItemIdList ): TTreeNode;
    procedure ProcessMenu( Node: TTreeNode; P: TPoint ); dynamic;

    procedure SortNode( Node: TTreeNode ); dynamic;

    procedure Synchronize( afApplyToGroup: Boolean );

    function GetDragDropAttributesForNode( Node: TTreeNode ): DWORD;

    { Inherited Properties & Events }
    property Height default 150;
    property Width default 150;
    property ItemHeightMargin default 3;
  end;


  {====================================}
  {== TRzShellTree Class Declaration ==}
  {====================================}

  TRzShellTree = class( TRzCustomShellTree )
  public
    property SelectedItem;
    property SelectedFolder;
    property SelectedPathName;
    property ShellTreeData;
    property Items;
  published
    property About: TRzAboutInfo
      read FAboutInfo
      write FAboutInfo
      stored False;

    { Inherited Properties & Events }
    property Align;
    property Anchors;
    property AutoExpand;
    property BaseFolder;
    property BiDiMode;
    property BorderStyle;
    property BorderWidth;
    property ChangeDelay;
    property Color;
    property Constraints;
    property Ctl3D;
    property DisabledColor;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property FocusColor;
    property FrameColor;
    property FrameControllerNotifications;
    property FrameController;
    property FrameHotColor;
    property FrameHotTrack;
    property FrameHotStyle;
    property FrameSides;
    property FrameStyle;
    property FrameVisible;
    property FramingPreference;
    property HideSelection;
    property HotTrack;
    // property Images;
    property Indent;
    property ItemHeight;
    property ItemHeightMargin;
    property MultiSelect;
    property MultiSelectStyle;
    property Options;
    property ParentBiDiMode;
    property ParentColor default False;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly default False;
    property RightClickSelect;
    property RowSelect;
    property SelectionPen;
    property ShellList;
    property ShowButtons;
    property ShowHint;
    property ShowLines;
    property ShowRoot default False;
    // property SortType;
    property StateImages;
    property TabOrder;
    property TabStop default True;
    property ToolTips;
    {$IFDEF VCL140_OR_HIGHER}
    property Touch;
    {$ENDIF}
    property Visible;

    property OnAddItem;
    property OnAdvancedCustomDraw;
    property OnAdvancedCustomDrawItem;
    property OnCancelEdit;
    property OnChange;
    property OnChanging;
    property OnClick;
    property OnCollapsed;
    property OnCollapsing;
    // property OnCompare;
    property OnContextPopup;
    property OnCustomDraw;
    property OnCustomDrawItem;
    property OnDblClick;
    property OnDeleteItem;
    property OnDeletion;
    property OnDragDrop;
    property OnDragOver;
    property OnEdited;
    property OnEditing;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnExpanded;
    property OnExpanding;
    property OnFillComplete;
    property OnFillStart;
    {$IFDEF VCL140_OR_HIGHER}
    property OnGesture;
    {$ENDIF}
    property OnGetImageIndex;
    property OnGetSelectedIndex;
    property OnInsertItem;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnNodeContextMenu;
    property OnPopupHint;
    property OnStartDock;
    property OnStartDrag;
  end;


  {==========================================}
  {== TRzCustomShellList Class Declaration ==}
  {==========================================}

  TRzCustomShellList_LCS = ( lcsNone, lcsShellDetails, lcsDefault ); // C++Builder demands formal type for enumerations.

  TRzCustomShellList = class( TRzCustomListView )
  private
    {$IFDEF PTDEBUG}
    mdbgNodes: Integer;                     // Count of TRzShellListData nodes created. Used for debugging lifetime of TRzShellListData objects
    {$ENDIF}
    FLastColState: TRzCustomShellList_LCS;
    FLastFolderWasDir: Boolean;

    FOnPopupHint: TRzShPopupHintEvent;
    FIShf: IShellFolder_NRC;                // When displaying items, this is the IShellFolder interface
    FIShfPidl: PItemIdList;                 // Item Id list of FIShf - Is this redundant? Surely FFolder.Pidl is what we want?

    FIShellDetailsValid: Boolean;
    FIShellDetails: IShellDetails_NRC;

    FFolder: TRzShellLocator;
    FOptions: TRzShellListOptions;
    FOnAddItem: TRzShAddItemEvent;
    FOnDeleteItem: TRzShListDeleteItemEvent;
    FOnDblClickOpen: TRzShDblClickOpenEvent;

    FOnFillComplete: TNotifyEvent;
    FOnFillStart: TNotifyEvent;
    FOnFolderChanged: TNotifyEvent;

    FFileFilter: string;                    // Storage for the FileFilter property - semicolon separated wildcards.
    FFilterLookupTable: Pointer;            // Used by filtering.
    FFilterExtensions: TStringList;         // When FFileFilter is assigned, mFileExtensions is filled as a list of each individual extension.

    FTimer: TTimer;

    FShellTree: TRzCustomShellTree;         // If a TRzShellTree component has its ShellList property set,
                                    // then a backpointer is kept to the tree in this variable.

    FShellCombo: TRzCustomShellCombo;       // If a TRzShellCombo component has its ShellList property set
                                    // then a backpointer is kept to the combo in this variable.

    FActiveContextMenu: IContextMenu_NRC;  // A valid IContextMenu interface when processing a right-click popup

    FSortColumn: Integer;                   // 1-based column index used to determine sort order.

    FIgnoreChanges: Integer;                // When >0 changes to Folder should not cause a refresh
    FIgnoreNextChangeNotify: Boolean;      // When True the next change detected through AMChangeNotify is ignored.
                                            // Used when renaming items.
    FChangeHandlerThread: Pointer;
    FDeferredEditName: string;

    FSkipRButtonUp: Boolean;
    FDeferRefresh: Boolean;
    FRefreshDeferred: Boolean;

        { The mCurrent* members are set during FillList and used in the InsertItem method to
          initialise the Data property of the item being inserted before the OnInsert event is
          called. }
    FCurrentItemData: TRzShellListData;
    FCurrentItemFlags: DWORD;
    FCurrentItemIShd: IShellDetails_NRC;

    FLastAutoScrollTick: Longint;

    FLoaded: Boolean;
           { Set to True in Loaded method. This is to prevent unnecessary initialization if the
             window handle is created prematurely (like when making an ActiveX control)}

    FInCreateWnd: Boolean;
           { When the list view is recreated (handle destroyed, then handle created again), VCL
             Streams out the state in the DestroyWnd proc and Streams it back in the CreateWnd proc.
             It uses a Streaming mechanism that causes Loaded to be called. This is not good because
             the control might not have been loaded at this point (and will not have in the case of
             an ActiveX control). This flag is used to prevent the Loaded method from doing anything
             in this special case. }

    FSortColumnAssigned: Boolean;
           { Used by ColClick to determine if the user is applying a custom sort to a column click
             event. }

    FDeletingItems: Boolean;

  protected                                 // -- Ole drag related members --
    FIDataObject: IDataObject_NRC;          // Source IDataObject for a drag operation.
    procedure OleBeginDrag( Button: TMouseButton );
  protected                                 // -- Ole drop related members --
    FIDropTarget: IDropTarget_NRC;
    FILastDropDataObject: IDataObject_NRC;
    FInitialDropKeyState: Longint;

    function CreateIDropSource( Button: TMouseButton; DataObject: IDataObject_NRC ): IDropSource_NRC; virtual;

    function OnDropTarget_DragEnter( const dataObj: IDataObject_NRC; grfKeyState: Longint; pt: TPoint;
                                     var dwEffect: Longint ): HResult; virtual;
    function OnDropTarget_DragOver( grfKeyState: Longint; pt: TPoint; var dwEffect: Longint ): HResult; virtual;
    function OnDropTarget_DragLeave: HResult; virtual;
    function OnDropTarget_Drop( const dataObj: IDataObject_NRC; grfKeyState: Longint; pt: TPoint;
                                var dwEffect: Longint ): HResult; virtual;

  private
    procedure InitColumns( ishd: IShellDetails_NRC );
    procedure InitImageLists;
    function IsFolderStored: Boolean;

    function GetCurrentFolderIShellDetails: IShellDetails_NRC;

    procedure HandleOnFolderChanged( Sender: TObject );

    function GetSelectedItem: TRzShellListData;
    function GetShellListData( Index: Integer ): TRzShellListData;

    procedure SetFileFilter( const Value: string );
    procedure SetFolder( Value: TRzShellLocator );
    procedure SetShellTree( Value: TRzCustomShellTree );
    procedure SetShellCombo( Value: TRzCustomShellCombo );
    procedure SetSortColumn( Value: Integer );
    procedure SetOptions( Value: TRzShellListOptions );

    procedure TimerElapsed( Sender: TObject );

    procedure AMChangeNotify( var Msg: TMessage ); message RZSH_AM_CHANGE_NOTIFY;
    procedure AMDeferredEdit( var Msg: TMessage ); message RZSH_AM_DEFERRED_EDIT;
    procedure AMDeferredFill( var Msg: TMessage ); message RZSH_AM_DEFERRED_FILL;
    procedure CMDesignHitTest( var Msg: TCMDesignHitTest ); message CM_DESIGNHITTEST;
    procedure CMWantSpecialKey( var Msg: TCMWantSpecialKey ); message CM_WANTSPECIALKEY;
    procedure CNNotify( var Msg: TWMNotify ); message CN_NOTIFY;
    procedure WMMenuChar( var Msg: TWMMenuChar ); message WM_MENUCHAR;
    procedure WMDrawItem( var Msg: TWMDrawItem ); message WM_DRAWITEM;
    procedure WMDestroy( var Msg: TWMDestroy ); message WM_DESTROY;
    procedure WMNCDestroy( var Msg: TWMNCDestroy ); message WM_NCDESTROY;
    procedure WMNCHitTest( var Msg: TWMNCHitTest ); message WM_NCHITTEST;
    procedure WMMeasureItem( var Msg: TWMMeasureItem ); message WM_MEASUREITEM;
    procedure WMMenuSelect( var Msg: TWMMenuSelect ); message WM_MENUSELECT;
    procedure WMRButtonUp( var Msg: TWMRButtonUp ); message WM_RBUTTONUP;
    procedure WMInitMenuPopup( var Msg: TWMInitMenuPopup ); message WM_INITMENUPOPUP;
    procedure WMWindowPosChanged( var Msg: TWMWindowPosChanged ); message WM_WINDOWPOSCHANGED;

    procedure WMGetIShellBrowser( var Msg: TMessage ); message WM_USER + 7; //--Undocumented
  protected
    function ShouldInclude( baseidlist, relidlist: PItemIdList; var attrib: DWORD ): Boolean;
    function AddNewShellItem( AbsIdList, RelIdList: PItemIdList ): HResult;
    function CanAdd( parentIShf: IShellFolder_NRC; parentAbsPidl: PItemIdList; itemRelPidl: PItemIdList;
                     itemAttributes: DWORD ): Bool; virtual;

    function CanEdit( Item: TListItem ): Boolean; override;
    function DblClickOpen: Boolean; dynamic;
    procedure DeviceChangeDetected( Sender: TObject; var Msg: TMessage ); virtual;

    procedure Edit( const Item: TLVItem ); override;
    procedure FillList( ishf: IShellFolder_NRC; basepidl: PItemIdList ); virtual;
    procedure FillComplete; dynamic;
    procedure FillStart; dynamic;
    procedure FolderChanged; dynamic;
    procedure Loaded; override;
    procedure Notification( Component: TComponent; Operation: TOperation ); override;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure TreeChanged( Node: TTreeNode ); // Called by TRzShellTree if the tree is associated with the list
    procedure Delete( Item: TListItem ); override;
    procedure ColClick( Column: TListColumn ); override;

    function GetUIObjectForAllSelected( const riid: TGUID; var interfaceOut: Pointer ): HResult; dynamic;
    function GetUIObjectForItem( Item: TListItem; const riid: TGUID; var interfaceOut: Pointer ): HResult; dynamic;
    procedure KeyDown( var Key: Word; ShiftState: TShiftState ); override;
    procedure KeyPress( var Key: Char ); override;
    procedure MouseDown( Button: TMouseButton; Shift: TShiftState; x, y: Integer ); override;
    procedure ItemContextMenu( Item: TListItem; var P: TPoint; var Menu: TPopupMenu ); override;

    function ItemHasData( Item: TListItem ): Boolean;
    procedure InsertItem( Item: TListItem ); override;
    function DoMouseWheelDown( Shift: TShiftState; MousePos: TPoint ): Boolean; override;
    function DoMouseWheelUp( Shift: TShiftState; MousePos: TPoint ): Boolean; override;

    procedure InstallChangeHandler; dynamic;

    procedure FilterPreApply; virtual;
    function FilterApply( const FileName: string; Attrib: DWORD ): Boolean; virtual;
    procedure FilterPostApply; virtual;

    function IsFolderNetworkShare: Boolean;


    property Columns stored False;
    property Items stored False;
    property AllocBy stored False;

    property SelectedItem: TRzShellListData
      read GetSelectedItem;

    property ShellListData[ Index: Integer ]: TRzShellListData
      read GetShellListData;

    property SortColumn: Integer
      read FSortColumn
      write SetSortColumn;

    property _IShellFolder: IShellFolder_NRC
      read FIShf;

    property Options: TRzShellListOptions
      read FOptions
      write SetOptions
      default [ sloAutofill, sloNonFilesystemAncestors, sloDefaultKeyHandling, sloContextMenus, sloDynamicRefresh,
                sloOleDrag, sloOleDrop, sloFolderContextMenu, sloShowHidden, sloFilesCanBeFolders ];

    property Folder: TRzShellLocator
      read FFolder
      write SetFolder
      stored IsFolderStored;

    property FileFilter: string
      read FFileFilter
      write SetFileFilter;

    property OnAddItem: TRzShAddItemEvent
      read FOnAddItem
      write FOnAddItem;

    property OnDeleteItem: TRzShListDeleteItemEvent
      read FOnDeleteItem
      write FOnDeleteItem;

    property OnPopupHint: TRzShPopupHintEvent
      read FOnPopupHint
      write FOnPopupHint;

    property OnDblClickOpen: TRzShDblClickOpenEvent
      read FOnDblClickOpen
      write FOnDblClickOpen;

    property OnFillComplete: TNotifyEvent
      read FOnFillComplete
      write FOnFillComplete;

    property OnFillStart: TNotifyEvent
      read FOnFillStart
      write FOnFillStart;

    property OnFolderChanged: TNotifyEvent
      read FOnFolderChanged
      write FOnFolderChanged;
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

    function CreateNewFolder( EditNow: Boolean ): Boolean;

    procedure DoCommandForItem( Item: TListItem; Cmd: PAnsiChar );
    procedure DoCommandForAllSelected( Cmd: PAnsiChar ); // See help for a list of available commands
    procedure DoCommandForFolder( Cmd: PAnsiChar );

    procedure FillItems;

    procedure GoUp( Levels: Integer );
    function GetDataFromItem( Item: TListItem ): TRzShellListData;
    procedure OpenItem( Item: TListItem );
    procedure OpenSelectedItems;
    procedure ProcessMenu( Item: TListItem; Pt: TPoint );
    procedure ProcessMenuForAllSelected( Pt: TPoint );
    procedure ProcessSendTo( Index: Integer );

    procedure RefreshItems;

    function GetDragDropAttributesForAllSelected: DWORD;
    function GetDragDropAttributesForItem( Item: TListItem ): DWORD;

    procedure SelectAll; override;

    function ShellSelCount: Integer;        // Returns the number of shell items selected. Non-shell items are not counted.
    procedure SortList;
    procedure Synchronize( ApplyToGroup: Boolean );
  end;


  {====================================}
  {== TRzShellList Class Declaration ==}
  {====================================}

  TRzShellList = class( TRzCustomShellList )
  public
    property SelectedItem;
    property ShellListData;
    property SortColumn;

        //-- From TRzCustomShellList
    property Columns;                       // Don't change the columns
    property Items;
    property _IShellFolder;

         // These
    property HeaderCanvas;
    property HeaderHandle;
    property HeaderSortColumn;
    property HeaderSortDirection;

  published
    property About: TRzAboutInfo
      read FAboutInfo
      write FAboutInfo
      stored False;

    { Inherited Properties & Events }
    property Align;
//    property AllocBy;
    property Anchors;
    property BiDiMode;
    property BorderStyle;
    property BorderWidth;
    property Checkboxes;
    property Color;
    property ColumnClick;
    property Constraints;
    property Ctl3D;
    property DisabledColor;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FileFilter;
    property FlatScrollBars;
    property Folder;
    property Font;
    property FocusColor;
    property FrameColor;
    property FrameControllerNotifications;
    property FrameController;
    property FrameHotColor;
    property FrameHotTrack;
    property FrameHotStyle;
    property FrameSides;
    property FrameStyle;
    property FrameVisible;
    property FramingPreference;
    property FullDrag;
    property GridLines;
    property HeaderSortDisplayMode;
    property HideSelection;
    property HotTrack;
    property HotTrackStyles default [ ];
    property IconOptions;
//    property LargeImages;
    property MultiSelect;
    property Options;
    property OwnerData;
    property OwnerDraw;
    property ParentBiDiMode;
    property ParentColor default False;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly default False;
    property RowSelect;
    property ShowColumnHeaders;
    property ShowHint;
//    property SmallImages;
//    property SortType;
    property StateImages;
    property TabOrder;
    property TabStop default True;
    {$IFDEF VCL140_OR_HIGHER}
    property Touch;
    {$ENDIF}
    property ViewStyle;
    property Visible;

    property OnAddItem;
    property OnAdvancedCustomDraw;
    property OnAdvancedCustomDrawItem;
    property OnAdvancedCustomDrawSubItem;
    property OnChange;
    property OnChanging;
    property OnClick;
    property OnColumnClick;
    property OnCompare;
    property OnContextPopup;
    property OnCustomDraw;
    property OnCustomDrawItem;
    property OnData;
    property OnDataFind;
    property OnDataHint;
    property OnDataStateChange;
    property OnDblClick;
    property OnDblClickOpen;
    property OnDeleteItem;
    property OnDeletion;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnEdited;
    property OnEditing;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnFillComplete;
    property OnFillStart;
    property OnFolderChanged;
    {$IFDEF VCL140_OR_HIGHER}
    property OnGesture;
    {$ENDIF}
    property OnGetImageIndex;
    property OnInfoTip;
    property OnInsert;
    property OnItemContextMenu;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnPopupHint;
    property OnResize;
    property OnSelectItem;
    property OnStartDock;
    property OnStartDrag;
  end;


  {===========================================}
  {== TRzCustomShellCombo Class Declaration ==}
  {===========================================}

  TRzCustomShellCombo = class( TRzCustomImageComboBox )
  private
    {$IFDEF PTDEBUG}
    mdbgNodes: Integer;                     // Count of TRzShellComboData nodes created. Used for debugging lifetime of TRzShellComboData objects
    {$ENDIF}
    FIgnoreChanges: Integer;

    FShellTree: TRzCustomShellTree;
    FShellList: TRzCustomShellList;
    FSelectedFolder: TRzShellLocator;

    FOptions: TRzShellComboOptions;
    FAutoDropDownCount: Boolean;

    procedure CMDesignHitTest( var Msg: TCMDesignHitTest ); message CM_DESIGNHITTEST;

    function GetSelectedFolder: TRzShellLocator;
    function GetShellComboData( Index: Integer ): TRzShellComboData;

    procedure SetSelectedFolder( Value: TRzShellLocator );
    procedure SetShellList( Value: TRzCustomShellList );
    procedure SetShellTree( Value: TRzCustomShellTree );

    procedure WMPaint( var Msg: TWMPaint ); message WM_PAINT;

  protected
    function CanAdd( ParentIShf: IShellFolder_NRC; ParentAbsPidl, ItemRelPidl: PItemIdList;
                     attribs: DWORD; Level: Integer ): Boolean; virtual;
    procedure SelEndOk; override;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure Loaded; override;
    procedure DeleteItem( Item: Pointer ); override;
    procedure DropDown; override;
    procedure GetItemData( Item: TRzImageComboBoxItem ); override;
    procedure SelectedFolderChanged( Sender: TObject ); virtual;
    procedure TreeChanged( Node: TTreeNode ); virtual;

    procedure DeviceChangeDetected( Sender: TObject; var Msg: TMessage ); virtual;

    procedure FillCombo( aIShf: IShellFolder_NRC;       // Base shellfolder - enumerates items at this level
                         aBasePidl: PItemIdList;        // Absolute ID list of ishf, nil for 'Desktop'
                         aIndent: Integer;              // Indentation for this group (-1 when ishf is Deskstop)
                         aSelectedItem: TRzIdListArray  // ItemIdList array of selected item
                        ); virtual;

    property AutoDropDownCount: Boolean
      read FAutoDropDownCount
      write FAutoDropDownCount
      default True;

    property ShellTree: TRzCustomShellTree
      read FShellTree
      write SetShellTree;

    property ShellList: TRzCustomShellList
      read FShellList
      write SetShellList;

    property ItemIndent;

    property Options: TRzShellComboOptions
      read FOptions
      write FOptions
      default [ scoAutofill, scoNonFilesystemAncestors ];

    property SelectedFolder: TRzShellLocator
      read GetSelectedFolder
      write SetSelectedFolder;

    property ShellComboData[ Index: Integer ]: TRzShellComboData
      read GetShellComboData;

    property Items stored False;
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

    procedure FillItems;

    procedure GoUp( Levels: Integer );

    procedure Synchronize( ApplyToGroup: Boolean );
  end;


  {=====================================}
  {== TRzShellCombo Class Declaration ==}
  {=====================================}

  TRzShellCombo = class( TRzCustomShellCombo )
  public
    property SelectedFolder;
    property ShellComboData;

  published
    property About: TRzAboutInfo
      read FAboutInfo
      write FAboutInfo
      stored False;

    property Align;
    property Anchors;
    property AutoDropDownCount;
    property BiDiMode;
    property Color;
    property Constraints;
    property Ctl3D;
    property DisabledColor;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DropDownCount;
    property DropDownWidth;
    property Enabled;
    property FlatButtons;
    property Font;
    property FocusColor;
    property FrameColor;
    property FrameControllerNotifications;
    property FrameController;
    property FrameHotColor;
    property FrameHotTrack;
    property FrameHotStyle;
    property FrameSides;
    property FrameStyle;
    property FrameVisible;
    property FramingPreference;
    property ImeMode;
    property ImeName;
    property ItemIndent;
//    property MaxLength;
    property Options;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShellList;
    property ShellTree;
    property ShowHint;
    property Sorted;
    property TabOrder;
    property TabStop;
//    property Text;
    {$IFDEF VCL140_OR_HIGHER}
    property Touch;
    {$ENDIF}
    property Visible;

    property OnChange;
    property OnClick;
    property OnCloseUp;
    property OnContextPopup;
    property OnDblClick;
    property OnDeleteItem;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnDropDown;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    {$IFDEF VCL140_OR_HIGHER}
    property OnGesture;
    {$ENDIF}
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnSelEndCancel;
    property OnSelEndOk;
    property OnStartDock;
    property OnStartDrag;
  end;


  {========================================}
  {== TRzShellTreeData Class Declaration ==}
  {========================================}

  TRzShellTreeData = class( TObject )
  private
    FOwner: TRzCustomShellTree;
    FParent: TRzShellTreeData;

    FRelPidl: PItemIdList;                  // Item ID list of this item relative to parent
    FAbsPidl: PItemIdList;                  // Absolute ID list of this item (parent+rel)
    FThisIshf: IShellFolder_NRC;            // IShellFolder for this node

    FParentIShf: IShellFolder_NRC;          // Used to hold the parent interface when FParent is nil
    FData: Pointer;

    function GetAbsPidl: PItemIdList;
    function GetAttributes: DWORD;
    function GetEditable: Boolean;
    function GetPathName: string;
    function GetParentIShf: IShellFolder_NRC;
    function GetThisIShf: IShellFolder_NRC;
    procedure SetData( AParent: TRzShellTreeData; thisRelativePidl: PItemIdlist );
    procedure SetRelPidl( newRelPidl: PItemIdList ); // Called by editing code
  public
    constructor Create( AOwner: TRzCustomShellTree );
    destructor Destroy; override;

    function GetIDropTarget( h: HWND; var idt: IDropTarget_NRC ): HResult;

    function IsRootDir: Boolean;
    procedure Flush;                        // Remove any cached data (FAbsPidl and FThisIshf);

    property Attributes: DWORD
      read GetAttributes;

    property Parent: TRzShellTreeData
      read FParent;

    property ParentIShf: IShellFolder_NRC
      read GetParentIShf;

    property AbsoluteIdList: PItemIdList
      read GetAbsPidl;

    property RelativeIdList: PItemIdList
      read FRelPidl;

    property ThisIShf: IShellFolder_NRC
      read GetThisIshf;

    property PathName: string
      read GetPathName;

    property Editable: Boolean
      read GetEditable;

    property Data: Pointer
      read FData
      write FData; // User defined data
  end;


  {========================================}
  {== TRzShellListData Class Declaration ==}
  {========================================}

  TRzShellListData = class( TObject )
  private
    FOwner: TRzCustomShellList;
    FAbsPidl: PItemIdList;
    FRelPidl: PItemIdList;
    FData: Pointer;
    FDisplayName: string;

    FDataValid: Boolean;
    FSize: string;
    FType: string;
    FModified: string;

    function GetAbsoluteIdList: PItemIdList;
    function GetAttributes: DWORD;
    function GetEditable: Boolean;
    function GetDisplayName: string;
    function GetFilename: string;
    function GetPathName: string;

    function GetColText( col: Integer ): string;

    procedure GetExtraData;

    function GetSize: string;
    function GetFileType: string;
    function GetModified: string;

    procedure SetData( aRelPidl: PItemIdList );
  public
    constructor Create( aOwner: TRzCustomShellList );
    destructor Destroy; override;

    function IsFolder: Boolean;
    function IsLnkShortcut: Boolean;
    function IsValid: Boolean;
    function IsFileSystem: Boolean;

    procedure Flush;

    property Owner: TRzCustomShellList
      read FOwner;

    property AbsoluteIdList: PItemIdList
      read GetAbsoluteIdList;

    property Editable: Boolean
      read GetEditable;

    property RelativeIdList: PItemIdList
      read FRelPidl;

    property Attributes: DWORD
      read GetAttributes;

    property DisplayName: string
      read GetDisplayName;

    property FileName: string
      read GetFilename;

    property PathName: string
      read GetPathName;

    property ColText[ Col: Integer ]: string
      read GetColText;

    property Size: string
      read GetSize;

    property FileType: string
      read GetFileType;

    property Modified: string
      read GetModified;

    property Data: Pointer
      read FData
      write FData; // User defined data
  end;


  {=========================================}
  {== TRzShellComboData Class Declaration ==}
  {=========================================}

  TRzShellComboData = class( TObject )
  protected
    FOwner: TRzCustomShellCombo;
    FIShfParent: IShellFolder_NRC;          // Interface of this node's parent
    FParentPidl: PItemIdList;
    FRelPidl: PItemIdList;
    FAbsPidl: PItemIdList;
    FThisIshf: IShellFolder_NRC;
    FData: Pointer;
    function GetThisIShf: IShellFolder_NRC;
    procedure SetData( aParentIShf: IShellFolder_NRC; parentPidl, curRelativePidl: PItemIdList );
  public
    constructor Create( aOwner: TRzCustomShellCombo );
    destructor Destroy; override;

    property ParentIdList: PItemIdList
      read FParentPidl;

    property ParentIShf: IShellFolder_NRC
      read FIShfParent;

    property RelativeIdList: PItemIdList
      read FRelPidl;

    property AbsoluteIdList: PItemIdList
      read FAbsPidl;

    property ThisIShf: IShellFolder_NRC
      read GetThisIshf;

    property Data: Pointer
      read FData
      write FData; // User defined data
  end;



  {==============================================}
  {== TRzChangeHandlerThread Class Declaration ==}
  {==============================================}

  TRzChangeHandlerThread = class( TThread )
  protected
    FTerminateEx: Boolean;
    FDoActionEvent: THandle;
    FActionProcessedEvent: THandle;
    FPendingChangeHandler: THandle;
    FPendingRemoveIndex: Integer;
    FOwnerWnd: HWND;
    FChangeMsgId: UINT;
    FChangeHandlerList: TList;
    FNewHandlerIndex: Integer;
    FThreadAction: ( taAddNewHandler, taRemoveHandler, taRemoveAllMonitors, taTerminate );
  public
    constructor Create( OwnerWnd: HWND; ChangeMsgId: UINT );
    destructor Destroy; override;
    procedure Execute; override;

    function AddMonitorDir( Pathname: string; Flags: DWORD; Recursive: Boolean; var Token: Pointer ): Boolean;
    procedure RemoveAllMonitors;
  end;




function RzShCreateNewFolder( aPathname: string; var aNewName: string ): Boolean;

function RzShIsFolder( Attributes: DWORD; CanTreatFilesAsFolders: Boolean ): Boolean;

var
  RZSH_CHANGE_NOTIFY_DELAY: Integer = 2000; // Milliseconds delay between when a change is detected and when the view is updated.
  RZSH_CHANGE_NOTIFY_FASTDELAY: Integer = 400; // Milliseconds delay between when a change is detected and when the view is updated when the control is focused.
  RZSH_TREE_KEY_UPDATE_DELAY: Integer = 500; // Milliseconds delay between a keyboard based move of the tree selection
                                                 // and when any associated tree or combo is updated.
  RZSH_MAX_FOLDER_ATTEMPTS: Integer = 50;   // Maximum number of times to try creating a new folder

  RZSH_AUTOSCROLL_THRESHOLD_X: Integer = ( 20 * 13 ) div 10;
  RZSH_AUTOSCROLL_THRESHOLD_Y: Integer = 20;
  RZSH_AUTOSCROLL_MINDELAY_MS: Integer = 100; // Milliseconds minimum delay between autoscrolls during OLE drag/drop

  RZSH_AUTOOPEN_DELAY_MS: Integer = 400;
  RZSH_AUTOOPEN_THRESHOLD_X: Integer = 2;
  RZSH_AUTOOPEN_THRESHOLD_Y: Integer = 2;

var
  CF_IDLIST: Word;                          // Registered clipboard format for IDList structures

{*****************************************************************************}
implementation

uses
  {$IFDEF VCL170_OR_HIGHER}
  UITypes,
  {$ENDIF}
  Registry,
  ComObj,
  AnsiStrings,
  RzShellConsts;

function _RegisterDragDrop( wnd: HWnd; dropTarget: IDropTarget_NRC ): HResult; stdcall; external 'ole32.dll' name 'RegisterDragDrop';

function _DoDragDrop( dataObj: IDataObject_NRC; dropSource: IDropSource_NRC; dwOKEffects: Longint; var dwEffect: Longint ): HResult; stdcall; external 'ole32.dll' name 'DoDragDrop';

{Some consistent command ids}
const
  ICM_CREATESHORTCUT = 16;
  ICM_DELETE = 17;
  ICM_RENAME = 18;
  ICM_PROPERTIES = 19;

  ICM_CUT = 24;
  ICM_COPY = 25;

{$IFDEF PTDEBUG}
var
  g_TreeNodes: Integer = 0;
  g_ListNodes: Integer = 0;
  g_ComboNodes: Integer = 0;

  g_DataObject_shlv: Integer = 0;
  g_DropTarget_shlv: Integer = 0;

  g_DropSource_shlvtv: Integer = 0;

  g_DataObject_shtv: Integer = 0;
  g_DropTarget_shtv: Integer = 0;

  g_FormatEtcList: Integer = 0;
  g_FormatEtcList_IEnumFormatEtc: Integer = 0;

  g_ChangeHandlerThread: Integer = 0;
{$ENDIF}

const
  CMDOFFSET = 1000;

{-- Internationalisation helpers -------------------------}
type
  TRzShellControlDefKeyRec = record
    str: string;
    shortcut: TShortcut;
  end;

  TRzShellControlDefKey = ( scdkSelectAll, scdkDelete, scdkCopy, scdkCut, scdkPaste, scdkRefresh, scdkEdit );

var
  gShellControlDefKeys: array[ TRzShellControlDefKey ] of TRzShellControlDefKeyRec = (
    ( str: SShKey_SelectAll; shortcut: 0 ),
    ( str: SShKey_Delete; shortcut: 0 ),
    ( str: SShKey_Copy; shortcut: 0 ),
    ( str: SShKey_Cut; shortcut: 0 ),
    ( str: SShKey_Paste; shortcut: 0 ),
    ( str: SShKey_Refresh; shortcut: 0 ),
    ( str: SShKey_Edit; shortcut: 0 )
    );

  gShellControlDefKeys_Valid: Boolean = False;

  { This version of TextToShortcut uses non-translated source text.
    "Ctrl" is always "Ctrl" no matter what language version of Delphi is being used. }

function PTTextToShortCut( Text: string ): TShortCut;
    { If the front of Text is equal to Front then remove the matching piece
      from Text and return True, otherwise return False }
  function CompareFront( var Text: string; const Front: string ): Boolean;
  begin
    Result := False;
    if ( Length( Text ) >= Length( Front ) ) and
       ( AnsiStrLIComp( PChar( Text ), PChar( Front ), Length( Front ) ) = 0 ) then
    begin
      Result := True;
      Delete( Text, 1, Length( Front ) );
    end;
  end;                                      {CompareFront - local}

  function ShortCutToText( ShortCut: TShortCut ): string;
  const
    N20_28: array[ $20..$28 ] of string = ( 'Space', 'PgUp', 'PgDn', 'End', 'Home', 'Left', 'Up', 'Right', 'Down' );
  var
    Name: string;
  begin
    case WordRec( ShortCut ).Lo of
      $08: Name := 'BkSp';
      $09: Name := 'Tab';
      $0D: Name := 'Enter';
      $1B: Name := 'Esc';
      $20..$28: Name := N20_28[ WordRec( ShortCut ).Lo ];
      $2D: Name := 'Ins';
      $2E: Name := 'Del';
      $30..$39: Name := Chr( WordRec( ShortCut ).Lo - $30 + Ord( '0' ) );
      $41..$5A: Name := Chr( WordRec( ShortCut ).Lo - $41 + Ord( 'A' ) );
      $60..$69: Name := Chr( WordRec( ShortCut ).Lo - $60 + Ord( '0' ) );
      $70..$87: Name := 'F' + IntToStr( WordRec( ShortCut ).Lo - $6F );
    else
      Name := '#' + IntToStr( Integer( ShortCut ) );
    end;
    if Name <> '' then
    begin
      Result := '';
      if ShortCut and scShift <> 0 then
        Result := Result + 'Shift+';
      if ShortCut and scCtrl <> 0 then
        Result := Result + 'Ctrl+';
      if ShortCut and scAlt <> 0 then
        Result := Result + 'Alt+';
      Result := Result + Name;
    end
    else
      Result := '';
  end;                                      {ShortCutToText - local}

var
  Key: TShortCut;
  Shift: TShortCut;
begin
  Result := 0;
  Shift := 0;
  while True do
  begin
    if CompareFront( Text, 'Shift+' ) then
      Shift := Shift or scShift
    else if CompareFront( Text, '^' ) then
      Shift := Shift or scCtrl
    else if CompareFront( Text, 'Ctrl+' ) then
      Shift := Shift or scCtrl
    else if CompareFront( Text, 'Alt+' ) then
      Shift := Shift or scAlt
    else
      Break;
  end;
  if Text = '' then
    Exit;
  for Key := $08 to $255 do                 { Copy range from table in ShortCutToText }
    if AnsiCompareText( Text, ShortCutToText( Key ) ) = 0 then
    begin
      Result := Key or Shift;
      Exit;
    end;
end;

procedure InitDefKeys;
var
  I: TRzShellControlDefKey;
begin
  if not gShellControlDefKeys_Valid then
  begin
    for I := Low( I ) to High( I ) do
      gShellControlDefKeys[ I ].shortcut := PTTextToShortcut( gShellControlDefKeys[ I ].str );
    gShellControlDefKeys_Valid := True;
  end;
end;                                        {InitDefKeys}
{== END Internationalisation Helpers =====================}


{-- Clipboard/Data Object utilities ----------------------}
const
  DRAGFLAGS = SFGAO_CANCOPY or SFGAO_CANMOVE or SFGAO_CANLINK;

type
  TRzFormatEtcList = class( TObject )
  private
    mList: TList;
    function GetItem( index: Integer ): PFormatEtc;
    function GetItemCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add( const aFmt: array of Word );
    procedure Clear;
    function CreateIEnumFormatEtc: IEnumFormatEtc_NRC;
    function FormatSupported( const aFmt: TFormatEtc ): HResult;
    property ItemCount: Integer read GetItemCount;
    property Items[ index: Integer ]: PFormatEtc read GetItem; default;
  end;                                      {TRzFormatEtcList}

       // IEnumFormatEtc interface handler for TRzFormatEtcList object
  TRzFormatEtcList_IEnumFormatEtc = class( TObject )
  private
    mcRefs: Integer;
    FOwner: TRzFormatEtcList;
    mCurrent: Integer;
  public
    constructor Create( aOwner: TRzFormatEtcList; aCurrent: Integer );
    destructor Destroy; override;

         // -- IUnknown
    function QueryInterface( const IID: TGUID; var Obj ): Integer; virtual; stdcall;
    function AddRef: Integer; virtual; stdcall;
    function Release: Integer; virtual; stdcall;
         // -- IEnumFormatEtc
    function Next( celt: Longint; var {out}  elt; pceltFetched: PLongint ): HResult; virtual; stdcall;
    function Skip( celt: Longint ): HResult; virtual; stdcall;
    function Reset: HResult; virtual; stdcall;
    function Clone( var {out}  enum: IEnumFormatEtc_NRC ): HResult; virtual; stdcall;
  end;


  {-- TRzFormatEtcList ------------------}

function NewFormatEtc( const aFmt: TFormatEtc ): PFormatEtc;
begin
  New( Result );
  Result^ := aFmt;
  if Assigned( aFmt.ptd ) then
  begin
    GetMem( Result.ptd, aFmt.ptd.tdSize );
    CopyMemory( Result.ptd, aFmt.ptd, aFmt.ptd.tdSize );
  end;
end;

procedure DisposeFormatEtc( pfmt: PFormatEtc );
begin
  if Assigned( pfmt.ptd ) then
    FreeMem( pfmt.ptd, pfmt.ptd.tdSize );
  Dispose( pfmt );
end;

constructor TRzFormatEtcList.Create;
begin
  {$IFDEF PTDEBUG}
  Inc( g_FormatEtcList );
  {$ENDIF}
  inherited;
  mList := TList.Create;
end;

destructor TRzFormatEtcList.Destroy;
begin
  {$IFDEF PTDEBUG}
  Dec( g_FormatEtcList );
  {$ENDIF}
  Clear;
  mList.Free;
  inherited;
end;

function TRzFormatEtcList.GetItem( index: Integer ): PFormatEtc;
begin
  Result := mList[ index ];
end;

function TRzFormatEtcList.GetItemCount: Integer;
begin
  Result := mList.Count;
end;

procedure TRzFormatEtcList.Add( const aFmt: array of Word );
var
  I: Integer;
  fme: TFormatEtc;
begin
  ZeroMemory( @fme, SizeOf( fme ) );
  fme.ptd := nil;
  fme.dwAspect := DVASPECT_CONTENT;
  fme.lIndex := -1;
  fme.tymed := TYMED_HGLOBAL;
  for I := Low( aFmt ) to High( aFmt ) do
  begin
    fme.cfFormat := aFmt[ I ];
    mList.Add( NewFormatEtc( fme ) );
  end;
end;

procedure TRzFormatEtcList.Clear;
var
  I: Integer;
begin
  for I := 0 to mList.Count - 1 do
    DisposeFormatEtc( Items[ I ] );
  mList.Clear;
end;

function TRzFormatEtcList.CreateIEnumFormatEtc: IEnumFormatEtc_NRC;
begin
  Result := Pointer( TRzFormatEtcList_IEnumFormatEtc.Create( Self, 0 ) );
end;

function TRzFormatEtcList.FormatSupported( const aFmt: TFormatEtc ): HResult;
var
  I: Integer;
  fAnyMatchingFormats: Boolean;
  function ClipFormatsEqual( const aFmt1, aFmt2: TFormatEtc ): Boolean;
  begin
    Result := ( aFmt1.cfFormat = aFmt2.cfFormat );
  end;
  function TymedsIntersect( const aFmt1, aFmt2: TFormatEtc ): Boolean;
  begin
    Result := ( aFmt1.tymed and aFmt2.tymed ) <> 0;
  end;
begin
  if aFmt.dwAspect <> DVASPECT_CONTENT then
  begin
    Result := DV_E_FORMATETC;
    Exit;
  end;

  fAnyMatchingFormats := False;

  for I := 0 to ItemCount - 1 do
    if ClipFormatsEqual( Items[ I ]^, aFmt ) then
    begin
      fAnyMatchingFormats := True;
      if TymedsIntersect( Items[ I ]^, aFmt ) then
      begin
        Result := S_OK;
        Exit;
      end;
    end;

  if fAnyMatchingFormats then
    Result := DV_E_TYMED
  else
    Result := DV_E_FORMATETC;
end;                                        {TRzFormatEtcList.FormatSupported}


  {-- TRzFormatEtcList_IEnumFormatEtc -----}

constructor TRzFormatEtcList_IEnumFormatEtc.Create( aOwner: TRzFormatEtcList; aCurrent: Integer );
begin
  {$IFDEF PTDEBUG}
  Inc( g_FormatEtcList_IEnumFormatEtc );
  {$ENDIF}
  inherited Create;
  FOwner := aOwner;
  mCurrent := aCurrent;
end;

destructor TRzFormatEtcList_IEnumFormatEtc.Destroy;
begin
  {$IFDEF PTDEBUG}
  Dec( g_FormatEtcList_IEnumFormatEtc );
  {$ENDIF}
  inherited;
end;

function TRzFormatEtcList_IEnumFormatEtc.QueryInterface( const IID: TGUID; var Obj ): Integer;
begin
  if IsEqualIID( iid, IID_IUnknown ) or IsEqualIID( iid, IID_IEnumFormatEtc ) then
  begin
    Pointer( obj ) := Self;
    AddRef;
    Result := S_OK;
  end
  else
  begin
    Pointer( obj ) := nil;
    Result := Integer( E_NOINTERFACE );
  end;
end;

function TRzFormatEtcList_IEnumFormatEtc.AddRef: Integer;
begin
  Inc( mcRefs );
  Result := mcRefs;
end;

function TRzFormatEtcList_IEnumFormatEtc.Release: Integer;
begin
  Dec( mcRefs );
  Result := mcRefs;
  if mcRefs = 0 then
    Free;
end;

function TRzFormatEtcList_IEnumFormatEtc.Next( celt: Longint; var {out}  elt; pceltFetched: PLongint ): HResult;
type
  TFormatList = packed array[ 0..255 ] of TFormatEtc;
var
  I: Integer;
begin
  I := 0;
  while ( I < celt ) and ( mCurrent < FOwner.ItemCount ) do
  begin
    TFormatList( elt )[ I ] := FOwner.Items[ mCurrent ]^;
    Inc( mCurrent );
    Inc( I );
  end;
  if pceltFetched <> nil then
    pceltFetched^ := I;
  if I = celt then
    Result := S_OK
  else
    Result := S_False;
end;

function TRzFormatEtcList_IEnumFormatEtc.Skip( celt: Longint ): HResult;
begin
  if celt <= FOwner.ItemCount - mCurrent then
  begin
    mCurrent := mCurrent + celt;
    Result := S_OK;
  end
  else
  begin
    mCurrent := FOwner.ItemCount;
    Result := S_False;
  end;
end;

function TRzFormatEtcList_IEnumFormatEtc.Reset: HResult;
begin
  mCurrent := 0;
  Result := S_OK;
end;

function TRzFormatEtcList_IEnumFormatEtc.Clone( var {out}  enum: IEnumFormatEtc_NRC ): HResult;
begin
  Pointer( enum ) := TRzFormatEtcList_IEnumFormatEtc.Create( FOwner, mCurrent );
  Result := S_OK;
end;

function Create_CFHDROP_HGlobal( files: TStrings; var h: THandle ): HResult;
var
  ptrDROPFILES: PDropFiles;
  CurPos, I: integer;
  FileName: AnsiString;
begin
  h := GlobalAlloc( GMEM_MOVEABLE or GMEM_ZEROINIT, Sizeof( TDROPFILES ) + MAX_PATH * files.Count + 1 );
  if ( h = 0 ) then
  begin
    Result := E_OUTOFMEMORY;
    Exit;
  end;

  ptrDROPFILES := GlobalLock( h );

  CurPos := sizeof( TDROPFILES );
  for I := 0 to files.Count - 1 do
  begin
    ptrDROPFILES.pFiles := sizeof( TDROPFILES );
    ptrDROPFILES.fWide := False;
    ptrDROPFILES.fNC := False;
    ptrDROPFILES.pt.x := 0;
    ptrDROPFILES.pt.y := 0;

    FileName := AnsiString( files[ I ] + chr( 0 ) );
    {$IFDEF VCL180_OR_HIGHER}AnsiStrings.{$ENDIF}StrPCopy( ( PAnsiChar( ptrDROPFILES ) + CurPos ), FileName );
    CurPos := CurPos + Length( FileName );
  end;
  GlobalUnlock( h );
  Result := S_OK;
end;                                        {Create_CFHDROP_HGlobal}


  {The CF_IDLIST format consists of an array count, an offset table and a list of id lists.

  the absolute id list of the parent folder, then the relative
   id list for each of the required children. }

function Create_CFIDLIST_HGlobal( absParentIdList: PItemIdList;
  itemRelIdList: TList;
  var h: THandle ): HResult;
var
  elements: UINT;
  p, curp: PAnsiChar;
  curindexpos: PAnsiChar;
  blocksize, I, itemlen: Integer;
  w: Word;
begin
  if not Assigned( absParentIdList ) then
  begin                                     // Sometimes the desktop likes to be referred to as 'nil', other times as '$0000'. This is one of the $0000 times.
    absParentIdList := Pointer( @w );
    w := 0;
  end;

  elements := itemRelIdList.Count;

   // Calculate the block size
  blocksize := SizeOf( UINT ) + Sizeof( UINT ) + Sizeof( UINT ) * elements; // element count, offset of parent, offsets of relatives
  blocksize := blocksize + IdListLen( Pointer( absParentIdList ) ); // parent id list
  for I := 0 to itemRelIdList.Count - 1 do  // list of relative ids
    blocksize := blocksize + IdListLen( itemRelIdList[ I ] );

   // Allocate the block
  h := GlobalAlloc( GMEM_MOVEABLE or GMEM_ZEROINIT, blocksize );
  if ( h = 0 ) then
  begin
    Result := E_OUTOFMEMORY;
    Exit;
  end;

   // Fill the block
  p := GlobalLock( h );
  curp := p;
  PIDA( curp ).cidl := elements;
  Inc( curp, Sizeof( UINT ) );
  curindexpos := curp;
  Inc( curp, Sizeof( UINT ) + Sizeof( UINT ) * elements ); // skip offset index table

  itemlen := IdListLen( Pointer( absParentIdList ) );
  Move( absParentIdList^, curp^, itemlen );
  PUINT( curindexpos )^ := Integer( curp ) - Integer( p );
  Inc( curindexpos, Sizeof( UINT ) );
  Inc( curp, itemlen );

  for I := 0 to itemRelIdList.Count - 1 do
  begin
    itemlen := IdListLen( itemRelIdList[ I ] );
    Move( itemRelIdList[ I ]^, curp^, itemlen );
    PUINT( curindexpos )^ := Integer( curp ) - Integer( p );
    Inc( curindexpos, Sizeof( UINT ) );
    Inc( curp, itemlen );
  end;

  GlobalUnlock( h );
  Result := S_OK;
end;                                        {Create_CFIDLIST_HGlobal}



function DuplicateHGlobal( hsrc: THandle ): THandle;
var
  pdest, psrc: Pointer;
  size: Integer;
begin
  size := Windows.GlobalSize( hsrc );
  Result := Windows.GlobalAlloc( GMEM_MOVEABLE, size );
  if ( Result <> 0 ) then
  begin
    pdest := Windows.GlobalLock( Result );
    psrc := Windows.GlobalLock( hsrc );
    System.Move( psrc^, pdest^, size );
    Windows.GlobalUnlock( hsrc );
    Windows.GlobalUnlock( Result );
  end;
end;                                        {DuplicateHGlobal}


  // Drop source object implementing IDropSource for the given shell list or tree
  // D2/D3/BCB
type
  TDropSource_shlvtv = class( TObject )
  private
    mcRefs: Integer;
    mDragButton: Integer;
  public
    constructor Create( aDragButton: TMouseButton );
    destructor Destroy; override;
         // -- IUnknown --
    function QueryInterface( const IID: TGUID; var Obj ): Integer; virtual; stdcall;
    function AddRef: Integer; virtual; stdcall;
    function Release: Integer; virtual; stdcall;
         // -- IDropSource --
    function QueryContinueDrag( fEscapePressed: Bool; grfKeyState: Longint ): HResult; virtual; stdcall;
    function GiveFeedback( dwEffect: Longint ): HResult; virtual; stdcall;
  end;

constructor TDropSource_shlvtv.Create( aDragButton: TMouseButton );
begin
  {$IFDEF PTDEBUG}
  Inc( g_DropSource_shlvtv );
  {$ENDIF}
  inherited Create;
  case aDragButton of
    mbLeft: mDragButton := MK_LBUTTON;
    mbRight: mDragButton := MK_RBUTTON;
    mbMiddle: mDragButton := MK_MBUTTON;
  end;
end;

destructor TDropSource_shlvtv.Destroy;
begin
  {$IFDEF PTDEBUG}
  Dec( g_DropSource_shlvtv );
  {$ENDIF}
  inherited;
end;


function TDropSource_shlvtv.QueryInterface( const IID: TGUID; var Obj ): Integer;
begin
  if IsEqualIID( iid, IID_IUnknown ) or IsEqualIID( iid, IID_IDropSource ) then
  begin
    Pointer( obj ) := Self;
    AddRef;
    Result := S_OK;
  end
  else
  begin
    Pointer( obj ) := nil;
    Result := Integer( E_NOINTERFACE );
  end;
end;

function TDropSource_shlvtv.AddRef: Integer;
begin
  Inc( mcRefs );
  Result := mcRefs;
end;

function TDropSource_shlvtv.Release: Integer;
begin
  Dec( mcRefs );
  Result := mcRefs;
  if ( mcRefs = 0 ) then
    Free;
end;

function TDropSource_shlvtv.QueryContinueDrag( fEscapePressed: Bool; grfKeyState: Longint ): HResult;
const
  BUTTONS = ( MK_LBUTTON or MK_MBUTTON or MK_RBUTTON );
begin
  if fEscapePressed or ( ( ( mDragButton xor BUTTONS ) and grfKeyState ) <> 0 ) then
    Result := DRAGDROP_S_CANCEL
  else if ( grfKeyState and mDragButton ) = 0 then
    Result := DRAGDROP_S_DROP
  else
    Result := S_OK;
end;

function TDropSource_shlvtv.GiveFeedback( dwEffect: Longint ): HResult;
begin
  Result := DRAGDROP_S_USEDEFAULTCURSORS;
end;
{== END Clipboard/Data Object utilities ==================}

function CreateShellDetailsAdapter( const AShellFolder: IShellFolder_NRC ): IShellDetails_NRC; forward;

type
  TShellFolder2ToShellDetailsAdapter = class( IShellDetails_NRC )
  private
    FCount: Integer;
    FShellFolder2: IShellFolder2_NRC;
  public
    constructor Create( AShellFolder: IShellFolder2_NRC );
    destructor Destroy; override;
  // -- IUnknown
    function QueryInterface( const IID: TGUID; var Obj ): HResult; override;
    function AddRef: Integer; override;
    function Release: Integer; override;
  // -- IShellDetails
    function GetDetailsOf( pidl: PItemIdList; col: UINT; var info: TShColInfo ): HResult; override;
    function ColumnClick( col: UINT ): HResult; override;
  end;


{-- Global procedures ----------------}

function RzShCreateNewFolder( aPathname: string; var aNewName: string ): Boolean;
type
  TAttemptResult = ( rOk, rExists, rFailure );

  function OneAttempt( u: UINT; ALongName: Boolean ): TAttemptResult;
  var
    s: string;
    dw: DWORD;
  begin
    if ALongName then
    begin
      s := SNewFolder;
      if ( u > 1 ) then
        s := s + Format( ' (%d)', [ u ] );
    end
    else
    begin
      s := 'fldr';
      if ( u > 1 ) then
        s := s + Format( '%d', [ u ] );
    end;

    if not Windows.CreateDirectory( PChar( aPathname + s ), nil ) then
    begin
      dw := GetLastError;
      if ( dw = ERROR_ALREADY_EXISTS ) then
        Result := rExists
      else
      begin
        MessageDlg( FormatStrPos( SUnableToCreateFolder, [ SysErrorMessage( dw ), s ] ),
          mtWarning, [ mbOk ], 0 );
        Result := rFailure;
      end;
    end
    else
    begin
      aNewName := s;
      Result := rOk;
    end;
  end;

  function Attempt( ALongName: Boolean ): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    for I := 1 to RZSH_MAX_FOLDER_ATTEMPTS do
      case OneAttempt( I, ALongName ) of
        rOk:
          begin
            Result := True;
            Break;
          end;
        rExists: ;                          // Continue
        rFailure: Exit;
      end;
  end;

begin  {= RzShCreateNewFolder =}

  if Length( aPathname ) = 0 then
    aPathName := GetCurrentDir;

  aPathName := EnsureTrailingCharDB( aPathName, '\' );

  Result := Attempt( True );
  if not Result then
    Result := Attempt( False );

  if not Result then
    MessageDlg( FormatStrPos( SUnableToCreateFolder, [ SysErrorMessage( ERROR_ALREADY_EXISTS ), SNewFolder ] ),
      mtWarning, [ mbOk ], 0 );
end; {= RzShCreateNewFolder =}


function RzShIsFolder( Attributes: DWORD; CanTreatFilesAsFolders: Boolean ): Boolean;
begin
  Result := ( Attributes and SFGAO_FOLDER <> 0 ) and
            ( CanTreatFilesAsFolders or
              (SHELL32_VER.version < SHELL32_VER60) or
              ( ( SHELL32_VER.version >= SHELL32_VER60 ) and
                ( Attributes and SFGAO_STREAM = 0 )
              )
            );
end;



{-- Local Procedures -----------------}
const
  SHCONTF_INCLUDEHIDDEN_FLAG: array[ Boolean ] of DWORD = ( 0, SHCONTF_INCLUDEHIDDEN );
  SHCONTF_NONFOLDERS_FLAG: array[ Boolean ] of DWORD = ( 0, SHCONTF_NONFOLDERS );

procedure RaiseSysError( code: DWORD );
begin
  raise Exception.Create( SysErrorMessage( code ) );
end;

function IsPathRoot( s: string ): Boolean;
begin
  Result := ( Length( s ) > 3 ) and not ( IsDBCSLeadByte( Byte( s[ 1 ] ) ) and ( s[ 2 ] = ':' ) );
end;

procedure SetWndStyle( ahWindow: HWND; aStyleBits: DWORD; aSetStyle: Boolean );
var
  orgStyle, style: DWORD;
begin
  if ahWindow <> 0 then
  begin
    orgStyle := GetWindowLong( ahWindow, GWL_STYLE );
    style := orgStyle;
    if not aSetStyle then
      Style := Style and not aStyleBits
    else
      Style := Style or aStyleBits;
    if ( style <> orgStyle ) then
      SetWindowLong( ahWindow, GWL_STYLE, Style );
  end;
end;                                        {SetWndStyle}


{ Make a 64-bit integer from two 32-bit integers. }
function MakeComp( lo, hi: DWORD ): Int64;
begin
  Int64Rec( Result ).Lo := lo;
  Int64Rec( Result ).Hi := hi;
end;


function GetExplorerDisplaySize( size: Int64 ): string;
var
  tmpd: Double;
begin
  tmpd := Int( size / 1024 );
  if Frac( size / 1024 ) > 0 then
    tmpd := tmpd + 1;
  Result := Format( SFilesizeKB, [ Format( '%.0n', [ tmpd ] ) ] );
end;



  {-- Routines to convert system time formats to Delphi TDateTime --}

function SystemTimeToDateTime( const aTime: TSystemTime ): TDateTime;
begin
  with aTime do
    Result := EncodeDate( wYear, wMonth, wDay ) + EncodeTime( wHour, wMinute, wSecond, wMilliseconds );
end;

function LocalFileTimeToDateTime( const ft: TFileTime ): TDateTime;
var
  st: TSystemTime;
begin
  FileTimeToSystemTime( ft, st );
  Result := SystemTimeToDateTime( st );
end;

function UTCFileTimeToDateTime( const ft: TFileTime ): TDateTime;
var
  tmpft: TFileTime;
begin
  FileTimeToLocalFileTime( ft, tmpft );
  Result := LocalFileTimeToDateTime( tmpft );
end;


procedure GetFileStrings( pidl: PItemIdList;  var aSize, aDate: String );
var
  pathname: String;
  w32fd: TWin32FindData;
  h: THandle;
  uCode: DWORD;
begin
  aSize := '';
  aDate := '';
  pathname := RzShellUtils.ShellGetPathFromIdList( pidl );
  if Length( pathname ) > 3 then
  begin
    uCode := SetErrorMode( 0 );
    try
      SetErrorMode( uCode or SEM_FAILCRITICALERRORS );

      h := Windows.FindFirstFile( PChar( pathname ), w32fd );
      if ( h <> INVALID_HANDLE_VALUE ) then
      begin
        if ( ( w32fd.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY ) = 0 ) then
          aSize := GetExplorerDisplaySize( MakeComp( w32fd.nFileSizeLow, w32fd.nFileSizeHigh ) );

        if ( w32fd.ftLastWriteTime.dwLowDateTime <> 0 ) or ( w32fd.ftLastWriteTime.dwHighDateTime <> 0 ) then
          aDate := FormatDateTime( FormatSettings.ShortDateFormat + ' ' + FormatSettings.ShortTimeFormat, UTCFileTimeToDateTime( w32fd.ftLastWriteTime ) )
        else
          aDate := FormatDateTime( FormatSettings.ShortDateFormat + ' ' + FormatSettings.ShortTimeFormat, UTCFileTimeToDateTime( w32fd.ftCreationTime ) )
      end;
      Windows.FindClose( h );
    finally
      SetErrorMode( uCode );
    end;
  end;
end; {= GetFileStrings =}


function GetTypeName( pidl: PItemIdList; attrib: DWORD ): string;
var
  shfi: TSHFileInfo;
begin
  if ( ( attrib and SFGAO_FILESYSTEM ) = 0 ) then
    Result := SSystemFolder     // If the item is not part of the filesystem it's a "System Folder".
                                       // We need to state this since SHGetFileInfo returns "File Folder" for these folders
                                       // eg. My Computer, Network Neighbourhood
  else
  begin
    if SHGetFileInfo( PChar( pidl ), 0, shfi, Sizeof( TSHFileInfo ),
      SHGFI_PIDL or
      SHGFI_TYPENAME
      ) <> 0 then
      Result := shfi.szTypeName
    else
      Result := '';
  end;
end;                                        {GetTypeName}

procedure GetFileStrings2( abspidl: PItemIdList; attr: DWORD;
  var aSize, aType, aDate: string );
begin
  GetFileStrings( abspidl, aSize, aDate );
  aType := GetTypeName( abspidl, attr );
end;

function GetFriendlyName( ishf: IShellFolder_NRC; pidl: PItemIdList; flags: DWORD ): string;
var
  strret: TStrRet;
  dw: DWord;
begin
  strret.uType := STRRET_WSTR;
  Result := '';
  dw := ishf.GetDisplayNameOf( pidl, flags, strret );
  if ( dw = S_OK ) then
  begin
    case strret.uType of
      STRRET_CSTR:
        Result := string( strret.cstr ); // typecast needed when compiling under RS2009

      STRRET_OFFSET:
        SetString( Result, PChar( UINT( pidl ) + strret.uOffset ), StrLen( PChar( UINT( pidl ) + strret.uOffset ) ) );

      STRRET_WSTR:
        begin
          Result := WideCharToString( strret.pOleStr );
          CoTaskMemFree( strret.pOleStr );
        end;
    end;
  end;
  {An error returned in 'dw' is deliberately not reported due to some shell folders mistakenly
   returning an error code when non existed (e.g. Recycle Bin in some cases). MS Explorer does not
   report errors from GetDisplayNameOf either.}
end;                                        {GetFriendlyName}


{
  Creates a popup menu and populates it with the context menu items for the given shellFolder:relPidl item.
  [in]  hwnd    - Window handle for error boxes (can be null for no error boxes)
  [in]  ishf    - Source IShellFolder interface
  [in]  count   - Number of items
  [in]  relpidl - Relative PIDL based on ishf
  [in]  baseid  - The command ids of the menu items start at this value
  [in]  opt     - Type of items to populate
  [out] h       - Handle of created popup, 0 if not created
  [out] icm     - IContextMenu interface, nil if not created
}

function CreateAndPopulateContextMenu( { in} hwnd: HWND;
                                       { in} ishf: IShellFolder_NRC;
                                       { in} count: Integer;
                                       { in} relpidl: PPItemIdList;
                                       { in} baseid: DWORD;
                                       { in} opt: DWORD;
                                       {out} var h: HMENU;
                                       {out} var icm: IContextMenu_NRC ): HResult;
begin
  h := 0;
  icm := nil;
  Result := ishf.GetUIObjectOf( hwnd, count, relpidl^, IID_IContextMenu, nil, Pointer( icm ) );
  if Failed( Result ) then
    Exit;
  try
    h := CreatePopupMenu;
    if ( h = 0 ) then
      Exit;
    Result := icm.QueryContextMenu( h, 0, baseid, $7FFF, opt );
    OleCheck( Result );
  except
    if Assigned( icm ) then
    begin
      icm.Release;
      icm := nil;
    end;
    if ( h <> 0 ) then
    begin
      DestroyMenu( h );
      h := 0;
    end;
    raise;
  end;
end;                                        {CreateAndPopulateContextMenu}


{
  [in] hwnd    - Window handle to be owner of interface elements
  [in] icm     - IContextMenu interface
  [in] item    - item to execute
}

function ExecuteContextMenuItem( {in} h: HWND;
                                 {in} icm: IContextMenu_NRC;
                                 {in} item: PAnsiChar ): HResult;
  function Invoke( p: PAnsiChar ): HResult;
  var
    cmi: TCMInvokeCommandInfo;
  begin
    cmi.cbSize := SizeOf( TCMInvokeCommandInfo ); // must be sizeof(CMINVOKECOMMANDINFO)
    cmi.fMask := 0;                         // any combination of CMIC_MASK_*
    cmi.hwnd := h;                          // might be NULL (indicating no owner window)
    cmi.lpVerb := p;                        // either a string of MAKEINTRESOURCE(idOffset)
    cmi.lpParameters := nil;                // might be NULL (indicating no parameter)
    cmi.lpDirectory := nil;                 // might be NULL (indicating no specific directory)
    cmi.nShow := SW_SHOWNORMAL;             // one of SW_ values for ShowWindow() API
    cmi.dwHotKey := 0;
    cmi.hIcon := 0;
    Result := icm.InvokeCommand( cmi );     // We don't report errors as they are typically already reported
  end;                                      {Invoke - local}

  function PtGetMenuString( AMenu: HMENU; ID: Integer ): string;
  var
    Buf: array[ 0..MAX_PATH ] of Char;
  begin
    GetMenuString( AMenu, ID, @Buf[ 0 ], High( Buf ) - 1, MF_BYPOSITION );
    Result := string( Buf );
  end;

  function FindCommandId( icm: IContextMenu_NRC; item: PAnsiChar ): PAnsiChar;
  var
    curid: Integer;
    hm: HMenu;
    I, max: Integer;
    ca: array[ 0..MAX_PATH ] of AnsiChar;
  begin                                     // Finds the command Id from the string - sometimes string commands don't work, so we look up the id and use that
    Result := nil;
    hm := CreatePopupMenu;
    if ( hm = 0 ) then
      Exit;
    try
      OleCheck( icm.QueryContextMenu( hm, 0, CMDOFFSET, $7FFF, CMF_EXPLORE ) );

      max := GetMenuItemCount( hm ) - 1;
      for I := 0 to max do
      begin
        curid := GetMenuItemid( hm, I );
        ca[ 0 ] := #0;
        if ( curid < CMDOFFSET ) or
          ( icm.GetCommandString( curid - CMDOFFSET, GCS_VERB, nil, @ca[ 0 ], High( ca ) ) <> NOERROR ) then
          Continue;

        if {$IFDEF VCL180_OR_HIGHER}AnsiStrings.{$ENDIF}AnsiStrIComp( ca, item ) = 0 then
        begin
          Result := PAnsiChar( curid - CMDOFFSET );
          Exit;
        end;
      end;
    finally
      if ( hm <> 0 ) then
        DestroyMenu( hm );
    end;
  end;                                      {FindCommandId - local}

var
  p: PAnsiChar;
begin
  if HiWord( Integer( item ) ) = 0 then
    p := item
  else
    p := FindCommandId( icm, item );
  Result := Invoke( p );
end;


{ Given an absolute id list and the (optional) associated shell folder interface, return the
  parent IShellFolder interface. }

function GetIShellFolderParent( aAbsIdl: PItemIdList;
  var aparent: IShellFolder_NRC;
  var parentIdlist, relativeIdList: PItemIdList ): HResult;
var
  pa: TRzIdListArray;
  tmppidl: PItemIdList;
begin
  Result := S_OK;
  tmppidl := nil;
  pa := TRzIdListArray.Create( aAbsIdl );
  try
    if ( pa.ItemCount <= 0 ) then
    begin
      Result := ShellGetIdListFromPath( ShellGetSpecialFolderPath( 0, csidlDesktop ), tmppidl );
      if Failed( Result ) then
        Exit;
      pa.Free;
      pa := TRzIdListArray.Create( tmppidl );
    end
    else if ( pa.ItemCount <= 1 ) then
    begin
      ShellGetDesktopFolder( aparent );
      parentIdList := nil;
      relativeIdList := CopyIdList( nil, aAbsIdl );
      Exit;
    end;
    parentIdList := CopyIdList( nil, pa.GoUp( 1 ) );
    relativeIdList := CopyIdList( nil, pa[ pa.ItemCount - 1 ] );
    Result := ShellGetFolderFromIdList( parentIdList, aParent );
    if Failed( Result ) then
      Exit;
  finally
    pa.Free;
    if Assigned( tmppidl ) then
      ShellMemFree( tmppidl );
  end;
end;                                        {GetIShellFolderParent}


function IShf_GetUIObjectOf( absIdList: PItemIdList; const iid: TGUID; var ppvOut: Pointer ): HResult;
var
  ishfp: IShellFolder_NRC;
  parentpidl, relpidl: PItemIdList;
begin
  ishfp := nil;
  parentpidl := nil;
  relpidl := nil;
  try
    Result := GetIShellFolderParent( absIdList, ishfp, parentpidl, relpidl );
    if Failed( Result ) then
      Exit;
    Result := ishfp.GetUIObjectOf( 0, 1, relpidl, iid, nil, ppvOut );
    if Failed( Result ) then
      Exit;
  finally
    if Assigned( ishfp ) then
      ishfp.Release;
    ShellMemFree( parentpidl );
    ShellMemFree( relpidl );
  end;
end;                                        {IShf_GetUIObjectOf}


{Gets the IDropTarget interface for the desktop. This isn't as easy as it sounds, since we need to
 get the Desktop's parent to be able to query for the IDropTarget interface. Instead of looking at
 the 'Desktop as root of the namespace' which has no parent, we use the 'c:\windows\desktop' folder
 which is equivalent in this case.}

function GetDesktopIDT( var idt: IDropTarget_NRC ): HResult;
var
  p: PItemIdList;
begin
  Result := ShellGetIdListFromPath( ShellGetSpecialFolderPath( 0, csidlDesktop ), p );
  if Failed( Result ) then
    Exit;
  try
    Result := IShf_GetUIObjectOf( p, IID_IDropTarget, Pointer( idt ) );
  finally
    ShellMemFree( p );
  end;
end;                                        {GetDesktopIDT}


{ Used by the tree and list to handle auto-scrolling. We simply tell the control to perform the
  appropriate scrollbar message. }

function DoAutoScroll( ctl: TWinControl; code, direction: Integer ): Boolean;
const
  conversion: array[ SB_HORZ..SB_VERT ] of Integer = ( WM_HSCROLL, WM_VSCROLL );
var
  info: TScrollInfo;
begin
  ZeroMemory( @info, Sizeof( info ) );
  info.cbSize := Sizeof( TScrollInfo );
  info.fMask := SIF_ALL;
  Windows.GetScrollInfo( ctl.Handle, code, info );
  if ( info.nMax - info.nMin - Integer( info.nPage ) ) > 0 then
    ctl.Perform( conversion[ code ], direction, 0 );
  Result := info.nPos <> Windows.GetScrollPos( ctl.Handle, code );
end;

function DoWheelScroll( aCtl: TWinControl; aSBar, aShort, aLong: DWORD ): Boolean;
var
  lines: DWord;
begin
  lines := Mouse.WheelScrollLines;
  if lines = WHEEL_PAGESCROLL then
    DoAutoScroll( aCtl, aSBar, aLong )
  else
  begin
    if aSBar = SB_HORZ then
      lines := 1;
    while ( ( lines and $FF ) <> 0 ) do     // the "and $FF" is just a sanity check
    begin
      DoAutoScroll( aCtl, aSBar, aShort );
      Dec( lines );
    end;
  end;
  Result := True;
end;                                        {DoWheelScroll}


{ Gets the CF_IDLIST format from the given IDataObject and determines what drop states are supported by the
  listed objects. States are copy, move and link. This is sort of the drop equivalent of
  TRzCustomShellList.GetDragDropAttributesForAllSelected . }

function GetDragDropAttributesForClipboardObjects( const dataObj: IDataObject_NRC ): DWORD;
type
  PPDWORD = ^PDWORD;
var
  iparentshf: IShellFolder_NRC;
  parentidl, p: PItemIdList;                   // Not a copy, points directly into pdata
  pdata: Pointer;
  curp: UINT;
  formatEtc: TFormatEtc;
  stgMedium: TStgMedium;
  childList: TList;
  dwa: TList;
//-- vars used during structure traversal--
  numElements: Integer;
  I: Integer;
  attr: DWORD;
begin                                       // Note that DROPEFFECT_COPY/MOVE/LINK and SFGAO_COPY/MOVE/LINK are equivalent (thankfully!)
  Result := DROPEFFECT_COPY or DROPEFFECT_MOVE or DROPEFFECT_LINK;
  childList := nil;
  dwa := nil;
  iparentshf := nil;
  ZeroMemory( @stgMedium, SizeOf( stgMedium ) );
  pData := nil;
  try
    childList := TList.Create;
    dwa := TList.Create;

   // Get a pointer to the CF_IDLIST clipboard type
    formatEtc.cfFormat := CF_IDLIST;
    formatEtc.ptd := nil;
    formatEtc.dwAspect := DVASPECT_CONTENT;
    formatEtc.lindex := -1;
    formatEtc.tymed := TYMED_HGLOBAL;

    if Failed( dataobj.QueryGetData( formatEtc ) ) then
      Exit;
      // Some IDataObject implementations barf on the following GetData if they have previously
      // said they don't support the CF_IDLIST format. Double-check here to prevent that.

    if Failed( dataobj.GetData( formatEtc, stgMedium ) ) then
      Exit;
    pData := GlobalLock( stgMedium.hGlobal );
    if not Assigned( pData ) then
      Exit;

   // Get the parentpidl from the CF_IDLIST structure
    numElements := PIDA( pData )^.cidl;
    curp := UINT( pData ) + Sizeof( DWORD ); // Start at offset of folder
    parentidl := Pointer( UINT( pData ) + PDWORD( curp )^ );
    Inc( curp, sizeof( DWORD ) );

   // Create the list of PItemIdLists for the children
    for I := 0 to numElements - 1 do
    begin
      childList.Add( Pointer( UINT( pdata ) + PDWORD( curp )^ ) );
      dwa.Add( Pointer( DRAGFLAGS ) );
      Inc( curp, sizeof( DWORD ) );
    end;

   // Open the parent folder
    OleCheck( ShellGetFolderFromIdList( parentidl, iparentshf ) );

    p := PPItemIdList( childList.List )^;
    attr := PDWORD( dwa.List )^;
    OleCheck( iparentshf.GetAttributesOf( numElements, p, attr ) );
    for I := 0 to dwa.Count - 1 do
      Result := Result and Integer( dwa[ I ] );

   // Free everything
  finally
    if Assigned( iparentshf ) then
      iparentshf.Release;
    if Assigned( pdata ) then
      GlobalUnlock( stgMedium.hGlobal );
    if Assigned( childList ) then
      childList.Free;
    if Assigned( dwa ) then
      dwa.Free;
    ReleaseStgMedium( stgMedium );
  end;
end;                                        {GetDragDropAttributesForClipboardObjects}


{ There are some IShellFolder functions you just can't do without going via an item's parent. This is a problem
  since the desktop has no parent. The trick here is to not use the Desktop's 'root of the namespace' personality,
  but to use the 'c:\windows\desktop' (or whatever) version. This function returns the parent (c:\windows) and
  relative (desktop) pidls. }

function GetDesktopParentChildPidls( var aParentAbsPidl, aChildRelPidl: PItemIdList ): HResult;
var
  tmppidl: PItemIdList;
  idla: TRzIdListArray;
begin
  Result := S_OK;
  aParentAbsPidl := nil;
  aChildRelPidl := nil;
  idla := nil;
  tmppidl := nil;
  try
    ShellGetIdListFromPath( ShellGetSpecialFolderPath( 0, csidlDesktop ), tmppidl );
    idla := TRzIdListArray.Create( tmppidl );
    aParentAbsPidl := CopyIdList( nil, idla.GoUp( 1 ) );
    aChildRelPidl := CopyIdList( nil, idla[ idla.ItemCount - 1 ] );
  finally
    ShellMemFree( tmppidl );
    if Failed( Result ) then
    begin
      ShellMemFree( aParentAbsPidl );
      aParentAbsPidl := nil;
      ShellMemFree( aChildRelPidl );
      aChildRelPidl := nil;
    end;
    idla.Free;
  end;
end;                                        {GetDesktopParentChildPidls}



{-- Methods to handle context menu events WM_InitPopup, WM_DrawItem, WM_MeasureItem, WM_MenuChar --}
var
  _sl: TStringList = nil;

procedure SendTo_WMInitMenuPopup( var aMsg: TWMInitMenuPopup; activeContextMenu: IContextMenu_NRC; var items: TStringList );
  procedure FindTheseFiles( path: string; extensions: array of string );
  var
    I: Integer;
    iconidx: Integer;
    fh: THandle;
    w32fd: TWin32FindData;
    abspidl: PItemIdList;
  begin
    for I := Low( extensions ) to High( extensions ) do
    begin
      fh := Windows.FindFirstFile( PChar( path + '\' + extensions[ I ] ), w32fd );
      if ( fh <> INVALID_HANDLE_VALUE ) then
      begin
        repeat
          if ( ( w32fd.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY ) = 0 ) and
            ( ( w32fd.dwFileAttributes and FILE_ATTRIBUTE_HIDDEN ) = 0 ) then
          begin
            abspidl := nil;
            if Failed( RzShellUtils.ShellGetIdListFromPath( path + '\' + w32fd.cFileName, abspidl ) ) then
              iconidx := -1
            else
              iconidx := ShellGetIconIndex( abspidl, SHGFI_SMALLICON );
            RzShellUtils.ShellMemFree( abspidl );
            items.AddObject( w32fd.cFileName, TObject( iconidx ) );
          end;
        until not Windows.FindNextFile( fh, w32fd );
        Windows.FindClose( fh );
      end;
    end;
  end;
var
  u: UINT;
  sendToPath: string;
  I, curid: Integer;
  icm2: IContextMenu2_NRC;
begin                                       {SendTo_WMInitPopup}
  if Assigned( activeContextMenu ) then
  begin
    try
      if activeContextMenu.QueryInterface( IID_IContextMenu2, icm2 ) = S_OK then
      begin
//        I := GetMenuItemCount(aMsg.menuPopup);
        try
          icm2.HandleMenuMsg( aMsg.Msg, TMessage( aMsg ).wParam, TMessage( aMsg ).lParam );
{         if GetMenuItemCount(aMsg.menuPopup) <> I then
          begin
            while DeleteMenu(aMsg.menuPopup, 0, MF_BYPOSITION) do ;
            AppendMenu( aMsg.menuPopup, MF_STRING or MF_GRAYED, 99, 'xxx' );
          end;}
        finally
          icm2.Release;
        end;
      end
      else
      begin
        u := GetMenuState( aMsg.menuPopup, 0, MF_BYPOSITION );
        if ( GetMenuItemCount( aMsg.menuPopup ) = 1 ) and ( u <> UINT( $FFFFFFFF ) ) and ( ( u and MF_GRAYED ) <> 0 ) then
        begin                               // We can be pretty sure the SendTo hasn't been filled in yet.
          if not Assigned( items ) then
          begin
            items := TStringList.Create;
            items.Sorted := True;
          end;
          DeleteMenu( aMsg.menuPopup, 0, MF_BYPOSITION );
          sendToPath := RzShellUtils.ShellGetSpecialFolderPath( 0, csidlSendTo );

          FindTheseFiles( sendToPath, [ '*' ] );

          curid := 1;
          for I := 0 to items.Count - 1 do
          begin
            AppendMenu( aMsg.menuPopup, MF_STRING or MF_OWNERDRAW, curid, PChar( I ) );
            Inc( curid );
          end;
        end;
      end;
    except
    end;
  end;
end;                                        {SendTo_WMInitMenuPopup}


procedure SendTo_WMDrawItem( var aMsg: TWMDrawItem; activeContextMenu: IContextMenu_NRC; images: TCustomImageList; items: TStrings );
var
  State: TOwnerDrawState;
  canvas: TCanvas;
  ncm: TNonClientMetrics;
  idx: Integer;
  s: string;

  icm2: IContextMenu2_NRC;
begin
  if Assigned( activeContextMenu ) then     // Doing this makes extensions that hook IContextMenu2 for their own drawings work.
  try
    if activeContextMenu.QueryInterface( IID_IContextMenu2, icm2 ) = S_OK then
    begin
      try
        icm2.HandleMenuMsg( aMsg.Msg, TMessage( aMsg ).wParam, TMessage( aMsg ).lParam );
      finally
        icm2.Release;
      end;
    end
    else
    begin
      if ( aMsg.drawItemStruct.ctlType = ODT_MENU ) and
        Assigned( items ) and
        ( aMsg.drawItemStruct.itemData < CMDOFFSET ) then
      begin
        State := TOwnerDrawState( LongRec( aMsg.drawItemStruct.itemState ).Lo );

        canvas := TCanvas.Create;
        canvas.Handle := aMsg.drawItemStruct.hDC;

        ZeroMemory( @ncm, Sizeof( ncm ) );
        ncm.cbSize := SizeOf( TNonClientMetrics );
        SystemParametersInfo( SPI_GETNONCLIENTMETRICS, 0, @ncm, 0 );

        canvas.Font.Handle := CreateFontIndirect( ncm.lfMenuFont );

        if ( Integer( aMsg.drawItemStruct.itemID ) >= 0 ) and ( odSelected in State ) then
        begin
          canvas.Brush.Color := clHighlight;
          canvas.Font.Color := clHighlightText;
        end
        else
        begin
          canvas.Brush.Color := clMenu;
          canvas.Font.Color := clMenuText;
        end;

        if Integer( aMsg.drawItemStruct.itemID ) >= 0 then
        begin
          s := items[ aMsg.drawItemStruct.itemData ];
          canvas.TextRect( aMsg.drawItemStruct.rcItem,
            aMsg.drawItemStruct.rcItem.left + 32, aMsg.drawItemStruct.rcItem.top + 2,
            Copy( s, 1, Length( s ) - Length( ExtractFileExt( s ) ) ) );

          idx := Integer( items.Objects[ aMsg.drawItemStruct.itemData ] );
          if idx >= 0 then
          begin
            ImageList_Draw( images.Handle, idx, canvas.Handle,
              aMsg.drawItemStruct.rcItem.left + 8, aMsg.drawItemStruct.rcItem.top + 2,
              ILD_TRANSPARENT );
          end;
        end
        else
          canvas.FillRect( aMsg.drawItemStruct.rcItem );
        canvas.Free;
      end;
    end;
  except
  end;
end;                                        {SendTo_WMDrawItem}


procedure SendTo_WMMenuChar( var aMsg: TWMMenuChar; activeContextMenu: IContextMenu_NRC; items: TStrings );
  function GetSelectedMenuItemPos( h: HMENU ): Integer;
  var
    mii: TMenuItemInfo;
    I, max: Integer;
  begin
    mii.cbSize := SizeOf( TMenuItemInfo );
    mii.fMask := MIIM_STATE;
    max := GetMenuItemCount( h );
    for I := 0 to max - 1 do
    begin
      if GetMenuItemInfo( h, I, True, mii ) then
      begin
        if ( ( mii.fState and MF_HILITE ) <> 0 ) then
        begin
          Result := I;
          Exit;
        end;
      end;
    end;
    Result := -1;
  end;

  function MoreThanOneWithChar( c: Char ): Bool;
  var
    I: Integer;
    count: Integer;
  begin
    count := 0;
    for I := 0 to items.Count - 1 do
      if items[ I ][ 1 ] = c then
        Inc( count );
    Result := ( count > 1 );
  end;

  function MakeResult( id: Integer ): Integer;
  begin
    if MoreThanOneWithChar( aMsg.user ) then
      Result := MakeLResult( id, MNC_SELECT )
    else
      Result := MakeLResult( id, MNC_EXECUTE );
  end;
var
  selitem, I: Integer;
  icm2: IContextMenu2_NRC;
begin
  if Assigned( activeContextMenu ) then
  begin
    if activeContextMenu.QueryInterface( IID_IContextMenu2, icm2 ) = S_OK then
    begin
      try
        icm2.HandleMenuMsg( aMsg.Msg, TMessage( aMsg ).wParam, TMessage( aMsg ).lParam );
      finally
        icm2.Release;
      end;
    end
    else
    begin
      if Assigned( items ) and ( aMsg.menuFlag = MF_POPUP ) then
      begin
        aMsg.user := Upcase( aMsg.user );
        selitem := GetSelectedMenuItemPos( aMsg.menu );
        if ( selitem < 0 ) then
          selitem := 0;
        for I := selitem + 1 to items.Count - 1 do
        begin
          if ( Upcase( items[ I ][ 1 ] ) = aMsg.user ) then
          begin
            aMsg.Result := MakeResult( I );
            Exit;
          end
        end;
        for I := 0 to selitem do
        begin
          if ( Upcase( items[ I ][ 1 ] ) = aMsg.user ) then
          begin
            aMsg.Result := MakeResult( I );
            Exit;
          end;
        end;
        aMsg.Result := 1;                   //Beep
      end;
    end;
  end;
end;                                        {SendTo_WMMenuChar}


procedure SendTo_WMMeasureItem( var aMsg: TWMMeasureItem; activeContextMenu: IContextMenu_NRC; items: TStrings );
var
  canvas: TCanvas;
  ncm: TNonClientMetrics;
  dc: HDC;
  s: string;

  icm2: IContextMenu2_NRC;
begin
  if Assigned( activeContextMenu ) then     // Doing this makes extensions that hook IContextMenu2 for their own drawings work.
  try
    if activeContextMenu.QueryInterface( IID_IContextMenu2, icm2 ) = S_OK then
    begin
      try
        icm2.HandleMenuMsg( aMsg.Msg, TMessage( aMsg ).wParam, TMessage( aMsg ).lParam );
      finally
        icm2.Release;
      end;
    end
    else
    begin
      if ( aMsg.measureItemStruct.ctlType = ODT_MENU ) and
        Assigned( items ) and
        ( aMsg.measureItemStruct.itemData < CMDOFFSET ) then
      begin
        canvas := TCanvas.Create;
        dc := GetDC( 0 );
        Canvas.Handle := dc;

        ZeroMemory( @ncm, Sizeof( ncm ) );
        ncm.cbSize := SizeOf( TNonClientMetrics );
        SystemParametersInfo( SPI_GETNONCLIENTMETRICS, 0, @ncm, 0 );

        s := items[ aMsg.measureItemStruct.itemData ];
        aMsg.measureItemStruct.itemWidth :=
          Canvas.TextWidth( Copy( s, 1, Length( s ) - Length( ExtractFileExt( s ) ) ) ) + 32;

        aMsg.measureItemStruct.itemHeight := GetSystemMetrics( SM_CYMENU ) + 2;

        canvas.Handle := 0;
        ReleaseDC( 0, dc );
        canvas.Free;
      end;
    end;
  except
  end;
end;                                        {SendTo_WMMeasureItem}

procedure Popup_WMMenuSelect( var aMsg: TWMMenuSelect;
  activeContextMenu: IContextMenu_NRC;
  aSelf: TObject;
  aPopupProc: TRzShPopupHintEvent );
var
  c: array[ 0..MAX_PATH ] of AnsiChar;
  s: string;
begin
  if Assigned( activeContextMenu ) and Assigned( aPopupProc ) then
  begin
    c[ 0 ] := #0;
    if ( aMsg.idItem >= CMDOFFSET ) and ( ( aMsg.menuFlag and ( MF_POPUP or MF_SYSMENU ) ) = 0 ) and
      not Failed( activeContextMenu.GetCommandString( aMsg.idItem - CMDOFFSET, GCS_HELPTEXT, nil, c, High( c ) ) ) then
    begin
      s := string( c );
      aPopupProc( aSelf, s );
    end
    else
      aPopupProc( aSelf, '' );
  end;
end;                                        {Popup_WMMenuSelect}


{function GetShortPathName( longPathName: String ): String;
var sz: array[0..MAX_PATH] of Char;
begin
  Windows.GetShortPathName( PChar(longPathName), sz, Sizeof(sz) );
  Result := sz;
end;}

function GetValidParentHWND( c: TControl ): HWND;
var
  f: TForm;
begin
  while c.Parent <> nil do
    c := c.Parent;

  if c is TForm then
    f := TForm( c )
  else
    f := nil;

  if f <> nil then
    Result := f.Handle
  else
    Result := 0;
end;                                        {GetValidParentHWND}


{ Looks up 'item' in the SendTo folder and simulates a 'drop' operation on that item. }

procedure ProcessSendTo_via_drop( item: string; data: IDataObject_NRC );
var
  idt: IDropTarget_NRC;
  sendtopidl: PItemIdList;
  itempidl: PItemIdList;
  ishfsendto: IShellFolder_NRC;
  dropeffect: Longint;
  oldCursor: TCursor;
  chEaten, attrib: Cardinal;
  wca: array[ 0..MAX_PATH ] of WideChar;
begin
  idt := nil;
  ishfsendto := nil;
  sendtopidl := nil;
  itempidl := nil;
  try
    OleCheck( ShellGetSpecialFolderIdList( 0, csidlSendTo, sendtopidl ) );
    OleCheck( ShellGetFolderFromIdList( sendtopidl, ishfsendto ) );
    StringToWideChar( item, @wca[ 0 ], High( wca ) );
    OleCheck( ishfsendto.ParseDisplayName( 0, nil, @wca[ 0 ], chEaten, itempidl, attrib ) );
    OleCheck( ishfsendto.GetUIObjectOf( 0, 1, itempidl, IID_IDropTarget, nil, Pointer( idt ) ) );
    dropeffect := DROPEFFECT_COPY;
    idt.DragEnter( data, MK_LBUTTON, Point( 0, 0 ), dropeffect );
    idt.DragOver( MK_LBUTTON, Point( 0, 0 ), dropeffect );
    oldCursor := Screen.Cursor;
    Screen.Cursor := crHourglass;
    try
      idt.Drop( data, MK_LBUTTON, Point( 0, 0 ), dropeffect );
    finally
      Screen.Cursor := oldCursor;
    end;
  finally
    if Assigned( idt ) then
      idt.Release;
    if Assigned( ishfsendto ) then
      ishfsendto.Release;
    ShellMemFree( sendtopidl );
    ShellMemFree( itempidl );
  end;
end;                                        {ProcessSendTo_via_drop}

procedure SleepWait( AMSecs: DWORD );
var
  c: TCursor;
begin
  c := Screen.Cursor;
  Screen.Cursor := crHourglass;
  try
    Sleep( AMSecs );
  finally
    Screen.Cursor := c;
  end;
end;
{== END Local Procedures =================================}


{**************************************
  TRzShellTreeData
**************************************}

constructor TRzShellTreeData.Create( aOwner: TRzCustomShellTree );
begin
  inherited Create;
  FOwner := aOwner;
  {$IFDEF PTDEBUG}
  Inc( FOwner.mdbgNodes );
  Inc( g_TreeNodes );
  {$ENDIF}
end;

destructor TRzShellTreeData.Destroy;
begin
  if Assigned( FThisIshf ) then
    FThisIshf.Release;
  if Assigned( FParentIShf ) then
  begin
    FParentIShf.Release;
    FParentIShf := nil;
  end;

  ShellMemFree( FRelPidl );
  ShellMemFree( FAbsPidl );
  {$IFDEF PTDEBUG}
  Dec( FOwner.mdbgNodes );
  Dec( g_TreeNodes );
  {$ENDIF}
  inherited;
end;                                        {TRzShellTreeData.Destroy}

function TRzShellTreeData.GetAbsPidl: PItemIdList;
begin
  if not Assigned( FAbsPidl ) then
    if Assigned( Parent ) then
      FAbsPidl := ConcatIdLists( nil, Parent.AbsoluteIdList, RelativeIdList )
    else
      FAbsPidl := CopyIdList( nil, RelativeIdList );
  Result := FAbsPidl;
end;                                        {TRzShellTreeData.GetAbsPidl}


function TRzShellTreeData.GetAttributes: DWORD;
var
  s: string;
  p: PItemIdList;
  uCode: DWord;
begin
  uCode := SetErrorMode( sem_FailCriticalErrors );
  try
    Result := SFGAO_HASSUBFOLDER or SFGAO_FOLDER or SFGAO_DISPLAYATTRMASK or
              SFGAO_FILESYSTEM or SFGAO_FILESYSANCESTOR or SFGAO_CANRENAME or
              SFGAO_CANCOPY or SFGAO_CANMOVE or SFGAO_CANLINK or SFGAO_CANDELETE;

    if SHELL32_VER.version >= SHELL32_VER60 then
      Result := Result or SFGAO_STREAM;

    // Work around a problem in IE4 where if SFGAO_READONLY is specified for a floppy then the floppy is accessed.
    s := PathName;
    if IsPathRoot( s ) and ( GetDriveType( PChar( s ) ) = DRIVE_REMOVABLE ) then
      Result := Result and not SFGAO_READONLY;

    p := RelativeIdList;
    ParentIShf.GetAttributesOf( 1, p, Result );
  finally
    SetErrorMode( uCode );
  end;
end;


function TRzShellTreeData.GetEditable: Boolean;
var
  dw, attr: DWORD;
  p: PItemIdList;
begin
  if not Assigned( AbsoluteIdList ) or ( PWord( AbsoluteIdList )^ = 0 ) or not Assigned( Parent ) then
    Result := False                         // Can't edit desktop
  else
  begin
    p := RelativeIdList;
    attr := SFGAO_CAPABILITYMASK or SFGAO_FILESYSTEM;
    dw := ParentIShf.GetAttributesOf( 1, p, attr );
    if Succeeded( dw ) then
      Result := ( attr and SFGAO_CANRENAME ) <> 0
    else
      Result := False;
  end;
end;                                        {TRzShellTreeData.GetEditable}

function TRzShellTreeData.GetPathName: string;
begin
  SetLength( Result, MAX_PATH );
  if SHGetPathFromIDList( AbsoluteIdList, PChar( Result ) ) then
    SetLength( Result, StrLen( PChar( Result ) ) )
  else
    Result := '';
end;                                        {TRzShellTreeData.GetPathName}

function TRzShellTreeData.GetParentIShf: IShellFolder_NRC;
var
  ParentIdList, RelativeIdList: PItemIdList;
begin
  if Assigned( FParent ) then
    Result := FParent.ThisIshf
  else
  begin
    if Assigned( FParentIShf ) then
    begin
      FParentIShf.Release;
      FParentIShf := nil;
    end;

    if Succeeded( GetIShellFolderParent( AbsoluteIdList, FParentIShf, ParentIdList, RelativeIdList ) ) then
    begin
      ShellMemFree( ParentIdList );
      ShellMemFree( RelativeIdList );
    end;

    Result := FParentIShf;
  end;
end;

function TRzShellTreeData.GetThisIShf: IShellFolder_NRC;
begin
  if not Assigned( FThisIshf ) then
  begin
    if Assigned( AbsoluteIdList ) then
    begin
      if Failed( ParentIShf.BindToObject( RelativeIdList, nil, IID_IShellFolder, Pointer( FThisIshf ) ) ) then
        FThisIshf := nil;
    end
    else
      OleCheck( ShellGetDesktopFolder( FThisIshf ) );
  end;
  Result := FThisIshf;
end; {= TRzShellTreeData.GetThisIShf =}


procedure TRzShellTreeData.SetData( aParent: TRzShellTreeData; thisRelativePidl: PItemIdlist );
var
  idlistarray: TRzIdListArray;
begin
  FParent := aParent;
  if Assigned(aParent) then
  begin
    FRelPidl := CopyIdList( nil, thisRelativePidl );
    FAbsPidl := nil;
  end
  else
  begin
    idlistarray := TRzIdListArray.Create( thisrelativePidl );
    try
      if idlistarray.ItemCount > 0 then
        FRelPidl := CopyIdList( nil, idlistarray.Item[ idlistarray.ItemCount - 1 ] )
      else
        FRelPidl := CopyIdList( nil, thisRelativePidl );
      FAbsPidl := CopyIdList( nil, thisRelativePidl );
    finally
      idlistarray.Free;
    end;
  end;
end; {= TRzShellTreeData.SetData =}


procedure TRzShellTreeData.SetRelPidl( newRelPidl: PItemIdList ); // Called after in-place editing
begin
  ShellMemFree( FAbsPidl );
  FAbsPidl := nil;
  ShellMemFree( FRelPidl );
  FRelPidl := nil;
  if Assigned( FThisIshf ) then
  begin
    FThisIshf.Release;
    FThisIshf := nil;
  end;
  FRelPidl := CopyIdList( nil, newRelPidl );
  if Assigned( FParent ) then
    FAbsPidl := ConcatIdLists( nil, Parent.AbsoluteIdList, FRelPidl )
  else
    FAbsPidl := CopyIdList( nil, FRelPidl );
end;                                        {SetRelPidl}

procedure TRzShellTreeData.Flush;
begin
  ShellMemFree( FAbsPidl );
  FAbsPidl := nil;
  if Assigned( FThisIshf ) then
  begin
    FThisIshf.Release;
    FThisIshf := nil;
  end;
end;

function TRzShellTreeData.GetIDropTarget( h: HWND; var idt: IDropTarget_NRC ): HResult;
var
  pidl: PItemIdList;
begin
  if Assigned( FParent ) then
  begin
    pidl := RelativeIdList;
    Result := FParent.ThisIShf.GetUIObjectOf( h, 1, pidl, IID_IDropTarget, nil, Pointer( idt ) );
  end
  else if not Assigned( AbsoluteIdList ) then
    Result := GetDesktopIDT( idt )
  else
    Result := IShf_GetUIObjectOf( AbsoluteIdList, IID_IDropTarget, Pointer( idt ) );
end;

function TRzShellTreeData.IsRootDir: Boolean;
var
  s: string;
begin
  s := PathName;
  Result := ( Length( s ) >= 2 ) and ( Length( s ) <= 3 ) and not IsDBCSLeadByte( Byte( s[ 1 ] ) ) and
    ( s[ 2 ] = ':' );
end;

{== END TRzShellTreeData ======================================}



{**************************************
  TRzShellListData
**************************************}

constructor TRzShellListData.Create( aOwner: TRzCustomShellList );
begin
  inherited Create;
  FOwner := aOwner;
  {$IFDEF PTDEBUG}
  Inc( FOwner.mdbgNodes );
  Inc( g_ListNodes );
  {$ENDIF}
end;                                        {TRzShellListData.Create}

destructor TRzShellListData.Destroy;
begin
  ShellMemFree( FRelPidl );
  ShellMemFree( FAbsPidl );
  {$IFDEF PTDEBUG}
  Dec( FOwner.mdbgNodes );
  Dec( g_ListNodes );
  {$ENDIF}
  inherited;
end;                                        {TRzShellListData.Destroy}

function TRzShellListData.IsFolder: Boolean;
begin
  Result := RzShIsFolder( Attributes, sloFilesCanBeFolders in FOwner.Options );
end;

function TRzShellListData.IsLnkShortcut: Boolean;
begin                                       // We're not interested in non .lnk file shortcuts
  Result := AnsiCompareText( ExtractFileExt( Pathname ), '.lnk' ) = 0;
end;

function TRzShellListData.IsValid: Boolean;
var
  attrib: DWORD;
begin
  attrib := SFGAO_VALIDATE;
  Result := Succeeded( FOwner.FIShf.GetAttributesOf( 1, FRelPidl, attrib ) );
end;                                        {TRzShellListData.IsValid}

function TRzShellListData.IsFileSystem: Boolean;
begin
  Result := ( Attributes and SFGAO_FILESYSTEM ) <> 0;
end;                                        {TRzShellListData.IsFileSystem}

function TRzShellListData.GetAbsoluteIdList: PItemIdList;
begin
  if not Assigned( FAbsPidl ) then
    FAbsPidl := ConcatIdLists( nil, FOwner.FIShfPidl, FRelPidl );
  Result := FAbsPidl;
end;                                        {TRzShellListData.GetAbsoluteIdList}

function TRzShellListData.GetAttributes: DWORD;
var
  s: string;
  uCode: DWord;
begin
  uCode := SetErrorMode( sem_FailCriticalErrors );
  try
    Result := SFGAO_FOLDER or SFGAO_DISPLAYATTRMASK or
              SFGAO_FILESYSTEM or SFGAO_FILESYSANCESTOR or SFGAO_CANRENAME or
              SFGAO_CANCOPY or SFGAO_CANMOVE or SFGAO_CANLINK or SFGAO_CANDELETE;

    if SHELL32_VER.version >= SHELL32_VER60 then
      Result := Result or SFGAO_STREAM;

    // Work around a problem in IE4 where if SFGAO_READONLY is specified for a floppy then the floppy is accessed.
    s := PathName;
    if IsPathRoot( s ) and ( GetDriveType( PChar( s ) ) = DRIVE_REMOVABLE ) then
      Result := Result and not SFGAO_READONLY;

    FOwner.FIShf.GetAttributesOf( 1, FRelPidl, Result );
  finally
    SetErrorMode( uCode );
  end;
end;                                        {TRzShellListData.GetAttributes}

function TRzShellListData.GetEditable: Boolean;
begin
  Result := ( Attributes and SFGAO_CANRENAME ) <> 0;
end;


function TRzShellListData.GetDisplayName: String;
begin
  if ( FDisplayName = '' ) then
    FDisplayName := ShellGetFriendlyNameFromIdList( FOwner.FIShf, RelativeIdList, fnNormal );
  Result := FDisplayName;
end; {TPTShListData.GetDisplayName}


function TRzShellListData.GetFilename: string;
begin
  Result := ExtractFileName( PathName );
end;

function TRzShellListData.GetPathName: string;
begin
  Result := ShellGetPathFromIDList( AbsoluteIdList );
end;                                        {TRzShellListData.GetPathName}

{The TRzShellListData object takes ownership of the id list}

procedure TRzShellListData.SetData( aRelPidl: PItemIdList );
begin
  ShellMemFree( FRelPidl );
  FRelPidl := aRelPidl;
  Flush;
end;

procedure TRzShellListData.Flush;
begin
  if Assigned( FAbsPidl ) then
  begin
    ShellMemFree( FAbsPidl );
    FAbsPidl := nil;
  end;
  FDisplayName := '';
  FDataValid := False;
end;

function TRzShellListData.GetColText( col: Integer ): string;
var
  ishd: IShellDetails_NRC;
  info: TShColInfo;
  uCode: DWord;
begin
  if ( col = 0 ) then
    Result := ''                            // Use GetDisplayName for column 0
  else
  begin
    ishd := FOwner.GetCurrentFolderIShellDetails;
    if Assigned( ishd ) then
    begin
      ZeroMemory( @info, Sizeof( info ) );
      uCode := SetErrorMode(sem_FailCriticalErrors );
      try
        info.text.uType := STRRET_WSTR;
        if Succeeded( ishd.GetDetailsOf( FRelPidl, col, info ) ) then
        begin
          Result := StrretToString( FRelPidl, info.text );
          StrretFree( info.text );
        end
        else
          Result := '';
      finally
        SetErrorMode( uCode );
      end;
    end
    else
    begin
      if not FDataValid then
        GetExtraData;
      case col of
        1: Result := FSize;
        2: Result := FType;
        3: Result := FModified;
      else
        Result := '';
      end;
    end;
  end;
end; {= TRzShellListData.GetColText =}


procedure TRzShellListData.GetExtraData;
begin
  if not FDataValid then
  begin
    {local.} GetFileStrings2( AbsoluteIdList, Attributes, FSize, FType, FModified );
    FDataValid := True;
  end;
end;

function TRzShellListData.GetSize: string;
begin
  GetExtraData;
  Result := FSize;
end;

function TRzShellListData.GetFileType: string;
begin
  GetExtraData;
  Result := FType;
end;

function TRzShellListData.GetModified: string;
begin
  GetExtraData;
  Result := FModified;
end;
{== END TRzShellListData ======================================}



{-- TRzChangeHandlerThread -------------------------------}

constructor TRzChangeHandlerThread.Create( OwnerWnd: HWND; ChangeMsgId: UINT );
begin
  {$IFDEF PTDEBUG}
  Inc( g_ChangeHandlerThread );
  {$ENDIF}
  inherited Create( True );

  FDoActionEvent := Windows.CreateEvent( nil, False, False, nil );
  FActionProcessedEvent := Windows.CreateEvent( nil, False, False, nil );
  FOwnerWnd := OwnerWnd;
  FChangeMsgId := ChangeMsgId;
//  Priority := tpIdle; -- Since we communicate with this thread synchronously, making it idle just slows down the UI thread
  FChangeHandlerList := TList.Create;
  FChangeHandlerList.Capacity := MAXIMUM_WAIT_OBJECTS;
  while FChangeHandlerList.Count < MAXIMUM_WAIT_OBJECTS - 1 do
    FChangeHandlerList.Add( Pointer( INVALID_HANDLE_VALUE ) );
  FreeOnTerminate := False;

  {$WARN SYMBOL_DEPRECATED OFF}
  Resume;
  {$WARN SYMBOL_DEPRECATED ON}
end;                                        {TChangeHandlerThread.Create}


destructor TRzChangeHandlerThread.Destroy;
var
  h1, h2: THandle;
  ch: TList;
  I: Integer;
begin
  {$IFDEF PTDEBUG}
  Dec( g_ChangeHandlerThread );
  {$ENDIF}

  FThreadAction := taTerminate;
  Windows.SetEvent( FDoActionEvent );
  Windows.WaitForSingleObject( Handle, 30000 ); // Wait for thread to exit

  h1 := FDoActionEvent;
  h2 := FActionProcessedEvent;
  ch := TList.Create;
  for I := 0 to FChangeHandlerList.Count - 1 do
    if Integer( FChangeHandlerList[ I ] ) <> Integer( INVALID_HANDLE_VALUE ) then
      ch.Add( FChangeHandlerList[ I ] );
  if FPendingChangeHandler <> INVALID_HANDLE_VALUE then
    ch.Add( Pointer( FPendingChangeHandler ) );
  FChangeHandlerList.Free;
  inherited;
  CloseHandle( h1 );
  CloseHandle( h2 );
  for I := 0 to ch.Count - 1 do
    FindCloseChangeNotification( Integer( ch[ I ] ) );
  ch.Free;
end;                                        {TChangeHandlerThread.Destroy}


procedure TRzChangeHandlerThread.Execute;
  procedure DoAddNewHandler;
  var
    I: Integer;
  begin
    for I := 0 to FChangeHandlerList.Count - 1 do
      if Integer( FChangeHandlerList[ I ] ) = Integer( INVALID_HANDLE_VALUE ) then
      begin
        FNewHandlerIndex := I;
        FChangeHandlerList[ I ] := Pointer( FPendingChangeHandler );
        FPendingChangeHandler := INVALID_HANDLE_VALUE;
        Exit;
      end;
    FNewHandlerIndex := -1;
  end;                                      {DoAddNewHandler - local}

  procedure DoRemoveHandler( handlerIndex: Integer );
  begin
    Windows.FindCloseChangeNotification( Integer( FChangeHandlerList[ handlerIndex ] ) );
    FChangeHandlerList[ handlerIndex ] := Pointer( INVALID_HANDLE_VALUE );
  end;                                      {DoRemoveHandler - local}

  procedure DoRemoveAll;
  var
    I: Integer;
  begin
    for I := 0 to FChangeHandlerList.Count - 1 do
      if Integer( FChangeHandlerList[ I ] ) <> Integer( INVALID_HANDLE_VALUE ) then
      begin
        Windows.FindCloseChangeNotification( Integer( FChangeHandlerList[ I ] ) );
        FChangeHandlerList[ I ] := Pointer( INVALID_HANDLE_VALUE );
      end;
  end;                                      {DoRemoveAll - local}

  procedure ChangeDetected( handlerIndex: Integer );
  begin
    if ( handlerIndex < 0 ) or ( handlerIndex >= FChangeHandlerList.Count ) then
      Exit;
    if ( FOwnerWnd <> 0 ) then
      PostMessage( FOwnerWnd, FChangeMsgId, handlerIndex, 0 );
    if not Windows.FindNextChangeNotification( Integer( FChangeHandlerList[ handlerIndex ] ) ) then
      DoRemoveHandler( handlerIndex );
  end;                                      {ChangeDetected - local}

var
  dw: DWORD;

  obja: TWOHandleArray;
  indexa: array[ 0..MAXIMUM_WAIT_OBJECTS - 1 ] of Integer;

  I: Integer;
  j: DWORD;
begin
  repeat
    obja[ 0 ] := FDoActionEvent;
    j := 1;
    for I := 0 to FChangeHandlerList.Count - 1 do
      if Integer( FChangeHandlerList[ I ] ) <> Integer( INVALID_HANDLE_VALUE ) then
      begin
        obja[ j ] := Integer( FChangeHandlerList[ I ] );
        indexa[ j ] := I;
        Inc( j );
      end;
    dw := Windows.WaitForMultipleObjects( j, @obja, False, INFINITE );
    if FTerminateEx then
      Break;
    if ( dw = WAIT_OBJECT_0 ) then
    begin
      case FThreadAction of
        taAddNewHandler: DoAddNewHandler;
        taRemoveHandler: DoRemoveHandler( FPendingRemoveIndex );
        taRemoveAllMonitors: DoRemoveAll;
        taTerminate: Break;
      end;
      Windows.SetEvent( FActionProcessedEvent );
    end
    else if ( dw > WAIT_OBJECT_0 ) and ( dw <= WAIT_OBJECT_0 + j ) then
      ChangeDetected( indexa[ dw - WAIT_OBJECT_0 ] )
    else if ( dw > WAIT_ABANDONED_0 ) and ( dw <= WAIT_ABANDONED_0 + j ) then
      DoRemoveHandler( indexa[ dw - WAIT_ABANDONED ] );
  until False;
end;                                        {TChangeHandlerThread.Execute}


// If param 2 is declared LongBool as in Windows.pas, then in Delphi 3 it takes on the value 0=False, -1=True. If you
// pass -1 as param 2 - while technically a valid value of True - this func. returns "Invalid parameter".
// I guess MS slipped up - I bet the code inside Windows looks like: if (param2 == True) ... which would be fine in
// Delphi, but in C/C++ no way!

function _FindFirstChangeNotification( p: PAnsiChar; I: Integer; f: DWORD ): THandle; stdcall; external 'kernel32.dll' name 'FindFirstChangeNotificationA';


{ If successful, returns a token in 'apToken'. You should pass this token to RemoveMonitorDir when you wish to
  stop monitoring the directory. }

function TRzChangeHandlerThread.AddMonitorDir( Pathname: string; Flags: DWORD; Recursive: Boolean; var Token: Pointer ): Boolean;
var
  PathStr: AnsiString;
const
  lookup: array[ Boolean ] of Integer = ( 0, 1 );
begin
  if Pathname = '' then
  begin
    Result := False;
    Exit;
  end;
  PathStr := AnsiString( Pathname );

  FPendingChangeHandler := _FindFirstChangeNotification( PAnsiChar( PathStr ), lookup[ Recursive ], Flags );

  if ( FPendingChangeHandler <> INVALID_HANDLE_VALUE ) then
  begin
    FThreadAction := taAddNewHandler;
    Windows.ResetEvent( FActionProcessedEvent );
    Windows.SetEvent( FDoActionEvent );
    if Windows.WaitForSingleObject( FActionProcessedEvent, 2000 ) = WAIT_OBJECT_0 then
    begin
      Token := Pointer( FNewHandlerIndex );
      Result := ( Integer( Token ) <> -1 );
    end
    else
      Result := False;
  end
  else
    Result := False;
end;                                        {TChangeHandlerThread.AddMonitorDir}

procedure TRzChangeHandlerThread.RemoveAllMonitors;
begin
  FThreadAction := taRemoveAllMonitors;
  Windows.ResetEvent( FActionProcessedEvent );
  Windows.SetEvent( FDoActionEvent );
  Windows.WaitForSingleObject( FActionProcessedEvent, 2000 );
end;                                        {TChangeHandlerThread.RemoveAllMonitors}

{== END Thread support =====================================}


{**************************************
  TRzShellComboData
**************************************}

constructor TRzShellComboData.Create( aOwner: TRzCustomShellCombo );
begin
  inherited Create;
  FOwner := aOwner;
  {$IFDEF PTDEBUG}
  Inc( FOwner.mdbgNodes );
  Inc( g_ComboNodes );
  {$ENDIF}
end;

destructor TRzShellComboData.Destroy;
begin
  {$IFDEF PTDEBUG}
  Dec( FOwner.mdbgNodes );
  Dec( g_ComboNodes );
  {$ENDIF}
  if Assigned( FIShfParent ) then
    FIShfParent.Release;
  ShellMemFree( FParentPidl );
  if Assigned( FThisIshf ) then
    FThisIshf.Release;
  ShellMemFree( FRelPidl );
  ShellMemFree( FAbsPidl );
  inherited;
end;

function TRzShellComboData.GetThisIShf: IShellFolder_NRC;
begin
  if not Assigned( FThisIshf ) then
  begin
    if Assigned( AbsoluteIdList ) then
      OleCheck( FIShfParent.BindToObject( RelativeIdList, nil, IID_IShellFolder, Pointer( FThisIshf ) ) )
    else
      OleCheck( ShellGetDesktopFolder( FThisIshf ) );
  end;
  Result := FThisIshf;
end;                                        {TRzShellComboData.GetThisIShf}

procedure TRzShellComboData.SetData( aParentIShf: IShellFolder_NRC; parentPidl, curRelativePidl: PItemIdList );
begin
  FParentPidl := CopyIdList( nil, parentPidl );
  FRelPidl := CopyIdList( nil, curRelativePidl );
  FIShfParent := aParentIShf;
  if Assigned( FIShfParent ) then
    FIShfParent.AddRef;
  FAbsPidl := ConcatIdLists( nil, parentPidl, curRelativePidl );
end;                                        {TRzShellComboData.SetData}
{== END TRzShellComboData ===================================}



{-- TRzCustomShellTree -----------------------------------}
const
  PTSHTREE_TIMER_NONE = 0;
  PTSHTREE_TIMER_KEYREFRESH = 1;
  PTSHTREE_TIMER_REFRESHNODES = 2;

  {-- Utilities local to TRzCustomShellTree --}
  // Creates a data object referencing all the selected items in the given shell list
  // D2/D3/BCB
type
  TDataObject_shtv = class( TObject )
  private
    mcRefs: Integer;
    mhCFHDrop: THandle;
    mhCFIdList: THandle;
    mFormats: TRzFormatEtcList;
  public
    constructor Create( shtv: TRzCustomShellTree; aNode: TTreeNode );
    destructor Destroy; override;
    procedure GenerateFormats( shtv: TRzCustomShellTree; aNode: TTreeNode );
         // -- IUnknown --
    function QueryInterface( const IID: TGUID; var Obj ): Integer; virtual; stdcall;
    function AddRef: Integer; virtual; stdcall;
    function Release: Integer; virtual; stdcall;
         // -- IDataObject --
    function GetData( const formatetcIn: TFormatEtc; var medium: TStgMedium ): HResult; virtual; stdcall;
    function GetDataHere( const formatetc: TFormatEtc; var medium: TStgMedium ): HResult; virtual; stdcall;
    function QueryGetData( const formatetc: TFormatEtc ): HResult; virtual; stdcall;
    function GetCanonicalFormatEtc( const formatetc: TFormatEtc; var formatetcOut: TFormatEtc ): HResult; virtual; stdcall;
    function SetData( const formatetc: TFormatEtc; var medium: TStgMedium; fRelease: BOOL ): HResult; virtual; stdcall;
    function EnumFormatEtc( dwDirection: Longint; var enumFormatEtc: IEnumFormatEtc_NRC ): HResult; virtual; stdcall;
    function DAdvise( const formatetc: TFormatEtc; advf: Longint; const advSink: IAdviseSink; var dwConnection: Longint ): HResult; virtual; stdcall;
    function DUnadvise( dwConnection: Longint ): HResult; virtual; stdcall;
    function EnumDAdvise( var enumAdvise: IEnumStatData ): HResult; virtual; stdcall;
  end;

constructor TDataObject_shtv.Create( shtv: TRzCustomShellTree; aNode: TTreeNode );
begin
  {$IFDEF PTDEBUG}
  Inc( g_DataObject_shtv );
  {$ENDIF}
  inherited Create;
  mFormats := TRzFormatEtcList.Create;      //Init( [CF_IDLIST, CF_HDROP] );
  GenerateFormats( shtv, aNode );
  if mhCFIdList <> 0 then
    mFormats.Add( [ CF_IDLIST ] );
  if mhCfHDrop <> 0 then
    mFormats.Add( [ CF_HDROP ] );
end;

destructor TDataObject_shtv.Destroy;
begin
  {$IFDEF PTDEBUG}
  Dec( g_DataObject_shtv );
  {$ENDIF}
  if ( mhCfHdrop <> 0 ) then
    GlobalFree( mhCfHDrop );
  if ( mhCfIdList <> 0 ) then
    GlobalFree( mhCfIdList );
  mFormats.Free;
  inherited Destroy;
end;

procedure TDataObject_shtv.GenerateFormats( shtv: TRzCustomShellTree; aNode: TTreeNode );
var
  l: TList;
  sl: TStringList;
  str: string;
  parentdata: TRzShellTreeData;
begin
  if not shtv.NodeHasData( aNode ) then
    Exit;
  l := nil;
  sl := nil;
  try
    l := TList.Create;
    l.Add( shtv.GetDataFromNode( aNode ).RelativeIdList );

    parentdata := shtv.GetDataFromNode( aNode.Parent );
    Create_CFIDLIST_HGlobal( parentdata.AbsoluteIdList, l, mhCfIdList );

    str := ShellGetFriendlyNameFromIdList( parentdata.ThisIShf, l[ 0 ], fnForParsing );
    sl := TStringList.Create;
    sl.Add( str );
    Create_CFHDROP_HGlobal( sl, mhCfHDrop );
  finally
    l.Free;
    sl.Free;
  end;
end;                                        {TDataObject_shtv.GenerateFormats}

function TDataObject_shtv.QueryInterface( const IID: TGUID; var Obj ): Integer;
begin
  if IsEqualIID( iid, IID_IUnknown ) or IsEqualIID( iid, IID_IDataObject ) then
  begin
    Pointer( obj ) := Self;
    AddRef;
    Result := S_OK;
  end
  else
  begin
    Pointer( obj ) := nil;
    Result := Integer( E_NOINTERFACE );
  end;
end;                                        {TDataObject_shtv.QueryInterface}

function TDataObject_shtv.AddRef: Integer;
begin
  Inc( mcRefs );
  Result := mcRefs;
end;                                        {TDataObject_shtv.AddRef}

function TDataObject_shtv.Release: Integer;
begin
  Dec( mcRefs );
  Result := mcRefs;
  if ( mcRefs = 0 ) then
    Free;
end;                                        {TDataObject_shtv.Release}

function TDataObject_shtv.GetData( const formatetcIn: TFormatEtc; var medium: TStgMedium ): HResult;
begin
  Result := QueryGetData( formatetcIn );
  if Failed( Result ) then
    Exit;

  medium.tymed := TYMED_HGLOBAL;
  medium.unkForRelease := nil;

  if formatetcIn.cfFormat = CF_IDLIST then
    medium.hGlobal := DuplicateHGlobal( mhCfIdList )
  else if formatetcIn.cfFormat = CF_HDROP then
    medium.hGlobal := DuplicateHGlobal( mhCfHDrop )
  else
  begin
    Result := DV_E_FORMATETC;
    Exit;
  end;
  if medium.hGlobal = 0 then
    Result := E_OUTOFMEMORY
  else
    Result := S_OK;
end;                                        {TDataObject_shtv.GetData}

function TDataObject_shtv.GetDataHere( const formatetc: TFormatEtc; var medium: TStgMedium ): HResult;
begin
  Result := E_NOTIMPL;
end;

function TDataObject_shtv.QueryGetData( const formatetc: TFormatEtc ): HResult;
begin
  Result := mFormats.FormatSupported( formatetc );
end;

function TDataObject_shtv.GetCanonicalFormatEtc( const formatetc: TFormatEtc; var formatetcOut: TFormatEtc ): HResult;
begin
  formatetcOut.ptd := nil;
  Result := E_NOTIMPL;
end;                                        {TDataObject_shtv.GetCanonicalFormatEtc}

function TDataObject_shtv.SetData( const formatetc: TFormatEtc; var medium: TStgMedium; fRelease: BOOL ): HResult;
begin
  Result := E_NOTIMPL;
end;

function TDataObject_shtv.EnumFormatEtc( dwDirection: Longint; var enumFormatEtc: IEnumFormatEtc_NRC ): HResult;
begin
  if dwDirection = DATADIR_GET then
  begin
    enumFormatEtc := mFormats.CreateIEnumFormatEtc;
    enumFormatEtc.AddRef;
    Result := S_OK;
  end
  else
  begin
    Pointer( enumFormatEtc ) := nil;
    Result := E_NOTIMPL;
  end;
end;                                        {TDataObject_shtv.EnumFormatEtc}


function TDataObject_shtv.DAdvise( const formatetc: TFormatEtc; advf: Longint; const advSink: IAdviseSink; var dwConnection: Longint ): HResult;
begin
  Result := OLE_E_ADVISENOTSUPPORTED;
end;


function TDataObject_shtv.DUnadvise( dwConnection: Longint ): HResult;
begin
  Result := OLE_E_ADVISENOTSUPPORTED;
end;


function TDataObject_shtv.EnumDAdvise( var enumAdvise: IEnumStatData ): HResult;
begin
  Result := OLE_E_ADVISENOTSUPPORTED;
end;


  // A DropTarget object for the list view.
  // D2/D3/BCB
type
  TDropTarget_shtv = class( TObject )
  private
    FOwner: TRzCustomShellTree;
    mcRefs: Integer;
  protected
         // -- IUnknown -----------------
    function QueryInterface( const IID: TGUID; var Obj ): Integer; virtual; stdcall;
    function AddRef: Integer; virtual; stdcall;
    function Release: Integer; virtual; stdcall;

         // -- IDropTarget --------------
    function DragEnter( const dataObj: IDataObject_NRC; grfKeyState: Longint; pt: TPoint; var dwEffect: Longint ): HResult; virtual; stdcall;
    function DragOver( grfKeyState: Longint; pt: TPoint; var dwEffect: Longint ): HResult; virtual; stdcall;
    function DragLeave: HResult; virtual; stdcall;
    function Drop( const dataObj: IDataObject_NRC; grfKeyState: Longint; pt: TPoint; var dwEffect: Longint ): HResult; virtual; stdcall;
  public
    constructor Create( aOwner: TRzCustomShellTree );
    destructor Destroy; override;
  end;

constructor TDropTarget_shtv.Create( aOwner: TRzCustomShellTree );
begin
  {$IFDEF PTDEBUG}
  Inc( g_DropTarget_shtv );
  {$ENDIF}
  inherited Create;
  FOwner := aOwner;
end;                                        {TDropTarget_shtv.Create}

destructor TDropTarget_shtv.Destroy;
begin
  {$IFDEF PTDEBUG}
  Dec( g_DropTarget_shtv );
  {$ENDIF}
  inherited Destroy;
end;


function TDropTarget_shtv.QueryInterface( const IID: TGUID; var Obj ): Integer;
begin
  if IsEqualIID( iid, IID_IUnknown ) or IsEqualIID( iid, IID_IDropTarget ) then
  begin
    Pointer( obj ) := Self;
    AddRef;
    Result := S_OK;
  end
  else
  begin
    Pointer( obj ) := nil;
    Result := Integer( E_NOINTERFACE );
  end;
end;


function TDropTarget_shtv.AddRef: Integer;
begin
  Inc( mcRefs );
  Result := mcRefs;
end;


function TDropTarget_shtv.Release: Integer;
begin
  Dec( mcRefs );
  Result := mcRefs;
  if ( mcRefs = 0 ) then
    Free;
end;


function TDropTarget_shtv.DragEnter( const dataObj: IDataObject_NRC; grfKeyState: Longint; pt: TPoint; var dwEffect: Longint ): HResult;
begin
  Result := FOwner.OnDropTarget_DragEnter( dataObj, grfKeyState, pt, dwEffect );
end;


function TDropTarget_shtv.DragOver( grfKeyState: Longint; pt: TPoint; var dwEffect: Longint ): HResult;
begin
  Result := FOwner.OnDropTarget_DragOver( grfKeyState, pt, dwEffect );
end;


function TDropTarget_shtv.DragLeave: HResult;
begin
  Result := FOwner.OnDropTarget_DragLeave;
end;


function TDropTarget_shtv.Drop( const dataObj: IDataObject_NRC; grfKeyState: Longint; pt: TPoint; var dwEffect: Longint ): HResult;
begin
  Result := FOwner.OnDropTarget_Drop( dataObj, grfKeyState, pt, dwEffect );
end;


{&RT}
{-- TRzCustomShellTree proper ----------------------------}

constructor TRzCustomShellTree.Create( aOwner: TComponent );
begin
  inherited;
  {&RCI}
  FTimer := TTimer.Create( Self );
  FTimer.Enabled := False;
  FTimer.OnTimer := TimerElapsed;

  FBaseFolder := TRzShellLocator.Create;
  FBaseFolder.OnChange := BaseFolderChanged;

  FSelectedFolder := TRzShellLocator.Create;
  FSelectedFolder.OnChange := SelectedFolderChanged;

  ShowRoot := False;

  FOptions := [ stoAutoFill, stoVirtualFolders, stoDefaultKeyHandling, stoContextMenus,
                stoDynamicRefresh, stoOleDrag, stoOleDrop, stoShowHidden ];

  Images := TImageList.CreateSize( 16, 16 );

  RzDeviceChangeHandler.Add( DeviceChangeDetected );

  InitImageList;

  Height := 150;
  Width := 150;
  ItemHeightMargin := 3;
  {&RV}
end; {= TRzCustomShellTree.Create =}



destructor TRzCustomShellTree.Destroy;
{$IFDEF PTDEBUG}
var
  treeNodes, dbgNodes: Integer;
{$ENDIF}
begin
  RzDeviceChangeHandler.Remove( DeviceChangeDetected );
  if Assigned( FIDropTarget ) then
  begin
    if HandleAllocated then
      RevokeDragDrop( Handle );
    FIDropTarget.Release;
    FIDropTarget := nil;
  end;
  FTimer.Free;
  TObject( FChangeHandlerThread ).Free;
  FChangeHandlerThread := nil;
  Images.Free;
  FBaseFolder.Free;
  FSelectedFolder.Free;
  {$IFDEF PTDEBUG}
  treeNodes := g_TreeNodes;
  dbgNodes := mdbgNodes;
  {$ENDIF}
  inherited Destroy;
  {$IFDEF PTDEBUG}
  if treeNodes - g_TreeNodes <> dbgNodes then
  begin
    MessageBeep( UINT( -1 ) );
    Windows.MessageBox( 0, PChar( Format( 'TRzCustomShellTree: %d nodes leaked!', [ treeNodes - g_TreeNodes ] ) ), 'Debug', MB_OK );
  end;
  {$ENDIF}
end; {= TRzCustomShellTree.Destroy =}



function TRzCustomShellTree.CreateIDropSource( Button: TMouseButton; DataObject: IDataObject_NRC ): IDropSource_NRC;
begin
  TDropSource_shlvtv.Create( Button ).QueryInterface( IID_IDropSource, Result );
end;


procedure TRzCustomShellTree.OleBeginDrag( Button: TMouseButton );
var
  effect: Longint;
  ids: IDropSource_NRC;
  fDeferred: Boolean;
  DragResult: HResult;
begin
  ids := nil;
  try
    FDeferRefresh := True;
    TDataObject_shtv.Create( Self, FDragNode ).QueryInterface( IID_IDataObject, FIDataObject );
    effect := Self.GetDragDropAttributesForNode( FDragNode );


    ids := CreateIDropSource( Button, FIDataObject );

    DragResult := _DoDragDrop( FIDataObject, ids, effect, effect );


    if DragResult = DRAGDROP_S_DROP then
      fDeferred := True
    else
      fDeferred := FRefreshDeferred;

  finally
    FDeferRefresh := False;
    FRefreshDeferred := False;
    if Assigned( ids ) then
      ids.Release;
    if Assigned( FIDataObject ) then
    begin
      FIDataObject.Release;
      FIDataObject := nil;
    end;
  end;
  if fDeferred then
    RefreshNodes;
end; {= TRzCustomShellTree.OleBeginDrag =}


function TRzCustomShellTree.OnDropTarget_DragEnter( const dataObj: IDataObject_NRC; grfKeyState: Longint; pt: TPoint; var dwEffect: Longint ): HResult;
var
  idt: IDropTarget_NRC;
  node: TTreeNode;
  hr: HResult;
begin
  FLastAutoScrollTick := Windows.GetTickCount;

  FILastDropDataObject := dataObj;
  FILastDropDataObject.AddRef;
  FInitialDropKeyState := grfKeyState;

  Result := S_OK;
  with ScreenToClient( pt ) do
    node := GetNodeAt( x, y );
  FLastAutoOpenPos := pt;

  if NodeHasData( node ) and Assigned( node.Parent ) then
  begin
    idt := nil;
    try
      hr := GetDataFromNode( node ).GetIDropTarget( GetParentHWND, idt );
      DropTarget := node;                   // Explorer highlights the tree node even if it's invalid
      if Succeeded( hr ) then
      begin
        Result := idt.DragEnter( dataObj, grfKeyState, pt, dwEffect );
        idt.DragLeave;
      end
      else
        dwEffect := DROPEFFECT_NONE;
    finally
      if Assigned( idt ) then
        idt.Release;
    end;
  end
  else
  begin
    DropTarget := FDragNode;
    dwEffect := DROPEFFECT_NONE;
  end;
end; {= TRzCustomShellTree.OnDropTarget_DragEnter =}


function TRzCustomShellTree.OnDropTarget_DragOver( grfKeyState: Longint; pt: TPoint; var dwEffect: Longint ): HResult;
var
  idt: IDropTarget_NRC;
  node: TTreeNode;
  hr: HResult;
  oldDropTarget: TTreeNode;
begin
  oldDropTarget := DropTarget;
  Result := S_OK;
  pt := ScreenToClient( pt );
  node := GetNodeAt( pt.x, pt.y );

  if NodeHasData( node ) then
  begin
    idt := nil;
    try
      hr := GetDataFromNode( node ).GetIDropTarget( GetParentHWND, idt );
      DropTarget := node;
      if Succeeded( hr ) then
      begin
        idt.DragEnter( FILastDropDataObject, grfKeyState, pt, dwEffect );
        Result := idt.DragOver( grfKeyState, pt, dwEffect );
        idt.DragLeave;
      end
      else
        dwEffect := DROPEFFECT_NONE;
    finally
      if Assigned( idt ) then
        idt.Release;
    end;
  end
  else
  begin
    dwEffect := DROPEFFECT_NONE;
    DropTarget := FDragNode;
  end;

  DoDropTargetAutoScroll( pt );
  dwEffect := dwEffect or Integer( DROPEFFECT_SCROLL );

  if oldDropTarget <> DropTarget then
    Update;
end; {= TRzCustomShellTree.OnDropTarget_DragOver =}


function TRzCustomShellTree.OnDropTarget_DragLeave: HResult;
begin
  FILastDropDataObject.Release;
  FILastDropDataObject := nil;
  DropTarget := nil;
  Result := S_OK;
end;


function TRzCustomShellTree.OnDropTarget_Drop( const dataObj: IDataObject_NRC; grfKeyState: Longint; pt: TPoint; var dwEffect: Longint ): HResult;
var
  idt: IDropTarget_NRC;
  hr: HResult;
  node, CurrentTopItem: TTreeNode;
  dw2: Longint;
  oldCursor: TCursor;
begin
  Result := S_OK;
  with ScreenToClient( pt ) do
    node := GetNodeAt( x, y );

  if NodeHasData( node ) then
  begin
    idt := nil;
    try
      hr := GetDataFromNode( node ).GetIDropTarget( GetParentHWND, idt );
      if Succeeded( hr ) then
      begin
        if ( FInitialDropKeyState and MK_RBUTTON ) <> 0 then
          grfKeyState := grfKeyState or MK_RBUTTON;
        if ( FInitialDropKeyState and MK_LBUTTON ) <> 0 then
          grfKeyState := grfKeyState or MK_LBUTTON;

        idt.DragEnter( dataObj, grfKeyState, pt, dw2 );
        idt.DragOver( grfKeyState, pt, dw2 );
        dwEffect := dwEffect and GetDragDropAttributesForClipboardObjects( dataObj );
        oldCursor := Screen.Cursor;
        Screen.Cursor := crHourglass;
        try
          Result := idt.Drop( dataObj, grfKeyState, pt, dwEffect );
        finally
          Screen.Cursor := oldCursor;
        end;
        CurrentTopItem := TopItem;
        RefreshNodes;
        TopItem := CurrentTopItem;
      end;
    finally
      if Assigned( idt ) then
        idt.Release;
    end;
  end;

  DropTarget := nil;
  FILastDropDataObject.Release;
  FILastDropDataObject := nil;
end; {= TRzCustomShellTree.OnDropTarget_Drop =}


procedure TRzCustomShellTree.DoDropTargetAutoscroll( pt: TPoint );
var
  Node: TTreeNode;
  ActiveRect: TRect;
  IsInActiveRect: Boolean;
begin
  if Windows.GetTickCount > Cardinal( FLastAutoScrollTick + RZSH_AUTOSCROLL_MINDELAY_MS ) then
  begin
    if ( pt.x < RZSH_AUTOSCROLL_THRESHOLD_X ) then
    begin
      if DoAutoScroll( Self, SB_HORZ, SB_LINEUP ) then
        FLastAutoOpenTick := Windows.GetTickCount;
    end
    else if ( pt.x > ClientRect.right - RZSH_AUTOSCROLL_THRESHOLD_X ) then
    begin
      if DoAutoScroll( Self, SB_HORZ, SB_LINEDOWN ) then
        FLastAutoOpenTick := Windows.GetTickCount;
    end;

    if ( pt.y < RZSH_AUTOSCROLL_THRESHOLD_Y ) then
    begin
      if DoAutoScroll( Self, SB_VERT, SB_LINEUP ) then
        FLastAutoOpenTick := Windows.GetTickCount;
    end
    else if ( pt.y > ClientRect.bottom - RZSH_AUTOSCROLL_THRESHOLD_Y ) then
    begin
      if DoAutoScroll( Self, SB_VERT, SB_LINEDOWN ) then
        FLastAutoOpenTick := Windows.GetTickCount;
    end;

    FLastAutoScrollTick := Windows.GetTickCount;
  end;

  ActiveRect := Rect(
    FLastAutoOpenPos.x - ( RZSH_AUTOOPEN_THRESHOLD_X ) div 2,
    FLastAutoOpenPos.Y - ( RZSH_AUTOOPEN_THRESHOLD_Y ) div 2,
    FLastAutoOpenPos.x + RZSH_AUTOOPEN_THRESHOLD_X,
    FLastAutoOpenPos.y + RZSH_AUTOOPEN_THRESHOLD_Y );

  IsInActiveRect := PtInRect( ActiveRect, pt );

  if IsInActiveRect then
  begin
    if Windows.GetTickCount > Cardinal( FLastAutoOpenTick + RZSH_AUTOOPEN_DELAY_MS ) then
    begin
      Node := GetNodeAt( FLastAutoOpenPos.x, FLastAutoOpenPos.y );
      Inc( FIgnoreErrors );
      try
        try
          if Assigned( Node ) and Node.HasChildren and not Node.Expanded then
            Node.Expand( False );
        except
          Node.HasChildren := False;
        end;
      finally
        Dec( FIgnoreErrors );
      end;

      FLastAutoOpenPos := pt;
      FLastAutoOpenTick := Windows.GetTickCount;
    end;
  end
  else                                      // if not in the active rectange, then reset the timer and move the active rectangle
  begin
    FLastAutoOpenPos := pt;
    FLastAutoOpenTick := Windows.GetTickCount;
  end;
end; {= TRzCustomShellTree.DoDropTargetAutoscroll =}


procedure TRzCustomShellTree.InitImageList;
begin
  Images.Handle := ShellGetSystemImageList( iszSmall );
  Images.ShareImages := True;

// If ShareImages is done before assigning the handle, then the existing image list is also shared and not
// correctly freed when overwritten by the assigning of the system image list handle.
end;


procedure TRzCustomShellTree.BaseFolderChanged( Sender: TObject );
begin
  if not ( csLoading in ComponentState ) then
  begin
    FSelectedFolder.Clear;
    if ( stoAutoFill in Options ) then
    begin
      FQuickSelect := True;
      FillItems;
    end;
  end;
end;


procedure TRzCustomShellTree.SelectedFolderChanged( Sender: TObject );
begin
  if Items.Count > 0 then
    DoSetSelectedIdList( SelectedFolder.IdList );
end;


function TreeCompareFunc( aNode1, aNode2, lParam: TRzNativeInt ): Integer; stdcall;
var
  node1: TTreeNode absolute aNode1;
  node2: TTreeNode absolute aNode2;

  node1Data: TRzShellTreeData;
  node2Data: TRzShellTreeData;
  hres: HResult;
begin
  if ( TObject( node1.Data ) is TRzShellTreeData ) and ( TObject( node2.Data ) is TRzShellTreeData ) then
  begin
    node1Data := TRzShellTreeData( node1.Data );
    node2Data := TRzShellTreeData( node2.Data );
    hres := node1Data.ParentIShf.CompareIds( 0, node1Data.RelativeIdList, node2Data.RelativeIdList );
    if Succeeded( hres ) then
      Result := Smallint( ResultCode( hres ) )
    else
      Result := 0;
  end
  else
    Result := 0;                            // Should call OnCompareItem event
end;


procedure TRzCustomShellTree.TimerElapsed( Sender: TObject );
begin
  FTimer.Enabled := False;
  case FTimer.Tag of
    PTSHTREE_TIMER_KEYREFRESH:
      if NodeHasData( Selected ) then
      begin
        Inc( FIgnoreChanges );
        try
          FSelectedFolder.IdList := GetDataFromNode( Selected ).AbsoluteIdList;
          if Assigned( ShellList ) then
            ShellList.TreeChanged( Selected );
          if Assigned( FShellCombo ) then
            FShellCombo.TreeChanged( Selected );
        finally
          Dec( FIgnoreChanges );
        end;
      end;

    PTSHTREE_TIMER_REFRESHNODES:
      RefreshNodes;
  end;                                      {case}
  FTimer.Tag := PTSHTREE_TIMER_NONE;
end;


procedure TRzCustomShellTree.SortNode( Node: TTreeNode );
begin
  if Assigned( Node ) then
    Node.CustomSort( TreeCompareFunc, NativeInt( Self ) )
  else
    CustomSort( TreeCompareFunc, NativeInt( Self ) );
end;


function TRzCustomShellTree.GetSelectedItem: TRzShellTreeData;
begin
  Result := GetDataFromNode( Selected );
end;


function TRzCustomShellTree.GetSelectedPathName: string;
begin
  if ( Selected = nil ) or not NodeHasData( Selected ) then
    Result := ''
  else
    Result := GetDataFromNode( Selected ).PathName;
end;


function TRzCustomShellTree.GetShellTreeData( idx: Integer ): TRzShellTreeData;
var
  o: TObject;
begin
  o := ( TObject( Items[ idx ].Data ) );
  if o is TRzShellTreeData then
    Result := TRzShellTreeData( o )
  else
    Result := nil;
end;


function TRzCustomShellTree.GetParentHWND: HWND;
begin
  if FIgnoreErrors > 0 then
    Result := 0
  else
    Result := GetValidParentHWND( Self );
end;


procedure TRzCustomShellTree.SetBaseFolder( Value: TRzShellLocator );
begin
  if not ( csLoading in ComponentState ) then
  begin
    FBaseFolder.Assign( Value );           // Let the OnChange notification do any updating
  end;
end;


procedure TRzCustomShellTree.SetShellCombo( Value: TRzCustomShellCombo );
begin
  FShellCombo := Value;
  if Assigned( FShellCombo ) then
    FShellCombo.FreeNotification( Self );
end;


procedure TRzCustomShellTree.AMChangeNotify( var Msg: TMessage );
begin
  if FIgnoreNextChangeNotify or FDeferRefresh then
  begin
    FIgnoreNextChangeNotify := False;
    Exit;
  end;

  FTimer.Enabled := False;
  FTimer.Tag := PTSHTREE_TIMER_REFRESHNODES;
  if Focused then
    FTimer.Interval := RZSH_CHANGE_NOTIFY_FASTDELAY
  else
    FTimer.Interval := RZSH_CHANGE_NOTIFY_DELAY;
  FTimer.Enabled := True;
end;


procedure TRzCustomShellTree.AMDeferredFill( var Msg: TMessage );
begin
  if Items.Count = 0 then
    FillItems;
end;


procedure TRzCustomShellTree.CMDesignHitTest( var Msg: TCMDesignHitTest );
var
  h: THitTests;
begin
  if ( stoDesignInteractive in Options ) then
  begin
    h := GetHitTestInfoAt( Msg.Pos.X, Msg.Pos.Y );
    if ( h * [ htOnButton, htToRight, htOnItem ] <> [ ] ) or ( Msg.Pos.X > ClientWidth ) then
      Msg.Result := 1
    else
      Msg.Result := 0;
  end
  else
    Msg.Result := 0;
end;


procedure TRzCustomShellTree.CMWantSpecialKey( var Msg: TCMWantSpecialKey );
begin
  inherited;
  if IsEditing then
    Msg.Result := 1;
end;


procedure TRzCustomShellTree.CNNotify( var Msg: TWMNotify );

  function GetNodeFromItem( const Item: TTVItem ): TTreeNode;
  begin
    with Item do
      if ( state and TVIF_PARAM ) <> 0 then
        Result := Pointer( lParam )
      else
        Result := Items.GetNode( hItem );
  end;

begin
 // The following code only deletes our object associated with the node.
 // We let the inherited procedure do the deletion of the node itSelf.
//  with Msg.NMHdr^ do
    case Msg.NMHdr.code of
      NM_CLICK:
        begin
          FQuickSelect := True;
          inherited;
        end;

      TVN_BEGINDRAG, TVN_BEGINRDRAG:
        if stoOleDrag in Options then
        begin
          with PNMTreeView( Pointer( Msg.NMHdr ) )^ do
            FDragNode := GetNodeFromItem( ItemNew );

          try
            DropTarget := FDragNode;
            if NodeHasData( FDragNode ) and Assigned( FDragNode.Parent ) then
              if Msg.NMHdr.code = TVN_BEGINDRAG then
                OleBeginDrag( mbLeft )
              else
                OleBeginDrag( mbRight );
          finally
            FDragNode := nil;
            DropTarget := nil;
          end;
        end
        else
          inherited;
      else
        inherited;
    end; {case}
end; {= TRzCustomShellTree.CNNotify =}


procedure TRzCustomShellTree.TVMDeleteItem( var Msg: TMessage );
begin
  if ( HTreeItem( Msg.lParam ) = TVI_ROOT ) then
  try                                       // Prevents a cascade of selection change messages causing dozens of updates, which sometimes occurs in ActiveX.
    Inc( FIgnoreChanges );
    inherited;
  finally
    Dec( FIgnoreChanges );
  end
  else
    inherited;
end;


procedure TRzCustomShellTree.WMMenuChar( var Msg: TWMMenuChar );
begin
  inherited;
  SendTo_WMMenuChar( Msg, FActiveContextMenu, _sl );
end;


procedure TRzCustomShellTree.WMDrawItem( var Msg: TWMDrawItem );
begin
  inherited;
  SendTo_WMDrawItem( Msg, FActiveContextMenu, Images, _sl );
end;


procedure TRzCustomShellTree.WMMeasureItem( var Msg: TWMMeasureItem );
begin
  inherited;
  SendTo_WMMeasureItem( Msg, FActiveContextMenu, _sl );
end;


procedure TRzCustomShellTree.WMMenuSelect( var Msg: TWMMenuSelect );
begin
  inherited;
  Popup_WMMenuSelect( Msg, FActiveContextMenu, Self, OnPopupHint );
end;


procedure TRzCustomShellTree.WMNCDestroy( var Msg: TWMNCDestroy );
begin
  Inc( FIgnoreChanges );
  try
    if Assigned( FIDropTarget ) then
    begin
      RevokeDragDrop( Handle );
      FIDropTarget.Release;
      FIDropTarget := nil;
    end;

    if Assigned( FChangeHandlerThread ) then
    begin
      TObject( FChangeHandlerThread ).Free;
      FChangeHandlerThread := nil;
    end;

    inherited;
  finally
    Dec( FIgnoreChanges );
  end;
end; {= TRzCustomShellTree.WMNCDsetroy =}


procedure TRzCustomShellTree.WMNCHitTest( var Msg: TWMNCHitTest );
begin
  DefaultHandler( Msg );                    // Bypass design time hooks to allow design time interactive scrollbars
end;


procedure TRzCustomShellTree.WMInitMenuPopup( var Msg: TWMInitMenuPopup );
begin
  inherited;
  SendTo_WMInitMenuPopup( Msg, FActiveContextMenu, _sl );
end;


procedure TRzCustomShellTree.WMWindowPosChanged( var Msg: TWMWindowPosChanged );
begin
  if not FDeletingNodes then
    inherited;
end;


procedure TRzCustomShellTree.WMPaint( var Msg: TWMPaint );
var
  StateImageList: THandle;
  OldBkColor: TColorRef;
  OldStateBkColor: TColorRef;
begin
  if not ( csDestroying in ComponentState ) then
  begin
    OldBkColor := ImageList_GetBkColor( Images.Handle );

    StateImageList := TreeView_GetImageList( Handle, TVSIL_STATE );
    if StateImageList <> 0 then
      OldStateBkColor := ImageList_GetBkColor( StateImageList )
    else
      OldStateBkColor := 0;                 // Unnecessary, but prevents warning

    try                                     // Use the API to prevent a Change event occuring for the imagelist component
      ImageList_SetBkColor( Images.Handle, ColorToRGB( Color ) );
      ImageList_SetBkColor( StateImageList, ColorToRGB( Color ) );
      inherited;
    finally
      ImageList_SetBkColor( Images.Handle, OldBkColor );
      if StateImageList <> 0 then
        ImageList_SetBkColor( StateImageList, OldStateBkColor );
    end;
  end;
end;


procedure TRzCustomShellTree.SetShellList( Value: TRzCustomShellList );
begin
  if Assigned( FShellList ) and ( Value <> FShellList ) then
    FShellList.SetShellTree( nil );
  FShellList := Value;
  if Assigned( FShellList ) then
  begin
    FShellList.SetShellTree( Self );
    FShellList.FreeNotification( Self );
    FShellList.Folder := SelectedFolder;
  end;
end;


procedure TRzCustomShellTree.SetOptions( Value: TRzShellTreeOptions );
const
  DRAGOPTS = [ stoOleDrag, stoOleDrop ];
var
  oldOptions: TRzShellTreeOptions;
begin
  oldOptions := FOptions;
  FOptions := Value;
  if not HandleAllocated then
    Exit;

  if ( oldOptions * [ stoOleDrop ] <> FOptions * [ stoOleDrop ] ) then
  begin
    if ( stoOleDrop in FOptions ) and not Assigned( FIDropTarget ) then
    begin                                   // drop capability turned ON
      TDropTarget_shtv.Create( Self ).QueryInterface( IID_IDropTarget, FIDropTarget );
      _RegisterDragDrop( Handle, FIDropTarget );
    end
    else if not ( stoOleDrop in FOptions ) and Assigned( FIDropTarget ) then
    begin                                   // drop capability turned OFF
      RevokeDragDrop( Handle );
      FIDropTarget.Release;
      FIDropTarget := nil;
    end;
  end;

  // Enable dragging (disabled=False) if OLE drag is enabled OR (OLE drag is disabled and Delphi drag is automatic)
  SetWndStyle( Handle, TVS_DISABLEDRAGDROP,
    not ( ( stoOleDrag in FOptions ) or ( not ( stoOleDrag in FOptions ) and ( DragMode = dmAutomatic ) ) )
    );
  // Also done in CreateParams

  if ( ( oldOptions * [ stoDynamicRefresh ] <> FOptions * [ stoDynamicRefresh ] ) ) and
    not ( csDesigning in ComponentState ) then
  begin                                     // stoDynamicRefresh State is different
    if ( stoDynamicRefresh in FOptions ) then
      InstallChangeHandlersForAllLocalDrives
    else if Assigned( FChangeHandlerThread ) then
      TRzChangeHandlerThread( FChangeHandlerThread ).RemoveAllMonitors;
  end;
end; {= TRzCustomShellTree.SetOptions =}


procedure TRzCustomShellTree.SetSelectedFolder( Value: TRzShellLocator );
begin
  if ( csLoading in ComponentState ) or ( FIgnoreChanges > 0 ) then
  begin
    if Assigned( FShellList ) then
      FShellList.Folder := SelectedFolder;
    Exit;
  end;
  //FSelectedFolder.Assign( Value );
  SelectedFolder.Assign( Value );
end;


procedure TRzCustomShellTree.SetSelectedPathname( const Value: string );
begin
  if ( FIgnoreChanges > 0 ) then
    Exit;
  SelectedFolder.PathName := Value;
end;

procedure TRzCustomShellTree.DoSetSelectedIdList( Value: PItemidList );
var
  pa: TRzIdListArray;
  pabase: TRzIdListArray;
  curidx: Integer;
  curp: PItemIdList;
  tmpn, basenode, found: TTreeNode;
begin
  if ( FIgnoreChanges > 0 ) then
    Exit;

  FQuickSelect := True;

  pabase := TRzIdListArray.Create( BaseFolder.IdList );

  pa := TRzIdListArray.Create( Value );
  try
    Items.BeginUpdate;
    if pa.ItemCount = 0 then
    begin                                   // Special case for 0 items - just select first root-level shell node
      if Items.Count > 0 then
        Selected := GetFirstRootLevelShellNode;
      Exit;
    end;

    curidx := pabase.ItemCount;

    if ( curidx >= pa.ItemCount ) then
    begin
      tmpn := GetFirstRootLevelShellNode;
      tmpn.Selected := True;
      tmpn.Expand( False );
      Exit;
    end;

    curp := pa[ curidx ];
    curidx := curidx;
    basenode := GetFirstRootLevelShellNode;
    basenode.Expand( False );
    found := nil;

    while curidx < pa.ItemCount do
    begin
      found := FindNodeWithIdList( basenode, curp );
      if not Assigned( found ) then
      begin
        if Succeeded(
          AddNewShellNode( GetDataFromNode( basenode ).ThisIShf,
          GetDataFromnode( basenode ).AbsoluteIdList,
          basenode,
          curp )
          ) then
          found := FLastNode
        else
        begin
          MessageBeep( uint( -1 ) );
          Break;
        end;
//        Abort;
        // raise Exception.Create( SysErrorMessage(ERROR_PATH_NOT_FOUND) );
        // If we come across Explorer's hidden folder bug try to bluff our way through silently
      end;

     // We auto-expand the node if it is not the last one in the path or if an associated shell list
     // has the 'hide folders when linked to tree' property set. We do this otherwise the user would
     // not be able to see the folders in the current folder without going to the tree and expanding
     // the node manually.
      if ( curidx < pa.ItemCount - 1 ) or
        ( Assigned( ShellList ) and ( sloHideFoldersWhenLinkedToTree in ShellList.Options ) ) then
        found.Expand( False );

      basenode := found;
      Inc( curidx );
      if ( curidx < pa.ItemCount ) then
        curp := pa[ curidx ];
    end;
    if Assigned( found ) then
      Selected := found
    else if Assigned( basenode ) then
      Selected := basenode;
  finally
    Items.EndUpdate;
    if Assigned( Selected ) then
      Selected.MakeVisible;
    pa.Free;
    pabase.Free;
  end;
end; {= TRzCustomShellTree.DoSetSelectedIdList =}


function TRzCustomShellTree.AddNewShellNode( Parent: IShellFolder_NRC; ParentAbsIdList: PItemIdList;
                                             ParentNode: TTreeNode; RelIdList: PItemIdList ): HResult;
var
  attrib: DWORD;
  fInclude: Boolean;
  newnode: TTreeNode;
begin
  if not Assigned( RelIdList ) then
  begin
    Result := S_OK;
    Exit;
  end;
  attrib := ( SFGAO_HASSUBFOLDER or SFGAO_FOLDER or SFGAO_DISPLAYATTRMASK or SFGAO_FILESYSANCESTOR or
              SFGAO_FILESYSTEM or SFGAO_FILESYSANCESTOR or SFGAO_REMOVABLE or SFGAO_GHOSTED )
              and
              not SFGAO_READONLY;

  if SHELL32_VER.version >= SHELL32_VER60 then
    attrib := attrib or SFGAO_STREAM;

  Result := Parent.GetAttributesOf( 1, RelIdList, attrib );
  if Failed( Result ) then
    Exit;

  if not ( stoVirtualFolders in Options ) and
    ( ( attrib and ( SFGAO_FILESYSTEM or SFGAO_FILESYSANCESTOR ) ) = 0 ) then
    fInclude := False
  else
    fInclude := True;

  if fInclude then
  begin
    if ( stoIncludeNonFolders in Options ) or RzShIsFolder( attrib, stoFilesCanBeFolders in Options ) then
      fInclude := True
    else
      fInclude := False;
  end;

  fInclude := fInclude and CanAdd( Parent, ParentAbsIdList, RelIdList, attrib );

  if fInclude then
  begin
    newnode := Items.AddChild( ParentNode, ShellGetFriendlyNameFromIdList( Parent, RelIdList, fnInFolder) );
   // This folder has subfolders, so put a plus sign in the tree view control.
   // The first time the user clicks the item, we should populate the subfolders.
   // NOTE: If all the subfolders are hidden, SFGAO_HASSUBFOLDER is False (on 95 and NT4). This BAD.
    if stoIncludeNonFolders in Options then
    begin
      if ( attrib and ( SFGAO_HASSUBFOLDER or SFGAO_FOLDER ) ) <> 0 then
        newnode.HasChildren := True;
    end
    else
    begin
      if ( attrib and ( SFGAO_HASSUBFOLDER or SFGAO_FOLDER ) ) = ( SFGAO_HASSUBFOLDER or SFGAO_FOLDER ) then
        newnode.HasChildren := True;
    end;

    if ( attrib and SFGAO_SHARE ) <> 0 then
      newnode.OverlayIndex := 0
    else if ( attrib and SFGAO_LINK ) <> 0 then
      newnode.OverlayIndex := 1;

    if ( attrib and SFGAO_GHOSTED ) <> 0 then
      newnode.Cut := True;
   // Do something about cut items here - probably have to compare clipboard contents with each node :(
    newnode.Data := TRzShellTreeData.Create( Self );
    with TRzShellTreeData( newnode.Data ) do
    begin
      if Assigned( ParentNode ) then
        SetData( GetDataFromNode( ParentNode ), RelIdList )
      else
        SetData( nil, RelIdList );
    end;
    DoOnInsertItem( newnode );
  end;                                      {if fInclude}
  Result := S_OK;

end; {= TRzCustomShellTree.AddNewShellNode =}


function TRzCustomShellTree.CanAdd( ParentIShf: IShellFolder_NRC; ParentAbsPidl, ItemRelPidl: PItemIdList; Attribs: DWORD ): Bool;
begin
  Result := True;
  if Assigned( FOnAddItem ) then
    FOnAddItem( Self, ParentIShf, ParentAbsPidl, ItemRelPidl, attribs, Result );
end;


function TRzCustomShellTree.CanExpand( Node: TTreeNode ): Boolean;
begin
  if FEatExpand then
  begin
    Result := False;
    Exit;
  end;
  Result := inherited CanExpand( Node );
  if Result then
    ExpandNode( Node );
end;                                        {TRzCustomShellTree.CanExpand}


function TRzCustomShellTree.CanEdit( Node: TTreeNode ): Boolean;
var
  NodeData: TRzShellTreeData;
  EditCtl: HWND;
begin
  if NodeHasData( Node ) then
  begin
    NodeData := GetDataFromNode( Node );
    Result := NodeData.Editable and inherited CanEdit( Node ) and not ( csDesigning in ComponentState );
    if Result then
    begin
      EditCtl := TreeView_GetEditControl( Handle );
      if EditCtl <> 0 then
        SetWindowText( EditCtl, PChar( ShellGetFriendlyNameFromIdList( NodeData.ParentIShf, NodeData.RelativeIdList,
                                                                       fnForEditing ) ) );
    end;
  end
  else
    Result := inherited CanEdit( Node );
end; {= TRzCustomShellTree.CanEdit =}


procedure TRzCustomShellTree.Change( Node: TTreeNode );
begin
  if ( FIgnoreChanges > 0 ) or ( csLoading in ComponentState ) then
  begin
    if NodeHasData( Node ) then
      FSelectedFolder.IdList := GetDataFromNode( Node ).AbsoluteIdList;
    Exit;
  end;

  inherited;

  if NodeHasData( Node ) then
  begin
    if FQuickSelect or not Assigned( ShellList ) then
    begin
      Inc( FIgnoreChanges );
      try
        FSelectedFolder.IdList := GetDataFromNode( Node ).AbsoluteIdList;
        if Assigned( ShellList ) then
          ShellList.TreeChanged( Selected );
        if Assigned( FShellCombo ) then
          FShellCombo.TreeChanged( Selected );
      finally
        Dec( FIgnoreChanges );
      end;
      FQuickSelect := False;
    end
    else
    begin
      FTimer.Enabled := False;              // We want to restart the timer if it hasn't gone off yet.
      FTimer.Tag := PTSHTREE_TIMER_KEYREFRESH;
      FTimer.Interval := RZSH_TREE_KEY_UPDATE_DELAY;
      FTimer.Enabled := True;
    end;
  end;
end; {= TRzCustomShellTree.Change =}


procedure TRzCustomShellTree.DeviceChangeDetected( Sender: TObject; var Msg: TMessage );
begin
  if ( Msg.WParam = DBT_DEVICEARRIVAL ) or ( Msg.WParam = DBT_DEVICEREMOVECOMPLETE ) then
  begin
    FlushDriveInfoCache;
    LockFlushDriveInfoCache;
    try
      RefreshNodes;
    finally
      UnlockFlushDriveInfoCache;
    end;
  end;
end;


procedure TRzCustomShellTree.DoOnInsertItem( Node: TTreeNode );
begin
  FLastNode := Node;
  if Assigned( OnInsertItem ) then
    OnInsertItem( Self, Node );
end;


procedure TRzCustomShellTree.NodeContextMenu( Node: TTreeNode; var P: TPoint; var Menu: TPopupMenu );
begin
  inherited;
  if ( stoContextMenus in Options ) and not ( csDesigning in ComponentState ) and Assigned( Node ) then
  begin
    ProcessMenu( Node, P );
    Menu := nil;
  end;
end;


function TRzCustomShellTree.NodeHasData( Node: TTreeNode ): Boolean;
begin
  Result := Assigned( Node ) and Assigned( Node.Data ) and ( TObject( Node.Data ) is TRzShellTreeData );
end;


procedure TRzCustomShellTree.Delete( Node: TTreeNode );
begin
  if Assigned( OnDeleteItem ) then
    OnDeleteItem( Self, Node, Pointer( Node.Data ) );
  TObject( Node.Data ).Free;
  inherited;
end;


procedure TRzCustomShellTree.ExpandNode( Node: TTreeNode );
var
  c: TCursor;
  shnd: TRzShellTreeData;
begin
  if ( Node.Count = 0 ) and Node.HasChildren and NodeHasData( Node ) then
  begin
    c := Screen.Cursor;
    Screen.Cursor := crHourglass;
    try
      shnd := GetDataFromNode( Node );
      FillTree( shnd.ThisIshf, Node );
    finally
      Screen.Cursor := c;
    end;
  end;
end;


procedure TRzCustomShellTree.Edit( const Item: TTVItem );

  function GetNodeFromItem( const Item: TTVItem ): TTreeNode;
  begin
    with Item do
      if ( state and TVIF_PARAM ) <> 0 then
        Result := Pointer( lParam )
      else
        Result := Items.GetNode( hItem );
  end;

var
  S: string;
  Node: TTreeNode;
  EditData: TRzShellTreeData;
  pidlout: PItemIdList;
  wca: array[ 0..MAX_PATH ] of WideChar;
  c: TCursor;

  procedure FlushChildren( Node: TTreeNode );
  var
    I: Integer;
    n: TTreeNode;
  begin
    for I := 0 to Node.Count - 1 do
    begin
      n := Node.Item[ I ];
      GetDataFromNode( n ).Flush;           // Force any cached absolute idlist or IShellFolder interface to be released
      FlushChildren( n );
    end;
  end;
var
  fRefresh: Boolean;
begin
  with Item do
    if pszText <> nil then
    begin
      S := Trim( pszText );
      Node := GetNodeFromItem( Item );

      if NodeHasData( Node ) then
      begin
        EditData := GetDataFromNode( Node );
        if Assigned( OnEdited ) then
          OnEdited( Self, Node, S );
        pidlout := nil;
        c := Screen.Cursor;
        Screen.Cursor := crHourglass;
        FIgnoreNextChangeNotify := True;
        Inc( FIgnoreChanges );
        try
          if Succeeded( EditData.ParentIShf.SetNameOf( GetParentHWND,
                                                       EditData.RelativeIdList,
                                                       StringToWideChar( s, wca, High( wca ) ),
                                                       SHCONTF_FOLDERS, pidlout ) ) then
          begin
            if Assigned( pidlout ) then
            begin
              FlushChildren( Node );
              Node.Text := ShellGetFriendlyNameFromIdList( EditData.ParentIShf, pidlout, fnInFolder );
              EditData.SetRelPidl( pidlout );
              FSelectedFolder.IdList := EditData.AbsoluteIdList;
              if Assigned( FShellList ) then
                FShellList.Folder.IdList := EditData.AbsoluteIdList;
              if Assigned( FShellCombo ) then
                FShellCombo.SelectedFolder.IdList := EditData.AbsoluteIdList;
            end
            else
              Node.Text := ShellGetFriendlyNameFromIdList( EditData.ParentIShf, EditData.RelativeIdList, fnInFolder );

            if Assigned( Node.Parent ) then
              SortNode( Node.Parent );
          end;
          fRefresh := FRefreshDeferred;
        finally
          Dec( FIgnoreChanges );
          Screen.Cursor := c;
          FRefreshDeferred := False;
          ShellMemFree( pidlout );
        end;
        if fRefresh then
          RefreshNodes;
      end
      else
        inherited;
    end
    else
      inherited;
end; {= TRzCustomShellTree.Edit =}


{ This function is used to find the root shell node. This will be Items[0] unless non-shell nodes
  have been added before the first shell item. }

function TRzCustomShellTree.GetFirstRootLevelShellNode: TTreeNode;
begin
  Result := Items.GetFirstNode;
  while Assigned( Result ) and not NodeHasData( Result ) do
    Result := Result.GetNextSibling;
end;


procedure TRzCustomShellTree.GetImageIndex( Node: TTreeNode );
begin
  if ( Node.ImageIndex = 0 ) and NodeHasData( Node ) then
  begin
    with GetDataFromNode( Node ) do
      Node.ImageIndex := ShellGetIconIndex( {.} AbsoluteIdList, SHGFI_SMALLICON );
  end;
  inherited;
end;


procedure TRzCustomShellTree.GetSelectedIndex( Node: TTreeNode );
begin
  if ( Node.SelectedIndex = 0 ) and NodeHasData( Node ) then
    with GetDataFromNode( Node ) do
      Node.SelectedIndex := ShellGetIconIndex( {.} AbsoluteIdList, SHGFI_SMALLICON or SHGFI_OPENICON );
  inherited;
end;


procedure TRzCustomShellTree.FillTree( ishf: IShellFolder_NRC; ABaseNode: TTreeNode );
var
  dw, dummy: DWORD;
  basenode: TTreeNode;
  basepidl: PItemIdList;
  ienum: IEnumIDList_NRC;
  curRelPidl: PItemIdList;
  oldErrorMode: DWORD;
  wval: WORD;
begin
  FillStart;
  ienum := nil;
  curRelpidl := nil;
  basepidl := nil;
//  Items.BeginUpdate;  -- causes worse flicker than not using it in this case.
  oldErrorMode := SetErrorMode( SEM_FAILCRITICALERRORS );
  try
    if ( aBasenode = nil ) then
    begin                                   {Special case when creating the base enumeration - we add an item for ishf itSelf.}
      wval := 0;

      if Assigned( FBaseFolder.IdList ) then
        basepidl := CopyIdList( nil, FBaseFolder.IdList )
      else
        basepidl := nil;
      basenode := Items.Add( nil, ShellGetFriendlyNameForLastIdListElement( basepidl ) );

      if basepidl = nil then
      begin
        basenode.ImageIndex := ShellGetIconIndex( Pointer( @wval ), SHGFI_SMALLICON );
        basenode.SelectedIndex := ShellGetIconIndex( Pointer( @wval ), SHGFI_SMALLICON or SHGFI_OPENICON );
      end
      else
      begin
        basenode.ImageIndex := ShellGetIconIndex( basepidl, SHGFI_SMALLICON );
        basenode.SelectedIndex := ShellGetIconIndex( basepidl, SHGFI_SMALLICON or SHGFI_OPENICON );
      end;

      basenode.Data := TRzShellTreeData.Create( Self );
      TRzShellTreeData( basenode.Data ).SetData( nil, basepidl );
      TRzShellTreeData( basenode.Data ).FThisIshf := ishf;
      TRzShellTreeData( basenode.Data ).FThisIshf.AddRef;

      DoOnInsertItem( basenode );
    end
    else
    begin
      baseNode := aBaseNode;
      with GetDataFromNode( baseNode ) do
        basepidl := ConcatIdLists( nil, {.} Parent.AbsoluteIdList, {.} RelativeIdList );
    end;

    dw := SHCONTF_FOLDERS or
          SHCONTF_INCLUDEHIDDEN_FLAG[ stoShowHidden in Options ] or
          SHCONTF_NONFOLDERS_FLAG[ stoIncludeNonFolders in Options ];

    dw := ishf.EnumObjects( GetParentHWND, dw, ienum );
    if Failed( dw ) or not Assigned( ienum ) then
    begin
      baseNode.HasChildren := True;
      Abort;                                // Error already reported (typically)
    end;

    dw := ienum.Next( 1, curRelPidl, PInteger( @dummy ) );
    while ( dw = S_OK ) do
    begin
      OleCheck( AddNewShellNode( ishf, basepidl, basenode, curRelPidl ) );
      ShellMemFree( curRelPidl );
      curRelPidl := nil;
      dw := ienum.Next( 1, curRelPidl, nil );
    end;
    if baseNode.Count = 0 then
      baseNode.HasChildren := False;
    if ( dw <> S_False ) then
      if dw = DWORD( E_FAIL ) then
        Abort                               // No point reporting 'unspecified error', just raise silent exception.
      else
        RaiseSysError( dw );

    SortNode( basenode );
    if aBaseNode = nil then
      basenode.Expand( False );
  finally
    SetErrorMode( oldErrorMode );
    ShellMemFree( basepidl );
    ShellMemFree( curRelPidl );
    if Assigned( ienum ) then
      ienum.Release;
//    Items.EndUpdate;
    FillComplete;
  end;
end; {= TRzCustomShellTree.FillTree =}


procedure TRzCustomShellTree.FillComplete;
begin
  if Assigned( FOnFillComplete ) then
    FOnFillComplete( Self );
end;


procedure TRzCustomShellTree.FillStart;
begin
  if Assigned( FOnFillStart ) then
    FOnFillStart( Self );
end;


function TRzCustomShellTree.GetDataFromNode( node: TTreeNode ): TRzShellTreeData;
begin
  if NodeHasData( node ) then
    Result := TObject( node.Data ) as TRzShellTreeData
  else
    Result := nil;
end;


procedure TRzCustomShellTree.Loaded;
begin
  if not ( csLoading in ComponentState ) then
    Exit;

  if ( stoAutofill in Options ) then
    if not Assigned( FShellCombo ) then
    begin
      if Items.Count = 0 then
        FillItems;
      if Assigned( FShellList ) then
        FShellList.Folder := SelectedFolder;
    end;

  FLoaded := True;

  FEatExpand := True;                      // Inherited calls FullExpand at design-time. FullExpand can take >30 minutes to complete!
  inherited Loaded;
  FEatExpand := False;
end;


procedure TRzCustomShellTree.Notification( Component: TComponent; Operation: TOperation );
begin
  inherited;
  if ( Operation = opRemove ) then
    if ( Component = ShellList ) then
      FShellList := nil
    else if ( Component = FShellCombo ) then
      FShellCombo := nil;
end;


procedure TRzCustomShellTree.CreateParams( var Params: TCreateParams );
const
  dragstyle: array[ Boolean ] of DWORD = ( 0, TVS_DISABLEDRAGDROP );
begin
  inherited;
  Params.Style := ( Params.Style and not TVS_DISABLEDRAGDROP ) or
                  dragstyle[ not ( ( stoOleDrag in FOptions ) or ( not ( stoOleDrag in FOptions ) and ( DragMode = dmAutomatic ) ) ) ];
  // Also done in SetOptions
end;


procedure TRzCustomShellTree.CreateWnd;
begin
  inherited;

  if not ( csDesigning in ComponentState ) and ( stoOleDrop in Options ) then
  begin
    TDropTarget_shtv.Create( Self ).QueryInterface( IID_IDropTarget, FIDropTarget );
    _RegisterDragDrop( Handle, FIDropTarget );
  end;

  if not ( csLoading in ComponentState ) {and not Assigned(FShellCombo)} then
  begin
    if ( stoAutoFill in Options ) and ( Items.Count = 0 ) {and FLoaded} then
    begin
      PostMessage( Handle, RZSH_AM_DEFERRED_FILL, 0, 0 );
    end;
    if Assigned( FShellList ) then
      FShellList.Folder := SelectedFolder;
  end;
end;


procedure TRzCustomShellTree.DestroyWnd;
{$IFDEF VCL150_OR_HIGHER}
var
  I: Integer;
  Obj: TObject;
{$ENDIF}
begin
  FDeletingNodes := True;

  if not FSettingParent then
    Items.BeginUpdate;
  try
    Inc( FIgnoreChanges );
    try
      Selected := nil;
    finally Dec( FIgnoreChanges );
    end;

    {$IFDEF VCL150_OR_HIGHER}
    if not ( csDesigning in ComponentState ) then
    begin
      // Doing the following at design-time causes Pointer Operation Exceptions
      // in Delphi 2009 and later
      for I := 0 to Items.Count - 1 do
      begin
        Obj := ( TObject( Items[ I ].Data ) );
        if Obj is TRzShellTreeData then
          TRzShellTreeData( Obj ).Free;
      end;
    end;
    {$ENDIF}

    Items.Clear;  // This is essential. Inherited DestroyWnd tries to save the nodes to a temporary Stream.
                  // We avoid that by clearing them first.
    inherited;
  finally
    if not FSettingParent then
      Items.EndUpdate;
    FDeletingNodes := False;
  end;
end;


procedure TRzCustomShellTree.SetParent( AParent: TWinControl );
begin
  FSettingParent := True;
  try
    inherited;
  finally
    FSettingParent := False;
  end;
end;


procedure TRzCustomShellTree.KeyDown( var Key: Word; ShiftState: TShiftState );
var
  oldk: Word;
  sc: TShortCut;

  procedure Def;
  begin
    key := oldk;
    inherited;
  end;

  function IsItThisOne( thisone: TRzShellControlDefKey ): Boolean;
  begin
    Result := ( sc = gShellControlDefKeys[ thisone ].shortcut );
  end;

begin
  inherited;
  if IsEditing or ( csDesigning in ComponentState ) or not ( stoDefaultKeyHandling in Options ) then
    Exit;

  sc := ShortCut( Key, ShiftState );
  oldk := Key;
  Key := 0;

  InitDefKeys;
  if IsItThisOne( scdkEdit ) then
  begin
    if Assigned( Selected ) then
      Selected.EditText;
  end
  else if IsItThisOne( scdkRefresh ) then
    RefreshNodes
  else if IsItThisOne( scdkCut ) then
    DoCommandForNode( Selected, RZSH_CMDS_CUT )
  else if IsItThisOne( scdkCopy ) then
    DoCommandForNode( Selected, RZSH_CMDS_COPY )
  else if IsItThisOne( scdkPaste ) then
  begin
    DoCommandForNode( Selected, RZSH_CMDS_PASTE );
    RefreshNodes;
  end
  else if IsItThisOne( scdkDelete ) then
  begin
    DoCommandForNode( Selected, RZSH_CMDS_DELETE );
    RefreshNodes;
  end
  else
  begin
    sc := ShortCut( oldk, ShiftState - [ ssShift ] );
    if IsItThisOne( scdkDelete ) then
    begin
      DoCommandForNode( Selected, RZSH_CMDS_DELETE );
      RefreshNodes;
    end
    else
      Def;
  end;
end; {= TRzCustomShellTree.KeyDown ==}


procedure TRzCustomShellTree.ProcessMenu( Node: TTreeNode; P: TPoint );
var
  ld: TRzShellTreeData;
  icm: IContextMenu_NRC;
  Resultid: Integer;
  dw: DWORD;
  renameBit: DWORD;
  hm: HMENU;
begin
  if not Assigned( Node ) or not NodeHasData( Node ) then
    Exit;

  ld := GetDataFromNode( Node );
  if not Assigned( ld.ParentIShf ) or ( ld.AbsoluteIdList = nil ) or ( PWord( ld.AbsoluteIdList )^ = 0 ) then
    Exit;                                   // No menu for Desktop item - see MS Knowledge base

  if ReadOnly then
    renameBit := 0
  else
    renameBit := CMF_CANRENAME;

  dw := CreateAndPopulateContextMenu( GetParentHWND, ld.ParentIShf, 1, @ld.RelativeIdList,
    CMDOFFSET, CMF_EXPLORE or renameBit, hm, icm );
  FDeferRefresh := True;
  try
    if Failed( dw ) then
      Exit;
    P := ClientToScreen( P );
    FActiveContextMenu := icm;

    Resultid := Integer( TrackPopupMenu( hm, TPM_LEFTALIGN or TPM_RETURNCMD or TPM_RIGHTBUTTON, P.X, P.Y, 0, Handle, nil ) );

    if ( Resultid >= CMDOFFSET ) then
      if Resultid - CMDOFFSET = ICM_RENAME then
        Node.EditText
      else
      begin
        ExecuteContextMenuItem( GetParentHWND, icm, MakeIntResourceA( Resultid - CMDOFFSET ) );
        RefreshNodes;
      end
    else if ( Resultid > 0 ) then
      ProcessSendTo( Node, Resultid - 1 );
  finally
    FDeferRefresh := False;
    if Assigned( icm ) then
      icm.Release;
    FActiveContextMenu := nil;
    if ( hm <> 0 ) then
      DestroyMenu( hm );
    if FRefreshDeferred then
    begin
      FRefreshDeferred := False;
      RefreshNodes;
    end;
  end;
end; {= TRzCustomShellTree.ProcessMenu =}


procedure TRzCustomShellTree.ProcessSendTo( Node: TTreeNode; Index: Integer );
var
  data: IDataObject_NRC;
begin
  data := nil;
  try
    TDataObject_shtv.Create( Self, Node ).QueryInterface( IID_IDataObject, data );
    ProcessSendTo_via_drop( _sl[ Index ], data );
  finally
    if Assigned( data ) then
      data.Release;
  end;
end;


function TRzCustomShellTree.DoMouseWheelDown( Shift: TShiftState; MousePos: TPoint ): Boolean;
begin
  Result := inherited DoMouseWheelDown( Shift, MousePos );

  if not Result then
    Result := DoWheelScroll( Self, SB_VERT, SB_LINEDOWN, SB_PAGEDOWN );
end;


function TRzCustomShellTree.DoMouseWheelUp( Shift: TShiftState; MousePos: TPoint ): Boolean;
begin
  Result := inherited DoMouseWheelUp( Shift, MousePos );

  if not Result then
    Result := DoWheelScroll( Self, SB_VERT, SB_LINEUP, SB_PAGEUP );
end;


procedure TRzCustomShellTree.InstallChangeHandler( const PathName: string );
var
  p: Pointer;
  s: string;
  opts: DWORD;
begin
  {$IFDEF PTNOTHREADS}
  Exit;
  {$ENDIF}
  if not Assigned( FChangeHandlerThread ) then
    FChangeHandlerThread := TRzChangeHandlerThread.Create( Handle, RZSH_AM_CHANGE_NOTIFY );

  if ( PathName = '' ) then
    TRzChangeHandlerThread( FChangeHandlerThread ).RemoveAllMonitors
  else
  begin
    if ( stoIncludeNonFolders in Options ) then
      opts := FILE_NOTIFY_CHANGE_FILE_NAME
    else
      opts := 0;

    s := EnsureTrailingCharDB( ExtractFileDrive( PathName ), '\' );

    TRzChangeHandlerThread( FChangeHandlerThread ).AddMonitorDir( s,
      FILE_NOTIFY_CHANGE_DIR_NAME or opts,
      True, p );
  end;
end; {= TRzCustomShellTree.InstallChangeHandler =}


procedure TRzCustomShellTree.InstallChangeHandlersForAllLocalDrives;

  procedure AddHandler( const s: string );
  var
    nType: DWORD;
  begin
    nType := Windows.GetDriveType( PChar( s ) );
    case nType of
      DRIVE_REMOTE:
        if IsNetworkDriveConnected( s[ 1 ] ) then
          InstallChangeHandler( s );

      DRIVE_FIXED, DRIVE_RAMDISK:
        InstallChangeHandler( s );
    end;
  end;

var
  sDrive: string;
  nPos: Integer;
  dwDriveList: DWORD;
begin
  {$IFDEF PTNOTHREADS}
  Exit;
  {$ENDIF}
  if not Assigned( FChangeHandlerThread ) then
    FChangeHandlerThread := TRzChangeHandlerThread.Create( Handle, RZSH_AM_CHANGE_NOTIFY );
  TRzChangeHandlerThread( FChangeHandlerThread ).RemoveAllMonitors;

  sDrive := '?:\';
  dwDriveList := Windows.GetLogicalDrives;

  nPos := 0;
  while ( dwDriveList <> 0 ) do
  begin
    if ( dwDriveList and 1 ) <> 0 then
    begin
      sDrive[ 1 ] := Chr( Ord( 'A' ) + nPos );
      AddHandler( sDrive );
    end;
    dwDriveList := dwDriveList shr 1;
    Inc( nPos );
  end;
end;                                        {TRzCustomShellTree.InstallChangeHandlersForAllLocalDrives}


procedure TRzCustomShellTree.FillItems;
var
  shf: IShellFolder_NRC;
  dw: DWORD;
  c: TCursor;
begin
  if FDeferRefresh or IsEditing then
  begin
    FRefreshDeferred := True;
    Exit;
  end;


  shf := nil;
  c := Screen.Cursor;
  Screen.Cursor := crHourglass;
  try
    Inc( FIgnoreChanges );
    try
      Selected := nil;
    finally Dec( FIgnoreChanges );
    end;
    Items.Clear;

    if Assigned( FBaseFolder.IdList ) then
      dw := ShellGetFolderFromIdList( FBaseFolder.IdList, shf )
    else
      dw := ShellGetDesktopFolder( shf );
    if ( dw <> S_OK ) then
      RaiseSysError( dw );
    FillTree( shf, nil );

    try
      if Assigned( SelectedFolder.IdList ) then
        DoSetSelectedIdList( SelectedFolder.IdList )
      else
        Selected := Items[ 0 ];
    finally
    end;

    Synchronize( False );
  finally
    if Assigned( shf ) then
      shf.Release;
    Screen.Cursor := c;
  end;

  if ( stoDynamicRefresh in Options ) and not ( csDesigning in ComponentState ) then
    InstallChangeHandlersForAllLocalDrives;
end;                                        {TRzCustomShellTree.FillItems}


{ Applies changes to all nodes already visible. }


procedure TRzCustomShellTree.RefreshNodes;
begin
  RefreshNodes( Items.GetFirstNode );
end;

// in the current RefreshNodes replace Items.GetFirstNode with nodeToRefresh

procedure TRzCustomShellTree.RefreshNodes( Node: TTreeNode );
var
  oldsel: TTreeNode;
  function GetNodeIShellFolder( n: TTreeNode ): IShellFolder_NRC;
  begin
    if NodeHasData( n ) then
      Result := GetDataFromNode( n ).ThisIShf
    else
      Result := nil;
  end;

  function IsDescendantOf( aAncestor: TTreeNode; aPossibleDescendant: TTreeNode ): Boolean;
  var
    cn: TTreeNode;
  begin
    if aAncestor = aPossibleDescendant then
      Result := True
    else
    begin
      Result := False;
      cn := aAncestor.GetFirstChild;
      while Assigned( cn ) and not Result do
      begin
        if cn = aPossibleDescendant then
          Result := True
        else
        begin
          Result := IsDescendantOf( cn, aPossibleDescendant );
          cn := cn.GetNextSibling;
        end;
      end;
    end;
  end;                                      {IsDescendantOf - local}

  procedure DoRefresh( aNode: TTreeNode );
  var
    cn: TTreeNode;
    aNodeIShf, tmpishf: IShellFolder_NRC;
    I, At: Integer;
    curidl: PItemIdList;
    dw: DWORD;
    ienum: IEnumIdList_NRC;
    enumidl: PItemIdList;
    dummy: DWORD;
    currentPidls: TRzPidlList;
    newPidls: TRzPidlList;
  begin
    tmpishf := nil;
    ienum := nil;
    enumidl := nil;
    currentPidls := nil;
    newPidls := nil;
    try
      if aNode.HasChildren and ( aNode.getFirstChild = nil ) then
        Exit;

      aNodeIShf := GetNodeIShellFolder( aNode );
      if not Assigned( aNodeIshf ) then
        Exit;

      currentPidls := TRzPidlList.Create;
      currentPidls.Sorted := False;
      currentPidls.Capacity := aNode.Count;
      currentPidls.ShellFolder := aNodeIShf;

      newPidls := TRzPidlList.Create;
      newPidls.Sorted := False;
      newPidls.Capacity := currentPidls.Capacity;
      newPidls.Malloc := RzShellUtils.ShellIMalloc;

      // get the pidls of the currently displayed nodes
      cn := aNode.GetFirstChild;
      while Assigned( cn ) do
      begin
        if NodeHasData( cn ) then
          currentPidls.AddObject( GetDataFromNode( cn ).RelativeIdList, cn );
        cn := aNode.GetNextChild( cn );
      end;
      currentPIDLS.Sorted := True;

      // get the pidls of the new items
      dw := aNodeIShf.EnumObjects( 0,
        SHCONTF_FOLDERS or
        SHCONTF_INCLUDEHIDDEN_FLAG[ stoShowHidden in Options ] or
        SHCONTF_NONFOLDERS_FLAG[ stoIncludeNonFolders in Options ],
        ienum );
      if dw = S_OK then
        dw := ienum.Next( 1, enumidl, PInteger( @dummy ) );
      while dw = S_OK do
      begin
        newPidls.Add( enumidl );
        enumidl := nil;
        dw := ienum.Next( 1, enumidl, PInteger( @dummy ) );
      end;

      // Find those items which are in both lists, and discard them.  Items in
      // NEWPIDLS are automatically Freed when deleted because the .MALLOC property is assigned.
      for I := newPidls.Count - 1 downto 0 do
      begin
        At := currentPIDLS.IndexOf( NewPidls[ I ] );
        if not ( At = -1 ) then             // If the item exists in both loops.
        begin
          newPIDLS.Delete( I );
          currentPIDLS.Delete( At );
        end;
      end;

      // Now, CurrentPIDLS contains a list of those files which were deleted,
      // and NEWPIDLS contains a list of those PIDLS which were added, take the appropriate action.
      for I := currentPidls.Count - 1 downto 0 do
      begin
        if IsDescendantOf( TTreeNode( currentPidls.Objects[ I ] ), oldsel ) then
          oldsel := TTreeNode( currentPidls.Objects[ I ] ).Parent;
        TTreeNode( currentPidls.Objects[ I ] ).Delete;
      end;

      // Add new files
      for I := 0 to newPidls.Count - 1 do
      begin
        curidl := newPidls[ I ];
        AddNewShellNode( aNodeIShf, GetDataFromNode( aNode ).AbsoluteIdList, aNode, curidl );
      end;

      if newPidls.Count > 0 then
        SortNode( aNode );

      // free the lists first to reduce memory consumption for the following recursive calls
      currentPidls.Free;
      currentPidls := nil;
      newPidls.Free;
      newPidls := nil;

      // Call ourSelf recursively for all aNode's children
      cn := aNode.GetFirstChild;
      while Assigned( cn ) do
      begin
        DoRefresh( cn );
        cn := aNode.GetNextChild( cn );
      end;
    finally
      currentPidls.Free;
      newPidls.Free;
      if Assigned( ienum ) then
        ienum.Release;
      if Assigned( tmpishf ) then
        tmpishf.Release;
      ShellMemFree( enumidl );
    end;
  end;                                      {DoRefresh - local}

var
  c: TCursor;

begin {= TRzCustomShellTree.RefreshNodes =}

  if FDeferRefresh or IsEditing then
  begin
    FRefreshDeferred := True;
    Exit;
  end;

  if Items.Count = 0 then
  begin                                     // No items at all so just fill them and be done with it
    FillItems;
    Exit;
  end;

  c := Screen.Cursor;
  Screen.Cursor := crHourglass;
  FillStart;
  oldsel := nil;
  try
    Inc( FIgnoreChanges );
    try
      oldsel := Selected;
      Selected := nil;
      if Node <> nil then
      begin
        try
          Items.BeginUpdate;
          DoRefresh( Node );
        finally
          Items.EndUpdate;
        end;
      end;

    finally
      Dec( FIgnoreChanges );

      if Assigned( oldsel ) then
      begin
        FQuickSelect := True;
        oldsel.Selected := True;
      end;
    end;
  finally
    Screen.Cursor := c;
    FillComplete;
  end;
end; {= TRzCustomShellTree.RefreshNodes =}


procedure TRzCustomShellTree.GoUp( Levels: Integer );
var
  I: Integer;
  target: TTreeNode;
begin
  if Assigned( Selected ) and Assigned( Selected.Parent ) then
  begin
    target := Selected;
    I := Levels;
    while ( I > 0 ) and Assigned( target.Parent ) do
    begin
      target := target.Parent;
      Dec( I );
    end;
    FQuickSelect := True;
    target.Selected := True;
  end;
end;


function TRzCustomShellTree.CreateNewFolder( EditNow: Boolean ): Boolean;
var
  spn, newname: string;
  I: Integer;
begin
  spn := SelectedPathname;
  FIgnoreNextChangeNotify := True;
  Result := RzShCreateNewFolder( spn, newname );
  if Result then
  begin
    if Assigned( SelectedFolder ) and
      ( ( SelectedFolder.IdList = nil ) or ( PWord( SelectedFolder.IdList )^ = 0 ) ) then
      SleepWait( 2100 )
    else
      SleepWait( 1000 );
    RefreshNodes;
    ExpandNode( Selected );
      { This ExpandNode line is necessary to work around a bug in the Windows tree view control.
        If a folder has one child, and that child is deleted and then a new child is created - a
        TVN_ITEMEXPANDING notification is not sent by the following Expand call. So we fill in the
        children of 'Selected' with this ExpandNode call before we expand the node. }
    Selected.Expand( False );
    Result := True;
    for I := 0 to Selected.Count - 1 do
      if Selected.Item[ I ].Text = newname then
      begin
        Selected.Item[ I ].EditText;
        Break;
      end;
  end
  else
    Result := False;
end;                                        {TRzCustomShellTree.CreateNewFolder}


function TRzCustomShellTree.DoCommandForNode( Node: TTreeNode; Cmd: PAnsiChar ): HResult;
var
  ld: TRzShellTreeData;
  icm: IContextMenu_NRC;
  hm: HMENU;
  pidl: PItemIdList;
begin
  if not Assigned( Node.Parent ) or not NodeHasData( Node ) then
  begin
    Result := S_False;
    Exit;
  end;
  icm := nil;
  hm := 0;
  ld := GetDataFromNode( Node );
  try
    pidl := ld.RelativeIdList;
    OleCheck( ld.ParentIshf.GetUIObjectOf( GetParentHWND, 1, pidl, IID_IContextMenu, nil, Pointer( icm ) ) );
    Result := ExecuteContextMenuItem( GetParentHWND, icm, Cmd );
    RefreshNodes;
  finally
    if Assigned( icm ) then
      icm.Release;
    if ( hm <> 0 ) then
      DestroyMenu( hm );
  end;
end;                                        {TRzCustomShellTree.DoCommandForNode}

{
 Desktop
  My Computer
   C:\
     Folder

[0.Desktop][1.My Computer][2.C:\][3.Folder]

curitem := [0.Desktop]
basenode := Node(nil)
found := FindNodeWithIdList( nil, [0.Desktop] )

if found then
  found.Expand
  curitem := [1.My Computer]
  basenode := found  Node(Desktop)
  found := FindNodeWithIdList( basenode Node(Desktop), curitem [1.My Computer] )
  if found then
    found.Expand
    curitem := [2.C:\]
    basenode := found  Node(My Computer)
    found := FindNodeWithIdList( basenode Node(MyComputer), curitem [2.C:\] )
    if found then
      found.Expand
      curitem := [3.Folder]
      basenode := found  Node(C:\)
      found := FindNodeWithIdList( basenode Node(C:\), curitem [3.Folder] )
      if found then
        node.Selected := True

curitem := pa[0]
basenode := nil
while True do
begin
  found := FindNodeWithIdList( basenode, curitem );
  if Assigned(found) then
  begin
    found.Expand
    basenode := found
  end
  else
    Exit - not found
  curitem := next item
  basenode := found
end;
found.Selected := True;
}

function TRzCustomShellTree.FindNodeWithIdList( baseNode: TTreeNode; pidl: PItemIdList ): TTreeNode;
var
  curChild: TTreeNode;
  curData: TRzShellTreeData;
begin
  Result := nil;
  if ( baseNode = nil ) then
  begin
    curChild := Items.GetFirstNode;
    while Assigned( curChild ) and not NodeHasData( curChild ) do
      curChild := curChild.GetNextSibling;
    Result := curChild;
  end
  else
  begin
    curChild := baseNode.GetFirstChild;
    while Assigned( curChild ) do
    begin
      curData := GetDataFromNode( curChild );
      if Assigned( curData ) then
      begin
        if curData.ParentIShf.CompareIDs( 0, curData.RelativeIdList, pidl ) = 0 then
        begin
          Result := curChild;
          Exit;
        end;
      end;
      curChild := baseNode.GetNextChild( curChild );
    end;
  end;
end;                                        {TRzCustomShellTree.FindNodeWithIdList}

procedure TRzCustomShellTree.Synchronize( afApplyToGroup: Boolean );
begin
  if afApplyToGroup then
  begin
    if Assigned( FShellList ) then
      FShellList.Synchronize( False );
    if Assigned( FShellCombo ) then
      FShellCombo.Synchronize( False );
  end;
  if FTimer.Enabled then
    TimerElapsed( FTimer );
end;                                        {TRzCustomShellTree.Synchronize}


function TRzCustomShellTree.GetDragDropAttributesForNode( node: TTreeNode ): DWORD;
var
  ishfparent: IShellFolder_NRC;
  relpidl: PItemIdList;
  tmpparentpidl, tmprelpidl: PItemIdList;
begin
  Result := DRAGFLAGS;
  if not NodeHasData( node ) then
    Exit;

  tmpparentpidl := nil;
  tmprelpidl := nil;
  try
    if Assigned( node.Parent ) then
    begin
      ishfparent := GetDataFromNode( node.Parent ).ThisIShf;
      relpidl := GetDataFromNode( node ).RelativeIdList;
    end
    else                                    // not Assigned(node.Parent)
    begin
      GetDesktopParentChildPidls( tmpparentpidl, tmprelpidl );
      if Failed( ShellGetFolderFromIdList( tmpparentpidl, ishfparent ) ) then
        Exit;
      relpidl := tmprelpidl;
    end;

    if Failed( ishfparent.GetAttributesOf( 1, relpidl, Result ) ) then
      Result := DRAGFLAGS;
  finally
    ShellMemFree( tmpparentpidl );
    ShellMemFree( tmprelpidl );
  end;
end;                                        {TRzCustomShellTree.GetDragDropAttributesForNode}

{== END TRzCustomShellTree =====================================}



{-- TRzCustomShellList -----------------------------------------}

  {-- Utilities local to TRzCustomShellList ---}

procedure GetCharsUpToNextCharDB( var aPos: Integer; aSource: string; var aDest: string; aCharToFind: Char );
begin
  aDest := '';
  while ( aSource[ aPos ] <> aCharToFind ) and ( aPos <= Length( aSource ) ) do
    CopyCharDB( aPos, aSource, aDest );
end;                                        {GetCharsUpToNextCharDB}

procedure ExtensionsToTStrings( aExtensions: string; aExts: TStrings );
var
  pos: Integer;
  ext: string;
begin
  pos := 1;
  while ( pos <= Length( aExtensions ) ) do
  begin
    GetCharsUpToNextCharDB( pos, aExtensions, ext, ';' );
    Inc( pos );
    if ( ext <> '' ) then
      aExts.Add( ext );
  end;
end;                                        {ExtensionsToTStrings}

procedure FilterInitLookupTable( var aLookupTable: Pointer );
begin
  aLookupTable := TStringList.Create;
  TStringList( aLookupTable ).Sorted := True;
  TStringList( aLookupTable ).Duplicates := dupIgnore;
end;                                        {FilterInitLookupTable}

function FilterAddFilespecToLookupTable( aLookupTable: Pointer; aFilespec: string ): Boolean;
var
  h: THandle;
  w32fd: TWin32FindData;
  uCode: UINT;
begin
  uCode := SetErrorMode( 0 );
  try
    SetErrorMode( uCode or SEM_FAILCRITICALERRORS );
    h := Windows.FindFirstFile( PChar( aFileSpec ), w32fd );
    if ( h <> INVALID_HANDLE_VALUE ) then
    begin
      repeat
        if ( w32fd.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY ) = 0 then
          TStringList( aLookupTable ).Add( AnsiUppercase( w32fd.cFileName ) );
      until not Windows.FindNextFile( h, w32fd );
      Windows.FindClose( h );
      Result := True;
    end
    else
      Result := GetLastError = ERROR_FILE_NOT_FOUND;
  finally
    SetErrorMode( uCode );
  end;
end;                                        {FilterAddFilespecToLookupTable}

function FilterLookup( aLookupTable: Pointer; const aValue: string ): Bool;
begin
  Result := ( TStringList( aLookupTable ).IndexOf( AnsiUppercase( aValue ) ) >= 0 );
end;                                        {FilterLookup}

procedure FilterFreeLookupTable( aLookupTable: Pointer );
begin
  TStrings( aLookupTable ).Free;
end;                                        {FilterFreeLookupTable}

procedure WriteCurrentColWidths( aLv: TRzCustomShellList; aStream: TMemoryStream );
var
  I, w: Integer;
begin
  aStream.Clear;
  w := aLv.Columns.Count;
  aStream.WriteBuffer( w, Sizeof( Integer ) );
  for I := 0 to aLv.Columns.Count - 1 do
  begin
    w := aLv.Columns[ I ].Width;
    aStream.WriteBuffer( w, Sizeof( Integer ) );
  end;
end;                                        {WriteCurrentColWidths - local}

var
  g_LastColWidths: TMemoryStream = nil;     // All shell lists share the same column widths of course!
  g_LastRegistryColData: packed array[ 0..10 ] of Word;
  g_LastRegistryColDataSize: Integer = 0;

  // Creates a data object referencing all the selected items in the given shell list
  // D2/D3/BCB
type
  TDataObject_shlv = class( TObject )
  private
    mcRefs: Integer;
    mhCFHDrop: THandle;
    mhCFIdList: THandle;
    mFormats: TRzFormatEtcList;
  public
    constructor Create( shlv: TRzCustomShellList );
    destructor Destroy; override;
    procedure GenerateFormats( shlv: TRzCustomShellList );
         // -- IUnknown --
    function QueryInterface( const IID: TGUID; var Obj ): Integer; virtual; stdcall;
    function AddRef: Integer; virtual; stdcall;
    function Release: Integer; virtual; stdcall;
         // -- IDataObject --
    function GetData( const formatetcIn: TFormatEtc; var medium: TStgMedium ): HResult; virtual; stdcall;
    function GetDataHere( const formatetc: TFormatEtc; var medium: TStgMedium ): HResult; virtual; stdcall;
    function QueryGetData( const formatetc: TFormatEtc ): HResult; virtual; stdcall;
    function GetCanonicalFormatEtc( const formatetc: TFormatEtc; var formatetcOut: TFormatEtc ): HResult; virtual; stdcall;
    function SetData( const formatetc: TFormatEtc; var medium: TStgMedium; fRelease: BOOL ): HResult; virtual; stdcall;
    function EnumFormatEtc( dwDirection: Longint; var enumFormatEtc: IEnumFormatEtc_NRC ): HResult; virtual; stdcall;
    function DAdvise( const formatetc: TFormatEtc; advf: Longint; const advSink: Pointer; var dwConnection: Longint ): HResult; virtual; stdcall;
    function DUnadvise( dwConnection: Longint ): HResult; virtual; stdcall;
    function EnumDAdvise( var enumAdvise: IEnumStatData ): HResult; virtual; stdcall;
  end;

constructor TDataObject_shlv.Create( shlv: TRzCustomShellList );
begin
  {$IFDEF PTDEBUG}
  Inc( g_DataObject_shlv );
  {$ENDIF}
  inherited Create;
  mFormats := TRzFormatEtcList.Create;      //Init( [CF_IDLIST, CF_HDROP] );
  GenerateFormats( shlv );
  if mhCFIdList <> 0 then
    mFormats.Add( [ CF_IDLIST ] );
  if mhCFHDrop <> 0 then
    mFormats.Add( [ CF_HDROP ] );
end;

destructor TDataObject_shlv.Destroy;
begin
  {$IFDEF PTDEBUG}
  Dec( g_DataObject_shlv );
  {$ENDIF}
  if ( mhCfHdrop <> 0 ) then
    GlobalFree( mhCfHDrop );
  if ( mhCfIdList <> 0 ) then
    GlobalFree( mhCfIdList );
  mFormats.Free;
  inherited Destroy;
end;

procedure TDataObject_shlv.GenerateFormats( shlv: TRzCustomShellList );
var
  l: TList;
  sl: TStringList;
  I: Integer;
  TheItem: TListItem;
begin
  l := nil;
  sl := nil;
  try
    l := TList.Create;
    TheItem := shlv.Selected;
    while Assigned( TheItem ) do
    begin
      if shlv.ItemHasData( TheItem ) then
        l.Add( shlv.GetDataFromItem( TheItem ).RelativeIdList );
      TheItem := shlv.GetNextItem( TheItem, sdAll, [ isSelected ] );
    end;

    if l.Count > 0 then
    begin
      Create_CFIDLIST_HGlobal( Pointer( shlv.Folder.IdList ), l, mhCfIdList );

      sl := TStringList.Create;
      for I := 0 to l.Count - 1 do
      begin
        sl.Add( ShellGetFriendlyNameFromIdList( shlv._IShellFolder, l[ I ], fnForParsing ) );
      end;
      Create_CFHDROP_HGlobal( sl, mhCfHDrop );
    end;
  finally
    l.Free;
    sl.Free;
  end;
end;

function TDataObject_shlv.QueryInterface( const IID: TGUID; var Obj ): Integer;
begin
  if IsEqualIID( iid, IID_IUnknown ) or IsEqualIID( iid, IID_IDataObject ) then
  begin
    Pointer( obj ) := Self;
    AddRef;
    Result := S_OK;
  end
  else
  begin
    Pointer( obj ) := nil;
    Result := Integer( E_NOINTERFACE );
  end;
end;

function TDataObject_shlv.AddRef: Integer;
begin
  Inc( mcRefs );
  Result := mcRefs;
end;

function TDataObject_shlv.Release: Integer;
begin
  Dec( mcRefs );
  Result := mcRefs;
  if ( mcRefs = 0 ) then
    Free;
end;

function TDataObject_shlv.GetData( const formatetcIn: TFormatEtc; var medium: TStgMedium ): HResult;
begin
  Result := QueryGetData( formatetcIn );
  if Failed( Result ) then
    Exit;

  medium.tymed := TYMED_HGLOBAL;
  medium.unkForRelease := nil;

  if formatetcIn.cfFormat = CF_IDLIST then
    medium.hGlobal := DuplicateHGlobal( mhCfIdList )
  else if formatetcIn.cfFormat = CF_HDROP then
    medium.hGlobal := DuplicateHGlobal( mhCfHDrop )
  else
  begin
    Result := DV_E_FORMATETC;
    Exit;
  end;
  if medium.hGlobal = 0 then
    Result := E_OUTOFMEMORY
  else
    Result := S_OK;
end;

function TDataObject_shlv.GetDataHere( const formatetc: TFormatEtc; var medium: TStgMedium ): HResult;
begin
  Result := E_NOTIMPL;
end;

function TDataObject_shlv.QueryGetData( const formatetc: TFormatEtc ): HResult;
begin
  Result := mFormats.FormatSupported( formatetc );
end;

function TDataObject_shlv.GetCanonicalFormatEtc( const formatetc: TFormatEtc; var formatetcOut: TFormatEtc ): HResult;
begin
  formatetcOut.ptd := nil;
  Result := E_NOTIMPL;
end;

function TDataObject_shlv.SetData( const formatetc: TFormatEtc; var medium: TStgMedium; fRelease: BOOL ): HResult;
begin
  Result := E_NOTIMPL;
end;

function TDataObject_shlv.EnumFormatEtc( dwDirection: Longint; var enumFormatEtc: IEnumFormatEtc_NRC ): HResult;
begin
  if dwDirection = DATADIR_GET then
  begin
    enumFormatEtc := mFormats.CreateIEnumFormatEtc;
    enumFormatEtc.AddRef;
    Result := S_OK;
  end
  else
  begin
    Pointer( enumFormatEtc ) := nil;
    Result := E_NOTIMPL;
  end;
end;

function TDataObject_shlv.DAdvise( const formatetc: TFormatEtc; advf: Longint; const advSink: Pointer; var dwConnection: Longint ): HResult;
begin
  Result := OLE_E_ADVISENOTSUPPORTED;
end;

function TDataObject_shlv.DUnadvise( dwConnection: Longint ): HResult;
begin
  Result := OLE_E_ADVISENOTSUPPORTED;
end;

function TDataObject_shlv.EnumDAdvise( var enumAdvise: IEnumStatData ): HResult;
begin
  Result := OLE_E_ADVISENOTSUPPORTED;
end;


  // A DropTarget object for the list view.
  // D2/D3/BCB
type
  TDropTarget_shlv = class( TObject )
  private
    FOwner: TRzCustomShellList;
    mcRefs: Integer;
  protected
         // -- IUnknown -----------------
    function QueryInterface( const IID: TGUID; var Obj ): Integer; virtual; stdcall;
    function AddRef: Integer; virtual; stdcall;
    function Release: Integer; virtual; stdcall;

         // -- IDropTarget --------------
    function DragEnter( const dataObj: IDataObject_NRC; grfKeyState: Longint; pt: TPoint; var dwEffect: Longint ): HResult; virtual; stdcall;
    function DragOver( grfKeyState: Longint; pt: TPoint; var dwEffect: Longint ): HResult; virtual; stdcall;
    function DragLeave: HResult; virtual; stdcall;
    function Drop( const dataObj: IDataObject_NRC; grfKeyState: Longint; pt: TPoint; var dwEffect: Longint ): HResult; virtual; stdcall;
  public
    constructor Create( aOwner: TRzCustomShellList );
    destructor Destroy; override;
  end;

constructor TDropTarget_shlv.Create( aOwner: TRzCustomShellList );
begin
  {$IFDEF PTDEBUG}
  Inc( g_DropTarget_shlv );
  {$ENDIF}
  inherited Create;
  FOwner := aOwner;
end;

destructor TDropTarget_shlv.Destroy;
begin
  {$IFDEF PTDEBUG}
  Dec( g_DropTarget_shlv );
  {$ENDIF}
  inherited Destroy;
end;

function TDropTarget_shlv.QueryInterface( const IID: TGUID; var Obj ): Integer;
begin
  if IsEqualIID( iid, IID_IUnknown ) or IsEqualIID( iid, IID_IDropTarget ) then
  begin
    Pointer( obj ) := Self;
    AddRef;
    Result := S_OK;
  end
  else
  begin
    Pointer( obj ) := nil;
    Result := Integer( E_NOINTERFACE );
  end;
end;

function TDropTarget_shlv.AddRef: Integer;
begin
  Inc( mcRefs );
  Result := mcRefs;
end;

function TDropTarget_shlv.Release: Integer;
begin
  Dec( mcRefs );
  Result := mcRefs;
  if ( mcRefs = 0 ) then
    Free;
end;

function TDropTarget_shlv.DragEnter( const dataObj: IDataObject_NRC; grfKeyState: Longint; pt: TPoint; var dwEffect: Longint ): HResult;
begin
  Result := FOwner.OnDropTarget_DragEnter( dataObj, grfKeyState, pt, dwEffect );
end;

function TDropTarget_shlv.DragOver( grfKeyState: Longint; pt: TPoint; var dwEffect: Longint ): HResult;
begin
  Result := FOwner.OnDropTarget_DragOver( grfKeyState, pt, dwEffect );
end;

function TDropTarget_shlv.DragLeave: HResult;
begin
  Result := FOwner.OnDropTarget_DragLeave;
end;

function TDropTarget_shlv.Drop( const dataObj: IDataObject_NRC; grfKeyState: Longint; pt: TPoint; var dwEffect: Longint ): HResult;
begin
  Result := FOwner.OnDropTarget_Drop( dataObj, grfKeyState, pt, dwEffect );
end;


{-- TRzCustomShellList proper ----------------------------------}

constructor TRzCustomShellList.Create( aOwner: TComponent );
begin
  inherited;
  {&RCI}
  FillLastColumn := False;

  FFolder := TRzShellLocator.Create;
  FFolder.OnChange := HandleOnFolderChanged;

  FTimer := TTimer.Create( Self );
  FTimer.Enabled := False;
  FTimer.OnTimer := TimerElapsed;

  FOptions := [ sloAutofill, sloNonFilesystemAncestors, sloDefaultKeyHandling, sloContextMenus, sloDynamicRefresh,
                sloOleDrag, sloOleDrop, sloFolderContextMenu, sloShowHidden, sloFilesCanBeFolders ];

  SmallImages := TImageList.CreateSize( 16, 16 );
  LargeImages := TImageList.CreateSize( 32, 32 );
  InitImageLists;

  IconOptions.AutoArrange := True;

  AllocBy := 256;

  FSortColumn := 1;

  RzDeviceChangeHandler.Add( DeviceChangeDetected );
  {&RV}
end; {= TRzCustomShellList.Create =}


destructor TRzCustomShellList.Destroy;
{$IFDEF PTDEBUG}
var
  listNodes, dbgNodes: Integer;
{$ENDIF}
begin
  RzDeviceChangeHandler.Remove( DeviceChangeDetected );
  if Assigned( FIDropTarget ) then
  begin
    FIDropTarget.Release;
    FIDropTarget := nil;
  end;
  FTimer.Free;
  TObject( FChangeHandlerThread ).Free;
  FChangeHandlerThread := nil;
  SmallImages.Free;
  LargeImages.Free;
  if Assigned( FIShf ) then
    FIShf.Release;
  ShellMemFree( FIShfPidl );
  if Assigned( FIShellDetails ) then
    FIShellDetails.Release;
  {$IFDEF PTDEBUG}
  listNodes := g_ListNodes;
  dbgNodes := mdbgNodes;
  {$ENDIF}
  FFolder.Free;
  if Assigned( FFilterLookupTable ) then
    FilterFreeLookupTable( FFilterLookupTable );
  if Assigned( FFilterExtensions ) then
    FFilterExtensions.Free;
  inherited;
  {$IFDEF PTDEBUG}
  if listNodes - g_ListNodes <> dbgNodes then
  begin
    MessageBeep( UINT( -1 ) );
    Windows.MessageBox( 0, PChar( Format( 'TRzCustomShellList: %d nodes leaked!', [ listNodes - g_ListNodes ] ) ), 'Debug', MB_OK );
  end;
  {$ENDIF}
end; {= TRzCustomShellList.Destroy =}


function TRzCustomShellList.CreateNewFolder( EditNow: Boolean ): Boolean;
  {IE4 and later Desktop dir behaves strangly. This code makes sure the system shell object
   has detected the change before we call RefreshItems.}
var
  spn, newname: string;
begin
  spn := Folder.PathName;
  FIgnoreNextChangeNotify := True;
  if RzShCreateNewFolder( spn, newname ) then
  begin
    if ( Folder.IdList = nil ) or ( PWord( Folder.IdList )^ = 0 ) then
      SleepWait( 2100 );
    RefreshItems;
    if EditNow then
    begin
      FDeferredEditName := newname;
      PostMessage( Handle, RZSH_AM_DEFERRED_EDIT, 0, 0 );
    end;
    Result := True;
  end
  else
    Result := False;
end;


procedure TRzCustomShellList.FillItems;
var
  shf: IShellFolder_NRC;
  c: TCursor;
begin
  if not HandleAllocated then
    Exit;

  if FDeferRefresh or IsEditing then
  begin
    FRefreshDeferred := True;
    Exit;
  end;

  shf := nil;
  c := Screen.Cursor;
  Screen.Cursor := crHourglass;
  Selected := nil;
  Items.BeginUpdate;
  try
    Items.Clear;
    Items.EndUpdate;

    Items.BeginUpdate;
    ShellGetFolderFromIdList( FFolder.IdList, shf );

    FilterPreApply;
    FillList( shf, FFolder.IdList );
    FilterPostApply;
  finally
    if Assigned( shf ) then
      shf.Release;
    Items.EndUpdate;
    Screen.Cursor := c;
  end;

  SortList;
  if Items.Count > 0 then
    Items[ 0 ].Focused := True;

  if ViewStyle in [ vsIcon, vsSmallIcon ] then
  begin
    Perform( WM_VSCROLL, SB_TOP, 0 );
    Perform( WM_HSCROLL, SB_TOP, 0 );
  end;

  if ( sloDynamicRefresh in Options ) and not ( csDesigning in ComponentState ) then
  begin
    InstallChangeHandler;
  end;
end; {= TRzCustomShellList.FillItems =}


procedure TRzCustomShellList.GoUp( Levels: Integer );
var
  pa: TRzIdListArray;
begin
  if sloDontChangeFolder in Options then
    Exit;
  pa := nil;
  try
    if Assigned( FShellTree ) then
      FShellTree.GoUp( Levels )
    else if Assigned( FShellCombo ) then
      FShellCombo.GoUp( Levels )
    else
    begin
      pa := TRzIdListArray.Create( Folder.IdList );
      if ( pa.ItemCount > 0 ) then
        Folder.IdList := pa.GoUp( Levels );
    end;
  finally
    pa.Free;
  end;
end; {= TRzCustomShellList.GoUp =}


{
  If Item=nil then use all selected items. If no items then create an item for the parent folder.
  If Item<>nil then just use that item.

  If linked to a treeview then
    Tell the treeview the new pidl
  else if linked to a combo then
    tell the combo the new pidl
  else
}

procedure TRzCustomShellList.OpenItem( Item: TListItem );
var
  c: TCursor;
  ld: TRzShellListData;
  newishf: IShellFolder_NRC;
  newpidl: PItemIdList;

  procedure DoDefaultMenuItem;
  var
    hm: HMENU;
    icm: IContextMenu_NRC;
    id: Integer;
  begin
    hm := 0;
    icm := nil;
    OleCheck( CreateAndPopulateContextMenu( GetValidParentHWND( Self ), FIShf, 1, @ld.RelativeIdList, CMDOFFSET, CMF_EXPLORE, hm, icm ) );
    try
      id := GetMenuDefaultItem( hm, 0, 0 );
      if ( id >= CMDOFFSET ) then
        ExecuteContextMenuItem( GetValidParentHWND( Self ), icm, MakeIntResourceA( id - CMDOFFSET ) );
    finally
      if Assigned( icm ) then
        icm.Release;
      if ( hm <> 0 ) then
        DestroyMenu( hm );
    end;
  end;                                      {DoDefaultMenuItem - local}

  procedure DoAssignment( pidl: PItemIdList );
  begin
    if not ( sloDontChangeFolder in Options ) then
    begin
      Selected := nil;
      Items.BeginUpdate;
      try
        Items.Clear;
      finally
        Items.EndUpdate;
      end;

      if Assigned( FShellTree ) then
        FShellTree.SelectedFolder.IdList := pidl
      else if Assigned( FShellCombo ) then
        FShellCombo.SelectedFolder.IdList := pidl
      else
        Folder.IdList := pidl;
    end;
  end;                                      {DoAssignment - local}

  function AttemptLnkProcessing( ald: TRzShellListData ): Boolean;
  var
    ld: TLinkData;
    pidl: PItemIdList;
  begin
    Result := False;
    FillChar( ld, SizeOf( ld ), 0 );
    ld.noUI := True;
    if Succeeded( ResolveShortcut( ald.PathName, ld, False ) ) then
    begin
      if ( ld.w32fd.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY ) <> 0 then
      begin
        pidl := nil;
        try
          if Failed( ShellGetIdListFromPath( ld.PathName, pidl ) ) then
            Exit;
          DoAssignment( pidl );
        finally
          ShellMemFree( pidl );
        end;
        Result := True;
      end;
    end;
  end;                                      {AttemptLnkProcessing - local}


begin {= TRzCustomShellList.OpenItem =}
  Synchronize( True );
  if ItemHasData( Item ) then
  begin
    c := Screen.Cursor;
    Screen.Cursor := crHourglass;
    newpidl := nil;
    newishf := nil;
    try
      ld := GetDataFromItem( Item );
      newpidl := ConcatIdLists( nil, FIShfPidl, ld.RelativeIdList );

      if not ld.IsFolder then
      begin  // Can't bind to object, so attempt default menu item processing
        if ld.IsLnkShortcut and AttemptLnkProcessing( ld ) then
          Exit;
        if DblClickOpen then
          DoDefaultMenuItem;
        Exit;
      end;
      DoAssignment( newpidl );
    finally
      ShellMemFree( newpidl );
      if Assigned( newishf ) then
        newishf.Release;
      Screen.Cursor := c;
    end;
  end
  else
    DblClickOpen;
end; {= TRzCustomShellList.OpenItem =}


procedure TRzCustomShellList.OpenSelectedItems;
var
  pidls: TList;
  I, defid: Integer;
  icm: IContextMenu_NRC;
  hm: HMENU;
  theItem: TListItem;
begin
  Synchronize( True );
  pidls := nil;
  icm := nil;
  hm := 0;
  try
    pidls := TList.Create;

    theItem := Selected;
    while Assigned( theItem ) do
    begin
      if ItemHasData( theItem ) then
        pidls.Add( CopyIdList( nil, GetDataFromItem( theItem ).RelativeIdList ) );
      theItem := GetNextItem( theItem, sdAll, [ isSelected ] );
    end;

    if pidls.Count > 1 then
    begin
      if DblClickOpen then
      begin
        OleCheck( CreateAndPopulateContextMenu( GetValidParentHWND( Self ), FIShf,
          pidls.Count, PPItemIdList( pidls.List ),
          CMDOFFSET, CMF_EXPLORE, hm, icm ) );
        defid := Windows.GetMenuDefaultItem( hm, 0, 0 );
        if ( defid >= CMDOFFSET ) then
          ExecuteContextMenuItem( GetValidParentHWND( Self ), icm, MakeIntResourceA( defid - CMDOFFSET ) );
      end;
    end
    else if Assigned( Selected ) then
      OpenItem( Selected );
  finally
    for I := 0 to pidls.Count - 1 do
      ShellMemFree( pidls[ I ] );
    if Assigned( icm ) then
      icm.Release;
    if ( hm <> 0 ) then
      DestroyMenu( hm );
    pidls.Free;
  end;
end; {= TRzCustomShellList.OpenSelectedItems =}


procedure TRzCustomShellList.ProcessMenu( Item: TListItem; Pt: TPoint );
var
  ld: TRzShellListData;
  icm: IContextMenu_NRC;
  Resultid: Integer;
  dw: DWORD;
  renameBit: DWORD;
  hm: HMENU;
begin
  if not ItemHasData( Item ) then
    Exit;

  ld := GetDataFromItem( Item );
  if ReadOnly then
    renameBit := 0
  else
    renameBit := CMF_CANRENAME;

  dw := CreateAndPopulateContextMenu( GetValidParentHWND( Self ), FIShf, 1, @ld.FRelPidl, CMDOFFSET,
    CMF_EXPLORE or renameBit, hm, icm );

  FDeferRefresh := True;
  FRefreshDeferred := False;
  try
    if Failed( dw ) then
      Exit;
    Pt := ClientToScreen( Pt );
    FActiveContextMenu := icm;
    Resultid := Integer( TrackPopupMenu( hm, TPM_LEFTALIGN or TPM_RETURNCMD or TPM_RIGHTBUTTON, Pt.X, Pt.Y, 0, Handle, nil ) );
    if ( Resultid >= CMDOFFSET ) then
      if Resultid - CMDOFFSET = ICM_RENAME then
        Item.EditCaption
      else
      begin
        ExecuteContextMenuItem( GetValidParentHWND( Self ), icm, MakeIntResourceA( Resultid - CMDOFFSET ) );
        if IsFolderNetworkShare then
          RefreshItems;
      end
    else if ( Resultid > 0 ) then
      ProcessSendTo( Resultid - 1 );
  finally
    if Assigned( icm ) then
      icm.Release;
    FActiveContextMenu := nil;
    if ( hm <> 0 ) then
      DestroyMenu( hm );
    if FRefreshDeferred then
    begin
      FRefreshDeferred := False;
      RefreshItems;
    end;
  end;
end; {= TRzCustomShellList.ProcessMenu =}


procedure TRzCustomShellList.ProcessMenuForAllSelected( Pt: TPoint );
var
  pidls: TList;
  hm: HMenu;
  I: Integer;
  icm: IContextMenu_NRC;
  dw: DWORD;
  renameBit: DWORD;
  focusedData: TRzShellListData;
  Resultid: Integer;
  theItem: TListItem;
begin
  pidls := nil;
  icm := nil;
  hm := 0;
  try
    pidls := TList.Create;

    theItem := Selected;
    while Assigned( theItem ) do
    begin
      if ItemHasData( theItem ) then
        pidls.Add( CopyIdList( nil, GetDataFromItem( theItem ).RelativeIdList ) );
      theItem := GetNextItem( theItem, sdAll, [ isSelected ] );
    end;

    if ( pidls.Count = 0 ) then
    begin
      Exit;
    end;

    dw := FIShf.GetUIObjectOf( GetValidParentHWND( Self ), pidls.Count, PPItemIdList( pidls.List )^,
      IID_IContextMenu, nil, Pointer( icm ) );
    if Failed( dw ) then
      Exit;
    hm := CreatePopupMenu;
    if ( hm = 0 ) then
      Exit;

    focusedData := GetDataFromItem( ItemFocused );
    if Assigned( focusedData ) and ( focusedData.PathName <> '' ) and not ReadOnly then
      renameBit := CMF_CANRENAME            // We only support renaming of filesystem items
    else
      renameBit := 0;

    OleCheck( icm.QueryContextMenu( hm, 0, CMDOFFSET, $7FFF, CMF_EXPLORE or renameBit ) );

   // We now have a valid context menu we can invoke and process...
    Pt := ClientToScreen( Pt );
    FActiveContextMenu := icm;
    Resultid := Integer( TrackPopupMenu( hm, TPM_LEFTALIGN or TPM_RETURNCMD or TPM_RIGHTBUTTON, Pt.X, Pt.Y, 0, Handle, nil ) );
    if ( Resultid >= CMDOFFSET ) then
      if Resultid - CMDOFFSET = ICM_RENAME then
      begin
        if Assigned( ItemFocused ) then
          ItemFocused.EditCaption;
      end
      else
      begin
        ExecuteContextMenuItem( GetValidParentHWND( Self ), icm, MakeIntResourceA( Resultid - CMDOFFSET ) );
        if IsFolderNetworkShare then
          RefreshItems;
      end
    else if ( Resultid > 0 ) then
      ProcessSendTo( Resultid - 1 );
  finally
    if ( hm <> 0 ) then
      DestroyMenu( hm );
    if Assigned( icm ) then
      icm.Release;
    FActiveContextMenu := nil;
    for I := 0 to pidls.Count - 1 do
      ShellMemFree( pidls[ I ] );
    pidls.Free;
  end;
end; {= TRzCustomShellList.ProcessMenuForAllSelected =}


procedure TRzCustomShellList.DoCommandForItem( Item: TListItem; Cmd: PAnsiChar );
var
  icm: IContextMenu_NRC;
begin
  icm := nil;
  try
    if Succeeded( GetUIObjectForItem( Item, IID_IContextMenu, Pointer( icm ) ) ) then
    begin
      ExecuteContextMenuItem( GetValidParentHWND( Self ), icm, Cmd );
      if IsFolderNetworkShare then
        RefreshItems;
    end;
  finally
    if Assigned( icm ) then
      icm.Release;
  end;
end;


procedure TRzCustomShellList.DoCommandForAllSelected( cmd: PAnsiChar ); // See help for a list of commands
var
  icm: IContextMenu_NRC;
begin
  icm := nil;
  try
    if Succeeded( GetUIObjectForAllSelected( IID_IContextMenu, Pointer( icm ) ) ) then
    begin
      ExecuteContextMenuItem( GetValidParentHWND( Self ), icm, cmd );
      if IsFolderNetworkShare then
        RefreshItems;
    end;
  finally
    if Assigned( icm ) then
      icm.Release;
  end;
end; {= TRzCustomShellList.DoCommandForAllSelected =}


procedure TRzCustomShellList.DoCommandForFolder( Cmd: PAnsiChar );
var
  icm: IContextMenu_NRC;
begin
  icm := nil;
  try
    OleCheck( IShf_GetUIObjectOf( FIShfPidl, IID_IContextMenu, Pointer( icm ) ) );
    OleCheck( ExecuteContextMenuItem( GetValidParentHWND( Self ), icm, cmd ) );
  finally
    if Assigned( icm ) then
      icm.Release;
  end;
end;


// This algorithm courtesy of Clinton R. Johnson.

procedure TRzCustomShellList.RefreshItems;
var
  dw, dummy: DWORD;
  ienum: IEnumIdList_NRC;
  absidl, enumidl: PItemIdList;

  currentPIDLS: TRzPidlList;
  newPIDLS: TRzPidlList;

  Loop: Integer;
  At: Integer;
  LD: TRzShellListData;

  P: PItemIDList;

  OldCursor: TCursor;
  updateBegan: Boolean;
begin
  if FDeferRefresh or IsEditing then
  begin
    FRefreshDeferred := True;
    Exit;
  end;

  if Items.Count = 0 then
  begin
    FillItems;
    Exit;
  end;


  OldCursor := Screen.Cursor;
  Screen.Cursor := crHourglass;
  enumidl := nil;
  ienum := nil;
  absidl := nil;
  currentPidls := nil;
  newpidls := nil;
  updateBegan := False;
  try
    FillStart;
    FilterPreApply;
    currentPIDLS := TRzPidlList.Create;
    currentPIDLS.Sorted := False;           // Turns out sorting it later is faster, inserting into
                                          // as sorted array causes too many moves.  The quick sort
                                          // minimizes the moves required, we will sort it AFTER
                                          // all items are added so that we can use a binary search.

    currentPIDLS.Capacity := Items.Count;   // Pre allocate the space for speed.
    if Folder.PathName = '' then            // If the folder is not a file-system folder
      CurrentPIDLS.ShellFolder := FIShf;    // Causes comparisons to be done by the IShellFolder.
    newPIDLS := TRzPidlList.Create;

    newPIDLS.Capacity := Items.Count;
    newPIDLS.Sorted := False;               // Only 1 list needs to be sorted, we'll sort the other list.
    newPIDLS.Malloc := ShellIMalloc;        // causes PIDLS to be free'ed

    for Loop := 0 to Items.Count - 1 do
    begin
      ld := GetDataFromItem( Items[ Loop ] );
      if Assigned( ld ) then
        CurrentPIDLS.AddObject( ld.FRelPidl, Items[ Loop ] );
    end;
    currentPIDLS.Sorted := True;

    // Add new items
    dw := FIShf.EnumObjects( GetValidParentHWND( Self ),
      SHCONTF_FOLDERS or SHCONTF_NONFOLDERS or SHCONTF_INCLUDEHIDDEN_FLAG[ sloShowHidden in Options ],
      ienum );
    if Succeeded( dw ) then
    begin
      dw := ienum.Next( 1, enumidl, PInteger( @dummy ) );
      while ( dw = S_OK ) do
      begin
        NewPidls.Add( EnumIDL );
        dw := ienum.Next( 1, enumidl, PInteger( @dummy ) );
      end;
    end;

    // Find those items which are in both lists, and discard them.  Items in
    // NEWPIDLS are automatically Freed when deleted because the .MALLOC property is assigned.
    for Loop := NewPidls.Count - 1 downto 0 do
    begin
      At := CurrentPIDLS.IndexOf( NewPidls[ Loop ] );
      if not ( At = -1 ) then               // If the item exists in both loops.
      begin
        NewPIDLS.Delete( Loop );
        CurrentPIDLS.Delete( At );
      end;
    end;

    // Now, CurrentPIDLS contains a list of those files which were deleted,
    // and NEWPIDLS contains a list of those PIDLS which were added, take the appropriate action.

    Items.BeginUpdate;
    updateBegan := True;
    for Loop := 0 to CurrentPIDls.Count - 1 do
      TListItem( CurrentPIDLS.Objects[ Loop ] ).Delete;

    // We need the remaind PIDLs not to be deleted.
    NewPIDLS.Malloc := nil;

    AllocBy := NewPidls.Count;              // To try any reduce overhead in the TLISTVIEW assignments.
    // I'm only going to add files here.
    for Loop := 0 to NewPIDLS.Count - 1 do
    begin
      P := NewPidls[ Loop ];
      absidl := ConcatIdLists( nil, FIShfPidl, P );
      AddNewShellItem( absidl, P );
      ShellMemFree( absidl );
      absidl := nil;
    end;
  finally
    newPIDLS.Free;
    currentPIDLS.Free;
    if Assigned( ienum ) then
      ienum.Release;
    ShellMemFree( absidl );
    FilterPostApply;
    if updateBegan then
      Items.EndUpdate;
    FillComplete;
    AllocBy := 256;                         // Put it back to default.
    Screen.Cursor := OldCursor;
  end;
end; {= TRzCustomShellList.RefreshItems =}


function TRzCustomShellList.GetDragDropAttributesForAllSelected: DWORD;
type
  PPDWORD = ^PDWORD;
var
  c: Integer;
  dwa: DWORD;
  pidla: TList;
  theItem: TListItem;
begin
  c := ShellSelCount;
  if ( c = 0 ) then
  begin
    Result := 0;
    Exit;
  end;

  pidla := TList.Create;
  dwa := DRAGFLAGS;

  if ( SelCount = 1 ) then
  begin
    pidla.Add( SelectedItem.RelativeIdList );
  end
  else
  begin
    theItem := Selected;
    while Assigned( theItem ) do
    begin
      if ItemHasData( theItem ) then
        pidla.Add( GetDataFromItem( theItem ).RelativeIdList );
      theItem := GetNextItem( theItem, sdAll, [ isSelected ] );
    end
  end;

  try
    OleCheck( _IShellFolder.GetAttributesOf( c, PPItemIdList( pidla.List )^, dwa ) );
    Result := dwa;
  finally
    pidla.Free;
  end;
end; {= TRzCustomShellList.GetDragDropAttributesForAllSelected =}


function TRzCustomShellList.GetDragDropAttributesForItem( item: TListItem ): DWORD;
var
  pidl: PItemIdList;
  shld: TRzShellListData;
begin
  shld := GetDataFromItem( item );
  pidl := shld.RelativeIdList;
  Result := DRAGFLAGS;
  OleCheck( _IShellFolder.GetAttributesOf( 1, pidl, Result ) );
end;


procedure TRzCustomShellList.SelectAll;
begin
  Items.BeginUpdate;
  try
    inherited;
  finally
    Items.EndUpdate;
  end;
end;

function TRzCustomShellList.ShellSelCount: Integer;
var
  item: TListItem;
begin
  Result := 0;
  item := Selected;
  while Assigned( item ) do
  begin
    if Assigned( item.Data ) then
      Inc( Result );
    item := GetNextItem( item, sdAll, [ isSelected ] );
  end;
end;


procedure TRzCustomShellList.Synchronize( ApplyToGroup: Boolean );
begin
  if ApplyToGroup then
  begin
    if Assigned( FShellCombo ) then
      FShellCombo.Synchronize( False );
    if Assigned( FShellTree ) then
      FShellTree.Synchronize( False );
  end;
  if FTimer.Enabled then
    TimerElapsed( FTimer );
end;


procedure TRzCustomShellList.ProcessSendTo( Index: Integer );
var
  data: IDataObject_NRC;
begin
  data := nil;
  try
    TDataObject_shlv.Create( Self ).QueryInterface( IID_IDataObject, data );
    ProcessSendTo_via_drop( _sl[ Index ], data );
  finally
    if Assigned( data ) then
      data.Release;
  end;
end;


function TRzCustomShellList.CreateIDropSource( Button: TMouseButton; DataObject: IDataObject_NRC ): IDropSource_NRC;
begin
  TDropSource_shlvtv.Create( Button ).QueryInterface( IID_IDropSource, Result );
end;


procedure TRzCustomShellList.OleBeginDrag( Button: TMouseButton );
var
  effect: Longint;
  ids: IDropSource_NRC;
  LocalRefreshDeferred: Boolean;
  DragResult: HResult;
begin
  ids := nil;
  try
    FDeferRefresh := True;
    FSkipRButtonUp := True;
    TDataObject_shlv.Create( Self ).QueryInterface( IID_IDataObject, FIDataObject );
    effect := Self.GetDragDropAttributesForAllSelected;

    ids := CreateIDropSource( Button, FIDataObject );

    DragResult := _DoDragDrop( FIDataObject, ids, effect, effect );


    if DragResult = DRAGDROP_S_DROP then
      LocalRefreshDeferred := True
    else
      LocalRefreshDeferred := FRefreshDeferred;

  finally
    FDeferRefresh := False;
    FRefreshDeferred := False;
    if Assigned( ids ) then
      ids.Release;
    if Assigned( FIDataObject ) then
    begin
      FIDataObject.Release;
      FIDataObject := nil;
    end;
  end;
  if LocalRefreshDeferred then
  begin
    RefreshItems;
    //SortList;
  end;
end; {= TRzCustomShellList.OleBeginDrag =}


function TRzCustomShellList.OnDropTarget_DragEnter( const dataObj: IDataObject_NRC; grfKeyState: Longint; pt: TPoint; var dwEffect: Longint ): HResult;
var
  idt: IDropTarget_NRC;
  pidl: PItemIdList;
  item: TListItem;
  hr: HResult;
begin
  FLastAutoScrollTick := Windows.GetTickCount;

  FILastDropDataObject := dataObj;
  FILastDropDataObject.AddRef;
  FInitialDropKeyState := grfKeyState;

  Result := S_OK;
  with ScreenToClient( pt ) do
    item := GetItemAt( x, y );

  idt := nil;
  try
    if ItemHasData( item ) then
    begin
      pidl := GetDataFromItem( item ).RelativeIdList; // No need to free
      hr := FIShf.GetUIObjectOf( GetValidParentHWND( Self ),
        1, pidl, IID_IDropTarget, nil, Pointer( idt ) );
      if not Succeeded( hr ) then
      begin
        hr := IShf_GetUIObjectOf( FIShfPidl, IID_IDropTarget, Pointer( idt ) );
        DropTarget := nil;
        item := nil;
      end;
    end
    else if FDeferRefresh then             // Don't allow drop on our own parent unless right-dragging
    begin
      if ( ( grfKeyState and MK_RBUTTON ) <> 0 ) then
      begin
        dwEffect := DROPEFFECT_COPY;
        DropTarget := nil;
        Exit;
      end
      else
        hr := E_FAIL
    end
    else
      hr := IShf_GetUIObjectOf( FIShfPidl, IID_IDropTarget, Pointer( idt ) );

    if Succeeded( hr ) then
    begin
      Result := idt.DragEnter( dataObj, grfKeyState, pt, dwEffect );
      idt.DragLeave;
      DropTarget := nil;
      if ( dwEffect <> 0 ) then
        DropTarget := item;
    end
    else
    begin
      DropTarget := nil;
      dwEffect := DROPEFFECT_NONE;
    end;
  finally
    if Assigned( idt ) then
      idt.Release;
  end;
end; {= TRzCustomShellList.OnDropTarget_DragEnter =}


function TRzCustomShellList.OnDropTarget_DragOver( grfKeyState: Longint; pt: TPoint; var dwEffect: Longint ): HResult;
var
  idt: IDropTarget_NRC;
  pidl: PItemIdList;
  item: TListItem;
  hr: HResult;
  oldDropTarget: TListItem;
begin
  oldDropTarget := DropTarget;
  Result := S_OK;
  pt := ScreenToClient( pt );
  item := GetItemAt( pt.x, pt.y );

  idt := nil;
  try
    if Assigned( item ) then
    begin
      if ItemHasData( item ) then
      begin
        pidl := GetDataFromItem( item ).RelativeIdList; // No need to free
        hr := FIShf.GetUIObjectOf( GetValidParentHWND( Self ),
          1, pidl, IID_IDropTarget, nil, Pointer( idt ) );
        if not Succeeded( hr ) then
        begin
          hr := IShf_GetUIObjectOf( FIShfPidl, IID_IDropTarget, Pointer( idt ) );
          DropTarget := nil;
          item := nil;
        end;
      end
      else
      begin
        dwEffect := DROPEFFECT_COPY;
        DropTarget := nil;
        Exit;
      end;
    end
    else if FDeferRefresh then             // Don't allow drop on our own parent unless right-dragging
    begin
      if ( ( grfKeyState and MK_RBUTTON ) <> 0 ) then
      begin
        dwEffect := DROPEFFECT_COPY;
        DropTarget := nil;
        Exit;
      end
      else
        hr := E_FAIL;
    end
    else
      hr := IShf_GetUIObjectOf( FIShfPidl, IID_IDropTarget, Pointer( idt ) );

    if Succeeded( hr ) then
    begin
      idt.DragEnter( FILastDropDataObject, grfKeyState, pt, dwEffect );
      Result := idt.DragOver( grfKeyState, pt, dwEffect );
      idt.DragLeave;
      if ( dwEffect <> 0 ) then
      begin
        if ( DropTarget <> item ) then
        begin
          DropTarget := nil;
          DropTarget := item;
        end;
      end
      else
        DropTarget := nil;
    end
    else
    begin
      dwEffect := DROPEFFECT_NONE;
      DropTarget := nil;
    end;
  finally
    if Assigned( idt ) then
      idt.Release;
  end;

// Handle auto-scrolling
  if Windows.GetTickCount > Cardinal( FLastAutoScrollTick + RZSH_AUTOSCROLL_MINDELAY_MS ) then
  begin
    if ( pt.x < RZSH_AUTOSCROLL_THRESHOLD_X ) then
      DoAutoScroll( Self, SB_HORZ, SB_LINEUP )
    else if ( pt.x > ClientRect.right - RZSH_AUTOSCROLL_THRESHOLD_X ) then
      DoAutoScroll( Self, SB_HORZ, SB_LINEDOWN );

    if ( pt.y < RZSH_AUTOSCROLL_THRESHOLD_Y ) then
      DoAutoScroll( Self, SB_VERT, SB_LINEUP )
    else if ( pt.y > ClientRect.bottom - RZSH_AUTOSCROLL_THRESHOLD_Y ) then
      DoAutoScroll( Self, SB_VERT, SB_LINEDOWN );

    FLastAutoScrollTick := Windows.GetTickCount;
  end;
  dwEffect := dwEffect or Integer( DROPEFFECT_SCROLL );

  if oldDropTarget <> DropTarget then
    Update;
end; {= TRzCustomShellList.OnDropTarget_DragOver =}


function TRzCustomShellList.OnDropTarget_DragLeave: HResult;
begin
  FILastDropDataObject.Release;
  FILastDropDataObject := nil;
  DropTarget := nil;
  Result := S_OK;
end;


function TRzCustomShellList.OnDropTarget_Drop( const dataObj: IDataObject_NRC; grfKeyState: Longint; pt: TPoint; var dwEffect: Longint ): HResult;
var
  idt: IDropTarget_NRC;
  hr: HResult;
  pidl: PItemIdList;
  item: TListItem;
  dw2: Longint;
  oldCursor: TCursor;
begin
  Result := S_OK;
  with ScreenToClient( pt ) do
    item := GetItemAt( x, y );

  idt := nil;
  try
    if ItemHasData( item ) then
    begin
      pidl := GetDataFromItem( item ).RelativeIdList; // No need to free
      hr := FIShf.GetUIObjectOf( GetValidParentHWND( Self ),
        1, pidl, IID_IDropTarget, nil, Pointer( idt ) );
      if not Succeeded( hr ) then
      begin
        hr := IShf_GetUIObjectOf( FIShfPidl, IID_IDropTarget, Pointer( idt ) );
        DropTarget := nil;
//        item := nil;
      end;
    end
    else if FDeferRefresh then             // Don't allow drop on our own parent unless right-dragging
    begin
      if ( ( FInitialDropKeyState and MK_RBUTTON ) <> 0 ) then
      begin
        dwEffect := dwEffect and not DROPEFFECT_MOVE;
        hr := IShf_GetUIObjectOf( FIShfPidl, IID_IDropTarget, Pointer( idt ) );
      end
      else
        hr := E_FAIL
    end
    else
      hr := IShf_GetUIObjectOf( FIShfPidl, IID_IDropTarget, Pointer( idt ) );

    if Succeeded( hr ) then
    begin
      if ( FInitialDropKeyState and MK_RBUTTON ) <> 0 then
        grfKeyState := grfKeyState or MK_RBUTTON;
      if ( FInitialDropKeyState and MK_LBUTTON ) <> 0 then
        grfKeyState := grfKeyState or MK_LBUTTON;

      idt.DragEnter( dataObj, grfKeyState, pt, dw2 );
      idt.DragOver( grfKeyState, pt, dw2 );
      dwEffect := dwEffect and GetDragDropAttributesForClipboardObjects( dataObj );

      oldCursor := Screen.Cursor;
      Screen.Cursor := crHourglass;
      try
        Result := idt.Drop( dataObj, grfKeyState, pt, dwEffect );
        RefreshItems;
      finally
        Screen.Cursor := oldCursor;
      end;
    end;
  finally
    if Assigned( idt ) then
      idt.Release;
  end;

  DropTarget := nil;
  FILastDropDataObject.Release;
  FILastDropDataObject := nil;
end; {= TRzCustomShellList.OnDropTarget_Drop =}


{
Algorithm for remembering column widths:
  IF IShellDetails available THEN
    IF an ordinary folder AND not the first time AND ... THEN
      use last widths
    ELSE
      use IShellDetails widths
    ENDIF
  ELSE
    IF first time OR explorer settings changed since last time THEN
      load widths from registry
    ELSE
      use last widths
    ENDIF
  ENDIF
}

procedure TRzCustomShellList.InitColumns( ishd: IShellDetails_NRC );

  procedure ReadCurrentColWidths( aStream: TMemoryStream );
  var
    I, max, w: Integer;
  begin
    aStream.Position := 0;
//    Columns.BeginUpdate; -- causes flicker
//    try
    if aStream.Read( max, Sizeof( Integer ) ) <> Sizeof( Integer ) then
      Exit;
    if max > Columns.Count - 1 then
      max := Columns.Count - 1;
    for I := 0 to max do
    begin
      if aStream.Read( w, Sizeof( Integer ) ) = Sizeof( Integer ) then
        Columns[ I ].Width := w
      else
        Break;
    end;
//    except
//    end;
  end;                                      {ReadCurrentColWidths - local}

  procedure FakeColumns;
  const
    KEY_EXPLORER = 'Software\Microsoft\Windows\CurrentVersion\Explorer';
    VAL_DIRECTORYCOLS = 'DirectoryCols';
  var
    lc: TListColumn;
    r: TRegistry;
    colData: packed array[ 0..10 ] of Word;
    colDataSize: Integer;
    fRegOk: Boolean;
    I: Integer;
    function NewDataMatchesOldData: Boolean;
    var
      I: Integer;
    begin
      Result := False;
      if ( colDataSize <> g_LastRegistryColDataSize ) then
        Exit;
      for I := 0 to ( colDataSize div Sizeof( coldata[ 0 ] ) ) - 1 do
        if colData[ I ] <> g_LastRegistryColData[ I ] then
          Exit;
      Result := True;
    end;
  begin
    colData[ 0 ] := 200;                    // name
    colData[ 1 ] := 60;                     // size
    colData[ 2 ] := 100;                    // type
    colData[ 3 ] := 120;                    // modified
    colData[ 4 ] := 50;                     // attributes

    colDataSize := 5 * Sizeof( Word );
    r := TRegistry.Create;
    try
      try
        fRegOk := r.OpenKey( KEY_EXPLORER, False );
        if fRegOk and r.ValueExists( VAL_DIRECTORYCOLS ) and ( r.GetDataSize( VAL_DIRECTORYCOLS ) <= Sizeof( colData ) ) then
          colDataSize := r.ReadBinaryData( VAL_DIRECTORYCOLS, colData, SizeOf( colData ) );
      except
      end;
    finally
      r.Free;
    end;

    Columns.BeginUpdate;
    Columns.Clear;
    lc := Columns.Add;
    lc.Caption := SColumnName;

    lc := Columns.Add;
    lc.Caption := SColumnSize;
    lc.Alignment := taRightJustify;

    lc := Columns.Add;
    lc.Caption := SColumnType;

    lc := Columns.Add;
    lc.Caption := SColumnModified;

    if ( g_LastRegistryColDataSize = 0 ) or ( not NewDataMatchesOldData ) or ( g_LastColWidths.Size = 0 ) then
      for I := 0 to 3 do
        Columns[ I ].Width := colData[ I ]
    else
      ReadCurrentColWidths( g_LastColWidths );

    CopyMemory( @g_LastRegistryColData[ 0 ], @colData[ 0 ], Sizeof( colData ) );
    g_LastRegistryColDataSize := colDataSize;
    Columns.EndUpdate;
    FLastColState := lcsDefault;
    FLastFolderWasDir := True;
  end;                                      {FakeColumns - local}

  procedure DynamicColumns;
  const
    _justify: array[ 0..2 ] of TAlignment = ( taLeftJustify, taRightJustify, taCenter );
  var
    I: Integer;
    dw: DWORD;
    info: TShColInfo;
    lc: TListColumn;
    fIsNormalFolder: Boolean;
  begin
    fIsNormalFolder := ( ShellGetPathFromIdList( FIShfPidl ) <> '' ); // and (g_LastColWidths.Size<>0);
    Columns.BeginUpdate;
    Columns.Clear;
    I := 0;
    ZeroMemory( @info, Sizeof( info ) );
    info.text.uType := STRRET_CSTR;
    dw := ishd.GetDetailsOf( nil, I, info );
    while Succeeded( dw ) do
    begin
      lc := Columns.Add;
      lc.Caption := StrretToString( nil, info.text );
      if not fIsNormalFolder or ( g_LastColWidths.Size = 0 ) then
        lc.Width := 9 * info.Width;         // What's the algorithm explorer uses? It's not GetDialogBaseUnits, tmAveCharWidth...?
      lc.Alignment := _justify[ info.justify ];
      StrretFree( info.text );
      ZeroMemory( @info, Sizeof( info ) );
      info.text.uType := STRRET_CSTR;
      Inc( I );
      dw := ishd.GetDetailsOf( nil, I, info );
    end;
    StrretFree( info.text );
    if fIsNormalFolder and ( g_LastColWidths.Size <> 0 ) then
      ReadCurrentColWidths( g_LastColWidths );
    FLastFolderWasDir := fIsNormalFolder;
    Columns.EndUpdate;
    FLastColState := lcsShellDetails;
  end;
begin {= TRzCustomShellList.InitColumns =}
  if ( Columns.Count > 0 ) and ( FLastFolderWasDir ) then
    WriteCurrentColWidths( Self, g_LastColWidths );

  case FLastColState of
    lcsNone, lcsShellDetails:
      if Assigned( ishd ) then
        DynamicColumns
      else
        FakeColumns;

    lcsDefault:
      if Assigned( ishd ) then
        DynamicColumns
      else
        FakeColumns;
  end;

  SetHeaderODStyle;
  if SortColumn > Columns.Count then
    SortColumn := 1
  else
    SortColumn := SortColumn;
end; {= TRzCustomShellList.InitColumns =}


procedure TRzCustomShellList.InitImageLists;
begin
  SmallImages.Handle := ShellGetSystemImageList( iszSmall );
  LargeImages.Handle := ShellGetSystemImageList( iszLarge );
  SmallImages.ShareImages := True;
  LargeImages.ShareImages := True;
end;


function TRzCustomShellList.IsFolderStored: Boolean;
begin
  Result := not Assigned( FShellTree ) and not Assigned( FShellCombo );
    // If we are linked to a tree view then we don't store the Folder property - we take our orders
    // from the associated TRzShellTree at run time.
end;


function TRzCustomShellList.GetCurrentFolderIShellDetails: IShellDetails_NRC;
begin
  if not ( FIShellDetailsValid ) then
  begin
    FIShellDetailsValid := True;
    FIShellDetails := CreateShellDetailsAdapter( FIShf );
  end;
  Result := FIShellDetails;
end;


procedure TRzCustomShellList.HandleOnFolderChanged( Sender: TObject );
begin
  if ( FIgnoreChanges <= 0 ) then
  begin
    Inc( FIgnoreChanges );
    try
      if Assigned( FShellTree ) then
        FShellTree.SelectedFolder := Folder
      else if Assigned( FShellCombo ) then
        FShellCombo.SelectedFolder := Folder;
      FillItems;
    finally
      Dec( FIgnoreChanges );
    end;
  end;
  FolderChanged;
end;


procedure TRzCustomShellList.SetShellTree( Value: TRzCustomShellTree );
begin
  FShellTree := Value;
  if Assigned( FShellTree ) then
    FShellTree.FreeNotification( Self );
end;


procedure TRzCustomShellList.SetShellCombo( Value: TRzCustomShellCombo );
begin
  FShellCombo := Value;
  if Assigned( FShellCombo ) then
    FShellCombo.FreeNotification( Self );
end;


procedure TRzCustomShellList.SetSortColumn( Value: Integer );
const
  _DIR: array[ Boolean ] of TRzLVSortDirection = ( sdDescending, sdAscending );
begin
  HeaderSortColumn := Abs( Value ) - 1;
  HeaderSortDirection := _DIR[ Value >= 0 ];
  FSortColumnAssigned := True;
  if FSortColumn <> Value then
  begin
    FSortColumn := Value;
    SortList;
  end;
end;


procedure TRzCustomShellList.SetOptions( Value: TRzShellListOptions );
const
  DRAGOPTS = [ sloOleDrag, sloOleDrop ];
var
  oldOptions: TRzShellListOptions;
  fNeedRefill: Boolean;
begin
  oldOptions := FOptions;
  FOptions := Value;
  if ( csLoading in ComponentState ) then
    Exit;
  if not HandleAllocated then
    Exit;

  fNeedRefill := False;

  if ( oldOptions * [ sloOleDrop ] <> FOptions * [ sloOleDrop ] ) then
  begin
    if ( sloOleDrop in FOptions ) and not Assigned( FIDropTarget ) then
    begin                                   // drop capability turned ON
      TDropTarget_shlv.Create( Self ).QueryInterface( IID_IDropTarget, FIDropTarget );
      _RegisterDragDrop( Handle, FIDropTarget );
    end
    else if not ( sloOleDrop in FOptions ) and Assigned( FIDropTarget ) then
    begin                                   // drop capability turned OFF
      RevokeDragDrop( Handle );
      FIDropTarget.Release;
      FIDropTarget := nil;
    end;
  end;

  if ( ( oldOptions * [ sloDynamicRefresh ] <> FOptions * [ sloDynamicRefresh ] ) ) and
    not ( csDesigning in ComponentState ) then
  begin                                     // sloDynamicRefresh State is different
    if ( sloDynamicRefresh in FOptions ) then
    begin
      InstallChangeHandler;
    end
    else if Assigned( FChangeHandlerThread ) then
      TRzChangeHandlerThread( FChangeHandlerThread ).RemoveAllMonitors;
  end;

  if ( oldOptions * [ sloHideFoldersWhenLinkedToTree ] <> FOptions * [ sloHideFoldersWhenLinkedToTree ] ) then
    fNeedRefill := True;                    // hidefolders state is different

  if ( oldOptions * [ sloShowHidden ] <> FOptions * [ sloShowHidden ] ) then
    fNeedRefill := True;

  // -- --

  if fNeedRefill and not ( csLoading in ComponentState ) then
    RefreshItems;
end; {= TRzCustomShellList.SetOptions =}


procedure TRzCustomShellList.SetFileFilter( const Value: string );
var
  s: string;
begin
  s := FFileFilter;
  FFileFilter := Value;
  if ( s <> Value ) then
  begin
    if not Assigned( FFilterExtensions ) then
      FFilterExtensions := TStringList.Create
    else
      FFilterExtensions.Clear;
    ExtensionsToTStrings( FFileFilter, FFilterExtensions );
    if not ( csLoading in ComponentState ) and ( sloAutoFill in Options ) then
      FillItems;
  end
  else if s = '' then
  begin
    FFilterExtensions.Free;
    FFilterExtensions := nil;
  end;
end; {= TRzCustomShellList.SetFileFilter =}


procedure TRzCustomShellList.SetFolder( Value: TRzShellLocator );
begin
//  if (csLoading in ComponentState) then Inc( FIgnoreChanges );
  try
    FFolder.Assign( Value );
  finally
//    if (csLoading in ComponentState) then Dec( FIgnoreChanges );
  end;
end;


function TRzCustomShellList.GetSelectedItem: TRzShellListData;
begin
  Result := GetDataFromItem( Selected );
end;


function TRzCustomShellList.GetShellListData( Index: Integer ): TRzShellListData;
begin
  Result := TObject( Items[ Index ].Data ) as TRzShellListData;
end;


function TRzCustomShellList.ShouldInclude( baseidlist, relidlist: PItemIdList; var attrib: DWORD ): Boolean;
begin
  attrib := ( SFGAO_FOLDER or SFGAO_DISPLAYATTRMASK or SFGAO_FILESYSANCESTOR or
    SFGAO_FILESYSTEM or SFGAO_FILESYSANCESTOR or SFGAO_REMOVABLE or SFGAO_GHOSTED or SFGAO_STREAM )
    and not SFGAO_READONLY;

  if SHELL32_VER.version >= SHELL32_VER60 then
    attrib := attrib or SFGAO_STREAM;

  // If SFGAO_READONLY is specified under IE4 and we're querying a floppy drive, it gets accessed. Bad thing.
  FIShf.GetAttributesOf( 1, relidlist, attrib );

  if not ( sloNonFilesystemAncestors in Options ) and
    ( ( attrib and ( SFGAO_FILESYSTEM or SFGAO_FILESYSANCESTOR ) ) = 0 ) then
    Result := False
  else
    Result := True;

  if ( sloHideFoldersWhenLinkedToTree in Options ) and
    Assigned( FShellTree ) and
    RzShIsFolder( attrib, sloFilesCanBeFolders in Options )  then
    Result := False;

  Result := Result and CanAdd( FIShf, baseidlist, relidlist, attrib );
end;


// Caller relinquishes ownership of aRelIdList, but retains ownership of aAbsIdList.

function TRzCustomShellList.AddNewShellItem( AbsIdList, RelIdList: PItemIdList ): HResult;
var
  curitem: TRzShellListData;
  attrib: DWORD;
begin
  try
    Result := S_OK;
    try
      if ShouldInclude( AbsIdList, RelIdList, attrib ) then
      begin
        curitem := TRzShellListData.Create( Self );
        curitem.SetData( RelIdList );
        RelIdList := nil;                  // SetData takes ownership of aRelIdList

        FCurrentItemData := curitem;        // The FCurrentItemData member will be used to initialise the new item's Data property in the InsertItem method
        FCurrentItemFlags := attrib;
        FCurrentItemIShd := FIShellDetails;
      {newitem := } Items.Add;
              // Check out the .InsertItem method. More setup of the newitem object is done there so it is correctly
              // set up for the OnInsert event call.
      end;                                  {if ShouldInclude}
    finally
      ShellMemFree( RelIdList );
    end;
  except
    ShowMessage( 'Exception in AddNewShellItem' );
    raise;
  end;
end; {= TRzCustomShellList.AddNewShellItem =}


function TRzCustomShellList.CanAdd( parentIShf: IShellFolder_NRC;
                                    parentAbsPidl: PItemIdList;
                                    itemRelPidl: PItemIdList;
                                    itemAttributes: DWORD ): Bool;
begin
  Result := True;

  if Assigned( FFilterLookupTable ) then
    Result := RzShIsFolder( itemAttributes, sloFilesCanBeFolders in Options ) or
              FilterApply( ExtractFileName( ShellGetFriendlyNameFromIdList( parentIShf, itemRelPidl, fnForParsing ) ),
                           itemAttributes );
  if Assigned( OnAddItem ) and Result then
    OnAddItem( Self, parentIShf, parentAbsPidl, itemRelPidl, itemAttributes, Result );
end;


function TRzCustomShellList.CanEdit( Item: TListItem ): Boolean;
var
  ItemData: TRzShellListData;
  EditCtl: HWND;
begin
  if ItemHasData( Item ) then
  begin
    ItemData := GetDataFromItem( Item );
    Result := ItemData.Editable and inherited CanEdit( Item ) and not ( csDesigning in ComponentState );
    if Result then
    begin
      EditCtl := ListView_GetEditControl( Handle );
      if EditCtl <> 0 then
        SetWindowText( EditCtl, PChar( ShellGetFriendlyNameFromIdList( FIShf, ItemData.RelativeIdList, fnForEditing ) ) );
    end;
  end
  else
    Result := inherited CanEdit( Item );
end;


function TRzCustomShellList.DblClickOpen: Boolean;
begin
  Result := False;
  if Assigned( OnDblClickOpen ) then
    OnDblClickOpen( Self, Result );
  Result := not Result;
end;


function ListCompareFunc( aNode1, aNode2, lParam: TRzNativeInt ): Integer; stdcall;
var
  node1: TListItem absolute aNode1;
  node2: TListItem absolute aNode2;
  parent: TRzCustomShellList absolute lParam;

  node1Data: TRzShellListData;
  node2Data: TRzShellListData;
  hres: HResult;
begin
  if ( TObject( node1.Data ) is TRzShellListData ) and ( TObject( node2.Data ) is TRzShellListData ) then
  begin
    node1Data := TRzShellListData( node1.Data );
    node2Data := TRzShellListData( node2.Data );
    hres := parent.FIShf.CompareIds( Abs( parent.SortColumn ) - 1, node1Data.RelativeIdList, node2Data.RelativeIdList );
    if Succeeded( hres ) then
      Result := Smallint( ResultCode( hres ) )
    else
      Result := 0;
    if ( parent.SortColumn < 0 ) then
      Result := -Result;
  end
  else
    Result := 0;                            // Should call OnCompareItem event
end;


procedure TRzCustomShellList.SortList;
begin
  if SortColumn = 0 then
    CustomSort( nil, 0 )                    // Uses OnCompare event
  else
    CustomSort( ListCompareFunc, Integer( Self ) );
end;



procedure TRzCustomShellList.DeviceChangeDetected( Sender: TObject; var Msg: TMessage );
begin
  if ( Msg.WParam = DBT_DEVICEARRIVAL ) or ( Msg.WParam = DBT_DEVICEREMOVECOMPLETE ) then
  begin
    FlushDriveInfoCache;
    LockFlushDriveInfoCache;
    try
      if RzShellUtils.ShellFindCSIDLFromIdList( Folder.IDList ) = csidlDrives then
        FillItems;
    finally
      UnlockFlushDriveInfoCache;
    end;
  end;
end;


procedure TRzCustomShellList.Edit( const Item: TLVItem );

  function GetItem( Value: TLVItem ): TListItem;
  begin
    with Value do
      if ( mask and LVIF_PARAM ) <> 0 then
        Result := TListItem( lParam )
      else
        Result := Items[ IItem ];
  end;

var
  S: string;
  EditItem: TListItem;
  EditData: TRzShellListData;
  pidlout: PItemIdList;
  wca: array[ 0..MAX_PATH ] of WideChar;
  c: TCursor;
  fRefresh: Boolean;

begin
  with Item do
  begin
    S := Trim( pszText );
    EditItem := GetItem( Item );
    if EditItem <> nil then
    begin
      EditData := GetDataFromItem( EditItem );
      if Assigned( EditData ) then
      begin
        if Assigned( OnEdited ) then
          OnEdited( Self, EditItem, S );
        pidlout := nil;
        c := Screen.Cursor;
        Screen.Cursor := crHourglass;
        FIgnoreNextChangeNotify := True;
        try
          if Succeeded( FIShf.SetNameOf( GetValidParentHWND( Self ),
                        EditData.RelativeIdList,
                        StringToWideChar( s, wca, High( wca ) ),
                        SHCONTF_FOLDERS, pidlout ) ) then
          begin
            if Assigned( pidlout ) then
            begin
              EditItem.Caption := ShellGetFriendlyNameFromIdList( FIShf, pidlout, fnInFolder );
              EditData.SetData( pidlout );
            end
            else
              EditItem.Caption := ShellGetFriendlyNameFromIdList( FIShf, EditData.RelativeIdList, fnInFolder );
          end;
          fRefresh := FRefreshDeferred;
        finally
          Screen.Cursor := c;
          FRefreshDeferred := False;
        end;
        if fRefresh then
          RefreshItems;
      end
      else
        inherited;
    end;
  end;                                      {end with}
end; {= TRzCustomShellList.Edit =}



procedure TRzCustomShellList.FillList( ishf: IShellFolder_NRC; basepidl: PItemIdList );
var
  dw, attrib, dummy: DWORD;
  ienum: IEnumIDList_NRC;
  orgShPidl: PItemIdList;
  curRelPidl, curAbsPidl: PItemIdList;

  ecode: UINT;
  ishd: IShellDetails_NRC;

  I: Integer;

  tmplist: TList;
  curitem: TRzShellListData;
begin
  ienum := nil;
  curRelpidl := nil;
  curAbsPidl := nil;
  orgShPidl := nil;
  Items.BeginUpdate;
  ecode := SetErrorMode( 0 );
  FillStart;
  try                                       //finally
    SetErrorMode( ecode or SEM_FAILCRITICALERRORS );
    try                                     //except
      orgShPidl := FIShfPidl;
      FIShfPidl := CopyIdList( nil, basepidl );

      if Assigned( FIShf ) then
        FIShf.Release;
      FIShf := ishf;
      if Assigned( FIShellDetails ) then
      begin
        FIShellDetails.Release;
        FIShellDetails := nil;
      end;
      FIShellDetailsValid := False;
      FIShf.AddRef;

      ishd := GetCurrentFolderIShellDetails;
      InitColumns( ishd );


      dw := ishf.EnumObjects( GetValidParentHWND( Self ),
        SHCONTF_FOLDERS or SHCONTF_NONFOLDERS or SHCONTF_INCLUDEHIDDEN_FLAG[ sloShowHidden in Options ],
        ienum );
        // SHCONTF_INCLUDEHIDDEN should only be included if Explorer View|Options "Show all files" is True.
        { I believe this to be a way of detecting the Show All Files state:
          When Show all files is True:
            [HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Explorer]
              "ShellState"=hex:10,00,00,00,03,00,00,00,00,00,00,00,00,00,00,00
                                           ^^ -- -- --
          When Show all files is False:
            [HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Explorer]
              "ShellState"=hex:10,00,00,00,02,00,00,00,00,00,00,00,00,00,00,00
                                           ^^ -- -- --
          The marked DWORD is the proposed candidate. It seems to be a bit field:
            $00000001 = Show hidden files and files with [list] of extensions
            $00000002 = Show file extensions
            $00000004 = (unknown)
            $00000008 = show compressed files in alternate color
          Verified for W95, W95/OSR2, W95/IE4, WNT4, WNT4/IE4
          Not verified for W98 or WNT5.
        }
      if Failed( dw ) or not Assigned( ienum ) then
        Abort;                              // Silent exception, as error already reported (typically)

     // -- The idea here is to create a copy of the list of items in the IEnumIdList interface in tmplist.
     // -- From there we can speed up adding to the actual list view by setting the AllocBy property to
     // -- the length of the list.
      tmplist := nil;
      try
        tmplist := TList.Create;
        tmplist.Capacity := 500;
        dw := ienum.Next( 1, curRelPidl, PInteger( @dummy ) );
        while ( dw = S_OK ) do
        begin
          if ( tmplist.Count = tmplist.Capacity ) then
            tmplist.Capacity := tmplist.Capacity + 500; // Otherwise TList just grows by a measily 16 elements

//          if ShouldInclude( basepidl, curRelPidl, {out}attrib ) then
          begin
            curitem := TRzShellListData.Create( Self );
            curitem.SetData( curRelPidl );
            curRelPidl := nil;
            tmplist.Add( curitem );
          end;

          ShellMemFree( curRelPidl );
          curRelPidl := nil;
          dw := ienum.Next( 1, curRelPidl, PInteger( @dummy ) );
        end;                                {while}
        ienum.Release;
        ienum := nil;
        if ( dw <> S_False ) and ( dw <> ( $80070000 or ERROR_NO_MORE_ITEMS ) ) then
          if dw = DWORD( E_FAIL ) then
            Abort                           // No point reporting 'unspecified error', just raise silent exception.
          else
            RaiseSysError( dw );

        AllocBy := tmplist.Count;           // This should be a significant performance boost

        for I := 0 to tmplist.Count - 1 do
        begin
          curitem := TRzShellListData( tmplist[ I ] );

          // We call ShouldInclude here, instead of back where will create tmplist so that the
          // OnAddItem event and the OnInsert item event will come one after another for each item.
          if ShouldInclude( basepidl, curitem.RelativeIdList, attrib ) then
          begin
            FCurrentItemData := curitem;    // The FCurrentItemData member will be used to initialise the new item's Data property in the InsertItem method
            FCurrentItemFlags := attrib;
            FCurrentItemIShd := ishd;
            {newitem := } Items.Add;
              // Check out the .InsertItem method. Setup of the new item object is done there so the item is
              // fully intialised before the OnInsert event is called.
          end
          else
            curitem.Free;

          tmplist[ I ] := nil;
        end;                                {for}
      finally
        for I := 0 to tmplist.Count - 1 do
          TObject( tmplist[ I ] ).Free;
        tmplist.Free;
      end;
    except
      ShellMemFree( FIShfPidl );
      FIShfPidl := orgShPidl;
      orgShPidl := nil;
      raise;
    end;
  finally
    Items.EndUpdate;
    SetErrorMode( eCode );
    ShellMemFree( curRelPidl );
    ShellMemFree( curAbsPidl );
    if Assigned( ienum ) then
      ienum.Release;
    ShellMemFree( orgShPidl );
    FillComplete;
  end;
end; {= TRzCustomShellList.FillList =}


procedure TRzCustomShellList.FillComplete;
begin
  if Assigned( OnFillComplete ) then
    OnFillComplete( Self );
end;

procedure TRzCustomShellList.FillStart;
begin
  if Assigned( OnFillStart ) then
    OnFillStart( Self );
end;

procedure TRzCustomShellList.FolderChanged;
begin
  if Assigned( OnFolderChanged ) then
    OnFolderChanged( Self );
end;


function TRzCustomShellList.GetDataFromItem( item: TListItem ): TRzShellListData;
begin
  if ItemHasData( item ) then
    Result := TObject( item.Data ) as TRzShellListData
  else
    Result := nil;
end;


procedure TRzCustomShellList.Loaded;
begin
  if not ( csLoading in ComponentState ) then
    Exit;

  if not FInCreateWnd then
  begin
    if ( sloAutofill in Options ) and ( Items.Count = 0 ) then
    begin
      PostMessage( Handle, RZSH_AM_DEFERRED_FILL, 0, 0 );
    end;
    FLoaded := True;
  end;
  inherited;
end;


procedure TRzCustomShellList.Notification( Component: TComponent; Operation: TOperation );
begin
  inherited;
  if ( Operation = opRemove ) then
    if ( Component = FShellTree ) then
      FShellTree := nil
    else if ( Component = FShellCombo ) then
      FShellCombo := nil;
end;


procedure TRzCustomShellList.CreateWnd;
begin
  FInCreateWnd := True;
  try
    inherited;

    if not ( csDesigning in ComponentState ) and ( sloOleDrop in Options ) then
    begin
      TDropTarget_shlv.Create( Self ).QueryInterface( IID_IDropTarget, FIDropTarget );
      _RegisterDragDrop( Handle, FIDropTarget );
    end;

    if ( sloAutofill in Options ) and ( Items.Count = 0 ) {and FLoaded} then
    begin
      PostMessage( Handle, RZSH_AM_DEFERRED_FILL, 0, 0 );
    end;
  finally
    FInCreateWnd := False;
  end;
end;


procedure TRzCustomShellList.DestroyWnd;
begin
  FDeletingItems := True;
  try
    Selected := nil;

    Items.BeginUpdate;
    Inc( FIgnoreChanges );
    try
      Selected := nil;
    finally Dec( FIgnoreChanges );
    end;
    Items.Clear;                            // Inherited DestroyWnd tries to save the items to a temporary Stream.
                  // We avoid that by clearing them first.
    Items.EndUpdate;
    inherited;
  finally
    FDeletingItems := False;
  end;
end;


procedure TRzCustomShellList.TreeChanged( Node: TTreeNode );
begin
  if Assigned( Node ) and Node.Selected and ( TObject( Node.Data ) is TRzShellTreeData ) and not ( csLoading in ComponentState ) then
    Self.Folder.IdList := TRzShellTreeData( Node.Data ).AbsoluteIdList;
end;


procedure TRzCustomShellList.ColClick( Column: TListColumn );
begin
  FSortColumnAssigned := False;
    { The following inherited call will cause the OnColumnClick event to fire. If this event calls
      SortColumn, then FSortColumnAssigned will become True, and our default column click handling
      will not happen. This allows custom sorting to work. }
  inherited;

  if not FSortColumnAssigned then
  begin
    if ( Column.Index + 1 = SortColumn ) then
      SortColumn := -SortColumn
    else
      SortColumn := Column.Index + 1;
  end;
end;


procedure TRzCustomShellList.Delete( item: TListItem );
var
  data: TRzShellListData;
begin
  if ItemHasData( item ) then
  begin
    if Assigned( OnDeleteItem ) then
      OnDeleteItem( Self, item, GetDataFromItem( item ) );
    data := item.Data;
    item.Data := nil;
    inherited;
    data.Free;
  end
  else
    inherited;
end;


function TRzCustomShellList.GetUIObjectForAllSelected( const riid: TGUID; var interfaceOut: Pointer ): HResult;
var
  pidls: TList;
  I: Integer;
  theItem: TListItem;
begin
  pidls := TList.Create;
  interfaceOut := nil;
  try
    theItem := Selected;
    while Assigned( theItem ) do
    begin
      if ItemHasData( theItem ) then
        pidls.Add( CopyIdList( nil, GetDataFromItem( theItem ).RelativeIdList ) );
      theItem := GetNextItem( theItem, sdAll, [ isSelected ] );
    end;

    if ( pidls.Count = 0 ) then
    begin
      Result := E_FAIL;
      Exit;
    end;

    Result := FIShf.GetUIObjectOf( GetValidParentHWND( Self ), pidls.Count, PPItemIdList( pidls.List )^,
      riid, nil, Pointer( interfaceOut ) );
  finally
    for I := 0 to pidls.Count - 1 do
      ShellMemFree( pidls[ I ] );
    pidls.Free;
  end;
end; {= TRzCustomShellList.GetUIObjectForAllSelected =}


function TRzCustomShellList.GetUIObjectForItem( Item: TListItem; const riid: TGUID; var interfaceOut: Pointer ): HResult;
var
  pidl: PItemIdList;
begin
  if ItemHasData( Item ) then
  begin
    pidl := GetDataFromItem( Item ).RelativeIdList;
    Result := FIShf.GetUIObjectOf( GetValidParentHWND( Self ), 1, pidl,
      riid, nil, Pointer( interfaceOut ) );
  end
  else
    Result := E_INVALIDARG;
end;


procedure TRzCustomShellList.KeyDown( var Key: Word; ShiftState: TShiftState );
var
  oldk: Word;
  sc: TShortCut;

  function IsItThisOne( thisone: TRzShellControlDefKey ): Boolean;
  begin
    Result := ( sc = gShellControlDefKeys[ thisone ].shortcut );
  end;

begin
  inherited;
  if not IsEditing and not ( csDesigning in ComponentState ) then
  begin
    if ( sloDefaultKeyHandling in Options ) then
    begin
      sc := ShortCut( Key, ShiftState );
      oldk := key;
      key := 0;

      InitDefKeys;

      if IsItThisOne( scdkSelectAll ) then
        SelectAll
      else if IsItThisOne( scdkRefresh ) then
        FillItems
      else if IsItThisOne( scdkCopy ) then
        DoCommandForAllSelected( RZSH_CMDS_COPY )
      else if IsItThisOne( scdkCut ) then
        DoCommandForAllSelected( RZSH_CMDS_CUT )
      else if IsItThisOne( scdkPaste ) then
      begin
        DoCommandForFolder( RZSH_CMDS_PASTE );
        RefreshItems;
      end
      else if IsItThisOne( scdkEdit ) then
      begin
        if Assigned( ItemFocused ) then
          ItemFocused.EditCaption
      end
      else if IsItThisOne( scdkDelete ) then
      begin
        DoCommandForAllSelected( RZSH_CMDS_DELETE );
        RefreshItems;
      end
      else
      begin
        sc := ShortCut( oldk, ShiftState - [ ssShift ] );
        if IsItThisOne( scdkDelete ) then
        begin
          DoCommandForAllSelected( RZSH_CMDS_DELETE );
          RefreshItems;
        end
        else
          key := oldk;
      end;
    end;
  end;
end; {= TRzCustomShellList.KeyDown =}


procedure TRzCustomShellList.KeyPress( var key: Char );
begin
  if ( sloDefaultKeyHandling in Options ) and not IsEditing and not ( csDesigning in ComponentState ) then
  begin
    case key of
      #13:
        begin
          OpenSelectedItems;
          key := #0
        end;

      #8:
        begin
          GoUp( 1 );
          key := #0;
        end;
    end;
  end;
  inherited;
end;


procedure TRzCustomShellList.MouseDown( Button: TMouseButton; Shift: TShiftState; x, y: Integer );
begin
  if ( Button = mbLeft ) then
  begin
    inherited;
    if ( ssDouble in Shift ) then
      OpenSelectedItems;
  end
  else
    inherited;
end;


procedure TRzCustomShellList.ItemContextMenu( Item: TListItem; var P: TPoint; var Menu: TPopupMenu );
begin
  inherited;
  if ( sloContextMenus in Options ) and not ( csDesigning in ComponentState ) and not FDeferRefresh then
  begin
    ProcessMenuForAllSelected( P );
    Menu := nil;
  end;
end;


function TRzCustomShellList.ItemHasData( Item: TListItem ): Boolean;
begin
  Result := Assigned( Item ) and ( TObject( Item.Data ) is TRzShellListData );
end;


procedure TRzCustomShellList.InsertItem( Item: TListItem );
var
  col: Integer;
begin
  if Assigned( FCurrentItemData ) then
  begin
    Item.Data := FCurrentItemData;

    if ( FCurrentItemFlags and SFGAO_SHARE ) <> 0 then
      Item.OverlayIndex := 0
    else if ( FCurrentItemFlags and SFGAO_LINK ) <> 0 then
      Item.OverlayIndex := 1;

    if ( FCurrentItemFlags and SFGAO_GHOSTED ) <> 0 then
      Item.Cut := True;

    if Assigned( FCurrentItemIShd ) then
      for col := 1 to Columns.Count - 1 do
        Item.SubItems.Add( '' )
    else
      for col := 1 to 3 do
        Item.SubItems.Add( '' );

    FCurrentItemData := nil;
  end;
  ListView_SetItemText( Handle, Item.Index, 0, LPSTR_TEXTCALLBACK );

  inherited;
end; {= TRzCustomShellList.InsertItem =}


const
  WHEELSBID: array[ Boolean ] of DWORD = ( SB_VERT, SB_HORZ );


function TRzCustomShellList.DoMouseWheelDown( Shift: TShiftState; MousePos: TPoint ): Boolean;
//var
//  ctlat: TWinControl;
begin
  Result := inherited DoMouseWheelDown( Shift, MousePos );

  (*
  ctlat := FindVCLWindow( MousePos );
  if ctlat <> Self then
  begin
    if ( ctlat is TRzCustomShellTree ) then
      Result := TRzCustomShellTree( ctlat ).DoMouseWheelDown( Shift, MousePos )
    else if ( ctlat is TRzCustomShellList ) then
      Result := TRzCustomShellList( ctlat ).DoMouseWheelDown( Shift, MousePos );
  end;
  *)
  
  if not Result then
    Result := DoWheelScroll( Self, WHEELSBID[ ViewStyle = vsList ], SB_LINEDOWN, SB_PAGEDOWN );
end;


function TRzCustomShellList.DoMouseWheelUp( Shift: TShiftState; MousePos: TPoint ): Boolean;
//var
//  ctlat: TWinControl;
begin
  Result := inherited DoMouseWheelUp( Shift, MousePos );

  (*
  ctlat := FindVCLWindow( MousePos );
  if ctlat <> Self then
  begin
    if ( ctlat is TRzCustomShellTree ) then
      Result := TRzCustomShellTree( ctlat ).DoMouseWheelUp( Shift, MousePos )
    else if ( ctlat is TRzCustomShellList ) then
      Result := TRzCustomShellList( ctlat ).DoMouseWheelUp( Shift, MousePos );
  end;
  *)
  
  if not Result then
    Result := DoWheelScroll( Self, WHEELSBID[ ViewStyle = vsList ], SB_LINEUP, SB_PAGEUP );
end;


procedure TRzCustomShellList.InstallChangeHandler;
var
  p: Pointer;
begin
  {$IFDEF PTNOTHREADS}
  Exit;
  {$ENDIF}
  if FTimer.Enabled then
    FTimer.Enabled := False;

  if not Assigned( FChangeHandlerThread ) then
    FChangeHandlerThread := TRzChangeHandlerThread.Create( Handle, RZSH_AM_CHANGE_NOTIFY )
  else
    TRzChangeHandlerThread( FChangeHandlerThread ).RemoveAllMonitors;

  TRzChangeHandlerThread( FChangeHandlerThread ).AddMonitorDir( Folder.PathName,
                                                                FILE_NOTIFY_CHANGE_DIR_NAME or FILE_NOTIFY_CHANGE_FILE_NAME,
                                                                False, p );
end;


procedure TRzCustomShellList.FilterPreApply;
var
  I: Integer;
  fspec: string;
begin
  if Assigned( FFilterLookupTable ) then
    FilterPostApply;

  fspec := Folder.PathName;
  if ( fspec = '' ) or ( FFileFilter = '' ) or ( FFileFilter = '*' ) or ( FFileFilter = '*.*' ) then
    Exit;

  FilterInitLookupTable( FFilterLookupTable );

 // Add each filespec in turn
  if Length( fspec ) > 0 then
  begin
    fspec := EnsureTrailingCharDB( fspec, '\' );
    for I := 0 to FFilterExtensions.Count - 1 do // If Folder.PathName = '' then don't do any filtering
      if not FilterAddFilespecToLookupTable( FFilterLookupTable, fspec + FFilterExtensions[ I ] ) then
        Break;                              // If one fails, don't go on
  end;
end; {= TRzCustomShellList.FilterPreApply =}


function TRzCustomShellList.FilterApply( const FileName: string; Attrib: DWORD ): Boolean;
begin
  Result := RzShIsFolder( Attrib, sloFilesCanBeFolders in Options ) or
            FilterLookup( FFilterLookupTable, Filename );
end;


procedure TRzCustomShellList.FilterPostApply;
begin
  if Assigned( FFilterLookupTable ) then
    FilterFreeLookupTable( FFilterLookupTable );
  FFilterLookupTable := nil;
end;


function TRzCustomShellList.IsFolderNetworkShare: Boolean;
begin
  Result := Copy( Folder.PathName, 1, 2 ) = '\\';
end;



procedure TRzCustomShellList.TimerElapsed( Sender: TObject );
begin
  FTimer.Enabled := False;
  Inc( FIgnoreChanges );
  try
    Synchronize( True );
  finally
    Dec( FIgnoreChanges );
  end;
  RefreshItems;
end;


procedure TRzCustomShellList.AMChangeNotify( var Msg: TMessage );
begin
  if FIgnoreNextChangeNotify or FDeferRefresh then
  begin
    FIgnoreNextChangeNotify := False;
    Exit;
  end;
  FTimer.Enabled := False;
  if Focused then
    FTimer.Interval := RZSH_CHANGE_NOTIFY_FASTDELAY
  else
    FTimer.Interval := RZSH_CHANGE_NOTIFY_DELAY;
  FTimer.Enabled := True;
end;


procedure TRzCustomShellList.AMDeferredEdit( var Msg: TMessage );
var
  I: Integer;
begin
  for I := 0 to Items.Count - 1 do
    if ItemHasData( Items[ I ] ) and ( ShellListData[ I ].DisplayName = FDeferredEditName ) then
    begin
      SetFocus;
      Items[ I ].EditCaption;
      Exit;
    end;
end;


procedure TRzCustomShellList.AMDeferredFill( var Msg: TMessage );
begin
  if Items.Count = 0 then
    FillItems;
end;


procedure TRzCustomShellList.CMDesignHitTest( var Msg: TCMDesignHitTest );
begin
  if sloDesignInteractive in Options then
  begin
    if Assigned( GetItemAt( Msg.Pos.X, Msg.Pos.Y ) ) then
      Msg.Result := 1
    else
      Msg.Result := 0;
  end
  else
    Msg.Result := 0;
end;


procedure TRzCustomShellList.CMWantSpecialKey( var Msg: TCMWantSpecialKey );
begin
  inherited;
  if IsEditing or ( ( Msg.CharCode = VK_RETURN ) and Assigned( Selected ) and ItemHasData( Selected ) and TRzShellListData( Selected.Data ).IsFolder ) then
    Msg.Result := 1;
end;


procedure TRzCustomShellList.CNNotify( var Msg: TWMNotify );

  function GetItem( Value: TLVItem ): TListItem;
  begin
    with Value do
      if ( mask and LVIF_PARAM ) <> 0 then
        Result := TListItem( lParam )
      else
        Result := Items[ IItem ];
  end;

var
  item: TListItem;
  ld: TRzShellListData;
begin
  case Msg.nmhdr.code of
    LVN_GETDISPINFO:
      begin
        begin
          item := GetItem( PLVDispInfo( Pointer( Msg.NMHdr ) )^.item );
          with PLVDispInfo( Pointer( Msg.NMHdr ) )^.item do
          begin
            if ( ( mask and LVIF_TEXT ) <> 0 ) and Assigned( pszText ) then
            begin
              ld := GetDataFromItem( item );
              if iSubItem = 0 then
              begin
                if Assigned( pszText ) then
                begin
                  if ( item.Caption = '' ) and Assigned( ld ) then // Assigned(ld) works around a bug in comctl32 v4.00.950
                    item.Caption := ld.DisplayName;
                  StrPLCopy( pszText, item.Caption, cchTextMax );
                end;
              end
              else                          {// we can dynamically create the subitems as well} if ( iSubItem <= Item.SubItems.Count ) then
              begin
                if Item.SubItems[ iSubItem - 1 ] = '' then // Unless overriden, use the system string...
                  StrPLCopy( pszText, ld.ColText[ iSubItem ], cchTextMax )
                else                        // ...otherwise use the user's string.
                  StrPLCopy( pszText, Item.SubItems[ iSubItem - 1 ], cchTextMax );
              end
              else
                pszText[ 0 ] := #0;
            end;                            {if}

            if ( ( mask and LVIF_IMAGE ) <> 0 ) and ( iSubItem = 0 ) then
            begin
              {Can't call GetImageIndex since it's (unnecessarily) private in TCustomListView. Just
               do what it does directly.}
              if Assigned( OnGetImageIndex ) then
                OnGetImageIndex( Self, Item );

              if item.ImageIndex > 0 then
                iImage := item.ImageIndex
              else
              begin
                ld := GetDataFromItem( item );
                if Assigned( ld ) then
                begin
                  iImage := ShellGetIconIndex( ld.AbsoluteIdList, SHGFI_SMALLICON );
                end;
              end;
            end;                            {if}
//          mask := mask or LVIF_DI_SETITEM;
          end;                              {with}
        end {if};
      end;                                  {LVN_GETDISPINFO}

    LVN_BEGINDRAG:
      begin
        if ( sloOleDrag in Options ) then
          OleBeginDrag( mbLeft );
      end;

    LVN_BEGINRDRAG:
      begin
        if ( sloOleDrag in Options ) then
          OleBeginDrag( mbRight );
      end;

  else
    inherited;
  end;
end; {= TRzCustomShellList.CNNotify =}


procedure TRzCustomShellList.WMMenuChar( var Msg: TWMMenuChar );
begin
  inherited;
  SendTo_WMMenuChar( Msg, FActiveContextMenu, _sl );
end;


procedure TRzCustomShellList.WMDrawItem( var Msg: TWMDrawItem );
begin
  inherited;
  SendTo_WMDrawItem( Msg, FActiveContextMenu, SmallImages, _sl );
end;


procedure TRzCustomShellList.WMMeasureItem( var Msg: TWMMeasureItem );
begin
  inherited;
  SendTo_WMMeasureItem( Msg, FActiveContextMenu, _sl );
end;


procedure TRzCustomShellList.WMMenuSelect( var Msg: TWMMenuSelect );
begin
  inherited;
  Popup_WMMenuSelect( Msg, FActiveContextMenu, Self, OnPopupHint );
end;


procedure TRzCustomShellList.WMDestroy( var Msg: TWMDestroy );
begin
  if ( Columns.Count > 0 ) and ( FLastFolderWasDir ) then
    WriteCurrentColWidths( Self, g_LastColWidths );

  if Assigned( FChangeHandlerThread ) then
  begin
    TObject( FChangeHandlerThread ).Free;
    FChangeHandlerThread := nil;
  end;

  if Assigned( FIDropTarget ) and HandleAllocated then
  begin
    RevokeDragDrop( Handle );
    FIDropTarget.Release;
    FIDropTarget := nil;
  end;

  inherited;
end;


procedure TRzCustomShellList.WMNCDestroy( var Msg: TWMNCDestroy );
begin
  inherited;
end;


procedure TRzCustomShellList.WMNCHitTest( var Msg: TWMNCHitTest );
begin
  DefaultHandler( Msg );
end;                                        // Bypass design time hooks to allow design time interactive scrollbars


procedure TRzCustomShellList.WMRButtonUp( var Msg: TWMRButtonUp );
begin
  if FSkipRButtonUp then
    DefaultHandler( Msg )
  else
    inherited;
end;


procedure TRzCustomShellList.WMInitMenuPopup( var Msg: TWMInitMenuPopup );
begin
  inherited;
  SendTo_WMInitMenuPopup( Msg, FActiveContextMenu, _sl );
end;


procedure TRzCustomShellList.WMWindowPosChanged( var Msg: TWMWindowPosChanged );
begin
  if not FDeletingItems then
    inherited;
end;


function _SHGetInstanceExplorer( var ppUnk: Pointer ): HResult; stdcall; external 'shell32.dll' name 'SHGetInstanceExplorer';

{This message can be sent to us when we call IShellFolder.CreateViewObject. We must respond with
 the IShellBrowser interface of explorer.}

procedure TRzCustomShellList.WMGetIShellBrowser( var Msg: TMessage );
var
  iunk: IUnknown_NRC;
begin
  _SHGetInstanceExplorer( Pointer( iunk ) );
  iunk.QueryInterface( IID_IShellBrowser, Pointer( Msg.Result ) );
  iunk.Release;
end;



{=================================}
{== TRzCustomShellCombo Methods ==}
{=================================}

constructor TRzCustomShellCombo.Create( aOwner: TComponent );
begin
  inherited;
  {&RCI}
  Images := TImageList.CreateSize( 16, 16 );
  Images.Handle := ShellGetSystemImageList( iszSmall );
  Images.ShareImages := True;

  FSelectedFolder := TRzShellLocator.Create;
  FSelectedFolder.OnChange := SelectedFolderChanged;

  RzDeviceChangeHandler.Add( DeviceChangeDetected );

  FOptions := [ scoAutofill, scoNonFilesystemAncestors ];
  FAutoDropDownCount := True;
  {&RV}
end;


destructor TRzCustomShellCombo.Destroy;
begin
  RzDeviceChangeHandler.Remove( DeviceChangeDetected );
  Images.Free;
  FSelectedFolder.Free;
  inherited;
end;


function TRzCustomShellCombo.CanAdd( ParentIShf: IShellFolder_NRC; ParentAbsPidl, ItemRelPidl: PItemIdList; attribs: DWORD; Level: Integer ): Boolean;
begin
  Result := True;
end;


procedure TRzCustomShellCombo.SelEndOk;
begin
  inherited;
  SelectedFolder.IdList := ( TObject( ImageComboItem[ ItemIndex ].Data ) as TRzShellComboData ).AbsoluteIdList;
end;


procedure TRzCustomShellCombo.CreateWnd;
begin
  inherited;
  if not ( csLoading in ComponentState ) and ( scoAutoFill in Options ) then
    FillItems;
end;

procedure TRzCustomShellCombo.DestroyWnd;
begin
  // Call Clear to prevent TCustomCombobox.DestroyWnd from streaming out Items.
  Clear;
  inherited;
end;


procedure TRzCustomShellCombo.Loaded;
begin
  if not ( csLoading in ComponentState ) then
    Exit;
  inherited;
  if ( scoAutofill in Options ) then
  begin
    Inc( FIgnoreChanges );
    try
      FillItems;
      if Assigned( FShellTree ) then
      begin
        FShellTree.SelectedFolder := SelectedFolder;
        FShellTree.RefreshNodes;
      end
      else if Assigned( FShellList ) then
        FShellList.Folder := SelectedFolder;
    finally
      Dec( FIgnoreChanges );
    end;
  end;
end;


procedure TRzCustomShellCombo.DeleteItem( Item: Pointer );
begin
  if Assigned( OnDeleteItem ) then
    OnDeleteItem( Self, TRzImageComboBoxItem( Item ) );

  if Assigned( Item ) then
  begin
    if Assigned( TRzImageComboBoxItem( Item ).Data ) then
    begin
      ( TObject( TRzImageComboBoxItem( Item ).Data ) as TRzShellComboData ).Free;
      TRzImageComboBoxItem( Item ).Data := nil;
    end;
    TObject( Item ).Free;
  end;
end;


procedure TRzCustomShellCombo.DropDown;
var
  ControlBottomOnScreen: Integer;
begin
  if AutoDropDownCount then
  begin
    ControlBottomOnScreen := ClientToScreen( Point( 0, Top + Height ) ).Y;
    DropDownCount := ( Screen.Height - ControlBottomOnScreen - 1 ) div ItemHeight;
  end;

  inherited;
end;


procedure TRzCustomShellCombo.GetItemData( Item: TRzImageComboBoxItem );
var
  Data: TRzShellComboData;
  IconOption: DWORD;
begin
  Data := ShellComboData[ Item.Index ];
  if Assigned( Data ) then
  begin
    if Item.Caption = '' then
      Item.Caption := ShellGetFriendlyNameFromIdList( Data.ParentIShf, Data.RelativeIdList, fnInFolder );

    if Item.ImageIndex = -1 then
    begin
      // if (currently open item) then IconOption = SHGFI_OPEN else IconOption := 0;
      IconOption := 0;
      Item.ImageIndex := ShellGetIconIndex( Data.AbsoluteIdList, SHGFI_SMALLICON or IconOption );
    end;
  end;
end;


procedure TRzCustomShellCombo.SelectedFolderChanged( Sender: TObject );
begin
  if ( FIgnoreChanges <= 0 ) then
  begin
    Inc( FIgnoreChanges );
    try
      if Assigned( FShellTree ) then
        FShellTree.SelectedFolder := SelectedFolder
      else if Assigned( FShellList ) then
        FShellList.Folder := SelectedFolder;
      FillItems;
    finally
      Dec( FIgnoreChanges )
    end;
  end;
end;


procedure TRzCustomShellCombo.TreeChanged( Node: TTreeNode );
begin
  if Assigned( Node ) and Node.Selected and Assigned( Node.Data ) then
    with TRzShellTreeData( Node.Data ) do
      SelectedFolder.IdList := {.} AbsoluteIdList;
end;


procedure TRzCustomShellCombo.DeviceChangeDetected( Sender: TObject; var Msg: TMessage );
begin
  if ( Msg.WParam = DBT_DEVICEARRIVAL ) or ( Msg.WParam = DBT_DEVICEREMOVECOMPLETE ) then
  begin
    FlushDriveInfoCache;
    LockFlushDriveInfoCache;
    try
      FillItems;
    finally
      UnlockFlushDriveInfoCache;
    end;
  end;
end;



{ toSort must all be pidls relative to ishf. }
var
  _sortIShf: IShellFolder_NRC = nil;

function _SortPidlsCompare( p1, p2: Pointer ): Integer;
var
  hres: HResult;
begin
  hres := _sortIShf.CompareIds( 0, p1, p2 );
  if Succeeded( hres ) then
    Result := Smallint( ResultCode( hres ) )
  else
    Result := 0;
end;                                        {_SortPidlsCompare}

procedure SortPidls( ishf: IShellFolder_NRC; toSort: TList );
begin
  if Assigned( _sortIShf ) then
    raise Exception.Create( 'Recursive call to SortPidls' );
  _sortIShf := ishf;
  try
  toSort.Sort( _SortPidlsCompare )finally _sortIShf := nil
  end;
end;                                        {SortPidls}


procedure TRzCustomShellCombo.FillCombo( aIShf: IShellFolder_NRC; aBasePidl: PItemIdList; aIndent: Integer;
                                         aSelectedItem: TRzIdListArray );

  procedure AddAllSelected( aCurIShf: IShellFolder_NRC; aCurAbsPidl, aCurRelPidl: PItemIdList );
  var
    I, max: Integer;
    attrib, opt: DWORD;
    newishf, thisIShf: IShellFolder_NRC;
    thisPidl, curRelPidl, curAbsPidl: PItemIdList;
    newitem: TRzImageComboBoxItem;
  begin
    max := aSelectedItem.ItemCount - 1;
    curAbsPidl := nil;
    curRelPidl := nil;
    newishf := nil;
    thisishf := nil;
    thispidl := nil;
    try
      OleCheck( aCurIShf.BindToObject( aCurRelPidl, nil, IID_IShellFolder, Pointer( newishf ) ) );
      thisIShf := newishf;
      newishf := nil;
      thisPidl := CopyIdList( nil, aCurAbsPidl );

      for I := aIndent to max do
      begin
        attrib := ( SFGAO_HASSUBFOLDER or SFGAO_FOLDER or SFGAO_DISPLAYATTRMASK or SFGAO_FILESYSANCESTOR ) and not SFGAO_READONLY;
        curRelPidl := aSelectedItem[ I ];   // Don't need to free
        thisIShf.GetAttributesOf( 1, curRelPidl, attrib );

        curAbsPidl := ConcatIdLists( nil, thisPidl, curRelPidl );
        if ( I = max ) then
          opt := SHGFI_OPENICON
        else
          opt := 0;
        if opt = 0 then
          newitem := AddItem( '',
            -1,
            I + 1 )
        else
          newitem := AddItem( GetFriendlyName( thisIShf, curRelPidl, SHGDN_INFOLDER or SHGDN_FORADDRESSBAR ),
            ShellGetIconIndex( curAbsPidl, SHGFI_SMALLICON or opt ),
            I + 1 );

        newitem.Data := TRzShellComboData.Create( Self );
        TRzShellComboData( newitem.Data ).SetData( thisIShf, thisPidl, curRelPidl );

        if ( attrib and SFGAO_SHARE ) <> 0 then
          newitem.OverlayIndex := 0
        else if ( attrib and SFGAO_LINK ) <> 0 then
          newitem.OverlayIndex := 1;

        OleCheck( thisIShf.BindToObject( curRelPidl, nil, IID_IShellFolder, Pointer( newishf ) ) );
        thisIShf.Release;
        thisIShf := newishf;
        newishf := nil;
        ShellMemFree( thisPidl );
        thisPidl := curAbsPidl;
        curAbsPidl := nil;
      end;
      Self.ItemIndex := Self.Count - 1;
    finally
      if Assigned( thisIShf ) then
        thisIShf.Release;
      ShellMemFree( curAbsPidl );
      ShellMemFree( thisPidl );
      if Assigned( newishf ) then
        newishf.Release;
    end;
  end;                                      {AddAllSelected}

var
  dw, dummy, attrib: DWORD;
  wval: Word;
  newishf: IShellFolder_NRC;
  ienum: IEnumIdList_NRC;
  basepidl, curRelPidl, curAbsPidl: PItemIdList;
  myComputerPidl: PItemIdList;              // Absolute pidl of My Computer - we recurse down this one by default
  imgidx: Integer;
  newitem: TRzImageComboBoxItem;
  fInclude: Bool;

  I: Integer;
  childList: TList;

  function IsMyComputer: Bool;
  begin
    Result := ( ( aIndent = 1 ) and ( RzShellUtils.CompareAbsIdLists( curAbsPidl, myComputerPidl ) = 0 ) );
  end;

begin                                       {TRzCustomShellCombo.FillCombo}
  childList := TList.Create;
  ienum := nil;
  basepidl := nil;
  curRelPidl := nil;
  curAbsPidl := nil;
  newishf := nil;
  myComputerPidl := nil;
  try
    dw := ShellGetSpecialFolderIdList( GetValidParentHWND( Self ), csidlDrives, myComputerPidl );
    if ( dw <> NOERROR ) then
      RaiseSysError( dw );

    if ( aIndent < 0 ) then
    begin                                   {Special case when creating the base enumeration - we add an item for ishf itSelf.}
      wval := 0;
      basepidl := nil;
      imgidx := ShellGetIconIndex( Pointer( @wval ), SHGFI_SMALLICON );

      newitem := AddItem( ShellGetFriendlyNameForLastIdListElement( basepidl ), imgidx, 0 );
      newitem.Data := TRzShellComboData.Create( Self );
      TRzShellComboData( newitem.Data ).SetData( aIShf, nil, nil );

      aIndent := 1;
    end
    else
      basepidl := CopyIdList( nil, aBasePidl );

    dw := aIShf.EnumObjects( GetValidParentHWND( Self ), SHCONTF_FOLDERS, ienum );
    if ( dw <> S_OK ) or not Assigned( ienum ) then
      RaiseSysError( dw );

    dw := ienum.Next( 1, curRelPidl, PInteger( @dummy ) );
    while ( dw = S_OK ) do
    begin
      childList.Add( curRelPidl );
      dw := ienum.Next( 1, curRelPidl, PInteger( @dummy ) );
    end;
    if Failed( dw ) then
      RaiseSysError( dw );
    ienum.Release;
    ienum := nil;

    SortPidls( aIShf, childList );

    for I := 0 to childList.Count - 1 do
    begin
      attrib := ( SFGAO_HASSUBFOLDER or SFGAO_FOLDER or SFGAO_DISPLAYATTRMASK or SFGAO_FILESYSANCESTOR or SFGAO_FILESYSTEM ) and not SFGAO_READONLY;
      curRelPidl := childList[ I ];
      aIShf.GetAttributesOf( 1, curRelPidl, attrib );

      if not ( scoNonFilesystemAncestors in Options ) and
        ( ( attrib and ( SFGAO_FILESYSTEM or SFGAO_FILESYSANCESTOR ) ) = 0 ) then
        fInclude := False
      else
        fInclude := True;
      fInclude := fInclude and CanAdd( aIShf, basepidl, curRelPidl, attrib, aIndent );

      if fInclude then
      begin
        curAbsPidl := ConcatIdLists( nil, basepidl, curRelPidl );
        dw := 0;
        if Assigned( aSelectedItem ) and
          ( aSelectedItem.ItemCount >= aIndent ) and
          ( aIShf.CompareIDs( 0, aSelectedItem[ aIndent - 1 ], curRelPidl ) = 0 ) and
          ( aSelectedItem.ItemCount = aIndent ) then
          dw := SHGFI_OPENICON;

        if dw = 0 then
          newitem := AddItem( '', -1, aIndent )
        else
          newitem := AddItem( ShellGetFriendlyNameFromIdList( aIShf, curRelPidl, fnInFolder ),
                              ShellGetIconIndex( curAbsPidl, SHGFI_SMALLICON or dw ), aIndent );

        newitem.Data := TRzShellComboData.Create( Self );
        TRzShellComboData( newitem.Data ).SetData( aIShf, basepidl, curRelPidl );


        if ( attrib and SFGAO_SHARE ) <> 0 then
          newitem.OverlayIndex := 0
        else if ( attrib and SFGAO_LINK ) <> 0 then
          newitem.OverlayIndex := 1;

        if Assigned( aSelectedItem ) then
          if ( aSelectedItem.ItemCount >= aIndent ) then
            if ( aIShf.CompareIDs( 0, aSelectedItem[ aIndent - 1 ], curRelPidl ) = 0 ) then
            begin                           // The current item matches the relevant part of the selected item
              if ( aSelectedItem.ItemCount = aIndent ) then
              begin                         // If this is the last part if selected item...
                Self.ItemIndex := Self.Count - 1; // Last element of selected item, so we've found it and we're done
                aSelectedItem := nil;
              end;

              if Assigned( aSelectedItem ) and not IsMyComputer then
              begin
                AddAllSelected( aIShf, curAbsPidl, curRelPidl );
                aSelectedItem := nil;
              end;
            end;

        if IsMyComputer then
        begin
          OleCheck( aIShf.BindToObject( curRelPidl, nil, IID_IShellFolder, Pointer( newishf ) ) );
          FillCombo( newishf, curAbsPidl, aIndent + 1, aSelectedItem );
          newishf.Release;
          newishf := nil;
        end;

        ShellMemFree( curAbsPidl );
        curAbsPidl := nil;
       // childList[] pidls are cleaned up in the finally block
      end;                                  {if fInclude}
    end;
  finally
    ShellMemFree( basepidl );
    ShellMemFree( curAbsPidl );
    ShellMemFree( myComputerPidl );
    for I := 0 to childList.Count - 1 do
      ShellMemFree( childList[ I ] );
    if Assigned( ienum ) then
      ienum.Release;
    if Assigned( newishf ) then
      newishf.Release;
    childList.Free;
  end;
end;                                        {TRzCustomShellCombo.FillCombo}


procedure TRzCustomShellCombo.FillItems;
var
  shf: IShellFolder_NRC;
  dw: DWORD;
  pidla: TRzIdListArray;
begin
  if not HandleAllocated then
    Exit;
  pidla := nil;
  ItemsBeginUpdate;
  try
    Inc( FIgnoreChanges );
    try
      ItemIndex := -1;
      ClearItems;
      ItemsEndUpdate;
    finally
      Dec( FIgnoreChanges );
    end;

    pidla := TRzIdListArray.Create( FSelectedFolder.IdList );

    ItemsBeginUpdate;
    dw := ShellGetDesktopFolder( shf );
    if ( dw <> S_OK ) then
      RaiseSysError( dw );

    FillCombo( shf, nil, -1, pidla );
    if ItemIndex < 0 then
      ItemIndex := 0;
  finally
    if Assigned( shf ) then
      shf.Release;
    pidla.Free;
    ItemsEndUpdate;
  end;
end; {= TRzCustomShellCombo.FillItems =}


procedure TRzCustomShellCombo.GoUp( Levels: Integer );
var
  I, targetLevel: Integer;
begin
  if ItemIndex < 0 then
    Exit;
  I := ItemIndex;
  targetLevel := ImageComboItem[ I ].IndentLevel - Levels + 1;
  while ( I > 0 ) and ( ImageComboItem[ I ].IndentLevel >= targetLevel ) do
    Dec( I );
  if ( ItemIndex <> I ) then
    SelectedFolder.IdList := ShellComboData[ I ].AbsoluteIdList;
end;


procedure TRzCustomShellCombo.Synchronize( ApplyToGroup: Boolean );
begin
  if ApplyToGroup then
  begin
    if Assigned( FShellTree ) then
      FShellTree.Synchronize( False );
    if Assigned( FShellList ) then
      FShellList.Synchronize( False );
  end;
end;


procedure TRzCustomShellCombo.CMDesignHitTest( var Msg: TCMDesignHitTest );
begin
  if Msg.xPos > ( ClientWidth - GetSystemMetrics( SM_CXVSCROLL ) ) then
    Msg.Result := 1
  else
    Msg.Result := 0;
  inherited;
end;


function TRzCustomShellCombo.GetSelectedFolder: TRzShellLocator;
begin
  Result := FSelectedFolder;
end;


function TRzCustomShellCombo.GetShellComboData( Index: Integer ): TRzShellComboData;
begin
//  Result := TObject( ( Items.Objects[ Index ] as TRzImageComboBoxItem ).Data ) as TRzShellComboData;
  Result := TObject( ImageComboItem[ Index ].Data ) as TRzShellComboData;
end;


procedure TRzCustomShellCombo.SetSelectedFolder( Value: TRzShellLocator );
begin
  FSelectedFolder.Assign( Value );
end;


procedure TRzCustomShellCombo.SetShellList( Value: TRzCustomShellList );
begin
  if Assigned( Value ) and Assigned( FShellTree ) then
    Exit;                                   // Only one can be set - so reject change.
  if Assigned( FShellList ) and ( Value <> FShellList ) then
    FShellList.SetShellCombo( nil );
  FShellList := Value;
  if Assigned( FShellList ) then
  begin
    FShellList.FreeNotification( Self );
    FShellList.SetShellCombo( Self );
  end;
end;


procedure TRzCustomShellCombo.SetShellTree( Value: TRzCustomShellTree );
begin
  if Assigned( Value ) and Assigned( FShellList ) then
    Exit;                                   // Only one can be set - so reject change.
  if Assigned( FShellTree ) and ( Value <> FShellTree ) then
    FShellTree.SetShellCombo( nil );
  FShellTree := Value;
  if Assigned( FShellTree ) then
  begin
    FShellTree.FreeNotification( Self );
    FShellTree.SetShellCombo( Self );
  end;
end;


procedure TRzCustomShellCombo.WMPaint( var Msg: TWMPaint );
var
  OldBkColor: TColorRef;
begin
  if not ( csDestroying in ComponentState ) then
  begin
    OldBkColor := ImageList_GetBkColor( Images.Handle );
    try                                     // Use the API to prevent a Change event occuring for the imagelist component
      ImageList_SetBkColor( Images.Handle, ColorToRGB( Color ) );
      inherited;
    finally
      ImageList_SetBkColor( Images.Handle, OldBkColor );
    end;
  end;
end;                                        {TRzCustomShellCombo.WMPaint}



{**************************************
  TRzShellLocator
**************************************}

destructor TRzShellLocator.Destroy;
begin
  ShellMemFree( FIdList );
  inherited;
end;


procedure TRzShellLocator.Assign( Source: TPersistent );
var
  src: TRzShellLocator;
begin
  if not Assigned( source ) then
  begin
    if not Assigned( FIdList ) then
      Exit;

   // Cleanup existing state
    ShellMemFree( FIdList );
    FIdList := nil;
    FWhich := usePidl;
    FIdList := nil;
    Changed;
  end
  else if Source is TRzShellLocator then
  begin
    if IsEqual( TRzShellLocator( Source ) ) then
      Exit;                                 // No need to do anything

   // Cleanup existing state
    ShellMemFree( FIdList );
    FIdList := nil;

   // Copy in new state
    src := TRzShellLocator( Source );
    FWhich := src.FWhich;
    if FWhich = usePidl then
      FIdList := CopyIdList( nil, src.FIdList )
    else
    begin
      FIdList := nil;
      FCSIDL := src.FCSIDL;
    end;
    Changed;
  end
  else
    inherited;
end; {= TRzShellLocator.Assign =}


function TRzShellLocator.IsEqual( Value: TRzShellLocator ): Boolean;
var
  I: Integer;
begin
  if Self.FWhich <> Value.FWhich then
    Result := False
  else if Self = Value then
    Result := True
  else
  begin
    case FWhich of
      useCSIDL:
        begin
          Result := ( Self.FCSIDL = Value.FCSIDL );
        end;

      usePidl:
        begin
          I := RzShellUtils.CompareAbsIdLists( FIdList, Value.FIdList );
          Result := ( I <> MAXINT ) and ( I = 0 );
        end;

    else
      Result := False;                      // prevent warning
    end;                                    {case}
  end;
end;

procedure TRzShellLocator.Clear;
begin
  IdList := nil;
end;


procedure TRzShellLocator.Changed;
begin
  if Assigned( FOnChange ) then
    FOnChange( Self );
end;


function TRzShellLocator.GetPathName: string;
begin
  Result := ShellGetPathFromIdList( IdList );
end;


function TRzShellLocator.GetIdList: PItemIdList;
begin
  case FWhich of
    usePIDL:
      Result := FIdList;

    useCSIDL:
      begin
        if not Assigned( FIdList ) then
          SHGetSpecialFolderLocation( Application.Handle, Integer( FCSIDL ), FIdList );
        Result := FIdList;
      end;
  else
    Result := nil;
  end;
end;                                        {TRzShellLocator.GetIdList}


function TRzShellLocator.GetCSIDL: TCSIDL;
begin
  if FWhich = useCSIDL then
    Result := FCSIDL
  else
    Result := csidlNone;
end;                                        {TRzShellLocator.GetCSIDL}


procedure TRzShellLocator.SetIdList( Value: PItemIdList );
var
  fChanged: Boolean;
  I: Integer;
begin
  fChanged := ( FWhich <> usePidl );
  FWhich := usePidl;

  if Assigned( FIdList ) then
  begin
    if not fChanged then
    begin
      I := CompareAbsIdLists( FIdList, Value );
      fChanged := ( I <> MAXINT ) and ( I <> 0 );
    end;
    ShellMemFree( FIdList );
  end
  else
    fChanged := fChanged or ( Assigned( Value ) and ( PWord( Value )^ <> 0 ) );

  if Assigned( Value ) and ( IdListLen( Value ) > 2 ) then
    FIdList := CopyIdList( nil, Value )
  else
    FIdList := nil;
  FCSIDL := csidlNone;

  if fChanged then
    Changed;
end; {= TRzShellLocator.SetIdList =}


procedure TRzShellLocator.SetCSIDL( Value: TCSIDL );
var
  fChanged: Boolean;
begin
  ShellMemFree( FIdList );
  FIdList := nil;
  fChanged := ( FWhich <> useCSIDL ) or ( ( FWhich = useCSIDL ) and ( FCSIDL <> Value ) );
  FWhich := useCSIDL;
  FCSIDL := Value;
  if fChanged then
    Changed;
end;


procedure TRzShellLocator.SetPathName( const Value: string );
var
  p: PItemIdList;
begin
  if ShellGetIdListFromPath( Value, p ) = S_OK then
  try
    IdList := p;
  finally ShellMemFree( p );
  end
  else
    IdList := nil;
end;                                        {TRzShellLocator.SetPathName}


procedure TRzShellLocator.ReadData( Stream: TStream );
var
  dw: DWORD;
begin
  Stream.ReadBuffer( FWhich, Sizeof( FWhich ) );
  if FWhich = usePidl then
  begin
    Stream.ReadBuffer( dw, Sizeof( dw ) );
    ShellMemFree( FIdList );
    if dw > 0 then
    begin
      FIdList := ShellMemAlloc( dw );
      Stream.ReadBuffer( FIdList^, dw );
    end
    else                                    // translate "pidl pointing to desktop" to "csidlDesktop"
    begin
      FWhich := useCsidl;
      FCSIDL := csidlDesktop;
      FIdList := nil;
    end;
  end
  else                                      {useCsidl}
  begin
    Stream.ReadBuffer( FCSIDL, Sizeof( FCSIDL ) );
    FWhich := useCsidl;                     // In case we read an invalid value, make it valid
  end;
end; {= TRzShellLocator.ReadData =}


procedure TRzShellLocator.WriteData( Stream: TStream );
var
  dw: DWORD;
begin
  dw := IdListLen( FIdList );
  Stream.WriteBuffer( FWhich, Sizeof( FWhich ) );
  if FWhich = usePidl then
  begin
    Stream.WriteBuffer( dw, Sizeof( dw ) );
    if Assigned( FIdList ) then
      Stream.WriteBuffer( FIdList^, dw );
  end
  else
    Stream.WriteBuffer( FCSIDL, Sizeof( FCSIDL ) );
end;


procedure TRzShellLocator.DefineProperties( Filer: TFiler );

  function NeedWrite: Boolean;
  begin
    Result := Assigned( FIdList ) or ( FWhich = useCSIDL );
  end;

begin
  inherited DefineProperties( Filer );
  Filer.DefineBinaryProperty( 'Pidl', ReadData, WriteData, NeedWrite );
end;


function CreateShellDetailsAdapter( const AShellFolder: IShellFolder_NRC ): IShellDetails_NRC;
var
  info: TShColInfo;
  ShellFolder2: IShellFolder2_NRC;
begin
  ShellFolder2 := nil;
  try
    if Succeeded( AShellFolder.QueryInterface( IID_IShellFolder2, ShellFolder2 ) ) then
    begin
      if Succeeded( ShellFolder2.GetDetailsOf( nil, 0, info ) ) then
        TShellFolder2ToShellDetailsAdapter.Create( ShellFolder2 ).QueryInterface( IID_IShellDetails, Result )
      else if Failed( AShellFolder.QueryInterface( IID_IShellDetails, Result ) ) then
        if Failed( AShellFolder.CreateViewObject( 0, IID_IShellDetails, Pointer( Result ) ) ) then
          Result := nil;
    end
    else if Failed( AShellFolder.CreateViewObject( 0, IID_IShellDetails, Pointer( Result ) ) ) then
      Result := nil;
  finally
    if Assigned( ShellFolder2 ) then
      ShellFolder2.Release;
  end;
end;                                        {CreateShellDetailsAdapter}



{ TShellFolder2ToShellDetailsAdapter }

constructor TShellFolder2ToShellDetailsAdapter.Create( AShellFolder: IShellFolder2_NRC );
begin
  inherited Create;
  FShellFolder2 := AShellFolder;
  FShellFolder2.AddRef;
end;

destructor TShellFolder2ToShellDetailsAdapter.Destroy;
begin
  FShellFolder2.Release;
  inherited;
end;

function TShellFolder2ToShellDetailsAdapter.QueryInterface( const IID: TGUID; var Obj ): HResult;
begin
  if IsEqualIID( iid, IID_IUnknown ) or IsEqualIID( iid, IID_IShellDetails ) then
  begin
    Pointer( obj ) := Self;
    AddRef;
    Result := S_OK;
  end
  else
  begin
    Pointer( obj ) := nil;
    Result := HResult( E_NOINTERFACE );
  end;
end;

function TShellFolder2ToShellDetailsAdapter.AddRef: Integer;
begin
  InterlockedIncrement( FCount );
  Result := FCount;
end;

function TShellFolder2ToShellDetailsAdapter.Release: Integer;
begin
  InterlockedDecrement( FCount );
  Result := FCount;
  if FCount = 0 then
    Free;
end;

function TShellFolder2ToShellDetailsAdapter.GetDetailsOf( pidl: PItemIdList; col: UINT; var info: TShColInfo ): HResult;
var
  Flags: DWORD;
begin
  Result := FShellFolder2.GetDefaultColumnState( col, Flags );
  if ( Succeeded( Result ) and ( ( Flags and $00000010 ) <> 0 ) ) or Failed( Result ) then
    Result := FShellFolder2.GetDetailsOf( pidl, col, info )
  else
    Result := E_INVALIDARG;
end;

function TShellFolder2ToShellDetailsAdapter.ColumnClick( col: UINT ): HResult;
begin
  Result := E_NOTIMPL;
end;


{$IFDEF PTDEBUG}

procedure DebugCheck;
var
  lines: Integer;
  s: string;
  procedure ShowIt;
  begin
    MessageBox( 0, PChar( s ), 'Debug - Leak(s) Detected',
      MB_OK or MB_APPLMODAL or MB_SETFOREGROUND or MB_ICONERROR );
  end;

  procedure Tryit( c: Integer; es: string );
  begin
    if ( c <> 0 ) then
    begin
      if ( s <> '' ) then
        s := s + #13#10;
      s := s + Format( '  %d %s', [ c, es ] );
      Inc( lines );
    end;
    if ( lines > 30 ) then
    begin
      ShowIt;
      s := '';
    end;
  end;
begin
  lines := 0;
  TryIt( g_TreeNodes, 'TRzCustomShellTree nodes' );
  TryIt( g_ListNodes, 'TRzCustomShellList nodes' );
  TryIt( g_ComboNodes, 'TRzCustomShellCombo nodes' );

  TryIt( g_DataObject_shlv, 'TDataObject_shlv' );
  TryIt( g_DropTarget_shlv, 'TDropTarget_shlv' );

  TryIt( g_DropSource_shlvtv, 'TDropSource_shlv' );

  TryIt( g_DataObject_shtv, 'TDataObject_shtv' );
  TryIt( g_DropTarget_shtv, 'TDropTarget_shtv' );

  TryIt( g_FormatEtcList, 'TRzFormatEtcList' );
  TryIt( g_FormatEtcList_IEnumFormatEtc, 'TRzFormatEtcList_IEnumFormatEtc' );

  TryIt( g_ChangeHandlerThread, 'TChangeHandlerThread' );

  if s <> '' then
    ShowIt;
end;
{$ENDIF}



procedure Init;
var
  Default: DWORD;
begin
  CF_IDLIST := RegisterClipboardFormat( CFSTR_SHELLIDLIST );
  g_LastColWidths := TMemoryStream.Create;
  ZeroMemory( @g_LastRegistryColData, Sizeof( g_LastRegistryColData ) );
  OleInitialize( nil );

  if SystemParametersInfo( SPI_GETMOUSEHOVERTIME, 0, @Default, 0 ) then
    RZSH_AUTOOPEN_DELAY_MS := Default;

  if SystemParametersInfo( SPI_GETMOUSEHOVERWIDTH, 0, @Default, 0 ) then
    RZSH_AUTOOPEN_THRESHOLD_X := Default;

  if SystemParametersInfo( SPI_GETMOUSEHOVERHEIGHT, 0, @Default, 0 ) then
    RZSH_AUTOOPEN_THRESHOLD_Y := Default;
end;


initialization
  Init;
  {&RUI}
finalization
  {$IFDEF PTDEBUG}
  DebugCheck;
  {$ENDIF}
  _sl.Free;
  g_LastColWidths.Free;
  OleUninitialize;
end.

