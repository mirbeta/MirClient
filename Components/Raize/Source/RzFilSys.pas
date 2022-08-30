{===============================================================================
  RzFilSys Unit

  Raize Components - Component Source Unit

  Copyright © 1995-2015 by Raize Software, Inc.  All Rights Reserved.


  Components
  ------------------------------------------------------------------------------
  TRzFileListBox
    Enhanced TFileListBox--supports custom framing, long file names, shell icons.

  TRzDirectoryTree
    Tree view that displays directories and sub-directories.

  TRzDirectoryListBox
    Enhanced TDirectoryListBox--supports custom framing, long file names,
    shell icons.

  TRzDriveComboBox
    Enhanced TDriveComboBox--supports custom framing, new glyphs.


  Modification History
  ------------------------------------------------------------------------------
  4.0    (23 Dec 2005)
    * Redesigned OnMouseEnter and OnMouseLeave events in TRzFileListBox,
      TRzDirectoryListBox, and TRzDrieComboBox to account for changes introduced
      in Borland Developer Studio 2006.
    * Fixed custom framing display problem that would occur in TRzFileListBox,
      TRzDirectoryListBox, and TRzDriveComboBox when FrameVisible was set to
      True and changes were made to control's appearance within calls to
      LockWindowUpdate.
  ------------------------------------------------------------------------------
  3.0.10 (26 Dec 2003)
    * Fixed problem where changing ParentColor to True in a control using Custom
      Framing did not reset internal color fields used to manage the color of
      the control at various states.
  ------------------------------------------------------------------------------
  3.0.3  (21 Jan 2003)
    * Added StoreColor and StoreFocusColor methods so that if control is
      disabled at design-time the Color and FocusColor properties are not
      streamed with the disabled color value.
  ------------------------------------------------------------------------------
  3.0    (20 Dec 2002)
    * Deprecated.  These compoents have been replaced with the new Shell
      controls: TRzShellTree, TRzShellList, and TRzShellCombo.
    * Add FocusColor and DisabledColor properties.
    * Renamed FrameFlat property to FrameHotTrack.
    * Renamed FrameFocusStyle property to FrameHotStyle.
    * Removed FrameFlatStyle property.
===============================================================================}

{$I RzComps.inc}
{$WARN SYMBOL_DEPRECATED OFF}
                   
unit RzFilSys;

interface

uses
  {$IFDEF USE_CS}
  CodeSiteLogging,
  {$ENDIF}
  Classes,
  Controls,
  Messages,
  Windows,
  StdCtrls,
  FileCtrl,
  ShellApi,
  Graphics,
  RzTreeVw,
  ComCtrls,
  CommCtrl,
  RzCommon;

type
  TDriveTypes = set of TDriveType;
  TDriveBits = set of 0..25;

  TRzFileInfo = class
    Name: string;
    Attr: Integer;
    Time: Longint;
    Size: Longint;
    IsDirectory: Boolean;
    IconHandle: THandle;
    IconGlyph: TBitmap;
  end;

  TRzDirectoryTree = class;                    { Forward class declaration }

  {======================================}
  {== TRzFileListBox Class Declaration ==}
  {======================================}

  TRzFileListBox = class( TFileListBox )
  private
    FAboutInfo: TRzAboutInfo;
    FDirTree: TRzDirectoryTree;
    FFileInfoList: TStringList;
    FShowLongNames: Boolean;
    FAllowOpen: Boolean;
    FUpdatingColor: Boolean;
    FDisabledColor: TColor;
    FFocusColor: TColor;
    FNormalColor: TColor;
    FFrameColor: TColor;
    FFrameController: TRzFrameController;
    FFrameHotColor: TColor;
    FFrameHotTrack: Boolean;
    FFrameHotStyle: TFrameStyle;
    FFrameSides: TSides;
    FFrameStyle: TFrameStyle;
    FFrameVisible: Boolean;
    FFramingPreference: TFramingPreference;
    FInReadFileNames: Boolean;

    procedure ResetItemHeight;

    procedure ReadOldFrameFlatProp( Reader: TReader );
    procedure ReadOldFrameFocusStyleProp( Reader: TReader );

    { Message Handling Methods }
    procedure CMColorChanged( var Msg: TMessage ); message cm_ColorChanged;
    procedure CMFontChanged( var Msg: TMessage ); message cm_FontChanged;
    procedure CNDrawItem( var Msg: TWMDrawItem ); message cn_DrawItem;
    procedure WMWindowPosChanging( var Msg: TWMWindowPosChanging ); message wm_WindowPosChanging;
    procedure WMNCPaint( var Msg: TWMNCPaint ); message wm_NCPaint;
    procedure CMParentColorChanged( var Msg: TMessage ); message cm_ParentColorChanged;
    procedure CMEnter( var Msg: TCMEnter ); message cm_Enter;
    procedure CMExit( var Msg: TCMExit ); message cm_Exit;
    procedure CMMouseEnter( var Msg: TMessage ); message cm_MouseEnter;
    procedure CMMouseLeave( var Msg: TMessage ); message cm_MouseLeave;
    procedure WMSize( var Msg: TWMSize ); message wm_Size;
    procedure CMEnabledChanged( var Msg: TMessage ); message cm_EnabledChanged;
  protected
    FCanvas: TCanvas;
    FOverControl: Boolean;

    procedure DefineProperties( Filer: TFiler ); override;
    procedure Loaded; override;
    procedure Notification( AComponent: TComponent; Operation: TOperation ); override;

    procedure UpdateColors; virtual;
    procedure UpdateFrame( ViaMouse, InFocus: Boolean ); virtual;
    procedure RepaintFrame; virtual;

    procedure ClearFileInfoList; virtual;

    { Event Dispatch Methods }
    procedure DblClick; override;
    procedure ReadBitmaps; override;
    procedure ReadFileNames; override;

    procedure LocalSetDirectory( const NewDirectory: string );
    function LocalGetFileName: string;
    procedure LocalSetFileName( const NewFile: string );

    function Compare( A, B: TRzFileInfo ): Integer; virtual;
    procedure QuickSort( L, R: Integer ); virtual;
    procedure DrawItem( Index: Integer; Rect: TRect;
                        State: TOwnerDrawState ); override;

    function DoMouseWheelDown( Shift: TShiftState; MousePos: TPoint ): Boolean; override;
    function DoMouseWheelUp( Shift: TShiftState; MousePos: TPoint ): Boolean; override;

    { Property Access Methods }
    function StoreColor: Boolean;
    function StoreFocusColor: Boolean;
    function NotUsingController: Boolean;
    procedure SetDisabledColor( Value: TColor ); virtual;
    procedure SetFocusColor( Value: TColor ); virtual;
    procedure SetFrameColor( Value: TColor ); virtual;
    procedure SetFrameController( Value: TRzFrameController ); virtual;
    procedure SetFrameHotColor( Value: TColor ); virtual;
    procedure SetFrameHotTrack( Value: Boolean ); virtual;
    procedure SetFrameHotStyle( Value: TFrameStyle ); virtual;
    procedure SetFrameSides( Value: TSides ); virtual;
    procedure SetFrameStyle( Value: TFrameStyle ); virtual;
    procedure SetFrameVisible( Value: Boolean ); virtual;
    procedure SetFramingPreference( Value: TFramingPreference ); virtual;

    function GetShowGlyphs: Boolean; virtual;
    procedure SetShowGlyphs( Value: Boolean ); virtual;
    procedure SetShowLongNames( Value: Boolean ); virtual;
    function GetLongFileName: string; virtual;
    function GetShortFileName: string; virtual;
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

    function UseThemes: Boolean; virtual;
    procedure UpOneLevel;
    procedure ApplyFilePath( const Value: string ); override;

    property LongFileName: string
      read GetLongFileName;

    property ShortFileName: string
      read GetShortFileName;
  published
    property About: TRzAboutInfo
      read FAboutInfo
      write FAboutInfo
      stored False;

    property AllowOpen: Boolean
      read FAllowOpen
      write FAllowOpen
      default False;

    property Columns;

    property Color
      stored StoreColor
      default clWindow;

    property DisabledColor: TColor
      read FDisabledColor
      write SetDisabledColor
      stored NotUsingController
      default clBtnFace;

    property FocusColor: TColor
      read FFocusColor
      write SetFocusColor
      stored StoreFocusColor
      default clWindow;

    property FrameColor: TColor
      read FFrameColor
      write SetFrameColor
      stored NotUsingController
      default clBtnShadow;

    property FrameController: TRzFrameController
      read FFrameController
      write SetFrameController;

    property FrameHotColor: TColor
      read FFrameHotColor
      write SetFrameHotColor
      stored NotUsingController
      default clBtnShadow;

    property FrameHotStyle: TFrameStyle
      read FFrameHotStyle
      write SetFrameHotStyle
      stored NotUsingController
      default fsFlatBold;

    property FrameHotTrack: Boolean
      read FFrameHotTrack
      write SetFrameHotTrack
      stored NotUsingController
      default False;

    property FrameSides: TSides
      read FFrameSides
      write SetFrameSides
      stored NotUsingController
      default sdAllSides;

    property FrameStyle: TFrameStyle
      read FFrameStyle
      write SetFrameStyle
      stored NotUsingController
      default fsFlat;

    property FrameVisible: Boolean
      read FFrameVisible
      write SetFrameVisible
      stored NotUsingController
      default False;

    property FramingPreference: TFramingPreference
      read FFramingPreference
      write SetFramingPreference
      default fpXPThemes;

    property ShowLongNames: Boolean
      read FShowLongNames
      write SetShowLongNames
      default True;

    property ShowGlyphs: Boolean
      read GetShowGlyphs
      write SetShowGlyphs
      default True;

    property OnMouseWheel;
    property OnMouseWheelUp;
    property OnMouseWheelDown;
  end deprecated;

  {========================================}
  {== TRzDirectoryTree Class Declaration ==}
  {========================================}

  PFolderInfo = ^TFolderInfo;
  TFolderInfo = record
    FullPath: string;
    ProcessedChildren: Boolean;
  end;

  TNetworkVolumeFormat = ( nvfExplorer, nvfUNC, nvfVolumeOnly );

  TRzDirectoryTree = class( TRzCustomTreeView )
  private
    FAboutInfo: TRzAboutInfo;
    FFileList: TRzFileListBox;
    FDirLabel: TLabel;
    FShowHiddenDirs: Boolean;
    FOpenCurrentDir: Boolean;
    FNetworkVolumeFormat: TNetworkVolumeFormat;

    FObjInst: Pointer;
    FOldWndProc: TFarProc;
    FFormHandle: HWnd;
    FSaveDirectory: string;
    FUpdating: Boolean;

    FImages: TImageList;
    FFolderOpenIconIndex: Integer;
    FFolderClosedIconIndex: Integer;

    FActiveDrives: TDriveBits;
    FDriveTypes: TDriveTypes;
    FDriveSerialNums: array[ 'A'..'Z' ] of DWord;

    FOldDrive: Char;
    FOnDriveChange: TNotifyEvent;
    FOnDeletion: TTVExpandedEvent;

    procedure AddFolderInfoToNode( Node: TTreeNode; const NodePath: string; IconIndex: Integer );
    procedure FormWndProc( var Msg: TMessage );
  protected
    procedure CreateWindowHandle( const Params: TCreateParams ); override;
    procedure CreateWnd; override;
    procedure DestroyWindowHandle; override;
    procedure DestroyWnd; override;

    procedure InitImageList; virtual;
    procedure InitView; virtual;
    procedure Loaded; override;
    procedure Notification( AComponent: TComponent; Operation: TOperation ); override;

    procedure UpdateActiveDrives; virtual;
    procedure ClearTree; virtual;
    function CanExpand( Node: TTreeNode ): Boolean; override;
    procedure ResetNode( Node: TTreeNode );virtual;
    procedure ProcessChildren( var Node: TTreeNode );
    function HaveProcessedChildren( Node: TTreeNode ): Boolean;
    procedure AddTempNodeIfHasChildren( var Node: TTreeNode );

    { Event Dispatch Methods }
    procedure Delete( Node: TTreeNode ); override;
    procedure DriveChange; dynamic;
    function CanChange( Node: TTreeNode ): Boolean; override;
    procedure Change( Node: TTreeNode ); override;
    procedure Click; override;
    procedure KeyDown( var Key: Word; Shift: TShiftState ); override;
    procedure EditingHandler( Sender: TObject; Node: TTreeNode;
                              var AllowEdit: Boolean );
    procedure EditedHandler( Sender: TObject; Node: TTreeNode; var S: String );

    { Property Access Methods }
    function GetDirectory: string; virtual;
    procedure SetDirectory( const Value: string ); virtual;
    function GetDrive: Char; virtual;
    function GetDrives: TDriveBits; virtual;
    procedure SetDriveTypes( Value: TDriveTypes ); virtual;
    procedure SetFileList( Value: TRzFileListBox ); virtual;
    procedure SetDirLabel( Value: TLabel ); virtual;
    procedure SetDirLabelCaption; virtual;
    procedure SetNetworkVolumeFormat( Value: TNetworkVolumeFormat ); virtual;
    procedure SetShowHiddenDirs( Value: Boolean ); virtual;

    property Items stored False;
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

    procedure RefreshTree; virtual;
    procedure RefreshDriveTree( DriveChar: Char );

    function NodeHasData( Node: TTreeNode ): Boolean;
    function GetNodeFromPath( Path: string ): TTreeNode;
    function GetPathFromNode( Node: TTreeNode ): string;

    procedure UpOneLevel;
    procedure CreateNewDir( NewDirName: string; PlaceInEditMode: Boolean );

    property Directory: string
      read GetDirectory
      write SetDirectory;

    property Drive: Char
      read GetDrive;
  published
    property About: TRzAboutInfo
      read FAboutInfo
      write FAboutInfo
      stored False;

    property DirLabel: TLabel
      read FDirLabel
      write SetDirLabel;

    property DriveTypes: TDriveTypes
      read FDriveTypes
      write SetDriveTypes
      default [ dtUnknown, dtFloppy, dtFixed, dtNetwork, dtCDROM, dtRAM ];

    property FileList: TRzFileListBox
      read FFileList
      write SetFileList;

    property OpenCurrentDir: Boolean
      read FOpenCurrentDir
      write FOpenCurrentDir
      default False;

    property NetworkVolumeFormat: TNetworkVolumeFormat
      read FNetworkVolumeFormat
      write SetNetworkVolumeFormat
      default nvfExplorer;

    property ShowHiddenDirs: Boolean
      read FShowHiddenDirs
      write SetShowHiddenDirs
      default False;

    property OnDriveChange: TNotifyEvent
      read FOnDriveChange
      write FOnDriveChange;

    property OnDeletion: TTVExpandedEvent
      read FOnDeletion
      write FOnDeletion;

    { Inherited Properties & Events }
    property Align;
    property Anchors;
    property AutoExpand;
    property AutoSelect;
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
    property FrameController;
    property FrameHotColor;
    property FrameHotTrack;
    property FrameHotStyle;
    property FrameSides;
    property FrameStyle;
    property FrameVisible;
    property FramingPreference;
    property Height default 150;
    property HideSelection;
    property HotTrack;
//    property Images;
    property Indent;
//    property Items;
    property ParentBiDiMode;
    property ParentColor default False;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly default True;   // When False, directories can be renamed
    property RightClickSelect;
    property RowSelect;
    property SelectionPen;
    property ShowButtons;
    property ShowHint;
    property ShowLines;
//    property ShowRoot;
//    property SortType;
    property StateImages;
    property TabOrder;
    property TabStop;
    property ToolTips;
    property Visible;
    property Width default 250;

    property OnChange;
    property OnChanging;
    property OnClick;
    property OnCollapsed;
    property OnCollapsing;
    property OnCompare;
    property OnContextPopup;
    property OnDblClick;
//    property OnDeletion;
    property OnDragDrop;
    property OnDragOver;
//    property OnEdited;
//    property OnEditing;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnExpanded;
    property OnExpanding;
//    property OnGetImageIndex;
//    property OnGetSelectedIndex;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnStartDock;
    property OnStartDrag;
  end deprecated;


  {===========================================}
  {== TRzDirectoryListBox Class Declaration ==}
  {===========================================}

  TRzDirectoryListBox = class( TDirectoryListBox )
  private
    FAboutInfo: TRzAboutInfo;

    FUpdatingColor: Boolean;
    FDisabledColor: TColor;
    FFocusColor: TColor;
    FNormalColor: TColor;
    FFrameColor: TColor;
    FFrameController: TRzFrameController;
    FFrameHotColor: TColor;
    FFrameHotTrack: Boolean;
    FFrameHotStyle: TFrameStyle;
    FFrameSides: TSides;
    FFrameStyle: TFrameStyle;
    FFrameVisible: Boolean;
    FFramingPreference: TFramingPreference;

    FShowLongNames: Boolean;

    procedure ReadOldFrameFlatProp( Reader: TReader );
    procedure ReadOldFrameFocusStyleProp( Reader: TReader );

    { Message Handling Methods }
    procedure CMColorChanged( var Msg: TMessage ); message cm_ColorChanged;
    procedure WMNCPaint( var Msg: TWMNCPaint ); message wm_NCPaint;
    procedure CMParentColorChanged( var Msg: TMessage ); message cm_ParentColorChanged;
    procedure CMEnter( var Msg: TCMEnter ); message cm_Enter;
    procedure CMExit( var Msg: TCMExit ); message cm_Exit;
    procedure CMMouseEnter( var Msg: TMessage ); message cm_MouseEnter;
    procedure CMMouseLeave( var Msg: TMessage ); message cm_MouseLeave;
    procedure WMSize( var Msg: TWMSize ); message wm_Size;
    procedure CMEnabledChanged( var Msg: TMessage ); message cm_EnabledChanged;
  protected
    FCanvas: TCanvas;
    FOverControl: Boolean;

    procedure DefineProperties( Filer: TFiler ); override;
    procedure Loaded; override;
    procedure Notification( AComponent: TComponent; Operation: TOperation ); override;

    procedure UpdateColors; virtual;
    procedure UpdateFrame( ViaMouse, InFocus: Boolean ); virtual;
    procedure RepaintFrame; virtual;

    procedure BuildList; override;

    { Event Dispatch Methods }
    procedure Change; override;
    procedure ReadBitmaps; override;
    procedure DrawItem( Index: Integer; Rect: TRect;
                        State: TOwnerDrawState ); override;

    function DirLevel( const PathName: string ): Integer;
    function GetLongDirName: string;
    procedure UpdateDirLabel;

    function DoMouseWheelDown( Shift: TShiftState; MousePos: TPoint ): Boolean; override;
    function DoMouseWheelUp( Shift: TShiftState; MousePos: TPoint ): Boolean; override;

    { Property Access Methods }
    function StoreColor: Boolean;
    function StoreFocusColor: Boolean;
    function NotUsingController: Boolean;
    procedure SetDisabledColor( Value: TColor ); virtual;
    procedure SetFocusColor( Value: TColor ); virtual;
    procedure SetFrameColor( Value: TColor ); virtual;
    procedure SetFrameController( Value: TRzFrameController ); virtual;
    procedure SetFrameHotColor( Value: TColor ); virtual;
    procedure SetFrameHotTrack( Value: Boolean ); virtual;
    procedure SetFrameHotStyle( Value: TFrameStyle ); virtual;
    procedure SetFrameSides( Value: TSides ); virtual;
    procedure SetFrameStyle( Value: TFrameStyle ); virtual;
    procedure SetFrameVisible( Value: Boolean ); virtual;
    procedure SetFramingPreference( Value: TFramingPreference ); virtual;

    procedure SetShowLongNames( Value: Boolean );
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

    function UseThemes: Boolean; virtual;

    property LongDirName: string
      read GetLongDirName;
  published
    property About: TRzAboutInfo
      read FAboutInfo
      write FAboutInfo
      stored False;

    property Color
      stored StoreColor
      default clWindow;

    property DisabledColor: TColor
      read FDisabledColor
      write SetDisabledColor
      stored NotUsingController
      default clBtnFace;

    property FocusColor: TColor
      read FFocusColor
      write SetFocusColor
      stored StoreFocusColor
      default clWindow;

    property FrameColor: TColor
      read FFrameColor
      write SetFrameColor
      stored NotUsingController
      default clBtnShadow;

    property FrameController: TRzFrameController
      read FFrameController
      write SetFrameController;

    property FrameHotColor: TColor
      read FFrameHotColor
      write SetFrameHotColor
      stored NotUsingController
      default clBtnShadow;

    property FrameHotStyle: TFrameStyle
      read FFrameHotStyle
      write SetFrameHotStyle
      stored NotUsingController
      default fsFlatBold;

    property FrameHotTrack: Boolean
      read FFrameHotTrack
      write SetFrameHotTrack
      stored NotUsingController
      default False;

    property FrameSides: TSides
      read FFrameSides
      write SetFrameSides
      stored NotUsingController
      default sdAllSides;

    property FrameStyle: TFrameStyle
      read FFrameStyle
      write SetFrameStyle
      stored NotUsingController
      default fsFlat;

    property FrameVisible: Boolean
      read FFrameVisible
      write SetFrameVisible
      stored NotUsingController
      default False;

    property FramingPreference: TFramingPreference
      read FFramingPreference
      write SetFramingPreference
      default fpXPThemes;

    property ShowLongNames: Boolean
      read FShowLongNames
      write SetShowLongNames
      default True;

    property OnMouseWheel;
    property OnMouseWheelUp;
    property OnMouseWheelDown;
  end deprecated;


  {========================================}
  {== TRzDriveComboBox Class Declaration ==}
  {========================================}

  TRzDriveComboBox = class( TDriveComboBox )
  private
    FAboutInfo: TRzAboutInfo;
    FDriveTypes: TDriveTypes;

    FFlatButtons: Boolean;
    FFlatButtonColor: TColor;
    FUpdatingColor: Boolean;
    FDisabledColor: TColor;
    FFocusColor: TColor;
    FNormalColor: TColor;
    FFrameColor: TColor;
    FFrameController: TRzFrameController;
    FFrameHotColor: TColor;
    FFrameHotTrack: Boolean;
    FFrameHotStyle: TFrameStyle;
    FFrameSides: TSides;
    FFrameStyle: TFrameStyle;
    FFrameVisible: Boolean;
    FFramingPreference: TFramingPreference;

    procedure ReadOldFrameFlatProp( Reader: TReader );
    procedure ReadOldFrameFocusStyleProp( Reader: TReader );

    { Message Handling Methods }
    procedure CMColorChanged( var Msg: TMessage ); message cm_ColorChanged;
    procedure CMFontChanged( var Msg: TMessage ); message cm_FontChanged;
    procedure WMPaint( var Msg: TWMPaint ); message wm_Paint;
    procedure CMEnter( var Msg: TCMEnter ); message cm_Enter;
    procedure CMExit( var Msg: TCMExit ); message cm_Exit;
    procedure CMMouseEnter( var Msg: TMessage ); message cm_MouseEnter;
    procedure CMMouseLeave( var Msg: TMessage ); message cm_MouseLeave;
    procedure WMSize( var Msg: TWMSize ); message wm_Size;
    procedure CMParentColorChanged( var Msg: TMessage ); message cm_ParentColorChanged;
    procedure CMEnabledChanged( var Msg: TMessage ); message cm_EnabledChanged;
  protected
    FCanvas: TCanvas;
    FInControl: Boolean;
    FOverControl: Boolean;
    FIsFocused: Boolean;

    procedure DefineProperties( Filer: TFiler ); override;
    procedure Loaded; override;
    procedure Notification( AComponent: TComponent; Operation: TOperation ); override;

    procedure UpdateColors; virtual;
    procedure UpdateFrame( ViaMouse, InFocus: Boolean ); virtual;

    procedure ReadNewBitmaps;
    procedure BuildList; override;
    procedure ResetItemHeight;

    { Property Access Methods }
    procedure SetFlatButtons( Value: Boolean ); virtual;
    procedure SetFlatButtonColor( Value: TColor ); virtual;
    function StoreColor: Boolean;
    function StoreFocusColor: Boolean;
    function NotUsingController: Boolean;
    procedure SetDisabledColor( Value: TColor ); virtual;
    procedure SetFocusColor( Value: TColor ); virtual;
    procedure SetFrameColor( Value: TColor ); virtual;
    procedure SetFrameController( Value: TRzFrameController ); virtual;
    procedure SetFrameHotColor( Value: TColor ); virtual;
    procedure SetFrameHotTrack( Value: Boolean ); virtual;
    procedure SetFrameHotStyle( Value: TFrameStyle ); virtual;
    procedure SetFrameSides( Value: TSides ); virtual;
    procedure SetFrameStyle( Value: TFrameStyle ); virtual;
    procedure SetFrameVisible( Value: Boolean ); virtual;
    procedure SetFramingPreference( Value: TFramingPreference ); virtual;


    procedure SetDriveTypes( Value: TDriveTypes );
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

    function UseThemes: Boolean; virtual;
  published
    property About: TRzAboutInfo
      read FAboutInfo
      write FAboutInfo
      stored False;

    property DriveTypes: TDriveTypes
      read FDriveTypes
      write SetDriveTypes
      default [ dtFloppy, dtFixed, dtNetwork, dtCDROM, dtRAM ];

    property Color
      stored StoreColor
      default clWindow;

    property FlatButtonColor: TColor
      read FFlatButtonColor
      write SetFlatButtonColor
      stored NotUsingController
      default clBtnFace;

    property FlatButtons: Boolean
      read FFlatButtons
      write SetFlatButtons
      stored NotUsingController
      default False;

    property DisabledColor: TColor
      read FDisabledColor
      write SetDisabledColor
      stored NotUsingController
      default clBtnFace;

    property FocusColor: TColor
      read FFocusColor
      write SetFocusColor
      stored StoreFocusColor
      default clWindow;

    property FrameColor: TColor
      read FFrameColor
      write SetFrameColor
      stored NotUsingController
      default clBtnShadow;

    property FrameController: TRzFrameController
      read FFrameController
      write SetFrameController;

    property FrameHotColor: TColor
      read FFrameHotColor
      write SetFrameHotColor
      stored NotUsingController
      default clBtnShadow;

    property FrameHotStyle: TFrameStyle
      read FFrameHotStyle
      write SetFrameHotStyle
      stored NotUsingController
      default fsFlatBold;

    property FrameHotTrack: Boolean
      read FFrameHotTrack
      write SetFrameHotTrack
      stored NotUsingController
      default False;

    property FrameSides: TSides
      read FFrameSides
      write SetFrameSides
      stored NotUsingController
      default sdAllSides;

    property FrameStyle: TFrameStyle
      read FFrameStyle
      write SetFrameStyle
      stored NotUsingController
      default fsFlat;

    property FrameVisible: Boolean
      read FFrameVisible
      write SetFrameVisible
      stored NotUsingController
      default False;

    property FramingPreference: TFramingPreference
      read FFramingPreference
      write SetFramingPreference
      default fpXPThemes;

    { Inherited Properties & Events }
    property Align;
  end deprecated;


function VolumeID( Drive: Char ): string;
function NetworkVolume( Drive: Char ): string;

function UNCPathToDriveMapping( UNCPath: string ): string;
procedure GetDriveInfo( Drive: Char; var Volume: string; var SerialNum: DWord );
procedure GetVolumeInfo( Drive: Char; VolumeFormat: TNetworkVolumeFormat;
                         var Volume: string; var SerialNum: DWord );
function GetDriveSerialNum( Drive: Char ): DWord;
function GetCurrentRootDir: string;

implementation

{$R RzFilSys.res}                                                        // Link in bitmaps for Directory & Drive Glyphs

uses
  {&RAS}
  Types,
  {$IFDEF VCL170_OR_HIGHER}
  UITypes,
  {$ENDIF}
  Themes,
  Dialogs,
  TypInfo,
  SysUtils,
  Forms;


resourcestring
  sRzFileDoesNotExist = 'The %s file does not exist';
  sRzDirectoryDoesNotExist = 'The %s directory does not exist';

var
  FIconCache: TStringList;


{=====================}
{== Support Methods ==}
{=====================}

function LongFileNameFromShort( const ShortName: string ): string;
var
  FindData: TWin32FindData;
  ShortNameStz: array[ 0..255 ] of Char;
  H: THandle;
begin
  if ( Length( ShortName ) <= 3 ) and
     ( ShortName[ 2 ] = ':' ) then
  begin
    Result := ShortName;
    Exit;
  end;

  FillChar( FindData, SizeOf( FindData ), #0 );
  StrPCopy( ShortNameStz, ShortName );
  H := FindFirstFile( ShortNameStz, FindData );

  if H = Invalid_Handle_Value then
    Result := ShortName
  else
  begin
    Result := StrPas( FindData.cFileName );
    Windows.FindClose( H );
  end;
end;


function LongPathFromShort( const ShortPath: string ): string;
var
  P: string;
  Done: Boolean;
begin
  P := ExpandFileName( ShortPath );
  if Length( P ) <= 3 then
  begin
    Result := P;
    Exit;
  end;

  if P[ Length( P ) ] = '\' then
    Delete( P, Length( P ), 1 );
  Result := '';
  Done := False;
  while ( P <> '' ) and not Done do
  begin
    Result := '\' + LongFileNameFromShort( P ) + Result;
    P := ExtractFilePath( P );
    Delete( P, Length( P ), 1 );
    if Length( P ) <= 3 then
    begin
      Result := P + Result;
      Done := True;
    end;
  end;
end;


{&RT}
{============================}
{== TRzFileListBox Methods ==}
{============================}

constructor TRzFileListBox.Create( AOwner: TComponent );
begin
  inherited;
  {&RCI}
  FFileInfoList := TStringList.Create;
  FAllowOpen := False;
  Sorted := False;
  FShowLongNames := True;
  FShowGlyphs := True;
  ResetItemHeight;

  FCanvas := TControlCanvas.Create;
  TControlCanvas( FCanvas ).Control := Self;

  FDisabledColor := clBtnFace;
  FFocusColor := clWindow;
  FNormalColor := clWindow;
  FFrameColor := clBtnShadow;
  FFrameController := nil;
  FFrameHotColor := clBtnShadow;
  FFrameHotTrack := False;
  FFrameHotStyle := fsFlatBold;
  FFrameSides := sdAllSides;
  FFrameStyle := fsFlat;
  FFrameVisible := False;
  FFramingPreference := fpXPThemes;

  FInReadFileNames := False;
  {&RV}
end;


destructor TRzFileListBox.Destroy;
begin
  ClearFileInfoList;
  FFileInfoList.Free;
  if FFrameController <> nil then
    FFrameController.RemoveControl( Self );
  FCanvas.Free;
  inherited;
end;


procedure TRzFileListBox.DefineProperties( Filer: TFiler );
begin
  inherited;
  // Handle the fact that the FrameFlat and FrameFocusStyle properties were renamed to
  // FrameHotStyle and FrameHotStyle respectively in version 3.
  Filer.DefineProperty( 'FrameFlat', ReadOldFrameFlatProp, nil, False );
  Filer.DefineProperty( 'FrameFocusStyle', ReadOldFrameFocusStyleProp, nil, False );

  // Handle the fact that the FrameFlatStyle was published in version 2.x
  Filer.DefineProperty( 'FrameFlatStyle', TRzOldPropReader.ReadOldEnumProp, nil, False );
end;


procedure TRzFileListBox.ReadOldFrameFlatProp( Reader: TReader );
begin
  FFrameHotTrack := Reader.ReadBoolean;
  if FFrameHotTrack then
  begin
    // If the FrameFlat property is stored, then init the FrameHotStyle property and the FrameStyle property.
    // These may be overridden when the rest of the stream is read in. However, we need to re-init them here
    // because the default values of fsStatus and fsLowered have changed in RC3.
    FFrameStyle := fsStatus;
    FFrameHotStyle := fsLowered;
  end;
end;


procedure TRzFileListBox.ReadOldFrameFocusStyleProp( Reader: TReader );
begin
  FFrameHotStyle := TFrameStyle( GetEnumValue( TypeInfo( TFrameStyle ), Reader.ReadIdent ) );
end;


procedure TRzFileListBox.Loaded;
begin
  inherited;
  UpdateColors;
  UpdateFrame( False, False );
end;


procedure TRzFileListBox.Notification( AComponent: TComponent; Operation: TOperation );
begin
  inherited;

  if ( Operation = opRemove ) and ( AComponent = FFrameController ) then
    FFrameController := nil;

  if ( Operation = opRemove ) and ( AComponent = FDirTree ) then
    FDirTree := nil;

  if Operation = opInsert then
  begin
    if ( AComponent is TRzDirectoryListBox ) and
       not Assigned( TRzDirectoryListBox( AComponent ).FileList ) then
    begin
      TRzDirectoryListBox( AComponent ).FileList := Self;
    end
    else if ( AComponent is TRzDirectoryTree ) and
            not Assigned( TRzDirectoryTree( AComponent ).FileList ) then
    begin
      TRzDirectoryTree( AComponent ).FileList := Self;
    end;
  end;
end;


procedure TRzFileListBox.ClearFileInfoList;
var
  I: Integer;
  FileExt: string;
begin
  for I := 0 to FFileInfoList.Count - 1 do
  begin
    FileExt := AnsiLowerCase( ExtractFileExt( TRzFileInfo( FFileInfoList.Objects[ I ] ).Name ) );
    if ( FileExt = '.exe' ) or
       ( FileExt = '.lnk' ) or
       ( FileExt = '.ico' ) or
       ( FileExt = '.scr' ) then
    begin
      DestroyIcon( TRzFileInfo( FFileInfoList.Objects[ I ] ).IconHandle );
    end;
    FFileInfoList.Objects[ I ].Free;
  end;
  FFileInfoList.Clear;
end;


procedure TRzFileListBox.SetShowLongNames( Value: Boolean );
begin
  if FShowLongNames <> Value then
  begin
    FShowLongNames := Value;
    ReadFileNames;
  end;
end;


procedure TRzFileListBox.ReadBitmaps;
begin
  ExeBmp := TBitmap.Create;
  ExeBmp.Handle := LoadBitmap( HInstance, 'RZFILBMP_EXECUTABLE' );
  DirBmp := TBitmap.Create;
  DirBmp.Handle := LoadBitmap( HInstance, 'RZFILBMP_CLOSEDFOLDER' );
  UnknownBmp := TBitmap.Create;
  UnknownBmp.Handle := LoadBitmap( HInstance, 'RZFILBMP_UNKNOWNFILE' );
end;


procedure TRzFileListBox.DblClick;
var
  Hnd: THandle;
begin
  inherited;

  // Normally, ItemIndex should be a valid item b/c you are double-clickikng
  // on an item in the list.  However, one user was actually deleting the
  // selected file during the double-click, which cause the ItemIndex property
  // to get reset back to -1.

  if ItemIndex = -1 then
    Exit;

  if DirectoryExists( Items[ ItemIndex ] ) then
  begin
    Items.BeginUpdate;
    try
      Directory := Items[ ItemIndex ];
    finally
      Items.EndUpdate;
    end;
  end
  else if FAllowOpen then
  begin
    { Try to open the file using ShellExecute }
    Hnd := ShellExecute( HWnd_Desktop, 'Open', PChar( Items[ ItemIndex ] ), nil,
                         nil, sw_ShowNormal );
    if Hnd <= 32 then
    begin
      MessageDlg( 'Could not open file.', mtInformation, [ mbOK ], 0 );
    end;
  end;
end;


procedure TRzFileListBox.UpOneLevel;
begin
  Items.BeginUpdate;
  try
    Directory := '..';
  finally
    Items.EndUpdate;
  end;
end;


procedure TRzFileListBox.LocalSetDirectory( const NewDirectory: string );
begin
  if AnsiCompareFileName( NewDirectory, FDirectory ) <> 0 then
  begin
    SetCurrentDir( NewDirectory + '\' );
    FDirectory := GetCurrentDir;
    ReadFileNames;
  end;
end;


function TRzFileListBox.LocalGetFileName: string;
var
  Idx: Integer;
begin
  // If multi-select is turned on, then using ItemIndex returns a
  // bogus value if nothing is selected
  Idx  := ItemIndex;
  if ( idx < 0 ) or ( Items.Count = 0 ) or ( Selected[ Idx ] = False ) then
    Result := ''
  else
    Result := Items[ Idx ];
end;

procedure TRzFileListBox.LocalSetFileName( const NewFile: string );
begin
  if AnsiCompareFileName( NewFile, LocalGetFileName ) <> 0 then
  begin
    ItemIndex := SendTextMessage( Handle, LB_FindStringExact, 0, NewFile );
    Change;
  end;
end;


procedure TRzFileListBox.ApplyFilePath( const Value: string );
var
  DirPart: string;
  FilePart: string;
  NewDrive: Char;
begin
  if AnsiCompareFileName( FileName, Value ) = 0 then
    Exit;

  if Length( Value ) = 0 then
    Exit;

  ProcessPath( Value, NewDrive, DirPart, FilePart );
  if FDirList <> nil then
    FDirList.Directory := Value;
  if FDirTree <> nil then
    FDirTree.Directory := NewDrive + ':' + DirPart;

  if ( FDirList = nil ) and ( FDirTree = nil ) then
  begin
    if NewDrive <> #0 then
      LocalSetDirectory( Format( '%s:%s', [ NewDrive, DirPart ] ) )
    else
      LocalSetDirectory( DirPart );
  end;

  if ( Pos( '*', FilePart ) > 0 ) or ( Pos( '?', FilePart ) > 0 ) then
    Mask := FilePart
  else if Length( FilePart ) > 0 then
  begin
    LocalSetFileName( FilePart );
    if FileExists( FilePart ) then
    begin
      if LocalGetFileName = '' then
      begin
        Mask := FilePart;
        LocalSetFileName( FilePart );
      end;
    end
    else
    begin
      raise EInOutError.CreateFmt( sRzFileDoesNotExist, [ Value ] );
    end;
  end;
end; {= TRzFileListBox.ApplyFilePath =}


procedure TRzFileListBox.ReadFileNames;
var
  AttrIndex: TFileAttr;
  I, IconIdx: Integer;
  FileExt: string;
  MaskPtr: PChar;
  Ptr: PChar;
  AttrWord: Word;
  SRec: TSearchRec;
  RetValue: Integer;
  FInfo: TRzFileInfo;
  IconHandle: THandle;
  FileInfo: TSHFileInfo;
  SaveCursor: TCursor;
  CheckForDuplicates: Boolean;
  OkToAdd: Boolean;
const
  Attributes: array[ TFileAttr ] of Word = ( faReadOnly, faHidden, faSysFile, faVolumeID, faDirectory, faArchive, 0 );
begin
  FInReadFileNames := True;
  try
    AttrWord := DDL_READWRITE;
    if HandleAllocated then
    begin
      { Set attribute flags based on values in FileType }
      for AttrIndex := ftReadOnly to ftArchive do
        if AttrIndex in FileType then
          AttrWord := AttrWord or Attributes[ AttrIndex ];

      SetCurrentDir( FDirectory );

      ClearFileInfoList;
      Clear;

      I := 0;
      SaveCursor := Screen.Cursor;
      try
        MaskPtr := PChar( FMask );
        CheckForDuplicates := StrScan( MaskPtr, ';' ) <> nil;

        while MaskPtr <> nil do
        begin
          Ptr := StrScan( MaskPtr, ';' );
          if Ptr <> nil then
            Ptr^ := #0;
          RetValue := FindFirst( MaskPtr, AttrWord, SRec );
          if RetValue = 0 then
          begin
            try
              repeat                   { Exclude normal files if ftNormal not set }
                if (ftNormal in FileType) or ( SRec.Attr and AttrWord <> 0) then
                begin
                  if CheckForDuplicates then
                    OkToAdd := Items.IndexOf( SRec.Name ) = -1
                  else
                    OkToAdd := True;

                  if OkToAdd then
                  begin

                    FInfo := TRzFileInfo.Create;
                    FInfo.Name := SRec.Name;
                    FInfo.Attr := SRec.Attr;
                    FInfo.Time := SRec.Time;
                    FInfo.Size := SRec.Size;

                    if ( SRec.Attr and faDirectory ) <> 0 then
                    begin
                      if ( SRec.Name = '.' ) or ( SRec.Name = '..' ) then
                      begin
                        FInfo.Free;  { Don't need it anymore }
                        Continue;
                      end;

                      if FShowGlyphs then
                      begin
                        SHGetFileInfo( PChar( SRec.Name ), 0, FileInfo, SizeOf( TSHFileInfo ),
                                       shgfi_Icon or shgfi_SysIconIndex or
                                       shgfi_SmallIcon or shgfi_ShellIconSize );
                        IconHandle := FileInfo.HIcon;
                        FInfo.IconHandle := IconHandle;
                      end;
                      FInfo.IsDirectory := True;
                      I := Items.Add( SRec.Name );
                      FFileInfoList.AddObject( SRec.Name, FInfo );
                    end
                    else
                    begin
                      FileExt := AnsiLowerCase( ExtractFileExt( SRec.Name ) );

                      if FShowGlyphs then
                      begin
                        IconIdx := FIconCache.IndexOf( FileExt );
                        if ( FileExt <> '.exe' ) and
                           ( FileExt <> '.lnk' ) and
                           ( FileExt <> '.ico' ) and
                           ( FileExt <> '.scr' ) and
                           ( IconIdx <> -1 ) then
                        begin
                          IconHandle := THandle( FIconCache.Objects[ IconIdx ] )
                        end
                        else
                        begin
                          SHGetFileInfo( PChar( SRec.Name ), 0, FileInfo, SizeOf( TSHFileInfo ),
                                         shgfi_Icon or shgfi_SysIconIndex or
                                         shgfi_SmallIcon or shgfi_ShellIconSize );
                          IconHandle := FileInfo.HIcon;
                          if ( FileExt <> '.exe' ) and
                             ( FileExt <> '.lnk' ) and
                             ( FileExt <> '.ico' ) and
                             ( FileExt <> '.scr' ) then
                          begin
                            FIconCache.AddObject( FileExt, TObject( IconHandle ) );
                          end;
                        end;
                        FInfo.IconHandle := IconHandle;
                      end;
                      FInfo.IsDirectory := False;
                      I := Items.Add( SRec.Name );
                      FFileInfoList.AddObject( SRec.Name, FInfo );
                    end;
                  end;
                end;
                if I = 100 then
                  Screen.Cursor := crHourGlass;
              until FindNext( SRec ) <> 0;
            finally
              FindClose( SRec );
            end;
          end;
          if Ptr <> nil then
          begin
            Ptr^ := ';';
            Inc( Ptr );
          end;
          MaskPtr := Ptr;
        end;

        { Sort List }
        if Items.Count > 1 then
        begin
          Items.BeginUpdate;
          try
            QuickSort( 0, Items.Count - 1 );
          finally
            Items.EndUpdate;
          end;
        end;

      finally
        Screen.Cursor := SaveCursor;
      end;
      Change;
    end;
  finally
    FInReadFileNames := False;
  end;
end; {= TRzFileListBox.ReadFileNames =}


function TRzFileListBox.Compare( A, B: TRzFileInfo ): Integer;
begin
  if A.IsDirectory = B.IsDirectory then
    Result := AnsiCompareText( A.Name, B.Name )
  else if A.IsDirectory then
    Result := -1
  else
    Result := 1;
end;


procedure TRzFileListBox.QuickSort( L, R: Integer );
var
  I, J: Integer;
  P: TRzFileInfo;
begin
  I := L;
  J := R;
  P := TRzFileInfo( FFileInfoList.Objects[ ( L + R ) shr 1 ] );
  repeat
    while Compare( TRzFileInfo( FFileInfoList.Objects[ I ] ), P ) < 0 do
      Inc( I );

    while Compare( TRzFileInfo( FFileInfoList.Objects[ J ] ), P ) > 0 do
      Dec( J );

    if I <= J then
    begin
      Items.Exchange( I, J );
      FFileInfoList.Exchange( I, J );
      Inc( I );
      Dec( J );
    end;
  until I > J;
  if L < J then
    QuickSort( L, J );
  if I < R then
    QuickSort( I, R );
end;


procedure TRzFileListBox.CNDrawItem( var Msg: TWMDrawItem );
begin
  { Indent owner-draw rectangle so focus rect doesn't cover glyph }
  if FShowGlyphs then
  begin
    with Msg.DrawItemStruct^ do
      rcItem.Left := rcItem.Left + 24;
  end;
  inherited;
end;


procedure TRzFileListBox.DrawItem( Index: Integer; Rect: TRect; State: TOwnerDrawState );
var
  Offset, TopOffset: Integer;
  FileName: string;
  FileInfo: TSHFileInfo;
begin
  with Canvas do
  begin
    FillRect( Rect );
    TopOffset := ( Rect.Bottom - Rect.Top - TextHeight( 'Pp' ) ) div 2;

    Offset := 2;
    TextRect( Rect, Rect.Left + Offset, Rect.Top + TopOffset, Items[ Index ] );

    if FShowGlyphs then
    begin
      if odSelected in State then
      begin
        if LastChar( Directory ) = '\' then
          FileName := Directory + Items[ Index ]
        else
          FileName := Directory + '\' + Items[ Index ];

        SHGetFileInfo( PChar( FileName ), 0, FileInfo, sizeof( TSHFileInfo ),
                       shgfi_Icon or shgfi_SysIconIndex or
                       shgfi_SmallIcon or shgfi_ShellIconSize or shgfi_Selected );
        DrawIconEx( Canvas.Handle, Rect.Left - 20, Rect.Top + TopOffset - 1,
                    FileInfo.HIcon, 0, 0, 0, 0, di_Normal );
      end
      else
      begin
        if FFileInfoList.Objects[ Index ] <> nil then
          DrawIconEx( Canvas.Handle, Rect.Left - 20, Rect.Top + TopOffset - 1,
                      TRzFileInfo( FFileInfoList.Objects[ Index ]).IconHandle, 0, 0, 0, 0, di_Normal );
      end;
    end;
  end;
end; {= TRzFileListBox.DrawItem =}


function TRzFileListBox.GetLongFileName: string;
begin
  Result := LongPathFromShort( FileName );
end;


function TRzFileListBox.GetShortFileName: string;
var
  ShortPathStz: array[ 0..255 ] of Char;
  R: Integer;
begin
  R := GetShortPathName( PChar( FileName ), ShortPathStz, 255 );

  if R = 0 then
    Result := FileName
  else
    Result := ShortPathStz;
end;


function TRzFileListBox.GetShowGlyphs: Boolean;
begin
  Result := FShowGlyphs;
end;

procedure TRzFileListBox.SetShowGlyphs( Value: Boolean );
begin
  if FShowGlyphs <> Value then
  begin
    FShowGlyphs := Value;
    ResetItemHeight;
    if FShowGlyphs then
      ReadFileNames;
    Invalidate;
  end;
end;


procedure TRzFileListBox.ResetItemHeight;
var
  H: Integer;
begin
  H := GetMinFontHeight( Font ) - 3;
  if FShowGlyphs then
  begin
    if H < 18 then
      H := 18;
  end;
  ItemHeight := H;
end;


procedure TRzFileListBox.CMFontChanged( var Msg: TMessage );
begin
  inherited;
  ResetItemHeight;
  RecreateWnd;
end;

procedure TRzFileListBox.WMWindowPosChanging( var Msg: TWMWindowPosChanging );
begin
  { There is a bug in the TListBox component that causes a divide
    error if the Columns > 0 and the Width of the list box is set to 2.
    The following code prevents this from happening. }
  if ( Columns > 0 ) and ( Msg.WindowPos.cx < 3 ) then
    Msg.WindowPos.cx := 3;
  inherited;
end;


procedure TRzFileListBox.CMColorChanged( var Msg: TMessage );
begin
  inherited;
  if not FUpdatingColor then
  begin
    if FFocusColor = FNormalColor then
      FFocusColor := Color;
    FNormalColor := Color;
  end;
  if FFrameVisible and not UseThemes then
    RepaintFrame;
end;


function TRzFileListBox.StoreColor: Boolean;
begin
  Result := NotUsingController and Enabled;
end;


function TRzFileListBox.StoreFocusColor: Boolean;
begin
  Result := NotUsingController and ( ColorToRGB( FFocusColor ) <> ColorToRGB( Color ) );
end;


function TRzFileListBox.NotUsingController: Boolean;
begin
  Result := FFrameController = nil;
end;


procedure TRzFileListBox.SetDisabledColor( Value: TColor );
begin
  FDisabledColor := Value;
  if not Enabled then
    UpdateColors;
end;


procedure TRzFileListBox.SetFocusColor( Value: TColor );
begin
  FFocusColor := Value;
  if Focused then
    UpdateColors;
end;


procedure TRzFileListBox.SetFrameColor( Value: TColor );
begin
  if FFrameColor <> Value then
  begin
    FFrameColor := Value;
    RepaintFrame;
  end;
end;


procedure TRzFileListBox.SetFrameController( Value: TRzFrameController );
begin
  if FFrameController <> nil then
    FFrameController.RemoveControl( Self );
  FFrameController := Value;
  if Value <> nil then
  begin
    Value.AddControl( Self );
    Value.FreeNotification( Self );
  end;
end;


procedure TRzFileListBox.SetFrameHotColor( Value: TColor );
begin
  if FFrameHotColor <> Value then
  begin
    FFrameHotColor := Value;
    RepaintFrame;
  end;
end;


procedure TRzFileListBox.SetFrameHotTrack( Value: Boolean );
begin
  if FFrameHotTrack <> Value then
  begin
    FFrameHotTrack := Value;
    if FFrameHotTrack then
    begin
      FrameVisible := True;
      if not ( csLoading in ComponentState ) then
        FFrameSides := sdAllSides;
    end;
    RepaintFrame;
    Invalidate;
  end;
end;


procedure TRzFileListBox.SetFrameHotStyle( Value: TFrameStyle );
begin
  if FFrameHotStyle <> Value then
  begin
    FFrameHotStyle := Value;
    RepaintFrame;
  end;
end;


procedure TRzFileListBox.SetFrameSides( Value: TSides );
begin
  if FFrameSides <> Value then
  begin
    FFrameSides := Value;
    RepaintFrame;
  end;
end;


procedure TRzFileListBox.SetFrameStyle( Value: TFrameStyle );
begin
  if FFrameStyle <> Value then
  begin
    FFrameStyle := Value;
    RepaintFrame;
  end;
end;


procedure TRzFileListBox.SetFrameVisible( Value: Boolean );
begin
  if FFrameVisible <> Value then
  begin
    FFrameVisible := Value;
    if FFrameVisible then
      Ctl3D := True;
    RecreateWnd;              { Must recreate window so Ctl3D border reappears }
  end;
end;


procedure TRzFileListBox.SetFramingPreference( Value: TFramingPreference );
begin
  if FFramingPreference <> Value then
  begin
    FFramingPreference := Value;
    if FFramingPreference = fpCustomFraming then
      RepaintFrame;
  end;
end;


procedure TRzFileListBox.RepaintFrame;
begin
  if FInReadFileNames then
    Exit;
  InvalidateWindowFrame( Handle, Rect( 0, 0, Width, Height ) );
end;


function TRzFileListBox.UseThemes: Boolean;
begin
  Result := ( FFramingPreference = fpXPThemes ) and ActiveStyleServicesEnabled;
end;


procedure TRzFileListBox.WMNCPaint( var Msg: TWMNCPaint );
var
  DC: HDC;
begin
  inherited;                       { Must call inherited so scroll bar show up }

  if FFrameVisible and not UseThemes then
  begin
    DC := GetWindowDC( Handle );
    FCanvas.Handle := DC;
    try
      if FFrameHotTrack and ( Focused or FOverControl ) then
        DrawFrame( FCanvas, Width, Height, FFrameHotStyle, Color, FFrameHotColor, FFrameSides )
      else
        DrawFrame( FCanvas, Width, Height, FFrameStyle, Color, FFrameColor, FFrameSides );
    finally
      FCanvas.Handle := 0;
      ReleaseDC( Handle, DC );
    end;
    Msg.Result := 0;
  end;
end; {= TRzFileListBox.WMNCPaint =}


procedure TRzFileListBox.CMParentColorChanged( var Msg: TMessage );
begin
  inherited;

  if ParentColor then
  begin
    // If ParentColor set to True, must reset FNormalColor and FFocusColor
    if FFocusColor = FNormalColor then
      FFocusColor := Color;
    FNormalColor := Color;
  end;

  if FrameVisible then
    RepaintFrame;
end;


procedure TRzFileListBox.UpdateColors;
begin
  if csLoading in ComponentState then
    Exit;

  FUpdatingColor := True;
  try
    if not Enabled then
      Color := FDisabledColor
    else if Focused then
      Color := FFocusColor
    else
      Color := FNormalColor;
  finally
    FUpdatingColor := False;
  end;
end;


procedure TRzFileListBox.UpdateFrame( ViaMouse, InFocus: Boolean );
begin
  if ViaMouse then
    FOverControl := InFocus;

  if FFrameHotTrack then
    RepaintFrame;

  UpdateColors;
end;


procedure TRzFileListBox.CMEnter( var Msg: TCMEnter );
begin
  UpdateFrame( False, True );
  inherited;
end;

procedure TRzFileListBox.CMExit( var Msg: TCMExit );
begin
  inherited;
  UpdateFrame( False, False );
end;


procedure TRzFileListBox.CMMouseEnter( var Msg: TMessage );
begin
  if csDesigning in ComponentState then
    Exit;

  inherited;

  UpdateFrame( True, True );
end;


procedure TRzFileListBox.CMMouseLeave( var Msg: TMessage );
begin
  inherited;

  UpdateFrame( True, False );
end;


procedure TRzFileListBox.WMSize( var Msg: TWMSize );
begin
  inherited;
  if FFrameVisible and not UseThemes then
    RepaintFrame;
end;


procedure TRzFileListBox.CMEnabledChanged( var Msg: TMessage );
begin
  inherited;
  UpdateColors;
end;


function TRzFileListBox.DoMouseWheelDown( Shift: TShiftState; MousePos: TPoint ): Boolean;
var
  Info: TScrollInfo;
begin
  Info.cbSize := SizeOf( Info );
  Info.fMask := sif_Pos;

  GetScrollInfo( Handle, sb_Vert, Info );

  Info.nPos := Info.nPos + Mouse.WheelScrollLines;
  SendMessage( Handle, wm_VScroll, MakeLParam( sb_ThumbPosition, Info.nPos ), 0 );

  SetScrollInfo( Handle, sb_Vert, Info, True );
  Result := True;
end;


function TRzFileListBox.DoMouseWheelUp( Shift: TShiftState; MousePos: TPoint ): Boolean;
var
  Info: TScrollInfo;
begin
  Info.cbSize := SizeOf( Info );
  Info.fMask := sif_Pos;

  GetScrollInfo( Handle, sb_Vert, Info );

  Info.nPos := Info.nPos - Mouse.WheelScrollLines;
  if Info.nPos >= 0 then
  begin
    SendMessage( Handle, wm_VScroll, MakeLParam( sb_ThumbPosition, Info.nPos ), 0 );
    SetScrollInfo( Handle, sb_Vert, Info, True );
  end;
  Result := True;
end;


{==============================}
{== TRzDirectoryTree Methods ==}
{==============================}

constructor TRzDirectoryTree.Create( AOwner: TComponent );
begin
  inherited;

  // This property was added in Delphi 6.  We do not want to
  // have the nodes restored during a CreateWnd process (eg.
  // reparenting the control in a TRzSplitter) because we
  // simply store the directory currently selected and then
  // restore that ourselves when the control is recreated.
  CreateWndRestores := False;

  ReadOnly := True;           { Don't allow renaming of directories by default }

  Width := 250;
  Height := 150;

  FObjInst := nil;
  FOldWndProc := nil;

  FDriveTypes := [ dtUnknown, dtFloppy, dtFixed, dtNetwork, dtCDROM, dtRAM ];

  HideSelection := False;
  FSaveDirectory := '';
  FUpdating := False;

  OnEditing := EditingHandler;
  OnEdited := EditedHandler;

  FImages := TImageList.Create( Self );
  Images := FImages;
  FShowHiddenDirs := False;
  FOpenCurrentDir := False;
  FNetworkVolumeFormat := nvfExplorer;
end;


procedure TRzDirectoryTree.CreateWindowHandle( const Params: TCreateParams );
begin
  inherited;

  if not ( csDesigning in ComponentState ) and ( GetParentForm( Self ) <> nil ) then
  begin
    // Hook into Form's window procedure so we can be notified of
    // wm_DeviceChange messages.
    FFormHandle := ValidParentForm( Self ).Handle;
    FObjInst := Classes.MakeObjectInstance( FormWndProc );
    FOldWndProc := TFarProc( SetWindowLongPtr( FFormHandle, gwl_WndProc, LONG_PTR( FObjInst ) ) );
  end;
end;


procedure TRzDirectoryTree.CreateWnd;
begin
  FUpdating := True;
  inherited;
  FUpdating := False;

  {&RCI}
  InitImageList;
  InitView;

  if not ( csDesigning in ComponentState ) then
  begin
    if FSaveDirectory <> '' then
    begin
      if not FRecreating then
        SetDirectory( FSaveDirectory );
      FSaveDirectory := '';
    end
    else
      SetDirectory( GetCurrentRootDir );
  end;
end;


procedure TRzDirectoryTree.InitImageList;
var
  DirInfo: TSHFileInfo;
begin
  FImages.Handle := SHGetFileInfo( '', 0, DirInfo, SizeOf( DirInfo ),
                                   shgfi_SysIconIndex or shgfi_SmallIcon or shgfi_Icon );
  FImages.ShareImages := True;

  FFolderClosedIconIndex := DirInfo.iIcon;

  SHGetFileInfo( '', 0, DirInfo, SizeOf( DirInfo ),
                 shgfi_OpenIcon or shgfi_SysIconIndex or shgfi_SmallIcon or shgfi_Icon );
  FFolderOpenIconIndex := DirInfo.iIcon;
end;



destructor TRzDirectoryTree.Destroy;
begin
  ClearTree;
  FImages.Free;
  inherited;
end;


procedure TRzDirectoryTree.DestroyWindowHandle;
begin
  if FFormHandle <> 0 then
  begin
    { Restore original window procedure for parent form }
    SetWindowLongPtr( FFormHandle, gwl_WndProc, LONG_PTR( FOldWndProc ) );
    Classes.FreeObjectInstance( FObjInst );
    FOldWndProc := nil;
  end;
  inherited;
end;

procedure TRzDirectoryTree.DestroyWnd;
begin
  FSaveDirectory := Directory;
  inherited;
end;


procedure TRzDirectoryTree.Loaded;
begin
  {&RV}
  inherited;
  InitView;

  if not ( csDesigning in ComponentState ) then
  begin
    if FOpenCurrentDir then
      SetDirectory( GetCurrentDir )
    else
      SetDirectory( GetCurrentRootDir );
  end;
end;


procedure TRzDirectoryTree.Notification( AComponent: TComponent; Operation: TOperation );
begin
  inherited;
  if Operation = opRemove then
  begin
    if AComponent = FFileList then
      FFileList := nil
    else if AComponent = FDirLabel then
      FDirLabel := nil;
  end
  else if Operation = opInsert then
  begin
    if ( AComponent is TRzFileListBox ) and not Assigned( FFileList ) then
      FFileList := TRzFileListBox( AComponent );
  end;
end;


function TRzDirectoryTree.NodeHasData( Node: TTreeNode ): Boolean;
begin
  Result := ( Node <> nil ) and ( Node.Data <> nil );
end;



procedure TRzDirectoryTree.AddFolderInfoToNode( Node: TTreeNode; const NodePath: string;
                                                IconIndex: Integer );
var
  FolderInfo: PFolderInfo;
  FileInfo: TSHFileInfo;
begin
  if Node = nil then
    Exit;

  // Record Path and initialize ProcessedChildren flag
  FolderInfo := New( PFolderInfo );
  FolderInfo^.FullPath := NodePath;
  FolderInfo^.ProcessedChildren := False;
  Node.Data := FolderInfo;

  // Specify Icon index to use for Node
  Node.ImageIndex := IconIndex;
  if IconIndex = FFolderClosedIconIndex then
    Node.SelectedIndex := FFolderOpenIconIndex
  else
  begin
    // If the passed in IconIndex does not represent the standard closed folder,
    // then call SHGetFileInfo to get the correct icon index to use when the
    // node is selected.
    SHGetFileInfo( PChar( PFolderInfo( Node.Data )^.FullPath ), 0,
                   FileInfo, SizeOf( TSHFileInfo ),
                   shgfi_SysIconIndex or shgfi_OpenIcon or shgfi_SmallIcon );
    Node.SelectedIndex := FileInfo.iIcon;
  end;
end; {= TRzDirectoryTree.AddFolderInfoToNode =}


function TRzDirectoryTree.GetPathFromNode( Node: TTreeNode ): string;
begin
  if ( Node <> nil ) and ( Node.Data <> nil ) then
    Result := PFolderInfo( Node.Data ).FullPath
  else
    Result := '';
end;


function TRzDirectoryTree.GetNodeFromPath( Path: string ): TTreeNode;
var
  OldCursor: TCursor;
  I: Integer;
  Found: Boolean;
  Node, SearchNode, MatchingNode: TTreeNode;
  FindPath: string;
begin
  Result := nil;

  OldCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    if LastChar( Path ) <> '\' then
      Path := Path + '\';
    Path := AnsiUpperCase( Path );

    Node := nil;

    for I := 1 to CountChar( '\', Path ) do
    begin
      FindPath := CopyEx( Path, 1, '\', I );
      MatchingNode := nil;
      if Items.Count > 0 then
      begin
        if Node <> nil then
          SearchNode := Node.GetFirstChild
        else
          SearchNode := Items[ 0 ];

        Found := False;
        while not Found and NodeHasData( SearchNode ) do
        begin
          if GetPathFromNode( SearchNode ) = FindPath then
          begin
            MatchingNode := SearchNode;
            Found := True;
          end;
          SearchNode := SearchNode.GetNextSibling;
        end;
      end;

      Node := MatchingNode;
      if Node = nil then
        Exit;
    end;
    Result := Node;
  finally
    Screen.Cursor := OldCursor;
  end;
end; {= TRzDirectoryTree.GetNodeFromPath =}


procedure TRzDirectoryTree.ClearTree;
var
  I: Integer;
begin
  if HandleAllocated then
    Items.BeginUpdate;
  try
    for I := 0 to Items.Count - 1 do
    begin
      try
        if NodeHasData( Items[ I ] ) then
        begin
          PFolderInfo( Items[ I ].Data )^.FullPath := '';
          Dispose( PFolderInfo( Items[ I ].Data ) );
        end;
      finally
        Items[ I ].Data := nil;
      end;
    end;
    Items.Clear;
  finally
    if HandleAllocated then
      Items.EndUpdate;
  end;
end;


procedure TRzDirectoryTree.InitView;
var
  OldCursor: TCursor;
  Node: TTreeNode;
  DriveNum: Integer;
  DrivePath: string;
  DriveType: TDriveType;
  Drives: TDriveBits;
  DirInfo: TSHFileInfo;
  V: string;
  SN: DWord;
begin
  if HandleAllocated and not ( csLoading in ComponentState ) then
  begin
    FUpdating := True;
    OldCursor := Screen.Cursor;
    Screen.Cursor := crHourGlass;
    Items.BeginUpdate;
    try
      ClearTree;
      Application.ProcessMessages;
      Drives := GetDrives;
      FActiveDrives := Drives;

      for DriveNum := 0 to 25 do
      begin
        if DriveNum in Drives then
        begin
          DrivePath := Char( DriveNum + Ord( 'A' ) ) + ':\';
          DriveType := TDriveType( GetDriveType( PChar( DrivePath ) ) );

          SHGetFileInfo( PChar( DrivePath ), 0, DirInfo, SizeOf( TSHFileInfo ),
                         shgfi_SysIconIndex or shgfi_SmallIcon or shgfi_DisplayName );

          Node := Items.Add( nil, '' );        // Add new drive to root of tree

          if DriveType <> dtFloppy then
          begin
            GetVolumeInfo( DrivePath[ 1 ], FNetworkVolumeFormat, V, SN );
            Node.Text := V;
            FDriveSerialNums[ UpCase( DrivePath[ 1 ] ) ] := SN;
          end
          else
            Node.Text := DirInfo.szDisplayName;

          AddFolderInfoToNode( Node, DrivePath, DirInfo.iIcon );

          case DriveType of
            dtUnknown, dtNoDrive, dtFloppy, dtCDROM:
              Items.AddChild( Node, ':' );

            else
              AddTempNodeIfHasChildren( Node );
          end;
        end;
        Application.ProcessMessages;
      end;
    finally
      Items.EndUpdate;
      if Selected <> nil then
        Selected.MakeVisible;
      Screen.Cursor := OldCursor;
      FUpdating := False;
    end;
  end;
end; {= TRzDirectoryTree.InitView =}


procedure TRzDirectoryTree.RefreshTree;
var
  SaveDir: string;
begin
  SaveDir := GetDirectory;
  InitView;
  SetDirectory( SaveDir );
end;


procedure TRzDirectoryTree.RefreshDriveTree( DriveChar: Char );
var
  VolumeID, SaveDir: string;
  SN: DWORD;
  Node: TTreeNode;
begin
  if not ( csDestroying in ComponentState ) and not FUpdating then
  begin
    SaveDir := GetDirectory;

    Node := GetNodeFromPath( DriveChar + ':\' );
    if Node <> nil then
    begin
      ResetNode( Node );
      GetVolumeInfo( DriveChar, FNetworkVolumeFormat, VolumeID, SN );
      Node.Text := VolumeID;
      FDriveSerialNums[ DriveChar ] := SN;
      if SN <> DWord( -1 ) then
        Items.AddChild( Node, ':' );
    end;

    SetDirectory( SaveDir );
  end;
end;


function TRzDirectoryTree.GetDrive: Char;
var
  S: string;
begin
  S := GetDirectory;
  if Length( S ) > 0 then
    Result := UpCase( S[ 1 ] )
  else
    Result := #0;
end;


function TRzDirectoryTree.GetDirectory: string;
begin
  Result := GetPathFromNode( Selected );
  if ( Result <> '' ) and ( Length( Result ) > 3 ) and ( LastChar( Result ) = '\' ) then
    Result := Copy( Result, 1, Length( Result ) - 1 );
end;


procedure TRzDirectoryTree.SetDirectory( const Value: string );
var
  Node, TempNode, MatchingNode: TTreeNode;
  I, C: Integer;
  FindPath, NewDir, S: string;
  OldCursor: TCursor;
  FoundMatch: Boolean;
begin
  NewDir := ExpandFileName( Value );
  NewDir := UNCPathToDriveMapping( NewDir );
  if ( Length( NewDir ) > 0 ) and
     ( AnsiCompareFileName( NewDir, Directory ) <> 0 ) then
  begin
    NewDir := ANSIUpperCase( NewDir );

    if not DirectoryExists( NewDir ) then
    begin
      S := NewDir;
      C := CountChar( '\', NewDir );
      FoundMatch := False;
      while ( C > 0 ) and not FoundMatch do
      begin
        S := CopyEx( NewDir, 1, '\', C );
        if DirectoryExists( S ) then
          FoundMatch := True
        else
          Dec( C );
      end; { while }

      if FoundMatch then
        NewDir := S
      else
        raise EInOutError.CreateFmt( sRzDirectoryDoesNotExist, [ Value ] );
    end;

    if LastChar( NewDir ) <> '\' then
      NewDir := NewDir + '\';

    OldCursor := Screen.Cursor;
    Screen.Cursor := crHourGlass;
    Items.BeginUpdate;
    try
      Node := nil;
      for I := 1 to CountChar( '\', NewDir ) do
      begin
        FindPath := CopyEx( NewDir, 1, '\', I );

        if I = 1 then
        begin
          { Traverse Root Nodes }
          Node := nil;
          MatchingNode := Items.GetFirstNode;
          while NodeHasData( MatchingNode ) do
          begin
            if lstrcmpi( PChar( FindPath ), PChar( GetPathFromNode( MatchingNode ) ) ) = 0 then
            begin
              if not HaveProcessedChildren( MatchingNode ) then
                ProcessChildren( MatchingNode );
              Node := MatchingNode;
              Break;
            end;
            MatchingNode := MatchingNode.GetNextSibling;
          end;
        end
        else
        begin
          { Traverse Child Nodes (i.e. subdirectories }
          TempNode := nil;
          MatchingNode := Node.GetFirstChild;
          while NodeHasData( MatchingNode ) do
          begin
            if lstrcmpi( PChar( FindPath ), PChar( GetPathFromNode( MatchingNode ) ) ) = 0 then
            begin
              if not HaveProcessedChildren( MatchingNode ) then
                ProcessChildren( MatchingNode );
              TempNode := MatchingNode;
              Break;
            end;
            MatchingNode := Node.GetNextChild( MatchingNode );
          end;
          Node := TempNode;
        end;

        if Node = nil then
          Exit;
      end;
      Selected := Node;
      Node.MakeVisible;
    finally
      Items.EndUpdate;
      Screen.Cursor := OldCursor;
    end;
  end;
end; {= TRzDirectoryTree.SetDirectory =}


procedure TRzDirectoryTree.ProcessChildren( var Node: TTreeNode );
var
  RetValue, I, OldErrorMode: Integer;
  SRec: TSearchRec;
  SearchFlags: Integer;
  Path, TempPath: string;
  SubDirList: TStringList;
  DirInfo: TSHFileInfo;
  NewNode: TTreeNode;
begin
  if ( csDesigning in ComponentState ) or ( Node = nil ) or HaveProcessedChildren( Node ) then
    Exit;

  SubDirList := TStringList.Create;
  try
    Path := GetPathFromNode( Node );
    Node.DeleteChildren;

    PFolderInfo( Node.Data )^.ProcessedChildren := True;

    FillChar( SRec, SizeOf( TSearchRec ), 0 );
    OldErrorMode := SetErrorMode( sem_FailCriticalErrors );
    try
      if FShowHiddenDirs then
        SearchFlags := faDirectory or faHidden
      else
        SearchFlags := faDirectory;
      RetValue := FindFirst( Path + '*.*', SearchFlags, SRec );
      while RetValue = 0 do
      begin
        if ( ( SRec.Attr and faDirectory ) > 0 ) and ( SRec.Name <> '.' ) and ( SRec.Name <> '..' ) then
          SubDirList.Add( SRec.Name );
        RetValue := FindNext( SRec );
      end;
      FindClose( SRec );
    finally
      SetErrorMode( OldErrorMode );
    end;

    SubDirList.Sorted := True;
    if SubDirList.Count > 0 then
    begin
      for I := 0 to SubDirList.Count - 1 do
      begin
        // Add all sub-directories to the current node
        if Node <> nil then
        begin
          TempPath := Path + SubDirList[ I ];
          SHGetFileInfo( PChar( TempPath ), 0, DirInfo, SizeOf( TSHFileInfo ),
                         shgfi_SysIconIndex or shgfi_SmallIcon or shgfi_DisplayName );

          NewNode := Node.Owner.AddChild( Node, ExtractFileName( TempPath ) );

          if LastChar( TempPath ) <> '\' then
            TempPath := TempPath + '\';
          AddFolderInfoToNode( NewNode, TempPath, DirInfo.iIcon );
        end;
      end;
    end;

  finally
    SubDirList.Free;
  end;
end; {= TRzDirectoryTree.ProcessChildren =}


{ Use trick described in MSDN about adding a temporary node so that the
  + box shows up next to the parent node }

procedure TRzDirectoryTree.AddTempNodeIfHasChildren( var Node: TTreeNode );
var
  SRec: TSearchRec;
  RetValue: Integer;
  P: string;
  OldErrorMode: Integer;
  SearchFlags: Integer;
begin
  if ( csDesigning in ComponentState ) or ( Node = nil ) or HaveProcessedChildren( Node ) then
    Exit;

  P := GetPathFromNode( Node );
  FillChar( SRec, SizeOf( TSearchRec ), 0 );

  OldErrorMode := SetErrorMode( sem_FailCriticalErrors );
  try
    if FShowHiddenDirs then
      SearchFlags := faDirectory or faHidden
    else
      SearchFlags := faDirectory;
    RetValue := FindFirst( P + '*.*', SearchFlags, SRec );
    while RetValue = 0 do
    begin
      if ( ( SRec.Attr and faDirectory ) > 0 ) and ( SRec.Name <> '.' ) and ( SRec.Name <> '..' ) then
      begin
        Items.AddChild( Node, ':' );
        Break;
      end;
      RetValue := FindNext( SRec );
    end;
    FindClose( SRec );
  finally
    SetErrorMode( OldErrorMode );
  end;
end; {= TRzDirectoryTree.AddTempNodeIfHasChildren =}


function TRzDirectoryTree.CanExpand( Node: TTreeNode ): Boolean;
var
  OldCursor: TCursor;
  ChildNode: TTreeNode;
begin
  OldCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    ProcessChildren( Node );

    if not ( csDesigning in ComponentState ) and ( Node <> nil ) then
    begin
      ChildNode := Node.GetFirstChild;

      while ChildNode <> nil do
      begin
        AddTempNodeIfHasChildren( ChildNode );
        ChildNode := Node.GetNextChild( ChildNode );
      end;
    end;
  finally
    Screen.Cursor := OldCursor;
  end;

  Result := inherited CanExpand( Node );
end;



procedure TRzDirectoryTree.SetNetworkVolumeFormat( Value: TNetworkVolumeFormat );
begin
  if FNetworkVolumeFormat <> Value then
  begin
    FNetworkVolumeFormat := Value;
    RefreshTree;
  end;
end;


procedure TRzDirectoryTree.SetShowHiddenDirs( Value: Boolean );
begin
  if FShowHiddenDirs <> Value then
  begin
    FShowHiddenDirs := Value;
    RefreshTree;
  end;
end;

procedure TRzDirectoryTree.DriveChange;
begin
  if Assigned( FOnDriveChange ) then
    FOnDriveChange( Self );
end;


function TRzDirectoryTree.CanChange( Node: TTreeNode ): Boolean;
begin
  Result := inherited CanChange( Node );
  FOldDrive := Drive;
end;

procedure TRzDirectoryTree.Change( Node: TTreeNode );
var
  VolumeID: string;
  DriveChar: Char;
  NewSN, SN: DWORD;
begin
  {&RV}
  if not ( csDestroying in ComponentState ) and not FUpdating then
  begin
    if NodeHasData( node ) then
    begin
      DriveChar := PFolderInfo( Node.Data )^.FullPath[ 1 ];
      NewSN := GetDriveSerialNum( DriveChar );

      if FDriveSerialNums[ DriveChar ] <> NewSN then
      begin
        Node := GetNodeFromPath( DriveChar + ':\' );
        if Node <> nil then
        begin
          ResetNode( Node );
          GetVolumeInfo( DriveChar, FNetworkVolumeFormat, VolumeID, SN );
          Node.Text := VolumeID;
          FDriveSerialNums[ DriveChar ] := SN;
          if SN <> DWord( -1 ) then
            Items.AddChild( Node, ':' );
        end;
      end;
    end;

    if FOldDrive <> Drive then
      DriveChange;

    if FFileList <> nil then
      FFileList.LocalSetDirectory( Directory );
    SetDirLabelCaption;


    inherited;
  end;
end;


procedure TRzDirectoryTree.Delete( Node: TTreeNode );
begin
  inherited;

  if NodeHasData( Node ) then
  begin
    PFolderInfo( Node.Data )^.FullPath := '';
    Dispose( PFolderInfo( Node.Data ) );
    Node.Data := nil;
  end;
end;


procedure TRzDirectoryTree.Click;
var
  SN: DWord;
  Node: TTreeNode;
  DriveChar: Char;
begin
  {&RV}
  Node := Selected;
  if NodeHasData( Node ) then
  begin
    DriveChar := PFolderInfo( Node.Data )^.FullPath[ 1 ];
    SN := GetDriveSerialNum( DriveChar );
    if FDriveSerialNums[ DriveChar ] <> SN then
      Change( Node )
    else
      inherited;
  end;
end;


procedure TRzDirectoryTree.KeyDown( var Key: Word; Shift: TShiftState );
begin
  inherited;
  if Key = vk_F2 then
    Selected.EditText;
end;


procedure TRzDirectoryTree.EditingHandler( Sender: TObject; Node: TTreeNode;
                                           var AllowEdit: Boolean );
begin
  if Node = nil then
    AllowEdit := False
  else if Node.Level = 0 then
    AllowEdit := False;                            // Cannot change drive names
end;


procedure TRzDirectoryTree.EditedHandler( Sender: TObject; Node: TTreeNode; var S: String );
var
  OldDir, NewDir: string;
begin
  if Node <> nil then
  begin
    OldDir := Directory;
    NewDir := ExpandFileName( OldDir + '\..\' + S );
    if RenameFile( OldDir, NewDir ) then
    begin
      Node.Text := S;
      PFolderInfo( Node.Data ).FullPath := NewDir;
      Change( Node );
    end
    else
    begin
      MessageDlg( 'A directory name cannot contain any of the following characters:'#13'\ / : * ? " < > |',
                  mtError, [ mbOK ], 0 );
      S := Node.Text;
    end;
  end;
end;


procedure TRzDirectoryTree.ResetNode( Node: TTreeNode );
begin
  if Node <> nil then
  begin
    Node.DeleteChildren;
    if NodeHasData( Node ) then
      PFolderInfo( Node.Data )^.ProcessedChildren := False;
    AddTempNodeIfHasChildren( Node );
  end;
end;


function TRzDirectoryTree.HaveProcessedChildren( Node: TTreeNode ): Boolean;
begin
  if NodeHasData( Node ) then
    Result := PFolderInfo( Node.Data )^.ProcessedChildren
  else
    Result := False;
end;


procedure TRzDirectoryTree.FormWndProc( var Msg: TMessage );
begin
  if Msg.Msg = wm_DeviceChange then
  begin
    Msg.Result := 0;
    UpdateActiveDrives;
    Change( Selected );
  end
  else if FOldWndProc <> nil then
    Msg.Result := CallWindowProc( FOldWndProc, FFormHandle, Msg.Msg,
                                  Msg.WParam, Msg.LParam );
end;


procedure TRzDirectoryTree.UpOneLevel;
begin
  if Selected <> nil then
  begin
    if Selected.Parent <> nil then
      Selected := Selected.Parent;
  end;
end;


procedure TRzDirectoryTree.CreateNewDir( NewDirName: string;
                                         PlaceInEditMode: Boolean );
var
  NewPath, TempPath: string;
  NewNode: TTreeNode;
  DirInfo: TSHFileInfo;
  Count: Integer;
  UniqueName: Boolean;
begin
  if Selected <> nil then
  begin
    if NewDirName = '' then
      NewDirName := 'New Folder';

    NewPath := Directory + '\' + NewDirName;
    TempPath := NewPath;
    Count := 0;
    repeat
      UniqueName := not DirectoryExists( TempPath );
      if not UniqueName then
      begin
        Inc( Count );
        TempPath := NewPath + IntToStr( Count );
      end;
    until UniqueName;
    NewPath := TempPath;

    if CreateDir( NewPath ) then
    begin
      SHGetFileInfo( PChar( NewPath ), 0, DirInfo, SizeOf( TSHFileInfo ),
                     shgfi_SysIconIndex or shgfi_SmallIcon or shgfi_DisplayName );

      NewNode := Selected.Owner.AddChild( Selected, ExtractFileName( NewPath ) );

      if LastChar( NewPath ) <> '\' then
        NewPath := NewPath + '\';
      AddFolderInfoToNode( NewNode, NewPath, DirInfo.iIcon );

      NewNode.Selected := True;
      // Put into Edit Mode;
      if PlaceInEditMode then
      begin
        SetFocus;
        Selected.EditText;
      end;
    end;
  end;
end;

procedure TRzDirectoryTree.UpdateActiveDrives;
var
  AvailableDrives: TDriveBits;
  DrivePath, V: string;
  DriveNum: Integer;
  SN: DWord;
  DriveType: TDriveType;
  Node, InsertionPoint: TTreeNode;
  DirInfo: TSHFileInfo;


  function FindInsertionPoint( const SearchPath: string ): TTreeNode;
  var
    Path: string;
    Node: TTreeNode;
  begin
    Result := nil;
    if Items.Count > 0 then
    begin
      Node := Items[ 0 ];
      while NodeHasData( Node ) and ( Result = nil ) do
      begin
        Path := GetPathFromNode( Node );
        if Path > SearchPath then
          Result := Node;
        Node := Node.GetNextSibling;
      end;
    end;
  end;

begin {= TRzDirectoryTree.UpdateActiveDrives =}
  FUpdating := True;
  Items.BeginUpdate;
  try
    AvailableDrives := GetDrives;

    for DriveNum := 0 to 25 do
    begin
      if ( DriveNum in FActiveDrives ) and not ( DriveNum in AvailableDrives ) then
      begin
        { The drive represented by DriveNum has been removed }
        Node := GetNodeFromPath( Char( DriveNum + Ord( 'A' ) ) + ':\' );
        if Node <> nil then
        begin
          ResetNode( Node );
          Items.Delete( Node );
        end;
      end;
    end;

    for DriveNum := 0 to 25 do
    begin
      if ( DriveNum in AvailableDrives ) and not ( DriveNum in FActiveDrives ) then
      begin
        { The drive represented by DriveNum has been added }
        DrivePath := Char( DriveNum + Ord( 'A' ) ) + ':\';
        DriveType := TDriveType( GetDriveType( PChar( DrivePath ) ) );

        SHGetFileInfo( PChar( DrivePath ), 0, DirInfo, SizeOf( TSHFileInfo ),
                       shgfi_SysIconIndex or shgfi_SmallIcon or shgfi_DisplayName );

        InsertionPoint := FindInsertionPoint( DrivePath );
        if InsertionPoint <> nil then
          Node := Items.Insert( InsertionPoint, '' )
        else
          Node := Items.Add( nil, '' );        // Add new drive to root of tree

        if DriveType <> dtFloppy then
        begin
          GetVolumeInfo( DrivePath[ 1 ], FNetworkVolumeFormat, V, SN );
          Node.Text := V;
          FDriveSerialNums[ UpCase( DrivePath[ 1 ] ) ] := SN;
        end
        else
          Node.Text := DirInfo.szDisplayName;

        AddFolderInfoToNode( Node, DrivePath, DirInfo.iIcon );

        case DriveType of
          dtUnknown, dtNoDrive, dtFloppy, dtCDROM:
            Items.AddChild( Node, ':' );

          else
            AddTempNodeIfHasChildren( Node );
        end;
      end;
    end;
    FActiveDrives := AvailableDrives;
  finally
    FUpdating := False;
    Items.EndUpdate;
    Update;
  end;
end; {= TRzDirectoryTree.UpdateActiveDrives =}



function TRzDirectoryTree.GetDrives: TDriveBits;
var
  OldErrorMode, DriveNum: Integer;
  DriveChar: Char;
begin
  OldErrorMode := SetErrorMode( sem_FailCriticalErrors );
  try
    Integer( Result ) := GetLogicalDrives;

    for DriveNum := 0 to 25 do
    begin
      if DriveNum in Result then
      begin
        DriveChar := Char( DriveNum + Ord( 'A' ) );

        case TDriveType( GetDriveType( PChar( DriveChar + ':\' ) ) ) of
          dtUnknown:
            if not ( dtUnknown in FDriveTypes ) then
              Exclude( Result, DriveNum );

          dtFloppy:
            if not ( dtFloppy in FDriveTypes ) then
              Exclude( Result, DriveNum );

          dtFixed:
            if not ( dtFixed in FDriveTypes ) then
              Exclude( Result, DriveNum );

          dtNetwork:
            if not ( dtNetwork in FDriveTypes ) then
              Exclude( Result, DriveNum );

          dtCDROM:
            if not ( dtCDROM in FDriveTypes ) then
              Exclude( Result, DriveNum );

          dtRAM:
            if not ( dtRAM in FDriveTypes ) then
              Exclude( Result, DriveNum );
        end;
      end;
    end;
  finally
    SetErrorMode( OldErrorMode );
  end;
end; {= TRzDirectoryTree.GetDrives =}


procedure TRzDirectoryTree.SetDriveTypes( Value: TDriveTypes );
begin
  if FDriveTypes <> Value then
  begin
    FDriveTypes := Value;
    UpdateActiveDrives;
  end;
end;

procedure TRzDirectoryTree.SetFileList( Value: TRzFileListBox );
begin
  if FFileList <> nil then
    FFileList.FDirTree := nil;
  FFileList := Value;
  if FFileList <> nil then
  begin
    FFileList.FDirTree := Self;
    FFileList.FreeNotification( Self );
  end;
end;

procedure TRzDirectoryTree.SetDirLabel( Value: TLabel );
begin
  FDirLabel := Value;
  if Value <> nil then
    Value.FreeNotification( Self );
  SetDirLabelCaption;
end;

procedure TRzDirectoryTree.SetDirLabelCaption;
var
  DirWidth: Integer;
begin
  if FDirLabel <> nil then
  begin
    DirWidth := Width;
    if not FDirLabel.AutoSize then
      DirWidth := FDirLabel.Width;
    FDirLabel.Caption := MinimizeName( Directory, FDirLabel.Canvas, DirWidth );
  end;
end;


{=================================}
{== TRzDirectoryListBox Methods ==}
{=================================}


constructor TRzDirectoryListBox.Create( AOwner: TComponent );
begin
  inherited;
  FShowLongNames := True;

  FCanvas := TControlCanvas.Create;
  TControlCanvas( FCanvas ).Control := Self;

  FDisabledColor := clBtnFace;
  FFocusColor := clWindow;
  FNormalColor := clWindow;
  FFrameColor := clBtnShadow;
  FFrameController := nil;
  FFrameHotColor := clBtnShadow;
  FFrameHotTrack := False;
  FFrameHotStyle := fsFlatBold;
  FFrameSides := sdAllSides;
  FFrameStyle := fsFlat;
  FFrameVisible := False;
  FFramingPreference := fpXPThemes;
  {&RCI}
end;


destructor TRzDirectoryListBox.Destroy;
begin
  if FFrameController <> nil then
    FFrameController.RemoveControl( Self );
  FCanvas.Free;
  inherited;
end;


procedure TRzDirectoryListBox.DefineProperties( Filer: TFiler );
begin
  inherited;
  // Handle the fact that the FrameFlat and FrameFocusStyle properties were renamed to
  // FrameHotStyle and FrameHotStyle respectively in version 3.
  Filer.DefineProperty( 'FrameFlat', ReadOldFrameFlatProp, nil, False );
  Filer.DefineProperty( 'FrameFocusStyle', ReadOldFrameFocusStyleProp, nil, False );

  // Handle the fact that the FrameFlatStyle was published in version 2.x
  Filer.DefineProperty( 'FrameFlatStyle', TRzOldPropReader.ReadOldEnumProp, nil, False );
end;


procedure TRzDirectoryListBox.ReadOldFrameFlatProp( Reader: TReader );
begin
  FFrameHotTrack := Reader.ReadBoolean;
  if FFrameHotTrack then
  begin
    // If the FrameFlat property is stored, then init the FrameHotStyle property and the FrameStyle property.
    // These may be overridden when the rest of the stream is read in. However, we need to re-init them here
    // because the default values of fsStatus and fsLowered have changed in RC3.
    FFrameStyle := fsStatus;
    FFrameHotStyle := fsLowered;
  end;
end;


procedure TRzDirectoryListBox.ReadOldFrameFocusStyleProp( Reader: TReader );
begin
  FFrameHotStyle := TFrameStyle( GetEnumValue( TypeInfo( TFrameStyle ), Reader.ReadIdent ) );
end;


procedure TRzDirectoryListBox.Loaded;
begin
  inherited;
  UpdateColors;
  UpdateFrame( False, False );
end;


procedure TRzDirectoryListBox.BuildList;
var
  D: string;
begin
  {&RV}
  D := Directory;
  while not DirectoryExists( D ) do
    D := ExtractFilePath( D );
  Directory := D;

  inherited;
end;


procedure TRzDirectoryListBox.Notification( AComponent: TComponent; Operation: TOperation );
begin
  inherited;

  if ( Operation = opRemove ) and ( AComponent = FFrameController ) then
    FFrameController := nil;

  if Operation = opInsert then
  begin
    if ( AComponent is TRzFileListBox ) and ( FileList <> nil ) then
      FileList := TRzFileListBox( AComponent )
    else if ( AComponent is TRzDriveComboBox ) and
       ( TRzDriveComboBox( AComponent ).DirList <> nil ) then
    begin
      TRzDriveComboBox( AComponent ).DirList := Self;
    end;
  end;
end;


procedure TRzDirectoryListBox.SetShowLongNames( Value: Boolean );
begin
  if FShowLongNames <> Value then
  begin
    FShowLongNames := Value;
    BuildList;
    UpdateDirLabel;
  end;
end;


procedure TRzDirectoryListBox.ReadBitmaps;
begin
  OpenedBmp := TBitmap.Create;
  OpenedBmp.LoadFromResourceName( HInstance, 'RZFILBMP_OPENFOLDER' );
  ClosedBmp := TBitmap.Create;
  ClosedBmp.LoadFromResourceName( HInstance, 'RZFILBMP_CLOSEDFOLDER' );
  CurrentBmp := TBitmap.Create;
  CurrentBmp.LoadFromResourceName( HInstance, 'RZFILBMP_CURRENTFOLDER' );
end;


function TRzDirectoryListBox.DirLevel( const PathName: string ): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 1 to Length( PathName ) do
  begin
    if PathName[ I ] = '\' then
      Inc( Result );
  end;
end;


procedure TRzDirectoryListBox.DrawItem( Index: Integer; Rect: TRect; State: TOwnerDrawState );
var
  Bitmap: TBitmap;
  BmpWidth: Integer;
  DirOffset: Integer;
  TopOffset: Integer;
  TransparentColor: TColor;
begin
  with Canvas do
  begin
    FillRect( Rect );
    BmpWidth  := 16;
    DirOffset := Index * 4 + 2;                            { Add 2 for spacing }

    Bitmap := TBitmap( Items.Objects[ Index ] );
    if ( Bitmap <> nil ) and ( Bitmap = ClosedBmp ) then
      DirOffset := ( DirLevel( Directory ) + 1 ) * 4 + 2;

    TopOffset := ( Rect.Bottom - Rect.Top - TextHeight( 'Pp' ) ) div 2;
    if FShowLongNames then
      TextRect( Rect, Rect.Left + BmpWidth + DirOffset + 4, Rect.Top + TopOffset, LongFileNameFromShort( GetItemPath( Index ) ) )
    else
      TextRect( Rect, Rect.Left + BmpWidth + DirOffset + 4, Rect.Top + TopOffset, Items[ Index ] );

    if Bitmap <> nil then
    begin
      TransparentColor := Bitmap.Canvas.Pixels[ 0, Bitmap.Height - 1 ];
      BrushCopy( Bounds( Rect.Left + DirOffset,
                         ( Rect.Top + Rect.Bottom - Bitmap.Height ) div 2,
                         Bitmap.Width, Bitmap.Height ),
                 Bitmap, Bounds( 0, 0, Bitmap.Width, Bitmap.Height ),
                 TransparentColor );
    end;
  end;
end;


function TRzDirectoryListBox.GetLongDirName: string;
begin
  Result := LongPathFromShort( Directory );
end;


procedure TRzDirectoryListBox.Change;
begin
  inherited;
  UpdateDirLabel;
end;


procedure TRzDirectoryListBox.UpdateDirLabel;
var
  W: Integer;
begin
  if DirLabel <> nil then
  begin
    W := Width;
    if not DirLabel.AutoSize then
      W := DirLabel.Width;
    if FShowLongNames then
      DirLabel.Caption := MinimizeName( LongDirName, DirLabel.Canvas, W )
    else
      DirLabel.Caption := MinimizeName( Directory, DirLabel.Canvas, W );
  end;
end;


procedure TRzDirectoryListBox.CMColorChanged( var Msg: TMessage );
begin
  inherited;
  if not FUpdatingColor then
  begin
    if FFocusColor = FNormalColor then
      FFocusColor := Color;
    FNormalColor := Color;
  end;
  if FFrameVisible and not UseThemes then
    RepaintFrame;
end;


function TRzDirectoryListBox.StoreColor: Boolean;
begin
  Result := NotUsingController and Enabled;
end;


function TRzDirectoryListBox.StoreFocusColor: Boolean;
begin
  Result := NotUsingController and ( ColorToRGB( FFocusColor ) <> ColorToRGB( Color ) );
end;


function TRzDirectoryListBox.NotUsingController: Boolean;
begin
  Result := FFrameController = nil;
end;


procedure TRzDirectoryListBox.SetDisabledColor( Value: TColor );
begin
  FDisabledColor := Value;
  if not Enabled then
    UpdateColors;
end;


procedure TRzDirectoryListBox.SetFocusColor( Value: TColor );
begin
  FFocusColor := Value;
  if Focused then
    UpdateColors;
end;


procedure TRzDirectoryListBox.SetFrameColor( Value: TColor );
begin
  if FFrameColor <> Value then
  begin
    FFrameColor := Value;
    RepaintFrame;
  end;
end;


procedure TRzDirectoryListBox.SetFrameController( Value: TRzFrameController );
begin
  if FFrameController <> nil then
    FFrameController.RemoveControl( Self );
  FFrameController := Value;
  if Value <> nil then
  begin
    Value.AddControl( Self );
    Value.FreeNotification( Self );
  end;
end;


procedure TRzDirectoryListBox.SetFrameHotColor( Value: TColor );
begin
  if FFrameHotColor <> Value then
  begin
    FFrameHotColor := Value;
    RepaintFrame;
  end;
end;


procedure TRzDirectoryListBox.SetFrameHotTrack( Value: Boolean );
begin
  if FFrameHotTrack <> Value then
  begin
    FFrameHotTrack := Value;
    if FFrameHotTrack then
    begin
      FrameVisible := True;
      if not ( csLoading in ComponentState ) then
        FFrameSides := sdAllSides;
    end;
    RepaintFrame;
    Invalidate;
  end;
end;


procedure TRzDirectoryListBox.SetFrameHotStyle( Value: TFrameStyle );
begin
  if FFrameHotStyle <> Value then
  begin
    FFrameHotStyle := Value;
    RepaintFrame;
  end;
end;


procedure TRzDirectoryListBox.SetFrameSides( Value: TSides );
begin
  if FFrameSides <> Value then
  begin
    FFrameSides := Value;
    RepaintFrame;
  end;
end;


procedure TRzDirectoryListBox.SetFrameStyle( Value: TFrameStyle );
begin
  if FFrameStyle <> Value then
  begin
    FFrameStyle := Value;
    RepaintFrame;
  end;
end;


procedure TRzDirectoryListBox.SetFrameVisible( Value: Boolean );
begin
  if FFrameVisible <> Value then
  begin
    FFrameVisible := Value;
    if FFrameVisible then
      Ctl3D := True;
    RecreateWnd;              { Must recreate window so Ctl3D border reappears }
  end;
end;


procedure TRzDirectoryListBox.SetFramingPreference( Value: TFramingPreference );
begin
  if FFramingPreference <> Value then
  begin
    FFramingPreference := Value;
    if FFramingPreference = fpCustomFraming then
      RepaintFrame;
  end;
end;


procedure TRzDirectoryListBox.RepaintFrame;
begin
  InvalidateWindowFrame( Handle, Rect( 0, 0, Width, Height ) );
end;


function TRzDirectoryListBox.UseThemes: Boolean;
begin
  Result := ( FFramingPreference = fpXPThemes ) and ActiveStyleServicesEnabled;
end;


procedure TRzDirectoryListBox.WMNCPaint( var Msg: TWMNCPaint );
var
  DC: HDC;
begin
  inherited;                       { Must call inherited so scroll bar show up }

  if FFrameVisible and not UseThemes then
  begin
    DC := GetWindowDC( Handle );
    FCanvas.Handle := DC;
    try
      if FFrameHotTrack and ( Focused or FOverControl ) then
        DrawFrame( FCanvas, Width, Height, FFrameHotStyle, Color, FFrameHotColor, FFrameSides )
      else
        DrawFrame( FCanvas, Width, Height, FFrameStyle, Color, FFrameColor, FFrameSides );
    finally
      FCanvas.Handle := 0;
      ReleaseDC( Handle, DC );
    end;
    Msg.Result := 0;
  end;
end; {= TRzDirectoryListBox.WMNCPaint =}


procedure TRzDirectoryListBox.CMParentColorChanged( var Msg: TMessage );
begin
  inherited;

  if ParentColor then
  begin
    // If ParentColor set to True, must reset FNormalColor and FFocusColor
    if FFocusColor = FNormalColor then
      FFocusColor := Color;
    FNormalColor := Color;
  end;

  if FrameVisible then
    RepaintFrame;
end;


procedure TRzDirectoryListBox.UpdateColors;
begin
  if csLoading in ComponentState then
    Exit;

  FUpdatingColor := True;
  try
    if not Enabled then
      Color := FDisabledColor
    else if Focused then
      Color := FFocusColor
    else
      Color := FNormalColor;
  finally
    FUpdatingColor := False;
  end;
end;


procedure TRzDirectoryListBox.UpdateFrame( ViaMouse, InFocus: Boolean );
begin
  if ViaMouse then
    FOverControl := InFocus;

  if FFrameHotTrack then
    RepaintFrame;

  UpdateColors;
end;


procedure TRzDirectoryListBox.CMEnter( var Msg: TCMEnter );
begin
  UpdateFrame( False, True );
  inherited;
end;


procedure TRzDirectoryListBox.CMExit( var Msg: TCMExit );
begin
  inherited;
  UpdateFrame( False, False );
end;


procedure TRzDirectoryListBox.CMMouseEnter( var Msg: TMessage );
begin
  if csDesigning in ComponentState then
    Exit;

  inherited;

  UpdateFrame( True, True );
end;


procedure TRzDirectoryListBox.CMMouseLeave( var Msg: TMessage );
begin
  inherited;

  UpdateFrame( True, False );
end;


procedure TRzDirectoryListBox.WMSize( var Msg: TWMSize );
begin
  inherited;
  if FFrameVisible and not UseThemes then
    RepaintFrame;
end;


procedure TRzDirectoryListBox.CMEnabledChanged( var Msg: TMessage );
begin
  inherited;
  UpdateColors;
end;


function TRzDirectoryListBox.DoMouseWheelDown( Shift: TShiftState; MousePos: TPoint ): Boolean;
var
  Info: TScrollInfo;
begin
  Info.cbSize := SizeOf( Info );
  Info.fMask := sif_Pos;

  GetScrollInfo( Handle, sb_Vert, Info );

  Info.nPos := Info.nPos + Mouse.WheelScrollLines;
  SendMessage( Handle, wm_VScroll, MakeLParam( sb_ThumbPosition, Info.nPos ), 0 );

  SetScrollInfo( Handle, sb_Vert, Info, True );
  Result := True;
end;


function TRzDirectoryListBox.DoMouseWheelUp( Shift: TShiftState; MousePos: TPoint ): Boolean;
var
  Info: TScrollInfo;
begin
  Info.cbSize := SizeOf( Info );
  Info.fMask := sif_Pos;

  GetScrollInfo( Handle, sb_Vert, Info );

  Info.nPos := Info.nPos - Mouse.WheelScrollLines;
  if Info.nPos >= 0 then
  begin
    SendMessage( Handle, wm_VScroll, MakeLParam( sb_ThumbPosition, Info.nPos ), 0 );
    SetScrollInfo( Handle, sb_Vert, Info, True );
  end;
  Result := True;
end;


{==============================}
{== TRzDriveComboBox Methods ==}
{==============================}

constructor TRzDriveComboBox.Create(AOwner: TComponent);
begin
  inherited;
  FDriveTypes := [ dtFloppy, dtFixed, dtNetwork, dtCDROM, dtRAM ];
  ReadNewBitmaps;
  ResetItemHeight;
  {&RCI}
  FCanvas := TControlCanvas.Create;
  TControlCanvas( FCanvas ).Control := Self;

  FFlatButtons := False;
  FFlatButtonColor := clBtnFace;
  FDisabledColor := clBtnFace;
  FFocusColor := clWindow;
  FNormalColor := clWindow;
  FFrameColor := clBtnShadow;
  FFrameController := nil;
  FFrameHotColor := clBtnShadow;
  FFrameHotTrack := False;
  FFrameHotStyle := fsFlatBold;
  FFrameSides := sdAllSides;
  FFrameStyle := fsFlat;
  FFrameVisible := False;
  FFramingPreference := fpXPThemes;
end;


destructor TRzDriveComboBox.Destroy;
begin
  if FFrameController <> nil then
    FFrameController.RemoveControl( Self );
  FCanvas.Free;
  inherited;
end;


procedure TRzDriveComboBox.DefineProperties( Filer: TFiler );
begin
  inherited;
  // Handle the fact that the FrameFlat and FrameFocusStyle properties were renamed to
  // FrameHotStyle and FrameHotStyle respectively in version 3.
  Filer.DefineProperty( 'FrameFlat', ReadOldFrameFlatProp, nil, False );
  Filer.DefineProperty( 'FrameFocusStyle', ReadOldFrameFocusStyleProp, nil, False );

  // Handle the fact that the FrameFlatStyle was published in version 2.x
  Filer.DefineProperty( 'FrameFlatStyle', TRzOldPropReader.ReadOldEnumProp, nil, False );
end;


procedure TRzDriveComboBox.ReadOldFrameFlatProp( Reader: TReader );
begin
  FFrameHotTrack := Reader.ReadBoolean;
  if FFrameHotTrack then
  begin
    // If the FrameFlat property is stored, then init the FrameHotStyle property and the FrameStyle property.
    // These may be overridden when the rest of the stream is read in. However, we need to re-init them here
    // because the default values of fsStatus and fsLowered have changed in RC3.
    FFrameStyle := fsStatus;
    FFrameHotStyle := fsLowered;
  end;
end;


procedure TRzDriveComboBox.ReadOldFrameFocusStyleProp( Reader: TReader );
begin
  FFrameHotStyle := TFrameStyle( GetEnumValue( TypeInfo( TFrameStyle ), Reader.ReadIdent ) );
end;


procedure TRzDriveComboBox.Loaded;
begin
  inherited;
  UpdateColors;
  UpdateFrame( False, False );
end;


procedure TRzDriveComboBox.Notification( AComponent: TComponent; Operation: TOperation );
begin
  inherited;

  if ( Operation = opRemove ) and ( AComponent = FFrameController ) then
    FFrameController := nil;

  if Operation = opInsert then
  begin
    if ( AComponent is TRzDirectoryListBox ) and ( DirList <> nil ) then
      DirList := TRzDirectoryListBox( AComponent );
  end;
end;

procedure TRzDriveComboBox.ReadNewBitmaps;
begin
  FloppyBmp.Handle := LoadBitmap( HInstance, 'RZFILBMP_FLOPPY' );
  FixedBmp.Handle := LoadBitmap( HInstance, 'RZFILBMP_FIXEDDISK' );
  CDROMBmp.Handle := LoadBitmap( HInstance, 'RZFILBMP_CDROM' );
  NetworkBmp.Handle := LoadBitmap( HInstance, 'RZFILBMP_NETDRIVE' );
  RAMBmp.Handle := LoadBitmap( HInstance, 'RZFILBMP_RAM' );
end;


procedure TRzDriveComboBox.ResetItemHeight;
var
  H: Integer;
begin
  H := GetMinFontHeight( Font );
  if H < ( FloppyBmp.Height ) then
    H := FloppyBmp.Height;
  ItemHeight := H;
end;


procedure TRzDriveComboBox.CMFontChanged( var Msg: TMessage );
begin
  inherited;
  ResetItemHeight;
  RecreateWnd;
end;


procedure TRzDriveComboBox.SetDriveTypes( Value: TDriveTypes );
begin
  if FDriveTypes <> Value then
  begin
    FDriveTypes := Value;
    RecreateWnd;
  end;
end;


function VolumeID( Drive: Char ): string;
var
  OldErrorMode: Integer;
  NotUsed, VolFlags: DWORD;
  Buf: array[ 0..MAX_PATH ] of Char;
begin
  OldErrorMode := SetErrorMode( SEM_FAILCRITICALERRORS );
  try
    if GetVolumeInformation( PChar( Drive + ':\' ), Buf,
                             SizeOf( Buf ), nil, NotUsed, VolFlags,
                             nil, 0 ) then
      Result := Buf
    else
      Result := '';

    if Length( Result ) > 0 then
    begin
      Result := AnsiLowerCaseFileName( Result );
      Result[ 1 ] := UpCase( Result[ 1 ] );
    end;
  finally
    SetErrorMode( OldErrorMode );
  end;
end;


function NetworkVolume( Drive: Char ): string;
var
  Buf: array[ 0..260 ] of Char;
  DriveStr: array[ 0..3 ] of Char;
  BufferSize: DWORD;
begin
  BufferSize := SizeOf( Buf );
  DriveStr[ 0 ] := UpCase( Drive );
  DriveStr[ 1 ] := ':';
  DriveStr[ 2 ] := #0;
  if WNetGetConnection( DriveStr, Buf, BufferSize ) = WN_SUCCESS then
  begin
    SetString( Result, Buf, BufferSize );

    if Drive < 'a' then
      Result := AnsiUpperCaseFileName( Result )
    else
      Result := AnsiLowerCaseFileName( Result );
  end
  else
    Result := VolumeID( Drive );
end;



procedure GetDriveInfo( Drive: Char; var Volume: string; var SerialNum: DWord );
begin
  GetVolumeInfo( Drive, nvfVolumeOnly, Volume, SerialNum );
end;

procedure GetVolumeInfo( Drive: Char; VolumeFormat: TNetworkVolumeFormat;
                         var Volume: string; var SerialNum: DWord );
var
  SN: DWord;
  OldErrorMode: Integer;
  NotUsed, VolFlags: DWORD;
  Buf: array[ 0..MAX_PATH ] of Char;
  DriveType: TDriveType;
  UNCPath, ServerName: string;

  function GetUNCPath( Drive: Char ): string;
  var
    I: Integer;
  begin
    Result := ExtractFileDrive( ExpandUNCFileName( Drive + ':\' ) );
    Result := AnsiLowerCaseFileName( Result );
    for I := 3 to Length( Result ) do
    begin
      if Result[ I - 1 ] = '\' then
        Result[ I ] := UpCase( Result[ I ] );
    end;
  end;

  function GetServerName( UNCPath: string ): string;
  begin
    Result := Copy( UNCPath, 3, 255 );
    Result := Copy( Result, 1, Pos( '\', Result ) - 1 );
  end;

begin
  OldErrorMode := SetErrorMode( SEM_FAILCRITICALERRORS );
  try
    if GetVolumeInformation( PChar( Drive + ':\' ), Buf,
                             SizeOf( Buf ), @SN, NotUsed, VolFlags,
                             nil, 0 ) then
    begin
      SetString( Volume, Buf, StrLen( Buf ) );
      if Length( Volume ) > 0 then
      begin
        Volume := AnsiLowerCaseFileName( Volume );
        Volume[ 1 ] := UpCase( Volume[ 1 ] );
      end;


      DriveType := TDriveType( GetDriveType( PChar( Drive + ':\' ) ) );
      if DriveType = dtNetwork then
      begin
        UNCPath := GetUNCPath( Drive );
        case VolumeFormat of
          nvfExplorer:
          begin
            ServerName := GetServerName( UNCPath );
            Volume := Volume + ' on ''' + ServerName + ''' (' + UpCase( Drive ) + ':)';
          end;

          nvfUNC:
          begin
            Volume := UNCPath + ' (' + UpCase( Drive ) + ':)';
          end;

          nvfVolumeOnly:
            Volume := Volume + ' (' + UpCase( Drive ) + ':)';
        end;
      end
      else
        Volume := Volume + ' (' + UpCase( Drive ) + ':)';
      SerialNum := SN;
    end
    else
    begin
      Volume := '(' + UpCase( Drive ) + ':)';
      SerialNum := DWord( -1 );
    end;
  finally
    SetErrorMode( OldErrorMode );
  end;
end;


function GetDriveSerialNum( Drive: Char ): DWord;
var
  V: string;
begin
  GetDriveInfo( Drive, V, Result );
end;


function UNCPathToDriveMapping( UNCPath: string ): string;
var
  EndOfShareName: Integer;
  ServerShareName: string;
  Drive: Char;
  I, Count: Integer;
begin
  { \\ServerName\ShareName\Path }
  if ( UNCPath = '' ) or
     ( ( UNCPath[ 1 ] <> '\' ) and ( UNCPath[ 2 ] <> '\' ) ) then
  begin
    { UNCPath is not in UNC path format }
    Result := UNCPath;
    Exit;
  end;

  EndOfShareName := 2;
  Count := 0;
  for I := 3 to Length( UNCPath ) do
  begin
    if UNCPath[ I ] = '\' then
    begin
      Inc( Count );
      if Count = 2 then
      begin
        EndOfShareName := I;
        Break;
      end;
    end;
  end;

  ServerShareName := UpperCase( Copy( UNCPath, 1, EndOfShareName ) );

  Result := '';
  for Drive := 'B' to 'Z' do
  begin
    if UpperCase( ExpandUNCFileName( Drive + ':\' ) ) = ServerShareName then
    begin
      Result := Drive + ':' + Copy( UNCPath, EndOfShareName, Length( UNCPath ) );
      Break;
    end;
  end;

end; {= UNCPathToDriveMapping =}


function GetCurrentRootDir: string;
begin
  Result := Copy( GetCurrentDir, 1, 3 );
end;


procedure TRzDriveComboBox.BuildList;
var
  DriveNum: Integer;
  DriveChar: Char;
  DriveType: TDriveType;
  DriveBits: set of 0..25;

  procedure AddDrive( const VolName: string; Obj: TObject );
  begin
    Items.AddObject( Format( '%s: %s', [ DriveChar, VolName ] ), Obj );
  end;

begin
  {&RV}
  Clear;
  Integer( DriveBits ) := GetLogicalDrives;
  for DriveNum := 0 to 25 do
  begin
    if DriveNum in DriveBits then
    begin
      DriveChar := Char( DriveNum + Ord( 'a' ) );
      DriveType := TDriveType( GetDriveType( PChar( DriveChar + ':\' ) ) );
      if TextCase = tcUpperCase then
        DriveChar := Upcase( DriveChar );

      if DriveType in FDriveTypes then
      begin
        case DriveType of
          dtFloppy:
            Items.AddObject( DriveChar + ':', FloppyBmp );
          dtFixed:
            AddDrive( VolumeID( DriveChar ), FixedBmp );
          dtNetwork:
            AddDrive( NetworkVolume( DriveChar ), NetworkBmp );
          dtCDROM:
            AddDrive( VolumeID( DriveChar ), CDROMBmp );
          dtRAM:
            AddDrive( VolumeID( DriveChar ), RAMBmp );
        end;
      end;
    end;
  end;
end; {= TRzDriveComboBox.BuildList =}


procedure TRzDriveComboBox.SetFlatButtonColor( Value: TColor );
begin
  if FFlatButtonColor <> Value then
  begin
    FFlatButtonColor := Value;
    Invalidate;
  end;
end;


procedure TRzDriveComboBox.SetFlatButtons( Value: Boolean );
begin
  if FFlatButtons <> Value then
  begin
    FFlatButtons := Value;
    Invalidate;
  end;
end;


procedure TRzDriveComboBox.CMColorChanged( var Msg: TMessage );
begin
  inherited;
  if not FUpdatingColor then
  begin
    if FFocusColor = FNormalColor then
      FFocusColor := Color;
    FNormalColor := Color;
  end;
end;


function TRzDriveComboBox.StoreColor: Boolean;
begin
  Result := NotUsingController and Enabled;
end;


function TRzDriveComboBox.StoreFocusColor: Boolean;
begin
  Result := NotUsingController and ( ColorToRGB( FFocusColor ) <> ColorToRGB( Color ) );
end;


function TRzDriveComboBox.NotUsingController: Boolean;
begin
  Result := FFrameController = nil;
end;


procedure TRzDriveComboBox.SetDisabledColor( Value: TColor );
begin
  FDisabledColor := Value;
  if not Enabled then
    UpdateColors;
end;


procedure TRzDriveComboBox.SetFocusColor( Value: TColor );
begin
  FFocusColor := Value;
  if Focused then
    UpdateColors;
end;


procedure TRzDriveComboBox.SetFrameColor( Value: TColor );
begin
  if FFrameColor <> Value then
  begin
    FFrameColor := Value;
    Invalidate;
  end;
end;


procedure TRzDriveComboBox.SetFrameController( Value: TRzFrameController );
begin
  if FFrameController <> nil then
    FFrameController.RemoveControl( Self );
  FFrameController := Value;
  if Value <> nil then
  begin
    Value.AddControl( Self );
    Value.FreeNotification( Self );
  end;
end;


procedure TRzDriveComboBox.SetFrameHotColor( Value: TColor );
begin
  if FFrameHotColor <> Value then
  begin
    FFrameHotColor := Value;
    Invalidate;
  end;
end;


procedure TRzDriveComboBox.SetFrameHotTrack( Value: Boolean );
begin
  if FFrameHotTrack <> Value then
  begin
    FFrameHotTrack := Value;
    if FFrameHotTrack then
    begin
      FrameVisible := True;
      if not ( csLoading in ComponentState ) then
        FFrameSides := sdAllSides;
    end;
    Invalidate;
  end;
end;


procedure TRzDriveComboBox.SetFrameHotStyle( Value: TFrameStyle );
begin
  if FFrameHotStyle <> Value then
  begin
    FFrameHotStyle := Value;
    Invalidate;
  end;
end;


procedure TRzDriveComboBox.SetFrameSides( Value: TSides );
begin
  if FFrameSides <> Value then
  begin
    FFrameSides := Value;
    Invalidate;
  end;
end;


procedure TRzDriveComboBox.SetFrameStyle( Value: TFrameStyle );
begin
  if FFrameStyle <> Value then
  begin
    FFrameStyle := Value;
    Invalidate;
  end;
end;


procedure TRzDriveComboBox.SetFrameVisible( Value: Boolean );
begin
  if FFrameVisible <> Value then
  begin
    FFrameVisible := Value;
    ParentCtl3D := not FFrameVisible;
    Ctl3D := not FFrameVisible;
    Invalidate;
  end;
end;


procedure TRzDriveComboBox.SetFramingPreference( Value: TFramingPreference );
begin
  if FFramingPreference <> Value then
  begin
    FFramingPreference := Value;
    if FFramingPreference = fpCustomFraming then
      Invalidate;
  end;
end;


function TRzDriveComboBox.UseThemes: Boolean;
begin
  Result := ( FFramingPreference = fpXPThemes ) and ActiveStyleServicesEnabled;
end;


procedure TRzDriveComboBox.WMPaint( var Msg: TWMPaint );
var
  BtnRect, TempRect: TRect;
  X, Y: Integer;
  ElementDetails: TThemedElementDetails;
begin
  inherited;

  if FFrameVisible and not UseThemes then
  begin
    // Erase Ctl3D Border
    DrawBevel( FCanvas, ClientRect, Color, Color, 2, sdAllSides );
    BtnRect := Rect( Width - GetSystemMetrics( sm_CxVScroll ) - 2, 2, Width - 2, Height - 2 );

    if FFlatButtons then
    begin
      if not ( FInControl or FOverControl ) then
      begin
        // Erase Button Border
        FCanvas.Brush.Color := Color;
        FCanvas.FillRect( BtnRect );

        if Enabled then
          FCanvas.Brush.Color := clBlack
        else
          FCanvas.Brush.Color := clBtnShadow;

        FCanvas.Pen.Color := Color;
        X := BtnRect.Left + GetSystemMetrics( sm_CxVScroll ) div 2;
        Y := BtnRect.Top + Height div 2;
        FCanvas.Polygon( [ Point( X, Y ), Point( X - 5, Y - 5 ), Point( X + 5, Y - 5 ) ] );
      end
      else
      begin
        // Erase Button Border
        if ActiveStyleServicesEnabled then
        begin
          if DroppedDown then
            ElementDetails := ActiveStyleServices.GetElementDetails( ttbButtonPressed )
          else
            ElementDetails := ActiveStyleServices.GetElementDetails( ttbButtonHot );

          ActiveStyleServices.DrawElement( FCanvas.Handle, ElementDetails, BtnRect );
        end
        else
        begin
          FCanvas.Brush.Color := FFlatButtonColor;

          if FFlatButtonColor = clBtnFace then
          begin
            if DroppedDown then
              TempRect := DrawBevel( FCanvas, BtnRect, clBtnShadow, clBtnHighlight, 1, sdAllSides )
            else
              TempRect := DrawBevel( FCanvas, BtnRect, clBtnHighlight, clBtnShadow, 1, sdAllSides );
          end
          else
          begin
            if DroppedDown then
              TempRect := DrawColorBorder( FCanvas, BtnRect, FFlatButtonColor, fsStatus )
            else
              TempRect := DrawColorBorder( FCanvas, BtnRect, FFlatButtonColor, fsPopup );
          end;

          FCanvas.FillRect( TempRect );
        end;

        if Enabled then
          FCanvas.Brush.Color := clBlack
        else
          FCanvas.Brush.Color := clBtnShadow;

        FCanvas.Pen.Color := FFlatButtonColor;
        X := BtnRect.Left + GetSystemMetrics( sm_CxVScroll ) div 2;
        Y := BtnRect.Top + Height div 2;
        if DroppedDown then
        begin
          Inc( X );
          Inc( Y );
        end;
        FCanvas.Polygon( [ Point( X, Y ), Point( X - 5, Y - 5 ), Point( X + 5, Y - 5 ) ] );
      end;
    end;


    if FFrameHotTrack and ( FInControl or FOverControl ) then
    begin
      if FFrameHotStyle = fsFlat then
        DrawSides( FCanvas, ClientRect, FFrameHotColor, FFrameHotColor, FFrameSides )
      else if FFrameHotStyle = fsFlatBold then
        DrawBevel( FCanvas, ClientRect, FFrameHotColor, FFrameHotColor, 2, FFrameSides )
      else if Color = clWindow then
        DrawBorderSides( FCanvas, ClientRect, FFrameHotStyle, FFrameSides )
      else
        DrawColorBorderSides( FCanvas, ClientRect, Color, FFrameHotStyle, FFrameSides );
    end
    else
    begin
      if FFrameStyle = fsFlat then
        DrawSides( FCanvas, ClientRect, FFrameColor, FFrameColor, FFrameSides )
      else if FFrameStyle = fsFlatBold then
        DrawBevel( FCanvas, ClientRect, FFrameColor, FFrameColor, 2, FFrameSides )
      else if Color = clWindow then
        DrawBorderSides( FCanvas, ClientRect, FFrameStyle, FFrameSides )
      else
        DrawColorBorderSides( FCanvas, ClientRect, Color, FFrameStyle, FFrameSides );
    end;
  end;
end; {= TRzDriveComboBox.WMPaint =}


procedure TRzDriveComboBox.UpdateColors;
begin
  if csLoading in ComponentState then
    Exit;

  FUpdatingColor := True;
  try
    if not Enabled then
      Color := FDisabledColor
    else if Focused then
      Color := FFocusColor
    else
      Color := FNormalColor;
  finally
    FUpdatingColor := False;
  end;
end;


procedure TRzDriveComboBox.UpdateFrame( ViaMouse, InFocus: Boolean );
var
  PaintIt: Boolean;
  R: TRect;
begin
  if ViaMouse then
    FOverControl := InFocus
  else
    FInControl := InFocus;

  PaintIt := FFlatButtons or FFrameHotTrack;

  if PaintIt and not DroppedDown then
  begin
    R := ClientRect;
    if not FFrameHotTrack then
      R.Left := R.Right - GetSystemMetrics( sm_CxVScroll ) - 2;
    RedrawWindow( Handle, @R, 0, rdw_Invalidate or rdw_NoErase );
  end;

  UpdateColors;
end;


procedure TRzDriveComboBox.CMEnter( var Msg: TCMEnter );
begin
  inherited;
  FIsFocused := True;
  UpdateFrame( False, True );
end;

procedure TRzDriveComboBox.CMExit( var Msg: TCMExit );
begin
  inherited;
  FOverControl := False;
  FIsFocused := False;
  UpdateFrame( False, False );
end;


procedure TRzDriveComboBox.CMMouseEnter( var Msg: TMessage );
begin
  if csDesigning in ComponentState then
    Exit;

  inherited;

  UpdateFrame( True, True );
end;


procedure TRzDriveComboBox.CMMouseLeave( var Msg: TMessage );
begin
  inherited;

  UpdateFrame( True, False );
end;


procedure TRzDriveComboBox.WMSize( var Msg: TWMSize );
begin
  inherited;
  if FFrameVisible and not UseThemes then
    Invalidate;
end;


procedure TRzDriveComboBox.CMParentColorChanged( var Msg: TMessage );
begin
  inherited;

  if ParentColor then
  begin
    // If ParentColor set to True, must reset FNormalColor and FFocusColor
    if FFocusColor = FNormalColor then
      FFocusColor := Color;
    FNormalColor := Color;
  end;
end;


procedure TRzDriveComboBox.CMEnabledChanged( var Msg: TMessage );
begin
  inherited;
  UpdateColors;
end;


procedure FreeIconCache; far;
var
  I: Integer;
begin
  for I := 0 to FIconCache.Count - 1 do
    DestroyIcon( THandle( FIconCache.Objects[ I ] ) );
  FIconCache.Free;
end;


initialization
  FIconCache := TStringList.Create;
  FIconCache.Sorted := True;
  FIconCache.Duplicates := dupIgnore;
  {&RUI}
finalization
  FreeIconCache;
end.
