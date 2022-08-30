{===============================================================================
  RzShellDialogs Unit

  Raize Components - Component Source Unit

  Copyright © 1995-2015 by Raize Software, Inc.  All Rights Reserved.


  Components
  ------------------------------------------------------------------------------
  TRzShellDialog
    Base component for all shell related dialog-based components in 
    Raize Components.

  TRzSelectFolderDialog
    Dialog-based component used to select folders from the Explorer namespace.

  TRzOpenDialog
    Dialog-based component used to select files from the file system.

  TRzSaveDialog
    Dialog-based component used to specify path and file name to be used in
    saving a file.


  Modification History
  ------------------------------------------------------------------------------
  6.1.8  (16 Apr 2014)
    * Added sfdoFilesCanBeFolders option to the TRzSelectFolderDialog.Options
      property.
  ------------------------------------------------------------------------------
  6.0    (09 Oct 2011)
    * Fixed display issue of shell tree in the TRzSelectFolderDialog component.
    * Added new ButtonCaptions property to TRzSelectFolderDialog. Changing any
      of the sub-properties (i.e. OK, Cancel, CreateFolder, DeleteFolder)
      overrides the default resource strings that are used for the buttons on
      the dialog form.
  ------------------------------------------------------------------------------
  4.0.3  (05 Apr 2006)
    * Added the following properties to the TRzOpenDialog, TRzSaveDialog, and
      TRzSelectFolderDialog components:  ToolBtnGradientColorStyle,
      ToolBtnSelectionColorStart, ToolBtnSelectionColorStop,
      ToolBtnSelectionFrameColor, and ToolBtnVisualStyle.  These properties
      allow the developer to change the appearance of the tool buttons used
      in the dialogs.
    * Added ShowButtonGlyphs property to TRzOpenDialog, TRzSaveDialog, and
      TRzSelectFolderDialog, which determines if glyphs are displayed on the
      command buttons used in the dialog (e.g. Ok, Cancel).
  ------------------------------------------------------------------------------
  3.0.13 (15 May 2005)
    * Fixed problem where calling ReadStateFromRegistry from a Shell Dialog
      would result in a Stream Read Error exception.
  ------------------------------------------------------------------------------
  3.0.11 (12 Dec 2004)
    * Added osoFilesCanBeFolders option.
    * Added FormPosition property to all Shell Dialogs, which allows the user
      to control the positioning of the dialog. Default is poScreenCenter.
    * Added FormLeft and FormTop properties to all Shell Dialogs. Use these
      properties when setting FormPosition to poDesigned.
  ------------------------------------------------------------------------------
  3.0.10 (26 Dec 2003)
    * Added properties to base Shell Dialog control to allow shell dialogs to
      use Custom Framing and HotTracking.
  ------------------------------------------------------------------------------
  3.0    (20 Dec 2002)  * Initial inclusion in Raize Components.
===============================================================================}

{$I RzComps.inc}

{$RANGECHECKS OFF}
{$WRITEABLECONST OFF}
{$TYPEDADDRESS ON}

unit RzShellDialogs;

interface

uses
  {$IFDEF USE_CS}
  CodeSiteLogging,
  {$ENDIF}
  Classes,
  Controls,
  Forms,
  Graphics,
  Windows,
  ShlObj,
  RzShellCtrls,
  RzCommon;

type
  TRzSelectFolderDialogOption =
  (
    sfdoCreateDeleteButtons,
    sfdoContextMenus,
    sfdoReadOnly,
    sfdoIncludeNonFolders,
    sfdoOleDrag,
    sfdoOleDrop,
    sfdoCreateFolderIcon,
    sfdoDeleteFolderIcon,
    sfdoVirtualFolders,
    sfdoShowHidden,
    sfdoFilesCanBeFolders
  );
  TRzSelectFolderDialogOptions = set of TRzSelectFolderDialogOption;

const
  DEF_SFDO_OPTIONS = [ sfdoContextMenus, sfdoCreateFolderIcon, sfdoDeleteFolderIcon, sfdoShowHidden ];

type
  {===================================================}
  {== TRzCustomSelectFolderDialog Class Declaration ==}
  {===================================================}

  TRzShellDialog = class( TComponent )
  private
    FFormPosition: TPosition;
    FFormLeft: Integer;
    FFormTop: Integer;
    FFormWidth: Integer;
    FFormHeight: Integer;
    FWindowState: TWindowState;
    FTitle: string;
    FOnInit: TNotifyEvent;
    FOnFormShow: TNotifyEvent;
    FOnFormClose: TNotifyEvent;
    FExecuting: Boolean;

    FFrameColor: TColor;
    FFrameStyle: TFrameStyle;
    FFrameVisible: Boolean;
    FFramingPreference: TFramingPreference;

    FButtonColor: TColor;
    FShowButtonGlyphs: Boolean;
    FHighlightColor: TColor;
    FHotTrack: Boolean;
    FHotTrackColor: TColor;
    FHotTrackColorType: TRzHotTrackColorType;

    FToolBtnGradientColorStyle: TRzGradientColorStyle;
    FToolBtnSelectionColorStart: TColor;
    FToolBtnSelectionColorStop: TColor;
    FToolBtnSelectionFrameColor: TColor;
    FToolBtnVisualStyle: TRzVisualStyle;
  protected
    FAboutInfo: TRzAboutInfo;

    procedure DoInitialized; dynamic;
    function DoExecute: Boolean; dynamic; abstract;

    property Executing: Boolean
      read FExecuting;

    property Title: string
      read FTitle
      write FTitle;

    property FormPosition: TPosition
      read FFormPosition
      write FFormPosition
      default poScreenCenter;

    property FormLeft: Integer
      read FFormLeft
      write FFormLeft
      default -1;

    property FormTop: Integer
      read FFormTop
      write FFormTop
      default -1;

    property FormWidth: Integer
      read FFormWidth
      write FFormWidth
      default -1;

    property FormHeight: Integer
      read FFormHeight
      write FFormHeight
      default -1;

    property FormWindowState: TWindowState
      read FWindowState
      write FWindowState
      default wsNormal;

    property ButtonColor: TColor
      read FButtonColor
      write FButtonColor
      default clBtnFace;

    property ShowButtonGlyphs: Boolean
      read FShowButtonGlyphs
      write FShowButtonGlyphs
      default False;

    property FrameColor: TColor
      read FFrameColor
      write FFrameColor
      default clBtnShadow;

    property FrameStyle: TFrameStyle
      read FFrameStyle
      write FFrameStyle
      default fsFlat;

    property FrameVisible: Boolean
      read FFrameVisible
      write FFrameVisible
      default False;

    property FramingPreference: TFramingPreference
      read FFramingPreference
      write FFramingPreference
      default fpXPThemes;

    property HotTrack: Boolean
      read FHotTrack
      write FHotTrack
      default False;

    property HighlightColor: TColor
      read FHighlightColor
      write FHighlightColor
      default clHighlight;

    property HotTrackColor: TColor
      read FHotTrackColor
      write FHotTrackColor
      default xpHotTrackColor;

    property HotTrackColorType: TRzHotTrackColorType
      read FHotTrackColorType
      write FHotTrackColorType
      default htctActual;

    property ToolBtnGradientColorStyle: TRzGradientColorStyle
      read FToolBtnGradientColorStyle
      write FToolBtnGradientColorStyle
      default gcsSystem;

    property ToolBtnSelectionColorStart: TColor
      read FToolBtnSelectionColorStart
      write FToolBtnSelectionColorStart
      default clBtnFace;

    property ToolBtnSelectionColorStop: TColor
      read FToolBtnSelectionColorStop
      write FToolBtnSelectionColorStop
      default clBtnShadow;

    property ToolBtnSelectionFrameColor: TColor
      read FToolBtnSelectionFrameColor
      write FToolBtnSelectionFrameColor
      default cl3DDkShadow;

    property ToolBtnVisualStyle: TRzVisualStyle
      read FToolBtnVisualStyle
      write FToolBtnVisualStyle
      default vsWinXP;

    property OnInitialized: TNotifyEvent
      read FOnInit
      write FOnInit;

    property OnFormClose: TNotifyEvent
      read FOnFormClose
      write FOnFormClose;

    property OnFormShow: TNotifyEvent
      read FOnFormShow
      write FOnFormShow;
  public
    constructor Create( AOwner: TComponent ); override;

    function Execute: Boolean;

    procedure ReadStateFromRegistry( BaseKey: HKEY; SubKeyName, ValueName: string );
    procedure WriteStateToRegistry( BaseKey: HKEY; SubKeyName, ValueName: string );

    procedure ReadStateFromStream( Stream: TStream ); dynamic;
    procedure WriteStateToStream( Stream: TStream ); dynamic;
  published
  end;



  {===================================================}
  {== TRzCustomSelectFolderDialog Class Declaration ==}
  {===================================================}

  TRzCustomSelectFolderDialog = class;

  TRzFolderBrowseSelChangeEvent = procedure( Sender: TObject; NewSel: PItemIdList ) of object;

  TRzSelectFolderButtonCaptions = class( TPersistent )
  private
    FOK: string;
    FCancel: string;
    FCreateFolder: string;
    FDeleteFolder: string;
  public
    constructor Create;
    procedure Assign( Source: TPersistent ); override;
  published
    property OK: string
      read FOK
      write FOK;

    property Cancel: string
      read FCancel
      write FCancel;

    property CreateFolder: string
      read FCreateFolder
      write FCreateFolder;

    property DeleteFolder: string
      read FDeleteFolder
      write FDeleteFolder;
  end;


  TRzCustomSelectFolderDialog = class( TRzShellDialog )
  private
    FForm: TCustomForm;
    FBaseFolder: TRzShellLocator;

    FOptions: TRzSelectFolderDialogOptions;
    FOnAddItem: TRzShAddItemEvent;
    FOnSelChange: TRzFolderBrowseSelChangeEvent;
    FStatus: string;
    FSelectedFolder: TRzShellLocator;

    FButtonCaptions: TRzSelectFolderButtonCaptions;

    function GetOkEnabled: Boolean;
    function GetSelectedPathName: string;

    procedure SetBaseFolder( Value: TRzShellLocator );
    procedure SetOkEnabled( Value: Boolean );
    procedure SetStatus( const Value: string );
    procedure SetSelectedPathName( const Value: string );
    procedure SetSelectedFolder( Value: TRzShellLocator );
    procedure SetButtonCaptions( Value: TRzSelectFolderButtonCaptions );
  protected
    procedure AssertFormActive;
    procedure AssertFormNotActive;

    function DoExecute: Boolean; override;
    function CreateForm: TCustomForm; virtual;
    procedure InitForm( Form: TCustomForm ); virtual;
    procedure SaveFormSettings( Form: TCustomForm ); virtual;

  protected
    property Form: TCustomForm
      read FForm;

    property ButtonCaptions: TRzSelectFolderButtonCaptions
      read FButtonCaptions
      write SetButtonCaptions;

    property SelectedPathName: string
      read GetSelectedPathName
      write SetSelectedPathName;

    property SelectedFolder: TRzShellLocator
      read FSelectedFolder
      write SetSelectedFolder;

    property Status: string
      read FStatus
      write SetStatus;

    property OkEnabled: Boolean
      read GetOkEnabled
      write SetOkEnabled;

    property BaseFolder: TRzShellLocator
      read FBaseFolder
      write SetBaseFolder;

    property Options: TRzSelectFolderDialogOptions
      read FOptions
      write FOptions
      default DEF_SFDO_OPTIONS;

    property OnAddItem: TRzShAddItemEvent
      read FOnAddItem
      write FOnAddItem;

    property OnSelChange: TRzFolderBrowseSelChangeEvent
      read FOnSelChange
      write FOnSelChange;
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

    property Executing;
  end;


  {=============================================}
  {== TRzSelectFolderDialog Class Declaration ==}
  {=============================================}

  TRzSelectFolderDialog = class( TRzCustomSelectFolderDialog )
  public
    property Form;
    property SelectedFolder;
    property SelectedPathName;
    property Status;
    property OkEnabled;
  published
    property About: TRzAboutInfo
      read FAboutInfo
      write FAboutInfo
      stored False;

    property ButtonCaptions;
    property Title;
    property FormPosition;
    property FormLeft;
    property FormTop;
    property FormWidth;
    property FormHeight;
    property FormWindowState;

    property BaseFolder;
    property Options;

    property FrameColor;
    property FrameStyle;
    property FrameVisible;
    property FramingPreference;
    property ButtonColor;
    property ShowButtonGlyphs;
    property HotTrack;
    property HighlightColor;
    property HotTrackColor;
    property HotTrackColorType;
    property ToolBtnGradientColorStyle;
    property ToolBtnSelectionColorStart;
    property ToolBtnSelectionColorStop;
    property ToolBtnSelectionFrameColor;
    property ToolBtnVisualStyle;

    property OnAddItem;
    property OnFormClose;
    property OnFormShow;
    property OnSelChange;
    property OnInitialized;
  end;



type
  TRzOpenSaveOption =
  (
    osoAllowMultiselect,     // When True, this option allows users to select more than one file in the File Name list view.

    osoCreatePrompt,         // When True, this option displays a dialog box with a message if the user enters a
                             // filename that doesn't exist in the File Name edit box and chooses OK (Open/Save).
                             // The message tells the user the file doesn't exist and asks if the user wants to create
                             // a new file with that name.

    osoExtensionDifferent,   // This option is set when the filename returned from the dialog box has an extension that
                             // differs from the default file extension, the value in the DefaultExt property. Your
                             // application can then use this information. Setting an ofExtensionDifferent value with
                             // the Object Inspector has no meaning.

    osoFileMustExist,        // If True, this option displays a dialog box with a message if the user enters a file that
                             // doesn't exist in the File Name edit box and chooses OK. The message informs the user the
                             // file can't be found and asks the user to make sure they entered the correct path and
                             // filename.
    osoHideReadOnly,
    osoNoChangeDir,
    osoNoDereferenceLinks,   // If True, directs the dialog box to return the path and filename of the selected shortcut
                             // (.LNK) file. If this value is not given, the dialog box returns the path and filename of
                             // the file referenced by the shortcut.

    //osoNoLongNames,
    //osoNoNetworkButton,
    osoNoReadOnlyReturn,     // If True, a message box appears informing the user if the selected file is read-only.

    osoNoTestFileCreate,     // This option applies only when the user wants to save a file on a create-no-modify
                             // network share point, which can't be opened again once it has been opened. If
                             // ofNoTestFileCreate is True, your application won't check for write protection, a full
                             // disk, an open drive door, or network protection when saving the file because doing so
                             // creates a test file. Your application will then have to handle file operations carefully
                             // so that a file isn't closed until you really want it to be.

    osoNoValidate,           // If True, this option doesn't prevent the user from entering invalid characters in a
                             // filename. If ofNoValidate is False and the user enters invalid characters for a filename
                             // in the File Name edit box, a message dialog box appears informing the user the filename
                             // contains invalid characters.

    osoOverwritePrompt,      // If True, this option displays a message dialog box if the user attempts to save a file
                             // that already exists. The message informs the user the file exists and lets the user
                             // choose to overwrite the existing file or not.

    osoReadOnly,             // If True, the Read Only check box is checked when the dialog box is displayed.

    osoPathMustExist,        // If this option is True, the user can type only existing path names as part of the
                             // filename in the File Name edit box. If the user enters a path name that doesn't exist,
                             // a message box appears informing the user that the path name is invalid.

    osoShareAware,           // If True, the dialog box ignores all sharing errors and returns the name of the selected
                             // file even though a sharing violation occurred. If ofShareAware is False, a sharing
                             // violation results in a message box informing the user of the problem.

    osoShowHelp,             // If True, this option displays a Help button in the dialog box.

    osoAllowTree,            // If True then a "Show Tree" button is placed on the button bar
    osoShowTree,             // If True then shows a tree view to the left of the list view, like a mini-explorer.
    osoShowHints,            // If True then popup hints are enabled
    osoHideFoldersInListWhenTreeVisible,

    osoOleDrag,              // True allows Ole drag operations
    osoOleDrop,              // True allows Ole drop operations

    osoShowHidden,           // If False, then hidden and system files do not appear in the tree or list.
    osoFilesCanBeFolders     // Treat certain files (eg. .zip) as folders when supported by the OS

  );

  TRzOpenSaveOptions = set of TRzOpenSaveOption;


const
  DEF_OPEN_OPTIONS = [ osoHideReadOnly, osoAllowTree, osoShowHints, osoOleDrag, osoOleDrop, osoShowHidden ];
  DEF_SAVE_OPTIONS = [ osoHideReadOnly, osoAllowTree, osoShowHints, osoOleDrag, osoOleDrop, osoShowHidden ];


type

  {=====================================}
  {== TRzFileDialog Class Declaration ==}
  {=====================================}

  TRzFileDialog = class( TRzShellDialog )
  private
    FHistoryList: TStrings;
    FOptions: TRzOpenSaveOptions;
    FDefaultExt: string;
    FFiles: TStrings;
    FFilter: string;
    FFilterIndex: Integer;
    FHelpContext: THelpContext;
    FInitialDir: string;
    FFormSplitterPos: Integer;

    FOnAddListItem: TRzShAddItemEvent;
    FOnAddTreeItem: TRzShAddItemEvent;
    FOnAddComboItem: TRzShAddItemEvent;
    FOnFolderChanged: TNotifyEvent;
    FOnSelectionChanged: TNotifyEvent;
    FOnTypeChanged: TNotifyEvent;
    FOnHelp: THelpEvent;

    function  GetFilename: string;
    function  GetFilterIndex: Integer;
    procedure SetFilename( const Value: string );
    procedure SetOptions( Value: TRzOpenSaveOptions );
    procedure SetFilter( const Value: string );
    procedure SetFilterIndex( Value: Integer );
    procedure SetFormSplitterPos( Value: Integer );
    procedure SetOnAddListItem( Value: TRzShAddItemEvent );
    procedure SetOnAddTreeItem( Value: TRzShAddItemEvent );
    procedure SetOnAddComboItem( Value: TRzShAddItemEvent );
    procedure SetHistoryList( Value: TStrings );
  protected
    FForm: TCustomForm;

    function DoExecute: Boolean; override;
    function CreateForm: TCustomForm; virtual;
    procedure InitForm( Form: TCustomForm ); virtual;
    procedure SaveFormSettings( Form: TCustomForm ); virtual;

    property Form: TCustomForm
      read FForm;

    property DefaultExt: string
      read FDefaultExt
      write FDefaultExt;

    property Options: TRzOpenSaveOptions
      read FOptions
      write SetOptions;

    property FileName: string
      read GetFilename
      write SetFilename;

    property Files: TStrings
      read FFiles;

    property Filter: string
      read FFilter
      write SetFilter;

    property FilterIndex: Integer
      read GetFilterIndex
      write SetFilterIndex
      default 1;

    property FormSplitterPos: Integer
      read FFormSplitterPos
      write SetFormSplitterPos
      default -1;

    property HelpContext: THelpContext
      read FHelpContext
      write FHelpContext
      default 0;

    property HistoryList: TStrings
      read FHistoryList
      write SetHistoryList
      stored False;

    property InitialDir: string
      read FInitialDir
      write FInitialDir;

    property OnAddListItem: TRzShAddItemEvent
      read FOnAddListItem
      write SetOnAddListItem;

    property OnAddTreeItem: TRzShAddItemEvent
      read FOnAddTreeItem
      write SetOnAddTreeItem;

    property OnAddComboItem: TRzShAddItemEvent
      read FOnAddComboItem
      write SetOnAddComboItem;

    property OnHelp: THelpEvent
      read FOnHelp
      write FOnHelp;

    property OnFolderChanged: TNotifyEvent
      read FOnFolderChanged
      write FOnFolderChanged;

    property OnSelectionChanged: TNotifyEvent
      read FOnSelectionChanged
      write FOnSelectionChanged;

    property OnTypeChanged: TNotifyEvent
      read FOnTypeChanged
      write FOnTypeChanged;
  public
    constructor Create( AOwner: TComponent ); override;
    destructor  Destroy; override;

    procedure ReadStateFromStream( Stream: TStream ); override;
    procedure WriteStateToStream( Stream: TStream ); override;

  published
    property FrameColor;
    property FrameStyle;
    property FrameVisible;
    property FramingPreference;
    property ButtonColor;
    property ShowButtonGlyphs;
    property HotTrack;
    property HighlightColor;
    property HotTrackColor;
    property HotTrackColorType;
    property ToolBtnGradientColorStyle;
    property ToolBtnSelectionColorStart;
    property ToolBtnSelectionColorStop;
    property ToolBtnSelectionFrameColor;
    property ToolBtnVisualStyle;
  end;


  {===========================================}
  {== TRzCustomOpenDialog Class Declaration ==}
  {===========================================}

  TRzCustomOpenDialog = class( TRzFileDialog )
  protected
    procedure InitForm( Form: TCustomForm ); override;
  public
    constructor Create( AOwner: TComponent ); override;
  end;


  {=====================================}
  {== TRzOpenDialog Class Declaration ==}
  {=====================================}

  TRzOpenDialog = class( TRzCustomOpenDialog )
  public
    property Executing;
    property FileName;
    property Files;
    property Form;
  published
    property About: TRzAboutInfo
      read FAboutInfo
      write FAboutInfo
      stored False;

    property Title;
    property Options default DEF_OPEN_OPTIONS;
    property Filter;
    property FilterIndex;
    property FormPosition;
    property FormLeft;
    property FormTop;
    property FormWidth;
    property FormHeight;
    property FormWindowState;
    property FormSplitterPos;
    property HelpContext;
    property HistoryList;
    property InitialDir;

    property DefaultExt;

    property OnAddListItem;
    property OnAddTreeItem;
    property OnAddComboItem;
    property OnInitialized;

    property OnHelp;

    property OnFormClose;
    property OnFormShow;
    property OnFolderChanged;
    property OnSelectionChanged;
    property OnTypeChanged;
  end;



  {===========================================}
  {== TRzCustomSaveDialog Class Declaration ==}
  {===========================================}

  TRzCustomSaveDialog = class( TRzFileDialog )
  protected
    procedure InitForm( Form: TCustomForm ); override;
  public
    constructor Create( aOwner: TComponent ); override;
  end;


  {=====================================}
  {== TRzSaveDialog Class Declaration ==}
  {=====================================}

  TRzSaveDialog = class( TRzCustomSaveDialog )
  public
    property Executing;
    property FileName;
    property Files;
    property Form;
  published
    property About: TRzAboutInfo
      read FAboutInfo
      write FAboutInfo
      stored False;

    property DefaultExt;

    property Title;
    property Options default DEF_SAVE_OPTIONS;
    property Filter;
    property FilterIndex;
    property FormPosition;
    property FormLeft;
    property FormTop;
    property FormWidth;
    property FormHeight;
    property FormWindowState;
    property FormSplitterPos;
    property HelpContext;
    property HistoryList;
    property InitialDir;

    property OnAddListItem;
    property OnAddTreeItem;
    property OnAddComboItem;
    property OnInitialized;

    property OnHelp;

    property OnFormClose;
    property OnFormShow;
    property OnFolderChanged;
    property OnSelectionChanged;
    property OnTypeChanged;
  end;


implementation

uses
  SysUtils,
  Registry,
  RzShellConsts,
  RzShellFolderForm,
  RzShellOpenForm;


procedure RaiseNotActive;
begin
  raise Exception.Create( 'TRzSelectFolderDialog form not active' );
end;


procedure RaiseAlreadyActive;
begin
  raise Exception.Create( 'TRzSelectFolderDialog form already active' );
end;


procedure GetWndRestoreRect( h: HWND; var r: TRect );
var
  wp: TWindowPlacement;
begin
  wp.length := Sizeof( TWindowPlacement );
  GetWindowPlacement( h, @wp );
  r := wp.rcNormalPosition;
end;


type
  TMemoryStream2 = class( TMemoryStream )
  public
    property Capacity;  // Why wasn't this public to start with???
  end;


{============================}
{== TRzShellDialog Methods ==}
{============================}

constructor TRzShellDialog.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );
  FFormLeft := -1;
  FFormTop := -1;
  FFormWidth := -1;
  FFormHeight := -1;
  FWindowState := wsNormal;
  FFormPosition := poScreenCenter;


  FFrameColor := clBtnShadow;
  FFrameStyle := fsFlat;
  FFrameVisible := False;
  FFramingPreference := fpXPThemes;

  FButtonColor := clBtnFace;
  FShowButtonGlyphs := False;
  FHighlightColor := clHighlight;
  FHotTrack := False;
  FHotTrackColor := xpHotTrackColor;
  FHotTrackColorType := htctActual;

  FToolBtnGradientColorStyle := gcsSystem;
  FToolBtnSelectionColorStart := clBtnFace;
  FToolBtnSelectionColorStop := clBtnShadow;
  FToolBtnSelectionFrameColor := cl3DDkShadow;
  FToolBtnVisualStyle := vsWinXP;
end;


procedure TRzShellDialog.DoInitialized;
begin
  if Assigned( FOnInit ) then
    FOnInit( Self );
end;


procedure TRzShellDialog.ReadStateFromRegistry( BaseKey: HKEY; SubKeyName, ValueName: string );
var
  r: TRegistry;
  ms: TMemoryStream2;
begin
  r := TRegistry.Create;
  ms := TMemoryStream2.Create;
  ms.Capacity := 128;
  try
    r.RootKey := baseKey;
    r.OpenKey( SubKeyName, False );
    ms.Size := r.ReadBinaryData( ValueName, ms.Memory^, ms.Capacity );
    ms.Position := 0;
    ReadStateFromStream( ms );
    r.CloseKey;
  finally
    ms.Free;
    r.Free;
  end;
end;


procedure TRzShellDialog.WriteStateToRegistry( BaseKey: HKEY; SubKeyName, ValueName: string );
var
  r: TRegistry;
  ms: TMemoryStream;
begin
  r := TRegistry.Create;
  ms := TMemoryStream.Create;
  try
    r.RootKey := baseKey;
    r.OpenKey( SubKeyName, True );
    WriteStateToStream( ms );
    r.WriteBinaryData( ValueName, ms.Memory^, ms.Size );
    r.CloseKey;
  finally
    ms.Free;
    r.Free;
  end;
end;


procedure TRzShellDialog.ReadStateFromStream( Stream: TStream );
begin
  Stream.ReadBuffer( FFormLeft, SizeOf( Integer ) );
  Stream.ReadBuffer( FFormTop, SizeOf( Integer ) );
  Stream.ReadBuffer( FFormWidth, SizeOf( Integer ) );
  Stream.ReadBuffer( FFormHeight, SizeOf( Integer ) );
  Stream.ReadBuffer( FWindowState, SizeOf( TWindowState ) );
  Stream.ReadBuffer( FFormPosition, SizeOf( TPosition ) );
end;


procedure TRzShellDialog.WriteStateToStream( Stream: TStream );
begin
  Stream.WriteBuffer( FFormLeft, SizeOf( Integer ) );
  Stream.WriteBuffer( FFormTop, SizeOf( Integer ) );
  Stream.WriteBuffer( FFormWidth, SizeOf( Integer ) );
  Stream.WriteBuffer( FFormHeight, SizeOf( Integer ) );
  Stream.WriteBuffer( FWindowState, SizeOf( TWindowState ) );
  Stream.WriteBuffer( FFormPosition, SizeOf( TPosition ) );
end;


function TRzShellDialog.Execute: Boolean;
begin
  if FExecuting then
    raise Exception.Create( ClassName + ' already executing' );
  FExecuting := True;
  try
    Result := DoExecute;
  finally
    FExecuting := False;
  end;
end;


{===========================================}
{== TRzSelectFolderButtonCaptions Methods ==}
{===========================================}

constructor TRzSelectFolderButtonCaptions.Create;
begin
  inherited Create;
  FOK := '';
  FCancel := '';
  FCreateFolder := '';
  FDeleteFolder := '';
end;


procedure TRzSelectFolderButtonCaptions.Assign( Source: TPersistent );
begin
  if Source is TRzSelectFolderButtonCaptions then
  begin
    OK := TRzSelectFolderButtonCaptions( Source ).OK;
    Cancel := TRzSelectFolderButtonCaptions( Source ).Cancel;
    CreateFolder := TRzSelectFolderButtonCaptions( Source ).CreateFolder;
    DeleteFolder := TRzSelectFolderButtonCaptions( Source ).DeleteFolder;
  end
  else
    inherited;
end;


{=========================================}
{== TRzCustomSelectFolderDialog Methods ==}
{=========================================}

constructor TRzCustomSelectFolderDialog.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );
  Title := '';
  Options := DEF_SFDO_OPTIONS;
  FSelectedFolder := TRzShellLocator.Create;
  FBaseFolder := TRzShellLocator.Create;

  FButtonCaptions := TRzSelectFolderButtonCaptions.Create;
end;


destructor TRzCustomSelectFolderDialog.Destroy;
begin
  FSelectedFolder.Free;
  FBaseFolder.Free;
  FButtonCaptions.Free;
  inherited;
end;


function TRzCustomSelectFolderDialog.GetOkEnabled: Boolean;
begin
  AssertFormActive;
  Result := TRzSelectFolderForm( FForm ).OkEnabled;
end;


function TRzCustomSelectFolderDialog.GetSelectedPathname: string;
begin
  Result := FSelectedFolder.Pathname;
end;


procedure TRzCustomSelectFolderDialog.SetBaseFolder( Value: TRzShellLocator );
begin
  FBaseFolder.Assign( Value );
end;


procedure TRzCustomSelectFolderDialog.SetOkEnabled( Value: Boolean );
begin
  AssertFormActive;
  TRzSelectFolderForm( FForm ).OkEnabled := Value;
end;


procedure TRzCustomSelectFolderDialog.SetStatus( const Value: string );
begin
  if Assigned( FForm ) then
    TRzSelectFolderForm( FForm ).Status := Value;
  FStatus := Value;
end;


procedure TRzCustomSelectFolderDialog.SetSelectedPathName( const Value: string );
begin
  FSelectedFolder.PathName := Value;
end;


procedure TRzCustomSelectFolderDialog.SetSelectedFolder( Value: TRzShellLocator );
begin
  FSelectedFolder.Assign( Value );
end;


procedure TRzCustomSelectFolderDialog.SetButtonCaptions( Value: TRzSelectFolderButtonCaptions );
begin
  FButtonCaptions.Assign( Value );
end;


procedure TRzCustomSelectFolderDialog.AssertFormActive;
begin
  if not Assigned( FForm ) then
    RaiseNotActive;
end;


procedure TRzCustomSelectFolderDialog.AssertFormNotActive;
begin
  if Assigned( FForm ) then
    RaiseAlreadyActive;
end;


function TRzCustomSelectFolderDialog.DoExecute: Boolean;
var
  c: TCursor;
  f: TRzSelectFolderForm;
begin
  AssertFormNotActive;
  c := Screen.Cursor;
  Screen.Cursor := crHourglass;
  f := nil;
  try
    f := TRzSelectFolderForm( CreateForm );
    InitForm( f );
    FForm := f;
    f := nil;

    DoInitialized;
    Result := ( FForm.ShowModal = mrOk );

    if not ( csDesigning in ComponentState ) then
      SaveFormSettings( FForm );
  finally
    FForm.Free;
    FForm := nil;
    f.Free;
    Screen.Cursor := c;
  end;
end; {= TRzCustomSelectFolderDialog.DoExecute =}


function TRzCustomSelectFolderDialog.CreateForm: TCustomForm;
begin
  Result := TRzSelectFolderForm.Create( Application );
end;


procedure TRzCustomSelectFolderDialog.InitForm( Form: TCustomForm );
var
  F: TRzSelectFolderForm;
begin
  F := TRzSelectFolderForm( Form );
  F.OnSelChange := OnSelChange;
  F.ShellTree.OnAddItem := OnAddItem;
  F.OnFormShow := OnFormShow;
  F.OnFormClose := OnFormClose;

  F.Status := Status;
  F.Position := FormPosition;
  if FormPosition = poDesigned then
  begin
    F.Left := FormLeft;
    F.Top := FormTop;
  end;

  if ( FormWidth > 0 ) then
    F.Width := FormWidth;
  if ( FormHeight > 0 ) then
    F.Height := FormHeight;
  F.WindowState := FormWindowState;

  F.Options := Options;

  F.ButtonCaptions := FButtonCaptions;
  if Title = '' then
    F.Caption := SBrowseForFolder
  else
    F.Caption := Title;

  F.InitFraming( FFrameColor, FFrameStyle, FFrameVisible, FFramingPreference );
  F.InitHotTracking( FButtonColor, FHotTrack, FHighlightColor,
                     FHotTrackColor, FHotTrackColorType );
  F.InitShowButtonGlyphs( FShowButtonGlyphs );
  F.InitToolButtons( FToolBtnGradientColorStyle, FToolBtnSelectionColorStart,
                     FToolBtnSelectionColorStop, FToolBtnSelectionFrameColor,
                     FToolBtnVisualStyle );

  F.ShellTree.BaseFolder := BaseFolder;
  F.ShellTree.SelectedFolder := SelectedFolder;
end;


procedure TRzCustomSelectFolderDialog.SaveFormSettings( Form: TCustomForm );
var
  tmpr: TRect;
begin
  GetWndRestoreRect( Form.Handle, tmpr );
  FormLeft := tmpr.left;
  FormTop := tmpr.top;
  FormWidth := tmpr.right - tmpr.left;
  FormHeight := tmpr.bottom - tmpr.top;

  FormWindowState := Form.WindowState;
  FormPosition := TRzSelectFolderForm( Form ).Position;

  if ( Form.ModalResult = mrOk ) then
    SelectedFolder.Assign( TRzSelectFolderForm( Form ).ShellTree.SelectedFolder );
end;


{===========================}
{== TRzFileDialog Methods ==}
{===========================}

constructor TRzFileDialog.Create( AOwner: TComponent );
begin
  inherited;
  FFormSplitterPos := -1;
  FFilterIndex := 1;
  FFiles := TStringList.Create;
  FHistoryList := TStringList.Create;
end;


destructor TRzFileDialog.Destroy;
begin
  FFiles.Free;
  FHistoryList.Free;
  inherited;
end;


function TRzFileDialog.CreateForm: TCustomForm;
begin
  Result := TRzShellOpenSaveForm.Create( Application );
end;


function TRzFileDialog.DoExecute: Boolean;
var
  c: TCursor;
  f: TRzShellOpenSaveForm;
begin
  if Assigned(FForm) then
    raise Exception.Create( Format('%s already executing', [Name]) );

  c := Screen.Cursor;
  Screen.Cursor := crHourglass;
  try
    if (csDesigning in ComponentState) then
      FFiles.Clear;

    f := TRzShellOpenSaveForm( CreateForm );
    InitForm(f);
    FForm := f;

    DoInitialized;

    Result := (FForm.ShowModal = mrOk);

    if not ( csDesigning in ComponentState ) then
      SaveFormSettings( FForm );
  finally
    FForm.Free;
    FForm := nil;
    Screen.Cursor := c;
  end;
end;


procedure TRzFileDialog.ReadStateFromStream( Stream: TStream );
var
  f: Boolean;
begin
  inherited ReadStateFromStream( Stream );
  Stream.ReadBuffer( f, Sizeof( Boolean ) );
  if f then
    Include( FOptions, osoShowTree )
  else
    Exclude( FOptions, osoShowTree );
  Stream.ReadBuffer( FFormSplitterPos, Sizeof( Integer ) );
end;


procedure TRzFileDialog.WriteStateToStream( Stream: TStream );
var
  f: Boolean;
begin
  inherited;
  f := ( osoShowTree in FOptions );
  Stream.WriteBuffer( f, Sizeof( Boolean ) );
  Stream.WriteBuffer( FFormSplitterPos, Sizeof( FFormSplitterPos ) );
end;


function TRzFileDialog.GetFilename: string;
begin
  if Assigned(FForm) then
    Result := TRzShellOpenSaveForm( FForm ).FileName
  else
    if FFiles.Count>0 then
      Result := FFiles[0]
    else
      Result := '';
end;


function TRzFileDialog.GetFilterIndex: Integer;
begin
  if Assigned( FForm ) then
    Result := TRzShellOpenSaveForm( FForm ).FilterIndex
  else
    Result := FFilterIndex;
end;


procedure TRzFileDialog.SetFilename( const Value: string );
begin
  FFiles.Clear;
  FFiles.Add( Value );
  if Assigned( FForm ) then
    TRzShellOpenSaveForm( FForm ).Filename := Value;
end;


procedure TRzFileDialog.SetOptions( Value: TRzOpenSaveOptions );
begin
  FOptions := Value;
  if Assigned( FForm ) then
    TRzShellOpenSaveForm( FForm ).Options := Value;
end;


procedure TRzFileDialog.SetFilter( const Value: string );
begin
  FFilter := Value;
  if Assigned( FForm ) then
    TRzShellOpenSaveForm( FForm ).Filter := Value;
end;


procedure TRzFileDialog.SetFilterIndex( Value: Integer );
begin
  FFilterIndex := Value;
  if Assigned( FForm ) then
    TRzShellOpenSaveForm( FForm ).FilterIndex := Value;
end;


procedure TRzFileDialog.SetFormSplitterPos( Value: Integer );
begin
  FFormSplitterPos := Value;
  if Assigned( FForm ) then
    TRzShellOpenSaveForm( FForm ).FormSplitterPos := Value;
end;


procedure TRzFileDialog.SetHistoryList( Value: TStrings );
begin
  FHistoryList.Assign( Value );
end;


procedure TRzFileDialog.InitForm( Form: TCustomForm );
var
  F: TRzShellOpenSaveForm;
begin
  F := TRzShellOpenSaveForm( Form );

  F.OnAddListItem := OnAddListItem;
  F.OnAddTreeItem := OnAddTreeItem;
  F.OnAddComboItem := OnAddComboItem;

  F.OnFormClose := OnFormClose;
  F.OnFormShow := OnFormShow;
  F.OnTypeChanged := OnTypeChanged;
  F.OnFolderChanged := OnFolderChanged;
  F.OnSelectionChanged := OnSelectionChanged;

  F.InitialDir := InitialDir;
  F.FileName := FileName;
  F.DefaultExt := DefaultExt;

  F.Position := FormPosition;
  if FormPosition = poDesigned then
  begin
    F.Left := FormLeft;
    F.Top := FormTop;
  end;

  F.Options := Options;
  if FormWidth > 0 then
    F.Width := FormWidth;
  if FormHeight > 0 then
    F.Height := FormHeight;
  F.WindowState := FormWindowState;

  F.Filter := Filter;
  F.FilterIndex := FilterIndex;
  F.FormSplitterPos := FormSplitterPos;

  F.InitFraming( FFrameColor, FFrameStyle, FFrameVisible, FFramingPreference );
  F.InitHotTracking( FButtonColor, FHotTrack, FHighlightColor,
                     FHotTrackColor, FHotTrackColorType );
  F.InitShowButtonGlyphs( FShowButtonGlyphs );
  F.InitToolButtons( FToolBtnGradientColorStyle, FToolBtnSelectionColorStart,
                     FToolBtnSelectionColorStop, FToolBtnSelectionFrameColor,
                     FToolBtnVisualStyle );

  F.lblLookIn.Caption := SLookIn;
  F.lblFilesOfType.Caption := SFilesOfType;
  F.HelpContext := HelpContext;
  F.OnFormHelp := OnHelp;
end;


procedure TRzFileDialog.SaveFormSettings( Form: TCustomForm );
var
  tmpr: TRect;
  F: TRzShellOpenSaveForm;
begin
  F := TRzShellOpenSaveForm( Form );
  Options := F.Options;
  Files.Assign( F.Files );

  GetWndRestoreRect( F.Handle, tmpr );
  FormLeft := tmpr.left;
  FormTop := tmpr.top;
  FormWidth := tmpr.right - tmpr.left;
  FormHeight := tmpr.bottom - tmpr.top;
  FormWindowState := F.WindowState;
  FormPosition := F.Position;

  FilterIndex := F.FilterIndex;
  FormSplitterPos := F.FormSplitterPos;
end;


procedure TRzFileDialog.SetOnAddListItem( Value: TRzShAddItemEvent );
begin
  FOnAddListItem := Value;
  if Assigned( FForm ) then
    TRzShellOpenSaveForm( FForm ).OnAddListItem := Value;
end;


procedure TRzFileDialog.SetOnAddTreeItem( Value: TRzShAddItemEvent );
begin
  FOnAddTreeItem := Value;
  if Assigned( FForm ) then
    TRzShellOpenSaveForm( FForm ).OnAddTreeItem := Value;
end;

procedure TRzFileDialog.SetOnAddComboItem( Value: TRzShAddItemEvent );
begin
  FOnAddComboItem := Value;
//  if Assigned(FForm) then FForm.PTShellTree1.OnAddItem := aValue;
end;


{=================================}
{== TRzCustomOpenDialog Methods ==}
{=================================}

constructor TRzCustomOpenDialog.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );
  FOptions := DEF_OPEN_OPTIONS;
end;

procedure TRzCustomOpenDialog.InitForm( Form: TCustomForm );
var
  F: TRzShellOpenSaveForm;
begin
  inherited;

  F := TRzShellOpenSaveForm( Form );

  if (Title = '') then
    F.Caption := SOpenCaption
  else
    F.Caption := Title;

  F.btnOpen.Caption := SOpenButton;
end;


{=================================}
{== TRzCustomSaveDialog Methods ==}
{=================================}

constructor TRzCustomSaveDialog.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );
  FOptions := DEF_SAVE_OPTIONS;
end;


procedure TRzCustomSaveDialog.InitForm( Form: TCustomForm );
var
  F: TRzShellOpenSaveForm;
begin
  inherited;

  F := TRzShellOpenSaveForm( Form );

  if (Title = '') then
    F.Caption := SSaveAsCaption
  else
    F.Caption := Title;

  F.btnOpen.Caption := SSaveButton;
end;


end.

