{===============================================================================
  RzShellOpenForm Unit

  Raize Components - Component Source Unit

  Copyright © 1995-2015 by Raize Software, Inc.  All Rights Reserved.


  Components
  ------------------------------------------------------------------------------
  TRzShellOpenSaveForm
    Custom dialog box form for Open and Save Dialog boxes.


  Modification History
  ------------------------------------------------------------------------------
  6.1.5  (02 Oct 2013)
    * Fixed issue where custom Places that specified a directory path including
      a trailing path delimiter would display an empty caption.
    * Fixed issues that could occur if the custom places section in the Registry
      contained invalid values.
  ------------------------------------------------------------------------------
  6.1.4  (29 May 2013)
    * Fixed issue where PlaceBar settings were not displayed in Windows 7
      and later.
    * Fixed issue where custom places could potentially cause an access
      violation when the dialog was displayed.
  ------------------------------------------------------------------------------
  6.1    (20 Sep 2012)
    * Fixed display issue in TRzOpenDialog and TRzSaveDialog that caused one
      of the panels in the dialog to appear incorrectly when using VCL Styles.
  ------------------------------------------------------------------------------
  6.0    (09 Oct 2011)
    * Made necessary modifications to TRzShellOpenSaveForm to support 64-bit.
      TRzShellOpenSaveForm is used by the TRzOpenDialog and TRzSaveDialog
      components.
  ------------------------------------------------------------------------------
  5.1    (16 Mar 2009)
    * Fixed flickering issue with Places bar and toolbar in the TRzOpenDialog
      and TRzSaveDialog components.
  ------------------------------------------------------------------------------
  4.1    (15 Dec 2006)
    * Fixed problem where navigating to a different directory and the user has
      less than 5 places (custom or default) would cause an index out of range
      exception.
    * Fixed issue where the incorrect caption was displayed in the Places bar
      for the "My Recent Documents" button in TRzOpenDialog and TRzSaveDialog.
    * Updated height of toolbar buttons on TRzOpenDialog and TRzSaveDialog to
      match height of combo box.
    * Fixed problem where the Desktop Places Bar button would not remain
      depressed after navigating to the desktop.
  ------------------------------------------------------------------------------
  4.0.3  (05 Apr 2006)
    * TRzOpenDialog and TRzSaveDialog now correctly handle situations where less
      than 5 Custom Places are defined by the system.
    * Fixed problem where the file list in a TRzOpenDialog or TRzSaveDialog
      would not get resized correctly when the form was maximized.
    * The Places buttons in the TRzOpenDialog and TRzSaveDialog now remain
      depressed if the current folder matches the place represented by the
      button.
  ------------------------------------------------------------------------------
  4.0    (23 Dec 2005)
    * Fixed problem where invoking the Open/Save dialog and specifying a
      DefaultExt but not any filters, caused an Index Out of Range exception if
      the user typed a file without an extension and closed the dialog box.
    * Fixed problem where invoking a TRzOpenDialog or TRzSaveDialog in Delphi 5
      would result in an exception.
    * Fixed issue where certain REG_EXPAND_SZ format Custom Places were not
      handled correctly in the TRzOpenDialog and TRzSaveDialog.
    * The icons displayed in the Custom Places bar now correctly pick up
      specialized folder icons.
  ------------------------------------------------------------------------------
  3.1    (04 Aug 2005)
    * The TRzOpenDialog and TRzSaveDialog components are now aware of changes
      made to the Custom Places bar.
    * Added new ButtonColor property to shell dialog boxes, which can be used to
      change the color of the push buttons used in the dialog boxes. This is
      useful when using the HotTracking feature.
  ------------------------------------------------------------------------------
  3.0.8  (29 Aug 2003)
    * Fixed problem where ShellList would not be initialized correctly if user
      specified a Filter and FilterIndex value for the TRzOpenDialog or
      TRzSaveDialog.
    * Replaced call to Application.HelpContext with a call to
      Application.HelpCommand to work-around the problem of HelpContext not
      generating a wm_Help message.
  ------------------------------------------------------------------------------
  3.0    (20 Dec 2002)
    * Initial inclusion in Raize Components.
===============================================================================}

{$I RzComps.inc}

{$RANGECHECKS OFF}
{$WRITEABLECONST OFF}
{$TYPEDADDRESS ON}

unit RzShellOpenForm;

interface

uses
  {$IFDEF USE_CS}
  CodeSiteLogging,
  {$ENDIF}
  Types,
  Classes,
  Windows,
  Messages,
  Controls,
  Forms,
  Graphics,
  Menus,
  Contnrs,
  ComCtrls,
  StdCtrls,
  ExtCtrls,
  Buttons,
  RzCommon,
  RzListVw,
  RzTreeVw,
  RzCmboBx,
  RzPanel,
  RzSplit,
  ImgList,
  RzButton,
  RzRadChk,
  Mask,
  RzEdit,
  RzShellCtrls,
  RzShellUtils,
  RzShellDialogs;

type
  TRzShellOpenSaveForm_LIS = ( lisEdit, lisList ); // C++ Builder demands formal type decl for enumerations.

  TRzShellOpenSaveForm = class( TForm )
    ShellCombo: TRzShellCombo;
    btnUpOneLevel: TRzToolButton;
    btnList: TRzToolButton;
    btnDetails: TRzToolButton;
    btnCreateNewFolder: TRzToolButton;
    PnlEdits: TRzPanel;
    lblFileName: TLabel;
    lblFilesOfType: TLabel;
    cbxFileTypes: TRzComboBox;
    edtFileName: TRzEdit;
    btnOpen: TRzBitBtn;
    btnCancel: TRzBitBtn;
    cbxFileName: TRzComboBox;
    chkReadOnly: TRzCheckBox;
    btnShowTree: TRzToolButton;
    btnHelp: TRzBitBtn;
    LvPopup: TPopupMenu;
    View1Mitm: TMenuItem;
    N1: TMenuItem;
    New1Mitm: TMenuItem;
    N2: TMenuItem;
    Properties1Mitm: TMenuItem;
    Folder1Mitm: TMenuItem;
    LargeIcons1Mitm: TMenuItem;
    Smallicons1MItm: TMenuItem;
    List1Mitm: TMenuItem;
    Details1Mitm: TMenuItem;
    Paste1Mitm: TMenuItem;
    N3: TMenuItem;
    btnShowDesktop: TRzToolButton;
    RzSplitter1: TRzSplitter;
    ShellTree: TRzShellTree;
    ShellList: TRzShellList;
    imlPlaces: TImageList;
    PnlJumps: TPanel;
    btnPlace0: TRzToolButton;
    btnPlace1: TRzToolButton;
    btnPlace2: TRzToolButton;
    btnPlace3: TRzToolButton;
    btnPlace4: TRzToolButton;
    lblLookIn: TLabel;
    PnlWork: TPanel;
    imlToolbar: TImageList;
    procedure ViewBtnClick( Sender: TObject );
    procedure ShellListChange( Sender: TObject; Item: TListItem; Change: TItemChange );
    procedure btnUpOneLevelClick( Sender: TObject );
    procedure btnShowTreeClick( Sender: TObject );
    procedure FormDestroy( Sender: TObject );
    procedure FormKeyDown( Sender: TObject; var Key: Word; Shift: TShiftState );
    procedure FormCloseQuery( Sender: TObject; var CanClose: Boolean );
    procedure ShellTreeChange( Sender: TObject; Node: TTreeNode );
    procedure cbxFileTypesSelEndOk( Sender: TObject );
    procedure btnCreateNewFolderClick( Sender: TObject );
    procedure edtFileNameChange( Sender: TObject );
    procedure Paste1MitmClick( Sender: TObject );
    procedure Properties1MitmClick( Sender: TObject );
    procedure btnHelpClick( Sender: TObject );
    procedure chkReadOnlyClick( Sender: TObject );
    procedure ShellListFolderChanged( Sender: TObject );
    procedure FormResize( Sender: TObject );
    procedure btnShowDesktopClick( Sender: TObject );
    procedure FormCreate(Sender: TObject);
    procedure btnPlaceClick(Sender: TObject);
  private
    FPlacesList: TObjectList;
    function FormHelp( Command: Word; Data: TRzNativeInt; var CallHelp: Boolean ): Boolean;
    procedure ListDblClickOpen( Sender: TObject;  var Handled: Boolean );
    procedure WMGetMinMaxInfo( var Msg: TWMGetMinMaxInfo ); message WM_GETMINMAXINFO;
    procedure InitCustomPlaces;
    procedure JumpToPlace( Num: Integer );
    procedure UpdateCurrentPlace;
  protected
    FDefaultExt: string;
    FOptions: TRzOpenSaveOptions;
    FFiles: TStrings;  // Last request for 'files'
    FFilter: string;
    FInitialDir: string;

    FOnTypeChanged: TNotifyEvent;
    FOnFolderChanged: TNotifyEvent;
    FOnSelectionChanged: TNotifyEvent;
    FOnFormShow: TNotifyEvent;
    FOnFormClose: TNotifyEvent;
    FOnFormHelp: THelpEvent;

    procedure DoOnFormClose; dynamic;
    procedure DoOnFolderChanged; dynamic;
    procedure DoOnSelectionChanged; dynamic;
    procedure DoOnFormShow; dynamic;
    procedure DoOnTypeChanged; dynamic;

    function  GetFilename: string;
    function  GetFiles: TStrings;
    function  GetFilterIndex: Integer;
    function  GetFormSplitterPos: Integer;
    function  GetOnAddListItem: TRzShAddItemEvent;
    function  GetOnAddTreeItem: TRzShAddItemEvent;
    function  GetOnAddComboItem: TRzShAddItemEvent;

    procedure SetFilename( const Value: string );
    procedure SetFilter( const Value: string );
    procedure SetFilterIndex( Value: Integer );
    procedure SetFormSplitterPos( Value: Integer );
    procedure SetInitialDir( const Value: string );
    procedure SetOptions( Value: TRzOpenSaveOptions );
    procedure SetOnAddListItem( Value: TRzShAddItemEvent );
    procedure SetOnAddTreeItem( Value: TRzShAddItemEvent );
    procedure SetOnAddComboItem( Value: TRzShAddItemEvent );

  protected
    FUserFilter: string; // Used for filters typed into the filename box
    FExecuting: Boolean;

    FSelections: TStrings;
    FLastInputState: TRzShellOpenSaveForm_LIS;

    FHGripWindow: HWND;

    procedure CreateWnd; override;
    procedure DoTranslation; dynamic;
    procedure ApplyUserFilter( Filter: string );
    procedure GetSelectedFiles( s: TStrings );
    procedure ShowTree( Show: Boolean );

    procedure DoHide; override;
    procedure DoShow; override;
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;
    function  ParseInputstring( const ins: string ): Boolean;

    procedure InitFraming( FrameColor: TColor; FrameStyle: TFrameStyle;
                           FrameVisible: Boolean;
                           FramingPreference: TFramingPreference );
    procedure InitHotTracking( ButtonColor: TColor;
                               HotTrack: Boolean; HighlightColor: TColor;
                               HotTrackColor: TColor;
                               HotTrackColorType: TRzHotTrackColorType );

    procedure InitShowButtonGlyphs( ShowButtonGlyphs: Boolean );
    procedure InitToolButtons( ToolBtnGradientColorStyle: TRzGradientColorStyle;
                               ToolBtnSelectionColorStart,
                               ToolBtnSelectionColorStop,
                               ToolBtnSelectionFrameColor: TColor;
                               ToolBtnVisualStyle: TRzVisualStyle );

    property DefaultExt: string
      read FDefaultExt
      write FDefaultExt;

    property Executing: Boolean
      read FExecuting;

    property Options: TRzOpenSaveOptions
      read FOptions
      write SetOptions;

    property FileName: string
      read GetFilename
      write SetFilename;

    property Files: TStrings
      read GetFiles;

    property Filter: string
      read FFilter
      write SetFilter;

    property FilterIndex: Integer
      read GetFilterIndex
      write SetFilterIndex
      default 1; // Does default count in this situation?

    property FormSplitterPos: Integer
      read GetFormSplitterPos
      write SetFormSplitterPos
      default -1; // Does default count in this situation?

    property HelpContext;

    property InitialDir: string
      read FInitialDir
      write SetInitialDir;

    property OnAddListItem: TRzShAddItemEvent
      read GetOnAddListItem
      write SetOnAddListItem;

    property OnAddTreeItem: TRzShAddItemEvent
      read GetOnAddTreeItem
      write SetOnAddTreeItem;

    property OnAddComboItem: TRzShAddItemEvent
      read GetOnAddComboItem
      write SetOnAddComboItem;

    property OnHelp;

    property OnFormHelp: THelpEvent
      read FOnFormHelp
      write FOnFormHelp;

    property OnFormClose: TNotifyEvent
      read FOnFormClose
      write FOnFormClose;

    property OnFormShow: TNotifyEvent
      read FOnFormShow
      write FOnFormShow;

    property OnFolderChanged: TNotifyEvent
      read FOnFolderChanged
      write FOnFolderChanged;

    property OnSelectionChanged: TNotifyEvent
      read FOnSelectionChanged
      write FOnSelectionChanged;

    property OnTypeChanged: TNotifyEvent
      read FOnTypeChanged
      write FOnTypeChanged;
  end;


implementation

{$R *.dfm}

uses
  SysUtils,
  Dialogs,
  TypInfo,
  Registry,
  ShlObj,
  RzShellConsts,
  RzShellIntf;

const
  SIZEGRIP_SIZE = 13;


{The list of filter strings is thus:
  [Visible][TStringList of extensions:[]]
  ---------------------------------------
  [Item1 ( *.* )][ [*.*] ]
  [Item2 ( *.doc )][ [*.doc] ]
  [Item3 ( *.gif, *.jpg, *.bmp )][ [*.gif][*.jpg][*.bmp] ]
}

type
  TFilterItemRec = record
    FExtension: string;
  end;
  PFilterItemRec = ^TFilterItemRec;


function NewFilterItemRec: PFilterItemRec;
begin
  New( Result );
end;


procedure DisposeFilterItemRec( pfir: PFilterItemRec );
begin
  Dispose( pfir );
end;


procedure GetCharsUpToNextCharDB( var Pos: Integer; Source: string; var Dest: string; CharToFind: Char );
begin
  Dest := '';
  while ( Source[ Pos ] <> CharToFind ) and ( Pos <= Length( Source ) ) do
    CopyCharDB( Pos, Source, Dest );
end;


// Takes a filter in the form "FileType1|*.ext11;*.ext12;*.ext1n|FileType2|*.ext21|" etc.
// and fills astrings.strings[] with the FileType part and the .Objects[] part with a TFilterItemRec.
// The TFilterItemRec comprises a TStringList itSelf which is a list of all the extensions
// eg. [*.ext11][*.ext12][*.ext1n]. The ExtensionsToTStrings method takes a semi-colon delimited list of
// extensions and adds them to a TStrings.

procedure FilterToTStrings( Filter: string; Strings: TStrings );
var
  pos: Integer;
  tmp: string;
  displayName: string;
  extensions: string;  // All extensions ( *.gif;*.jpg;*.bmp;etc... )
  p: PFilterItemRec;
begin
  pos := 1;
  SetLength( tmp, 255 ); tmp:='';  // Allocate some space now to prevent reallocations
  while ( pos <= Length( Filter ) ) do
  begin
   // Get all chars up to '|' character
    GetCharsUpToNextCharDB( pos, Filter, displayName, '|' ); Inc( pos ); // skip bar
    GetCharsUpToNextCharDB( pos, Filter, extensions, '|' );  Inc( pos ); // skip bar
    p := NewFilterItemRec;
    p.FExtension := extensions;
    Strings.AddObject( displayName, TObject( p ) );
  end;
end;


procedure FilterstringsFree( Strings: TStrings );
var
  I: Integer;
begin
  for I := 0 to Strings.Count-1 do
    DisposeFilterItemRec( Pointer( Strings.Objects[ I ] ) );
end;


function GetFriendlyCaption( CSIDL: TCSIDL ): string; overload;
var
  idList: PItemIdList;
begin
  ShellGetSpecialFolderIdList( 0, CSIDL, idList );
  try
    Result := ShellGetFriendlyNameForLastIdListElement( idList );
    if Result = '' then
      Result := ShellGetFriendlyNameFromIdList( nil, idList, fnNormal );
  finally
    ShellMemFree( idlist );
  end;
end;


function GetFriendlyCaption( const path: string ): string; overload;
var
  idList: PItemIdList;
begin
  ShellGetIdListFromPath( path, idList );
  try
    Result := ShellGetFriendlyNameForLastIdListElement( idList );
    if Result = '' then
      Result := ShellGetFriendlyNameFromIdList( nil, idList, fnNormal );
  finally
    ShellMemFree( idList );
  end;
end;



{========================}
{== TRzPlaceData Class ==}
{========================}

type
  TRzPlaceData = class
    CSIDL: TCSIDL;
    Caption: string;
    Path: string;
    ImageIndex: Integer;
    constructor Create( IDL: TCSIDL ); overload;
    constructor Create( const Directory: string ); overload;
  end;


constructor TRzPlaceData.Create( IDL: TCSIDL );
begin
  inherited Create;
  CSIDL := IDL;
  Caption := GetFriendlyCaption( CSIDL );
  Path := '';
  try
    ImageIndex := ShellGetSpecialFolderIconIndex( CSIDL, 0 );
  except
    ImageIndex := ShellGetSpecialFolderIconIndex( csidlCommonDocuments, 0 );
  end;
end;


constructor TRzPlaceData.Create( const directory: string );
var
  S: string;
begin
  inherited Create;

  // Expand any environment variables
  S := ExcludeTrailingPathDelimiter( ExpandEnvironmentVariables( directory ) );

  CSIDL := csidlNone;
  if Length( S ) <= 3 then
  begin
    // Dealing with a drive
    Caption := GetFriendlyCaption( S );
  end
  else
  begin
    // Dealing with a normal directory
    Caption := ExtractFileName( S );
  end;
  Path := S;
  ImageIndex := ShellGetIconIndexFromPath( S, 0 );
end;



{==================================}
{== TRzShellOpenSaveForm Methods ==}
{==================================}


constructor TRzShellOpenSaveForm.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );

  FFiles := TStringList.Create;
  FSelections := TStringList.Create;
  ShellList.OnDblClickOpen := ListDblClickOpen;
  OnHelp := FormHelp;
end;


procedure TRzShellOpenSaveForm.FormCreate(Sender: TObject);
begin
  PopupMode := pmAuto;

  pnlJumps.ParentBackground := False;
  pnlJumps.DoubleBuffered := True;
  pnlWork.ParentBackground := False;
  pnlWork.DoubleBuffered := True;
  
  imlPlaces.Handle := ShellGetSystemImageList( iszLarge );
  imlPlaces.ShareImages := True;
  // If ShareImages is done before assigning the handle, then the existing image
  // list is also shared and not correctly freed when overwritten by the
  // assigning of the system image list handle.

  FPlacesList := TObjectList.Create;
  InitCustomPlaces;
end;


procedure TRzShellOpenSaveForm.InitCustomPlaces;
const
  cRegBasePlaces = 'Software\Microsoft\Windows\CurrentVersion\Policies\Comdlg32\Placesbar';
  cPlace = 'Place';
  DefaultCSIDLs: array[ 0..4 ] of TCSIDL = ( csidlRecent, csidlDesktop, csidlPersonal, csidlDrives, csidlNetwork );
var
  I, PlacesFound, IDL: Integer;
  CSIDL: TCSIDL;
  RegDataInfo: TRegDataInfo;
  Reg: TRegistry;
  PlaceData: TRzPlaceData;
  Place: string;
begin
  Reg := TRegistry.Create;
  try
    PlacesFound := 0;
    if Reg.OpenKeyReadOnly( cRegBasePlaces ) then
    begin
      // Go through all the places. Their key data will be:
      // REG_DWORD: A CSIDL value that identifies a folder.
      // REG_SZ or REG_EXPAND_SZ: A null-terminated string that specifies a valid path.

      for I := 0 to 4 do
      begin
        if Reg.ValueExists( cPlace + IntToStr( I ) ) and
           Reg.GetDataInfo( cPlace + IntToStr( I ), RegDataInfo ) then
        begin
          Inc( PlacesFound );
          if RegDataInfo.RegData in [ rdString, rdExpandString ] then
          begin
            // The folder is a valid path, so add a string
            Place := Reg.ReadString( cPlace + IntToStr( I ) );
            if Trim( Place ) <> '' then
            begin
              PlaceData := TRzPlaceData.Create( Place );
              FPlacesList.Add( PlaceData );
            end
            else
            begin
              PlaceData := TRzPlaceData.Create( DefaultCSIDLs[ I ] );
              FPlacesList.Add( PlaceData );
            end;
          end
          else // It is a CSIDL to id the folder, so add a idl
          begin
            IDL := Reg.ReadInteger( cPlace + IntToStr( I ) );
            if ( IDL >= $0000 ) and ( IDL <= $003d ) then
            begin
              CSIDL := TCSIDL( IDL );
              if CSIDL in [ csidl_None1, csidl_None2, csidl_None3, csidl_None4, csidl_None5, csidl_None6 ] then
                CSIDL := DefaultCSIDLs[ I ];
            end
            else
              CSIDL := DefaultCSIDLs[ I ];

            PlaceData := TRzPlaceData.Create( CSIDL );
            FPlacesList.Add( PlaceData );
          end;
        end;
      end;
    end
    else
    begin
      for I := 0 to 4 do
      begin
        PlaceData := TRzPlaceData.Create( DefaultCSIDLs[ I ] );
        FPlacesList.Add( PlaceData );
      end;
      PlacesFound := 5;
    end;

    if PlacesFound > 0 then
    begin
      btnPlace0.Caption := TRzPlaceData( FPlacesList[ 0 ] ).Caption;
      btnPlace0.ImageIndex := TRzPlaceData( FPlacesList[ 0 ] ).ImageIndex;
    end
    else
      btnPlace0.Visible := False;;

    if PlacesFound > 1 then
    begin
      btnPlace1.Caption := TRzPlaceData( FPlacesList[ 1 ] ).Caption;
      btnPlace1.ImageIndex := TRzPlaceData( FPlacesList[ 1 ] ).ImageIndex;
    end
    else
      btnPlace1.Visible := False;;

    if PlacesFound > 2 then
    begin
      btnPlace2.Caption := TRzPlaceData( FPlacesList[ 2 ] ).Caption;
      btnPlace2.ImageIndex := TRzPlaceData( FPlacesList[ 2 ] ).ImageIndex;
    end
    else
      btnPlace2.Visible := False;;

    if PlacesFound > 3 then
    begin
      btnPlace3.Caption := TRzPlaceData( FPlacesList[ 3 ] ).Caption;
      btnPlace3.ImageIndex := TRzPlaceData( FPlacesList[ 3 ] ).ImageIndex;
    end
    else
      btnPlace3.Visible := False;

    if PlacesFound > 4 then
    begin
      btnPlace4.Caption := TRzPlaceData( FPlacesList[ 4 ] ).Caption;
      btnPlace4.ImageIndex := TRzPlaceData( FPlacesList[ 4 ] ).ImageIndex;
    end
    else
      btnPlace4.Visible := False;

  finally
    Reg.Free;
  end;

end;


procedure TRzShellOpenSaveForm.DoTranslation;

  function IMax( a, b: Integer ): Integer;
  begin
    if ( a>b ) then
      Result := a
    else
      Result := b;
  end;

var
  x: Integer;
begin
  btnCancel.Caption := SCancelButton;
  btnHelp.Caption := SHelpButton;
  btnUpOneLevel.Hint := SUpOneLevelHint;
  btnCreateNewFolder.Hint := SCreateNewFolderHint;

  btnList.Hint := SViewListHint + '|' + SViewListContext;
  btnDetails.Hint := SViewDetailsHint + '|' + SViewDetailsContext;

  chkReadOnly.Caption := SOpenAsReadOnly;
  lblFileName.Caption := SFileName;
  btnShowTree.Hint := SShowTreeHint;

  {It is the responsibility of the caller to translate LookInTxt, FilesOfTypesTxt, OpenBtn and the
   form's title itSelf.}
  View1Mitm.Caption := SViewMenu;
  View1Mitm.Hint := SViewContext;

  LargeIcons1Mitm.Caption := SViewLargeIconsMenu;
  LargeIcons1Mitm.Hint := SViewLargeIconsContext;

  SmallIcons1Mitm.Caption := SViewSmallIconsMenu;
  SmallIcons1Mitm.Hint := SViewSmallIconsContext;

  List1Mitm.Caption := SViewListMenu;
  List1Mitm.Hint := SViewListContext;

  Details1Mitm.Caption := SViewDetailsMenu;
  Details1Mitm.Hint := SViewDetailsContext;

  Paste1Mitm.Caption := SEditPasteMenu;
  Paste1Mitm.Hint := SPasteContext;

  New1Mitm.Caption := SNewMenu;
  New1Mitm.Hint := SNewPopupContext;

  Folder1Mitm.Caption := SNewFolderMenu;
  Folder1Mitm.Hint := SCreateFolderContext;

  Properties1Mitm.Caption := SPropertiesMenu;
  Properties1Mitm.Hint := SPropertiesContext;

   // Adjust controls for different language text lengths
  x := IMax( lblFilesOfType.BoundsRect.Right, lblFileName.BoundsRect.Right ) + 8;

  with cbxFileTypes.BoundsRect do
    cbxFileTypes.BoundsRect := Rect( x, Top, Right, Bottom );
  with edtFileName.BoundsRect do
    edtFileName.BoundsRect := Rect( x, Top, Right, Bottom );
  with chkReadOnly.BoundsRect do
    chkReadOnly.BoundsRect := Rect( x, Top, Right, Bottom );

end; {= TRzShellOpenSaveForm.DoTranslation =}


destructor TRzShellOpenSaveForm.Destroy;
begin
  FPlacesList.Free;
  FSelections.Free;
  FFiles.Free;
  inherited;
end;


procedure TRzShellOpenSaveForm.CreateWnd;
begin
  inherited;

  if UsingSystemStyle then
  begin
    // Create the size-grip in the bottom right corner of the window.
    FHGripWindow := CreateWindowEx( WS_EX_LEFT or WS_EX_LTRREADING or WS_EX_RIGHTSCROLLBAR, 'SCROLLBAR', '',
                                    WS_CHILDWINDOW or WS_VISIBLE or WS_CLIPSIBLINGS or WS_CLIPCHILDREN or SBS_SIZEGRIP,
                                    ClientRect.Right-SIZEGRIP_SIZE, ClientRect.Bottom-SIZEGRIP_SIZE,
                                    SIZEGRIP_SIZE, SIZEGRIP_SIZE, Handle, 0,HInstance, nil );

    SetWindowPos( FHGripWindow, HWND_TOP, 0,0,0,0, SWP_NOSIZE or SWP_NOMOVE );
  end;
end;


procedure TRzShellOpenSaveForm.InitFraming( FrameColor: TColor; FrameStyle: TFrameStyle;
                                            FrameVisible: Boolean;
                                            FramingPreference: TFramingPreference );
begin
  ShellCombo.FrameColor := FrameColor;
  ShellCombo.FrameStyle := FrameStyle;
  ShellCombo.FrameVisible := FrameVisible;
  ShellCombo.FlatButtons := FrameVisible;
  ShellCombo.FramingPreference := FramingPreference;

  ShellTree.FrameColor := FrameColor;
  ShellTree.FrameStyle := FrameStyle;
  ShellTree.FrameVisible := FrameVisible;
  ShellTree.FramingPreference := FramingPreference;

  ShellList.FrameColor := FrameColor;
  ShellList.FrameStyle := FrameStyle;
  ShellList.FrameVisible := FrameVisible;
  ShellList.FramingPreference := FramingPreference;

  cbxFileName.FrameColor := FrameColor;
  cbxFileName.FrameStyle := FrameStyle;
  cbxFileName.FrameVisible := FrameVisible;
  cbxFileName.FlatButtons := FrameVisible;
  cbxFileName.FramingPreference := FramingPreference;

  edtFileName.FrameColor := FrameColor;
  edtFileName.FrameStyle := FrameStyle;
  edtFileName.FrameVisible := FrameVisible;
  edtFileName.FramingPreference := FramingPreference;

  cbxFileTypes.FrameColor := FrameColor;
  cbxFileTypes.FrameStyle := FrameStyle;
  cbxFileTypes.FrameVisible := FrameVisible;
  cbxFileTypes.FlatButtons := FrameVisible;
  cbxFileTypes.FramingPreference := FramingPreference;
end;


procedure TRzShellOpenSaveForm.InitHotTracking( ButtonColor: TColor;
                                                HotTrack: Boolean; HighlightColor: TColor;
                                                HotTrackColor: TColor;
                                                HotTrackColorType: TRzHotTrackColorType );
begin
  chkReadOnly.HotTrack := HotTrack;
  chkReadOnly.HighlightColor := HighlightColor;
  chkReadOnly.HotTrackColor := HotTrackColor;
  chkReadOnly.HotTrackColorType := HotTrackColorType;

  btnOpen.Color := ButtonColor;
  btnOpen.HotTrack := HotTrack;
  btnOpen.HighlightColor := HighlightColor;
  btnOpen.HotTrackColor := HotTrackColor;
  btnOpen.HotTrackColorType := HotTrackColorType;

  btnCancel.Color := ButtonColor;
  btnCancel.HotTrack := HotTrack;
  btnCancel.HighlightColor := HighlightColor;
  btnCancel.HotTrackColor := HotTrackColor;
  btnCancel.HotTrackColorType := HotTrackColorType;

  btnHelp.Color := ButtonColor;
  btnHelp.HotTrack := HotTrack;
  btnHelp.HighlightColor := HighlightColor;
  btnHelp.HotTrackColor := HotTrackColor;
  btnHelp.HotTrackColorType := HotTrackColorType;
end;


procedure TRzShellOpenSaveForm.InitShowButtonGlyphs( ShowButtonGlyphs: Boolean );
begin
  if ShowButtonGlyphs then
  begin
    btnOpen.Glyph.Handle := LoadBitmap( HInstance, 'RZCOMMON_OK' );
    btnCancel.Glyph.Handle := LoadBitmap( HInstance, 'RZCOMMON_CANCEL' );
    btnHelp.Glyph.Handle := LoadBitmap( HInstance, 'RZCOMMON_HELP' );

    // Adjust widths of buttons and edits/combos
    btnOpen.Width := btnOpen.Width + 20;
    btnCancel.Width := btnCancel.Width + 20;
    btnHelp.Width := btnHelp.Width + 20;
  end;
end;


procedure TRzShellOpenSaveForm.InitToolButtons( ToolBtnGradientColorStyle: TRzGradientColorStyle;
                                                ToolBtnSelectionColorStart,
                                                ToolBtnSelectionColorStop,
                                                ToolBtnSelectionFrameColor: TColor;
                                                ToolBtnVisualStyle: TRzVisualStyle );
begin
  btnPlace0.GradientColorStyle := ToolBtnGradientColorStyle;
  btnPlace0.SelectionColorStart := ToolBtnSelectionColorStart;
  btnPlace0.SelectionColorStop := ToolBtnSelectionColorStop;
  btnPlace0.SelectionFrameColor := ToolBtnSelectionFrameColor;
  btnPlace0.VisualStyle := ToolBtnVisualStyle;

  btnPlace1.GradientColorStyle := ToolBtnGradientColorStyle;
  btnPlace1.SelectionColorStart := ToolBtnSelectionColorStart;
  btnPlace1.SelectionColorStop := ToolBtnSelectionColorStop;
  btnPlace1.SelectionFrameColor := ToolBtnSelectionFrameColor;
  btnPlace1.VisualStyle := ToolBtnVisualStyle;

  btnPlace2.GradientColorStyle := ToolBtnGradientColorStyle;
  btnPlace2.SelectionColorStart := ToolBtnSelectionColorStart;
  btnPlace2.SelectionColorStop := ToolBtnSelectionColorStop;
  btnPlace2.SelectionFrameColor := ToolBtnSelectionFrameColor;
  btnPlace2.VisualStyle := ToolBtnVisualStyle;

  btnPlace3.GradientColorStyle := ToolBtnGradientColorStyle;
  btnPlace3.SelectionColorStart := ToolBtnSelectionColorStart;
  btnPlace3.SelectionColorStop := ToolBtnSelectionColorStop;
  btnPlace3.SelectionFrameColor := ToolBtnSelectionFrameColor;
  btnPlace3.VisualStyle := ToolBtnVisualStyle;

  btnPlace4.GradientColorStyle := ToolBtnGradientColorStyle;
  btnPlace4.SelectionColorStart := ToolBtnSelectionColorStart;
  btnPlace4.SelectionColorStop := ToolBtnSelectionColorStop;
  btnPlace4.SelectionFrameColor := ToolBtnSelectionFrameColor;
  btnPlace4.VisualStyle := ToolBtnVisualStyle;

  btnUpOneLevel.GradientColorStyle := ToolBtnGradientColorStyle;
  btnUpOneLevel.SelectionColorStart := ToolBtnSelectionColorStart;
  btnUpOneLevel.SelectionColorStop := ToolBtnSelectionColorStop;
  btnUpOneLevel.SelectionFrameColor := ToolBtnSelectionFrameColor;
  btnUpOneLevel.VisualStyle := ToolBtnVisualStyle;

  btnShowDesktop.GradientColorStyle := ToolBtnGradientColorStyle;
  btnShowDesktop.SelectionColorStart := ToolBtnSelectionColorStart;
  btnShowDesktop.SelectionColorStop := ToolBtnSelectionColorStop;
  btnShowDesktop.SelectionFrameColor := ToolBtnSelectionFrameColor;
  btnShowDesktop.VisualStyle := ToolBtnVisualStyle;

  btnCreateNewFolder.GradientColorStyle := ToolBtnGradientColorStyle;
  btnCreateNewFolder.SelectionColorStart := ToolBtnSelectionColorStart;
  btnCreateNewFolder.SelectionColorStop := ToolBtnSelectionColorStop;
  btnCreateNewFolder.SelectionFrameColor := ToolBtnSelectionFrameColor;
  btnCreateNewFolder.VisualStyle := ToolBtnVisualStyle;

  btnShowTree.GradientColorStyle := ToolBtnGradientColorStyle;
  btnShowTree.SelectionColorStart := ToolBtnSelectionColorStart;
  btnShowTree.SelectionColorStop := ToolBtnSelectionColorStop;
  btnShowTree.SelectionFrameColor := ToolBtnSelectionFrameColor;
  btnShowTree.VisualStyle := ToolBtnVisualStyle;

  btnList.GradientColorStyle := ToolBtnGradientColorStyle;
  btnList.SelectionColorStart := ToolBtnSelectionColorStart;
  btnList.SelectionColorStop := ToolBtnSelectionColorStop;
  btnList.SelectionFrameColor := ToolBtnSelectionFrameColor;
  btnList.VisualStyle := ToolBtnVisualStyle;

  btnDetails.GradientColorStyle := ToolBtnGradientColorStyle;
  btnDetails.SelectionColorStart := ToolBtnSelectionColorStart;
  btnDetails.SelectionColorStop := ToolBtnSelectionColorStop;
  btnDetails.SelectionFrameColor := ToolBtnSelectionFrameColor;
  btnDetails.VisualStyle := ToolBtnVisualStyle;

end;



//  Respond to view menu or view button click. Update state of menu and buttons so they remain synchronised.

procedure TRzShellOpenSaveForm.ViewBtnClick( Sender: TObject );
var
  Tag: Integer;

  procedure CheckItems( a: array of TComponent; prop: string );
  var i: Integer;
  begin
    for i := Low( a ) to High( a ) do
      SetOrdProp( a[i], GetPropInfo( a[i].ClassInfo, prop ), Integer( a[i].Tag=tag ) );
  end;

begin
  tag := ( sender as TComponent ).Tag;
  ShellList.ViewStyle := TViewStyle( tag );
  CheckItems( [btnList, btnDetails], 'Down' );
  CheckItems( [LargeIcons1Mitm, SmallIcons1Mitm, List1Mitm, Details1Mitm], 'Checked' );
end;


procedure TRzShellOpenSaveForm.ListDblClickOpen( Sender: TObject; var Handled: Boolean );
begin
  ModalResult := mrOk;
  Handled := True;
end;


procedure TRzShellOpenSaveForm.WMGetMinMaxInfo( var Msg: TWMGetMinMaxInfo );
begin
  Msg.minMaxInfo.ptMinTrackSize := Point( 463, 274 );
end;


procedure TRzShellOpenSaveForm.DoOnFormClose;
begin
  if Assigned( FOnFormClose ) then
    FOnFormClose( Self );
end;


procedure TRzShellOpenSaveForm.DoOnFolderChanged;
begin
  if Assigned( FOnFolderChanged ) then
    FOnFolderChanged( Self );
end;


procedure TRzShellOpenSaveForm.DoOnSelectionChanged;
begin
  if Assigned( FOnSelectionChanged ) then
    FOnSelectionChanged( Self );
end;


procedure TRzShellOpenSaveForm.DoOnFormShow;
begin
  // Force a resize in case form was maximized
  FormResize( nil );
  if Assigned( FOnFormShow ) then
    FOnFormShow( Self );
end;


procedure TRzShellOpenSaveForm.DoOnTypeChanged;
begin
  if Assigned( FOnTypeChanged ) then
    FOnTypeChanged( Self );
end;


function TRzShellOpenSaveForm.GetFilename: string;
begin
  if Executing then
  begin
    GetSelectedFiles( FFiles );
    if FFiles.Count>0 then
      Result := FFiles[0]
    else
      Result := '';
  end
  else
    Result := edtFileName.Text;
end;


function TRzShellOpenSaveForm.GetFiles: TStrings;
begin
  GetSelectedFiles( FFiles );
  Result := FFiles;
end;


function TRzShellOpenSaveForm.GetFilterIndex: Integer;
begin
  if cbxFileTypes.Items.Count>0 then
    Result := cbxFileTypes.ItemIndex + 1
  else
    Result := 0;
end;


function TRzShellOpenSaveForm.GetFormSplitterPos: Integer;
begin
  Result := RzSplitter1.Position;
end;


function TRzShellOpenSaveForm.GetOnAddListItem: TRzShAddItemEvent;
begin
  Result := ShellList.OnAddItem;
end;


function TRzShellOpenSaveForm.GetOnAddTreeItem: TRzShAddItemEvent;
begin
  Result := ShellTree.OnAddItem;
end;


function TRzShellOpenSaveForm.GetOnAddComboItem: TRzShAddItemEvent;
begin
  {Result := ShellCombo.OnAddItem;}
end;


procedure TRzShellOpenSaveForm.SetFilename( const Value: string );
begin
  edtFileName.Text := Value;
end;


procedure TRzShellOpenSaveForm.SetFilter( const Value: string );
begin
  FFilter := Value;
  ShellList.FileFilter := Filter;
  FilterToTStrings( FFilter, cbxFileTypes.Items );
end;


procedure TRzShellOpenSaveForm.SetFilterIndex( Value: Integer );
begin
  if ( Value>=1 ) and ( Value <= cbxFileTypes.Items.Count ) then
  begin
    cbxFileTypes.ItemIndex := Value-1;
    cbxFileTypesSelEndOk( Self );
  end
  else if cbxFileTypes.Items.Count>0 then
    cbxFileTypes.ItemIndex := 0;
end;


procedure TRzShellOpenSaveForm.SetFormSplitterPos( Value: Integer );
begin
  RzSplitter1.Position := Value;
end;


procedure TRzShellOpenSaveForm.SetInitialDir( const Value: string );
begin
  FInitialDir := Value;
  ShellList.Folder.Pathname := Value;
end;


procedure TRzShellOpenSaveForm.SetOptions( Value: TRzOpenSaveOptions );
var
  TreeOptions: TRzShellTreeOptions;
  ListOptions: TRzShellListOptions;

  procedure ApplyListOption( Apply: Boolean; ListOpt: TRzShellListOption );
  begin
    if Apply then
      Include( ListOptions, ListOpt )
    else
      Exclude( ListOptions, ListOpt );
  end;

  procedure ApplyTreeOption( Apply: Boolean; TreeOpt: TRzShellTreeOption );
  begin
    if Apply then
      Include( TreeOptions, TreeOpt )
    else
      Exclude( TreeOptions, TreeOpt );
  end;

  procedure ApplyOptions( Apply: Boolean; TreeOpt: TRzShellTreeOption; ListOpt: TRzShellListOption );
  begin
    ApplyListOption( Apply, ListOpt );
    ApplyTreeOption( Apply, TreeOpt );
  end;

begin {= TRzShellOpenSaveForm.SetOptions =}
  FOptions := Value;

  TreeOptions := ShellTree.Options;
  ListOptions := ShellList.Options;
  ApplyOptions( osoOleDrag in Value,  stoOleDrag, sloOleDrag );
  ApplyOptions( osoOleDrop in Value,  stoOleDrop, sloOleDrop );
  ApplyOptions( osoShowHidden in Value, stoShowHidden, sloShowHidden );
  ApplyListOption( osoHideFoldersInListWhenTreeVisible in Value, sloHideFoldersWhenLinkedToTree );
  ApplyListOption( osoFilesCanBeFolders in Value, sloFilesCanBeFolders );
  ShellList.MultiSelect := ( osoAllowMultiselect in Value );
  ShellTree.Options := treeOptions;
  ShellList.Options := listOptions;

  chkReadOnly.Visible := not ( osoHideReadOnly in Value );
  btnHelp.Visible := ( osoShowHelp in Value );

  ShowHint := ( osoShowHints in Value );

end; {= TRzShellOpenSaveForm.SetOptions =}


procedure TRzShellOpenSaveForm.SetOnAddListItem( Value: TRzShAddItemEvent );
begin
  ShellList.OnAddItem := Value;
end;


procedure TRzShellOpenSaveForm.SetOnAddTreeItem( Value: TRzShAddItemEvent );
begin
  ShellTree.OnAddItem := Value;
end;


procedure TRzShellOpenSaveForm.SetOnAddComboItem( Value: TRzShAddItemEvent );
begin
  {ShellCombo.OnAddItem := Value;}
end;


procedure TRzShellOpenSaveForm.ShowTree( Show: Boolean );
var
  c: TCursor;
begin
  if Show then
  begin
    if Assigned( ShellCombo.ShellTree ) and RzSplitter1.UpperLeft.Visible  then
      Exit; // Already showing

    try
      btnShowTree.Down := True;
      if not Assigned( ShellCombo.ShellTree ) then
      begin
        c := Screen.Cursor;
        Screen.Cursor := crHourglass;
        try
          ShellTree.SelectedFolder := ShellCombo.SelectedFolder;
            // Assign selected folder before linking the list and combo to prevent redundant update

          ShellCombo.ShellList := nil;
          ShellCombo.ShellTree := ShellTree;
          ShellTree.ShellList := ShellList;

          RzSplitter1.UpperLeft.Visible := True;
          if FormSplitterPos < 0 then
            RzSplitter1.Position := 200
          else
            RzSplitter1.Position := FormSplitterPos;
        finally
          Screen.Cursor := c;
        end;
      end
      else
      begin
        //  Support for ptsloHideFoldersWhenLinkedToTree option
        ShellTree.ShellList := ShellList;
        ShellCombo.ShellTree := ShellTree;
        if FormSplitterPos<0 then
          RzSplitter1.Position := 200
        else
          RzSplitter1.Position := FormSplitterPos;
      end;
    except
      btnShowTree.Down := False;
      raise;
    end;
    Options := Options + [osoShowTree];
    ShellTree.TabStop := True;
  end
  else // not aShow
  begin
    btnShowTree.Down := False;
    if ShellTree.Focused then
      ShellList.SetFocus;
    ShellTree.TabStop := False;
    {-- Support for ptsloHideFoldersWhenLinkedToTree option}
    ShellTree.ShellList := nil;
    ShellCombo.ShellTree := nil;
    ShellCombo.ShellList := ShellList;
    FormSplitterPos := RzSplitter1.Position;
    RzSplitter1.UpperLeft.Visible := False;
    Options := Options - [osoShowTree];
  end;
  if ( ShellList.Visible ) and ( sloHideFoldersWhenLinkedToTree in ShellList.Options ) then
    ShellList.FillItems;
end; {= TRzShellOpenSaveForm.ShowTree =}


procedure TRzShellOpenSaveForm.ApplyUserFilter( Filter: string );
begin
  FUserFilter := Filter;
  ShellList.FileFilter := Filter;
  ShellList.FillItems;
end;


procedure TRzShellOpenSaveForm.GetSelectedFiles( s: TStrings );
begin
  s.Assign( FSelections );
end;


procedure TRzShellOpenSaveForm.ShellListChange( Sender: TObject; Item: TListItem; Change: TItemChange );

  procedure AddFilename( var sofar: string;  const toadd: string );
  begin
    if Length( sofar )>0 then sofar := sofar + ' ';
    sofar := sofar + '"' + toadd + '"';
  end;

var
  ld: TRzShellListData;
  vsi: TList; // Valid selected items
  i: Integer;
  tmpitem: TListItem;
  tmps: string;
begin
  if ( Change <> ctState ) or ( not Executing ) then
    Exit; // Only interested in selection changes

  vsi := TList.Create;
  try
    if ShellList.SelCount > 1 then
    begin
      for i := ShellList.Selected.Index to ShellList.Items.Count-1 do
      begin
        tmpitem := ShellList.Items[i];
        if tmpitem.Selected and Assigned( tmpitem.Data ) then
        begin
          ld := ShellList.ShellListData[i];
          if not ld.IsFolder then
            vsi.Add( ld );
        end;
      end;
    end
    else if ( ShellList.SelCount = 1 ) then
    begin
      tmpitem := ShellList.Selected;
      if Assigned( tmpitem ) and Assigned( tmpitem.Data ) then
      begin
        begin
          ld := ShellList.GetDataFromItem( ShellList.Selected );
          if not ld.IsFolder then
            vsi.Add( ld );
        end;
      end;
    end;

    if vsi.Count>1 then
    begin
      tmps := '';
      for i := 0 to vsi.Count-1 do
        AddFilename( tmps, TRzShellListData( vsi[i] ).FileName );
      edtFileName.Text := tmps;
    end
    else if vsi.Count=1 then
      edtFileName.Text := TRzShellListData( vsi[0] ).DisplayName;

    FLastInputState := lisList;
  finally
    vsi.Free;
  end;

  DoOnSelectionChanged;
end; {= TRzShellOpenSaveForm.ShellListChange =}


procedure TRzShellOpenSaveForm.btnUpOneLevelClick( Sender: TObject );
begin
  ShellList.GoUp( 1 );
end;


procedure TRzShellOpenSaveForm.btnShowTreeClick( Sender: TObject );
begin
  ShowTree( btnShowTree.Down )
end;


procedure TRzShellOpenSaveForm.FormDestroy( Sender: TObject );
begin
  FilterstringsFree( cbxFileTypes.Items );
end;


procedure TRzShellOpenSaveForm.FormKeyDown( Sender: TObject; var Key: Word; Shift: TShiftState );
begin
  case key of
    VK_F4:
      if Shift=[] then
      begin
        if ShellCombo.DroppedDown then
        begin
          ShellCombo.DroppedDown := False;
          ShellList.SetFocus;
          ShellCombo.Perform( CN_COMMAND, MakeLong( 0,CBN_SELENDOK ), ShellCombo.Handle );
        end
        else
        begin
          ShellCombo.SetFocus;
          ShellCombo.DroppedDown := True;
        end;
      end;

    VK_F5:
      if Shift=[] then
      begin
        if not ShellCombo.Focused then ShellCombo.FillItems;
        if not ShellList.Focused then ShellList.FillItems;
        if Assigned( ShellTree.ShellList ) and not ( ShellTree.Focused ) then
          ShellTree.RefreshNodes;
      end;

    VK_F12:
      if Shift=[] then
      begin
        if ( osoAllowTree in Options ) then
          ShowTree( not btnShowTree.Down );
      end;
  end;
end;


{Do processing in WideChars for easy DBCS support. To do this sort of processing in native DBCS is a real pain - and
 possibly slower than doing the DBCS->UNICODE, UNICODE<-DBCS conversion anyway.

 Given a starting fully qualified path 'aCurrent' and a relative modifier path 'aEntered' returns the
 new fully qualified path. Supports drive-letters and UNC names.}

function ApplyPathname( aCurrent, aEntered: string ): string;
var
  wcCurrent, wcEntered, wcResult: array[0..MAX_PATH] of WideChar;

  function StrLenW( pwc: PWideChar ): Integer;
  begin
    Result := 0;
    while pwc^ <> WideChar( #0 ) do
    begin
      Inc( pwc );
      Inc( Result );
    end;
  end;


  function AllDots( wc: PWideChar ): Bool;
  begin
    while wc^ <> WideChar( #0 ) do
    begin
      if wc^ <> WideChar( '.' ) then
      begin
        Result := False;
        Exit;
      end;
      Inc( wc );  // Add 2
    end;
    Result := True;
  end;


  {Might add a wide char to the string. The caller is responsible for ensuring there is sufficient space.}
  procedure EnsureTrailingSlash( pwc: PWideChar;  len: Integer );
  begin
    if ( len<0 ) then len := StrLenW( pwc );
    Inc( pwc, len-1 );
    if ( pwc^ <> WideChar( '\' ) ) then
    begin
      ( pwc+1 )^ := WideChar( '\' );
      ( pwc+2 )^ := WideChar( #0 );
    end;
  end;


  procedure EnsureNoTrailingSlash( pwc: PWideChar;  len: Integer );
  begin
    if ( len<0 ) then len := StrLenW( pwc );
    Inc( pwc, len-1 );
    if ( pwc^ = WideChar( '\' ) ) then
      pwc^ := WideChar( #0 );
  end;


  {Returns a ptr to the position of the minimum position - the first part of the path that you cannot go back below}
  function GetMinimumSizePtr( pwc: PWideChar;  len: Integer ): PWideChar;
  begin
    if ( len<0 ) then len := StrLenW( pwc );
    if ( len>=3 ) and ( ( pwc+1 )^ = WideChar( ':' ) ) then
      Result := ( pwc+3 )
    else if ( len>2 ) and ( ( pwc+0 )^ = WideChar( '\' ) ) and ( ( pwc+1 )^ = WideChar( '\' ) ) then
    begin
      Inc( pwc, 2 );  // Skip the first two slashes
      while ( pwc^ <> WideChar( #0 ) ) and ( pwc^ <> WideChar( '\' ) ) do // Find next slash
        Inc( pwc );
      if ( pwc^ = WideChar( #0 ) ) then
      begin
        Result:=nil;
        Exit;
      end;  // If end reached here then failed

      Inc( pwc );
      while ( pwc^ <> WideChar( #0 ) ) and ( pwc^ <> WideChar( '\' ) ) do // Find next slash or end
        Inc( pwc );

      Result := ( pwc );
    end
    else
      Result := nil;
  end; {GetMinimumSizePtr - local}


  procedure RemoveRightmostElement( pwc: PWideChar );
  var len: DWORD;
      endc: PWideChar;
      minpos: PWideChar;
  begin
    len := StrLenW( pwc );
    endc := PWideChar( UINT( pwc ) + len*2 -2 );
    minpos := GetMinimumSizePtr( pwc, len );
    if UINT( minpos ) - UINT( pwc ) = len*2 then
    begin
      Exit;
    end;
    while ( UINT( endc ) > UINT( minpos ) ) and ( endc <> pwc ) and ( endc^ <> WideChar( '\' ) ) do
      Dec( endc );
    endc^ := WideChar( #0 );
  end;


  procedure StrAppendW( pdest, ptoappend: PWideChar );
  var len: Integer;
  begin
    len := StrLenW( pdest );
    pdest := PWideChar( Integer( pdest )+len*2 );
    while ptoappend^ <> WideChar( #0 ) do
    begin
      pdest^ := ptoappend^;
      Inc( pdest );
      Inc( ptoappend );
    end;
    pdest^ := WideChar( #0 );
  end;


  procedure GetTokenAndAdvance( var pwc: PWideChar;  ptoken: PWideChar );
  var ptok: PWideChar;
  begin
    ptok := ptoken;
    while ( pwc^ <> WideChar( #0 ) ) and ( pwc^ <> WideChar( '\' ) ) do
    begin
      ptok^ := pwc^;
      Inc( pwc );
      Inc( ptok );
    end;
    if ( pwc^ = WideChar( '\' ) ) then Inc( pwc );
    ptok^ := WideChar( #0 );
  end; {GetTokenAndAdvance - local}


  procedure Merge;
  var token: array[0..MAX_PATH] of WideChar;
      pinentered: PWideChar;
      i, max: Integer;
  begin
    Move( wcCurrent, wcResult, ( Length( aCurrent )+1 )*2 );
    pinentered := @wcEntered[0];

    token[0] := WideChar( #0 );
    GetTokenAndAdvance( pinentered, @token[0] );
    while token[0] <> WideChar( #0 ) do
    begin
      if AllDots( @token[0] ) then
      begin
        max := StrLenW( token )-1;
        for i := 1 to max do
          RemoveRightmostElement( @wcResult[0] );
      end
      else
      begin
        if ( wcResult[0] <> WideChar( #0 ) ) then
          EnsureTrailingSlash( @wcResult[0], -1 );
        StrAppendW( @wcResult[0], @token[0] );
      end;
      token[0] := WideChar( #0 );
      GetTokenAndAdvance( pinentered, @token[0] );
    end;
  end; {Merge - local}


type
  TPathType = ( ptAbsDisk, ptAbsNet, ptRel, ptErr );

  function GetPathTypeW( pwc: PWideChar ): TPathType;
  var
    len: Integer;
  begin
    len := StrLenW( pwc );
    if ( len>=2 ) and ( ( pwc+1 )^ = WideChar( ':' ) ) then
      Result := ptAbsDisk
    else if ( len>=3 ) and ( ( pwc+0 )^ = WideChar( '\' ) ) and ( ( pwc+1 )^ = WideChar( '\' ) ) then
      Result := ptAbsNet
    else if ( pwc^ = WideChar( '\' ) ) then
      Result := ptAbsDisk // Just one slash means 'current drive root directory'
    else if ( len>0 ) then
      Result := ptRel
    else
      Result := ptErr;
  end; {GetPathTypeW - local}


var
  pwc: PWideChar;

begin {= ApplyPathname =}
  if ( Length( aCurrent )>2 ) and ( aCurrent[ Length( aCurrent ) ]='\' ) then
    Delete( aCurrent,Length( aCurrent ),1 );

  stringToWideChar( aCurrent, @wcCurrent[0], Sizeof( wcCurrent ) );
  stringToWideChar( aEntered, @wcEntered[0], Sizeof( wcEntered ) );

  wcResult[0] := WideChar( #0 );

 // Determine if the entered string is an absolute path, if so we can ignore aCurrent
  case GetPathTypeW( wcEntered ) of
    ptAbsDisk:
      begin
        if wcEntered[0] = WideChar( '\' ) then
        begin
          case GetPathTypeW( wcCurrent ) of
            ptAbsDisk:
              begin
                wcResult[0] := wcCurrent[0];
                wcResult[1] := wcCurrent[1];
                wcResult[2] := WideChar( #0 );
              end;
            ptAbsNet:
              begin
                Move( wcCurrent, wcResult, ( Length( aCurrent )+1 )*2 );
                pwc := GetMinimumSizePtr( wcResult, -1 );
                pwc^ := WideChar( #0 );
              end;
            else // can't be ptRel, by definition the current path must be an absolute path
          end;
          StrAppendW( @wcResult[0], @wcEntered[0] );
        end
        else if Length( aEntered )=2 then
        begin
          wcResult[0] := wcEntered[0];
          wcResult[1] := wcEntered[1];
          wcResult[2] := WideChar( '\' );
          wcResult[3] := WideChar( #0 );
        end
        else
          Move( wcEntered, wcResult, ( Length( aEntered )+1 )*2 );
      end;

    ptAbsNet:
      begin
        Move( wcEntered, wcResult, ( Length( aEntered )+1 )*2 );
      end;

    ptRel:
      begin
        Merge;
      end;
    else
  end; {case}

  Result := WideCharTostring( @wcResult[0] );
end; {= ApplyPathname =}


function IsFileReadOnly( aFile: string ): Boolean;
var dw: DWORD;
begin
  dw := Windows.GetFileAttributes( PChar( aFile ) );
  Result := ( ( dw <> $FFFFFFFF ) and ( ( dw and FILE_ATTRIBUTE_READONLY )<>0 ) );
end;


function IsExtensionRegistered( const ext: string ): Boolean;
var r: TRegistry;
begin
  r := TRegistry.Create;
  try
    r.RootKey := HKEY_CLASSES_ROOT;
    Result := r.KeyExists( ext );
  finally
    r.Free;
  end;
end;


function MessageDlgCaption( const Caption, Msg: string; dlgType: TMsgDlgType; buttons: TMsgDlgButtons;
                            helpCtx: Integer ): Integer;
begin
  with Dialogs.CreateMessageDialog( Msg, dlgType, buttons ) do
    try
      Caption := Caption;
      HelpContext := helpctx;
      Result := ShowModal;
    finally
      Free;
    end;
end; {MessageDlgCaption}


procedure NotFound( const caption, filename: string );
begin
  MessageDlgCaption( caption, Format( SFileNotFound, [filename] ), mtWarning, [mbOk], 0 );
end;

function DoYouWishToCreateIt( const caption, filename: string ): Boolean;
begin
  Result := ( MessageDlgCaption( caption, Format( SDoesNotExistCreate, [filename] ), mtConfirmation, [mbYes, mbNo], 0 ) = mrYes );
end;

procedure NoReadOnlyReturn( const caption, filename: string );
begin
  MessageDlgCaption( Caption, Format( SExistsAndIsReadOnly, [filename] ), mtWarning, [mbOk], 0 );
end;

function FileExistsOverwrite( const caption, filename: string ): Boolean;
begin
  Result := ( MessageDlgCaption( caption, Format( SFileExistsReplace, [filename] ), mtWarning, [mbYes, mbNo], 0 ) = mrYes );
end;

procedure ThereCanBeOnlyOne( const caption, filename: string );
begin
  MessageDlgCaption( caption, Format( SThereCanBeOnlyOne, [filename] ), mtWarning, [mbOk], 0 );
end;

procedure ThisFilenameIsNotValid( const caption, filename: string );
begin
  MessageDlgCaption( caption, Format( SFilenameIsInvalid, [filename] ), mtWarning, [mbOk], 0 );
end;


{Returns True if a '*' or '?' char is found - DBCS enabled}
function AnyWildcardsDB( s: string ): Boolean;
var pos: Integer;
begin
  pos := 1;
  while ( pos <= Length( s ) ) do
  begin
    if IsDBCSLeadByte( Byte( s[pos] ) ) then
      Inc( pos,2 )
    else
    begin
      if ( s[pos] = '*' ) or ( s[pos] = '?' ) then
      begin
        Result := True;
        Exit;
      end;
      Inc( pos );
    end;
  end;
  Result := False;
end; {AnyWildcardsDB}


function AnyOfThisCharDB( const ins: string;  thisChar: Char ): Boolean;
var inpos: Integer;
begin
  inpos := 1;
  while ( inpos <= Length( ins ) ) do
  begin
    if IsDBCSLeadByte( Byte( ins[inpos] ) ) then
      Inc( inpos, 2 )
    else if ( ins[inpos] = thisChar ) then
    begin
      Result := True;
      Exit;
    end
    else
      Inc( inpos );
  end;
  Result := False;
end; {AnyOfThisCharDB}


procedure ParametizeDB_special( const ins: string;  outs: TStrings );
{$IFNDEF VCL30PLUS}
  function AnsiPos( const Substr, S: string ): Integer;
  begin
    Result := Pos( Substr, S );
  end;
{$ENDIF}
const WHITESPACE = [' ',#9];
var curs: string;
    state: ( sNormal, sInQuotes, sInWhitespace );
    inpos: Integer;
    curchar: Char;
    fIsDBCS: Boolean;
begin
  curs := '';
  state := sInWhitespace;
  inpos := 1;
  while ( inpos <= Length( ins ) ) do
  begin
    curchar := ins[inpos];
    fIsDBCS := IsDBCSLeadByte( Byte( curchar ) );
    case state of
      sNormal:
        begin
          if not fIsDBCS and ( curchar = '"' ) then
          begin
            curs := TrimRightDB( curs );
            if Length( curs )>0 then
            begin
              outs.Add( curs );
              curs := '';
            end;
            state := sInQuotes;
            Inc( inpos, 1 );
          end
          else
            CopyCharDB( inpos, ins, curs );
        end;

      sInQuotes:
        begin
          if not fIsDBCS and ( curchar = '"' ) then
          begin
            curs := TrimRightDB( curs );
            if Length( curs )>0 then
            begin
              outs.Add( curs );
              curs := '';
            end;
            state := sInWhitespace;
            Inc( inpos );
          end
          else
            CopyCharDB( inpos, ins, curs );
        end;

      sInWhitespace:
        begin
          if not fIsDBCS then
          begin
            if ( curchar = '"' ) then
            begin
              curs := '';
              state := sInQuotes;
            end
            else if not CharInSet( curchar, WHITESPACE ) then
            begin
              curs := curchar;
              state := sNormal;
            end;
            Inc( inpos, 1 );
          end
          else // fIsDBCS
          begin
            CopyCharDB( inpos, ins, curs );
            state := sNormal;
          end;
        end;
    end; {case}
  end; {while}

  curs := TrimRightDB( curs );
  if Length( curs )>0 then
    outs.Add( curs );
end; {ParametizeDB}



// Also input: all the selected items in ShellList
function TRzShellOpenSaveForm.ParseInputstring( const ins: string ): Boolean;
  function ApplyOptions( pathname: string;  options: TRzOpenSaveOptions ): Boolean;
  var fFileExists: Boolean;
  begin
    fFileExists := FileExists( pathname );
    if fFileExists then
    begin
      if IsFileReadOnly( pathname ) and ( osoNoReadOnlyReturn in options ) then
      begin
        NoReadOnlyReturn( Caption, pathname );
        Result := False;
        Exit;
      end;

      if ( osoOverwritePrompt in options ) then
      begin
        if not FileExistsOverwrite( Caption, pathname ) then
          begin Result := False; Exit; end;
      end;

      Result := True;
      Exit;
    end;
   // not FileExists

    if ( osoFileMustExist in options ) then
      begin NotFound( Caption, pathname ); Result := False; Exit; end;

    if ( osoCreatePrompt in options ) then
      if not DoYouWishToCreateIt( Caption, ExtractFileName( pathname ) ) then
        begin Result := False; Exit; end;

    Result := True;
  end; {ApplyOptions - local}

  function GetCurrentFolderPath: string;
  begin
    Result := ShellList.Folder.Pathname;
  end; {GetCurrentFolderPath - local}

  function IfFolderOpenIt( pathname: string ): Boolean;
  var dskishf, ishf: IShellFolder_NRC;
      fFileExists: Boolean;
      pidl: PItemIdList;
      wca: array[0..MAX_PATH] of WideChar;
      dw, dw2, dwAttrib, chEaten: DWORD;
  begin
    Result := False;

    dskishf:=nil; pidl:=nil; ishf:=nil;
    try
      stringToWideChar( pathname, @wca[0], SizeOf( wca ) );
      ShellGetDesktopFolder( dskishf );
      dw := dskishf.ParseDisplayName( Handle, nil, @wca[0], chEaten, pidl, dwAttrib );

      fFileExists := FileExists( pathname );
      if Succeeded( dw ) then
      begin
        dwAttrib := SFGAO_FOLDER;
        dw2 := dskishf.GetAttributesOf( 1, pidl, dwAttrib );
        if Succeeded( dw2 ) and ( not fFileExists ) and ( ( dwAttrib and SFGAO_FOLDER )<>0 ) then
        begin
          dw2 := dskishf.BindToObject( pidl, nil, IID_IShellFolder, Pointer( ishf ) );
          if Failed( dw2 ) then raise Exception.Create( SysErrorMessage( dw2 ) );
          ShellCombo.SelectedFolder.IdList := pidl;
          edtFileName.SelectAll;
          Result := True;
        end;
      end;
    finally
      if Assigned( ishf ) then ishf.Release;
      if Assigned( pidl ) then ShellMemFree( pidl );
      if Assigned( dskishf ) then dskishf.Release;
    end;
  end; {IfFolderOpenIt - local}

  function DereferenceShortcut( pathname: string ): string;
  var ld: TLinkData;
  begin
    if ( AnsiCompareText( ExtractFileExt( pathname ), '.lnk' )=0 ) and
       Succeeded( ResolveShortcut( pathname, ld, False ) ) and
       ( ld.pathname <> '' )
    then
      Result := ld.pathname
    else
      Result := pathname;
  end; {DereferenceShortcut - local}

  procedure HandleDefaultExt( var pathname: string );
    procedure HandleUnregisteredExt;
    var
      ext: string;
    begin
      if cbxFileTypes.ItemIndex <> -1 then
        ext := ExtractFileExt( PFilterItemRec( cbxFileTypes.Items.Objects[cbxFileTypes.ItemIndex] ).FExtension )
      else
        ext := '';
      if ( ext <> '' ) then
      begin
        if AnsiCompareText( ExtractFileExt( pathname ), ext )<>0 then
        begin
          pathname := EnsureTrailingCharDB( pathname, '.' ) + Copy( ext,2,MAXINT );
          if AnsiCompareText( ext, '.'+DefaultExt )<>0 then
            Options := Options + [osoExtensionDifferent]
          else
            Options := Options - [osoExtensionDifferent];
        end;
      end
      else
      begin
        pathname := EnsureTrailingCharDB( pathname, '.' ) + DefaultExt;
        Options := Options - [osoExtensionDifferent];
      end;
    end;
  var
    ext: string;
  begin
    if DefaultExt <> '' then
    begin
      ext := ExtractFileExt( pathname );
      if Length( ext ) > 0 then
      begin
        if IsExtensionRegistered( ext ) then
          Options := Options + [osoExtensionDifferent]
        else
        begin
          HandleUnregisteredExt;
        end;
      end
      else
      begin
        HandleUnregisteredExt;
      end;
    end;
  end; {HandleDefaultExt}

  { Look for invalid chars and simple invalid sequences. }
  function InitialValidityCheck( s: string ): Boolean;
    function AllCharsValid( s: string ): Boolean;
    var i: Integer;
    begin
      i := 1;
      while ( i <= Length( s ) ) do
      begin
        if IsDBCSLeadByte( Byte( s[i] ) ) then
          Inc( i,2 )
        else if CharInSet( s[i], ['/', '|','<','>'] ) then
        begin
          Result := False;
          Exit;
        end
        else
          Inc( i );
      end;
      Result := True;
    end;

    function DoubleBackslashOk( s: string ): Boolean;
    var i: Integer;
    begin
      Result := True;
      i := 3;
      while ( i <= Length( s ) ) do
      begin
        if IsDBCSLeadByte( Byte( s[i] ) ) then
          Inc( i,2 )
        else if ( s[i] = '\' ) and ( s[i-1] = '\' ) then
        begin
          Result := False;
          Break;
        end
        else
          Inc( i );
      end;
    end;
  begin
    Result := AllCharsValid( s ) and DoubleBackslashOk( s );
  end;

var sl: TStrings;
    i, li: Integer;
    curpathname, curname, curpath, curfldpath: string;
    firstFound: TListItem;

begin {ParseInputstring}
  Result := False;
  sl := TStringList.Create;
  FSelections.Clear;
  try
    if AnyOfThisCharDB( ins, '"' ) then
      ParametizeDB_special( ins, sl )
    else
      sl.Add( ins );

    curfldpath := GetCurrentFolderPath;
    EnsureTrailingCharDB( curfldpath, '\' );

    if sl.Count > 0 then
    begin
      for i := 0 to sl.Count-1 do
      begin
        if not ( osoNoValidate in Options ) and not InitialValidityCheck( ins ) then
        begin
          ThisFilenameIsNotValid( Caption, ins );
          Exit;
        end;

        curpathname := ApplyPathname( curfldpath, sl[i] );
        if ( curpathname='' ) then Continue;

        if IfFolderOpenIt( curpathname ) then
          Exit;

        if not ( osoNoDereferenceLinks in Options ) then
          curpathname := DereferenceShortcut( curpathname );

        if ( sl.Count=1 ) then
        begin
          if ( FLastInputState = lisList ) and Assigned( ShellList.SelectedItem ) then
            curpathname := ShellList.SelectedItem.Pathname
          else // FLastInputState = lisEdit
          begin
            HandleDefaultExt( curpathname );

            curname := ExtractFileName( curpathname );
            curpath := ExtractFilePath( curpathname );
            EnsureTrailingCharDB( curpath, '\' );

            if AnsiCompareText( curpath, curfldpath )<>0 then
              ShellCombo.SelectedFolder.Pathname := ExtractFilePath( curpathname );

            firstFound := nil;
            for li := 0 to ShellList.Items.Count-1 do
            begin
              if AnsiCompareText( ShellList.Items[li].Caption, curname )=0 then
              begin
                if Assigned( firstFound ) then
                begin
                  ThereCanBeOnlyOne( Caption, curname );
                  firstFound.Selected := True;
                  firstFound.Focused := True;
                  ShellList.SetFocus;
                  Exit;
                end
                else
                  firstFound := ShellList.Items[li];
              end;
              if Assigned( firstFound ) then
                curpathname := TRzShellListData( firstFound.Data ).Pathname;
            end;
          end;
          Result := ApplyOptions( curpathname, Options );
        end {if sl.Count=1}
        else
          Result := ApplyOptions( curpathname, Options );  // v1.3h
//          Result := ApplyOptions( curpathname, Options + [osoFileMustExist] );  pre v1.3h

        if Result then
          FSelections.Add( curpathname )
        else
          Exit;
      end;
    end;
  finally
    sl.Free;
    if not Result then FSelections.Clear;
  end;
end; {TRzShellOpenSaveForm.ParseInputstring}


procedure TRzShellOpenSaveForm.FormCloseQuery( Sender: TObject; var CanClose: Boolean );
var fname: string;
begin
  if ModalResult = mrOk then
  begin
    fname := ExtractFileName( edtFileName.Text );
    if AnyWildcardsDB( fname ) then
    begin
      CanClose := False;
      ParseInputstring( ExtractFilePath( edtFileName.Text ) );
      ApplyUserFilter( fname );
      edtFileName.Text := fname;
      edtFileName.SelectAll;
    end
    else
    begin
      CanClose := ParseInputstring( edtFileName.Text );
    end;
  end;

  if CanClose and not ( osoNoChangeDir in Options ) and ( ShellList.Folder.PathName <> '' ) then
    try
      SetCurrentDirectory( PChar( ShellList.Folder.PathName ) );
    except
    end;
end;


procedure TRzShellOpenSaveForm.ShellTreeChange( Sender: TObject; Node: TTreeNode );
begin
  (*
  // The following prevents the FileName edit from being initialized because this
  // event is fired when the form is first displayed.
  if Executing then
    if ( node <> nil ) then
      FileNameEdt.Text := '';
  *)
end;


procedure TRzShellOpenSaveForm.cbxFileTypesSelEndOk( Sender: TObject );
begin
  if ( FUserFilter <> '' ) then
    edtFileName.Clear;
  FUserFilter := '';
  ShellList.FileFilter := PFilterItemRec( cbxFileTypes.Items.Objects[cbxFileTypes.ItemIndex] ).FExtension;

  if Executing then
    DoOnTypeChanged;
end;


procedure TRzShellOpenSaveForm.btnCreateNewFolderClick( Sender: TObject );
begin
  if ShellTree.Focused or ( btnShowTree.Down and ( osoHideFoldersInListWhenTreeVisible in Options ) ) then
    ShellTree.CreateNewFolder( True )
  else
    ShellList.CreateNewFolder( True );
end;


procedure TRzShellOpenSaveForm.edtFileNameChange( Sender: TObject );
begin
  if Executing then
    FLastInputState := lisEdit;
end;


procedure TRzShellOpenSaveForm.Paste1MitmClick( Sender: TObject );
begin
  ShellList.DoCommandForFolder( RZSH_CMDS_PASTE );
end;


procedure TRzShellOpenSaveForm.Properties1MitmClick( Sender: TObject );
begin
  ShellList.DoCommandForFolder( RZSH_CMDS_PROPERTIES );
end;


function TRzShellOpenSaveForm.FormHelp( Command: Word; Data: TRzNativeInt; var CallHelp: Boolean ): Boolean;
begin
  if Assigned( OnFormHelp ) then
    Result := OnFormHelp( command, data, callhelp )
  else
    Result := False;
end;


procedure TRzShellOpenSaveForm.btnHelpClick( Sender: TObject );
begin
  //Application.HelpContext( HelpContext ); // FormHelp is still called in this case
  // There is a bug in Delphi 6 and 7 that causes Application.HelpContext fail to generate a wm_Help message.
  // This causes problems with help systems, especially CHM help.  The following is a work-around.
  Application.HelpCommand( HELP_CONTEXT, HelpContext );
end;


procedure TRzShellOpenSaveForm.DoHide;
begin
  FExecuting := False;
  inherited;
  DoOnFormClose;
end;


procedure TRzShellOpenSaveForm.DoShow;

  procedure SetPnlEditsHeight;
  var
    I, Max: Integer;
  begin
    Max := 0;
    for I := 0 to PnlEdits.ControlCount - 1 do
      with PnlEdits.Controls[ i ] do
        if Visible then
          with BoundsRect do
            if bottom > max then
              max := bottom;
    PnlEdits.Height := max + 8;
  end;

var
  ofsx: Integer;
  tmps1: string;
begin {= TRzShellOpenSaveForm.DoShow =}
  Screen.Cursor := crHourglass;
  Cursor := crHourglass;
  inherited;
  Font.Name := SDialogFontName;
  cbxFileTypes.Perform( CB_SETEXTENDEDUI, 1,0 );

 // If no tree button, then hide it and move the other buttons across a bit
  if not ( osoAllowTree in Options ) then
  begin
    btnShowTree.Visible := False;
    ofsx := btnList.Left - btnShowTree.Left;
    btnList.Left := btnList.Left - ofsx;
    btnDetails.Left := btnDetails.Left - ofsx;
  end;

  SetPnlEditsHeight;

  DoTranslation;

  ShowTree( osoShowTree in Options );  // Causes events that cause edit field to be reset.

  tmps1 := ExtractFilePath( Filename );
  if ( InitialDir = '' ) then
    if Length( tmps1 ) <> 0 then
    begin
      ShellCombo.SelectedFolder.Pathname := tmps1;
      tmps1 := ExtractFileName( Filename );
      if ( tmps1 <> '' ) then
        Filename := tmps1;
    end
    else
      ShellCombo.SelectedFolder.Pathname := GetCurrentDir
  else
    ShellCombo.SelectedFolder.Pathname := InitialDir;

  FLastInputState := lisList;

  FExecuting := True;

  DoOnFormShow;

  Cursor := crDefault;
  Screen.Cursor := crDefault;
end; {= TRzShellOpenSaveForm.DoShow =}


procedure TRzShellOpenSaveForm.chkReadOnlyClick( Sender: TObject );
begin
  if chkReadOnly.Checked then
    Include( FOptions, osoReadOnly )
  else
    Exclude( FOptions, osoReadOnly );
end;


procedure TRzShellOpenSaveForm.ShellListFolderChanged( Sender: TObject );
begin
  if Executing then
  begin
    DoOnFolderChanged;
    UpdateCurrentPlace;
  end;
end;


procedure TRzShellOpenSaveForm.FormResize( Sender: TObject );
const
  BUTTON_RIGHT_MARGIN = 16; // was 4 pre v1.h, increased to accomodate size-grip
var
  W, X, Y: Integer;
begin
  inherited;

  Y := ShellCombo.BoundsRect.Bottom + 8;
  RzSplitter1.BoundsRect := Rect( 0, Y, PnlWork.Width - 8, Height - Y - PnlEdits.Height );

  W := btnOpen.Width;

  X := PnlWork.Width - w - BUTTON_RIGHT_MARGIN;

  btnOpen.Left := X;
  btnCancel.Left := X;
  btnHelp.Left := X;

  cbxFileName.Width := X - cbxFileName.Left - 8;
  edtFileName.Width := X - edtFileName.Left - 8;
  cbxFileTypes.Width := X - cbxFileTypes.Left - 8;

  SetWindowPos( FHGripWindow, HWND_TOP, ClientRect.Right-SIZEGRIP_SIZE, ClientRect.Bottom - SIZEGRIP_SIZE, 0, 0,
                SWP_NOSIZE );
end; {= TRzShellOpenSaveForm.FormResize =}


procedure TRzShellOpenSaveForm.btnShowDesktopClick( Sender: TObject );
begin
  ShellList.Folder.CSIDL := csidlDesktop;
end;


procedure TRzShellOpenSaveForm.JumpToPlace( Num: Integer );
begin
  if TRzPlaceData( FPlacesList[ Num ] ).CSIDL <> csidlNone then
  begin
    ShellList.Folder.CSIDL := TRzPlaceData( FPlacesList[ Num ] ).CSIDL;
  end
  else
  begin
    ShellList.Folder.PathName := TRzPlaceData( FPlacesList[ Num ] ).Path;
  end;
end;


procedure TRzShellOpenSaveForm.btnPlaceClick(Sender: TObject);
begin
  JumpToPlace( TRzToolButton( Sender ).Tag );
  UpdateCurrentPlace;
end;


procedure TRzShellOpenSaveForm.UpdateCurrentPlace;
var
  CurrCSIDL: TCSIDL;
  CurrPath: string;
begin
  CurrCSIDL := ShellFindCSIDLFromIdList( ShellList.Folder.IdList );
  CurrPath:= UpperCase( ShellList.Folder.PathName );

  if FPlacesList.Count > 0 then
  begin
    if TRzPlaceData( FPlacesList[ 0 ] ).CSIDL <> csidlNone then
      btnPlace0.Down := TRzPlaceData( FPlacesList[ 0 ] ).CSIDL = CurrCSIDL
    else
      btnPlace0.Down := UpperCase( TRzPlaceData( FPlacesList[ 0 ] ).Path ) = CurrPath;
  end;

  if FPlacesList.Count > 1 then
  begin
    if TRzPlaceData( FPlacesList[ 1 ] ).CSIDL <> csidlNone then
      btnPlace1.Down := TRzPlaceData( FPlacesList[ 1 ] ).CSIDL = CurrCSIDL
    else
      btnPlace1.Down := UpperCase( TRzPlaceData( FPlacesList[ 1 ] ).Path ) = CurrPath;
  end;

  if FPlacesList.Count > 2 then
  begin
    if TRzPlaceData( FPlacesList[ 2 ] ).CSIDL <> csidlNone then
      btnPlace2.Down := TRzPlaceData( FPlacesList[ 2 ] ).CSIDL = CurrCSIDL
    else
      btnPlace2.Down := UpperCase( TRzPlaceData( FPlacesList[ 2 ] ).Path ) = CurrPath;
  end;

  if FPlacesList.Count > 3 then
  begin
    if TRzPlaceData( FPlacesList[ 3 ] ).CSIDL <> csidlNone then
      btnPlace3.Down := TRzPlaceData( FPlacesList[ 3 ] ).CSIDL = CurrCSIDL
    else
      btnPlace3.Down := UpperCase( TRzPlaceData( FPlacesList[ 3 ] ).Path ) = CurrPath;
  end;

  if FPlacesList.Count > 4 then
  begin
    if TRzPlaceData( FPlacesList[ 4 ] ).CSIDL <> csidlNone then
      btnPlace4.Down := TRzPlaceData( FPlacesList[ 4 ] ).CSIDL = CurrCSIDL
    else
      btnPlace4.Down := UpperCase( TRzPlaceData( FPlacesList[ 4 ] ).Path ) = CurrPath;
  end;
end;

end.


