{===============================================================================
  RzShellIntf Unit

  Raize Components - Component Source Unit

  Copyright © 1995-2015 by Raize Software, Inc.  All Rights Reserved.


  Modification History
  ------------------------------------------------------------------------------
  4.3.2  (04 Nov 2007)
    * Updated initialization and finalization of RzShellIntf unit to ensure
      that _CoUninitialize gets called appropriately according to the updated
      MSDN documentation.
  ------------------------------------------------------------------------------
  3.0    (20 Dec 2002)
    * Initial inclusion in Raize Components.
===============================================================================}

{$I RzComps.inc}

{$RANGECHECKS OFF}
{$WRITEABLECONST OFF}
{$TYPEDADDRESS ON}

unit RzShellIntf;

interface

{$HPPEMIT '#define NO_WIN32_LEAN_AND_MEAN' }

uses
  {$IFDEF USE_CS}
  CodeSiteLogging,
  {$ENDIF}
  ActiveX,
  Windows,
  Graphics,
  ShlObj,
  ShellApi;

const
  PTSHELLCONTROLS_VERSION = 190;            // Of form "mmnn" where mm is the major version, nn is the minor version
  PTSHELLCONTROLS_PATCHSYM = Ord( ' ' );
  PTSHELLCONTROLS_PATCH = PTSHELLCONTROLS_PATCHSYM - Ord( 'a' ) + 1;

{$IFDEF BCB}
type
  TRzHandle = Integer;
{$ELSE}
type
  TRzHandle = HWND;
{$ENDIF}


//===========================================================================
//
// Object identifiers in the explorer's name space (ItemID and IDList)
//
// All the items that the user can browse with the explorer (such as files,
// directories, servers, work-groups, etc.) has an identifier which is unique
// among items within the parent folder. Those identifiers are called item
// IDs (SHITEMID). Since all its parent folders have their own item IDs,
// any items can be uniquely identified by a list of item IDs, which is called
// an ID list (ITEMIDLIST).
//
// ID lists are almost always allocated by the task allocator (see some
// description below as well as OLE 2.0 SDK) and may be passed across
// some of shell interfaces (such as IShellFolder). Each item ID in an ID list
// is only meaningful to its parent folder (which has generated it), and all
// the clients must treat it as an opaque binary data except the first two
// bytes, which indicates the size of the item ID.
//
// When a shell extension -- which implements the IShellFolder interace --
// generates an item ID, it may put any information in it, not only the data
// with that it needs to identifies the item, but also some additional
// information, which would help implementing some other functions efficiently.
// For example, the shell's IShellFolder implementation of file system items
// stores the primary (long) name of a file or a directory as the item
// identifier, but it also stores its alternative (short) name, size and date
// etc.
//
// When an ID list is passed to one of shell APIs (such as SHGetPathFromIDList),
// it is always an absolute path -- relative from the root of the name space,
// which is the desktop folder. When an ID list is passed to one of IShellFolder
// member function, it is always a relative path from the folder (unless it
// is explicitly specified).
//
//===========================================================================

type
  {$EXTERNALSYM PPItemIdList}
  PPItemIdList = ^PItemIDList;

{-- Shell OLE GUIDs --------------------------------------------------------------------------------------------------}

{-- Classes --}
const
  {$EXTERNALSYM CLSID_ShellDesktop}
  CLSID_ShellDesktop: TGUID = ( D1: $00021400; D2: $0000; D3: $0000; D4: ( $C0, $00, $00, $00, $00, $00, $00, $46 ) );
  {$EXTERNALSYM CLSID_InternetShortcut}
  CLSID_InternetShortcut: TGUID = ( D1: $FBF23B40; D2: $E3F0; D3: $101B; D4: ( $84, $88, $00, $AA, $00, $3E, $56, $F8 ) );


type
  {$EXTERNALSYM IID_IUnknown}
  IID_IUnknown = IUnknown;
  {$EXTERNALSYM IID_IEnumFormatEtc}
  IID_IEnumFormatEtc = IEnumFormatEtc;
  {$EXTERNALSYM IID_IDropTarget}
  IID_IDropTarget = IDropTarget;
  {$EXTERNALSYM IID_IDropSource}
  IID_IDropSource = IDropSource;
  {$EXTERNALSYM IID_IDataObject}
  IID_IDataObject = IDataObject;

{-- Interfaces --}
const
  {$EXTERNALSYM IID_INewShortcutHookA}
  IID_INewShortcutHookA:  TGUID = ( D1: $000214E1; D2: $0000; D3: $0000; D4: ( $C0, $00, $00, $00, $00, $00, $00, $46 ) );
  {$EXTERNALSYM IID_IShellBrowser}
  IID_IShellBrowser:      TGUID = ( D1: $000214E2; D2: $0000; D3: $0000; D4: ( $C0, $00, $00, $00, $00, $00, $00, $46 ) );
  {$EXTERNALSYM IID_IShellView}
  IID_IShellView:         TGUID = ( D1: $000214E3; D2: $0000; D3: $0000; D4: ( $C0, $00, $00, $00, $00, $00, $00, $46 ) );
  {$EXTERNALSYM IID_IContextMenu}
  IID_IContextMenu:       TGUID = ( D1: $000214E4; D2: $0000; D3: $0000; D4: ( $C0, $00, $00, $00, $00, $00, $00, $46 ) );
  {$EXTERNALSYM IID_IShellIcon}
  IID_IShellIcon:         TGUID = ( D1: $000214E5; D2: $0000; D3: $0000; D4: ( $C0, $00, $00, $00, $00, $00, $00, $46 ) );
  {$EXTERNALSYM IID_IShellFolder}
  IID_IShellFolder:       TGUID = ( D1: $000214E6; D2: $0000; D3: $0000; D4: ( $C0, $00, $00, $00, $00, $00, $00, $46 ) );
  {$EXTERNALSYM IID_IShellExtInit}
  IID_IShellExtInit:      TGUID = ( D1: $000214E8; D2: $0000; D3: $0000; D4: ( $C0, $00, $00, $00, $00, $00, $00, $46 ) );
  {$EXTERNALSYM IID_IShellPropSheetExt}
  IID_IShellPropSheetExt: TGUID = ( D1: $000214E9; D2: $0000; D3: $0000; D4: ( $C0, $00, $00, $00, $00, $00, $00, $46 ) );
  {$EXTERNALSYM IID_IExtractIconA}
  IID_IExtractIconA:      TGUID = ( D1: $000214EB; D2: $0000; D3: $0000; D4: ( $C0, $00, $00, $00, $00, $00, $00, $46 ) );
  {$EXTERNALSYM IID_IExtractIcon}
  IID_IExtractIcon:       TGUID = ( D1: $000214EB; D2: $0000; D3: $0000; D4: ( $C0, $00, $00, $00, $00, $00, $00, $46 ) );
  {$EXTERNALSYM IID_IShellLinkA}
  IID_IShellLinkA:        TGUID = ( D1: $000214EE; D2: $0000; D3: $0000; D4: ( $C0, $00, $00, $00, $00, $00, $00, $46 ) );
  {$EXTERNALSYM IID_IShellLink}
  IID_IShellLink:         TGUID = ( D1: $000214EE; D2: $0000; D3: $0000; D4: ( $C0, $00, $00, $00, $00, $00, $00, $46 ) );
  {$EXTERNALSYM IID_IShellCopyHook}
  IID_IShellCopyHook:     TGUID = ( D1: $000214EF; D2: $0000; D3: $0000; D4: ( $C0, $00, $00, $00, $00, $00, $00, $46 ) );
  {$EXTERNALSYM IID_IFileViewer}
  IID_IFileViewer:        TGUID = ( D1: $000214F0; D2: $0000; D3: $0000; D4: ( $C0, $00, $00, $00, $00, $00, $00, $46 ) );
  {$EXTERNALSYM IID_ICommDlgBrowser}
  IID_ICommDlgBrowser:    TGUID = ( D1: $000214F1; D2: $0000; D3: $0000; D4: ( $C0, $00, $00, $00, $00, $00, $00, $46 ) );
  {$EXTERNALSYM IID_IEnumIDList}
  IID_IEnumIDList:        TGUID = ( D1: $000214F2; D2: $0000; D3: $0000; D4: ( $C0, $00, $00, $00, $00, $00, $00, $46 ) );
  {$EXTERNALSYM IID_IFileViewerSite}
  IID_IFileViewerSite:    TGUID = ( D1: $000214F3; D2: $0000; D3: $0000; D4: ( $C0, $00, $00, $00, $00, $00, $00, $46 ) );
  {$EXTERNALSYM IID_IContextMenu2}
  IID_IContextMenu2:      TGUID = ( D1: $000214F4; D2: $0000; D3: $0000; D4: ( $C0, $00, $00, $00, $00, $00, $00, $46 ) );
  {$EXTERNALSYM IID_IShellDetails}
  IID_IShellDetails:      TGUID = ( D1: $000214EC; D2: $0000; D3: $0000; D4: ( $C0, $00, $00, $00, $00, $00, $00, $46 ) );
  {$EXTERNALSYM IID_IShellFolder2}
  IID_IShellFolder2:      TGUID = ( D1: $93F2F68C; D2: $1D1B; D3: $11D3; D4: ( $A3, $0E, $00, $C0, $4F, $79, $AB, $D1 ) );

type
  IUnknown_NRC = class( TObject )
  public
    function QueryInterface( const iid: TGUID; var obj ): HResult; virtual; stdcall; abstract;
    function AddRef: Integer; virtual; stdcall; abstract;
    function Release: Integer; virtual; stdcall; abstract;
  end;

  IOleWindow_NRC = class( IUnknown_NRC )
  public
    function GetWindow( var wnd: HWnd ): HResult; virtual; stdcall; abstract;
    function ContextSensitiveHelp( fEnterMode: BOOL ): HResult; virtual; stdcall; abstract;
  end;

  IMalloc_NRC = class( IUnknown_NRC )
  public
    function Alloc( cb: Longint ): Pointer; virtual; stdcall; abstract;
    function Realloc( pv: Pointer; cb: Longint ): Pointer; virtual; stdcall; abstract;
    procedure Free( pv: Pointer ); virtual; stdcall; abstract;
    function GetSize( pv: Pointer ): Longint; virtual; stdcall; abstract;
    function DidAlloc( pv: Pointer ): Integer; virtual; stdcall; abstract;
    procedure HeapMinimize; virtual; stdcall; abstract;
  end;


//-------------------------------------------------------------------------
//
// struct STRRET
//
// structure for returning strings from IShellFolder member functions
//
//-------------------------------------------------------------------------
const
  {$EXTERNALSYM STRRET_WSTR}
  STRRET_WSTR = $0000;
  {$EXTERNALSYM STRRET_OFFSET}
  STRRET_OFFSET = $0001;
  {$EXTERNALSYM STRRET_CSTR}
  STRRET_CSTR = $0002;

type
  TStrRet = record
    uType: UINT;
    case Integer of
      0: ( pOleStr: PWideChar );            // OLESTR that will be freed
      1: ( uOffset: UINT );                 // Offset into SHITEMID (ANSI)
      2: ( cStr: array[ 0..MAX_PATH ] of AnsiChar ); // Buffer to fill in
  end;
  PStrRet = ^TStrRet;

//===========================================================================
//
// IShellDetails interface
// (undocumented - thanks to chris@dbn.lia.net and herb@con2.com for providing this information!)
//===========================================================================
type
  TShColInfo = record
    justify: Integer;
    width: Integer;
    text: TStrRet;
  end;
  PShColInfo = ^TShColInfo;


type
  IShellDetails_NRC = class( IUnknown_NRC )
    function GetDetailsOf( pidl: PItemIdList; col: UINT; var info: TShColInfo ): HResult; virtual; stdcall; abstract;
    function ColumnClick( col: UINT ): HResult; virtual; stdcall; abstract;
  end;


// IShellLink::Resolve fFlags
// Not excluded from BCB4 since ShlObj.h places these in an enum, so no conflict
type
  {$EXTERNALSYM SLR_FLAGS}
  SLR_FLAGS = Integer;

const
  {$EXTERNALSYM SLR_NO_UI}
  SLR_NO_UI = $0001;
  {$EXTERNALSYM SLR_ANY_MATCH}
  SLR_ANY_MATCH = $0002;
  {$EXTERNALSYM SLR_UPDATE}
  SLR_UPDATE = $0004;


// IShellLink::GetPath fFlags
// Not excluded from BCB4 since ShlObj.h places these in an enum, so no conflict
type
  {$EXTERNALSYM SLGP_FLAGS}
  SLGP_FLAGS = Integer;

const
  {$EXTERNALSYM SLGP_SHORTPATH}
  SLGP_SHORTPATH = $0001;
  {$EXTERNALSYM SLGP_UNCPRIORITY}
  SLGP_UNCPRIORITY = $0002;

// QueryContextMenu uFlags
const
  {$EXTERNALSYM CMF_NORMAL}
  CMF_NORMAL = $00000000;
  {$EXTERNALSYM CMF_DEFAULTONLY}
  CMF_DEFAULTONLY = $00000001;
  {$EXTERNALSYM CMF_VERBSONLY}
  CMF_VERBSONLY = $00000002;
  {$EXTERNALSYM CMF_EXPLORE}
  CMF_EXPLORE = $00000004;
  {$EXTERNALSYM CMF_NOVERBS}
  CMF_NOVERBS = $00000008;
  {$EXTERNALSYM CMF_CANRENAME}
  CMF_CANRENAME = $00000010;
  {$EXTERNALSYM CMF_NODEFAULT}
  CMF_NODEFAULT = $00000020;
  {$EXTERNALSYM CMF_INCLUDESTATIC}
  CMF_INCLUDESTATIC = $00000040;
  {$EXTERNALSYM CMF_RESERVED}
  CMF_RESERVED = $FFFF0000;                 // View specific

// GetCommandString uFlags
const
  {$EXTERNALSYM GCS_VERBA}
  GCS_VERBA = $00000000;                    // canonical verb
  {$EXTERNALSYM GCS_HELPTEXTA}
  GCS_HELPTEXTA = $00000001;                // help text (for status bar)
  {$EXTERNALSYM GCS_VALIDATEA}
  GCS_VALIDATEA = $00000002;                // validate command exists
  {$EXTERNALSYM GCS_VERBW}
  GCS_VERBW = $00000004;                    // canonical verb (unicode)
  {$EXTERNALSYM GCS_HELPTEXTW}
  GCS_HELPTEXTW = $00000005;                // help text (unicode version)
  {$EXTERNALSYM GCS_VALIDATEW}
  GCS_VALIDATEW = $00000006;                // validate command exists (unicode)
  {$EXTERNALSYM GCS_UNICODE}
  GCS_UNICODE = $00000004;                  // for bit testing - Unicode string

  {$EXTERNALSYM GCS_VERB}
  GCS_VERB = GCS_VERBA;
  {$EXTERNALSYM GCS_HELPTEXT}
  GCS_HELPTEXT = GCS_HELPTEXTA;
  {$EXTERNALSYM GCS_VALIDATE}
  GCS_VALIDATE = GCS_VALIDATEA;

const
  {$EXTERNALSYM CMDSTR_NEWFOLDER}
  CMDSTR_NEWFOLDER = 'NewFolder';
  {$EXTERNALSYM CMDSTR_VIEWLIST}
  CMDSTR_VIEWLIST = 'ViewList';
  {$EXTERNALSYM CMDSTR_VIEWDETAILS}
  CMDSTR_VIEWDETAILS = 'ViewDetails';

const
  {$EXTERNALSYM SEE_MASK_NO_CONSOLE}
  SEE_MASK_NO_CONSOLE = $00008000;
  {$EXTERNALSYM SEE_MASK_ASYNCOK}
  SEE_MASK_ASYNCOK = $00100000;

const
  {$EXTERNALSYM CMIC_MASK_HOTKEY}
  CMIC_MASK_HOTKEY = SEE_MASK_HOTKEY;
  {$EXTERNALSYM CMIC_MASK_ICON}
  CMIC_MASK_ICON = SEE_MASK_ICON;
  {$EXTERNALSYM CMIC_MASK_FLAG_NO_UI}
  CMIC_MASK_FLAG_NO_UI = SEE_MASK_FLAG_NO_UI;
  {$EXTERNALSYM CMIC_MASK_UNICODE}
  CMIC_MASK_UNICODE = SEE_MASK_UNICODE;
  {$EXTERNALSYM CMIC_MASK_NO_CONSOLE}
  CMIC_MASK_NO_CONSOLE = SEE_MASK_NO_CONSOLE;
//CMIC_MASK_HASLINKNAME = SEE_MASK_HASLINKNAME;
//CMIC_MASK_FLAG_SEP_VDM = SEE_MASK_FLAG_SEPVDM;
//CMIC_MASK_HASTITLE = SEE_MASK_HASTITLE;
  {$EXTERNALSYM CMIC_MASK_ASYNCOK}
  CMIC_MASK_ASYNCOK = SEE_MASK_ASYNCOK;

  {$EXTERNALSYM CMIC_MASK_MODAL}
  CMIC_MASK_MODAL = $80000000;              // Internal

  {$EXTERNALSYM CMIC_VALID_SEE_FLAGS}
  CMIC_VALID_SEE_FLAGS = 0;                 //SEE_VALID_CMIC_FLAGS; // Internal

type
  TCMInvokeCommandInfo = packed record
    cbSize: UINT;                           // must be sizeof(CMINVOKECOMMANDINFO)
    fMask: UINT;                            // any combination of CMIC_MASK_*
    hwnd: THandle;                          // might be NULL (indicating no owner window)
    lpVerb: PAnsiChar;                          // either a string of MAKEINTRESOURCE(idOffset)
    lpParameters: PAnsiChar;                    // might be NULL (indicating no parameter)
    lpDirectory: PAnsiChar;                     // might be NULL (indicating no specific directory)
    nShow: Integer;                         // one of SW_ values for ShowWindow() API

    dwHotKey: DWORD;
    hIcon: THandle;
  end;
  PCMInvokeCommandInfo = ^TCMInvokeCommandInfo;


  IContextMenu_NRC = class( IUnknown_NRC )
  public
    function QueryContextMenu( hMenu: THandle; indexMenu, idCmdFirst, idCmdLast, uFlags: UINT ): HResult; virtual; stdcall; abstract;
    function InvokeCommand( const ici: TCMInvokeCommandInfo ): HResult; virtual; stdcall; abstract;
    function GetCommandString( idCmd, uType: UINT; pwReserved: PUINT;
      pszName: PAnsiChar; cchMax: UINT ): HResult; virtual; stdcall; abstract;
  end;

type
  IContextMenu2_NRC = class( IContextMenu_NRC )
  public
    function HandleMenuMsg( uMsg, wParam, lParam: UINT ): HResult; virtual; stdcall; abstract;
  end;

type
  IEnumIDList_NRC = class( IUnknown_NRC )
  public
    function Next( celt: Integer; var pidl: PItemIdList; pFetched: PInteger ): HResult; virtual; stdcall; abstract;
    function Skip( celt: Integer ): HResult; virtual; stdcall; abstract;
    function Reset: HResult; virtual; stdcall; abstract;
    function Clone( var enumIdList: IEnumIdList_NRC ): HResult; virtual; stdcall; abstract;
  end;

// IShellFolder::GetDisplayNameOf/SetNameOf uFlags

// IShellFolder::GetAttributesOf flags
const
  {$EXTERNALSYM SFGAO_COMPRESSED}
  SFGAO_COMPRESSED    = $04000000;     // object is compressed
  {$EXTERNALSYM SFGAO_BROWSABLE}
  SFGAO_BROWSABLE     = $08000000;     // is in-place browsable
  {$EXTERNALSYM SFGAO_NONENUMERATED}
  SFGAO_NONENUMERATED = $00100000;     // is a non-enumerated object
  {$EXTERNALSYM SFGAO_NEWCONTENT}
  SFGAO_NEWCONTENT    = $00200000;     // should show bold in explorer tree
  {$EXTERNALSYM SFGAO_STREAM}
  SFGAO_STREAM        = $00400000;     // supports BindToObject(IID_IStream)

type
  IShellFolder_NRC = class( IUnknown_NRC )
  public
    function ParseDisplayName( hwndOwner: HWND; reserved: Pointer; displayName: PWideChar; var chEaten: DWORD; var pidl: PItemIdList; var dwAttributes: DWORD ): HResult; virtual; stdcall; abstract;
    function EnumObjects( hwndOwner: HWND; grfFlags: DWORD; var penumIdList: IEnumIdList_NRC ): HResult; virtual; stdcall; abstract;
    function BindToObject( pidl: PItemIdList; reserved: Pointer; const riid: TGUID; var pvOut: Pointer ): HResult; virtual; stdcall; abstract;
    function BindToStorage( pidl: PItemIdList; reserved: Pointer; const riid: TGUID; var pvObj: Pointer ): HResult; virtual; stdcall; abstract;
    function CompareIDs( lparam: LPARAM; pidl1, pidl2: PItemIdList ): HResult; virtual; stdcall; abstract;
    function CreateViewObject( hwndOwner: HWND; const riid: TGUID; var ppvOut: Pointer ): HResult; virtual; stdcall; abstract;
    function GetAttributesOf( cidl: UINT; var pidl: PItemIdList; var rgfInOut: DWORD ): HResult; virtual; stdcall; abstract;
    function GetUIObjectOf( hwndOwner: HWND; cild: UINT; var pidl: PItemIdList; const riid: TGUID; prgfInOut: PUINT; var ppvOut: Pointer ): HResult; virtual; stdcall; abstract;
    function GetDisplayNameOf( pidl: PItemIdList; uFlags: DWORD; var name: TStrRet ): HResult; virtual; stdcall; abstract;
    function SetNameOf( hwndOwner: HWND; pidl: PItemIdList; swzName: PWideChar; uFlags: DWORD; var pidlOut: PItemIdList ): HResult; virtual; stdcall; abstract;
  end;

  IShellFolder2_NRC = class( IShellFolder_NRC )
  public
    procedure F1; virtual; stdcall; abstract;
    procedure F2; virtual; stdcall; abstract;
    procedure F3; virtual; stdcall; abstract;
    function GetDefaultColumnState( iColumn: UINT; var pcsFlags: DWORD ): HResult; virtual; stdcall; abstract;
    procedure F5; virtual; stdcall; abstract;
    function GetDetailsOf( pidl: PItemIdList; iColumn: UINT; var sd: TShColInfo ): HResult; virtual; stdcall; abstract;
  end;

  IShellLink_NRC = class( IUnknown_NRC )
  public
    function GetPath( pszFile: PChar; cchMaxPath: Integer;
      var fd: TWin32FindData; fFlags: UINT ): HResult; virtual; stdcall; abstract;
    function GetIDList( var pidl: PItemIDList ): HResult; virtual; stdcall; abstract;
    function SetIDList( pidl: PItemIDList ): HResult; virtual; stdcall; abstract;
    function GetDescription( pszName: PChar; cchMaxName: Integer ): HResult; virtual; stdcall; abstract;
    function SetDescription( pszName: PChar ): HResult; virtual; stdcall; abstract;
    function GetWorkingDirectory( pszDir: PChar; cchMaxPath: Integer ): HResult; virtual; stdcall; abstract;
    function SetWorkingDirectory( pszDir: PChar ): HResult; virtual; stdcall; abstract;
    function GetArguments( pszArgs: PChar; cchMaxPath: Integer ): HResult; virtual; stdcall; abstract;
    function SetArguments( pszArgs: PChar ): HResult; virtual; stdcall; abstract;
    function GetHotKey( var wHotKey: Word ): HResult; virtual; stdcall; abstract;
    function SetHotKey( wHotKey: Word ): HResult; virtual; stdcall; abstract;
    function GetShowCmd( var iShowCmd: Integer ): HResult; virtual; stdcall; abstract;
    function SetShowCmd( iShowCmd: Integer ): HResult; virtual; stdcall; abstract;
    function GetIconLocation( pszIconPath: PChar; cchIconPath: Integer; var iIcon: Integer ): HResult; virtual; stdcall; abstract;
    function SetIconLocation( pszIconPath: PChar; iIcon: Integer ): HResult; virtual; stdcall; abstract;
    function SetRelativePath( pszPathRel: PChar; dwReserved: UINT ): HResult; virtual; stdcall; abstract;
    function Resolve( hwnd: HWND; fFlags: UINT ): HResult; virtual; stdcall; abstract;
    function SetPath( pszFile: PChar ): HResult; virtual; stdcall; abstract;
  end;


{-- Access to exported functions -----}

// registry entries for special paths are kept in :
//const REGSTR_PATH_SPECIAL_FOLDERS  = REGSTR_PATH_EXPLORER+'\Shell Folders';
const
  {$EXTERNALSYM CSIDL_DESKTOP}
  CSIDL_DESKTOP = $0000;
  {$EXTERNALSYM CSIDL_INTERNET}
  CSIDL_INTERNET = $0001;                   // 1.3i
  {$EXTERNALSYM CSIDL_PROGRAMS}
  CSIDL_PROGRAMS = $0002;
  {$EXTERNALSYM CSIDL_CONTROLS}
  CSIDL_CONTROLS = $0003;
  {$EXTERNALSYM CSIDL_PRINTERS}
  CSIDL_PRINTERS = $0004;
  {$EXTERNALSYM CSIDL_PERSONAL}
  CSIDL_PERSONAL = $0005;
  {$EXTERNALSYM CSIDL_FAVORITES}
  CSIDL_FAVORITES = $0006;
  {$EXTERNALSYM CSIDL_STARTUP}
  CSIDL_STARTUP = $0007;
  {$EXTERNALSYM CSIDL_RECENT}
  CSIDL_RECENT = $0008;
  {$EXTERNALSYM CSIDL_SENDTO}
  CSIDL_SENDTO = $0009;
  {$EXTERNALSYM CSIDL_BITBUCKET}
  CSIDL_BITBUCKET = $000A;
  {$EXTERNALSYM CSIDL_STARTMENU}
  CSIDL_STARTMENU = $000B;
  {$EXTERNALSYM CSIDL_DESKTOPDIRECTORY}
  CSIDL_DESKTOPDIRECTORY = $0010;
  {$EXTERNALSYM CSIDL_DRIVES}
  CSIDL_DRIVES = $0011;
  {$EXTERNALSYM CSIDL_NETWORK}
  CSIDL_NETWORK = $0012;
  {$EXTERNALSYM CSIDL_NETHOOD}
  CSIDL_NETHOOD = $0013;
  {$EXTERNALSYM CSIDL_FONTS}
  CSIDL_FONTS = $0014;
  {$EXTERNALSYM CSIDL_TEMPLATES}
  CSIDL_TEMPLATES = $0015;
  {$EXTERNALSYM CSIDL_COMMON_STARTMENU}
  CSIDL_COMMON_STARTMENU = $0016;           // WinNT 4.0 items below here
  {$EXTERNALSYM CSIDL_COMMON_PROGRAMS}
  CSIDL_COMMON_PROGRAMS = $0017;
  {$EXTERNALSYM CSIDL_COMMON_STARTUP}
  CSIDL_COMMON_STARTUP = $0018;
  {$EXTERNALSYM CSIDL_COMMON_DESKTOPDIRECTORY}
  CSIDL_COMMON_DESKTOPDIRECTORY = $0019;
  {$EXTERNALSYM CSIDL_APPDATA}
  CSIDL_APPDATA = $001A;
  {$EXTERNALSYM CSIDL_PRINTHOOD}
  CSIDL_PRINTHOOD = $001B;
  {$EXTERNALSYM CSIDL_ALTSTARTUP}
  CSIDL_ALTSTARTUP = $001D;                 // 1.3i, DBCS
  {$EXTERNALSYM CSIDL_COMMON_ALTSTARTUP}
  CSIDL_COMMON_ALTSTARTUP = $001E;          // 1.3i, DBCS
  {$EXTERNALSYM CSIDL_COMMON_FAVORITES}
  CSIDL_COMMON_FAVORITES = $001F;           // 1.3i
  {$EXTERNALSYM CSIDL_INTERNET_CACHE}
  CSIDL_INTERNET_CACHE = $0020;             // 1.3i
  {$EXTERNALSYM CSIDL_COOKIES}
  CSIDL_COOKIES = $0021;                    // 1.3i
  {$EXTERNALSYM CSIDL_HISTORY}
  CSIDL_HISTORY = $0022;                    // 1.3i

type
  IEnumFormatEtc_NRC = class( IUnknown_NRC )
  public
    function Next( celt: Longint; var elt; pceltFetched: PLongint ): HResult; virtual; stdcall; abstract;
    function Skip( celt: Longint ): HResult; virtual; stdcall; abstract;
    function Reset: HResult; virtual; stdcall; abstract;
    function Clone( var Enum: IEnumFormatEtc_NRC ): HResult; virtual; stdcall; abstract;
  end;

  IDataObject_NRC = class( IUnknown_NRC )
  public
    function GetData( var formatetcIn: TFormatEtc; var medium: TStgMedium ): HResult; virtual; stdcall; abstract;
    function GetDataHere( var formatetc: TFormatEtc; var medium: TStgMedium ): HResult; virtual; stdcall; abstract;
    function QueryGetData( var formatetc: TFormatEtc ): HResult; virtual; stdcall; abstract;
    function GetCanonicalFormatEtc( var formatetc: TFormatEtc; var formatetcOut: TFormatEtc ): HResult; virtual; stdcall; abstract;
    function SetData( var formatetc: TFormatEtc; var medium: TStgMedium; fRelease: BOOL ): HResult; virtual; stdcall; abstract;
    function EnumFormatEtc( dwDirection: Longint; var enumFormatEtc: IEnumFormatEtc_NRC ): HResult; virtual; stdcall; abstract;
    function DAdvise( var formatetc: TFormatEtc; advf: Longint; advSink: Pointer; var dwConnection: Longint ): HResult; virtual; stdcall; abstract;
    function DUnadvise( dwConnection: Longint ): HResult; virtual; stdcall; abstract;
    function EnumDAdvise( var enumAdvise: Pointer ): HResult; virtual; stdcall; abstract;
  end;

  IDropTarget_NRC = class( IUnknown_NRC )
  public
    function DragEnter( dataObj: IDataObject_NRC; grfKeyState: Longint; pt: TPoint; var dwEffect: Longint ): HResult; virtual; stdcall; abstract;
    function DragOver( grfKeyState: Longint; pt: TPoint; var dwEffect: Longint ): HResult; virtual; stdcall; abstract;
    function DragLeave: HResult; virtual; stdcall; abstract;
    function Drop( dataObj_NRC: IDataObject_NRC; grfKeyState: Longint; pt: TPoint; var dwEffect: Longint ): HResult; virtual; stdcall; abstract;
  end;

  IDropSource_NRC = class( IUnknown_NRC )
  public
    function QueryContinueDrag( fEscapePressed: BOOL; grfKeyState: Longint ): HResult; virtual; stdcall; abstract;
    function GiveFeedback( dwEffect: Longint ): HResult; virtual; stdcall; abstract;
  end;

implementation

function _CoInitialize( pvReserved: Pointer ): HResult; stdcall; external 'ole32.dll' name 'CoInitialize';

procedure _CoUninitialize; stdcall; external 'ole32.dll' name 'CoUninitialize';

var
  _NeedToUnload: Boolean;

initialization
  _NeedToUnload := Succeeded( _CoInitialize( nil ) );

finalization
  if _NeedToUnload then
    _CoUninitialize;
end.

