{===============================================================================
  RzShellUtils Unit

  Raize Components - Component Source Unit

  Copyright © 1995-2015 by Raize Software, Inc.  All Rights Reserved.


  Description
  ------------------------------------------------------------------------------
  Implements low-level utilities useful for dealing with shell interfaces and
  structures. Also includes utilities for creating and resolving shortcuts.


  Modification History
  ------------------------------------------------------------------------------
  6.1    (20 Sep 2012)
    * The CreateShortcut procedure now creates properly formatted shortcuts.
  ------------------------------------------------------------------------------
  5.1    (16 Mar 2009)
    * Fixed AnsiChar/PChar issue in GetPIDLSize and BinaryDataSort functions.
  ------------------------------------------------------------------------------
  3.0.11 (12 Dec 2004)
    * Fixed memory leak in IsNetworkDriveConnected.
    * Added SHELL32_VER.
  ------------------------------------------------------------------------------
  3.0.10 (26 Dec 2003)
    * Added IsNetworkDriveConnection function.
  ------------------------------------------------------------------------------
  3.0    (20 Dec 2002)
    * Initial inclusion in Raize Components.
===============================================================================}

{$I RzComps.inc}

{$RANGECHECKS OFF}
{$WRITEABLECONST OFF}
{$TYPEDADDRESS ON}

unit RzShellUtils;

interface

uses
  {$IFDEF USE_CS}
  CodeSiteLogging,
  {$ENDIF}
  ActiveX,
  Windows,
  SysUtils,
  ShellApi,
  Dialogs,
  Messages,
  Forms,
  Classes,
  ShlObj,
  ComObj,
  RzShellIntf;


type
  TCSIDL = ( csidlDesktop,                           // $0000
             csidlInternet,                          // $0001
             csidlPrograms,                          // $0002
             csidlControls,                          // $0003
             csidlPrinters,                          // $0004
             csidlPersonal,                          // $0005
             csidlFavorites,                         // $0006
             csidlStartup,                           // $0007
             csidlRecent,                            // $0008
             csidlSendTo,                            // $0009
             csidlBitBucket,                         // $000A
             csidlStartMenu,                         // $000B
             csidl_None1,                            // $000C
             csidlMyMusic,                           // $000D
             csidlMyVideo,                           // $000E
             csidl_None2,                            // $000F
             csidlDesktopDirectory,                  // $0010
             csidlDrives,                            // $0011
             csidlNetwork,                           // $0012
             csidlNethood,                           // $0013
             csidlFonts,                             // $0014
             csidlTemplates,                         // $0015
             csidlCommonStartMenu,                   // $0016
             csidlCommonPrograms,                    // $0017
             csidlCommonStartup,                     // $0018
             csidlCommonDesktopDirectory,            // $0019
             csidlAppData,                           // $001a
             csidlPrintHood,                         // $001b
             csidlLocalAppData,                      // $001c
             csidlAltStartup,                        // $001d
             csidlCommonAltStartup,                  // $001e
             csidlCommonFavorites,                   // $001f
             csidlInternetCache,                     // $0020
             csidlCookies,                           // $0021
             csidlHistory,                           // $0022
             csidlCommonAppData,                     // $0023
             csidlWindows,                           // $0024
             csidlSystem,                            // $0025
             csidlProgramFiles,                      // $0026
             csidlMyPictures,                        // $0027
             csidlProfile,                           // $0028
             csidlSystemX86,                         // $0029
             csidlProgramFilesX86,                   // $002a
             csidlProgramFilesCommon,                // $002b
             csidlProgramFilesCommonX86,             // $002c
             csidlCommonTemplates,                   // $002d
             csidlCommonDocuments,                   // $002e
             csidlCommonAdminTools,                  // $002f
             csidlAdminTools,                        // $0030
             csidlConnections,                       // $0031
             csidl_None3,                            // $0032
             csidl_None4,                            // $0033
             csidl_None5,                            // $0034
             csidlCommonMusic,                       // $0035
             csidlCommonPictures,                    // $0036
             csidlCommonVideo,                       // $0037
             csidlResources,                         // $0038
             csidlResourcesLocalized,                // $0039
             csidlCommonOEMLinks,                    // $003a
             csidlCDBurnArean,                       // $003b
             csidl_None6,                            // $003c
             csidlComputersNearMe,                   // $003d
             csidlNone );

(*
const
  {$EXTERNALSYM CSIDL_FLAG_CREATE}
  CSIDL_FLAG_CREATE = $8000;
  {$EXTERNALSYM CSIDL_FLAG_DONT_VERIFY}
  CSIDL_FLAG_DONT_VERIFY = $4000;
  {$EXTERNALSYM CSIDL_FLAG_MASK}
  CSIDL_FLAG_MASK = $FF00;
  *)


{-- General utilities. These are not necessarily shell related but are used by more than one Shell Control Pack unit. -- }

function IsWin95: Boolean;
function IsOSR2OrGreater: Boolean;          // Returns TRUE if running Win95 OSR2 or higher.
function IsWinNT: Boolean;
function IsWin2000: Boolean;
function HasWin95Shell: Boolean;

type
  TRzModuleVersion = packed record
    case Integer of
      0: ( w1, w2, w3, w4: Word );          // Higher number means more significant - w4=major, w3=minor etc.
      1: ( dw1, dw2: Integer );
      {$IFNDEF BCB}
      2: ( asComp: Int64 );                  // Treat as a single 64-bit integer
      {$ENDIF}
      3: ( _1, _2, minor, major: Word );
      4: ( _3, version: Integer );
      5: ( asInt64: Int64 );
  end;
  PRzModuleVersion = ^TRzModuleVersion;
  // Unless you are specifically interested in the build version (w2 or w1) then you would normally
  // compare .version members.

function GetModuleVersion( const aModuleName: string; var {out}  aVersion: TRzModuleVersion ): Boolean;

{-- Comctl32.dll support --}
const
  COMCTL32_VER600 = ( 6 shl 16 ) or 00;     // WinXP version
  COMCTL32_VER581 = ( 5 shl 16 ) or 81;     // IE6 version
  COMCTL32_VER580 = ( 5 shl 16 ) or 80;     // IE5 version
  COMCTL32_VER472 = ( 4 shl 16 ) or 72;     // IE4.01 version
  COMCTL32_VER471 = ( 4 shl 16 ) or 71;     // IE4 version
  COMCTL32_VER470 = ( 4 shl 16 ) or 70;     // IE3 version
  COMCTL32_VER400 = ( 4 shl 16 ) or 00;     // Win95 first release version

const
  SHELL32_VER50 = ( 5 shl 16 ) or 00;           // Win2K, WinME
  SHELL32_VER60 = ( 6 shl 16 ) or 00;           // WinXP

var
  COMCTL32_VER: TRzModuleVersion;
  SHELL32_VER: TRzModuleVersion;


{-- Utilities. There is virtually no performance penalty for using these ShellMem* routines compared
    to calling SHGetMalloc yourself - and you don't have to manage the IMalloc interface. }
function ShellMemAlloc( size: Cardinal ): Pointer;
procedure ShellMemFree( p: Pointer );
function ShellMemRealloc( p: Pointer; size: Cardinal ): Pointer;
function ShellIMalloc: IMalloc_NRC;

{-- Higher level conversion utils ----}
function ShellGetFolderFromIdList( p: PItemIdList; var ish: IShellFolder_NRC ): HResult;
function ShellGetIdListFromPath( const path: string; var p: PItemIdList ): HResult;
function ShellGetPathFromIdList( p: PItemIdList ): string;
function ShellGetDisplayPathName( aPathName: string ): string; // Returns the properly cased pathname
function ShellGetSpecialFolderPath( ahwnd: TRzHandle; csidl: TCSIDL ): string;
function ShellGetSpecialFolderIdList( ahwnd: TRzHandle; csidl: TCSIDL; var idlist: PItemIdList ): HResult;
function ShellGetIconIndex( absIdList: PItemIdList; uFlags: DWORD ): Integer;
function ShellGetIconIndexFromPath( const path: string; uFlags: DWORD ): Integer;
function ShellGetIconIndexFromExt( const ext: string; uFlags: DWORD ): Integer;
function ShellGetSpecialFolderIconIndex( csidl: TCSIDL; uFlags: DWORD ): Integer;
function ShellFindCSIDLFromIdList( aIdList: PItemIdList ): TCSIDL;
function ShellCSIDLEqualsIdList( aIdList: PitemIdList;  csidl: TCSIDL ): Boolean;

type
  TRzFriendlyNameFlags = ( fnNormal, fnInFolder, fnForParsing, fnForEditing );


function ShellGetFriendlyNameFromIdList( ishf: IShellFolder_NRC; pidl: PItemIdList; flags: TRzFriendlyNameFlags ): string;
{
 If ishf=nil, then pidl is an absolute item id list. A temporary IShellFolder for the desktop will
 be created to get the name.
 flags can be any SHGNO constant.

                  File system path     Display name             Notes
                  -------------------- ------------------------ ----------------------------------------
     fnNormal     C:\Windows\File.txt  file                     If not showing extensions
                  \\Computer\Share     share on computer
                  C:\                  My Drive (C)             Where C has the volume name My Drive

     fnInFolder   C:\Windows\File.txt  file
                  \\Computer\Share     share
                  C:\                  My Drive (C)

     fnForParsing C:\Windows\File.txt  C:\Windows\File.txt
                  \\Computer\Share     \\Computer\Share
                  C:\                  C:\
}

function ShellGetFriendlyNameForLastIdListElement( AAbsoluteIdList: PItemIdList ): string;
{ Returns the friendly name of the last item ID part of an absolute ID list.

  Desktop                 [] none
  My Computer             [0.my computer]
  C:\                     [0.my computer][1.C:\]
  C:\Program Files        [0.my computer][1.C:\][2.Program Files]
  C:\Program Files\Stuff  [0.my computer][1.C:\][2.Program Files][3.Stuff]

  The algorithm is:
    If only one item use desktop to get friendly name.
    If >1 item, open the Count-2 folder and use it to get the friendly name of the last node.}


type
  TRzShellIconSize = ( iszSmall, iszLarge );

function ShellGetSystemImageList( Size: TRzShellIconSize ): THandle;

function ShellGetDesktopFolder( var Folder: IShellFolder_NRC ): HResult;

{-- String utilities -----------------}
function StrretToString( pidl: PItemIdList; const r: TStrRet ): string;
procedure StrretFree( const r: TStrRet );

function EnsureTrailingCharDB( const Source: string; TrailingChar: Char ): string;

{-- Low-level Pidl Utilities ---------}
function CopyIdList( ishm: IMalloc_NRC; pidl: PItemIdList ): PItemIdList;
function ConcatIdLists( ishm: IMalloc_NRC; First, Second: PItemIdList ): PItemIdList;
function IdListLen( pidl: PItemIdList ): Integer;
function CompareAbsIdLists( pidl1, pidl2: PItemIdList ): Integer;
  // Compare absolute (relative to desktop) pidls. Returns <0, 0 or >0. If Result=MAXINT then function failed.

{The TRzIdListArray class treats an item id list as an array of items. You can easily process each
 element of the pidl.

 property Item[ idx: Integer ]: PItemIdList;
   The returned id is allocated from shell memory and returned. You don't have to free it. If you
   want to keep it you should use CopyIdList() to make a copy. Each call to Item invalidates the previous
   return value.

 Example:
   procedure DoWork( pa: TRzIdListArray ):
   var p1, p2: PItemIdList;
   begin
     p1 := pa.items[1];
     p2 := pa.items[2]; // BUG p1 is now invalid.
     // ... work ...
   end;

  You should instead do this:
   procedure DoWork( pa: TRzIdListArray ):
   var p1, p2: PItemIdList;
   begin
     p1:=nil; p2:=nil;
     try
       p1 := CopyIdList(pa.items[1]);
       p2 := CopyIdList(pa.items[2]);
       // ... work ...
     finally
       if Assigned(p1) then ShellMemFree(p1);
       if Assigned(p2) then ShellMemFree(p2);
     end;
   end;

   Since you will very rarely be processing more that one item at a time, you should very rarely need to
   go to this trouble.

   The GoUp method works in a similar way (it invalidates previous Results of GoUp or Item[].
   GoUp(n) removes the last "n" items from the item id list and returns the Result. The id list
   being operated on is not affected, hence GoUp() calls on a given TRzIdListArray are NOT cumulative.
 }
type
  TRzIdListArray = class( TObject )
  protected
    FPidl: PItemIdList;

    FLastItem: PItemIdList;
    function GetCount: Integer;
    function GetItem( Index: Integer ): PItemIdList;
  public
    constructor Create( p: PItemIdList );
    destructor Destroy; override;

    function GoUp( items: Integer ): PItemIdList;

    property ItemCount: Integer read GetCount;

    property Item[ Index: Integer ]: PItemIdList
      read GetItem; default;

    property Pidl: PItemIdList
      read FPidl;
  end;


type
  TRzPidlList = class( TObject )
  private
    FList: TList;
    FSorted: Boolean;
    FShellFolder: IShellFolder_NRC;
    FMalloc: IMalloc_NRC;
    FDuplicates: TDuplicates;

    function GetPIDL( Index: Integer ): PItemIdList;
    procedure SetPIDL( Index: Integer; PIDL: PItemIdList );
    function GetObject( Index: Integer ): Pointer;
    procedure SetObject( Index: Integer; aObject: Pointer );
    procedure SetSorted( isSorted: Boolean );

    function GetCount: Integer;

    function GetCapacity: Integer;
    procedure SetCapacity( cap: Integer );

  protected
    function BinarySearch( const PIDL: PItemIdList; var Index: Integer ): Boolean;
    function LinearSearch( const PIDL: PItemIdList; var Index: Integer ): Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    function Add( const PIDL: PItemIdList ): Integer; virtual;
    function AddObject( const PIDL: PItemIdList; aObject: TObject ): Integer; virtual;
    procedure Delete( index: Integer );
    procedure Clear;
    function IndexOf( const PIDL: PItemIdList ): Integer;

    procedure Insert( Index: Integer; Pidl: PItemIdList );
    procedure InsertObject( Index: Integer; Pidl: PItemIdList; aObject: Pointer );

    procedure Sort;
    property PIDLs[ index: Integer ]: PItemIdList read GetPIDL write SetPIDL; default;
    property Objects[ index: Integer ]: Pointer read GetObject write SetObject;
    property Sorted: Boolean read FSorted write SetSorted;
    property Malloc: IMalloc_NRC read FMalloc write FMalloc;
    property Count: Integer read GetCount;
    property ShellFolder: IShellFolder_NRC read FShellFolder write FShellFolder;

    property Duplicates: TDuplicates read FDuplicates write FDuplicates;
    property Capacity: Integer read GetCapacity write SetCapacity;
  end;



{-- Shortcuts Utilities --------------}
type
  TLinkDataOption = ( ldoUseDesc, ldoUseArgs, ldoUseIcon, ldoUseWorkDir, ldoUseHotKey, ldoUseShowCmd );
  TLinkDataOptions = set of TLinkDataOption;

  TLinkData = record
       // Mandatory members
    pathName: string;                       // Pathname of original object
    options: TLinkDataOptions;              // Set of flags indicating optional member usage

       // Optional members
    desc: string;                           // Description of link file (its filename for example)
    args: string;                           // Command-line arguments
    iconPath: string;                       // Pathname of file containing the icon
    iconIndex: Integer;                     // Index of icon in 'iconPath'.  -ve values are resource ids (i think?).
    workingDir: string;                     // Working directory when process starts
    showCmd: Integer;                       // How to show the initial window
    hotkey: Word;                           // Hot key for the link
    noUI: Boolean;                          // Prevent any error or search dialogs from displaying

       // Output members - used by ResolveShortcut, not used by CreateShortcut or CreateQuickShortcut
    idList: PItemIdList;
    w32fd: TWin32FindData;
  end;                                      {TLinkData}

function CreateShortcut( const linkPathName: string; const linkData: TLinkData ): HResult;
function CreateQuickShortcut( const linkPathName, targetPathName: string ): HResult;

function ResolveShortcut( const linkPathName: string; var linkData: TLinkData; afWantIdList: Boolean ): HResult;

//--
function RzClsidFromFileType( aExtension: string; var aCLSID: TGUID ): Boolean;

//--
var
  gFlushDriveInfoSem: Integer;
{$IFDEF PTDEBUG}
var
  gFlushDriveInfoCallCount: Integer;
{$ENDIF}

procedure FlushDriveInfoCache;
procedure LockFlushDriveInfoCache;
procedure UnlockFlushDriveInfoCache;


//-- WM_DEVICECHANGE broadcast handler

type
  TRzDeviceChangeEvent = procedure( ASender: TObject; var AMessage: TMessage ) of object;

  TRzDeviceChangeHandler = class( TObject )
  private
    FActive: Boolean;
    FWindowReceiver: HWND;
    FNotifyList: TList;
  protected
    procedure DeleteItem( AItemIndex: Integer );
    procedure WndProc( var AMessage: TMessage );
    procedure BroadcastToList( var AMessage: TMessage );
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add( AToNotify: TRzDeviceChangeEvent );
    procedure Remove( AToNotify: TRzDeviceChangeEvent );
    property Active: Boolean read FActive write FActive;
  end;

function RzDeviceChangeHandler: TRzDeviceChangeHandler;


{__________________________________________________________________________________________________}
// You can pass these 'verbs' to TRzShellList.DoCommandForAllSelected and TRzShellTree.DoCommandForNode
// to execute the relevant menu command. These strings are never displayed and are language independent.

// -- These commands are available to most folders --
const
  RZSH_CMDS_DELETE = 'delete';
  RZSH_CMDS_PASTE = 'paste';
  RZSH_CMDS_CUT = 'cut';
  RZSH_CMDS_COPY = 'copy';
  RZSH_CMDS_PROPERTIES = 'properties';
  RZSH_CMDS_EXPLORE = 'explore';            // Opens a Windows explorer
  RZSH_CMDS_OPEN = 'open';                  // Opens a Windows explorer folder-view
  RZSH_CMDS_FIND = 'find';                  // Open the find dialog
  RZSH_CMDS_LINK = 'link';                  // Same as 'Create Shortcut' menu item

// -- Commands used by Dialup Networking
const
  RZSH_CMDS_DUN_CREATE = 'create';          // Create new connection wizard
  RZSH_CMDS_DUN_CONNECT = 'connect';        // Connect

// -- These are commands that have no 'verb' but have tested to have the same ID under Win95, Win95OSR2, WinNT4 and IE4
// -- So the id is pretty reliable, but there a no promises!
// -- The other thing to remember is that the IDs are reused for different types of folders. So make sure you use the
// -- right command ID with the right kind of folder.
const
  RZSH_CMDID_FORMAT = PChar( 35 );          // Only on drive root directory folders
                                        // Doesn't seem to work

{ Substitutes strings of the form %1,%2 etc. into aFmtStr and returns the Result. }
function FormatStrPos( aFmtStr: string; data: array of string ): string;

{ Given a command line string 'ins' returns all the parameters, taking into account
  quotes and double-byte characters. }
procedure ParametizeCmdLineDB( const ins: string; outs: TStrings );

{DBCS enabled TrimRight}
function TrimRightDB( Str: string ): string;

{Copies possible DB char at 'aPos' from 'aSource' and appends to 'aDest', incrementing 'aPos' by 1 or 2.}
procedure CopyCharDB( var APos: Integer; const ASource: string; var ADest: string );


// __ WM_DEVICECHANGE constants ___________________________
const
  {$EXTERNALSYM DBT_DEVNODES_CHANGED}
  DBT_DEVNODES_CHANGED = $0007;
  {$EXTERNALSYM DBT_QUERYCHANGECONFIG}
  DBT_QUERYCHANGECONFIG = $0017;
  {$EXTERNALSYM DBT_CONFIGCHANGED}
  DBT_CONFIGCHANGED = $0018;
  {$EXTERNALSYM DBT_CONFIGCHANGECANCELED}
  DBT_CONFIGCHANGECANCELED = $0019;
  {$EXTERNALSYM DBT_MONITORCHANGE}
  DBT_MONITORCHANGE = $001B;
  {$EXTERNALSYM DBT_SHELLLOGGEDON}
  DBT_SHELLLOGGEDON = $0020;
  {$EXTERNALSYM DBT_CONFIGMGAPI32}
  DBT_CONFIGMGAPI32 = $0022;
  {$EXTERNALSYM DBT_VXDINITCOMPLETE}
  DBT_VXDINITCOMPLETE = $0023;
  {$EXTERNALSYM DBT_VOLLOCKQUERYLOCK}
  DBT_VOLLOCKQUERYLOCK = $8041;
  {$EXTERNALSYM DBT_VOLLOCKLOCKTAKEN}
  DBT_VOLLOCKLOCKTAKEN = $8042;
  {$EXTERNALSYM DBT_VOLLOCKLOCKFAILED}
  DBT_VOLLOCKLOCKFAILED = $8043;
  {$EXTERNALSYM DBT_VOLLOCKQUERYUNLOCK}
  DBT_VOLLOCKQUERYUNLOCK = $8044;
  {$EXTERNALSYM DBT_VOLLOCKLOCKRELEASED}
  DBT_VOLLOCKLOCKRELEASED = $8045;
  {$EXTERNALSYM DBT_VOLLOCKUNLOCKFAILED}
  DBT_VOLLOCKUNLOCKFAILED = $8046;
  {$EXTERNALSYM DBT_NO_DISK_SPACE}
  DBT_NO_DISK_SPACE = $0047;
  {$EXTERNALSYM DBT_LOW_DISK_SPACE}
  DBT_LOW_DISK_SPACE = $0048;
  {$EXTERNALSYM DBT_DEVICEARRIVAL}
  DBT_DEVICEARRIVAL = $8000;                // system detected a new device
  {$EXTERNALSYM DBT_DEVICEQUERYREMOVE}
  DBT_DEVICEQUERYREMOVE = $8001;            // wants to remove, may fail
  {$EXTERNALSYM DBT_DEVICEQUERYREMOVEFAILED}
  DBT_DEVICEQUERYREMOVEFAILED = $8002;      // removal aborted
  {$EXTERNALSYM DBT_DEVICEREMOVEPENDING}
  DBT_DEVICEREMOVEPENDING = $8003;          // about to remove, still avail.
  {$EXTERNALSYM DBT_DEVICEREMOVECOMPLETE}
  DBT_DEVICEREMOVECOMPLETE = $8004;         // device is gone
  {$EXTERNALSYM DBT_DEVICETYPESPECIFIC}
  DBT_DEVICETYPESPECIFIC = $8005;           // type specific event
  {$EXTERNALSYM DBT_DEVTYP_OEM}
  DBT_DEVTYP_OEM = $00000000;               // oem-defined device type
  {$EXTERNALSYM DBT_DEVTYP_DEVNODE}
  DBT_DEVTYP_DEVNODE = $00000001;           // devnode number
  {$EXTERNALSYM DBT_DEVTYP_VOLUME}
  DBT_DEVTYP_VOLUME = $00000002;            // logical volume
  {$EXTERNALSYM DBT_DEVTYP_PORT}
  DBT_DEVTYP_PORT = $00000003;              // serial, parallel
  {$EXTERNALSYM DBT_DEVTYP_NET}
  DBT_DEVTYP_NET = $00000004;               // network resource
  {$EXTERNALSYM DBT_DEVTYP_DEVICEINTERFACE}
  DBT_DEVTYP_DEVICEINTERFACE = $00000005;   // device interface class
  {$EXTERNALSYM DBT_DEVTYP_HANDLE}
  DBT_DEVTYP_HANDLE = $00000006;            // file system handle


function IsNetworkDriveConnected( ADriveLetter: Char ): Boolean;


implementation

uses
  RzCommon,
  CommCtrl,
  Registry;


{-- General Utilities ----------------}
var
  gOSVer: TOSVersionInfo;
  g_IShm: IMalloc_NRC = nil;

function IsWin95: Boolean;
begin
  Result := ( gOSVer.dwPlatformId = VER_PLATFORM_WIN32_WINDOWS );
end;

function IsOSR2OrGreater: Boolean;          // Returns TRUE if running Win95 OSR2 or higher.
begin
  Result := IsWin95 and ( LoWord( gOsVer.dwBuildNumber ) > 1000 );
end;

function IsWinNT: Boolean;
begin
  Result := ( gOSVer.dwPlatformId = VER_PLATFORM_WIN32_NT );
end;

function IsWin2000: Boolean;
begin
  Result := ( gOSVer.dwPlatformId = VER_PLATFORM_WIN32_NT ) and ( gOsVer.dwMajorVersion >= 5 );
end;

function HasWin95Shell: Boolean;
begin
  Result := IsWin95 or ( IsWinNT and ( gOSVer.dwMajorVersion >= 4 ) );
end;


{ Returns the 64-bit version information for the given DLL in 'aVersion'. Returns true if the function
  succeeds, false if failed. }

function GetModuleVersion( const aModuleName: string; var {out}  aVersion: TRzModuleVersion ): Boolean;
var
  pinfo: Pointer;
  dummy: DWORD;
  size: Integer;
  pffinfo: PVSFixedFileInfo;
begin
  Result := false;
  pinfo := nil;
  try
    size := GetFileVersionInfoSize( PChar( aModuleName ), dummy );
    if size = 0 then
      Exit;

    GetMem( pinfo, size );
    if not GetFileVersionInfo( PChar( aModuleName ), 0, size, pinfo ) then
      Exit;

    if not VerQueryValue( pinfo, '\', Pointer( pffinfo ), dummy ) then
      Exit;
    aVersion.dw2 := pffinfo.dwFileVersionMS;
    aVersion.dw1 := pffinfo.dwFileVersionLS;
    Result := true;
  finally
    FreeMem( pinfo );
  end;
end;                                        {GetModuleVersion}



{-- Global Utilities -----------------}

function ShellMemAlloc( size: Cardinal ): Pointer;
begin
  Result := g_IShm.Alloc( size );
end;                                        {ShellMemAlloc}


function ShellMemRealloc( p: Pointer; size: Cardinal ): Pointer;
begin
  if Assigned( p ) then
    Result := g_IShm.Realloc( p, size )
  else
    Result := nil;
end;                                        {ShellMemRealloc}


procedure ShellMemFree( p: Pointer );
begin
  if Assigned( p ) then
    g_IShm.Free( p );
end;                                        {ShellMemFree}


function ShellIMalloc: IMalloc_NRC;
begin
  Result := g_IShm;
end;                                        {ShellIMalloc}


function ShellGetFolderFromIdList( p: PItemIdList; var ish: IShellFolder_NRC ): HResult;
var
  idesk: IShellFolder_NRC;
begin
  ish := nil;
  Result := ShellGetDesktopFolder( idesk );
  if ( Result <> S_OK ) then
    Exit;

  if ( p = nil ) or ( PWord( p )^ = 0 ) then
    ish := idesk                            // If 'p' refers to the desktop, there is no binding required.
  else
  try
    Result := idesk.BindToObject( p, nil, IID_IShellFolder, Pointer( ish ) );
  finally
    idesk.Release;
  end;
end;                                        {ShellGetFolderFromIdList}


function ShellGetIdListFromPath( const path: string; var p: PItemIdList ): HResult;
var
  ishf: IShellFolder_NRC;
  wtmp: array[ 0..MAX_PATH ] of WideChar;
  chEaten: DWORD;
  dwAttributes: DWORD;
begin
  {$IFNDEF RX102_OR_HIGHER}
  Result := S_FALSE;
  {$ENDIF}

  ishf := nil;
  p := nil;
  try
    try
      Result := ShellGetDesktopFolder( ishf );
      if Result <> S_OK then
        Exit;

      StringToWideChar( path, wtmp, High( wtmp ) );
      Result := ishf.ParseDisplayName( 0, nil, wtmp, chEaten, p, dwAttributes );
      if Result <> S_OK then
        Exit;
    except
      ShellMemFree( p );
      raise;
    end;
  finally
    if Assigned( ishf ) then
      ishf.Release;
  end;
end;                                        {ShellGetIdListFromPath}


function ShellGetPathFromIdList( p: PItemIdList ): string;
var
  sz: array[ 0..MAX_PATH ] of Char;
  w: Word;
begin
  if ( p = nil ) then
  begin                                     // If p is the desktop, do the 'make it work' hack
    w := 0;
    p := Pointer( @w );
  end;

  if SHGetPathFromIdList( p, @sz[ 0 ] ) then
    SetString( Result, sz, Strlen( sz ) )
  else
    Result := '';
end;                                        {ShellGetPathFromIdList}


// Returns the properly cased pathname

function ShellGetDisplayPathName( aPathName: string ): string;
var
  ish: IShellFolder_NRC;
  wtmp: array[ 0..MAX_PATH ] of WideChar;
  chEaten: DWORD;
  dwAttributes: DWORD;
  pidl: PItemIdList;
begin
  Result := '';
  ish := nil;
  pidl := nil;
  try
    if ShellGetDesktopFolder( ish ) <> S_OK then
      Exit;

    StringToWideChar( aPathName, wtmp, High( wtmp ) );
    if ish.ParseDisplayName( 0, nil, wtmp, chEaten, pidl, dwAttributes ) <> S_OK then
      Exit;
    SetLength( Result, MAX_PATH );
    SHGetPathFromIdList( pidl, PChar( Result ) );
    SetLength( Result, StrLen( PChar( Result ) ) );

  finally
    ShellMemFree( pidl );
    if Assigned( ish ) then
      ish.Release;
  end;
end;                                        {ShellGetDisplayPathname}


function ShellGetSpecialFolderPath( ahwnd: TRzHandle; csidl: TCSIDL ): string;
var
  pidl: PItemIdList;
begin
  if Succeeded( SHGetSpecialFolderLocation( ahwnd, Integer( csidl ), pidl ) ) then
  begin
    Result := ShellGetPathFromIdList( pidl );
    ShellMemFree( pidl );
  end
  else
    Result := '';
end;                                        {ShellGetSpecialFolderPathname}


function ShellGetSpecialFolderIdList( ahwnd: TRzHandle; csidl: TCSIDL; var idlist: PItemIdList ): HResult;
begin
  Result := SHGetSpecialFolderLocation( ahwnd, Integer( csidl ), idlist );
end;


function ShellGetIconIndex( absIdList: PItemIdList; uFlags: DWORD ): Integer;
var
  shfi: TSHFileInfo;
  wval: Word;
begin
  try
    FillChar( shfi, SizeOf( shfi ), 0 );
    if absIdList = nil then
    begin
      wval := 0;
      absIdList := PItemIdList( @wval );
    end;
    SHGetFileInfo( PChar( absIdList ), 0, shfi, Sizeof( TSHFileInfo ), SHGFI_PIDL or SHGFI_SYSICONINDEX or uflags );
    Result := shfi.iIcon;
  except
    Result := 0;                            // Corrupt files or broken icon shell extensions can cause raise exceptions that need to be caught
  end;
end;                                        {ShellGetIconIndex}


function ShellGetIconIndexFromPath( const path: string; uFlags: DWORD ): Integer;
var
  shfi: TSHFileInfo;
begin
  try
    FillChar( shfi, SizeOf( shfi ), 0 );
    SHGetFileInfo( PChar( path ), 0, shfi, Sizeof( TSHFileInfo ), SHGFI_SYSICONINDEX or uFlags );
    Result := shfi.iIcon;
  except
    Result := 0;                            // Corrupt files or broken icon shell extensions can cause raise exceptions that need to be caught
  end;
end;                                        {ShellGetIconIndexFromPath}


function ShellGetIconIndexFromExt( const ext: string; uFlags: DWORD ): Integer;
var
  shfi: TSHFileInfo;
begin
  try
    FillChar( shfi, SizeOf( shfi ), 0 );
    SHGetFileInfo( PChar( ext ), 0, shfi, Sizeof( TSHFileInfo ), SHGFI_SYSICONINDEX or SHGFI_USEFILEATTRIBUTES or uflags );
    Result := shfi.iIcon;
  except
    Result := 0;                            // Corrupt files or broken icon shell extensions can cause raise exceptions that need to be caught
  end;
end;                                        {ShellGetIconIndexFromExt}


function ShellGetSpecialFolderIconIndex( csidl: TCSIDL; uFlags: DWORD ): Integer;
var
  IdList: PItemIdList;
begin
  OleCheck( ShellGetSpecialFolderIdList( 0, csidl, IdList ) );
  try
    Result := ShellGetIconIndex( IdList, uFlags );
  finally
    ShellMemFree( IdList );
  end;
end;                                        {ShellGetSpecialFolderIconIndex}


function ShellFindCSIDLFromIdList( aIdList: PItemIdList ): TCSIDL;
var
  Index: TCSIDL;
  IdListToTest: PItemIdList;
begin
  IdListToTest := nil;
  try
    for Index := Low( Index ) to High( Index ) do
    begin
      if Succeeded( ShellGetSpecialFolderIdList( 0, Index, IdListToTest ) ) then
      begin
        if CompareAbsIdLists( aIdList, IdListToTest ) = 0 then // found
        begin
          Result := Index;
          Exit;                             //
        end;
      end;
      ShellMemFree( IDListToTest );
      IdListToTest := nil;
    end;
    Result := csidlNone;                    // not found
  finally
    ShellMemFree( IDListToTest );
  end;
end;                                        {ShellFindCSIDLFromIdList}


function ShellCSIDLEqualsIdList( aIdList: PitemIdList;  csidl: TCSIDL ): Boolean;
var
  csidlIdList: PItemIdList;
begin
  OleCheck( ShellGetSpecialFolderIdList( 0, csidl, csidlIdList ) );
  try
    Result := CompareAbsIdLists( aIdList, csidlIdList ) = 0;
  finally
    ShellMemFree( csidlIdList );
  end;
end;


function ShellGetFriendlyNameFromIdList( ishf: IShellFolder_NRC; pidl: PItemIdList; flags: TRzFriendlyNameFlags ): string;
const
  _F: array[ TRzFriendlyNameFlags ] of DWORD = ( SHGDN_NORMAL, SHGDN_INFOLDER, SHGDN_FORPARSING,
                                                 SHGDN_INFOLDER or SHGDN_FOREDITING );
var
  strret: TStrRet;
  dw: DWord;
  procedure DoDesktop;
  var
    idsk: IShellFolder_NRC;
  begin
    ShellGetDesktopFolder( idsk );
    Result := ShellGetFriendlyNameFromIdList( idsk, pidl, flags );
    idsk.Release;
  end;
begin
  if Assigned( ishf ) then
  begin
    strret.uType := STRRET_WSTR;
    result := '';
    dw := ishf.GetDisplayNameOf(pidl, _F[flags], strret);
    if Succeeded(dw) then
    begin
      Result := StrretToString(pidl, strret);
      StrretFree(strret);
    end
    else
    begin
      strret.uType := STRRET_CSTR;
      result := '';
      dw := ishf.GetDisplayNameOf( pidl, _F[flags], strret );
      if Succeeded(dw) then
      begin
        result := StrretToString(pidl, strret);
        StrretFree(strret);
      end;
    end;
  end
  else
    DoDesktop;
end;                                        {ShellGetFriendlyNameFromIdList}


function ShellGetFriendlyNameForLastIdListElement( AAbsoluteIdList: PItemIdList ): string;
var
  desk, parent: IShellFolder_NRC;
  pa: TRzIdListArray;
  c: Integer;
begin
  pa := nil;
  desk := nil;
  parent := nil;
  Result := '';
  try
    if ShellGetDesktopFolder( desk ) <> S_OK then
      Exit;
    pa := TRzIdListArray.Create( AAbsoluteIdList );

    c := pa.ItemCount;
    if ( c <= 1 ) then
      Result := ShellGetFriendlyNameFromIdList( desk, AAbsoluteIdList, fnInFolder )
    else
    begin
      OleCheck( desk.BindToObject( pa.GoUp( 1 ), nil, IID_IShellFolder, Pointer( parent ) ) );
      Result := ShellGetFriendlyNameFromIdList( parent, pa.Item[ c - 1 ], fnInFolder );
    end;

  finally
    if Assigned( desk ) then
      desk.Release;
    if Assigned( parent ) then
      parent.Release;
    pa.Free;
  end;
end;                                        {ShellGetFriendlyNameForLastIdListElement}


var
  gOverlaysAppliedAlready: Boolean = false;

{ On WinNT/IE4 and later we get a completely private image list, with no overlays. We need to load
  up the shell link and network share images, add them to the list and make them overlays before
  we return. }

function ShellGetSystemImageList( Size: TRzShellIconSize ): THandle;

  procedure ApplyOverlaysToSysImageLists;
  const
    SIC_SHARING_HAND = 29;
    SIC_SHORTCUT = 30;

    LR_SHARED = $8000;
  var
    h16, h32: THandle;
    hicon: THandle;
    hShell32: THandle;
    shfi: TSHFileInfo;
    w: Word;
  begin
    hShell32 := Windows.LoadLibrary( 'shell32.dll' );
    if ( hShell32 = 0 ) then
      Exit;
    try
      w := 0;
      h16 := SHGetFileInfo( 'C:\', 0, shfi, Sizeof( TSHFileInfo ), SHGFI_SYSICONINDEX or SHGFI_SMALLICON );
      if h16 = 0 then
        h16 := SHGetFileInfo( PChar( @w ),0, shfi,Sizeof( TSHFileInfo ), SHGFI_SYSICONINDEX or SHGFI_SMALLICON or SHGFI_PIDL );

      h32 := SHGetFileInfo( 'C:\', 0, shfi, Sizeof( TSHFileInfo ), SHGFI_SYSICONINDEX or SHGFI_LARGEICON );
      if h32 = 0 then
        h32 := SHGetFileInfo( PChar( @w ),0, shfi,Sizeof( TSHFileInfo ), SHGFI_SYSICONINDEX or SHGFI_LARGEICON or SHGFI_PIDL );

        // Have to apply the icons to both image lists simultaneously

      hicon := Windows.LoadImage( hShell32, PChar( SIC_SHARING_HAND ), IMAGE_ICON, 16, 16, LR_SHARED );
      ImageList_SetOverlayImage( h16, ImageList_AddIcon( h16, hicon ), 1 );
      DestroyIcon( hicon );

      hicon := Windows.LoadImage( hShell32, PChar( SIC_SHARING_HAND ), IMAGE_ICON, 32, 32, LR_SHARED );
      ImageList_SetOverlayImage( h32, ImageList_AddIcon( h32, hicon ), 1 );
      DestroyIcon( hicon );

      hicon := Windows.LoadImage( hShell32, PChar( SIC_SHORTCUT ), IMAGE_ICON, 16, 16, LR_SHARED );
      ImageList_SetOverlayImage( h16, ImageList_AddIcon( h16, hicon ), 2 );
      DestroyIcon( hicon );

      hicon := Windows.LoadImage( hShell32, PChar( SIC_SHORTCUT ), IMAGE_ICON, 32, 32, LR_SHARED );
      ImageList_SetOverlayImage( h32, ImageList_AddIcon( h32, hicon ), 2 );
      DestroyIcon( hicon );

      gOverlaysAppliedAlready := true;
    finally
      if ( hShell32 <> 0 ) then
        FreeLibrary( hShell32 );
    end;
  end; {= ApplyOverlaysToSysImageLists =}

const
  _VALS: array[ TRzShellIconSize ] of DWORD = ( SHGFI_SMALLICON, SHGFI_LARGEICON );

var
  shfi: TSHFileInfo;
  w: Word;

begin {= ShellGetSystemImageList =}
  if IsWinNT and ( COMCTL32_VER.version >= COMCTL32_VER471 ) and not gOverlaysAppliedAlready then
    ApplyOverlaysToSysImageLists;
  Result := SHGetFileInfo( 'C:\', 0, shfi, Sizeof( TSHFileInfo ),
                           SHGFI_SYSICONINDEX or _VALS[ Size ] );
  if Result = 0 then
  begin
    w := 0;
    Result := SHGetFileInfo( PChar( @w ), 0, shfi, Sizeof( TSHFileInfo ),
                             SHGFI_SYSICONINDEX or _VALS[ Size ] or SHGFI_PIDL );

  end;
end; {= ShellGetSystemImageList =}


function _SHGetDesktopFolder( var AShellFolder ): HResult; stdcall; external 'shell32.dll' name 'SHGetDesktopFolder';

function ShellGetDesktopFolder( var Folder: IShellFolder_NRC ): HResult;
begin
  Result := _SHGetDesktopFolder( Folder );
end;


function StrretToString( pidl: PItemIdList; const r: TStrRet ): string;
begin
  case r.uType of
    STRRET_CSTR:
      Result := string( r.cstr ); // typecast needed when compiling under RS2009

    STRRET_OFFSET:
      if Assigned( pidl ) then
        SetString( Result, PChar( UINT( pidl ) + r.uOffset ), StrLen( PChar( UINT( pidl ) + r.uOffset ) ) );

    STRRET_WSTR:
      Result := WideCharToString( r.pOleStr );
  end;
end;                                        {StrretToString}


procedure _CoTaskMemFree( pv: Pointer ); stdcall; external 'ole32.dll' name 'CoTaskMemFree';

procedure StrretFree( const r: TStrRet );
begin
  if ( r.uType = STRRET_WSTR ) and Assigned( r.pOleStr ) then
    _CoTaskMemFree( r.pOleStr );
end;                                        {StrretFree}


{ Ensures that 'aTrailingChar' is in fact the last character in the string.
  Exception: If aSource is empty then the trailing character is NOT appended.
  Double-byte safe under VCL3 or higher. }

function EnsureTrailingCharDB( const Source: string; TrailingChar: Char ): string;
begin
  if ( Length( Source ) > 0 ) then
    if ByteType( Source, Length( Source ) ) = mbSingleByte then
    begin
      if Source[ Length( Source ) ] <> TrailingChar then
        Result := Source + TrailingChar
      else
        Result := Source;
    end
    else
      Result := Source + TrailingChar
  else
    Result := Source;
end;


function IdListLen( pidl: PItemIdList ): Integer;
var
  p: Pointer;
begin
  Result := 0;
  if not Assigned( pidl ) then
    Exit;
  p := pidl;
  while PSHItemID( p ).cb <> 0 do
  begin
    Inc( Result, PSHItemID( p ).cb );
    Inc( PByte( p ), PSHItemID( p ).cb );
  end;
  Inc( Result, 2 );                         // terminator
end;                                        {IdListLen}


function ResultCode( Res: HResult ): Integer;
begin
  Result := Res and $0000FFFF;
end;


function CompareAbsIdLists( pidl1, pidl2: PItemIdList ): Integer;
var
  ishf: IShellFolder_NRC;
  dw: DWORD;
begin
  Result := MAXINT;                         // MAXINT means failure in this case

 // Special cases, IShellFolder.CompareIDs doesn't work well with empty pidls
  if not Assigned( pidl1 ) and not Assigned( pidl2 ) then
    Result := 0
  else if not Assigned( pidl1 ) or not Assigned( pidl2 ) then
    Result := -1
  else if ( PWord( pidl1 )^ = 0 ) and ( PWord( pidl2 )^ = 0 ) then
    Result := 0
  else if ( PWord( pidl1 )^ = 0 ) or ( PWord( pidl2 )^ = 0 ) then
    Result := -1
  else if Succeeded( ShellGetDesktopFolder( ishf ) ) then
  try
    dw := ishf.CompareIDs( 0, pidl1, pidl2 );
    if Succeeded( dw ) then
      Result := Smallint( ResultCode( dw ) );
  finally
    ishf.Release;
  end;
end;                                        {CompareAbsIdLists}



{You must free the returned IDlist with the passed allocator - aFirst and/or aSecond can be nil.}

function ConcatIdLists( ishm: IMalloc_NRC; First, Second: PItemIdList ): PItemIdList;
var
  flen, slen: Integer;
begin
  flen := IdListLen( First ) - 2;          // not including terminator
  if ( flen < 0 ) then
    flen := 0;
  slen := IdListLen( Second ) - 2;         // not including terminator
  if ( slen < 0 ) then
    slen := 0;
  if Assigned( ishm ) then
    Result := ishm.Alloc( flen + slen + 2 ) // +2 for null-terminator
  else
    Result := ShellMemAlloc( flen + slen + 2 );
  if Assigned( Result ) then
  begin
    CopyMemory( Result, First, flen );
    CopyMemory( Pointer( Integer( Result ) + flen ), Second, slen );
    PWord( Integer( Result ) + flen + slen )^ := 0; // null-terminator at end the list
  end;
end;                                        {ConcatIdLists}


function CopyIdList( ishm: IMalloc_NRC; pidl: PItemIdList ): PItemIdList;
var
  len: Integer;
begin
  len := IdListLen( pidl ) - 2;
  if ( len < 0 ) then
    len := 0;
  if Assigned( ishm ) then
    Result := ishm.Alloc( len + 2 )
  else
    Result := ShellMemAlloc( len + 2 );
  if Assigned( Result ) then
  begin
    CopyMemory( Result, pidl, len );
    PWord( Integer( Result ) + len )^ := 0; // null-terminator to end the list
  end;
end;                                        {CopyIdList}


{============================}
{== TRzIdListArray Methods ==}
{============================}

constructor TRzIdListArray.Create( p: PItemIdList );
begin
  inherited Create;
  FPidl := p;
end;                                        {TRzIdListArray.Create}

destructor TRzIdListArray.Destroy;
begin
  if Assigned( FLastItem ) then
    ShellMemFree( FLastItem );
  inherited;
end;                                        {TRzIdListArray.Destroy}

function TRzIdListArray.GoUp( items: Integer ): PItemIdList;
var
  size: Integer;
  p: Pointer;
  curItem, endItem: Integer;
begin
  if Assigned( FLastItem ) then
  begin
    ShellMemFree( FLastItem );
    FLastItem := nil;
  end;

  p := FPidl;
  Result := nil;
  if ( p = nil ) then
    Exit;

  endItem := ( ItemCount - items );
  size := 0;
  curItem := 0;
  while ( PWord( p )^ <> 0 ) and ( curItem <> endItem ) do
  begin
    Inc( size, PWord( p )^ );
    Inc( PByte( p ), PWord( p )^ );
    Inc( curItem )
  end;

  if ( curItem = enditem ) then
  begin
    FLastItem := ShellMemAlloc( size + 2 );
    ZeroMemory( FLastItem, size + 2 );
    Move( FPidl^, FLastItem^, size );
  end
  else
    FLastItem := nil;
  Result := FLastItem;
end; {= TRzIdListArray.GoUp =}


function TRzIdListArray.GetCount: Integer;
var
  p: Pointer;
begin
  p := FPidl;
  Result := 0;
  if ( p = nil ) then
    Exit;
  while PWord( p )^ <> 0 do
  begin
    Inc( PByte( p ), PWord( p )^ );
    Inc( Result );
  end;
end; {= TRzIdListArray.GetCount =}


function TRzIdListArray.GetItem( Index: Integer ): PItemIdList;
var
  p: Pointer;
  curidx: Integer;
begin
  if Assigned( FLastItem ) then
  begin
    ShellMemFree( FLastItem );
    FLastItem := nil;
  end;
  p := FPidl;

  if ( p = nil ) or ( Index < 0 ) or ( ( Index >= ItemCount ) and ( ItemCount <> 0 ) ) then
  begin
    Result := nil;
    Exit;
  end;

  curidx := 0;
  while ( PWord( p )^ <> 0 ) and ( curidx <> Index ) do
  begin
    Inc( PByte( p ), PWord( p )^ );
    Inc( curidx );
  end;

  if ( curidx = Index ) then
  begin
    FLastItem := ShellMemAlloc( PWord( p )^ + 2 );
    Move( p^, FLastItem^, PWord( p )^ );
    PWord( UINT( FLastItem ) + PWord( p )^ )^ := 0;
  end
  else
    FLastItem := nil;
  Result := FLastItem;
end; {= TRzIdListArray.GetItem =}


//*****************************************************************************
//
// PIDLLIST - Written by Clinton R. Johnson  and adapted for use by Plasmatech.
//
// April 12, 1998
//
// This object collects and manages PIDLS in a manner consistant with TLISTs,
// and extendes the concept with a binary search algorithm (adapted from the
// TSTRINGLIST.FIND method written by Borland International) when the SORTED
// property is set to true.
//
// TPIDLLISTs maybe sorted in one of two ways : Assigning the SHELLFOLDER property
// causes all comparisons to be performed by the ISHELLFOLDER.COMPAREID call,
// otherwise, if the SHELLFOLDER property is not assigned, Or is NIL), a raw
// comparision is performed on the binary data of the PIDL itself.
//
// The raw binary data comparision is on the order of 10x faster because it
// lacks a number of conversion steps before evaluation, and does not require
// data marshaling.  This impacts the performance of ADD/ADDOBJECT/INDEXOF
// methods when sorted is true, and SORT always.
//
// When the SORTED property is false, all algorithms are LINEAR (and thus
// slow).
//
// If you assign the MALLOC property, PIDLs are freed with the item is deleted,
// or when the list is cleared or freed, otherwise the PIDL is NOT touched.
//
// PIDLS ARE NOT COPIED INTO THIS LIST, ONLY THEIR POINTERS.  IF YOU DO NOT
// ASSIGN THE MALLOC PROPERTY, YOU ARE RESPONSIBLE FOR FREEING THE PIDLS
// YOURSELF.
//
// - Clinton R. Johnson
//
//*****************************************************************************

function GetPIDLSize( pidl: PItemIdList ): Integer;
var
  Item: PShItemID;
  P: PAnsiChar absolute Item;
begin
  if not Assigned( pidl ) then
  begin
    Result := 0;
    Exit;
  end;
  Result := SizeOf( Item^.cb );
  P := PAnsiChar( pidl );
  while ( Item^.cb <> 0 ) do
  begin
    Result := Result + Item^.cb;
    P := ( P + Item^.cb );
  end;
end;


type
  PPidlItem = ^TPIdlItem;
  TPIDLItem = record
    PIDL: PItemIDList;
    AObject: Pointer;
  end;

var
  GlobalShellFolder: IShellFolder_NRC;      // used in comparison function

function BinaryDataSort( Item1: Pointer; Item2: Pointer ): Integer;
var
  S1L: Integer;
  S2L: Integer;
  Len: Integer;
  P1: PAnsiChar;
  P2: PAnsiChar;
begin
  S1L := GetPIDLSize( PPidlItem( Item1 ).PIDL );
  S2L := GetPIDLSize( PPidlItem( Item2 ).PIDL );

  if S1L < S2L then
    Len := S1L
  else
    Len := S2L;

  P1 := PAnsiChar( PPidlItem( Item1 ).PIDL );
  P2 := PAnsiChar( PPidlItem( Item2 ).PIDL );

  while ( Len > 0 ) and ( P1[ 0 ] = P2[ 0 ] ) do
  begin
    Inc( P1 );
    Inc( P2 );
    Dec( Len );
  end;

  if ( Len = 0 ) then
    Result := S1L - S2L
  else
    Result := Integer( Byte( P1[ 0 ] ) ) - Integer( Byte( P2[ 0 ] ) );
end;

function ShellFolderSort( Item1: Pointer; Item2: Pointer ): Integer;
begin
  Result := Smallint( ResultCode( GlobalShellFolder.CompareIDs( 0,
    PPidlItem( Item1 ).PIDL, PPidlItem( Item2 ).PIDL ) ) );
end;

constructor TRzPidlList.Create;
begin
  inherited;
  FList := TList.Create;
  FShellFolder := nil;
  FMalloc := nil;
  FSorted := False;
  Duplicates := dupError;
end;

destructor TRzPidlList.Destroy;
begin
  Clear;
  FList.Free;
  inherited;
end;

function TRzPidlList.GetPIDL( Index: INteger ): PItemIDList;
begin
  Result := PPidlItem( FList[ Index ] ).PIDL;
end;

procedure TRzPidlList.SetPIDL( Index: Integer; PIDL: PItemIDList );
begin
  PPidlItem( FList[ Index ] ).PIDL := PIDL;
end;

function TRzPidlList.GetObject( Index: Integer ): Pointer;
begin
  Result := PPidlItem( FList[ Index ] ).AObject;
end;

procedure TRzPidlList.SetObject( Index: Integer; AObject: Pointer );
begin
  PPidlItem( FList[ Index ] ).AObject := AObject;
end;

procedure TRzPidlList.SetSorted( IsSorted: Boolean );
begin
  if ( IsSorted <> FSorted ) then
  begin
    FSorted := IsSorted;
    if FSorted then
      Sort;
  end;
end;

function TRzPidlList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TRzPidlList.Add( const PIDL: PItemIDList ): Integer;
begin
  Result := AddObject( PIDL, nil );
end;

function TRzPidlList.AddObject( const PIDL: PItemIDList; AObject: TObject ): Integer;
begin
  if Sorted then
  begin
    if BinarySearch( PIDL, Result ) then
    begin
      case Duplicates of
        dupIgnore: Exit;
        dupError: raise Exception.Create( 'TRzPidlList: Duplicate PIDL' );
      end;
    end;
  end
  else
    Result := Count;
  InsertObject( Result, PIDL, AObject );
end;

procedure TRzPidlList.Sort;
begin
  if Assigned( fShellFolder ) then
  begin
    try
      GlobalShellFolder := FShellFolder;
      FList.Sort( ShellFolderSort );
    finally
      GlobalShellFolder := nil;
    end;
  end
  else
    FList.Sort( BinaryDataSort );
end;

function TRzPidlList.BinarySearch( const PIDL: PItemIDList; var Index: Integer ): Boolean;
var
  L, H, I, C: Integer;
  SearchItem: PPidlItem;
  Compare: TListSortCompare;
begin
  Result := False;
  GetMem( SearchItem, Sizeof( SearchItem^ ) );
  if Assigned( FShellFolder ) then
  begin
    GlobalShellFolder := FShellFolder;
    Compare := ShellFolderSort;
  end
  else
    Compare := BinaryDataSort;

  try
    SearchItem.PIDL := PIDL;
    L := 0;
    H := FList.Count - 1;
    while L <= H do
    begin
      I := ( L + H ) shr 1;
      C := Compare( FList[ I ], SearchItem );
      if C < 0 then
        L := I + 1
      else
      begin
        H := I - 1;
        if C = 0 then
        begin
          Result := True;
          if Duplicates <> dupAccept then
            L := I;
        end;
      end;
    end;
    Index := L;
  finally
    FreeMem( SearchItem, SizeOf( SearchItem^ ) );
    GlobalShellFolder := nil;
  end;
end;

function TRzPidlList.LinearSearch( const PIDL: PItemIDList; var Index: Integer ): Boolean;
var
  Loop: Integer;
  SearchItem: PPidlItem;
  Compare: TListSortCompare;
begin
  GetMem( SearchItem, SizeOf( SearchItem^ ) );
  if Assigned( FShellFolder ) then
  begin
    GlobalShellFolder := FShellFolder;
    Compare := ShellFolderSort;
  end
  else
    Compare := BinaryDataSort;

  Result := False;
  try
    SearchItem.PIDL := PIDL;

    for Loop := 0 to Count - 1 do
    begin
      if ( Compare( FList[ Loop ], SearchItem ) = 0 ) then
      begin
        Index := Loop;
        Result := True;
        Break;
      end;
    end;
  finally
    FreeMem( SearchItem, SizeOf( SearchItem^ ) );
    GlobalShellfolder := nil;
  end;
end;

function TRzPidlList.IndexOf( const PIDL: PItemIDList ): Integer;
begin
  if Sorted then
  begin
    if not BinarySearch( PIDL, Result ) then
      Result := -1;
  end
  else
  begin
    if not LinearSearch( PIDL, Result ) then
      Result := -1;
  end;
end;

procedure TRzPidlList.Delete( index: Integer );
var
  PidlItem: PPidlItem;
begin
  if Assigned( FMalloc ) then
  begin
    PidlItem := PPidlItem( Flist[ Index ] );
    FMalloc.Free( PidlItem.PIDL );
  end;
  FreeMem( FList[ index ] );
  Flist.Delete( Index );
end;

procedure TRzPidlList.Clear;
var
  Loop: Integer;
  PidlItem: PPidlItem;
begin
  if Assigned( FMalloc ) then
  begin
    for loop := 0 to Flist.Count - 1 do
    begin
      PidlItem := PPidlItem( Flist[ Loop ] );
      FMalloc.Free( PidlItem.PIDL );
    end;
  end;

  for loop := 0 to FList.Count - 1 do
    FreeMem( Flist[ loop ] );
  FList.Clear;
end;

procedure TRzPidlList.Insert( Index: Integer; Pidl: PItemIDList );
begin
  InsertObject( Index, PIDL, nil );
end;

procedure TRzPidlList.InsertObject( Index: Integer; Pidl: PItemIDList; aObject: Pointer );
var
  PidlItem: PPidlItem;
begin
  GetMem( PidlItem, Sizeof( PidlItem^ ) );
  PidlItem.PIDL := PIDL;
  PidlItem.AObject := aObject;
  FList.Insert( Index, PIDLItem );
end;

function TRzPidlList.GetCapacity: Integer;
begin
  Result := FList.Capacity;
end;

procedure TRzPidlList.SetCapacity( Cap: Integer );
begin
  FList.Capacity := Cap;
end;
// -- END TRzPidlList ================== //

type
  IPersist_NRC = class( IUnknown_NRC )
  public
    function GetClassID( var classID: TGUID ): HResult; virtual; stdcall; abstract;
  end;

  IPersistFile_NRC = class( IPersist_NRC )
  public
    function IsDirty: HResult; virtual; stdcall; abstract;
    function Load( pszFileName: PWideChar; dwMode: Longint ): HResult; virtual; stdcall; abstract;
    function Save( pszFileName: PWideChar; fRemember: BOOL ): HResult; virtual; stdcall; abstract;
    function SaveCompleted( pszFileName: PWideChar ): HResult; virtual; stdcall; abstract;
    function GetCurFile( var pszFileName: PWideChar ): HResult; virtual; stdcall; abstract;
  end;


function _CoCreateInstance( const clsid: TGUID; unkOuter: IUnknown_NRC;
  dwClsContext: Longint; const iid: TGUID; var pv ): HResult; stdcall;
  external 'ole32.dll' name 'CoCreateInstance';

const
  _IID_IPersistFile: TGUID = ( D1: $0000010B; D2: $0000; D3: $0000; D4: ( $C0, $00, $00, $00, $00, $00, $00, $46 ) );
//  _IID_IShellLinkA: TGUID = ( D1: $000214EE; D2: $0000; D3: $0000; D4: ( $C0, $00, $00, $00, $00, $00, $00, $46 ) );
  _IID_IShellLink: TGUID = ( D1: $000214F9; D2: $0000; D3: $0000; D4: ( $C0, $00, $00, $00, $00, $00, $00, $46 ) );
  _CLSCTX_INPROC_SERVER = 1;
  _STGM_READ = $00000000;


function CreateShortcut( const linkPathName: string; const linkData: TLinkData ): HResult;
var
  ish: IShellLink_NRC;
  ips: IPersistFile_NRC;
  wsz: array[ 0..MAX_PATH ] of WideChar;
begin
  ish := nil;
  ips := nil;
  try
    Result := _CoCreateInstance( CLSID_ShellLink, nil, _CLSCTX_INPROC_SERVER, _IID_IShellLink, ish );
    if Failed( Result ) then
      Exit;
    Result := ish.QueryInterface( _IID_IPersistFile, ips );
    if Failed( Result ) then
      Exit;

   // Initialise the shortcut
    ish.SetPath( PChar( linkData.pathName ) );
    if ( ldoUseDesc in linkData.options ) then
      ish.SetDescription( PChar( linkData.desc ) );
    if ( ldoUseArgs in linkData.options ) then
      ish.SetArguments( PChar( linkData.args ) );
    if ( ldoUseIcon in linkData.options ) then
      ish.SetIconLocation( PChar( linkData.iconPath ), linkData.iconIndex );
    if ( ldoUseWorkdir in linkData.options ) then
      ish.SetWorkingDirectory( PChar( linkData.workingDir ) );
    if ( ldoUseHotKey in linkData.options ) then
      ish.SetHotKey( linkData.hotkey );
    if ( ldoUseShowCmd in linkData.options ) then
      ish.SetShowCmd( linkData.showCmd );

   // Now save the shortcut to disk
    Result := ips.Save( StringToWideChar( linkPathName, wsz, High( wsz ) ), TRUE );
    if Result <> S_OK then
      Exit;
  finally
    if Assigned( ips ) then
      ips.Release;
    if Assigned( ish ) then
      ish.Release;
  end;
end;                                        {CreateShortcut}


function CreateQuickShortcut( const linkPathName, targetPathName: string ): HResult;
var
  ld: TLinkData;
begin
  ld.pathName := targetPathName;
  ld.options := [ ldoUseDesc, ldoUseWorkDir ];
  ld.desc := targetPathName;
  ld.workingDir := ExtractFilePath( targetPathName );
  Result := CreateShortcut( linkPathName, ld );
end;                                        {CreateQuickShortcut}


function ResolveShortcut( const linkPathName: string; var linkData: TLinkData; afWantIdList: Boolean ): HResult;
const
  NOUI_FLAG: array[ Boolean ] of DWORD = ( 0, SLR_NO_UI );
  NOUI_WINDOW: array[ Boolean ] of HWND = ( HWND_DESKTOP, $FFFFFFFF );
var
  ishl: IShellLink_NRC;
  ipf: IPersistFile_NRC;
  tmpsz: array[ 0..MAX_PATH ] of Char;
//  wsz: array[ 0..MAX_PATH ] of WideChar;
begin
  ishl := nil;
  ipf := nil;
  try
    Result := _CoCreateInstance( CLSID_ShellLink, nil, _CLSCTX_INPROC_SERVER, _IID_IShellLink, ishl );
    if Failed( Result ) then
      Exit;
    Result := ishl.QueryInterface( _IID_IPersistFile, ipf );
    if Failed( Result ) then
      Exit;

    Result := ipf.Load( PWideChar( linkPathName ), _STGM_READ );
//    Result := ipf.Load( StringToWideChar( linkPathName, @wsz[ 0 ], High( wsz ) ), _STGM_READ );
    if Failed( Result ) then
      Exit;

    Result := ishl.Resolve( NOUI_WINDOW[ linkData.noUI ], MakeLong( 0, NOUI_FLAG[ linkData.noUI ] ) );
    if Failed( Result ) then
      Exit;

    Result := ishl.GetPath( @tmpsz[ 0 ], sizeof( tmpsz ), linkData.w32fd, 0 );
    if Failed( Result ) then
      Exit;
    linkData.pathName := string( tmpsz );

    Result := ishl.GetArguments( @tmpsz[ 0 ], sizeof( tmpsz ) );
    if Failed( Result ) then
      Exit;
    linkData.args := string( tmpsz );

    Result := ishl.GetDescription( @tmpsz[ 0 ], sizeof( tmpsz ) );
    if Failed( Result ) then
      Exit;
    linkData.desc := string( tmpsz );

    Result := ishl.GetIconLocation( @tmpsz[ 0 ], sizeof( tmpsz ), linkData.iconIndex );
    if Failed( Result ) then
      Exit;
    linkData.iconPath := string( tmpsz );

    Result := ishl.GetWorkingDirectory( @tmpsz[ 0 ], sizeof( tmpsz ) );
    if Failed( Result ) then
      Exit;
    linkData.workingDir := string( tmpsz );

    Result := ishl.GetShowCmd( linkData.showCmd );
    if Failed( Result ) then
      Exit;
    Result := ishl.GetHotKey( linkData.hotKey );
    if Failed( Result ) then
      Exit;

    linkData.options := [ ldoUseDesc, ldoUseArgs, ldoUseIcon, ldoUseWorkDir, ldoUseHotKey, ldoUseShowCmd ];

    if afWantIdList then
    begin
      Result := ishl.GetIdList( linkData.idList );
      if Failed( Result ) then
        Exit;
    end
    else
      linkData.idList := nil;
  finally
    if Assigned( ipf ) then
      ipf.Release;
    if Assigned( ishl ) then
      ishl.Release;
  end;
end;                                        {ResolveShortcut}


function _CLSIDFromString( psz: PWideChar; var clsid: TGUID ): HResult; stdcall; external 'ole32.dll' name 'CLSIDFromString';

// Given a file extension (include the '.'), returns the CLSID of the associated object - if any.

function RzClsidFromFileType( aExtension: string; var aCLSID: TGUID ): Boolean;
const
  CLSID_KEY = 'CLSID';
var
  r: TRegistry;
  s: string;
  wca: array[ 0..79 ] of WideChar;
begin
  Result := FALSE;
  r := TRegistry.Create;
  try
    r.RootKey := HKEY_CLASSES_ROOT;
    if not r.KeyExists( aExtension ) then
      Exit;
    if not r.OpenKey( aExtension, FALSE ) then
      Exit;

    s := r.ReadString( '' );                // Try default value first
    if AnsiCompareText( Copy( s, 1, 6 ), 'clsid\' ) = 0 then
    begin
      StringToWideChar( Copy( s, 7, $FF ), @wca[ 0 ], High( wca ) );
      Result := Succeeded( _CLSIDFromString( @wca[ 0 ], aCLSID ) );
      Exit;
    end;

    if r.KeyExists( CLSID_KEY ) then
    begin
      if not r.OpenKey( CLSID_KEY, FALSE ) then
        Exit;
      StringToWideChar( r.ReadString( '' ), @wca[ 0 ], High( wca ) );
      Result := Succeeded( _CLSIDFromString( @wca[ 0 ], aCLSID ) );
      Exit;
    end;

    r.CloseKey;
    if not r.OpenKey( s, FALSE ) then
      Exit;
    if r.KeyExists( CLSID_KEY ) then
    begin
      if not r.OpenKey( CLSID_KEY, FALSE ) then
        Exit;
      StringToWideChar( r.ReadString( '' ), @wca[ 0 ], High( wca ) );
      Result := Succeeded( _CLSIDFromString( @wca[ 0 ], aCLSID ) );
      Exit;
    end;
  finally
    r.Free;
  end;
end; {= RzClsidFromFileType =}


procedure FlushDriveInfoCache;
var
  nilptr: PItemIdList;
  shfMyComputer: IShellFolder_NRC;
  pidlMyComputer: PItemIdList;
  dwAttr: DWORD;
begin
  if gFlushDriveInfoSem <> 0 then
    Exit;
  {$IFDEF PTDEBUG}
  Inc( gFlushDriveInfoCallCount );
  {$ENDIF}

  pidlMyComputer := nil;
  shfMyComputer := nil;
  try
    OleCheck( ShellGetSpecialFolderIdList( 0, csidlDrives, pidlMyComputer ) );
    OleCheck( ShellGetFolderFromIdList( pidlMyComputer, shfMyComputer ) );
    dwAttr := SFGAO_VALIDATE;
    nilptr := nil;
    shfMyComputer.GetAttributesOf( 0, nilptr, dwAttr );
  finally
    if Assigned( shfMyComputer ) then
      shfMyComputer.Release;
    ShellMemFree( pidlMyComputer );
  end;
end;                                        {FlushDriveInfoCache}

procedure LockFlushDriveInfoCache;
begin
  Inc( gFlushDriveInfoSem );
end;

procedure UnlockFlushDriveInfoCache;
begin
  Dec( gFlushDriveInfoSem );
end;


function FormatStrPos( aFmtStr: string; data: array of string ): string;
var
  i: Integer;
  params: array[ 0..49 ] of PChar;
  pBuff: PChar;
  len: UINT;
begin
  if ( aFmtStr = '' ) then
  begin
    Result := '';
    Exit;
  end;
  if High( data ) > High( params ) then
    raise Exception.Create( 'FormatStrPos: Too many substitution strings' );
  for i := 0 to High( data ) do
    params[ i ] := PChar( data[ i ] );
  pbuff := nil;
  len := FormatMessage( FORMAT_MESSAGE_FROM_STRING or FORMAT_MESSAGE_ARGUMENT_ARRAY or FORMAT_MESSAGE_ALLOCATE_BUFFER,
    PChar( aFmtStr ), 0, 0,
    Pointer( @pBuff ), 256 {MINIMUM size!},
    @params[ 0 ] );
  try
//    if (len = 0) then raise Exception.Create( 'FormatStrPos: FormatMessage failed.  '+SysErrorMessage(GetLastError)+#13'"'+aFmtStr+'"' );
//    if (pBuff = nil) then raise Exception.Create( 'FormatStrPos: FormatMessage failed.  '+SysErrorMessage(GetLastError)+#13'"'+aFmtStr+'"' );
//    Result := String(pBuff);
    if ( len = 0 ) or ( pbuff = nil ) then
      Result := ''
    else
      Result := string( pBuff );
  finally
    if len <> 0 then
      LocalFree( UINT( pBuff ) );
  end;
end;                                        {FormatStrPos}


procedure ParametizeCmdLineDB( const ins: string; outs: TStrings );
const
  WHITESPACE = [ ' ', #9 ];
var
  curs: string;
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
    curchar := ins[ inpos ];
    fIsDBCS := IsDBCSLeadByte( Byte( curchar ) );
    case state of
      sNormal:
        begin
          if ( curchar = '"' ) or CharInSet( curchar, WHITESPACE ) then
          begin
            curs := TrimRightDB( curs );
            if Length( curs ) > 0 then
            begin
              outs.Add( curs );
              curs := '';
            end;
            if curchar = '"' then
              state := sInQuotes
            else
              state := sInWhitespace;
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
            if Length( curs ) > 0 then
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
          else                              // fIsDBCS
          begin
            CopyCharDB( inpos, ins, curs );
            state := sNormal;
          end;
        end;
    end;                                    {case}
  end;                                      {while}

  curs := TrimRightDB( curs );
  if Length( curs ) > 0 then
    outs.Add( curs );
end;                                        {ParametizeCmdLineDB}


{DBCS enabled TrimRight}

function TrimRightDB( Str: string ): string;
const
  WHITESPACE_SET = [ #0..#31, ' ' ];
var
  CurPos: Integer;
  LastSpace: Integer;
  EndPos: Integer;
begin
  EndPos := Length( Str );
  LastSpace := EndPos;
  CurPos := 1;
  while CurPos <= EndPos do
  begin
    if IsDBCSLeadByte( Byte( Str[ CurPos ] ) ) then
    begin
      LastSpace := EndPos;
      Inc( CurPos, 2 )
    end
    else if CharInSet( Str[ CurPos ], WHITESPACE_SET ) then
    begin
      if LastSpace = EndPos then
        LastSpace := CurPos - 1;
      Inc( Curpos );
    end
    else
    begin
      LastSpace := EndPos;
      Inc( CurPos );
    end;
  end;
  Result := Copy( Str, 1, LastSpace );
end;                                        {TrimRightDB}

procedure CopyCharDB( var APos: Integer; const ASource: string; var ADest: string );
begin
  if IsDBCSLeadByte( Byte( ASource[ APos ] ) ) then
  begin
    ADest := ADest + ASource[ APos ] + ASource[ APos + 1 ];
    Inc( APos, 2 );
  end
  else
  begin
    ADest := ADest + ASource[ APos ];
    Inc( APos );
  end;
end;                                        {CopyCharDB}


{__________________________________________________________________________________________________}
{ TDeviceChangeHandler }
type
  PMethod = ^TMethod;

const
  AM_DEFERRED_UPDATE = WM_USER + 100;

procedure TRzDeviceChangeHandler.Add( AToNotify: TRzDeviceChangeEvent );
var
  MethodData: PMethod;
begin
  New( MethodData );
  MethodData^ := TMethod( AToNotify );
  FNotifyList.Add( MethodData );
end;                                        {TRzDeviceChangeHandler.Add}

procedure TRzDeviceChangeHandler.BroadcastToList( var AMessage: TMessage );
var
  MethodData: PMethod;
  Index: Integer;
begin
  if not Active then
    Exit;

  for Index := 0 to FNotifyList.Count - 1 do
  begin
    MethodData := PMethod( FNotifyList[ Index ] );
    try
      TRzDeviceChangeEvent( MethodData^ )( Self, AMessage );
    except
      on E: Exception do
        Application.HandleException( E );
    end;
  end;
end;                                        {TRzDeviceChangeHandler.BroadcastToList}

constructor TRzDeviceChangeHandler.Create;
begin
  inherited Create;
  FNotifyList := TList.Create;
  FWindowReceiver := AllocateHWnd( WndProc );
  Active := true;
end;                                        {TRzDeviceChangeHandler.Create}

procedure TRzDeviceChangeHandler.DeleteItem( AItemIndex: Integer );
begin
  Dispose( PMethod( FNotifyList[ AItemIndex ] ) );
  FNotifyList.Delete( AItemIndex );
end;                                        {TRzDeviceChangeHandler.DeleteItem}

destructor TRzDeviceChangeHandler.Destroy;
begin
  FNotifyList.Free;
  DeallocateHWnd( FWindowReceiver );
  inherited;
end;                                        {TRzDeviceChangeHandler.Destroy}

procedure TRzDeviceChangeHandler.Remove( AToNotify: TRzDeviceChangeEvent );
var
  Index: Integer;
  ThisItem: PMethod;
begin
  for Index := 0 to FNotifyList.Count - 1 do
  begin
    ThisItem := PMethod( FNotifyList[ Index ] );
    if ( ThisItem.Code = TMethod( AToNotify ).Code ) and ( ThisItem.Data = TMethod( AToNotify ).Data ) then
    begin
      DeleteItem( Index );
      Exit;
    end;
  end;
end;                                        {TRzDeviceChangeHandler.Remove}

procedure TRzDeviceChangeHandler.WndProc( var AMessage: TMessage );
begin
  case AMessage.Msg of
    WM_DEVICECHANGE:
      begin
        PostMessage( FWindowReceiver, AM_DEFERRED_UPDATE, AMessage.WParam, AMessage.LParam );
        AMessage.Result := DefWindowProc( FWindowReceiver, AMessage.Msg, AMessage.WParam, AMessage.LParam );
      end;

    AM_DEFERRED_UPDATE:
      BroadcastToList( AMessage );

  else
    AMessage.Result := DefWindowProc( FWindowReceiver, AMessage.Msg, AMessage.WParam,
      AMessage.LParam );
  end;
end;                                        {TRzDeviceChangeHandler.WndProc}


var
  gDeviceChangeHandler: TRzDeviceChangeHandler;

function RzDeviceChangeHandler: TRzDeviceChangeHandler;
begin
  if not Assigned( gDeviceChangeHandler ) then
    gDeviceChangeHandler := TRzDeviceChangeHandler.Create;
  Result := gDeviceChangeHandler;
end;


function _SHGetMalloc( var ppMalloc: IMalloc_NRC ): HResult; stdcall; external 'shell32.dll' name 'SHGetMalloc';

const
  netapi32dll = 'netapi32.dll';

var
  _NetApiConnectState: (notTried, triedAndFailed, triedAndSucceeded) = notTried;
  _NetApi32Dll: THandle;

  _NetUseGetInfo: function(UncServerName: LPWSTR; UseName: LPWSTR; Level: DWORD;  var{out} BufPtr): DWORD stdcall;
  _NetApiBufferFree: function(Buffer: Pointer): DWORD stdcall;

function BindNetApi: Boolean;
begin
  if _NetApiConnectState = notTried then
  begin
    _NetApiConnectState := triedAndFailed;
    _NetApi32Dll := LoadLibrary(netapi32dll);
    if _NetApi32Dll <> 0 then
    begin
      @_NetUseGetInfo := GetProcAddress(_NetApi32Dll, 'NetUseGetInfo');
      @_NetApiBufferFree := GetProcAddress(_NetApi32Dll, 'NetApiBufferFree');
      if Assigned(@_NetUseGetInfo) and Assigned(@_NetApiBufferFree)then
        _NetApiConnectState := triedAndSucceeded;
    end
  end;
  Result := _NetApiConnectState = triedAndSucceeded;
end;

function IsNetworkDriveConnected(ADriveLetter: Char): Boolean;
type
  TUseInfo1 = record
    ui1_local: LPWSTR;
    ui1_remote: LPWSTR;
    ui1_password: LPWSTR;
    ui1_status: DWORD;
    ui1_asg_type: DWORD;
    ui1_refcount: DWORD;
    ui1_usecount: DWORD;
  end;
  PUseInfo1 = ^TUseInfo1;
var
  useinfo1: PUseInfo1;
  tmpw: array[0..2] of WideChar;
begin
  if BindNetApi then
  begin
    tmpw[0] := WideChar(ADriveLetter);
    tmpw[1] := ':';
    tmpw[2] := #0000;
    useinfo1 := nil;
    if _NetUseGetInfo(nil, @tmpw[0], 1, useinfo1) = 0 then
    begin
      Result := useinfo1.ui1_status = 0;
      _NetApiBufferFree(useinfo1);
    end
    else
      Result := False;
  end
  else
    Result := True;
end;

initialization
  gOSVer.dwOSVersionInfoSize := SizeOf( TOSVersionInfo );
  Windows.GetVersionEx( gOsVer );
  _SHGetMalloc( g_IShm );
  if not GetModuleVersion( 'comctl32.dll', COMCTL32_VER ) then
    ZeroMemory( @COMCTL32_VER, Sizeof( COMCTL32_VER ) );
  if not GetModuleVersion( 'shell32.dll', SHELL32_VER ) then
    ZeroMemory( @SHELL32_VER, Sizeof( SHELL32_VER ) );
finalization
  gDeviceChangeHandler.Free;
  gDeviceChangeHandler := nil;
  if _NetApi32Dll <> 0 then
    FreeLibrary( _NetApi32Dll );
  if Assigned( g_IShm ) then
  begin
    {In some circumstances, the finalization section can be called before messages like WM_DELETEITEM
    are received. We need to use the IMalloc reference to clean up in response to these messages.
    For this reason we don't release g_IShm at all.}
    // g_IShm.Release;
    // g_IShm:=nil;
  end;
end.

