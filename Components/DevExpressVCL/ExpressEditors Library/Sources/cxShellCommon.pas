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

unit cxShellCommon;

{$I cxVer.inc}

interface

uses
  Types, MaskUtils,
  Windows, ActiveX, Classes, ComObj, Controls, Dialogs, Forms, Math, Messages,
  ShellApi, ShlObj, SyncObjs, SysUtils, cxClasses, Menus, Graphics;

resourcestring
  SShellDefaultNameStr = 'Name';
  SShellDefaultSizeStr = 'Size';
  SShellDefaultTypeStr = 'Type';
  SShellDefaultModifiedStr = 'Modified';

const
  cxShellObjectInternalAbsoluteVirtualPathPrefix = '::{9C211B58-E6F1-456A-9F22-7B3B418A7BB1}';
  cxShellObjectInternalRelativeVirtualPathPrefix = '::{63BE9ADB-E4B5-4623-96AA-57440B4EF5A8}';
  cxShellObjectInternalVirtualPathPrefixLength = 40;
  cxShellNormalItemOverlayIndex = -1;
  cxShellSharedItemOverlayIndex = 0;
  cxShellShortcutItemOverlayIndex = 1;
  SID_IImageList           = '{46EB5926-582E-4017-9FDF-E8998DAA0950}';
  IID_IImageList: TGUID    = SID_IImageList;
  CLSID_ShellItem: TGUID = (
    D1:$43826d1e; D2:$e718; D3:$42ee; D4:($bc,$55,$a1,$e2,$61,$c3,$7b,$fe));

  cxSFGAO_GHOSTED = $00008000; // Error in ShlObj.pas

// Interface declarations, that missed in D4 and D5 versions

(*$HPPEMIT '#include <OleIdl.h>'*)

// cxShell common classes
type
  ITEMIDLISTARRAY = array [0..MaxInt div SizeOf(PItemIDList) - 1] of PItemIDList;
  PITEMIDLISTARRAY = ^ITEMIDLISTARRAY;

  TcxBrowseFolder =(bfCustomPath, bfAltStartup, bfBitBucket,
    bfCommonDesktopDirectory, bfCommonDocuments, bfCommonFavorites,
    bfCommonPrograms, bfCommonStartMenu, bfCommonStartup, bfCommonTemplates,
    bfControls, bfDesktop, bfDesktopDirectory, bfDrives, bfPrinters,
    bfFavorites, bfFonts, bfHistory, bfMyMusic, bfMyPictures, bfNetHood,
    bfProfile, bfProgramFiles, bfPrograms, bfRecent, bfStartMenu, bfStartUp,
    bfTemplates, bfMyDocuments, bfNetwork);

  TcxDropEffect = (deCopy, deMove, deLink);
  TcxDropEffectSet = set of TcxDropEffect;

  TcxCustomItemProducer = class;
  TcxCustomShellRoot = class;
  TcxShellItemsInfoGatherer = class;
  TcxShellItemInfo = class;

  IExtractImage = interface(IUnknown)
    [SID_IExtractImage]
    function GetLocation(pszPathBuffer: LPWSTR; cch: DWORD; var pdwPriority: DWORD;
      var prgSize: TSize; dwRecClrDepth: DWORD;
      var pdwFlags: DWORD): HRESULT; stdcall;
    function Extract(var phBmpThumbnail: HBITMAP): HRESULT; stdcall;
  end;

  IShellItemImageFactory = interface(IUnknown)
    [SID_IShellItemImageFactory]
    function GetImage(size: TSize; flags: UINT; out phbm: HBITMAP): HRESULT; stdcall;
  end;

  IcxDropSource = interface(IDropSource)
  ['{FCCB8EC5-ABB4-4256-B34C-25E3805EA046}']
  end;

  TcxDropSource=class(TInterfacedObject, IcxDropSource)
  private
    FOwner: TWinControl;
  protected
    function QueryContinueDrag(fEscapePressed: BOOL;
      grfKeyState: Longint): HResult; stdcall;
    function GiveFeedback(dwEffect: Longint): HResult; stdcall;
  public
    constructor Create(AOwner:TWinControl);virtual;
    property Owner:TWinControl read FOwner;
  end;

  { TcxShellOptions }

  TcxShellOptions = class(TPersistent)
  private
    FContextMenus: Boolean;
    FFileMask: string;
    FLock: Integer;
    FMasks: TStringList;
    FOwner: TWinControl;
    FShowFolders: Boolean;
    FShowToolTip: Boolean;
    FShowNonFolders: Boolean;
    FShowHidden: Boolean;
    FShowZipFilesWithFolders: Boolean;
    FTrackShellChanges: Boolean;
    FOnShowToolTipChanged: TNotifyEvent;
    procedure SetFileMask(const Value: string);
    procedure SetShowFolders(Value: Boolean);
    procedure SetShowHidden(Value: Boolean);
    procedure SetShowNonFolders(Value: Boolean);
    procedure SetShowToolTip(Value: Boolean);
    procedure SetShowZipFilesWithFolders(AValue: Boolean);
    procedure SetTrackShellChanges(AValue: Boolean);
    procedure UpdateMasks;
  protected
    procedure DoAssign(Source: TcxShellOptions); virtual;
    procedure DoNotifyUpdateContents; virtual;
    procedure NotifyUpdateContents; virtual;
    property OnShowToolTipChanged: TNotifyEvent read FOnShowToolTipChanged
      write FOnShowToolTipChanged;
  public
    constructor Create(AOwner: TWinControl); virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure BeginUpdate;
    procedure EndUpdate;
    function GetEnumFlags: Cardinal;
    function IsFileNameValid(const AName: string): Boolean;
    property FileMask: string read FFileMask write SetFileMask;
    property Owner: TWinControl read FOwner;
  published
    property ShowFolders: Boolean read FShowFolders write SetShowFolders default True;
    property ShowNonFolders: Boolean read FShowNonFolders write SetShowNonFolders default True;
    property ShowHidden: Boolean read FShowHidden write SetShowHidden default False;
    property ShowZipFilesWithFolders: Boolean read FShowZipFilesWithFolders write SetShowZipFilesWithFolders default True;
    property ContextMenus: Boolean read FContextMenus write FContextMenus default True;
    property TrackShellChanges: Boolean read FTrackShellChanges write SetTrackShellChanges default True;
    property ShowToolTip: Boolean read FShowToolTip write SetShowToolTip default True;
  end;

  TcxShellThumbnailOptions = class(TPersistent)
  private
    FHeight: Integer;
    FLock: Integer;
    FOwner: TPersistent;
    FShowThumbnails: Boolean;
    FWidth: Integer;
    FOnChange: TNotifyEvent;
    procedure SetHeight(const Value: Integer);
    procedure SetShowThumbnails(const Value: Boolean);
    procedure SetWidth(const Value: Integer);
  protected
    procedure Changed;
    procedure DoChange;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    constructor Create(AOwner: TPersistent); virtual;
    procedure Assign(Source: TPersistent); override;
    procedure BeginUpdate;
    procedure EndUpdate;
  published
    property Height: Integer read FHeight write SetHeight default 96;
    property ShowThumbnails: Boolean read FShowThumbnails write SetShowThumbnails default False;
    property Width: Integer read FWidth write SetWidth default 96;
  end;

  TcxDetailItem = record
    Text: string;
    Width: Integer;
    Alignment: TAlignment;
    ID: Integer;
  end;

  PcxDetailItem=^TcxDetailItem;

  TcxShellDetails=class
  private
    FItems: TList;
    function GetItems(Index: Integer): PcxDetailItem;
    function GetCount: Integer;
  protected
    property Items:TList read FItems;
  public
    constructor Create;
    destructor Destroy;override;
    procedure ProcessDetails(ACharWidth: Integer; AShellFolder: IShellFolder;
      AFileSystem: Boolean);
    procedure Clear;
    function Add:PcxDetailItem;
    procedure Remove(Item:PcxDetailItem);
    property Item[Index:Integer]:PcxDetailItem read GetItems;default;
    property Count:Integer read GetCount;
  end;

  { TcxShellFolder }

  TcxShellFolderAttribute = (sfaGhosted, sfaHidden, sfaIsSlow, sfaLink,
    sfaReadOnly, sfaShare);
  TcxShellFolderAttributes = set of TcxShellFolderAttribute;

  TcxShellFolderCapability = (sfcCanCopy, sfcCanDelete, sfcCanLink, sfcCanMove,
    sfcCanRename, sfcDropTarget, sfcHasPropSheet);
  TcxShellFolderCapabilities = set of TcxShellFolderCapability;

  TcxShellFolderProperty = (sfpBrowsable, sfpCompressed, sfpEncrypted,
    sfpNewContent, sfpNonEnumerated, sfpRemovable);
  TcxShellFolderProperties = set of TcxShellFolderProperty;

  TcxShellFolderStorageCapability = (sfscFileSysAncestor, sfscFileSystem,
    sfscFolder, sfscLink, sfscReadOnly, sfscStorage, sfscStorageAncestor,
    sfscStream);
  TcxShellFolderStorageCapabilities = set of TcxShellFolderStorageCapability;

  TcxShellFolder = class
  private
    FAbsolutePIDL: PItemIDList;
    FParentShellFolder: IShellFolder;
    FRelativePIDL: PItemIDList;
    function GetAttributes: TcxShellFolderAttributes;
    function GetCapabilities: TcxShellFolderCapabilities;
    function GetDisplayName: string;
    function GetIsFolder: Boolean;
    function GetPathName: string;
    function GetProperties: TcxShellFolderProperties;
    function GetShellAttributes(ARequestedAttributes: LongWord): LongWord;
    function GetShellFolder: IShellFolder;
    function GetStorageCapabilities: TcxShellFolderStorageCapabilities;
    function GetSubFolders: Boolean;
    function HasShellAttribute(AAttribute: LongWord): Boolean; overload;
    function HasShellAttribute(AAttributes, AAttribute: LongWord): Boolean; overload;
    function InternalGetDisplayName(AFolder: IShellFolder; APIDL: PItemIDList;
      ANameType: DWORD): string;
  public
    constructor Create(AAbsolutePIDL: PItemIDList);
    destructor Destroy; override;

    property Attributes: TcxShellFolderAttributes read GetAttributes;
    property Capabilities: TcxShellFolderCapabilities read GetCapabilities;
    property IsFolder: Boolean read GetIsFolder;
    property Properties: TcxShellFolderProperties read GetProperties;
    property StorageCapabilities: TcxShellFolderStorageCapabilities
      read GetStorageCapabilities;
    property SubFolders: Boolean read GetSubFolders;

    property AbsolutePIDL: PItemIDList read FAbsolutePIDL;
    property DisplayName: string read GetDisplayName;
    property ParentShellFolder: IShellFolder read FParentShellFolder;
    property PathName: string read GetPathName;
    property RelativePIDL: PItemIDList read FRelativePIDL;
    property ShellFolder: IShellFolder read GetShellFolder;
  end;

  TcxRootChangedEvent = procedure (Sender: TObject; Root: TcxCustomShellRoot) of object;

  TcxCustomShellRoot = class(TPersistent)
  private
    FAttributes: Cardinal;
    FBrowseFolder: TcxBrowseFolder;
    FCustomPath: WideString;
    FFolder: TcxShellFolder;
    FIsRootChecking: Boolean;
    FOwner: TPersistent;
    FParentWindow: HWND;
    FPidl: PItemIDList;
    FRootChangingCount: Integer;
    FShellFolder: IShellFolder;
    FUpdating: Boolean;
    FValid: Boolean;
    FOnFolderChanged: TcxRootChangedEvent;
    FOnSettingsChanged: TNotifyEvent;
    procedure SetBrowseFolder(Value: TcxBrowseFolder);
    procedure SetCustomPath(const Value: WideString);
    procedure SetPidl(const Value: PItemIDList);
    function GetCurrentPath: WideString;
    procedure UpdateFolder;
  protected
    procedure CheckRoot; virtual;
    procedure DoSettingsChanged;
    procedure RootUpdated; virtual;
    property Owner: TPersistent read FOwner;
    property ParentWindow: HWND read FParentWindow;
  public
    constructor Create(AOwner: TPersistent; AParentWindow: HWND); virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Update(ARoot: TcxCustomShellRoot);

    property Attributes: Cardinal read FAttributes;
    property CurrentPath: WideString read GetCurrentPath;
    property Folder: TcxShellFolder read FFolder;
    property IsValid: Boolean read FValid;
    property Pidl: PItemIDList read FPidl write SetPidl;
    property ShellFolder: IShellFolder read FShellFolder;
    property OnFolderChanged: TcxRootChangedEvent read FOnFolderChanged
      write FOnFolderChanged;
    property OnSettingsChanged: TNotifyEvent read FOnSettingsChanged
      write FOnSettingsChanged;
  published
    property BrowseFolder: TcxBrowseFolder read FBrowseFolder
      write SetBrowseFolder default bfDesktop;
    property CustomPath: WideString read FCustomPath write SetCustomPath;
  end;

  TcxShellItemInfo = class
  private
    FCanRename: Boolean;
    FData: Pointer;
    FDetails: TStrings;
    FFolder: TcxShellFolder;
    FFullPIDL: PItemIDList;
    FHasSubfolder: Boolean;
    FIconIndex: Integer;
    FInfoTip: WideString;
    FInitialized: Boolean;
    FIsDropTarget: Boolean;
    FIsFilesystem: Boolean;
    FIsFolder: Boolean;
    FIsGhosted: Boolean;
    FIsLink: Boolean;
    FIsRemovable: Boolean;
    FIsShare: Boolean;
    FItemProducer: TcxCustomItemProducer;
    FName: WideString;
    FOpenIconIndex: Integer;
    FOverlayIndex: Integer;
    Fpidl: PItemIDList;
    FProcessed: Boolean;
    FThumbnailIndex: Integer;
    FThumbnailUpdated: Boolean;
    FThumbnailUpdating: Boolean;
    FUpdated: Boolean;
    FUpdating: Boolean;
    function GetItemIndex: Integer;
    procedure UpdatePidl(AParentPidl, APidl: PItemIDList);
  protected
    property Updating: Boolean read FUpdating write FUpdating;
  public
    constructor Create(AItemProducer: TcxCustomItemProducer;
      AParentIFolder: IShellFolder; AParentPIDL, APIDL: PItemIDList;
      AFast: Boolean); virtual;
    destructor Destroy;override;
    procedure CheckUpdate(AShellFolder: IShellFolder; AFolderPidl: PItemIDList; AFast:Boolean);
    procedure CheckInitialize(AIFolder: IShellFolder; APIDL: PItemIDList);
    procedure FetchDetails(wnd:HWND;ShellFolder:IShellFolder;DetailsMap:TcxShellDetails);
    procedure CheckSubitems(AParentIFolder: IShellFolder;
      AEnumSettings: Cardinal);
    function GetOverlayIndex: Integer;
    function HasThumbnail: Boolean;
    procedure SetNewPidl(pFolder: IShellFolder; AFolderPidl, APidl: PItemIDList);
    procedure UpdateThumbnail;

    property CanRename: Boolean read FCanRename;
    property Data: Pointer read FData write FData;
    property Details: TStrings read FDetails;
    property Folder: TcxShellFolder read FFolder;
    property FullPIDL: PItemIDList read FFullPIDL;
    property HasSubfolder: Boolean read FHasSubfolder;
    property IconIndex:Integer read FIconIndex;
    property InfoTip:WideString read FInfoTip;
    property Initialized:Boolean read FInitialized;
    property IsDropTarget:Boolean read FIsDropTarget;
    property IsFilesystem:Boolean read FIsFilesystem;
    property IsFolder:Boolean read FIsFolder;
    property IsGhosted:Boolean read FIsGhosted;
    property IsLink:Boolean read FIsLink;
    property IsRemovable:Boolean read FIsRemovable;
    property IsShare:Boolean read FIsShare;
    property ItemIndex: Integer read GetItemIndex;
    property ItemProducer: TcxCustomItemProducer read FItemProducer;
    property Name: WideString read FName;
    property OpenIconIndex: Integer read FOpenIconIndex;
    property OverlayIndex: Integer read FOverlayIndex;
    property pidl: PItemIDList read Fpidl;
    property Processed: Boolean read FProcessed write FProcessed;
    property ThumbnailIndex: Integer read FThumbnailIndex;
    property ThumbnailUpdated: Boolean read FThumbnailUpdated;
    property Updated: Boolean read FUpdated write FUpdated;
  end;

  PcxShellItemInfo = TcxShellItemInfo;

  TFetchThread = class(TcxCustomThread)
  private
    FInfoGatherer: TcxShellItemsInfoGatherer;
    procedure LockStopFetch;
    procedure UnlockStopFetch;
  protected
    procedure Execute; override;
  public
    constructor Create(AInfoGatherer: TcxShellItemsInfoGatherer);
    destructor Destroy; override;
  end;

  { TcxShellItemsInfoGatherer }

  TcxShellItemsInfoGatherer = class
  private
    FQueuePopulated: TcxEvent;
    FFetchQueue: TThreadList;
    FFetchThread: TFetchThread;
    FIsFetchQueueClearing: Boolean;
    FLockFetchThread: TRTLCriticalSection;
    FOwner: TWinControl;
    FProcessedItems: TThreadList;
    FStopFetchCount: Integer;
    procedure CreateFetchThread;
    procedure StartRequest;
  protected
    procedure DestroyFetchThread;
    procedure LockFetchThread;
    procedure UnlockFetchThread;
    procedure WaitForRequest;
    property FetchQueue: TThreadList read FFetchQueue;
  public
    constructor Create(AOwner: TWinControl);
    destructor Destroy; override;
    procedure ClearFetchQueue(AItemProducer: TcxCustomItemProducer);
    procedure ClearVisibleItems;
    procedure RequestItemInfo(AItem: TcxShellItemInfo);
    procedure RequestItems(AItems: TList);
    procedure ResumeFetch;
    procedure StopFetch;
    property ProcessedItems: TThreadList read FProcessedItems;
  end;

  TcxCustomItemProducer = class
  private
    FDetails: TcxShellDetails;
    FFolderPidl: PItemIDList;
    FItems: TList;
    FItemsLock: TMultiReadExclusiveWriteSynchronizer;
    FOwner: TWinControl;
    FShellFolder: IShellFolder;
    FSortColumn: Integer;
    FSortDescending: Boolean;
    function GetFolderPidl: PItemIDList;
    procedure SetFolderPidl(AValue: PItemIDList);
    function GetItemInfo(AIndex: Integer): TcxShellItemInfo;
  protected
    function CanAddFolder(AFolder: TcxShellFolder): Boolean; virtual;
    function CreateShellItemInfo(APidl: PItemIDList; AFast: Boolean): TcxShellItemInfo;
    procedure DoSlowInitialization(AItem: TcxShellItemInfo); virtual; abstract;
    function DoCompareItems(AItem1, AItem2: TcxShellFolder; out ACompare: Integer): Boolean; virtual;
    procedure DoDestroy; virtual;
    procedure DoSort; virtual;
    function EnumerateItems: Boolean;
    procedure FetchItems(APreloadItems: Integer);
    function GetEnumerator(out pEnum: IEnumIDList): Boolean;
    function GetEnumFlags: Cardinal; virtual; abstract;
    function GetItemsInfoGatherer: TcxShellItemsInfoGatherer; virtual;
    function GetShowToolTip: Boolean; virtual; abstract;
    function GetThumbnailIndex(AItem: TcxShellItemInfo): Integer; virtual;
    procedure Initialize(AIFolder: IShellFolder; AFolderPIDL: PItemIDList);
    procedure InitializeItem(AItem: TcxShellItemInfo); virtual;
    function InternalAddItem(APidl: PItemIDList): TcxShellItemInfo;
    procedure CheckForSubitems(AItem: TcxShellItemInfo); virtual;
    procedure ClearFetchQueue;
    procedure PopulateItems(pEnum: IEnumIDList);
    function SlowInitializationDone(AItem: TcxShellItemInfo): Boolean; virtual; abstract;

    property ItemInfo[Index: Integer]: TcxShellItemInfo read GetItemInfo;
    property ItemsLock: TMultiReadExclusiveWriteSynchronizer read FItemsLock;
    property ShellFolder: IShellFolder read FShellFolder;
    property SortColumn: Integer read FSortColumn write FSortColumn;
    property SortDescending: Boolean read FSortDescending write FSortDescending;
    property FolderPidl: PItemIDList read GetFolderPidl write SetFolderPidl;
    property Owner: TWinControl read FOwner;
  public
    constructor Create(AOwner: TWinControl); virtual;
    destructor Destroy; override;
    procedure ProcessItems(AIFolder: IShellFolder; AFolderPIDL: PItemIDList;
      APreloadItemCount: Integer); virtual;
    procedure ProcessDetails(ShellFolder: IShellFolder; CharWidth: Integer); virtual;
    procedure FetchRequest(AItem: TcxShellItemInfo);
    procedure ClearItems;
    procedure LockRead;
    procedure LockWrite;
    procedure UnlockRead;
    procedure UnlockWrite;
    procedure SetItemsCount(Count: Integer); virtual;
    procedure NotifyRemoveItem(Index: Integer); virtual;
    procedure NotifyAddItem(Index: Integer); virtual;
    procedure DoGetInfoTip(Handle: HWND; ItemIndex: Integer; InfoTip: PChar; cch: Integer);
    function GetItemByPidl(APidl: PItemIDList): TcxShellItemInfo;
    function GetItemIndexByPidl(APidl: PItemIDList): Integer;
    procedure Sort;
    property Details: TcxShellDetails read FDetails;
    property Items: TList read FItems;
    property ItemsInfoGatherer: TcxShellItemsInfoGatherer read GetItemsInfoGatherer;
  end;

  TcxDragDropSettings = class(TPersistent)
  private
    FAllowDragObjects: Boolean;
    FDefaultDropEffect: TcxDropEffect;
    FDropEffect: TcxDropEffectSet;
    FScroll: Boolean;
    FOnChange: TNotifyEvent;
    function GetDefaultDropEffectAPI: Integer;
    function GetDropEffectAPI: DWORD;
    procedure SetAllowDragObjects(Value: Boolean);
  protected
    procedure Changed;
  public
    property DropEffectAPI: DWORD read GetDropEffectApi;
    property DefaultDropEffectAPI: Integer read GetDefaultDropEffectAPI;
    constructor Create;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property AllowDragObjects: Boolean read FAllowDragObjects
      write SetAllowDragObjects default True;
    property DefaultDropEffect: TcxDropEffect read FDefaultDropEffect
      write FDefaultDropEffect default deMove;
    property DropEffect: TcxDropEffectSet read FDropEffect write FDropEffect
      default [deCopy, deMove, deLink];
    property Scroll: Boolean read FScroll write FScroll stored False; // deprecated
  end;

  TShChangeNotifyEntry = packed record
    pidlPath: PItemIDList;
    bWatchSubtree: BOOL;
  end;

  DWORDITEMID=record
    cb: SHORT;
    dwItem1: DWORD;
    dwItem2: DWORD;
  end;

  PDWORDITEMID=^DWORDITEMID;

  PShChangeNotifyEntry = ^TShChangeNotifyEntry;

  TcxShellCustomContextMenu = class
  private
    FContextMenu: IContextMenu;
    FFirstInvokeShellCommandIndex: Cardinal;
    FMenu: HMenu;
  protected
    procedure DoAddDefaultShellItems;
    procedure DoPopup(APos: TPoint); virtual;
    procedure ExecuteMenuItemCommand(ACommandId: Cardinal); virtual;
    function GetWindowHandle: THandle; virtual; abstract;
    procedure Populate; virtual; abstract;

    property Menu: HMenu read FMenu;
    property WindowHandle: THandle read GetWindowHandle;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddDefaultShellItems(AIShellFolder: IShellFolder); overload;
    procedure AddDefaultShellItems(AIShellFolder: IShellFolder; AItemPIDLList: TList); overload;
    procedure Popup(APos: TPoint); virtual;
  end;

function GetDesktopIShellFolder: IShellFolder;
function GetTextFromStrRet(var AStrRet: TStrRet; APIDL: PitemIDList): WideString;
function GetShellDetails(pFolder:IShellFolder;pidl:PItemIDList;out sd:IShellDetails):Hresult;
function HasSubItems(AParentIFolder: IShellFolder; AFullPIDL: PItemIDList;
  AEnumSettings: Cardinal): Boolean;
function cxGetFolderLocation(AWnd: HWND; ACSIDL: Integer; AToken: THandle;
  AReserwed: DWORD; var APIDL: PItemIDList): HRESULT;
function cxFileTimeToDateTime(fTime:FILETIME):TDateTime;
function cxMalloc: IMalloc;
procedure DisplayContextMenu(AWnd: HWND; AIFolder: IShellFolder;
  AItemPIDLList: TList; const APos: TPoint);

{ Pidl Tools}

function GetPidlItemsCount(APidl: PItemIDList): Integer;
function GetPidlSize(APidl: PItemIDList): Integer;
function GetNextItemID(APidl: PItemIDList): PItemIDList;
function GetPidlCopy(APidl: PItemIDList): PItemIDList;
function GetLastPidlItem(APidl: PItemIDList): PItemIDList;
function GetPidlName(APIDL: PItemIDList): WideString;
function ConcatenatePidls(APidl1, APidl2: PItemIDList): PItemIDList;
procedure DisposePidl(APidl: PItemIDList);
function GetPidlParent(APidl: PItemIDList): PItemIDList;
function CreateEmptyPidl:PItemIDList;
function CreatePidlArrayFromList(AList: TList): PITEMIDLISTARRAY;
procedure DisposePidlArray(APidls: PITEMIDLISTARRAY);
function ExtractParticularPidl(APidl: PItemIDList): PItemIDList;
function EqualPIDLs(APIDL1, APIDL2: PItemIDList): Boolean;
function IsSubPath(APIDL1, APIDL2: PItemIDList): Boolean;

{ Unicode Tools }

procedure StrPLCopyW(Dest:PWideChar;Source:WideString;MaxLen:Cardinal);
function StrPasW(Source:PWideChar):WideString;
function StrLenW(Source:PWideChar):Cardinal;
function UpperCaseW(Source:WideString):WideString;
function LowerCaseW(Source:WideString):WideString;

procedure CheckShellRoot(ARoot: TcxCustomShellRoot);
function GetShellItemDisplayName(AIFolder: IShellFolder;
  APIDL: PItemIDList; ACheckIsFolder: Boolean): WideString;

function cxShellGetThreadSafeFileInfo(pszPath: PChar; dwFileAttributes: DWORD;
  var psfi: TSHFileInfo; cbFileInfo, uFlags: UINT): DWORD; stdcall;

const
  DSM_SETCOUNT                    = CM_BASE + 315;
  DSM_NOTIFYUPDATE                = CM_BASE + 316;
  DSM_NOTIFYREMOVEITEM            = CM_BASE + 318;
  DSM_NOTIFYADDITEM               = CM_BASE + 319;
  DSM_NOTIFYUPDATECONTENTS        = CM_BASE + 320;
  DSM_SHELLCHANGENOTIFY           = CM_BASE + 321;
  DSM_DONAVIGATE                  = CM_BASE + 322;
  DSM_SYNCHRONIZEROOT             = CM_BASE + 323;
  DSM_SHELLTREECHANGENOTIFY       = CM_BASE + 324;
  DSM_SHELLTREERESTORECURRENTPATH = CM_BASE + 325;
  DSM_SYSTEMSHELLCHANGENOTIFY     = CM_BASE + 326;

  DSM_FIRST = DSM_SETCOUNT;
  DSM_LAST  = DSM_SYSTEMSHELLCHANGENOTIFY;

  PRELOAD_ITEMS_COUNT = 10;

  SHCNF_ACCEPT_INTERRUPTS =     $1;
  SHCNF_ACCEPT_NON_INTERRUPTS = $2;
  SHCNF_NO_PROXY =              $8000;

type
  TPidlList = array [0..1] of PItemIDList;
  PPidlList = ^TPidlList;

var
  SHChangeNotifyRegister: function (hwnd: HWND; dwFlags: DWORD; wEventMask: DWORD;
    uMsg: UINT; cItems: DWORD; lpItems: PShChangeNotifyEntry): Cardinal; stdcall;
  SHChangeNotifyUnregister: function (hNotify: Cardinal): Boolean; stdcall;
  SHChangeNotification_Lock: function (hChange: THandle; dwProcId: DWORD;
    var PPidls: PPidlList; var plEvent: Longint): THandle; stdcall;
  SHChangeNotification_UnLock: function (hLock: THandle): BOOL; stdcall;
  SHGetImageList: function (iImageList: Integer; const riid: TIID; out ppv): HResult; stdcall;

implementation

uses
  cxContainer, cxControls, cxEdit, dxUxTheme, dxCore, Contnrs, ComCtrls, CommCtrl;

const
  SFGAO_ENCRYPTED       = $00002000;
  SFGAO_ISSLOW          = $00004000;
  SFGAO_STORAGE         = $00000008;
  SFGAO_STORAGEANCESTOR = $00800000;
  SFGAO_STORAGECAPMASK  = $70C50008;
  SFGAO_STREAM          = $00400000;

type
  TcxContextMenuMessageWindow = class(TcxMessageWindow)
  private
    FContextMenu: IContextMenu2;
  protected
    procedure WndProc(var Message: TMessage); override;
  public
    property ContextMenu: IContextMenu2 read FContextMenu write FContextMenu;
  end;

  TSHGetPathFromIDList = function(APIDL: PItemIDList; APath: PChar): BOOL; stdcall;
  TSHGetPathFromIDListW = function(APIDL: PItemIDList; APath: PWideChar): BOOL; stdcall;

  cxIShellFolder2 = interface(IShellFolder2)
    ['{93F2F68C-1D1B-11D3-A30E-00C04F79ABD1}']
  end;

var
  FComInitializationSucceeded: Boolean;
  FSysFileIconIndex: Integer = -1;
  FSysFolderIconIndex: Integer = -1;
  FSysFolderOpenIconIndex: Integer = -1;
  cxSHGetFolderLocation: function (wnd: HWND; nFolder: Integer; hToken: THandle;
    dwReserwed: DWORD; var ppidl: PItemIDList): HResult; stdcall;
  cxSHGetPathFromIDList: TSHGetPathFromIDList = nil;
  cxSHGetPathFromIDListW: TSHGetPathFromIDListW = nil;
  dxILIsParent: function (pidl1: PItemIDList; pidl2: PItemIDList;
    fImmediate: BOOL): BOOL; stdcall;
  ShellLibrary: HMODULE = 0;
  FcxMalloc: IMalloc;
  FShellItemsInfoGatherers: TList;
  FShellLock: TCriticalSection;

function ShellSortFunction(Item1, Item2: Pointer): Integer;
const
  R: array[Boolean] of Byte = (0, 1);
var
  AItemInfo1, AItemInfo2: TcxShellItemInfo;
  AItemProducer: TcxCustomItemProducer;
begin
  AItemInfo1 := TcxShellItemInfo(Item1);
  AItemInfo2 := TcxShellItemInfo(Item2);
  AItemProducer := AItemInfo1.ItemProducer;
  if not AItemProducer.DoCompareItems(AItemInfo1.Folder, AItemInfo2.Folder, Result) then
  begin
    Result := R[AItemInfo2.IsFolder] - R[AItemInfo1.IsFolder];
    if Result = 0 then
      Result := SmallInt(AItemProducer.ShellFolder.CompareIDs(
        AItemProducer.FSortColumn, AItemInfo1.pidl, AItemInfo2.pidl));
    if AItemProducer.FSortDescending then
      Result := -Result;
  end;
end;

procedure CheckShellRoot(ARoot: TcxCustomShellRoot);
begin
  if ARoot.ShellFolder = nil then
    ARoot.CheckRoot;
end;

function GetShellItemDisplayName(AIFolder: IShellFolder;
  APIDL: PItemIDList; ACheckIsFolder: Boolean): WideString;
var
  AStrRet: TStrRet;
begin
  if Succeeded(AIFolder.GetDisplayNameOf(APIDL, SHGDN_INFOLDER, AStrRet)) then
    Result := GetTextFromStrRet(AStrRet, APIDL)
  else
    Result := '';
end;

function cxShellGetThreadSafeFileInfo(pszPath: PChar; dwFileAttributes: DWORD;
  var psfi: TSHFileInfo; cbFileInfo, uFlags: UINT): DWORD;
begin
  FShellLock.Enter;
  Result := SHGetFileInfo(pszPath, dwFileAttributes, psfi, cbFileInfo, uFlags);
  FShellLock.Leave;
end;

function HasSubItems(AParentIFolder: IShellFolder; AFullPIDL: PItemIDList;
  AEnumSettings: Cardinal): Boolean;

  function HasAttributes(AAttributes: UINT): Boolean;
  var
    ATempAttributes: UINT;
    ATempPIDL: PItemIDList;
  begin
    ATempAttributes := AAttributes;
    ATempPIDL := GetLastPidlItem(AFullPIDL);
    AParentIFolder.GetAttributesOf(1, ATempPIDL, ATempAttributes);
    Result := ATempAttributes and AAttributes = AAttributes;
  end;

  function CheckLocalFolder(out AHasSubItems: Boolean): Boolean;
  var
    AAttributes, AParsedCharCount: ULONG;
    AFileSearchAttributes: Integer;
    ATempPIDL: PItemIDList;
    ASearchRec: TSearchRec;
    S: WideString;
  begin
    Result := False;
    S := GetPidlName(AFullPIDL);
    if (S = '')(* or (Pos('\\', S) = 1)*) then
      Exit;
    AAttributes := 0;
    GetDesktopIShellFolder.ParseDisplayName(0, nil, PWideChar(S),
      AParsedCharCount, ATempPIDL, AAttributes);
    if ATempPIDL = nil then
      Exit;
    try
      Result := True;
      AHasSubItems := False;

      AFileSearchAttributes := faReadOnly or faSysFile or faArchive;
      if AEnumSettings and SHCONTF_FOLDERS <> 0 then
        AFileSearchAttributes := AFileSearchAttributes or faDirectory;
      if AEnumSettings and SHCONTF_INCLUDEHIDDEN <> 0 then
        AFileSearchAttributes := AFileSearchAttributes or faHidden;

      if S[Length(S)] = PathDelim then
        Delete(S, Length(S), 1);

      if FindFirst(S + PathDelim + '*.*', AFileSearchAttributes, ASearchRec) = 0 then
      begin
        repeat
          AHasSubItems := (ASearchRec.Name <> '.') and (ASearchRec.Name <> '..');
        until (FindNext(ASearchRec) <> 0) or AHasSubItems;
        FindClose(ASearchRec);
      end;
    finally
      DisposePidl(ATempPIDL);
    end;
  end;

var
  ATempIFolder: IShellFolder;
  ATempPIDL: PItemIDList;
  AIEnum: IEnumIDList;
  AFetchedItemCount: Cardinal;
begin
  Result := HasAttributes(SFGAO_FOLDER) and Succeeded(GetDesktopIShellFolder.BindToObject(
    AFullPIDL, nil, IID_IShellFolder, ATempIFolder));
  if Result then
    if AEnumSettings and SHCONTF_NONFOLDERS = 0 then
      Result := HasAttributes(SFGAO_HASSUBFOLDER)
    else
      if not CheckLocalFolder(Result) then
      begin
        Result := HasAttributes(SFGAO_HASSUBFOLDER);
        if not Result and (ATempIFolder <> nil) and Succeeded(ATempIFolder.EnumObjects(0, AEnumSettings, AIEnum)) and
          Assigned(AIEnum) and (AIEnum.Next(1, ATempPIDL, AFetchedItemCount) = S_OK) then
          try
            Result := AFetchedItemCount = 1;
          finally
            DisposePidl(ATempPIDL);
          end;
      end;
end;

function GetDesktopIShellFolder: IShellFolder;
begin
  OleCheck(SHGetDesktopFolder(Result));
end;

function GetTextFromStrRet(var AStrRet: TStrRet; APIDL: PitemIDList): WideString;
var
  P: PChar;
  ATmp: PChar;
begin
  case AStrRet.uType of
    STRRET_CSTR:
      begin
        ATmp := PChar(dxAnsiStringToString(AStrRet.cStr));
        SetString(Result, ATmp, lstrlen(ATmp));
      end;
    STRRET_OFFSET:
      begin
        P := @APIDL.mkid.abID;
        Inc(P, AStrRet.uOffset - SizeOf(APIDL.mkid.cb));
        SetString(Result, P, APIDL.mkid.cb - AStrRet.uOffset);
      end;
    STRRET_WSTR:
      begin
        Result := StrPasW(AStrRet.pOleStr);
        cxMalloc.Free(AStrRet.pOleStr);
      end;
  end;
end;

function GetShellDetails(pFolder:IShellFolder;pidl:PItemIDList;out sd:IShellDetails):Hresult;
begin
  try
    Result := pFolder.QueryInterface(IID_IShellDetails, sd);
    if Result = S_OK then
       Exit;
    Result:=pFolder.GetUIObjectOf(0,0,pidl,IID_IShellDetails,nil,sd);
    if Result = S_OK then
       Exit;
    Result:=pFolder.CreateViewObject(0,IID_IShellDetails,sd);
    if Result = S_OK then
       Exit;
    Result:=pFolder.GetUIObjectOf(0,Integer(pidl<>nil)(*1*),pidl,IID_IShellDetails,nil,sd);
  finally
    if sd = nil then
      Result := E_NOINTERFACE;
  end;
end;

function cxGetFolderLocation(AWnd: HWND; ACSIDL: Integer; AToken: THandle;
  AReserwed: DWORD; var APIDL: PItemIDList): HRESULT;
begin
  if Win32MajorVersion < 5 then
    Result := SHGetSpecialFolderLocation(AWnd, ACSIDL, APIDL)
  else
    Result := cxSHGetFolderLocation(AWnd, ACSIDL, AToken, AReserwed, APIDL);
end;

function cxFileTimeToDateTime(fTime:FILETIME):TDateTime;
var
  LocalTime:TFileTime;
  Age:Integer;
begin
  FileTimeToLocalFileTime(FTime,LocalTime);
  if FileTimeToDosDateTime(LocalTime,LongRec(Age).Hi,LongRec(Age).Lo) then
     Result:=FileDateToDateTime(Age)
  else
     Result:=-1;
end;

function cxMalloc: IMalloc;
begin
  if FcxMalloc = nil then
    SHGetMalloc(FcxMalloc);
  Result := FcxMalloc;
end;

procedure TcxContextMenuMessageWindow.WndProc(var Message: TMessage);
begin
  case Message.Msg of
    WM_INITMENUPOPUP:
      begin
        ContextMenu.HandleMenuMsg(Message.Msg, Message.wParam, Message.lParam);
        Message.Result := 0;
      end;
    WM_DRAWITEM, WM_MEASUREITEM:
      begin
        ContextMenu.HandleMenuMsg(Message.Msg, Message.wParam, Message.lParam);
        Message.Result := 1;
      end;
    else
      inherited WndProc(Message);
  end;
end;

function CreateCallbackWnd(AContextMenu: IContextMenu2): TcxContextMenuMessageWindow;
begin
  Result := TcxContextMenuMessageWindow.Create;
  Result.ContextMenu := AContextMenu;
end;

type
  TcxShellContextMenuHelper = class(TcxShellCustomContextMenu)
  private
    FFolder: IShellFolder;
    FItemPIDLList: TList;
    FWnd: HWND;
  protected
    function GetWindowHandle: THandle; override;
    procedure Populate; override;
  public
    constructor Create(AWnd: HWND; AIFolder: IShellFolder;
      AItemPIDLList: TList);
  end;

{ TcxShellContextMenuHelper }

constructor TcxShellContextMenuHelper.Create(AWnd: HWND; AIFolder: IShellFolder;
  AItemPIDLList: TList);
begin
  inherited Create;
  FWnd := AWnd;
  FItemPIDLList := AItemPIDLList;
  FFolder := AIFolder;
end;

function TcxShellContextMenuHelper.GetWindowHandle: THandle;
begin
  Result := FWnd;
end;

procedure TcxShellContextMenuHelper.Populate;
begin
  AddDefaultShellItems(FFolder, FItemPIDLList);
end;

procedure DisplayContextMenu(AWnd: HWND; AIFolder: IShellFolder;
  AItemPIDLList: TList; const APos: TPoint);
begin
  if (AIFolder <> nil) and (AItemPIDLList.Count <> 0) then
    with TcxShellContextMenuHelper.Create(AWnd, AIFolder, AItemPIDLList) do
    begin
      Popup(APos);
      Free;
    end;
end;

function SysFileIconIndex: Integer;
var
  AFileInfo: TSHFileInfo;
begin
  if FSysFileIconIndex = -1 then
  begin
    cxShellGetThreadSafeFileInfo('C:\CXDUMMYFILE.TXT', FILE_ATTRIBUTE_NORMAL, AFileInfo,
      SizeOf(AFileInfo), SHGFI_SYSICONINDEX or SHGFI_USEFILEATTRIBUTES);
    FSysFileIconIndex := AFileInfo.iIcon;
  end;
  Result := FSysFileIconIndex;
end;

function SysFolderIconIndex: Integer;
var
  AFileInfo: TSHFileInfo;
begin
  if FSysFolderIconIndex = -1 then
  begin
    cxShellGetThreadSafeFileInfo('C:\CXDUMMYFOLDER', FILE_ATTRIBUTE_DIRECTORY, AFileInfo,
      SizeOf(AFileInfo), SHGFI_SYSICONINDEX or SHGFI_USEFILEATTRIBUTES);
    FSysFolderIconIndex := AFileInfo.iIcon;
  end;
  Result := FSysFolderIconIndex;
end;

function SysFolderOpenIconIndex: Integer;
var
  AFileInfo: TSHFileInfo;
begin
  if FSysFolderOpenIconIndex = -1 then
  begin
    cxShellGetThreadSafeFileInfo('C:\CXDUMMYFOLDER', FILE_ATTRIBUTE_DIRECTORY, AFileInfo,
      SizeOf(AFileInfo), SHGFI_SYSICONINDEX or SHGFI_USEFILEATTRIBUTES or SHGFI_OPENICON);
    FSysFolderOpenIconIndex := AFileInfo.iIcon;
  end;
  Result := FSysFolderOpenIconIndex;
end;

{ Unicode Tools }

function UpperCaseW(Source: WideString):WideString;
begin
  Result := AnsiUpperCase(Source);
end;

function LowerCaseW(Source:WideString):WideString;
begin
  Result := AnsiLowerCase(Source);
end;

function StrLenW(Source: PWideChar): Cardinal;
begin
  Result := StrLen(Source);
end;

function StrPasW(Source: PWideChar): WideString;
var
  StringLength:Cardinal;
begin
  StringLength:=StrLenW(Source);
  SetLength(Result,StringLength);
  CopyMemory(Pointer(Result),Source,StringLength*2);
end;

procedure StrPLCopyW(Dest:PWideChar;Source:WideString;MaxLen:Cardinal);
begin
  lstrcpynw(Dest,PWideChar(Source),MaxLen);
end;

{ PidlTools}

function GetPidlParent(APidl: PItemIDList): PItemIDList;
var
  ASourceSize: Integer;
  APrevPidl: PItemIDList;
  AInitialPidl: PItemIDList;
begin
  Result := nil;
  ASourceSize := 0;
  AInitialPidl := APidl;
  APrevPidl := nil;
  if APidl <> nil then
  begin
    while APidl.mkid.cb <> 0 do
    begin
      Inc(ASourceSize, APidl.mkid.cb);
      APrevPidl := APidl;
      APidl := GetNextItemID(APidl);
    end;
    if ASourceSize > 0 then
      Dec(ASourceSize,APrevPidl.mkid.cb);
    Result := cxMalloc.Alloc(ASourceSize + 2);
    ZeroMemory(Result, ASourceSize + 2);
    CopyMemory(Result, AInitialPidl, ASourceSize);
  end;
end;

function CreateEmptyPidl:PItemIDList;
begin
  Result:=cxMalloc.Alloc(SizeOf(ITEMIDLIST));
  Result.mkid.cb:=0;
  Result.mkid.abID[0]:=0;
end;

function CreatePidlArrayFromList(AList: TList): PITEMIDLISTARRAY;
var
  I: Integer;
begin
  Result := nil;
  if AList = nil then
    Exit;
  Result := cxMalloc.Alloc(AList.Count * SizeOf(Pointer));
  for I := 0 to AList.Count - 1 do
    Result[I] := AList[I];
end;

procedure DisposePidlArray(APidls: PITEMIDLISTARRAY);
begin
  if APidls <> nil then
     cxMalloc.Free(APidls);
end;

function ExtractParticularPidl(APidl: PItemIDList): PItemIDList;
begin
  Result := nil;
  if (APidl <> nil) and (APidl.mkid.cb <> 0) then
  begin
    Result := cxMalloc.Alloc(APidl.mkid.cb + 2);
    ZeroMemory(Result, APidl.mkid.cb + 2);
    CopyMemory(Result, APidl, APidl.mkid.cb);
  end;
end;

function EqualPIDLs(APIDL1, APIDL2: PItemIDList): Boolean;
var
  L1, L2: Integer;
begin
  Result := APIDL1 = APIDL2;
  if not Result then
    if (APIDL1 = nil) or (APIDL2 = nil) then
      Exit
    else
    begin
      L1 := GetPidlSize(APIDL1);
      L2 := GetPidlSize(APIDL2);
      Result := (L1 = L2) and CompareMem(APIDL1, APIDL2, L1);
    end;
end;

function IsSubPath(APIDL1, APIDL2: PItemIDList): Boolean; // TODO
var
  L1, L2: Integer;
begin
 if Assigned(dxILIsParent) then
   Result := dxILIsParent(APIDL1, APIDL2, False)
 else
 begin
    L1 := GetPidlSize(APIDL1);
    L2 := GetPidlSize(APIDL2);
    Result := (L1 = 0) or (L2 >= L1) and CompareMem(APIDL1, APIDL2, L1);
 end;
end;

function ConcatenatePidls(APidl1, APidl2: PItemIDList): PItemIDList;
var
  cb1,cb2:Integer;
begin
  if (APidl1 = nil) and (APidl2 = nil) then
    Result := nil
  else
    if APidl1 = nil then
      Result := GetPidlCopy(APidl2)
    else
      if APidl2 = nil then
        Result := GetPidlCopy(APidl1)
      else
      begin
        cb1 := GetPidlSize(APidl1);
        cb2 := GetPidlSize(APidl2) + 2;
        Result := cxMalloc.Alloc(cb1 + cb2);
        if Result <> nil then
        begin
          CopyMemory(Result, APidl1, cb1);
          CopyMemory(ShiftPointer(Result, cb1), APidl2, cb2);
        end;
      end;
end;

function GetPidlName(APIDL: PItemIDList): WideString;
var
  P: PChar;
  PW: PWideChar;
begin
  Result := '';
  if APIDL = nil then
    Exit;
  if not Assigned(cxSHGetPathFromIDListW) then
  begin
    GetMem(P, MAX_PATH + 1);
    try
      cxSHGetPathFromIDList(APIDL, P);
      Result := StrPas(P);
    finally
      FreeMem(P);
    end;
  end
  else
  begin
    GetMem(PW, (MAX_PATH + 1) * 2);
    try
      cxSHGetPathFromIDListW(APIDL, PW);
      Result := StrPasW(PW);
    finally
      FreeMem(PW);
    end;
  end;
end;

function GetLastPidlItem(APidl: PItemIDList): PItemIDList;
var
  ATempPidl: PItemIDList;
begin
  Result := APidl;
  if APidl <> nil then
  begin
    ATempPidl := APidl;
    while ATempPidl.mkid.cb <> 0 do
    begin
      Result := ATempPidl;
      ATempPidl := GetNextItemID(ATempPidl);
    end;
  end;
end;

procedure DisposePidl(APidl: PItemIDList);
begin
  if APidl <> nil then
    cxMalloc.Free(APidl);
end;

function GetPidlCopy(APidl: PItemIDList): PItemIDList;
var
  ASize: Integer;
begin
  Result := nil;
  if APidl <> nil then
  begin
    ASize := GetPidlSize(APidl) + 2;
    Result := cxMalloc.Alloc(ASize);
    CopyMemory(Result, APidl, ASize);
  end;
end;

function GetPidlItemsCount(APidl: PItemIDList): Integer;
begin
  Result := 0;
  if APidl <> nil then
    while APidl.mkid.cb <> 0 do
    begin
      Inc(Result);
      APidl := GetNextItemID(APidl);
      if Result > MAX_PATH then
      begin
        Result := -1;
        Break;
      end;
    end;
end;

function GetPidlSize(APidl: PItemIDList):Integer;
begin
  Result := 0;
  while (APidl <> nil) and (APidl.mkid.cb <> 0) do
  begin
    Inc(Result, APidl.mkid.cb);
    APidl := GetNextItemID(APidl);
  end;
end;

function GetNextItemID(APidl: PItemIDList): PItemIDList;
begin
  Result := nil;
  if (APidl <> nil) and (APidl.mkid.cb <> 0) then
     Result := PItemIDList(ShiftPointer(APidl, APidl.mkid.cb));
end;

procedure RegisterShellItemsInfoGatherer(AGatherer: TcxShellItemsInfoGatherer);
begin
  if FShellItemsInfoGatherers = nil then
    FShellItemsInfoGatherers := TList.Create;
  FShellItemsInfoGatherers.Add(AGatherer);
end;

procedure UnregisterShellItemsInfoGatherer(AGatherer: TcxShellItemsInfoGatherer);
begin
  FShellItemsInfoGatherers.Remove(AGatherer);
  if FShellItemsInfoGatherers.Count = 0 then
    FreeAndNil(FShellItemsInfoGatherers);
end;

{ TcxCustomShellRoot }

procedure TcxCustomShellRoot.CheckRoot;
const
  ACSIDLs: array[TcxBrowseFolder] of Integer = (
    CSIDL_DESKTOP, CSIDL_STARTUP, CSIDL_BITBUCKET, CSIDL_COMMON_DESKTOPDIRECTORY,
    CSIDL_COMMON_DOCUMENTS, CSIDL_COMMON_FAVORITES, CSIDL_COMMON_PROGRAMS,
    CSIDL_COMMON_STARTMENU, CSIDL_COMMON_STARTUP, CSIDL_COMMON_TEMPLATES,
    CSIDL_CONTROLS, CSIDL_DESKTOP, CSIDL_DESKTOPDIRECTORY, CSIDL_DRIVES,
    CSIDL_PRINTERS, CSIDL_FAVORITES, CSIDL_FONTS, CSIDL_HISTORY, CSIDL_MYMUSIC,
    CSIDL_MYPICTURES, CSIDL_NETHOOD, CSIDL_PROFILE, CSIDL_PROGRAM_FILES,
    CSIDL_PROGRAMS, CSIDL_RECENT, CSIDL_STARTMENU, CSIDL_STARTUP, CSIDL_TEMPLATES,
    CSIDL_PERSONAL, CSIDL_NETWORK);
var
  ABrowseFolder: TcxBrowseFolder;
  ADesktopFolder: IShellFolder;
  AParsedCharCount, AAttributes: Cardinal;
  ATempCustomPath: PWideChar;
  ATempPIDL: PItemIDList;
begin
  if FIsRootChecking then
    Exit;

  ADesktopFolder := GetDesktopIShellFolder;
  ATempPIDL := nil;
  FValid := False;
  FShellFolder := nil;
  if FPidl <> nil then
  begin
    DisposePidl(FPidl);
    FPidl := nil;
  end;

  ABrowseFolder := BrowseFolder;
  if (ABrowseFolder = bfCustomPath) and not DirectoryExists(CustomPath) then
    ABrowseFolder := bfDesktop;

  FIsRootChecking := True;
  try
    try
      if ABrowseFolder = bfCustomPath then
      begin
        ATempCustomPath := PWideChar(CustomPath);
        OleCheck(ADesktopFolder.ParseDisplayName(ParentWindow, nil,
          ATempCustomPath, AParsedCharCount, ATempPIDL, AAttributes));
      end
      else
        OleCheck(cxGetFolderLocation(ParentWindow, ACSIDLs[ABrowseFolder], 0, 0, ATempPIDL));
    except
      on E: Exception do
        if FRootChangingCount > 0 then
          raise EcxEditError.Create(E.Message)
        else
        begin
          RootUpdated;
          Exit;
        end;
    end;
    if ABrowseFolder = bfDesktop then
    begin
      FShellFolder := ADesktopFolder;
      FPidl := GetPidlCopy(ATempPIDL);
      FValid := True;
      FAttributes := SFGAO_FILESYSTEM;
      RootUpdated;
    end
    else
      Pidl := ATempPIDL;
  finally
    FIsRootChecking := False;
    if ATempPIDL <> nil then
      DisposePidl(ATempPIDL);
  end;
end;

procedure TcxCustomShellRoot.DoSettingsChanged;
begin
  if not FUpdating and Assigned(FOnSettingsChanged) then
    FOnSettingsChanged(Self);
end;

procedure TcxCustomShellRoot.RootUpdated;
begin
  UpdateFolder;
  if Assigned(FOnFolderChanged) then
     FOnFolderChanged(FOwner, Self);
end;

procedure TcxCustomShellRoot.SetPidl(const Value: PItemIDList);
var
  AFolder: IShellFolder;
begin
  if Value = nil then
     Exit;
  if FPidl <> nil then
  begin
    DisposePidl(FPidl);
    FPidl := nil;
    FValid := False;
    FAttributes := 0;
  end;
  if Succeeded(GetDesktopIShellFolder.BindToObject(Value, nil, IID_IShellFolder, AFolder)) then
  begin
    FShellFolder := AFolder;
    FPidl := GetPidlCopy(Value);
    FValid := True;
    FAttributes := 0;
    if Failed(GetDesktopIShellFolder.GetAttributesOf(1, FPidl, FAttributes)) then
       FAttributes := 0;
  end
  else
  begin
    FShellFolder := GetDesktopIShellFolder;
    FPidl := GetPidlCopy(Value);
    FValid := True;
    FAttributes := SFGAO_FILESYSTEM;
  end;
  RootUpdated;
end;

constructor TcxCustomShellRoot.Create(AOwner: TPersistent; AParentWindow: HWND);
begin
  inherited Create;
  FOwner := AOwner;
  FParentWindow := AParentWindow;
  FBrowseFolder := bfDesktop;
  FCustomPath := '';
  FShellFolder := nil;
  FPidl := nil;
end;

destructor TcxCustomShellRoot.Destroy;
begin
  FreeAndNil(FFolder);
  FShellFolder := nil;
  DisposePidl(FPidl);
  inherited;
end;

procedure TcxCustomShellRoot.Assign(Source: TPersistent);
var
  APrevBrowseFolder: TcxBrowseFolder;
  APrevCustomPath: WideString;
begin
  if Source is TcxCustomShellRoot then
  begin
    APrevBrowseFolder := FBrowseFolder;
    APrevCustomPath := FCustomPath;
    try
      FBrowseFolder := TcxCustomShellRoot(Source).FBrowseFolder;
      FCustomPath := TcxCustomShellRoot(Source).FCustomPath;
      Inc(Self.FRootChangingCount);
      try
        CheckRoot;
      finally
        Dec(FRootChangingCount);
      end;
      DoSettingsChanged;
    except
      FBrowseFolder := APrevBrowseFolder;
      FCustomPath := APrevCustomPath;
      CheckRoot;
      raise;
    end;
  end
  else
    inherited Assign(Source);
end;

procedure TcxCustomShellRoot.Update(ARoot: TcxCustomShellRoot);
begin
  if FUpdating then
    Exit;
  FUpdating := True;
  try
    Assign(ARoot);
  finally
    FUpdating := False;
  end;
end;

procedure TcxCustomShellRoot.SetBrowseFolder(Value: TcxBrowseFolder);
var
  APrevBrowseFolder: TcxBrowseFolder;
begin
  APrevBrowseFolder := FBrowseFolder;
  try
    Inc(FRootChangingCount);
    try
      if FBrowseFolder <> Value then
      begin
        FBrowseFolder := Value;
        CheckRoot;
      end
      else
        if Pidl = nil then
          CheckRoot;
    finally
      Dec(FRootChangingCount);
    end;
    DoSettingsChanged;
  except
    FBrowseFolder := APrevBrowseFolder;
    CheckRoot;
    raise;
  end;
end;

procedure TcxCustomShellRoot.SetCustomPath(const Value: WideString);
var
  APrevCustomPath: WideString;
begin
  APrevCustomPath := FCustomPath;
  try
    FCustomPath := Value;
    Inc(FRootChangingCount);
    try
      if BrowseFolder = bfCustomPath then
        CheckRoot;
    finally
      Dec(FRootChangingCount);
    end;
    DoSettingsChanged;
  except
    FCustomPath := APrevCustomPath;
    CheckRoot;
    raise;
  end;
end;

function TcxCustomShellRoot.GetCurrentPath: WideString;
var
  AStrName: TStrRet;
begin
  Result := '';
  if (Pidl <> nil) and
    Succeeded(GetDesktopIShellFolder.GetDisplayNameOf(Pidl, SHGDN_NORMAL or SHGDN_FORPARSING, AStrName)) then
    Result := GetTextFromStrRet(AStrName, Pidl);
end;

procedure TcxCustomShellRoot.UpdateFolder;
begin
  FreeAndNil(FFolder);
  FFolder := TcxShellFolder.Create(PIDL);
end;

{ TcxCustomItemProducer }

procedure TcxCustomItemProducer.ClearItems;

  (*function HasItems: Boolean;
  begin
    LockRead;
    try
      Result := Items.Count <> 0;
    finally
      UnlockRead;
    end;
  end;*)

var
  I: Integer;
begin
  //if HasItems then
  begin
    ClearFetchQueue;
    for I := 0 to Items.Count - 1 do
      ItemInfo[I].Free;
    Items.Clear;
  end;
  FShellFolder := nil;
  FolderPidl := nil;
end;

constructor TcxCustomItemProducer.Create(AOwner: TWinControl);
begin
  inherited Create;
  FOwner := AOwner;
  FDetails := TcxShellDetails.Create;
  FItems := TList.Create;
  FItemsLock := TMultiReadExclusiveWriteSynchronizer.Create;
end;

destructor TcxCustomItemProducer.Destroy;
begin
  DoDestroy;
  inherited Destroy;
end;

function TcxCustomItemProducer.CreateShellItemInfo(
  APidl: PItemIDList; AFast: Boolean): TcxShellItemInfo;
begin
  Result := TcxShellItemInfo.Create(Self, ShellFolder, FFolderPidl, APidl, AFast);
end;

procedure TcxCustomItemProducer.LockRead;
begin
  ItemsLock.BeginRead;
end;

procedure TcxCustomItemProducer.LockWrite;
begin
  ItemsLock.BeginWrite;
end;

procedure TcxCustomItemProducer.ProcessItems(AIFolder: IShellFolder;
  AFolderPIDL: PItemIDList; APreloadItemCount: Integer);
begin
  Initialize(AIFolder, AFolderPIDL);
  ProcessDetails(ShellFolder, APreloadItemCount);
  FetchItems(APreloadItemCount);
end;

procedure TcxCustomItemProducer.SetItemsCount(Count: Integer);
begin
  if Owner.HandleAllocated then
     SendMessage(Owner.Handle, DSM_SETCOUNT, Count, 0);
end;

procedure TcxCustomItemProducer.UnlockRead;
begin
  ItemsLock.EndRead;
end;

procedure TcxCustomItemProducer.UnlockWrite;
begin
  ItemsLock.EndWrite;
end;

procedure TcxCustomItemProducer.NotifyRemoveItem(Index: Integer);
begin
  if Owner.HandleAllocated then
     SendMessage(Owner.Handle, DSM_NOTIFYREMOVEITEM, Index, 0);
end;

procedure TcxCustomItemProducer.NotifyAddItem(Index: Integer);
begin
  if Owner.HandleAllocated then
     SendMessage(Owner.Handle, DSM_NOTIFYADDITEM, Index, 0);
end;

function TcxCustomItemProducer.GetItemsInfoGatherer: TcxShellItemsInfoGatherer;
begin
  Result := nil;
end;

function TcxCustomItemProducer.GetThumbnailIndex(AItem: TcxShellItemInfo): Integer;
begin
  Result := -1;
end;

procedure TcxCustomItemProducer.Initialize(AIFolder: IShellFolder;
  AFolderPIDL: PItemIDList);
begin
  FShellFolder := AIFolder;
  FolderPidl := AFolderPIDL;
end;

procedure TcxCustomItemProducer.InitializeItem(AItem: TcxShellItemInfo);
begin
  AItem.CheckUpdate(ShellFolder, FolderPidl, False);
end;

function TcxCustomItemProducer.InternalAddItem(APidl: PItemIDList): TcxShellItemInfo;
begin
  Result := CreateShellItemInfo(APidl, False);
  if (Result.Name <> '') and CanAddFolder(Result.Folder) then
    Items.Add(Result)
  else
    FreeAndNil(Result);
end;

function TcxCustomItemProducer.CanAddFolder(AFolder: TcxShellFolder): Boolean;
begin
  Result := True;
end;

function TcxCustomItemProducer.DoCompareItems(AItem1, AItem2: TcxShellFolder;
  out ACompare: Integer): Boolean;
begin
  Result := False;
end;

procedure TcxCustomItemProducer.DoDestroy;
begin
  ClearItems;
  FreeAndNil(FDetails);
  FreeAndNil(FItems);
  FreeAndNil(FItemsLock);
end;

procedure TcxCustomItemProducer.DoSort;
begin
  Items.Sort(@ShellSortFunction);
end;

function TcxCustomItemProducer.EnumerateItems: Boolean;
var
  pEnum: IEnumIDList;
begin
  Result := GetEnumerator(pEnum);
  if Result then
  begin
    PopulateItems(pEnum);
    Sort;
  end;
end;

procedure TcxCustomItemProducer.FetchItems(APreloadItems: Integer);

  procedure InitializeItems;
  var
    I: Integer;
  begin
    for I := 0 to Min(APreloadItems - 1, Items.Count - 1) do
      InitializeItem(Items[I]);
  end;

begin
  ShowHourglassCursor;
  LockWrite;
  try
    EnumerateItems;
    InitializeItems;
    SetItemsCount(Items.Count);
  finally
    UnlockWrite;
    HideHourglassCursor;
  end;
end;

function TcxCustomItemProducer.GetEnumerator(out pEnum: IEnumIDList): Boolean;
begin
  Result := (ShellFolder <> nil) and Succeeded(ShellFolder.EnumObjects(Owner.ParentWindow,
    GetEnumFlags, pEnum)) and Assigned(pEnum);
end;

procedure TcxCustomItemProducer.ProcessDetails(ShellFolder: IShellFolder; CharWidth: Integer);
var
  Attr: Cardinal;
  ATempPidl: PitemIDList;
begin
  Attr := 0;
  ATempPidl := GetPidlCopy(FolderPidl);
  try
    if Failed(GetDesktopIShellFolder.GetAttributesOf(1, ATempPidl, Attr)) then
       Attr := 0;
    Details.ProcessDetails(CharWidth, ShellFolder, (Attr and SFGAO_FILESYSTEM) = SFGAO_FILESYSTEM);
  finally
    DisposePidl(ATempPidl);
  end;
end;

procedure TcxCustomItemProducer.DoGetInfoTip(Handle:HWND; ItemIndex: Integer;
  InfoTip: PChar; cch: Integer);

  procedure GetInfoTip(AInfoStr: PWideChar);
  begin
    StrLCopy(InfoTip, AInfoStr, cch);
  end;

var
  ATempPidl: PItemIDList;
  AQueryInfo: IQueryInfo;
  AInfoStr: PWideChar;
begin
  if GetShowToolTip then
  begin
    if ItemIndex > Items.Count - 1 then
       Exit;
    ATempPidl := GetPidlCopy(ItemInfo[ItemIndex].pidl);
    try
      if Succeeded(ShellFolder.GetUIObjectOf(Handle, 1, ATempPidl, IQueryInfo, nil, AQueryInfo)) and
        Succeeded(AQueryInfo.GetInfoTip(0, AInfoStr)) and (AInfoStr <> nil) then
      begin
        GetInfoTip(AInfoStr);
        cxMalloc.Free(AInfoStr);
      end;
    finally
      DisposePidl(ATempPidl);
    end;
  end
  else
    StrPLCopy(InfoTip, '', cch);
end;

function TcxCustomItemProducer.GetItemByPidl(APidl: PItemIDList): TcxShellItemInfo;
var
  AItemIndex: Integer;
begin
  AItemIndex := GetItemIndexByPidl(APidl);
  if AItemIndex <> -1 then
    Result := ItemInfo[AItemIndex]
  else
    Result := nil;
end;

function TcxCustomItemProducer.GetItemIndexByPidl(APidl: PItemIDList): Integer;
var
  I: Integer;
begin
  Result := -1;
  LockRead;
  try
    for I := 0 to Items.Count - 1 do
      if SmallInt(ShellFolder.CompareIDs(0, ItemInfo[I].pidl, APidl)) = 0 then
      begin
        Result := I;
        Break;
      end;
  finally
    UnlockRead;
  end;
end;

procedure TcxCustomItemProducer.Sort;
begin
  LockWrite;
  try
    DoSort;
  finally
    UnlockWrite;
  end;
end;

procedure TcxCustomItemProducer.FetchRequest(AItem: TcxShellItemInfo);
begin
  if ItemsInfoGatherer <> nil then
    ItemsInfoGatherer.RequestItemInfo(AItem);
end;

procedure TcxCustomItemProducer.ClearFetchQueue;
begin
  if ItemsInfoGatherer <> nil then
    ItemsInfoGatherer.ClearFetchQueue(Self);
end;

procedure TcxCustomItemProducer.CheckForSubitems(AItem: TcxShellItemInfo);
begin
end;

procedure TcxCustomItemProducer.PopulateItems(pEnum: IEnumIDList);
var
  ACeltFetched: Cardinal;
  APidl: PItemIDList;
  AHResult: HRESULT;
  AIsEnumElementValid: Boolean;
begin
  repeat
    AHResult := pEnum.Next(1, APidl, ACeltFetched);
    if AHResult = E_INVALIDARG then
    begin
      AHResult := pEnum.Next(1, APidl, ACeltFetched);
    end;
    AIsEnumElementValid := Succeeded(AHResult) and (AHResult <> S_FALSE) and (ACeltFetched > 0) and (APidl <> nil);
    if AIsEnumElementValid then
    try
      InternalAddItem(APidl);
    finally
      DisposePidl(APidl);
    end;
  until not AIsEnumElementValid;
end;

function TcxCustomItemProducer.GetFolderPidl: PItemIDList;
begin
  Result := FFolderPidl;
end;

procedure TcxCustomItemProducer.SetFolderPidl(AValue: PItemIDList);
begin
  if FFolderPidl <> nil then
    DisposePidl(FFolderPidl);
  FFolderPidl := GetPidlCopy(AValue);
end;

function TcxCustomItemProducer.GetItemInfo(AIndex: Integer): TcxShellItemInfo;
begin
  Result := TcxShellItemInfo(Items[AIndex]);
end;

{ TFetchThread }

constructor TFetchThread.Create(AInfoGatherer: TcxShellItemsInfoGatherer);
begin
  inherited Create(True);
  FInfoGatherer := AInfoGatherer;
end;

destructor TFetchThread.Destroy;
begin

  inherited;
end;

procedure TFetchThread.Execute;

  procedure AddToProcessedItems(AItem: TcxShellItemInfo);
  var
    AList: TList;
  begin
    AList := FInfoGatherer.ProcessedItems.LockList;
    try
      dxTestCheck(AList.IndexOf(AItem) = -1, 'AddToProcessedItems fails');
      AList.Add(AItem);
    finally
      FInfoGatherer.ProcessedItems.UnlockList;
    end;
  end;

  procedure RemoveDuplicateItems(AItem: TcxShellItemInfo);
  var
    AList: TList;
  begin
    AList := FInfoGatherer.ProcessedItems.LockList;
    AList.Remove(AItem);
    FInfoGatherer.ProcessedItems.UnlockList;
  end;

  procedure ProcessFetchQueueItem(AItem: TcxShellItemInfo);
  var
    AItemProducer: TcxCustomItemProducer;
  begin
    AItemProducer := AItem.ItemProducer;
    AItemProducer.LockRead;
    try
      if not AItemProducer.SlowInitializationDone(AItem) then
        AItemProducer.DoSlowInitialization(AItem);
    finally
      AItemProducer.UnlockRead;
    end;
    AddToProcessedItems(AItem);
  end;

var
  AList: TList;
  AThreadList: TThreadList;
  ARequestItem: TcxShellItemInfo;
  ASucceeded: Boolean;
begin
  try
    ASucceeded := Succeeded(CoInitializeEx(nil, COINIT_APARTMENTTHREADED));
    try
      AThreadList := FInfoGatherer.FetchQueue;
      repeat
        LockStopFetch;
        try
          AList := AThreadList.LockList;
          try
            if AList.Count = 0 then
              ARequestItem := nil
            else
              ARequestItem := TcxShellItemInfo(AList.Extract(AList.First))
          finally
            AThreadList.UnlockList;
          end;
          if ARequestItem <> nil then
          begin
            RemoveDuplicateItems(ARequestItem);
            ProcessFetchQueueItem(ARequestItem);
          end;
        finally
          UnlockStopFetch;
        end;
       if ARequestItem = nil then
         FInfoGatherer.WaitForRequest;
       until Terminated;
    finally
      if ASucceeded then
        CoUninitialize;
    end;
  except
    HandleException;
  end;
end;

procedure TFetchThread.LockStopFetch;
begin
  FInfoGatherer.LockFetchThread;
end;

procedure TFetchThread.UnlockStopFetch;
begin
  FInfoGatherer.UnlockFetchThread;
end;

{ TcxShellItemsInfoGatherer }

constructor TcxShellItemsInfoGatherer.Create(AOwner: TWinControl);
begin
  inherited Create;
  FOwner := AOwner;
  FFetchQueue := TThreadList.Create;
  FProcessedItems := TThreadList.Create;
  FQueuePopulated := TcxEvent.Create(False, False);
  InitializeCriticalSection(FLockFetchThread);
  CreateFetchThread;
  RegisterShellItemsInfoGatherer(Self);
end;

destructor TcxShellItemsInfoGatherer.Destroy;
begin
  UnregisterShellItemsInfoGatherer(Self);
  DestroyFetchThread;
  DeleteCriticalSection(FLockFetchThread);
  FreeAndNil(FQueuePopulated);
  FreeAndNil(FProcessedItems);
  FreeAndNil(FFetchQueue);
  inherited Destroy;
end;

procedure TcxShellItemsInfoGatherer.ClearFetchQueue(
  AItemProducer: TcxCustomItemProducer);

  procedure InternalClearFetchQueue(AQueue: TThreadList);
  var
    I: Integer;
    AList: TList;
  begin
    AList := AQueue.LockList;
    try
      if AItemProducer = nil then
        AList.Clear
      else
        for I := 0 to AItemProducer.Items.Count - 1 do
          AList.Remove(AItemProducer.Items[I]);
    finally
      AQueue.UnlockList;
    end;
  end;

begin
  if FIsFetchQueueClearing then
    Exit;
  FIsFetchQueueClearing := True;
  StopFetch;
  try
    InternalClearFetchQueue(FetchQueue);
    InternalClearFetchQueue(FProcessedItems);
  finally
    FIsFetchQueueClearing := False;
    ResumeFetch;
  end;
end;

procedure TcxShellItemsInfoGatherer.ClearVisibleItems;
var
  AList, AProcessedList: TList;
begin
  AList := FFetchQueue.LockList;
  AProcessedList := FProcessedItems.LockList;
  try
    AList.Clear;
    AProcessedList.Clear;
  finally
    FProcessedItems.UnlockList;
    FFetchQueue.UnlockList;
  end;
end;

procedure TcxShellItemsInfoGatherer.RequestItemInfo(AItem: TcxShellItemInfo);
var
  AList: TList;
  I: Integer;
begin
  AList := FFetchQueue.LockList;
  try
    I := AList.IndexOf(AItem);
    if I <> -1 then
      AList.Move(I, 0)
    else
      AList.Add(AItem);
  finally
    FFetchQueue.UnlockList;
  end;
  StartRequest;
end;

procedure TcxShellItemsInfoGatherer.RequestItems(AItems: TList);
var
  AList: TList;
  I: Integer;
begin
  AList := FFetchQueue.LockList;
  try
    for I := 0 to AItems.Count - 1 do
      AList.Add(AItems[I]);
  finally
    FFetchQueue.UnlockList;
  end;
  StartRequest;
end;

procedure TcxShellItemsInfoGatherer.ResumeFetch;
begin
  if FStopFetchCount > 0 then
  begin
    Dec(FStopFetchCount);
    if FStopFetchCount = 0 then
      UnLockFetchThread;
  end;
end;

procedure TcxShellItemsInfoGatherer.StopFetch;
begin
  Inc(FStopFetchCount);
  if FStopFetchCount = 1 then
    LockFetchThread;
end;

procedure TcxShellItemsInfoGatherer.DestroyFetchThread;
begin
  if FFetchThread <> nil then
  begin
    FFetchThread.Terminate;
    StartRequest;
    FFetchThread.WaitFor;
    FreeAndNil(FFetchThread);
  end;
end;

procedure TcxShellItemsInfoGatherer.LockFetchThread;
begin
  EnterCriticalSection(FLockFetchThread);
end;

procedure TcxShellItemsInfoGatherer.UnlockFetchThread;
begin
  LeaveCriticalSection(FLockFetchThread);
end;

procedure TcxShellItemsInfoGatherer.WaitForRequest;
begin
  FQueuePopulated.WaitFor(INFINITE);
end;

procedure TcxShellItemsInfoGatherer.CreateFetchThread;
begin
  FFetchThread := TFetchThread.Create(Self);
  FFetchThread.Priority := tpIdle;
  FFetchThread.Start;
end;

procedure TcxShellItemsInfoGatherer.StartRequest;
begin
  FQueuePopulated.SetEvent;
end;

{ TcxShellFolder }

constructor TcxShellFolder.Create(AAbsolutePIDL: PItemIDList);
var
  AParentPIDL: PItemIDList;
begin
  inherited Create;
  FAbsolutePIDL := AAbsolutePIDL;
  if GetPIDLItemsCount(FAbsolutePIDL) <= 1 then
  begin
    FParentShellFolder := GetDesktopIShellFolder;
    FRelativePIDL := GetPIDLCopy(FAbsolutePIDL);
  end
  else
  begin
    AParentPIDL := GetPIDLParent(FAbsolutePIDL);
    try
      GetDesktopIShellFolder.BindToObject(AParentPIDL, nil, IID_IShellFolder,
        FParentShellFolder);
    finally
      DisposePidl(AParentPIDL);
    end;
    FRelativePIDL := GetPIDLCopy(GetLastPIDLItem(FAbsolutePIDL));
  end;
end;

destructor TcxShellFolder.Destroy;
begin
  DisposePIDL(FRelativePIDL);
  inherited Destroy;
end;

function TcxShellFolder.GetAttributes: TcxShellFolderAttributes;

  procedure CheckAttribute(AShellAttributes, AAttributeShellAttribute: LongWord;
    AAttribute: TcxShellFolderAttribute);
  begin
    if HasShellAttribute(AShellAttributes, AAttributeShellAttribute) then
      Include(Result, AAttribute);
  end;

var
  AShellAttributes: LongWord;
begin
  AShellAttributes := GetShellAttributes(SFGAO_DISPLAYATTRMASK);
  Result := [];
  CheckAttribute(AShellAttributes, cxSFGAO_GHOSTED, sfaGhosted);
  CheckAttribute(AShellAttributes, SFGAO_HIDDEN, sfaHidden);
  CheckAttribute(AShellAttributes, SFGAO_ISSLOW, sfaIsSlow);
  CheckAttribute(AShellAttributes, SFGAO_LINK, sfaLink);
  CheckAttribute(AShellAttributes, SFGAO_READONLY, sfaReadOnly);
  CheckAttribute(AShellAttributes, SFGAO_SHARE, sfaShare);
end;

function TcxShellFolder.GetCapabilities: TcxShellFolderCapabilities;

  procedure CheckCapability(AShellAttributes, ACapabilityShellAttribute: LongWord;
    ACapability: TcxShellFolderCapability);
  begin
    if HasShellAttribute(AShellAttributes, ACapabilityShellAttribute) then
      Include(Result, ACapability);
  end;

var
  AShellAttributes: LongWord;
begin
  AShellAttributes := GetShellAttributes(SFGAO_CAPABILITYMASK);
  Result := [];
  CheckCapability(AShellAttributes, SFGAO_CANCOPY, sfcCanCopy);
  CheckCapability(AShellAttributes, SFGAO_CANDELETE, sfcCanDelete);
  CheckCapability(AShellAttributes, SFGAO_CANLINK, sfcCanLink);
  CheckCapability(AShellAttributes, SFGAO_CANMOVE, sfcCanMove);
  CheckCapability(AShellAttributes, SFGAO_CANRENAME, sfcCanRename);
  CheckCapability(AShellAttributes, SFGAO_DROPTARGET, sfcDropTarget);
  CheckCapability(AShellAttributes, SFGAO_HASPROPSHEET, sfcHasPropSheet);
end;

function TcxShellFolder.GetDisplayName: string;
begin
  Result := InternalGetDisplayName(ParentShellFolder, RelativePIDL, SHGDN_INFOLDER);
end;

function TcxShellFolder.GetIsFolder: Boolean;
begin
  Result := HasShellAttribute(SFGAO_FOLDER);
end;

function TcxShellFolder.GetPathName: string;

  function GetDisplayName(ANameType: DWORD): string;
  begin
    Result := InternalGetDisplayName(GetDesktopIShellFolder, AbsolutePIDL, ANameType);
  end;

begin
  Result := InternalGetDisplayName(GetDesktopIShellFolder, AbsolutePIDL, SHGDN_FORPARSING);
  if Pos('::{', Result) = 1 then
    Result := InternalGetDisplayName(GetDesktopIShellFolder, AbsolutePIDL, SHGDN_NORMAL);
end;

function TcxShellFolder.GetProperties: TcxShellFolderProperties;

  procedure CheckProperty(AShellAttributes, APropertyShellAttribute: LongWord;
    AProperty: TcxShellFolderProperty);
  begin
    if HasShellAttribute(AShellAttributes, APropertyShellAttribute) then
      Include(Result, AProperty);
  end;

var
  AShellAttributes: LongWord;
begin
  AShellAttributes := GetShellAttributes(SFGAO_BROWSABLE or SFGAO_COMPRESSED or
    SFGAO_ENCRYPTED or SFGAO_NEWCONTENT or SFGAO_NONENUMERATED or SFGAO_REMOVABLE);
  Result := [];
  CheckProperty(AShellAttributes, SFGAO_BROWSABLE, sfpBrowsable);
  CheckProperty(AShellAttributes, SFGAO_COMPRESSED, sfpCompressed);
  CheckProperty(AShellAttributes, SFGAO_ENCRYPTED, sfpEncrypted);
  CheckProperty(AShellAttributes, SFGAO_NEWCONTENT, sfpNewContent);
  CheckProperty(AShellAttributes, SFGAO_NONENUMERATED, sfpNonEnumerated);
  CheckProperty(AShellAttributes, SFGAO_REMOVABLE, sfpRemovable);
end;

function TcxShellFolder.GetShellAttributes(ARequestedAttributes: LongWord): LongWord;
begin
  ParentShellFolder.GetAttributesOf(1, FRelativePIDL, ARequestedAttributes);
  Result := ARequestedAttributes;
end;

function TcxShellFolder.GetShellFolder: IShellFolder;
begin
  if GetPIDLItemsCount(AbsolutePIDL) = 0 then
    Result := GetDesktopIShellFolder
  else
    GetDesktopIShellFolder.BindToObject(AbsolutePIDL, nil, IID_IShellFolder, Result);
end;

function TcxShellFolder.GetStorageCapabilities: TcxShellFolderStorageCapabilities;

  procedure CheckStorageCapability(AShellAttributes, AStorageCapabilityShellAttribute: LongWord;
    AStorageCapability: TcxShellFolderStorageCapability);
  begin
    if HasShellAttribute(AShellAttributes, AStorageCapabilityShellAttribute) then
      Include(Result, AStorageCapability);
  end;

var
  AShellAttributes: LongWord;
begin
  AShellAttributes := GetShellAttributes(SFGAO_STORAGECAPMASK);
  Result := [];
  CheckStorageCapability(AShellAttributes, SFGAO_FILESYSANCESTOR, sfscFileSysAncestor);
  CheckStorageCapability(AShellAttributes, SFGAO_FILESYSTEM, sfscFileSystem);
  CheckStorageCapability(AShellAttributes, SFGAO_FOLDER, sfscFolder);
  CheckStorageCapability(AShellAttributes, SFGAO_LINK, sfscLink);
  CheckStorageCapability(AShellAttributes, SFGAO_READONLY, sfscReadOnly);
  CheckStorageCapability(AShellAttributes, SFGAO_STORAGE, sfscStorage);
  CheckStorageCapability(AShellAttributes, SFGAO_STORAGEANCESTOR, sfscStorageAncestor);
  CheckStorageCapability(AShellAttributes, SFGAO_STREAM, sfscStream);
end;

function TcxShellFolder.GetSubFolders: Boolean;
begin
  Result := HasShellAttribute(SFGAO_HASSUBFOLDER);
end;

function TcxShellFolder.HasShellAttribute(AAttribute: LongWord): Boolean;
begin
  Result := HasShellAttribute(GetShellAttributes(AAttribute), AAttribute);
end;

function TcxShellFolder.HasShellAttribute(AAttributes, AAttribute: LongWord): Boolean;
begin
  Result := AAttributes and AAttribute <> 0;
end;

function TcxShellFolder.InternalGetDisplayName(AFolder: IShellFolder;
  APIDL: PItemIDList; ANameType: DWORD): string;
var
  AStrRet: TStrRet;
begin
  if Succeeded(AFolder.GetDisplayNameOf(APIDL, ANameType, AStrRet)) then
    Result := GetTextFromStrRet(AStrRet, APIDL)
  else
    Result := '';
end;

{ TcxShellItemInfo }

procedure TcxShellItemInfo.CheckInitialize(AIFolder: IShellFolder;
  APIDL: PItemIDList);
var
  AAttributes: Cardinal;
begin
  if Initialized then
    Exit;
  AAttributes := SFGAO_FOLDER;
  if Succeeded(AIFolder.GetAttributesOf(1, APIDL, AAttributes)) then
    FIsFolder := AAttributes and SFGAO_FOLDER <> 0
  else
  begin
    FIsFolder := False;
    FIsFilesystem := False;
    FIsDropTarget := True;
    FCanRename := True;
  end;
  FHasSubfolder := IsFolder;
  FName := GetShellItemDisplayName(AIFolder, APIDL, IsFolder);
  if IsFolder then
  begin
    FIconIndex := sysFolderIconIndex;
    FOpenIconIndex := sysFolderOpenIconIndex;
  end
  else
  begin
    FIconIndex := sysFileIconIndex;
    FOpenIconIndex := sysFileIconIndex;
  end;
  FOverlayIndex := -1;
  FThumbnailIndex := -1;
  FInitialized := True;
end;

{ TcxShellItemInfo }

procedure TcxShellItemInfo.CheckSubitems(AParentIFolder: IShellFolder;
  AEnumSettings: Cardinal);
begin
  FHasSubfolder := HasSubItems(AParentIFolder, FFullPIDL, AEnumSettings);
end;

procedure TcxShellItemInfo.CheckUpdate(AShellFolder: IShellFolder;
  AFolderPidl: PItemIDList; AFast: Boolean);
var
  AAttr: Cardinal;
  AFileInfo: TShFileInfo;
  AfqPidl: PItemIDList;
  AFlags: Cardinal;
  pszName: PChar;
  ATempPidl: PItemIDList;
begin
  if Updated or Updating then
     Exit;
  Updating := True;
  try
    Assert(pidl <> nil,'Item object not initialized');
    if pidl = nil then
      Exit;
    AfqPidl := ConcatenatePidls(AFolderPidl, pidl);
    try
      AAttr := 0;
      ATempPidl := pidl;
      CheckInitialize(AShellFolder, ATempPidl);
      if AFast then
      begin
        if not IsFolder then
        begin
          GetMem(pszName, MAX_PATH);
          try
            StrPLCopy(pszName, Name, MAX_PATH);
            cxShellGetThreadSafeFileInfo(pszName, FILE_ATTRIBUTE_NORMAL, AFileInfo, SizeOf(TShFileInfo),
              SHGFI_SYSICONINDEX or SHGFI_USEFILEATTRIBUTES);
            FIconIndex:=AFileInfo.iIcon;
            cxShellGetThreadSafeFileInfo(pszName, FILE_ATTRIBUTE_NORMAL, AFileInfo, SizeOf(TShFileInfo),
              SHGFI_SYSICONINDEX or SHGFI_USEFILEATTRIBUTES or
              SHGFI_OPENICON);
            FOpenIconIndex := AFileInfo.iIcon;
          finally
            FreeMem(pszName);
          end;
        end
        else
        begin
          AFlags := SHGFI_PIDL or SHGFI_SYSICONINDEX;
          cxShellGetThreadSafeFileInfo(PChar(AfqPidl), 0, AFileInfo, SizeOf(AFileInfo), AFlags);
          FIconIndex := AFileInfo.iIcon;
        end;
      end
      else
      begin
         // Processing attributes
         if Succeeded(AShellFolder.GetAttributesOf(1, ATempPidl, AAttr)) then
            FIsFilesystem := (AAttr and SFGAO_FILESYSTEM) = SFGAO_FILESYSTEM;
         AAttr := SFGAO_HIDDEN or SFGAO_SHARE or SFGAO_LINK or SFGAO_REMOVABLE;
         if Succeeded(AShellFolder.GetAttributesOf(1, ATempPidl, AAttr)) then
         begin
           FIsGhosted := (AAttr and SFGAO_HIDDEN) = SFGAO_HIDDEN;
           FIsShare := (AAttr and SFGAO_SHARE) = SFGAO_SHARE;
           FIsLink := (AAttr and SFGAO_LINK) = SFGAO_LINK;
           FIsRemovable := (AAttr and SFGAO_REMOVABLE) = SFGAO_REMOVABLE;
         end;
         AAttr := SFGAO_CAPABILITYMASK;
         if Succeeded(AShellFolder.GetAttributesOf(1, ATempPidl, AAttr)) then
         begin
           FIsDropTarget := (AAttr and SFGAO_DROPTARGET) = SFGAO_DROPTARGET;
           FCanRename := (AAttr and SFGAO_CANRENAME) = SFGAO_CANRENAME;
         end;
         // Processing icons
         AFlags := SHGFI_PIDL or SHGFI_SYSICONINDEX or SHGFI_TYPENAME;
         cxShellGetThreadSafeFileInfo(PChar(AfqPidl), 0, AFileInfo, SizeOf(AFileInfo), AFlags);
         FIconIndex := AFileInfo.iIcon;
         if FIsFolder then
           cxShellGetThreadSafeFileInfo(PChar(AfqPidl), 0, AFileInfo, SizeOf(AFileInfo),
             AFlags or SHGFI_OPENICON);
         FOpenIconIndex := AFileInfo.iIcon;
         FOverlayIndex := GetOverlayIndex;
        // UpdateThumbnail;
         Updated := True;
      end;
    finally
      DisposePidl(AfqPidl);
    end;
  finally
    Updating := False;
  end;
end;

constructor TcxShellItemInfo.Create(AItemProducer: TcxCustomItemProducer;
  AParentIFolder: IShellFolder; AParentPIDL, APIDL: PItemIDList;
  AFast: Boolean);
var
  AWithoutAV: Boolean;
begin
  inherited Create;
  FItemProducer := AItemProducer;
  // the following code required to get rid of bug, that occasionally appeared
  // on Windows XP. The pidl received from thr shell, anothed memory block
  // allocated internally, but occasionally appeared exception thad CopyMemory
  // can't be performed
  FDetails := TStringList.Create;
  repeat
    try
      UpdatePidl(AParentPIDL, APIDL);
      AWithoutAV := True;
    except
      AWithoutAV := False;
    end;
  until AWithoutAV;
  if not AFast then
    CheckInitialize(AParentIFolder, APIDL)
  else
  begin
    FName := ' ';
    FIconIndex := sysFileIconIndex;
    FOpenIconIndex := sysFileIconIndex;
    FOverlayIndex := -1;
    FThumbnailIndex := -1;
  end;
  FInfoTip := '';
end;

destructor TcxShellItemInfo.Destroy;
begin
  FreeAndNil(FFolder);
  DisposePidl(FFullPIDL);
  DisposePidl(Fpidl);
  FreeAndNil(FDetails);
  inherited;
end;

procedure TcxShellItemInfo.FetchDetails(wnd:HWND;ShellFolder: IShellFolder;DetailsMap:TcxShellDetails);

  function FormatSizeStr(AStr: string): string;
  begin
    Result := FormatMaskText('!### ### ### KB;0;*', AStr);
  end;

  function GetFileTypeInfo(const AFilename: string): string;
  begin
    Result := GetRegStringValue(GetRegStringValue(ExtractFileExt(AFileName), ''), '');
  end;

  function MapColumnToSCID(AShellFolder2: cxIShellFolder2; AColumnID: UINT;
    var pscid: TShColumnID): Boolean;
  var
    AHResult: HRESULT;
  begin
    AHResult := AShellFolder2.MapColumnToSCID(AColumnID, pscid);
    Result := AHResult = S_OK;
  end;

var
  AColumnDetails: TShellDetails;
  AFileInfo: TWIN32FindData;
  AFileSize: record
    case integer of
      0:(l,h:cardinal);
      1:(c:int64);
    end;
  AFindFileHandle: THandle;
  APDetailItem: PcxDetailItem;
  AShellDetails: IShellDetails;
  AShellFolder2: cxIShellFolder2;
  AStrPath: TStrRet;
  ATempName: PChar;
  I: Integer;
  AStr: OleVariant;
  AShColumnID: SHCOLUMNID;
begin
  // Processing details
  Details.Clear;
  if Succeeded(ShellFolder.QueryInterface(cxIShellFolder2, AShellFolder2)) then
  begin
    for I := 0 to DetailsMap.Count - 1 do
    begin
      APDetailItem := DetailsMap[I];
      if APDetailItem.ID = 0 then
        Continue; // Name column already exists
      if AShellFolder2.GetDetailsOf(pidl, APDetailItem.ID, AColumnDetails) = S_OK then
        Details.Add(GetTextFromStrRet(AColumnDetails.str, pidl))
      else
        if MapColumnToSCID(AShellFolder2, APDetailItem.ID, AShColumnID) and
          (AShellFolder2.GetDetailsEx(pidl, AShColumnID, @AStr) = S_OK) then
          Details.Add(AStr)
        else
          Details.Add('');
    end;
  end
  else
    if Succeeded(GetShellDetails(ShellFolder, pidl, AShellDetails)) then
    begin
      for I := 0 to DetailsMap.Count - 1 do
      begin
        APDetailItem := DetailsMap[I];
        if APDetailItem.ID = 0 then
           Continue; // Name column already exists
        if AShellDetails.GetDetailsOf(pidl, APDetailItem.ID, AColumnDetails) = S_OK then
          Details.Add(GetTextFromStrRet(AColumnDetails.str, pidl))
        else
          Details.Add('');
      end;
    end
    else
      if IsFilesystem then
      begin
        if Failed(ShellFolder.GetDisplayNameOf(pidl, SHGDN_NORMAL or SHGDN_FORPARSING, AStrPath)) then
          Exit;
        GetMem(ATempName, MAX_PATH);
        try
          StrPLCopy(ATempName, GetTextFromStrRet(AStrPath, pidl), MAX_PATH);
          AFindFileHandle := FindFirstFile(ATempName, AFileInfo);
          if AFindFileHandle <> INVALID_HANDLE_VALUE then
          try
            AFileSize.h := AFileInfo.nFileSizeHigh;
            AFileSize.l := AFileInfo.nFileSizeLow;
            Details.Add(FormatSizeStr(IntToStr(Ceil(AFileSize.c/1024))));
            Details.Add(GetFileTypeInfo(AFileInfo.cFileName));
            Details.Add(DateTimeToStr(cxFileTimeToDateTime(AFileInfo.ftLastWriteTime)));
          finally
            Windows.FindClose(AFindFileHandle);
          end;
        finally
          FreeMem(ATempName);
        end;
      end;
end;

function TcxShellItemInfo.GetItemIndex: Integer;
begin
  Result := ItemProducer.FItems.IndexOf(Self);
end;

procedure TcxShellItemInfo.UpdatePidl(AParentPidl, APidl: PItemIDList);
begin
  DisposePidl(FPidl);
  FPidl := GetPidlCopy(APidl);
  DisposePidl(FFullPIDL);
  FFullPIDL := ConcatenatePidls(AParentPidl, APidl);
  FFolder.Free;
  FFolder := TcxShellFolder.Create(FFullPIDL);
end;

function TcxShellItemInfo.GetOverlayIndex: Integer;
const
  SHGFI_OVERLAYINDEX = $40;
var
  AFileInfo: TShFileInfo;
  AFlags: Cardinal;
begin
  if GetComCtlVersion >= ComCtlVersionIE5 then
  begin
    AFlags := SHGFI_PIDL or SHGFI_ICON or SHGFI_OVERLAYINDEX;
    ZeroMemory(@AFileInfo, SizeOf(AFileInfo));
    cxShellGetThreadSafeFileInfo(PChar(FullPIDL), 0, AFileInfo, SizeOf(AFileInfo), AFlags);
    DestroyIcon(AFileInfo.hIcon);
    Result := AFileInfo.iIcon;
    Result := (Result shr ((SizeOf(Result) - 1) * 8)) and $FF - 1;
  end
  else
  begin
    if IsLink then
      Result := cxShellShortcutItemOverlayIndex
    else
      if IsShare then
        Result := cxShellSharedItemOverlayIndex
      else
        Result := cxShellNormalItemOverlayIndex;
  end;
end;

function TcxShellItemInfo.HasThumbnail: Boolean;
begin
  Result := FThumbnailIndex <> -1;
end;

procedure TcxShellItemInfo.SetNewPidl(pFolder: IShellFolder; AFolderPidl, APidl: PItemIDList);
begin
  if APidl = nil then
    Exit;
  UpdatePidl(AFolderPidl, APidl);
  Updated := False;
  Processed := False;
  FInitialized := False;
  CheckUpdate(pFolder, AFolderPidl, False);
end;

procedure TcxShellItemInfo.UpdateThumbnail;
begin
  if not FThumbnailUpdating then
  begin
    FThumbnailUpdating := True;
    try
      FThumbnailIndex := ItemProducer.GetThumbnailIndex(Self);
      FThumbnailUpdated := True;
    finally
      FThumbnailUpdating := False;
    end;
  end;
end;

{ TcxShellOptions }

constructor TcxShellOptions.Create(AOwner: TWinControl);
begin
  inherited Create;
  FOwner := AOwner;
  FContextMenus := True;
  FMasks := TStringList.Create;
  FShowFolders := True;
  FShowNonFolders := True;
  FShowToolTip := True;
  FShowZipFilesWithFolders := True;
  FTrackShellChanges := True;
end;

destructor TcxShellOptions.Destroy;
begin
  FreeAndNil(FMasks);
  inherited Destroy;
end;

procedure TcxShellOptions.Assign(Source: TPersistent);
begin
  if Source is TcxShellOptions then
    with TcxShellOptions(Source) do
    begin
      BeginUpdate;
      try
        DoAssign(TcxShellOptions(Source));
      finally
        EndUpdate;
      end;
    end
  else
    inherited Assign(Source);
end;

procedure TcxShellOptions.BeginUpdate;
begin
  Inc(FLock);
end;

procedure TcxShellOptions.EndUpdate;
begin
  Dec(FLock);
  if FLock <= 0 then
    NotifyUpdateContents;
end;

function TcxShellOptions.GetEnumFlags: Cardinal;
begin
  if ShowFolders then
   Result := SHCONTF_FOLDERS
  else
   Result := 0;
  if ShowNonFolders then
   Result := Result or SHCONTF_NONFOLDERS;
  if ShowHidden then
   Result := Result or SHCONTF_INCLUDEHIDDEN;
end;

type
  TParseState = class
  private
    AnySym: Boolean;
    MaskPos: Integer;
    NamePos: Integer;
  end;

function TcxShellOptions.IsFileNameValid(const AName: string): Boolean;
var
  AParseStates: TObjectList;
  AState: TParseState;
  AMaskLength, ANameLength: Integer;

  procedure StoreCurrentState(const AMaskPos, ANamePos: Integer; AAnySym: Boolean);
  begin
    AState := TParseState.Create;
    AParseStates.Add(AState);
    AState.MaskPos := AMaskPos;
    AState.NamePos := ANamePos;
    AState.AnySym := AAnySym;
  end;

  procedure RestoreCurrentState(out AMaskPos, ANamePos: Integer; out AAnySym: Boolean);
  begin
    AState := TParseState(AParseStates.Extract(AParseStates.Last));
    AMaskPos := AState.MaskPos;
    ANamePos := AState.NamePos + 1;
    AAnySym := AState.AnySym;
    AState.Free;
  end;

  function Parse(const AMask, AFileName: string): Boolean;
  var
    P, AFirstSym: Integer;
    AMaskPos, ANamePos: Integer;
    AAnySym: Boolean;
  begin
    Result := False;
    RestoreCurrentState(AMaskPos, ANamePos, AAnySym);
    while AMaskPos <= AMaskLength do
    begin
      case AMask[AMaskPos] of
        '*':
          begin
            while (AMaskPos < AMaskLength) and (AMask[AMaskPos + 1] = '*') do
              Inc(AMaskPos);
            AAnySym := True;
          end;
        '?':
          begin
            if ANamePos > ANameLength then
              Exit;
            Inc(ANamePos);
          end;
        else
        begin
          AFirstSym := AMaskPos;
          while (AMaskPos < AMaskLength) and (AMask[AMaskPos + 1] <> '*') and (AMask[AMaskPos + 1] <> '?') do
            Inc(AMaskPos);
          P := Pos(Copy(AMask, AFirstSym, AMaskPos - AFirstSym + 1), Copy(AFileName, ANamePos, 260));
          if (P = 0) or (not AAnySym and (P <> 1)) then
            Exit;

          StoreCurrentState(AMaskPos, ANamePos, AAnySym);

          Inc(ANamePos, P - 1 + (AMaskPos - AFirstSym + 1));
          AAnySym := False;
        end;
      end;
      Inc(AMaskPos);
    end;
    Result := (ANamePos > ANameLength) or AAnySym or (AParseStates.Count > 0) and Parse(AMask, AFileName);
  end;

var
  AMask, AFileName: string;
  I: Integer;
begin
  Result := True;
  AParseStates := TObjectList.Create;
  try
    AFileName := AnsiUpperCase(AName);
    ANameLength := Length(AFileName);
    for I := 0 to FMasks.Count - 1 do
    begin
      AParseStates.Clear;
      AMask := AnsiUpperCase(FMasks[I]);
      AMaskLength := Length(AMask);
      StoreCurrentState(1, 0, False);
      Result := Parse(AMask, AFileName);
      if Result then
        Break;
    end;
  finally
    AParseStates.Free;
  end;
end;

procedure TcxShellOptions.DoAssign(Source: TcxShellOptions);
begin
  Self.ContextMenus := Source.ContextMenus;
  Self.FileMask := Source.FileMask;
  Self.ShowFolders := Source.ShowFolders;
  Self.ShowHidden := Source.ShowHidden;
  Self.ShowNonFolders := Source.ShowNonFolders;
  Self.ShowToolTip := Source.ShowToolTip;
  Self.ShowZipFilesWithFolders := Source.ShowZipFilesWithFolders;
  Self.TrackShellChanges := Source.TrackShellChanges;
end;

procedure TcxShellOptions.DoNotifyUpdateContents;
begin
  SendMessage(Owner.Handle, DSM_NOTIFYUPDATECONTENTS, 0, 0);
end;

procedure TcxShellOptions.NotifyUpdateContents;
begin
  if (FLock <= 0) and Owner.HandleAllocated then
    DoNotifyUpdateContents;
end;

procedure TcxShellOptions.SetFileMask(const Value: string);
begin
  if Value <> FFileMask then
  begin
    FFileMask := Value;
    UpdateMasks;
    NotifyUpdateContents;
  end;
end;

procedure TcxShellOptions.SetShowFolders(Value: Boolean);
begin
  FShowFolders := Value;
  NotifyUpdateContents;
end;

procedure TcxShellOptions.SetShowHidden(Value: Boolean);
begin
  FShowHidden := Value;
  NotifyUpdateContents;
end;

procedure TcxShellOptions.SetShowNonFolders(Value: Boolean);
begin
  FShowNonFolders := Value;
  NotifyUpdateContents;
end;

procedure TcxShellOptions.SetShowToolTip(Value: Boolean);
begin
  if Value <> FShowToolTip then
  begin
    FShowToolTip := Value;
    if Assigned(FOnShowToolTipChanged) then
      FOnShowToolTipChanged(Self);
  end;
end;

procedure TcxShellOptions.SetShowZipFilesWithFolders(AValue: Boolean);
begin
  FShowZipFilesWithFolders := AValue;
  NotifyUpdateContents;
end;

procedure TcxShellOptions.SetTrackShellChanges(AValue: Boolean);
begin
  if FTrackShellChanges <> AValue then
  begin
    FTrackShellChanges := AValue;
    NotifyUpdateContents;
  end;
end;

procedure TcxShellOptions.UpdateMasks;
var
  I: Integer;
begin
  FMasks.Clear;
  FMasks.Text := StringReplace(FFileMask, ';', #13#10, [rfReplaceAll]);
  for I := 0 to FMasks.Count - 1 do
  begin
    if (FMasks[I] = '') or (FMasks[I] = '*') or (FMasks[I] = '*.*') then
    begin
      FMasks.Clear;
      Break;
    end;
  end;
end;

{ TcxShellThumbnailOptions }

constructor TcxShellThumbnailOptions.Create(AOwner: TPersistent);
begin
  inherited Create;
  FOwner := AOwner;
  FWidth := 96;
  FHeight := 96;
end;

procedure TcxShellThumbnailOptions.Assign(Source: TPersistent);
begin
  if Source is TcxShellThumbnailOptions then
    with TcxShellThumbnailOptions(Source) do
    begin
      BeginUpdate;
      try
        Self.Height := Height;
        Self.Width := Width;
        Self.ShowThumbnails := ShowThumbnails;
      finally
        EndUpdate;
      end;
    end
  else
    inherited Assign(Source);
end;

procedure TcxShellThumbnailOptions.BeginUpdate;
begin
  Inc(FLock);
end;

procedure TcxShellThumbnailOptions.EndUpdate;
begin
  Dec(FLock);
  Changed;
end;

procedure TcxShellThumbnailOptions.Changed;
begin
  if FLock <= 0 then
    DoChange;
end;

procedure TcxShellThumbnailOptions.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TcxShellThumbnailOptions.SetHeight(const Value: Integer);
begin
  if Value <> FHeight then
  begin
    FHeight := Value;
    Changed;
  end;
end;

procedure TcxShellThumbnailOptions.SetShowThumbnails(const Value: Boolean);
begin
  if Value <> FShowThumbnails then
  begin
    FShowThumbnails := Value;
    Changed;
  end;
end;

procedure TcxShellThumbnailOptions.SetWidth(const Value: Integer);
begin
  if Value <> FWidth then
  begin
    FWidth := Value;
    Changed;
  end;
end;

{ TcxShellDetails }

function TcxShellDetails.Add: PcxDetailItem;
begin
  New(Result);
  Items.Add(Result);
end;

procedure TcxShellDetails.Clear;
var
  di:PcxDetailItem;
begin
  while Items.Count<>0 do
  begin
    di:=Items.Last;
    Items.Remove(di);
    Dispose(di);
  end;
end;

constructor TcxShellDetails.Create;
begin
  inherited Create;
  FItems:=TList.Create;
end;

destructor TcxShellDetails.Destroy;
begin
  Clear;
  FreeAndNil(FItems);
  inherited;
end;

function TcxShellDetails.GetCount: Integer;
begin
  Result:=Items.Count;
end;

function TcxShellDetails.GetItems(Index: Integer): PcxDetailItem;
begin
  Result:=Items[Index];
end;

procedure TcxShellDetails.ProcessDetails(ACharWidth: Integer;
  AShellFolder: IShellFolder; AFileSystem: Boolean);
const
  AAlignment: array[0..2] of TAlignment = (taLeftJustify, taRightJustify, taCenter);
var
  AColumnDetails: TShellDetails;
  AColumnFlags: Cardinal;
  AColumnIndex: Integer;
  SD: IShellDetails;
  SF2: IShellFolder2;

  procedure SetItemInfo(AItem: PcxDetailItem; AText: string; AWidth:Integer;
    AAlignment: TAlignment; AID:Integer);
  begin
    AItem.Text := AText;
    AItem.Width := AWidth * ACharWidth;
    AItem.Alignment := AAlignment;
    AItem.ID := AID;
  end;

  procedure AddItem(ADetails: TShellDetails; AIndex: Integer; AText: string);
  var
    ANewColumn: PcxDetailItem;
  begin
    ANewColumn := Add;
    SetItemInfo(ANewColumn, AText, ADetails.cxChar, AAlignment[ADetails.fmt], AIndex);
  end;

var
  ADefaultColumns: Boolean;
  AText: WideString;
begin
  ZeroMemory(@AColumnDetails, SizeOf(AColumnDetails));
  AColumnIndex := 0;
  Clear;
  if Succeeded(AShellFolder.QueryInterface(cxIShellFolder2, SF2)) then
  begin
    ADefaultColumns := False;
    while SF2.GetDetailsOf(nil, AColumnIndex, AColumnDetails) = S_OK do
    begin
      Inc(AColumnIndex);
      AText := GetTextFromStrRet(AColumnDetails.str, nil);
      if Succeeded(SF2.GetDefaultColumnState(AColumnIndex - 1, AColumnFlags)) then
      begin
        ADefaultColumns := ADefaultColumns or (AColumnFlags and SHCOLSTATE_ONBYDEFAULT = SHCOLSTATE_ONBYDEFAULT);
        if not IsWinXPOrLater and ADefaultColumns and
          (AColumnFlags and SHCOLSTATE_ONBYDEFAULT <> SHCOLSTATE_ONBYDEFAULT) then
          Break;
        if (AColumnFlags and SHCOLSTATE_ONBYDEFAULT <> SHCOLSTATE_ONBYDEFAULT) or
          (AColumnFlags and SHCOLSTATE_HIDDEN = SHCOLSTATE_HIDDEN) then
          Continue;
      end;
      AddItem(AColumnDetails, AColumnIndex - 1, AText);
    end;
  end
  else
    if GetShellDetails(AShellFolder, nil, SD) = S_OK then
    begin
      while SD.GetDetailsOf(nil, AColumnIndex, AColumnDetails) = S_OK do
      begin
        AText := GetTextFromStrRet(AColumnDetails.str, nil);
        AddItem(AColumnDetails, AColumnIndex, AText);
        Inc(AColumnIndex);
      end;
    end
    else
    begin // Processing creating columns manually (for Win95/98)
      SetItemInfo(Add, SShellDefaultNameStr, 25, taLeftJustify, 0);
      if AFileSystem then
      begin
        SetItemInfo(Add, SShellDefaultSizeStr, 10, taRightJustify, 1);
        SetItemInfo(Add, SShellDefaultTypeStr, 10, taLeftJustify, 2);
        SetItemInfo(Add, SShellDefaultModifiedStr, 14, taLeftJustify, 3);
      end;
    end;
end;

procedure TcxShellDetails.Remove(Item: PcxDetailItem);
begin
  Items.Remove(Item);
  Dispose(Item);
end;

{ TcxDropTarget }

constructor TcxDropSource.Create(AOwner: TWinControl);
begin
  inherited Create;
  FOwner := AOwner;
end;

function TcxDropSource.GiveFeedback(dwEffect: Integer): HResult;
begin
  Result:=DRAGDROP_S_USEDEFAULTCURSORS;
end;

function TcxDropSource.QueryContinueDrag(fEscapePressed: BOOL;
  grfKeyState: Integer): HResult;
begin
  if fEscapePressed then
     Result:=DRAGDROP_S_CANCEL
  else
  if ((grfKeyState and MK_LBUTTON)<>MK_LBUTTON) and
     ((grfKeyState and MK_RBUTTON)<>MK_RBUTTON) then
     Result:=DRAGDROP_S_DROP
  else
     Result:=S_OK;
end;

{ TcxDragDropSettings }

constructor TcxDragDropSettings.Create;
begin
  inherited Create;
  FAllowDragObjects := True;
  FDefaultDropEffect := deMove;
  FDropEffect := [deMove, deCopy, deLink];
end;

procedure TcxDragDropSettings.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

function TcxDragDropSettings.GetDefaultDropEffectAPI: Integer;
begin
  case DefaultDropEffect of
    deCopy:
      Result := DROPEFFECT_COPY;
    deMove:
      Result := DROPEFFECT_MOVE;
    deLink:
      Result := DROPEFFECT_LINK;
  else
    Result := DROPEFFECT_NONE;
  end;
end;

function TcxDragDropSettings.GetDropEffectAPI: DWORD;
begin
  Result := 0;
  if deCopy in DropEffect then
    Result := Result or DROPEFFECT_COPY;
  if deMove in DropEffect then
    Result := Result or DROPEFFECT_MOVE;
  if deLink in DropEffect then
    Result := Result or DROPEFFECT_LINK;
end;

procedure TcxDragDropSettings.SetAllowDragObjects(Value: Boolean);
begin
  if Value <> FAllowDragObjects then
  begin
    FAllowDragObjects := Value;
    Changed;
  end;
end;

procedure cxShellInitialize;
begin
  FComInitializationSucceeded := Succeeded(OleInitialize(nil));
  FShellLock := TCriticalSection.Create;
  ShellLibrary := LoadLibrary(shell32);
  cxSHGetFolderLocation := GetProcAddress(ShellLibrary, 'SHGetFolderLocation');
  SHChangeNotifyRegister := GetProcAddress(ShellLibrary,PChar(2));
  SHChangeNotifyUnregister := GetProcAddress(ShellLibrary,PChar(4));
  SHChangeNotification_Lock := GetProcAddress(ShellLibrary, PChar(644));
  SHChangeNotification_UnLock := GetProcAddress(ShellLibrary, PChar(645));
  SHGetImageList := GetProcAddress(ShellLibrary, 'SHGetImageList');
  cxSHGetPathFromIDList := GetProcAddress(ShellLibrary, 'SHGetPathFromIDListA');
  cxSHGetPathFromIDListW := GetProcAddress(ShellLibrary, 'SHGetPathFromIDListW');
  dxILIsParent := GetProcAddress(ShellLibrary, 'ILIsParent');
end;

procedure cxShellUninitialize;
var
  I: Integer;
begin
  if FShellItemsInfoGatherers <> nil then
    for I := 0 to FShellItemsInfoGatherers.Count - 1 do
      TcxShellItemsInfoGatherer(FShellItemsInfoGatherers[I]).DestroyFetchThread;

  FcxMalloc := nil;
  if ShellLibrary <> 0 then
    FreeLibrary(ShellLibrary);
  FreeAndNil(FShellLock);
  if FComInitializationSucceeded then
    OleUninitialize;
end;

{ TcxShellCustomContextMenu }

procedure TcxShellCustomContextMenu.AddDefaultShellItems(
  AIShellFolder: IShellFolder);
begin
  FContextMenu := nil;
  if Succeeded(AIShellFolder.CreateViewObject(WindowHandle, IID_IContextMenu, FContextMenu)) then
   DoAddDefaultShellItems;
end;

procedure TcxShellCustomContextMenu.AddDefaultShellItems(
  AIShellFolder: IShellFolder; AItemPIDLList: TList);
var
  APIDLs: PITEMIDLISTARRAY;
begin
  APIDLs := CreatePidlArrayFromList(AItemPIDLList);
  try
    if Succeeded(AIShellFolder.GetUIObjectOf(WindowHandle, AItemPIDLList.Count,
      APIDLs[0], IID_IContextMenu, nil, FContextMenu)) then
      DoAddDefaultShellItems;
  finally
    DisposePidlArray(APIDLs);
  end;
end;

constructor TcxShellCustomContextMenu.Create;
begin
  inherited Create;
  FFirstInvokeShellCommandIndex := 200;
end;

destructor TcxShellCustomContextMenu.Destroy;
begin
  DestroyMenu(FMenu);
  inherited Destroy;
end;

procedure TcxShellCustomContextMenu.Popup(APos: TPoint);
begin
  FMenu := CreatePopupMenu;
  try
    Populate;
    DoPopup(APos);
  finally
    DestroyMenu(FMenu);
  end;
end;

procedure TcxShellCustomContextMenu.DoAddDefaultShellItems;
const
  CMF_EXTENDEDVERBS = $00000100;
var
  AContextMenuQueryFlags: Cardinal;
begin
  AContextMenuQueryFlags := CMF_NORMAL;
  if GetKeyState(VK_SHIFT) < 0 then
    AContextMenuQueryFlags := AContextMenuQueryFlags or CMF_EXTENDEDVERBS;
  FContextMenu.QueryContextMenu(Menu, GetMenuItemCount(Menu),
    FFirstInvokeShellCommandIndex, $7FFF, AContextMenuQueryFlags);
end;

procedure TcxShellCustomContextMenu.DoPopup(APos: TPoint);
var
  ACommandId: BOOL;
  AHandle: THandle;
  ACallbackWnd: TcxContextMenuMessageWindow;
  AContextMenu2: IContextMenu2;
begin
  if (FContextMenu <> nil) and Succeeded(FContextMenu.QueryInterface(IID_IContextMenu2, AContextMenu2)) then
    ACallbackWnd := CreateCallbackWnd(AContextMenu2)
  else
    ACallbackWnd := nil;
  try
    if ACallbackWnd <> nil then
      AHandle := ACallbackWnd.Handle
    else
      AHandle := WindowHandle;
    ACommandId := TrackPopupMenu(Menu, TPM_LEFTALIGN or TPM_LEFTBUTTON or
      TPM_RIGHTBUTTON or TPM_RETURNCMD, APos.X, APos.Y, 0, AHandle, nil);
  finally
    FreeAndNil(ACallbackWnd);
  end;
  ExecuteMenuItemCommand(Cardinal(ACommandId));
end;

procedure TcxShellCustomContextMenu.ExecuteMenuItemCommand(ACommandId: Cardinal);
var
  AInvokeCommandInfo: TCMInvokeCommandInfo;
begin
  if (FContextMenu <> nil) and (ACommandId >= FFirstInvokeShellCommandIndex) then
  begin
    ZeroMemory(@AInvokeCommandInfo, SizeOf(AInvokeCommandInfo));
    AInvokeCommandInfo.cbSize := SizeOf(AInvokeCommandInfo);
    AInvokeCommandInfo.hwnd := WindowHandle;
    AInvokeCommandInfo.lpVerb := MakeIntResourceA(ACommandId - FFirstInvokeShellCommandIndex);
    AInvokeCommandInfo.nShow := SW_SHOWNORMAL;
    FContextMenu.InvokeCommand(AInvokeCommandInfo);
  end;
end;

initialization
  cxShellInitialize;

finalization
  cxShellUninitialize;

end.
