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

unit cxShellListView;

{$I cxVer.inc}

interface

uses
  Windows, Messages, Comctrls, Controls, Forms, Classes, Menus, ShlObj, StdCtrls,
  dxCOre, dxMessages, cxGraphics, cxGeometry, cxContainer, cxControls, cxDataUtils,
  cxCustomData, cxScrollBar, cxLookAndFeels, cxLookAndFeelPainters, cxListView,
  cxShellCommon, cxShellControls, cxHeader, cxEdit;

type
  TcxShellObjectPathType = (sptAbsolutePhysical, sptRelativePhysical, sptUNC, sptVirtual,
    sptInternalAbsoluteVirtual, sptInternalRelativeVirtual, sptIncorrect);

  TcxShellViewOption = (svoShowFiles, svoShowFolders, svoShowHidden);
  TcxShellViewOptions = set of TcxShellViewOption;

  TcxCustomShellListView = class;

  TcxBeforeNavigationEvent = procedure(Sender: TcxCustomShellListView; ANewAbsolutePIDL: PItemIDList) of object;
  TcxCurrentFolderChangedEvent = procedure(Sender: TcxCustomShellListView) of object;

  TcxShellIconOptions = class(TcxIconOptions)
  private
    function GetSize: TcxShellIconSize;
    procedure SetSize(const Value: TcxShellIconSize);
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Size: TcxShellIconSize read GetSize write SetSize default isDefault;
  end;

  { TcxInnerShellListView }

  TcxInnerShellListView = class(TcxCustomInnerShellListView)
  private
    FAbsolutePIDL: PItemIDList;
    FOnChange: TLVChangeEvent;
    function GetAbsolutePIDL: PItemIDList;
    function GetContainer: TcxCustomShellListView;
    function GetPath: string;
    procedure SaveAbsolutePIDL(AValue: PItemIDList);
    procedure SetAbsolutePIDL(AValue: PItemIDList);
    procedure SetPath(AValue: string);
    procedure DSMShellChangeNotify(var Message: TMessage); message DSM_SHELLCHANGENOTIFY;

    property AbsolutePIDL: PItemIDList read GetAbsolutePIDL write SetAbsolutePIDL;
    property Path: string read GetPath write SetPath;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    function DoCompare(AItem1, AItem2: TcxShellFolder;
      out ACompare: Integer): Boolean; override;
    procedure Navigate(APIDL: PItemIDList); override;
    function NeedCheckScrollBars(var Message: TMessage): Boolean; override;
    procedure WndProc(var Message: TMessage); override;
    procedure ChangeHandler(Sender: TObject; AItem: TListItem;
      AChange: TItemChange); virtual;

    property Container: TcxCustomShellListView read GetContainer;
    property OnChange: TLVChangeEvent read FOnChange write FOnChange;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
  public
    property Anchors;
    property BorderStyle;
    property Color;
    property DragDropSettings;
    property HotTrack;
    property IconOptions;
    property Items;
    property ListViewStyle;
    property MultiSelect;
    property Options;
    property Root;
    property Visible;
    property AfterNavigation;
    property BeforeNavigation;
    property OnAddFolder;
    property OnCompare;
    property OnRootChanged;
    property OnSelectItem;
    property OnShellChange;
  end;

  { TcxCustomShellListView }

  TcxCustomShellListView = class(TcxListViewContainer, IcxShellDependedControls, IcxShellRoot)
  private
    FIsExitProcessing: Boolean;
    FSelectedFiles: TStringList;
    FOnAddFolder: TcxShellAddFolderEvent;
    FOnBeforeNavigation: TcxBeforeNavigationEvent;
    FOnChange: TLVChangeEvent;
    FOnCurrentFolderChanged: TcxCurrentFolderChangedEvent;
    FOnCompare: TcxShellCompareEvent;
    FOnEdited: TLVEditedEvent;
    FOnEditing: TLVEditingEvent;
    FOnExecuteItem: TcxShellExecuteItemEvent;
    FOnSelectItem: TLVSelectItemEvent;
    FOnShellChange: TcxShellChangeEvent;

    procedure AddFolderHandler(Sender: TObject; AFolder: TcxShellFolder; var ACanAdd: Boolean);
    procedure BeforeNavigationHandler(Sender: TcxCustomInnerShellListView;
      APItemIDList: PItemIDList; AFolderPath: WideString);
    procedure ChangeHandler(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure EditedHandler(Sender: TObject; Item: TListItem; var S: string);
    procedure EditingHandler(Sender: TObject; Item: TListItem;
      var AllowEdit: Boolean);
    procedure ExecuteItemHandler(Sender: TObject; APIDL: PItemIDList; var AHandled: Boolean);
    procedure SelectItemHandler(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure ShellChangeHandler(Sender: TObject; AEventID: DWORD;
      APIDL1, APIDL2: PItemIDList);

    function DoCompare(AItem1, AItem2: TcxShellFolder;
      out ACompare: Integer): Boolean;
    function GetAbsolutePIDL: PItemIDList;
    function GetDragDropSettings: TcxDragDropSettings;
    function GetFolder(AIndex: Integer): TcxShellFolder;
    function GetFolderCount: Integer;
    function GetIconOptions: TcxShellIconOptions;
    function GetInnerListView: TcxInnerShellListView;
    function GetListHotTrack: Boolean;
    function GetOptions: TcxShellListViewOptions;
    function GetPath: string;
    function GetRoot: TcxShellListRoot;
    function GetSelectedFilePaths: TStrings;
    function GetSorting: Boolean;
    function GetThumbnailOptions: TcxShellThumbnailOptions;
    procedure SetAbsolutePIDL(Value: PItemIDList);
    procedure SetDragDropSettings(Value: TcxDragDropSettings);
    procedure SetIconOptions(Value: TcxShellIconOptions);
    procedure SetListHotTrack(Value: Boolean);
    procedure SetOptions(Value: TcxShellListViewOptions);
    procedure SetPath(Value: string);
    procedure SetRoot(Value: TcxShellListRoot);
    procedure SetSorting(const Value: Boolean);
    procedure SetThumbnailOptions(const Value: TcxShellThumbnailOptions);
  protected
    FDataBinding: TcxCustomDataBinding;
    procedure DoExit; override;
    procedure Loaded; override;
    procedure CurrentFolderChangedHandler(Sender: TObject; Root: TcxCustomShellRoot); virtual;
    function GetDataBindingClass: TcxCustomDataBindingClass; virtual;
    function GetIconOptionsClass: TcxIconOptionsClass; override;
    function GetInnerListViewClass: TcxInnerListViewClass; override;
    procedure InitializeInnerListView; override;
    // IcxShellDependedControls
    function GetDependedControls: TcxShellDependedControls;
    // IcxShellRoot
    function IcxShellRoot.GetRoot = GetShellRoot;
    function GetShellRoot: TcxCustomShellRoot;

    function GetViewOptions(AForNavigation: Boolean = False): TcxShellViewOptions;
    procedure SetViewStyle(Value: TViewStyle); override;
    procedure WndProc(var Message: TMessage); override;

    property DataBinding: TcxCustomDataBinding read FDataBinding;
    property DragDropSettings: TcxDragDropSettings read GetDragDropSettings write SetDragDropSettings;
    property IconOptions: TcxShellIconOptions read GetIconOptions write SetIconOptions;
    property ListHotTrack: Boolean read GetListHotTrack write SetListHotTrack default False;
    property Options: TcxShellListViewOptions read GetOptions write SetOptions;
    property Root: TcxShellListRoot read GetRoot write SetRoot;
    property Sorting: Boolean read GetSorting write SetSorting default False;
    property ThumbnailOptions: TcxShellThumbnailOptions read GetThumbnailOptions write SetThumbnailOptions;
    property OnAddFolder: TcxShellAddFolderEvent read FOnAddFolder write FOnAddFolder;
    property OnBeforeNavigation: TcxBeforeNavigationEvent read FOnBeforeNavigation write FOnBeforeNavigation;
    property OnChange: TLVChangeEvent read FOnChange write FOnChange;
    property OnCompare: TcxShellCompareEvent read FOnCompare write FOnCompare;
    property OnCurrentFolderChanged: TcxCurrentFolderChangedEvent
      read FOnCurrentFolderChanged write FOnCurrentFolderChanged;
    property OnEdited: TLVEditedEvent read FOnEdited write FOnEdited;
    property OnEditing: TLVEditingEvent read FOnEditing write FOnEditing;
    property OnExecuteItem: TcxShellExecuteItemEvent read FOnExecuteItem write FOnExecuteItem;
    property OnSelectItem: TLVSelectItemEvent read FOnSelectItem write FOnSelectItem;
    property OnShellChange: TcxShellChangeEvent read FOnShellChange write FOnShellChange;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    procedure SetFocus; override;
    procedure BrowseParent;
    function GetItemAbsolutePIDL(AIndex: Integer): PItemIDList;
    procedure ProcessTreeViewNavigate(APidl: PItemIDList);
    procedure Sort; overload;
    procedure Sort(AColumnIndex: Integer; AIsAscending: Boolean); overload;
    procedure UpdateContent;

    property AbsolutePath: string read GetPath write SetPath; // deprecated
    property AbsolutePIDL: PItemIDList read GetAbsolutePIDL write SetAbsolutePIDL;
    property FolderCount: Integer read GetFolderCount;
    property Folders[AIndex: Integer]: TcxShellFolder read GetFolder;
    property InnerListView: TcxInnerShellListView read GetInnerListView;
    property Path: string read GetPath write SetPath;
    property SelectedFilePaths: TStrings read GetSelectedFilePaths;
  end;

  { TcxShellListView }

  TcxShellListView = class(TcxCustomShellListView)
  published
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragDropSettings;
    property Enabled;
    property IconOptions;
    property ListHotTrack;
    property MultiSelect;
    property Options;
    property ParentBiDiMode;
    property ParentColor default False;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property Root;
    property ShowColumnHeaders;
    property ShowHint;
    property Sorting;
    property Style;
    property StyleDisabled;
    property StyleFocused;
    property StyleHot;
    property TabOrder;
    property TabStop;
    property ThumbnailOptions;
    property ViewStyle;
    property Visible;
    property OnAddFolder;
    property OnBeforeNavigation;
    property OnChange;
    property OnClick;
    property OnCompare;
    property OnContextPopup;
    property OnCurrentFolderChanged;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEdited;
    property OnEditing;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExecuteItem;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnSelectItem;
    property OnShellChange;
    property OnStartDock;
    property OnStartDrag;
  end;

  TcxShellSpecialFolderInfoTableItem = record
    Attributes: ULONG;
    PIDL: PItemIDList;
    PIDLDisplayName, PIDLName, PIDLUpperCaseDisplayName: string;
  end;

function CheckAbsolutePIDL(var APIDL: PItemIDList; ARoot: TcxCustomShellRoot;
  ACheckObjectExistence: Boolean; ACheckIsSubPath: Boolean = True): Boolean;
function CheckShellObjectExistence(APIDL: PItemIDList): Boolean;
function CheckShellObjectPath(var APath: string; ACurrentPath: string;
  AIsDisplayText: Boolean): TcxShellObjectPathType;
function CheckViewOptions(AViewOptions: TcxShellViewOptions;
  AShellObjectAttributes: ULONG): Boolean;
function GetPIDLDisplayName(APIDL: PItemIDList; AShowFullPath: Boolean = False): string;
function InternalParseDisplayName(AParentIFolder: IShellFolder;
  ADisplayName: string; AViewOptions: TcxShellViewOptions): PItemIDList;
function PathToAbsolutePIDL(APath: string; ARoot: TcxCustomShellRoot;
  AViewOptions: TcxShellViewOptions; ACheckIsSubPath: Boolean = True): PItemIDList;
function ShellObjectInternalVirtualPathToPIDL(APath: string;
  ARoot: TcxCustomShellRoot; AViewOptions: TcxShellViewOptions): PItemIDList;

const
  cxShellSpecialFolderInfoTableLength = CSIDL_HISTORY - CSIDL_DESKTOP + 1;

var
  cxShellSpecialFolderInfoTable: array[CSIDL_DESKTOP..CSIDL_HISTORY] of
    TcxShellSpecialFolderInfoTableItem;

implementation

uses
  Variants, SysUtils, CommCtrl, ComObj, Graphics, Types, ShellAPI,
  cxClasses;

type
  TcxCustomDataBindingAccess = class(TcxCustomDataBinding);
  TcxContainerAccess = class(TcxContainer);
  TcxCustomShellRootAccess = class(TcxCustomShellRoot);

function CheckAbsolutePIDL(var APIDL: PItemIDList; ARoot: TcxCustomShellRoot;
  ACheckObjectExistence: Boolean; ACheckIsSubPath: Boolean = True): Boolean;
begin
  CheckShellRoot(ARoot);
  if APIDL = nil then
  begin
    Result := True;
    APIDL := ARoot.Pidl;
  end
  else
  begin
    Result := not ACheckIsSubPath or IsSubPath(ARoot.Pidl, APIDL);
    if Result and ACheckObjectExistence then
      Result := CheckShellObjectExistence(APIDL);
  end;
end;

function CheckShellObjectExistence(APIDL: PItemIDList): Boolean;
var
  ASHFileInfo: TSHFileInfo;
begin
  Result := SHGetFileInfo(PChar(APIDL), 0, ASHFileInfo, SizeOf(ASHFileInfo),
    SHGFI_PIDL or SHGFI_SYSICONINDEX or SHGFI_SMALLICON) <> 0;
end;

function CheckShellObjectPath(var APath: string; ACurrentPath: string;
  AIsDisplayText: Boolean): TcxShellObjectPathType;
var
  APathLength: Integer;
  S: string;
begin
  APathLength := Length(APath);
  Result := sptIncorrect;
  if APathLength = 0 then
    Exit;

  if (APathLength > 1) and (APath[APathLength] = '\') and (APath[APathLength - 1] <> ':') then
  begin
    Dec(APathLength);
    SetLength(APath, APathLength);
  end;

  if (APathLength > 2) and (APath[1] = '\') and (APath[2] = '\') then
  begin
    Result := sptUNC;
    Exit;
  end;
  if APathLength >= cxShellObjectInternalVirtualPathPrefixLength then
  begin
    S := AnsiUpperCase(Copy(APath, 1, cxShellObjectInternalVirtualPathPrefixLength));
    if S = cxShellObjectInternalAbsoluteVirtualPathPrefix then
    begin
      Result := sptInternalAbsoluteVirtual;
      Exit;
    end;
    if S = cxShellObjectInternalRelativeVirtualPathPrefix then
    begin
      Result := sptInternalRelativeVirtual;
      Exit;
    end;
    if Copy(S, 1, 3) = '::{' then
    begin
      Result := sptVirtual;
      Exit;
    end;
  end;
  if (Length(APath) >= 3) and (APath[2] = ':') and (APath[3] = '\') and
    dxCharInSet(APath[1], ['A'..'Z', 'a'..'z']) then
  begin
    Result := sptAbsolutePhysical;
    Exit;
  end;
  if (APath[1] = '\') or (Length(APath) >= 2) and (APath[2] = ':') and
    dxCharInSet(APath[1], ['A'..'Z', 'a'..'z']) then
  begin
    if (Length(ACurrentPath) < 3) or (ACurrentPath[2] <> ':') or
        (ACurrentPath[3] <> '\') or not dxCharInSet(ACurrentPath[1], ['A'..'Z', 'a'..'z']) then
      Exit;
    if (APath[1] <> '\') and (UpperCase(APath[1]) <> UpperCase(ACurrentPath[1])) then
      Exit;
    if (APath[1] <> '\') and (APathLength = 2) then
    begin
      if AIsDisplayText then
      begin
        APath := ACurrentPath;
        Result := sptAbsolutePhysical;
        Exit;
      end
      else
        Exit;
    end
    else
      if APath[1] = '\' then
      begin
        if APathLength = 1 then
          APath := Copy(ACurrentPath, 1, 3)
        else
          APath := Copy(ACurrentPath, 1, 2) + APath;
        Result := sptAbsolutePhysical;
        Exit;
      end
      else
        if not AIsDisplayText then
          Exit
        else
        begin
          APath := Copy(APath, 3, APathLength - 2);
          Result := sptRelativePhysical;
          Exit;
        end;
  end;
  Result := sptRelativePhysical;
end;

function CheckViewOptions(AViewOptions: TcxShellViewOptions;
  AShellObjectAttributes: ULONG): Boolean;
begin
  Result := not((AShellObjectAttributes and SFGAO_HIDDEN <> 0) and
    not(svoShowHidden in AViewOptions));
  Result := Result and not((AShellObjectAttributes and SFGAO_FOLDER = 0) and
    not(svoShowFiles in AViewOptions));
  Result := Result and not((AShellObjectAttributes and SFGAO_FOLDER <> 0) and
    not(svoShowFolders in AViewOptions));
end;

procedure DestroyShellSpecialFolderInfoTable;
var
  ACSIDL: Integer;
begin
  for ACSIDL := CSIDL_DESKTOP to CSIDL_HISTORY do
  begin
    DisposePidl(cxShellSpecialFolderInfoTable[ACSIDL].PIDL);
    cxShellSpecialFolderInfoTable[ACSIDL].PIDL := nil;
  end;
end;

function GetShellEnumObjectsFlags(AViewOptions: TcxShellViewOptions): DWORD;
begin
  Result := 0;
  if svoShowFiles in AViewOptions then
    Result := Result or SHCONTF_NONFOLDERS;
  if svoShowFolders in AViewOptions then
    Result := Result or SHCONTF_FOLDERS;
  if svoShowHidden in AViewOptions then
    Result := Result or SHCONTF_INCLUDEHIDDEN;
end;

function GetPIDLDisplayName(APIDL: PItemIDList; AShowFullPath: Boolean = False): string;
var
  AParentIFolder: IShellFolder;
  AStrRet: TStrRet;
  ATempPIDL: PItemIDList;
  I: Integer;
begin
  Result := '';
  if AShowFullPath then
  begin
    Result := GetPidlName(APIDL);
    if Result <> '' then
      Exit;
  end;
  AParentIFolder := GetDesktopIShellFolder;
  for I := 1 to GetPidlItemsCount(APIDL) + Integer(AShowFullPath) - 1 do
  begin
    ATempPIDL := cxMalloc.Alloc(APIDL^.mkid.cb + 2);
    ZeroMemory(ATempPIDL, APIDL^.mkid.cb + 2);
    CopyMemory(ATempPIDL, APIDL, APIDL^.mkid.cb);
    APIDL := ShiftPointer(APIDL, APIDL^.mkid.cb);

    if AShowFullPath then
    begin
      if AParentIFolder.GetDisplayNameOf(ATempPIDL, SHGDN_INFOLDER, AStrRet) <> S_OK then
        Break;
      if Result <> '' then
        Result := Result + '\';
      Result := Result + GetTextFromStrRet(AStrRet, APIDL);
    end;

    AParentIFolder.BindToObject(ATempPIDL, nil, IID_IShellFolder, Pointer(AParentIFolder));
    DisposePidl(ATempPIDL);
  end;

  if not AShowFullPath and (AParentIFolder.GetDisplayNameOf(APIDL, SHGDN_NORMAL, AStrRet) = S_OK) then
    Result := GetTextFromStrRet(AStrRet, APIDL);
end;

function InternalParseDisplayName(AParentIFolder: IShellFolder;
  ADisplayName: string; AViewOptions: TcxShellViewOptions): PItemIDList;

  function GetItemPidlThroughEnumObjects: PItemIDList;
  var
    AFetchedItemCount, AFlags: Cardinal;
    AIEnumIDList: IEnumIDList;
    //AStrRet: TStrRet;
    ATempPIDL: PItemIDList;
  begin
    Result := nil;
    AFlags := GetShellEnumObjectsFlags(AViewOptions);
    if (AParentIFolder.EnumObjects(0, AFlags, AIEnumIDList) = S_OK) and
      Assigned(AIEnumIDList) then
    begin
      ADisplayName := AnsiUpperCase(ADisplayName);
      while AIEnumIDList.Next(1, ATempPIDL, AFetchedItemCount) = NOERROR do
      begin
    //      FillChar(AStrRet, SizeOf(AStrRet), 0);
    //      AParentIFolder.GetDisplayNameOf(ATempPIDL, SHGDN_INFOLDER, AStrRet);
    //      if AnsiUpperCase(GetTextFromStrRet(AStrRet, ATempPIDL)) = ADisplayName then
        if AnsiUpperCase(GetShellItemDisplayName(AParentIFolder, ATempPIDL, True)) = ADisplayName then
        begin
          Result := ATempPIDL;
          Break;
        end
        else
          DisposePidl(ATempPIDL);
      end;
    end;
  end;

var
  AAttributes, AParsedCharCount: Cardinal;
begin
  Result := nil;
  AAttributes := SFGAO_HIDDEN or SFGAO_FOLDER;
  AParentIFolder.ParseDisplayName(0, nil, PWideChar(ADisplayName),
    AParsedCharCount, Result, AAttributes);
  if (Result = nil) or not CheckViewOptions(AViewOptions, AAttributes) then
    Result := GetItemPidlThroughEnumObjects;
end;

function PathToAbsolutePIDL(APath: string; ARoot: TcxCustomShellRoot;
  AViewOptions: TcxShellViewOptions; ACheckIsSubPath: Boolean = True): PItemIDList;

  function InternalPathToAbsolutePIDL: PItemIDList;
  var
    ACSIDL: Integer;
    APathType: TcxShellObjectPathType;
    ATempPIDL: PItemIDList;
  begin
    Result := nil;
    APathType := CheckShellObjectPath(APath, AnsiUpperCase(GetPidlName(ARoot.Pidl)), False);
    case APathType of
      sptIncorrect:
        Exit;
      sptAbsolutePhysical, sptUNC, sptVirtual:
        Result := InternalParseDisplayName(GetDesktopIShellFolder, APath, AViewOptions);
      sptInternalAbsoluteVirtual, sptInternalRelativeVirtual:
        Result := ShellObjectInternalVirtualPathToPIDL(APath, ARoot, AViewOptions);
      sptRelativePhysical:
        begin
          ATempPIDL := InternalParseDisplayName(ARoot.ShellFolder, APath, AViewOptions);
          if ATempPIDL <> nil then
          begin
            Result := ConcatenatePidls(ARoot.Pidl, ATempPIDL);
            DisposePidl(ATempPIDL);
            Exit;
          end;

          for ACSIDL := CSIDL_DESKTOP to CSIDL_HISTORY do
            with cxShellSpecialFolderInfoTable[ACSIDL] do
              if (PIDL <> nil) and (PIDLUpperCaseDisplayName = APath) and
                CheckViewOptions(AViewOptions, Attributes) then
              begin
                Result := GetPidlCopy(PIDL);
                Break;
              end;
        end;
    end;
  end;

begin
  CheckShellRoot(ARoot);

  if APath = '' then
    Result := GetPidlCopy(ARoot.Pidl)
  else
  begin
    APath := AnsiUpperCase(APath);
    Result := InternalPathToAbsolutePIDL;
  end;

  if (Result <> nil) and ACheckIsSubPath and not IsSubPath(ARoot.Pidl, Result) then
  begin
    DisposePidl(Result);
    Result := nil;
  end;
end;

function ShellObjectInternalVirtualPathToPIDL(APath: string;
  ARoot: TcxCustomShellRoot; AViewOptions: TcxShellViewOptions): PItemIDList;
var
  AAttributes: UINT;
  AFetchedItemCount: ULONG;
  AFlags: DWORD;
  AIEnumIDList: IEnumIDList;
  AParentIFolder: IShellFolder;
  AStrRet: TStrRet;
  ATempPIDL, ATempPIDL1, ATempPIDL2: PItemIDList;
  I: Integer;
  S: string;
begin
  Result := nil;

  if Copy(APath, 1, cxShellObjectInternalVirtualPathPrefixLength) = cxShellObjectInternalAbsoluteVirtualPathPrefix then
  begin
    AParentIFolder := GetDesktopIShellFolder;
    ATempPIDL := nil;
  end
  else
  begin
    AParentIFolder := ARoot.ShellFolder;
    ATempPIDL := GetPidlCopy(ARoot.Pidl);
  end;
  APath := Copy(APath, cxShellObjectInternalVirtualPathPrefixLength + 2,
    Length(APath) - cxShellObjectInternalVirtualPathPrefixLength - 1);
  if APath = '' then
  begin
    Result := CreateEmptyPidl;
    Exit;
  end;

  repeat
    I := Pos('\', APath);
    if I = 0 then
    begin
      S := APath;
      APath := '';
    end
    else
    begin
      S := Copy(APath, 1, I - 1);
      APath := Copy(APath, I + 1, Length(APath) - I);
    end;

    AFlags := GetShellEnumObjectsFlags(AViewOptions);
    if (AParentIFolder.EnumObjects(0, AFlags, AIEnumIDList) <> S_OK) or
      not Assigned(AIEnumIDList) then
    begin
      DisposePidl(ATempPIDL);
      Exit;
    end;
    while AIEnumIDList.Next(1, ATempPIDL1, AFetchedItemCount) = NOERROR do
    begin
      FillChar(AStrRet, SizeOf(AStrRet), 0);
      AParentIFolder.GetDisplayNameOf(ATempPIDL1, SHGDN_INFOLDER, AStrRet);
      if AnsiUpperCase(GetTextFromStrRet(AStrRet, ATempPIDL1)) = S then
      begin
        if APath = '' then
        begin
          Result := ConcatenatePidls(ATempPIDL, ATempPIDL1);
          DisposePidl(ATempPIDL);
          DisposePidl(ATempPIDL1);
          Exit;
        end;
        AAttributes := SFGAO_FOLDER;
        AParentIFolder.GetAttributesOf(1, ATempPIDL1, AAttributes);
        if AAttributes and SFGAO_FOLDER = 0 then
        begin
          DisposePidl(ATempPIDL);
          DisposePidl(ATempPIDL1);
          Exit;
        end;
        AParentIFolder.BindToObject(ATempPIDL1, nil, IID_IShellFolder, Pointer(AParentIFolder));
        ATempPIDL2 := ATempPIDL;
        ATempPIDL := ConcatenatePidls(ATempPIDL, ATempPIDL1);
        DisposePidl(ATempPIDL1);
        DisposePidl(ATempPIDL2);
        Break;
      end;
    end;
  until I = 0;
end;

procedure PrepareShellSpecialFolderInfoTable;
var
  ACSIDL: Integer;
  ADesktopIFolder: IShellFolder;
  ATempPIDL: PItemIDList;
begin
  ADesktopIFolder := GetDesktopIShellFolder;
  for ACSIDL := CSIDL_DESKTOP to CSIDL_HISTORY do
    with cxShellSpecialFolderInfoTable[ACSIDL] do
    begin
      if SHGetSpecialFolderLocation(0, ACSIDL, PIDL) <> S_OK then
      begin
        Attributes := 0;
        PIDL := nil;
        PIDLDisplayName := '';
        PIDLName := '';
        PIDLUpperCaseDisplayName := '';
        Continue;
      end;

      PIDLDisplayName := GetPIDLDisplayName(PIDL);
      PIDLUpperCaseDisplayName := AnsiUpperCase(PIDLDisplayName);
      PIDLName := AnsiUpperCase(GetPidlName(PIDL));

      if PIDL <> nil then
      begin
        Attributes := SFGAO_HIDDEN or SFGAO_FOLDER;
        ATempPIDL := GetLastPidlItem(PIDL);
        if ADesktopIFolder.GetAttributesOf(1, ATempPIDL, Attributes) <> NOERROR then
          raise EcxEditError.Create('');
        Attributes := Attributes and (SFGAO_HIDDEN or SFGAO_FOLDER);
      end;
    end;
end;

{ TcxInnerShellListView }

constructor TcxInnerShellListView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  inherited OnChange := ChangeHandler;
end;

destructor TcxInnerShellListView.Destroy;
begin
  SaveAbsolutePIDL(nil);
  inherited Destroy;
end;

function TcxInnerShellListView.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or
    TcxCustomDataBindingAccess(Container.FDataBinding).ExecuteAction(Action);
end;

function TcxInnerShellListView.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or
    TcxCustomDataBindingAccess(Container.FDataBinding).UpdateAction(Action);
end;

procedure TcxInnerShellListView.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  if Container.IconOptions.AutoArrange then
    Params.Style := Params.Style or LVS_AUTOARRANGE
  else
    Params.Style := Params.Style and not LVS_AUTOARRANGE;
  if not Container.ShowColumnHeaders then
    Params.Style := Params.Style or LVS_NOCOLUMNHEADER;
end;

procedure TcxInnerShellListView.CreateWnd;
begin
  inherited CreateWnd;
  if FAbsolutePIDL <> nil then
    AbsolutePIDL := FAbsolutePIDL;
end;

procedure TcxInnerShellListView.DestroyWnd;
var
  ATempPIDL: PItemIDList;
begin
  ATempPIDL := AbsolutePIDL;
  try
    SaveAbsolutePIDL(ATempPIDL);
  finally
    DisposePidl(ATempPIDL);
  end;
  inherited DestroyWnd;
end;

function TcxInnerShellListView.DoCompare(AItem1, AItem2: TcxShellFolder;
  out ACompare: Integer): Boolean;
begin
  Result := Container.DoCompare(AItem1, AItem2, ACompare);
end;

procedure TcxInnerShellListView.Navigate(APIDL: PItemIDList);
begin
  inherited Navigate(APIDL);
  if HandleAllocated then
  begin
    SendMessage(Handle, WM_HSCROLL, MakeWParam(SB_LEFT, 0), 0);
    SendMessage(Handle, WM_VSCROLL, MakeWParam(SB_TOP, 0), 0);
  end;
end;

function TcxInnerShellListView.NeedCheckScrollBars(var Message: TMessage): Boolean;
begin
  Result := inherited NeedCheckScrollBars(Message) or
    (Message.Msg = DSM_NOTIFYUPDATECONTENTS) or
    (Message.Msg = DSM_NOTIFYUPDATE) or
    (Message.Msg = WM_SETREDRAW) and (Message.WParam <> 0);
end;

procedure TcxInnerShellListView.WndProc(var Message: TMessage);
begin
  if (Container <> nil) and Container.InnerControlMenuHandler(Message) then
    Exit;

  if Container <> nil then
    if ((Message.Msg = WM_LBUTTONDOWN) or (Message.Msg = WM_LBUTTONDBLCLK)) and
      (Container.DragMode = dmAutomatic) and not Container.IsDesigning then
    begin
      Container.BeginAutoDrag;
      Exit;
    end;

  inherited WndProc(Message);
end;

procedure TcxInnerShellListView.ChangeHandler(Sender: TObject; AItem: TListItem;
  AChange: TItemChange);
begin
  if AItem <> nil then
    try
      if Assigned(FOnChange) then
        FOnChange(Sender, AItem, AChange);
    finally
      Container.SetScrollBarsParameters;
    end;
end;

function TcxInnerShellListView.GetAbsolutePIDL: PItemIDList;
begin
  if HandleAllocated then
  begin
    CheckShellRoot(Root);
    Result := GetPidlCopy(Root.Pidl);
  end
  else
    Result := GetPidlCopy(FAbsolutePIDL);
end;

function TcxInnerShellListView.GetContainer: TcxCustomShellListView;
begin
  Result := TcxCustomShellListView(FContainer);
end;

function TcxInnerShellListView.GetPath: string;
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

procedure TcxInnerShellListView.SaveAbsolutePIDL(AValue: PItemIDList);
var
  ATempPIDL: PItemIDList;
begin
  ATempPIDL := GetPidlCopy(AValue);
  DisposePidl(FAbsolutePIDL);
  FAbsolutePIDL := ATempPIDL;
end;

procedure TcxInnerShellListView.SetAbsolutePIDL(AValue: PItemIDList);
begin
  if HandleAllocated then
  begin
    if CheckAbsolutePIDL(AValue, Root, True, False) then
    begin
      ProcessTreeViewNavigate(AValue);
      DoNavigateTreeView;
    end;
  end
  else
    SaveAbsolutePIDL(AValue);
end;

procedure TcxInnerShellListView.SetPath(AValue: string);
var
  APIDL: PItemIDList;
begin
  APIDL := PathToAbsolutePIDL(AValue, Root, Container.GetViewOptions(True), False);
  if APIDL <> nil then
    try
      AbsolutePIDL := APIDL;
    finally
      DisposePidl(APIDL);
    end;
end;

procedure TcxInnerShellListView.DSMShellChangeNotify(var Message: TMessage);
begin
  inherited;
  Container.SetScrollBarsParameters;
end;

{ TcxCustomShellListView }

constructor TcxCustomShellListView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSelectedFiles := TStringList.Create;
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
  Width := 250;
  Height := 150;
end;

destructor TcxCustomShellListView.Destroy;
begin
  FreeAndNil(FDataBinding);
  FreeAndNil(FSelectedFiles);
  inherited Destroy;
end;

function TcxCustomShellListView.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or
    TcxCustomDataBindingAccess(FDataBinding).ExecuteAction(Action);
end;

function TcxCustomShellListView.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or
    TcxCustomDataBindingAccess(FDataBinding).UpdateAction(Action);
end;

procedure TcxCustomShellListView.SetFocus;
begin
  if not IsDesigning then
    inherited SetFocus;
end;

procedure TcxCustomShellListView.BrowseParent;
begin
  InnerListView.BrowseParent;
end;

function TcxCustomShellListView.GetItemAbsolutePIDL(AIndex: Integer): PItemIDList;
begin
  CheckShellRoot(Root);
  Result := InnerListView.GetPidlByItemIndex(AIndex);
  Result := ConcatenatePidls(Root.Pidl, Result);
end;

procedure TcxCustomShellListView.ProcessTreeViewNavigate(APidl: PItemIDList);
begin
  InnerListView.ProcessTreeViewNavigate(APidl);
end;

procedure TcxCustomShellListView.Sort;
begin
  InnerListView.Sort;
end;

procedure TcxCustomShellListView.Sort(AColumnIndex: Integer; AIsAscending: Boolean);
begin
  InnerListView.Sort(AColumnIndex, AIsAscending);
end;

procedure TcxCustomShellListView.UpdateContent;
begin
  InnerListView.UpdateContent;
end;

procedure TcxCustomShellListView.DoExit;
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

procedure TcxCustomShellListView.Loaded;
begin
  inherited Loaded;
  InnerListView.Loaded;
  SetScrollBarsParameters;
end;

procedure TcxCustomShellListView.CurrentFolderChangedHandler(Sender: TObject; Root: TcxCustomShellRoot);
begin
  try
    if Assigned(FOnCurrentFolderChanged) then
      FOnCurrentFolderChanged(Self);
  finally
    SetScrollBarsParameters;
  end;
end;

function TcxCustomShellListView.GetDataBindingClass: TcxCustomDataBindingClass;
begin
  Result := TcxDataBinding;
end;

function TcxCustomShellListView.GetIconOptionsClass: TcxIconOptionsClass;
begin
  Result := TcxShellIconOptions;
end;

function TcxCustomShellListView.GetInnerListViewClass: TcxInnerListViewClass;
begin
  Result := TcxInnerShellListView;
end;

procedure TcxCustomShellListView.InitializeInnerListView;
begin
  inherited InitializeInnerListView;
  with InnerListView do
  begin
    BeforeNavigation := Self.BeforeNavigationHandler;
    OnAddFolder := Self.AddFolderHandler;
    OnChange := Self.ChangeHandler;
    InnerListView.OnEdited := Self.EditedHandler;
    InnerListView.OnEditing := Self.EditingHandler;
    OnExecuteItem := ExecuteItemHandler;
    OnRootChanged := Self.CurrentFolderChangedHandler;
    OnSelectItem := Self.SelectItemHandler;
    OnShellChange := Self.ShellChangeHandler;
  end;
end;

function TcxCustomShellListView.GetDependedControls: TcxShellDependedControls;
var
  ADependedControls: IcxShellDependedControls;
begin
  if Supports(InnerControl, IcxShellDependedControls, ADependedControls) then
    Result := ADependedControls.GetDependedControls
  else
    Result := nil;
end;

function TcxCustomShellListView.GetSelectedFilePaths: TStrings;
var
  AItem: TListItem;
begin
  FSelectedFiles.Clear;
  AItem := InnerListView.Selected;
  while AItem <> nil do
  begin
    FSelectedFiles.Add(Folders[AItem.Index].PathName);
    AItem := InnerListView.GetNextItem(AItem, sdAll, [isSelected]);
  end;
  Result := FSelectedFiles;
end;

function TcxCustomShellListView.GetShellRoot: TcxCustomShellRoot;
var
  ARoot: IcxShellRoot;
begin
  if Supports(InnerControl, IcxShellRoot, ARoot) then
    Result := ARoot.GetRoot
  else
    Result := nil;
end;

function TcxCustomShellListView.GetViewOptions(AForNavigation: Boolean = False): TcxShellViewOptions;
begin
  if AForNavigation then
    Result := [svoShowFolders, svoShowHidden]
  else
    with InnerListView do
      begin
        Result := [];
        if Options.ShowNonFolders then
          Include(Result, svoShowFiles);
        if Options.ShowFolders then
          Include(Result, svoShowFolders);
        if Options.ShowHidden then
          Include(Result, svoShowHidden);
      end;
end;

procedure TcxCustomShellListView.SetViewStyle(Value: TViewStyle);
begin
  InnerListView.ListViewStyle := TcxListViewStyle(Value);
  SetScrollBarsParameters;
end;

procedure TcxCustomShellListView.WndProc(var Message: TMessage);
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

procedure TcxCustomShellListView.SetThumbnailOptions(
  const Value: TcxShellThumbnailOptions);
begin
  InnerListView.ThumbnailOptions := Value;
end;

procedure TcxCustomShellListView.AddFolderHandler(Sender: TObject;
  AFolder: TcxShellFolder; var ACanAdd: Boolean);
begin
  if Assigned(FOnAddFolder) then
    FOnAddFolder(Self, AFolder, ACanAdd);
end;

procedure TcxCustomShellListView.BeforeNavigationHandler(Sender: TcxCustomInnerShellListView;
  APItemIDList: PItemIDList; AFolderPath: WideString);
begin
  if Assigned(FOnBeforeNavigation) then
    FOnBeforeNavigation(Self, APItemIDList);
end;

procedure TcxCustomShellListView.ChangeHandler(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  if Assigned(FOnChange) then
    FOnChange(Self, Item, Change);
end;

procedure TcxCustomShellListView.EditedHandler(Sender: TObject; Item: TListItem;
  var S: string);
begin
  try
    if Assigned(FOnEdited) then
      FOnEdited(Self, Item, S);
  finally
    SetScrollBarsParameters;
  end;
end;

procedure TcxCustomShellListView.EditingHandler(Sender: TObject; Item: TListItem;
  var AllowEdit: Boolean);
begin
  if Assigned(FOnEditing) then
    FOnEditing(Self, Item, AllowEdit);
end;

procedure TcxCustomShellListView.ExecuteItemHandler(Sender: TObject; APIDL: PItemIDList; var AHandled: Boolean);
begin
  if Assigned(OnExecuteItem) then
    OnExecuteItem(Self, APIDL, AHandled);
end;

procedure TcxCustomShellListView.SelectItemHandler(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  if Assigned(FOnSelectItem) then
    FOnSelectItem(Self, Item, Selected);
end;

procedure TcxCustomShellListView.ShellChangeHandler(Sender: TObject;
  AEventID: DWORD; APIDL1, APIDL2: PItemIDList);
begin
  if Assigned(FOnShellChange) then
    FOnShellChange(Self, AEventID, APIDL1, APIDL2);
end;

function TcxCustomShellListView.DoCompare(AItem1, AItem2: TcxShellFolder;
  out ACompare: Integer): Boolean;
begin
  Result := Assigned(FOnCompare);
  ACompare := 0;
  if Result then
    FOnCompare(Self, AItem1, AItem2, ACompare);
end;

function TcxCustomShellListView.GetAbsolutePIDL: PItemIDList;
begin
  Result := nil;
  if not IsDestroying then
    Result := InnerListView.AbsolutePIDL;
end;

function TcxCustomShellListView.GetDragDropSettings: TcxDragDropSettings;
begin
  Result := TcxDragDropSettings(InnerListView.DragDropSettings);
end;

function TcxCustomShellListView.GetFolder(AIndex: Integer): TcxShellFolder;
begin
  Result := InnerListView.Folders[AIndex];
end;

function TcxCustomShellListView.GetFolderCount: Integer;
begin
  Result := InnerListView.FolderCount;
end;

function TcxCustomShellListView.GetIconOptions: TcxShellIconOptions;
begin
  Result := TcxShellIconOptions(inherited IconOptions);
end;

function TcxCustomShellListView.GetInnerListView: TcxInnerShellListView;
begin
  Result := TcxInnerShellListView(inherited InnerListView);
end;

function TcxCustomShellListView.GetListHotTrack: Boolean;
begin
  Result := InnerListView.HotTrack;
end;

function TcxCustomShellListView.GetOptions: TcxShellListViewOptions;
begin
  Result := InnerListView.Options;
end;

function TcxCustomShellListView.GetPath: string;
begin
  Result := '';
  if not IsDestroying then
    Result := InnerListView.Path;
end;

function TcxCustomShellListView.GetRoot: TcxShellListRoot;
begin
  Result := TcxShellListRoot(InnerListView.Root)
end;

function TcxCustomShellListView.GetSorting: Boolean;
begin
  Result := InnerListView.Sorting;
end;

function TcxCustomShellListView.GetThumbnailOptions: TcxShellThumbnailOptions;
begin
  Result := InnerListView.ThumbnailOptions;
end;

procedure TcxCustomShellListView.SetAbsolutePIDL(Value: PItemIDList);
begin
  if not IsDestroying then
    InnerListView.AbsolutePIDL := Value;
end;

procedure TcxCustomShellListView.SetDragDropSettings(Value: TcxDragDropSettings);
begin
  InnerListView.DragDropSettings := Value;
end;

procedure TcxCustomShellListView.SetIconOptions(Value: TcxShellIconOptions);
begin
  inherited IconOptions := Value;
end;

procedure TcxCustomShellListView.SetListHotTrack(Value: Boolean);
begin
  InnerListView.HotTrack := Value;
end;

procedure TcxCustomShellListView.SetOptions(Value: TcxShellListViewOptions);
begin
  InnerListView.Options.Assign(Value);
end;

procedure TcxCustomShellListView.SetPath(Value: string);
begin
  if not IsDestroying then
    InnerListView.Path := Value;
end;

procedure TcxCustomShellListView.SetRoot(Value: TcxShellListRoot);
begin
  InnerListView.Root := Value;
end;

procedure TcxCustomShellListView.SetSorting(const Value: Boolean);
begin
  InnerListView.Sorting := Value;
end;

{ TcxShellIconOptions }

procedure TcxShellIconOptions.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TcxShellIconOptions then
    Size := TcxShellIconOptions(Source).Size;
end;

function TcxShellIconOptions.GetSize: TcxShellIconSize;
begin
  Result := TcxCustomShellListView(ListViewContainer).InnerListView.LargeIconSize;
end;

procedure TcxShellIconOptions.SetSize(const Value: TcxShellIconSize);
begin
  TcxCustomShellListView(ListViewContainer).InnerListView.LargeIconSize := Value;
end;

initialization
  PrepareShellSpecialFolderInfoTable;

finalization
  DestroyShellSpecialFolderInfoTable;

end.
