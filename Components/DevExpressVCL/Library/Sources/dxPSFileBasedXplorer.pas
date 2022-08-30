{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressPrinting System                                   }
{                                                                    }
{           Copyright (C) 1998-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSPRINTING SYSTEM AND            }
{   ALL ACCOMPANYING VCL CONTROLS AS PART OF AN                      }
{   EXECUTABLE PROGRAM ONLY.                                         }
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

unit dxPSFileBasedXplorer;

interface

{$I cxVer.inc}

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Windows, Classes, SysUtils, cxClasses, dxPSGlbl, dxPSCore, dxCore, IniFiles;

type
  TdxPSFileBasedExplorer = class;
  TdxPSFileBasedExplorerItem = class;

  TdxPSFileBasedExplorerFolder = class(TdxPSExplorerFolder)
  private
    function GetFolder(Index: Integer): TdxPSFileBasedExplorerFolder;
    function GetFullQualifiedDirName: string;
    function GetFullQualifiedDirPath: string;
    function GetIsVolume: Boolean;
    function GetItem(Index: Integer): TdxPSFileBasedExplorerItem;
  protected
    function DoDelete: Boolean; override;
    function DoMove(AParent: TdxPSExplorerFolder): Boolean; override;
    function DoRename(var ANewName: string): Boolean; override;

    function GetDirName: string; virtual;
    function GetDisplayName: string; override;
  public
    function CannotRenameMessageText(const AOldName, ANewName: string): string; override;
    function CanMoveTo(AParent: TCustomdxPSExplorerItem): Boolean; override;
    function CanRenameTo(const AName: string): Boolean; override;
    function Explorer: TdxPSFileBasedExplorer; reintroduce; overload;
    function GetUniqueID(out AnUniqueID: TBytes): Integer; override;
    function ItemByName(const AName: string): TdxPSExplorerItem; override;

    function FindFolderByFullQualifiedDirName(const AFullQualifiedDirName: string): TdxPSFileBasedExplorerFolder;
    function FindItemByFullQualifiedFileName(const AFullQualifiedName: string): TdxPSFileBasedExplorerItem;

    property DirName: string read GetDirName;
    property Folders[Index: Integer]: TdxPSFileBasedExplorerFolder read GetFolder; default;
    property FullQualifiedDirName: string read GetFullQualifiedDirName;
    property FullQualifiedDirPath: string read GetFullQualifiedDirPath;
    property IsVolume: Boolean read GetIsVolume;
    property Items[Index: Integer]: TdxPSFileBasedExplorerItem read GetItem;
  end;

  TdxPSFileBasedExplorerRootFolder = class(TdxPSFileBasedExplorerFolder)
  private
    function GetDriveType: TdxDriveType;
    function GetVolumeLabel: string;
  protected
    function GetImageIndex: Integer; override;
    function GetSelectedIndex: Integer; override;
  public
    property VolumeLabel: string read GetVolumeLabel;
    property DriveType: TdxDriveType read GetDriveType;
  end;

  TdxPSFileBasedExplorerItem = class(TdxPSExplorerItem)
  private
    FIsIOOutwardlyControlled: Boolean;
    function GetFullQualifiedFileName: string;
    function GetFullQualifiedFilePath: string;
  protected
    function AcquireExtension(const AName: string): string;
    function SuppressExtension(const AName: string): string;

    function DoDelete: Boolean; override;
    function DoMove(AParent: TdxPSExplorerFolder): Boolean; override;
    function DoRename(var ANewName: string): Boolean; override;

    function GetDisplayName: string; override;
    function GetFileName: string; virtual;
    function GetFileSize: Int64; virtual; // in bytes
    function GetFormCaption: string; override;
    function GetInfoTip: string; override;
    function GetNewName(AReportLink: TBasedxReportLink): string; override;
    procedure SetName(const Value: string); override;

    property IsIOOutwardlyControlled: Boolean read FIsIOOutwardlyControlled write FIsIOOutwardlyControlled;
  public
    function CannotRenameMessageText(const AOldName, ANewName: string): string; override;
    function CanMoveTo(AParent: TCustomdxPSExplorerItem): Boolean; override;
    function CanRenameTo(const AName: string): Boolean; override;
    function DataLoadErrorText: string; override;
    function Explorer: TdxPSFileBasedExplorer; reintroduce; overload;
    function GetUniqueID(out AnUniqueID: TBytes): Integer; override;
    function IsNameChanged(const ANewName: string): Boolean; override;

    property FileName: string read GetFileName;
    property FileSize: Int64 read GetFileSize;
    property FullQualifiedFileName: string read GetFullQualifiedFileName;
    property FullQualifiedFilePath: string read GetFullQualifiedFilePath;
  end;

  TdxPSFileBasedExplorerContextCommandClass = class of TdxPSFileBasedExplorerContextCommand;

  TdxPSFileBasedExplorerContextCommand = class(TCustomdxPSExplorerContextCommand)
  public
    function Explorer: TdxPSFileBasedExplorer; reintroduce; overload;
  end;

  TdxPSFileBasedExplorerSetAsRootContextCommand = class(TdxPSFileBasedExplorerContextCommand)
  public
    constructor Create(AnExplorer: TCustomdxPSExplorer); override;
    function Enabled: Boolean; override;
    procedure Execute; override;
  end;

  TdxPSFileBasedExplorerChangeRootContextCommand = class(TdxPSFileBasedExplorerContextCommand)
  public
    constructor Create(AnExplorer: TCustomdxPSExplorer); override;
    function Enabled: Boolean; override;
    procedure Execute; override;
  end;

  TdxPSFileBasedExplorerGoToUpOneLevelContextCommand = class(TdxPSFileBasedExplorerContextCommand)
  public
    constructor Create(AnExplorer: TCustomdxPSExplorer); override;
    function Enabled: Boolean; override;
    procedure Execute; override;
  end;

  TdxPSFileBasedExplorerLoadErrorEvent = procedure(Sender: TdxPSFileBasedExplorer;
    const AName: string) of object;

  TdxPSFileBasedExplorerOption = (eoLoadAll, eoShowIOErrors, eoStoreToRegistry);
  TdxPSFileBasedExplorerOptions = set of TdxPSFileBasedExplorerOption;

  { TdxPSFileBasedExplorer }

  TdxPSFileBasedExplorer = class(TCustomdxPSExplorer)
  private
    FIOLockCounter: Integer;
    FIOStatus: Word;
    FLastLoadedFileName: string;
    FOptions: TdxPSFileBasedExplorerOptions;
    FRootPath: string;
    FOnLoadError: TdxPSFileBasedExplorerLoadErrorEvent;
    function GetActiveFolder: TdxPSFileBasedExplorerFolder;
    function GetActiveFolderPath: string;
    function GetRealRootPath: string;
    function GetRoot: TdxPSFileBasedExplorerRootFolder;
    procedure SetActiveFolder(Value: TdxPSFileBasedExplorerFolder);
    procedure SetOptions(Value: TdxPSFileBasedExplorerOptions);
    procedure SetRootPath(const Value: string);
  protected
    procedure Loaded; override;
    procedure LoadDefaultRegistry;
    procedure SaveDefaultRegistry;
    // IdxPSExplorerBuildContextCommands
    procedure BuildCommandSet(ABuilder: IdxPSExplorerContextCommandBuilder); override;
    // IdxPSExplorerContextCommands2
    procedure FinalizeCommand(ACommand: TCustomdxPSExplorerContextCommand); override;
    procedure InitializeCommand(ACommand: TCustomdxPSExplorerContextCommand); override;

    class function AcceptItemNameChar(AnItem: TCustomdxPSExplorerItem; Ch: Char): Boolean; override;
    function AcquireExtension(const AName: string): string;
    function SuppressExtension(const AName: string): string;

    procedure CreateAndCloseFile(const AName: string);
    function CreateDataStream(const AFileName: string; AMode: TdxPSStreamMode): TStream;
    function CreateItemDataStream(AnItem: TdxPSExplorerItem; AMode: TdxPSStreamMode): TStream; override;

    procedure DoLoadData(AFolder: TdxPSExplorerFolder); override;
    procedure DoLoadError(const AName: string); dynamic;
    procedure RestoreLoadedItem; virtual;
    procedure SaveLoadedItem; virtual;

    class function GetFolderClass: TdxPSExplorerFolderClass; override;
    class function GetItemClass: TdxPSExplorerItemClass; override;
    class function GetRootFolderClass: TdxPSExplorerFolderClass; override;
    function GetRootDisplayName: string; override;
    procedure MoveTo(AnItem: TCustomdxPSExplorerItem; AParent: TdxPSExplorerFolder); override;

    procedure BeginIO;
    procedure EndIO;

    procedure CheckIOError(AnIOResult: Boolean);
    procedure ProcessIOError(AnUnconditionalRaiseException: Boolean = False); virtual;

    property IOStatus: Word read FIOStatus write FIOStatus;
  public
    constructor Create(AOwner: TComponent); override;
    procedure BeforeDestruction; override;

    procedure LoadFromIniFile(AIniFile: TCustomIniFile; const ASection: string); overload; virtual;
    procedure LoadFromIniFile(const AFileName: string); overload;
    procedure LoadFromRegistry(const APath: string);
    procedure SaveToIniFile(AIniFile: TCustomIniFile; const ASection: string); overload; virtual;
    procedure SaveToIniFile(const AFileName: string); overload;
    procedure SaveToRegistry(const APath: string);

    class function FileExtension: string; virtual;
    procedure PopulatePath(APath: string);

    function CreateNewFolder(AParent: TdxPSExplorerFolder): TdxPSExplorerFolder; override;
    function CreateNewItem(AParent: TdxPSExplorerFolder; AReportLink: TBasedxReportLink): TdxPSExplorerItem; override;

    function FindCustomItemByUniqueID(const AnUniqueID: TBytes): TCustomdxPSExplorerItem; override;
    function FindFolderByFullQualifiedDirName(const AFullQualifiedDirName: string): TdxPSFileBasedExplorerFolder;
    function FindItemByFullQualifiedFileName(const AFullQualifiedName: string): TdxPSFileBasedExplorerItem;

    function LoadedItem: TdxPSFileBasedExplorerItem; reintroduce; overload;
    procedure LoadItemData(const AFullQualifiedFileName: string; AReportLink: TBasedxReportLink); overload;
    procedure UnloadItemData(const AFullQualifiedFileName: string); overload;

    function CanGoToUpOneLevel: Boolean; virtual;
    function ShowChangeRootPathDlg: Boolean;
    procedure GoToUpOneLevel;

    function CanSetActiveFolderAsRoot: Boolean; virtual;
    procedure SetActiveFolderAsRoot;

    property ActiveFolder: TdxPSFileBasedExplorerFolder read GetActiveFolder write SetActiveFolder;
    property ActiveFolderPath: string read GetActiveFolderPath;
    property RealRootPath: string read GetRealRootPath;
    property Root: TdxPSFileBasedExplorerRootFolder read GetRoot;
  published
    property Options: TdxPSFileBasedExplorerOptions read FOptions write SetOptions default [eoShowIOErrors];
    property RootPath: string read FRootPath write SetRootPath;
    property OnLoadError: TdxPSFileBasedExplorerLoadErrorEvent read FOnLoadError write FOnLoadError;
  end;

implementation

uses
  RTLConsts, Menus, Dialogs, Controls, Registry, ShellAPI, dxPSUtl, dxPSRes,
  dxPSImgs,dxPSEngn;

const
  sdxRootPath = 'RootPath';                     // Don't localize
  sdxRootPathSection = 'Explorers';             // Don't localize

{ Helpers }

function IsSystemObject(const ASearchRec: TSearchRec): Boolean;
begin
  Result := (ASearchRec.Name = '.') or (ASearchRec.Name = '..');
end;

function FirstPathDelimiter(const Source: string): Integer;
begin
  Result := Pos(dxPSGlbl.PathDelimiter, Source);
end;

function HasExtension(const AName, AExtension: string): Boolean;
begin
  Result := dxSameText(ExtractFileExt(AName), AExtension);
end;

procedure IOError;
begin
  RaiseLastOSError;
end;

function RemoveTrailingBackSlash(const Source: string): string;
var
  I: Integer;
begin
  I := Length(Source);
  while (I <> 0) and (Source[I] = '\') do
    Dec(I);

  if I > 0 then
    Result := Copy(Source, 1, I)
  else
    Result := '';
end;

function ShellRemoveFile(const AName: string; AConfirmation: Boolean): Boolean;
const
  ConfirmationMap: array[Boolean] of FILEOP_FLAGS = (FOF_NOCONFIRMATION, 0);
var
  FOS: TSHFileOpStruct;
begin
  FillChar(FOS, SizeOf(FOS), 0);
  with FOS do
  begin
    Wnd := 0;
    pFrom := PChar(AName + #0);
    wFunc := FO_DELETE;
    fFlags := FOF_ALLOWUNDO or FOF_NOERRORUI or FOF_SILENT or ConfirmationMap[AConfirmation];
  end;
  Result := (SHFileOperation(FOS) = 0) and not FOS.fAnyOperationsAborted;
end;

function ShellRemoveFolder(const APath: string): Boolean;
var
  FOS: TSHFileOpStruct;
begin
  FillChar(FOS, SizeOf(FOS), 0);
  with FOS do
  begin
    Wnd := 0;
    pFrom := PChar(APath + #0);
    wFunc := FO_DELETE;
    fFlags := FOF_ALLOWUNDO or FOF_NOCONFIRMATION or FOF_NOERRORUI or FOF_SILENT;
  end;
  Result := SHFileOperation(FOS) = 0;
end;

function ValidateFileName(const AName: string): Boolean;

  function HasAnyChar(const S, AChars: string): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    for I := 1 to Length(AChars) do
    begin
      Result := Pos(AChars[I], S) > 0;
      if Result then Exit;
    end;
  end;

begin
  Result := (Trim(AName) <> '') and not HasAnyChar(AName, dxPSUtl.InvalidFileNameChars);
end;

{ TdxPSFileBasedExplorerFolder }

function TdxPSFileBasedExplorerFolder.CannotRenameMessageText(const AOldName, ANewName: string): string;
begin
  if not ValidateFileName(ANewName) then
    Result := cxGetResourceString(@sdxInvalidFolderName)
  else
    Result := inherited CannotRenameMessageText(AOldName, ANewName);
end;

function TdxPSFileBasedExplorerFolder.CanMoveTo(AParent: TCustomdxPSExplorerItem): Boolean;
// Name is equal '' in creation phase
begin
  Result := inherited CanMoveTo(AParent) and (AParent <> nil) and
    ((esLoading in Explorer.State) or (Name = '') or
     CanRenameTo(TdxPSFileBasedExplorerFolder(AParent).FullQualifiedDirName + '\' + Name));
end;

function TdxPSFileBasedExplorerFolder.CanRenameTo(const AName: string): Boolean;
begin
  Result := inherited CanRenameTo(AName) and ValidateFileName(ExtractFileName(AName));
end;

function TdxPSFileBasedExplorerFolder.Explorer: TdxPSFileBasedExplorer;
begin
  Result := inherited Explorer as TdxPSFileBasedExplorer;
end;

function TdxPSFileBasedExplorerFolder.GetUniqueID(out AnUniqueID: TBytes): Integer;
begin
  Result := Length(FullQualifiedDirName);
  SetLength(AnUniqueID, Result);
  Move(Pointer(FullQualifiedDirName)^, Pointer(AnUniqueID)^, Result);
end;

function TdxPSFileBasedExplorerFolder.ItemByName(const AName: string): TdxPSExplorerItem;
begin
  Result := inherited ItemByName(Explorer.AcquireExtension(AName));
end;

function TdxPSFileBasedExplorerFolder.FindFolderByFullQualifiedDirName(const AFullQualifiedDirName: string): TdxPSFileBasedExplorerFolder;

  function InternalFindFolder(AFolder: TdxPSFileBasedExplorerFolder; ADirName: string): TdxPSFileBasedExplorerFolder;
  var
    P: Integer;
    FolderName: string;
  begin
    P := FirstPathDelimiter(ADirName);
    if P <> 0 then
    begin
      FolderName := Copy(ADirName, 1, P - 1);
      Result := TdxPSFileBasedExplorerFolder(AFolder.FolderByName(FolderName));
      if Result <> nil then
      begin
        System.Delete(ADirName, 1, P);
        Result := InternalFindFolder(Result, ADirName);
      end;
    end
    else
      Result := TdxPSFileBasedExplorerFolder(AFolder.FolderByName(ADirName));
  end;

var
  S, DirName: string;
begin
  Result := nil;
  S := dxPSUtl.GetLongFileName(AFullQualifiedDirName);
  if Pos(FullQualifiedDirName, S) = 1 then
  begin
    DirName := Copy(S, Length(FullQualifiedDirName) + 1 + 1{PathDelimiter}, Length(S));
    if DirName = '' then
      Result := Self
    else
      Result := InternalFindFolder(Self, DirName);
  end;
end;

function TdxPSFileBasedExplorerFolder.FindItemByFullQualifiedFileName(const AFullQualifiedName: string): TdxPSFileBasedExplorerItem;
var
  S: string;
  Folder: TdxPSFileBasedExplorerFolder;
  SuppressedName: string;
begin
  S := dxPSUtl.GetLongFileName(AFullQualifiedName);
  Folder := FindFolderByFullQualifiedDirName(ExtractFileDir(S));
  if Folder <> nil then
  begin
    SuppressedName := Explorer.SuppressExtension(ExtractFileName(S));
    Result := TdxPSFileBasedExplorerItem(Folder.ItemByName(SuppressedName));
  end
  else
    Result := nil;
end;

function TdxPSFileBasedExplorerFolder.DoDelete: Boolean;
begin
  Result := inherited DoDelete;
  if Result and DirectoryExists(FullQualifiedDirName) then
  begin
    ShellRemoveFolder(FullQualifiedDirName);
    Result := not DirectoryExists(FullQualifiedDirName);
  end;
end;

function TdxPSFileBasedExplorerFolder.DoMove(AParent: TdxPSExplorerFolder): Boolean;
const
  Buttons: TMsgDlgButtons = [mbYes, mbYesToAll, mbNo, mbCancel{, mbHelp}];

  function MoveFiles(AParent: TdxPSFileBasedExplorerFolder): Boolean;
  var
    I: Integer;
    Item: TdxPSFileBasedExplorerItem;
  begin
    for I := ItemCount - 1 downto 0 do
    begin
      Item := Items[I];
      Item.IsIOOutwardlyControlled := True;
      try
        if Explorer.IOStatus <> mrYesToAll then
          if FileExists(AParent.FullQualifiedDirName + '\' + Item.Name) then
          begin
            Explorer.IOStatus := MessageDlg(Item.OverwriteMessageText(AParent), mtWarning, Buttons, 0);
            if Explorer.IOStatus = mrCancel then
              Break;
            if Explorer.IOStatus = mrNo then
              Continue;
          end;
        Item.Parent := AParent;
      finally
        Item.IsIOOutwardlyControlled := False;
      end;
    end;
    Result := Explorer.IOStatus <> mrCancel;
  end;

  function MoveFolders(AParent: TdxPSFileBasedExplorerFolder): Boolean;
  var
    I: Integer;
    Folder: TdxPSExplorerFolder;
  begin
    for I := FolderCount - 1 downto 0 do
    begin
      Folder := Folders[I];
      Folder.Populate;
      Folder.Parent := AParent;
      if Explorer.IOStatus = mrCancel then Break;
    end;
    Result := Explorer.IOStatus <> mrCancel;
  end;

var
  DestName: string;
  NewParent: TdxPSFileBasedExplorerFolder;
begin
  Result := inherited DoMove(AParent);
  if Result and ([esLoading, esFolderCreating] * Explorer.State = []) then
  begin
    if Explorer.IOStatus = mrCancel then
    begin
      Result := False;
      Exit;
    end;

    if not DirectoryExists(FullQualifiedDirName) then
    begin
      Result := False;
      Delete;
      Exit;
    end;

    DestName := TdxPSFileBasedExplorerFolder(AParent).FullQualifiedDirName + '\' + DirName;
    if DirectoryExists(DestName) then
    begin
      Populate;
      NewParent := TdxPSFileBasedExplorerFolder(AParent).FindFolderByFullQualifiedDirName(DestName);
      NewParent.Populate;

      if Explorer.IOStatus <> mrYesToAll then
      begin
        Explorer.IOStatus := MessageDlg(OverwriteMessageText(AParent), mtWarning, Buttons, 0);
        if Explorer.IOStatus in [mrNo, mrCancel] then
        begin
          Result := False;
          Exit;
        end;
      end;

      Result := MoveFolders(NewParent) and MoveFiles(NewParent);
      if Result then
      begin
        Result := RemoveDir(FullQualifiedDirName);
        Explorer.CheckIOError(Result);
        if not DirectoryExists(FullQualifiedDirName) then Delete;
      end;
    end
    else
    begin
      Result := RenameFile(FullQualifiedDirName, DestName);
      Explorer.CheckIOError(Result);
    end;
  end;
end;

function TdxPSFileBasedExplorerFolder.DoRename(var ANewName: string): Boolean;
begin
  Result := inherited DoRename(ANewName);
  if Result and not (esLoading in Explorer.State) then
  begin
    Result := RenameFile(FullQualifiedDirName, FullQualifiedDirPath + ANewName);
    Explorer.CheckIOError(Result);
  end;
end;

function TdxPSFileBasedExplorerFolder.GetDirName: string;
begin
  Result := Name;
end;

function TdxPSFileBasedExplorerFolder.GetDisplayName: string;
begin
  Result := DirName;
end;

function TdxPSFileBasedExplorerFolder.GetIsVolume: Boolean;
begin
  Result := ExtractFileDir(FullQualifiedDirName) = FullQualifiedDirName;
end;

function TdxPSFileBasedExplorerFolder.GetFolder(Index: Integer): TdxPSFileBasedExplorerFolder;
begin
  Result := inherited Folders[Index] as TdxPSFileBasedExplorerFolder;
end;

function TdxPSFileBasedExplorerFolder.GetFullQualifiedDirName: string;
begin
  Result := FullQualifiedDirPath;
  if IsRoot then
    Result := RemoveTrailingBackSlash(Result)
  else
    Result := Result + DirName;
end;

function TdxPSFileBasedExplorerFolder.GetFullQualifiedDirPath: string;
begin
  if IsRoot then
    Result := Explorer.RealRootPath
  else
    Result := TdxPSFileBasedExplorerFolder(Parent).FullQualifiedDirName;
  Result := Result + '\';
end;

function TdxPSFileBasedExplorerFolder.GetItem(Index: Integer): TdxPSFileBasedExplorerItem;
begin
  Result := inherited Items[Index] as TdxPSFileBasedExplorerItem;
end;

{ TdxPSFileBasedExplorerRootFolder }

function TdxPSFileBasedExplorerRootFolder.GetImageIndex: Integer;
begin
  if IsVolume and not (DriveType in [dxPSGlbl.dtUnknown, dxPSGlbl.dtNoRootDir]) then
    Result := dxPSCore.iiDriveTypes[DriveType]
  else
    Result := inherited GetImageIndex;
end;

function TdxPSFileBasedExplorerRootFolder.GetSelectedIndex: Integer;
begin
  if IsVolume and not (DriveType in [dxPSGlbl.dtUnknown, dxPSGlbl.dtNoRootDir]) then
    Result := dxPSCore.iiDriveTypes[DriveType]
  else
    Result := inherited GetSelectedIndex;
end;

function TdxPSFileBasedExplorerRootFolder.GetDriveType: TdxDriveType;
begin
  Result := TdxDriveType(Windows.GetDriveType(PChar(FullQualifiedDirPath)));
end;

function TdxPSFileBasedExplorerRootFolder.GetVolumeLabel: string;
begin
  if IsVolume then
    Result := dxPSUtl.GetVolumeName(FullQualifiedDirPath)
  else
    Result := '';
end;

{ TdxPSFileBasedExplorerItem }

function TdxPSFileBasedExplorerItem.AcquireExtension(const AName: string): string;
begin
  if Explorer <> nil then
    Result := Explorer.AcquireExtension(AName)
  else
    Result := AName;
end;

function TdxPSFileBasedExplorerItem.SuppressExtension(const AName: string): string;
begin
  if Explorer <> nil then
    Result := Explorer.SuppressExtension(AName)
  else
    Result := AName;
end;

function TdxPSFileBasedExplorerItem.CannotRenameMessageText(const AOldName, ANewName: string): string;
begin
  if not ValidateFileName(ANewName) then
    Result := cxGetResourceString(@sdxInvalidReportName)
  else
    Result := inherited CannotRenameMessageText(AOldName, ANewName);
end;

function TdxPSFileBasedExplorerItem.CanMoveTo(AParent: TCustomdxPSExplorerItem): Boolean;
// Name is equal '' in creation phase
begin
  Result := inherited CanMoveTo(AParent) and (AParent <> nil) and
    ((esLoading in Explorer.State) or (Name = '') or
    CanRenameTo(TdxPSFileBasedExplorerFolder(AParent).FullQualifiedDirName + '\' + Name));
end;

function TdxPSFileBasedExplorerItem.CanRenameTo(const AName: string): Boolean;
begin
  Result := inherited CanRenameTo(AName) and ValidateFileName(ExtractFileName(AName));
end;

function TdxPSFileBasedExplorerItem.DataLoadErrorText: string;
begin
  Result := cxGetResourceString(@sdxFileBasedExplorerItemDataLoadError);
end;

function TdxPSFileBasedExplorerItem.Explorer: TdxPSFileBasedExplorer;
begin
  Result := inherited Explorer as TdxPSFileBasedExplorer;
end;

function TdxPSFileBasedExplorerItem.GetUniqueID(out AnUniqueID: TBytes): Integer;
begin
  Result := Length(FullQualifiedFileName);
  SetLength(AnUniqueID, Result);
  Move(Pointer(FullQualifiedFileName)^, Pointer(AnUniqueID)^, Result);
end;

function TdxPSFileBasedExplorerItem.IsNameChanged(const ANewName: string): Boolean;
begin
  Result := inherited IsNameChanged(AcquireExtension(ANewName));
end;

function TdxPSFileBasedExplorerItem.DoDelete: Boolean;
begin
  Result := inherited DoDelete;
  if Result and FileExists(FullQualifiedFileName) then
    Result := ShellRemoveFile(FullQualifiedFileName, False) and not FileExists(FullQualifiedFileName);
end;

function TdxPSFileBasedExplorerItem.GetDisplayName: string;
begin
  Result := SuppressExtension(FileName);
end;

function TdxPSFileBasedExplorerItem.GetFileName: string;
begin
  Result := AcquireExtension(Name);
end;

function TdxPSFileBasedExplorerItem.GetFileSize: Int64; // in bytes
var
  FileName: string;
  FileHandle: THandle;
  LowPart, HighPart: DWORD;
begin
  Result := 0;
  FileName := FullQualifiedFileName;
  if FileExists(FileName) then
  begin
    FileHandle := FileOpen(FileName, fmOpenRead or fmShareDenyWrite);
    if FileHandle > 0 then
    try
      LowPart := Windows.GetFileSize(FileHandle, @HighPart);
      if LowPart <> INVALID_FILE_SIZE then
        Result := (Int64(HighPart) shl 32) or Int64(LowPart);
    finally
      FileClose(FileHandle);
    end;
  end;
end;

function TdxPSFileBasedExplorerItem.GetFormCaption: string;
begin
  Result := FullQualifiedFileName;
end;

function TdxPSFileBasedExplorerItem.GetInfoTip: string;
var
  AFileSize: Int64;
begin
  Result := inherited GetInfoTip;

  AFileSize := FileSize;
  if AFileSize > 0 then
    Result := Result + dxCRLF + dxPSUtl.DropAmpersand(cxGetResourceString(@sdxSize))+ ': ' + dxPSUtl.FormatFileSize(AFileSize);
end;

function TdxPSFileBasedExplorerItem.GetNewName(AReportLink: TBasedxReportLink): string;
begin
  Result := Explorer.AcquireExtension(inherited GetNewName(AReportLink));
end;

function TdxPSFileBasedExplorerItem.DoMove(AParent: TdxPSExplorerFolder): Boolean;
var
  CancelOperation: Boolean;
  DestName: string;
  Item: TdxPSExplorerItem;
begin
  Result := inherited DoMove(AParent);
  if Result and ([esLoading, esItemCreating] * Explorer.State = []) then
  begin
    if not FileExists(FullQualifiedFileName) then
    begin
      Delete;
      Result := False;
      Exit;
    end;

    CancelOperation := False;
    DestName := TdxPSFileBasedExplorerFolder(AParent).FullQualifiedDirName + '\' + Name;
    if FileExists(DestName) then
    begin
      if IsIOOutwardlyControlled then
        CancelOperation := not ShellRemoveFile(DestName, False)
      else
        CancelOperation := not MessageQuestion(OverwriteMessageText(AParent)) or not ShellRemoveFile(DestName, False);
      if not CancelOperation then
      begin
        Item := AParent.ItemByName(Name);
        if Item <> nil then Item.Delete;
      end;
    end;

    Result := not CancelOperation;
    if Result then
    begin
      Result := RenameFile(FullQualifiedFileName, DestName);
      Explorer.CheckIOError(Result);
    end;
  end;
end;

function TdxPSFileBasedExplorerItem.DoRename(var ANewName: string): Boolean;
var
  ExtensionedName: string;
begin
  ExtensionedName := AcquireExtension(ANewName);
  Result := inherited DoRename(ExtensionedName);
  if Result and not (esLoading in Explorer.State) and
    not dxSameText(FullQualifiedFileName, FullQualifiedFilePath + ExtensionedName) then
  begin
    Result := RenameFile(FullQualifiedFileName, FullQualifiedFilePath + ExtensionedName);
    Explorer.CheckIOError(Result);
  end;
end;

procedure TdxPSFileBasedExplorerItem.SetName(const Value: string);
begin
  inherited SetName(AcquireExtension(Value));
end;

function TdxPSFileBasedExplorerItem.GetFullQualifiedFileName: string;
begin
  Result := FullQualifiedFilePath + FileName;
end;

function TdxPSFileBasedExplorerItem.GetFullQualifiedFilePath: string;
begin
  if Parent <> nil then
    if Parent.IsRoot then
      Result := Explorer.RealRootPath
    else
      Result := TdxPSFileBasedExplorerFolder(Parent).FullQualifiedDirName
  else
    Result := '';

  Result := Result + '\';
end;

{ TdxPSFileBasedExplorerContextCommand }

function TdxPSFileBasedExplorerContextCommand.Explorer: TdxPSFileBasedExplorer;
begin
  Result := inherited Explorer as TdxPSFileBasedExplorer;
end;

{ TdxPSFileBasedExplorerSetAsRootContextCommand }

constructor TdxPSFileBasedExplorerSetAsRootContextCommand.Create(AnExplorer: TCustomdxPSExplorer);
begin
  inherited;
  Caption := cxGetResourceString(@sdxMenuExplorerSetAsRoot);
  Hint := cxGetResourceString(@sdxHintExplorerSetAsRoot);
end;

function TdxPSFileBasedExplorerSetAsRootContextCommand.Enabled: Boolean;
begin
  Result := Explorer.CanSetActiveFolderAsRoot;
end;

procedure TdxPSFileBasedExplorerSetAsRootContextCommand.Execute;
begin
  if Enabled then
    Explorer.SetActiveFolderAsRoot;
end;

{ TdxPSFileBasedExplorerChangeRootContextCommand }

constructor TdxPSFileBasedExplorerChangeRootContextCommand.Create(AnExplorer: TCustomdxPSExplorer);
begin
  inherited;
  Caption := cxGetResourceString(@sdxMenuExplorerChangeRootPath);
  Hint := cxGetResourceString(@sdxHintExplorerChangeRootPath);
end;

function TdxPSFileBasedExplorerChangeRootContextCommand.Enabled: Boolean;
begin
  Result := True;
end;

procedure TdxPSFileBasedExplorerChangeRootContextCommand.Execute;
begin
  Explorer.ShowChangeRootPathDlg;
end;

{ TdxPSFileBasedExplorerGoToUpOneLevelContextCommand }

constructor TdxPSFileBasedExplorerGoToUpOneLevelContextCommand.Create(AnExplorer: TCustomdxPSExplorer);
begin
  inherited;
  Caption := cxGetResourceString(@sdxMenuExplorerGoToUpOneLevel);
  Hint := cxGetResourceString(@sdxHintExplorerGoToUpOneLevel);
  ShortCut := Menus.TextToShortCut('Alt+2');
  dxLoadBitmapFromResource(Bitmap, IDB_DXPSGOTOUPONELEVEL);
  Bitmap.Transparent := True;
end;

function TdxPSFileBasedExplorerGoToUpOneLevelContextCommand.Enabled: Boolean;
begin
  Result := Explorer.CanGoToUpOneLevel;
end;

procedure TdxPSFileBasedExplorerGoToUpOneLevelContextCommand.Execute;
begin
  if Enabled then
    Explorer.GoToUpOneLevel;
end;

{ TdxPSFileBasedExplorer }

constructor TdxPSFileBasedExplorer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOptions := [eoShowIOErrors];
end;

procedure TdxPSFileBasedExplorer.BeforeDestruction;
begin
  SaveDefaultRegistry;
  inherited BeforeDestruction;
end;

procedure TdxPSFileBasedExplorer.LoadDefaultRegistry;
var
  AIniFile: TCustomIniFile;
begin
  if dxPSStoringManager.BeginStoring(AIniFile) then
  try
    LoadFromIniFile(AIniFile, sdxRootPathSection + '\' + Name);
  finally
    dxPSStoringManager.EndStoring(AIniFile);
  end;
end;

procedure TdxPSFileBasedExplorer.SaveDefaultRegistry;
var
  AIniFile: TCustomIniFile;
begin
  if dxPSStoringManager.BeginStoring(AIniFile) then
  try
    SaveToIniFile(AIniFile, sdxRootPathSection + '\' + Name);
  finally
    dxPSStoringManager.EndStoring(AIniFile);
  end;
end;

procedure TdxPSFileBasedExplorer.LoadFromIniFile(
  AIniFile: TCustomIniFile; const ASection: string);
begin
  RootPath := AIniFile.ReadString(ASection, sdxRootPath, RootPath);
end;

procedure TdxPSFileBasedExplorer.LoadFromIniFile(const AFileName: string);
var
  AIniFile: TCustomIniFile;
begin
  AIniFile := TMemIniFile.Create(AFileName);
  try
    LoadFromIniFile(AIniFile, sdxRootPathSection + '\' + Name);
  finally
    AIniFile.Free;
  end;
end;

procedure TdxPSFileBasedExplorer.LoadFromRegistry(const APath: string);
var
  AIniFile: TRegistryIniFile;
begin
  AIniFile := TRegistryIniFile.Create(APath);
  try
    LoadFromIniFile(AIniFile, sdxRootPathSection + '\' + Name);
  finally
    AIniFile.Free;
  end;
end;

procedure TdxPSFileBasedExplorer.SaveToIniFile(
  AIniFile: TCustomIniFile; const ASection: string);
begin
  AIniFile.WriteString(ASection, sdxRootPath, RootPath);
end;

procedure TdxPSFileBasedExplorer.SaveToIniFile(const AFileName: string);
var
  AIniFile: TMemIniFile;
begin
  AIniFile := TMemIniFile.Create(AFileName);
  try
    SaveToIniFile(AIniFile, sdxRootPathSection + '\' + Name);
    AIniFile.UpdateFile;
  finally
    AIniFile.Free;
  end;
end;

procedure TdxPSFileBasedExplorer.SaveToRegistry(const APath: string);
var
  AIniFile: TRegistryIniFile;
begin
  AIniFile := TRegistryIniFile.Create(APath);
  try
    SaveToIniFile(AIniFile, sdxRootPathSection + '\' + Name);
  finally
    AIniFile.Free;
  end;
end;

class function TdxPSFileBasedExplorer.FileExtension: string;
begin
  Result := dxPSCore.dxPSReportFileShortExtension;
end;

procedure TdxPSFileBasedExplorer.PopulatePath(APath: string);

  procedure InternalPopulateFolder(AFolder: TdxPSFileBasedExplorerFolder; ADirName: string);
  var
    P: Integer;
    FolderName: string;
  begin
    P := FirstPathDelimiter(ADirName);
    if P <> 0 then
      FolderName := Copy(ADirName, 1, P - 1)
    else
      FolderName := ADirName;

    AFolder := TdxPSFileBasedExplorerFolder(AFolder.FolderByName(FolderName));
    if AFolder <> nil then
    begin
      if P = 0 then
        AFolder.Populate
      else
      begin
        System.Delete(ADirName, 1, P);
        InternalPopulateFolder(AFolder, ADirName);
      end;
    end;
  end;

begin
  APath := dxPSUtl.GetLongFileName(APath);
  APath := RemoveTrailingBackSlash(APath);
  if DirectoryExists(APath) then
    if Pos(RealRootPath + '\', APath) = 1 then
    begin
      System.Delete(APath, 1, Length(RealRootPath) + 1);
      InternalPopulateFolder(Root, APath);
    end;
end;

function TdxPSFileBasedExplorer.CreateNewFolder(AParent: TdxPSExplorerFolder): TdxPSExplorerFolder;
var
  DirectoryName: string;
begin
  if CanCreateFolder then
  begin
    Result := inherited CreateNewFolder(AParent);
    try
      DirectoryName := TdxPSFileBasedExplorerFolder(Result).FullQualifiedDirName;
      if not ForceDirectories(DirectoryName) then
        ProcessIOError(True);
    except
      FreeAndNil(Result);
      if eoShowIOErrors in Options then raise;
    end;
  end
  else
    Result := nil;
end;

function TdxPSFileBasedExplorer.CreateNewItem(
  AParent: TdxPSExplorerFolder; AReportLink: TBasedxReportLink): TdxPSExplorerItem;
var
  FileName: string;
begin
  if CanCreateItem then
  begin
    Result := inherited CreateNewItem(AParent, AReportLink);
    try
      FileName := TdxPSFileBasedExplorerItem(Result).FullQualifiedFileName;
      CreateAndCloseFile(FileName);
      Result.RetrieveReportData(AReportLink);
    except
      if FileExists(FileName) then
        ShellRemoveFile(FileName, False);
      FreeAndNil(Result);
      if eoShowIOErrors in Options then
        raise;
    end;
  end
  else
    Result := nil;
end;

function TdxPSFileBasedExplorer.FindCustomItemByUniqueID(const AnUniqueID: TBytes): TCustomdxPSExplorerItem;
var
  Name: string;
begin
  SetLength(Name, Length(AnUniqueID));
  Move(Pointer(AnUniqueID)^, Pointer(Name)^, Length(Name));
  Result := FindFolderByFullQualifiedDirName(Name);
  if Result = nil then
    Result := FindItemByFullQualifiedFileName(Name);
end;

function TdxPSFileBasedExplorer.FindFolderByFullQualifiedDirName(const AFullQualifiedDirName: string): TdxPSFileBasedExplorerFolder;
begin
  Result := Root.FindFolderByFullQualifiedDirName(AFullQualifiedDirName);
end;

function TdxPSFileBasedExplorer.FindItemByFullQualifiedFileName(const AFullQualifiedName: string): TdxPSFileBasedExplorerItem;
begin
  Result := Root.FindItemByFullQualifiedFileName(AFullQualifiedName);
end;

function TdxPSFileBasedExplorer.LoadedItem: TdxPSFileBasedExplorerItem;
begin
  Result := inherited LoadedItem as TdxPSFileBasedExplorerItem;
end;

procedure TdxPSFileBasedExplorer.LoadItemData(
  const AFullQualifiedFileName: string; AReportLink: TBasedxReportLink);
var
  S: string;
  Item: TdxPSFileBasedExplorerItem;
begin
  S := dxPSUtl.GetLongFileName(AFullQualifiedFileName);
  PopulatePath(ExtractFileDir(S));
  Item := FindItemByFullQualifiedFileName(S);
  if Item <> nil then
    Item.Load(AReportLink);
end;

procedure TdxPSFileBasedExplorer.UnloadItemData(const AFullQualifiedFileName: string);
var
  Item: TdxPSExplorerItem;
begin
  Item := FindItemByFullQualifiedFileName(dxPSUtl.GetLongFileName(AFullQualifiedFileName));
  if Item <> nil then
    Item.Unload;
end;

function TdxPSFileBasedExplorer.CanGoToUpOneLevel: Boolean;
begin
  Result := not Root.IsVolume;
end;

procedure TdxPSFileBasedExplorer.GoToUpOneLevel;
begin
  if CanGotoUpOneLevel then
    RootPath := ExtractFileDir(RealRootPath);
end;

function TdxPSFileBasedExplorer.ShowChangeRootPathDlg: Boolean;
var
  S: string;
begin
  S := RealRootPath;
  Result := dxPSUtl.ShowSystemSelectFolderDlg(S);
  if Result then
    RootPath := S;
end;

function TdxPSFileBasedExplorer.CanSetActiveFolderAsRoot: Boolean;
begin
  Result := (ActiveFolderPath <> '') and //ValidateFileName(ActiveFolderPath) and
    not dxSameText(RootPath, ActiveFolderPath);
end;

procedure TdxPSFileBasedExplorer.SetActiveFolderAsRoot;
begin
  if CanSetActiveFolderAsRoot then
    RootPath := ActiveFolderPath;
end;

procedure TdxPSFileBasedExplorer.Loaded;
begin
  inherited Loaded;
  LoadDefaultRegistry;
end;

{ IdxPSExplorerBuildContextCommands }

procedure TdxPSFileBasedExplorer.BuildCommandSet(ABuilder: IdxPSExplorerContextCommandBuilder);
begin
  inherited;
  ABuilder.AddExplorerContextCommand(AddCommandSeparator);
  ABuilder.AddExplorerContextCommand(AddCommand(TdxPSFileBasedExplorerChangeRootContextCommand));
  ABuilder.AddExplorerContextCommand(AddCommand(TdxPSFileBasedExplorerSetAsRootContextCommand));
  ABuilder.AddExplorerContextCommand(AddCommand(TdxPSFileBasedExplorerGoToUpOneLevelContextCommand));
end;

procedure TdxPSFileBasedExplorer.FinalizeCommand(ACommand: TCustomdxPSExplorerContextCommand);
begin
  inherited;
end;

procedure TdxPSFileBasedExplorer.InitializeCommand(ACommand: TCustomdxPSExplorerContextCommand);
begin
  inherited;
end;

class function TdxPSFileBasedExplorer.AcceptItemNameChar(AnItem: TCustomdxPSExplorerItem;
  Ch: Char): Boolean;
begin
  Result := Pos(Ch, InvalidFileNameChars) = 0;
end;

function TdxPSFileBasedExplorer.AcquireExtension(const AName: string): string;
begin
  Result := ChangeFileExt(SuppressExtension(AName), '.' + FileExtension);
end;

function TdxPSFileBasedExplorer.SuppressExtension(const AName: string): string;
begin
  Result := ChangeFileExt(AName, '');
end;

procedure TdxPSFileBasedExplorer.CreateAndCloseFile(const AName: string);
var
  DirectoryName: string;
begin
  DirectoryName := ExtractFileDir(AName);
  if not DirectoryExists(DirectoryName) then
  begin
    if not ForceDirectories(DirectoryName) then
      ProcessIOError(True);
  end;

  with TFileStream.Create(AName, fmCreate) do
    Free;
end;

function TdxPSFileBasedExplorer.CreateDataStream(const AFileName: string;
  AMode: TdxPSStreamMode): TStream;
const
  FileModesMap: array[TdxPSStreamMode] of Word =
   (fmOpenRead or fmShareDenyWrite, fmCreate or fmOpenWrite or fmShareDenyWrite, fmOpenReadWrite);
begin
  if FileExists(AFileName) then
  try
    Result := TFileStream.Create(AFileName, FileModesMap[AMode]);
  except
    Result := nil;
  end
  else
    Result := nil;
end;

function TdxPSFileBasedExplorer.CreateItemDataStream(AnItem: TdxPSExplorerItem;
  AMode: TdxPSStreamMode): TStream;
begin
  Result := CreateDataStream((AnItem as TdxPSFileBasedExplorerItem).FullQualifiedFileName, AMode);
end;

procedure TdxPSFileBasedExplorer.DoLoadData(AFolder: TdxPSExplorerFolder);

  function AddChildItem(AParent: TdxPSExplorerFolder;
    const ASearchRec: TSearchRec; const AFileName: string): TdxPSExplorerItem;
  var
    AStream: TStream;
    AReportDocument: TdxPSReportDocument;
  begin
    Result := GetItemClass.Create(Self, AParent);
    try
      Result.Name := ASearchRec.Name;
      AStream := CreateDataStream(AFileName, smRead);
      try
        AReportDocument := TBasedxReportLink.ExtractReportDocument(AStream, True);
        try
          Result.ReportDocument.Assign(AReportDocument);
          Result.ReportDocument.Caption := SuppressExtension(Result.Name);
        finally
          AReportDocument.Free;
        end;
      finally
        AStream.Free;
      end;
    except
      Result.Free;
      raise;
    end;
  end;

  function DoFilterLinkClass(const AFileName: string): Boolean;
  var
    AStream: TStream;
  begin
    Result := True;
    if FilterLinkClass <> nil then
    begin
      AStream := CreateDataStream(AFileName, smRead);
      try
        Result := SameText(FilterLinkClassName,
          TBasedxReportLink.ExtractComponentClassName(AStream, False));
      finally
        AStream.Free;
      end;
    end;
  end;

  function IsValidReportFile(const AName: string): Boolean;
  begin
    Result := HasExtension(AName, '.' + FileExtension) and DoFilterLinkClass(AName);
  end;

  function HasChildren(APath: string): Boolean;
  var
    SR: TSearchRec;
  begin
    Result := False;
    APath := dxValidatePath(APath);
    if FindFirst(APath + '*.*', faAnyFile, SR) = 0 then
    try
      repeat
        if not IsSystemObject(SR) then
          Result := (SR.Attr and faDirectory <> 0) or IsValidReportFile(APath + SR.Name);
      until (FindNext(SR) <> 0) or Result;
    finally
      FindClose(SR);
    end;
  end;

  procedure LoadDirectory(AParent: TdxPSExplorerFolder; APath: string);
  var
    AFileName: string;
    AFolder: TdxPSExplorerFolder;
    SR: TSearchRec;
  begin
    APath := dxValidatePath(APath);
    if FindFirst(APath + '*.*', faAnyFile, SR) = 0 then
    try
      repeat
        if not IsSystemObject(SR) then
        try
          AFileName := APath + SR.Name;
          if SR.Attr and faDirectory = 0 then
          begin
            if IsValidReportFile(AFileName) then
              AddChildItem(AParent, SR, AFileName);
          end
          else
          begin
            AFolder := GetFolderClass.Create(Self, AParent);
            AFolder.Name := SR.Name;
            if eoLoadAll in Options then
              LoadDirectory(AFolder, AFileName)
            else
              TdxPSExplorerFolderHelper.SetHasChildren(AFolder, HasChildren(AFileName));
          end;
        except
          DoLoadError(SR.Name);
        end;
      until FindNext(SR) <> 0;
    finally
      FindClose(SR);
    end;
  end;

begin
  LoadDirectory(AFolder, TdxPSFileBasedExplorerFolder(AFolder).FullQualifiedDirName);
end;

procedure TdxPSFileBasedExplorer.DoLoadError(const AName: string);
begin
  if Assigned(FOnLoadError) then FOnLoadError(Self, AName);
end;

procedure TdxPSFileBasedExplorer.RestoreLoadedItem;
begin
  if FLastLoadedFileName <> '' then
  begin
    InternalSetLoadedItem(FindItemByFullQualifiedFileName(FLastLoadedFileName));
    FLastLoadedFileName := '';
  end;
end;

procedure TdxPSFileBasedExplorer.SaveLoadedItem;
begin
  if LoadedItem <> nil then
    FLastLoadedFileName := LoadedItem.FullQualifiedFileName
  else
    FLastLoadedFileName := '';
end;

class function TdxPSFileBasedExplorer.GetFolderClass: TdxPSExplorerFolderClass;
begin
  Result := TdxPSFileBasedExplorerFolder;
end;

class function TdxPSFileBasedExplorer.GetRootFolderClass: TdxPSExplorerFolderClass;
begin
  Result := TdxPSFileBasedExplorerRootFolder;
end;

class function TdxPSFileBasedExplorer.GetItemClass: TdxPSExplorerItemClass;
begin
  Result := TdxPSFileBasedExplorerItem;
end;

function TdxPSFileBasedExplorer.GetRootDisplayName: string;
begin
  if not Root.IsVolume then
  begin
    if dxSameText(RealRootPath, RemoveTrailingBackSlash(RootPath)) then
      Result := RootPath
    else
      Result := inherited GetRootDisplayName + ' (' + RealRootPath + ')';
    Result := RemoveTrailingBackSlash(Result);
  end
  else
    Result := Root.VolumeLabel + ' (' + RemoveTrailingBackSlash(RootPath) + ')';
end;

procedure TdxPSFileBasedExplorer.MoveTo(AnItem: TCustomdxPSExplorerItem;
  AParent: TdxPSExplorerFolder);
begin
  BeginIO;
  try
    inherited;
  finally
    EndIO;
  end;
end;

procedure TdxPSFileBasedExplorer.BeginIO;
begin
  if FIOLockCounter = 0 then IOStatus := mrNone;
  Inc(FIOLockCounter);
end;

procedure TdxPSFileBasedExplorer.EndIO;
begin
  Dec(FIOLockCounter);
end;

procedure TdxPSFileBasedExplorer.CheckIOError(AnIOResult: Boolean);
begin
  if not AnIOResult then ProcessIOError;
end;

procedure TdxPSFileBasedExplorer.ProcessIOError(AnUnconditionalRaiseException: Boolean = False);
begin
  if AnUnconditionalRaiseException or (eoShowIOErrors in Options) then
    IOError;
end;

function TdxPSFileBasedExplorer.GetActiveFolder: TdxPSFileBasedExplorerFolder;
begin
  Result := inherited ActiveFolder as TdxPSFileBasedExplorerFolder;
end;

function TdxPSFileBasedExplorer.GetActiveFolderPath: string;
begin
  if ActiveFolder <> nil then
    Result := ActiveFolder.FullQualifiedDirName
  else
    Result := '';
end;

function TdxPSFileBasedExplorer.GetRealRootPath: string;
begin
  if RootPath <> '' then
    Result := RootPath
  else
    Result := GetCurrentDir;
  Result := RemoveTrailingBackSlash(Result);
end;

function TdxPSFileBasedExplorer.GetRoot: TdxPSFileBasedExplorerRootFolder;
begin
  Result := inherited Root as TdxPSFileBasedExplorerRootFolder;
end;

procedure TdxPSFileBasedExplorer.SetActiveFolder(Value: TdxPSFileBasedExplorerFolder);
begin
  inherited ActiveFolder := Value;
end;

procedure TdxPSFileBasedExplorer.SetOptions(Value: TdxPSFileBasedExplorerOptions);
var
  ChangedBits: TdxPSFileBasedExplorerOptions;
begin
  if FOptions <> Value then
  begin
    ChangedBits := FOptions + Value - FOptions * Value;
    FOptions := Value;
    if [eoLoadAll] * ChangedBits <> [] then Refresh;
  end;
end;

procedure TdxPSFileBasedExplorer.SetRootPath(const Value: string);
var
  S: string;
begin
  S := dxPSUtl.GetLongFileName(Value);
  if not dxSameText(FRootPath, S) then
    if not dxSameText(RealRootPath, Value) then
    begin
      BeforeRefresh;
      try
        FRootPath := S;
        Refresh;
      finally
        AfterRefresh;
      end;
    end
    else
      FRootPath := S;
end;

end.
