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

unit dxPSDBBasedXplorer;

interface

{$I cxVer.inc}

uses
  Types, Variants, SysUtils, Classes, DB, cxClasses, dxPSGlbl, dxPSCore, dxCore;

const
  fnmBase = 0;
  fnmID = fnmBase + 0;
  fnmName = fnmBase + 1;
  fnmParentID = fnmBase + 2;
  fnmData = fnmBase + 3;

type
  TdxDBBasedExplorerItemType = (eitFolder, eitItem);

  PdxDBBasedExplorerItemUniqueID = ^TdxDBBasedExplorerItemUniqueID;
  TdxDBBasedExplorerItemUniqueID = record
    ID: Integer;
    ItemType: TdxDBBasedExplorerItemType;
  end;

  TdxPSDBBasedExplorerItem = class;
  TdxPSDBBasedExplorer = class;

  TdxPSDBBasedExplorerFolder = class(TdxPSExplorerFolder)
  private
    FID: Integer;
    FParentID: Integer;
    function GetFolder(Index: Integer): TdxPSDBBasedExplorerFolder;
    function GetItem(Index: Integer): TdxPSDBBasedExplorerItem;
    procedure SetParentID(Value: Integer);
  protected
    procedure InternalMove(AParent: TdxPSExplorerFolder); override;
  public
    constructor Create(AnExplorer: TCustomdxPSExplorer; AParent: TdxPSExplorerFolder); override;

    function CanMoveTo(AParentID: Integer): Boolean; overload; virtual;
    function Explorer: TdxPSDBBasedExplorer; reintroduce; overload;
    function GetUniqueID(out AnUniqueID: TBytes): Integer; override;

    property Folders[Index: Integer]: TdxPSDBBasedExplorerFolder read GetFolder; default;
    property ID: Integer read FID;
    property Items[Index: Integer]: TdxPSDBBasedExplorerItem read GetItem;
    property ParentID: Integer read FParentID write SetParentID;
  end;

  TdxPSDBBasedExplorerItem = class(TdxPSExplorerItem)
  private
    FID: Integer;
    FParentID: Integer;
    procedure SetParentID(Value: Integer);
  protected
    procedure InternalMove(AParent: TdxPSExplorerFolder); override;
    procedure SaveDocument; override;
    procedure SaveItem; virtual;
  public
    constructor Create(AnExplorer: TCustomdxPSExplorer; AParent: TdxPSExplorerFolder); override;

    function CanMoveTo(AParentID: Integer): Boolean; overload; virtual;
    function DataLoadErrorText: string; override;
    function Explorer: TdxPSDBBasedExplorer; reintroduce; overload;
    function GetUniqueID(out AnUniqueID: TBytes): Integer; override;

    property ID: Integer read FID;
    property ParentID: Integer read FParentID write SetParentID;
  end;

  TCustomdxPSDBBasedExplorerFieldNamesMap = class(TPersistent)
  private
    FExplorer: TdxPSDBBasedExplorer;
    FItems: TStringList;
    function GetCount: Integer;
  protected
    function GetItem(Index: Integer): string;
    procedure SetItem(Index: Integer; const Value: string);

    procedure Changed; dynamic;
    function GetMapCount: Integer; virtual;
    procedure InitializeItems; virtual;

    property Count: Integer read GetCount;
    property Items[Index: Integer]: string read GetItem write SetItem; default;
    property MapCount: Integer read GetMapCount;
  public
    constructor Create(AnExplorer: TdxPSDBBasedExplorer); virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    property Explorer: TdxPSDBBasedExplorer read FExplorer;
  end;

  TdxPSDBBasedExplorerFieldNamesMapClass = class of TCustomdxPSDBBasedExplorerFieldNamesMap;

  TdxPSDBBasedExplorerFieldNamesMap = class(TCustomdxPSDBBasedExplorerFieldNamesMap)
  protected
    function GetMapCount: Integer; override;
  published
    property ID: string Index fnmID read GetItem write SetItem;
    property Name: string Index fnmName read GetItem write SetItem;
    property ParentID: string Index fnmParentID read GetItem write SetItem;
  end;

  TdxPSDBBasedExplorerFoldersFieldNamesMapClass = class of TdxPSDBBasedExplorerFoldersFieldNamesMap;

  TdxPSDBBasedExplorerFoldersFieldNamesMap = class(TdxPSDBBasedExplorerFieldNamesMap);

  TdxPSDBBasedExplorerItemsFieldNamesMapClass = class of TdxPSDBBasedExplorerItemsFieldNamesMap;

  TdxPSDBBasedExplorerItemsFieldNamesMap = class(TdxPSDBBasedExplorerFieldNamesMap)
  protected
    function GetMapCount: Integer; override;
  published
    property Data: string Index fnmData read GetItem write SetItem;
  end;

  TdxPSDBBasedExplorerLoadErrorEvent = procedure(Sender: TdxPSDBBasedExplorer; ADataSet: TDataSet) of object;

  TdxPSDBBasedExplorer = class(TCustomdxPSExplorer)
  private
    FFolderList: TList;
    FFolders: TDataSet;
    FFoldersFieldNamesMap: TdxPSDBBasedExplorerFoldersFieldNamesMap;
    FItemList: TList;
    FItems: TDataSet;
    FItemsFieldNamesMap: TdxPSDBBasedExplorerItemsFieldNamesMap;
    FOnLoadError: TdxPSDBBasedExplorerLoadErrorEvent;
    function GetRoot: TdxPSDBBasedExplorerFolder;
    procedure SetFolders(Value: TDataSet);
    procedure SetFoldersFieldNamesMap(Value: TdxPSDBBasedExplorerFoldersFieldNamesMap);
    procedure SetItems(Value: TDataSet);
    procedure SetItemsFieldNamesMap(Value: TdxPSDBBasedExplorerItemsFieldNamesMap);
  protected
    function CreateItemDataStream(AnItem: TdxPSExplorerItem; AMode: TdxPSStreamMode): TStream; override;
    class function GetFolderClass: TdxPSExplorerFolderClass; override;
    class function GetItemClass: TdxPSExplorerItemClass; override;
    class function GetRootFolderClass: TdxPSExplorerFolderClass; override;
    procedure DoLoadData(AFolder: TdxPSExplorerFolder); override;
    function CanDelete(AnItem: TCustomdxPSExplorerItem): Boolean; override;
    function CanMoveTo(AnItem, AParent: TCustomdxPSExplorerItem): Boolean; override;
    function CanRenameTo(AnItem: TCustomdxPSExplorerItem; const AName: string): Boolean; override;
    procedure Delete(AnItem: TCustomdxPSExplorerItem); override;
    procedure MoveTo(AnItem: TCustomdxPSExplorerItem; AParent: TdxPSExplorerFolder); override;
    procedure RenameTo(AnItem: TCustomdxPSExplorerItem; AName: string); override;

    procedure DoRefresh; override;

    class function GetFoldersFieldNamesMapClass: TdxPSDBBasedExplorerFoldersFieldNamesMapClass; virtual;
    class function GetItemsFieldNamesMapClass: TdxPSDBBasedExplorerItemsFieldNamesMapClass; virtual;

    function AreIDsEqual(AnID1, AnID2: Integer): Boolean;
    procedure Changed; dynamic;
    function CheckDataSets: Boolean; virtual;
    function CheckItemDataSet(AnItem: TCustomdxPSExplorerItem): Boolean; overload; virtual;
    function CheckItemDataSet(AnItemClass: TCustomdxPSExplorerItemClass): Boolean; overload; virtual;
    function CreateDataStream(AMode: TBlobStreamMode): TStream; virtual;
    function GetItemDataSet(AnItem: TCustomdxPSExplorerItem): TDataSet; overload; virtual;
    function GetItemDataSet(AnItemClass: TCustomdxPSExplorerItemClass): TDataSet; overload; virtual;
    function GetItemFieldNamesMap(AnItem: TCustomdxPSExplorerItem): TdxPSDBBasedExplorerFieldNamesMap; overload; virtual;
    function GetItemFieldNamesMap(AnItemClass: TCustomdxPSExplorerItemClass): TdxPSDBBasedExplorerFieldNamesMap; overload; virtual;
    function GetUniqueFolderID: Integer; virtual;
    function GetUniqueItemID: Integer; virtual;
    function IsFolderIDAutoIncField: Boolean;
    function IsItemIDAutoIncField: Boolean;

    procedure LoadFolderFromCurrentRecord(AFolder: TdxPSDBBasedExplorerFolder); virtual;
    procedure LoadItemFromCurrentRecord(AnItem: TdxPSDBBasedExplorerItem); virtual;
    procedure SaveFolderToCurrentRecord(AFolder: TdxPSDBBasedExplorerFolder); virtual;
    procedure SaveItemToCurrentRecord(AnItem: TdxPSDBBasedExplorerItem; AReportLink: TBasedxReportLink); virtual;

    procedure DoLoadError(ADataSet: TDataSet); dynamic;

    function GetFieldValue(ADataSet: TDataSet; const AName: string): Variant;
    function IsFieldNameValid(ADataSet: TDataSet; const AName: string): Boolean;
    procedure SetFieldValue(ADataSet: TDataSet; const AName: string; const Value: Variant);

    function LocateItem(AnItem: TCustomdxPSExplorerItem): Boolean;
    procedure PrepareIterate(ADataSet: TDataSet; out ABookmark: TBookmark);
    procedure UnprepareIterate(ADataSet: TDataSet; var ABookmark: TBookmark);

    property FolderList: TList read FFolderList;
    property ItemList: TList read FItemList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function AreFieldNameMapsValid: Boolean; virtual;

    function CanCreateFolder: Boolean; override;
    function CanCreateItem: Boolean; override;
    function CreateNewFolder(AParent: TdxPSExplorerFolder): TdxPSExplorerFolder; override;
    function CreateNewItem(AParent: TdxPSExplorerFolder; AReportLink: TBasedxReportLink): TdxPSExplorerItem; override;

    function FindCustomItemByUniqueID(const AnUniqueID: TBytes): TCustomdxPSExplorerItem; override;
    function FindFolderByID(AnID: Integer): TdxPSDBBasedExplorerFolder; virtual;
    function FindItemByID(AnID: Integer): TdxPSDBBasedExplorerItem; virtual;

    function LoadedItem: TdxPSDBBasedExplorerItem; reintroduce; overload;

    property Root: TdxPSDBBasedExplorerFolder read GetRoot;
  published
    property Folders: TDataSet read FFolders write SetFolders;
    property FoldersFieldNamesMap: TdxPSDBBasedExplorerFoldersFieldNamesMap read FFoldersFieldNamesMap write SetFoldersFieldNamesMap;
    property Items: TDataSet read FItems write SetItems;
    property ItemsFieldNamesMap: TdxPSDBBasedExplorerItemsFieldNamesMap read FItemsFieldNamesMap write SetItemsFieldNamesMap;
    property OnLoadError: TdxPSDBBasedExplorerLoadErrorEvent read FOnLoadError write FOnLoadError;
  end;

implementation

uses
  dxPSRes;

const
  RootIDValue: Integer = 0;

function VarToInt(const V: Variant): Integer;
begin
  try
    if Variants.VarIsNull(V) then
      Result := 0
    else
      Result := V;
  except
    Result := 0;
  end;
end;

{ TdxPSDBBasedExplorerFolder }

constructor TdxPSDBBasedExplorerFolder.Create(AnExplorer: TCustomdxPSExplorer;
  AParent: TdxPSExplorerFolder);
begin
  inherited;
  FID := RootIDValue;
  if AParent <> nil then
    FParentID := TdxPSDBBasedExplorerFolder(AParent).ID;
end;

function TdxPSDBBasedExplorerFolder.CanMoveTo(AParentID: Integer): Boolean;
var
  Parent: TdxPSDBBasedExplorerFolder;
begin
  Parent := Explorer.FindFolderByID(AParentID);
  Result := (Parent <> nil) and CanMoveTo(Parent);
end;

function TdxPSDBBasedExplorerFolder.Explorer: TdxPSDBBasedExplorer;
begin
  Result := inherited Explorer as TdxPSDBBasedExplorer;
end;

function TdxPSDBBasedExplorerFolder.GetUniqueID(out AnUniqueID: TBytes): Integer;
var
  UniqueID: TdxDBBasedExplorerItemUniqueID;
begin
  UniqueID.ID := ID;
  UniqueID.ItemType := eitFolder;
  SetLength(AnUniqueID, SizeOf(UniqueID));
  Move(UniqueID, Pointer(AnUniqueID)^, SizeOf(UniqueID));
  Result := Length(AnUniqueID);
end;

procedure TdxPSDBBasedExplorerFolder.InternalMove(AParent: TdxPSExplorerFolder);
begin
  FParentID := RootIDValue;
  if AParent <> nil then
    FParentID := TdxPSDBBasedExplorerFolder(AParent).ID;
  inherited;
end;

function TdxPSDBBasedExplorerFolder.GetFolder(Index: Integer): TdxPSDBBasedExplorerFolder;
begin
  Result := inherited Folders[Index] as TdxPSDBBasedExplorerFolder;
end;

function TdxPSDBBasedExplorerFolder.GetItem(Index: Integer): TdxPSDBBasedExplorerItem;
begin
  Result := inherited Items[Index] as TdxPSDBBasedExplorerItem;
end;

procedure TdxPSDBBasedExplorerFolder.SetParentID(Value: Integer);
begin
  if (ParentID <> Value) and CanMoveTo(Value) then
    Parent := Explorer.FindFolderByID(Value);
end;

{ TdxPSDBBasedExplorerItem }

constructor TdxPSDBBasedExplorerItem.Create(AnExplorer: TCustomdxPSExplorer;
  AParent: TdxPSExplorerFolder);
begin
  inherited;
  FID := RootIDValue;
  FParentID := RootIDValue;
  if AParent <> nil then
    FParentID := TdxPSDBBasedExplorerFolder(AParent).ID;
end;

function TdxPSDBBasedExplorerItem.CanMoveTo(AParentID: Integer): Boolean;
var
  Parent: TdxPSDBBasedExplorerFolder;
begin
  Parent := Explorer.FindFolderByID(AParentID);
  Result := (Parent <> nil) and CanMoveTo(Parent);
end;

function TdxPSDBBasedExplorerItem.DataLoadErrorText: string;
begin
  Result := cxGetResourceString(@sdxDBBasedExplorerItemDataLoadError);
end;

function TdxPSDBBasedExplorerItem.Explorer: TdxPSDBBasedExplorer;
begin
  Result := inherited Explorer as TdxPSDBBasedExplorer;
end;

function TdxPSDBBasedExplorerItem.GetUniqueID(out AnUniqueID: TBytes): Integer;
var
  UniqueID: TdxDBBasedExplorerItemUniqueID;
begin
  UniqueID.ID := ID;
  UniqueID.ItemType := eitItem;
  SetLength(AnUniqueID, SizeOf(UniqueID));
  Move(UniqueID, Pointer(AnUniqueID)^, SizeOf(UniqueID));
  Result := Length(AnUniqueID);
end;

procedure TdxPSDBBasedExplorerItem.InternalMove(AParent: TdxPSExplorerFolder);
begin
  FParentID := RootIDValue;
  if AParent <> nil then
    FParentID := TdxPSDBBasedExplorerFolder(AParent).ID;
  inherited;
end;

procedure TdxPSDBBasedExplorerItem.SaveDocument;
begin
  if Explorer <> nil then SaveItem;
  inherited;
end;

procedure TdxPSDBBasedExplorerItem.SaveItem;
begin
  with Explorer do
    if CheckItemDataSet(Self) and LocateItem(Self) then
    begin
      SetFieldValue(Items, FoldersFieldNamesMap.Name, Name);
    end;
end;

procedure TdxPSDBBasedExplorerItem.SetParentID(Value: Integer);
begin
  if (ParentID <> Value) and CanMoveTo(Value) then
    Parent := Explorer.FindFolderByID(Value);
end;

{ TCustomdxPSDBBasedExplorerFieldNamesMap }

constructor TCustomdxPSDBBasedExplorerFieldNamesMap.Create(AnExplorer: TdxPSDBBasedExplorer);
begin
  inherited Create;
  FExplorer := AnExplorer;
  FItems := TStringList.Create;
  InitializeItems;
end;

destructor TCustomdxPSDBBasedExplorerFieldNamesMap.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

procedure TCustomdxPSDBBasedExplorerFieldNamesMap.Assign(Source: TPersistent);
begin
  if Source is TCustomdxPSDBBasedExplorerFieldNamesMap then
    FItems.Assign(TCustomdxPSDBBasedExplorerFieldNamesMap(Source).FItems)
  else
    inherited;
end;

function TCustomdxPSDBBasedExplorerFieldNamesMap.GetItem(Index: Integer): string;
begin
  Result := FItems[Index];
end;

procedure TCustomdxPSDBBasedExplorerFieldNamesMap.SetItem(Index: Integer; const Value: string);
begin
  if FItems[Index] <> Value then
  begin
    FItems[Index] := Value;
    Changed;
  end;
end;

procedure TCustomdxPSDBBasedExplorerFieldNamesMap.Changed;
begin
  if Explorer <> nil then Explorer.Changed;
end;

function TCustomdxPSDBBasedExplorerFieldNamesMap.GetMapCount: Integer;
begin
  Result := 0;
end;

procedure TCustomdxPSDBBasedExplorerFieldNamesMap.InitializeItems;
var
  I: Integer;
begin
  for I := 0 to MapCount - 1 do
    FItems.Add('');
end;

function TCustomdxPSDBBasedExplorerFieldNamesMap.GetCount: Integer;
begin
  Result := FItems.Count;
end;

{ TdxPSDBBasedExplorerFieldNamesMap }

function TdxPSDBBasedExplorerFieldNamesMap.GetMapCount: Integer;
begin
  Result := inherited GetMapCount + 3; {6 ->  if include Creator, DateTimeCreated}
end;

{ TdxPSDBBasedExplorerItemsFieldNamesMap }

function TdxPSDBBasedExplorerItemsFieldNamesMap.GetMapCount: Integer;
begin
  Result := inherited GetMapCount + 1;
end;

{ TdxPSDBBasedExplorer }

constructor TdxPSDBBasedExplorer.Create(AOwner: TComponent);
begin
  inherited;
  FFoldersFieldNamesMap := GetFoldersFieldNamesMapClass.Create(Self);
  FItemsFieldNamesMap := GetItemsFieldNamesMapClass.Create(Self);
end;

destructor TdxPSDBBasedExplorer.Destroy;
begin
  FreeAndNil(FItemsFieldNamesMap);
  FreeAndNil(FFoldersFieldNamesMap);
  FreeAndNil(FItemList);
  FreeAndNil(FFolderList);
  inherited;
end;

function TdxPSDBBasedExplorer.AreFieldNameMapsValid: Boolean;
begin
  Result := IsFieldNameValid(Items, ItemsFieldNamesMap.ID) and
            IsFieldNameValid(Items, ItemsFieldNamesMap.Name) and
            IsFieldNameValid(Items, ItemsFieldNamesMap.ParentID) and
            IsFieldNameValid(Items, ItemsFieldNamesMap.Data) and
            IsFieldNameValid(Folders, FoldersFieldNamesMap.ID) and
            IsFieldNameValid(Folders, FoldersFieldNamesMap.Name) and
            IsFieldNameValid(Folders, FoldersFieldNamesMap.ParentID);
end;

function TdxPSDBBasedExplorer.CanCreateFolder: Boolean;
begin
  Result := CheckDataSets;
end;

function TdxPSDBBasedExplorer.CanCreateItem: Boolean;
begin
  Result := CheckDataSets;
end;

function TdxPSDBBasedExplorer.CreateNewFolder(AParent: TdxPSExplorerFolder): TdxPSExplorerFolder;
begin
  if CanCreateFolder then
  begin
    BeginLoading;
    try
      Folders.Append;
      Result := inherited CreateNewFolder(AParent);
      if not IsFolderIDAutoIncField then
        TdxPSDBBasedExplorerFolder(Result).FID := GetUniqueFolderID;
      SaveFolderToCurrentRecord(Result as TdxPSDBBasedExplorerFolder);
      Folders.Post;
      if IsFolderIDAutoIncField then
        TdxPSDBBasedExplorerFolder(Result).FID := GetFieldValue(Folders, FoldersFieldNamesMap.ID);


      if FolderList <> nil then FolderList.Add(Result);
    finally
      EndLoading;
    end;
  end
  else
    Result := nil;
end;

function TdxPSDBBasedExplorer.CreateNewItem(AParent: TdxPSExplorerFolder;
  AReportLink: TBasedxReportLink): TdxPSExplorerItem;
begin
  if CanCreateItem then
  begin
    BeginLoading;
    try
      Items.Append;
      Result := inherited CreateNewItem(AParent, AReportLink);
      if not IsItemIDAutoIncField then
        TdxPSDBBasedExplorerItem(Result).FID := GetUniqueItemID;
      SaveItemToCurrentRecord(Result as TdxPSDBBasedExplorerItem, AReportLink);
      Items.Post;
      if IsItemIDAutoIncField then
        TdxPSDBBasedExplorerItem(Result).FID := GetFieldValue(Items, ItemsFieldNamesMap.ID);

      if ItemList <> nil then ItemList.Add(Result);
    finally
      EndLoading;
    end;
  end
  else
    Result := nil;
end;

function TdxPSDBBasedExplorer.FindCustomItemByUniqueID(const AnUniqueID: TBytes): TCustomdxPSExplorerItem;
var
  UniqueID: TdxDBBasedExplorerItemUniqueID;
begin
  Move(Pointer(AnUniqueID)^, UniqueID, SizeOf(UniqueID));
  with UniqueID do
    if ItemType = eitFolder then
      Result := FindFolderByID(ID)
    else
      Result := FindItemByID(ID);
end;

function TdxPSDBBasedExplorer.FindFolderByID(AnID: Integer): TdxPSDBBasedExplorerFolder;
var
  I: Integer;
begin
  RootNeeded;
  if Root.ID <> AnID then
  begin
    if FolderList <> nil then
      for I := 0 to FolderList.Count - 1 do
      begin
        Result := TdxPSDBBasedExplorerFolder(FolderList[I]);
        if AreIDsEqual(Result.ID, AnID) then Exit;
      end;
    Result := nil;
  end
  else
    Result := Root;
end;

function TdxPSDBBasedExplorer.FindItemByID(AnID: Integer): TdxPSDBBasedExplorerItem;
var
  I: Integer;
begin
  RootNeeded;
  if ItemList <> nil then
    for I := 0 to ItemList.Count - 1 do
    begin
      Result := TdxPSDBBasedExplorerItem(ItemList[I]);
      if AreIDsEqual(Result.ID, AnID) then Exit;
    end;
  Result := nil;
end;

function TdxPSDBBasedExplorer.LoadedItem: TdxPSDBBasedExplorerItem;
begin
  Result := inherited LoadedItem as TdxPSDBBasedExplorerItem;
end;

function TdxPSDBBasedExplorer.CreateItemDataStream(AnItem: TdxPSExplorerItem;
  AMode: TdxPSStreamMode): TStream;
const
  BlobStreamModesMap: array[TdxPSStreamMode] of TBlobStreamMode = (bmRead, bmWrite, bmReadWrite);
begin
  if IsFieldNameValid(Items, ItemsFieldNamesMap.Data) and LocateItem(AnItem) then
    Result := CreateDataStream(BlobStreamModesMap[AMode])
  else
    Result := nil;
end;

class function TdxPSDBBasedExplorer.GetFolderClass: TdxPSExplorerFolderClass;
begin
  Result := TdxPSDBBasedExplorerFolder;
end;

class function TdxPSDBBasedExplorer.GetItemClass: TdxPSExplorerItemClass;
begin
  Result := TdxPSDBBasedExplorerItem;
end;

class function TdxPSDBBasedExplorer.GetRootFolderClass: TdxPSExplorerFolderClass;
begin
  Result := TdxPSDBBasedExplorerFolder;
end;

procedure TdxPSDBBasedExplorer.DoLoadData(AFolder: TdxPSExplorerFolder);

  procedure LoadFolders;
  var
    Bookmark: TBookmark;
    Folder: TdxPSExplorerFolder;
  begin
    PrepareIterate(Folders, Bookmark);
    try
      Folders.First;
      while not Folders.EOF do
      begin
        try
          Folder := GetFolderClass.Create(Self, nil) as TdxPSDBBasedExplorerFolder;
          LoadFolderFromCurrentRecord(Folder as TdxPSDBBasedExplorerFolder);
          FolderList.Add(Folder);
        except
          DoLoadError(Folders);
        end;
        Folders.Next;
      end;
    finally
      UnprepareIterate(Folders, Bookmark);
    end;
  end;

  function DoFilterLinkClass: Boolean;
  var
    Stream: TStream;
  begin
    Result := True;
    if FilterLinkClass <> nil then
    begin
      Stream := CreateDataStream(bmRead);
      try
        Result := FilterLinkClass = TBasedxReportLink.ExtractComponentClass(Stream, False);
      finally
        Stream.Free;
      end;
    end;
  end;

  procedure LoadItems;

    procedure LoadItemReportDocument(AnItem: TdxPSDBBasedExplorerItem);
    var
      Stream: TStream;
      ReportDocument: TdxPSReportDocument;
    begin
      Stream := CreateDataStream(bmRead);
      try
        ReportDocument := TBasedxReportLink.ExtractReportDocument(Stream, True);
        try
          AnItem.ReportDocument.Assign(ReportDocument);
        finally
          ReportDocument.Free;
        end;
      finally
        Stream.Free;
      end;
    end;

  var
    Bookmark: TBookmark;
    Item: TdxPSDBBasedExplorerItem;
  begin
    PrepareIterate(Items, Bookmark);
    try
      Items.First;
      while not Items.EOF do
      begin
        try
          if DoFilterLinkClass then
          begin
            Item := GetItemClass.Create(Self, nil) as TdxPSDBBasedExplorerItem;
            LoadItemFromCurrentRecord(Item);
            LoadItemReportDocument(Item);
            ItemList.Add(Item)
          end;
        except
          DoLoadError(Items);
        end;
        Items.Next;
      end;
    finally
      UnprepareIterate(Items, Bookmark);
    end;
  end;

  procedure BuildTree;

    procedure BuildFolders;
    var
      I: Integer;
      Folder, Parent: TdxPSDBBasedExplorerFolder;
    begin
      for I := 0 to FolderList.Count - 1 do
      begin
        Folder := TdxPSDBBasedExplorerFolder(FolderList[I]);
        if not AreIDsEqual(Folder.ParentID, TdxPSDBBasedExplorerFolder(Root).ID) then
        begin
          Parent := FindFolderByID(Folder.ParentID);
          if Parent <> nil then
            if not Parent.HasAsParent(Folder) then
              Folder.Parent := Parent
            else
              raise EdxPSExplorer.CreateFmt(cxGetResourceString(@sdxCyclicIDReferences), [Folder.Name, Parent.Name]);
        end
        else
          Folder.Parent := Root;
      end;
    end;

    procedure BuildItems;
    var
      I: Integer;
      Parent: TdxPSDBBasedExplorerFolder;
      Item: TdxPSDBBasedExplorerItem;
    begin
      for I := 0 to ItemList.Count - 1 do
      begin
        Item := TdxPSDBBasedExplorerItem(ItemList[I]);
        if not AreIDsEqual(Item.ParentID, TdxPSDBBasedExplorerFolder(Root).ID) then
        begin
          Parent := FindFolderByID(Item.ParentID);
          if Parent <> nil then
            if not Parent.HasAsParent(Item) then
              Item.Parent := Parent
            else
              raise EdxPSExplorer.CreateFmt(cxGetResourceString(@sdxCyclicIDReferences), [Item.Name, Parent.Name]);
        end
        else
          Item.Parent := Root;
      end;
    end;

  begin
    BuildFolders;
    BuildItems;
  end;

begin
  if CheckDataSets and AreFieldNameMapsValid then
  begin
    FFolderList := TList.Create;
    FItemList := TList.Create;

    LoadFolders;
    LoadItems;
    BuildTree;
  end;
end;

function TdxPSDBBasedExplorer.CanDelete(AnItem: TCustomdxPSExplorerItem): Boolean;
begin
  Result := inherited CanDelete(AnItem) and CheckItemDataSet(AnItem);
end;

function TdxPSDBBasedExplorer.CanMoveTo(AnItem, AParent: TCustomdxPSExplorerItem): Boolean;
begin
  Result := inherited CanMoveTo(AnItem, AParent) and ((esLoading in State) or CheckItemDataSet(AnItem));
end;

function TdxPSDBBasedExplorer.CanRenameTo(AnItem: TCustomdxPSExplorerItem;
  const AName: string): Boolean;
begin
  Result := inherited CanRenameTo(AnItem, AName) and ((esLoading in State) or CheckItemDataSet(AnItem));
end;

procedure TdxPSDBBasedExplorer.Delete(AnItem: TCustomdxPSExplorerItem);
begin
  if not (esLoading in State) and CanDelete(AnItem) and LocateItem(AnItem) then
    if AnItem is TdxPSExplorerFolder then
    begin
      FolderList.Remove(AnItem);
      Folders.Delete;
    end
    else
    begin
      ItemList.Remove(AnItem);
      Items.Delete;
    end;

  inherited;
end;

procedure TdxPSDBBasedExplorer.MoveTo(AnItem: TCustomdxPSExplorerItem;
  AParent: TdxPSExplorerFolder);
var
  ParentID: Integer;
begin
  if not (esLoading in State) and CanMoveTo(AnItem, AParent) and LocateItem(AnItem) then
    with GetItemDataSet(AnItem) do
    begin
      Edit;
      ParentID := RootIDValue;
      if AParent <> nil then
        ParentID := TdxPSDBBasedExplorerFolder(AParent).ID;
      FieldValues[GetItemFieldNamesMap(AnItem).ParentID] := ParentID;
      Post;
    end;
  inherited;
end;

procedure TdxPSDBBasedExplorer.RenameTo(AnItem: TCustomdxPSExplorerItem; AName: string);
begin
  if not (esLoading in State) and LocateItem(AnItem) then
    with GetItemDataSet(AnItem) do
    begin
      Edit;
      FieldValues[GetItemFieldNamesMap(AnItem).Name] := AName;
      Post;
    end;
  inherited;
end;

procedure TdxPSDBBasedExplorer.DoRefresh;
begin
  inherited;
  FreeAndNil(FFolderList);
  FreeAndNil(FItemList);
end;

class function TdxPSDBBasedExplorer.GetFoldersFieldNamesMapClass: TdxPSDBBasedExplorerFoldersFieldNamesMapClass;
begin
  Result := TdxPSDBBasedExplorerFoldersFieldNamesMap;
end;

class function TdxPSDBBasedExplorer.GetItemsFieldNamesMapClass: TdxPSDBBasedExplorerItemsFieldNamesMapClass;
begin
  Result := TdxPSDBBasedExplorerItemsFieldNamesMap;
end;

function TdxPSDBBasedExplorer.AreIDsEqual(AnID1, AnID2: Integer): Boolean;
begin
  Result := AnID1 = AnID2;
end;

procedure TdxPSDBBasedExplorer.Changed;
begin
end;

function TdxPSDBBasedExplorer.CheckDataSets: Boolean;
begin
  Result := CheckItemDataSet(TdxPSDBBasedExplorerFolder) and CheckItemDataSet(TdxPSDBBasedExplorerItem);
end;

function TdxPSDBBasedExplorer.CheckItemDataSet(AnItem: TCustomdxPSExplorerItem): Boolean;
begin
  Result := CheckItemDataSet(TdxPSExplorerItemClass(AnItem.ClassType));
end;

function TdxPSDBBasedExplorer.CheckItemDataSet(AnItemClass: TCustomdxPSExplorerItemClass): Boolean;
var
  DataSet: TDataSet;
begin
  DataSet := GetItemDataSet(AnItemClass);
  Result := (DataSet <> nil) and DataSet.Active;
end;

function TdxPSDBBasedExplorer.CreateDataStream(AMode: TBlobStreamMode): TStream;
begin
  Result := Items.CreateBlobStream(Items.FindField(ItemsFieldNamesMap.Data), AMode);
end;

function TdxPSDBBasedExplorer.GetItemDataSet(AnItem: TCustomdxPSExplorerItem): TDataSet;
begin
  Result := GetItemDataSet(TCustomdxPSExplorerItemClass(AnItem.ClassType));
end;

function TdxPSDBBasedExplorer.GetItemDataSet(AnItemClass: TCustomdxPSExplorerItemClass): TDataSet;
begin
  if AnItemClass.InheritsFrom(TdxPSDBBasedExplorerFolder) then
    Result := Folders
  else
    if AnItemClass.InheritsFrom(TdxPSDBBasedExplorerItem) then
      Result := Items
    else
      Result := nil;
end;

function TdxPSDBBasedExplorer.GetItemFieldNamesMap(AnItem: TCustomdxPSExplorerItem): TdxPSDBBasedExplorerFieldNamesMap;
begin
  Result := GetItemFieldNamesMap(TdxPSExplorerItemClass(AnItem.ClassType));
end;

function TdxPSDBBasedExplorer.GetItemFieldNamesMap(AnItemClass: TCustomdxPSExplorerItemClass): TdxPSDBBasedExplorerFieldNamesMap;
begin
  if AnItemClass.InheritsFrom(TdxPSDBBasedExplorerFolder) then
    Result := FoldersFieldNamesMap
  else
    if AnItemClass.InheritsFrom(TdxPSDBBasedExplorerItem) then
      Result := ItemsFieldNamesMap
    else
      Result := nil;
end;

function TdxPSDBBasedExplorer.GetUniqueFolderID: Integer;
var
  I, ID: Integer;
begin
  if FolderList <> nil then
  begin
    Result := -1;
    for I := 0 to FolderList.Count - 1 do
    begin
      ID := TdxPSDBBasedExplorerFolder(FolderList.List[I]).ID;
      if ID > Result then Result := ID;
    end;
    Inc(Result);
  end
  else
    Result := 0;
end;

function TdxPSDBBasedExplorer.GetUniqueItemID: Integer;
var
  I, ID: Integer;
begin
  if ItemList <> nil then
  begin
    Result := -1;
    for I := 0 to ItemList.Count - 1 do
    begin
      ID := TdxPSDBBasedExplorerItem(ItemList.List[I]).ID;
      if ID > Result then Result := ID;
    end;
    Inc(Result);
  end
  else
    Result := 0;
end;

procedure TdxPSDBBasedExplorer.LoadFolderFromCurrentRecord(AFolder: TdxPSDBBasedExplorerFolder);
begin
  AFolder.FID := GetFieldValue(Folders, FoldersFieldNamesMap.ID);
  AFolder.FParentID := VarToInt(GetFieldValue(Folders, FoldersFieldNamesMap.ParentID));
  AFolder.Name := VarToStr(GetFieldValue(Folders, FoldersFieldNamesMap.Name));
end;

procedure TdxPSDBBasedExplorer.LoadItemFromCurrentRecord(AnItem: TdxPSDBBasedExplorerItem);
begin
  AnItem.FID := GetFieldValue(Items, ItemsFieldNamesMap.ID);
  AnItem.FParentID := VarToInt(GetFieldValue(Items, ItemsFieldNamesMap.ParentID));
  AnItem.Name := VarToStr(GetFieldValue(Items, ItemsFieldNamesMap.Name));
end;

procedure TdxPSDBBasedExplorer.SaveFolderToCurrentRecord(AFolder: TdxPSDBBasedExplorerFolder);
begin
  if not IsFolderIDAutoIncField then
    SetFieldValue(Folders, FoldersFieldNamesMap.ID, AFolder.ID);
  SetFieldValue(Folders, FoldersFieldNamesMap.Name, AFolder.Name);
  SetFieldValue(Folders, FoldersFieldNamesMap.ParentID, AFolder.ParentID);
end;

procedure TdxPSDBBasedExplorer.SaveItemToCurrentRecord(AnItem: TdxPSDBBasedExplorerItem;
  AReportLink: TBasedxReportLink);
begin
  if not IsItemIDAutoIncField then
    SetFieldValue(Items, ItemsFieldNamesMap.ID, AnItem.ID);
  SetFieldValue(Items, ItemsFieldNamesMap.Name, AnItem.Name);
  SetFieldValue(Items, ItemsFieldNamesMap.ParentID, AnItem.ParentID);
  AnItem.RetrieveReportData(AReportLink);
end;

procedure TdxPSDBBasedExplorer.DoLoadError(ADataSet: TDataSet);
begin
  if Assigned(FOnLoadError) then FOnLoadError(Self, ADataSet);
end;

function TdxPSDBBasedExplorer.GetFieldValue(ADataSet: TDataSet; const AName: string): Variant;
begin
  if IsFieldNameValid(ADataSet, AName) then
    Result := ADataSet[AName]
  else
    Result := Null;
end;

function TdxPSDBBasedExplorer.IsFieldNameValid(ADataSet: TDataSet; const AName: string): Boolean;
begin
  Result := (AName <> '') and (ADataSet <> nil) and (ADataSet.FindField(AName) <> nil);
end;

procedure TdxPSDBBasedExplorer.SetFieldValue(ADataSet: TDataSet; const AName: string;
  const Value: Variant);
begin
  if IsFieldNameValid(ADataSet, AName) then
    ADataSet[AName] := Value;
end;

function TdxPSDBBasedExplorer.LocateItem(AnItem: TCustomdxPSExplorerItem): Boolean;
begin
  Result := True;
  if esLoading in State then Exit;

  if AnItem is TdxPSDBBasedExplorerItem then
    Result := IsFieldNameValid(Items, ItemsFieldNamesMap.ID) and
      Items.Locate(ItemsFieldNamesMap.ID, TdxPSDBBasedExplorerItem(AnItem).ID, [])
  else
    if AnItem is TdxPSDBBasedExplorerFolder then
      Result := IsFieldNameValid(Folders, FoldersFieldNamesMap.ID) and
        Folders.Locate(FoldersFieldNamesMap.ID, TdxPSDBBasedExplorerFolder(AnItem).ID, []);
end;

procedure TdxPSDBBasedExplorer.PrepareIterate(ADataSet: TDataSet; out ABookmark: TBookmark);
begin
  ADataSet.DisableControls;
  ABookmark := nil;
  if ADataSet.Active and (ADataSet.RecordCount > 0) then
    ABookmark := ADataSet.GetBookmark;
end;

procedure TdxPSDBBasedExplorer.UnprepareIterate(ADataSet: TDataSet; var ABookmark: TBookmark);
begin
  if ABookmark <> nil then
  begin
    if ADataSet.Active and ADataSet.BookmarkValid(ABookmark) then
      ADataSet.GotoBookmark(ABookmark);
    ADataSet.FreeBookmark(ABookmark);
    ABookmark := nil;
  end;
  ADataSet.EnableControls;
end;

function TdxPSDBBasedExplorer.IsFolderIDAutoIncField: Boolean;
begin
  Result := Folders.FieldByName(FoldersFieldNamesMap.ID) is TAutoIncField;
end;

function TdxPSDBBasedExplorer.IsItemIDAutoIncField: Boolean;
begin
  Result := Items.FieldByName(ItemsFieldNamesMap.ID) is TAutoIncField;
end;

function TdxPSDBBasedExplorer.GetRoot: TdxPSDBBasedExplorerFolder;
begin
  Result := inherited Root as TdxPSDBBasedExplorerFolder;
end;

procedure TdxPSDBBasedExplorer.SetFolders(Value: TDataSet);
begin
  if FFolders <> Value then
  begin
    FFolders := Value;
    //FDataLink.DataSource := FFolders.DataSource;
    //if Value <> nil then Value.FreeNotification(Self);

    Changed;
  end;
end;

procedure TdxPSDBBasedExplorer.SetFoldersFieldNamesMap(Value: TdxPSDBBasedExplorerFoldersFieldNamesMap);
begin
  FoldersFieldNamesMap.Assign(Value);
end;

procedure TdxPSDBBasedExplorer.SetItems(Value: TDataSet);
begin
  if FItems <> Value then
  begin
    FItems := Value;
    Changed;
  end;
end;

procedure TdxPSDBBasedExplorer.SetItemsFieldNamesMap(Value: TdxPSDBBasedExplorerItemsFieldNamesMap);
begin
  ItemsFieldNamesMap.Assign(Value);
end;

end.
