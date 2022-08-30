{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressOfficeCore Library classes                        }
{                                                                    }
{           Copyright (c) 2000-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSOFFICECORE LIBRARY AND ALL     }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM       }
{   ONLY.                                                            }
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

unit dxCloudStorage;

{$I cxVer.inc}
{$SCOPEDENUMS ON}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, SysUtils, Classes, Forms, Windows, Graphics,
  Generics.Defaults, Generics.Collections,
  dxWinInet, dxAuthorizationAgents, dxThreading,
  dxCore, dxCoreClasses, dxGenerics, dxForms, cxClasses,
  dxWebFileTransferManager;

type
  TdxCloudStorage = class;
  TdxCloudStorageProvider = class;
  TdxCloudStorageItem = class;
  TdxCloudStorageFolder = class;
  TdxCloudStorageFolderList = class;
  TdxCloudStorageFiles = class;
  TdxCloudStorageFileTransferManager = class;

  { TdxCloudStorageProviderImageDictionary }

  TdxCloudImageSize = (is16, is32, is64, is128, is256);

  TdxCloudStorageProviderImageDictionary = class
  public const
    CloudImageSizeMap: array[TdxCloudImageSize] of Integer = (16, 32, 64, 128, 256);
  private type
  {$REGION 'private type'}
    TIcons = TDictionary<string, Integer>;
  {$ENDREGION}
  strict private
    FList: TdxFastObjectList;
    FIcons: TIcons;
    FProvider: TdxCloudStorageProvider;
  protected
    function AddImage(const AUri: string; AGraphic: TGraphic): Integer;
    property List: TdxFastObjectList read FList;
  public
    constructor Create(AProvider: TdxCloudStorageProvider);
    destructor Destroy; override;

    function GetIcon(const AUri: string): TGraphic;

    property Provider: TdxCloudStorageProvider read FProvider;
  end;

  { TdxCloudStorageItemData }

  TdxCloudStorageItemData = record
    ID: string;
    Name: string;
    MIMEType: string;
    Parents: TArray<string>;
    IconLink: string;
    CreatedDate: TDateTime;
    ModifiedDate: TDateTime;
    Shared: Boolean;
    Trashed: Boolean;
    DownloadLink: string;
    FileSize: Integer;
    DriveID: string;
  end;
  TdxCloudStorageItemDataList = class(TList<TdxCloudStorageItemData>);

  { TdxCloudStorageItem }

  TdxCloudStorageItem = class abstract
  strict private
    FCreatedDate: TDateTime;
    FDownloadLink: string;
    FDriveID: string;
    FFileSize: Integer;
    FID: string;
    FIconLink: string;
    FMIMEType: string;
    FModifiedDate: TDateTime;
    FName: string;
    FParents: TdxCloudStorageFolderList;
    FShared: Boolean;
    FTrashed: Boolean;

    FOwner: TdxCloudStorageFiles;

    function GetProvider: TdxCloudStorageProvider; inline;
  protected
    procedure Assign(const AData: TdxCloudStorageItemData); virtual;
    procedure AddParent(const Value: TdxCloudStorageFolder); virtual;
    procedure ClearParents;
    procedure RemoveParent(const Value: TdxCloudStorageFolder); virtual;

    property DownloadLink: string read FDownloadLink;
    property FileSize: Integer read FFileSize;

    property Owner: TdxCloudStorageFiles read FOwner;
    property Provider: TdxCloudStorageProvider read GetProvider;
  public
    constructor Create(AOwner: TdxCloudStorageFiles); virtual;
    destructor Destroy; override;
    procedure BeforeDestruction; override;

    procedure CopyTo(AParent: TdxCloudStorageFolder); virtual;
    procedure Delete; virtual;
    procedure MoveTo(AParent: TdxCloudStorageFolder); virtual;
    procedure MoveToTrash; virtual;
    procedure RestoreFromTrash; virtual;

    function IsFolder: Boolean; virtual;

    property CreatedDate: TDateTime read FCreatedDate;
    property DriveID: string read FDriveID;
    property IconLink: string read FIconLink;
    property ID: string read FID;
    property MIMEType: string read FMIMEType;
    property ModifiedDate: TDateTime read FModifiedDate;
    property Name: string read FName;
    property Parents: TdxCloudStorageFolderList read FParents;
    property Shared: Boolean read FShared;
    property Trashed: Boolean read FTrashed;
  end;

  { TdxCloudStorageItemList }

  TdxCloudStorageItemList<T: TdxCloudStorageItem> = class abstract
  strict private
    FItems: TcxObjectList;
  private
    function GetCount: Integer; inline;
    function GetItem(Index: Integer): T; inline;
  protected
    procedure Add(AItem: T); inline;
    procedure Clear; inline;
    procedure Extract(AItem: T); inline;

    function AreOwnObjects: Boolean; virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;

    function IndexOf(AItem: T): Integer;
    function FindByID(const Value: string): T;

    property Count: Integer read GetCount;
    property Items[Index: Integer]: T read GetItem; default;
  end;

  TdxCloudStorageItemList = class(TdxCloudStorageItemList<TdxCloudStorageItem>)
  protected
    function AreOwnObjects: Boolean; override;
  end;

  { TdxCloudStorageFile }

  TdxCloudStorageFile = class(TdxCloudStorageItem)
  public
    procedure DownloadContent;
    function GetExtension: string;
    procedure UploadContent(AStream: TStream);

    property DownloadLink;
    property FileSize;
  end;

  { TdxCloudStorageFolderChildren }

  TdxCloudStorageFolderChildren = class(TdxCloudStorageItemList)
  protected
    function AreOwnObjects: Boolean; override;
  end;

  { TdxCloudStorageCustomFolder }

  TdxCloudStorageCustomFolder = class(TdxCloudStorageItem)
  strict private
    FChildren: TdxCloudStorageFolderChildren;
    FIsLoaded: Boolean;
  protected
    procedure AddChild(AChild: TdxCloudStorageItem); overload; virtual; abstract;
    function AddChild(const AData: TdxCloudStorageItemData): TdxCloudStorageItem; overload;
    procedure AssignChildren(AList: TdxCloudStorageItemDataList);
    procedure Clear; virtual;
    procedure DoAssignChildren(AList: TdxCloudStorageItemDataList); virtual;
    procedure DoFetchChildren; virtual; abstract;

    procedure SetIsLoaded(const Value: Boolean);
  public
    constructor Create(AOwner: TdxCloudStorageFiles); override;
    destructor Destroy; override;

    function HasChildren: Boolean;
    function IndexOf(AItem: TdxCloudStorageItem): Integer;

    procedure FetchChildren(AForce: Boolean = False);

    function IsFolder: Boolean; override;
    function IsRoot: Boolean; virtual;

    property Children: TdxCloudStorageFolderChildren read FChildren;
    property IsLoaded: Boolean read FIsLoaded;
  end;

  { TdxCloudStorageFolder }

  TdxCloudStorageFolder = class(TdxCloudStorageCustomFolder)
  protected
    procedure AddChild(AChild: TdxCloudStorageItem); override;
    procedure Clear; override;
    procedure DoFetchChildren; override;
  public
    destructor Destroy; override;
    procedure CreateFolder(const AName: string);
    procedure UploadFile(const AFileName: string);
  end;

  { TdxCloudStorageSpecialFolder }

  TdxCloudStorageSpecialFolder = class(TdxCloudStorageCustomFolder)
  public type
    TType = (Recent, SharedByMe, SharedWithMe, Starred, Discover, Trash);
  strict private
    FType: TType;
  protected
    procedure AddChild(AChild: TdxCloudStorageItem); override;
    procedure Clear; override;
    procedure DoAssignChildren(AList: TdxCloudStorageItemDataList); override;
    procedure DoFetchChildren; override;
    procedure RemoveChild(AChild: TdxCloudStorageItem);
  public
    constructor Create(AOwner: TdxCloudStorageFiles; AType: TType); reintroduce;

    procedure CopyTo(AParent: TdxCloudStorageFolder); override;
    procedure Delete; override;
    procedure MoveTo(AParent: TdxCloudStorageFolder); override;
    procedure MoveToTrash; override;
    procedure RestoreFromTrash; override;

    property &Type: TType read FType;
  end;

  { TdxCloudStorageSpecialFolderList }

  TdxCloudStorageSpecialFolderList = class(TdxCloudStorageItemList<TdxCloudStorageSpecialFolder>)
  public type
    TEnumProc = reference to procedure (const ASpecialFolder: TdxCloudStorageSpecialFolder);
  protected
    function AreOwnObjects: Boolean; override;
  public
    function GetByType(AType: TdxCloudStorageSpecialFolder.TType): TdxCloudStorageSpecialFolder;
    procedure Enum(AProc: TEnumProc);
  end;

  { TdxCloudStorageFolderList }

  TdxCloudStorageFolderList = class(TdxCloudStorageItemList<TdxCloudStorageFolder>)
  protected
    function AreOwnObjects: Boolean; override;
  end;

  { TdxCloudStorageRoot }

  TdxCloudStorageRoot = class(TdxCloudStorageFolder)
  protected
    procedure AddParent(const Value: TdxCloudStorageFolder); override;
    procedure RemoveParent(const Value: TdxCloudStorageFolder); override;
  public
    procedure CopyTo(AParent: TdxCloudStorageFolder); override;
    procedure Delete; override;
    procedure MoveTo(AParent: TdxCloudStorageFolder); override;
    procedure MoveToTrash; override;
    procedure RestoreFromTrash; override;

    function IsRoot: Boolean; override;
  end;

  { TdxCloudStorageProviderCustomTask }

  TdxCloudStorageProviderCustomTask = class abstract(TdxWebTask)
  strict private
    FProvider: TdxCloudStorageProvider;
    FProviderLink: TcxObjectLink;
  protected
    function IsValid: Boolean; override;

    procedure DoError(AObject: TdxJSONValue); override;
    function GetHeader: string; override;
    function GetUserAgent: string; override;

    property Provider: TdxCloudStorageProvider read FProvider;
  public
    constructor Create(AProvider: TdxCloudStorageProvider); reintroduce; virtual;
    destructor Destroy; override;
    function IsEqual(const ATask: TdxWebTask): Boolean; override;
  end;

  { TdxCloudStorageProviderCreateFolderTask }

  TdxCloudStorageProviderCreateFolderTask = class abstract(TdxCloudStorageProviderCustomTask)
  strict private
    FData: TdxCloudStorageItemData;
    FName: string;
    FParentID: string;
  protected
    procedure DoComplete; override;
    function DoRun(const ACancelStatus: TdxTaskCancelCallback): TdxTaskCompletedStatus; override; final;

    function GetRequestObject: TdxJSONObject; virtual; abstract;
    function IsError(AObject: TdxJSONObject): Boolean; virtual; abstract;
    function PostRequest(AObject: TdxJSONObject): TdxJSONObject; virtual; abstract;

    property Name: string read FName;
    property ParentID: string read FParentID;
  public
    constructor Create(AProvider: TdxCloudStorageProvider; AParent: TdxCloudStorageFolder;
      const AName: string); reintroduce; virtual;
  end;

  { TdxCloudStorageProviderLoadImageTask }

  TdxCloudStorageProviderLoadImageTask = class(TdxCloudStorageProviderCustomTask)
  strict private
    FStream: TStream;
    FUri: string;
  protected
    function DoRun(const ACancelStatus: TdxTaskCancelCallback): TdxTaskCompletedStatus; override;
    procedure DoComplete; override;

    property Stream: TStream read FStream;
    property Uri: string read FUri;
  public
    constructor Create(AProvider: TdxCloudStorageProvider; const AUri: string); reintroduce; virtual;
    destructor Destroy; override;

    function IsEqual(const ATask: TdxWebTask): Boolean; override;
  end;

  { TdxCloudStorageProviderItemCustomTask }

  TdxCloudStorageProviderItemCustomTask = class abstract(TdxCloudStorageProviderCustomTask)
  strict private
    FItem: TdxCloudStorageItem;
    FItemLink: TcxObjectLink;
  protected
    function IsValid: Boolean; override;
  public
    constructor Create(AProvider: TdxCloudStorageProvider; AItem: TdxCloudStorageItem); reintroduce;
    destructor Destroy; override;

    function IsEqual(const ATask: TdxWebTask): Boolean; override;
    property Item: TdxCloudStorageItem read FItem;
  end;

  { TdxCloudStorageProviderDeleteItemTask }

  TdxCloudStorageProviderDeleteItemTask = class(TdxCloudStorageProviderItemCustomTask)
  protected
    procedure DoComplete; override;
  end;

  { TdxCloudStorageProviderMoveItemToTrashTask }

  TdxCloudStorageProviderMoveItemToTrashTask = class(TdxCloudStorageProviderItemCustomTask)
  protected
    FData: TdxCloudStorageItemData;
    procedure AssignData(AObject: TdxJSONObject);
    procedure DoComplete; override;

    property Data: TdxCloudStorageItemData read FData;
  end;

  { TdxCloudStorageProviderMoveItemToCustomTask }

  TdxCloudStorageProviderMoveItemToCustomTask = class abstract(TdxCloudStorageProviderItemCustomTask)
  strict private
    FData: TdxCloudStorageItemData;
    FParent: TdxCloudStorageFolder;
    FParentLink: TcxObjectLink;
  protected
    procedure AssignData(AObject: TdxJSONObject);
    function IsValid: Boolean; override;

    property Data: TdxCloudStorageItemData read FData;
  public
    constructor Create(AProvider: TdxCloudStorageProvider; AItem: TdxCloudStorageItem; AParent: TdxCloudStorageFolder); reintroduce;
    destructor Destroy; override;

    function IsEqual(const ATask: TdxWebTask): Boolean; override;
    property Parent: TdxCloudStorageFolder read FParent;
  end;

  { TdxCloudStorageProviderUpdateFolderCustomTask }

  TdxCloudStorageProviderUpdateFolderCustomTask = class abstract(TdxCloudStorageProviderItemCustomTask)
  strict private
    FChildren: TdxCloudStorageItemDataList;
    FData: TdxCloudStorageItemData;
    function GetFolder: TdxCloudStorageCustomFolder; inline;
  protected
    function AssignData(const ACancelStatus: TdxTaskCancelCallback; var AData: TdxCloudStorageItemData): TdxTaskCompletedStatus; virtual; abstract;
    function PopulateItems(const ACancelStatus: TdxTaskCancelCallback): TdxTaskCompletedStatus; virtual; abstract;

    procedure DoComplete; override; final;
    function DoRun(const ACancelStatus: TdxTaskCancelCallback): TdxTaskCompletedStatus; override; final;

    property Children: TdxCloudStorageItemDataList read FChildren;
  public
    constructor Create(AProvider: TdxCloudStorageProvider; AFolder: TdxCloudStorageCustomFolder); reintroduce;
    destructor Destroy; override;

    property Folder: TdxCloudStorageCustomFolder read GetFolder;
  end;

  { TdxCloudStorageProviderFetchAllCustomTask }

  TdxCloudStorageProviderFetchAllCustomTask = class abstract(TdxCloudStorageProviderCustomTask)
  strict private
    FItems: TdxCloudStorageItemDataList;
    FResult: TdxCloudStorageItemList;
    FRoot: TdxCloudStorageItemData;
  protected
    function BuildTree(const ACancelStatus: TdxTaskCancelCallback): TdxTaskCompletedStatus;
    procedure DoComplete; override;
    function DoRun(const ACancelStatus: TdxTaskCancelCallback): TdxTaskCompletedStatus; override; final;

    function AssignRoot(var ARoot: TdxCloudStorageItemData; const ACancelStatus: TdxTaskCancelCallback): TdxTaskCompletedStatus; virtual; abstract;
    function PopulateItems(const ACancelStatus: TdxTaskCancelCallback): TdxTaskCompletedStatus; virtual; abstract;

    property Items: TdxCloudStorageItemDataList read FItems;
  public
    constructor Create(AProvider: TdxCloudStorageProvider); override;
    destructor Destroy; override;
  end;

  { TdxCloudStorageFiles }

  TdxCloudStorageFiles = class abstract
  strict private
    FItems: TdxCloudStorageItemList;
    FOwner: TdxCloudStorageProvider;
    FSpecialFolders: TdxCloudStorageSpecialFolderList;
    function GetRoot: TdxCloudStorageRoot; inline;
    function GetStorage: TdxCloudStorage; inline;
    function GetTrash: TdxCloudStorageSpecialFolder; inline;
  protected
    procedure DoClear; virtual;

    function AddChild(const AParentID: string; const AChild: TdxCloudStorageItemData): TdxCloudStorageItem;
    procedure AddSpecialFolder(AType: TdxCloudStorageSpecialFolder.TType);
    procedure CreateFolder(AParent: TdxCloudStorageFolder; const AName: string);
    function CreateRoot: TdxCloudStorageItem;
    procedure Clear(ACreateRoot: Boolean = True);
    procedure CopyItemTo(AItem: TdxCloudStorageItem; AParent: TdxCloudStorageFolder);
    procedure DeleteItem(AItem: TdxCloudStorageItem);
    procedure MoveItemTo(AItem: TdxCloudStorageItem; AParent: TdxCloudStorageFolder);
    procedure MoveItemToTrash(AItem: TdxCloudStorageItem);
    procedure RestoreItemFromTrash(AItem: TdxCloudStorageItem);
    procedure PopulateSpecialFolders; virtual; abstract;
    procedure UpdateRecentFolder;

    property Items: TdxCloudStorageItemList read FItems;
    property Owner: TdxCloudStorageProvider read FOwner;
    property Storage: TdxCloudStorage read GetStorage;
  public
    constructor Create(AOwner: TdxCloudStorageProvider);
    destructor Destroy; override;

    function FindByID(const Value: string): TdxCloudStorageItem;
    procedure FetchAll;

    property Root: TdxCloudStorageRoot read GetRoot;
    property SpecialFolders: TdxCloudStorageSpecialFolderList read FSpecialFolders;
    property Trash: TdxCloudStorageSpecialFolder read GetTrash;
  end;

  { TdxCloudStorageFileDownloadStreamTask }

  TdxCloudStorageFileDownloadStreamTask = class(TdxWebFileDownloadStreamTask)
  public
    constructor Create(AManager: TdxCloudStorageFileTransferManager;
      AFile: TdxCloudStorageFile); reintroduce;
  end;

  { TdxCloudStorageUploadTask }

  TdxCloudStorageUploadTask = class abstract(TdxWebFileUploadTask)
  strict private
    FData: TdxCloudStorageItemData;
    FProvider: TdxCloudStorageProvider;
  protected
    procedure AssignData(AObject: TdxJSONObject);
    function GetResultFileID: string; override;

    property Data: TdxCloudStorageItemData read FData;
    property Provider: TdxCloudStorageProvider read FProvider;
  public
    constructor Create(AProvider: TdxCloudStorageProvider; const AProcessID, AUri: string; AStream: TStream); reintroduce;
  end;

  { TdxCloudStorageFileTransferManager }

  TdxCloudStorageFileTransferManager = class abstract(TdxWebFileTransferManager)
  strict private
    FProvider: TdxCloudStorageProvider;
    function GetStorage: TdxCloudStorage;
    function FindFile(const AProcessID: string): TdxCloudStorageFile;
  protected
    function CreateDownloadFileStreamTask(AFile: TdxCloudStorageFile): TdxCloudStorageFileDownloadStreamTask; virtual;
    function CreateUpdateFileStreamTask(AFile: TdxCloudStorageFile; AStream: TStream): TdxCloudStorageUploadTask; virtual; abstract;
    function CreateUploadFileTask(AParent: TdxCloudStorageFolder; const AFileName: string): TdxCloudStorageUploadTask; virtual; abstract;

    procedure DoDownload(const AProcessID: string; const APosition: Cardinal); override;
    procedure DoDownloaded(const AProcessID: string; const AStream: TStream); override;
    procedure DoError(const AErrorObject); override;
    procedure DoDownloading(const AProcessID: string; const AFileSize: Integer); override;
    procedure DoUploaded(const AProcessID, AFileID: string); override;
    procedure DoUploading(const AProcessID: string; const ASize: Integer); override;
    procedure DoUpload(const AProcessID: string; const APosition: Cardinal); override;

    property Provider: TdxCloudStorageProvider read FProvider;
    property Storage: TdxCloudStorage read GetStorage;
  public
    constructor Create(AProvider: TdxCloudStorageProvider);

    procedure DownloadFileStream(AFile: TdxCloudStorageFile);
    procedure UpdateFileContent(AFile: TdxCloudStorageFile; AStream: TStream);
    procedure UploadFile(AParent: TdxCloudStorageFolder; const AFileName: string);
  end;

  { TdxCloudStorageProvider }

  TdxCloudStorageProvider = class abstract(TcxInterfacedPersistent)
  public const
    DefaultRecentDayCountLimit = 100;
    DefaultRecentFileCountLimit = 100;
  strict private class var
    FRegistryExtensionToMIMEType: TDictionary<string, string>;
  private
    class procedure Initialize; static;
    class procedure Finalize; static;
  strict private
    FAuthorizationAgent: TdxCustomAuthorizationAgent;
    FFiles: TdxCloudStorageFiles;
    FFileTransferManager: TdxCloudStorageFileTransferManager;
    FFreeNotificator: TcxFreeNotificator;
    FImageDictionary: TdxCloudStorageProviderImageDictionary;
    FRecentDayCountLimit: Integer;
    FRecentFileCountLimit: Integer;
    FTasks: TdxWebTaskManager;

    function GetStorage: TdxCloudStorage; inline;
    procedure FreeNotificationHandler(Sender: TComponent);
    procedure SetAuthorizationAgent(const Value: TdxCustomAuthorizationAgent);
    procedure SetRecentDayCountLimit(const Value: Integer);
    procedure SetRecentFileCountLimit(const Value: Integer);
  protected
    function CreateFiles: TdxCloudStorageFiles; virtual; abstract;
    function CreateFileTransferManager: TdxCloudStorageFileTransferManager; virtual; abstract;

    procedure RunTask(ATask: TdxCloudStorageProviderCustomTask);

    class procedure RaiseOperationNotSupportedException; static;
    function CreateCopyItemToTask(AItem: TdxCloudStorageItem; AParent: TdxCloudStorageFolder): TdxCloudStorageProviderMoveItemToCustomTask; virtual;
    function CreateCreateFolderTask(AParent: TdxCloudStorageFolder; const AName: string): TdxCloudStorageProviderCreateFolderTask; virtual;
    function CreateDeleteItemTask(AItem: TdxCloudStorageItem): TdxCloudStorageProviderDeleteItemTask; virtual;
    function CreateFetchAllTask: TdxCloudStorageProviderFetchAllCustomTask; virtual;
    function CreateLoadImageTask(const AUri: string): TdxCloudStorageProviderLoadImageTask; virtual;
    function CreateMoveItemToTask(AItem: TdxCloudStorageItem; AParent: TdxCloudStorageFolder): TdxCloudStorageProviderMoveItemToCustomTask; virtual;
    function CreateMoveItemToTrashTask(AItem: TdxCloudStorageItem): TdxCloudStorageProviderMoveItemToTrashTask; virtual;
    function CreateRestoreItemFromTrashTask(AItem: TdxCloudStorageItem): TdxCloudStorageProviderItemCustomTask; virtual;
    function CreateUpdateFolderTask(AFolder: TdxCloudStorageFolder): TdxCloudStorageProviderUpdateFolderCustomTask; virtual;
    function CreateUpdateSpecialFolderTask(AFolder: TdxCloudStorageSpecialFolder): TdxCloudStorageProviderUpdateFolderCustomTask; virtual;

    function CreateItem(const AMIMEType: string): TdxCloudStorageItem; overload; virtual; abstract;
    function CreateItem(const AData: TdxCloudStorageItemData): TdxCloudStorageItem; overload;
    function CreateRoot(const AData: TdxCloudStorageItemData): TdxCloudStorageItem; overload;
    function DoGetSpecialFolderData(const AType: TdxCloudStorageSpecialFolder.TType): TdxCloudStorageItemData; virtual;
    function GetSpecialFolderData(const AType: TdxCloudStorageSpecialFolder.TType): TdxCloudStorageItemData;
    function ObjectToData(const AObject: TObject): TdxCloudStorageItemData; virtual; abstract;

    procedure LoadImageComplete(const AUri: string; const AStream: TStream);
    procedure LoadImage(const AUri: string); overload;

    function IsReady: Boolean; virtual;

    procedure CopyItemTo(AItem: TdxCloudStorageItem; AParent: TdxCloudStorageFolder);
    procedure CopyItemToComplete(AItem: TdxCloudStorageItem; AParent: TdxCloudStorageFolder; const AData: TdxCloudStorageItemData);
    procedure CreateFolder(AParent: TdxCloudStorageFolder; const AName: string);
    procedure CreateFolderComplete(const AFolderData: TdxCloudStorageItemData);
    procedure DeleteItem(AItem: TdxCloudStorageItem);
    procedure DeleteItemComplete(AItem: TdxCloudStorageItem);
    function GetHttpHeaders: string; virtual;
    procedure FetchAll;
    procedure FetchAllComplete(AItems: TdxCloudStorageItemList);
    procedure MoveItemTo(AItem: TdxCloudStorageItem; AParent: TdxCloudStorageFolder);
    procedure MoveItemToComplete(AItem: TdxCloudStorageItem; AParent: TdxCloudStorageFolder);
    procedure MoveItemToTrash(AItem: TdxCloudStorageItem);
    procedure MoveItemToTrashComplete(AItem: TdxCloudStorageItem; const ANewData: TdxCloudStorageItemData);
    procedure RestoreItemFromTrash(AItem: TdxCloudStorageItem);
    procedure RestoreItemFromTrashComplete(AItem: TdxCloudStorageItem; const ANewData: TdxCloudStorageItemData);
    procedure UpdateFolder(AFolder: TdxCloudStorageFolder);
    procedure UpdateSpecialFolder(AFolder: TdxCloudStorageSpecialFolder);
    procedure UpdateRoot;

    procedure Changed; virtual;
    procedure SubscribeAuthorizationAgent; virtual;
    procedure UnsubscribeAuthorizationAgent; virtual;

    property Files: TdxCloudStorageFiles read FFiles;
    property FileTransferManager: TdxCloudStorageFileTransferManager read FFileTransferManager;
    property RecentFileCountLimit: Integer read FRecentFileCountLimit write SetRecentFileCountLimit default DefaultRecentFileCountLimit;
    property RecentDayCountLimit: Integer read FRecentDayCountLimit write SetRecentDayCountLimit default DefaultRecentDayCountLimit;
    property Tasks: TdxWebTaskManager read FTasks;

    property ImageDictionary: TdxCloudStorageProviderImageDictionary read FImageDictionary;
  public
    constructor Create(AStorage: TdxCloudStorage); reintroduce;
    destructor Destroy; override;
    procedure BeforeDestruction; override;

    function GetExtension(AItem: TdxCloudStorageItem): string; virtual;

    class function GetDisplayName: string; virtual; abstract;
    class procedure Register;
    class procedure Unregister;

    property AuthorizationAgent: TdxCustomAuthorizationAgent read FAuthorizationAgent write SetAuthorizationAgent;
    property Storage: TdxCloudStorage read GetStorage;
    class property RegistryExtensionToMIMEType: TDictionary<string, string> read FRegistryExtensionToMIMEType;
  end;
  TdxCloudStorageProviderClass = class of TdxCloudStorageProvider;

  { TdxCloudStorageOAuth2Provider }

  TdxCloudStorageOAuth2Provider = class abstract(TdxCloudStorageProvider,
    IdxOAuth2AuthorizationAgentScopeRequestor)
  strict private
    function GetAuthorizationAgent: TdxOAuth2AuthorizationAgent; inline;
    procedure SetAuthorizationAgent(const Value: TdxOAuth2AuthorizationAgent); inline;
  protected
    function GetHttpHeaders: string; override;
    procedure SubscribeAuthorizationAgent; override;
    procedure UnsubscribeAuthorizationAgent; override;

    // IdxOAuth2AuthorizationAgentScopeRequestor
    function GetScopes: TStringList; virtual;
  public
    property AuthorizationAgent: TdxOAuth2AuthorizationAgent read GetAuthorizationAgent write SetAuthorizationAgent;
  end;

  { TdxCloudStorage }

  TdxCloudStorage = class(TcxComponent)
  public type
    TErrorEvent = procedure(Sender: TObject; const AErrorObject) of object;
    TFolderChangeEvent = procedure(Sender: TObject; AFolder: TdxCloudStorageCustomFolder) of object;
    TFolderChangeEventHandler = TdxMulticastMethod<TFolderChangeEvent>;
    TItemChangedEvent = procedure(Sender: TObject; const AItem: TdxCloudStorageItem) of object;
    TItemChangedEventHandler = TdxMulticastMethod<TItemChangedEvent>;

    TItemDownloadedEvent = procedure(Sender: TObject; const AItem: TdxCloudStorageItem; AStream: TStream) of object;
    TItemDownloadEvent = procedure(Sender: TObject; const AItem: TdxCloudStorageItem; const APosition: Integer) of object;
    TItemDownloadingEvent = procedure(Sender: TObject; const AItem: TdxCloudStorageItem; const ASize: Integer) of object;
    TItemUploadedEvent = procedure(Sender: TObject; const AItem: TdxCloudStorageItem) of object;
    TItemUploadEvent = procedure(Sender: TObject; const AFileName: string; const APosition: Integer) of object;
    TItemUploadingEvent = procedure(Sender: TObject; const AFileName: string; const ASize: Integer) of object;

    TFileContentUploadedEvent = procedure(Sender: TObject; const AFile: TdxCloudStorageFile) of object;
    TFileContentUploadEvent = procedure(Sender: TObject; const AFile: TdxCloudStorageFile; const APosition: Integer) of object;
    TFileContentUploadingEvent = procedure(Sender: TObject; const AFile: TdxCloudStorageFile; const ASize: Integer) of object;
  protected type
    TImageLoadedEvent = procedure(Sender: TObject; const AUri: string) of object;
    TImageLoadedEventHandler = TdxMulticastMethod<TImageLoadedEvent>;
  private var
    class var FRegisteredProviders: TcxRegisteredClasses;
    class function GetRegisteredProviders: TcxRegisteredClasses; static;
    class procedure Finalize; static;
  strict private
    FConnected: Boolean;
    FProvider: TdxCloudStorageProvider;
    FProviderClass: TdxCloudStorageProviderClass;

    FChangedHandlers: TdxNotifyEventHandler;
    FConnectedChangedHandlers: TdxNotifyEventHandler;
    FTreeDataLoadingHandlers: TFolderChangeEventHandler;
    FTreeDataLoadedHandlers: TFolderChangeEventHandler;
    FFolderCreatedHandlers: TFolderChangeEventHandler;
    FImageLoadedHandlers: TImageLoadedEventHandler;
    FItemCopiedHandlers: TItemChangedEventHandler;
    FItemDeletedHandlers: TItemChangedEventHandler;
    FItemMovedHandlers: TItemChangedEventHandler;
    FItemMovedToTrashHandlers: TItemChangedEventHandler;
    FItemRestoredFromTrashHandlers: TItemChangedEventHandler;

    FOnChanged: TNotifyEvent;
    FOnConnectedChanged: TNotifyEvent;
    FOnTreeDataLoading: TFolderChangeEvent;
    FOnTreeDataLoaded: TFolderChangeEvent;
    FOnError: TErrorEvent;
    FOnFolderCreated: TFolderChangeEvent;
    FOnItemCopied: TItemChangedEvent;
    FOnItemDeleted: TItemChangedEvent;
    FOnItemMoved: TItemChangedEvent;
    FOnItemMovedToTrash: TItemChangedEvent;
    FOnItemRestoredFromTrash: TItemChangedEvent;

    FOnItemDownload: TItemDownloadEvent;
    FOnItemDownloaded: TItemDownloadedEvent;
    FOnItemDownloading: TItemDownloadingEvent;
    FOnItemUpload: TItemUploadEvent;
    FOnItemUploaded: TItemUploadedEvent;
    FOnItemUploading: TItemUploadingEvent;

    FOnFileContentUpload: TFileContentUploadEvent;
    FOnFileContentUploaded: TFileContentUploadedEvent;
    FOnFileContentUploading: TFileContentUploadingEvent;

    function CanConnection: Boolean;
    procedure CheckConnection;
    function GetFiles: TdxCloudStorageFiles; inline;
    function GetProviderClassName: string; inline;
    procedure RecreateProvider;
    procedure SetConnected(const Value: Boolean);
    procedure SetProvider(const Value: TdxCloudStorageProvider);
    procedure SetProviderClass(const Value: TdxCloudStorageProviderClass);
    procedure SetProviderClassName(const Value: string);
  protected
    procedure UpdateFolder(AFolder: TdxCloudStorageFolder);

    procedure DoChanged;
    procedure DoConnectedChanged;
    procedure DoError(const AErrorObject);
    procedure DoFolderCreated(AFolder: TdxCloudStorageFolder);
    procedure DoImageLoaded(const AUri: string);
    procedure DoItemCopied(ANewItem: TdxCloudStorageItem);
    procedure DoItemDeleted(AItem: TdxCloudStorageItem);
    procedure DoItemMoved(AItem: TdxCloudStorageItem);
    procedure DoItemMovedToTrash(AItem: TdxCloudStorageItem);
    procedure DoItemRestoredFromTrash(AItem: TdxCloudStorageItem);
    procedure DoTreeDataLoading(AFolder: TdxCloudStorageCustomFolder);
    procedure DoTreeDataLoaded(AFolder: TdxCloudStorageCustomFolder);

    procedure DoFileContentUpload(const AFile: TdxCloudStorageFile; const APosition: Integer);
    procedure DoFileContentUploaded(const AFile: TdxCloudStorageFile);
    procedure DoFileContentUploading(const AFile: TdxCloudStorageFile; const ASize: Integer);
    procedure DoItemDownload(const AItem: TdxCloudStorageItem; const APosition: Integer);
    procedure DoItemDownloaded(const AItem: TdxCloudStorageItem; AStream: TStream);
    procedure DoItemDownloading(const AItem: TdxCloudStorageItem; const ASize: Integer);
    procedure DoItemUpload(const AFileName: string; const APosition: Cardinal);
    procedure DoItemUploaded(const AItem: TdxCloudStorageItem);
    procedure DoItemUploading(const AFileName: string; const ASize: Integer);

    class procedure RegisterProvider(AProvider: TdxCloudStorageProviderClass); static;
    class procedure UnregisterProvider(AProvider: TdxCloudStorageProviderClass); static;

    function IsDestroying: Boolean;

    property ChangedHandlers: TdxNotifyEventHandler read FChangedHandlers;
    property ConnectedChangedHandlers: TdxNotifyEventHandler read FConnectedChangedHandlers;
    property FolderCreatedHandlers: TFolderChangeEventHandler read FFolderCreatedHandlers;
    property TreeDataLoadingHandlers: TFolderChangeEventHandler read FTreeDataLoadingHandlers;
    property TreeDataLoadedHandlers: TFolderChangeEventHandler read FTreeDataLoadedHandlers;
    property ImageLoadedHandlers: TImageLoadedEventHandler read FImageLoadedHandlers;
    property ItemCopiedHandlers: TItemChangedEventHandler read FItemCopiedHandlers;
    property ItemDeletedHandlers: TItemChangedEventHandler read FItemDeletedHandlers;
    property ItemMovedHandlers: TItemChangedEventHandler read FItemMovedHandlers;
    property ItemMovedToTrashHandlers: TItemChangedEventHandler read FItemMovedToTrashHandlers;
    property ItemRestoredFromTrashHandlers: TItemChangedEventHandler read FItemRestoredFromTrashHandlers;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    class property RegisteredProviders: TcxRegisteredClasses read GetRegisteredProviders;

    property ProviderClass: TdxCloudStorageProviderClass read FProviderClass write SetProviderClass;
    property Files: TdxCloudStorageFiles read GetFiles;
  published
    property ProviderClassName: string read GetProviderClassName write SetProviderClassName;
    property Provider: TdxCloudStorageProvider read FProvider write SetProvider;
    property Connected: Boolean read FConnected write SetConnected default False;

    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    property OnConnectedChanged: TNotifyEvent read FOnConnectedChanged write FOnConnectedChanged;
    property OnError: TErrorEvent read FOnError write FOnError;
    property OnFolderCreated: TFolderChangeEvent read FOnFolderCreated write FOnFolderCreated;
    property OnItemCopied: TItemChangedEvent read FOnItemCopied write FOnItemCopied;
    property OnItemDeleted: TItemChangedEvent read FOnItemDeleted write FOnItemDeleted;
    property OnItemMoved: TItemChangedEvent read FOnItemMoved write FOnItemMoved;
    property OnItemMovedToTrash: TItemChangedEvent read FOnItemMovedToTrash write FOnItemMovedToTrash;
    property OnItemRestoredFromTrash: TItemChangedEvent read FOnItemRestoredFromTrash write FOnItemRestoredFromTrash;
    property OnTreeDataLoading: TFolderChangeEvent read FOnTreeDataLoading write FOnTreeDataLoading;
    property OnTreeDataLoaded: TFolderChangeEvent read FOnTreeDataLoaded write FOnTreeDataLoaded;

    property OnFileContentUpload: TFileContentUploadEvent read FOnFileContentUpload write FOnFileContentUpload;
    property OnFileContentUploaded: TFileContentUploadedEvent read FOnFileContentUploaded write FOnFileContentUploaded;
    property OnFileContentUploading: TFileContentUploadingEvent read FOnFileContentUploading write FOnFileContentUploading;

    property OnItemDownload: TItemDownloadEvent read FOnItemDownload write FOnItemDownload;
    property OnItemDownloaded: TItemDownloadedEvent read FOnItemDownloaded write FOnItemDownloaded;
    property OnItemDownloading: TItemDownloadingEvent read FOnItemDownloading write FOnItemDownloading;
    property OnItemUpload: TItemUploadEvent read FOnItemUpload write FOnItemUpload;
    property OnItemUploaded: TItemUploadedEvent read FOnItemUploaded write FOnItemUploaded;
    property OnItemUploading: TItemUploadingEvent read FOnItemUploading write FOnItemUploading;
  end;


implementation

uses
  RTLConsts, Registry,
  cxDateUtils, dxGDIPlusClasses,

  dxCloudStorageGoogleDriveProvider,
  dxCloudStorageMicrosoftOneDriveProvider,

  dxCloudStorageStrs;


{ TdxCloudStorageProviderImageDictionary }

constructor TdxCloudStorageProviderImageDictionary.Create(AProvider: TdxCloudStorageProvider);
begin
  inherited Create;
  FProvider := AProvider;
  FList := TdxFastObjectList.Create;
  FIcons := TIcons.Create;
end;

destructor TdxCloudStorageProviderImageDictionary.Destroy;
begin
  FreeAndNil(FIcons);
  FreeAndNil(FList);
  inherited Destroy;
end;

function TdxCloudStorageProviderImageDictionary.GetIcon(const AUri: string): TGraphic;
var
  AResult: Integer;
begin
  if not FIcons.TryGetValue(AUri, AResult) then
  begin
    Result := nil;
    Provider.LoadImage(AUri);
  end
  else
    Result := TGraphic(FList[AResult]);
end;

function TdxCloudStorageProviderImageDictionary.AddImage(const AUri: string; AGraphic: TGraphic): Integer;
var
  ACachedImage: TObject;
begin
  if not FIcons.TryGetValue(AUri, Result) then
  begin
    Result := FList.Add(AGraphic);
    FIcons.Add(AUri, Result);
    ACachedImage := nil;
  end
  else
  begin
    ACachedImage := FList[Result];
    FList[Result] := AGraphic;
  end;
  Provider.Storage.DoImageLoaded(AUri);
  ACachedImage.Free;
end;

{ TdxCloudStorageItem }

constructor TdxCloudStorageItem.Create(AOwner: TdxCloudStorageFiles);
begin
  inherited Create;
  FOwner := AOwner;
  FParents := TdxCloudStorageFolderList.Create;
end;

destructor TdxCloudStorageItem.Destroy;
begin
  ClearParents;
  FreeAndNil(FParents);
  inherited Destroy;;
end;

procedure TdxCloudStorageItem.BeforeDestruction;
begin
  inherited BeforeDestruction;
  cxClearObjectLinks(Self);
end;

procedure TdxCloudStorageItem.ClearParents;
begin
  while Parents.Count > 0 do
    RemoveParent(Parents[0]);
end;

procedure TdxCloudStorageItem.CopyTo(AParent: TdxCloudStorageFolder);
begin
  Owner.CopyItemTo(Self, AParent);
end;

procedure TdxCloudStorageItem.Delete;
begin
  Owner.DeleteItem(Self);
end;

procedure TdxCloudStorageItem.MoveTo(AParent: TdxCloudStorageFolder);
begin
  Owner.MoveItemTo(Self, AParent);
end;

procedure TdxCloudStorageItem.MoveToTrash;
begin
  if not Trashed then
    Owner.MoveItemToTrash(Self);
end;

procedure TdxCloudStorageItem.RestoreFromTrash;
begin
  if Trashed then
    Owner.RestoreItemFromTrash(Self);
end;

function TdxCloudStorageItem.IsFolder: Boolean;
begin
  Result := False;
end;

procedure TdxCloudStorageItem.Assign(const AData: TdxCloudStorageItemData);
begin
  FID := AData.ID;
  FDownloadLink := AData.DownloadLink;
  FName := AData.Name;
  FMIMEType := AData.MIMEType;
  FCreatedDate := AData.CreatedDate;
  FModifiedDate := AData.ModifiedDate;
  FIconLink := AData.IconLink;
  FShared := AData.Shared;
  FTrashed := AData.Trashed;
  FFileSize := AData.FileSize;
  FDriveID := AData.DriveID;
end;

procedure TdxCloudStorageItem.AddParent(const Value: TdxCloudStorageFolder);
begin
  if FParents.IndexOf(Value) = -1 then
  begin
    FParents.Add(Value);
    Value.Children.Add(Self);
  end;
end;

procedure TdxCloudStorageItem.RemoveParent(const Value: TdxCloudStorageFolder);
begin
  FParents.Extract(Value);
  Value.Children.Extract(Self);
end;

function TdxCloudStorageItem.GetProvider: TdxCloudStorageProvider;
begin
  Result := Owner.Owner;
end;

{ TdxCloudStorageItemList }

constructor TdxCloudStorageItemList<T>.Create;
begin
  inherited Create;
  FItems := TcxObjectList.Create(AreOwnObjects);
end;

destructor TdxCloudStorageItemList<T>.Destroy;
begin
  FreeAndNil(FItems);
  inherited Destroy;
end;

function TdxCloudStorageItemList<T>.IndexOf(AItem: T): Integer;
begin
  Result := FItems.IndexOf(TObject(AItem));
end;

function TdxCloudStorageItemList<T>.FindByID(const Value: string): T;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Result := Items[I];
    if Result.ID = Value then
      Exit;
  end;
  Result := Default(T);
end;

procedure TdxCloudStorageItemList<T>.Add(AItem: T);
begin
  if IndexOf(AItem) >= 0 then
  begin
    Exit;
  end;
  FItems.Add(TObject(AItem));
end;

procedure TdxCloudStorageItemList<T>.Clear;
begin
  FItems.Clear;
end;

procedure TdxCloudStorageItemList<T>.Extract(AItem: T);
begin
  FItems.Extract(TObject(AItem));
end;

function TdxCloudStorageItemList<T>.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TdxCloudStorageItemList<T>.GetItem(Index: Integer): T;
begin
  Result := T(FItems[Index]);
end;

{ TdxCloudStorageItemList }

function TdxCloudStorageItemList.AreOwnObjects: Boolean;
begin
  Result := True;
end;

{ TdxCloudStorageFile }

procedure TdxCloudStorageFile.DownloadContent;
begin
  Provider.FileTransferManager.DownloadFileStream(Self);
end;

function TdxCloudStorageFile.GetExtension: string;
begin
  Result := Provider.GetExtension(Self);
end;

procedure TdxCloudStorageFile.UploadContent(AStream: TStream);
begin
  Provider.FileTransferManager.UpdateFileContent(Self, AStream);
end;

{ TdxCloudStorageFolderChildren }

function TdxCloudStorageFolderChildren.AreOwnObjects: Boolean;
begin
  Result := False;
end;

{ TdxCloudStorageCustomFolder }

constructor TdxCloudStorageCustomFolder.Create(AOwner: TdxCloudStorageFiles);
begin
  inherited Create(AOwner);
  FChildren := TdxCloudStorageFolderChildren.Create;
end;

destructor TdxCloudStorageCustomFolder.Destroy;
begin
  FreeAndNil(FChildren);
  inherited Destroy;
end;

procedure TdxCloudStorageCustomFolder.FetchChildren(AForce: Boolean = False);
begin
  if not IsLoaded or AForce then
  begin
    FIsLoaded := False;
    DoFetchChildren;
  end;
end;

function TdxCloudStorageCustomFolder.IndexOf(AItem: TdxCloudStorageItem): Integer;
begin
  Result := FChildren.IndexOf(AItem);
end;

function TdxCloudStorageCustomFolder.AddChild(const AData: TdxCloudStorageItemData): TdxCloudStorageItem;
begin
  Result := Owner.Items.FindByID(AData.ID);
  if Result = nil then
  begin
    Result := Provider.CreateItem(AData);
    Owner.Items.Add(Result);
  end
  else
    Result.Assign(AData);
  AddChild(Result);
end;

procedure TdxCloudStorageCustomFolder.AssignChildren(AList: TdxCloudStorageItemDataList);
begin
  Clear;
  DoAssignChildren(AList);
  FIsLoaded := True;
end;

procedure TdxCloudStorageCustomFolder.Clear;
begin
  Children.Clear;
end;

procedure TdxCloudStorageCustomFolder.DoAssignChildren(AList: TdxCloudStorageItemDataList);
var
  AItem: TdxCloudStorageItemData;
begin
  Clear;
  for AItem in AList do
    AddChild(AItem);
end;

procedure TdxCloudStorageCustomFolder.SetIsLoaded(const Value: Boolean);
begin
  FIsLoaded := Value;
end;

function TdxCloudStorageCustomFolder.IsFolder: Boolean;
begin
  Result := True;
end;

function TdxCloudStorageCustomFolder.IsRoot: Boolean;
begin
  Result := False;
end;

function TdxCloudStorageCustomFolder.HasChildren: Boolean;
begin
  Result := Children.Count > 0;
end;

{ TdxCloudStorageFolder }

destructor TdxCloudStorageFolder.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TdxCloudStorageFolder.CreateFolder(const AName: string);
begin
  Owner.CreateFolder(Self, AName);
end;

procedure TdxCloudStorageFolder.UploadFile(const AFileName: string);
begin
  Provider.FileTransferManager.UploadFile(Self, AFileName);
end;

procedure TdxCloudStorageFolder.AddChild(AChild: TdxCloudStorageItem);
begin
  AChild.AddParent(Self);
end;

procedure TdxCloudStorageFolder.Clear;
begin
  while Children.Count > 0 do
    Children[0].RemoveParent(Self);
end;

procedure TdxCloudStorageFolder.DoFetchChildren;
begin
  Provider.UpdateFolder(Self);
end;

{ TdxCloudStorageSpecialFolder }

constructor TdxCloudStorageSpecialFolder.Create(AOwner: TdxCloudStorageFiles; AType: TType);
begin
  inherited Create(AOwner);
  FType := AType;
  Assign(Provider.GetSpecialFolderData(FType));
end;

procedure TdxCloudStorageSpecialFolder.CopyTo(AParent: TdxCloudStorageFolder);
begin
  raise Exception.Create(cxGetResourceString(@sdxCloudStorageSpecialFolderCannotBeDeletedOrMovedException));
end;

procedure TdxCloudStorageSpecialFolder.Delete;
begin
  raise Exception.Create(cxGetResourceString(@sdxCloudStorageSpecialFolderCannotBeDeletedOrMovedException));
end;

procedure TdxCloudStorageSpecialFolder.MoveTo(AParent: TdxCloudStorageFolder);
begin
  raise Exception.Create(cxGetResourceString(@sdxCloudStorageSpecialFolderCannotBeDeletedOrMovedException));
end;

procedure TdxCloudStorageSpecialFolder.MoveToTrash;
begin
  raise Exception.Create(cxGetResourceString(@sdxCloudStorageSpecialFolderCannotBeDeletedOrMovedException));
end;

procedure TdxCloudStorageSpecialFolder.RestoreFromTrash;
begin
  raise Exception.Create(cxGetResourceString(@sdxCloudStorageSpecialFolderCannotBeDeletedOrMovedException));
end;

procedure TdxCloudStorageSpecialFolder.AddChild(AChild: TdxCloudStorageItem);
begin
  Children.Add(AChild);
end;

procedure TdxCloudStorageSpecialFolder.DoAssignChildren(AList: TdxCloudStorageItemDataList);
var
  AItem: TdxCloudStorageItemData;
  AChild, AParent: TdxCloudStorageItem;
  I: Integer;
begin
  inherited DoAssignChildren(AList);
  if &Type = TdxCloudStorageSpecialFolder.TType.Trash then
  begin
    for AItem in AList do
    begin
      AChild := Children.FindByID(AItem.ID);
      for I := Low(AItem.Parents) to High(AItem.Parents) do
      begin
        AParent := Children.FindByID(AItem.Parents[I]);
        if AParent <> nil then
        begin
          AChild.AddParent(TdxCloudStorageFolder(AParent));
        end;
      end;
    end;
    for I := Children.Count - 1 downto 0 do
    begin
      if Children[I].Parents.Count > 0 then
        Children.Extract(Children[I]);
    end;
  end;
end;

procedure TdxCloudStorageSpecialFolder.Clear;
begin
  Children.Clear;
end;

procedure TdxCloudStorageSpecialFolder.DoFetchChildren;
begin
  Provider.UpdateSpecialFolder(Self);
end;

procedure TdxCloudStorageSpecialFolder.RemoveChild(AChild: TdxCloudStorageItem);
begin
  Children.Extract(AChild);
end;

{ TdxCloudStorageSpecialFolderList }

procedure TdxCloudStorageSpecialFolderList.Enum(AProc: TEnumProc);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    AProc(Items[I]);
end;

function TdxCloudStorageSpecialFolderList.GetByType(AType: TdxCloudStorageSpecialFolder.TType): TdxCloudStorageSpecialFolder;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Items[I].&Type = AType then
      Exit(Items[I]);
  Result := nil;
end;

function TdxCloudStorageSpecialFolderList.AreOwnObjects: Boolean;
begin
  Result := True;
end;

{ TdxCloudStorageFolderList }

function TdxCloudStorageFolderList.AreOwnObjects: Boolean;
begin
  Result := False;
end;

{ TdxCloudStorageRoot }

procedure TdxCloudStorageRoot.AddParent(const Value: TdxCloudStorageFolder);
begin
  raise EArgumentException.Create(cxGetResourceString(@sdxCloudStorageRootShouldNotHaveParentException));
end;

procedure TdxCloudStorageRoot.CopyTo(AParent: TdxCloudStorageFolder);
begin
  raise Exception.Create(cxGetResourceString(@sdxCloudStorageRootCannotBeDeletedOrMovedException));
end;

procedure TdxCloudStorageRoot.Delete;
begin
  raise Exception.Create(cxGetResourceString(@sdxCloudStorageRootCannotBeDeletedOrMovedException));
end;

procedure TdxCloudStorageRoot.MoveTo(AParent: TdxCloudStorageFolder);
begin
  raise Exception.Create(cxGetResourceString(@sdxCloudStorageRootCannotBeDeletedOrMovedException));
end;

procedure TdxCloudStorageRoot.MoveToTrash;
begin
  raise Exception.Create(cxGetResourceString(@sdxCloudStorageRootCannotBeDeletedOrMovedException));
end;

procedure TdxCloudStorageRoot.RestoreFromTrash;
begin
  raise Exception.Create(cxGetResourceString(@sdxCloudStorageRootCannotBeDeletedOrMovedException));
end;

function TdxCloudStorageRoot.IsRoot: Boolean;
begin
  Result := True;
end;

procedure TdxCloudStorageRoot.RemoveParent(const Value: TdxCloudStorageFolder);
begin
  raise EArgumentException.Create(cxGetResourceString(@sdxCloudStorageRootShouldNotHaveParentException));
end;

{ TdxCloudStorageCustomTask }

constructor TdxCloudStorageProviderCustomTask.Create(AProvider: TdxCloudStorageProvider);
begin
  inherited Create(AProvider.Tasks);
  FProvider := AProvider;
  FProviderLink := cxAddObjectLink(Provider);
end;

destructor TdxCloudStorageProviderCustomTask.Destroy;
begin
  cxRemoveObjectLink(FProviderLink);
  inherited Destroy;
end;

procedure TdxCloudStorageProviderCustomTask.DoError(AObject: TdxJSONValue);
begin
  MainThreadSynchronize(procedure ()
    begin
      Provider.Storage.DoError(AObject);
    end);
end;

function TdxCloudStorageProviderCustomTask.GetHeader: string;
begin
  Result := Provider.GetHttpHeaders
end;

function TdxCloudStorageProviderCustomTask.GetUserAgent: string;
begin
  if Provider.AuthorizationAgent <> nil then
    Result := Provider.AuthorizationAgent.UserAgent
  else
    Result := '';
end;

function TdxCloudStorageProviderCustomTask.IsEqual(const ATask: TdxWebTask): Boolean;
begin
  Result := inherited IsEqual(ATask) and (TdxCloudStorageProviderCustomTask(ATask).Provider = Provider);
end;

function TdxCloudStorageProviderCustomTask.IsValid: Boolean;
var
  AResult: Boolean;
  AProvider: TdxCloudStorageProvider;
begin
  Result := FProviderLink.Ref <> nil;
  if Result then
  begin
    AProvider := Provider;
    MainThreadSynchronize(procedure()
      begin
        AResult := AProvider.IsReady;
      end);
    Result := AResult;
  end;
end;

{ TdxCloudStorageProviderCreateFolderTask }

constructor TdxCloudStorageProviderCreateFolderTask.Create(
  AProvider: TdxCloudStorageProvider; AParent: TdxCloudStorageFolder;
  const AName: string);
begin
  inherited Create(AProvider);
  FParentID := AParent.ID;
  FName := AName;
end;

procedure TdxCloudStorageProviderCreateFolderTask.DoComplete;
begin
  Provider.CreateFolderComplete(FData);
end;

function TdxCloudStorageProviderCreateFolderTask.DoRun(const ACancelStatus: TdxTaskCancelCallback): TdxTaskCompletedStatus;
var
  AObject: TdxJSONObject;
  AResult: TdxJSONObject;
begin
  Result := TdxTaskCompletedStatus.Fail;
  AObject := GetRequestObject;
  try
    UpdateRequestParams;
    AResult := PostRequest(AObject);
    try
      if (AResult = nil) or IsError(AResult) then
      begin
        DoError(AResult);
        Exit;
      end;
      FData := Provider.ObjectToData(AResult);
      Result := TdxTaskCompletedStatus.Success;
    finally
      AResult.Free;
    end;
  finally
    AObject.Free;
  end;
end;

{ TdxCloudStorageProviderLoadImageTask }

constructor TdxCloudStorageProviderLoadImageTask.Create(AProvider: TdxCloudStorageProvider; const AUri: string);
begin
  inherited Create(AProvider);
  FUri := AUri;
  FStream := TMemoryStream.Create;
end;

destructor TdxCloudStorageProviderLoadImageTask.Destroy;
begin
  FreeAndNil(FStream);
  inherited Destroy;
end;

function TdxCloudStorageProviderLoadImageTask.IsEqual(const ATask: TdxWebTask): Boolean;
begin
  Result := inherited IsEqual(ATask) and (TdxCloudStorageProviderLoadImageTask(ATask).Uri = Uri);
end;

function TdxCloudStorageProviderLoadImageTask.DoRun(const ACancelStatus: TdxTaskCancelCallback): TdxTaskCompletedStatus;
var
  AResult: Boolean;
begin
  Result := TdxTaskCompletedStatus.Success;
  if ACancelStatus then
    Exit(TdxTaskCompletedStatus.Cancelled);
  if not IsValid then
    Exit(TdxTaskCompletedStatus.Fail);
  UpdateRequestParams;
  AResult := TdxHttpHelper.GetStream(UserAgent, Uri, Header, FStream,
    function(const AUri: string): Boolean
    begin
      Result := not IsValid or ACancelStatus;
    end);
  if ACancelStatus then
    Exit(TdxTaskCompletedStatus.Cancelled);
  if not AResult then
    Result := TdxTaskCompletedStatus.Fail;
end;

procedure TdxCloudStorageProviderLoadImageTask.DoComplete;
begin
  Provider.LoadImageComplete(Uri, Stream);
end;

{ TdxCloudStorageProviderItemCustomTask }

constructor TdxCloudStorageProviderItemCustomTask.Create(AProvider: TdxCloudStorageProvider; AItem: TdxCloudStorageItem);
begin
  inherited Create(AProvider);
  FItem := AItem;
  FItemLink := cxAddObjectLink(Item);
end;

destructor TdxCloudStorageProviderItemCustomTask.Destroy;
begin
  cxRemoveObjectLink(FItemLink);
  inherited Destroy;
end;

function TdxCloudStorageProviderItemCustomTask.IsEqual(const ATask: TdxWebTask): Boolean;
begin
  Result := inherited IsEqual(ATask) and (TdxCloudStorageProviderItemCustomTask(ATask).Item = Item);
end;

function TdxCloudStorageProviderItemCustomTask.IsValid: Boolean;
begin
  Result := inherited IsValid and (FItemLink.Ref <> nil);
end;

{ TdxCloudStorageProviderDeleteItemTask }

procedure TdxCloudStorageProviderDeleteItemTask.DoComplete;
begin
  Provider.DeleteItemComplete(Item);
end;

{ TdxCloudStorageProviderMoveItemToTrashTask }

procedure TdxCloudStorageProviderMoveItemToTrashTask.AssignData(AObject: TdxJSONObject);
begin
  FData := Provider.ObjectToData(AObject);
end;

procedure TdxCloudStorageProviderMoveItemToTrashTask.DoComplete;
begin
  Provider.MoveItemToTrashComplete(Item, FData);
end;

{ TdxCloudStorageProviderMoveItemToCustomTask }

constructor TdxCloudStorageProviderMoveItemToCustomTask.Create(AProvider: TdxCloudStorageProvider; AItem: TdxCloudStorageItem; AParent: TdxCloudStorageFolder);
begin
  inherited Create(AProvider, AItem);
  FParent := AParent;
  FParentLink := cxAddObjectLink(FParent);
end;

destructor TdxCloudStorageProviderMoveItemToCustomTask.Destroy;
begin
  FreeAndNil(FParentLink);
  inherited Destroy;
end;

procedure TdxCloudStorageProviderMoveItemToCustomTask.AssignData(AObject: TdxJSONObject);
begin
  FData := Provider.ObjectToData(AObject);
end;

function TdxCloudStorageProviderMoveItemToCustomTask.IsValid: Boolean;
begin
  Result := inherited IsValid and (FParentLink.Ref <> nil);
end;

function TdxCloudStorageProviderMoveItemToCustomTask.IsEqual(const ATask: TdxWebTask): Boolean;
begin
  Result := inherited IsEqual(ATask) and (TdxCloudStorageProviderMoveItemToCustomTask(ATask).Parent = Parent);
end;

{ TdxCloudStorageProviderUpdateFolderCustomTask }

constructor TdxCloudStorageProviderUpdateFolderCustomTask.Create(AProvider: TdxCloudStorageProvider; AFolder: TdxCloudStorageCustomFolder);
begin
  inherited Create(AProvider, AFolder);
  FChildren := TdxCloudStorageItemDataList.Create;
end;

destructor TdxCloudStorageProviderUpdateFolderCustomTask.Destroy;
begin
  FreeAndNil(FChildren);
  inherited Destroy;
end;

procedure TdxCloudStorageProviderUpdateFolderCustomTask.DoComplete;
begin
  Provider.Storage.DoTreeDataLoading(Folder);
  try
    Folder.Assign(FData);
    Folder.AssignChildren(FChildren);
  finally
    Provider.Storage.DoTreeDataLoaded(Folder);
  end;
end;

function TdxCloudStorageProviderUpdateFolderCustomTask.DoRun(const ACancelStatus: TdxTaskCancelCallback): TdxTaskCompletedStatus;
begin
  Result := AssignData(ACancelStatus, FData);
  if Result <> TdxTaskCompletedStatus.Success then
    Exit;
  Result := PopulateItems(ACancelStatus);
end;

function TdxCloudStorageProviderUpdateFolderCustomTask.GetFolder: TdxCloudStorageCustomFolder;
begin
  Result := TdxCloudStorageCustomFolder(inherited Item);
end;

{ TdxCloudStorageProviderFetchAllCustomTask }

constructor TdxCloudStorageProviderFetchAllCustomTask.Create(AProvider: TdxCloudStorageProvider);
begin
  inherited Create(AProvider);
  FItems := TdxCloudStorageItemDataList.Create;
  FResult := TdxCloudStorageItemList.Create;
end;

destructor TdxCloudStorageProviderFetchAllCustomTask.Destroy;
begin
  FreeAndNil(FResult);
  FreeAndNil(FItems);
  inherited Destroy;
end;

function TdxCloudStorageProviderFetchAllCustomTask.BuildTree(const ACancelStatus: TdxTaskCancelCallback): TdxTaskCompletedStatus;
var
  AData: TdxCloudStorageItemData;
  AItem: TdxCloudStorageItem;
  AParent: TdxCloudStorageItem;
  I: Integer;
begin
  Result := AssignRoot(FRoot, ACancelStatus);
  if Result <> TdxTaskCompletedStatus.Success then
    Exit;
  FResult.Add(Provider.CreateRoot(FRoot));
  for AData in FItems do
  begin
    if ACancelStatus then
      Exit(TdxTaskCompletedStatus.Cancelled);
    FResult.Add(Provider.CreateItem(AData));
  end;
  for AData in FItems do
  begin
    if ACancelStatus then
      Exit(TdxTaskCompletedStatus.Cancelled);
    AItem := FResult.FindByID(AData.ID);
    for I := 0 to Length(AData.Parents) - 1 do
    begin
      AParent := FResult.FindByID(AData.Parents[I]);
      if AParent.IsFolder then
        AItem.AddParent(TdxCloudStorageFolder(AParent));
    end;
  end;
  Result := TdxTaskCompletedStatus.Success;
end;

function TdxCloudStorageProviderFetchAllCustomTask.DoRun(const ACancelStatus: TdxTaskCancelCallback): TdxTaskCompletedStatus;
begin
  Result := PopulateItems(ACancelStatus);
  if Result = TdxTaskCompletedStatus.Success then
    Result := BuildTree(ACancelStatus);
end;

procedure TdxCloudStorageProviderFetchAllCustomTask.DoComplete;
begin
  Provider.FetchAllComplete(FResult);
end;

{ TdxCloudStorageFiles }

constructor TdxCloudStorageFiles.Create(AOwner: TdxCloudStorageProvider);
begin
  inherited Create;
  FOwner := AOwner;
  FItems := TdxCloudStorageItemList.Create;
  FItems.Add(CreateRoot);
  FSpecialFolders := TdxCloudStorageSpecialFolderList.Create;
  PopulateSpecialFolders;
end;

destructor TdxCloudStorageFiles.Destroy;
begin
  FreeAndNil(FSpecialFolders);
  FreeAndNil(FItems);
  inherited Destroy;
end;

function TdxCloudStorageFiles.FindByID(const Value: string): TdxCloudStorageItem;
begin
  Result := Items.FindByID(Value);
end;

procedure TdxCloudStorageFiles.FetchAll;
begin
  Owner.FetchAll;
end;

function TdxCloudStorageFiles.GetStorage: TdxCloudStorage;
begin
  Result := Owner.Storage;
end;

function TdxCloudStorageFiles.GetTrash: TdxCloudStorageSpecialFolder;
begin
  Result := SpecialFolders.GetByType(TdxCloudStorageSpecialFolder.TType.Trash);
end;

procedure TdxCloudStorageFiles.DoClear;
begin
// do nothing
end;

function TdxCloudStorageFiles.AddChild(const AParentID: string; const AChild: TdxCloudStorageItemData): TdxCloudStorageItem;
var
  AParent: TdxCloudStorageItem;
  AFolder: TdxCloudStorageCustomFolder;
begin
  Result := nil;
  AParent := FindByID(AParentID);
  if (AParent <> nil) and AParent.IsFolder then
  begin
    AFolder := TdxCloudStorageCustomFolder(AParent);
    if AFolder.IsLoaded then
    begin
      Owner.Storage.DoTreeDataLoading(AFolder);
      try
        Result := AFolder.AddChild(AChild);
      finally
        Owner.Storage.DoTreeDataLoaded(AFolder);
      end;
    end;
  end;
end;

procedure TdxCloudStorageFiles.AddSpecialFolder(AType: TdxCloudStorageSpecialFolder.TType);
begin
  FSpecialFolders.Add(TdxCloudStorageSpecialFolder.Create(Self, AType));
end;

procedure TdxCloudStorageFiles.CreateFolder(AParent: TdxCloudStorageFolder; const AName: string);
begin
  FOwner.CreateFolder(AParent, AName);
end;

function TdxCloudStorageFiles.CreateRoot: TdxCloudStorageItem;
begin
  Result := TdxCloudStorageRoot.Create(Self);
end;

procedure TdxCloudStorageFiles.Clear(ACreateRoot: Boolean = True);
begin
  FItems.Clear;
  if ACreateRoot then
    FItems.Add(CreateRoot);
  DoClear;
end;

procedure TdxCloudStorageFiles.CopyItemTo(AItem: TdxCloudStorageItem; AParent: TdxCloudStorageFolder);
begin
  Owner.CopyItemTo(AItem, AParent);
end;

procedure TdxCloudStorageFiles.DeleteItem(AItem: TdxCloudStorageItem);
begin
  Owner.DeleteItem(AItem);
end;

procedure TdxCloudStorageFiles.MoveItemTo(AItem: TdxCloudStorageItem; AParent: TdxCloudStorageFolder);
begin
  Owner.MoveItemTo(AItem, AParent);
end;

procedure TdxCloudStorageFiles.MoveItemToTrash(AItem: TdxCloudStorageItem);
begin
  Owner.MoveItemToTrash(AItem);
end;

procedure TdxCloudStorageFiles.RestoreItemFromTrash(AItem: TdxCloudStorageItem);
begin
  Owner.RestoreItemFromTrash(AItem);
end;

procedure TdxCloudStorageFiles.UpdateRecentFolder;
var
  AFolder: TdxCloudStorageSpecialFolder;
begin
  AFolder := SpecialFolders.GetByType(TdxCloudStorageSpecialFolder.TType.Recent);
  if AFolder = nil then
    Exit;
  if AFolder.IsLoaded then
    AFolder.FetchChildren(True);
end;

function TdxCloudStorageFiles.GetRoot: TdxCloudStorageRoot;
begin
  if Items.Count = 0 then
    CreateRoot;
  Result := TdxCloudStorageRoot(Items[0]);
end;

{ TdxCloudStorageFileDownloadStreamTask }

constructor TdxCloudStorageFileDownloadStreamTask.Create(
  AManager: TdxCloudStorageFileTransferManager; AFile: TdxCloudStorageFile);
begin
  inherited Create(AManager, AManager.Provider.AuthorizationAgent, AFile.ID, AFile.DownloadLink);
end;

{ TdxCloudStorageUploadTask }

constructor TdxCloudStorageUploadTask.Create(
  AProvider: TdxCloudStorageProvider; const AProcessID, AUri: string; AStream: TStream);
begin
  inherited Create(AProvider.FileTransferManager, AProvider.AuthorizationAgent,
    AProcessID, AUri, AStream);
  FProvider := AProvider;
end;

procedure TdxCloudStorageUploadTask.AssignData(AObject: TdxJSONObject);
begin
  FData := Provider.ObjectToData(AObject);
end;

function TdxCloudStorageUploadTask.GetResultFileID: string;
begin
  Result := Data.ID;
end;

{ TdxCloudStorageFileTransferManager }

constructor TdxCloudStorageFileTransferManager.Create(AProvider: TdxCloudStorageProvider);
begin
  inherited Create;
  FProvider := AProvider;
end;

procedure TdxCloudStorageFileTransferManager.DownloadFileStream(AFile: TdxCloudStorageFile);
begin
  if Provider.IsReady then
    RunDownloadFileTask(CreateDownloadFileStreamTask(AFile));
end;

procedure TdxCloudStorageFileTransferManager.UploadFile(AParent: TdxCloudStorageFolder; const AFileName: string);
begin
  if Provider.IsReady then
    RunUploadFileTask(CreateUploadFileTask(AParent, AFileName));
end;

procedure TdxCloudStorageFileTransferManager.UpdateFileContent(AFile: TdxCloudStorageFile; AStream: TStream);
begin
  if Provider.IsReady then
    RunUploadFileTask(CreateUpdateFileStreamTask(AFile, AStream));
end;

function TdxCloudStorageFileTransferManager.CreateDownloadFileStreamTask(AFile: TdxCloudStorageFile): TdxCloudStorageFileDownloadStreamTask;
begin
  Result := TdxCloudStorageFileDownloadStreamTask.Create(Self, AFile);
end;

procedure TdxCloudStorageFileTransferManager.DoDownload(const AProcessID: string; const APosition: Cardinal);
begin
  inherited DoDownload(AProcessID, APosition);
  Storage.DoItemDownload(FindFile(AProcessID), APosition);
end;

procedure TdxCloudStorageFileTransferManager.DoError(const AErrorObject);
begin
  inherited DoError(AErrorObject);
  Storage.DoError(AErrorObject);
end;

procedure TdxCloudStorageFileTransferManager.DoDownloaded(const AProcessID: string; const AStream: TStream);
begin
  inherited DoDownloaded(AProcessID, AStream);
  Storage.DoItemDownloaded(FindFile(AProcessID), AStream);
end;

procedure TdxCloudStorageFileTransferManager.DoDownloading(const AProcessID: string; const AFileSize: Integer);
var
  ASize: Integer;
  AItem: TdxCloudStorageItem;
begin
  inherited DoDownloading(AProcessID, AFileSize);
  AItem := FindFile(AProcessID);
  if (AFileSize = -1) and (AItem <> nil) then
    ASize := AItem.FileSize
  else
    ASize := AFileSize;
  Storage.DoItemDownloading(AItem, ASize);
end;

procedure TdxCloudStorageFileTransferManager.DoUploaded(const AProcessID, AFileID: string);
var
  AItem: TdxCloudStorageFile;
begin
  inherited DoUploaded(AProcessID, AFileID);
  AItem := FindFile(AFileID);
  if AProcessID <> AFileID then
    Storage.DoItemUploaded(AItem)
  else
    Storage.DoFileContentUploaded(AItem);
end;

procedure TdxCloudStorageFileTransferManager.DoUploading(const AProcessID: string; const ASize: Integer);
var
  AItem: TdxCloudStorageFile;
begin
  inherited DoUploading(AProcessID, ASize);
  AItem := FindFile(AProcessID);
  if AItem <> nil then
    Storage.DoFileContentUploading(AItem, ASize)
  else
    Storage.DoItemUploading(AProcessID, ASize);
end;

procedure TdxCloudStorageFileTransferManager.DoUpload(const AProcessID: string; const APosition: Cardinal);
var
  AItem: TdxCloudStorageFile;
begin
  inherited DoUpload(AProcessID, APosition);
  AItem := FindFile(AProcessID);
  if AItem <> nil then
    Storage.DoFileContentUpload(AItem, APosition)
  else
    Storage.DoItemUpload(AProcessID, APosition);
end;

function TdxCloudStorageFileTransferManager.GetStorage: TdxCloudStorage;
begin
  Result := FProvider.Storage;
end;

function TdxCloudStorageFileTransferManager.FindFile(const AProcessID: string): TdxCloudStorageFile;
var
  AItem: TdxCloudStorageItem;
begin
  Result := nil;
  if FProvider.Files <> nil then
  begin
    AItem := FProvider.Files.Items.FindByID(AProcessID);
    if AItem is TdxCloudStorageFile then
      Result := TdxCloudStorageFile(AItem);
  end;
end;

{ TdxCloudStorageProvider }

class procedure TdxCloudStorageProvider.Initialize;
var
  ARegistry: TRegistry;
  AExts: TStringList;
  AExt, AMIMEType: string;
  I: Integer;
begin
  FRegistryExtensionToMIMEType := TDictionary<string, string>.Create;
  ARegistry := TRegistry.Create;
  try
    ARegistry.RootKey := HKEY_LOCAL_MACHINE;
    if ARegistry.OpenKeyReadOnly('\SOFTWARE\Classes\') and ARegistry.HasSubKeys then
    begin
      AExts := TStringList.Create;
      try
        ARegistry.GetKeyNames(AExts);
        ARegistry.CloseKey;
        for I := 0 to AExts.Count - 1 do
        begin
          AExt := AExts[I];
          if (Length(AExt) = 0) or (AExt[1] <> '.') then
            Continue;
          if ARegistry.OpenKeyReadOnly(Format('\SOFTWARE\Classes\%s\', [AExt])) then
          try
            AMIMEType := ARegistry.ReadString('Content Type');
            if Length(AMIMEType) = 0 then
              Continue;
            FRegistryExtensionToMIMEType.Add(LowerCase(AExt), AMIMEType);
          finally
            ARegistry.CloseKey;
          end;
        end;
      finally
        AExts.Free;
      end;
    end;
  finally
    ARegistry.Free;
  end;
end;

class procedure TdxCloudStorageProvider.Finalize;
begin
  FreeAndNil(FRegistryExtensionToMIMEType);
end;

constructor TdxCloudStorageProvider.Create(AStorage: TdxCloudStorage);
begin
  inherited Create(AStorage);
  FFreeNotificator := TcxFreeNotificator.Create(nil);
  FFreeNotificator.OnFreeNotification := FreeNotificationHandler;
  FTasks := TdxWebTaskManager.Create;
  FFileTransferManager := CreateFileTransferManager;

  FImageDictionary := TdxCloudStorageProviderImageDictionary.Create(Self);
  FFiles := CreateFiles;
  FRecentFileCountLimit := DefaultRecentFileCountLimit;
  FRecentDayCountLimit := DefaultRecentDayCountLimit;
end;

destructor TdxCloudStorageProvider.Destroy;
begin
  AuthorizationAgent := nil;
  FreeAndNil(FFileTransferManager);
  FreeAndNil(FTasks);
  FreeAndNil(FFiles);
  FreeAndNil(FImageDictionary);
  FreeAndNil(FFreeNotificator);
  inherited Destroy;
end;

procedure TdxCloudStorageProvider.BeforeDestruction;
begin
  inherited BeforeDestruction;
  cxClearObjectLinks(Self);
end;

function TdxCloudStorageProvider.GetExtension(AItem: TdxCloudStorageItem): string;
begin
  if AItem = nil then
    Exit('');
  Result := ExtractFileExt(AItem.Name);
end;

class procedure TdxCloudStorageProvider.Register;
begin
  TdxCloudStorage.RegisterProvider(Self);
end;

class procedure TdxCloudStorageProvider.Unregister;
begin
  TdxCloudStorage.UnregisterProvider(Self);
end;

function TdxCloudStorageProvider.CreateCopyItemToTask(AItem: TdxCloudStorageItem; AParent: TdxCloudStorageFolder): TdxCloudStorageProviderMoveItemToCustomTask;
begin
  Result := nil;
  RaiseOperationNotSupportedException;
end;

function TdxCloudStorageProvider.CreateCreateFolderTask(AParent: TdxCloudStorageFolder; const AName: string): TdxCloudStorageProviderCreateFolderTask;
begin
  Result := nil;
  RaiseOperationNotSupportedException;
end;

function TdxCloudStorageProvider.CreateDeleteItemTask(AItem: TdxCloudStorageItem): TdxCloudStorageProviderDeleteItemTask;
begin
  Result := nil;
  RaiseOperationNotSupportedException;
end;

function TdxCloudStorageProvider.CreateFetchAllTask: TdxCloudStorageProviderFetchAllCustomTask;
begin
  Result := nil;
  RaiseOperationNotSupportedException;
end;

function TdxCloudStorageProvider.CreateLoadImageTask(const AUri: string): TdxCloudStorageProviderLoadImageTask;
begin
  Result := TdxCloudStorageProviderLoadImageTask.Create(Self, AUri);
end;

function TdxCloudStorageProvider.CreateMoveItemToTask(AItem: TdxCloudStorageItem; AParent: TdxCloudStorageFolder): TdxCloudStorageProviderMoveItemToCustomTask;
begin
  Result := nil;
  RaiseOperationNotSupportedException;
end;

function TdxCloudStorageProvider.CreateMoveItemToTrashTask(AItem: TdxCloudStorageItem): TdxCloudStorageProviderMoveItemToTrashTask;
begin
  Result := nil;
  RaiseOperationNotSupportedException;
end;

function TdxCloudStorageProvider.CreateRestoreItemFromTrashTask(AItem: TdxCloudStorageItem): TdxCloudStorageProviderItemCustomTask;
begin
  Result := nil;
  RaiseOperationNotSupportedException;
end;

function TdxCloudStorageProvider.CreateUpdateFolderTask(AFolder: TdxCloudStorageFolder): TdxCloudStorageProviderUpdateFolderCustomTask;
begin
  Result := nil;
  RaiseOperationNotSupportedException;
end;

function TdxCloudStorageProvider.CreateUpdateSpecialFolderTask(AFolder: TdxCloudStorageSpecialFolder): TdxCloudStorageProviderUpdateFolderCustomTask;
begin
  Result := nil;
  RaiseOperationNotSupportedException;
end;

procedure TdxCloudStorageProvider.RunTask(ATask: TdxCloudStorageProviderCustomTask);
begin
  if Tasks.HasSameTask(ATask) then
  begin
    ATask.Free;
    Exit;
  end;
  Tasks.RunTask(ATask);
end;

class procedure TdxCloudStorageProvider.RaiseOperationNotSupportedException;
begin
  raise Exception.Create(cxGetResourceString(@sdxCloudStorageProviderDoesNotSupportThisOperationException));
end;

function TdxCloudStorageProvider.CreateItem(const AData: TdxCloudStorageItemData): TdxCloudStorageItem;
begin
  Result := CreateItem(AData.MIMEType);
  Result.Assign(AData);
end;

function TdxCloudStorageProvider.CreateRoot(const AData: TdxCloudStorageItemData): TdxCloudStorageItem;
begin
  Result := Files.CreateRoot;
  Result.Assign(AData);
end;

function TdxCloudStorageProvider.DoGetSpecialFolderData(const AType: TdxCloudStorageSpecialFolder.TType): TdxCloudStorageItemData;
begin
  Result.ID := '';
  case AType of
    TdxCloudStorageSpecialFolder.TType.Recent:
      Result.Name := cxGetResourceString(@sdxCloudStorageRecentFolderName);
    TdxCloudStorageSpecialFolder.TType.SharedWithMe:
      Result.Name := cxGetResourceString(@sdxCloudStorageSharedWithMeFolderName);
    TdxCloudStorageSpecialFolder.TType.SharedByMe:
      Result.Name := cxGetResourceString(@sdxCloudStorageSharedByMeFolderName);
    TdxCloudStorageSpecialFolder.TType.Starred:
      Result.Name := cxGetResourceString(@sdxCloudStorageStarredFolderName);
    TdxCloudStorageSpecialFolder.TType.Discover:
      Result.Name := cxGetResourceString(@sdxCloudStorageDiscoverFolderName);
    TdxCloudStorageSpecialFolder.TType.Trash:
      Result.Name := cxGetResourceString(@sdxCloudStorageTrashFolderName);
  else
    Result.Name := '';
  end;
  Result.Parents := nil;
  Result.IconLink := '';
  Result.CreatedDate := InvalidDate;
  Result.ModifiedDate := InvalidDate;
  Result.Shared := False;
  Result.Trashed := False;
end;

function TdxCloudStorageProvider.GetSpecialFolderData(const AType: TdxCloudStorageSpecialFolder.TType): TdxCloudStorageItemData;
begin
  Result := DoGetSpecialFolderData(AType);
end;

procedure TdxCloudStorageProvider.LoadImageComplete(const AUri: string; const AStream: TStream);
var
  AImage: TdxSmartImage;
begin
  AStream.Position := 0;
  AImage := TdxSmartImage.CreateFromStream(AStream);
  try
    AImage.HandleNeeded;
    FImageDictionary.AddImage(AUri, AImage);
  except
    AImage.Free;
  end;
end;

procedure TdxCloudStorageProvider.LoadImage(const AUri: string);
begin
  if IsReady then
    RunTask(CreateLoadImageTask(AUri));
end;

function TdxCloudStorageProvider.IsReady: Boolean;
begin
  Result := Storage.Connected and (AuthorizationAgent <> nil);
  if Result then
  begin
    AuthorizationAgent.ValidateAuthorization;
    Result := AuthorizationAgent.IsAuthorized;
  end;
end;

procedure TdxCloudStorageProvider.CopyItemTo(AItem: TdxCloudStorageItem; AParent: TdxCloudStorageFolder);
begin
  if IsReady then
    RunTask(CreateCopyItemToTask(AItem, AParent));
end;

procedure TdxCloudStorageProvider.CopyItemToComplete(AItem: TdxCloudStorageItem; AParent: TdxCloudStorageFolder; const AData: TdxCloudStorageItemData);
var
  ANewItem: TdxCloudStorageItem;
begin
  if AParent.IsLoaded then
  begin
    ANewItem := AParent.AddChild(AData);
    Storage.DoItemCopied(ANewItem);
  end;
end;

procedure TdxCloudStorageProvider.CreateFolder(AParent: TdxCloudStorageFolder; const AName: string);
begin
  if IsReady then
    RunTask(CreateCreateFolderTask(AParent, AName));
end;

procedure TdxCloudStorageProvider.CreateFolderComplete(const AFolderData: TdxCloudStorageItemData);
var
  AParentID: string;
  AFolder: TdxCloudStorageItem;
  AParent: TdxCloudStorageItem;
begin
  AFolder := Files.Items.FindByID(AFolderData.ID);
  if AFolder = nil then
  begin
    AFolder := CreateItem(AFolderData);
    Files.Items.Add(AFolder);
  end
  else
    AFolder.Assign(AFolderData);
  for AParentID in AFolderData.Parents do
  begin
    AParent := Files.Items.FindByID(AParentID);
    if (AParent <> nil) and AParent.IsFolder then
    begin
      AFolder.AddParent(TdxCloudStorageFolder(AParent));
      TdxCloudStorageFolder(AParent).FetchChildren;
    end;
  end;
  Storage.DoFolderCreated(TdxCloudStorageFolder(AFolder));
end;

procedure TdxCloudStorageProvider.DeleteItem(AItem: TdxCloudStorageItem);
begin
  if IsReady then
    RunTask(CreateDeleteItemTask(AItem));
end;

procedure TdxCloudStorageProvider.DeleteItemComplete(AItem: TdxCloudStorageItem);
begin
  AItem.ClearParents;
  Storage.DoItemDeleted(AItem);
  Files.Items.Extract(AItem);
  AItem.Free;
end;

procedure TdxCloudStorageProvider.FetchAll;
begin
  Tasks.CancelAllTasks;
  if IsReady then
    RunTask(CreateFetchAllTask);
end;

procedure TdxCloudStorageProvider.FetchAllComplete(AItems: TdxCloudStorageItemList);
var
  AItem: TdxCloudStorageItem;
begin
  Storage.DoTreeDataLoading(nil);
  try
    Files.Clear(False);
    while AItems.Count > 0 do
    begin
      AItem := AItems[0];
      if AItem.IsFolder then
        TdxCloudStorageCustomFolder(AItem).SetIsLoaded(True);
      AItems.Extract(AItem);
      Files.Items.Add(AItem);
    end;
    Files.SpecialFolders.Enum(procedure(const ASpecialFolder: TdxCloudStorageSpecialFolder)
      begin
        ASpecialFolder.FetchChildren(True);
      end);
  finally
    Storage.DoTreeDataLoaded(nil);
  end;
end;

procedure TdxCloudStorageProvider.MoveItemTo(AItem: TdxCloudStorageItem; AParent: TdxCloudStorageFolder);
begin
  if IsReady then
    RunTask(CreateMoveItemToTask(AItem, AParent));
end;

procedure TdxCloudStorageProvider.MoveItemToComplete(AItem: TdxCloudStorageItem; AParent: TdxCloudStorageFolder);
begin
  AItem.ClearParents;
  if AParent.IsLoaded then
    AParent.AddChild(AItem);
  Storage.DoItemMoved(AItem);
end;

procedure TdxCloudStorageProvider.MoveItemToTrash(AItem: TdxCloudStorageItem);
begin
  if IsReady then
    RunTask(CreateMoveItemToTrashTask(AItem));
end;

procedure TdxCloudStorageProvider.MoveItemToTrashComplete(AItem: TdxCloudStorageItem; const ANewData: TdxCloudStorageItemData);
var
  ATrash: TdxCloudStorageSpecialFolder;
begin
  AItem.ClearParents;
  AItem.Assign(ANewData);
  ATrash := Files.Trash;
  if (ATrash <> nil) and ATrash.IsLoaded then
    ATrash.AddChild(AItem);
  Storage.DoItemMovedToTrash(AItem);
end;

procedure TdxCloudStorageProvider.RestoreItemFromTrash(AItem: TdxCloudStorageItem);
begin
  if IsReady then
    RunTask(CreateRestoreItemFromTrashTask(AItem));
end;

procedure TdxCloudStorageProvider.RestoreItemFromTrashComplete(AItem: TdxCloudStorageItem; const ANewData: TdxCloudStorageItemData);
var
  ATrash: TdxCloudStorageSpecialFolder;
  AParentID: string;
  AParent: TdxCloudStorageItem;
begin
  if Files = nil then
    Exit;
  ATrash := Files.Trash;
  if (ATrash <> nil) and ATrash.IsLoaded then
    ATrash.RemoveChild(AItem);
  AItem.Assign(ANewData);
  for AParentID in ANewData.Parents do
  begin
    AParent := Files.FindByID(AParentID);
    if (AParent is TdxCloudStorageFolder) and TdxCloudStorageFolder(AParent).IsLoaded then
      AItem.AddParent(TdxCloudStorageFolder(AParent));
  end;
  Storage.DoItemRestoredFromTrash(AItem);
end;

procedure TdxCloudStorageProvider.UpdateFolder(AFolder: TdxCloudStorageFolder);
begin
  if IsReady then
    RunTask(CreateUpdateFolderTask(AFolder));
end;

procedure TdxCloudStorageProvider.UpdateSpecialFolder(AFolder: TdxCloudStorageSpecialFolder);
begin
  if IsReady then
    RunTask(CreateUpdateSpecialFolderTask(AFolder));
end;

procedure TdxCloudStorageProvider.UpdateRoot;
begin
  if IsReady then
    RunTask(CreateUpdateFolderTask(Files.Root));
end;

function TdxCloudStorageProvider.GetHttpHeaders: string;
begin
  if AuthorizationAgent <> nil then
    Result := AuthorizationAgent.GetAuthorizationHeader
  else
    Result := '';
end;

procedure TdxCloudStorageProvider.Changed;
begin
  Tasks.CancelAllTasks;
  Storage.DoChanged;
end;

procedure TdxCloudStorageProvider.SubscribeAuthorizationAgent;
begin
  if FAuthorizationAgent <> nil then
    cxAddFreeNotification(FFreeNotificator, FAuthorizationAgent);
end;

procedure TdxCloudStorageProvider.UnsubscribeAuthorizationAgent;
begin
  if FAuthorizationAgent <> nil then
    cxRemoveFreeNotification(FFreeNotificator, FAuthorizationAgent);
end;

function TdxCloudStorageProvider.GetStorage: TdxCloudStorage;
begin
  Result := TdxCloudStorage(Owner);
end;

procedure TdxCloudStorageProvider.FreeNotificationHandler(Sender: TComponent);
begin
  if AuthorizationAgent = Sender then
    AuthorizationAgent := nil;
end;

procedure TdxCloudStorageProvider.SetAuthorizationAgent(const Value: TdxCustomAuthorizationAgent);
begin
  if FAuthorizationAgent <> Value then
  begin
    UnsubscribeAuthorizationAgent;
    FAuthorizationAgent := Value;
    SubscribeAuthorizationAgent;
    Changed;
  end;
end;

procedure TdxCloudStorageProvider.SetRecentDayCountLimit(const Value: Integer);
begin
  if RecentDayCountLimit <> Value then
  begin
    FRecentDayCountLimit := Value;
    Files.UpdateRecentFolder;
  end;
end;

procedure TdxCloudStorageProvider.SetRecentFileCountLimit(const Value: Integer);
begin
  if RecentFileCountLimit <> Value then
  begin
    FRecentFileCountLimit := Value;
    Files.UpdateRecentFolder;
  end;
end;

{ TdxCloudStorageOAuth2Provider }

function TdxCloudStorageOAuth2Provider.GetHttpHeaders: string;
begin
  Result := TdxHttpHelper.ConcatenateHeaders([TdxHttpHelper.ContentTypeJSONHeader, inherited GetHttpHeaders]);
end;

procedure TdxCloudStorageOAuth2Provider.SubscribeAuthorizationAgent;
begin
  inherited SubscribeAuthorizationAgent;
  if AuthorizationAgent <> nil then
    AuthorizationAgent.RegisterScopeRequestor(Self);
end;

procedure TdxCloudStorageOAuth2Provider.UnsubscribeAuthorizationAgent;
begin
  if AuthorizationAgent <> nil then
    AuthorizationAgent.UnregisterScopeRequestor(Self);
  inherited UnsubscribeAuthorizationAgent;
end;

function TdxCloudStorageOAuth2Provider.GetScopes: TStringList;
begin
  Result := TStringList.Create;
end;

function TdxCloudStorageOAuth2Provider.GetAuthorizationAgent: TdxOAuth2AuthorizationAgent;
begin
  Result := TdxOAuth2AuthorizationAgent(inherited AuthorizationAgent);
end;

procedure TdxCloudStorageOAuth2Provider.SetAuthorizationAgent(const Value: TdxOAuth2AuthorizationAgent);
begin
  inherited AuthorizationAgent := Value;
end;

{ TdxCloudStorage }

class procedure TdxCloudStorage.Finalize;
begin
  FreeAndNil(FRegisteredProviders);
end;

constructor TdxCloudStorage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

end;

destructor TdxCloudStorage.Destroy;
begin
  Connected := False;
  FreeAndNil(FProvider);
  inherited Destroy;
end;

class function TdxCloudStorage.GetRegisteredProviders: TcxRegisteredClasses;
begin
  if FRegisteredProviders = nil then
    FRegisteredProviders := TcxRegisteredClasses.Create;
  Result := FRegisteredProviders;
end;

procedure TdxCloudStorage.DoChanged;
begin
  if IsDestroying then
    Exit;
  CheckConnection;
  if not ChangedHandlers.Empty then
    ChangedHandlers.Invoke(Self);
  CallNotify(FOnChanged, Self);
end;

procedure TdxCloudStorage.DoConnectedChanged;
begin
  if IsDestroying then
    Exit;
  if not ConnectedChangedHandlers.Empty then
    ConnectedChangedHandlers.Invoke(Self);
  CallNotify(FOnConnectedChanged, Self);
end;

procedure TdxCloudStorage.DoError(const AErrorObject);
begin
  if Assigned(FOnError) then
    FOnError(Self, AErrorObject);
end;

procedure TdxCloudStorage.DoFolderCreated(AFolder: TdxCloudStorageFolder);
begin
  if not FolderCreatedHandlers.Empty then
    FolderCreatedHandlers.Invoke(Self, AFolder);
  if Assigned(OnFolderCreated) then
    OnFolderCreated(Self, AFolder);
end;

procedure TdxCloudStorage.UpdateFolder(AFolder: TdxCloudStorageFolder);
begin
  if (Provider <> nil) then
    Provider.UpdateFolder(AFolder);
end;

procedure TdxCloudStorage.DoTreeDataLoading(AFolder: TdxCloudStorageCustomFolder);
begin
  if not TreeDataLoadingHandlers.Empty then
    TreeDataLoadingHandlers.Invoke(Self, AFolder);
  if Assigned(OnTreeDataLoading) then
    OnTreeDataLoading(Self, AFolder);
end;

procedure TdxCloudStorage.DoTreeDataLoaded(AFolder: TdxCloudStorageCustomFolder);
begin
  if not TreeDataLoadedHandlers.Empty then
    TreeDataLoadedHandlers.Invoke(Self, AFolder);
  if Assigned(OnTreeDataLoaded) then
    OnTreeDataLoaded(Self, AFolder);
end;

procedure TdxCloudStorage.DoImageLoaded(const AUri: string);
begin
  if not FImageLoadedHandlers.Empty then
    FImageLoadedHandlers.Invoke(Self, AUri);
end;

procedure TdxCloudStorage.DoItemCopied(ANewItem: TdxCloudStorageItem);
begin
  if not FItemCopiedHandlers.Empty then
    FItemCopiedHandlers.Invoke(Self, ANewItem);
  if Assigned(FOnItemCopied) then
    FOnItemCopied(Self, ANewItem);
end;

procedure TdxCloudStorage.DoItemDeleted(AItem: TdxCloudStorageItem);
begin
  if not FItemDeletedHandlers.Empty then
    FItemDeletedHandlers.Invoke(Self, AItem);
  if Assigned(FOnItemDeleted) then
    FOnItemDeleted(Self, AItem);
end;

procedure TdxCloudStorage.DoItemMoved(AItem: TdxCloudStorageItem);
begin
  if not FItemMovedHandlers.Empty then
    FItemMovedHandlers.Invoke(Self, AItem);
  if Assigned(FOnItemMoved) then
    FOnItemMoved(Self, AItem);
end;

procedure TdxCloudStorage.DoItemMovedToTrash(AItem: TdxCloudStorageItem);
begin
  if not FItemMovedToTrashHandlers.Empty then
    FItemMovedToTrashHandlers.Invoke(Self, AItem);
  if Assigned(FOnItemMovedToTrash) then
    FOnItemMovedToTrash(Self, AItem);
end;

procedure TdxCloudStorage.DoItemRestoredFromTrash(AItem: TdxCloudStorageItem);
begin
  if not FItemRestoredFromTrashHandlers.Empty then
    FItemRestoredFromTrashHandlers.Invoke(Self, AItem);
  if Assigned(FOnItemRestoredFromTrash) then
    FOnItemRestoredFromTrash(Self, AItem);
end;

procedure TdxCloudStorage.DoItemDownload(const AItem: TdxCloudStorageItem; const APosition: Integer);
begin
  if Assigned(FOnItemDownload) then
    FOnItemDownload(Self, AItem, APosition);
end;

procedure TdxCloudStorage.DoItemDownloaded(const AItem: TdxCloudStorageItem; AStream: TStream);
begin
  if Assigned(FOnItemDownloaded) then
    OnItemDownloaded(Self, AItem, AStream);
end;

procedure TdxCloudStorage.DoItemDownloading(const AItem: TdxCloudStorageItem; const ASize: Integer);
begin
  if Assigned(FOnItemDownloading) then
    OnItemDownloading(Self, AItem, ASize);
end;

procedure TdxCloudStorage.DoItemUploaded(const AItem: TdxCloudStorageItem);
begin
  if Assigned(FOnItemUploaded) then
    FOnItemUploaded(Self, AItem);
end;

procedure TdxCloudStorage.DoItemUploading(const AFileName: string; const ASize: Integer);
begin
  if Assigned(FOnItemUploading) then
    FOnItemUploading(Self, AFileName, ASize);
end;

procedure TdxCloudStorage.DoItemUpload(const AFileName: string; const APosition: Cardinal);
begin
  if Assigned(FOnItemUpload) then
    FOnItemUpload(Self, AFileName, APosition);
end;

procedure TdxCloudStorage.DoFileContentUploaded(const AFile: TdxCloudStorageFile);
begin
  if Assigned(FOnFileContentUploaded) then
    FOnFileContentUploaded(Self, AFile);
end;

procedure TdxCloudStorage.DoFileContentUploading(const AFile: TdxCloudStorageFile; const ASize: Integer);
begin
  if Assigned(FOnFileContentUploading) then
    FOnFileContentUploading(Self, AFile, ASize);
end;

procedure TdxCloudStorage.DoFileContentUpload(const AFile: TdxCloudStorageFile; const APosition: Integer);
begin
  if Assigned(FOnFileContentUpload) then
    FOnFileContentUpload(Self, AFile, APosition);
end;

class procedure TdxCloudStorage.RegisterProvider(
  AProvider: TdxCloudStorageProviderClass);
begin
  RegisteredProviders.Register(AProvider, AProvider.GetDisplayName);
end;

class procedure TdxCloudStorage.UnregisterProvider(
  AProvider: TdxCloudStorageProviderClass);
begin
  if FRegisteredProviders <> nil then
    FRegisteredProviders.Unregister(AProvider);
end;

function TdxCloudStorage.IsDestroying: Boolean;
begin
  Result := csDestroying in ComponentState;
end;

function TdxCloudStorage.CanConnection: Boolean;
begin
  Result := (Provider <> nil) and (Provider.AuthorizationAgent <> nil);
  if Result then
  begin
    if not Provider.AuthorizationAgent.IsAuthorized then
      Provider.AuthorizationAgent.StartAuthorization
    else
      Provider.AuthorizationAgent.ValidateAuthorization;
    Result := Provider.AuthorizationAgent.IsAuthorized;
  end;
end;

procedure TdxCloudStorage.CheckConnection;
begin
  Connected := Connected and CanConnection;
end;

function TdxCloudStorage.GetFiles: TdxCloudStorageFiles;
begin
  if Provider <> nil then
    Result := Provider.Files
  else
    Result := nil;
end;

function TdxCloudStorage.GetProviderClassName: string;
begin
  if FProvider <> nil then
    Result := FProvider.ClassName
  else
    Result := '';
end;

procedure TdxCloudStorage.RecreateProvider;
begin
  Connected := False;
  FreeAndNil(FProvider);
  if FProviderClass <> nil then
    FProvider := FProviderClass.Create(Self);
  DoChanged;
end;

procedure TdxCloudStorage.SetConnected(const Value: Boolean);
begin
  if Connected <> Value then
  begin
    if Value and not CanConnection then
      Exit;
    FConnected := Value;
    DoConnectedChanged;
  end;
end;

procedure TdxCloudStorage.SetProvider(const Value: TdxCloudStorageProvider);
begin
  if Provider = Value then
    Exit;
  if Value = nil then
    ProviderClass := nil
  else
  begin
    if FProvider = nil then
      ProviderClass := TdxCloudStorageProviderClass(Value.ClassType);
    FProvider.Assign(Value);
  end;
end;

procedure TdxCloudStorage.SetProviderClass(const Value: TdxCloudStorageProviderClass);
begin
  if FProviderClass <> Value then
  begin
    FProviderClass := Value;
    RecreateProvider;
  end;
end;

procedure TdxCloudStorage.SetProviderClassName(const Value: string);
begin
  if ProviderClassName <> Value then
    ProviderClass := TdxCloudStorageProviderClass(TdxCloudStorage.RegisteredProviders.FindByClassName(Value));
end;

initialization
  TdxCloudStorageProvider.Initialize;

finalization
  TdxCloudStorageProvider.Finalize;
  TdxCloudStorage.Finalize;

end.
