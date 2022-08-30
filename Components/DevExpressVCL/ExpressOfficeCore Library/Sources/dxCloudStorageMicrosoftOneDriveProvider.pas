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

unit dxCloudStorageMicrosoftOneDriveProvider;

{$I cxVer.inc}
{$SCOPEDENUMS ON}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, SysUtils, Classes, Forms, Windows,
  Generics.Defaults, Generics.Collections,
  dxWinInet, dxCore, dxCoreClasses, cxClasses, dxThreading,
  dxCloudStorage, dxAuthorizationAgents;

type
  TdxCloudStorageMicrosoftOneDriveProvider = class;
  TdxCloudStorageMicrosoftOneDriveFileTransferManager = class;

  { TdxCloudStorageMicrosoftOneDriveProviderFetchAllTask }

  TdxCloudStorageMicrosoftOneDriveProviderFetchAllTask = class(TdxCloudStorageProviderFetchAllCustomTask)
  strict private
    function GetProvider: TdxCloudStorageMicrosoftOneDriveProvider; inline;
  protected
    function AssignRoot(var ARoot: TdxCloudStorageItemData; const ACancelStatus: TdxTaskCancelCallback): TdxTaskCompletedStatus; override;
    function PopulateItems(const ACancelStatus: TdxTaskCancelCallback): TdxTaskCompletedStatus; override;
    property Provider: TdxCloudStorageMicrosoftOneDriveProvider read GetProvider;
  end;

  { TdxCloudStorageMicrosoftOneDriveProviderMoveItemToCustomTask }

  TdxCloudStorageMicrosoftOneDriveProviderMoveItemToCustomTask = class abstract(TdxCloudStorageProviderMoveItemToCustomTask)
  protected
    function GetHeader: string; override;
    function GetResultObject(const ACancelStatus: TdxTaskCancelCallback; out AResult: TdxJSONObject): TdxTaskCompletedStatus; virtual; abstract;
    function DoRun(const ACancelStatus: TdxTaskCancelCallback): TdxTaskCompletedStatus; override; final;
  end;

  { TdxCloudStorageMicrosoftOneDriveProviderCopyItemToTask }

  TdxCloudStorageMicrosoftOneDriveProviderCopyItemToTask = class(TdxCloudStorageMicrosoftOneDriveProviderMoveItemToCustomTask)
  strict private
    function GetRequestObject: TdxJSONObject;
  protected
    procedure DoComplete; override;
    function GetHeader: string; override;
    function GetResultObject(const ACancelStatus: TdxTaskCancelCallback; out AResult: TdxJSONObject): TdxTaskCompletedStatus; override;
  end;

  { TdxCloudStorageMicrosoftOneDriveProviderMoveItemToTask }

  TdxCloudStorageMicrosoftOneDriveProviderMoveItemToTask = class(TdxCloudStorageMicrosoftOneDriveProviderMoveItemToCustomTask)
  protected
    procedure DoComplete; override;
    function GetResultObject(const ACancelStatus: TdxTaskCancelCallback; out AResult: TdxJSONObject): TdxTaskCompletedStatus; override;
  end;

  { TdxCloudStorageMicrosoftOneDriveProviderMoveItemToTrashTask }

  TdxCloudStorageMicrosoftOneDriveProviderMoveItemToTrashTask = class(TdxCloudStorageProviderMoveItemToTrashTask)
  protected
    function DoRun(const ACancelStatus: TdxTaskCancelCallback): TdxTaskCompletedStatus; override;
  end;

  { TdxCloudStorageMicrosoftOneDriveProviderCreateFolderTask }

  TdxCloudStorageMicrosoftOneDriveProviderCreateFolderTask = class(TdxCloudStorageProviderCreateFolderTask)
  protected
    function GetRequestObject: TdxJSONObject; override;
    function PostRequest(AObject: TdxJSONObject): TdxJSONObject; override;
    function IsError(AObject: TdxJSONObject): Boolean; override;
  end;

  { TdxCloudStorageMicrosoftOneDriveProviderUpdateFolderTask }

  TdxCloudStorageMicrosoftOneDriveProviderUpdateFolderTask = class(TdxCloudStorageProviderUpdateFolderCustomTask)
  strict private
    function GetProvider: TdxCloudStorageMicrosoftOneDriveProvider; inline;
  protected
    function AssignData(const ACancelStatus: TdxTaskCancelCallback; var AData: TdxCloudStorageItemData): TdxTaskCompletedStatus; override;
    function PopulateItems(const ACancelStatus: TdxTaskCancelCallback): TdxTaskCompletedStatus; override;
    function GetUri: string; virtual;

    property Provider: TdxCloudStorageMicrosoftOneDriveProvider read GetProvider;
  end;

  { TdxCloudStorageMicrosoftOneDriveProviderUpdateSpecialFolderTask }

  TdxCloudStorageMicrosoftOneDriveProviderUpdateSpecialFolderTask = class(TdxCloudStorageMicrosoftOneDriveProviderUpdateFolderTask)
  strict private
    function GetFolder: TdxCloudStorageSpecialFolder; inline;
  protected
    function AssignData(const ACancelStatus: TdxTaskCancelCallback; var AData: TdxCloudStorageItemData): TdxTaskCompletedStatus; override;
    function GetUri: string; override;
  public
    property Folder: TdxCloudStorageSpecialFolder read GetFolder;
  end;

  { TdxCloudStorageMicrosoftOneDriveUploadTask }

  TdxCloudStorageMicrosoftOneDriveUploadTask = class abstract(TdxCloudStorageUploadTask)
  strict private
    function GetProvider: TdxCloudStorageMicrosoftOneDriveProvider; inline;
  protected
    function DoRun(const ACancelStatus: TdxTaskCancelCallback): TdxTaskCompletedStatus; override;
    function GetHeader: string; override;
    function GetServerName: string; override;
    function GetObjectName: string; override;
    function GetVerb: string; override;
    function IsSuccess(ARequest: TdxInternet): Boolean; override;

    function GetMIMEType: string; virtual; abstract;

    procedure CloseUploadSession(const AServer, AObjectName: string);
    function OpenUploadSession: string;

    function DoUploadLargeFile(const ACancelStatus: TdxTaskCancelCallback): TdxTaskCompletedStatus;
    function GetCreateUploadSessionUri: string; virtual; abstract;
    function GetUploadSessionRequestObject: TdxJSONObject; virtual; abstract;

    property Provider: TdxCloudStorageMicrosoftOneDriveProvider read GetProvider;
  end;

  { TdxCloudStorageMicrosoftOneDriveUpdateFileStreamTask }

  TdxCloudStorageMicrosoftOneDriveUpdateFileStreamTask = class(TdxCloudStorageMicrosoftOneDriveUploadTask)
  strict private
    FFile: TdxCloudStorageFile;
    FFileLink: TcxObjectLink;
  protected
    function GetCreateUploadSessionUri: string; override;
    function GetMIMEType: string; override;
    function GetUploadSessionRequestObject: TdxJSONObject; override;
    function IsValid: Boolean; override;
  public
    constructor Create(AProvider: TdxCloudStorageMicrosoftOneDriveProvider;
      AFile: TdxCloudStorageFile; AStream: TStream); reintroduce;
    destructor Destroy; override;
  end;

  { TdxCloudStorageMicrosoftOneDriveUploadFileTask }

  TdxCloudStorageMicrosoftOneDriveUploadFileTask = class(TdxCloudStorageMicrosoftOneDriveUploadTask)
  strict private
    FFileName: string;
    FParent: TdxCloudStorageFolder;
    FParentLink: TcxObjectLink;
  protected
    procedure DoComplete; override;
    function GetCreateUploadSessionUri: string; override;
    function GetMIMEType: string; override;
    function GetUploadSessionRequestObject: TdxJSONObject; override;
    function IsValid: Boolean; override;
  public
    constructor Create(AProvider: TdxCloudStorageMicrosoftOneDriveProvider;
      AParent: TdxCloudStorageFolder; const AFileName: string); reintroduce;
    destructor Destroy; override;
  end;

  { TdxCloudStorageMicrosoftOneDriveFileTransferManager }

  TdxCloudStorageMicrosoftOneDriveFileTransferManager = class(TdxCloudStorageFileTransferManager)
  strict private
    function GetProvider: TdxCloudStorageMicrosoftOneDriveProvider; inline;
  protected
    function CreateUpdateFileStreamTask(AFile: TdxCloudStorageFile; AStream: TStream): TdxCloudStorageUploadTask; override;
    function CreateUploadFileTask(AParent: TdxCloudStorageFolder; const AFileName: string): TdxCloudStorageUploadTask; override;

    property Provider: TdxCloudStorageMicrosoftOneDriveProvider read GetProvider;
  end;

  { TdxCloudStorageMicrosoftOneDriveFiles }

  TdxCloudStorageMicrosoftOneDriveFiles = class(TdxCloudStorageFiles)
  protected
    procedure PopulateSpecialFolders; override;
  end;

  { TdxCloudStorageMicrosoftOneDriveProvider }

  TdxCloudStorageMicrosoftOneDriveProvider = class(TdxCloudStorageOAuth2Provider)
  protected const
    FFolderMIMEType = 'folder';
    FServerName = 'graph.microsoft.com';
  strict private
    function GetAuthorizationAgent: TdxMicrosoftGraphAPIOAuth2AuthorizationAgent; inline;
    function GetFiles: TdxCloudStorageMicrosoftOneDriveFiles; inline;
    procedure SetAuthorizationAgent(const Value: TdxMicrosoftGraphAPIOAuth2AuthorizationAgent); inline;
  protected
    function CreateFiles: TdxCloudStorageFiles; override;
    function CreateFileTransferManager: TdxCloudStorageFileTransferManager; override;
    function CreateItem(const AMIMEType: string): TdxCloudStorageItem; override;
    function GetScopes: TStringList; override;
    function DoGetSpecialFolderData(const AType: TdxCloudStorageSpecialFolder.TType): TdxCloudStorageItemData; override;
    function ObjectToData(const AObject: TObject): TdxCloudStorageItemData; override;

    function CreateCopyItemToTask(AItem: TdxCloudStorageItem; AParent: TdxCloudStorageFolder): TdxCloudStorageProviderMoveItemToCustomTask; override;
    function CreateCreateFolderTask(AParent: TdxCloudStorageFolder; const AName: string): TdxCloudStorageProviderCreateFolderTask; override;
    function CreateFetchAllTask: TdxCloudStorageProviderFetchAllCustomTask; override;
    function CreateMoveItemToTask(AItem: TdxCloudStorageItem; AParent: TdxCloudStorageFolder): TdxCloudStorageProviderMoveItemToCustomTask; override;
    function CreateMoveItemToTrashTask(AItem: TdxCloudStorageItem): TdxCloudStorageProviderMoveItemToTrashTask; override;
    function CreateUpdateFolderTask(AFolder: TdxCloudStorageFolder): TdxCloudStorageProviderUpdateFolderCustomTask; override;
    function CreateUpdateSpecialFolderTask(AFolder: TdxCloudStorageSpecialFolder): TdxCloudStorageProviderUpdateFolderCustomTask; override;
    property Files: TdxCloudStorageMicrosoftOneDriveFiles read GetFiles;
  public
    class function GetDisplayName: string; override;
  published
    property AuthorizationAgent: TdxMicrosoftGraphAPIOAuth2AuthorizationAgent read GetAuthorizationAgent write SetAuthorizationAgent;
  end;

implementation

uses
  Math, WinInet,
  dxUriRecord,
  dxCloudStorageStrs;

type
  TdxHttpHelperAccess = class(TdxHttpHelper);

{ TdxCloudStorageMicrosoftOneDriveProviderFetchAllTask }

function TdxCloudStorageMicrosoftOneDriveProviderFetchAllTask.AssignRoot(var ARoot: TdxCloudStorageItemData; const ACancelStatus: TdxTaskCancelCallback): TdxTaskCompletedStatus;
var
  AObject: TdxJSONObject;
  AUri: string;
begin
  if ACancelStatus then
    Exit(TdxTaskCompletedStatus.Cancelled);
  if not IsValid then
    Exit(TdxTaskCompletedStatus.Fail);
  AUri := 'https://graph.microsoft.com/v1.0/me/drive/root';
  UpdateRequestParams;
  AObject := TdxHttpHelper.GetRequest(UserAgent, AUri, Header);
  try
    if (AObject = nil) or not AObject.HasParam('id') or (AObject.HasParam('parentReference') and AObject.HasChildParam('parentReference', 'parentId') ) then
    begin
      if AObject <> nil then
        DoError(AObject);
      Exit(TdxTaskCompletedStatus.Fail);
    end;
    ARoot := Provider.ObjectToData(AObject);
  finally
    AObject.Free;
  end;
  Result := TdxTaskCompletedStatus.Success;
end;

function TdxCloudStorageMicrosoftOneDriveProviderFetchAllTask.PopulateItems(const ACancelStatus: TdxTaskCancelCallback): TdxTaskCompletedStatus;
var
  ANextLink: string;
  AUri: string;
  AObject: TdxJSONObject;
  AFile: TdxJSONObject;
  AFiles: TdxJSONArray;
  I: Integer;
  AData: TdxCloudStorageItemData;
begin
  ANextLink := '';
  while True do
  begin
    if ACancelStatus then
      Exit(TdxTaskCompletedStatus.Cancelled);
    if not IsValid then
      Exit(TdxTaskCompletedStatus.Fail);
    if ANextLink = '' then
      AUri := 'https://graph.microsoft.com/v1.0/me/drive/root/search(q='''')'
    else
      AUri := ANextLink;
    UpdateRequestParams;
    AObject := TdxHttpHelper.GetRequest(UserAgent, AUri, Header);
    try
      if (AObject = nil) or not AObject.HasParam('value') or (AObject.GetParamValue('@odata.context') <> 'https://graph.microsoft.com/v1.0/$metadata#Collection(driveItem)') then
      begin
        if AObject <> nil then
          DoError(AObject);
        Exit(TdxTaskCompletedStatus.Fail);
      end;
      if ACancelStatus then
        Exit(TdxTaskCompletedStatus.Cancelled);
      if not IsValid then
        Exit(TdxTaskCompletedStatus.Fail);
      ANextLink := AObject.GetParamValue('@odata.nextLink');
      AFiles := TdxJSONArray(AObject.GetValue('value'));
      for I := 0 to AFiles.Count - 1 do
      begin
        AFile := TdxJSONObject(AFiles.Items[I]);
        AData := Provider.ObjectToData(AFile);
        Items.Add(AData);
      end;
    finally
      AObject.Free;
    end;
    if ANextLink = '' then
      Break;
  end;

  Result := TdxTaskCompletedStatus.Success;
end;

function TdxCloudStorageMicrosoftOneDriveProviderFetchAllTask.GetProvider: TdxCloudStorageMicrosoftOneDriveProvider;
begin
  Result := TdxCloudStorageMicrosoftOneDriveProvider(inherited Provider);
end;

{ TdxCloudStorageMicrosoftOneDriveProviderMoveItemToCustomTask }

function TdxCloudStorageMicrosoftOneDriveProviderMoveItemToCustomTask.DoRun(const ACancelStatus: TdxTaskCancelCallback): TdxTaskCompletedStatus;
var
  AResult: TdxJSONObject;
begin
  Result := GetResultObject(ACancelStatus, AResult);
  try
    if Result = TdxTaskCompletedStatus.Success then
    begin
      Result := TdxTaskCompletedStatus.Fail;
      if AResult = nil then
        Exit;
      if AResult.HasParam('id') then
      begin
        AssignData(AResult);
        Result := TdxTaskCompletedStatus.Success;
      end
      else
        DoError(AResult);
    end;
  finally
    AResult.Free;
  end;
end;

function TdxCloudStorageMicrosoftOneDriveProviderMoveItemToCustomTask.GetHeader: string;
begin
  Result := TdxHttpHelper.ConcatenateHeaders([inherited GetHeader, TdxHttpHelper.ContentTypeJSONHeader]);
end;

{ TdxCloudStorageMicrosoftOneDriveProviderCopyItemToTask }

procedure TdxCloudStorageMicrosoftOneDriveProviderCopyItemToTask.DoComplete;
begin
  TdxCloudStorageMicrosoftOneDriveProvider(Provider).CopyItemToComplete(Item, Parent, Data);
end;

function TdxCloudStorageMicrosoftOneDriveProviderCopyItemToTask.GetHeader: string;
begin
  Result := TdxHttpHelper.ConcatenateHeaders([inherited GetHeader, 'Prefer: respond-async']);
end;

function TdxCloudStorageMicrosoftOneDriveProviderCopyItemToTask.GetResultObject(const ACancelStatus: TdxTaskCancelCallback;
  out AResult: TdxJSONObject): TdxTaskCompletedStatus;
var
  ARequestObject: TdxJSONObject;
  AUri: string;
  AParams: TBytes;
  AInet, AConnect, ARequest: TdxInternet;
  ALocation: string;
  AObject: TdxJSONObject;
begin
  Result := TdxTaskCompletedStatus.Fail;
  AResult := nil;
  UpdateRequestParams;
  ARequestObject := GetRequestObject;
  try
    if Item.DriveID <> '' then
      AUri := Format('v1.0/drives/%s/items/%s/copy', [Item.DriveID, Item.ID])
    else
      AUri := Format('v1.0/me/drive/items/%s/copy', [Item.ID]);
    AParams := TdxHttpHelper.JSONObjectToBytes(ARequestObject);

    AInet := TdxHttpHelper.InternetOpen(UserAgent);
    if Assigned(AInet) then
    try
      AConnect := TdxHttpHelper.InternetConnect(AInet, PChar(TdxCloudStorageMicrosoftOneDriveProvider.FServerName));
      if Assigned(AConnect) then
      try
        ARequest := TdxHttpHelper.OpenRequest(AConnect, 'POST', AUri);
        if Assigned(ARequest) then
        try
          if not HTTPSendRequest(ARequest, PChar(Header), Length(Header), @AParams[0], Length(AParams)) then
            Exit;
          if ACancelStatus then
            Exit(TdxTaskCompletedStatus.Cancelled);
          ALocation := TdxHttpHelper.GetHttpResponseCustomHeader(ARequest, 'Location');
          if ALocation = '' then
            Exit;
          while True do
          begin
            AObject := TdxHttpHelper.GetRequest(UserAgent, ALocation, '');
            try
              if ACancelStatus then
                Exit(TdxTaskCompletedStatus.Cancelled);
              if AObject = nil then
                Break;
              if not AObject.HasParam('status') then
              begin
                DoError(AObject);
                Exit;
              end;
              if AObject.HasParam('resourceId') then
              begin
                AResult := TdxHttpHelper.GetRequest(UserAgent,
                  Format('https://graph.microsoft.com/v1.0/me/drive/items/%s', [AObject.GetParamValue('resourceId')]), Header);
                Result := TdxTaskCompletedStatus.Success;
                Break;
              end;
            finally
              AObject.Free;
            end;
          end;
        finally
          InternetCloseHandle(ARequest);
        end;
      finally
        InternetCloseHandle(AConnect);
      end;
    finally
      InternetCloseHandle(AInet);
    end;
  finally
    ARequestObject.Free;
  end;
end;

function TdxCloudStorageMicrosoftOneDriveProviderCopyItemToTask.GetRequestObject: TdxJSONObject;
var
  AParentReference: TdxJSONObject;
begin
  Result := TdxJSONObject.Create;;
  AParentReference := TdxJSONObject.Create;
  AParentReference.AddPair('id', Parent.ID);
  if Parent.DriveID <> '' then
    AParentReference.AddPair('driveId', Parent.DriveID);
  Result.AddPair('parentReference', AParentReference);
end;

{ TdxCloudStorageMicrosoftOneDriveProviderMoveItemToTask }

procedure TdxCloudStorageMicrosoftOneDriveProviderMoveItemToTask.DoComplete;
begin
  TdxCloudStorageMicrosoftOneDriveProvider(Provider).MoveItemToComplete(Item, Parent);
end;

function TdxCloudStorageMicrosoftOneDriveProviderMoveItemToTask.GetResultObject(const ACancelStatus: TdxTaskCancelCallback; out AResult: TdxJSONObject): TdxTaskCompletedStatus;
var
  ARequest: TdxJSONObject;
begin
  ARequest := TdxJSONObject.Create;
  try
    ARequest.AddPair('parentReference', TdxJSONObject.Create(TdxJSONPair.Create('id', Parent.ID)));
    UpdateRequestParams;
    AResult := TdxHttpHelper.PatchRequest(UserAgent, TdxCloudStorageMicrosoftOneDriveProvider.FServerName,
      Format('v1.0/me/drives/%s/items/%s', [Item.DriveID, Item.ID]), Header, ARequest);
    Result := TdxTaskCompletedStatus.Success;
  finally
    ARequest.Free;
  end;
end;

{ TdxCloudStorageMicrosoftOneDriveProviderDeleteItemTask }

function TdxCloudStorageMicrosoftOneDriveProviderMoveItemToTrashTask.DoRun(
  const ACancelStatus: TdxTaskCancelCallback): TdxTaskCompletedStatus;
var
  AResult: TdxJSONObject;
begin
  Result := TdxTaskCompletedStatus.Fail;
  UpdateRequestParams;
  AResult := TdxHttpHelper.GetRequest(UserAgent, TdxCloudStorageMicrosoftOneDriveProvider.FServerName,
    Format('v1.0/me/drive/items/%s', [Item.ID]), Header, nil);
  try
    if AResult = nil then
    begin
      DoError(AResult);
      Exit;
    end;
    AssignData(AResult);
    FData.Trashed := True;
  finally
    AResult.Free;
  end;

  UpdateRequestParams;
  AResult := TdxHttpHelper.DeleteRequest(UserAgent, TdxCloudStorageMicrosoftOneDriveProvider.FServerName,
    Format('v1.0/me/drive/items/%s', [Item.ID]), Header, nil);
  try
    if AResult <> nil then
      DoError(AResult)
    else
      Result := TdxTaskCompletedStatus.Success;
  finally
    AResult.Free;
  end;
end;

{ TdxCloudStorageMicrosoftOneDriveProviderCreateFolderTask }

function TdxCloudStorageMicrosoftOneDriveProviderCreateFolderTask.GetRequestObject: TdxJSONObject;
begin
  Result := TdxJSONObject.Create;
  Result.AddPair('name', Name);
  Result.AddPair('folder', TdxJSONObject.Create);
  Result.AddPair('@microsoft.graph.conflictBehavior', 'rename');
end;

function TdxCloudStorageMicrosoftOneDriveProviderCreateFolderTask.PostRequest(AObject: TdxJSONObject): TdxJSONObject;
begin
  Result := TdxHttpHelper.PostRequest(UserAgent, TdxCloudStorageMicrosoftOneDriveProvider.FServerName,
    Format('v1.0/me/drive/items/%s/children', [ParentID]), Header, AObject);
end;

function TdxCloudStorageMicrosoftOneDriveProviderCreateFolderTask.IsError(AObject: TdxJSONObject): Boolean;
begin
  Result := not AObject.HasParam('id') or not AObject.HasParam('folder');
end;

{ TdxCloudStorageMicrosoftOneDriveProviderUpdateFolderTask }

function TdxCloudStorageMicrosoftOneDriveProviderUpdateFolderTask.AssignData(const ACancelStatus: TdxTaskCancelCallback; var AData: TdxCloudStorageItemData): TdxTaskCompletedStatus;
var
  AObject: TdxJSONObject;
  AUri: string;
begin
  if ACancelStatus then
    Exit(TdxTaskCompletedStatus.Cancelled);
  if not IsValid then
    Exit(TdxTaskCompletedStatus.Fail);

  if Folder.IsRoot then
    AUri := 'https://graph.microsoft.com/v1.0/me/drive/root'
  else
    if Folder.DriveID <> '' then
      AUri := Format('https://graph.microsoft.com/v1.0/drives/%s/items/%s', [Folder.DriveID, Folder.ID])
    else
      AUri := Format('https://graph.microsoft.com/v1.0/me/drive/items/%s', [Folder.ID]);

  UpdateRequestParams;
  AObject := TdxHttpHelper.GetRequest(UserAgent, AUri, Header);
  try
    if (AObject = nil) or not AObject.HasParam('id') then
    begin
      if AObject <> nil then
        DoError(AObject);
      Exit(TdxTaskCompletedStatus.Fail);
    end;
    AData := Provider.ObjectToData(AObject);
  finally
    AObject.Free;
  end;
  Result := TdxTaskCompletedStatus.Success;
end;

function TdxCloudStorageMicrosoftOneDriveProviderUpdateFolderTask.PopulateItems(const ACancelStatus: TdxTaskCancelCallback): TdxTaskCompletedStatus;
var
  ANextLink: string;
  AUri: string;
  AObject: TdxJSONObject;
  AFile: TdxJSONObject;
  AFiles: TdxJSONArray;
  I: Integer;
  AData: TdxCloudStorageItemData;
begin
  ANextLink := '';
  while True do
  begin
    if ACancelStatus then
      Exit(TdxTaskCompletedStatus.Cancelled);
    if not IsValid then
      Exit(TdxTaskCompletedStatus.Fail);
    if ANextLink = '' then
      AUri := GetUri
    else
      AUri := ANextLink;
    UpdateRequestParams;
    AObject := TdxHttpHelper.GetRequest(UserAgent, AUri, Header);
    try
      if (AObject = nil) or not AObject.HasParam('value') then
      begin
        if AObject <> nil then
          DoError(AObject);
        Exit(TdxTaskCompletedStatus.Fail);
      end;
      if ACancelStatus then
        Exit(TdxTaskCompletedStatus.Cancelled);
      if not IsValid then
        Exit(TdxTaskCompletedStatus.Fail);
      ANextLink := AObject.GetParamValue('@odata.nextLink');
      AFiles := TdxJSONArray(AObject.GetValue('value'));
      for I := 0 to AFiles.Count - 1 do
      begin
        AFile := TdxJSONObject(AFiles.Items[I]);
        AData := Provider.ObjectToData(AFile);
        Children.Add(AData);
      end;
    finally
      AObject.Free;
    end;
    if ANextLink = '' then
      Break;
  end;

  Result := TdxTaskCompletedStatus.Success;
end;

function TdxCloudStorageMicrosoftOneDriveProviderUpdateFolderTask.GetProvider: TdxCloudStorageMicrosoftOneDriveProvider;
begin
  Result := TdxCloudStorageMicrosoftOneDriveProvider(inherited Provider);
end;

function TdxCloudStorageMicrosoftOneDriveProviderUpdateFolderTask.GetUri: string;
begin
  if Folder.IsRoot then
    Result := 'https://graph.microsoft.com/v1.0/me/drive/root/children'
  else
    if Folder.DriveID <> '' then
      Result := Format('https://graph.microsoft.com/v1.0/drives/%s/items/%s/children', [Folder.DriveID, Folder.ID])
    else
      Result := Format('https://graph.microsoft.com/v1.0/me/drive/items/%s/children', [Folder.ID]);
end;

{ TdxCloudStorageMicrosoftOneDriveProviderUpdateSpecialFolderTask }

function TdxCloudStorageMicrosoftOneDriveProviderUpdateSpecialFolderTask.AssignData(const ACancelStatus: TdxTaskCancelCallback; var AData: TdxCloudStorageItemData): TdxTaskCompletedStatus;
begin
  Result := TdxTaskCompletedStatus.Success;
  AData := Provider.GetSpecialFolderData(Folder.&Type);
end;

function TdxCloudStorageMicrosoftOneDriveProviderUpdateSpecialFolderTask.GetUri: string;
begin
  case Folder.&Type of
    TdxCloudStorageSpecialFolder.TType.Recent:
      Result := 'https://graph.microsoft.com/v1.0/me/drive/recent';
    TdxCloudStorageSpecialFolder.TType.SharedWithMe:
      Result := 'https://graph.microsoft.com/v1.0/me/drive/sharedWithMe';
  else
    Result := '';
  end;
end;

function TdxCloudStorageMicrosoftOneDriveProviderUpdateSpecialFolderTask.GetFolder: TdxCloudStorageSpecialFolder;
begin
  Result := TdxCloudStorageSpecialFolder(inherited Folder);
end;

{ TdxCloudStorageMicrosoftOneDriveUploadTask }

function TdxCloudStorageMicrosoftOneDriveUploadTask.DoRun(const ACancelStatus: TdxTaskCancelCallback): TdxTaskCompletedStatus;
begin
  if Stream.Size < 4 * 1024 * 1024 then
    Result := inherited DoRun(ACancelStatus)
  else
    Result := DoUploadLargeFile(ACancelStatus);
end;

function TdxCloudStorageMicrosoftOneDriveUploadTask.GetHeader: string;
begin
  Result := TdxHttpHelper.ConcatenateHeaders([inherited GetHeader,
    Format('Content-Type: %s', [GetMIMEType]),
    Format('Content-Length: %d', [Stream.Size])]);
end;

function TdxCloudStorageMicrosoftOneDriveUploadTask.GetServerName: string;
begin
  Result := TdxCloudStorageMicrosoftOneDriveProvider.FServerName;
end;

function TdxCloudStorageMicrosoftOneDriveUploadTask.GetObjectName: string;
begin
  Result := Uri;
end;

function TdxCloudStorageMicrosoftOneDriveUploadTask.GetVerb: string;
begin
  Result := 'PUT';
end;

function TdxCloudStorageMicrosoftOneDriveUploadTask.IsSuccess(ARequest: TdxInternet): Boolean;
var
  AObject: TdxJSONObject;
begin
  AObject := TdxHttpHelperAccess(TdxHttpHelper).GetJSONObject(ARequest);
  try
    Result := (AObject <> nil) and AObject.HasParam('id');
    if not Result then
      DoError(AObject)
    else
      AssignData(AObject);
  finally
    AObject.Free;
  end;
end;

procedure TdxCloudStorageMicrosoftOneDriveUploadTask.CloseUploadSession(const AServer, AObjectName: string);
var
  AResult: TdxJSONObject;
begin
  AResult := TdxHttpHelper.DeleteRequest(UserAgent, AServer, AObjectName, '', nil);
{$IFDEF CXTEXT}
  Assert(AResult = nil);
{$ENDIF}
  AResult.Free;
end;

function TdxCloudStorageMicrosoftOneDriveUploadTask.OpenUploadSession: string;
var
  AHeader: string;
  AUri: string;
  ARequestObject, AResult: TdxJSONObject;
begin
  AUri := GetCreateUploadSessionUri;
  ARequestObject := GetUploadSessionRequestObject;
  try
    UpdateRequestParams;
    AHeader := TdxHttpHelper.ConcatenateHeaders([inherited GetHeader, TdxHttpHelper.ContentTypeJSONHeader]);
    AResult := TdxHttpHelper.PostRequest(UserAgent, TdxCloudStorageMicrosoftOneDriveProvider.FServerName,
      AUri, AHeader, ARequestObject);
    try
      Result := '';
      if AResult = nil then
        Exit;
      if not AResult.HasParam('uploadUrl') then
      begin
        DoError(AResult);
        Exit;
      end;
      Result := AResult.GetParamValue('uploadUrl');
    finally
      AResult.Free;
    end;
  finally
    ARequestObject.Free;
  end;
end;

function TdxCloudStorageMicrosoftOneDriveUploadTask.DoUploadLargeFile(const ACancelStatus: TdxTaskCancelCallback): TdxTaskCompletedStatus;
var
  AUrl: string;
  AUriRecord: TdxURI;
  AHeader: string;
  AInet: TdxInternet;
  AConnect: TdxInternet;
  ARequest: TdxInternet;
  ASize: Cardinal;
  AObjectName: string;
begin
  Result := TdxTaskCompletedStatus.Fail;
  try
    if ACancelStatus then
      Exit(TdxTaskCompletedStatus.Cancelled);
    AUrl := OpenUploadSession;
    if AUrl = '' then
      Exit;
    AUriRecord := TdxURI.Create(AUrl);
    AObjectName := AUriRecord.Path;
    if Length(AUriRecord.Params) > 0 then
      AObjectName := Format('%s?%s', [AObjectName, AUriRecord.Params]);
    if ACancelStatus then
      Exit(TdxTaskCompletedStatus.Cancelled);
    try
      UpdateRequestParams;
      AInet := TdxHttpHelper.InternetOpen(UserAgent);
      if Assigned(AInet) then
      try
        AConnect := TdxHttpHelper.InternetConnect(AInet, PChar(AUriRecord.Host));
        if Assigned(AConnect) then
        try
          ARequest := TdxHttpHelper.OpenRequest(AConnect, 'PUT', AObjectName);
          try
            Position := 0;
            while Position < Stream.Size do
            begin
              if ACancelStatus then
                Exit(TdxTaskCompletedStatus.Cancelled);
              ASize := Min(Manager.DefaultBlockSize, Stream.Size - Position);
              AHeader := TdxHttpHelper.ConcatenateHeaders([
                Format('Content-Length: %d', [ASize]),
                Format('Content-Range: bytes %d-%d/%d', [Position, Position + ASize - 1, Stream.Size])]);
              if not HttpSendRequest(ARequest, PChar(AHeader), Length(AHeader), @Stream.Bytes[Position], ASize) then
                Exit;
              Position := Position + ASize;
            end;
            if (Position = Stream.Size) and IsSuccess(ARequest) then
              Result := TdxTaskCompletedStatus.Success;
          finally
            InternetCloseHandle(ARequest);
          end;
        finally
          InternetCloseHandle(AConnect);
        end;
      finally
        InternetCloseHandle(AInet);
      end;
    finally
      CloseUploadSession(AUriRecord.Host, AObjectName);
    end;
  except
    Result := TdxTaskCompletedStatus.Fail;
  end;
end;

function TdxCloudStorageMicrosoftOneDriveUploadTask.GetProvider: TdxCloudStorageMicrosoftOneDriveProvider;
begin
  Result := TdxCloudStorageMicrosoftOneDriveProvider(inherited Provider);
end;

{ TdxCloudStorageMicrosoftOneDriveUpdateFileStreamTask }

constructor TdxCloudStorageMicrosoftOneDriveUpdateFileStreamTask.Create(
  AProvider: TdxCloudStorageMicrosoftOneDriveProvider;
  AFile: TdxCloudStorageFile; AStream: TStream);
begin
  inherited Create(AProvider, AFile.ID,
    Format('v1.0/drives/%s/items/%s/content', [AFile.DriveID, AFile.ID]), AStream);
  FFile := AFile;
  FFileLink := cxAddObjectLink(FFile);
end;

destructor TdxCloudStorageMicrosoftOneDriveUpdateFileStreamTask.Destroy;
begin
  cxRemoveObjectLink(FFileLink);
  inherited Destroy;
end;

function TdxCloudStorageMicrosoftOneDriveUpdateFileStreamTask.GetCreateUploadSessionUri: string;
begin
  Result := Format('v1.0/drives/%s/items/%s/createUploadSession', [FFile.DriveID, FFile.ID]);
end;

function TdxCloudStorageMicrosoftOneDriveUpdateFileStreamTask.GetMIMEType: string;
begin
  Result := FFile.MIMEType;
end;

function TdxCloudStorageMicrosoftOneDriveUpdateFileStreamTask.GetUploadSessionRequestObject: TdxJSONObject;
begin
  Result := TdxJSONObject.Create;
  Result.AddPair('@microsoft.graph.conflictBehavior', 'replace');
  Result := TdxJSONObject.Create(TdxJSONPair.Create('item', Result));
end;

function TdxCloudStorageMicrosoftOneDriveUpdateFileStreamTask.IsValid: Boolean;
begin
  Result := inherited IsValid and (FFileLink.Ref <> nil);
end;

{ TdxCloudStorageMicrosoftOneDriveUploadFileTask }

constructor TdxCloudStorageMicrosoftOneDriveUploadFileTask.Create(
  AProvider: TdxCloudStorageMicrosoftOneDriveProvider;
  AParent: TdxCloudStorageFolder; const AFileName: string);
var
  AStream: TFileStream;
begin
  AStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    inherited Create(AProvider, AFileName,
      Format('v1.0/drives/%s/items/%s:/%s:/content', [AParent.DriveID, AParent.ID, TdxUri.EscapeDataString(ExtractFileName(AFileName))]), AStream);
  finally
    AStream.Free;
  end;
  FParent := AParent;
  FParentLink := cxAddObjectLink(FParent);
  FFileName := AFileName;
end;

destructor TdxCloudStorageMicrosoftOneDriveUploadFileTask.Destroy;
begin
  cxRemoveObjectLink(FParentLink);
  inherited Destroy;
end;

procedure TdxCloudStorageMicrosoftOneDriveUploadFileTask.DoComplete;
begin
  Provider.Files.AddChild(FParent.ID, Data);
  inherited DoComplete;
end;

function TdxCloudStorageMicrosoftOneDriveUploadFileTask.GetCreateUploadSessionUri: string;
begin
  Result := Format('v1.0/me/drive/items/%s:/%s:/createUploadSession', [FParent.ID, TdxUri.EscapeDataString(ExtractFileName(FFileName))]);
end;

function TdxCloudStorageMicrosoftOneDriveUploadFileTask.GetMIMEType: string;
begin
  if not TdxCloudStorageProvider.RegistryExtensionToMIMEType.TryGetValue(LowerCase(ExtractFileExt(FFileName)), Result) then
    Result := 'application/octet-stream';
end;

function TdxCloudStorageMicrosoftOneDriveUploadFileTask.GetUploadSessionRequestObject: TdxJSONObject;
begin
  Result := TdxJSONObject.Create;
  Result.AddPair('@microsoft.graph.conflictBehavior', 'rename');
  Result.AddPair('name', ExtractFileName(FFileName));
  Result := TdxJSONObject.Create(TdxJSONPair.Create('item', Result));
end;

function TdxCloudStorageMicrosoftOneDriveUploadFileTask.IsValid: Boolean;
begin
  Result := inherited IsValid and (FParentLink.Ref <> nil);
end;

{ TdxCloudStorageMicrosoftOneDriveFileTransferManager }

function TdxCloudStorageMicrosoftOneDriveFileTransferManager.CreateUpdateFileStreamTask(AFile: TdxCloudStorageFile; AStream: TStream): TdxCloudStorageUploadTask;
begin
  Result := TdxCloudStorageMicrosoftOneDriveUpdateFileStreamTask.Create(Provider, AFile, AStream);
end;

function TdxCloudStorageMicrosoftOneDriveFileTransferManager.CreateUploadFileTask(AParent: TdxCloudStorageFolder; const AFileName: string): TdxCloudStorageUploadTask;
begin
  Result := TdxCloudStorageMicrosoftOneDriveUploadFileTask.Create(Provider, AParent, AFileName);
end;

function TdxCloudStorageMicrosoftOneDriveFileTransferManager.GetProvider: TdxCloudStorageMicrosoftOneDriveProvider;
begin
  Result := TdxCloudStorageMicrosoftOneDriveProvider(inherited Provider);
end;

{ TdxCloudStorageMicrosoftOneDriveFiles }

procedure TdxCloudStorageMicrosoftOneDriveFiles.PopulateSpecialFolders;
begin
  AddSpecialFolder(TdxCloudStorageSpecialFolder.TType.Recent);
  AddSpecialFolder(TdxCloudStorageSpecialFolder.TType.SharedWithMe);
end;

{ TdxCloudStorageMicrosoftOneDriveProvider }

function TdxCloudStorageMicrosoftOneDriveProvider.GetAuthorizationAgent: TdxMicrosoftGraphAPIOAuth2AuthorizationAgent;
begin
  Result := TdxMicrosoftGraphAPIOAuth2AuthorizationAgent(inherited AuthorizationAgent);
end;

function TdxCloudStorageMicrosoftOneDriveProvider.GetFiles: TdxCloudStorageMicrosoftOneDriveFiles;
begin
  Result := TdxCloudStorageMicrosoftOneDriveFiles(inherited Files);
end;

class function TdxCloudStorageMicrosoftOneDriveProvider.GetDisplayName: string;
begin
  Result := 'Microsoft OneDrive';
end;

function TdxCloudStorageMicrosoftOneDriveProvider.CreateFiles: TdxCloudStorageFiles;
begin
  Result := TdxCloudStorageMicrosoftOneDriveFiles.Create(Self);
end;

function TdxCloudStorageMicrosoftOneDriveProvider.CreateFileTransferManager: TdxCloudStorageFileTransferManager;
begin
  Result := TdxCloudStorageMicrosoftOneDriveFileTransferManager.Create(Self);
end;

function TdxCloudStorageMicrosoftOneDriveProvider.CreateItem(const AMIMEType: string): TdxCloudStorageItem;
begin
  if AMIMEType = FFolderMIMEType then
    Result := TdxCloudStorageFolder.Create(Files)
  else
    Result := TdxCloudStorageFile.Create(Files);
end;

function TdxCloudStorageMicrosoftOneDriveProvider.GetScopes: TStringList;
begin
  Result := inherited GetScopes;
  Result.Add('Files.ReadWrite.All');
end;

function TdxCloudStorageMicrosoftOneDriveProvider.DoGetSpecialFolderData(const AType: TdxCloudStorageSpecialFolder.TType): TdxCloudStorageItemData;
begin
  Result := inherited DoGetSpecialFolderData(AType);
  case AType of
    TdxCloudStorageSpecialFolder.TType.Discover:
      Result.Name := cxGetResourceString(@sdxCloudStorageMicrosoftOneDriveProviderDiscoverFolderName);
    TdxCloudStorageSpecialFolder.TType.Trash:
      Result.Name := cxGetResourceString(@sdxCloudStorageMicrosoftOneDriveProviderTrashFolderName);
  end;
end;

function TdxCloudStorageMicrosoftOneDriveProvider.ObjectToData(const AObject: TObject): TdxCloudStorageItemData;
var
  AJson: TdxJSONObject;
  ARemoteItem: TdxJSONValue;
begin
  AJson := TdxJSONObject(AObject);
  with Result do
  begin
    ID := AJson.GetParamValue('id');
    Name := AJson.GetParamValue('name');
    if AJson.HasParam('file') then
      MIMEType := AJson.GetChildParamValue('file', 'mimeType')
    else if AJson.HasParam('folder') then
      MIMEType := FFolderMIMEType;
    Parents := TArray<string>.Create(AJson.GetChildParamValue('parentReference', 'id'));
    DriveID := AJson.GetChildParamValue('parentReference', 'driveId');
    CreatedDate := TdxISO8601Helper.StringToDateTime(AJson.GetParamValue('createdDateTime'));
    ModifiedDate := TdxISO8601Helper.StringToDateTime(AJson.GetParamValue('lastModifiedDateTime'));
    if not TryStrToInt(AJson.GetParamValue('size'), FileSize) then
      FileSize := -1;
    Shared := AJson.HasParam('remoteItem') or AJson.HasParam('shared');
    if AJson.HasParam('remoteItem') then
    begin
      ARemoteItem := AJson.GetValue('remoteItem');
      ID := ARemoteItem.GetParamValue('id');
      DriveID := ARemoteItem.GetChildParamValue('parentReference', 'driveId');
    end;
    if AJson.HasParam('@microsoft.graph.downloadUrl') then
      DownloadLink := AJson.GetParamValue('@microsoft.graph.downloadUrl')
    else
      if AJson.HasParam('@content.downloadUrl') then
        DownloadLink := AJson.GetParamValue('@content.downloadUrl')
      else
        if DriveID <> '' then
          DownloadLink := Format('https://graph.microsoft.com/v1.0/drives/%s/items/%s/content', [DriveID, ID])
        else
          DownloadLink := Format('https://graph.microsoft.com/v1.0/me/drive/items/%s/content', [ID]);
    IconLink := '';//AJson.GetParamValue('iconLink');
    Trashed := False; //AJson.GetChild('labels', 'trashed').IsTrue;
  end;
end;

function TdxCloudStorageMicrosoftOneDriveProvider.CreateCopyItemToTask(AItem: TdxCloudStorageItem; AParent: TdxCloudStorageFolder): TdxCloudStorageProviderMoveItemToCustomTask;
begin
  Result := TdxCloudStorageMicrosoftOneDriveProviderCopyItemToTask.Create(Self, AItem, AParent);
end;

function TdxCloudStorageMicrosoftOneDriveProvider.CreateCreateFolderTask(AParent: TdxCloudStorageFolder;
  const AName: string): TdxCloudStorageProviderCreateFolderTask;
begin
  Result := TdxCloudStorageMicrosoftOneDriveProviderCreateFolderTask.Create(Self, AParent, AName);
end;

function TdxCloudStorageMicrosoftOneDriveProvider.CreateMoveItemToTrashTask(AItem: TdxCloudStorageItem): TdxCloudStorageProviderMoveItemToTrashTask;
begin
  Result := TdxCloudStorageMicrosoftOneDriveProviderMoveItemToTrashTask.Create(Self, AItem);
end;

function TdxCloudStorageMicrosoftOneDriveProvider.CreateFetchAllTask: TdxCloudStorageProviderFetchAllCustomTask;
begin
  Result := TdxCloudStorageMicrosoftOneDriveProviderFetchAllTask.Create(Self);
end;

function TdxCloudStorageMicrosoftOneDriveProvider.CreateMoveItemToTask(AItem: TdxCloudStorageItem; AParent: TdxCloudStorageFolder): TdxCloudStorageProviderMoveItemToCustomTask;
begin
  Result := TdxCloudStorageMicrosoftOneDriveProviderMoveItemToTask.Create(Self, AItem, AParent);
end;

function TdxCloudStorageMicrosoftOneDriveProvider.CreateUpdateFolderTask(AFolder: TdxCloudStorageFolder): TdxCloudStorageProviderUpdateFolderCustomTask;
begin
  Result := TdxCloudStorageMicrosoftOneDriveProviderUpdateFolderTask.Create(Self, AFolder);
end;

function TdxCloudStorageMicrosoftOneDriveProvider.CreateUpdateSpecialFolderTask(AFolder: TdxCloudStorageSpecialFolder): TdxCloudStorageProviderUpdateFolderCustomTask;
begin
  Result := TdxCloudStorageMicrosoftOneDriveProviderUpdateSpecialFolderTask.Create(Self, AFolder);
end;

procedure TdxCloudStorageMicrosoftOneDriveProvider.SetAuthorizationAgent(
  const Value: TdxMicrosoftGraphAPIOAuth2AuthorizationAgent);
begin
  inherited AuthorizationAgent := Value;
end;

initialization
  TdxCloudStorageMicrosoftOneDriveProvider.Register;

finalization
  TdxCloudStorageMicrosoftOneDriveProvider.Unregister;

end.
