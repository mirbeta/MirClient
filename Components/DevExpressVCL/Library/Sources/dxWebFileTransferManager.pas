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

unit dxWebFileTransferManager;

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
  dxCore, dxCoreClasses, dxGenerics, dxForms, cxClasses;

type
  TdxWebFileTransferManager = class;

  { TdxWebFileTransferTask }

  TdxWebFileTransferTask = class abstract(TdxWebTask)
  strict private
    FAgent: TdxCustomAuthorizationAgent;
    FFileSize: Integer;
    FFreeNotificator: TcxFreeNotificator;
    FManager: TdxWebFileTransferManager;
    FManagerLink: TcxObjectLink;
    FPosition: Cardinal;
    FProcessID: string;
    FUri: string;
    procedure FreeNotificationHandler(Sender: TComponent);
  protected
    function GetHeader: string; override;
    function GetUserAgent: string; override;
    function IsValid: Boolean; override;

    procedure DoError(AObject: TdxJSONValue); override;
    procedure SetFileSize(const Value: DWORD);

    property FileSize: Integer read FFileSize;
    property FreeNotificator: TcxFreeNotificator read FFreeNotificator;
    property Manager: TdxWebFileTransferManager read FManager;
    property Position: Cardinal read FPosition;
  public
    constructor Create(AManager: TdxWebFileTransferManager;
      AAgent: TdxCustomAuthorizationAgent; const AProcessID, AUri: string); reintroduce; virtual;
    destructor Destroy; override;
    function IsEqual(const ATask: TdxWebTask): Boolean; override;

    property Agent: TdxCustomAuthorizationAgent read FAgent;
    property ProcessID: string read FProcessID;
    property Uri: string read FUri;
  end;

  { TdxWebFileDownloadTask }

  TdxWebFileDownloadTask = class abstract(TdxWebFileTransferTask)
  strict private
    FStream: TStream;
  protected
    function CreateStream: TStream; virtual; abstract;
    procedure WriteBuffer(const ABuffer; ACount: Integer); virtual;

    procedure DoCompleteDownloadFile; virtual; abstract;
    procedure DoDownloadFile; virtual;
    procedure DoStartDownloadFile; virtual;

    procedure DoComplete; override;
    function DoRun(const ACancelStatus: TdxTaskCancelCallback): TdxTaskCompletedStatus; override; final;

    property Stream: TStream read FStream;
  public
    destructor Destroy; override;
  end;

  { TdxWebFileDownloadStreamTask }

  TdxWebFileDownloadStreamTask = class(TdxWebFileDownloadTask)
  protected
    function CreateStream: TStream; override;
    procedure DoCompleteDownloadFile; override;
    procedure WriteBuffer(const ABuffer; ACount: Integer); override;
  end;

  { TdxWebFileUploadTask }

  TdxWebFileUploadTask = class abstract(TdxWebFileTransferTask)
  strict private
    FPosition: Cardinal;
    FStream: TBytesStream;
    procedure SetPosition(const Value: Cardinal);
  protected
    function GetObjectName: string; virtual; abstract;
    function GetResultFileID: string; virtual;
    function GetServerName: string; virtual; abstract;
    function GetVerb: string; virtual; abstract;

    function IsSuccess(ARequest: TdxInternet): Boolean; virtual; abstract;
    procedure DoComplete; override;
    function DoRun(const ACancelStatus: TdxTaskCancelCallback): TdxTaskCompletedStatus; override;
    function Upload(ARequest: TdxInternet; const AHeader: string; const ACancelStatus: TdxTaskCancelCallback): TdxTaskCompletedStatus; virtual;
    function  WriteStream(ARequest: TdxInternet; const ACancelStatus: TdxTaskCancelCallback): TdxTaskCompletedStatus; virtual;

    procedure DoCompleteUpload;
    procedure DoStartUpload;
    procedure DoUpload;

    property ObjectName: string read GetObjectName;
    property ServerName: string read GetServerName;
    property Stream: TBytesStream read FStream;
    property Verb: string read GetVerb;
    property Position: Cardinal read FPosition write SetPosition;
  public
    constructor Create(AManager: TdxWebFileTransferManager; AAgent: TdxCustomAuthorizationAgent;
      const AProcessID, AUri: string; AStream: TStream); reintroduce;
    destructor Destroy; override;
  end;

  { TdxWebFileTransferManager }

  TdxWebFileTransferManager = class
  public const
    DefaultBlockSize = 64 * 1024;
  strict private
    FTaskManager: TdxWebTaskManager;
  protected
    procedure DoDownload(const AProcessID: string; const APosition: Cardinal); virtual;
    procedure DoDownloaded(const AProcessID: string; const AStream: TStream); virtual;
    procedure DoDownloading(const AProcessID: string; const ASize: Integer); virtual;
    procedure DoError(const AErrorObject); virtual;
    procedure DoUploaded(const AProcessID, AFileID: string); virtual;
    procedure DoUploading(const AProcessID: string; const ASize: Integer); virtual;
    procedure DoUpload(const AProcessID: string; const APosition: Cardinal); virtual;

    property Tasks: TdxWebTaskManager read FTaskManager;
  public
    constructor Create;
    destructor Destroy; override;
    procedure BeforeDestruction; override;

    procedure CancelAllTasks;
    procedure CancelTask(const AProcessID: string);
    procedure RunDownloadFileTask(const ATask: TdxWebFileDownloadTask);
    procedure RunUploadFileTask(const ATask: TdxWebFileUploadTask);
  end;

implementation

uses
  Math, WinInet;

{ TdxWebFileTransferTask }

constructor TdxWebFileTransferTask.Create(AManager: TdxWebFileTransferManager;
  AAgent: TdxCustomAuthorizationAgent;
  const AProcessID, AUri: string);
begin
  inherited Create(AManager.Tasks);
  FManager := AManager;
  FAgent := AAgent;
  FProcessID := AProcessID;
  FUri := AUri;

  FFreeNotificator := TcxFreeNotificator.Create(nil);
  FFreeNotificator.FreeNotification(Agent);
  FFreeNotificator.OnFreeNotification := FreeNotificationHandler;
  FManagerLink := cxAddObjectLink(Manager);
end;

destructor TdxWebFileTransferTask.Destroy;
begin
  FreeAndNil(FFreeNotificator);
  cxRemoveObjectLink(FManagerLink);
  inherited Destroy;
end;

function TdxWebFileTransferTask.IsEqual(const ATask: TdxWebTask): Boolean;
begin
  Result := inherited IsEqual(ATask);
  Result := Result and (Agent = Agent) and (CompareText(Uri, TdxWebFileTransferTask(ATask).Uri) = 0);
end;

function TdxWebFileTransferTask.GetHeader: string;
begin
  if Agent <> nil then
    Result := Agent.GetAuthorizationHeader;
end;

function TdxWebFileTransferTask.GetUserAgent: string;
begin
  if Agent <> nil then
    Result := Agent.UserAgent;
end;

function TdxWebFileTransferTask.IsValid: Boolean;
var
  AResult: Boolean;
begin
  Result := inherited IsValid and (FManagerLink.Ref <> nil) and (FAgent <> nil);
  if Result then
  begin
    MainThreadSynchronize(procedure()
      begin
        AResult := Agent <> nil;
        if AResult then
        begin
          Agent.ValidateAuthorization;
          AResult := Agent.IsAuthorized;
        end;
      end);
    Result := AResult;
  end;
end;

procedure TdxWebFileTransferTask.DoError(AObject: TdxJSONValue);
begin
  MainThreadSynchronize(procedure()
    begin
      Manager.DoError(AObject);
    end);
end;

procedure TdxWebFileTransferTask.SetFileSize(const Value: DWORD);
begin
  FFileSize := Value;
end;

procedure TdxWebFileTransferTask.FreeNotificationHandler(Sender: TComponent);
begin
  if Agent = Sender then
    FAgent := nil;
end;

{ TdxWebFileDownloadTask }

destructor TdxWebFileDownloadTask.Destroy;
begin
  FreeAndNil(FStream);
  inherited Destroy;
end;

procedure TdxWebFileDownloadTask.WriteBuffer(const ABuffer; ACount: Integer);
begin
  DoDownloadFile;
end;

procedure TdxWebFileDownloadTask.DoDownloadFile;
begin
  MainThreadSynchronize(procedure ()
    begin
      Manager.DoDownload(ProcessID, Stream.Position);
    end);
end;

procedure TdxWebFileDownloadTask.DoStartDownloadFile;
begin
  MainThreadSynchronize(procedure ()
    begin
      Manager.DoDownloading(ProcessID, FileSize);
    end);
end;

procedure TdxWebFileDownloadTask.DoComplete;
begin
  DoCompleteDownloadFile;
end;

function TdxWebFileDownloadTask.DoRun(const ACancelStatus: TdxTaskCancelCallback): TdxTaskCompletedStatus;
const
  ABufferSize = TdxWebFileTransferManager.DefaultBlockSize;
var
  AInet: TdxInternet;
  ARequest: TdxInternet;
  ABuffer: array[0..ABufferSize - 1] of Byte;
  ALen: DWORD;
  AFileSize: Integer;
begin
  Result := TdxTaskCompletedStatus.Fail;
  try
    FStream := CreateStream;
  except
    Exit;
  end;
  UpdateRequestParams;
  AInet := TdxHttpHelper.InternetOpen(UserAgent);
  try
    if GetLastError <> 0 then
      Exit(TdxTaskCompletedStatus.Fail);
    ARequest := InternetOpenUrl(AInet, PChar(Uri), PChar(Header), Length(Header),
      TdxHttpHelper.GetInternetOpenUrlFlags(Uri), 0);
    try
      if GetLastError <> 0 then
        Exit(TdxTaskCompletedStatus.Fail);
      AFileSize := TdxHttpHelper.GetContentLength(ARequest);
      SetFileSize(AFileSize);
      DoStartDownloadFile;
      if ACancelStatus then
        Exit(TdxTaskCompletedStatus.Cancelled);
      while InternetReadFile(ARequest, @ABuffer, SizeOf(ABuffer), ALen) do
      begin
        if (GetLastError <> 0) or not IsValid then
          Exit(TdxTaskCompletedStatus.Fail);
        if ACancelStatus then
          Exit(TdxTaskCompletedStatus.Cancelled);
        if ALen = 0 then
          Break;
        WriteBuffer(ABuffer, ALen);
      end;
      Result := TdxTaskCompletedStatus.Success;
    finally
      InternetCloseHandle(ARequest);
    end;
  finally
    InternetCloseHandle(AInet);
  end;
end;

{ TdxWebFileDownloadStreamTask }

function TdxWebFileDownloadStreamTask.CreateStream: TStream;
begin
  Result := TMemoryStream.Create;
end;

procedure TdxWebFileDownloadStreamTask.DoCompleteDownloadFile;
begin
  MainThreadSynchronize(procedure()
    begin
      Manager.DoDownloaded(ProcessID, Stream);
    end);
end;

procedure TdxWebFileDownloadStreamTask.WriteBuffer(const ABuffer; ACount: Integer);
begin
  Stream.WriteBuffer(ABuffer, ACount);
  inherited WriteBuffer(ABuffer, ACount);
end;

{ TdxWebFileUploadTask }

constructor TdxWebFileUploadTask.Create(
  AManager: TdxWebFileTransferManager; AAgent: TdxCustomAuthorizationAgent;
  const AProcessID, AUri: string; AStream: TStream);
begin
  inherited Create(AManager, AAgent, AProcessID, AUri);
  FStream := TBytesStream.Create;
  if AStream <> nil then
    FStream.CopyFrom(AStream, 0);
  FPosition := MaxInt;
end;

destructor TdxWebFileUploadTask.Destroy;
begin
  FreeAndNil(FStream);
  inherited Destroy;
end;

function TdxWebFileUploadTask.GetResultFileID: string;
begin
  Result := ProcessID;
end;

procedure TdxWebFileUploadTask.DoComplete;
begin
  DoCompleteUpload;
end;

function TdxWebFileUploadTask.DoRun(const ACancelStatus: TdxTaskCancelCallback): TdxTaskCompletedStatus;
var
  AInet: TdxInternet;
  AConnect: TdxInternet;
  ARequest: TdxInternet;
begin
  Result := TdxTaskCompletedStatus.Fail;
  AInet := TdxHttpHelper.InternetOpen(UserAgent);
  try
    if GetLastError <> 0 then
      Exit;
    AConnect := TdxHttpHelper.InternetConnect(AInet, ServerName);
    try
      if GetLastError <> 0 then
        Exit;
      ARequest := TdxHttpHelper.OpenRequest(AConnect, Verb, ObjectName);
      try
        if GetLastError <> 0 then
          Exit;
        DoStartUpload;
        if ACancelStatus then
          Exit(TdxTaskCompletedStatus.Cancelled);
        UpdateRequestParams;
        Result := Upload(ARequest, Header, ACancelStatus);
      finally
        InternetCloseHandle(ARequest);
      end;
    finally
      InternetCloseHandle(AConnect);
    end;
  finally
    InternetCloseHandle(AInet);
  end;
end;

function TdxWebFileUploadTask.Upload(ARequest: TdxInternet;
  const AHeader: string; const ACancelStatus: TdxTaskCancelCallback): TdxTaskCompletedStatus;
var
  ABuffersIn: INTERNET_BUFFERS;
begin
  Result := TdxTaskCompletedStatus.Fail;
  if ACancelStatus then
    Exit(TdxTaskCompletedStatus.Cancelled);
  if not HttpAddRequestHeaders(ARequest, PChar(AHeader), Length(AHeader), HTTP_ADDREQ_FLAG_ADD or HTTP_ADDREQ_FLAG_REPLACE) then
    Exit;
  Position := 0;

  FillChar(ABuffersIn, SizeOf(ABuffersIn), 0);
  ABuffersIn.dwStructSize := SizeOf(ABuffersIn);
  ABuffersIn.dwBufferTotal := Stream.Size;
  if not HttpSendRequestEx(ARequest, @ABuffersIn, nil, HSR_INITIATE, 0) then
    Exit;
  try
    Result := WriteStream(ARequest, ACancelStatus);
  finally
    HttpEndRequest(ARequest, nil, 0, 0);
  end;
  if (Position = Stream.Size) and IsSuccess(ARequest) then
    Result := TdxTaskCompletedStatus.Success;
end;

function TdxWebFileUploadTask.WriteStream(ARequest: TdxInternet; const ACancelStatus: TdxTaskCancelCallback): TdxTaskCompletedStatus;
var
  ACount: Cardinal;
  ASize: Integer;
begin
  Result := TdxTaskCompletedStatus.Success;
  try
    while Position < Stream.Size do
    begin
      if ACancelStatus then
        Exit(TdxTaskCompletedStatus.Cancelled);
      ASize := Min(Manager.DefaultBlockSize, Stream.Size - Position);
      if not InternetWriteFile(ARequest, @Stream.Bytes[Position], ASize, ACount) then
        Exit;
      if (ACount = 0) and (ASize > 0) then
        Exit;
      Position := Position + ACount;
    end;
  except
    Result := TdxTaskCompletedStatus.Fail;
  end;
end;

procedure TdxWebFileUploadTask.DoUpload;
begin
  MainThreadSynchronize(procedure ()
    begin
      Manager.DoUpload(ProcessID, Position);
    end);
end;

procedure TdxWebFileUploadTask.DoCompleteUpload;
begin
  MainThreadSynchronize(procedure ()
    begin
      Manager.DoUploaded(ProcessID, GetResultFileID);
    end);
end;

procedure TdxWebFileUploadTask.DoStartUpload;
begin
  MainThreadSynchronize(procedure ()
    begin
      Manager.DoUploading(ProcessID, Stream.Size);
    end);
end;

procedure TdxWebFileUploadTask.SetPosition(const Value: Cardinal);
begin
  if Position <> Value then
  begin
    FPosition := Value;
    DoUpload;
  end;
end;

{ TdxWebFileTransferManager }

constructor TdxWebFileTransferManager.Create;
begin
  inherited Create;
  FTaskManager := TdxWebTaskManager.Create;
end;

destructor TdxWebFileTransferManager.Destroy;
begin
  cxClearObjectLinks(Self);
  FreeAndNil(FTaskManager);
  inherited Destroy;
end;

procedure TdxWebFileTransferManager.BeforeDestruction;
begin
  CancelAllTasks;
  inherited BeforeDestruction;
end;

procedure TdxWebFileTransferManager.DoDownloaded(const AProcessID: string;
  const AStream: TStream);
begin
// do nothing
end;

procedure TdxWebFileTransferManager.DoError(const AErrorObject);
begin
// do nothing
end;

procedure TdxWebFileTransferManager.DoDownload(const AProcessID: string;
  const APosition: Cardinal);
begin
// do nothing
end;

procedure TdxWebFileTransferManager.DoDownloading(const AProcessID: string; const ASize: Integer);
begin
// do nothing
end;

procedure TdxWebFileTransferManager.DoUpload(const AProcessID: string; const APosition: Cardinal);
begin
// do nothing
end;

procedure TdxWebFileTransferManager.DoUploaded(const AProcessID, AFileID: string);
begin
// do nothing
end;

procedure TdxWebFileTransferManager.DoUploading(const AProcessID: string; const ASize: Integer);
begin
// do nothing
end;

procedure TdxWebFileTransferManager.CancelAllTasks;
begin
  FTaskManager.CancelAllTasks;
end;

procedure TdxWebFileTransferManager.CancelTask(const AProcessID: string);
begin
  FTaskManager.ForEach(procedure(ATask: TdxWebTask)
    begin
      if CompareText(TdxWebFileTransferTask(ATask).ProcessID, AProcessID) = 0 then
        FTaskManager.CancelTask(ATask);
    end);
end;

procedure TdxWebFileTransferManager.RunDownloadFileTask(const ATask: TdxWebFileDownloadTask);
begin
  FTaskManager.RunTask(ATask);
end;

procedure TdxWebFileTransferManager.RunUploadFileTask(const ATask: TdxWebFileUploadTask);
begin
  FTaskManager.RunTask(ATask);
end;

end.
