{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressMapControl                                        }
{                                                                    }
{           Copyright (c) 2013-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSMAPCONTROL AND ALL             }
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

unit dxRESTService;

interface

{$I cxVer.inc}

uses
  Windows, SysUtils, StrUtils, Classes, Types, Generics.Defaults, Generics.Collections,
  dxCore, dxXMLDoc, dxCustomTree, cxClasses,
  dxMapControlHttpRequest, dxBingMapRESTServiceStrs, dxMapControlTypes;

type
  TdxRESTService = class;

  TdxRestServiceOutputFormat = (rsofXml, rsofJson);

  TdxAsyncTask = class
  private
    FThread: TThread;
    FOnTaskFinished: TNotifyEvent;
  protected
    procedure Execute; virtual;
    procedure TaskFinished; virtual;
    property Thread: TThread read FThread write FThread;
  public
    property OnTaskFinished: TNotifyEvent read FOnTaskFinished write FOnTaskFinished;
  end;

  TdxAsyncTaskRunnerThread = class(TcxThread)
  private
    FTask: TdxAsyncTask;
  protected
    procedure Execute; override;
    procedure ExecuteTask; virtual;
    procedure TaskFinished; virtual;
  public
    constructor Create(ATask: TdxAsyncTask);
  end;

  TdxAsyncTaskRunner = class
  private
    FAsyncRequests: TObjectList<TdxAsyncTaskRunnerThread>;
    procedure AsyncRequestTerminated(Sender: TObject);
    procedure WaitRequests;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure CancelRequests;
    procedure RunTask(ATask: TdxAsyncTask);
  end;

  TdxRestServiceResponse = class
  private
    FIsSuccess: Boolean;
  protected
    procedure DoParse(ADoc: TdxXMLDocument); virtual;
    procedure SetStatus(AIsSuccess: Boolean);
  public
    procedure Parse(AStream: TStream; AFormat: TdxRestServiceOutputFormat); virtual;
    property IsSuccess: Boolean read FIsSuccess;
  end;

  TdxRESTServiceResponseEvent = procedure(Sender: TObject;
    AResponse: TdxRestServiceResponse; var ADestroyResponse: Boolean) of object;

  TdxRESTServiceTask = class(TdxAsyncTask)
  private
    FResponse: TdxRestServiceResponse;
    FService: TdxRESTService;
    FUrl: string;
    procedure DoOnRequest(ASender: TObject;
      ARequestMode: TdxMapControlHttpRequestMode; ACount: Int64; var ACancel: Boolean);
  protected
    procedure Execute; override;
    procedure TaskFinished; override;
    property Response: TdxRestServiceResponse read FResponse write FResponse;
  public
    constructor Create(AService: TdxRESTService; const AUrl: string);
  end;

  TdxRESTService = class
  private
    FAsyncTaskRunner: TdxAsyncTaskRunner;
    FOnResponse: TdxRESTServiceResponseEvent;
  protected
    function CreateResponse: TdxRestServiceResponse; virtual;
    procedure DoRequest(const AUrl: string; out AResponse: TdxRestServiceResponse;
      AOnRequest: TdxMapControlHttpRequestEvent = nil); virtual;
    procedure DoResponse(var AResponse: TdxRestServiceResponse); virtual;
    function GetResponseFormat: TdxRestServiceOutputFormat; virtual;
    procedure ParseResponse(AStream: TStream; AResponse: TdxRestServiceResponse); virtual;
    procedure RunAsyncTask(ATask: TdxRESTServiceTask);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure CancelRequests;
    procedure Request(const AUrl: string); overload; virtual;
    procedure Request(const AUrl: string; out AResponse: TdxRestServiceResponse); overload; virtual;
    procedure RequestAsync(const AUrl: string); virtual;
    property OnResponse: TdxRESTServiceResponseEvent read FOnResponse write FOnResponse;
  end;

implementation

{ TdxAsyncTask }

procedure TdxAsyncTask.Execute;
begin
end;

procedure TdxAsyncTask.TaskFinished;
begin
  dxCallNotify(FOnTaskFinished, Self);
end;

{ TdxAsyncTaskRunnerThread }

constructor TdxAsyncTaskRunnerThread.Create(ATask: TdxAsyncTask);
begin
  inherited Create(True);
  FTask := ATask;
  FTask.Thread := Self;
end;

procedure TdxAsyncTaskRunnerThread.Execute;
begin
  ExecuteTask;
  if not Terminated then
    Synchronize(TaskFinished);
  FTask.Free;
end;

procedure TdxAsyncTaskRunnerThread.ExecuteTask;
begin
  FTask.Execute;
end;

procedure TdxAsyncTaskRunnerThread.TaskFinished;
begin
  if not Terminated then
    FTask.TaskFinished;
end;

{ TdxAsyncTaskRunner}

constructor TdxAsyncTaskRunner.Create;
begin
  inherited Create;
  FAsyncRequests := TObjectList<TdxAsyncTaskRunnerThread>.Create(False);
end;

destructor TdxAsyncTaskRunner.Destroy;
begin
  CancelRequests;
  WaitRequests;
  FreeAndNil(FAsyncRequests);
  inherited Destroy;
end;

procedure TdxAsyncTaskRunner.CancelRequests;
var
  ARequest: TdxAsyncTaskRunnerThread;
begin
  for ARequest in FAsyncRequests do
    ARequest.Terminate;
end;

procedure TdxAsyncTaskRunner.RunTask(ATask: TdxAsyncTask);
var
  AThread: TdxAsyncTaskRunnerThread;
begin
  AThread := TdxAsyncTaskRunnerThread.Create(ATask);
  AThread.FreeOnTerminate := True;
  AThread.Priority := tpIdle;
  AThread.OnTerminate := AsyncRequestTerminated;
  FAsyncRequests.Add(AThread);
  AThread.Start;
end;

procedure TdxAsyncTaskRunner.AsyncRequestTerminated(Sender: TObject);
begin
  FAsyncRequests.Remove(Sender as TdxAsyncTaskRunnerThread);
end;

procedure TdxAsyncTaskRunner.WaitRequests;
begin
  while FAsyncRequests.Count > 0 do
    CheckSynchronize;
end;

{ TdxRestServiceResponse }

procedure TdxRestServiceResponse.Parse(AStream: TStream;
  AFormat: TdxRestServiceOutputFormat);
var
  ADoc: TdxXMLDocument;
begin
  if AFormat = rsofXml then
  begin
    ADoc := TdxXMLDocument.Create(nil);
    try
      AStream.Position := 0;
      ADoc.LoadFromStream(AStream);
      DoParse(ADoc);
    finally
      ADoc.Free;
    end;
  end;
end;

procedure TdxRestServiceResponse.DoParse(ADoc: TdxXMLDocument);
begin
end;

procedure TdxRestServiceResponse.SetStatus(AIsSuccess: Boolean);
begin
  FIsSuccess := AIsSuccess;
end;

{ TdxRESTServiceTask }

constructor TdxRESTServiceTask.Create(AService: TdxRESTService; const AUrl: string);
begin
  inherited Create;
  FService := AService;
  FUrl := AUrl;
end;

procedure TdxRESTServiceTask.Execute;
begin
  FService.DoRequest(FUrl, FResponse, DoOnRequest);
end;

procedure TdxRESTServiceTask.TaskFinished;
begin
  FService.DoResponse(FResponse);
end;

procedure TdxRESTServiceTask.DoOnRequest(ASender: TObject;
  ARequestMode: TdxMapControlHttpRequestMode; ACount: Int64; var ACancel: Boolean);
begin
  ACancel := (Thread as TdxAsyncTaskRunnerThread).Terminated;
end;

{ TdxRESTService }

constructor TdxRESTService.Create;
begin
  inherited Create;
  FAsyncTaskRunner := TdxAsyncTaskRunner.Create;
end;

destructor TdxRESTService.Destroy;
begin
  FreeAndNil(FAsyncTaskRunner);
  inherited Destroy;
end;

procedure TdxRESTService.CancelRequests;
begin
  FAsyncTaskRunner.CancelRequests;
end;

procedure TdxRESTService.Request(const AUrl: string);
var
  AResponse: TdxRestServiceResponse;
begin
  DoRequest(AUrl, AResponse);
  DoResponse(AResponse);
end;

procedure TdxRESTService.Request(const AUrl: string;
  out AResponse: TdxRestServiceResponse);
begin
  DoRequest(AUrl, AResponse);
end;

procedure TdxRESTService.RequestAsync(const AUrl: string);
var
  ATask: TdxRESTServiceTask;
begin
  ATask := TdxRESTServiceTask.Create(Self, AUrl);
  RunAsyncTask(ATask);
end;

function TdxRESTService.CreateResponse: TdxRestServiceResponse;
begin
  Result := nil;
end;

procedure TdxRESTService.DoRequest(const AUrl: string;
  out AResponse: TdxRestServiceResponse; AOnRequest: TdxMapControlHttpRequestEvent = nil);
var
  AStream: TMemoryStream;
begin
  AResponse := nil;
  AStream := TMemoryStream.Create;
  try
    with TdxMapControlHttpRequest.Create do
    try
      OnRequest := AOnRequest;
      Get(AUrl, AStream);
      if not HasErrors and not IsCancelled then
      begin
        AResponse := CreateResponse;
        ParseResponse(AStream, AResponse);
      end;
    finally
      Free;
    end;
  finally
    AStream.Free;
  end;
end;

procedure TdxRESTService.DoResponse(var AResponse: TdxRestServiceResponse);
var
  ADestroyResponse: Boolean;
begin
  ADestroyResponse := True;
  if Assigned(FOnResponse) then
    FOnResponse(Self, AResponse, ADestroyResponse);
  if ADestroyResponse then
    FreeAndNil(AResponse);
end;

function TdxRESTService.GetResponseFormat: TdxRestServiceOutputFormat;
begin
  Result := rsofXml;
end;

procedure TdxRESTService.ParseResponse(AStream: TStream;
  AResponse: TdxRestServiceResponse);
begin
  if AResponse <> nil then
    AResponse.Parse(AStream, GetResponseFormat);
end;

procedure TdxRESTService.RunAsyncTask(ATask: TdxRESTServiceTask);
begin
  FAsyncTaskRunner.RunTask(ATask);
end;

end.
