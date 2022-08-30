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

unit dxWinInet;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, SysUtils, Windows,
{$IFDEF DELPHIXE6}
  JSON,
{$ELSE}
  DBXJSON,
{$ENDIF}
  Classes, WinInet, Generics.Collections,
  dxThreading, cxClasses;

{$IFDEF WIN32}
  {$HPPEMIT '#pragma link "wininet.lib"'}
{$ENDIF}

type
  TdxWebTaskManager = class;

  TdxJSONValue = TJSONValue;
  TdxJSONObject = TJSONObject;
  TdxJSONPair = TJSONPair;
  TdxJSONArray = TJSONArray;
  TdxJSONString = TJSONString;
  TdxJSONTrue = TJSONTrue;
  TdxJSONFalse = TJSONFalse;
  TdxJSONNull = TJSONNull;

  TdxInternet = HINTERNET;

  { TdxHttpHelper }

  TdxHttpHelper = class
  public type
    TInterruptOperationFunc = reference to function(const AUri: string): Boolean;
    TSendRequestResultFunc = reference to procedure(ARequest: HINTERNET);
  public const
  {$REGION 'public const'}
    HTTP_VERSION_1_1 = 'HTTP/1.1';
    InternetOpenFlags: DWORD = 0;
    InternetOpenProxy: string = '';
    InternetOpenProxyBypass: string = '';
    InternetOpenAccessType: DWORD = INTERNET_OPEN_TYPE_PRECONFIG;
    ContentTypeJSONHeader = 'Content-Type: application/json';
    DefaultOpenRequestFlags = INTERNET_SERVICE_HTTP or INTERNET_FLAG_SECURE or INTERNET_FLAG_PASSIVE or INTERNET_FLAG_KEEP_CONNECTION or INTERNET_FLAG_DONT_CACHE;
  {$ENDREGION}
  protected
    class function GetJSONObject(ARequest: HINTERNET): TdxJSONObject; overload; static;
    class function SendRequest(const AUserAgent, AServerName, AObjectName, AHeader, AVerb: string; const AParams: TdxJSONObject): TdxJSONObject; overload; static;
  public
    class function ConcatenateHeaders(const AHeaders: array of string): string; static;
    class function GetContentLength(ARequest: HINTERNET): Integer; static;
    class function GetInternetOpenUrlFlags(const AUri: string): DWORD; static;
    class function InternetOpen(const AUserAgent: string): HINTERNET; static;
    class function InternetConnect(AInet: HINTERNET; const AServerName: string): HINTERNET;
    class function OpenRequest(AConnect: HINTERNET; const AVerb, AObjectName: string): HINTERNET; static;
    class function SendRequest(const AUserAgent, AServerName, AObjectName, AHeader, AVerb: string; const AParams: TBytes): TdxJSONObject; overload; static;

    class function JSONObjectToBytes(const AObject: TJSONObject): TBytes; static;

    class function GetHttpResponseHeaders(ARequest: HINTERNET): TStrings; static;
    class function GetHttpResponseCustomHeader(ARequest: HINTERNET; const AHeader: string): string; static;
    class function GetHttpStatus(ARequest: HINTERNET): Cardinal; static;

    class function DeleteRequest(const AUserAgent, AServerName, AObjectName, AHeader: string; const AParams: TdxJSONObject): TdxJSONObject; static;
    class function GetRequest(const AUserAgent, AUri, AHeader: string): TdxJSONObject; overload; static;
    class function GetRequest(const AUserAgent, AServerName, AObjectName, AHeader: string; const AParams: TdxJSONObject): TdxJSONObject; overload; static;
    class function GetRequest(const AUserAgent, AServerName, AObjectName, AHeader: string; const AParams: TBytes): TdxJSONObject; overload; static;
    class function GetStream(const AUserAgent, AUri, AHeader: string; const AStream: TStream; const AInterruptFunc: TInterruptOperationFunc = nil): Boolean; static;
    class function PatchRequest(const AUserAgent, AServerName, AObjectName, AHeader: string; const AParams: TdxJSONObject): TdxJSONObject; static;
    class function PostRequest(const AUserAgent, AServerName, AObjectName, AHeader: string; const AParams: TdxJSONObject): TdxJSONObject; overload; static;
    class function PostRequest(const AUserAgent, AServerName, AObjectName, AHeader: string; const AParams: TBytes): TdxJSONObject; overload; static;
    class function PutRequest(const AUserAgent, AServerName, AObjectName, AHeader: string; const AParams: TdxJSONObject): TdxJSONObject; static;
  end;

  { TJSONValueHelper }

  TJSONValueHelper = class helper for TJSONValue
  public
    class function CreateBooleanValue(const Value: Boolean): TdxJSONValue; static;
    function Get(const AParamName: string): TdxJSONValue;
    function GetChild(const AParentName, AParamName: string): TdxJSONValue;
    function GetChildParamValue(const AParentName, AParamName: string): string;
    function GetPair(const AParamName: string): TdxJSONPair;
    function GetParamValue(const AParamName: string): string;
    function HasChildParam(const AParentName, AParamName: string): Boolean;
    function HasParam(const AParamName: string): Boolean;
    function IsArray: Boolean;
    function IsTrue: Boolean;
  end;

{$IFNDEF DELPHIXE6}
  { TJSONObjectHelper }

  TJSONObjectHelper = class helper for TJSONObject
  private
    function GetCount: Integer;
    function GetItem(Index: Integer): TJSONPair;
  public
  {$IFNDEF DELPHI2010}
    procedure AddPair(const AName, AValue: UnicodeString); overload;
  {$ENDIF}
    function GetValue(const AParamName: string): TdxJSONValue;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TJSONPair read GetItem;
  end;

  { TJSONArrayHelper }

  TJSONArrayHelper = class helper for TJSONArray
  private
    function GetCount: Integer;
    function GetItem(Index: Integer): TJSONValue;
  public
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TJSONValue read GetItem;
  end;

{$ENDIF}

  { TdxISO8601Helper }

  TdxISO8601Helper = class
  public const
    UTCTimeZoneName = 'UTC';
  public
    class function DateTimeToString(AValue: TDateTime): string; static;
    class function StringToDateTime(const AValue: string; out AIsUtcTimeZone: Boolean): TDateTime; overload; static;
    class function StringToDateTime(const AValue: string): TDateTime; overload; static;
  end;

  { TdxWebTask }

  TdxWebTask = class abstract(TInterfacedObject, IdxTask)
  strict private
    FHeader: string;
    FOwner: TdxWebTaskManager;
    FOwnerLink: TcxObjectLink;
    FTaskHandle: THandle;
    FUserAgent: string;
  protected
    function GetHeader: string; virtual; abstract;
    function GetUserAgent: string; virtual; abstract;
    procedure UpdateRequestParams; virtual;

    function IsValid: Boolean; virtual;
    procedure DoError(AObject: TdxJSONValue); virtual; abstract;
    procedure DoComplete; virtual; abstract;
    function DoRun(const ACancelStatus: TdxTaskCancelCallback): TdxTaskCompletedStatus; virtual; abstract;

    procedure MainThreadSynchronize(AThreadProc: TThreadProcedure);

    property Header: string read FHeader;
    property Owner: TdxWebTaskManager read FOwner;
    property TaskHandle: THandle read FTaskHandle write FTaskHandle;
    property UserAgent: string read FUserAgent;
  public
    constructor Create(AOwner: TdxWebTaskManager);
    destructor Destroy; override;
    procedure BeforeDestruction; override;

    function IsEqual(const ATask: TdxWebTask): Boolean; virtual;

  {$REGION 'IdxTask'}
    function Run(const ACancelStatus: TdxTaskCancelCallback): TdxTaskCompletedStatus;
    procedure OnComplete(AStatus: TdxTaskCompletedStatus);
  {$ENDREGION}
  end;

  { TdxWebTaskManager }

  TdxWebTaskManager = class(TPersistent)
  strict private type
    TForEachProcRef = reference to procedure (ATask: TdxWebTask);
  strict private
    FIsDestroying: Boolean;
    FTasks: TList<TdxWebTask>;
  protected
    procedure Remove(const ATask: TdxWebTask);
    property IsDestroying: Boolean read FIsDestroying;
    property Tasks: TList<TdxWebTask> read FTasks;
  public
    constructor Create;
    destructor Destroy; override;
    procedure BeforeDestruction; override;

    procedure CancelAllTasks;
    procedure CancelTask(const ATask: TdxWebTask);
    procedure ForEach(AProc: TForEachProcRef);
    function HasSameTask(const ATask: TdxWebTask): Boolean;
    procedure RunTask(const ATask: TdxWebTask); virtual;
  end;

implementation

uses
  DateUtils,
  dxCore, cxDateUtils, dxUriRecord, dxStringHelper;

{ TdxHttpHelper }

class function TdxHttpHelper.GetJSONObject(ARequest: HINTERNET): TdxJSONObject;
const
  ABufSize = 1024;
var
  ABuf, ABuffer: TBytes;
  ACount: DWORD;
  AReadByteCount: DWORD;
  AResult: TdxJSONValue;
{$IFNDEF DELPHIXE}
  I: Cardinal;
  AIsString: Boolean;
{$ENDIF}
begin
  Result := nil;
  SetLength(ABuf, ABufSize);
  AReadByteCount := 0;
  repeat
    ACount := 0;
    InternetReadFile(ARequest, @ABuf[0], ABufSize, ACount);
    AReadByteCount := AReadByteCount + ACount;
    if ACount > 0 then
    begin
      SetLength(ABuffer, AReadByteCount);
      Move(ABuf[0], ABuffer[AReadByteCount - ACount], ACount);
    end;
  until (ACount = 0);
{$IFNDEF DELPHIXE}
  AIsString := False;
  for I := AReadByteCount - 1 downto 0 do
  begin
    if ABuffer[I] = Ord('"') then
    begin
      if AIsString then
      begin
        if ABuffer[I - 1] <> Ord('\') then
          AIsString := False;
      end
      else
        AIsString := True;
    end
    else
    begin
      if not AIsString and (ABuffer[I] in [10, 13, 32]) then
      begin
        Move(ABuffer[I + 1], ABuffer[I], AReadByteCount - I);
        Dec(AReadByteCount);
      end;
    end;
    SetLength(ABuffer, AReadByteCount);
  end;
{$ENDIF}
  AResult := TJSONObject.ParseJSONValue(ABuffer, 0, AReadByteCount);
  try
    if AResult is TJSONObject then
      Result := TJSONObject(AResult);
  finally
    if Result <> AResult then
      AResult.Free;
  end;
end;

class function TdxHttpHelper.ConcatenateHeaders(const AHeaders: array of string): string;
const
  ASeparator = #$0D + #$0A;
var
  I: Integer;
  AHeader: string;
begin
  Result := '';
  for I := 0 to Length(AHeaders) - 1 do
  begin
    AHeader := Trim(AHeaders[I]);
    if AHeader <> '' then
    begin
      if Result <> '' then
        Result := Result + ASeparator;
      Result := Result + AHeader;
    end;
  end;
end;

class function TdxHttpHelper.GetContentLength(ARequest: HINTERNET): Integer;
var
  ABuffer: DWORD;
  ALength: DWORD;
  AIndex: DWORD;
begin
  ALength := SizeOf(ABuffer);
  AIndex := 0;
  if HttpQueryInfo(ARequest,
        HTTP_QUERY_FLAG_REQUEST_HEADERS or HTTP_QUERY_CONTENT_LENGTH or HTTP_QUERY_FLAG_NUMBER,
        @ABuffer, ALength, AIndex) then
      Result := ABuffer
  else
    Result := -1;
end;

class function TdxHttpHelper.DeleteRequest(const AUserAgent, AServerName, AObjectName, AHeader: string; const AParams: TdxJSONObject): TdxJSONObject;
begin
  Result := SendRequest(AUserAgent, AServerName, AObjectName, AHeader, 'DELETE', AParams);
end;

class function TdxHttpHelper.GetRequest(const AUserAgent, AUri, AHeader: string): TdxJSONObject;
var
  AInet: HINTERNET;
  ARequest: HINTERNET;
begin
  Result := nil;
  try
    AInet := InternetOpen(AUserAgent);
    if Assigned(AInet) then
    try
      ARequest := InternetOpenUrl(AInet, PChar(AUri), PChar(AHeader), Length(AHeader),
        INTERNET_FLAG_KEEP_CONNECTION or INTERNET_FLAG_PASSIVE or INTERNET_FLAG_SECURE or INTERNET_FLAG_DONT_CACHE, 0);
      if Assigned(ARequest) then
      try
        Result := TdxHttpHelper.GetJSONObject(ARequest);
      finally
        InternetCloseHandle(ARequest);
      end;
    finally
      InternetCloseHandle(AInet);
    end;
  except
  end;
end;

class function TdxHttpHelper.SendRequest(
  const AUserAgent, AServerName, AObjectName, AHeader, AVerb: string;
  const AParams: TBytes): TdxJSONObject;
var
  AInet: HINTERNET;
  AConnect: HINTERNET;
  ARequest: HINTERNET;
begin
  Result := nil;
  AInet := InternetOpen(AUserAgent);
  if Assigned(AInet) then
  try
    AConnect := InternetConnect(AInet, PChar(AServerName));
    if Assigned(AConnect) then
    try
      ARequest := OpenRequest(AConnect, AVerb, AObjectName);
      if Assigned(ARequest) then
      try
        if not HTTPSendRequest(ARequest, PChar(AHeader), Length(AHeader), @AParams[0], Length(AParams)) then
          Exit;
        Result := TdxHttpHelper.GetJSONObject(ARequest);
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

class function TdxHttpHelper.JSONObjectToBytes(const AObject: TJSONObject): TBytes;
begin
  if AObject <> nil then
    Result := TEncoding.UTF8.GetBytes(AObject.ToString)
  else
    Result := nil;
end;

class function TdxHttpHelper.GetHttpResponseHeaders(ARequest: HINTERNET): TStrings;
var
  AResult: string;
  ASize, AIndex: Cardinal;
begin
  Result := TStringList.Create;
  AResult := ' ';
  ASize := 1;
  AIndex := 0;
  if not HttpQueryInfo(ARequest, HTTP_QUERY_RAW_HEADERS_CRLF, PChar(AResult), ASize, AIndex) then
  begin
    if GetLastError = ERROR_INSUFFICIENT_BUFFER then
    begin
      SetLength(AResult, ASize);
      if HttpQueryInfo(ARequest, HTTP_QUERY_RAW_HEADERS_CRLF, PChar(AResult), ASize, AIndex) then
        Result.Text := AResult;
    end;
  end;
end;

class function TdxHttpHelper.GetHttpResponseCustomHeader(ARequest: HINTERNET; const AHeader: string): string;
var
  ASize, AIndex: Cardinal;
begin
  Result := AHeader;
  ASize := Length(Result);
  AIndex := 0;
  if not HttpQueryInfo(ARequest, HTTP_QUERY_CUSTOM, PChar(Result), ASize, AIndex) then
  begin
    if GetLastError = ERROR_INSUFFICIENT_BUFFER then
    begin
      SetLength(Result, ASize);
      AIndex := 0;
      if not HttpQueryInfo(ARequest, HTTP_QUERY_CUSTOM, PChar(Result), ASize, AIndex) then
        Result := '';
    end;
  end;
end;

class function TdxHttpHelper.GetHttpStatus(ARequest: HINTERNET): Cardinal;
var
  ASize, AIndex: Cardinal;
begin
  ASize := SizeOf(Result);
  if not HttpQueryInfo(ARequest, HTTP_QUERY_FLAG_NUMBER or HTTP_QUERY_STATUS_CODE, @Result, ASize, AIndex) then
    Result := 0;
end;

class function TdxHttpHelper.SendRequest(const AUserAgent, AServerName, AObjectName, AHeader, AVerb: string; const AParams: TdxJSONObject): TdxJSONObject;
var
  ABytes: TBytes;
begin
  ABytes := JSONObjectToBytes(AParams);
  Result := SendRequest(AUserAgent, AServerName, AObjectName, AHeader, AVerb, ABytes);
end;

class function TdxHttpHelper.GetRequest(
  const AUserAgent, AServerName, AObjectName, AHeader: string;
  const AParams: TdxJSONObject): TdxJSONObject;
begin
  Result := SendRequest(AUserAgent, AServerName, AObjectName, AHeader, 'GET', AParams);
end;

class function TdxHttpHelper.GetRequest(const AUserAgent, AServerName, AObjectName, AHeader: string; const AParams: TBytes): TdxJSONObject;
begin
  Result := SendRequest(AUserAgent, AServerName, AObjectName, AHeader, 'GET', AParams);
end;

class function TdxHttpHelper.GetStream(const AUserAgent, AUri, AHeader: string;
  const AStream: TStream; const AInterruptFunc: TInterruptOperationFunc = nil): Boolean;
const
  BufferSize = 1024;
var
  AInet: HINTERNET;
  ARequest: HINTERNET;
  ABuffer: array[0..BufferSize - 1] of Byte;
  ALen: DWORD;
begin
  Result := False;
  AInet := InternetOpen(AUserAgent);
  try
    if GetLastError <> 0 then
      Exit;
    ARequest := InternetOpenUrl(AInet, PChar(AUri), PChar(AHeader), Length(AHeader),
      GetInternetOpenUrlFlags(AUri), 0);
    try
      if GetLastError <> 0 then
        Exit;
      while InternetReadFile(ARequest, @ABuffer, SizeOf(ABuffer), ALen) do
      begin
        if GetLastError <> 0 then
          Break;
        if ALen = 0 then
          Break;
        if Assigned(AInterruptFunc) and AInterruptFunc(AUri) then
          Exit(False);
        Result := True;
        AStream.WriteBuffer(ABuffer, ALen);
      end;
    finally
      InternetCloseHandle(ARequest);
    end;
  finally
    InternetCloseHandle(AInet);
  end;
end;

class function TdxHttpHelper.PostRequest(const AUserAgent, AServerName, AObjectName,
  AHeader: string; const AParams: TdxJSONObject): TdxJSONObject;
begin
  Result := SendRequest(AUserAgent, AServerName, AObjectName, AHeader, 'POST', AParams);
end;

class function TdxHttpHelper.PostRequest(const AUserAgent, AServerName, AObjectName, AHeader: string; const AParams: TBytes): TdxJSONObject;
begin
  Result := SendRequest(AUserAgent, AServerName, AObjectName, AHeader, 'POST', AParams);
end;

class function TdxHttpHelper.PatchRequest(const AUserAgent, AServerName, AObjectName,
  AHeader: string; const AParams: TdxJSONObject): TdxJSONObject;
begin
  Result := SendRequest(AUserAgent, AServerName, AObjectName, AHeader, 'PATCH', AParams);
end;

class function TdxHttpHelper.PutRequest(const AUserAgent, AServerName,
  AObjectName, AHeader: string; const AParams: TdxJSONObject): TdxJSONObject;
begin
  Result := SendRequest(AUserAgent, AServerName, AObjectName, AHeader, 'PUT', AParams);
end;

class function TdxHttpHelper.GetInternetOpenUrlFlags(const AUri: string): DWORD;
var
  AUriRecord: TdxUri;
begin
  Result := INTERNET_FLAG_NEED_FILE or INTERNET_FLAG_KEEP_CONNECTION or INTERNET_FLAG_PASSIVE or INTERNET_FLAG_DONT_CACHE;
  AUriRecord := TdxUri.Create(AUri);
  if AUriRecord.IsAbsoluteUri and AUriRecord.IsSecurityUri then
    Result := Result or SECURITY_INTERNET_MASK;
end;

class function TdxHttpHelper.InternetConnect(AInet: HINTERNET;
  const AServerName: string): HINTERNET;
begin
  Result := Wininet.InternetConnect(AInet, PChar(AServerName), INTERNET_DEFAULT_HTTPS_PORT, nil, nil, INTERNET_SERVICE_HTTP, 0, 0);
end;

class function TdxHttpHelper.InternetOpen(const AUserAgent: string): HINTERNET;
begin
  Result := WinInet.InternetOpen(PChar(AUserAgent), InternetOpenAccessType,
    PChar(InternetOpenProxy), PChar(InternetOpenProxyBypass), InternetOpenFlags);
end;

class function TdxHttpHelper.OpenRequest(AConnect: HINTERNET; const AVerb, AObjectName: string): HINTERNET;
begin
  Result := HttpOpenRequest(AConnect, PChar(AVerb), PChar(AObjectName), HTTP_VERSION_1_1, '', nil,
    DefaultOpenRequestFlags, 0);
end;

{ TJSONValueHelper }

class function TJSONValueHelper.CreateBooleanValue(const Value: Boolean): TdxJSONValue;
begin
  if Value then
    Result := TJSONTrue.Create
  else
    Result := TJSONFalse.Create;
end;

function TJSONValueHelper.GetPair(const AParamName: string): TdxJSONPair;
{$IFNDEF DELPHIXE}
  var
    I: Integer;
{$ENDIF}
begin
  Result := nil;
  if Self is TdxJSONObject then
  begin
  {$IFNDEF DELPHIXE}
    for I := 0 to TdxJSONObject(Self).Size - 1 do
      if TdxJSONObject(Self).Get(I).JsonString.Value = AParamName then
      begin
        Result := TdxJSONObject(Self).Get(I);
        Break;
      end;
  {$ELSE}
    Result := TdxJSONObject(Self).Get(AParamName);
  {$ENDIF}
  end;
end;

function TJSONValueHelper.Get(const AParamName: string): TdxJSONValue;
var
  APair: TJSONPair;
begin
  APair := GetPair(AParamName);
  if APair <> nil then
    Result := APair.JsonValue
  else
    Result := nil;
end;

function TJSONValueHelper.GetChild(const AParentName, AParamName: string): TdxJSONValue;
var
  AValue: TdxJSONValue;
begin
  AValue := Self.Get(AParentName);
  if AValue <> nil then
    Result := AValue.Get(AParamName)
  else
    Result := nil;
end;

function TJSONValueHelper.GetChildParamValue(const AParentName, AParamName: string): string;
var
  AValue: TdxJSONValue;
begin
  AValue := Self.GetChild(AParentName, AParamName);
  if AValue <> nil then
    Result := AValue.Value
  else
    Result := '';
end;

function TJSONValueHelper.GetParamValue(const AParamName: string): string;
var
  AObject: TdxJSONValue;
begin
  AObject := Get(AParamName);
  if AObject <> nil then
    Result := AObject.Value
  else
    Result := '';
end;

function TJSONValueHelper.HasParam(const AParamName: string): Boolean;
begin
  Result := Get(AParamName) <> nil;
end;

function TJSONValueHelper.IsArray: Boolean;
begin
  Result := Self is TdxJSONArray;
end;

function TJSONValueHelper.IsTrue: Boolean;
begin
  Result := Self is TdxJSONTrue;
end;

function TJSONValueHelper.HasChildParam(const AParentName, AParamName: string): Boolean;
var
  AObject: TdxJSONValue;
begin
  AObject := Get(AParentName);
  Result := (AObject <> nil) and (AObject.HasParam(AParamName));
end;

{$IFNDEF DELPHIXE6}
  { TJSONObjectHelper }

  {$IFNDEF DELPHI2010}
    procedure TJSONObjectHelper.AddPair(const AName, AValue: UnicodeString);
    begin
      AddPair(TJSONPair.Create(AName, AValue));
    end;
  {$ENDIF}

  function TJSONObjectHelper.GetValue(const AParamName: string): TdxJSONValue;
  begin
    Result := TJSONValue(Self).Get(AParamName);
  end;

  function TJSONObjectHelper.GetCount: Integer;
  begin
    Result := Size;
  end;

  function TJSONObjectHelper.GetItem(Index: Integer): TJSONPair;
  begin
    Result := Get(Index);
  end;

  { TJSONArrayHelper }

  function TJSONArrayHelper.GetCount: Integer;
  begin
    Result := Size;
  end;

  function TJSONArrayHelper.GetItem(Index: Integer): TJSONValue;
  begin
    Result := Get(Index);
  end;
{$ENDIF}

{ TdxISO8601Helper }

class function TdxISO8601Helper.DateTimeToString(AValue: TDateTime): string;
const
  AIso8601UtcDateTimeFormat = 'yyyy-MM-dd"T"HH:mm:ss"Z"';
  AIso8601UtcDateFormat = 'yyyy-MM-dd';
begin
  if dxDateOf(AValue) = AValue then
    SysUtils.DateTimeToString(Result, AIso8601UtcDateFormat, AValue)
  else
    SysUtils.DateTimeToString(Result, AIso8601UtcDateTimeFormat, AValue);
end;

class function TdxISO8601Helper.StringToDateTime(const AValue: string; out AIsUtcTimeZone: Boolean): TDateTime;
var
  S: string;
  AYear, AMonth, ADay, AHours, AMinute, ASec, AMSec: Integer;
  AMSecValue: string;
  AOffsetHours, AOffsetMinute: Integer;
  AOffsetSign: Integer;
  AOffset: TTime;
  P: PChar;
begin
  AYear := 0;
  AMonth := 0;
  ADay := 0;
  AHours := 0;
  AMinute := 0;
  ASec := 0;
  AMSec := 0;
  AOffsetSign := 0;
  AIsUtcTimeZone := False;
  Result := InvalidDate;
  if AValue <> '' then
  begin
    S := AValue + #0;
    P := PChar(S);
    if not TryStrToInt(Copy(P, 1, 4), AYear) then
      Exit;
    Inc(P, 5);
    if not TryStrToInt(Copy(P, 1, 2), AMonth) then
      Exit;
    Inc(P, 3);
    if not TryStrToInt(Copy(P, 1, 2), ADay) then
      Exit;
    Inc(P, 3);
    if P^ = #0 then
      AOffset := 0
    else
    begin
      if not TryStrToInt(Copy(P, 1, 2), AHours) then
        Exit;
      Inc(P, 3);
      if not TryStrToInt(Copy(P, 1, 2), AMinute) then
        Exit;
      Inc(P, 3);
      if not TryStrToInt(Copy(P, 1, 2), ASec) then
        Exit;
      Inc(P, 2);
      if P^ = '.' then
      begin
        Inc(P);
        AMSecValue := '';
        while dxCharInSet(P^, ['0'..'9']) do
        begin
          AMSecValue := AMSecValue + P^;
          Inc(P);
        end;
        if not TryStrToInt(AMSecValue, AMSec) then
          Exit;
      end;
      AOffset := 0;
      if (P^ <> #0) then
      begin
        if P^ = '-' then
          AOffsetSign := -1
        else
          if P^ = '+' then
            AOffsetSign := 1;
        if (AOffsetSign = 0) and (P^ <> 'Z') then
          Exit;
        if AOffsetSign <> 0 then
        begin
          Inc(P);
          if not TryStrToInt(Copy(P, 1, 2), AOffsetHours) then
            Exit;
          Inc(P, 3);
          if dxCharInSet(P^, ['0'..'9']) then
          begin
            if not TryStrToInt(Copy(P, 1, 2), AOffsetMinute) then
              Exit;
          end;
          AOffset := EncodeTime(AOffsetHours, AOffsetMinute, 0, 0);
        end;
      end;
      AIsUtcTimeZone := (P^ = 'Z') or (AOffset <> 0);
    end;
    Result := EncodeDateTime(AYear, AMonth, ADay, AHours, AMinute, ASec, AMSec);
    Result := Result - AOffsetSign * AOffset;
  end;
end;

class function TdxISO8601Helper.StringToDateTime(const AValue: string): TDateTime;
var
  AIsUtcTimeZone: Boolean;
begin
  Result := StringToDateTime(AValue, AIsUtcTimeZone);
end;

{ TdxWebTask }

constructor TdxWebTask.Create(AOwner: TdxWebTaskManager);
begin
  inherited Create;
  FOwner := AOwner;
  FOwnerLink := cxAddObjectLink(Owner);
end;

destructor TdxWebTask.Destroy;
begin
  cxRemoveObjectLink(FOwnerLink);
  inherited Destroy;
end;

procedure TdxWebTask.BeforeDestruction;
begin
  if (FOwnerLink.Ref <> nil) then
    Owner.Remove(Self);
  inherited BeforeDestruction;
end;

function TdxWebTask.IsEqual(const ATask: TdxWebTask): Boolean;
begin
  Result := ClassType = ATask.ClassType;
end;

procedure TdxWebTask.MainThreadSynchronize(AThreadProc: TThreadProcedure);
begin
  TThread.Synchronize(nil, AThreadProc);
end;

procedure TdxWebTask.OnComplete(AStatus: TdxTaskCompletedStatus);
begin
  if AStatus = TdxTaskCompletedStatus.Success then
    DoComplete;
end;

function TdxWebTask.Run(
  const ACancelStatus: TdxTaskCancelCallback): TdxTaskCompletedStatus;
begin
  if ACancelStatus then
    Exit(TdxTaskCompletedStatus.Cancelled);
  if not IsValid then
    Exit(TdxTaskCompletedStatus.Fail);
  Result := DoRun(ACancelStatus);
end;

procedure TdxWebTask.UpdateRequestParams;
begin
  FUserAgent := GetUserAgent;
  FHeader := GetHeader;
end;

function TdxWebTask.IsValid: Boolean;
begin
  Result := FOwnerLink.Ref <> nil;
end;

{ TdxWebTaskManager }

constructor TdxWebTaskManager.Create;
begin
  inherited Create;
  FTasks := TList<TdxWebTask>.Create;
end;

destructor TdxWebTaskManager.Destroy;
begin
  FreeAndNil(FTasks);
  inherited Destroy;
end;

procedure TdxWebTaskManager.BeforeDestruction;
begin
  FIsDestroying := True;
  inherited BeforeDestruction;
  cxClearObjectLinks(Self);
  CancelAllTasks;
end;

procedure TdxWebTaskManager.RunTask(const ATask: TdxWebTask);
begin
  FTasks.Add(ATask);
  ATask.TaskHandle := dxTasksDispatcher.Run(ATask);
end;

procedure TdxWebTaskManager.CancelTask(const ATask: TdxWebTask);
begin
  FTasks.Remove(ATask);
  dxTasksDispatcher.Cancel(ATask.TaskHandle);
end;

procedure TdxWebTaskManager.ForEach(AProc: TForEachProcRef);
var
  ATask: TdxWebTask;
begin
  for ATask in Tasks do
    AProc(ATask);
end;

function TdxWebTaskManager.HasSameTask(const ATask: TdxWebTask): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := Tasks.Count - 1 downto 0 do
  begin
    Result := Tasks[I].IsEqual(ATask);
    if Result then
      Exit;
  end;
end;

procedure TdxWebTaskManager.CancelAllTasks;
begin
  while FTasks.Count > 0 do
    CancelTask(FTasks.Last);
end;

procedure TdxWebTaskManager.Remove(const ATask: TdxWebTask);
begin
  FTasks.Remove(ATask)
end;

end.
