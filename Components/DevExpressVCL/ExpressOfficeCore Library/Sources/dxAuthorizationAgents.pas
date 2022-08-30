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

unit dxAuthorizationAgents;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, SysUtils, Classes, Forms, Windows,
  Generics.Defaults, Generics.Collections,
  OleCtrls, SHDocVw, dxWinInet,
  dxCore, dxCoreClasses, dxForms, cxClasses;

type
  { TdxAuthorizationForm }

  TdxAuthorizationForm = class(TdxForm)
  public const
    DefaultHeight: Integer = 500;
    DefaultWidth: Integer = 500;
  protected type
    TDocumentCompleteEvent = procedure (AWebBrowser: TWebBrowser; var AComplete: Boolean) of object;
    TNavigateCompleteEvent = procedure (const AUri: string; var AComplete: Boolean) of object;
  strict private
    FComplete: Boolean;
    FWebBrowser: TWebBrowser;
    FOnDocumentComplete: TDocumentCompleteEvent;
    FOnNavigateComplete: TNavigateCompleteEvent;
    procedure DocumentCompleteHandler(ASender: TObject; const pDisp: IDispatch; {$IFDEF DELPHI16}const{$ELSE}var{$ENDIF} URL: OleVariant);
    procedure NavigateCompleteHandler(ASender: TObject; const pDisp: IDispatch; {$IFDEF DELPHI16}const{$ELSE}var{$ENDIF} URL: OleVariant);
    procedure TitleChangedHandler(ASender: TObject; const Text: WideString);
  protected
    procedure DoComplete;
    procedure DoClose(var Action: TCloseAction); override;
    procedure Navigate(const AUri: string);
    property OnDocumentComplete: TDocumentCompleteEvent read FOnDocumentComplete write FOnDocumentComplete;
    property OnNavigateComplete: TNavigateCompleteEvent read FOnNavigateComplete write FOnNavigateComplete;
  public
    constructor CreateEx;
    destructor Destroy; override;
    function ShowModal: Integer; override;
    property WebBrowser: TWebBrowser read FWebBrowser;
  end;

  { TdxCustomAuthorizationAgent }

  TdxCustomAuthorizationAgent = class abstract(TComponent)
  public type
    TErrorEvent = procedure(Sender: TObject; const AErrorObject) of object;
  public const
    DefaultUserAgent: string = '';
  strict private
    FUserAgent: string;
    FIsAuthorized: Boolean;

    FOnFinishAuthorization: TNotifyEvent;
    FOnError: TErrorEvent;
    FOnStartAuthorization: TNotifyEvent;
    function IsUserAgentStored: Boolean;
  protected
    procedure DoFinishAuthorization; virtual; abstract;
    procedure DoError(const AErrorObject); virtual;
    function DoStartAuthorization: Boolean; virtual; abstract;
    function DoValidateAuthorization: Boolean; virtual; abstract;
    procedure Initialize; virtual;

    function IsDestroying: Boolean;
    function IsReady: Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;

    procedure FinishAuthorization;
    procedure StartAuthorization;
    procedure RestartAuthorization;
    procedure ValidateAuthorization;

    function GetAuthorizationHeader: string; virtual; abstract;

    property IsAuthorized: Boolean read FIsAuthorized;
  published
    property UserAgent: string read FUserAgent write FUserAgent stored IsUserAgentStored;
    property OnFinishAuthorization: TNotifyEvent read FOnFinishAuthorization write FOnFinishAuthorization;
    property OnError: TErrorEvent read FOnError write FOnError;
    property OnStartAuthorization: TNotifyEvent read FOnStartAuthorization write FOnStartAuthorization;
  end;
  TdxCustomAuthorizationAgentClass = class of TdxCustomAuthorizationAgent;

  { IdxOAuth2AuthorizationAgentScopeRequestor }

  IdxOAuth2AuthorizationAgentScopeRequestor = interface
    function GetScopes: TStringList;
  end;

  { TdxOAuth2AuthorizationAgent }

  TdxOAuth2AuthorizationAgent = class abstract(TdxCustomAuthorizationAgent)
  public type
    TReceiveAuthorizationCodeEvent = procedure(Sender: TObject; out AAuthorizationCode: string) of object;
    TReceiveAccessTokenEvent = procedure(Sender: TObject; out ATokenAccess, ATokenRefresh, ATokenType: string) of object;
    TGetClientSecretEvent = procedure(Sender: TObject; var AClientSecret: string) of object;
  public const
    DefaultRedirectUri: string = 'http://localhost';
    DefaultIncludeGrantedScopes: Boolean = True;
    DefaultTokenExpiresIn = 3600;

    HeaderContentType = 'Content-Type: application/x-www-form-urlencoded';
  strict private const
    FHeaderAuthorization = 'Authorization: %s %s';
    FJSONAccessTokenParamName = 'access_token';
    FJSONAccessTokenExpiresInParamName = 'expires_in';
    FJSONAccessTokenTypeParamName = 'token_type';
    FJSONRefreshTokenParamName = 'refresh_token';
  strict private
    FAuthorizationForm: TdxAuthorizationForm;

    FAccessToken: string;
    FAccessTokenExpiresIn: Integer;
    FAccessTokenType: string;
    FAuthorizationCode: string;
    FGetAccessTokenTime: Cardinal;
    FRefreshToken: string;

    FClientID: string;
    FClientSecret: string;
    FRedirectUri: string;
    FIncludeGrantedScopes: Boolean;

    FAdditionalScopes: TStrings;
    FCachedScopes: string;
    FScopeRequestors: TList<IdxOAuth2AuthorizationAgentScopeRequestor>;

    FOnGetClientSecret: TGetClientSecretEvent;
    FOnReceiveAuthorizationCode: TReceiveAuthorizationCodeEvent;
    FOnReceiveAccessToken: TReceiveAccessTokenEvent;
    procedure AuthorizationFormNavigateCompleteHandler(const AUri: string; var AComplete: Boolean);
    procedure SetAdditionalScopes(const Value: TStrings);
    procedure SetClientID(const Value: string);
  private
    function CreateAuthorizationForm: TdxAuthorizationForm; virtual;

    function HasAuthorizationCode: Boolean;
    function HasAccessToken: Boolean;
    function HasRefreshToken: Boolean;

    function DoReceiveAuthorizationCode: Boolean; virtual;
    function DoReceiveAccessToken: Boolean; overload; virtual;

    function ReceiveAuthorizationCode: Boolean;
    function ReceiveAccessToken: Boolean;

    function IsIncludeGrantedScopesStored: Boolean;
    function IsRedirectUriStored: Boolean;
    procedure SetAuthorizationCode(const Value: string);
    procedure SetRedirectUri(const Value: string);
    procedure SetIncludeGrantedScopes(const Value: Boolean);
    procedure SetClientSecret(const Value: string);
  protected
    function CanUseClientSecret: Boolean; virtual;
    procedure DoFinishAuthorization; override;
    function DoStartAuthorization: Boolean; override;
    function DoValidateAuthorization: Boolean; override;
    procedure Initialize; override;

    function DoGetClientSecret: string; virtual;
    function GetAuthEndPoint: string; virtual; abstract;
    function GetReceiveTokenEndPointParams: string; virtual; abstract;
    function GetRefreshTokenEndPointParams: string; virtual; abstract;
    function GetRevokeAccessTokenEndPoint: string; virtual; abstract;
    function GetTokenEndPointObjectName: string; virtual; abstract;
    function GetTokenEndPointServerName: string; virtual; abstract;

    function IsReady: Boolean; override;

    function GetHeader: string;
    function GetScopes: string;
    function GetScopeDelimiter: Char; virtual; abstract;
    procedure PrepareScopes(AList: TStrings); virtual;

    class function EscapeDataString(const AData: string): string; static;

    procedure Clear; virtual;
    procedure DoReceiveAccessToken(AJSONObject: TdxJSONObject); overload; virtual;
    procedure DoRefreshAccessToken(AJSONObject: TdxJSONObject); virtual;

    property AuthorizationCode: string read FAuthorizationCode;
    property IncludeGrantedScopes: Boolean read FIncludeGrantedScopes write SetIncludeGrantedScopes stored IsIncludeGrantedScopesStored;
    property OnReceiveAuthorizationCode: TReceiveAuthorizationCodeEvent read FOnReceiveAuthorizationCode write FOnReceiveAuthorizationCode;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Load(AAccessToken, ARefreshToken, AAccessTokenType: string);

    function GetAuthorizationHeader: string; override;
    function IsAccessTokenValid: Boolean;

    function RefreshAccessToken: Boolean;
    procedure RevokeAccessToken;
    procedure ValidateAccessToken;

    property AccessToken: string read FAccessToken;
    property AccessTokenExpiresIn: Integer read FAccessTokenExpiresIn;
    property AccessTokenType: string read FAccessTokenType;
    property RefreshToken: string read FRefreshToken;

    procedure RefreshScopes(ARestartAgent: Boolean);
    procedure RegisterScopeRequestor(const AIntf: IdxOAuth2AuthorizationAgentScopeRequestor);
    procedure UnregisterScopeRequestor(const AIntf: IdxOAuth2AuthorizationAgentScopeRequestor);
  published
    property AdditionalScopes: TStrings read FAdditionalScopes write SetAdditionalScopes;
    property ClientID: string read FClientID write SetClientID;
    property ClientSecret: string read FClientSecret write SetClientSecret stored False;
    property RedirectUri: string read FRedirectUri write SetRedirectUri stored IsRedirectUriStored;
    property OnGetClientSecret: TGetClientSecretEvent read FOnGetClientSecret write FOnGetClientSecret;
    property OnReceiveAccessToken: TReceiveAccessTokenEvent read FOnReceiveAccessToken write FOnReceiveAccessToken;
  end;
  TdxOAuth2AuthorizationAgentClass = class of TdxOAuth2AuthorizationAgent;

  { TdxGoogleAPIOAuth2AuthorizationAgent }

  TdxGoogleAPIOAuth2AuthorizationAgent = class(TdxOAuth2AuthorizationAgent)
  public const
    DefaultClientID: string = '';
    DefaultClientSecret: string = '';
  strict private const
  {$REGION 'strict private const'}
    FAuthEndPoint = 'https://accounts.google.com/o/oauth2/v2/auth?' +
      'scope=%s&' +
      'access_type=offline&' +
      'include_granted_scopes=true&' +
      'state=state_parameter_passthrough_value&' +
      'redirect_uri=%s&' +
      'response_type=code&' +
      'client_id=%s';
    FRevokeTokenAccessEndPoint = 'https://accounts.google.com/o/oauth2/revoke?token=%s';

    FTokenEndPointVerb = 'www.googleapis.com';
    FTokenEndPointObjectName = 'oauth2/v4/token';
    FReceiveTokenEndPointParams =
      'code=%s&' +
      'redirect_uri=%s&' +
      'client_id=%s&' +
      'client_secret=%s&' +
      'scope=%s&' +
      'grant_type=authorization_code';
    FRefreshTokenEndPointParams =
      'client_secret=%s&' +
      'grant_type=refresh_token&' +
      'refresh_token=%s&' +
      'client_id=%s';
  {$ENDREGION}
  strict private
    procedure AuthorizationFormDocumentCompleteHandler(AWebBrowser: TWebBrowser; var AComplete: Boolean);
  protected
    function CreateAuthorizationForm: TdxAuthorizationForm; override;
    procedure Initialize; override;

    function GetAuthEndPoint: string; override;
    function GetReceiveTokenEndPointParams: string; override;
    function GetRefreshTokenEndPointParams: string; override;
    function GetRevokeAccessTokenEndPoint: string; override;
    function GetTokenEndPointObjectName: string; override;
    function GetTokenEndPointServerName: string; override;
    function GetScopeDelimiter: Char; override;
  end;

  { TdxMicrosoftGraphAPIOAuth2AuthorizationAgent }

  TdxMicrosoftGraphAPIOAuth2AuthorizationAgent = class(TdxOAuth2AuthorizationAgent)
  public const
    DefaultClientID: string = '';
    DefaultClientSecret: string = '';
  strict private const
  {$REGION 'strict private const'}
    FAuthEndPoint = 'https://login.microsoftonline.com/common/oauth2/v2.0/authorize?' +
      'access_type=offline&' +
      'client_id=%s&' +
      'response_type=code&response_mode=query&' +
      'redirect_uri=%s&' +
      'scope=%s';
    FTokenEndPointVerb = 'login.microsoftonline.com';
    FTokenEndPointObjectName = 'common/oauth2/v2.0/token';
    FReceiveTokenEndPointParams =
      'code=%s&' +
      'redirect_uri=%s&' +
      'client_id=%s&' +
      'client_secret=%s&' +
      'scope=%s&' +
      'grant_type=authorization_code';
    FReceiveTokenWithoutClientSecretEndPointParams =
      'code=%s&' +
      'redirect_uri=%s&' +
      'client_id=%s&' +
      'scope=%s&' +
      'grant_type=authorization_code';
    FRefreshTokenEndPointParams =
      'client_id=%s&' +
      'grant_type=refresh_token&' +
      'scope=%s&' +
      'refresh_token=%s&' +
      'redirect_uri=%s&' +
      'client_secret=%s';
    FRefreshTokenWithoutClientSecretEndPointParams =
      'client_id=%s&' +
      'grant_type=refresh_token&' +
      'scope=%s&' +
      'refresh_token=%s&' +
      'redirect_uri=%s';
    FRevokeTokenAccessEndPoint = 'https://login.microsoftonline.com/common/oauth2/v2.0/revoke?token=%s';
  {$ENDREGION}
  protected
    function CanUseClientSecret: Boolean; override;
    procedure Initialize; override;

    function GetAuthEndPoint: string; override;
    function GetReceiveTokenEndPointParams: string; override;
    function GetRefreshTokenEndPointParams: string; override;
    function GetRevokeAccessTokenEndPoint: string; override;
    function GetTokenEndPointObjectName: string; override;
    function GetTokenEndPointServerName: string; override;
    function GetScopeDelimiter: Char; override;
    procedure PrepareScopes(AList: TStrings); override;
  end;

  { TdxAuthorizationAgentUserInfo }

  TdxAuthorizationAgentUserInfoClass = class of TdxAuthorizationAgentUserInfo;
  TdxAuthorizationAgentUserInfo = class abstract(TcxIUnknownObject)
  strict private
    FAuthorizationAgent: TdxCustomAuthorizationAgent;
  strict private class var
    FUserInfoClassDictionary: TDictionary<TdxCustomAuthorizationAgentClass, TdxAuthorizationAgentUserInfoClass>;
  protected
    FDisplayName: string;
    FMail: string;
    procedure DoUpdateInfo; virtual; abstract;

    class procedure Initialize; static;
    class procedure Finalize; static;
    class procedure Register(AUserInfoClass: TdxAuthorizationAgentUserInfoClass; AAuthorizationAgentClass: TdxCustomAuthorizationAgentClass); static;
  public
    constructor Create(AAuthorizationAgent: TdxCustomAuthorizationAgent);

    class function GetUserInfo(AAuthorizationAgent: TdxCustomAuthorizationAgent): TdxAuthorizationAgentUserInfo; static;

    procedure UpdateInfo;

    property AuthorizationAgent: TdxCustomAuthorizationAgent read FAuthorizationAgent;
    property DisplayName: string read FDisplayName;
    property Mail: string read FMail;
  end;

  { TdxOAuth2AuthorizationAgentUserInfo }

  TdxOAuth2AuthorizationAgentUserInfo = class abstract(TdxAuthorizationAgentUserInfo,
    IdxOAuth2AuthorizationAgentScopeRequestor)
  strict private
    function GetAuthorizationAgent: TdxOAuth2AuthorizationAgent;
  protected
    procedure DoUpdateInfo; override;
    function GetUpdateUserInfoUri: string; virtual; abstract;
    procedure JSONToUserInfo(AObject: TdxJSONObject); virtual; abstract;
  {$REGION 'IdxOAuth2AuthorizationAgentScopeRequestor'}
    function GetScopes: TStringList; virtual; abstract;
  {$ENDREGION}
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    property AuthorizationAgent: TdxOAuth2AuthorizationAgent read GetAuthorizationAgent;
  end;

  { TdxGoogleAPIOAuth2AuthorizationAgentUserInfo }

  TdxGoogleAPIOAuth2AuthorizationAgentUserInfo = class(TdxOAuth2AuthorizationAgentUserInfo)
  protected
    function GetUpdateUserInfoUri: string; override;
    procedure JSONToUserInfo(AObject: TdxJSONObject); override;
    function GetScopes: TStringList; override;
  end;

  { TdxMicrosoftGraphAPIOAuth2AuthorizationAgentUserInfo }

  TdxMicrosoftGraphAPIOAuth2AuthorizationAgentUserInfo = class(TdxOAuth2AuthorizationAgentUserInfo)
  protected
    function GetUpdateUserInfoUri: string; override;
    procedure JSONToUserInfo(AObject: TdxJSONObject); override;
    function GetScopes: TStringList; override;
  end;

implementation

uses
  Controls, Messages, MSHTML,
  dxUriRecord, dxStringHelper;

{ TdxAuthorizationForm }

constructor TdxAuthorizationForm.CreateEx;
begin
  inherited CreateNew(nil);
  BorderIcons := [biSystemMenu];
  FWebBrowser := TWebBrowser.Create(Self);
  FWebBrowser.Align := alClient;
  FWebBrowser.OnDocumentComplete := DocumentCompleteHandler;
  FWebBrowser.OnTitleChange := TitleChangedHandler;
  FWebBrowser.OnNavigateComplete2 := NavigateCompleteHandler;
  FWebBrowser.SetParentComponent(Self);
  Width := DefaultWidth;
  Height := DefaultHeight;
end;

destructor TdxAuthorizationForm.Destroy;
begin
  FreeAndNil(FWebBrowser);
  inherited Destroy;
end;

procedure TdxAuthorizationForm.DoComplete;
begin
  OnDocumentComplete := nil;
  OnNavigateComplete := nil;
  ModalResult := mrOk;
end;

procedure TdxAuthorizationForm.DoClose(var Action: TCloseAction);
begin
  Action := caFree;
  inherited DoClose(Action);
end;

procedure TdxAuthorizationForm.DocumentCompleteHandler(ASender: TObject;
  const pDisp: IDispatch; {$IFDEF DELPHI16}const{$ELSE}var{$ENDIF} URL: OleVariant);
begin
  if FComplete then
    Exit;
  if Assigned(OnDocumentComplete) then
    OnDocumentComplete(FWebBrowser, FComplete);
  if FComplete then
    DoComplete;
end;

procedure TdxAuthorizationForm.NavigateCompleteHandler(ASender: TObject;
  const pDisp: IDispatch; {$IFDEF DELPHI16}const{$ELSE}var{$ENDIF} URL: OleVariant);
begin
  if FComplete then
    Exit;
  if Assigned(OnNavigateComplete) then
    OnNavigateComplete(URL, FComplete);
  if FComplete then
    DoComplete;
end;

procedure TdxAuthorizationForm.TitleChangedHandler(ASender: TObject; const Text: WideString);
begin
  Caption := Text;
end;

procedure TdxAuthorizationForm.Navigate(const AUri: string);
begin
  FWebBrowser.Navigate(AUri);
end;

function TdxAuthorizationForm.ShowModal: Integer;
begin
  cxDialogsMetricsStore.InitDialog(Self);
  Result := inherited ShowModal;
  cxDialogsMetricsStore.StoreMetrics(Self);
end;

{ TdxCustomAuthorizationAgent }

constructor TdxCustomAuthorizationAgent.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Initialize;
end;

procedure TdxCustomAuthorizationAgent.FinishAuthorization;
begin
  if not IsAuthorized then
    Exit;
  DoFinishAuthorization;
  FIsAuthorized := False;
  CallNotify(FOnFinishAuthorization, Self);
end;

procedure TdxCustomAuthorizationAgent.StartAuthorization;
begin
  if IsAuthorized then
    Exit;
  FIsAuthorized := DoStartAuthorization;
  if IsAuthorized then
    CallNotify(FOnStartAuthorization, Self);
end;

procedure TdxCustomAuthorizationAgent.RestartAuthorization;
begin
  if IsAuthorized then
  begin
    FinishAuthorization;
    StartAuthorization;
  end;
end;

procedure TdxCustomAuthorizationAgent.ValidateAuthorization;
begin
  if not IsAuthorized then
    Exit;
  if not DoValidateAuthorization then
    FinishAuthorization;
end;

procedure TdxCustomAuthorizationAgent.DoError(const AErrorObject);
begin
  if Assigned(FOnError) then
    FOnError(Self, AErrorObject);
end;

procedure TdxCustomAuthorizationAgent.Initialize;
begin
  FUserAgent := DefaultUserAgent;
end;

function TdxCustomAuthorizationAgent.IsDestroying: Boolean;
begin
  Result := csDestroying in ComponentState;
end;

function TdxCustomAuthorizationAgent.IsReady: Boolean;
begin
  Result := True;
end;

function TdxCustomAuthorizationAgent.IsUserAgentStored: Boolean;
begin
  Result := FUserAgent <> DefaultUserAgent;
end;

{ TdxOAuth2AuthorizationAgent }

constructor TdxOAuth2AuthorizationAgent.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FScopeRequestors := TList<IdxOAuth2AuthorizationAgentScopeRequestor>.Create;
  FAdditionalScopes := TStringList.Create;
end;

destructor TdxOAuth2AuthorizationAgent.Destroy;
begin
  FreeAndNil(FAdditionalScopes);
  FreeAndNil(FScopeRequestors);
  inherited Destroy;
end;

procedure TdxOAuth2AuthorizationAgent.Load(AAccessToken, ARefreshToken, AAccessTokenType: string);
begin
  FinishAuthorization;
  FAccessToken := AAccessToken;
  FRefreshToken := ARefreshToken;
  FAccessTokenType := AAccessTokenType;
  FAccessTokenExpiresIn := 0;
  FGetAccessTokenTime := 0;
end;

function TdxOAuth2AuthorizationAgent.GetAuthorizationHeader: string;
begin
  Result := Format(FHeaderAuthorization, [AccessTokenType, EscapeDataString(AccessToken)]);
end;

function TdxOAuth2AuthorizationAgent.RefreshAccessToken: Boolean;
var
  AParams: TBytes;
  AJSONObject: TdxJSONObject;
begin
  Result := False;
  if not HasRefreshToken then
    Exit;
  FAccessToken := '';

  Result := False;
  AParams := TEncoding.UTF8.GetBytes(GetRefreshTokenEndPointParams);
  AJSONObject := TdxHttpHelper.PostRequest(UserAgent, GetTokenEndPointServerName, GetTokenEndPointObjectName, GetHeader, AParams);
  if AJSONObject <> nil then
  try
    FAccessToken := AJSONObject.GetParamValue(FJSONAccessTokenParamName);
    if not TryStrToInt(AJSONObject.GetParamValue(FJSONAccessTokenExpiresInParamName), FAccessTokenExpiresIn) then
      FAccessTokenExpiresIn := DefaultTokenExpiresIn;
    FAccessTokenType := AJSONObject.GetParamValue(FJSONAccessTokenTypeParamName);
    if AJSONObject.HasParam(FJSONRefreshTokenParamName) then
      FRefreshToken := AJSONObject.GetParamValue(FJSONRefreshTokenParamName);
    DoRefreshAccessToken(AJSONObject);
    Result := HasAccessToken;
    if Result then
      FGetAccessTokenTime := GetTickCount;
  finally
    AJSONObject.Free;
  end;
end;

procedure TdxOAuth2AuthorizationAgent.RevokeAccessToken;
var
  AUri: string;
  AJSONObject: TdxJSONObject;
begin
  if not IsAuthorized then
    Exit;
  if not HasAccessToken then
    Exit;
  AUri := GetRevokeAccessTokenEndPoint;
  AJSONObject := TdxHttpHelper.GetRequest(UserAgent, AUri, GetHeader);
  try
  finally
    AJSONObject.Free;
  end;
  FinishAuthorization;
end;

procedure TdxOAuth2AuthorizationAgent.Clear;
begin
  FAccessToken := '';
  FAuthorizationCode := '';
  FAccessTokenExpiresIn := DefaultTokenExpiresIn;
  FRefreshToken := '';
end;

function TdxOAuth2AuthorizationAgent.CanUseClientSecret: Boolean;
begin
  Result := True;
end;

procedure TdxOAuth2AuthorizationAgent.DoFinishAuthorization;
begin
  Clear;
end;

function TdxOAuth2AuthorizationAgent.DoStartAuthorization: Boolean;
begin
  if HasAccessToken then
    ValidateAccessToken;
  Result := IsAccessTokenValid or ReceiveAccessToken;
end;

function TdxOAuth2AuthorizationAgent.DoValidateAuthorization: Boolean;
begin
  ValidateAccessToken;
  Result := IsAccessTokenValid;
end;

procedure TdxOAuth2AuthorizationAgent.Initialize;
begin
  inherited Initialize;
  FRedirectUri := DefaultRedirectUri;
  FIncludeGrantedScopes := DefaultIncludeGrantedScopes;
end;

function TdxOAuth2AuthorizationAgent.GetScopes: string;
var
  I, J: Integer;
  AResult: TStringList;
  AScopes: TStringList;
  S: string;
begin
  if FCachedScopes <> '' then
    Exit(FCachedScopes);
  AResult := TStringList.Create;
  try
    AResult.Assign(AdditionalScopes);
    for I := 0 to FScopeRequestors.Count - 1 do
    begin
      AScopes := FScopeRequestors[I].GetScopes;
      try
        for J := 0 to AScopes.Count - 1 do
        begin
          S := Trim(AScopes[J]);
          if (Length(S) > 0) and (AResult.IndexOf(S) = -1) then
            AResult.Add(S);
        end;
        AScopes.Sort;
      finally
        AScopes.Free;
      end;
    end;
    PrepareScopes(AResult);
    AResult.Delimiter := GetScopeDelimiter;
    FCachedScopes := EscapeDataString(AResult.DelimitedText);
  finally
    AResult.Free;
  end;
  Result := FCachedScopes;
end;

procedure TdxOAuth2AuthorizationAgent.PrepareScopes(AList: TStrings);
begin
// do nothing
end;

function TdxOAuth2AuthorizationAgent.DoGetClientSecret: string;
begin
  Result := ClientSecret;
  if Assigned(OnGetClientSecret) then
    OnGetClientSecret(Self, Result);
end;

function TdxOAuth2AuthorizationAgent.IsReady: Boolean;
begin
  Result := inherited IsReady and (ClientID <> '') and
    (not CanUseClientSecret or (ClientSecret <> '') or Assigned(OnGetClientSecret)) and
    ((FScopeRequestors.Count > 0) or (AdditionalScopes.Count > 0));
end;

function TdxOAuth2AuthorizationAgent.GetHeader: string;
begin
  Result := HeaderContentType;
end;

class function TdxOAuth2AuthorizationAgent.EscapeDataString(const AData: string): string;
begin
  Result := TdxUri.EscapeDataString(AData);
end;

procedure TdxOAuth2AuthorizationAgent.DoReceiveAccessToken(AJSONObject: TdxJSONObject);
begin
// do nothing
end;

procedure TdxOAuth2AuthorizationAgent.DoRefreshAccessToken(AJSONObject: TdxJSONObject);
begin
// do nothing
end;

procedure TdxOAuth2AuthorizationAgent.ValidateAccessToken;
begin
  if not IsAccessTokenValid then
    RefreshAccessToken;
end;

procedure TdxOAuth2AuthorizationAgent.RefreshScopes(ARestartAgent: Boolean);
var
  AOldScopes: string;
begin
  if IsDestroying then
    Exit;
  if IsAuthorized then
    AOldScopes := GetScopes;
  FCachedScopes := '';
  if not IsAuthorized then
    Exit;
  if AOldScopes <> GetScopes then
    if ARestartAgent then
      RestartAuthorization;
end;

procedure TdxOAuth2AuthorizationAgent.RegisterScopeRequestor(const AIntf: IdxOAuth2AuthorizationAgentScopeRequestor);
begin
  FScopeRequestors.Add(AIntf);
  RefreshScopes(False);
end;

procedure TdxOAuth2AuthorizationAgent.UnregisterScopeRequestor(const AIntf: IdxOAuth2AuthorizationAgentScopeRequestor);
begin
  if FScopeRequestors = nil then
    Exit;
  FScopeRequestors.Remove(AIntf);
  RefreshScopes(False);
end;

function TdxOAuth2AuthorizationAgent.IsAccessTokenValid: Boolean;
begin
  Result := HasAccessToken and ((GetTickCount - FGetAccessTokenTime) / 1000 < AccessTokenExpiresIn);
end;

function TdxOAuth2AuthorizationAgent.CreateAuthorizationForm: TdxAuthorizationForm;
begin
  Result := TdxAuthorizationForm.CreateEx;
  Result.OnNavigateComplete := AuthorizationFormNavigateCompleteHandler;
end;

function TdxOAuth2AuthorizationAgent.HasAuthorizationCode: Boolean;
begin
  Result := AuthorizationCode <> '';
end;

function TdxOAuth2AuthorizationAgent.HasAccessToken: Boolean;
begin
  Result := AccessToken <> '';
end;

function TdxOAuth2AuthorizationAgent.HasRefreshToken: Boolean;
begin
  Result := RefreshToken <> '';
end;

function TdxOAuth2AuthorizationAgent.DoReceiveAuthorizationCode: Boolean;
begin
  if Assigned(FOnReceiveAuthorizationCode) then
    FOnReceiveAuthorizationCode(Self, FAuthorizationCode);
  Result := HasAuthorizationCode;
end;

function TdxOAuth2AuthorizationAgent.DoReceiveAccessToken: Boolean;
begin
  if Assigned(FOnReceiveAccessToken) then
    FOnReceiveAccessToken(Self, FAccessToken, FRefreshToken, FAccessTokenType);
  Result := HasAccessToken;
end;

function TdxOAuth2AuthorizationAgent.ReceiveAuthorizationCode: Boolean;
var
  AUri: string;
begin
  if HasAuthorizationCode then
    Exit(True);
  if DoReceiveAuthorizationCode then
    Exit(HasAuthorizationCode);
  try
    AUri := GetAuthEndPoint;
    FAuthorizationForm := CreateAuthorizationForm;
    FAuthorizationForm.Navigate(AUri);
    FAuthorizationForm.ShowModal;
    FAuthorizationForm := nil;
    Result := HasAuthorizationCode;
  except
    Result := False;
  end;
end;

function TdxOAuth2AuthorizationAgent.ReceiveAccessToken: Boolean;
var
  AParams: TBytes;
  AJSONObject: TdxJSONObject;
begin
  FAccessToken := '';

  Result := False;
  if DoReceiveAccessToken then
    Exit(True);
  if not HasAuthorizationCode and not ReceiveAuthorizationCode then
    Exit;
  AParams := TEncoding.UTF8.GetBytes(GetReceiveTokenEndPointParams);
  AJSONObject := TdxHttpHelper.PostRequest(UserAgent, GetTokenEndPointServerName, GetTokenEndPointObjectName, GetHeader, AParams);
  if AJSONObject <> nil then
  try
    FAccessToken := AJSONObject.GetParamValue(FJSONAccessTokenParamName);
    FRefreshToken := AJSONObject.GetParamValue(FJSONRefreshTokenParamName);
    if not TryStrToInt(AJSONObject.GetParamValue(FJSONAccessTokenExpiresInParamName), FAccessTokenExpiresIn) then
      FAccessTokenExpiresIn := DefaultTokenExpiresIn;
    FAccessTokenType := AJSONObject.GetParamValue(FJSONAccessTokenTypeParamName);
    DoReceiveAccessToken(AJSONObject);
    Result := HasAccessToken;
    if Result then
      FGetAccessTokenTime := GetTickCount
    else
      DoError(AJSONObject);
  finally
    AJSONObject.Free;
  end;
end;

function TdxOAuth2AuthorizationAgent.IsIncludeGrantedScopesStored: Boolean;
begin
  Result := IncludeGrantedScopes <> DefaultIncludeGrantedScopes;
end;

function TdxOAuth2AuthorizationAgent.IsRedirectUriStored: Boolean;
begin
  Result := RedirectUri <> DefaultRedirectUri;
end;

procedure TdxOAuth2AuthorizationAgent.AuthorizationFormNavigateCompleteHandler(const AUri: string; var AComplete: Boolean);

  function ParseAuthorizationCode(const S: string): string;
  var
    AStrings: TStringList;
    I: Integer;
  begin
    Result := '';
    AStrings := TStringList.Create;
    try
      AStrings.Delimiter := '&';
      AStrings.DelimitedText := S;
      for I := 0 to AStrings.Count - 1 do
      begin
        if TdxStringHelper.StartsWith(AStrings[I], 'code=') then
        begin
          Result := Copy(AStrings[I], 6, Length(AStrings[I]));
          Break;
        end;
      end;
    finally
      AStrings.Free;
    end;
  end;

var
  AUriRecord: TdxURI;
begin
  AComplete := TdxStringHelper.StartsWith(AUri, RedirectUri);
  if AComplete then
  begin
    AUriRecord := TdxURI.Create(AUri);
    FAuthorizationCode := ParseAuthorizationCode(AUriRecord.Params);
  end;
end;

procedure TdxOAuth2AuthorizationAgent.SetAdditionalScopes(const Value: TStrings);
begin
  FAdditionalScopes.Assign(Value);
end;

procedure TdxOAuth2AuthorizationAgent.SetClientID(const Value: string);
begin
  if FClientID <> Value then
  begin
    FinishAuthorization;
    FClientID := Value;
  end;
end;

procedure TdxOAuth2AuthorizationAgent.SetClientSecret(const Value: string);
begin
  if FClientSecret <> Value then
  begin
    FinishAuthorization;
    FClientSecret := Value;
  end;
end;

procedure TdxOAuth2AuthorizationAgent.SetIncludeGrantedScopes(
  const Value: Boolean);
begin
  if FIncludeGrantedScopes <> Value then
  begin
    FinishAuthorization;
    FIncludeGrantedScopes := Value;
  end;
end;

procedure TdxOAuth2AuthorizationAgent.SetAuthorizationCode(const Value: string);
begin
  FAuthorizationCode := Value;
end;

procedure TdxOAuth2AuthorizationAgent.SetRedirectUri(const Value: string);
begin
  if FRedirectUri <> Value then
  begin
    FinishAuthorization;
    FRedirectUri := Value;
  end;
end;

{ TdxGoogleAPIOAuth2AuthorizationAgent }

function TdxGoogleAPIOAuth2AuthorizationAgent.CreateAuthorizationForm: TdxAuthorizationForm;
var
  AUri: TdxURI;
begin
  Result := inherited CreateAuthorizationForm;
  AUri := TdxURI.Create(RedirectUri);
  if not AUri.IsWebScheme then
    Result.OnDocumentComplete := AuthorizationFormDocumentCompleteHandler;
end;

procedure TdxGoogleAPIOAuth2AuthorizationAgent.Initialize;
begin
  inherited Initialize;
  ClientID := DefaultClientID;
  ClientSecret := DefaultClientSecret;
end;

function TdxGoogleAPIOAuth2AuthorizationAgent.GetAuthEndPoint: string;
begin
  Result :=  Format(FAuthEndPoint,
    [GetScopes, EscapeDataString(RedirectUri), EscapeDataString(ClientID)]);
end;

function TdxGoogleAPIOAuth2AuthorizationAgent.GetReceiveTokenEndPointParams: string;
begin
  Result := Format(FReceiveTokenEndPointParams, [EscapeDataString(AuthorizationCode), EscapeDataString(RedirectUri),
    EscapeDataString(ClientID), EscapeDataString(DoGetClientSecret), GetScopes])
end;

function TdxGoogleAPIOAuth2AuthorizationAgent.GetRefreshTokenEndPointParams: string;
begin
  Result := Format(FRefreshTokenEndPointParams,
    [EscapeDataString(DoGetClientSecret), EscapeDataString(RefreshToken), EscapeDataString(ClientID)])
end;

function TdxGoogleAPIOAuth2AuthorizationAgent.GetRevokeAccessTokenEndPoint: string;
begin
  Result :=  Format(FRevokeTokenAccessEndPoint, [EscapeDataString(AccessToken)]);
end;

function TdxGoogleAPIOAuth2AuthorizationAgent.GetTokenEndPointObjectName: string;
begin
  Result := FTokenEndPointObjectName;
end;

function TdxGoogleAPIOAuth2AuthorizationAgent.GetTokenEndPointServerName: string;
begin
  Result := FTokenEndPointVerb;
end;

function TdxGoogleAPIOAuth2AuthorizationAgent.GetScopeDelimiter: Char;
begin
  Result := ' ';
end;

procedure TdxGoogleAPIOAuth2AuthorizationAgent.AuthorizationFormDocumentCompleteHandler(AWebBrowser: TWebBrowser; var AComplete: Boolean);
var
  ACode: IHTMLElement;
  ADocument: IHTMLDocument3;
begin
  ADocument := AWebBrowser.Document as IHTMLDocument3;
  try
    ACode := ADocument.getElementById('code');
    AComplete := ACode <> nil;
    if AComplete then
      SetAuthorizationCode(ACode.getAttribute('value', 0));
  finally
    ADocument := nil;
  end;
end;

{ TdxMicrosoftGraphAPIOAuth2AuthorizationAgent }

function TdxMicrosoftGraphAPIOAuth2AuthorizationAgent.GetAuthEndPoint: string;
begin
  Result := Format(FAuthEndPoint, [ClientID, EscapeDataString(RedirectUri), GetScopes]);
end;

function TdxMicrosoftGraphAPIOAuth2AuthorizationAgent.GetReceiveTokenEndPointParams: string;
begin
  if CanUseClientSecret then
    Result := Format(FReceiveTokenEndPointParams, [EscapeDataString(AuthorizationCode), EscapeDataString(RedirectUri),
      EscapeDataString(ClientID), EscapeDataString(DoGetClientSecret), GetScopes])
  else
    Result := Format(FReceiveTokenWithoutClientSecretEndPointParams, [EscapeDataString(AuthorizationCode), EscapeDataString(RedirectUri),
      EscapeDataString(ClientID), GetScopes]);
end;

function TdxMicrosoftGraphAPIOAuth2AuthorizationAgent.GetRefreshTokenEndPointParams: string;
begin
  if CanUseClientSecret then
    Result := Format(FRefreshTokenEndPointParams,
      [EscapeDataString(ClientID), GetScopes, EscapeDataString(RefreshToken), EscapeDataString(RedirectUri), EscapeDataString(DoGetClientSecret)])
  else
    Result := Format(FRefreshTokenWithoutClientSecretEndPointParams,
      [EscapeDataString(ClientID), GetScopes, EscapeDataString(RefreshToken), EscapeDataString(RedirectUri)])
end;

function TdxMicrosoftGraphAPIOAuth2AuthorizationAgent.GetRevokeAccessTokenEndPoint: string;
begin
  Result := Format(FRevokeTokenAccessEndPoint, [EscapeDataString(AccessToken)]);
end;

function TdxMicrosoftGraphAPIOAuth2AuthorizationAgent.GetTokenEndPointObjectName: string;
begin
  Result := FTokenEndPointObjectName;
end;

function TdxMicrosoftGraphAPIOAuth2AuthorizationAgent.GetTokenEndPointServerName: string;
begin
  Result := FTokenEndPointVerb;
end;

function TdxMicrosoftGraphAPIOAuth2AuthorizationAgent.GetScopeDelimiter: Char;
begin
  Result := ' ';
end;

procedure TdxMicrosoftGraphAPIOAuth2AuthorizationAgent.Initialize;
begin
  inherited Initialize;
  ClientID := DefaultClientID;
  ClientSecret := DefaultClientSecret;
end;

procedure TdxMicrosoftGraphAPIOAuth2AuthorizationAgent.PrepareScopes(AList: TStrings);
begin
  AList.Insert(0, 'offline_access');
end;

function TdxMicrosoftGraphAPIOAuth2AuthorizationAgent.CanUseClientSecret: Boolean;
var
  AUri: TdxURI;
begin
  AUri := TdxURI.Create(RedirectUri);
  Result := AUri.IsWebScheme;
end;

{ TdxAuthorizationAgentUserInfo }

constructor TdxAuthorizationAgentUserInfo.Create(AAuthorizationAgent: TdxCustomAuthorizationAgent);
begin
  inherited Create;
  FAuthorizationAgent := AAuthorizationAgent;
end;

class function TdxAuthorizationAgentUserInfo.GetUserInfo(AAuthorizationAgent: TdxCustomAuthorizationAgent): TdxAuthorizationAgentUserInfo;
var
  AClass: TdxAuthorizationAgentUserInfoClass;
begin
  if not FUserInfoClassDictionary.TryGetValue(TdxCustomAuthorizationAgentClass(AAuthorizationAgent.ClassType), AClass) then
    Exit(nil);
  Result := AClass.Create(AAuthorizationAgent);
end;

procedure TdxAuthorizationAgentUserInfo.UpdateInfo;
begin
  AuthorizationAgent.StartAuthorization;
  if not AuthorizationAgent.IsAuthorized then
    Exit;
  DoUpdateInfo;
end;

class procedure TdxAuthorizationAgentUserInfo.Initialize;
begin
  FUserInfoClassDictionary := TDictionary<TdxCustomAuthorizationAgentClass, TdxAuthorizationAgentUserInfoClass>.Create;
end;

class procedure TdxAuthorizationAgentUserInfo.Finalize;
begin
  FreeAndNil(FUserInfoClassDictionary);
end;

class procedure TdxAuthorizationAgentUserInfo.Register(AUserInfoClass: TdxAuthorizationAgentUserInfoClass; AAuthorizationAgentClass: TdxCustomAuthorizationAgentClass);
begin
  FUserInfoClassDictionary.Add(AAuthorizationAgentClass, AUserInfoClass);
end;

{ TdxOAuth2AuthorizationAgentUserInfo }

procedure TdxOAuth2AuthorizationAgentUserInfo.AfterConstruction;
begin
  inherited AfterConstruction;
  AuthorizationAgent.RegisterScopeRequestor(Self);
end;

procedure TdxOAuth2AuthorizationAgentUserInfo.BeforeDestruction;
begin
  AuthorizationAgent.UnregisterScopeRequestor(Self);
  inherited BeforeDestruction;
end;

procedure TdxOAuth2AuthorizationAgentUserInfo.DoUpdateInfo;
var
  AObject: TdxJSONObject;
  AHeader: string;
begin
  AHeader := TdxOAuth2AuthorizationAgent.HeaderContentType + #13#10 +
    AuthorizationAgent.GetAuthorizationHeader;
  AObject := TdxHttpHelper.GetRequest(AuthorizationAgent.UserAgent, GetUpdateUserInfoUri, AHeader);
  if Assigned(AObject) then
  try
    JSONToUserInfo(AObject);
  finally
    AObject.Free;
  end;
end;

function TdxOAuth2AuthorizationAgentUserInfo.GetAuthorizationAgent: TdxOAuth2AuthorizationAgent;
begin
  Result := TdxOAuth2AuthorizationAgent(inherited AuthorizationAgent);
end;

{ TdxGoogleAPIOAuth2AuthorizationAgentUserInfo }

function TdxGoogleAPIOAuth2AuthorizationAgentUserInfo.GetScopes: TStringList;
begin
  Result := TStringList.Create;
  Result.Insert(0, 'profile');
  Result.Insert(0, 'email');
end;

function TdxGoogleAPIOAuth2AuthorizationAgentUserInfo.GetUpdateUserInfoUri: string;
begin
  Result := 'https://www.googleapis.com/userinfo/v2/me';
end;

procedure TdxGoogleAPIOAuth2AuthorizationAgentUserInfo.JSONToUserInfo(
  AObject: TdxJSONObject);
begin
  FDisplayName := AObject.GetParamValue('name');
  FMail := AObject.GetParamValue('email');
end;

{ TdxMicrosoftGraphAPIOAuth2AuthorizationAgentUserInfo }

function TdxMicrosoftGraphAPIOAuth2AuthorizationAgentUserInfo.GetScopes: TStringList;
begin
  Result := TStringList.Create;
  Result.Insert(0, 'User.Read');
end;

function TdxMicrosoftGraphAPIOAuth2AuthorizationAgentUserInfo.GetUpdateUserInfoUri: string;
begin
  Result := 'https://graph.microsoft.com/v1.0/me/';
end;

procedure TdxMicrosoftGraphAPIOAuth2AuthorizationAgentUserInfo.JSONToUserInfo(
  AObject: TdxJSONObject);
begin
  FDisplayName := AObject.GetParamValue('displayName');
  FMail := AObject.GetParamValue('mail');
end;

initialization
  GroupDescendentsWith(TdxCustomAuthorizationAgent, TControl);
  TdxAuthorizationAgentUserInfo.Initialize;
  TdxAuthorizationAgentUserInfo.Register(TdxGoogleAPIOAuth2AuthorizationAgentUserInfo, TdxGoogleAPIOAuth2AuthorizationAgent);
  TdxAuthorizationAgentUserInfo.Register(TdxMicrosoftGraphAPIOAuth2AuthorizationAgentUserInfo, TdxMicrosoftGraphAPIOAuth2AuthorizationAgent);

finalization
  TdxAuthorizationAgentUserInfo.Finalize;

end.
