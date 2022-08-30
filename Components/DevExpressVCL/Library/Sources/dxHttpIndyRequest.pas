{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           Express Cross Platform Library controls                  }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSCROSSPLATFORMLIBRARY AND ALL   }
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

unit dxHttpIndyRequest;

interface

{$I cxVer.inc}

uses
  SysUtils, Classes, IdHTTP, IdComponent, IdHTTPHeaderInfo, Forms, dxHttpRequest;

type
  TdxHttpIndyRequest = class(TdxHttpRequestHandler)
  private
    FHasErrors: Boolean;
    FIsCancelled: Boolean;
    procedure DoWork(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);
  public
    function Get(const AUri: string; AResponseContent: TStream): Boolean; override;
    function HasErrors: Boolean; override;
    function IsCancelled: Boolean; override;
  end;

function GetContent(const AUri: string; AResponseContent: TStream): Boolean;
function dxParamsEncode(const ASrc: string): string;
function dxHttpIndyDefaultProxyInfo: TIdProxyConnectionInfo;

implementation

uses
  IdURI;

var
  FHttpDefaultProxyInfo: TIdProxyConnectionInfo;

function dxHttpIndyDefaultProxyInfo: TIdProxyConnectionInfo;
begin
  if FHttpDefaultProxyInfo = nil then
    FHttpDefaultProxyInfo := TIdProxyConnectionInfo.Create;
  Result := FHttpDefaultProxyInfo;
end;

procedure SetUserAgent(AHTTP: TIdHTTP);
begin
  AHTTP.Request.UserAgent := StringReplace(AHTTP.Request.UserAgent, 'Indy Library', Application.Title, []);
end;

procedure SetProxyParams(AHTTP: TIdHTTP);
begin
  if FHttpDefaultProxyInfo <> nil then
    AHTTP.ProxyParams.Assign(FHttpDefaultProxyInfo);
end;

function GetContent(const AUri: string; AResponseContent: TStream): Boolean;
var
  AHTTP: TIdHTTP;
begin
  Result := True;
  try
    AHTTP := TIdHTTP.Create(nil);
    try
      SetUserAgent(AHTTP);
      SetProxyParams(AHTTP);
      AHTTP.Get(AUri, AResponseContent);
    finally
      AHTTP.Free;
    end;
  except
    on Exception do
      Result := False;
  end;
end;

function dxParamsEncode(const ASrc: string): string;
begin
  Result := TIdURI.ParamsEncode(ASrc);
end;

function URLEncode(const ASrc: string): string;
begin
  Result := TIdURI.URLEncode(ASrc);
end;

{ TdxHttpIndyRequest }

function TdxHttpIndyRequest.Get(const AUri: string;
  AResponseContent: TStream): Boolean;
var
  AHTTP: TIdHTTP;
begin
  FHasErrors := False;
  FIsCancelled := False;
  try
    AHTTP := TIdHTTP.Create(nil);
    try
      SetUserAgent(AHTTP);
      SetProxyParams(AHTTP);
      AHTTP.OnWork := DoWork;
      AHTTP.Get(dxURLEncode(AUri), AResponseContent);
      AHTTP.OnWork := nil;
    finally
      AHTTP.Free;
    end;
  except
    on Exception do
      FHasErrors := True;
  end;
  Result := not FHasErrors and not FIsCancelled;
end;

function TdxHttpIndyRequest.HasErrors: Boolean;
begin
  Result := FHasErrors;
end;

function TdxHttpIndyRequest.IsCancelled: Boolean;
begin
  Result := FIsCancelled;
end;

procedure TdxHttpIndyRequest.DoWork(ASender: TObject;
  AWorkMode: TWorkMode; AWorkCount: Int64);
const
  WorkModeToRequestMode: array [TWorkMode] of TdxHttpRequestMode = (mhrmRead, mhrmWrite);
begin
  if Assigned(OnRequest) then
    OnRequest(Self, WorkModeToRequestMode[AWorkMode], AWorkCount, FIsCancelled);
  if FIsCancelled then
    (ASender as TIdHttp).Disconnect;
end;


initialization
  TdxHttpRequest.RegisterHandler(TdxHttpIndyRequest);
  dxURLEncode := URLEncode;

finalization
  dxURLEncode := nil;
  FreeAndNil(FHttpDefaultProxyInfo);

end.
