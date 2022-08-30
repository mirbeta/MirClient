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

unit dxHttpRequest;

interface

{$I cxVer.inc}

uses
  SysUtils, Classes, Forms, Math, Generics.Defaults, Generics.Collections, dxGdiPlusClasses;

type
  TdxHttpRequestMode = (mhrmRead, mhrmWrite);
  TdxHttpRequestEvent = procedure (ASender: TObject;
    ARequestMode: TdxHttpRequestMode; ACount: Int64; var ACancel: Boolean) of object;

  TdxHttpRequestHandler = class abstract
  private
    FOnRequest: TdxHttpRequestEvent;
  public
    constructor Create; virtual;
    function Get(const AUri: string; AResponseContent: TStream): Boolean; virtual; abstract;
    function HasErrors: Boolean; virtual; abstract;
    function IsCancelled: Boolean; virtual; abstract;
    property OnRequest: TdxHttpRequestEvent read FOnRequest write FOnRequest;
  end;
  TdxHttpRequestHandlerClass = class of TdxHttpRequestHandler;

  TdxHttpRequest = class
  strict private
    class var
      FHandlers: TList<TdxHttpRequestHandlerClass>;
    class constructor Initialize;
{$IFDEF DELPHIXE}
    class destructor Finalize;
{$ELSE}
  {$HINTS OFF} // #Ch http://qc.embarcadero.com/wc/qcmain.aspx?d=80785
    class destructor Finalize;
  {$HINTS ON}
{$ENDIF}
  public
    class function CreateHandler: TdxHttpRequestHandler;
    class function GetContent(const AUri: string; AResponseContent: TStream): Boolean;
    class function GetHandlerClass: TdxHttpRequestHandlerClass;
    class function GetImage(const AUri: string): TdxSmartImage;
    class procedure RegisterHandler(AHandlerClass: TdxHttpRequestHandlerClass);
  end;

var
  dxURLEncode: function (const ASrc: string): string;

implementation

{ TdxHttpRequest }

class constructor TdxHttpRequest.Initialize;
begin
  FHandlers := TList<TdxHttpRequestHandlerClass>.Create;
end;

class destructor TdxHttpRequest.Finalize;
begin
  FreeAndNil(FHandlers);
end;

class function TdxHttpRequest.CreateHandler: TdxHttpRequestHandler;
begin
  if FHandlers.Count > 0 then
    Result := GetHandlerClass.Create
  else
    Result := nil;
end;

class function TdxHttpRequest.GetContent(const AUri: string;
  AResponseContent: TStream): Boolean;
var
  AHandler: TdxHttpRequestHandler;
begin
  AHandler := CreateHandler;
  try
    Result := (AHandler <> nil) and
      AHandler.Get(AUri, AResponseContent);
  finally
    AHandler.Free;
  end;
end;

class function TdxHttpRequest.GetHandlerClass: TdxHttpRequestHandlerClass;
begin
  if FHandlers.Count > 0 then
    Result := FHandlers.Last
  else
    Result := nil;
end;

class function TdxHttpRequest.GetImage(const AUri: string): TdxSmartImage;
var
  AStream: TStream;
begin
  Result := nil;
  AStream := TMemoryStream.Create;
  try
    if GetContent(AUri, AStream) then
    begin
      AStream.Position := 0;
      Result := TdxSmartImage.CreateFromStream(AStream);
    end;
  finally
    AStream.Free;
  end;
end;

class procedure TdxHttpRequest.RegisterHandler(
  AHandlerClass: TdxHttpRequestHandlerClass);
begin
  FHandlers.Add(AHandlerClass);
end;

{ TdxHttpRequestHandler }

constructor TdxHttpRequestHandler.Create;
begin
  inherited Create;
end;

end.
