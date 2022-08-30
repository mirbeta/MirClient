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

unit dxBingMapRESTService;

interface

{$I cxVer.inc}

uses
  Windows, SysUtils, StrUtils, Classes, Types, Generics.Defaults, Generics.Collections, Math,
  dxCore, dxXMLDoc, dxCustomTree, cxClasses, dxCoreClasses,
  dxBingMapRESTServiceStrs, dxMapControlTypes, dxRESTService;

type
  TdxBingMapRESTServiceParams = class
  private
    FKey: string;
    FOutputFormat: TdxRestServiceOutputFormat;
    FSuppressStatus: Boolean;
    FUserIp: string;
    FUseLocation: Boolean;
    FUseMapView: Boolean;
    FUserLocation: TdxMapControlGeoPoint;
    FUserMapView: TdxMapControlGeoRect;
  protected
    procedure AddParamAfter(var AParams: string; const AAddingParam: string);
    procedure AddParamBefore(var AParams: string; const AAddingParam: string);
    function AsString: string; virtual;
    function DoAsString: string; virtual;
    function GetBingKeyParam: string;
    function GetCultureParam: string;
    function GetOutputFormatParam: string;
    function GetSuppressStatusParam: string;
    function GetUserIpParam: string;
    function GetUserLocationParam: string;
    function GetUserMapViewParam: string;
  public
    constructor Create; virtual;
    property Key: string read FKey write FKey;
    property OutputFormat: TdxRestServiceOutputFormat read FOutputFormat;
    property UserIp: string read FUserIp write FUserIp;
    property UseLocation: Boolean read FUseLocation write FUseLocation;
    property UseMapView: Boolean read FUseMapView write FUseMapView;
    property UserLocation: TdxMapControlGeoPoint read FUserLocation write FUserLocation;
    property UserMapView: TdxMapControlGeoRect read FUserMapView write FUserMapView;
  end;

  TdxBingMapErrorInfo = class
  private
    FAuthenticationResultCode: string;
    FErrorDetails: string;
    FStatusCode: string;
    FStatusDescription: string;
  public
    procedure Parse(ANode: TdxXmlNode);
    property AuthenticationResultCode: string read FAuthenticationResultCode;
    property ErrorDetails: string read FErrorDetails;
    property StatusCode: string read FStatusCode;
    property StatusDescription: string read FStatusDescription;
  end;

  TdxBingMapResponse = class(TdxRestServiceResponse)
  private
    FErrorInfo: TdxBingMapErrorInfo;
  protected
    procedure DoParse(ADoc: TdxXMLDocument); override;
    procedure ParseResource(ANode: TdxXMLNode); virtual;
    procedure ParseResources(ANode: TdxXMLNode); virtual;
  public
    destructor Destroy; override;
    property ErrorInfo: TdxBingMapErrorInfo read FErrorInfo;
  end;

  TdxBingMapRESTService = class(TdxRESTService)
  private
    FParams: TdxBingMapRESTServiceParams;
    function GetBingKey: string;
    function GetUserIp: string;
    function GetUserLocation: TdxMapControlGeoPoint;
    function GetUserMapView: TdxMapControlGeoRect;
    procedure SetBingKey(const Value: string);
    procedure SetUserIp(const Value: string);
    procedure SetUserLocation(const Value: TdxMapControlGeoPoint);
    procedure SetUserMapView(const Value: TdxMapControlGeoRect);
  protected
    function GetBaseUrl: string; virtual; abstract;
    function GetRequestUrl: string; virtual;
    function CreateParams: TdxBingMapRESTServiceParams; virtual;
    function CreateResponse: TdxRestServiceResponse; override;
    function GetResponseFormat: TdxRestServiceOutputFormat; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Execute; overload;
    procedure Execute(out AResponse: TdxBingMapResponse); overload;
    procedure ExecuteAsync;

    property BingKey: string read GetBingKey write SetBingKey;
    property UserIp: string read GetUserIp write SetUserIp;
    property UserLocation: TdxMapControlGeoPoint read GetUserLocation write SetUserLocation;
    property UserMapView: TdxMapControlGeoRect read GetUserMapView write SetUserMapView;
    property Params: TdxBingMapRESTServiceParams read FParams;
  end;

  TdxBingMapDataXmlParser = class
  public
    class function ParseBoundingBoxNode(ANode: TdxXMLNode; var ARect: TdxMapControlGeoRect): Boolean;
    class function ParsePointNode(ANode: TdxXMLNode; var APoint: TdxMapControlGeoPoint): Boolean;
  end;

function dxBingMapGetCulture: string;

implementation

function dxBingMapGetCulture: string;
var
  AIndex: Integer;
begin
  Result := '';
  AIndex := dxLanguages.IndexOf(GetThreadLocale);
  if AIndex <> -1 then
    Result := dxLanguages.LocaleName[AIndex];
end;

{ TdxBingMapRESTServiceParams }

constructor TdxBingMapRESTServiceParams.Create;
begin
  inherited Create;
  FSuppressStatus := True;
end;

procedure TdxBingMapRESTServiceParams.AddParamAfter(var AParams: string; const AAddingParam: string);
begin
  if AAddingParam <> '' then
    AParams := AParams + IfThen(AParams <> '', dxUrlParamsDevider) + AAddingParam;
end;

procedure TdxBingMapRESTServiceParams.AddParamBefore(var AParams: string;
  const AAddingParam: string);
begin
  if AAddingParam <> '' then
    AParams := AAddingParam + IfThen(AParams <> '', dxUrlParamsDevider) + AParams;
end;

function TdxBingMapRESTServiceParams.AsString: string;
begin
  Result := '?' + DoAsString;
end;

function TdxBingMapRESTServiceParams.DoAsString: string;
begin
  Result := '';
  AddParamAfter(Result, GetUserMapViewParam);
  AddParamAfter(Result, GetUserIpParam);
  AddParamAfter(Result, GetUserLocationParam);
  AddParamAfter(Result, GetCultureParam);
  AddParamAfter(Result, GetSuppressStatusParam);
  AddParamAfter(Result, GetOutputFormatParam);
  AddParamAfter(Result, GetBingKeyParam);
end;

function TdxBingMapRESTServiceParams.GetBingKeyParam: string;
begin
  Result := Format(dxBingKeyParam, [FKey]);
end;

function TdxBingMapRESTServiceParams.GetCultureParam: string;
begin
  Result := dxBingMapGetCulture;
  if Result <> '' then
    Result := Format(dxBingCultureParam, [Result]);
end;

function TdxBingMapRESTServiceParams.GetOutputFormatParam: string;
begin
  Result := Format(dxBingOutputParam, [dxBingOutputFormats[Ord(FOutputFormat)]]);
end;

function TdxBingMapRESTServiceParams.GetSuppressStatusParam: string;
begin
  if FSuppressStatus then
    Result := Format(dxBingSuppressStatusParam, ['true'])
  else
    Result := '';
end;

function TdxBingMapRESTServiceParams.GetUserIpParam: string;
begin
  if FUserIp <> '' then
    Result := Format(dxBingUserIpParam, [FUserIp])
  else
    Result := '';
end;

function TdxBingMapRESTServiceParams.GetUserLocationParam: string;
begin
  if FUseLocation and FUserLocation.IsValid then
    Result := Format(dxBingUserLocationParam, [FUserLocation.AsString])
  else
    Result := '';
end;

function TdxBingMapRESTServiceParams.GetUserMapViewParam: string;
begin
  if FUseMapView and FUserMapView.IsValid then
    Result := Format(dxBingUserMapViewParam, [FUserMapView.AsString])
  else
    Result := '';
end;

{ TdxBingMapErrorInfo }

procedure TdxBingMapErrorInfo.Parse(ANode: TdxXmlNode);
var
  ATempNode: TdxXmlNode;
begin
  if ANode.FindChild(dxBingStatusCode, ATempNode) then
    FStatusCode := ATempNode.TextAsString;
  if ANode.FindChild('AuthenticationResultCode', ATempNode) then
    FAuthenticationResultCode := ATempNode.TextAsString;
  if ANode.FindChild('StatusDescription', ATempNode) then
    FStatusDescription := ATempNode.TextAsString;
  if ANode.FindChild(['ErrorDetails', 'string'], ATempNode) then
    FErrorDetails := ATempNode.TextAsString;
end;

{ TdxBingMapResponse }

destructor TdxBingMapResponse.Destroy;
begin
  FreeAndNil(FErrorInfo);
  inherited Destroy;
end;

procedure TdxBingMapResponse.DoParse(ADoc: TdxXMLDocument);
var
  ANode: TdxXMLNode;
begin
  SetStatus(ADoc.FindChild(dxBingStatusCodeXmlPath, ANode) and
    SameText(ANode.TextAsString, dxBingStatusCodeOkValue));
  if not IsSuccess then
  begin
    FErrorInfo := TdxBingMapErrorInfo.Create;
    FErrorInfo.Parse(ADoc.FindChild([dxBingResponse]));
  end
  else
    if ADoc.FindChild(dxBingResourcesXmlPath, ANode) then
      ParseResources(ANode);
end;

procedure TdxBingMapResponse.ParseResource(ANode: TdxXMLNode);
begin
end;

procedure TdxBingMapResponse.ParseResources(ANode: TdxXMLNode);
begin
  ANode := ANode.First;
  while ANode <> nil do
  begin
    ParseResource(ANode);
    ANode := ANode.Next;
  end;
end;

{ TdxBingMapRESTService}

constructor TdxBingMapRESTService.Create;
begin
  inherited Create;
  FParams := CreateParams;
end;

function TdxBingMapRESTService.CreateParams: TdxBingMapRESTServiceParams;
begin
  Result := TdxBingMapRESTServiceParams.Create;
end;

function TdxBingMapRESTService.CreateResponse: TdxRestServiceResponse;
begin
  Result := TdxBingMapResponse.Create;
end;

destructor TdxBingMapRESTService.Destroy;
begin
  FreeAndNil(FParams);
  inherited Destroy;
end;

procedure TdxBingMapRESTService.Execute;
begin
  Request(GetRequestUrl);
end;

procedure TdxBingMapRESTService.Execute(out AResponse: TdxBingMapResponse);
var
  AR: TdxRestServiceResponse;
begin
  Request(GetRequestUrl, AR);
  AResponse := TdxBingMapResponse(AR);
end;

procedure TdxBingMapRESTService.ExecuteAsync;
begin
  RequestAsync(GetRequestUrl);
end;

function TdxBingMapRESTService.GetResponseFormat: TdxRestServiceOutputFormat;
begin
  Result := FParams.OutputFormat;
end;

function TdxBingMapRESTService.GetBingKey: string;
begin
  Result := FParams.Key;
end;

function TdxBingMapRESTService.GetRequestUrl: string;
begin
  Result := GetBaseUrl + FParams.AsString;
end;

function TdxBingMapRESTService.GetUserIp: string;
begin
  Result := FParams.UserIp;
end;

function TdxBingMapRESTService.GetUserLocation: TdxMapControlGeoPoint;
begin
  Result := FParams.UserLocation;
end;

function TdxBingMapRESTService.GetUserMapView: TdxMapControlGeoRect;
begin
  Result := FParams.UserMapView;
end;

procedure TdxBingMapRESTService.SetBingKey(const Value: string);
begin
  FParams.Key := Value;
end;

procedure TdxBingMapRESTService.SetUserIp(const Value: string);
begin
  FParams.UserIp:= Value;
end;

procedure TdxBingMapRESTService.SetUserLocation(
  const Value: TdxMapControlGeoPoint);
begin
  FParams.UserLocation := Value;
end;

procedure TdxBingMapRESTService.SetUserMapView(
  const Value: TdxMapControlGeoRect);
begin
  FParams.UserMapView := Value;
end;

{ TdxBingMapDataXmlParser }

class function TdxBingMapDataXmlParser.ParseBoundingBoxNode(ANode: TdxXMLNode;
  var ARect: TdxMapControlGeoRect): Boolean;
var
  ASouthLatitudeNode, AWestLongitudeNode,
    ANorthLatitudeNode, AEastLongitudeNode: TdxXMLNode;
begin
  Result := ANode.FindChild('SouthLatitude', ASouthLatitudeNode) and
    ANode.FindChild('WestLongitude', AWestLongitudeNode) and
    ANode.FindChild('NorthLatitude', ANorthLatitudeNode) and
    ANode.FindChild('EastLongitude', AEastLongitudeNode);
  if Result then
  begin
    ARect.SouthLatitude := dxStrToFloatDef(ASouthLatitudeNode.TextAsString, '.');
    ARect.WestLongitude := dxStrToFloatDef(AWestLongitudeNode.TextAsString, '.');
    ARect.NorthLatitude := dxStrToFloatDef(ANorthLatitudeNode.TextAsString, '.');
    ARect.EastLongitude := dxStrToFloatDef(AEastLongitudeNode.TextAsString, '.');
  end;
end;

class function TdxBingMapDataXmlParser.ParsePointNode(ANode: TdxXMLNode;
  var APoint: TdxMapControlGeoPoint): Boolean;
var
  ATempNode: TdxXMLNode;
begin
  Result := False;
  if ANode.FindChild('Latitude', ATempNode) then
  begin
    APoint.Latitude := dxStrToFloatDef(ATempNode.TextAsString, '.');
    if ANode.FindChild('Longitude', ATempNode) then
    begin
      APoint.Longitude := dxStrToFloatDef(ATempNode.TextAsString, '.');
      Result := True;
    end;
  end;
end;

end.
