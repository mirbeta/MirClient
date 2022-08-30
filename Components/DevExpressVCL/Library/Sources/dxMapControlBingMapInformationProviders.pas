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

unit dxMapControlBingMapInformationProviders;

interface

{$I cxVer.inc}

uses
  Types, Classes, SysUtils, Graphics, Math,
  dxCoreClasses, cxGraphics, cxClasses, cxGeometry,
  dxMapControlTypes, dxRESTService, dxBingMapRESTService,
  dxMapControlInformationProvider, dxBingMapLocationDataService, dxBingMapRouteDataService;

type
  TdxMapControlBingMapInformationProvider = class(TdxMapControlInformationProvider)
  private
    FBingService: TdxBingMapRESTService;
    FUserLocation: TdxMapControlGeoLocation;
    FUserMapView: TdxMapControlGeoRectPersistent;
    function GetBingKey: string;
    function GetParams: TdxBingMapRESTServiceParams;
    function GetUserIp: string;
    procedure SetBingKey(const Value: string);
    procedure SetUserIp(const Value: string);
    procedure SetUserLocation(const Value: TdxMapControlGeoLocation);
    procedure SetUserMapView(const Value: TdxMapControlGeoRectPersistent);
    procedure SetUseLocation(const Value: Boolean);
    procedure SetUseMapView(const Value: Boolean);
    procedure UserLocationChanged(Sender: TObject);
    procedure UserMapViewChange(Sender: TObject);
    function GetUseLocation: Boolean;
    function GetUseMapView: Boolean;
  protected
    function CreateService: TdxBingMapRESTService; virtual;
    procedure DoAssign(Source: TPersistent); override;
    procedure DoResponse(Sender: TObject; AResponse: TdxRestServiceResponse; var ADestroyResponse: Boolean); virtual;
    property BingService: TdxBingMapRESTService read FBingService;
    property Params: TdxBingMapRESTServiceParams read GetParams;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CancelRequests;
  published
    property BingKey: string read GetBingKey write SetBingKey;
    property UserIp: string read GetUserIp write SetUserIp;
    property UseLocation: Boolean read GetUseLocation write SetUseLocation default False;
    property UseMapView: Boolean read GetUseMapView write SetUseMapView default False;
    property UserLocation: TdxMapControlGeoLocation read FUserLocation write SetUserLocation;
    property UserMapView: TdxMapControlGeoRectPersistent read FUserMapView write SetUserMapView;
  end;

  TdxBingMapLocationDataProviderResponseEvent = procedure(Sender: TObject;
    AResponse: TdxBingMapLocationDataServiceResponse; var ADestroyResponse: Boolean) of object;

  TdxMapControlBingMapCustomLocationDataProvider = class(TdxMapControlBingMapInformationProvider)
  private
    FOnResponse: TdxBingMapLocationDataProviderResponseEvent;
    function GetIncludeNeighborhood: Boolean;
    function GetParams: TdxBingMapRESTServiceLocationParams;
    procedure SetIncludeNeighborhood(const Value: Boolean);
  protected
    procedure DoAssign(Source: TPersistent); override;
    procedure DoResponse(Sender: TObject; AResponse: TdxRestServiceResponse; var ADestroyResponse: Boolean); override;
    property Params: TdxBingMapRESTServiceLocationParams read GetParams;
  published
    property IncludeNeighborhood: Boolean read GetIncludeNeighborhood write SetIncludeNeighborhood default True;
    property OnResponse: TdxBingMapLocationDataProviderResponseEvent read FOnResponse write FOnResponse;
  end;

  TdxMapControlBingMapGeoCodingDataProvider = class(TdxMapControlBingMapCustomLocationDataProvider)
  private
    function GetMaxResults: Integer;
    function GetParams: TdxBingMapRESTServiceLocationByQueryParams;
    procedure SetMaxResults(const Value: Integer);
  protected
    function CreateService: TdxBingMapRESTService; override;
    procedure DoAssign(Source: TPersistent); override;
    property Params: TdxBingMapRESTServiceLocationByQueryParams read GetParams;
  public
    procedure Search(const AQuery: string); overload;
    procedure Search(const AQuery: string;
      out AResponse: TdxBingMapLocationDataServiceResponse); overload;
    procedure SearchAsync(const AQuery: string);
  published
    property MaxResults: Integer read GetMaxResults write SetMaxResults default 0;
  end;

  TdxMapControlBingMapReverseGeoCodingDataProvider = class(TdxMapControlBingMapCustomLocationDataProvider)
  private
    function GetIncludeEntityTypes: TdxBingMapIncludeEntityTypes;
    function GetParams: TdxBingMapRESTServiceLocationByPointParams;
    procedure SetIncludeEntityTypes(const Value: TdxBingMapIncludeEntityTypes);
  protected
    function CreateService: TdxBingMapRESTService; override;
    procedure DoAssign(Source: TPersistent); override;
    property Params: TdxBingMapRESTServiceLocationByPointParams read GetParams;
  public
    procedure Search(const AGeoPoint: TdxMapControlGeoPoint); overload;
    procedure Search(const AGeoPoint: TdxMapControlGeoPoint;
      out AResponse: TdxBingMapLocationDataServiceResponse); overload;
    procedure SearchAsync(const AGeoPoint: TdxMapControlGeoPoint);
  published
    property IncludeEntityTypes: TdxBingMapIncludeEntityTypes read GetIncludeEntityTypes write SetIncludeEntityTypes default [];
  end;

  TdxBingMapRouteDataProviderResponseEvent = procedure(Sender: TObject;
    AResponse: TdxBingMapRouteDataServiceResponse; var ADestroyResponse: Boolean) of object;

  TdxMapControlBingMapCustomRouteDataProvider = class(TdxMapControlBingMapInformationProvider)
  private
    FOnResponse: TdxBingMapRouteDataProviderResponseEvent;
    function GetDistanceUnit: TdxBingMapDistanceUnit;
    function GetParams: TdxBingMapRESTServiceCustomRouteParams;
    function GetRoutePathOutput: Boolean;
    procedure SetDistanceUnit(Value: TdxBingMapDistanceUnit);
    procedure SetRoutePathOutput(Value: Boolean);
  protected
    procedure DoAssign(Source: TPersistent); override;
    procedure DoResponse(Sender: TObject; AResponse: TdxRestServiceResponse; var ADestroyResponse: Boolean); override;
    property Params: TdxBingMapRESTServiceCustomRouteParams read GetParams;
  published
    property DistanceUnit: TdxBingMapDistanceUnit read GetDistanceUnit write SetDistanceUnit default bmduKilometers;
    property RoutePathOutput: Boolean read GetRoutePathOutput write SetRoutePathOutput default True;
    property OnResponse: TdxBingMapRouteDataProviderResponseEvent read FOnResponse write FOnResponse;
  end;

  TdxMapControlBingMapRouteDataProvider = class(TdxMapControlBingMapCustomRouteDataProvider)
  private
    function GetDateTime: TDateTime;
    function GetExcludeHighways: TdxBingMapExcludeRoadType;
    function GetExcludeTolls: TdxBingMapExcludeRoadType;
    function GetMaxSolutions: Integer;
    function GetOptimize: TdxBingMapOptimizeRouteType;
    function GetParams: TdxBingMapRESTServiceRouteParams;
    function GetTimeType: TdxBingMapTransitTimeType;
    function GetTravelMode: TdxBingMapTravelMode;
    procedure SetDateTime(const Value: TDateTime);
    procedure SetExcludeHighways(const Value: TdxBingMapExcludeRoadType);
    procedure SetExcludeTolls(const Value: TdxBingMapExcludeRoadType);
    procedure SetMaxSolutions(const Value: Integer);
    procedure SetOptimize(const Value: TdxBingMapOptimizeRouteType);
    procedure SetTimeType(const Value: TdxBingMapTransitTimeType);
    procedure SetTravelMode(const Value: TdxBingMapTravelMode);
  protected
    function CreateService: TdxBingMapRESTService; override;
    procedure DoAssign(Source: TPersistent); override;
    property Params: TdxBingMapRESTServiceRouteParams read GetParams;
  public
    procedure Calculate(ARouteWaypoints: TdxBingMapRouteWaypoints); overload;
    procedure Calculate(ARouteWaypoints: TdxBingMapRouteWaypoints;
      out AResponse: TdxBingMapRouteDataServiceResponse); overload;
    procedure CalculateAsync(ARouteWaypoints: TdxBingMapRouteWaypoints);
  published
    property DateTime: TDateTime read GetDateTime write SetDateTime;
    property ExcludeHighways: TdxBingMapExcludeRoadType read GetExcludeHighways write SetExcludeHighways default bmetNone;
    property ExcludeTolls: TdxBingMapExcludeRoadType read GetExcludeTolls write SetExcludeTolls default bmetNone;
    property MaxSolutions: Integer read GetMaxSolutions write SetMaxSolutions default 1;
    property Optimize: TdxBingMapOptimizeRouteType read GetOptimize write SetOptimize default bmorTime;
    property TimeType: TdxBingMapTransitTimeType read GetTimeType write SetTimeType default bmttArrival;
    property TravelMode: TdxBingMapTravelMode read GetTravelMode write SetTravelMode default bmtmDriving;
  end;

  TdxMapControlBingMapMajorRoadRouteDataProvider = class(TdxMapControlBingMapCustomRouteDataProvider)
  private
    function GetExcludeRoutes: Boolean;
    function GetParams: TdxBingMapRESTServiceRoutesFromMajorRoadsParams;
    procedure SetExcludeRoutes(Value: Boolean);
  protected
    function CreateService: TdxBingMapRESTService; override;
    procedure DoAssign(Source: TPersistent); override;
    property Params: TdxBingMapRESTServiceRoutesFromMajorRoadsParams read GetParams;
  public
    procedure Calculate(ARouteWaypoint: TdxBingMapRouteWaypoint); overload;
    procedure Calculate(ARouteWaypoint: TdxBingMapRouteWaypoint;
      out AResponse: TdxBingMapRouteDataServiceResponse); overload;
    procedure CalculateAsync(ARouteWaypoint: TdxBingMapRouteWaypoint);
  published
    property ExcludeRoutes: Boolean read GetExcludeRoutes write SetExcludeRoutes default False;
  end;

implementation

{ TdxMapControlBingMapInformationProvider }

constructor TdxMapControlBingMapInformationProvider.Create(
  AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBingService := CreateService;
  FBingService.OnResponse := DoResponse;
  FUserLocation := TdxMapControlGeoLocation.Create(Self);
  FUserLocation.OnChanged := UserLocationChanged;
  FUserMapView := TdxMapControlGeoRectPersistent.Create(Self);
  FUserMapView.OnChange := UserMapViewChange;
end;

destructor TdxMapControlBingMapInformationProvider.Destroy;
begin
  FreeandNil(FUserMapView);
  FreeandNil(FUserLocation);
  FreeAndNil(FBingService);
  inherited;
end;

procedure TdxMapControlBingMapInformationProvider.CancelRequests;
begin
  FBingService.CancelRequests;
end;

procedure TdxMapControlBingMapInformationProvider.DoAssign(
  Source: TPersistent);
var
  AInformationProvider: TdxMapControlBingMapInformationProvider;
begin
  inherited DoAssign(Source);
  if Source is TdxMapControlBingMapInformationProvider then
  begin
    AInformationProvider := TdxMapControlBingMapInformationProvider(Source);
    UserIp := AInformationProvider.UserIp;
    UserLocation := AInformationProvider.UserLocation;
    UserMapView := AInformationProvider.UserMapView;
  end;
end;

procedure TdxMapControlBingMapInformationProvider.DoResponse(
  Sender: TObject; AResponse: TdxRestServiceResponse; var ADestroyResponse: Boolean);
begin
end;

function TdxMapControlBingMapInformationProvider.CreateService: TdxBingMapRESTService;
begin
  Result := nil;
end;

function TdxMapControlBingMapInformationProvider.GetBingKey: string;
begin
  Result := Params.Key;
end;

function TdxMapControlBingMapInformationProvider.GetParams: TdxBingMapRESTServiceParams;
begin
  Result := FBingService.Params;
end;

function TdxMapControlBingMapInformationProvider.GetUseLocation: Boolean;
begin
  Result := Params.UseLocation;
end;

function TdxMapControlBingMapInformationProvider.GetUseMapView: Boolean;
begin
  Result := Params.UseMapView;
end;

function TdxMapControlBingMapInformationProvider.GetUserIp: string;
begin
  Result := Params.UserIp;
end;

procedure TdxMapControlBingMapInformationProvider.SetBingKey(
  const Value: string);
begin
  Params.Key := Value;
end;

procedure TdxMapControlBingMapInformationProvider.SetUseLocation(
  const Value: Boolean);
begin
  Params.UseLocation := Value;
end;

procedure TdxMapControlBingMapInformationProvider.SetUseMapView(
  const Value: Boolean);
begin
  Params.UseMapView := Value;
end;

procedure TdxMapControlBingMapInformationProvider.SetUserIp(
  const Value: string);
begin
  Params.UserIp := Value;
end;

procedure TdxMapControlBingMapInformationProvider.SetUserLocation(
  const Value: TdxMapControlGeoLocation);
begin
  FUserLocation.Assign(Value);
end;

procedure TdxMapControlBingMapInformationProvider.SetUserMapView(
  const Value: TdxMapControlGeoRectPersistent);
begin
  FUserMapView.Assign(Value);
end;

procedure TdxMapControlBingMapInformationProvider.UserLocationChanged(
  Sender: TObject);
begin
  Params.UserLocation := UserLocation.GeoPoint;
end;

procedure TdxMapControlBingMapInformationProvider.UserMapViewChange(
  Sender: TObject);
begin
  FBingService.Params.UserMapView := UserMapView.GeoRect;
end;

{ TdxMapControlBingMapCustomLocationDataProvider }

procedure TdxMapControlBingMapCustomLocationDataProvider.DoAssign(
  Source: TPersistent);
var
  ABingMapLocationDataProvider: TdxMapControlBingMapCustomLocationDataProvider;
begin
  inherited DoAssign(Source);
  if Source is TdxMapControlBingMapCustomLocationDataProvider then
  begin
    ABingMapLocationDataProvider := TdxMapControlBingMapCustomLocationDataProvider(Source);
    IncludeNeighborhood := ABingMapLocationDataProvider.IncludeNeighborhood;
  end;
end;

procedure TdxMapControlBingMapCustomLocationDataProvider.DoResponse(
  Sender: TObject; AResponse: TdxRestServiceResponse;
  var ADestroyResponse: Boolean);
begin
  if Assigned(FOnResponse) then
    FOnResponse(Self, AResponse as TdxBingMapLocationDataServiceResponse, ADestroyResponse);
end;

function TdxMapControlBingMapCustomLocationDataProvider.GetIncludeNeighborhood: Boolean;
begin
  Result := Params.IncludeNeighborhood;
end;

function TdxMapControlBingMapCustomLocationDataProvider.GetParams: TdxBingMapRESTServiceLocationParams;
begin
  Result := inherited Params as TdxBingMapRESTServiceLocationParams;
end;

procedure TdxMapControlBingMapCustomLocationDataProvider.SetIncludeNeighborhood(
  const Value: Boolean);
begin
  Params.IncludeNeighborhood := Value;
end;

{ TdxMapControlBingMapSearchDataProvider }

function TdxMapControlBingMapGeoCodingDataProvider.CreateService: TdxBingMapRESTService;
begin
  Result := TdxBingMapLocationByQueryDataService.Create;
end;

procedure TdxMapControlBingMapGeoCodingDataProvider.DoAssign(
  Source: TPersistent);
var
  ABingMapGeoCodingDataProvider: TdxMapControlBingMapGeoCodingDataProvider;
begin
  inherited DoAssign(Source);
  if Source is TdxMapControlBingMapGeoCodingDataProvider then
  begin
    ABingMapGeoCodingDataProvider := TdxMapControlBingMapGeoCodingDataProvider(Source);
    MaxResults := ABingMapGeoCodingDataProvider.MaxResults;
  end;
end;

function TdxMapControlBingMapGeoCodingDataProvider.GetMaxResults: Integer;
begin
  Result := Params.MaxResults;
end;

function TdxMapControlBingMapGeoCodingDataProvider.GetParams: TdxBingMapRESTServiceLocationByQueryParams;
begin
  Result := inherited Params as TdxBingMapRESTServiceLocationByQueryParams;
end;

procedure TdxMapControlBingMapGeoCodingDataProvider.Search(
  const AQuery: string);
begin
  Params.Query := AQuery;
  BingService.Execute;
end;

procedure TdxMapControlBingMapGeoCodingDataProvider.Search(const AQuery: string;
  out AResponse: TdxBingMapLocationDataServiceResponse);
begin
  Params.Query := AQuery;
  BingService.Execute(TdxBingMapResponse(AResponse));
end;

procedure TdxMapControlBingMapGeoCodingDataProvider.SearchAsync(
  const AQuery: string);
begin
  Params.Query := AQuery;
  BingService.ExecuteAsync;
end;

procedure TdxMapControlBingMapGeoCodingDataProvider.SetMaxResults(
  const Value: Integer);
begin
  Params.MaxResults := Max(0, Value);
end;

{ TdxMapControlBingMapReverseGeoCodingDataProvider }

function TdxMapControlBingMapReverseGeoCodingDataProvider.CreateService: TdxBingMapRESTService;
begin
  Result := TdxBingMapLocationByPointDataService.Create;
end;

procedure TdxMapControlBingMapReverseGeoCodingDataProvider.DoAssign(
  Source: TPersistent);
var
  ABingMapReverseGeoCodingDataProvider: TdxMapControlBingMapReverseGeoCodingDataProvider;
begin
  inherited DoAssign(Source);
  if Source is TdxMapControlBingMapReverseGeoCodingDataProvider then
  begin
    ABingMapReverseGeoCodingDataProvider := TdxMapControlBingMapReverseGeoCodingDataProvider(Source);
    IncludeEntityTypes := ABingMapReverseGeoCodingDataProvider.IncludeEntityTypes;
  end;
end;

function TdxMapControlBingMapReverseGeoCodingDataProvider.GetIncludeEntityTypes: TdxBingMapIncludeEntityTypes;
begin
  Result := Params.IncludeEntityTypes;
end;

function TdxMapControlBingMapReverseGeoCodingDataProvider.GetParams: TdxBingMapRESTServiceLocationByPointParams;
begin
  Result := inherited Params as TdxBingMapRESTServiceLocationByPointParams;
end;

procedure TdxMapControlBingMapReverseGeoCodingDataProvider.Search(
  const AGeoPoint: TdxMapControlGeoPoint);
begin
  Params.Point := AGeoPoint;
  BingService.Execute;
end;

procedure TdxMapControlBingMapReverseGeoCodingDataProvider.Search(
  const AGeoPoint: TdxMapControlGeoPoint;
  out AResponse: TdxBingMapLocationDataServiceResponse);
begin
  Params.Point := AGeoPoint;
  BingService.Execute(TdxBingMapResponse(AResponse));
end;

procedure TdxMapControlBingMapReverseGeoCodingDataProvider.SearchAsync(
  const AGeoPoint: TdxMapControlGeoPoint);
begin
  Params.Point := AGeoPoint;
  BingService.ExecuteAsync;
end;

procedure TdxMapControlBingMapReverseGeoCodingDataProvider.SetIncludeEntityTypes(
  const Value: TdxBingMapIncludeEntityTypes);
begin
  Params.IncludeEntityTypes := Value;
end;

{ TdxMapControlBingMapCustomRouteDataProvider }

procedure TdxMapControlBingMapCustomRouteDataProvider.DoAssign(
  Source: TPersistent);
var
  ABingMapCustomRouteDataProvider: TdxMapControlBingMapCustomRouteDataProvider;
begin
  inherited DoAssign(Source);
  if Source is TdxMapControlBingMapCustomRouteDataProvider then
  begin
    ABingMapCustomRouteDataProvider := TdxMapControlBingMapCustomRouteDataProvider(Source);
    DistanceUnit := ABingMapCustomRouteDataProvider.DistanceUnit;
    RoutePathOutput := ABingMapCustomRouteDataProvider.RoutePathOutput;
  end;
end;

procedure TdxMapControlBingMapCustomRouteDataProvider.DoResponse(
  Sender: TObject; AResponse: TdxRestServiceResponse;
  var ADestroyResponse: Boolean);
begin
  if Assigned(FOnResponse) then
    FOnResponse(Self, AResponse as TdxBingMapRouteDataServiceResponse, ADestroyResponse);
end;

function TdxMapControlBingMapCustomRouteDataProvider.GetDistanceUnit: TdxBingMapDistanceUnit;
begin
  Result := Params.DistanceUnit;
end;

function TdxMapControlBingMapCustomRouteDataProvider.GetParams: TdxBingMapRESTServiceCustomRouteParams;
begin
  Result := inherited Params as TdxBingMapRESTServiceCustomRouteParams;
end;

function TdxMapControlBingMapCustomRouteDataProvider.GetRoutePathOutput: Boolean;
begin
  Result := Params.RoutePathOutput;
end;

procedure TdxMapControlBingMapCustomRouteDataProvider.SetDistanceUnit(
  Value: TdxBingMapDistanceUnit);
begin
  Params.DistanceUnit := Value;
end;

procedure TdxMapControlBingMapCustomRouteDataProvider.SetRoutePathOutput(
  Value: Boolean);
begin
  Params.RoutePathOutput := Value;
end;

{ TdxMapControlBingMapRouteDataProvider }

procedure TdxMapControlBingMapRouteDataProvider.Calculate(
  ARouteWaypoints: TdxBingMapRouteWaypoints);
begin
  Params.BuildRouteWayPointsParam(ARouteWaypoints);
  BingService.Execute;
end;

procedure TdxMapControlBingMapRouteDataProvider.Calculate(
  ARouteWaypoints: TdxBingMapRouteWaypoints;
  out AResponse: TdxBingMapRouteDataServiceResponse);
begin
  Params.BuildRouteWayPointsParam(ARouteWaypoints);
  BingService.Execute(TdxBingMapResponse(AResponse));
end;

procedure TdxMapControlBingMapRouteDataProvider.CalculateAsync(
  ARouteWaypoints: TdxBingMapRouteWaypoints);
begin
  Params.BuildRouteWayPointsParam(ARouteWaypoints);
  BingService.ExecuteAsync;
end;

function TdxMapControlBingMapRouteDataProvider.CreateService: TdxBingMapRESTService;
begin
  Result := TdxBingMapRouteDataService.Create;
end;

procedure TdxMapControlBingMapRouteDataProvider.DoAssign(Source: TPersistent);
var
  ABingMapRouteDataProvider: TdxMapControlBingMapRouteDataProvider;
begin
  inherited DoAssign(Source);
  if Source is TdxMapControlBingMapRouteDataProvider then
  begin
    ABingMapRouteDataProvider := TdxMapControlBingMapRouteDataProvider(Source);
    DateTime := ABingMapRouteDataProvider.DateTime;
    ExcludeHighways := ABingMapRouteDataProvider.ExcludeHighways;
    ExcludeTolls := ABingMapRouteDataProvider.ExcludeTolls;
    MaxSolutions := ABingMapRouteDataProvider.MaxSolutions;
    Optimize := ABingMapRouteDataProvider.Optimize;
    TimeType := ABingMapRouteDataProvider.TimeType;
    TravelMode := ABingMapRouteDataProvider.TravelMode;
  end;
end;

function TdxMapControlBingMapRouteDataProvider.GetDateTime: TDateTime;
begin
  Result := Params.DateTime;
end;

function TdxMapControlBingMapRouteDataProvider.GetExcludeHighways: TdxBingMapExcludeRoadType;
begin
  Result := Params.ExcludeHighways;
end;

function TdxMapControlBingMapRouteDataProvider.GetExcludeTolls: TdxBingMapExcludeRoadType;
begin
  Result := Params.ExcludeTolls;
end;

function TdxMapControlBingMapRouteDataProvider.GetMaxSolutions: Integer;
begin
  Result := Params.MaxSolutions;
end;

function TdxMapControlBingMapRouteDataProvider.GetOptimize: TdxBingMapOptimizeRouteType;
begin
  Result := Params.Optimize;
end;

function TdxMapControlBingMapRouteDataProvider.GetParams: TdxBingMapRESTServiceRouteParams;
begin
  Result := inherited Params as TdxBingMapRESTServiceRouteParams;
end;

function TdxMapControlBingMapRouteDataProvider.GetTimeType: TdxBingMapTransitTimeType;
begin
  Result := Params.TimeType;
end;

function TdxMapControlBingMapRouteDataProvider.GetTravelMode: TdxBingMapTravelMode;
begin
  Result := Params.TravelMode;
end;

procedure TdxMapControlBingMapRouteDataProvider.SetDateTime(
  const Value: TDateTime);
begin
  Params.DateTime := Value;
end;

procedure TdxMapControlBingMapRouteDataProvider.SetExcludeHighways(
  const Value: TdxBingMapExcludeRoadType);
begin
  Params.ExcludeHighways := Value;
end;

procedure TdxMapControlBingMapRouteDataProvider.SetExcludeTolls(
  const Value: TdxBingMapExcludeRoadType);
begin
  Params.ExcludeTolls := Value;
end;

procedure TdxMapControlBingMapRouteDataProvider.SetMaxSolutions(
  const Value: Integer);
begin
  Params.MaxSolutions := Value;
end;

procedure TdxMapControlBingMapRouteDataProvider.SetOptimize(
  const Value: TdxBingMapOptimizeRouteType);
begin
  Params.Optimize := Value;
end;

procedure TdxMapControlBingMapRouteDataProvider.SetTimeType(
  const Value: TdxBingMapTransitTimeType);
begin
  Params.TimeType := Value;
end;

procedure TdxMapControlBingMapRouteDataProvider.SetTravelMode(
  const Value: TdxBingMapTravelMode);
begin
  Params.TravelMode := Value;
end;

{ TdxMapControlBingMapMajorRoadRouteDataProvider }

procedure TdxMapControlBingMapMajorRoadRouteDataProvider.Calculate(
  ARouteWaypoint: TdxBingMapRouteWaypoint);
begin
  Params.Destination := ARouteWaypoint.AsString;
  BingService.Execute;
end;

procedure TdxMapControlBingMapMajorRoadRouteDataProvider.Calculate(
  ARouteWaypoint: TdxBingMapRouteWaypoint;
  out AResponse: TdxBingMapRouteDataServiceResponse);
begin
  Params.Destination := ARouteWaypoint.AsString;
  BingService.Execute(TdxBingMapResponse(AResponse));
end;

procedure TdxMapControlBingMapMajorRoadRouteDataProvider.CalculateAsync(
  ARouteWaypoint: TdxBingMapRouteWaypoint);
begin
  Params.Destination := ARouteWaypoint.AsString;
  BingService.ExecuteAsync;
end;

function TdxMapControlBingMapMajorRoadRouteDataProvider.CreateService: TdxBingMapRESTService;
begin
  Result := TdxBingMapRoutesFromMajorRoadsDataService.Create;
end;

procedure TdxMapControlBingMapMajorRoadRouteDataProvider.DoAssign(
  Source: TPersistent);
var
  ABingMapMajorRoadRouteDataProvider: TdxMapControlBingMapMajorRoadRouteDataProvider;
begin
  inherited DoAssign(Source);
  if Source is TdxMapControlBingMapMajorRoadRouteDataProvider then
  begin
    ABingMapMajorRoadRouteDataProvider := TdxMapControlBingMapMajorRoadRouteDataProvider(Source);
    ExcludeRoutes := ABingMapMajorRoadRouteDataProvider.ExcludeRoutes;
  end;
end;

function TdxMapControlBingMapMajorRoadRouteDataProvider.GetExcludeRoutes: Boolean;
begin
  Result := Params.ExcludeRoutes;
end;

function TdxMapControlBingMapMajorRoadRouteDataProvider.GetParams: TdxBingMapRESTServiceRoutesFromMajorRoadsParams;
begin
  Result := inherited Params as TdxBingMapRESTServiceRoutesFromMajorRoadsParams;
end;

procedure TdxMapControlBingMapMajorRoadRouteDataProvider.SetExcludeRoutes(
  Value: Boolean);
begin
  Params.ExcludeRoutes := Value;
end;

end.
