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

unit dxBingMapRouteDataService;

interface

{$I cxVer.inc}

uses
  SysUtils, StrUtils, Classes, Generics.Defaults, Generics.Collections, Math,
  dxCore, dxXMLDoc, dxCustomTree,
  dxMapControlTypes, dxBingMapLocationDataService, dxBingMapRESTService,
  dxRESTService, dxBingMapRESTServiceStrs;

type
  TdxBingMapRouteDataManeuverType = (
    rdmtArriveFinish,
    rdmtArriveIntermediate,
    rdmtBearLeft,
    rdmtBearLeftThenBearLeft,
    rdmtBearLeftThenBearRight,
    rdmtBearLeftThenTurnLeft,
    rdmtBearLeftThenTurnRight,
    rdmtBearRight,
    rdmtBearRightThenBearLeft,
    rdmtBearRightThenBearRight,
    rdmtBearRightThenTurnLeft,
    rdmtBearRightThenTurnRight,
    rdmtBearThenKeep,
    rdmtBearThenMerge,
    rdmtContinue,
    rdmtDepartIntermediateStop,
    rdmtDepartIntermediateStopReturning,
    rdmtDepartStart,
    rdmtEnterRoundabout,
    rdmtExitRoundabout,
    rdmtEnterThenExitRoundabout,
    rdmtKeepLeft,
    rdmtKeepOnRampLeft,
    rdmtKeepOnRampRight,
    rdmtKeepOnRampStraight,
    rdmtKeepRight,
    rdmtKeepStraight,
    rdmtKeepToStayLeft,
    rdmtKeepToStayRight,
    rdmtKeepToStayStraight,
    rdmtMerge,
    rdmtNone,
    rdmtRampThenHighwayLeft,
    rdmtRampThenHighwayRight,
    rdmtRampThenHighwayStraight,
    rdmtRoadNameChange,
    rdmtTake,
    rdmtTakeRampLeft,
    rdmtTakeRampRight,
    rdmtTakeRampStraight,
    rdmtTakeTransit,
    rdmtTransfer,
    rdmtTransitArrive,
    rdmtTransitDepart,
    rdmtTurnBack,
    rdmtTurnLeft,
    rdmtTurnLeftThenBearLeft,
    rdmtTurnLeftThenBearRight,
    rdmtTurnLeftThenTurnLeft,
    rdmtTurnLeftThenTurnRight,
    rdmtTurnRight,
    rdmtTurnRightThenBearLeft,
    rdmtTurnRightThenBearRight,
    rdmtTurnRightThenTurnLeft,
    rdmtTurnRightThenTurnRight,
    rdmtTurnThenMerge,
    rdmtTurnToStayLeft,
    rdmtTurnToStayRight,
    rdmtUnknown,
    rdmtUTurn,
    rdmtWait,
    rdmtWalk
  );

  TdxBingMapItineraryItemDetail = class
  private
    FCompassDegrees: string;
    FEndPathIndex: Integer;
    FLocationCode: string;
    FManeuverType: TdxBingMapRouteDataManeuverType;
    FNames: TStringList;
    FRoadType: string;
    FStartPathIndex: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Parse(ANode: TdxXmlNode);
    property CompassDegrees: string read FCompassDegrees;
    property EndPathIndex: Integer read FEndPathIndex;
    property LocationCode: string read FLocationCode;
    property ManeuverType: TdxBingMapRouteDataManeuverType read FManeuverType;
    property Names: TStringList read FNames;
    property RoadType: string read FRoadType;
    property StartPathIndex: Integer read FStartPathIndex;
  end;

  TdxBingMapManeuverInstruction = class
  private
    FManeuverType: string;
    FDescription: string;
  public
    procedure Parse(ANode: TdxXmlNode);
    property ManeuverType: string read FManeuverType;
    property Description: string read FDescription;
  end;

  TdxBingMapItineraryItem = class
  private
    FChildItineraryItems: TObjectList<TdxBingMapItineraryItem>;
    FCompassDirection: string;
    FDetails: TObjectList<TdxBingMapItineraryItemDetail>;
    FInstruction: TdxBingMapManeuverInstruction;
    FManeuverPoint: TdxMapControlGeoPoint;
    FSideOfStreet: string;
    FTravelDistance: Double;
    FTravelDuration: Double;
    FTravelMode: string;
    procedure ParseChildItineraryItems(ANode: TdxXmlNode);
    procedure ParseDetail(ANode: TdxXmlNode);
    procedure ParseInstruction(ANode: TdxXmlNode);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Parse(ANode: TdxXmlNode);
    property CompassDirection: string read FCompassDirection;
    property Details: TObjectList<TdxBingMapItineraryItemDetail> read FDetails;
    property Instruction: TdxBingMapManeuverInstruction read FInstruction;
    property ManeuverPoint: TdxMapControlGeoPoint read FManeuverPoint;
    property SideOfStreet: string read FSideOfStreet;
    property TravelDistance: Double read FTravelDistance;
    property TravelDuration: Double read FTravelDuration;
    property TravelMode: string read FTravelMode;
  end;

  TdxBingMapRouteLeg = class
  private
    FActualEnd: TdxMapControlGeoPoint;
    FActualStart: TdxMapControlGeoPoint;
    FEndLocation: TdxBingMapLocationInfo;
    FItineraryItems: TObjectList<TdxBingMapItineraryItem>;
    FStartLocation: TdxBingMapLocationInfo;
    FTravelDistance: Double;
    FTravelDuration: Double;
    procedure ParseItineraryItem(ANode: TdxXmlNode);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Parse(ANode: TdxXmlNode);
    property ActualEnd: TdxMapControlGeoPoint read FActualEnd;
    property ActualStart: TdxMapControlGeoPoint read FActualStart;
    property EndLocation: TdxBingMapLocationInfo read FEndLocation;
    property ItineraryItems: TObjectList<TdxBingMapItineraryItem> read FItineraryItems;
    property StartLocation: TdxBingMapLocationInfo read FStartLocation;
    property TravelDistance: Double read FTravelDistance;
    property TravelDuration: Double read FTravelDuration;
  end;

  TdxBingMapRouteInfo = class
  private
    FBoundingBox: TdxMapControlGeoRect;
    FDistanceUnit: string;
    FDurationUnit: string;
    FTravelDistance: Double;
    FTravelDuration: Double;
    FTravelDurationTraffic: Double;
    FPath: TdxMapControlGeoPoints;
    FRouteLegs: TObjectList<TdxBingMapRouteLeg>;
  protected
    procedure ParseRouteLegNode(ANode: TdxXMLNode);
    procedure ParseRoutePathNode(ANode: TdxXMLNode);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Parse(ANode: TdxXmlNode);

    property BoundingBox: TdxMapControlGeoRect read FBoundingBox;
    property DistanceUnit: string read FDistanceUnit;
    property DurationUnit: string read FDurationUnit;
    property TravelDistance: Double read FTravelDistance;
    property TravelDuration: Double read FTravelDuration;
    property TravelDurationTraffic: Double read FTravelDurationTraffic;
    property Path: TdxMapControlGeoPoints read FPath;
    property RouteLegs: TObjectList<TdxBingMapRouteLeg> read FRouteLegs;
  end;

  TdxBingMapRouteDataServiceResponse = class(TdxBingMapLocationDataServiceResponse)
  private
    FRoutes: TObjectList<TdxBingMapRouteInfo>;
    procedure AddRoute(ANode: TdxXMLNode);
  protected
    procedure ParseResource(ANode: TdxXMLNode); override;
  public
    constructor Create;
    destructor Destroy; override;
    property Routes: TObjectList<TdxBingMapRouteInfo> read FRoutes;
  end;

  TdxBingMapRouteWaypointType = (bmwtWayPoint, bmwtViaWayPoint);

  TdxBingMapRouteWaypoint = class
  private
    FGeoPoint: TdxMapControlGeoPoint;
    FLandmark: string;
    FPointType: TdxBingMapRouteWaypointType;
  public
    constructor Create(const AGeoPoint: TdxMapControlGeoPoint); overload; virtual;
    constructor Create(const ALandmark: string); overload; virtual;
    function AsString: string; virtual;
    property PointType: TdxBingMapRouteWaypointType read FPointType write FPointType;
  end;

  TdxBingMapRouteWaypoints = class
  private
    FWaypoints: TObjectList<TdxBingMapRouteWaypoint>;
    procedure AddWaypoint(AWayPoint: TdxBingMapRouteWaypoint);
    function GetCount: Integer;
    function GetItems(Index: Integer): TdxBingMapRouteWaypoint;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(const AGeoPoint: TdxMapControlGeoPoint;
      APointType: TdxBingMapRouteWaypointType = bmwtWayPoint): TdxBingMapRouteWaypoint; overload;
    function Add(const ALandmark: string;
      APointType: TdxBingMapRouteWaypointType = bmwtWayPoint): TdxBingMapRouteWaypoint; overload;
    procedure Clear;
    property Count: Integer read GetCount;
    property Items [Index: Integer]: TdxBingMapRouteWaypoint read GetItems; default;
  end;

  TdxBingMapDistanceUnit = (bmduKilometers, bmduMiles);

  TdxBingMapRESTServiceCustomRouteParams = class(TdxBingMapRESTServiceParams)
  private
    FDistanceUnit: TdxBingMapDistanceUnit;
    FRoutePathOutput: Boolean;
    function GetDistanceUnitParam: string;
    function GetRoutePathOutputParam: string;
  protected
    function DoAsString: string; override;
  public
    constructor Create; override;
    property DistanceUnit: TdxBingMapDistanceUnit read FDistanceUnit write FDistanceUnit;
    property RoutePathOutput: Boolean read FRoutePathOutput write FRoutePathOutput;
  end;

  TdxBingMapTravelMode = (bmtmDriving, bmtmWalking, bmtmTransit);
  TdxBingMapExcludeRoadType = (bmetNone, bmetAvoid, bmetMinimize);
  TdxBingMapOptimizeRouteType = (bmorTime, bmorDistance, bmorTimeWithTraffic, bmorTimeAvoidClosure);
  TdxBingMapTransitTimeType = (bmttArrival, bmttDeparture, bmttLastAvailable);

  TdxBingMapRESTServiceRouteParams = class(TdxBingMapRESTServiceCustomRouteParams)
  private
    FDateTime: TDateTime;
    FExcludeHighways: TdxBingMapExcludeRoadType;
    FExcludeTolls: TdxBingMapExcludeRoadType;
    FMaxSolutions: Integer;
    FOptimize: TdxBingMapOptimizeRouteType;
    FTimeType: TdxBingMapTransitTimeType;
    FTravelMode: TdxBingMapTravelMode;
    FWayPointsParam: string;
    function GetAvoidParam: string;
    function GetDateTimeParam: string;
    function GetMaxSolutionsParam: string;
    function GetOptimizeParam: string;
    function GetTimeTypeParam: string;
    function GetTravelModeParam: string;
  protected
    function AsString: string; override;
    function DoAsString: string; override;
  public
    constructor Create; override;
    procedure BuildRouteWayPointsParam(AWayPoints: TdxBingMapRouteWaypoints);
    property DateTime: TDateTime read FDateTime write FDateTime;
    property ExcludeHighways: TdxBingMapExcludeRoadType read FExcludeHighways write FExcludeHighways;
    property ExcludeTolls: TdxBingMapExcludeRoadType read FExcludeTolls write FExcludeTolls;
    property MaxSolutions: Integer read FMaxSolutions write FMaxSolutions;
    property Optimize: TdxBingMapOptimizeRouteType read FOptimize write FOptimize;
    property TimeType: TdxBingMapTransitTimeType read FTimeType write FTimeType;
    property TravelMode: TdxBingMapTravelMode read FTravelMode write FTravelMode;
  end;

  TdxBingMapRESTServiceRoutesFromMajorRoadsParams = class(TdxBingMapRESTServiceCustomRouteParams)
  private
    FDestination: string;
    FExcludeRoutes: Boolean;
    function GetDestinationParam: string;
    function GetRouteExcludeParam: string;
  protected
    function DoAsString: string; override;
  public
    property Destination: string read FDestination write FDestination;
    property ExcludeRoutes: Boolean read FExcludeRoutes write FExcludeRoutes;
  end;

  TdxBingMapCustomRouteDataService = class(TdxBingMapRESTService)
  protected
    function CreateResponse: TdxRestServiceResponse; override;
  end;

  TdxBingMapRouteDataService = class(TdxBingMapCustomRouteDataService)
  protected
    function CreateParams: TdxBingMapRESTServiceParams; override;
    function GetBaseUrl: string; override;
  end;

  TdxBingMapRoutesFromMajorRoadsDataService = class(TdxBingMapCustomRouteDataService)
  protected
    function CreateParams: TdxBingMapRESTServiceParams; override;
    function GetBaseUrl: string; override;
  end;

const
  dxBingMapRouteDataManeuverType: array [Low(TdxBingMapRouteDataManeuverType)..High(TdxBingMapRouteDataManeuverType)] of string = (
    'ArriveFinish',
    'ArriveIntermediate',
    'BearLeft',
    'BearLeftThenBearLeft',
    'BearLeftThenBearRight',
    'BearLeftThenTurnLeft',
    'BearLeftThenTurnRight',
    'BearRight',
    'BearRightThenBearLeft',
    'BearRightThenBearRight',
    'BearRightThenTurnLeft',
    'BearRightThenTurnRight',
    'BearThenKeep',
    'BearThenMerge',
    'Continue',
    'DepartIntermediateStop',
    'DepartIntermediateStopReturning',
    'DepartStart',
    'EnterRoundabout',
    'ExitRoundabout',
    'EnterThenExitRoundabout',
    'KeepLeft',
    'KeepOnRampLeft',
    'KeepOnRampRight',
    'KeepOnRampStraight',
    'KeepRight',
    'KeepStraight',
    'KeepToStayLeft',
    'KeepToStayRight',
    'KeepToStayStraight',
    'Merge',
    'None',
    'RampThenHighwayLeft',
    'RampThenHighwayRight',
    'RampThenHighwayStraight',
    'RoadNameChange',
    'Take',
    'TakeRampLeft',
    'TakeRampRight',
    'TakeRampStraight',
    'TakeTransit',
    'Transfer',
    'TransitArrive',
    'TransitDepart',
    'TurnBack',
    'TurnLeft',
    'TurnLeftThenBearLeft',
    'TurnLeftThenBearRight',
    'TurnLeftThenTurnLeft',
    'TurnLeftThenTurnRight',
    'TurnRight',
    'TurnRightThenBearLeft',
    'TurnRightThenBearRight',
    'TurnRightThenTurnLeft',
    'TurnRightThenTurnRight',
    'TurnThenMerge',
    'TurnToStayLeft',
    'TurnToStayRight',
    'Unknown',
    'UTurn',
    'Wait',
    'Walk'
  );

function GetManeuverTypeByName(const AManeuver: string): TdxBingMapRouteDataManeuverType;

implementation

uses
  dxMapControlHttpRequest;

function GetManeuverTypeByName(const AManeuver: string): TdxBingMapRouteDataManeuverType;
var
  I: TdxBingMapRouteDataManeuverType;
begin
  Result := rdmtNone;
  for I := Low(Result) to High(Result) do
    if SameText(AManeuver, dxBingMapRouteDataManeuverType[I]) then
    begin
      Result := I;
      Break;
    end;
end;

{ TdxBingMapRouteWaypoint }

constructor TdxBingMapRouteWaypoint.Create(const AGeoPoint: TdxMapControlGeoPoint);
begin
  inherited Create;
  FGeoPoint := AGeoPoint;
end;

constructor TdxBingMapRouteWaypoint.Create(const ALandmark: string);
begin
  inherited Create;
  FLandmark := ALandmark;
end;

function TdxBingMapRouteWaypoint.AsString: string;
begin
  if FLandmark <> '' then
    Result := FLandmark
  else
    Result := FGeoPoint.AsString;
end;

{ TdxBingMapRouteWaypoints }

constructor TdxBingMapRouteWaypoints.Create;
begin
  inherited Create;
  FWaypoints := TObjectList<TdxBingMapRouteWaypoint>.Create;
end;

destructor TdxBingMapRouteWaypoints.Destroy;
begin
  FreeAndNil(FWaypoints);
  inherited Destroy;
end;

function TdxBingMapRouteWaypoints.Add(const AGeoPoint: TdxMapControlGeoPoint;
  APointType: TdxBingMapRouteWaypointType = bmwtWayPoint): TdxBingMapRouteWaypoint;
begin
  Result := TdxBingMapRouteWaypoint.Create(AGeoPoint);
  Result.PointType := APointType;
  AddWaypoint(Result);
end;

function TdxBingMapRouteWaypoints.Add(const ALandmark: string;
  APointType: TdxBingMapRouteWaypointType = bmwtWayPoint): TdxBingMapRouteWaypoint;
begin
  Result := TdxBingMapRouteWaypoint.Create(ALandmark);
  Result.PointType := APointType;
  AddWaypoint(Result);
end;

procedure TdxBingMapRouteWaypoints.Clear;
begin
  FWaypoints.Clear;
end;

procedure TdxBingMapRouteWaypoints.AddWaypoint(AWayPoint: TdxBingMapRouteWaypoint);
begin
  FWaypoints.Add(AWayPoint);
end;

function TdxBingMapRouteWaypoints.GetCount: Integer;
begin
  Result := FWaypoints.Count;
end;

function TdxBingMapRouteWaypoints.GetItems(
  Index: Integer): TdxBingMapRouteWaypoint;
begin
  Result := FWaypoints[Index];
end;

{ TdxBingMapRouteInfo }

constructor TdxBingMapRouteInfo.Create;
begin
  inherited Create;
  FRouteLegs := TObjectList<TdxBingMapRouteLeg>.Create;
end;

destructor TdxBingMapRouteInfo.Destroy;
begin
  FreeAndNil(FRouteLegs);
  inherited Destroy;
end;

procedure TdxBingMapRouteInfo.Parse(ANode: TdxXmlNode);
var
  I: Integer;
  ANodeName: string;
begin
  for I := 0 to ANode.Count - 1 do
  begin
    ANodeName := ANode[I].NameAsString;
    if SameText(ANodeName, 'RoutePath') then
      ParseRoutePathNode(ANode[I])
    else
      if SameText(ANodeName, 'RouteLeg') then
        ParseRouteLegNode(ANode[I])
      else
        if SameText(ANodeName, 'DistanceUnit') then
          FDistanceUnit := ANode[I].TextAsString
        else
          if SameText(ANodeName, 'DurationUnit') then
            FDurationUnit := ANode[I].TextAsString
          else
            if SameText(ANodeName, 'TravelDistance') then
              FTravelDistance := dxStrToFloat(ANode[I].TextAsString)
            else
              if SameText(ANodeName, 'TravelDuration') then
                FTravelDuration := dxStrToFloat(ANode[I].TextAsString)
              else
                if SameText(ANodeName, 'TravelDurationTraffic') then
                  FTravelDurationTraffic := dxStrToFloat(ANode[I].TextAsString)
                else
                  if SameText(ANodeName, 'BoundingBox') then
                    TdxBingMapDataXmlParser.ParseBoundingBoxNode(ANode[I], FBoundingBox);

  end;
end;

procedure TdxBingMapRouteInfo.ParseRouteLegNode(ANode: TdxXMLNode);
var
  ARouteLeg: TdxBingMapRouteLeg;
begin
  ARouteLeg := TdxBingMapRouteLeg.Create;
  ARouteLeg.Parse(ANode);
  FRouteLegs.Add(ARouteLeg);
end;

procedure TdxBingMapRouteInfo.ParseRoutePathNode(ANode: TdxXMLNode);
var
  ALineNode: TdxXMLNode;
  I, APointIndex: Integer;
begin
  if ANode.FindChild('Line', ALineNode) then
  begin
    APointIndex := 0;
    SetLength(FPath, ALineNode.Count);
    for I := 0 to ALineNode.Count - 1 do
      if SameText(ALineNode[I].NameAsString, 'Point') then
      begin
        TdxBingMapDataXmlParser.ParsePointNode(ALineNode[I], FPath[APointIndex]);
        Inc(APointIndex);
      end;
    SetLength(FPath, APointIndex);
  end;
end;

{ TdxBingMapRouteLeg }

constructor TdxBingMapRouteLeg.Create;
begin
  inherited Create;
  FItineraryItems := TObjectList<TdxBingMapItineraryItem>.Create;
end;

destructor TdxBingMapRouteLeg.Destroy;
begin
  FreeAndNil(FStartLocation);
  FreeAndNil(FEndLocation);
  FreeAndNil(FItineraryItems);
  inherited Destroy;
end;

procedure TdxBingMapRouteLeg.Parse(ANode: TdxXmlNode);
var
  I: Integer;
  ANodeName: string;
begin
  for I := 0 to ANode.Count - 1 do
  begin
    ANodeName := ANode[I].NameAsString;
    if SameText(ANodeName, 'ItineraryItem') then
      ParseItineraryItem(ANode[I])
    else
      if SameText(ANodeName, 'TravelDistance') then
        FTravelDistance := dxStrToFloat(ANode[I].TextAsString)
      else
        if SameText(ANodeName, 'TravelDuration') then
          FTravelDuration := dxStrToFloat(ANode[I].TextAsString)
        else
          if SameText(ANodeName, 'ActualStart') then
            TdxBingMapDataXmlParser.ParsePointNode(ANode[I], FActualStart)
          else
            if SameText(ANodeName, 'ActualEnd') then
              TdxBingMapDataXmlParser.ParsePointNode(ANode[I], FActualEnd)
            else
              if SameText(ANodeName, 'StartLocation') then
              begin
                FStartLocation := TdxBingMapLocationInfo.Create;
                FStartLocation.Parse(ANode[I])
              end
              else
                if SameText(ANodeName, 'EndLocation') then
                begin
                  FEndLocation := TdxBingMapLocationInfo.Create;
                  FEndLocation.Parse(ANode[I])
                end;
  end;
end;

procedure TdxBingMapRouteLeg.ParseItineraryItem(ANode: TdxXmlNode);
var
  AItem: TdxBingMapItineraryItem;
begin
  AItem := TdxBingMapItineraryItem.Create;
  AItem.Parse(ANode);
  FItineraryItems.Add(AItem);
end;

{ TdxBingMapItineraryItem }

constructor TdxBingMapItineraryItem.Create;
begin
  FChildItineraryItems := TObjectList<TdxBingMapItineraryItem>.Create;
  FDetails := TObjectList<TdxBingMapItineraryItemDetail>.Create;
end;

destructor TdxBingMapItineraryItem.Destroy;
begin
  FreeAndNil(FDetails);
  FreeAndNil(FChildItineraryItems);
  FreeAndNil(FDetails);
  FreeAndNil(FInstruction);
  inherited Destroy;
end;

procedure TdxBingMapItineraryItem.Parse(ANode: TdxXmlNode);
var
  I: Integer;
  ANodeName: string;
begin
  for I := 0 to ANode.Count - 1 do
  begin
    ANodeName := ANode[I].NameAsString;
    if SameText(ANodeName, 'TravelMode') then
      FTravelMode := ANode[I].TextAsString
    else
      if SameText(ANodeName, 'TravelDistance') then
        FTravelDistance := dxStrToFloat(ANode[I].TextAsString)
      else
        if SameText(ANodeName, 'TravelDuration') then
          FTravelDuration := dxStrToFloat(ANode[I].TextAsString)
        else
          if SameText(ANodeName, 'ManeuverPoint') then
            TdxBingMapDataXmlParser.ParsePointNode(ANode[I], FManeuverPoint)
          else
            if SameText(ANodeName, 'Instruction') then
              ParseInstruction(ANode[I])
            else
              if SameText(ANodeName, 'CompassDirection') then
                FCompassDirection := ANode[I].TextAsString
              else
                if SameText(ANodeName, 'ChildItineraryItems') then
                  ParseChildItineraryItems(ANode[I])
                else
                  if SameText(ANodeName, 'SideOfStreet') then
                    FSideOfStreet := ANode[I].TextAsString
                  else
                    if SameText(ANodeName, 'Detail') then
                      ParseDetail(ANode[I]);
  end;
end;

procedure TdxBingMapItineraryItem.ParseChildItineraryItems(ANode: TdxXmlNode);
var
  AItineraryItem: TdxBingMapItineraryItem;
begin
  AItineraryItem := TdxBingMapItineraryItem.Create;
  AItineraryItem.Parse(ANode);
  FChildItineraryItems.Add(AItineraryItem);
end;

procedure TdxBingMapItineraryItem.ParseDetail(ANode: TdxXmlNode);
var
  ADetail: TdxBingMapItineraryItemDetail;
begin
  ADetail := TdxBingMapItineraryItemDetail.Create;
  ADetail.Parse(ANode);
  FDetails.Add(ADetail);
end;

procedure TdxBingMapItineraryItem.ParseInstruction(ANode: TdxXmlNode);
begin
  FInstruction := TdxBingMapManeuverInstruction.Create;
  FInstruction.Parse(ANode);
end;

{ TdxBingMapRouteDataServiceResponse }

constructor TdxBingMapRouteDataServiceResponse.Create;
begin
  inherited Create;
  FRoutes := TObjectList<TdxBingMapRouteInfo>.Create;
end;

destructor TdxBingMapRouteDataServiceResponse.Destroy;
begin
  FreeAndNil(FRoutes);
  inherited Destroy;
end;

procedure TdxBingMapRouteDataServiceResponse.ParseResource(ANode: TdxXMLNode);
begin
  if SameText(ANode.NameAsString, 'Route') then
    AddRoute(ANode)
  else
    inherited ParseResource(ANode);
end;

procedure TdxBingMapRouteDataServiceResponse.AddRoute(ANode: TdxXMLNode);
var
  ARoute: TdxBingMapRouteInfo;
begin
  ARoute := TdxBingMapRouteInfo.Create;
  ARoute.Parse(ANode);
  FRoutes.Add(ARoute);
end;

{ TdxBingMapItineraryItemDetail }

constructor TdxBingMapItineraryItemDetail.Create;
begin
  inherited Create;
  FNames := TStringList.Create;
end;

destructor TdxBingMapItineraryItemDetail.Destroy;
begin
  FreeAndNil(FNames);
  inherited Destroy;
end;

procedure TdxBingMapItineraryItemDetail.Parse(ANode: TdxXmlNode);
var
  I: Integer;
  ANodeName: string;
begin
  for I := 0 to ANode.Count - 1 do
  begin
    ANodeName := ANode[I].NameAsString;
    if SameText(ANodeName, 'CompassDegrees') then
      FCompassDegrees := ANode[I].TextAsString
    else
      if SameText(ANodeName, 'EndPathIndex') then
        FEndPathIndex := StrToInt(ANode[I].TextAsString)
      else
        if SameText(ANodeName, 'LocationCode') then
          FLocationCode := ANode[I].TextAsString
        else
          if SameText(ANodeName, 'ManeuverType') then
            FManeuverType := GetManeuverTypeByName(ANode[I].TextAsString)
          else
            if SameText(ANodeName, 'Name') then
              FNames.Add(ANode[I].TextAsString)
            else
              if SameText(ANodeName, 'RoadType') then
                FRoadType := ANode[I].TextAsString
              else
                if SameText(ANodeName, 'StartPathIndex') then
                  FStartPathIndex := StrToInt(ANode[I].TextAsString);
  end;
end;

{ TdxBingMapManeuverInstruction }

procedure TdxBingMapManeuverInstruction.Parse(ANode: TdxXmlNode);
var
  AAttr: TdxXmlNodeAttribute;
begin
  if ANode.Attributes.Find('maneuverType', AAttr) then
    FManeuverType := AAttr.ValueAsString;
  FDescription := ANode.TextAsString;
end;

  { TdxBingMapRESTServiceCustomRouteParams }

constructor TdxBingMapRESTServiceCustomRouteParams.Create;
begin
  inherited Create;
  FRoutePathOutput := True;
end;

function TdxBingMapRESTServiceCustomRouteParams.DoAsString: string;
begin
  Result := inherited DoAsString;
  AddParamBefore(Result, GetRoutePathOutputParam);
  AddParamBefore(Result, GetDistanceUnitParam);
end;

function TdxBingMapRESTServiceCustomRouteParams.GetDistanceUnitParam: string;
begin
  Result := Format(dxBingRouteDistanceUnitParam, [dxBingRouteDistanceUnitValues[Ord(FDistanceUnit)]]);
end;

function TdxBingMapRESTServiceCustomRouteParams.GetRoutePathOutputParam: string;
begin
  Result := Format(dxBingRoutePathOutput, [dxBingRoutePathOutputValues[IfThen(FRoutePathOutput, 1)]])
end;

{ TdxBingMapRESTServiceRouteParams }

constructor TdxBingMapRESTServiceRouteParams.Create;
begin
  inherited Create;
  FMaxSolutions := 1;
end;

procedure TdxBingMapRESTServiceRouteParams.BuildRouteWayPointsParam(
  AWayPoints: TdxBingMapRouteWaypoints);
var
  I: Integer;
  AWayPoint: TdxBingMapRouteWaypoint;
begin
  FWayPointsParam := '';
  for I := 0 to AWayPoints.Count - 1 do
  begin
    AWayPoint := AWayPoints[I];
    FWayPointsParam := FWayPointsParam + IfThen(I > 0, dxUrlParamsDevider) +
      Format(IfThen(AWayPoint.PointType = bmwtWayPoint, dxBingWayPointParam, dxBingViaWayPointParam),
        [I, AWayPoint.AsString]);
  end;
end;

function TdxBingMapRESTServiceRouteParams.AsString: string;
begin
  Result := GetTravelModeParam + inherited AsString;
end;

function TdxBingMapRESTServiceRouteParams.DoAsString: string;
begin
  Result := inherited DoAsString;
  AddParamBefore(Result, GetAvoidParam);
  AddParamBefore(Result, GetOptimizeParam);
  AddParamBefore(Result, GetMaxSolutionsParam);
  if TravelMode = bmtmTransit then
  begin
    AddParamBefore(Result, GetDateTimeParam);
    AddParamBefore(Result, GetTimeTypeParam);
  end;
  AddParamBefore(Result, FWayPointsParam);
end;

function TdxBingMapRESTServiceRouteParams.GetAvoidParam: string;
const
  AExcludeHighways: array [TdxBingMapExcludeRoadType] of string = (
    '', 'highways', 'minimizeHighways');
  AFExcludeTolls: array [TdxBingMapExcludeRoadType] of string = (
    '', 'tolls', 'minimizeTolls');
var
  AAvoidHighways, AAvoidTolls: string;
begin
  AAvoidHighways := AExcludeHighways[FExcludeHighways];
  AAvoidTolls := AFExcludeTolls[FExcludeTolls];
  Result := AAvoidHighways + IfThen((AAvoidHighways <> '') and (AAvoidTolls <> ''), ',') + AAvoidTolls;
  if Result <> '' then
    Result := Format(dxBingRouteAvoidParam, [Result]);
end;

function TdxBingMapRESTServiceRouteParams.GetDateTimeParam: string;
begin
  Result := Format(dxBingRouteDateTimeParam,
    [FormatDateTime('mm"/"dd"/"yyyy%20hh":"mm":"ss', FDateTime)]);
end;

function TdxBingMapRESTServiceRouteParams.GetMaxSolutionsParam: string;
begin
  Result := Format(dxBingRouteMaxSolutionsParam, [EnsureRange(FMaxSolutions, 1, 3)]);
end;

function TdxBingMapRESTServiceRouteParams.GetOptimizeParam: string;
begin
  Result := Format(dxBingRouteOptimizeParam, [dxBingRouteOptimizationType[Ord(FOptimize)]]);
end;

function TdxBingMapRESTServiceRouteParams.GetTimeTypeParam: string;
begin
  Result := Format(dxBingRouteTimeTypeParam, [dxBingRouteTimeType[Ord(FTimeType)]]);
end;

function TdxBingMapRESTServiceRouteParams.GetTravelModeParam: string;
begin
  Result := dxBingTravelMode[Ord(FTravelMode)];
end;

{ TdxBingMapRESTServiceRoutesFromMajorRoadsParams }

function TdxBingMapRESTServiceRoutesFromMajorRoadsParams.DoAsString: string;
begin
  Result := inherited DoAsString;
  AddParamBefore(Result, GetRouteExcludeParam);
  AddParamBefore(Result, GetDestinationParam);
end;

function TdxBingMapRESTServiceRoutesFromMajorRoadsParams.GetDestinationParam: string;
begin
  Result := Format(dxBingRouteDestinationParam , [FDestination]);
end;

function TdxBingMapRESTServiceRoutesFromMajorRoadsParams.GetRouteExcludeParam: string;
begin
  if FExcludeRoutes then
    Result := dxBingRouteExcludeParam
  else
    Result := '';
end;

{ TdxBingMapCustomRouteDataService }

function TdxBingMapCustomRouteDataService.CreateResponse: TdxRestServiceResponse;
begin
  Result := TdxBingMapRouteDataServiceResponse.Create;
end;

{ TdxMapControlBingMapRouteDataProvider }

function TdxBingMapRouteDataService.CreateParams: TdxBingMapRESTServiceParams;
begin
  Result := TdxBingMapRESTServiceRouteParams.Create;
end;

function TdxBingMapRouteDataService.GetBaseUrl: string;
begin
  Result := dxBingRouteServiceAddress + '/';
end;

{ TdxMapControlBingMapRoutesFromMajorRoadsDataService }

function TdxBingMapRoutesFromMajorRoadsDataService.CreateParams: TdxBingMapRESTServiceParams;
begin
  Result := TdxBingMapRESTServiceRoutesFromMajorRoadsParams.Create;
end;

function TdxBingMapRoutesFromMajorRoadsDataService.GetBaseUrl: string;
begin
  Result := dxBingRoutesFromMajorRoadsAddress;
end;

end.
