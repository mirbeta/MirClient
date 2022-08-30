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

unit dxBingMapLocationDataService;

interface

{$I cxVer.inc}

uses
  SysUtils, StrUtils, Classes, Generics.Defaults, Generics.Collections, Math,
  dxCore, dxXMLDoc, dxCustomTree,
  dxMapControlTypes, dxBingMapRESTService, dxRESTService, dxBingMapRESTServiceStrs;

type
  TdxBingMapAddress = class
  private
    FAddressLine: string;
    FAdminDistrict: string;
    FAdminDistrict2: string;
    FCountryRegion: string;
    FCountryRegionIso2: string;
    FFormattedAddress: string;
    FLandmark: string;
    FLocality: string;
    FNeighborhood: string;
    FPostalCode: string;
  public
    function GetDisplayText(const AEntityType: string): string;
    procedure Parse(ANode: TdxXMLNode);
    property AddressLine: string read FAddressLine;
    property AdminDistrict: string read FAdminDistrict;
    property AdminDistrict2: string read FAdminDistrict2;
    property CountryRegion: string read FCountryRegion;
    property CountryRegionIso2: string read FCountryRegionIso2;
    property FormattedAddress: string read FFormattedAddress;
    property Landmark: string read FLandmark;
    property Locality: string read FLocality;
    property Neighborhood: string read FNeighborhood;
    property PostalCode: string read FPostalCode;
  end;

  TdxBingMapLocationConfidence = (bmlcUnknown, bmlcHigh, bmlcMedium, bmlcLow);
  TdxBingMapLocationMatchCode = (bmmcGood, bmmcAmbiguous, bmmcUpHierarchy);
  TdxBingMapLocationMatchCodes = set of TdxBingMapLocationMatchCode;

  TdxBingMapLocationInfo = class
  private
    FAddress: TdxBingMapAddress;
    FBoundingBox: TdxMapControlGeoRect;
    FConfidence: TdxBingMapLocationConfidence;
    FEntityType: string;
    FMatchCode: TdxBingMapLocationMatchCodes;
    FName: string;
    FPoint: TdxMapControlGeoPoint;
    function GetConfidence(const AValue: string): TdxBingMapLocationConfidence;
    function GetMatchCode(const AValue: string): TdxBingMapLocationMatchCodes;
  protected
    procedure ParseAddressNode(ANode: TdxXMLNode);
  public
    constructor Create;
    destructor Destroy; override;
    function GetDisplayText: string;
    procedure Parse(ANode: TdxXmlNode);
    property Address: TdxBingMapAddress read FAddress;
    property BoundingBox: TdxMapControlGeoRect read FBoundingBox;
    property Confidence: TdxBingMapLocationConfidence read FConfidence;
    property EntityType: string read FEntityType;
    property MatchCode: TdxBingMapLocationMatchCodes read FMatchCode;
    property Name: string read FName;
    property Point: TdxMapControlGeoPoint read FPoint;
  end;

  TdxBingMapLocationDataServiceResponse = class(TdxBingMapResponse)
  private
    FLocations: TObjectList<TdxBingMapLocationInfo>;
    procedure AddLocation(ANode: TdxXMLNode);
  protected
    procedure ParseResource(ANode: TdxXMLNode); override;
  public
    constructor Create;
    destructor Destroy; override;
    property Locations: TObjectList<TdxBingMapLocationInfo> read FLocations;
  end;

  TdxBingMapRESTServiceLocationParams = class(TdxBingMapRESTServiceParams)
  private
    FIncludeNeighborhood: Boolean;
 //   FInclude: string;
    function GetIncludeNeighborhoodParam: string;
  protected
    function DoAsString: string; override;
  public
    constructor Create; override;
    property IncludeNeighborhood: Boolean read FIncludeNeighborhood write FIncludeNeighborhood;
  //  property Include: string read FInclude write FInclude;
  end;

  TdxBingMapIncludeEntityType = (bmietAddress, bmietNeighborhood, bmietPopulatedPlace, bmietPostcode1,
    bmietAdminDivision1, bmietAdminDivision2, bmietCountryRegion);
  TdxBingMapIncludeEntityTypes = set of TdxBingMapIncludeEntityType;

  TdxBingMapRESTServiceLocationByPointParams = class(TdxBingMapRESTServiceLocationParams)
  private
    FIncludeEntityTypes: TdxBingMapIncludeEntityTypes;
    FPoint: TdxMapControlGeoPoint;
    function GetIncludeEntityTypesParam: string;
    function GetPointParam: string;
  protected
    function AsString: string; override;
    function DoAsString: string; override;
  public
    constructor Create; override;
    property IncludeEntityTypes: TdxBingMapIncludeEntityTypes read FIncludeEntityTypes write FIncludeEntityTypes;
    property Point: TdxMapControlGeoPoint read FPoint write FPoint;
  end;

  TdxBingMapRESTServiceLocationByQueryParams = class(TdxBingMapRESTServiceLocationParams)
  private
    FMaxResults: Integer;
    FQuery: string;
    function GetMaxResultsParam: string;
    function GetQueryParam: string;
  protected
    function DoAsString: string; override;
  public
    property MaxResults: Integer read FMaxResults write FMaxResults;
    property Query: string read FQuery write FQuery;
  end;

  TdxBingMapCustomLocationDataService = class(TdxBingMapRESTService)
  protected
    function CreateResponse: TdxRestServiceResponse; override;
    function GetBaseUrl: string; override;
  end;

  TdxBingMapLocationByQueryDataService = class(TdxBingMapCustomLocationDataService)
  protected
    function CreateParams: TdxBingMapRESTServiceParams; override;
  end;

  TdxBingMapLocationByPointDataService = class(TdxBingMapCustomLocationDataService)
  protected
    function CreateParams: TdxBingMapRESTServiceParams; override;
    function GetBaseUrl: string; override;
  end;

const
  AllEntityTypes = [Low(TdxBingMapIncludeEntityType)..High(TdxBingMapIncludeEntityType)];

implementation

uses
  dxMapControlHttpRequest;

{ TdxBingMapLocationInfo }

constructor TdxBingMapLocationInfo.Create;
begin
  inherited;
end;

destructor TdxBingMapLocationInfo.Destroy;
begin
  FreeAndNil(FAddress);
  inherited;
end;

function TdxBingMapLocationInfo.GetDisplayText: string;
begin
  if Address <> nil then
    Result := Address.GetDisplayText(EntityType)
  else
    Result := Name;
end;

procedure TdxBingMapLocationInfo.Parse(ANode: TdxXmlNode);
var
  ATempNode: TdxXmlNode;
begin
  if ANode.FindChild('Name', ATempNode) then
    FName := ATempNode.TextAsString;
  if ANode.FindChild('Point', ATempNode) then
    TdxBingMapDataXmlParser.ParsePointNode(ATempNode, FPoint);
  if ANode.FindChild('Address', ATempNode) then
    ParseAddressNode(ATempNode);
  if ANode.FindChild('EntityType', ATempNode) then
    FEntityType := ATempNode.TextAsString;
  if ANode.FindChild('BoundingBox', ATempNode) then
    TdxBingMapDataXmlParser.ParseBoundingBoxNode(ATempNode, FBoundingBox);
  if ANode.FindChild('Confidence', ATempNode) then
    FConfidence := GetConfidence(ATempNode.TextAsString);
  if ANode.FindChild('MatchCode', ATempNode) then
    FMatchCode := GetMatchCode(ATempNode.TextAsString);
end;

procedure TdxBingMapLocationInfo.ParseAddressNode(ANode: TdxXMLNode);
begin
  if FAddress = nil then
    FAddress := TdxBingMapAddress.Create;
  FAddress.Parse(ANode);
end;

function TdxBingMapLocationInfo.GetConfidence(const AValue: string): TdxBingMapLocationConfidence;
var
  I: Integer;
begin
  Result := bmlcUnknown;
  for I := 1 to High(dxBingLocationConfidence) do
    if SameText(dxBingLocationConfidence[I], AValue) then
    begin
      Result := TdxBingMapLocationConfidence(I);
      Break;
    end;
end;

function TdxBingMapLocationInfo.GetMatchCode(
  const AValue: string): TdxBingMapLocationMatchCodes;
var
  I: Integer;
begin
  Result := [];
  for I := 0 to High(dxBingLocationMatchCode) do
    if Pos(dxBingLocationMatchCode[I], AValue) <> 0 then
    begin
      Include(Result, TdxBingMapLocationMatchCode(I));
      Break;
    end;
end;

{ TdxBingMapAddress }

function TdxBingMapAddress.GetDisplayText(const AEntityType: string): string;

  function AddToDisplayText(var ADisplayText: string; const AAddressPart: string): Boolean;
  begin
    Result := AAddressPart <> '';
    if Result then
      ADisplayText := ADisplayText + IfThen(ADisplayText <> '', ', ') + AAddressPart;
  end;

begin
  Result := '';
  if SameText(AEntityType, 'Address') then
    AddToDisplayText(Result, FormattedAddress)
  else
  begin
    AddToDisplayText(Result, Landmark);
    AddToDisplayText(Result, Neighborhood);
    AddToDisplayText(Result, Locality);
    if not AddToDisplayText(Result, AdminDistrict2) then
      AddToDisplayText(Result, AdminDistrict);
    AddToDisplayText(Result, CountryRegion);
  end;
end;

procedure TdxBingMapAddress.Parse(ANode: TdxXMLNode);
var
  ATempNode: TdxXmlNode;
begin
  if ANode.FindChild('AddressLine', ATempNode) then
    FAddressLine := ATempNode.TextAsString;
  if ANode.FindChild('AdminDistrict', ATempNode) then
    FAdminDistrict := ATempNode.TextAsString;
  if ANode.FindChild('AdminDistrict2', ATempNode) then
    FAdminDistrict2 := ATempNode.TextAsString;
  if ANode.FindChild('CountryRegion', ATempNode) then
    FCountryRegion := ATempNode.TextAsString;
  if ANode.FindChild('CountryRegionIso2', ATempNode) then
    FCountryRegionIso2 := ATempNode.TextAsString;
  if ANode.FindChild('FormattedAddress', ATempNode) then
    FFormattedAddress := ATempNode.TextAsString;
  if ANode.FindChild('Landmark', ATempNode) then
    FLandmark := ATempNode.TextAsString;
  if ANode.FindChild('Locality', ATempNode) then
    FLocality := ATempNode.TextAsString;
  if ANode.FindChild('Neighborhood', ATempNode) then
    FNeighborhood := ATempNode.TextAsString;
  if ANode.FindChild('PostalCode', ATempNode) then
    FPostalCode := ATempNode.TextAsString;
end;

{ TdxBingMapRESTServiceLocationByQueryParams }

function TdxBingMapRESTServiceLocationByQueryParams.DoAsString: string;
begin
  Result := inherited DoAsString;
  AddParamBefore(Result, GetMaxResultsParam);
  AddParamBefore(Result, GetQueryParam);
end;

function TdxBingMapRESTServiceLocationByQueryParams.GetMaxResultsParam: string;
begin
  if FMaxResults <> 0 then
    Result := Format(dxBingMaxResultsParam, [FMaxResults])
  else
    Result := '';
end;

function TdxBingMapRESTServiceLocationByQueryParams.GetQueryParam: string;
begin
  Result := Format(dxBingQueryParam, [FQuery]);
end;

{ TdxBingMapRESTServiceLocationByPointParams }

function TdxBingMapRESTServiceLocationByPointParams.AsString: string;
begin
  Result := GetPointParam + inherited AsString;
end;

constructor TdxBingMapRESTServiceLocationByPointParams.Create;
begin
  inherited Create;
  FIncludeEntityTypes := [];
end;

function TdxBingMapRESTServiceLocationByPointParams.DoAsString: string;
begin
  Result := inherited DoAsString;
  AddParamBefore(Result, GetIncludeEntityTypesParam);
end;

function TdxBingMapRESTServiceLocationByPointParams.GetIncludeEntityTypesParam: string;
var
  I: TdxBingMapIncludeEntityType;
  AIncludeEntityTypes: string;
begin
  AIncludeEntityTypes := '';
  for I := Low(TdxBingMapIncludeEntityType) to High(TdxBingMapIncludeEntityType) do
    if I in FIncludeEntityTypes then
      AIncludeEntityTypes := AIncludeEntityTypes +
        IfThen(AIncludeEntityTypes <> '', ',') + dxBingEntityTypes[Ord(I)];
  if AIncludeEntityTypes <> '' then
    Result := Format(dxBingIncludeEntityTypesParam, [AIncludeEntityTypes])
  else
    Result := '';
end;

function TdxBingMapRESTServiceLocationByPointParams.GetPointParam: string;
begin
  Result := FPoint.AsString;
end;

{ TdxBingMapRESTServiceLocationParams }

constructor TdxBingMapRESTServiceLocationParams.Create;
begin
  inherited Create;
  FIncludeNeighborhood := True;
end;

function TdxBingMapRESTServiceLocationParams.DoAsString: string;
begin
  Result := inherited DoAsString;
  AddParamBefore(Result, GetIncludeNeighborhoodParam);
end;

function TdxBingMapRESTServiceLocationParams.GetIncludeNeighborhoodParam: string;
const
  AIncludeNeighborhood: array [Boolean] of string = ('0', '1');
begin
  Result := Format(dxBingIncludeNeighborhoodParam, [AIncludeNeighborhood[FIncludeNeighborhood]]);
end;

{ TdxBingMapLocationDataServiceResponse }

constructor TdxBingMapLocationDataServiceResponse.Create;
begin
  inherited Create;
  FLocations := TObjectList<TdxBingMapLocationInfo>.Create;
end;

destructor TdxBingMapLocationDataServiceResponse.Destroy;
begin
  FreeAndNil(FLocations);
  inherited Destroy;
end;

procedure TdxBingMapLocationDataServiceResponse.ParseResource(
  ANode: TdxXMLNode);
begin
  if SameText(ANode.NameAsString, 'Location') then
    AddLocation(ANode)
end;

procedure TdxBingMapLocationDataServiceResponse.AddLocation(ANode: TdxXMLNode);
var
  ALocation: TdxBingMapLocationInfo;
begin
  ALocation := TdxBingMapLocationInfo.Create;
  ALocation.Parse(ANode);
  FLocations.Add(ALocation);
end;

{ TdxBingMapCustomLocationDataService }

function TdxBingMapCustomLocationDataService.CreateResponse: TdxRestServiceResponse;
begin
  Result := TdxBingMapLocationDataServiceResponse.Create;
end;

function TdxBingMapCustomLocationDataService.GetBaseUrl: string;
begin
  Result := dxBingLocationServiceAddress;
end;

{ TdxBingMapLocationByQueryDataService }

function TdxBingMapLocationByQueryDataService.CreateParams: TdxBingMapRESTServiceParams;
begin
  Result := TdxBingMapRESTServiceLocationByQueryParams.Create;
end;

{ TdxBingMapLocationByPointDataService }

function TdxBingMapLocationByPointDataService.CreateParams: TdxBingMapRESTServiceParams;
begin
  Result := TdxBingMapRESTServiceLocationByPointParams.Create;
end;

function TdxBingMapLocationByPointDataService.GetBaseUrl: string;
begin
  Result := inherited GetBaseUrl + '/';
end;

end.
