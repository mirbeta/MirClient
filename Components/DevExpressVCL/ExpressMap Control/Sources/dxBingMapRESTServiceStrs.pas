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

unit dxBingMapRESTServiceStrs;

interface

{$I cxVer.inc}

uses
  dxXmlDoc;

const
  dxBingMapRESTServiceAddress = 'http://dev.virtualearth.net/REST/v1/';
  dxUrlParamsDevider = '&';
  dxBingCultureParam = 'c=%s';
  dxBingKeyParam = 'key=%s';
  dxBingOutputParam = 'o=%s';
  dxBingUserIpParam = 'uip=%s';
  dxBingUserLocationParam = 'ul=%s';
  dxBingUserMapViewParam = 'umv=%s';
  dxBingSuppressStatusParam = 'ss=%s';
  dxBingOutputFormats: array [0..1] of string = ('xml', 'json');
  // location service
  dxBingLocationConfidence: array [0..3] of string = ('Unknown', 'High', 'Medium', 'Low');
  dxBingLocationMatchCode: array [0..2] of string = ('Good', 'Ambiguous', 'UpHierarchy');
  dxBingLocationServiceAddress = dxBingMapRESTServiceAddress + 'Locations';
  dxBingMaxResultsParam = 'maxRes=%d';
  dxBingIncludeNeighborhoodParam = 'inclnb=%s';
  dxBingQueryParam = 'q=%s';
  dxBingEntityTypes: array [0..6] of string = ('Address', 'Neighborhood', 'PopulatedPlace', 'Postcode1',
    'AdminDivision1', 'AdminDivision2', 'CountryRegion');
  dxBingIncludeEntityTypesParam = 'includeEntityTypes=%s';
  // imagery service
  dxBingImageryMetadataServiceAddress = dxBingMapRESTServiceAddress + 'Imagery/Metadata';
  // route service
  dxBingRouteServiceAddress = dxBingMapRESTServiceAddress + 'Routes';
  dxBingWayPointParam = 'wp.%d=%s';
  dxBingViaWayPointParam = 'vwp.%d=%s';
  dxBingTravelMode: array [0..2] of string = ('Driving', 'Walking', 'Transit');
  dxBingRoutePathOutputValues: array [0..1] of string = ('None', 'Points');
  dxBingRoutePathOutput = 'rpo=%s';
  dxBingRouteAvoidParam = 'avoid=%s';
  dxBingRouteDistanceBeforeFirstTurnParam = 'dbft=%d';
  dxBingRouteHeadingParam = 'hd=%d';
  dxBingRouteOptimizationType: array [0..3] of string = ('Time', 'Distance', 'TimeWithTraffic', 'TimeAvoidClosure');
  dxBingRouteOptimizeParam = 'optmz=%s';
  dxBingRouteTolerancesParam = 'tl=%s';
  dxBingRouteDistanceUnitParam = 'du=%s';
  dxBingRouteDistanceUnitValues: array [0..1] of string = ('km', 'mi');
  dxBingRouteDateTimeParam = 'dt=%s';
  dxBingRouteTimeType: array [0..2] of string = ('Arrival', 'Departure', 'LastAvailable');
  dxBingRouteTimeTypeParam = 'tt=%s';
  dxBingRouteMaxSolutionsParam = 'maxSolns=%d';
  // routes from major roads
  dxBingRoutesFromMajorRoadsAddress = dxBingRouteServiceAddress + '/FromMajorRoads';
  dxBingRouteDestinationParam = 'dest=%s';
  dxBingRouteExcludeParam = 'excl=routes';

  // xml response
  dxBingResponse = 'Response';
  dxBingResourceSets = 'ResourceSets';
  dxBingResourceSet = 'ResourceSet';
  dxBingResources = 'Resources';
  dxBingResourcesXmlPath: array [0..3] of TdxXmlString = (dxBingResponse, dxBingResourceSets,
    dxBingResourceSet, dxBingResources);
  dxBingStatusCode = 'StatusCode';
  dxBingStatusCodeXmlPath: array [0..1] of TdxXmlString = (dxBingResponse, dxBingStatusCode);
  dxBingStatusCodeOkValue = '200';

implementation

end.
