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

unit dxMapControlBingImageryServiceInfo;

interface

{$I cxVer.inc}

uses
  Math, SysUtils, Classes,
  dxCore, dxBingMapImageryDataService, dxBingMapRESTService, dxRESTService;

const
  dxBingMapDefaultTileSize = 256;

type
  TdxMapControlBingImageryServiceInfo = class
  private
    FImageUrlTemplate: string;
    FIsValid: Boolean;
    FMaxZoomLevel: Integer;
    FSubdomains: TStrings;
    FTileHeight: Integer;
    FTileWidth: Integer;
    function GetSubdomain(I: Integer): string;
    function GetImageHeight: Integer;
    function GetImageWidth: Integer;
    function GetSubdomainCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure DoOnResponse(Sender: TObject; AResponse: TdxRestServiceResponse; var ADestroyResponse: Boolean);

    property ImageHeight: Integer read GetImageHeight;
    property ImageUrlTemplate: string read FImageUrlTemplate;
    property ImageWidth: Integer read GetImageWidth;
    property IsValid: Boolean read FIsValid;
    property Subdomain [I: Integer]: string read GetSubdomain;
    property SubdomainCount: Integer read GetSubdomainCount;
    property TileHeight: Integer read FTileHeight;
    property TileWidth: Integer read FTileWidth;
  end;

implementation

const
  DefaultMaxZoomLevel = 21;

{ BingImageryServiceInfo }

constructor TdxMapControlBingImageryServiceInfo.Create;
begin
  inherited Create;
  FTileHeight := dxBingMapDefaultTileSize;
  FTileWidth := dxBingMapDefaultTileSize;
  FMaxZoomLevel := DefaultMaxZoomLevel;
  FSubdomains := TStringList.Create;
end;

destructor TdxMapControlBingImageryServiceInfo.Destroy;
begin
  FreeAndNil(FSubdomains);
  inherited Destroy;
end;

procedure TdxMapControlBingImageryServiceInfo.DoOnResponse(Sender: TObject;
  AResponse: TdxRestServiceResponse; var ADestroyResponse: Boolean);
var
  AMetaData: TdxBingMapImageryMetadata;
begin
  FIsValid := (AResponse <> nil) and AResponse.IsSuccess;
  if FIsValid then
  begin
    AMetaData := (AResponse as TdxBingMapImageryDataServiceResponse).ImageryMetadata;
    FIsValid := (AMetaData <> nil) and
      (AMetaData.ImageUrl <> '') and
      (AMetaData.ImageUrlSubdomains.Count > 0);
    if FIsValid then
    begin
      FImageUrlTemplate := AMetaData.ImageUrl;
      FSubdomains.Assign(AMetaData.ImageUrlSubdomains);
      FTileWidth := AMetaData.ImageWidth;
      if FTileWidth = 0 then
        FTileWidth := dxBingMapDefaultTileSize;
      FTileHeight := AMetaData.ImageHeight;
      if FTileHeight = 0 then
        FTileHeight := dxBingMapDefaultTileSize;
      FMaxZoomLevel := AMetaData.ZoomMax;
      if FMaxZoomLevel = 0 then
        FMaxZoomLevel := DefaultMaxZoomLevel;
    end;
  end;
end;

function TdxMapControlBingImageryServiceInfo.GetImageHeight: Integer;
begin
  Result := Round(Power(2.0, FMaxZoomLevel) * TileHeight);
end;

function TdxMapControlBingImageryServiceInfo.GetImageWidth: Integer;
begin
  Result := Round(Power(2.0, FMaxZoomLevel) * TileWidth);
end;

function TdxMapControlBingImageryServiceInfo.GetSubdomain(I: Integer): string;
begin
  Result := FSubdomains[I];
end;

function TdxMapControlBingImageryServiceInfo.GetSubdomainCount: Integer;
begin
  Result := FSubdomains.Count;
end;

end.
