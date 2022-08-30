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

unit dxBingMapImageryDataService;

interface

{$I cxVer.inc}

uses
  SysUtils, StrUtils, Classes, Math,
  dxCore, dxXMLDoc, dxCustomTree,
  dxBingMapRESTService, dxRESTService, dxBingMapRESTServiceStrs;

type
  TdxBingMapRESTServiceImageryParams = class(TdxBingMapRESTServiceParams)
  private
    FImagerySetKind: string;
  protected
    function AsString: string; override;
  public
    property ImagerySetKind: string read FImagerySetKind write FImagerySetKind;
  end;

  TdxBingMapImageryMetadata = class
  private
    FImageUrl: string;
    FImageUrlSubdomains: TStrings;
    FImageWidth: Integer;
    FImageHeight: Integer;
    FZoomMax: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Parse(ANode: TdxXMLNode);
    property ImageUrl: string read FImageUrl;
    property ImageUrlSubdomains: TStrings read FImageUrlSubdomains;
    property ImageWidth: Integer read FImageWidth;
    property ImageHeight: Integer read FImageHeight;
    property ZoomMax: Integer read FZoomMax;
  end;

  TdxBingMapImageryDataServiceResponse = class(TdxBingMapResponse)
  private
    FImageryMetadata: TdxBingMapImageryMetadata;
  protected
    procedure ParseResource(ANode: TdxXMLNode); override;
  public
    destructor Destroy; override;
    property ImageryMetadata: TdxBingMapImageryMetadata read FImageryMetadata;
  end;

  TdxBingMapImageryDataService = class(TdxBingMapRESTService)
  protected
    function CreateParams: TdxBingMapRESTServiceParams; override;
    function CreateResponse: TdxRestServiceResponse; override;
    function GetBaseUrl: string; override;
  public
    procedure RequestImageryData(const AImagerySetKind: string);
  end;

implementation

{ TdxBingMapImageryDataService }

function TdxBingMapImageryDataService.GetBaseUrl: string;
begin
  Result := dxBingImageryMetadataServiceAddress + '/';
end;

procedure TdxBingMapImageryDataService.RequestImageryData(
  const AImagerySetKind: string);
begin
  (Params as TdxBingMapRESTServiceImageryParams).ImagerySetKind := AImagerySetKind;
  Execute;
end;

function TdxBingMapImageryDataService.CreateParams: TdxBingMapRESTServiceParams;
begin
  Result := TdxBingMapRESTServiceImageryParams.Create;
end;

function TdxBingMapImageryDataService.CreateResponse: TdxRestServiceResponse;
begin
  Result := TdxBingMapImageryDataServiceResponse.Create;
end;

{ TdxBingMapImageryDataServiceResponse }

destructor TdxBingMapImageryDataServiceResponse.Destroy;
begin
  FreeAndNil(FImageryMetadata);
  inherited Destroy;
end;

procedure TdxBingMapImageryDataServiceResponse.ParseResource(
  ANode: TdxXMLNode);
begin
  if SameText(ANode.NameAsString, 'ImageryMetadata') then
  begin
    FImageryMetadata := TdxBingMapImageryMetadata.Create;
    FImageryMetadata.Parse(ANode);
  end;
end;

{ TdxBingMapImageryMetadata }

constructor TdxBingMapImageryMetadata.Create;
begin
  inherited Create;
  FImageUrlSubdomains := TStringList.Create;
end;

destructor TdxBingMapImageryMetadata.Destroy;
begin
  FreeAndNil(FImageUrlSubdomains);
  inherited Destroy;
end;

procedure TdxBingMapImageryMetadata.Parse(ANode: TdxXMLNode);
var
  ATempNode: TdxXMLNode;
begin
  if ANode.FindChild('ImageUrl', ATempNode) then
    FImageUrl := ATempNode.TextAsString;
  if ANode.FindChild('ImageUrlSubdomains', ATempNode) then
  begin
    ATempNode.ForEach(
      procedure (ANode: TdxXMLNode; AData: Pointer)
      begin
        FImageUrlSubdomains.Add(ANode.TextAsString);
      end);
  end;
  if ANode.FindChild('ImageWidth', ATempNode) then
    FImageWidth := StrToInt(ATempNode.TextAsString);
  if ANode.FindChild('ImageHeight', ATempNode) then
    FImageHeight := StrToInt(ATempNode.TextAsString);
  if ANode.FindChild('ZoomMax', ATempNode) then
    FZoomMax:= StrToInt(ATempNode.TextAsString);
end;

{ TdxBingMapRESTServiceImageryParams }

function TdxBingMapRESTServiceImageryParams.AsString: string;
begin
  Result := FImagerySetKind + inherited AsString;
end;

end.
