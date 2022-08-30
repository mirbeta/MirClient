{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressPrinting System                                   }
{                                                                    }
{           Copyright (C) 1998-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSPRINTING SYSTEM AND            }
{   ALL ACCOMPANYING VCL CONTROLS AS PART OF AN                      }
{   EXECUTABLE PROGRAM ONLY                                          }
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

unit dxPSdxMapControlLnk;

{$I cxVer.inc}

interface

uses
  Classes, Windows, Graphics, dxPSCore, dxPSGraphicLnk, dxMapControl;

type
  { TdxMapControlCustomReportLink }

  TdxMapControlCustomReportLink = class(TCustomdxGraphicReportLink)
  private
    function GetMapControl: TdxCustomMapControl;
  protected
    function GetGraphic: TGraphic; override;
    function GetGraphicClass: TGraphicClass; override;
    function GetGraphicHeight: Integer; override;
    function GetGraphicWidth: Integer; override;

    property MapControl: TdxCustomMapControl read GetMapControl;
  end;

  { TdxMapControlReportLink }

  TdxMapControlReportLink = class(TdxMapControlCustomReportLink)
  end;

implementation

uses
  SysUtils, Types, Controls, dxPrnPg, cxGraphics;

type
  TdxCustomMapControlAccess = class(TdxCustomMapControl);

{ TdxMapControlCustomReportLink }

function TdxMapControlCustomReportLink.GetGraphic: TGraphic;
begin
  Result := TBitmap.Create;
  TBitmap(Result).SetSize(MapControl.Width, MapControl.Height);
  TBitmap(Result).PixelFormat := pf32bit;
  MapControl.PaintTo(TBitmap(Result).Canvas, 0, 0);
end;

function TdxMapControlCustomReportLink.GetGraphicClass: TGraphicClass;
begin
  Result := TBitmap;
end;

function TdxMapControlCustomReportLink.GetGraphicHeight: Integer;
begin
  if MapControl <> nil then
    Result := MapControl.Height
  else
    Result := inherited GetGraphicHeight;
end;

function TdxMapControlCustomReportLink.GetGraphicWidth: Integer;
begin
  if MapControl <> nil then
    Result := MapControl.Width
  else
    Result := inherited GetGraphicWidth;
end;

function TdxMapControlCustomReportLink.GetMapControl: TdxCustomMapControl;
begin
  Result := inherited Component as TdxCustomMapControl;
end;

initialization
  dxPSRegisterReportLink(TdxMapControlReportLink, TdxMapControl, nil);

finalization
  dxPSUnregisterReportLink(TdxMapControlReportLink, TdxMapControl, nil);

end.
