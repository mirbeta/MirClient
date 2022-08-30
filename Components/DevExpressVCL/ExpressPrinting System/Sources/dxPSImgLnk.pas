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
{   EXECUTABLE PROGRAM ONLY.                                         }
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

unit dxPSImgLnk;

interface

{$I cxVer.inc}

uses
  Graphics, ExtCtrls, dxPSGraphicLnk;

type
  TdxImageReportLink = class(TCustomdxPictureReportLink)
  private
    function GetImage: TImage;
  protected
    procedure InternalRestoreFromOriginal; override;

    function GetGraphicHeight: Integer; override;
    function GetGraphicWidth: Integer; override;
    function GetPicture: TPicture; override;
  public
    function DataProviderPresent: Boolean; override;
    property Image: TImage read GetImage;
  published
    property BorderColor;
    property Center;
    property DrawBorder;
    property Proportional;
    property Stretch;
    property Transparent;
    property TransparentColor;
  end;

implementation

uses
  dxPSCore;

function TdxImageReportLink.DataProviderPresent: Boolean;
begin
  if DataSource = rldsComponent then
    Result := (Image <> nil) and not IsPictureEmpty(Image.Picture)
  else
    Result := inherited DataProviderPresent;
end;

procedure TdxImageReportLink.InternalRestoreFromOriginal;
begin
  inherited InternalRestoreFromOriginal;
  Center := Image.Center;
  Proportional := Image.Proportional;
  Stretch := Image.Stretch;
end;

function TdxImageReportLink.GetGraphicHeight: Integer;
begin
  if Image <> nil then
  begin
    Result := Image.Height;
    // 3.2
    //if IsAggregated and (Result < inherited GetGraphicHeight) then
    //  Result := inherited GetGraphicHeight;
  end
  else
    Result := inherited GetGraphicHeight;
end;

function TdxImageReportLink.GetGraphicWidth: Integer;
begin
  if Image <> nil then
  begin
    Result := Image.Width;
    // 3.2
    //if IsAggregated and (Result < inherited GetGraphicWidth) then
    //  Result := inherited GetGraphicWidth;
  end
  else
    Result := inherited GetGraphicWidth;
end;

function TdxImageReportLink.GetPicture: TPicture;
begin
  if Image <> nil then
    Result := Image.Picture
  else
    Result := inherited GetPicture;
end;

function TdxImageReportLink.GetImage: TImage;
begin
  Result := TImage(Component);
end;

initialization
  dxPSRegisterReportLink(TdxImageReportLink, TImage, nil);

finalization
  dxPSUnregisterReportLink(TdxImageReportLink, TImage, nil);

end.
