{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressRichEditControl                                   }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSRICHEDITCONTROL AND ALL        }
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

unit dxRichEdit.Utils.TextColors;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
{$IFDEF DELPHIXE2}
  System.UITypes,
{$ENDIF}
  SysUtils, Graphics, Generics.Defaults, Generics.Collections, dxCoreGraphics;

type

  { TdxTextColors }

  TdxTextColors = class
  strict private
    FDefaultBackgroundColor: TdxAlphaColor;
    FLightTextColor: TdxAlphaColor;
    FDarkTextColor: TdxAlphaColor;
    FDarkBoundary: Integer;
    class constructor Initialize;
    class destructor Finalize;
  public class var
    Defaults: TdxTextColors;
  public const
    DefaultBoundary = 3982098;
  public
    constructor Create(ADefaultBackgroundColor: TdxAlphaColor; ALightTextColor: TdxAlphaColor; ADarkTextColor: TdxAlphaColor; ADarkBoundary: Integer);
    class function FromSkinColors(ABackgroundColor: TdxAlphaColor; ATextColor: TdxAlphaColor): TdxTextColors; static;

    property LightTextColor: TdxAlphaColor read FLightTextColor;
    property DarkTextColor: TdxAlphaColor read FDarkTextColor;
    property DefaultBackgroundColor: TdxAlphaColor read FDefaultBackgroundColor;
    property DarkBoundary: Integer read FDarkBoundary;
  end;

  { TdxAutoColorUtils }

  TdxAutoColorUtils = class
  public
    class function CalculateLumaY(AColor: TdxAlphaColor): Integer; static;
    class function IsDarkColor(AColor: TdxAlphaColor; ABoundary: Integer): Boolean; static;
    class function GetActualForeColor(ABackColor, AForeColor: TdxAlphaColor; ATextColors: TdxTextColors): TdxAlphaColor; static;
  end;

implementation

uses
  Windows, Math;

{ TdxTextColors }

constructor TdxTextColors.Create(ADefaultBackgroundColor: TdxAlphaColor; ALightTextColor: TdxAlphaColor; ADarkTextColor: TdxAlphaColor; ADarkBoundary: Integer);
begin
  inherited Create;
  FDefaultBackgroundColor := ADefaultBackgroundColor;
  FLightTextColor := ALightTextColor;
  FDarkTextColor := ADarkTextColor;
  FDarkBoundary := ADarkBoundary;
end;

class constructor TdxTextColors.Initialize;
begin
  Defaults := FromSkinColors(TdxAlphaColors.Window, TdxAlphaColors.Black);
end;

class destructor TdxTextColors.Finalize;
begin
  FreeAndNil(Defaults);
end;

class function TdxTextColors.FromSkinColors(ABackgroundColor: TdxAlphaColor; ATextColor: TdxAlphaColor): TdxTextColors;
var
  ABackgroundLuma, ATextColorLuma: Integer;
begin
  ABackgroundLuma := TdxAutoColorUtils.CalculateLumaY(ABackgroundColor);
  ATextColorLuma := TdxAutoColorUtils.CalculateLumaY(ATextColor);
  if ABackgroundLuma < ATextColorLuma then
    Result := TdxTextColors.Create(ABackgroundColor, ATextColor, TdxAlphaColors.Black, Max(ABackgroundLuma + 1, DefaultBoundary))
  else
    Result := TdxTextColors.Create(ABackgroundColor, TdxAlphaColors.White, ATextColor, Min(ABackgroundLuma - 1, DefaultBoundary));
end;

{ TdxAutoColorUtils }

class function TdxAutoColorUtils.CalculateLumaY(AColor: TdxAlphaColor): Integer;
begin
  Result := 19595 * dxGetRed(AColor) + 38470 * dxGetGreen(AColor) + 7471 * dxGetBlue(AColor);
end;

class function TdxAutoColorUtils.IsDarkColor(AColor: TdxAlphaColor; ABoundary: Integer): Boolean;
begin
  Result := CalculateLumaY(AColor) < ABoundary;
end;

class function TdxAutoColorUtils.GetActualForeColor(ABackColor, AForeColor: TdxAlphaColor; ATextColors: TdxTextColors): TdxAlphaColor;
begin
  if not TdxAlphaColors.IsEmpty(AForeColor) then
    Exit(AForeColor);
  if not TdxAlphaColors.IsEmpty(ABackColor) and (IsDarkColor(ABackColor, ATextColors.DarkBoundary)) then
    Result := ATextColors.LightTextColor
  else
    Result := ATextColors.DarkTextColor;
end;

end.
