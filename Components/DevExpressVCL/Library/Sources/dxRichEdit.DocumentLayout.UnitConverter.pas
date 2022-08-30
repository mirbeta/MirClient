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

unit dxRichEdit.DocumentLayout.UnitConverter;

interface

{$I cxVer.inc}
{$I dxRichEditControl.inc}

uses
  Types, dxCoreClasses, cxGeometry;

type
  TdxDocumentLayoutUnit = (
      Document,
      Twip,
      Pixel
    );

  TdxLayoutGraphicsUnit = (Pixel = 2, Point = 3, Document = 5);

  { TdxDpiSupport }

  TdxDpiSupport = class abstract (TcxIUnknownObject)
  strict private
    FScreenDpiX: Single;
    FScreenDpiY: Single;
    FScreenDpi: Single;
  public
    constructor Create(AScreenDpiX: Single; AScreenDpiY: Single; AScreenDpi: Single); overload;
    constructor Create(AScreenDpiX: Single; AScreenDpiY: Single); overload;
    constructor Create; overload;

    property ScreenDpiX: Single read FScreenDpiX;
    property ScreenDpiY: Single read FScreenDpiY;
    property ScreenDpi: Single read FScreenDpi;
  end;

  { TdxDocumentLayoutUnitConverter }

  TdxDocumentLayoutUnitConverter = class abstract (TdxDpiSupport)
  protected
    function GetDpi: Single; virtual; abstract;
    function GetFontSizeScale: Single; virtual; abstract;
    function GetFontSizeScaleForPrinting: Single; virtual;
    function GetFontUnit: TdxLayoutGraphicsUnit; virtual; abstract;
    function GetGraphicsPageScale: Single; virtual; abstract;
    function GetGraphicsPageUnit: TdxLayoutGraphicsUnit; virtual; abstract;
  public
    function Equals(Obj: TObject): Boolean; override;

    class function CreateConverter(const AUnit: TdxDocumentLayoutUnit; const ADpi: Single): TdxDocumentLayoutUnitConverter;
    function DocumentsToFontUnitsF(const AValue: Single): Single; virtual; abstract;
    function DocumentsToLayoutUnits(const AValue: Integer): Integer; overload; virtual; abstract;
    function DocumentsToLayoutUnits(const AValue: TdxRectF): TdxRectF; overload; virtual; abstract;
    function DocumentsToLayoutUnits(const AValue: TRect): TRect; overload; virtual; abstract;
    function InchesToFontUnitsF(const AValue: Single): Single; virtual; abstract;
    function LayoutUnitsToDocuments(const AValue: TdxRectF): TdxRectF; overload; virtual; abstract;
    function LayoutUnitsToDocuments(const AValue: TRect): TRect; overload; virtual; abstract;
    function LayoutUnitsToHundredthsOfInch(const AValue: Integer): Integer; overload; virtual; abstract;
    function LayoutUnitsToHundredthsOfInch(const AValue: TSize): TSize; overload; virtual; abstract;
    function LayoutUnitsToPixels(const AValue: Integer; const ADpi: Single): Integer; overload; virtual; abstract;
    function LayoutUnitsToPixels(const AValue: Integer): Integer; overload;
    function LayoutUnitsToPixels(const AValue: TSize): TSize; overload;
    function LayoutUnitsToPixels(const AValue: TSize; const ADpiX, ADpiY: Single): TSize; overload; virtual; abstract;
    function LayoutUnitsToPixels(const AValue: TPoint; const ADpiX, ADpiY: Single): TPoint; overload; virtual; abstract;
    function LayoutUnitsToPixels(const AValue: TRect; const ADpiX, ADpiY: Single): TRect; overload; virtual; abstract;
    function LayoutUnitsToPixelsF(const AValue: Single; const ADpi: Single): Single; virtual; abstract;
    function LayoutUnitsToPointsF(const AValue: Single): Single; virtual; abstract;
    function LayoutUnitsToTwips(const AValue: Int64): Int64; overload; virtual; abstract;
    function LayoutUnitsToTwips(const AValue: Integer): Integer; overload; virtual; abstract;
    function MillimetersToFontUnitsF(const AValue: Single): Single; virtual; abstract;
    function PixelsToLayoutUnits(const AValue: Integer): Integer; overload;
    function PixelsToLayoutUnits(const AValue: Integer; const ADpi: Single): Integer; overload; virtual; abstract;
    function PixelsToLayoutUnits(const AValue: TPoint; const ADpiX, ADpiY: Single): TPoint; overload; virtual;
    function PixelsToLayoutUnits(const AValue: TSize; const ADpiX, ADpiY: Single): TSize; overload; virtual; abstract;
    function PixelsToLayoutUnits(const AValue: TRect): TRect; overload;
    function PixelsToLayoutUnits(const AValue: TRect; const ADpiX, ADpiY: Single): TRect; overload; virtual; abstract;
    function PixelsToLayoutUnitsF(const AValue: Single; const ADpi: Single): Single; virtual; abstract;
    function PointsToFontUnits(const AValue: Integer): Integer; virtual; abstract;
    function PointsToFontUnitsF(const AValue: Single): Single; virtual; abstract;
    function PointsToLayoutUnits(const AValue: Integer): Integer; virtual; abstract;
    function PointsToLayoutUnitsF(const AValue: Single): Single; virtual; abstract;
    function SnapToPixels(const AValue: Integer; const ADpi: Single): Integer; virtual; abstract;
    function TwipsToLayoutUnits(const AValue: Int64): Int64; overload; virtual; abstract;
    function TwipsToLayoutUnits(const AValue: Integer): Integer; overload; virtual; abstract;

    property Dpi: Single read GetDpi;
    property FontSizeScale: Single read GetFontSizeScale;
    property FontSizeScaleForPrinting: Single read GetFontSizeScaleForPrinting;
    property FontUnit: TdxLayoutGraphicsUnit read GetFontUnit;
    property GraphicsPageScale: Single read GetGraphicsPageScale;
    property GraphicsPageUnit: TdxLayoutGraphicsUnit read GetGraphicsPageUnit;
  end;

  { TdxDocumentModelDpi }

  TdxDocumentModelDpi = class
  public
    class function DpiX: Single; static;
    class function DpiY: Single; static;
    class function Dpi: Single; static;
  end;

implementation

uses
  dxTypeHelpers,
  dxRichEdit.Utils.Graphics,
  dxRichEdit.DocumentLayout.UnitDocumentConverter,
  dxRichEdit.DocumentLayout.UnitPixelsConverter,
  dxRichEdit.DocumentLayout.UnitTwipsConverter;

{ TdxDpiSupport }

constructor TdxDpiSupport.Create;
begin
  Create(TdxDocumentModelDpi.DpiX, TdxDocumentModelDpi.DpiY, TdxDocumentModelDpi.Dpi);
end;

constructor TdxDpiSupport.Create(AScreenDpiX: Single; AScreenDpiY: Single);
begin
  Create(AScreenDpiX, AScreenDpiY, AScreenDpiX);
end;

constructor TdxDpiSupport.Create(AScreenDpiX: Single; AScreenDpiY: Single; AScreenDpi: Single);
begin
  inherited Create;
  FScreenDpiX := AScreenDpiX;
  FScreenDpiY := AScreenDpiY;
  FScreenDpi := AScreenDpi;
end;

{ TdxDocumentLayoutUnitConverter }

function TdxDocumentLayoutUnitConverter.Equals(Obj: TObject): Boolean;
begin
  Result := (Obj = Self) or
    ((Obj <> nil) and (Obj.ClassType = ClassType) and (TdxDocumentLayoutUnitConverter(Obj).Dpi = Dpi));
end;

class function TdxDocumentLayoutUnitConverter.CreateConverter(
  const AUnit: TdxDocumentLayoutUnit; const ADpi: Single): TdxDocumentLayoutUnitConverter;
begin
  case AUnit of
    TdxDocumentLayoutUnit.Document:
      Result := TdxDocumentLayoutUnitDocumentConverter.Create;
    TdxDocumentLayoutUnit.Twip:
      Result := TdxDocumentLayoutUnitTwipsConverter.Create;
  else
      Result := TdxDocumentLayoutUnitPixelsConverter.Create(ADpi);
  end;
end;

function TdxDocumentLayoutUnitConverter.GetFontSizeScaleForPrinting: Single;
begin
  Result := 1.0;
end;

function TdxDocumentLayoutUnitConverter.LayoutUnitsToPixels(const AValue: Integer): Integer;
begin
  Result := LayoutUnitsToPixels(AValue, ScreenDpi);
end;

function TdxDocumentLayoutUnitConverter.LayoutUnitsToPixels(const AValue: TSize): TSize;
begin
  Result := LayoutUnitsToPixels(AValue, ScreenDpiX, ScreenDpiY);
end;

function TdxDocumentLayoutUnitConverter.PixelsToLayoutUnits(const AValue: TPoint; const ADpiX, ADpiY: Single): TPoint;
begin
  Result.Init(PixelsToLayoutUnits(AValue.X, ADpiX), PixelsToLayoutUnits(AValue.Y, ADpiY));
end;

function TdxDocumentLayoutUnitConverter.PixelsToLayoutUnits(const AValue: Integer): Integer;
begin
  Result := PixelsToLayoutUnits(AValue, ScreenDpi);
end;

function TdxDocumentLayoutUnitConverter.PixelsToLayoutUnits(const AValue: TRect): TRect;
begin
  Result := PixelsToLayoutUnits(AValue, ScreenDpiX, ScreenDpiY);
end;

{ TdxDocumentModelDpi }

class function TdxDocumentModelDpi.Dpi: Single;
begin
  Result := TdxGraphicsDpi.Pixel;
end;

class function TdxDocumentModelDpi.DpiX: Single;
begin
  Result := TdxGraphicsDpi.Pixel;
end;

class function TdxDocumentModelDpi.DpiY: Single;
begin
  Result := TdxGraphicsDpi.Pixel;
end;

end.
