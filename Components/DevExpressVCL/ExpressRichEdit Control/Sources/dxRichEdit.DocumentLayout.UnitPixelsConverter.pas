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

unit dxRichEdit.DocumentLayout.UnitPixelsConverter;

interface

{$I cxVer.inc}
{$I dxRichEditControl.inc}

uses
  Types, cxGeometry,
  dxRichEdit.DocumentLayout.UnitConverter;

type
  { TdxDocumentLayoutUnitPixelsConverter }

  TdxDocumentLayoutUnitPixelsConverter = class sealed (TdxDocumentLayoutUnitConverter)
  private
    FDpi: Single;
  protected
    function GetDpi: Single; override;
    function GetFontSizeScale: Single; override;
    function GetFontUnit: TdxLayoutGraphicsUnit; override;
    function GetGraphicsPageScale: Single; override;
    function GetGraphicsPageUnit: TdxLayoutGraphicsUnit; override;
  public
    constructor Create(ADpi: Single);
    function DocumentsToFontUnitsF(const AValue: Single): Single; override;
    function DocumentsToLayoutUnits(const AValue: Integer): Integer; overload; override;
    function DocumentsToLayoutUnits(const AValue: TdxRectF): TdxRectF; overload; override;
    function DocumentsToLayoutUnits(const AValue: TRect): TRect; overload; override;
    function InchesToFontUnitsF(const AValue: Single): Single; override;
    function LayoutUnitsToDocuments(const AValue: TdxRectF): TdxRectF; overload; override;
    function LayoutUnitsToDocuments(const AValue: TRect): TRect; overload; override;
    function LayoutUnitsToHundredthsOfInch(const AValue: Integer): Integer; overload; override;
    function LayoutUnitsToHundredthsOfInch(const AValue: TSize): TSize; overload; override;
    function LayoutUnitsToPixels(const AValue: Integer; const ADpi: Single): Integer; overload; override;
    function LayoutUnitsToPixels(const AValue: TSize; const ADpiX, ADpiY: Single): TSize; overload; override;
    function LayoutUnitsToPixels(const AValue: TPoint; const ADpiX, ADpiY: Single): TPoint; overload; override;
    function LayoutUnitsToPixels(const AValue: TRect; const ADpiX, ADpiY: Single): TRect; overload; override;
    function LayoutUnitsToPixelsF(const AValue: Single; const ADpi: Single): Single; override;
    function LayoutUnitsToPointsF(const AValue: Single): Single; override;
    function LayoutUnitsToTwips(const AValue: Int64): Int64; overload; override;
    function LayoutUnitsToTwips(const AValue: Integer): Integer; overload; override;
    function MillimetersToFontUnitsF(const AValue: Single): Single; override;
    function PixelsToLayoutUnits(const AValue: Integer; const ADpi: Single): Integer; overload; override;
    function PixelsToLayoutUnits(const AValue: TSize; const ADpiX, ADpiY: Single): TSize; overload; override;
    function PixelsToLayoutUnits(const AValue: TRect; const ADpiX, ADpiY: Single): TRect; overload; override;
    function PixelsToLayoutUnitsF(const AValue: Single; const ADpi: Single): Single; override;
    function PointsToFontUnits(const AValue: Integer): Integer; override;
    function PointsToFontUnitsF(const AValue: Single): Single; override;
    function PointsToLayoutUnits(const AValue: Integer): Integer; override;
    function PointsToLayoutUnitsF(const AValue: Single): Single; override;
    function SnapToPixels(const AValue: Integer; const ADpi: Single): Integer; override;
    function TwipsToLayoutUnits(const AValue: Int64): Int64; overload; override;
    function TwipsToLayoutUnits(const AValue: Integer): Integer; overload; override;
  end;

implementation

uses
  dxTypeHelpers,
  dxMeasurementUnits;

{ TdxDocumentLayoutUnitPixelsConverter }

constructor TdxDocumentLayoutUnitPixelsConverter.Create(ADpi: Single);
begin
  inherited Create;
  FDpi := ADpi;
end;

function TdxDocumentLayoutUnitPixelsConverter.DocumentsToFontUnitsF(const AValue: Single): Single;
begin
  Result := DocumentsToPointsF(AValue);
end;

function TdxDocumentLayoutUnitPixelsConverter.DocumentsToLayoutUnits(const AValue: Integer): Integer;
begin
  Result := DocumentsToPixels(AValue, FDpi);
end;

function TdxDocumentLayoutUnitPixelsConverter.DocumentsToLayoutUnits(const AValue: TdxRectF): TdxRectF;
begin
  Result := DocumentsToPixels(AValue, FDpi, FDpi);
end;

function TdxDocumentLayoutUnitPixelsConverter.DocumentsToLayoutUnits(const AValue: TRect): TRect;
begin
  Result := DocumentsToPixels(AValue, FDpi, FDpi);
end;

function TdxDocumentLayoutUnitPixelsConverter.InchesToFontUnitsF(const AValue: Single): Single;
begin
  Result := InchesToPointsF(AValue);
end;

function TdxDocumentLayoutUnitPixelsConverter.LayoutUnitsToDocuments(const AValue: TdxRectF): TdxRectF;
begin
  Result := PixelsToDocuments(AValue, FDpi, FDpi);
end;

function TdxDocumentLayoutUnitPixelsConverter.LayoutUnitsToDocuments(const AValue: TRect): TRect;
begin
  Result := PixelsToDocuments(AValue, FDpi, FDpi);
end;

function TdxDocumentLayoutUnitPixelsConverter.LayoutUnitsToHundredthsOfInch(const AValue: Integer): Integer;
begin
  Result := PixelsToHundredthsOfInch(AValue, FDpi);
end;

function TdxDocumentLayoutUnitPixelsConverter.LayoutUnitsToHundredthsOfInch(const AValue: TSize): TSize;
begin
  Result := PixelsToHundredthsOfInch(AValue, FDpi);
end;

function TdxDocumentLayoutUnitPixelsConverter.LayoutUnitsToPixels(const AValue: Integer;
  const ADpi: Single): Integer;
begin
  Result := MulDiv(AValue, FDpi, ADpi);
end;

function TdxDocumentLayoutUnitPixelsConverter.LayoutUnitsToPixels(const AValue: TSize;
  const ADpiX, ADpiY: Single): TSize;
begin
  Result.Init(MulDiv(AValue.cx, FDpi, ADpiX), MulDiv(AValue.cy, FDpi, ADpiY));
end;

function TdxDocumentLayoutUnitPixelsConverter.LayoutUnitsToPixels(const AValue: TPoint;
  const ADpiX, ADpiY: Single): TPoint;
begin
  Result.Init(MulDiv(AValue.X, FDpi, ADpiX), MulDiv(AValue.Y, FDpi, ADpiY));
end;

function TdxDocumentLayoutUnitPixelsConverter.LayoutUnitsToPixels(const AValue: TRect;
  const ADpiX, ADpiY: Single): TRect;
begin
  Result.Init(MulDiv(AValue.Left, FDpi, ADpiX), MulDiv(AValue.Top, FDpi, ADpiY),
    MulDiv(AValue.Right, FDpi, ADpiX), MulDiv(AValue.Bottom, FDpi, ADpiY));
end;

function TdxDocumentLayoutUnitPixelsConverter.LayoutUnitsToPixelsF(const AValue: Single;
  const ADpi: Single): Single;
begin
  Result := AValue * FDpi / ADpi;
end;

function TdxDocumentLayoutUnitPixelsConverter.LayoutUnitsToPointsF(const AValue: Single): Single;
begin
  Result := PixelsToPointsF(AValue, FDpi);
end;

function TdxDocumentLayoutUnitPixelsConverter.LayoutUnitsToTwips(const AValue: Int64): Int64;
begin
  Result := PixelsToTwipsL(AValue, FDpi);
end;

function TdxDocumentLayoutUnitPixelsConverter.LayoutUnitsToTwips(const AValue: Integer): Integer;
begin
  Result := PixelsToTwips(AValue, FDpi);
end;

function TdxDocumentLayoutUnitPixelsConverter.MillimetersToFontUnitsF(const AValue: Single): Single;
begin
  Result := MillimetersToPointsF(AValue);
end;

function TdxDocumentLayoutUnitPixelsConverter.PixelsToLayoutUnits(const AValue: Integer;
  const ADpi: Single): Integer;
begin
  Result := MulDiv(AValue, FDpi, ADpi);
end;

function TdxDocumentLayoutUnitPixelsConverter.PixelsToLayoutUnits(const AValue: TSize;
  const ADpiX, ADpiY: Single): TSize;
begin
  Result.Init(MulDiv(AValue.cx, FDpi, ADpiX), MulDiv(AValue.cy, FDpi, ADpiY));
end;

function TdxDocumentLayoutUnitPixelsConverter.PixelsToLayoutUnits(const AValue: TRect;
  const ADpiX, ADpiY: Single): TRect;
begin
  Result.Init(MulDiv(AValue.Left, FDpi, ADpiX), MulDiv(AValue.Top, FDpi, ADpiY),
    MulDiv(AValue.Right, FDpi, ADpiX), MulDiv(AValue.Bottom, FDpi, ADpiY));
end;

function TdxDocumentLayoutUnitPixelsConverter.PixelsToLayoutUnitsF(const AValue: Single;
  const ADpi: Single): Single;
begin
  Result := AValue * FDpi / ADpi;
end;

function TdxDocumentLayoutUnitPixelsConverter.PointsToFontUnits(const AValue: Integer): Integer;
begin
  Result := AValue;
end;

function TdxDocumentLayoutUnitPixelsConverter.PointsToFontUnitsF(const AValue: Single): Single;
begin
  Result := AValue;
end;

function TdxDocumentLayoutUnitPixelsConverter.PointsToLayoutUnits(const AValue: Integer): Integer;
begin
  Result := PointsToPixels(AValue, FDpi);
end;

function TdxDocumentLayoutUnitPixelsConverter.PointsToLayoutUnitsF(const AValue: Single): Single;
begin
  Result := PointsToPixelsF(AValue, FDpi);
end;

function TdxDocumentLayoutUnitPixelsConverter.SnapToPixels(const AValue: Integer; const ADpi: Single): Integer;
begin
  Result := AValue;
end;

function TdxDocumentLayoutUnitPixelsConverter.TwipsToLayoutUnits(const AValue: Int64): Int64;
begin
  Result := TwipsToPixelsL(AValue, FDpi);
end;

function TdxDocumentLayoutUnitPixelsConverter.TwipsToLayoutUnits(const AValue: Integer): Integer;
begin
  Result := TwipsToPixels(AValue, FDpi);
end;

function TdxDocumentLayoutUnitPixelsConverter.GetDpi: Single;
begin
  Result := FDpi;
end;

function TdxDocumentLayoutUnitPixelsConverter.GetFontSizeScale: Single;
begin
  Result := 72 / FDpi;
end;

function TdxDocumentLayoutUnitPixelsConverter.GetFontUnit: TdxLayoutGraphicsUnit;
begin
  Result := TdxLayoutGraphicsUnit.Point;
end;

function TdxDocumentLayoutUnitPixelsConverter.GetGraphicsPageScale: Single;
begin
  Result := 1;
end;

function TdxDocumentLayoutUnitPixelsConverter.GetGraphicsPageUnit: TdxLayoutGraphicsUnit;
begin
  Result := TdxLayoutGraphicsUnit.Pixel;
end;

end.

