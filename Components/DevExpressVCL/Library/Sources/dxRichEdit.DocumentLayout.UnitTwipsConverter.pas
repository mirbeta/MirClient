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

unit dxRichEdit.DocumentLayout.UnitTwipsConverter;

interface

{$I cxVer.inc}
{$I dxRichEditControl.inc}

uses
  Types, cxGeometry,
  dxRichEdit.DocumentLayout.UnitConverter;

type
  { TdxDocumentLayoutUnitTwipsConverter }

  TdxDocumentLayoutUnitTwipsConverter = class sealed (TdxDocumentLayoutUnitConverter)
  protected
    function GetDpi: Single; override;
    function GetFontSizeScale: Single; override;
    function GetFontSizeScaleForPrinting: Single; override;
    function GetFontUnit: TdxLayoutGraphicsUnit; override;
    function GetGraphicsPageScale: Single; override;
    function GetGraphicsPageUnit: TdxLayoutGraphicsUnit; override;
  public
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

{ TdxDocumentLayoutUnitTwipsConverter }

function TdxDocumentLayoutUnitTwipsConverter.DocumentsToFontUnitsF(const AValue: Single): Single;
begin
  Result := DocumentsToPointsF(AValue);
end;

function TdxDocumentLayoutUnitTwipsConverter.DocumentsToLayoutUnits(const AValue: Integer): Integer;
begin
  Result := DocumentsToTwips(AValue);
end;

function TdxDocumentLayoutUnitTwipsConverter.DocumentsToLayoutUnits(const AValue: TdxRectF): TdxRectF;
begin
  Result := DocumentsToTwips(AValue);
end;

function TdxDocumentLayoutUnitTwipsConverter.DocumentsToLayoutUnits(const AValue: TRect): TRect;
begin
  Result := DocumentsToTwips(AValue);
end;

function TdxDocumentLayoutUnitTwipsConverter.InchesToFontUnitsF(const AValue: Single): Single;
begin
  Result := InchesToPointsF(AValue);
end;

function TdxDocumentLayoutUnitTwipsConverter.LayoutUnitsToDocuments(const AValue: TdxRectF): TdxRectF;
begin
  Result := TwipsToDocuments(AValue);
end;

function TdxDocumentLayoutUnitTwipsConverter.LayoutUnitsToDocuments(const AValue: TRect): TRect;
begin
  Result := TwipsToDocuments(AValue);
end;

function TdxDocumentLayoutUnitTwipsConverter.LayoutUnitsToHundredthsOfInch(const AValue: Integer): Integer;
begin
  Result := TwipsToHundredthsOfInch(AValue);
end;

function TdxDocumentLayoutUnitTwipsConverter.LayoutUnitsToHundredthsOfInch(const AValue: TSize): TSize;
begin
  Result := TwipsToHundredthsOfInch(AValue);
end;

function TdxDocumentLayoutUnitTwipsConverter.LayoutUnitsToPixels(const AValue: Integer;
  const ADpi: Single): Integer;
begin
  Result := TwipsToPixels(AValue, ADpi);
end;

function TdxDocumentLayoutUnitTwipsConverter.LayoutUnitsToPixels(const AValue: TSize; const ADpiX, ADpiY: Single): TSize;
begin
  Result := TwipsToPixels(AValue, ADpiX, ADpiY);
end;

function TdxDocumentLayoutUnitTwipsConverter.LayoutUnitsToPixels(const AValue: TPoint; const ADpiX, ADpiY: Single): TPoint;
begin
  Result := TwipsToPixels(AValue, ADpiX, ADpiY);
end;

function TdxDocumentLayoutUnitTwipsConverter.LayoutUnitsToPixels(const AValue: TRect;
  const ADpiX, ADpiY: Single): TRect;
begin
  Result := TwipsToPixels(AValue, ADpiX, ADpiY);
end;

function TdxDocumentLayoutUnitTwipsConverter.LayoutUnitsToPixelsF(const AValue: Single;
  const ADpi: Single): Single;
begin
  Result := TwipsToPixelsF(AValue, ADpi);
end;

function TdxDocumentLayoutUnitTwipsConverter.LayoutUnitsToPointsF(const AValue: Single): Single;
begin
  Result := TwipsToPointsF(AValue);
end;

function TdxDocumentLayoutUnitTwipsConverter.LayoutUnitsToTwips(const AValue: Int64): Int64;
begin
  Result := AValue;
end;

function TdxDocumentLayoutUnitTwipsConverter.LayoutUnitsToTwips(const AValue: Integer): Integer;
begin
  Result := AValue;
end;

function TdxDocumentLayoutUnitTwipsConverter.MillimetersToFontUnitsF(const AValue: Single): Single;
begin
  Result := MillimetersToPointsF(AValue);
end;

function TdxDocumentLayoutUnitTwipsConverter.PixelsToLayoutUnits(const AValue: Integer;
  const ADpi: Single): Integer;
begin
  Result := PixelsToTwips(AValue, ADpi);
end;

function TdxDocumentLayoutUnitTwipsConverter.PixelsToLayoutUnits(const AValue: TSize; const ADpiX, ADpiY: Single): TSize;
begin
  Result := PixelsToTwips(AValue, ADpiX, ADpiY);
end;

function TdxDocumentLayoutUnitTwipsConverter.PixelsToLayoutUnits(const AValue: TRect;
  const ADpiX, ADpiY: Single): TRect;
begin
  Result := PixelsToTwips(AValue, ADpiX, ADpiY);
end;

function TdxDocumentLayoutUnitTwipsConverter.PixelsToLayoutUnitsF(const AValue: Single;
  const ADpi: Single): Single;
begin
  Result := PixelsToTwipsF(AValue, ADpi);
end;

function TdxDocumentLayoutUnitTwipsConverter.PointsToFontUnits(const AValue: Integer): Integer;
begin
  Result := AValue;
end;

function TdxDocumentLayoutUnitTwipsConverter.PointsToFontUnitsF(const AValue: Single): Single;
begin
  Result := AValue;
end;

function TdxDocumentLayoutUnitTwipsConverter.PointsToLayoutUnits(const AValue: Integer): Integer;
begin
  Result := PointsToTwips(AValue);
end;

function TdxDocumentLayoutUnitTwipsConverter.PointsToLayoutUnitsF(const AValue: Single): Single;
begin
  Result := PointsToTwipsF(AValue);
end;

function TdxDocumentLayoutUnitTwipsConverter.SnapToPixels(const AValue: Integer; const ADpi: Single): Integer;
begin
  Result := Round(Dpi * Round(ADpi * AValue / Dpi) / ADpi);
end;

function TdxDocumentLayoutUnitTwipsConverter.TwipsToLayoutUnits(const AValue: Int64): Int64;
begin
  Result := AValue;
end;

function TdxDocumentLayoutUnitTwipsConverter.TwipsToLayoutUnits(const AValue: Integer): Integer;
begin
  Result := AValue;
end;

function TdxDocumentLayoutUnitTwipsConverter.GetDpi: Single;
begin
  Result := 1440;
end;

function TdxDocumentLayoutUnitTwipsConverter.GetFontSizeScale: Single;
begin
  Result := 1 / 20;
end;

function TdxDocumentLayoutUnitTwipsConverter.GetFontSizeScaleForPrinting: Single;
begin
  Result := 20;
end;

function TdxDocumentLayoutUnitTwipsConverter.GetFontUnit: TdxLayoutGraphicsUnit;
begin
  Result := TdxLayoutGraphicsUnit.Point;
end;

function TdxDocumentLayoutUnitTwipsConverter.GetGraphicsPageScale: Single;
begin
  Result := 1 / 20;
end;

function TdxDocumentLayoutUnitTwipsConverter.GetGraphicsPageUnit: TdxLayoutGraphicsUnit;
begin
  Result := TdxLayoutGraphicsUnit.Point;
end;

end.

