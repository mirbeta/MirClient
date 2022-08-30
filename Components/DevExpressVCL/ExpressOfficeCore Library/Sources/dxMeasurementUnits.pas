{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressOfficeCore Library classes                        }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSOFFICECORE LIBRARY AND ALL     }
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

unit dxMeasurementUnits;

{$I cxVer.inc}

interface

uses
  Windows, Types, cxGeometry, dxTypeHelpers, dxDPIAwareUtils;

type
  TdxMeasurementUnits = (muDefault, muInches, muMillimeters);

  TdxUnitsConverterClass = class of TdxUnitsConverter;
  TdxUnitsConverter = class
  public
    class function FromInch(const Value: Integer): Integer; overload; virtual;
    class function FromInch(const Value: Single): Single; overload; virtual;
    class function FromInch(const Value: TPoint): TPoint; overload;
    class function FromInch(const Value: TRect): TRect; overload;
    class function FromLoMetric(const Value: Integer): Single; overload; virtual;
    class function FromLoMetric(const Value: TPoint): TPoint; overload;
    class function FromLoMetric(const Value: TRect): TRect; overload;
    class function FromMM(const Value: Integer): Integer; overload; virtual;
    class function FromMM(const Value: Single): Single; overload; virtual;
    class function FromMM(const Value: TPoint): TPoint; overload;
    class function FromMM(const Value: TRect): TRect; overload;
    class function ToInch(const Value: Integer): Integer; overload; virtual;
    class function ToInch(const Value: Single): Single; overload; virtual;
    class function ToInch(const Value: TPoint): TPoint; overload;
    class function ToInch(const Value: TRect): TRect; overload;
    class function ToLoMetric(const Value: Integer): Integer; overload; virtual;
    class function ToLoMetric(const Value: Single): Single; overload; virtual;
    class function ToLoMetric(const Value: TPoint): TPoint; overload;
    class function ToLoMetric(const Value: TRect): TRect; overload;
    class function ToMM(const Value: Integer): Integer; overload; virtual;
    class function ToMM(const Value: Single): Single; overload; virtual;
    class function ToMM(const Value: TPoint): TPoint; overload;
    class function ToMM(const Value: TRect): TRect; overload;
  end;

  { TdxInchesUnits }

  TdxInchesUnits = class(TdxUnitsConverter)
  public
    class function FromLoMetric(const Value: Integer): Single; override;
    class function FromMM(const Value: Integer): Integer; override;
    class function FromMM(const Value: Single): Single; override;
    class function ToLoMetric(const Value: Integer): Integer; override;
    class function ToLoMetric(const Value: Single): Single; override;
    class function ToMM(const Value: Integer): Integer; override;
    class function ToMM(const Value: Single): Single; override;
  end;

  { TdxMillimetersUnits }

  TdxMillimetersUnits = class(TdxUnitsConverter)
  public
    class function FromInch(const Value: Integer): Integer; override;
    class function FromInch(const Value: Single): Single; override;
    class function FromLoMetric(const Value: Integer): Single; override;
    class function ToInch(const Value: Integer): Integer; override;
    class function ToInch(const Value: Single): Single; override;
    class function ToLoMetric(const Value: Integer): Integer; override;
    class function ToLoMetric(const Value: Single): Single; override;
  end;

function dxGetActualMeasurementUnits(AValue: TdxMeasurementUnits): TdxMeasurementUnits;
function dxGetDefaultMeasurementUnits: TdxMeasurementUnits;

function DocumentsToCentimetersF(const AValue: Single): Single; inline;
function DocumentsToEmu(const AValue: Integer): Integer; inline;
function DocumentsToEmuF(const AValue: Single): Integer; inline;
function DocumentsToEmuL(const AValue: Int64): Int64; inline;
function DocumentsToHundredthsOfInch(const AValue: Integer): Integer; overload; inline;
function DocumentsToHundredthsOfInch(const AValue: TSize): TSize; overload; inline;
function DocumentsToHundredthsOfMillimeter(const AValue: Integer): Integer; overload; inline;
function DocumentsToHundredthsOfMillimeter(const AValue: TSize): TSize; overload; inline;
function DocumentsToInchesF(const AValue: Single): Single; inline;
function DocumentsToMillimetersF(const AValue: Single): Single; inline;
function DocumentsToPicasF(const AValue: Single): Single; inline;
function DocumentsToPixels(const AValue: Integer; const ADpi: Single): Integer; overload; inline;
function DocumentsToPixels(const AValue: TdxRectF; const ADpiX, ADpiY: Single): TdxRectF; overload; inline;
function DocumentsToPixels(const AValue: TPoint; const ADpiX, ADpiY: Single): TPoint; overload; inline;
function DocumentsToPixels(const AValue: TRect; const ADpiX, ADpiY: Single): TRect; overload; inline;
function DocumentsToPixels(const AValue: TSize; const ADpiX, ADpiY: Single): TSize; overload; inline;
function DocumentsToPixelsF(const AValue: Single; const ADpi: Single): Single; inline;
function DocumentsToPoints(const AValue: Integer): Integer; inline;
function DocumentsToPointsF(const AValue: Single): Single; inline;
function DocumentsToPointsFRound(const AValue: Single): Single; inline;
function DocumentsToTwips(const AValue: Integer): Integer; overload; inline;
function DocumentsToTwips(const AValue: TdxRectF): TdxRectF; overload; inline;
function DocumentsToTwips(const AValue: TRect): TRect; overload; inline;
function DocumentsToTwips(const AValue: TSize): TSize; overload; inline;
function DocumentsToTwipsF(const AValue: Single): Single; inline;
function DocumentsToTwipsL(const AValue: Int64): Int64; inline;

function TwipsToCentimetersF(const AValue: Single): Single; inline;
function TwipsToDocuments(const AValue: Integer): Integer; overload; inline;
function TwipsToDocuments(const AValue: TdxRectF): TdxRectF; overload; inline;
function TwipsToDocuments(const AValue: TRect): TRect; overload; inline;
function TwipsToDocuments(const AValue: TSize): TSize; overload; inline;
function TwipsToDocumentsF(const AValue: Single): Single; inline;
function TwipsToDocumentsL(const AValue: Int64): Int64; inline;
function TwipsToEmu(const AValue: Integer): Integer; inline;
function TwipsToEmuF(const AValue: Single): Integer; inline;
function TwipsToEmuL(const AValue: Int64): Int64; inline;
function TwipsToHundredthsOfInch(const AValue: Integer): Integer; overload; inline;
function TwipsToHundredthsOfInch(const AValue: TSize): TSize; overload; inline;
function TwipsToHundredthsOfMillimeter(const AValue: TSize): TSize; inline;
function TwipsToInchesF(const AValue: Single): Single; inline;
function TwipsToMillimetersF(const AValue: Single): Single; inline;
function TwipsToPixels(const AValue: Integer; const ADpi: Single): Integer; overload; inline;
function TwipsToPixels(const AValue: TPoint; const ADpiX, ADpiY: Single): TPoint; overload; inline;
function TwipsToPixels(const AValue: TRect; const ADpiX, ADpiY: Single): TRect; overload; inline;
function TwipsToPixels(const AValue: TSize; const ADpiX, ADpiY: Single): TSize; overload; inline;
function TwipsToPixelsF(const AValue: Single; const ADpi: Single): Single; inline;
function TwipsToPixelsL(const AValue: Int64; const ADpi: Single): Int64; inline;
function TwipsToPointsF(const AValue: Single): Single; inline;
function TwipsToPointsFRound(const AValue: Single): Single; inline;

function PixelsToDocuments(const APoint: TPoint; ADpiX, ADpiY: Single): TPoint; overload; inline;
function PixelsToDocuments(const ARect: TdxRectF; ADpiX, ADpiY: Single): TdxRectF; overload; inline;
function PixelsToDocuments(const ARect: TRect; ADpiX, ADpiY: Single): TRect; overload; inline;
function PixelsToDocuments(const ASize: TSize; ADpiX, ADpiY: Single): TSize; overload; inline;
function PixelsToDocuments(const AValue: Double; const ADpi: Single): Integer; overload; inline;
function PixelsToDocuments(const AValue: Integer; const ADpi: Single): Integer; overload; inline;
function PixelsToDocumentsF(const AValue: Single; const ADpi: Single): Single; inline;
function PixelsToDocumentsRound(const ASize: TSize; ADpiX, ADpiY: Single): TSize; overload; inline;
function PixelsToDocumentsRound(const AValue: Integer; ADpi: Single): Integer; overload; inline;
function PixelsToHundredthsOfInch(const AValue: Integer; const ADpi: Single): Integer; overload; inline;
function PixelsToHundredthsOfInch(const AValue: TSize; const ADpi: Single): TSize; overload; inline;
function PixelsToHundredthsOfMillimeter(const AValue: Integer; const ADpi: Single): Integer; overload; inline;
function PixelsToHundredthsOfMillimeter(const AValue: TSize; const ADpiX, ADpiY: Single): TSize; overload; inline;
function PixelsToLoMetric(Value: Integer; DPI: Integer = dxDefaultDPI): Integer;
function PixelsToPoints(const AValue: Integer; const ADpi: Single): Integer; inline;
function PixelsToPointsF(const AValue: Single; const ADpi: Single): Single; inline;
function PixelsToTwips(const ARect: TRect; ADpiX, ADpiY: Single): TRect; overload; inline;
function PixelsToTwips(const ASize: TSize; ADpiX, ADpiY: Single): TSize; overload; inline;
function PixelsToTwips(const AValue: Integer; const ADpi: Single): Integer; overload; inline;
function PixelsToTwipsF(const AValue: Single; const ADpi: Single): Single; inline;
function PixelsToTwipsL(const AValue: Int64; const ADpi: Single): Int64; inline;
function PixelsToTwipsRound(const ASize: TSize; ADpiX, ADpiY: Single): TSize; inline;

function HundredthsOfMillimeterToDocuments(const AValue: Integer): Integer; overload; inline;
function HundredthsOfMillimeterToDocuments(const AValue: TSize): TSize; overload; inline;
function HundredthsOfMillimeterToDocumentsRound(const AValue: TSize): TSize; overload; inline;
function HundredthsOfMillimeterToPixels(const AValue: Integer; const ADpi: Single): Integer; overload; inline;
function HundredthsOfMillimeterToPixels(const AValue: TSize; const ADpiX, ADpiY: Single): TSize; overload; inline;
function HundredthsOfMillimeterToTwips(const AValue: Integer): Integer; overload; inline;
function HundredthsOfMillimeterToTwips(const AValue: TSize): TSize; overload; inline;
function HundredthsOfMillimeterToTwipsRound(const AValue: TSize): TSize; overload; inline;

function PointsToDocuments(const AValue: Integer): Integer; inline;
function PointsToDocumentsF(const AValue: Single): Single; inline;
function PointsToPixels(const AValue: Integer; const ADpi: Single): Integer; inline;
function PointsToPixelsF(const AValue: Single; const ADpi: Single): Single; inline;
function PointsToTwips(const AValue: Integer): Integer; inline;
function PointsToTwipsF(const AValue: Single): Single; inline;

function EmuToDocuments(const AValue: Integer): Integer; inline;
function EmuToDocumentsL(const AValue: Int64): Int64; inline;
function EmuToDocumentsF(const AValue: Integer): Single; inline;
function EmuToTwips(const AValue: Integer): Integer; inline;
function EmuToTwipsL(const AValue: Int64): Int64; inline;
function EmuToTwipsF(const AValue: Integer): Single; inline;

function HundredthsOfInchToDocuments(const AValue: Integer): Integer; overload; inline;
function HundredthsOfInchToDocuments(const AValue: TSize): TSize; overload; inline;
function HundredthsOfInchToTwips(const AValue: Integer): Integer; overload; inline;
function HundredthsOfInchToTwips(const AValue: TSize): TSize; overload; inline;

function CentimetersToDocumentsF(const AValue: Single): Single; inline;
function CentimetersToTwipsF(const AValue: Single): Single; inline;

function InchesToDocumentsF(const AValue: Single): Single; inline;
function InchesToLoMetric(const AValue: Single): Integer; inline;
function InchesToMillimeters(const AValue: Integer): Integer; inline;
function InchesToMillimetersF(const AValue: Single): Single; inline;
function InchesToPointsF(const AValue: Single): Single; inline;
function InchesToTwipsF(const AValue: Single): Single; inline;

function MillimetersToDocumentsF(const AValue: Single): Single; inline;
function MillimetersToInches(const AValue: Integer): Integer; inline;
function MillimetersToInchesF(const AValue: Single): Single; inline;
function MillimetersToLoMetric(const AValue: Single): Integer; overload; inline;
function MillimetersToLoMetric(const AValue: Integer): Integer; overload; inline;
function MillimetersToPoints(const AValue: Integer): Integer; inline;
function MillimetersToPointsF(const AValue: Single): Single; inline;
function MillimetersToTwipsF(const AValue: Single): Single; inline;

function PicasToDocumentsF(const AValue: Single): Single; inline;
function PicasToTwipsF(const AValue: Single): Single; inline;

function LoMetricToInches(const AValue: Integer): Single; inline;
function LoMetricToMillimeters(const AValue: Integer): Integer; inline;
function LoMetricToPixels(Value: Integer; DPI: Integer = dxDefaultDPI): Integer; overload;
function LoMetricToPixels(const Pt: TPoint; DPI: Integer = dxDefaultDPI): TPoint; overload;

function MulDiv(AValue, AMul, ADiv: Integer): Integer; overload;
function MulDiv(AValue: Integer; AMul, ADiv: Single): Integer; overload;
implementation

uses
  SysUtils;

function dxGetActualMeasurementUnits(AValue: TdxMeasurementUnits): TdxMeasurementUnits;
begin
  if AValue <> muDefault then
    Result := AValue
  else
    Result := dxGetDefaultMeasurementUnits;
end;

{$WARN SYMBOL_PLATFORM OFF}
function dxGetDefaultMeasurementUnits: TdxMeasurementUnits;
const
  MeasurementUnitsMap: array[Boolean] of TdxMeasurementUnits = (muInches, muMillimeters);
begin
  Result := MeasurementUnitsMap[GetLocaleChar(LOCALE_USER_DEFAULT, LOCALE_IMEASURE, '0') = '0'];
end;
{$WARN SYMBOL_PLATFORM ON}

function MulDiv(AValue, AMul, ADiv: Integer): Integer; overload; inline;
begin
  if ADiv = 0 then
    Result := -1
  else
    Result := Int64(AValue) * Int64(AMul) div ADiv;
end;

function MulDiv(AValue: Integer; AMul, ADiv: Single): Integer; overload; inline;
begin
  Result := Trunc(AMul * AValue / ADiv);
end;

function MulDivL(AValue, AMul, ADiv: Int64): Int64; overload; inline;
begin
  Result := Trunc(Int64(AValue * AMul) / ADiv);
end;

function MulDivL(AValue, AMul: Int64; ADiv: Single): Int64; overload; inline;
begin
  Result := Trunc(Int64(AValue * AMul) / ADiv);
end;

function MulDivF(AValue, AMul, ADiv: Single): Single; inline;
begin
  Result := (AMul * AValue) / ADiv;
end;

function PixelsToPointsCore(const AValue: Integer; const ADpi: Single): Integer; overload; inline;
begin
  Result := MulDiv(AValue, 72, ADpi);
end;

function PixelsToPointsCore(const AValue: Single; const ADpi: Single): Single; overload; inline;
begin
  Result := MulDivF(AValue, 72, ADpi);
end;

function PixelsToDocumentsCore(const AValue: Integer; const ADpi: Single): Integer; overload; inline;
begin
  Result := MulDiv(AValue, 300, ADpi);
end;

function PixelsToDocumentsCoreRound(const AValue: Integer; const ADpi: Single): Integer; inline;
begin
  Result := Round(MulDivF(AValue, 300, ADpi));
end;

function PixelsToTwipsCore(const AValue: Integer; const ADpi: Single): Integer; overload; inline;
begin
  Result := MulDiv(AValue, 1440, ADpi);
end;

function PixelsToTwipsCoreRound(const AValue: Integer; const ADpi: Single): Integer; inline;
begin
  Result := Round(MulDivF(AValue, 1440, ADpi));
end;

function PixelsToTwipsCore(const AValue: Int64; const ADpi: Single): Int64; overload; inline;
begin
  Result := MulDivL(AValue, 1440, ADpi);
end;

function PixelsToTwipsCore(const AValue: Single; const ADpi: Single): Single; overload; inline;
begin
  Result := MulDivF(AValue, 1440, ADpi);
end;

function PixelsToDocumentsCore(const AValue: Double; const ADpi: Single): Integer; overload; inline;
begin
  Result := Round(300 * AValue / ADpi);
end;

function PixelsToDocumentsCoreF(const AValue: Single; const ADpi: Single): Single; inline;
begin
  Result := MulDivF(AValue, 300, ADpi);
end;


function DocumentsToTwips(const AValue: Integer): Integer;
begin
  Result := MulDiv(AValue, 24, 5);
end;

function DocumentsToEmu(const AValue: Integer): Integer;
begin
  Result := MulDiv(AValue, 9144, 3);
end;

function DocumentsToEmuL(const AValue: Int64): Int64;
begin
  Result := MulDivL(AValue, 9144, 3);
end;

function DocumentsToEmuF(const AValue: Single): Integer;
begin
  Result := Trunc(MulDivF(AValue, 9144, 3));
end;

function DocumentsToTwipsL(const AValue: Int64): Int64;
begin
  Result := MulDivL(AValue, 24, 5);
end;

function DocumentsToTwipsF(const AValue: Single): Single;
begin
  Result := MulDivF(AValue, 24, 5);
end;

function DocumentsToTwips(const AValue: TSize): TSize;
begin
  Result.Init(DocumentsToTwips(AValue.cx), DocumentsToTwips(AValue.cy));
end;

function DocumentsToTwips(const AValue: TRect): TRect;
begin
  Result.Init(DocumentsToTwips(AValue.Left), DocumentsToTwips(AValue.Top),
    DocumentsToTwips(AValue.Right), DocumentsToTwips(AValue.Bottom));
end;

function DocumentsToTwips(const AValue: TdxRectF): TdxRectF;
begin
  Result.Init(DocumentsToTwipsF(AValue.Left), DocumentsToTwipsF(AValue.Top),
    DocumentsToTwipsF(AValue.Right), DocumentsToTwipsF(AValue.Bottom));
end;

function DocumentsToPixels(const AValue: Integer; const ADpi: Single): Integer;
begin
  if AValue >= 0 then
    Result := Trunc(ADpi * AValue / 300 + 0.99)
  else
    Result := Trunc(ADpi * AValue / 300 - 0.99);
end;

function DocumentsToPixels(const AValue: TPoint; const ADpiX, ADpiY: Single): TPoint;
begin
  Result.Init(DocumentsToPixels(AValue.X, ADpiX), DocumentsToPixels(AValue.Y, ADpiY));
end;

function DocumentsToPixels(const AValue: TSize; const ADpiX, ADpiY: Single): TSize;
begin
  Result.Init(DocumentsToPixels(AValue.cx, ADpiX), DocumentsToPixels(AValue.cy, ADpiY));
end;

function DocumentsToPixels(const AValue: TRect; const ADpiX, ADpiY: Single): TRect;
begin
  Result.Init(DocumentsToPixels(AValue.Left, ADpiX), DocumentsToPixels(AValue.Top, ADpiY),
    DocumentsToPixels(AValue.Right, ADpiX), DocumentsToPixels(AValue.Bottom, ADpiY));
end;

function DocumentsToPixels(const AValue: TdxRectF; const ADpiX, ADpiY: Single): TdxRectF;
begin
  Result.Init(DocumentsToPixelsF(AValue.Left, ADpiX), DocumentsToPixelsF(AValue.Top, ADpiY),
    DocumentsToPixelsF(AValue.Right, ADpiX), DocumentsToPixelsF(AValue.Bottom, ADpiY));
end;

function DocumentsToPixelsF(const AValue: Single; const ADpi: Single): Single;
begin
  Result := MulDivF(AValue, ADpi, 300);
end;

function DocumentsToPoints(const AValue: Integer): Integer;
begin
  Result := MulDiv(AValue, 6, 25);
end;

function DocumentsToPointsF(const AValue: Single): Single;
begin
  Result := MulDivF(AValue, 6, 25);
end;

function DocumentsToPointsFRound(const AValue: Single): Single;
begin
  Result := Round(DocumentsToPointsF(AValue));
end;

function DocumentsToHundredthsOfMillimeter(const AValue: Integer): Integer;
begin
  Result := MulDiv(AValue, 127, 15);
end;

function DocumentsToHundredthsOfMillimeter(const AValue: TSize): TSize;
begin
  Result.Init(DocumentsToHundredthsOfMillimeter(AValue.cx),
    DocumentsToHundredthsOfMillimeter(AValue.cy));
end;

function DocumentsToHundredthsOfInch(const AValue: Integer): Integer;
begin
  Result := Trunc(AValue / 3);
end;

function DocumentsToHundredthsOfInch(const AValue: TSize): TSize;
begin
  Result.Init(DocumentsToHundredthsOfInch(AValue.cx), DocumentsToHundredthsOfInch(AValue.cy));
end;

function DocumentsToCentimetersF(const AValue: Single): Single;
begin
  Result := MulDivF(AValue, 127, 15000);
end;

function DocumentsToInchesF(const AValue: Single): Single;
begin
  Result := AValue / 300;
end;

function DocumentsToMillimetersF(const AValue: Single): Single;
begin
  Result := MulDivF(AValue, 127, 1500);
end;

function DocumentsToPicasF(const AValue: Single): Single;
begin
  Result := AValue / 50;
end;

function TwipsToDocuments(const AValue: Integer): Integer;
begin
  Result := MulDiv(AValue, 5, 24);
end;

function TwipsToDocuments(const AValue: TSize): TSize;
begin
  Result.Init(TwipsToDocuments(AValue.cx), TwipsToDocuments(AValue.cy));
end;

function TwipsToEmu(const AValue: Integer): Integer;
begin
  Result := AValue * 635;
end;

function TwipsToEmuL(const AValue: Int64): Int64;
begin
  Result := AValue * 635;
end;

function TwipsToEmuF(const AValue: Single): Integer;
begin
  Result := Trunc(AValue * 635);
end;

function TwipsToDocumentsL(const AValue: Int64): Int64;
begin
  Result := MulDivL(AValue, 5, 24);
end;

function TwipsToDocuments(const AValue: TRect): TRect;
begin
  Result.Init(TwipsToDocuments(AValue.Left), TwipsToDocuments(AValue.Top),
    TwipsToDocuments(AValue.Right), TwipsToDocuments(AValue.Bottom));
end;

function TwipsToDocuments(const AValue: TdxRectF): TdxRectF;
begin
  Result.Init(TwipsToDocumentsF(AValue.Left), TwipsToDocumentsF(AValue.Top),
    TwipsToDocumentsF(AValue.Right), TwipsToDocumentsF(AValue.Bottom));
end;

function TwipsToPixels(const AValue: Integer; const ADpi: Single): Integer;
begin
  Result := Trunc(ADpi * AValue / 1440 + 0.99);
end;

function TwipsToPixelsL(const AValue: Int64; const ADpi: Single): Int64;
begin
  Result := Trunc(ADpi * AValue / 1440 + 0.99);
end;

function TwipsToPixels(const AValue: TPoint; const ADpiX, ADpiY: Single): TPoint;
begin
  Result.Init(TwipsToPixels(AValue.X, ADpiX), TwipsToPixels(AValue.Y, ADpiY));
end;

function TwipsToPixels(const AValue: TSize; const ADpiX, ADpiY: Single): TSize;
begin
  Result.Init(TwipsToPixels(AValue.cx, ADpiX), TwipsToPixels(AValue.cy, ADpiY));
end;

function TwipsToPixels(const AValue: TRect; const ADpiX, ADpiY: Single): TRect;
begin
  Result.Init(TwipsToPixels(AValue.Left, ADpiX), TwipsToPixels(AValue.Top, ADpiY),
    TwipsToPixels(AValue.Right, ADpiX), TwipsToPixels(AValue.Bottom, ADpiY));
end;

function TwipsToPixelsF(const AValue: Single; const ADpi: Single): Single;
begin
  Result := MulDivF(AValue, ADpi, 1440);
end;

function TwipsToHundredthsOfMillimeter(const AValue: TSize): TSize;
begin
  Result.Init(MulDiv(AValue.cx, 127, 72), MulDiv(AValue.cy, 127, 72));
end;

function TwipsToPointsF(const AValue: Single): Single;
begin
  Result := AValue / 20;
end;

function TwipsToPointsFRound(const AValue: Single): Single;
begin
  Result := Round(TwipsToPointsF(AValue));
end;

function TwipsToCentimetersF(const AValue: Single): Single;
begin
  Result := MulDivF(AValue, 2.54, 1440);
end;

function TwipsToInchesF(const AValue: Single): Single;
begin
  Result := AValue / 1440;
end;

function TwipsToMillimetersF(const AValue: Single): Single;
begin
  Result := MulDivF(AValue, 25.4, 1440);
end;

function TwipsToDocumentsF(const AValue: Single): Single;
begin
  Result := MulDivF(AValue, 5, 24);
end;

function TwipsToHundredthsOfInch(const AValue: Integer): Integer;
begin
  Result := MulDiv(AValue, 5, 72);
end;

function TwipsToHundredthsOfInch(const AValue: TSize): TSize;
begin
  Result.Init(TwipsToHundredthsOfInch(AValue.cx), TwipsToHundredthsOfInch(AValue.cy));
end;

function PixelsToLoMetric(Value: Integer; DPI: Integer = dxDefaultDPI): Integer;
begin
  Result := MulDiv(Value, 254, DPI);
end;

function PixelsToPoints(const AValue: Integer; const ADpi: Single): Integer;
begin
  if ADpi = 0 then
    Result := 0
  else
    Result := PixelsToPointsCore(AValue, ADpi);
end;

function PixelsToPointsF(const AValue: Single; const ADpi: Single): Single;
begin
  if ADpi = 0 then
    Result := 0
  else
    Result := PixelsToPointsCore(AValue, ADpi);
end;

function PixelsToDocuments(const AValue: Integer; const ADpi: Single): Integer;
begin
  if ADpi = 0 then
    Result := 0
  else
    Result := PixelsToDocumentsCore(AValue, ADpi);
end;

function PixelsToDocumentsF(const AValue: Single; const ADpi: Single): Single;
begin
  if ADpi = 0 then
    Result := 0
  else
    Result := MulDivF(AValue, 300, ADpi);
end;

function PixelsToTwips(const AValue: Integer; const ADpi: Single): Integer;
begin
  if ADpi = 0 then
    Result := 0
  else
    Result := PixelsToTwipsCore(AValue, ADpi);
end;

function PixelsToTwipsL(const AValue: Int64; const ADpi: Single): Int64;
begin
  if ADpi = 0 then
    Result := 0
  else
    Result := PixelsToTwipsCore(AValue, ADpi);
end;

function PixelsToTwipsF(const AValue: Single; const ADpi: Single): Single;
begin
  if ADpi = 0 then
    Result := 0
  else
    Result := PixelsToTwipsCore(AValue, ADpi);
end;

function PixelsToDocuments(const AValue: Double; const ADpi: Single): Integer;
begin
  if ADpi = 0 then
    Result := 0
  else
    Result := PixelsToDocumentsCore(AValue, ADpi);
end;

function PixelsToDocuments(const ARect: TRect; ADpiX, ADpiY: Single): TRect;
begin
  if ADpiX = 0 then
    ADpiX := 300;
  if ADpiY = 0 then
    ADpiY := 300;
  Result.Init(PixelsToDocumentsCore(ARect.Left, ADpiX), PixelsToDocumentsCore(ARect.Top, ADpiY),
    PixelsToDocumentsCore(ARect.Right, ADpiX), PixelsToDocumentsCore(ARect.Bottom, ADpiY));
end;

function PixelsToDocuments(const ARect: TdxRectF; ADpiX, ADpiY: Single): TdxRectF;
begin
  if ADpiX = 0 then
    ADpiX := 300;
  if ADpiY = 0 then
    ADpiY := 300;
  Result.Init(PixelsToDocumentsCoreF(ARect.Left, ADpiX), PixelsToDocumentsCoreF(ARect.Top, ADpiY),
    PixelsToDocumentsCoreF(ARect.Right, ADpiX), PixelsToDocumentsCoreF(ARect.Bottom, ADpiY));
end;

function PixelsToDocuments(const ASize: TSize; ADpiX, ADpiY: Single): TSize;
begin
  if ADpiX = 0 then
    ADpiX := 300;
  if ADpiY = 0 then
    ADpiY := 300;
  Result.Init(PixelsToDocumentsCore(ASize.cx, ADpiX), PixelsToDocumentsCore(ASize.cy, ADpiY));
end;

function PixelsToDocumentsRound(const AValue: Integer; ADpi: Single): Integer;
begin
  if ADpi = 0 then
    ADpi := 300;
  Result := PixelsToDocumentsCoreRound(AValue, ADpi);
end;

function PixelsToDocumentsRound(const ASize: TSize; ADpiX, ADpiY: Single): TSize;
begin
  if ADpiX = 0 then
    ADpiX := 300;
  if ADpiY = 0 then
    ADpiY := 300;
  Result.Init(PixelsToDocumentsCoreRound(ASize.cx, ADpiX), PixelsToDocumentsCoreRound(ASize.cy, ADpiY));
end;

function PixelsToTwips(const ASize: TSize; ADpiX, ADpiY: Single): TSize;
begin
  if ADpiX = 0 then
    ADpiX := 1440;
  if ADpiY = 0 then
    ADpiY := 1440;
  Result.Init(PixelsToTwipsCore(ASize.cx, ADpiX), PixelsToTwipsCore(ASize.cy, ADpiY));
end;

function PixelsToTwipsRound(const ASize: TSize; ADpiX, ADpiY: Single): TSize;
begin
  if ADpiX = 0 then
    ADpiX := 1440;
  if ADpiY = 0 then
    ADpiY := 1440;
  Result.Init(PixelsToTwipsCoreRound(ASize.cx, ADpiX), PixelsToTwipsCoreRound(ASize.cy, ADpiY));
end;

function PixelsToTwips(const ARect: TRect; ADpiX, ADpiY: Single): TRect;
begin
  if ADpiX = 0 then
    ADpiX := 1440;
  if ADpiY = 0 then
    ADpiY := 1440;
  Result.Init(PixelsToTwipsCore(ARect.Left, ADpiX), PixelsToTwipsCore(ARect.Top, ADpiY),
    PixelsToTwipsCore(ARect.Right, ADpiX), PixelsToTwipsCore(ARect.Bottom, ADpiY));
end;

function PixelsToDocuments(const APoint: TPoint; ADpiX, ADpiY: Single): TPoint;
begin
  if ADpiX = 0 then
    ADpiX := 300;
  if ADpiY = 0 then
    ADpiY := 300;
  Result.Init(PixelsToDocumentsCore(APoint.X, ADpiX), PixelsToDocumentsCore(APoint.Y, ADpiY));
end;

function PixelsToHundredthsOfMillimeter(const AValue: Integer; const ADpi: Single): Integer;
begin
  Result := Round(2540 * (AValue / ADpi));
end;

function PixelsToHundredthsOfMillimeter(const AValue: TSize; const ADpiX, ADpiY: Single): TSize;
begin
  Result.Init(PixelsToHundredthsOfMillimeter(AValue.cx, ADpiX),
    PixelsToHundredthsOfMillimeter(AValue.cy, ADpiY));
end;

function PixelsToHundredthsOfInch(const AValue: Integer; const ADpi: Single): Integer;
begin
  Result := MulDiv(AValue, 100, ADpi);
end;

function PixelsToHundredthsOfInch(const AValue: TSize; const ADpi: Single): TSize;
begin
  Result.Init(PixelsToHundredthsOfInch(AValue.cx, ADpi),
    PixelsToHundredthsOfInch(AValue.cy, ADpi));
end;

function HundredthsOfMillimeterToDocuments(const AValue: Integer): Integer;
begin
  Result := MulDiv(AValue, 15, 127);
end;

function HundredthsOfMillimeterToDocuments(const AValue: TSize): TSize;
begin
  Result.Init(HundredthsOfMillimeterToDocuments(AValue.cx),
    HundredthsOfMillimeterToDocuments(AValue.cy));
end;

function HundredthsOfMillimeterToDocumentsRound(const AValue: Integer): Integer; overload; inline;
begin
  Result := Round(MulDivF(AValue, 15, 127));
end;

function HundredthsOfMillimeterToDocumentsRound(const AValue: TSize): TSize; overload;
begin
  Result.Init(HundredthsOfMillimeterToDocumentsRound(AValue.cx),
    HundredthsOfMillimeterToDocumentsRound(AValue.cy));
end;

function HundredthsOfMillimeterToTwips(const AValue: Integer): Integer;
begin
  Result := MulDiv(AValue, 72, 127);
end;

function HundredthsOfMillimeterToTwips(const AValue: TSize): TSize;
begin
  Result.Init(HundredthsOfMillimeterToTwips(AValue.cx),
    HundredthsOfMillimeterToTwips(AValue.cy));
end;

function HundredthsOfMillimeterToTwipsRound(const AValue: Integer): Integer; overload; inline;
begin
  Result := Round(MulDivF(AValue, 72, 127));
end;

function HundredthsOfMillimeterToTwipsRound(const AValue: TSize): TSize; overload;
begin
  Result.Init(HundredthsOfMillimeterToTwipsRound(AValue.cx),
    HundredthsOfMillimeterToTwipsRound(AValue.cy));
end;

function HundredthsOfMillimeterToPixels(const AValue: Integer; const ADpi: Single): Integer; overload;
begin
  Result := Round(MulDivF(AValue, ADpi, 2540));
end;

function HundredthsOfMillimeterToPixels(const AValue: TSize; const ADpiX, ADpiY: Single): TSize; overload;
begin
  Result.Init(HundredthsOfMillimeterToPixels(AValue.cx, ADpiX),
    HundredthsOfMillimeterToPixels(AValue.cy, ADpiY));
end;

function PointsToDocuments(const AValue: Integer): Integer;
begin
  Result := MulDiv(AValue, 25, 6);
end;

function PointsToDocumentsF(const AValue: Single): Single;
begin
  Result := MulDivF(AValue, 25, 6);
end;

function PointsToTwipsF(const AValue: Single): Single;
begin
  Result := AValue * 20;
end;

function PointsToTwips(const AValue: Integer): Integer;
begin
  Result := AValue * 20;
end;

function PointsToPixels(const AValue: Integer; const ADpi: Single): Integer;
begin
  if AValue >= 0 then
    Result := Trunc((ADpi * ((AValue / 72) + 0.99)))
  else
    Result := Trunc((ADpi * ((AValue / 72) - 0.99)));
end;

function PointsToPixelsF(const AValue: Single; const ADpi: Single): Single;
begin
  Result := MulDivF(AValue, ADpi, 72);
end;

function EmuToDocuments(const AValue: Integer): Integer;
begin
  Result := MulDiv(AValue, 3, 9144);
end;

function EmuToDocumentsL(const AValue: Int64): Int64;
begin
  Result := MulDivL(AValue, 3, 9144);
end;

function EmuToDocumentsF(const AValue: Integer): Single;
begin
  Result := MulDivF(AValue, 3, 9144);
end;

function EmuToTwips(const AValue: Integer): Integer;
begin
  Result := Trunc(AValue / 635);
end;

function EmuToTwipsL(const AValue: Int64): Int64;
begin
  Result := Trunc(AValue / 635);
end;

function EmuToTwipsF(const AValue: Integer): Single;
begin
  Result := AValue / 635.0;
end;

function HundredthsOfInchToDocuments(const AValue: Integer): Integer;
begin
  Result := AValue * 3;
end;

function HundredthsOfInchToDocuments(const AValue: TSize): TSize;
begin
  Result.Init(HundredthsOfInchToDocuments(AValue.cx), HundredthsOfInchToDocuments(AValue.cy));
end;

function HundredthsOfInchToTwips(const AValue: Integer): Integer;
begin
  Result := MulDiv(AValue, 72, 5);
end;

function HundredthsOfInchToTwips(const AValue: TSize): TSize;
begin
  Result.Init(HundredthsOfInchToTwips(AValue.cx), HundredthsOfInchToTwips(AValue.cy));
end;

function CentimetersToDocumentsF(const AValue: Single): Single;
begin
  Result := MulDivF(AValue, 15000, 127);
end;

function CentimetersToTwipsF(const AValue: Single): Single;
begin
  Result := MulDivF(AValue, 1440, 2.54);
end;

function InchesToDocumentsF(const AValue: Single): Single;
begin
  Result := 300 * AValue;
end;

function InchesToLoMetric(const AValue: Single): Integer;
begin
  Result := Round(AValue * 254);
end;

function InchesToMillimeters(const AValue: Integer): Integer;
begin
  Result := MulDiv(AValue, 254, 10);
  Result := 10 * Round(Result / 10);
end;

function InchesToMillimetersF(const AValue: Single): Single;
begin
  Result := AValue * 25.4;
end;

function InchesToTwipsF(const AValue: Single): Single;
begin
  Result := 1440 * AValue;
end;

function InchesToPointsF(const AValue: Single): Single;
begin
  Result := 72 * AValue;
end;

function MillimetersToDocumentsF(const AValue: Single): Single;
begin
  Result := MulDivF(AValue, 1500, 127);
end;

function MillimetersToTwipsF(const AValue: Single): Single;
begin
  Result := MulDivF(AValue, 1440, 25.4);
end;

function MillimetersToInches(const AValue: Integer): Integer;
begin
  Result := MulDiv(AValue, 10, 254);
  Result := 100 * Round(Result / 100);
end;

function MillimetersToInchesF(const AValue: Single): Single;
begin
  Result := AValue / 25.4;
end;

function MillimetersToLoMetric(const AValue: Integer): Integer;
begin
  Result := AValue * 10;
end;

function MillimetersToLoMetric(const AValue: Single): Integer;
begin
  Result := Round(AValue * 10);
end;

function PicasToDocumentsF(const AValue: Single): Single;
begin
  Result := 50 * AValue;
end;

function PicasToTwipsF(const AValue: Single): Single;
begin
  Result := 240 * AValue;
end;

function LoMetricToInches(const AValue: Integer): Single;
begin
  Result := AValue / 254;
end;

function LoMetricToMillimeters(const AValue: Integer): Integer;
begin
  Result := Round(AValue / 10);
end;

function LoMetricToPixels(Value: Integer; DPI: Integer = dxDefaultDPI): Integer;
begin
  Result := MulDiv(Value, DPI, 254);
end;

function LoMetricToPixels(const Pt: TPoint; DPI: Integer = dxDefaultDPI): TPoint;
begin
  Result.X := LoMetricToPixels(Pt.X, DPI);
  Result.Y := LoMetricToPixels(Pt.Y, DPI);
end;

function MillimetersToPoints(const AValue: Integer): Integer;
begin
  Result := MulDiv(AValue, 360, 127);
end;

function MillimetersToPointsF(const AValue: Single): Single;
begin
  Result := MulDivF(AValue, 72, 25.4);
end;

{ TdxUnitsConverter }

class function TdxUnitsConverter.FromInch(const Value: Integer): Integer;
begin
  Result := Value;
end;

class function TdxUnitsConverter.FromInch(const Value: Single): Single;
begin
  Result := Value;
end;

class function TdxUnitsConverter.FromInch(const Value: TPoint): TPoint;
begin
  Result.X := FromInch(Value.X);
  Result.Y := FromInch(Value.Y);
end;

class function TdxUnitsConverter.FromInch(const Value: TRect): TRect;
begin
  Result.Left := FromInch(Value.Left);
  Result.Top := FromInch(Value.Top);
  Result.Right := FromInch(Value.Right);
  Result.Bottom := FromInch(Value.Bottom);
end;

class function TdxUnitsConverter.FromLoMetric(const Value: Integer): Single;
begin
  Result := Value;
end;

class function TdxUnitsConverter.FromLoMetric(const Value: TPoint): TPoint;
begin
  Result.X := Round(FromLoMetric(Value.X));
  Result.Y := Round(FromLoMetric(Value.Y));
end;

class function TdxUnitsConverter.FromLoMetric(const Value: TRect): TRect;
begin
  Result.Left := Round(FromLoMetric(Value.Left));
  Result.Top := Round(FromLoMetric(Value.Top));
  Result.Right := Round(FromLoMetric(Value.Right));
  Result.Bottom := Round(FromLoMetric(Value.Bottom));
end;

class function TdxUnitsConverter.FromMM(const Value: Integer): Integer;
begin
  Result := Value;
end;

class function TdxUnitsConverter.FromMM(const Value: Single): Single;
begin
  Result := Value;
end;

class function TdxUnitsConverter.FromMM(const Value: TPoint): TPoint;
begin
  Result.X := FromMM(Value.X);
  Result.Y := FromMM(Value.Y);
end;

class function TdxUnitsConverter.FromMM(const Value: TRect): TRect;
begin
  Result.Left := FromMM(Value.Left);
  Result.Top := FromMM(Value.Top);
  Result.Right := FromMM(Value.Right);
  Result.Bottom := FromMM(Value.Bottom);
end;

class function TdxUnitsConverter.ToInch(const Value: Integer): Integer;
begin
  Result := Value;
end;

class function TdxUnitsConverter.ToInch(const Value: Single): Single;
begin
  Result := Value;
end;

class function TdxUnitsConverter.ToInch(const Value: TPoint): TPoint;
begin
  Result.X := ToInch(Value.X);
  Result.Y := ToInch(Value.Y);
end;

class function TdxUnitsConverter.ToInch(const Value: TRect): TRect;
begin
  Result.Left := ToInch(Value.Left);
  Result.Top := ToInch(Value.Top);
  Result.Right := ToInch(Value.Right);
  Result.Bottom := ToInch(Value.Bottom);
end;

class function TdxUnitsConverter.ToLoMetric(const Value: Integer): Integer;
begin
  Result := Value;
end;

class function TdxUnitsConverter.ToLoMetric(const Value: Single): Single;
begin
  Result := Value;
end;

class function TdxUnitsConverter.ToLoMetric(const Value: TPoint): TPoint;
begin
  Result.X := ToLoMetric(Value.X);
  Result.Y := ToLoMetric(Value.Y);
end;

class function TdxUnitsConverter.ToLoMetric(const Value: TRect): TRect;
begin
  Result.Left := ToLoMetric(Value.Left);
  Result.Top := ToLoMetric(Value.Top);
  Result.Right := ToLoMetric(Value.Right);
  Result.Bottom := ToLoMetric(Value.Bottom);
end;

class function TdxUnitsConverter.ToMM(const Value: Integer): Integer;
begin
  Result := Value;
end;

class function TdxUnitsConverter.ToMM(const Value: Single): Single;
begin
  Result := Value;
end;

class function TdxUnitsConverter.ToMM(const Value: TPoint): TPoint;
begin
  Result.X := ToMM(Value.X);
  Result.Y := ToMM(Value.Y);
end;

class function TdxUnitsConverter.ToMM(const Value: TRect): TRect;
begin
  Result.Left := ToMM(Value.Left);
  Result.Top := ToMM(Value.Top);
  Result.Right := ToMM(Value.Right);
  Result.Bottom := ToMM(Value.Bottom);
end;

{ TdxInchesUnits }

class function TdxInchesUnits.FromLoMetric(const Value: Integer): Single;
begin
  Result := LoMetricToInches(Value);
end;

class function TdxInchesUnits.FromMM(const Value: Integer): Integer;
begin
  Result := MillimetersToInches(Value);
end;

class function TdxInchesUnits.FromMM(const Value: Single): Single;
begin
  Result := MillimetersToInchesF(Value);
end;

class function TdxInchesUnits.ToLoMetric(const Value: Integer): Integer;
begin
  Result := InchesToLoMetric(Value);
end;

class function TdxInchesUnits.ToLoMetric(const Value: Single): Single;
begin
  Result := InchesToLoMetric(Value);
end;

class function TdxInchesUnits.ToMM(const Value: Integer): Integer;
begin
  Result := InchesToMillimeters(Value);
end;

class function TdxInchesUnits.ToMM(const Value: Single): Single;
begin
  Result := InchesToMillimetersF(Value);
end;

{ TdxMillimetersUnits }

class function TdxMillimetersUnits.FromInch(const Value: Integer): Integer;
begin
  Result := InchesToMillimeters(Value);
end;

class function TdxMillimetersUnits.FromInch(const Value: Single): Single;
begin
  Result := InchesToMillimetersF(Value);
end;

class function TdxMillimetersUnits.FromLoMetric(const Value: Integer): Single;
begin
  Result := LoMetricToMillimeters(Value);
end;

class function TdxMillimetersUnits.ToInch(const Value: Integer): Integer;
begin
  Result := MillimetersToInches(Value);
end;

class function TdxMillimetersUnits.ToInch(const Value: Single): Single;
begin
  Result := MillimetersToInchesF(Value);
end;

class function TdxMillimetersUnits.ToLoMetric(const Value: Integer): Integer;
begin
  Result := MillimetersToLoMetric(Value);
end;

class function TdxMillimetersUnits.ToLoMetric(const Value: Single): Single;
begin
  Result := MillimetersToLoMetric(Round(Value));
end;

end.
