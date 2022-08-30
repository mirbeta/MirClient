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

unit dxRichEdit.DocumentModel.UnitTwipsConverter;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types,
  dxRichEdit.DocumentLayout.UnitConverter,
  dxRichEdit.DocumentModel.UnitConverter,
  dxRichEdit.DocumentModel.UnitToLayoutUnitConverter;

type

  { TdxDocumentModelUnitTwipsConverter }

  TdxDocumentModelUnitTwipsConverter = class(TdxDocumentModelUnitConverter)
  public
    function CreateConverterToLayoutUnits(const AUnit: TdxDocumentLayoutUnit; const ADpi: Single): TdxDocumentModelUnitToLayoutUnitConverter; override;
    function TwipsToModelUnits(const AValue: Integer): Integer; overload; override;
    function TwipsToModelUnits(const AValue: TSize): TSize; overload; override;
    function MillimetersToModelUnitsF(const AValue: Single): Single; override;
    function PointsToModelUnits(const AValue: Integer): Integer; override;
    function PointsToModelUnitsF(const AValue: Single): Single; override;
    function PixelsToModelUnits(const AValue: Integer; const ADpi: Single): Integer; overload; override;
    function PixelsToModelUnits(const AValue: TSize; const ADpiX, ADpiY: Single): TSize; overload; override;
    function HundredthsOfInchToModelUnits(const AValue: Integer): Integer; overload; override;
    function HundredthsOfInchToModelUnits(const AValue: TSize): TSize; overload; override;
    function HundredthsOfMillimeterToModelUnits(const AValue: Integer): Integer; overload; override;
    function HundredthsOfMillimeterToModelUnits(const AValue: TSize): TSize; overload; override;
    function HundredthsOfMillimeterToModelUnitsRound(const AValue: Integer): Integer; override;
    function CentimetersToModelUnitsF(const AValue: Single): Single; override;
    function InchesToModelUnitsF(const AValue: Single): Single; override;
    function PicasToModelUnitsF(const AValue: Single): Single; override;
    function DocumentsToModelUnits(const AValue: Integer): Integer; overload; override;
    function DocumentsToModelUnits(const AValue: TSize): TSize; overload; override;
    function DocumentsToModelUnitsF(const AValue: Single): Single; override;
    function ModelUnitsToTwips(const AValue: Integer): Integer; overload; override;
    function ModelUnitsToTwipsF(const AValue: Single): Single; override;
    function ModelUnitsToTwips(const AValue: TSize): TSize; overload; override;
    function ModelUnitsToHundredthsOfMillimeter(const AValue: TSize): TSize; override;
    function ModelUnitsToPointsF(const AValue: Single): Single; override;
    function ModelUnitsToPointsFRound(const AValue: Single): Single; override;
    function ModelUnitsToPixels(const AValue: Integer; const ADpi: Single): Integer; override;
    function ModelUnitsToPixelsF(const AValue: Single; const ADpi: Single): Single; override;
    function ModelUnitsToCentimetersF(const AValue: Single): Single; override;
    function ModelUnitsToInchesF(const AValue: Single): Single; override;
    function ModelUnitsToMillimetersF(const AValue: Single): Single; override;
    function ModelUnitsToDocumentsF(const AValue: Single): Single; override;
    function ModelUnitsToHundredthsOfInch(const AValue: Integer): Integer; overload; override;
    function ModelUnitsToHundredthsOfInch(const AValue: TSize): TSize; overload; override;
    function EmuToModelUnits(const AValue: Integer): Integer; override;
    function EmuToModelUnitsL(const AValue: Int64): Int64; override;
    function EmuToModelUnitsF(const AValue: Integer): Single; override;
    function ModelUnitsToEmu(const AValue: Integer): Integer; override;
    function ModelUnitsToEmuL(const AValue: Int64): Int64; override;
    function ModelUnitsToEmuF(const AValue: Single): Integer; override;
    function FDToModelUnits(const AValue: Integer): Integer; override;
    function ModelUnitsToFD(const AValue: Integer): Integer; override;
  end;

implementation

uses
  cxGeometry, dxTypeHelpers,
  dxMeasurementUnits,
  dxRichEdit.DocumentModel.TwipsToLayoutTwipsConverter,
  dxRichEdit.DocumentModel.TwipsToLayoutDocumentsConverter,
  dxRichEdit.DocumentModel.TwipsToLayoutPixelsConverter;

{ TdxDocumentModelUnitTwipsConverter }

function TdxDocumentModelUnitTwipsConverter.CreateConverterToLayoutUnits(const AUnit: TdxDocumentLayoutUnit;
  const ADpi: Single): TdxDocumentModelUnitToLayoutUnitConverter;
begin
  case AUnit of
    TdxDocumentLayoutUnit.Pixel:
      Result := TdxDocumentModelTwipsToLayoutPixelsConverter.Create(ADpi);
    TdxDocumentLayoutUnit.Document:
      Result := TdxDocumentModelTwipsToLayoutDocumentsConverter.Create;
  else
    Result := TdxDocumentModelTwipsToLayoutTwipsConverter.Create;
  end;
end;

function TdxDocumentModelUnitTwipsConverter.TwipsToModelUnits(const AValue: Integer): Integer;
begin
  Result := AValue;
end;

function TdxDocumentModelUnitTwipsConverter.TwipsToModelUnits(const AValue: TSize): TSize;
begin
  Result := AValue;
end;

function TdxDocumentModelUnitTwipsConverter.MillimetersToModelUnitsF(const AValue: Single): Single;
begin
  Result := MillimetersToTwipsF(AValue);
end;

function TdxDocumentModelUnitTwipsConverter.PointsToModelUnits(const AValue: Integer): Integer;
begin
  Result := PointsToTwips(AValue);
end;

function TdxDocumentModelUnitTwipsConverter.PointsToModelUnitsF(const AValue: Single): Single;
begin
  Result := PointsToTwipsF(AValue);
end;

function TdxDocumentModelUnitTwipsConverter.PixelsToModelUnits(const AValue: Integer; const ADpi: Single): Integer;
begin
  Result := PixelsToTwips(AValue, ADpi);
end;

function TdxDocumentModelUnitTwipsConverter.PixelsToModelUnits(const AValue: TSize; const ADpiX, ADpiY: Single): TSize;
begin
  Result := PixelsToTwips(AValue, ADpiX, ADpiY);
end;

function TdxDocumentModelUnitTwipsConverter.HundredthsOfInchToModelUnits(const AValue: Integer): Integer;
begin
  Result := HundredthsOfInchToTwips(AValue);
end;

function TdxDocumentModelUnitTwipsConverter.HundredthsOfInchToModelUnits(const AValue: TSize): TSize;
begin
  Result := HundredthsOfInchToTwips(AValue);
end;

function TdxDocumentModelUnitTwipsConverter.HundredthsOfMillimeterToModelUnits(const AValue: Integer): Integer;
begin
  Result := HundredthsOfMillimeterToTwips(AValue);
end;

function TdxDocumentModelUnitTwipsConverter.HundredthsOfMillimeterToModelUnits(const AValue: TSize): TSize;
begin
  Result := HundredthsOfMillimeterToTwips(AValue);
end;

function TdxDocumentModelUnitTwipsConverter.HundredthsOfMillimeterToModelUnitsRound(const AValue: Integer): Integer;
begin
  Result := Round(1440 * (AValue / 2540));
end;

function TdxDocumentModelUnitTwipsConverter.CentimetersToModelUnitsF(const AValue: Single): Single;
begin
  Result := CentimetersToTwipsF(AValue);
end;

function TdxDocumentModelUnitTwipsConverter.InchesToModelUnitsF(const AValue: Single): Single;
begin
  Result := InchesToTwipsF(AValue);
end;

function TdxDocumentModelUnitTwipsConverter.PicasToModelUnitsF(const AValue: Single): Single;
begin
  Result := PicasToTwipsF(AValue);
end;

function TdxDocumentModelUnitTwipsConverter.DocumentsToModelUnits(const AValue: Integer): Integer;
begin
  Result := DocumentsToTwips(AValue);
end;

function TdxDocumentModelUnitTwipsConverter.DocumentsToModelUnits(const AValue: TSize): TSize;
begin
  Result := DocumentsToTwips(AValue);
end;

function TdxDocumentModelUnitTwipsConverter.DocumentsToModelUnitsF(const AValue: Single): Single;
begin
  Result := DocumentsToTwipsF(AValue);
end;

function TdxDocumentModelUnitTwipsConverter.ModelUnitsToTwips(const AValue: Integer): Integer;
begin
  Result := AValue;
end;

function TdxDocumentModelUnitTwipsConverter.ModelUnitsToTwipsF(const AValue: Single): Single;
begin
  Result := AValue;
end;

function TdxDocumentModelUnitTwipsConverter.ModelUnitsToTwips(const AValue: TSize): TSize;
begin
  Result := AValue;
end;

function TdxDocumentModelUnitTwipsConverter.ModelUnitsToHundredthsOfMillimeter(const AValue: TSize): TSize;
begin
  Result := TwipsToHundredthsOfMillimeter(AValue);
end;

function TdxDocumentModelUnitTwipsConverter.ModelUnitsToPointsF(const AValue: Single): Single;
begin
  Result := TwipsToPointsF(AValue);
end;

function TdxDocumentModelUnitTwipsConverter.ModelUnitsToPointsFRound(const AValue: Single): Single;
begin
  Result := TwipsToPointsFRound(AValue);
end;

function TdxDocumentModelUnitTwipsConverter.ModelUnitsToPixels(const AValue: Integer; const ADpi: Single): Integer;
begin
  Result := TwipsToPixels(AValue, ADpi);
end;

function TdxDocumentModelUnitTwipsConverter.ModelUnitsToPixelsF(const AValue: Single; const ADpi: Single): Single;
begin
  Result := TwipsToPixelsF(AValue, ADpi);
end;

function TdxDocumentModelUnitTwipsConverter.ModelUnitsToCentimetersF(const AValue: Single): Single;
begin
  Result := TwipsToCentimetersF(AValue);
end;

function TdxDocumentModelUnitTwipsConverter.ModelUnitsToInchesF(const AValue: Single): Single;
begin
  Result := TwipsToInchesF(AValue);
end;

function TdxDocumentModelUnitTwipsConverter.ModelUnitsToMillimetersF(const AValue: Single): Single;
begin
  Result := TwipsToMillimetersF(AValue);
end;

function TdxDocumentModelUnitTwipsConverter.ModelUnitsToDocumentsF(const AValue: Single): Single;
begin
  Result := TwipsToDocumentsF(AValue);
end;

function TdxDocumentModelUnitTwipsConverter.ModelUnitsToHundredthsOfInch(const AValue: Integer): Integer;
begin
  Result := TwipsToHundredthsOfInch(AValue);
end;

function TdxDocumentModelUnitTwipsConverter.ModelUnitsToHundredthsOfInch(const AValue: TSize): TSize;
begin
  Result := TwipsToHundredthsOfInch(AValue);
end;

function TdxDocumentModelUnitTwipsConverter.EmuToModelUnits(const AValue: Integer): Integer;
begin
  Result := EmuToTwips(AValue);
end;

function TdxDocumentModelUnitTwipsConverter.EmuToModelUnitsL(const AValue: Int64): Int64;
begin
  Result := EmuToTwipsL(AValue);
end;

function TdxDocumentModelUnitTwipsConverter.EmuToModelUnitsF(const AValue: Integer): Single;
begin
  Result := EmuToTwipsF(AValue);
end;

function TdxDocumentModelUnitTwipsConverter.ModelUnitsToEmu(const AValue: Integer): Integer;
begin
  Result := TwipsToEmu(AValue);
end;

function TdxDocumentModelUnitTwipsConverter.ModelUnitsToEmuL(const AValue: Int64): Int64;
begin
  Result := TwipsToEmuL(AValue);
end;

function TdxDocumentModelUnitTwipsConverter.ModelUnitsToEmuF(const AValue: Single): Integer;
begin
  Result := TwipsToEmuF(AValue);
end;

function TdxDocumentModelUnitTwipsConverter.FDToModelUnits(const AValue: Integer): Integer;
begin
  Result := Trunc(AValue * (1875 / 2048));
end;

function TdxDocumentModelUnitTwipsConverter.ModelUnitsToFD(const AValue: Integer): Integer;
begin
  Result := Trunc(AValue * (2048 / 1875));
end;

end.
