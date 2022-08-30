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

unit dxRichEdit.DocumentLayout.UnitDocumentConverter;

interface

{$I cxVer.inc}
{$I dxRichEditControl.inc}

uses
  Types, cxGeometry,
  dxRichEdit.DocumentLayout.UnitConverter;

type
  { TdxDocumentLayoutUnitDocumentConverter }

  TdxDocumentLayoutUnitDocumentConverter = class sealed (TdxDocumentLayoutUnitConverter)
  public const
    DefaultDpi = 300;
  protected
    function GetDpi: Single; override;
    function GetFontSizeScale: Single; override;
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
    function LayoutUnitsToHundredthsOfInch(const AValue: Size): Size; overload; override;
    function LayoutUnitsToPixels(const AValue: Integer; const ADpi: Single): Integer; overload; override;
    function LayoutUnitsToPixels(const AValue: Size; const ADpiX, ADpiY: Single): Size; overload; override;
    function LayoutUnitsToPixels(const AValue: TPoint; const ADpiX, ADpiY: Single): TPoint; overload; override;
    function LayoutUnitsToPixels(const AValue: TRect; const ADpiX, ADpiY: Single): TRect; overload; override;
    function LayoutUnitsToPixelsF(const AValue: Single; const ADpi: Single): Single; override;
    function LayoutUnitsToPointsF(const AValue: Single): Single; override;
    function LayoutUnitsToTwips(const AValue: Int64): Int64; overload; override;
    function LayoutUnitsToTwips(const AValue: Integer): Integer; overload; override;
    function MillimetersToFontUnitsF(const AValue: Single): Single; override;
    function PixelsToLayoutUnits(const AValue: Integer; const ADpi: Single): Integer; overload; override;
    function PixelsToLayoutUnits(const AValue: Size; const ADpiX, ADpiY: Single): Size; overload; override;
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


function TdxDocumentLayoutUnitDocumentConverter.DocumentsToFontUnitsF(const AValue: Single): Single;
begin
  Result := AValue;
end;

function TdxDocumentLayoutUnitDocumentConverter.DocumentsToLayoutUnits(const AValue: Integer): Integer;
begin
  Result := AValue;
end;

function TdxDocumentLayoutUnitDocumentConverter.DocumentsToLayoutUnits(const AValue: TdxRectF): TdxRectF;
begin
  Result := AValue;
end;

function TdxDocumentLayoutUnitDocumentConverter.DocumentsToLayoutUnits(const AValue: TRect): TRect;
begin
  Result := AValue;
end;

function TdxDocumentLayoutUnitDocumentConverter.InchesToFontUnitsF(const AValue: Single): Single;
begin
  Result := InchesToDocumentsF(AValue);
end;

function TdxDocumentLayoutUnitDocumentConverter.LayoutUnitsToDocuments(const AValue: TdxRectF): TdxRectF;
begin
  Result := AValue;
end;

function TdxDocumentLayoutUnitDocumentConverter.LayoutUnitsToDocuments(const AValue: TRect): TRect;
begin
  Result := AValue;
end;

function TdxDocumentLayoutUnitDocumentConverter.LayoutUnitsToHundredthsOfInch(const AValue: Integer): Integer;
begin
  Result := DocumentsToHundredthsOfInch(AValue);
end;

function TdxDocumentLayoutUnitDocumentConverter.LayoutUnitsToHundredthsOfInch(const AValue: Size): Size;
begin
  Result := DocumentsToHundredthsOfInch(AValue);
end;

function TdxDocumentLayoutUnitDocumentConverter.LayoutUnitsToPixels(const AValue: Integer;
  const ADpi: Single): Integer;
begin
  Result := DocumentsToPixels(AValue, ADpi);
end;

function TdxDocumentLayoutUnitDocumentConverter.LayoutUnitsToPixels(const AValue: Size;
  const ADpiX, ADpiY: Single): Size;
begin
  Result := DocumentsToPixels(AValue, ADpiX, ADpiY);
end;

function TdxDocumentLayoutUnitDocumentConverter.LayoutUnitsToPixels(const AValue: TPoint;
  const ADpiX, ADpiY: Single): TPoint;
begin
  Result := DocumentsToPixels(AValue, ADpiX, ADpiY);
end;

function TdxDocumentLayoutUnitDocumentConverter.LayoutUnitsToPixels(const AValue: TRect;
  const ADpiX, ADpiY: Single): TRect;
begin
  Result := DocumentsToPixels(AValue, ADpiX, ADpiY);
end;

function TdxDocumentLayoutUnitDocumentConverter.LayoutUnitsToPixelsF(const AValue: Single;
  const ADpi: Single): Single;
begin
  Result := DocumentsToPixelsF(AValue, ADpi);
end;

function TdxDocumentLayoutUnitDocumentConverter.LayoutUnitsToPointsF(const AValue: Single): Single;
begin
  Result := DocumentsToPointsF(AValue);
end;

function TdxDocumentLayoutUnitDocumentConverter.LayoutUnitsToTwips(const AValue: Int64): Int64;
begin
  Result := DocumentsToTwipsL(AValue);
end;

function TdxDocumentLayoutUnitDocumentConverter.LayoutUnitsToTwips(const AValue: Integer): Integer;
begin
  Result := DocumentsToTwips(AValue);
end;

function TdxDocumentLayoutUnitDocumentConverter.MillimetersToFontUnitsF(const AValue: Single): Single;
begin
  Result := MillimetersToDocumentsF(AValue);
end;

function TdxDocumentLayoutUnitDocumentConverter.PixelsToLayoutUnits(const AValue: Integer;
  const ADpi: Single): Integer;
begin
  Result := PixelsToDocuments(AValue, ADpi);
end;

function TdxDocumentLayoutUnitDocumentConverter.PixelsToLayoutUnits(const AValue: Size;
  const ADpiX, ADpiY: Single): Size;
begin
  Result := PixelsToDocuments(AValue, ADpiX, ADpiY);
end;

function TdxDocumentLayoutUnitDocumentConverter.PixelsToLayoutUnits(const AValue: TRect;
  const ADpiX, ADpiY: Single): TRect;
begin
  Result := PixelsToDocuments(AValue, ADpiX, ADpiY);
end;

function TdxDocumentLayoutUnitDocumentConverter.PixelsToLayoutUnitsF(const AValue: Single;
  const ADpi: Single): Single;
begin
  Result := PixelsToDocumentsF(AValue, ADpi);
end;

function TdxDocumentLayoutUnitDocumentConverter.PointsToFontUnits(const AValue: Integer): Integer;
begin
  Result := PointsToDocuments(AValue);
end;

function TdxDocumentLayoutUnitDocumentConverter.PointsToFontUnitsF(const AValue: Single): Single;
begin
  Result := PointsToDocumentsF(AValue);
end;

function TdxDocumentLayoutUnitDocumentConverter.PointsToLayoutUnits(const AValue: Integer): Integer;
begin
  Result := PointsToDocuments(AValue);
end;

function TdxDocumentLayoutUnitDocumentConverter.PointsToLayoutUnitsF(const AValue: Single): Single;
begin
  Result := PointsToDocumentsF(AValue);
end;

function TdxDocumentLayoutUnitDocumentConverter.SnapToPixels(const AValue: Integer; const ADpi: Single): Integer;
begin
  Result := AValue;
end;

function TdxDocumentLayoutUnitDocumentConverter.TwipsToLayoutUnits(const AValue: Int64): Int64;
begin
  Result := TwipsToDocumentsL(AValue);
end;

function TdxDocumentLayoutUnitDocumentConverter.TwipsToLayoutUnits(const AValue: Integer): Integer;
begin
  Result := TwipsToDocuments(AValue);
end;

function TdxDocumentLayoutUnitDocumentConverter.GetDpi: Single;
begin
  Result := DefaultDpi;
end;

function TdxDocumentLayoutUnitDocumentConverter.GetFontSizeScale: Single;
begin
  Result := 1;
end;

function TdxDocumentLayoutUnitDocumentConverter.GetFontUnit: TdxLayoutGraphicsUnit;
begin
  Result := TdxLayoutGraphicsUnit.Document;
end;

function TdxDocumentLayoutUnitDocumentConverter.GetGraphicsPageScale: Single;
begin
  Result := 1;
end;

function TdxDocumentLayoutUnitDocumentConverter.GetGraphicsPageUnit: TdxLayoutGraphicsUnit;
begin
  Result := TdxLayoutGraphicsUnit.Document;
end;

end.

