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

unit dxRichEdit.Utils.ModelUnitConverter;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types;

type
  { IdxDocumentModelUnitConverter }

  IdxDocumentModelUnitConverter = interface
    function TwipsToModelUnits(const AValue: Integer): Integer; overload;
    function TwipsToModelUnits(const AValue: TSize): TSize; overload;
    function MillimetersToModelUnitsF(const AValue: Single): Single;
    function PointsToModelUnits(const AValue: Integer): Integer;
    function PointsToModelUnitsF(const AValue: Single): Single;
    function PixelsToModelUnits(const AValue: Integer; const ADpi: Single): Integer; overload;
    function PixelsToModelUnits(const AValue: TSize; const ADpiX, ADpiY: Single): TSize; overload;
    function HundredthsOfInchToModelUnits(const AValue: Integer): Integer; overload;
    function HundredthsOfInchToModelUnits(const AValue: TSize): TSize; overload;
    function HundredthsOfMillimeterToModelUnits(const AValue: Integer): Integer; overload;
    function HundredthsOfMillimeterToModelUnits(const AValue: TSize): TSize; overload;
    function HundredthsOfMillimeterToModelUnitsRound(const AValue: Integer): Integer;
    function CentimetersToModelUnitsF(const AValue: Single): Single;
    function InchesToModelUnitsF(const AValue: Single): Single;
    function PicasToModelUnitsF(const AValue: Single): Single;
    function DocumentsToModelUnits(const AValue: Integer): Integer; overload;
    function DocumentsToModelUnits(const AValue: TSize): TSize; overload;
    function DocumentsToModelUnitsF(const AValue: Single): Single;
    function DegreeToModelUnits(const AValue: Single): Integer;
    function FDToModelUnits(const AValue: Integer): Integer;
    function ModelUnitsToTwips(const AValue: Integer): Integer; overload;
    function ModelUnitsToTwipsF(const AValue: Single): Single;
    function ModelUnitsToTwips(const AValue: TSize): TSize; overload;
    function ModelUnitsToHundredthsOfMillimeter(const AValue: TSize): TSize;
    function ModelUnitsToPointsF(const AValue: Single): Single;
    function ModelUnitsToPointsFRound(const AValue: Single): Single;
    function ModelUnitsToPixels(const AValue: Integer; const ADpi: Single): Integer;
    function ModelUnitsToPixelsF(const AValue: Single; const ADpi: Single): Single;
    function ModelUnitsToCentimetersF(const AValue: Single): Single;
    function ModelUnitsToInchesF(const AValue: Single): Single;
    function ModelUnitsToMillimetersF(const AValue: Single): Single;
    function ModelUnitsToDocumentsF(const AValue: Single): Single;
    function ModelUnitsToHundredthsOfInch(const AValue: Integer): Integer; overload;
    function ModelUnitsToHundredthsOfInch(const AValue: TSize): TSize; overload;
    function ModelUnitsToDegree(const AValue: Integer): Integer;
    function ModelUnitsToFD(const AValue: Integer): Integer;
  end;

implementation

end.
