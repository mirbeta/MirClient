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

unit dxRichEdit.NumberConverters.EnglishUS;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Generics.Defaults, Generics.Collections,
  dxCoreClasses,
  dxRichEdit.NativeApi,
  dxRichEdit.DocumentModel.NumberingFormatting,
  dxRichEdit.NumberConverters;

type
  { TdxCardinalEnglishNumericsProvider }

  TdxCardinalEnglishNumericsProvider = class(TInterfacedObject, IdxNumericsProvider)
  strict private
    class var
      FSeparator: TArray<string>;
      FGeneralSingles: TArray<string>;
      FTeens: TArray<string>;
      FTenths: TArray<string>;
      FHundreds: TArray<string>;
      FThousands: TArray<string>;
      FMillion: TArray<string>;
      FBillion: TArray<string>;
      FTrillion: TArray<string>;
      FQuadrillion: TArray<string>;
      FQuintillion: TArray<string>;
    class constructor Initialize;
  protected
    //IdxNumericsProvider
    function GetBillion: TArray<string>;
    function GetHundreds: TArray<string>;
    function GetMillion: TArray<string>;
    function GetQuadrillion: TArray<string>;
    function GetQuintillion: TArray<string>;
    function GetSeparator: TArray<string>;
    function GetSingles: TArray<string>;
    function GetSinglesNumeral: TArray<string>;
    function GetTeens: TArray<string>;
    function GetTenths: TArray<string>;
    function GetThousands: TArray<string>;
    function GetTrillion: TArray<string>;
  end;

  { TdxOrdinalEnglishNumericsProvider }

  TdxOrdinalEnglishNumericsProvider = class(TInterfacedObject, IdxNumericsProvider)
  strict private
    class var
      FBillion: TArray<string>;
      FGeneralSingles: TArray<string>;
      FHundreds: TArray<string>;
      FMillion: TArray<string>;
      FQuadrillion: TArray<string>;
      FQuintillion: TArray<string>;
      FSeparator: TArray<string>;
      FTeens: TArray<string>;
      FTenths: TArray<string>;
      FThousands: TArray<string>;
      FTrillion: TArray<string>;
  strict private
    class constructor Initialize;
  public
    //IdxNumericsProvider
    function GetBillion: TArray<string>;
    function GetHundreds: TArray<string>;
    function GetMillion: TArray<string>;
    function GetQuadrillion: TArray<string>;
    function GetQuintillion: TArray<string>;
    function GetSeparator: TArray<string>;
    function GetSingles: TArray<string>;
    function GetSinglesNumeral: TArray<string>;
    function GetTeens: TArray<string>;
    function GetTenths: TArray<string>;
    function GetThousands: TArray<string>;
    function GetTrillion: TArray<string>;
  end;

  { TdxDescriptiveCardinalEnglishNumberConverter }

  TdxDescriptiveCardinalEnglishNumberConverter = class(TdxDescriptiveNumberConverterBase)
  protected
    function GetType: TdxRichEditNumberingFormat; override;
  end;

  { TdxDescriptiveOrdinalEnglishNumberConverter }

  TdxDescriptiveOrdinalEnglishNumberConverter = class(TdxDescriptiveNumberConverterBase)
  protected
    function GetType: TdxRichEditNumberingFormat; override;
    procedure GenerateDigits(ADigits: TdxDigitInfoCollection; AValue: Int64); override;
  end;

  { TdxOrdinalEnglishNumberConverter }

  TdxOrdinalEnglishNumberConverter = class(TdxOrdinalBasedNumberConverter)
  strict private
    class var
      FEnding: TArray<string>;
  strict private
    class constructor Initialize;
  protected
    function ConvertNumberCore(AValue: Int64): string; override;
    function GetType: TdxRichEditNumberingFormat; override;
  end;

implementation

{ TdxCardinalEnglishNumericsProvider }

class constructor TdxCardinalEnglishNumericsProvider.Initialize;
begin
  FSeparator := TArray<string>.Create(' ', '-');
  FGeneralSingles := TArray<string>.Create('one', 'two', 'three', 'four', 'five', 'six', 'seven', 'eight', 'nine', 'zero');
  FTeens := TArray<string>.Create('ten', 'eleven', 'twelve', 'thirteen', 'fourteen', 'fifteen', 'sixteen', 'seventeen', 'eighteen', 'nineteen');
  FTenths := TArray<string>.Create('twenty', 'thirty', 'forty', 'fifty', 'sixty', 'seventy', 'eighty', 'ninety');
  FHundreds := TArray<string>.Create('one hundred', 'two hundred', 'three hundred', 'four hundred', 'five hundred', 'six hundred', 'seven hundred', 'eight hundred', 'nine hundred');
  FThousands := TArray<string>.Create('thousand');
  FMillion := TArray<string>.Create('million');
  FBillion := TArray<string>.Create('billion');
  FTrillion := TArray<string>.Create('trillion');
  FQuadrillion := TArray<string>.Create('quadrillion');
  FQuintillion := TArray<string>.Create('quintillion');
end;

function TdxCardinalEnglishNumericsProvider.GetBillion: TArray<string>;
begin
  Result := FBillion;
end;

function TdxCardinalEnglishNumericsProvider.GetHundreds: TArray<string>;
begin
  Result := FHundreds;
end;

function TdxCardinalEnglishNumericsProvider.GetMillion: TArray<string>;
begin
  Result := FMillion;
end;

function TdxCardinalEnglishNumericsProvider.GetQuadrillion: TArray<string>;
begin
  Result := FQuadrillion;
end;

function TdxCardinalEnglishNumericsProvider.GetQuintillion: TArray<string>;
begin
  Result := FQuintillion;
end;

function TdxCardinalEnglishNumericsProvider.GetSeparator: TArray<string>;
begin
  Result := FSeparator;
end;

function TdxCardinalEnglishNumericsProvider.GetSingles: TArray<string>;
begin
  Result := FGeneralSingles;
end;

function TdxCardinalEnglishNumericsProvider.GetSinglesNumeral: TArray<string>;
begin
  Result := FGeneralSingles;
end;

function TdxCardinalEnglishNumericsProvider.GetTeens: TArray<string>;
begin
  Result := FTeens;
end;

function TdxCardinalEnglishNumericsProvider.GetTenths: TArray<string>;
begin
  Result := FTenths;
end;

function TdxCardinalEnglishNumericsProvider.GetThousands: TArray<string>;
begin
  Result := FThousands;
end;

function TdxCardinalEnglishNumericsProvider.GetTrillion: TArray<string>;
begin
  Result := FTrillion;
end;

{ TdxOrdinalEnglishNumericsProvider }

function TdxOrdinalEnglishNumericsProvider.GetBillion: TArray<string>;
begin
  Result := FBillion;
end;

function TdxOrdinalEnglishNumericsProvider.GetHundreds: TArray<string>;
begin
  Result := FHundreds;
end;

function TdxOrdinalEnglishNumericsProvider.GetMillion: TArray<string>;
begin
  Result := FMillion;
end;

function TdxOrdinalEnglishNumericsProvider.GetQuadrillion: TArray<string>;
begin
  Result := FQuadrillion;
end;

function TdxOrdinalEnglishNumericsProvider.GetQuintillion: TArray<string>;
begin
  Result := FQuintillion;
end;

function TdxOrdinalEnglishNumericsProvider.GetSeparator: TArray<string>;
begin
  Result := FSeparator;
end;

function TdxOrdinalEnglishNumericsProvider.GetSingles: TArray<string>;
begin
  Result := FGeneralSingles;
end;

function TdxOrdinalEnglishNumericsProvider.GetSinglesNumeral: TArray<string>;
begin
  Result := FGeneralSingles;
end;

function TdxOrdinalEnglishNumericsProvider.GetTeens: TArray<string>;
begin
  Result := FTeens;
end;

function TdxOrdinalEnglishNumericsProvider.GetTenths: TArray<string>;
begin
  Result := FTenths;
end;

function TdxOrdinalEnglishNumericsProvider.GetThousands: TArray<string>;
begin
  Result := FThousands;
end;

function TdxOrdinalEnglishNumericsProvider.GetTrillion: TArray<string>;
begin
  Result := FTrillion;
end;

class constructor TdxOrdinalEnglishNumericsProvider.Initialize;
begin
  FGeneralSingles := TArray<string>.Create('first', 'second', 'third', 'fourth', 'fifth', 'sixth', 'seventh', 'eighth', 'ninth', 'zeroth');
  FSeparator := TArray<string>.Create(' ', '-');
  FTeens := TArray<string>.Create('tenth', 'eleventh', 'twelfth', 'thirteenth', 'fourteenth', 'fifteenth', 'sixteenth', 'seventeenth', 'eighteenth', 'nineteenth');
  FTenths := TArray<string>.Create('twentieth', 'thirtieth', 'fortieth', 'fiftieth', 'sixtieth', 'seventieth', 'eightieth', 'ninetieth');
  FHundreds := TArray<string>.Create('one hundredth', 'two hundredth', 'three hundredth', 'four hundredth', 'five hundredth', 'six hundredth', 'seven hundredth', 'eight hundredth', 'nine hundredth');
  FThousands := TArray<string>.Create('thousandth');
  FMillion := TArray<string>.Create('millionth');
  FBillion := TArray<string>.Create('billionth');
  FTrillion := TArray<string>.Create('trillionth');
  FQuadrillion := TArray<string>.Create('quadrillionth');
  FQuintillion := TArray<string>.Create('quintillionth');
end;

{ TdxDescriptiveCardinalEnglishNumberConverter }

function TdxDescriptiveCardinalEnglishNumberConverter.GetType: TdxRichEditNumberingFormat;
begin
  Result := TdxRichEditNumberingFormat.CardinalText;
end;

{ TdxDescriptiveOrdinalEnglishNumberConverter }

function TdxDescriptiveOrdinalEnglishNumberConverter.GetType: TdxRichEditNumberingFormat;
begin
  Result := TdxRichEditNumberingFormat.OrdinalText;
end;

procedure TdxDescriptiveOrdinalEnglishNumberConverter.GenerateDigits(ADigits: TdxDigitInfoCollection; AValue: Int64);
begin
  inherited GenerateDigits(ADigits, AValue);
  ADigits.Last.Provider := TdxOrdinalEnglishNumericsProvider.Create;
end;

{ TdxOrdinalEnglishNumberConverter }

class constructor TdxOrdinalEnglishNumberConverter.Initialize;
begin
  FEnding := TArray<string>.Create('st', 'nd', 'rd', 'th');
end;
function TdxOrdinalEnglishNumberConverter.GetType: TdxRichEditNumberingFormat;
begin
  Result := TdxRichEditNumberingFormat.Ordinal;
end;

function TdxOrdinalEnglishNumberConverter.ConvertNumberCore(AValue: Int64): string;
var
  ATemp: Int64;
begin
  ATemp := AValue mod 100;
  if ATemp < 21 then
    case ATemp of
      1:
        Exit(Format('%d%s', [AValue, FEnding[0]]));
      2:
        Exit(Format('%d%s', [AValue, FEnding[1]]));
      3:
        Exit(Format('%d%s', [AValue, FEnding[2]]));
      else
        Exit(Format('%d%s', [AValue, FEnding[3]]));
    end;
  Dec(AValue);
  ATemp := AValue mod 10;
  if ATemp < 3 then
    Exit(Format('%d%s', [AValue + 1, FEnding[ATemp mod 3]]));
  Result := Format('%d%s', [AValue + 1, FEnding[3]]);
end;

end.
