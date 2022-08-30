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

unit dxRichEdit.Utils.PredefinedFontSizeCollection;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Classes, Generics.Defaults, Generics.Collections,
  dxRichEdit.Utils.Types,
  dxGenerics;

type
  TdxPredefinedFontSizeCollection = class(TdxIntegerList)
  public const
    MinFontSize = 1;
    MaxFontSize = 1638;
  protected
    procedure CreateDefaultContent; virtual;
    function CalculatePreviousFontSizeCore(AFontSize: Integer): Integer; virtual;
    function CalculateNextFontSizeCore(AFontSize: Integer): Integer; virtual;
    function CalcPrevTen(AValue: Integer): Integer;
    function CalcNextTen(AValue: Integer): Integer;
  public
    constructor Create;
    class function ValidateDoubleFontSize(AValue: Integer): Integer; static;
    class function ValidateFontSize(AValue: Single): Single; static;
    function CalculatePreviousDoubleFontSize(AFontSize: Integer): Integer;
    function CalculateNextDoubleFontSize(AFontSize: Integer): Integer;
  end;

implementation

uses
  Math, RTLConsts, Contnrs, cxGraphics;

constructor TdxPredefinedFontSizeCollection.Create;
begin
  inherited Create;
  CreateDefaultContent;
end;

class function TdxPredefinedFontSizeCollection.ValidateDoubleFontSize(AValue: Integer): Integer;
begin
  Result := Max(Min(AValue, MaxFontSize * 2), MinFontSize * 2);
end;

class function TdxPredefinedFontSizeCollection.ValidateFontSize(AValue: Single): Single;
begin
  Result := ValidateDoubleFontSize(Round(AValue * 2.0)) / 2.0;
end;

procedure TdxPredefinedFontSizeCollection.CreateDefaultContent;
var
  I: Integer;
begin
  for I := 0 to dxDefaultFontSizeCount - 1 do
    Add(dxDefaultFontSizes[I]);
end;

function TdxPredefinedFontSizeCollection.CalculateNextDoubleFontSize(AFontSize: Integer): Integer;
begin
  Result := ValidateDoubleFontSize(CalculateNextFontSizeCore(AFontSize div 2) * 2);
end;

function TdxPredefinedFontSizeCollection.CalculateNextFontSizeCore(AFontSize: Integer): Integer;
var
  AFontSizeIndex: Integer;
begin
  if Count = 0 then
    Exit(AFontSize + 1);

  if AFontSize < First then
    Exit(AFontSize + 1);

  if BinarySearch(AFontSize, AFontSizeIndex) then
    Inc(AFontSizeIndex);

  if AFontSizeIndex < Count then
    Result := Items[AFontSizeIndex]
  else
    Result := CalcNextTen(AFontSize);
end;

function TdxPredefinedFontSizeCollection.CalculatePreviousDoubleFontSize(AFontSize: Integer): Integer;
begin
  Result := ValidateDoubleFontSize(CalculatePreviousFontSizeCore(AFontSize div 2) * 2);
end;

function TdxPredefinedFontSizeCollection.CalculatePreviousFontSizeCore(AFontSize: Integer): Integer;
var
  AFontSizeIndex: Integer;
begin
  if Count = 0 then
    Exit(AFontSize - 1);

  if AFontSize <= First then
    Exit(AFontSize - 1);

  if BinarySearch(AFontSize, AFontSizeIndex) then
    Exit(Items[AFontSizeIndex - 1]);

  if AFontSizeIndex <> Count then
    Exit(Items[AFontSizeIndex - 1]);

  if AFontSize > CalcNextTen(Last) then
    Result := CalcPrevTen(AFontSize)
  else
    Result := Items[AFontSizeIndex - 1];
end;

function TdxPredefinedFontSizeCollection.CalcNextTen(AValue: Integer): Integer;
begin
  Result := AValue + (10 - (AValue mod 10));
end;

function TdxPredefinedFontSizeCollection.CalcPrevTen(AValue: Integer): Integer;
begin
  if (AValue mod 10) <> 0 then
    Result := AValue - (AValue mod 10)
  else
    Result := AValue - 10;
end;

end.
