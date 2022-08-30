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

unit dxRichEdit.Utils.FastComparer;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Classes, Generics.Defaults, Generics.Collections, TypInfo;

type

  { TdxFastComparer<T> }

  TdxFastComparerType = (Unknown, Shortint, Smallint, Integer, Int64,
    Single, Double, Extended, Currency);

  TdxFastComparer<T> = class
  strict private
    class function GetComparerType: TdxFastComparerType; static; inline;
  public
    class function IsValueEquals(var AComparerType: TdxFastComparerType; const AValue1, AValue2: T): Boolean; static;
  end;

function CompareInteger(const ALeft, ARight: Integer): Integer; inline;

implementation

{$INLINE ON}

function CompareInteger(const ALeft, ARight: Integer): Integer; inline;
begin
  if ALeft < ARight then
    Result := -1
  else if ALeft > ARight then
    Result := 1
  else
    Result := 0;
end;

{ TdxFastComparer<T> }


function ShortintEqualityComparer(const Value1, Value2: Pointer): Boolean; inline;
var
  AValue1: PShortint absolute Value1;
  AValue2: PShortint absolute Value2;
begin
  Result := AValue1^ = AValue2^;
end;

function SmallintEqualityComparer(const Value1, Value2: Pointer): Boolean; inline;
var
  AValue1: PSmallint absolute Value1;
  AValue2: PSmallint absolute Value2;
begin
  Result := AValue1^ = AValue2^;
end;

function IntegerEqualityComparer(const Value1, Value2: Pointer): Boolean; inline;
var
  AValue1: PInteger absolute Value1;
  AValue2: PInteger absolute Value2;
begin
  Result := AValue1^ = AValue2^;
end;

function Int64EqualityComparer(const Value1, Value2: Pointer): Boolean; inline;
var
  AValue1: PInt64 absolute Value1;
  AValue2: PInt64 absolute Value2;
begin
  Result := AValue1^ = AValue2^;
end;


function SingleEqualityComparer(const Value1, Value2: Pointer): Boolean; inline;
var
  AValue1: PSingle absolute Value1;
  AValue2: PSingle absolute Value2;
begin
  Result := AValue1^ = AValue2^;
end;

function DoubleEqualityComparer(const Value1, Value2: Pointer): Boolean; inline;
var
  AValue1: PDouble absolute Value1;
  AValue2: PDouble absolute Value2;
begin
  Result := AValue1^ = AValue2^;
end;

function ExtendedEqualityComparer(const Value1, Value2: Pointer): Boolean; inline;
var
  AValue1: PExtended absolute Value1;
  AValue2: PExtended absolute Value2;
begin
  Result := AValue1^ = AValue2^;
end;

function CurrencyEqualityComparer(const Value1, Value2: Pointer): Boolean; inline;
var
  AValue1: PCurrency absolute Value1;
  AValue2: PCurrency absolute Value2;
begin
  Result := AValue1^ = AValue2^;
end;


class function TdxFastComparer<T>.GetComparerType: TdxFastComparerType;
var
  ATypeInfo: PTypeInfo;
begin
  Result := TdxFastComparerType.Unknown;
  ATypeInfo := PTypeInfo(TypeInfo(T));
  case ATypeInfo.Kind of
    tkInteger, tkEnumeration:
      case GetTypeData(ATypeInfo)^.OrdType of
        otSByte, otUByte:
          Result := TdxFastComparerType.Shortint;
        otSWord, otUWord:
          Result := TdxFastComparerType.Smallint;
        otSLong, otULong:
          Result := TdxFastComparerType.Integer;
      end;
    tkPointer, tkClass, tkInterface, tkClassRef, tkProcedure:
      Result := TdxFastComparerType.Integer;
    tkInt64:
      Result := TdxFastComparerType.Int64;
    tkFloat:
      case GetTypeData(ATypeInfo)^.FloatType of
        ftSingle:
          Result := TdxFastComparerType.Single;
        ftDouble:
          Result := TdxFastComparerType.Double;
        ftExtended:
          Result := TdxFastComparerType.Extended;
        ftCurr:
          Result := TdxFastComparerType.Currency;
      end;
  end;
end;

class function TdxFastComparer<T>.IsValueEquals(var AComparerType: TdxFastComparerType; const AValue1, AValue2: T): Boolean;
begin
  case AComparerType of
    TdxFastComparerType.Unknown:
    begin
      AComparerType := GetComparerType;
      Assert(AComparerType <> TdxFastComparerType.Unknown);
      Result := IsValueEquals(AComparerType, AValue1, AValue2);
    end;
    TdxFastComparerType.Shortint:
      Result := ShortintEqualityComparer(@AValue1, @AValue2);
    TdxFastComparerType.Smallint:
      Result := SmallintEqualityComparer(@AValue1, @AValue2);
    TdxFastComparerType.Integer:
      Result := IntegerEqualityComparer(@AValue1, @AValue2);
    TdxFastComparerType.Int64:
      Result := Int64EqualityComparer(@AValue1, @AValue2);
    TdxFastComparerType.Single:
      Result := SingleEqualityComparer(@AValue1, @AValue2);
    TdxFastComparerType.Double:
      Result := DoubleEqualityComparer(@AValue1, @AValue2);
    TdxFastComparerType.Extended:
      Result := ExtendedEqualityComparer(@AValue1, @AValue2);
    TdxFastComparerType.Currency:
      Result := CurrencyEqualityComparer(@AValue1, @AValue2);
    else
      Assert(False);
  end;
end;

end.
