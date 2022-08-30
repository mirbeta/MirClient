{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressEntityMapping Framework                           }
{                                                                    }
{           Copyright (c) 2016-2019 Developer Express Inc.           }
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
{   (DCU, OBJ, DLL, DPU, SO, ETC.) ARE CONFIDENTIAL AND PROPRIETARY  }
{   TRADE SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER }
{   IS LICENSED TO DISTRIBUTE THE EXPRESSENTITYMAPPING FRAMEWORK     }
{   AS PART OF AN EXECUTABLE PROGRAM ONLY.                           }
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

unit dxEMF.Linq.Expressions;

interface

{$I cxVer.inc}
{$I dxEMF.inc}

uses
  SysUtils, TypInfo, Generics.Defaults, Generics.Collections, Classes,
  dxEMF.Types,
  dxEMF.DB.Criteria;

type
  { TdxLinqExpression }

  TdxLinqExpression = record
  private type

    IOperatorHolder = interface
    ['{9C6FEF9F-33D8-4497-9641-8DFC1B820358}']
//      function Extract: TdxCriteriaOperator;
    end;

    TOperatorHolder = class(TInterfacedObject, IdxExpression, IOperatorHolder)
    public
      constructor Create(AOperator: TdxCriteriaOperator); overload;
      destructor Destroy; override;
    end;

  private
    FExpression: IdxExpression;
    constructor Create(const AOperator: IdxCriteriaOperator); overload;
    constructor Create(const APropertyName: string); overload;

    function GetOperator: IdxCriteriaOperator;
    class function ToArray(const A: array of TdxLinqExpression): TArray<IdxCriteriaOperator>; overload; static;
  public
    class operator Implicit(AOperator: TdxCriteriaOperator): TdxLinqExpression;
    class operator Implicit(const A: TdxLinqExpression): IdxCriteriaOperator;
    class operator Explicit(const A: TdxLinqExpression): IdxCriteriaOperator;
    class operator Implicit(const A: TdxLinqExpression): IdxExpression;
    class operator Implicit(const A: IdxExpression): TdxLinqExpression;
    class operator Implicit(A: Byte): TdxLinqExpression;
    class operator Implicit(A: Integer): TdxLinqExpression;
    class operator Implicit(A: Int64): TdxLinqExpression;
    class operator Implicit(A: Single): TdxLinqExpression;
    class operator Implicit(A: Double): TdxLinqExpression;
    class operator Implicit(A: Extended): TdxLinqExpression;
    class operator Implicit(A: TDateTime): TdxLinqExpression;
    class operator Implicit(A: Char): TdxLinqExpression;
    class operator Implicit(const A: string): TdxLinqExpression;

    class operator Explicit(const A: string): TdxLinqExpression;
    class operator Implicit(A: pointer): TdxLinqExpression;
    // "="
    class operator Equal(const A, B: TdxLinqExpression): TdxLinqExpression;
    class operator Equal(const A: TdxLinqExpression; const B: TGUID): TdxLinqExpression;
    class operator Equal(const A: TGUID; const B: TdxLinqExpression): TdxLinqExpression;
    // "<>"
    class operator NotEqual(const A, B: TdxLinqExpression): TdxLinqExpression;
    class operator NotEqual(const A: TdxLinqExpression; const B: TGUID): TdxLinqExpression;
    class operator NotEqual(const A: TGUID; const B: TdxLinqExpression): TdxLinqExpression;
    // ">"
    class operator GreaterThan(const A, B: TdxLinqExpression): TdxLinqExpression;
    // ">="
    class operator GreaterThanOrEqual(const A, B: TdxLinqExpression): TdxLinqExpression;
    // "<"
    class operator LessThan(const A, B: TdxLinqExpression): TdxLinqExpression;
    // "<="
    class operator LessThanOrEqual(const A, B: TdxLinqExpression): TdxLinqExpression;
    // "and"
    class operator LogicalAnd(const A, B: TdxLinqExpression): TdxLinqExpression;
    // "or"
    class operator LogicalOr(const A, B: TdxLinqExpression): TdxLinqExpression;
    // "not"
    class operator LogicalNot(const A: TdxLinqExpression): TdxLinqExpression;
    // "+"
    class operator Add(const A, B: TdxLinqExpression): TdxLinqExpression;
    // "-"
    class operator Subtract(const A, B: TdxLinqExpression): TdxLinqExpression;
    // "*"
    class operator Multiply(const A, B: TdxLinqExpression): TdxLinqExpression;
    // "/"
    class operator Divide(const A, B: TdxLinqExpression): TdxLinqExpression;
    // "mod"
    class operator Modulus(const A, B: TdxLinqExpression): TdxLinqExpression;
    // "+"
    class operator Positive(const A: TdxLinqExpression): TdxLinqExpression;
    // "-"
    class operator Negative(const A: TdxLinqExpression): TdxLinqExpression;
    // "in"
    {$IFDEF DELPHIXE3}
    class operator In(const A: TdxLinqExpression; const B: array of TdxLinqExpression): TdxLinqExpression;
    {$ENDIF}
    function &In<T>(const B: TArray<T>): TdxLinqExpression;
    // "between"
    function Between(const ABeginExpression, AEndExpression: TdxLinqExpression): TdxLinqExpression;
    // "Is Null"
    function IsNull: TdxLinqExpression;
  end;

function IfThen(const AValue, ATrue, AFalse: TdxLinqExpression): TdxLinqExpression; overload;
function Iif(const AValue, ATrue, AFalse: TdxLinqExpression): TdxLinqExpression; overload; inline;
// Mathematical functions
function Abs(const X: TdxLinqExpression): TdxLinqExpression; overload;
function Sqr(const X: TdxLinqExpression): TdxLinqExpression; overload;
function Random(const X: TdxLinqExpression): TdxLinqExpression; overload;
function Log(const X: TdxLinqExpression): TdxLinqExpression; overload;
function Log(const X, Y: TdxLinqExpression): TdxLinqExpression; overload;
function BigMul(const X, Y: TdxLinqExpression): TdxLinqExpression; overload;
function Log10(const X: TdxLinqExpression): TdxLinqExpression; overload;
function Sin(const X: TdxLinqExpression): TdxLinqExpression; overload;
function Tan(const X: TdxLinqExpression): TdxLinqExpression; overload;
function ArcTan(const X: TdxLinqExpression): TdxLinqExpression; overload;
function ArcTan2(const X, Y: TdxLinqExpression): TdxLinqExpression; overload;
function Cos(const X: TdxLinqExpression): TdxLinqExpression; overload;
function ArcCos(const X: TdxLinqExpression): TdxLinqExpression; overload;
function ArcSin(const X: TdxLinqExpression): TdxLinqExpression; overload;
function Cosh(const X: TdxLinqExpression): TdxLinqExpression; overload;
function Sinh(const X: TdxLinqExpression): TdxLinqExpression; overload;
function Tanh(const X: TdxLinqExpression): TdxLinqExpression; overload;
function Exp(const X: TdxLinqExpression): TdxLinqExpression; overload;
function Power(const ABase, AExponent: TdxLinqExpression): TdxLinqExpression; overload;
function Round(const X: TdxLinqExpression): TdxLinqExpression; overload;
function Round(const AValue, ADigit: TdxLinqExpression): TdxLinqExpression; overload;
function Sign(const X: TdxLinqExpression): TdxLinqExpression; overload;
function Floor(const X: TdxLinqExpression): TdxLinqExpression; overload;
function Ceil(const X: TdxLinqExpression): TdxLinqExpression; overload;
function Max(const A, B: TdxLinqExpression): TdxLinqExpression; overload;
function Min(const A, B: TdxLinqExpression): TdxLinqExpression; overload;
function Ascii(const X: TdxLinqExpression): TdxLinqExpression; overload;
// Date/Time functions
function MillisecondOf(const AValue: TdxLinqExpression): TdxLinqExpression; overload;
function SecondOf(const AValue: TdxLinqExpression): TdxLinqExpression; overload;
function MinuteOf(const AValue: TdxLinqExpression): TdxLinqExpression; overload;
function HourOf(const AValue: TdxLinqExpression): TdxLinqExpression; overload;
function DayOf(const AValue: TdxLinqExpression): TdxLinqExpression; overload;
function MonthOf(const AValue: TdxLinqExpression): TdxLinqExpression; overload;
function YearOf(const AValue: TdxLinqExpression): TdxLinqExpression; overload;
function GetTimeOfDay(const AValue: TdxLinqExpression): TdxLinqExpression; overload;
function DayOfTheWeek(const AValue: TdxLinqExpression): TdxLinqExpression; overload;
function DayOfTheYear(const AValue: TdxLinqExpression): TdxLinqExpression; overload;
function DateOf(const AValue: TdxLinqExpression): TdxLinqExpression; overload;
function IncTick(const AValue, ANumberOfTicks: TdxLinqExpression): TdxLinqExpression; overload;
function IncMillisecond(const AValue, ANumberOfMilliseconds: TdxLinqExpression): TdxLinqExpression; overload;
function IncSecond(const AValue, ANumberOfSeconds: TdxLinqExpression): TdxLinqExpression; overload;
function IncMinute(const AValue, ANumberOfSeconds: TdxLinqExpression): TdxLinqExpression; overload;
function IncHour(const AValue, ANumberOfHours: TdxLinqExpression): TdxLinqExpression; overload;
function IncDay(const AValue, ANumberOfDays: TdxLinqExpression): TdxLinqExpression; overload;
function IncMonth(const AValue, ANumberOfMonths: TdxLinqExpression): TdxLinqExpression; overload;
function IncYear(const AValue, ANumberOfYears: TdxLinqExpression): TdxLinqExpression; overload;
function MillisecondsBetween(const ANow, AThen: TdxLinqExpression): TdxLinqExpression; overload;
function SecondsBetween(const ANow, AThen: TdxLinqExpression): TdxLinqExpression; overload;
function MinutesBetween(const ANow, AThen: TdxLinqExpression): TdxLinqExpression; overload;
function HoursBetween(const ANow, AThen: TdxLinqExpression): TdxLinqExpression; overload;
function DaysBetween(const ANow, AThen: TdxLinqExpression): TdxLinqExpression; overload;
function MonthsBetween(const ANow, AThen: TdxLinqExpression): TdxLinqExpression; overload;
function YearsBetween(const ANow, AThen: TdxLinqExpression): TdxLinqExpression; overload;
function Now: TdxLinqExpression; overload;
function UtcNow: TdxLinqExpression; overload;
function Today: TdxLinqExpression; overload;
// String functions
function Len(const AValue: TdxLinqExpression): TdxLinqExpression; overload;
function LowerCase(const AValue: TdxLinqExpression): TdxLinqExpression; overload;
function UpperCase(const AValue: TdxLinqExpression): TdxLinqExpression; overload;
function Concat(const AValues: array of TdxLinqExpression): TdxLinqExpression; overload;
function Replace(const AValue, AOldValue, ANewValue: TdxLinqExpression): TdxLinqExpression; overload;
function Reverse(const AValue: TdxLinqExpression): TdxLinqExpression; overload;
function Insert(const AValue, AIndex, ASubValue: TdxLinqExpression): TdxLinqExpression; overload;
function Remove(const AValue, AStartIndex: TdxLinqExpression): TdxLinqExpression; overload;
function Remove(const AValue, AStartIndex, ACount: TdxLinqExpression): TdxLinqExpression; overload;
function CharIndex(const ASubStr, AValue: TdxLinqExpression): TdxLinqExpression; overload;
function CharIndex(const ASubStr, AValue, AOffset: TdxLinqExpression): TdxLinqExpression; overload;
function CharIndex(const ASubStr, AValue, AOffset, ACount: TdxLinqExpression): TdxLinqExpression; overload;
function Pos(const ASubStr, AValue: TdxLinqExpression): TdxLinqExpression; overload; inline;
function Pos(const ASubStr, AValue, AOffset: TdxLinqExpression): TdxLinqExpression; overload; inline;
function Pos(const ASubStr, AValue, AOffset, ACount: TdxLinqExpression): TdxLinqExpression; overload; inline;
function PadLeft(const AValue, ATotalWidth: TdxLinqExpression): TdxLinqExpression; overload;
function PadLeft(const AValue, ATotalWidth, APaddingChar: TdxLinqExpression): TdxLinqExpression; overload;
function PadRight(const AValue, ATotalWidth: TdxLinqExpression): TdxLinqExpression; overload;
function PadRight(const AValue, ATotalWidth, APaddingChar: TdxLinqExpression): TdxLinqExpression; overload;
function StartsWith(const AValue, ASubStr: TdxLinqExpression): TdxLinqExpression; overload;
function EndsWith(const AValue, ASubStr: TdxLinqExpression): TdxLinqExpression; overload;
function Contains(const AValue, ASubStr: TdxLinqExpression): TdxLinqExpression; overload;
function Substring(const AValue, AStartIndex: TdxLinqExpression): TdxLinqExpression; overload;
function Substring(const AValue, AStartIndex, ALength: TdxLinqExpression): TdxLinqExpression; overload;
function Trim(const AValue: TdxLinqExpression): TdxLinqExpression; overload;
function IsNullOrEmpty(const AValue: TdxLinqExpression): TdxLinqExpression; overload;
// Conversion functions
function ToChar(const AValue: TdxLinqExpression): TdxLinqExpression;
function ToInteger(const AValue: TdxLinqExpression): TdxLinqExpression;
function ToInt64(const AValue: TdxLinqExpression): TdxLinqExpression;
function ToSingle(const AValue: TdxLinqExpression): TdxLinqExpression;
function ToDouble(const AValue: TdxLinqExpression): TdxLinqExpression;
function ToDecimal(const AValue: TdxLinqExpression): TdxLinqExpression;
function ToStr(const AValue: TdxLinqExpression): TdxLinqExpression;

implementation

uses
  Rtti, Variants,
  dxCore,
  dxEMF.Strs,
  dxEMF.Utils;

function IfThen(const AValue, ATrue, AFalse: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxFunctionOperator.Create(TdxFunctionOperatorType.Iif,
    [AValue.GetOperator, ATrue.GetOperator, AFalse.GetOperator]);
end;

function Iif(const AValue, ATrue, AFalse: TdxLinqExpression): TdxLinqExpression;
begin
  Result := IfThen(AValue, ATrue, AFalse);
end;

function Abs(const X: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxFunctionOperator.Create(TdxFunctionOperatorType.Abs, [X.GetOperator]);
end;

function Sqr(const X: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxFunctionOperator.Create(TdxFunctionOperatorType.Sqr, [X.GetOperator]);
end;

function Random(const X: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxFunctionOperator.Create(TdxFunctionOperatorType.Random, [X.GetOperator]);
end;

function Log(const X: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxFunctionOperator.Create(TdxFunctionOperatorType.Log, [X.GetOperator]);
end;

function Log(const X, Y: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxFunctionOperator.Create(TdxFunctionOperatorType.Log, [X.GetOperator, Y.GetOperator]);
end;

function BigMul(const X, Y: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxFunctionOperator.Create(TdxFunctionOperatorType.BigMul, [X.GetOperator, Y.GetOperator]);
end;

function Log10(const X: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxFunctionOperator.Create(TdxFunctionOperatorType.Log10, [X.GetOperator]);
end;

function Sin(const X: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxFunctionOperator.Create(TdxFunctionOperatorType.Sin, [X.GetOperator]);
end;

function Tan(const X: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxFunctionOperator.Create(TdxFunctionOperatorType.Tan, [X.GetOperator]);
end;

function ArcTan(const X: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxFunctionOperator.Create(TdxFunctionOperatorType.ArcTan, [X.GetOperator]);
end;

function ArcTan2(const X, Y: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxFunctionOperator.Create(TdxFunctionOperatorType.ArcTan2, [X.GetOperator, Y.GetOperator]);
end;

function Cos(const X: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxFunctionOperator.Create(TdxFunctionOperatorType.Cos, [X.GetOperator]);
end;

function ArcCos(const X: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxFunctionOperator.Create(TdxFunctionOperatorType.ArcCos, [X.GetOperator]);
end;

function ArcSin(const X: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxFunctionOperator.Create(TdxFunctionOperatorType.ArcSin, [X.GetOperator]);
end;

function Cosh(const X: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxFunctionOperator.Create(TdxFunctionOperatorType.Cosh, [X.GetOperator]);
end;

function Sinh(const X: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxFunctionOperator.Create(TdxFunctionOperatorType.Sinh, [X.GetOperator]);
end;

function Tanh(const X: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxFunctionOperator.Create(TdxFunctionOperatorType.Tanh, [X.GetOperator]);
end;

function Exp(const X: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxFunctionOperator.Create(TdxFunctionOperatorType.Exp, [X.GetOperator]);
end;

function Power(const ABase, AExponent: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxFunctionOperator.Create(TdxFunctionOperatorType.Power, [ABase.GetOperator, AExponent.GetOperator]);
end;

function Round(const X: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxFunctionOperator.Create(TdxFunctionOperatorType.Round, [X.GetOperator]);
end;

function Round(const AValue, ADigit: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxFunctionOperator.Create(TdxFunctionOperatorType.Round, [AValue.GetOperator, ADigit.GetOperator]);
end;

function Sign(const X: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxFunctionOperator.Create(TdxFunctionOperatorType.Sign, [X.GetOperator]);
end;

function Floor(const X: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxFunctionOperator.Create(TdxFunctionOperatorType.Floor, [X.GetOperator]);
end;

function Ceil(const X: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxFunctionOperator.Create(TdxFunctionOperatorType.Ceil, [X.GetOperator]);
end;

function Max(const A, B: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxFunctionOperator.Create(TdxFunctionOperatorType.Max, [A.GetOperator, B.GetOperator]);
end;

function Min(const A, B: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxFunctionOperator.Create(TdxFunctionOperatorType.Min, [A.GetOperator, B.GetOperator]);
end;

function Ascii(const X: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxFunctionOperator.Create(TdxFunctionOperatorType.Ascii, [X.GetOperator]);
end;

function MillisecondOf(const AValue: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxFunctionOperator.Create(TdxFunctionOperatorType.MillisecondOf, [AValue.GetOperator]);
end;

function SecondOf(const AValue: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxFunctionOperator.Create(TdxFunctionOperatorType.SecondOf, [AValue.GetOperator]);
end;

function MinuteOf(const AValue: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxFunctionOperator.Create(TdxFunctionOperatorType.MinuteOf, [AValue.GetOperator]);
end;

function HourOf(const AValue: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxFunctionOperator.Create(TdxFunctionOperatorType.HourOf, [AValue.GetOperator]);
end;

function DayOf(const AValue: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxFunctionOperator.Create(TdxFunctionOperatorType.DayOf, [AValue.GetOperator]);
end;

function MonthOf(const AValue: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxFunctionOperator.Create(TdxFunctionOperatorType.MonthOf, [AValue.GetOperator]);
end;

function YearOf(const AValue: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxFunctionOperator.Create(TdxFunctionOperatorType.YearOf, [AValue.GetOperator]);
end;

function GetTimeOfDay(const AValue: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxFunctionOperator.Create(TdxFunctionOperatorType.GetTimeOfDay, [AValue.GetOperator]);
end;

function DayOfTheWeek(const AValue: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxFunctionOperator.Create(TdxFunctionOperatorType.DayOfTheWeek, [AValue.GetOperator]);
end;

function DayOfTheYear(const AValue: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxFunctionOperator.Create(TdxFunctionOperatorType.DayOfTheYear, [AValue.GetOperator]);
end;

function DateOf(const AValue: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxFunctionOperator.Create(TdxFunctionOperatorType.DateOf, [AValue.GetOperator]);
end;

function IncTick(const AValue, ANumberOfTicks: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxFunctionOperator.Create(TdxFunctionOperatorType.IncTick,
    [AValue.GetOperator, ANumberOfTicks.GetOperator]);
end;

function IncMillisecond(const AValue, ANumberOfMilliseconds: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxFunctionOperator.Create(TdxFunctionOperatorType.IncMillisecond,
    [AValue.GetOperator, ANumberOfMilliseconds.GetOperator]);
end;

function IncSecond(const AValue, ANumberOfSeconds: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxFunctionOperator.Create(TdxFunctionOperatorType.IncSecond,
    [AValue.GetOperator, ANumberOfSeconds.GetOperator]);
end;

function IncMinute(const AValue, ANumberOfSeconds: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxFunctionOperator.Create(TdxFunctionOperatorType.IncMinute,
    [AValue.GetOperator, ANumberOfSeconds.GetOperator]);
end;

function IncHour(const AValue, ANumberOfHours: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxFunctionOperator.Create(TdxFunctionOperatorType.IncHour,
    [AValue.GetOperator, ANumberOfHours.GetOperator]);
end;

function IncDay(const AValue, ANumberOfDays: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxFunctionOperator.Create(TdxFunctionOperatorType.IncDay,
    [AValue.GetOperator, ANumberOfDays.GetOperator]);
end;

function IncMonth(const AValue, ANumberOfMonths: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxFunctionOperator.Create(TdxFunctionOperatorType.IncMonth,
    [AValue.GetOperator, ANumberOfMonths.GetOperator]);
end;

function IncYear(const AValue, ANumberOfYears: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxFunctionOperator.Create(TdxFunctionOperatorType.IncYear,
    [AValue.GetOperator, ANumberOfYears.GetOperator]);
end;

function MillisecondsBetween(const ANow, AThen: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxFunctionOperator.Create(TdxFunctionOperatorType.MillisecondsBetween,
    [ANow.GetOperator, AThen.GetOperator]);
end;

function SecondsBetween(const ANow, AThen: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxFunctionOperator.Create(TdxFunctionOperatorType.SecondsBetween,
    [ANow.GetOperator, AThen.GetOperator]);
end;

function MinutesBetween(const ANow, AThen: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxFunctionOperator.Create(TdxFunctionOperatorType.MinutesBetween,
    [ANow.GetOperator, AThen.GetOperator]);
end;

function HoursBetween(const ANow, AThen: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxFunctionOperator.Create(TdxFunctionOperatorType.HoursBetween,
    [ANow.GetOperator, AThen.GetOperator]);
end;

function DaysBetween(const ANow, AThen: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxFunctionOperator.Create(TdxFunctionOperatorType.DaysBetween,
    [ANow.GetOperator, AThen.GetOperator]);
end;

function MonthsBetween(const ANow, AThen: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxFunctionOperator.Create(TdxFunctionOperatorType.MonthsBetween,
    [ANow.GetOperator, AThen.GetOperator]);
end;

function YearsBetween(const ANow, AThen: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxFunctionOperator.Create(TdxFunctionOperatorType.YearsBetween,
    [ANow.GetOperator, AThen.GetOperator]);
end;

function Now: TdxLinqExpression;
begin
  Result := TdxFunctionOperator.Create(TdxFunctionOperatorType.Now, []);
end;

function UtcNow: TdxLinqExpression;
begin
  Result := TdxFunctionOperator.Create(TdxFunctionOperatorType.UtcNow, []);
end;

function Today: TdxLinqExpression;
begin
  Result := TdxFunctionOperator.Create(TdxFunctionOperatorType.Today, []);
end;

function Len(const AValue: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxFunctionOperator.Create(TdxFunctionOperatorType.Len, [AValue.GetOperator]);
end;

function LowerCase(const AValue: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxFunctionOperator.Create(TdxFunctionOperatorType.LowerCase, [AValue.GetOperator]);
end;

function UpperCase(const AValue: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxFunctionOperator.Create(TdxFunctionOperatorType.UpperCase, [AValue.GetOperator]);
end;

function Concat(const AValues: array of TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxFunctionOperator.Create(TdxFunctionOperatorType.Concat, TdxLinqExpression.ToArray(AValues));
end;

function Replace(const AValue, AOldValue, ANewValue: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxFunctionOperator.Create(TdxFunctionOperatorType.Replace,
    [AValue.GetOperator, AOldValue.GetOperator, ANewValue.GetOperator]);
end;

function Reverse(const AValue: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxFunctionOperator.Create(TdxFunctionOperatorType.Reverse, [AValue.GetOperator]);
end;

function Insert(const AValue, AIndex, ASubValue: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxFunctionOperator.Create(TdxFunctionOperatorType.Insert,
    [AValue.GetOperator, AIndex.GetOperator, ASubValue.GetOperator]);
end;

function Remove(const AValue, AStartIndex: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxFunctionOperator.Create(TdxFunctionOperatorType.Remove,
    [AValue.GetOperator, AStartIndex.GetOperator]);
end;

function Remove(const AValue, AStartIndex, ACount: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxFunctionOperator.Create(TdxFunctionOperatorType.Remove,
    [AValue.GetOperator, AStartIndex.GetOperator, ACount.GetOperator]);
end;

function CharIndex(const ASubStr, AValue: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxFunctionOperator.Create(TdxFunctionOperatorType.CharIndex,
    [ASubStr.GetOperator, AValue.GetOperator]);
end;

function CharIndex(const ASubStr, AValue, AOffset: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxFunctionOperator.Create(TdxFunctionOperatorType.CharIndex,
    [ASubStr.GetOperator, AValue.GetOperator, AOffset.GetOperator]);
end;

function CharIndex(const ASubStr, AValue, AOffset, ACount: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxFunctionOperator.Create(TdxFunctionOperatorType.CharIndex,
    [ASubStr.GetOperator, AValue.GetOperator, AOffset.GetOperator, ACount.GetOperator]);
end;

function Pos(const ASubStr, AValue: TdxLinqExpression): TdxLinqExpression;
begin
  Result := CharIndex(ASubStr, AValue);
end;

function Pos(const ASubStr, AValue, AOffset: TdxLinqExpression): TdxLinqExpression;
begin
  Result := CharIndex(ASubStr, AValue, AOffset);
end;

function Pos(const ASubStr, AValue, AOffset, ACount: TdxLinqExpression): TdxLinqExpression;
begin
  Result := CharIndex(ASubStr, AValue, AOffset, ACount);
end;

function PadLeft(const AValue, ATotalWidth: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxFunctionOperator.Create(TdxFunctionOperatorType.PadLeft,
    [AValue.GetOperator, ATotalWidth.GetOperator]);
end;

function PadLeft(const AValue, ATotalWidth, APaddingChar: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxFunctionOperator.Create(TdxFunctionOperatorType.PadLeft,
    [AValue.GetOperator, ATotalWidth.GetOperator, APaddingChar.GetOperator]);
end;

function PadRight(const AValue, ATotalWidth: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxFunctionOperator.Create(TdxFunctionOperatorType.PadRight,
    [AValue.GetOperator, ATotalWidth.GetOperator]);
end;

function PadRight(const AValue, ATotalWidth, APaddingChar: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxFunctionOperator.Create(TdxFunctionOperatorType.PadRight,
    [AValue.GetOperator, ATotalWidth.GetOperator, APaddingChar.GetOperator]);
end;

function StartsWith(const AValue, ASubStr: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxFunctionOperator.Create(TdxFunctionOperatorType.StartsWith,
    [AValue.GetOperator, ASubStr.GetOperator]);
end;

function EndsWith(const AValue, ASubStr: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxFunctionOperator.Create(TdxFunctionOperatorType.EndsWith,
    [AValue.GetOperator, ASubStr.GetOperator]);
end;

function Contains(const AValue, ASubStr: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxFunctionOperator.Create(TdxFunctionOperatorType.Contains,
    [AValue.GetOperator, ASubStr.GetOperator]);
end;

function Substring(const AValue, AStartIndex: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxFunctionOperator.Create(TdxFunctionOperatorType.Substring,
    [AValue.GetOperator, AStartIndex.GetOperator]);
end;

function Substring(const AValue, AStartIndex, ALength: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxFunctionOperator.Create(TdxFunctionOperatorType.Substring,
    [AValue.GetOperator, AStartIndex.GetOperator, ALength.GetOperator]);
end;

function Trim(const AValue: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxFunctionOperator.Create(TdxFunctionOperatorType.Trim, [AValue.GetOperator]);
end;

function IsNullOrEmpty(const AValue: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxFunctionOperator.Create(TdxFunctionOperatorType.IsNullOrEmpty, [AValue.GetOperator]);
end;

function ToChar(const AValue: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxFunctionOperator.Create(TdxFunctionOperatorType.Char, [AValue.GetOperator]);
end;

function ToDecimal(const AValue: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxFunctionOperator.Create(TdxFunctionOperatorType.ToDecimal, [AValue.GetOperator]);
end;

function ToDouble(const AValue: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxFunctionOperator.Create(TdxFunctionOperatorType.ToDouble, [AValue.GetOperator]);
end;

function ToInt64(const AValue: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxFunctionOperator.Create(TdxFunctionOperatorType.ToInt64, [AValue.GetOperator]);
end;

function ToInteger(const AValue: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxFunctionOperator.Create(TdxFunctionOperatorType.ToInteger, [AValue.GetOperator]);
end;

function ToSingle(const AValue: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxFunctionOperator.Create(TdxFunctionOperatorType.ToSingle, [AValue.GetOperator]);
end;

function ToStr(const AValue: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxFunctionOperator.Create(TdxFunctionOperatorType.ToString, [AValue.GetOperator]);
end;

{ TdxLinqExpression.TFreeTheValue }

constructor TdxLinqExpression.TOperatorHolder.Create(AOperator: TdxCriteriaOperator);
begin
  inherited Create;
//  FOperator := AOperator;
end;


destructor TdxLinqExpression.TOperatorHolder.Destroy;
begin
//  FOperator.Release;
  inherited;
end;


//function TdxLinqExpression.TOperatorHolder.Extract: TdxCriteriaOperator;
//begin
//  Result := TdxCriteriaOperator(FOperator);
//  FOperator := nil;
//end;
//
//function TdxLinqExpression.TOperatorHolder.GetCriteriaOperator: TdxCriteriaOperator;
//begin
//  Result := FOperator;
//end;

{ TdxLinqExpression }

constructor TdxLinqExpression.Create(const APropertyName: string);
begin
  FExpression := TdxOperandProperty.Create(APropertyName);
end;


constructor TdxLinqExpression.Create(const AOperator: IdxCriteriaOperator);
begin
//  FExpression := TOperatorHolder.Create(AOperator);
  FExpression := AOperator;
end;

class operator TdxLinqExpression.Implicit(A: Int64): TdxLinqExpression;
begin
  Result := TdxLinqExpression.Create(TdxOperandValue.Create(A));
end;

class operator TdxLinqExpression.Implicit(A: Double): TdxLinqExpression;
begin
  Result := TdxLinqExpression.Create(TdxOperandValue.Create(A));
end;

class operator TdxLinqExpression.Implicit(const A: string): TdxLinqExpression;
begin
  Result := TdxLinqExpression.Create(TdxOperandValue.Create(A));
end;

class operator TdxLinqExpression.Explicit(const A: string): TdxLinqExpression;
begin
  Result := TdxLinqExpression.Create(A);
end;

class operator TdxLinqExpression.Implicit(A: Single): TdxLinqExpression;
begin
  Result := TdxLinqExpression.Create(TdxOperandValue.Create(A));
end;

class operator TdxLinqExpression.Implicit(A: Char): TdxLinqExpression;
begin
  Result := TdxLinqExpression.Create(TdxOperandValue.Create(A));
end;

class operator TdxLinqExpression.Implicit(A: pointer): TdxLinqExpression;
begin
  if A <> nil then
    raise EInvalidOperation.Create('nil expected');
  Result.FExpression := nil;
end;

class operator TdxLinqExpression.Implicit(const A: IdxExpression): TdxLinqExpression;
begin
  Result.FExpression := A;
end;

class operator TdxLinqExpression.Implicit(const A: TdxLinqExpression): IdxExpression;
begin
  Result := A.FExpression
end;

class operator TdxLinqExpression.Implicit(const A: TdxLinqExpression): IdxCriteriaOperator;
begin
  Result := A.GetOperator;
end;


class operator TdxLinqExpression.Equal(const A: TdxLinqExpression; const B: TGUID): TdxLinqExpression;
begin
  Result := TdxBinaryOperator.Create(A.GetOperator, TdxOperandValue.Create(B), TdxBinaryOperatorType.Equal);
end;

class operator TdxLinqExpression.Equal(const A, B: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxBinaryOperator.Create(A.GetOperator, B.GetOperator, TdxBinaryOperatorType.Equal);
end;

class operator TdxLinqExpression.Equal(const A: TGUID; const B: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxBinaryOperator.Create(TdxOperandValue.Create(A), B.GetOperator, TdxBinaryOperatorType.Equal);
end;

class operator TdxLinqExpression.Explicit(const A: TdxLinqExpression): IdxCriteriaOperator;
begin
  Result := A.GetOperator;
end;

function TdxLinqExpression.GetOperator: IdxCriteriaOperator;
begin
  if FExpression = nil then
    Result := nil
  else
    Result := FExpression as IdxCriteriaOperator;
end;

class operator TdxLinqExpression.GreaterThan(const A, B: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxBinaryOperator.Create(A.GetOperator, B.GetOperator, TdxBinaryOperatorType.Greater);
end;

class operator TdxLinqExpression.GreaterThanOrEqual(const A, B: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxBinaryOperator.Create(A.GetOperator, B.GetOperator, TdxBinaryOperatorType.GreaterOrEqual);
end;

class operator TdxLinqExpression.Implicit(A: Extended): TdxLinqExpression;
begin
  Result := TdxLinqExpression.Create(TdxOperandValue.Create(A));
end;

class operator TdxLinqExpression.Implicit(A: TDateTime): TdxLinqExpression;
begin
  Result := TdxLinqExpression.Create(TdxOperandValue.Create(A));
end;

class operator TdxLinqExpression.Implicit(A: Byte): TdxLinqExpression;
begin
  Result := TdxLinqExpression.Create(TdxOperandValue.Create(A));
end;

class operator TdxLinqExpression.Implicit(A: Integer): TdxLinqExpression;
begin
  Result := TdxLinqExpression.Create(TdxOperandValue.Create(A));
end;

class operator TdxLinqExpression.Implicit(AOperator: TdxCriteriaOperator): TdxLinqExpression;
begin
  Result := TdxLinqExpression.Create(AOperator);
end;

function TdxLinqExpression.Between(const ABeginExpression, AEndExpression: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxBetweenOperator.Create(Self.GetOperator, ABeginExpression.GetOperator, AEndExpression.GetOperator);
end;

class operator TdxLinqExpression.Divide(const A, B: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxBinaryOperator.Create(A.GetOperator, B.GetOperator, TdxBinaryOperatorType.Divide);
end;

class operator TdxLinqExpression.LessThan(const A, B: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxBinaryOperator.Create(A.GetOperator, B.GetOperator, TdxBinaryOperatorType.Less);
end;

class operator TdxLinqExpression.LessThanOrEqual(const A, B: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxBinaryOperator.Create(A.GetOperator, B.GetOperator, TdxBinaryOperatorType.LessOrEqual);
end;

class operator TdxLinqExpression.LogicalAnd(const A, B: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxGroupOperator.Combine(TdxGroupOperatorType.&And, A.GetOperator, B.GetOperator);
end;

class operator TdxLinqExpression.LogicalNot(const A: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxUnaryOperator.Create(TdxUnaryOperatorType.&Not, A.GetOperator);
end;

class operator TdxLinqExpression.LogicalOr(const A, B: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxGroupOperator.Combine(TdxGroupOperatorType.&Or, A.GetOperator, B.GetOperator);
end;

class operator TdxLinqExpression.Modulus(const A, B: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxBinaryOperator.Create(A.GetOperator, B.GetOperator, TdxBinaryOperatorType.Modulo);
end;

class operator TdxLinqExpression.Multiply(const A, B: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxBinaryOperator.Create(A.GetOperator, B.GetOperator, TdxBinaryOperatorType.Multiply);
end;

class operator TdxLinqExpression.Negative(const A: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxUnaryOperator.Create(TdxUnaryOperatorType.Minus, A.GetOperator);
end;

class operator TdxLinqExpression.NotEqual(const A: TdxLinqExpression; const B: TGUID): TdxLinqExpression;
begin
  Result := TdxBinaryOperator.Create(A.GetOperator, TdxOperandValue.Create(B), TdxBinaryOperatorType.NotEqual);
end;

class operator TdxLinqExpression.NotEqual(const A: TGUID; const B: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxBinaryOperator.Create(TdxOperandValue.Create(A), B.GetOperator, TdxBinaryOperatorType.NotEqual);
end;

class operator TdxLinqExpression.Positive(const A: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxUnaryOperator.Create(TdxUnaryOperatorType.Plus, A.GetOperator);
end;

class operator TdxLinqExpression.Subtract(const A, B: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxBinaryOperator.Create(A.GetOperator, B.GetOperator, TdxBinaryOperatorType.Minus);
end;

class function TdxLinqExpression.ToArray(const A: array of TdxLinqExpression): TArray<IdxCriteriaOperator>;
var
  I: Integer;
begin
  SetLength(Result, System.Length(A));
  for I := 0 to System.Length(A) - 1 do
    Result[I] := A[I].GetOperator;
end;


class operator TdxLinqExpression.NotEqual(const A, B: TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxBinaryOperator.Create(A.GetOperator, B.GetOperator, TdxBinaryOperatorType.NotEqual);
end;

{$IFDEF DELPHIXE3}
class operator TdxLinqExpression.In(const A: TdxLinqExpression; const B: array of TdxLinqExpression): TdxLinqExpression;
begin
  Result := TdxInOperator.Create(A.GetOperator, ToArray(B));
end;
{$ENDIF}

function TdxLinqExpression.&In<T>(const B: TArray<T>): TdxLinqExpression;
var
  I: Integer;
  AOperandValues: TArray<IdxCriteriaOperator>;
begin
  SetLength(AOperandValues, System.Length(B));
  for I := 0 to System.Length(B) - 1 do
    AOperandValues[I] := TdxOperandValue.Create<T>(B[I]);
  Result := TdxInOperator.Create(Self.GetOperator, AOperandValues);
end;

function TdxLinqExpression.IsNull: TdxLinqExpression;
begin
  Result := TdxUnaryOperator.Create(TdxUnaryOperatorType.IsNull, Self.GetOperator);
end;


class operator TdxLinqExpression.Add(const A, B: TdxLinqExpression): TdxLinqExpression;
begin
    Result := TdxBinaryOperator.Create(A.GetOperator, B.GetOperator, TdxBinaryOperatorType.Plus);
end;


(*$HPPEMIT END 'namespace Dxemf {'*)
(*$HPPEMIT END 'namespace Linq {'*)
(*$HPPEMIT END 'namespace Expressions {'*)
(*$HPPEMIT END '    inline TdxLinqExpression operator==(const TdxLinqExpression A, const TdxLinqExpression B) { return TdxLinqExpression::_op_Equality(A, B); }'*)
(*$HPPEMIT END '    template<typename T>TdxLinqExpression operator==(const TdxLinqExpression A, T B) { TdxLinqExpression E; E = new TdxOperandValue(B); return TdxLinqExpression::_op_Equality(A, E); };'*)
(*$HPPEMIT END '    template<typename T>TdxLinqExpression operator==(T A, const TdxLinqExpression B) { TdxLinqExpression E; E = new TdxOperandValue(A); return TdxLinqExpression::_op_Equality(E, B); };'*)

(*$HPPEMIT END '    inline TdxLinqExpression operator!=(const TdxLinqExpression A, const TdxLinqExpression B) { return TdxLinqExpression::_op_Inequality(A, B); }'*)
(*$HPPEMIT END '    template<typename T>TdxLinqExpression operator!=(const TdxLinqExpression A, T B) { TdxLinqExpression E; E = new TdxOperandValue(B); return TdxLinqExpression::_op_Inequality(A, E); };'*)
(*$HPPEMIT END '    template<typename T>TdxLinqExpression operator!=(T A, const TdxLinqExpression B) { TdxLinqExpression E; E = new TdxOperandValue(A); return TdxLinqExpression::_op_Inequality(E, B); };'*)

(*$HPPEMIT END '    inline TdxLinqExpression operator>(const TdxLinqExpression A, const TdxLinqExpression B) { return TdxLinqExpression::_op_GreaterThan(A, B); }'*)
(*$HPPEMIT END '    template<typename T>TdxLinqExpression operator>(const TdxLinqExpression A, T B) { TdxLinqExpression E; E = new TdxOperandValue(B); return TdxLinqExpression::_op_GreaterThan(A, E); };'*)
(*$HPPEMIT END '    template<typename T>TdxLinqExpression operator>(T A, const TdxLinqExpression B) { TdxLinqExpression E; E = new TdxOperandValue(A); return TdxLinqExpression::_op_GreaterThan(E, B); };'*)

(*$HPPEMIT END '    inline TdxLinqExpression operator>=(const TdxLinqExpression A, const TdxLinqExpression B) { return TdxLinqExpression::_op_GreaterThanOrEqual(A, B); }'*)
(*$HPPEMIT END '    template<typename T>TdxLinqExpression operator>=(const TdxLinqExpression A, T B) { TdxLinqExpression E; E = new TdxOperandValue(B); return TdxLinqExpression::_op_GreaterThanOrEqual(A, E); };'*)
(*$HPPEMIT END '    template<typename T>TdxLinqExpression operator>=(T A, const TdxLinqExpression B) { TdxLinqExpression E; E = new TdxOperandValue(A); return TdxLinqExpression::_op_GreaterThanOrEqual(E, B); };'*)

(*$HPPEMIT END '    inline TdxLinqExpression operator<(const TdxLinqExpression A, const TdxLinqExpression B) { return TdxLinqExpression::_op_LessThan(A, B); }'*)
(*$HPPEMIT END '    template<typename T>TdxLinqExpression operator<(const TdxLinqExpression A, T B) { TdxLinqExpression E; E = new TdxOperandValue(B); return TdxLinqExpression::_op_LessThan(A, E); };'*)
(*$HPPEMIT END '    template<typename T>TdxLinqExpression operator<(T A, const TdxLinqExpression B) { TdxLinqExpression E; E = new TdxOperandValue(A); return TdxLinqExpression::_op_LessThan(E, B); };'*)

(*$HPPEMIT END '    inline TdxLinqExpression operator<=(const TdxLinqExpression A, const TdxLinqExpression B) { return TdxLinqExpression::_op_LessThanOrEqual(A, B); }'*)
(*$HPPEMIT END '    template<typename T>TdxLinqExpression operator<=(const TdxLinqExpression A, T B) { TdxLinqExpression E; E = new TdxOperandValue(B); return TdxLinqExpression::_op_LessThanOrEqual(A, E); };'*)
(*$HPPEMIT END '    template<typename T>TdxLinqExpression operator<=(T A, const TdxLinqExpression B) { TdxLinqExpression E; E = new TdxOperandValue(A); return TdxLinqExpression::_op_LessThanOrEqual(E, B); };'*)

(*$HPPEMIT END '    inline TdxLinqExpression operator&&(const TdxLinqExpression A, const TdxLinqExpression B) { return TdxLinqExpression::_op_LogicalAnd(A, B); }'*)
(*$HPPEMIT END '    template<typename T>TdxLinqExpression operator&&(const TdxLinqExpression A, T B) { TdxLinqExpression E; E = new TdxOperandValue(B); return TdxLinqExpression::_op_LogicalAnd(A, E); };'*)
(*$HPPEMIT END '    template<typename T>TdxLinqExpression operator&&(T A, const TdxLinqExpression B) { TdxLinqExpression E; E = new TdxOperandValue(A); return TdxLinqExpression::_op_LogicalAnd(E, B); };'*)

(*$HPPEMIT END '    inline TdxLinqExpression operator||(const TdxLinqExpression A, const TdxLinqExpression B) { return TdxLinqExpression::_op_LogicalOr(A, B); }'*)
(*$HPPEMIT END '    template<typename T>TdxLinqExpression operator||(const TdxLinqExpression A, T B) { TdxLinqExpression E; E = new TdxOperandValue(B); return TdxLinqExpression::_op_LogicalOr(A, E); };'*)
(*$HPPEMIT END '    template<typename T>TdxLinqExpression operator||(T A, const TdxLinqExpression B) { TdxLinqExpression E; E = new TdxOperandValue(A); return TdxLinqExpression::_op_LogicalOr(E, B); };'*)

(*$HPPEMIT END '    inline TdxLinqExpression operator+(const TdxLinqExpression A, const TdxLinqExpression B) { return TdxLinqExpression::_op_Addition(A, B); }'*)
(*$HPPEMIT END '    template<typename T>TdxLinqExpression operator+(const TdxLinqExpression A, T B) { TdxLinqExpression E; E = new TdxOperandValue(B); return TdxLinqExpression::_op_Addition(A, E); };'*)
(*$HPPEMIT END '    template<typename T>TdxLinqExpression operator+(T A, const TdxLinqExpression B) { TdxLinqExpression E; E = new TdxOperandValue(A); return TdxLinqExpression::_op_Addition(E, B); };'*)

(*$HPPEMIT END '    inline TdxLinqExpression operator-(const TdxLinqExpression A, const TdxLinqExpression B) { return TdxLinqExpression::_op_Subtraction(A, B); }'*)
(*$HPPEMIT END '    template<typename T>TdxLinqExpression operator-(const TdxLinqExpression A, T B) { TdxLinqExpression E; E = new TdxOperandValue(B); return TdxLinqExpression::_op_Subtraction(A, E); };'*)
(*$HPPEMIT END '    template<typename T>TdxLinqExpression operator-(T A, const TdxLinqExpression B) { TdxLinqExpression E; E = new TdxOperandValue(A); return TdxLinqExpression::_op_Subtraction(E, B); };'*)

(*$HPPEMIT END '    inline TdxLinqExpression operator*(const TdxLinqExpression A, const TdxLinqExpression B) { return TdxLinqExpression::_op_Multiply(A, B); }'*)
(*$HPPEMIT END '    template<typename T>TdxLinqExpression operator*(const TdxLinqExpression A, T B) { TdxLinqExpression E; E = new TdxOperandValue(B); return TdxLinqExpression::_op_Multiply(A, E); };'*)
(*$HPPEMIT END '    template<typename T>TdxLinqExpression operator*(T A, const TdxLinqExpression B) { TdxLinqExpression E; E = new TdxOperandValue(A); return TdxLinqExpression::_op_Multiply(E, B); };'*)

(*$HPPEMIT END '    inline TdxLinqExpression operator/(const TdxLinqExpression A, const TdxLinqExpression B) { return TdxLinqExpression::_op_Division(A, B); }'*)
(*$HPPEMIT END '    template<typename T>TdxLinqExpression operator/(const TdxLinqExpression A, T B) { TdxLinqExpression E; E = new TdxOperandValue(B); return TdxLinqExpression::_op_Division(A, E); };'*)
(*$HPPEMIT END '    template<typename T>TdxLinqExpression operator/(T A, const TdxLinqExpression B) { TdxLinqExpression E; E = new TdxOperandValue(A); return TdxLinqExpression::_op_Division(E, B); };'*)

(*$HPPEMIT END '    inline TdxLinqExpression operator%(const TdxLinqExpression A, const TdxLinqExpression B) { return TdxLinqExpression::_op_Modulus(A, B); }'*)
(*$HPPEMIT END '    template<typename T>TdxLinqExpression operator%(const TdxLinqExpression A, T B) { TdxLinqExpression E; E = new TdxOperandValue(B); return TdxLinqExpression::_op_Modulus(A, E); };'*)
(*$HPPEMIT END '    template<typename T>TdxLinqExpression operator%(T A, const TdxLinqExpression B) { TdxLinqExpression E; E = new TdxOperandValue(A); return TdxLinqExpression::_op_Modulus(E, B); };'*)

(*$HPPEMIT END '    inline TdxLinqExpression operator~(const TdxLinqExpression A) { return TdxLinqExpression::_op_LogicalNot(A); }'*)
(*$HPPEMIT END '    inline TdxLinqExpression operator+(const TdxLinqExpression A) { return TdxLinqExpression::_op_UnaryPlus(A); }'*)
(*$HPPEMIT END '    inline TdxLinqExpression operator-(const TdxLinqExpression A) { return TdxLinqExpression::_op_UnaryNegation(A); }'*)
(*$HPPEMIT END '} ' *)
(*$HPPEMIT END '} ' *)
(*$HPPEMIT END '} ' *)

end.
