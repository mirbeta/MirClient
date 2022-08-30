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

unit dxEMF.Utils.Expressions;

interface

{$I cxVer.inc}
{$I dxEMF.inc}

uses
  SysUtils, Classes, TypInfo, Generics.Defaults, Generics.Collections, Rtti,
  dxEMF.Types;

type

  { TdxFunctionEditorCategory }

  TdxFunctionEditorCategory = (
    DateTime,
    Logical,
    Math,
    &String,
    Aggregate,
    All
  );

  { IdxExpressionEditor }

  IdxExpressionEditor = interface
  end;

  { TdxFunctionInfo }

  TdxFunctionInfo = class
  strict private
    FFunction: string;
    FDescription: string;
    FCursorOffset: Integer;
    FCategory: TdxFunctionEditorCategory;
  public
    constructor Create(const AFunction, ADescription: string; ACursorOffset: Integer; ACategory: TdxFunctionEditorCategory);

    property &Function: string read FFunction;
    property Description: string read FDescription;
    property CursorOffset: Integer read FCursorOffset;
    property Category: TdxFunctionEditorCategory read FCategory;
  end;

  { TdxFunctionOperatorHelper }

  TdxFunctionOperatorHelper = class
  strict private
    class var
      FFunctionInfoStaticDictionary: TDictionary<TdxFunctionOperatorType, TDictionary<Integer, TdxFunctionInfo>>;
      FAggregateFunctionInfo: TList<TdxFunctionInfo>;
    class constructor Create;
    class destructor Destroy;
  protected
    class procedure GetAllCustomFunctionInfo(const AEditor: IdxExpressionEditor; AResult: TList<TdxFunctionInfo>); static;
  public
    class function GetFunctionInfo(const AEditor: IdxExpressionEditor; AFunctionType: TdxFunctionOperatorType; AArgumentCount: Integer): TdxFunctionInfo; static;
    class function GetLocalizedFunctionInfo(const AEditor: IdxExpressionEditor; AFunctionInfo: TdxFunctionInfo): TdxFunctionInfo; static;
    class function ToFunctionEditorCategory(ACategory: TdxFunctionCategory): TdxFunctionEditorCategory; static;
    class function GetAllFunctionInfo(const AEditor: IdxExpressionEditor): TArray<TdxFunctionInfo>; static;
    class function IsValidCustomFunctionArgumentCount(const AFunctionName: string; AArgumentCount: Integer): Boolean; static;
    class function GetFunctionArgumentCount(AFunctionType: TdxFunctionOperatorType): TArray<Integer>; static;
  end;

implementation

uses
  dxCore,
  dxEMF.Utils,
  dxEMF.DB.Criteria,
  dxEMF.Strs;

{ TdxFunctionInfo }

constructor TdxFunctionInfo.Create(const AFunction, ADescription: string; ACursorOffset: Integer; ACategory: TdxFunctionEditorCategory);
begin
  FFunction := AFunction;
  FDescription := ADescription;
  FCursorOffset := ACursorOffset;
  FCategory := ACategory;
end;

{ TdxFunctionOperatorHelper }

class constructor TdxFunctionOperatorHelper.Create;
begin
  FFunctionInfoStaticDictionary := TObjectDictionary<TdxFunctionOperatorType, TDictionary<Integer, TdxFunctionInfo>>.Create([doOwnsValues]);
  FAggregateFunctionInfo := TObjectList<TdxFunctionInfo>.Create;


  FAggregateFunctionInfo.Add(TdxFunctionInfo.Create('Avg()', 'AvgAggregate.Description', 1, TdxFunctionEditorCategory.Aggregate));
  FAggregateFunctionInfo.Add(TdxFunctionInfo.Create('Count()', 'CountAggregate.Description', 0, TdxFunctionEditorCategory.Aggregate));
  FAggregateFunctionInfo.Add(TdxFunctionInfo.Create('Exists()', 'ExistsAggregate.Description', 0, TdxFunctionEditorCategory.Aggregate));
  FAggregateFunctionInfo.Add(TdxFunctionInfo.Create('Max()', 'MaxAggregate.Description', 1, TdxFunctionEditorCategory.Aggregate));
  FAggregateFunctionInfo.Add(TdxFunctionInfo.Create('Min()', 'MinAggregate.Description', 1, TdxFunctionEditorCategory.Aggregate));
  FAggregateFunctionInfo.Add(TdxFunctionInfo.Create('Sum()', 'SumAggregate.Description', 1, TdxFunctionEditorCategory.Aggregate));
  FAggregateFunctionInfo.Add(TdxFunctionInfo.Create('Single()', 'SingleAggregate.Description', 0, TdxFunctionEditorCategory.Aggregate));
end;

class destructor TdxFunctionOperatorHelper.Destroy;
begin
  FreeAndNil(FFunctionInfoStaticDictionary);
  FreeAndNil(FAggregateFunctionInfo);
end;

class function TdxFunctionOperatorHelper.GetFunctionInfo(const AEditor: IdxExpressionEditor;
  AFunctionType: TdxFunctionOperatorType; AArgumentCount: Integer): TdxFunctionInfo;
var
  AArgumentsDictionary: TDictionary<Integer, TdxFunctionInfo>;
  AStubFunctionInfo: TdxFunctionInfo;
begin
  if not FFunctionInfoStaticDictionary.TryGetValue(AFunctionType, AArgumentsDictionary) then
    raise EInvalidOperation.CreateFmt(sdxFunctionInfoNotFound,
      [TdxFormatterHelper.GetName(AFunctionType)]);
  if AArgumentsDictionary = nil then
    Exit(nil);
  if not AArgumentsDictionary.TryGetValue(AArgumentCount, AStubFunctionInfo) then
    raise EInvalidOperation.CreateFmt(sdxFunctionInfoNotFound2,
      [TdxFormatterHelper.GetName(AFunctionType), AArgumentCount]);
  if AStubFunctionInfo = nil then
    Exit(nil);
  Result := GetLocalizedFunctionInfo(AEditor, AStubFunctionInfo);
end;

class function TdxFunctionOperatorHelper.GetLocalizedFunctionInfo(const AEditor: IdxExpressionEditor; AFunctionInfo: TdxFunctionInfo): TdxFunctionInfo;
begin
  Result := TdxFunctionInfo.Create(AFunctionInfo.&Function,
    AFunctionInfo.Description,
    AFunctionInfo.CursorOffset, AFunctionInfo.Category);
end;

class procedure TdxFunctionOperatorHelper.GetAllCustomFunctionInfo(const AEditor: IdxExpressionEditor; AResult: TList<TdxFunctionInfo>);
begin
  NotImplemented;
end;

class function TdxFunctionOperatorHelper.ToFunctionEditorCategory(ACategory: TdxFunctionCategory): TdxFunctionEditorCategory;
begin
  case ACategory of
    TdxFunctionCategory.All:
      Result := TdxFunctionEditorCategory.All;
    TdxFunctionCategory.DateTime:
      Result := TdxFunctionEditorCategory.DateTime;
    TdxFunctionCategory.Logical:
      Result := TdxFunctionEditorCategory.Logical;
    TdxFunctionCategory.Math:
      Result := TdxFunctionEditorCategory.Math;
    TdxFunctionCategory.Text:
      Result := TdxFunctionEditorCategory.String;
    else
      raise EArgumentException.Create(sdxUnknownCategory);
  end;
end;

class function TdxFunctionOperatorHelper.GetAllFunctionInfo(const AEditor: IdxExpressionEditor): TArray<TdxFunctionInfo>;
var
  AResult: TList<TdxFunctionInfo>;
  AFunctionType: TdxFunctionOperatorType;
  AArgumentCount: TArray<Integer>;
  I: Integer;
  AFunctionInfo: TdxFunctionInfo;
begin
  AResult := TList<TdxFunctionInfo>.Create;
  for AFunctionType := Low(TdxFunctionOperatorType) to High(TdxFunctionOperatorType) do
  begin
    if (AFunctionType = TdxFunctionOperatorType.None) or (AFunctionType = TdxFunctionOperatorType.CustomNonDeterministic) then
      Continue;
    if AFunctionType = TdxFunctionOperatorType.Custom then
    begin
      GetAllCustomFunctionInfo(AEditor, AResult);
      Continue;
    end;
    AArgumentCount := GetFunctionArgumentCount(AFunctionType);
    if AArgumentCount = nil then
      raise EInvalidOperation.CreateFmt(sdxArgumentCountInformationNotFound,
        [TdxFormatterHelper.GetName(AFunctionType)]);
    for I := 0 to Length(AArgumentCount) - 1 do
    begin
      if AArgumentCount[I] < 0 then
        Continue;
      AFunctionInfo := GetFunctionInfo(AEditor, AFunctionType, AArgumentCount[I]);
      if AFunctionInfo = nil then
        Continue;
      AResult.Add(AFunctionInfo);
    end;
  end;

  NotImplemented;
  Result := AResult.ToArray;
end;

class function TdxFunctionOperatorHelper.IsValidCustomFunctionArgumentCount(const AFunctionName: string; AArgumentCount: Integer): Boolean;
begin
  Result := True;
end;

class function TdxFunctionOperatorHelper.GetFunctionArgumentCount(AFunctionType: TdxFunctionOperatorType): TArray<Integer>;
begin
  case AFunctionType of
    TdxFunctionOperatorType.LocalDateTimeDayAfterTomorrow,
    TdxFunctionOperatorType.LocalDateTimeLastWeek,
    TdxFunctionOperatorType.LocalDateTimeNextMonth,
    TdxFunctionOperatorType.LocalDateTimeNextWeek,
    TdxFunctionOperatorType.LocalDateTimeNextYear,
    TdxFunctionOperatorType.LocalDateTimeNow,
    TdxFunctionOperatorType.LocalDateTimeThisMonth,
    TdxFunctionOperatorType.LocalDateTimeThisWeek,
    TdxFunctionOperatorType.LocalDateTimeThisYear,
    TdxFunctionOperatorType.LocalDateTimeToday,
    TdxFunctionOperatorType.LocalDateTimeTomorrow,
    TdxFunctionOperatorType.LocalDateTimeTwoWeeksAway,
    TdxFunctionOperatorType.LocalDateTimeYesterday,
    TdxFunctionOperatorType.LocalDateTimeTwoMonthsAway,
    TdxFunctionOperatorType.LocalDateTimeTwoYearsAway,
    TdxFunctionOperatorType.LocalDateTimeLastMonth,
    TdxFunctionOperatorType.LocalDateTimeLastYear,
    TdxFunctionOperatorType.LocalDateTimeYearBeforeToday,
    TdxFunctionOperatorType.Now,
    TdxFunctionOperatorType.UtcNow,
    TdxFunctionOperatorType.Today,
    TdxFunctionOperatorType.Random:
      Result := TArray<Integer>.Create(0);
    TdxFunctionOperatorType.IsNullOrEmpty,
    TdxFunctionOperatorType.Trim,
    TdxFunctionOperatorType.UpperCase,
    TdxFunctionOperatorType.LowerCase,
    TdxFunctionOperatorType.Len,
    TdxFunctionOperatorType.IsOutlookIntervalBeyondThisYear,
    TdxFunctionOperatorType.IsOutlookIntervalEarlierThisMonth,
    TdxFunctionOperatorType.IsOutlookIntervalEarlierThisWeek,
    TdxFunctionOperatorType.IsOutlookIntervalEarlierThisYear,
    TdxFunctionOperatorType.IsOutlookIntervalLastWeek,
    TdxFunctionOperatorType.IsOutlookIntervalLaterThisMonth,
    TdxFunctionOperatorType.IsOutlookIntervalLaterThisWeek,
    TdxFunctionOperatorType.IsOutlookIntervalLaterThisYear,
    TdxFunctionOperatorType.IsOutlookIntervalNextWeek,
    TdxFunctionOperatorType.IsOutlookIntervalPriorThisYear,
    TdxFunctionOperatorType.IsOutlookIntervalToday,
    TdxFunctionOperatorType.IsOutlookIntervalTomorrow,
    TdxFunctionOperatorType.IsOutlookIntervalYesterday,
    TdxFunctionOperatorType.Ascii,
    TdxFunctionOperatorType.Char,
    TdxFunctionOperatorType.ToInteger,
    TdxFunctionOperatorType.ToInt64,
    TdxFunctionOperatorType.ToSingle,
    TdxFunctionOperatorType.ToDouble,
    TdxFunctionOperatorType.ToDecimal,
    TdxFunctionOperatorType.ToString,
    TdxFunctionOperatorType.Reverse,
    TdxFunctionOperatorType.Abs,
    TdxFunctionOperatorType.Sqr,
    TdxFunctionOperatorType.Cos,
    TdxFunctionOperatorType.Sin,
    TdxFunctionOperatorType.ArcTan,
    TdxFunctionOperatorType.Exp,
    TdxFunctionOperatorType.Log10,
    TdxFunctionOperatorType.Tan,
    TdxFunctionOperatorType.Sign,
    TdxFunctionOperatorType.Ceil,
    TdxFunctionOperatorType.Floor,
    TdxFunctionOperatorType.ArcSin,
    TdxFunctionOperatorType.ArcCos,
    TdxFunctionOperatorType.Cosh,
    TdxFunctionOperatorType.Sinh,
    TdxFunctionOperatorType.Tanh,
    TdxFunctionOperatorType.DateOf,
    TdxFunctionOperatorType.DayOf,
    TdxFunctionOperatorType.DayOfTheWeek,
    TdxFunctionOperatorType.DayOfTheYear,
    TdxFunctionOperatorType.HourOf,
    TdxFunctionOperatorType.MillisecondOf,
    TdxFunctionOperatorType.MinuteOf,
    TdxFunctionOperatorType.MonthOf,
    TdxFunctionOperatorType.SecondOf,
    TdxFunctionOperatorType.GetTimeOfDay,
    TdxFunctionOperatorType.YearOf,
    TdxFunctionOperatorType.IsThisMonth,
    TdxFunctionOperatorType.IsThisWeek,
    TdxFunctionOperatorType.IsThisYear,
    TdxFunctionOperatorType.IsNextMonth,
    TdxFunctionOperatorType.IsNextYear,
    TdxFunctionOperatorType.IsLastMonth,
    TdxFunctionOperatorType.IsLastYear,
    TdxFunctionOperatorType.IsYearToDate,
    TdxFunctionOperatorType.IsJanuary,
    TdxFunctionOperatorType.IsFebruary,
    TdxFunctionOperatorType.IsMarch,
    TdxFunctionOperatorType.IsApril,
    TdxFunctionOperatorType.IsMay,
    TdxFunctionOperatorType.IsJune,
    TdxFunctionOperatorType.IsJuly,
    TdxFunctionOperatorType.IsAugust,
    TdxFunctionOperatorType.IsSeptember,
    TdxFunctionOperatorType.IsOctober,
    TdxFunctionOperatorType.IsNovember,
    TdxFunctionOperatorType.IsDecember:
      Result := TArray<Integer>.Create(1);
    TdxFunctionOperatorType.Power,
    TdxFunctionOperatorType.ArcTan2,
    TdxFunctionOperatorType.BigMul,
    TdxFunctionOperatorType.IncDay,
    TdxFunctionOperatorType.IncHour,
    TdxFunctionOperatorType.IncMillisecond,
    TdxFunctionOperatorType.IncMinute,
    TdxFunctionOperatorType.IncMonth,
    TdxFunctionOperatorType.IncSecond,
    TdxFunctionOperatorType.IncTick,
    TdxFunctionOperatorType.AddTimeSpan,
    TdxFunctionOperatorType.IncYear,
    TdxFunctionOperatorType.Max,
    TdxFunctionOperatorType.Min,
    TdxFunctionOperatorType.DaysBetween,
    TdxFunctionOperatorType.HoursBetween,
    TdxFunctionOperatorType.MillisecondsBetween,
    TdxFunctionOperatorType.MinutesBetween,
    TdxFunctionOperatorType.MonthsBetween,
    TdxFunctionOperatorType.SecondsBetween,
    TdxFunctionOperatorType.DateDiffTicks,
    TdxFunctionOperatorType.YearsBetween,
    TdxFunctionOperatorType.StartsWith,
    TdxFunctionOperatorType.EndsWith,
    TdxFunctionOperatorType.Contains:
      Result := TArray<Integer>.Create(2);
    TdxFunctionOperatorType.Log,
    TdxFunctionOperatorType.IsNull,
    TdxFunctionOperatorType.Round:
      Result := TArray<Integer>.Create(1, 2);
    TdxFunctionOperatorType.Insert,
    TdxFunctionOperatorType.Replace:
      Result := TArray<Integer>.Create(3);
    TdxFunctionOperatorType.PadLeft,
    TdxFunctionOperatorType.PadRight,
    TdxFunctionOperatorType.Remove,
    TdxFunctionOperatorType.Substring:
      Result := TArray<Integer>.Create(2, 3);
    TdxFunctionOperatorType.CharIndex:
      Result := TArray<Integer>.Create(2, 3, 4);
    TdxFunctionOperatorType.Custom,
    TdxFunctionOperatorType.CustomNonDeterministic:
      Result := TArray<Integer>.Create(-1);
    TdxFunctionOperatorType.Concat:
      Result := TArray<Integer>.Create(1, 2, -1);
    TdxFunctionOperatorType.Iif:
      Result := TArray<Integer>.Create(3, -3);
    TdxFunctionOperatorType.IsSameDay:
      Result := TArray<Integer>.Create(2, -2);
    else
      Result := nil;
  end;
end;


end.
