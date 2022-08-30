{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressDataController                                    }
{                                                                    }
{           Copyright (c) 1998-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSDATACONTROLLER AND ALL         }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY. }
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

unit cxFilter;

{$I cxVer.inc}

interface

uses
  Types, SysUtils, Classes, Variants, Windows, cxClasses, dxCore, dxCoreClasses;

type
  TcxFilterCriteriaItemList = class;
  TcxFilterCriteriaItem = class;
  TcxFilterCriteria = class;
  TcxFilterValueList = class;

  TcxFilterOperatorKind = (foEqual, foNotEqual, foLess, foLessEqual,
    foGreater, foGreaterEqual, foLike, foNotLike, foBetween, foNotBetween,
    foInList, foNotInList,
    foYesterday, foToday, foTomorrow,
    foLast7Days, foLastWeek, foLast14Days, foLastTwoWeeks, foLast30Days, foLastMonth, foLastYear, foInPast,
    foThisWeek, foThisMonth, foThisYear,
    foNext7Days, foNextWeek, foNext14Days, foNextTwoWeeks, foNext30Days, foNextMonth, foNextYear, foInFuture,
    foContains, foNotContains, foBeginsWith, foEndsWith);
  TcxFilterOperatorKinds = set of TcxFilterOperatorKind;
  TcxFilterBoolOperatorKind = (fboAnd, fboOr, fboNotAnd, fboNotOr);

  { TcxFilterOperator
    CompareValues(): Value1 is Data Value, Value2 is CompareItem's Value }

  TcxFilterOperator = class
  private
    FCriteriaItem: TcxFilterCriteriaItem;
    FCriticalSection: TRTLCriticalSection;
  protected
    function GetExpressionFilterText(const AValue: Variant): string; virtual;
    function GetExpressionValue(const AValue: Variant): string; virtual;
    procedure Lock; inline;
    procedure PrepareDisplayValue(var DisplayValue: string); virtual;
    procedure Prepare; virtual;
    function PrepareExpressionValue(const AValue: Variant; var DisplayValue: string): Boolean; virtual;
    procedure Unlock; inline;
  public
    constructor Create(ACriteriaItem: TcxFilterCriteriaItem); virtual;
    destructor Destroy; override;
    function CompareValues(const AValue1, AValue2: Variant): Boolean; virtual; abstract;
    function DisplayText: string; virtual;
    function FilterText: string; virtual; abstract;
    function IsDescription: Boolean; virtual;
    function IsExpression: Boolean; virtual;
    function IsNullOperator: Boolean; virtual;
    property CriteriaItem: TcxFilterCriteriaItem read FCriteriaItem;
  end;

  TcxFilterOperatorClass = class of TcxFilterOperator;

  { TcxFilterEqualOperator }

  TcxFilterEqualOperator = class(TcxFilterOperator)
  protected
    function GetExpressionFilterText(const AValue: Variant): string; override;
  public
    function CompareValues(const AValue1, AValue2: Variant): Boolean; override;
    function DisplayText: string; override;
    function FilterText: string; override;
    function IsExpression: Boolean; override;
  end;

  { TcxFilterNotEqualOperator }

  TcxFilterNotEqualOperator = class(TcxFilterEqualOperator)
  protected
    function GetExpressionFilterText(const AValue: Variant): string; override;
  public
    function CompareValues(const AValue1, AValue2: Variant): Boolean; override;
    function DisplayText: string; override;
    function FilterText: string; override;
  end;

  { TcxFilterLessOperator }

  TcxFilterLessOperator = class(TcxFilterOperator)
  public
    function CompareValues(const AValue1, AValue2: Variant): Boolean; override;
    function DisplayText: string; override;
    function FilterText: string; override;
  end;

  { TcxFilterLessEqualOperator }

  TcxFilterLessEqualOperator = class(TcxFilterOperator)
  public
    function CompareValues(const AValue1, AValue2: Variant): Boolean; override;
    function DisplayText: string; override;
    function FilterText: string; override;
  end;

  { TcxFilterGreaterOperator }

  TcxFilterGreaterOperator = class(TcxFilterOperator)
  public
    function CompareValues(const AValue1, AValue2: Variant): Boolean; override;
    function DisplayText: string; override;
    function FilterText: string; override;
  end;

  { TcxFilterGreaterEqualOperator }

  TcxFilterGreaterEqualOperator = class(TcxFilterOperator)
  public
    function CompareValues(const AValue1, AValue2: Variant): Boolean; override;
    function DisplayText: string; override;
    function FilterText: string; override;
  end;

  { TcxFilterCustomLikeOperator }

  TcxFilterCustomLikeOperator = class(TcxFilterOperator)
  protected
    function AllowUserMasks: Boolean; virtual;
    function GetEscapeWildard: Char; virtual; deprecated 'use GetEscapeWildcard instead';
    function GetEscapeWildcard: Char; virtual;
    function GetPercentWildcard: Char; virtual;
    function GetUnderscoreWildcard: Char; virtual;
    function GetExpressionFilterText(const AValue: Variant): string; override;
    function GetExpressionOperatorText: string; virtual;
    function PrepareEscapedString(const AString: string): string; virtual;
    function PrepareFilterString(AValue: Variant): string; virtual;
    function PreparePatternString(AValue: Variant): string; virtual;
    function PrepareString(AValue: Variant): string; virtual;
  public
    function CompareValues(const AValue1, AValue2: Variant): Boolean; override;
    function FilterText: string; override;
    function IsExpression: Boolean; override;
  end;

  { TcxFilterLikeOperator }

  TcxFilterLikeOperator = class(TcxFilterCustomLikeOperator)
  protected
    function AllowUserMasks: Boolean; override;
    procedure PrepareDisplayValue(var DisplayValue: string); override;
  public
    function DisplayText: string; override;
    function FilterText: string; override;
    function IsExpression: Boolean; override;
  end;

  { TcxFilterNotLikeOperator }

  TcxFilterNotLikeOperator = class(TcxFilterLikeOperator)
  protected
    function GetExpressionFilterText(const AValue: Variant): string; override;
  public
    function CompareValues(const AValue1, AValue2: Variant): Boolean; override;
    function DisplayText: string; override;
    function FilterText: string; override;
  end;

  { TcxFilterContainsOperator }

  TcxFilterContainsOperator = class(TcxFilterCustomLikeOperator)
  protected
    function GetEscapeWildcard: Char; override;
    function PrepareFilterString(AValue: Variant): string; override;
    function PreparePatternString(AValue: Variant): string; override;
  public
    function DisplayText: string; override;
  end;

  { TcxFilterNotContainsOperator }

  TcxFilterNotContainsOperator = class(TcxFilterContainsOperator)
  protected
    function GetExpressionFilterText(const AValue: Variant): string; override;
    function GetExpressionOperatorText: string; override;
  public
    function CompareValues(const AValue1, AValue2: Variant): Boolean; override;
    function DisplayText: string; override;
  end;

  { TcxFilterBeginsWithOperator }

  TcxFilterBeginsWithOperator = class(TcxFilterCustomLikeOperator)
  protected
    function GetEscapeWildcard: Char; override;
    function PrepareFilterString(AValue: Variant): string; override;
    function PreparePatternString(AValue: Variant): string; override;
  public
    function DisplayText: string; override;
  end;

  { TcxFilterEndsWithOperator }

  TcxFilterEndsWithOperator = class(TcxFilterCustomLikeOperator)
  protected
    function GetEscapeWildcard: Char; override;
    function PrepareFilterString(AValue: Variant): string; override;
    function PreparePatternString(AValue: Variant): string; override;
  public
    function DisplayText: string; override;
  end;

  { TcxFilterBetweenOperator }

  TcxFilterBetweenOperator = class(TcxFilterOperator)
  protected
    function GetExpressionFilterText(const AValue: Variant): string; override;
    procedure PrepareDisplayValue(var DisplayValue: string); override;
    function PrepareExpressionValue(const AValue: Variant; var DisplayValue: string): Boolean; override;
  public
    function CompareValues(const AValue1, AValue2: Variant): Boolean; override;
    function DisplayText: string; override;
    function FilterText: string; override;
    function IsDescription: Boolean; override;
    function IsExpression: Boolean; override;
  end;

  { TcxFilterNotBetweenOperator }

  TcxFilterNotBetweenOperator = class(TcxFilterBetweenOperator)
  protected
    function GetExpressionFilterText(const AValue: Variant): string; override;
  public
    function CompareValues(const AValue1, AValue2: Variant): Boolean; override;
    function DisplayText: string; override;
    function FilterText: string; override;
  end;

  { TcxFilterInListOperator }

  TcxFilterInListOperator = class(TcxFilterOperator)
  protected
    function GetExpressionFilterText(const AValue: Variant): string; override;
    procedure PrepareDisplayValue(var DisplayValue: string); override;
    function PrepareExpressionValue(const AValue: Variant; var DisplayValue: string): Boolean; override;
  public
    function CompareValues(const AValue1, AValue2: Variant): Boolean; override;
    function DisplayText: string; override;
    function FilterText: string; override;
    function IsDescription: Boolean; override;
    function IsExpression: Boolean; override;
  end;

  { TcxFilterNotInListOperator }

  TcxFilterNotInListOperator = class(TcxFilterInListOperator)
  protected
    function GetExpressionFilterText(const AValue: Variant): string; override;
  public
    function CompareValues(const AValue1, AValue2: Variant): Boolean; override;
    function DisplayText: string; override;
    function FilterText: string; override;
  end;

  { TcxFilterNullOperator }

  TcxFilterNullOperator = class(TcxFilterEqualOperator)
  protected
    procedure PrepareDisplayValue(var DisplayValue: string); override;
  public
    function CompareValues(const AValue1, AValue2: Variant): Boolean; override;
    function DisplayText: string; override;
    function IsNullOperator: Boolean; override;
  end;

  { TcxFilterNotNullOperator }

  TcxFilterNotNullOperator = class(TcxFilterNullOperator)
  public
    function CompareValues(const AValue1, AValue2: Variant): Boolean; override;
    function DisplayText: string; override;
    function FilterText: string; override;
  end;

  { TcxFilterDateOperator }

  TcxFilterDateOperator = class(TcxFilterOperator)
  private
    FDate1: TDateTime;
    FDate2: TDateTime;
  protected
    procedure PrepareDisplayValue(var DisplayValue: string); override;
    function GetExpressionFilterText(const AValue: Variant): string; override;
  public
    function CompareValues(const AValue1, AValue2: Variant): Boolean; override;
    function FilterText: string; override;
    function IsExpression: Boolean; override;
    property Date1: TDateTime read FDate1 write FDate1;
    property Date2: TDateTime read FDate2 write FDate2;
  end;

  { Yesterday }

  TcxFilterYesterdayOperator = class(TcxFilterDateOperator)
  protected
    procedure Prepare; override;
  public
    function DisplayText: string; override;
  end;

  { Today }

  TcxFilterTodayOperator = class(TcxFilterDateOperator)
  protected
    procedure Prepare; override;
  public
    function DisplayText: string; override;
  end;

  { Tomorrow }

  TcxFilterTomorrowOperator = class(TcxFilterDateOperator)
  protected
    procedure Prepare; override;
  public
    function DisplayText: string; override;
  end;

  { LastNDays }

  TcxFilterLastNDaysOperator = class(TcxFilterDateOperator)
  protected
    function DayCount: Integer; virtual; abstract;
    procedure Prepare; override;
  end;

  { Last7Days }

  TcxFilterLast7DaysOperator = class(TcxFilterLastNDaysOperator)
  protected
    function DayCount: Integer; override;
  public
    function DisplayText: string; override;
  end;

  { LastWeek }

  TcxFilterLastWeekOperator = class(TcxFilterDateOperator)
  protected
    procedure Prepare; override;
  public
    function DisplayText: string; override;
  end;

  { Last14Days }

  TcxFilterLast14DaysOperator = class(TcxFilterLastNDaysOperator)
  protected
    function DayCount: Integer; override;
  public
    function DisplayText: string; override;
  end;

  { LastTwoWeeks }

  TcxFilterLastTwoWeeksOperator = class(TcxFilterDateOperator)
  protected
    procedure Prepare; override;
  public
    function DisplayText: string; override;
  end;

  { Last30Days }

  TcxFilterLast30DaysOperator = class(TcxFilterLastNDaysOperator)
  protected
    function DayCount: Integer; override;
  public
    function DisplayText: string; override;
  end;

  { LastMonth }

  TcxFilterLastMonthOperator = class(TcxFilterDateOperator)
  protected
    procedure Prepare; override;
  public
    function DisplayText: string; override;
  end;

  { LastYear }

  TcxFilterLastYearOperator = class(TcxFilterDateOperator)
  protected
    procedure Prepare; override;
  public
    function DisplayText: string; override;
  end;

  { InPast }

  TcxFilterInPastOperator = class(TcxFilterDateOperator)
  protected
    procedure Prepare; override;
  public
    function DisplayText: string; override;
  end;

  { ThisWeek }

  TcxFilterThisWeekOperator = class(TcxFilterDateOperator)
  protected
    procedure Prepare; override;
  public
    function DisplayText: string; override;
  end;

  { ThisMonth }

  TcxFilterThisMonthOperator = class(TcxFilterDateOperator)
  protected
    procedure Prepare; override;
  public
    function DisplayText: string; override;
  end;

  { ThisYear }

  TcxFilterThisYearOperator = class(TcxFilterDateOperator)
  protected
    procedure Prepare; override;
  public
    function DisplayText: string; override;
  end;

  { NextNDays }

  TcxFilterNextNDaysOperator = class(TcxFilterDateOperator)
  protected
    function DayCount: Integer; virtual; abstract;
    procedure Prepare; override;
  end;

  { Next7Days }

  TcxFilterNext7DaysOperator = class(TcxFilterNextNDaysOperator)
  protected
    function DayCount: Integer; override;
  public
    function DisplayText: string; override;
  end;

  { NextWeek }

  TcxFilterNextWeekOperator = class(TcxFilterDateOperator)
  protected
    procedure Prepare; override;
  public
    function DisplayText: string; override;
  end;

  { Next14Days }

  TcxFilterNext14DaysOperator = class(TcxFilterNextNDaysOperator)
  protected
    function DayCount: Integer; override;
  public
    function DisplayText: string; override;
  end;

  { NextTwoWeeks }

  TcxFilterNextTwoWeeksOperator = class(TcxFilterDateOperator)
  protected
    procedure Prepare; override;
  public
    function DisplayText: string; override;
  end;

  { Next30Days }

  TcxFilterNext30DaysOperator = class(TcxFilterNextNDaysOperator)
  protected
    function DayCount: Integer; override;
  public
    function DisplayText: string; override;
  end;

  { NextMonth }

  TcxFilterNextMonthOperator = class(TcxFilterDateOperator)
  protected
    procedure Prepare; override;
  public
    function DisplayText: string; override;
  end;

  { NextYear }

  TcxFilterNextYearOperator = class(TcxFilterDateOperator)
  protected
    procedure Prepare; override;
  public
    function DisplayText: string; override;
  end;

  { InFuture }

  TcxFilterInFutureOperator = class(TcxFilterDateOperator)
  protected
    procedure Prepare; override;
  public
    function DisplayText: string; override;
  end;

  { TcxCustomFilterCriteriaItem }

  TcxCustomFilterCriteriaItem = class
  private
    FParent: TcxFilterCriteriaItemList;
  protected
    procedure Changed; virtual;
    function DoFilterData(AData: TObject): Boolean; virtual; abstract;
    function GetCriteria: TcxFilterCriteria; virtual;
    function GetIsItemList: Boolean; virtual; abstract;
    procedure ReadData(AStream: TStream); virtual;
    procedure WriteData(AStream: TStream); virtual;
  public
    constructor Create(AOwner: TcxFilterCriteriaItemList);
    destructor Destroy; override;
    function IsEmpty: Boolean; virtual; abstract;
    property IsItemList: Boolean read GetIsItemList;
    property Criteria: TcxFilterCriteria read GetCriteria;
    property Parent: TcxFilterCriteriaItemList read FParent;
  end;

  { TcxFilterCriteriaItemList }

  TcxFilterCriteriaItemListClass = class of TcxFilterCriteriaItemList;

  TcxFilterCriteriaItemList = class(TcxCustomFilterCriteriaItem)
  private
    FBoolOperatorKind: TcxFilterBoolOperatorKind;
    FCriteria: TcxFilterCriteria;
    FItems: TdxFastList;

    procedure CheckFilterResult(var AResult: Boolean; out ADone: Boolean);
    function GetCount: Integer;
    function GetItem(Index: Integer): TcxCustomFilterCriteriaItem;
    procedure SetBoolOperatorKind(Value: TcxFilterBoolOperatorKind);
  protected
    function DoFilterData(AData: TObject): Boolean; override;
    function GetCriteria: TcxFilterCriteria; override;
    function GetIsItemList: Boolean; override;
    procedure RemoveItem(AItem: TcxCustomFilterCriteriaItem); virtual;
    procedure ReadData(AStream: TStream); override;
    procedure WriteData(AStream: TStream); override;
    function ReadItem(AStream: TStream): TcxCustomFilterCriteriaItem;
    procedure WriteItem(AStream: TStream; AItem: TcxCustomFilterCriteriaItem);
  public
    constructor Create(AOwner: TcxFilterCriteriaItemList; ABoolOperatorKind: TcxFilterBoolOperatorKind); virtual;
    destructor Destroy; override;
    function AddItem(AItemLink: TObject; AOperatorKind: TcxFilterOperatorKind;
      const AValue: Variant; const ADisplayValue: string): TcxFilterCriteriaItem;
    function AddItemList(ABoolOperatorKind: TcxFilterBoolOperatorKind): TcxFilterCriteriaItemList;
    procedure Clear;
    function IsEmpty: Boolean; override;
    property BoolOperatorKind: TcxFilterBoolOperatorKind read FBoolOperatorKind write SetBoolOperatorKind default fboAnd;
    property Count: Integer read GetCount;
    property Criteria: TcxFilterCriteria read GetCriteria;
    property Items[Index: Integer]: TcxCustomFilterCriteriaItem read GetItem; default;
  end;

  { TcxFilterCriteriaItem }

  TcxFilterCriteriaItem = class(TcxCustomFilterCriteriaItem)
  private
    FDisplayValue: string;
    FItemLink: TObject;
    FOperator: TcxFilterOperator;
    FOperatorKind: TcxFilterOperatorKind;
    FPreparedValue: Variant;
    FValue: Variant;

    function Compare(AData: TObject): Boolean;
    procedure InternalSetValue(const Value: Variant);
    procedure SetDisplayValue(const Value: string);
    procedure SetOperatorKind(Value: TcxFilterOperatorKind);
    procedure SetValue(const Value: Variant);
  protected
    procedure CheckDisplayValue;
    function DoFilterData(AData: TObject): Boolean; override;
    function GetDataValue(AData: TObject): Variant; virtual; abstract;
    function GetDisplayValue: string; virtual;
    function GetExpressionValue(AIsCaption: Boolean): string; virtual;
    function GetFieldCaption: string; virtual;
    function GetFieldName: string; virtual;
    function GetFilterOperatorClass: TcxFilterOperatorClass; virtual;
    function GetItemLink: TObject; virtual;
    procedure SetItemLink(Value: TObject); virtual;
    function GetIsItemList: Boolean; override;
    procedure Prepare;
    procedure PrepareValue;
    function CompareByDisplayValue: Boolean;
    procedure RecreateOperator; virtual;
    procedure ReadData(AStream: TStream); override;
    function SupportsMultiThreading: Boolean; virtual;
    procedure WriteData(AStream: TStream); override;
  public
    constructor Create(AOwner: TcxFilterCriteriaItemList; AItemLink: TObject;
      AOperatorKind: TcxFilterOperatorKind; const AValue: Variant;
      const ADisplayValue: string); virtual;
    destructor Destroy; override;
    function IsEmpty: Boolean; override;
    function ValueIsNull(const AValue: Variant): Boolean; virtual;
    property DisplayValue: string read FDisplayValue write SetDisplayValue;
    property ItemLink: TObject read GetItemLink;
    property Operator: TcxFilterOperator read FOperator;
    property OperatorKind: TcxFilterOperatorKind read FOperatorKind write SetOperatorKind;
    property PreparedValue: Variant read FPreparedValue;
    property Value: Variant read FValue write SetValue;
  end;

  TcxFilterCriteriaItemClass = class of TcxFilterCriteriaItem;

  { TcxFilterValueList }

  TcxFilterValueItemKind = (fviAll, fviCustom, fviBlanks, fviNonBlanks, fviUser,
    fviValue, fviMRU, fviMRUSeparator, fviSpecial, fviUserEx);

  TcxUserFilteringEvent = procedure(Sender: TObject; const AValue: Variant; const ADisplayText: string) of object;
  TcxUserFilteringExEvent = procedure(Sender: TObject; AFilterList: TcxFilterCriteriaItemList;
    const AValue: Variant; const ADisplayText: string) of object;
  TcxGetFilterDisplayTextEvent = procedure(Sender: TObject; const AValue: Variant; var ADisplayText: string) of object;
  TcxGetFilterValuesEvent = procedure(Sender: TObject; AValueList: TcxFilterValueList) of object;

  TcxFilterValueItem = class
  private
    FPreparedDisplayText: string;
    FPreparedValue: Variant;
    FDisplayText: string;
    FKind: TcxFilterValueItemKind;
    FOwner: TcxFilterValueList;
    FValue: Variant;
    function GetCriteria: TcxFilterCriteria; inline;
  protected
    procedure Prepare;

    property Criteria: TcxFilterCriteria read GetCriteria;
    property PreparedDisplayText: string read FPreparedDisplayText;
    property PreparedValue: Variant read FPreparedValue;
  public
    constructor Create(AOwner: TcxFilterValueList; AKind: TcxFilterValueItemKind;
      const AValue: Variant; const ADisplayText: string);
    function ComparePreparedValue(const APreparedValue: Variant; const APreparedDisplayText: string): Integer;

    property Kind: TcxFilterValueItemKind read FKind;
    property Value: Variant read FValue;
    property DisplayText: string read FDisplayText;
  end;

  TcxFilterMRUValueItem = class(TcxMRUItem)
  public
    Value: Variant;
    DisplayText: string;
    constructor Create(const AValue: Variant; const ADisplayText: string);
    function Equals(AItem: TcxMRUItem): Boolean; override;
  end;

  TcxFilterMRUValueItemsClass = class of TcxFilterMRUValueItems;

  TcxFilterMRUValueItems = class(TcxMRUItems)
  private
    function GetItem(Index: Integer): TcxFilterMRUValueItem;
  public
    procedure Add(const AValue: Variant; const ADisplayText: string);
    procedure AddItemsTo(AValueList: TcxFilterValueList);
    property Items[Index: Integer]: TcxFilterMRUValueItem read GetItem; default;
  end;

  TcxFilterValueList = class
  private
    FCriteria: TcxFilterCriteria;
    FItems: TdxFastObjectList;
    FIsUnsortedValues: Boolean;
    FSortByDisplayText: Boolean;
    function GetCount: Integer;
    function GetItem(Index: Integer): TcxFilterValueItem;
    function GetMaxCount: Integer;
    function GetStartValueIndex: Integer;
  protected
    procedure Add(AKind: TcxFilterValueItemKind; const AValue: Variant; const ADisplayText: string; ANoSorting: Boolean;
      AUniqueOnly: Boolean); overload;
    function CompareItem(AIndex: Integer; const APreparedValue: Variant;
      const APreparedDisplayText: string): Integer; virtual;
    function CreateItem(AKind: TcxFilterValueItemKind;
      const AValue: Variant; const ADisplayText: string): TcxFilterValueItem;
    function GetMRUSeparatorIndex: Integer;
    function FindValueIndex(const APreparedValue: Variant; const APreparedDisplayText: string; out AIndex: Integer): Boolean;
  public
    constructor Create(ACriteria: TcxFilterCriteria); virtual;
    destructor Destroy; override;
    procedure Add(AKind: TcxFilterValueItemKind; const AValue: Variant; const ADisplayText: string;
      ANoSorting: Boolean); overload; virtual;
    procedure Clear; virtual;
    procedure Delete(AIndex: Integer);
    function Find(const AValue: Variant; const ADisplayText: string; var AIndex: Integer): Boolean; virtual;
    function FindItemByKind(AKind: TcxFilterValueItemKind): Integer; overload;
    function FindItemByKind(AKind: TcxFilterValueItemKind; const AValue: Variant): Integer; overload;
    function FindItemByValue(const AValue: Variant): Integer;
    function GetIndexByCriteriaItem(ACriteriaItem: TcxFilterCriteriaItem): Integer; virtual;

    property Count: Integer read GetCount;
    property Criteria: TcxFilterCriteria read FCriteria;
    property Items[Index: Integer]: TcxFilterValueItem read GetItem; default;
    property ItemsList: TdxFastObjectList read FItems;
    property MaxCount: Integer read GetMaxCount;
    property SortByDisplayText: Boolean read FSortByDisplayText write FSortByDisplayText;
    property StartValueIndex: Integer read GetStartValueIndex;
  end;

  TcxFilterValueListClass = class of TcxFilterValueList;

  { TcxFilterCriteria }

  TcxFilterCriteriaOption = (fcoCaseInsensitive, fcoShowOperatorDescription,
    fcoSoftNull, fcoSoftCompare, fcoIgnoreNull);
  TcxFilterCriteriaOptions = set of TcxFilterCriteriaOption;

  TcxFilterCriteria = class(TPersistent)
  private
    FChanged: Boolean;
    FCriticalSection: TRTLCriticalSection;
    FDateTimeFormat: string;
    FEscapeWildcard: Char;
    FIsUnicode: Boolean;
    FLoadedVersion: Byte;
    FLockCount: Integer;
    FMaxValueListCount: Integer;
    FOptions: TcxFilterCriteriaOptions;
    FPercentWildcard: Char;
    FCompareByDisplayValues: Boolean;
    FRoot: TcxFilterCriteriaItemList;
    FSavedVersion: Byte;
    FSavingToStream: Boolean;
    FSupportedLike: Boolean;
    FTranslateBetween: Boolean;
    FTranslateLike: Boolean;
    FTranslateIn: Boolean;
    FUnderscoreWildcard: Char;
    FVersion: Byte;
    FOnChanged: TNotifyEvent;
    function GetOptions: TcxFilterCriteriaOptions;
    function GetStoreItemLinkNames: Boolean;
    procedure SetDateTimeFormat(const Value: string);
    procedure SetOptions(Value: TcxFilterCriteriaOptions);
    procedure SetPercentWildcard(Value: Char);
    procedure SetStoreItemLinkNames(Value: Boolean);
    procedure SetUnderscoreWildcard(Value: Char);
  protected
    procedure CheckChanges; virtual;
    function ConvertBoolToStr(const AValue: Variant): string; virtual;
    function ConvertDateToStr(const AValue: Variant): string; virtual;
    function DoFilterData(AData: TObject): Boolean;
    function FilterTextUsed: Boolean; virtual;
    procedure FormatFilterTextValue(AItem: TcxFilterCriteriaItem; const AValue: Variant;
      var ADisplayValue: string); virtual;
    function GetFilterCaption: string; virtual;
    function GetFilterExpression(AIsCaption: Boolean): string; virtual;
    function GetFilterText: string; virtual;
    function GetIDByItemLink(AItemLink: TObject): Integer; virtual; abstract;
    function GetNameByItemLink(AItemLink: TObject): string; virtual; abstract;
    function GetItemClass: TcxFilterCriteriaItemClass; virtual;
    function GetItemListClass: TcxFilterCriteriaItemListClass; virtual;
    function GetItemExpression(AItem: TcxFilterCriteriaItem; AIsCaption: Boolean): string; virtual;
    function GetItemExpressionFieldName(AItem: TcxFilterCriteriaItem; AIsCaption: Boolean): string; virtual;
    function GetItemExpressionOperator(AItem: TcxFilterCriteriaItem; AIsCaption: Boolean): string; virtual;
    function GetItemExpressionValue(AItem: TcxFilterCriteriaItem; AIsCaption: Boolean): string; virtual;
    function GetItemLinkByID(AID: Integer): TObject; virtual; abstract;
    function GetItemLinkByName(const AName: string): TObject; virtual; abstract;
    function GetRootClass: TcxFilterCriteriaItemListClass; virtual;
    function GetValueListClass: TcxFilterValueListClass; virtual;
    function IsStore: Boolean;
    procedure Lock; inline;
    procedure Prepare; virtual;
    function PrepareDisplayText(const ADisplayText: string): string; virtual;
    function PrepareValue(const AValue: Variant): Variant; virtual;
    procedure SetFilterText(const Value: string); virtual;
    function SupportsMultiThreading: Boolean;
    procedure Unlock; inline;
    procedure Update; virtual;

    property EscapeWildcard: Char read FEscapeWildcard write FEscapeWildcard;
    property IsUnicode: Boolean read FIsUnicode;
    property LoadedVersion: Byte read FLoadedVersion;
    property LockCount: Integer read FLockCount;
    property SavedVersion: Byte read FSavedVersion;
    property Version: Byte read FVersion write FVersion;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent; AIgnoreItemNames: Boolean = False); reintroduce; virtual;
    procedure AssignEvents(Source: TPersistent); virtual;
    procedure AssignItems(ASource: TcxFilterCriteria; AIgnoreItemNames: Boolean = False); virtual;
    function AddItem(AParent: TcxFilterCriteriaItemList; AItemLink: TObject;
      AOperatorKind: TcxFilterOperatorKind; const AValue: Variant;
      const ADisplayValue: string): TcxFilterCriteriaItem; virtual;
    procedure BeginUpdate;
    procedure CancelUpdate;
    procedure Clear;
    procedure Changed; virtual;
    procedure EndUpdate;
    function EqualItems(AFilterCriteria: TcxFilterCriteria; AIgnoreItemNames: Boolean = False): Boolean;
    function FindItemByItemLink(AItemLink: TObject): TcxFilterCriteriaItem; virtual;
    function IsEmpty: Boolean; virtual;
    procedure LoadFromStream(AStream: TStream); virtual;
    procedure Refresh;
    procedure RemoveItemByItemLink(AItemLink: TObject);
    procedure RestoreDefaults; virtual;
    procedure SaveToStream(AStream: TStream); virtual;
    function ValueIsNull(const AValue: Variant): Boolean; virtual;
    // internal
    procedure ReadData(AStream: TStream); virtual;
    procedure WriteData(AStream: TStream); overload;
    procedure WriteData(AStream: TStream; AVersion: Byte); overload; virtual;

    property DateTimeFormat: string read FDateTimeFormat write SetDateTimeFormat;
    property FilterCaption: string read GetFilterCaption;
    property FilterText: string read GetFilterText write SetFilterText;
    property CompareByDisplayValues: Boolean read FCompareByDisplayValues
      write FCompareByDisplayValues;
    property Root: TcxFilterCriteriaItemList read FRoot;
    property StoreItemLinkNames: Boolean read GetStoreItemLinkNames write SetStoreItemLinkNames;
    property SupportedLike: Boolean read FSupportedLike write FSupportedLike default True;
    property TranslateBetween: Boolean read FTranslateBetween write FTranslateBetween default False;
    property TranslateIn: Boolean read FTranslateIn write FTranslateIn default False;
    property TranslateLike: Boolean read FTranslateLike write FTranslateLike default False;
  published
    property MaxValueListCount: Integer read FMaxValueListCount write FMaxValueListCount default 0;
    property Options: TcxFilterCriteriaOptions read GetOptions write SetOptions default [];
    property PercentWildcard: Char read FPercentWildcard write SetPercentWildcard default '%';
    property UnderscoreWildcard: Char read FUnderscoreWildcard write SetUnderscoreWildcard default '_';
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  end;

function ExtractFilterDisplayValue(const AValues: string; var Pos: Integer): string;

//function StrToVarBetweenArray(const AValue: string): Variant;
//function StrToVarListArray(const AValue: string): Variant;

function VarBetweenArrayToStr(const AValue: Variant): string;
function VarListArrayToStr(const AValue: Variant): string;

var
  cxFilterIncludeTodayInLastNextDaysList: Boolean = True;

implementation

uses
  Math, RTLConsts, SqlTimSt,
  cxVariants, cxLike, cxFilterConsts, cxDateUtils, StrUtils;

const
  cxFilterNullDate = 0;  // it is safe because we do not use such past dates for the property values

type
  TFilterWrapper = class(TComponent)
  private
    FFilter: TcxFilterCriteria;
  published
    property Filter: TcxFilterCriteria read FFilter write FFilter;
  end;

function FilterVarToStr(const AValue: Variant): string;
begin
  if VarIsNull(AValue) then
    Result := cxSFilterString(@cxSFilterBlankCaption)
  else
    Result := VarToStr(AValue);
end;

function ExtractFilterDisplayValue(const AValues: string; var Pos: Integer): string;
var
  I: Integer;
begin
  I := Pos;
  while (I <= Length(AValues)) and (AValues[I] <> ';') do Inc(I);
  Result := Trim(Copy(AValues, Pos, I - Pos));
  if (I <= Length(AValues)) and (AValues[I] = ';') then Inc(I);
  Pos := I;
end;

function StrToVarBetweenArray(const AValue: string): Variant;
var
  APos: Integer;
  S1, S2: string;
begin
  S1 := '';
  S2 := '';
  APos := 1;
  S1 := ExtractFilterDisplayValue(AValue, APos);
  if APos <= Length(AValue) then
    S2 := ExtractFilterDisplayValue(AValue, APos);
  Result := VarBetweenArrayCreate(S1, S2);
end;

function StrToVarListArray(const AValue: string): Variant;
var
  AEmpty: Boolean;
  APos, I: Integer;
  S: string;
begin
  AEmpty := True;
  APos := 1;
  repeat
    I := APos;
    while (I <= Length(AValue)) and (AValue[I] <> ';') do Inc(I);
    S := Trim(Copy(AValue, APos, I - APos));
    if AEmpty then
    begin
      Result := VarListArrayCreate(S);
      AEmpty := False;
    end
    else
      VarListArrayAddValue(Result, S);
    APos := PosEx(';', AValue, I);
    if APos = 0 then
      Break
    else
      Inc(APos);
  until False;
  if AEmpty then
    Result := VarListArrayCreate('');
end;

function StreamsEqual(AStream1, AStream2: TMemoryStream): Boolean;
begin
  Result := (AStream1.Size = AStream2.Size) and
    CompareMem(AStream1.Memory, AStream2.Memory, AStream1.Size);
end;

function VarBetweenArrayToStr(const AValue: Variant): string;
begin
  Result := FilterVarToStr(AValue[0]) + ' ' +
    cxSFilterString(@cxSFilterAndCaption) + ' ' + FilterVarToStr(AValue[1]);
end;

function VarListArrayToStr(const AValue: Variant): string;
var
  I: Integer;
begin
  Result := '(' + FilterVarToStr(AValue[0]);
  for I := VarArrayLowBound(AValue, 1) + 1 to VarArrayHighBound(AValue, 1) do
    Result := Result + ', ' + FilterVarToStr(AValue[I]);
  Result := Result + ')';
end;

{ TcxFilterOperator }

constructor TcxFilterOperator.Create(ACriteriaItem: TcxFilterCriteriaItem);
begin
  inherited Create;
  FCriteriaItem := ACriteriaItem;
  InitializeCriticalSectionAndSpinCount(FCriticalSection, 512);
end;

destructor TcxFilterOperator.Destroy;
begin
  DeleteCriticalSection(FCriticalSection);
  inherited Destroy;
end;

function TcxFilterOperator.DisplayText: string;
begin
  Result := FilterText;
end;

function TcxFilterOperator.IsDescription: Boolean;
begin
  Result := False;
end;

function TcxFilterOperator.IsExpression: Boolean;
begin
  Result := False;
end;

function TcxFilterOperator.IsNullOperator: Boolean;
begin
  Result := False;
end;

function TcxFilterOperator.GetExpressionFilterText(const AValue: Variant): string;
begin
  Result := GetExpressionValue(AValue);
end;

function TcxFilterOperator.GetExpressionValue(const AValue: Variant): string;
var
  AVarType: Integer;
begin
  if not PrepareExpressionValue(AValue, Result) then
  begin
    if VarIsStr(AValue) then
      Result := QuotedStr(VarToStr(AValue))
    else
    begin
      AVarType := VarType(AValue);
      if (AVarType = varDate) or (AVarType = VarSQLTimeStamp) then
        Result := '''' + CriteriaItem.Criteria.ConvertDateToStr(AValue) + ''''
      else
        if AVarType = varBoolean then
          Result := CriteriaItem.Criteria.ConvertBoolToStr(AValue)
        else
          if AVarType = varNull then
            Result := 'NULL'
          else
            Result := VarToStr(AValue);
    end;
    CriteriaItem.Criteria.FormatFilterTextValue(CriteriaItem, AValue, Result);
  end;
end;

procedure TcxFilterOperator.Lock;
begin
  EnterCriticalSection(FCriticalSection);
end;

procedure TcxFilterOperator.PrepareDisplayValue(var DisplayValue: string);
begin
end;

procedure TcxFilterOperator.Prepare;
begin
end;

function TcxFilterOperator.PrepareExpressionValue(const AValue: Variant; var DisplayValue: string): Boolean;
begin
  Result := False;
end;

procedure TcxFilterOperator.Unlock;
begin
  LeaveCriticalSection(FCriticalSection);
end;

{ TcxFilterEqualOperator }

function TcxFilterEqualOperator.CompareValues(const AValue1, AValue2: Variant): Boolean;
begin
  Result := VarCompare(AValue1, AValue2) = 0;
end;

function TcxFilterEqualOperator.DisplayText: string;
begin
  Result := cxSFilterString(@cxSFilterOperatorEqual);
end;

function TcxFilterEqualOperator.FilterText: string;
begin
  Result := '=';
end;

function TcxFilterEqualOperator.IsExpression: Boolean;
begin
  Result := VarIsArray(CriteriaItem.Value);
end;

function TcxFilterEqualOperator.GetExpressionFilterText(const AValue: Variant): string;

  function ExtractFieldName(const Fields: string; var Pos: Integer): string;
  var
    I: Integer;
  begin
    I := Pos;
    while (I <= Length(Fields)) and (Fields[I] <> ';') do Inc(I);
    Result := Trim(Copy(Fields, Pos, I - Pos));
    if (I <= Length(Fields)) and (Fields[I] = ';') then Inc(I);
    Pos := I;
  end;

var
  AFieldNames: string;
  I, APos: Integer;
begin
  AFieldNames := CriteriaItem.GetFieldName;
  APos := 1;
  Result := '(' + ExtractFieldName(AFieldNames, APos) + ' = ' + GetExpressionValue(AValue[0]) + ')';
  for I := VarArrayLowBound(AValue, 1) + 1 to VarArrayHighBound(AValue, 1) do
    Result := Result + ' AND ' +
      '(' + ExtractFieldName(AFieldNames, APos) + ' = ' + GetExpressionValue(AValue[I]) + ')';
end;

{ TcxFilterNotEqualOperator }

function TcxFilterNotEqualOperator.CompareValues(const AValue1, AValue2: Variant): Boolean;
begin
  Result := not inherited CompareValues(AValue1, AValue2);
end;

function TcxFilterNotEqualOperator.DisplayText: string;
begin
  Result := cxSFilterString(@cxSFilterOperatorNotEqual);
end;

function TcxFilterNotEqualOperator.FilterText: string;
begin
  Result := '<>';
end;

function TcxFilterNotEqualOperator.GetExpressionFilterText(const AValue: Variant): string;
begin
  Result := 'NOT (' + inherited GetExpressionFilterText(AValue) + ')';
end;

{ TcxFilterLessOperator }

function TcxFilterLessOperator.CompareValues(const AValue1, AValue2: Variant): Boolean;
begin
  Result := VarCompare(AValue1, AValue2) < 0;
end;

function TcxFilterLessOperator.DisplayText: string;
begin
  Result := cxSFilterString(@cxSFilterOperatorLess);
end;

function TcxFilterLessOperator.FilterText: string;
begin
  Result := '<';
end;

{ TcxFilterLessEqualOperator }

function TcxFilterLessEqualOperator.CompareValues(const AValue1, AValue2: Variant): Boolean;
begin
  Result := VarCompare(AValue1, AValue2) <= 0;
end;

function TcxFilterLessEqualOperator.DisplayText: string;
begin
  Result := cxSFilterString(@cxSFilterOperatorLessEqual);
end;

function TcxFilterLessEqualOperator.FilterText: string;
begin
  Result := '<=';
end;

{ TcxFilterGreaterOperator }

function TcxFilterGreaterOperator.CompareValues(const AValue1, AValue2: Variant): Boolean;
begin
  Result := VarCompare(AValue1, AValue2) > 0;
end;

function TcxFilterGreaterOperator.DisplayText: string;
begin
  Result := cxSFilterString(@cxSFilterOperatorGreater);
end;

function TcxFilterGreaterOperator.FilterText: string;
begin
  Result := '>';
end;

{ TcxFilterGreaterEqualOperator }

function TcxFilterGreaterEqualOperator.CompareValues(const AValue1, AValue2: Variant): Boolean;
begin
  Result := VarCompare(AValue1, AValue2) >= 0;
end;

function TcxFilterGreaterEqualOperator.DisplayText: string;
begin
  Result := cxSFilterString(@cxSFilterOperatorGreaterEqual);
end;

function TcxFilterGreaterEqualOperator.FilterText: string;
begin
  Result := '>=';
end;

{ TcxFilterCustomLikeOperator }

function TcxFilterCustomLikeOperator.CompareValues(const AValue1, AValue2: Variant): Boolean;
begin
  Result := LikeStr(PrepareString(AValue1), PrepareFilterString(AValue2), GetPercentWildcard, GetUnderscoreWildcard,
    GetEscapeWildcard);
end;

function TcxFilterCustomLikeOperator.FilterText: string;
begin
  Result := DisplayText;
end;

function TcxFilterCustomLikeOperator.IsExpression: Boolean;
begin
  Result := True;
end;

function TcxFilterCustomLikeOperator.AllowUserMasks: Boolean;
begin
  Result := False;
end;

function TcxFilterCustomLikeOperator.GetEscapeWildard: Char;
begin
  Result := GetEscapeWildcard;
end;

function TcxFilterCustomLikeOperator.GetEscapeWildcard: Char;
begin
  Result := CriteriaItem.Criteria.EscapeWildcard;
end;

function TcxFilterCustomLikeOperator.GetPercentWildcard: Char;
begin
  Result := CriteriaItem.Criteria.PercentWildcard;
end;

function TcxFilterCustomLikeOperator.GetUnderscoreWildcard: Char;
begin
  Result := CriteriaItem.Criteria.UnderscoreWildcard;
end;

function TcxFilterCustomLikeOperator.GetExpressionFilterText(const AValue: Variant): string;
begin
  Result := CriteriaItem.GetFieldName + ' ' + GetExpressionOperatorText + ' ' + GetExpressionValue(PreparePatternString(AValue));
end;

function TcxFilterCustomLikeOperator.GetExpressionOperatorText: string;
begin
  Result := 'LIKE';
end;

function TcxFilterCustomLikeOperator.PrepareEscapedString(const AString: string): string;
var
  I: Integer;
  AChar: Char;
  ACharSet: TSysCharSet;
begin
  Result := '';
  ACharSet := [GetPercentWildcard, GetUnderscoreWildcard, GetEscapeWildcard];
  for I := 1 to Length(AString) do
  begin
    AChar := AString[I];
    if dxCharInSet(AChar, ACharSet) then
      Result := Result + GetEscapeWildcard;
    Result := Result + AChar;
  end;
end;

function TcxFilterCustomLikeOperator.PrepareFilterString(AValue: Variant): string;
begin
  Result := PrepareString(AValue);
  if not AllowUserMasks then
    Result := PrepareEscapedString(Result);
end;

function TcxFilterCustomLikeOperator.PreparePatternString(AValue: Variant): string;
begin
  Result := PrepareString(AValue);
end;

function TcxFilterCustomLikeOperator.PrepareString(AValue: Variant): string;
begin
  Result := VarToStr(AValue);
end;

{ TcxFilterLikeOperator }

function TcxFilterLikeOperator.DisplayText: string;
var
  S: string;
begin
  S := CriteriaItem.Value;
  case LikeOperatorByPattern(S, CriteriaItem.Criteria.PercentWildcard) of
    floBeginsWith:
      Result := cxSFilterString(@cxSFilterOperatorBeginsWith);
    floEndsWith:
      Result := cxSFilterString(@cxSFilterOperatorEndsWith);
    floContains:
      Result := cxSFilterString(@cxSFilterOperatorContains);
  else
    Result := cxSFilterString(@cxSFilterOperatorLike);
  end;
end;

function TcxFilterLikeOperator.FilterText: string;
begin
  Result := 'LIKE';
end;

function TcxFilterLikeOperator.IsExpression: Boolean;
begin
  Result := CriteriaItem.Criteria.TranslateLike;
end;

function TcxFilterLikeOperator.AllowUserMasks: Boolean;
begin
  Result := True;
end;

procedure TcxFilterLikeOperator.PrepareDisplayValue(var DisplayValue: string);
begin
  if fcoShowOperatorDescription in CriteriaItem.Criteria.Options then
    LikeOperatorByPattern(DisplayValue, CriteriaItem.Criteria.PercentWildcard);
end;

{ TcxFilterNotLikeOperator }

function TcxFilterNotLikeOperator.CompareValues(const AValue1, AValue2: Variant): Boolean;
begin
  Result := not inherited CompareValues(AValue1, AValue2);
end;

function TcxFilterNotLikeOperator.DisplayText: string;
var
  S: string;
begin
  S := CriteriaItem.Value;
  case LikeOperatorByPattern(S, CriteriaItem.Criteria.PercentWildcard) of
    floBeginsWith:
      Result := cxSFilterString(@cxSFilterOperatorDoesNotBeginWith);
    floEndsWith:
      Result := cxSFilterString(@cxSFilterOperatorDoesNotEndWith);
    floContains:
      Result := cxSFilterString(@cxSFilterOperatorDoesNotContain);
  else
    Result := cxSFilterString(@cxSFilterOperatorNotLike);
  end;
end;

function TcxFilterNotLikeOperator.FilterText: string;
begin
  Result := 'NOT LIKE';
end;

function TcxFilterNotLikeOperator.GetExpressionFilterText(const AValue: Variant): string;
begin
  Result := ' NOT (' + inherited GetExpressionFilterText(AValue) + ')';
end;

{ TcxFilterContainsOperator }

function TcxFilterContainsOperator.DisplayText: string;
begin
  Result := cxSFilterString(@cxSFilterOperatorContains);
end;

function TcxFilterContainsOperator.GetEscapeWildcard: Char;
begin
  Result := '^';
end;

function TcxFilterContainsOperator.PrepareFilterString(AValue: Variant): string;
begin
  Result := CriteriaItem.Criteria.PercentWildcard + inherited PrepareFilterString(AValue) +
    CriteriaItem.Criteria.PercentWildcard
end;

function TcxFilterContainsOperator.PreparePatternString(AValue: Variant): string;
begin
  Result := inherited PreparePatternString(AValue);
  if CriteriaItem.Criteria.FilterTextUsed then
    Result := CriteriaItem.Criteria.PercentWildcard + Result + CriteriaItem.Criteria.PercentWildcard;
end;

{ TcxFilterNotContainsOperator }

function TcxFilterNotContainsOperator.CompareValues(const AValue1, AValue2: Variant): Boolean;
begin
  Result := not inherited CompareValues(AValue1, AValue2);
end;

function TcxFilterNotContainsOperator.DisplayText: string;
begin
  Result := cxSFilterString(@cxSFilterOperatorDoesNotContain);
end;

function TcxFilterNotContainsOperator.GetExpressionFilterText(const AValue: Variant): string;
begin
  Result := inherited GetExpressionFilterText(AValue);
  if CriteriaItem.Criteria.TranslateLike then
    Result := ' NOT (' + Result + ')'
end;

function TcxFilterNotContainsOperator.GetExpressionOperatorText: string;
begin
  Result := inherited GetExpressionOperatorText;
  if not CriteriaItem.Criteria.TranslateLike then
    Result := 'NOT ' + Result;
end;

{ TcxFilterBeginsWithOperator }

function TcxFilterBeginsWithOperator.DisplayText: string;
begin
  Result := cxSFilterString(@cxSFilterOperatorBeginsWith);
end;

function TcxFilterBeginsWithOperator.GetEscapeWildcard: Char;
begin
  Result := '^';
end;

function TcxFilterBeginsWithOperator.PrepareFilterString(AValue: Variant): string;
begin
  Result := inherited PrepareFilterString(AValue) + CriteriaItem.Criteria.PercentWildcard;
end;

function TcxFilterBeginsWithOperator.PreparePatternString(AValue: Variant): string;
begin
  Result := inherited PreparePatternString(AValue);
  if CriteriaItem.Criteria.FilterTextUsed then
    Result := Result + CriteriaItem.Criteria.PercentWildcard;
end;

{ TcxFilterEndsWithOperator }

function TcxFilterEndsWithOperator.DisplayText: string;
begin
  Result := cxSFilterString(@cxSFilterOperatorEndsWith);
end;

function TcxFilterEndsWithOperator.GetEscapeWildcard: Char;
begin
  Result := '^';
end;

function TcxFilterEndsWithOperator.PrepareFilterString(AValue: Variant): string;
begin
  Result := CriteriaItem.Criteria.PercentWildcard + inherited PrepareFilterString(AValue);
end;

function TcxFilterEndsWithOperator.PreparePatternString(AValue: Variant): string;
begin
  Result := inherited PreparePatternString(AValue);
  if CriteriaItem.Criteria.FilterTextUsed then
    Result := CriteriaItem.Criteria.PercentWildcard + Result;
end;

{ TcxFilterBetweenOperator }

function TcxFilterBetweenOperator.CompareValues(const AValue1, AValue2: Variant): Boolean;
begin
  Lock;
  try
    Result := (VarCompare(AValue2[0], AValue1) <= 0) and (VarCompare(AValue1, AValue2[1]) <= 0);
  finally
    Unlock;
  end;
end;

function TcxFilterBetweenOperator.DisplayText: string;
begin
  Result := cxSFilterString(@cxSFilterOperatorBetween);
end;

function TcxFilterBetweenOperator.FilterText: string;
begin
  Result := 'BETWEEN';
end;

function TcxFilterBetweenOperator.IsDescription: Boolean;
begin
  Result := True;
end;

function TcxFilterBetweenOperator.IsExpression: Boolean;
begin
  Result := CriteriaItem.Criteria.TranslateBetween;
end;

function TcxFilterBetweenOperator.GetExpressionFilterText(const AValue: Variant): string;
var
  AFieldName: string;
begin
  AFieldName := CriteriaItem.GetFieldName;
  Result := '(' + AFieldName + ' >= ' + GetExpressionValue(AValue[0]) + ') AND (' +
    AFieldName + ' <= ' + GetExpressionValue(AValue[1]) + ')';
end;

procedure TcxFilterBetweenOperator.PrepareDisplayValue(var DisplayValue: string);
var
  S: string;
begin
  S := DisplayValue;
  DisplayValue := VarBetweenArrayToStr(StrToVarBetweenArray(S));
end;

function TcxFilterBetweenOperator.PrepareExpressionValue(const AValue: Variant;
  var DisplayValue: string): Boolean;
begin
  Result := VarIsArray(AValue);
  if Result then
    DisplayValue := GetExpressionValue(AValue[0]) + ' AND ' + GetExpressionValue(AValue[1]);
end;

{ TcxFilterNotBetweenOperator }

function TcxFilterNotBetweenOperator.CompareValues(const AValue1, AValue2: Variant): Boolean;
begin
  Result := not inherited CompareValues(AValue1, AValue2);
end;

function TcxFilterNotBetweenOperator.DisplayText: string;
begin
  Result := cxSFilterString(@cxSFilterOperatorNotBetween);
end;

function TcxFilterNotBetweenOperator.FilterText: string;
begin
  Result := 'NOT BETWEEN';
end;

function TcxFilterNotBetweenOperator.GetExpressionFilterText(const AValue: Variant): string;
begin
  Result := 'NOT (' + inherited GetExpressionFilterText(AValue) + ')';
end;

{ TcxFilterInListOperator }

function TcxFilterInListOperator.CompareValues(const AValue1, AValue2: Variant): Boolean;
var
  I: Integer;
begin
  Result := False;
  Lock;
  try
    for I := VarArrayLowBound(AValue2, 1) to VarArrayHighBound(AValue2, 1) do
    begin
      if AValue1 = AValue2[I] then
      begin
        Result := True;
        Break;
      end;
    end;
  finally
    Unlock;
  end;
end;

function TcxFilterInListOperator.DisplayText: string;
begin
  Result := cxSFilterString(@cxSFilterOperatorInList);
end;

function TcxFilterInListOperator.FilterText: string;
begin
  Result := 'IN';
end;

function TcxFilterInListOperator.IsDescription: Boolean;
begin
  Result := True;
end;

function TcxFilterInListOperator.IsExpression: Boolean;
begin
  Result := CriteriaItem.Criteria.TranslateIn;
end;

function TcxFilterInListOperator.GetExpressionFilterText(const AValue: Variant): string;
var
  AFieldName: string;
  I: Integer;
begin
  AFieldName := CriteriaItem.GetFieldName;
  Result := '(' + AFieldName + ' = ' + GetExpressionValue(AValue[0]) + ')';
  for I := VarArrayLowBound(AValue, 1) + 1 to VarArrayHighBound(AValue, 1) do
    Result := Result + ' OR ' + '(' + AFieldName + ' = ' + GetExpressionValue(AValue[I]) + ')';
end;

procedure TcxFilterInListOperator.PrepareDisplayValue(var DisplayValue: string);
var
  S: string;
begin
  S := DisplayValue;
  DisplayValue := VarListArrayToStr(StrToVarListArray(S));
end;

function TcxFilterInListOperator.PrepareExpressionValue(const AValue: Variant;
  var DisplayValue: string): Boolean;
var
  I: Integer;
begin
  Result := VarIsArray(AValue);
  if Result then
  begin
    DisplayValue := '(' + GetExpressionValue(AValue[0]);
    for I := VarArrayLowBound(AValue, 1) + 1 to VarArrayHighBound(AValue, 1) do
      DisplayValue := DisplayValue + ', ' + GetExpressionValue(AValue[I]);
    DisplayValue := DisplayValue + ')';
  end;
end;

{ TcxFilterNotInListOperator }

function TcxFilterNotInListOperator.CompareValues(const AValue1, AValue2: Variant): Boolean;
begin
  Result := not inherited CompareValues(AValue1, AValue2);
end;

function TcxFilterNotInListOperator.DisplayText: string;
begin
  Result := cxSFilterString(@cxSFilterOperatorNotInList);
end;

function TcxFilterNotInListOperator.FilterText: string;
begin
  Result := 'NOT IN';
end;

function TcxFilterNotInListOperator.GetExpressionFilterText(const AValue: Variant): string;
begin
  Result := 'NOT (' + inherited GetExpressionFilterText(AValue) + ')';
end;

{ TcxFilterNullOperator }

function TcxFilterNullOperator.CompareValues(const AValue1, AValue2: Variant): Boolean;
begin
  Result := CriteriaItem.ValueIsNull(AValue1);
end;

function TcxFilterNullOperator.DisplayText: string;
begin
  Result := cxSFilterString(@cxSFilterOperatorIsNull);
end;

function TcxFilterNullOperator.IsNullOperator: Boolean;
begin
  Result := True;
end;

procedure TcxFilterNullOperator.PrepareDisplayValue(var DisplayValue: string);
begin
  if fcoShowOperatorDescription in CriteriaItem.Criteria.Options then
    DisplayValue := '';
end;

{ TcxFilterNotNullOperator }

function TcxFilterNotNullOperator.CompareValues(const AValue1, AValue2: Variant): Boolean;
begin
  Result := not inherited CompareValues(AValue1, AValue2);
end;

function TcxFilterNotNullOperator.DisplayText: string;
begin
  Result := cxSFilterString(@cxSFilterOperatorIsNotNull);
end;

function TcxFilterNotNullOperator.FilterText: string;
begin
  Result := '<>';
end;

{ TcxFilterDateOperator }

function TcxFilterDateOperator.CompareValues(const AValue1, AValue2: Variant): Boolean;
begin
  if CriteriaItem.ValueIsNull(AValue1) then
    Result := False
  else
    Result :=
      ((Date1 = cxFilterNullDate) or (Date1 <= AValue1)) and
      ((Date2 = cxFilterNullDate) or (AValue1 < Date2));
end;

function TcxFilterDateOperator.FilterText: string;
begin
  Result := DisplayText;
end;

function TcxFilterDateOperator.IsExpression: Boolean;
begin
  Result := True;
end;

function TcxFilterDateOperator.GetExpressionFilterText(const AValue: Variant): string;
var
  AFieldName: string;
begin
  AFieldName := CriteriaItem.GetFieldName;
  if Date1 <> cxFilterNullDate then
    Result := '(' + AFieldName + ' >= ' + GetExpressionValue(Date1) + ')'
  else
    Result := '';
  if Date2 <> cxFilterNullDate then
  begin
    if Result <> '' then
      Result := Result + ' AND ';
    Result := Result + '(' + AFieldName + ' < ' + GetExpressionValue(Date2) + ')';
  end;
end;

procedure TcxFilterDateOperator.PrepareDisplayValue(var DisplayValue: string);
begin
  DisplayValue := '';
end;

{ Yesterday }

procedure TcxFilterYesterdayOperator.Prepare;
var
  ANowDate: TDateTime;
begin
  ANowDate := Date;
  Date1 := ANowDate - 1;
  Date2 := ANowDate;
end;

function TcxFilterYesterdayOperator.DisplayText: string;
begin
  Result := cxSFilterString(@cxSFilterOperatorYesterday);
end;

{ Today }

procedure TcxFilterTodayOperator.Prepare;
var
  ANowDate: TDateTime;
begin
  ANowDate := Date;
  Date1 := ANowDate;
  Date2 := ANowDate + 1;
end;

function TcxFilterTodayOperator.DisplayText: string;
begin
  Result := cxSFilterString(@cxSFilterOperatorToday);
end;

{ Tomorrow }

procedure TcxFilterTomorrowOperator.Prepare;
var
  ANowDate: TDateTime;
begin
  ANowDate := Date;
  Date1 := ANowDate + 1;
  Date2 := ANowDate + 2;
end;

function TcxFilterTomorrowOperator.DisplayText: string;
begin
  Result := cxSFilterString(@cxSFilterOperatorTomorrow);
end;

{ LastNDays }

procedure TcxFilterLastNDaysOperator.Prepare;
begin
  Date2 := Date;
  if cxFilterIncludeTodayInLastNextDaysList then
    Date2 := Date2 + 1;
  Date1 := Date2 - DayCount;
end;

{ Last7Days }

function TcxFilterLast7DaysOperator.DayCount: Integer;
begin
  Result := 7;
end;

function TcxFilterLast7DaysOperator.DisplayText: string;
begin
  Result := cxSFilterString(@cxSFilterOperatorLast7Days);
end;

{ LastWeek }

procedure TcxFilterLastWeekOperator.Prepare;
var
  ANowDate: TDateTime;
begin
  ANowDate := Date;
  Date1 := dxGetStartDateOfWeek(ANowDate) - 7;
  Date2 := Date1 + 7;
end;

function TcxFilterLastWeekOperator.DisplayText: string;
begin
  Result := cxSFilterString(@cxSFilterOperatorLastWeek);
end;

{ Last14Days }

function TcxFilterLast14DaysOperator.DayCount: Integer;
begin
  Result := 14;
end;

function TcxFilterLast14DaysOperator.DisplayText: string;
begin
  Result := cxSFilterString(@cxSFilterOperatorLast14Days);
end;

{ LastTwoWeeks }

procedure TcxFilterLastTwoWeeksOperator.Prepare;
begin
  Date1 := dxGetStartDateOfWeek(Date) - 2 * 7;
  Date2 := Date1 + 2 * 7;
end;

function TcxFilterLastTwoWeeksOperator.DisplayText: string;
begin
  Result := cxSFilterString(@cxSFilterOperatorLastTwoWeeks);
end;

{ Last30Days }

function TcxFilterLast30DaysOperator.DayCount: Integer;
begin
  Result := 30;
end;

function TcxFilterLast30DaysOperator.DisplayText: string;
begin
  Result := cxSFilterString(@cxSFilterOperatorLast30Days);
end;

{ LastMonth }

procedure TcxFilterLastMonthOperator.Prepare;
var
  ANowDate: TDateTime;
  D, M, Y: Word;
begin
  ANowDate := Date;
  DecodeDate(ANowDate, Y, M, D);
  if M > 1 then
  begin
    Date1 := EncodeDate(Y, M - 1, 1);
    Date2 := EncodeDate(Y, M, 1);
  end
  else
  begin
    Date1 := EncodeDate(Y - 1, 12, 1);
    Date2 := EncodeDate(Y, 1, 1);
  end;
end;

function TcxFilterLastMonthOperator.DisplayText: string;
begin
  Result := cxSFilterString(@cxSFilterOperatorLastMonth);
end;

{ LastYear }

procedure TcxFilterLastYearOperator.Prepare;
var
  ANowDate: TDateTime;
  D, M, Y: Word;
begin
  ANowDate := Date;
  DecodeDate(ANowDate, Y, M, D);
  Date1 := EncodeDate(Y - 1, 1, 1);
  Date2 := EncodeDate(Y, 1, 1);
end;

function TcxFilterLastYearOperator.DisplayText: string;
begin
  Result := cxSFilterString(@cxSFilterOperatorLastYear);
end;

{ InPast }

procedure TcxFilterInPastOperator.Prepare;
begin
  Date1 := cxFilterNullDate;
  Date2 := Date;
end;

function TcxFilterInPastOperator.DisplayText: string;
begin
  Result := cxSFilterString(@cxSFilterOperatorPast);
end;

{ ThisWeek }

procedure TcxFilterThisWeekOperator.Prepare;
var
  ANowDate: TDateTime;
begin
  ANowDate := Date;
  Date1 := dxGetStartDateOfWeek(ANowDate);
  Date2 := Date1 + 7;
end;

function TcxFilterThisWeekOperator.DisplayText: string;
begin
  Result := cxSFilterString(@cxSFilterOperatorThisWeek);
end;

{ ThisMonth }

procedure TcxFilterThisMonthOperator.Prepare;
var
  ANowDate: TDateTime;
  D, M, Y: Word;
begin
  ANowDate := Date;
  DecodeDate(ANowDate, Y, M, D);
  if M < 12 then
  begin
    Date1 := EncodeDate(Y, M, 1);
    Date2 := EncodeDate(Y, M + 1, 1);
  end
  else
  begin
    Date1 := EncodeDate(Y, M, 1);
    Date2 := EncodeDate(Y + 1, 1, 1);
  end;
end;

function TcxFilterThisMonthOperator.DisplayText: string;
begin
  Result := cxSFilterString(@cxSFilterOperatorThisMonth);
end;

{ ThisYear }

procedure TcxFilterThisYearOperator.Prepare;
var
  ANowDate: TDateTime;
  D, M, Y: Word;
begin
  ANowDate := Date;
  DecodeDate(ANowDate, Y, M, D);
  Date1 := EncodeDate(Y, 1, 1);
  Date2 := EncodeDate(Y + 1, 1, 1);
end;

function TcxFilterThisYearOperator.DisplayText: string;
begin
  Result := cxSFilterString(@cxSFilterOperatorThisYear);
end;

{ NextNDays }

procedure TcxFilterNextNDaysOperator.Prepare;
begin
  Date1 := Date;
  if not cxFilterIncludeTodayInLastNextDaysList then
    Date1 := Date1 + 1;
  Date2 := Date1 + DayCount;
end;

{ Next7Days }

function TcxFilterNext7DaysOperator.DayCount: Integer;
begin
  Result := 7;
end;

function TcxFilterNext7DaysOperator.DisplayText: string;
begin
  Result := cxSFilterString(@cxSFilterOperatorNext7Days);
end;

{ NextWeek }

procedure TcxFilterNextWeekOperator.Prepare;
var
  ANowDate: TDateTime;
begin
  ANowDate := Date;
  Date1 := dxGetStartDateOfWeek(ANowDate) + 7;
  Date2 := Date1 + 7;
end;

function TcxFilterNextWeekOperator.DisplayText: string;
begin
  Result := cxSFilterString(@cxSFilterOperatorNextWeek);
end;

{ Next14Days }

function TcxFilterNext14DaysOperator.DayCount: Integer;
begin
  Result := 14;
end;

function TcxFilterNext14DaysOperator.DisplayText: string;
begin
  Result := cxSFilterString(@cxSFilterOperatorNext14Days);
end;

{ NextTwoWeeks }

procedure TcxFilterNextTwoWeeksOperator.Prepare;
begin
  Date1 := dxGetStartDateOfWeek(Date) + 7;
  Date2 := Date1 + 2 * 7;
end;

function TcxFilterNextTwoWeeksOperator.DisplayText: string;
begin
  Result := cxSFilterString(@cxSFilterOperatorNextTwoWeeks);
end;

{ Next30Days }

function TcxFilterNext30DaysOperator.DayCount: Integer;
begin
  Result := 30;
end;

function TcxFilterNext30DaysOperator.DisplayText: string;
begin
  Result := cxSFilterString(@cxSFilterOperatorNext30Days);
end;

{ NextMonth }

procedure TcxFilterNextMonthOperator.Prepare;
var
  ANowDate: TDateTime;
  D, M, Y: Word;
begin
  ANowDate := Date;
  DecodeDate(ANowDate, Y, M, D);
  if M < 11 then
  begin
    Date1 := EncodeDate(Y, M + 1, 1);
    Date2 := EncodeDate(Y, M + 2, 1);
  end
  else
    if M = 11 then
    begin
      Date1 := EncodeDate(Y, 12, 1);
      Date2 := EncodeDate(Y + 1, 1, 1);
    end
    else
      if M = 12 then
      begin
        Date1 := EncodeDate(Y + 1, 1, 1);
        Date2 := EncodeDate(Y + 1, 2, 1);
      end;
end;

function TcxFilterNextMonthOperator.DisplayText: string;
begin
  Result := cxSFilterString(@cxSFilterOperatorNextMonth);
end;

{ NextYear }

procedure TcxFilterNextYearOperator.Prepare;
var
  ANowDate: TDateTime;
  D, M, Y: Word;
begin
  ANowDate := Date;
  DecodeDate(ANowDate, Y, M, D);
  Date1 := EncodeDate(Y + 1, 1, 1);
  Date2 := EncodeDate(Y + 2, 1, 1);
end;

function TcxFilterNextYearOperator.DisplayText: string;
begin
  Result := cxSFilterString(@cxSFilterOperatorNextYear);
end;

{ InFuture }

procedure TcxFilterInFutureOperator.Prepare;
begin
  Date1 := Date + 1;
  Date2 := cxFilterNullDate;
end;

function TcxFilterInFutureOperator.DisplayText: string;
begin
  Result := cxSFilterString(@cxSFilterOperatorFuture);
end;

{ TcxCustomFilterCriteriaItem }

constructor TcxCustomFilterCriteriaItem.Create(AOwner: TcxFilterCriteriaItemList);
begin
  inherited Create;
  FParent := AOwner;
end;

destructor TcxCustomFilterCriteriaItem.Destroy;
begin
  if FParent <> nil then
    FParent.RemoveItem(Self);
  inherited Destroy;
end;

procedure TcxCustomFilterCriteriaItem.Changed;
begin
  Criteria.Changed;
end;

function TcxCustomFilterCriteriaItem.GetCriteria: TcxFilterCriteria;
begin
  Result := Parent.Criteria;
end;

procedure TcxCustomFilterCriteriaItem.ReadData(AStream: TStream);
begin
end;

procedure TcxCustomFilterCriteriaItem.WriteData(AStream: TStream);
begin
end;

{ TcxFilterCriteriaItemList }

constructor TcxFilterCriteriaItemList.Create(AOwner: TcxFilterCriteriaItemList;
  ABoolOperatorKind: TcxFilterBoolOperatorKind);
begin
  inherited Create(AOwner);
  FBoolOperatorKind := ABoolOperatorKind;
  FItems := TdxFastList.Create;
end;

destructor TcxFilterCriteriaItemList.Destroy;
begin
  Clear;
  FItems.Free;
  inherited Destroy;
end;

function TcxFilterCriteriaItemList.AddItem(AItemLink: TObject; AOperatorKind: TcxFilterOperatorKind;
  const AValue: Variant; const ADisplayValue: string): TcxFilterCriteriaItem;
begin
  Result := Criteria.GetItemClass.Create(Self, AItemLink, AOperatorKind, AValue, ADisplayValue);
  FItems.Add(Result);
  Changed;
end;

function TcxFilterCriteriaItemList.AddItemList(ABoolOperatorKind: TcxFilterBoolOperatorKind): TcxFilterCriteriaItemList;
begin
  Result := Criteria.GetItemListClass.Create(Self, ABoolOperatorKind);
  FItems.Add(Result);
  Changed;
end;

procedure TcxFilterCriteriaItemList.Clear;
var
  I: Integer;
begin
  Criteria.BeginUpdate;
  try
    for I := Count - 1 downto 0 do
      Items[I].Free;
  finally
    Criteria.EndUpdate;
  end;
end;

function TcxFilterCriteriaItemList.IsEmpty: Boolean;
begin
  Result := Count = 0;
end;

function TcxFilterCriteriaItemList.DoFilterData(AData: TObject): Boolean;
var
  I: Integer;
  ADone: Boolean;
begin
  Result := True;
  for I := 0 to Count - 1 do
  begin
    Result := Items[I].DoFilterData(AData);
    CheckFilterResult(Result, ADone);
    if ADone then
      Break;
  end;
end;

function TcxFilterCriteriaItemList.GetCriteria: TcxFilterCriteria;
begin
  if FCriteria <> nil then
    Result := FCriteria
  else
    Result := inherited GetCriteria;
end;

function TcxFilterCriteriaItemList.GetIsItemList: Boolean;
begin
  Result := True;
end;

procedure TcxFilterCriteriaItemList.RemoveItem(AItem: TcxCustomFilterCriteriaItem);
begin
  if FItems.Remove(AItem) <> -1 then
    Changed;
end;

procedure TcxFilterCriteriaItemList.ReadData(AStream: TStream);
var
  ACount, I: Integer;
begin
  inherited;
  BoolOperatorKind := TcxFilterBoolOperatorKind(ReadByteFunc(AStream));
  ACount := ReadIntegerFunc(AStream);
  for I := 0 to ACount - 1 do
    ReadItem(AStream);
end;

procedure TcxFilterCriteriaItemList.WriteData(AStream: TStream);
var
  I: Integer;
begin
  inherited;
  WriteByteProc(AStream, Byte(BoolOperatorKind));
  WriteIntegerProc(AStream, Count);
  for I := 0 to Count - 1 do
    WriteItem(AStream, Items[I]);
end;

function TcxFilterCriteriaItemList.ReadItem(AStream: TStream): TcxCustomFilterCriteriaItem;
var
  AIsItemList: Boolean;
begin
  AIsItemList := ReadBooleanFunc(AStream);
  if AIsItemList then
    Result := AddItemList(fboAnd)
  else
    Result := AddItem(nil, foEqual, Unassigned, '');
  Result.ReadData(AStream);
  if Result.IsEmpty then
    FreeAndNil(Result);
end;

procedure TcxFilterCriteriaItemList.WriteItem(AStream: TStream;
  AItem: TcxCustomFilterCriteriaItem);
begin
  WriteBooleanProc(AStream, AItem.IsItemList);
  AItem.WriteData(AStream);
end;

procedure TcxFilterCriteriaItemList.CheckFilterResult(var AResult: Boolean; out ADone: Boolean);
begin
  ADone := ((BoolOperatorKind in [fboOr, fboNotOr]) and AResult) or
    ((BoolOperatorKind in [fboAnd, fboNotAnd]) and not AResult);
  if BoolOperatorKind in [fboNotAnd, fboNotOr] then
    AResult := not AResult;
end;

function TcxFilterCriteriaItemList.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TcxFilterCriteriaItemList.GetItem(Index: Integer): TcxCustomFilterCriteriaItem;
begin
  if (0 <= Index) and (Index < Count) then
    Result := TcxCustomFilterCriteriaItem(FItems[Index])
  else
    Result := nil;
end;

procedure TcxFilterCriteriaItemList.SetBoolOperatorKind(Value: TcxFilterBoolOperatorKind);
begin
  if FBoolOperatorKind <> Value then
  begin
    FBoolOperatorKind := Value;
    Changed;
  end;
end;

{ TcxFilterCriteriaItem }

constructor TcxFilterCriteriaItem.Create(AOwner: TcxFilterCriteriaItemList;
  AItemLink: TObject; AOperatorKind: TcxFilterOperatorKind; const AValue: Variant;
  const ADisplayValue: string);
begin
  inherited Create(AOwner);
  SetItemLink(AItemLink);
  FDisplayValue := ADisplayValue;
  FOperatorKind := AOperatorKind;
  InternalSetValue(AValue);
  CheckDisplayValue;
end;

destructor TcxFilterCriteriaItem.Destroy;
begin
  FOperator.Free;
  FOperator := nil;
  inherited Destroy;
end;

function TcxFilterCriteriaItem.IsEmpty: Boolean;
begin
  Result := ItemLink = nil;
end;

function TcxFilterCriteriaItem.ValueIsNull(const AValue: Variant): Boolean;
begin
  Result := Criteria.ValueIsNull(AValue);
end;

procedure TcxFilterCriteriaItem.CheckDisplayValue;
begin
  if ((FOperator is TcxFilterNullOperator) or (FOperator is TcxFilterNotNullOperator)) and
    (FDisplayValue = '') then
    FDisplayValue := cxSFilterString(@cxSFilterBlankCaption);
end;

function TcxFilterCriteriaItem.DoFilterData(AData: TObject): Boolean;
begin
  Result := Compare(AData);
end;

function TcxFilterCriteriaItem.GetDisplayValue: string;
begin
  Result := DisplayValue;
  Operator.PrepareDisplayValue(Result);
end;

function TcxFilterCriteriaItem.GetExpressionValue(AIsCaption: Boolean): string;
begin
  if AIsCaption then
    Result := GetDisplayValue
  else
    Result := Operator.GetExpressionValue(Value);
end;

function TcxFilterCriteriaItem.GetFieldCaption: string;
begin
  Result := '';
end;

function TcxFilterCriteriaItem.GetFieldName: string;
begin
  Result := '';
end;

function TcxFilterCriteriaItem.GetFilterOperatorClass: TcxFilterOperatorClass;
const
  AOperatorClasses: array[TcxFilterOperatorKind] of TcxFilterOperatorClass = (
    TcxFilterEqualOperator, TcxFilterNotEqualOperator,
    TcxFilterLessOperator, TcxFilterLessEqualOperator,
    TcxFilterGreaterOperator, TcxFilterGreaterEqualOperator,
    TcxFilterLikeOperator, TcxFilterNotLikeOperator,
    TcxFilterBetweenOperator, TcxFilterNotBetweenOperator,
    TcxFilterInListOperator, TcxFilterNotInListOperator,
    TcxFilterYesterdayOperator, TcxFilterTodayOperator, TcxFilterTomorrowOperator,
    TcxFilterLast7DaysOperator, TcxFilterLastWeekOperator, TcxFilterLast14DaysOperator, TcxFilterLastTwoWeeksOperator,
    TcxFilterLast30DaysOperator, TcxFilterLastMonthOperator, TcxFilterLastYearOperator, TcxFilterInPastOperator,
    TcxFilterThisWeekOperator, TcxFilterThisMonthOperator, TcxFilterThisYearOperator,
    TcxFilterNext7DaysOperator, TcxFilterNextWeekOperator, TcxFilterNext14DaysOperator, TcxFilterNextTwoWeeksOperator,
    TcxFilterNext30DaysOperator, TcxFilterNextMonthOperator, TcxFilterNextYearOperator, TcxFilterInFutureOperator,
    TcxFilterContainsOperator, TcxFilterNotContainsOperator, TcxFilterBeginsWithOperator, TcxFilterEndsWithOperator);
  ANullOperatorClasses: array[Boolean] of TcxFilterOperatorClass = (
    TcxFilterNullOperator, TcxFilterNotNullOperator);
begin
  if (OperatorKind in [foEqual, foNotEqual, foLike, foNotLike, foContains..foEndsWith]) and (ValueIsNull(Value)) then
    Result := ANullOperatorClasses[OperatorKind in [foNotEqual, foNotLike, foNotContains]]
  else
    Result := AOperatorClasses[OperatorKind];
end;

function TcxFilterCriteriaItem.GetItemLink: TObject;
begin
  Result := FItemLink;
end;

procedure TcxFilterCriteriaItem.SetItemLink(Value: TObject);
begin
  FItemLink := Value;
end;

function TcxFilterCriteriaItem.GetIsItemList: Boolean;
begin
  Result := False;
end;

procedure TcxFilterCriteriaItem.RecreateOperator;
var
  AIsConstruction: Boolean;
begin
  AIsConstruction := FOperator = nil;
  FOperator.Free;
  FOperator := GetFilterOperatorClass.Create(Self);
  if not AIsConstruction then
    Changed;
end;

procedure TcxFilterCriteriaItem.ReadData(AStream: TStream);

  function FindItemLink(const AName: string; AID: Integer): TObject;
  begin
    if AName = '' then
      Result := Criteria.GetItemLinkByID(AID)
    else
    begin
      Result := Criteria.GetItemLinkByName(AName);
      if Result = nil then
      begin
        Result := Criteria.GetItemLinkByID(AID);
        if (Result <> nil) and (Criteria.GetNameByItemLink(Result) <> '') then
          Result := nil;
      end;
    end;
  end;

var
  AItemLinkID: Integer;
  AItemLinkName: string;
  AdxStream: TdxStream;
begin
  inherited;
  AdxStream := TdxStream.Create(AStream);
  try
    OperatorKind := TcxFilterOperatorKind(ReadByteFunc(AStream));
    if AdxStream.IsUnicode then
      DisplayValue := ReadWideStringFunc(AStream)
    else
      DisplayValue := dxAnsiStringToString(ReadAnsiStringFunc(AStream));
    AItemLinkID := ReadIntegerFunc(AStream);
    if Criteria.LoadedVersion >= 3 then
    begin
      if AdxStream.IsUnicode then
        AItemLinkName := ReadWideStringFunc(AStream)
      else
        AItemLinkName := dxAnsiStringToString(ReadAnsiStringFunc(AStream))
    end
    else
      AItemLinkName := '';

    Value := ReadVariantFunc(AStream);
  finally
    AdxStream.Free;
  end;
  SetItemLink(FindItemLink(AItemLinkName, AItemLinkID));
  CheckDisplayValue;
end;

function TcxFilterCriteriaItem.SupportsMultiThreading: Boolean;
begin
  Result := True;
end;

procedure TcxFilterCriteriaItem.Prepare;
begin
  PrepareValue;
  Operator.Prepare;
end;

procedure TcxFilterCriteriaItem.PrepareValue;
begin
  FPreparedValue := Criteria.PrepareValue(Value);
end;

function TcxFilterCriteriaItem.CompareByDisplayValue: Boolean;
begin
  Result := Criteria.CompareByDisplayValues;
end;

procedure TcxFilterCriteriaItem.WriteData(AStream: TStream);
begin
  inherited WriteData(AStream);
  dxWriteStreamType(AStream);
  WriteByteProc(AStream, Byte(OperatorKind));
  if Criteria.IsUnicode then
    WriteWideStringProc(AStream, DisplayValue)
  else
    WriteAnsiStringProc(AStream, dxStringToAnsiString(DisplayValue));
  WriteIntegerProc(AStream, Criteria.GetIDByItemLink(ItemLink));
  if Criteria.SavedVersion >= 3 then
  begin
    if Criteria.IsUnicode then
      WriteWideStringProc(AStream, Criteria.GetNameByItemLink(ItemLink))
    else
      WriteAnsiStringProc(AStream, dxStringToAnsiString(Criteria.GetNameByItemLink(ItemLink)));
  end;
  WriteVariantProc(AStream, Value);
end;

procedure TcxFilterCriteriaItem.SetOperatorKind(Value: TcxFilterOperatorKind);
begin
  if FOperatorKind <> Value then
  begin
    FOperatorKind := Value;
    RecreateOperator;
  end;
end;

function TcxFilterCriteriaItem.Compare(AData: TObject): Boolean;
var
  AValue, ADataValue: Variant;
begin
  try
    ADataValue := GetDataValue(AData);
    AValue := Criteria.PrepareValue(ADataValue);
    // for Null special compare
    if (fcoIgnoreNull in Criteria.Options) and
      not Operator.IsNullOperator and ValueIsNull(AValue) then
      Result := False
    else
      Result := Operator.CompareValues(AValue, PreparedValue);
  except
    on EVariantError do
      if fcoSoftCompare in Criteria.Options then
        Result := False
      else
        raise;
  end;
end;

procedure TcxFilterCriteriaItem.InternalSetValue(const Value: Variant);
begin
  FValue := Value;
  PrepareValue;
  RecreateOperator;
end;

procedure TcxFilterCriteriaItem.SetDisplayValue(const Value: string);
begin
  if FDisplayValue <> Value then
  begin
    FDisplayValue := Value;
    Changed;
  end;
end;

procedure TcxFilterCriteriaItem.SetValue(const Value: Variant);
begin
  if VarCompare(FValue, Value) <> 0 then
    InternalSetValue(Value);
end;

{ TcxFilterValueItem }

constructor TcxFilterValueItem.Create(AOwner: TcxFilterValueList;
  AKind: TcxFilterValueItemKind; const AValue: Variant;
  const ADisplayText: string);
begin
  FOwner := AOwner;
  FKind := AKind;
  FValue := AValue;
  FDisplayText := ADisplayText;
  Prepare;
end;

function TcxFilterValueItem.ComparePreparedValue(const APreparedValue: Variant;
  const APreparedDisplayText: string): Integer;
begin
  if FOwner.SortByDisplayText then
  begin
    Result := AnsiCompareStr(PreparedDisplayText, APreparedDisplayText);
    if Result <> 0 then Exit;
  end;
  Result := VarCompare(PreparedValue, APreparedValue);
end;

procedure TcxFilterValueItem.Prepare;
begin
  FPreparedValue := Criteria.PrepareValue(FValue);
  FPreparedDisplayText := Criteria.PrepareDisplayText(FDisplayText)
end;

function TcxFilterValueItem.GetCriteria: TcxFilterCriteria;
begin
  Result := FOwner.Criteria;
end;

{ TcxGridFilterMRUValueItem }

constructor TcxFilterMRUValueItem.Create(const AValue: Variant; const ADisplayText: string);
begin
  inherited Create;
  Value := AValue;
  DisplayText := ADisplayText;
end;

function TcxFilterMRUValueItem.Equals(AItem: TcxMRUItem): Boolean;
begin
  Result := VarCompare(Value, TcxFilterMRUValueItem(AItem).Value) = 0;
end;

{ TcxGridFilterMRUValueItems }

function TcxFilterMRUValueItems.GetItem(Index: Integer): TcxFilterMRUValueItem;
begin
  Result := TcxFilterMRUValueItem(inherited Items[Index]);
end;

procedure TcxFilterMRUValueItems.Add(const AValue: Variant; const ADisplayText: string);
begin
  inherited Add(TcxFilterMRUValueItem.Create(AValue, ADisplayText));
end;

procedure TcxFilterMRUValueItems.AddItemsTo(AValueList: TcxFilterValueList);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    with Items[I] do
      AValueList.Add(fviMRU, Value, DisplayText, True);
end;

{ TcxFilterValueList }

constructor TcxFilterValueList.Create(ACriteria: TcxFilterCriteria);
begin
  inherited Create;
  FCriteria := ACriteria;
  FItems := TdxFastObjectList.Create;
end;

destructor TcxFilterValueList.Destroy;
begin
  FreeAndNil(FItems);
  inherited Destroy;
end;

procedure TcxFilterValueList.Add(AKind: TcxFilterValueItemKind; const AValue: Variant;
  const ADisplayText: string; ANoSorting: Boolean);
begin
  Add(AKind, AValue, ADisplayText, ANoSorting, True);
end;

procedure TcxFilterValueList.Clear;
begin
  FItems.Clear;
  FIsUnsortedValues := False;
end;

procedure TcxFilterValueList.Delete(AIndex: Integer);
begin
  FItems.Delete(AIndex);
end;

function TcxFilterValueList.Find(const AValue: Variant; const ADisplayText: string;
  var AIndex: Integer): Boolean;
var
  I, AMRUSeparatorIndex: Integer;
  APreparedValue: Variant;
  APreparedDisplayText: string;
begin
  APreparedValue := Criteria.PrepareValue(AValue);
  APreparedDisplayText := Criteria.PrepareDisplayText(ADisplayText);
  // MRU
  AMRUSeparatorIndex := GetMRUSeparatorIndex;
  if AMRUSeparatorIndex <> -1 then
    for I := 0 to AMRUSeparatorIndex - 1 do
      if CompareItem(I, APreparedValue, APreparedDisplayText) = 0 then
      begin
        AIndex := I;
        Result := True;
        Exit;
      end;
  Result := FindValueIndex(APreparedValue, APreparedDisplayText, AIndex);
end;

function TcxFilterValueList.FindItemByKind(AKind: TcxFilterValueItemKind): Integer;
begin
  Result := FindItemByKind(AKind, Null);
end;

function TcxFilterValueList.FindItemByKind(AKind: TcxFilterValueItemKind;
  const AValue: Variant): Integer;
begin
  for Result := 0 to Count - 1 do
    if (Items[Result].Kind = AKind) and
      (VarIsNull(AValue) or VarEquals(Items[Result].Value, AValue)) then
      Exit;
  Result := -1;
end;

function TcxFilterValueList.FindItemByValue(const AValue: Variant): Integer;
var
  ASaveSortByDisplayText: Boolean;
begin
  ASaveSortByDisplayText := SortByDisplayText;
  try
    SortByDisplayText := False;
    if not FindValueIndex(Criteria.PrepareValue(AValue), '', Result) then
      Result := -1;
  finally
    SortByDisplayText := ASaveSortByDisplayText;
  end;
end;

function TcxFilterValueList.GetIndexByCriteriaItem(ACriteriaItem: TcxFilterCriteriaItem): Integer;
begin
  if ACriteriaItem = nil then
    Result := FindItemByKind(fviAll)
  else
    if ACriteriaItem.ValueIsNull(ACriteriaItem.Value) and
      (ACriteriaItem.OperatorKind in [foEqual, foNotEqual]) then
      if ACriteriaItem.OperatorKind = foEqual then
        Result := FindItemByKind(fviBlanks)
      else
        Result := FindItemByKind(fviNonBlanks)
    else
      if not ({(ACriteriaItem.Parent = Criteria.Root) and }(ACriteriaItem.OperatorKind = foEqual) and
        Find(ACriteriaItem.Value, ACriteriaItem.DisplayValue, Result)) then
        Result := FindItemByKind(fviCustom);
end;

function TcxFilterValueList.FindValueIndex(const APreparedValue: Variant; const APreparedDisplayText: string; out AIndex: Integer): Boolean;
var
  L, I, H, C, AStartIndex: Integer;
begin
  Result := False;
  AStartIndex := StartValueIndex;
  L := AStartIndex;
  H := Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := CompareItem(I, APreparedValue, APreparedDisplayText);
    if C < 0 then
      L := I + 1
    else
    begin
      if C = 0 then
      begin
        Result := True;
        L := I;
      end;
      H := I - 1;
    end;
  end;
  AIndex := L;
  // values are not sorted
  if not Result and FIsUnsortedValues then
    for I := AStartIndex to Count - 1 do
    begin
      Result := CompareItem(I, APreparedValue, APreparedDisplayText) = 0;
      if Result then
      begin
        AIndex := I;
        Break;
      end;
    end;
end;

procedure TcxFilterValueList.Add(AKind: TcxFilterValueItemKind; const AValue: Variant; const ADisplayText: string;
  ANoSorting: Boolean; AUniqueOnly: Boolean);
var
  AIndex: Integer;
begin
  AIndex := -1;
  if AKind = fviMRU then
  begin
    AIndex := GetMRUSeparatorIndex;
    if AIndex = -1 then // first MRU item
    begin
      AIndex := 0;
      // add MRU Separator
      FItems.Insert(AIndex, CreateItem(fviMRUSeparator, Null, ''));
    end;
  end
  else
    if AKind <> fviValue then
      AIndex := StartValueIndex
    else
      if ANoSorting then
      begin
        AIndex := Count;
        FIsUnsortedValues := True;
      end
      else
        if ((MaxCount = 0) or (Count < MaxCount)) and Find(AValue, ADisplayText, AIndex) and AUniqueOnly then
          AIndex := -1;
  if AIndex <> -1 then
    FItems.Insert(AIndex, CreateItem(AKind, AValue, ADisplayText));
end;

function TcxFilterValueList.CompareItem(AIndex: Integer; const APreparedValue: Variant;
  const APreparedDisplayText: string): Integer;
begin
  Result := Items[AIndex].ComparePreparedValue(APreparedValue, APreparedDisplayText);
end;

function TcxFilterValueList.CreateItem(AKind: TcxFilterValueItemKind;
  const AValue: Variant; const ADisplayText: string): TcxFilterValueItem;
begin
  Result := TcxFilterValueItem.Create(Self, AKind, AValue, ADisplayText);
end;

function TcxFilterValueList.GetMRUSeparatorIndex: Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Count - 1 do
  begin
    if Items[I].Kind = fviMRUSeparator then
    begin
      Result := I;
      Break;
    end;
    if Items[I].Kind <> fviMRU then
      Break;
  end;
end;

function TcxFilterValueList.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TcxFilterValueList.GetItem(Index: Integer): TcxFilterValueItem;
begin
  Result := TcxFilterValueItem(FItems[Index]);
end;

function TcxFilterValueList.GetMaxCount: Integer;
begin
  Result := Criteria.MaxValueListCount;
end;

function TcxFilterValueList.GetStartValueIndex: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to FItems.Count - 1 do
    if Items[I].Kind <> fviValue then
      Inc(Result)
    else
      Break;
end;

{ TcxFilterCriteria }

constructor TcxFilterCriteria.Create;
begin
  inherited Create;
  FRoot := GetRootClass.Create(nil, fboAnd);
  FRoot.FCriteria := Self;
  FEscapeWildcard := #0;
  FPercentWildcard := '%';
  FSupportedLike := True;
  FUnderscoreWildcard := '_';
  FVersion := 2;
  FIsUnicode := True;
  InitializeCriticalSectionAndSpinCount(FCriticalSection, 512);
end;

destructor TcxFilterCriteria.Destroy;
begin
  FRoot.Free;
  FRoot := nil;
  DeleteCriticalSection(FCriticalSection);
  inherited Destroy;
end;

procedure TcxFilterCriteria.Assign(Source: TPersistent; AIgnoreItemNames: Boolean = False);
begin
  if Source is TcxFilterCriteria then
  begin
    BeginUpdate;
    try
      DateTimeFormat := TcxFilterCriteria(Source).DateTimeFormat;
      MaxValueListCount := TcxFilterCriteria(Source).MaxValueListCount;
      Options := TcxFilterCriteria(Source).Options;
      PercentWildcard := TcxFilterCriteria(Source).PercentWildcard;
      TranslateBetween := TcxFilterCriteria(Source).TranslateBetween;
      TranslateLike := TcxFilterCriteria(Source).TranslateLike;
      TranslateIn := TcxFilterCriteria(Source).TranslateIn;
      SupportedLike := TcxFilterCriteria(Source).SupportedLike;
      UnderscoreWildcard := TcxFilterCriteria(Source).UnderscoreWildcard;
      AssignItems(TcxFilterCriteria(Source), AIgnoreItemNames);
      AssignEvents(Source); // TODO: option?
    finally
      EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;

procedure TcxFilterCriteria.AssignEvents(Source: TPersistent);
begin
  if Source is TcxFilterCriteria then
    OnChanged := TcxFilterCriteria(Source).OnChanged;
end;

procedure TcxFilterCriteria.AssignItems(ASource: TcxFilterCriteria;
  AIgnoreItemNames: Boolean = False);
var
  AStream: TMemoryStream;
  ASourceVersion: Byte;
begin
  if EqualItems(ASource, AIgnoreItemNames) then Exit;
  BeginUpdate;
  try
    Clear;
    AStream := TMemoryStream.Create;
    try
      ASourceVersion := ASource.Version;
      if AIgnoreItemNames then
        ASource.Version := Min(ASource.Version, 2);
      ASource.WriteData(AStream, Version);
      ASource.Version := ASourceVersion;
      AStream.Position := 0;
      ReadData(AStream);
    finally
      AStream.Free;
    end;
  finally
    EndUpdate;
  end;
end;

function TcxFilterCriteria.AddItem(AParent: TcxFilterCriteriaItemList; AItemLink: TObject;
  AOperatorKind: TcxFilterOperatorKind; const AValue: Variant;
  const ADisplayValue: string): TcxFilterCriteriaItem;
begin
  if AParent = nil then
    AParent := Root;
  Result := AParent.AddItem(AItemLink, AOperatorKind, AValue, ADisplayValue);
end;

procedure TcxFilterCriteria.BeginUpdate;
begin
  Inc(FLockCount);
end;

procedure TcxFilterCriteria.CancelUpdate;
begin
  Dec(FLockCount);
  // obsolete - use EndUpdate
end;

procedure TcxFilterCriteria.Clear;
begin
  BeginUpdate;
  try
    Root.Clear;
    Root.BoolOperatorKind := fboAnd;
  finally
    EndUpdate;
  end;
end;

procedure TcxFilterCriteria.Changed;
begin
  FChanged := True;
  CheckChanges;
end;

procedure TcxFilterCriteria.EndUpdate;
begin
  Dec(FLockCount);
  CheckChanges;
end;

function TcxFilterCriteria.EqualItems(AFilterCriteria: TcxFilterCriteria;
  AIgnoreItemNames: Boolean = False): Boolean;
var
  AMaxVersion: Byte;
  AStream1, AStream2: TMemoryStream;
begin
  if AIgnoreItemNames then
    AMaxVersion := 2
  else
    AMaxVersion := 3;
  AStream1 := TMemoryStream.Create;
  AStream2 := TMemoryStream.Create;
  try
    WriteData(AStream1, Min(AFilterCriteria.Version, AMaxVersion));
    AFilterCriteria.WriteData(AStream2, Min(Version, AMaxVersion));
    Result := StreamsEqual(AStream1, AStream2);
  finally
    AStream2.Free;
    AStream1.Free;
  end;
end;

function TcxFilterCriteria.FindItemByItemLink(AItemLink: TObject): TcxFilterCriteriaItem;

  procedure FindInBranch(ABranch: TcxFilterCriteriaItemList);
  var
    I: Integer;
  begin
    for I := 0 to ABranch.Count - 1 do
    begin
      if Result <> nil then
        Break;
      if TcxCustomFilterCriteriaItem(ABranch.Items[I]).IsItemList then
        FindInBranch(TcxFilterCriteriaItemList(ABranch.Items[I]))
      else
        if TcxFilterCriteriaItem(ABranch.Items[I]).FItemLink = AItemLink then // TODO: !!!ItemLink
        begin
          Result := TcxFilterCriteriaItem(ABranch.Items[I]);
          Break;
        end;
    end;
  end;

begin
  Result := nil;
  FindInBranch(Root);
end;

function TcxFilterCriteria.IsEmpty: Boolean;
begin
  Result := Root.IsEmpty;
end;

procedure TcxFilterCriteria.LoadFromStream(AStream: TStream);
var
  AWrapper: TFilterWrapper;
begin
  BeginUpdate;
  try
    AWrapper := TFilterWrapper.Create(nil);
    try
      AWrapper.Filter := Self;
      AStream.ReadComponent(AWrapper);
    finally
      AWrapper.Free;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TcxFilterCriteria.Refresh;

  procedure RecreateOperators(ABranch: TcxFilterCriteriaItemList);
  var
    I: Integer;
  begin
    for I := 0 to ABranch.Count - 1 do
      if ABranch[I].IsItemList then
        RecreateOperators(TcxFilterCriteriaItemList(ABranch[I]))
      else
        TcxFilterCriteriaItem(ABranch[I]).RecreateOperator;
  end;

begin
  BeginUpdate;
  try
    RecreateOperators(Root);
  finally
    EndUpdate;
  end;
end;

procedure TcxFilterCriteria.RemoveItemByItemLink(AItemLink: TObject);

  procedure CheckEmptyParent(AParent: TcxFilterCriteriaItemList);
  var
    ANewParent: TcxFilterCriteriaItemList;
  begin
    if AParent = FRoot then Exit;
    if AParent.IsEmpty then
    begin
      ANewParent := AParent.Parent;
      AParent.Free;
      CheckEmptyParent(ANewParent);
    end;
  end;

  procedure CheckEmptyList(AParent: TcxFilterCriteriaItemList);
  var
    ANewParent: TcxFilterCriteriaItemList;
    AItem: TcxCustomFilterCriteriaItem;
  begin
    if AParent = FRoot then Exit;
    if AParent.IsEmpty then
    begin
      ANewParent := AParent.Parent;
      AParent.Free;
      CheckEmptyParent(ANewParent);
    end
    else
      if AParent.Count = 1 then
      begin
        ANewParent := AParent.Parent;
        AItem := AParent.Items[0];
        ANewParent.FItems.Remove(AParent);
        ANewParent.FItems.Add(AItem);
        AItem.FParent := ANewParent;
        AParent.FItems.Clear;
        AParent.Free;
      end;
  end;

var
  AItem: TcxFilterCriteriaItem;
  AParent: TcxFilterCriteriaItemList;
  AChanged: Boolean;
begin
  BeginUpdate;
  AChanged := False;
  try
    AItem := FindItemByItemLink(AItemLink);
    while AItem <> nil do
    begin
      AChanged := True;
      AParent := AItem.Parent;
      AItem.Free;
      CheckEmptyList(AParent);
      AItem := FindItemByItemLink(AItemLink);
    end;
  finally
    if AChanged then
      EndUpdate
    else
      CancelUpdate;
  end;
end;

procedure TcxFilterCriteria.RestoreDefaults;
begin
  BeginUpdate;
  try
    Options := [];
    PercentWildcard := '%';
    SupportedLike := True;
    TranslateBetween := False;
    TranslateLike := False;
    TranslateIn := False;
    UnderscoreWildcard := '_';
  finally
    EndUpdate;
  end;
end;

procedure TcxFilterCriteria.SaveToStream(AStream: TStream);
var
  AWrapper: TFilterWrapper;
begin
  AWrapper := TFilterWrapper.Create(nil);
  FSavingToStream := True;
  try
    AWrapper.Filter := Self;
    AStream.WriteComponent(AWrapper);
  finally
    FSavingToStream := False;
    AWrapper.Free;
  end;
end;

function GetFilterVersion(AVersionID: Integer): Byte;
begin
  case AVersionID of
    -1: Result := 2;
    -2: Result := 3;
  else
    Result := 1;
  end;
end;

function GetFilterVersionID(AVersion: Byte): Integer;
begin
  case AVersion of
    2: Result := -1;
    3: Result := -2;
  else
    Result := 0;  // undefined
  end;
end;

procedure TcxFilterCriteria.ReadData(AStream: TStream);
var
  AItemCount, I: Integer;
begin
  BeginUpdate;
  try
    Clear;
    AItemCount := ReadIntegerFunc(AStream);
    FLoadedVersion := GetFilterVersion(AItemCount);
    if LoadedVersion > 1 then  // new format
      Root.ReadData(AStream)
    else
      for I := 0 to AItemCount - 1 do
        Root.ReadItem(AStream);
  finally
    EndUpdate;
  end;
end;

procedure TcxFilterCriteria.WriteData(AStream: TStream);
begin
  WriteData(AStream, 0);
end;

procedure TcxFilterCriteria.WriteData(AStream: TStream; AVersion: Byte);
begin
  if AVersion = 0 then
    FSavedVersion := Version
  else
    FSavedVersion := Min(Version, AVersion);
  WriteIntegerProc(AStream, GetFilterVersionID(SavedVersion));  // new format
  Root.WriteData(AStream);
end;

function TcxFilterCriteria.ValueIsNull(const AValue: Variant): Boolean;
begin
  if fcoSoftNull in Options then
    Result := VarIsSoftNull(AValue)
  else
    Result := VarIsNull(AValue);
end;

procedure TcxFilterCriteria.CheckChanges;
begin
  if FChanged and (LockCount = 0) then
  begin
    FChanged := False;
    Prepare;
    Update;
  end;
end;

function TcxFilterCriteria.ConvertBoolToStr(const AValue: Variant): string;
begin
  Result := VarToStr(AValue);
end;

function TcxFilterCriteria.ConvertDateToStr(const AValue: Variant): string;
begin
  if DateTimeFormat <> '' then
    Result := FormatDateTime(DateTimeFormat, AValue)
  else
    Result := VarToStr(AValue)
end;

function TcxFilterCriteria.DoFilterData(AData: TObject): Boolean;
begin
  Result := Root.DoFilterData(AData);
end;

function TcxFilterCriteria.FilterTextUsed: Boolean;
begin
  Result := True;
end;

procedure TcxFilterCriteria.FormatFilterTextValue(AItem: TcxFilterCriteriaItem;
  const AValue: Variant; var ADisplayValue: string);
begin
end;

function TcxFilterCriteria.GetFilterCaption: string;
begin
  Result := GetFilterExpression(True);
end;

function TcxFilterCriteria.GetFilterExpression(AIsCaption: Boolean): string;

  function GetOperatorText(AIsCaption: Boolean;
    ABoolOperatorKind: TcxFilterBoolOperatorKind): string;
  begin
    case ABoolOperatorKind of
      fboAnd, fboNotAnd:
        begin
          if AIsCaption then
            Result := cxSFilterString(@cxSFilterAndCaption)
          else
            Result := 'AND';
        end;
      fboOr, fboNotOr:
        begin
          if AIsCaption then
            Result := cxSFilterString(@cxSFilterOrCaption)
          else
            Result := 'OR';
        end;
    end;
  end;

  function GetNotText(AIsCaption: Boolean): string;
  begin
    if AIsCaption then
      Result := cxSFilterString(@cxSFilterNotCaption)
    else
      Result := 'NOT';
  end;

  function FilterSubString(ABranch: TcxFilterCriteriaItemList): string;
  var
    I: Integer;
  begin
    Result := '';
    for I := 0 to ABranch.Count - 1 do
    begin
      if ABranch[I].IsItemList then
        Result := Result + '(' + FilterSubString(TcxFilterCriteriaItemList(ABranch[I])) + ')'
      else
        Result := Result + '(' + GetItemExpression(TcxFilterCriteriaItem(ABranch[I]), AIsCaption) + ')';
      if I < ABranch.Count - 1 then
        Result := Result + ' ' + GetOperatorText(AIsCaption, ABranch.BoolOperatorKind) + ' ';
    end;
    if ABranch.BoolOperatorKind in [fboNotAnd, fboNotOr] then
      Result := GetNotText(AIsCaption) + ' (' + Result + ')';
  end;

begin
  Result := FilterSubString(Root);
end;

function TcxFilterCriteria.GetFilterText: string;
begin
  Result := GetFilterExpression(False);
end;

function TcxFilterCriteria.GetItemClass: TcxFilterCriteriaItemClass;
begin
  Result := TcxFilterCriteriaItem;
end;

function TcxFilterCriteria.GetItemListClass: TcxFilterCriteriaItemListClass;
begin
  Result := TcxFilterCriteriaItemList;
end;

function TcxFilterCriteria.GetItemExpression(AItem: TcxFilterCriteriaItem;
  AIsCaption: Boolean): string;
var
  S: string;
begin
  if not AIsCaption and AItem.Operator.IsExpression then
    Result := AItem.Operator.GetExpressionFilterText(AItem.Value)
  else
  begin
    S := GetItemExpressionValue(AItem, AIsCaption);
    Result := GetItemExpressionFieldName(AItem, AIsCaption) + ' ' +
      GetItemExpressionOperator(AItem, AIsCaption);
    if S <> '' then
      Result := Result + ' ' + S;
  end;
end;

function TcxFilterCriteria.GetItemExpressionFieldName(AItem: TcxFilterCriteriaItem;
  AIsCaption: Boolean): string;
begin
  if AIsCaption then
    Result := AItem.GetFieldCaption
  else
    Result := AItem.GetFieldName;
end;

function TcxFilterCriteria.GetItemExpressionOperator(AItem: TcxFilterCriteriaItem;
  AIsCaption: Boolean): string;
begin
  if AIsCaption and (AItem.Operator.IsDescription or (fcoShowOperatorDescription in Options)) then
    Result := AItem.Operator.DisplayText
  else
    Result := AItem.Operator.FilterText;
end;

function TcxFilterCriteria.GetItemExpressionValue(AItem: TcxFilterCriteriaItem;
  AIsCaption: Boolean): string;
begin
  Result := AItem.GetExpressionValue(AIsCaption);
end;

function TcxFilterCriteria.GetRootClass: TcxFilterCriteriaItemListClass;
begin
  Result := TcxFilterCriteriaItemList;
end;

function TcxFilterCriteria.IsStore: Boolean;
begin
  Result := FSavingToStream or (Root.BoolOperatorKind <> fboAnd) or not Root.IsEmpty;
end;

procedure TcxFilterCriteria.Lock;
begin
  EnterCriticalSection(FCriticalSection);
end;

procedure TcxFilterCriteria.Prepare;

  procedure PrepareOperators(ABranch: TcxFilterCriteriaItemList);
  var
    I: Integer;
  begin
    for I := 0 to ABranch.Count - 1 do
      if ABranch[I].IsItemList then
        PrepareOperators(TcxFilterCriteriaItemList(ABranch[I]))
      else
        TcxFilterCriteriaItem(ABranch[I]).Prepare;
  end;

begin
  PrepareOperators(Root);
end;

function TcxFilterCriteria.PrepareDisplayText(const ADisplayText: string): string;
begin
  if fcoCaseInsensitive in Options then
    Result := AnsiUpperCase(ADisplayText)
  else
    Result := ADisplayText;
end;

function TcxFilterCriteria.PrepareValue(const AValue: Variant): Variant;
var
  I: Integer;
begin
  if VarType(AValue) = varCurrency then
    Result := VarAsType(AValue, varDouble) // bug in Delphi
  else
    if VarIsStr(AValue) and (fcoCaseInsensitive in Options) then
      Result := AnsiUpperCase(AValue)
    else
    begin
      Result := AValue;
      if VarIsArray(AValue) then
      begin
        Lock;
        try
          for I := VarArrayLowBound(Result, 1) to VarArrayHighBound(Result, 1) do
            Result[I] := PrepareValue(Result[I]);
        finally
          Unlock;
        end;
      end;
    end;
end;

function TcxFilterCriteria.GetValueListClass: TcxFilterValueListClass;
begin
  Result := TcxFilterValueList;
end;

procedure TcxFilterCriteria.SetFilterText(const Value: string);
begin
end;

function TcxFilterCriteria.SupportsMultiThreading: Boolean;

  procedure CheckBranch(ABranch: TcxFilterCriteriaItemList; var ASupports: Boolean);
  var
    I: Integer;
  begin
    for I := 0 to ABranch.Count - 1 do
    begin
      if not ASupports then
        Break;
      if ABranch[I].IsItemList then
        CheckBranch(TcxFilterCriteriaItemList(ABranch[I]), ASupports)
      else
        ASupports := TcxFilterCriteriaItem(ABranch[I]).SupportsMultiThreading;
    end;
  end;

begin
  Result := True;
  CheckBranch(Root, Result)
end;

procedure TcxFilterCriteria.Unlock;
begin
  LeaveCriticalSection(FCriticalSection);
end;

procedure TcxFilterCriteria.Update;
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

function TcxFilterCriteria.GetOptions: TcxFilterCriteriaOptions;
begin
  Result := FOptions;
end;

function TcxFilterCriteria.GetStoreItemLinkNames: Boolean;
begin
  Result := Version >= 3;
end;

procedure TcxFilterCriteria.SetDateTimeFormat(const Value: string);
begin
  if FDateTimeFormat <> Value then
  begin
    FDateTimeFormat := Value;
    Changed;
  end;
end;

procedure TcxFilterCriteria.SetOptions(Value: TcxFilterCriteriaOptions);
begin
  if FOptions <> Value then
  begin
    FOptions := Value;
    Changed;
  end;
end;

procedure TcxFilterCriteria.SetPercentWildcard(Value: Char);
begin
  if FPercentWildcard <> Value then
  begin
    FPercentWildcard := Value;
    Changed;
  end;
end;

procedure TcxFilterCriteria.SetStoreItemLinkNames(Value: Boolean);
begin
  if StoreItemLinkNames <> Value then
    if Value then
      Version := 3
    else
      Version := 2;
end;

procedure TcxFilterCriteria.SetUnderscoreWildcard(Value: Char);
begin
  if FUnderscoreWildcard <> Value then
  begin
    FUnderscoreWildcard := Value;
    Changed;
  end;
end;

end.
