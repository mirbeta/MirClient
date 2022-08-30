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

unit dxEMF.Types;

{$I cxVer.inc}
{$I dxEMF.inc}

interface

uses
  Classes, SysUtils, Rtti, Variants;

type

  TdxPredicate = reference to function(AObject: TObject): Boolean;
  TdxPredicate<T> = reference to function(AData: T): Boolean;

  TdxFunc<TResult> = reference to function: TResult;
  TdxFunc<T, TResult> = reference to function (const AArg1: T): TResult;
  TdxFunc<T1, T2, TResult> = reference to function (const AArg1: T1; const AArg2: T2): TResult;
  TdxFunc<T1, T2, T3, TResult> = reference to function (const AArg1: T1; const AArg2: T2; const AArg3: T3): TResult;
  TdxFunc<T1, T2, T3, T4, TResult> = reference to function (const AArg1: T1; const AArg2: T2; const AArg3: T3; const AArg4: T4): TResult;

  IdxQueryProvider = interface;
  {$HPPEMIT 'template<typename T> __interface IdxQueryProvider__1;'}
  IdxQueryProvider<T> = interface;

  { TdxLoadingStrategy }

  TdxLoadingStrategy = (
    Lazy,
    Eager
  );

  { IdxSession }

  IdxSession = interface
  ['{AF50681A-0275-4E82-97B8-C02BC3DE4573}']
    function GetLoadingStrategy: TdxLoadingStrategy;
    property LoadingStrategy: TdxLoadingStrategy read GetLoadingStrategy;
  end;

  { IdxLinq }

  IdxLinq = interface
  ['{6773E7C7-6460-4B0F-AA5E-08B9A6D15066}']
  end;

  { IdxExpression }

  IdxExpression = interface
  ['{F2F95129-603D-4E27-8EC2-5EBBD5F87450}']
  end;

  { IdxQueryable }

  IdxQueryable = interface(IEnumerable)
  ['{1B20845B-236C-4EA4-BB03-5697C0DA9AEA}']
    function GetProvider: IdxQueryProvider;
    property Provider: IdxQueryProvider read GetProvider;
  end;

  { IdxQueryable<T> }

  IdxQueryable<T> = interface(IEnumerable<T>)
    function GetProvider: IdxQueryProvider<T>;
    property Provider: IdxQueryProvider<T> read GetProvider;
  end;

  { IdxQueryProvider }

  IdxQueryProvider = interface
  ['{4C03DD99-D728-4A91-AA30-5C8ABB264912}']
    function CreateQuery(const AExpression: IdxExpression): IdxQueryable;
  end;

  { IdxQueryProvider<T> }

  IdxQueryProvider<T> = interface(IdxQueryProvider)
    function CreateQuery(const AExpression: IdxExpression): IdxQueryable<T>;
  end;

  { IdxCollection }

  IdxCollection = interface(IEnumerable)
  ['{D0E59BAC-699F-4D75-885C-9A2909CF1634}']
    function GetCount: Integer;
    property Count: Integer read GetCount;
  end;

  { IdxCollection<T> }

  IdxCollection<T> = interface(IEnumerable<T>)
    function GetCount: Integer;
    function Add(const AValue: T): Integer;
    procedure Remove(const AValue: T);
    function Contains(const AValue: T): Boolean;
    property Count: Integer read GetCount;
  end;

  { IdxList }

  IdxList = interface(IdxCollection)
  ['{A7977263-A6EC-4074-8B3C-16D6BBD40B2E}']
    function GetItems(AIndex: Integer): TObject;
    function Add(AValue: TObject): Integer;
    function Contains(AValue: TObject): Boolean;
    function IndexOf(AObject: TObject): Integer;
    procedure Remove(AValue: TObject);
    property Items[AIndex: Integer]: TObject read GetItems; default;
  end;

  { IdxList<T> }

  IdxList<T> = interface(IdxCollection<T>)
    function GetItems(AIndex: Integer): T;
    function IndexOf(const AItem: T): Integer;
    property Items[AIndex: Integer]: T read GetItems; default;
  end;

  { IdxEMFCollection }

  IdxEMFCollection = interface(IdxList)
    ['{7702EDCA-BCED-42C6-B166-2170B4F6B59B}']
    function GetDeleteObjectOnRemove: Boolean;
    procedure SetDeleteObjectOnRemove(AValue: Boolean);

    function First: TObject;
    function Last: TObject;
    property DeleteObjectOnRemove: Boolean read GetDeleteObjectOnRemove write SetDeleteObjectOnRemove;
  end;

  { IdxEMFCollection<T> }

  IdxEMFCollection<T> = interface(IdxList<T>)
    function GetDeleteObjectOnRemove: Boolean;
    procedure SetDeleteObjectOnRemove(AValue: Boolean);
    function First: T;
    function Last: T;
    function Find(const AKey: TValue): T; overload;
    function Find(APredicate: TdxPredicate<T>): T; overload;
    function GetObjects(APredicate: TdxPredicate<T>): TArray<T>; overload;
    property DeleteObjectOnRemove: Boolean read GetDeleteObjectOnRemove write SetDeleteObjectOnRemove;
  end;


  { IdxEntityMemberHolder }

  IdxEntityMemberHolder = interface
  ['{8A151AE6-C8BB-4620-9BC7-A842ECE1C0B7}']
    function AsVariant: Variant;
    function GetMemberName: string;
    property MemberName: string read GetMemberName;
  end;

  { TdxSortDirection }

  TdxSortDirection = (
    Ascending,
    Descending
  );

  { IdxSortByExpression }

  IdxSortByExpression = interface
  ['{69FE6D49-FA21-4C48-AC2D-4A99D8DD2789}']
    function GetExpression: IdxExpression;
    function GetSortDirection: TdxSortDirection;
    property Expression: IdxExpression read GetExpression;
    property SortDirection: TdxSortDirection read GetSortDirection;
  end;

  { IdxSortByExpressions }

  IdxSortByExpressions = interface(IEnumerable<IdxSortByExpression>)
  ['{560F3B8D-ACF8-44CD-81D1-FC544FFE8B6E}']
//    [HPPGEN('HIDESBASE virtual System::DelphiInterface<IFuture__1<T> > __fastcall StartFuture() = 0')]
//    [HPPGEN('virtual System::DelphiInterface<IEnumerator__1<T> > __fastcall GetEnumeratorT(void) = 0')]
  end;

  { TdxDBEngine }

  TdxDBEngine = type string;

  TdxDBEngines = class
  public const
    Empty = '';
    Unknown = 'Unknown';
    Firebird = 'Firebird';
    MySQL = 'MySQL';
    MSSQL = 'MSSQLServer';
    Oracle = 'Oracle';
    SQLite = 'SQLite';
    MSAccess = 'MSAccess';
  end;


  { TdxConnectionSupport }

  TdxConnectionType = type string;

  TdxConnectionTypes = class
  public const
    Unknown = 'Unknown';
    ADO = 'ADO';
    FireDAC = 'FireDAC';
  end;

  { TdxConnectionParameter }

  TdxConnectionParameter = (
    Database
  );

  { TdxAutoCreateOption }

  TdxAutoCreateOption = (
    None,
    DatabaseAndSchema,
    SchemaOnly,
    SchemaAlreadyExists
  );

  TdxFunctionCategory = (
    DateTime,
    Logical,
    Math,
    Text,
    All
  );

  { TdxFunctionOperatorType }

  TdxFunctionOperatorType = (
    None,
    Custom,
    CustomNonDeterministic,
    Iif,
    IsNull, IsNullOrEmpty,
    Trim, Len, Substring, UpperCase, LowerCase, Concat, Ascii, Char, ToString, Replace, Reverse, Insert, CharIndex, Remove,
    Abs, Sqr, Cos, Sin, ArcTan, Exp, Log, Random,
    Tan, Power, Sign, Round, Ceil, Floor, Max, Min,

    ArcCos, ArcSin, ArcTan2,
    BigMul, Cosh, Log10, Sinh, Tanh,
    PadLeft, PadRight,
    StartsWith, EndsWith, Contains,
    ToInteger, ToInt64, ToSingle, ToDouble, ToDecimal,

    LocalDateTimeThisYear,
    LocalDateTimeThisMonth,
    LocalDateTimeLastWeek,
    LocalDateTimeThisWeek,
    LocalDateTimeYesterday,
    LocalDateTimeToday,
    LocalDateTimeNow,
    LocalDateTimeTomorrow,
    LocalDateTimeDayAfterTomorrow,
    LocalDateTimeNextWeek,
    LocalDateTimeTwoWeeksAway,
    LocalDateTimeNextMonth,
    LocalDateTimeNextYear,
    LocalDateTimeTwoMonthsAway,
    LocalDateTimeTwoYearsAway,
    LocalDateTimeLastMonth,
    LocalDateTimeLastYear,
    LocalDateTimeYearBeforeToday,

    IsOutlookIntervalBeyondThisYear,    // NextYear <= x
    IsOutlookIntervalLaterThisYear,     // NextMonth <= x < NextYear
    IsOutlookIntervalLaterThisMonth,    // TwoWeeksAway <= x < NextMonth
    IsOutlookIntervalNextWeek,          // NextWeek <= x < TwoWeeksAway
    IsOutlookIntervalLaterThisWeek,     // DayAfterTomorrow <= x < NextWeek
    IsOutlookIntervalTomorrow,          // Tomorrow <= x < DayAfterTomorrow
    IsOutlookIntervalToday,             // Today <= x < Tomorrow
    IsOutlookIntervalYesterday,         // Yesterday <= x < Today
    IsOutlookIntervalEarlierThisWeek,   // ThisWeek <= x < Yesterday
    IsOutlookIntervalLastWeek,          // LastWeek <= x < ThisWeek
    IsOutlookIntervalEarlierThisMonth,  // ThisMonth <= x < LastWeek
    IsOutlookIntervalEarlierThisYear,   // ThisYear <= x < ThisMonth
    IsOutlookIntervalPriorThisYear,     // x < ThisYear


    IsThisWeek, IsThisMonth, IsThisYear,
    IsNextMonth, IsNextYear,
    IsLastMonth, IsLastYear,
    IsYearToDate,

    IsSameDay,

    IsJanuary, IsFebruary,
    IsMarch, IsApril, IsMay,
    IsJune, IsJuly, IsAugust,
    IsSeptember, IsOctober, IsNovember,
    IsDecember,

    DateDiffTicks,
    MillisecondsBetween,
    SecondsBetween,
    MinutesBetween,
    HoursBetween,
    DaysBetween,
    MonthsBetween,
    YearsBetween,

    DateOf,
    MillisecondOf,
    SecondOf,
    MinuteOf,
    HourOf,
    DayOf,
    MonthOf,
    YearOf,
    DayOfTheWeek,
    DayOfTheYear,
    GetTimeOfDay,
    Now,
    UTCNow,
    Today,

    AddTimeSpan,
    IncTick,
    IncMillisecond,
    IncSecond,
    IncMinute,
    IncHour,
    IncDay,
    IncMonth,
    IncYear
  );

  { TdxBinaryOperatorType }

  TdxBinaryOperatorType  = (
    Equal,
    NotEqual,
    Greater,
    Less,
    LessOrEqual,
    GreaterOrEqual,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    Divide,
    Modulo,
    Multiply,
    Plus,
    Minus
  );

  { TdxUnaryOperatorType }

  TdxUnaryOperatorType = (
    BitwiseNot,
    Plus,
    Minus,
    &Not,
    IsNull
  );

  { TdxJoinType }

  TdxJoinType = (
    Inner,
    LeftOuter
  );

  { TdxAggregateFunctionType }

  TdxAggregateFunctionType = (
    Exists,
    Count,
    Max,
    Min,
    Avg,
    Sum,
    Single
  );

  { TdxGroupOperatorType }

  TdxGroupOperatorType = (
    &And,
    &Or
  );

  { TdxUpdateSchemaResult }

  TdxUpdateSchemaResult = (
    SchemaExists,
    FirstTableNotExists
  );

  { TdxBooleanCriteriaState }

  TdxBooleanCriteriaState = (
    Logical,
    Value,
    Undefined
  );

  { TdxDBColumnType }

  TdxDBColumnType = (
    Unknown,
    Boolean,
    Byte,
    SByte,
    Char,
    Decimal,
    Double,
    Single,
    Int32,
    UInt32,
    Int16,
    UInt16,
    Int64,
    UInt64,
    &String,
    DateTime,
    Guid,
    ByteArray,
    TimeSpan
   );

  { TdxAttributeType }

  TdxAttributeType = (
    &Class,
    &Property,
    Field,
    Index,
    Service
  );

  { TdxAttribute }

  TdxAttribute = (
  //Entity attributes
    Entity,
    Automapping,
    Table,
    SchemaName,
    Inheritance,
  //Column attributes
    Column,
    NonPersistent,
    Association,
    Aggregated,
    Size,
    ReadOnly,
    Nullable,
    Default,
    Key,
    Indexed,
    Unique,
    NoForeignKey,
    Blob,
    Delayed,
    Generator,
    VirtualColumn
  );

  TdxAttributes = set of TdxAttribute;

  { TdxLockingOption }

  TdxLockingOption = (
    None,
    Optimistic
  );


  { TdxOptimisticLockingBehavior }

  TdxOptimisticLockingBehavior = (
    NoLocking,
    ConsiderOptimisticLockingField,
    LockModified,
    LockAll
  );

  { TdxGeneratorType }

  TdxGeneratorType = (
    None,
    Identity,
    GUID,
    SequentialGUID,
    Sequence
  );

  { TdxDiscriminatorType }

  TdxDiscriminatorType = (
    &String,
    Integer
  );

  { TdxMapInheritanceType }

  TdxMapInheritanceType = (
    ParentTable,
    OwnTable
  );

  { TdxGenerator }

  TdxGenerator = record
  private
    FGeneratorType: TdxGeneratorType;
    FSource: string;
    function GetSequenceName: string; inline;
  public
    constructor Create(AGeneratorType: TdxGeneratorType); overload;
    constructor Create(AGeneratorType: TdxGeneratorType; const ASequenceName: string); overload;
    function NewGuid: TGUID;
    property GeneratorType: TdxGeneratorType read FGeneratorType;
    property SequenceName: string read GetSequenceName;
  end;

implementation

uses
  dxEMF.Utils;

{ TdxGenerator }

constructor TdxGenerator.Create(AGeneratorType: TdxGeneratorType);
begin
  FGeneratorType := AGeneratorType;
end;

constructor TdxGenerator.Create(AGeneratorType: TdxGeneratorType; const ASequenceName: string);
begin
  Create(AGeneratorType);
  FSource := ASequenceName;
end;

function TdxGenerator.GetSequenceName: string;
begin
  Result := FSource;
end;


function TdxGenerator.NewGuid: TGUID;
begin
  if FGeneratorType = TdxGeneratorType.SequentialGUID then
    CreateSequential(Result)
  else
    CreateGUID(Result)
end;

end.
