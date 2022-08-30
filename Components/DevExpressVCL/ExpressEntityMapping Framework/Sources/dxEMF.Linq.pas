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

unit dxEMF.Linq;

interface

{$I cxVer.inc}
{$I dxEMF.inc}

uses
  SysUtils, Classes, Generics.Defaults, Generics.Collections, TypInfo, Rtti,
  dxCoreClasses,
  dxEMF.Types,
  dxEMF.DB.Criteria,
  dxEMF.Linq.Expressions;

type

  IdxDataContext = interface;
  IdxLinqWhere = interface;
  IdxLinqOrderBy = interface;
  IdxLinqTake = interface;
  IdxLinqSelect = interface;

  {$HPPEMIT 'template<typename T> __interface IdxLinqWhere__1;'}
  IdxLinqWhere<T> = interface;
  {$HPPEMIT 'template<typename T> __interface IdxLinqOrderBy__1;'}
  IdxLinqOrderBy<T> = interface;
  {$HPPEMIT 'template<typename T> __interface IdxLinqIndexOrderBy__1;'}
  IdxLinqIndexOrderBy<T> = interface;
  {$HPPEMIT 'template<typename T> __interface IdxLinqTake__1;'}
  IdxLinqTake<T> = interface;
  {$HPPEMIT 'template<typename T> __interface IdxLinqSelect__1;'}
  IdxLinqSelect<T> = interface;

  TdxOnNew<T> = reference to procedure(var AObject: T);

  { IdxEntityInfo }

  IdxEntityInfo = interface(IInvokable)
  ['{52F469AF-E863-4969-8FDC-5EB75D9782B6}']
    function FieldByName(const AName: string): TdxLinqExpression;
    {$IFDEF DELPHIXE4}
    property Fields[const AName: string]: TdxLinqExpression read FieldByName; default;
    {$ENDIF}
  end;

  { IdxLinqCollectionExpression }

  IdxLinqCollectionExpression = interface(IInvokable)
  ['{9BE67C74-C735-4FCC-B30A-26F6AE3D200E}']
    function Count: TdxLinqExpression; overload;
  end;

  { IdxLinqCollectionExpression<T> }

  IdxLinqCollectionExpression<T: IdxEntityInfo> = interface(IdxLinqCollectionExpression)
    function Fields: T;
  end;

  { IdxEntityMetadataInfo }

  IdxEntityMetadataInfo = interface
  ['{09379531-B4A0-45E5-9FF5-252495A71829}']
    function GetDataContext: IdxDataContext;
    function GetEntityClass: TClass;
    function GetProvider: IdxQueryProvider;
    procedure CaptureInstance(AInstance: Pointer);
    procedure ReleaseInstance;
    property EntityClass: TClass read GetEntityClass;
    property Provider: IdxQueryProvider read GetProvider;
    property DataContext: IdxDataContext read GetDataContext;
  end;

  IdxEntityMetadataInfo<T> = interface
    function GetQueryProvider: IdxQueryProvider<T>;
    property QueryProvider: IdxQueryProvider<T> read GetQueryProvider;
  end;

  { IdxDataContext }

  IdxDataContext = interface(IInvokable)
  ['{1B6891E1-9C7A-424B-98FC-4DEC441E80CE}']
    function GetSession: IdxSession;
    function GetEntityInfo(AClass: TClass): IdxEntityInfo;
    property Session: IdxSession read GetSession;
  end;

  { IdxLinqSkip }

  IdxLinqSkip = interface(IdxQueryable)
  ['{AB815A20-A89D-4FDB-A940-8599914AA493}']
    function First: IdxQueryable;
    function Take(ACount: Integer): IdxQueryable;
  end;

  { IdxLinqSkip<T> }

  IdxLinqSkip<T> = interface(IdxQueryable<T>)
    function First: IdxQueryable<T>;
    function Take(ACount: Integer): IdxQueryable<T>;
  end;

  { IdxLinqTake }

  IdxLinqTake = interface(IdxQueryable)
  ['{7B700F9C-B94D-4CBA-823E-D43A12D21B24}']
    function Skip(ACount: Integer): IdxQueryable;
  end;

  { IdxLinqTake<T> }

  IdxLinqTake<T> = interface(IdxQueryable<T>)
    function Skip(ACount: Integer): IdxQueryable<T>;
  end;

  { IdxLinqSelect }

  IdxLinqSelect = interface(IdxQueryable)
  ['{D72100D9-8089-4C9E-B17A-AF0826BBD2EC}']
    function First: IdxQueryable;
    function Take(ACount: Integer): IdxLinqTake;
    function Skip(ACount: Integer): IdxLinqSkip;
  end;

  { IdxLinqSelect<T> }

  IdxLinqSelect<T> = interface(IdxQueryable<T>)
    function First: IdxQueryable<T>;
    function Take(ACount: Integer): IdxLinqTake<T>;
    function Skip(ACount: Integer): IdxLinqSkip<T>;
  end;


  { TdxSelectQueryable }

  TdxSelectQueryable = record
  strict private
    FQueryable: IdxQueryable;
  public
    class operator Implicit(const AQueryable: IdxQueryable): TdxSelectQueryable;
    class operator Implicit(const AOperator: TdxSelectQueryable): IdxQueryable;
  end;

  { TdxSelectTupleQueryable }

  TdxSelectTupleQueryable = record
  strict private
    FQueryable: IdxQueryable;
  public
    class operator Implicit(const AQueryable: IdxQueryable): TdxSelectTupleQueryable;
    class operator Implicit(const AOperator: TdxSelectTupleQueryable): IdxQueryable;
  end;

  { TdxSelectTake }

  TdxSelectTake = record
  strict private
    FQueryable: IdxQueryable;
  public
    class operator Implicit(const AQueryable: IdxQueryable): TdxSelectTake;
    class operator Implicit(const AOperator: TdxSelectTake): IdxQueryable;
    function Skip(ACount: Integer): TdxSelectQueryable;
  end;

  { TdxSelectTupleTake }

  TdxSelectTupleTake = record
  strict private
    FQueryable: IdxQueryable;
  public
    class operator Implicit(const AQueryable: IdxQueryable): TdxSelectTupleTake;
    class operator Implicit(const AOperator: TdxSelectTupleTake): IdxQueryable;
    function Skip(ACount: Integer): TdxSelectQueryable;
  end;


  { TdxSelectSkip }

  TdxSelectSkip = record
  strict private
    FQueryable: IdxQueryable;
  public
    class operator Implicit(const AQueryable: IdxQueryable): TdxSelectSkip;
    class operator Implicit(const AOperator: TdxSelectSkip): IdxQueryable;
    function First: TdxSelectQueryable;
    function Take(ACount: Integer): TdxSelectQueryable;
  end;

  { TdxSelectTupleSkip }

  TdxSelectTupleSkip = record
  strict private
    FQueryable: IdxQueryable;
  public
    class operator Implicit(const AQueryable: IdxQueryable): TdxSelectTupleSkip;
    class operator Implicit(const AOperator: TdxSelectTupleSkip): IdxQueryable;
    function First: TdxSelectQueryable;
    function Take(ACount: Integer): TdxSelectQueryable;
  end;

  { TdxSelect }

  TdxSelect = record
  strict private
    FQueryable: IdxQueryable;
  public
    class operator Implicit(const AQueryable: IdxQueryable): TdxSelect;
    class operator Implicit(const AOperator: TdxSelect): IdxQueryable;

    function First: TdxSelectQueryable;
    function Take(ACount: Integer): TdxSelectTake;
    function Skip(ACount: Integer): TdxSelectSkip;
  end;

  { TdxSelectTuple }

  TdxSelectTuple = record
  strict private
    FQueryable: IdxQueryable;
  public
    class operator Implicit(const AQueryable: IdxQueryable): TdxSelectTuple;
    class operator Implicit(const AOperator: TdxSelectTuple): IdxQueryable;

    function First: TdxSelectTupleQueryable;
    function Take(ACount: Integer): TdxSelectTupleTake;
    function Skip(ACount: Integer): TdxSelectTupleSkip;
  end;


  { IdxLinqFrom }

  IdxLinqFrom = interface(IdxLinq)
  ['{59B2C4FC-6CA1-47BD-BAB6-AA599591C41F}']
    function Where(const AExpression: TdxLinqExpression): IdxLinqWhere; overload;
    function OrderBy(const AExpression: TdxLinqExpression): IdxLinqOrderBy; overload;
    function OrderBy(const AExpression1, AExpression2: TdxLinqExpression): IdxLinqOrderBy; overload;
    function OrderBy(const AExpression1, AExpression2, AExpression3: TdxLinqExpression): IdxLinqOrderBy; overload;
    function OrderBy(const AExpression1, AExpression2, AExpression3, AExpression4: TdxLinqExpression): IdxLinqOrderBy; overload;
    function OrderBy(const AExpression1, AExpression2, AExpression3, AExpression4, AExpression5: TdxLinqExpression): IdxLinqOrderBy; overload;
    function OrderBy(const AExpression1, AExpression2, AExpression3, AExpression4, AExpression5, AExpression6: TdxLinqExpression): IdxLinqOrderBy; overload;
    function OrderBy(const AExpression1, AExpression2, AExpression3, AExpression4, AExpression5, AExpression6, AExpression7: TdxLinqExpression): IdxLinqOrderBy; overload;
    function OrderBy(const AExpressions: array of TdxLinqExpression): IdxLinqOrderBy; overload;
    function Select: TdxSelect; overload;
  end;

  { IdxLinqFrom<T> }

  IdxLinqFrom<T> = interface(IdxLinq)
    function Where(const AExpression: TdxLinqExpression): IdxLinqWhere<T>; overload;
    function OrderBy(const AExpression: TdxLinqExpression): IdxLinqOrderBy<T>; overload;
    function OrderBy(const AExpression1, AExpression2: TdxLinqExpression): IdxLinqOrderBy<T>; overload;
    function OrderBy(const AExpression1, AExpression2, AExpression3: TdxLinqExpression): IdxLinqOrderBy<T>; overload;
    function OrderBy(const AExpression1, AExpression2, AExpression3, AExpression4: TdxLinqExpression): IdxLinqOrderBy<T>; overload;
    function OrderBy(const AExpression1, AExpression2, AExpression3, AExpression4, AExpression5: TdxLinqExpression): IdxLinqOrderBy<T>; overload;
    function OrderBy(const AExpression1, AExpression2, AExpression3, AExpression4, AExpression5, AExpression6: TdxLinqExpression): IdxLinqOrderBy<T>; overload;
    function OrderBy(const AExpression1, AExpression2, AExpression3, AExpression4, AExpression5, AExpression6, AExpression7: TdxLinqExpression): IdxLinqOrderBy<T>; overload;
    function OrderBy(const AExpressions: array of TdxLinqExpression): IdxLinqOrderBy<T>; overload;
    function Select: IdxLinqSelect<T>; overload;
  end;

  { IdxLinqSimpleFrom<T> }

  IdxLinqSimpleFrom<T> = interface(IdxLinq)
    function OrderBy(const AIndex: Integer): IdxLinqIndexOrderBy<T>; overload;
    function OrderBy(const AIndex1, AIndex2: Integer): IdxLinqIndexOrderBy<T>; overload;
    function OrderBy(const AIndex1, AIndex2, AIndex3: Integer): IdxLinqIndexOrderBy<T>; overload;
    function OrderBy(const AIndex1, AIndex2, AIndex3, AIndex4: Integer): IdxLinqIndexOrderBy<T>; overload;
    function OrderBy(const AIndex1, AIndex2, AIndex3, AIndex4, AIndex5: Integer): IdxLinqIndexOrderBy<T>; overload;
    function OrderBy(const AIndex1, AIndex2, AIndex3, AIndex4, AIndex5, AIndex6: Integer): IdxLinqIndexOrderBy<T>; overload;
    function OrderBy(const AIndex1, AIndex2, AIndex3, AIndex4, AIndex5, AIndex6, AIndex7: Integer): IdxLinqIndexOrderBy<T>; overload;
    function OrderBy(const AIndexes: array of Integer): IdxLinqIndexOrderBy<T>; overload;
    function Select: IdxLinqSelect<T>; overload;
  end;

  { IdxLinqWhere }

  IdxLinqWhere = interface(IdxLinq)
  ['{16B4E696-A6DC-4474-967D-A5294AA87220}']
    function OrderBy(const AExpression: TdxLinqExpression): IdxLinqOrderBy; overload;
    function OrderBy(const AExpression1, AExpression2: TdxLinqExpression): IdxLinqOrderBy; overload;
    function OrderBy(const AExpression1, AExpression2, AExpression3: TdxLinqExpression): IdxLinqOrderBy; overload;
    function OrderBy(const AExpression1, AExpression2, AExpression3, AExpression4: TdxLinqExpression): IdxLinqOrderBy; overload;
    function OrderBy(const AExpression1, AExpression2, AExpression3, AExpression4, AExpression5: TdxLinqExpression): IdxLinqOrderBy; overload;
    function OrderBy(const AExpression1, AExpression2, AExpression3, AExpression4, AExpression5, AExpression6: TdxLinqExpression): IdxLinqOrderBy; overload;
    function OrderBy(const AExpression1, AExpression2, AExpression3, AExpression4, AExpression5, AExpression6, AExpression7: TdxLinqExpression): IdxLinqOrderBy; overload;
    function OrderBy(const AExpressions: array of TdxLinqExpression): IdxLinqOrderBy; overload;
    function Select: TdxSelect; overload;
  end;

  { IdxLinqWhere<T> }

  IdxLinqWhere<T> = interface(IdxLinq)
    function OrderBy(const AExpression: TdxLinqExpression): IdxLinqOrderBy<T>; overload;
    function OrderBy(const AExpression1, AExpression2: TdxLinqExpression): IdxLinqOrderBy<T>; overload;
    function OrderBy(const AExpression1, AExpression2, AExpression3: TdxLinqExpression): IdxLinqOrderBy<T>; overload;
    function OrderBy(const AExpression1, AExpression2, AExpression3, AExpression4: TdxLinqExpression): IdxLinqOrderBy<T>; overload;
    function OrderBy(const AExpression1, AExpression2, AExpression3, AExpression4, AExpression5: TdxLinqExpression): IdxLinqOrderBy<T>; overload;
    function OrderBy(const AExpression1, AExpression2, AExpression3, AExpression4, AExpression5, AExpression6: TdxLinqExpression): IdxLinqOrderBy<T>; overload;
    function OrderBy(const AExpression1, AExpression2, AExpression3, AExpression4, AExpression5, AExpression6, AExpression7: TdxLinqExpression): IdxLinqOrderBy<T>; overload;
    function OrderBy(const AExpressions: array of TdxLinqExpression): IdxLinqOrderBy<T>; overload;
    function Select: IdxLinqSelect<T>; overload;
  end;

  IdxLinqOrderBy = interface(IdxLinq)
  ['{76CB55C8-6B5C-4786-BD1A-7850C6D67B20}']
    function OrderBy(const AExpression: TdxLinqExpression): IdxLinqOrderBy; overload;
    function OrderBy(const AExpression1, AExpression2: TdxLinqExpression): IdxLinqOrderBy; overload;
    function OrderBy(const AExpression1, AExpression2, AExpression3: TdxLinqExpression): IdxLinqOrderBy; overload;
    function OrderBy(const AExpression1, AExpression2, AExpression3, AExpression4: TdxLinqExpression): IdxLinqOrderBy; overload;
    function OrderBy(const AExpression1, AExpression2, AExpression3, AExpression4, AExpression5: TdxLinqExpression): IdxLinqOrderBy; overload;
    function OrderBy(const AExpression1, AExpression2, AExpression3, AExpression4, AExpression5, AExpression6: TdxLinqExpression): IdxLinqOrderBy; overload;
    function OrderBy(const AExpression1, AExpression2, AExpression3, AExpression4, AExpression5, AExpression6, AExpression7: TdxLinqExpression): IdxLinqOrderBy; overload;
    function OrderBy(const AExpressions: array of TdxLinqExpression): IdxLinqOrderBy; overload;
    function Descending: IdxLinqOrderBy;
    function Select: TdxSelect; overload;
  end;

  { IdxLinqOrderBy<T> }

  IdxLinqOrderBy<T> = interface(IdxLinq)
    function OrderBy(const AExpression1: TdxLinqExpression): IdxLinqOrderBy<T>; overload;
    function OrderBy(const AExpression1, AExpression2: TdxLinqExpression): IdxLinqOrderBy<T>; overload;
    function OrderBy(const AExpression1, AExpression2, AExpression3: TdxLinqExpression): IdxLinqOrderBy<T>; overload;
    function OrderBy(const AExpression1, AExpression2, AExpression3, AExpression4: TdxLinqExpression): IdxLinqOrderBy<T>; overload;
    function OrderBy(const AExpression1, AExpression2, AExpression3, AExpression4, AExpression5: TdxLinqExpression): IdxLinqOrderBy<T>; overload;
    function OrderBy(const AExpression1, AExpression2, AExpression3, AExpression4, AExpression5, AExpression6: TdxLinqExpression): IdxLinqOrderBy<T>; overload;
    function OrderBy(const AExpression1, AExpression2, AExpression3, AExpression4, AExpression5, AExpression6, AExpression7: TdxLinqExpression): IdxLinqOrderBy<T>; overload;
    function OrderBy(const AExpressions: array of TdxLinqExpression): IdxLinqOrderBy<T>; overload;
    function Descending: IdxLinqOrderBy<T>;

    function Select: IdxLinqSelect<T>; overload;
  end;

  { IdxLinqIndexOrderBy<T> }

  IdxLinqIndexOrderBy<T> = interface(IdxLinq)
    function OrderBy(const AExpression1: Integer): IdxLinqIndexOrderBy<T>; overload;
    function OrderBy(const AExpression1, AExpression2: Integer): IdxLinqIndexOrderBy<T>; overload;
    function OrderBy(const AExpression1, AExpression2, AExpression3: Integer): IdxLinqIndexOrderBy<T>; overload;
    function OrderBy(const AExpression1, AExpression2, AExpression3, AExpression4: Integer): IdxLinqIndexOrderBy<T>; overload;
    function OrderBy(const AExpression1, AExpression2, AExpression3, AExpression4, AExpression5: Integer): IdxLinqIndexOrderBy<T>; overload;
    function OrderBy(const AExpression1, AExpression2, AExpression3, AExpression4, AExpression5, AExpression6: Integer): IdxLinqIndexOrderBy<T>; overload;
    function OrderBy(const AExpression1, AExpression2, AExpression3, AExpression4, AExpression5, AExpression6, AExpression7: Integer): IdxLinqIndexOrderBy<T>; overload;
    function OrderBy(const AExpressions: array of Integer): IdxLinqIndexOrderBy<T>; overload;
    function Descending: IdxLinqIndexOrderBy<T>;

    function Select: IdxLinqSelect<T>; overload;
  end;


  { IdxFromClause }

  IdxFromClause = interface
  ['{7C563E8D-D7B6-420D-BD62-8F2BF76AB7DD}']
    function GetFromClass: TClass;
    function GetIsEntity: Boolean;
    property FromClass: TClass read GetFromClass;
    property IsEntity: Boolean read GetIsEntity;
  end;

  { IdxLinqQueryBuilder }

  IdxLinqQueryBuilder = interface
  ['{5FE028D7-AAFD-4D1D-9754-B2A14C2605C5}']
    function GetSkipSelectedRecords: Integer;
    function GetFromClause: IdxFromClause;
    function GetOrderByClause: IdxSortByExpressions;
    function GetTopSelectedRecords: Integer;
    function GetWhereClause: TdxLinqExpression;

    property FromClause: IdxFromClause read GetFromClause;
    property OrderByClause: IdxSortByExpressions read GetOrderByClause;
    property WhereClause: TdxLinqExpression read GetWhereClause;
    property SkipSelectedRecords: Integer read GetSkipSelectedRecords;
    property TopSelectedRecords: Integer read GetTopSelectedRecords;
  end;


  { TdxLinqQuery }

  TdxLinqQueryBuilder = class(TInterfacedObject, IEnumerable, IdxQueryable,
    IdxLinq, IdxLinqFrom, IdxLinqWhere, IdxLinqOrderBy,
    IdxLinqSkip, IdxLinqTake, IdxLinqSelect,
    IdxFromClause,
    IdxExpression, IdxLinqQueryBuilder)
  private type

    TOrderBy = class(TdxSortByExpressions)
    protected
      FLastIndex: Integer;
      procedure SetDescendingAtMark;
      procedure SetMark;
    end;

    TEmptyEnumerator = class(TInterfacedObject, IEnumerator)
    protected
      function GetCurrent: TObject;
      function MoveNext: Boolean;
      procedure Reset;
    end;

  private
    FOwner: IdxQueryProvider;
    FDataContext: IdxDataContext;
    FFirstOnly: Boolean;
    FSkipSelectedRecords: Integer;
    FTopSelectedRecords: Integer;
    FWhereClause: TdxLinqExpression;
    FOrderByClause: IdxSortByExpressions;

    FFromClass: TClass;
    FIsEntity: TdxNullableValue<Boolean>;
    FQueryable: IdxQueryable;
    FEntityMetadataInfo: IdxEntityMetadataInfo;
    function GetOrderByClause: IdxSortByExpressions;
    function GetWhereClause: TdxLinqExpression;
  protected
    procedure SetFromClass(const Value: TClass);
    function IsEntity: Boolean;

    // IEnumerable
    function GetEnumerator: IEnumerator;
    // IdxQueryable
    function GetProvider: IdxQueryProvider;


    function From<T: class>: TdxLinqQueryBuilder; overload;
    function From<T: class>(var A: TdxLinqExpression): TdxLinqQueryBuilder; overload;
    function From(const AEntityInfo: IdxEntityInfo): IdxLinqFrom; overload;

    // IdxFromClause
    function GetFromClass: TClass;
    function GetIsEntity: Boolean;
    // IdxLinqQueryBuilder
    function GetSkipSelectedRecords: Integer;
    function GetFromClause: IdxFromClause;
    function GetTopSelectedRecords: Integer;
    // IdxLinqWhere
    function Where(const AExpression: TdxLinqExpression): IdxLinqWhere; overload;
    function Where(const AExpression: TdxPredicate): IdxLinqWhere; overload;
    // IdxLinqOrderBy
    function OrderBy(const AExpression: TdxLinqExpression): IdxLinqOrderBy; overload;
    function OrderBy(const AExpression1, AExpression2: TdxLinqExpression): IdxLinqOrderBy; overload;
    function OrderBy(const AExpression1, AExpression2, AExpression3: TdxLinqExpression): IdxLinqOrderBy; overload;
    function OrderBy(const AExpression1, AExpression2, AExpression3, AExpression4: TdxLinqExpression): IdxLinqOrderBy; overload;
    function OrderBy(const AExpression1, AExpression2, AExpression3, AExpression4, AExpression5: TdxLinqExpression): IdxLinqOrderBy; overload;
    function OrderBy(const AExpression1, AExpression2, AExpression3, AExpression4, AExpression5, AExpression6: TdxLinqExpression): IdxLinqOrderBy; overload;
    function OrderBy(const AExpression1, AExpression2, AExpression3, AExpression4, AExpression5, AExpression6, AExpression7: TdxLinqExpression): IdxLinqOrderBy; overload;
    function OrderBy(const AExpressions: array of TdxLinqExpression): IdxLinqOrderBy; overload;
    function Descending: IdxLinqOrderBy;
    // IdxSelect
    function Select: TdxSelect; overload;
    // IdxLinqAggregate
    function Count: IdxQueryable;
    function Max: IdxQueryable;
    function Min: IdxQueryable;
    function Sum: IdxQueryable;
    function Average: IdxQueryable;
    function First: IdxQueryable;
    function Take(ACount: Integer): IdxLinqTake;
    function Skip(ACount: Integer): IdxLinqSkip;
    // IdxLinqTake
    function TakeSkip(ACount: Integer): IdxQueryable;
    function IdxLinqTake.Skip = TakeSkip;
    // IdxLinqSkip
    function SkipTake(ACount: Integer): IdxQueryable;
    function IdxLinqSkip.Take = SkipTake;

    property FirstOnly: Boolean read FFirstOnly;
    property FromClass: TClass read FFromClass;
    property SkipSelectedRecords: Integer read FSkipSelectedRecords;
    property TopSelectedRecords: Integer read FTopSelectedRecords;
    property WhereClause: TdxLinqExpression read GetWhereClause;
    property OrderByClause: IdxSortByExpressions read GetOrderByClause;
  public
    constructor Create; overload;
    constructor Create(const ADataContext: IdxDataContext); overload;
    destructor Destroy; override;
  end;

  { TdxLinqQuery<T> }

  TdxLinqQueryBuilder<T: class> = class(TdxLinqQueryBuilder, IEnumerable<T>, IdxQueryable<T>,
    IdxLinqFrom<T>, IdxLinqSimpleFrom<T>, IdxLinqWhere<T>, IdxLinqOrderBy<T>, IdxLinqIndexOrderBy<T>,
    IdxLinqSkip<T>, IdxLinqTake<T>, IdxLinqSelect<T>)
  strict private type

      TEmptyEnumerator = class(TInterfacedObject, IEnumerator<T>)
      protected
        { IEnumerator }
        function GetCurrent: TObject;
        function MoveNext: Boolean;
        procedure Reset;
        { IEnumerator<T> }
        function GetCurrentGeneric: T;
        function IEnumerator<T>.GetCurrent = GetCurrentGeneric;
      end;

  strict private
    FNewFunc: TdxFunc<TArray<TValue>, T>;
    FQueryProvider: IdxQueryProvider<T>;
    FQueryable: IdxQueryable<T>;
    function GetMemberExpression(AIndex: Integer): TdxLinqExpression;
  protected
    function FromBuilder: TdxLinqQueryBuilder<T>;

    { IEnumerable<T> }
    function GetEnumerator: IEnumerator<T>;
    { IdxQueryable<T> }
    procedure NewFunc(AFunc: TdxFunc<TArray<TValue>, T>);
    function GetProvider: IdxQueryProvider<T>;
    { IdxLinqFrom<T> }
    function Where(const AExpression: TdxLinqExpression): IdxLinqWhere<T>; overload;
    function OrderBy(const AExpression: TdxLinqExpression): IdxLinqOrderBy<T>; overload;
    function OrderBy(const AExpression1, AExpression2: TdxLinqExpression): IdxLinqOrderBy<T>; overload;
    function OrderBy(const AExpression1, AExpression2, AExpression3: TdxLinqExpression): IdxLinqOrderBy<T>; overload;
    function OrderBy(const AExpression1, AExpression2, AExpression3, AExpression4: TdxLinqExpression): IdxLinqOrderBy<T>; overload;
    function OrderBy(const AExpression1, AExpression2, AExpression3, AExpression4, AExpression5: TdxLinqExpression): IdxLinqOrderBy<T>; overload;
    function OrderBy(const AExpression1, AExpression2, AExpression3, AExpression4, AExpression5, AExpression6: TdxLinqExpression): IdxLinqOrderBy<T>; overload;
    function OrderBy(const AExpression1, AExpression2, AExpression3, AExpression4, AExpression5, AExpression6, AExpression7: TdxLinqExpression): IdxLinqOrderBy<T>; overload;
    function OrderBy(const AExpressions: array of TdxLinqExpression): IdxLinqOrderBy<T>; overload;
    function Descending: IdxLinqOrderBy<T>;
    { IdxLinqIndexOrderBy<T> }
    function OrderBy(const AIndex: Integer): IdxLinqIndexOrderBy<T>; overload;
    function OrderBy(const AIndex1, AIndex2: Integer): IdxLinqIndexOrderBy<T>; overload;
    function OrderBy(const AIndex1, AIndex2, AIndex3: Integer): IdxLinqIndexOrderBy<T>; overload;
    function OrderBy(const AIndex1, AIndex2, AIndex3, AIndex4: Integer): IdxLinqIndexOrderBy<T>; overload;
    function OrderBy(const AIndex1, AIndex2, AIndex3, AIndex4, AIndex5: Integer): IdxLinqIndexOrderBy<T>; overload;
    function OrderBy(const AIndex1, AIndex2, AIndex3, AIndex4, AIndex5, AIndex6: Integer): IdxLinqIndexOrderBy<T>; overload;
    function OrderBy(const AIndex1, AIndex2, AIndex3, AIndex4, AIndex5, AIndex6, AIndex7: Integer): IdxLinqIndexOrderBy<T>; overload;
    function OrderBy(const AIndexes: array of Integer): IdxLinqIndexOrderBy<T>; overload;
    function IndexOrderByDescending: IdxLinqIndexOrderBy<T>;
    function IdxLinqIndexOrderBy<T>.Descending = IndexOrderByDescending;

    function Select: IdxLinqSelect<T>; overload;

    // IdxLinqSkip<T>
    function First: IdxQueryable<T>;
    function Take(ACount: Integer): IdxLinqTake<T>;
    function Skip(ACount: Integer): IdxLinqSkip<T>;
    // IdxLinqTake<T>
    function TakeSkip(ACount: Integer): IdxQueryable<T>;
    function IdxLinqTake<T>.Skip = TakeSkip;
    // IdxLinqSkip<T>
    function SkipTake(ACount: Integer): IdxQueryable<T>;
    function IdxLinqSkip<T>.Take = SkipTake;
  end;

  { TdxLinqBuilder }

  TdxLinqBuilder = class(TInterfacedObject, IEnumerable, IdxQueryProvider)
  strict private
    FOwner: TdxLinqQueryBuilder;
  protected
    function GetEnumerator: IEnumerator;
    // IdxQueryProvider
    function CreateQuery(const AExpression: IdxExpression): IdxQueryable;
    property Owner: TdxLinqQueryBuilder read FOwner;
  public
    constructor Create(AOwner: TdxLinqQueryBuilder); overload;
  end;

  { TdxLinqBuilder<TTo> }

  TdxLinqBuilder<T> = class(TdxLinqBuilder, IEnumerable<T>, IdxQueryProvider<T>, IdxQueryable<T>)
  strict private type

    TEnumerator = class(TInterfacedObject, IEnumerator<T>)
    private
      FLinqBuilder: TdxLinqBuilder<T>;
      FOwnerEnumerator: IEnumerator;
      FCurrentValue: T;
    protected
      FIsClass: Boolean;
      FHasDispose: Boolean;
      procedure Init;
      procedure ClearCurrentValue(var AValue: T);
      { IEnumerator }
      function GetCurrent: TObject;
      { IEnumerator<T> }
      function GetCurrentGeneric: T;
      function IEnumerator<T>.GetCurrent = GetCurrentGeneric;
      function MoveNext: Boolean;
      procedure Reset;
    public
      constructor Create(ALinqBuilder: TdxLinqBuilder<T>);
      destructor Destroy; override;
    end;

  strict private
    FDataManager: TObject;
    FOnNew: TdxFunc<T>;
    FOnNewFromVariants: TdxFunc<TArray<Variant>, T>;
  protected
    function GetTuple(AInstance: Pointer): TObject;
    function GetValue(AInstance: Pointer): T;
    function GetMemberValues(AInstance: Pointer): TArray<Variant>;
    // IEnumerable<T>
    function GetEnumerator: IEnumerator<T>;
    // IdxQueryable<T>
    function GetProvider: IdxQueryProvider<T>;
    // IdxQueryProvider<T>
    function CreateQueryGeneric(const AExpression: IdxExpression): IdxQueryable<T>;
    function IdxQueryProvider<T>.CreateQuery = CreateQueryGeneric;
  public
    constructor Create(AOwner: TdxLinqQueryBuilder; const ANewFunc: TdxFunc<T>); overload;
    constructor Create(AOwner: TdxLinqQueryBuilder; const ANewFunc: TdxFunc<TArray<Variant>, T>); overload;
  end;

  TdxLinqBuilder<TFrom: class; T> = class(TdxLinqQueryBuilder<TFrom>)
  public
    constructor Create(const AOwner: IdxQueryable<TFrom>);
  end;

  { TdxLinqFrom }

  TdxLinqFrom = record
  private
    FDataContext: IdxDataContext;
  public
    class operator Implicit(const ADataContext: IdxDataContext): TdxLinqFrom;

    function From(const AEntityInfo: IdxEntityInfo): IdxLinqFrom; overload;
    function From<T: class>(const AEntityInfo: IdxEntityInfo): IdxLinqFrom<T>; overload;
    function From<T: class>: IdxLinqSimpleFrom<T>; overload;
  end;

  { TdxLinqExpressionFactory }

  TdxLinqExpressionFactory = class sealed
  private
    class var
      FExpressions: TDictionary<TClass, PTypeInfo>;
      FDataContexts: TList<PTypeInfo>;
  protected
    class constructor Create;
    class destructor Destroy;
    class function DataContextEntityClasses(ADataContext: PTypeInfo): TArray<TClass>; static;
    class procedure RegisterExpression(AClass: TClass; AExpression: PTypeInfo); overload; static;
    class procedure RegisterDataContext(ADataContext: PTypeInfo); overload; static;
    class function TryGetExpression(AClass: TClass; out AExpression: PTypeInfo): Boolean;

    class property Expressions: TDictionary<TClass, PTypeInfo> read FExpressions;
    class property DataContexts: TList<PTypeInfo> read FDataContexts;
  public
    class function GetDataContext(const ASession: IdxSession): IdxDataContext; overload;
    class function GetDataContext(ADataContext: PTypeInfo; const ASession: IdxSession): IdxDataContext; overload;
    class function GetDataContexts: TArray<PTypeInfo>;
    class function GetEntityMetadata(AClass: TClass; const AOwner: IdxQueryProvider): IdxEntityInfo;
    class function FindClass(AExpression: PTypeInfo): TClass; overload;
    class function FindClass(const AIID: TGUID): TClass; overload;

    class procedure Register<TEntity: class; TExpression: IdxEntityInfo>; overload;
    class procedure Register<TDataContext: IdxDataContext>; overload;
    class procedure UnRegister(AClass: TClass); overload;
    class procedure UnRegister(AClasses: array of TClass); overload;
    class procedure UnRegister<TDataContext: IdxDataContext>; overload;
  end;

function Linq: TdxLinqFrom; overload;
function Linq(const ADataContext: IdxDataContext): TdxLinqFrom; overload;

implementation

uses
{$IFNDEF DELPHIXE2}
  Windows, RTLConsts,
{$ENDIF}
  dxCore,
  dxEMF.Strs,
  dxEMF.Core,
  dxEMF.Utils,
  dxEMF.Metadata,
  dxEMF.Core.Collections;

function Linq: TdxLinqFrom;
begin
  Result := nil;
end;

function Linq(const ADataContext: IdxDataContext): TdxLinqFrom; overload;
begin
  Result := ADataContext;
end;

{ TdxLinqQuery }

constructor TdxLinqQueryBuilder.Create;
begin
  inherited Create;
  FOrderByClause := TOrderBy.Create;
end;

constructor TdxLinqQueryBuilder.Create(const ADataContext: IdxDataContext);
begin
  Create;
  FDataContext := ADataContext;
end;

destructor TdxLinqQueryBuilder.Destroy;
begin
  FQueryable := nil;
  inherited Destroy;
end;

function TdxLinqQueryBuilder.From<T>: TdxLinqQueryBuilder;
begin
  Result := Self;
  FFromClass := T;
end;



function TdxLinqQueryBuilder.From(const AEntityInfo: IdxEntityInfo): IdxLinqFrom;
var
  AClass: TClass;
begin
  if Supports(AEntityInfo, IdxEntityMetadataInfo, FEntityMetadataInfo) then
  begin
    AClass := FEntityMetadataInfo.EntityClass;
    FDataContext := FEntityMetadataInfo.DataContext;
  end
  else
    raise Exception.Create('EntityMetadataInfo expected');
  FOwner := FEntityMetadataInfo.Provider;
  SetFromClass(AClass);
  Result := Self;
end;

function TdxLinqQueryBuilder.From<T>(var A: TdxLinqExpression): TdxLinqQueryBuilder;
begin
  Result := Self;
  FFromClass := T;
end;

function TdxLinqQueryBuilder.GetEnumerator: IEnumerator;
begin
  if FOwner <> nil then
  begin
    if FQueryable = nil then
      FQueryable := FOwner.CreateQuery(Self);
    Result := FQueryable.GetEnumerator;
  end
  else
    Result := TEmptyEnumerator.Create;
end;

function TdxLinqQueryBuilder.GetFromClass: TClass;
begin
  Result := FFromClass;
end;

function TdxLinqQueryBuilder.GetFromClause: IdxFromClause;
begin
  Result := Self;
end;

function TdxLinqQueryBuilder.GetIsEntity: Boolean;
begin
  Result := EntityManager.IsEntity(FFromClass);
end;

function TdxLinqQueryBuilder.GetOrderByClause: IdxSortByExpressions;
begin
  Result := FOrderByClause;
end;

function TdxLinqQueryBuilder.GetProvider: IdxQueryProvider;
begin
  Result := FOwner;
end;

function TdxLinqQueryBuilder.GetSkipSelectedRecords: Integer;
begin
  Result := SkipSelectedRecords;
end;

function TdxLinqQueryBuilder.GetTopSelectedRecords: Integer;
begin
  Result := TopSelectedRecords;
end;

function TdxLinqQueryBuilder.GetWhereClause: TdxLinqExpression;
begin
  Result := FWhereClause;
end;

function TdxLinqQueryBuilder.IsEntity: Boolean;
begin
  if not FIsEntity.HasValue then
    FIsEntity := EntityManager.IsEntity(FFromClass);
  Result := FIsEntity;
end;


function TdxLinqQueryBuilder.OrderBy(const AExpression: TdxLinqExpression): IdxLinqOrderBy;
var
  AOrderBy: TOrderBy;
begin
  AOrderBy := FOrderByClause as TOrderBy;
  AOrderBy.SetMark;
  AOrderBy.Add(TdxSortByExpression.Create(TdxLinqExpression(AExpression)));
  Result := Self;
end;

function TdxLinqQueryBuilder.OrderBy(const AExpression1, AExpression2: TdxLinqExpression): IdxLinqOrderBy;
begin
  Result := OrderBy([AExpression1, AExpression2]);
end;

function TdxLinqQueryBuilder.OrderBy(const AExpression1, AExpression2, AExpression3: TdxLinqExpression): IdxLinqOrderBy;
begin
  Result := OrderBy([AExpression1, AExpression2, AExpression3]);
end;

function TdxLinqQueryBuilder.OrderBy(const AExpression1, AExpression2, AExpression3,
  AExpression4: TdxLinqExpression): IdxLinqOrderBy;
begin
  Result := OrderBy([AExpression1, AExpression2, AExpression3, AExpression4]);
end;

function TdxLinqQueryBuilder.OrderBy(const AExpression1, AExpression2, AExpression3, AExpression4,
  AExpression5: TdxLinqExpression): IdxLinqOrderBy;
begin
  Result := OrderBy([AExpression1, AExpression2, AExpression3, AExpression4, AExpression5]);
end;

function TdxLinqQueryBuilder.OrderBy(const AExpression1, AExpression2, AExpression3, AExpression4, AExpression5,
  AExpression6: TdxLinqExpression): IdxLinqOrderBy;
begin
  Result := OrderBy([AExpression1, AExpression2, AExpression3, AExpression4, AExpression5, AExpression6]);
end;

function TdxLinqQueryBuilder.OrderBy(const AExpression1, AExpression2, AExpression3, AExpression4, AExpression5, AExpression6,
  AExpression7: TdxLinqExpression): IdxLinqOrderBy;
begin
  Result := OrderBy([AExpression1, AExpression2, AExpression3, AExpression4, AExpression5, AExpression6, AExpression7]);
end;

function TdxLinqQueryBuilder.OrderBy(const AExpressions: array of TdxLinqExpression): IdxLinqOrderBy;
var
  AOrderBy: TOrderBy;
  AExpression: TdxLinqExpression;
begin
  AOrderBy := FOrderByClause as TOrderBy;
  AOrderBy.SetMark;
  for AExpression in AExpressions do
    AOrderBy.Add(TdxSortByExpression.Create(AExpression));
  Result := Self;
end;

function TdxLinqQueryBuilder.Where(const AExpression: TdxLinqExpression): IdxLinqWhere;
begin
  Result := Self;
  FWhereClause := AExpression;
end;


function TdxLinqQueryBuilder.Descending: IdxLinqOrderBy;
begin
  (FOrderByClause as TOrderBy).SetDescendingAtMark;
  Result := Self;
end;

function TdxLinqQueryBuilder.Select: TdxSelect;
begin
  Result := Self;
end;


procedure TdxLinqQueryBuilder.SetFromClass(const Value: TClass);
begin
  FFromClass := Value;
end;

function TdxLinqQueryBuilder.Where(const AExpression: TdxPredicate): IdxLinqWhere;
begin

  Result := Self;
end;

function TdxLinqQueryBuilder.Count: IdxQueryable;
begin
  Result := Self;
end;

function TdxLinqQueryBuilder.Max: IdxQueryable;
begin
  Result := Self;
end;

function TdxLinqQueryBuilder.Min: IdxQueryable;
begin
  Result := Self;
end;

function TdxLinqQueryBuilder.Sum: IdxQueryable;
begin
  Result := Self;
end;

function TdxLinqQueryBuilder.Average: IdxQueryable;
begin
  Result := Self;
end;


function TdxLinqQueryBuilder.First: IdxQueryable;
begin
  FFirstOnly := True;
  Result := Self;
end;

function TdxLinqQueryBuilder.Take(ACount: Integer): IdxLinqTake;
begin
  FTopSelectedRecords := ACount;
  Result := Self;
end;

function TdxLinqQueryBuilder.TakeSkip(ACount: Integer): IdxQueryable;
begin
  FSkipSelectedRecords := ACount;
  Result := Self;
end;

function TdxLinqQueryBuilder.Skip(ACount: Integer): IdxLinqSkip;
begin
  FSkipSelectedRecords := ACount;
  Result := Self;
end;

function TdxLinqQueryBuilder.SkipTake(ACount: Integer): IdxQueryable;
begin
  FTopSelectedRecords := ACount;
  Result := Self;
end;

{ TdxSelectQueryable }

class operator TdxSelectQueryable.Implicit(const AQueryable: IdxQueryable): TdxSelectQueryable;
begin
  Result.FQueryable := AQueryable;
end;

class operator TdxSelectQueryable.Implicit(const AOperator: TdxSelectQueryable): IdxQueryable;
begin
  Result := AOperator.FQueryable;
end;


{ TdxSelectTupleQueryable }

class operator TdxSelectTupleQueryable.Implicit(const AQueryable: IdxQueryable): TdxSelectTupleQueryable;
begin
  Result.FQueryable := AQueryable;
end;

class operator TdxSelectTupleQueryable.Implicit(const AOperator: TdxSelectTupleQueryable): IdxQueryable;
begin
  Result := AOperator.FQueryable;
end;


{ TdxSelectTake }

class operator TdxSelectTake.Implicit(const AQueryable: IdxQueryable): TdxSelectTake;
begin
  Result.FQueryable := AQueryable;
end;

class operator TdxSelectTake.Implicit(const AOperator: TdxSelectTake): IdxQueryable;
begin
  Result := AOperator.FQueryable;
end;

function TdxSelectTake.Skip(ACount: Integer): TdxSelectQueryable;
begin
  Result := (FQueryable as IdxLinqTake).Skip(ACount);
end;


{ TdxSelectTupleTake }

class operator TdxSelectTupleTake.Implicit(const AQueryable: IdxQueryable): TdxSelectTupleTake;
begin
  Result.FQueryable := AQueryable;
end;

class operator TdxSelectTupleTake.Implicit(const AOperator: TdxSelectTupleTake): IdxQueryable;
begin
  Result := AOperator.FQueryable;
end;

function TdxSelectTupleTake.Skip(ACount: Integer): TdxSelectQueryable;
begin
  NotImplemented;
end;


{ TdxSelectSkip }

class operator TdxSelectSkip.Implicit(const AQueryable: IdxQueryable): TdxSelectSkip;
begin
  Result.FQueryable := AQueryable;
end;

class operator TdxSelectSkip.Implicit(const AOperator: TdxSelectSkip): IdxQueryable;
begin
  Result := AOperator.FQueryable;
end;

function TdxSelectSkip.First: TdxSelectQueryable;
begin
  Result := (FQueryable as IdxLinqSelect).First;
end;

function TdxSelectSkip.Take(ACount: Integer): TdxSelectQueryable;
begin
  Result := (FQueryable as IdxLinqSelect).Take(ACount);
end;


{ TdxSelectTupleSkip }

class operator TdxSelectTupleSkip.Implicit(const AQueryable: IdxQueryable): TdxSelectTupleSkip;
begin
  Result.FQueryable := AQueryable;
end;

class operator TdxSelectTupleSkip.Implicit(const AOperator: TdxSelectTupleSkip): IdxQueryable;
begin
  Result := AOperator.FQueryable;
end;
function TdxSelectTupleSkip.First: TdxSelectQueryable;
begin
  NotImplemented
end;
function TdxSelectTupleSkip.Take(ACount: Integer): TdxSelectQueryable;
begin
  NotImplemented
end;

{ TdxSelect }

class operator TdxSelect.Implicit(const AQueryable: IdxQueryable): TdxSelect;
begin
  Result.FQueryable := AQueryable;
end;

class operator TdxSelect.Implicit(const AOperator: TdxSelect): IdxQueryable;
begin
  Result := AOperator.FQueryable;
end;

function TdxSelect.First: TdxSelectQueryable;
begin
  (FQueryable as IdxLinqSelect).First;
  Result := FQueryable;
end;

function TdxSelect.Take(ACount: Integer): TdxSelectTake;
begin
  (FQueryable as IdxLinqSelect).Take(ACount);
  Result := FQueryable;
end;

function TdxSelect.Skip(ACount: Integer): TdxSelectSkip;
begin
  Result := (FQueryable as IdxLinqSelect).Skip(ACount);
end;


{ TdxSelectTuple }

class operator TdxSelectTuple.Implicit(const AQueryable: IdxQueryable): TdxSelectTuple;
begin
  Result.FQueryable := AQueryable;
end;

class operator TdxSelectTuple.Implicit(const AOperator: TdxSelectTuple): IdxQueryable;
begin
  Result := AOperator.FQueryable;
end;

function TdxSelectTuple.First: TdxSelectTupleQueryable;
begin
  NotImplemented;
end;

function TdxSelectTuple.Take(ACount: Integer): TdxSelectTupleTake;
begin
  NotImplemented;
end;

function TdxSelectTuple.Skip(ACount: Integer): TdxSelectTupleSkip;
begin
  NotImplemented;
end;



{ TdxLinqQuery<T> }

function TdxLinqQueryBuilder<T>.Descending: IdxLinqOrderBy<T>;
begin
  inherited Descending;
  Result := Self;
end;

function TdxLinqQueryBuilder<T>.First: IdxQueryable<T>;
begin
  inherited First;
  Result := Self;
end;

function TdxLinqQueryBuilder<T>.FromBuilder: TdxLinqQueryBuilder<T>;
begin
  inherited From<T>;
  Result := Self;
end;

function TdxLinqQueryBuilder<T>.GetEnumerator: IEnumerator<T>;
var
  AQueryProvider: IdxQueryProvider<T>;
begin
  AQueryProvider := GetProvider;

  FQueryable := AQueryProvider.CreateQuery(Self);
  if FQueryable <> nil then
    Result := FQueryable.GetEnumerator
  else
    Result := TEmptyEnumerator.Create;
end;

function TdxLinqQueryBuilder<T>.GetMemberExpression(AIndex: Integer): TdxLinqExpression;
var
  AEntityInfo: TdxEntityInfo;
begin
  AEntityInfo := EntityManager.GetEntityInfo(FromClass);
  if (AIndex < 1) or (AIndex > AEntityInfo.MemberAttributes.Count) then
    raise Exception.CreateFmt(sdxOrderByTermOutOfRange, [AEntityInfo.MemberAttributes.Count]);
  Result := TdxLinqExpression(AEntityInfo.MemberAttributes[AIndex - 1].MemberName);
end;

{$REGION 'IdxLinqIndexOrderBy<T>'}
function TdxLinqQueryBuilder<T>.OrderBy(const AIndex: Integer): IdxLinqIndexOrderBy<T>;
begin
  inherited OrderBy(GetMemberExpression(AIndex));
  Result := Self;
end;

function TdxLinqQueryBuilder<T>.OrderBy(const AIndex1, AIndex2: Integer): IdxLinqIndexOrderBy<T>;
begin
  inherited OrderBy([GetMemberExpression(AIndex1), GetMemberExpression(AIndex2)]);
  Result := Self;
end;

function TdxLinqQueryBuilder<T>.OrderBy(const AIndex1, AIndex2, AIndex3: Integer): IdxLinqIndexOrderBy<T>;
begin
  inherited OrderBy([GetMemberExpression(AIndex1), GetMemberExpression(AIndex2), GetMemberExpression(AIndex3)]);
  Result := Self;
end;

function TdxLinqQueryBuilder<T>.OrderBy(const AIndex1, AIndex2, AIndex3,
  AIndex4: Integer): IdxLinqIndexOrderBy<T>;
begin
  inherited OrderBy([GetMemberExpression(AIndex1), GetMemberExpression(AIndex2), GetMemberExpression(AIndex3),
    GetMemberExpression(AIndex4)]);
  Result := Self;
end;

function TdxLinqQueryBuilder<T>.OrderBy(const AIndex1, AIndex2, AIndex3, AIndex4,
  AIndex5: Integer): IdxLinqIndexOrderBy<T>;
begin
  inherited OrderBy([GetMemberExpression(AIndex1), GetMemberExpression(AIndex2), GetMemberExpression(AIndex3),
    GetMemberExpression(AIndex4), GetMemberExpression(AIndex5)]);
  Result := Self;
end;

function TdxLinqQueryBuilder<T>.OrderBy(const AIndex1, AIndex2, AIndex3, AIndex4, AIndex5,
  AIndex6: Integer): IdxLinqIndexOrderBy<T>;
begin
  inherited OrderBy([GetMemberExpression(AIndex1), GetMemberExpression(AIndex2), GetMemberExpression(AIndex3),
    GetMemberExpression(AIndex4), GetMemberExpression(AIndex5), GetMemberExpression(AIndex6)]);
  Result := Self;
end;

function TdxLinqQueryBuilder<T>.OrderBy(const AIndex1, AIndex2, AIndex3, AIndex4, AIndex5,
  AIndex6, AIndex7: Integer): IdxLinqIndexOrderBy<T>;
begin
  inherited OrderBy([GetMemberExpression(AIndex1), GetMemberExpression(AIndex2), GetMemberExpression(AIndex3),
    GetMemberExpression(AIndex4), GetMemberExpression(AIndex5), GetMemberExpression(AIndex6), GetMemberExpression(AIndex7)]);
  Result := Self;
end;

function TdxLinqQueryBuilder<T>.OrderBy(const AIndexes: array of Integer): IdxLinqIndexOrderBy<T>;
var
  AExpressions: TArray<TdxLinqExpression>;
  I: Integer;
begin
  SetLength(AExpressions, Length(AIndexes));
  for I := 0 to Length(AIndexes) - 1 do
    AExpressions[I] := GetMemberExpression(AIndexes[I]);
  inherited OrderBy(AExpressions);
  Result := Self;
end;

function TdxLinqQueryBuilder<T>.IndexOrderByDescending: IdxLinqIndexOrderBy<T>;
begin
  inherited Descending;
  Result := Self;
end;
procedure TdxLinqQueryBuilder<T>.NewFunc(AFunc: TdxFunc<TArray<TValue>, T>);
begin
  FNewFunc := AFunc;
end;

{$ENDREGION}

{ TdxLinqQuery<T>.TEmptyEnumerator }

function TdxLinqQueryBuilder<T>.TEmptyEnumerator.GetCurrent: TObject;
begin
  Result := nil;
end;

function TdxLinqQueryBuilder<T>.TEmptyEnumerator.GetCurrentGeneric: T;
begin
  Result := nil;
end;

function TdxLinqQueryBuilder<T>.TEmptyEnumerator.MoveNext: Boolean;
begin
  Result := False;
end;

procedure TdxLinqQueryBuilder<T>.TEmptyEnumerator.Reset;
begin
  // do nothing
end;



function TdxLinqQueryBuilder<T>.GetProvider: IdxQueryProvider<T>;
begin
  if FQueryProvider = nil then
    FQueryProvider := (FDataContext.Session as TdxEMFCustomSession).GetQueryProvider<T>;
  Result := FQueryProvider;
end;

{$REGION 'OrderBy'}
function TdxLinqQueryBuilder<T>.OrderBy(const AExpression: TdxLinqExpression): IdxLinqOrderBy<T>;
begin
  inherited OrderBy(AExpression);
  Result := Self;
end;

function TdxLinqQueryBuilder<T>.OrderBy(const AExpression1, AExpression2: TdxLinqExpression): IdxLinqOrderBy<T>;
begin
  inherited OrderBy(AExpression1, AExpression2);
  Result := Self;
end;

function TdxLinqQueryBuilder<T>.OrderBy(const AExpression1, AExpression2, AExpression3: TdxLinqExpression): IdxLinqOrderBy<T>;
begin
  inherited OrderBy(AExpression1, AExpression2, AExpression3);
  Result := Self;
end;

function TdxLinqQueryBuilder<T>.OrderBy(const AExpression1, AExpression2, AExpression3,
  AExpression4: TdxLinqExpression): IdxLinqOrderBy<T>;
begin
  inherited OrderBy(AExpression1, AExpression2, AExpression3, AExpression4);
  Result := Self;
end;

function TdxLinqQueryBuilder<T>.OrderBy(const AExpression1, AExpression2, AExpression3, AExpression4,
  AExpression5: TdxLinqExpression): IdxLinqOrderBy<T>;
begin
  inherited OrderBy(AExpression1, AExpression2, AExpression3, AExpression4, AExpression5);
  Result := Self;
end;

function TdxLinqQueryBuilder<T>.OrderBy(const AExpression1, AExpression2, AExpression3, AExpression4, AExpression5,
  AExpression6: TdxLinqExpression): IdxLinqOrderBy<T>;
begin
  inherited OrderBy(AExpression1, AExpression2, AExpression3, AExpression4, AExpression5, AExpression6);
  Result := Self;
end;

function TdxLinqQueryBuilder<T>.OrderBy(const AExpression1, AExpression2, AExpression3, AExpression4, AExpression5,
  AExpression6, AExpression7: TdxLinqExpression): IdxLinqOrderBy<T>;
begin
  inherited OrderBy(AExpression1, AExpression2, AExpression3, AExpression4, AExpression5, AExpression6,
    AExpression7);
  Result := Self;
end;

function TdxLinqQueryBuilder<T>.OrderBy(const AExpressions: array of TdxLinqExpression): IdxLinqOrderBy<T>;
begin
  inherited OrderBy(AExpressions);
  Result := Self;
end;
{$ENDREGION}

function TdxLinqQueryBuilder<T>.Select: IdxLinqSelect<T>;
begin
  Result := Self;
end;


function TdxLinqQueryBuilder<T>.Skip(ACount: Integer): IdxLinqSkip<T>;
begin
  inherited Skip(ACount);
  Result := Self;
end;

function TdxLinqQueryBuilder<T>.SkipTake(ACount: Integer): IdxQueryable<T>;
begin
  inherited Take(ACount);
  Result := Self;
end;

function TdxLinqQueryBuilder<T>.Take(ACount: Integer): IdxLinqTake<T>;
begin
  inherited Take(ACount);
  Result := Self;
end;

function TdxLinqQueryBuilder<T>.TakeSkip(ACount: Integer): IdxQueryable<T>;
begin
  inherited Skip(ACount);
  Result := Self;
end;

function TdxLinqQueryBuilder<T>.Where(const AExpression: TdxLinqExpression): IdxLinqWhere<T>;
begin
  inherited Where(AExpression);
  Result := Self;
end;

{ TdxLinqQuery.TOrderBy }

procedure TdxLinqQueryBuilder.TOrderBy.SetDescendingAtMark;
var
  I: Integer;
begin
  for I := FLastIndex to Count - 1 do
    (Items[I] as TdxSortByExpression).SortDirection := TdxSortDirection.Descending;
end;

procedure TdxLinqQueryBuilder.TOrderBy.SetMark;
begin
  FLastIndex := Count;
end;

{ TdxLinqQuery.TEmptyEnumerator }

function TdxLinqQueryBuilder.TEmptyEnumerator.GetCurrent: TObject;
begin
  Result := nil;
end;

function TdxLinqQueryBuilder.TEmptyEnumerator.MoveNext: Boolean;
begin
  Result := False;
end;

procedure TdxLinqQueryBuilder.TEmptyEnumerator.Reset;
begin
  // do nothing
end;

{ TdxLinqBuilder }

constructor TdxLinqBuilder.Create(AOwner: TdxLinqQueryBuilder);
begin
  inherited Create;
  FOwner := AOwner;
end;

function TdxLinqBuilder.CreateQuery(const AExpression: IdxExpression): IdxQueryable;
begin
  NotImplemented;
  Result := nil;
end;

function TdxLinqBuilder.GetEnumerator: IEnumerator;
begin
  Result := nil;
end;

{ TdxLinqBuilder<T>.TEnumerator }

procedure TdxLinqBuilder<T>.TEnumerator.ClearCurrentValue(var AValue: T);
var
  AObject: TObject absolute AValue;
begin
  if FHasDispose then
    FreeAndNil(AObject)
  else
    AValue := Default(T);
end;

constructor TdxLinqBuilder<T>.TEnumerator.Create(ALinqBuilder: TdxLinqBuilder<T>);
var
  ALinqQueryBuilder: TdxLinqQueryBuilder;
begin
  FLinqBuilder := ALinqBuilder;
  ALinqQueryBuilder := FLinqBuilder.Owner as TdxLinqQueryBuilder;
  FOwnerEnumerator := ALinqQueryBuilder.GetEnumerator;
  Init;
end;

destructor TdxLinqBuilder<T>.TEnumerator.Destroy;
begin
  ClearCurrentValue(FCurrentValue);
  inherited Destroy;
end;

function TdxLinqBuilder<T>.TEnumerator.GetCurrent: TObject;
begin
  FOwnerEnumerator.GetCurrent;
end;

function TdxLinqBuilder<T>.TEnumerator.GetCurrentGeneric: T;
var
  AObject: TObject;
begin
  AObject := FOwnerEnumerator.GetCurrent;
  FLinqBuilder.Owner.FEntityMetadataInfo.CaptureInstance(AObject);
  ClearCurrentValue(FCurrentValue);
  Result := FLinqBuilder.GetValue(AObject);
  FCurrentValue := Result;
end;

procedure TdxLinqBuilder<T>.TEnumerator.Init;
var
  ATypeInfo: PTypeInfo;
begin
  ATypeInfo := TypeInfo(T);
  FIsClass := ATypeInfo.Kind = tkClass;
  if FIsClass then
    FHasDispose := EntityManager.GetEntityInfo(ATypeInfo.TypeData.ClassType) = nil;
end;

function TdxLinqBuilder<T>.TEnumerator.MoveNext: Boolean;
begin
  if FOwnerEnumerator = nil then
    Result := False
  else
    Result := FOwnerEnumerator.MoveNext;
  if not Result then
    FLinqBuilder.Owner.FEntityMetadataInfo.ReleaseInstance;
end;

procedure TdxLinqBuilder<T>.TEnumerator.Reset;
begin
  if FOwnerEnumerator <> nil then
    FOwnerEnumerator.Reset;
end;

{ TdxLinqBuilder<T> }

constructor TdxLinqBuilder<T>.Create(AOwner: TdxLinqQueryBuilder; const ANewFunc: TdxFunc<T>);
begin
  inherited Create(AOwner);
  FOnNew := ANewFunc;
end;

constructor TdxLinqBuilder<T>.Create(AOwner: TdxLinqQueryBuilder; const ANewFunc: TdxFunc<TArray<Variant>, T>);
begin
  inherited Create(AOwner);
  FOnNewFromVariants := ANewFunc;
end;

function TdxLinqBuilder<T>.GetProvider: IdxQueryProvider<T>;
begin
  Result := Self;
end;

function TdxLinqBuilder<T>.GetTuple(AInstance: Pointer): TObject;
begin
  NotImplemented;
  Result := nil;
end;

function TdxLinqBuilder<T>.GetValue(AInstance: Pointer): T;
var
  ACurrent: T;
  AObject: TObject absolute ACurrent;
begin
  if Assigned(FOnNew) then
    Result := FOnNew
  else
  if Assigned(FOnNewFromVariants) then
    Result := FOnNewFromVariants(GetMemberValues(AInstance))
  else
  begin
    AObject := GetTuple(AInstance);
    Result := ACurrent;
  end;
end;

function TdxLinqBuilder<T>.CreateQueryGeneric(const AExpression: IdxExpression): IdxQueryable<T>;
begin
  Result := nil;
end;

function TdxLinqBuilder<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := TEnumerator.Create(Self);
end;

function TdxLinqBuilder<T>.GetMemberValues(AInstance: Pointer): TArray<Variant>;
begin
  if Owner.IsEntity then
  begin
    if FDataManager = nil then
      FDataManager := EntityManager.GetEntityInfo(Owner.FromClass);
  end
  else
    NotImplemented;
  Result := TdxEntityInfo(FDataManager).GetMemberValues(AInstance);
end;

{ TdxLinqBuilder<TFrom: class; T> }

constructor TdxLinqBuilder<TFrom; T>.Create(const AOwner: IdxQueryable<TFrom>);
begin
end;

{ TdxLinqFrom }

function TdxLinqFrom.From(const AEntityInfo: IdxEntityInfo): IdxLinqFrom;
var
  ALinqQuery: TdxLinqQueryBuilder;
begin
  ALinqQuery := TdxLinqQueryBuilder.Create;
  Result := ALinqQuery.From(AEntityInfo);
end;

function TdxLinqFrom.From<T>(const AEntityInfo: IdxEntityInfo): IdxLinqFrom<T>;
var
  ALinqQuery: TdxLinqQueryBuilder<T>;
begin
  ALinqQuery := TdxLinqQueryBuilder<T>.Create;
  ALinqQuery.From(AEntityInfo);
  Result := ALinqQuery;
  if ALinqQuery.FromClass <> TClass(T) then
    raise Exception.Create(sdxErrorMessage);
end;

function TdxLinqFrom.From<T>: IdxLinqSimpleFrom<T>;
var
  ALinqQuery: TdxLinqQueryBuilder<T>;
begin
  ALinqQuery := TdxLinqQueryBuilder<T>.Create(FDataContext);
  Result := (ALinqQuery as TdxLinqQueryBuilder<T>).FromBuilder;
end;

class operator TdxLinqFrom.Implicit(const ADataContext: IdxDataContext): TdxLinqFrom;
begin
  Result.FDataContext := ADataContext;
end;

{$REGION 'VirtualInterface'}

{$IFNDEF DELPHIXE2}
type
  TVirtualInterfaceInvokeEvent = reference to procedure(Method: TRttiMethod;
    const Args: TArray<TValue>; out Result: TValue);

  TVirtualInterface = class(TInterfacedObject, IInterface)
  private type

    TImplInfo = class
    private
      FImpl: TMethodImplementation;
      FMethod: TRttiMethod;
      function GetCodeAddress: Pointer;
      function GetVirtualIndex: SmallInt;
    public
      constructor Create(AMethod: TRttiMethod; const ACallback: TMethodImplementationCallback);
      destructor Destroy; override;
      property CodeAddress: Pointer read GetCodeAddress;
      property VirtualIndex: SmallInt read GetVirtualIndex;
    end;

  private
    VTable: PPointer;
    FIID: TGUID;
    FContext: TRttiContext;
    FIntercepts: TObjectList<TImplInfo>;
    FOnInvoke: TVirtualInterfaceInvokeEvent;
    function _AddRefFromIntf: Integer; stdcall;
    function _ReleaseFromIntf: Integer; stdcall;
    function _QIFromIntf(const IID: TGUID; out AObject): HResult; stdcall;

    procedure RawCallback(AUserData: Pointer; const AArgs: TArray<TValue>; out AResult: TValue); virtual;
    procedure ErrorProc;
  protected
    function QueryInterface(const IID: TGUID; out AObject): HResult; virtual; stdcall;
  public
    constructor Create(PIID: PTypeInfo); overload;
    constructor Create(PIID: PTypeInfo; InvokeEvent: TVirtualInterfaceInvokeEvent); overload;
    destructor Destroy; override;
    property OnInvoke: TVirtualInterfaceInvokeEvent read FOnInvoke write FOnInvoke;
  end;
{$POINTERMATH ON}
  PVTablePtr = ^Pointer;
{$POINTERMATH OFF}
  PPVTable = ^PVTable;
  PVTable = ^TVTable;
  TVTable = array[0..MaxInt div SizeOf(Pointer) - 1] of Pointer;

  PInterceptFrame = Pointer;


{ TVirtualInterface }

constructor TVirtualInterface.Create(PIID: PTypeInfo);
var
  AMethods: TArray<TRttiMethod>;
  AMethod: TRttiMethod;
  ATyp: TRttiType;
  AMaxIndex, I: Integer;
begin
  FIntercepts := TObjectList<TImplInfo>.Create(True);
  ATyp := FContext.GetType(PIID);
  FIID := TRttiInterfaceType(ATyp).GUID;

  AMethods := ATyp.GetMethods;
  AMaxIndex := 2;
  for AMethod in AMethods do
  begin
    if AMaxIndex < AMethod.VirtualIndex then
      AMaxIndex := AMethod.VirtualIndex;
    FIntercepts.Add(TImplInfo.Create(AMethod, RawCallBack));
  end;

  VTable := AllocMem(SizeOf(Pointer) * (AMaxIndex + 1));
  PVTablePtr(VTable)[0] := @TVirtualInterface._QIFromIntf;
  PVTablePtr(VTable)[1] := @TVirtualInterface._AddRefFromIntf;
  PVTablePtr(VTable)[2] := @TVirtualInterface._ReleaseFromIntf;
  for I := 0 to FIntercepts.Count-1 do
    PVTablePtr(VTable)[FIntercepts[I].VirtualIndex] := FIntercepts[I].CodeAddress;
  for I := 3 to AMaxIndex do
    if PVTablePtr(VTable)[I] = nil then
      PVTablePtr(VTable)[I] := @TVirtualInterface.ErrorProc;
end;

constructor TVirtualInterface.Create(PIID: PTypeInfo;
  InvokeEvent: TVirtualInterfaceInvokeEvent);
begin
  Create(PIID);
  FOnInvoke := InvokeEvent;
end;

destructor TVirtualInterface.Destroy;
begin
  if VTable <> nil then
    FreeMem(VTable);
  FIntercepts.Free;
  inherited;
end;

procedure TVirtualInterface.RawCallback(AUserData: Pointer; const AArgs: TArray<TValue>; out AResult: TValue);
begin
  if Assigned(FOnInvoke) then
    FOnInvoke(TImplInfo(AUserData).FMethod, AArgs, AResult);
end;

procedure TVirtualInterface.ErrorProc;
begin
  raise EInsufficientRtti.CreateRes(@SInsufficientRtti);
end;

function TVirtualInterface._AddRefFromIntf: Integer;
begin
  Result := TVirtualInterface(PByte(Self) - (PByte(@Self.VTable) - PByte(Self)))._AddRef;
end;

function TVirtualInterface._ReleaseFromIntf: Integer;
begin
  Result := TVirtualInterface(PByte(Self) - (PByte(@Self.VTable) - PByte(Self)))._Release;
end;

function TVirtualInterface._QIFromIntf(const IID: TGUID; out AObject): HResult;
begin
  Result := TVirtualInterface(PByte(Self) - (PByte(@Self.VTable) - PByte(Self))).QueryInterface(IID, AObject);
end;

function TVirtualInterface.QueryInterface(const IID: TGUID; out AObject): HResult;
begin
  if IID = FIID then
  begin
    _AddRef;
    Pointer(AObject) := @VTable;
    Result := S_OK;
  end
  else
    Result := inherited
end;

{ TVirtualInterface.TImplInfo }

constructor TVirtualInterface.TImplInfo.Create(AMethod: TRttiMethod;
  const ACallback: TMethodImplementationCallback);
begin
  FImpl := AMethod.CreateImplementation(Pointer(Self), ACallback);
  FMethod := AMethod;
end;

destructor TVirtualInterface.TImplInfo.Destroy;
begin
  FImpl.Free;
  inherited;
end;

function TVirtualInterface.TImplInfo.GetCodeAddress: Pointer;
begin
  Result := FImpl.CodeAddress;
end;

function TVirtualInterface.TImplInfo.GetVirtualIndex: SmallInt;
begin
  Result := FMethod.VirtualIndex;
end;

{$ENDIF}

{$ENDREGION}

type
  TdxEMFCustomSessionAccess = class(TdxEMFCustomSession);

  { TdxEntityMetadataInfo }

  TdxEntityMetadataInfo = class(TInterfacedObject, IdxEntityMetadataInfo, IdxEntityInfo)
  strict private
    FClass: TClass;
    FOwner: IdxQueryProvider;
    FInstance: Pointer;
  protected
    procedure CaptureInstance(AInstance: Pointer);
    procedure ReleaseInstance;
    function GetEntityClass: TClass;
    function GetProvider: IdxQueryProvider;
    function GetDataContext: IdxDataContext;
    function FieldByName(const AIndex: string): TdxLinqExpression;
  public
    constructor Create(AClass: TClass; const AOwner: IdxQueryProvider); overload;
  end;

  { TdxEntityMetadata }

  TdxEntityMetadata = class(TVirtualInterface, IdxEntityMetadataInfo, IdxEntityInfo)
  strict private type

    TCustomEntityMember = class(TInterfacedObject)
    private
      FEntityMetadata: TdxEntityMetadata;
      FMemberInfo: TdxMappingMemberInfo;
    protected
      function GetMemberName: string;
    public
      constructor Create(AEntityMetadata: TdxEntityMetadata; AMemberInfo: TdxMappingMemberInfo);
      property EntityMetadata: TdxEntityMetadata read FEntityMetadata;
      property MemberInfo: TdxMappingMemberInfo read FMemberInfo;
      property MemberName: string read GetMemberName;
    end;

    TEntityMember = class(TCustomEntityMember, IdxEntityMemberHolder)
    protected
      function AsVariant: Variant;
    end;

    TCollectionExpression = class(TVirtualInterface, IdxLinqCollectionExpression)
    strict private
      class var
        FAggregateFunctionTypes: TDictionary<string, TdxAggregateFunctionType>;
      class destructor Destroy;
      class procedure PopulateAggregateFunctionTypes;
    private
      FEntityMetadata: TdxEntityMetadata;
      FMemberInfo: TdxMappingMemberInfo;
      function GetMemberName: string; inline;
    protected
      procedure InvokeEvent(Method: TRttiMethod; const Args: TArray<TValue>; out Result: TValue);
      // aggregate expressions
      function Average(const AValue: TdxLinqExpression): TdxLinqExpression;
      function Count: TdxLinqExpression; overload;
      function Count(const AValue: TdxLinqExpression): TdxLinqExpression; overload;
      function Max(const AValue: TdxLinqExpression): TdxLinqExpression;
      function Min(const AValue: TdxLinqExpression): TdxLinqExpression;
      function Sum(const AValue: TdxLinqExpression): TdxLinqExpression;

      property MemberName: string read GetMemberName;
    public
      constructor Create(PIID: PTypeInfo; AEntityMetadata: TdxEntityMetadata; AMemberInfo: TdxMappingMemberInfo); overload;
      constructor Create(AEntityMetadata: TdxEntityMetadata; AMemberInfo: TdxMappingMemberInfo); overload;
    end;

  strict private
    FHandle: PTypeInfo;
    FClass: TClass;
    FInstance: Pointer;
    FEntityInfo: TdxEntityInfo;
    FOwner: IdxQueryProvider;
    FExpressions: TDictionary<string, TdxLinqExpression>;
    function GetIsCapture: Boolean; inline;
  protected
    procedure EntityMetadataInvokeEvent(Method: TRttiMethod; const Args: TArray<TValue>; out Result: TValue);
    procedure CaptureInstance(AInstance: Pointer);
    procedure ReleaseInstance;
    function GetEntityClass: TClass;
    function GetProvider: IdxQueryProvider;
    function GetDataContext: IdxDataContext;
    function FieldByName(const AName: string): TdxLinqExpression;

    property Instance: Pointer read FInstance;
    property IsCapture: Boolean read GetIsCapture;
  public
    constructor Create(PIID: PTypeInfo; AClass: TClass; const AOwner: IdxQueryProvider); overload;
    destructor Destroy; override;
    property Handle: PTypeInfo read FHandle;
  end;

  { TdxDataContext }

  TdxDataContext = class(TVirtualInterface, IdxDataContext)
  strict private type

    TInfo = record
      EntityInfo: IdxEntityInfo;
      Handle: PTypeInfo;
    end;

  strict private
    FSession: IdxSession;
    FEntities: TDictionary<string, TInfo>;
    procedure PopulateEntities(PIID: PTypeInfo);
  protected
    procedure DataContextInvokeEvent(Method: TRttiMethod; const Args: TArray<TValue>; out Result: TValue);
    function GetSession: IdxSession;
    function EntityByName(const AName: string): IdxEntityInfo;
    function GetEntityInfo(AClass: TClass): IdxEntityInfo; overload;
  public
    constructor Create(PIID: PTypeInfo; const ASession: IdxSession); overload;
    constructor Create(const ASession: IdxSession); overload;
    destructor Destroy; override;
    property Session: IdxSession read FSession;
  end;

{ TdxEntityMetadata.TCustomEntityMember }

constructor TdxEntityMetadata.TCustomEntityMember.Create(AEntityMetadata: TdxEntityMetadata;
  AMemberInfo: TdxMappingMemberInfo);
begin
  inherited Create;
  FEntityMetadata := AEntityMetadata;
  FMemberInfo := AMemberInfo;
end;

function TdxEntityMetadata.TCustomEntityMember.GetMemberName: string;
begin
  Result := FMemberInfo.MemberName;
end;

{ TdxEntityMetadata.TEntityMember }

function TdxEntityMetadata.TEntityMember.AsVariant: Variant;
begin
  Result := FMemberInfo.GetValue(FEntityMetadata.Instance).ToVariant;
end;

{ TdxEntityMetadata.TCollectionExpression }

constructor TdxEntityMetadata.TCollectionExpression.Create(PIID: PTypeInfo; AEntityMetadata: TdxEntityMetadata;
  AMemberInfo: TdxMappingMemberInfo);
begin
  inherited Create(PIID, InvokeEvent);
  FEntityMetadata := AEntityMetadata;
  FMemberInfo := AMemberInfo;
end;

constructor TdxEntityMetadata.TCollectionExpression.Create(AEntityMetadata: TdxEntityMetadata;
  AMemberInfo: TdxMappingMemberInfo);
begin
  inherited Create;
  FEntityMetadata := AEntityMetadata;
  FMemberInfo := AMemberInfo;
end;

class destructor TdxEntityMetadata.TCollectionExpression.Destroy;
begin
  FreeAndNil(FAggregateFunctionTypes);
end;

class procedure TdxEntityMetadata.TCollectionExpression.PopulateAggregateFunctionTypes;
begin
  FAggregateFunctionTypes.Add('average', TdxAggregateFunctionType.Avg);
  FAggregateFunctionTypes.Add('count', TdxAggregateFunctionType.Count);
  FAggregateFunctionTypes.Add('max', TdxAggregateFunctionType.Max);
  FAggregateFunctionTypes.Add('min', TdxAggregateFunctionType.Min);
  FAggregateFunctionTypes.Add('sum', TdxAggregateFunctionType.Sum);
end;

function TdxEntityMetadata.TCollectionExpression.GetMemberName: string;
begin
  Result := FMemberInfo.MemberName;
end;

function TdxEntityMetadata.TCollectionExpression.Average(const AValue: TdxLinqExpression): TdxLinqExpression;
begin
  raise ENotImplemented.Create('');
end;

function TdxEntityMetadata.TCollectionExpression.Count: TdxLinqExpression;
var
  AOperandProperty: IdxOperandProperty;
begin
  AOperandProperty := TdxOperandProperty.Create(MemberName);
  Result := TdxAggregateOperand.Create(AOperandProperty, nil, TdxAggregateFunctionType.Count, nil);
end;

function TdxEntityMetadata.TCollectionExpression.Count(const AValue: TdxLinqExpression): TdxLinqExpression;
begin
  raise ENotImplemented.Create('');
end;

procedure TdxEntityMetadata.TCollectionExpression.InvokeEvent(Method: TRttiMethod; const Args: TArray<TValue>;
  out Result: TValue);

  function GetArgument: TdxLinqExpression;
  begin
    if Length(Args) <> 2 then
      raise EArgumentException.Create('');
    Result := Args[1].AsType<TdxLinqExpression>;
  end;

var
  AAggregateFunc: TdxAggregateFunctionType;
begin
  if FAggregateFunctionTypes = nil then
  begin
    FAggregateFunctionTypes := TDictionary<string, TdxAggregateFunctionType>.Create;
    PopulateAggregateFunctionTypes;
  end;
  if FAggregateFunctionTypes.TryGetValue(LowerCase(Method.Name), AAggregateFunc) then
  begin
    case AAggregateFunc of
      TdxAggregateFunctionType.Count:
        if Length(Args) = 1 then
          Result := TValue.From(Count)
        else
          Result := TValue.From(Count(GetArgument));
      TdxAggregateFunctionType.Max:
        Result := TValue.From(Max(GetArgument));
      TdxAggregateFunctionType.Min:
        Result := TValue.From(Min(GetArgument));
      TdxAggregateFunctionType.Avg:
        Result := TValue.From(Average(GetArgument));
      TdxAggregateFunctionType.Sum:
        Result := TValue.From(Sum(GetArgument));
    end;
  end;
end;

function TdxEntityMetadata.TCollectionExpression.Max(const AValue: TdxLinqExpression): TdxLinqExpression;
begin
  raise ENotImplemented.Create('');
end;

function TdxEntityMetadata.TCollectionExpression.Min(const AValue: TdxLinqExpression): TdxLinqExpression;
begin
  raise ENotImplemented.Create('');
end;

function TdxEntityMetadata.TCollectionExpression.Sum(const AValue: TdxLinqExpression): TdxLinqExpression;
begin
  raise ENotImplemented.Create('');
end;

{ TdxEntityMetadata }

constructor TdxEntityMetadata.Create(PIID: PTypeInfo; AClass: TClass; const AOwner: IdxQueryProvider);
begin
  inherited Create(PIID, EntityMetadataInvokeEvent);
  FHandle := PIID;
  FClass := AClass;
  FOwner := AOwner;
  FEntityInfo := EntityManager.GetEntityInfo(AClass);
end;

destructor TdxEntityMetadata.Destroy;
begin
  FreeAndNil(FExpressions);
  inherited Destroy;
end;

function TdxEntityMetadata.GetIsCapture: Boolean;
begin
  Result := FInstance <> nil;
end;

procedure TdxEntityMetadata.CaptureInstance(AInstance: Pointer);
begin
  FInstance := AInstance;
  if FExpressions = nil then
    FExpressions := TDictionary<string, TdxLinqExpression>.Create;
end;

procedure TdxEntityMetadata.EntityMetadataInvokeEvent(Method: TRttiMethod; const Args: TArray<TValue>; out Result: TValue);

  function GetLinqCollectionType(AType: TRttiType): TRttiType;
  begin
    if AType = nil then
      Exit(nil);
    if AType.Handle = TypeInfo(IdxLinqCollectionExpression) then
      Result := AType
    else
      Result := GetLinqCollectionType(AType.BaseType);
  end;

var
  AReturnType: TRttiType;
  AReturnValue: TValue;
  AMember: TdxMappingMemberInfo;
  ALinqCollectionExpression: IdxLinqCollectionExpression;
begin
  if SameText(Method.Name, 'FieldByName') then
    Result := TValue.From<TdxLinqExpression>(FieldByName(Args[1].AsString))
  else
  begin
    AReturnType := Method.ReturnType;
    if AReturnType.TypeKind = tkInterface then
    begin
      if not FEntityInfo.Members.TryGetValue(Method.Name, AMember) then
        raise Exception.Create('sdxFieldNotFound');
      if AReturnType.Handle = TypeInfo(IdxLinqCollectionExpression) then
      begin
        ALinqCollectionExpression := TCollectionExpression.Create(Self, AMember);
        Result := TValue.From(ALinqCollectionExpression);
      end
      else
      begin
        if GetLinqCollectionType(AReturnType) = nil then
        begin
          AReturnValue := Method.Invoke(FInstance, []);
          raise EIntfCastError.Create('IdxLinqCollectionExpression');
        end
        else
        begin
          Supports(TCollectionExpression.Create(AReturnType.Handle, Self, AMember), AReturnType.Handle.TypeData.Guid,
            ALinqCollectionExpression);
          TValue.Make(@ALinqCollectionExpression, AReturnType.Handle, Result);
        end;
      end;
    end
    else
      Result := TValue.From<TdxLinqExpression>(FieldByName(Method.Name));
  end;
end;

function TdxEntityMetadata.GetDataContext: IdxDataContext;
begin
  Result := FOwner as IdxDataContext;
end;

function TdxEntityMetadata.GetEntityClass: TClass;
begin
  Result := FClass;
end;

function TdxEntityMetadata.FieldByName(const AName: string): TdxLinqExpression;
var
  AMember: TdxMappingMemberInfo;
  AEntityMemberHolder: IdxEntityMemberHolder;
begin
  if (FEntityInfo <> nil) and IsCapture then
  begin
    if not FExpressions.TryGetValue(AName, Result) then
    begin
      if not FEntityInfo.Members.TryGetValue(AName, AMember) then
        raise Exception.CreateFmt(sdxLinqExpressionFieldNotFound, [AName, FEntityInfo.ClassAttributes.EntityName]);
      AEntityMemberHolder := TEntityMember.Create(Self, AMember);
//      Result := TdxLinqExpression(AEntityMemberHolder);
      FExpressions.Add(AName, Result);
    end;
  end
  else
    Result := TdxLinqExpression(AName);
end;

function TdxEntityMetadata.GetProvider: IdxQueryProvider;
begin
  Result := FOwner;
end;

procedure TdxEntityMetadata.ReleaseInstance;
begin
  FInstance := nil;
end;

{ TdxLinqExpressionFactory }

class constructor TdxLinqExpressionFactory.Create;
begin
  FDataContexts := TList<PTypeInfo>.Create;
end;

class destructor TdxLinqExpressionFactory.Destroy;
begin
  FreeAndNil(FExpressions);
  FreeAndNil(FDataContexts);
end;

class function TdxLinqExpressionFactory.FindClass(AExpression: PTypeInfo): TClass;
var
  I: TPair<TClass, PTypeInfo>;
begin
  for I in FExpressions do
    if I.Value = AExpression then
      Exit(I.Key);
  Result := nil;
end;

class function TdxLinqExpressionFactory.FindClass(const AIID: TGUID): TClass;
var
  I: TPair<TClass, PTypeInfo>;
begin
  for I in FExpressions do
    if I.Value.TypeData.Guid = AIID then
      Exit(I.Key);
  Result := nil;
end;

class function TdxLinqExpressionFactory.GetDataContext(const ASession: IdxSession): IdxDataContext;
begin
  Result := TdxDataContext.Create(ASession);
end;

class function TdxLinqExpressionFactory.GetDataContext(ADataContext: PTypeInfo; const ASession: IdxSession): IdxDataContext;
begin
  Result := TdxDataContext.Create(ADataContext, ASession);
end;

class function TdxLinqExpressionFactory.GetDataContexts: TArray<PTypeInfo>;
begin
  if FDataContexts = nil then
    Result := nil
  else
    Result := FDataContexts.ToArray;
end;

class function TdxLinqExpressionFactory.DataContextEntityClasses(ADataContext: PTypeInfo): TArray<TClass>;
var
  AType: TRttiType;
  AMethods: TArray<TRttiMethod>;
  AMethod: TRttiMethod;
  AItem: TPair<TClass, PTypeInfo>;
  AResult: TList<TClass>;
begin
  if DataContexts.IndexOf(ADataContext) = - 1 then
    Exit(nil);
  AType := TRttiContext.Create.GetType(ADataContext);
  AResult := TList<TClass>.Create;
  try
    AMethods := AType.GetDeclaredMethods;
    for AMethod in AMethods do
      if AMethod.MethodKind = TMethodKind.mkFunction then
      begin
        AType := AMethod.ReturnType;
        for AItem in Expressions do
        begin
          if AItem.Value = AType.Handle then
          begin
            AResult.Add(AItem.Key);
            Break;
          end;
        end;
      end;
    Result := AResult.ToArray;
  finally
    AResult.Free;
  end;
end;

class function TdxLinqExpressionFactory.GetEntityMetadata(AClass: TClass; const AOwner: IdxQueryProvider): IdxEntityInfo;
var
  AExpression: PTypeInfo;
begin
  if not TryGetExpression(AClass, AExpression) then
    Exit(TdxEntityMetadataInfo.Create(AClass, AOwner));
  Supports(TdxEntityMetadata.Create(AExpression, AClass, AOwner), AExpression.TypeData.Guid, Result);
end;


class procedure TdxLinqExpressionFactory.Register<TDataContext>;
begin
  RegisterDataContext(TypeInfo(TDataContext));
end;

class procedure TdxLinqExpressionFactory.Register<TEntity, TExpression>;
begin
  RegisterExpression(TClass(TEntity), TypeInfo(TExpression));
end;

class procedure TdxLinqExpressionFactory.RegisterDataContext(ADataContext: PTypeInfo);
begin
  if FDataContexts = nil then
    FDataContexts := TList<PTypeInfo>.Create;
  if FDataContexts.IndexOf(ADataContext) = -1 then
    FDataContexts.Add(ADataContext);
end;



class procedure TdxLinqExpressionFactory.RegisterExpression(AClass: TClass; AExpression: PTypeInfo);
var
  AEntityInfo: TdxEntityInfo;
begin
  if FExpressions = nil then
    FExpressions := TDictionary<TClass, PTypeInfo>.Create;
  AEntityInfo := EntityManager.GetEntityInfo(AClass);
  if AEntityInfo = nil then
    raise Exception.Create(sdxErrorMessage);
  FExpressions.AddOrSetValue(AClass, AExpression);
end;

class function TdxLinqExpressionFactory.TryGetExpression(AClass: TClass; out AExpression: PTypeInfo): Boolean;
begin
  if FExpressions = nil then
    Result := False
  else
    Result := FExpressions.TryGetValue(AClass, AExpression);
end;

class procedure TdxLinqExpressionFactory.UnRegister(AClass: TClass);
begin
  FExpressions.Remove(AClass);
end;

class procedure TdxLinqExpressionFactory.UnRegister(AClasses: array of TClass);
var
  AClass: TClass;
begin
  for AClass in AClasses do
    UnRegister(AClass);
end;

class procedure TdxLinqExpressionFactory.UnRegister<TDataContext>;
begin
  FDataContexts.Extract(TypeInfo(TDataContext));
end;

{ TdxEntityMetadataInfo }

constructor TdxEntityMetadataInfo.Create(AClass: TClass; const AOwner: IdxQueryProvider);
begin
  inherited Create;
  FClass := AClass;
  FOwner := AOwner;
end;

procedure TdxEntityMetadataInfo.CaptureInstance(AInstance: Pointer);
begin
  FInstance := AInstance;
end;

function TdxEntityMetadataInfo.GetDataContext: IdxDataContext;
begin
  Result := FOwner as IdxDataContext;
end;

function TdxEntityMetadataInfo.GetEntityClass: TClass;
begin
  Result := FClass;
end;


function TdxEntityMetadataInfo.FieldByName(const AIndex: string): TdxLinqExpression;
begin
  Result := TdxLinqExpression(AIndex);
end;

function TdxEntityMetadataInfo.GetProvider: IdxQueryProvider;
begin
  Result := FOwner;
end;

procedure TdxEntityMetadataInfo.ReleaseInstance;
begin
  FInstance := nil;
end;

{ TdxLinqContext }

constructor TdxDataContext.Create(const ASession: IdxSession);
begin
  inherited Create;
  FSession := ASession;
  FEntities := TDictionary<string, TInfo>.Create;
end;

constructor TdxDataContext.Create(PIID: PTypeInfo; const ASession: IdxSession);
begin
  inherited Create(PIID, DataContextInvokeEvent);
  FSession := ASession;
  FEntities := TDictionary<string, TInfo>.Create;
  PopulateEntities(PIID);
end;

destructor TdxDataContext.Destroy;
begin
  FreeAndNil(FEntities);
  inherited Destroy;
end;

function TdxDataContext.GetSession: IdxSession;
begin
  Result := FSession;
end;

procedure TdxDataContext.PopulateEntities(PIID: PTypeInfo);
var
  AContext: TRttiContext;
  AType: TRttiInterfaceType;
  AMethods: TArray<TRttiMethod>;
  AReturnType: TRttiInterfaceType;
  AClass: TClass;
  AEntityInfo: IdxEntityInfo;
  AOwner: IdxQueryProvider;
  AInfo: TInfo;
  I: Integer;
begin
  AContext := TRttiContext.Create;
  try
    Supports(FSession, IdxQueryProvider, AOwner);
    AType := AContext.GetType(PIID) as TRttiInterfaceType;
    AMethods := AType.GetDeclaredMethods;
    for I := 0 to Length(AMethods) - 1 do
    begin
      AReturnType := AMethods[I].ReturnType as TRttiInterfaceType;
      AClass := TdxLinqExpressionFactory.FindClass(AReturnType.Handle);
      AEntityInfo := TdxLinqExpressionFactory.GetEntityMetadata(AClass, AOwner);
      AInfo.EntityInfo := AEntityInfo;
      AInfo.Handle := AReturnType.Handle;
      FEntities.Add(UpperCase(AMethods[I].Name), AInfo);
    end;
  finally
    AContext.Free;
  end;
end;

procedure TdxDataContext.DataContextInvokeEvent(Method: TRttiMethod; const Args: TArray<TValue>; out Result: TValue);
var
  AInfo: TInfo;
  ASource: Pointer;
begin
  if SameText(Method.Name, 'GetSession') then
    Result := TValue.From<IdxSession>(GetSession)
  else
  if SameText(Method.Name, 'EntityByName') then
    Result := TValue.From<IdxEntityInfo>(EntityByName(Args[1].AsString))
  else
  if SameText(Method.Name, 'GetEntityInfo') then
    Result := TValue.From<IdxEntityInfo>(GetEntityInfo(Args[1].AsType<TClass>))
  else
  begin
    if not FEntities.TryGetValue(UpperCase(Method.Name), AInfo) then
    begin
      Result := TValue.Empty;
      Exit;
    end;
    ASource := Pointer(AInfo.EntityInfo);
    TValue.Make(@ASource, AInfo.Handle, Result);
  end;
end;

function TdxDataContext.EntityByName(const AName: string): IdxEntityInfo;
var
  AEntityName: string;
  AEntityInfo: TdxEntityInfo;
  AInfo: TInfo;
begin
  AEntityName := UpperCase(AName);
  if FEntities.TryGetValue(AEntityName, AInfo) then
  begin
    Exit(AInfo.EntityInfo);
  end;
  AEntityInfo := EntityManager.GetEntityInfo(AEntityName);
  if AEntityInfo = nil then
    raise Exception.Create(AName + ' not found');
  Result := GetEntityInfo(AEntityInfo.ClassAttributes.PersistentClass);
  AInfo.EntityInfo := Result;
  FEntities.Add(AEntityName, AInfo);
end;

function TdxDataContext.GetEntityInfo(AClass: TClass): IdxEntityInfo;
var
  AQueryProvider: IdxQueryProvider;
begin
  Supports(Session, IdxQueryProvider, AQueryProvider);
  Result := TdxLinqExpressionFactory.GetEntityMetadata(AClass, AQueryProvider);
end;


end.
