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

unit dxEMF.DB.Criteria;

interface

{$I cxVer.inc}
{$I dxEMF.inc}

uses
  Types, Classes, SysUtils, Generics.Defaults, Generics.Collections, Rtti,
  dxEMF.Types,
  dxEMF.Utils;

type

  TdxCriteriaOperator = class;
  TdxOperandValue = class;
  TdxBetweenOperator = class;
  TdxBinaryOperator = class;
  TdxUnaryOperator = class;
  TdxInOperator = class;
  TdxGroupOperator = class;
  TdxFunctionOperator = class;
  TdxOperandProperty = class;
  TdxJoinOperand = class;
  TdxQueryOperand = class;
  TdxAggregateOperand = class;
  TdxCriteriaOperatorList = class;

  IdxOperandValue = interface;
  IdxBetweenOperator = interface;
  IdxBinaryOperator = interface;
  IdxUnaryOperator = interface;
  IdxInOperator = interface;
  IdxGroupOperator = interface;
  IdxFunctionOperator = interface;
  IdxOperandProperty = interface;
  IdxJoinOperand = interface;
  IdxQueryOperand = interface;
  IdxAggregateOperand = interface;

  IdxCriteriaOperatorCollection = interface;

  { IdxCriteriaOperator }

  IdxCriteriaOperator = interface(IdxExpression)
  ['{F5957C1A-1CD6-4E57-8E00-CA779D7D44D4}']
  end;

  { IdxCriteriaVisitor }

  IdxCriteriaVisitor = interface
    procedure Visit(const AOperator: IdxBetweenOperator); overload;
    procedure Visit(const AOperator: IdxBinaryOperator); overload;
    procedure Visit(const AOperator: IdxUnaryOperator); overload;
    procedure Visit(const AOperator: IdxInOperator); overload;
    procedure Visit(const AOperator: IdxGroupOperator); overload;
    procedure Visit(const AOperator: IdxOperandValue); overload;
    procedure Visit(const AOperator: IdxFunctionOperator); overload;
  end;

  IdxCriteriaVisitor<T> = interface
    function Visit(const AOperator: IdxBetweenOperator): T; overload;
    function Visit(const AOperator: IdxBinaryOperator): T; overload;
    function Visit(const AOperator: IdxUnaryOperator): T; overload;
    function Visit(const AOperator: IdxInOperator): T; overload;
    function Visit(const AOperator: IdxGroupOperator): T; overload;
    function Visit(const AOperator: IdxOperandValue): T; overload;
    function Visit(const AOperator: IdxFunctionOperator): T; overload;
  end;

  IdxClientCriteriaVisitor = interface(IdxCriteriaVisitor)
    procedure Visit(const AOperand: IdxOperandProperty); overload;
    procedure Visit(const AOperator: IdxAggregateOperand); overload;
    procedure Visit(const AOperator: IdxJoinOperand); overload;
  end;

  IdxClientCriteriaVisitor<T> = interface(IdxCriteriaVisitor<T>)
    function Visit(const AOperand: IdxOperandProperty): T; overload;
    function Visit(const AOperator: IdxAggregateOperand): T; overload;
    function Visit(const AOperator: IdxJoinOperand): T; overload;
  end;

  IdxCriteriaOperatorVisitor = interface(IdxClientCriteriaVisitor<IdxCriteriaOperator>)
  end;

  { IdxSQLGeneratorVisitor }

  IdxSQLGeneratorVisitor = interface(IdxCriteriaVisitor<string>)
    function Visit(const AOperand: IdxQueryOperand): string; overload;
  end;

  TdxListString = TList<string>;

  IdxCriteriaVisitorListString = interface(IdxCriteriaVisitor<TdxListString>)
  end;

  { TdxCriteriaToStringVisitResult }

  TdxCriteriaPriorityClass = (Atom, Neg, Mul, Add, BinaryNot, BinaryAnd, BinaryXor, BinaryOr, InBetween, CmpGt, CmpEq, IsNull, &Not, &And, &Or);

  TdxCriteriaToStringVisitResult = record
  public const
    NullCriteriaResult = '()';
  strict private
    class var
      FNull: TdxCriteriaToStringVisitResult;
    class constructor Create;
  strict private
    FResult: string;
    FPriority: TdxCriteriaPriorityClass;
    function GetIsNull: Boolean;
  public
    constructor Create(const AResult: string; APriorityClass: TdxCriteriaPriorityClass); overload;
    constructor Create(const AResult: string); overload;
    function GetEnclosedResult: string;
    function GetEnclosedResultOnGreaterOrEqual(ABasePriority: TdxCriteriaPriorityClass): string;
    function GetEnclosedResultOnGreater(ABasePriority: TdxCriteriaPriorityClass): string;

    property IsNull: Boolean read GetIsNull;
    property Priority: TdxCriteriaPriorityClass read FPriority;
    property Result: string read FResult;
    class property Null: TdxCriteriaToStringVisitResult read FNull;
  end;

  { IdxClientCriteriaToStringVisitor }

  IdxClientCriteriaToStringVisitor = IdxClientCriteriaVisitor<TdxCriteriaToStringVisitResult>;

  { IdxOperandValue }

  IdxOperandValue = interface(IdxCriteriaOperator)
  ['{3DD978CA-B51F-42DA-A5FD-281D0E672663}']
    function GetValue: TValue;
    procedure SetValue(const AValue: TValue);
    property Value: TValue read GetValue write SetValue;
  end;

  { TdxCriteriaOperator }

  TdxCriteriaOperator = class(TInterfacedObject, IdxExpression, IdxCriteriaOperator)
  strict private
    class function GetCustomFunctionCount: Integer; static;
  protected
    class function ToBasicStyleString(ACriteria: TdxCriteriaOperator): string; static;
    class function ObjectToCriteriaSafe(AObject: TObject): TdxCriteriaOperator; static;
    class property CustomFunctionCount: Integer read GetCustomFunctionCount;
  public

    procedure Accept(const AVisitor: IdxCriteriaVisitor); overload; virtual; abstract;
    function Accept(const AVisitor: IdxCriteriaOperatorVisitor): IdxCriteriaOperator; overload; virtual; abstract;
    function Accept(const AVisitor: IdxSQLGeneratorVisitor): string; overload; virtual; abstract;
    function Accept(const AVisitor: IdxClientCriteriaToStringVisitor): TdxCriteriaToStringVisitResult; overload; virtual; abstract;
    function Accept(const AVisitor: IdxCriteriaVisitorListString): TList<string>; overload; virtual; abstract;
    function Accept(const AVisitor: IdxClientCriteriaVisitor<TdxBooleanCriteriaState>): TdxBooleanCriteriaState; overload; virtual; abstract;
    class function Equals(AObjectA, AObjectB: TObject): Boolean; reintroduce; overload; static;
    class function Equals(const ACriteriaA, ACriteriaB: IdxCriteriaOperator): Boolean; reintroduce; overload; static;
    class function Parse(const AStringCriteria: string; out ACriteriaParametersList: TArray<IdxOperandValue>): IdxCriteriaOperator; overload; static;
    class function Parse(const ACriteria: string; const AParameters: array of TValue): IdxCriteriaOperator; overload; static;
    class function Parse(const ACriteria: string): IdxCriteriaOperator; overload; static;
    class function ParseList(const ACriteriaList: string; out ACriteriaParametersList: TArray<IdxOperandValue>): TArray<IdxCriteriaOperator>; overload; static;
    class function ParseList(const ACriteriaList: string; const AParameters: array of TValue): TArray<IdxCriteriaOperator>; overload; static;
    class function ParseList(const ACriteriaList: string): TArray<IdxCriteriaOperator>; overload; static;
    class function ToString(const ACriteria: IdxCriteriaOperator): string; reintroduce; overload; static;
    function ToString: string; overload; override;
    class function &And(const ALeftOperator, ARightOperator: IdxCriteriaOperator): IdxCriteriaOperator; overload; static;
    class function &And(const AOperands: TArray<IdxCriteriaOperator>): IdxCriteriaOperator; overload; static;
    class function &And(AOperands: TdxCriteriaOperatorList): IdxCriteriaOperator; overload; static;
    class function &Or(const ALeftOperator, ARightOperator: IdxCriteriaOperator): IdxCriteriaOperator; overload; static;
    class function &Or(AOperands: TdxCriteriaOperatorList): IdxCriteriaOperator; overload; static;
    function &Not: IdxUnaryOperator;
    class function CriterionEquals(ALeftOperator: TdxCriteriaOperator; ARightOperator: TdxCriteriaOperator): Boolean; static;
  end;

  { TdxCriteriaOperatorList }

  TdxCriteriaOperatorList = class(TList<IdxCriteriaOperator>)
  public
    function ToString: string; override;
  end;


  { IdxCriteriaOperatorCollection }

  IdxCriteriaOperatorCollection = interface(IdxList<IdxCriteriaOperator>)
  ['{4E158BB5-5A4C-4480-83AE-285129B061A7}']
  end;

  { TdxCriteriaOperatorCollection }

  TdxCriteriaOperatorCollection = class(TInterfacedObject, IdxCriteriaOperatorCollection)
  strict private type

    TEnumerator = class(TInterfacedObject, IEnumerator<IdxCriteriaOperator>)
    private
      FCriteriaOperatorCollection: TdxCriteriaOperatorCollection;
      FIndex: Integer;
    protected
      { IEnumerator }
      function GetCurrent: TObject;
      { IEnumerator<T> }
      function GetCurrentGeneric: IdxCriteriaOperator;
      function IEnumerator<IdxCriteriaOperator>.GetCurrent = GetCurrentGeneric;
    public
      constructor Create(ACriteriaOperatorCollection: TdxCriteriaOperatorCollection);
      function MoveNext: Boolean;
      procedure Reset;
      property Current: IdxCriteriaOperator read GetCurrentGeneric;
    end;

  strict private
    FList: TdxCriteriaOperatorList;
    procedure SetItems(AIndex: Integer; const Value: IdxCriteriaOperator);
  protected
    function GetEnumeratorGeneric: IEnumerator<IdxCriteriaOperator>;
    function GetEnumerator: IEnumerator;
    function IdxCriteriaOperatorCollection.GetEnumerator = GetEnumeratorGeneric;

    function GetCount: Integer;
    function GetItems(AIndex: Integer): IdxCriteriaOperator;
  public
    constructor Create; overload;
    constructor Create(ACapacity: Integer); overload;
    destructor Destroy; override;

    procedure AddRange(const AOperands: TArray<IdxCriteriaOperator>); overload;
    procedure AddRange(const AOperands: array of IdxCriteriaOperator); overload;
    procedure AddRange(AOperands: TEnumerable<IdxCriteriaOperator>); overload;
    procedure AddRange(const AOperands: IEnumerable<IdxCriteriaOperator>); overload;

    function Equals(AObject: TObject): Boolean; override;
    function GetHashCode: Integer; override;
    function ToArray: TArray<IdxCriteriaOperator>;
    function ToString: string; override;

    function Add(const AValue: IdxCriteriaOperator): Integer;
    procedure Remove(const AValue: IdxCriteriaOperator);
    function Contains(const AValue: IdxCriteriaOperator): Boolean;
    procedure Delete(AIndex: Integer);
    function IndexOf(const AItem: IdxCriteriaOperator): Integer;

    procedure DeleteRange(AIndex, ACount: Integer);
    property Items[AIndex: Integer]: IdxCriteriaOperator read GetItems write SetItems; default;
    property Count: Integer read GetCount;
  end;

 { TdxOperandValue }

  TdxOperandValue = class(TdxCriteriaOperator, IdxOperandValue)
  strict private
    FValue: TValue;
  protected
    function GetValue: TValue;
    procedure SetValue(const AValue: TValue);
  public
    constructor Create(const AValue: TValue); overload;
    constructor Create(const AValue: TDateTime); overload;
    constructor Create(const AValue: string); overload;
    constructor Create(const AValue: Char); overload;
    class function Create<T>(const AValue: T): TdxOperandValue; overload;

    function Accept(const AVisitor: IdxCriteriaOperatorVisitor): IdxCriteriaOperator; override;
    function Accept(const AVisitor: IdxSQLGeneratorVisitor): string; override;
    function Accept(const AVisitor: IdxClientCriteriaToStringVisitor): TdxCriteriaToStringVisitResult; override;
    function Accept(const AVisitor: IdxCriteriaVisitorListString): TList<string>; override;
    function Accept(const AVisitor: IdxClientCriteriaVisitor<TdxBooleanCriteriaState>): TdxBooleanCriteriaState; override;

    function Equals(AObject: TObject): Boolean; override;
    function GetHashCode: Integer; override;
    property Value: TValue read FValue write FValue;
  end;

  { IdxOperandParameter }

  IdxOperandParameter = interface(IdxOperandValue)
  ['{E9480C6A-5C18-4A9B-BD43-CF5D1741C127}']
    function GetParameterName: string;
    property ParameterName: string read GetParameterName;
  end;

  { TdxOperandParameter }

  TdxOperandParameter = class(TdxOperandValue, IdxOperandParameter)
  strict private
    FParameterName: string;
    function GetParameterName: string;
  public
    constructor Create(const AParameterName: string; const AValue: TValue); overload;
    constructor Create(const AParameterName: string); overload;
    function Equals(AObject: TObject): Boolean; override;
    function GetHashCode: Integer; override;

    property ParameterName: string read FParameterName write FParameterName;
  end;

  { IdxOperandProperty }

  IdxOperandProperty = interface(IdxCriteriaOperator)
  ['{B3356E7C-291C-4DDA-871C-735FBEA09BCF}']
    function GetPropertyName: string;
    property PropertyName: string read GetPropertyName;
  end;

  { TdxOperandProperty }

  TdxOperandProperty = class(TdxCriteriaOperator, IdxOperandProperty)
  strict private
    FPropertyName: string;
  protected
    function GetPropertyName: string;
  public
    constructor Create(const APropertyName: string); overload;

    function Accept(const AVisitor: IdxCriteriaOperatorVisitor): IdxCriteriaOperator; override;
    function Accept(const AVisitor: IdxClientCriteriaToStringVisitor): TdxCriteriaToStringVisitResult; override;
    function Accept(const AVisitor: IdxClientCriteriaVisitor<TdxBooleanCriteriaState>): TdxBooleanCriteriaState; override;
    function Equals(AObject: TObject): Boolean; override;
    function GetHashCode: Integer; override;

    property PropertyName: string read FPropertyName write FPropertyName;
  end;

  { IdxConstantValue }

  IdxConstantValue = interface(IdxOperandValue)
  ['{538E03AD-F1AB-4876-BFD0-B3EA51BB7E5C}']
  end;

  { TdxConstantValue }

  TdxConstantValue = class(TdxOperandValue, IdxConstantValue);

  { IdxBinaryOperator }

  IdxBinaryOperator = interface(IdxCriteriaOperator)
  ['{915D8116-8A76-43CB-9FF9-C6EDCFD26320}']
    function GetOperatorType: TdxBinaryOperatorType;
    function GetLeftOperand: IdxCriteriaOperator;
    function GetRightOperand: IdxCriteriaOperator;
    property OperatorType: TdxBinaryOperatorType read GetOperatorType;
    property LeftOperand: IdxCriteriaOperator read GetLeftOperand;
    property RightOperand: IdxCriteriaOperator read GetRightOperand;
  end;

  { TdxBinaryOperator }

  TdxBinaryOperator = class(TdxCriteriaOperator, IdxBinaryOperator)
  strict private
    FOperatorType: TdxBinaryOperatorType;
    FLeftOperand: IdxCriteriaOperator;
    FRightOperand: IdxCriteriaOperator;
    function GetOperatorType: TdxBinaryOperatorType;
    function GetLeftOperand: IdxCriteriaOperator;
    function GetRightOperand: IdxCriteriaOperator;
  public
    constructor Create(const ALeftOperator, ARightOperator: IdxCriteriaOperator; AType: TdxBinaryOperatorType); overload;
    constructor Create(const APropertyName: string; const AValue: TValue; AType: TdxBinaryOperatorType = TdxBinaryOperatorType.Equal); overload;
    constructor Create(const APropertyName: string; const AValue: string; AType: TdxBinaryOperatorType = TdxBinaryOperatorType.Equal); overload;
    constructor Create(const APropertyName: string; AValue: Double; AType: TdxBinaryOperatorType = TdxBinaryOperatorType.Equal); overload;
    constructor Create(const APropertyName: string; AValue: Integer; AType: TdxBinaryOperatorType = TdxBinaryOperatorType.Equal); overload;

    function Accept(const AVisitor: IdxCriteriaOperatorVisitor): IdxCriteriaOperator; override;
    function Accept(const AVisitor: IdxSQLGeneratorVisitor): string; override;
    function Accept(const AVisitor: IdxClientCriteriaToStringVisitor): TdxCriteriaToStringVisitResult; override;
    function Accept(const AVisitor: IdxCriteriaVisitorListString): TList<string>; override;
    function Accept(const AVisitor: IdxClientCriteriaVisitor<TdxBooleanCriteriaState>): TdxBooleanCriteriaState; override;
    function Equals(AObject: TObject): Boolean; override;
    function GetHashCode: Integer; override;
    property OperatorType: TdxBinaryOperatorType read FOperatorType;
    property LeftOperand: IdxCriteriaOperator read FLeftOperand write FLeftOperand;
    property RightOperand: IdxCriteriaOperator read FRightOperand write FRightOperand;
  end;

  { IdxUnaryOperator }

  IdxUnaryOperator = interface(IdxCriteriaOperator)
  ['{D0EA88D4-32B7-4E57-934C-BDA828AF8D53}']
    function GetOperand: IdxCriteriaOperator;
    function GetOperatorType: TdxUnaryOperatorType;
    property Operand: IdxCriteriaOperator read GetOperand;
    property OperatorType: TdxUnaryOperatorType read GetOperatorType;
  end;

  { TdxUnaryOperator }

  TdxUnaryOperator = class(TdxCriteriaOperator, IdxUnaryOperator)
  strict private
    FOperand: IdxCriteriaOperator;
    FOperatorType: TdxUnaryOperatorType;
    function GetOperatorType: TdxUnaryOperatorType;
    function GetOperand: IdxCriteriaOperator;
  public
    constructor Create; overload;
    constructor Create(AOperatorType: TdxUnaryOperatorType; const AOperand: IdxCriteriaOperator); overload;

    function Accept(const AVisitor: IdxCriteriaOperatorVisitor): IdxCriteriaOperator; override;
    function Accept(const AVisitor: IdxSQLGeneratorVisitor): string; override;
    function Accept(const AVisitor: IdxClientCriteriaToStringVisitor): TdxCriteriaToStringVisitResult; override;
    function Accept(const AVisitor: IdxCriteriaVisitorListString): TList<string>; override;
    function Accept(const AVisitor: IdxClientCriteriaVisitor<TdxBooleanCriteriaState>): TdxBooleanCriteriaState; override;
    function Equals(AObject: TObject): Boolean; override;
    function GetHashCode: Integer; override;
    property Operand: IdxCriteriaOperator read FOperand write FOperand;
    property OperatorType: TdxUnaryOperatorType read FOperatorType;
  end;

  { IdxBetweenOperator }

  IdxBetweenOperator = interface(IdxCriteriaOperator)
  ['{D70252BD-F7B9-45E5-9D83-D95C9DDFD01C}']
    function GetBeginExpression: IdxCriteriaOperator;
    function GetEndExpression: IdxCriteriaOperator;
    function GetTestExpression: IdxCriteriaOperator;
    property BeginExpression: IdxCriteriaOperator read GetBeginExpression;
    property EndExpression: IdxCriteriaOperator read GetEndExpression;
    property TestExpression: IdxCriteriaOperator read GetTestExpression;
  end;

  { TdxBetweenOperator }

  TdxBetweenOperator = class(TdxCriteriaOperator, IdxBetweenOperator)
  strict private
    FTestExpression: IdxCriteriaOperator;
    FBeginExpression: IdxCriteriaOperator;
    FEndExpression: IdxCriteriaOperator;
    function GetBeginExpression: IdxCriteriaOperator;
    function GetEndExpression: IdxCriteriaOperator;
    function GetTestExpression: IdxCriteriaOperator;
  protected
    constructor Create; overload;
  public
    constructor Create(const ATestPropertyName: string; const ABeginValue, AEndValue: TValue); overload;
    constructor Create(const ATestExpression, ABeginExpression, AEndExpression: IdxCriteriaOperator); overload;
    constructor Create(const ATestPropertyName: string; const ABeginExpression, AEndExpression: IdxCriteriaOperator); overload;

    function Accept(const AVisitor: IdxCriteriaOperatorVisitor): IdxCriteriaOperator; override;
    function Accept(const AVisitor: IdxSQLGeneratorVisitor): string; override;
    function Accept(const AVisitor: IdxClientCriteriaToStringVisitor): TdxCriteriaToStringVisitResult; override;
    function Accept(const AVisitor: IdxCriteriaVisitorListString): TList<string>; override;
    function Accept(const AVisitor: IdxClientCriteriaVisitor<TdxBooleanCriteriaState>): TdxBooleanCriteriaState; override;
    function Equals(AObject: TObject): Boolean; override;
    function GetHashCode: Integer; override;

    property BeginExpression: IdxCriteriaOperator read FBeginExpression write FBeginExpression;
    property EndExpression: IdxCriteriaOperator read FEndExpression write FEndExpression;
    property TestExpression: IdxCriteriaOperator read FTestExpression write FTestExpression;
  end;

  { IdxInOperator }

  IdxInOperator = interface(IdxCriteriaOperator)
  ['{4455F6F0-E3CB-48B0-9F91-3A83E6C8D508}']
    function GetLeftOperand: IdxCriteriaOperator;
    function GetOperands: IdxCriteriaOperatorCollection;
    property LeftOperand: IdxCriteriaOperator read GetLeftOperand;
    property Operands: IdxCriteriaOperatorCollection read GetOperands;
  end;

  { TdxInOperator }

  TdxInOperator = class(TdxCriteriaOperator, IdxInOperator)
  strict private
    FLeftOperand: IdxCriteriaOperator;
    FOperands: IdxCriteriaOperatorCollection;
    function GetLeftOperand: IdxCriteriaOperator;
    function GetOperands: IdxCriteriaOperatorCollection;
  protected
    class function ObjectsToCriteriaSafe(const AOperands: TArray<TObject>): TArray<IdxCriteriaOperator>; static;
    class function VarRecToValue(const AVarRec: TVarRec): TValue; static;
  public
    constructor Create(const ALeftOperand: IdxCriteriaOperator); overload;
    constructor Create(const APropertyName: string; const AOperands: TArray<TObject>); overload;
    constructor Create(const APropertyName: string; const AOperands: array of const); overload;
    class function Create<T>(const APropertyName: string; const AOperands: array of T): TdxInOperator; overload;
    constructor Create(const ALeftOperand: IdxCriteriaOperator; const AOperands: TArray<IdxCriteriaOperator>); overload;
    constructor Create(const ALeftOperand: IdxCriteriaOperator; const AOperands: array of IdxCriteriaOperator); overload;
    constructor Create(const ALeftOperand: IdxCriteriaOperator; const AOperands: array of const); overload;
    constructor Create(const ALeftOperand: IdxCriteriaOperator; AOperands: TEnumerable<IdxCriteriaOperator>); overload;
    constructor Create(const ALeftOperand: IdxCriteriaOperator; const AOperands: IEnumerable<IdxCriteriaOperator>); overload;

    function Accept(const AVisitor: IdxCriteriaOperatorVisitor): IdxCriteriaOperator; override;
    function Accept(const AVisitor: IdxSQLGeneratorVisitor): string; override;
    function Accept(const AVisitor: IdxClientCriteriaToStringVisitor): TdxCriteriaToStringVisitResult; override;
    function Accept(const AVisitor: IdxCriteriaVisitorListString): TList<string>; override;
    function Accept(const AVisitor: IdxClientCriteriaVisitor<TdxBooleanCriteriaState>): TdxBooleanCriteriaState; override;
    function Equals(AObject: TObject): Boolean; override;
    function GetHashCode: Integer; override;

    property LeftOperand: IdxCriteriaOperator read FLeftOperand;
    property Operands: IdxCriteriaOperatorCollection read FOperands;
  end;

  { IdxAggregateOperand }

  IdxAggregateOperand = interface(IdxCriteriaOperator)
  ['{CF09F7A5-1609-45A5-91A6-E59A398FB97E}']
    function GetAggregatedExpression: IdxCriteriaOperator;
    function GetAggregateFunctionType: TdxAggregateFunctionType;
    function GetCondition: IdxCriteriaOperator;
    function GetProperty: IdxOperandProperty;
    function GetIsTopLevel: Boolean;
    property AggregatedExpression: IdxCriteriaOperator read GetAggregatedExpression;
    property AggregateFunctionType: TdxAggregateFunctionType read GetAggregateFunctionType;
    property Condition: IdxCriteriaOperator read GetCondition;
    property CollectionProperty: IdxOperandProperty read GetProperty;
    property IsTopLevel: Boolean read GetIsTopLevel;
  end;

  { TdxAggregateOperand }

  TdxAggregateOperand = class(TdxCriteriaOperator, IdxAggregateOperand)
  strict private
    FCondition: IdxCriteriaOperator;
    FProperty: IdxOperandProperty;
    FAggregatedExpression: IdxCriteriaOperator;
    FAggregateFunctionType: TdxAggregateFunctionType;
    class function GetPropertyByName(const APropertyName: string): IdxOperandProperty; static;
    function GetIsTopLevel: Boolean;
  private
    function GetAggregatedExpression: IdxCriteriaOperator;
    function GetAggregateFunctionType: TdxAggregateFunctionType;
    function GetCondition: IdxCriteriaOperator;
    function GetProperty: IdxOperandProperty;
  public
    constructor Create; overload;
    constructor Create(const ACollectionProperty: IdxOperandProperty; const AAggregatedExpression: IdxCriteriaOperator;
      AAggregateFunctionType: TdxAggregateFunctionType; const ACondition: IdxCriteriaOperator); overload;
    constructor Create(const ACollectionProperty: string; const AAggregatedExpression: string; AAggregateFunctionType: TdxAggregateFunctionType;
      const ACondition: IdxCriteriaOperator); overload;
    constructor Create(const ACollectionProperty: string; AAggregateFunctionType: TdxAggregateFunctionType; const ACondition: IdxCriteriaOperator); overload;
    constructor Create(const ACollectionProperty: string; const AAggregatedExpression: string; AAggregateFunctionType: TdxAggregateFunctionType); overload;
    constructor Create(const ACollectionProperty: string; AAggregateFunctionType: TdxAggregateFunctionType); overload;
    procedure Accept(const AVisitor: IdxCriteriaVisitor); override;
    function Accept(const AVisitor: IdxCriteriaOperatorVisitor): IdxCriteriaOperator; override;
    function Accept(const AVisitor: IdxClientCriteriaToStringVisitor): TdxCriteriaToStringVisitResult; override;
    function Accept(const AVisitor: IdxClientCriteriaVisitor<TdxBooleanCriteriaState>): TdxBooleanCriteriaState; override;
    function Equals(AObject: TObject): Boolean; overload; override;
    function GetHashCode: Integer; override;
    function Exists(const AAggregatedExpression: IdxCriteriaOperator = nil): IdxAggregateOperand;
    function Count(const AAggregatedExpression: IdxCriteriaOperator = nil): IdxAggregateOperand;
    function Avg(const AAggregatedExpression: IdxCriteriaOperator): IdxAggregateOperand;
    function Max(const AAggregatedExpression: IdxCriteriaOperator): IdxAggregateOperand;
    function Min(const AAggregatedExpression: IdxCriteriaOperator): IdxAggregateOperand;
    function Sum(const AAggregatedExpression: IdxCriteriaOperator): IdxAggregateOperand;
    function Single(const AAggregatedExpression: IdxCriteriaOperator): IdxAggregateOperand;

    property Condition: IdxCriteriaOperator read FCondition write FCondition;
    property CollectionProperty: IdxOperandProperty read FProperty write FProperty;
    property AggregatedExpression: IdxCriteriaOperator read FAggregatedExpression write FAggregatedExpression;
    property AggregateFunctionType: TdxAggregateFunctionType read FAggregateFunctionType write FAggregateFunctionType;
    property IsTopLevel: Boolean read GetIsTopLevel;
  end;

  { TdxContainsOperator }

  TdxContainsOperator = class(TdxAggregateOperand)
  public
    constructor Create; overload;
    constructor Create(const ACollectionProperty: IdxOperandProperty; const ACondition: IdxCriteriaOperator); overload;
    constructor Create(const ACollectionProperty: string; const ACondition: IdxCriteriaOperator); overload;
  end;

  { IdxJoinOperand }

  IdxJoinOperand = interface(IdxCriteriaOperator)
  ['{2164F8F0-9025-4165-ACF9-9C3E009E336D}']
    function GetAggregatedExpression: IdxCriteriaOperator;
    function GetAggregateFunctionType: TdxAggregateFunctionType;
    function GetCondition: IdxCriteriaOperator;
    function GetJoinTypeName: string;
    property AggregatedExpression: IdxCriteriaOperator read GetAggregatedExpression;
    property AggregateFunctionType: TdxAggregateFunctionType read GetAggregateFunctionType;
    property Condition: IdxCriteriaOperator read GetCondition;
    property JoinTypeName: string read GetJoinTypeName;
  end;

  { TdxJoinOperand }

  TdxJoinOperand = class(TdxCriteriaOperator, IdxJoinOperand)
  strict private
    FJoinTypeName: string;
    FCondition: IdxCriteriaOperator;
    FAggregatedExpression: IdxCriteriaOperator;
    FAggregateFunctionType: TdxAggregateFunctionType;
    function GetAggregateFunctionType: TdxAggregateFunctionType;
    function GetAggregatedExpression: IdxCriteriaOperator;
    function GetCondition: IdxCriteriaOperator;
    function GetJoinTypeName: string;
  public
    constructor Create(const AJoinTypeName: string; const ACondition: IdxCriteriaOperator; AType: TdxAggregateFunctionType;
      const AAggregatedExpression: IdxCriteriaOperator); overload;
    constructor Create(const AJoinTypeName: string; const ACondition: IdxCriteriaOperator); overload;
    function Accept(const AVisitor: IdxCriteriaOperatorVisitor): IdxCriteriaOperator; override;
    function Accept(const AVisitor: IdxSQLGeneratorVisitor): string; override;
    function Accept(const AVisitor: IdxClientCriteriaToStringVisitor): TdxCriteriaToStringVisitResult; override;
    function Accept(const AVisitor: IdxClientCriteriaVisitor<TdxBooleanCriteriaState>): TdxBooleanCriteriaState; override;
    function Equals(AObject: TObject): Boolean; override;
    function GetHashCode: Integer; override;
    function Count(const AAggregatedExpression: IdxCriteriaOperator = nil): IdxJoinOperand;
    function Avg(const AAggregatedExpression: IdxCriteriaOperator): IdxJoinOperand;
    function Max(const AAggregatedExpression: IdxCriteriaOperator): IdxJoinOperand;
    function Min(const AAggregatedExpression: IdxCriteriaOperator): IdxJoinOperand;
    function Sum(const AAggregatedExpression: IdxCriteriaOperator): IdxJoinOperand;
    property AggregatedExpression: IdxCriteriaOperator read FAggregatedExpression write FAggregatedExpression;
    property AggregateFunctionType: TdxAggregateFunctionType read FAggregateFunctionType write FAggregateFunctionType;
    property Condition: IdxCriteriaOperator read FCondition write FCondition;
    property JoinTypeName: string read FJoinTypeName write FJoinTypeName;
  end;

  { IdxGroupOperator }

  IdxGroupOperator = interface(IdxCriteriaOperator)
  ['{98B7F5A1-C8A8-424A-84C3-204EB6F8E603}']
    function GetOperands: IdxCriteriaOperatorCollection;
    function GetOperatorType: TdxGroupOperatorType;
    property Operands: IdxCriteriaOperatorCollection read GetOperands;
    property OperatorType: TdxGroupOperatorType read GetOperatorType;
  end;

  { TdxGroupOperator }

  TdxGroupOperator = class(TdxCriteriaOperator, IdxGroupOperator)
  strict private
    FOperands: IdxCriteriaOperatorCollection;
    FOperatorType: TdxGroupOperatorType;
    function GetOperands: IdxCriteriaOperatorCollection;
    function GetOperatorType: TdxGroupOperatorType;
  protected
    class function CombineCore(AOperatorType: TdxGroupOperatorType;
      const AOperands: array of IdxCriteriaOperator): TArray<IdxCriteriaOperator>; overload; static;
  public
    constructor Create(AType: TdxGroupOperatorType = TdxGroupOperatorType.&And); overload;
    constructor Create(AType: TdxGroupOperatorType; const AOperands: TArray<IdxCriteriaOperator>); overload;
    constructor Create(AType: TdxGroupOperatorType; const AOperands: array of IdxCriteriaOperator); overload;
    constructor Create(AType: TdxGroupOperatorType; const AOperands: TEnumerable<IdxCriteriaOperator>); overload;
    constructor Create(AType: TdxGroupOperatorType; const AOperands: IEnumerable<IdxCriteriaOperator>); overload;
    constructor Create(const AOperands: TArray<IdxCriteriaOperator>); overload;
    constructor Create(const AOperands: array of IdxCriteriaOperator); overload;

    function Accept(const AVisitor: IdxCriteriaOperatorVisitor): IdxCriteriaOperator; override;
    function Accept(const AVisitor: IdxSQLGeneratorVisitor): string; override;
    function Accept(const AVisitor: IdxClientCriteriaToStringVisitor): TdxCriteriaToStringVisitResult; override;
    function Accept(const AVisitor: IdxClientCriteriaVisitor<TdxBooleanCriteriaState>): TdxBooleanCriteriaState; override;
    function Equals(AObject: TObject): Boolean; override;
    function GetHashCode: Integer; override;
    class function Combine(AOperatorType: TdxGroupOperatorType; const ALeftOperator, ARightOperator: IdxCriteriaOperator): IdxCriteriaOperator; overload; static;
    class function Combine(AOperatorType: TdxGroupOperatorType; const AOperands: array of IdxCriteriaOperator): IdxCriteriaOperator; overload; static;
    class function Combine(AOperatorType: TdxGroupOperatorType; AOperands: TdxCriteriaOperatorList): IdxCriteriaOperator; overload; static;

    property Operands: IdxCriteriaOperatorCollection read FOperands;
    property OperatorType: TdxGroupOperatorType read FOperatorType write FOperatorType;
  end;

  { IdxFunctionOperator }

  IdxFunctionOperator = interface(IdxCriteriaOperator)
  ['{B260CC5B-A2EE-4D30-BC0C-ADD77F50E82B}']
    function GetOperands: IdxCriteriaOperatorCollection;
    function GetOperatorType: TdxFunctionOperatorType;
    property Operands: IdxCriteriaOperatorCollection read GetOperands;
    property OperatorType: TdxFunctionOperatorType read GetOperatorType;
  end;

  { TdxFunctionOperator }

  TdxFunctionOperator = class(TdxCriteriaOperator, IdxFunctionOperator)
  strict private
    FOperands: IdxCriteriaOperatorCollection;
    FOperatorType: TdxFunctionOperatorType;
    function GetOperands: IdxCriteriaOperatorCollection;
    function GetOperatorType: TdxFunctionOperatorType;
  public
    constructor Create; overload;
    constructor Create(AType: TdxFunctionOperatorType; const AOperands: TArray<IdxCriteriaOperator>); overload;
    constructor Create(AType: TdxFunctionOperatorType; const AOperands: array of IdxCriteriaOperator); overload;
    constructor Create(AType: TdxFunctionOperatorType; AOperands: TEnumerable<IdxCriteriaOperator>); overload;
    constructor Create(AType: TdxFunctionOperatorType; const AOperands: IEnumerable<IdxCriteriaOperator>); overload;
    function Accept(const AVisitor: IdxSQLGeneratorVisitor): string; override;
    function Accept(const AVisitor: IdxCriteriaOperatorVisitor): IdxCriteriaOperator; override;
    function Accept(const AVisitor: IdxClientCriteriaToStringVisitor): TdxCriteriaToStringVisitResult; override;
    function Accept(const AVisitor: IdxCriteriaVisitorListString): TList<string>; override;
    function Accept(const AVisitor: IdxClientCriteriaVisitor<TdxBooleanCriteriaState>): TdxBooleanCriteriaState; override;
    function Equals(AObject: TObject): Boolean; override;
    function GetHashCode: Integer; override;

    class function GuessIsLogicalCustomFunction(const AOperator: IdxFunctionOperator): Boolean; static;

    property Operands: IdxCriteriaOperatorCollection read FOperands;
    property OperatorType: TdxFunctionOperatorType read FOperatorType write FOperatorType;
  end;

  { TdxSortByExpression }

  TdxSortByExpression = class(TInterfacedObject, IdxSortByExpression)
  strict private
    FProperty: IdxCriteriaOperator;
    FSortDirection: TdxSortDirection;
    function GetPropertyName: string;
    procedure SetPropertyName(const AValue: string);
  protected
    // IdxSortByExpression
    function GetExpression: IdxExpression;
    function GetSortDirection: TdxSortDirection;
  public
    constructor Create(const AProperty: IdxCriteriaOperator; ASortDirection: TdxSortDirection = TdxSortDirection.Ascending); overload;
    constructor Create(const APropertyName: string; ASortDirection: TdxSortDirection = TdxSortDirection.Ascending); overload;
    constructor Create; overload;

    property PropertyName: string read GetPropertyName write SetPropertyName;
    property &Property: IdxCriteriaOperator read FProperty write FProperty;
    property SortDirection: TdxSortDirection read FSortDirection write FSortDirection;
  end;

  { TdxSortByExpressions }

  TdxSortByExpressions = class(TInterfacedObject, IdxSortByExpressions)
  strict private type

    TEnumerator = class(TInterfacedObject, IEnumerator<IdxSortByExpression>)
    private
      FSortByExpressions: TdxSortByExpressions;
      FIndex: Integer;
    protected
      { IEnumerator }
      function GetCurrent: TObject;
      { IEnumerator<T> }
      function GetCurrentGeneric: IdxSortByExpression;
      function IEnumerator<IdxSortByExpression>.GetCurrent = GetCurrentGeneric;
    public
      constructor Create(ASortByExpressions: TdxSortByExpressions);
      function MoveNext: Boolean;
      procedure Reset;
      property Current: IdxSortByExpression read GetCurrentGeneric;
    end;

  strict private
    FList: TList<IdxSortByExpression>;
    FOnChanged: TNotifyEvent;
    function GetCount: Integer;
    function GetItems(AIndex: Integer): IdxSortByExpression;
  protected
    procedure DoChanged;
    // IdxSortByExpressions
    function GetEnumerator: IEnumerator;
    function GetEnumeratorGeneric: IEnumerator<IdxSortByExpression>;
    function IdxSortByExpressions.GetEnumerator = GetEnumeratorGeneric;
  public
    constructor Create; overload;
    constructor Create(const ASortProperties: TArray<IdxSortByExpression>); overload;
    constructor Create(const ASortProperties: array of IdxSortByExpression); overload;
    destructor Destroy; override;
    procedure AddRange(const ASortProperties: TArray<IdxSortByExpression>);
    procedure Add(const ASortProperties: IdxSortByExpressions); overload;
    function Add(const AValue: IdxSortByExpression): Integer; overload;
    procedure Clear;
    function Contains(const AValue: IdxSortByExpression): Boolean;
    function IndexOf(const AValue: IdxSortByExpression): Integer;
    procedure Insert(AIndex: Integer; const AValue: IdxSortByExpression);
    procedure Remove(const AValue: IdxSortByExpression);
    procedure RemoveAt(AIndex: Integer);

    property Count: Integer read GetCount;
    property Items[AIndex: Integer]: IdxSortByExpression read GetItems; default;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  end;

  { IdxQueryOperand }

  IdxQueryOperand = interface(IdxCriteriaOperator)
  ['{AB07E8CB-CB8B-43BE-BAC6-1D571B1D9AD7}']
    function GetColumnName: string;
    function GetColumnType: TdxDBColumnType;
    function GetNodeAlias: string;
    property ColumnName: string read GetColumnName;
    property ColumnType: TdxDBColumnType read GetColumnType;
    property NodeAlias: string read GetNodeAlias;
  end;

  { IdxParentCriteria }

  IdxParentCriteria = interface
  ['{73E4BF31-A99E-4092-A67D-7598C5775874}']
    function GetParentCriteria: IdxCriteriaOperator;
    property ParentCriteria: IdxCriteriaOperator read GetParentCriteria;
  end;

  { TdxQueryOperand }

  TdxQueryOperand = class(TdxCriteriaOperator, IdxQueryOperand, IdxParentCriteria)
  strict private
    FColumnName: string;
    FColumnType: TdxDBColumnType;
    FNodeAlias: string;
    FParentCriteria: IdxCriteriaOperator;
    function GetColumnName: string;
    function GetColumnType: TdxDBColumnType;
    function GetNodeAlias: string;
    function GetParentCriteria: IdxCriteriaOperator;
  protected
    property ParentCriteria: IdxCriteriaOperator read GetParentCriteria;
  public
    constructor Create; overload;
    constructor Create(const AColumnName: string; const ANodeAlias: string); overload;
    constructor Create(const AColumnName: string; const ANodeAlias: string; AColumnType: TdxDBColumnType;
      const AParentCriteria: IdxCriteriaOperator = nil); overload;
    function Accept(const AVisitor: IdxCriteriaOperatorVisitor): IdxCriteriaOperator; override;
    function Accept(const AVisitor: IdxSQLGeneratorVisitor): string; override;
    function Accept(const AVisitor: IdxClientCriteriaToStringVisitor): TdxCriteriaToStringVisitResult; override;
    function Accept(const AVisitor: IdxCriteriaVisitorListString): TList<string>; override;
    function Equals(AObject: TObject): Boolean; override;
    function GetHashCode: Integer; override;
    function Clone: TdxQueryOperand;

    property ColumnName: string read FColumnName;
    property ColumnType: TdxDBColumnType read FColumnType;
    property NodeAlias: string read FNodeAlias write FNodeAlias;
  end;

  { TdxObjectGeneratorCriteriaSet }

  TdxObjectGeneratorCriteriaSet = class
  strict private
    FCriteriaDictionary: TDictionary<string, IdxCriteriaOperator>;
    FCommonCriteria: IdxCriteriaOperator;
  public
    constructor Create;
    destructor Destroy; override;
    function TryGetCriteria(const ATableName: string; out ACriteria: IdxCriteriaOperator): Boolean;
    function GetCompleteCriteria(const ATableName: string): IdxCriteriaOperator;
    procedure UpdateCriteria(const ATableName: string; const ACriteria: IdxCriteriaOperator);
    procedure UpdateCommonCriteria(const ACriteria: IdxCriteriaOperator);
    class function GetCommonCriteriaSet(const ACommonCriteria: IdxCriteriaOperator): TdxObjectGeneratorCriteriaSet; static;
    class function GetCriteriaSet(const ATableName: string; const ACriteria: IdxCriteriaOperator): TdxObjectGeneratorCriteriaSet; overload; static;
    class function GetCriteriaSet(const ATableName: string; const ACriteria,
      ACommonCriteria: IdxCriteriaOperator): TdxObjectGeneratorCriteriaSet; overload; static;

    property CommonCriteria: IdxCriteriaOperator read FCommonCriteria;
  end;

  { TdxAcceptor }

  TdxAcceptor = class sealed
  public type
    TCriteria = (
      TBetweenOperator,
      TBinaryOperator,
      TUnaryOperator,
      TInOperator,
      TGroupOperator,
      TOperandValue,
      TFunctionOperator,

      TOperandProperty,
      TAggregateOperand,
      TJoinOperand
    );
  strict private
    class var
      FCriteria: TDictionary<TClass, TCriteria>;
    class constructor Create;
    class destructor Destroy;
  public
    class function Accept<T>(const ACriteriaOperator: IdxCriteriaOperator; const AVisitor: IdxClientCriteriaVisitor<T>): T;
    class function GetCriteriaType(const ACriteriaOperator: IdxCriteriaOperator): TCriteria;
  end;

implementation

uses
  Math, TypInfo, TimeSpan, StrUtils,
  dxCore, dxCoreClasses, dxStringHelper,
  dxEMF.Utils.CriteriaParser,
  dxEMF.DB.Query;

type

  { TdxCriteriaToStringBase }

  TdxCriteriaToStringBase = class abstract(TcxIUnknownObject,
    IdxCriteriaVisitor<TdxCriteriaToStringVisitResult>,
    IdxClientCriteriaVisitor<TdxCriteriaToStringVisitResult>,
    IQueryCriteriaToStringCriteriaVisitor)
  protected
    function ProcessToCommaDelimitedList(const AOperands: IEnumerable<IdxCriteriaOperator>): string;
    function GetBetweenText: string; inline;
    function GetIsNotNullText: string; inline;
    function CreateIsNotNull(ANullOp: TdxUnaryOperator): TdxCriteriaToStringVisitResult;
    function GetNotLikeText: string; inline;
    function CreateNotLike(const ALikeOp: IdxBinaryOperator): TdxCriteriaToStringVisitResult;
    function GetIsNullText: string; inline;
    function GetInText: string; inline;
    function GetFunctionText(AOperandType: TdxFunctionOperatorType): string; virtual;
    function GetCustomFunctionText(const P: string): string; virtual;
    function GetOperatorString(AOperandType: TdxAggregateFunctionType): string; overload;
    function Process(const AOperand: IdxCriteriaOperator): TdxCriteriaToStringVisitResult;
  public
    function GetOperatorString(AOperatorType: TdxBinaryOperatorType): string; overload; virtual; abstract;
    function GetOperatorString(AOperatorType: TdxGroupOperatorType): string; overload; virtual; abstract;
    function GetOperatorString(AOperatorType: TdxUnaryOperatorType): string; overload; virtual; abstract;
    function Visit(const AOperand: IdxBetweenOperator): TdxCriteriaToStringVisitResult; overload;
    function Visit(const AOperand: IdxBinaryOperator): TdxCriteriaToStringVisitResult; overload;
    function Visit(const AOperand: IdxUnaryOperator): TdxCriteriaToStringVisitResult; overload;
    function Visit(const AOperand: IdxInOperator): TdxCriteriaToStringVisitResult; overload;
    function Visit(const AOperand: IdxGroupOperator): TdxCriteriaToStringVisitResult; overload;
    function Visit(const AOperand: IdxOperandValue): TdxCriteriaToStringVisitResult; overload; virtual; abstract;
    function Visit(const AOperand: IdxFunctionOperator): TdxCriteriaToStringVisitResult; overload;
    function Visit(const AOperand: IdxAggregateOperand): TdxCriteriaToStringVisitResult; overload;
    function Visit(const AOperand: IdxJoinOperand): TdxCriteriaToStringVisitResult; overload;
    function Visit(const AOperand: IdxOperandProperty): TdxCriteriaToStringVisitResult; overload;

    function Visit(const AOperand: IdxQueryOperand): TdxCriteriaToStringVisitResult; overload;
    function Visit(const AOperand: IdxQuerySubQueryContainer): TdxCriteriaToStringVisitResult; overload;
  end;

  { TdxCriteriaToStringWithParametersProcessor }

  TdxCriteriaToStringWithParametersProcessor = class abstract(TdxCriteriaToStringBase)
  public const
    NamedParameterPrefix = ':';
    ParameterPrefix = '?';
  strict private
    FParameters: TList<IdxOperandValue>;
  public
    constructor Create;
    destructor Destroy; override;
    function Visit(const AOperand: IdxOperandValue): TdxCriteriaToStringVisitResult; override;
  end;

  { TdxCriteriaToStringParameterlessProcessor }

  TdxCriteriaToStringParameterlessProcessor = class abstract(TdxCriteriaToStringBase)
  public const
    NullString = 'null';
  public
    class function ValueToString(AValue: TValue): string; overload; static;
    class function ValueToString(const AValue: TValue; AIsLegacy: Boolean): string; overload; static;
    class function UserTypeToString(const AValue: TValue; AIsLegacy: Boolean): string; static;
    class function OperandValueOrParameterToString(const AVal: IdxOperandValue; AIsLegacy: Boolean): string; static;
    class function FixNonFixedText(const AToFix: string; AIsLegacy: Boolean; const AValue: TValue): string; static;
    class function GetSuffix(const AValue: TValue): string; static;
    class function ValueToCriteriaToStringVisitResult(const AOperand: IdxOperandValue): TdxCriteriaToStringVisitResult; static;
    function Visit(const AOperand: IdxOperandValue): TdxCriteriaToStringVisitResult; override;
  end;

  { TdxCriteriaToBasicStyleParameterlessProcessor }

  TdxCriteriaToBasicStyleParameterlessProcessor = class(TdxCriteriaToStringParameterlessProcessor)
  strict private
    class var
      FInstance: TdxCriteriaToBasicStyleParameterlessProcessor;
    class constructor Initialize;
    class destructor Finalize;
  public
    class function GetBasicOperatorString(AOperatorType: TdxBinaryOperatorType): string; overload; static;
    class function GetBasicOperatorString(AOperatorType: TdxUnaryOperatorType): string; overload; static;
    class function GetBasicOperatorString(AOperatorType: TdxGroupOperatorType): string; overload; static;
    function GetOperatorString(AOperatorType: TdxUnaryOperatorType): string; overload; override;
    function GetOperatorString(AOperatorType: TdxBinaryOperatorType): string; overload; override;
    function GetOperatorString(AOperatorType: TdxGroupOperatorType): string; overload; override;
    class function ToString(const AOperand: IdxCriteriaOperator): string; reintroduce; overload; static;
  end;

{ TdxLikeCustomFunction }

  TdxLikeCustomFunction = class
  public const
    Name = 'Like';
  public
    class function Create(const AValue, APattern: IdxCriteriaOperator): TdxFunctionOperator; static;
    class function IsName(const AName: string): Boolean; static;
    class function Convert(const ALike: IdxFunctionOperator): IdxBinaryOperator; overload; static;
    class function Convert(const ALike: IdxBinaryOperator): IdxFunctionOperator; overload; static;
    class function IsBinaryCompatibleLikeFunction(const AFunc: IdxFunctionOperator): Boolean; static;
  end;

{ TdxCriteriaToStringVisitResult }

constructor TdxCriteriaToStringVisitResult.Create(const AResult: string);
begin
  Create(AResult, TdxCriteriaPriorityClass.Atom);
end;

constructor TdxCriteriaToStringVisitResult.Create(const AResult: string; APriorityClass: TdxCriteriaPriorityClass);
begin
  FResult := AResult;
  FPriority := APriorityClass;
end;

class constructor TdxCriteriaToStringVisitResult.Create;
begin
  FNull := TdxCriteriaToStringVisitResult.Create(NullCriteriaResult, TdxCriteriaPriorityClass.Atom);
end;

function TdxCriteriaToStringVisitResult.GetIsNull: Boolean;
begin
  Result := FResult = NullCriteriaResult;
end;

function TdxCriteriaToStringVisitResult.GetEnclosedResult: string;
begin
  Result := '(' + FResult + ')';
end;

function TdxCriteriaToStringVisitResult.GetEnclosedResultOnGreaterOrEqual(ABasePriority: TdxCriteriaPriorityClass): string;
begin
  if Priority >= ABasePriority then
    Result := GetEnclosedResult
  else
    Result := FResult;
end;

function TdxCriteriaToStringVisitResult.GetEnclosedResultOnGreater(ABasePriority: TdxCriteriaPriorityClass): string;
begin
  if Priority > ABasePriority then
    Result := GetEnclosedResult
  else
    Result := FResult;
end;

{ TdxCriteriaToStringBase }

function TdxCriteriaToStringBase.GetBetweenText: string;
begin
  Result := 'Between';
end;

function TdxCriteriaToStringBase.GetIsNotNullText: string;
begin
  Result := 'Is Not Null';
end;

function TdxCriteriaToStringBase.GetIsNullText: string;
begin
  Result := 'Is Null';
end;

function TdxCriteriaToStringBase.GetInText: string;
begin
  Result := 'In';
end;

function TdxCriteriaToStringBase.ProcessToCommaDelimitedList(const AOperands: IEnumerable<IdxCriteriaOperator>): string;
var
  AOperandsList: TStringBuilder;
  AOperator: IdxCriteriaOperator;
begin
  if AOperands = nil then
    Exit('');
  AOperandsList := TStringBuilder.Create;
  try
    for AOperator in AOperands do
    begin
      if AOperandsList.Length > 0 then
        AOperandsList.Append(', ');
      AOperandsList.Append(Process(AOperator).Result);
    end;
    Result := AOperandsList.ToString;
  finally
    AOperandsList.Free;
  end;
end;

function TdxCriteriaToStringBase.Visit(const AOperand: IdxBetweenOperator): TdxCriteriaToStringVisitResult;
var
  AVisitResult: TdxCriteriaToStringVisitResult;
  AResult: TStringBuilder;
  AOperands: IdxCriteriaOperatorCollection;
begin
  AVisitResult := Process(AOperand.TestExpression as TdxCriteriaOperator);
  AResult := TStringBuilder.Create;
  try
    AResult.Append(AVisitResult.GetEnclosedResultOnGreaterOrEqual(TdxCriteriaPriorityClass.InBetween));
    AResult.Append(' ').Append(GetBetweenText).Append('(');
    AOperands := TdxCriteriaOperatorCollection.Create;
    TdxCriteriaOperatorCollection(AOperands).AddRange([AOperand.BeginExpression, AOperand.EndExpression]);
    AResult.Append(ProcessToCommaDelimitedList(AOperands));
    AResult.Append(')');
    Result := TdxCriteriaToStringVisitResult.Create(AResult.ToString, TdxCriteriaPriorityClass.InBetween);
  finally
    AResult.Free;
  end;
end;

function TdxCriteriaToStringBase.Visit(const AOperand: IdxBinaryOperator): TdxCriteriaToStringVisitResult;
var
  AOperatorString, AResult: string;
  APriority: TdxCriteriaPriorityClass;
  ALeftVisitResult, ARightVisitResult: TdxCriteriaToStringVisitResult;
begin
  AOperatorString := GetOperatorString(AOperand.OperatorType);
  case AOperand.OperatorType of
    TdxBinaryOperatorType.Divide,
    TdxBinaryOperatorType.Multiply,
    TdxBinaryOperatorType.Modulo:
      APriority := TdxCriteriaPriorityClass.Mul;
    TdxBinaryOperatorType.Plus,
    TdxBinaryOperatorType.Minus:
      APriority := TdxCriteriaPriorityClass.Add;
    TdxBinaryOperatorType.BitwiseAnd:
      APriority := TdxCriteriaPriorityClass.BinaryAnd;
    TdxBinaryOperatorType.BitwiseXor:
      APriority := TdxCriteriaPriorityClass.BinaryXor;
    TdxBinaryOperatorType.BitwiseOr:
      APriority := TdxCriteriaPriorityClass.BinaryOr;
    TdxBinaryOperatorType.Greater,
    TdxBinaryOperatorType.GreaterOrEqual,
    TdxBinaryOperatorType.Less,
    TdxBinaryOperatorType.LessOrEqual:
      APriority := TdxCriteriaPriorityClass.CmpGt;
    TdxBinaryOperatorType.Equal,
    TdxBinaryOperatorType.NotEqual:

      APriority := TdxCriteriaPriorityClass.CmpEq;
    else
      raise EInvalidOperation.Create('');
  end;
  ALeftVisitResult := Process(AOperand.LeftOperand as TdxCriteriaOperator);
  ARightVisitResult := Process(AOperand.RightOperand as TdxCriteriaOperator);
  AResult := Format('%s %s %s',
    [ALeftVisitResult.GetEnclosedResultOnGreater(APriority),
     AOperatorString,
     ARightVisitResult.GetEnclosedResultOnGreaterOrEqual(APriority)]);
  Result := TdxCriteriaToStringVisitResult.Create(AResult, APriority);
end;

function TdxCriteriaToStringBase.CreateIsNotNull(ANullOp: TdxUnaryOperator): TdxCriteriaToStringVisitResult;
var
  AInnerResult: TdxCriteriaToStringVisitResult;
  AResult: string;
begin
  AInnerResult := Process(ANullOp.Operand as TdxCriteriaOperator);
  AResult := AInnerResult.GetEnclosedResultOnGreaterOrEqual(TdxCriteriaPriorityClass.IsNull) +
    ' ' + GetIsNotNullText;
  Result := TdxCriteriaToStringVisitResult.Create(AResult, TdxCriteriaPriorityClass.IsNull);
end;

function TdxCriteriaToStringBase.GetNotLikeText: string;
begin
  Result := 'Not Like';
end;

function TdxCriteriaToStringBase.CreateNotLike(const ALikeOp: IdxBinaryOperator): TdxCriteriaToStringVisitResult;
var
  ALeftResult, ARightResult: TdxCriteriaToStringVisitResult;
  AResult: string;
begin
  ALeftResult := Process(ALikeOp.LeftOperand);
  ARightResult := Process(ALikeOp.RightOperand);
  AResult := ALeftResult.GetEnclosedResultOnGreaterOrEqual(TdxCriteriaPriorityClass.CmpEq) +
    ' ' + GetNotLikeText + ' ' +
    ARightResult.GetEnclosedResultOnGreaterOrEqual(TdxCriteriaPriorityClass.CmpEq);
  Result := TdxCriteriaToStringVisitResult.Create(AResult, TdxCriteriaPriorityClass.CmpEq);
end;

function TdxCriteriaToStringBase.Visit(const AOperand: IdxUnaryOperator): TdxCriteriaToStringVisitResult;
var
  ANullOp: TdxUnaryOperator;
  AOperator: TdxUnaryOperator;
  AFnLikeOp: TdxFunctionOperator;
  AInnerResult: TdxCriteriaToStringVisitResult;
  APriority: TdxCriteriaPriorityClass;
  AResult, AOperatorString: string;
begin
  if AOperand.OperatorType = TdxUnaryOperatorType.Not then
  begin
    ANullOp := Safe<TdxUnaryOperator>.Cast(AOperand.Operand as TdxCriteriaOperator);
    if (ANullOp <> nil) and (ANullOp.OperatorType = TdxUnaryOperatorType.IsNull) then
      Exit(CreateIsNotNull(ANullOp));

    AFnLikeOp := Safe<TdxFunctionOperator>.Cast(AOperand.Operand as TdxCriteriaOperator);
    if TdxLikeCustomFunction.IsBinaryCompatibleLikeFunction(AFnLikeOp) then
    begin
      AOperator := TdxUnaryOperator.Create(TdxUnaryOperatorType.Not, TdxLikeCustomFunction.Convert(AFnLikeOp));
      try
        Exit(Process(AOperator));
      finally
        AOperator.Free;
      end;
    end;
  end;
  AInnerResult := Process(AOperand.Operand as TdxCriteriaOperator);
  case AOperand.OperatorType of
    TdxUnaryOperatorType.BitwiseNot:
      APriority := TdxCriteriaPriorityClass.BinaryNot;
    TdxUnaryOperatorType.IsNull:
      APriority := TdxCriteriaPriorityClass.IsNull;
    TdxUnaryOperatorType.Minus,
    TdxUnaryOperatorType.Plus:
      APriority := TdxCriteriaPriorityClass.Neg;
    TdxUnaryOperatorType.Not:
      APriority := TdxCriteriaPriorityClass.Not;
    else
      raise EInvalidOperation.Create('');
  end;
  AResult := AInnerResult.GetEnclosedResultOnGreater(APriority);
  if AOperand.OperatorType = TdxUnaryOperatorType.IsNull then
    AResult := AResult + ' ' + GetIsNullText
  else
  begin
    AOperatorString := GetOperatorString(AOperand.OperatorType);
    AResult := AOperatorString + ' ' + AResult;
  end;
  Result := TdxCriteriaToStringVisitResult.Create(AResult, APriority);
end;

function TdxCriteriaToStringBase.Visit(const AOperand: IdxInOperator): TdxCriteriaToStringVisitResult;
var
  AResult: TdxCriteriaToStringVisitResult;
  AStrRes: string;
begin
  AResult := Process(AOperand.LeftOperand);
  AStrRes := AResult.GetEnclosedResultOnGreaterOrEqual(TdxCriteriaPriorityClass.InBetween) +
    ' ' + GetInText + ' (' +
    ProcessToCommaDelimitedList(AOperand.Operands) +
    ')';
  Result := TdxCriteriaToStringVisitResult.Create(AStrRes, TdxCriteriaPriorityClass.InBetween);
end;

function TdxCriteriaToStringBase.Visit(const AOperand: IdxGroupOperator): TdxCriteriaToStringVisitResult;
var
  ADelimiter: string;
  ABasePriority: TdxCriteriaPriorityClass;
  ACurrentResult: TdxCriteriaToStringVisitResult;
  AResult: TStringBuilder;
  I: Integer;
begin
  case AOperand.Operands.Count of
    0:
      Exit(TdxCriteriaToStringVisitResult.Null);
    1:
      Exit(Process(TdxCriteriaOperator(AOperand.Operands[0])));
  end;
  ADelimiter := ' ' + GetOperatorString(AOperand.OperatorType) + ' ';
  case AOperand.OperatorType of
    TdxGroupOperatorType.And:
      ABasePriority := TdxCriteriaPriorityClass.And;
    TdxGroupOperatorType.Or:
      ABasePriority := TdxCriteriaPriorityClass.Or;
    else
      raise EInvalidOperation.Create('');
  end;
  ACurrentResult := Process(TdxCriteriaOperator(AOperand.Operands[0]));
  AResult := TStringBuilder.Create(ACurrentResult.GetEnclosedResultOnGreater(ABasePriority));
  try
    for I := 1 to AOperand.Operands.Count - 1 do
    begin
      AResult.Append(ADelimiter);
      ACurrentResult := Process(TdxCriteriaOperator(AOperand.Operands[I]));
      AResult.Append(ACurrentResult.GetEnclosedResultOnGreater(ABasePriority));
    end;
    Result := TdxCriteriaToStringVisitResult.Create(AResult.ToString, ABasePriority);
  finally
    AResult.Free;
  end;
end;

function TdxCriteriaToStringBase.GetFunctionText(AOperandType: TdxFunctionOperatorType): string;
begin
  Result := GetEnumName(TypeInfo(TdxFunctionOperatorType), Ord(AOperandType));
end;

function TdxCriteriaToStringBase.Visit(const AOperand: IdxFunctionOperator): TdxCriteriaToStringVisitResult;
var
  AFunctionName, AResult: string;
  AOperands: IdxCriteriaOperatorCollection;
  AOperandProperty: IdxCriteriaOperator;
  AOperator: IdxBinaryOperator;
begin
  if AOperand.OperatorType = TdxFunctionOperatorType.Custom then
  begin
    if TdxLikeCustomFunction.IsBinaryCompatibleLikeFunction(AOperand) then
    begin
      AOperator := TdxLikeCustomFunction.Convert(AOperand);
      Exit(Process(AOperator));
    end;
    AFunctionName := GetCustomFunctionText((TdxOperandValue(AOperand.Operands[0])).Value.AsString);
    if TdxCriteriaLexer.IsGoodUnescapedName(AFunctionName) then
      AResult := AFunctionName
    else
      begin
        AOperandProperty := TdxOperandProperty.Create(AFunctionName);
        AResult := (AOperandProperty as TdxCriteriaOperator).ToString;
      end;
    AOperands := TdxCriteriaOperatorCollection.Create;
    TdxCriteriaOperatorCollection(AOperands).AddRange(AOperand.Operands);
    TdxCriteriaOperatorCollection(AOperands).Delete(0);
    AResult := AResult + '(' +
      ProcessToCommaDelimitedList(AOperands) +
      ')';
    Result := TdxCriteriaToStringVisitResult.Create(AResult);
  end
  else
  begin
    AResult := GetFunctionText(AOperand.OperatorType) +
      '(' +
      ProcessToCommaDelimitedList(AOperand.Operands) +
      ')';
    Result := TdxCriteriaToStringVisitResult.Create(AResult);
  end;
end;

function TdxCriteriaToStringBase.GetCustomFunctionText(const P: string): string;
begin
  Result := P;
end;

function TdxCriteriaToStringBase.GetOperatorString(AOperandType: TdxAggregateFunctionType): string;
begin
  Result := GetEnumName(TypeInfo(TdxAggregateFunctionType), Ord(AOperandType));
end;

function TdxCriteriaToStringBase.Visit(const AOperand: IdxAggregateOperand): TdxCriteriaToStringVisitResult;
var
  AToStringProperty: IdxOperandProperty;
  AResult: TStringBuilder;
  AOperandVisitResult, AAggregatedPropertyVisitResult: TdxCriteriaToStringVisitResult;
begin
  AToStringProperty := TdxOperandProperty(AOperand.CollectionProperty);
  if AToStringProperty = nil then
    AToStringProperty := TdxOperandProperty.Create;
  AResult := TStringBuilder.Create;
  try
    AResult.Append(Process(AToStringProperty as TdxOperandProperty).Result)
      .Append('[');
    AOperandVisitResult := Process(AOperand.Condition);
    if not AOperandVisitResult.IsNull then
      AResult.Append(AOperandVisitResult.Result);
    AResult.Append(']');
    if (AOperand.AggregateFunctionType <> TdxAggregateFunctionType.Exists) or (AOperand.AggregatedExpression <> nil) then
    begin
      if AResult.ToString = '[][]' then
        AResult.Clear
      else
        AResult.Append('.');
      AResult.Append(GetOperatorString(AOperand.AggregateFunctionType))
        .Append('(');
      AAggregatedPropertyVisitResult := Process(AOperand.AggregatedExpression);
      if not AAggregatedPropertyVisitResult.IsNull then
        AResult.Append(AAggregatedPropertyVisitResult.Result);
      AResult.Append(')');
    end;
    Result := TdxCriteriaToStringVisitResult.Create(AResult.ToString);
  finally
    AResult.Free;
  end;
end;

function TdxCriteriaToStringBase.Visit(const AOperand: IdxJoinOperand): TdxCriteriaToStringVisitResult;
var
  AToStringProperty: IdxOperandProperty;
  AResult: TStringBuilder;
  AOperandVisitResult, AAggregatedPropertyVisitResult: TdxCriteriaToStringVisitResult;
begin
  AToStringProperty := TdxOperandProperty.Create('<' + AOperand.JoinTypeName + '>');
  AResult := TStringBuilder.Create;
  try
    AResult.Append(Process(AToStringProperty as TdxOperandProperty).Result).
      Append('[');
    AOperandVisitResult := Process(AOperand.Condition);
    if not AOperandVisitResult.IsNull then
      AResult.Append(AOperandVisitResult.Result);
    AResult.Append(']');
    if (AOperand.AggregateFunctionType <> TdxAggregateFunctionType.Exists) or (AOperand.AggregatedExpression <> nil) then
    begin
      AResult.Append('.').
        Append(GetOperatorString(AOperand.AggregateFunctionType)).
        Append('(');
      AAggregatedPropertyVisitResult := Process(AOperand.AggregatedExpression);
      if not AAggregatedPropertyVisitResult.IsNull then
        AResult.Append(AAggregatedPropertyVisitResult.Result);
      AResult.Append(')');
    end;
    Result := TdxCriteriaToStringVisitResult.Create(AResult.ToString);
  finally
    AResult.Free;
  end;
end;

function TdxCriteriaToStringBase.Visit(const AOperand: IdxOperandProperty): TdxCriteriaToStringVisitResult;
var
  AResult: string;
begin
  AResult := AOperand.PropertyName;
  {$IFDEF DELPHIXE3}
  AResult := '[' + AResult.Replace('\', '\\').Replace(']', '\]').Replace(#10, '\n').Replace(#13, '\r').Replace(#9, '\t') + ']';
  {$ELSE}
  AResult := '[' +  TdxStringHelper.Replace(AResult, '\', '\\');
  AResult := TdxStringHelper.Replace(AResult, ']', '\]');
  AResult := TdxStringHelper.Replace(AResult, #10, '\n');
  AResult := TdxStringHelper.Replace(AResult, #13, '\r');
  AResult := TdxStringHelper.Replace(AResult, #9, '\t') + ']';
  {$ENDIF}
  Result := TdxCriteriaToStringVisitResult.Create(AResult);
end;

function TdxCriteriaToStringBase.Process(const AOperand: IdxCriteriaOperator): TdxCriteriaToStringVisitResult;
begin
  if AOperand = nil then
    Result := TdxCriteriaToStringVisitResult.Null
  else
    Result := (AOperand as TdxCriteriaOperator).Accept(Self);
end;

function TdxCriteriaToStringBase.Visit(const AOperand: IdxQueryOperand): TdxCriteriaToStringVisitResult;
var
  AColumnType, AResult: string;
begin
  if AOperand.ColumnType <> TdxDBColumnType.Unknown then
    AColumnType := ',' + GetEnumName(TypeInfo(TdxDBColumnType), Ord(AOperand.ColumnType))
  else
    AColumnType := '';
  AResult := AOperand.NodeAlias + '.{' + AOperand.ColumnName + AColumnType + '}';
  Result := TdxCriteriaToStringVisitResult.Create(AResult);
end;

function TdxCriteriaToStringBase.Visit(const AOperand: IdxQuerySubQueryContainer): TdxCriteriaToStringVisitResult;
var
  AResult: TStringBuilder;
begin
  AResult := TStringBuilder.Create;
  try
    AResult.Append('SubQuery(').
      Append(GetEnumName(TypeInfo(TdxAggregateFunctionType), Ord(AOperand.AggregateFunctionType))).
      Append(',');
    if AOperand.AggregateProperty <> nil then
      AResult.Append(Process(AOperand.AggregateProperty).Result);
    AResult.Append(',');
    if AOperand.Node <> nil then
      AResult.Append(AOperand.Node.ToString);
    AResult.Append(')');
    Result := TdxCriteriaToStringVisitResult.Create(AResult.ToString);
  finally
    AResult.Free;
  end;
end;

{ TdxCriteriaToStringWithParametersProcessor }

constructor TdxCriteriaToStringWithParametersProcessor.Create;
begin
  FParameters := TList<IdxOperandValue>.Create;
end;

destructor TdxCriteriaToStringWithParametersProcessor.Destroy;
begin
  FreeAndNil(FParameters);
  inherited Destroy;
end;

function TdxCriteriaToStringWithParametersProcessor.Visit(const AOperand: IdxOperandValue): TdxCriteriaToStringVisitResult;
var
  AParam: TdxOperandParameter;
begin
  FParameters.Add(AOperand);
  AParam := Safe<TdxOperandParameter>.Cast(AOperand as TdxCriteriaOperator);
  if AParam <> nil then
    Result := TdxCriteriaToStringVisitResult.Create(NamedParameterPrefix + AParam.ParameterName)
  else
    Result := TdxCriteriaToStringVisitResult.Create(ParameterPrefix);
end;

{ TdxCriteriaToStringParameterlessProcessor }

class function TdxCriteriaToStringParameterlessProcessor.ValueToString(AValue: TValue): string;
begin
  Result := ValueToString(AValue, False);
end;

class function TdxCriteriaToStringParameterlessProcessor.ValueToString(const AValue: TValue; AIsLegacy: Boolean): string;
var
  ATc: TTypeKind;
  ADateTimeValue: TDateTime;
  ATimeStamp: TTimeStamp;
  ADateTimeFormatPattern: string;
  D: Double;
begin
  if AValue.IsEmpty then
    Exit(NullString);
    ATc := AValue.Kind;
  case ATc of
    tkUnknown:
      Result := NullString;
    tkInteger:
//      if AValue is TdxEnum then
//        Exit(UserTypeToString(AValue, AIsLegacy));
      if AIsLegacy then
        Result := IntToStr(AValue.AsOrdinal)
      else
        Result := AValue.ToString + GetSuffix(AValue);
    tkChar, tkWChar:
      Result := #$27 + AValue.AsString + IfThen(AIsLegacy, '''', '''c');
    tkEnumeration:
      if AValue.TypeInfo = TypeInfo(Boolean) then
        Result := IfThen(AValue.AsBoolean, 'True', 'False')
      else
        Result := UserTypeToString(AValue, AIsLegacy);
    tkFloat:
      begin
        if AValue.IsDateTime then
        begin
          ADateTimeValue := AValue.AsType<TDateTime>;
          ATimeStamp := DateTimeToTimeStamp(ADateTimeValue);
          if ATimeStamp.Time = 0 then
            ADateTimeFormatPattern := 'yyyy-MM-dd'
          else
            ADateTimeFormatPattern := 'yyyy-MM-dd HH:mm:ss';
          Result := '#' + FormatDateTime(ADateTimeFormatPattern, ADateTimeValue) + '#';
        end
        else
        begin
          if AValue.IsSingle then
            D := RoundTo(AValue.AsSingle, -6)
          else
            D := AValue.AsExtended;
          Result := FixNonFixedText(FloatToStr(D, InvariantCulture), AIsLegacy, AValue); //ToString('r', CultureInfo.InvariantCulture)
        end;
      end;
    tkString, tkLString, tkWString, tkUString:
      {$IFDEF DELPHIXE3}
      Result := #$27 + AValue.AsString.Replace(#$27, #$27#$27) + #$27;
      {$ELSE}
      Result := #$27 + TdxStringHelper.Replace(AValue.AsString, #$27, #$27#$27) + #$27;
      {$ENDIF}
    else
      if AValue.IsType<TGuid> then
        Exit(GUIDToString(AValue.AsType<TGuid>))
      else
        if AValue.IsType<TTimeSpan> then
        {$IFDEF DELPHIXE3}
          Result := '#' + string(AValue.AsType<TTimeSpan>).Replace('#', '##') + '#'
        {$ELSE}
          Result := '#' + TdxStringHelper.Replace(AValue.AsType<TTimeSpan>, '#', '##') + '#'
        {$ENDIF}
        else
          Result := UserTypeToString(AValue, AIsLegacy);
  end;
end;

class function TdxCriteriaToStringParameterlessProcessor.UserTypeToString(const AValue: TValue; AIsLegacy: Boolean): string;
begin
  Result := AValue.ToString;
end;


class function TdxCriteriaToStringParameterlessProcessor.OperandValueOrParameterToString(const AVal: IdxOperandValue; AIsLegacy: Boolean): string;
var
  P: TdxOperandParameter;
begin
  if AVal = nil then
    raise EArgumentNilException.Create('val');
  P := Safe<TdxOperandParameter>.Cast(AVal as TdxCriteriaOperator);
  if P <> nil then
    Exit(TdxCriteriaToStringWithParametersProcessor.NamedParameterPrefix + P.ParameterName);
  if (AVal.Value.IsEmpty) and not (AVal is TdxConstantValue) then
    Exit(TdxCriteriaToStringWithParametersProcessor.ParameterPrefix);
  Result := ValueToString(AVal.Value, AIsLegacy);
end;

class function TdxCriteriaToStringParameterlessProcessor.FixNonFixedText(const AToFix: string; AIsLegacy: Boolean; const AValue: TValue): string;
begin
  {$IFDEF DELPHIXE3}
  if AToFix.IndexOfAny(['.', 'e', 'E']) < 0 then
  {$ELSE}
  if TdxStringHelper.IndexOfAny(AToFix, ['.', 'e', 'E']) < 0 then
  {$ENDIF}
    Result := AToFix + '.0'
  else
    Result := AToFix;
  if not AIsLegacy then
    Result := Result + GetSuffix(AValue);
end;

class function TdxCriteriaToStringParameterlessProcessor.GetSuffix(const AValue: TValue): string;
var
  ATypeInfo: PTypeInfo;
begin
  ATypeInfo := AValue.TypeInfo;
  if ATypeInfo = TypeInfo(Single) then
    Result := 'f'
  else
  if ATypeInfo = TypeInfo(Byte) then
    Result := 'b'
  else
  if ATypeInfo = TypeInfo(ShortInt) then
    Result := 'sb'
  else
  if ATypeInfo = TypeInfo(SmallInt) then
    Result := 's'
  else
  if ATypeInfo = TypeInfo(Word) then
    Result := 'us'
  else
  if ATypeInfo = TypeInfo(Cardinal) then
    Result := 'u'
  else
  if ATypeInfo = TypeInfo(Int64) then
    Result := 'L'
    else
      Result := '';
end;

class function TdxCriteriaToStringParameterlessProcessor.ValueToCriteriaToStringVisitResult(const AOperand: IdxOperandValue): TdxCriteriaToStringVisitResult;
begin
  Result := TdxCriteriaToStringVisitResult.Create(OperandValueOrParameterToString(AOperand, False));
end;

function TdxCriteriaToStringParameterlessProcessor.Visit(const AOperand: IdxOperandValue): TdxCriteriaToStringVisitResult;
begin
  Result := ValueToCriteriaToStringVisitResult(AOperand);
end;

{ TdxCriteriaToBasicStyleParameterlessProcessor }

class constructor TdxCriteriaToBasicStyleParameterlessProcessor.Initialize;
begin
  FInstance := TdxCriteriaToBasicStyleParameterlessProcessor.Create;
end;

class destructor TdxCriteriaToBasicStyleParameterlessProcessor.Finalize;
begin
  FreeAndNil(FInstance);
end;

class function TdxCriteriaToBasicStyleParameterlessProcessor.GetBasicOperatorString(AOperatorType: TdxBinaryOperatorType): string;
begin
  case AOperatorType of
    TdxBinaryOperatorType.BitwiseAnd:
      Exit('&');
    TdxBinaryOperatorType.BitwiseOr:
      Exit('|');
    TdxBinaryOperatorType.BitwiseXor:
      Exit('^');
    TdxBinaryOperatorType.Divide:
      Exit('/');
    TdxBinaryOperatorType.Equal:
      Exit('=');
    TdxBinaryOperatorType.Greater:
      Exit('>');
    TdxBinaryOperatorType.GreaterOrEqual:
      Exit('>=');
    TdxBinaryOperatorType.Less:
      Exit('<');
    TdxBinaryOperatorType.LessOrEqual:
      Exit('<=');
    TdxBinaryOperatorType.Minus:
      Exit('-');
    TdxBinaryOperatorType.Modulo:
      Exit('%');
    TdxBinaryOperatorType.Multiply:
      Exit('*');
    TdxBinaryOperatorType.NotEqual:
      Exit('<>');
    TdxBinaryOperatorType.Plus:
      Exit('+');
    else
      raise EInvalidOperation.Create('');
  end;
end;

class function TdxCriteriaToBasicStyleParameterlessProcessor.GetBasicOperatorString(AOperatorType: TdxUnaryOperatorType): string;
begin
  case AOperatorType of
    TdxUnaryOperatorType.BitwiseNot:
      Exit('~');
    TdxUnaryOperatorType.Minus:
      Exit('-');
    TdxUnaryOperatorType.Not:
      Exit('Not');
    TdxUnaryOperatorType.Plus:
      Exit('+');
    else
      raise EInvalidOperation.Create('');
  end;
end;

class function TdxCriteriaToBasicStyleParameterlessProcessor.GetBasicOperatorString(AOperatorType: TdxGroupOperatorType): string;
begin

  case AOperatorType of
    TdxGroupOperatorType.And:
      Exit('And');
    TdxGroupOperatorType.Or:
      Exit('Or');
    else
      raise EInvalidOperation.Create('');
  end;
end;

function TdxCriteriaToBasicStyleParameterlessProcessor.GetOperatorString(AOperatorType: TdxUnaryOperatorType): string;
begin
  Result := GetBasicOperatorString(AOperatorType);
end;

function TdxCriteriaToBasicStyleParameterlessProcessor.GetOperatorString(AOperatorType: TdxBinaryOperatorType): string;
begin
  Result := GetBasicOperatorString(AOperatorType);
end;

function TdxCriteriaToBasicStyleParameterlessProcessor.GetOperatorString(AOperatorType: TdxGroupOperatorType): string;
begin
  Result := GetBasicOperatorString(AOperatorType);
end;

class function TdxCriteriaToBasicStyleParameterlessProcessor.ToString(const AOperand: IdxCriteriaOperator): string;
begin
  if AOperand = nil then
    Result := ''
  else
    Result := FInstance.Process(AOperand as TdxCriteriaOperator).Result;
end;


{ TdxCriteriaOperator }


class function TdxCriteriaOperator.Parse(const AStringCriteria: string; out ACriteriaParametersList: TArray<IdxOperandValue>): IdxCriteriaOperator;
begin
  Result := TdxCriteriaParser.Parse(AStringCriteria, ACriteriaParametersList);
end;

class function TdxCriteriaOperator.ObjectToCriteriaSafe(AObject: TObject): TdxCriteriaOperator;
begin
  Result := Safe<TdxCriteriaOperator>.Cast(AObject);
  if Result = nil then
    Result := TdxOperandValue.Create(AObject);
end;

class function TdxCriteriaOperator.Parse(const ACriteria: string; const AParameters: array of TValue): IdxCriteriaOperator;
var
  ACriteriaParametersList: TArray<IdxOperandValue>;
  I: Integer;
begin
  Result := Parse(ACriteria, ACriteriaParametersList);
  if (Length(AParameters) > 0) and (ACriteriaParametersList <> nil) then
  begin
    for I := 0 to Min(Length(AParameters), Length(ACriteriaParametersList)) - 1 do
      (ACriteriaParametersList[I] as TdxOperandValue).Value := AParameters[I];
  end;
end;

class function TdxCriteriaOperator.Parse(const ACriteria: string): IdxCriteriaOperator;
var
  ACriteriaParametersList: TArray<IdxOperandValue>;
begin
  Result := Parse(ACriteria, ACriteriaParametersList);
end;


class function TdxCriteriaOperator.ParseList(const ACriteriaList: string): TArray<IdxCriteriaOperator>;
var
  ACriteriaParametersList: TArray<IdxOperandValue>;
begin
  Result := ParseList(ACriteriaList, ACriteriaParametersList);
end;

class function TdxCriteriaOperator.ParseList(const ACriteriaList: string;
  out ACriteriaParametersList: TArray<IdxOperandValue>): TArray<IdxCriteriaOperator>;
begin
  Result := TdxCriteriaParser.ParseList(ACriteriaList, ACriteriaParametersList, False);
end;

class function TdxCriteriaOperator.ParseList(const ACriteriaList: string;
  const AParameters: array of TValue): TArray<IdxCriteriaOperator>;
var
  ACriteriaParametersList: TArray<IdxOperandValue>;
  I: Integer;
begin
  Result := ParseList(ACriteriaList, ACriteriaParametersList);
  if (Length(AParameters) <> 0) and (ACriteriaParametersList <> nil) then
    for I := 0 to Math.Min(Length(AParameters), Length(ACriteriaParametersList)) - 1 do
      ACriteriaParametersList[I].Value := AParameters[I];
end;

class function TdxCriteriaOperator.ToBasicStyleString(ACriteria: TdxCriteriaOperator): string;
begin
  Result := TdxCriteriaToBasicStyleParameterlessProcessor.ToString(ACriteria);
end;


class function TdxCriteriaOperator.ToString(const ACriteria: IdxCriteriaOperator): string;
begin
  Result := ToBasicStyleString(ACriteria as TdxCriteriaOperator);
end;


function TdxCriteriaOperator.ToString: string;
begin
  Result := ToString(Self);
end;

//function TdxCriteriaOperator.Accept<T>(const AVisitor: IdxCriteriaVisitor<T>): T;
//begin
//  Result := AVisitor.Visit(Self);
//end;

class function TdxCriteriaOperator.&Or(const ALeftOperator, ARightOperator: IdxCriteriaOperator): IdxCriteriaOperator;
begin
  Result := TdxGroupOperator.Combine(TdxGroupOperatorType.Or, ALeftOperator, ARightOperator) as TdxCriteriaOperator;
end;

class function TdxCriteriaOperator.&And(const ALeftOperator, ARightOperator: IdxCriteriaOperator): IdxCriteriaOperator;
begin
  Result := TdxGroupOperator.Combine(TdxGroupOperatorType.And, ALeftOperator, ARightOperator);
end;

class function TdxCriteriaOperator.&And(const AOperands: TArray<IdxCriteriaOperator>): IdxCriteriaOperator;
begin
  Result := TdxGroupOperator.Combine(TdxGroupOperatorType.And, AOperands);
end;

class function TdxCriteriaOperator.&And(AOperands: TdxCriteriaOperatorList): IdxCriteriaOperator;
begin
  Result := TdxGroupOperator.Combine(TdxGroupOperatorType.And, AOperands);
end;

class function TdxCriteriaOperator.&Or(AOperands: TdxCriteriaOperatorList): IdxCriteriaOperator;
begin
  Result := TdxGroupOperator.Combine(TdxGroupOperatorType.Or, AOperands);
end;


function TdxCriteriaOperator.&Not: IdxUnaryOperator;
begin
  Result := TdxUnaryOperator.Create(TdxUnaryOperatorType.&Not, Self);
end;


class function TdxCriteriaOperator.GetCustomFunctionCount: Integer;
begin
  Result := 0;
end;


class function TdxCriteriaOperator.CriterionEquals(ALeftOperator: TdxCriteriaOperator; ARightOperator: TdxCriteriaOperator): Boolean;
begin
  if ALeftOperator = ARightOperator then
    Exit(True);
  if ALeftOperator = nil then
    Exit(False);
  Result := ALeftOperator.Equals(ARightOperator);
end;

class function TdxCriteriaOperator.Equals(AObjectA, AObjectB: TObject): Boolean;
begin
  if (AObjectA = nil) and (AObjectB = nil) then
    Exit(True);
  if (AObjectA = nil) or (AObjectB = nil) then
    Exit(False);
  Result := AObjectA.Equals(AObjectB);
end;

class function TdxCriteriaOperator.Equals(const ACriteriaA, ACriteriaB: IdxCriteriaOperator): Boolean;
begin
  if (ACriteriaA = nil) and (ACriteriaB = nil) then
    Exit(True);
  if (ACriteriaA = nil) or (ACriteriaB = nil) then
    Exit(False);
  Result := (ACriteriaA as TdxCriteriaOperator).Equals(ACriteriaB as TdxCriteriaOperator);
end;

{ TdxCriteriaOperatorCollection.TEnumerator }

constructor TdxCriteriaOperatorCollection.TEnumerator.Create(ACriteriaOperatorCollection: TdxCriteriaOperatorCollection);
begin
  inherited Create;
  FCriteriaOperatorCollection := ACriteriaOperatorCollection;
  FIndex := -1;
end;

function TdxCriteriaOperatorCollection.TEnumerator.GetCurrent: TObject;
begin
  Result := GetCurrentGeneric as TObject;
end;

function TdxCriteriaOperatorCollection.TEnumerator.GetCurrentGeneric: IdxCriteriaOperator;
begin
  if FIndex >= 0 then
    Result := FCriteriaOperatorCollection[FIndex]
  else
    Result := nil;
end;

function TdxCriteriaOperatorCollection.TEnumerator.MoveNext: Boolean;
begin
  Result := FIndex < FCriteriaOperatorCollection.Count - 1;
  if Result then
    Inc(FIndex);
end;

procedure TdxCriteriaOperatorCollection.TEnumerator.Reset;
begin
  FIndex := -1;
end;

{ TdxCriteriaOperatorList }

function TdxCriteriaOperatorList.ToString: string;
var
  AResult: TStringBuilder;
  AOperator: IdxCriteriaOperator;
begin
  AResult := TStringBuilder.Create;
  try
    for AOperator in Self do
    begin
      if AResult.Length > 0 then
        AResult.Append(', ');
      AResult.Append(TdxCriteriaOperator.ToString(AOperator as TdxCriteriaOperator));
    end;
    Result := AResult.ToString;
  finally
    AResult.Free;
  end;
end;


{ TdxCriteriaOperatorCollection }

constructor TdxCriteriaOperatorCollection.Create;
begin
  inherited Create;
  FList := TdxCriteriaOperatorList.Create;
end;

constructor TdxCriteriaOperatorCollection.Create(ACapacity: Integer);
begin
  Create;
  FList.Capacity := ACapacity;
end;

destructor TdxCriteriaOperatorCollection.Destroy;
begin
  FreeAndNil(FList);
  inherited Destroy;
end;

procedure TdxCriteriaOperatorCollection.Delete(AIndex: Integer);
begin
  FList.Delete(AIndex);
end;

procedure TdxCriteriaOperatorCollection.DeleteRange(AIndex, ACount: Integer);
begin
  FList.DeleteRange(AIndex, ACount);
end;

function TdxCriteriaOperatorCollection.Add(const AValue: IdxCriteriaOperator): Integer;
begin
  Result := FList.Add(AValue);
end;

procedure TdxCriteriaOperatorCollection.AddRange(const AOperands: TArray<IdxCriteriaOperator>);
begin
  FList.AddRange(AOperands);
end;

procedure TdxCriteriaOperatorCollection.AddRange(const AOperands: array of IdxCriteriaOperator);
begin
  FList.AddRange(AOperands);
end;

procedure TdxCriteriaOperatorCollection.AddRange(AOperands: TEnumerable<IdxCriteriaOperator>);
begin
  FList.AddRange(AOperands);
end;

procedure TdxCriteriaOperatorCollection.AddRange(const AOperands: IEnumerable<IdxCriteriaOperator>);
begin
  FList.AddRange(AOperands);
end;

function TdxCriteriaOperatorCollection.Contains(const AValue: IdxCriteriaOperator): Boolean;
begin
  Result := FList.Contains(AValue);
end;

function TdxCriteriaOperatorCollection.Equals(AObject: TObject): Boolean;
var
  AAnother: TdxCriteriaOperatorCollection;
  I: Integer;
begin
  AAnother := Safe<TdxCriteriaOperatorCollection>.Cast(AObject);
  if AAnother = nil then
    Exit(False);
  if Count <> AAnother.Count then
    Exit(False);
  for I := 0 to Count - 1 do
    if not TdxCriteriaOperator.Equals(Self[I] as TdxCriteriaOperator, AAnother[I] as TdxCriteriaOperator) then
      Exit(False);
  Result := True;
end;

function TdxCriteriaOperatorCollection.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TdxCriteriaOperatorCollection.GetEnumerator: IEnumerator;
begin
  Result := GetEnumeratorGeneric;
end;

function TdxCriteriaOperatorCollection.GetEnumeratorGeneric: IEnumerator<IdxCriteriaOperator>;
begin
  Result := TEnumerator.Create(Self);
end;

function TdxCriteriaOperatorCollection.GetHashCode: Integer;
var
  O: IdxCriteriaOperator;
begin
  Result := 0;
  for O in FList do
    Result := Result xor (O as TdxCriteriaOperator).GetHashCode;
end;

function TdxCriteriaOperatorCollection.GetItems(AIndex: Integer): IdxCriteriaOperator;
begin
  Result := FList.Items[AIndex];
end;

function TdxCriteriaOperatorCollection.IndexOf(const AItem: IdxCriteriaOperator): Integer;
begin
  Result := FList.IndexOf(AItem);
end;

procedure TdxCriteriaOperatorCollection.Remove(const AValue: IdxCriteriaOperator);
begin
  FList.Remove(AValue)
end;

procedure TdxCriteriaOperatorCollection.SetItems(AIndex: Integer; const Value: IdxCriteriaOperator);
begin
  FList[AIndex] := Value;
end;

function TdxCriteriaOperatorCollection.ToArray: TArray<IdxCriteriaOperator>;
begin
  Result := FList.ToArray;
end;

function TdxCriteriaOperatorCollection.ToString: string;
begin
  Result := FList.ToString;
end;

{ TdxOperandValue }

constructor TdxOperandValue.Create(const AValue: TValue);
begin
  inherited Create;
  FValue := AValue;
end;

constructor TdxOperandValue.Create(const AValue: Char);
begin
  Create(TValue.From<Char>(AValue));
end;

constructor TdxOperandValue.Create(const AValue: TDateTime);
begin
  Create(TValue.From<TDateTime>(AValue));
end;

constructor TdxOperandValue.Create(const AValue: string);
begin
  Create(TValue.From<string>(AValue));
end;

class function TdxOperandValue.Create<T>(const AValue: T): TdxOperandValue;
begin
  Result := Self.Create(TValue.From<T>(AValue));
end;

function TdxOperandValue.Equals(AObject: TObject): Boolean;
var
  AOperandValue: TdxOperandValue absolute AObject;
begin
  if (AObject = nil) or (ClassType <> AObject.ClassType) then
    Exit(False);
  if (Value.Kind = AOperandValue.Value.Kind) or (Value.IsString and AOperandValue.Value.IsString) then
  begin
    if (Value.Kind = tkFloat) and (Value.IsDateTime xor AOperandValue.Value.IsDateTime) then
      Result := False
    else
      Result := Value.Equals(AOperandValue.Value);
  end
  else
    Result := False
end;

function TdxOperandValue.GetHashCode: Integer;
begin
  Result := GetValueHash(Value);
end;

function TdxOperandValue.GetValue: TValue;
begin
  Result := FValue
end;

procedure TdxOperandValue.SetValue(const AValue: TValue);
begin
  FValue := AValue;
end;

function TdxOperandValue.Accept(const AVisitor: IdxCriteriaOperatorVisitor): IdxCriteriaOperator;
begin
  Result := AVisitor.Visit(Self);
end;

function TdxOperandValue.Accept(const AVisitor: IdxSQLGeneratorVisitor): string;
begin
  Result := AVisitor.Visit(Self);
end;

function TdxOperandValue.Accept(const AVisitor: IdxClientCriteriaToStringVisitor): TdxCriteriaToStringVisitResult;
begin
  Result := AVisitor.Visit(Self);
end;

function TdxOperandValue.Accept(const AVisitor: IdxCriteriaVisitorListString): TList<string>;
begin
  Result := AVisitor.Visit(Self);
end;

function TdxOperandValue.Accept(const AVisitor: IdxClientCriteriaVisitor<TdxBooleanCriteriaState>): TdxBooleanCriteriaState;
begin
  Result := AVisitor.Visit(Self);
end;

{ TdxOperandParameter }

constructor TdxOperandParameter.Create(const AParameterName: string);
begin
  Create(AParameterName, TValue.Empty);
end;

constructor TdxOperandParameter.Create(const AParameterName: string; const AValue: TValue);
begin
  inherited Create(AValue);
  FParameterName := AParameterName;
end;

function TdxOperandParameter.GetHashCode: Integer;
begin
  if ParameterName = '' then
    Result := inherited GetHashCode
  else
    Result := inherited GetHashCode xor GetValueHash(ParameterName);
end;

function TdxOperandParameter.GetParameterName: string;
begin
  Result := FParameterName;
end;

function TdxOperandParameter.Equals(AObject: TObject): Boolean;
begin
  Result := inherited Equals(AObject) and (ParameterName = (TdxOperandParameter(AObject)).ParameterName);
end;


{ TdxOperandProperty }

constructor TdxOperandProperty.Create(const APropertyName: string);
begin
  inherited Create;
  FPropertyName := APropertyName;
end;

function TdxOperandProperty.Accept(const AVisitor: IdxCriteriaOperatorVisitor): IdxCriteriaOperator;
begin
  Result := AVisitor.Visit(Self);
end;

function TdxOperandProperty.Accept(const AVisitor: IdxClientCriteriaToStringVisitor): TdxCriteriaToStringVisitResult;
begin
  Result := AVisitor.Visit(Self);
end;

function TdxOperandProperty.Accept(const AVisitor: IdxClientCriteriaVisitor<TdxBooleanCriteriaState>): TdxBooleanCriteriaState;
begin
  Result := AVisitor.Visit(Self);
end;



function TdxOperandProperty.Equals(AObject: TObject): Boolean;
begin
  if AObject = nil then
    Exit(False);
  if ClassType <> AObject.ClassType then
    Exit(False);
  Result := PropertyName = TdxOperandProperty(AObject).PropertyName;
end;

function TdxOperandProperty.GetHashCode: Integer;
begin
  if PropertyName <> '' then
    Result := GetValueHash(PropertyName)
  else
    Result := -1;
end;

function TdxOperandProperty.GetPropertyName: string;
begin
  Result := FPropertyName;
end;


{ TdxBinaryOperator }

constructor TdxBinaryOperator.Create(const APropertyName: string; AValue: Double; AType: TdxBinaryOperatorType);
begin
  Create(APropertyName, TValue.From<Double>(AValue), AType);
end;

constructor TdxBinaryOperator.Create(const APropertyName: string; AValue: Integer; AType: TdxBinaryOperatorType);
begin
  Create(APropertyName, TValue.From<Integer>(AValue), AType);
end;

constructor TdxBinaryOperator.Create(const APropertyName, AValue: string; AType: TdxBinaryOperatorType);
begin
  Create(APropertyName, TValue.From<string>(AValue), AType);
end;

constructor TdxBinaryOperator.Create(const ALeftOperator, ARightOperator: IdxCriteriaOperator; AType: TdxBinaryOperatorType);
begin
  inherited Create;
  FLeftOperand := ALeftOperator;
  FRightOperand := ARightOperator;
  FOperatorType := AType;
end;

constructor TdxBinaryOperator.Create(const APropertyName: string; const AValue: TValue; AType: TdxBinaryOperatorType);
begin
  Create(TdxOperandProperty.Create(APropertyName), TdxOperandValue.Create(AValue), AType);
end;

function TdxBinaryOperator.GetLeftOperand: IdxCriteriaOperator;
begin
  Result := FLeftOperand;
end;

function TdxBinaryOperator.GetOperatorType: TdxBinaryOperatorType;
begin
  Result := FOperatorType;
end;

function TdxBinaryOperator.GetRightOperand: IdxCriteriaOperator;
begin
  Result := FRightOperand;
end;

function TdxBinaryOperator.Accept(const AVisitor: IdxCriteriaOperatorVisitor): IdxCriteriaOperator;
begin
  Result := AVisitor.Visit(Self);
end;

function TdxBinaryOperator.Accept(const AVisitor: IdxSQLGeneratorVisitor): string;
begin
  Result := AVisitor.Visit(Self);
end;

function TdxBinaryOperator.Accept(const AVisitor: IdxClientCriteriaToStringVisitor): TdxCriteriaToStringVisitResult;
begin
  Result := AVisitor.Visit(Self);
end;

function TdxBinaryOperator.Accept(const AVisitor: IdxCriteriaVisitorListString): TList<string>;
begin
  Result := AVisitor.Visit(Self);
end;

function TdxBinaryOperator.Accept(const AVisitor: IdxClientCriteriaVisitor<TdxBooleanCriteriaState>): TdxBooleanCriteriaState;
begin
  Result := AVisitor.Visit(Self);
end;

function TdxBinaryOperator.Equals(AObject: TObject): Boolean;
begin
  if AObject = nil then
    Exit(False);
  if ClassType <> AObject.ClassType then
    Exit(False);
  Result := (OperatorType = TdxBinaryOperator(AObject).OperatorType) and
    (LeftOperand as TdxCriteriaOperator).Equals(TdxBinaryOperator(AObject).LeftOperand as TdxCriteriaOperator) and
    (RightOperand as TdxCriteriaOperator).Equals(TdxBinaryOperator(AObject).RightOperand as TdxCriteriaOperator);
end;

function TdxBinaryOperator.GetHashCode: Integer;
begin
  if LeftOperand = nil then
    Result := -1
  else
    Result := (LeftOperand as TdxCriteriaOperator).GetHashCode;
  if RightOperand = nil then
    Result := Result xor -1
  else
    Result := Result xor (RightOperand as TdxCriteriaOperator).GetHashCode;
end;


{ TdxUnaryOperator }

constructor TdxUnaryOperator.Create(AOperatorType: TdxUnaryOperatorType; const AOperand: IdxCriteriaOperator);
begin
  inherited Create;
  FOperand := AOperand;
  FOperatorType := AOperatorType;
end;


constructor TdxUnaryOperator.Create;
begin
  Create(TdxUnaryOperatorType.Not, nil);
end;


function TdxUnaryOperator.Equals(AObject: TObject): Boolean;
begin
  if AObject = nil then
    Exit(False);
  if ClassInfo <> AObject.ClassInfo then
    Exit(False);
  Result := (OperatorType = TdxUnaryOperator(AObject).OperatorType) and (Operand as TdxCriteriaOperator).Equals(TdxUnaryOperator(AObject).Operand as TdxCriteriaOperator);
end;

function TdxUnaryOperator.GetHashCode: Integer;
begin
  if Operand <> nil then
    Result := (Operand as TdxCriteriaOperator).GetHashCode
  else
    Result := -1;
end;


function TdxUnaryOperator.GetOperand: IdxCriteriaOperator;
begin
  Result := FOperand;
end;

function TdxUnaryOperator.GetOperatorType: TdxUnaryOperatorType;
begin
  Result := FOperatorType;
end;

function TdxUnaryOperator.Accept(const AVisitor: IdxCriteriaOperatorVisitor): IdxCriteriaOperator;
begin
  Result := AVisitor.Visit(Self);
end;

function TdxUnaryOperator.Accept(const AVisitor: IdxSQLGeneratorVisitor): string;
begin
  Result := AVisitor.Visit(Self);
end;

function TdxUnaryOperator.Accept(const AVisitor: IdxClientCriteriaToStringVisitor): TdxCriteriaToStringVisitResult;
begin
  Result := AVisitor.Visit(Self);
end;

function TdxUnaryOperator.Accept(const AVisitor: IdxCriteriaVisitorListString): TList<string>;
begin
  Result := AVisitor.Visit(Self);
end;

function TdxUnaryOperator.Accept(const AVisitor: IdxClientCriteriaVisitor<TdxBooleanCriteriaState>): TdxBooleanCriteriaState;
begin
  Result := AVisitor.Visit(Self);
end;

{ TdxBetweenOperator }

constructor TdxBetweenOperator.Create(const ATestExpression, ABeginExpression, AEndExpression: IdxCriteriaOperator);
begin
  inherited Create;
  FBeginExpression := ABeginExpression;
  FEndExpression := AEndExpression;
  FTestExpression := ATestExpression;
end;

constructor TdxBetweenOperator.Create(const ATestPropertyName: string; const ABeginExpression,
  AEndExpression: IdxCriteriaOperator);
begin
  Create(TdxOperandProperty.Create(ATestPropertyName), ABeginExpression, AEndExpression);
end;

constructor TdxBetweenOperator.Create(const ATestPropertyName: string; const ABeginValue, AEndValue: TValue);
begin
  Create(TdxOperandProperty.Create(ATestPropertyName),
    TdxOperandValue.Create(ABeginValue), TdxOperandValue.Create(AEndValue));
end;

constructor TdxBetweenOperator.Create;
begin
 // Create(TdxCriteriaOperator(nil), nil, nil);
end;

function TdxBetweenOperator.GetBeginExpression: IdxCriteriaOperator;
begin
  Result := FBeginExpression;
end;

function TdxBetweenOperator.GetEndExpression: IdxCriteriaOperator;
begin
  Result := FEndExpression;
end;

function TdxBetweenOperator.GetTestExpression: IdxCriteriaOperator;
begin
  Result := FTestExpression;
end;

function TdxBetweenOperator.Accept(const AVisitor: IdxCriteriaOperatorVisitor): IdxCriteriaOperator;
begin
  Result := AVisitor.Visit(Self);
end;

function TdxBetweenOperator.Accept(const AVisitor: IdxSQLGeneratorVisitor): string;
begin
  Result := AVisitor.Visit(Self);
end;

function TdxBetweenOperator.Accept(const AVisitor: IdxClientCriteriaToStringVisitor): TdxCriteriaToStringVisitResult;
begin
  Result := AVisitor.Visit(Self);
end;

function TdxBetweenOperator.Accept(const AVisitor: IdxCriteriaVisitorListString): TList<string>;
begin
  Result := AVisitor.Visit(Self);
end;

function TdxBetweenOperator.Accept(const AVisitor: IdxClientCriteriaVisitor<TdxBooleanCriteriaState>): TdxBooleanCriteriaState;
begin
  Result := AVisitor.Visit(Self);
end;


function TdxBetweenOperator.Equals(AObject: TObject): Boolean;
begin
  if AObject = nil then
    Exit(False);
  if ClassType <> AObject.ClassType then
    Exit(False);
  Result := (TestExpression as TdxCriteriaOperator).Equals(TdxBetweenOperator(AObject).TestExpression as TdxCriteriaOperator) and
    (BeginExpression as TdxCriteriaOperator).Equals(TdxBetweenOperator(AObject).BeginExpression as TdxCriteriaOperator) and
    (EndExpression as TdxCriteriaOperator).Equals(TdxBetweenOperator(AObject).EndExpression as TdxCriteriaOperator);
end;

function TdxBetweenOperator.GetHashCode: Integer;
begin
  if TestExpression = nil then
    Result := -1
  else
    Result := (TestExpression as TdxCriteriaOperator).GetHashCode;
  if BeginExpression <> nil then
    Result := Result xor (BeginExpression as TdxCriteriaOperator).GetHashCode;
  if EndExpression <> nil then
    Result := Result xor (EndExpression as TdxCriteriaOperator).GetHashCode;
end;


{ TdxInOperator }

constructor TdxInOperator.Create(const ALeftOperand: IdxCriteriaOperator);
begin
  inherited Create;
  FOperands := TdxCriteriaOperatorCollection.Create;
  FLeftOperand := ALeftOperand;
end;

constructor TdxInOperator.Create(const ALeftOperand: IdxCriteriaOperator; const AOperands: TArray<IdxCriteriaOperator>);
begin
  Create(ALeftOperand);
  TdxCriteriaOperatorCollection(FOperands).AddRange(AOperands);
end;

constructor TdxInOperator.Create(const ALeftOperand: IdxCriteriaOperator; const AOperands: array of IdxCriteriaOperator);
begin
  Create(ALeftOperand);
  TdxCriteriaOperatorCollection(FOperands).AddRange(AOperands);
end;


constructor TdxInOperator.Create(const ALeftOperand: IdxCriteriaOperator; AOperands: TEnumerable<IdxCriteriaOperator>);
begin
  Create(ALeftOperand);
  if AOperands <> nil then
    TdxCriteriaOperatorCollection(FOperands).AddRange(AOperands);
end;

constructor TdxInOperator.Create(const ALeftOperand: IdxCriteriaOperator;
  const AOperands: IEnumerable<IdxCriteriaOperator>);
begin
  Create(ALeftOperand);
  if AOperands <> nil then
    TdxCriteriaOperatorCollection(FOperands).AddRange(AOperands);
end;

class function TdxInOperator.Create<T>(const APropertyName: string; const AOperands: array of T): TdxInOperator;
var
  I: Integer;
begin
  Result := TdxInOperator.Create(TdxOperandProperty.Create(APropertyName));
  for I := Low(AOperands) to High(AOperands) do
    Result.FOperands.Add(TdxOperandValue.Create(TValue.From<T>(AOperands[I])));
end;

constructor TdxInOperator.Create(const APropertyName: string; const AOperands: array of const);
begin
  Create(TdxOperandProperty.Create(APropertyName), AOperands);
end;

constructor TdxInOperator.Create(const ALeftOperand: IdxCriteriaOperator; const AOperands: array of const);
var
  I: Integer;
  AValue: TValue;
begin
  Create(ALeftOperand);
  for I := Low(AOperands) to High(AOperands) do
  begin
    AValue := VarRecToValue(AOperands[I]);
    if AValue.IsObject then
      FOperands.Add(AValue.AsObject as TdxCriteriaOperator)
    else
      FOperands.Add(TdxOperandValue.Create(AValue));
  end;
end;

constructor TdxInOperator.Create(const APropertyName: string; const AOperands: TArray<TObject>);
begin
  Create(TdxOperandProperty.Create(APropertyName));
  TdxCriteriaOperatorCollection(FOperands).AddRange(ObjectsToCriteriaSafe(AOperands));
end;

function TdxInOperator.Accept(const AVisitor: IdxCriteriaOperatorVisitor): IdxCriteriaOperator;
begin
  Result := AVisitor.Visit(Self);
end;

function TdxInOperator.Accept(const AVisitor: IdxSQLGeneratorVisitor): string;
begin
  Result := AVisitor.Visit(Self);
end;

function TdxInOperator.Accept(const AVisitor: IdxClientCriteriaToStringVisitor): TdxCriteriaToStringVisitResult;
begin
  Result := AVisitor.Visit(Self);
end;

function TdxInOperator.Accept(const AVisitor: IdxCriteriaVisitorListString): TList<string>;
begin
  Result := AVisitor.Visit(Self);
end;

function TdxInOperator.Accept(const AVisitor: IdxClientCriteriaVisitor<TdxBooleanCriteriaState>): TdxBooleanCriteriaState;
begin
  Result := AVisitor.Visit(Self);
end;

function TdxInOperator.GetLeftOperand: IdxCriteriaOperator;
begin
  Result := FLeftOperand;
end;

function TdxInOperator.GetOperands: IdxCriteriaOperatorCollection;
begin
  Result := FOperands;
end;

class function TdxInOperator.ObjectsToCriteriaSafe(const AOperands: TArray<TObject>): TArray<IdxCriteriaOperator>;
var
  I: Integer;
begin
  SetLength(Result, Length(AOperands));
  for I := 0 to Length(AOperands) - 1 do
    Result[I] := ObjectToCriteriaSafe(AOperands[I]);
end;

class function TdxInOperator.VarRecToValue(const AVarRec: TVarRec): TValue;
begin
  case AVarRec.VType of
    vtInteger:
      Result := AVarRec.VInteger;
    vtBoolean:
      Result := AVarRec.VBoolean;
    vtChar:
      Result := Char(AVarRec.VChar);
    vtExtended:
      Result := AVarRec.VExtended^;
    vtString:
      Result := String(AVarRec.VString^);
    vtPChar:
      Result := Char(AVarRec.VPChar);
    vtObject:
      Result := ObjectToCriteriaSafe(AVarRec.VObject);
    vtWideChar:
      Result := AVarRec.VWideChar;
    vtPWideChar:
      Result := AVarRec.VPWideChar;
    vtAnsiString:
      Result := AVarRec.VAnsiString;
    vtCurrency:
      Result := AVarRec.VCurrency^;
    vtVariant:
      Result := TValue.FromVariant(AVarRec.VVariant^);
    vtWideString:
      Result := AVarRec.VWideString;
    vtInt64:
      Result := AVarRec.VInt64^;
    vtUnicodeString:
      Result := AVarRec.VUnicodeString;
    else
      Result := TValue.Empty;
  end;
end;

function TdxInOperator.Equals(AObject: TObject): Boolean;
var
  ALeftOperands, ARightOperands: IdxCriteriaOperatorCollection;
  I: Integer;
begin
  if AObject = nil then
    Exit(False);
  if ClassType <> AObject.ClassType then
    Exit(False);
  ALeftOperands := Operands;
  ARightOperands := TdxInOperator(AObject).Operands;
  if Operands.Count <> ARightOperands.Count then
    Exit(False);
  if not (LeftOperand as TdxCriteriaOperator).Equals(TdxInOperator(AObject).LeftOperand as TdxCriteriaOperator) then
    Exit(False);
  for I := 0 to ALeftOperands.Count - 1 do
    if not (ALeftOperands[I] as TdxCriteriaOperator).Equals(ARightOperands[I] as TdxCriteriaOperator) then
      Exit(False);
  Result := True;
end;

function TdxInOperator.GetHashCode: Integer;
var
  AHash: Integer;
  AOperand: IdxCriteriaOperator;
begin
  if LeftOperand = nil then
    AHash := -1
  else
    AHash := (LeftOperand as TdxCriteriaOperator).GetHashCode;
  for AOperand in Operands do
    if AOperand <> nil then
      AHash := AHash xor (AOperand as TdxCriteriaOperator).GetHashCode;
  Result := AHash;
end;


{ TdxAggregateOperand }

constructor TdxAggregateOperand.Create;
begin
  Create(TdxOperandProperty.Create, TdxCriteriaOperator(nil), TdxAggregateFunctionType.Exists, nil);
end;

constructor TdxAggregateOperand.Create(const ACollectionProperty: string; AAggregateFunctionType: TdxAggregateFunctionType);
begin
  Create(ACollectionProperty, AAggregateFunctionType, nil);
end;

constructor TdxAggregateOperand.Create(const ACollectionProperty: string; const AAggregatedExpression: string; AAggregateFunctionType: TdxAggregateFunctionType);
begin
  Create(ACollectionProperty, AAggregatedExpression, AAggregateFunctionType, nil);
end;

constructor TdxAggregateOperand.Create(const ACollectionProperty: string; AAggregateFunctionType: TdxAggregateFunctionType;
  const ACondition: IdxCriteriaOperator);
begin
  Create(GetPropertyByName(ACollectionProperty), nil, AAggregateFunctionType, ACondition);
end;

constructor TdxAggregateOperand.Create(const ACollectionProperty: string; const AAggregatedExpression: string;
  AAggregateFunctionType: TdxAggregateFunctionType; const ACondition: IdxCriteriaOperator);
begin
  Create(GetPropertyByName(ACollectionProperty), GetPropertyByName(AAggregatedExpression), AAggregateFunctionType, ACondition);
end;

constructor TdxAggregateOperand.Create(const ACollectionProperty: IdxOperandProperty; const AAggregatedExpression: IdxCriteriaOperator;
  AAggregateFunctionType: TdxAggregateFunctionType; const ACondition: IdxCriteriaOperator);
begin
  inherited Create;
  FCondition := ACondition;
  FProperty := ACollectionProperty;
  FAggregatedExpression := AAggregatedExpression;
  FAggregateFunctionType := AAggregateFunctionType;
end;

function TdxAggregateOperand.GetProperty: IdxOperandProperty;
begin
  Result := FProperty;
end;

class function TdxAggregateOperand.GetPropertyByName(const APropertyName: string): IdxOperandProperty;
begin
  if APropertyName = '' then
    Result := nil
  else
    Result := TdxOperandProperty.Create(APropertyName);
end;

function TdxAggregateOperand.Equals(AObject: TObject): Boolean;
var
  AAnother: TdxAggregateOperand;
begin
  AAnother := Safe<TdxAggregateOperand>.Cast(AObject);
  if AAnother = nil then
    Result := False
  else
    Result := (AggregateFunctionType = AAnother.AggregateFunctionType) and Equals(Condition, AAnother.Condition) and
      Equals(CollectionProperty, AAnother.CollectionProperty) and Equals(AggregatedExpression, AAnother.AggregatedExpression);
end;

function TdxAggregateOperand.GetAggregatedExpression: IdxCriteriaOperator;
begin
  Result := FAggregatedExpression;
end;

function TdxAggregateOperand.GetAggregateFunctionType: TdxAggregateFunctionType;
begin
  Result := FAggregateFunctionType;
end;

function TdxAggregateOperand.GetCondition: IdxCriteriaOperator;
begin
  Result := FCondition;
end;

function TdxAggregateOperand.GetHashCode: Integer;
begin
  if Condition = nil then
    Result := -1
  else
    Result := (Condition as TdxCriteriaOperator).GetHashCode;
end;

function TdxAggregateOperand.GetIsTopLevel: Boolean;
begin
  if CollectionProperty = nil then
    Result := True
  else
    Result := CollectionProperty.PropertyName = '';
end;

function TdxAggregateOperand.Exists(const AAggregatedExpression: IdxCriteriaOperator): IdxAggregateOperand;
begin
  Result := TdxAggregateOperand.Create(CollectionProperty, AAggregatedExpression, TdxAggregateFunctionType.Exists, Condition);
end;

function TdxAggregateOperand.Count(const AAggregatedExpression: IdxCriteriaOperator): IdxAggregateOperand;
begin
  Result := TdxAggregateOperand.Create(CollectionProperty, AAggregatedExpression, TdxAggregateFunctionType.Count, Condition);
end;

function TdxAggregateOperand.Accept(const AVisitor: IdxCriteriaOperatorVisitor): IdxCriteriaOperator;
begin
  Result := AVisitor.Visit(Self)
end;


procedure TdxAggregateOperand.Accept(const AVisitor: IdxCriteriaVisitor);
begin
  IdxClientCriteriaVisitor(AVisitor).Visit(Self);
end;

function TdxAggregateOperand.Accept(const AVisitor: IdxClientCriteriaToStringVisitor): TdxCriteriaToStringVisitResult;
begin
  Result := AVisitor.Visit(Self)
end;

function TdxAggregateOperand.Accept(const AVisitor: IdxClientCriteriaVisitor<TdxBooleanCriteriaState>): TdxBooleanCriteriaState;
begin
  Result := AVisitor.Visit(Self);
end;

function TdxAggregateOperand.Avg(const AAggregatedExpression: IdxCriteriaOperator): IdxAggregateOperand;
begin
  Result := TdxAggregateOperand.Create(CollectionProperty, AAggregatedExpression, TdxAggregateFunctionType.Avg, Condition);
end;

function TdxAggregateOperand.Max(const AAggregatedExpression: IdxCriteriaOperator): IdxAggregateOperand;
begin
  Result := TdxAggregateOperand.Create(CollectionProperty, AAggregatedExpression, TdxAggregateFunctionType.Max, Condition);
end;

function TdxAggregateOperand.Min(const AAggregatedExpression: IdxCriteriaOperator): IdxAggregateOperand;
begin
  Result := TdxAggregateOperand.Create(CollectionProperty, AAggregatedExpression, TdxAggregateFunctionType.Min, Condition);
end;

function TdxAggregateOperand.Sum(const AAggregatedExpression: IdxCriteriaOperator): IdxAggregateOperand;
begin
  Result := TdxAggregateOperand.Create(CollectionProperty, AAggregatedExpression, TdxAggregateFunctionType.Sum, Condition);
end;

function TdxAggregateOperand.Single(const AAggregatedExpression: IdxCriteriaOperator): IdxAggregateOperand;
begin
  Result := TdxAggregateOperand.Create(CollectionProperty, AAggregatedExpression, TdxAggregateFunctionType.Single, Condition);
end;


{ TdxContainsOperator }

constructor TdxContainsOperator.Create(const ACollectionProperty: string; const ACondition: IdxCriteriaOperator);
begin
  Create(TdxOperandProperty.Create(ACollectionProperty), ACondition);
end;

constructor TdxContainsOperator.Create;
begin
  Create(nil, nil);
end;

constructor TdxContainsOperator.Create(const ACollectionProperty: IdxOperandProperty; const ACondition: IdxCriteriaOperator);
begin
  inherited Create(ACollectionProperty, nil, TdxAggregateFunctionType.Exists, ACondition);
end;

{ TdxJoinOperand }

constructor TdxJoinOperand.Create(const AJoinTypeName: string; const ACondition: IdxCriteriaOperator);
begin
  Create(AJoinTypeName, ACondition, TdxAggregateFunctionType.Exists, nil);
end;

constructor TdxJoinOperand.Create(const AJoinTypeName: string; const ACondition: IdxCriteriaOperator; AType: TdxAggregateFunctionType;
  const AAggregatedExpression: IdxCriteriaOperator);
begin
  inherited Create;
  FCondition := ACondition;
  FJoinTypeName := AJoinTypeName;
  FAggregatedExpression := AAggregatedExpression;
  FAggregateFunctionType := AType;
end;


function TdxJoinOperand.Equals(AObject: TObject): Boolean;
var
  AAnother: TdxJoinOperand;
begin
  AAnother := Safe<TdxJoinOperand>.Cast(AObject);
  if AAnother = nil then
    Exit(False);
  Result := (Condition as TdxCriteriaOperator).Equals(AAnother.Condition as TdxCriteriaOperator) and (JoinTypeName = AAnother.JoinTypeName) and
    (AggregatedExpression as TdxCriteriaOperator).Equals(AAnother.AggregatedExpression as TdxCriteriaOperator) and (AggregateFunctionType = AAnother.AggregateFunctionType);
end;

function TdxJoinOperand.GetAggregatedExpression: IdxCriteriaOperator;
begin
  Result := FAggregatedExpression;
end;

function TdxJoinOperand.GetCondition: IdxCriteriaOperator;
begin
  Result := FCondition;
end;

function TdxJoinOperand.GetHashCode: Integer;
begin
  if Condition = nil then
    Result := -1
  else
    Result := (Condition as TdxCriteriaOperator).GetHashCode;
  Result := Result xor GetValueHash(JoinTypeName);
end;

function TdxJoinOperand.GetJoinTypeName: string;
begin
  Result := FJoinTypeName;
end;

function TdxJoinOperand.GetAggregateFunctionType: TdxAggregateFunctionType;
begin
  Result := FAggregateFunctionType;
end;


function TdxJoinOperand.Count(const AAggregatedExpression: IdxCriteriaOperator): IdxJoinOperand;
begin
  Result := TdxJoinOperand.Create(JoinTypeName, Condition, TdxAggregateFunctionType.Count, AAggregatedExpression);
end;


function TdxJoinOperand.Accept(const AVisitor: IdxCriteriaOperatorVisitor): IdxCriteriaOperator;
begin
  Result := AVisitor.Visit(Self);
end;

function TdxJoinOperand.Accept(const AVisitor: IdxSQLGeneratorVisitor): string;
begin
  NotImplemented
//  Result := AVisitor.Visit(Self);
end;

function TdxJoinOperand.Accept(const AVisitor: IdxClientCriteriaToStringVisitor): TdxCriteriaToStringVisitResult;
begin
  Result := AVisitor.Visit(Self);
end;

function TdxJoinOperand.Accept(const AVisitor: IdxClientCriteriaVisitor<TdxBooleanCriteriaState>): TdxBooleanCriteriaState;
begin
  Result := AVisitor.Visit(Self);
end;

function TdxJoinOperand.Avg(const AAggregatedExpression: IdxCriteriaOperator): IdxJoinOperand;
begin
  Result := TdxJoinOperand.Create(JoinTypeName, Condition, TdxAggregateFunctionType.Avg, AAggregatedExpression);
end;

function TdxJoinOperand.Max(const AAggregatedExpression: IdxCriteriaOperator): IdxJoinOperand;
begin
  Result := TdxJoinOperand.Create(JoinTypeName, Condition, TdxAggregateFunctionType.Max, AAggregatedExpression);
end;

function TdxJoinOperand.Min(const AAggregatedExpression: IdxCriteriaOperator): IdxJoinOperand;
begin
  Result := TdxJoinOperand.Create(JoinTypeName, Condition, TdxAggregateFunctionType.Min, AAggregatedExpression);
end;

function TdxJoinOperand.Sum(const AAggregatedExpression: IdxCriteriaOperator): IdxJoinOperand;
begin
  Result := TdxJoinOperand.Create(JoinTypeName, Condition, TdxAggregateFunctionType.Sum, AAggregatedExpression);
end;


{ TdxGroupOperator }

constructor TdxGroupOperator.Create(AType: TdxGroupOperatorType = TdxGroupOperatorType.&And);
begin
  inherited Create;
  FOperands := TdxCriteriaOperatorCollection.Create;
  FOperatorType := AType;
end;

constructor TdxGroupOperator.Create(const AOperands: TArray<IdxCriteriaOperator>);
begin
  Create(TdxGroupOperatorType.And);
  TdxCriteriaOperatorCollection(FOperands).AddRange(AOperands);
end;

constructor TdxGroupOperator.Create(AType: TdxGroupOperatorType; const AOperands: array of IdxCriteriaOperator);
begin
  Create(AType);
  TdxCriteriaOperatorCollection(FOperands).AddRange(AOperands);
end;

constructor TdxGroupOperator.Create(AType: TdxGroupOperatorType; const AOperands: TEnumerable<IdxCriteriaOperator>);
begin
  Create(AType);
  TdxCriteriaOperatorCollection(FOperands).AddRange(AOperands);
end;

constructor TdxGroupOperator.Create(AType: TdxGroupOperatorType; const AOperands: TArray<IdxCriteriaOperator>);
begin
  Create(AType);
  TdxCriteriaOperatorCollection(FOperands).AddRange(AOperands);
end;

constructor TdxGroupOperator.Create(const AOperands: array of IdxCriteriaOperator);
begin
  Create(TdxGroupOperatorType.And);
  TdxCriteriaOperatorCollection(FOperands).AddRange(AOperands);
end;

constructor TdxGroupOperator.Create(AType: TdxGroupOperatorType; const AOperands: IEnumerable<IdxCriteriaOperator>);
begin
  Create(AType);
  TdxCriteriaOperatorCollection(FOperands).AddRange(AOperands);
end;


function TdxGroupOperator.Equals(AObject: TObject): Boolean;
var
  I: Integer;
begin
  if AObject = nil then
    Exit(False);
  if ClassType <> AObject.ClassType then
    Exit(False);
  if OperatorType <> TdxGroupOperator(AObject).OperatorType then
    Exit(False);
  if Operands.Count <> (TdxGroupOperator(AObject)).Operands.Count then
    Exit(False);
  for I := 0 to Operands.Count - 1 do
    if not (Operands[I] as TdxCriteriaOperator).Equals(TdxGroupOperator(AObject).Operands[I] as TdxCriteriaOperator) then
      Exit(False);
  Result := True;
end;

function TdxGroupOperator.GetHashCode: Integer;
var
  AHash: Integer;
  AOperand: IdxCriteriaOperator;
begin
  AHash := -1;
  for AOperand in Operands do
    if AOperand <> nil then
      AHash := AHash xor (AOperand as TdxCriteriaOperator).GetHashCode;
  Result := AHash;
end;

function TdxGroupOperator.GetOperands: IdxCriteriaOperatorCollection;
begin
  Result := FOperands;
end;

function TdxGroupOperator.GetOperatorType: TdxGroupOperatorType;
begin
  Result := FOperatorType;
end;

class function TdxGroupOperator.Combine(AOperatorType: TdxGroupOperatorType; const ALeftOperator, ARightOperator: IdxCriteriaOperator): IdxCriteriaOperator;
begin
  if ALeftOperator = nil then
    Exit(ARightOperator);
  if ARightOperator = nil then
    Exit(ALeftOperator);
  Result := Combine(AOperatorType, [ALeftOperator, ARightOperator]);
end;

function TdxGroupOperator.Accept(const AVisitor: IdxCriteriaOperatorVisitor): IdxCriteriaOperator;
begin
  Result := AVisitor.Visit(Self);
end;

function TdxGroupOperator.Accept(const AVisitor: IdxSQLGeneratorVisitor): string;
begin
  Result := AVisitor.Visit(Self);
end;

function TdxGroupOperator.Accept(const AVisitor: IdxClientCriteriaToStringVisitor): TdxCriteriaToStringVisitResult;
begin
  Result := AVisitor.Visit(Self);
end;

function TdxGroupOperator.Accept(const AVisitor: IdxClientCriteriaVisitor<TdxBooleanCriteriaState>): TdxBooleanCriteriaState;
begin
  Result := AVisitor.Visit(Self);
end;

class function TdxGroupOperator.Combine(AOperatorType: TdxGroupOperatorType; const AOperands: array of IdxCriteriaOperator): IdxCriteriaOperator;
var
  AROperands: TArray<IdxCriteriaOperator>;
begin
  case Length(AOperands) of
    0:
      Exit(nil);
    1:
      Exit(AOperands[0]);
  end;
  AROperands := CombineCore(AOperatorType, AOperands);
  case Length(AROperands) of
    0:
      Result := nil;
    1:
      Result := AROperands[0];
    else
      Result := TdxGroupOperator.Create(AOperatorType, AROperands);
  end;
end;

class function TdxGroupOperator.Combine(AOperatorType: TdxGroupOperatorType;
  AOperands: TdxCriteriaOperatorList): IdxCriteriaOperator;
begin
  if (AOperands = nil) or (AOperands.Count = 0) then
    Exit(nil);
  NotImplemented;
end;

class function TdxGroupOperator.CombineCore(AOperatorType: TdxGroupOperatorType;
  const AOperands: array of IdxCriteriaOperator): TArray<IdxCriteriaOperator>;
var
  AResult: TdxCriteriaOperatorList;
  AOperator, AOperator2: IdxCriteriaOperator;
  AGroupOperator: TdxGroupOperator;
begin
  AResult := TdxCriteriaOperatorList.Create;
  try
    for AOperator in AOperands do
    begin
      if AOperator = nil then
        Continue;
      AGroupOperator := Safe<TdxGroupOperator>.Cast(AOperator as TdxCriteriaOperator);
      if (AGroupOperator = nil) or (AGroupOperator.OperatorType <> AOperatorType) then
      begin
        AResult.Add(AOperator);
        Continue;
      end;
      for AOperator2 in AGroupOperator.Operands do
        AResult.Add(AOperator2);
    end;
    Result := AResult.ToArray;
  finally
    AResult.Free;
  end;
end;


{ TdxFunctionOperator }

constructor TdxFunctionOperator.Create;
begin
  inherited Create;
  FOperands := TdxCriteriaOperatorCollection.Create;
end;

constructor TdxFunctionOperator.Create(AType: TdxFunctionOperatorType; AOperands: TEnumerable<IdxCriteriaOperator>);
begin
  Create;
  OperatorType := AType;
  TdxCriteriaOperatorCollection(FOperands).AddRange(AOperands);
end;

constructor TdxFunctionOperator.Create(AType: TdxFunctionOperatorType;
  const AOperands: IEnumerable<IdxCriteriaOperator>);
begin
  Create;
  OperatorType := AType;
  TdxCriteriaOperatorCollection(FOperands).AddRange(AOperands);
end;

constructor TdxFunctionOperator.Create(AType: TdxFunctionOperatorType; const AOperands: TArray<IdxCriteriaOperator>);
begin
  Create;
  OperatorType := AType;
  TdxCriteriaOperatorCollection(FOperands).AddRange(AOperands);
end;

constructor TdxFunctionOperator.Create(AType: TdxFunctionOperatorType; const AOperands: array of IdxCriteriaOperator);
begin
  Create;
  OperatorType := AType;
  TdxCriteriaOperatorCollection(FOperands).AddRange(AOperands);
end;



function TdxFunctionOperator.Accept(const AVisitor: IdxClientCriteriaToStringVisitor): TdxCriteriaToStringVisitResult;
begin
  Result := AVisitor.Visit(Self);
end;

function TdxFunctionOperator.Accept(const AVisitor: IdxCriteriaVisitorListString): TList<string>;
begin
  Result := AVisitor.Visit(Self);
end;

function TdxFunctionOperator.Accept(const AVisitor: IdxClientCriteriaVisitor<TdxBooleanCriteriaState>): TdxBooleanCriteriaState;
begin
  Result := AVisitor.Visit(Self);
end;

function TdxFunctionOperator.Accept(const AVisitor: IdxSQLGeneratorVisitor): string;
begin
  Result := AVisitor.Visit(Self);
end;

function TdxFunctionOperator.Accept(const AVisitor: IdxCriteriaOperatorVisitor): IdxCriteriaOperator;
begin
  Result := AVisitor.Visit(Self);
end;

function TdxFunctionOperator.Equals(AObject: TObject): Boolean;
var
  I: Integer;
begin
  if AObject = nil then
    Exit(False);
  if ClassType <> AObject.ClassType then
    Exit(False);
  if OperatorType <> TdxFunctionOperator(AObject).OperatorType then
    Exit(False);
  if Operands.Count <> (TdxFunctionOperator(AObject)).Operands.Count then
    Exit(False);
  for I := 0 to Operands.Count - 1 do
    if not (Operands[I] as TdxCriteriaOperator).Equals(TdxFunctionOperator(AObject).Operands[I] as TdxCriteriaOperator) then
      Exit(False);
  Result := True;
end;

function TdxFunctionOperator.GetHashCode: Integer;
var
  AHash: Integer;
  AOperand: IdxCriteriaOperator;
begin
  AHash := -1;
  for AOperand in Operands do
    if AOperand <> nil then
      AHash := AHash xor (AOperand as TdxCriteriaOperator).GetHashCode;
  Result := AHash;
end;

function TdxFunctionOperator.GetOperands: IdxCriteriaOperatorCollection;
begin
  Result := FOperands;
end;

function TdxFunctionOperator.GetOperatorType: TdxFunctionOperatorType;
begin
  Result := FOperatorType;
end;

class function TdxFunctionOperator.GuessIsLogicalCustomFunction(const AOperator: IdxFunctionOperator): Boolean;
begin
  Result := False;
end;


{ TdxSortByExpression }

constructor TdxSortByExpression.Create;
begin
  Create(nil, TdxSortDirection.Ascending);
end;

constructor TdxSortByExpression.Create(const APropertyName: string; ASortDirection: TdxSortDirection);
begin
  Create(TdxCriteriaOperator.Parse(APropertyName), ASortDirection);
end;

constructor TdxSortByExpression.Create(const AProperty: IdxCriteriaOperator; ASortDirection: TdxSortDirection);
begin
  inherited Create;
  FProperty := AProperty;
  FSortDirection := ASortDirection;
end;


function TdxSortByExpression.GetSortDirection: TdxSortDirection;
begin
  Result := FSortDirection;
end;

function TdxSortByExpression.GetExpression: IdxExpression;
begin
  Result := FProperty;
end;

function TdxSortByExpression.GetPropertyName: string;
begin
  if FProperty = nil then
    Result := ''
  else
    Result := (FProperty as TdxCriteriaOperator).ToString;
end;

procedure TdxSortByExpression.SetPropertyName(const AValue: string);
begin
  if AValue = '' then
    FProperty := nil
  else
    FProperty := TdxCriteriaOperator.Parse(AValue);
end;

{ TdxSortByExpressions.TEnumerator }

constructor TdxSortByExpressions.TEnumerator.Create(ASortByExpressions: TdxSortByExpressions);
begin
  inherited Create;
  FSortByExpressions := ASortByExpressions;
  FIndex := -1;
end;

function TdxSortByExpressions.TEnumerator.GetCurrent: TObject;
begin
  Result := GetCurrentGeneric as TObject;
end;

function TdxSortByExpressions.TEnumerator.GetCurrentGeneric: IdxSortByExpression;
begin
  if FIndex >= 0 then
    Result := FSortByExpressions[FIndex]
  else
    Result := nil;
end;

function TdxSortByExpressions.TEnumerator.MoveNext: Boolean;
begin
  Result := FIndex < FSortByExpressions.Count - 1;
  if Result then
    Inc(FIndex);
end;

procedure TdxSortByExpressions.TEnumerator.Reset;
begin
  FIndex := -1;
end;

{ TdxSortByExpressions }

constructor TdxSortByExpressions.Create(const ASortProperties: TArray<IdxSortByExpression>);
begin
  Create;
  if ASortProperties <> nil then
    AddRange(ASortProperties);
end;

constructor TdxSortByExpressions.Create(const ASortProperties: array of IdxSortByExpression);
var
  ASortByExpression: IdxSortByExpression;
begin
  Create;
  for ASortByExpression in ASortProperties do
    Add(ASortByExpression);
end;

constructor TdxSortByExpressions.Create;
begin
  inherited Create;
  FList := TList<IdxSortByExpression>.Create;
end;

destructor TdxSortByExpressions.Destroy;
begin
  FreeAndNil(FList);
  inherited Destroy;
end;

procedure TdxSortByExpressions.AddRange(const ASortProperties: TArray<IdxSortByExpression>);
var
  ASp: IdxSortByExpression;
begin
  for ASp in ASortProperties do
    if ASp <> nil then
      FList.Add(ASp);
  DoChanged;
end;

procedure TdxSortByExpressions.Add(const ASortProperties: IdxSortByExpressions);
var
  ASp: IdxSortByExpression;
begin
  for ASp in ASortProperties do
    FList.Add(ASp);
  DoChanged;
end;

function TdxSortByExpressions.Add(const AValue: IdxSortByExpression): Integer;
var
  APos: Integer;
begin
  APos := FList.Add(AValue);
  DoChanged;
  Result := APos;
end;

procedure TdxSortByExpressions.DoChanged;
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

procedure TdxSortByExpressions.Clear;
begin
  FList.Clear;
  DoChanged;
end;

function TdxSortByExpressions.Contains(const AValue: IdxSortByExpression): Boolean;
begin
  Result := FList.Contains(AValue);
end;

function TdxSortByExpressions.IndexOf(const AValue: IdxSortByExpression): Integer;
begin
  Result := FList.IndexOf(AValue);
end;

procedure TdxSortByExpressions.Insert(AIndex: Integer; const AValue: IdxSortByExpression);
begin
  FList.Insert(AIndex, AValue);
  DoChanged;
end;

procedure TdxSortByExpressions.Remove(const AValue: IdxSortByExpression);
begin
  FList.Remove(AValue);
  DoChanged;
end;

procedure TdxSortByExpressions.RemoveAt(AIndex: Integer);
begin
  FList.Delete(AIndex);
  DoChanged;
end;

function TdxSortByExpressions.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TdxSortByExpressions.GetEnumeratorGeneric: IEnumerator<IdxSortByExpression>;
begin
  Result := TEnumerator.Create(Self);
end;

function TdxSortByExpressions.GetItems(AIndex: Integer): IdxSortByExpression;
begin
  Result := FList[AIndex];
end;

function TdxSortByExpressions.GetEnumerator: IEnumerator;
begin
  Result := GetEnumeratorGeneric;
end;

{ TdxLikeCustomFunction }


class function TdxLikeCustomFunction.Create(const AValue, APattern: IdxCriteriaOperator): TdxFunctionOperator;
begin
  Result := NotImplemented;
end;

class function TdxLikeCustomFunction.IsName(const AName: string): Boolean;
begin
  Result := SameText(AName, Name);
end;

class function TdxLikeCustomFunction.Convert(const ALike: IdxFunctionOperator): IdxBinaryOperator;
begin
  if not IsBinaryCompatibleLikeFunction(ALike) then
    raise EArgumentException.Create('!IsBinaryCompatibleLikeFunction(like)');

  Result := nil;
  NotImplemented;
end;

class function TdxLikeCustomFunction.Convert(const ALike: IdxBinaryOperator): IdxFunctionOperator;
begin

  Result := Create(ALike.LeftOperand, ALike.RightOperand);
end;

class function TdxLikeCustomFunction.IsBinaryCompatibleLikeFunction(const AFunc: IdxFunctionOperator): Boolean;
var
  ANmv: TdxOperandValue;
  ANm: string;
begin
  if AFunc = nil then
    Exit(False);
  if AFunc.OperatorType <> TdxFunctionOperatorType.Custom then
    Exit(False);
  if AFunc.Operands.Count <> 3 then
    Exit(False);
  ANmv := Safe<TdxOperandValue>.Cast(AFunc.Operands[0] as TdxCriteriaOperator);
  if ANmv = nil then
    Exit(False);
  if ANmv.Value.IsString then
    ANm := ANmv.Value.AsString
  else
    ANm := '';
  Result := IsName(ANm);
end;


{ TdxQueryOperand }

constructor TdxQueryOperand.Create(const AColumnName: string; const ANodeAlias: string; AColumnType: TdxDBColumnType;
  const AParentCriteria: IdxCriteriaOperator = nil);
begin
  inherited Create;
  FColumnName := AColumnName;
  FNodeAlias := ANodeAlias;
  FColumnType := AColumnType;
  FParentCriteria := AParentCriteria;
end;

constructor TdxQueryOperand.Create(const AColumnName, ANodeAlias: string);
begin
  Create(AColumnName, ANodeAlias, TdxDBColumnType.Unknown);
end;

constructor TdxQueryOperand.Create;
begin
  Create('', '', TdxDBColumnType.Unknown);
end;


function TdxQueryOperand.Equals(AObject: TObject): Boolean;
var
  AAnother: TdxQueryOperand;
begin
  AAnother := Safe<TdxQueryOperand>.Cast(AObject);
  if AAnother = nil then
    Exit(False);
  Result := (ColumnName = AAnother.ColumnName) and (NodeAlias = AAnother.NodeAlias);
end;

function TdxQueryOperand.GetColumnName: string;
begin
  Result := FColumnName;
end;

function TdxQueryOperand.GetColumnType: TdxDBColumnType;
begin
  Result := FColumnType;
end;

function TdxQueryOperand.GetHashCode: Integer;
begin
  if ColumnName <> '' then
    Result := GetValueHash(ColumnName)
  else
    Result := -1;
end;

function TdxQueryOperand.GetNodeAlias: string;
begin
  Result := FNodeAlias;
end;

function TdxQueryOperand.GetParentCriteria: IdxCriteriaOperator;
begin
  Result := FParentCriteria;
end;


function TdxQueryOperand.Accept(const AVisitor: IdxSQLGeneratorVisitor): string;
begin
  Result := AVisitor.Visit(Self);
end;

function TdxQueryOperand.Accept(const AVisitor: IdxCriteriaOperatorVisitor): IdxCriteriaOperator;
var
  AQueryVisitor: IdxQueryCriteriaOperatorVisitor;
begin
  AQueryVisitor := AVisitor as IdxQueryCriteriaOperatorVisitor;
  Result := AQueryVisitor.Visit(Self);
end;

function TdxQueryOperand.Accept(const AVisitor: IdxClientCriteriaToStringVisitor): TdxCriteriaToStringVisitResult;
begin
  Result := (AVisitor as IQueryCriteriaToStringCriteriaVisitor).Visit(Self);
end;

function TdxQueryOperand.Accept(const AVisitor: IdxCriteriaVisitorListString): TList<string>;
begin
  Result := (AVisitor as IQueryCriteriaToStringListVisitor).Visit(Self);
end;

function TdxQueryOperand.Clone: TdxQueryOperand;
begin
  Result := TdxQueryOperand.Create(FColumnName, FNodeAlias, FColumnType);
end;

{ TdxObjectGeneratorCriteriaSet }

constructor TdxObjectGeneratorCriteriaSet.Create;
begin
  inherited Create;
  FCriteriaDictionary := TDictionary<string, IdxCriteriaOperator>.Create;
end;

destructor TdxObjectGeneratorCriteriaSet.Destroy;
begin
  FreeAndNil(FCriteriaDictionary);
  inherited Destroy;
end;

function TdxObjectGeneratorCriteriaSet.TryGetCriteria(const ATableName: string; out ACriteria: IdxCriteriaOperator): Boolean;
begin
  Result := FCriteriaDictionary.TryGetValue(ATableName, ACriteria);
end;

function TdxObjectGeneratorCriteriaSet.GetCompleteCriteria(const ATableName: string): IdxCriteriaOperator;
var
  ACriteria: IdxCriteriaOperator;
begin
  if not FCriteriaDictionary.TryGetValue(ATableName, ACriteria) then
    Exit(FCommonCriteria);
  if FCommonCriteria = nil then
    Result := ACriteria
  else
    Result := TdxGroupOperator.And(FCommonCriteria, ACriteria);
end;

procedure TdxObjectGeneratorCriteriaSet.UpdateCriteria(const ATableName: string; const ACriteria: IdxCriteriaOperator);
var
  AGroupCriteria: IdxCriteriaOperator;
  AGroup: TdxGroupOperator;
begin
  if ACriteria = nil then
    Exit;
  if FCriteriaDictionary.TryGetValue(ATableName, AGroupCriteria) then
  begin
    AGroup := Safe<TdxGroupOperator>.Cast(AGroupCriteria as TdxCriteriaOperator);
    if (AGroup <> nil) and (AGroup.OperatorType = TdxGroupOperatorType.And) then
      AGroup.Operands.Add(ACriteria)
    else
      FCriteriaDictionary.AddOrSetValue(ATableName, TdxGroupOperator.And(AGroupCriteria, ACriteria));
  end
  else
    FCriteriaDictionary.Add(ATableName, ACriteria);
end;

procedure TdxObjectGeneratorCriteriaSet.UpdateCommonCriteria(const ACriteria: IdxCriteriaOperator);
var
  AGroup: TdxGroupOperator;
begin
  if ACriteria = nil then
    Exit;
  if FCommonCriteria = nil then
    FCommonCriteria := ACriteria
  else
  begin
    AGroup := Safe<TdxGroupOperator>.Cast(FCommonCriteria as TdxCriteriaOperator);
    if (AGroup <> nil) and (AGroup.OperatorType = TdxGroupOperatorType.And) then
      AGroup.Operands.Add(ACriteria)
    else
      FCommonCriteria := TdxGroupOperator.And(FCommonCriteria, ACriteria);
  end;
end;

class function TdxObjectGeneratorCriteriaSet.GetCommonCriteriaSet(const ACommonCriteria: IdxCriteriaOperator): TdxObjectGeneratorCriteriaSet;
begin
  Result := TdxObjectGeneratorCriteriaSet.Create;
  Result.UpdateCommonCriteria(ACommonCriteria);
end;

class function TdxObjectGeneratorCriteriaSet.GetCriteriaSet(const ATableName: string;
  const ACriteria: IdxCriteriaOperator): TdxObjectGeneratorCriteriaSet;
begin
  Result := TdxObjectGeneratorCriteriaSet.Create;
  Result.UpdateCriteria(ATableName, ACriteria);
end;

class function TdxObjectGeneratorCriteriaSet.GetCriteriaSet(const ATableName: string;
  const ACriteria, ACommonCriteria: IdxCriteriaOperator): TdxObjectGeneratorCriteriaSet;
begin
  Result := TdxObjectGeneratorCriteriaSet.Create;
  Result.UpdateCommonCriteria(ACommonCriteria);
  Result.UpdateCriteria(ATableName, ACriteria);
end;

{ TdxAccept }

class constructor TdxAcceptor.Create;
begin
  FCriteria := TDictionary<TClass, TCriteria>.Create;
  FCriteria.Add(TdxBetweenOperator, TCriteria.TBetweenOperator);
  FCriteria.Add(TdxBinaryOperator, TCriteria.TBinaryOperator);
  FCriteria.Add(TdxUnaryOperator, TCriteria.TUnaryOperator);
  FCriteria.Add(TdxInOperator, TCriteria.TInOperator);
  FCriteria.Add(TdxGroupOperator, TCriteria.TGroupOperator);
  FCriteria.Add(TdxOperandValue, TCriteria.TOperandValue);
  FCriteria.Add(TdxConstantValue, TCriteria.TOperandValue);
  FCriteria.Add(TdxFunctionOperator, TCriteria.TFunctionOperator);

  FCriteria.Add(TdxOperandProperty, TCriteria.TOperandProperty);
  FCriteria.Add(TdxAggregateOperand, TCriteria.TAggregateOperand);
  FCriteria.Add(TdxJoinOperand, TCriteria.TJoinOperand);
end;

class destructor TdxAcceptor.Destroy;
begin
  FreeAndNil(FCriteria);
end;

class function TdxAcceptor.GetCriteriaType(const ACriteriaOperator: IdxCriteriaOperator): TCriteria;
begin
  Result := FCriteria[(ACriteriaOperator as TObject).ClassType];
end;

class function TdxAcceptor.Accept<T>(const ACriteriaOperator: IdxCriteriaOperator; const AVisitor: IdxClientCriteriaVisitor<T>): T;
begin
  case GetCriteriaType(ACriteriaOperator) of
    TCriteria.TBetweenOperator:
      Result := AVisitor.Visit(TdxBetweenOperator(ACriteriaOperator));
    TCriteria.TBinaryOperator:
      Result := AVisitor.Visit(TdxBinaryOperator(ACriteriaOperator));
    TCriteria.TUnaryOperator:
      Result := AVisitor.Visit(TdxUnaryOperator(ACriteriaOperator));
    TCriteria.TInOperator:
      Result := AVisitor.Visit(TdxInOperator(ACriteriaOperator));
    TCriteria.TGroupOperator:
      Result := AVisitor.Visit(TdxGroupOperator(ACriteriaOperator));
    TCriteria.TOperandValue:
      Result := AVisitor.Visit(TdxOperandValue(ACriteriaOperator));
    TCriteria.TFunctionOperator:
      Result := AVisitor.Visit(TdxFunctionOperator(ACriteriaOperator));

    TCriteria.TOperandProperty:
      Result := AVisitor.Visit(TdxOperandProperty(ACriteriaOperator));
    TCriteria.TAggregateOperand:
      Result := AVisitor.Visit(TdxAggregateOperand(ACriteriaOperator));
    TCriteria.TJoinOperand:
      Result := AVisitor.Visit(TdxJoinOperand(ACriteriaOperator));
    else
      raise EArgumentException.Create('');
  end;
end;

end.
