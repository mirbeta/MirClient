{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressFlowChart                                         }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSFLOWCHART AND ALL ACCOMPANYING }
{   VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY.              }
{                                                                    }
{   THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED       }
{   FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE         }
{   COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE        }
{   AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT   }
{   AND PERMISSION FROM DEVELOPER EXPRESS INC.                       }
{                                                                    }
{   CONSULT THE end USER LICENSE AGREEMENT FOR INFORMATION ON        }
{   ADDITIONAL RESTRICTIONS.                                         }
{                                                                    }
{********************************************************************}

unit dxEvaluator;

interface

uses
  Windows, SysUtils, Classes, Math, Generics.Collections;

const
  // Token Types
  dxTokenUndefined  = 0;
  dxTokenSpace      = 1;
  dxTokenDelimiter  = 2;
  dxTokenQuot       = 3;
  dxTokenQuotedText = 4;
  dxTokenIdent      = 5;

  dxTokenMax        = dxTokenIdent;

  dxExprTokenFunction   = dxTokenMax + 1;
  dxExprTokenOperator   = dxExprTokenFunction + 1;
  dxExprTokenConstantFloat = dxExprTokenOperator + 1;
  dxExprTokenConstantInt   = dxExprTokenConstantFloat + 1;

  dxExprTokenMax = dxExprTokenConstantInt;

const
  sErrorCursorInfo = 'Token: "%s", Scan Cursor: "%s"';
  sErrorFewResults = 'Syntax Error: too many expressions in the string';
  sErrorFunctionAlreadyRegistered = 'The "%s" function with %d arguments already registered';
  sErrorFunctionNotFound = 'The "%s" function was not found';
  sErrorNotCompiled = 'Expression not compiled';
  sErrorOperatorArguments = 'Operator must have 1 or 2 arguments';
  sErrorStackIsEmpty = 'Stack is empty';
  sErrorTooManyArguments = 'Syntax error: function "%s" has too many arguments';
  sErrorTooSmallArguments = 'Syntax error: function "%s" has too small arguments';
  sErrorUnequalBrackets = 'Syntax error: Unequal brackets';
  sErrorUnexpectedToken = 'Syntax Error: Unexpected token';
  sErrorVariableNotFound = 'The "%s" variable was not found';
  sErrorVariableAlreadyRegistered = 'The "%s" variable already registered';

type
  EdxExpression = class(Exception);
  EdxExpressionCompiler = class(Exception);
  TdxExpressionElements = class;

  TdxCustomExpression = class;
  TdxCustomExpressionCompiler = class;

  { TdxParserToken }

  TdxParserToken = record
    Context: Pointer;
    Data: PChar;
    DataLength: Integer;
    TokenType: Integer;
    function Compare(const S: string; IgnoreCase: Boolean = True): Boolean;
    function ToString: string;
    procedure Reset;
  end;

  { TdxParser }

  TdxParser = class
  protected
    FDelimiters: PChar;
    FDelimitersLength: Integer;
    FQuotes: PChar;
    FQuotesLength: Integer;
    FSpaces: PChar;
    FSpacesLength: Integer;

    FScan: PChar;
    FScanBuffer: string;
    FScanCount: Integer;
    FQuotedTextAsSingleToken: Boolean;
    FQuotedTextAsSingleTokenUnquot: Boolean;

    FSkipDelimiters: Boolean;
    FSkipQuotes: Boolean;
    FSkipSpaces: Boolean;

    function Contains(const W: WideChar; L: PChar; C: Integer): LongBool; inline;
    function FetchToken(var P: PChar; var C: Integer; var AToken: TdxParserToken): Boolean; virtual;
    function MoveToNext(var P: PChar; var C: Integer): Boolean; inline;
    function ShouldSkipToken(const AToken: TdxParserToken): Boolean; virtual;
  public
    constructor Create(const ADelimiters, AQuotes, ASpaces: string);
    destructor Destroy; override;
    procedure Initialize(const P: PChar; C: Integer); overload;
    procedure Initialize(const S: string); overload;
    // Tokens
    function GetToken(out AToken: TdxParserToken): Boolean; overload;
    function GetToken(out AToken: TdxParserToken; const ADelimiters: string): Boolean; overload;
    function GetToken(out AToken: TdxParserToken; const ADelimiters, AQuotes, ASpaces: string): Boolean; overload;
    function MoveToNextSymbol: Boolean; inline;
    // Buffer
    property Scan: PChar read FScan;
    property ScanCount: Integer read FScanCount;
    // Parser Options
    property QuotedTextAsSingleToken: Boolean read FQuotedTextAsSingleToken write FQuotedTextAsSingleToken;
    property QuotedTextAsSingleTokenUnquot: Boolean read FQuotedTextAsSingleTokenUnquot write FQuotedTextAsSingleTokenUnquot;
    property SkipDelimiters: Boolean read FSkipDelimiters write FSkipDelimiters;
    property SkipQuotes: Boolean read FSkipQuotes write FSkipQuotes;
    property SkipSpaces: Boolean read FSkipSpaces write FSkipSpaces;
  end;

  { TdxExpressionFastStack }

  TdxExpressionFastStack<T: class> = class(TObject)
  strict private
    FBuffer: array of T;
    FCapacity: Integer;
    FCount: Integer;
    FOwnObjects: Boolean;

    procedure SetCapacity(ACapacity: Integer);
  public
    constructor Create(AOwnObjects: Boolean); virtual;
    destructor Destroy; override;
    procedure FreeObjects;
    function Peek: T;
    function Pop: T;
    function Push(AObject: T): Integer;
    //
    property Count: Integer read FCount;
  end;

  { TdxExpressionEvalFunction }

  TdxExpressionEvalProc = function (AParams: TdxExpressionElements): Variant of object;

  TdxExpressionEvalFunction = class
  strict private
    FCategory: Byte;
    FDependsFromParametersOnly: Boolean;
    FName: string;
    FParamsCount: Integer;
    FProc: TdxExpressionEvalProc;
  public
    constructor Create(const AName: string; AParamsCount: Integer;
      ADependsFromParametersOnly: Boolean; AProc: TdxExpressionEvalProc; ACategory: Byte);
    function ToString: string; override;
    //
    property Category: Byte read FCategory;
    property DependsFromParametersOnly: Boolean read FDependsFromParametersOnly;
    property Name: string read FName;
    property ParamsCount: Integer read FParamsCount; // -1 = Variable Parameters Count
    property Proc: TdxExpressionEvalProc read FProc;
  end;

  { TdxExpressionEvalFunctionList }

  TdxExpressionEvalFunctionList = class(TObjectList<TdxExpressionEvalFunction>)
  strict private
    FRemap: TDictionary<string, TdxExpressionEvalFunction>;

    function Compare(const S: string; B: PChar; L: Integer): Boolean; inline;
  public
    constructor Create;
    destructor Destroy; override;
    //
    function Find(const AName: PChar; ANameLength, AParamsCount: Integer; out AFunction: TdxExpressionEvalFunction): Boolean; overload; virtual;
    function Find(const AName: PChar; ANameLength: Integer; out AFunction: TdxExpressionEvalFunction): Boolean; overload; virtual;
    function Find(const AName: string; AParamsCount: Integer; out AFunction: TdxExpressionEvalFunction): Boolean; overload; inline;
    function Find(const AName: string; out AFunction: TdxExpressionEvalFunction): Boolean; overload; inline;
    //
    property Remap: TDictionary<string, TdxExpressionEvalFunction> read FRemap;
  end;

  { TdxExpressionEvalOperator }

  TdxExpressionEvalOperatorAssociativity = (eoaLeftToRight, eoaRightToLeft);

  TdxExpressionEvalOperator = class(TdxExpressionEvalFunction)
  strict private
    FAssociativity: TdxExpressionEvalOperatorAssociativity;
    FPriority: Integer;
  public
    constructor Create(const AName: string; AProc: TdxExpressionEvalProc;
      APriority, AParamsCount: Integer; AAssociativity: TdxExpressionEvalOperatorAssociativity = eoaLeftToRight);
    //
    property Associativity: TdxExpressionEvalOperatorAssociativity read FAssociativity;
    property Priority: Integer read FPriority;
  end;

  { TdxExpressionEvalVariable }

  TdxExpressionEvalVariable = class(TdxExpressionEvalFunction)
  strict private
    function EvalProc(AParams: TdxExpressionElements): Variant;
  protected
    FValue: PVariant;
  public
    constructor Create(const AName: string; const AValue: PVariant);
  end;

  { TdxExpressionElement }

  TdxExpressionElement = class abstract
  protected
    function GetIsConstant: Boolean; virtual; abstract;
  public
    procedure Optimize; virtual; abstract;
    function Evaluate: Variant; virtual; abstract;
    function ToString(AExpression: TdxCustomExpression): string; reintroduce; virtual; abstract;
    //
    property IsConstant: Boolean read GetIsConstant;
  end;

  { TdxExpressionElements }

  TdxExpressionElements = class
  strict private
    function GetCount: Integer; inline;
    function GetItem(Index: Integer): TdxExpressionElement; inline;
  protected
    FList: TList;

    procedure Add(AElement: TdxExpressionElement);
    procedure AddFromStack(AStack: TdxExpressionFastStack<TdxExpressionElement>; ACount: Integer);
    procedure Clear;
    procedure Optimize;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function IsConstant: Boolean;
    function ToString(AExpression: TdxCustomExpression): string; reintroduce;
    //
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TdxExpressionElement read GetItem; default;
  end;

  { TdxExpressionElementConstant }

  TdxExpressionElementConstant = class(TdxExpressionElement)
  strict private
    FValue: Variant;
  protected
    function GetIsConstant: Boolean; override;
  public
    constructor Create(const AValue: Variant); virtual;
    procedure Optimize; override;
    function Evaluate: Variant; override;
    function ToString(AExpression: TdxCustomExpression): string; override;
  end;

  { TdxExpressionElementFunction }

  TdxExpressionElementFunctionClass = class of TdxExpressionElementFunction;
  TdxExpressionElementFunction = class(TdxExpressionElement)
  strict private
    FEvalFunc: TdxExpressionEvalFunction;
    FParams: TdxExpressionElements;

    function GetName: string;
  protected
    function GetIsConstant: Boolean; override;
  public
    constructor Create(AEvalFunc: TdxExpressionEvalFunction); virtual;
    destructor Destroy; override;
    procedure Optimize; override;
    function Evaluate: Variant; override;
    function ToString(AExpression: TdxCustomExpression): string; override;
    //
    property EvalFunc: TdxExpressionEvalFunction read FEvalFunc write FEvalFunc;
    property Name: string read GetName;
    property Params: TdxExpressionElements read FParams;
  end;

  { TdxExpressionElementOperator }

  TdxExpressionElementOperator = class(TdxExpressionElementFunction)
  public
    function ToString(AExpression: TdxCustomExpression): string; override;
  end;

  { TdxCustomExpression }

  TdxCustomExpression = class
  public const
    CategoryGeneral = 0;
    DefaultSpaceChars = ' '#13#10#9#0;
    DefaultIdentDelimiters = '%=:+-\/*;,|(){}<>[].@#$^&?!"“”«»'#39 + DefaultSpaceChars;
    DefaultDelimiterChars = DefaultIdentDelimiters + '_';
    DefaultQuoteChars = '"'#39;
  strict private
    FDelimiterChars: string;
    FQuoteChars: string;
    FSpaceChars: string;
    FCache: TStringList;
    FKnownFunctions: TdxExpressionEvalFunctionList;
    FKnownOperators: TdxExpressionEvalFunctionList;

    function GetCacheSize: Byte;
    procedure SetCacheSize(const AValue: Byte);
  protected
    FCompiled: TdxExpressionElement;

    function CreateCompiler: TdxCustomExpressionCompiler; virtual;
    function CreateKnownFunctions: TdxExpressionEvalFunctionList; virtual;
    function CreateKnownOperators: TdxExpressionEvalFunctionList; virtual;
    function InternalCompile(const S: string): TdxExpressionElement;
    //
    property KnownFunctions: TdxExpressionEvalFunctionList read FKnownFunctions;
    property KnownOperators: TdxExpressionEvalFunctionList read FKnownOperators;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Compile(const S: string); virtual;
    procedure Optimize;
    function Evaluate: Variant; overload; virtual;
    function Evaluate(const S: string): Variant; overload;
    function ToString: string; override;
    //
    procedure RegisterFunction(const AName: string; AProc: TdxExpressionEvalProc; ACategory: Byte = CategoryGeneral); overload;
    procedure RegisterFunction(const AName: string; AProc: TdxExpressionEvalProc;
      AParamsCount: Integer; ADependsFromParametersOnly: Boolean; ACategory: Byte = CategoryGeneral); overload;
    procedure RegisterFunctionRemap(const AOldName, ANewName: string);
    procedure RegisterOperator(const AName: string;
      AProc: TdxExpressionEvalProc; AParamsCount, APriority: Integer);
    procedure RegisterVariable(const AName: string; const AValue: PVariant);
    procedure UnregisterFunctionOrVariable(const AName: string);
    //
    property CacheSize: Byte read GetCacheSize write SetCacheSize;
    property DelimiterChars: string read FDelimiterChars write FDelimiterChars;
    property QuoteChars: string read FQuoteChars write FQuoteChars;
    property SpaceChars: string read FSpaceChars write FSpaceChars;
  end;

  { TdxCustomExpressionCompiler }

  TdxExpressionCompilerSolidTokenType = (ecsttNone, ecsttOperand, ecsttOperator);

  TdxCustomExpressionCompiler = class(TdxParser)
  strict private
    FOperatorStack: TdxExpressionFastStack<TdxExpressionEvalOperator>;
    FOutputBuffer: TdxExpressionFastStack<TdxExpressionElement>;
    FOwner: TdxCustomExpression;

    procedure ParseParametersList(AFunctionElement: TdxExpressionElementFunction);
  protected
    ClassFunction: TdxExpressionElementFunctionClass;
    ClassOperator: TdxExpressionElementFunctionClass;
    PrevSolidToken: TdxExpressionCompilerSolidTokenType;
    Token: TdxParserToken;

    procedure Error(const AMessage: string); overload;
    procedure Error(const AMessage: string; const AArguments: array of const); overload;
    // Compiler
    function CompileCore: TdxExpressionElement; virtual;
    function ProcessToken: Boolean; inline;
    function ProcessTokenAsDelimiter: Boolean; virtual;
    function ProcessTokenAsFunction: Boolean; inline;
    function ProcessTokenAsOperator: Boolean; inline;
    // Internal
    procedure OutputOperator(AOperator: TdxExpressionEvalOperator);
    //
    property OperatorStack: TdxExpressionFastStack<TdxExpressionEvalOperator> read FOperatorStack;
    property OutputBuffer: TdxExpressionFastStack<TdxExpressionElement> read FOutputBuffer;
    property Owner: TdxCustomExpression read FOwner;
  public
    constructor Create(AOwner: TdxCustomExpression); reintroduce; virtual;
    function Compile: TdxExpressionElement; virtual;
  end;

  { TdxMathExpression }

  TdxMathExpression = class(TdxCustomExpression)
  strict private
    // Built-in Functions
    function FuncAbs(AParams: TdxExpressionElements): Variant;
    function FuncCos(AParams: TdxExpressionElements): Variant;
    function FuncExp(AParams: TdxExpressionElements): Variant;
    function FuncIF(AParams: TdxExpressionElements): Variant;
    function FuncLn(AParams: TdxExpressionElements): Variant;
    function FuncLog10(AParams: TdxExpressionElements): Variant;
    function FuncLogN(AParams: TdxExpressionElements): Variant;
    function FuncMax(AParams: TdxExpressionElements): Variant;
    function FuncMin(AParams: TdxExpressionElements): Variant;
    function FuncPower(AParams: TdxExpressionElements): Variant;
    function FuncRandom(AParams: TdxExpressionElements): Variant;
    function FuncRound(AParams: TdxExpressionElements): Variant;
    function FuncSin(AParams: TdxExpressionElements): Variant;
    function FuncTrunc(AParams: TdxExpressionElements): Variant;

    // Built-in Operators
    function OperatorAnd(AParams: TdxExpressionElements): Variant;
    function OperatorDivide(AParams: TdxExpressionElements): Variant;
    function OperatorDivideInt(AParams: TdxExpressionElements): Variant;
    function OperatorEqual(AParams: TdxExpressionElements): Variant;
    function OperatorGreater(AParams: TdxExpressionElements): Variant;
    function OperatorGreaterOrEqual(AParams: TdxExpressionElements): Variant;
    function OperatorLower(AParams: TdxExpressionElements): Variant;
    function OperatorLowerOrEqual(AParams: TdxExpressionElements): Variant;
    function OperatorMinus(AParams: TdxExpressionElements): Variant;
    function OperatorMod(AParams: TdxExpressionElements): Variant;
    function OperatorMultiply(AParams: TdxExpressionElements): Variant;
    function OperatorNot(AParams: TdxExpressionElements): Variant;
    function OperatorNotEqual(AParams: TdxExpressionElements): Variant;
    function OperatorOr(AParams: TdxExpressionElements): Variant;
    function OperatorPlus(AParams: TdxExpressionElements): Variant;
    function OperatorPower(AParams: TdxExpressionElements): Variant;
    function OperatorXor(AParams: TdxExpressionElements): Variant;
  protected
    function CreateCompiler: TdxCustomExpressionCompiler; override;
  public
    constructor Create; override;
  end;

  { TdxMathExpressionCompiler }

  TdxMathExpressionCompiler = class(TdxCustomExpressionCompiler)
  private
    function GetOwner: TdxMathExpression; inline;
    function IsNumeric(const AToken: TdxParserToken): Boolean; inline;
  protected
    function FetchToken(var P: PChar; var C: Integer; var AToken: TdxParserToken): Boolean; override;
    //
    property Owner: TdxMathExpression read GetOwner;
  end;

function dxCompareTokens(B1, B2: PChar; L1, L2: Integer): Boolean; overload;
function dxCompareTokens(const S1, S2: string): Boolean; overload;
function dxAllocStr(const S: string): PChar; overload;
function dxAllocStr(const S: string; out ALength: Integer): PChar; overload;
function dxCompareStrings(const S1, S2: string; AIgnoreCase: Boolean = True): Integer; overload;
function dxCompareStrings(P1, P2: PChar; L1, L2: Integer; AIgnoreCase: Boolean = True): Integer; overload;

implementation

uses
  dxCore, Variants, dxStringHelper;

function dxCompareTokens(const S1, S2: string): Boolean; overload;
begin
  Result := dxCompareTokens(PChar(S1), PChar(S2), Length(S1), Length(S2));
end;

function dxCompareTokens(B1, B2: PChar; L1, L2: Integer): Boolean; overload;
var
  C1, C2: Word;
begin
  Result := L1 = L2;
  if Result then
    while L1 > 0 do
    begin
      C1 := Ord(B1^);
      C2 := Ord(B2^);
      if C1 <> C2 then
      begin
        if (C1 >= Ord('a')) and (C1 <= Ord('z')) then
          C1 := C1 xor $20;
        if (C2 >= Ord('a')) and (C2 <= Ord('z')) then
          C2 := C2 xor $20;
        if (C1 <> C2) then
          Exit(False);
      end;
      Inc(B1);
      Inc(B2);
      Dec(L1);
    end;
end;

function dxAllocStr(const S: string; out ALength: Integer): PChar; overload;
begin
  ALength := Length(S);
  Result := AllocMem((ALength + 1) * SizeOf(WideChar));
  Move(S[1], Result^, ALength * SizeOf(WideChar));
end;

function dxAllocStr(const S: string): PChar; overload;
var
  L: Integer;
begin
  Result := dxAllocStr(S, L);
end;

function dxCompareStrings(P1, P2: PChar; L1, L2: Integer; AIgnoreCase: Boolean = True): Integer; overload;
const
  CaseMap: array[Boolean] of Integer = (0, NORM_IGNORECASE);
begin
  Result := CompareStringW(LOCALE_USER_DEFAULT, CaseMap[AIgnoreCase], P1, L1, P2, L2) - CSTR_EQUAL;
end;

function dxCompareStrings(const S1, S2: string; AIgnoreCase: Boolean = True): Integer; overload;
begin
  Result := dxCompareStrings(PChar(S1), PChar(S2), Length(S1), Length(S2), AIgnoreCase);
end;

function dxMakeString(const P: PChar; L: Integer): string;
begin
  SetString(Result, P, L);
end;

function StrScan(Str: PChar; ACount: Integer; C: Char): PChar;
begin
  Result := Str;
  while (Result <> nil) and (Result^ <> C) do
  begin
    Dec(ACount);
    Inc(Result);
    if ACount <= 0 then
    begin
      Result := nil;
      Break;
    end;
  end;
end;

procedure OptimizeElement(var AElement: TdxExpressionElement);
var
  APrevElement: TdxExpressionElement;
begin
  if AElement <> nil then
  begin
    AElement.Optimize;
    if AElement.IsConstant and (AElement.ClassType <> TdxExpressionElementConstant) then
    begin
      APrevElement := AElement;
      AElement := TdxExpressionElementConstant.Create(APrevElement.Evaluate);
      APrevElement.Free;
    end;
  end;
end;

{ TdxParser }

constructor TdxParser.Create(const ADelimiters, AQuotes, ASpaces: string);
begin
  FQuotes := dxAllocStr(AQuotes, FQuotesLength);
  FSpaces := dxAllocStr(ASpaces, FSpacesLength);
  FDelimiters := dxAllocStr(ADelimiters, FDelimitersLength);
  FQuotedTextAsSingleToken := False;
  FQuotedTextAsSingleTokenUnquot := True;
  FSkipDelimiters := True;
  FSkipSpaces := True;
end;

destructor TdxParser.Destroy;
begin
  FreeMem(FDelimiters);
  FreeMem(FSpaces);
  FreeMem(FQuotes);
  inherited Destroy;
end;

procedure TdxParser.Initialize(const S: string);
begin
  FScanBuffer := S;
  FScan := PChar(FScanBuffer);
  FScanCount := Length(FScanBuffer);
end;

procedure TdxParser.Initialize(const P: PChar; C: Integer);
begin
  FScan := P;
  FScanCount := C;
  FScanBuffer := EmptyStr;
end;

function TdxParser.GetToken(out AToken: TdxParserToken): Boolean;
begin
  repeat
    AToken.Reset;
    Result := FetchToken(FScan, FScanCount, AToken);
  until not (Result and ShouldSkipToken(AToken));
end;

function TdxParser.GetToken(out AToken: TdxParserToken; const ADelimiters: string): Boolean;
var
  TB: PChar;
  TL: Integer;
begin
  TB := FDelimiters;
  TL := FDelimitersLength;
  try
    FDelimiters := PChar(ADelimiters);
    FDelimitersLength := Length(ADelimiters);
    Result := GetToken(AToken);
  finally
    FDelimitersLength := TL;
    FDelimiters := TB;
  end;
end;

function TdxParser.GetToken(out AToken: TdxParserToken; const ADelimiters, AQuotes, ASpaces: string): Boolean;
var
  TB1, TB2: PChar;
  TL1, TL2: Integer;
begin
  TB1 := FQuotes;
  TB2 := FSpaces;
  TL1 := FQuotesLength;
  TL2 := FSpacesLength;
  try
    FQuotes := PChar(AQuotes);
    FQuotesLength := Length(AQuotes);
    FSpaces := PChar(ASpaces);
    FSpacesLength := Length(ASpaces);
    Result := GetToken(AToken, ADelimiters);
  finally
    FSpacesLength := TL2;
    FQuotesLength := TL1;
    FSpaces := TB2;
    FQuotes := TB1;
  end;
end;

function TdxParser.MoveToNextSymbol: Boolean;
begin
  Result := MoveToNext(FScan, FScanCount);
end;

function TdxParser.Contains(const W: WideChar; L: PChar; C: Integer): LongBool;
begin
  Result := False;
  while C > 0 do
  begin
    Result := W = L^;
    if Result then
      Break;
    Inc(L);
    Dec(C);
  end;
end;

function TdxParser.FetchToken(var P: PChar; var C: Integer; var AToken: TdxParserToken): Boolean;

  procedure ExtractIdent(var AToken: TdxParserToken; ATokenType: Integer; ADelimiters: PChar; ADelimitersCount: Integer);
  begin
    AToken.Data := P;
    AToken.TokenType := ATokenType;
    while (C > 0) and not Contains(P^, ADelimiters, ADelimitersCount) do
    begin
      Dec(C);
      Inc(P);
    end;
    AToken.DataLength := (NativeUInt(P) - NativeUInt(AToken.Data)) div SizeOf(WideChar);
  end;

  procedure SetToken(var AToken: TdxParserToken; var P: PChar; var C: Integer; ATokenType, ATokenLength: Integer); inline;
  begin
    AToken.Data := P;
    AToken.DataLength := ATokenLength;
    AToken.TokenType := ATokenType;
    Inc(P, ATokenLength);
    Dec(C, ATokenLength);
  end;

var
  AQuot: WideChar;
  ASavedC: Integer;
  ASavedP: PChar;
begin
  if C > 0 then
  begin
    // Spaces
    if Contains(P^, FSpaces, FSpacesLength) then
      SetToken(AToken, P, C, dxTokenSpace, 1)
    else

    // Quotes
    if Contains(P^, FQuotes, FQuotesLength) then
    begin
      if QuotedTextAsSingleToken then
      begin
        AQuot := P^;
        ASavedP := P;
        ASavedC := C;
        MoveToNext(P, C);
        ExtractIdent(AToken, dxTokenQuotedText, @AQuot, 1);
        if C = 0 then // unterminated quoted string
        begin
          P := ASavedP;
          C := ASavedC;
          SetToken(AToken, P, C, dxTokenQuot, 1);
        end
        else
        begin
          MoveToNext(P, C);
          if not QuotedTextAsSingleTokenUnquot then
          begin
            Dec(AToken.Data);
            Inc(AToken.DataLength, 2);
          end;
        end;
      end
      else
        SetToken(AToken, P, C, dxTokenQuot, 1);
    end
    else

    // Delimiters
    if Contains(P^, FDelimiters, FDelimitersLength) then
      SetToken(AToken, P, C, dxTokenDelimiter, 1)
    else
      ExtractIdent(AToken, dxTokenIdent, FDelimiters, FDelimitersLength);
  end;
  Result := AToken.DataLength > 0;
end;

function TdxParser.MoveToNext(var P: PChar; var C: Integer): Boolean;
begin
  Result := C > 0;
  if Result then
  begin
    Inc(P);
    Dec(C);
  end;
end;

function TdxParser.ShouldSkipToken(const AToken: TdxParserToken): Boolean;
begin
  Result :=
    SkipQuotes and (AToken.TokenType = dxTokenQuot) or
    SkipSpaces and (AToken.TokenType = dxTokenSpace) or
    SkipDelimiters and (AToken.TokenType = dxTokenDelimiter);
end;

{ TdxParserToken }

function TdxParserToken.Compare(const S: string; IgnoreCase: Boolean = True): Boolean;
begin
  if Length(S) <> DataLength then
    Exit(False);
  if IgnoreCase then
    Result := dxCompareStrings(Data, PChar(S), DataLength, DataLength) = 0
  else
    Result := CompareMem(Data, PChar(S), DataLength);
end;

procedure TdxParserToken.Reset;
begin
  FillChar(Self, SizeOf(Self), 0);
end;

function TdxParserToken.ToString: string;
begin
  SetString(Result, Data, DataLength);
end;
{ TdxExpressionFastStack }

constructor TdxExpressionFastStack<T>.Create(AOwnObjects: Boolean);
begin
  inherited Create;
  FOwnObjects := AOwnObjects;
  SetCapacity(16);
end;

destructor TdxExpressionFastStack<T>.Destroy;
begin
  if FOwnObjects then
    FreeObjects;
  inherited Destroy;
end;

procedure TdxExpressionFastStack<T>.FreeObjects;
begin
  while Count > 0 do
    Pop.Free;
end;

function TdxExpressionFastStack<T>.Peek: T;
begin
  if Count = 0 then
    raise Exception.Create(sErrorStackIsEmpty);
  Result := FBuffer[Count - 1];
end;

function TdxExpressionFastStack<T>.Pop: T;
begin
  if Count = 0 then
    raise Exception.Create(sErrorStackIsEmpty);
  Result := FBuffer[Count - 1];
  Dec(FCount);
end;

function TdxExpressionFastStack<T>.Push(AObject: T): Integer;
begin
  Result := Count;
  if Count + 1 > FCapacity then
    SetCapacity(FCapacity * 2);
  FBuffer[Count] := AObject;
  Inc(FCount);
end;

procedure TdxExpressionFastStack<T>.SetCapacity(ACapacity: Integer);
begin
  ACapacity := Max(ACapacity, Count);
  if ACapacity <> FCapacity then
  begin
    FCapacity := ACapacity;
    SetLength(FBuffer, FCapacity);
  end;
end;

{ TdxExpressionEvalFunction }

constructor TdxExpressionEvalFunction.Create(const AName: string;
  AParamsCount: Integer; ADependsFromParametersOnly: Boolean;
  AProc: TdxExpressionEvalProc; ACategory: Byte);
begin
  inherited Create;
  FName := AName;
  FProc := AProc;
  FCategory := ACategory;
  FParamsCount := AParamsCount;
  FDependsFromParametersOnly := ADependsFromParametersOnly;
end;

function TdxExpressionEvalFunction.ToString: string;
var
  B: TStringBuilder;
  I: Integer;
begin
  if ParamsCount = 0 then
    Exit(Name);

  B := TdxStringBuilderManager.Get(32);
  try
    B.Append(Name);
    B.Append('(');
    if ParamsCount < 0 then
      B.Append('..')
    else
      for I := 0 to ParamsCount - 1 do
      begin
        if I > 0 then
          B.Append(',');
        B.Append(Chr(Ord('A') + I));
      end;
    B.Append(')');
    Result := B.ToString;
  finally
    TdxStringBuilderManager.Release(B);
  end;
end;

{ TdxExpressionEvalFunctionList }

constructor TdxExpressionEvalFunctionList.Create;
begin
  inherited Create;
  FRemap := TDictionary<string, TdxExpressionEvalFunction>.Create;
end;

destructor TdxExpressionEvalFunctionList.Destroy;
begin
  FreeAndNil(FRemap);
  inherited Destroy;
end;

function TdxExpressionEvalFunctionList.Find(const AName: PChar; ANameLength: Integer; out AFunction: TdxExpressionEvalFunction): Boolean;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Compare(Items[I].Name, AName, ANameLength) then
    begin
      AFunction := Items[I];
      Exit(True);
    end;

  if Remap.Count > 0 then
  begin
    if Remap.TryGetValue(UpperCase(dxMakeString(AName, ANameLength)), AFunction) then
      Exit(True);
  end;

  Result := False;
end;

function TdxExpressionEvalFunctionList.Find(const AName: PChar;
  ANameLength, AParamsCount: Integer; out AFunction: TdxExpressionEvalFunction): Boolean;
var
  AItem: TdxExpressionEvalFunction;
  I: Integer;
begin
  Result := False;
  for I := 0 to Count - 1 do
  begin
    AItem := Items[I];
    if (AItem.ParamsCount = AParamsCount) and Compare(AItem.Name, AName, ANameLength) then
    begin
      AFunction := AItem;
      Exit(True);
    end;
  end;
end;

function TdxExpressionEvalFunctionList.Find(const AName: string; out AFunction: TdxExpressionEvalFunction): Boolean;
begin
  Result := Find(PChar(AName), Length(AName), AFunction);
end;

function TdxExpressionEvalFunctionList.Find(const AName: string;
  AParamsCount: Integer; out AFunction: TdxExpressionEvalFunction): Boolean;
begin
  Result := Find(PChar(AName), Length(AName), AParamsCount, AFunction);
end;

function TdxExpressionEvalFunctionList.Compare(const S: string; B: PChar; L: Integer): Boolean;
begin
  Result := dxCompareTokens(PChar(S), B, L, Length(S));
end;

{ TdxExpressionEvalOperator }

constructor TdxExpressionEvalOperator.Create(const AName: string; AProc: TdxExpressionEvalProc;
  APriority, AParamsCount: Integer; AAssociativity: TdxExpressionEvalOperatorAssociativity);
begin
  inherited Create(AName, AParamsCount, True, AProc, 0);
  FAssociativity := AAssociativity;
  FPriority := APriority;
end;

{ TdxExpressionEvalVariable }

constructor TdxExpressionEvalVariable.Create(const AName: string; const AValue: PVariant);
begin
  inherited Create(AName, 0, False, EvalProc, 0);
  FValue := AValue;
end;

function TdxExpressionEvalVariable.EvalProc(AParams: TdxExpressionElements): Variant;
begin
  Result := FValue^;
end;

{ TdxExpressionElements }

constructor TdxExpressionElements.Create;
begin
  inherited Create;
  FList := TList.Create;
end;

destructor TdxExpressionElements.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited Destroy;
end;

procedure TdxExpressionElements.Add(AElement: TdxExpressionElement);
begin
  FList.Add(AElement);
end;

procedure TdxExpressionElements.AddFromStack(
  AStack: TdxExpressionFastStack<TdxExpressionElement>; ACount: Integer);
var
  AIndex: Integer;
begin
  FList.Count := FList.Count + ACount;
  AIndex := Count - 1;
  while ACount > 0 do
  begin
    FList.List[AIndex] := AStack.Pop;
    Dec(AIndex);
    Dec(ACount);
  end;
end;

procedure TdxExpressionElements.Clear;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].Free;
end;

function TdxExpressionElements.ToString(AExpression: TdxCustomExpression): string;
var
  I: Integer;
begin
  with TStringBuilder.Create(64) do
  try
    for I := 0 to Count - 1 do
    begin
      if I > 0 then
        Append(',');
      Append(Items[I].ToString(AExpression));
    end;
    Result := ToString;
  finally
    Free;
  end;
end;

function TdxExpressionElements.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TdxExpressionElements.GetItem(Index: Integer): TdxExpressionElement;
begin
  Result := TdxExpressionElement(FList.List[Index]);
end;

function TdxExpressionElements.IsConstant: Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 0 to Count - 1 do
    Result := Result and Items[I].IsConstant;
end;

procedure TdxExpressionElements.Optimize;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    OptimizeElement(TdxExpressionElement(FList.List[I]));
end;

{ TdxExpressionElementConstant }

constructor TdxExpressionElementConstant.Create(const AValue: Variant);
begin
  FValue := AValue;
end;

procedure TdxExpressionElementConstant.Optimize;
begin
  // do nothing
end;

function TdxExpressionElementConstant.Evaluate: Variant;
begin
  Result := FValue;
end;

function TdxExpressionElementConstant.ToString(AExpression: TdxCustomExpression): string;
begin
  Result := FValue;
end;

function TdxExpressionElementConstant.GetIsConstant: Boolean;
begin
  Result := True;
end;

{ TdxExpressionElementFunction }

constructor TdxExpressionElementFunction.Create(AEvalFunc: TdxExpressionEvalFunction);
begin
  FEvalFunc := AEvalFunc;
  if FEvalFunc = nil then
    raise EdxExpressionCompiler.Create('EvalFunc not assigned');
  FParams := TdxExpressionElements.Create;
end;

destructor TdxExpressionElementFunction.Destroy;
begin
  FreeAndNil(FParams);
  inherited Destroy;
end;

procedure TdxExpressionElementFunction.Optimize;
begin
  Params.Optimize;
end;

function TdxExpressionElementFunction.Evaluate: Variant;
begin
  Result := EvalFunc.Proc(Params);
end;

function TdxExpressionElementFunction.ToString(AExpression: TdxCustomExpression): string;
begin
  Result := Name;
  if Params.Count > 0 then
    Result := Result + '(' + Params.ToString(AExpression) + ')';
end;

function TdxExpressionElementFunction.GetIsConstant: Boolean;
begin
  Result := EvalFunc.DependsFromParametersOnly and Params.IsConstant;
end;

function TdxExpressionElementFunction.GetName: string;
begin
  Result := EvalFunc.Name;
end;

{ TdxExpressionElementOperator }

function TdxExpressionElementOperator.ToString(AExpression: TdxCustomExpression): string;

  function ParamToString(AParam: TdxExpressionElement): string;
  begin
    Result := AParam.ToString(AExpression);
    if AParam is TdxExpressionElementOperator then
      Result := '(' + Result + ')';
  end;

begin
  if Params.Count = 2 then
    Result := ParamToString(Params[0]) + ' ' + Name + ' ' + ParamToString(Params[1])
  else
    Result := Name + ' ' + ParamToString(Params[0]);
end;

{ TdxCustomExpression }

constructor TdxCustomExpression.Create;
begin
  inherited Create;
  FKnownFunctions := CreateKnownFunctions;
  FKnownOperators := CreateKnownOperators;
  FCache := TStringList.Create;
  FDelimiterChars := DefaultDelimiterChars;
  FQuoteChars := DefaultQuoteChars;
  FSpaceChars := DefaultSpaceChars;
end;

destructor TdxCustomExpression.Destroy;
begin
  CacheSize := 0;
  FreeAndNil(FCache);
  FreeAndNil(FKnownFunctions);
  FreeAndNil(FKnownOperators);
  FreeAndNil(FCompiled);
  inherited Destroy;
end;

procedure TdxCustomExpression.Compile(const S: string);
var
  AElement: TdxExpressionElement;
  AIndex: Integer;
begin
  if CacheSize > 0 then
  begin
    AIndex := FCache.IndexOf(S);
    if AIndex < 0 then
    begin
      if FCache.Count = CacheSize then
      begin
        FCache.Objects[0].Free;
        FCache.Delete(0);
      end;
      AElement := InternalCompile(S);
      OptimizeElement(AElement);
      AIndex := FCache.AddObject(S, AElement);
    end;
    FCompiled := TdxExpressionElement(FCache.Objects[AIndex]);
  end
  else
  begin
    FreeAndNil(FCompiled);
    FCompiled := InternalCompile(S);
  end;
end;

procedure TdxCustomExpression.Optimize;
begin
  if (FCompiled <> nil) and (CacheSize = 0) then
    OptimizeElement(FCompiled);
end;

function TdxCustomExpression.Evaluate: Variant;
begin
  if FCompiled <> nil then
    Result := FCompiled.Evaluate
  else
    raise EdxExpression.Create(sErrorNotCompiled);
end;

function TdxCustomExpression.Evaluate(const S: string): Variant;
begin
  Compile(S);
  Result := Evaluate;
end;

function TdxCustomExpression.ToString: string;
begin
  if FCompiled <> nil then
    Result := FCompiled.ToString(Self)
  else
    Result := '';
end;

procedure TdxCustomExpression.RegisterFunction(
  const AName: string; AProc: TdxExpressionEvalProc; ACategory: Byte = 0);
begin
  RegisterFunction(AName, AProc, 0, False, ACategory);
end;

procedure TdxCustomExpression.RegisterFunction(const AName: string;
  AProc: TdxExpressionEvalProc; AParamsCount: Integer; ADependsFromParametersOnly: Boolean; ACategory: Byte = 0);
var
  AFunction: TdxExpressionEvalFunction;
begin
  if KnownFunctions.Find(AName, AParamsCount, AFunction) then
    raise EdxExpression.CreateFmt(sErrorFunctionAlreadyRegistered, [AName, AParamsCount]);
  KnownFunctions.Add(TdxExpressionEvalFunction.Create(AName, AParamsCount, ADependsFromParametersOnly, AProc, ACategory));
end;

procedure TdxCustomExpression.RegisterFunctionRemap(const AOldName, ANewName: string);
var
  AFunction: TdxExpressionEvalFunction;
begin
  if KnownFunctions.Find(ANewName, AFunction) then
    KnownFunctions.Remap.Add(UpperCase(AOldName), AFunction)
  else
    raise EdxExpression.CreateFmt(sErrorFunctionNotFound, [ANewName]);
end;

procedure TdxCustomExpression.RegisterOperator(
  const AName: string; AProc: TdxExpressionEvalProc; AParamsCount, APriority: Integer);
const
  Map: array[Boolean] of TdxExpressionEvalOperatorAssociativity = (eoaRightToLeft, eoaLeftToRight);
begin
  if (AParamsCount < 1) or (AParamsCount > 2) then
    raise EdxExpression.Create(sErrorOperatorArguments);
  KnownOperators.Add(TdxExpressionEvalOperator.Create(AName, AProc, APriority, AParamsCount, Map[AParamsCount = 2]));
end;

procedure TdxCustomExpression.RegisterVariable(const AName: string; const AValue: PVariant);
var
  AFunction: TdxExpressionEvalFunction;
begin
  if KnownFunctions.Find(AName, AFunction) then
  begin
    if AFunction is TdxExpressionEvalVariable then
      TdxExpressionEvalVariable(AFunction).FValue := AValue
    else
      raise EdxExpression.CreateFmt(sErrorVariableAlreadyRegistered, [AName]);
  end;
  KnownFunctions.Add(TdxExpressionEvalVariable.Create(AName, AValue));
end;

procedure TdxCustomExpression.UnregisterFunctionOrVariable(const AName: string);
var
  AFunction: TdxExpressionEvalFunction;
begin
  if KnownFunctions.Find(AName, AFunction) then
    KnownFunctions.Remove(AFunction);
end;

function TdxCustomExpression.CreateCompiler: TdxCustomExpressionCompiler;
begin
  Result := TdxCustomExpressionCompiler.Create(Self);
end;

function TdxCustomExpression.CreateKnownFunctions: TdxExpressionEvalFunctionList;
begin
  Result := TdxExpressionEvalFunctionList.Create
end;

function TdxCustomExpression.CreateKnownOperators: TdxExpressionEvalFunctionList;
begin
  Result := TdxExpressionEvalFunctionList.Create
end;

function TdxCustomExpression.InternalCompile(const S: string): TdxExpressionElement;
var
  ACompiler: TdxCustomExpressionCompiler;
begin
  ACompiler := CreateCompiler;
  try
    ACompiler.Initialize(S);
    Result := ACompiler.Compile;
  finally
    ACompiler.Free;
  end;
end;

function TdxCustomExpression.GetCacheSize: Byte;
begin
  Result := FCache.Capacity;
end;

procedure TdxCustomExpression.SetCacheSize(const AValue: Byte);
begin
  if CacheSize <> AValue then
  begin
    while FCache.Count > AValue do
    begin
      if FCache.Objects[0] = FCompiled then
        FCompiled := nil;
      FCache.Objects[0].Free;
      FCache.Delete(0);
    end;
    FCache.Capacity := AValue;
  end;
end;

{ TdxCustomExpressionCompiler }

constructor TdxCustomExpressionCompiler.Create(AOwner: TdxCustomExpression);
begin
  FOwner := AOwner;
  inherited Create(AOwner.DelimiterChars, AOwner.QuoteChars, AOwner.SpaceChars);
  ClassOperator := TdxExpressionElementOperator;
  ClassFunction := TdxExpressionElementFunction;
  QuotedTextAsSingleTokenUnquot := True;
  QuotedTextAsSingleToken := True;
  SkipDelimiters := False;
  SkipSpaces := True;
end;

function TdxCustomExpressionCompiler.Compile: TdxExpressionElement;
begin
  FOutputBuffer := TdxExpressionFastStack<TdxExpressionElement>.Create(True);
  try
    FOperatorStack := TdxExpressionFastStack<TdxExpressionEvalOperator>.Create(False);
    try
      Result := CompileCore;
    finally
      FreeAndNil(FOperatorStack);
    end;
  finally
    FreeAndNil(FOutputBuffer);
  end;
end;

procedure TdxCustomExpressionCompiler.Error(const AMessage: string);
var
  AScanArea: string;
begin
  if ScanCount > 0 then
  begin
    SetString(AScanArea, Scan, Min(ScanCount, 16));
    raise EdxExpressionCompiler.CreateFmt(AMessage + #13#10 + sErrorCursorInfo, [Token.ToString, AScanArea]);
  end
  else
    raise EdxExpressionCompiler.Create(AMessage);
end;

procedure TdxCustomExpressionCompiler.Error(const AMessage: string; const AArguments: array of const);
begin
  Error(Format(AMessage, AArguments));
end;

function TdxCustomExpressionCompiler.CompileCore: TdxExpressionElement;
begin
  PrevSolidToken := ecsttNone;
  while GetToken(Token) do
  begin
    if not ProcessToken then
      Error(sErrorUnexpectedToken);
  end;

  while OperatorStack.Count > 0 do
    OutputOperator(OperatorStack.Pop);

  if OutputBuffer.Count <> 1 then
    Error(sErrorFewResults);
  Result := OutputBuffer.Pop;
end;

function TdxCustomExpressionCompiler.ProcessToken: Boolean;
var
  AValueD: Double;
  AValueI: Integer;
begin
  Result := False;
  case Token.TokenType of
    dxTokenDelimiter:
      Result := ProcessTokenAsDelimiter;
    dxExprTokenOperator:
      Result := ProcessTokenAsOperator;
    dxExprTokenFunction:
      Result := ProcessTokenAsFunction;

    dxTokenQuotedText:
      begin
        OutputBuffer.Push(TdxExpressionElementConstant.Create(Token.ToString));
        PrevSolidToken := ecsttOperand;
        Result := True;
      end;

    dxExprTokenConstantFloat:
      if TryStrToFloat(Token.ToString, AValueD, dxInvariantFormatSettings) then
      begin
        OutputBuffer.Push(TdxExpressionElementConstant.Create(AValueD));
        PrevSolidToken := ecsttOperand;
        Result := True;
      end;

    dxExprTokenConstantInt:
      if TryStrToInt(Token.ToString, AValueI) then
      begin
        OutputBuffer.Push(TdxExpressionElementConstant.Create(AValueI));
        PrevSolidToken := ecsttOperand;
        Result := True;
      end;
  end;
end;

function TdxCustomExpressionCompiler.ProcessTokenAsDelimiter: Boolean;
var
  AOperator: TdxExpressionEvalOperator;
begin
  Result := False;
  if Token.DataLength = 1 then
    case Token.Data^ of
      '(':
        begin
          OperatorStack.Push(nil);
          Result := True;
        end;
      ')':
        begin
          repeat
            AOperator := OperatorStack.Pop;
            if AOperator = nil then
              Break;
            OutputOperator(AOperator);
          until False;
          Result := True;
        end;
    end;
end;

function TdxCustomExpressionCompiler.ProcessTokenAsFunction: Boolean;
var
  AFunction: TdxExpressionEvalFunction;
  AFunctionElement: TdxExpressionElementFunction;
begin
  AFunctionElement := ClassFunction.Create(TdxExpressionEvalFunction(Token.Context));
  try
    if (ScanCount > 0) and (Scan^ = '(') then
      ParseParametersList(AFunctionElement);
    if AFunctionElement.EvalFunc.ParamsCount >= 0 then
    begin
      if AFunctionElement.EvalFunc.ParamsCount <> AFunctionElement.Params.Count then
      begin
        // try to find overload version of this function with other number of arguments
        if Owner.KnownFunctions.Find(AFunctionElement.Name, AFunctionElement.Params.Count, AFunction) then
          AFunctionElement.EvalFunc := AFunction;
      end;
      if AFunctionElement.EvalFunc.ParamsCount > AFunctionElement.Params.Count then
        Error(sErrorTooSmallArguments, [AFunctionElement.Name]);
      if AFunctionElement.EvalFunc.ParamsCount < AFunctionElement.Params.Count then
        Error(sErrorTooManyArguments, [AFunctionElement.Name]);
    end;
  except
    FreeAndNil(AFunctionElement);
    raise;
  end;
  OutputBuffer.Push(AFunctionElement);
  PrevSolidToken := ecsttOperand;
  Result := True;
end;

function TdxCustomExpressionCompiler.ProcessTokenAsOperator: Boolean;
var
  AOperator: TdxExpressionEvalOperator;
  AOperatorPeek: TdxExpressionEvalOperator;
begin
  AOperator := TdxExpressionEvalOperator(Token.Context);
  repeat
    if OperatorStack.Count > 0 then
      AOperatorPeek := OperatorStack.Peek
    else
      AOperatorPeek := nil;

    if AOperatorPeek <> nil then
    begin
      if (AOperator.Associativity = eoaRightToLeft) and (AOperator.Priority <  AOperatorPeek.Priority) or
         (AOperator.Associativity = eoaLeftToRight) and (AOperator.Priority <= AOperatorPeek.Priority)
      then
        OutputOperator(OperatorStack.Pop)
      else
        Break;
    end;
  until AOperatorPeek = nil;
  OperatorStack.Push(AOperator);
  PrevSolidToken := ecsttOperator;
  Result := True;
end;

procedure TdxCustomExpressionCompiler.OutputOperator(AOperator: TdxExpressionEvalOperator);
var
  AFunction: TdxExpressionElementFunction;
begin
  if AOperator = nil then
    Error(sErrorUnequalBrackets);

  AFunction := ClassOperator.Create(AOperator);
  try
    AFunction.Params.AddFromStack(OutputBuffer, AOperator.ParamsCount);
    OutputBuffer.Push(AFunction);
  except
    FreeAndNil(AFunction);
    Error(sErrorTooSmallArguments, [AOperator.Name]);
  end;
end;

procedure TdxCustomExpressionCompiler.ParseParametersList(AFunctionElement: TdxExpressionElementFunction);

  function ExtractParameter(AScanStart, AScanFinish: PChar): TdxExpressionElement;
  var
    ALength: Integer;
  begin
    ALength := (NativeUInt(AScanFinish) - NativeUInt(AScanStart)) div SizeOf(WideChar);
    if ALength > 0 then
      Result := Owner.InternalCompile(dxMakeString(AScanStart, ALength))
    else
      Result := nil;

    if Result = nil then
      Result := TdxExpressionElementConstant.Create('');
  end;

var
  ABracketLevel: Integer;
  AParameterCursor: PChar;
  AToken: TdxParserToken;
begin
  ABracketLevel := 0;
  MoveToNextSymbol; // skip Bracket
  AParameterCursor := Scan;
  while GetToken(AToken) do
  begin
    if (AToken.TokenType = dxTokenDelimiter) and (AToken.DataLength = 1) then
      case AToken.Data^ of
        '(':
          Inc(ABracketLevel);

        ')':
          if ABracketLevel = 0 then
          begin
            if (AParameterCursor <> Scan - AToken.DataLength) or (AFunctionElement.Params.Count > 0) then
              AFunctionElement.Params.Add(ExtractParameter(AParameterCursor, Scan - AToken.DataLength));
            Break;
          end
          else
            Dec(ABracketLevel);

        ',':
          if ABracketLevel = 0 then
          begin
            AFunctionElement.Params.Add(ExtractParameter(AParameterCursor, Scan - AToken.DataLength));
            AParameterCursor := Scan;
          end;
      end;
  end;

  if ABracketLevel <> 0 then
    Error(sErrorUnequalBrackets);
end;

{ TdxMathExpression }

constructor TdxMathExpression.Create;
begin
  inherited Create;
  // Functions
  RegisterFunction('Abs', FuncAbs, 1, True);
  RegisterFunction('Cos', FuncCos, 1, True);
  RegisterFunction('Exp', FuncExp, 1, True);
  RegisterFunction('IF', FuncIF, 3, True);
  RegisterFunction('Ln', FuncLn, 1, True);
  RegisterFunction('Log10', FuncLog10, 1, True);
  RegisterFunction('LogN', FuncLogN, 2, True);
  RegisterFunction('Max', FuncMax, 2, True);
  RegisterFunction('Min', FuncMin, 2, True);
  RegisterFunction('Power', FuncPower, 2, True);
  RegisterFunction('Random', FuncRandom, 0, False);
  RegisterFunction('Round', FuncRound, 1, True);
  RegisterFunction('Sin', FuncSin, 1, True);
  RegisterFunction('Trunc', FuncTrunc, 1, True);

  // Operators
  RegisterOperator('*', OperatorMultiply, 2, 10);
  RegisterOperator('/', OperatorDivide, 2, 10);
  RegisterOperator('^', OperatorPower, 2, 10);
  RegisterOperator('div', OperatorDivideInt, 2, 10);
  RegisterOperator('mod', OperatorMod, 2, 10);

  RegisterOperator('-', OperatorMinus, 1, 9);
  RegisterOperator('-', OperatorMinus, 2, 9);
  RegisterOperator('+', OperatorPlus, 2, 9);

  RegisterOperator('>', OperatorGreater, 2, 9);
  RegisterOperator('>=', OperatorGreaterOrEqual, 2, 9);
  RegisterOperator('<', OperatorLower, 2, 9);
  RegisterOperator('<=', OperatorLowerOrEqual, 2, 9);
  RegisterOperator('<>', OperatorNotEqual, 2, 9);
  RegisterOperator('=', OperatorEqual, 2, 9);

  RegisterOperator('not', OperatorNot, 1, 8);

  RegisterOperator('and', OperatorAnd, 2, 7);
  RegisterOperator('or', OperatorOr, 2, 7);
  RegisterOperator('xor', OperatorXor, 2, 7);
end;

function TdxMathExpression.CreateCompiler: TdxCustomExpressionCompiler;
begin
  Result := TdxMathExpressionCompiler.Create(Self);
end;

function TdxMathExpression.FuncAbs(AParams: TdxExpressionElements): Variant;
begin
  Result := Abs(AParams[0].Evaluate);
end;

function TdxMathExpression.FuncCos(AParams: TdxExpressionElements): Variant;
begin
  Result := Cos(AParams[0].Evaluate);
end;

function TdxMathExpression.FuncExp(AParams: TdxExpressionElements): Variant;
begin
  Result := Exp(AParams[0].Evaluate);
end;

function TdxMathExpression.FuncIF(AParams: TdxExpressionElements): Variant;
begin
  if AParams[0].Evaluate then
    Result := AParams[1].Evaluate
  else
    Result := AParams[2].Evaluate;
end;

function TdxMathExpression.FuncLn(AParams: TdxExpressionElements): Variant;
begin
  Result := Ln(AParams[0].Evaluate);
end;

function TdxMathExpression.FuncLog10(AParams: TdxExpressionElements): Variant;
begin
  Result := Log10(AParams[0].Evaluate);
end;

function TdxMathExpression.FuncLogN(AParams: TdxExpressionElements): Variant;
begin
  Result := LogN(AParams[0].Evaluate, AParams[1].Evaluate);
end;

function TdxMathExpression.FuncMax(AParams: TdxExpressionElements): Variant;
begin
  Result := Max(AParams[0].Evaluate, AParams[1].Evaluate);
end;

function TdxMathExpression.FuncMin(AParams: TdxExpressionElements): Variant;
begin
  Result := Min(AParams[0].Evaluate, AParams[1].Evaluate);
end;

function TdxMathExpression.FuncPower(AParams: TdxExpressionElements): Variant;
begin
  Result := Power(AParams[0].Evaluate, AParams[1].Evaluate);
end;

function TdxMathExpression.FuncRandom(AParams: TdxExpressionElements): Variant;
begin
  Result := Random;
end;

function TdxMathExpression.FuncRound(AParams: TdxExpressionElements): Variant;
begin
  Result := Round(AParams[0].Evaluate);
end;

function TdxMathExpression.FuncSin(AParams: TdxExpressionElements): Variant;
begin
  Result := Sin(AParams[0].Evaluate);
end;

function TdxMathExpression.FuncTrunc(AParams: TdxExpressionElements): Variant;
begin
  Result := Trunc(AParams[0].Evaluate);
end;

function TdxMathExpression.OperatorAnd(AParams: TdxExpressionElements): Variant;
begin
  Result := Integer(AParams[0].Evaluate) and Integer(AParams[1].Evaluate);
end;

function TdxMathExpression.OperatorDivide(AParams: TdxExpressionElements): Variant;
begin
  Result := AParams[0].Evaluate / AParams[1].Evaluate;
end;

function TdxMathExpression.OperatorDivideInt(AParams: TdxExpressionElements): Variant;
begin
  Result := AParams[0].Evaluate div AParams[1].Evaluate;
end;

function TdxMathExpression.OperatorEqual(AParams: TdxExpressionElements): Variant;
begin
  try
    Result := Ord(AParams[0].Evaluate = AParams[1].Evaluate);
  except
    on EVariantError do
      Result := False
    else
      raise;
  end;
end;

function TdxMathExpression.OperatorGreater(AParams: TdxExpressionElements): Variant;
begin
  Result := Ord(AParams[0].Evaluate > AParams[1].Evaluate);
end;

function TdxMathExpression.OperatorGreaterOrEqual(AParams: TdxExpressionElements): Variant;
begin
  Result := Ord(AParams[0].Evaluate >= AParams[1].Evaluate);
end;

function TdxMathExpression.OperatorLower(AParams: TdxExpressionElements): Variant;
begin
  Result := Ord(AParams[0].Evaluate < AParams[1].Evaluate);
end;

function TdxMathExpression.OperatorLowerOrEqual(AParams: TdxExpressionElements): Variant;
begin
  Result := Ord(AParams[0].Evaluate <= AParams[1].Evaluate);
end;

function TdxMathExpression.OperatorMinus(AParams: TdxExpressionElements): Variant;
begin
  if AParams.Count = 1 then
    Result := -AParams[0].Evaluate
  else
    Result := AParams[0].Evaluate - AParams[1].Evaluate;
end;

function TdxMathExpression.OperatorMod(AParams: TdxExpressionElements): Variant;
begin
  Result := AParams[0].Evaluate mod AParams[1].Evaluate;
end;

function TdxMathExpression.OperatorMultiply(AParams: TdxExpressionElements): Variant;
begin
  Result := AParams[0].Evaluate * AParams[1].Evaluate;
end;

function TdxMathExpression.OperatorNot(AParams: TdxExpressionElements): Variant;
begin
  Result := Ord(Integer(AParams[0].Evaluate) = 0);
end;

function TdxMathExpression.OperatorNotEqual(AParams: TdxExpressionElements): Variant;
begin
  Result := Ord(AParams[0].Evaluate <> AParams[1].Evaluate);
end;

function TdxMathExpression.OperatorOr(AParams: TdxExpressionElements): Variant;
begin
   Result := Integer(AParams[0].Evaluate) or Integer(AParams[1].Evaluate);
end;

function TdxMathExpression.OperatorPlus(AParams: TdxExpressionElements): Variant;
begin
  Result := AParams[0].Evaluate + AParams[1].Evaluate;
end;

function TdxMathExpression.OperatorPower(AParams: TdxExpressionElements): Variant;
begin
  Result := Power(AParams[0].Evaluate, AParams[1].Evaluate);
end;

function TdxMathExpression.OperatorXor(AParams: TdxExpressionElements): Variant;
begin
   Result := Integer(AParams[0].Evaluate) xor Integer(AParams[1].Evaluate);
end;

{ TdxMathExpressionCompiler }

function TdxMathExpressionCompiler.FetchToken(var P: PChar; var C: Integer; var AToken: TdxParserToken): Boolean;
var
  AEvalFunction: TdxExpressionEvalFunction;
  D: Integer;
  K: PChar;
  T: TdxParserToken;
begin
  Result := False;

  K := P;
  D := C;
  T.Reset;
  T.Data := P;
  T.DataLength := 1;
  while (D > 0) and Contains(K^, FDelimiters, FDelimitersLength) do
  begin
    if Owner.KnownOperators.Find(T.Data, T.DataLength, 1 + Ord(PrevSolidToken = ecsttOperand), AEvalFunction) then
    begin
      AToken.Context := AEvalFunction;
      AToken.TokenType := dxExprTokenOperator;
      AToken.Data := T.Data;
      AToken.DataLength := T.DataLength;
      Result := True;
      P := K + 1;
      C := D - 1;
    end;
    Inc(T.DataLength);
    Inc(K);
    Dec(D);
  end;

  if not Result then
  begin
    Result := inherited FetchToken(P, C, AToken);
    if Result then
      case AToken.TokenType of
        dxTokenDelimiter:
          if (AToken.DataLength = 1) and (AToken.Data^ = '[') then // Is it variable?
          begin
            K := StrScan(P, C, ']');
            if K <> nil then
            begin
              Inc(AToken.Data); // skip '['
              AToken.DataLength := (NativeUInt(K) - NativeUInt(P)) div SizeOf(Char);
              AToken.TokenType := dxExprTokenFunction;
              if Owner.KnownFunctions.Find(AToken.Data, AToken.DataLength, AEvalFunction) then
                Token.Context := AEvalFunction
              else
                Error(Format(sErrorVariableNotFound, [AToken.ToString]));

              C := C - (AToken.DataLength + 1);
              P := K + 1;
            end;
          end;

        dxTokenIdent:
          if IsNumeric(AToken) then
          begin
            if P^ = '.' then // Its seems like float
            begin
              K := P + 1;
              D := C - 1;
              if inherited FetchToken(K, D, T) and IsNumeric(T) then
              begin
                AToken.DataLength := AToken.DataLength + T.DataLength + 1;
                AToken.TokenType := dxExprTokenConstantFloat;
                P := K;
                C := D;
              end;
            end
            else
              AToken.TokenType := dxExprTokenConstantInt;
          end
          else
            if Owner.KnownFunctions.Find(AToken.Data, AToken.DataLength, AEvalFunction) then
            begin
              AToken.TokenType := dxExprTokenFunction;
              AToken.Context := AEvalFunction;
            end
            else
              if Owner.KnownOperators.Find(AToken.Data, AToken.DataLength, AEvalFunction) then
              begin
                AToken.TokenType := dxExprTokenOperator;
                AToken.Context := AEvalFunction;
              end;
      end;
  end;
end;

function TdxMathExpressionCompiler.IsNumeric(const AToken: TdxParserToken): Boolean;
var
  C: PChar;
  I: Integer;
begin
  Result := True;
  C := AToken.Data;
  for I := 0 to AToken.DataLength - 1 do
  begin
    Result := Result and CharInSet(C^, ['0'..'9']);
    if not Result then Break;
    Inc(C);
  end;
end;

function TdxMathExpressionCompiler.GetOwner: TdxMathExpression;
begin
  Result := TdxMathExpression(inherited Owner);
end;

end.
