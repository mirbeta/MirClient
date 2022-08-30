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

unit dxRichEdit.DocumentModel.Fields.InfixNotationParser;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Generics.Defaults, Generics.Collections, dxCultureInfo,
  dxGenerics;

type
  TdxInfixNotationParser = class;

  TdxOperationType = (
    Addition,
    Subtraction,
    Multiplication,
    Division,
    Power,
    Percentage,
    Negation);

  TdxMathTokenType = (
    Constant,
    &Operator);

  IdxMathToken = interface
    function GetType: TdxMathTokenType;
    property &Type: TdxMathTokenType read GetType;
  end;

  TdxInfixNotationParserState = (
    Start,
    Constant,
    AfterOperand,
    AfterPercentage,
    WaitOperator,
    Operator,
    Operand,
    OpenBracket,
    CloseBracket);

  TdxOperationPriority = (
    Lowest,
    Low,
    Medium,
    High,
    Highest);

  TdxAssociation = (
    Left,
    Right);

  TdxOperationHandler = procedure(AOperands: TStack<Double>);

  TdxMathTokenCollection = TList<IdxMathToken>;

  { TdxMathematicalCalculator }

  TdxMathematicalCalculator = class
  private class var
    Operations: array[TdxOperationType] of TdxOperationHandler;
  strict private
    class constructor Initialize;
    class procedure ProcessAdditionOperation(AOperands: TStack<Double>); static;
    class procedure ProcessSubstractionOperation(AOperands: TStack<Double>); static;
    class procedure ProcessMultiplicationOperation(AOperands: TStack<Double>); static;
    class procedure ProcessDivisionOperation(AOperands: TStack<Double>); static;
    class procedure ProcessPowerOperation(AOperands: TStack<Double>); static;
    class procedure ProcessPercentageOperation(AOperands: TStack<Double>); static;
    class procedure ProcessNegationOperation(AOperands: TStack<Double>); static;
    class function GetOperand(AOperands: TStack<Double>): Double; static;
    class procedure SetOperand(AValue: Double; AOperands: TStack<Double>); static;
  public
    function Calculate(const ANotation: string; const ACulture: TdxCultureInfo): Double; virtual;
    function ParseInfixNotation(const ANotation: string; const ACulture: TdxCultureInfo): TdxMathTokenCollection;
    function ProcessTokens(AResult: TdxMathTokenCollection): TArray<Double>;
    procedure ProcessToken(const AToken: IdxMathToken; AOperands: TStack<Double>);
  end;

  { TdxOperationBase }

  IdxOperationBase = interface(IdxMathToken)
    function GetOperationType: TdxOperationType;
    function GetPriority: TdxOperationPriority;
    function GetAssociation: TdxAssociation;

    function ToString: string;

    property Association: TdxAssociation read GetAssociation;
    property Priority: TdxOperationPriority read GetPriority;
    property OperationType: TdxOperationType read GetOperationType;
  end;

  TdxOperationBase = class abstract(TInterfacedObject, IdxOperationBase, IdxMathToken)
  protected
    // IdxOperationBase
    function GetOperationType: TdxOperationType; virtual; abstract;
    function GetPriority: TdxOperationPriority; virtual; abstract;
    function GetAssociation: TdxAssociation; virtual; abstract;
    //IdxMathToken
    function GetType: TdxMathTokenType; virtual;
  public
    function ToString: string; override;

    property Association: TdxAssociation read GetAssociation;
    property Priority: TdxOperationPriority read GetPriority;
    property OperationType: TdxOperationType read GetOperationType;
  end;

  TdxOperationStack = class(TStack<IdxOperationBase>);

  { TdxInfixNotationParserStateBase }

  TdxInfixNotationParserStateBase = class abstract
  strict private
    class var
      FDelimiters: TdxCharList;
  strict private
    FParser: TdxInfixNotationParser;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateDelimitersTable: TdxCharList; static;
    function GetCulture: TdxCultureInfo;
  protected
    procedure ChangeState(AType: TdxInfixNotationParserState);
    function IsDelimiter(ACh: Char): Boolean;
  public
    constructor Create(AParser: TdxInfixNotationParser);
    function ProcessChar(const ANotation: string; AIndex: Integer): Boolean; virtual; abstract;

    property Parser: TdxInfixNotationParser read FParser;
    property Culture: TdxCultureInfo read GetCulture;
  end;

  { TdxInfixNotationParser }

  TdxInfixNotationParser = class
  strict private
    FCulture: TdxCultureInfo;
    FCurrentExpression: TdxOperationStack;
    FOuterExpressions: TStack<TdxOperationStack>;
    FParsingError: Boolean;
    FState: TdxInfixNotationParserStateBase;
    FTokens: TdxMathTokenCollection;
  protected
    procedure ParseCore(const ANotation: string); virtual;
    procedure BeginParse; virtual;
    procedure EndParse(out AResult: TdxMathTokenCollection); virtual;
    procedure EnsureOuterExpressionsAreFinished;
    procedure ChangeState(AType: TdxInfixNotationParserState); virtual;
    procedure AddOperation(const AOperation: IdxOperationBase); virtual;
    function ShouldEjectOperation(const AOperation: IdxOperationBase): Boolean; virtual;
    function GetTopOperator: IdxOperationBase; virtual;
    procedure AddToken(const AToken: IdxMathToken); virtual;
    procedure AddTokensFromCurrentExpression; virtual;
    procedure StartInnerExpression; virtual;
    procedure EndInnerExpression; virtual;
  public
    constructor Create(const ACulture: TdxCultureInfo);
    destructor Destroy; override;
    function Parse(const ANotation: string): TdxMathTokenCollection; virtual;

    property State: TdxInfixNotationParserStateBase read FState;
    property Culture: TdxCultureInfo read FCulture;
  end;


implementation

uses
  Math, Character,
  dxCoreClasses,
  dxRichEdit.Utils.Exceptions,
  dxCharacters,
  dxRichEdit.DocumentModel.FieldFormatter;

type
  { TdxAdditionOperation }

  TdxAdditionOperation = class(TdxOperationBase)
  protected
    function GetPriority: TdxOperationPriority; override;
    function GetOperationType: TdxOperationType; override;
    function GetAssociation: TdxAssociation; override;
  end;

  { TdxSubtractionOperation }

  TdxSubtractionOperation = class(TdxOperationBase)
  protected
    function GetPriority: TdxOperationPriority; override;
    function GetOperationType: TdxOperationType; override;
    function GetAssociation: TdxAssociation; override;
  end;

  { TdxMultiplicationOperation }

  TdxMultiplicationOperation = class(TdxOperationBase)
  protected
    function GetPriority: TdxOperationPriority; override;
    function GetOperationType: TdxOperationType; override;
    function GetAssociation: TdxAssociation; override;
  end;

  { TdxDivisionOperation }

  TdxDivisionOperation = class(TdxOperationBase)
  protected
    function GetPriority: TdxOperationPriority; override;
    function GetOperationType: TdxOperationType; override;
    function GetAssociation: TdxAssociation; override;
  end;

  { TdxPowerOperation }

  TdxPowerOperation = class(TdxOperationBase)
  protected
    function GetPriority: TdxOperationPriority; override;
    function GetOperationType: TdxOperationType; override;
    function GetAssociation: TdxAssociation; override;
  end;

  { TdxPercentageOperation }

  TdxPercentageOperation = class(TdxOperationBase)
  protected
    function GetPriority: TdxOperationPriority; override;
    function GetOperationType: TdxOperationType; override;
    function GetAssociation: TdxAssociation; override;
  end;

  { TdxNegationOperation }

  TdxNegationOperation = class(TdxOperationBase)
  protected
    function GetPriority: TdxOperationPriority; override;
    function GetOperationType: TdxOperationType; override;
    function GetAssociation: TdxAssociation; override;
  end;

  { TdxConstant }

  TdxConstant = class abstract(TInterfacedObject, IdxMathToken)
  strict private
    FValue: Double;
    //IdxMathToken
    function GetType: TdxMathTokenType;
  public
    constructor Create(AValue: Double);
    function ToString: string; override;

    property Value: Double read FValue;
  end;

  { TdxStartInfixNotationParserState }

  TdxStartInfixNotationParserState = class(TdxInfixNotationParserStateBase)
  protected
    function TryChangeState(ACh: Char): Boolean; virtual;
  public
    function ProcessChar(const ANotation: string; AIndex: Integer): Boolean; override;
  end;

  { TdxConstantInfixNotationParserState }

  TdxConstantInfixNotationParserState = class(TdxInfixNotationParserStateBase)
  strict private
    FNumber: TStringBuilder;
  protected
    procedure AddConstantToken; virtual;
  public
    constructor Create(AParser: TdxInfixNotationParser);
    destructor Destroy; override;
    function ProcessChar(const ANotation: string; AIndex: Integer): Boolean; override;
  end;

  { TdxAfterOperandInfixNotationParserState }

  TdxAfterOperandInfixNotationParserState = class(TdxInfixNotationParserStateBase)
  protected
    procedure ChangeState(ACh: Char); overload; virtual;
  public
    function ProcessChar(const ANotation: string; AIndex: Integer): Boolean; override;
  end;

  { TdxAfterPercentageInfixNotationParserState }

  TdxAfterPercentageInfixNotationParserState = class(TdxAfterOperandInfixNotationParserState)
  public
    function ProcessChar(const ANotation: string; AIndex: Integer): Boolean; override;
  end;

  { TdxOperandInfixNotationParserState }

  TdxOperandInfixNotationParserState = class(TdxInfixNotationParserStateBase)
  protected
    function TryChangeState(ACh: Char): Boolean; virtual;
  public
    function ProcessChar(const ANotation: string; AIndex: Integer): Boolean; override;
  end;

  { TdxWaitOperatorInfixNotationParserState }

  TdxWaitOperatorInfixNotationParserState = class(TdxInfixNotationParserStateBase)
  public
    function ProcessChar(const ANotation: string; AIndex: Integer): Boolean; override;
  end;

  { TdxOperatorInfixNotationParserState }

  TdxOperatorInfixNotationParserState = class(TdxInfixNotationParserStateBase)
  strict private
    FOperation: TStringBuilder;
    class function TryGetOperation(const AOperation: string; var AOperationType: TdxOperationType): Boolean; static;
  protected
    procedure TryAddOperation(ACh: Char); virtual;
    procedure ChangeStateByOperationType(AType: TdxOperationType); virtual;
    function CreateOperation(const AOperation: string): IdxOperationBase; virtual;
    function CreateOperationCore(AType: TdxOperationType): IdxOperationBase; virtual;
  public
    constructor Create(AParser: TdxInfixNotationParser);
    destructor Destroy; override;
    function ProcessChar(const ANotation: string; AIndex: Integer): Boolean; override;
  end;

  { TdxOpenBracketInfixNotationParserState }

  TdxOpenBracketInfixNotationParserState = class(TdxInfixNotationParserStateBase)
  public
    function ProcessChar(const ANotation: string; AIndex: Integer): Boolean; override;
  end;

  { TdxCloseBracketInfixNotationParserState }

  TdxCloseBracketInfixNotationParserState = class(TdxInfixNotationParserStateBase)
  public
    function ProcessChar(const ANotation: string; AIndex: Integer): Boolean; override;
  end;

{ TdxOperationBase }

function TdxOperationBase.ToString: string;
const
  ResultMap: array[TdxOperationType] of string = ('Addition', 'Subtraction', 'Multiplication',
    'Division', 'Power', 'Percentage', 'Negation');
begin
  Result := ResultMap[OperationType];
end;

function TdxOperationBase.GetType: TdxMathTokenType;
begin
  Result := TdxMathTokenType.Operator;
end;

{ TdxAdditionOperation }

function TdxAdditionOperation.GetPriority: TdxOperationPriority;
begin
  Result := TdxOperationPriority.Low;
end;

function TdxAdditionOperation.GetOperationType: TdxOperationType;
begin
  Result := TdxOperationType.Addition;
end;

function TdxAdditionOperation.GetAssociation: TdxAssociation;
begin
  Result := TdxAssociation.Left;
end;

{ TdxSubtractionOperation }

function TdxSubtractionOperation.GetPriority: TdxOperationPriority;
begin
  Result := TdxOperationPriority.Low;
end;

function TdxSubtractionOperation.GetOperationType: TdxOperationType;
begin
  Result := TdxOperationType.Subtraction;
end;

function TdxSubtractionOperation.GetAssociation: TdxAssociation;
begin
  Result := TdxAssociation.Left;
end;

{ TdxMultiplicationOperation }

function TdxMultiplicationOperation.GetPriority: TdxOperationPriority;
begin
  Result := TdxOperationPriority.Medium;
end;

function TdxMultiplicationOperation.GetOperationType: TdxOperationType;
begin
  Result := TdxOperationType.Multiplication;
end;

function TdxMultiplicationOperation.GetAssociation: TdxAssociation;
begin
  Result := TdxAssociation.Left;
end;

{ TdxDivisionOperation }

function TdxDivisionOperation.GetPriority: TdxOperationPriority;
begin
  Result := TdxOperationPriority.Medium;
end;

function TdxDivisionOperation.GetOperationType: TdxOperationType;
begin
  Result := TdxOperationType.Division;
end;

function TdxDivisionOperation.GetAssociation: TdxAssociation;
begin
  Result := TdxAssociation.Left;
end;

{ TdxPowerOperation }

function TdxPowerOperation.GetPriority: TdxOperationPriority;
begin
  Result := TdxOperationPriority.High;
end;

function TdxPowerOperation.GetOperationType: TdxOperationType;
begin
  Result := TdxOperationType.Power;
end;

function TdxPowerOperation.GetAssociation: TdxAssociation;
begin
  Result := TdxAssociation.Right;
end;

{ TdxPercentageOperation }

function TdxPercentageOperation.GetPriority: TdxOperationPriority;
begin
  Result := TdxOperationPriority.Highest;
end;

function TdxPercentageOperation.GetOperationType: TdxOperationType;
begin
  Result := TdxOperationType.Percentage;
end;

function TdxPercentageOperation.GetAssociation: TdxAssociation;
begin
  Result := TdxAssociation.Right;
end;

{ TdxNegationOperation }

function TdxNegationOperation.GetPriority: TdxOperationPriority;
begin
  Result := TdxOperationPriority.Highest;
end;

function TdxNegationOperation.GetOperationType: TdxOperationType;
begin
  Result := TdxOperationType.Negation;
end;

function TdxNegationOperation.GetAssociation: TdxAssociation;
begin
  Result := TdxAssociation.Right;
end;

{ TdxConstant }

constructor TdxConstant.Create(AValue: Double);
begin
  inherited Create;
  FValue := AValue;
end;

function TdxConstant.GetType: TdxMathTokenType;
begin
  Result := TdxMathTokenType.Constant;
end;

function TdxConstant.ToString: string;
begin
  Result := Format('Constant: %g', [Value]);
end;

{ TdxInfixNotationParserStateBase }

class constructor TdxInfixNotationParserStateBase.Initialize;
begin
  FDelimiters := CreateDelimitersTable;
end;

class destructor TdxInfixNotationParserStateBase.Finalize;
begin
  FreeAndNil(FDelimiters);
end;

constructor TdxInfixNotationParserStateBase.Create(AParser: TdxInfixNotationParser);
begin
  inherited Create;
  FParser := AParser;
end;

class function TdxInfixNotationParserStateBase.CreateDelimitersTable: TdxCharList;
begin
  Result := TdxCharList.Create;
  Result.Add(' ');
  Result.Add(#$09);
  Result.Add(#$0A);
  Result.Add(#$0D);
  Result.Add(TdxCharacters.EmSpace);
  Result.Add(TdxCharacters.EnSpace);
  Result.Add(TdxCharacters.QmSpace);
end;

function TdxInfixNotationParserStateBase.GetCulture: TdxCultureInfo;
begin
  Result := Parser.Culture;
end;

procedure TdxInfixNotationParserStateBase.ChangeState(AType: TdxInfixNotationParserState);
begin
  Parser.ChangeState(AType);
end;

function TdxInfixNotationParserStateBase.IsDelimiter(ACh: Char): Boolean;
begin
  Result := FDelimiters.Contains(ACh);
end;

{ TdxStartInfixNotationParserState }

function TdxStartInfixNotationParserState.ProcessChar(const ANotation: string; AIndex: Integer): Boolean;
begin
  if IsDelimiter(ANotation[AIndex]) then
    Exit(True);

  if not TryChangeState(ANotation[AIndex]) then
    TdxFieldFormatter.ThrowSyntaxError(ANotation[AIndex]);
  Result := False;
end;

function TdxStartInfixNotationParserState.TryChangeState(ACh: Char): Boolean;
var
  ADecimalSeparator: Char;
begin
  Result := True;
  ADecimalSeparator := Culture.FormatSettings.DecimalSeparator;
  if ACh = '-' then
  begin
    ChangeState(TdxInfixNotationParserState.Operand);
    Exit;
  end;
  if {$IFDEF DELPHIXE4}ACh.IsDigit{$ELSE}TCharacter.IsDigit(ACh){$ENDIF} or (ACh = ADecimalSeparator) then
  begin
    ChangeState(TdxInfixNotationParserState.Constant);
    Exit;
  end;
  if ACh = '(' then
  begin
    ChangeState(TdxInfixNotationParserState.OpenBracket);
    Exit;
  end;
  Result := False;
end;

{ TdxConstantInfixNotationParserState }

constructor TdxConstantInfixNotationParserState.Create(AParser: TdxInfixNotationParser);
begin
  inherited Create(AParser);
  FNumber := TStringBuilder.Create;
end;

destructor TdxConstantInfixNotationParserState.Destroy;
begin
  FreeAndNil(FNumber);
  inherited Destroy;
end;

function TdxConstantInfixNotationParserState.ProcessChar(const ANotation: string; AIndex: Integer): Boolean;
var
  ACh, ADecimalSeparator: Char;
begin
  ACh := ANotation[AIndex];
  ADecimalSeparator := Culture.FormatSettings.DecimalSeparator;
  if {$IFDEF DELPHIXE4}ACh.IsDigit{$ELSE}TCharacter.IsDigit(ACh){$ENDIF} or (ACh = ADecimalSeparator) then
  begin
    FNumber.Append(ACh);
    if AIndex = Length(ANotation) then
      AddConstantToken;
    Exit(True);
  end
  else
  begin
    AddConstantToken;
    ChangeState(TdxInfixNotationParserState.AfterOperand);
  end;
  Result := False;
end;

procedure TdxConstantInfixNotationParserState.AddConstantToken;
begin
  Parser.AddToken(TdxConstant.Create(StrToFloat(FNumber.ToString, Culture.FormatSettings)));
end;

{ TdxAfterOperandInfixNotationParserState }

function TdxAfterOperandInfixNotationParserState.ProcessChar(const ANotation: string; AIndex: Integer): Boolean;
begin
  if IsDelimiter(ANotation[AIndex]) then
    Exit(True);

  if {$IFDEF DELPHIXE4}ANotation[AIndex].IsDigit{$ELSE}TCharacter.IsDigit(ANotation[AIndex]){$ENDIF} then
    TdxFieldFormatter.ThrowMissingOperatorError
  else
    ChangeState(ANotation[AIndex]);
  Result := False;
end;

procedure TdxAfterOperandInfixNotationParserState.ChangeState(ACh: Char);
begin
  if ACh = ')' then
    ChangeState(TdxInfixNotationParserState.CloseBracket)
  else
    ChangeState(TdxInfixNotationParserState.WaitOperator);
end;

{ TdxAfterPercentageInfixNotationParserState }

function TdxAfterPercentageInfixNotationParserState.ProcessChar(const ANotation: string; AIndex: Integer): Boolean;
begin
  if ANotation[AIndex] = '%' then
  begin
    TdxFieldFormatter.ThrowSyntaxError(ANotation[AIndex]);
    Exit(False);
  end;
  Result := inherited ProcessChar(ANotation, AIndex);
end;

{ TdxOperandInfixNotationParserState }

function TdxOperandInfixNotationParserState.ProcessChar(const ANotation: string; AIndex: Integer): Boolean;
begin
  if IsDelimiter(ANotation[AIndex]) then
    Exit(True);

  if ANotation[AIndex] = '-' then
  begin
    Parser.AddOperation(TdxNegationOperation.Create);
    Exit(True);
  end
  else
    if not TryChangeState(ANotation[AIndex]) then
      TdxFieldFormatter.ThrowSyntaxError(ANotation[AIndex]);
  Result := False;
end;

function TdxOperandInfixNotationParserState.TryChangeState(ACh: Char): Boolean;
var
  ADecimalSeparator: Char;
begin
  ADecimalSeparator := Culture.FormatSettings.DecimalSeparator;
  if {$IFDEF DELPHIXE4}ACh.IsDigit{$ELSE}TCharacter.IsDigit(ACh){$ENDIF} or (ACh = ADecimalSeparator) then
  begin
    ChangeState(TdxInfixNotationParserState.Constant);
    Exit(True);
  end;
  if ACh = '(' then
  begin
    ChangeState(TdxInfixNotationParserState.OpenBracket);
    Exit(True);
  end;
  Result := False;
end;

{ TdxWaitOperatorInfixNotationParserState }

function TdxWaitOperatorInfixNotationParserState.ProcessChar(const ANotation: string; AIndex: Integer): Boolean;
var
  ACh: Char;
begin
  ACh := ANotation[AIndex];
  if IsDelimiter(ACh) then
  begin
    ChangeState(TdxInfixNotationParserState.Operand);
    Exit(True);
  end;
{$IFDEF DELPHIXE4}
  if ACh.IsSymbol or ACh.IsPunctuation then
{$ELSE}
  if TCharacter.IsSymbol(ACh) or TCharacter.IsPunctuation(ACh) then
{$ENDIF}
  begin
    ChangeState(TdxInfixNotationParserState.Operator);
    Exit(False);
  end
  else
  begin
    TdxFieldFormatter.ThrowSyntaxError(ACh);
    Exit(False);
  end;
end;

{ TdxOperatorInfixNotationParserState }

constructor TdxOperatorInfixNotationParserState.Create(AParser: TdxInfixNotationParser);
begin
  inherited Create(AParser);
  FOperation := TStringBuilder.Create;
end;

destructor TdxOperatorInfixNotationParserState.Destroy;
begin
  FreeAndNil(FOperation);
  inherited Destroy;
end;

function TdxOperatorInfixNotationParserState.ProcessChar(const ANotation: string; AIndex: Integer): Boolean;
var
  ACh: Char;
begin
  ACh := ANotation[AIndex];
{$IFDEF DELPHIXE4}
  if ACh.IsSymbol or ACh.IsPunctuation then
{$ELSE}
  if TCharacter.IsSymbol(ACh) or TCharacter.IsPunctuation(ACh) then
{$ENDIF}
    TryAddOperation(ACh)
  else
  begin
    TdxFieldFormatter.ThrowSyntaxError(FOperation[0]);
    Exit(False);
  end;
  Result := True;
end;

procedure TdxOperatorInfixNotationParserState.TryAddOperation(ACh: Char);
var
  AOperation: IdxOperationBase;
begin
  FOperation.Append(ACh);
  AOperation := CreateOperation(FOperation.ToString);
  if AOperation <> nil then
  begin
    Parser.AddOperation(AOperation);
    ChangeStateByOperationType(AOperation.&OperationType);
  end;
end;

class function TdxOperatorInfixNotationParserState.TryGetOperation(const AOperation: string;
  var AOperationType: TdxOperationType): Boolean;
begin
  if Length(AOperation) <> 1 then
    Exit(False);
  case AOperation[1] of
    '%':
      AOperationType := TdxOperationType.Percentage;
    '*':
      AOperationType := TdxOperationType.Multiplication;
    '+':
      AOperationType := TdxOperationType.Addition;
    '-':
      AOperationType := TdxOperationType.Subtraction;
    '/':
      AOperationType := TdxOperationType.Division;
    '^':
      AOperationType := TdxOperationType.Power
    else
      Exit(False);
  end;
  Result := True;
end;

procedure TdxOperatorInfixNotationParserState.ChangeStateByOperationType(AType: TdxOperationType);
begin
  if AType = TdxOperationType.Percentage then
    ChangeState(TdxInfixNotationParserState.AfterPercentage)
  else
    ChangeState(TdxInfixNotationParserState.Operand);
end;

function TdxOperatorInfixNotationParserState.CreateOperation(const AOperation: string): IdxOperationBase;
var
  AOperationType: TdxOperationType;
begin
  if TryGetOperation(AOperation, AOperationType) then
    Result := CreateOperationCore(AOperationType)
  else
    Result := nil;
end;

function TdxOperatorInfixNotationParserState.CreateOperationCore(AType: TdxOperationType): IdxOperationBase;
begin
  case AType of
    TdxOperationType.Addition:
      Exit(TdxAdditionOperation.Create);
    TdxOperationType.Subtraction:
      Exit(TdxSubtractionOperation.Create);
    TdxOperationType.Multiplication:
      Exit(TdxMultiplicationOperation.Create);
    TdxOperationType.Division:
      Exit(TdxDivisionOperation.Create);
    TdxOperationType.Power:
      Exit(TdxPowerOperation.Create);
    TdxOperationType.Percentage:
      Exit(TdxPercentageOperation.Create);
    else
      TdxRichEditExceptions.ThrowInternalException;
      Exit(nil);
  end;
end;

{ TdxOpenBracketInfixNotationParserState }

function TdxOpenBracketInfixNotationParserState.ProcessChar(const ANotation: string; AIndex: Integer): Boolean;
begin
  Parser.StartInnerExpression;
  ChangeState(TdxInfixNotationParserState.Start);
  Result := True;
end;

{ TdxCloseBracketInfixNotationParserState }

function TdxCloseBracketInfixNotationParserState.ProcessChar(const ANotation: string; AIndex: Integer): Boolean;
begin
  Parser.EndInnerExpression;
  ChangeState(TdxInfixNotationParserState.AfterOperand);
  Result := True;
end;

{ TdxInfixNotationParser }

constructor TdxInfixNotationParser.Create(const ACulture: TdxCultureInfo);
begin
  inherited Create;
  FCulture := ACulture;
  FState := TdxStartInfixNotationParserState.Create(Self);
end;

destructor TdxInfixNotationParser.Destroy;
begin
  FreeAndNil(FState);
  inherited Destroy;
end;

function TdxInfixNotationParser.Parse(const ANotation: string): TdxMathTokenCollection;
begin
  BeginParse;
  try
    try
      ParseCore(ANotation);
    except
      FParsingError := True;
      raise ;
    end;
  finally
    EndParse(Result);
  end;
end;

procedure TdxInfixNotationParser.ParseCore(const ANotation: string);
var
  AIndex, ANotationLength: Integer;
begin
  AIndex := 1;
  ANotationLength := Length(ANotation);
  while AIndex <= ANotationLength do
  begin
    if FState.ProcessChar(ANotation, AIndex) then
      Inc(AIndex);
  end;
end;

procedure TdxInfixNotationParser.BeginParse;
begin
  FParsingError := False;
  FTokens := TdxMathTokenCollection.Create;
  FCurrentExpression := TdxOperationStack.Create;
  FOuterExpressions := TStack<TdxOperationStack>.Create;
end;

procedure TdxInfixNotationParser.EndParse(out AResult: TdxMathTokenCollection);
begin
  AResult := nil;
  try
    AddTokensFromCurrentExpression;
    FParsingError := FParsingError or (FOuterExpressions.Count > 0);
    EnsureOuterExpressionsAreFinished;
  finally
    if FParsingError then
      FTokens.Free
    else
      AResult := FTokens;
    FTokens := nil;
    FreeAndNil(FCurrentExpression);
    while FOuterExpressions.Count > 0 do
      FOuterExpressions.Pop.Free;
    FreeAndNil(FOuterExpressions);
  end;
end;

procedure TdxInfixNotationParser.EnsureOuterExpressionsAreFinished;
begin
  if FOuterExpressions.Count > 0 then
    TdxFieldFormatter.ThrowUnexpectedEndOfFormulaError;
end;

procedure TdxInfixNotationParser.ChangeState(AType: TdxInfixNotationParserState);
begin
  FState.Free;
  case AType of
    TdxInfixNotationParserState.Start:
      FState := TdxStartInfixNotationParserState.Create(Self);
    TdxInfixNotationParserState.Constant:
      FState := TdxConstantInfixNotationParserState.Create(Self);
    TdxInfixNotationParserState.AfterOperand:
      FState := TdxAfterOperandInfixNotationParserState.Create(Self);
    TdxInfixNotationParserState.AfterPercentage:
      FState := TdxAfterPercentageInfixNotationParserState.Create(Self);
    TdxInfixNotationParserState.WaitOperator:
      FState := TdxWaitOperatorInfixNotationParserState.Create(Self);
    TdxInfixNotationParserState.Operator:
      FState := TdxOperatorInfixNotationParserState.Create(Self);
    TdxInfixNotationParserState.Operand:
      FState := TdxOperandInfixNotationParserState.Create(Self);
    TdxInfixNotationParserState.OpenBracket:
      FState := TdxOpenBracketInfixNotationParserState.Create(Self);
    TdxInfixNotationParserState.CloseBracket:
      FState := TdxCloseBracketInfixNotationParserState.Create(Self);
    else
      TdxRichEditExceptions.ThrowInternalException;
  end;
end;

procedure TdxInfixNotationParser.AddOperation(const AOperation: IdxOperationBase);
begin
  while (FCurrentExpression.Count > 0) and ShouldEjectOperation(AOperation) do
    AddToken(FCurrentExpression.Pop);
  FCurrentExpression.Push(AOperation);
end;

function TdxInfixNotationParser.ShouldEjectOperation(const AOperation: IdxOperationBase): Boolean;
begin
  if AOperation.Association = TdxAssociation.Left then
    Exit(GetTopOperator.Priority >= AOperation.Priority)
  else
    Exit(GetTopOperator.Priority > AOperation.Priority);
end;

function TdxInfixNotationParser.GetTopOperator: IdxOperationBase;
begin
  Result := FCurrentExpression.Peek;
end;

procedure TdxInfixNotationParser.AddToken(const AToken: IdxMathToken);
begin
  FTokens.Add(AToken);
end;

procedure TdxInfixNotationParser.AddTokensFromCurrentExpression;
begin
  while FCurrentExpression.Count > 0 do
    AddToken(FCurrentExpression.Pop);
end;

procedure TdxInfixNotationParser.StartInnerExpression;
begin
  FOuterExpressions.Push(FCurrentExpression);
  FCurrentExpression := TdxOperationStack.Create;
end;

procedure TdxInfixNotationParser.EndInnerExpression;
begin
  AddTokensFromCurrentExpression;
  if FOuterExpressions.Count > 0 then
  begin
    FCurrentExpression.Free;
    FCurrentExpression := FOuterExpressions.Pop
  end
  else
    TdxFieldFormatter.ThrowUnexpectedEndOfFormulaError;
end;

{ TdxMathematicalCalculator }

class constructor TdxMathematicalCalculator.Initialize;
begin
  Operations[TdxOperationType.Addition] := ProcessAdditionOperation;
  Operations[TdxOperationType.Division] := ProcessDivisionOperation;
  Operations[TdxOperationType.Multiplication] := ProcessMultiplicationOperation;
  Operations[TdxOperationType.Negation] := ProcessNegationOperation;
  Operations[TdxOperationType.Percentage] := ProcessPercentageOperation;
  Operations[TdxOperationType.Power] := ProcessPowerOperation;
  Operations[TdxOperationType.Subtraction] := ProcessSubstractionOperation;
end;

class procedure TdxMathematicalCalculator.ProcessAdditionOperation(AOperands: TStack<Double>);
begin
  SetOperand(GetOperand(AOperands) + GetOperand(AOperands), AOperands);
end;

class procedure TdxMathematicalCalculator.ProcessSubstractionOperation(AOperands: TStack<Double>);
var
  ASecondOperand, AFirstOperand: Double;
begin
  ASecondOperand := GetOperand(AOperands);
  AFirstOperand := GetOperand(AOperands);
  SetOperand(AFirstOperand - ASecondOperand, AOperands);
end;

class procedure TdxMathematicalCalculator.ProcessMultiplicationOperation(AOperands: TStack<Double>);
begin
  SetOperand(GetOperand(AOperands) * GetOperand(AOperands), AOperands);
end;

class procedure TdxMathematicalCalculator.ProcessDivisionOperation(AOperands: TStack<Double>);
var
  ASecondOperand, AFirstOperand: Double;
begin
  ASecondOperand := GetOperand(AOperands);
  AFirstOperand := GetOperand(AOperands);
  if ASecondOperand <> 0 then
    SetOperand(AFirstOperand / ASecondOperand, AOperands)
  else
    TdxFieldFormatter.ThrowZeroDivideError;
end;

class procedure TdxMathematicalCalculator.ProcessPowerOperation(AOperands: TStack<Double>);
var
  ASecondOperand, AFirstOperand: Double;
begin
  ASecondOperand := GetOperand(AOperands);
  AFirstOperand := GetOperand(AOperands);
  SetOperand(Power(AFirstOperand, ASecondOperand), AOperands);
end;

class procedure TdxMathematicalCalculator.ProcessPercentageOperation(AOperands: TStack<Double>);
begin
  SetOperand(GetOperand(AOperands) / 100, AOperands);
end;

class procedure TdxMathematicalCalculator.ProcessNegationOperation(AOperands: TStack<Double>);
begin
  SetOperand(-GetOperand(AOperands), AOperands);
end;

class function TdxMathematicalCalculator.GetOperand(AOperands: TStack<Double>): Double;
begin
  if AOperands.Count = 0 then
    TdxFieldFormatter.ThrowUnexpectedEndOfFormulaError;
  Result := AOperands.Pop;
end;

class procedure TdxMathematicalCalculator.SetOperand(AValue: Double; AOperands: TStack<Double>);
begin
  AOperands.Push(AValue);
end;

function TdxMathematicalCalculator.Calculate(const ANotation: string; const ACulture: TdxCultureInfo): Double;
var
  AOutputConstants: TArray<Double>;
  AValue: Double;
  ATokens: TdxMathTokenCollection;
begin
  ATokens := ParseInfixNotation(ANotation, ACulture);
  try
    AOutputConstants := ProcessTokens(ATokens);
    Result := 0;
    for AValue in AOutputConstants do
      Result := Result + AValue;
  finally
    ATokens.Free;
  end;
end;

function TdxMathematicalCalculator.ParseInfixNotation(const ANotation: string; const ACulture: TdxCultureInfo): TdxMathTokenCollection;
var
  AParser: TdxInfixNotationParser;
begin
  AParser := TdxInfixNotationParser.Create(ACulture);
  try
    Result := AParser.Parse(ANotation);
  finally
    AParser.Free;
  end;
end;

function TdxMathematicalCalculator.ProcessTokens(AResult: TdxMathTokenCollection): TArray<Double>;
var
  AOperands: TStack<Double>;
  AToken: IdxMathToken;
begin
  AOperands := TStack<Double>.Create;
  try
    for AToken in AResult do
      ProcessToken(AToken, AOperands);
    Result := AOperands.ToArray;
  finally
    AOperands.Free;
  end;
end;

procedure TdxMathematicalCalculator.ProcessToken(const AToken: IdxMathToken; AOperands: TStack<Double>);
var
  AType: TdxOperationType;
begin
  if AToken.&Type = TdxMathTokenType.Operator then
  begin
    AType := TdxOperationBase(AToken).OperationType;
    Operations[AType](AOperands);
  end
  else
    AOperands.Push(TdxConstant(AToken).Value);
end;

end.

