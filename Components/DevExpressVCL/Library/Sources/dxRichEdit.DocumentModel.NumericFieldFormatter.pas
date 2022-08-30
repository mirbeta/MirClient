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

unit dxRichEdit.DocumentModel.NumericFieldFormatter;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Classes, Generics.Defaults, Generics.Collections, dxCultureInfo,
  dxGenerics,
  dxRichEdit.Options,
  dxRichEdit.DocumentModel.FieldFormatter;

type
  TdxNumberFieldFormatInfo = class;
  TdxNumberPattern = class;
  TdxFormatRangeIteratorBackward = class;
  TdxFormatRangeIteratorForward = class;
  TdxFormatRangeIterator = class;
  TdxNumberFormatRange = class;

  TdxNumberFormatParserState = (
    Start,
    IntegerPart,
    FractionPart,
    EmbedText,
    Finish);

  TdxDigitProcessResult = (
    Success,
    Skip,
    &End);

  IdxNumberFormatParserStateBase = interface
    function GetType: TdxNumberFormatParserState;
    procedure ProcessChar(AIndex: Integer; AFormatInfo: TdxNumberFieldFormatInfo);

    property &Type: TdxNumberFormatParserState read GetType;
  end;

  IdxNumberFormatterStateBase = interface
    function GetCanChangeState: Boolean;

    function FormatNextDigit(AIterator: TdxFormatRangeIterator; AIndex: Integer;
      const ANumber: string; AResult: TStringBuilder): TdxDigitProcessResult;
    function GetNextState: IdxNumberFormatterStateBase;

    property CanChangeState: Boolean read GetCanChangeState;
  end;

  TdxNumberFormatRangeType = (
    None,
    Text,
    Sign,
    StartNumberMask,
    NumberMask,
    DecimalSeparator,
    GroupSeparator,
    Terminator
  );

  { TdxNumberFormatRange }

  TdxNumberFormatRange = class
  strict private
    FStartIndex: Integer;
    FEndIndex: Integer;
    FType: TdxNumberFormatRangeType;
    procedure SetEndIndex(const AValue: Integer);
    function GetLength: Integer;
  public
    constructor Create(AStartIndex: Integer = -1; AEndIndex: Integer = -1;
      AType: TdxNumberFormatRangeType = TdxNumberFormatRangeType.None); overload;
    constructor Create(AType: TdxNumberFormatRangeType); overload;
    constructor Create(AIndex: Integer; AType: TdxNumberFormatRangeType); overload;
    function GetText(const AFormat: string): string;

    property StartIndex: Integer read FStartIndex write FEndIndex;
    property EndIndex: Integer read FEndIndex write SetEndIndex;
    property &Type: TdxNumberFormatRangeType read FType write FType;
    property Length: Integer read GetLength;
  end;

  TdxNumberFormatRangeCollection = TdxObjectList<TdxNumberFormatRange>;

  { TdxNumericFieldFormatter }

  TdxNumericFieldFormatter = class(TdxSpecificFieldFormatter<Double>)
  strict private
    const
      MaxFormatStringLength = 64;
      MaxFractionLength = 10;
  strict private
    FNumber: Double;
    FFormatInfo: TdxNumberFieldFormatInfo;
    FState: IdxNumberFormatterStateBase;
    FCustomSeparators: TdxMailMergeCustomSeparators;
    function GetDecimalSeparator: Char;
    function GetFormatString: string;
  protected
    function InternalFormat(const AValue: Double; const AFormat: string): string; override;
    function FormatByDefault(const AValue: Double): string; override;
    procedure BeginFormat(const ANumber: Double; const AFormat: string);
    procedure EndFormat;
    procedure PopulateNumberFormatInfo(AFormatInfo: TdxNumberFieldFormatInfo); virtual;
    procedure Format(AResult: TStringBuilder); overload; virtual;
    function GetActualNumber(ARoundValue: Integer): Double; virtual;
    function GetNumberString(ANumber: Double): string; virtual;
    procedure FormatIntegerPart(ANumber: Double; APattern: TdxNumberPattern; AResult: TStringBuilder); virtual;
    procedure FormatIntegerPartCore(ANumber: Double; AIterator: TdxFormatRangeIteratorBackward; AResult: TStringBuilder);
    procedure FormatFractionalPart(ANumber: Double; APattern: TdxNumberPattern; AResult: TStringBuilder); virtual;
    procedure FormatFractionalPartCore(ANumber: Double; AResult: TStringBuilder; AIterator: TdxFormatRangeIteratorForward);
    procedure FormatNumberPart(AIterator: TdxFormatRangeIterator; const ANumberString: string; AResult: TStringBuilder);

    property Number: Double read FNumber write FNumber;
    property NumberFormatInfo: TdxNumberFieldFormatInfo read FFormatInfo write FFormatInfo;
    property DecimalSeparator: Char read GetDecimalSeparator;
    property FormatString: string read GetFormatString;
  public
    constructor Create;
    destructor Destroy; override;

    // for internal use
    property CustomSeparators: TdxMailMergeCustomSeparators read FCustomSeparators;
  end;

  { TdxNumberFieldFormatInfo }

  TdxNumberFieldFormatInfo = class
  strict private
    FFormatString: string;
    FFormatInfo: TdxNumberFormatInfo;
    FPositivePattern: TdxNumberPattern;
    FNegativePattern: TdxNumberPattern;
    FZeroPattern: TdxNumberPattern;
    FCustomSeparators: TdxMailMergeCustomSeparators;
    FCurrentPattern: TdxNumberPattern;
    FCurrentRange: TdxNumberFormatRange;
    function GetDecimalSeparator: Char;
    function GetGroupSeparator: Char;
    function GetGroupSize: Integer;
  protected
    function GetNumberFormat: TdxNumberFormatInfo; virtual;

    property NumberFormat: TdxNumberFormatInfo read GetNumberFormat;
    property CurrentRange: TdxNumberFormatRange read FCurrentRange;
    property CurrentPattern: TdxNumberPattern read FCurrentPattern;
  public
    constructor Create(const AFormatString: string; const AFormatInfo: TdxNumberFormatInfo);
    destructor Destroy; override;
    function GetPattern(AValue: Double): TdxNumberPattern;

    procedure AddToRange(ACharIndex: Integer; ARangeType: TdxNumberFormatRangeType);
    function ChangeCurrentPatternToNext: Boolean;
    procedure ResetRangeMerging;

    property FormatString: string read FFormatString;
    property DecimalSeparator: Char read GetDecimalSeparator;
    property GroupSeparator: Char read GetGroupSeparator;
    property CustomSeparators: TdxMailMergeCustomSeparators read FCustomSeparators;
    property GroupSize: Integer read GetGroupSize;
    property PositivePattern: TdxNumberPattern read FPositivePattern;
    property NegativePattern: TdxNumberPattern read FNegativePattern;
    property ZeroPattern: TdxNumberPattern read FZeroPattern;
  end;

  { TdxNumberFormatterStateBase }

  TdxNumberFormatterStateBase = class abstract(TInterfacedObject, IdxNumberFormatterStateBase)
  strict private
    FFormatter: TdxNumericFieldFormatter;
    function GetNumber: Double;
  protected
    function GetCanChangeState: Boolean; virtual; abstract;
    procedure AppendResult(const AStr: string; AResultString: TStringBuilder); overload; virtual; abstract;
    procedure AppendResult(ACh: Char; AResultString: TStringBuilder); overload; virtual; abstract;
    procedure AppendResult(ARange: TdxNumberFormatRange; AResultString: TStringBuilder); overload; virtual;
    function GetNumberSign(APatternChar: Char): Char; virtual;
    function GetPlaceholder(APatternChar: Char): Char; virtual;

    property Formatter: TdxNumericFieldFormatter read FFormatter;
    property Number: Double read GetNumber;
  public
    constructor Create(AFormatter: TdxNumericFieldFormatter);
    function FormatNextDigit(AIterator: TdxFormatRangeIterator; AIndex: Integer; const ANumber: string; AResult: TStringBuilder): TdxDigitProcessResult; virtual; abstract;
    function GetNextState: IdxNumberFormatterStateBase; virtual; abstract;

    property CanChangeState: Boolean read GetCanChangeState;
  end;

  { TdxNumberPattern }

  TdxNumberPatternType = (
    Positive,
    Negative,
    Zero);

  TdxNumberPattern = class
  strict private
    FRanges: TdxNumberFormatRangeCollection;
    FType: TdxNumberPatternType;
    function GetIsEmpty: Boolean;
    function GetHasSign: Boolean;
    function GetHasGroupSeparator: Boolean;
  public
    constructor Create(AType: TdxNumberPatternType);
    destructor Destroy; override;
    function GetFractionLength: Integer;
    function GetSeparatorIndex: Integer;

    property &Type: TdxNumberPatternType read FType;
    property Ranges: TdxNumberFormatRangeCollection read FRanges;
    property IsEmpty: Boolean read GetIsEmpty;
    property HasSign: Boolean read GetHasSign;
    property HasGroupSeparator: Boolean read GetHasGroupSeparator;
  end;

  { TdxFormatRangeIterator }

  TdxFormatRangeIterator = class abstract
  strict private
    FPattern: TdxNumberPattern;
    FFormat: string;
    FCurrentRangeIndex: Integer;
    FCurrentCharacterIndex: Integer;
    FPreviousRangesLength: Integer;
    FCachedRangeIndex: Integer;
    FCachedRangeText: string;
    function GetCurrentRange: TdxNumberFormatRange;
    function GetInternalCurrentCharacter: Char;
    function GetRanges: TdxNumberFormatRangeCollection;
  protected
    function MoveRangeToNext: Boolean; virtual;
    function GetCurrentCharacter: Char; virtual;
    function GetActualCharIndex: Integer; virtual;
    procedure MoveRangeIndexToNext; virtual; abstract;
    function GetLogicalCharIndex: Integer; virtual; abstract;

    property Ranges: TdxNumberFormatRangeCollection read GetRanges;
    property CurrentCharacterIndex: Integer read FCurrentCharacterIndex write FCurrentRangeIndex;
    property CurrentRangeIndex: Integer read FCurrentRangeIndex write FCurrentRangeIndex;
  public
    constructor Create(APattern: TdxNumberPattern; ARangeIndex: Integer; const AFormat: string);
    function MoveNext: Boolean; virtual;
    function IsTextRange(ARange: TdxNumberFormatRange): Boolean;
    function IsCharIndexOutOfCurrentRange(ACharIndex: Integer): Boolean;

    property Pattern: TdxNumberPattern read FPattern;
    property FormatString: string read FFormat;
    property CurrentRange: TdxNumberFormatRange read GetCurrentRange;
    property CurrentCharacter: Char read GetInternalCurrentCharacter;
  end;

  { TdxFormatRangeIteratorBackward }

  TdxFormatRangeIteratorBackward = class(TdxFormatRangeIterator)
  protected
    function GetLogicalCharIndex: Integer; override;
    procedure MoveRangeIndexToNext; override;
  end;

  { TdxFormatRangeIteratorForward }

  TdxFormatRangeIteratorForward = class(TdxFormatRangeIterator)
  protected
    function GetLogicalCharIndex: Integer; override;
    procedure MoveRangeIndexToNext; override;
  end;

  { TdxNumberFormatParser }

  TdxNumberFormatParser = class
  strict private
    FState: IdxNumberFormatParserStateBase;
  protected
    property State: IdxNumberFormatParserStateBase read FState;
  public
    constructor Create;
    procedure Parse(AFormatInfo: TdxNumberFieldFormatInfo);
    procedure ChangeState(const AState: IdxNumberFormatParserStateBase); overload;
    procedure ChangeState(AType: TdxNumberFormatParserState); overload;
  end;

  { TdxNumberFormatParserStateBase }

  TdxNumberFormatParserStateBase = class abstract(TInterfacedObject, IdxNumberFormatParserStateBase)
  strict private
    FCalculator: TdxNumberFormatParser;
  protected
    function GetType: TdxNumberFormatParserState; virtual; abstract;
    function GetCalculator: TdxNumberFormatParser; virtual;
    procedure ChangeState(const AState: IdxNumberFormatParserStateBase); overload;
    procedure ChangeState(AType: TdxNumberFormatParserState); overload;
  public
    constructor Create(AFormatter: TdxNumberFormatParser);
    procedure ProcessChar(AIndex: Integer; AFormatInfo: TdxNumberFieldFormatInfo); virtual; abstract;

    property Calculator: TdxNumberFormatParser read GetCalculator;
    property &Type: TdxNumberFormatParserState read GetType;
  end;

implementation

uses
  Contnrs, Math,
  dxRichEdit.Utils.Exceptions,
  dxStringHelper;

type
  { TdxNumberPatternStateBase }

  TdxNumberPatternStateBase = class abstract(TdxNumberFormatParserStateBase)
  protected
    function ProcessCharStateSpecific(AIndex: Integer; AFormatInfo: TdxNumberFieldFormatInfo): Boolean; virtual; abstract;
    procedure ProcessCharStateCommon(AIndex: Integer; AFormatInfo: TdxNumberFieldFormatInfo);
    procedure ProcessDoubleQuotesChar(AIndex: Integer; AFormatInfo: TdxNumberFieldFormatInfo);
    procedure ProcessBackslashChar(AIndex: Integer; AFormatInfo: TdxNumberFieldFormatInfo);
    function IsDecimalSeparator(ACh: Char; AFormatInfo: TdxNumberFieldFormatInfo): Boolean;
    function IsGroupSeparator(ACh: Char; AFormatInfo: TdxNumberFieldFormatInfo): Boolean;
  public
    procedure ProcessChar(AIndex: Integer; AFormatInfo: TdxNumberFieldFormatInfo); override;
  end;

  { TdxStartPatternState }

  TdxStartPatternState = class(TdxNumberPatternStateBase)
  protected
    function GetType: TdxNumberFormatParserState; override;
    function ProcessCharStateSpecific(AIndex: Integer; AFormatInfo: TdxNumberFieldFormatInfo): Boolean; override;
  end;

  { TdxNumberMaskStateBase }

  TdxNumberMaskStateBase = class abstract(TdxNumberPatternStateBase)
  protected
    function ProcessCharStateSpecific(AIndex: Integer; AFormatInfo: TdxNumberFieldFormatInfo): Boolean; override;
  end;

  { TdxIntegerPartState }

  TdxIntegerPartState = class(TdxNumberMaskStateBase)
  protected
    function GetType: TdxNumberFormatParserState; override;
    function ProcessCharStateSpecific(AIndex: Integer; AFormatInfo: TdxNumberFieldFormatInfo): Boolean; override;
  end;

  { TdxFractionPartState }

  TdxFractionPartState = class(TdxNumberMaskStateBase)
  protected
    function GetType: TdxNumberFormatParserState; override;
  end;

  { TdxEmbedTextState }

  TdxEmbedTextState = class(TdxNumberFormatParserStateBase)
  strict private
    FPrevState: IdxNumberFormatParserStateBase;
  protected
    function GetType: TdxNumberFormatParserState; override;
  public
    constructor Create(ACalculator: TdxNumberFormatParser; const APrevState: IdxNumberFormatParserStateBase); reintroduce;
    procedure ProcessChar(AIndex: Integer; AFormatInfo: TdxNumberFieldFormatInfo); override;
  end;

  { TdxFinishState }

  TdxFinishState = class(TdxNumberFormatParserStateBase)
  protected
    function GetType: TdxNumberFormatParserState; override;
  public
    procedure ProcessChar(AIndex: Integer; AFormatInfo: TdxNumberFieldFormatInfo); override;
  end;

  { TdxIntegerFormatterStateBase }

  TdxIntegerFormatterStateBase = class abstract(TdxNumberFormatterStateBase)
  strict private
    function GetGroupSize: Integer;
    function GetGroupSeparator: string;
  protected
    procedure AppendResult(const AStr: string; AResultString: TStringBuilder); override;
    procedure AppendResult(ACh: Char; AResultString: TStringBuilder); override;
    procedure AppendSignIfNeeded(APattern: TdxNumberPattern; AResultString: TStringBuilder); virtual;
    procedure AppendGroupSeparator(ADigitCount: Integer; AResultString: TStringBuilder); virtual;

    property GroupSize: Integer read GetGroupSize;
    property GroupSeparator: string read GetGroupSeparator;
  end;

  { TdxIntegerFormatterState }

  TdxIntegerFormatterState = class(TdxIntegerFormatterStateBase)
  protected
    function GetCanChangeState: Boolean; override;
    procedure AppendRemainedDigits(AIndex: Integer; const ANumber: string; ASeparateGroups: Boolean; AResult: TStringBuilder); virtual;
    procedure AppendCurrentDigit(AIndex: Integer; const ANumber: string; ASeparateGroups: Boolean; AResult: TStringBuilder); virtual;
  public
    function FormatNextDigit(AIterator: TdxFormatRangeIterator; AIndex: Integer; const ANumber: string; AResult: TStringBuilder): TdxDigitProcessResult; override;
    function GetNextState: IdxNumberFormatterStateBase; override;
  end;

  { TdxFinalIntegerFormatterState }

  TdxFinalIntegerFormatterState = class(TdxIntegerFormatterStateBase)
  protected
    function GetCanChangeState: Boolean; override;
  public
    function FormatNextDigit(AIterator: TdxFormatRangeIterator; AIndex: Integer; const ANumber: string; AResult: TStringBuilder): TdxDigitProcessResult; override;
    function GetNextState: IdxNumberFormatterStateBase; override;
  end;

  { TdxFractionFormatterStateBase }

  TdxFractionFormatterStateBase = class abstract(TdxNumberFormatterStateBase)
  strict private
    function GetDecimalSeparator: Char;
  protected
    procedure AppendResult(ACh: Char; AResultString: TStringBuilder); override;
    procedure AppendResult(const AStr: string; AResultString: TStringBuilder); override;

    property DecimalSeparator: Char read GetDecimalSeparator;
  end;

  { TdxFractionalFormatterState }

  TdxFractionalFormatterState = class(TdxFractionFormatterStateBase)
  protected
    function GetCanChangeState: Boolean; override;
  public
    function FormatNextDigit(AIterator: TdxFormatRangeIterator; AIndex: Integer; const ANumber: string; AResult: TStringBuilder): TdxDigitProcessResult; override;
    function GetNextState: IdxNumberFormatterStateBase; override;
  end;

  { TdxFinalFractionalFormatterState }

  TdxFinalFractionalFormatterState = class(TdxFractionFormatterStateBase)
  protected
    function GetCanChangeState: Boolean; override;
  public
    function FormatNextDigit(AIterator: TdxFormatRangeIterator; AIndex: Integer; const ANumber: string; AResult: TStringBuilder): TdxDigitProcessResult; override;
    function GetNextState: IdxNumberFormatterStateBase; override;
  end;

{ TdxNumberFormatParser }

constructor TdxNumberFormatParser.Create;
begin
  inherited Create;
  FState := TdxStartPatternState.Create(Self);
end;

procedure TdxNumberFormatParser.Parse(AFormatInfo: TdxNumberFieldFormatInfo);
var
  AFormatString: string;
  ALength, I: Integer;
begin
  AFormatString := AFormatInfo.FormatString;
  if AFormatString = '' then
    Exit;

  ALength := Length(AFormatString);
  for I := 1 to ALength do
  begin
    if State.&Type = TdxNumberFormatParserState.Finish then
      Break;
    State.ProcessChar(I, AFormatInfo);
  end;
end;

procedure TdxNumberFormatParser.ChangeState(const AState: IdxNumberFormatParserStateBase);
begin
  FState := AState;
end;

procedure TdxNumberFormatParser.ChangeState(AType: TdxNumberFormatParserState);
begin
  case AType of
    TdxNumberFormatParserState.IntegerPart:
      ChangeState(TdxIntegerPartState.Create(Self));
    TdxNumberFormatParserState.FractionPart:
      ChangeState(TdxFractionPartState.Create(Self));
    TdxNumberFormatParserState.Start:
      ChangeState(TdxStartPatternState.Create(Self));
    TdxNumberFormatParserState.Finish:
      ChangeState(TdxFinishState.Create(Self));
    else
      TdxRichEditExceptions.ThrowInternalException;
  end;
end;

{ TdxNumberFormatParserStateBase }

constructor TdxNumberFormatParserStateBase.Create(AFormatter: TdxNumberFormatParser);
begin
  inherited Create;
  FCalculator := AFormatter;
end;

function TdxNumberFormatParserStateBase.GetCalculator: TdxNumberFormatParser;
begin
  Result := FCalculator;
end;

procedure TdxNumberFormatParserStateBase.ChangeState(const AState: IdxNumberFormatParserStateBase);
begin
  Calculator.ChangeState(AState);
end;

procedure TdxNumberFormatParserStateBase.ChangeState(AType: TdxNumberFormatParserState);
begin
  Calculator.ChangeState(AType);
end;

{ TdxNumberPatternStateBase }

procedure TdxNumberPatternStateBase.ProcessChar(AIndex: Integer; AFormatInfo: TdxNumberFieldFormatInfo);
begin
  if not ProcessCharStateSpecific(AIndex, AFormatInfo) then
    ProcessCharStateCommon(AIndex, AFormatInfo);
end;

procedure TdxNumberPatternStateBase.ProcessCharStateCommon(AIndex: Integer; AFormatInfo: TdxNumberFieldFormatInfo);
var
  ACh: Char;
begin
  ACh := AFormatInfo.FormatString[AIndex];

  if IsGroupSeparator(ACh, AFormatInfo) then
  begin
    AFormatInfo.AddToRange(AIndex, TdxNumberFormatRangeType.GroupSeparator);
    Exit;
  end;
  case ACh of
    '+',
    '-':
      begin
        AFormatInfo.ResetRangeMerging;
        AFormatInfo.AddToRange(AIndex, TdxNumberFormatRangeType.Sign);
      end;
    '''':
      ChangeState(TdxEmbedTextState.Create(Calculator, Self));
    '"':
      ProcessDoubleQuotesChar(AIndex, AFormatInfo);
    '\':
      ProcessBackslashChar(AIndex, AFormatInfo);
    else
      AFormatInfo.AddToRange(AIndex, TdxNumberFormatRangeType.Text);
  end;
end;

procedure TdxNumberPatternStateBase.ProcessDoubleQuotesChar(AIndex: Integer; AFormatInfo: TdxNumberFieldFormatInfo);
var
  APrevCharIndex: Integer;
begin
  APrevCharIndex := AIndex - 1;
  if (APrevCharIndex >= 0) and (AFormatInfo.FormatString[APrevCharIndex] = '\') then
  begin
    AFormatInfo.ResetRangeMerging;
    AFormatInfo.AddToRange(AIndex, TdxNumberFormatRangeType.Text);
  end
  else
    TdxNumericFieldFormatter.ThrowSyntaxError('"');
end;

procedure TdxNumberPatternStateBase.ProcessBackslashChar(AIndex: Integer; AFormatInfo: TdxNumberFieldFormatInfo);
var
  APrevCharIndex: Integer;
begin
  APrevCharIndex := AIndex;
  while (APrevCharIndex >= 0) and (AFormatInfo.FormatString[APrevCharIndex] = '\') do
    Dec(APrevCharIndex);
  if (AIndex - APrevCharIndex) mod 2 = 0 then
    AFormatInfo.AddToRange(AIndex, TdxNumberFormatRangeType.Text)
  else
    AFormatInfo.ResetRangeMerging;
end;

function TdxNumberPatternStateBase.IsDecimalSeparator(ACh: Char; AFormatInfo: TdxNumberFieldFormatInfo): Boolean;
begin
  Result := ACh = '.';
end;

function TdxNumberPatternStateBase.IsGroupSeparator(ACh: Char; AFormatInfo: TdxNumberFieldFormatInfo): Boolean;
begin
  Result := ACh = ',';
end;

{ TdxStartPatternState }

function TdxStartPatternState.GetType: TdxNumberFormatParserState;
begin
  Result := TdxNumberFormatParserState.Start;
end;

function TdxStartPatternState.ProcessCharStateSpecific(AIndex: Integer; AFormatInfo: TdxNumberFieldFormatInfo): Boolean;
var
  ACh: Char;
begin
  ACh := AFormatInfo.FormatString[AIndex];

  if IsDecimalSeparator(ACh, AFormatInfo) then
  begin
    AFormatInfo.AddToRange(AIndex, TdxNumberFormatRangeType.DecimalSeparator);
    ChangeState(TdxNumberFormatParserState.FractionPart);
    Exit(True);
  end;
  if ACh = 'x' then
  begin
    AFormatInfo.AddToRange(AIndex, TdxNumberFormatRangeType.Terminator);
    ChangeState(TdxNumberFormatParserState.IntegerPart);
    Exit(True);
  end;
  if (ACh = '#') or (ACh = '0') then
  begin
    AFormatInfo.AddToRange(AIndex, TdxNumberFormatRangeType.StartNumberMask);
    ChangeState(TdxNumberFormatParserState.IntegerPart);
    Exit(True);
  end;
  if ACh = ';' then
  begin
    if not AFormatInfo.ChangeCurrentPatternToNext then
      ChangeState(TdxNumberFormatParserState.Finish);
    Exit(True);
  end;
  Result := False;
end;

{ TdxNumberMaskStateBase }

function TdxNumberMaskStateBase.ProcessCharStateSpecific(AIndex: Integer; AFormatInfo: TdxNumberFieldFormatInfo): Boolean;
var
  ACh: Char;
begin
  ACh := AFormatInfo.FormatString[AIndex];

  if (ACh = 'x') or (ACh = '#') or (ACh = '0') then
  begin
    AFormatInfo.AddToRange(AIndex, TdxNumberFormatRangeType.NumberMask);
    Exit(True);
  end;
  if ACh = ';' then
  begin
    if AFormatInfo.ChangeCurrentPatternToNext then
      ChangeState(TdxNumberFormatParserState.Start)
    else
      ChangeState(TdxNumberFormatParserState.Finish);
    Exit(True);
  end;
  Result := False;
end;

{ TdxIntegerPartState }

function TdxIntegerPartState.GetType: TdxNumberFormatParserState;
begin
  Result := TdxNumberFormatParserState.IntegerPart;
end;

function TdxIntegerPartState.ProcessCharStateSpecific(AIndex: Integer; AFormatInfo: TdxNumberFieldFormatInfo): Boolean;
begin
  if IsDecimalSeparator(AFormatInfo.FormatString[AIndex], AFormatInfo) then
  begin
    AFormatInfo.AddToRange(AIndex, TdxNumberFormatRangeType.DecimalSeparator);
    ChangeState(TdxNumberFormatParserState.FractionPart);
    Exit(True);
  end;
  Result := inherited ProcessCharStateSpecific(AIndex, AFormatInfo);
end;

{ TdxFractionPartState }

function TdxFractionPartState.GetType: TdxNumberFormatParserState;
begin
  Result := TdxNumberFormatParserState.FractionPart;
end;
{ TdxEmbedTextState }

constructor TdxEmbedTextState.Create(ACalculator: TdxNumberFormatParser; const APrevState: IdxNumberFormatParserStateBase);
begin
  inherited Create(ACalculator);
  FPrevState := APrevState;
end;

function TdxEmbedTextState.GetType: TdxNumberFormatParserState;
begin
  Result := TdxNumberFormatParserState.EmbedText;
end;

procedure TdxEmbedTextState.ProcessChar(AIndex: Integer; AFormatInfo: TdxNumberFieldFormatInfo);
var
  AFormatString: string;
begin
  AFormatString := AFormatInfo.FormatString;
  if AFormatString[AIndex] = '''' then
  begin
    AFormatInfo.ResetRangeMerging;
    ChangeState(FPrevState);
    Exit;
  end;
  if AIndex = Length(AFormatString) then
    TdxFieldFormatter.ThrowUnmatchedQuotesError;
  AFormatInfo.AddToRange(AIndex, TdxNumberFormatRangeType.Text);
end;

{ TdxFinishState }

function TdxFinishState.GetType: TdxNumberFormatParserState;
begin
  Result := TdxNumberFormatParserState.Finish;
end;

procedure TdxFinishState.ProcessChar(AIndex: Integer; AFormatInfo: TdxNumberFieldFormatInfo);
begin
end;

{ TdxIntegerFormatterStateBase }

function TdxIntegerFormatterStateBase.GetGroupSize: Integer;
begin
  Result := Formatter.Culture.NumberFormat.NumberGroupSizes[0];
end;

function TdxIntegerFormatterStateBase.GetGroupSeparator: string;
begin
  if Formatter.CustomSeparators.FieldResultGroupSeparator <> '' then
    Exit(Formatter.CustomSeparators.FieldResultGroupSeparator);
  Result := Formatter.Culture.NumberFormat.NumberGroupSeparator;
end;

procedure TdxIntegerFormatterStateBase.AppendResult(const AStr: string; AResultString: TStringBuilder);
begin
  AResultString.Insert(0, AStr);
end;

procedure TdxIntegerFormatterStateBase.AppendResult(ACh: Char; AResultString: TStringBuilder);
begin
  AResultString.Insert(0, ACh);
end;

procedure TdxIntegerFormatterStateBase.AppendSignIfNeeded(APattern: TdxNumberPattern; AResultString: TStringBuilder);
begin
  if APattern.&Type <> TdxNumberPatternType.Positive then
    Exit;
  if not APattern.HasSign and (Number < 0) then
    AppendResult('-', AResultString);
end;

procedure TdxIntegerFormatterStateBase.AppendGroupSeparator(ADigitCount: Integer; AResultString: TStringBuilder);
begin
  if (ADigitCount = 1) or ((ADigitCount - 1) mod GroupSize <> 0) then
    Exit;
  AppendResult(GroupSeparator, AResultString);
end;

{ TdxIntegerFormatterState }

function TdxIntegerFormatterState.GetCanChangeState: Boolean;
begin
  Result := True;
end;

function TdxIntegerFormatterState.FormatNextDigit(AIterator: TdxFormatRangeIterator; AIndex: Integer;
  const ANumber: string; AResult: TStringBuilder): TdxDigitProcessResult;
var
  AActualIndex: Integer;
  AProcessResult: TdxDigitProcessResult;
begin
  AActualIndex := Length(ANumber) - AIndex + 1;
  AProcessResult := TdxDigitProcessResult.Skip;
  case AIterator.CurrentRange.&Type of
    TdxNumberFormatRangeType.StartNumberMask:
      begin
        AppendRemainedDigits(AActualIndex, ANumber, AIterator.Pattern.HasGroupSeparator, AResult);
        AppendSignIfNeeded(AIterator.Pattern, AResult);
        AProcessResult := TdxDigitProcessResult.&End;
      end;
    TdxNumberFormatRangeType.Terminator:
      begin
        AppendResult(ANumber[AActualIndex], AResult);
        AProcessResult := TdxDigitProcessResult.&End;
      end;
    TdxNumberFormatRangeType.NumberMask:
      begin
        AppendCurrentDigit(AActualIndex, ANumber, AIterator.Pattern.HasGroupSeparator, AResult);
        AProcessResult := TdxDigitProcessResult.Success;
      end;
    TdxNumberFormatRangeType.GroupSeparator:;
    TdxNumberFormatRangeType.Text:
      AppendResult(AIterator.CurrentRange, AResult);
    TdxNumberFormatRangeType.Sign:
      AppendResult(GetNumberSign(AIterator.CurrentCharacter), AResult);
    else
      TdxRichEditExceptions.ThrowInternalException;
  end;
  Result := AProcessResult;
end;

function TdxIntegerFormatterState.GetNextState: IdxNumberFormatterStateBase;
begin
  Result := TdxFinalIntegerFormatterState.Create(Formatter);
end;

procedure TdxIntegerFormatterState.AppendRemainedDigits(AIndex: Integer; const ANumber: string; ASeparateGroups: Boolean; AResult: TStringBuilder);
var
  I: Integer;
begin
  for I := AIndex downto 1 do
    AppendCurrentDigit(I, ANumber, ASeparateGroups, AResult);
end;

procedure TdxIntegerFormatterState.AppendCurrentDigit(AIndex: Integer; const ANumber: string; ASeparateGroups: Boolean; AResult: TStringBuilder);
var
  ADigitCount: Integer;
begin
  AppendResult(ANumber[AIndex], AResult);
  if ASeparateGroups and (AIndex <> 1) then
  begin
    ADigitCount := Length(ANumber) - AIndex + 1 + 1;
    AppendGroupSeparator(ADigitCount, AResult);
  end;
end;

{ TdxFinalIntegerFormatterState }

function TdxFinalIntegerFormatterState.GetCanChangeState: Boolean;
begin
  Result := False;
end;

function TdxFinalIntegerFormatterState.FormatNextDigit(AIterator: TdxFormatRangeIterator; AIndex: Integer; const ANumber: string; AResult: TStringBuilder): TdxDigitProcessResult;
var
  AProcessResult: TdxDigitProcessResult;
  ACh: Char;
begin
  AProcessResult := TdxDigitProcessResult.Skip;
  ACh := AIterator.CurrentCharacter;
  case AIterator.CurrentRange.&Type of
    TdxNumberFormatRangeType.StartNumberMask:
      begin
        AppendResult(GetPlaceholder(ACh), AResult);
        AppendSignIfNeeded(AIterator.Pattern, AResult);
      end;
    TdxNumberFormatRangeType.NumberMask:
      begin
        if (ACh = '0') and AIterator.Pattern.HasGroupSeparator then
          AppendGroupSeparator(AIndex, AResult);
        AppendResult(GetPlaceholder(ACh), AResult);
        AProcessResult := TdxDigitProcessResult.Success;
      end;
    TdxNumberFormatRangeType.Terminator:
      AppendResult(' ', AResult);
    TdxNumberFormatRangeType.GroupSeparator:;
    TdxNumberFormatRangeType.Text:
      AppendResult(AIterator.CurrentRange, AResult);
    TdxNumberFormatRangeType.Sign:
      AppendResult(GetNumberSign(ACh), AResult);
    else
      TdxRichEditExceptions.ThrowInternalException;
  end;
  Result := AProcessResult;
end;

function TdxFinalIntegerFormatterState.GetNextState: IdxNumberFormatterStateBase;
begin
  Result := nil;
end;

{ TdxFractionFormatterStateBase }

function TdxFractionFormatterStateBase.GetDecimalSeparator: Char;
begin
  if Formatter.CustomSeparators.FieldResultDecimalSeparator <> '' then
    Exit(Formatter.CustomSeparators.FieldResultDecimalSeparator[1]);
  Result := Formatter.Culture.NumberFormat.NumberDecimalSeparator;
end;

procedure TdxFractionFormatterStateBase.AppendResult(ACh: Char; AResultString: TStringBuilder);
begin
  AResultString.Append(ACh);
end;

procedure TdxFractionFormatterStateBase.AppendResult(const AStr: string; AResultString: TStringBuilder);
begin
  AResultString.Append(AStr);
end;

{ TdxFractionalFormatterState }

function TdxFractionalFormatterState.GetCanChangeState: Boolean;
begin
  Result := True;
end;

function TdxFractionalFormatterState.FormatNextDigit(AIterator: TdxFormatRangeIterator; AIndex: Integer; const ANumber: string; AResult: TStringBuilder): TdxDigitProcessResult;
var
  AProcessResult: TdxDigitProcessResult;
begin
  AProcessResult := TdxDigitProcessResult.Skip;
  case AIterator.CurrentRange.&Type of
    TdxNumberFormatRangeType.Terminator,
    TdxNumberFormatRangeType.NumberMask:
      begin
        AppendResult(ANumber[AIndex], AResult);
        AProcessResult := TdxDigitProcessResult.Success;
      end;
    TdxNumberFormatRangeType.DecimalSeparator:
      AppendResult(DecimalSeparator, AResult);
    TdxNumberFormatRangeType.GroupSeparator:;
    TdxNumberFormatRangeType.Text:
      AppendResult(AIterator.CurrentRange, AResult);
    TdxNumberFormatRangeType.Sign:
      AppendResult(GetNumberSign(AIterator.CurrentCharacter), AResult);
    else
      TdxRichEditExceptions.ThrowInternalException;
  end;
  Result := AProcessResult;
end;

function TdxFractionalFormatterState.GetNextState: IdxNumberFormatterStateBase;
begin
  Result := TdxFinalFractionalFormatterState.Create(Formatter);
end;

{ TdxFinalFractionalFormatterState }

function TdxFinalFractionalFormatterState.GetCanChangeState: Boolean;
begin
  Result := False;
end;

function TdxFinalFractionalFormatterState.FormatNextDigit(AIterator: TdxFormatRangeIterator; AIndex: Integer; const ANumber: string; AResult: TStringBuilder): TdxDigitProcessResult;
var
  AProcessResult: TdxDigitProcessResult;
  ACh: Char;
begin
  AProcessResult := TdxDigitProcessResult.Skip;
  ACh := AIterator.CurrentCharacter;
  case AIterator.CurrentRange.&Type of
    TdxNumberFormatRangeType.NumberMask:
      if ACh <> 'x' then
        AppendResult(GetPlaceholder(ACh), AResult);
    TdxNumberFormatRangeType.DecimalSeparator:
      AppendResult(DecimalSeparator, AResult);
    TdxNumberFormatRangeType.GroupSeparator:;
    TdxNumberFormatRangeType.Terminator:;
    TdxNumberFormatRangeType.Text:
      AppendResult(AIterator.CurrentRange, AResult);
    TdxNumberFormatRangeType.Sign:
      AppendResult(GetNumberSign(ACh), AResult);
    else
      TdxRichEditExceptions.ThrowInternalException;
  end;
  Result := AProcessResult;
end;

function TdxFinalFractionalFormatterState.GetNextState: IdxNumberFormatterStateBase;
begin
  Result := nil;
end;

{ TdxNumberFormatRange }

constructor TdxNumberFormatRange.Create(AStartIndex: Integer = -1; AEndIndex: Integer  = -1;
  AType: TdxNumberFormatRangeType = TdxNumberFormatRangeType.None);
begin
  inherited Create;
  FStartIndex := AStartIndex;
  FEndIndex := AEndIndex;
  FType := AType;
end;

constructor TdxNumberFormatRange.Create(AIndex: Integer; AType: TdxNumberFormatRangeType);
begin
  Create(AIndex, AIndex, AType);
end;

constructor TdxNumberFormatRange.Create(AType: TdxNumberFormatRangeType);
begin
  Create(-1, -1, AType);
end;

procedure TdxNumberFormatRange.SetEndIndex(const AValue: Integer);
begin
  if FEndIndex < FStartIndex then
    TdxRichEditExceptions.ThrowInternalException;
  FEndIndex := AValue;
end;

function TdxNumberFormatRange.GetLength: Integer;
begin
  Result := FEndIndex - FStartIndex + 1;
end;

function TdxNumberFormatRange.GetText(const AFormat: string): string;
begin
  Result := Copy(AFormat, StartIndex + 1, Length);
end;

{ TdxNumericFieldFormatter }

constructor TdxNumericFieldFormatter.Create;
begin
  inherited Create;
  FCustomSeparators := TdxMailMergeCustomSeparators.Create;
end;

destructor TdxNumericFieldFormatter.Destroy;
begin
  FreeAndNil(FCustomSeparators);
  inherited Destroy;
end;

function TdxNumericFieldFormatter.GetDecimalSeparator: Char;
begin
  Result := NumberFormatInfo.DecimalSeparator;
end;

function TdxNumericFieldFormatter.GetFormatString: string;
begin
  Result := NumberFormatInfo.FormatString;
end;

function TdxNumericFieldFormatter.InternalFormat(const AValue: Double; const AFormat: string): string;
var
  AResult: TStringBuilder;
begin
  AResult := TStringBuilder.Create;
  try
    try
      BeginFormat(AValue, AFormat);
      Format(AResult);
    finally
      EndFormat;
    end;
    Result := TdxStringHelper.TrimEnd(AResult.ToString, [' ']);
  finally
    AResult.Free;
  end;
end;

function TdxNumericFieldFormatter.FormatByDefault(const AValue: Double): string;
begin
  Result := FloatToStr(AValue, Culture.FormatSettings);
end;

procedure TdxNumericFieldFormatter.BeginFormat(const ANumber: Double; const AFormat: string);
var
  AFormatString: string;
begin
  FNumber := ANumber;
  if Length(AFormat) > MaxFormatStringLength then
    AFormatString := TdxStringHelper.Substring(AFormat, 0, MaxFormatStringLength)
  else
    AFormatString := AFormat;
  FFormatInfo := TdxNumberFieldFormatInfo.Create(AFormatString, Culture.NumberFormat);
  FFormatInfo.CustomSeparators.Assign(CustomSeparators);
  PopulateNumberFormatInfo(FFormatInfo);
end;

procedure TdxNumericFieldFormatter.EndFormat;
begin
  FNumber := 0;
  FreeAndNil(FFormatInfo);
end;

procedure TdxNumericFieldFormatter.PopulateNumberFormatInfo(AFormatInfo: TdxNumberFieldFormatInfo);
var
  AParser: TdxNumberFormatParser;
begin
  AParser := TdxNumberFormatParser.Create;
  try
    AParser.Parse(AFormatInfo);
  finally
    AParser.Free;
  end;
end;

procedure TdxNumericFieldFormatter.Format(AResult: TStringBuilder);
var
  APattern: TdxNumberPattern;
  ARoundValue: Integer;
  AActualNumber: Double;
begin
  APattern := NumberFormatInfo.GetPattern(Number);
  if APattern.IsEmpty then
    Exit;

  ARoundValue := APattern.GetFractionLength;
  AActualNumber := GetActualNumber(ARoundValue);
  FormatIntegerPart(AActualNumber, APattern, AResult);
  FormatFractionalPart(AActualNumber, APattern, AResult);
end;

function InternalRoundTo(const AValue: Double; const ADigit: TRoundToRange = -2): Double;
const
  DoubleResolution = 1000 * 1E-15;
var
  AFactor: Double;
  AOriginRoundingMode: {$IFDEF DELPHIXE2}TRoundingMode{$ELSE}TFPURoundingMode{$ENDIF};
begin
  AOriginRoundingMode := SetRoundMode(rmNearest);
  try
    AFactor := IntPower(10.0, ADigit);
    if AValue < 0 then
      Result := Int((AValue / AFactor) - (0.5 - DoubleResolution)) * AFactor
    else
      Result := Int((AValue / AFactor) + (0.5 - DoubleResolution)) * AFactor;
  finally
    SetRoundMode(AOriginRoundingMode);
  end;
end;

function TdxNumericFieldFormatter.GetActualNumber(ARoundValue: Integer): Double;
var
  ADecimals: Integer;
begin
  ADecimals := Min(MaxFractionLength, ARoundValue);
  Result := InternalRoundTo(Number, -ADecimals);
end;

function TdxNumericFieldFormatter.GetNumberString(ANumber: Double): string;
var
  ACulture: TdxCultureInfo;
  AFormatSettings: TFormatSettings;
begin
  if NumberFormatInfo.CustomSeparators.MaskDecimalSeparator = '' then
    Exit(FloatToStr(ANumber, Culture.FormatSettings));
  ACulture := Culture.Clone;
  AFormatSettings := ACulture.NumberFormat.FormatSettings;
  AFormatSettings.DecimalSeparator := NumberFormatInfo.CustomSeparators.MaskDecimalSeparator[1];
  Result := FloatToStr(ANumber, AFormatSettings);
end;

procedure TdxNumericFieldFormatter.FormatIntegerPart(ANumber: Double; APattern: TdxNumberPattern; AResult: TStringBuilder);
var
  ASeparatorIndex, ARangeIndex: Integer;
  AIterator: TdxFormatRangeIteratorBackward;
begin
  ASeparatorIndex := APattern.GetSeparatorIndex;
  if (ASeparatorIndex < 0) then
    ARangeIndex := APattern.Ranges.Count - 1
  else
    ARangeIndex := ASeparatorIndex - 1;
  AIterator := TdxFormatRangeIteratorBackward.Create(APattern, ARangeIndex, FormatString);
  try
    FormatIntegerPartCore(ANumber, AIterator, AResult);
  finally
    AIterator.Free;
  end;
end;

procedure TdxNumericFieldFormatter.FormatIntegerPartCore(ANumber: Double; AIterator: TdxFormatRangeIteratorBackward; AResult: TStringBuilder);
var
  AIntegerPart: Double;
  ANumberString: string;
begin
  FState := TdxIntegerFormatterState.Create(Self);

  AIntegerPart := Int(ANumber);

  if AIntegerPart <> 0.0 then
    ANumberString := GetNumberString(Abs(AIntegerPart))
  else
    ANumberString := '';
  FormatNumberPart(AIterator, ANumberString, AResult);
end;

procedure TdxNumericFieldFormatter.FormatFractionalPart(ANumber: Double; APattern: TdxNumberPattern; AResult: TStringBuilder);
var
  ASeparatorIndex: Integer;
  AIterator: TdxFormatRangeIteratorForward;
begin
  ASeparatorIndex := APattern.GetSeparatorIndex;
  if ASeparatorIndex < 0 then
    Exit;
  AIterator := TdxFormatRangeIteratorForward.Create(APattern, ASeparatorIndex, FormatString);
  try
    FormatFractionalPartCore(ANumber, AResult, AIterator);
  finally
    AIterator.Free;
  end;
end;

procedure TdxNumericFieldFormatter.FormatFractionalPartCore(ANumber: Double; AResult: TStringBuilder; AIterator: TdxFormatRangeIteratorForward);
var
  ANumberString: string;
  ANumberParts: TArray<string>;
begin
  FState := TdxFractionalFormatterState.Create(Self);
  ANumberString := GetNumberString(ANumber);
  ANumberParts := TdxStringHelper.Split(ANumberString, [DecimalSeparator]);
  if Length(ANumberParts) > 1 then
    ANumberString := ANumberParts[1]
  else
    ANumberString := '';
  FormatNumberPart(AIterator, ANumberString, AResult);
end;

procedure TdxNumericFieldFormatter.FormatNumberPart(AIterator: TdxFormatRangeIterator; const ANumberString: string; AResult: TStringBuilder);
var
  ADigitIndex: Integer;
  AProcessResult: TdxDigitProcessResult;
begin
  ADigitIndex := 1;
  while AIterator.MoveNext do
  begin
    if (ADigitIndex > Length(ANumberString)) and FState.CanChangeState then
      FState := FState.GetNextState;
    AProcessResult := FState.FormatNextDigit(AIterator, ADigitIndex, ANumberString, AResult);
    if AProcessResult = TdxDigitProcessResult.&End then
      FState := FState.GetNextState
    else
      if AProcessResult = TdxDigitProcessResult.Success then
        Inc(ADigitIndex);
  end;
end;

{ TdxNumberFieldFormatInfo }

constructor TdxNumberFieldFormatInfo.Create(const AFormatString: string; const AFormatInfo: TdxNumberFormatInfo);
begin
  inherited Create;
  FCustomSeparators := TdxMailMergeCustomSeparators.Create;
  if AFormatString = '' then
    TdxRichEditExceptions.ThrowArgumentException('formatString', AFormatString);
  FFormatString := AFormatString;
  FFormatInfo := AFormatInfo;
  FPositivePattern := TdxNumberPattern.Create(TdxNumberPatternType.Positive);
  FNegativePattern := TdxNumberPattern.Create(TdxNumberPatternType.Negative);
  FZeroPattern := TdxNumberPattern.Create(TdxNumberPatternType.Zero);
  FCurrentPattern := FPositivePattern;
end;

destructor TdxNumberFieldFormatInfo.Destroy;
begin
  FreeAndNil(FPositivePattern);
  FreeAndNil(FNegativePattern);
  FreeAndNil(FZeroPattern);
  FreeAndNil(FCustomSeparators);
  inherited Destroy;
end;

function TdxNumberFieldFormatInfo.GetDecimalSeparator: Char;
begin
  if CustomSeparators.MaskDecimalSeparator <> '' then
    Exit(CustomSeparators.MaskDecimalSeparator[1]);
  Result := NumberFormat.NumberDecimalSeparator;
end;

function TdxNumberFieldFormatInfo.GetGroupSeparator: Char;
begin
  if CustomSeparators.MaskGroupSeparator <> '' then
    Exit(CustomSeparators.MaskGroupSeparator[1]);
  Result := NumberFormat.NumberGroupSeparator;
end;

function TdxNumberFieldFormatInfo.GetGroupSize: Integer;
begin
  Result := NumberFormat.NumberGroupSizes[0];
end;

function TdxNumberFieldFormatInfo.GetNumberFormat: TdxNumberFormatInfo;
begin
  Result := FFormatInfo;
end;

function TdxNumberFieldFormatInfo.GetPattern(AValue: Double): TdxNumberPattern;
begin
  if (AValue < 0) and not NegativePattern.IsEmpty then
    Exit(NegativePattern);
  if (AValue = 0) and not ZeroPattern.IsEmpty then
    Exit(ZeroPattern);
  Result := PositivePattern;
end;

procedure TdxNumberFieldFormatInfo.AddToRange(ACharIndex: Integer; ARangeType: TdxNumberFormatRangeType);
begin
  if (FCurrentRange <> nil) and (FCurrentRange.&Type = ARangeType) then
    FCurrentRange.EndIndex := ACharIndex - 1
  else
  begin
    FCurrentRange := TdxNumberFormatRange.Create(ACharIndex - 1, ARangeType);
    FCurrentPattern.Ranges.Add(FCurrentRange);
  end;
end;

function TdxNumberFieldFormatInfo.ChangeCurrentPatternToNext: Boolean;
begin
  ResetRangeMerging;

  if FCurrentPattern.&Type = TdxNumberPatternType.Positive then
  begin
    FCurrentPattern := NegativePattern;
    Exit(True);
  end;
  if FCurrentPattern.&Type = TdxNumberPatternType.Negative then
  begin
    FCurrentPattern := ZeroPattern;
    Exit(True);
  end;
  Result := False;
end;

procedure TdxNumberFieldFormatInfo.ResetRangeMerging;
begin
  FCurrentRange := nil;
end;

{ TdxNumberFormatterStateBase }

constructor TdxNumberFormatterStateBase.Create(AFormatter: TdxNumericFieldFormatter);
begin
  inherited Create;
  FFormatter := AFormatter;
end;

function TdxNumberFormatterStateBase.GetNumber: Double;
begin
  Result := Formatter.Number;
end;

procedure TdxNumberFormatterStateBase.AppendResult(ARange: TdxNumberFormatRange; AResultString: TStringBuilder);
begin
  AppendResult(ARange.GetText(Formatter.FormatString), AResultString);
end;

function TdxNumberFormatterStateBase.GetNumberSign(APatternChar: Char): Char;
begin
  if ((Number > 0) and (APatternChar = '-')) or (Number = 0) then
    Exit(' ')
  else
    if Number > 0 then
      Exit('+');
  Result := '-';
end;

function TdxNumberFormatterStateBase.GetPlaceholder(APatternChar: Char): Char;
begin
  if (APatternChar = '#') or (APatternChar = 'x') then
    Exit(' ');
  Result := '0';
end;

{ TdxNumberPattern }

constructor TdxNumberPattern.Create(AType: TdxNumberPatternType);
begin
  inherited Create;
  FType := AType;
  FRanges := TdxNumberFormatRangeCollection.Create;
end;

destructor TdxNumberPattern.Destroy;
begin
  FreeAndNil(FRanges);
  inherited Destroy;
end;

function TdxNumberPattern.GetIsEmpty: Boolean;
begin
  Result := Ranges.Count = 0;
end;

function TdxNumberPattern.GetHasSign: Boolean;
var
  ARange: TdxNumberFormatRange;
  I: Integer;
begin
  for I := 0 to Ranges.Count - 1 do
  begin
    ARange := Ranges[I];
    if ARange.&Type = TdxNumberFormatRangeType.Sign then
      Exit(True);
  end;
  Result := False;
end;

function TdxNumberPattern.GetHasGroupSeparator: Boolean;
var
  ARange: TdxNumberFormatRange;
  I: Integer;
begin
  for I := 0 to Ranges.Count - 1 do
  begin
    ARange := Ranges[I];
    if ARange.&Type = TdxNumberFormatRangeType.GroupSeparator then
      Exit(True);
  end;
  Result := False;
end;

function TdxNumberPattern.GetFractionLength: Integer;
var
  AResult, I: Integer;
  ARange: TdxNumberFormatRange;
begin
  AResult := 0;
  for I := FRanges.Count - 1 downto 0 do
  begin
    ARange := FRanges[I];
    if ARange.&Type = TdxNumberFormatRangeType.DecimalSeparator then
      Exit(AResult);
    if ARange.&Type = TdxNumberFormatRangeType.NumberMask then
      Inc(AResult, ARange.Length);
  end;
  Result := 0;
end;

function TdxNumberPattern.GetSeparatorIndex: Integer;
var
  ACount, I: Integer;
  ARange: TdxNumberFormatRange;
begin
  ACount := Ranges.Count;
  for I := 0 to ACount - 1 do
  begin
    ARange := Ranges[I];
    if ARange.&Type = TdxNumberFormatRangeType.DecimalSeparator then
      Exit(I);
  end;
  Result := -1;
end;

{ TdxFormatRangeIterator }

constructor TdxFormatRangeIterator.Create(APattern: TdxNumberPattern; ARangeIndex: Integer; const AFormat: string);
begin
  inherited Create;
  FCurrentRangeIndex := -1;
  FCurrentCharacterIndex := -1;
  FCachedRangeIndex := -1;
  FCachedRangeText := '';
  FPattern := APattern;
  FCurrentRangeIndex := ARangeIndex;
  if AFormat = '' then
    TdxRichEditExceptions.ThrowArgumentException('format', AFormat);
  FFormat := AFormat;
end;

function TdxFormatRangeIterator.GetCurrentRange: TdxNumberFormatRange;
begin
  if (FCurrentRangeIndex >= 0) and (FCurrentRangeIndex < Ranges.Count) then
    Exit(Ranges[FCurrentRangeIndex]);
  Result := nil;
end;

function TdxFormatRangeIterator.GetInternalCurrentCharacter: Char;
begin
  if FCachedRangeIndex <> FCurrentRangeIndex then
  begin
    FCachedRangeIndex := FCurrentRangeIndex;
    FCachedRangeText := CurrentRange.GetText(FormatString);
  end;
  Result := GetCurrentCharacter;
end;

function TdxFormatRangeIterator.GetRanges: TdxNumberFormatRangeCollection;
begin
  Result := Pattern.Ranges;
end;

function TdxFormatRangeIterator.MoveNext: Boolean;
begin
  if CurrentRange = nil then
    Exit(False);

  if (FCurrentCharacterIndex >= 0) and IsTextRange(CurrentRange) then
  begin
    FCurrentCharacterIndex := FCurrentCharacterIndex + CurrentRange.Length;
    Exit(MoveRangeToNext);
  end;
  Inc(FCurrentCharacterIndex);
  if IsCharIndexOutOfCurrentRange(FCurrentCharacterIndex) then
    Exit(MoveRangeToNext)
  else
    Exit(True);
end;

function TdxFormatRangeIterator.IsTextRange(ARange: TdxNumberFormatRange): Boolean;
begin
  Result := ARange.&Type = TdxNumberFormatRangeType.Text;
end;

function TdxFormatRangeIterator.IsCharIndexOutOfCurrentRange(ACharIndex: Integer): Boolean;
begin
  Result := ACharIndex >= FPreviousRangesLength + CurrentRange.Length;
end;

function TdxFormatRangeIterator.MoveRangeToNext: Boolean;
begin
  FPreviousRangesLength := FPreviousRangesLength + CurrentRange.Length;
  MoveRangeIndexToNext;
  if (CurrentRange <> nil) then
    Result := True
  else
    Result := False;
end;

function TdxFormatRangeIterator.GetCurrentCharacter: Char;
var
  ALogicalCharIndex: Integer;
begin
  ALogicalCharIndex := GetLogicalCharIndex;
  Result := FCachedRangeText[ALogicalCharIndex];
end;

function TdxFormatRangeIterator.GetActualCharIndex: Integer;
begin
  Result := FCurrentCharacterIndex - FPreviousRangesLength + 1;
end;

{ TdxFormatRangeIteratorBackward }

function TdxFormatRangeIteratorBackward.GetLogicalCharIndex: Integer;
var
  AActualCharIndex: Integer;
begin
  AActualCharIndex := GetActualCharIndex;
  Result := CurrentRange.Length - AActualCharIndex + 1;
end;

procedure TdxFormatRangeIteratorBackward.MoveRangeIndexToNext;
begin
  CurrentRangeIndex := CurrentRangeIndex - 1;
end;

{ TdxFormatRangeIteratorForward }

function TdxFormatRangeIteratorForward.GetLogicalCharIndex: Integer;
begin
  Result := GetActualCharIndex;
end;

procedure TdxFormatRangeIteratorForward.MoveRangeIndexToNext;
begin
  CurrentRangeIndex := CurrentRangeIndex + 1;
end;

end.
