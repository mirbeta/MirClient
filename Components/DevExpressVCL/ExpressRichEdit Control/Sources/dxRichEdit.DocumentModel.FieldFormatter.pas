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

unit dxRichEdit.DocumentModel.FieldFormatter;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Generics.Defaults, Generics.Collections, Rtti,
  dxCore, dxCoreClasses, dxCultureInfo, cxFormats,
  dxRichEdit.Utils.Types,
  dxGenerics,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.NumberingFormatting,
  dxRichEdit.NumberConverters;

type
  TdxGeneralNumberFormatter = class;
  TdxGeneralStringFormatter = class;
  TdxGeneralDocumentModelStringFormatter = class;

  { TdxFieldFormatter }

  TdxFieldFormatter = class abstract
  strict private
    type
      TCultureUpdater = class (TcxIUnknownObject, IcxFormatControllerListener)
      public
        constructor Create;
        destructor Destroy; override;
        procedure FormatChanged;
      end;
  strict private
    class var
      FCultureUpdater: TCultureUpdater;
      FCulture: TdxCultureInfo;
    class destructor Finalize;
  protected
    function GetCulture: TdxCultureInfo; virtual;
    class procedure ThrowException(const AMessage: TcxResourceStringID); overload; static;
    class procedure ThrowException(const AMessage: TcxResourceStringID; const AArgument: array of const); overload; static;
    class procedure UpdateCurrentCulture; static;
  public
    constructor Create;
    class procedure ThrowIncorrectNumericFieldFormatError; static;
    class procedure ThrowSyntaxError(const AIncorrectFormat: string); overload; static;
    class procedure ThrowSyntaxError(AIncorrectChar: Char); overload; static;
    class procedure ThrowUnmatchedQuotesError; static;
    class procedure ThrowUnknownSwitchArgumentError; static;
    class procedure ThrowUnexpectedEndOfFormulaError; static;
    class procedure ThrowMissingOperatorError; static;
    class procedure ThrowZeroDivideError; static;

    property Culture: TdxCultureInfo read GetCulture;
  end;

  { TdxSpecificFieldFormatter<T> }

  TdxSpecificFieldFormatter<T> = class(TdxFieldFormatter)
  protected
    function FormatByDefault(const AValue: T): string; virtual; abstract;
    function InternalFormat(const AValue: T; const AFormat: string): string; virtual; abstract;
  public
    function Format(const AValue: T; const AFormat: string; AHasFormat: Boolean): string;
  end;

  { TdxGeneralFieldFormatter }

  TdxGeneralFieldFormatter = class(TdxFieldFormatter)
  strict private
    FNumberFormatter: TdxGeneralNumberFormatter;
    FStringFormatter: TdxGeneralStringFormatter;
    FDocumentModelStringFormatter: TdxGeneralDocumentModelStringFormatter;
  protected
    procedure BeginFormat; virtual;
    procedure EndFormat; virtual;
    function FormatCore(const AValue: TValue; const AFormattedValue, AKeyword: string): string; virtual;
    function FormatAsNumber(const AValue: TValue; const AFormattedValue, AKeyword: string): string; virtual;
    function FormatAsString(const AValue: TValue; const AFormattedValue, AKeyword: string): string; virtual;
    function TryGetValue(const ANotation: string; out AResult: Double): Boolean; virtual;
  public
    function Format(const AValue: TValue; const AFormattedValue: string; const AKeyword: string): string; virtual;
    function IsGeneralDocumentModelStringFormatter(const AKeyword: string): Boolean; virtual;

    // for internal use
    procedure FormatPieceTable(APieceTable: TdxCustomPieceTable{TdxPieceTable}; const AKeyword: string); virtual;
  end;

  { TdxGeneralFormatterBase }

  TdxFormatKeywordHandler = reference to function(const Value: TValue; const ACulture: TdxCultureInfo): string;
  TdxFormatKeywordTable = class(TdxNamedDelegateDictionary<TdxFormatKeywordHandler>);

  TdxGeneralFormatterBase = class abstract
  strict private
    FCulture: TdxCultureInfo;
  protected
    class function GetUpperCaseString(const AStr: string; const ACulture: TdxCultureInfo): string; static;
    class function GetLowerCaseString(const AStr: string; const ACulture: TdxCultureInfo): string; static;

    class function GetKeywords: TdxFormatKeywordTable; virtual;
    function GetCaseSensitiveKeyword(const AKeyword: string): string;

    property Culture: TdxCultureInfo read FCulture;
  public
    constructor Create(const ACulture: TdxCultureInfo);
    function Format(const AValue: TValue; const AKeyword: string): string; virtual;
    class function ContainsKeyword(const AKeyword: string; const ACulture: TdxCultureInfo): Boolean; virtual;
  end;

  { TdxGeneralNumberFormatter }

  TdxGeneralNumberFormatter = class(TdxGeneralFormatterBase)
  strict private
    class var
      FNumberFormatKeywords: TdxFormatKeywordTable;
  strict private
    class constructor Initialize;
    class destructor Finalize;
    class function CreateNumberFormatKeywordTable: TdxFormatKeywordTable; static;
    class function ConvertToUppercaseAlphabeticLatinCharacters(const AValue: TValue; const ACulture: TdxCultureInfo): string; static;
    class function ConvertToLowercaseAlphabeticLatinCharacters(const AValue: TValue; const ACulture: TdxCultureInfo): string; static;
    class function ValidateValueForConvertingToLatinChars(const AValue: TValue): Integer; static;
    class function ConvertToArabicCardinalNumerals(const AValue: TValue; const ACulture: TdxCultureInfo): string; static;
    class function ConvertToArabicCardinalNumeralsWithDashes(const AValue: TValue; const ACulture: TdxCultureInfo): string; static;
    class function ConvertToLowercaseCardinalText(const AValue: TValue; const ACulture: TdxCultureInfo): string; static;
    class function ConvertToLowercaseOrdinalText(const AValue: TValue; const ACulture: TdxCultureInfo): string; static;
    class function ConvertToLowercaseText(const AValue: TValue; AConverter: TdxDescriptiveNumberConverterBase; const ACulture: TdxCultureInfo): string; static;
    class function GetDescriptiveCardinalNumberConverter(const ACultureName: string): TdxDescriptiveNumberConverterBase; static;
    class function GetDescriptiveOrdinalNumberConverter(const ACultureName: string): TdxDescriptiveNumberConverterBase; static;
    class function ConvertToOrdinalNumber(const AValue: TValue; const ACulture: TdxCultureInfo): string; static;
    class function GetOrdinalNumberConverter(const ACultureName: string): TdxOrdinalBasedNumberConverter; static;
    class function ConvertToNumberEnclosedInCircle(const AValue: TValue; const ACulture: TdxCultureInfo): string; static;
    class function ConvertToNumberFollowedByPeriod(const AValue: TValue; const ACulture: TdxCultureInfo): string; static;
    class function ConvertToNumberEnclosedInBrackets(const AValue: TValue; const ACulture: TdxCultureInfo): string; static;
    class function ConvertToIdeographsEnclosedInBrackets(const AValue: TValue; const ACulture: TdxCultureInfo): string; static;
    class function ConvertToNumericalTraditionalIdeographs(const AValue: TValue; const ACulture: TdxCultureInfo): string; static;
    class function ConvertToNumericalZodiacIdeographs(const AValue: TValue; const ACulture: TdxCultureInfo): string; static;
    class function ConvertToNumericalTraditionalZodiacIdeographs(const AValue: TValue; const ACulture: TdxCultureInfo): string; static;
    class function ConvertNumberToCharacter(const AValue: TValue; AConverter: TdxNumberToSingleCharConverter; const ACulture: TdxCultureInfo): string; static;
    class function ConvertToDoubleByteArabicNumber(const AValue: TValue; const ACulture: TdxCultureInfo): string; static;
    class function ConvertToSequentialDigitalIdeographs(const AValue: TValue; const ACulture: TdxCultureInfo): string; static;
    class function ConvertToDollarText(const AValue: TValue; const ACulture: TdxCultureInfo): string; static;
    class function GetDollarTextConverter(const ACultureName: string): TdxNumberToDollarTextConverter; static;
    class function ConvertToNumberEnclosedInCircleTruncated(const AValue: TValue; const ACulture: TdxCultureInfo): string; static;
    class function ConvertToHexadecimalNumber(const AValue: TValue; const ACulture: TdxCultureInfo): string; static;
    class function ConvertToUppercaseRomanNumber(const AValue: TValue; const ACulture: TdxCultureInfo): string; static;
    class function ConvertToLowercaseRomanNumber(const AValue: TValue; const ACulture: TdxCultureInfo): string; static;
    class function ConvertToArabicNumber(const AValue: TValue; const ACulture: TdxCultureInfo): string; static;
    class function ConvertToInteger(const AValue: TValue): Integer; static;
    class function ConvertToShortInteger(const AValue: TValue): Integer; static;
    class function GetNumberString(const AValue: TValue; const ACulture: TdxCultureInfo): string; overload; static;
    class function GetNumberString(const AValue: Integer; const ACulture: TdxCultureInfo): string; overload; static;
    class function GetFormattedNumberString(const AFormat: string; AValue: Integer; const ACulture: TdxCultureInfo): string; static;
  protected
    class function GetKeywords: TdxFormatKeywordTable; override;
  end;

  { TdxGeneralStringFormatter }

  TdxGeneralStringFormatter = class(TdxGeneralFormatterBase)
  strict private
    class var
      FStringFormatKeywords: TdxFormatKeywordTable;
  strict private
    class constructor Initialize;
    class destructor Finalize;
    class function CreateStringFormatKeywordsTable: TdxFormatKeywordTable; static;
    class function CapitalizesFirstLetterOfEachWord(const AValue: TValue; const ACulture: TdxCultureInfo): string; static;
    class function SkipCharacters(AIndex: Integer; AStr: TStringBuilder; const APredicate: TdxPredicate<Char>): Integer; static;
    class function IsLetterOrDigit(const ACh: Char): Boolean; static;
    class function IsNotLetterOrDigit(const ACh: Char): Boolean; static;
    class function CapitalizesFirstLetterOfFirstWord(const AValue: TValue; const ACulture: TdxCultureInfo): string; static;
    class function ConvertToLowercaseString(const AValue: TValue; const ACulture: TdxCultureInfo): string; static;
    class function ConvertToUppercaseString(const AValue: TValue; const ACulture: TdxCultureInfo): string; static;
  protected
    class function GetKeywords: TdxFormatKeywordTable; override;
  end;

  { TdxGeneralDocumentModelStringFormatter }

  TdxGeneralDocumentModelStringFormatter = class(TdxGeneralFormatterBase)
  strict private
    class var
      FStringFormatKeywords: TdxFormatKeywordTable;
  strict private
    class constructor Initialize;
    class destructor Finalize;
    class function CreateStringFormatKeywordsTable: TdxFormatKeywordTable; static;
    class function CapitalizesFirstLetterOfEachWord(const AValue: TValue; const ACulture: TdxCultureInfo): string; static;
    class function CapitalizesFirstLetterOfFirstWord(const AValue: TValue; const ACulture: TdxCultureInfo): string; static;
    class function ConvertToLowercaseString(const AValue: TValue; const ACulture: TdxCultureInfo): string; static;
    class function ConvertToUppercaseString(const AValue: TValue; const ACulture: TdxCultureInfo): string; static;
  protected
    class function GetKeywords: TdxFormatKeywordTable; override;
  end;

  { TdxNumberToEnclosedCircleNumberConverter }

  TdxNumberToEnclosedCircleNumberConverter = class(TdxNumberToSingleCharConverter)
  protected
    function CreateAlphanumericCharsTable: TArray<Char>; override;
  end;

  { TdxNumberToNumberFollowedByPeriodConverter }

  TdxNumberToNumberFollowedByPeriodConverter = class(TdxNumberToSingleCharConverter)
  protected
    function CreateAlphanumericCharsTable: TArray<Char>; override;
  end;

  { TdxNumberToEnclosedBracketsNumberConverter }

  TdxNumberToEnclosedBracketsNumberConverter = class(TdxNumberToSingleCharConverter)
  protected
    function CreateAlphanumericCharsTable: TArray<Char>; override;
  end;

  { TdxNumberToIdeographsEnclosedBracketsConverter }

  TdxNumberToIdeographsEnclosedBracketsConverter = class(TdxNumberToSingleCharConverter)
  protected
    function CreateAlphanumericCharsTable: TArray<Char>; override;
  end;

  { TdxNumberToTraditionalIdeographsConverter }

  TdxNumberToTraditionalIdeographsConverter = class(TdxNumberToSingleCharConverter)
  protected
    function CreateAlphanumericCharsTable: TArray<Char>; override;
  end;

  { TdxNumberToZodiacIdeographsConverter }

  TdxNumberToZodiacIdeographsConverter = class(TdxNumberToSingleCharConverter)
  protected
    function CreateAlphanumericCharsTable: TArray<Char>; override;
  end;

  { TdxNumberToTraditionalZodiacIdeographsConverter }

  TdxNumberToTraditionalZodiacIdeographsConverter = class
  strict private
    class var
      FHeavenlyTrunksIdeographs: TArray<Char>;
      FEarthlyBranchesIdeographs: TArray<Char>;
    class constructor Initialize;
  public
    function Convert(ANumber: Integer): string;
  end;

  { TdxNumberToMultipleCharsConverter }

  TdxNumberToMultipleCharsConverter = class abstract
  protected
    function ConvertSingleChar(ACh: Char): Char; virtual; abstract;
  public
    function Convert(ANumber: Double; const ACulture: TdxCultureInfo): string; virtual;
  end;

  { TdxNumberToDoubleByteNumberConverter }

  TdxNumberToDoubleByteNumberConverter = class(TdxNumberToMultipleCharsConverter)
  private const
    DoubleByteNumberCharOffset = 65248;
  protected
    function ConvertSingleChar(ACh: Char): Char; override;
  end;

  { TdxNumberToDigitalIdeographsConverter }

  TdxNumberToDigitalIdeographsConverter = class(TdxNumberToMultipleCharsConverter)
  strict private
    const
      ZeroCharCode = 48;
    class var
      FDigitalIdeographs: TArray<Char>;
    class constructor Initialize;
  protected
    function ConvertSingleChar(ACh: Char): Char; override;
  end;

  { TdxNumberToEnglishDollarTextConverter }

  TdxNumberToEnglishDollarTextConverter = class(TdxNumberToDollarTextConverter)
  protected
    function GetNumberConverter: TdxDescriptiveNumberConverterBase; override;
    function GetContactString: string; override;
  end;

implementation

uses
  Character, Variants, Math,
  cxVariants,
  dxRichEdit.NumberConverters.EnglishUS,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.PieceTableIterators,
  dxRichEdit.DocumentModel.Fields.InfixNotationParser,
  dxRichEdit.Utils.ChunkedStringBuilder,
  dxRichEdit.Utils.Exceptions,
  dxRichEdit.Utils.Exceptions.Strs;

{ TdxFieldFormatter.TCultureUpdater }

constructor TdxFieldFormatter.TCultureUpdater.Create;
begin
  inherited Create;
  cxFormatController.AddListener(Self);
  TdxFieldFormatter.UpdateCurrentCulture;
end;

destructor TdxFieldFormatter.TCultureUpdater.Destroy;
begin
  inherited Destroy;
end;

procedure TdxFieldFormatter.TCultureUpdater.FormatChanged;
begin
  TdxFieldFormatter.UpdateCurrentCulture;
end;

{ TdxFieldFormatter }

constructor TdxFieldFormatter.Create;
begin
  inherited Create;
  if FCultureUpdater = nil then
    FCultureUpdater := TCultureUpdater.Create;
end;

class destructor TdxFieldFormatter.Finalize;
begin
  FreeAndNil(FCultureUpdater);
end;

function TdxFieldFormatter.GetCulture: TdxCultureInfo;
begin
  Result := FCulture;
end;

class procedure TdxFieldFormatter.ThrowException(const AMessage: TcxResourceStringID);
var
  S: string;
begin
  S := cxGetResourceString(AMessage);
  raise EArgumentException.Create(S);
end;

class procedure TdxFieldFormatter.ThrowException(const AMessage: TcxResourceStringID;
  const AArgument: array of const);
var
  S: string;
begin
  S := cxGetResourceString(AMessage);
  raise EArgumentException.Create(Format(S, AArgument));
end;

class procedure TdxFieldFormatter.ThrowIncorrectNumericFieldFormatError;
begin
  ThrowException(@sdxRichEditExceptionIncorrectNumericFieldFormat);
end;

class procedure TdxFieldFormatter.ThrowMissingOperatorError;
begin
  ThrowException(@sdxRichEditExceptionMissingOperator);
end;

class procedure TdxFieldFormatter.ThrowSyntaxError(
  const AIncorrectFormat: string);
begin
  ThrowException(@sdxRichEditExceptionSyntaxErrorInFieldPattern, [AIncorrectFormat]);
end;

class procedure TdxFieldFormatter.ThrowSyntaxError(AIncorrectChar: Char);
var
  S: string;
begin
  S := AIncorrectChar;
  ThrowSyntaxError(S);
end;

class procedure TdxFieldFormatter.ThrowUnexpectedEndOfFormulaError;
begin
  ThrowException(@sdxRichEditExceptionUnexpectedEndOfFormula);
end;

class procedure TdxFieldFormatter.ThrowUnknownSwitchArgumentError;
begin
  ThrowException(@sdxRichEditExceptionUnknownSwitchArgument);
end;

class procedure TdxFieldFormatter.ThrowUnmatchedQuotesError;
begin
  ThrowException(@sdxRichEditExceptionUnmatchedQuotesInFieldPattern);
end;

class procedure TdxFieldFormatter.ThrowZeroDivideError;
begin
  ThrowException(@sdxRichEditExceptionZeroDivide);
end;

class procedure TdxFieldFormatter.UpdateCurrentCulture;
begin
  FCulture := TdxCultureInfo.CurrentCulture;
end;

{ TdxSpecificFieldFormatter<T> }

function TdxSpecificFieldFormatter<T>.Format(const AValue: T;
  const AFormat: string; AHasFormat: Boolean): string;
begin
  if not AHasFormat then
    Result := FormatByDefault(AValue)
  else
  begin
    if AFormat = '' then
      Result := ''
    else
      Result := InternalFormat(AValue, AFormat);
  end;
end;

{ TdxGeneralFieldFormatter }

function TdxGeneralFieldFormatter.Format(const AValue: TValue; const AFormattedValue: string; const AKeyword: string): string;
begin
  BeginFormat;
  try
    Result := FormatCore(AValue, AFormattedValue, AKeyword);
  finally
    EndFormat;
  end;
end;

procedure TdxGeneralFieldFormatter.BeginFormat;
begin
  FNumberFormatter := TdxGeneralNumberFormatter.Create(Culture);
  FStringFormatter := TdxGeneralStringFormatter.Create(Culture);
  FDocumentModelStringFormatter := TdxGeneralDocumentModelStringFormatter.Create(Culture);
end;

procedure TdxGeneralFieldFormatter.EndFormat;
begin
  FreeAndNil(FNumberFormatter);
  FreeAndNil(FStringFormatter);
  FreeAndNil(FDocumentModelStringFormatter);
end;

function TdxGeneralFieldFormatter.IsGeneralDocumentModelStringFormatter(const AKeyword: string): Boolean;
begin
  Result := TdxGeneralDocumentModelStringFormatter.ContainsKeyword(AKeyword, Culture);
end;

procedure TdxGeneralFieldFormatter.FormatPieceTable(APieceTable: TdxCustomPieceTable{TdxPieceTable}; const AKeyword: string);
begin
  if TdxGeneralDocumentModelStringFormatter.ContainsKeyword(AKeyword, Culture) then
  begin
    BeginFormat;
    try
      FDocumentModelStringFormatter.Format(APieceTable, AKeyword);
    finally
      EndFormat;
    end;
  end;
end;

function TdxGeneralFieldFormatter.FormatCore(const AValue: TValue; const AFormattedValue, AKeyword: string): string;
begin
  if FNumberFormatter.ContainsKeyword(AKeyword, Culture) then
    Exit(FormatAsNumber(AValue, AFormattedValue, AKeyword))
  else
    if FStringFormatter.ContainsKeyword(AKeyword, Culture) then
      Exit(FormatAsString(AValue, AFormattedValue, AKeyword))
    else
      TdxFieldFormatter.ThrowUnknownSwitchArgumentError;
  Result := AFormattedValue;
end;

function TdxGeneralFieldFormatter.FormatAsNumber(const AValue: TValue; const AFormattedValue, AKeyword: string): string;
var
  ADoubleValue, AProcessedValue: Double;
begin
  try
    ADoubleValue := AValue.ToFloat;
    Result := FNumberFormatter.Format(ADoubleValue, AKeyword);
  except
    if TryGetValue(AFormattedValue, AProcessedValue) then
      Result := FNumberFormatter.Format(AProcessedValue, AKeyword)
    else
      Result := AFormattedValue;
  end;
end;

function TdxGeneralFieldFormatter.FormatAsString(const AValue: TValue; const AFormattedValue, AKeyword: string): string;
begin
  if AFormattedValue <> '' then
    Result := FStringFormatter.Format(AFormattedValue, AKeyword)
  else
    Result := AFormattedValue;
end;

function TdxGeneralFieldFormatter.TryGetValue(const ANotation: string; out AResult: Double): Boolean;
var
  ACalculator: TdxMathematicalCalculator;
begin
  AResult := 0;
  if ANotation = '' then
    Exit(False);
  try
    ACalculator := TdxMathematicalCalculator.Create;
    try
      AResult := ACalculator.Calculate(ANotation, Culture);
    finally
      ACalculator.Free;
    end;
    Result := True;
  except
    Result := False;
  end;
end;

{ TdxGeneralFormatterBase }

constructor TdxGeneralFormatterBase.Create(const ACulture: TdxCultureInfo);
begin
  inherited Create;
  FCulture := ACulture;
end;

class function TdxGeneralFormatterBase.GetUpperCaseString(const AStr: string; const ACulture: TdxCultureInfo): string;
begin
{$IFDEF DELPHIXE4}
  Result := AStr.ToUpper;
{$ELSE}
  Result := TCharacter.ToUpper(AStr);
{$ENDIF}
end;

class function TdxGeneralFormatterBase.GetLowerCaseString(const AStr: string; const ACulture: TdxCultureInfo): string;
begin
{$IFDEF DELPHIXE4}
  Result := AStr.ToLower;
{$ELSE}
  Result := TCharacter.ToLower(AStr);
{$ENDIF}
end;

function TdxGeneralFormatterBase.Format(const AValue: TValue; const AKeyword: string): string;
var
  AHandler: TdxFormatKeywordHandler;
  ACaseSensitiveKeyword, ALowerCaseKeyword: string;
begin
  Result := '';
  ACaseSensitiveKeyword := GetCaseSensitiveKeyword(AKeyword);
  if GetKeywords.TryGetValue(ACaseSensitiveKeyword, AHandler) then
    Exit(AHandler(AValue, Culture))
  else
  begin
    ALowerCaseKeyword := GetLowerCaseString(AKeyword, Culture);
    if GetKeywords.TryGetValue(ALowerCaseKeyword, AHandler) then
      Exit(AHandler(AValue, Culture));
  end;
end;

class function TdxGeneralFormatterBase.ContainsKeyword(const AKeyword: string; const ACulture: TdxCultureInfo): Boolean;
begin
  if GetKeywords.ContainsKey(GetUpperCaseString(AKeyword, ACulture)) then
    Exit(True);
  Result := GetKeywords.ContainsKey(GetLowerCaseString(AKeyword, ACulture));
end;

class function TdxGeneralFormatterBase.GetKeywords: TdxFormatKeywordTable;
begin
  raise Exception.Create('');
end;

function TdxGeneralFormatterBase.GetCaseSensitiveKeyword(const AKeyword: string): string;
begin
  if {$IFDEF DELPHIXE4}AKeyword[1].IsUpper{$ELSE}TCharacter.IsUpper(AKeyword[1]){$ENDIF} then
    Result := GetUpperCaseString(AKeyword, Culture)
  else
    Result := GetLowerCaseString(AKeyword, Culture);
end;

{ TdxGeneralNumberFormatter }

class constructor TdxGeneralNumberFormatter.Initialize;
begin
  FNumberFormatKeywords := CreateNumberFormatKeywordTable;
end;

class destructor TdxGeneralNumberFormatter.Finalize;
begin
  FreeAndNil(FNumberFormatKeywords);
end;

class function TdxGeneralNumberFormatter.CreateNumberFormatKeywordTable: TdxFormatKeywordTable;
begin
  Result := TdxFormatKeywordTable.Create;
  Result.Add('ALPHABETIC', ConvertToUppercaseAlphabeticLatinCharacters);
  Result.Add('alphabetic', ConvertToLowercaseAlphabeticLatinCharacters);
  Result.Add('arabic', ConvertToArabicCardinalNumerals);
  Result.Add('arabicdash', ConvertToArabicCardinalNumeralsWithDashes);
  Result.Add('cardtext', ConvertToLowercaseCardinalText);
  Result.Add('circlenum', ConvertToNumberEnclosedInCircle);
  Result.Add('dbchar', ConvertToDoubleByteArabicNumber);
  Result.Add('dbnum1', ConvertToSequentialDigitalIdeographs);
  Result.Add('kanjinum1', ConvertToSequentialDigitalIdeographs);
  Result.Add('dollartext', ConvertToDollarText);
  Result.Add('gb1', ConvertToNumberFollowedByPeriod);
  Result.Add('gb2', ConvertToNumberEnclosedInBrackets);
  Result.Add('gb3', ConvertToNumberEnclosedInCircleTruncated);
  Result.Add('gb4', ConvertToIdeographsEnclosedInBrackets);
  Result.Add('hex', ConvertToHexadecimalNumber);
  Result.Add('ordinal', ConvertToOrdinalNumber);
  Result.Add('ordtext', ConvertToLowercaseOrdinalText);
  Result.Add('ROMAN', ConvertToUppercaseRomanNumber);
  Result.Add('roman', ConvertToLowercaseRomanNumber);
  Result.Add('sbchar', ConvertToArabicNumber);
  Result.Add('zodiac1', ConvertToNumericalTraditionalIdeographs);
  Result.Add('zodiac2', ConvertToNumericalZodiacIdeographs);
  Result.Add('zodiac3', ConvertToNumericalTraditionalZodiacIdeographs);
end;

class function TdxGeneralNumberFormatter.ConvertToUppercaseAlphabeticLatinCharacters(const AValue: TValue;
  const ACulture: TdxCultureInfo): string;
var
  AIntegerValue: Integer;
  AConverter: TdxUpperLatinLetterNumberConverter;
begin
  AIntegerValue := ValidateValueForConvertingToLatinChars(AValue);
  AConverter := TdxUpperLatinLetterNumberConverter.Create;
  try
    Result := AConverter.ConvertNumber(AIntegerValue);
  finally
    AConverter.Free;
  end;
end;

class function TdxGeneralNumberFormatter.ConvertToLowercaseAlphabeticLatinCharacters(const AValue: TValue;
  const ACulture: TdxCultureInfo): string;
var
  AIntegerValue: Integer;
  AConverter: TdxLowerLatinLetterNumberConverter;
begin
  AIntegerValue := ValidateValueForConvertingToLatinChars(AValue);
  AConverter := TdxLowerLatinLetterNumberConverter.Create;
  try
    Result := AConverter.ConvertNumber(AIntegerValue);
  finally
    AConverter.Free;
  end;
end;

class function TdxGeneralNumberFormatter.ValidateValueForConvertingToLatinChars(const AValue: TValue): Integer;
var
  AIntegerValue: Integer;
begin
  AIntegerValue := ConvertToInteger(AValue);
  if (AIntegerValue < 0) or (780 < AIntegerValue) then
    TdxFieldFormatter.ThrowIncorrectNumericFieldFormatError;
  Result := AIntegerValue;
end;

class function TdxGeneralNumberFormatter.ConvertToArabicCardinalNumerals(const AValue: TValue; const ACulture: TdxCultureInfo): string;
var
  AIntegerValue: Integer;
begin
  AIntegerValue := ConvertToInteger(AValue);
  Result := GetNumberString(AIntegerValue, ACulture);
end;

class function TdxGeneralNumberFormatter.ConvertToArabicCardinalNumeralsWithDashes(const AValue: TValue; const ACulture: TdxCultureInfo): string;
var
  AIntegerValue: Integer;
begin
  AIntegerValue := ConvertToInteger(AValue);
  Result := SysUtils.Format('- %s -', [GetFormattedNumberString('0', AIntegerValue, ACulture)]);
end;

class function TdxGeneralNumberFormatter.ConvertToLowercaseCardinalText(const AValue: TValue; const ACulture: TdxCultureInfo): string;
var
  AConverter: TdxDescriptiveNumberConverterBase;
begin
  AConverter := GetDescriptiveCardinalNumberConverter(ACulture.Parent.EnglishName);
  try
    Result := ConvertToLowercaseText(AValue, AConverter, ACulture);
  finally
    AConverter.Free;
  end;
end;

class function TdxGeneralNumberFormatter.ConvertToLowercaseOrdinalText(const AValue: TValue; const ACulture: TdxCultureInfo): string;
var
  AConverter: TdxDescriptiveNumberConverterBase;
begin
  AConverter := GetDescriptiveOrdinalNumberConverter(ACulture.Parent.EnglishName);
  try
    Result := ConvertToLowercaseText(AValue, AConverter, ACulture);
  finally
    AConverter.Free;
  end;
end;

class function TdxGeneralNumberFormatter.ConvertToLowercaseText(const AValue: TValue; AConverter: TdxDescriptiveNumberConverterBase;
  const ACulture: TdxCultureInfo): string;
var
  AIntegerValue: Integer;
begin
  AIntegerValue := ConvertToInteger(AValue);
  if (AIntegerValue < 0) or (999999 < AIntegerValue) then
    TdxFieldFormatter.ThrowIncorrectNumericFieldFormatError;
  Result := GetLowerCaseString(AConverter.ConvertNumber(AIntegerValue), ACulture);
end;

class function TdxGeneralNumberFormatter.GetDescriptiveCardinalNumberConverter(const ACultureName: string): TdxDescriptiveNumberConverterBase;
begin
    Result := TdxDescriptiveCardinalEnglishNumberConverter.Create;
end;

class function TdxGeneralNumberFormatter.GetDescriptiveOrdinalNumberConverter(const ACultureName: string): TdxDescriptiveNumberConverterBase;
begin
      Exit(TdxDescriptiveOrdinalEnglishNumberConverter.Create);
end;

class function TdxGeneralNumberFormatter.ConvertToOrdinalNumber(const AValue: TValue; const ACulture: TdxCultureInfo): string;
var
  AIntegerValue: Integer;
  AConverter: TdxOrdinalBasedNumberConverter;
begin
  AIntegerValue := ConvertToInteger(AValue);
  AConverter := GetOrdinalNumberConverter(ACulture.Parent.EnglishName);
  try
    Result := AConverter.ConvertNumber(AIntegerValue);
  finally
    AConverter.Free;
  end;
end;

class function TdxGeneralNumberFormatter.GetOrdinalNumberConverter(const ACultureName: string): TdxOrdinalBasedNumberConverter;
begin
      Exit(TdxOrdinalEnglishNumberConverter.Create);
end;

class function TdxGeneralNumberFormatter.ConvertToNumberEnclosedInCircle(const AValue: TValue; const ACulture: TdxCultureInfo): string;
var
  AConverter: TdxNumberToEnclosedCircleNumberConverter;
begin
  AConverter := TdxNumberToEnclosedCircleNumberConverter.Create;
  try
    Result := ConvertNumberToCharacter(AValue, AConverter, ACulture);
  finally
    AConverter.Free;
  end;
end;

class function TdxGeneralNumberFormatter.ConvertToNumberFollowedByPeriod(const AValue: TValue; const ACulture: TdxCultureInfo): string;
var
  AConverter: TdxNumberToNumberFollowedByPeriodConverter;
begin
  AConverter := TdxNumberToNumberFollowedByPeriodConverter.Create;
  try
    Result := ConvertNumberToCharacter(AValue, AConverter, ACulture);
  finally
    AConverter.Free;
  end;
end;

class function TdxGeneralNumberFormatter.ConvertToNumberEnclosedInBrackets(const AValue: TValue; const ACulture: TdxCultureInfo): string;
var
  AConverter: TdxNumberToEnclosedBracketsNumberConverter;
begin
  AConverter := TdxNumberToEnclosedBracketsNumberConverter.Create;
  try
    Result := ConvertNumberToCharacter(AValue, AConverter, ACulture);
  finally
    AConverter.Free;
  end;
end;

class function TdxGeneralNumberFormatter.ConvertToIdeographsEnclosedInBrackets(const AValue: TValue; const ACulture: TdxCultureInfo): string;
var
  AConverter: TdxNumberToIdeographsEnclosedBracketsConverter;
begin
  AConverter := TdxNumberToIdeographsEnclosedBracketsConverter.Create;
  try
    Result := ConvertNumberToCharacter(AValue, AConverter, ACulture);
  finally
    AConverter.Free;
  end;
end;

class function TdxGeneralNumberFormatter.ConvertToNumericalTraditionalIdeographs(const AValue: TValue; const ACulture: TdxCultureInfo): string;
var
  AConverter: TdxNumberToTraditionalIdeographsConverter;
begin
  AConverter := TdxNumberToTraditionalIdeographsConverter.Create;
  try
    Result := ConvertNumberToCharacter(AValue, AConverter, ACulture);
  finally
    AConverter.Free;
  end;
end;

class function TdxGeneralNumberFormatter.ConvertToNumericalZodiacIdeographs(const AValue: TValue; const ACulture: TdxCultureInfo): string;
var
  AConverter: TdxNumberToZodiacIdeographsConverter;
begin
  AConverter := TdxNumberToZodiacIdeographsConverter.Create;
  try
    Result := ConvertNumberToCharacter(AValue, AConverter, ACulture);
  finally
    AConverter.Free;
  end;
end;

class function TdxGeneralNumberFormatter.ConvertToNumericalTraditionalZodiacIdeographs(const AValue: TValue; const ACulture: TdxCultureInfo): string;
var
  AIntegerValue: Integer;
  AConverter: TdxNumberToTraditionalZodiacIdeographsConverter;
begin
  if IsZero(AValue.ToFloat) then
    Exit(GetNumberString(0, ACulture));
  if AValue.ToFloat < 0 then
    TdxFieldFormatter.ThrowIncorrectNumericFieldFormatError;
  AIntegerValue := ConvertToInteger(AValue);
  AConverter := TdxNumberToTraditionalZodiacIdeographsConverter.Create;
  try
    Result := AConverter.Convert(AIntegerValue);
  finally
    AConverter.Free;
  end;
end;

class function TdxGeneralNumberFormatter.ConvertNumberToCharacter(const AValue: TValue; AConverter: TdxNumberToSingleCharConverter;
  const ACulture: TdxCultureInfo): string;
var
  AIntegerValue: Integer;
begin
  if AValue.ToFloat < 0 then
    TdxFieldFormatter.ThrowIncorrectNumericFieldFormatError;
  AIntegerValue := ConvertToInteger(AValue);
  if not AConverter.TryConvert(AIntegerValue, Result) then
    Result := GetNumberString(AIntegerValue, ACulture);
end;

class function TdxGeneralNumberFormatter.ConvertToDoubleByteArabicNumber(const AValue: TValue; const ACulture: TdxCultureInfo): string;
var
  AConverter: TdxNumberToDoubleByteNumberConverter;
begin
  AConverter := TdxNumberToDoubleByteNumberConverter.Create;
  try
    Result := AConverter.Convert(AValue.ToFloat, ACulture);
  finally
    AConverter.Free;
  end;
end;

class function TdxGeneralNumberFormatter.ConvertToSequentialDigitalIdeographs(const AValue: TValue; const ACulture: TdxCultureInfo): string;
var
  ARoundValue: Double;
  AConverter: TdxNumberToDigitalIdeographsConverter;
begin
  ARoundValue := ConvertToInteger(AValue);
  AConverter := TdxNumberToDigitalIdeographsConverter.Create;
  try
    Result := AConverter.Convert(ARoundValue, ACulture);
  finally
    AConverter.Free;
  end;
end;

class function TdxGeneralNumberFormatter.ConvertToDollarText(const AValue: TValue; const ACulture: TdxCultureInfo): string;
var
  AConverter: TdxNumberToDollarTextConverter;
  ADollar: Double;
begin
  ADollar := AValue.ToFloat;
  if (ADollar < 0) or (ADollar >= 1000000) then
    TdxFieldFormatter.ThrowIncorrectNumericFieldFormatError;
  AConverter := GetDollarTextConverter(ACulture.Parent.EnglishName);
  try
    Result := GetLowerCaseString(AConverter.Convert(ADollar), ACulture);
  finally
    AConverter.Free;
  end;
end;

class function TdxGeneralNumberFormatter.GetDollarTextConverter(const ACultureName: string): TdxNumberToDollarTextConverter;
begin
      Exit(TdxNumberToEnglishDollarTextConverter.Create);
end;

class function TdxGeneralNumberFormatter.ConvertToNumberEnclosedInCircleTruncated(const AValue: TValue; const ACulture: TdxCultureInfo): string;
var
  AIntegerValue: Integer;
begin
  AIntegerValue := ConvertToInteger(AValue);
  if AIntegerValue < 11 then
    Exit(ConvertToNumberEnclosedInCircle(AIntegerValue, ACulture))
  else
    Exit(GetNumberString(AIntegerValue, ACulture));
end;

class function TdxGeneralNumberFormatter.ConvertToHexadecimalNumber(const AValue: TValue; const ACulture: TdxCultureInfo): string;
var
  AIntegerValue: Integer;
begin
  AIntegerValue := ConvertToShortInteger(AValue);
  Result := IntToHex(AIntegerValue, 0);
end;

class function TdxGeneralNumberFormatter.ConvertToUppercaseRomanNumber(const AValue: TValue; const ACulture: TdxCultureInfo): string;
var
  AIntegerValue: Integer;
  AConverter: TdxUpperRomanNumberConverterClassic;
begin
  AIntegerValue := ConvertToShortInteger(AValue);
  AConverter := TdxUpperRomanNumberConverterClassic.Create;
  try
    Result := AConverter.ConvertNumber(AIntegerValue);
  finally
    AConverter.Free;
  end;
end;

class function TdxGeneralNumberFormatter.ConvertToLowercaseRomanNumber(const AValue: TValue; const ACulture: TdxCultureInfo): string;
var
  AIntegerValue: Integer;
  AConverter: TdxLowerRomanNumberConverterClassic;
begin
  AIntegerValue := ConvertToShortInteger(AValue);
  AConverter := TdxLowerRomanNumberConverterClassic.Create;
  try
    Result := AConverter.ConvertNumber(AIntegerValue);
  finally
    AConverter.Free;
  end;
end;

class function TdxGeneralNumberFormatter.ConvertToArabicNumber(const AValue: TValue; const ACulture: TdxCultureInfo): string;
begin
  Result := GetNumberString(AValue, ACulture);
end;

class function TdxGeneralNumberFormatter.ConvertToInteger(const AValue: TValue): Integer;
begin
  try
    Result := Round(AValue.ToFloat);
  except
    on EMathError do
    begin
      TdxFieldFormatter.ThrowIncorrectNumericFieldFormatError;
      Result := -1;
    end;
  end;
end;

class function TdxGeneralNumberFormatter.ConvertToShortInteger(const AValue: TValue): Integer;
begin
  try
    Result := Round(AValue.ToFloat);
  except
    on EMathError do
    begin
      TdxFieldFormatter.ThrowIncorrectNumericFieldFormatError;
      Result := -1;
    end;
  end;
end;

class function TdxGeneralNumberFormatter.GetNumberString(const AValue: TValue; const ACulture: TdxCultureInfo): string;
var
  AFormattingString: string;
begin
  AFormattingString := '0.' + StringOfChar('#', 14);
  Result := FormatFloat(AFormattingString, AValue.ToFloat, ACulture.FormatSettings);
end;

class function TdxGeneralNumberFormatter.GetNumberString(const AValue: Integer; const ACulture: TdxCultureInfo): string;
begin
  Result := IntToStr(AValue);
end;

class function TdxGeneralNumberFormatter.GetFormattedNumberString(const AFormat: string; AValue: Integer; const ACulture: TdxCultureInfo): string;
var
  ATemp: Double;
begin
  ATemp := AValue;
  Result := FormatFloat(AFormat, ATemp, ACulture.FormatSettings);
end;

class function TdxGeneralNumberFormatter.GetKeywords: TdxFormatKeywordTable;
begin
  Result := FNumberFormatKeywords;
end;

{ TdxGeneralStringFormatter }

class constructor TdxGeneralStringFormatter.Initialize;
begin
  FStringFormatKeywords := CreateStringFormatKeywordsTable;
end;

class destructor TdxGeneralStringFormatter.Finalize;
begin
  FreeAndNil(FStringFormatKeywords);
end;

class function TdxGeneralStringFormatter.CreateStringFormatKeywordsTable: TdxFormatKeywordTable;
begin
  Result := TdxFormatKeywordTable.Create;
  Result.Add('caps', CapitalizesFirstLetterOfEachWord);
  Result.Add('firstcap', CapitalizesFirstLetterOfFirstWord);
  Result.Add('lower', ConvertToLowercaseString);
  Result.Add('upper', ConvertToUppercaseString);
end;

class function TdxGeneralStringFormatter.CapitalizesFirstLetterOfEachWord(const AValue: TValue;
  const ACulture: TdxCultureInfo): string;
var
  ABuilder: TStringBuilder;
  AIndex: Integer;
begin
  ABuilder := TStringBuilder.Create({$IFDEF DELPHIXE4}AValue.AsString.ToLower{$ELSE}TCharacter.ToLower(AValue.AsString){$ENDIF});
  try
    AIndex := 0;
    while AIndex < ABuilder.Length do
    begin
      ABuilder[AIndex] := {$IFDEF DELPHIXE4}ABuilder[AIndex].ToUpper{$ELSE}TCharacter.ToUpper(ABuilder[AIndex]){$ENDIF};
      AIndex := SkipCharacters(AIndex, ABuilder, IsLetterOrDigit);
      AIndex := SkipCharacters(AIndex, ABuilder, IsNotLetterOrDigit);
    end;
    Result := ABuilder.ToString;
  finally
    ABuilder.Free;
  end;
end;

class function TdxGeneralStringFormatter.SkipCharacters(AIndex: Integer; AStr: TStringBuilder;
  const APredicate: TdxPredicate<Char>): Integer;
begin
  while (AIndex < AStr.Length) and APredicate(AStr[AIndex]) do
    Inc(AIndex);
  Result := AIndex;
end;

class function TdxGeneralStringFormatter.IsLetterOrDigit(const ACh: Char): Boolean;
begin
  Result := {$IFDEF DELPHIXE4}ACh.IsLetterOrDigit{$ELSE}TCharacter.IsLetterOrDigit(ACh){$ENDIF};
end;

class function TdxGeneralStringFormatter.IsNotLetterOrDigit(const ACh: Char): Boolean;
begin
  Result := not IsLetterOrDigit(ACh);
end;

class function TdxGeneralStringFormatter.CapitalizesFirstLetterOfFirstWord(const AValue: TValue;
  const ACulture: TdxCultureInfo): string;
var
  ABuilder: TStringBuilder;
begin
  ABuilder := TStringBuilder.Create(AValue.AsString);
  try
    if ABuilder.Length > 0 then
      ABuilder[0] := {$IFDEF DELPHIXE4}ABuilder[0].ToUpper{$ELSE}TCharacter.ToUpper(ABuilder[0]){$ENDIF};
    Result := ABuilder.ToString;
  finally
    ABuilder.Free;
  end;
end;

class function TdxGeneralStringFormatter.ConvertToLowercaseString(const AValue: TValue;
  const ACulture: TdxCultureInfo): string;
begin
  Result := {$IFDEF DELPHIXE4}AValue.AsString.ToLower{$ELSE}TCharacter.ToLower(AValue.AsString){$ENDIF};
end;

class function TdxGeneralStringFormatter.ConvertToUppercaseString(const AValue: TValue;
  const ACulture: TdxCultureInfo): string;
begin
  Result := {$IFDEF DELPHIXE4}AValue.AsString.ToUpper{$ELSE}TCharacter.ToUpper(AValue.AsString){$ENDIF};
end;

class function TdxGeneralStringFormatter.GetKeywords: TdxFormatKeywordTable;
begin
  Result := FStringFormatKeywords;
end;

{ TdxGeneralDocumentModelStringFormatter }

class constructor TdxGeneralDocumentModelStringFormatter.Initialize;
begin
  FStringFormatKeywords := CreateStringFormatKeywordsTable;
end;

class destructor TdxGeneralDocumentModelStringFormatter.Finalize;
begin
  FreeAndNil(FStringFormatKeywords);
end;

class function TdxGeneralDocumentModelStringFormatter.CreateStringFormatKeywordsTable: TdxFormatKeywordTable;
begin
  Result := TdxFormatKeywordTable.Create;

  Result.Add('caps', CapitalizesFirstLetterOfEachWord);
  Result.Add('firstcap', CapitalizesFirstLetterOfFirstWord);
  Result.Add('lower', ConvertToLowercaseString);
  Result.Add('upper', ConvertToUppercaseString);
end;

class function TdxGeneralDocumentModelStringFormatter.CapitalizesFirstLetterOfEachWord(const AValue: TValue;
  const ACulture: TdxCultureInfo): string;
var
  ATable: TdxPieceTable;
  AIterator: TdxWordsDocumentModelIterator;
  APos: TdxDocumentModelPosition;
  ATextBuffer: TdxChunkedStringBuilder;
  ARanges: TdxTextRunCollection;
  ARange: TdxTextRunBase;
  APosition: Integer;
begin
  ATable := TdxPieceTable(AValue.AsObject);
  AIterator := TdxWordsDocumentModelIterator.Create(ATable);
  try
    APos := TdxDocumentModelPosition.Create(ATable);
    if not AIterator.IsInsideWord(APos) then
      APos := AIterator.MoveForward(APos);
    ATextBuffer := ATable.TextBuffer;
    ARanges := ATable.Runs;
    while APos.LogPosition < ATable.DocumentEndLogPosition do
    begin
      ARange := ARanges[APos.RunIndex];
      APosition := ARange.StartIndex + APos.RunOffset;
      ATextBuffer[APosition] := {$IFDEF DELPHIXE4}ATextBuffer[APosition].ToUpper{$ELSE}TCharacter.ToUpper(ATextBuffer[APosition]){$ENDIF};
      APos := AIterator.MoveForward(APos);
    end;
    Result := '';
  finally
    AIterator.Free;
  end;
end;

class function TdxGeneralDocumentModelStringFormatter.CapitalizesFirstLetterOfFirstWord(const AValue: TValue;
  const ACulture: TdxCultureInfo): string;
var
  ATable: TdxPieceTable;
  AIterator: TdxWordsDocumentModelIterator;
  APos: TdxDocumentModelPosition;
  ATextBuffer: TdxChunkedStringBuilder;
  ARanges: TdxTextRunCollection;
  ARange: TdxTextRunBase;
  APosition: Integer;
begin
  ATable := TdxPieceTable(AValue.AsObject);
  AIterator := TdxWordsDocumentModelIterator.Create(ATable);
  try
    APos := TdxDocumentModelPosition.Create(ATable);
    if not AIterator.IsInsideWord(APos) then
      APos := AIterator.MoveForward(APos);
    ATextBuffer := ATable.TextBuffer;
    ARanges := ATable.Runs;
    if APos.LogPosition < ATable.DocumentEndLogPosition then
    begin
      ARange := ARanges[APos.RunIndex];
      APosition := ARange.StartIndex + APos.RunOffset;
      ATextBuffer[APosition] := {$IFDEF DELPHIXE4}ATextBuffer[APosition].ToUpper{$ELSE}TCharacter.ToUpper(ATextBuffer[APosition]){$ENDIF};
    end;
    Result := '';
  finally
    AIterator.Free;
  end;
end;

class function TdxGeneralDocumentModelStringFormatter.ConvertToLowercaseString(const AValue: TValue;
  const ACulture: TdxCultureInfo): string;
var
  ATable: TdxPieceTable;
  ATextBuffer: TdxChunkedStringBuilder;
  ACount, I: Integer;
begin
  ATable := TdxPieceTable(AValue.AsObject);
  ATextBuffer := ATable.TextBuffer;
  ACount := ATextBuffer.Length;
  for I := 0 to ACount - 1 do
    ATextBuffer[I] := {$IFDEF DELPHIXE4}ATextBuffer[I].ToLower{$ELSE}TCharacter.ToLower(ATextBuffer[I]){$ENDIF};
  Result := '';
end;

class function TdxGeneralDocumentModelStringFormatter.ConvertToUppercaseString(const AValue: TValue;
  const ACulture: TdxCultureInfo): string;
var
  ATable: TdxPieceTable;
  ATextBuffer: TdxChunkedStringBuilder;
  ACount, I: Integer;
begin
  ATable := TdxPieceTable(AValue.AsObject);
  ATextBuffer := ATable.TextBuffer;
  ACount := ATextBuffer.Length;
  for I := 0 to ACount - 1 do
    ATextBuffer[I] := {$IFDEF DELPHIXE4}ATextBuffer[I].ToUpper{$ELSE}TCharacter.ToUpper(ATextBuffer[I]){$ENDIF};
  Result := '';
end;

class function TdxGeneralDocumentModelStringFormatter.GetKeywords: TdxFormatKeywordTable;
begin
  Result := FStringFormatKeywords;
end;

{ TdxNumberToEnclosedCircleNumberConverter }

function TdxNumberToEnclosedCircleNumberConverter.CreateAlphanumericCharsTable: TArray<Char>;
begin
  Result := TArray<Char>.Create(#$2460, #$2461, #$2462, #$2463, #$2464, #$2465, #$2466,
    #$2467, #$2468, #$2469, #$246A, #$246B, #$246C, #$246D, #$246E, #$246F, #$2470, #$2471,
    #$2472, #$2473);
end;

{ TdxNumberToNumberFollowedByPeriodConverter }

function TdxNumberToNumberFollowedByPeriodConverter.CreateAlphanumericCharsTable: TArray<Char>;
begin
  Result := TArray<Char>.Create(#$2488, #$2489, #$248A, #$248B, #$248C, #$248D, #$248E, #$248F,
    #$2490, #$2491, #$2492, #$2493, #$2494, #$2495, #$2496, #$2497, #$2498, #$2499, #$249A, #$249B);
end;

{ TdxNumberToEnclosedBracketsNumberConverter }

function TdxNumberToEnclosedBracketsNumberConverter.CreateAlphanumericCharsTable: TArray<Char>;
begin
  Result := TArray<Char>.Create(#$2474, #$2475, #$2476, #$2477, #$2478, #$2479, #$247A, #$247B,
    #$247C, #$247D, #$247E, #$247F, #$2480, #$2481, #$2482, #$2483, #$2484, #$2485, #$2486, #$2487);
end;

{ TdxNumberToIdeographsEnclosedBracketsConverter }

function TdxNumberToIdeographsEnclosedBracketsConverter.CreateAlphanumericCharsTable: TArray<Char>;
begin
  Result := TArray<Char>.Create(#$3220, #$3221, #$3222, #$3223, #$3224, #$3225, #$3226, #$3227,
    #$3228, #$3229);
end;

{ TdxNumberToTraditionalIdeographsConverter }

function TdxNumberToTraditionalIdeographsConverter.CreateAlphanumericCharsTable: TArray<Char>;
begin
  Result := TArray<Char>.Create(#$7532, #$4E59, #$4E19, #$4E01, #$620A, #$5DF1, #$5E9A, #$8F9B,
    #$58EC, #$7678);
end;

{ TdxNumberToZodiacIdeographsConverter }

function TdxNumberToZodiacIdeographsConverter.CreateAlphanumericCharsTable: TArray<Char>;
begin
  Result := TArray<Char>.Create(#$5B50, #$4E11, #$5BC5, #$536F, #$8FB0, #$5DF3, #$5348, #$672A,
    #$7533, #$9149, #$620D, #$4EA5);
end;

{ TdxNumberToTraditionalZodiacIdeographsConverter }

class constructor TdxNumberToTraditionalZodiacIdeographsConverter.Initialize;
begin
  FHeavenlyTrunksIdeographs := TArray<Char>.Create(#$7532, #$4E59, #$4E19,
    #$4E01, #$620A, #$5DF1, #$5E9A, #$8F9B, #$58EC, #$7678);
  FEarthlyBranchesIdeographs := TArray<Char>.Create(#$5B50, #$4E11, #$5BC5,
    #$536F, #$8FB0, #$5DF3, #$5348, #$672A, #$7533, #$9149, #$620D, #$4EA5);
end;

function TdxNumberToTraditionalZodiacIdeographsConverter.Convert(ANumber: Integer): string;
var
  AValue: Integer;
  ABuilder: TStringBuilder;
begin
  AValue := ANumber - 1;
  ABuilder := TStringBuilder.Create(2);
  try
    ABuilder.Append(FHeavenlyTrunksIdeographs[AValue mod Length(FHeavenlyTrunksIdeographs)]);
    ABuilder.Append(FEarthlyBranchesIdeographs[AValue mod Length(FEarthlyBranchesIdeographs)]);
    Result := ABuilder.ToString;
  finally
    ABuilder.Free;
  end;
end;

{ TdxNumberToMultipleCharsConverter }

function TdxNumberToMultipleCharsConverter.Convert(ANumber: Double;
  const ACulture: TdxCultureInfo): string;
var
  ANumberStr: string;
  ABuilder: TStringBuilder;
  ACh, ANewChar: Char;
begin
  ANumberStr := FloatToStr(ANumber, ACulture.FormatSettings);
  ABuilder := TStringBuilder.Create;
  try
    for ACh in ANumberStr do
    begin
      ANewChar := ConvertSingleChar(ACh);
      ABuilder.Append(ANewChar);
    end;
    Result := ABuilder.ToString;
  finally
    ABuilder.Free;
  end;
end;

{ TdxNumberToDoubleByteNumberConverter }

function TdxNumberToDoubleByteNumberConverter.ConvertSingleChar(
  ACh: Char): Char;
var
  ANewCharCode: Integer;
begin
  ANewCharCode := Ord(ACh) + DoubleByteNumberCharOffset;
  if ANewCharCode > 65536 then
    TdxRichEditExceptions.ThrowArgumentException('numeric character', ACh);
  Result := Char(ANewCharCode);
end;

{ TdxNumberToDigitalIdeographsConverter }

class constructor TdxNumberToDigitalIdeographsConverter.Initialize;
begin
  FDigitalIdeographs := TArray<Char>.Create(#$3007, #$4E00, #$4E8C, #$4E09, #$56DB,
    #$4E94, #$516D, #$4E03, #$516B, #$4E5D);
end;

function TdxNumberToDigitalIdeographsConverter.ConvertSingleChar(
  ACh: Char): Char;
var
  ADigit: Integer;
begin
  ADigit := Ord(ACh) - ZeroCharCode;
  if (ADigit < 0) or (ADigit > 9) then
    TdxRichEditExceptions.ThrowArgumentException('digit', ACh);
  Result := FDigitalIdeographs[ADigit];
end;

{ TdxNumberToEnglishDollarTextConverter }

function TdxNumberToEnglishDollarTextConverter.GetNumberConverter: TdxDescriptiveNumberConverterBase;
begin
  Result := TdxDescriptiveCardinalEnglishNumberConverter.Create;
end;

function TdxNumberToEnglishDollarTextConverter.GetContactString: string;
begin
  Result := 'and';
end;

end.
