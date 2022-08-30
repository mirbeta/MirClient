{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressSpreadSheet                                       }
{                                                                    }
{           Copyright (c) 2001-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSSPREADSHEET CONTROL AND ALL    }
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

unit dxSpreadSheetNumberFormatCore;

interface

{$I cxVer.inc}

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Windows, Types, SysUtils, Graphics, Math, StrUtils, Variants, Generics.Collections, Generics.Defaults, dxCore,
  dxSpreadSheetCoreStrs, dxSpreadSheetUtils, dxSpreadSheetTypes, dxSpreadSheetClasses;

type
  TdxNumberFormatResult = class;

  { TdxNumberFormatType }

  TdxNumberFormatType = (nftDateTime, nftGeneral, nftNumeric, nftText);

  { TdxNumberFormatResult }

  TdxNumberFormatResult = class(TStringBuilder)
  public
    Color: TColor;
    IsError: Boolean;
    IsText: TdxDefaultBoolean;

    procedure AfterConstruction; override;
    procedure FillBySpaces(AStartIndex: Integer);
  end;

  { TdxNumberFormatFloatRec }

  TdxNumberFormatFloatRec = record
  public const
    MaxDigits = 19;
  strict private
    procedure CalculateDigitCount(ADecimals: Integer);
    procedure Initialize(const AValue: Extended; ADecimals: Integer);
    procedure MoveDigits(ADelta: Integer);
  public
    Data: TFloatRec;
    DigitCount: Integer;
    FirstDecimal: Integer;

    class function Create(const AValue: Extended; ADecimals: Integer): TdxNumberFormatFloatRec; static;
    function GetDigit(AIndex: Integer): Byte;
    function SetDigit(AIndex: Integer; AValue: Byte): Boolean;
    procedure RoundTo(ADecimals: Integer);
  end;

  { TdxNumberFormat }

  TdxNumberFormat = class abstract
  protected
    function GetValueType: TdxNumberFormatType; virtual; abstract;
  public
    procedure Format(const AValue: Variant; AValueType: TdxSpreadSheetCellDataType;
      const AFormatSettings: TdxSpreadSheetCustomFormatSettings; AResult: TdxNumberFormatResult); virtual;
    function IsTimeFormat: Boolean; virtual; abstract;
    //
    property ValueType: TdxNumberFormatType read GetValueType;
  end;

  { TdxNumberFormatDesignator }

  TdxNumberFormatDesignator = class
  public const
    Default = $0;
    AmPm    = $1; // a
    Asterisk  = $2; // *
    At = $4; // @
    Backslash = $8; // \
    Bracket = $10; // [
    DateSeparator = $20;
    Day = $40;
    DayOfWeek = $80;
    DigitEmpty = $100; // #
    DigitSpace = $200; // ?
    DigitZero = $400; // 0
    DecimalSeparator = $800;
    EndOfPart = $1000; // ;
    Exponent = $2000;
    FractionOrDateSeparator = $4000; // /
    General = $8000;
    GroupSeparator = $10000;
    Hour = $20000;
    InvariantYear = $40000; // e
    JapaneseEra = $80000; // g
    Minute = $100000;
    Month = $200000;
    Percent = $400000; // %
    Quote = $800000; // "
    Second = $1000000;
    ThaiYear = $2000000; // b
    TimeSeparator = $4000000;
    Underline = $8000000; // _
    Year = $10000000;
  end;

  { TdxNumberFormatElement }

  TdxNumberFormatElement = class abstract
  public
    function Format(const AValue: Variant; AValueType: TdxSpreadSheetCellDataType;
      const AFormatSettings: TdxSpreadSheetCustomFormatSettings; var AColor: TColor): string; overload;
    procedure Format(const AValue: Variant; AValueType: TdxSpreadSheetCellDataType;
      const AFormatSettings: TdxSpreadSheetCustomFormatSettings; AResult: TdxNumberFormatResult); overload; virtual; abstract;
    function FormatEmpty(const AFormatSettings: TdxSpreadSheetCustomFormatSettings; var AColor: TColor): string; overload;
    procedure FormatEmpty(const AFormatSettings: TdxSpreadSheetCustomFormatSettings; AResult: TdxNumberFormatResult); overload; virtual;

    function Designator: Integer; virtual; abstract;
    function IsDigit: Boolean; virtual;
  end;

  { TdxNumberFormatElementList }

  TdxNumberFormatElementList = class(TObjectList<TdxNumberFormatElement>)
  public
    procedure ExtractFrom(Source: TdxNumberFormatElementList);
  end;

  { TdxSimpleNumberFormat }

  TdxSimpleNumberFormat = class(TdxNumberFormat)
  protected
    FList: TdxNumberFormatElementList;
  public
    constructor Create(AElements: TdxNumberFormatElementList);
    destructor Destroy; override;
    function IsTimeFormat: Boolean; override;
    //
    property List: TdxNumberFormatElementList read FList;
  end;

  { TdxSimpleNumberFormatList }

  TdxSimpleNumberFormatList = class(TObjectList<TdxSimpleNumberFormat>)
  public
    procedure ExtractFrom(Source: TdxSimpleNumberFormatList);
  end;

//----------------------------------------------------------------------------------------------------------------------
// Elements
//----------------------------------------------------------------------------------------------------------------------

  { TdxNumberFormatElementGeneral }

  TdxNumberFormatElementGeneral = class(TdxNumberFormatElement)
  public
    function Designator: Integer; override;
    procedure Format(const AValue: Variant; AValueType: TdxSpreadSheetCellDataType;
      const AFormatSettings: TdxSpreadSheetCustomFormatSettings; AResult: TdxNumberFormatResult); override;
  end;

  { TdxNumberFormatElementCondition }

  TdxNumberFormatElementCondition = class(TdxNumberFormatElement)
  public
    function Designator: Integer; override;
  end;

  { TdxNumberFormatElementColor }

  TdxNumberFormatElementColor = class(TdxNumberFormatElementCondition)
  strict private
    FColor: TColor;
  public
    constructor Create(AColor: TColor);
    procedure Format(const AValue: Variant; AValueType: TdxSpreadSheetCellDataType;
      const AFormatSettings: TdxSpreadSheetCustomFormatSettings; AResult: TdxNumberFormatResult); override;
    //
    property Color: TColor read FColor;
  end;

  { TdxNumberFormatElementDigitZero }

  TdxNumberFormatElementDigitZero = class(TdxNumberFormatElement)
  public
    function Designator: Integer; override;
    procedure Format(const AValue: Variant; AValueType: TdxSpreadSheetCellDataType;
      const AFormatSettings: TdxSpreadSheetCustomFormatSettings; AResult: TdxNumberFormatResult); override;
    procedure FormatEmpty(const AFormatSettings: TdxSpreadSheetCustomFormatSettings; AResult: TdxNumberFormatResult); override;
    function IsDigit: Boolean; override;
  end;

  { TdxNumberFormatElementDigitEmpty }

  TdxNumberFormatElementDigitEmpty = class(TdxNumberFormatElementDigitZero)
  public
    function Designator: Integer; override;
    procedure FormatEmpty(const AFormatSettings: TdxSpreadSheetCustomFormatSettings; AResult: TdxNumberFormatResult); override;
  end;

  { TdxNumberFormatElementDigitSpace }

  TdxNumberFormatElementDigitSpace = class(TdxNumberFormatElementDigitZero)
  public
    function Designator: Integer; override;
    procedure FormatEmpty(const AFormatSettings: TdxSpreadSheetCustomFormatSettings; AResult: TdxNumberFormatResult); override;
  end;

  { TdxNumberFormatElementTextContent }

  TdxNumberFormatElementTextContent = class(TdxNumberFormatElement)
  public
    function Designator: Integer; override;
    procedure Format(const AValue: Variant; AValueType: TdxSpreadSheetCellDataType;
      const AFormatSettings: TdxSpreadSheetCustomFormatSettings; AResult: TdxNumberFormatResult); override;
  end;

  { TdxNumberFormatElementTextBase }

  TdxNumberFormatElementTextBase = class abstract(TdxNumberFormatElement)
  strict private
    FText: string;
  public
    constructor Create(const AText: string); virtual;
    procedure Format(const AValue: Variant; AValueType: TdxSpreadSheetCellDataType;
      const AFormatSettings: TdxSpreadSheetCustomFormatSettings; AResult: TdxNumberFormatResult); override;
    //
    property Text: string read FText;
  end;

  { TdxNumberFormatElementBackslashedText }

  TdxNumberFormatElementBackslashedText = class(TdxNumberFormatElementTextBase)
  public
    function Designator: Integer; override;
  end;

  { TdxNumberFormatElementDisplayLocale }

  TdxNumberFormatElementDisplayLocale = class(TdxNumberFormatElementCondition)
  strict private
    FCurrency: string;
    FLocaleId: Integer;
  public
    constructor Create(ALocaleId: Integer; const ACurrency: string);
    procedure Format(const AValue: Variant; AValueType: TdxSpreadSheetCellDataType;
      const AFormatSettings: TdxSpreadSheetCustomFormatSettings; AResult: TdxNumberFormatResult); override;
    //
    property Currency: string read FCurrency;
    property LocaleId: Integer read FLocaleId;
  end;

  { TdxNumberFormatElementDateBase }

  TdxNumberFormatElementDateBase = class abstract(TdxNumberFormatElement)
  strict private
    FCount: Integer;
  protected
    procedure AdjustDateTime(var AValue: TDateTime; ADateSystem: TdxSpreadSheetDateTimeSystem); virtual;
    procedure FormatDateTime(const AMacro: string; AValue: TDateTime;
      const AFormatSettings: TdxSpreadSheetCustomFormatSettings; AResult: TdxNumberFormatResult);
  public
    constructor Create(ACount: Integer);
    //
    property Count: Integer read FCount;
  end;

  { TdxNumberFormatElementTimeBase }

  TdxNumberFormatElementTimeBase = class abstract(TdxNumberFormatElementDateBase)
  strict private
    FElapsed: Boolean;
  protected
    function ExtractTimeValue(const ADateTime: TDateTime): Int64; virtual; abstract;
  public
    constructor Create(ACount: Integer; AElapsed: Boolean);
    procedure Format(const AValue: Variant; AValueType: TdxSpreadSheetCellDataType;
      const AFormatSettings: TdxSpreadSheetCustomFormatSettings; AResult: TdxNumberFormatResult); override;
    //
    property Elapsed: Boolean read FElapsed;
  end;

  { TdxNumberFormatElementAmPm }

  TdxNumberFormatElementAmPm = class(TdxNumberFormatElementDateBase)
  strict private
    FIsAMLower: Boolean;
    FIsPMLower: Boolean;

    function GetSuffix(AIsAM: Boolean): string;
  public
    constructor Create;
    constructor CreateEx(AIsAMLower, AIsPMLower: Boolean);
    function Designator: Integer; override;
    procedure Format(const AValue: Variant; AValueType: TdxSpreadSheetCellDataType;
      const AFormatSettings: TdxSpreadSheetCustomFormatSettings; AResult: TdxNumberFormatResult); override;
  end;

  { TdxNumberFormatElementHours }

  TdxNumberFormatElementHours = class(TdxNumberFormatElementTimeBase)
  strict private
    FIs12HourTime: Boolean;
  protected
    function ExtractTimeValue(const ADateTime: TDateTime): Int64; override;
  public
    constructor Create(ACount: Integer; AElapsed, AIs12HourTime: Boolean);
    function Designator: Integer; override;

    property Is12HourTime: Boolean read FIs12HourTime write FIs12HourTime;
  end;

  { TdxNumberFormatElementMinutes }

  TdxNumberFormatElementMinutes = class(TdxNumberFormatElementTimeBase)
  protected
    function ExtractTimeValue(const ADateTime: TDateTime): Int64; override;
  public
    function Designator: Integer; override;
  end;

  { TdxNumberFormatElementSeconds }

  TdxNumberFormatElementSeconds = class(TdxNumberFormatElementTimeBase)
  protected
    function ExtractTimeValue(const ADateTime: TDateTime): Int64; override;
  public
    function Designator: Integer; override;
  end;

  { TdxNumberFormatElementMilliseconds }

  TdxNumberFormatElementMilliseconds = class(TdxNumberFormatElementTimeBase)
  protected
    function ExtractTimeValue(const ADateTime: TDateTime): Int64; override;
  public
    function Designator: Integer; override;
    procedure Format(const AValue: Variant; AValueType: TdxSpreadSheetCellDataType;
      const AFormatSettings: TdxSpreadSheetCustomFormatSettings; AResult: TdxNumberFormatResult); override;
  end;

  { TdxNumberFormatElementExprCondition }

  TdxNumberFormatElementExprCondition = class(TdxNumberFormatElementCondition)
  strict private
    FExpression: string;
  public
    constructor Create(const AExpression: string);
    procedure Format(const AValue: Variant; AValueType: TdxSpreadSheetCellDataType;
      const AFormatSettings: TdxSpreadSheetCustomFormatSettings; AResult: TdxNumberFormatResult); override;

    property Expression: string read FExpression;
  end;

  { TdxNumberFormatElementAsterisk }

  TdxNumberFormatElementAsterisk = class(TdxNumberFormatElementBackslashedText)
  public
    function Designator: Integer; override;
  end;

  { TdxNumberFormatElementUnderline }

  TdxNumberFormatElementUnderline = class(TdxNumberFormatElementBackslashedText)
  public
    constructor Create(const AText: string); override;
    function Designator: Integer; override;
  end;

  { TdxNumberFormatElementDecimalSeparator }

  TdxNumberFormatElementDecimalSeparator = class(TdxNumberFormatElement)
  public
    function Designator: Integer; override;
    procedure Format(const AValue: Variant; AValueType: TdxSpreadSheetCellDataType;
      const AFormatSettings: TdxSpreadSheetCustomFormatSettings; AResult: TdxNumberFormatResult); override;
  end;

  { TdxNumberFormatElementDefaultDateSeparator }

  TdxNumberFormatElementDefaultDateSeparator = class(TdxNumberFormatElement)
  public
    function Designator: Integer; override;
    procedure Format(const AValue: Variant; AValueType: TdxSpreadSheetCellDataType;
      const AFormatSettings: TdxSpreadSheetCustomFormatSettings; AResult: TdxNumberFormatResult); override;
  end;

  { TdxNumberFormatElementDateSeparator }

  TdxNumberFormatElementDateSeparator = class(TdxNumberFormatElement)
  public
    function Designator: Integer; override;
    procedure Format(const AValue: Variant; AValueType: TdxSpreadSheetCellDataType;
      const AFormatSettings: TdxSpreadSheetCustomFormatSettings; AResult: TdxNumberFormatResult); override;
  end;


  { TdxNumberFormatElementTimeSeparator }

  TdxNumberFormatElementTimeSeparator = class(TdxNumberFormatElement)
  public
    function Designator: Integer; override;
    procedure Format(const AValue: Variant; AValueType: TdxSpreadSheetCellDataType;
      const AFormatSettings: TdxSpreadSheetCustomFormatSettings; AResult: TdxNumberFormatResult); override;
  end;

  { TdxNumberFormatElementQuotedText }

  TdxNumberFormatElementQuotedText = class(TdxNumberFormatElementTextBase)
  public
    function Designator: Integer; override;
  end;

  { TdxNumberFormatElementDay }

  TdxNumberFormatElementDay = class(TdxNumberFormatElementDateBase)
  public
    function Designator: Integer; override;
    procedure Format(const AValue: Variant; AValueType: TdxSpreadSheetCellDataType;
      const AFormatSettings: TdxSpreadSheetCustomFormatSettings; AResult: TdxNumberFormatResult); override;
  end;

  { TdxNumberFormatElementDayOfWeek }

  TdxNumberFormatElementDayOfWeek = class(TdxNumberFormatElementDateBase)
  public
    function Designator: Integer; override;
    procedure Format(const AValue: Variant; AValueType: TdxSpreadSheetCellDataType;
      const AFormatSettings: TdxSpreadSheetCustomFormatSettings; AResult: TdxNumberFormatResult); override;
  end;

  { TdxNumberFormatElementMonth }

  TdxNumberFormatElementMonth = class(TdxNumberFormatElementDateBase)
  protected
    procedure AdjustDateTime(var AValue: TDateTime; ADateSystem: TdxSpreadSheetDateTimeSystem); override;
  public
    function Designator: Integer; override;
    procedure Format(const AValue: Variant; AValueType: TdxSpreadSheetCellDataType;
      const AFormatSettings: TdxSpreadSheetCustomFormatSettings; AResult: TdxNumberFormatResult); override;
  end;

  { TdxNumberFormatElementYear }

  TdxNumberFormatElementYear = class(TdxNumberFormatElementDateBase)
  protected
    procedure AdjustDateTime(var AValue: TDateTime; ADateSystem: TdxSpreadSheetDateTimeSystem); override;
  public
    function Designator: Integer; override;
    procedure Format(const AValue: Variant; AValueType: TdxSpreadSheetCellDataType;
      const AFormatSettings: TdxSpreadSheetCustomFormatSettings; AResult: TdxNumberFormatResult); override;
  end;

  { TdxNumberFormatElementThaiYear }

  TdxNumberFormatElementThaiYear = class(TdxNumberFormatElementYear)
  protected
    procedure AdjustDateTime(var AValue: TDateTime; ADateSystem: TdxSpreadSheetDateTimeSystem); override;
  public
    function Designator: Integer; override;
  end;

  { TdxNumberFormatElementInvariantYear }

  TdxNumberFormatElementInvariantYear = class(TdxNumberFormatElementYear)
  public
    constructor Create(ACount: Integer);
    function Designator: Integer; override;
  end;

  { TdxNumberFormatElementJapaneseEra }

  TdxNumberFormatElementJapaneseEra = class(TdxNumberFormatElementDateBase)
  public
    function Designator: Integer; override;
    procedure Format(const AValue: Variant; AValueType: TdxSpreadSheetCellDataType;
      const AFormatSettings: TdxSpreadSheetCustomFormatSettings; AResult: TdxNumberFormatResult); override;
  end;

  { TdxNumberFormatElementPercent }

  TdxNumberFormatElementPercent = class(TdxNumberFormatElement)
  public
    function Designator: Integer; override;
    procedure Format(const AValue: Variant; AValueType: TdxSpreadSheetCellDataType;
      const AFormatSettings: TdxSpreadSheetCustomFormatSettings; AResult: TdxNumberFormatResult); override;
  end;

//----------------------------------------------------------------------------------------------------------------------
// Number Formats
//----------------------------------------------------------------------------------------------------------------------

  { TdxDateTimeNumberFormatBase }

  TdxDateTimeNumberFormatBase = class(TdxSimpleNumberFormat)
  public const
    SystemLongDate = $F800;
    SystemLongTime = $F400;
  protected
    procedure FormatDateTime(const AValue: Variant; AValueType: TdxSpreadSheetCellDataType;
      const AFormatSettings: TdxSpreadSheetCustomFormatSettings; AResult: TdxNumberFormatResult); virtual;
    function GetValueType: TdxNumberFormatType; override;
  public
    procedure Format(const AValue: Variant; AValueType: TdxSpreadSheetCellDataType;
      const AFormatSettings: TdxSpreadSheetCustomFormatSettings; AResult: TdxNumberFormatResult); override;
    function HasMilliseconds: Boolean; virtual;
  end;

  { TdxDateTimeSystemDateTimeNumberFormat }

  TdxDateTimeSystemDateTimeNumberFormat = class abstract(TdxDateTimeNumberFormatBase)
  strict private
    FFormatTemplate: string;
    FFormatter: TdxNumberFormat;

    procedure SetFormatTemplate(const Value: string);
  protected
    procedure FormatDateTime(const AValue: Variant; AValueType: TdxSpreadSheetCellDataType;
      const AFormatSettings: TdxSpreadSheetCustomFormatSettings; AResult: TdxNumberFormatResult); override;
    function GetFormatString(const AData: TFormatSettings): string; virtual; abstract;
    //
    property FormatTemplate: string read FFormatTemplate write SetFormatTemplate;
    property Formatter: TdxNumberFormat read FFormatter;
  public
    destructor Destroy; override;
    procedure AfterConstruction; override;
    function IsTimeFormat: Boolean; override;
  end;

  { TdxDateTimeSystemLongDateNumberFormat }

  TdxDateTimeSystemLongDateNumberFormat = class(TdxDateTimeSystemDateTimeNumberFormat)
  protected
    function GetFormatString(const AData: TFormatSettings): string; override;
  end;

  { TdxDateTimeSystemLongTimeNumberFormat }

  TdxDateTimeSystemLongTimeNumberFormat = class(TdxDateTimeSystemDateTimeNumberFormat)
  protected
    function GetFormatString(const AData: TFormatSettings): string; override;
  end;

  { TdxDateTimeSystemShortDateNumberFormat }

  TdxDateTimeSystemShortDateNumberFormat = class(TdxDateTimeSystemDateTimeNumberFormat)
  protected
    function GetFormatString(const AData: TFormatSettings): string; override;
  end;

  { TdxDateTimeNumberFormat }

  TdxDateTimeNumberFormat = class(TdxDateTimeNumberFormatBase)
  strict private
    FHasMilliseconds: Boolean;
    FLocale: TdxNumberFormatElementDisplayLocale;
  public
    constructor Create(AElements: TdxNumberFormatElementList; ALocale: TdxNumberFormatElementDisplayLocale; AHasMilliseconds: Boolean);
    procedure Format(const AValue: Variant; AValueType: TdxSpreadSheetCellDataType;
      const AFormatSettings: TdxSpreadSheetCustomFormatSettings; AResult: TdxNumberFormatResult); override;
    function HasMilliseconds: Boolean; override;
  end;

  { TdxGeneralNumberFormat }

  TdxGeneralNumberFormat = class(TdxSimpleNumberFormat)
  strict private
    class var FDefault: TdxGeneralNumberFormat;

    class function GetDefault: TdxGeneralNumberFormat; static;
  protected
    function GetValueType: TdxNumberFormatType; override;
    class procedure Finalize;
  public
    constructor Create; overload;
    procedure Format(const AValue: Variant; AValueType: TdxSpreadSheetCellDataType;
      const AFormatSettings: TdxSpreadSheetCustomFormatSettings; AResult: TdxNumberFormatResult); override;
    function IsTimeFormat: Boolean; override;
    //
    class property Default: TdxGeneralNumberFormat read GetDefault;
  end;

  { TdxTextNumberFormat }

  TdxTextNumberFormat = class(TdxSimpleNumberFormat)
  protected
    function GetValueType: TdxNumberFormatType; override;
  public
    procedure Format(const AValue: Variant; AValueType: TdxSpreadSheetCellDataType;
      const AFormatSettings: TdxSpreadSheetCustomFormatSettings; AResult: TdxNumberFormatResult); override;
  end;

  { TdxConditionalNumberFormat }

  TdxConditionalNumberFormat = class(TdxGeneralNumberFormat)
  strict private
    FParts: TdxSimpleNumberFormatList;

    function CalculateActualPart(const AValue: Variant; AValueType: TdxSpreadSheetCellDataType): TdxNumberFormat;
  protected
    function GetValueType: TdxNumberFormatType; override;
    //
    property Parts: TdxSimpleNumberFormatList read FParts;
  public
    constructor Create(AFormats: TdxSimpleNumberFormatList);
    destructor Destroy; override;
    procedure Format(const AValue: Variant; AValueType: TdxSpreadSheetCellDataType;
      const AFormatSettings: TdxSpreadSheetCustomFormatSettings; AResult: TdxNumberFormatResult); override;
    function IsTimeFormat: Boolean; override;
  end;

  { TdxSimpleNumericNumberFormat }

  TdxSimpleNumericNumberFormat = class(TdxSimpleNumberFormat)
  protected

    FDecimalCount: Integer;
    FDecimalSeparatorIndex: Integer;
    FDisplayFactor: Double;
    FGrouping: Boolean;
    FIntegerCount: Integer;
    FIsNegativePart: Boolean;
    FPercentCount: Integer;

    procedure InsertSign(ABuffer: TStringBuilder; AIsNegative: Boolean; AIndex: Integer = 0);
    procedure FormatDecimalPart(ABuffer: TdxNumberFormatResult; const AValue: TdxNumberFormatFloatRec;
      AStartIndex, AEndIndex, ADigits: Integer; const AFormatSettings: TdxSpreadSheetCustomFormatSettings);
    procedure FormatIntegerPart(ABuffer: TdxNumberFormatResult; const AValue: TdxNumberFormatFloatRec;
      AStartIndex, AEndIndex, ADigitCount: Integer; const AFormatSettings: TdxSpreadSheetCustomFormatSettings); overload;
    function FormatIntegerPart(const AValue: TdxNumberFormatFloatRec; AStartIndex, AEndIndex, ADigitCount: Integer;
      const AFormatSettings: TdxSpreadSheetCustomFormatSettings): string; overload;
    procedure FormatSimple(const AValue: TdxNumberFormatFloatRec; AEndIndex: Integer;
      const AFormatSettings: TdxSpreadSheetCustomFormatSettings; AResult: TdxNumberFormatResult);

    function GetValueType: TdxNumberFormatType; override;
  public
    constructor Create(AElements: TdxNumberFormatElementList;
      APercentCount, AIntegerCount, ADecimalCount, ADisplayFactor, ADecimalSeparatorIndex: Integer;
      AGrouping, AIsNegativePart: Boolean);
    procedure Format(const AValue: Variant; AValueType: TdxSpreadSheetCellDataType;
      const AFormatSettings: TdxSpreadSheetCustomFormatSettings; AResult: TdxNumberFormatResult); override;
  end;

  { TdxNumericNumberFormatExponent }

  TdxNumericNumberFormatExponent = class(TdxSimpleNumericNumberFormat)
  strict private
    FExpCount: Integer;
    FExpIndex: Integer;
    FExplicitSign: Boolean;

    function CalculateExponent(const AValue: TdxNumberFormatFloatRec): Integer;
  public
    constructor Create(AElements: TdxNumberFormatElementList;
      AIntegerCount, ADecimalCount, ADecimalSeparatorIndex, AExpIndex, AExpCount: Integer;
      AExplicitSign, AGrouping, AIsNegativePart: Boolean);
    procedure Format(const AValue: Variant; AValueType: TdxSpreadSheetCellDataType;
      const AFormatSettings: TdxSpreadSheetCustomFormatSettings; AResult: TdxNumberFormatResult); override;
  end;

  { TdxNumericFractionNumberFormat }

  TdxNumericFractionNumberFormat = class(TdxSimpleNumericNumberFormat)
  protected
    FDividendCount: Integer;
    FDivisorCount: Integer;
    FDivisorIndex: Integer;

    function CalculateRationalApproximation(AValue: Double): TPair<Double, Integer>; virtual;
    procedure FormatDivisor(ADivisor, AEndIndex: Integer;
      const AFormatSettings: TdxSpreadSheetCustomFormatSettings; AResult: TdxNumberFormatResult); virtual;
    procedure FormatZeroIntegerPart(AStartIndex, AEndIndex: Integer;
      const AFormatSettings: TdxSpreadSheetCustomFormatSettings; AResult: TdxNumberFormatResult);
  public
    constructor Create(AElements: TdxNumberFormatElementList;
      APercentCount, AIntegerCount, APreFractionIndex, AFractionSeparatorIndex: Integer;
      ADivisorIndex, ADividendCount, ADivisorCount: Integer; AGrouping, AIsNegativePart: Boolean);
    procedure Format(const AValue: Variant; AValueType: TdxSpreadSheetCellDataType;
      const AFormatSettings: TdxSpreadSheetCustomFormatSettings; AResult: TdxNumberFormatResult); override;
  end;

  { TdxExplicitNumericFractionNumberFormat }

  TdxExplicitNumericFractionNumberFormat = class(TdxNumericFractionNumberFormat)
  protected
    function CalculateRationalApproximation(AValue: Double): TPair<Double, Integer>; override;
    procedure FormatDivisor(ADivisor, AEndIndex: Integer;
      const AFormatSettings: TdxSpreadSheetCustomFormatSettings; AResult: TdxNumberFormatResult); override;
  public
    property ExplicitDivisor: Integer read FDivisorCount;
  end;

implementation

uses
  dxSpreadSheetNumberFormatParser, dxSpreadSheetNumberFormat;

{ TdxNumberFormatResult }

procedure TdxNumberFormatResult.AfterConstruction;
begin
  inherited AfterConstruction;
  Color := clDefault;
  IsError := False;
  IsText := bDefault;
end;

procedure TdxNumberFormatResult.FillBySpaces(AStartIndex: Integer);
begin
  while AStartIndex < Length do
  begin
    Chars[AStartIndex] := ' ';
    Inc(AStartIndex);
  end;
end;

{ TdxNumberFormatFloatRec }

class function TdxNumberFormatFloatRec.Create(const AValue: Extended; ADecimals: Integer): TdxNumberFormatFloatRec;
begin
  Result.Initialize(AValue, 9999);
  Result.RoundTo(ADecimals);
end;

function TdxNumberFormatFloatRec.GetDigit(AIndex: Integer): Byte;
begin
  if InRange(AIndex, 0, MaxDigits - 1) then
  begin
    Result := Byte(Data.Digits[AIndex]);
    if Result <> 0 then
      Dec(Result, Ord('0'));
  end
  else
    Result := 0;
end;

function TdxNumberFormatFloatRec.SetDigit(AIndex: Integer; AValue: Byte): Boolean;
begin
  Result := InRange(AIndex, 0, MaxDigits - 1) and InRange(AValue, 0, 9);
  if Result then
  {$IFDEF DELPHIXE3}
    Data.Digits[AIndex] := AValue + Ord('0');
  {$ELSE}
    Data.Digits[AIndex] := AnsiChar(AValue + Ord('0'));
  {$ENDIF}
end;

procedure TdxNumberFormatFloatRec.RoundTo(ADecimals: Integer);
var
  ARoundIndex, AIndex: Integer;
begin
  ARoundIndex := FirstDecimal + ADecimals;
  if ARoundIndex >= DigitCount then
    Exit;
  if ARoundIndex < 0 then
  begin
    Initialize(0, 0);
    Exit;
  end;
  if FirstDecimal < 0 then
  begin
    MoveDigits(-FirstDecimal);
    ARoundIndex := ADecimals;
  end;

  AIndex := ARoundIndex + 1;
  while (AIndex < MaxDigits) and (GetDigit(AIndex) = 9) do
    Inc(AIndex);
  if AIndex >= MaxDigits - 2 then
    SetDigit(ARoundIndex, GetDigit(ARoundIndex) + 1);

  if GetDigit(ARoundIndex) >= 5 then
  begin
    repeat
      Dec(ARoundIndex);
      if ARoundIndex < 0 then
      begin
        MoveDigits(-ARoundIndex);
        ARoundIndex := 0;
      end;
    until GetDigit(ARoundIndex) <> 9;
    SetDigit(ARoundIndex, GetDigit(ARoundIndex) + 1);
  end;

  Inc(ARoundIndex);
  while ARoundIndex < MaxDigits do
  begin
    SetDigit(ARoundIndex, 0);
    Inc(ARoundIndex);
  end;

  CalculateDigitCount(ADecimals);
end;

procedure TdxNumberFormatFloatRec.CalculateDigitCount(ADecimals: Integer);
var
  I: Integer;
begin
  ADecimals := Min(FirstDecimal + ADecimals, MaxDigits);
  for I := ADecimals - 1 downto 0 do
    if GetDigit(I) <> 0 then
    begin
      DigitCount := I + 1;
      Break;
    end;

  DigitCount := Max(DigitCount, FirstDecimal);
end;

procedure TdxNumberFormatFloatRec.Initialize(const AValue: Extended; ADecimals: Integer);
begin
  FillChar(Self, SizeOf(Self), 0);
  FloatToDecimal(Data, AValue, fvExtended, 18, ADecimals);
  FirstDecimal := Data.Exponent;
  CalculateDigitCount(ADecimals);
end;

procedure TdxNumberFormatFloatRec.MoveDigits(ADelta: Integer);
var
  AIndex: Integer;
begin
  if ADelta > 0 then
  begin
    for AIndex := MaxDigits - 1 downto ADelta do
      SetDigit(AIndex, GetDigit(AIndex - ADelta));
    for AIndex := 0 to ADelta - 1 do
      SetDigit(AIndex, 0);
  end
  else
  begin
    for AIndex := 0 to MaxDigits - ADelta - 1 do
      SetDigit(AIndex, GetDigit(AIndex + ADelta));
    for AIndex := MaxDigits - ADelta to MaxDigits - 1 do
      SetDigit(AIndex, 0);
  end;
  Inc(Data.Exponent, ADelta);
  Inc(FirstDecimal, ADelta);
end;

{ TdxNumberFormat }

procedure TdxNumberFormat.Format(const AValue: Variant; AValueType: TdxSpreadSheetCellDataType;
  const AFormatSettings: TdxSpreadSheetCustomFormatSettings; AResult: TdxNumberFormatResult);
var
  AText: string;
begin
  case AValueType of
    cdtBlank:
      {do nothing};
    cdtBoolean:
      AResult.Append(dxBoolToString[Boolean(AValue)]);
    cdtError:
      AResult.Append(dxSpreadSheetErrorCodeToString(TdxSpreadSheetFormulaErrorCode(AValue)));
    cdtCurrency, cdtFloat, cdtDateTime, cdtInteger:
      AResult.Append(dxFloatToStr(AValue, AFormatSettings.Data));
    cdtString:
      begin
        if VarIsNull(AValue) then
          AText := ''
        else
          AText := AValue;

        if (AText <> '') and (AText[1] = '''') then
          Delete(AText, 1, 1);
        AResult.Append(AText);
      end;
  else
    raise EdxSpreadSheetNumberFormatError.CreateFmt(cxGetResourceString(@sdxErrorInternal), ['cannot format variant']);
  end;
end;

{ TdxNumberFormatElement }

function TdxNumberFormatElement.Format(
  const AValue: Variant; AValueType: TdxSpreadSheetCellDataType;
  const AFormatSettings: TdxSpreadSheetCustomFormatSettings; var AColor: TColor): string;
var
  AResult: TdxNumberFormatResult;
begin
  AResult := TdxNumberFormatResult.Create(4);
  try
    AResult.Color := AColor;
    Format(AValue, AValueType, AFormatSettings, AResult);
    Result := AResult.ToString;
    AColor := AResult.Color;
  finally
    AResult.Free;
  end;
end;

function TdxNumberFormatElement.FormatEmpty(
  const AFormatSettings: TdxSpreadSheetCustomFormatSettings; var AColor: TColor): string;
var
  AResult: TdxNumberFormatResult;
begin
  AResult := TdxNumberFormatResult.Create(4);
  try
    AResult.Color := AColor;
    FormatEmpty(AFormatSettings, AResult);
    Result := AResult.ToString;
    AColor := AResult.Color;
  finally
    AResult.Free;
  end;
end;

procedure TdxNumberFormatElement.FormatEmpty(
  const AFormatSettings: TdxSpreadSheetCustomFormatSettings; AResult: TdxNumberFormatResult);
begin
  Format(Null, cdtBlank, AFormatSettings, AResult);
end;

function TdxNumberFormatElement.IsDigit: Boolean;
begin
  Result := False;
end;

{ TdxNumberFormatElementList }

procedure TdxNumberFormatElementList.ExtractFrom(Source: TdxNumberFormatElementList);
begin
  AddRange(Source);
  if Source.OwnsObjects then
  begin
    Source.OwnsObjects := False;
    Source.Clear;
    Source.OwnsObjects := True;
  end
  else
    Source.Clear;
end;

{ TdxSimpleNumberFormat }

constructor TdxSimpleNumberFormat.Create(AElements: TdxNumberFormatElementList);
begin
  FList := TdxNumberFormatElementList.Create;
  if AElements <> nil then
    FList.ExtractFrom(AElements);
end;

destructor TdxSimpleNumberFormat.Destroy;
begin
  FreeAndNil(FList);
  inherited Destroy;
end;

function TdxSimpleNumberFormat.IsTimeFormat: Boolean;
var
  AItem: TdxNumberFormatElement;
begin
  if ValueType <> nftDateTime then
    Exit(False);

  for AItem in FList do
  begin
    if (AItem.Designator = TdxNumberFormatDesignator.Day) or
       (AItem.Designator = TdxNumberFormatDesignator.Month) or
       (AItem.Designator = TdxNumberFormatDesignator.Year) or
       (AItem.Designator = TdxNumberFormatDesignator.DayOfWeek) or
       (AItem.Designator = TdxNumberFormatDesignator.InvariantYear) or
       (AItem.Designator = TdxNumberFormatDesignator.JapaneseEra) or
       (AItem.Designator = TdxNumberFormatDesignator.ThaiYear)
    then
      Exit(False);
  end;
  Result := True;
end;

{ TdxSimpleNumberFormatList }

procedure TdxSimpleNumberFormatList.ExtractFrom(Source: TdxSimpleNumberFormatList);
begin
  AddRange(Source);
  if Source.OwnsObjects then
  begin
    Source.OwnsObjects := False;
    Source.Clear;
    Source.OwnsObjects := True;
  end
  else
    Source.Clear;
end;

//----------------------------------------------------------------------------------------------------------------------
// Elements
//----------------------------------------------------------------------------------------------------------------------

{ TdxNumberFormatElementGeneral }

function TdxNumberFormatElementGeneral.Designator: Integer;
begin
  Result := TdxNumberFormatDesignator.Default;
end;

procedure TdxNumberFormatElementGeneral.Format(const AValue: Variant; AValueType: TdxSpreadSheetCellDataType;
  const AFormatSettings: TdxSpreadSheetCustomFormatSettings; AResult: TdxNumberFormatResult);
begin
  TdxGeneralNumberFormat.Default.Format(AValue, AValueType, AFormatSettings, AResult);
end;

{ TdxNumberFormatElementCondition }

function TdxNumberFormatElementCondition.Designator: Integer;
begin
  Result := TdxNumberFormatDesignator.Bracket;
end;

{ TdxNumberFormatElementColor }

constructor TdxNumberFormatElementColor.Create(AColor: TColor);
begin
  inherited Create;
  FColor := AColor;
end;

procedure TdxNumberFormatElementColor.Format(
  const AValue: Variant; AValueType: TdxSpreadSheetCellDataType;
  const AFormatSettings: TdxSpreadSheetCustomFormatSettings; AResult: TdxNumberFormatResult);
begin
  AResult.Color := Color;
end;

{ TdxNumberFormatElementDigitZero }

function TdxNumberFormatElementDigitZero.Designator: Integer;
begin
  Result := TdxNumberFormatDesignator.DigitZero;
end;

procedure TdxNumberFormatElementDigitZero.Format(
  const AValue: Variant; AValueType: TdxSpreadSheetCellDataType;
  const AFormatSettings: TdxSpreadSheetCustomFormatSettings; AResult: TdxNumberFormatResult);
begin
  AResult.Append(Integer(AValue));
end;

procedure TdxNumberFormatElementDigitZero.FormatEmpty(
  const AFormatSettings: TdxSpreadSheetCustomFormatSettings; AResult: TdxNumberFormatResult);
begin
  AResult.Append('0');
end;

function TdxNumberFormatElementDigitZero.IsDigit: Boolean;
begin
  Result := True;
end;

{ TdxNumberFormatElementDigitEmpty }

function TdxNumberFormatElementDigitEmpty.Designator: Integer;
begin
  Result := TdxNumberFormatDesignator.DigitEmpty;
end;

procedure TdxNumberFormatElementDigitEmpty.FormatEmpty(
  const AFormatSettings: TdxSpreadSheetCustomFormatSettings; AResult: TdxNumberFormatResult);
begin
  // do nothing
end;

{ TdxNumberFormatElementDigitSpace }

function TdxNumberFormatElementDigitSpace.Designator: Integer;
begin
  Result := TdxNumberFormatDesignator.DigitSpace;
end;

procedure TdxNumberFormatElementDigitSpace.FormatEmpty(
  const AFormatSettings: TdxSpreadSheetCustomFormatSettings; AResult: TdxNumberFormatResult);
begin
  AResult.Append(' ');
end;

{ TdxNumberFormatElementTextContent }

function TdxNumberFormatElementTextContent.Designator: Integer;
begin
  Result := TdxNumberFormatDesignator.At;
end;

procedure TdxNumberFormatElementTextContent.Format(
  const AValue: Variant; AValueType: TdxSpreadSheetCellDataType;
  const AFormatSettings: TdxSpreadSheetCustomFormatSettings; AResult: TdxNumberFormatResult);
begin
  TdxGeneralNumberFormat.Default.Format(AValue, AValueType, AFormatSettings, AResult);
  AResult.IsText := bTrue;
end;

{ TdxNumberFormatElementTextBase }

constructor TdxNumberFormatElementTextBase.Create(const AText: string);
begin
  FText := AText;
end;

procedure TdxNumberFormatElementTextBase.Format(
  const AValue: Variant; AValueType: TdxSpreadSheetCellDataType;
  const AFormatSettings: TdxSpreadSheetCustomFormatSettings; AResult: TdxNumberFormatResult);
begin
  AResult.Append(Text);
  AResult.IsText := bTrue;
end;

{ TdxNumberFormatElementBackslashedText }

function TdxNumberFormatElementBackslashedText.Designator: Integer;
begin
  Result := TdxNumberFormatDesignator.Backslash;
end;

{ TdxNumberFormatElementDisplayLocale }

constructor TdxNumberFormatElementDisplayLocale.Create(ALocaleId: Integer; const ACurrency: string);
begin
  inherited Create;
  FCurrency := ACurrency;
  FLocaleId := ALocaleId;
end;

procedure TdxNumberFormatElementDisplayLocale.Format(
  const AValue: Variant; AValueType: TdxSpreadSheetCellDataType;
  const AFormatSettings: TdxSpreadSheetCustomFormatSettings; AResult: TdxNumberFormatResult);
begin
  AResult.Append(FCurrency);
end;

{ TdxNumberFormatElementDateBase }

constructor TdxNumberFormatElementDateBase.Create(ACount: Integer);
begin
  FCount := ACount;
end;

procedure TdxNumberFormatElementDateBase.AdjustDateTime(var AValue: TDateTime; ADateSystem: TdxSpreadSheetDateTimeSystem);
begin
  AValue := dxDateTimeToRealDateTime(AValue, ADateSystem);
end;

procedure TdxNumberFormatElementDateBase.FormatDateTime(const AMacro: string; AValue: TDateTime;
  const AFormatSettings: TdxSpreadSheetCustomFormatSettings; AResult: TdxNumberFormatResult);
begin
  try
    AdjustDateTime(AValue, AFormatSettings.DateTimeSystem);
    if (AValue < 0) or (AValue >= MaxDateTime) then
    begin
      AResult.IsError := True;
      AResult.Append('#');
    end
    else
      AResult.Append(SysUtils.FormatDateTime(DupeString(AMacro, Count), AValue, AFormatSettings.Data));
  except
    AResult.IsError := True;
  end;
end;

{ TdxNumberFormatElementTimeBase }

constructor TdxNumberFormatElementTimeBase.Create(ACount: Integer; AElapsed: Boolean);
begin
  inherited Create(ACount);
  FElapsed := AElapsed;
end;

procedure TdxNumberFormatElementTimeBase.Format(
  const AValue: Variant; AValueType: TdxSpreadSheetCellDataType;
  const AFormatSettings: TdxSpreadSheetCustomFormatSettings; AResult: TdxNumberFormatResult);
begin
  AResult.Append(FormatFloat(DupeString('0', Count), ExtractTimeValue(AValue)));
end;

{ TdxNumberFormatElementAmPm }

constructor TdxNumberFormatElementAmPm.Create;
begin
  inherited Create(2);
end;

constructor TdxNumberFormatElementAmPm.CreateEx(AIsAMLower, AIsPMLower: Boolean);
begin
  inherited Create(1);
  FIsAMLower := AIsAMLower;
  FIsPMLower := AIsPMLower;
end;

function TdxNumberFormatElementAmPm.Designator: Integer;
begin
  Result := TdxNumberFormatDesignator.AmPm;
end;

procedure TdxNumberFormatElementAmPm.Format(
  const AValue: Variant; AValueType: TdxSpreadSheetCellDataType;
  const AFormatSettings: TdxSpreadSheetCustomFormatSettings; AResult: TdxNumberFormatResult);
begin
  AResult.Append(GetSuffix(Frac(AValue) < 0.5));
end;

function TdxNumberFormatElementAmPm.GetSuffix(AIsAM: Boolean): string;
begin
  if AIsAM then
  begin
    if Count = 1 then
      Result := IfThen(FIsAmLower, 'a', 'A')
    else
      Result := IfThen(FIsAmLower, 'am', 'AM');
  end
  else
    if Count = 1 then
      Result := IfThen(FIsPMLower, 'p', 'P')
    else
      Result := IfThen(FIsPMLower, 'pm', 'PM');
end;

{ TdxNumberFormatElementHours }

constructor TdxNumberFormatElementHours.Create(ACount: Integer; AElapsed, AIs12HourTime: Boolean);
begin
  inherited Create(ACount, AElapsed);
  FIs12HourTime := AIs12HourTime;
end;

function TdxNumberFormatElementHours.Designator: Integer;
begin
  Result := TdxNumberFormatDesignator.Hour;
end;

function TdxNumberFormatElementHours.ExtractTimeValue(const ADateTime: TDateTime): Int64;
var
  H, X: Word;
begin
  if Elapsed then
    Result := Trunc(RoundTo(ADateTime * 24.0, -6))
  else
  begin
    DecodeTime(ADateTime, H, X, X, X);
    Result := H;
  end;

  if Is12HourTime then
  begin
    Result := Result mod 12;
    if Result = 0 then
      Result := 12;
  end;
end;

{ TdxNumberFormatElementMinutes }

function TdxNumberFormatElementMinutes.Designator: Integer;
begin
  Result := TdxNumberFormatDesignator.Minute;
end;

function TdxNumberFormatElementMinutes.ExtractTimeValue(const ADateTime: TDateTime): Int64;
var
  M, X: Word;
begin
  if Elapsed then
    Result := Trunc(ADateTime * 24 * 60)
  else
  begin
    DecodeTime(ADateTime, X, M, X, X);
    Result := M;
  end;
end;

{ TdxNumberFormatElementSeconds }

function TdxNumberFormatElementSeconds.Designator: Integer;
begin
  Result := TdxNumberFormatDesignator.Second;
end;

function TdxNumberFormatElementSeconds.ExtractTimeValue(const ADateTime: TDateTime): Int64;
var
  AResult: Extended;
  S, X: Word;
begin
  if Elapsed then
  begin
    AResult := ADateTime * 24 * 60 * 60;
    Result := Trunc(AResult);
    if Abs(AResult - Result - 1) < 1E-9 then
      Inc(Result);
  end
  else
  begin
    DecodeTime(ADateTime, X, X, S, X);
    Result := S;
  end;
end;

{ TdxNumberFormatElementMilliseconds }

function TdxNumberFormatElementMilliseconds.Designator: Integer;
begin
  Result := TdxNumberFormatDesignator.DigitZero;
end;

function TdxNumberFormatElementMilliseconds.ExtractTimeValue(const ADateTime: TDateTime): Int64;
var
  M, X: Word;
begin
  DecodeTime(ADateTime, X, X, X, M);
  if Count < 3 then
    Result := Trunc(M / Power(10, 3 - Count))
  else
    Result := M;
end;

procedure TdxNumberFormatElementMilliseconds.Format(
  const AValue: Variant; AValueType: TdxSpreadSheetCellDataType;
  const AFormatSettings: TdxSpreadSheetCustomFormatSettings; AResult: TdxNumberFormatResult);
begin
  AResult.Append(AFormatSettings.Data.DecimalSeparator);
  inherited Format(AValue, AValueType, AFormatSettings, AResult);
end;

{ TdxNumberFormatElementExprCondition }

constructor TdxNumberFormatElementExprCondition.Create(const AExpression: string);
begin
  FExpression := AExpression;
end;

procedure TdxNumberFormatElementExprCondition.Format(
  const AValue: Variant; AValueType: TdxSpreadSheetCellDataType;
  const AFormatSettings: TdxSpreadSheetCustomFormatSettings; AResult: TdxNumberFormatResult);
begin
  AResult.Append('[' + Expression + ']');
  AResult.IsText := bTrue;
end;

{ TdxNumberFormatElementAsterisk }

function TdxNumberFormatElementAsterisk.Designator: Integer;
begin
  Result := TdxNumberFormatDesignator.Asterisk;
end;

{ TdxNumberFormatElementUnderline }

constructor TdxNumberFormatElementUnderline.Create(const AText: string);
begin
  inherited Create(' ');
end;

function TdxNumberFormatElementUnderline.Designator: Integer;
begin
  Result := TdxNumberFormatDesignator.Underline;
end;

{ TdxNumberFormatElementDecimalSeparator }

function TdxNumberFormatElementDecimalSeparator.Designator: Integer;
begin
  Result := TdxNumberFormatDesignator.DecimalSeparator;
end;

procedure TdxNumberFormatElementDecimalSeparator.Format(
  const AValue: Variant; AValueType: TdxSpreadSheetCellDataType;
  const AFormatSettings: TdxSpreadSheetCustomFormatSettings; AResult: TdxNumberFormatResult);
begin
  AResult.Append(AFormatSettings.Data.DecimalSeparator);
end;

{ TdxNumberFormatElementDefaultDateSeparator }

function TdxNumberFormatElementDefaultDateSeparator.Designator: Integer;
begin
  Result := TdxNumberFormatDesignator.FractionOrDateSeparator;
end;

procedure TdxNumberFormatElementDefaultDateSeparator.Format(
  const AValue: Variant; AValueType: TdxSpreadSheetCellDataType;
  const AFormatSettings: TdxSpreadSheetCustomFormatSettings; AResult: TdxNumberFormatResult);
begin
  AResult.Append('/');
end;

{ TdxNumberFormatElementDateSeparator }

function TdxNumberFormatElementDateSeparator.Designator: Integer;
begin
  Result := TdxNumberFormatDesignator.DateSeparator;
end;

procedure TdxNumberFormatElementDateSeparator.Format(
  const AValue: Variant; AValueType: TdxSpreadSheetCellDataType;
  const AFormatSettings: TdxSpreadSheetCustomFormatSettings; AResult: TdxNumberFormatResult);
begin
  AResult.Append(AFormatSettings.Data.DateSeparator);
end;

{ TdxNumberFormatElementTimeSeparator }

function TdxNumberFormatElementTimeSeparator.Designator: Integer;
begin
  Result := TdxNumberFormatDesignator.TimeSeparator;
end;

procedure TdxNumberFormatElementTimeSeparator.Format(
  const AValue: Variant; AValueType: TdxSpreadSheetCellDataType;
  const AFormatSettings: TdxSpreadSheetCustomFormatSettings; AResult: TdxNumberFormatResult);
begin
  AResult.Append(AFormatSettings.Data.TimeSeparator);
end;

{ TdxNumberFormatElementQuotedText }

function TdxNumberFormatElementQuotedText.Designator: Integer;
begin
  Result := TdxNumberFormatDesignator.Quote;
end;

{ TdxNumberFormatElementDay }

function TdxNumberFormatElementDay.Designator: Integer;
begin
  Result := TdxNumberFormatDesignator.Day;
end;

procedure TdxNumberFormatElementDay.Format(
  const AValue: Variant; AValueType: TdxSpreadSheetCellDataType;
  const AFormatSettings: TdxSpreadSheetCustomFormatSettings; AResult: TdxNumberFormatResult);
var
  ADateTime: TDateTime;
begin
  ADateTime := AValue;
  if (Count <= 2) and (AFormatSettings.DateTimeSystem = dts1900) then
  begin
    if ADateTime < 1 then
    begin
      AResult.Append(DupeString('0', Count));
      Exit;
    end;
    if Trunc(ADateTime) = 60 then
    begin
      AResult.Append('29');
      Exit;
    end;
    if ADateTime < 61 then
      ADateTime := ADateTime + 1;
    ADateTime := Max(ADateTime, 2);
  end;
  FormatDateTime('d', ADateTime, AFormatSettings, AResult);
end;

{ TdxNumberFormatElementDayOfWeek }

function TdxNumberFormatElementDayOfWeek.Designator: Integer;
begin
  Result := TdxNumberFormatDesignator.DayOfWeek;
end;

procedure TdxNumberFormatElementDayOfWeek.Format(
  const AValue: Variant; AValueType: TdxSpreadSheetCellDataType;
  const AFormatSettings: TdxSpreadSheetCustomFormatSettings; AResult: TdxNumberFormatResult);
begin
  FormatDateTime('d', AValue, AFormatSettings, AResult);
end;

{ TdxNumberFormatElementMonth }

function TdxNumberFormatElementMonth.Designator: Integer;
begin
  Result := TdxNumberFormatDesignator.Month;
end;

procedure TdxNumberFormatElementMonth.Format(
  const AValue: Variant; AValueType: TdxSpreadSheetCellDataType;
  const AFormatSettings: TdxSpreadSheetCustomFormatSettings; AResult: TdxNumberFormatResult);
var
  ATempResult: TdxNumberFormatResult;
begin
  if Count > 4 then
  begin
    ATempResult := TdxNumberFormatResult.Create;
    try
      FormatDateTime('M', AValue, AFormatSettings, ATempResult);
      if ATempResult.Length > 0 then
        AResult.Append(ATempResult.Chars[0]);
    finally
      ATempResult.Free;
    end;
  end
  else
    FormatDateTime('M', AValue, AFormatSettings, AResult);
end;

procedure TdxNumberFormatElementMonth.AdjustDateTime(var AValue: TDateTime; ADateSystem: TdxSpreadSheetDateTimeSystem);
begin
  inherited AdjustDateTime(AValue, ADateSystem);

  if AValue < 61 then
  begin
    if AValue < 60 then
      AValue := AValue + 1;
    if AValue >= 60 then
      AValue := AValue - 1;
    AValue := Max(AValue, 2);
  end;
end;

{ TdxNumberFormatElementYear }

procedure TdxNumberFormatElementYear.AdjustDateTime(var AValue: TDateTime; ADateSystem: TdxSpreadSheetDateTimeSystem);
begin
  inherited AdjustDateTime(AValue, ADateSystem);

  AValue := Max(AValue, 2);
end;

function TdxNumberFormatElementYear.Designator: Integer;
begin
  Result := TdxNumberFormatDesignator.Year;
end;

procedure TdxNumberFormatElementYear.Format(
  const AValue: Variant; AValueType: TdxSpreadSheetCellDataType;
  const AFormatSettings: TdxSpreadSheetCustomFormatSettings; AResult: TdxNumberFormatResult);
begin
  FormatDateTime('Y', AValue, AFormatSettings, AResult)
end;

{ TdxNumberFormatElementThaiYear }

function TdxNumberFormatElementThaiYear.Designator: Integer;
begin
  Result := TdxNumberFormatDesignator.ThaiYear;
end;

procedure TdxNumberFormatElementThaiYear.AdjustDateTime(var AValue: TDateTime; ADateSystem: TdxSpreadSheetDateTimeSystem);
var
  Y, M, D, H, S, N, MS: Word;
begin
  inherited AdjustDateTime(AValue, ADateSystem);
  DecodeDate(AValue, Y, M, D);
  DecodeTime(AValue, H, N, S, MS);
  AValue := EncodeDate(Y + 543, M, D) + EncodeTime(H, M, S, MS);
end;

{ TdxNumberFormatElementInvariantYear }

constructor TdxNumberFormatElementInvariantYear.Create(ACount: Integer);
begin
  inherited Create(4);
end;

function TdxNumberFormatElementInvariantYear.Designator: Integer;
begin
  Result := TdxNumberFormatDesignator.InvariantYear;
end;

{ TdxNumberFormatElementJapaneseEra }

function TdxNumberFormatElementJapaneseEra.Designator: Integer;
begin
  Result := TdxNumberFormatDesignator.JapaneseEra;
end;

procedure TdxNumberFormatElementJapaneseEra.Format(
  const AValue: Variant; AValueType: TdxSpreadSheetCellDataType;
  const AFormatSettings: TdxSpreadSheetCustomFormatSettings; AResult: TdxNumberFormatResult);
begin
end;

{ TdxNumberFormatElementPercent }

function TdxNumberFormatElementPercent.Designator: Integer;
begin
  Result := TdxNumberFormatDesignator.Percent;
end;

procedure TdxNumberFormatElementPercent.Format(
  const AValue: Variant; AValueType: TdxSpreadSheetCellDataType;
  const AFormatSettings: TdxSpreadSheetCustomFormatSettings; AResult: TdxNumberFormatResult);
begin
  AResult.Append('%');
end;

//----------------------------------------------------------------------------------------------------------------------
// NumberFormats
//----------------------------------------------------------------------------------------------------------------------

{ TdxDateTimeNumberFormatBase }

procedure TdxDateTimeNumberFormatBase.Format(
  const AValue: Variant; AValueType: TdxSpreadSheetCellDataType;
  const AFormatSettings: TdxSpreadSheetCustomFormatSettings; AResult: TdxNumberFormatResult);
begin
  if not dxIsNumericType(AValueType) then
    inherited Format(AValue, AValueType, AFormatSettings, AResult)
  else
    if (AValue < 0) and (AFormatSettings.DateTimeSystem = dts1904) then
    begin
      AResult.Append('-');
      Format(Abs(AValue), AValueType, AFormatSettings, AResult);
    end
    else
      if (AValue < 0) or (AValue >= MaxDateTime) then
      begin
        AResult.IsError := True;
        AResult.Append('#');
      end
      else
      begin
        if HasMilliseconds then
          FormatDateTime(AValue, AValueType, AFormatSettings, AResult)
        else
          FormatDateTime(AValue + EncodeTime(0, 0, 0, 499), AValueType, AFormatSettings, AResult);

        AResult.IsText := bFalse;
      end;
end;

function TdxDateTimeNumberFormatBase.HasMilliseconds: Boolean;
begin
  Result := False;
end;

procedure TdxDateTimeNumberFormatBase.FormatDateTime(
  const AValue: Variant; AValueType: TdxSpreadSheetCellDataType;
  const AFormatSettings: TdxSpreadSheetCustomFormatSettings; AResult: TdxNumberFormatResult);
var
  I: Integer;
begin
  for I := 0 to FList.Count - 1 do
    FList[I].Format(AValue, AValueType, AFormatSettings, AResult);
end;

function TdxDateTimeNumberFormatBase.GetValueType: TdxNumberFormatType;
begin
  Result := nftDateTime;
end;

{ TdxDateTimeSystemDateTimeNumberFormat }

destructor TdxDateTimeSystemDateTimeNumberFormat.Destroy;
begin
  FreeAndNil(FFormatter);
  inherited Destroy;
end;

procedure TdxDateTimeSystemDateTimeNumberFormat.AfterConstruction;
var
  AFormatSettings: TFormatSettings;
begin
  inherited AfterConstruction;
  dxGetLocaleFormatSettings(GetThreadLocale, AFormatSettings);
  FormatTemplate := GetFormatString(AFormatSettings);
end;

procedure TdxDateTimeSystemDateTimeNumberFormat.FormatDateTime(
  const AValue: Variant; AValueType: TdxSpreadSheetCellDataType;
  const AFormatSettings: TdxSpreadSheetCustomFormatSettings; AResult: TdxNumberFormatResult);
begin
  FormatTemplate := GetFormatString(AFormatSettings.Data);
  Formatter.Format(AValue, AValueType, AFormatSettings, AResult);
end;

function TdxDateTimeSystemDateTimeNumberFormat.IsTimeFormat: Boolean;
begin
  Result := Formatter.IsTimeFormat;
end;

procedure TdxDateTimeSystemDateTimeNumberFormat.SetFormatTemplate(const Value: string);
begin
  if Value <> FormatTemplate then
  begin
    FreeAndNil(FFormatter);
    FFormatTemplate := Value;
    FFormatter := TdxNumberFormatParser.Parse(TdxSpreadSheetDisplayFormatConverter.ConvertDateTimeDisplayFormat(FormatTemplate, 0), -1);
  end;
end;

{ TdxDateTimeSystemLongDateNumberFormat }

function TdxDateTimeSystemLongDateNumberFormat.GetFormatString(const AData: TFormatSettings): string;
begin
  Result := AData.LongDateFormat;
end;

{ TdxDateTimeSystemLongTimeNumberFormat }

function TdxDateTimeSystemLongTimeNumberFormat.GetFormatString(const AData: TFormatSettings): string;
begin
  Result := AData.LongTimeFormat;
end;

{ TdxDateTimeSystemShortDateNumberFormat }

function TdxDateTimeSystemShortDateNumberFormat.GetFormatString(const AData: TFormatSettings): string;
begin
  Result := AData.ShortDateFormat;
end;

{ TdxDateTimeNumberFormat }

constructor TdxDateTimeNumberFormat.Create(AElements: TdxNumberFormatElementList;
  ALocale: TdxNumberFormatElementDisplayLocale; AHasMilliseconds: Boolean);
begin
  inherited Create(AElements);
  FHasMilliseconds := AHasMilliseconds;
  FLocale := ALocale;
end;

procedure TdxDateTimeNumberFormat.Format(const AValue: Variant; AValueType: TdxSpreadSheetCellDataType;
  const AFormatSettings: TdxSpreadSheetCustomFormatSettings; AResult: TdxNumberFormatResult);
var
  ALocalizedFormatSettings: TdxSpreadSheetCustomFormatSettings;
begin
  if (FLocale <> nil) and (FLocale.LocaleId > 0) then
  begin
    ALocalizedFormatSettings := TdxSpreadSheetCustomFormatSettings.Create;
    try
      ALocalizedFormatSettings.DateTimeSystem := AFormatSettings.DateTimeSystem;
      dxGetLocaleFormatSettings(FLocale.LocaleId and $FFFF, ALocalizedFormatSettings.Data);
      inherited Format(AValue, AValueType, ALocalizedFormatSettings, AResult);
    finally
      ALocalizedFormatSettings.Free;
    end;
  end
  else
    inherited Format(AValue, AValueType, AFormatSettings, AResult);
end;

function TdxDateTimeNumberFormat.HasMilliseconds: Boolean;
begin
  Result := FHasMilliseconds;
end;

{ TdxGeneralNumberFormat }

constructor TdxGeneralNumberFormat.Create;
begin
  Create(nil);
end;

class procedure TdxGeneralNumberFormat.Finalize;
begin
  FreeAndNil(FDefault);
end;

procedure TdxGeneralNumberFormat.Format(
  const AValue: Variant; AValueType: TdxSpreadSheetCellDataType;
  const AFormatSettings: TdxSpreadSheetCustomFormatSettings; AResult: TdxNumberFormatResult);
var
  AIsText: TdxDefaultBoolean;
  I: Integer;
begin
  if List.Count > 0 then
  begin
    AIsText := AResult.IsText;
    for I := 0 to List.Count - 1 do
      List[I].Format(AValue, AValueType, AFormatSettings, AResult);
    AResult.IsText := AIsText;
  end
  else
    inherited Format(AValue, AValueType, AFormatSettings, AResult);
end;

function TdxGeneralNumberFormat.IsTimeFormat: Boolean;
begin
  Result := False;
end;

class function TdxGeneralNumberFormat.GetDefault: TdxGeneralNumberFormat;
begin
  if FDefault = nil then
    FDefault := TdxGeneralNumberFormat.Create;
  Result := FDefault;
end;

function TdxGeneralNumberFormat.GetValueType: TdxNumberFormatType;
begin
  Result := nftGeneral;
end;

{ TdxTextNumberFormat }

procedure TdxTextNumberFormat.Format(const AValue: Variant; AValueType: TdxSpreadSheetCellDataType;
  const AFormatSettings: TdxSpreadSheetCustomFormatSettings; AResult: TdxNumberFormatResult);
var
  I: Integer;
begin
  if AValueType in [cdtCurrency, cdtFloat, cdtDateTime, cdtInteger, cdtError] then
  begin
    TdxGeneralNumberFormat.Default.Format(AValue, AValueType, AFormatSettings, AResult);
    Exit;
  end;

  for I := 0 to FList.Count - 1 do
    FList[I].Format(AValue, AValueType, AFormatSettings, AResult);
end;

function TdxTextNumberFormat.GetValueType: TdxNumberFormatType;
begin
  Result := nftText;
end;

{ TdxConditionalNumberFormat }

constructor TdxConditionalNumberFormat.Create(AFormats: TdxSimpleNumberFormatList);
begin
  inherited Create;
  FParts := TdxSimpleNumberFormatList.Create;
  FParts.ExtractFrom(AFormats);
end;

destructor TdxConditionalNumberFormat.Destroy;
begin
  FreeAndNil(FParts);
  inherited Destroy;
end;

procedure TdxConditionalNumberFormat.Format(
  const AValue: Variant; AValueType: TdxSpreadSheetCellDataType;
  const AFormatSettings: TdxSpreadSheetCustomFormatSettings; AResult: TdxNumberFormatResult);
var
  APart: TdxNumberFormat;
  APartValue: Variant;
begin
  APart := CalculateActualPart(AValue, AValueType);
  if APart <> nil then
  begin
    APartValue := AValue;
    if (Parts.Count >= 2) and (APart = Parts[1]) then
      APartValue := -APartValue;
    APart.Format(APartValue, AValueType, AFormatSettings, AResult);
  end
  else
    inherited Format(AValue, AValueType, AFormatSettings, AResult);
end;

function TdxConditionalNumberFormat.IsTimeFormat: Boolean;
begin
  Result := Parts.First.IsTimeFormat;
end;

function TdxConditionalNumberFormat.GetValueType: TdxNumberFormatType;
begin
  Result := Parts.First.ValueType
end;

function TdxConditionalNumberFormat.CalculateActualPart(const AValue: Variant; AValueType: TdxSpreadSheetCellDataType): TdxNumberFormat;
begin
  Result := nil;

  if Parts.Count = 1 then
  begin
    if dxIsNumericType(AValueType) or (Parts[0].ValueType = nftText) then
      Result := Parts[0];
  end
  else

  if Parts.Count = 2 then
  begin
    if dxIsNumericType(AValueType) then
    begin
      if AValue < 0 then
        Result := Parts[1]
      else
        Result := Parts[0];
    end;
  end
  else

  if Parts.Count >= 3 then
  begin
    if not dxIsNumericType(AValueType) then
    begin
      if Parts.Count >= 4 then
        Result := Parts[3];
    end
    else
      if AValue < 0 then
        Result := Parts[1]
      else
        if AValue = 0 then
          Result := Parts[2]
        else
          Result := Parts[0];
  end;
end;

{ TdxSimpleNumericNumberFormat }

constructor TdxSimpleNumericNumberFormat.Create(AElements: TdxNumberFormatElementList; APercentCount, AIntegerCount,
  ADecimalCount, ADisplayFactor, ADecimalSeparatorIndex: Integer; AGrouping, AIsNegativePart: Boolean);
begin
  inherited Create(AElements);
  FDecimalCount := ADecimalCount;
  FDecimalSeparatorIndex := ADecimalSeparatorIndex;
  FDisplayFactor := Power(10, 3 * ADisplayFactor);
  FGrouping := AGrouping;
  FIntegerCount := AIntegerCount;
  FIsNegativePart := AIsNegativePart;
  FPercentCount := APercentCount;
end;

procedure TdxSimpleNumericNumberFormat.Format(
  const AValue: Variant; AValueType: TdxSpreadSheetCellDataType;
  const AFormatSettings: TdxSpreadSheetCustomFormatSettings; AResult: TdxNumberFormatResult);
var
  AExtendedValue: Extended;
  I: Integer;
begin
  if dxIsNumericType(AValueType) then
  begin
    if IsZero(AValue) then
    begin
      for I := 0 to FList.Count - 1 do
        FList[I].FormatEmpty(AFormatSettings, AResult);
    end
    else
    begin
      AExtendedValue := AValue;
      if FPercentCount > 0 then
        AExtendedValue := AExtendedValue * 100;
      if FDisplayFactor > 1 then
        AExtendedValue := AExtendedValue / FDisplayFactor;
      FormatSimple(TdxNumberFormatFloatRec.Create(AExtendedValue, FDecimalCount), FList.Count - 1, AFormatSettings, AResult);
    end;
    AResult.IsText := bFalse;
  end
  else
    inherited Format(AValue, AValueType, AFormatSettings, AResult);
end;

procedure TdxSimpleNumericNumberFormat.FormatSimple(
  const AValue: TdxNumberFormatFloatRec; AEndIndex: Integer;
  const AFormatSettings: TdxSpreadSheetCustomFormatSettings; AResult: TdxNumberFormatResult);
var
  AStartIndex: Integer;
begin
  AStartIndex := AResult.Length;
  if FDecimalSeparatorIndex < 0 then
    FormatIntegerPart(AResult, AValue, AEndIndex, 0, FIntegerCount, AFormatSettings)
  else
  begin
    FormatIntegerPart(AResult, AValue, FDecimalSeparatorIndex - 1, 0, FIntegerCount, AFormatSettings);
    AResult.Append(AFormatSettings.Data.DecimalSeparator);
    FormatDecimalPart(AResult, AValue, FDecimalSeparatorIndex + 1, AEndIndex, FDecimalCount, AFormatSettings);
  end;
  InsertSign(AResult, AValue.Data.Negative, AStartIndex);
end;

procedure TdxSimpleNumericNumberFormat.InsertSign(ABuffer: TStringBuilder; AIsNegative: Boolean; AIndex: Integer = 0);
begin
  if AIsNegative and not FIsNegativePart then
    ABuffer.Insert(AIndex, '-');
end;

procedure TdxSimpleNumericNumberFormat.FormatDecimalPart(ABuffer: TdxNumberFormatResult;
  const AValue: TdxNumberFormatFloatRec; AStartIndex, AEndIndex, ADigits: Integer;
  const AFormatSettings: TdxSpreadSheetCustomFormatSettings);
var
  AElement: TdxNumberFormatElement;
  AIndex: Integer;
  I: Integer;
begin
  I := AStartIndex;
  AIndex := AValue.FirstDecimal;
  while (AIndex < AValue.DigitCount) and (I < FList.Count) do
  begin
    AElement := FList[I];
    if AElement.IsDigit then
    begin
      AElement.Format(AValue.GetDigit(AIndex), cdtInteger, AFormatSettings, ABuffer);
      Inc(AIndex);
    end
    else
      AElement.FormatEmpty(AFormatSettings, ABuffer);

    Inc(I);
  end;

  while I <= AEndIndex do
  begin
    FList[I].FormatEmpty(AFormatSettings, ABuffer);
    Inc(I);
  end;
end;

procedure TdxSimpleNumericNumberFormat.FormatIntegerPart(ABuffer: TdxNumberFormatResult;
  const AValue: TdxNumberFormatFloatRec; AStartIndex, AEndIndex, ADigitCount: Integer;
  const AFormatSettings: TdxSpreadSheetCustomFormatSettings);
var
  ADigitsCountInQueue: Integer;
  AElement: TdxNumberFormatElement;
  AIndex: Integer;
  AInsertPosition: Integer;
  ANumIndex: Integer;
  I: Integer;
begin
  AInsertPosition := ABuffer.Length;

  I := AStartIndex;
  AIndex := AValue.Data.Exponent - 1;
  if AIndex >= 0 then
  begin
    ANumIndex := 0;
    ADigitsCountInQueue := ADigitCount;
    while (AIndex >= 0) and (ADigitsCountInQueue > 0) do
    begin
      AElement := FList[I];

      if AElement.IsDigit then
      begin
        if FGrouping and (ANumIndex mod 3 = 0) and (ANumIndex > 0) then
          ABuffer.Insert(AInsertPosition, AFormatSettings.Data.ThousandSeparator);
        ABuffer.Insert(AInsertPosition, AElement.Format(AValue.GetDigit(AIndex), cdtInteger, AFormatSettings, ABuffer.Color));
        Dec(ADigitsCountInQueue);
        Dec(AIndex);
        Inc(ANumIndex);
      end
      else
        ABuffer.Insert(AInsertPosition, AElement.FormatEmpty(AFormatSettings, ABuffer.Color));

      Dec(I);
    end;

    if (ADigitCount > 0) or (FDecimalSeparatorIndex >= 0) then
      while AIndex >= 0 do
      begin
        if FGrouping and (ANumIndex mod 3 = 0) and (ANumIndex > 0) then
          ABuffer.Insert(AInsertPosition, AFormatSettings.Data.ThousandSeparator);
        ABuffer.Insert(AInsertPosition, AValue.GetDigit(AIndex));
        Inc(ANumIndex);
        Dec(AIndex);
      end;
  end;

  while I >= AEndIndex do
  begin
    ABuffer.Insert(AInsertPosition, FList[I].FormatEmpty(AFormatSettings, ABuffer.Color));
    Dec(I);
  end;
end;

function TdxSimpleNumericNumberFormat.FormatIntegerPart(
  const AValue: TdxNumberFormatFloatRec; AStartIndex, AEndIndex, ADigitCount: Integer;
  const AFormatSettings: TdxSpreadSheetCustomFormatSettings): string;
var
  ABuffer: TdxNumberFormatResult;
begin
  ABuffer := TdxNumberFormatResult.Create;
  try
    FormatIntegerPart(ABuffer, AValue, AStartIndex, AEndIndex, ADigitCount, AFormatSettings);
    Result := ABuffer.ToString;
  finally
    ABuffer.Free;
  end;
end;

function TdxSimpleNumericNumberFormat.GetValueType: TdxNumberFormatType;
begin
  Result := nftNumeric;
end;

{ TdxNumericNumberFormatExponent }

constructor TdxNumericNumberFormatExponent.Create(AElements: TdxNumberFormatElementList; AIntegerCount, ADecimalCount,
  ADecimalSeparatorIndex, AExpIndex, AExpCount: Integer; AExplicitSign, AGrouping, AIsNegativePart: Boolean);
begin
  inherited Create(AElements, 0, AIntegerCount, ADecimalCount, 0, ADecimalSeparatorIndex, AGrouping, AIsNegativePart);
  FExpCount := AExpCount;
  FExpIndex := AExpIndex;
  FExplicitSign := AExplicitSign;
end;

procedure TdxNumericNumberFormatExponent.Format(
  const AValue: Variant; AValueType: TdxSpreadSheetCellDataType;
  const AFormatSettings: TdxSpreadSheetCustomFormatSettings; AResult: TdxNumberFormatResult);
var
  AExponent: Integer;
  AExtendedValue: Extended;
  AParsedValue: TdxNumberFormatFloatRec;
  ASignIndex: Integer;
begin
  if not dxIsNumericType(AValueType) then
  begin
    TdxGeneralNumberFormat.Default.Format(AValue, AValueType, AFormatSettings, AResult);
    Exit;
  end;

  AExtendedValue := AValue;
  ASignIndex := AResult.Length;

  AParsedValue := TdxNumberFormatFloatRec.Create(Abs(AExtendedValue), 9999);
  AExponent := CalculateExponent(AParsedValue);
  Dec(AParsedValue.Data.Exponent, AExponent);
  AParsedValue.FirstDecimal := AParsedValue.Data.Exponent;
  AParsedValue.RoundTo(FDecimalCount);
  AParsedValue.DigitCount := AParsedValue.FirstDecimal + FDecimalCount;
  FormatSimple(AParsedValue, FExpIndex - 1, AFormatSettings, AResult);

  AResult.Append('E');
  if AExponent < 0 then
    AResult.Append('-')
  else if FExplicitSign then
    AResult.Append('+');

  AResult.Append(FormatIntegerPart(
    TdxNumberFormatFloatRec.Create(Abs(AExponent), 0),
    FList.Count - 1, FExpIndex + 1, FIntegerCount, AFormatSettings));

  InsertSign(AResult, AExtendedValue < 0, ASignIndex);
  AResult.IsText := bFalse;
end;

function TdxNumericNumberFormatExponent.CalculateExponent(const AValue: TdxNumberFormatFloatRec): Integer;

  function GetIndexOfNotNullDigit: Integer;
  var
    I: Integer;
  begin
    Result := -1;
    for I := 0 to AValue.MaxDigits - 1 do
    begin
      if AValue.GetDigit(I) <> 0 then
        Exit(I);
    end;
  end;

begin
  if AValue.Data.Exponent > 0 then
    Result := AValue.Data.Exponent - FIntegerCount
  else
    Result := AValue.Data.Exponent - (GetIndexOfNotNullDigit + 1);
end;

{ TdxNumericFractionNumberFormat }

constructor TdxNumericFractionNumberFormat.Create(AElements: TdxNumberFormatElementList; APercentCount, AIntegerCount,
  APreFractionIndex, AFractionSeparatorIndex, ADivisorIndex, ADividendCount, ADivisorCount: Integer; AGrouping,
  AIsNegativePart: Boolean);
begin
  inherited Create(AElements, APercentCount, AIntegerCount, AFractionSeparatorIndex, 0, APreFractionIndex, FGrouping, AIsNegativePart);
  FDividendCount := ADividendCount;
  FDivisorCount := ADivisorCount;
  FDivisorIndex := ADivisorIndex;
end;

function TdxNumericFractionNumberFormat.CalculateRationalApproximation(AValue: Double): TPair<Double, Integer>;
var
  AAbsDelta: Double;
  AApproximation: Double;
  ABestDividend: Integer;
  ABestDivisor: Integer;
  ADividend: Integer;
  ADivisor: Integer;
  AIntegerPart: Double;
  ALeftDividend: Integer;
  ALeftDivisor: Integer;
  AMaxDivisor: Integer;
  AMinimalError: Double;
  ARightDividend: Integer;
  ARightDivisor: Integer;
begin
  AIntegerPart := Trunc(AValue);
  AValue := AValue - AIntegerPart;
  if (AValue = 0) or (AValue < 1E-7) then
    Exit(TPair<Double, Integer>.Create(AIntegerPart, 1));
  ALeftDividend := 0;
  ALeftDivisor := 1;
  ARightDividend := 1;
  ARightDivisor := 1;

  if Abs(AValue) < Abs(AValue - ARightDividend) then
  begin
    AMinimalError := Abs(AValue);
    ABestDividend := ALeftDividend;
    ABestDivisor := ALeftDivisor;
  end
  else
  begin
    AMinimalError := Abs(AValue - ARightDividend);
    ABestDividend := ARightDividend;
    ABestDivisor := ARightDivisor;
  end;
  AMaxDivisor := Trunc(Power(10, Min(FDivisorCount, 7)));

  while True do
  begin
    ADivisor := ALeftDivisor + ARightDivisor;
    if ADivisor >= AMaxDivisor then
      Break;

    ADividend := ALeftDividend + ARightDividend;

    if AValue * ADivisor < ADividend then
    begin
      ARightDividend := ADividend;
      ARightDivisor := ADivisor;
    end
    else
    begin
      ALeftDividend := ADividend;
      ALeftDivisor := ADivisor;
    end;

    AApproximation := ADividend / ADivisor;
    AAbsDelta := Abs(AValue - AApproximation);
    if AAbsDelta < AMinimalError then
    begin
      AMinimalError := AAbsDelta;
      ABestDividend := ADividend;
      ABestDivisor := ADivisor;
    end;
  end;
  Result := TPair<Double, Integer>.Create(ABestDividend + AIntegerPart * ABestDivisor, ABestDivisor);
end;

procedure TdxNumericFractionNumberFormat.Format(
  const AValue: Variant; AValueType: TdxSpreadSheetCellDataType;
  const AFormatSettings: TdxSpreadSheetCustomFormatSettings; AResult: TdxNumberFormatResult);
var
  ADivisor: Integer;
  AElement: TdxNumberFormatElement;
  AFracValues: TPair<Double, Integer>;
  AIndex: Integer;
  ANumericValue, AIntegerPart, ADividend: Double;
  ASignIndex: Integer;
  ATextIndex: Integer;
begin
  if not dxIsNumericType(AValueType) then
  begin
    TdxGeneralNumberFormat.Default.Format(AValue, AValueType, AFormatSettings, AResult);
    Exit;
  end;

  AIndex := 0;
  ASignIndex := AResult.Length;
  ANumericValue := Abs(AValue) * Power(100, FPercentCount);
  AIntegerPart := Trunc(ANumericValue);

  if FIntegerCount > 0 then
  begin
    AFracValues := CalculateRationalApproximation(ANumericValue - AIntegerPart);
    while True do
    begin
      AElement := FList[AIndex];
      if AElement.IsDigit then
        Break;
      AElement.FormatEmpty(AFormatSettings, AResult);
      Inc(AIndex);
    end;
    if AIntegerPart = 0 then
    begin
      ATextIndex := AResult.Length;
      FormatZeroIntegerPart(FDecimalSeparatorIndex, AIndex, AFormatSettings, AResult);
      if not IsZero(AFracValues.Key) then
        AResult.FillBySpaces(ATextIndex);
    end
    else
      AResult.Append(FormatIntegerPart(
        TdxNumberFormatFloatRec.Create(AIntegerPart, 0),
        FDecimalSeparatorIndex, AIndex, FIntegerCount, AFormatSettings));
  end
  else
    AFracValues := CalculateRationalApproximation(ANumericValue);

  ADividend := AFracValues.Key;
  ADivisor := AFracValues.Value;
  if IsZero(ADividend) then
  begin
    ATextIndex := AResult.Length;
    FormatZeroIntegerPart(FDecimalCount - 1, FDecimalSeparatorIndex + 1, AFormatSettings, AResult);
    AResult.Append('/');
    FormatDivisor(ADivisor, FDivisorIndex, AFormatSettings, AResult);
    if FIntegerCount > 0 then
      AResult.FillBySpaces(ATextIndex);
  end
  else
  begin
    AResult.Append(FormatIntegerPart(
      TdxNumberFormatFloatRec.Create(ADividend, 0), FDecimalCount - 1,
      FDecimalSeparatorIndex + 1, FDividendCount, AFormatSettings));
    AResult.Append('/');
    FormatDivisor(ADivisor, FDivisorIndex, AFormatSettings, AResult);
  end;

  AIndex := FDivisorIndex;
  while AIndex < FList.Count do
  begin
    FList[AIndex].FormatEmpty(AFormatSettings, AResult);
    Inc(AIndex);
  end;

  InsertSign(AResult, AValue < 0, ASignIndex);
  AResult.IsText := bFalse;
end;

procedure TdxNumericFractionNumberFormat.FormatDivisor(ADivisor, AEndIndex: Integer;
  const AFormatSettings: TdxSpreadSheetCustomFormatSettings; AResult: TdxNumberFormatResult);
var
  ADivisorTemp: Integer;
  AElement: TdxNumberFormatElement;
  AIndex: Integer;
  APower: Integer;
begin
  APower := Trunc(Power(10, Trunc(Log10(ADivisor)) + 1));
  AIndex := FDecimalCount;
  while (APower > 1) and (AIndex < AEndIndex) do
  begin
    AElement := FList[AIndex];
    if AElement.IsDigit then
    begin
      APower := APower div 10;
      ADivisorTemp := ADivisor mod APower;
      AElement.Format((ADivisor - ADivisorTemp) / APower, cdtFloat, AFormatSettings, AResult);
      ADivisor := ADivisorTemp;
    end
    else
      AElement.FormatEmpty(AFormatSettings, AResult);

    Inc(AIndex);
  end;

  while AIndex < AEndIndex do
  begin
    FList[AIndex].FormatEmpty(AFormatSettings, AResult);
    Inc(AIndex);
  end;
end;

procedure TdxNumericFractionNumberFormat.FormatZeroIntegerPart(AStartIndex, AEndIndex: Integer;
  const AFormatSettings: TdxSpreadSheetCustomFormatSettings; AResult: TdxNumberFormatResult);
var
  AElement: TdxNumberFormatElement;
  AIndex: Integer;
  AInsertIndex: Integer;
begin
  AInsertIndex := AResult.Length;
  AIndex := AStartIndex;
  while AIndex >= AEndIndex do
  begin
    AElement := FList[AIndex];
    if AElement.IsDigit then
    begin
      AResult.Insert(AInsertIndex, '0');
      Dec(AIndex);
      Break;
    end;
    AResult.Insert(AInsertIndex, AElement.FormatEmpty(AFormatSettings, AResult.Color));
    Dec(AIndex);
  end;
  while AIndex >= AEndIndex do
  begin
    AResult.Insert(AInsertIndex, FList[AIndex].FormatEmpty(AFormatSettings, AResult.Color));
    Dec(AIndex);
  end;
end;

{ TdxExplicitNumericFractionNumberFormat }

function TdxExplicitNumericFractionNumberFormat.CalculateRationalApproximation(AValue: Double): TPair<Double, Integer>;
var
  ADividend, AIntegerPart: Double;
begin
  AIntegerPart := Trunc(AValue);
  ADividend := Round((AValue - AIntegerPart) * ExplicitDivisor);
  Result := TPair<Double, Integer>.Create(ADividend + AIntegerPart * ExplicitDivisor, ExplicitDivisor);
end;

procedure TdxExplicitNumericFractionNumberFormat.FormatDivisor(ADivisor, AEndIndex: Integer;
  const AFormatSettings: TdxSpreadSheetCustomFormatSettings; AResult: TdxNumberFormatResult);
var
  I: Integer;
begin
  for I := FDecimalCount to AEndIndex - 1 do
    FList[I].FormatEmpty(AFormatSettings, AResult);
  AResult.Append(ExplicitDivisor);
end;

initialization

finalization
  TdxGeneralNumberFormat.Finalize;
end.
