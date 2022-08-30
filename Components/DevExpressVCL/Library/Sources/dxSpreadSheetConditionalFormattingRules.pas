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

unit dxSpreadSheetConditionalFormattingRules;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, Windows, Classes, Graphics, Math, Variants, Generics.Defaults, Generics.Collections,
  dxCore, cxClasses, cxVariants, cxGraphics, cxStorage,
  dxSpreadSheetClasses,
  dxSpreadSheetConditionalFormatting,
  dxSpreadSheetConditionalFormattingIconSet,
  dxSpreadSheetCoreFormulas,
  dxSpreadSheetCoreFormulasParser,
  dxSpreadSheetCoreStyles,
  dxSpreadSheetStyles,
  dxSpreadSheetTypes;

type
  TdxSpreadSheetConditionalFormattingExpression = class;
  TdxSpreadSheetConditionalFormattingRuleCustomColorScale = class;
  TdxSpreadSheetConditionalFormattingRuleCustomScale = class;

  { TdxSpreadSheetConditionalFormattingExpression }

  TdxSpreadSheetConditionalFormattingExpression = class(TdxSpreadSheetCustomFormula)
  strict private
    FOwner: TdxSpreadSheetCustomConditionalFormatting;
    function GetConditionalFormattingOwner: IdxSpreadSheetConditionalFormattingOwner;
  protected
    function GetController: TdxSpreadSheetCustomFormulaController; override;
    function GetResultValue: TdxSpreadSheetFormulaResult; override;
    function GetValue: Variant; override;
    function GetView: TObject; override;
  public
    constructor Create(const AOwner: TdxSpreadSheetCustomConditionalFormatting);
  end;

  { TdxSpreadSheetConditionalFormattingStyle }

  TdxSpreadSheetConditionalFormattingStyle = class(TdxSpreadSheetCellStyle)
  protected
    procedure DoChanged; override;
  end;

  { TdxSpreadSheetConditionalFormattingRuleStyleBased }

  TdxSpreadSheetConditionalFormattingRuleStyleBasedClass = class of TdxSpreadSheetConditionalFormattingRuleStyleBased;
  TdxSpreadSheetConditionalFormattingRuleStyleBased = class(TdxSpreadSheetConditionalFormattingCustomRule,
    IdxSpreadSheetCellStyleOwner)
  strict private
    FStyle: TdxSpreadSheetCellStyle;

    procedure SetStyle(AValue: TdxSpreadSheetCellStyle);
  protected
    // IdxSpreadSheetCellStyleOwner
    procedure CellStyleChanged;
    procedure CellStyleChanging;
    function GetFormatSettings: TdxSpreadSheetCustomFormatSettings;
    function GetCellStyles: TdxSpreadSheetCellStyles;
    procedure ProcessStyleChanges(APrevStyle, ANewStyle: TdxSpreadSheetCellStyleHandle);

    function GetProperties(AProperties: TStrings): Boolean; override;
    procedure GetPropertyValue(const AName: string; var AValue: Variant); override;
    procedure SetPropertyValue(const AName: string; const AValue: Variant); override;

    procedure Apply(const AStyle: TdxSpreadSheetCellDisplayStyle; ARow, AColumn: Integer; const ACell: IdxSpreadSheetCellData = nil); override;
    procedure CalculateStyle(AStyle: TdxSpreadSheetCellDisplayStyle);
    procedure DoAssign(Source: TdxSpreadSheetCustomConditionalFormattingRule); override;
    procedure DoLoadFromStream(AReader: TcxReader); override;
    procedure DoSaveToStream(AWriter: TcxWriter); override;
    procedure DrawPreview(ACanvas: TcxCanvas; R: TRect); override;
  public
    constructor Create(AOwner: TdxSpreadSheetCustomConditionalFormatting); override;
    destructor Destroy; override;
  published
    property StopIfTrue;
    property Style: TdxSpreadSheetCellStyle read FStyle write SetStyle;
  end;

  { TdxSpreadSheetConditionalFormattingRuleAboveOrBelowAverage }

  TdxSpreadSheetConditionalFormattingRuleAboveOrBelowAverageComparisonOperator = (
    abacoAboveAverage, abacoAboveOrEqualAverage, abacoBelowAverage, abacoBelowOrEqualAverage,
    abacoAboveAverageOnStandardDeviation, abacoBelowAverageOnStandardDeviation);

  TdxSpreadSheetConditionalFormattingRuleAboveOrBelowAverage = class(TdxSpreadSheetConditionalFormattingRuleStyleBased)
  public const
    MaxStandardDeviationLevel = 3;
    MinStandardDeviationLevel = 1;
  strict private
    FComparisonOperator: TdxSpreadSheetConditionalFormattingRuleAboveOrBelowAverageComparisonOperator;
    FStandardDeviationLevel: Integer;

    procedure SetComparisonOperator(AValue: TdxSpreadSheetConditionalFormattingRuleAboveOrBelowAverageComparisonOperator);
    procedure SetStandardDeviationLevel(AValue: Integer);
  protected
    function GetProperties(AProperties: TStrings): Boolean; override;
    procedure GetPropertyValue(const AName: string; var AValue: Variant); override;
    procedure SetPropertyValue(const AName: string; const AValue: Variant); override;

    function CanApplyCore(ARow, AColumn: Integer; const ACell: IdxSpreadSheetCellData = nil): Boolean; override;

    procedure DoAssign(Source: TdxSpreadSheetCustomConditionalFormattingRule); override;
    procedure DoLoadFromStream(AReader: TcxReader); override;
    procedure DoSaveToStream(AWriter: TcxWriter); override;
    function GetDetails: string; override;
  public
    constructor Create(AOwner: TdxSpreadSheetCustomConditionalFormatting); override;
  published
    property ComparisonOperator: TdxSpreadSheetConditionalFormattingRuleAboveOrBelowAverageComparisonOperator read FComparisonOperator write SetComparisonOperator default abacoAboveAverage;
    property StandardDeviationLevel: Integer read FStandardDeviationLevel write SetStandardDeviationLevel default MinStandardDeviationLevel;
  end;

  { TdxSpreadSheetConditionalFormattingRuleExpression }

  TdxSpreadSheetConditionalFormattingRuleExpression = class(TdxSpreadSheetConditionalFormattingRuleStyleBased)
  strict private
    FFormulas: array[0..1] of TdxSpreadSheetConditionalFormattingExpression;

    function GetFormula(AIndex: Integer): TdxSpreadSheetConditionalFormattingExpression;
    function GetExpression(AIndex: Integer): string;
    procedure SetExpression(AIndex: Integer; const AValue: string);
  protected
    function GetFormulaValue(AIndex: Integer; ARow, AColumn: Integer): Variant;
    function GetProperties(AProperties: TStrings): Boolean; override;
    procedure GetPropertyValue(const AName: string; var AValue: Variant); override;
    procedure SetPropertyValue(const AName: string; const AValue: Variant); override;

    function CanApplyCore(ARow, AColumn: Integer; const ACell: IdxSpreadSheetCellData = nil): Boolean; override;

    function ApplyToTheRowSupported: Boolean; override;
    procedure Changed; override;
    procedure DoAssign(Source: TdxSpreadSheetCustomConditionalFormattingRule); override;
    procedure DoLoadFromStream(AReader: TcxReader); override;
    procedure DoSaveToStream(AWriter: TcxWriter); override;
    procedure DoParseReference(const AReference: string; var AFormula: TdxSpreadSheetConditionalFormattingExpression); virtual;
    function GetDetails: string; override;

    procedure RestoreFormulaAnchors;
    procedure UpdateFormulaAnchor(AFormula: TdxSpreadSheetConditionalFormattingExpression; ARow, AColumn: Integer);

    property Formulas[AIndex: Integer]: TdxSpreadSheetConditionalFormattingExpression read GetFormula;
    property Expression2: string index 1 read GetExpression write SetExpression;
  public
    destructor Destroy; override;
  published
    property Expression: string index 0 read GetExpression write SetExpression;
  end;

  { TdxSpreadSheetConditionalFormattingRuleCellIs }

  TdxSpreadSheetConditionalFormattingRuleCellIsComparisonOperator = (cicoBetween, cicoEqual,
    cicoGreaterThan, cicoGreaterThanOrEqual, cicoLessThan, cicoLessThanOrEqual, cicoNotBetween, cicoNotEqual);

  TdxSpreadSheetConditionalFormattingRuleCellIs = class(TdxSpreadSheetConditionalFormattingRuleExpression)
  strict private
    FComparisonOperator: TdxSpreadSheetConditionalFormattingRuleCellIsComparisonOperator;

    procedure SetComparisonOperator(AValue: TdxSpreadSheetConditionalFormattingRuleCellIsComparisonOperator);
  protected
    function GetProperties(AProperties: TStrings): Boolean; override;
    procedure GetPropertyValue(const AName: string; var AValue: Variant); override;
    procedure SetPropertyValue(const AName: string; const AValue: Variant); override;

    function CanApplyCore(ARow, AColumn: Integer; const ACell: IdxSpreadSheetCellData = nil): Boolean; override;

    function ApplyToTheRowSupported: Boolean; override;
    procedure DoAssign(Source: TdxSpreadSheetCustomConditionalFormattingRule); override;
    procedure DoLoadFromStream(AReader: TcxReader); override;
    procedure DoSaveToStream(AWriter: TcxWriter); override;
    procedure DoParseReference(const AReference: string; var AFormula: TdxSpreadSheetConditionalFormattingExpression); override;
    function GetDetails: string; override;
  published
    property ComparisonOperator: TdxSpreadSheetConditionalFormattingRuleCellIsComparisonOperator read FComparisonOperator write SetComparisonOperator;
    property Expression2;
  end;

  { TdxSpreadSheetConditionalFormattingRuleDuplicateValues }

  TdxSpreadSheetConditionalFormattingRuleDuplicateValues = class(TdxSpreadSheetConditionalFormattingRuleStyleBased)
  protected
    function CanApplyCore(ARow, AColumn: Integer; const ACell: IdxSpreadSheetCellData = nil): Boolean; override;
    function GetDetails: string; override;
  end;

  { TdxSpreadSheetConditionalFormattingRuleTopBottomValues }

  TdxSpreadSheetConditionalFormattingRuleTopBottomValuesDirection = (tbvdTop, tbvdBottom);
  TdxSpreadSheetConditionalFormattingRuleTopBottomValuesValueType = (tbvvtRank, tbvvtPercent);

  TdxSpreadSheetConditionalFormattingRuleTopBottomValues = class(TdxSpreadSheetConditionalFormattingRuleStyleBased)
  strict private
    FDirection: TdxSpreadSheetConditionalFormattingRuleTopBottomValuesDirection;
    FValue: Integer;
    FValueType: TdxSpreadSheetConditionalFormattingRuleTopBottomValuesValueType;

    function ValidateValue(AValue: Integer): Integer;
    procedure SetDirection(AValue: TdxSpreadSheetConditionalFormattingRuleTopBottomValuesDirection);
    procedure SetValue(AValue: Integer);
    procedure SetValueType(AValue: TdxSpreadSheetConditionalFormattingRuleTopBottomValuesValueType);
  protected
    function GetProperties(AProperties: TStrings): Boolean; override;
    procedure GetPropertyValue(const AName: string; var AValue: Variant); override;
    procedure SetPropertyValue(const AName: string; const AValue: Variant); override;

    function CanApplyCore(ARow, AColumn: Integer; const ACell: IdxSpreadSheetCellData = nil): Boolean; override;

    procedure DoAssign(Source: TdxSpreadSheetCustomConditionalFormattingRule); override;
    procedure DoLoadFromStream(AReader: TcxReader); override;
    procedure DoSaveToStream(AWriter: TcxWriter); override;
    function GetDetails: string; override;
  public
    constructor Create(AOwner: TdxSpreadSheetCustomConditionalFormatting); override;
  published
    property Direction: TdxSpreadSheetConditionalFormattingRuleTopBottomValuesDirection read FDirection write SetDirection default tbvdTop;
    property ValueType: TdxSpreadSheetConditionalFormattingRuleTopBottomValuesValueType read FValueType write SetValueType default tbvvtRank;
    property Value: Integer read FValue write SetValue default 10;
  end;

  { TdxSpreadSheetConditionalFormattingRuleUniqueValues }

  TdxSpreadSheetConditionalFormattingRuleUniqueValues = class(TdxSpreadSheetConditionalFormattingRuleStyleBased)
  protected
    function CanApplyCore(ARow, AColumn: Integer; const ACell: IdxSpreadSheetCellData = nil): Boolean; override;
    function GetDetails: string; override;
  end;

  { TdxSpreadSheetConditionalFormattingRuleCustomScaleStop }

  TdxSpreadSheetConditionalFormattingRuleCustomScaleStopValueType = (
    cssvtLimitValue, cssvtValue, cssvtPercent, cssvtFormula, cssvtPercentile);

  TdxSpreadSheetConditionalFormattingRuleCustomScaleStopClass = class of TdxSpreadSheetConditionalFormattingRuleCustomScaleStop;
  TdxSpreadSheetConditionalFormattingRuleCustomScaleStop = class(TInterfacedPersistent,
    IcxStoredObject)
  strict private
    FValue: Variant;
    FValueAsExpression: TdxSpreadSheetConditionalFormattingExpression;
    FValueType: TdxSpreadSheetConditionalFormattingRuleCustomScaleStopValueType;

    function GetActualValue: Double;
    function GetIndex: Integer;
    function GetValue: Variant;
    procedure SetValue(AValue: Variant);
    procedure SetValueType(const AValue: TdxSpreadSheetConditionalFormattingRuleCustomScaleStopValueType);
  protected
    FActualValue: Double;
    FOwner: TdxSpreadSheetConditionalFormattingRuleCustomScale;

    procedure DoAssign(Source: TdxSpreadSheetConditionalFormattingRuleCustomScaleStop); virtual;
    procedure Changed;
    procedure LoadFromStream(AReader: TcxReader); virtual;
    procedure SaveToStream(AWriter: TcxWriter); virtual;
    function ParseExpression(const AValue: string): TdxSpreadSheetConditionalFormattingExpression;
    procedure ResetValue; virtual;

    function GetDefaultValueType: TdxSpreadSheetConditionalFormattingRuleCustomScaleStopValueType; virtual;
    procedure ValidateValue(var AValue: Variant); virtual;
    procedure ValidateValueType(var AValueType: TdxSpreadSheetConditionalFormattingRuleCustomScaleStopValueType); virtual;

  {$REGION 'IcxStoredObject'}
    function GetObjectName: string;
    function GetProperties(AProperties: TStrings): Boolean; virtual;
    procedure GetPropertyValue(const AName: string; var AValue: Variant); virtual;
    procedure SetPropertyValue(const AName: string; const AValue: Variant); virtual;
  {$ENDREGION}

    property ValueAsExpression: TdxSpreadSheetConditionalFormattingExpression read FValueAsExpression;
  public
    constructor Create(AOwner: TdxSpreadSheetConditionalFormattingRuleCustomScale); virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure BeginUpdate;
    procedure EndUpdate;
    //
    property ActualValue: Double read GetActualValue;
    property Index: Integer read GetIndex;
  published
    property ValueType: TdxSpreadSheetConditionalFormattingRuleCustomScaleStopValueType read FValueType write SetValueType;
    property Value: Variant read GetValue write SetValue;
  end;

  { TdxSpreadSheetConditionalFormattingRuleCustomScale }

  TdxSpreadSheetConditionalFormattingRuleCustomScale = class(TdxSpreadSheetConditionalFormattingCustomRule,
    IcxStoredParent)
  strict private
    FCacheIsDirty: Boolean;
    FStopClass: TdxSpreadSheetConditionalFormattingRuleCustomScaleStopClass;
  protected
    FStops: TObjectList<TdxSpreadSheetConditionalFormattingRuleCustomScaleStop>;

    procedure CalculateActualValue(AStop: TdxSpreadSheetConditionalFormattingRuleCustomScaleStop; const ALimitValue: Double);
    procedure CalculateActualValues; virtual; abstract;
    procedure Changed; override;
    procedure CheckActualValues;
    procedure DoAssign(Source: TdxSpreadSheetCustomConditionalFormattingRule); override;
    procedure DoLoadFromStream(AReader: TcxReader); override;
    procedure DoSaveToStream(AWriter: TcxWriter); override;
    procedure FlushCache; override;

    function AddStop: TdxSpreadSheetConditionalFormattingRuleCustomScaleStop;
    function GetStopClass: TdxSpreadSheetConditionalFormattingRuleCustomScaleStopClass; virtual;
    function GetStopCount: Integer;
    function GetStopName(AStop: TdxSpreadSheetConditionalFormattingRuleCustomScaleStop): string; virtual;

  {$REGION 'IcxStoredParent'}
    function CreateChild(const AObjectName, AClassName: string): TObject;
    procedure DeleteChild(const AObjectName: string; AObject: TObject);
    procedure GetChildren(AChildren: TStringList);
  {$ENDREGION}
    //
    property StopCount: Integer read GetStopCount;
  public
    constructor Create(AOwner: TdxSpreadSheetCustomConditionalFormatting); override;
    destructor Destroy; override;
  end;

  { TdxSpreadSheetConditionalFormattingRuleColorScaleStop }

  TdxSpreadSheetConditionalFormattingRuleColorScaleStop = class(TdxSpreadSheetConditionalFormattingRuleCustomScaleStop)
  strict private
    FColor: TColor;

    procedure SetColor(const AValue: TColor);
  protected
    function GetProperties(AProperties: TStrings): Boolean; override;
    procedure GetPropertyValue(const AName: string; var AValue: Variant); override;
    procedure SetPropertyValue(const AName: string; const AValue: Variant); override;

    procedure DoAssign(Source: TdxSpreadSheetConditionalFormattingRuleCustomScaleStop); override;
    procedure LoadFromStream(AReader: TcxReader); override;
    procedure SaveToStream(AWriter: TcxWriter); override;
  public
    procedure AfterConstruction; override;
  published
    property Color: TColor read FColor write SetColor default clYellow;
  end;

  { TdxSpreadSheetConditionalFormattingRuleCustomColorScale }

  TdxSpreadSheetConditionalFormattingRuleCustomColorScale = class(TdxSpreadSheetConditionalFormattingRuleCustomScale)
  protected
    procedure Apply(const AStyle: TdxSpreadSheetCellDisplayStyle; ARow, AColumn: Integer; const ACell: IdxSpreadSheetCellData = nil); override;
    function CanApplyCore(ARow, AColumn: Integer; const ACell: IdxSpreadSheetCellData = nil): Boolean; override;

    function CalculateColor(const AValue: Double): TColor;
    procedure DrawPreview(ACanvas: TcxCanvas; R: TRect); override;
    function GetDetails: string; override;
    function GetStop(AIndex: Integer): TdxSpreadSheetConditionalFormattingRuleColorScaleStop;
    function GetStopClass: TdxSpreadSheetConditionalFormattingRuleCustomScaleStopClass; override;
    procedure SetStop(AIndex: Integer; AValue: TdxSpreadSheetConditionalFormattingRuleColorScaleStop);
    //
    property Stops[Index: Integer]: TdxSpreadSheetConditionalFormattingRuleColorScaleStop read GetStop write SetStop;
  end;

  { TdxSpreadSheetConditionalFormattingRuleThreeColorScale }

  TdxSpreadSheetConditionalFormattingRuleThreeColorScale = class(TdxSpreadSheetConditionalFormattingRuleCustomColorScale)
  public const
    DefaultMinValueColor: TColor = clYellow;
    DefaultMiddleValueColor: TColor = clYellow;
    DefaultMaxValueColor: TColor = clYellow;
  protected
    procedure CalculateActualValues; override;
    function GetStopName(AStop: TdxSpreadSheetConditionalFormattingRuleCustomScaleStop): string; override;
  public
    constructor Create(AOwner: TdxSpreadSheetCustomConditionalFormatting); override;
  published
    property MinValue: TdxSpreadSheetConditionalFormattingRuleColorScaleStop index 0 read GetStop write SetStop;
    property MiddleValue: TdxSpreadSheetConditionalFormattingRuleColorScaleStop index 1 read GetStop write SetStop;
    property MaxValue: TdxSpreadSheetConditionalFormattingRuleColorScaleStop index 2 read GetStop write SetStop;
  end;

  { TdxSpreadSheetConditionalFormattingRuleTwoColorScale }

  TdxSpreadSheetConditionalFormattingRuleTwoColorScale = class(TdxSpreadSheetConditionalFormattingRuleCustomColorScale)
  public const
    DefaultMinValueColor: TColor = clYellow;
    DefaultMaxValueColor: TColor = clYellow;
  protected
    procedure CalculateActualValues; override;
    function GetStopName(AStop: TdxSpreadSheetConditionalFormattingRuleCustomScaleStop): string; override;
  public
    constructor Create(AOwner: TdxSpreadSheetCustomConditionalFormatting); override;
  published
    property MinValue: TdxSpreadSheetConditionalFormattingRuleColorScaleStop index 0 read GetStop write SetStop;
    property MaxValue: TdxSpreadSheetConditionalFormattingRuleColorScaleStop index 1 read GetStop write SetStop;
  end;

  { TdxSpreadSheetConditionalFormattingRuleDataBarStyle }

  TdxSpreadSheetConditionalFormattingRuleDataBarAxisPosition = (dbapAuto, dbapMidpoint, dbapNone);
  TdxSpreadSheetConditionalFormattingRuleDataBarDirection = (dbdAuto, dbdLeftToRight, dbdRightToLeft);
  TdxSpreadSheetConditionalFormattingRuleDataBarFillMode = (dbfmSolid, dbfmGradient);

  TdxSpreadSheetConditionalFormattingRuleDataBarStyle = class(TPersistent)
  strict private
    FAxisColor: TColor;
    FAxisPosition: TdxSpreadSheetConditionalFormattingRuleDataBarAxisPosition;
    FDirection: TdxSpreadSheetConditionalFormattingRuleDataBarDirection;
    FFillMode: TdxSpreadSheetConditionalFormattingRuleDataBarFillMode;
    FNegativeBarBorderColor: TColor;
    FNegativeBarColor: TColor;
    FPositiveBarBorderColor: TColor;
    FPositiveBarColor: TColor;

    FOnChanged: TNotifyEvent;
    FOnChanging: TNotifyEvent;

    function GetActualNegativeBarBorderColor: TColor;
    function GetActualNegativeBarColor: TColor;
    procedure SetAxisColor(AValue: TColor);
    procedure SetAxisPosition(AValue: TdxSpreadSheetConditionalFormattingRuleDataBarAxisPosition);
    procedure SetDirection(AValue: TdxSpreadSheetConditionalFormattingRuleDataBarDirection);
    procedure SetFillMode(AValue: TdxSpreadSheetConditionalFormattingRuleDataBarFillMode);
    procedure SetNegativeBarBorderColor(AValue: TColor);
    procedure SetNegativeBarColor(AValue: TColor);
    procedure SetPositiveBarBorderColor(AValue: TColor);
    procedure SetPositiveBarColor(AValue: TColor);
  protected
    procedure Changed;
    procedure Changing;
    procedure LoadFromStream(AReader: TcxReader);
    procedure SaveToStream(AWriter: TcxWriter);
    //
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
  public
    constructor Create; virtual;
    procedure Assign(Source: TPersistent); override;
    //
    property ActualNegativeBarBorderColor: TColor read GetActualNegativeBarBorderColor;
    property ActualNegativeBarColor: TColor read GetActualNegativeBarColor;
  published
    property AxisColor: TColor read FAxisColor write SetAxisColor default clBlack;
    property AxisPosition: TdxSpreadSheetConditionalFormattingRuleDataBarAxisPosition read FAxisPosition write SetAxisPosition default dbapAuto;
    property Direction: TdxSpreadSheetConditionalFormattingRuleDataBarDirection read FDirection write SetDirection default dbdAuto;
    property FillMode: TdxSpreadSheetConditionalFormattingRuleDataBarFillMode read FFillMode write SetFillMode default dbfmGradient;
    property NegativeBarBorderColor: TColor read FNegativeBarBorderColor write SetNegativeBarBorderColor default clDefault;
    property NegativeBarColor: TColor read FNegativeBarColor write SetNegativeBarColor default clDefault;
    property PositiveBarBorderColor: TColor read FPositiveBarBorderColor write SetPositiveBarBorderColor default clNone;
    property PositiveBarColor: TColor read FPositiveBarColor write SetPositiveBarColor default $EF8B00;
  end;

  { TdxSpreadSheetConditionalFormattingRuleDataBar }

  TdxSpreadSheetConditionalFormattingRuleDataBar = class(TdxSpreadSheetConditionalFormattingRuleCustomScale)
  strict private
    FShowValue: Boolean;
    FStyle: TdxSpreadSheetConditionalFormattingRuleDataBarStyle;

    procedure ApplyDirection(const AStyle: TdxSpreadSheetCellDisplayStyle; const ACell: IdxSpreadSheetCellData = nil);
    procedure SetShowValue(AValue: Boolean);
    procedure SetStyle(AValue: TdxSpreadSheetConditionalFormattingRuleDataBarStyle);
    procedure StyleChangedHandler(Sender: TObject);
    procedure StyleChangingHandler(Sender: TObject);
  protected
    function GetProperties(AProperties: TStrings): Boolean; override;
    procedure GetPropertyValue(const AName: string; var AValue: Variant); override;
    procedure SetPropertyValue(const AName: string; const AValue: Variant); override;

    procedure Apply(const AStyle: TdxSpreadSheetCellDisplayStyle; ARow, AColumn: Integer; const ACell: IdxSpreadSheetCellData = nil); override;
    function CanApplyCore(ARow, AColumn: Integer; const ACell: IdxSpreadSheetCellData = nil): Boolean; override;

    procedure CalculateActualValues; override;
    procedure CalculateStyle(const AStyle: TdxSpreadSheetCellDisplayStyle; const AValue: Double);
    procedure DoAssign(Source: TdxSpreadSheetCustomConditionalFormattingRule); override;
    procedure DoLoadFromStream(AReader: TcxReader); override;
    procedure DoSaveToStream(AWriter: TcxWriter); override;
    procedure DrawPreview(ACanvas: TcxCanvas; R: TRect); override;
    function GetDetails: string; override;
    function GetStop(AIndex: Integer): TdxSpreadSheetConditionalFormattingRuleCustomScaleStop;
    function GetStopName(AStop: TdxSpreadSheetConditionalFormattingRuleCustomScaleStop): string; override;
    function IsRightToLeftDirection: Boolean; virtual;
    procedure SetStop(AIndex: Integer; AValue: TdxSpreadSheetConditionalFormattingRuleCustomScaleStop);
  public
    constructor Create(AOwner: TdxSpreadSheetCustomConditionalFormatting); override;
    destructor Destroy; override;
  published
    property MaxValue: TdxSpreadSheetConditionalFormattingRuleCustomScaleStop index 1 read GetStop write SetStop;
    property MinValue: TdxSpreadSheetConditionalFormattingRuleCustomScaleStop index 0 read GetStop write SetStop;
    property ShowValue: Boolean read FShowValue write SetShowValue default True;
    property Style: TdxSpreadSheetConditionalFormattingRuleDataBarStyle read FStyle write SetStyle;
  end;

  { TdxSpreadSheetConditionalFormattingRuleIconSetStop }

  TdxSpreadSheetConditionalFormattingRuleIconSetStopComparisonOperator = (isscoGreaterThanOrEqual, isscoGreaterThan);

  TdxSpreadSheetConditionalFormattingRuleIconSetStop = class(TdxSpreadSheetConditionalFormattingRuleCustomScaleStop)
  strict private
    FComparisonOperator: TdxSpreadSheetConditionalFormattingRuleIconSetStopComparisonOperator;
    FIconIndex: Integer;

    procedure SetComparisonOperator(AValue: TdxSpreadSheetConditionalFormattingRuleIconSetStopComparisonOperator);
    procedure SetIconIndex(AValue: Integer);
  protected
    function GetProperties(AProperties: TStrings): Boolean; override;
    procedure GetPropertyValue(const AName: string; var AValue: Variant); override;
    procedure SetPropertyValue(const AName: string; const AValue: Variant); override;

    function Compare(const AValue: Double): Boolean; inline;
    procedure DoAssign(Source: TdxSpreadSheetConditionalFormattingRuleCustomScaleStop); override;
    function GetDefaultValueType: TdxSpreadSheetConditionalFormattingRuleCustomScaleStopValueType; override;
    procedure LoadFromStream(AReader: TcxReader); override;
    procedure SaveToStream(AWriter: TcxWriter); override;
    procedure ResetValue; override;
    procedure ValidateValueType(var AValueType: TdxSpreadSheetConditionalFormattingRuleCustomScaleStopValueType); override;
  public
    constructor Create(AOwner: TdxSpreadSheetConditionalFormattingRuleCustomScale); override;
  published
    property ComparisonOperator: TdxSpreadSheetConditionalFormattingRuleIconSetStopComparisonOperator
      read FComparisonOperator write SetComparisonOperator default isscoGreaterThanOrEqual;
    property IconIndex: Integer read FIconIndex write SetIconIndex default 0;
  end;

  { TdxSpreadSheetConditionalFormattingRuleIconSet }

  TdxSpreadSheetConditionalFormattingRuleIconSetOrder = (isioNormal, isioReversed);

  TdxSpreadSheetConditionalFormattingRuleIconSet = class(TdxSpreadSheetConditionalFormattingRuleCustomScale)
  strict private
    FOrder: TdxSpreadSheetConditionalFormattingRuleIconSetOrder;
    FPreset: TdxSpreadSheetConditionalFormattingIconSetPreset;
    FShowValue: Boolean;

    procedure LoadStopsFromPreset(APreset: TdxSpreadSheetConditionalFormattingIconSetPreset);
    function GetIcons: TcxImageList;
    function GetPresetName: string;
    function GetStop(AIndex: Integer): TdxSpreadSheetConditionalFormattingRuleIconSetStop;
    procedure SetOrder(AValue: TdxSpreadSheetConditionalFormattingRuleIconSetOrder);
    procedure SetPresetName(const AValue: string);
    procedure SetShowValue(AValue: Boolean);
    procedure SetStop(AIndex: Integer; AValue: TdxSpreadSheetConditionalFormattingRuleIconSetStop);
    procedure SetStopCount(AValue: Integer);
  protected
    function GetProperties(AProperties: TStrings): Boolean; override;
    procedure GetPropertyValue(const AName: string; var AValue: Variant); override;
    procedure SetPropertyValue(const AName: string; const AValue: Variant); override;

    procedure Apply(const AStyle: TdxSpreadSheetCellDisplayStyle; ARow, AColumn: Integer; const ACell: IdxSpreadSheetCellData = nil); override;
    function CanApplyCore(ARow, AColumn: Integer; const ACell: IdxSpreadSheetCellData = nil): Boolean; override;

    procedure CalculateActualValues; override;
    procedure DoAssign(Source: TdxSpreadSheetCustomConditionalFormattingRule); override;
    procedure DoLoadFromStream(AReader: TcxReader); override;
    procedure DoSaveToStream(AWriter: TcxWriter); override;
    procedure DrawPreview(ACanvas: TcxCanvas; R: TRect); override;
    function GetDetails: string; override;

    procedure IconsChanged;
    function GetStopClass: TdxSpreadSheetConditionalFormattingRuleCustomScaleStopClass; override;

    property Icons: TcxImageList read GetIcons;
  public
    constructor Create(AOwner: TdxSpreadSheetCustomConditionalFormatting); override;
    //
    property StopCount: Integer read GetStopCount write SetStopCount;
    property Stops[Index: Integer]: TdxSpreadSheetConditionalFormattingRuleIconSetStop read GetStop write SetStop;
  published
    property Order: TdxSpreadSheetConditionalFormattingRuleIconSetOrder read FOrder write SetOrder default isioNormal;
    property PresetName: string read GetPresetName write SetPresetName;
    property ShowValue: Boolean read FShowValue write SetShowValue default True;
  end;

implementation

uses
  SysUtils,
  dxGDIPlusAPI, dxGDIPlusClasses, cxGeometry, dxDPIAwareUtils, dxCoreGraphics, cxDrawTextUtils,
  dxSpreadSheetCoreStrs,
  dxSpreadSheetUtils,
  dxSpreadSheetGraphics;

{ TdxSpreadSheetConditionalFormattingExpression }

constructor TdxSpreadSheetConditionalFormattingExpression.Create(const AOwner: TdxSpreadSheetCustomConditionalFormatting);
begin
  inherited Create;
  FOwner := AOwner;
end;

function TdxSpreadSheetConditionalFormattingExpression.GetConditionalFormattingOwner: IdxSpreadSheetConditionalFormattingOwner;
begin
  Result := FOwner.Owner;
end;

function TdxSpreadSheetConditionalFormattingExpression.GetController: TdxSpreadSheetCustomFormulaController;
begin
  Result := GetConditionalFormattingOwner.FormulaController;
end;

function TdxSpreadSheetConditionalFormattingExpression.GetResultValue: TdxSpreadSheetFormulaResult;
begin
  FreeAndNil(FResultValue);
  FResultValue := Calculate(FTokens);
  Result := FResultValue;
end;

function TdxSpreadSheetConditionalFormattingExpression.GetView: TObject;
begin
  Result := GetConditionalFormattingOwner as TObject;
end;

function TdxSpreadSheetConditionalFormattingExpression.GetValue: Variant;
var
  AResult: TdxSpreadSheetFormulaResult;
begin
  AResult := ResultValue;
  if AResult <> nil then
    Result := AResult.Value
  else
    Result := 0;
end;

{ TdxSpreadSheetConditionalFormattingStyle }

procedure TdxSpreadSheetConditionalFormattingStyle.DoChanged;
begin
  TdxSpreadSheetConditionalFormattingRuleStyleBased(Owner).Changed;
end;

{ TdxSpreadSheetConditionalFormattingRuleStyleBased }

constructor TdxSpreadSheetConditionalFormattingRuleStyleBased.Create(
  AOwner: TdxSpreadSheetCustomConditionalFormatting);
begin
  inherited Create(AOwner);
  FStyle := TdxSpreadSheetConditionalFormattingStyle.Create(Self);
end;

destructor TdxSpreadSheetConditionalFormattingRuleStyleBased.Destroy;
begin
  FreeAndNil(FStyle);
  inherited Destroy;
end;

procedure TdxSpreadSheetConditionalFormattingRuleStyleBased.Apply(
  const AStyle: TdxSpreadSheetCellDisplayStyle; ARow, AColumn: Integer; const ACell: IdxSpreadSheetCellData = nil);
begin
  CalculateStyle(AStyle);
end;

procedure TdxSpreadSheetConditionalFormattingRuleStyleBased.CalculateStyle(
  AStyle: TdxSpreadSheetCellDisplayStyle);
var
  AHelper: TdxSpreadSheetCellStyleMergeHelper;
begin
  AHelper := TdxSpreadSheetCellStyleMergeHelper.Create(Owner.CellStyles.DefaultStyle, Style.Handle);
  try
    AHelper.ProcessCellStyle(AStyle);
  finally
    AHelper.Free;
  end;
end;

procedure TdxSpreadSheetConditionalFormattingRuleStyleBased.DoAssign(
  Source: TdxSpreadSheetCustomConditionalFormattingRule);
begin
  inherited DoAssign(Source);
  if Source is TdxSpreadSheetConditionalFormattingRuleStyleBased then
    Style := TdxSpreadSheetConditionalFormattingRuleStyleBased(Source).Style;
end;

procedure TdxSpreadSheetConditionalFormattingRuleStyleBased.DoLoadFromStream(AReader: TcxReader);
begin
  inherited DoLoadFromStream(AReader);
  Style.Handle := Owner.CellStyles.CreateStyleFromStream(AReader);
end;

procedure TdxSpreadSheetConditionalFormattingRuleStyleBased.DoSaveToStream(AWriter: TcxWriter);
begin
  inherited DoSaveToStream(AWriter);
  Style.Handle.SaveToStream(AWriter);
end;

procedure TdxSpreadSheetConditionalFormattingRuleStyleBased.DrawPreview(ACanvas: TcxCanvas; R: TRect);
var
  ABackgroundColor: TColor;
  ABordersStyles: TdxSpreadSheetCellBordersStyles;
  AFont: TFont;
  ASide: TcxBorder;
begin
  ABackgroundColor := cxGetActualColor(Style.Brush.BackgroundColor, clWindow);
  dxSpreadSheetDrawBackground(ACanvas, R, ABackgroundColor, Style.Brush.ForegroundColor, Style.Brush.Style);

  R := cxRectInflate(R, 0, 0, -1, -1);
  for ASide := Low(TcxBorder) to High(TcxBorder) do
    ABordersStyles[ASide] := Style.Borders[ASide].Style;
  for ASide := Low(TcxBorder) to High(TcxBorder) do
  begin
    if ABordersStyles[ASide] <> sscbsDefault then
      dxSpreadSheetDrawBorder(ACanvas, dxSpreadSheetGetBorderBounds(R, ASide, ABordersStyles),
        Style.Borders[ASide].Color, ABackgroundColor, ABordersStyles[ASide], ASide in [bTop, bBottom]);
  end;

  AFont := TFont.Create;
  try
    Style.Font.AssignToFont(AFont);
    AFont.Height := dxGetScaleFactorForCanvas(ACanvas.Canvas).Apply(AFont.Height);
    cxTextOut(ACanvas.Handle, cxGetResourceString(@sdxPreviewText), R,
      CXTO_CENTER_HORIZONTALLY or CXTO_CENTER_VERTICALLY or CXTO_SINGLELINE, AFont);
  finally
    AFont.Free;
  end;
end;

procedure TdxSpreadSheetConditionalFormattingRuleStyleBased.CellStyleChanged;
begin
// do nothing
end;

procedure TdxSpreadSheetConditionalFormattingRuleStyleBased.CellStyleChanging;
begin
  Changing;
end;

function TdxSpreadSheetConditionalFormattingRuleStyleBased.GetFormatSettings: TdxSpreadSheetCustomFormatSettings;
begin
  Result := Owner.Owner.FormulaController.FormatSettings;
end;

function TdxSpreadSheetConditionalFormattingRuleStyleBased.GetCellStyles: TdxSpreadSheetCellStyles;
begin
  Result := Owner.CellStyles;
end;

procedure TdxSpreadSheetConditionalFormattingRuleStyleBased.ProcessStyleChanges(APrevStyle, ANewStyle: TdxSpreadSheetCellStyleHandle);
begin
  // do nothing
end;

function TdxSpreadSheetConditionalFormattingRuleStyleBased.GetProperties(AProperties: TStrings): Boolean;
begin
  inherited GetProperties(AProperties);
  AProperties.Add('FontCharset');
  AProperties.Add('FontColor');
  AProperties.Add('FontSize');
  AProperties.Add('FontName');
  AProperties.Add('FontPitch');
  AProperties.Add('FontStyleBold');
  AProperties.Add('FontStyleItalic');
  AProperties.Add('FontStyleUnderline');
  AProperties.Add('FontStyleStrikeOut');
  AProperties.Add('BrushBackgroundColor');
  AProperties.Add('BrushForegroundColor');
  AProperties.Add('BrushStyle');
  AProperties.Add('BorderLeftColor');
  AProperties.Add('BorderLeftStyle');
  AProperties.Add('BorderTopColor');
  AProperties.Add('BorderTopStyle');
  AProperties.Add('BorderRightColor');
  AProperties.Add('BorderRightStyle');
  AProperties.Add('BorderBottomColor');
  AProperties.Add('BorderBottomStyle');
  AProperties.Add('FormatCode');
  AProperties.Add('FormatCodeID');
  Result := True;
end;

procedure TdxSpreadSheetConditionalFormattingRuleStyleBased.GetPropertyValue(const AName: string; var AValue: Variant);
begin
  if AName = 'FontCharset' then
    AValue := Style.Font.Charset
  else if AName = 'FontColor' then
    AValue := Style.Font.Color
  else if AName = 'FontSize' then
    AValue := Style.Font.Size
  else if AName = 'FontName' then
    AValue := Style.Font.Name
  else if AName = 'FontPitch' then
    AValue := Style.Font.Pitch
  else if AName = 'FontStyleBold' then
    AValue := fsBold in Style.Font.Style
  else if AName = 'FontStyleItalic' then
    AValue := fsItalic in Style.Font.Style
  else if AName = 'FontStyleUnderline' then
    AValue := fsUnderline in Style.Font.Style
  else if AName = 'FontStyleStrikeOut' then
    AValue := fsStrikeOut in Style.Font.Style
  else if AName = 'BrushBackgroundColor' then
    AValue := Style.Brush.BackgroundColor
  else if AName = 'BrushForegroundColor' then
    AValue := Style.Brush.ForegroundColor
  else if AName = 'BrushStyle' then
    AValue := Style.Brush.Style
  else if AName = 'BorderLeftColor' then
    AValue := Style.Borders[bLeft].Color
  else if AName = 'BorderLeftStyle' then
    AValue := Style.Borders[bLeft].Style
  else if AName = 'BorderTopColor' then
    AValue := Style.Borders[bTop].Color
  else if AName = 'BorderTopStyle' then
    AValue := Style.Borders[bTop].Style
  else if AName = 'BorderRightColor' then
    AValue := Style.Borders[bRight].Color
  else if AName = 'BorderRightStyle' then
    AValue := Style.Borders[bRight].Style
  else if AName = 'BorderBottomColor' then
    AValue := Style.Borders[bBottom].Color
  else if AName = 'BorderBottomStyle' then
    AValue := Style.Borders[bBottom].Style
  else if AName = 'FormatCode' then
    AValue := Style.DataFormat.FormatCode
  else if AName = 'FormatCodeID' then
    AValue := Style.DataFormat.FormatCodeID
  else
    inherited GetPropertyValue(AName, AValue);
end;

procedure TdxSpreadSheetConditionalFormattingRuleStyleBased.SetPropertyValue(const AName: string; const AValue: Variant);
begin
  if AName = 'FontCharset' then
    Style.Font.Charset := AValue
  else if AName = 'FontColor' then
    Style.Font.Color := AValue
  else if AName = 'FontSize' then
    Style.Font.Size := AValue
  else if AName = 'FontName' then
    Style.Font.Name := AValue
  else if AName = 'FontPitch' then
    Style.Font.Pitch := AValue
  else if AName = 'FontStyleBold' then
  begin
    if AValue then
      Style.Font.Style := Style.Font.Style + [fsBold]
    else
      Style.Font.Style := Style.Font.Style - [fsBold];
  end
  else if AName = 'FontStyleItalic' then
  begin
    if AValue then
      Style.Font.Style := Style.Font.Style + [fsItalic]
    else
      Style.Font.Style := Style.Font.Style - [fsItalic];
  end
  else if AName = 'FontStyleUnderline' then
  begin
    if AValue then
      Style.Font.Style := Style.Font.Style + [fsUnderline]
    else
      Style.Font.Style := Style.Font.Style - [fsUnderline];
  end
  else if AName = 'FontStyleStrikeOut' then
  begin
    if AValue then
      Style.Font.Style := Style.Font.Style + [fsStrikeOut]
    else
      Style.Font.Style := Style.Font.Style - [fsStrikeOut];
  end
  else if AName = 'BrushBackgroundColor' then
    Style.Brush.BackgroundColor := AValue
  else if AName = 'BrushForegroundColor' then
    Style.Brush.ForegroundColor := AValue
  else if AName = 'BrushStyle' then
    Style.Brush.Style := AValue
  else if AName = 'BorderLeftColor' then
    Style.Borders[bLeft].Color := AValue
  else if AName = 'BorderLeftStyle' then
    Style.Borders[bLeft].Style := AValue
  else if AName = 'BorderTopColor' then
    Style.Borders[bTop].Color := AValue
  else if AName = 'BorderTopStyle' then
    Style.Borders[bTop].Style := AValue
  else if AName = 'BorderRightColor' then
    Style.Borders[bRight].Color := AValue
  else if AName = 'BorderRightStyle' then
    Style.Borders[bRight].Style := AValue
  else if AName = 'BorderBottomColor' then
    Style.Borders[bBottom].Color := AValue
  else if AName = 'BorderBottomStyle' then
    Style.Borders[bBottom].Style := AValue
  else if AName = 'FormatCode' then
    Style.DataFormat.FormatCode := AValue
  else if AName = 'FormatCodeID' then
    Style.DataFormat.FormatCodeID := AValue
  else
    inherited SetPropertyValue(AName, AValue);
end;

procedure TdxSpreadSheetConditionalFormattingRuleStyleBased.SetStyle(AValue: TdxSpreadSheetCellStyle);
begin
  FStyle.Assign(AValue);
end;

{ TdxSpreadSheetConditionalFormattingRuleAboveOrBelowAverage }

constructor TdxSpreadSheetConditionalFormattingRuleAboveOrBelowAverage.Create(
  AOwner: TdxSpreadSheetCustomConditionalFormatting);
begin
  inherited Create(AOwner);
  FStandardDeviationLevel := MinStandardDeviationLevel;
end;

function TdxSpreadSheetConditionalFormattingRuleAboveOrBelowAverage.GetProperties(AProperties: TStrings): Boolean;
begin
  inherited GetProperties(AProperties);
  AProperties.Add('ComparisonOperator');
  AProperties.Add('StandardDeviationLevel');
  Result := True;
end;

procedure TdxSpreadSheetConditionalFormattingRuleAboveOrBelowAverage.GetPropertyValue(const AName: string; var AValue: Variant);
begin
  if AName = 'ComparisonOperator' then
    AValue := ComparisonOperator
  else if AName = 'StandardDeviationLevel' then
    AValue := StandardDeviationLevel
  else
    inherited GetPropertyValue(AName, AValue);
end;

procedure TdxSpreadSheetConditionalFormattingRuleAboveOrBelowAverage.SetPropertyValue(const AName: string; const AValue: Variant);
begin
  if AName = 'ComparisonOperator' then
    ComparisonOperator := AValue
  else if AName = 'StandardDeviationLevel' then
    StandardDeviationLevel := AValue
  else
    inherited SetPropertyValue(AName, AValue);
end;

procedure TdxSpreadSheetConditionalFormattingRuleAboveOrBelowAverage.DoAssign(
  Source: TdxSpreadSheetCustomConditionalFormattingRule);
begin
  inherited DoAssign(Source);
  if Source is TdxSpreadSheetConditionalFormattingRuleAboveOrBelowAverage then
  begin
    ComparisonOperator := TdxSpreadSheetConditionalFormattingRuleAboveOrBelowAverage(Source).ComparisonOperator;
    StandardDeviationLevel := TdxSpreadSheetConditionalFormattingRuleAboveOrBelowAverage(Source).StandardDeviationLevel;
  end;
end;

function TdxSpreadSheetConditionalFormattingRuleAboveOrBelowAverage.CanApplyCore(
  ARow, AColumn: Integer; const ACell: IdxSpreadSheetCellData = nil): Boolean;
begin
  if (ACell = nil) or not ACell.IsNumericValue then
    Exit(False);

  case ComparisonOperator of
    abacoAboveAverage:
      Result := ACell.AsFloat > AreaInfo.AverageValue;
    abacoAboveOrEqualAverage:
      Result := ACell.AsFloat >= AreaInfo.AverageValue;
    abacoBelowAverage:
      Result := ACell.AsFloat < AreaInfo.AverageValue;
    abacoBelowOrEqualAverage:
      Result := ACell.AsFloat <= AreaInfo.AverageValue;
    abacoAboveAverageOnStandardDeviation:
      Result := ACell.AsFloat > AreaInfo.AverageValue + StandardDeviationLevel * AreaInfo.StandardDeviation;
    abacoBelowAverageOnStandardDeviation:
      Result := ACell.AsFloat < AreaInfo.AverageValue - StandardDeviationLevel * AreaInfo.StandardDeviation;
  else
    Result := False;
  end;
end;

procedure TdxSpreadSheetConditionalFormattingRuleAboveOrBelowAverage.DoLoadFromStream(AReader: TcxReader);
begin
  inherited DoLoadFromStream(AReader);
  ComparisonOperator := TdxSpreadSheetConditionalFormattingRuleAboveOrBelowAverageComparisonOperator(AReader.ReadByte);
  StandardDeviationLevel := AReader.ReadInteger;
end;

procedure TdxSpreadSheetConditionalFormattingRuleAboveOrBelowAverage.DoSaveToStream(AWriter: TcxWriter);
begin
  inherited DoSaveToStream(AWriter);
  AWriter.WriteByte(Ord(ComparisonOperator));
  AWriter.WriteInteger(StandardDeviationLevel);
end;

function TdxSpreadSheetConditionalFormattingRuleAboveOrBelowAverage.GetDetails: string;
const
  Map: array[TdxSpreadSheetConditionalFormattingRuleAboveOrBelowAverageComparisonOperator] of Pointer = (
    @sdxConditionalFormattingAboveAverage,
    @sdxConditionalFormattingAboveOrEqualAverage,
    @sdxConditionalFormattingBelowAverage,
    @sdxConditionalFormattingBelowOrEqualAverage,
    @sdxConditionalFormattingAboveAverageOnStandardDeviation,
    @sdxConditionalFormattingBelowAverageOnStandardDeviation
  );
begin
  Result := cxGetResourceString(Map[ComparisonOperator]);
  case ComparisonOperator of
    abacoAboveAverageOnStandardDeviation, abacoBelowAverageOnStandardDeviation:
      Result := Format(Result, [StandardDeviationLevel]);
  end;
end;

procedure TdxSpreadSheetConditionalFormattingRuleAboveOrBelowAverage.SetComparisonOperator(
  AValue: TdxSpreadSheetConditionalFormattingRuleAboveOrBelowAverageComparisonOperator);
begin
  if FComparisonOperator <> AValue then
  begin
    BeginUpdate;
    try
      FComparisonOperator := AValue;
      Changed;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TdxSpreadSheetConditionalFormattingRuleAboveOrBelowAverage.SetStandardDeviationLevel(AValue: Integer);
begin
  AValue := Min(Max(AValue, MinStandardDeviationLevel), MaxStandardDeviationLevel);
  if AValue <> FStandardDeviationLevel then
  begin
    BeginUpdate;
    try
      FStandardDeviationLevel := AValue;
      Changed;
    finally
      EndUpdate;
    end;
  end;
end;

{ TdxSpreadSheetConditionalFormattingRuleExpression }

destructor TdxSpreadSheetConditionalFormattingRuleExpression.Destroy;
var
  I: Integer;
begin
  for I := Low(FFormulas) to High(FFormulas) do
    FreeAndNil(FFormulas[I]);
  inherited Destroy;
end;

function TdxSpreadSheetConditionalFormattingRuleExpression.GetFormulaValue(
  AIndex: Integer; ARow, AColumn: Integer): Variant;
var
  AFormula: TdxSpreadSheetConditionalFormattingExpression;
begin
  AFormula := Formulas[AIndex];
  if AFormula <> nil then
  begin
    if AFormula <> nil then
    begin
      UpdateFormulaAnchor(AFormula, ARow, AColumn);
      try
        Result := AFormula.Value;
      finally
        RestoreFormulaAnchors;
      end;
    end;
  end
  else
    Result := Null;
end;

function TdxSpreadSheetConditionalFormattingRuleExpression.GetProperties(AProperties: TStrings): Boolean;
begin
  inherited GetProperties(AProperties);
  AProperties.Add('Expression');
  AProperties.Add('Expression2');
  Result := True;
end;

procedure TdxSpreadSheetConditionalFormattingRuleExpression.GetPropertyValue(const AName: string; var AValue: Variant);
begin
  if AName = 'Expression' then
    AValue := Expression
  else if AName = 'Expression2' then
    AValue := Expression2
  else
    inherited GetPropertyValue(AName, AValue);
end;

procedure TdxSpreadSheetConditionalFormattingRuleExpression.SetPropertyValue(const AName: string; const AValue: Variant);
begin
  if AName = 'Expression' then
    Expression := AValue
  else if AName = 'Expression2' then
    Expression2 := AValue
  else
    inherited SetPropertyValue(AName, AValue);
end;

function TdxSpreadSheetConditionalFormattingRuleExpression.ApplyToTheRowSupported: Boolean;
begin
  Result := True;
end;

procedure TdxSpreadSheetConditionalFormattingRuleExpression.Changed;
begin
  RestoreFormulaAnchors;
  inherited Changed;
end;

procedure TdxSpreadSheetConditionalFormattingRuleExpression.DoAssign(
  Source: TdxSpreadSheetCustomConditionalFormattingRule);
begin
  inherited DoAssign(Source);
  if Source is TdxSpreadSheetConditionalFormattingRuleExpression then
  begin
    Expression := TdxSpreadSheetConditionalFormattingRuleExpression(Source).Expression;
    Expression2 := TdxSpreadSheetConditionalFormattingRuleExpression(Source).Expression2;
  end;
end;

function TdxSpreadSheetConditionalFormattingRuleExpression.CanApplyCore(
  ARow, AColumn: Integer; const ACell: IdxSpreadSheetCellData = nil): Boolean;
var
  V: Variant;
begin
  V := GetFormulaValue(0, ARow, AColumn);
  Result := VarIsNumeric(V) and (V <> 0);
end;

procedure TdxSpreadSheetConditionalFormattingRuleExpression.DoLoadFromStream(AReader: TcxReader);
begin
  inherited DoLoadFromStream(AReader);
  Expression := AReader.ReadWideString;
  Expression2 := AReader.ReadWideString;
end;

procedure TdxSpreadSheetConditionalFormattingRuleExpression.DoSaveToStream(AWriter: TcxWriter);
begin
  inherited DoSaveToStream(AWriter);
  AWriter.WriteWideString(Expression);
  AWriter.WriteWideString(Expression2);
end;

procedure TdxSpreadSheetConditionalFormattingRuleExpression.DoParseReference(
  const AReference: string; var AFormula: TdxSpreadSheetConditionalFormattingExpression);
begin
  dxSpreadSheetParseReference(AReference, AFormula);
end;

function TdxSpreadSheetConditionalFormattingRuleExpression.GetDetails: string;
begin
  Result := Format(cxGetResourceString(@sdxConditionalFormattingExpressionRuleDetails),
    [dxSpreadSheetFormulaExcludeEqualSymbol(Expression)]);
end;

procedure TdxSpreadSheetConditionalFormattingRuleExpression.RestoreFormulaAnchors;
begin
  UpdateFormulaAnchor(Formulas[0], Area.Top, Area.Left);
  UpdateFormulaAnchor(Formulas[1], Area.Top, Area.Left);
end;

procedure TdxSpreadSheetConditionalFormattingRuleExpression.UpdateFormulaAnchor(
  AFormula: TdxSpreadSheetConditionalFormattingExpression; ARow, AColumn: Integer);
begin
  if AFormula <> nil then
  begin
    AFormula.FAnchorRow := ARow;
    AFormula.FAnchorColumn := AColumn;
    AFormula.CalculateAnchors;
  end;
end;

function TdxSpreadSheetConditionalFormattingRuleExpression.GetFormula(
  AIndex: Integer): TdxSpreadSheetConditionalFormattingExpression;
begin
  Result := FFormulas[AIndex];
end;

function TdxSpreadSheetConditionalFormattingRuleExpression.GetExpression(AIndex: Integer): string;
begin
  if Formulas[AIndex] <> nil then
    Result := Formulas[AIndex].AsText
  else
    Result := '';
end;

procedure TdxSpreadSheetConditionalFormattingRuleExpression.SetExpression(AIndex: Integer; const AValue: string);
begin
  if WideCompareStr(AValue, GetExpression(AIndex)) <> 0 then
  begin
    BeginUpdate;
    try
      FreeAndNil(FFormulas[AIndex]);
      if AValue <> '' then
      begin
        FFormulas[AIndex] := TdxSpreadSheetConditionalFormattingExpression.Create(Owner);
        RestoreFormulaAnchors;
        DoParseReference(AValue, FFormulas[AIndex]);
      end;
      Changed;
    finally
      EndUpdate;
    end;
  end;
end;

{ TdxSpreadSheetConditionalFormattingRuleCellIs }

function TdxSpreadSheetConditionalFormattingRuleCellIs.GetProperties(AProperties: TStrings): Boolean;
begin
  inherited GetProperties(AProperties);
  AProperties.Add('ComparisonOperator');
  Result := True;
end;

procedure TdxSpreadSheetConditionalFormattingRuleCellIs.GetPropertyValue(const AName: string; var AValue: Variant);
begin
  if AName = 'ComparisonOperator' then
    AValue := ComparisonOperator
  else
    inherited GetPropertyValue(AName, AValue);
end;

procedure TdxSpreadSheetConditionalFormattingRuleCellIs.SetPropertyValue(const AName: string; const AValue: Variant);
begin
  if AName = 'ComparisonOperator' then
    ComparisonOperator := AValue
  else
    inherited SetPropertyValue(AName, AValue);
end;

function TdxSpreadSheetConditionalFormattingRuleCellIs.ApplyToTheRowSupported: Boolean;
begin
  Result := False;
end;

procedure TdxSpreadSheetConditionalFormattingRuleCellIs.DoAssign(Source: TdxSpreadSheetCustomConditionalFormattingRule);
begin
  inherited DoAssign(Source);
  if Source is TdxSpreadSheetConditionalFormattingRuleCellIs then
    ComparisonOperator := TdxSpreadSheetConditionalFormattingRuleCellIs(Source).ComparisonOperator;
end;

function TdxSpreadSheetConditionalFormattingRuleCellIs.CanApplyCore(
  ARow, AColumn: Integer; const ACell: IdxSpreadSheetCellData = nil): Boolean;

  function InnerCompare(const V1, V2: Variant): Integer;
  begin
    if VarIsStr(V1) and VarIsStr(V2) then
      Result := dxSpreadSheetCompareText(V1, V2)
    else
      Result := VarCompare(V1, V2);
  end;

var
  AValue, AFormulaValue1, AFormulaValue2: Variant;
begin
  if ACell = nil then
    Exit(False);

  AValue := ACell.AsVariant;
  AFormulaValue1 := GetFormulaValue(0, ARow, AColumn);
  AFormulaValue2 := GetFormulaValue(1, ARow, AColumn);

  case ComparisonOperator of
    cicoGreaterThan:
      Result := InnerCompare(AValue, AFormulaValue1) > 0;
    cicoLessThan:
      Result := InnerCompare(AValue, AFormulaValue1) < 0;
    cicoGreaterThanOrEqual:
      Result := InnerCompare(AValue, AFormulaValue1) >= 0;
    cicoLessThanOrEqual:
      Result := InnerCompare(AValue, AFormulaValue1) <= 0;
    cicoEqual:
      Result := InnerCompare(AValue, AFormulaValue1) = 0;
    cicoNotEqual:
      Result := InnerCompare(AValue, AFormulaValue1) <> 0;
  else // cicoBetween cicoNotBetween
    Result := (ComparisonOperator = cicoNotBetween) xor
      (((InnerCompare(AValue, AFormulaValue1) >= 0) and (InnerCompare(AValue, AFormulaValue2) <= 0)) or
      ((InnerCompare(AValue, AFormulaValue2) >= 0) and (InnerCompare(AValue, AFormulaValue1) <= 0)));
  end;
end;

procedure TdxSpreadSheetConditionalFormattingRuleCellIs.DoLoadFromStream(AReader: TcxReader);
begin
  inherited DoLoadFromStream(AReader);
  ComparisonOperator := TdxSpreadSheetConditionalFormattingRuleCellIsComparisonOperator(AReader.ReadInteger);
end;

procedure TdxSpreadSheetConditionalFormattingRuleCellIs.DoSaveToStream(AWriter: TcxWriter);
begin
  inherited DoSaveToStream(AWriter);
  AWriter.WriteInteger(Ord(ComparisonOperator));
end;

procedure TdxSpreadSheetConditionalFormattingRuleCellIs.DoParseReference(
  const AReference: string; var AFormula: TdxSpreadSheetConditionalFormattingExpression);
var
  AResultValue: TdxSpreadSheetFormulaResult;
begin
  if (ComparisonOperator = cicoEqual) and not dxSpreadSheetIsFormula(AReference) then
  begin
    try
      dxSpreadSheetParseReference(AReference, AFormula);
      AResultValue := AFormula.ResultValue;
    except
      AResultValue := nil;
    end;
    if (AResultValue = nil) or not AResultValue.Validate or (AFormula.ErrorIndex <> 0) then
    begin
      AFormula.SetError(ecNone, 0);
      dxSpreadSheetParseReference(dxSpreadSheetTextValueAsString(AReference), AFormula);
    end;
  end
  else
    inherited;
end;

function TdxSpreadSheetConditionalFormattingRuleCellIs.GetDetails: string;
const
  ComparisonOperatorNameMap: array[TdxSpreadSheetConditionalFormattingRuleCellIsComparisonOperator] of Pointer = (
    @sdxConditionalFormattingCellIsRuleDetailsBetween,
    @sdxConditionalFormattingCellIsRuleDetailsEqual,
    @sdxConditionalFormattingCellIsRuleDetailsGreaterThan,
    @sdxConditionalFormattingCellIsRuleDetailsGreaterThanOrEqual,
    @sdxConditionalFormattingCellIsRuleDetailsLessThan,
    @sdxConditionalFormattingCellIsRuleDetailsLessThanOrEqual,
    @sdxConditionalFormattingCellIsRuleDetailsNotBetween,
    @sdxConditionalFormattingCellIsRuleDetailsNotEqual
  );
var
  AFormatLine: string;
begin
  AFormatLine := cxGetResourceString(ComparisonOperatorNameMap[ComparisonOperator]);
  if ComparisonOperator in [cicoBetween, cicoNotBetween] then
    Result := Format(AFormatLine, [dxSpreadSheetFormulaExcludeEqualSymbol(Expression), dxSpreadSheetFormulaExcludeEqualSymbol(Expression2)])
  else
    Result := Format(AFormatLine, [dxSpreadSheetFormulaExcludeEqualSymbol(Expression)]);
end;

procedure TdxSpreadSheetConditionalFormattingRuleCellIs.SetComparisonOperator(
  AValue: TdxSpreadSheetConditionalFormattingRuleCellIsComparisonOperator);
begin
  if FComparisonOperator <> AValue then
  begin
    BeginUpdate;
    try
      FComparisonOperator := AValue;
      Changed;
    finally
      EndUpdate;
    end;
  end;
end;

{ TdxSpreadSheetConditionalFormattingRuleDuplicateValues }

function TdxSpreadSheetConditionalFormattingRuleDuplicateValues.CanApplyCore(
  ARow, AColumn: Integer; const ACell: IdxSpreadSheetCellData = nil): Boolean;
begin
  if (ACell = nil) or ACell.IsEmpty then
    Result := False
  else
    Result := not AreaInfo.IsUniqueValue(ACell.AsVariant);
end;

function TdxSpreadSheetConditionalFormattingRuleDuplicateValues.GetDetails: string;
begin
  Result := cxGetResourceString(@sdxConditionalFormattingDuplicateValuesRuleDetails);
end;

{ TdxSpreadSheetConditionalFormattingRuleTopBottomValues }

constructor TdxSpreadSheetConditionalFormattingRuleTopBottomValues.Create(AOwner: TdxSpreadSheetCustomConditionalFormatting);
begin
  inherited Create(AOwner);
  FValue := 10;
end;

function TdxSpreadSheetConditionalFormattingRuleTopBottomValues.GetProperties(AProperties: TStrings): Boolean;
begin
  inherited GetProperties(AProperties);
  AProperties.Add('Direction');
  AProperties.Add('ValueType');
  AProperties.Add('Value');
  Result := True;
end;

procedure TdxSpreadSheetConditionalFormattingRuleTopBottomValues.GetPropertyValue(const AName: string; var AValue: Variant);
begin
  if AName = 'Direction' then
    AValue := Direction
  else if AName = 'ValueType' then
    AValue := ValueType
  else if AName = 'Value' then
    AValue := Value
  else
    inherited GetPropertyValue(AName, AValue);
end;

procedure TdxSpreadSheetConditionalFormattingRuleTopBottomValues.SetPropertyValue(const AName: string; const AValue: Variant);
begin
  if AName = 'Direction' then
    Direction := AValue
  else if AName = 'ValueType' then
    ValueType := AValue
  else if AName = 'Value' then
    Value := AValue
  else
    inherited SetPropertyValue(AName, AValue);
end;

procedure TdxSpreadSheetConditionalFormattingRuleTopBottomValues.DoAssign(
  Source: TdxSpreadSheetCustomConditionalFormattingRule);
begin
  inherited DoAssign(Source);
  if Source is TdxSpreadSheetConditionalFormattingRuleTopBottomValues then
  begin
    Direction := TdxSpreadSheetConditionalFormattingRuleTopBottomValues(Source).Direction;
    Value := TdxSpreadSheetConditionalFormattingRuleTopBottomValues(Source).Value;
    ValueType := TdxSpreadSheetConditionalFormattingRuleTopBottomValues(Source).ValueType;
  end;
end;

function TdxSpreadSheetConditionalFormattingRuleTopBottomValues.CanApplyCore(
  ARow, AColumn: Integer; const ACell: IdxSpreadSheetCellData = nil): Boolean;
var
  ARank: Integer;
begin
  if (ACell = nil) or ACell.IsEmpty then
    Exit(False);

  if ValueType = tbvvtRank then
    ARank := Value
  else
    ARank := Max(1, Trunc(AreaInfo.NumericValueCount * Value / 100));

  if Direction = tbvdTop then
    Result := AreaInfo.IsTopValue(ACell.AsFloat, ARank)
  else
    Result := AreaInfo.IsBottomValue(ACell.AsFloat, ARank);
end;

procedure TdxSpreadSheetConditionalFormattingRuleTopBottomValues.DoLoadFromStream(AReader: TcxReader);
begin
  inherited DoLoadFromStream(AReader);
  Direction := TdxSpreadSheetConditionalFormattingRuleTopBottomValuesDirection(AReader.ReadByte);
  ValueType := TdxSpreadSheetConditionalFormattingRuleTopBottomValuesValueType(AReader.ReadByte);
  Value := AReader.ReadInteger;
end;

procedure TdxSpreadSheetConditionalFormattingRuleTopBottomValues.DoSaveToStream(AWriter: TcxWriter);
begin
  inherited DoSaveToStream(AWriter);
  AWriter.WriteByte(Ord(Direction));
  AWriter.WriteByte(Ord(ValueType));
  AWriter.WriteInteger(Value);
end;

function TdxSpreadSheetConditionalFormattingRuleTopBottomValues.GetDetails: string;
const
  Map: array[TdxSpreadSheetConditionalFormattingRuleTopBottomValuesDirection] of Pointer = (
    @sdxConditionalFormattingTopValues,
    @sdxConditionalFormattingBottomValues
  );
var
  AValue: string;
begin
  if ValueType = tbvvtPercent then
    AValue := IntToStr(Value) + '%'
  else
    AValue := IntToStr(Value);

  Result := Format(cxGetResourceString(Map[Direction]), [AValue]);
end;

function TdxSpreadSheetConditionalFormattingRuleTopBottomValues.ValidateValue(AValue: Integer): Integer;
begin
  Result := Max(AValue, 1);
  if ValueType = tbvvtPercent then
    Result := Min(Result, 100)
  else
    Result := Min(Result, 1000)
end;

procedure TdxSpreadSheetConditionalFormattingRuleTopBottomValues.SetDirection(
  AValue: TdxSpreadSheetConditionalFormattingRuleTopBottomValuesDirection);
begin
  if FDirection <> AValue then
  begin
    BeginUpdate;
    try
      FDirection := AValue;
      Changed;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TdxSpreadSheetConditionalFormattingRuleTopBottomValues.SetValue(AValue: Integer);
begin
  AValue := ValidateValue(AValue);
  if FValue <> AValue then
  begin
    BeginUpdate;
    try
      FValue := AValue;
      Changed;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TdxSpreadSheetConditionalFormattingRuleTopBottomValues.SetValueType(
  AValue: TdxSpreadSheetConditionalFormattingRuleTopBottomValuesValueType);
begin
  if FValueType <> AValue then
  begin
    BeginUpdate;
    try
      FValueType := AValue;
      FValue := ValidateValue(Value);
      Changed;
    finally
      EndUpdate;
    end;
  end;
end;

{ TdxSpreadSheetConditionalFormattingRuleUniqueValues }

function TdxSpreadSheetConditionalFormattingRuleUniqueValues.CanApplyCore(
  ARow, AColumn: Integer; const ACell: IdxSpreadSheetCellData = nil): Boolean;
begin
  if (ACell = nil) or ACell.IsEmpty then
    Result := False
  else
    Result := AreaInfo.IsUniqueValue(ACell.AsVariant);
end;

function TdxSpreadSheetConditionalFormattingRuleUniqueValues.GetDetails: string;
begin
  Result := cxGetResourceString(@sdxConditionalFormattingUniqueValuesRuleDetails);
end;

{ TdxSpreadSheetConditionalFormattingRuleCustomScaleStop }

constructor TdxSpreadSheetConditionalFormattingRuleCustomScaleStop.Create(
  AOwner: TdxSpreadSheetConditionalFormattingRuleCustomScale);
begin
  inherited Create;
  FOwner := AOwner;
  FValueType := GetDefaultValueType;
end;

destructor TdxSpreadSheetConditionalFormattingRuleCustomScaleStop.Destroy;
begin
  FreeAndNil(FValueAsExpression);
  inherited Destroy;
end;

procedure TdxSpreadSheetConditionalFormattingRuleCustomScaleStop.Assign(Source: TPersistent);
begin
  if Source is TdxSpreadSheetConditionalFormattingRuleCustomScaleStop then
  begin
    BeginUpdate;
    try
      DoAssign(TdxSpreadSheetConditionalFormattingRuleCustomScaleStop(Source))
    finally
      EndUpdate;
    end;
  end;
end;

procedure TdxSpreadSheetConditionalFormattingRuleCustomScaleStop.BeginUpdate;
begin
  FOwner.BeginUpdate;
end;

procedure TdxSpreadSheetConditionalFormattingRuleCustomScaleStop.EndUpdate;
begin
  FOwner.EndUpdate;
end;

function TdxSpreadSheetConditionalFormattingRuleCustomScaleStop.GetObjectName: string;
begin
  Result := FOwner.GetStopName(Self);
end;

function TdxSpreadSheetConditionalFormattingRuleCustomScaleStop.GetProperties(AProperties: TStrings): Boolean;
begin
  AProperties.Add('ValueType');
  AProperties.Add('Value');
  Result := True;
end;

procedure TdxSpreadSheetConditionalFormattingRuleCustomScaleStop.GetPropertyValue(const AName: string; var AValue: Variant);
begin
  if AName = 'ValueType' then
    AValue := ValueType
  else if AName = 'Value' then
    AValue := Value;
end;

procedure TdxSpreadSheetConditionalFormattingRuleCustomScaleStop.SetPropertyValue(const AName: string; const AValue: Variant);
begin
  if AName = 'ValueType' then
    ValueType := AValue
  else if AName = 'Value' then
    Value := AValue;
end;

procedure TdxSpreadSheetConditionalFormattingRuleCustomScaleStop.DoAssign(
  Source: TdxSpreadSheetConditionalFormattingRuleCustomScaleStop);
begin
  ValueType := Source.ValueType;
  Value := Source.Value;
end;

procedure TdxSpreadSheetConditionalFormattingRuleCustomScaleStop.Changed;
begin
  FOwner.Changed;
end;

procedure TdxSpreadSheetConditionalFormattingRuleCustomScaleStop.LoadFromStream(AReader: TcxReader);
begin
  ValueType := TdxSpreadSheetConditionalFormattingRuleCustomScaleStopValueType(AReader.ReadInteger);
  Value := AReader.ReadVariant;
end;

procedure TdxSpreadSheetConditionalFormattingRuleCustomScaleStop.SaveToStream(AWriter: TcxWriter);
begin
  AWriter.WriteInteger(Ord(ValueType));
  AWriter.WriteVariant(Value);
end;

function TdxSpreadSheetConditionalFormattingRuleCustomScaleStop.ParseExpression(const AValue: string): TdxSpreadSheetConditionalFormattingExpression;
begin
  if AValue <> '' then
  begin
    Result := TdxSpreadSheetConditionalFormattingExpression.Create(FOwner.Owner);
    dxSpreadSheetParseReference(AValue, Result);
  end
  else
    Result := nil;
end;

procedure TdxSpreadSheetConditionalFormattingRuleCustomScaleStop.ResetValue;
begin
  case FValueType of
    cssvtLimitValue:
      Value := Null;
    cssvtPercent, cssvtPercentile:
      Value := MulDiv(Index, 100, FOwner.StopCount - 1);
    cssvtFormula:
      Value := '';
    cssvtValue:
      Value := 0;
  end;
end;

function TdxSpreadSheetConditionalFormattingRuleCustomScaleStop.GetDefaultValueType: TdxSpreadSheetConditionalFormattingRuleCustomScaleStopValueType;
begin
  Result := cssvtLimitValue;
end;

function TdxSpreadSheetConditionalFormattingRuleCustomScaleStop.GetIndex: Integer;
begin
  Result := FOwner.FStops.IndexOf(Self);
end;

function TdxSpreadSheetConditionalFormattingRuleCustomScaleStop.GetValue: Variant;
begin
  if (ValueType = cssvtFormula) and (ValueAsExpression <> nil) then
    Result := ValueAsExpression.AsText
  else
    Result := FValue;
end;

procedure TdxSpreadSheetConditionalFormattingRuleCustomScaleStop.ValidateValue(var AValue: Variant);
begin
  case ValueType of
    cssvtFormula:
      AValue := dxSpreadSheetFormulaIncludeEqualSymbol(AValue);

    cssvtValue:
      try
        AValue := Double(AValue);
      except
        AValue := 0;
      end;

    cssvtPercent, cssvtPercentile:
      try
        AValue := Max(0, Min(Integer(AValue), 100));
      except
        AValue := 0;
      end;
  end;
end;

procedure TdxSpreadSheetConditionalFormattingRuleCustomScaleStop.ValidateValueType(
  var AValueType: TdxSpreadSheetConditionalFormattingRuleCustomScaleStopValueType);
begin
  // do nothing
end;

function TdxSpreadSheetConditionalFormattingRuleCustomScaleStop.GetActualValue: Double;
begin
  FOwner.CheckActualValues;
  Result := FActualValue;
end;

procedure TdxSpreadSheetConditionalFormattingRuleCustomScaleStop.SetValue(AValue: Variant);
begin
  ValidateValue(AValue);
  if (VarType(AValue) <> VarType(FValue)) or (FValue <> AValue) then
  begin
    BeginUpdate;
    try
      FreeAndNil(FValueAsExpression);
      FValue := AValue;
      if ValueType = cssvtFormula then
        FValueAsExpression := ParseExpression(Value);
      Changed;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TdxSpreadSheetConditionalFormattingRuleCustomScaleStop.SetValueType(
  const AValue: TdxSpreadSheetConditionalFormattingRuleCustomScaleStopValueType);
begin
  if FValueType <> AValue then
  begin
    BeginUpdate;
    try
      FreeAndNil(FValueAsExpression);
      FValueType := AValue;
      ResetValue;
      Changed;
    finally
      EndUpdate;
    end;
  end;
end;

{ TdxSpreadSheetConditionalFormattingRuleCustomScale }

constructor TdxSpreadSheetConditionalFormattingRuleCustomScale.Create(AOwner: TdxSpreadSheetCustomConditionalFormatting);
begin
  inherited Create(AOwner);
  FStops := TObjectList<TdxSpreadSheetConditionalFormattingRuleCustomScaleStop>.Create;
  FStopClass := GetStopClass;
  FCacheIsDirty := True;
end;

destructor TdxSpreadSheetConditionalFormattingRuleCustomScale.Destroy;
begin
  FreeAndNil(FStops);
  inherited Destroy;
end;

procedure TdxSpreadSheetConditionalFormattingRuleCustomScale.Changed;
begin
  FCacheIsDirty := True;
  inherited Changed;
end;

procedure TdxSpreadSheetConditionalFormattingRuleCustomScale.CalculateActualValue(
  AStop: TdxSpreadSheetConditionalFormattingRuleCustomScaleStop; const ALimitValue: Double);

  function CalculateFormula(const AFormula: TdxSpreadSheetConditionalFormattingExpression): Double;
  begin
    if AFormula <> nil then
      Result := AFormula.Value
    else
      Result := 0;
  end;

begin
  case AStop.ValueType of
    cssvtLimitValue:
      AStop.FActualValue := ALimitValue;
    cssvtValue:
      AStop.FActualValue := AStop.Value;
    cssvtPercentile:
      AStop.FActualValue := AreaInfo.GetLimitValueForPercentile(AStop.Value);
    cssvtPercent:
      AStop.FActualValue := AreaInfo.GetLimitValueForPercent(AStop.Value);
    cssvtFormula:
      AStop.FActualValue := CalculateFormula(AStop.ValueAsExpression);
  end;
end;

procedure TdxSpreadSheetConditionalFormattingRuleCustomScale.CheckActualValues;
begin
  if FCacheIsDirty then
  begin
    CalculateActualValues;
    FCacheIsDirty := False;
  end;
end;

procedure TdxSpreadSheetConditionalFormattingRuleCustomScale.DoAssign(Source: TdxSpreadSheetCustomConditionalFormattingRule);
var
  I: Integer;
begin
  inherited DoAssign(Source);
  if (Source <> nil) and (ClassType = Source.ClassType) then
  begin
    FStops.Clear;
    for I := 0 to TdxSpreadSheetConditionalFormattingRuleCustomScale(Source).StopCount - 1 do
      AddStop.Assign(TdxSpreadSheetConditionalFormattingRuleCustomScale(Source).FStops[I]);
  end;
end;

procedure TdxSpreadSheetConditionalFormattingRuleCustomScale.DoLoadFromStream(AReader: TcxReader);
var
  I: Integer;
begin
  inherited DoLoadFromStream(AReader);
  for I := 0 to StopCount - 1 do
    FStops[I].LoadFromStream(AReader);
end;

procedure TdxSpreadSheetConditionalFormattingRuleCustomScale.DoSaveToStream(AWriter: TcxWriter);
var
  I: Integer;
begin
  inherited DoSaveToStream(AWriter);
  for I := 0 to StopCount - 1 do
    FStops[I].SaveToStream(AWriter);
end;

procedure TdxSpreadSheetConditionalFormattingRuleCustomScale.FlushCache;
begin
  FCacheIsDirty := True;
  inherited FlushCache;
end;

function TdxSpreadSheetConditionalFormattingRuleCustomScale.AddStop: TdxSpreadSheetConditionalFormattingRuleCustomScaleStop;
begin
  BeginUpdate;
  try
    Result := FStopClass.Create(Self);
    FStops.Add(Result);
    Result.ResetValue;
  finally
    EndUpdate;
  end;
end;

function TdxSpreadSheetConditionalFormattingRuleCustomScale.GetStopName(AStop: TdxSpreadSheetConditionalFormattingRuleCustomScaleStop): string;
begin
  Result := Format('Stop%d', [FStops.IndexOf(AStop)]);
end;

function TdxSpreadSheetConditionalFormattingRuleCustomScale.CreateChild(const AObjectName, AClassName: string): TObject;
var
  I: Integer;
begin
  Result := nil;
  if AClassName <> GetStopClass.ClassName then
    Exit;
  for I := 0 to FStops.Count - 1 do
    if GetStopName(FStops[I]) = AObjectName then
    begin
      Result := FStops[I];
      Break;
    end;
end;

procedure TdxSpreadSheetConditionalFormattingRuleCustomScale.DeleteChild(const AObjectName: string; AObject: TObject);
begin
// do nothing
end;

procedure TdxSpreadSheetConditionalFormattingRuleCustomScale.GetChildren(AChildren: TStringList);
var
  I: Integer;
begin
  for I := 0 to StopCount - 1 do
    AChildren.AddObject(Format('Stop%d', [I]), FStops[I]);
end;

function TdxSpreadSheetConditionalFormattingRuleCustomScale.GetStopClass: TdxSpreadSheetConditionalFormattingRuleCustomScaleStopClass;
begin
  Result := TdxSpreadSheetConditionalFormattingRuleCustomScaleStop;
end;

function TdxSpreadSheetConditionalFormattingRuleCustomScale.GetStopCount: Integer;
begin
  Result := FStops.Count;
end;

{ TdxSpreadSheetConditionalFormattingRuleColorScaleStop }

procedure TdxSpreadSheetConditionalFormattingRuleColorScaleStop.AfterConstruction;
begin
  inherited AfterConstruction;
  FColor := clYellow;
end;

function TdxSpreadSheetConditionalFormattingRuleColorScaleStop.GetProperties(AProperties: TStrings): Boolean;
begin
  inherited GetProperties(AProperties);
  AProperties.Add('Color');
  Result := True;
end;

procedure TdxSpreadSheetConditionalFormattingRuleColorScaleStop.GetPropertyValue(const AName: string; var AValue: Variant);
begin
  if AName = 'Color' then
    AValue := Color
  else
    inherited GetPropertyValue(AName, AValue);
end;

procedure TdxSpreadSheetConditionalFormattingRuleColorScaleStop.SetPropertyValue(const AName: string; const AValue: Variant);
begin
  if AName = 'Color' then
    Color := AValue
  else
    inherited SetPropertyValue(AName, AValue);
end;

procedure TdxSpreadSheetConditionalFormattingRuleColorScaleStop.DoAssign(
  Source: TdxSpreadSheetConditionalFormattingRuleCustomScaleStop);
begin
  inherited DoAssign(Source);
  if Source is TdxSpreadSheetConditionalFormattingRuleColorScaleStop then
    Color := TdxSpreadSheetConditionalFormattingRuleColorScaleStop(Source).Color;
end;

procedure TdxSpreadSheetConditionalFormattingRuleColorScaleStop.LoadFromStream(AReader: TcxReader);
begin
  inherited LoadFromStream(AReader);
  Color := AReader.ReadInteger;
end;

procedure TdxSpreadSheetConditionalFormattingRuleColorScaleStop.SaveToStream(AWriter: TcxWriter);
begin
  inherited SaveToStream(AWriter);
  AWriter.WriteInteger(Color);
end;

procedure TdxSpreadSheetConditionalFormattingRuleColorScaleStop.SetColor(const AValue: TColor);
begin
  if FColor <> AValue then
  begin
    BeginUpdate;
    try
      FColor := AValue;
      Changed;
    finally
      EndUpdate;
    end;
  end;
end;

{ TdxSpreadSheetConditionalFormattingRuleCustomColorScale }

function TdxSpreadSheetConditionalFormattingRuleCustomColorScale.CalculateColor(const AValue: Double): TColor;
var
  ARange: Double;
  AStop: TdxSpreadSheetConditionalFormattingRuleColorScaleStop;
  AStopNext: TdxSpreadSheetConditionalFormattingRuleColorScaleStop;
  I: Integer;
begin
  if AValue <= Stops[0].FActualValue then
    Exit(Stops[0].Color);
  if AValue >= Stops[StopCount - 1].FActualValue then
    Exit(Stops[StopCount - 1].Color);

  AStop := Stops[0];
  for I := 0 to StopCount - 2 do
  begin
    AStopNext := Stops[I + 1];
    if InRange(AValue, AStop.FActualValue, AStopNext.FActualValue) then
    begin
      ARange := AStopNext.FActualValue - AStop.FActualValue;
      if IsZero(ARange) then
        Exit(AStop.Color)
      else
        Exit(dxGetMiddleRGB(AStop.Color, AStopNext.Color, Round(100 - 100 * (AValue - AStop.FActualValue) / ARange)));
    end;
    AStop := AStopNext;
  end;
  Result := clNone;
end;

procedure TdxSpreadSheetConditionalFormattingRuleCustomColorScale.Apply(
  const AStyle: TdxSpreadSheetCellDisplayStyle; ARow, AColumn: Integer; const ACell: IdxSpreadSheetCellData = nil);
var
  AColor: TColor;
begin
  CheckActualValues;
  AColor := CalculateColor(ACell.AsFloat);
  if AColor <> clNone then
    AStyle.Brush.BackgroundColor := AColor;
end;

function TdxSpreadSheetConditionalFormattingRuleCustomColorScale.CanApplyCore(
  ARow, AColumn: Integer; const ACell: IdxSpreadSheetCellData = nil): Boolean;
begin
  Result := not ((ACell = nil) or not ACell.IsNumericValue or (StopCount = 0));
end;

procedure TdxSpreadSheetConditionalFormattingRuleCustomColorScale.DrawPreview(ACanvas: TcxCanvas; R: TRect);
var
  ABrush: TdxGPBrush;
  ADelta: Single;
  I: Integer;
begin
  if StopCount >= 2 then
  begin
    ABrush := TdxGPBrush.Create;
    try
      ADelta := 1 / (StopCount - 1);
      for I := 0 to StopCount - 1 do
        ABrush.GradientPoints.Add(I * ADelta, dxColorToAlphaColor(Stops[I].Color));
      ABrush.GradientMode := gpbgmHorizontal;
      ABrush.Style := gpbsGradient;

      dxGPPaintCanvas.BeginPaint(ACanvas.Handle, R);
      dxGPPaintCanvas.FillRectangle(R, ABrush);
      dxGPPaintCanvas.EndPaint;
    finally
      ABrush.Free;
    end;
  end;
end;

function TdxSpreadSheetConditionalFormattingRuleCustomColorScale.GetDetails: string;
begin
  Result := cxGetResourceString(@sdxConditionalFormattingColorScale);
end;

function TdxSpreadSheetConditionalFormattingRuleCustomColorScale.GetStop(
  AIndex: Integer): TdxSpreadSheetConditionalFormattingRuleColorScaleStop;
begin
  Result := TdxSpreadSheetConditionalFormattingRuleColorScaleStop(FStops[AIndex]);
end;

function TdxSpreadSheetConditionalFormattingRuleCustomColorScale.GetStopClass: TdxSpreadSheetConditionalFormattingRuleCustomScaleStopClass;
begin
  Result := TdxSpreadSheetConditionalFormattingRuleColorScaleStop;
end;

procedure TdxSpreadSheetConditionalFormattingRuleCustomColorScale.SetStop(
  AIndex: Integer; AValue: TdxSpreadSheetConditionalFormattingRuleColorScaleStop);
begin
  FStops[AIndex].Assign(AValue);
end;

{ TdxSpreadSheetConditionalFormattingRuleThreeColorScale }

constructor TdxSpreadSheetConditionalFormattingRuleThreeColorScale.Create(AOwner: TdxSpreadSheetCustomConditionalFormatting);
begin
  inherited Create(AOwner);
  AddStop;
  AddStop;
  AddStop;
  MinValue.Color := DefaultMinValueColor;
  MaxValue.Color := DefaultMaxValueColor;
  MiddleValue.Color := DefaultMiddleValueColor;
end;

procedure TdxSpreadSheetConditionalFormattingRuleThreeColorScale.CalculateActualValues;
begin
  CalculateActualValue(MinValue, AreaInfo.MinValue);
  CalculateActualValue(MiddleValue, AreaInfo.MinValue);
  CalculateActualValue(MaxValue, AreaInfo.MaxValue);
end;

function TdxSpreadSheetConditionalFormattingRuleThreeColorScale.GetStopName(AStop: TdxSpreadSheetConditionalFormattingRuleCustomScaleStop): string;
begin
  if MaxValue = AStop then
    Result := 'MaxValue'
  else if MinValue = AStop then
    Result := 'MinValue'
  else if MiddleValue = AStop then
    Result := 'MiddleValue'
  else
    Result := inherited GetStopName(AStop);
end;

{ TdxSpreadSheetConditionalFormattingRuleTwoColorScale }

constructor TdxSpreadSheetConditionalFormattingRuleTwoColorScale.Create(AOwner: TdxSpreadSheetCustomConditionalFormatting);
begin
  inherited Create(AOwner);
  AddStop;
  AddStop;
  MinValue.Color := DefaultMinValueColor;
  MaxValue.Color := DefaultMaxValueColor;
end;

procedure TdxSpreadSheetConditionalFormattingRuleTwoColorScale.CalculateActualValues;
begin
  CalculateActualValue(MinValue, AreaInfo.MinValue);
  CalculateActualValue(MaxValue, AreaInfo.MaxValue);
end;

function TdxSpreadSheetConditionalFormattingRuleTwoColorScale.GetStopName(AStop: TdxSpreadSheetConditionalFormattingRuleCustomScaleStop): string;
begin
  if MaxValue = AStop then
    Result := 'MaxValue'
  else if MinValue = AStop then
    Result := 'MinValue'
  else
    Result := inherited GetStopName(AStop);
end;

{ TdxSpreadSheetConditionalFormattingRuleDataBarStyle }

constructor TdxSpreadSheetConditionalFormattingRuleDataBarStyle.Create;
begin
  inherited Create;
  FAxisColor := clBlack;
  FNegativeBarBorderColor := clDefault;
  FNegativeBarColor := clDefault;
  FPositiveBarBorderColor := clNone;
  FPositiveBarColor := $EF8B00;
  FFillMode := dbfmGradient;
end;

procedure TdxSpreadSheetConditionalFormattingRuleDataBarStyle.Assign(Source: TPersistent);
begin
  if Source is TdxSpreadSheetConditionalFormattingRuleDataBarStyle then
  begin
    Changing;
    try
      AxisColor := TdxSpreadSheetConditionalFormattingRuleDataBarStyle(Source).AxisColor;
      AxisPosition := TdxSpreadSheetConditionalFormattingRuleDataBarStyle(Source).AxisPosition;
      Direction := TdxSpreadSheetConditionalFormattingRuleDataBarStyle(Source).Direction;
      NegativeBarBorderColor := TdxSpreadSheetConditionalFormattingRuleDataBarStyle(Source).NegativeBarBorderColor;
      NegativeBarColor := TdxSpreadSheetConditionalFormattingRuleDataBarStyle(Source).NegativeBarColor;
      PositiveBarBorderColor := TdxSpreadSheetConditionalFormattingRuleDataBarStyle(Source).PositiveBarBorderColor;
      PositiveBarColor := TdxSpreadSheetConditionalFormattingRuleDataBarStyle(Source).PositiveBarColor;
      FillMode := TdxSpreadSheetConditionalFormattingRuleDataBarStyle(Source).FillMode;
    finally
      Changed;
    end;
  end;
end;

procedure TdxSpreadSheetConditionalFormattingRuleDataBarStyle.Changed;
begin
  dxCallNotify(OnChanged, Self);
end;

procedure TdxSpreadSheetConditionalFormattingRuleDataBarStyle.Changing;
begin
  dxCallNotify(OnChanging, Self);
end;

procedure TdxSpreadSheetConditionalFormattingRuleDataBarStyle.LoadFromStream(AReader: TcxReader);
begin
  Changing;
  try
    AxisColor := AReader.ReadInteger;
    AxisPosition := TdxSpreadSheetConditionalFormattingRuleDataBarAxisPosition(AReader.ReadInteger);
    Direction := TdxSpreadSheetConditionalFormattingRuleDataBarDirection(AReader.ReadInteger);
    FillMode := TdxSpreadSheetConditionalFormattingRuleDataBarFillMode(AReader.ReadInteger);
    NegativeBarBorderColor := AReader.ReadInteger;
    NegativeBarColor := AReader.ReadInteger;
    PositiveBarBorderColor := AReader.ReadInteger;
    PositiveBarColor := AReader.ReadInteger;
  finally
    Changed;
  end;
end;

procedure TdxSpreadSheetConditionalFormattingRuleDataBarStyle.SaveToStream(AWriter: TcxWriter);
begin
  AWriter.WriteInteger(AxisColor);
  AWriter.WriteInteger(Ord(AxisPosition));
  AWriter.WriteInteger(Ord(Direction));
  AWriter.WriteInteger(Ord(FillMode));
  AWriter.WriteInteger(NegativeBarBorderColor);
  AWriter.WriteInteger(NegativeBarColor);
  AWriter.WriteInteger(PositiveBarBorderColor);
  AWriter.WriteInteger(PositiveBarColor);
end;

function TdxSpreadSheetConditionalFormattingRuleDataBarStyle.GetActualNegativeBarBorderColor: TColor;
begin
  Result := cxGetActualColor(NegativeBarBorderColor, PositiveBarBorderColor);
end;

function TdxSpreadSheetConditionalFormattingRuleDataBarStyle.GetActualNegativeBarColor: TColor;
begin
  Result := cxGetActualColor(NegativeBarColor, PositiveBarColor);
end;

procedure TdxSpreadSheetConditionalFormattingRuleDataBarStyle.SetAxisColor(AValue: TColor);
begin
  if FAxisColor <> AValue then
  begin
    Changing;
    try
      FAxisColor := AValue;
    finally
      Changed;
    end;
  end;
end;

procedure TdxSpreadSheetConditionalFormattingRuleDataBarStyle.SetAxisPosition(
  AValue: TdxSpreadSheetConditionalFormattingRuleDataBarAxisPosition);
begin
  if FAxisPosition <> AValue then
  begin
    Changing;
    try
      FAxisPosition := AValue;
    finally
      Changed;
    end;
  end;
end;

procedure TdxSpreadSheetConditionalFormattingRuleDataBarStyle.SetDirection(
  AValue: TdxSpreadSheetConditionalFormattingRuleDataBarDirection);
begin
  if FDirection <> AValue then
  begin
    Changing;
    try
      FDirection := AValue;
    finally
      Changed;
    end;
  end;
end;

procedure TdxSpreadSheetConditionalFormattingRuleDataBarStyle.SetFillMode(
  AValue: TdxSpreadSheetConditionalFormattingRuleDataBarFillMode);
begin
  if FFillMode <> AValue then
  begin
    Changing;
    try
      FFillMode := AValue;
    finally
      Changed;
    end;
  end;
end;

procedure TdxSpreadSheetConditionalFormattingRuleDataBarStyle.SetNegativeBarBorderColor(AValue: TColor);
begin
  if FNegativeBarBorderColor <> AValue then
  begin
    Changing;
    try
      FNegativeBarBorderColor := AValue;
    finally
      Changed;
    end;
  end;
end;

procedure TdxSpreadSheetConditionalFormattingRuleDataBarStyle.SetNegativeBarColor(AValue: TColor);
begin
  if FNegativeBarColor <> AValue then
  begin
    Changing;
    try
      FNegativeBarColor := AValue;
    finally
      Changed;
    end;
  end;
end;

procedure TdxSpreadSheetConditionalFormattingRuleDataBarStyle.SetPositiveBarBorderColor(AValue: TColor);
begin
  if FPositiveBarBorderColor <> AValue then
  begin
    Changing;
    try
      FPositiveBarBorderColor := AValue;
    finally
      Changed;
    end;
  end;
end;

procedure TdxSpreadSheetConditionalFormattingRuleDataBarStyle.SetPositiveBarColor(AValue: TColor);
begin
  if FPositiveBarColor <> AValue then
  begin
    Changing;
    try
      FPositiveBarColor := AValue;
    finally
      Changed;
    end;
  end;
end;

{ TdxSpreadSheetConditionalFormattingRuleDataBar }

constructor TdxSpreadSheetConditionalFormattingRuleDataBar.Create(AOwner: TdxSpreadSheetCustomConditionalFormatting);
begin
  inherited Create(AOwner);
  FStyle := TdxSpreadSheetConditionalFormattingRuleDataBarStyle.Create;
  FStyle.OnChanged := StyleChangedHandler;
  FStyle.OnChanging := StyleChangingHandler;
  FShowValue := True;
  AddStop;
  AddStop;
end;

destructor TdxSpreadSheetConditionalFormattingRuleDataBar.Destroy;
begin
  FreeAndNil(FStyle);
  inherited Destroy;
end;

function TdxSpreadSheetConditionalFormattingRuleDataBar.GetProperties(AProperties: TStrings): Boolean;
begin
  inherited GetProperties(AProperties);
  AProperties.Add('ShowValue');
  AProperties.Add('StyleAxisColor');
  AProperties.Add('StyleAxisPosition');
  AProperties.Add('StyleDirection');
  AProperties.Add('StyleFillMode');
  AProperties.Add('StyleNegativeBarBorderColor');
  AProperties.Add('StyleNegativeBarColor');
  AProperties.Add('StylePositiveBarBorderColor');
  AProperties.Add('StylePositiveBarColor');
  Result := True;
end;

procedure TdxSpreadSheetConditionalFormattingRuleDataBar.GetPropertyValue(const AName: string; var AValue: Variant);
begin
  if AName = 'ShowValue' then
    AValue := ShowValue
  else if AName = 'StyleAxisColor' then
    AValue := Style.AxisColor
  else if AName = 'StyleAxisPosition' then
    AValue := Style.AxisPosition
  else if AName = 'StyleDirection' then
    AValue := Style.Direction
  else if AName = 'StyleFillMode' then
    AValue := Style.FillMode
  else if AName = 'StyleNegativeBarBorderColor' then
    AValue := Style.NegativeBarBorderColor
  else if AName = 'StyleNegativeBarColor' then
    AValue := Style.NegativeBarColor
  else if AName = 'StylePositiveBarBorderColor' then
    AValue := Style.PositiveBarBorderColor
  else if AName = 'StylePositiveBarColor' then
    AValue := Style.PositiveBarColor
  else
    inherited GetPropertyValue(AName, AValue);
end;

procedure TdxSpreadSheetConditionalFormattingRuleDataBar.SetPropertyValue(const AName: string; const AValue: Variant);
begin
  if AName = 'ShowValue' then
    ShowValue := AValue
  else if AName = 'StyleAxisColor' then
    Style.AxisColor := AValue
  else if AName = 'StyleAxisPosition' then
    Style.AxisPosition := AValue
  else if AName = 'StyleDirection' then
    Style.Direction := AValue
  else if AName = 'StyleFillMode' then
    Style.FillMode := AValue
  else if AName = 'StyleNegativeBarBorderColor' then
    Style.NegativeBarBorderColor := AValue
  else if AName = 'StyleNegativeBarColor' then
    Style.NegativeBarColor := AValue
  else if AName = 'StylePositiveBarBorderColor' then
    Style.PositiveBarBorderColor := AValue
  else if AName = 'StylePositiveBarColor' then
    Style.PositiveBarColor := AValue
  else
    inherited SetPropertyValue(AName, AValue);
end;

procedure TdxSpreadSheetConditionalFormattingRuleDataBar.CalculateActualValues;
begin
  CalculateActualValue(MinValue, AreaInfo.MinValue);
  CalculateActualValue(MaxValue, AreaInfo.MaxValue);
end;

procedure TdxSpreadSheetConditionalFormattingRuleDataBar.CalculateStyle(
  const AStyle: TdxSpreadSheetCellDisplayStyle; const AValue: Double);
begin
  if Style.AxisPosition <> dbapNone then
    AStyle.DataBar.AxisColor := Style.AxisColor;

  if AValue < 0 then
  begin
    AStyle.DataBar.Border := Style.ActualNegativeBarBorderColor;
    AStyle.DataBar.Color1 := Style.ActualNegativeBarColor;
  end
  else
  begin
    AStyle.DataBar.Border := Style.PositiveBarBorderColor;
    AStyle.DataBar.Color1 := Style.PositiveBarColor;
  end;

  AStyle.DataBar.Color2 := AStyle.DataBar.Color1;
  if Style.FillMode = dbfmGradient then
  begin
    if (AValue < 0) and (Style.AxisPosition <> dbapNone) then
      AStyle.DataBar.Color1 := clWhite
    else
      AStyle.DataBar.Color2 := clWhite;
  end;

  if not ShowValue then
    AStyle.ShowCellValue := False;
end;

procedure TdxSpreadSheetConditionalFormattingRuleDataBar.DoAssign(Source: TdxSpreadSheetCustomConditionalFormattingRule);
begin
  inherited DoAssign(Source);
  if Source is TdxSpreadSheetConditionalFormattingRuleDataBar then
  begin
    Style := TdxSpreadSheetConditionalFormattingRuleDataBar(Source).Style;
    ShowValue := TdxSpreadSheetConditionalFormattingRuleDataBar(Source).ShowValue;
  end;
end;

function TdxSpreadSheetConditionalFormattingRuleDataBar.CanApplyCore(
  ARow, AColumn: Integer; const ACell: IdxSpreadSheetCellData = nil): Boolean;
begin
  Result := (ACell <> nil) and ACell.IsNumericValue;
end;

procedure TdxSpreadSheetConditionalFormattingRuleDataBar.Apply(
  const AStyle: TdxSpreadSheetCellDisplayStyle; ARow, AColumn: Integer; const ACell: IdxSpreadSheetCellData = nil);

  function CheckRange(const AValue: Double): Double; inline;
  begin
    Result := Min(Max(AValue, 0), 1);
  end;

var
  ARange: Double;
  AValue: Double;
begin
  CheckActualValues;

  AValue := ACell.AsFloat;
  if (AValue < MinValue.ActualValue) or (AValue > MaxValue.ActualValue) then
    Exit;

  if Style.AxisPosition = dbapMidpoint then
  begin
    ARange := Max(Abs(MinValue.ActualValue), Abs(MaxValue.ActualValue));
    if (ARange < 0) or IsZero(ARange) then
      Exit;
    if (AValue < 0) and (MinValue.ActualValue >= 0) then
      AValue := 0;
    if (AValue > 0) and (MaxValue.ActualValue <= 0) then
      AValue := 0;
    AStyle.DataBar.AxisPosition := 0.5;
    AStyle.DataBar.Position := AStyle.DataBar.AxisPosition + 0.5 * Sign(AValue) * CheckRange(Abs(AValue) / ARange);
  end
  else
  begin
    ARange := MaxValue.ActualValue - Min(MinValue.ActualValue, 0);
    if (ARange < 0) or IsZero(ARange) then
      Exit;
    if Style.AxisPosition = dbapAuto then
      AStyle.DataBar.AxisPosition := CheckRange(-MinValue.ActualValue / ARange)
    else
      AStyle.DataBar.AxisPosition := 0;

    AStyle.DataBar.Position := CheckRange((AValue - Min(MinValue.ActualValue, 0)) / ARange);
  end;

  CalculateStyle(AStyle, AValue);
  ApplyDirection(AStyle, ACell);
end;

procedure TdxSpreadSheetConditionalFormattingRuleDataBar.DoLoadFromStream(AReader: TcxReader);
begin
  inherited DoLoadFromStream(AReader);
  ShowValue := AReader.ReadBoolean;
  Style.LoadFromStream(AReader);
end;

procedure TdxSpreadSheetConditionalFormattingRuleDataBar.DoSaveToStream(AWriter: TcxWriter);
begin
  inherited DoSaveToStream(AWriter);
  AWriter.WriteBoolean(ShowValue);
  Style.SaveToStream(AWriter);
end;

procedure TdxSpreadSheetConditionalFormattingRuleDataBar.DrawPreview(ACanvas: TcxCanvas; R: TRect);
var
  AColor1: TColor;
  AColor2: TColor;
begin
  R := cxRectInflate(R, -cxTextSpace);

  AColor1 := Style.PositiveBarColor;
  AColor2 := Style.PositiveBarColor;
  if Style.FillMode = dbfmGradient then
    AColor2 := clWhite;
  if IsRightToLeftDirection then
    ExchangeLongWords(AColor1, AColor2);
  if IsRightToLeftDirection then
    Inc(R.Left, cxRectHeight(R))
  else
    Dec(R.Right, cxRectHeight(R));

  dxGpFillRectByGradient(ACanvas.Handle, R, AColor1, AColor2, LinearGradientModeHorizontal);
  ACanvas.FrameRect(R, Style.PositiveBarBorderColor);
end;

function TdxSpreadSheetConditionalFormattingRuleDataBar.GetDetails: string;
begin
  Result := cxGetResourceString(@sdxConditionalFormattingDataBar);
end;

function TdxSpreadSheetConditionalFormattingRuleDataBar.GetStop(
  AIndex: Integer): TdxSpreadSheetConditionalFormattingRuleCustomScaleStop;
begin
  Result := FStops[AIndex];
end;

function TdxSpreadSheetConditionalFormattingRuleDataBar.GetStopName(AStop: TdxSpreadSheetConditionalFormattingRuleCustomScaleStop): string;
begin
  if MaxValue = AStop then
    Result := 'MaxValue'
  else if MinValue = AStop then
    Result := 'MinValue'
  else
    Result := inherited GetStopName(AStop);
end;

function TdxSpreadSheetConditionalFormattingRuleDataBar.IsRightToLeftDirection: Boolean;
begin
  Result := (Style.Direction = dbdRightToLeft) or (Style.Direction = dbdAuto) and Owner.Owner.IsRightToLeft;
end;

procedure TdxSpreadSheetConditionalFormattingRuleDataBar.SetStop(
  AIndex: Integer; AValue: TdxSpreadSheetConditionalFormattingRuleCustomScaleStop);
begin
  FStops[AIndex].Assign(AValue);
end;

procedure TdxSpreadSheetConditionalFormattingRuleDataBar.ApplyDirection(
  const AStyle: TdxSpreadSheetCellDisplayStyle; const ACell: IdxSpreadSheetCellData = nil);
var
  AColor: TColor;
begin
  if IsRightToLeftDirection then
  begin
    AColor := AStyle.DataBar.Color2;
    AStyle.DataBar.Color2 := AStyle.DataBar.Color1;
    AStyle.DataBar.Color1 := AColor;

    AStyle.DataBar.AxisPosition := 1 - AStyle.DataBar.AxisPosition;
    AStyle.DataBar.Position := 1 - AStyle.DataBar.Position;
  end;
end;

procedure TdxSpreadSheetConditionalFormattingRuleDataBar.StyleChangedHandler(Sender: TObject);
begin
  Changed;
  EndUpdate;
end;

procedure TdxSpreadSheetConditionalFormattingRuleDataBar.StyleChangingHandler(Sender: TObject);
begin
  BeginUpdate;
end;

procedure TdxSpreadSheetConditionalFormattingRuleDataBar.SetShowValue(AValue: Boolean);
begin
  if FShowValue <> AValue then
  begin
    BeginUpdate;
    try
      FShowValue := AValue;
      Changed;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TdxSpreadSheetConditionalFormattingRuleDataBar.SetStyle(AValue: TdxSpreadSheetConditionalFormattingRuleDataBarStyle);
begin
  FStyle.Assign(AValue);
end;

{ TdxSpreadSheetConditionalFormattingRuleIconSetStop }

constructor TdxSpreadSheetConditionalFormattingRuleIconSetStop.Create(
  AOwner: TdxSpreadSheetConditionalFormattingRuleCustomScale);
begin
  inherited Create(AOwner);
  FComparisonOperator := isscoGreaterThanOrEqual;
end;

function TdxSpreadSheetConditionalFormattingRuleIconSetStop.GetProperties(AProperties: TStrings): Boolean;
begin
  inherited GetProperties(AProperties);
  AProperties.Add('ComparisonOperator');
  AProperties.Add('IconIndex');
  Result := True;
end;

procedure TdxSpreadSheetConditionalFormattingRuleIconSetStop.GetPropertyValue(const AName: string; var AValue: Variant);
begin
  if AName = 'ComparisonOperator' then
    AValue := ComparisonOperator
  else if AName = 'IconIndex' then
    AValue := IconIndex
  else
    inherited GetPropertyValue(AName, AValue);
end;

procedure TdxSpreadSheetConditionalFormattingRuleIconSetStop.SetPropertyValue(const AName: string; const AValue: Variant);
begin
  if AName = 'ComparisonOperator' then
    ComparisonOperator := AValue
  else if AName = 'IconIndex' then
    IconIndex := AValue
  else
    inherited SetPropertyValue(AName, AValue);
end;

function TdxSpreadSheetConditionalFormattingRuleIconSetStop.Compare(const AValue: Double): Boolean;
begin
  if ComparisonOperator = isscoGreaterThan then
    Result := AValue > ActualValue
  else
    Result := AValue >= ActualValue
end;

procedure TdxSpreadSheetConditionalFormattingRuleIconSetStop.DoAssign(Source: TdxSpreadSheetConditionalFormattingRuleCustomScaleStop);
begin
  inherited DoAssign(Source);
  if Source is TdxSpreadSheetConditionalFormattingRuleIconSetStop then
  begin
    ComparisonOperator := TdxSpreadSheetConditionalFormattingRuleIconSetStop(Source).ComparisonOperator;
    IconIndex := TdxSpreadSheetConditionalFormattingRuleIconSetStop(Source).IconIndex;
  end;
end;

function TdxSpreadSheetConditionalFormattingRuleIconSetStop.GetDefaultValueType: TdxSpreadSheetConditionalFormattingRuleCustomScaleStopValueType;
begin
  Result := cssvtPercent;
end;

procedure TdxSpreadSheetConditionalFormattingRuleIconSetStop.LoadFromStream(AReader: TcxReader);
begin
  inherited LoadFromStream(AReader);
  ComparisonOperator := TdxSpreadSheetConditionalFormattingRuleIconSetStopComparisonOperator(AReader.ReadInteger);
  IconIndex := AReader.ReadInteger;
end;

procedure TdxSpreadSheetConditionalFormattingRuleIconSetStop.SaveToStream(AWriter: TcxWriter);
begin
  inherited SaveToStream(AWriter);
  AWriter.WriteInteger(Ord(ComparisonOperator));
  AWriter.WriteInteger(IconIndex);
end;

procedure TdxSpreadSheetConditionalFormattingRuleIconSetStop.ResetValue;
begin
  case ValueType of
    cssvtPercent, cssvtPercentile:
      Value := MulDiv(Index, 100, FOwner.StopCount);
  else
    inherited ResetValue;
  end;
end;

procedure TdxSpreadSheetConditionalFormattingRuleIconSetStop.ValidateValueType(
  var AValueType: TdxSpreadSheetConditionalFormattingRuleCustomScaleStopValueType);
begin
  if AValueType = cssvtLimitValue then
    AValueType := cssvtValue;
end;

procedure TdxSpreadSheetConditionalFormattingRuleIconSetStop.SetComparisonOperator(
  AValue: TdxSpreadSheetConditionalFormattingRuleIconSetStopComparisonOperator);
begin
  if FComparisonOperator <> AValue then
  begin
    BeginUpdate;
    try
      FComparisonOperator := AValue;
      Changed;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TdxSpreadSheetConditionalFormattingRuleIconSetStop.SetIconIndex(AValue: Integer);
begin
  AValue := Max(-1, Min(AValue, ConditionalFormattingIconSet.Icons.Count - 1));
  if FIconIndex <> AValue then
  begin
    BeginUpdate;
    try
      FIconIndex := AValue;
      TdxSpreadSheetConditionalFormattingRuleIconSet(FOwner).IconsChanged;
    finally
      EndUpdate;
    end;
  end;
end;

{ TdxSpreadSheetConditionalFormattingRuleIconSet }

constructor TdxSpreadSheetConditionalFormattingRuleIconSet.Create(AOwner: TdxSpreadSheetCustomConditionalFormatting);
begin
  inherited Create(AOwner);
  FShowValue := True;
  PresetName := ConditionalFormattingIconSet.Presets.First.Name;
end;

function TdxSpreadSheetConditionalFormattingRuleIconSet.GetProperties(AProperties: TStrings): Boolean;
begin
  inherited GetProperties(AProperties);
  AProperties.Add('Order');
  AProperties.Add('PresetName');
  AProperties.Add('ShowValue');
  AProperties.Add('StopCount');
  Result := True;
end;

procedure TdxSpreadSheetConditionalFormattingRuleIconSet.GetPropertyValue(const AName: string; var AValue: Variant);
begin
  if AName = 'Order' then
    AValue := Order
  else if AName = 'PresetName' then
    AValue := PresetName
  else if AName = 'ShowValue' then
    AValue := ShowValue
  else if AName = 'StopCount' then
    AValue := StopCount
  else
    inherited GetPropertyValue(AName, AValue);
end;

procedure TdxSpreadSheetConditionalFormattingRuleIconSet.SetPropertyValue(const AName: string; const AValue: Variant);
begin
  if AName = 'Order' then
    Order := AValue
  else if AName = 'PresetName' then
    PresetName := AValue
  else if AName = 'ShowValue' then
    ShowValue := AValue
  else if AName = 'StopCount' then
    StopCount := AValue
  else
    inherited SetPropertyValue(AName, AValue);
end;

procedure TdxSpreadSheetConditionalFormattingRuleIconSet.CalculateActualValues;
var
  I: Integer;
begin
  for I := 0 to StopCount - 1 do
    CalculateActualValue(Stops[I], 0);
end;

procedure TdxSpreadSheetConditionalFormattingRuleIconSet.DoAssign(Source: TdxSpreadSheetCustomConditionalFormattingRule);
begin
  inherited DoAssign(Source);
  if Source is TdxSpreadSheetConditionalFormattingRuleIconSet then
  begin
    PresetName := TdxSpreadSheetConditionalFormattingRuleIconSet(Source).PresetName;
    Order := TdxSpreadSheetConditionalFormattingRuleIconSet(Source).Order;
    ShowValue := TdxSpreadSheetConditionalFormattingRuleIconSet(Source).ShowValue;
  end;
end;

procedure TdxSpreadSheetConditionalFormattingRuleIconSet.Apply(
  const AStyle: TdxSpreadSheetCellDisplayStyle; ARow, AColumn: Integer; const ACell: IdxSpreadSheetCellData = nil);
var
  AValue: Double;
  I: Integer;
begin
  CheckActualValues;
  AValue := ACell.AsFloat;
  for I := StopCount - 1 downto 0 do
    if Stops[I].Compare(AValue) then
    begin
      AStyle.IconIndex := Stops[I].IconIndex;
      if not ShowValue then
        AStyle.ShowCellValue := False;
      Break;
    end;
end;

function TdxSpreadSheetConditionalFormattingRuleIconSet.CanApplyCore(ARow, AColumn: Integer; const ACell: IdxSpreadSheetCellData = nil): Boolean;
begin
  Result := (ACell <> nil) and ACell.IsNumericValue and (StopCount > 0);
end;

procedure TdxSpreadSheetConditionalFormattingRuleIconSet.DoLoadFromStream(AReader: TcxReader);
begin
  ShowValue := AReader.ReadBoolean;
  Order := TdxSpreadSheetConditionalFormattingRuleIconSetOrder(AReader.ReadInteger);
  PresetName := AReader.ReadWideString;
  inherited DoLoadFromStream(AReader);
end;

procedure TdxSpreadSheetConditionalFormattingRuleIconSet.DoSaveToStream(AWriter: TcxWriter);
begin
  AWriter.WriteBoolean(ShowValue);
  AWriter.WriteInteger(Ord(Order));
  AWriter.WriteWideString(PresetName);
  inherited DoSaveToStream(AWriter);
end;

procedure TdxSpreadSheetConditionalFormattingRuleIconSet.DrawPreview(ACanvas: TcxCanvas; R: TRect);
const
  SpaceBetweenIcons = 5;
var
  AIconSize: TSize;
  AScaleFactor: TdxScaleFactor;
  ASpaceBetweenIcons: Integer;
  I: Integer;
begin
  AScaleFactor := dxGetScaleFactorForCanvas(ACanvas.Canvas);
  ASpaceBetweenIcons := AScaleFactor.Apply(SpaceBetweenIcons);
  AIconSize := dxGetImageSize(Icons, AScaleFactor);

  R := cxRectCenter(R, (AIconSize.cx + ASpaceBetweenIcons) * StopCount - ASpaceBetweenIcons, AIconSize.cy);
  R := cxRectSetWidth(R, AIconSize.cx);
  for I := 0 to StopCount - 1 do
  begin
    Icons.Draw(ACanvas.Canvas, R, Stops[I].IconIndex);
    R := cxRectOffset(R, cxRectWidth(R) + ASpaceBetweenIcons, 0);
  end;
end;

function TdxSpreadSheetConditionalFormattingRuleIconSet.GetDetails: string;
begin
  Result := cxGetResourceString(@sdxConditionalFormattingIconSet);
end;

procedure TdxSpreadSheetConditionalFormattingRuleIconSet.IconsChanged;
begin
  FPreset := nil;
  Changed;
end;

function TdxSpreadSheetConditionalFormattingRuleIconSet.GetStopClass: TdxSpreadSheetConditionalFormattingRuleCustomScaleStopClass;
begin
  Result := TdxSpreadSheetConditionalFormattingRuleIconSetStop;
end;

procedure TdxSpreadSheetConditionalFormattingRuleIconSet.LoadStopsFromPreset(
  APreset: TdxSpreadSheetConditionalFormattingIconSetPreset);
var
  I: Integer;
begin
  BeginUpdate;
  try
    StopCount := APreset.IconIndexCount;

    if Order = isioReversed then
    begin
      for I := 0 to StopCount - 1 do
        Stops[I].IconIndex := APreset.IconIndexes[StopCount - I - 1]
    end
    else
      for I := 0 to StopCount - 1 do
        Stops[I].IconIndex := APreset.IconIndexes[I];

    FPreset := APreset;
  finally
    EndUpdate;
  end;
end;

function TdxSpreadSheetConditionalFormattingRuleIconSet.GetIcons: TcxImageList;
begin
  Result := ConditionalFormattingIconSet.Icons;
end;

function TdxSpreadSheetConditionalFormattingRuleIconSet.GetPresetName: string;
begin
  if FPreset <> nil then
    Result := FPreset.Name
  else
    Result := '';
end;

function TdxSpreadSheetConditionalFormattingRuleIconSet.GetStop(
  AIndex: Integer): TdxSpreadSheetConditionalFormattingRuleIconSetStop;
begin
  Result := TdxSpreadSheetConditionalFormattingRuleIconSetStop(FStops[AIndex]);
end;

procedure TdxSpreadSheetConditionalFormattingRuleIconSet.SetShowValue(AValue: Boolean);
begin
  if FShowValue <> AValue then
  begin
    BeginUpdate;
    try
      FShowValue := AValue;
      Changed;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TdxSpreadSheetConditionalFormattingRuleIconSet.SetOrder(
  AValue: TdxSpreadSheetConditionalFormattingRuleIconSetOrder);
var
  AIconIndex: Integer;
  I: Integer;
begin
  if Order <> AValue then
  begin
    BeginUpdate;
    try
      FOrder := AValue;
      if FPreset <> nil then
        LoadStopsFromPreset(FPreset)
      else
        for I := 0 to StopCount div 2 - 1 do
        begin
          AIconIndex := Stops[I].IconIndex;
          Stops[I].IconIndex := Stops[StopCount - 1 - I].IconIndex;
          Stops[StopCount - 1 - I].IconIndex := AIconIndex;
        end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TdxSpreadSheetConditionalFormattingRuleIconSet.SetPresetName(const AValue: string);
var
  APreset: TdxSpreadSheetConditionalFormattingIconSetPreset;
  APrevStopCount: Integer;
  I: Integer;
begin
  if PresetName <> AValue then
  begin
    BeginUpdate;
    try
      FPreset := nil;
      if ConditionalFormattingIconSet.Presets.FindByName(AValue, APreset) then
      begin
        FPreset := APreset;
        APrevStopCount := StopCount;
        LoadStopsFromPreset(APreset);
        if APrevStopCount <> StopCount then
        begin
          for I := 0 to StopCount - 1 do
            Stops[I].ResetValue;
        end;
      end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TdxSpreadSheetConditionalFormattingRuleIconSet.SetStop(
  AIndex: Integer; AValue: TdxSpreadSheetConditionalFormattingRuleIconSetStop);
begin
  FStops[AIndex].Assign(AValue);
end;

procedure TdxSpreadSheetConditionalFormattingRuleIconSet.SetStopCount(AValue: Integer);
var
  I: Integer;
begin
  AValue := Min(Max(AValue, 3), 5);
  if StopCount <> AValue then
  begin
    BeginUpdate;
    try
      for I := FStops.Count to AValue - 1 do
        AddStop;
      FStops.Count := AValue;
      IconsChanged;
    finally
      EndUpdate;
    end;
  end;
end;

initialization
  RegisterClass(TdxSpreadSheetConditionalFormattingRuleAboveOrBelowAverage);
  RegisterClass(TdxSpreadSheetConditionalFormattingRuleCellIs);
  RegisterClass(TdxSpreadSheetConditionalFormattingRuleDataBar);
  RegisterClass(TdxSpreadSheetConditionalFormattingRuleDuplicateValues);
  RegisterClass(TdxSpreadSheetConditionalFormattingRuleExpression);
  RegisterClass(TdxSpreadSheetConditionalFormattingRuleIconSet);
  RegisterClass(TdxSpreadSheetConditionalFormattingRuleThreeColorScale);
  RegisterClass(TdxSpreadSheetConditionalFormattingRuleTopBottomValues);
  RegisterClass(TdxSpreadSheetConditionalFormattingRuleTwoColorScale);
  RegisterClass(TdxSpreadSheetConditionalFormattingRuleUniqueValues);
end.
