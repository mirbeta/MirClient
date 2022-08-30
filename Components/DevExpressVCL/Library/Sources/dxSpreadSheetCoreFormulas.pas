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

unit dxSpreadSheetCoreFormulas;

{$I cxVer.Inc}

interface

uses
  Types, Math, Classes, SysUtils, Variants, StrUtils, Generics.Collections, Generics.Defaults,
  // cx
  cxClasses,
  cxDateUtils,
  cxFormats,
  cxGeometry,
  cxVariants,
  dxCore,
  dxCoreClasses,
  // SpreadSheet
  dxSpreadSheetTypes,
  dxSpreadSheetClasses,
  dxSpreadSheetCoreStyles,
  dxSpreadSheetUtils;

type
  PdxSpreadSheetCustomFormula = ^TdxSpreadSheetCustomFormula;
  TdxSpreadSheetCustomFormula = class;
  TdxSpreadSheetFormulaResult = class;
  TdxSpreadSheetFormulaToken = class;
  TdxSpreadSheetFormulaTokenClass = class of TdxSpreadSheetFormulaToken;
  TdxSpreadSheetFormulaFormattedText = class;
  TdxSpreadSheet3DReferenceCustomLink = class;

  TdxSpreadSheetFunctionResultKind = (frkValue, frkArray, frkNonArrayValue, frkParamValue);

  TdxSpreadSheetFunctionParamKind = (fpkValue, fpkArray, fpkUnlimited, fpkNonRequiredValue, fpkNonRequiredArray, fpkNonRequiredUnlimited);
  TdxSpreadSheetFunctionParamKindInfo = array of TdxSpreadSheetFunctionParamKind;

  TdxSpreadSheetForEachCallBack = function(const AValue: Variant; ACanConvertStrToNumber: Boolean;
    var AErrorCode: TdxSpreadSheetFormulaErrorCode; AData, AInfo: Pointer): Boolean;

  TdxSpreadSheetFormulaEnumReferencesProc = reference to procedure (const AArea: TRect; AView: TObject);

  TdxSpreadSheetArrayFormulaPart = (afpNone, afpMasterCell, afpSlaveCell);

  { Exceptions }

  EdxSpreadSheetCircularReferencesError = class(EdxSpreadSheetError);
  EdxSpreadSheetFormulaIsTooLongError = class(EdxSpreadSheetError);

  { TdxSpreadSheetCustomFormulaList }

  TdxSpreadSheetCustomFormulaList = class(TdxFastList)
  strict private
    function GetItem(Index: Integer): TdxSpreadSheetCustomFormula; inline;
  public
    property Items[Index: Integer]: TdxSpreadSheetCustomFormula read GetItem; default;
  end;

  { TdxSpreadSheetCustomFormulaController }

  TdxSpreadSheetEnumFormulasFunc = reference to function (AFormula: TdxSpreadSheetCustomFormula): Boolean;

  TdxSpreadSheetCustomFormulaController = class abstract(TcxObjectList,
    IcxFormatControllerListener,
    IdxLocalizerListener,
    IUnknown)
  strict private
    FArrayFormulas: TDictionary<TObject, TdxSpreadSheetCustomFormulaList>;
    FCalculationInProcess: Boolean;
    FCircularPath: TdxSpreadSheetReferencePath;
    FCircularReference: Boolean;
    FCircularReferencePaths: TcxObjectList;
    FHasCircularReferences: Boolean;
    FUpdatingSlaveCells: Boolean;

    function GetCircularReferencePathsAsString: string;
    function GetItem(AIndex: Integer): TdxSpreadSheetCustomFormula;
  protected
    procedure AddPath(ARow, AColumn: Integer; AView: TObject);
    procedure RemovePath(ARow, AColumn: Integer; AView: TObject);

    procedure CheckCircularReferences;
    function CircularPathToString(APath: TdxSpreadSheetReferencePath): string;

    procedure CleanFunctionTokenCachedResults;
    procedure ClearFormulasResults; virtual;
    function CreateParser: TObject; {TdxSpreadSheetCustomFormulaParser} virtual; abstract;
    function GetFormatSettings: TdxSpreadSheetFormatSettings; virtual; abstract;
    function NeedAddFeatureFunctionPrefixToFunctionName: Boolean; virtual;
    procedure InitializeStrings; virtual;

    // Values
    function ExtractVector(AView: TObject; ARow, AColumn, ALength: Integer;
      AIsVertical: Boolean; var AErrorCode: TdxSpreadSheetFormulaErrorCode): TdxSpreadSheetVector;
    procedure GetCellValue(const AViewData: IdxSpreadSheetViewData; ARow, AColumn: Integer;
      out AValue: Variant; out AErrorCode: TdxSpreadSheetFormulaErrorCode); inline;
    procedure GetCellValueCore(const AViewData: IdxSpreadSheetViewData; ARow, AColumn: Integer;
      out AValue: Variant; out AErrorCode: TdxSpreadSheetFormulaErrorCode);

    // Array Formulas
    function GetArrayFormulaSlaveCellValue(AViewData: IdxSpreadSheetViewData; AArrayFormula: TdxSpreadSheetCustomFormula;
      const ARow, AColumn: Integer; var AErrorCode: TdxSpreadSheetFormulaErrorCode): Variant;
    function IsPartOfArrayFormula(AView: TObject; const ARow, AColumn: Integer;
      AFormula: PdxSpreadSheetCustomFormula = nil): TdxSpreadSheetArrayFormulaPart;
    procedure UpdateSlaveCells(AView: TObject; AFormula: TdxSpreadSheetCustomFormula);

    // IdxLocalizerListener
    procedure TranslationChanged;
    // IcxFormatControllerListener
    procedure FormatChanged;
    // IUnknown
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function QueryInterface(const IID: TGUID; out Obj): HResult; virtual; stdcall;

    property CircularPath: TdxSpreadSheetReferencePath read FCircularPath;
    property CircularReference: Boolean read FCircularReference write FCircularReference;
    property HasCircularReferences: Boolean read FHasCircularReferences write FHasCircularReferences;
    property UpdatingSlaveCells: Boolean read FUpdatingSlaveCells;
    property ArrayFormulas: TDictionary<TObject, TdxSpreadSheetCustomFormulaList> read FArrayFormulas;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddArrayFormula(AFormula: TdxSpreadSheetCustomFormula);
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure Calculate; virtual;
    procedure EnumArrayFormulas(AView: TObject; AProc: TdxSpreadSheetEnumFormulasFunc);
    procedure Remove(AFormula: TdxSpreadSheetCustomFormula);
    procedure UpdateAnchorsAndBounds;

    property CalculationInProcess: Boolean read FCalculationInProcess;
    property CircularReferencePaths: TcxObjectList read FCircularReferencePaths;
    property CircularReferencePathsAsString: string read GetCircularReferencePathsAsString;
    property FormatSettings: TdxSpreadSheetFormatSettings read GetFormatSettings;
    property Items[Index: Integer]: TdxSpreadSheetCustomFormula read GetItem; default;
  end;

  { TdxSpreadSheetCustomFormula }

  TdxSpreadSheetCustomFormula = class
  strict private
    FCalculatingSubResult: TdxSpreadSheetFormulaResult;
    FErrorCode: TdxSpreadSheetFormulaErrorCode;
    FErrorIndex: Integer;
    FIsArrayFormula: Boolean;
    FSourceText: string;

    function GetActualErrorCode: TdxSpreadSheetFormulaErrorCode;
    function GetActualResult: TdxSpreadSheetFormulaResult;
    function GetActualValue: Variant;
    function GetAnchor: TPoint; inline;
    function GetAsText: string;
    function GetFormatSettings: TdxSpreadSheetFormatSettings;
    procedure SetIsArrayFormula(AValue: Boolean);
  protected
    FAnchorColumn: Integer;
    FAnchorRow: Integer;
    FArrayFormulaArea: TRect;
    FArrayFormulaSize: TSize;
    FIteration: Integer;
    FResult: Variant;
    FResultValid: Boolean;
    FResultValue: TdxSpreadSheetFormulaResult;
    FTokens: TdxSpreadSheetFormulaToken;

    procedure CacheResultValue(const AValue: Variant);
    procedure CleanFunctionTokenCachedResults; overload;
    procedure CleanFunctionTokenCachedResults(AExpression: TdxSpreadSheetFormulaToken); overload;
    procedure ClearResult; virtual;
    procedure DestroyTokens;
    procedure ForceRefresh;

    function Calculate(AExpression: TdxSpreadSheetFormulaToken): TdxSpreadSheetFormulaResult;
    function GetResultValue: TdxSpreadSheetFormulaResult; virtual;
    function GetValue: Variant; virtual;
    function IsCalculationInProcess: Boolean;
    function IsLinkedToCell: Boolean; virtual;
    procedure Offset(DY, DX: Integer);

    procedure CalculateAnchors; virtual;
    function GetController: TdxSpreadSheetCustomFormulaController; virtual;
    function GetExpressionAsText(AExpression: TdxSpreadSheetFormulaToken): TdxSpreadSheetFormulaFormattedText; virtual;
    function GetMaxIterationCount: Integer; virtual;
    function GetView: TObject; virtual;

    procedure SetArrayFormulaSize(const ASize: TSize);
    procedure SetError(AErrorCode: TdxSpreadSheetFormulaErrorCode; AErrorIndex: Integer);

    function UpdateReferences(AView: TObject; const AArea: TRect;
      const ATargetOrigin: TPoint; AMode: TdxSpreadSheetFormulaUpdateReferencesMode): Boolean; virtual;
    procedure UpdateSlaveCells;

    property ActualErrorCode: TdxSpreadSheetFormulaErrorCode read GetActualErrorCode;
    property ActualResult: TdxSpreadSheetFormulaResult read GetActualResult;
  public
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure EnumReferences(AProc: TdxSpreadSheetFormulaEnumReferencesProc; AProcessDefinedNames: Boolean = False);
    procedure Validate;
    //
    property ActualValue: Variant read GetActualValue;
    property Anchor: TPoint  read GetAnchor;
    property AnchorColumn: Integer read FAnchorColumn;
    property AnchorRow: Integer read FAnchorRow;
    property AsText: string read GetAsText;
    property ErrorCode: TdxSpreadSheetFormulaErrorCode read FErrorCode;
    property ErrorIndex: Integer read FErrorIndex;
    property Iteration: Integer read FIteration;
    property ResultValue: TdxSpreadSheetFormulaResult read GetResultValue;
    property SourceText: string read FSourceText write FSourceText;
    property Tokens: TdxSpreadSheetFormulaToken read FTokens;
    property Value: Variant read GetValue;
    // ArrayFormula
    property ArrayFormulaArea: TRect read FArrayFormulaArea;
    property ArrayFormulaSize: TSize read FArrayFormulaSize;
    property IsArrayFormula: Boolean read FIsArrayFormula write SetIsArrayFormula;
    //
    property Controller: TdxSpreadSheetCustomFormulaController read GetController;
    property FormatSettings: TdxSpreadSheetFormatSettings read GetFormatSettings;
    property View: TObject read GetView;
  end;

  { TdxSpreadSheetFormulaToken }

  TdxSpreadSheetFormulaToken = class
  strict private
    FChildFirst: TdxSpreadSheetFormulaToken;
    FChildLast: TdxSpreadSheetFormulaToken;
    FNext: TdxSpreadSheetFormulaToken;
    FParent: TdxSpreadSheetFormulaToken;
    FPrev: TdxSpreadSheetFormulaToken;

    // Flags
    FIsDestroyingByParent: Boolean;
    FIsDimensionCalculated: Boolean;
    FIsDirty: Boolean;

    function CheckCircularReference: Boolean; inline;
    function GetChildCount: Integer; inline;
    function GetController: TdxSpreadSheetCustomFormulaController; inline;
    function GetCount(AToken: TdxSpreadSheetFormulaToken): Integer; inline;
    function GetFirstSibling: TdxSpreadSheetFormulaToken; inline;
    function GetFormatSettings: TdxSpreadSheetFormatSettings; inline;
    function GetHasChildren: Boolean; inline;
    function GetItem(AIndex: Integer): TdxSpreadSheetFormulaToken; inline;
    function GetLastSibling: TdxSpreadSheetFormulaToken; inline;
    function GetSiblingCount: Integer; inline;
    procedure SetOwner(const Value: TdxSpreadSheetCustomFormula);
  protected
    FDimension: TdxSpreadSheetFormulaTokenDimension;
    FOwner: TdxSpreadSheetCustomFormula;

    procedure AddResult(AToken: TdxSpreadSheetFormulaToken);
    procedure AttachString(AToken: TdxSpreadSheetFormulaToken; const AValue: TdxSpreadSheetFormulaFormattedText); overload;
    procedure AttachString(AToken: TdxSpreadSheetFormulaToken; const AValue: string); overload;
    procedure AttachString(AToken: TdxSpreadSheetFormulaToken; const ABeforeValue: string;
      AValue: TdxSpreadSheetFormulaFormattedText; const AAfterValue: string); overload;
    function ExtractLastTokenAsString(AList: TdxSpreadSheetFormulaToken): TdxSpreadSheetFormulaFormattedText; virtual;
    function GetExpressionAsText(AExpression: TdxSpreadSheetFormulaToken): TdxSpreadSheetFormulaFormattedText;

    procedure Calculate(AResult: TdxSpreadSheetFormulaResult); virtual;
    procedure CalculateDimension(var ADimension: TdxSpreadSheetFormulaTokenDimension; var AErrorCode: TdxSpreadSheetFormulaErrorCode); virtual;
    procedure CleanFunctionTokenCachedResult; virtual;
    procedure ClearIsDimensionCalculated;

    function ExtractColumn(const AIndex: Integer; var AErrorCode: TdxSpreadSheetFormulaErrorCode): TdxSpreadSheetVector; virtual;
    function ExtractRow(const AIndex: Integer; var AErrorCode: TdxSpreadSheetFormulaErrorCode): TdxSpreadSheetVector; virtual;
    function ExtractValueAsVector(var AErrorCode: TdxSpreadSheetFormulaErrorCode): TdxSpreadSheetVector; virtual;
    function ForEach(AProc: TdxSpreadSheetForEachCallBack; const AData: Pointer; var AErrorCode: TdxSpreadSheetFormulaErrorCode): Boolean; virtual;
    function ForEachCell(AView: TObject; AStartColumn, AStartRow, AFinishColumn, AFinishRow: Integer;
      AProc: TdxSpreadSheetForEachCallBack;const AData: Pointer; var AErrorCode: TdxSpreadSheetFormulaErrorCode): Boolean;

    function GetTokenPriority: Integer; virtual;
    function GetView: TObject; virtual;
    function NeedForceDimensionCalculating: Boolean; virtual;
    procedure Offset(DY, DX: Integer); virtual;
    procedure SetLink(var AField; ALink: TdxSpreadSheet3DReferenceCustomLink);
    procedure SetNext(ANextToken: TdxSpreadSheetFormulaToken);
    procedure ToString(var AAsText: TdxSpreadSheetFormulaToken); reintroduce; virtual;
    procedure UpdateReferences(AView: TObject; const AArea: TRect; const ATargetOrigin: TPoint;
      AMode: TdxSpreadSheetFormulaUpdateReferencesMode; var AModified: Boolean); virtual;

    property Controller: TdxSpreadSheetCustomFormulaController read GetController;
    property IsDirty: Boolean read FIsDirty write FIsDirty;
  public
    destructor Destroy; override;
    function CanConvertStrToNumber: Boolean; virtual;
    procedure CheckNeighbors; virtual;
    procedure EnumReferences(AProc: TdxSpreadSheetFormulaEnumReferencesProc; AProcessDefinedNames: Boolean = False); virtual;
    function ExtractFromList: TdxSpreadSheetFormulaToken;
    function GetDimension(var AErrorCode: TdxSpreadSheetFormulaErrorCode): TdxSpreadSheetFormulaTokenDimension;
    function IsEnumeration: Boolean; virtual;

    // Values
    procedure GetCellValue(AView: TObject; const ARow, AColumn: Integer;
      var AValue: Variant; var AErrorCode: TdxSpreadSheetFormulaErrorCode); virtual;
    procedure GetCellValueAsOrdinal(AView: TObject; const ARow, AColumn: Integer;
      var AValue: Variant; var AErrorCode: TdxSpreadSheetFormulaErrorCode); inline;
    procedure GetValueAsArrayItem(const ARow, AColumn: Integer;
      var AValue: Variant; var AErrorCode: TdxSpreadSheetFormulaErrorCode); virtual;
    procedure GetValue(var AValue: Variant;
      var AErrorCode: TdxSpreadSheetFormulaErrorCode); virtual;
    function GetValueFromArray(ARowIndex, AColumnIndex: Integer;
      var AErrorCode: TdxSpreadSheetFormulaErrorCode): Variant;
    procedure GetValueRelatedWithCell(const ACell: TPoint;
      var AValue: Variant; var AErrorCode: TdxSpreadSheetFormulaErrorCode); virtual;

    // Utilities
    class procedure Add(APrev, AToken: TdxSpreadSheetFormulaToken);
    class procedure AddChild(AParent, AToken: TdxSpreadSheetFormulaToken);
    class procedure Append(APrev, AToken: TdxSpreadSheetFormulaToken); inline;
    class procedure DestroyTokens(var ATokens: TdxSpreadSheetFormulaToken);
    class procedure Replace(var ASource: TdxSpreadSheetFormulaToken; const ADest: TdxSpreadSheetFormulaToken);

    property Child: TdxSpreadSheetFormulaToken read FChildFirst;
    property ChildCount: Integer read GetChildCount;
    property Dimension: TdxSpreadSheetFormulaTokenDimension read FDimension;
    property FirstChild: TdxSpreadSheetFormulaToken read FChildFirst;
    property FirstSibling: TdxSpreadSheetFormulaToken read GetFirstSibling;
    property FormatSettings: TdxSpreadSheetFormatSettings read GetFormatSettings;
    property HasChildren: Boolean read GetHasChildren;
    property Items[Index: Integer]: TdxSpreadSheetFormulaToken read GetItem; default;
    property LastChild: TdxSpreadSheetFormulaToken read FChildLast;
    property LastSibling: TdxSpreadSheetFormulaToken read GetLastSibling;
    property Next: TdxSpreadSheetFormulaToken read FNext;
    property Owner: TdxSpreadSheetCustomFormula read FOwner write SetOwner;
    property Parent: TdxSpreadSheetFormulaToken read FParent;
    property Prev: TdxSpreadSheetFormulaToken read FPrev;
    property Priority: Integer read GetTokenPriority;
    property SiblingCount: Integer read GetSiblingCount;
    property View: TObject read GetView;
  end;

  { TdxSpreadSheetReferenceVector }

  TdxSpreadSheetReferenceVector = class(TdxSpreadSheetVector)
  strict private
    FFormulaController: TdxSpreadSheetCustomFormulaController;
    FIsRow: Boolean;
    FLength: Integer;
    FTableItemIndex: Integer;
    FViewData: IdxSpreadSheetViewData;
  protected
    function GetItem(Index: Integer): TdxSpreadSheetVectorValue; override;
    function GetLength: Integer; override;
  public
    constructor Create(AView: TObject; AFormulaController: TdxSpreadSheetCustomFormulaController;
      AIsRow: Boolean; ATableItemIndex, AAnchorItemsIndex, ALength: Integer); reintroduce; virtual;
    function GetNextItemIndex(AIndex: Integer; AForward: Boolean): Integer; override;

    property IsRow: Boolean read FIsRow;
    property FormulaController: TdxSpreadSheetCustomFormulaController read FFormulaController;
    property TableItemIndex: Integer read FTableItemIndex;
    property ViewData: IdxSpreadSheetViewData read FViewData;
  end;

  { TdxSpreadSheetFormulaFormattedText }

  TdxSpreadSheetFormulaFormattedText = class(TdxSpreadSheetFormulaToken)
  strict private
    FRuns: TdxSpreadSheetFormattedSharedStringRuns;
    FValue: string;
  public
    constructor Create(const AValue: string = '');
    destructor Destroy; override;
    procedure Add(const AValue: TdxSpreadSheetFormulaFormattedText); overload;
    procedure Add(const AValue: string); overload;
    procedure AddInBeginning(const AValue: string);
    //
    property Runs: TdxSpreadSheetFormattedSharedStringRuns read FRuns;
    property Value: string read FValue;
  end;

  { TdxSpreadSheet3DReferenceCustomLink }

  TdxSpreadSheet3DReferenceCustomLink = class
  strict private
    FLink: TcxObjectLink;

    function GetData: TObject;
    procedure SetData(AValue: TObject);
  protected
    FOwner: TdxSpreadSheetFormulaToken;
  public
    constructor Create(AData: TObject); virtual;
    destructor Destroy; override;
    function ToString: string; reintroduce; virtual;

    property Data: TObject read GetData write SetData;
    property Owner: TdxSpreadSheetFormulaToken read FOwner;
  end;

  { TdxSpreadSheet3DReferenceCustomExternalLink }

  TdxSpreadSheet3DReferenceCustomExternalLink = class(TdxSpreadSheet3DReferenceCustomLink)
  strict private
    FName: string;
  public
    constructor Create(AData: TObject; const AName: string); reintroduce; overload;
    //
    property Name: string read FName write FName;
  end;

  { TdxSpreadSheetFormulaResult }

  TdxSpreadSheetFormulaResult = class
  strict private
    FErrorCode: TdxSpreadSheetFormulaErrorCode;
    FHasCircularReferences: Boolean;
    FOwner: TdxSpreadSheetCustomFormula;
    FTemporaryTokens: TList;
    FValues: TList;

    function GetCount: Integer; inline;
    function GetFormatSettings: TdxSpreadSheetFormatSettings;
    function GetItem(AIndex: Integer): TdxSpreadSheetFormulaToken; inline;
    function GetValue: Variant;

    property TemporaryTokens: TList read FTemporaryTokens;
    property Values: TList read FValues;
  protected
    procedure ConvertNullValueToZero;
    function DoExtractDateTimeParameter(var AParameter: Variant; const AParams: TdxSpreadSheetFormulaToken; AIndex: Integer; AWithoutBoolean: Boolean): Boolean;
  public
    constructor Create(AOwner: TdxSpreadSheetCustomFormula);
    destructor Destroy; override;
    procedure BeforeDestruction; override;

    procedure Add(AValue: TdxSpreadSheetFormulaToken); inline;
    procedure AddTemporary(AValue: TdxSpreadSheetFormulaToken); inline;
    procedure AddValue(const AValue: Double); overload; inline;
    procedure AddValue(const AValue: Extended); overload; inline;
    procedure AddValue(const AValue: Integer); overload; inline;
    procedure AddValue(const AValue: string); overload; inline;
    procedure AddValue(const AValue: Variant); overload; inline;
    procedure AddResultValue(const AValue: TdxSpreadSheetFormulaResult); inline;
    procedure CheckValue;
    procedure Clear;
    procedure ClearValues;
    function ConvertToNumeric(var AValue: Variant; ACanConvertStr, AWithoutBoolean: Boolean): Boolean;
    procedure Delete(Index: Integer);
    function FirstItem: TdxSpreadSheetFormulaToken; inline;
    function LastItem: TdxSpreadSheetFormulaToken; inline;

    procedure ExtractCondition(AParams: TdxSpreadSheetFormulaToken; AMaxCount, AIndex: Integer;
      var AConditionValue: Variant; var AOperation: TdxSpreadSheetFormulaOperation);
    function ExtractColumnFromRange(const ARange: TdxSpreadSheetFormulaToken; AColumnIndex: Integer; var AErrorCode: TdxSpreadSheetFormulaErrorCode): TdxSpreadSheetVector; inline;
    function ExtractDateTimeOnlyParameter(var AParameter: Variant; const AParams: TdxSpreadSheetFormulaToken; AIndex: Integer = 0): Boolean;
    function ExtractDateTimeParameter(var AParameter: Variant; const AParams: TdxSpreadSheetFormulaToken; AIndex: Integer = 0): Boolean;
    function ExtractDateTimeParameterWithoutBoolean(var AParameter: Variant; const AParams: TdxSpreadSheetFormulaToken; AIndex: Integer = 0): Boolean;
    function ExtractErrorCode(const AParams: TdxSpreadSheetFormulaToken): TdxSpreadSheetFormulaErrorCode; overload;
    function ExtractNumericParameter(var AParameter: Variant; const AParams: TdxSpreadSheetFormulaToken; AIndex: Integer = 0): Boolean; overload;
    function ExtractNumericParameterDef(var AParameter: Variant; const ADefaultIfNoExist, ADefaultIfNull: Variant; const AParams: TdxSpreadSheetFormulaToken; AIndex: Integer = 0): Boolean; overload;
    function ExtractNumericParameterDef(var AParameter: Variant; const ADefaultIfNull: Variant; const AParams: TdxSpreadSheetFormulaToken; AIndex: Integer = 0): Boolean; overload;
    function ExtractNumericParameterDefWithoutBoolean(var AParameter: Variant; const ADefault: Variant; const AParams: TdxSpreadSheetFormulaToken; AIndex: Integer = 0): Boolean;
    function ExtractNumericParameterWithoutBoolean(var AParameter: Variant; const AParams: TdxSpreadSheetFormulaToken; AIndex: Integer = 0): Boolean;
    function ExtractNumericValue(var AValue: Variant): Boolean;
    function ExtractParameter(var AParameter: Variant; const AParams: TdxSpreadSheetFormulaToken; AIndex: Integer = 0): Boolean; overload;
    function ExtractParameter(var AParameter: Variant; out ACanConvertStrToNumber: Boolean; const AParams: TdxSpreadSheetFormulaToken; AIndex: Integer = 0): Boolean; overload;
    function ExtractParameterDef(var AParameter: Variant; ADefaultIfNoExist, ADefaultIfNull: Variant; const AParams: TdxSpreadSheetFormulaToken; AIndex: Integer = 0): Boolean; overload;
    function ExtractParameterDef(var AParameter: Variant; ADefaultIfNull: Variant; const AParams: TdxSpreadSheetFormulaToken; AIndex: Integer = 0): Boolean; overload;
    function ExtractRowFromRange(const ARange: TdxSpreadSheetFormulaToken; ARowIndex: Integer; var AErrorCode: TdxSpreadSheetFormulaErrorCode): TdxSpreadSheetVector; inline;
    function ExtractStringParameter(const AParams: TdxSpreadSheetFormulaToken; AIndex: Integer = 0): Variant; inline;
    function ExtractStringValue: Variant; inline;
    procedure ExtractToken(AToken: TdxSpreadSheetFormulaToken; out AOwnership: TStreamOwnership);
    function ExtractValue(out ACanConvertStrToNumber: Boolean): Variant; inline;
    function ExtractValueToken(out AOwnership: TStreamOwnership): TdxSpreadSheetFormulaToken;
    procedure ForEach(AParams: TdxSpreadSheetFormulaToken; AProc: TdxSpreadSheetForEachCallBack; const AData: Pointer); overload;
    function GetParamsCount(const AParams: TdxSpreadSheetFormulaToken): Integer; inline;
    function ParameterExists(const AParams: TdxSpreadSheetFormulaToken; AIndex: Integer): Boolean; inline;
    function ParameterIsNull(var AParameter: Variant; const AParams: TdxSpreadSheetFormulaToken; AIndex: Integer = 0): Boolean; overload; inline;
    function ParameterIsNull(var AParameter: Variant; out ACanConvertStrToNumber: Boolean; const AParams: TdxSpreadSheetFormulaToken; AIndex: Integer = 0): Boolean; overload; inline;
    procedure SetError(ACode: TdxSpreadSheetFormulaErrorCode); inline;
    function Validate: Boolean; inline;

    property Count: Integer read GetCount;
    property ErrorCode: TdxSpreadSheetFormulaErrorCode read FErrorCode;
    property FormatSettings: TdxSpreadSheetFormatSettings read GetFormatSettings;
    property HasCircularReferences: Boolean read FHasCircularReferences write FHasCircularReferences;
    property Items[Index: Integer]: TdxSpreadSheetFormulaToken read GetItem; default;
    property Owner: TdxSpreadSheetCustomFormula read FOwner;
    property Value: Variant read GetValue;
  end;

function CheckCellReference(ARow, AColumn: Integer; var AErrorCode: TdxSpreadSheetFormulaErrorCode): Boolean; inline;
procedure dxSpreadSheetCalculateExpression(AExpression: TdxSpreadSheetFormulaToken; var AResult: TdxSpreadSheetFormulaResult;
  const AForceConvertNullValueToZero: Boolean = False);
procedure dxSpreadSheetInitializeParamInfo(const ACount: Integer;
  var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo); inline;
function dxSpreadSheetIsReferenceToken(var AToken: TdxSpreadSheetFormulaToken; var AResult: TdxSpreadSheetFormulaResult): Boolean;
procedure dxSpreadSheetParseReference(Reference: string; var Formula);

function dxSpreadSheetEscapeString(const S: string): string;
function dxSpreadSheetTextValueAsString(const S: string): string;
function dxSpreadSheetUnescapeString(const S: string): string;
implementation

uses
  // SpreadSheet
  dxSpreadSheetCoreFormulasHelpers,
  dxSpreadSheetCoreFormulasParser,
  dxSpreadSheetCoreFormulasTokens,
  dxSpreadSheetFunctions,
  dxSpreadSheetCoreStrs;

type
  TResultAccess = class(TdxSpreadSheetFormulaResult);
  TTokenAccess = class(TdxSpreadSheetFormulaToken);

function GetDataProvider(AView: TObject): IdxSpreadSheetViewData; inline;
begin
  if not Supports(AView, IdxSpreadSheetViewData, Result) then
    raise EdxSpreadSheetError.Create('The IdxSpreadSheetViewData was not supported');
end;

function CheckCellReference(ARow, AColumn: Integer; var AErrorCode: TdxSpreadSheetFormulaErrorCode): Boolean;
begin
  if (ARow < 0) or (ARow > dxSpreadSheetMaxRowIndex) or (AColumn < 0) or (AColumn > dxSpreadSheetMaxColumnIndex) then
    AErrorCode := ecRefErr
  else
    AErrorCode := ecNone;

  Result := AErrorCode = ecNone;
end;

procedure dxSpreadSheetCalculateExpression(AExpression: TdxSpreadSheetFormulaToken; var AResult: TdxSpreadSheetFormulaResult;
  const AForceConvertNullValueToZero: Boolean = False);
begin
  if AExpression.Next = nil then
    TTokenAccess(AExpression).Calculate(AResult)
  else
    while AExpression <> nil do
    begin
      TTokenAccess(AExpression).Calculate(AResult);
      AExpression := AExpression.Next;
    end;
  if AForceConvertNullValueToZero then
    AResult.ConvertNullValueToZero;
end;

procedure dxSpreadSheetInitializeParamInfo(const ACount: Integer;
  var AParamCount: Integer; var AParamKind: TdxSpreadSheetFunctionParamKindInfo); inline;
begin
  AParamCount := ACount;
  SetLength(AParamKind, AParamCount);
end;

function dxSpreadSheetIsReferenceToken(var AToken: TdxSpreadSheetFormulaToken; var AResult: TdxSpreadSheetFormulaResult): Boolean;
begin
  if not (AToken is TdxSpreadSheetFormulaReference) then
  begin
    dxSpreadSheetCalculateExpression(AToken, AResult);
    if TResultAccess(AResult).Count > 0 then
      AToken := TResultAccess(AResult).LastItem;
  end;
  Result := AToken is TdxSpreadSheetFormulaReference;
end;

function dxSpreadSheetEscapeString(const S: string): string;
begin
  Result := StringReplace(S, dxStringMarkChar, dxStringMarkChar + dxStringMarkChar, [rfReplaceAll]);
end;

function dxSpreadSheetTextValueAsString(const S: string): string;
begin
  Result := dxStringMarkChar + dxSpreadSheetEscapeString(S) + dxStringMarkChar;
end;

function dxSpreadSheetUnescapeString(const S: string): string;
begin
  Result := StringReplace(S, dxStringMarkChar + dxStringMarkChar, dxStringMarkChar, [rfReplaceAll]);
end;

procedure dxSpreadSheetParseReference(Reference: string; var Formula);
var
  AErrorIndex: Integer;
  AFormula: TdxSpreadSheetCustomFormula absolute Formula;
  AParser: TdxSpreadSheetCustomFormulaParser;
begin
  AParser := AFormula.Controller.CreateParser as TdxSpreadSheetCustomFormulaParser;
  try
    Reference := dxSpreadSheetFormulaIncludeEqualSymbol(Trim(Reference));
    if not AParser.ParseFormula(Reference, AFormula) then
    begin
      AErrorIndex := AFormula.ErrorIndex;
      FreeAndNil(AFormula);
      raise EdxSpreadSheetError.CreateFmt(cxGetResourceString(@sdxErrorInvalidReference), [Reference, AErrorIndex]);
    end;
  finally
    AParser.Free;
  end;
end;

{ TdxSpreadSheetCustomFormulaController }

constructor TdxSpreadSheetCustomFormulaController.Create;
begin
  inherited Create(True);
  FCircularReferencePaths := TcxObjectList.Create;
  FArrayFormulas := TObjectDictionary<TObject, TdxSpreadSheetCustomFormulaList>.Create([doOwnsValues]);
end;

destructor TdxSpreadSheetCustomFormulaController.Destroy;
begin
  FreeAndNil(FCircularReferencePaths);
  FreeAndNil(FArrayFormulas);
  inherited Destroy;
end;

procedure TdxSpreadSheetCustomFormulaController.AddArrayFormula(AFormula: TdxSpreadSheetCustomFormula);
var
  AList: TdxSpreadSheetCustomFormulaList;
  AView: TObject;
begin
  AView := AFormula.View;
  if not FArrayFormulas.TryGetValue(AView, AList) then
  begin
    AList := TdxSpreadSheetCustomFormulaList.Create(1);
    FArrayFormulas.Add(AView, AList);
  end;
  AList.Add(AFormula);
end;

procedure TdxSpreadSheetCustomFormulaController.AfterConstruction;
begin
  inherited;
  dxResourceStringsRepository.AddListener(Self);
end;

procedure TdxSpreadSheetCustomFormulaController.BeforeDestruction;
begin
  inherited;
  dxResourceStringsRepository.RemoveListener(Self);
end;

procedure TdxSpreadSheetCustomFormulaController.Calculate;
var
  AFormula: TdxSpreadSheetCustomFormula;
  AIndex: Integer;
begin
  FCalculationInProcess := True;
  CircularReferencePaths.Clear;
  FCircularPath := nil;
  try
    ClearFormulasResults;
    UpdateAnchorsAndBounds;
    CircularReferencePaths.Capacity := Count;

    AIndex := 0;
    while AIndex < Count do
    begin
      AFormula := Items[AIndex];
      if not AFormula.IsArrayFormula and (IsPartOfArrayFormula(AFormula.View, AFormula.AnchorRow, AFormula.AnchorColumn) = afpSlaveCell) then
      begin
        AFormula.Free;
        Continue;
      end;

      CircularReference := False;
      try
        AFormula.GetResultValue;
      finally
        if CircularReference then
        begin
          AFormula.ClearResult;
          AFormula.CleanFunctionTokenCachedResults;
          CircularReference := False;
          FCircularPath := TdxSpreadSheetReferencePath.Create(AFormula.AnchorRow, AFormula.AnchorColumn, AFormula.View);
          try
            AFormula.GetResultValue;
          finally
            if CircularReference then
              CircularReferencePaths.Add(FCircularPath)
            else
              FreeAndNil(FCircularPath);
          end;
        end;
        Inc(AIndex);
      end;
    end;
  finally
    CleanFunctionTokenCachedResults;
    FCalculationInProcess := False;
  end;
end;

procedure TdxSpreadSheetCustomFormulaController.EnumArrayFormulas(AView: TObject; AProc: TdxSpreadSheetEnumFormulasFunc);
var
  AList: TdxSpreadSheetCustomFormulaList;
  I: Integer;
begin
  if FArrayFormulas.TryGetValue(AView, AList) then
    for I := 0 to AList.Count - 1 do
    begin
      if not AProc(AList[I]) then
        Break;
    end;
end;

procedure TdxSpreadSheetCustomFormulaController.Remove(AFormula: TdxSpreadSheetCustomFormula);
var
  AList: TdxSpreadSheetCustomFormulaList;
begin
  if AFormula.IsArrayFormula then
  begin
    if FArrayFormulas.TryGetValue(AFormula.View, AList) then
    begin
      AList.Remove(AFormula);
      if AList.Count = 0 then
        FArrayFormulas.Remove(AFormula.View);
    end;
  end;
  inherited Remove(AFormula);
end;

procedure TdxSpreadSheetCustomFormulaController.AddPath(ARow, AColumn: Integer; AView: TObject);
begin
  if CircularPath <> nil then
    CircularPath.Add(ARow, AColumn, AView);
end;

procedure TdxSpreadSheetCustomFormulaController.RemovePath(ARow, AColumn: Integer; AView: TObject);
begin
  if not CircularReference and (CircularPath <> nil) then
    CircularPath.Remove(ARow, AColumn, AView);
end;

procedure TdxSpreadSheetCustomFormulaController.CheckCircularReferences;
begin
  if HasCircularReferences and (CircularReferencePaths.Count > 0) then
    Exit;

  HasCircularReferences := CircularReferencePaths.Count > 0;
  if HasCircularReferences then
    raise EdxSpreadSheetCircularReferencesError.Create(cxGetResourceString(@sdxErrorCircularMessage));
end;

function TdxSpreadSheetCustomFormulaController.CircularPathToString(APath: TdxSpreadSheetReferencePath): string;
var
  AIntf: IdxSpreadSheetViewCaption;
  APathAsString: string;
begin
  Result := '';
  while APath <> nil do
  begin
    APathAsString := TdxSpreadSheetColumnHelper.NameByIndex(APath.Column) + IntToStr(APath.Row + 1);
    if Supports(APath.Sheet, IdxSpreadSheetViewCaption, AIntf) then
      APathAsString := AIntf.GetCaption + '!' + APathAsString;
    if Result <> '' then
      Result := Result + ' -> '
    else
      Result := cxGetResourceString(@sdxErrorCircularPathPrefix);
    Result := Result + APathAsString;
    APath := APath.Next;
  end;
end;

procedure TdxSpreadSheetCustomFormulaController.CleanFunctionTokenCachedResults;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].CleanFunctionTokenCachedResults;
end;

procedure TdxSpreadSheetCustomFormulaController.ClearFormulasResults;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].ClearResult;
end;

function TdxSpreadSheetCustomFormulaController.NeedAddFeatureFunctionPrefixToFunctionName: Boolean;
begin
  Result := False;
end;

procedure TdxSpreadSheetCustomFormulaController.InitializeStrings;
begin
  FormatSettings.UpdateSettings;
end;

procedure TdxSpreadSheetCustomFormulaController.UpdateAnchorsAndBounds;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].CalculateAnchors;
end;

function TdxSpreadSheetCustomFormulaController.GetArrayFormulaSlaveCellValue(
  AViewData: IdxSpreadSheetViewData; AArrayFormula: TdxSpreadSheetCustomFormula;
  const ARow, AColumn: Integer; var AErrorCode: TdxSpreadSheetFormulaErrorCode): Variant;
var
  AMasterResult: TdxSpreadSheetFormulaResult;
  AResultToken: TdxSpreadSheetFormulaToken;
begin
  AMasterResult := AArrayFormula.ActualResult;
  if AMasterResult.Validate then
  begin
    AResultToken := AMasterResult.LastItem;
    Result := AResultToken.GetValueFromArray(ARow - AArrayFormula.AnchorRow, AColumn - AArrayFormula.AnchorColumn, AErrorCode);
    if (AResultToken.Dimension.RowCount = 0) and (AResultToken.Dimension.ColumnCount = 0) then
      AResultToken.IsDirty := True;
  end
  else
  begin
    AErrorCode := AMasterResult.ErrorCode;
    Result := dxSpreadSheetErrorCodeToString(AErrorCode);
  end;
end;

function TdxSpreadSheetCustomFormulaController.IsPartOfArrayFormula(
  AView: TObject; const ARow, AColumn: Integer; AFormula: PdxSpreadSheetCustomFormula): TdxSpreadSheetArrayFormulaPart;
var
  AItem: TdxSpreadSheetCustomFormula;
  AList: TdxSpreadSheetCustomFormulaList;
  I: Integer;
begin
  Result := afpNone;
  if FArrayFormulas.TryGetValue(AView, AList) then
    for I := 0 to AList.Count - 1 do
    begin
      AItem := TdxSpreadSheetCustomFormula(AList.List[I]);
      if dxSpreadSheetContains(AItem.ArrayFormulaArea, ARow, AColumn) then
      begin
        if AFormula <> nil then
          AFormula^ := AItem;
        if (ARow = AItem.AnchorRow) and (AColumn = AItem.AnchorColumn) then
          Result := afpMasterCell
        else
          Result := afpSlaveCell;
        Break;
      end;
    end;
end;

procedure TdxSpreadSheetCustomFormulaController.UpdateSlaveCells(AView: TObject; AFormula: TdxSpreadSheetCustomFormula);
var
  AArrayFormulaArea: TRect;
  AErrorCode: TdxSpreadSheetFormulaErrorCode;
  ARow, AColumn: Integer;
  AValue: Variant;
  AViewData: IdxSpreadSheetViewData;
begin
  FUpdatingSlaveCells := True;
  try
    AViewData := GetDataProvider(AView);
    AArrayFormulaArea := AFormula.ArrayFormulaArea;
    for ARow := AArrayFormulaArea.Top to AArrayFormulaArea.Bottom do
      for AColumn := AArrayFormulaArea.Left to AArrayFormulaArea.Right do
        if (ARow <> AArrayFormulaArea.Top) or (AColumn <> AArrayFormulaArea.Left) then
        begin
          AValue := GetArrayFormulaSlaveCellValue(AViewData, AFormula, ARow, AColumn, AErrorCode);
          if AValue = Unassigned then
            AValue := 0;
          AViewData.SetCellData(ARow, AColumn, AValue, AErrorCode);
        end;
  finally
    FUpdatingSlaveCells := False;
  end;
end;

function TdxSpreadSheetCustomFormulaController.ExtractVector(
  AView: TObject; ARow, AColumn, ALength: Integer; AIsVertical: Boolean;
  var AErrorCode: TdxSpreadSheetFormulaErrorCode): TdxSpreadSheetVector;
var
  AValue: Variant;
  AVector: TdxSpreadSheetSimpleVector;
  AViewData: IdxSpreadSheetViewData;
  I: Integer;
begin
  if TdxSpreadSheetInvalidObject.IsLive(AView) then
    AErrorCode := ecNone
  else
    AErrorCode := ecRefErr;

  if AErrorCode = ecNone then
  begin
    AViewData := GetDataProvider(AView);
    AVector := TdxSpreadSheetSimpleVector.Create(ALength);
    for I := 0 to ALength - 1 do
    begin
      GetCellValue(AViewData, ARow, AColumn, AValue, AErrorCode);
      AVector.Add(AValue, AErrorCode);
      if AIsVertical then
        Inc(ARow)
      else
        Inc(AColumn);
    end;
    Result := AVector;
  end
  else
    Result := TdxSpreadSheetSimpleVector.Create;
end;

procedure TdxSpreadSheetCustomFormulaController.GetCellValue(
  const AViewData: IdxSpreadSheetViewData; ARow, AColumn: Integer;
  out AValue: Variant; out AErrorCode: TdxSpreadSheetFormulaErrorCode);
begin
  if CheckCellReference(ARow, AColumn, AErrorCode) then
    GetCellValueCore(AViewData, ARow, AColumn, AValue, AErrorCode)
  else
    AValue := Unassigned;
end;

procedure TdxSpreadSheetCustomFormulaController.GetCellValueCore(
  const AViewData: IdxSpreadSheetViewData; ARow, AColumn: Integer;
  out AValue: Variant; out AErrorCode: TdxSpreadSheetFormulaErrorCode);
var
  ACell: IdxSpreadSheetCellData;
  AFormula: TdxSpreadSheetCustomFormula;
begin
  AErrorCode := ecNone;
  ACell := AViewData.GetCellData(ARow, AColumn);
  if ACell = nil then
    AValue := Unassigned
  else
    case ACell.DataType of
      cdtDateTime:
        AValue := ACell.AsFloat;

      cdtError:
        begin
          AErrorCode := ACell.AsError;
          AValue := dxSpreadSheetErrorCodeToString(AErrorCode);
        end;

      cdtFormula:
        begin
          AddPath(ARow, AColumn, Self);
          try
            AFormula := TdxSpreadSheetCustomFormula(ACell.AsFormula);
            if AFormula.ActualResult.HasCircularReferences then
              CircularReference := True;
            AValue := AFormula.ActualValue;
            AErrorCode := AFormula.ActualResult.ErrorCode;
          finally
            RemovePath(ARow, AColumn, Self);
          end;
        end;

    else
      AValue := ACell.AsVariant;
    end;
end;

function TdxSpreadSheetCustomFormulaController.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := cxE_NOINTERFACE;
end;

procedure TdxSpreadSheetCustomFormulaController.FormatChanged;
begin
  InitializeStrings;
end;

procedure TdxSpreadSheetCustomFormulaController.TranslationChanged;
begin
  InitializeStrings;
  dxSpreadSheetFunctionsRepository.TranslationChanged(FormatSettings);
end;

function TdxSpreadSheetCustomFormulaController._AddRef: Integer;
begin
  Result := -1;
end;

function TdxSpreadSheetCustomFormulaController._Release: Integer;
begin
  Result := -1;
end;

function TdxSpreadSheetCustomFormulaController.GetCircularReferencePathsAsString: string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to CircularReferencePaths.Count - 1 do
  begin
    if I > 0 then
      Result := Result + dxCRLF;
    Result := Result + CircularPathToString(TdxSpreadSheetReferencePath(CircularReferencePaths[I]));
  end;
end;

function TdxSpreadSheetCustomFormulaController.GetItem(AIndex: Integer): TdxSpreadSheetCustomFormula;
begin
  Result := TdxSpreadSheetCustomFormula(inherited Items[AIndex]);
end;

{ TdxSpreadSheetCustomFormula }

destructor TdxSpreadSheetCustomFormula.Destroy;
begin
  DestroyTokens;
  inherited Destroy;
end;

procedure TdxSpreadSheetCustomFormula.AfterConstruction;
begin
  inherited;
  CalculateAnchors;
end;

procedure TdxSpreadSheetCustomFormula.BeforeDestruction;
begin
  inherited BeforeDestruction;
  if Controller <> nil then
    Controller.Remove(Self);
end;

procedure TdxSpreadSheetCustomFormula.EnumReferences(
  AProc: TdxSpreadSheetFormulaEnumReferencesProc; AProcessDefinedNames: Boolean = False);
var
  AToken: TdxSpreadSheetFormulaToken;
begin
  AToken := FTokens;
  while AToken <> nil do
  begin
    AToken.EnumReferences(AProc, AProcessDefinedNames);
    AToken := AToken.Next;
  end;
end;

procedure TdxSpreadSheetCustomFormula.Validate;
const
  MaxTextLength = 8192;
  MaxArgsCount = 254;
  MaxRangeCount = 2048;
  MaxLevel = 64;

var
  AIsCorrectFormula: Boolean;
  ARangeCount, AArgsCount, ALevel: Integer;

  procedure DoCheckTokens(AToken: TdxSpreadSheetFormulaToken; const ATokenLevel: Integer);
  var
    ACount: Integer;
  begin
    ACount := 0;
    while AToken <> nil do
    begin
      DoCheckTokens(AToken.FirstChild, ATokenLevel + 1);
      if AToken is TdxSpreadSheetFormulaReference then
        Inc(ARangeCount);
      Inc(ACount);
      AToken := AToken.Next;
    end;
    if ACount > AArgsCount then
      AArgsCount := ACount;
    if ATokenLevel > ALevel then
      ALevel := ATokenLevel;
  end;

begin
  ARangeCount := 0;
  AArgsCount := 0;
  ALevel := 0;
  if SourceText = '' then
    SourceText := AsText;
  AIsCorrectFormula := Length(SourceText) <= MaxTextLength;
  if AIsCorrectFormula then
  begin
    DoCheckTokens(Tokens, 0);
    AIsCorrectFormula := (ARangeCount <= MaxRangeCount) and (AArgsCount <= MaxArgsCount) and (ALevel <= MaxLevel);
  end;
  if not AIsCorrectFormula then
    raise EdxSpreadSheetFormulaIsTooLongError.Create(SourceText);
end;

procedure TdxSpreadSheetCustomFormula.CacheResultValue(const AValue: Variant);
begin
  FResult := AValue;
  FResultValid := True;
end;

procedure TdxSpreadSheetCustomFormula.CleanFunctionTokenCachedResults;
begin
  CleanFunctionTokenCachedResults(FTokens);
end;

procedure TdxSpreadSheetCustomFormula.CleanFunctionTokenCachedResults(AExpression: TdxSpreadSheetFormulaToken);
begin
  while AExpression <> nil do
  begin
    AExpression.CleanFunctionTokenCachedResult;
    AExpression := AExpression.Next;
  end;
end;

procedure TdxSpreadSheetCustomFormula.ClearResult;
begin
  FResultValid := False;
  FIteration := 0;
  FreeAndNil(FResultValue);
end;

procedure TdxSpreadSheetCustomFormula.DestroyTokens;
begin
  ClearResult;
  TdxSpreadSheetFormulaToken.DestroyTokens(FTokens);
end;

procedure TdxSpreadSheetCustomFormula.ForceRefresh;
begin
  ClearResult;
  FResultValue := Calculate(FTokens);
end;

function TdxSpreadSheetCustomFormula.Calculate(AExpression: TdxSpreadSheetFormulaToken): TdxSpreadSheetFormulaResult;
var
  ATemporaryResultIsNil: Boolean;
begin
  Result := TdxSpreadSheetFormulaResult.Create(Self);
  ATemporaryResultIsNil := (FCalculatingSubResult = nil) and IsArrayFormula;
  if ATemporaryResultIsNil then
    FCalculatingSubResult := Result;
  try
    if AExpression = nil then
    begin
      Result.SetError(FErrorCode);
      Exit;
    end;
    while (AExpression <> nil) and Result.Validate do
    begin
      AExpression.Calculate(Result);
      AExpression := AExpression.Next;
    end;
    Result.CheckValue;
  except
    // do nothing
  end;
  if ATemporaryResultIsNil then
    FCalculatingSubResult := nil;
end;

function TdxSpreadSheetCustomFormula.GetActualResult: TdxSpreadSheetFormulaResult;
begin
  Result := nil;
  if (FCalculatingSubResult <> nil) then
    Result := FCalculatingSubResult;
  if (Result = nil) or (Result.Count = 0) then
    Result := ResultValue;
end;

function TdxSpreadSheetCustomFormula.GetResultValue: TdxSpreadSheetFormulaResult;
var
  ATempResult: TdxSpreadSheetFormulaResult;
  APrevIteration: Integer;
begin
  if (FResultValue <> nil) or not IsCalculationInProcess then
  begin
    if FResultValue = nil then
    begin
      FResultValue := TdxSpreadSheetFormulaResult.Create(Self);
      FResultValue.AddValue(0);
    end;
  end
  else
  begin
    ATempResult := nil;
    APrevIteration := FIteration;
    Inc(FIteration);
    try
      if (Iteration > 1) and (Iteration >= GetMaxIterationCount) then
      begin
        ATempResult := TdxSpreadSheetFormulaResult.Create(Self);
        ATempResult.AddValue(0);
        Controller.CircularReference := Controller.CircularReference or (GetMaxIterationCount = 1)
      end
      else
        ATempResult := Calculate(FTokens);

      if Controller.CircularReference then
      begin
        ATempResult.Clear;
        ATempResult.HasCircularReferences := True;
        ATempResult.AddValue(0);
      end;
    finally
      FreeAndNil(FResultValue);
      FResultValue := ATempResult;
      if APrevIteration = 0 then
      begin
        if GetMaxIterationCount > 1 then
          CacheResultValue(FResultValue.Value);
        FIteration := 0;
        UpdateSlaveCells;
      end;
    end;
  end;
  Result := FResultValue;
end;

function TdxSpreadSheetCustomFormula.GetValue: Variant;
begin
  if FResultValid then
    Result := FResult
  else if not IsCalculationInProcess and (FResultValue = nil) then
    Result := 0
  else if ResultValue <> nil then
    Result := ResultValue.Value
  else
    Result := 0;

  if not IsCalculationInProcess or (GetMaxIterationCount = 1) then
    CacheResultValue(Result);
end;

function TdxSpreadSheetCustomFormula.IsCalculationInProcess: Boolean;
begin
  Result := (Controller <> nil) and Controller.CalculationInProcess;
end;

function TdxSpreadSheetCustomFormula.IsLinkedToCell: Boolean;
begin
  Result := False;
end;

procedure TdxSpreadSheetCustomFormula.Offset(DY, DX: Integer);

  procedure DoOffset(AToken: TdxSpreadSheetFormulaToken);
  begin
    while AToken <> nil do
    begin
      AToken.Offset(DY, DX);
      DoOffset(AToken.FirstChild);
      AToken := AToken.Next;
    end;
  end;

begin
  DoOffset(Tokens);
end;

procedure TdxSpreadSheetCustomFormula.CalculateAnchors;
begin
  FArrayFormulaArea := dxSpreadSheetArea(Anchor, ArrayFormulaSize);
end;

function TdxSpreadSheetCustomFormula.GetController: TdxSpreadSheetCustomFormulaController;
begin
  Result := nil;
end;

function TdxSpreadSheetCustomFormula.GetExpressionAsText(
  AExpression: TdxSpreadSheetFormulaToken): TdxSpreadSheetFormulaFormattedText;
var
  AToken: TdxSpreadSheetFormulaToken;
begin
  if AExpression = nil then
    Exit(TdxSpreadSheetFormulaFormattedText.Create);

  AToken := TdxSpreadSheetFormulaToken.Create;
  try
    while AExpression <> nil do
    begin
      AExpression.ToString(AToken);
      AExpression := AExpression.Next;
    end;
    Result := AToken.ExtractLastTokenAsString(AToken);
  finally
    AToken.Free;
  end;
end;

function TdxSpreadSheetCustomFormula.GetMaxIterationCount: Integer;
begin
  Result := 1;
end;

function TdxSpreadSheetCustomFormula.GetView: TObject;
begin
  Result := nil;
end;

procedure TdxSpreadSheetCustomFormula.SetArrayFormulaSize(const ASize: TSize);
begin
  FArrayFormulaSize := ASize;
  IsArrayFormula := True;
  CalculateAnchors;
end;

procedure TdxSpreadSheetCustomFormula.SetError(AErrorCode: TdxSpreadSheetFormulaErrorCode; AErrorIndex: Integer);
begin
  FErrorCode := AErrorCode;
  FErrorIndex := AErrorIndex;
end;

function TdxSpreadSheetCustomFormula.UpdateReferences(AView: TObject; const AArea: TRect;
  const ATargetOrigin: TPoint; AMode: TdxSpreadSheetFormulaUpdateReferencesMode): Boolean;
var
  AToken: TdxSpreadSheetFormulaToken;
begin
  Result := False;
  AToken := FTokens;
  while AToken <> nil do
  begin
    AToken.UpdateReferences(AView, AArea, ATargetOrigin, AMode, Result);
    AToken := AToken.Next;
  end;
end;

procedure TdxSpreadSheetCustomFormula.UpdateSlaveCells;
begin
  if IsArrayFormula and not cxSizeIsEmpty(ArrayFormulaSize) then
  begin
    if not Controller.CircularReference or (Controller.CircularPath <> nil) then
      Controller.UpdateSlaveCells(View, Self);
  end;
end;

function TdxSpreadSheetCustomFormula.GetActualErrorCode: TdxSpreadSheetFormulaErrorCode;
begin
  Result := FErrorCode;
  if (Result = ecNone) and (ResultValue <> nil) then
    Result := ResultValue.ErrorCode;
end;

function TdxSpreadSheetCustomFormula.GetActualValue: Variant;
begin
  Result := Value;
  if dxSpreadSheetIsNullValue(Result) then
    Result := 0;
end;

function TdxSpreadSheetCustomFormula.GetAnchor: TPoint;
begin
  Result := cxPoint(AnchorColumn, AnchorRow);
end;

function TdxSpreadSheetCustomFormula.GetAsText: string;
var
  AFormattedText: TdxSpreadSheetFormulaFormattedText;
begin
  Result := FormatSettings.Operations[opEQ];
  if ErrorIndex <> 0 then
    Result := Result + dxSpreadSheetFormulaExcludeEqualSymbol(SourceText)
  else
  begin
    AFormattedText := GetExpressionAsText(FTokens);
    if AFormattedText <> nil then
    try
      Result := Result + AFormattedText.Value;
    finally
      AFormattedText.Free;
    end;
  end;
end;

function TdxSpreadSheetCustomFormula.GetFormatSettings: TdxSpreadSheetFormatSettings;
begin
  Result := Controller.GetFormatSettings;
end;

procedure TdxSpreadSheetCustomFormula.SetIsArrayFormula(AValue: Boolean);
begin
  if AValue <> FIsArrayFormula then
  begin
    FIsArrayFormula := AValue;
    ClearResult;
  end;
end;

{ TdxSpreadSheetFormulaToken }

destructor TdxSpreadSheetFormulaToken.Destroy;
begin
  if not FIsDestroyingByParent then
    ExtractFromList;
  if FChildFirst <> nil then
    DestroyTokens(FChildFirst);
  inherited Destroy;
end;

procedure TdxSpreadSheetFormulaToken.AddResult(AToken: TdxSpreadSheetFormulaToken);
begin
  // do nothing
end;

procedure TdxSpreadSheetFormulaToken.AttachString(AToken: TdxSpreadSheetFormulaToken; const AValue: string);
begin
  AttachString(AToken, TdxSpreadSheetFormulaFormattedText.Create(AValue));
end;

procedure TdxSpreadSheetFormulaToken.AttachString(AToken: TdxSpreadSheetFormulaToken; const AValue: TdxSpreadSheetFormulaFormattedText);
begin
  TdxSpreadSheetFormulaToken.AddChild(AToken, AValue);
end;

procedure TdxSpreadSheetFormulaToken.AttachString(AToken: TdxSpreadSheetFormulaToken;
  const ABeforeValue: string; AValue: TdxSpreadSheetFormulaFormattedText; const AAfterValue: string);
begin
  AValue.AddInBeginning(ABeforeValue);
  AValue.Add(AAfterValue);
  AttachString(AToken, AValue);
end;

function TdxSpreadSheetFormulaToken.ExtractLastTokenAsString(AList: TdxSpreadSheetFormulaToken): TdxSpreadSheetFormulaFormattedText;
var
  AToken: TdxSpreadSheetFormulaToken;
begin
  AToken := AList.LastChild;
  if AToken is TdxSpreadSheetFormulaFormattedText then
    Result := TdxSpreadSheetFormulaFormattedText(AToken.ExtractFromList)
  else
    Result := TdxSpreadSheetFormulaFormattedText.Create;
end;

function TdxSpreadSheetFormulaToken.GetExpressionAsText(AExpression: TdxSpreadSheetFormulaToken): TdxSpreadSheetFormulaFormattedText;
begin
  Result := Owner.GetExpressionAsText(AExpression);
end;

procedure TdxSpreadSheetFormulaToken.Calculate(AResult: TdxSpreadSheetFormulaResult);
begin
  AResult.Add(Self);
end;

function TdxSpreadSheetFormulaToken.CheckCircularReference: Boolean;
begin
  Result := (Owner <> nil) and (Controller <> nil) and Controller.CircularReference;
end;

procedure TdxSpreadSheetFormulaToken.CheckNeighbors;
begin
end;

procedure TdxSpreadSheetFormulaToken.EnumReferences(
  AProc: TdxSpreadSheetFormulaEnumReferencesProc; AProcessDefinedNames: Boolean = False);
var
  AToken: TdxSpreadSheetFormulaToken;
begin
  AToken := FChildFirst;
  while AToken <> nil do
  begin
    AToken.EnumReferences(AProc, AProcessDefinedNames);
    AToken := AToken.Next;
  end;
end;

function TdxSpreadSheetFormulaToken.ExtractFromList: TdxSpreadSheetFormulaToken;
begin
  Result := Self;
  if FPrev <> nil then
    FPrev.FNext := FNext;
  if FNext <> nil then
    FNext.FPrev := FPrev;
  if FParent <> nil then
  begin
    if FPrev = nil then
      FParent.FChildFirst := FNext;
    if FNext = nil then
      FParent.FChildLast := FPrev;
  end;
  FParent := nil;
  FPrev := nil;
  FNext := nil;
end;

function TdxSpreadSheetFormulaToken.ExtractColumn(const AIndex: Integer; var AErrorCode: TdxSpreadSheetFormulaErrorCode): TdxSpreadSheetVector;
begin
  Result := ExtractValueAsVector(AErrorCode);
end;

function TdxSpreadSheetFormulaToken.ExtractRow(const AIndex: Integer; var AErrorCode: TdxSpreadSheetFormulaErrorCode): TdxSpreadSheetVector;
begin
  Result := ExtractValueAsVector(AErrorCode);
end;

function TdxSpreadSheetFormulaToken.ExtractValueAsVector(var AErrorCode: TdxSpreadSheetFormulaErrorCode): TdxSpreadSheetVector;
var
  AValue: Variant;
  AValueErrorCode: TdxSpreadSheetFormulaErrorCode;
  AVector: TdxSpreadSheetSimpleVector;
begin
  AErrorCode := ecNone;
  GetValue(AValue, AValueErrorCode);
  AVector := TdxSpreadSheetSimpleVector.Create(1);
  AVector.Add(AValue, AValueErrorCode);
  Result := AVector
end;

function TdxSpreadSheetFormulaToken.ForEach(AProc: TdxSpreadSheetForEachCallBack;
  const AData: Pointer; var AErrorCode: TdxSpreadSheetFormulaErrorCode): Boolean;
var
  AValue: Variant;
begin
  GetValue(AValue, AErrorCode);
  Result := AProc(AValue, CanConvertStrToNumber, AErrorCode, AData, nil) and (AErrorCode = ecNone);
end;

function TdxSpreadSheetFormulaToken.ForEachCell(AView: TObject;
  AStartColumn, AStartRow, AFinishColumn, AFinishRow: Integer; AProc: TdxSpreadSheetForEachCallBack;
  const AData: Pointer; var AErrorCode: TdxSpreadSheetFormulaErrorCode): Boolean;

  function ApplyConditionsForEmptyCells(ABeginRow, AEndRow: Integer; var ACellReference: TdxSpreadSheetCellReference): Boolean;
  var
    AResultData: TdxSpreadSheetEnumValuesWithCondition;
    ACountValues: Integer;
    AResultValue: Variant;
  begin
    AResultData := TObject(AData) as TdxSpreadSheetEnumValuesWithCondition;
    ACellReference.RowIndex := ABeginRow;
    ACountValues := AResultData.CountValues;
    AResultValue := AResultData.ResultValue;
    Result := AProc(Unassigned, CanConvertStrToNumber, AErrorCode, AData, @ACellReference);
    if Result and (AErrorCode = ecNone) then
    begin
      AResultData.CountValues := AResultData.CountValues + (AResultData.CountValues - ACountValues) * (AEndRow - ABeginRow);
      AResultData.ResultValue := AResultData.ResultValue + (AResultData.ResultValue - AResultValue) * (AEndRow - ABeginRow);
    end;
end;

  function CheckRange: Boolean;
  begin
    Result :=
      InRange(AStartRow, 0, dxSpreadSheetMaxRowIndex) and InRange(AFinishRow, 0, dxSpreadSheetMaxRowIndex) and
      InRange(AStartColumn, 0, dxSpreadSheetMaxColumnIndex) and InRange(AFinishColumn, 0, dxSpreadSheetMaxColumnIndex);
  end;

var
  ACellReference: TdxSpreadSheetCellReference;
  AColumnIndex: Integer;
  ADataProvider: IdxSpreadSheetViewData;
  AOldRowIndex: Integer;
  ARowIndex: Integer;
  AValue: Variant;
begin
  Result := True;
  ACellReference.View := View;
  if not CheckRange then
  begin
    AErrorCode := ecRefErr;
    Exit;
  end;
  if not Supports(View, IdxSpreadSheetViewData, ADataProvider) then
  begin
    AErrorCode := ecNA;
    Exit;
  end;

  AFinishColumn := Min(ADataProvider.GetMaxColumnIndex, AFinishColumn);
  AFinishRow := Min(ADataProvider.GetMaxRowIndex, AFinishRow);

  for AColumnIndex := AStartColumn to AFinishColumn do
  begin
    ACellReference.ColumnIndex := AColumnIndex;
    ARowIndex := AStartRow;
    while (ARowIndex <= AFinishRow) and not CheckCircularReference do
    begin
      ACellReference.RowIndex := ARowIndex;
      GetCellValueAsOrdinal(AView, ARowIndex, AColumnIndex, AValue, AErrorCode);
      if CheckCircularReference then
        Break;
      Result := AProc(AValue, CanConvertStrToNumber, AErrorCode, AData, @ACellReference);
      if Result and (AErrorCode = ecNone) then
        if ARowIndex < AFinishRow then
        begin
          AOldRowIndex := ARowIndex;
          ARowIndex := ADataProvider.GetNextRowWithNonEmptyCell(ARowIndex, AColumnIndex, True);
          ARowIndex := Min(ARowIndex, AFinishRow);
          if ARowIndex - AOldRowIndex > 1 then
            if TObject(AData) is TdxSpreadSheetEnumValuesWithCondition then
              Result := ApplyConditionsForEmptyCells(AOldRowIndex + 1, Min(ARowIndex - 1, AFinishRow), ACellReference)
            else
              if TObject(AData) is TdxSpreadSheetEnumValues then
                TdxSpreadSheetEnumValues(AData).IncrementPassedCellCount(Min(ARowIndex - 1, AFinishRow) - AOldRowIndex);
        end
        else
          Inc(ARowIndex);

      if not Result or (AErrorCode <> ecNone) then
        Break;
    end;
    if not Result or (AErrorCode <> ecNone) or CheckCircularReference then
      Break;
  end
end;

procedure TdxSpreadSheetFormulaToken.CleanFunctionTokenCachedResult;
var
  AToken: TdxSpreadSheetFormulaToken;
begin
  AToken := FChildFirst;
  while AToken <> nil do
  begin
    AToken.CleanFunctionTokenCachedResult;
    AToken := AToken.Next;
  end;
end;

procedure TdxSpreadSheetFormulaToken.GetCellValue(AView: TObject;
  const ARow, AColumn: Integer; var AValue: Variant; var AErrorCode: TdxSpreadSheetFormulaErrorCode);
var
  AArrayFormula: TdxSpreadSheetCustomFormula;
  AViewData: IdxSpreadSheetViewData;
begin
  if CheckCellReference(ARow, AColumn, AErrorCode) then
  begin
    if TdxSpreadSheetInvalidObject.IsLive(AView) then
    begin
      AViewData := GetDataProvider(AView);
      if Controller.IsPartOfArrayFormula(AView, ARow, AColumn, @AArrayFormula) <> afpNone then
        AValue := Controller.GetArrayFormulaSlaveCellValue(AViewData, AArrayFormula, ARow, AColumn, AErrorCode)
      else
        Controller.GetCellValueCore(AViewData, ARow, AColumn, AValue, AErrorCode);
    end
    else
      AErrorCode := ecRefErr;
  end;
end;

procedure TdxSpreadSheetFormulaToken.GetCellValueAsOrdinal(AView: TObject;
  const ARow, AColumn: Integer; var AValue: Variant; var AErrorCode: TdxSpreadSheetFormulaErrorCode);
var
  ACellValue: Variant;
  ACell: IdxSpreadSheetCellData;
begin
  GetCellValue(AView, ARow, AColumn, ACellValue, AErrorCode);
  AValue := ACellValue;
  ACell := GetDataProvider(AView).GetCellData(ARow, AColumn);
  if VarIsStr(ACellValue) and not((ACell <> nil) and (ACell.DataType = cdtString)) and
     not dxTryStrToOrdinal(ACellValue, AValue, Owner.FormatSettings) then
    AValue := ACellValue;
end;

procedure TdxSpreadSheetFormulaToken.GetValueAsArrayItem(
  const ARow, AColumn: Integer; var AValue: Variant; var AErrorCode: TdxSpreadSheetFormulaErrorCode);
begin
  GetValue(AValue, AErrorCode);
end;

function TdxSpreadSheetFormulaToken.CanConvertStrToNumber: Boolean;
begin
  Result := True;
end;

function TdxSpreadSheetFormulaToken.GetTokenPriority: Integer;
begin
  Result := -1;
end;

function TdxSpreadSheetFormulaToken.GetView: TObject;
begin
  Result := Owner.View;
end;

procedure TdxSpreadSheetFormulaToken.Offset(DY, DX: Integer);
begin
end;

procedure TdxSpreadSheetFormulaToken.CalculateDimension(
  var ADimension: TdxSpreadSheetFormulaTokenDimension; var AErrorCode: TdxSpreadSheetFormulaErrorCode);
begin
  ADimension.SetDimension(1, 1);
  AErrorCode := ecNone;
end;

procedure TdxSpreadSheetFormulaToken.ClearIsDimensionCalculated;
begin
  FIsDimensionCalculated := False;
end;

function TdxSpreadSheetFormulaToken.GetDimension(var AErrorCode: TdxSpreadSheetFormulaErrorCode): TdxSpreadSheetFormulaTokenDimension;
begin
  if not FIsDimensionCalculated or NeedForceDimensionCalculating then
    CalculateDimension(FDimension, AErrorCode)
  else
    AErrorCode := ecNone;

  Result := FDimension;
  FIsDimensionCalculated := True;
  if (AErrorCode = ecNone) and (FDimension.RowCount = 0) then
    AErrorCode := ecValue;
end;

procedure TdxSpreadSheetFormulaToken.GetValue(var AValue: Variant; var AErrorCode: TdxSpreadSheetFormulaErrorCode);
var
  AItemResult: TdxSpreadSheetFormulaResult;
begin
  if Owner = nil then
  begin
    AErrorCode := ecValue;
    Exit;
  end;

  AItemResult := Owner.Calculate(FirstChild);
  try
    AValue := AItemResult.Value;
    AErrorCode := AItemResult.ErrorCode;
  finally
    AItemResult.Free;
  end;
end;

function TdxSpreadSheetFormulaToken.GetValueFromArray(ARowIndex, AColumnIndex: Integer; var AErrorCode: TdxSpreadSheetFormulaErrorCode): Variant;

  procedure CheckIndex(var AIndex: Integer; ACount: Integer);
  begin
    if (AIndex > ACount - 1) then
      if ACount = 1 then
        AIndex := 0
      else
        AErrorCode := ecNA;
  end;

var
  ADimension: TdxSpreadSheetFormulaTokenDimension;
begin
  Result := Null;
  if Self = nil then
    Exit;
  ADimension := GetDimension(AErrorCode);
  CheckIndex(ARowIndex, ADimension.RowCount);
  if AErrorCode <> ecNA then
    CheckIndex(AColumnIndex, ADimension.ColumnCount);
  if AErrorCode <> ecNA then
    GetValueAsArrayItem(ARowIndex, AColumnIndex, Result, AErrorCode);
end;

procedure TdxSpreadSheetFormulaToken.GetValueRelatedWithCell(
  const ACell: TPoint; var AValue: Variant; var AErrorCode: TdxSpreadSheetFormulaErrorCode);
begin
  GetValue(AValue, AErrorCode);
end;

function TdxSpreadSheetFormulaToken.IsEnumeration: Boolean;
begin
  Result := False;
end;

function TdxSpreadSheetFormulaToken.NeedForceDimensionCalculating: Boolean;
begin
  Result := False;
end;

class procedure TdxSpreadSheetFormulaToken.Add(APrev, AToken: TdxSpreadSheetFormulaToken);
begin
  if APrev.FNext <> nil then
  begin
  {$IFDEF DEBUG}
    dxTestCheck(AToken.Next = nil, 'TdxSpreadSheetFormulaToken.Add');
  {$ENDIF}
    AToken.FNext := APrev.FNext;
    AToken.FNext.FPrev := AToken;
    AToken.FParent := APrev.Parent;
    AToken.FPrev := APrev;
    APrev.FNext := AToken;
  end
  else
    if APrev.Parent <> nil then
      AddChild(APrev.Parent, AToken)
    else
    begin
      APrev.FNext := AToken;
      AToken.FPrev := APrev;
      if AToken.Parent <> nil then
        while AToken <> nil do
        begin
          AToken.FParent := nil;
          AToken := AToken.Next;
        end;
    end;
end;

class procedure TdxSpreadSheetFormulaToken.AddChild(AParent, AToken: TdxSpreadSheetFormulaToken);
var
  ALast: TdxSpreadSheetFormulaToken;
begin
  if AParent.FChildFirst = nil then
    AParent.FChildFirst := AToken
  else
  begin
    ALast := AParent.FChildLast;
    ALast.FNext := AToken;
    AToken.FPrev := ALast;
  end;

  while AToken <> nil do
  begin
    AParent.FChildLast := AToken;
    AToken.FParent := AParent;
    AToken := AToken.Next;
  end;
end;

class procedure TdxSpreadSheetFormulaToken.Append(APrev, AToken: TdxSpreadSheetFormulaToken);
begin
  Add(APrev.LastSibling, AToken);
end;

class procedure TdxSpreadSheetFormulaToken.DestroyTokens(var ATokens: TdxSpreadSheetFormulaToken);
var
  ANextToken: TdxSpreadSheetFormulaToken;
  AParentToken: TdxSpreadSheetFormulaToken;
  AToken: TdxSpreadSheetFormulaToken;
begin
  if ATokens = nil then Exit;

  AToken := ATokens;
  AParentToken := AToken.Parent;
  while AToken <> nil do
  begin
    ANextToken := AToken.Next;
    AToken.FIsDestroyingByParent := True;
    AToken.Free;
    AToken := ANextToken;
  end;
  if AParentToken <> nil then
  begin
    AParentToken.FChildFirst := nil;
    AParentToken.FChildLast := nil;
  end;
  ATokens := nil;
end;

class procedure TdxSpreadSheetFormulaToken.Replace(
  var ASource: TdxSpreadSheetFormulaToken; const ADest: TdxSpreadSheetFormulaToken);
begin
  ADest.FParent := ASource.Parent;
  if ASource.Parent <> nil then
  begin
    if ASource.Parent.FChildFirst = ASource then
      ASource.Parent.FChildFirst := ADest;
    if ASource.Parent.FChildLast = ASource then
      ASource.Parent.FChildLast := ADest;
  end;

  ADest.Owner := ASource.Owner;
  ADest.FNext := ASource.FNext;
  ADest.FPrev := ASource.FPrev;
  if ASource.Next <> nil then
    ASource.Next.FPrev := ADest;
  if ASource.Prev <> nil then
    ASource.Prev.FNext := ADest;

  ASource.FParent := nil;
  ASource.FNext := nil;
  ASource.FPrev := nil;

  TdxSpreadSheetFormulaToken.DestroyTokens(ASource);
  ASource := ADest;
end;

procedure TdxSpreadSheetFormulaToken.SetLink(var AField; ALink: TdxSpreadSheet3DReferenceCustomLink);
begin
  TdxSpreadSheet3DReferenceCustomLink(AField) := ALink;
  if ALink <> nil then
    ALink.FOwner := Self;
end;

procedure TdxSpreadSheetFormulaToken.SetNext(ANextToken: TdxSpreadSheetFormulaToken);
begin
  FNext := ANextToken;
end;

procedure TdxSpreadSheetFormulaToken.SetOwner(const Value: TdxSpreadSheetCustomFormula);
var
  AToken: TdxSpreadSheetFormulaToken;
begin
  AToken := Self;
  while AToken <> nil do
  begin
    AToken.FOwner := Value;
    AToken.Child.Owner := Value;
    AToken := AToken.Next;
  end;
end;

procedure TdxSpreadSheetFormulaToken.ToString(var AAsText: TdxSpreadSheetFormulaToken);
begin
  if FChildFirst <> nil then
    AttachString(AAsText, Owner.GetExpressionAsText(FChildFirst))
  else
    AttachString(AAsText, '');
end;

procedure TdxSpreadSheetFormulaToken.UpdateReferences(AView: TObject; const AArea: TRect;
  const ATargetOrigin: TPoint; AMode: TdxSpreadSheetFormulaUpdateReferencesMode; var AModified: Boolean);
var
  AChild: TdxSpreadSheetFormulaToken;
begin
  AChild := FirstChild;
  while AChild <> nil do
  begin
    AChild.UpdateReferences(AView, AArea, ATargetOrigin, AMode, AModified);
    AChild := AChild.Next;
  end;
end;

function TdxSpreadSheetFormulaToken.GetChildCount: Integer;
begin
  Result := GetCount(FChildFirst);
end;

function TdxSpreadSheetFormulaToken.GetController: TdxSpreadSheetCustomFormulaController;
begin
  Result := Owner.Controller;
end;

function TdxSpreadSheetFormulaToken.GetCount(AToken: TdxSpreadSheetFormulaToken): Integer;
begin
  Result := 0;
  while AToken <> nil do
  begin
    Inc(Result);
    AToken := AToken.Next;
  end;
end;

function TdxSpreadSheetFormulaToken.GetFirstSibling: TdxSpreadSheetFormulaToken;
begin
  Result := Self;
  while Result.Prev <> nil do
    Result := Result.Prev;
end;

function TdxSpreadSheetFormulaToken.GetFormatSettings: TdxSpreadSheetFormatSettings;
begin
  Result := Owner.FormatSettings;
end;

function TdxSpreadSheetFormulaToken.GetItem(AIndex: Integer): TdxSpreadSheetFormulaToken;
begin
  if AIndex < 0 then
    Result := nil
  else
  begin
    Result := FChildFirst;
    while (AIndex > 0) and (Result <> nil) do
    begin
      Result := Result.Next;
      Dec(AIndex);
    end;
  end;
end;

function TdxSpreadSheetFormulaToken.GetHasChildren: Boolean;
begin
  Result := FChildFirst <> nil;
end;

function TdxSpreadSheetFormulaToken.GetLastSibling: TdxSpreadSheetFormulaToken;
begin
  if Self = nil then
    Exit(nil);

  if Parent <> nil then
    Result := Parent.LastChild
  else
  begin
    Result := Self;
    while Result.FNext <> nil do
      Result := Result.FNext;
  end;
end;

function TdxSpreadSheetFormulaToken.GetSiblingCount: Integer;
begin
  Result := GetCount(FirstSibling);
end;

{ TdxSpreadSheetReferenceVector }

constructor TdxSpreadSheetReferenceVector.Create(
  AView: TObject; AFormulaController: TdxSpreadSheetCustomFormulaController;
  AIsRow: Boolean; ATableItemIndex, AAnchorItemsIndex, ALength: Integer);
begin
  inherited Create;
  FIsRow := AIsRow;
  FLength := ALength;
  FTableItemIndex := ATableItemIndex;
  FAnchorItemsIndex := AAnchorItemsIndex;
  FFormulaController := AFormulaController;

  if IsRow then
    FIsAllItems := (FAnchorItemsIndex = 0) and (Length = dxSpreadSheetMaxColumnCount)
  else
    FIsAllItems := (FAnchorItemsIndex = 0) and (Length = dxSpreadSheetMaxRowCount);

  if TdxSpreadSheetInvalidObject.IsLive(AView) then
    FViewData := GetDataProvider(AView)
  else
    FViewData := nil;
end;

function TdxSpreadSheetReferenceVector.GetLength: Integer;
begin
  Result := FLength;
end;

function TdxSpreadSheetReferenceVector.GetItem(Index: Integer): TdxSpreadSheetVectorValue;
var
  AErrorCode: TdxSpreadSheetFormulaErrorCode;
  AValue: Variant;
begin
  AErrorCode := ecNone;
  if ViewData = nil then
  begin
    AErrorCode := ecRefErr;
    AValue := dxSpreadSheetErrorCodeToString(AErrorCode);
  end;

  if AErrorCode = ecNone then
  begin
    if IsRow then
      FormulaController.GetCellValue(ViewData, TableItemIndex, AnchorItemsIndex + Index, AValue, AErrorCode)
    else
      FormulaController.GetCellValue(ViewData, AnchorItemsIndex + Index, TableItemIndex, AValue, AErrorCode);
  end;

  Result := TdxSpreadSheetVectorValue.Create(AValue, AErrorCode);
end;

function TdxSpreadSheetReferenceVector.GetNextItemIndex(AIndex: Integer; AForward: Boolean): Integer;
begin
  if ViewData = nil then
    Exit(-1);

  if IsRow then
    Result := ViewData.GetNextColumnWithNonEmptyCell(TableItemIndex, AnchorItemsIndex + AIndex, AForward) - AnchorItemsIndex
  else
    Result := ViewData.GetNextRowWithNonEmptyCell(AnchorItemsIndex + AIndex, TableItemIndex, AForward) - AnchorItemsIndex;

  if Result = AIndex then
    Result := IfThen(AForward, Result + 1, Result - 1);
end;

{ TdxSpreadSheetFormulaFormattedText }

constructor TdxSpreadSheetFormulaFormattedText.Create(const AValue: string = '');
begin
  inherited Create;
  FValue := AValue;
  FRuns := TdxSpreadSheetFormattedSharedStringRuns.Create;
end;

destructor TdxSpreadSheetFormulaFormattedText.Destroy;
begin
  FreeAndNil(FRuns);
  inherited Destroy;
end;

procedure TdxSpreadSheetFormulaFormattedText.Add(const AValue: TdxSpreadSheetFormulaFormattedText);
begin
  try
    AValue.Runs.Offset(Length(Value));
    Runs.Append(AValue.Runs);
    FValue := FValue + AValue.Value;
  finally
    AValue.Free;
  end;
end;

procedure TdxSpreadSheetFormulaFormattedText.Add(const AValue: string);
begin
  FValue := Value + AValue;
end;

procedure TdxSpreadSheetFormulaFormattedText.AddInBeginning(const AValue: string);
begin
  Runs.Offset(Length(AValue));
  FValue := AValue + Value;
end;

{ TdxSpreadSheet3DReferenceCustomLink }

constructor TdxSpreadSheet3DReferenceCustomLink.Create(AData: TObject);
begin
  inherited Create;
  Data := AData;
end;

destructor TdxSpreadSheet3DReferenceCustomLink.Destroy;
begin
  Data := nil;
  inherited Destroy;
end;

function TdxSpreadSheet3DReferenceCustomLink.ToString: string;
begin
  Result := '';
end;

function TdxSpreadSheet3DReferenceCustomLink.GetData: TObject;
begin
  if (FLink <> nil) and (FLink.Ref <> nil) then
    Result := FLink.Ref
  else
    if FLink <> nil then
      TdxSpreadSheetInvalidObject.AssignTo(Result)
    else
      Result := nil;
end;

procedure TdxSpreadSheet3DReferenceCustomLink.SetData(AValue: TObject);
begin
  cxRemoveObjectLink(FLink);
  if AValue <> nil then
    FLink := cxAddObjectLink(AValue)
  else
    FLink := nil;
end;

{ TdxSpreadSheet3DReferenceCustomExternalLink }

constructor TdxSpreadSheet3DReferenceCustomExternalLink.Create(AData: TObject; const AName: string);
begin
  inherited Create(AData);
  FName := AName;
end;

{ TdxSpreadSheetFormulaResult }

constructor TdxSpreadSheetFormulaResult.Create(AOwner: TdxSpreadSheetCustomFormula);
begin
  inherited Create;
  FOwner := AOwner;
  FValues := TList.Create;
  FValues.Capacity := 128;
  FTemporaryTokens := TList.Create;
  FTemporaryTokens.Capacity := 16;
end;

destructor TdxSpreadSheetFormulaResult.Destroy;
begin
  Clear;
  FreeAndNil(FTemporaryTokens);
  FreeAndNil(FValues);
  inherited Destroy;
end;

procedure TdxSpreadSheetFormulaResult.BeforeDestruction;
begin
  inherited BeforeDestruction;
  if Owner <> nil then
  begin
    if Owner.FResultValue = Self then
      Owner.FResultValue := nil;
  end;
end;

procedure TdxSpreadSheetFormulaResult.Add(AValue: TdxSpreadSheetFormulaToken);
begin
  FValues.Add(AValue);
end;

procedure TdxSpreadSheetFormulaResult.AddTemporary(AValue: TdxSpreadSheetFormulaToken);
begin
{$IFDEF DEBUG}
  if AValue.Owner <> nil then
    raise EInvalidOperation.Create(ClassName);
{$ENDIF}
  if Owner <> nil then
    AValue.Owner := Owner;
  FTemporaryTokens.Add(AValue);
  Add(AValue);
end;

procedure TdxSpreadSheetFormulaResult.AddValue(const AValue: Double);
begin
  AddTemporary(TdxSpreadSheetFormulaFloatValueToken.Create(AValue));
end;

procedure TdxSpreadSheetFormulaResult.AddValue(const AValue: Extended);
begin
  AddValue(Variant(AValue));
end;

procedure TdxSpreadSheetFormulaResult.AddValue(const AValue: Integer);
begin
  AddTemporary(TdxSpreadSheetFormulaIntegerValueToken.Create(AValue));
end;

procedure TdxSpreadSheetFormulaResult.AddValue(const AValue: string);
begin
  AddTemporary(TdxSpreadSheetFormulaStringValueToken.Create(AValue));
end;

procedure TdxSpreadSheetFormulaResult.AddValue(const AValue: Variant);
begin
  AddTemporary(TdxSpreadSheetFormulaVariantToken.Create(AValue));
end;

procedure TdxSpreadSheetFormulaResult.AddResultValue(const AValue: TdxSpreadSheetFormulaResult);
begin
  if AValue = nil then
    SetError(ecValue)
  else
    try
      if AValue.Validate then
      begin
        dxAppendList(AValue.TemporaryTokens, TemporaryTokens);
        dxAppendList(AValue.Values, Values);
        AValue.TemporaryTokens.Clear;
        AValue.Values.Clear;
      end
      else
        SetError(AValue.ErrorCode)
    finally
      AValue.Free;
    end;
end;

procedure TdxSpreadSheetFormulaResult.CheckValue;
begin
  if (Values.Count = 0) or not TdxSpreadSheetFormulaToken(Values.Last).IsEnumeration then
    GetValue;
end;

procedure TdxSpreadSheetFormulaResult.Clear;
begin
  FErrorCode := ecNone;
  HasCircularReferences := False;
  ClearValues;
end;

procedure TdxSpreadSheetFormulaResult.ClearValues;
var
  I: Integer;
begin
  try
    for I := 0 to FTemporaryTokens.Count - 1 do
      TObject(FTemporaryTokens[I]).Free;
  finally
    FTemporaryTokens.Clear;
    Values.Clear;
  end;
end;

function dxTryConvertFormattedStringToFloat(const S: string; out AFloatValue: Double; out AIsCurrency: Boolean;
  const AFormatSettings: TdxSpreadSheetFormatSettings): Boolean;
var
  st: string;
  P, AThousand, ADecimal, I: Integer;
  AIsNegative: Boolean;
begin
  Result := True;
  st := Trim(S);
  if st = '' then
    Exit(False);

  P := Pos('-', st);
  AIsNegative := (P > 0) or (st[1] = '(') and (st[Length(st)] = ')');
  if AIsNegative then
    if P > 0 then
      Delete(st, P, 1)
    else
      st := Copy(st, 2, Length(st) - 2);

  AIsCurrency := False;
  P := Pos(AFormatSettings.CurrencyFormat, st);
  if P > 0 then
  begin
    Result := (P = 1) or (P = Length(st) - Length(AFormatSettings.CurrencyFormat) + 1);
    if Result then
    begin
      Delete(st, P, Length(AFormatSettings.CurrencyFormat));
      st := Trim(st);
      Result := (Pos(AFormatSettings.CurrencyFormat, st) = 0) and (Length(st) > 0);
    end;
  end;
  if not Result then
    Exit;

  AThousand := 0;
  ADecimal := 0;
  I := 1;
  repeat
    if st[I] = AFormatSettings.DecimalSeparator then
    begin
      ADecimal := I;
      Break;
    end;
    if st[I] = AFormatSettings.Data.ThousandSeparator then
    begin
      Result := (I > 1) and ((AThousand = 0) or (I - AThousand = 3));
      AThousand := I;
      Delete(st, I, 1);
    end
    else
      Inc(I);
  until not Result or (I > Length(st));

  if Result then
  begin
    Result := AThousand = 0;
    if not Result then
      if ADecimal > 0 then
        Result := ADecimal - AThousand = 3
      else
        Result := Length(st) - AThousand = 2;
    Result := Result and TryStrToFloat(st, AFloatValue);
  end;
  if Result and AIsNegative then
    AFloatValue := -AFloatValue;
end;

function dxTryCreateDateOnShort(ANumber1, ANumber2: Word; var AValue: TDateTime; const ASettings: TFormatSettings): Boolean; inline;
var
  ADay, AMonth: Word;
begin
  Result := False;
  if (ANumber1 < 1) or (ANumber2 < 1) then
    Exit;
  Result := True;
  if InRange(ANumber1, 1, 12) and (ANumber2 > 12) then
  begin
    if InRange(ANumber2, 1, 29) then
      AValue := EncodeDate(2000 + ANumber2, ANumber1, 1)
    else
      if InRange(ANumber2, 30, 99) then
        AValue := EncodeDate(1900 + ANumber2, ANumber1, 1)
      else
        if ANumber2 > 1899 then
          AValue := EncodeDate(ANumber2, ANumber1, 1)
        else
          Result := False;
  end
  else
  try
    if Pos('D', UpperCase(ASettings.ShortDateFormat)) < Pos('M', UpperCase(ASettings.ShortDateFormat)) then
    begin
      ADay := ANumber1;
      AMonth := ANumber2;
    end
    else
    begin
      ADay := ANumber2;
      AMonth := ANumber1;
    end;
    AValue := EncodeDate(CurrentYear, AMonth, ADay);
  except
    Result := False;
  end;
end;

function dxIsShortDate(const AText: string; var AValue: TDateTime; const ASettings: TFormatSettings): Boolean; inline;
var
  APos, ANumber1, ANumber2: Integer;
  S1, S2: string;
begin
  Result := False;
  APos := Pos(ASettings.DateSeparator, AText);
  if APos = 0 then
  case ASettings.DateSeparator of
    '-': APos := Pos('/', AText);
    '/': APos := Pos('-', AText);
    '.': begin
           APos := Pos('/', AText);
           if APos = 0 then
             APos := Pos('-', AText);
         end;
  end;
  if APos > 0 then
  begin
    S1 := Copy(AText, 1, APos - 1);
    S2 := Trim(Copy(AText, APos + 1, Length(AText) - APos));
    if TryStrToInt(S1, ANumber1) and TryStrToInt(S2, ANumber2) then
      Result := dxTryCreateDateOnShort(ANumber1, ANumber2, AValue, ASettings);
  end;
end;

function TdxSpreadSheetFormulaResult.ConvertToNumeric(var AValue: Variant; ACanConvertStr, AWithoutBoolean: Boolean): Boolean;
var
  ANumeric: Double;
  ADate: TDateTime;
  AIsCurrency: Boolean;
begin
  if VarIsStr(AValue) and not ACanConvertStr then
    SetError(ecValue)
  else
    if dxSpreadSheetIsNullValue(AValue) then
      AValue := 0
    else
      if VarType(AValue) = varBoolean then
      begin
        if AWithoutBoolean then
          SetError(ecValue)
        else
          AValue := Integer(AValue = True)
      end
      else
      begin
        if VarIsStr(AValue) and
          (TryStrToFloat(AValue, ANumeric) or dxTryConvertFormattedStringToFloat(AValue, ANumeric, AIsCurrency, FormatSettings)) then
          AValue := ANumeric
        else
        begin
          if not VarIsNumeric(AValue) and not VarIsDate(AValue) then
          begin
            if dxTryStrToDateTime(AValue, ADate, FormatSettings.Data) or dxIsShortDate(AValue, ADate, FormatSettings.Data) then
            begin
              if FormatSettings.DateTimeSystem = dts1904 then
                ADate := dxRealDateTimeToDateTime(ADate, FormatSettings.DateTimeSystem)
              else
                if (ADate >= 1) and (ADate <= 60) then
                  ADate := ADate - 1;
              AValue := Double(ADate);
            end
            else
              SetError(ecValue);
          end;
        end;
      end;
  Result := Validate;
end;

procedure TdxSpreadSheetFormulaResult.Delete(Index: Integer);
var
  AToken: TdxSpreadSheetFormulaToken;
begin
  AToken := Items[Index];
  Values.Delete(Index);
  if TemporaryTokens.Remove(AToken) >= 0 then
    AToken.Free;
end;

function TdxSpreadSheetFormulaResult.FirstItem: TdxSpreadSheetFormulaToken;
begin
  Result := Items[0];
end;

function TdxSpreadSheetFormulaResult.LastItem: TdxSpreadSheetFormulaToken;
begin
  Result := Items[Count - 1];
end;

procedure TdxSpreadSheetFormulaResult.ExtractCondition(AParams: TdxSpreadSheetFormulaToken;
  AMaxCount, AIndex: Integer; var AConditionValue: Variant; var AOperation: TdxSpreadSheetFormulaOperation);
var
  ARes: TdxSpreadSheetFormulaResult;
  AParamCount: Integer;
begin
  AConditionValue := Null;
  AParamCount := GetParamsCount(AParams);
  if (AParamCount <= AIndex) or (AMaxCount > 0) and (AParamCount > AMaxCount) then
    SetError(ecValue);
  while (AParams <> nil) and (AIndex > 0) do
  begin
    AParams := AParams.Next;
    Dec(AIndex);
  end;
  if AParams <> nil then
    AParams := AParams.FirstChild;
  if AParams = nil then
    SetError(ecValue);
  if ErrorCode <> ecNone then
    Exit;

  AOperation := opEQ;
  ARes := Owner.Calculate(AParams);
  try
    AConditionValue := ARes.Value;
    if VarIsNull(AConditionValue) or VarIsEmpty(AConditionValue) then
      AConditionValue := 0;
    dxSpreadSheetExtractConditionParams(VarToStr(AConditionValue), Owner.FormatSettings, AConditionValue, AOperation);
  finally
    ARes.Free;
  end;
end;

function TdxSpreadSheetFormulaResult.ExtractColumnFromRange(const ARange: TdxSpreadSheetFormulaToken;
  AColumnIndex: Integer; var AErrorCode: TdxSpreadSheetFormulaErrorCode): TdxSpreadSheetVector;
begin
  AErrorCode := ecNone;
  Result := ARange.ExtractColumn(AColumnIndex, AErrorCode);
  if AErrorCode <> ecNone then
    SetError(AErrorCode);
end;

function TdxSpreadSheetFormulaResult.ExtractRowFromRange(const ARange: TdxSpreadSheetFormulaToken;
  ARowIndex: Integer; var AErrorCode: TdxSpreadSheetFormulaErrorCode): TdxSpreadSheetVector;
begin
  AErrorCode := ecNone;
  Result := ARange.ExtractRow(ARowIndex, AErrorCode);
  if AErrorCode <> ecNone then
    SetError(AErrorCode);
end;

function TdxSpreadSheetFormulaResult.GetCount: Integer;
begin
  Result := FValues.Count;
end;

function TdxSpreadSheetFormulaResult.GetFormatSettings: TdxSpreadSheetFormatSettings;
begin
  Result := Owner.FormatSettings;
end;

function TdxSpreadSheetFormulaResult.GetItem(AIndex: Integer): TdxSpreadSheetFormulaToken;
begin
  Result := TdxSpreadSheetFormulaToken(FValues[AIndex]);
end;

function TdxSpreadSheetFormulaResult.GetValue: Variant;
begin
  if Validate and (Values.Count > 0) then
  begin
    LastItem.GetValueRelatedWithCell(Owner.Anchor, Result, FErrorCode);
    if Validate then
      Exit;
  end;
  if Validate then
    FErrorCode := ecValue;
  case FErrorCode of
    ecNull:
      Result := cxGetResourceString(@serNullError);
    ecDivByZero:
      Result := cxGetResourceString(@serDivZeroError);
    ecValue:
      Result := cxGetResourceString(@serValueError);
    ecRefErr:
      Result := cxGetResourceString(@serRefError);
    ecNUM:
      Result := cxGetResourceString(@serNumError);
    ecName:
      Result := cxGetResourceString(@serNameError);
    ecNA:
      Result := cxGetResourceString(@serNAError);
  end;
end;

procedure TdxSpreadSheetFormulaResult.ConvertNullValueToZero;
var
  AValue: TdxSpreadSheetFormulaToken;
begin
  if not Validate or (Count = 0) then
    Exit;

  AValue := Items[Count - 1];
  if AValue is TdxSpreadSheetFormulaVariantToken then
    TdxSpreadSheetFormulaVariantToken(AValue).ConvertNullValueToZero;
end;

function TdxSpreadSheetFormulaResult.DoExtractDateTimeParameter(var AParameter: Variant;
  const AParams: TdxSpreadSheetFormulaToken; AIndex: Integer; AWithoutBoolean: Boolean): Boolean;
var
  ADateTimeSystem: TdxSpreadSheetDateTimeSystem;
  ACanConvertStrToNumber: Boolean;
  ADate: TDateTime;
begin
  Result := ExtractParameter(AParameter, ACanConvertStrToNumber, AParams);
  if not Result then
    Exit;

  ADateTimeSystem := FormatSettings.DateTimeSystem;
  if ConvertToNumeric(AParameter, ACanConvertStrToNumber, AWithoutBoolean) then
  begin
    if AParameter >= 0 then
      AParameter := dxDateTimeToRealDateTime(AParameter, ADateTimeSystem)
    else
      SetError(ecNum);
  end
  else
  if not dxConvertToXLSDate(AParameter, ADate) or ((ADateTimeSystem = dts1904) and (ADate < 1462)) then
  begin
    if not Validate then
      SetError(ecValue);
  end
  else
  begin
    SetError(ecNone);
    AParameter := ADate;
    if (ADateTimeSystem <> dts1904) and (ADate <= 60) then
      AParameter := AParameter - 1;
  end;
  Result := Validate;
end;

function TdxSpreadSheetFormulaResult.ExtractDateTimeOnlyParameter(var AParameter: Variant;
  const AParams: TdxSpreadSheetFormulaToken; AIndex: Integer = 0): Boolean;
var
  ADate: TDateTime;
  AFloat: Double;
begin
  if ExtractParameter(AParameter, AParams, AIndex) then
    if VarIsStr(AParameter) and TryStrToFloat(VarToStr(AParameter), AFloat) then
      SetError(ecValue)
    else
      if not dxConvertToXLSDate(AParameter, ADate) then
        SetError(ecValue)
      else
        AParameter := ADate;
  Result := Validate;
end;

function TdxSpreadSheetFormulaResult.ExtractDateTimeParameter(var AParameter: Variant;
  const AParams: TdxSpreadSheetFormulaToken; AIndex: Integer = 0): Boolean;
begin
  Result := DoExtractDateTimeParameter(AParameter, AParams, AIndex, False);
end;

function TdxSpreadSheetFormulaResult.ExtractDateTimeParameterWithoutBoolean(var AParameter: Variant;
  const AParams: TdxSpreadSheetFormulaToken; AIndex: Integer = 0): Boolean;
begin
  Result := DoExtractDateTimeParameter(AParameter, AParams, AIndex, True);
end;

function TdxSpreadSheetFormulaResult.ExtractErrorCode(const AParams: TdxSpreadSheetFormulaToken): TdxSpreadSheetFormulaErrorCode;
var
  AParamsResult: TdxSpreadSheetFormulaResult;
begin
  AParamsResult := Owner.Calculate(AParams);
  try
    Result := AParamsResult.ErrorCode;
  finally
    AParamsResult.Free;
  end;
end;

function TdxSpreadSheetFormulaResult.ExtractNumericValue(var AValue: Variant): Boolean;
var
  ACanConvertStrToNumber: Boolean;
  AFloatValue: Double;
  AIntValue: Integer;
  AOwnership: TStreamOwnership;
  AToken: TdxSpreadSheetFormulaToken;
begin
  Result := True;
  AToken := ExtractValueToken(AOwnership);
  try
    AValue := Null;
    if AToken <> nil then
    begin
      ACanConvertStrToNumber := AToken.CanConvertStrToNumber;
      AToken.GetValue(AValue, FErrorCode);
      if (FErrorCode = ecNone) and VarIsStr(AValue) then
      begin
        if ACanConvertStrToNumber then
          SetError(ecValue)
        else if TryStrToInt(AValue, AIntValue) then
          AValue := AIntValue
        else if dxTryStrToNumeric(AValue, AFloatValue) then
          AValue := AFloatValue
        else
          SetError(ecValue)
      end
      else
        if ErrorCode = ecNone then
          Result := ConvertToNumeric(AValue, ACanConvertStrToNumber, False);
    end;
  finally
    if AOwnership = soOwned then
      AToken.Free;
  end;
  Result := Result and (ErrorCode = ecNone);
end;

function TdxSpreadSheetFormulaResult.ExtractNumericParameter(var AParameter: Variant;
  const AParams: TdxSpreadSheetFormulaToken; AIndex: Integer = 0): Boolean;
var
  ADate: TDateTime;
  AValue: Variant;
  ACanConvertStrToNumber: Boolean;
begin
  ExtractParameter(AParameter, ACanConvertStrToNumber, AParams, AIndex);
  AValue := AParameter;
  Result := ConvertToNumeric(AParameter, ACanConvertStrToNumber, False);
  if not Result and dxConvertToXLSDate(AValue, ADate) then
  begin
    Result := True;
    SetError(ecNone);
    AParameter := ADate;
  end;
end;

function TdxSpreadSheetFormulaResult.ExtractNumericParameterDef(var AParameter: Variant;
  const ADefaultIfNoExist, ADefaultIfNull: Variant; const AParams: TdxSpreadSheetFormulaToken; AIndex: Integer = 0): Boolean;
begin
  Result := True;
  if not ParameterExists(AParams, AIndex) then
    AParameter := ADefaultIfNoExist
  else
    Result := ExtractNumericParameterDef(AParameter, ADefaultIfNull, AParams, AIndex);
end;

function TdxSpreadSheetFormulaResult.ExtractNumericParameterDef(var AParameter: Variant;
  const ADefaultIfNull: Variant; const AParams: TdxSpreadSheetFormulaToken; AIndex: Integer = 0): Boolean;
var
  ACanConvertStrToNumber: Boolean;
begin
  Result := ParameterIsNull(AParameter, ACanConvertStrToNumber, AParams, AIndex);
  if Result then
    AParameter := ADefaultIfNull
  else
    Result := ConvertToNumeric(AParameter, ACanConvertStrToNumber, False);
end;

function TdxSpreadSheetFormulaResult.ExtractNumericParameterDefWithoutBoolean(var AParameter: Variant;
  const ADefault: Variant; const AParams: TdxSpreadSheetFormulaToken; AIndex: Integer = 0): Boolean;
var
  ACanConvertStrToNumber: Boolean;
begin
  Result := not ParameterExists(AParams, AIndex) or ParameterIsNull(AParameter, ACanConvertStrToNumber, AParams, AIndex);
  if Result then
    AParameter := ADefault
  else
    Result := ConvertToNumeric(AParameter, ACanConvertStrToNumber, True);
end;

function TdxSpreadSheetFormulaResult.ExtractNumericParameterWithoutBoolean(var AParameter: Variant;
  const AParams: TdxSpreadSheetFormulaToken; AIndex: Integer = 0): Boolean;
var
  ACanConvertStrToNumber: Boolean;
begin
  Result := ExtractParameter(AParameter, ACanConvertStrToNumber, AParams, AIndex) and
    ConvertToNumeric(AParameter, ACanConvertStrToNumber, True);
end;

function TdxSpreadSheetFormulaResult.ExtractParameter(var AParameter: Variant; const AParams: TdxSpreadSheetFormulaToken;
  AIndex: Integer = 0): Boolean;
var
  ACanConvertStrToNumber: Boolean;
begin
  Result := ExtractParameter(AParameter, ACanConvertStrToNumber, AParams, AIndex);
end;

function TdxSpreadSheetFormulaResult.ExtractParameter(var AParameter: Variant; out ACanConvertStrToNumber: Boolean;
  const AParams: TdxSpreadSheetFormulaToken; AIndex: Integer = 0): Boolean;
var
  AItemIndex: Integer;
  AItem: TdxSpreadSheetFormulaToken;
  AItemResult: TdxSpreadSheetFormulaResult;
begin
  AParameter := Null;
  ACanConvertStrToNumber := False;
  if AParams = nil then
    SetError(ecNA);
  if Validate then
  begin
    AItemIndex := 0;
    AItem := AParams;
    while AItem <> nil do
    begin
      if AItemIndex = AIndex then
      begin
        AItemResult := Owner.Calculate(AItem.FirstChild);
        try
          if AItemResult.Validate then
          begin
            AParameter := AItemResult.Value;
            ACanConvertStrToNumber := AItem.FirstChild.CanConvertStrToNumber;
          end
          else
            SetError(AItemResult.ErrorCode);
        finally
          AItemResult.Free;
        end;
        Break;
      end;
      Inc(AItemIndex);
      AItem := AItem.Next;
    end;
  end;
  Result := Validate;
end;

function TdxSpreadSheetFormulaResult.ExtractParameterDef(var AParameter: Variant; ADefaultIfNull: Variant;
  const AParams: TdxSpreadSheetFormulaToken; AIndex: Integer = 0): Boolean;
begin
  if ParameterIsNull(AParameter, AParams, AIndex) then
    AParameter := ADefaultIfNull;
  Result := Validate;
end;

function TdxSpreadSheetFormulaResult.ExtractParameterDef(var AParameter: Variant; ADefaultIfNoExist, ADefaultIfNull: Variant;
  const AParams: TdxSpreadSheetFormulaToken; AIndex: Integer = 0): Boolean;
begin
  AParameter := ADefaultIfNoExist;
  if ParameterExists(AParams, AIndex) then
    ExtractParameterDef(AParameter, ADefaultIfNull, AParams, AIndex);
  Result := Validate;
end;

function TdxSpreadSheetFormulaResult.ExtractStringValue: Variant;
var
  ACanConvertStrToNumber: Boolean;
begin
  Result := VarToStr(ExtractValue(ACanConvertStrToNumber));
end;

function TdxSpreadSheetFormulaResult.ExtractStringParameter(const AParams: TdxSpreadSheetFormulaToken; AIndex: Integer = 0): Variant;
var
  ACanConvertStrToNumber: Boolean;
begin
  Result := Null;
  if ExtractParameter(Result, ACanConvertStrToNumber, AParams, AIndex) then
    Result := VarToStr(Result);
end;

procedure TdxSpreadSheetFormulaResult.ExtractToken(AToken: TdxSpreadSheetFormulaToken; out AOwnership: TStreamOwnership);
begin
  Values.Remove(AToken);
  if TemporaryTokens.Remove(AToken) >= 0 then
    AOwnership := soOwned
  else
    AOwnership := soReference;
end;

function TdxSpreadSheetFormulaResult.ExtractValue(out ACanConvertStrToNumber: Boolean): Variant;
var
  AOwnership: TStreamOwnership;
  AValue: TdxSpreadSheetFormulaToken;
begin
  AValue := ExtractValueToken(AOwnership);
  try
    ACanConvertStrToNumber := False;
    if AValue <> nil then
    begin
      ACanConvertStrToNumber := AValue.CanConvertStrToNumber;
      AValue.GetValueRelatedWithCell(Owner.Anchor, Result, FErrorCode);
    end
    else
      Result := Null;
  finally
    if AOwnership = soOwned then
      AValue.Free;
  end;
end;

function TdxSpreadSheetFormulaResult.ExtractValueToken(out AOwnership: TStreamOwnership): TdxSpreadSheetFormulaToken;
begin
  Result := nil;
  if Validate then
  begin
    if Values.Count > 0 then
    begin
      Result := LastItem;
      ExtractToken(Result, AOwnership);
    end
    else
    begin
      Result := TdxSpreadSheetFormulaNullToken.Create;
      AOwnership := soOwned;
    end;
  end;
end;

procedure TdxSpreadSheetFormulaResult.ForEach(AParams: TdxSpreadSheetFormulaToken; AProc: TdxSpreadSheetForEachCallBack; const AData: Pointer);
var
  I: Integer;
  AItemResult: TdxSpreadSheetFormulaResult;
  AToken: TdxSpreadSheetFormulaToken;
  AErrorCode: TdxSpreadSheetFormulaErrorCode;
begin
  if not Validate then
    Exit;
  AToken := AParams.FirstChild;
  if (AToken <> nil) and (AToken.SiblingCount = 1) and AToken.IsEnumeration then
    AToken.ForEach(AProc, AData, FErrorCode)
  else
  begin
    AItemResult := Owner.Calculate(AToken);
    try
      if (AItemResult.Count = 1) and AItemResult.FirstItem.IsEnumeration then
      begin
        if AItemResult.ErrorCode = ecNull then
        begin
          AErrorCode := AItemResult.ErrorCode;
          AProc(Unassigned, False, AErrorCode, AData, nil);
        end
        else
          AItemResult.FirstItem.ForEach(AProc, AData, FErrorCode);
      end
      else
      begin
        if not AItemResult.Validate then
        begin
          AItemResult.ClearValues;
          AItemResult.AddTemporary(TdxSpreadSheetFormulaErrorValueToken.Create(AItemResult.ErrorCode));
        end;
        AErrorCode := AItemResult.ErrorCode;
        for I := 0 to AItemResult.Count - 1 do
        begin
          if not AItemResult[I].ForEach(AProc, AData, AErrorCode) then
            Break;
        end;
        if AErrorCode <> ecNone then
          SetError(AErrorCode);
      end;
    finally
      AItemResult.Free;
    end;
  end;
end;

function TdxSpreadSheetFormulaResult.GetParamsCount(const AParams: TdxSpreadSheetFormulaToken): Integer;
var
  AParam: TdxSpreadSheetFormulaToken;
begin
  if AParams = nil then
    Result := 0
  else
    if AParams.Parent <> nil then
      Result := AParams.Parent.ChildCount
    else
    begin
      Result := 1;
      AParam := AParams.Next;
      while AParam <> nil do
      begin
        Inc(Result);
        AParam := AParam.Next;
      end;
      AParam := AParams.Prev;
      while AParam <> nil do
      begin
        Inc(Result);
        AParam := AParam.Prev;
      end;
    end;
end;

function TdxSpreadSheetFormulaResult.ParameterExists(const AParams: TdxSpreadSheetFormulaToken; AIndex: Integer): Boolean;
begin
  Result := (AParams <> nil) and (GetParamsCount(AParams) - 1 >= AIndex);
end;

function TdxSpreadSheetFormulaResult.ParameterIsNull(var AParameter: Variant;
  const AParams: TdxSpreadSheetFormulaToken; AIndex: Integer = 0): Boolean;
begin
  Result := ExtractParameter(AParameter, AParams, AIndex) and VarIsNull(AParameter);
end;

function TdxSpreadSheetFormulaResult.ParameterIsNull(var AParameter: Variant; out ACanConvertStrToNumber: Boolean;
  const AParams: TdxSpreadSheetFormulaToken; AIndex: Integer = 0): Boolean;
begin
  Result := ExtractParameter(AParameter, ACanConvertStrToNumber, AParams, AIndex) and VarIsNull(AParameter);
end;

procedure TdxSpreadSheetFormulaResult.SetError(ACode: TdxSpreadSheetFormulaErrorCode);
begin
  FErrorCode := ACode;
end;

function TdxSpreadSheetFormulaResult.Validate: Boolean;
begin
  Result := FErrorCode = ecNone;
end;

{ TdxSpreadSheetCustomFormulaList }

function TdxSpreadSheetCustomFormulaList.GetItem(Index: Integer): TdxSpreadSheetCustomFormula;
begin
  Result := TdxSpreadSheetCustomFormula(List[Index]);
end;

end.


