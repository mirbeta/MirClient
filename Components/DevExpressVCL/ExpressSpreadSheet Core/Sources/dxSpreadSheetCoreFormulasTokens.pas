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

unit dxSpreadSheetCoreFormulasTokens;

{$I cxVer.Inc}

interface

uses
  Windows, Types, SysUtils, Classes, Variants,
  // CX
  cxClasses,
  cxGeometry,
  cxVariants,
  dxCoreClasses,
  // SpreadSheet
  dxSpreadSheetClasses,
  dxSpreadSheetCoreFormulas,
  dxSpreadSheetFunctions,
  dxSpreadSheetTypes,
  dxSpreadSheetUtils;

type
  TdxSpreadSheetFormulaArrayToken = class;

{$REGION 'Simple Tokens'}

  { TdxSpreadSheetFormulaNullToken }

  TdxSpreadSheetFormulaNullToken = class(TdxSpreadSheetFormulaToken)
  protected
    procedure Calculate(AResult: TdxSpreadSheetFormulaResult); override;
    procedure ToString(var AAsText: TdxSpreadSheetFormulaToken); override;
  public
    procedure GetValue(var AValue: Variant; var AErrorCode: TdxSpreadSheetFormulaErrorCode); override;
  end;

  { TdxSpreadSheetFormulaStringValueToken }

  TdxSpreadSheetFormulaStringValueToken = class(TdxSpreadSheetFormulaToken)
  protected
    FValue: string;

    procedure ToString(var AAsText: TdxSpreadSheetFormulaToken); override;
  public
    constructor Create(const AValue: string); virtual;
    procedure GetValue(var AValue: Variant; var AErrorCode: TdxSpreadSheetFormulaErrorCode); override;

    property Value: string read FValue write FValue;
  end;

  { TdxSpreadSheetFormulaTextValueToken }

  TdxSpreadSheetFormulaTextValueToken = class(TdxSpreadSheetFormulaStringValueToken)
  protected
    procedure ToString(var AAsText: TdxSpreadSheetFormulaToken); override;
  end;

  { TdxSpreadSheetFormulaVariantToken }

  TdxSpreadSheetFormulaVariantToken = class(TdxSpreadSheetFormulaToken)
  strict private
    FValue: Variant;
  public
    constructor Create(const AValue: Variant); virtual;
    procedure ConvertNullValueToZero;
    procedure GetValue(var AValue: Variant; var AErrorCode: TdxSpreadSheetFormulaErrorCode); override;

    property Value: Variant read FValue write FValue;
  end;

  { TdxSpreadSheetFormulaBooleanValueToken }

  TdxSpreadSheetFormulaBooleanValueToken = class(TdxSpreadSheetFormulaToken)
  protected
    FValue: Boolean;
    procedure ToString(var AAsText: TdxSpreadSheetFormulaToken); override;
  public
    constructor Create(AValue: Boolean); virtual;
    procedure GetValue(var AValue: Variant; var AErrorCode: TdxSpreadSheetFormulaErrorCode); override;

    property Value: Boolean read FValue write FValue;
  end;

  { TdxSpreadSheetFormulaIntegerValueToken }

  TdxSpreadSheetFormulaIntegerValueToken = class(TdxSpreadSheetFormulaToken)
  protected
    FValue: Integer;
    procedure ToString(var AAsText: TdxSpreadSheetFormulaToken); override;
  public
    constructor Create(AValue: Integer); virtual;
    procedure GetValue(var AValue: Variant; var AErrorCode: TdxSpreadSheetFormulaErrorCode); override;

    property Value: Integer read FValue write FValue;
  end;

  { TdxSpreadSheetFormulaFloatValueToken }

  TdxSpreadSheetFormulaFloatValueToken = class(TdxSpreadSheetFormulaToken)
  protected
    FValue: Double;
    procedure ToString(var AAsText: TdxSpreadSheetFormulaToken); override;
  public
    constructor Create(const AValue: Double); virtual;
    procedure GetValue(var AValue: Variant; var AErrorCode: TdxSpreadSheetFormulaErrorCode); override;

    property Value: Double read FValue write FValue;
  end;

  { TdxSpreadSheetFormulaCurrencyValueToken }

  TdxSpreadSheetFormulaCurrencyValueToken = class(TdxSpreadSheetFormulaToken)
  protected
    FValue: Currency;
    procedure ToString(var AAsText: TdxSpreadSheetFormulaToken); override;
  public
    constructor Create(AValue: Currency); virtual;
    procedure GetValue(var AValue: Variant; var AErrorCode: TdxSpreadSheetFormulaErrorCode); override;

    property Value: Currency read FValue write FValue;
  end;

  { TdxSpreadSheetFormulaDateTimeValueToken }

  TdxSpreadSheetFormulaDateTimeValueToken = class(TdxSpreadSheetFormulaToken)
  protected
    FValue: TDateTime;
    procedure Calculate(AResult: TdxSpreadSheetFormulaResult); override;
    procedure ToString(var AAsText: TdxSpreadSheetFormulaToken); override;
  public
    constructor Create(AValue: TDateTime); virtual;

    property Value: TDateTime read FValue write FValue;
  end;

{$ENDREGION}

  { TdxSpreadSheetFormulaOperationToken }

  TdxSpreadSheetFormulaOperationToken = class(TdxSpreadSheetFormulaToken)
  strict private
    FOperation: TdxSpreadSheetFormulaOperation;

    function CalculateAsArray(var AResult: TdxSpreadSheetFormulaResult;
      ALeftToken, ARightToken: TdxSpreadSheetFormulaToken; ADimension: TdxSpreadSheetFormulaTokenDimension): TdxSpreadSheetFormulaArrayToken;
    procedure ExcludeResultValue(var AResult: TdxSpreadSheetFormulaResult; const ANumAtLast: Integer);
    procedure ExtractIterationOperands(var AResult: TdxSpreadSheetFormulaResult; const ACalculateAsArray: Boolean;
      ALeftToken, ARightToken: TdxSpreadSheetFormulaToken; const ARowIndex, AColumnIndex: Integer;
      var ALeft, ARight: Variant; var AErrorCode: TdxSpreadSheetFormulaErrorCode);
    function GetElementaryResult(AResult: TdxSpreadSheetFormulaResult;
      const ALeftValue, ARightValue: Variant; var AErrorCode: TdxSpreadSheetFormulaErrorCode): Variant; inline;
    function IsMustBeLeftValue: Boolean;
    procedure ResetResultErrorCode(var AResult: TdxSpreadSheetFormulaResult; var AErrorCode: TdxSpreadSheetFormulaErrorCode);
  protected
    procedure Calculate(AResult: TdxSpreadSheetFormulaResult); override;
    procedure CalculateReferences(AResult: TdxSpreadSheetFormulaResult);
    function ExtractReference(AResult: TdxSpreadSheetFormulaResult; var AView: TObject; var AArea: TRect): Boolean;
    function GetTokenPriority: Integer; override;
    procedure ToString(var AAsText: TdxSpreadSheetFormulaToken); override;
  public
    constructor Create(AOperation: TdxSpreadSheetFormulaOperation); virtual;
    procedure CheckNeighbors; override;

    property Operation: TdxSpreadSheetFormulaOperation read FOperation;
  end;

  { TdxSpreadSheetFormulaErrorValueToken }

  TdxSpreadSheetFormulaErrorValueToken = class(TdxSpreadSheetFormulaToken)
  strict private
    FErrorCode: TdxSpreadSheetFormulaErrorCode;
  protected
    procedure Calculate(AResult: TdxSpreadSheetFormulaResult); override;
    procedure ToString(var AAsText: TdxSpreadSheetFormulaToken); override;
  public
    constructor Create(AErrorCode: TdxSpreadSheetFormulaErrorCode); virtual;
    procedure GetValue(var AValue: Variant; var AErrorCode: TdxSpreadSheetFormulaErrorCode); override;

    property ErrorCode: TdxSpreadSheetFormulaErrorCode read FErrorCode;
  end;

  { TdxSpreadSheetCustomDefinedNameToken }

  TdxSpreadSheetCustomDefinedNameToken = class(TdxSpreadSheetFormulaStringValueToken)
  public
    function CanConvertStrToNumber: Boolean; override;
  end;

{$REGION 'Array Formula Tokens'}

  { TdxSpreadSheetListToken }

  TdxSpreadSheetListToken = class(TdxSpreadSheetFormulaToken)
  protected
    function ParametersToString: TdxSpreadSheetFormulaFormattedText; virtual;
    procedure ToString(var AAsText: TdxSpreadSheetFormulaToken); override;
  public
    function IsEnumeration: Boolean; override;
  end;

  { TdxSpreadSheetFormulaArrayToken }

  TdxSpreadSheetFormulaArrayToken = class(TdxSpreadSheetListToken)
  strict private
    FIndex: array of TdxSpreadSheetFormulaToken;
    FSize: TSize;
  protected
    procedure CalculateDimension(var ADimension: TdxSpreadSheetFormulaTokenDimension; var AErrorCode: TdxSpreadSheetFormulaErrorCode); override;
    procedure CalculateIndex;
    function ForEach(AProc: TdxSpreadSheetForEachCallBack; const AData: Pointer; var AErrorCode: TdxSpreadSheetFormulaErrorCode): Boolean; override;
    function MakeIndex(ARowIndex, AColumnIndex: Integer): Integer; inline;
    procedure ToString(var AAsText: TdxSpreadSheetFormulaToken); override;
  public
    function CanConvertStrToNumber: Boolean; override;
    function ExtractColumnAsArray(const AIndex: Integer; var AErrorCode: TdxSpreadSheetFormulaErrorCode): TdxSpreadSheetFormulaArrayToken; virtual;
    function ExtractColumn(const AIndex: Integer; var AErrorCode: TdxSpreadSheetFormulaErrorCode): TdxSpreadSheetVector; override;
    function ExtractRow(const AIndex: Integer; var AErrorCode: TdxSpreadSheetFormulaErrorCode): TdxSpreadSheetVector; override;
    function ExtractRowAsArray(const AIndex: Integer; var AErrorCode: TdxSpreadSheetFormulaErrorCode): TdxSpreadSheetFormulaArrayToken; virtual;
    function GetArray(var AErrorCode: TdxSpreadSheetFormulaErrorCode): Variant;
    procedure GetValue(var AValue: Variant; var AErrorCode: TdxSpreadSheetFormulaErrorCode); override;
    procedure GetValueAsArrayItem(const ARow, AColumn: Integer; var AValue: Variant; var AErrorCode: TdxSpreadSheetFormulaErrorCode); override;
    property Size: TSize read FSize;
  end;

  { TdxSpreadSheetFormulaArrayRowSeparator }

  TdxSpreadSheetFormulaArrayRowSeparator = class(TdxSpreadSheetFormulaToken);

{$ENDREGION}

{$REGION 'Reference Tokens'}

  { TdxSpreadSheetFormulaReference }

  TdxSpreadSheetFormulaReference = class(TdxSpreadSheetFormulaToken)
  strict private
    function GetAbsoluteColumn: Boolean;
    function GetAbsoluteRow: Boolean;
    function GetActualColumn: Integer; inline;
    function GetActualRow: Integer; inline;
    function GetAnchorColumn: Integer; inline;
    function GetAnchorRow: Integer; inline;
    function GetIsError: Boolean;
    function GetR1C1Reference: Boolean;
    procedure SetIsError(AValue: Boolean);
  protected
    FColumn: TdxSpreadSheetReference;
    FRow: TdxSpreadSheetReference;

    function ExtractReference(var AView: TObject; var AArea: TRect): Boolean; virtual;
    function ForEach(AProc: TdxSpreadSheetForEachCallBack; const AData: Pointer; var AErrorCode: TdxSpreadSheetFormulaErrorCode): Boolean; override;
    function IsValid: Boolean; virtual;
    procedure Offset(DY, DX: Integer); override;

    // To String
    function LinkToString(ALink: TdxSpreadSheet3DReferenceCustomLink): string; inline;
    function ReferenceToString(const ARow, AColumn: TdxSpreadSheetReference): string; overload; virtual;
    function ReferenceToString: string; overload; virtual;
    procedure ToString(var AAsText: TdxSpreadSheetFormulaToken); override;

    procedure UpdateReferences(AView: TObject; const AArea: TRect;
      const ATargetOrigin: TPoint; AMode: TdxSpreadSheetFormulaUpdateReferencesMode; var AModified: Boolean); override;
    procedure UpdateReferencesCore(AView: TObject; const AArea: TRect;
      const ATargetOrigin: TPoint; AMode: TdxSpreadSheetFormulaUpdateReferencesMode; var AModified: Boolean); virtual;

    property R1C1Reference: Boolean read GetR1C1Reference;
  public
    constructor Create(ARow, AColumn: Integer; AAbsoluteRow, AAbsoluteColumn: Boolean);
    function CanConvertStrToNumber: Boolean; override;
    procedure EnumReferences(AProc: TdxSpreadSheetFormulaEnumReferencesProc; AProcessDefinedNames: Boolean = False); override;
    function ExtractVector(ALength: Integer; AIsVertical: Boolean): TdxSpreadSheetVector;
    procedure GetValue(var AValue: Variant; var AErrorCode: TdxSpreadSheetFormulaErrorCode); override;

    property AbsoluteColumn: Boolean read GetAbsoluteColumn;
    property AbsoluteRow: Boolean read GetAbsoluteRow;
    property ActualColumn: Integer read GetActualColumn;
    property ActualRow: Integer read GetActualRow;
    property AnchorColumn: Integer read GetAnchorColumn;
    property AnchorRow: Integer read GetAnchorRow;
    property IsError: Boolean read GetIsError write SetIsError;
  end;

  { TdxSpreadSheetFormulaAreaReference }

  TdxSpreadSheetFormulaAreaReference = class(TdxSpreadSheetFormulaReference)
  strict private
    procedure CalculateActualRowAndColumnIndexes(var AActualRow, AActualRow2, AActualColumn, AActualColumn2: Integer); inline;
    procedure ExchangeReferences(var ARef, ARef2: TdxSpreadSheetReference);
    function GetAbsoluteColumn2: Boolean;
    function GetAbsoluteRow2: Boolean;
    function GetActualColumn2: Integer;
    function GetActualRow2: Integer;
  protected
    FColumn2: TdxSpreadSheetReference;
    FRow2: TdxSpreadSheetReference;

    procedure CalculateDimension(var ADimension: TdxSpreadSheetFormulaTokenDimension; var AErrorCode: TdxSpreadSheetFormulaErrorCode); override;
    function ExtractReference(var AView: TObject; var AArea: TRect): Boolean; override;
    function ForEach(AProc: TdxSpreadSheetForEachCallBack; const AData: Pointer; var AErrorCode: TdxSpreadSheetFormulaErrorCode): Boolean; override;
    function IsValid: Boolean; override;
    procedure Offset(DY, DX: Integer); override;
    function ReferenceToString: string; override;
    procedure UpdateReferencesCore(AView: TObject; const AArea: TRect;
      const ATargetOrigin: TPoint; AMode: TdxSpreadSheetFormulaUpdateReferencesMode; var AModified: Boolean); override;
  public
    constructor Create(ARow, AColumn, ARow2, AColumn2: Integer;
      AAbsoluteRow, AAbsoluteColumn, AAbsoluteRow2, AAbsoluteColumn2: Boolean);
    procedure Check;
    function ExtractColumn(const AIndex: Integer; var AErrorCode: TdxSpreadSheetFormulaErrorCode): TdxSpreadSheetVector; override;
    function ExtractRow(const AIndex: Integer; var AErrorCode: TdxSpreadSheetFormulaErrorCode): TdxSpreadSheetVector;  override;
    procedure GetValueAsArrayItem(const ARow, AColumn: Integer; var AValue: Variant; var AErrorCode: TdxSpreadSheetFormulaErrorCode); override;
    procedure GetValueRelatedWithCell(const ACell: TPoint; var AValue: Variant; var AErrorCode: TdxSpreadSheetFormulaErrorCode); override;
    function IsEnumeration: Boolean; override;

    property AbsoluteColumn2: Boolean read GetAbsoluteColumn2;
    property AbsoluteRow2: Boolean read GetAbsoluteRow2;
    property ActualColumn2: Integer read GetActualColumn2;
    property ActualRow2: Integer read GetActualRow2;
  end;

  { TdxSpreadSheetFormula3DReference }

  TdxSpreadSheetFormula3DReference = class(TdxSpreadSheetFormulaReference)
  strict private
    FLink: TdxSpreadSheet3DReferenceCustomLink;
  protected
    function ExtractReference(var AView: TObject; var AArea: TRect): Boolean; override;
    function GetView: TObject; override;
    function IsValid: Boolean; override;
    function ReferenceToString: string; override;
  public
    constructor Create(ALink: TdxSpreadSheet3DReferenceCustomLink; ARow, AColumn: Integer; AAbsoluteRow, AAbsoluteColumn: Boolean);
    destructor Destroy; override;
    procedure GetValue(var AValue: Variant; var AErrorCode: TdxSpreadSheetFormulaErrorCode); override;

    property Link: TdxSpreadSheet3DReferenceCustomLink read FLink;
  end;

  { TdxSpreadSheet3DReferenceLink }

  TdxSpreadSheet3DReferenceLink = class(TdxSpreadSheet3DReferenceCustomLink)
  public
    function ToString: string; override;
  end;

  { TdxSpreadSheetFormula3DAreaReference }

  TdxSpreadSheetFormula3DAreaReference = class(TdxSpreadSheetFormulaAreaReference)
  protected
    FLink: TdxSpreadSheet3DReferenceCustomLink;
    FLink2: TdxSpreadSheet3DReferenceCustomLink;

    function ExtractReference(var AView: TObject; var AArea: TRect): Boolean; override;
    function GetView: TObject; override;
    function IsValid: Boolean; override;
    function ReferenceToString: string; override;
  public
    constructor Create(ALink, ALink2: TdxSpreadSheet3DReferenceCustomLink; ARow, AColumn, ARow2, AColumn2: Integer;
      AAbsoluteRow, AAbsoluteColumn, AAbsoluteRow2, AAbsoluteColumn2: Boolean);
    destructor Destroy; override;

    property Link: TdxSpreadSheet3DReferenceCustomLink read FLink;
    property Link2: TdxSpreadSheet3DReferenceCustomLink read FLink2;
    property AbsoluteColumn;
    property AbsoluteColumn2;
    property AbsoluteRow;
    property AbsoluteRow2;
    property ActualColumn;
    property ActualColumn2;
    property ActualRow;
    property ActualRow2;
  end;

{$ENDREGION}

{$REGION 'Functions'}

  { TdxSpreadSheetFormulaFunctionToken }

  TdxSpreadSheetFunctionFakeParams = array of TdxSpreadSheetFormulaToken;

  TdxSpreadSheetFormulaFunctionToken = class(TdxSpreadSheetListToken)
  strict private
    FCachedResultErrorCode: TdxSpreadSheetFormulaErrorCode;
    FCachedResultValue: Variant;
    FCalculatedDataList: TcxObjectList;
    FChildrenOrder: TList;
    FFakeParams: TdxSpreadSheetFunctionFakeParams;
    FFirstChildParent: TdxSpreadSheetFormulaToken;
    FFirstFakeToken: TdxSpreadSheetFormulaToken;
    FInformation: TdxSpreadSheetFunctionInfo;
    FIsCachedResultPresent: Boolean;
    FIsDirtyParamInfo: Boolean;
    FMaxParamCount: Integer;
    FParamKind: TdxSpreadSheetFunctionParamKindInfo;

    procedure CalculateAsArray(AResult: TdxSpreadSheetFormulaResult);
    procedure ClearTemporaryData;
    function CreateArrayCopy(const AArray: TdxSpreadSheetFormulaToken): TdxSpreadSheetFormulaToken;
    function CreateErrorToken(const AErrorCode: TdxSpreadSheetFormulaErrorCode): TdxSpreadSheetFormulaToken;
    function CreateFakeToken(AParam: TdxSpreadSheetFormulaToken; const AIndex: Integer;
      var ADimension: TdxSpreadSheetFormulaTokenDimension; var AErrorCode: TdxSpreadSheetFormulaErrorCode): TdxSpreadSheetFormulaToken;
    procedure DestroyFakeTokensChildren(var AResult: TdxSpreadSheetFormulaResult);
    function HasArrayArgument: Boolean;
    procedure InitializeFakeParams;
    function IsArrayInsteadValue(const AIndex: Integer; AParam: TdxSpreadSheetFormulaToken; ACheckedClass: TClass): Boolean;
    function IsExpectedValueParam(AIndex: Integer): Boolean;
    function GetFakeToken(AIndex: Integer; AParam: TdxSpreadSheetFormulaToken;
      var ADimension: TdxSpreadSheetFormulaTokenDimension;
      var AErrorCode: TdxSpreadSheetFormulaErrorCode): TdxSpreadSheetFormulaToken;
    function GetMaxParamCount: Integer;
    function GetParamKind: TdxSpreadSheetFunctionParamKindInfo;
    procedure PopulateFakeTokensByChildren(const ARow, AColumn: Integer);
    procedure RestoreChildrenOrder;
    procedure SpecifyMaxParamCount;
    procedure StoreChildrenOrder;
  protected
    procedure Calculate(AResult: TdxSpreadSheetFormulaResult); override;
    procedure CalculateDimension(
      var ADimension: TdxSpreadSheetFormulaTokenDimension;
      var AErrorCode: TdxSpreadSheetFormulaErrorCode); override;
    procedure CleanCachedResult;
    procedure CleanFunctionTokenCachedResult; override;
    procedure DoCacheResult(AResult: TdxSpreadSheetFormulaResult);
    procedure DoInformationProc(var AResult: TdxSpreadSheetFormulaResult);
    function ForEach(AProc: TdxSpreadSheetForEachCallBack; const AData: Pointer; var AErrorCode: TdxSpreadSheetFormulaErrorCode): Boolean; override;
    function GetCachedResult(var AResult: TdxSpreadSheetFormulaResult): Boolean;
    function NeedAddFeatureFunctionPrefixToFunctionName: Boolean;
    function NeedForceDimensionCalculating: Boolean; override;
    procedure ToString(var AAsText: TdxSpreadSheetFormulaToken); override;
  public
    constructor Create(AInformation: TdxSpreadSheetFunctionInfo); virtual;
    procedure InitializeParamInfo;
    function IsEnumeration: Boolean; override;

    property Information: TdxSpreadSheetFunctionInfo read FInformation;
    property MaxParamCount: Integer read GetMaxParamCount;
    property ParamKind: TdxSpreadSheetFunctionParamKindInfo read GetParamKind;
  end;

{$ENDREGION}

  { TdxSpreadSheetFormulaParenthesesToken }

  TdxSpreadSheetFormulaParenthesesToken = class(TdxSpreadSheetFormulaToken)
  protected
    procedure Calculate(AResult: TdxSpreadSheetFormulaResult); override;
    procedure ToString(var AAsText: TdxSpreadSheetFormulaToken); override;
  end;

  { TdxSpreadSheetFormulaAttributeToken }

  TdxSpreadSheetFormulaAttributeToken = class(TdxSpreadSheetFormulaToken)
  protected
    procedure Calculate(AResult: TdxSpreadSheetFormulaResult); override;
    function GetTokenPriority: Integer; override;
    procedure ToString(var AAsText: TdxSpreadSheetFormulaToken); override;
  end;

{$REGION 'Unknown Tokens'}

  { TdxSpreadSheetFormulaUnknownNameToken }

  TdxSpreadSheetFormulaUnknownNameToken = class(TdxSpreadSheetListToken)
  strict private
    FLink: TdxSpreadSheet3DReferenceCustomLink;
    FName: string;
    procedure SetLink(const Value: TdxSpreadSheet3DReferenceCustomLink);
  protected
    procedure Calculate(AResult: TdxSpreadSheetFormulaResult); override;
    function LinkAsString: string;
    procedure ToString(var AAsText: TdxSpreadSheetFormulaToken); override;
  public
    constructor Create(const AName: string); virtual;
    destructor Destroy; override;
    function IsEnumeration: Boolean; override;

    property Link: TdxSpreadSheet3DReferenceCustomLink read FLink write SetLink;
    property Name: string read FName;
  end;

  { TdxSpreadSheetFormulaUnknownFunctionToken }

  TdxSpreadSheetFormulaUnknownFunctionToken = class(TdxSpreadSheetFormulaUnknownNameToken)
  protected
    procedure ToString(var AAsText: TdxSpreadSheetFormulaToken); override;
  end;

{$ENDREGION}

procedure dxSpreadSheetAddReferenceToken(AResult: TdxSpreadSheetFormulaResult; AView: TObject; const R: TRect);
function dxSpreadSheetCreateResultToken(const AResultValue: Variant;
  const AErrorCode: TdxSpreadSheetFormulaErrorCode): TdxSpreadSheetFormulaToken; inline;
implementation

uses
  dxSpreadSheetCoreStrs, Math, dxStringHelper;

type
  TControllerAccess = class(TdxSpreadSheetCustomFormulaController);
  TFormulaAccess = class(TdxSpreadSheetCustomFormula);
  TResultAccess = class(TdxSpreadSheetFormulaResult);
  TTokenAccess = class(TdxSpreadSheetFormulaToken);

{ Helpers }

procedure dxSpreadSheetAddReferenceToken(AResult: TdxSpreadSheetFormulaResult; AView: TObject; const R: TRect);
var
  AReference: TdxSpreadSheetFormulaReference;
begin
  if (R.Top = R.Bottom) and (R.Left = R.Right) then
    AReference := TdxSpreadSheetFormula3DReference.Create(
      TdxSpreadSheet3DReferenceLink.Create(AView), R.Top, R.Left, True, True)
  else
    AReference := TdxSpreadSheetFormula3DAreaReference.Create(
      TdxSpreadSheet3DReferenceLink.Create(AView),
      TdxSpreadSheet3DReferenceLink.Create(AView),
      R.Top, R.Left, R.Bottom, R.Right, True, True, True, True);

  AResult.AddTemporary(AReference);
end;

function dxSpreadSheetCreateResultToken(const AResultValue: Variant; const AErrorCode: TdxSpreadSheetFormulaErrorCode): TdxSpreadSheetFormulaToken;
begin
  if AErrorCode = ecNone then
    Result := TdxSpreadSheetFormulaVariantToken.Create(AResultValue)
  else
    Result := TdxSpreadSheetFormulaErrorValueToken.Create(AErrorCode);
end;

{$REGION 'Simple Tokens'}
{ TdxSpreadSheetFormulaEmptyToken }

procedure TdxSpreadSheetFormulaNullToken.Calculate(AResult: TdxSpreadSheetFormulaResult);
begin
  AResult.AddValue(Null)
end;

procedure TdxSpreadSheetFormulaNullToken.GetValue(var AValue: Variant; var AErrorCode: TdxSpreadSheetFormulaErrorCode);
begin
  AValue := 0;
  AErrorCode := ecNone;
end;

procedure TdxSpreadSheetFormulaNullToken.ToString(var AAsText: TdxSpreadSheetFormulaToken);
begin
  AttachString(AAsText, '');
end;

{ TdxSpreadSheetFormulaStringValueToken }

constructor TdxSpreadSheetFormulaStringValueToken.Create(const AValue: string);
begin
  FValue := AValue;
end;

procedure TdxSpreadSheetFormulaStringValueToken.GetValue(
  var AValue: Variant; var AErrorCode: TdxSpreadSheetFormulaErrorCode);
begin
  AValue := FValue;
  AErrorCode := ecNone;
end;

procedure TdxSpreadSheetFormulaStringValueToken.ToString(var AAsText: TdxSpreadSheetFormulaToken);
begin
  AttachString(AAsText, dxSpreadSheetTextValueAsString(Value));
end;

{ TdxSpreadSheetFormulaTextValueToken }

procedure TdxSpreadSheetFormulaTextValueToken.ToString(var AAsText: TdxSpreadSheetFormulaToken);
begin
  AttachString(AAsText, Value);
end;

{ TdxSpreadSheetFormulaVariantToken }

constructor TdxSpreadSheetFormulaVariantToken.Create(const AValue: Variant);
begin
  FValue := AValue;
end;

procedure TdxSpreadSheetFormulaVariantToken.ConvertNullValueToZero;
begin
  if dxSpreadSheetIsNullValue(FValue) then
    FValue := 0;
end;

procedure TdxSpreadSheetFormulaVariantToken.GetValue(var AValue: Variant; var AErrorCode: TdxSpreadSheetFormulaErrorCode);
begin
  AValue := FValue;
  AErrorCode := ecNone;
end;

{ TdxSpreadSheetFormulaBooleanValueToken }

constructor TdxSpreadSheetFormulaBooleanValueToken.Create(AValue: Boolean);
begin
  FValue := AValue;
end;

procedure TdxSpreadSheetFormulaBooleanValueToken.GetValue(
  var AValue: Variant; var AErrorCode: TdxSpreadSheetFormulaErrorCode);
begin
  AValue := FValue;
  AErrorCode := ecNone;
end;

procedure TdxSpreadSheetFormulaBooleanValueToken.ToString(var AAsText: TdxSpreadSheetFormulaToken);
begin
  AttachString(AAsText, dxBoolToString[FValue]);
end;

{ TdxSpreadSheetFormulaIntegerValueToken }

constructor TdxSpreadSheetFormulaIntegerValueToken.Create(AValue: Integer);
begin
  FValue := AValue;
end;

procedure TdxSpreadSheetFormulaIntegerValueToken.GetValue(
  var AValue: Variant; var AErrorCode: TdxSpreadSheetFormulaErrorCode);
begin
  AValue := FValue;
  AErrorCode := ecNone;
end;

procedure TdxSpreadSheetFormulaIntegerValueToken.ToString(var AAsText: TdxSpreadSheetFormulaToken);
begin
  AttachString(AAsText, IntToStr(FValue));
end;

{ TdxSpreadSheetFormulaFloatValueToken }

constructor TdxSpreadSheetFormulaFloatValueToken.Create(const AValue: Double);
begin
  FValue := AValue;
end;

procedure TdxSpreadSheetFormulaFloatValueToken.GetValue(
  var AValue: Variant; var AErrorCode: TdxSpreadSheetFormulaErrorCode);
begin
  AValue := FValue;
  AErrorCode := ecNone;
end;

procedure TdxSpreadSheetFormulaFloatValueToken.ToString(var AAsText: TdxSpreadSheetFormulaToken);
begin
  AttachString(AAsText, dxFloatToStr(FValue, FormatSettings.Data));
end;

{ TdxSpreadSheetFormulaCurrencyValueToken }

constructor TdxSpreadSheetFormulaCurrencyValueToken.Create(AValue: Currency);
begin
  FValue := AValue;
end;

procedure TdxSpreadSheetFormulaCurrencyValueToken.GetValue(
  var AValue: Variant; var AErrorCode: TdxSpreadSheetFormulaErrorCode);
begin
  AValue := FValue;
  AErrorCode := ecNone;
end;

procedure TdxSpreadSheetFormulaCurrencyValueToken.ToString(var AAsText: TdxSpreadSheetFormulaToken);
begin
  AttachString(AAsText, dxFloatToStr(FValue, FormatSettings.Data));
end;

{ TdxSpreadSheetFormulaDateTimeValueToken }

constructor TdxSpreadSheetFormulaDateTimeValueToken.Create(AValue: TDateTime);
begin
  FValue := AValue;
end;

procedure TdxSpreadSheetFormulaDateTimeValueToken.Calculate(AResult: TdxSpreadSheetFormulaResult);
begin
  AResult.AddValue(FValue);
end;

procedure TdxSpreadSheetFormulaDateTimeValueToken.ToString(var AAsText: TdxSpreadSheetFormulaToken);
begin
  AttachString(AAsText, DateTimeToStr(FValue));
end;
{$ENDREGION}

{ TdxSpreadSheetFormulaOperationToken }

constructor TdxSpreadSheetFormulaOperationToken.Create(AOperation: TdxSpreadSheetFormulaOperation);
begin
  FOperation := AOperation;
end;

procedure TdxSpreadSheetFormulaOperationToken.CheckNeighbors;
begin
  if not (Operation in [opAdd, opSub]) then Exit;
  if (Prev = nil) or (Prev.Priority >= 1) or
     ((Operation = opSub) and (Prev is TdxSpreadSheetFormulaOperationToken) and
      (TdxSpreadSheetFormulaOperationToken(Prev).Operation = opUminus)) then
    Inc(FOperation, $F);
end;

procedure TdxSpreadSheetFormulaOperationToken.Calculate(AResult: TdxSpreadSheetFormulaResult);

  procedure PrepareCalculation(var ALeftToken, ARightToken: TdxSpreadSheetFormulaToken;
    var AResultDimension: TdxSpreadSheetFormulaTokenDimension;
    var ACalculateAsArray: Boolean;
    var AErrorCode: TdxSpreadSheetFormulaErrorCode);
  var
    ALeftDimension, ARightDimension: TdxSpreadSheetFormulaTokenDimension;
  begin
    ARightToken := AResult.Items[AResult.Count - 1];
    ACalculateAsArray := Owner.IsArrayFormula or (ARightToken is TdxSpreadSheetFormulaArrayToken);
    if ACalculateAsArray then
      ARightDimension := ARightToken.GetDimension(AErrorCode)
    else
      ARightDimension.SetDimension(1, 1);
    if AErrorCode = ecNone then
      if AResult.Count > 1 then
      begin
        ALeftToken := AResult.Items[AResult.Count - 2];
        ACalculateAsArray := ACalculateAsArray or (ALeftToken is TdxSpreadSheetFormulaArrayToken);
        if ACalculateAsArray then
          ALeftDimension := ALeftToken.GetDimension(AErrorCode)
        else
          ALeftDimension.SetDimension(1, 1);
      end
      else
      begin
        ALeftToken := nil;
        ALeftDimension.SetDimension(0, 0);
      end;

    if AErrorCode = ecNone then
    begin
      if ACalculateAsArray then
      begin
        AResultDimension.SetDimension(
          Max(ALeftDimension.RowCount, ARightDimension.RowCount),
          Max(ALeftDimension.ColumnCount, ARightDimension.ColumnCount))
      end
      else
        AResultDimension.SetDimension(1, 1);
    end;
  end;

var
  ACalculateAsArray: Boolean;
  AChildResult: TdxSpreadSheetFormulaToken;
  ADimension: TdxSpreadSheetFormulaTokenDimension;
  AErrorCode: TdxSpreadSheetFormulaErrorCode;
  ALeft, ARight: Variant;
  ALeftToken, ARightToken: TdxSpreadSheetFormulaToken;
begin
  if Operation in [opIsect, opUnion, opRange] then
  begin
    CalculateReferences(AResult);
    Exit;
  end;

  AErrorCode := ecNone;
  PrepareCalculation(ALeftToken, ARightToken, ADimension, ACalculateAsArray, AErrorCode);
  if AErrorCode = ecNone then
  begin
    if ACalculateAsArray then
      AChildResult := CalculateAsArray(AResult, ALeftToken, ARightToken, ADimension)
    else
    begin
      ExtractIterationOperands(AResult, ACalculateAsArray, ALeftToken, ARightToken, 0, 0, ALeft, ARight, AErrorCode);
      AChildResult := dxSpreadSheetCreateResultToken(GetElementaryResult(AResult, ALeft, ARight, AErrorCode), AErrorCode);
      AResult.AddTemporary(AChildResult);
    end;
    if AChildResult is TdxSpreadSheetFormulaErrorValueToken then
      AResult.SetError(TdxSpreadSheetFormulaErrorValueToken(AChildResult).ErrorCode);
  end
  else
    AResult.SetError(AErrorCode);
end;

function TdxSpreadSheetFormulaOperationToken.CalculateAsArray(var AResult: TdxSpreadSheetFormulaResult;
  ALeftToken, ARightToken: TdxSpreadSheetFormulaToken; ADimension: TdxSpreadSheetFormulaTokenDimension): TdxSpreadSheetFormulaArrayToken;
var
  AArrayToken, ANewToken: TdxSpreadSheetFormulaToken;
  AErrorCode: TdxSpreadSheetFormulaErrorCode;
  ALeft, ARight, AIterationResult: Variant;
  ARepeatingWhenIsDirty: Boolean;
  ARowIndex, AColumnIndex: Integer;
begin
  Result := TdxSpreadSheetFormulaArrayToken.Create;
  AResult.AddTemporary(Result);
  AArrayToken := nil;
  ARepeatingWhenIsDirty := False;
  repeat
    TTokenAccess(Result).IsDirty := False;
    for ARowIndex := 0 to ADimension.RowCount - 1 do
      for AColumnIndex := 0 to ADimension.ColumnCount - 1 do
      begin
        if (AColumnIndex = 0) and (ARowIndex > 0) then
          if not ARepeatingWhenIsDirty then
            TdxSpreadSheetFormulaToken.AddChild(Result, TdxSpreadSheetFormulaArrayRowSeparator.Create)
          else
            AArrayToken := AArrayToken.Next;

        ExtractIterationOperands(AResult, True, ALeftToken, ARightToken, ARowIndex, AColumnIndex, ALeft, ARight, AErrorCode);
        AIterationResult := GetElementaryResult(AResult, ALeft, ARight, AErrorCode);
        if ARepeatingWhenIsDirty then
        begin
          ANewToken := AArrayToken;
          ANewToken.FirstChild.Free;
        end
        else
          ANewToken := TdxSpreadSheetFormulaToken.Create;

        TdxSpreadSheetFormulaToken.AddChild(ANewToken, dxSpreadSheetCreateResultToken(AIterationResult, AErrorCode));
        if not ARepeatingWhenIsDirty then
          TdxSpreadSheetFormulaToken.AddChild(Result, ANewToken);
        AErrorCode := ecNone;

        if ARepeatingWhenIsDirty then
          AArrayToken := AArrayToken.Next;
      end;
    TTokenAccess(Result).ClearIsDimensionCalculated;
    ARepeatingWhenIsDirty := not ARepeatingWhenIsDirty and TTokenAccess(Result).IsDirty;
    if ARepeatingWhenIsDirty then
      AArrayToken := Result.FirstChild;
  until not ARepeatingWhenIsDirty;

  ExcludeResultValue(AResult, 2);
  if IsMustBeLeftValue then
    ExcludeResultValue(AResult, 2);
end;

procedure TdxSpreadSheetFormulaOperationToken.CalculateReferences(AResult: TdxSpreadSheetFormulaResult);
var
  R1, R2: TRect;
  ASheet1, ASheet2: TObject;
begin
  if not (ExtractReference(AResult, ASheet2, R2) and ExtractReference(AResult, ASheet1, R1)) then
    Exit;
  if ASheet1 = nil then
    ASheet1 := ASheet2;
  if ASheet1 = nil then
    ASheet1 := AResult.Owner.View;

  if Operation = opIsect then
  begin
    if not dxSpreadSheetIntersects(R1, R2, R1) then
      AResult.SetError(ecNull);
  end
  else
    if Operation = opUnion then
      R1 := dxSpreadSheetCellsUnion(R1, R2)
    else
      R1.BottomRight := R2.BottomRight;

  dxSpreadSheetAddReferenceToken(AResult, ASheet1, R1);
end;

procedure TdxSpreadSheetFormulaOperationToken.ExcludeResultValue(var AResult: TdxSpreadSheetFormulaResult; const ANumAtLast: Integer);
var
  ACount: Integer;
begin
  ACount := TResultAccess(AResult).Count;
  if ACount > 0 then
    TResultAccess(AResult).Delete(ACount - ANumAtLast);
end;

function TdxSpreadSheetFormulaOperationToken.ExtractReference(
  AResult: TdxSpreadSheetFormulaResult; var AView: TObject; var AArea: TRect): Boolean;
var
  AOwnership: TStreamOwnership;
  AToken: TdxSpreadSheetFormulaToken;
begin
  Result := AResult.ErrorCode = ecNone;
  if Result then
  begin
    AToken := AResult.ExtractValueToken(AOwnership);
    try
      Result := AToken is TdxSpreadSheetFormulaReference;
      if not Result or not TdxSpreadSheetFormulaReference(AToken).ExtractReference(AView, AArea) then
        AResult.SetError(ecRefErr);
      AArea := cxRectAdjust(AArea);
    finally
      if AOwnership = soOwned then
        AToken.Free;
    end;
  end;
end;

procedure TdxSpreadSheetFormulaOperationToken.ExtractIterationOperands(var AResult: TdxSpreadSheetFormulaResult;
  const ACalculateAsArray: Boolean; ALeftToken, ARightToken: TdxSpreadSheetFormulaToken;
  const ARowIndex, AColumnIndex: Integer; var ALeft, ARight: Variant; var AErrorCode: TdxSpreadSheetFormulaErrorCode);

  procedure InternalGetValue(AToken: TdxSpreadSheetFormulaToken; var AValue: Variant; var AErrorCode: TdxSpreadSheetFormulaErrorCode);
  var
    ACanConvertStrToNumber: Boolean;
  begin
    if AToken is TdxSpreadSheetFormulaAreaReference then
    begin
      AToken.GetValueRelatedWithCell(Owner.Anchor, AValue, AErrorCode);
      ExcludeResultValue(AResult, 1);
    end
    else
      if AToken is TdxSpreadSheetFormulaReference then
      begin
        AToken.GetValue(AValue, AErrorCode);
        ExcludeResultValue(AResult, 1);
      end
      else
      begin
        AValue := AResult.ExtractValue(ACanConvertStrToNumber);
        ResetResultErrorCode(AResult, AErrorCode);
      end;
  end;

begin
  ALeft := Null;
  if ACalculateAsArray then
  begin
    ARight := ARightToken.GetValueFromArray(ARowIndex, AColumnIndex, AErrorCode);
    if (AErrorCode = ecNone) and IsMustBeLeftValue then
      ALeft := ALeftToken.GetValueFromArray(ARowIndex, AColumnIndex, AErrorCode);
  end
  else
  begin
    InternalGetValue(ARightToken, ARight, AErrorCode);
    if IsMustBeLeftValue then
      if AErrorCode = ecNone then
        InternalGetValue(ALeftToken, ALeft, AErrorCode)
      else
        ExcludeResultValue(AResult, 1);
  end;
  if (AErrorCode = ecNone) and (Operation = opConcat) then
  begin
    ARight := VarToStr(ARight);
    ALeft := VarToStr(ALeft);
  end;
end;

function TdxSpreadSheetFormulaOperationToken.GetElementaryResult(AResult: TdxSpreadSheetFormulaResult;
  const ALeftValue, ARightValue: Variant; var AErrorCode: TdxSpreadSheetFormulaErrorCode): Variant;
var
  ALeft, ARight: Variant;
begin
  Result := Null;
  ALeft := ALeftValue;
  ARight := ARightValue;
  if AErrorCode <> ecNone then
    Exit;

  if Operation in [opAdd, opSub, opMul] then
  begin
    if dxSpreadSheetIsNullValue(ALeft) and dxSpreadSheetIsNullValue(ARight) then
      Exit(0);
  end;

  if Operation in [opAdd, opSub, opMul, opDiv, opPower, opUplus, opUminus] then
  begin
    if not (AResult.ConvertToNumeric(ALeft, True, False) and AResult.ConvertToNumeric(ARight, True, False)) then
    begin
      AErrorCode := ecValue;
      Exit;
    end;
  end;

  AErrorCode := ecNone;
  case FOperation of
    opAdd, opConcat:
      Result := ALeft + ARight;
    opSub:
      Result := ALeft - ARight;
    opMul:
      Result := ALeft * ARight;
    opDiv:
      try
        if Abs(ARight) <= MinDouble then
          AErrorCode := ecDivByZero
        else
          Result := Extended(ALeft) / Extended(ARight);
      except
        AErrorCode := ecDivByZero;
      end;

    opPower:
      if (ALeft = 0) and (ARight = 0) then
        AErrorCode := ecNUM
      else
        Result := Power(ALeft, ARight);

    opUplus:
      Result := ARight;
    opUminus:
      Result := -ARight;
    opPercent:
      Result := ARight / 100;

    opLT:
      Result := dxSpreadSheetVarCompare(ALeft, ARight, False) < 0;
    opLE:
      Result := dxSpreadSheetVarCompare(ALeft, ARight, False) <= 0;
    opEQ:
      Result := dxSpreadSheetVarCompare(ALeft, ARight, False) = 0;
    opGE:
      Result := dxSpreadSheetVarCompare(ALeft, ARight, False) >= 0;
    opGT:
      Result := dxSpreadSheetVarCompare(ALeft, ARight, False) > 0;
    opNE:
      Result := dxSpreadSheetVarCompare(ALeft, ARight, False) <> 0;
  end;
end;

function TdxSpreadSheetFormulaOperationToken.IsMustBeLeftValue: Boolean;
begin
  Result := FOperation in [opAdd, opConcat, opSub, opMul, opDiv, opPower, opLT, opLE, opEQ, opGE, opGT, opNE];
end;

function TdxSpreadSheetFormulaOperationToken.GetTokenPriority: Integer;
const
  OperationToPriority: array[TdxSpreadSheetFormulaOperation] of Integer =
   (2, 2, 3, 3, 4, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 6, 6, -1, -1);
begin
  Result := OperationToPriority[FOperation];
end;

procedure TdxSpreadSheetFormulaOperationToken.ResetResultErrorCode(var AResult: TdxSpreadSheetFormulaResult;
  var AErrorCode: TdxSpreadSheetFormulaErrorCode);
begin
  AErrorCode := AResult.ErrorCode;
  AResult.SetError(ecNone);
end;

procedure TdxSpreadSheetFormulaOperationToken.ToString(var AAsText: TdxSpreadSheetFormulaToken);
var
  ALeft, ARight: TdxSpreadSheetFormulaFormattedText;
begin
  case FOperation of
    opUplus, opUminus:
      AttachString(AAsText, FormatSettings.Operations[FOperation], ExtractLastTokenAsString(AAsText), '');
    opPercent:
      AttachString(AAsText, '', ExtractLastTokenAsString(AAsText), FormatSettings.Operations[FOperation]);
    opParen:
      AttachString(AAsText, dxLeftParenthesis, ExtractLastTokenAsString(AAsText), dxRightParenthesis);
  else
    begin
      ARight := ExtractLastTokenAsString(AAsText);
      ALeft := ExtractLastTokenAsString(AAsText);
      ALeft.Add(FormatSettings.Operations[FOperation]);
      ALeft.Add(ARight);
      AttachString(AAsText, ALeft);
    end;
  end;
end;

{ TdxSpreadSheetFormulaErrorValueToken }

constructor TdxSpreadSheetFormulaErrorValueToken.Create(AErrorCode: TdxSpreadSheetFormulaErrorCode);
begin
  FErrorCode := AErrorCode;
end;

procedure TdxSpreadSheetFormulaErrorValueToken.GetValue(
  var AValue: Variant; var AErrorCode: TdxSpreadSheetFormulaErrorCode);
begin
  AValue := dxSpreadSheetErrorCodeToString(FErrorCode);
  AErrorCode := FErrorCode;
end;

procedure TdxSpreadSheetFormulaErrorValueToken.Calculate(AResult: TdxSpreadSheetFormulaResult);
begin
  AResult.SetError(FErrorCode);
end;

procedure TdxSpreadSheetFormulaErrorValueToken.ToString(var AAsText: TdxSpreadSheetFormulaToken);
begin
  AttachString(AAsText, dxSpreadSheetErrorCodeToString(FErrorCode));
end;

{ TdxSpreadSheetCustomDefinedNameToken }

function TdxSpreadSheetCustomDefinedNameToken.CanConvertStrToNumber: Boolean;
begin
  Result := False;
end;

{$REGION 'Array Formula Tokens'}

{ TdxSpreadSheetListToken }

function TdxSpreadSheetListToken.IsEnumeration: Boolean;
begin
  Result := True;
end;

function TdxSpreadSheetListToken.ParametersToString: TdxSpreadSheetFormulaFormattedText;
var
  AToken: TdxSpreadSheetFormulaToken;
begin
  Result := TdxSpreadSheetFormulaFormattedText.Create;
  AToken := FirstChild;
  if AToken <> nil then
  repeat
    if AToken.HasChildren then
    begin
      Result.Add(GetExpressionAsText(AToken.FirstChild));
      if (AToken.Next <> nil) and AToken.Next.HasChildren then
        Result.Add(FormatSettings.ListSeparator);
    end
    else
      Result.Add(FormatSettings.ArraySeparator);

    AToken := AToken.Next;
  until AToken = nil;
end;

procedure TdxSpreadSheetListToken.ToString(var AAsText: TdxSpreadSheetFormulaToken);
begin
  AttachString(AAsText, ParametersToString);
end;

{ TdxSpreadSheetFormulaArrayToken }

procedure TdxSpreadSheetFormulaArrayToken.CalculateDimension(
  var ADimension: TdxSpreadSheetFormulaTokenDimension;
  var AErrorCode: TdxSpreadSheetFormulaErrorCode);
var
  I: Integer;
begin
  CalculateIndex;

  AErrorCode := ecNone;
  for I := 0 to Length(FIndex) - 1 do
    if FIndex[I] = nil then
    begin
      AErrorCode := ecValue;
      Break;
    end;

  if (AErrorCode <> ecNone) or (FSize.cx = 0) then
    ADimension.SetDimension(0, 0)
  else
    ADimension.SetDimension(FSize.cy, FSize.cx);
end;

procedure TdxSpreadSheetFormulaArrayToken.CalculateIndex;

  procedure CalculateDimensions(out X, Y: Integer);
  var
    ACount: Integer;
    AToken: TdxSpreadSheetFormulaToken;
  begin
    X := 0;
    Y := 1;
    ACount := 0;
    AToken := FirstChild;
    while AToken <> nil do
    begin
      if AToken.ClassType = TdxSpreadSheetFormulaArrayRowSeparator then
      begin
        X := Max(X, ACount);
        ACount := 0;
        Inc(Y);
      end
      else
        Inc(ACount);

      AToken := AToken.Next;
    end;
    X := Max(X, ACount);
  end;

  procedure PopulateIndex;
  var
    AIndex: Integer;
    ARowIndex: Integer;
    AToken: TdxSpreadSheetFormulaToken;
  begin
    AIndex := 0;
    ARowIndex := 0;
    AToken := FirstChild;
    while AToken <> nil do
    begin
      if AToken.ClassType = TdxSpreadSheetFormulaArrayRowSeparator then
      begin
        Inc(ARowIndex);
        AIndex := MakeIndex(ARowIndex, 0);
      end
      else
      begin
        if AToken.HasChildren then
          FIndex[AIndex] := AToken;
        Inc(AIndex);
      end;
      AToken := AToken.Next;
    end;
  end;

begin
  CalculateDimensions(FSize.cx, FSize.cy);
  SetLength(FIndex, Size.cx * Size.cy);
  FillChar(FIndex[0], Length(FIndex) * SizeOf(Pointer), 0);
  PopulateIndex;
end;

function TdxSpreadSheetFormulaArrayToken.CanConvertStrToNumber: Boolean;
begin
  Result := False;
end;

function TdxSpreadSheetFormulaArrayToken.ExtractColumn(
  const AIndex: Integer; var AErrorCode: TdxSpreadSheetFormulaErrorCode): TdxSpreadSheetVector;
var
  ADimension: TdxSpreadSheetFormulaTokenDimension;
  ARowIndex: Integer;
  AToken: TdxSpreadSheetFormulaToken;
  AValue: Variant;
  AValueErrorCode: TdxSpreadSheetFormulaErrorCode;
  AVector: TdxSpreadSheetSimpleVector;
begin
  ADimension := GetDimension(AErrorCode);
  if (AErrorCode = ecNone) and (AIndex >= ADimension.ColumnCount) then
    AErrorCode := ecRefErr;
  if AErrorCode <> ecNone then
    Exit(TdxSpreadSheetSimpleVector.Create);

  AVector := TdxSpreadSheetSimpleVector.Create(ADimension.RowCount);
  for ARowIndex := 0 to ADimension.RowCount - 1 do
  begin
    AToken := FIndex[MakeIndex(ARowIndex, AIndex)];
    if (AToken <> nil) and AToken.FirstChild.CanConvertStrToNumber then
    begin
      AToken.FirstChild.GetValue(AValue, AValueErrorCode);
      AVector.Add(AValue, AValueErrorCode);
    end
    else
    begin
      AErrorCode := ecValue;
      Break;
    end;
  end;
  Result := AVector;
end;

function TdxSpreadSheetFormulaArrayToken.ExtractColumnAsArray(
  const AIndex: Integer; var AErrorCode: TdxSpreadSheetFormulaErrorCode): TdxSpreadSheetFormulaArrayToken;
var
  ADimension: TdxSpreadSheetFormulaTokenDimension;
  ARow: Integer;
  AItemToken: TdxSpreadSheetFormulaToken;
  AValue: Variant;
begin
  Result := TdxSpreadSheetFormulaArrayToken.Create;
  ADimension := GetDimension(AErrorCode);
  if (AErrorCode = ecNone) and (AIndex >= ADimension.ColumnCount) then
    AErrorCode := ecRefErr;
  if AErrorCode <> ecNone then
    Exit;
  for ARow := 0 to ADimension.RowCount - 1 do
  begin
    AItemToken := TdxSpreadSheetFormulaToken.Create;
    AValue := GetValueFromArray(ARow, AIndex, AErrorCode);
    TdxSpreadSheetFormulaToken.AddChild(AItemToken, dxSpreadSheetCreateResultToken(AValue, AErrorCode));
    TdxSpreadSheetFormulaToken.AddChild(Result, AItemToken);
    if ARow < ADimension.RowCount - 1 then
      TdxSpreadSheetFormulaToken.AddChild(Result, TdxSpreadSheetFormulaArrayRowSeparator.Create);
  end;
end;

function TdxSpreadSheetFormulaArrayToken.ExtractRow(
  const AIndex: Integer; var AErrorCode: TdxSpreadSheetFormulaErrorCode): TdxSpreadSheetVector;
var
  AColumnIndex: Integer;
  ADimension: TdxSpreadSheetFormulaTokenDimension;
  AToken: TdxSpreadSheetFormulaToken;
  AValue: Variant;
  AValueErrorCode: TdxSpreadSheetFormulaErrorCode;
  AVector: TdxSpreadSheetSimpleVector;
begin
  ADimension := GetDimension(AErrorCode);
  if (AErrorCode = ecNone) and (AIndex >= ADimension.RowCount) then
    AErrorCode := ecRefErr;
  if AErrorCode <> ecNone then
    Exit(TdxSpreadSheetSimpleVector.Create);

  AVector := TdxSpreadSheetSimpleVector.Create(ADimension.ColumnCount);
  for AColumnIndex := 0 to ADimension.ColumnCount - 1 do
  begin
    AToken := FIndex[MakeIndex(AIndex, AColumnIndex)];
    if (AToken <> nil) and AToken.FirstChild.CanConvertStrToNumber then
    begin
      AToken.FirstChild.GetValue(AValue, AValueErrorCode);
      AVector.Add(AValue, AValueErrorCode);
    end
    else
    begin
      AErrorCode := ecValue;
      Break;
    end;
  end;
  Result := AVector;
end;

function TdxSpreadSheetFormulaArrayToken.ExtractRowAsArray(
  const AIndex: Integer; var AErrorCode: TdxSpreadSheetFormulaErrorCode): TdxSpreadSheetFormulaArrayToken;
var
  ADimension: TdxSpreadSheetFormulaTokenDimension;
  AColumn: Integer;
  AItemToken: TdxSpreadSheetFormulaToken;
  AValue: Variant;
begin
  Result := TdxSpreadSheetFormulaArrayToken.Create;
  ADimension := GetDimension(AErrorCode);
  if (AErrorCode = ecNone) and (AIndex >= ADimension.RowCount) then
    AErrorCode := ecRefErr;
  if AErrorCode <> ecNone then
    Exit;
  for AColumn := 0 to ADimension.ColumnCount - 1 do
  begin
    AItemToken := TdxSpreadSheetFormulaToken.Create;
    AValue := GetValueFromArray(AIndex, AColumn, AErrorCode);
    TdxSpreadSheetFormulaToken.AddChild(AItemToken, dxSpreadSheetCreateResultToken(AValue, AErrorCode));
    TdxSpreadSheetFormulaToken.AddChild(Result, AItemToken);
  end;
end;

function TdxSpreadSheetFormulaArrayToken.ForEach(AProc: TdxSpreadSheetForEachCallBack;
  const AData: Pointer; var AErrorCode: TdxSpreadSheetFormulaErrorCode): Boolean;
var
  AValue: Variant;
  AToken: TdxSpreadSheetFormulaToken;
begin
  Result := Owner.IsArrayFormula;
  AToken := FirstChild;
  while (AToken <> nil) and (AErrorCode = ecNone) do
  begin
    if not (AToken is TdxSpreadSheetFormulaArrayRowSeparator) then
    begin
      if AToken.HasChildren then
        AToken.FirstChild.GetValue(AValue, AErrorCode)
      else
        AErrorCode := ecValue;

      if not Owner.IsArrayFormula then
        Result := (AErrorCode = ecNone) and AProc(AValue, CanConvertStrToNumber, AErrorCode, AData, nil) and (AErrorCode = ecNone)
      else
        AProc(AValue, CanConvertStrToNumber, AErrorCode, AData, nil);
    end;
    AToken := AToken.Next;
  end;
end;

function TdxSpreadSheetFormulaArrayToken.MakeIndex(ARowIndex, AColumnIndex: Integer): Integer;
begin
  Result := ARowIndex * Size.cx + AColumnIndex;
end;

function TdxSpreadSheetFormulaArrayToken.GetArray(var AErrorCode: TdxSpreadSheetFormulaErrorCode): Variant;
var
  AColumn: Integer;
  ADimension: TdxSpreadSheetFormulaTokenDimension;
  AIndex: Integer;
  ARow: Integer;
  AToken: TdxSpreadSheetFormulaToken;
  AValue: Variant;
begin
  ADimension := GetDimension(AErrorCode);
  if AErrorCode <> ecNone then
    Exit(Null);

  Result := VarArrayCreate([0, ADimension.RowCount - 1, 0, ADimension.ColumnCount - 1], varVariant);
  for ARow := 0 to ADimension.RowCount - 1 do
  begin
    AIndex := MakeIndex(ARow, 0);
    for AColumn := 0 to ADimension.ColumnCount - 1 do
    begin
      AToken := FIndex[AIndex + AColumn];
      if (AToken <> nil) and AToken.FirstChild.CanConvertStrToNumber then
      begin
        AToken.FirstChild.GetValue(AValue, AErrorCode);
        if AErrorCode = ecNone then
          Result[ARow, AColumn] := AValue;
      end
      else
        AErrorCode := ecValue;

      if AErrorCode <> ecNone then
        Exit(Null);
    end;
    if AErrorCode <> ecNone then
      Exit(Null);
  end;
end;

procedure TdxSpreadSheetFormulaArrayToken.GetValueAsArrayItem(
  const ARow, AColumn: Integer; var AValue: Variant; var AErrorCode: TdxSpreadSheetFormulaErrorCode);
var
  ADimension: TdxSpreadSheetFormulaTokenDimension;
  AToken: TdxSpreadSheetFormulaToken;
begin
  ADimension := GetDimension(AErrorCode);
  if AErrorCode <> ecNone then
    Exit;
  if InRange(ARow, 0, ADimension.RowCount - 1) and InRange(AColumn, 0, ADimension.ColumnCount - 1) then
  begin
    AToken := FIndex[MakeIndex(ARow, AColumn)];
    if AToken <> nil then
      AToken.FirstChild.GetValue(AValue, AErrorCode)
    else
      AErrorCode := ecNA;
  end
  else
    AErrorCode := ecNA;
end;

procedure TdxSpreadSheetFormulaArrayToken.GetValue(var AValue: Variant; var AErrorCode: TdxSpreadSheetFormulaErrorCode);
var
  AToken: TdxSpreadSheetFormulaToken;
begin
  AToken := FirstChild;
  if AToken.HasChildren then
    AToken.FirstChild.GetValue(AValue, AErrorCode)
  else
    AErrorCode := ecValue;
end;

procedure TdxSpreadSheetFormulaArrayToken.ToString(var AAsText: TdxSpreadSheetFormulaToken);
begin
  AttachString(AAsText, dxLeftArrayParenthesis, ParametersToString, dxRightArrayParenthesis);
end;

{$ENDREGION}

{$REGION 'Reference Tokens'}

{ TdxSpreadSheetFormulaReference }

constructor TdxSpreadSheetFormulaReference.Create(ARow, AColumn: Integer; AAbsoluteRow, AAbsoluteColumn: Boolean);
begin
  inherited Create;
  FRow.Offset := ARow;
  FRow.IsAbsolute := AAbsoluteRow;
  FColumn.Offset := AColumn;
  FColumn.IsAbsolute := AAbsoluteColumn;
end;

function TdxSpreadSheetFormulaReference.CanConvertStrToNumber: Boolean;
begin
  Result := False;
end;

procedure TdxSpreadSheetFormulaReference.EnumReferences(
  AProc: TdxSpreadSheetFormulaEnumReferencesProc; AProcessDefinedNames: Boolean = False);
var
  AArea: TRect;
  AView: TObject;
begin
  inherited EnumReferences(AProc, AProcessDefinedNames);

  if ExtractReference(AView, AArea) then
    AProc(dxSpreadSheetGetRealArea(AArea), AView)
  else
    AProc(cxInvalidRect, nil);
end;

function TdxSpreadSheetFormulaReference.ExtractVector(ALength: Integer; AIsVertical: Boolean): TdxSpreadSheetVector;
begin
  if AIsVertical then
  begin
    if FRow.IsAllItems then
      Result := TdxSpreadSheetReferenceVector.Create(View, Controller, False, ActualColumn, 0, dxSpreadSheetMaxRowCount)
    else
      Result := TdxSpreadSheetReferenceVector.Create(View, Controller, False, ActualColumn, ActualRow, ALength);
  end
  else
    if FColumn.IsAllItems then
      Result := TdxSpreadSheetReferenceVector.Create(View, Controller, True, ActualRow, 0, dxSpreadSheetMaxColumnCount)
    else
      Result := TdxSpreadSheetReferenceVector.Create(View, Controller, True, ActualRow, ActualColumn, ALength);
end;

function TdxSpreadSheetFormulaReference.GetAbsoluteColumn: Boolean;
begin
  Result := FColumn.IsAbsolute;
end;

function TdxSpreadSheetFormulaReference.GetAbsoluteRow: Boolean;
begin
  Result := FRow.IsAbsolute;
end;

function TdxSpreadSheetFormulaReference.GetActualColumn: Integer;
begin
  Result := FColumn.ActualValue(AnchorColumn);
end;

function TdxSpreadSheetFormulaReference.GetActualRow: Integer;
begin
  Result := FRow.ActualValue(AnchorRow);
end;

function TdxSpreadSheetFormulaReference.GetAnchorColumn: Integer;
begin
  if Owner <> nil then
    Result := Owner.AnchorColumn
  else
    Result := 0;
end;

function TdxSpreadSheetFormulaReference.GetAnchorRow: Integer;
begin
  if Owner <> nil then
    Result := Owner.AnchorRow
  else
    Result := 0;
end;

function TdxSpreadSheetFormulaReference.GetIsError: Boolean;
begin
  Result := FRow.IsError or FColumn.IsError;
end;

function TdxSpreadSheetFormulaReference.GetR1C1Reference: Boolean;
begin
  Result := Owner.FormatSettings.R1C1Reference;
end;

procedure TdxSpreadSheetFormulaReference.SetIsError(AValue: Boolean);
begin
  FRow.IsError := True;
  FColumn.IsError := True;
end;

procedure TdxSpreadSheetFormulaReference.GetValue(var AValue: Variant; var AErrorCode: TdxSpreadSheetFormulaErrorCode);
begin
  GetCellValue(Owner.View, ActualRow, ActualColumn, AValue, AErrorCode);
end;

function TdxSpreadSheetFormulaReference.ExtractReference(var AView: TObject; var AArea: TRect): Boolean;
begin
  AView := nil;
  AArea.Top := ActualRow;
  AArea.Left := ActualColumn;
  AArea.BottomRight := AArea.TopLeft;
  Result := (AArea.Top >= 0) and (AArea.Left >= 0);
  if Result then
  begin
    if FColumn.IsAllItems then
      AArea.Left := 0;
    if FRow.IsAllItems then
      AArea.Top := 0;
  end;
end;

function TdxSpreadSheetFormulaReference.ForEach(AProc: TdxSpreadSheetForEachCallBack;
  const AData: Pointer; var AErrorCode: TdxSpreadSheetFormulaErrorCode): Boolean;
var
  ACellReference: TdxSpreadSheetCellReference;
  AValue: Variant;
begin
  GetValue(AValue, AErrorCode);
  ACellReference.ColumnIndex := ActualColumn;
  ACellReference.RowIndex := ActualRow;
  ACellReference.View := View;
  Result := AProc(AValue, CanConvertStrToNumber, AErrorCode, AData, @ACellReference) and (AErrorCode = ecNone);
end;

function TdxSpreadSheetFormulaReference.IsValid: Boolean;
var
  AErrorCode: TdxSpreadSheetFormulaErrorCode;
begin
  Result := CheckCellReference(ActualRow, ActualColumn, AErrorCode);
end;

procedure TdxSpreadSheetFormulaReference.Offset(DY, DX: Integer);
begin
  FRow.Move(DY);
  FColumn.Move(DX);
end;

function TdxSpreadSheetFormulaReference.LinkToString(ALink: TdxSpreadSheet3DReferenceCustomLink): string;
begin
  if ALink <> nil then
    Result := ALink.ToString
  else
    Result := '';
end;

function TdxSpreadSheetFormulaReference.ReferenceToString: string;
begin
  Result := ReferenceToString(FRow, FColumn);
end;

function TdxSpreadSheetFormulaReference.ReferenceToString(const ARow, AColumn: TdxSpreadSheetReference): string;
begin
  if R1C1Reference then
    Result := dxR1C1ReferenceToString(AnchorRow, AnchorColumn, ARow, AColumn)
  else
    Result := dxReferenceToString(AnchorRow, AnchorColumn, ARow, AColumn);
end;

procedure TdxSpreadSheetFormulaReference.ToString(var AAsText: TdxSpreadSheetFormulaToken);
var
  AFormattedText: TdxSpreadSheetFormulaFormattedText;
begin
  AFormattedText := TdxSpreadSheetFormulaFormattedText.Create(ReferenceToString);
  AFormattedText.Runs.Add(1, nil, 1);
  AFormattedText.Runs.Add(1 + Length(AFormattedText.Value), nil);
  AttachString(AAsText, AFormattedText);
end;

procedure TdxSpreadSheetFormulaReference.UpdateReferences(AView: TObject;
  const AArea: TRect; const ATargetOrigin: TPoint; AMode: TdxSpreadSheetFormulaUpdateReferencesMode; var AModified: Boolean);
var
  AReferenceArea: TRect;
begin
  if (Owner.View = AView) or (View = AView) then
  begin
    UpdateReferencesCore(AView, AArea, ATargetOrigin, AMode, AModified);
    if not AModified and (AMode = urmMove) and ExtractReference(AView, AReferenceArea) then
      AModified := dxSpreadSheetIntersects(AReferenceArea, cxRectSetOrigin(AArea, ATargetOrigin));
    if AModified then
      ClearIsDimensionCalculated;
  end;
end;

procedure TdxSpreadSheetFormulaReference.UpdateReferencesCore(AView: TObject; const AArea: TRect;
  const ATargetOrigin: TPoint; AMode: TdxSpreadSheetFormulaUpdateReferencesMode; var AModified: Boolean);
var
  AIsAnchorInArea: Boolean;
  AIsReferenceInArea: Boolean;
begin
  if AMode <> urmMove then
  begin
    if InRange(AnchorColumn, AArea.Left, AArea.Right) then
      AModified := FRow.UpdateReference(AnchorRow, AArea.Top, AArea.Bottom, ATargetOrigin.Y, AView = View, Owner.View = AView) or AModified;
    if InRange(AnchorRow, AArea.Top, AArea.Bottom) then
      AModified := FColumn.UpdateReference(AnchorColumn, AArea.Left, AArea.Right, ATargetOrigin.X, AView = View, Owner.View = AView) or AModified;
  end
  else
  begin
    AIsAnchorInArea := (Owner.View = AView) and dxSpreadSheetContains(AArea, AnchorRow, AnchorColumn);
    AIsReferenceInArea := (AView = View) and dxSpreadSheetContains(AArea, ActualRow, ActualColumn);

    AModified := FRow.UpdateReference(AnchorRow, AArea.Top, AArea.Bottom,
      ATargetOrigin.Y, AIsReferenceInArea, AIsAnchorInArea) or AModified;
    AModified := FColumn.UpdateReference(AnchorColumn, AArea.Left, AArea.Right,
      ATargetOrigin.X, AIsReferenceInArea, AIsAnchorInArea) or AModified;
  end;
end;

{ TdxSpreadSheetFormulaAreaReference }

constructor TdxSpreadSheetFormulaAreaReference.Create(ARow, AColumn, ARow2, AColumn2: Integer;
  AAbsoluteRow, AAbsoluteColumn, AAbsoluteRow2, AAbsoluteColumn2: Boolean);
begin
  inherited Create(ARow, AColumn, AAbsoluteRow, AAbsoluteColumn);
  FRow2.Offset := ARow2;
  FRow2.IsAbsolute := AAbsoluteRow2;
  FColumn2.Offset := AColumn2;
  FColumn2.IsAbsolute := AAbsoluteColumn2;
  FRow.IsAllItems := ARow2 = MaxInt;
  FColumn.IsAllItems := AColumn2 = MaxInt;
end;

procedure TdxSpreadSheetFormulaAreaReference.CalculateActualRowAndColumnIndexes(var AActualRow, AActualRow2, AActualColumn, AActualColumn2: Integer);
begin
  AActualRow := ActualRow;
  AActualRow2 := ActualRow2;
  if FRow.IsAllItems or FRow2.IsAllItems then
  begin
    AActualRow := 0;
    AActualRow2 := dxSpreadSheetMaxRowIndex;
  end;
  AActualColumn := ActualColumn;
  AActualColumn2 := ActualColumn2;
  if FColumn.IsAllItems or FColumn2.IsAllItems then
  begin
    AActualColumn := 0;
    AActualColumn2 := dxSpreadSheetMaxColumnIndex;
  end;
end;

procedure TdxSpreadSheetFormulaAreaReference.CalculateDimension(var ADimension: TdxSpreadSheetFormulaTokenDimension; var AErrorCode: TdxSpreadSheetFormulaErrorCode);
var
  AActualRow, AActualRow2, AActualColumn, AActualColumn2: Integer;
begin
  inherited CalculateDimension(ADimension, AErrorCode);
  CalculateActualRowAndColumnIndexes(AActualRow, AActualRow2, AActualColumn, AActualColumn2);
  if (AActualColumn < 0) or (AActualColumn2 < 0) or (AActualRow < 0) or (AActualRow2 < 0) then
    AErrorCode := ecRefErr
  else
    ADimension.SetDimension(Abs(AActualRow2 - AActualRow) + 1, Abs(AActualColumn2 - AActualColumn) + 1);
end;

procedure TdxSpreadSheetFormulaAreaReference.ExchangeReferences(var ARef, ARef2: TdxSpreadSheetReference);
var
  A: TdxSpreadSheetReference;
begin
  A := ARef;
  ARef := ARef2;
  ARef2 := A;
end;

procedure TdxSpreadSheetFormulaAreaReference.Check;

  function GetRealIndex(ARef: TdxSpreadSheetReference; const ACellIndex: Integer): Integer;
  begin
    Result := ARef.ActualValue(0);
    if not ARef.IsAbsolute then
      Inc(Result, ACellIndex);
  end;

  procedure CheckReferences(var ARef, ARef2: TdxSpreadSheetReference; const ACellIndex: Integer);
  var
    AIndex, AIndex2: Integer;
  begin
    AIndex := GetRealIndex(ARef, ACellIndex);
    AIndex2 := GetRealIndex(ARef2, ACellIndex);
    if AIndex > AIndex2 then
      ExchangeReferences(ARef, ARef2);
  end;

begin
  CheckReferences(FRow, FRow2, AnchorRow);
  CheckReferences(FColumn, FColumn2, AnchorColumn);
end;

procedure TdxSpreadSheetFormulaAreaReference.GetValueAsArrayItem(const ARow, AColumn: Integer; var AValue: Variant; var AErrorCode: TdxSpreadSheetFormulaErrorCode);
var
  AActualColumn: Integer;
  AActualColumn2: Integer;
  AActualRow: Integer;
  AActualRow2: Integer;
begin
  CalculateActualRowAndColumnIndexes(AActualRow, AActualRow2, AActualColumn, AActualColumn2);
  GetCellValue(GetView, AActualRow + ARow, AActualColumn + AColumn, AValue, AErrorCode);
end;

procedure TdxSpreadSheetFormulaAreaReference.GetValueRelatedWithCell(
  const ACell: TPoint; var AValue: Variant; var AErrorCode: TdxSpreadSheetFormulaErrorCode);

  function InternalCheck(var AIndex: Integer; var AError: TdxSpreadSheetFormulaErrorCode; const AIndex1, AIndex2: Integer): Boolean;
  begin
    Result := (AIndex >= Min(AIndex1, AIndex2)) and (AIndex <= Max(AIndex1, AIndex2));
    if not Result then
      AIndex := Min(AIndex1, AIndex2);
    if Min(AIndex1, AIndex2) < 0 then
      AError := ecValue;
  end;

var
  AAreaColumn: Integer;
  AAreaColumn2: Integer;
  AAreaRow: Integer;
  AAreaRow2: Integer;
  AColumn: Integer;
  AIsColumnValid: Boolean;
  AIsOutOfRange: Boolean;
  AIsRowValid: Boolean;
  ARow: Integer;
begin
  CalculateActualRowAndColumnIndexes(AAreaRow, AAreaRow2, AAreaColumn, AAreaColumn2);
  if (AAreaRow <> AAreaRow2) and (AAreaColumn <> AAreaColumn2) then
    AErrorCode := ecValue
  else
  begin
    ARow := ACell.Y;
    AColumn := ACell.X;
    AIsRowValid := InternalCheck(ARow, AErrorCode, AAreaRow, AAreaRow2) and (AAreaColumn = AAreaColumn2);
    AIsColumnValid := InternalCheck(AColumn, AErrorCode, AAreaColumn, AAreaColumn2) and (AAreaRow = AAreaRow2);
    AIsOutOfRange := not AIsRowValid and not AIsColumnValid;
    if AIsOutOfRange and (AErrorCode = ecNone) then
    begin
      if TControllerAccess(Owner.Controller).IsPartOfArrayFormula(View, ACell.Y, ACell.X) = afpNone then
        AErrorCode := ecValue;
    end;
    if AErrorCode <> ecValue then
      GetCellValue(View, ARow, AColumn, AValue, AErrorCode);
  end;
end;

function TdxSpreadSheetFormulaAreaReference.IsEnumeration: Boolean;
begin
  Result := True;
end;

function TdxSpreadSheetFormulaAreaReference.ExtractColumn(const AIndex: Integer; var AErrorCode: TdxSpreadSheetFormulaErrorCode): TdxSpreadSheetVector;
var
  AActualRow, AActualRow2: Integer;
begin
  AErrorCode := ecNone;
  if FRow.IsAllItems then
    Result := TdxSpreadSheetReferenceVector.Create(View, Controller, False,
      ActualColumn + AIndex, 0, dxSpreadSheetMaxRowCount)
  else
  begin
    AActualRow := ActualRow;
    AActualRow2 := ActualRow2;
    Result := TdxSpreadSheetReferenceVector.Create(View, Controller, False,
      ActualColumn + AIndex, AActualRow, AActualRow2 - AActualRow + 1);
  end;
end;

function TdxSpreadSheetFormulaAreaReference.ExtractRow(const AIndex: Integer; var AErrorCode: TdxSpreadSheetFormulaErrorCode): TdxSpreadSheetVector;
var
  AActualColumn, AActualColumn2: Integer;
begin
  AErrorCode := ecNone;
  if FColumn.IsAllItems then
    Result := TdxSpreadSheetReferenceVector.Create(View, Controller, True,
      ActualRow + AIndex, 0, dxSpreadSheetMaxColumnCount)
  else
  begin
    AActualColumn := ActualColumn;
    AActualColumn2 := ActualColumn2;
    Result := TdxSpreadSheetReferenceVector.Create(View, Controller, True,
      ActualRow + AIndex, AActualColumn, AActualColumn2 - AActualColumn + 1);
  end;
end;

function TdxSpreadSheetFormulaAreaReference.ForEach(AProc: TdxSpreadSheetForEachCallBack;
  const AData: Pointer; var AErrorCode: TdxSpreadSheetFormulaErrorCode): Boolean;
var
  ARow, ARow2, AColumn, AColumn2: Integer;
begin
  CalculateActualRowAndColumnIndexes(ARow, ARow2, AColumn, AColumn2);
  Result := ForEachCell(View, AColumn, ARow, AColumn2, ARow2, AProc, AData, AErrorCode);
end;

function TdxSpreadSheetFormulaAreaReference.IsValid: Boolean;
var
  AErrorCode: TdxSpreadSheetFormulaErrorCode;
begin
  Result := inherited IsValid and CheckCellReference(ActualRow2, ActualColumn2, AErrorCode);
end;

function TdxSpreadSheetFormulaAreaReference.ExtractReference(var AView: TObject; var AArea: TRect): Boolean;
begin
  Result := inherited ExtractReference(AView, AArea);
  if Result then
  begin
    AArea.Bottom := ActualRow2;
    AArea.Right := ActualColumn2;
    Result := (AArea.Bottom >= 0) and (AArea.Right >= 0);
  end;
end;

function TdxSpreadSheetFormulaAreaReference.ReferenceToString: string;
begin
  if (FColumn.IsError or FRow.IsError) and (FColumn2.IsError or FRow2.IsError) then
    Result := serRefError
  else
    if R1C1Reference then
      Result := dxR1C1ReferenceAreaToString(AnchorRow, AnchorColumn, '', FRow, FColumn, '', FRow2, FColumn2)
    else
      Result := ReferenceToString(FRow, FColumn) + dxAreaSeparator + ReferenceToString(FRow2, FColumn2);
end;

procedure TdxSpreadSheetFormulaAreaReference.Offset(DY, DX: Integer);
begin
  inherited Offset(DY, DX);
  FRow2.Move(DY);
  FColumn2.Move(DX);
end;

procedure TdxSpreadSheetFormulaAreaReference.UpdateReferencesCore(
  AView: TObject; const AArea: TRect; const ATargetOrigin: TPoint;
  AMode: TdxSpreadSheetFormulaUpdateReferencesMode; var AModified: Boolean);

  function CanMoveReference: Boolean;
  begin
    Result := (AMode <> urmMove) or
      dxSpreadSheetContains(AArea, ActualRow, ActualColumn) and
      dxSpreadSheetContains(AArea, ActualRow2, ActualColumn2);
  end;

  function CanUpdateColumnReference: Boolean;
  begin
    Result := (AArea.Left <> ATargetOrigin.X) and InRange(AnchorRow, AArea.Top, AArea.Bottom);
    if AMode = urmMove then
      Result := Result and InRange(AnchorColumn, AArea.Left, AArea.Right);
  end;

  function CanUpdateRowReference: Boolean;
  begin
    Result := (AArea.Top <> ATargetOrigin.Y) and InRange(AnchorColumn, AArea.Left, AArea.Right);
    if AMode = urmMove then
      Result := Result and InRange(AnchorRow, AArea.Top, AArea.Bottom);
  end;

begin
  if CanUpdateRowReference then
    AModified := FRow.UpdateAreaReference(AnchorRow, AArea.Top, AArea.Bottom, ATargetOrigin.Y, FRow2, CanMoveReference, Owner.View = AView) or AModified;
  if CanUpdateColumnReference then
    AModified := FColumn.UpdateAreaReference(AnchorColumn, AArea.Left, AArea.Right, ATargetOrigin.X, FColumn2, CanMoveReference, Owner.View = AView) or AModified;
end;

function TdxSpreadSheetFormulaAreaReference.GetAbsoluteColumn2: Boolean;
begin
  Result := FColumn2.IsAbsolute;
end;

function TdxSpreadSheetFormulaAreaReference.GetAbsoluteRow2: Boolean;
begin
  Result := FRow2.IsAbsolute;
end;

function TdxSpreadSheetFormulaAreaReference.GetActualColumn2: Integer;
begin
  Result := FColumn2.ActualValue(AnchorColumn);
end;

function TdxSpreadSheetFormulaAreaReference.GetActualRow2: Integer;
begin
  Result := FRow2.ActualValue(AnchorRow);
end;

{ TdxSpreadSheetFormula3DReference }

constructor TdxSpreadSheetFormula3DReference.Create(ALink: TdxSpreadSheet3DReferenceCustomLink;
  ARow, AColumn: Integer; AAbsoluteRow, AAbsoluteColumn: Boolean);
begin
  inherited Create(ARow, AColumn, AAbsoluteRow, AAbsoluteColumn);
  SetLink(FLink, ALink);
end;

destructor TdxSpreadSheetFormula3DReference.Destroy;
begin
  FreeAndNil(FLink);
  inherited Destroy;
end;

procedure TdxSpreadSheetFormula3DReference.GetValue(var AValue: Variant; var AErrorCode: TdxSpreadSheetFormulaErrorCode);
var
  AView: TObject;
begin
  AView := View;
  if AView <> nil then
    GetCellValue(AView, ActualRow, ActualColumn, AValue, AErrorCode)
  else
    AErrorCode := ecRefErr;
end;

function TdxSpreadSheetFormula3DReference.ExtractReference(var AView: TObject; var AArea: TRect): Boolean;
begin
  Result := inherited ExtractReference(AView, AArea) and (View <> nil);
  if Result then
    AView := View;
end;

function TdxSpreadSheetFormula3DReference.GetView: TObject;
begin
  if (Link is TdxSpreadSheet3DReferenceLink) and TdxSpreadSheetInvalidObject.IsLive(Link.Data) then
    Result := Link.Data
  else
    Result := nil;
end;

function TdxSpreadSheetFormula3DReference.IsValid: Boolean;
begin
  Result := (View <> nil) and inherited IsValid;
end;

function TdxSpreadSheetFormula3DReference.ReferenceToString: string;
begin
  Result := LinkToString(Link) + ReferenceToString(FRow, FColumn);
end;

{ TdxSpreadSheet3DReferenceLink }

function TdxSpreadSheet3DReferenceLink.ToString: string;
var
  AIntf: IdxSpreadSheetViewCaption;
  AView: TObject;
begin
  AView := Data;
  if AView = nil then
    Result := ''
  else
    if TdxSpreadSheetInvalidObject.IsInvalid(AView) then
      Result := serRefError
    else
      if Supports(AView, IdxSpreadSheetViewCaption, AIntf) then
      begin
        Result := AIntf.GetCaption;
        if AIntf.IsCaptionTextDelimited then
          Result := dxStringMarkChar2 + Result + dxStringMarkChar2;
        Result := Result + dxRefSeparator
      end
      else
        Result := serRefError;
end;

{ TdxSpreadSheetFormula3DAreaReference }

constructor TdxSpreadSheetFormula3DAreaReference.Create(ALink, ALink2: TdxSpreadSheet3DReferenceCustomLink;
  ARow, AColumn, ARow2, AColumn2: Integer; AAbsoluteRow, AAbsoluteColumn, AAbsoluteRow2, AAbsoluteColumn2: Boolean);
begin
  inherited Create(ARow, AColumn, ARow2, AColumn2, AAbsoluteRow, AAbsoluteColumn, AAbsoluteRow2, AAbsoluteColumn2);
  SetLink(FLink, ALink);
  SetLink(FLink2, ALink2);
end;

destructor TdxSpreadSheetFormula3DAreaReference.Destroy;
begin
  FreeAndNil(FLink);
  FreeAndNil(FLink2);
  inherited Destroy;
end;

function TdxSpreadSheetFormula3DAreaReference.ExtractReference(var AView: TObject; var AArea: TRect): Boolean;
begin
  Result := inherited ExtractReference(AView, AArea) and (View <> nil);
  if Result then
    AView := View;
end;

function TdxSpreadSheetFormula3DAreaReference.GetView: TObject;
begin
  Result := nil;
  if (Link <> nil) and TdxSpreadSheetInvalidObject.IsLive(Link.Data) then
    Result := Link.Data;
  if (Result = nil) and (Link2 <> nil) and TdxSpreadSheetInvalidObject.IsLive(Link2.Data) then
    Result := Link2.Data;
end;

function TdxSpreadSheetFormula3DAreaReference.IsValid: Boolean;
begin
  Result := (View <> nil) and inherited IsValid;
end;

function TdxSpreadSheetFormula3DAreaReference.ReferenceToString: string;
begin
  if R1C1Reference then
  begin
    Result := dxR1C1ReferenceAreaToString(AnchorRow, AnchorColumn,
      LinkToString(Link), FRow, FColumn, LinkToString(Link2), FRow2, FColumn2)
  end
  else
    Result := LinkToString(Link) + ReferenceToString(FRow, FColumn) +
      dxAreaSeparator + LinkToString(Link2) + ReferenceToString(FRow2, FColumn2);
end;

{$ENDREGION}

{$REGION 'Functions'}
{ TdxSpreadSheetFormulaFunctionToken }

constructor TdxSpreadSheetFormulaFunctionToken.Create(AInformation: TdxSpreadSheetFunctionInfo);
begin
  FInformation := AInformation;
  FIsDirtyParamInfo := True;
end;

function TdxSpreadSheetFormulaFunctionToken.CreateArrayCopy(const AArray: TdxSpreadSheetFormulaToken): TdxSpreadSheetFormulaToken;
var
  ADimension: TdxSpreadSheetFormulaTokenDimension;
  AErrorCode: TdxSpreadSheetFormulaErrorCode;
  ARow, AColumn: Integer;
  AValue: Variant;
  AArrayCopy, AArrayItemToken: TdxSpreadSheetFormulaToken;
begin
  AArrayCopy := TdxSpreadSheetFormulaArrayToken.Create;
  ADimension := AArray.GetDimension(AErrorCode);
  for ARow := 0 to ADimension.RowCount - 1 do
    for AColumn := 0 to ADimension.ColumnCount - 1 do
    begin
      if (AColumn = 0) and (ARow > 0) then
        TdxSpreadSheetFormulaToken.AddChild(AArrayCopy, TdxSpreadSheetFormulaArrayRowSeparator.Create());

      AArrayItemToken := TdxSpreadSheetFormulaToken.Create;
      AValue := AArray.GetValueFromArray(ARow, AColumn, AErrorCode);
      TdxSpreadSheetFormulaToken.AddChild(AArrayItemToken, dxSpreadSheetCreateResultToken(AValue, AErrorCode));
      TdxSpreadSheetFormulaToken.AddChild(AArrayCopy, AArrayItemToken);
    end;
  Result := TdxSpreadSheetFormulaToken.Create;
  TdxSpreadSheetFormulaToken.AddChild(Result, AArrayCopy);
  FCalculatedDataList.Add(Result);
end;

function TdxSpreadSheetFormulaFunctionToken.CreateErrorToken(const AErrorCode: TdxSpreadSheetFormulaErrorCode): TdxSpreadSheetFormulaToken;
var
  AErrorToken: TdxSpreadSheetFormulaToken;
begin
  AErrorToken := TdxSpreadSheetFormulaErrorValueToken.Create(AErrorCode);
  Result := TdxSpreadSheetFormulaToken.Create;
  TdxSpreadSheetFormulaToken.AddChild(Result, AErrorToken);
  FCalculatedDataList.Add(Result);
end;

function TdxSpreadSheetFormulaFunctionToken.CreateFakeToken(AParam: TdxSpreadSheetFormulaToken; const AIndex: Integer;
  var ADimension: TdxSpreadSheetFormulaTokenDimension; var AErrorCode: TdxSpreadSheetFormulaErrorCode): TdxSpreadSheetFormulaToken;
begin
  Result := TdxSpreadSheetFormulaToken.Create;
  FFakeParams[AIndex] := AParam;
  ADimension := FFakeParams[AIndex].FirstChild.GetDimension(AErrorCode);
  FCalculatedDataList.Add(Result);
end;

function TdxSpreadSheetFormulaFunctionToken.HasArrayArgument: Boolean;
var
  AToken, AChild: TdxSpreadSheetFormulaToken;
begin
  Result := False;
  AToken := FirstChild;
  while (AToken <> nil) and not Result do
  begin
    AChild := AToken.FirstChild;
    while (AChild <> nil) and not Result do
    begin
      if AChild is TdxSpreadSheetFormulaFunctionToken then
        Result := TdxSpreadSheetFormulaFunctionToken(AChild).HasArrayArgument
      else
        Result := AChild is TdxSpreadSheetFormulaArrayToken;
      AChild := AChild.Next;
    end;
    AToken := AToken.Next;
  end;
end;

procedure TdxSpreadSheetFormulaFunctionToken.Calculate(AResult: TdxSpreadSheetFormulaResult);
var
  ADimension: TdxSpreadSheetFormulaTokenDimension;
  AErrorCode: TdxSpreadSheetFormulaErrorCode;
begin
  if (FInformation = nil) or not Assigned(FInformation.Proc) then
    AResult.SetError(ecName)
  else
    if (FInformation.ResultKind <> frkValue) or (FInformation.ResultKind = frkParamValue) or not(Owner.IsArrayFormula or HasArrayArgument) then
      DoInformationProc(AResult)
    else
      begin
        StoreChildrenOrder;
        FCalculatedDataList := TcxObjectList.Create;
        try
          ADimension := GetDimension(AErrorCode);
          if AErrorCode <> ecNone then
          begin
            ClearTemporaryData;
            AResult.SetError(AErrorCode);
            FInformation.Proc(AResult, FirstChild)
          end
          else
            if (ADimension.RowCount = 1) and (ADimension.ColumnCount = 1) then
            begin
              ClearTemporaryData;
              DoInformationProc(AResult);
            end
            else
              CalculateAsArray(AResult);
        finally
          ClearTemporaryData;
        end;
      end;
end;

procedure TdxSpreadSheetFormulaFunctionToken.CalculateAsArray(AResult: TdxSpreadSheetFormulaResult);
var
  AErrorCode: TdxSpreadSheetFormulaErrorCode;
  ARow, AColumn: Integer;
  AArrayItemFormulaToken: TdxSpreadSheetFormulaToken;
  AIterationResult: TdxSpreadSheetFormulaResult;
  AResultArray: TdxSpreadSheetFormulaArrayToken;
  AArrayToken: TdxSpreadSheetFormulaToken;
  ARepeatingWhenIsDirty: Boolean;
begin
  AResultArray := TdxSpreadSheetFormulaArrayToken.Create;
  AResult.AddTemporary(AResultArray);
  AIterationResult := TdxSpreadSheetFormulaResult.Create(Owner);
  try
    ARepeatingWhenIsDirty := False;
    AArrayToken := nil;
    repeat
      TTokenAccess(AResultArray).IsDirty := False;
      for ARow := 0 to FDimension.RowCount - 1 do
        for AColumn := 0 to FDimension.ColumnCount - 1 do
        begin
          if (AColumn = 0) and (ARow > 0) then
            if not ARepeatingWhenIsDirty then
              TdxSpreadSheetFormulaToken.AddChild(AResultArray, TdxSpreadSheetFormulaArrayRowSeparator.Create())
            else
              AArrayToken := AArrayToken.Next;

          PopulateFakeTokensByChildren(ARow, AColumn);
          if not ARepeatingWhenIsDirty then
            AArrayItemFormulaToken := TdxSpreadSheetFormulaToken.Create
          else
          begin
            AArrayItemFormulaToken := AArrayToken;
            AArrayItemFormulaToken.FirstChild.Free;
          end;

          FInformation.Proc(AIterationResult, FFirstFakeToken);
          AErrorCode := AIterationResult.ErrorCode;
          TdxSpreadSheetFormulaToken.AddChild(AArrayItemFormulaToken, dxSpreadSheetCreateResultToken(AIterationResult.Value, AErrorCode));
          if not ARepeatingWhenIsDirty then
            TdxSpreadSheetFormulaToken.AddChild(AResultArray, AArrayItemFormulaToken);
          DestroyFakeTokensChildren(AIterationResult);
          AIterationResult.Clear;
          if ARepeatingWhenIsDirty then
            AArrayToken := AArrayToken.Next;
        end;
      TTokenAccess(AResultArray).ClearIsDimensionCalculated;
      ARepeatingWhenIsDirty := not ARepeatingWhenIsDirty and TTokenAccess(AResultArray).IsDirty;
      if ARepeatingWhenIsDirty then
        AArrayToken := AResultArray.FirstChild;
    until not ARepeatingWhenIsDirty;
  finally
    AIterationResult.Free;
  end;
end;

procedure TdxSpreadSheetFormulaFunctionToken.CalculateDimension(
  var ADimension: TdxSpreadSheetFormulaTokenDimension; var AErrorCode: TdxSpreadSheetFormulaErrorCode);
var
  ACurrentParam: TdxSpreadSheetFormulaToken;
  AFakeToken: TdxSpreadSheetFormulaToken;
  AParamDimension: TdxSpreadSheetFormulaTokenDimension;
  APriorToken: TdxSpreadSheetFormulaToken;
  I: Integer;
begin
  inherited CalculateDimension(ADimension, AErrorCode);
  FFirstFakeToken := nil;
  APriorToken := nil;
  ACurrentParam := FirstChild;
  I := 0;
  while ACurrentParam <> nil do
  begin
    AParamDimension.SetDimension(1, 1);

    AErrorCode := ecNone;
    AFakeToken := GetFakeToken(I, ACurrentParam, AParamDimension, AErrorCode);

    ADimension.SetDimension(Max(ADimension.RowCount, AParamDimension.RowCount),
      Max(ADimension.ColumnCount, AParamDimension.ColumnCount));

    if APriorToken <> nil then
      TTokenAccess(APriorToken).SetNext(AFakeToken)
    else
      FFirstFakeToken := AFakeToken;

    APriorToken := AFakeToken;
    ACurrentParam := ACurrentParam.Next;
    Inc(I);
  end;
end;

procedure TdxSpreadSheetFormulaFunctionToken.CleanCachedResult;
begin
  FIsCachedResultPresent := False;
end;

procedure TdxSpreadSheetFormulaFunctionToken.CleanFunctionTokenCachedResult;
begin
  CleanCachedResult;
  inherited CleanFunctionTokenCachedResult;
end;

procedure TdxSpreadSheetFormulaFunctionToken.DestroyFakeTokensChildren(var AResult: TdxSpreadSheetFormulaResult);
var
  AOwnership: TStreamOwnership;
  AParam: TdxSpreadSheetFormulaToken;
  I: Integer;
begin
  I := 0;
  AParam := FFirstFakeToken;
  while AParam <> nil do
  begin
    if (FFakeParams[I] <> nil) and (AParam.FirstChild <> nil) then
    begin
      AResult.ExtractToken(AParam.FirstChild, AOwnership);
      AParam.FirstChild.Free;
    end;
    AParam := AParam.Next;
    Inc(I);
  end;
end;

procedure TdxSpreadSheetFormulaFunctionToken.DoCacheResult(AResult: TdxSpreadSheetFormulaResult);
var
  ALastToken: TdxSpreadSheetFormulaToken;
begin
  FIsCachedResultPresent := False;
  if Controller.CalculationInProcess then
  begin
    FIsCachedResultPresent := True;
    FCachedResultErrorCode := AResult.ErrorCode;
    if AResult.Count > 0 then
    begin
      ALastToken := AResult.LastItem;
      if ALastToken.IsEnumeration then
        FIsCachedResultPresent := False
      else
        ALastToken.GetValue(FCachedResultValue, FCachedResultErrorCode);
    end
    else
      FCachedResultValue := Unassigned;
  end;
end;

procedure TdxSpreadSheetFormulaFunctionToken.DoInformationProc(var AResult: TdxSpreadSheetFormulaResult);
begin
  if not GetCachedResult(AResult) then
  begin
    FInformation.Proc(AResult, FirstChild);
    DoCacheResult(AResult);
  end;
end;

function TdxSpreadSheetFormulaFunctionToken.ForEach(AProc: TdxSpreadSheetForEachCallBack; const AData: Pointer; var AErrorCode: TdxSpreadSheetFormulaErrorCode): Boolean;
var
  AResult: TdxSpreadSheetFormulaResult;
  AValue: Variant;
begin
  if Owner = nil then
  begin
    AErrorCode := ecValue;
    Result := False;
  end
  else
  begin
    AResult := TdxSpreadSheetFormulaResult.Create(Owner);
    try
      Calculate(AResult);
      AErrorCode := AResult.ErrorCode;
      if not IsEnumeration then
      begin
        AValue := AResult.Value;
        Result := AProc(AValue, CanConvertStrToNumber, AErrorCode, AData, nil) and (AErrorCode = ecNone);
      end
      else
         Result := TTokenAccess(AResult.LastItem).ForEach(AProc, AData, AErrorCode);
    finally
      AResult.Free;
    end;
  end;
end;

function TdxSpreadSheetFormulaFunctionToken.GetCachedResult(var AResult: TdxSpreadSheetFormulaResult): Boolean;
begin
  Result := FIsCachedResultPresent;
  if not Result then Exit;
  AResult.SetError(FCachedResultErrorCode);
  if FCachedResultErrorCode = ecNone then
    AResult.AddValue(FCachedResultValue);
end;

function TdxSpreadSheetFormulaFunctionToken.GetFakeToken(AIndex: Integer; AParam: TdxSpreadSheetFormulaToken;
  var ADimension: TdxSpreadSheetFormulaTokenDimension; var AErrorCode: TdxSpreadSheetFormulaErrorCode): TdxSpreadSheetFormulaToken;

  function IsArray(const AInnerIndex: Integer; AToken: TdxSpreadSheetFormulaToken): Boolean;
  begin
    Result :=
      IsArrayInsteadValue(AInnerIndex, AToken, TdxSpreadSheetFormulaArrayToken) or
      IsArrayInsteadValue(AInnerIndex, AToken, TdxSpreadSheetFormulaAreaReference) and Owner.IsArrayFormula;
  end;

var
  AParamResultToken: TdxSpreadSheetFormulaToken;
  AParamResult: TdxSpreadSheetFormulaResult;
  ADirtyErrorCode: TdxSpreadSheetFormulaErrorCode;
begin
  Result := AParam;
  if IsExpectedValueParam(AIndex) then
    if IsArray(AIndex, AParam) then
      Result := CreateFakeToken(AParam, AIndex, ADimension, AErrorCode)
    else
      if (AParam.ChildCount > 1) or (AParam.FirstChild is TdxSpreadSheetFormulaFunctionToken) or
        (AParam.FirstChild is TdxSpreadSheetFormulaParenthesesToken) then
      begin
        AParamResult := TFormulaAccess(Owner).Calculate(AParam.FirstChild);
        try
          AErrorCode := AParamResult.ErrorCode;
          if AErrorCode = ecNone then
          begin
            AParamResultToken := AParamResult.LastItem;
            if IsArray(AIndex, AParamResultToken) then
              Result := CreateFakeToken(CreateArrayCopy(AParamResultToken), AIndex, ADimension, AErrorCode)
          end
          else
            Result := CreateFakeToken(CreateErrorToken(AErrorCode), AIndex, ADimension, ADirtyErrorCode);
        finally
          AParamResult.Free;
        end;
      end;
end;

function TdxSpreadSheetFormulaFunctionToken.GetMaxParamCount: Integer;
begin
  if FIsDirtyParamInfo then
    InitializeParamInfo;
  Result := FMaxParamCount;
end;

function TdxSpreadSheetFormulaFunctionToken.GetParamKind: TdxSpreadSheetFunctionParamKindInfo;
begin
  if FIsDirtyParamInfo then
    InitializeParamInfo;
  Result := FParamKind;
end;

function TdxSpreadSheetFormulaFunctionToken.IsArrayInsteadValue(const AIndex: Integer;
  AParam: TdxSpreadSheetFormulaToken; ACheckedClass: TClass): Boolean;
begin
  Result := IsExpectedValueParam(AIndex) and
    ((AParam is ACheckedClass) or ((AParam.ChildCount = 1) and (AParam.FirstChild is ACheckedClass)));
end;

procedure TdxSpreadSheetFormulaFunctionToken.InitializeFakeParams;
var
  I: Integer;
begin
  SetLength(FFakeParams, MaxParamCount);
  for I := 0 to MaxParamCount - 1 do
    FFakeParams[I] := nil;
end;

procedure TdxSpreadSheetFormulaFunctionToken.InitializeParamInfo;
begin
  if Assigned(FInformation.ParamInfo) then
  begin
    FInformation.ParamInfo(FMaxParamCount, FParamKind);
    SpecifyMaxParamCount;
    FIsDirtyParamInfo := False;
    InitializeFakeParams;
  end;
end;

function TdxSpreadSheetFormulaFunctionToken.IsEnumeration: Boolean;
begin
  Result := FInformation.ResultKind = frkArray;
end;

function TdxSpreadSheetFormulaFunctionToken.IsExpectedValueParam(AIndex: Integer): Boolean;
begin
  Result := AIndex < MaxParamCount;
  if Result then
    Result := (ParamKind[AIndex] = fpkValue) or (ParamKind[AIndex] = fpkNonRequiredValue)
  else
    Result := (ParamKind[MaxParamCount - 1] = fpkUnlimited) or (ParamKind[MaxParamCount - 1] = fpkNonRequiredUnlimited);
end;

function TdxSpreadSheetFormulaFunctionToken.NeedAddFeatureFunctionPrefixToFunctionName: Boolean;
begin
  Result := (Information.Token = 255) and (Information.PrefixID > 0) and (Owner.Controller <> nil) and
    TControllerAccess(Owner.Controller).NeedAddFeatureFunctionPrefixToFunctionName;
end;

function TdxSpreadSheetFormulaFunctionToken.NeedForceDimensionCalculating: Boolean;
begin
  Result := True;
end;

procedure TdxSpreadSheetFormulaFunctionToken.PopulateFakeTokensByChildren(const ARow, AColumn: Integer);
var
  AParam: TdxSpreadSheetFormulaToken;
  I: Integer;
  AParamValue: Variant;
  AErrorCode: TdxSpreadSheetFormulaErrorCode;
begin
  I := 0;
  AParam := FFirstFakeToken;
  while AParam <> nil do
  begin
    if FFakeParams[I] <> nil then
    begin
      AParamValue := FFakeParams[I].FirstChild.GetValueFromArray(ARow, AColumn, AErrorCode);
      TdxSpreadSheetFormulaToken.AddChild(AParam, dxSpreadSheetCreateResultToken(AParamValue, AErrorCode));
    end;
    AParam := AParam.Next;
    Inc(I);
  end;
end;

procedure TdxSpreadSheetFormulaFunctionToken.RestoreChildrenOrder;
var
  AChild: TdxSpreadSheetFormulaToken;
  I: Integer;
begin
  AChild := FirstChild;
  I := 1;
  while AChild <> nil do
  begin
    if I < FChildrenOrder.Count then
      TTokenAccess(AChild).SetNext(FChildrenOrder[I])
    else
      TTokenAccess(AChild).SetNext(nil);

    AChild := AChild.Next;
    Inc(I);
  end;
end;

procedure TdxSpreadSheetFormulaFunctionToken.SpecifyMaxParamCount;
var
  I, AOldCount, ANewCount: Integer;
begin
  AOldCount := FMaxParamCount;
  ANewCount := AOldCount;
  for I := 0 to AOldCount - 1 do
    if FParamKind[I] in [fpkUnlimited, fpkNonRequiredUnlimited] then
    begin
      ANewCount := Max(FMaxParamCount, ChildCount);
      Break;
    end;
  if ANewCount > AOldCount then
  begin
    dxSpreadSheetInitializeParamInfo(ANewCount, FMaxParamCount, FParamKind);
    for I := AOldCount + 1 to FMaxParamCount do
      FParamKind[I - 1] := fpkNonRequiredValue;
  end;
end;

procedure TdxSpreadSheetFormulaFunctionToken.StoreChildrenOrder;
var
  AChild: TdxSpreadSheetFormulaToken;
begin
  FChildrenOrder := TList.Create;
  AChild := FirstChild;
  if AChild <> nil then
    FFirstChildParent := AChild.Parent;
  while AChild <> nil do
  begin
    FChildrenOrder.Add(AChild);
    AChild := AChild.Next;
  end;
end;

procedure TdxSpreadSheetFormulaFunctionToken.ClearTemporaryData;
begin
  if FChildrenOrder <> nil then
    RestoreChildrenOrder;
  FreeAndNil(FChildrenOrder);
  FreeAndNil(FCalculatedDataList);
  FFirstFakeToken := nil;
end;

procedure TdxSpreadSheetFormulaFunctionToken.ToString(var AAsText: TdxSpreadSheetFormulaToken);
var
  AName: string;
begin
  AName := '';
  if Information <> nil then
  begin
    if Information.NamePtr <> nil then
      AName := FormatSettings.GetFunctionName(Information.NamePtr)
    else
      AName := Information.Name;

    if NeedAddFeatureFunctionPrefixToFunctionName then
      AName := Information.GetPrefix + AName;
  end;
  AttachString(AAsText, AName + dxLeftParenthesis, ParametersToString, dxRightParenthesis);
end;
{$ENDREGION}

{ TdxSpreadSheetFormulaParenthesesToken }

procedure TdxSpreadSheetFormulaParenthesesToken.Calculate(AResult: TdxSpreadSheetFormulaResult);
begin
  if HasChildren then
    AResult.AddResultValue(TFormulaAccess(Owner).Calculate(FirstChild))
end;

procedure TdxSpreadSheetFormulaParenthesesToken.ToString(var AAsText: TdxSpreadSheetFormulaToken);
begin
  AttachString(AAsText, dxLeftParenthesis, GetExpressionAsText(FirstChild), dxRightParenthesis);
end;

{ TdxSpreadSheetFormulaAttributeToken }

procedure TdxSpreadSheetFormulaAttributeToken.ToString(var AAsText: TdxSpreadSheetFormulaToken);
begin
// todo: need qualification how to processing this token
//  AAsText.AddString(dxOperations[opIsect] + AAsText.ExtractLastTokenAsString);
end;

procedure TdxSpreadSheetFormulaAttributeToken.Calculate(AResult: TdxSpreadSheetFormulaResult);
begin
end;

function TdxSpreadSheetFormulaAttributeToken.GetTokenPriority: Integer;
begin
  Result := -1;
end;

{$REGION 'Unknown Tokens'}

{ TdxSpreadSheetFormulaUnknownNameToken }

constructor TdxSpreadSheetFormulaUnknownNameToken.Create(const AName: string);
begin
  FName := AName;
end;

destructor TdxSpreadSheetFormulaUnknownNameToken.Destroy;
begin
  FreeAndNil(FLink);
  inherited Destroy;
end;

function TdxSpreadSheetFormulaUnknownNameToken.IsEnumeration: Boolean;
begin
  Result := False;
end;

procedure TdxSpreadSheetFormulaUnknownNameToken.Calculate(AResult: TdxSpreadSheetFormulaResult);
begin
  AResult.SetError(ecName);
end;

function TdxSpreadSheetFormulaUnknownNameToken.LinkAsString: string;
begin
  if FLink = nil then
    Result := ''
  else
  begin
    if FormatSettings.ExpandExternalLinks then
      Result := dxStringMarkChar2 + FLink.ToString + dxStringMarkChar2 + dxRefSeparator
    else
      Result := FLink.ToString + dxRefSeparator
  end;
end;

procedure TdxSpreadSheetFormulaUnknownNameToken.SetLink(const Value: TdxSpreadSheet3DReferenceCustomLink);
begin
  inherited SetLink(FLink, Value);
end;

procedure TdxSpreadSheetFormulaUnknownNameToken.ToString(var AAsText: TdxSpreadSheetFormulaToken);
begin
  AttachString(AAsText, LinkAsString + Name);
end;

{ TdxSpreadSheetFormulaUnknownFunctionToken }

procedure TdxSpreadSheetFormulaUnknownFunctionToken.ToString(var AAsText: TdxSpreadSheetFormulaToken);
begin
  AttachString(AAsText, LinkAsString + Name + dxLeftParenthesis, ParametersToString, dxRightParenthesis);
end;

{$ENDREGION}
end.
