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

unit dxSpreadSheetConditionalFormattingRuleEditDialogHelpers;

{$I cxVer.Inc}

interface

uses
  Types, Windows, Classes, Generics.Defaults, Generics.Collections, Graphics,
  cxGraphics, cxDropDownEdit, cxMaskEdit, cxTL, cxColorComboBox, cxImageComboBox, cxEdit,
  dxSpreadSheetConditionalFormatting, dxSpreadSheetConditionalFormattingRules, dxSpreadSheetConditionalFormattingIconSet,
  dxSpreadSheetStyles, dxSpreadSheetCoreFormulas, dxSpreadSheetCoreDialogsStrs, dxSpreadSheetCoreStrs;

type
  TdxSpreadSheetConditionalFormattingRuleType = (cfrtCellIs, cfrtExpression, cfrtDuplicateValues,
    cfrtUniqueValues, cfrtAboveOrBelowAverage, cfrtTopBottomValues, cfrtTwoColorScale, cfrtThreeColorScale,
    cfrtIconSet, cfrtDataBar);

  TdxSpreadSheetConditionalFormattingRuleEditDialogHelper = class;

  { TdxSpreadSheetConditionalFormattingRuleEditDialogRuleInfo }

  TdxSpreadSheetConditionalFormattingRuleEditDialogRuleInfo = class
  strict private
    FHelper: TdxSpreadSheetConditionalFormattingRuleEditDialogHelper;

    function GetConditionalFormatting: TdxSpreadSheetCustomConditionalFormatting;
    function GetDescription: string;
    function GetName: string;
    function GetRuleClass: TdxSpreadSheetCustomConditionalFormattingRuleClass;
    function GetRulePreview: TdxSpreadSheetCustomConditionalFormattingRule;
  protected
    FRulePreview: TdxSpreadSheetCustomConditionalFormattingRule;
    FRuleType: TdxSpreadSheetConditionalFormattingRuleType;

    property ConditionalFormatting: TdxSpreadSheetCustomConditionalFormatting read GetConditionalFormatting;
  public
    constructor Create(
      AHelper: TdxSpreadSheetConditionalFormattingRuleEditDialogHelper;
      ARuleType: TdxSpreadSheetConditionalFormattingRuleType);
    destructor Destroy; override;
    procedure DrawPreview(ACanvas: TcxCanvas; const R: TRect);
    function GetStyle(out AStyle: TdxSpreadSheetCellStyle): Boolean;
    //
    property Description: string read GetDescription;
    property Name: string read GetName;
    property RuleClass: TdxSpreadSheetCustomConditionalFormattingRuleClass read GetRuleClass;
    property RulePreview: TdxSpreadSheetCustomConditionalFormattingRule read GetRulePreview;
    property RuleType: TdxSpreadSheetConditionalFormattingRuleType read FRuleType;
  end;

  { TdxSpreadSheetConditionalFormattingRuleEditDialogHelper }

  TdxSpreadSheetConditionalFormattingRuleEditDialogHelper = class
  strict private
    FCancelChanges: Boolean;
    FConditionalFormatting: TdxSpreadSheetCustomConditionalFormatting;
    FRuleInfos: TObjectList<TdxSpreadSheetConditionalFormattingRuleEditDialogRuleInfo>;
    FSelectedArea: TRect;
    FSelectedRuleInfo: TdxSpreadSheetConditionalFormattingRuleEditDialogRuleInfo;

    function GetSelectedRulePreview: TdxSpreadSheetCustomConditionalFormattingRule;
    procedure SetSelectedRuleInfo(AValue: TdxSpreadSheetConditionalFormattingRuleEditDialogRuleInfo);
  protected
    function CanValidateExpressionResult: Boolean; virtual;

    procedure BeginEditing;
    procedure EndEditing(ACancel: Boolean);
    procedure PopulateRulesInfo;
  public
    constructor Create(AConditionalFormatting: TdxSpreadSheetCustomConditionalFormatting);
    destructor Destroy; override;
    procedure BeforeDestruction; override;
    procedure SaveChanges(var ARule: TdxSpreadSheetCustomConditionalFormattingRule);

    // Validation
    function AreExpressionsValid(ARule: TdxSpreadSheetConditionalFormattingRuleCellIs): Boolean; overload;
    function AreExpressionsValid(ARule: TdxSpreadSheetConditionalFormattingRuleExpression): Boolean; overload;
    function IsExpressionValid(AExpression: TdxSpreadSheetCustomFormula): Boolean;

    property RuleInfos: TObjectList<TdxSpreadSheetConditionalFormattingRuleEditDialogRuleInfo> read FRuleInfos;
    property SelectedArea: TRect read FSelectedArea write FSelectedArea;
    property SelectedRuleInfo: TdxSpreadSheetConditionalFormattingRuleEditDialogRuleInfo read FSelectedRuleInfo write SetSelectedRuleInfo;
    property SelectedRulePreview: TdxSpreadSheetCustomConditionalFormattingRule read GetSelectedRulePreview;
    property ConditionalFormatting: TdxSpreadSheetCustomConditionalFormatting read FConditionalFormatting;
  end;

  { TdxSpreadSheetConditionalFormattingRuleEditDialogScaleHelper }

  TdxSpreadSheetConditionalFormattingRuleEditDialogScaleHelper = record
    EditorColor: array[0..2] of TcxColorComboBox;
    EditorType: array[0..2] of TcxComboBox;
    EditorValue: array[0..2] of TcxMaskEdit;
    Stops: array[0..2] of TdxSpreadSheetConditionalFormattingRuleCustomScaleStop;

    function AreActualValuesInValidOrder: Boolean;
    procedure BindEditors(ATypeEditor: TcxComboBox; AValueEditor: TcxMaskEdit; AColorEditor: TcxColorComboBox);
    procedure Initialize(ARule: TdxSpreadSheetConditionalFormattingRuleCustomScale);
    procedure Reset;

    function GetStopColor(AStop: TdxSpreadSheetConditionalFormattingRuleCustomScaleStop): TColor;
    procedure SetStopColor(AIndex: Integer; AColor: TColor); overload;
    procedure SetStopColor(AStop: TdxSpreadSheetConditionalFormattingRuleCustomScaleStop; AColor: TColor); overload;
  end;

  { TdxSpreadSheetConditionalFormattingRuleEditDialogIconSetHelper }

  TdxSpreadSheetConditionalFormattingRuleEditDialogIconSetHelper = class
  strict private
    FRule: TdxSpreadSheetConditionalFormattingRuleIconSet;
    FTreeList: TcxTreeList;

    function GetIcons: TcxImageList;
    function GetPresets: TdxSpreadSheetConditionalFormattingIconSetPresets;
    function GetValueFromComboBox(ANode: TcxTreeListNode; AColumn: TcxTreeListColumn): TObject;
    procedure LoadStop(ANode: TcxTreeListNode; AStop: TdxSpreadSheetConditionalFormattingRuleIconSetStop);
  public
    constructor Create(ATreeList: TcxTreeList);
    function AreActualValuesInValidOrder: Boolean;
    function GetIconSetIndex(AItems: TcxImageComboBoxItems; const APresetName: string): Integer;
    function GetStopType(ANode: TcxTreeListNode; AValueTypeColumn: TcxTreeListColumn): TdxSpreadSheetConditionalFormattingRuleCustomScaleStopValueType;
    procedure Load(ARule: TdxSpreadSheetConditionalFormattingRuleIconSet);

    procedure PopulateComparisonOperators(AProperties: TcxComboBoxProperties);
    procedure PopulateIcons(AProperties: TcxImageComboBoxProperties);
    procedure PopulateIconSets(AIconSets: TcxImageComboBox);
    procedure PopulateStopValueTypes(AProperties: TcxComboBoxProperties);

    procedure SetComparisonOperator(ANode: TcxTreeListNode; AColumn: TcxTreeListColumn);
    procedure SetIconIndex(ANode: TcxTreeListNode; AColumn: TcxTreeListColumn);
    procedure SetIconSet(ASetIndex: Integer);
    procedure SetReverseIconOrder(AReverseIconOrder: Boolean);
    procedure SetShowIconsOnly(AShowIconsOnly: Boolean);
    procedure SetStopValue(ANode: TcxTreeListNode; const AValue: Variant);
    procedure SetStopValueType(ANode: TcxTreeListNode;
      AValueType: TdxSpreadSheetConditionalFormattingRuleCustomScaleStopValueType);

    procedure UpdateRowInfo;
    //
    property Icons: TcxImageList read GetIcons;
    property Presets: TdxSpreadSheetConditionalFormattingIconSetPresets read GetPresets;
    property TreeList: TcxTreeList read FTreeList;
  end;

  { TdxSpreadSheetConditionalFormattingRuleEditDialogUIHelper }

  TdxSpreadSheetConditionalFormattingRuleEditDialogUIHelper = class
  public
    class function FindRuleType(ATreeList: TcxTreeList; AClass: TClass; out ANode: TcxTreeListNode): Boolean;
    class procedure PopulateCellIsOperators(AItems: TStrings);
    class procedure PopulateRuleTypes(ATreeList: TcxTreeList; AHelper: TdxSpreadSheetConditionalFormattingRuleEditDialogHelper);
    class procedure PopulateTopBottomDirections(AItems: TStrings);

    // AboveOrBelowAverageOperators
    class procedure LoadAboveOrBelowAverageRuleInfo(ACombobox: TcxComboBox;
      ARule: TdxSpreadSheetConditionalFormattingRuleAboveOrBelowAverage);
    class procedure SaveAboveOrBelowAverageRuleInfo(ACombobox: TcxComboBox;
      ARule: TdxSpreadSheetConditionalFormattingRuleAboveOrBelowAverage);
    class procedure PopulateAboveOrBelowAverageOperators(AItems: TStrings);

    // Scales
    class function GetScaleStopValueType(ACombobox: TcxComboBox): TdxSpreadSheetConditionalFormattingRuleCustomScaleStopValueType;
    class procedure PopulateScaleStopTypes(ACombobox: TcxComboBox); overload;
    class procedure PopulateScaleStopTypes(AItems: TStrings; AStopIndex: Integer); overload;
  end;

implementation

uses
  SysUtils, dxCore, cxGeometry, Math, cxFormats, cxVariants, dxSpreadSheetFormatCellsDialog, dxSpreadSheetFormulas,
  dxSpreadSheetTypes, dxCoreClasses;

const
  RuleClasses: array[TdxSpreadSheetConditionalFormattingRuleType] of TdxSpreadSheetCustomConditionalFormattingRuleClass = (
    TdxSpreadSheetConditionalFormattingRuleCellIs,
    TdxSpreadSheetConditionalFormattingRuleExpression,
    TdxSpreadSheetConditionalFormattingRuleDuplicateValues,
    TdxSpreadSheetConditionalFormattingRuleUniqueValues,
    TdxSpreadSheetConditionalFormattingRuleAboveOrBelowAverage,
    TdxSpreadSheetConditionalFormattingRuleTopBottomValues,
    TdxSpreadSheetConditionalFormattingRuleTwoColorScale,
    TdxSpreadSheetConditionalFormattingRuleThreeColorScale,
    TdxSpreadSheetConditionalFormattingRuleIconSet,
    TdxSpreadSheetConditionalFormattingRuleDataBar
  );

type
  TdxSpreadSheetCustomConditionalFormattingAccess = class(TdxSpreadSheetCustomConditionalFormatting);
  TdxSpreadSheetConditionalFormattingRuleExpressionAccess = class(TdxSpreadSheetConditionalFormattingRuleExpression);
  TdxSpreadSheetCustomConditionalFormattingRuleAccess = class(TdxSpreadSheetCustomConditionalFormattingRule);

function GetScaleStopValueTypeName(AType: TdxSpreadSheetConditionalFormattingRuleCustomScaleStopValueType): string;
var
  AMessagePtr: Pointer;
begin
  AMessagePtr := nil;
  case AType of
    cssvtValue:
      AMessagePtr := @sdxConditionalFormattingRuleEditDialogScaleValueTypeValue;
    cssvtPercent:
      AMessagePtr := @sdxConditionalFormattingRuleEditDialogScaleValueTypePercent;
    cssvtFormula:
      AMessagePtr := @sdxConditionalFormattingRuleEditDialogScaleValueTypeFormula;
    cssvtPercentile:
      AMessagePtr := @sdxConditionalFormattingRuleEditDialogScaleValueTypePercentile;
  end;
  Result := cxGetResourceString(AMessagePtr);
end;

function GetCellIsRuleOperatorName(AOperator: TdxSpreadSheetConditionalFormattingRuleCellIsComparisonOperator): string;
var
  AMessagePtr: Pointer;
begin
  AMessagePtr := nil;
  case AOperator of
    cicoBetween:
      AMessagePtr := @sdxConditionalFormattingCellIsRuleComparisonOperatorBetween;
    cicoEqual:
      AMessagePtr := @sdxConditionalFormattingCellIsRuleComparisonOperatorEqual;
    cicoGreaterThan:
      AMessagePtr := @sdxConditionalFormattingCellIsRuleComparisonOperatorGreaterThan;
    cicoGreaterThanOrEqual:
      AMessagePtr := @sdxConditionalFormattingCellIsRuleComparisonOperatorGreaterThanOrEqual;
    cicoLessThan:
      AMessagePtr := @sdxConditionalFormattingCellIsRuleComparisonOperatorLessThan;
    cicoLessThanOrEqual:
      AMessagePtr := @sdxConditionalFormattingCellIsRuleComparisonOperatorLessThanOrEqual;
    cicoNotBetween:
      AMessagePtr := @sdxConditionalFormattingCellIsRuleComparisonOperatorNotBetween;
    cicoNotEqual:
      AMessagePtr := @sdxConditionalFormattingCellIsRuleComparisonOperatorNotEqual;
  end;
  Result := cxGetResourceString(AMessagePtr);
end;

{ TdxSpreadSheetConditionalFormattingRuleEditDialogRuleInfo }

constructor TdxSpreadSheetConditionalFormattingRuleEditDialogRuleInfo.Create(
  AHelper: TdxSpreadSheetConditionalFormattingRuleEditDialogHelper; ARuleType: TdxSpreadSheetConditionalFormattingRuleType);
begin
  inherited Create;
  FHelper := AHelper;
  FRuleType := ARuleType;
end;

destructor TdxSpreadSheetConditionalFormattingRuleEditDialogRuleInfo.Destroy;
begin
  FreeAndNil(FRulePreview);
  inherited Destroy;
end;

procedure TdxSpreadSheetConditionalFormattingRuleEditDialogRuleInfo.DrawPreview(ACanvas: TcxCanvas; const R: TRect);
begin
  TdxSpreadSheetCustomConditionalFormattingRuleAccess(RulePreview).DrawPreview(ACanvas, R);
end;

function TdxSpreadSheetConditionalFormattingRuleEditDialogRuleInfo.GetStyle(out AStyle: TdxSpreadSheetCellStyle): Boolean;
begin
  Result := RulePreview is TdxSpreadSheetConditionalFormattingRuleStyleBased;
  if Result then
    AStyle := TdxSpreadSheetConditionalFormattingRuleStyleBased(RulePreview).Style;
end;

function TdxSpreadSheetConditionalFormattingRuleEditDialogRuleInfo.GetConditionalFormatting: TdxSpreadSheetCustomConditionalFormatting;
begin
  Result := FHelper.ConditionalFormatting;
end;

function TdxSpreadSheetConditionalFormattingRuleEditDialogRuleInfo.GetDescription: string;
var
  AMessagePtr: Pointer;
begin
  AMessagePtr := nil;
  case RuleType of
    cfrtCellIs:
      AMessagePtr := @sdxConditionalFormattingRuleEditDialogRuleDescriptionCellIs;
    cfrtExpression:
      AMessagePtr := @sdxConditionalFormattingRuleEditDialogRuleDescriptionExpression;
    cfrtAboveOrBelowAverage:
      AMessagePtr := @sdxConditionalFormattingRuleEditDialogRuleDescriptionAboveOrBelowAverage;
    cfrtTopBottomValues:
      AMessagePtr := @sdxConditionalFormattingRuleEditDialogRuleDescriptionTopBottomValues;
    cfrtIconSet:
      AMessagePtr := @sdxConditionalFormattingRuleEditDialogRuleDescriptionIconSet;
  end;
  Result := cxGetResourceString(AMessagePtr);
end;

function TdxSpreadSheetConditionalFormattingRuleEditDialogRuleInfo.GetName: string;
var
  AMessagePtr: Pointer;
begin
  AMessagePtr := nil;
  case FRuleType of
    cfrtCellIs:
      AMessagePtr := @sdxConditionalFormattingRuleEditDialogRuleNameCellIs;
    cfrtExpression:
      AMessagePtr := @sdxConditionalFormattingRuleEditDialogRuleNameExpression;
    cfrtDuplicateValues:
      AMessagePtr := @sdxConditionalFormattingRuleEditDialogRuleNameDuplicateValues;
    cfrtUniqueValues:
      AMessagePtr := @sdxConditionalFormattingRuleEditDialogRuleNameUniqueValues;
    cfrtAboveOrBelowAverage:
      AMessagePtr := @sdxConditionalFormattingRuleEditDialogRuleNameAboveOrBelowAverage;
    cfrtTopBottomValues:
      AMessagePtr := @sdxConditionalFormattingRuleEditDialogRuleNameTopBottomValues;
    cfrtTwoColorScale:
      AMessagePtr := @sdxConditionalFormattingRuleEditDialogRuleNameTwoColorScale;
    cfrtThreeColorScale:
      AMessagePtr := @sdxConditionalFormattingRuleEditDialogRuleNameThreeColorScale;
    cfrtIconSet:
      AMessagePtr := @sdxConditionalFormattingRuleEditDialogRuleNameIconSet;
    cfrtDataBar:
      AMessagePtr := @sdxConditionalFormattingRuleEditDialogRuleNameDataBar;
  end;
  Result := cxGetResourceString(AMessagePtr);
end;

function TdxSpreadSheetConditionalFormattingRuleEditDialogRuleInfo.GetRuleClass: TdxSpreadSheetCustomConditionalFormattingRuleClass;
begin
  Result := RuleClasses[RuleType];
end;

function TdxSpreadSheetConditionalFormattingRuleEditDialogRuleInfo.GetRulePreview: TdxSpreadSheetCustomConditionalFormattingRule;
begin
  if FRulePreview = nil then
  begin
    FRulePreview := RuleClass.Create(ConditionalFormatting);
    FRulePreview.Area := FHelper.SelectedArea;
  end;
  Result := FRulePreview;
end;

{ TdxSpreadSheetConditionalFormattingRuleEditDialogHelper }

constructor TdxSpreadSheetConditionalFormattingRuleEditDialogHelper.Create(AConditionalFormatting: TdxSpreadSheetCustomConditionalFormatting);
begin
  inherited Create;
  FConditionalFormatting := AConditionalFormatting;
  FSelectedArea := AConditionalFormatting.Owner.GetSelectionArea;
  FRuleInfos := TObjectList<TdxSpreadSheetConditionalFormattingRuleEditDialogRuleInfo>.Create;
  FCancelChanges := True;
  PopulateRulesInfo;
  BeginEditing;
end;

destructor TdxSpreadSheetConditionalFormattingRuleEditDialogHelper.Destroy;
begin
  EndEditing(FCancelChanges);
  FreeAndNil(FRuleInfos);
  inherited Destroy;
end;

procedure TdxSpreadSheetConditionalFormattingRuleEditDialogHelper.BeforeDestruction;
begin
  inherited BeforeDestruction;
  RuleInfos.Clear;
end;

procedure TdxSpreadSheetConditionalFormattingRuleEditDialogHelper.SaveChanges(
  var ARule: TdxSpreadSheetCustomConditionalFormattingRule);
var
  AAreas: TdxSpreadSheetAreaList;
begin
  if SelectedRuleInfo = nil then
    Exit;

  AAreas := TdxSpreadSheetAreaList.Create;
  try
    if ARule <> nil then
      AAreas.Assign(ARule.Areas)
    else
      AAreas.Add(ConditionalFormatting.Owner.GetSelectionArea);

    if (ARule = nil) or (ARule.ClassType <> SelectedRulePreview.ClassType) then
    begin
      FreeAndNil(ARule);
      ARule := TdxSpreadSheetCustomConditionalFormattingRuleClass(SelectedRulePreview.ClassType).Create(ConditionalFormatting);
    end;

    ARule.Assign(SelectedRulePreview);
    ARule.Areas := AAreas;
  finally
    AAreas.Free;
  end;
  FCancelChanges := False;
end;

function TdxSpreadSheetConditionalFormattingRuleEditDialogHelper.AreExpressionsValid(
  ARule: TdxSpreadSheetConditionalFormattingRuleCellIs): Boolean;
begin
  Result := AreExpressionsValid(TdxSpreadSheetConditionalFormattingRuleExpression(ARule));
  if Result and (ARule.ComparisonOperator in [cicoBetween, cicoNotBetween]) then
    Result := IsExpressionValid(TdxSpreadSheetConditionalFormattingRuleExpressionAccess(ARule).Formulas[1]);
end;

function TdxSpreadSheetConditionalFormattingRuleEditDialogHelper.AreExpressionsValid(
  ARule: TdxSpreadSheetConditionalFormattingRuleExpression): Boolean;
begin
  Result := IsExpressionValid(TdxSpreadSheetConditionalFormattingRuleExpressionAccess(ARule).Formulas[0]);
end;

function TdxSpreadSheetConditionalFormattingRuleEditDialogHelper.IsExpressionValid(AExpression: TdxSpreadSheetCustomFormula): Boolean;
var
  AResultValue: TdxSpreadSheetFormulaResult;
begin
  Result := (AExpression <> nil) and not AExpression.IsArrayFormula;
  if Result and CanValidateExpressionResult then
  begin
    AResultValue := AExpression.ResultValue;
    Result := (AResultValue <> nil) and AResultValue.Validate
  end;
end;

procedure TdxSpreadSheetConditionalFormattingRuleEditDialogHelper.SetSelectedRuleInfo(
  AValue: TdxSpreadSheetConditionalFormattingRuleEditDialogRuleInfo);
var
  ATargetStyle, ASourceStyle: TdxSpreadSheetCellStyle;
begin
  if FSelectedRuleInfo <> AValue then
  begin
    if (FSelectedRuleInfo <> nil) and (AValue <> nil) and (AValue.FRulePreview = nil) then
    begin
      if AValue.GetStyle(ATargetStyle) and FSelectedRuleInfo.GetStyle(ASourceStyle) then
        ATargetStyle.Assign(ASourceStyle);
    end;
    FSelectedRuleInfo := AValue;
  end;
end;

procedure TdxSpreadSheetConditionalFormattingRuleEditDialogHelper.BeginEditing;
begin
  ConditionalFormatting.BeginEditing;
end;

function TdxSpreadSheetConditionalFormattingRuleEditDialogHelper.CanValidateExpressionResult: Boolean;
begin
  Result := TdxSpreadSheetCustomConditionalFormattingAccess(ConditionalFormatting).CanValidateExpressionRuleResultValue;
end;

procedure TdxSpreadSheetConditionalFormattingRuleEditDialogHelper.EndEditing(ACancel: Boolean);
begin
  ConditionalFormatting.EndEditing(ACancel);
end;

procedure TdxSpreadSheetConditionalFormattingRuleEditDialogHelper.PopulateRulesInfo;
var
  AType: TdxSpreadSheetConditionalFormattingRuleType;
begin
  for AType := Low(TdxSpreadSheetConditionalFormattingRuleType) to High(TdxSpreadSheetConditionalFormattingRuleType) do
    RuleInfos.Add(TdxSpreadSheetConditionalFormattingRuleEditDialogRuleInfo.Create(Self, AType));
end;

function TdxSpreadSheetConditionalFormattingRuleEditDialogHelper.GetSelectedRulePreview: TdxSpreadSheetCustomConditionalFormattingRule;
begin
  Result := SelectedRuleInfo.RulePreview;
end;

{ TdxSpreadSheetConditionalFormattingRuleEditDialogScaleHelper }

function TdxSpreadSheetConditionalFormattingRuleEditDialogScaleHelper.AreActualValuesInValidOrder: Boolean;

  function CanProcessStop(AStop: TdxSpreadSheetConditionalFormattingRuleCustomScaleStop): Boolean;
  begin
    Result := (AStop <> nil) and not (AStop.ValueType in [cssvtLimitValue, cssvtFormula]);
  end;

var
  AIndex: Integer;
  AValue: Double;
begin
  Result := True;
  AIndex := Low(Stops);
  while (AIndex <= High(Stops)) and not CanProcessStop(Stops[AIndex]) do
    Inc(AIndex);
  if AIndex <= High(Stops) then
  begin
    AValue := Stops[AIndex].ActualValue;
    while AIndex <= High(Stops) do
    begin
      if CanProcessStop(Stops[AIndex]) then
      begin
        if Stops[AIndex].ActualValue < AValue then
          Exit(False);
        AValue := Stops[AIndex].ActualValue;
      end;
      Inc(AIndex);
    end;
  end;
end;

procedure TdxSpreadSheetConditionalFormattingRuleEditDialogScaleHelper.BindEditors(
  ATypeEditor: TcxComboBox; AValueEditor: TcxMaskEdit; AColorEditor: TcxColorComboBox);
begin
  EditorColor[AColorEditor.Tag] := AColorEditor;
  EditorType[ATypeEditor.Tag] := ATypeEditor;
  EditorValue[AValueEditor.Tag] := AValueEditor;
end;

procedure TdxSpreadSheetConditionalFormattingRuleEditDialogScaleHelper.Initialize(
  ARule: TdxSpreadSheetConditionalFormattingRuleCustomScale);
var
  AStop: TdxSpreadSheetConditionalFormattingRuleCustomScaleStop;
  I: Integer;
begin
  Reset;

  if ARule is TdxSpreadSheetConditionalFormattingRuleDataBar then
  begin
    Stops[0] := TdxSpreadSheetConditionalFormattingRuleDataBar(ARule).MinValue;
    Stops[2] := TdxSpreadSheetConditionalFormattingRuleDataBar(ARule).MaxValue;
  end
  else

  if ARule is TdxSpreadSheetConditionalFormattingRuleTwoColorScale then
  begin
    Stops[0] := TdxSpreadSheetConditionalFormattingRuleTwoColorScale(ARule).MinValue;
    Stops[2] := TdxSpreadSheetConditionalFormattingRuleTwoColorScale(ARule).MaxValue;
  end
  else

  if ARule is TdxSpreadSheetConditionalFormattingRuleThreeColorScale then
  begin
    Stops[0] := TdxSpreadSheetConditionalFormattingRuleThreeColorScale(ARule).MinValue;
    Stops[1] := TdxSpreadSheetConditionalFormattingRuleThreeColorScale(ARule).MiddleValue;
    Stops[2] := TdxSpreadSheetConditionalFormattingRuleThreeColorScale(ARule).MaxValue;
  end;

  for I := Low(Stops) to High(Stops) do
  begin
    AStop := Stops[I];
    if AStop <> nil then
    begin
      EditorType[I].ItemIndex := Max(0, EditorType[I].Properties.Items.IndexOfObject(TObject(AStop.ValueType)));
      EditorValue[I].EditValue := AStop.Value;
      EditorColor[I].ColorValue := GetStopColor(AStop);
    end;
  end;
end;

procedure TdxSpreadSheetConditionalFormattingRuleEditDialogScaleHelper.Reset;
var
  I: Integer;
begin
  for I := Low(Stops) to High(Stops) do
    Stops[I] := nil;
end;

function TdxSpreadSheetConditionalFormattingRuleEditDialogScaleHelper.GetStopColor(
  AStop: TdxSpreadSheetConditionalFormattingRuleCustomScaleStop): TColor;
begin
  if AStop is TdxSpreadSheetConditionalFormattingRuleColorScaleStop then
    Result := TdxSpreadSheetConditionalFormattingRuleColorScaleStop(AStop).Color
  else
    Result := clDefault;
end;

procedure TdxSpreadSheetConditionalFormattingRuleEditDialogScaleHelper.SetStopColor(AIndex: Integer; AColor: TColor);
begin
  SetStopColor(Stops[AIndex], AColor);
end;

procedure TdxSpreadSheetConditionalFormattingRuleEditDialogScaleHelper.SetStopColor(
  AStop: TdxSpreadSheetConditionalFormattingRuleCustomScaleStop; AColor: TColor);
begin
  if AStop is TdxSpreadSheetConditionalFormattingRuleColorScaleStop then
    TdxSpreadSheetConditionalFormattingRuleColorScaleStop(AStop).Color := AColor;
end;

{ TdxSpreadSheetConditionalFormattingRuleEditDialogIconSetHelper }

constructor TdxSpreadSheetConditionalFormattingRuleEditDialogIconSetHelper.Create(ATreeList: TcxTreeList);
begin
  inherited Create;
  FTreeList := ATreeList;
end;

function TdxSpreadSheetConditionalFormattingRuleEditDialogIconSetHelper.AreActualValuesInValidOrder: Boolean;

  function CanProcessStop(AStop: TdxSpreadSheetConditionalFormattingRuleIconSetStop): Boolean;
  begin
    Result := (AStop <> nil) and not (AStop.ValueType in [cssvtLimitValue, cssvtFormula]);
  end;

var
  AIndex: Integer;
  AValue: Double;
begin
  Result := True;
  AIndex := 1;
  while (AIndex < FRule.StopCount) and not CanProcessStop(FRule.Stops[AIndex]) do
    Inc(AIndex);
  if AIndex < FRule.StopCount then
  begin
    AValue := FRule.Stops[AIndex].ActualValue;
    while AIndex < FRule.StopCount do
    begin
      if CanProcessStop(FRule.Stops[AIndex]) then
      begin
        if FRule.Stops[AIndex].ActualValue < AValue then
          Exit(False);
        AValue := FRule.Stops[AIndex].ActualValue;
      end;
      Inc(AIndex);
    end;
  end;
end;

function TdxSpreadSheetConditionalFormattingRuleEditDialogIconSetHelper.GetIconSetIndex(
  AItems: TcxImageComboBoxItems; const APresetName: string): Integer;
var
  APreset: TdxSpreadSheetConditionalFormattingIconSetPreset;
  I: Integer;
begin
  Result := -1;
  if ConditionalFormattingIconSet.Presets.FindByName(APresetName, APreset) then
    for I := 0 to AItems.Count - 1 do
      if AItems[I].Tag = NativeInt(APreset) then
      begin
        Result := I;
        Break;
      end;
end;

function TdxSpreadSheetConditionalFormattingRuleEditDialogIconSetHelper.GetStopType(
  ANode: TcxTreeListNode; AValueTypeColumn: TcxTreeListColumn): TdxSpreadSheetConditionalFormattingRuleCustomScaleStopValueType;
begin
  Result := TdxSpreadSheetConditionalFormattingRuleCustomScaleStopValueType(GetValueFromComboBox(ANode, AValueTypeColumn));
end;

procedure TdxSpreadSheetConditionalFormattingRuleEditDialogIconSetHelper.Load(ARule: TdxSpreadSheetConditionalFormattingRuleIconSet);
var
  I: Integer;
begin
  FRule := ARule;
  TreeList.BeginUpdate;
  try
    TreeList.Clear;
    for I := ARule.StopCount - 1 downto 0 do
      LoadStop(TreeList.Add, ARule.Stops[I]);
    UpdateRowInfo;
  finally
    TreeList.EndUpdate;
  end;
end;

procedure TdxSpreadSheetConditionalFormattingRuleEditDialogIconSetHelper.PopulateComparisonOperators(AProperties: TcxComboBoxProperties);
begin
  AProperties.Items.BeginUpdate;
  try
    AProperties.Items.Clear;
    AProperties.Items.AddObject('>=', TObject(isscoGreaterThanOrEqual));
    AProperties.Items.AddObject('>', TObject(isscoGreaterThan));
  finally
    AProperties.Items.EndUpdate;
  end;
end;

procedure TdxSpreadSheetConditionalFormattingRuleEditDialogIconSetHelper.PopulateIcons(AProperties: TcxImageComboBoxProperties);
var
  AItem: TcxImageComboBoxItem;
  I: Integer;
begin
  AProperties.Items.BeginUpdate;
  try
    AProperties.Images := ConditionalFormattingIconSet.Icons;
    AProperties.Items.Clear;
    for I := -1 to ConditionalFormattingIconSet.Icons.Count - 1 do
    begin
      AItem := AProperties.Items.Add;
      AItem.ImageIndex := I;
      AItem.Value := I;
    end;
  finally
    AProperties.Items.EndUpdate;
  end;
end;

procedure TdxSpreadSheetConditionalFormattingRuleEditDialogIconSetHelper.PopulateIconSets(AIconSets: TcxImageComboBox);
var
  AImageIndex: Integer;
  AItem: TcxImageComboBoxItem;
  APreset: TdxSpreadSheetConditionalFormattingIconSetPreset;
  I: Integer;
begin
  AIconSets.Properties.Items.BeginUpdate;
  try
    AImageIndex := 0;
    AIconSets.Properties.Items.Clear;
    for I := 0 to Presets.Count - 1 do
    begin
      APreset := Presets[I];
      if APreset.IndexOf(-1) < 0 then
      begin
        AItem := AIconSets.Properties.Items.Add;
        AItem.ImageIndex := AImageIndex;
        AItem.Tag := NativeInt(APreset);
        AItem.Value := I;
        Inc(AImageIndex);
      end;
    end;
  finally
    AIconSets.Properties.Items.EndUpdate;
  end;
end;

procedure TdxSpreadSheetConditionalFormattingRuleEditDialogIconSetHelper.PopulateStopValueTypes(
  AProperties: TcxComboBoxProperties);
var
  AType: TdxSpreadSheetConditionalFormattingRuleCustomScaleStopValueType;
begin
  AProperties.Items.BeginUpdate;
  try
    AProperties.Items.Clear;
    for AType := Low(AType) to High(AType) do
    begin
      if AType <> cssvtLimitValue then
        AProperties.Items.AddObject(GetScaleStopValueTypeName(AType), TObject(AType));
    end;
  finally
    AProperties.Items.EndUpdate;
  end;
end;

procedure TdxSpreadSheetConditionalFormattingRuleEditDialogIconSetHelper.SetComparisonOperator(
  ANode: TcxTreeListNode; AColumn: TcxTreeListColumn);
var
  AStop: TdxSpreadSheetConditionalFormattingRuleIconSetStop;
begin
  AStop := TdxSpreadSheetConditionalFormattingRuleIconSetStop(ANode.Data);
  AStop.ComparisonOperator := TdxSpreadSheetConditionalFormattingRuleIconSetStopComparisonOperator(GetValueFromComboBox(ANode, AColumn));
  LoadStop(ANode, AStop);
  UpdateRowInfo;
end;

procedure TdxSpreadSheetConditionalFormattingRuleEditDialogIconSetHelper.SetIconIndex(
  ANode: TcxTreeListNode; AColumn: TcxTreeListColumn);
begin
  TdxSpreadSheetConditionalFormattingRuleIconSetStop(ANode.Data).IconIndex := ANode.Values[AColumn.ItemIndex];
end;

procedure TdxSpreadSheetConditionalFormattingRuleEditDialogIconSetHelper.SetIconSet(ASetIndex: Integer);
begin
  if ASetIndex >= 0 then
  begin
    FRule.PresetName := ConditionalFormattingIconSet.Presets[ASetIndex].Name;
    Load(FRule);
  end;
end;

procedure TdxSpreadSheetConditionalFormattingRuleEditDialogIconSetHelper.SetReverseIconOrder(AReverseIconOrder: Boolean);
const
  Map: array[Boolean] of TdxSpreadSheetConditionalFormattingRuleIconSetOrder = (isioNormal, isioReversed);
begin
  FRule.Order := Map[AReverseIconOrder];
  Load(FRule);
end;

procedure TdxSpreadSheetConditionalFormattingRuleEditDialogIconSetHelper.SetShowIconsOnly(AShowIconsOnly: Boolean);
begin
  FRule.ShowValue := not AShowIconsOnly;
end;

procedure TdxSpreadSheetConditionalFormattingRuleEditDialogIconSetHelper.SetStopValue(
  ANode: TcxTreeListNode; const AValue: Variant);
var
  AStop: TdxSpreadSheetConditionalFormattingRuleIconSetStop;
begin
  AStop := TdxSpreadSheetConditionalFormattingRuleIconSetStop(ANode.Data);
  AStop.Value := AValue;
  LoadStop(ANode, AStop);
  UpdateRowInfo;
end;

procedure TdxSpreadSheetConditionalFormattingRuleEditDialogIconSetHelper.SetStopValueType(
  ANode: TcxTreeListNode; AValueType: TdxSpreadSheetConditionalFormattingRuleCustomScaleStopValueType);
var
  AStop: TdxSpreadSheetConditionalFormattingRuleIconSetStop;
begin
  AStop := TdxSpreadSheetConditionalFormattingRuleIconSetStop(ANode.Data);
  AStop.ValueType := AValueType;
  LoadStop(ANode, AStop);
  UpdateRowInfo;
end;

procedure TdxSpreadSheetConditionalFormattingRuleEditDialogIconSetHelper.UpdateRowInfo;

  function GetValue(ANode: TcxTreeListNode): string;
  begin
    if TdxSpreadSheetConditionalFormattingRuleIconSetStop(ANode.Data).ValueType = cssvtFormula then
      Result := cxGetResourceString(@sdxConditionalFormattingRuleEditDialogScaleValueTypeFormula)
    else
      if VarIsSoftNull(ANode.Values[3]) then
        Result := ''
      else
        Result := ANode.Values[3];
  end;

  function GetFormatString(ANode: TcxTreeListNode): string;
  begin
    if ANode.IsFirst then
      Result := cxGetResourceString(@sdxConditionalFormattingRuleEditDialogWhenValueIs)
    else
      if TdxSpreadSheetConditionalFormattingRuleIconSetStop(ANode.getPrevSibling.Data).ComparisonOperator = isscoGreaterThan then
      begin
        if ANode.IsLast then
          Result := cxGetResourceString(@sdxConditionalFormattingRuleEditDialogWhenValueLessOrEqual)
        else
          Result := cxGetResourceString(@sdxConditionalFormattingRuleEditDialogWhenValueLessOrEqualAnd);
      end
      else
        if ANode.IsLast then
          Result := cxGetResourceString(@sdxConditionalFormattingRuleEditDialogWhenValueLess)
        else
          Result := cxGetResourceString(@sdxConditionalFormattingRuleEditDialogWhenValueLessAnd);
  end;

  procedure UpdateInfo(ANode: TcxTreeListNode);
  var
    AFormatString: string;
  begin
    AFormatString := GetFormatString(ANode);
    if ANode.IsFirst then
      ANode.Values[1] := AFormatString
    else
      ANode.Values[1] := Format(AFormatString, [GetValue(ANode.getPrevSibling)]);
  end;

var
  I: Integer;
begin
  for I := 0 to TreeList.Root.Count - 1 do
    UpdateInfo(TreeList.Root[I]);
end;

function TdxSpreadSheetConditionalFormattingRuleEditDialogIconSetHelper.GetIcons: TcxImageList;
begin
  Result := ConditionalFormattingIconSet.Icons;
end;

function TdxSpreadSheetConditionalFormattingRuleEditDialogIconSetHelper.GetPresets: TdxSpreadSheetConditionalFormattingIconSetPresets;
begin
  Result := ConditionalFormattingIconSet.Presets;
end;

function TdxSpreadSheetConditionalFormattingRuleEditDialogIconSetHelper.GetValueFromComboBox(
  ANode: TcxTreeListNode; AColumn: TcxTreeListColumn): TObject;
var
  AItems: TStrings;
  AValue: string;
begin
  AValue := AColumn.Values[ANode];
  AItems := (AColumn.Properties as TcxComboBoxProperties).Items;
  Result := AItems.Objects[AItems.IndexOf(AValue)];
end;

procedure TdxSpreadSheetConditionalFormattingRuleEditDialogIconSetHelper.LoadStop(
  ANode: TcxTreeListNode; AStop: TdxSpreadSheetConditionalFormattingRuleIconSetStop);

  function GetComboBoxValue(AColumnIndex, AValue: Integer): string;
  var
    AItems: TStrings;
  begin
    AItems := (FTreeList.Columns[AColumnIndex].Properties as TcxComboBoxProperties).Items;
    Result := AItems[AItems.IndexOfObject(TObject(AValue))];
  end;

begin
  ANode.Data := AStop;
  ANode.Values[0] := AStop.IconIndex;
  ANode.Values[2] := GetComboBoxValue(2, Ord(AStop.ComparisonOperator));
  ANode.Values[3] := AStop.Value;
  ANode.Values[4] := GetComboBoxValue(4, Ord(AStop.ValueType));
end;

{ TdxSpreadSheetConditionalFormattingRuleEditDialogUIHelper }

class function TdxSpreadSheetConditionalFormattingRuleEditDialogUIHelper.FindRuleType(
  ATreeList: TcxTreeList; AClass: TClass; out ANode: TcxTreeListNode): Boolean;
var
  I: Integer;
begin
  for I := 0 to ATreeList.Root.Count - 1 do
  begin
    ANode := ATreeList.Root[I];
    if TdxSpreadSheetConditionalFormattingRuleEditDialogRuleInfo(ANode.Data).RuleClass = AClass then
      Exit(True);
  end;
  Result := False;
end;

class procedure TdxSpreadSheetConditionalFormattingRuleEditDialogUIHelper.PopulateCellIsOperators(AItems: TStrings);
var
  AComparisonOperator: TdxSpreadSheetConditionalFormattingRuleCellIsComparisonOperator;
begin
  AItems.BeginUpdate;
  try
    AItems.Clear;
    for AComparisonOperator := Low(AComparisonOperator) to High(AComparisonOperator) do
      AItems.AddObject(GetCellIsRuleOperatorName(AComparisonOperator), TObject(AComparisonOperator));
  finally
    AItems.EndUpdate;
  end;
end;

class procedure TdxSpreadSheetConditionalFormattingRuleEditDialogUIHelper.PopulateRuleTypes(
  ATreeList: TcxTreeList; AHelper: TdxSpreadSheetConditionalFormattingRuleEditDialogHelper);
var
  I: Integer;
begin
  ATreeList.BeginUpdate;
  try
    ATreeList.Clear;
    for I := 0 to AHelper.RuleInfos.Count - 1 do
      ATreeList.AddChild(nil, AHelper.RuleInfos[I]).Texts[0] := AHelper.RuleInfos[I].Name;
  finally
    ATreeList.EndUpdate;
  end;
end;

class procedure TdxSpreadSheetConditionalFormattingRuleEditDialogUIHelper.PopulateTopBottomDirections(AItems: TStrings);
begin
  AItems.BeginUpdate;
  try
    AItems.Clear;
    AItems.AddObject(Format(cxGetResourceString(@sdxConditionalFormattingTopValues), ['']), TObject(tbvdTop));
    AItems.AddObject(Format(cxGetResourceString(@sdxConditionalFormattingBottomValues), ['']), TObject(tbvdBottom));
  finally
    AItems.EndUpdate;
  end;
end;

class procedure TdxSpreadSheetConditionalFormattingRuleEditDialogUIHelper.LoadAboveOrBelowAverageRuleInfo(
  ACombobox: TcxComboBox; ARule: TdxSpreadSheetConditionalFormattingRuleAboveOrBelowAverage);
var
  AIndex: Integer;
begin
  case ARule.ComparisonOperator of
    abacoAboveAverageOnStandardDeviation:
      AIndex :=  10 * ARule.StandardDeviationLevel;
    abacoBelowAverageOnStandardDeviation:
      AIndex := -10 * ARule.StandardDeviationLevel;
  else
    AIndex := Ord(ARule.ComparisonOperator);
  end;
  ACombobox.ItemIndex := ACombobox.Properties.Items.IndexOfObject(TObject(AIndex));
end;

class procedure TdxSpreadSheetConditionalFormattingRuleEditDialogUIHelper.SaveAboveOrBelowAverageRuleInfo(
  ACombobox: TcxComboBox; ARule: TdxSpreadSheetConditionalFormattingRuleAboveOrBelowAverage);
var
  AIndex: Integer;
begin
  AIndex := Integer(ACombobox.ItemObject);
  if InRange(AIndex, 0, 9) then
    ARule.ComparisonOperator := TdxSpreadSheetConditionalFormattingRuleAboveOrBelowAverageComparisonOperator(AIndex)
  else
  begin
    if AIndex < 0 then
      ARule.ComparisonOperator := abacoBelowAverageOnStandardDeviation
    else
      ARule.ComparisonOperator := abacoAboveAverageOnStandardDeviation;

    ARule.StandardDeviationLevel := Abs(AIndex div 10);
  end;
end;

class procedure TdxSpreadSheetConditionalFormattingRuleEditDialogUIHelper.PopulateAboveOrBelowAverageOperators(AItems: TStrings);
var
  I: Integer;
begin
  AItems.BeginUpdate;
  try
    AItems.Clear;
    AItems.AddObject(cxGetResourceString(@sdxConditionalFormattingAboveAverage), TObject(abacoAboveAverage));
    AItems.AddObject(cxGetResourceString(@sdxConditionalFormattingBelowAverage), TObject(abacoBelowAverage));
    AItems.AddObject(cxGetResourceString(@sdxConditionalFormattingAboveOrEqualAverage), TObject(abacoAboveOrEqualAverage));
    AItems.AddObject(cxGetResourceString(@sdxConditionalFormattingBelowOrEqualAverage), TObject(abacoBelowOrEqualAverage));

    for I := TdxSpreadSheetConditionalFormattingRuleAboveOrBelowAverage.MinStandardDeviationLevel to
      TdxSpreadSheetConditionalFormattingRuleAboveOrBelowAverage.MaxStandardDeviationLevel do
    begin
      AItems.AddObject(Format(cxGetResourceString(@sdxConditionalFormattingAboveAverageOnStandardDeviation), [I]), TObject(I * 10));
      AItems.AddObject(Format(cxGetResourceString(@sdxConditionalFormattingBelowAverageOnStandardDeviation), [I]), TObject(-I * 10));
    end;
  finally
    AItems.EndUpdate;
  end;
end;

class function TdxSpreadSheetConditionalFormattingRuleEditDialogUIHelper.GetScaleStopValueType(
  ACombobox: TcxComboBox): TdxSpreadSheetConditionalFormattingRuleCustomScaleStopValueType;
begin
  Result := TdxSpreadSheetConditionalFormattingRuleCustomScaleStopValueType(ACombobox.ItemObject);
end;

class procedure TdxSpreadSheetConditionalFormattingRuleEditDialogUIHelper.PopulateScaleStopTypes(ACombobox: TcxComboBox);
begin
  PopulateScaleStopTypes(ACombobox.Properties.Items, ACombobox.Tag);
end;

class procedure TdxSpreadSheetConditionalFormattingRuleEditDialogUIHelper.PopulateScaleStopTypes(
  AItems: TStrings; AStopIndex: Integer);
var
  AString: string;
  AType: TdxSpreadSheetConditionalFormattingRuleCustomScaleStopValueType;
begin
  AItems.BeginUpdate;
  try
    AItems.Clear;
    for AType := Low(AType) to High(AType) do
    begin
      if AType <> cssvtLimitValue then
        AString := GetScaleStopValueTypeName(AType)
      else
        case AStopIndex of
          0: AString := cxGetResourceString(@sdxConditionalFormattingRuleEditDialogScaleValueTypeLowestValue);
          1: Continue;
          2: AString := cxGetResourceString(@sdxConditionalFormattingRuleEditDialogScaleValueTypeHighestValue);
        end;

      AItems.AddObject(AString, TObject(AType));
    end;
  finally
    AItems.EndUpdate;
  end;
end;

end.
