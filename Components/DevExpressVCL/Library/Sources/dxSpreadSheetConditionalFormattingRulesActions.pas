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

unit dxSpreadSheetConditionalFormattingRulesActions;

{$I cxVer.Inc}

interface

uses
  Classes, Types, Variants, StdCtrls, Graphics, dxGallery, dxActions, dxSpreadSheetActions, dxSpreadSheetCore,
  dxSpreadSheetConditionalFormatting, dxSpreadSheetConditionalFormattingRules;

type
  { TdxSpreadSheetShowConditionalFormattingRulesManager }

  TdxSpreadSheetShowConditionalFormattingRulesManager = class(TdxSpreadSheetCustomFormatCellsAction)
  protected
    procedure DoExecute; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetCustomConditionalFormattingGalleryAction }

  TdxSpreadSheetCustomConditionalFormattingGalleryAction = class(TdxSpreadSheetCustomGalleryAction)
  protected
     //IdxActionGalleryClient
    function GetValue: Variant; override;

    function IsEnabled: Boolean; override;
  end;

  { TdxSpreadSheetConditionalFormattingTopBottomRulesGallery }

  TdxSpreadSheetConditionalFormattingTopBottomRulesGallery = class(TdxSpreadSheetCustomConditionalFormattingGalleryAction)
  protected type
    TRule = (rTop10Items, rTop10Percents, rBottom10Items, rBottom10Percents, rAboveAverage, rBelowAverage);
  strict private
    function GetRuleCaption(const ARule: TRule): string;

    procedure ReadGalleryGroup(Reader: TReader);
    procedure WriteGalleryGroup(Writer: TWriter);
  protected
    FGalleryGroup: TdxCustomGalleryGroup;

    procedure DefineProperties(Filer: TFiler); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure UpdateGalleryContentResourceStrings; override;

     //IdxActionGalleryClient
    procedure SetValue(const AValue: Variant); override;
    procedure PopulateGalleryInfo(AInfo: IdxActionGalleryInfo); override;

    procedure AddTopBottomRule(ADirection: TdxSpreadSheetConditionalFormattingRuleTopBottomValuesDirection;
      AValueType: TdxSpreadSheetConditionalFormattingRuleTopBottomValuesValueType); virtual;
    procedure AddAboveOrBelowAverageRule(
      AComparisonOperator: TdxSpreadSheetConditionalFormattingRuleAboveOrBelowAverageComparisonOperator); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;
  end;

  { TdxSpreadSheetConditionalFormattingColorScalesGallery }

  TdxSpreadSheetConditionalFormattingColorScalesGallery = class(TdxSpreadSheetCustomConditionalFormattingGalleryAction)
  protected type
    TColorScale = array [0 .. 2] of TColor;
    TPredefinedColorScale = (pcsGreenYellowRed, pcsRedYellowGreen, pcsGreenWhiteRed, pcsRedWhiteGreen, pcsBlueWhiteRed,
      pcsRedWhiteBlue, pcsWhiteRed, pcsRedWhite, pcsGreenWhite, pcsWhiteGreen, pcsGreenYellow, pcsYellowGreen);
  protected const
    CPredefinedColorScales: array [TPredefinedColorScale] of TColorScale = (
      ($6B69F8, $84EBFF, $7BBE63),
      ($7BBE63, $84EBFF, $6B69F8),
      ($6B69F8, $FFFFFF, $7BBE63),
      ($7BBE63, $FFFFFF, $6B69F8),
      ($6B69F8, $FFFFFF, $C68A5A),
      ($C68A5A, $FFFFFF, $6B69F8),
      ($6B69F8, clDefault, $FFFFFF),
      ($FFFFFF, clDefault, $6B69F8),
      ($FFFFFF, clDefault, $7BBE63),
      ($7BBE63, clDefault, $FFFFFF),
      ($84EBFF, clDefault, $7BBE63),
      ($7BBE63, clDefault, $84EBFF));
  protected
     //IdxActionGalleryClient
    procedure SetValue(const AValue: Variant); override;
    procedure PopulateGalleryInfo(AInfo: IdxActionGalleryInfo); override;

    procedure AddThreeColorScaleRule(const AColorScale: TColorScale); virtual;
    procedure AddTwoColorScaleRule(const AColorScale: TColorScale); virtual;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetConditionalFormattingDataBarsGallery }

  TdxSpreadSheetConditionalFormattingDataBarsGallery = class(TdxSpreadSheetCustomConditionalFormattingGalleryAction)
  protected type
    TDataBar = (dbGradientBlue, dbGradientGreen, dbGradientRed, dbGradientOrange, dbGradientLightBlue, dbGradientPurple,
      dbSolidBlue, dbSolidGreen, dbSolidRed, dbSolidOrange, dbSolidLightBlue, dbSolidPurple);
    TDataBarGroup = (dbgGradient, dbgSolid);
  strict private
    function GetDataBarGroupCaption(const ADataBarGroup: TDataBarGroup): string;

    procedure ReadGalleryGroupGradientFill(Reader: TReader);
    procedure ReadGalleryGroupSolidFill(Reader: TReader);
    procedure WriteGalleryGroupGradientFill(Writer: TWriter);
    procedure WriteGalleryGroupSolidFill(Writer: TWriter);
  protected
    FGalleryGroupGradientFill: TdxCustomGalleryGroup;
    FGalleryGroupSolidFill: TdxCustomGalleryGroup;

    procedure DefineProperties(Filer: TFiler); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure UpdateGalleryContentResourceStrings; override;

     //IdxActionGalleryClient
    procedure SetValue(const AValue: Variant); override;
    procedure PopulateGalleryInfo(AInfo: IdxActionGalleryInfo); override;

    procedure AddDataBarRule(AColor: TColor; AIsSolidFill: Boolean); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;
  end;

  { TdxSpreadSheetConditionalFormattingIconSetsGallery }

  TdxSpreadSheetConditionalFormattingIconSetsGallery = class(TdxSpreadSheetCustomConditionalFormattingGalleryAction)
  protected type
    TIconSet = (isDirectional, isShapes, isIndicators, isRatings);
  strict private
    function GetIconSetGroupCaption(const AIconSet: TIconSet): string;

    procedure ReadGalleryGroupDirectional(Reader: TReader);
    procedure ReadGalleryGroupIndicators(Reader: TReader);
    procedure ReadGalleryGroupRatings(Reader: TReader);
    procedure ReadGalleryGroupShapes(Reader: TReader);
    procedure WriteGalleryGroupDirectional(Writer: TWriter);
    procedure WriteGalleryGroupIndicators(Writer: TWriter);
    procedure WriteGalleryGroupRatings(Writer: TWriter);
    procedure WriteGalleryGroupShapes(Writer: TWriter);
  protected
    FGalleryGroupDirectional: TdxCustomGalleryGroup;
    FGalleryGroupIndicators: TdxCustomGalleryGroup;
    FGalleryGroupRatings: TdxCustomGalleryGroup;
    FGalleryGroupShapes: TdxCustomGalleryGroup;

    procedure DefineProperties(Filer: TFiler); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure UpdateGalleryContentResourceStrings; override;

     //IdxActionGalleryClient
    procedure SetValue(const AValue: Variant); override;
    procedure PopulateGalleryInfo(AInfo: IdxActionGalleryInfo); override;

    procedure AddIconSetRule(const APresetName: string); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;
  end;

  { TdxSpreadSheetConditionalFormattingMoreRules }

  TdxSpreadSheetConditionalFormattingMoreRules = class(TdxSpreadSheetCustomFormatCellsAction)
  protected
    procedure DoExecute; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetConditionalFormattingNewRule }

  TdxSpreadSheetConditionalFormattingNewRule = class(TdxSpreadSheetConditionalFormattingMoreRules)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetConditionalFormattingClearRulesFromSelectedCells }

  TdxSpreadSheetConditionalFormattingClearRulesFromSelectedCells = class(TdxSpreadSheetCustomFormatCellsAction)
  protected
    procedure DoExecute; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TdxSpreadSheetConditionalFormattingClearRulesFromEntireSheet }

  TdxSpreadSheetConditionalFormattingClearRulesFromEntireSheet = class(TdxSpreadSheetCustomFormatCellsAction)
  protected
    procedure DoExecute; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

uses
  Generics.Collections, dxCore, cxGeometry, dxSpreadSheetConditionalFormattingRulesManagerDialog,
  dxSpreadSheetActionsStrs, dxSpreadSheetConditionalFormattingIconSet, dxSpreadSheetConditionalFormattingRuleEditDialog,
  dxSpreadSheetCoreHelpers, dxSpreadSheetUtils;

type
  TdxSpreadSheetTableViewAccess = class(TdxSpreadSheetTableView);
  TdxSpreadSheetTableViewCustomCellsModificationHelperAccess =
    class(TdxSpreadSheetTableViewCustomCellsModificationHelper);
  TdxCustomSpreadSheetAccess = class(TdxCustomSpreadSheet);

procedure RemoveRulesFromSelectedArea(AView: TdxSpreadSheetTableView;
  ARuleClass: TdxSpreadSheetConditionalFormattingCustomRuleClass);
var
  AArea: TRect;
  ARule: TdxSpreadSheetCustomConditionalFormattingRule;
  I: Integer;
begin
  AView.BeginUpdate;
  try
    AArea := AView.Selection.Area;
    for I := AView.ConditionalFormatting.RuleCount - 1 downto 0 do
    begin
      ARule := AView.ConditionalFormatting.Rules[I];
      if cxRectIsEqual(ARule.Area, AArea) and (ARule is ARuleClass) then
        ARule.Free;
    end;
  finally
    AView.EndUpdate;
  end;
end;

{ TdxSpreadSheetShowConditionalFormattingRulesManager }

constructor TdxSpreadSheetShowConditionalFormattingRulesManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultCaptionResString := @sdxSpreadSheetActionShowConditionalFormattingRulesManagerCaption;
  FDefaultHintResString := @sdxSpreadSheetActionShowConditionalFormattingRulesManagerHint;
  FDefaultImageNameInIconLibrary := 'Conditional Formatting\ManageRules.png';
end;

procedure TdxSpreadSheetShowConditionalFormattingRulesManager.DoExecute;
begin
  ShowConditionalFormattingRulesManagerDialog(Control);
end;

{ TdxSpreadSheetCustomConditionalFormattingGalleryAction }

function TdxSpreadSheetCustomConditionalFormattingGalleryAction.GetValue: Variant;
begin
  Result := Null;
end;

function TdxSpreadSheetCustomConditionalFormattingGalleryAction.IsEnabled: Boolean;
begin
  Result := inherited IsEnabled and Control.OptionsBehavior.Editing and not ActiveTable.IsEditing and
    (not ActiveTable.OptionsProtection.&Protected or
    (TdxSpreadSheetTableViewAccess(ActiveTable).GetLockedStateOfCellsInArea(ActiveSelection.Area) = cbUnchecked)) and
    ActiveTable.OptionsProtection.ActualAllowFormatCells;
end;

{ TdxSpreadSheetConditionalFormattingTopBottomRulesGallery }

constructor TdxSpreadSheetConditionalFormattingTopBottomRulesGallery.Create(AOwner: TComponent);
begin
  inherited;
  FDefaultCaptionResString := @sdxSpreadSheetActionConditionalFormattingTopBottomRulesGalleryCaption;
  FDefaultImageNameInIconLibrary := 'Conditional Formatting\TopBottomRules.png';
end;

procedure TdxSpreadSheetConditionalFormattingTopBottomRulesGallery.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TdxSpreadSheetConditionalFormattingTopBottomRulesGallery then
    FGalleryGroup := TdxSpreadSheetConditionalFormattingTopBottomRulesGallery(Source).FGalleryGroup;
end;

procedure TdxSpreadSheetConditionalFormattingTopBottomRulesGallery.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('GalleryGroup', ReadGalleryGroup, WriteGalleryGroup, FGalleryGroup <> nil);
end;

procedure TdxSpreadSheetConditionalFormattingTopBottomRulesGallery.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FGalleryGroup) then
    FGalleryGroup := nil;
end;

procedure TdxSpreadSheetConditionalFormattingTopBottomRulesGallery.UpdateGalleryContentResourceStrings;

  procedure UpdateGalleryGroupItemResourceStrings(const ARule: TRule);
  var
    AItem: TdxCustomGalleryItem;
  begin
    AItem := FindGalleryGroupItem(FGalleryGroup, ARule);
    if AItem <> nil then
      AItem.Caption := GetRuleCaption(ARule);
  end;

var
  ARule: TRule;
begin
  if (Control <> nil) and (FGalleryGroup <> nil) then
    for ARule := Low(TRule) to High(TRule) do
      UpdateGalleryGroupItemResourceStrings(ARule);
end;

procedure TdxSpreadSheetConditionalFormattingTopBottomRulesGallery.SetValue(const AValue: Variant);
begin
  case TRule(AValue) of
    rTop10Items:
      AddTopBottomRule(tbvdTop, tbvvtRank);
    rTop10Percents:
      AddTopBottomRule(tbvdTop, tbvvtPercent);
    rBottom10Items:
      AddTopBottomRule(tbvdBottom, tbvvtRank);
    rBottom10Percents:
      AddTopBottomRule(tbvdBottom, tbvvtPercent);
    rAboveAverage:
      AddAboveOrBelowAverageRule(abacoAboveAverage);
    rBelowAverage:
      AddAboveOrBelowAverageRule(abacoBelowAverage);
  end;
end;

procedure TdxSpreadSheetConditionalFormattingTopBottomRulesGallery.PopulateGalleryInfo(
  AInfo: IdxActionGalleryInfo);

  function GetImageFileName(const ARule: TRule): string;
  begin
    case ARule of
      rTop10Items:
        Result := 'Conditional Formatting\Top10Items.png';
      rTop10Percents:
        Result := 'Conditional Formatting\Top.png';
      rBottom10Items:
        Result := 'Conditional Formatting\Bottom10Items.png';
      rBottom10Percents:
        Result := 'Conditional Formatting\Bottom10%.png';
      rAboveAverage:
        Result := 'Conditional Formatting\AboveAverage.png';
      rBelowAverage:
        Result := 'Conditional Formatting\BelowAverage.png';
    else
      Result := '';
    end;
  end;

var
  ARule: TRule;
begin
  if AInfo.GetGroups is TdxCustomGalleryGroups then
    FGalleryGroup := TdxCustomGalleryGroups(AInfo.GetGroups).Add
  else
    FGalleryGroup := nil;

  for ARule := Low(TRule) to High(TRule) do
    AInfo.Add(ARule, '', GetRuleCaption(ARule), '', GetImageFileName(ARule));
end;

procedure TdxSpreadSheetConditionalFormattingTopBottomRulesGallery.AddTopBottomRule(
  ADirection: TdxSpreadSheetConditionalFormattingRuleTopBottomValuesDirection;
  AValueType: TdxSpreadSheetConditionalFormattingRuleTopBottomValuesValueType);
var
  ARule: TdxSpreadSheetConditionalFormattingRuleTopBottomValues;
begin
  ActiveTable.History.BeginAction(TdxSpreadSheetHistoryChangeConditionalFormattingAction);
  try
    RemoveRulesFromSelectedArea(ActiveTable, TdxSpreadSheetConditionalFormattingRuleAboveOrBelowAverage);
    RemoveRulesFromSelectedArea(ActiveTable, TdxSpreadSheetConditionalFormattingRuleTopBottomValues);
    ARule := TdxSpreadSheetConditionalFormattingRuleTopBottomValues.Create(ActiveTable.ConditionalFormatting,
      ActiveSelection.Area);
    ARule.BeginUpdate;
    try
      ARule.Style.Brush.BackgroundColor := $9CFFFF;
      ARule.Direction := ADirection;
      ARule.ValueType := AValueType;
      ARule.Value := 10;
    finally
      ARule.EndUpdate;
    end;
  finally
    ActiveTable.History.EndAction;
  end;
end;

procedure TdxSpreadSheetConditionalFormattingTopBottomRulesGallery.AddAboveOrBelowAverageRule(
  AComparisonOperator: TdxSpreadSheetConditionalFormattingRuleAboveOrBelowAverageComparisonOperator);
var
  ARule: TdxSpreadSheetConditionalFormattingRuleAboveOrBelowAverage;
begin
  ActiveTable.History.BeginAction(TdxSpreadSheetHistoryChangeConditionalFormattingAction);
  try
    RemoveRulesFromSelectedArea(ActiveTable, TdxSpreadSheetConditionalFormattingRuleAboveOrBelowAverage);
    RemoveRulesFromSelectedArea(ActiveTable, TdxSpreadSheetConditionalFormattingRuleTopBottomValues);
    ARule := TdxSpreadSheetConditionalFormattingRuleAboveOrBelowAverage.Create(ActiveTable.ConditionalFormatting,
      ActiveSelection.Area);
    ARule.BeginUpdate;
    try
      ARule.ComparisonOperator := AComparisonOperator;
      ARule.Style.Brush.BackgroundColor := $9CFFFF;
    finally
      ARule.EndUpdate;
    end;
  finally
    ActiveTable.History.EndAction;
  end;
end;

function TdxSpreadSheetConditionalFormattingTopBottomRulesGallery.GetRuleCaption(const ARule: TRule): string;
begin
  case ARule of
    rTop10Items:
      Result :=
        cxGetResourceString(@sdxSpreadSheetActionConditionalFormattingTopBottomRulesGalleryTop10ItemsCaption);
    rTop10Percents:
      Result :=
        cxGetResourceString(@sdxSpreadSheetActionConditionalFormattingTopBottomRulesGalleryTop10PercentsCaption);
    rBottom10Items:
      Result :=
        cxGetResourceString(@sdxSpreadSheetActionConditionalFormattingTopBottomRulesGalleryBottom10ItemsCaption);
    rBottom10Percents:
      Result :=
        cxGetResourceString(@sdxSpreadSheetActionConditionalFormattingTopBottomRulesGalleryBottom10PercentsCaption);
    rAboveAverage:
      Result :=
        cxGetResourceString(@sdxSpreadSheetActionConditionalFormattingTopBottomRulesGalleryAboveAverageCaption);
    rBelowAverage:
      Result :=
        cxGetResourceString(@sdxSpreadSheetActionConditionalFormattingTopBottomRulesGalleryBelowAverageCaption);
  else
    Result := '';
  end;
end;

procedure TdxSpreadSheetConditionalFormattingTopBottomRulesGallery.ReadGalleryGroup(Reader: TReader);
begin
  FGalleryGroup := FindGalleryGroup(Reader.ReadIdent);
end;

procedure TdxSpreadSheetConditionalFormattingTopBottomRulesGallery.WriteGalleryGroup(Writer: TWriter);
begin
  Writer.WriteIdent(FGalleryGroup.Name);
end;

{ TdxSpreadSheetConditionalFormattingColorScalesGallery }

constructor TdxSpreadSheetConditionalFormattingColorScalesGallery.Create(AOwner: TComponent);
begin
  inherited;
  FDefaultCaptionResString := @sdxSpreadSheetActionConditionalFormattingColorScalesGalleryCaption;
  FDefaultImageNameInIconLibrary := 'Scales\GreenYellowRed.png';
end;

procedure TdxSpreadSheetConditionalFormattingColorScalesGallery.SetValue(const AValue: Variant);
begin
  if TPredefinedColorScale(AValue) in
    [pcsGreenYellowRed, pcsRedYellowGreen, pcsGreenWhiteRed, pcsRedWhiteGreen, pcsBlueWhiteRed, pcsRedWhiteBlue] then
    AddThreeColorScaleRule(CPredefinedColorScales[TPredefinedColorScale(AValue)])
  else
    AddTwoColorScaleRule(CPredefinedColorScales[TPredefinedColorScale(AValue)]);
end;

procedure TdxSpreadSheetConditionalFormattingColorScalesGallery.PopulateGalleryInfo(
  AInfo: IdxActionGalleryInfo);

  function GetImageFileName(const APredefinedColorScale: TPredefinedColorScale): string;
  begin
    case APredefinedColorScale of
      pcsGreenYellowRed:
        Result := 'Scales\GreenYellowRed.png';
      pcsRedYellowGreen:
        Result := 'Scales\RedYellowGreen.png';
      pcsGreenWhiteRed:
        Result := 'Scales\GreenWhiteRed.png';
      pcsRedWhiteGreen:
        Result := 'Scales\RedWhiteGreen.png';
      pcsBlueWhiteRed:
        Result := 'Scales\BlueWhiteRed.png';
      pcsRedWhiteBlue:
        Result := 'Scales\RedWhiteBlue.png';
      pcsWhiteRed:
        Result := 'Scales\WhiteRed.png';
      pcsRedWhite:
        Result := 'Scales\RedWhite.png';
      pcsGreenWhite:
        Result := 'Scales\GreenWhite.png';
      pcsWhiteGreen:
        Result := 'Scales\WhiteGreen.png';
      pcsGreenYellow:
        Result := 'Scales\GreenYellow.png';
      pcsYellowGreen:
        Result := 'Scales\YellowGreen.png';
    else
      Result := '';
    end;
  end;

var
  AColorScale: TPredefinedColorScale;
begin
  for AColorScale := Low(TPredefinedColorScale) to High(TPredefinedColorScale) do
    AInfo.Add(AColorScale, '', '', '', GetImageFileName(AColorScale));
end;

procedure TdxSpreadSheetConditionalFormattingColorScalesGallery.AddThreeColorScaleRule(const AColorScale: TColorScale);
var
  ARule: TdxSpreadSheetConditionalFormattingRuleThreeColorScale;
begin
  ActiveTable.History.BeginAction(TdxSpreadSheetHistoryChangeConditionalFormattingAction);
  try
    RemoveRulesFromSelectedArea(ActiveTable, TdxSpreadSheetConditionalFormattingRuleCustomColorScale);
    ARule := TdxSpreadSheetConditionalFormattingRuleThreeColorScale.Create(ActiveTable.ConditionalFormatting,
      ActiveSelection.Area);
    ARule.BeginUpdate;
    try
      ARule.MinValue.Color := AColorScale[0];
      ARule.MiddleValue.Color := AColorScale[1];
      ARule.MiddleValue.ValueType := cssvtPercentile;
      ARule.MaxValue.Color := AColorScale[2];
    finally
      ARule.EndUpdate;
    end;
  finally
    ActiveTable.History.EndAction;
  end;
end;

procedure TdxSpreadSheetConditionalFormattingColorScalesGallery.AddTwoColorScaleRule(const AColorScale: TColorScale);
var
  ARule: TdxSpreadSheetConditionalFormattingRuleTwoColorScale;
begin
  ActiveTable.History.BeginAction(TdxSpreadSheetHistoryChangeConditionalFormattingAction);
  try
    RemoveRulesFromSelectedArea(ActiveTable, TdxSpreadSheetConditionalFormattingRuleCustomColorScale);
    ARule := TdxSpreadSheetConditionalFormattingRuleTwoColorScale.Create(ActiveTable.ConditionalFormatting,
      ActiveSelection.Area);
    ARule.BeginUpdate;
    try
      ARule.MinValue.Color := AColorScale[0];
      ARule.MaxValue.Color := AColorScale[2];
    finally
      ARule.EndUpdate;
    end;
  finally
    ActiveTable.History.EndAction;
  end;
end;

{ TdxSpreadSheetConditionalFormattingDataBarsGallery }

constructor TdxSpreadSheetConditionalFormattingDataBarsGallery.Create(AOwner: TComponent);
begin
  inherited;
  FDefaultCaptionResString := @sdxSpreadSheetActionConditionalFormattingDataBarsGalleryCaption;
  FDefaultImageNameInIconLibrary := 'Data Bars\SolidBlueDataBar.png';
end;

procedure TdxSpreadSheetConditionalFormattingDataBarsGallery.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TdxSpreadSheetConditionalFormattingDataBarsGallery then
  begin
    FGalleryGroupGradientFill := TdxSpreadSheetConditionalFormattingDataBarsGallery(Source).FGalleryGroupGradientFill;
    FGalleryGroupSolidFill := TdxSpreadSheetConditionalFormattingDataBarsGallery(Source).FGalleryGroupSolidFill;
  end;
end;

procedure TdxSpreadSheetConditionalFormattingDataBarsGallery.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('GalleryGroupGradientFill', ReadGalleryGroupGradientFill, WriteGalleryGroupGradientFill,
    FGalleryGroupGradientFill <> nil);
  Filer.DefineProperty('GalleryGroupSolidFill', ReadGalleryGroupSolidFill, WriteGalleryGroupSolidFill,
    FGalleryGroupSolidFill <> nil);
end;

procedure TdxSpreadSheetConditionalFormattingDataBarsGallery.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if AComponent = FGalleryGroupGradientFill then
      FGalleryGroupGradientFill := nil;
    if AComponent = FGalleryGroupSolidFill then
      FGalleryGroupSolidFill := nil;
  end;
end;

procedure TdxSpreadSheetConditionalFormattingDataBarsGallery.UpdateGalleryContentResourceStrings;
begin
  if (Control <> nil) then
  begin
    if FGalleryGroupGradientFill <> nil then
      FGalleryGroupGradientFill.Caption := GetDataBarGroupCaption(dbgGradient);
    if FGalleryGroupSolidFill <> nil then
      FGalleryGroupSolidFill.Caption := GetDataBarGroupCaption(dbgSolid);
  end;
end;

procedure TdxSpreadSheetConditionalFormattingDataBarsGallery.SetValue(const AValue: Variant);
var
  ADataBar: TDataBar;
begin
  ADataBar := TDataBar(AValue);
  case ADataBar of
    dbGradientBlue, dbSolidBlue:
      AddDataBarRule($C68E63, ADataBar = dbSolidBlue);
    dbGradientGreen, dbSolidGreen:
      AddDataBarRule($84C363, ADataBar = dbSolidGreen);
    dbGradientRed, dbSolidRed:
      AddDataBarRule($5A55FF, ADataBar = dbSolidRed);
    dbGradientOrange, dbSolidOrange:
      AddDataBarRule($28B6FF, ADataBar = dbSolidOrange);
    dbGradientLightBlue, dbSolidLightBlue:
      AddDataBarRule($EF8A00, ADataBar = dbSolidLightBlue);
    dbGradientPurple, dbSolidPurple:
      AddDataBarRule($7B00D6, ADataBar = dbSolidPurple);
  end;
end;

procedure TdxSpreadSheetConditionalFormattingDataBarsGallery.PopulateGalleryInfo(
  AInfo: IdxActionGalleryInfo);

  function GetGroupCaption(ADataBar: TDataBar): string;
  begin
    if ADataBar in
      [dbGradientBlue, dbGradientGreen, dbGradientRed, dbGradientOrange, dbGradientLightBlue, dbGradientPurple] then
      Result := GetDataBarGroupCaption(dbgGradient)
    else
      Result := GetDataBarGroupCaption(dbgSolid);
  end;

  function GetImageFileName(ADataBar: TDataBar): string;
  begin
    case ADataBar of
      dbGradientBlue:
        Result := 'Data Bars\GradientBlueDataBar.png';
      dbGradientGreen:
        Result := 'Data Bars\GradientGreenDataBar.png';
      dbGradientRed:
        Result := 'Data Bars\GradientRedDataBar.png';
      dbGradientOrange:
        Result := 'Data Bars\GradientOrangeDataBar.png';
      dbGradientLightBlue:
        Result := 'Data Bars\GradientLightBlueDataBar.png';
      dbGradientPurple:
        Result := 'Data Bars\GradientPurpleDataBar.png';
      dbSolidBlue:
        Result := 'Data Bars\SolidBlueDataBar.png';
      dbSolidGreen:
        Result := 'Data Bars\SolidGreenDataBar.png';
      dbSolidRed:
        Result := 'Data Bars\SolidRedDataBar.png';
      dbSolidOrange:
        Result := 'Data Bars\SolidOrangeDataBar.png';
      dbSolidLightBlue:
        Result := 'Data Bars\SolidLightBlueDataBar.png';
      dbSolidPurple:
        Result := 'Data Bars\SolidPurpleDataBar.png';
    else
      Result := '';
    end;
  end;

var
  ADataBar: TDataBar;
begin
  if AInfo.GetGroups is TdxCustomGalleryGroups then
  begin
    FGalleryGroupGradientFill := TdxCustomGalleryGroups(AInfo.GetGroups).Add;
    FGalleryGroupGradientFill.Caption := GetDataBarGroupCaption(dbgGradient);

    FGalleryGroupSolidFill := TdxCustomGalleryGroups(AInfo.GetGroups).Add;
    FGalleryGroupSolidFill.Caption := GetDataBarGroupCaption(dbgSolid);
  end
  else
  begin
    FGalleryGroupGradientFill := nil;
    FGalleryGroupSolidFill := nil;
  end;

  for ADataBar := Low(TDataBar) to High(TDataBar) do
    AInfo.Add(ADataBar, GetGroupCaption(ADataBar), '', '', GetImageFileName(ADataBar));
end;

procedure TdxSpreadSheetConditionalFormattingDataBarsGallery.AddDataBarRule(AColor: TColor; AIsSolidFill: Boolean);
var
  ARule: TdxSpreadSheetConditionalFormattingRuleDataBar;
begin
  ActiveTable.History.BeginAction(TdxSpreadSheetHistoryChangeConditionalFormattingAction);
  try
    RemoveRulesFromSelectedArea(ActiveTable, TdxSpreadSheetConditionalFormattingRuleDataBar);
    ARule := TdxSpreadSheetConditionalFormattingRuleDataBar.Create(ActiveTable.ConditionalFormatting,
      ActiveSelection.Area);
    ARule.BeginUpdate;
    try
      ARule.Style.NegativeBarColor := clRed;
      ARule.Style.NegativeBarBorderColor := clRed;
      ARule.Style.PositiveBarColor := AColor;
      ARule.Style.PositiveBarBorderColor := AColor;
      if AIsSolidFill then
        ARule.Style.FillMode := dbfmSolid
      else
        ARule.Style.FillMode := dbfmGradient;
    finally
      ARule.EndUpdate;
    end;
  finally
    ActiveTable.History.EndAction;
  end;
end;

function TdxSpreadSheetConditionalFormattingDataBarsGallery.GetDataBarGroupCaption(
  const ADataBarGroup: TDataBarGroup): string;
begin
  case ADataBarGroup of
    dbgGradient:
      Result := cxGetResourceString(@sdxSpreadSheetActionConditionalFormattingDataBarsGalleryGradientFillGroupCaption);
    dbgSolid:
      Result := cxGetResourceString(@sdxSpreadSheetActionConditionalFormattingDataBarsGallerySolidFillGroupCaption);
  else
    Result := ''
  end;
end;

procedure TdxSpreadSheetConditionalFormattingDataBarsGallery.ReadGalleryGroupGradientFill(Reader: TReader);
begin
  FGalleryGroupGradientFill := FindGalleryGroup(Reader.ReadIdent);
end;

procedure TdxSpreadSheetConditionalFormattingDataBarsGallery.ReadGalleryGroupSolidFill(Reader: TReader);
begin
  FGalleryGroupSolidFill := FindGalleryGroup(Reader.ReadIdent);
end;

procedure TdxSpreadSheetConditionalFormattingDataBarsGallery.WriteGalleryGroupGradientFill(Writer: TWriter);
begin
  Writer.WriteIdent(FGalleryGroupGradientFill.Name);
end;

procedure TdxSpreadSheetConditionalFormattingDataBarsGallery.WriteGalleryGroupSolidFill(Writer: TWriter);
begin
  Writer.WriteIdent(FGalleryGroupSolidFill.Name);
end;

{ TdxSpreadSheetConditionalFormattingIconSetsGallery }

constructor TdxSpreadSheetConditionalFormattingIconSetsGallery.Create(AOwner: TComponent);
begin
  inherited;
  FDefaultCaptionResString := @sdxSpreadSheetActionConditionalFormattingIconSetsGalleryCaption;
  FDefaultImageNameInIconLibrary := 'Conditional Formatting\IconSetArrows4.png';
end;

procedure TdxSpreadSheetConditionalFormattingIconSetsGallery.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TdxSpreadSheetConditionalFormattingIconSetsGallery then
  begin
    FGalleryGroupDirectional := TdxSpreadSheetConditionalFormattingIconSetsGallery(Source).FGalleryGroupDirectional;
    FGalleryGroupIndicators := TdxSpreadSheetConditionalFormattingIconSetsGallery(Source).FGalleryGroupIndicators;
    FGalleryGroupRatings := TdxSpreadSheetConditionalFormattingIconSetsGallery(Source).FGalleryGroupRatings;
    FGalleryGroupShapes := TdxSpreadSheetConditionalFormattingIconSetsGallery(Source).FGalleryGroupShapes;
  end;
end;

procedure TdxSpreadSheetConditionalFormattingIconSetsGallery.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('GalleryGroupDirectional', ReadGalleryGroupDirectional, WriteGalleryGroupDirectional,
    FGalleryGroupDirectional <> nil);
  Filer.DefineProperty('GalleryGroupIndicators', ReadGalleryGroupIndicators, WriteGalleryGroupIndicators,
    FGalleryGroupIndicators <> nil);
  Filer.DefineProperty('GalleryGroupRatings', ReadGalleryGroupRatings, WriteGalleryGroupRatings,
    FGalleryGroupRatings <> nil);
  Filer.DefineProperty('GalleryGroupShapes', ReadGalleryGroupShapes, WriteGalleryGroupShapes,
    FGalleryGroupShapes <> nil);
end;

procedure TdxSpreadSheetConditionalFormattingIconSetsGallery.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if AComponent = FGalleryGroupDirectional then
      FGalleryGroupDirectional := nil;
    if AComponent = FGalleryGroupIndicators then
      FGalleryGroupIndicators := nil;
    if AComponent = FGalleryGroupRatings then
      FGalleryGroupRatings := nil;
    if AComponent = FGalleryGroupShapes then
      FGalleryGroupShapes := nil;
  end;
end;

procedure TdxSpreadSheetConditionalFormattingIconSetsGallery.UpdateGalleryContentResourceStrings;
begin
  if (Control <> nil) then
  begin
    if FGalleryGroupDirectional <> nil then
      FGalleryGroupDirectional.Caption := GetIconSetGroupCaption(isDirectional);
    if FGalleryGroupIndicators <> nil then
      FGalleryGroupIndicators.Caption := GetIconSetGroupCaption(isIndicators);
    if FGalleryGroupRatings <> nil then
      FGalleryGroupRatings.Caption := GetIconSetGroupCaption(isRatings);
    if FGalleryGroupShapes <> nil then
      FGalleryGroupShapes.Caption := GetIconSetGroupCaption(isShapes);
  end;
end;

procedure TdxSpreadSheetConditionalFormattingIconSetsGallery.SetValue(const AValue: Variant);
begin
  AddIconSetRule(VarToStr(AValue));
end;

procedure TdxSpreadSheetConditionalFormattingIconSetsGallery.PopulateGalleryInfo(AInfo: IdxActionGalleryInfo);

  function GetGroupCaption(APresetIndex: Integer): string;
  begin
    case APresetIndex of
      0, 1, 8, 9, 10, 14, 15:
        Result := GetIconSetGroupCaption(isDirectional);
      3, 6, 7, 12, 13:
        Result := GetIconSetGroupCaption(isShapes);
      2, 4, 5:
        Result := GetIconSetGroupCaption(isIndicators);
      11, 16, 17, 18:
        Result := GetIconSetGroupCaption(isRatings);
    else
      Result := '';
    end;
  end;

var
  AGroups: TdxCustomGalleryGroups;
  APreset: TdxSpreadSheetConditionalFormattingIconSetPreset;
  I: Integer;
begin
  if AInfo.GetGroups is TdxCustomGalleryGroups then
  begin
    AGroups := TdxCustomGalleryGroups(AInfo.GetGroups);

    FGalleryGroupDirectional := AGroups.Add;
    FGalleryGroupDirectional.Caption := GetIconSetGroupCaption(isDirectional);

    FGalleryGroupIndicators := AGroups.Add;
    FGalleryGroupIndicators.Caption := GetIconSetGroupCaption(isIndicators);

    FGalleryGroupShapes := AGroups.Add;
    FGalleryGroupShapes.Caption := GetIconSetGroupCaption(isShapes);

    FGalleryGroupRatings := AGroups.Add;
    FGalleryGroupRatings.Caption := GetIconSetGroupCaption(isRatings);
  end
  else
  begin
    FGalleryGroupDirectional := nil;
    FGalleryGroupIndicators := nil;
    FGalleryGroupShapes := nil;
    FGalleryGroupRatings := nil;
  end;

  AInfo.SetExternalImageList(ConditionalFormattingIconSet.PresetPreviews);
  for I := 0 to ConditionalFormattingIconSet.Presets.Count - 1 do
  begin
    APreset := ConditionalFormattingIconSet.Presets[I];
    if APreset.IndexOf(-1) < 0 then
      AInfo.Add(APreset.Name, GetGroupCaption(I), '', '', '', I);
  end;
end;

procedure TdxSpreadSheetConditionalFormattingIconSetsGallery.AddIconSetRule(const APresetName: string);
var
  ARule: TdxSpreadSheetConditionalFormattingRuleIconSet;
begin
  ActiveTable.History.BeginAction(TdxSpreadSheetHistoryChangeConditionalFormattingAction);
  try
    RemoveRulesFromSelectedArea(ActiveTable, TdxSpreadSheetConditionalFormattingRuleIconSet);
    ARule := TdxSpreadSheetConditionalFormattingRuleIconSet.Create(ActiveTable.ConditionalFormatting,
      ActiveSelection.Area);
    ARule.PresetName := APresetName;
  finally
    ActiveTable.History.EndAction;
  end;
end;

function TdxSpreadSheetConditionalFormattingIconSetsGallery.GetIconSetGroupCaption(const AIconSet: TIconSet): string;
begin
  case AIconSet of
    isDirectional:
      Result := cxGetResourceString(@sdxSpreadSheetActionConditionalFormattingIconSetsGalleryDirectionalGroupCaption);
    isShapes:
      Result := cxGetResourceString(@sdxSpreadSheetActionConditionalFormattingIconSetsGalleryShapesGroupCaption);
    isIndicators:
      Result := cxGetResourceString(@sdxSpreadSheetActionConditionalFormattingIconSetsGalleryIndicatorsGroupCaption);
    isRatings:
      Result := cxGetResourceString(@sdxSpreadSheetActionConditionalFormattingIconSetsGalleryRatingsGroupCaption);
  else
    Result := '';
  end;
end;

procedure TdxSpreadSheetConditionalFormattingIconSetsGallery.ReadGalleryGroupDirectional(Reader: TReader);
begin
  FGalleryGroupDirectional := FindGalleryGroup(Reader.ReadIdent);
end;

procedure TdxSpreadSheetConditionalFormattingIconSetsGallery.ReadGalleryGroupIndicators(Reader: TReader);
begin
  FGalleryGroupIndicators := FindGalleryGroup(Reader.ReadIdent);
end;

procedure TdxSpreadSheetConditionalFormattingIconSetsGallery.ReadGalleryGroupRatings(Reader: TReader);
begin
  FGalleryGroupRatings := FindGalleryGroup(Reader.ReadIdent);
end;

procedure TdxSpreadSheetConditionalFormattingIconSetsGallery.ReadGalleryGroupShapes(Reader: TReader);
begin
  FGalleryGroupShapes := FindGalleryGroup(Reader.ReadIdent);
end;

procedure TdxSpreadSheetConditionalFormattingIconSetsGallery.WriteGalleryGroupDirectional(Writer: TWriter);
begin
  Writer.WriteIdent(FGalleryGroupDirectional.Name);
end;

procedure TdxSpreadSheetConditionalFormattingIconSetsGallery.WriteGalleryGroupIndicators(Writer: TWriter);
begin
  Writer.WriteIdent(FGalleryGroupIndicators.Name);
end;

procedure TdxSpreadSheetConditionalFormattingIconSetsGallery.WriteGalleryGroupRatings(Writer: TWriter);
begin
  Writer.WriteIdent(FGalleryGroupRatings.Name);
end;

procedure TdxSpreadSheetConditionalFormattingIconSetsGallery.WriteGalleryGroupShapes(Writer: TWriter);
begin
  Writer.WriteIdent(FGalleryGroupShapes.Name);
end;

{ TdxSpreadSheetConditionalFormattingMoreRules }

constructor  TdxSpreadSheetConditionalFormattingMoreRules.Create(AOwner: TComponent);
begin
  inherited;
  FDefaultCaptionResString := @sdxSpreadSheetActionConditionalFormattingMoreRulesCaption;
  FDefaultHintResString := @sdxSpreadSheetActionConditionalFormattingMoreRulesHint;
  FDefaultImageNameInIconLibrary := '';
end;

procedure TdxSpreadSheetConditionalFormattingMoreRules.DoExecute;
var
  ARule: TdxSpreadSheetCustomConditionalFormattingRule;
begin
  ARule := nil;
  ShowConditionalFormattingRuleEditDialog(Control, ARule);
end;

{ TdxSpreadSheetConditionalFormattingNewRule }

constructor TdxSpreadSheetConditionalFormattingNewRule.Create(AOwner: TComponent);
begin
  inherited;
  FDefaultCaptionResString := @sdxSpreadSheetActionConditionalFormattingNewRuleCaption;
  FDefaultHintResString := @sdxSpreadSheetActionConditionalFormattingNewRuleHint;
  FDefaultImageNameInIconLibrary := 'Spreadsheet\NewConditionalFormattingRule.png';
end;

{ TdxSpreadSheetConditionalFormattingClearRulesFromSelectedCells }

constructor TdxSpreadSheetConditionalFormattingClearRulesFromSelectedCells.Create(AOwner: TComponent);
begin
  inherited;
  FDefaultCaptionResString := @sdxSpreadSheetActionConditionalFormattingClearRulesFromSelectedCellsCaption;
  FDefaultHintResString := @sdxSpreadSheetActionConditionalFormattingClearRulesFromSelectedCellsHint;
  FDefaultImageNameInIconLibrary := '';
end;

procedure TdxSpreadSheetConditionalFormattingClearRulesFromSelectedCells.DoExecute;
var
  AArea: TRect;
  AHelper: TdxSpreadSheetTableViewCustomCellsModificationHelperAccess;
  ARule: TdxSpreadSheetCustomConditionalFormattingRule;
  I, J: Integer;
begin
  Control.BeginUpdate;
  try
    AArea := ActiveSelection.Area;
    AHelper := TdxSpreadSheetTableViewCustomCellsModificationHelperAccess.Create(ActiveTable);
    try
      AHelper.CreateIndependentConditionalFormattingAreasForMovedArea(AArea);
    finally
      AHelper.Free;
    end;
    for I := ActiveTable.ConditionalFormatting.RuleCount - 1 downto 0 do
    begin
      ARule := ActiveTable.ConditionalFormatting.Rules[I];
      ARule.BeginUpdate;
      try
        for J := ARule.Areas.Count - 1 downto 0 do
          if dxSpreadSheetContains(AArea, ARule.Areas[J]) then
            ARule.Areas.Delete(J);
      finally
        ARule.EndUpdate;
      end;
    end;
  finally
    TdxCustomSpreadSheetAccess(Control).AddChanges([sscData, sscModified]);
    Control.EndUpdate;
  end;
end;

{ TdxSpreadSheetConditionalFormattingClearRulesFromEntireSheet }

constructor TdxSpreadSheetConditionalFormattingClearRulesFromEntireSheet.Create(AOwner: TComponent);
begin
  inherited;
  FDefaultCaptionResString := @sdxSpreadSheetActionConditionalFormattingClearRulesFromEntireSheetCaption;
  FDefaultHintResString := @sdxSpreadSheetActionConditionalFormattingClearRulesFromEntireSheetHint;
  FDefaultImageNameInIconLibrary := '';
end;

procedure TdxSpreadSheetConditionalFormattingClearRulesFromEntireSheet.DoExecute;
begin
  ActiveTable.ConditionalFormatting.Clear;
end;

end.
