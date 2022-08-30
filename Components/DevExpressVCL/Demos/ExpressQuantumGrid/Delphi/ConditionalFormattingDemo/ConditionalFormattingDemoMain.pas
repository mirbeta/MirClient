unit ConditionalFormattingDemoMain;

{$I cxVer.inc}

interface

uses
  Windows, Messages, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  cxGridLevel, cxControls, cxGridCustomView, cxGridCustomTableView, cxGridTableView,
  cxGridDBTableView, cxGrid, ExtCtrls, Grids, DBGrids, cxStyles, Menus, ActnList,
  ImgList, cxLookAndFeels, ComCtrls, cxCustomData, cxGraphics, cxFilter, cxData,
  cxEdit, DB, cxDBData, cxClasses, cxDataStorage, cxImage, cxTextEdit, cxMemo,
  cxCheckBox, cxBlobEdit, cxHyperLinkEdit, cxLookAndFeelPainters, BaseForm,
  cxGridCardView, cxNavigator, cxCurrencyEdit, cxImageList,
{$IFDEF EXPRESSBARS}
  ConditionalFormattingDemoPopupMenuHelper,
{$ENDIF}
  cxButtons, cxDataControllerConditionalFormattingRulesManagerDialog,
  dxSpreadSheetConditionalFormatting,
  cxDataControllerConditionalFormatting, dxSpreadSheetConditionalFormattingRules;

type
  TConditionalFormattingDemoMainForm = class(TfmBaseForm)
    miOptions: TMenuItem;
    Grid: TcxGrid;
    tvConditionalFormatting: TcxGridDBTableView;
    tvConditionalFormattingState: TcxGridDBColumn;
    tvConditionalFormattingSales: TcxGridDBColumn;
    tvConditionalFormattingProfit: TcxGridDBColumn;
    tvConditionalFormattingSalesVsTarget: TcxGridDBColumn;
    tvConditionalFormattingCustomersSatisfaction: TcxGridDBColumn;
    tvConditionalFormattingMarketShare: TcxGridDBColumn;
    GridLevel: TcxGridLevel;
    N1: TMenuItem;
    miManageRules: TMenuItem;
    PopupMenu1: TPopupMenu;
    ilBarSmall: TcxImageList;
    ColorScales1: TMenuItem;
    DataBars1: TMenuItem;
    miIconSets: TMenuItem;
    opBottomRules1: TMenuItem;
    miTop10: TMenuItem;
    op102: TMenuItem;
    Bottom101: TMenuItem;
    Bottom102: TMenuItem;
    AboveAverage1: TMenuItem;
    BelowAverage1: TMenuItem;
    N2: TMenuItem;
    ClearRulesfromThisColumn1: TMenuItem;
    ClearRulesfromAllColumns: TMenuItem;
    ManageRules2: TMenuItem;
    N3: TMenuItem;
    GreenYellowRedColorScale1: TMenuItem;
    RedYellowGreenColorScale1: TMenuItem;
    GreenWhiteRedColorScale1: TMenuItem;
    RedWhiteGreenColorScale1: TMenuItem;
    BlueWhiteRedColorScale1: TMenuItem;
    RedWhiteBlueColorScale1: TMenuItem;
    N4: TMenuItem;
    WhiteRedColorScale1: TMenuItem;
    RedWhiteColorScale1: TMenuItem;
    GreenWhiteColorScale1: TMenuItem;
    WhiteGreenColorScale1: TMenuItem;
    GreenYellowColorScale1: TMenuItem;
    YellowGreenColorScale1: TMenuItem;
    BlueDataBarGradient1: TMenuItem;
    GreenDataBarGradient1: TMenuItem;
    BlueDataBarSolid1: TMenuItem;
    GreenDataBarSolid1: TMenuItem;
    procedure miExitClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ManageRulesClick(Sender: TObject);
    procedure tvConditionalFormattingSalesVsTargetGetDisplayText(
      Sender: TcxCustomGridTableItem; ARecord: TcxCustomGridRecord;
      var AText: string);
    procedure tvConditionalFormattingSalesVsTargetGetFilterDisplayText(
      Sender: TcxCustomGridTableItem; const AValue: Variant;
      var ADisplayText: string);
    procedure tvConditionalFormattingMarketShareGetDisplayText(
      Sender: TcxCustomGridTableItem; ARecord: TcxCustomGridRecord;
      var AText: string);
    procedure tvConditionalFormattingMarketShareGetFilterDisplayText(
      Sender: TcxCustomGridTableItem; const AValue: Variant;
      var ADisplayText: string);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure miTop10Click(Sender: TObject);
    procedure ThreeColorScaleClick(Sender: TObject);
    procedure TwoColorScaleClick(Sender: TObject);
    procedure DataBarClick(Sender: TObject);
    procedure IconSetsClick(Sender: TObject);
    procedure ClearRulesfromThisColumn1Click(Sender: TObject);
    procedure ClearRulesfromAllColumnsClick(Sender: TObject);
  private
  {$IFDEF EXPRESSBARS}
    FConditionalFormattingPopupMenuHelper: TConditionalFormattingPopupMenuHelper;
  {$ENDIF}
    function GetConditionalFormatting: TcxDataControllerConditionalFormatting;
  protected
    property ConditionalFormatting: TcxDataControllerConditionalFormatting read GetConditionalFormatting;
  public
    procedure AddDataBarRule(APositiveBarColor, ANegativeBarColor: TColor; AIsSolidFill: Boolean);
    procedure AddThreeColorScaleRule(AColor1, AColor2, AColor3: TColor);
    procedure AddTwoColorScaleRule(AColor1, AColor2: TColor);
    procedure AddTopBottomRule(ADirection: TdxSpreadSheetConditionalFormattingRuleTopBottomValuesDirection;
      AValueType: TdxSpreadSheetConditionalFormattingRuleTopBottomValuesValueType);
    procedure AddAboveOrBelowAverageRule(AComparasionOperator: TdxSpreadSheetConditionalFormattingRuleAboveOrBelowAverageComparisonOperator);
    procedure AddIconSetRule(APresetIndex: Integer);
    procedure ClearRulesFromSelectedArea;
    procedure RemoveRulesFromSelectedArea(ARuleClass: TdxSpreadSheetConditionalFormattingCustomRuleClass);
  end;

var
  ConditionalFormattingDemoMainForm: TConditionalFormattingDemoMainForm;

implementation

{$R *.dfm}

uses
  Variants,
  dxSpreadSheetConditionalFormattingIconSet,
  AboutDemoForm, ConditionalFormattingDemoData, SysUtils, dxTypeHelpers;

procedure TConditionalFormattingDemoMainForm.FormCreate(Sender: TObject);
var
  AItem: TMenuItem;
  APreset: TdxSpreadSheetConditionalFormattingIconSetPreset;
  I: Integer;
begin
  miIconSets.SubMenuImages := ConditionalFormattingIconSet.PresetPreviews;
  for I := 0 to ConditionalFormattingIconSet.Presets.Count - 1 do
  begin
    APreset := ConditionalFormattingIconSet.Presets[I];
    if APreset.IndexOf(-1) < 0 then
    begin
      AItem := TMenuItem.Create(miIconSets.Owner);
      AItem.Caption := APreset.Description;
      AItem.OnClick := IconSetsClick;
      AItem.ImageIndex := I;
      AItem.Tag := I;
      miIconSets.Add(AItem);
    end;
  end;
{$IFDEF EXPRESSBARS}
  FConditionalFormattingPopupMenuHelper := TConditionalFormattingPopupMenuHelper.Create(nil);
{$ENDIF}
end;

procedure TConditionalFormattingDemoMainForm.FormDestroy(Sender: TObject);
begin
{$IFDEF EXPRESSBARS}
  FConditionalFormattingPopupMenuHelper.Free;
{$ENDIF}
end;

procedure TConditionalFormattingDemoMainForm.FormShow(Sender: TObject);
begin
  inherited;
{$IFDEF EXPRESSBARS}
  tvConditionalFormatting.PopupMenu := FConditionalFormattingPopupMenuHelper.ConditionalFormattingPopupMenu;
{$ENDIF}
end;

procedure TConditionalFormattingDemoMainForm.ManageRulesClick(Sender: TObject);
begin
  inherited;
  tvConditionalFormatting.ConditionalFormatting.ShowRulesManagerDialog;
end;

procedure TConditionalFormattingDemoMainForm.miExitClick(Sender: TObject);
begin
  Close;
end;

procedure TConditionalFormattingDemoMainForm.miTop10Click(Sender: TObject);
begin
  case TComponent(Sender).Tag of
    0: AddTopBottomRule(tbvdTop, tbvvtRank);
    1: AddTopBottomRule(tbvdTop, tbvvtPercent);
    2: AddTopBottomRule(tbvdBottom, tbvvtRank);
    3: AddTopBottomRule(tbvdBottom, tbvvtPercent);
    4: AddAboveOrBelowAverageRule(abacoAboveAverage);
    5: AddAboveOrBelowAverageRule(abacoBelowAverage);
  end;
end;

procedure TConditionalFormattingDemoMainForm.tvConditionalFormattingMarketShareGetDisplayText(
  Sender: TcxCustomGridTableItem; ARecord: TcxCustomGridRecord;
  var AText: string);
var
  V: Variant;
  AValue: Double;
begin
  V := tvConditionalFormatting.DataController.Values[ARecord.RecordIndex, Sender.Index];
  if VarIsNumeric(V) then
  begin
    AValue := V * 100;
    AText := Format('%d%%', [Round(AValue)]);
  end;
end;

procedure TConditionalFormattingDemoMainForm.tvConditionalFormattingMarketShareGetFilterDisplayText(
  Sender: TcxCustomGridTableItem; const AValue: Variant;
  var ADisplayText: string);
var
  ANewValue: Double;
begin
  if VarIsNumeric(AValue) then
  begin
    ANewValue := AValue * 100;
    ADisplayText := Format('%d%%', [Round(ANewValue)]);
  end;
end;

procedure TConditionalFormattingDemoMainForm.tvConditionalFormattingSalesVsTargetGetDisplayText(
  Sender: TcxCustomGridTableItem; ARecord: TcxCustomGridRecord;
  var AText: string);
var
  V: Variant;
  AValue: Double;
begin
  V := tvConditionalFormatting.DataController.Values[ARecord.RecordIndex, Sender.Index];
  if VarIsNumeric(V) then
  begin
    AValue := V * 100;
    AText := Format('%g%%', [AValue]);
  end;
end;

procedure TConditionalFormattingDemoMainForm.tvConditionalFormattingSalesVsTargetGetFilterDisplayText(
  Sender: TcxCustomGridTableItem; const AValue: Variant;
  var ADisplayText: string);
var
  ANewValue: Double;
begin
  if VarIsNumeric(AValue) then
  begin
    ANewValue := AValue * 100;
    ADisplayText := Format('%g%%', [ANewValue]);
  end;
end;

procedure TConditionalFormattingDemoMainForm.TwoColorScaleClick(
  Sender: TObject);
begin
  case TComponent(Sender).Tag of
    0: AddTwoColorScaleRule($6B69F8, $FFFFFF);
    1: AddTwoColorScaleRule($FFFFFF, $6B69F8);
    2: AddTwoColorScaleRule($FFFFFF, $7BBE63);
    3: AddTwoColorScaleRule($7BBE63, $FFFFFF);
    4: AddTwoColorScaleRule($84EBFF, $7BBE63);
    5: AddTwoColorScaleRule($7BBE63, $84EBFF);
  end;
end;

procedure TConditionalFormattingDemoMainForm.AddDataBarRule(APositiveBarColor, ANegativeBarColor: TColor; AIsSolidFill: Boolean);
var
  ARule: TdxSpreadSheetConditionalFormattingRuleDataBar;
begin
  ConditionalFormatting.BeginUpdate;
  try
    RemoveRulesFromSelectedArea(TdxSpreadSheetConditionalFormattingRuleDataBar);
    ConditionalFormatting.Add(ConditionalFormatting.Owner.GetSelectionArea.Left, TdxSpreadSheetConditionalFormattingRuleDataBar, ARule);
    ARule.BeginUpdate;
    try
      ARule.Style.NegativeBarColor := ANegativeBarColor;
      ARule.Style.NegativeBarBorderColor := ANegativeBarColor;
      ARule.Style.PositiveBarColor := APositiveBarColor;
      ARule.Style.PositiveBarBorderColor := APositiveBarColor;
      if AIsSolidFill then
        ARule.Style.FillMode := dbfmSolid
      else
        ARule.Style.FillMode := dbfmGradient;
    finally
      ARule.EndUpdate;
    end;
  finally
    ConditionalFormatting.EndUpdate;
  end;
end;

procedure TConditionalFormattingDemoMainForm.AddThreeColorScaleRule(AColor1, AColor2, AColor3: TColor);
var
  ARule: TdxSpreadSheetConditionalFormattingRuleThreeColorScale;
begin
  ConditionalFormatting.BeginUpdate;
  try
    RemoveRulesFromSelectedArea(TdxSpreadSheetConditionalFormattingRuleCustomColorScale);
    ConditionalFormatting.Add(ConditionalFormatting.Owner.GetSelectionArea.Left, TdxSpreadSheetConditionalFormattingRuleThreeColorScale, ARule);
    ARule.BeginUpdate;
    try
      ARule.MinValue.Color := AColor1;
      ARule.MiddleValue.Color := AColor2;
      ARule.MiddleValue.ValueType := cssvtPercentile;
      ARule.MaxValue.Color := AColor3;
    finally
      ARule.EndUpdate;
    end;
  finally
    ConditionalFormatting.EndUpdate;
  end;
end;

procedure TConditionalFormattingDemoMainForm.AddTwoColorScaleRule(AColor1, AColor2: TColor);
var
  ARule: TdxSpreadSheetConditionalFormattingRuleTwoColorScale;
begin
  ConditionalFormatting.BeginUpdate;
  try
    RemoveRulesFromSelectedArea(TdxSpreadSheetConditionalFormattingRuleCustomColorScale);
    ConditionalFormatting.Add(ConditionalFormatting.Owner.GetSelectionArea.Left, TdxSpreadSheetConditionalFormattingRuleTwoColorScale, ARule);
    ARule.BeginUpdate;
    try
      ARule.MinValue.Color := AColor1;
      ARule.MaxValue.Color := AColor2;
    finally
      ARule.EndUpdate;
    end;
  finally
    ConditionalFormatting.EndUpdate;
  end;
end;

procedure TConditionalFormattingDemoMainForm.AddTopBottomRule(ADirection: TdxSpreadSheetConditionalFormattingRuleTopBottomValuesDirection;
  AValueType: TdxSpreadSheetConditionalFormattingRuleTopBottomValuesValueType);
var
  ARule: TdxSpreadSheetConditionalFormattingRuleTopBottomValues;
begin
  ConditionalFormatting.BeginUpdate;
  try
    RemoveRulesFromSelectedArea(TdxSpreadSheetConditionalFormattingRuleAboveOrBelowAverage);
    RemoveRulesFromSelectedArea(TdxSpreadSheetConditionalFormattingRuleTopBottomValues);
    ConditionalFormatting.Add(ConditionalFormatting.Owner.GetSelectionArea.Left, TdxSpreadSheetConditionalFormattingRuleTopBottomValues, ARule);
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
    ConditionalFormatting.EndUpdate;
  end;
end;

procedure TConditionalFormattingDemoMainForm.AddAboveOrBelowAverageRule(AComparasionOperator: TdxSpreadSheetConditionalFormattingRuleAboveOrBelowAverageComparisonOperator);
var
  ARule: TdxSpreadSheetConditionalFormattingRuleAboveOrBelowAverage;
begin
  ConditionalFormatting.BeginUpdate;
  try
    RemoveRulesFromSelectedArea(TdxSpreadSheetConditionalFormattingRuleAboveOrBelowAverage);
    RemoveRulesFromSelectedArea(TdxSpreadSheetConditionalFormattingRuleTopBottomValues);
    ConditionalFormatting.Add(ConditionalFormatting.Owner.GetSelectionArea.Left, TdxSpreadSheetConditionalFormattingRuleAboveOrBelowAverage, ARule);
    ARule.BeginUpdate;
    try
      ARule.ComparisonOperator := AComparasionOperator;
      ARule.Style.Brush.BackgroundColor := $9CFFFF;
    finally
      ARule.EndUpdate;
    end;
  finally
    ConditionalFormatting.EndUpdate;
  end;
end;

procedure TConditionalFormattingDemoMainForm.AddIconSetRule(APresetIndex: Integer);
var
  ARule: TdxSpreadSheetConditionalFormattingRuleIconSet;
begin
  ConditionalFormatting.BeginUpdate;
  try
    RemoveRulesFromSelectedArea(TdxSpreadSheetConditionalFormattingRuleIconSet);
    ConditionalFormatting.Add(ConditionalFormatting.Owner.GetSelectionArea.Left, TdxSpreadSheetConditionalFormattingRuleIconSet, ARule);
    ARule.PresetName := ConditionalFormattingIconSet.Presets[APresetIndex].Name;
  finally
    ConditionalFormatting.EndUpdate;
  end;
end;

procedure TConditionalFormattingDemoMainForm.ClearRulesfromAllColumnsClick(
  Sender: TObject);
begin
  if MessageDlg('Do you want to clear all conditional formatting rules?', mtConfirmation, mbYesNoCancel, 0) = mrYes then
    ConditionalFormatting.Clear;
end;

procedure TConditionalFormattingDemoMainForm.ClearRulesFromSelectedArea;
var
  AArea: TRect;
  ARule: TdxSpreadSheetCustomConditionalFormattingRule;
  I: Integer;
begin
  ConditionalFormatting.BeginUpdate;
  try
    AArea := ConditionalFormatting.Owner.GetSelectionArea;
    for I := ConditionalFormatting.RuleCount - 1 downto 0 do
    begin
      ARule := ConditionalFormatting.Rules[I];
      if ARule.Area.IntersectsWith(AArea) then
        ARule.Free;
    end;
  finally
    ConditionalFormatting.EndUpdate;
  end;
end;

procedure TConditionalFormattingDemoMainForm.ClearRulesfromThisColumn1Click(
  Sender: TObject);
begin
  ClearRulesFromSelectedArea;
end;

procedure TConditionalFormattingDemoMainForm.DataBarClick(Sender: TObject);
begin
  case TComponent(Sender).Tag of
    0: AddDataBarRule($C68E63, clRed, True);
    1: AddDataBarRule($84C363, clRed, True);
    2: AddDataBarRule($C68E63, clRed, False);
    3: AddDataBarRule($84C363, clRed, False);
  end;
end;

procedure TConditionalFormattingDemoMainForm.IconSetsClick(Sender: TObject);
begin
  AddIconSetRule(TComponent(Sender).Tag);
end;

procedure TConditionalFormattingDemoMainForm.RemoveRulesFromSelectedArea(ARuleClass: TdxSpreadSheetConditionalFormattingCustomRuleClass);
var
  AArea: TRect;
  ARule: TdxSpreadSheetCustomConditionalFormattingRule;
  I: Integer;
begin
  ConditionalFormatting.BeginUpdate;
  try
    AArea := ConditionalFormatting.Owner.GetSelectionArea;
    for I := ConditionalFormatting.RuleCount - 1 downto 0 do
    begin
      ARule := ConditionalFormatting.Rules[I];
      if ARule.Area.IsEqual(AArea) and (ARule is ARuleClass) then
        ARule.Free;
    end;
  finally
    ConditionalFormatting.EndUpdate;
  end;
end;

procedure TConditionalFormattingDemoMainForm.ThreeColorScaleClick(
  Sender: TObject);
begin
  case TComponent(Sender).Tag of
    0: AddThreeColorScaleRule($6B69F8, $84EBFF, $7BBE63);
    1: AddThreeColorScaleRule($7BBE63, $84EBFF, $6B69F8);
    2: AddThreeColorScaleRule($6B69F8, $FFFFFF, $7BBE63);
    3: AddThreeColorScaleRule($7BBE63, $FFFFFF, $6B69F8);
    4: AddThreeColorScaleRule($6B69F8, $FFFFFF, $C68A5A);
    5: AddThreeColorScaleRule($C68A5A, $FFFFFF, $6B69F8);
  end;
end;

function TConditionalFormattingDemoMainForm.GetConditionalFormatting: TcxDataControllerConditionalFormatting;
begin
  Result := tvConditionalFormatting.ConditionalFormatting;
end;

initialization
  TdxSpreadSheetConditionalFormattingRuleThreeColorScale.DefaultMinValueColor := clWhite;
  TdxSpreadSheetConditionalFormattingRuleThreeColorScale.DefaultMiddleValueColor := clGreen;
  TdxSpreadSheetConditionalFormattingRuleThreeColorScale.DefaultMaxValueColor := clRed;
  TdxSpreadSheetConditionalFormattingRuleTwoColorScale.DefaultMinValueColor := clRed;
  TdxSpreadSheetConditionalFormattingRuleTwoColorScale.DefaultMaxValueColor := clGreen;


finalization

end.
