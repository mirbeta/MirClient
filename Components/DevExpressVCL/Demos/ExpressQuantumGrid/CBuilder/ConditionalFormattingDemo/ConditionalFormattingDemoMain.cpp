//---------------------------------------------------------------------------

#include <vcl.h>
#include <shellapi.hpp>
#pragma hdrstop

#include "ConditionalFormattingDemoMain.h"
#include "ConditionalFormattingDemoData.h"
#include "dialogs.hpp"
#include "AboutDemoForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxClasses"
#pragma link "cxControls"
#pragma link "cxCustomData"
#pragma link "cxData"
#pragma link "cxDBData"
#pragma link "cxEdit"
#pragma link "cxFilter"
#pragma link "cxGraphics"
#pragma link "cxGrid"
#pragma link "cxGridCustomTableView"
#pragma link "cxGridCustomView"
#pragma link "cxGridDBTableView"
#pragma link "cxGridLevel"
#pragma link "cxGridTableView"
#pragma link "cxStyles"
#pragma link "cxLookAndFeels"
#pragma link "cxBlobEdit"
#pragma link "cxCheckBox"
#pragma link "cxDataStorage"
#pragma link "cxGridCardView"
#pragma link "cxHyperLinkEdit"
#pragma link "cxImage"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxMemo"
#pragma link "cxTextEdit"
#pragma link "cxDataControllerConditionalFormattingRulesManagerDialog"
#pragma link "cxNavigator"
#pragma link "cxCurrencyEdit"
#pragma link "ConditionalFormattingDemoData"
#pragma link "cxImageList"
#pragma link "dxSpreadSheetConditionalFormattingRules"
#pragma link "cxDataControllerConditionalFormatting"
#pragma link "dxSpreadSheetConditionalFormatting"
#pragma link "dxSpreadSheetConditionalFormattingIconSet"
#pragma resource "*.dfm"
TConditionalFormattingDemoMainForm *ConditionalFormattingDemoMainForm;
//---------------------------------------------------------------------------
__fastcall TConditionalFormattingDemoMainForm::TConditionalFormattingDemoMainForm(TComponent* Owner)
  : TfmBaseForm(Owner)
{
  TdxSpreadSheetConditionalFormattingRuleThreeColorScale::DefaultMinValueColor = clWhite;
  TdxSpreadSheetConditionalFormattingRuleThreeColorScale::DefaultMiddleValueColor = clGreen;
  TdxSpreadSheetConditionalFormattingRuleThreeColorScale::DefaultMaxValueColor = clRed;
  TdxSpreadSheetConditionalFormattingRuleTwoColorScale::DefaultMinValueColor = clRed;
  TdxSpreadSheetConditionalFormattingRuleTwoColorScale::DefaultMaxValueColor = clGreen;
}
//---------------------------------------------------------------------------
void __fastcall TConditionalFormattingDemoMainForm::ManageRulesClick(TObject *Sender)
{
  ConditionalFormatting->ShowRulesManagerDialog();
}
//---------------------------------------------------------------------------


void __fastcall TConditionalFormattingDemoMainForm::tvConditionalFormattingSalesVsTargetGetDisplayText(TcxCustomGridTableItem *Sender,
		  TcxCustomGridRecord *ARecord, UnicodeString &AText)
{
	Variant V = tvConditionalFormatting->DataController->Values[ARecord->RecordIndex][Sender->Index];
	if (VarIsNumeric(V)) {
		float AValue = (int)(V * 10000);
		TVarRec vr[] = {AValue / 100};
		AText = WideFormat("%g%%", vr, 1);
    }
}
//---------------------------------------------------------------------------

void __fastcall TConditionalFormattingDemoMainForm::tvConditionalFormattingSalesVsTargetGetFilterDisplayText(TcxCustomGridTableItem *Sender,
		  const Variant &AValue, UnicodeString &ADisplayText)
{
  if (VarIsNumeric(AValue)) {
	float ANewValue = int(AValue * 10000);
	TVarRec vr[] = {ANewValue / 100};
	ADisplayText = WideFormat("%g%%", vr, 1);
  }
}
//---------------------------------------------------------------------------

void __fastcall TConditionalFormattingDemoMainForm::tvConditionalFormattingMarketShareGetDisplayText(TcxCustomGridTableItem *Sender,
		  TcxCustomGridRecord *ARecord, UnicodeString &AText)

{
	Variant V = tvConditionalFormatting->DataController->Values[ARecord->RecordIndex][Sender->Index];
	if (VarIsNumeric(V)) {
		float AValue = V * 100;
		TVarRec vr[] = {(int)AValue};
		AText = WideFormat("%d%%", vr, 1);
	}
}
//---------------------------------------------------------------------------

void __fastcall TConditionalFormattingDemoMainForm::tvConditionalFormattingMarketShareGetFilterDisplayText(TcxCustomGridTableItem *Sender,
		  const Variant &AValue, UnicodeString &ADisplayText)

{
  if (VarIsNumeric(AValue)) {
	float ANewValue = AValue * 100;
	TVarRec vr[] = {(int)ANewValue};
	ADisplayText = WideFormat("%d%%", vr, 1);
  }
}
//---------------------------------------------------------------------------

void __fastcall TConditionalFormattingDemoMainForm::FormCreate(TObject *Sender)
{
  TMenuItem *AItem;
  TdxSpreadSheetConditionalFormattingIconSetPreset *APreset;
  miIconSets->SubMenuImages = ConditionalFormattingIconSet()->PresetPreviews;
  for (int i = 0; i < ConditionalFormattingIconSet()->Presets->Count; i++) {
	APreset = ConditionalFormattingIconSet()->Presets->Items[i];
	if (APreset->IndexOf(-1) < 0) {
	  AItem = new TMenuItem(miIconSets->Owner);
	  AItem->Caption = APreset->Description;
	  AItem->OnClick = IconSetsClick;
	  AItem->ImageIndex = i;
	  AItem->Tag = i;
	  miIconSets->Add(AItem);
	}
  }
}
//---------------------------------------------------------------------------

TcxDataControllerConditionalFormatting* __fastcall TConditionalFormattingDemoMainForm::GetConditionalFormatting(void)
{
	return tvConditionalFormatting->ConditionalFormatting;
}
//---------------------------------------------------------------------------
void __fastcall TConditionalFormattingDemoMainForm::miTop10Click(TObject *Sender)
{
  switch(((TComponent*)Sender)->Tag) {
	case 0:
		AddTopBottomRule(tbvdTop, tbvvtRank);
		break;
	case 1:
		AddTopBottomRule(tbvdTop, tbvvtPercent);
		break;
	case 2:
		AddTopBottomRule(tbvdBottom, tbvvtRank);
		break;
	case 3:
		AddTopBottomRule(tbvdBottom, tbvvtPercent);
		break;
	case 4:
		AddAboveOrBelowAverageRule(abacoAboveAverage);
		break;
	case 5:
		AddAboveOrBelowAverageRule(abacoBelowAverage);
		break;
  }
}
//---------------------------------------------------------------------------
void __fastcall TConditionalFormattingDemoMainForm::ThreeColorScaleClick(TObject *Sender)
{
  switch (((TComponent*)Sender)->Tag) {
	case
		0: AddThreeColorScaleRule((TColor)0x6B69F8, (TColor)0x84EBFF, (TColor)0x7BBE63);
		break;
	case 1:
		AddThreeColorScaleRule((TColor)0x7BBE63, (TColor)0x84EBFF, (TColor)0x6B69F8);
		break;
	case 2:
		AddThreeColorScaleRule((TColor)0x6B69F8, (TColor)0xFFFFFF, (TColor)0x7BBE63);
		break;
	case 3:
		AddThreeColorScaleRule((TColor)0x7BBE63, (TColor)0xFFFFFF, (TColor)0x6B69F8);
		break;
	case 4:
		AddThreeColorScaleRule((TColor)0x6B69F8, (TColor)0xFFFFFF, (TColor)0xC68A5A);
		break;
	case 5:
		AddThreeColorScaleRule((TColor)0xC68A5A, (TColor)0xFFFFFF, (TColor)0x6B69F8);
		break;
  }
}
//---------------------------------------------------------------------------
void __fastcall TConditionalFormattingDemoMainForm::TwoColorScaleClick(TObject *Sender)
{
  switch (((TComponent*)Sender)->Tag) {
	case 0:
		AddTwoColorScaleRule((TColor)0x6B69F8, (TColor)0xFFFFFF);
		break;
	case 1:
		AddTwoColorScaleRule((TColor)0xFFFFFF, (TColor)0x6B69F8);
		break;
	case 2:
		AddTwoColorScaleRule((TColor)0xFFFFFF, (TColor)0x7BBE63);
		break;
	case 3:
		AddTwoColorScaleRule((TColor)0x7BBE63, (TColor)0xFFFFFF);
		break;
	case 4:
		AddTwoColorScaleRule((TColor)0x84EBFF, (TColor)0x7BBE63);
		break;
	case 5:
		AddTwoColorScaleRule((TColor)0x7BBE63, (TColor)0x84EBFF);
		break;
  }
}
//---------------------------------------------------------------------------
void __fastcall TConditionalFormattingDemoMainForm::DataBarClick(TObject *Sender)
{
  switch (((TComponent*)Sender)->Tag) {
	case 0:
		AddDataBarRule((TColor)0xC68E63, clRed, True);
		break;
	case 1:
		AddDataBarRule((TColor)0x84C363, clRed, True);
		break;
	case 2:
		AddDataBarRule((TColor)0xC68E63, clRed, False);
		break;
	case 3:
		AddDataBarRule((TColor)0x84C363, clRed, False);
		break;
  }
}
//---------------------------------------------------------------------------
void __fastcall TConditionalFormattingDemoMainForm::IconSetsClick(TObject *Sender)
{
  AddIconSetRule(((TComponent*)Sender)->Tag);
}
//---------------------------------------------------------------------------
void __fastcall TConditionalFormattingDemoMainForm::ClearRulesfromThisColumn1Click(TObject *Sender)
{
  ClearRulesFromSelectedArea();
}
//---------------------------------------------------------------------------
void __fastcall TConditionalFormattingDemoMainForm::ClearRulesfromAllColumnsClick(TObject *Sender)
{
  if (MessageDlg("Do you want to clear all conditional formatting rules?", mtConfirmation, mbYesNoCancel, 0) == mrYes)
	ConditionalFormatting->Clear();
}
//---------------------------------------------------------------------------
void __fastcall TConditionalFormattingDemoMainForm::AddDataBarRule(TColor APositiveBarColor, TColor ANegativeBarColor, bool AIsSolidFill)
{
  TdxSpreadSheetConditionalFormattingRuleDataBar *ARule;
  ConditionalFormatting->BeginUpdate();
  try {
	RemoveRulesFromSelectedArea(__classid(TdxSpreadSheetConditionalFormattingRuleDataBar));
	ConditionalFormatting->Add(ConditionalFormatting->Owner->GetSelectionArea().Left, __classid(TdxSpreadSheetConditionalFormattingRuleDataBar), &ARule);
	ARule->BeginUpdate();
	try {
	  ARule->Style->NegativeBarColor = ANegativeBarColor;
	  ARule->Style->NegativeBarBorderColor = ANegativeBarColor;
	  ARule->Style->PositiveBarColor = APositiveBarColor;
	  ARule->Style->PositiveBarBorderColor = APositiveBarColor;
	  if (AIsSolidFill)
		ARule->Style->FillMode = dbfmSolid;
	  else
		ARule->Style->FillMode = dbfmGradient;
	} catch (...) {
	}
	ARule->EndUpdate();
  } catch (...) {
  }
  ConditionalFormatting->EndUpdate();
}
//---------------------------------------------------------------------------

void __fastcall TConditionalFormattingDemoMainForm::AddThreeColorScaleRule(TColor AColor1, TColor AColor2, TColor AColor3)
{
  TdxSpreadSheetConditionalFormattingRuleThreeColorScale *ARule;
  ConditionalFormatting->BeginUpdate();
  try {
	RemoveRulesFromSelectedArea(__classid(TdxSpreadSheetConditionalFormattingRuleCustomColorScale));
	ConditionalFormatting->Add(ConditionalFormatting->Owner->GetSelectionArea().Left, __classid(TdxSpreadSheetConditionalFormattingRuleThreeColorScale), &ARule);
	ARule->BeginUpdate();
	try {
	  ARule->MinValue->Color = AColor1;
	  ARule->MiddleValue->Color = AColor2;
	  ARule->MiddleValue->ValueType = cssvtPercentile;
	  ARule->MaxValue->Color = AColor3;
	} catch (...) {
	}
	ARule->EndUpdate();
  } catch (...) {
  }
  ConditionalFormatting->EndUpdate();
}
//---------------------------------------------------------------------------

void __fastcall TConditionalFormattingDemoMainForm::AddTwoColorScaleRule(TColor AColor1, TColor AColor2)
{
  TdxSpreadSheetConditionalFormattingRuleTwoColorScale *ARule;
  ConditionalFormatting->BeginUpdate();
  try {
	RemoveRulesFromSelectedArea(__classid(TdxSpreadSheetConditionalFormattingRuleCustomColorScale));
	ConditionalFormatting->Add(ConditionalFormatting->Owner->GetSelectionArea().Left, __classid(TdxSpreadSheetConditionalFormattingRuleTwoColorScale), &ARule);
	ARule->BeginUpdate();
	try {
	  ARule->MinValue->Color = AColor1;
	  ARule->MaxValue->Color = AColor2;
	} catch (...) {
	}
	ARule->EndUpdate();
  } catch (...) {
  }
  ConditionalFormatting->EndUpdate();
}
//---------------------------------------------------------------------------

void __fastcall TConditionalFormattingDemoMainForm::AddTopBottomRule(TdxSpreadSheetConditionalFormattingRuleTopBottomValuesDirection ADirection, TdxSpreadSheetConditionalFormattingRuleTopBottomValuesValueType AValueType)
{
  TdxSpreadSheetConditionalFormattingRuleTopBottomValues *ARule;
  ConditionalFormatting->BeginUpdate();
  try {
	RemoveRulesFromSelectedArea(__classid(TdxSpreadSheetConditionalFormattingRuleAboveOrBelowAverage));
	RemoveRulesFromSelectedArea(__classid(TdxSpreadSheetConditionalFormattingRuleTopBottomValues));
	ConditionalFormatting->Add(ConditionalFormatting->Owner->GetSelectionArea().Left, __classid(TdxSpreadSheetConditionalFormattingRuleTopBottomValues), &ARule);
	ARule->BeginUpdate();
	try {
	  ARule->Style->Brush->BackgroundColor = (TColor)0x9CFFFF;
	  ARule->Direction = ADirection;
	  ARule->ValueType = AValueType;
	  ARule->Value = 10;
	} catch (...) {
	}
	ARule->EndUpdate();
  } catch (...) {
  }
  ConditionalFormatting->EndUpdate();
}
//---------------------------------------------------------------------------

void __fastcall TConditionalFormattingDemoMainForm::AddAboveOrBelowAverageRule(TdxSpreadSheetConditionalFormattingRuleAboveOrBelowAverageComparisonOperator AComparasionOperator)
{
  TdxSpreadSheetConditionalFormattingRuleAboveOrBelowAverage *ARule;
  ConditionalFormatting->BeginUpdate();
  try {
	RemoveRulesFromSelectedArea(__classid(TdxSpreadSheetConditionalFormattingRuleAboveOrBelowAverage));
	RemoveRulesFromSelectedArea(__classid(TdxSpreadSheetConditionalFormattingRuleTopBottomValues));
	ConditionalFormatting->Add(ConditionalFormatting->Owner->GetSelectionArea().Left, __classid(TdxSpreadSheetConditionalFormattingRuleAboveOrBelowAverage), &ARule);
	ARule->BeginUpdate();
	try {
	  ARule->ComparisonOperator = AComparasionOperator;
	  ARule->Style->Brush->BackgroundColor = (TColor)0x9CFFFF;
	} catch (...) {
	}
	ARule->EndUpdate();
  } catch (...) {
  }
  ConditionalFormatting->EndUpdate();
}
//---------------------------------------------------------------------------

void __fastcall TConditionalFormattingDemoMainForm::AddIconSetRule(int APresetIndex)
{
  TdxSpreadSheetConditionalFormattingRuleIconSet *ARule;
  ConditionalFormatting->BeginUpdate();
  try {
	RemoveRulesFromSelectedArea(__classid(TdxSpreadSheetConditionalFormattingRuleIconSet));
	ConditionalFormatting->Add(ConditionalFormatting->Owner->GetSelectionArea().Left, __classid(TdxSpreadSheetConditionalFormattingRuleIconSet), &ARule);
	ARule->PresetName = ConditionalFormattingIconSet()->Presets->Items[APresetIndex]->Name;
  } catch (...) {
  }
  ConditionalFormatting->EndUpdate();
}
//---------------------------------------------------------------------------

void __fastcall TConditionalFormattingDemoMainForm::ClearRulesFromSelectedArea()
{
  TdxSpreadSheetCustomConditionalFormattingRule *ARule;
  ConditionalFormatting->BeginUpdate();
  try {
	TRect AArea = ConditionalFormatting->Owner->GetSelectionArea();
	for (int I = ConditionalFormatting->RuleCount - 1; I >= 0; I--)
	{
	  ARule = ConditionalFormatting->Rules[I];
	  TRect ARuleArea = ARule->Area;
	  if (!((AArea.Right < ARuleArea.Left) || (AArea.Bottom < ARuleArea.Top) || (ARuleArea.Right < AArea.Left) || (ARuleArea.Bottom < AArea.Top)))
		ARule->Free();
	}
  } catch (...) {
  }
  ConditionalFormatting->EndUpdate();
}
//---------------------------------------------------------------------------

void __fastcall TConditionalFormattingDemoMainForm::RemoveRulesFromSelectedArea(TdxSpreadSheetCustomConditionalFormattingRuleClass ARuleClass)
{
  TdxSpreadSheetCustomConditionalFormattingRule *ARule;
  ConditionalFormatting->BeginUpdate();
  try {
	TRect AArea = ConditionalFormatting->Owner->GetSelectionArea();
	for (int i = ConditionalFormatting->RuleCount - 1; i >= 0; i--) {
	  ARule = ConditionalFormatting->Rules[i];
	  if ((ARule->ClassType() == ARuleClass) && cxRectIsEqual(AArea, ARule->Area))
		ARule->Free();
	}
  } catch (...) {
  }
  ConditionalFormatting->EndUpdate();
}
//---------------------------------------------------------------------------

void __fastcall TConditionalFormattingDemoMainForm::FormDestroy(TObject *Sender)
{
// do nothing
}
//---------------------------------------------------------------------------

void __fastcall TConditionalFormattingDemoMainForm::FormShow(TObject *Sender)
{
// do nothing
}
//---------------------------------------------------------------------------

