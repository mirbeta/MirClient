//---------------------------------------------------------------------------

#ifndef ConditionalFormattingDemoMainH
#define ConditionalFormattingDemoMainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxClasses.hpp"
#include "cxControls.hpp"
#include "cxCustomData.hpp"
#include "cxData.hpp"
#include "cxDBData.hpp"
#include "cxEdit.hpp"
#include "cxFilter.hpp"
#include "cxGraphics.hpp"
#include "cxGrid.hpp"
#include "cxGridCustomTableView.hpp"
#include "cxGridCustomView.hpp"
#include "cxGridDBTableView.hpp"
#include "cxGridLevel.hpp"
#include "cxGridTableView.hpp"
#include "cxStyles.hpp"
#include <ActnList.hpp>
#include <ComCtrls.hpp>
#include <DB.hpp>
#include <ExtCtrls.hpp>
#include <ImgList.hpp>
#include <Menus.hpp>
#include "cxLookAndFeels.hpp"
#include "BaseForm.h"
#include "cxBlobEdit.hpp"
#include "cxCheckBox.hpp"
#include "cxDataStorage.hpp"
#include "cxGridCardView.hpp"
#include "cxHyperLinkEdit.hpp"
#include "cxImage.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxMemo.hpp"
#include "cxTextEdit.hpp"
#include "cxDataControllerConditionalFormattingRulesManagerDialog.hpp"
#include "dxSpreadSheetConditionalFormattingRules.hpp"
#include "cxDataControllerConditionalFormatting.hpp"
#include "dxSpreadSheetConditionalFormatting.hpp"
#include "cxNavigator.hpp"
#include "cxCurrencyEdit.hpp"
#include "cxImageList.hpp"
//---------------------------------------------------------------------------
class TConditionalFormattingDemoMainForm : public TfmBaseForm
{
__published:	// IDE-managed Components
  TSplitter *Splitter;
  TMenuItem *miOptions;
	TcxGrid *Grid;
	TcxGridDBTableView *tvConditionalFormatting;
	TcxGridDBColumn *tvConditionalFormattingState;
	TcxGridDBColumn *tvConditionalFormattingSales;
	TcxGridDBColumn *tvConditionalFormattingProfit;
	TcxGridDBColumn *tvConditionalFormattingSalesVsTarget;
	TcxGridDBColumn *tvConditionalFormattingCustomersSatisfaction;
	TcxGridDBColumn *tvConditionalFormattingMarketShare;
	TcxGridLevel *GridLevel;
	TMenuItem *N1;
	TMenuItem *miManageRules;
	TPopupMenu *PopupMenu1;
	TcxImageList *ilBarSmall;
	TMenuItem *ColorScales1;
	TMenuItem *DataBars1;
	TMenuItem *miIconSets;
	TMenuItem *opBottomRules1;
	TMenuItem *miTop10;
	TMenuItem *op102;
	TMenuItem *Bottom101;
	TMenuItem *Bottom102;
	TMenuItem *AboveAverage1;
	TMenuItem *BelowAverage1;
	TMenuItem *N2;
	TMenuItem *ClearRulesfromThisColumn1;
	TMenuItem *ClearRulesfromAllColumns;
	TMenuItem *ManageRules2;
    TMenuItem *N3;
	TMenuItem *GreenYellowRedColorScale1;
	TMenuItem *RedYellowGreenColorScale1;
	TMenuItem *GreenWhiteRedColorScale1;
	TMenuItem *RedWhiteGreenColorScale1;
	TMenuItem *BlueWhiteRedColorScale1;
	TMenuItem *RedWhiteBlueColorScale1;
	TMenuItem *N4;
	TMenuItem *WhiteRedColorScale1;
	TMenuItem *RedWhiteColorScale1;
	TMenuItem *GreenWhiteColorScale1;
	TMenuItem *WhiteGreenColorScale1;
	TMenuItem *GreenYellowColorScale1;
	TMenuItem *YellowGreenColorScale1;
	TMenuItem *BlueDataBarGradient1;
	TMenuItem *GreenDataBarGradient1;
	TMenuItem *BlueDataBarSolid1;
    TMenuItem *GreenDataBarSolid1;
	void __fastcall ManageRulesClick(TObject *Sender);
	void __fastcall tvConditionalFormattingSalesVsTargetGetDisplayText(TcxCustomGridTableItem *Sender,
		  TcxCustomGridRecord *ARecord, UnicodeString &AText);
	void __fastcall tvConditionalFormattingSalesVsTargetGetFilterDisplayText(TcxCustomGridTableItem *Sender,
		  const Variant &AValue, UnicodeString &ADisplayText);
	void __fastcall tvConditionalFormattingMarketShareGetDisplayText(TcxCustomGridTableItem *Sender,
		  TcxCustomGridRecord *ARecord, UnicodeString &AText);
	void __fastcall tvConditionalFormattingMarketShareGetFilterDisplayText(TcxCustomGridTableItem *Sender,
		  const Variant &AValue, UnicodeString &ADisplayText);
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall miTop10Click(TObject *Sender);
	void __fastcall ThreeColorScaleClick(TObject *Sender);
	void __fastcall TwoColorScaleClick(TObject *Sender);
	void __fastcall DataBarClick(TObject *Sender);
	void __fastcall IconSetsClick(TObject *Sender);
	void __fastcall ClearRulesfromThisColumn1Click(TObject *Sender);
	void __fastcall ClearRulesfromAllColumnsClick(TObject *Sender);
	void __fastcall FormDestroy(TObject *Sender);
	void __fastcall FormShow(TObject *Sender);
private:	// User declarations
	TcxDataControllerConditionalFormatting* __fastcall GetConditionalFormatting(void);
protected:
	__property TcxDataControllerConditionalFormatting* ConditionalFormatting = {read=GetConditionalFormatting};
public:		// User declarations
	__fastcall TConditionalFormattingDemoMainForm(TComponent* Owner);

	void __fastcall AddDataBarRule(TColor APositiveBarColor, TColor ANegativeBarColor, bool AIsSolidFill);
	void __fastcall AddThreeColorScaleRule(TColor AColor1, TColor AColor2, TColor AColor3);
	void __fastcall AddTwoColorScaleRule(TColor AColor1, TColor AColor2);
	void __fastcall AddTopBottomRule(TdxSpreadSheetConditionalFormattingRuleTopBottomValuesDirection ADirection, TdxSpreadSheetConditionalFormattingRuleTopBottomValuesValueType AValueType);
	void __fastcall AddAboveOrBelowAverageRule(TdxSpreadSheetConditionalFormattingRuleAboveOrBelowAverageComparisonOperator AComparasionOperator);
	void __fastcall AddIconSetRule(int APresetIndex);
	void __fastcall ClearRulesFromSelectedArea();
	void __fastcall RemoveRulesFromSelectedArea(TdxSpreadSheetCustomConditionalFormattingRuleClass ARuleClass);
};
//---------------------------------------------------------------------------
extern PACKAGE TConditionalFormattingDemoMainForm *ConditionalFormattingDemoMainForm;
//---------------------------------------------------------------------------
#endif
