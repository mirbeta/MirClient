//---------------------------------------------------------------------------

#include <vcl.h>
#include "..\cxDemosBCB.inc"
#pragma hdrstop

#include "SummariesDemoMain.h"
#include "SummariesDemoData.h"
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
#pragma link "cxStyles"
#pragma link "cxLookAndFeels"
#pragma link "DemoBasicMain"
#pragma link "cxCalc"
#pragma link "cxDBEditRepository"
#pragma link "cxDBLookupComboBox"
#pragma link "cxDBTL"
#pragma link "cxEditRepositoryItems"
#pragma link "cxImageComboBox"
#pragma link "cxInplaceContainer"
#pragma link "cxMaskEdit"
#pragma link "cxSpinEdit"
#pragma link "cxTimeEdit"
#pragma link "cxTL"
#pragma link "cxTLData"
#pragma link "cxCalendar"
#pragma link "cxCurrencyEdit"
#pragma link "cxCheckBox"
#pragma resource "*.dfm"
TSummariesDemoMainForm *SummariesDemoMainForm;
//---------------------------------------------------------------------------
__fastcall TSummariesDemoMainForm::TSummariesDemoMainForm(TComponent* Owner)
  : TDemoBasicMainForm(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TSummariesDemoMainForm::clNameTcxTreeListColumnSummaryGroupFooterSummaryItems0GetText(TcxTreeListSummaryItem *Sender,
          const Variant &AValue, String &AText)
{
  if (FAlternateCounting)
  {
	if (AValue < 40)
	  AText = "Less than 40";
	else
	  AText = "Equal to or Greater than 40";
  }
}
//---------------------------------------------------------------------------

void __fastcall TSummariesDemoMainForm::tlDepartmentsAfterSummary(TObject *Sender)

{
  TcxTreeListNode* ANode = tlDepartments->FindNodeByText("Top Management", clName);
  if (ANode == NULL) return;
  TcxTreeListSummaryItem* ASummaryItem = clName->Summary->GroupFooterSummaryItems->GetItemByKind(skCount);
  if (ASummaryItem == NULL) return;
  String ASummaryText = ANode->FooterSummaryTexts[ASummaryItem->AbsoluteIndex];
  String AAboutChildDepartmentInclusionText;
  if (ASummaryItem->AllNodes)
	AAboutChildDepartmentInclusionText = "included";
  else
	AAboutChildDepartmentInclusionText = "excluded";

  lblSummary->Caption = "The'''Top Management'' department has " + ASummaryText +
    " departments (child departments are " + 
    AAboutChildDepartmentInclusionText + "), matching the specified criteria";
}
//---------------------------------------------------------------------------

void __fastcall TSummariesDemoMainForm::tlDepartmentsSummary(TcxCustomTreeList *ASender,
          const TcxTreeListSummaryEventArguments &Arguments, TcxTreeListSummaryEventOutArguments &OutArguments)

{
// If a department doesn't match the specified criteria, exclude its
// values from use in automatic summary calculations 
 OutArguments.Done = FCheckBudget &&
	(Arguments.Node->Values[clBudget->ItemIndex] <= 100000) ||
	FCheckVacancies && !Arguments.Node->Values[clVacancy->ItemIndex].boolVal;
}
//---------------------------------------------------------------------------

void __fastcall TSummariesDemoMainForm::tlDepartmentsPopupMenusFooterMenuClick(TcxCustomTreeList *Sender,
          TObject *AItem, bool &AHandled)
{
  switch (((TComponent*)AItem)->Tag)
  {
	case tlcmUser:
	  FCheckBudget = !FCheckBudget;
	break;
	case tlcmUser + 1:
	  FCheckVacancies = !FCheckVacancies;
	break;
	case tlcmUser + 2:
	  FAlternateCounting = !FAlternateCounting;
	break;
  };
  tlDepartments->Summary->Recalculate();
}
//---------------------------------------------------------------------------

void __fastcall TSummariesDemoMainForm::tlDepartmentsPopupMenusFooterMenuPopup(TcxCustomTreeList *Sender,
          TcxTreeListPopupMenu *AContextMenu, bool &AHandled)
{
  AContextMenu->CreateMenuItem(AContextMenu->Root, "Budget exceeds 100000", tlcmUser, true,
	tlmitChecked, FCheckBudget, -1, true);
  AContextMenu->CreateMenuItem(AContextMenu->Root, "Department has vacancies", tlcmUser + 1, true,
	tlmitChecked, FCheckVacancies, 0);
  AContextMenu->CreateMenuItem(AContextMenu->Root, "Alternate department tally method", tlcmUser + 2, true,
	tlmitChecked, FAlternateCounting);
}
//---------------------------------------------------------------------------

void __fastcall TSummariesDemoMainForm::FormCreate(TObject *Sender)
{
  TDemoBasicMainForm::FormCreate(Sender);
  tlDepartments->PopupMenus->FooterMenu->UserImages = ilUser;
  tlDepartments->PopupMenus->GroupFooterMenu->UserImages = ilUser;
}
//---------------------------------------------------------------------------

