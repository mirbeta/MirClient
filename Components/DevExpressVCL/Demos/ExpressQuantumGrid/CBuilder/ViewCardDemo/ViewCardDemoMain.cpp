//---------------------------------------------------------------------------

#include <vcl.h>
#include "shellapi.hpp"
#pragma hdrstop

#include "ViewCardDemoMain.h"
#include "ViewCardDemoData.h"
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
#pragma link "cxButtons"
#pragma link "cxCheckBox"
#pragma link "cxContainer"
#pragma link "cxGridCardView"
#pragma link "cxGridDBCardView"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxMaskEdit"
#pragma link "cxSpinEdit"
#pragma link "cxTextEdit"
#pragma link "cxLookAndFeels"
#pragma link "cxDataStorage"
#pragma link "cxDBLookupComboBox"
#pragma link "cxGridCustomLayoutView"
#pragma link "cxHyperLinkEdit"
#pragma link "cxImageComboBox"
#pragma link "cxMemo"
#pragma resource "*.dfm"
TViewCardDemoMainForm *ViewCardDemoMainForm;
//---------------------------------------------------------------------------
__fastcall TViewCardDemoMainForm::TViewCardDemoMainForm(TComponent* Owner)
  : TfmBaseForm(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TViewCardDemoMainForm::miShowEmptyRowsClick(TObject *Sender)
{
  cvPersons->OptionsView->EmptyRows = ((TMenuItem*)Sender)->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TViewCardDemoMainForm::miFilteringClick(TObject *Sender)
{
  cvPersons->OptionsCustomize->RowFiltering = ((TMenuItem*)Sender)->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TViewCardDemoMainForm::miExpandingCollapsingClick(TObject *Sender)
{
  cvPersons->OptionsCustomize->CardExpanding = ((TMenuItem*)Sender)->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TViewCardDemoMainForm::miRowsCustomizationClick(TObject *Sender)
{
  cvPersons->Controller->Customization = true;
}
//---------------------------------------------------------------------------


