//---------------------------------------------------------------------------

#include <vcl.h>
#include "shellapi.hpp"
#pragma hdrstop

#include "ColumnsSimpleDemoMain.h"
#include "AboutDemoForm.h"
#include "ColumnsSimpleDemoData.h"
#include "ColumnsSimpleDemoCars.h"
#include "ColumnsSimpleDemoCities.h"
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
#pragma link "cxButtonEdit"
#pragma link "cxCalc"
#pragma link "cxCheckBox"
#pragma link "cxDataStorage"
#pragma link "cxDBLookupComboBox"
#pragma link "cxDropDownEdit"
#pragma link "cxImageComboBox"
#pragma link "cxMaskEdit"
#pragma link "cxMRUEdit"
#pragma link "cxSpinEdit"
#pragma link "cxTimeEdit"
#pragma link "cxGridCardView"
#pragma link "cxLookAndFeelPainters"
#pragma resource "*.dfm"
TColumnsSimpleDemoMainForm *ColumnsSimpleDemoMainForm;
//---------------------------------------------------------------------------
__fastcall TColumnsSimpleDemoMainForm::TColumnsSimpleDemoMainForm(TComponent* Owner)
  : TfmBaseForm(Owner)
{
}
//---------------------------------------------------------------------------

TcxGridTableView* TColumnsSimpleDemoMainForm::GetView()
{
  return (TcxGridTableView*)Grid->FocusedView;
}
//---------------------------------------------------------------------------

void __fastcall TColumnsSimpleDemoMainForm::UpdateMenu()
{
  miShowEditButtons->Items[(int)View->OptionsView->ShowEditButtons]->Checked = true;

  miCellSelect->Checked = View->OptionsSelection->CellSelect;
  miHideFocusRect->Checked = View->OptionsSelection->HideFocusRect;
  miHideSelection->Checked = View->OptionsSelection->HideSelection;
  miInvertSelect->Checked = View->OptionsSelection->InvertSelect;
  miMultiSelect->Checked = View->OptionsSelection->MultiSelect;
}
//---------------------------------------------------------------------------

void __fastcall TColumnsSimpleDemoMainForm::FormShow(TObject *Sender)
{
  if(tvCustomers->Controller->FocusedRecord != NULL)
    tvCustomers->Controller->FocusedRecord->Expanded = true;
}
//---------------------------------------------------------------------------

void __fastcall TColumnsSimpleDemoMainForm::miEditButtonsClick(TObject *Sender)
{
  View->OptionsView->ShowEditButtons = (TcxGridShowEditButtons)((TMenuItem*)Sender)->MenuIndex;
  UpdateMenu();
}
//---------------------------------------------------------------------------

void __fastcall TColumnsSimpleDemoMainForm::miCellSelectClick(TObject *Sender)
{
  View->OptionsSelection->CellSelect = !View->OptionsSelection->CellSelect;
  UpdateMenu();
}
//---------------------------------------------------------------------------

void __fastcall TColumnsSimpleDemoMainForm::miHideFocusRectClick(TObject *Sender)
{
  View->OptionsSelection->HideFocusRectOnExit = !View->OptionsSelection->HideFocusRectOnExit;
  UpdateMenu();
}
//---------------------------------------------------------------------------

void __fastcall TColumnsSimpleDemoMainForm::miHideSelectionClick(TObject *Sender)
{
  View->OptionsSelection->HideSelection = !View->OptionsSelection->HideSelection;
  UpdateMenu();
}
//---------------------------------------------------------------------------

void __fastcall TColumnsSimpleDemoMainForm::miInvertSelectClick(TObject *Sender)
{
  View->OptionsSelection->InvertSelect = !View->OptionsSelection->InvertSelect;
  UpdateMenu();
}
//---------------------------------------------------------------------------

void __fastcall TColumnsSimpleDemoMainForm::miMultiSelectClick(TObject *Sender)
{
  View->OptionsSelection->MultiSelect = !View->OptionsSelection->MultiSelect;
  UpdateMenu();
}
//---------------------------------------------------------------------------

void __fastcall TColumnsSimpleDemoMainForm::tvCustomersStatePropertiesButtonClick(TObject *Sender)
{
  MessageDlg("Button click event handler", mtInformation, TMsgDlgButtons() << mbOK, 0);
}
//---------------------------------------------------------------------------

void __fastcall TColumnsSimpleDemoMainForm::tvCustomersCityPropertiesButtonClick(
      TObject *Sender, int AButtonIndex)
{
  switch(AButtonIndex) {
    case 0:
      if (ColumnsSimpleDemoCitiesForm->ShowModal() == mrOk) {
        ColumnsSimpleDemoDataDM->tblCustomers->Edit();
        ColumnsSimpleDemoDataDM->tblCustomersCity->Value = ColumnsSimpleDemoCitiesForm->Value;
      }; break;
    case 1:
      ShellExecute(Handle, "OPEN", "http://www.usacitiesonline.com/", NULL, NULL, SW_SHOWMAXIMIZED); break;
  }
}
//---------------------------------------------------------------------------

void __fastcall TColumnsSimpleDemoMainForm::tvOrdersCarInfoPropertiesInitPopup(TObject *Sender)
{
  ColumnSimpleDemoCarsForm->bvCars->DataController->LocateByKey(
    ColumnsSimpleDemoDataDM->tblOrdersProductID->AsInteger);
}
//---------------------------------------------------------------------------

void __fastcall TColumnsSimpleDemoMainForm::tvOrdersCarInfoPropertiesCloseUp(TObject *Sender)
{
  ColumnsSimpleDemoDataDM->tblOrders->Edit();
  ColumnsSimpleDemoDataDM->tblOrdersProductID->Value = dmGridCars->mdModelsID->Value;
}
//---------------------------------------------------------------------------

void __fastcall TColumnsSimpleDemoMainForm::GridFocusedViewChanged(
      TcxCustomGrid *Sender, TcxCustomGridView *APrevFocusedView,
      TcxCustomGridView *AFocusedView)
{
  UpdateMenu();
}
//---------------------------------------------------------------------------

void __fastcall TColumnsSimpleDemoMainForm::tvCustomersCompanyGetCellHint(
      TcxCustomGridTableItem *Sender, TcxCustomGridRecord *ARecord,
      TcxGridTableDataCellViewInfo *ACellViewInfo, const TPoint &AMousePos,
      TCaption &AHintText, bool &AIsHintMultiLine, TRect &AHintTextRect)
{
  AHintText = ARecord->DisplayTexts[tvCustomersAddress->Index] + CHAR(13) +
    ARecord->DisplayTexts[tvCustomersCity->Index] + ", " + ARecord->DisplayTexts[tvCustomersState->Index] + " " +
    ARecord->DisplayTexts[tvCustomersZipCode->Index] + CHAR(13) +
    ARecord->DisplayTexts[tvCustomersFaxPhone->Index];
  AHintTextRect.Top = ACellViewInfo->Bounds.Bottom + 3;
}
//---------------------------------------------------------------------------

