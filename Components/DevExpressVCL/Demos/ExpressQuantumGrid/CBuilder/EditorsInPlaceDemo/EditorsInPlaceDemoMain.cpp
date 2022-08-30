//---------------------------------------------------------------------------

#include <vcl.h>
#include "shellapi.hpp"
#pragma hdrstop

#include "EditorsInPlaceDemoMain.h"
#include "EditorsInPlaceDemoData.h"
#include "AboutDemoForm.h"
#include "EditorsInPlaceDemoCities.h"
#include "EditorsInPlaceDemoCars.h"

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
#pragma link "cxGridCardView"
#pragma link "cxGridDBCardView"
#pragma link "cxLookAndFeels"
#pragma link "cxButtonEdit"
#pragma link "cxCalc"
#pragma link "cxCheckBox"
#pragma link "cxDataStorage"
#pragma link "cxDBLookupComboBox"
#pragma link "cxDropDownEdit"
#pragma link "cxGridCustomLayoutView"
#pragma link "cxHyperLinkEdit"
#pragma link "cxImage"
#pragma link "cxImageComboBox"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxMaskEdit"
#pragma link "cxMemo"
#pragma link "cxMRUEdit"
#pragma link "cxSpinEdit"
#pragma link "cxTimeEdit"
#pragma resource "*.dfm"
TEditorsInPlaceDemoMainForm *EditorsInPlaceDemoMainForm;
//---------------------------------------------------------------------------
__fastcall TEditorsInPlaceDemoMainForm::TEditorsInPlaceDemoMainForm(TComponent* Owner)
  : TfmBaseForm(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TEditorsInPlaceDemoMainForm::FormShow(TObject *Sender)
{
  ((TcxPopupEditPropertiesAccess*)cvCarsInfo->Properties)->ImmediatePopup = true;
}
//---------------------------------------------------------------------------

void __fastcall TEditorsInPlaceDemoMainForm::cvCustomersStatePropertiesButtonClick(TObject *Sender)
{
  MessageDlg("Button click event handler", mtInformation, TMsgDlgButtons()<<mbOK, 0);
}
//---------------------------------------------------------------------------

void __fastcall TEditorsInPlaceDemoMainForm::cvCustomersCityPropertiesButtonClick(TObject *Sender,
    int AButtonIndex)
{
  switch (AButtonIndex){
    case 0:
      if (EditorsInPlaceDemoCitiesForm->ShowModal() == mrOk){
        EditorsInPlaceDemoDataDM->tblCustomers->Edit();
        EditorsInPlaceDemoDataDM->tblCustomersCity->Value = EditorsInPlaceDemoCitiesForm->Value;
      }
      break;
    case 1:
      ShellExecute(Handle, PAnsiChar("OPEN"), PAnsiChar("http://www.usacitiesonline.com/"), NULL, NULL, SW_SHOWMAXIMIZED);
  }
}
//---------------------------------------------------------------------------

void __fastcall TEditorsInPlaceDemoMainForm::GridFocusedViewChanged(TcxCustomGrid *Sender,
    TcxCustomGridView *APrevFocusedView, TcxCustomGridView *AFocusedView)
{
  UpdateOptions();
}
//---------------------------------------------------------------------------

void __fastcall TEditorsInPlaceDemoMainForm::miEditButtonsClick(TObject *Sender)
{
  if (!((TMenuItem*)Sender)->Checked) {
    TcxGridDBCardView *AView = (TcxGridDBCardView*)Grid->FocusedView;
    switch (((TMenuItem*)Sender)->Tag){
      case 0:
        AView->OptionsView->ShowEditButtons = gsebNever;
        break;
      case 1:
        AView->OptionsView->ShowEditButtons = gsebForFocusedRecord;
        break;
      case 2:
        AView->OptionsView->ShowEditButtons = gsebAlways;
    }
    ((TMenuItem*)Sender)->Checked = true;
  }
}
//---------------------------------------------------------------------------

void __fastcall TEditorsInPlaceDemoMainForm::cvCarsCustomDrawCell(TcxCustomGridTableView *Sender,
    TcxCanvas *ACanvas, TcxGridTableDataCellViewInfo *AViewInfo,
    bool &ADone)
{
  if ((AViewInfo->Item->Index == cvCarsInfo->Index) &&
    (dynamic_cast< TcxGridCardRowDataViewInfo* >(AViewInfo) != 0)){
    ACanvas->Canvas->Brush->Style = bsSolid;
    ACanvas->Canvas->Brush->Color = (TColor)0x00F7EAD9;
    ACanvas->Canvas->FillRect(AViewInfo->Bounds);
    SetBkMode(ACanvas->Handle, TRANSPARENT);
    ACanvas->Font->Color = clBlack;
    ACanvas->Canvas->TextOut(AViewInfo->Bounds.Left, AViewInfo->Bounds.Top, "<click here and view dropdown window>");
    ADone = true;
  }
}
//---------------------------------------------------------------------------

void TEditorsInPlaceDemoMainForm::UpdateOptions()
{
  switch (((TcxGridDBCardView*)Grid->FocusedView)->OptionsView->ShowEditButtons){
  case gsebNever:
    miEditButtonsNever->Checked = true;
    break;
  case gsebForFocusedRecord:
    miEditButtonsForFocusedRecord->Checked = true;
    break;
  case gsebAlways:
    miEditButtonsAlways->Checked = true;
 }
}
//---------------------------------------------------------------------------

