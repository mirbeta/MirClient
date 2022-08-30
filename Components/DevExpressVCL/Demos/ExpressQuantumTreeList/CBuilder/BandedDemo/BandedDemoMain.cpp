//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "BandedDemoMain.h"
#include "BandedDemoData.h"
#include "BandedDemoBands.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxControls"
#pragma link "cxClasses"
#pragma link "cxLookAndFeels"
#pragma link "cxGraphics"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxCustomData"
#pragma link "cxData"
#pragma link "cxDBData"
#pragma link "cxEdit"
#pragma link "cxFilter"
#pragma link "cxGraphics"
#pragma link "cxStyles"
#pragma link "cxLookAndFeels"
#pragma link "DemoBasicMain"
#pragma link "cxDBLookupComboBox"
#pragma link "cxDBTL"
#pragma link "cxEditRepositoryItems"
#pragma link "cxInplaceContainer"
#pragma link "cxMaskEdit"
#pragma link "cxTL"
#pragma link "cxTLData"
#pragma link "cxCheckBox"
#pragma resource "*.dfm"
TBandedDemoMainForm *BandedDemoMainForm;
//---------------------------------------------------------------------------
__fastcall TBandedDemoMainForm::TBandedDemoMainForm(TComponent* Owner)
  : TDemoBasicMainForm(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TBandedDemoMainForm::FormShow(TObject *Sender)
{
/* remove/add the closing slash on this line to disable/enable the following code */

  cxDBTreeList->FullExpand();
  cxDBTreeListNAME->ApplyBestFit();

//*/

/* remove/add the closing slash on this line to disable/enable the following code *

  ShowMessage("WARNING: tutorial not completed. First, please apply the steps "
    "shown in the doc file");

//*/
}
//---------------------------------------------------------------------------


void __fastcall TBandedDemoMainForm::miAddBandClick(TObject *Sender)
{
/* remove/add the closing slash on this line to disable/enable the following code */

  String ABandCaption;
  if (InputQuery("Create band", "Specify a caption of the band", ABandCaption))
    if (GetBandByCaption(ABandCaption) != NULL)
       MessageDlg("Band with this caption already exists", mtWarning, TMsgDlgButtons()<<mbOK, 0);
    else {
      TcxTreeListBand *ABand = cxDBTreeList->Bands->Add();
      ABand->Caption->Text = ABandCaption;
      ABand->Caption->AlignHorz = taCenter;
    }

//*/
}
//---------------------------------------------------------------------------

void TBandedDemoMainForm::AddBands(TStrings *AStringList)
{
  for (int I = 0; I < cxDBTreeList->Bands->Count; I++)
    AStringList->AddObject(cxDBTreeList->Bands->Items[I]->Caption->Text,
      cxDBTreeList->Bands->Items[I]);
}

void TBandedDemoMainForm::RemoveBands(TcxListBox *AListBox)
{
  for (int I = 0; I < AListBox->Items->Count; I++)
    if (AListBox->Selected[I])
      ((TcxTreeListBand*)AListBox->Items->Objects[I])->Free();
}

void __fastcall TBandedDemoMainForm::miRemoveBandsClick(TObject *Sender)
{
/* remove/add the closing slash on this line to disable/enable the following code */

  TBandedDemoBandsForm *AForm = new TBandedDemoBandsForm(NULL);
  __try{
    AForm->lbBands->Items->Clear();
    AddBands(AForm->lbBands->Items);
    if (AForm->ShowModal() == mrOk)
      RemoveBands(AForm->lbBands);
  }
  __finally{
    AForm->Free();
  }

//*/
}
//---------------------------------------------------------------------------

void __fastcall TBandedDemoMainForm::actCustomizationFormExecute(TObject *Sender)
{
  cxDBTreeList->Customizing->Visible = true;
}
//---------------------------------------------------------------------------

void __fastcall TBandedDemoMainForm::miBandHorzSizingClick(TObject *Sender)
{
  ((TMenuItem*)Sender)->Checked = !((TMenuItem*)Sender)->Checked;
  cxDBTreeList->OptionsCustomizing->BandHorzSizing = ((TMenuItem*)Sender)->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TBandedDemoMainForm::miBandVertSizingClick(TObject *Sender)
{
  ((TMenuItem*)Sender)->Checked = !((TMenuItem*)Sender)->Checked;
  cxDBTreeList->OptionsCustomizing->BandVertSizing = ((TMenuItem*)Sender)->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TBandedDemoMainForm::miBandMovingClick(TObject *Sender)
{
  ((TMenuItem*)Sender)->Checked = !((TMenuItem*)Sender)->Checked;
  cxDBTreeList->OptionsCustomizing->BandMoving = ((TMenuItem*)Sender)->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TBandedDemoMainForm::mnuNodeOptionsPopup(TObject *Sender)
{
  FHitBand = cxDBTreeList->HitTest->HitBand;
  if (FHitBand == NULL)
    Abort();
}
//---------------------------------------------------------------------------

void __fastcall TBandedDemoMainForm::miBandDeleteClick(TObject *Sender)
{
  FHitBand->Free();
}
//---------------------------------------------------------------------------

void __fastcall TBandedDemoMainForm::miBandHideClick(TObject *Sender)
{
  FHitBand->Visible = false;
}
//---------------------------------------------------------------------------

void __fastcall TBandedDemoMainForm::cxDBTreeListInitInsertingRecord(
  TcxCustomDBTreeList *Sender, TcxDBTreeListNode *AFocusedNode, bool &AHandled)
{
  if (AFocusedNode != NULL)
  {
    BandedDemoDataDM->SetParentValue(AFocusedNode->ParentKeyValue);
  }
}
//---------------------------------------------------------------------------

TcxTreeListBand* TBandedDemoMainForm::GetBandByCaption(String ABandCaption)
{
  TcxTreeListBand *Result = NULL;
  for (int I = 0; I < cxDBTreeList->Bands->Count; I++)
    if (cxDBTreeList->Bands->Items[I]->Caption->Text == ABandCaption){
      Result = cxDBTreeList->Bands->Items[I];
      break;
    }
  return Result;
}
//---------------------------------------------------------------------------

Variant TBandedDemoMainForm::GetFocusedNodeParentValue()
{
  return ((TcxDBTreeListNode*)cxDBTreeList->FocusedNode)->ParentKeyValue;
}
//---------------------------------------------------------------------------
