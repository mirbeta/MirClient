//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "BandedFixedDemoMain.h"
#include "BandedFixedDemoData.h"
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
#pragma link "cxDBLookupComboBox"
#pragma link "cxDBTL"
#pragma link "cxEditRepositoryItems"
#pragma link "cxInplaceContainer"
#pragma link "cxMaskEdit"
#pragma link "cxTL"
#pragma link "cxTLData"
#pragma resource "*.dfm"
TBandedFixedDemoMainForm *BandedFixedDemoMainForm;
//---------------------------------------------------------------------------
__fastcall TBandedFixedDemoMainForm::TBandedFixedDemoMainForm(TComponent* Owner)
  : TDemoBasicMainForm(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TBandedFixedDemoMainForm::FormShow(TObject *Sender)
{
  if (cxDBTreeList->TopNode != NULL){
    cxDBTreeList->TopNode->Focused = true;
    cxDBTreeList->TopNode->MakeVisible();
  }

/* remove/add the closing slash on this line to disable/enable the following code *

  ShowMessage("WARNING: tutorial not completed. First, please apply the steps "
    "shown in the doc file");

//*/
}
//---------------------------------------------------------------------------

void __fastcall TBandedFixedDemoMainForm:: actCustomizationFormExecute(TObject *Sender)
{
  cxDBTreeList->Customizing->Visible = true;
}
//---------------------------------------------------------------------------

void __fastcall TBandedFixedDemoMainForm:: miBandHorzSizingClick(TObject *Sender)
{
  ((TMenuItem*)Sender)->Checked = !((TMenuItem*)Sender)->Checked;
  cxDBTreeList->OptionsCustomizing->BandHorzSizing = ((TMenuItem*)Sender)->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TBandedFixedDemoMainForm:: miBandVertSizingClick(TObject *Sender)
{
  ((TMenuItem*)Sender)->Checked = !((TMenuItem*)Sender)->Checked;
  cxDBTreeList->OptionsCustomizing->BandVertSizing = ((TMenuItem*)Sender)->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TBandedFixedDemoMainForm:: miBandMovingClick(TObject *Sender)
{
  ((TMenuItem*)Sender)->Checked = !((TMenuItem*)Sender)->Checked;
  cxDBTreeList->OptionsCustomizing->BandMoving = ((TMenuItem*)Sender)->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TBandedFixedDemoMainForm:: mnuNodeOptionsPopup(TObject *Sender)
{
  FHitBand = cxDBTreeList->HitTest->HitBand;
  if (FHitBand == NULL)
    Abort();
  else
    miFixBand->Items[(int)FHitBand->FixedKind]->Checked = true;
}
//---------------------------------------------------------------------------

void __fastcall TBandedFixedDemoMainForm:: miBandHideClick(TObject *Sender)
{
  FHitBand->Visible = false;
}
//---------------------------------------------------------------------------

void __fastcall TBandedFixedDemoMainForm::cxDBTreeListStylesGetContentStyle(
          TcxCustomTreeList *Sender, TcxTreeListColumn *AColumn,
          TcxTreeListNode *ANode, TcxStyle *&AStyle)
{
  if (ANode->IsGroupNode)
    AStyle = BandedFixedDemoDataDM->stlGroupNode;
}
//---------------------------------------------------------------------------

void __fastcall TBandedFixedDemoMainForm:: cxDBTreeListEmployeeGetDisplayText(
    TcxTreeListColumn *Sender, TcxTreeListNode *ANode, String &Value)
{
  if (ANode->IsGroupNode){
    int AProjectID = ANode->Values[cxDBTreeListPROJECTID->ItemIndex];
    Value = "Project: " + BandedFixedDemoDataDM->GetProjectNameByID(AProjectID)+
      "; Manager: " + BandedFixedDemoDataDM->GetPersonNameByID(
      ((TcxDBTreeListNode*)ANode)->KeyValue) + " (" + IntToStr(ANode->Count) + " employees)" ;
  }
}
//---------------------------------------------------------------------------

void __fastcall TBandedFixedDemoMainForm:: cxDBTreeListEmployeeGetEditProperties(
    TcxTreeListColumn *Sender, TcxTreeListNode *ANode,
    TcxCustomEditProperties *&EditProperties)
{
  if (ANode->IsGroupNode)
    EditProperties = cxEditRepository1TextItem1->Properties;
}
//---------------------------------------------------------------------------

void __fastcall TBandedFixedDemoMainForm:: miShowBandsClick(TObject *Sender)
{
  ((TMenuItem*)Sender)->Checked = !((TMenuItem*)Sender)->Checked;
  cxDBTreeList->OptionsView->Bands = ((TMenuItem*)Sender)->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TBandedFixedDemoMainForm:: miShowHeadersClick(TObject *Sender)
{
  ((TMenuItem*)Sender)->Checked = !((TMenuItem*)Sender)->Checked;
  cxDBTreeList->OptionsView->Headers = ((TMenuItem*)Sender)->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TBandedFixedDemoMainForm:: miFixBandClick(TObject *Sender)
{
  ((TMenuItem*)Sender)->Checked = true;
  FHitBand->FixedKind = TcxTreeListBandFixedKind(((TMenuItem*)Sender)->Tag);
}
//---------------------------------------------------------------------------


