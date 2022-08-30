//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "SimpleTreeDemoMain.h"
#include "SimpleTreeDemoData.h"
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
#pragma link "cxCheckBox"
#pragma link "cxCurrencyEdit"
#pragma link "cxDBTL"
#pragma link "cxInplaceContainer"
#pragma link "cxMaskEdit"
#pragma link "cxTL"
#pragma link "cxTLData"
#pragma resource "*.dfm"
TSimpleTreeDemoMainForm *SimpleTreeDemoMainForm;
//---------------------------------------------------------------------------
__fastcall TSimpleTreeDemoMainForm::TSimpleTreeDemoMainForm(TComponent* Owner)
  : TDemoBasicMainForm(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TSimpleTreeDemoMainForm::FormShow(TObject *Sender)
{
  cxDBTreeList->FullExpand();

/* remove/add the closing slash on this line to disable/enable the following code *

  ShowMessage("WARNING: tutorial not completed. First, please apply the steps "
    "shown in the doc file");

//*/
}
//---------------------------------------------------------------------------

void __fastcall TSimpleTreeDemoMainForm::mnuNodeOptionsPopup(TObject *Sender)
{
  if (!cxDBTreeList->HitTest->HitAtIndicator) Abort();
  FHitNode = (TcxDBTreeListNode*)cxDBTreeList->HitTest->HitNode;
}
//---------------------------------------------------------------------------

void __fastcall TSimpleTreeDemoMainForm::miFullCollapseClick(TObject *Sender)
{
/* remove/add the closing slash on this line to disable/enable the following code */

  cxDBTreeList->FullCollapse();

//*/
}
//---------------------------------------------------------------------------

void __fastcall TSimpleTreeDemoMainForm::miFullExpandClick(TObject *Sender)
{
/* remove/add the closing slash on this line to disable/enable the following code */

  cxDBTreeList->FullExpand();

//*/
}
//---------------------------------------------------------------------------

void __fastcall TSimpleTreeDemoMainForm::miHeadersClick(TObject *Sender)
{
/* remove/add the closing slash on this line to disable/enable the following code */

  ((TMenuItem*)Sender)->Checked = !((TMenuItem*)Sender)->Checked;
  cxDBTreeList->OptionsView->Headers = ((TMenuItem*)Sender)->Checked;

//*/
}
//---------------------------------------------------------------------------

void __fastcall TSimpleTreeDemoMainForm::miIndicatorClick(TObject *Sender)
{
/* remove/add the closing slash on this line to disable/enable the following code */

  ((TMenuItem*)Sender)->Checked = !((TMenuItem*)Sender)->Checked;
  cxDBTreeList->OptionsView->Indicator = ((TMenuItem*)Sender)->Checked;

//*/
}
//---------------------------------------------------------------------------

void __fastcall TSimpleTreeDemoMainForm::miButtonsClick(TObject *Sender)
{
/* remove/add the closing slash on this line to disable/enable the following code */

  ((TMenuItem*)Sender)->Checked = !((TMenuItem*)Sender)->Checked;
  cxDBTreeList->OptionsView->Buttons = ((TMenuItem*)Sender)->Checked;

//*/
}
//---------------------------------------------------------------------------

void __fastcall TSimpleTreeDemoMainForm::miShowRootClick(TObject *Sender)
{
/* remove/add the closing slash on this line to disable/enable the following code */

  ((TMenuItem*)Sender)->Checked = !((TMenuItem*)Sender)->Checked;
  cxDBTreeList->OptionsView->ShowRoot = ((TMenuItem*)Sender)->Checked;

//*/
}
//---------------------------------------------------------------------------

void __fastcall TSimpleTreeDemoMainForm::miColumnCustomizationClick(
  TObject *Sender)
{
/* remove/add the closing slash on this line to disable/enable the following code */

  cxDBTreeList->Customizing->Visible = true;

//*/
}
//---------------------------------------------------------------------------

void __fastcall TSimpleTreeDemoMainForm::miNodeDeleteClick(TObject *Sender)
{
/* remove/add the closing slash on this line to disable/enable the following code */

  FHitNode->Delete();

//*/
}
//---------------------------------------------------------------------------

void __fastcall TSimpleTreeDemoMainForm::miNodeAddClick(TObject *Sender)
{
/* remove/add the closing slash on this line to disable/enable the following code */

  InsertNode(FHitNode->ParentKeyValue);

//*/
}
//---------------------------------------------------------------------------

void __fastcall TSimpleTreeDemoMainForm::miNodeAddChildClick(TObject *Sender)
{
/* remove/add the closing slash on this line to disable/enable the following code */

  InsertNode(FHitNode->KeyValue);

//*/
}
//---------------------------------------------------------------------------

void __fastcall TSimpleTreeDemoMainForm::miExpandClick(TObject *Sender)
{
/* remove/add the closing slash on this line to disable/enable the following code */

  FHitNode->Expand(true);

//*/
}
//---------------------------------------------------------------------------

void __fastcall TSimpleTreeDemoMainForm::miCollapseClick(TObject *Sender)
{
/* remove/add the closing slash on this line to disable/enable the following code */

  FHitNode->Collapse(true);

//*/
}
//---------------------------------------------------------------------------

void __fastcall TSimpleTreeDemoMainForm::miPreviewClick(TObject *Sender)
{
/* remove/add the closing slash on this line to disable/enable the following code */

  ((TMenuItem*)Sender)->Checked = !((TMenuItem*)Sender)->Checked;
  if (((TMenuItem*)Sender)->Checked)
    cxDBTreeList->Preview->Column = cxDBTreeListNAME;
  else{
    cxDBTreeList->Preview->Column = NULL;
    cxDBTreeListNAME->ApplyBestFit();
  }

//*/
}
//---------------------------------------------------------------------------

void __fastcall TSimpleTreeDemoMainForm::cxDBTreeListInitInsertingRecord(
  TcxCustomDBTreeList *Sender, TcxDBTreeListNode *AFocusedNode, bool &AHandled)
{
//
}
//---------------------------------------------------------------------------

void __fastcall TSimpleTreeDemoMainForm::InsertNode(int AParentID)
{
/* remove/add the closing slash on this line to disable/enable the following code */

  TDataSet *ADataSet = cxDBTreeList->DataController->DataSet;
  ADataSet->DisableControls();
  __try{
    TField *AField = ADataSet->FindField(cxDBTreeList->DataController->ParentField);
    if (AField != NULL){
      ADataSet->Insert();
      AField->Value = AParentID;
    }
  }
  __finally{
    ADataSet->EnableControls();
  }

//*/
}
//---------------------------------------------------------------------------


