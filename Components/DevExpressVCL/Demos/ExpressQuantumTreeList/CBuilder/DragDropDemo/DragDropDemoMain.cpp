//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "DragDropDemoMain.h"
#include "DragDropDemoData.h"
#include "DragDropDemoDictionary.h"
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
TDragDropDemoMainForm *DragDropDemoMainForm;
//---------------------------------------------------------------------------
__fastcall TDragDropDemoMainForm::TDragDropDemoMainForm(TComponent* Owner)
  : TDemoBasicMainForm(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TDragDropDemoMainForm::FormShow(TObject *Sender)
{
  tlDepartments->FullExpand();
  DragDropDemoDictionaryForm->Show();

/* remove/add the closing slash on this line to disable/enable the following code *

  ShowMessage("WARNING: tutorial not completed. First, please apply the steps "
    "shown in the doc file");

//*/
}
//---------------------------------------------------------------------------

void __fastcall TDragDropDemoMainForm::miColumnCustomizationClick(TObject *Sender)
{
  tlDepartments->Customizing->Visible = true;
}
//---------------------------------------------------------------------------

void __fastcall TDragDropDemoMainForm::miDragCollapseClick(TObject *Sender)
{
  ((TMenuItem*)Sender)->Checked = !((TMenuItem*)Sender)->Checked;
  tlDepartments->OptionsBehavior->DragCollapse = ((TMenuItem*)Sender)->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TDragDropDemoMainForm::miDragExpandeClick(TObject *Sender)
{
  ((TMenuItem*)Sender)->Checked = !((TMenuItem*)Sender)->Checked;
  tlDepartments->OptionsBehavior->DragExpand = ((TMenuItem*)Sender)->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TDragDropDemoMainForm::ShowDictionaries1Click(TObject *Sender)
{
  DragDropDemoDictionaryForm->Show();
}
//---------------------------------------------------------------------------

void __fastcall TDragDropDemoMainForm::tlDepartmentsInitInsertingRecord(
  TcxCustomDBTreeList *Sender, TcxDBTreeListNode *AFocusedNode, bool &AHandled)
{
  if (AFocusedNode != NULL)
    DragDropDemoDataDM->SetParentValue(AFocusedNode->ParentKeyValue);
}
//---------------------------------------------------------------------------

void __fastcall TDragDropDemoMainForm::tlDepartmentsDragDrop(TObject *Sender,
  TObject *Source, int X,  int Y)
{
/* remove/add the closing slash on this line to disable/enable the following code */

  if (Sender == Source || !IsHitAtNode((TcxDBTreeList*)Sender, X, Y))
    return;
  TcxDBTreeListNode *AHitNode =
    (TcxDBTreeListNode*)((TcxDBTreeList*)Sender)->HitTest->HitNode;
  if (Source == DragDropDemoDictionaryForm->tlDeptDict){
    if (IsDropAsChild((TcxDBTreeList*)Sender))
      DragDropDemoDictionaryForm->SetDeptSelectionParentValue(AHitNode->KeyValue);
    else
      DragDropDemoDictionaryForm->SetDeptSelectionParentValue(AHitNode->ParentKeyValue);
    ((TcxDBTreeList*)Sender)->DataController->DataSet->Refresh();
  }
  else{
    if (Source == DragDropDemoDictionaryForm->tlEmplDict)
      SetEmplDictSelectionDeptID(AHitNode->KeyValue);
    else
      SetSelectedNodesValue(tlEmployees, tlEmployeesDepartmentID->ItemIndex,
        AHitNode->KeyValue);
  }

//*/
}

bool TDragDropDemoMainForm::IsDropAsChild(TcxDBTreeList *Sender)
{
   return !(Sender->HitTest->HitAtIndent || Sender->HitTest->HitAtIndicator);
}
//---------------------------------------------------------------------------

void __fastcall TDragDropDemoMainForm::tlDepartmentsDragOver(TObject *Sender,
  TObject *Source, int X, int Y, TDragState State, bool &Accept)
{
/* remove/add the closing slash on this line to disable/enable the following code */

  Accept = IsHitAtNode((TcxDBTreeList*)Sender, X, Y);

//*/
}
//---------------------------------------------------------------------------

void __fastcall TDragDropDemoMainForm::tlEmployeesDragDrop(TObject *Sender,
  TObject *Source, int X,  int Y)
{
/* remove/add the closing slash on this line to disable/enable the following code */

  if (Source == DragDropDemoDictionaryForm->tlEmplDict){
    Variant AValue;
    if (tlDepartments->FocusedNode != NULL)
      AValue = ((TcxDBTreeListNode*)tlDepartments->FocusedNode)->KeyValue;
    else
      AValue = -1;
    SetEmplDictSelectionDeptID(AValue);
  }

//*/
}
//---------------------------------------------------------------------------

void __fastcall TDragDropDemoMainForm::tlEmployeesDragOver(TObject *Sender,
  TObject *Source, int X, int Y, TDragState State, bool &Accept)
{
/* remove/add the closing slash on this line to disable/enable the following code */

  Accept = (Source == DragDropDemoDictionaryForm->tlEmplDict);

//*/
}
//---------------------------------------------------------------------------

void __fastcall TDragDropDemoMainForm::tlEmployeesMoveTo(TcxCustomTreeList *Sender,
  TcxTreeListNode *AttachNode, TcxTreeListNodeAttachMode AttachMode,
  TList Nodes, bool &IsCopy, bool &Done)
{
/* remove/add the closing slash on this line to disable/enable the following code */

  Done = true;

//*/
}
//---------------------------------------------------------------------------

bool TDragDropDemoMainForm::IsHitAtNode(TcxDBTreeList *ATreeList, int X, int Y)
{
    ATreeList->HitTest->ReCalculate(Point(X,Y));
    return ATreeList->HitTest->HitAtNode;
}
//---------------------------------------------------------------------------

void TDragDropDemoMainForm::SetEmplDictSelectionDeptID(Variant AValue)
{
  DragDropDemoDictionaryForm->SetEmplSelectionDeptID(AValue);
  tlEmployees->DataController->DataSet->Close();
  tlEmployees->DataController->DataSet->Open();
}
//---------------------------------------------------------------------------


