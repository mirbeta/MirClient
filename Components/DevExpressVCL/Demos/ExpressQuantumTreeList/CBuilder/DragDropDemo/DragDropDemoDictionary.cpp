//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "DragDropDemoDictionary.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxCheckBox"
#pragma link "cxControls"
#pragma link "cxCurrencyEdit"
#pragma link "cxCustomData"
#pragma link "cxDBTL"
#pragma link "cxGraphics"
#pragma link "cxInplaceContainer"
#pragma link "cxMaskEdit"
#pragma link "cxStyles"
#pragma link "cxTL"
#pragma link "cxTLData"
#pragma resource "*.dfm"
TDragDropDemoDictionaryForm *DragDropDemoDictionaryForm;

int __fastcall cxCompareNodes(void *AItem1, void *AItem2)
{
  return ((TcxDBTreeListNode*)AItem2)->KeyValue -
    ((TcxDBTreeListNode*)AItem1)->KeyValue;
}

void SetSelectedNodesValue(TcxDBTreeList *ATreeList, int AItemIndex,
  Variant AValue)
{
  TList *AList = new TList();
  ATreeList->BeginUpdate();
  __try{
    ATreeList->GetSelections(AList);
    AList->Sort(cxCompareNodes);
    for (int I = 0; I < AList->Count; I++)
      if (((TcxTreeListNode*)AList->Items[I])->Values[AItemIndex] != AValue){
        ((TcxTreeListNode*)AList->Items[I])->Focused = true;
        ATreeList->DataController->Edit();
        ((TcxTreeListNode*)AList->Items[I])->Values[AItemIndex] = AValue;
        ATreeList->DataController->Post();
      }
    }
  __finally{
    delete AList;
    ATreeList->EndUpdate();
  }
}

//---------------------------------------------------------------------------

__fastcall TDragDropDemoDictionaryForm::TDragDropDemoDictionaryForm(TComponent* Owner)
  : TForm(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TDragDropDemoDictionaryForm::tlDictDragOver(TObject *Sender,
  TObject* Source, int X, int Y, TDragState State, bool &Accept)
{
  Accept = false;
}
//---------------------------------------------------------------------------

void __fastcall TDragDropDemoDictionaryForm::tlDictMoveTo(TcxCustomTreeList *Sender,
  TcxTreeListNode *AttachNode, TcxTreeListNodeAttachMode AttachMode,
  TList *Nodes, bool &IsCopy, bool &Done)
{
  Done = true;
}
//---------------------------------------------------------------------------

void TDragDropDemoDictionaryForm::SetDeptSelectionParentValue(Variant AParentValue)
{
  SetSelectedNodesValue(tlDeptDict, tlDeptDictPARENTID->ItemIndex, AParentValue);
}
//---------------------------------------------------------------------------

void TDragDropDemoDictionaryForm::SetEmplSelectionDeptID(Variant ADepartmentID)
{
  SetSelectedNodesValue(tlEmplDict, tlEmplDictDepartmentID->ItemIndex, ADepartmentID);
}
//---------------------------------------------------------------------------

