//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "ColumnsMultiEditorsDemoPopup.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxControls"
#pragma link "cxCustomData"
#pragma link "cxGraphics"
#pragma link "cxInplaceContainer"
#pragma link "cxStyles"
#pragma link "cxTextEdit"
#pragma link "cxTL"
#pragma resource "*.dfm"
//---------------------------------------------------------------------------
__fastcall TColumnsMultiEditorsDemoPopupForm::TColumnsMultiEditorsDemoPopupForm(TComponent* Owner)
  : TForm(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TColumnsMultiEditorsDemoPopupForm::tlPopupGetNodeImageIndex(TcxCustomTreeList *Sender,
    TcxTreeListNode *ANode, TcxTreeListImageIndexType AIndexType,
    TcxImageIndex &AIndex)
{
  if (AIndexType == tlitStateIndex) return;
  if (ANode->Level == 0){
    AIndex = 0;
    if (ANode->Expanded)
      AIndex++;
  }
  else
     AIndex = 4;
  if (ANode->HotTrack)
    if (ANode->Level == 0)
      AIndex += 2;
    else
      AIndex++;
}
//---------------------------------------------------------------------------

void __fastcall TColumnsMultiEditorsDemoPopupForm::tlPopupHotTrackNode(TcxCustomTreeList *Sender, TcxTreeListNode *ANode,
    TShiftState AShift, TCursor &ACursor)
{
  if (ANode->Level == 1)
    ACursor = crHandPoint;
  else
    ACursor = crDefault;
}
//---------------------------------------------------------------------------

void __fastcall TColumnsMultiEditorsDemoPopupForm::tlPopupClick(TObject *Sender)
{
  if ((tlPopup->FocusedNode != NULL) && (tlPopup->FocusedNode->Level == 1))
    ClosePopupForm(true);
}
//---------------------------------------------------------------------------

void __fastcall TColumnsMultiEditorsDemoPopupForm::tlPopupStylesGetHotTrackStyle(TcxCustomTreeList *Sender, TcxTreeListColumn *AColumn,
    TcxTreeListNode *ANode, TcxStyle *&AStyle)
{
  if (ANode->Level == 0)
    AStyle = stlHotRoot;
  else
    AStyle = stlHotItem;
}
//---------------------------------------------------------------------------

void __fastcall TColumnsMultiEditorsDemoPopupForm::tlPopupKeyDown(TObject *Sender, Word &Key,
    TShiftState Shift)
{
  if (Key == VK_ESCAPE)
    ClosePopupForm(false);
  if (Key == VK_RETURN)
    tlPopupClick(NULL);
}
//---------------------------------------------------------------------------

void TColumnsMultiEditorsDemoPopupForm::ClosePopupForm(bool Accept)
{
  if (PopupEdit != NULL){
    PopupEdit->DroppedDown = false;
    if (Accept){
      if (tlPopup->FocusedNode != NULL)
        PopupEdit->EditingText = tlPopup->FocusedNode->Values[0];
    }
  }
}
//---------------------------------------------------------------------------

