//---------------------------------------------------------------------------
#include <vcl.h>
#include "shellapi.hpp"
#pragma hdrstop

#include "DragDropMain.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "dxNavBar"
#pragma link "dxNavBarBase"
#pragma link "dxNavBarCollns"
#pragma resource "*.dfm"
#pragma resource "*.res"
TfmDragDropMain *fmDragDropMain;
//---------------------------------------------------------------------------
__fastcall TfmDragDropMain::TfmDragDropMain(TComponent* Owner)
        : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TfmDragDropMain::lvComponentsStartDrag(TObject *Sender,
      TDragObject *&DragObject)
{
  if (lvComponents->Selected != NULL)
    dxNavBarDragObject = new TdxNavBarDragObject(nbMain, DragObject, NULL, NULL,
      (TdxNavBarItem*)lvComponents->Selected->Data);
}
//---------------------------------------------------------------------------

void __fastcall TfmDragDropMain::lvComponentsEndDrag(TObject *Sender, 
  TObject *Target, int X, int Y)
{
  delete dxNavBarDragObject;
  dxNavBarDragObject = NULL;
}  
//---------------------------------------------------------------------------

void __fastcall TfmDragDropMain::imRecycleBinDragOver(TObject *Sender,
      TObject *Source, int X, int Y, TDragState State, bool &Accept)
{
  if ((dxNavBarDragObject != NULL) && ((dxNavBarDragObject->SourceLink != NULL) ||
    (dxNavBarDragObject->SourceGroup != NULL)))
  {
    Accept = true;
    if (State == dsDragEnter)
      imRecycleBin->Picture->Bitmap->LoadFromResourceName((unsigned int)HInstance, "RECYCLE2");
    else if (State == dsDragLeave)
      imRecycleBin->Picture->Bitmap->LoadFromResourceName((unsigned int)HInstance, "RECYCLE1");
  }
  else Accept = false;
}
//---------------------------------------------------------------------------

void __fastcall TfmDragDropMain::nbMainEndDrag(TObject *Sender, TObject *Target,
      int X, int Y)
{
  if (Target != NULL)
    if (Target->ClassType() == __classid(TImage))
    {
      if (dxNavBarDragObject->SourceGroup != NULL)
        nbMain->Groups->Delete(dxNavBarDragObject->SourceGroup->Index);
      if (dxNavBarDragObject->SourceLink != NULL)
        dxNavBarDragObject->SourceLink->Group->RemoveLink(dxNavBarDragObject->SourceLink->Index);
      imRecycleBin->Picture->Bitmap->LoadFromResourceName((unsigned int)HInstance, "RECYCLE1");
    }
}
//---------------------------------------------------------------------------

void __fastcall TfmDragDropMain::FormCreate(TObject *Sender)
{
  for(int i = 0; i < nbMain->Items->Count; i ++)
  {
    TListItem* lItem = lvComponents->Items->Add();
    lItem->Caption = nbMain->Items->Items[i]->Caption;
    lItem->ImageIndex = nbMain->Items->Items[i]->SmallImageIndex;
    lItem->Data = nbMain->Items->Items[i];
  }
  actDragLink->Checked = nbMain->DragDropFlags.Contains(fAllowDragLink);
  actDragGroup->Checked = nbMain->DragDropFlags.Contains(fAllowDragGroup);
  actDropLink->Checked = nbMain->DragDropFlags.Contains(fAllowDropLink);
  actDropGroup->Checked = nbMain->DragDropFlags.Contains(fAllowDropGroup);
  actSelectLinks->Checked = nbMain->AllowSelectLinks;
}
//---------------------------------------------------------------------------

void __fastcall TfmDragDropMain::actDragDropOptionExecute(TObject *Sender)
{
  ((TAction*)Sender)->Checked = !((TAction*)Sender)->Checked;
  if (((TAction*)Sender)->Checked)
    nbMain->DragDropFlags =
        nbMain->DragDropFlags << (TdxNavBarDragDropFlag)((TAction*)Sender)->Tag;
  else
    nbMain->DragDropFlags =
        nbMain->DragDropFlags >> (TdxNavBarDragDropFlag)((TAction*)Sender)->Tag;
}
//---------------------------------------------------------------------------

void __fastcall TfmDragDropMain::actSelectLinksExecute(TObject *Sender)
{
  ((TAction*)Sender)->Checked = !((TAction*)Sender)->Checked;
  nbMain->AllowSelectLinks = ((TAction*)Sender)->Checked;
}
//---------------------------------------------------------------------------


