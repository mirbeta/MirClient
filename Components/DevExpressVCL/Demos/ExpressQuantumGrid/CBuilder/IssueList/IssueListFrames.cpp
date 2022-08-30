//---------------------------------------------------------------------------
#include "IssueListFrames.h"


TFrameManager*  FInstance;

TFrameManager* FrameManager()
{
  if (!FInstance)
  FInstance = new TFrameManager();

  return FInstance;
}
//---------------------------------------------------------------------------


TFrameInfo::TFrameInfo(int ID, String ACaption)
{
  FID = ID;
  FCaption = ACaption;
  FFrame = NULL;
}
//---------------------------------------------------------------------------

void __fastcall TFrameInfo::CreateFrame(TfrmBasic* AFrame)
{
  FFrame = AFrame;
  Frame->Caption = FCaption;
}
//---------------------------------------------------------------------------

void __fastcall TFrameInfo::DestroyFrame()
{
  delete FFrame;
  FFrame = NULL;
}
//---------------------------------------------------------------------------

void __fastcall TFrameInfo::HideFrame()
{
  Frame->Visible = false;
}
//---------------------------------------------------------------------------

void __fastcall TFrameInfo::ShowFrame(TWinControl* AParent)
{
  Frame->Parent = AParent;
  Frame->Visible = true;
}
//---------------------------------------------------------------------------

TFrameManager::TFrameManager()
{
  FFrameInfoList = new TList();
  FActiveFrameInfo = NULL;
}
//---------------------------------------------------------------------------

TFrameManager::~TFrameManager()
{
  for (int i = 0; i < FrameInfoList->Count; i++)
    delete (TFrameInfo*)FrameInfoList->Items[i];

  delete FFrameInfoList;
}
//---------------------------------------------------------------------------

TFrameInfo* __fastcall TFrameManager::GetFrameInfoByID(int AFrameID)
{
  for (int i= 0; i< FrameInfoList->Count; i++)
    if (Items[i]->ID == AFrameID)
      return Items[i];
  return 0;
}
//---------------------------------------------------------------------------

int TFrameManager::GetCount()
{
  if (FrameInfoList)
    return FrameInfoList->Count;

  return 0;
}
//---------------------------------------------------------------------------

TFrameInfo* __fastcall TFrameManager::GetItem(int Index)
{
  return (TFrameInfo*)FrameInfoList->Items[Index];
}
//---------------------------------------------------------------------------


void __fastcall TFrameManager::RegisterFrame(int AFrameID, String ACaption)
{
  TFrameInfo*  AInfo = new TFrameInfo(AFrameID, ACaption);
  FrameInfoList->Add(AInfo);
}
//---------------------------------------------------------------------------

void __fastcall TFrameManager::ShowFrame(int AFrameID, TWinControl* AParent)
{
  if (ActiveFrameInfo)
    if (ActiveFrameInfo->ID == AFrameID)
       return;
    else {
      ActiveFrameInfo->HideFrame();
      FActiveFrameInfo = NULL;
    }

  TFrameInfo* AFrameInfo = GetFrameInfoByID(AFrameID);
  if (AFrameInfo) {
    AFrameInfo->ShowFrame(AParent);
    FActiveFrameInfo = AFrameInfo;
  }
}
//---------------------------------------------------------------------------

bool __fastcall TFrameManager::CanCreate(int AFrameID)
{
  for(int i = 0; i < FrameInfoList->Count; i++)
    if (Items[i]->ID == AFrameID)
      return Items[i]->Frame == NULL;
  return false;
}
//---------------------------------------------------------------------------

void __fastcall TFrameManager::CreateFrame(int AFrameID, TfrmBasic* AFrame)
{
  GetFrameInfoByID(AFrameID)->CreateFrame(AFrame);
}
//---------------------------------------------------------------------------


