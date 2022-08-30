//---------------------------------------------------------------------------
#include <vcl.h>
#include "shellapi.hpp"
#pragma hdrstop

#include "FeaturesMain.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "dxNavBar"
#pragma link "dxNavBarBase"
#pragma link "dxNavBarCollns"
#pragma link "dxNavBarStyles"
#pragma resource "*.dfm"
TfmFeaturesMain *fmFeaturesMain;

void ClearPopupMenuItems(TMenuItem *AMenuItem)
{
  for (int I = AMenuItem->Count - 1; I >= 0; I--)
    delete AMenuItem->Items[I];
}

//---------------------------------------------------------------------------

__fastcall TfmFeaturesMain::TfmFeaturesMain(TComponent* Owner)
        : TForm(Owner)
{
}
//---------------------------------------------------------------------------

void TfmFeaturesMain::InitImageIndexComoboBoxes()
{
    for (int I = 0; I < imgSmall->Count; I++){
      cbGSmallImageIndex->Items->Add(IntToStr(I));
      cbISmallImageIndex->Items->Add(IntToStr(I));
    }
    for (int I = 0; I < imgLarge->Count; I++){
      cbGLargeImageIndex->Items->Add(IntToStr(I));
      cbILargeImageIndex->Items->Add(IntToStr(I));
    }
}

void __fastcall TfmFeaturesMain::FormCreate(TObject *Sender)
{
  ClearPopupMenuItems(miLookAndFeel);
  for (int I = 0; I < dxNavBarViewsFactory()->Count; I++)
    AddDropDownMenuItem(miLookAndFeel, dxNavBarViewsFactory()->Names[I], -1, true,
      miLookAndFeelItemClick);
  miLookAndFeel->Items[dxNavBarViewsFactory()->IndexOfID(nbMain->View)]->Checked = true;
  gbProperties->Height = 190;
  actShowCaptions->Checked = nbMain->ShowGroupCaptions;
  actAllowSelectLinks->Checked = nbMain->AllowSelectLinks;
  actEachGroupHasSelectedLink->Checked = nbMain->EachGroupHasSelectedLink;
  actShowGroupHints->Checked = nbMain->ShowGroupsHint;
  actShowLinkHints->Checked = nbMain->ShowLinksHint;

  InitImageIndexComoboBoxes();
  UpdateTreeView();
  SelectDefaultGroup();
  UpdateGroupProperties();
  UpdateItemProperties();
}
//---------------------------------------------------------------------------

void __fastcall TfmFeaturesMain::btAddGroupClick(TObject *Sender)
{
  TdxNavBarGroup *AGroup = nbMain->Groups->Add();
  TTreeNode *ANode, *AParentNode;
  AGroup->OnClick = NavBarItemClick;
  if (CurrentGroup != NULL)
  {
    if (((TObject*)tvNavBar->Selected->Data)->ClassType() == __classid(TdxNavBarGroup))
      AParentNode = tvNavBar->Selected;
    else AParentNode = tvNavBar->Selected->Parent;
    ANode = tvNavBar->Items->InsertObject(AParentNode, AGroup->Caption, AGroup);
    AGroup->Index = CurrentGroup->Index;
  }
  else ANode = tvNavBar->Items->AddObject(NULL, AGroup->Caption, AGroup);
  ANode->ImageIndex = AGroup->SmallImageIndex;
  ANode->SelectedIndex = ANode->ImageIndex;
  tvNavBar->Selected = ANode;
  tvNavBar->FullExpand();
  UpdateGroupProperties();
  UpdateItemProperties();
}
//---------------------------------------------------------------------------

void __fastcall TfmFeaturesMain::btDeleteGroupClick(TObject *Sender)
{
  if (CurrentGroup != NULL)
  {
    TdxNavBarGroup *AGroup = CurrentGroup;
    if (((TObject*)tvNavBar->Selected->Data)->ClassType() == __classid(TdxNavBarGroup))
      tvNavBar->Items->Delete(tvNavBar->Selected);
    else tvNavBar->Items->Delete(tvNavBar->Selected->Parent);
    nbMain->Groups->Delete(AGroup->Index);

    SelectDefaultGroup();
    UpdateGroupProperties();
    UpdateItemProperties();
  }
}
//---------------------------------------------------------------------------

void __fastcall TfmFeaturesMain::btAddLinkClick(TObject *Sender)
{
  if (CurrentGroup != NULL)
  {
    TPoint APoint = ((TButton*)Sender)->ClientToScreen(Point(0, ((TButton*)Sender)->Height));
    pmnuItems->Popup(APoint.x, APoint.y);
  }
}
//---------------------------------------------------------------------------

void __fastcall TfmFeaturesMain::btDeleteLinkClick(TObject *Sender)
{
  if (CurrentLink != NULL)
  {
    TdxNavBarItemLink *ALink = CurrentLink;
    if (((TObject*)tvNavBar->Selected->Data)->ClassType() == __classid(TdxNavBarItemLink))
      tvNavBar->Items->Delete(tvNavBar->Selected);
    ALink->Group->RemoveLink(ALink->Index);

    SelectDefaultGroup();
    UpdateGroupProperties();
    UpdateItemProperties();
  }
}
//---------------------------------------------------------------------------

void __fastcall TfmFeaturesMain::FormActivate(TObject *Sender)
{
  UpdateGroupProperties();
  UpdateItemProperties();
}
//---------------------------------------------------------------------------

void __fastcall TfmFeaturesMain::nbMainEndDrag(TObject *Sender, TObject *Target,
      int X, int Y)
{
  UpdateTreeView();
}
//---------------------------------------------------------------------------

void __fastcall TfmFeaturesMain::miLookAndFeelItemClick(TObject *Sender)
{
  ((TMenuItem*)Sender)->Checked = true;
  nbMain->View = dxNavBarViewsFactory()->IDs[((TMenuItem*)Sender)->MenuIndex];
}
//---------------------------------------------------------------------------

void __fastcall TfmFeaturesMain::actShowCaptionsExecute(TObject *Sender)
{
  ((TAction*)Sender)->Checked = !((TAction*)Sender)->Checked;
  nbMain->ShowGroupCaptions = ((TAction*)Sender)->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TfmFeaturesMain::actShowSpecialGroupExecute(TObject *Sender)
{
  ((TAction*)Sender)->Checked = !((TAction*)Sender)->Checked;
  nbMain->ShowSpecialGroup = ((TAction*)Sender)->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TfmFeaturesMain::actAllowSelectLinksExecute(TObject *Sender)
{
  ((TAction*)Sender)->Checked = !((TAction*)Sender)->Checked;
  nbMain->AllowSelectLinks = ((TAction*)Sender)->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TfmFeaturesMain::actEachGroupHasSelectedLinkExecute(TObject *Sender)
{
  ((TAction*)Sender)->Checked = !((TAction*)Sender)->Checked;
  nbMain->EachGroupHasSelectedLink = ((TAction*)Sender)->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TfmFeaturesMain::actShowGroupHintsExecute(TObject *Sender)
{
  ((TAction*)Sender)->Checked = !((TAction*)Sender)->Checked;
  nbMain->ShowGroupsHint = ((TAction*)Sender)->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TfmFeaturesMain::actShowLinkHintsExecute(TObject *Sender)
{
  ((TAction*)Sender)->Checked = !((TAction*)Sender)->Checked;
  nbMain->ShowLinksHint = ((TAction*)Sender)->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TfmFeaturesMain::pmnuItemClick(TObject *Sender)
{
  if (CurrentGroup != NULL){
    TdxNavBarItem *AItem = nbMain->Items->Items[((TMenuItem*)Sender)->MenuIndex];
    TdxNavBarItemLink *ALink = CurrentGroup->CreateLink(AItem);
    TTreeNode *ANode;
    if (dynamic_cast<TdxNavBarGroup *>((TObject*)tvNavBar->Selected->Data) != 0)
      ANode = tvNavBar->Selected;
    else
      ANode = tvNavBar->Selected->Parent;
    ANode = tvNavBar->Items->AddChildObject(ANode, AItem->Caption, ALink);
    SetNodeImageIndex(ANode, AItem->SmallImageIndex);
    ANode->Selected = true;
    tvNavBar->FullExpand();
  }
}
//---------------------------------------------------------------------------

void __fastcall TfmFeaturesMain::pmnuItemsPopup(TObject *Sender)
{
  UpdateItemsDropDownMenu();
}
//---------------------------------------------------------------------------


void TfmFeaturesMain::ShowPropertiesTabSheet(TTabSheet *ATabSheet)
{
  ATabSheet->Parent = gbProperties;
  ATabSheet->Visible = true;
  gbProperties->Caption = ATabSheet->Caption;
}

void __fastcall TfmFeaturesMain::tvNavBarChange(TObject *Sender, TTreeNode *Node)
{
 UpdateGroupProperties();
 UpdateItemProperties();
 if (Node->Level == 0){
   HidePropertiesTabSheet(tsSelectedItemProps);
   ShowPropertiesTabSheet(tsSelectedGroupProps);
 }
 else{
   HidePropertiesTabSheet(tsSelectedGroupProps);
   ShowPropertiesTabSheet(tsSelectedItemProps);
 }
}
//---------------------------------------------------------------------------

void __fastcall TfmFeaturesMain::cbGExpandedClick(TObject *Sender)
{
  if (CurrentGroup != NULL)
    CurrentGroup->Expanded = ((TCheckBox*)Sender)->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TfmFeaturesMain::cbGVisibleClick(TObject *Sender)
{
  if (CurrentGroup != NULL)
    CurrentGroup->Visible = ((TCheckBox*)Sender)->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TfmFeaturesMain::cbGShowAsIconViewClick(TObject *Sender)
{
  if (CurrentGroup != NULL)
    CurrentGroup->ShowAsIconView = ((TCheckBox*)Sender)->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TfmFeaturesMain::cbGLinkUseSmallImagesClick(TObject *Sender)
{
  if (CurrentGroup != NULL)
    CurrentGroup->LinksUseSmallImages = ((TCheckBox*)Sender)->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TfmFeaturesMain::cbGUseSmallImagesClick(TObject *Sender)
{
  if (CurrentGroup != NULL)
    CurrentGroup->UseSmallImages = ((TCheckBox*)Sender)->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TfmFeaturesMain::eGCaptionChange(TObject *Sender)
{
  if (CurrentGroup != NULL){
    CurrentGroup->Caption = eGCaption->Text;
    tvNavBar->Selected->Text = CurrentGroup->Caption;
  }
}
//---------------------------------------------------------------------------

void __fastcall TfmFeaturesMain::cbGSmallImageIndexChange(TObject *Sender)
{
  if (CurrentGroup == NULL)
    return;
  CurrentGroup->SmallImageIndex = cbGSmallImageIndex->ItemIndex;
  tvNavBar->Selected->ImageIndex = CurrentGroup->SmallImageIndex;
  tvNavBar->Selected->StateIndex = CurrentGroup->SmallImageIndex;
  tvNavBar->Selected->SelectedIndex = CurrentGroup->SmallImageIndex;
}
//---------------------------------------------------------------------------

void __fastcall TfmFeaturesMain::cbGLargeImageIndexChange(TObject *Sender)
{
  if (CurrentGroup != NULL)
    CurrentGroup->LargeImageIndex = cbGLargeImageIndex->ItemIndex;
}
//---------------------------------------------------------------------------

void __fastcall TfmFeaturesMain::cbIEnabledClick(TObject *Sender)
{
  if (CurrentItem != NULL)
    CurrentItem->Enabled = ((TCheckBox*)Sender)->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TfmFeaturesMain::cbIVisibleClick(TObject *Sender)
{
  if (CurrentItem != NULL)
    CurrentItem->Visible = ((TCheckBox*)Sender)->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TfmFeaturesMain::eICaptionChange(TObject *Sender)
{
  if (CurrentItem != NULL){
    CurrentItem->Caption = eICaption->Text;
    for (int I = 0; I < tvNavBar->Items->Count; I++)
      if ((dynamic_cast<TdxNavBarItemLink *>((TObject*)tvNavBar->Items->Item[I]->Data) != 0) &&
        (((TdxNavBarItemLink*)tvNavBar->Items->Item[I]->Data)->Item == CurrentItem)){
          tvNavBar->Items->Item[I]->Text = CurrentItem->Caption;
          break;
      }
  }    
}
//---------------------------------------------------------------------------

void __fastcall TfmFeaturesMain::cbISmallImageIndexChange(TObject *Sender)
{
  if (CurrentItem != NULL){
    CurrentItem->SmallImageIndex = cbISmallImageIndex->ItemIndex;
    tvNavBar->Selected->ImageIndex = CurrentItem->SmallImageIndex;
    tvNavBar->Selected->StateIndex = CurrentItem->SmallImageIndex;
    tvNavBar->Selected->SelectedIndex = CurrentItem->SmallImageIndex;
  }
}
//---------------------------------------------------------------------------

void __fastcall TfmFeaturesMain::cbILargeImageIndexChange(TObject *Sender)
{
  if (CurrentItem != NULL)
    CurrentItem->LargeImageIndex = cbILargeImageIndex->ItemIndex;
}
//---------------------------------------------------------------------------

void __fastcall TfmFeaturesMain::FormDestroy(TObject *Sender)
{
  tsSelectedItemProps->Parent = pcProperties;
  tsSelectedGroupProps->Parent = pcProperties;
}
//---------------------------------------------------------------------------

void __fastcall TfmFeaturesMain::NavBarItemClick(TObject *Sender)
{
  for (int I = 0; I < tvNavBar->Items->Count; I++)
    if (tvNavBar->Items->Item[I]->Data == Sender){
      tvNavBar->Items->Item[I]->Selected = true;
      break;
    }
    else
      if ( (dynamic_cast<TdxNavBarItemLink *>((TObject*)tvNavBar->Items->Item[I]->Data) != 0) &&
        (((TdxNavBarItemLink*)tvNavBar->Items->Item[I]->Data)->Item == Sender) &&
        (tvNavBar->Items->Item[I]->Data == nbMain->PressedLink)){
          tvNavBar->Items->Item[I]->Selected = true;
          break;
      }
}
//---------------------------------------------------------------------------

void TfmFeaturesMain::SelectDefaultGroup()
{
  if (tvNavBar->Items->Count > 0)
    tvNavBar->Selected = tvNavBar->Items->Item[0];
}
//---------------------------------------------------------------------------

void TfmFeaturesMain::UpdateGroupProperties()
{
  if (CurrentGroup != NULL)
  {
    eGCaption->Text = CurrentGroup->Caption;
    cbGExpanded->Checked = CurrentGroup->Expanded;
    cbGVisible->Checked = CurrentGroup->Visible;
    cbGLinkUseSmallImages->Checked = CurrentGroup->LinksUseSmallImages;
    cbGUseSmallImages->Checked = CurrentGroup->UseSmallImages;
    cbGShowAsIconView->Checked = CurrentGroup->ShowAsIconView;
    cbGSmallImageIndex->ItemIndex = CurrentGroup->SmallImageIndex;
    cbGLargeImageIndex->ItemIndex = CurrentGroup->LargeImageIndex;
  }
  UpdateGroupPropertiesState();
}
//---------------------------------------------------------------------------

void TfmFeaturesMain::UpdateItemProperties()
{
  if (CurrentItem != NULL)
  {
    eICaption->Text = CurrentItem->Caption;
    cbIEnabled->Checked = CurrentItem->Enabled;
    cbIVisible->Checked = CurrentItem->Visible;
    cbISmallImageIndex->ItemIndex = CurrentItem->SmallImageIndex;
    cbILargeImageIndex->ItemIndex = CurrentItem->LargeImageIndex;
  }
  UpdateItemPropertiesState();
}
//---------------------------------------------------------------------------

void TfmFeaturesMain::UpdateGroupPropertiesState()
{
  btDeleteGroup->Enabled = CurrentGroup != NULL;
  eGCaption->Enabled = CurrentGroup != NULL;
  if (!eGCaption->Enabled)
    eGCaption->Text = "";
  cbGExpanded->Enabled = CurrentGroup != NULL;
  if (!cbGExpanded->Enabled)
    cbGExpanded->Checked = False;
  cbGVisible->Enabled = CurrentGroup != NULL;
  if (!cbGVisible->Enabled)
    cbGVisible->Checked = False;
  cbGLinkUseSmallImages->Enabled = CurrentGroup != NULL;
  if (!cbGLinkUseSmallImages->Enabled)
    cbGLinkUseSmallImages->Checked = False;
  cbGUseSmallImages->Enabled = CurrentGroup != NULL;
  if (!cbGUseSmallImages->Enabled)
    cbGUseSmallImages->Checked = False;
  cbGShowAsIconView->Enabled = CurrentGroup != NULL;
  if (!cbGShowAsIconView->Enabled)
    cbGShowAsIconView->Checked = False;
}
//---------------------------------------------------------------------------

void TfmFeaturesMain::UpdateItemPropertiesState()
{
  btAddLink->Enabled = CurrentGroup != NULL;
  btDeleteLink->Enabled = CurrentLink != NULL;
  eICaption->Enabled = CurrentLink != NULL;
  if (!eICaption->Enabled)
    eICaption->Text = "";
  cbIEnabled->Enabled = CurrentLink != NULL;
  if (!cbIEnabled->Enabled)
    cbIEnabled->Checked = False;
  cbIVisible->Enabled = CurrentLink != NULL;
  if (!cbIVisible->Enabled)
    cbIVisible->Checked = False;
}
//---------------------------------------------------------------------------

void TfmFeaturesMain::UpdateTreeView()
{
  tvNavBar->Items->BeginUpdate();
  try
  {
    tvNavBar->Items->Clear();
    tvNavBar->Images = nbMain->SmallImages;
    for(int i = 0; i < nbMain->Groups->Count; i ++)
    {
      TTreeNode* ANode = tvNavBar->Items->AddObject(NULL, nbMain->Groups->Items[i]->Caption,
        nbMain->Groups->Items[i]);
      SetNodeImageIndex(ANode, nbMain->Groups->Items[i]->SmallImageIndex);
      for(int j = 0; j < nbMain->Groups->Items[i]->LinkCount; j ++)
        if (nbMain->Groups->Items[i]->Links[j]->Item != NULL)
        {
          TTreeNode* AChildNode = tvNavBar->Items->AddChildObject(ANode,
            nbMain->Groups->Items[i]->Links[j]->Item->Caption,
            nbMain->Groups->Items[i]->Links[j]);
          SetNodeImageIndex(AChildNode,
            nbMain->Groups->Items[i]->Links[j]->Item->SmallImageIndex);
        }
    }
    tvNavBar->FullExpand();
  }
  __finally
  {
    tvNavBar->Items->EndUpdate();
  }
}
//---------------------------------------------------------------------------

TdxNavBarGroup* TfmFeaturesMain::GetCurrentGroup()
{
  if (tvNavBar->Selected != NULL)
  {
    if (((TObject*)tvNavBar->Selected->Data)->ClassType() == __classid(TdxNavBarGroup))
      return (TdxNavBarGroup*)tvNavBar->Selected->Data;
    else return (TdxNavBarGroup*)tvNavBar->Selected->Parent->Data;
  }
  else return NULL;
}
//---------------------------------------------------------------------------

TdxNavBarItem* TfmFeaturesMain::GetCurrentItem()
{
  if (CurrentLink != NULL)
    return CurrentLink->Item;
  else return NULL;
}
//---------------------------------------------------------------------------

TdxNavBarItemLink* TfmFeaturesMain::GetCurrentLink()
{
  if (tvNavBar->Selected != NULL)
  {
    if (((TObject*)tvNavBar->Selected->Data)->ClassType() == __classid(TdxNavBarGroup))
      return NULL;
    else return (TdxNavBarItemLink*)tvNavBar->Selected->Data;
  }
  else return NULL;
}
//---------------------------------------------------------------------------

void TfmFeaturesMain::AddDropDownMenuItem(TMenuItem *AMenuItems, String ACaption,
  int AImageIndex, bool ARadioItem, TNotifyEvent AClickHandler)
{
  TMenuItem *AMenuItem = new TMenuItem(this);
  AMenuItem->Caption = ACaption;
  AMenuItem->ImageIndex = AImageIndex;
  AMenuItem->RadioItem = true;
  if (AClickHandler != NULL)
    AMenuItem->OnClick = AClickHandler;
  else
    AMenuItem->Enabled = false;
  AMenuItems->Add(AMenuItem);
}
//---------------------------------------------------------------------------

void TfmFeaturesMain::HidePropertiesTabSheet(TTabSheet *ATabSheet)
{
  ATabSheet->Visible = false;
  ATabSheet->Parent = pcProperties;
}
//---------------------------------------------------------------------------

void TfmFeaturesMain::SetNodeImageIndex(TTreeNode *ANode, int AImageIndex)
{
  ANode->ImageIndex = AImageIndex;
  ANode->StateIndex = AImageIndex;
  ANode->SelectedIndex = AImageIndex;
}
//---------------------------------------------------------------------------

void TfmFeaturesMain::UpdateItemsDropDownMenu()
{
  ClearPopupMenuItems(pmnuItems->Items);
  pmnuItems->Images = nbMain->SmallImages;
  for (int I = 0; I < nbMain->Items->Count; I++) 
    AddDropDownMenuItem(pmnuItems->Items, nbMain->Items->Items[I]->Caption,
      nbMain->Items->Items[I]->SmallImageIndex, false, pmnuItemClick);
}
//---------------------------------------------------------------------------


void __fastcall TfmFeaturesMain::nbMainActiveGroupChanged(TObject *Sender)
{
  NavBarItemClick(nbMain->ActiveGroup);
}
//---------------------------------------------------------------------------


void __fastcall TfmFeaturesMain::nbMainLinkClick(TObject *Sender,
      TdxNavBarItemLink *ALink)
{
  NavBarItemClick(ALink);
}
//---------------------------------------------------------------------------

