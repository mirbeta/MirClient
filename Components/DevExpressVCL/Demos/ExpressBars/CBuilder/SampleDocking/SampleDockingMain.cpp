//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "SampleDockingMain.h"
#include "EBarsDemoRating.h"
#include "SampleDockingListBox.h"
#include "SampleDockingRadioGroup.h"
#include "SampleDockingRichText.h"
#include "SampleDockingTreeView.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxControls"
#pragma link "dxStatusBar"
#pragma link "dxBar"
#pragma link "dxBarExtItems"
#pragma link "dxDockControl"
#pragma link "cxGraphics"
#pragma link "cxClasses"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxLookAndFeels"
#pragma link "cxPC"
#pragma resource "*.dfm"
TSampleDockingMainForm *SampleDockingMainForm;
//---------------------------------------------------------------------------
__fastcall TSampleDockingMainForm::TSampleDockingMainForm(TComponent* Owner)
  : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TSampleDockingMainForm::FormDestroy(TObject *Sender)
{
  ClearContent();
}
//---------------------------------------------------------------------------

TWinControl* __fastcall GetFocusedControl(TWinControl* AWinControl)
{
  TWinControl* Result = NULL;
  for (int i=0; i< AWinControl->ControlCount; i++)
    if (dynamic_cast<TWinControl*>(AWinControl->Controls[i])) {
      Result = GetFocusedControl(((TWinControl*)AWinControl->Controls[i]));
      if (Result != NULL) return (Result);
      if (((TWinControl*)AWinControl->Controls[i])->Focused()) {
        Result = ((TWinControl*)AWinControl->Controls[i]);
        return (Result);
      }
    }
  return (Result);  
}
//---------------------------------------------------------------------------

void __fastcall RestorePersistentInfo(TdxCustomDockControl* Sender)
{
  if (Sender->Visible && (Sender->Tag != 0)) {
    if (((PPersistInfo)Sender->Tag)->WinControl->CanFocus()) {
      ((PPersistInfo)Sender->Tag)->WinControl->SetFocus();
    }
    if (dynamic_cast<TCustomEdit*>(((PPersistInfo)Sender->Tag)->WinControl)) {
      ((TCustomEdit*)((PPersistInfo)Sender->Tag)->WinControl)->SelStart = ((PRichEditSelInfo)((PPersistInfo)Sender->Tag)->SpecInfo)->SelStart;
      ((TCustomEdit*)((PPersistInfo)Sender->Tag)->WinControl)->SelLength = ((PRichEditSelInfo)((PPersistInfo)Sender->Tag)->SpecInfo)->SelLength;
      delete (PRichEditSelInfo)((PPersistInfo)Sender->Tag)->SpecInfo;
    }
    delete (PPersistInfo)Sender->Tag;
    Sender->Tag = 0;
  }
}
//---------------------------------------------------------------------------

void __fastcall StorePersistentInfo(TdxCustomDockControl* Sender)
{
  if (Sender->Visible && (Sender->Tag == 0)) {
    TWinControl*  AWinControl = GetFocusedControl(Sender);
    if (AWinControl != NULL) {
      PPersistInfo p = new TPersistInfo;
      p->WinControl = AWinControl;
      if (dynamic_cast<TCustomEdit*>(AWinControl)) {
        PRichEditSelInfo PSelInfo = new TRichEditSelInfo;
        PSelInfo->SelStart = ((TCustomEdit*)AWinControl)->SelStart;
        PSelInfo->SelLength = ((TCustomEdit*)AWinControl)->SelLength;
        p->SpecInfo = (int)PSelInfo;
        Sender->Tag = (int)p;
      }
      Sender->Tag = (int)p;
    }
  }
}
//---------------------------------------------------------------------------

void __fastcall TSampleDockingMainForm::dxBarDockStyleStandardClick(TObject *Sender)
{
  if (dxBarDockStyleVS2005->Down)
  {
	dxDockingManager->DockStyle = dsVS2005;
	dxDockingManager->Options = dxDockingManager->Options + (TdxDockingOptions() << doFillDockingSelection);
  }
  else
  {
	dxDockingManager->Options = dxDockingManager->Options - (TdxDockingOptions() << doFillDockingSelection);
	dxDockingManager->DockStyle = dsStandard;
  };
}
//---------------------------------------------------------------------------

void __fastcall TSampleDockingMainForm::ClearContent()
{
  int Count = dxDockingController()->DockControlCount - 1;
  while (Count >= 0) {
    if (dynamic_cast<TdxDockPanel*>(dxDockingController()->DockControls[Count]))
      delete dxDockingController()->DockControls[Count];
    if ((dxDockingController()->DockControlCount - 1) < (Count - 1))
      Count = dxDockingController()->DockControlCount - 1; else
    Count--;
  };
  FTreeViewFrameCount = 0;
  FDockingRichTextFrameCount = 0;
  FRadioGroupFrameCount = 0;
  FListBoxFrameCount = 0;
}
//---------------------------------------------------------------------------

void __fastcall TSampleDockingMainForm::SetPanelsVisibility(bool AVisible)
{
  for(int i=0; i < dxDockingController()->DockControlCount; i++)
    if (dynamic_cast<TdxCustomDockControl*>(dxDockingController()->DockControls[i]) && (dxDockingController()->DockControls[i] != DockSite1)) {
      if ((!AVisible) && dxDockingController()->DockControls[i]->AutoHide)
        dxDockingController()->DockControls[i]->Visible = true;
      dxDockingController()->DockControls[i]->Visible = AVisible;
    }
}
//---------------------------------------------------------------------------

void __fastcall TSampleDockingMainForm::CreateScheme1()
{
  TdxDockPanel* Panel1 = new TdxDockPanel(this);
  HookupEvents(Panel1);
  Panel1->Height = 360;
  Panel1->DockTo(DockSite1, dtBottom, 0);
  CreateFrame(ListBoxFrame, Panel1);

  TdxDockPanel* Panel2 = new TdxDockPanel(this);
  HookupEvents(Panel2);
  Panel2->DockTo(Panel1, dtClient, 1);
  CreateFrame(RadioGroupFrame, Panel2);

  TdxDockPanel* Panel = new TdxDockPanel(this);
  HookupEvents(Panel);
  Panel->DockTo(Panel2->TabContainer, dtTop, 0);
  CreateFrame(TreeViewFrame, Panel);

  Panel1 = new TdxDockPanel(this);
  HookupEvents(Panel1);
  Panel1->DockTo(Panel, dtClient, 0);
  CreateFrame(RichTextFrame, Panel1);
}
//---------------------------------------------------------------------------

void __fastcall TSampleDockingMainForm::CreateScheme2()
{
  TdxDockPanel* Panel1 = new TdxDockPanel(this);
  HookupEvents(Panel1);
  Panel1->Height = 300;
  Panel1->DockTo(DockSite1, dtRight, 0);
  CreateFrame(TreeViewFrame, Panel1);

  TdxDockPanel* Panel2 = new TdxDockPanel(this);
  HookupEvents(Panel2);
  Panel2->DockTo(Panel1, dtClient, 1);
  CreateFrame(RichTextFrame, Panel2);

  TdxDockPanel* Panel = new TdxDockPanel(this);
  HookupEvents(Panel);
  Panel->DockTo(Panel1, dtLeft, 0);
  CreateFrame(RadioGroupFrame, Panel);
}
//---------------------------------------------------------------------------

void __fastcall TSampleDockingMainForm::CreateScheme3()
{
  TdxDockPanel* Panel1 = new TdxDockPanel(this);
  HookupEvents(Panel1);
  Panel1->DockTo(DockSite1, dtLeft, 0);
  Panel1->AutoHide = true;
  CreateFrame(RichTextFrame, Panel1);

  TdxDockPanel* Panel2 = new TdxDockPanel(this);
  HookupEvents(Panel2);
  Panel2->Height = 170;
  Panel2->DockTo(DockSite1, dtTop, 0);
  Panel2->AutoHide = true;
  CreateFrame(RadioGroupFrame, Panel2);

  TdxDockPanel* Panel3 = new TdxDockPanel(this);
  HookupEvents(Panel3);
  Panel3->DockTo(DockSite1, dtRight, 0);
  Panel3->AutoHide = true;
  CreateFrame(TreeViewFrame, Panel3);

  TdxDockPanel* Panel4 = new TdxDockPanel(this);
  HookupEvents(Panel4);
  Panel4->Height = 190;
  Panel4->DockTo(DockSite1, dtBottom, 0);
  Panel4->AutoHide = true;
  CreateFrame(ListBoxFrame, Panel4);
}
//---------------------------------------------------------------------------

void __fastcall TSampleDockingMainForm::CreateScheme4()
{
  TdxDockPanel* Panel1 = new TdxDockPanel(this);
  HookupEvents(Panel1);
  Panel1->MakeFloating(Left + 50, Top + 100);
  CreateFrame(TreeViewFrame, Panel1);

  TdxDockPanel* Panel2 = new TdxDockPanel(this);
  HookupEvents(Panel2);
  Panel2->DockTo(Panel1, dtClient, 0);
  CreateFrame(RichTextFrame, Panel2);
}
//---------------------------------------------------------------------------

void __fastcall TSampleDockingMainForm::CreateScheme5()
{
  TdxDockPanel* Panel1 = new TdxDockPanel(this);
  HookupEvents(Panel1);
  Panel1->Height = 350;
  Panel1->DockTo(DockSite1, dtBottom, 0);
  CreateFrame(ListBoxFrame, Panel1);

  TdxDockPanel* Panel2 = new TdxDockPanel(this);
  HookupEvents(Panel2);
  Panel2->DockTo(Panel1, dtRight, 1);
  CreateFrame(TreeViewFrame, Panel2);

  TdxDockPanel* Panel3 = new TdxDockPanel(this);
  HookupEvents(Panel3);
  Panel3->DockTo(Panel1, dtTop, 0);
  CreateFrame(RichTextFrame, Panel3);
}
//---------------------------------------------------------------------------

void __fastcall TSampleDockingMainForm::HookupEvents(TdxCustomDockControl* Sender)
{
  if (dynamic_cast<TdxDockSite*>(Sender)) {
    ((TdxDockSite*)Sender)->OnShowControl = ShowDockControl;
    ((TdxDockSite*)Sender)->OnHideControl = HideDockControl;
  }
  Sender->OnAutoHideChanged = AutoHideChanged;
  Sender->OnAutoHideChanging = AutoHideChanging;
  #if __BORLANDC__ == 0x0560
  Sender->OnContextPopup = dpContextPopup;
  #endif
  Sender->OnEndDocking = EndDock;
  Sender->OnStartDocking = StartDock;
  Sender->OnCreateTabContainer = CreateTabContainer;
  Sender->OnCreateSideContainer = CreateSiteContainer;
  Sender->OnCreateFloatSite = CreateFloatSite;
}
//---------------------------------------------------------------------------


void __fastcall TSampleDockingMainForm::CreateFrame(int AFrameClassID, TdxDockPanel* AOwner)
{
  TForm* AFrame;
  String ACaption;
  int AImageIndex;

  AImageIndex = -1;
  if (AFrameClassID == TreeViewFrame) {
    AFrame = new TSampleDockingTreeViewFrame(AOwner);
    FTreeViewFrameCount++;
    ACaption = "TreeView Panel " + IntToStr(FTreeViewFrameCount);
    AImageIndex = 0;
  } else
  if (AFrameClassID == RichTextFrame) {
    AFrame = new TSampleDockingRichTextFrame(AOwner);
    FDockingRichTextFrameCount++;
    ACaption = "RichText Panel " + IntToStr(FDockingRichTextFrameCount);
    AImageIndex = 1;
  } else
  if (AFrameClassID == RadioGroupFrame) {
    AFrame = new TSampleDockingRadioGroupFrame(AOwner);
    FRadioGroupFrameCount++;
    ACaption = "RadioGroup Panel " + IntToStr(FRadioGroupFrameCount);
    AImageIndex = 2;
  } else
  if (AFrameClassID == ListBoxFrame){
    AFrame = new TSampleDockingListBoxFrame(AOwner);
    FListBoxFrameCount++;
    ACaption = "ListBox Panel " + IntToStr(FListBoxFrameCount);
    AImageIndex = 3;
  }
  AFrame->Parent = AOwner;
  AFrame->Align = alClient;
  AOwner->Caption = ACaption;
  AOwner->ImageIndex = AImageIndex;
  AFrame->Visible = true;
}
//---------------------------------------------------------------------------

void __fastcall TSampleDockingMainForm::ShowDockControl(
      TdxDockSite *Sender, TdxCustomDockControl *AControl)
{
  RestorePersistentInfo(AControl);
}
//---------------------------------------------------------------------------

void __fastcall TSampleDockingMainForm::HideDockControl(
      TdxDockSite *Sender, TdxCustomDockControl *AControl)
{
  StorePersistentInfo(AControl);
}
//---------------------------------------------------------------------------

void __fastcall TSampleDockingMainForm::AutoHideChanged(
      TdxCustomDockControl *Sender)
{
  RestorePersistentInfo(Sender);
}
//---------------------------------------------------------------------------

void __fastcall TSampleDockingMainForm::AutoHideChanging(
      TdxCustomDockControl *Sender)
{
  StorePersistentInfo(Sender);
}
//---------------------------------------------------------------------------

void __fastcall TSampleDockingMainForm::dpContextPopup(TObject *Sender,
      const TPoint &MousePos, bool &Handled)
{
  TPoint pt;
  GetCursorPos(&pt);
  TdxCustomDockControl* AControl = dxDockingController()->GetDockControlAtPos(pt);
  if (AControl != NULL) {
    FPopupMenuDockControl = AControl;
    dxBarButtonDockable->Down = AControl->Dockable;
    dxBarButtonFloating->Down = AControl->FloatDockSite == NULL;
    dxBarButtonAutoHide->Enabled = AControl->CanAutoHide();
    dxBarButtonAutoHide->Down = AControl->AutoHide;
    dxBarPopupMenu->PopupFromCursorPos();
    Handled = true;
  }
}
//---------------------------------------------------------------------------
void __fastcall TSampleDockingMainForm::EndDock(
      TdxCustomDockControl *Sender, TdxZone *Zone, int X, int Y)
{
  RestorePersistentInfo(Sender);
}
//---------------------------------------------------------------------------

void __fastcall TSampleDockingMainForm::StartDock(
      TdxCustomDockControl *Sender, int X, int Y)
{
  StorePersistentInfo(Sender);
}
//---------------------------------------------------------------------------

void __fastcall TSampleDockingMainForm::CreateTabContainer(
      TdxCustomDockControl *Sender, TdxTabContainerDockSite *ATabContainer)
{
  HookupEvents(ATabContainer);
}
//---------------------------------------------------------------------------

void __fastcall TSampleDockingMainForm::CreateSiteContainer(
      TdxCustomDockControl *Sender,
      TdxSideContainerDockSite *ASideContainer)
{
  HookupEvents(ASideContainer);
}
//---------------------------------------------------------------------------

void __fastcall TSampleDockingMainForm::CreateFloatSite(
      TdxCustomDockControl *Sender, TdxFloatDockSite *AFloatSite)
{
  HookupEvents(AFloatSite);
}
//---------------------------------------------------------------------------

void __fastcall TSampleDockingMainForm::SchemeClick(TObject *Sender)
{
  switch (((TComponent*)Sender)->Tag) {
    case 0: CreateScheme1(); break;
    case 1: CreateScheme2(); break;
    case 2: CreateScheme3(); break;
    case 3: CreateScheme4(); break;
    case 4: CreateScheme5(); break;
  }
}
//---------------------------------------------------------------------------

void __fastcall TSampleDockingMainForm::dxBarButton18Click(TObject *Sender)
{
  SetPanelsVisibility(true);
}
//---------------------------------------------------------------------------

void __fastcall TSampleDockingMainForm::dxBarButton19Click(TObject *Sender)
{
  SetPanelsVisibility(false);
}
//---------------------------------------------------------------------------

void __fastcall TSampleDockingMainForm::dxBarButton12Click(TObject *Sender)
{
  ClearContent();
}
//---------------------------------------------------------------------------

void __fastcall TSampleDockingMainForm::dxBarButtonDockableClick(
      TObject *Sender)
{
  if (FPopupMenuDockControl != NULL) {
    FPopupMenuDockControl->Dockable = ((TdxBarButton*)Sender)->Down;
    FPopupMenuDockControl = NULL;
  }
}
//---------------------------------------------------------------------------

void __fastcall TSampleDockingMainForm::dxBarButton22Click(TObject *Sender)
{
  if (FPopupMenuDockControl != NULL) {
    FPopupMenuDockControl->Visible = false;
    FPopupMenuDockControl = NULL;
  }
}
//---------------------------------------------------------------------------

void __fastcall TSampleDockingMainForm::dxBarButtonAutoHideClick(
      TObject *Sender)
{
  if (FPopupMenuDockControl != NULL) {
    FPopupMenuDockControl->AutoHide = ((TdxBarButton*)Sender)->Down;
    FPopupMenuDockControl = NULL;
  }
}
//---------------------------------------------------------------------------

void __fastcall TSampleDockingMainForm::dxBarButtonFloatingClick(
      TObject *Sender)
{
  TPoint pt;
  if ((FPopupMenuDockControl != NULL) && (FPopupMenuDockControl->DockState != dsFloating)) {
    GetCursorPos(&pt);
    FPopupMenuDockControl->MakeFloating(pt.x, pt.y);
    FPopupMenuDockControl = NULL;
  }
}
//---------------------------------------------------------------------------

void __fastcall TSampleDockingMainForm::dxbLookAndFeelKindsClick(TObject *Sender)
{
  cxLookAndFeelController->Kind = (TcxLookAndFeelKind)((TdxBarListItem*)Sender)->ItemIndex;
  cxLookAndFeelController->NativeStyle = False;
  UpdateLookAndFeelMenu();
}
//---------------------------------------------------------------------------

void __fastcall TSampleDockingMainForm::dxbNativeStyleClick(TObject *Sender)
{
  cxLookAndFeelController->NativeStyle = True;
  UpdateLookAndFeelMenu();
}
//---------------------------------------------------------------------------

void __fastcall TSampleDockingMainForm::FormCreate(TObject *Sender)
{
  UpdateLookAndFeelMenu();
}
//---------------------------------------------------------------------------

void __fastcall TSampleDockingMainForm::UpdateLookAndFeelMenu()
{
  dxbNativeStyle->Down = cxLookAndFeelController->NativeStyle;
  if (cxLookAndFeelController->NativeStyle)
    dxbLookAndFeelKinds->ItemIndex = -1;
  else
    dxbLookAndFeelKinds->ItemIndex = (Integer)cxLookAndFeelController->Kind;
}
//---------------------------------------------------------------------------

void __fastcall TSampleDockingMainForm::dxBarButtonExitClick(
      TObject *Sender)
{
  Close();  
}
//---------------------------------------------------------------------------
