//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "DockingMegaDemoMain.h"
#include "EBarsDemoRating.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "dxBar"
#pragma link "dxDockControl"
#pragma link "dxDockPanel"
#pragma link "dxBarExtItems"
#pragma link "cxPC" 
#pragma link "cxLookAndFeels" 
#pragma link "cxClasses"
#pragma link "cxControls"
#pragma link "cxGraphics"
#pragma link "cxLookAndFeelPainters"
#pragma link "dxNavBarBase"
#pragma link "dxNavBarCollns"
#pragma resource "*.dfm"
TDockingMegaDemoMainForm *DockingMegaDemoMainForm;
//---------------------------------------------------------------------------
__fastcall TDockingMegaDemoMainForm::TDockingMegaDemoMainForm(TComponent* Owner)
  : TForm(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TDockingMegaDemoMainForm::FormShow(TObject *Sender)
{
  DockControl = dsHost;
  UpdateSolutionTreeView();
  UpdateClassViewTreeView();
  ComboBox1->Align = alClient;
}
//---------------------------------------------------------------------------

void __fastcall TDockingMegaDemoMainForm::CheckSite(TdxCustomDockControl* AControl, TTreeNode* ANode)
{
  String ANodeName;
  if (AControl->Name != "")
    ANodeName = AControl->Name;
  else ANodeName = AControl->ClassName();
  TTreeNode* AChild = tvSolutionExplorer->Items->AddChildObject(ANode, ANodeName, AControl);
  AChild->StateIndex = AControl->ImageIndex;
  for (int i=0; i < AControl->ChildCount; i++)
    CheckSite(AControl->Children[i], AChild);
}
//---------------------------------------------------------------------------

void __fastcall TDockingMegaDemoMainForm::UpdateSolutionTreeView()
{
  if (!tvSolutionExplorer->HandleAllocated()) return;
  tvSolutionExplorer->Items->BeginUpdate();
  try {
    tvSolutionExplorer->Items->Clear();
    CheckSite(dsHost, NULL);
    tvSolutionExplorer->FullExpand();
  }
  __finally {
    tvSolutionExplorer->Items->EndUpdate();
  }
  tvSolutionExplorer->Selected = NULL;
  for (int i=0; i < tvSolutionExplorer->Items->Count; i++)
    if (((TdxCustomDockControl*)tvSolutionExplorer->Items->Item[i]->Data) == DockControl)
      tvSolutionExplorer->Selected = tvSolutionExplorer->Items->Item[i];
  UpdateProperties();
}
//---------------------------------------------------------------------------

void __fastcall TDockingMegaDemoMainForm::dxBarDockStyleVS2005Click(TObject *Sender)
{
  if (dxBarDockStyleVS2005->Down)
  {
	dxDockingManager1->DockStyle = dsVS2005;
	dxDockingManager1->Options = dxDockingManager1->Options + (TdxDockingOptions() << doFillDockingSelection);
  }
  else
  {
    dxDockingManager1->Options = dxDockingManager1->Options - (TdxDockingOptions() << doFillDockingSelection);
    dxDockingManager1->DockStyle = dsStandard;
  };
}
//---------------------------------------------------------------------------

void __fastcall TDockingMegaDemoMainForm::UpdateClassViewTreeView()
{
  if (!tvClassView->HandleAllocated()) return;
  TTreeNode* ANode;
  tvClassView->Items->BeginUpdate();
  try {
    tvClassView->Items->Clear();
    ANode = tvClassView->Items->AddChild(NULL, "TdxCustomDockControl");
    ANode = tvClassView->Items->AddChild(ANode, "TdxCustomDockSite");
    tvClassView->Items->AddChild(ANode, "TdxLayoutDockSite");
    ANode = tvClassView->Items->AddChild(ANode, "TdxContainerDockSite");
    tvClassView->Items->AddChild(ANode, "TdxTabContainerDockSite");
    ANode = tvClassView->Items->AddChild(ANode, "TdxSideContainerDockSite");
    tvClassView->Items->AddChild(ANode, "TdxHorizContainerDockSite");
    tvClassView->Items->AddChild(ANode, "TdxVertContainerDockSite");
    tvClassView->Items->AddChild(tvClassView->Items->Item[1], "TdxFloatDockSite");
    tvClassView->Items->AddChild(tvClassView->Items->Item[1], "TdxDockSite");
    tvClassView->Items->AddChild(tvClassView->Items->Item[0], "TdxDockPanel");
    tvClassView->FullExpand();
  }
  __finally {
    tvClassView->Items->EndUpdate();
  }
}
//---------------------------------------------------------------------------

void __fastcall TDockingMegaDemoMainForm::MakeVisible(TdxDockPanel* AControl)
{
  if (AControl == NULL) return;
  if (!AControl->Visible)
    AControl->Visible = true;
  dxDockingController()->ActiveDockControl = AControl;
}
//---------------------------------------------------------------------------

void __fastcall TDockingMegaDemoMainForm::dxBarButtonPageClick(TObject *Sender)
{
  switch (((TComponent*)Sender)->Tag) {
    case 0: MakeVisible(dpStartPage); break;
    case 1: MakeVisible(dpSolutionExplorer); break;
    case 2: MakeVisible(dpClassView); break;
    case 3: MakeVisible(dpProperties); break;
    case 4: MakeVisible(dpToolbox); break;
    case 5: MakeVisible(dpCallStack); break;
    case 6: MakeVisible(dpOutput); break;
    case 7: MakeVisible(dpWatch); break;
  }
}
//---------------------------------------------------------------------------
void __fastcall TDockingMegaDemoMainForm::dxBarButtonLoadClick(TObject *Sender)
{
  if (OpenDialog1->Execute())
    dxDockingManager1->LoadLayoutFromIniFile(OpenDialog1->FileName);
}
//---------------------------------------------------------------------------

void __fastcall TDockingMegaDemoMainForm::dxBarButtonSaveClick(TObject *Sender)
{
  if (SaveDialog1->Execute())
    dxDockingManager1->SaveLayoutToIniFile(SaveDialog1->FileName);
}
//---------------------------------------------------------------------------

void __fastcall TDockingMegaDemoMainForm::dxBarButtonColorClick(TObject *Sender)
{
  ColorDialog1->Color = dxDockingManager1->Color;
  if (ColorDialog1->Execute())
    dxDockingManager1->Color = ColorDialog1->Color;
}
//---------------------------------------------------------------------------

void __fastcall TDockingMegaDemoMainForm::dxBarButtonFontClick(TObject *Sender)
{
  FontDialog1->Font = dxDockingManager1->Font;
  if (FontDialog1->Execute())
    dxDockingManager1->Font = FontDialog1->Font;
}
//---------------------------------------------------------------------------

void __fastcall TDockingMegaDemoMainForm::dxbLookAndFeelKindClick(TObject *Sender)
{
  LookAndFeelController->Kind = (TcxLookAndFeelKind)((TdxBarListItem *)Sender)->ItemIndex;
  LookAndFeelController->NativeStyle = False;
  UpdateLookAndFeelMenu();
}
//---------------------------------------------------------------------------

void __fastcall TDockingMegaDemoMainForm::dxbNativeClick(TObject *Sender)
{
  LookAndFeelController->NativeStyle = True;
  UpdateLookAndFeelMenu();
}
//---------------------------------------------------------------------------

void __fastcall TDockingMegaDemoMainForm::frStartPage1Label1Click(TObject *Sender)
{
  dxBarButtonLoadClick(Sender);
}
//---------------------------------------------------------------------------

void __fastcall TDockingMegaDemoMainForm::frStartPage1Label2Click(TObject *Sender)
{
  dxBarButtonSaveClick(Sender);
}
//---------------------------------------------------------------------------

void __fastcall TDockingMegaDemoMainForm::dxBarButtonExitClick(TObject *Sender)
{
  Close();  
}
//---------------------------------------------------------------------------


void __fastcall TDockingMegaDemoMainForm::dxBarButtonDockableClick(TObject *Sender)
{
  if (FPopupMenuDockControl != NULL) {
    FPopupMenuDockControl->Dockable = ((TdxBarButton*)Sender)->Down;
    FPopupMenuDockControl = NULL;
  }
}
//---------------------------------------------------------------------------

void __fastcall TDockingMegaDemoMainForm::dxBarButtonHideClick(TObject *Sender)
{
  if (FPopupMenuDockControl != NULL) {
    FPopupMenuDockControl->Visible = false;
    FPopupMenuDockControl = NULL;
  }
}
//---------------------------------------------------------------------------

void __fastcall TDockingMegaDemoMainForm::dxBarButtonFloatingClick(TObject *Sender)
{
  TPoint pt;
  if ((FPopupMenuDockControl != NULL) && (FPopupMenuDockControl->DockState != dsFloating)) {
    GetCursorPos(&pt);
    FPopupMenuDockControl->MakeFloating(pt.x, pt. y);
    FPopupMenuDockControl = NULL;
  }
}
//---------------------------------------------------------------------------

void __fastcall TDockingMegaDemoMainForm::dxBarButtonAutoHideClick(TObject *Sender)
{
  if (FPopupMenuDockControl != NULL) {
    FPopupMenuDockControl->AutoHide = ((TdxBarButton*)Sender)->Down;
    FPopupMenuDockControl = NULL;
  }
}
//---------------------------------------------------------------------------

void __fastcall TDockingMegaDemoMainForm::UpdateLookAndFeelMenu()
{
  dxbNative->Down = LookAndFeelController->NativeStyle;
  if (LookAndFeelController->NativeStyle)
    dxbLookAndFeelKinds->ItemIndex = -1;
  else
    dxbLookAndFeelKinds->ItemIndex = (Integer)LookAndFeelController->Kind;
}
//---------------------------------------------------------------------------

void __fastcall TDockingMegaDemoMainForm::dpContextPopup(TObject *Sender, const TPoint &MousePos, bool &Handled)
{
  TPoint pt;
  GetCursorPos(&pt);
  TdxCustomDockControl* AControl = dxDockingController()->GetDockControlAtPos(pt);
  if (AControl != NULL) {
    FPopupMenuDockControl = AControl;
    dxBarButtonDockable->Down = FPopupMenuDockControl->Dockable;
    dxBarButtonFloating->Down = FPopupMenuDockControl->FloatDockSite != NULL;
    dxBarButtonAutoHide->Enabled = FPopupMenuDockControl->CanAutoHide();
    dxBarButtonAutoHide->Down = FPopupMenuDockControl->AutoHide;
    dxBarPopupMenu1->PopupFromCursorPos();
    Handled = true;
  }
}
//---------------------------------------------------------------------------

void __fastcall TDockingMegaDemoMainForm::SetEnableState(TWinControl *AContainer, Boolean AValue)
{
  for (int I = 0; I < AContainer->ControlCount - 1; I++)
  {
      AContainer->Controls[I]->Enabled = AValue;
      if (dynamic_cast<TWinControl*>(AContainer->Controls[I]))
	    SetEnableState((TWinControl *)AContainer->Controls[I], AValue);
  }
}
//---------------------------------------------------------------------------

void __fastcall TDockingMegaDemoMainForm::UpdateProperties()
{
  Boolean AHasDockControl = ((FDockControl != NULL) && !FDockControl->ComponentState.Contains(csDestroying));
  SetEnableState(ScrollBox1, AHasDockControl);
  SetEnableState(Panel4, AHasDockControl);
  if (AHasDockControl)
  {
    cbManagerColor->Checked = FDockControl->ManagerColor;
    cbManagerFont->Checked = FDockControl->ManagerFont;
    eCaption->Text = FDockControl->Caption;
    cbShowCaption->Checked = FDockControl->ShowCaption;
    cbShowCloseButton->Checked = FDockControl->CaptionButtons.Contains(cbClose);
    cbShowMaxButton->Checked = FDockControl->CaptionButtons.Contains(cbMaximize);
    cbShowHideButton->Checked = FDockControl->CaptionButtons.Contains(cbHide);

    cbAllowDockLeft->Checked = FDockControl->AllowDock.Contains(dtLeft);
    cbAllowDockTop->Checked = FDockControl->AllowDock.Contains(dtTop);
    cbAllowDockRight->Checked = FDockControl->AllowDock.Contains(dtRight);
    cbAllowDockBottom->Checked = FDockControl->AllowDock.Contains(dtBottom);
    cbAllowDockClient->Checked = FDockControl->AllowDock.Contains(dtClient);
    cbAllowFloating->Checked = FDockControl->AllowFloating;
    cbAllowDockClientsLeft->Checked = FDockControl->AllowDockClients.Contains(dtLeft);
    cbAllowDockClientsTop->Checked = FDockControl->AllowDockClients.Contains(dtTop);
    cbAllowDockClientsRight->Checked = FDockControl->AllowDockClients.Contains(dtRight);
    cbAllowDockClientsBottom->Checked = FDockControl->AllowDockClients.Contains(dtBottom);
    cbAllowDockClientsClient->Checked = FDockControl->AllowDockClients.Contains(dtClient);
	
	cbVisible->Checked = FDockControl->Visible;

	SetEnableState(Panel6, dynamic_cast<TdxTabContainerDockSite*>(FDockControl));
	if (dynamic_cast<TdxTabContainerDockSite*>(FDockControl))
	{
      cbTabsPosition->ItemIndex = (Integer)(((TdxTabContainerDockSite *)FDockControl)->TabsProperties->TabPosition);
      cbTabsRotate->Checked = ((TdxTabContainerDockSite *)FDockControl)->TabsProperties->Rotate;
      cbCloseButtonMode->ItemIndex = (Integer)((TdxTabContainerDockSite*)FDockControl)->TabsProperties->CloseButtonMode;
      cbTabsScrollable->Checked = ((TdxTabContainerDockSite*)FDockControl)->TabsProperties->TabsScroll;
    };
  };
}
//---------------------------------------------------------------------------

void __fastcall TDockingMegaDemoMainForm::ApplyProperties()
{
  TdxDockingTypes ADockTypes;
  if ((FDockControl == NULL) || (FDockControl->ComponentState.Contains(csDestroying))) return;

  FDockControl->ManagerColor = cbManagerColor->Checked;
  FDockControl->ManagerFont = cbManagerFont->Checked;
  FDockControl->Caption = eCaption->Text;
  FDockControl->ShowCaption = cbShowCaption->Checked;
  FDockControl->CaptionButtons.Clear();
  if (cbShowCloseButton->Checked)
    FDockControl->CaptionButtons = FDockControl->CaptionButtons + (TdxCaptionButtons() << cbClose);
  if (cbShowMaxButton->Checked)
    FDockControl->CaptionButtons = FDockControl->CaptionButtons + (TdxCaptionButtons() << cbMaximize);
  if (cbShowHideButton->Checked)
    FDockControl->CaptionButtons = FDockControl->CaptionButtons + (TdxCaptionButtons() << cbHide);

  ADockTypes.Clear();
  if (cbAllowDockLeft->Checked) ADockTypes = ADockTypes + (TdxDockingTypes() << dtLeft);
  if (cbAllowDockTop->Checked) ADockTypes = ADockTypes + (TdxDockingTypes() << dtTop);
  if (cbAllowDockRight->Checked) ADockTypes = ADockTypes + (TdxDockingTypes() << dtRight);
  if (cbAllowDockBottom->Checked) ADockTypes = ADockTypes + (TdxDockingTypes() << dtBottom);
  if (cbAllowDockClient->Checked) ADockTypes = ADockTypes + (TdxDockingTypes() << dtClient);
  FDockControl->AllowDock = ADockTypes;

  FDockControl->AllowFloating = cbAllowFloating->Checked;
  FDockControl->Visible = cbVisible->Checked;

  ADockTypes.Clear();
  if (cbAllowDockClientsLeft->Checked) ADockTypes = ADockTypes + (TdxDockingTypes() << dtLeft);
  if (cbAllowDockClientsTop->Checked) ADockTypes = ADockTypes + (TdxDockingTypes() << dtTop);
  if (cbAllowDockClientsRight->Checked) ADockTypes = ADockTypes + (TdxDockingTypes() << dtRight);
  if (cbAllowDockClientsBottom->Checked) ADockTypes = ADockTypes + (TdxDockingTypes() << dtBottom);
  if (cbAllowDockClientsClient->Checked) ADockTypes = ADockTypes + (TdxDockingTypes() << dtClient);
  FDockControl->AllowDockClients = ADockTypes;

  if(dynamic_cast<TdxTabContainerDockSite*>(FDockControl)) 
  {
	((TdxTabContainerDockSite*)FDockControl)->TabsProperties->TabPosition = (TcxTabPosition)cbTabsPosition->ItemIndex;
    ((TdxTabContainerDockSite*)FDockControl)->TabsProperties->CloseButtonMode = (TcxPCButtonMode)cbCloseButtonMode->ItemIndex;
    ((TdxTabContainerDockSite*)FDockControl)->TabsProperties->Rotate = cbTabsRotate->Checked;
    ((TdxTabContainerDockSite*)FDockControl)->TabsProperties->TabsScroll = cbTabsScrollable->Checked;
  }
}
//---------------------------------------------------------------------------

void __fastcall TDockingMegaDemoMainForm::SetDockControl(TdxCustomDockControl* Value)
{
  if (FDockControl != Value) {
    FDockControl = Value;
    UpdateProperties();
  }
}
//---------------------------------------------------------------------------

void __fastcall TDockingMegaDemoMainForm::btnApplyClick(TObject *Sender)
{
  ApplyProperties();
  UpdateProperties();
}
//---------------------------------------------------------------------------

void __fastcall TDockingMegaDemoMainForm::btnCancelClick(TObject *Sender)
{
  UpdateProperties();
}
//---------------------------------------------------------------------------


void __fastcall TDockingMegaDemoMainForm::tvSolutionExplorerChange(
      TObject *Sender, TTreeNode *Node)
{
  TTreeNode* ANode = tvSolutionExplorer->Selected;
  if (ANode != NULL)
    DockControl = ((TdxCustomDockControl*)ANode->Data);
  else DockControl = dsHost;
}
//---------------------------------------------------------------------------

void __fastcall TDockingMegaDemoMainForm::dxDockingManager1LayoutChanged(
      TdxCustomDockControl *Sender)
{
  UpdateSolutionTreeView();
}
//---------------------------------------------------------------------------

void __fastcall TDockingMegaDemoMainForm::dxBarButtonStartPageClick(
      TObject *Sender)
{
  if (dpStartPage != NULL) {
    if (!dpStartPage->Visible)
      dpStartPage->Visible = true;
    dxDockingController()->ActiveDockControl = dpStartPage;
  }
}
//---------------------------------------------------------------------------

void __fastcall TDockingMegaDemoMainForm::dxBarButtonSolutionExplorerClick(
      TObject *Sender)
{
  if (dpSolutionExplorer != NULL) {
    if (!dpSolutionExplorer->Visible)
      dpSolutionExplorer->Visible = true;
    dxDockingController()->ActiveDockControl = dpSolutionExplorer;
  }
}
//---------------------------------------------------------------------------

void __fastcall TDockingMegaDemoMainForm::dxBarButtonClassViewClick(
      TObject *Sender)
{
  if (dpClassView != NULL) {
    if (!dpClassView->Visible)
      dpClassView->Visible = true;
    dxDockingController()->ActiveDockControl = dpClassView;
  }
}
//---------------------------------------------------------------------------

void __fastcall TDockingMegaDemoMainForm::dxBarButtonPropertiesClick(
      TObject *Sender)
{
  if (dpProperties != NULL) {
    if (!dpProperties->Visible)
      dpProperties->Visible = true;
    dxDockingController()->ActiveDockControl = dpProperties;
  }
}
//---------------------------------------------------------------------------

void __fastcall TDockingMegaDemoMainForm::dxBarButtonToolBoxClick(
      TObject *Sender)
{
  if (dpToolbox != NULL) {
    if (!dpToolbox->Visible)
      dpToolbox->Visible = true;
    dxDockingController()->ActiveDockControl = dpToolbox;
  }
}
//---------------------------------------------------------------------------

void __fastcall TDockingMegaDemoMainForm::dxBarButtonCallStackClick(
      TObject *Sender)
{
  if (dpCallStack != NULL) {
    if (!dpCallStack->Visible)
      dpCallStack->Visible = true;
    dxDockingController()->ActiveDockControl = dpCallStack;
  }
}
//---------------------------------------------------------------------------

void __fastcall TDockingMegaDemoMainForm::dxBarButtonOutputClick(
      TObject *Sender)
{
  if (dpOutput != NULL) {
  if (!dpOutput->Visible)
    dpOutput->Visible = true;
  dxDockingController()->ActiveDockControl = dpOutput;
  }
}
//---------------------------------------------------------------------------

void __fastcall TDockingMegaDemoMainForm::dxBarButtonWatchClick(
      TObject *Sender)
{
  if (dpWatch != NULL) {
    if (!dpWatch->Visible)
      dpWatch->Visible = true;
    dxDockingController()->ActiveDockControl = dpWatch;
  }
}
//---------------------------------------------------------------------------
void __fastcall TDockingMegaDemoMainForm::FormCreate(TObject *Sender)
{
  dpStartPage->OnContextPopup = (TContextPopupEvent)&dpContextPopup;
  dpProperties->OnContextPopup = (TContextPopupEvent)&dpContextPopup;
  dpSolutionExplorer->OnContextPopup = (TContextPopupEvent)&dpContextPopup;
  dpClassView->OnContextPopup = (TContextPopupEvent)&dpContextPopup;
  dpOutput->OnContextPopup = (TContextPopupEvent)&dpContextPopup;
  dpCallStack->OnContextPopup = (TContextPopupEvent)&dpContextPopup;
  dpWatch->OnContextPopup = (TContextPopupEvent)&dpContextPopup;
  dpToolbox->OnContextPopup = (TContextPopupEvent)&dpContextPopup;
  UpdateLookAndFeelMenu();
}
//---------------------------------------------------------------------------

