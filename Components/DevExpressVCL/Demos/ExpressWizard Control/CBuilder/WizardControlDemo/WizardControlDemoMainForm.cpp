//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "WizardControlDemoMainForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxControls"
#pragma link "cxGraphics"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxLookAndFeels"
#pragma link "dxCustomWizardControl"
#pragma link "dxWizardControl"
#pragma link "dxWizardControlForm"
#pragma link "cxCheckBox"
#pragma link "cxCheckListBox"
#pragma link "cxClasses"
#pragma link "cxContainer"
#pragma link "cxEdit"
#pragma link "cxLabel"
#pragma link "cxMemo"
#pragma link "cxProgressBar"
#pragma link "cxRadioGroup"
#pragma link "cxShellCommon"
#pragma link "cxShellTreeView"
#pragma link "cxTextEdit"
#pragma link "cxTreeView"
#pragma link "dxBevel"
#pragma link "dxBreadcrumbEdit"
#pragma link "dxGDIPlusClasses"
#pragma link "dxImageSlider"
#pragma link "dxShellBreadcrumbEdit"
#pragma resource "*.dfm"
#pragma resource "*.res"
TfrmWizardControlDemoMain *frmWizardControlDemoMain;

const char *sdxActionInstall = "Click Install to begin the installation.";
const char *sdxActionRemove = "Click Remove to remove DevExpress VCL Products from your computer.";
const char *sdxActionRemoveHint = "After removal, these products will no longer be available for use.";
const char *sdxActionRepair = "Click Repair to fix any installed files, shortcuts and registry entries.";
const char *sdxActionRepairHint = "This option also recompiles and re-registers all the installed products in the IDE(s).";
const char *sdxDeletingHelp = "Deleting help files...";
const char *sdxDeletingItem = "Deleting files for %s...";
const char *sdxEULAPath = "EULA.txt";
const char *sdxExitMessage = "Do you want to exit WizardControl demo?";
const char *sdxInstallButtonCaption = "&Install";
const char *sdxInstallingHelp = "Installing help files...";
const char *sdxInstallingItem = "Installing files for %s...";
const char *sdxNextButtonCaption = "&Next";
const char *sdxReadyToInstall = "Ready to Install";
const char *sdxReadyToRemove = "Ready to Uninstall";
const char *sdxReadyToRepair = "Ready to Repair";
const char *sdxRemoveButtonCaption = "&Remove";
const char *sdxRepairButtonCaption = "&Repair";
const char *sdxTransparentIcon = "TRANSPARENTICON";

//---------------------------------------------------------------------------
class TcxRadioButtonAccess : public TcxRadioButton
{
public:
  TRect TextRectAccess()
  {
	return this->TextRect;
  };
};
//---------------------------------------------------------------------------
__fastcall TfrmWizardControlDemoMain::TfrmWizardControlDemoMain(TComponent* Owner)
		: TdxWizardControlForm(Owner)
{
}
//---------------------------------------------------------------------------
TInstallMode TfrmWizardControlDemoMain::GetInstallMode()
{
  if(rbModify->Checked)
    return imModify;
  else
    if(rbRemove->Checked)
      return imRemove;
    else
      return imRepair;
}
//---------------------------------------------------------------------------
bool TfrmWizardControlDemoMain::GetIsPlatformsSelected()
{
  bool Res;
  Res = False;
  for(int I = 0; I < clbPlatforms->Items->Count; I++)
	Res = Res || clbPlatforms->Items->Items[I]->Checked;
  return Res;
}
//---------------------------------------------------------------------------
void TfrmWizardControlDemoMain::PrepareEULAPage()
{
  wcMain->Buttons->Back->Enabled = False;
  wcMain->Buttons->Next->Enabled = cbAcceptEULA->Checked;
}
//---------------------------------------------------------------------------
void TfrmWizardControlDemoMain::PrepareFinishPage()
{
  wcMain->Buttons->Back->Enabled = False;
  wcMain->Buttons->Cancel->Enabled = False;
}
//---------------------------------------------------------------------------
void TfrmWizardControlDemoMain::PopulateInstallingItems()
{
  TcxCheckListBoxItem *AItem;
  TVarRec *AArgs = new TVarRec[1];

  FInstallingItems->Add(sdxDeletingHelp);
  for(int I = 0; I < clbPlatforms->Items->Count; I++)
  {
	AArgs[0] = clbPlatforms->Items->Items[I]->Text;
	FInstallingItems->Add(Sysutils::Format(sdxDeletingItem, AArgs, 1));
  };
  if(GetInstallMode() != imRemove)
	{
	 for(int I = 0; I < clbPlatforms->Items->Count; I++)
	   {
		 AItem = clbPlatforms->Items->Items[I];
		 AArgs[0] = AItem->Text;
		 if(AItem->Checked || (GetInstallMode() == imRepair))
		   FInstallingItems->Add(Sysutils::Format(sdxInstallingItem, AArgs, 1));
	   };

	 if(cbInstallHelp->Checked || (GetInstallMode() == imRepair))
	   FInstallingItems->Add(sdxInstallingHelp);
	};
  delete[] AArgs;
}
//---------------------------------------------------------------------------
void TfrmWizardControlDemoMain::PrepareInProgressPage()
{
  PopulateInstallingItems();
  wcMain->Buttons->Back->Enabled = False;
  wcMain->Buttons->Next->Enabled = False;
  wcMain->Buttons->Cancel->Enabled = False;
  tmFeaturesSlider->Enabled = True;
  tmProgress->Enabled = True;
}
//---------------------------------------------------------------------------
void TfrmWizardControlDemoMain::PopulateSteps(TList *ASteps)
{
  for(int I = 0; I < wcMain->PageCount; I++)
	if(wcMain->Pages[I]->PageVisible)
	  ASteps->Add(wcMain->Pages[I]);
}
//---------------------------------------------------------------------------
void TfrmWizardControlDemoMain::DrawSteps(TList *ASteps, TcxBitmap *AWatermark, TdxWizardControlCustomPage *ACurPage)
{
  const int Indent = 10;

  TdxWizardControlPage *APage;
  int ACurStepHeight;
  int ARegularStepHeight;
  struct TRect ARect;

  ACurStepHeight = cxRectHeight(icStepItem->ClientRect);
  ARegularStepHeight = cxTextHeight(AWatermark->cxCanvas->Font);
  ARect = cxRectInflate(AWatermark->ClientRect, -Indent, -Indent * 2, 0, 0);
  for(int I = 0; I < ASteps->Count; I++)
  {
	APage = (TdxWizardControlPage*)ASteps->Items[I];
	if(APage == ACurPage)
	{
	  ARect = cxRectSetHeight(ARect, ACurStepHeight);
	  AWatermark->cxCanvas->Font->Color = clDefault;
	  AWatermark->cxCanvas->Font->Style << fsBold;
	  AWatermark->cxCanvas->Draw(0, ARect.Top, icStepItem->Picture->Graphic);
	}
	else
	{
	  ARect = cxRectSetHeight(ARect, ARegularStepHeight);
	  AWatermark->cxCanvas->Font->Color = (TColor)clcxLightGray;
	  AWatermark->cxCanvas->Font->Style >> fsBold;
	};
	cxDrawText(AWatermark->cxCanvas, APage->Header->Title, ARect, cxAlignCenter);
	ARect = cxRectSetTop(ARect, ARect.Bottom + Indent);
  };
}
//---------------------------------------------------------------------------
void TfrmWizardControlDemoMain::PreparePageWatermark(TdxWizardControlCustomPage *ACurPage)
{
  TList *ASteps;
  TcxBitmap *AWatermark = new TcxBitmap(icWatermarkItem->ClientRect);

  AWatermark = new TcxBitmap(icWatermarkItem->ClientRect);
  try
  {
	AWatermark->cxCanvas->Draw(0, 0, icWatermarkItem->Picture->Graphic);
	ASteps = new TList;
	try
	{
	  PopulateSteps(ASteps);
	  DrawSteps(ASteps, AWatermark, ACurPage);
	  ACurPage->Watermark->BackgroundImage->Image->SetBitmap(AWatermark);
	  ACurPage->Watermark->BackgroundImage->Margins->Bottom = AWatermark->Height - 10;
	}
	__finally
	{
	  ASteps->Free();
	};
  }
  __finally
  {
	AWatermark->Free();
  };
}
//---------------------------------------------------------------------------
void TfrmWizardControlDemoMain::PreparePathPage()
{
  stvTree->AbsolutePath = "";
}
//---------------------------------------------------------------------------
void TfrmWizardControlDemoMain::PrepareReadyToInstallPage()
{
  if(GetInstallMode() == imModify)
  {
	wcMain->Buttons->Next->Caption = sdxInstallButtonCaption;
	lbActionTitle->Caption = sdxActionInstall;
	lbActionHint->Caption = "";
  };
  if(GetInstallMode() == imRepair)
  {
	wcMain->Buttons->Next->Caption = sdxRepairButtonCaption;
	lbActionTitle->Caption = sdxActionRepair;
	lbActionHint->Caption = sdxActionRepairHint;
  };
  if(GetInstallMode() == imRemove)
  {
	wcMain->Buttons->Next->Caption = sdxRemoveButtonCaption;
	lbActionTitle->Caption = sdxActionRemove;
	lbActionHint->Caption = sdxActionRemoveHint;
  };
  lbActionHint->Visible = lbActionHint->Caption != "";
}
//---------------------------------------------------------------------------
void TfrmWizardControlDemoMain::PrepareSelectionPage()
{
  bool AIsPlatformsSelected;

  AIsPlatformsSelected = GetIsPlatformsSelected();
  wcMain->Buttons->Next->Enabled = AIsPlatformsSelected;
  cbInstallForAllUsers->Enabled = AIsPlatformsSelected;
  cbInstallHelp->Enabled = AIsPlatformsSelected;
}
//---------------------------------------------------------------------------
void TfrmWizardControlDemoMain::UpdateButtonsState()
{
  wcMain->Buttons->Back->Enabled = True;
  wcMain->Buttons->Next->Enabled = True;
  wcMain->Buttons->Cancel->Enabled = True;
}
//---------------------------------------------------------------------------
void __fastcall TfrmWizardControlDemoMain::cbAcceptEULAClick(TObject *Sender)
{
  wcMain->Buttons->Next->Enabled = cbAcceptEULA->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TfrmWizardControlDemoMain::clbPlatformsClickCheck(
  TObject *Sender, int AIndex, TcxCheckBoxState *APrevState, TcxCheckBoxState *ANewState)
{
  PrepareSelectionPage();
}
//---------------------------------------------------------------------------
void __fastcall TfrmWizardControlDemoMain::tmFeaturesSliderTimer(TObject *Sender)
{
  if(isFeatures->CanGoToNextImage())
	isFeatures->GoToNextImage();
  else
	isFeatures->GoToImage(0);
}
//---------------------------------------------------------------------------
void __fastcall TfrmWizardControlDemoMain::FormCloseQuery(TObject *Sender, bool &CanClose)
{
  int AResult;
  if(wcMain->ActivePage != wcpFinishPage)
  {
     AResult = MessageDlg(sdxExitMessage, mtConfirmation, TMsgDlgButtons() << mbYes << mbNo, 0);
	 if ((AResult == mrNo) || (AResult == mrCancel))
	   CanClose = False;
  }
}
//---------------------------------------------------------------------------
void TfrmWizardControlDemoMain::SetupAeroStyleFormIcon()
{
  TcxBitmap32 *ABitmap;
  TIcon *AIcon;
  TdxSmartImage *AGlyph;

  AIcon = new TIcon;
  try
  {
    int ADummy = 0;
	AIcon->Handle = (HICON)Perform(WM_GETICON, (WPARAM)ICON_SMALL2, ADummy);
	ABitmap = new TcxBitmap32(AIcon->Width, AIcon->Height, True);
	try
	{
	  ABitmap->Canvas->Draw(0, 0, AIcon);
	  AGlyph = new TdxSmartImage(ABitmap);
	  try
	  {
		wcMain->OptionsViewStyleAero->Title->Glyph ->Assign(AGlyph);
		Icon->Handle = LoadIconA(HInstance, sdxTransparentIcon);
	  }
	  __finally
	  {
		delete AGlyph;
	  };
	}
	__finally
	{
	  delete ABitmap;
	};
  }
  __finally
  {
	delete AIcon;
  };
}
//---------------------------------------------------------------------------
void TfrmWizardControlDemoMain::SetupAeroStyle()
{
  wcMain->OptionsViewStyleAero->Title->Text = Caption;
  Caption = "";
  SetupAeroStyleFormIcon();
}
//---------------------------------------------------------------------------
void TfrmWizardControlDemoMain::PlaceInfoLabels()
{
  lbModifyDescription->Left = rbModify->Left + ((TcxRadioButtonAccess*)rbModify)->TextRectAccess().Left;
  lbRepairDescription->Left = rbRepair->Left + ((TcxRadioButtonAccess*)rbRepair)->TextRectAccess().Left;
  lbRemoveDescription->Left = rbRemove->Left + ((TcxRadioButtonAccess*)rbRemove)->TextRectAccess().Left;
}
//---------------------------------------------------------------------------
void __fastcall TfrmWizardControlDemoMain::FormCreate(TObject *Sender)
{

  TWizardControlDemoSetupForm *ASetupSettngsForm;

  FInstallingItems = new TStringList;
  mmEULA->Lines->LoadFromFile(Application->GetNamePath() + sdxEULAPath);

  ASetupSettngsForm = new TWizardControlDemoSetupForm(NULL);
  try
  {
	if(ASetupSettngsForm->ShowModal() == mrOk)
	{
	  wcMain->OptionsAnimate->TransitionEffect = ASetupSettngsForm->GetTransitionEffect();
	  wcMain->ViewStyle = ASetupSettngsForm->GetViewStyle();
	  wcMain->OptionsViewStyleAero->EnableTitleAero = !(ASetupSettngsForm->GetSkinForm());
	  wcMain->LookAndFeel = RootLookAndFeel();
	  if(wcMain->ViewStyle == wcvsAero)
		SetupAeroStyle();
	};
	PlaceInfoLabels();
  }
  __finally
  {
	ASetupSettngsForm->Free();
  };
}
//---------------------------------------------------------------------------
void __fastcall TfrmWizardControlDemoMain::FormDestroy(TObject *Sender)
{
  	delete FInstallingItems;
}
//---------------------------------------------------------------------------
void __fastcall TfrmWizardControlDemoMain::tmProgressTimer(TObject *Sender)
{
  if(FInstallingItems->Count > 0)
  {
	lbInstallingItem->Caption = FInstallingItems->Strings[
	  Int(FInstallingItems->Count * (pbInstallationProgress->Position / 100))];
    pbInstallationProgress->Position = pbInstallationProgress->Position + 1;
	if(pbInstallationProgress->Position == 100)
    {
      tmFeaturesSlider->Enabled = False;
      tmProgress->Enabled = False;
	  wcMain->GoToNextPage();
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TfrmWizardControlDemoMain::rbModifyClick(TObject *Sender)
{
  wcpSelectionPage->PageVisible = ((TcxRadioButton*)Sender)->Tag == 0;
  wcpPathPage->PageVisible = ((TcxRadioButton*)Sender)->Tag == 0;
  if(GetInstallMode() == imModify)
	wcpReadyToInstall->Header->Title = sdxReadyToInstall;
  if(GetInstallMode() == imRepair)
	wcpReadyToInstall->Header->Title = sdxReadyToRepair;
  if(GetInstallMode() == imRemove)
	wcpReadyToInstall->Header->Title = sdxReadyToRemove;
  PreparePageWatermark(wcMain->ActivePage);
}
//---------------------------------------------------------------------------
void __fastcall TfrmWizardControlDemoMain::wcMainButtonClick(TObject *Sender,
  TdxWizardControlButtonKind AKind, bool &AHandled)
{
  if((AKind == wcbkFinish) || (AKind == wcbkCancel))
	Close();
}
//---------------------------------------------------------------------------
void __fastcall TfrmWizardControlDemoMain::wcMainPageChanging(
  TObject *Sender, TdxWizardControlCustomPage *NewPage, bool &AllowChange)
{
  UpdateButtonsState();
  PreparePageWatermark(NewPage);
  wcMain->Buttons->CustomButtons->Buttons->Items[0]->Visible = NewPage == wcpEULAPage;
  if(NewPage == wcpEULAPage)
	PrepareEULAPage();
  if(NewPage == wcpSelectionPage)
	PrepareSelectionPage();
  if(NewPage == wcpReadyToInstall)
	PrepareReadyToInstallPage();
  if(NewPage == wcpInProgressPage)
	PrepareInProgressPage();
  if(NewPage == wcpFinishPage)
	PrepareFinishPage();
  if(NewPage == wcpPathPage)
	PreparePathPage();
  if(wcMain->ActivePage == wcpReadyToInstall)
    wcMain->Buttons->Next->Caption = sdxNextButtonCaption;
}
//---------------------------------------------------------------------------
void __fastcall TfrmWizardControlDemoMain::wcMainCustomButtons0Click(TObject *Sender)
{
	ShowMessage("Clicked!");
}
//---------------------------------------------------------------------------
