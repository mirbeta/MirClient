//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "WizardControlDemoSetupForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxControls"
#pragma link "cxGraphics"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxLookAndFeels"
#pragma link "dxCustomWizardControl"
#pragma link "dxWizardControl"
#pragma link "dxWizardControlForm"
#pragma link "cxButtons"
#pragma link "cxCheckBox"
#pragma link "cxContainer"
#pragma link "cxControls"
#pragma link "cxEdit"
#pragma link "cxGraphics"
#pragma link "cxGroupBox"
#pragma link "cxLabel"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxLookAndFeels"
#pragma link "cxRadioGroup"
#pragma link "cxTreeView"
#pragma resource "*.dfm"
TWizardControlDemoSetupForm *WizardControlDemoSetupForm;
//---------------------------------------------------------------------------
__fastcall TWizardControlDemoSetupForm::TWizardControlDemoSetupForm(TComponent* Owner)
        : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TWizardControlDemoSetupForm::FormCreate(TObject *Sender)
{
  TTreeNode *AStandardNode, *ASkinsNode;
  TcxCustomLookAndFeelPainter *APainter;

  ASkinsNode = tvSkins->Items->Add(NULL, "Skins");
  AStandardNode = tvSkins->Items->Add(NULL, "Standard");
  for(int I = 0; I < cxLookAndFeelPaintersManager->Count; I++)
  {
    APainter = cxLookAndFeelPaintersManager->Items[I];
	if(APainter->LookAndFeelStyle() == lfsSkin)
	  tvSkins->Items->AddChildObject(ASkinsNode, APainter->LookAndFeelName(), APainter);
    else
	  tvSkins->Items->AddChildObject(AStandardNode, APainter->LookAndFeelName(), APainter);
  };
  tvSkins->FullExpand();

  if(ASkinsNode->Count == 0)
  {
	AStandardNode->getFirstChild()->Selected = True;
	ASkinsNode->Free();
  }
  else
	ASkinsNode->getFirstChild()->Selected = True;
}
//---------------------------------------------------------------------------
bool TWizardControlDemoSetupForm::GetSkinForm()
{
  return cbSkinForm->Checked && cbSkinForm->Enabled;
}
//---------------------------------------------------------------------------
void __fastcall TWizardControlDemoSetupForm::cbSkinFormClick(TObject *Sender)
{
	//nothing
};
//---------------------------------------------------------------------------
TdxWizardControlTransitionEffect TWizardControlDemoSetupForm::GetTransitionEffect()
{
  if(rgTransitionEffect->ItemIndex == 0)
    return wcteNone;
  else
    if(rgTransitionEffect->ItemIndex == 1)
      return wcteFade;
    else
      return wcteSlide;
}
//---------------------------------------------------------------------------
TdxWizardControlViewStyle TWizardControlDemoSetupForm::GetViewStyle()
{
  if(rgViewStyle->ItemIndex == 0)
    return wcvsAero;
  else
    return wcvsWizard97;
}
//---------------------------------------------------------------------------
void TWizardControlDemoSetupForm::ChangeStyle(TcxCustomLookAndFeelPainter *APainter)
{
  cbSkinForm->Enabled = False;
  if(APainter->LookAndFeelStyle() == lfsNative)
	RootLookAndFeel()->NativeStyle = True;
  else
	if(APainter->LookAndFeelStyle() == lfsSkin)
	{
	  RootLookAndFeel()->NativeStyle = False;
	  RootLookAndFeel()->SkinName = APainter->LookAndFeelName();
	  cbSkinForm->Enabled = True;
	}
	else
	{
	  RootLookAndFeel()->NativeStyle = False;
	  RootLookAndFeel()->SkinName = "";
	  RootLookAndFeel()->SetStyle(APainter->LookAndFeelStyle());
	};
}
//---------------------------------------------------------------------------
void __fastcall TWizardControlDemoSetupForm::tvSkinsChange(TObject *Sender, TTreeNode *Node)
{
  if(tvSkins->Selected->Data != NULL)
	ChangeStyle((TcxCustomLookAndFeelPainter*)tvSkins->Selected->Data);
}
//---------------------------------------------------------------------------
void __fastcall TWizardControlDemoSetupForm::tvSkinsCustomDrawItem(TCustomTreeView *Sender, TTreeNode *Node, TCustomDrawState *State, boolean &DefaultDraw)
{
  if(Node->Data == NULL)
    Sender->Canvas->Font->Style << fsBold;
}
//---------------------------------------------------------------------------
