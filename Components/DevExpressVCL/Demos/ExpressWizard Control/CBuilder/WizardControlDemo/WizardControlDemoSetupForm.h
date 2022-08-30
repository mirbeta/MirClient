//---------------------------------------------------------------------------

#ifndef WizardControlDemoSetupFormH
#define WizardControlDemoSetupFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxButtons.hpp"
#include "cxCheckBox.hpp"
#include "cxContainer.hpp"
#include "cxControls.hpp"
#include "cxEdit.hpp"
#include "cxGraphics.hpp"
#include "cxGroupBox.hpp"
#include "cxLabel.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxLookAndFeels.hpp"
#include "cxRadioGroup.hpp"
#include "cxTreeView.hpp"
#include "dxCustomWizardControl.hpp"
#include <ComCtrls.hpp>
#include <Menus.hpp>
//---------------------------------------------------------------------------
class TWizardControlDemoSetupForm : public TForm
{
__published:	// IDE-managed Components
  TcxButton *btnStartDemo;
  TcxCheckBox *cbSkinForm;
  TcxLabel *lbChooseSkin;
  TcxRadioGroup *rgTransitionEffect;
  TcxRadioGroup *rgViewStyle;
  TcxTreeView *tvSkins;
  void __fastcall FormCreate(TObject *Sender);
  void __fastcall tvSkinsChange(TObject *Sender, TTreeNode *Node);
  void __fastcall tvSkinsCustomDrawItem(TCustomTreeView *Sender, TTreeNode *Node, TCustomDrawState *State, boolean &DefaultDraw);
  void __fastcall cbSkinFormClick(TObject *Sender);
private:	// User declarations
  void ChangeStyle(TcxCustomLookAndFeelPainter *APainter);
public:		// User declarations
  bool GetSkinForm();
  TdxWizardControlTransitionEffect GetTransitionEffect();
  TdxWizardControlViewStyle GetViewStyle();
  __fastcall TWizardControlDemoSetupForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TWizardControlDemoSetupForm *WizardControlDemoSetupForm;
//---------------------------------------------------------------------------
#endif