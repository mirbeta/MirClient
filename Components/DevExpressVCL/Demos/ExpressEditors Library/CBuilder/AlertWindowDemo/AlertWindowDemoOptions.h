//---------------------------------------------------------------------------

#ifndef AlertWindowDemoOptionsH
#define AlertWindowDemoOptionsH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxButtons.hpp"
#include "cxCheckBox.hpp"
#include "cxContainer.hpp"
#include "cxControls.hpp"
#include "cxDropDownEdit.hpp"
#include "cxEdit.hpp"
#include "cxGraphics.hpp"
#include "cxGroupBox.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxLookAndFeels.hpp"
#include "cxMaskEdit.hpp"
#include "cxSpinEdit.hpp"
#include "cxTextEdit.hpp"
#include "dxAlertWindow.hpp"
#include <Menus.hpp>
#include "cxLabel.hpp"
//---------------------------------------------------------------------------
class TFormOptions : public TForm
{
__published:	// IDE-managed Components
	TcxButton *btOk;
	TcxButton *btCancel;
	TcxGroupBox *gbOptionsManager;
	TcxLabel *lbPosition;
	TcxComboBox *cbPosition;
	TcxSpinEdit *seMaxInDisplay;
	TcxGroupBox *gbOptionsAnimation;
	TcxLabel *lbShowingDirection;
	TcxComboBox *cbShowingDirection;
	TcxLabel *lbShowingAnimation;
	TcxLabel *lbHidingAnimation;
	TcxComboBox *cbHidingAnimation;
	TcxComboBox *cbHidingDirection;
	TcxLabel *lbHidingDirection;
	TcxComboBox *cbShowingAnimation;
	TcxGroupBox *gbOptionsSize;
	TcxLabel *lbWidth;
	TcxLabel *lbHeight;
	TcxSpinEdit *seWidth;
	TcxSpinEdit *seHeight;
	TcxCheckBox *cbAutoWidth;
	TcxCheckBox *cbAutoHeight;
	TcxGroupBox *gbOptionsBehavior;
	TcxLabel *lbTime;
	TcxSpinEdit *seTime;
	TcxCheckBox *cbCloseOnRightClick;
	TcxCheckBox *cbHotTrack;
	TcxCheckBox *cbAutoSizeAdjustment;
	TcxCheckBox *cbCollapseEmptySlots;
	void __fastcall btnApplyClick(TObject *Sender);
	void __fastcall btCancelClick(TObject *Sender);
	void __fastcall btOkClick(TObject *Sender);
private:	// User declarations
public:		// User declarations
	__fastcall TFormOptions(TComponent* Owner);
	void LoadWindowOptions(TdxAlertWindowManager* AManager);
	void SaveWindowOptions(TdxAlertWindowManager* AManager);
};
//---------------------------------------------------------------------------
extern PACKAGE TFormOptions *FormOptions;
//---------------------------------------------------------------------------
#endif
