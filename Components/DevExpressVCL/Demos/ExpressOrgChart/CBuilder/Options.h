//---------------------------------------------------------------------------
#ifndef OptionsH
#define OptionsH
//---------------------------------------------------------------------------
#include <classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ExtCtrls.hpp>
#include <Buttons.hpp>
#include <Mask.hpp>
#include "main.h"
#include "cxControls.hpp"
#include "cxGraphics.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxLookAndFeels.hpp"
#include <Menus.hpp>
#include "Spin.hpp"
//---------------------------------------------------------------------------
class TOptionsForm : public TForm
{
__published:	// IDE-managed Components
	TBevel *Bevel1;
	TButton *BitBtn2;
	TButton *BitBtn1;
	TCheckBox *cbShowImages;
	TCheckBox *cbInsDel;
	TCheckBox *cbShowDrag;
	TCheckBox *cbCanDrag;
	TCheckBox *cbSelect;
	TCheckBox *cbFocus;
	TCheckBox *cbButtons;
	TCheckBox *cbEdit;
	TGroupBox *GroupBox1;
	TCheckBox *cbLeft;
	TCheckBox *cbCenter;
	TCheckBox *cbRight;
	TCheckBox *cbVCenter;
	TCheckBox *cbWrap;
	TCheckBox *cbUpper;
	TCheckBox *cbLower;
	TCheckBox *cbGrow;
	TLabel *Label3;
	TLabel *Label2;
	TLabel *Label1;
	TSpinEdit *seX;
	TSpinEdit *seY;
	TSpinEdit *seLineWidth;
	void __fastcall FormActivate(TObject *Sender);
	void __fastcall BitBtn2Click(TObject *Sender);
private:	// User declarations
public:		// User declarations
	__fastcall TOptionsForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern TOptionsForm *OptionsForm;
//---------------------------------------------------------------------------
#endif
