//---------------------------------------------------------------------------

#ifndef BasicDemoMainH
#define BasicDemoMainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ActnList.hpp>
#include <Menus.hpp>
#include "cxControls.hpp"
#include "dxLayoutControl.hpp"
#include "dxLayoutContainer.hpp"

enum dxSitePage {spProducts, spDownloads, spStart, spSupport, spMyDX};

//---------------------------------------------------------------------------
class TfrmBasicDemoMain : public TForm
{
__published:	// IDE-managed Components
	TMainMenu *mmMain;
	TMenuItem *File1;
	TMenuItem *Exit1;
	TMenuItem *Options1;
	TMenuItem *Customization1;
	TMenuItem *Autosize1;
	TMenuItem *miStyle;
	TMenuItem *Standard1;
	TMenuItem *Office1;
	TMenuItem *Web1;
	TMenuItem *UsecxLookAndFeel1;
	TMenuItem *UltraFlat1;
	TMenuItem *Flat1;
	TMenuItem *Standard2;
	TMenuItem *Office111;
	TMenuItem *Native1;
	TMenuItem *Help1;
	TMenuItem *DeveloperExpressProducts1;
	TMenuItem *DeveloperExpressDownloads1;
	TMenuItem *DeveloperExpressontheWeb1;
	TMenuItem *SupportCenter1;
	TMenuItem *N2;
	TMenuItem *Aboutthisdemo1;
	TActionList *alMain;
	TAction *acLayoutStandard;
	TAction *acLayoutOffice;
	TAction *acLayoutWeb;
	TAction *acFlat;
	TAction *acStandard;
	TAction *acUltraFlat;
	TAction *acOffice11;
	TAction *acNative;
	TAction *aAutosize;
	TdxLayoutControl *lcMain;
	TdxLayoutGroup *lcMainGroup_Root1;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall LayoutStyleExecute(TObject *Sender);
	void __fastcall aAutosizeExecute(TObject *Sender);
	void __fastcall Aboutthisdemo1Click(TObject *Sender);
	void __fastcall DeveloperExpressProducts1Click(TObject *Sender);
	void __fastcall Exit1Click(TObject *Sender);
	void __fastcall Customization1Click(TObject *Sender);
private:	// User declarations
public:		// User declarations
	__fastcall TfrmBasicDemoMain(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmBasicDemoMain *frmBasicDemoMain;
//---------------------------------------------------------------------------
#endif
