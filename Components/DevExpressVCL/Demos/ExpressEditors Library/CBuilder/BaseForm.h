//---------------------------------------------------------------------------

#ifndef BaseFormH
#define BaseFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <SysUtils.hpp>
#include <Graphics.hpp>
#include <Dialogs.hpp>
#include <StdCtrls.hpp>
#include <Menus.hpp>
#include <ComCtrls.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxClasses.hpp"
#include "cxStyles.hpp"
#include "cxLookAndFeels.hpp"

//---------------------------------------------------------------------------
class TfmBaseForm : public TForm
{
__published:	// IDE-managed Components
	TLabel *lbDescription;
	TMenuItem *miAbout;
	TMenuItem *miExit;
	TMenuItem *miFile;
	TMainMenu *mmMain;
	void __fastcall miAboutClick(TObject *Sender);
	void __fastcall miExitClick(TObject *Sender);
protected:
	void __fastcall BuildLookAndFeelMenu();
public:		// User declarations
  __fastcall TfmBaseForm(TComponent* Owner);
};

#endif
