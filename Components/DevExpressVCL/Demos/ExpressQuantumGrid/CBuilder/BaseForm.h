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
#include "cxGridTableView.hpp"
#include "cxLookAndFeels.hpp"
#include "cxGridCardView.hpp"

//---------------------------------------------------------------------------
class TfmBaseForm : public TForm
{
__published:	// IDE-managed Components
	TLabel *lbDescription;
	TMenuItem *miAbout;
	TMenuItem *miExit;
	TMenuItem *miFile;
	TMainMenu *mmMain;
	TcxStyleRepository *StyleRepository;
        TcxLookAndFeelController *cxLookAndFeelController1;
	TcxStyle *cxStyle1;
	TcxStyle *cxStyle2;
	TcxStyle *cxStyle3;
	TcxStyle *cxStyle4;
	TcxStyle *cxStyle5;
	TcxStyle *cxStyle6;
	TcxStyle *cxStyle7;
	TcxStyle *cxStyle8;
	TcxStyle *cxStyle9;
    TcxStyle *cxStyle10;
    TcxStyle *cxStyle11;
	TcxStyle *cxStyle12;
	TcxStyle *cxStyle13;
	TcxStyle *cxStyle14;
	TcxStyle *cxStyle15;
	TcxStyle *cxStyle16;
	TcxStyle *cxStyle17;
	TcxStyle *cxStyle18;
	TcxStyle *cxStyle19;
	TcxStyle *cxStyle20;
	TcxStyle *cxStyle21;
	TcxStyle *cxStyle22;
	TcxStyle *cxStyle23;
	TcxStyle *cxStyle24;
	TcxGridTableViewStyleSheet *GridTableViewStyleSheetDevExpress;
	TcxGridCardViewStyleSheet *GridCardViewStyleSheetDevExpress;
	TStatusBar *sbMain;
	void __fastcall miAboutClick(TObject *Sender);
	void __fastcall miExitClick(TObject *Sender);
public:		// User declarations
  __fastcall TfmBaseForm(TComponent* Owner);
};

#endif
