//---------------------------------------------------------------------------

#ifndef BaseFormH
#define BaseFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxClasses.hpp"
#include "cxLookAndFeels.hpp"
#include <ComCtrls.hpp>
#include <Dialogs.hpp>
#include <Menus.hpp>
#include "dxSpreadSheet.hpp"
//---------------------------------------------------------------------------
class TfmBaseForm : public TForm
{
__published:	// IDE-managed Components
	TStatusBar *sbMain;
	TMainMenu *mmMain;
	TMenuItem *miFile;
	TMenuItem *miSaveAs;
	TMenuItem *miOptions;
	TMenuItem *miExit;
	TMenuItem *miShowFormulas;
	TMenuItem *miAbout;
	TcxLookAndFeelController *cxLookAndFeelController1;
	TSaveDialog *SaveDialog;
	void __fastcall miSaveAsClick(TObject *Sender);
	void __fastcall miExitClick(TObject *Sender);
	void __fastcall miAboutClick(TObject *Sender);
	void __fastcall miShowFormulasClick(TObject *Sender);
	void __fastcall FormCreate(TObject *Sender);
private:	// User declarations
	TcxLookAndFeelController *FLookAndFeelController;
	void __fastcall TouchModeClick(TObject *Sender);
protected:
	virtual TdxSpreadSheet* GetSpreadSheet();
public:		// User declarations
	__fastcall TfmBaseForm(TComponent* Owner);
	virtual void __fastcall AddLookAndFeelMenu();
	virtual void __fastcall CreateTouchModeMenuOption();
	virtual TcxLookAndFeelKind __fastcall GetDefaultLookAndFeelKind();
	virtual bool __fastcall IsNativeDefaultStyle();
    virtual void __fastcall SetDefaultLookAndFeel();
};
//---------------------------------------------------------------------------
extern PACKAGE TfmBaseForm *fmBaseForm;
//---------------------------------------------------------------------------
#endif
