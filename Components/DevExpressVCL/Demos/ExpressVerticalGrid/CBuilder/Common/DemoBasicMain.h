//---------------------------------------------------------------------------

#ifndef DemoBasicMainH
#define DemoBasicMainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxLookAndFeels.hpp"
#include <ActnList.hpp>
#include <ComCtrls.hpp>
#include <ImgList.hpp>
#include <Menus.hpp>
//---------------------------------------------------------------------------
class TDemoBasicMainForm : public TForm
{
__published:	// IDE-managed Components
  TLabel *lbDescrip;
  TStatusBar *sbMain;
  TMemo *memAboutText;
  TMainMenu *mmMain;
  TMenuItem *miFile;
  TMenuItem *miExit;
  TMenuItem *miOptions;
  TMenuItem *miSeparator;
  TMenuItem *miShowDemoDescription;
  TMenuItem *miHelp;
  TMenuItem *miGridHelp;
  TMenuItem *miRate;
  TMenuItem *miSeparator3;
  TMenuItem *miProducts;
  TMenuItem *miDownloads;
  TMenuItem *miForum;
  TMenuItem *miDeveloperExpressontheweb;
  TMenuItem *miSeparator4;
  TMenuItem *miAbout;
  TImageList *ilMain;
  TActionList *alMain;
  TAction *actHelp;
  TAction *actRateDemo;
  TAction *actDownloads;
  TAction *actForum;
  TAction *actDXOnTheWeb;
  TAction *actProducts;
  TAction *actAbout;
  TAction *actExit;
  TAction *actShowDemoDescription;
  TcxLookAndFeelController *cxLookAndFeelController;
  TMenuItem *miTouchMode;
  void __fastcall miTouchModeClick(TObject *Sender);
  void __fastcall actDXOnTheWebExecute(TObject *Sender);
  void __fastcall actExitExecute(TObject *Sender);
  void __fastcall actShowDemoDescriptionExecute(TObject *Sender);
  void __fastcall actHelpExecute(TObject *Sender);
  void __fastcall actRateDemoExecute(TObject *Sender);
  void __fastcall actAboutExecute(TObject *Sender);
	void __fastcall FormCreate(TObject *Sender);
protected:
  void __fastcall AdjustAboutText(TStrings *AAboutText);
  void __fastcall ShowAbout(bool AModal, bool AOnTop);
public:		// User declarations
  __fastcall TDemoBasicMainForm(TComponent* Owner);
  virtual void __fastcall AddLookAndFeelMenu();
  virtual TcxLookAndFeelKind __fastcall GetDefaultLookAndFeelKind();
  virtual bool __fastcall IsNativeDefaultStyle();
  virtual void __fastcall SetDefaultLookAndFeel();
};
//---------------------------------------------------------------------------
extern PACKAGE TDemoBasicMainForm *DemoBasicMainForm;
//---------------------------------------------------------------------------
#endif
