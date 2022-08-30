//---------------------------------------------------------------------------

#ifndef DemoBasicMainH
#define DemoBasicMainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxLookAndFeels.hpp"
#include "cxTLExportLink.hpp"
#include "cxTL.hpp"
#include <ActnList.hpp>
#include <ComCtrls.hpp>
#include <ImgList.hpp>
#include <Menus.hpp>
#include <Dialogs.hpp>
//---------------------------------------------------------------------------
class TDemoBasicMainForm : public TForm
{
__published:	// IDE-managed Components
  TStatusBar *sbMain;
  TMainMenu *mmMain;
  TMenuItem *miFile;
  TMenuItem *miExit;
  TMenuItem *miOptions;
  TMenuItem *miSeparator2;
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
  TMenuItem *miSupport;
  TMenuItem *Exportto1;
  TMenuItem *excel1;
  TMenuItem *Excel2;
  TMenuItem *html1;
  TMenuItem *text1;
  TMenuItem *xml1;
  TMenuItem *miAbout;
  TImageList *ilMain;
  TActionList *alMain;
  TAction *actHelp;
  TAction *actDownloads;
  TAction *actForum;
  TAction *actDXOnTheWeb;
  TAction *actProducts;
  TAction *actAbout;
  TAction *actExit;
  TAction *actShowDemoDescription;
  TAction *actSupport;
  TAction *actRateDemo;
  TSaveDialog *SaveDialog1;
  TcxLookAndFeelController *cxLookAndFeelController;
  TLabel  *lscrip;
  TMenuItem *miTouchMode;
  void __fastcall actAboutExecute(TObject *Sender);
  void __fastcall actDXOnTheWebExecute(TObject *Sender);
  void __fastcall actExitExecute(TObject *Sender);
  void __fastcall actHelpExecute(TObject *Sender);
  void __fastcall actRateDemoExecute(TObject *Sender);
  void __fastcall actShowDemoDescriptionExecute(TObject *Sender);
  void __fastcall ExportToClick(TObject *Sender);
  void __fastcall FormCreate(TObject *Sender);
  void __fastcall miTouchModeClick(TObject *Sender);
private:
  TcxCustomTreeList* __fastcall GetTreeList();
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
 
