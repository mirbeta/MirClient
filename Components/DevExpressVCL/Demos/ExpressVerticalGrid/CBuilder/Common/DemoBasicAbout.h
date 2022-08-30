//---------------------------------------------------------------------------

#ifndef DemoBasicAboutH
#define DemoBasicAboutH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxButtons.hpp"
#include "cxContainer.hpp"
#include "cxControls.hpp"
#include "cxEdit.hpp"
#include "cxImage.hpp"
#include "cxLookAndFeelPainters.hpp"
#include <ComCtrls.hpp>
#include <ExtCtrls.hpp>
#include <Graphics.hpp>
//---------------------------------------------------------------------------
class TDemoBasicAboutForm : public TForm
{
__published:	// IDE-managed Components
  TLabel *lbDemoName;
  TLabel *lbCopyright;
  TBevel *bvBottom;
  TLabel *lbCompanyName;
  TRichEdit *reDemoInfo;
  TcxButton *btnOK;
  TcxImage *cxImage;
  void __fastcall btnOKClick(TObject *Sender);
  void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
  void __fastcall FormDestroy(TObject *Sender);
private:	// User declarations
public:		// User declarations
  __fastcall TDemoBasicAboutForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TDemoBasicAboutForm *DemoBasicAboutForm;
//---------------------------------------------------------------------------
#endif
