//---------------------------------------------------------------------------

#ifndef DemoRatingH
#define DemoRatingH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxButtons.hpp"
#include "cxContainer.hpp"
#include "cxControls.hpp"
#include "cxEdit.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxMemo.hpp"
#include "cxRadioGroup.hpp"
#include "cxTextEdit.hpp"
//---------------------------------------------------------------------------
class TDemoRatingForm : public TForm
{
__published:	// IDE-managed Components
  TLabel *Label1;
  TLabel *Label2;
  TLabel *Label3;
  TLabel *Label4;
  TcxMemo *memRateDescrip;
  TcxButton *btnSend;
  TcxRadioGroup *rgRate;
  void __fastcall rgRateChange(TObject *Sender);
  void __fastcall btnSendClick(TObject *Sender);
private:	// User declarations
  void __fastcall AdjustMessageBody(String &ABody, String ASearchStr,
    String AReplaceStr);
public:		// User declarations
  __fastcall TDemoRatingForm(TComponent* Owner);
};

//---------------------------------------------------------------------------
extern PACKAGE TDemoRatingForm *DemoRatingForm;
//---------------------------------------------------------------------------
#endif
