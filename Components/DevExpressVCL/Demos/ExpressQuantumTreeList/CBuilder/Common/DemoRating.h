//---------------------------------------------------------------------------

#ifndef DemoRatingH
#define DemoRatingH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ExtCtrls.hpp>
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
  void __fastcall btnSendClick(TObject *Sender);
  void __fastcall rgRatePropertiesChange(TObject *Sender);
private:	// User declarations
public:		// User declarations
  __fastcall TDemoRatingForm(TComponent* Owner);
  void __fastcall AdjustMessageBody(AnsiString &ABody, AnsiString ASearchStr, AnsiString AReplaceStr);
};
//---------------------------------------------------------------------------
extern PACKAGE TDemoRatingForm *DemoRatingForm;
//---------------------------------------------------------------------------
#endif
