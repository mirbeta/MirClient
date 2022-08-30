//---------------------------------------------------------------------------

#ifndef UnboundModeDemoCustomFieldH
#define UnboundModeDemoCustomFieldH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxButtons.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "UnboundModeDemoTypes.h"
//---------------------------------------------------------------------------
class TUnboundModeDemoCustomFieldForm : public TForm
{
__published:	// IDE-managed Components
  TLabel *lbHeight;
  TLabel *lbWidth;
  TLabel *lbMineCount;
  TEdit *edtHeight;
  TEdit *edtWidth;
  TEdit *edtMineCount;
  TcxButton *btnOK;
  TcxButton *bntCancel;
  void __fastcall edtKeyPress(TObject *Sender, char &Key);
private:	// User declarations
public:		// User declarations
  __fastcall TUnboundModeDemoCustomFieldForm(TComponent* Owner);
  int __fastcall ShowModal();
};
//---------------------------------------------------------------------------
extern PACKAGE TUnboundModeDemoCustomFieldForm *UnboundModeDemoCustomFieldForm;
//---------------------------------------------------------------------------
#endif
