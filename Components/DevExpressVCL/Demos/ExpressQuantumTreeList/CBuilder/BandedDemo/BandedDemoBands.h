//---------------------------------------------------------------------------

#ifndef BandedDemoBandsH
#define BandedDemoBandsH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxButtons.hpp"
#include "cxContainer.hpp"
#include "cxControls.hpp"
#include "cxListBox.hpp"
#include "cxLookAndFeelPainters.hpp"
//---------------------------------------------------------------------------
class TBandedDemoBandsForm : public TForm
{
__published:	// IDE-managed Components
  TcxListBox *lbBands;
  TcxButton *btnOK;
  TcxButton *btnCancel;
  void __fastcall btnCancelClick(TObject &Sender);
private:	// User declarations
public:		// User declarations
  __fastcall TBandedDemoBandsForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TBandedDemoBandsForm *BandedDemoBandsForm;
//---------------------------------------------------------------------------
#endif
