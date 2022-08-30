//---------------------------------------------------------------------------

#ifndef ViewBandeDemoBandsH
#define ViewBandeDemoBandsH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxButtons.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxListBox.hpp"
#include "cxContainer.hpp"
#include "cxControls.hpp"
#include "cxEdit.hpp"
#include "cxGraphics.hpp"
#include "cxLookAndFeels.hpp"
#include <Menus.hpp>
//---------------------------------------------------------------------------
class TViewBandeDemoBandsForm : public TForm
{
__published:	// IDE-managed Components
  TcxListBox *lbBands;
  TcxButton *btnOK;
  TcxButton *btnCancel;
  void __fastcall btnCancelClick(TObject *Sender);
private:	// User declarations
public:		// User declarations
  __fastcall TViewBandeDemoBandsForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TViewBandeDemoBandsForm *ViewBandeDemoBandsForm;
//---------------------------------------------------------------------------
#endif
