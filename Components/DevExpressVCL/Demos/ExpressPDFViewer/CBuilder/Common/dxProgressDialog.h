//---------------------------------------------------------------------------

#ifndef dxProgressDialogH
#define dxProgressDialogH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxContainer.hpp"
#include "cxControls.hpp"
#include "cxEdit.hpp"
#include "cxGraphics.hpp"
#include "cxLabel.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxLookAndFeels.hpp"
#include "cxProgressBar.hpp"
//---------------------------------------------------------------------------
class TfrmProgress : public TForm
{
__published:	// IDE-managed Components
	TcxProgressBar *pbProgress;
	TcxLabel *lbTitle;
private:	// User declarations
public:		// User declarations
	__fastcall TfrmProgress(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmProgress *frmProgress;
//---------------------------------------------------------------------------
#endif
