//---------------------------------------------------------------------------

#ifndef uExportToBitmapsH
#define uExportToBitmapsH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "uExportToFileDialog.h"
#include "cxCheckBox.hpp"
#include "cxClasses.hpp"
#include "cxContainer.hpp"
#include "cxControls.hpp"
#include "cxEdit.hpp"
#include "cxGraphics.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxLookAndFeels.hpp"
#include "cxMaskEdit.hpp"
#include "cxSpinEdit.hpp"
#include "cxTextEdit.hpp"
#include "dxLayoutContainer.hpp"
#include "dxLayoutControl.hpp"
#include "dxLayoutControlAdapters.hpp"
#include "dxLayoutcxEditAdapters.hpp"
//---------------------------------------------------------------------------
class TfrmExportToBitmaps : public TfrmExportToFileDialog
{
__published:	// IDE-managed Components
	TcxTextEdit *teFilePrefix;
	TdxLayoutItem *dxLayoutItem2;
	void __fastcall FormCreate(TObject *Sender);
private:	// User declarations
public:		// User declarations
	__fastcall TfrmExportToBitmaps(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmExportToBitmaps *frmExportToBitmaps;
//---------------------------------------------------------------------------
#endif
