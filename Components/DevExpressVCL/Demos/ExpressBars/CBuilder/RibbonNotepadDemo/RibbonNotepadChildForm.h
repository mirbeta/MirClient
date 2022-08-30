//---------------------------------------------------------------------------

#ifndef RibbonNotepadChildFormH
#define RibbonNotepadChildFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxContainer.hpp"
#include "cxControls.hpp"
#include "cxEdit.hpp"
#include "cxGraphics.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxLookAndFeels.hpp"
#include "cxMemo.hpp"
#include "cxRichEdit.hpp"
#include "cxTextEdit.hpp"
#include "NotepadChildForm.h"
#include <Dialogs.hpp>
#include <ExtCtrls.hpp>
//---------------------------------------------------------------------------
class TfrmRibbonNotepadChild : public TfrmNotepadChild
{
__published:	// IDE-managed Components
private:	// User declarations
public:		// User declarations
	__fastcall TfrmRibbonNotepadChild(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmRibbonNotepadChild *frmRibbonNotepadChild;
//---------------------------------------------------------------------------
#endif
