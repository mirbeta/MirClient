//---------------------------------------------------------------------------

#ifndef RealtorWorldUnderConstructionH
#define RealtorWorldUnderConstructionH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxContainer.hpp"
#include "cxControls.hpp"
#include "cxEdit.hpp"
#include "cxGraphics.hpp"
#include "cxImage.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxLookAndFeels.hpp"
#include "dxGDIPlusClasses.hpp"
#include "RealtorWorldBaseFrame.h"
//---------------------------------------------------------------------------
class TfrmUnderConstruction : public TfrmBase
{
__published:	// IDE-managed Components
	TcxImage *cxImage1;
private:	// User declarations
public:		// User declarations
	__fastcall TfrmUnderConstruction(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmUnderConstruction *frmUnderConstruction;
//---------------------------------------------------------------------------
#endif
