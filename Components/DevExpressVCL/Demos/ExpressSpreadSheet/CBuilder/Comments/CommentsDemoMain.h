//---------------------------------------------------------------------------

#ifndef CommentsDemoMainH
#define CommentsDemoMainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "BaseForm.h"
#include "cxClasses.hpp"
#include "cxControls.hpp"
#include "cxGraphics.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxLookAndFeels.hpp"
#include "dxCore.hpp"
#include "dxCoreClasses.hpp"
#include "dxHashUtils.hpp"
#include "dxSpreadSheet.hpp"
#include "dxSpreadSheetClasses.hpp"
#include "dxSpreadSheetCore.hpp"
#include "dxSpreadSheetFormulas.hpp"
#include "dxSpreadSheetFunctions.hpp"
#include "dxSpreadSheetGraphics.hpp"
#include "dxSpreadSheetTypes.hpp"
#include "dxSpreadSheetConditionalFormatting.hpp"
#include "dxSpreadSheetFormulaBar.hpp"
#include "cxSplitter.hpp"
#include <ComCtrls.hpp>
#include <Dialogs.hpp>
#include <Menus.hpp>
//---------------------------------------------------------------------------
class TfrmComments : public TfmBaseForm
{
__published:	// IDE-managed Components
	TdxSpreadSheet *SpreadSheet;
    TdxSpreadSheetFormulaBar* FormulaBar;
    TcxSplitter* FormulaBarSplitter;
	
	void __fastcall FormCreate(TObject *Sender);
private:	// User declarations
protected:
	virtual TdxSpreadSheet* GetSpreadSheet();
public:		// User declarations
	__fastcall TfrmComments(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmComments *frmComments;
//---------------------------------------------------------------------------
#endif
