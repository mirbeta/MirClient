//---------------------------------------------------------------------------

#ifndef CustomDrawDemoMainH
#define CustomDrawDemoMainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "BaseForm.h"
#include "cxClasses.hpp"
#include "cxLookAndFeels.hpp"
#include <ComCtrls.hpp>
#include <Dialogs.hpp>
#include <Menus.hpp>
#include "cxControls.hpp"
#include "cxGraphics.hpp"
#include "cxLookAndFeelPainters.hpp"
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
#include "dxSpreadSheetFormulaBar.hpp"
#include "cxSplitter.hpp"
//---------------------------------------------------------------------------
class TfrmCustomDraw : public TfmBaseForm
{
__published:	// IDE-managed Components
	TdxSpreadSheet *SpreadSheet;
    TdxSpreadSheetFormulaBar* FormulaBar;
    TcxSplitter* FormulaBarSplitter;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall SpreadSheetCustomDrawTableViewCell(TdxSpreadSheetTableView *Sender, TcxCanvas *ACanvas, TdxSpreadSheetTableViewCellViewInfo *AViewInfo,
		  bool &AHandled);
private:	// User declarations
protected:
	virtual TdxSpreadSheet* GetSpreadSheet();
public:		// User declarations
	__fastcall TfrmCustomDraw(TComponent* Owner);
	void __fastcall DrawCallout(TcxCanvas *ACanvas, TdxSpreadSheetTableViewCellViewInfo *AViewInfo);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmCustomDraw *frmCustomDraw;
//---------------------------------------------------------------------------
#endif
