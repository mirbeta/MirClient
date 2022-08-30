//---------------------------------------------------------------------------

#ifndef OutlineDemoMainH
#define OutlineDemoMainH
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
#include "dxSpreadSheetFormulaBar.hpp"
#include "cxSplitter.hpp"
#include <ComCtrls.hpp>
#include <Dialogs.hpp>
#include <Menus.hpp>
//---------------------------------------------------------------------------
class TfrmOutline : public TfmBaseForm
{
__published:	// IDE-managed Components
	TdxSpreadSheet *SpreadSheet;
    TMenuItem *miGroupColumns;
    TMenuItem *miGrouping;
    TMenuItem *miGroupRows;
    TMenuItem *miLine2;
    TMenuItem *miUngroupColumns;
    TMenuItem *miUngroupRows;	
    TMenuItem *GroupExpandButtonPosition1;
    TMenuItem *N2;
    TMenuItem *miGroupStart;
    TMenuItem *miGroupFinish;	
    TdxSpreadSheetFormulaBar* FormulaBar;
    TcxSplitter* FormulaBarSplitter;
	
	void __fastcall FormCreate(TObject *Sender);
    void __fastcall miGroupColumnsClick(TObject *Sender);
    void __fastcall miGroupRowsClick(TObject *Sender);
    void __fastcall miUngroupColumnsClick(TObject *Sender);
    void __fastcall miUngroupRowsClick(TObject *Sender);	
	void __fastcall miGroupFinishClick(TObject *Sender);
private:	// User declarations
protected:
	virtual TdxSpreadSheet* GetSpreadSheet();
public:		// User declarations
	__fastcall TfrmOutline(TComponent* Owner);
	void __fastcall UpdateExpandButtonState();
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmOutline *frmOutline;
//---------------------------------------------------------------------------
#endif
