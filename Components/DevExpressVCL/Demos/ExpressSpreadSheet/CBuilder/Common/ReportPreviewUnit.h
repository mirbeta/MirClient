//---------------------------------------------------------------------------

#ifndef ReportPreviewUnitH
#define ReportPreviewUnitH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxControls.hpp"
#include "cxGraphics.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxLookAndFeels.hpp"
#include "dxCore.hpp"
#include "dxCoreClasses.hpp"
#include "dxHashUtils.hpp"
#include "dxSpreadSheet.hpp"
#include "dxSpreadSheetClasses.hpp"
#include "dxSpreadSheetConditionalFormatting.hpp"
#include "dxSpreadSheetConditionalFormattingRules.hpp"
#include "dxSpreadSheetContainers.hpp"
#include "dxSpreadSheetCore.hpp"
#include "dxSpreadSheetCoreHistory.hpp"
#include "dxSpreadSheetFormulas.hpp"
#include "dxSpreadSheetFunctions.hpp"
#include "dxSpreadSheetGraphics.hpp"
#include "dxSpreadSheetHyperlinks.hpp"
#include "dxSpreadSheetPrinting.hpp"
#include "dxSpreadSheetTypes.hpp"
#include "dxSpreadSheetUtils.hpp"
#include <Dialogs.hpp>
#include <Menus.hpp>
//---------------------------------------------------------------------------
class TfrmPreview : public TForm
{
__published:	// IDE-managed Components
	TdxSpreadSheet *ssResult;
	TMainMenu *MainMenu1;
	TMenuItem *File1;
	TMenuItem *miSaveAs;
	TMenuItem *N1;
	TMenuItem *Close1;
	TMenuItem *Options1;
	TMenuItem *miShowFormulas;
	TSaveDialog *sveDialog;
	void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
	void __fastcall miCloseClick(TObject *Sender);
	void __fastcall miShowFormulasClick(TObject *Sender);
	void __fastcall miSaveAsClick(TObject *Sender);
public:		// User declarations
	__fastcall TfrmPreview(TComponent* Owner);
	inline __fastcall virtual ~TfrmPreview();
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmPreview *Preview;
//---------------------------------------------------------------------------
#endif
