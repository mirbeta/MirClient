//---------------------------------------------------------------------------
#ifndef SpreadSheetRLMainH
#define SpreadSheetRLMainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxControls.hpp"
#include <ActnList.hpp>
#include <ComCtrls.hpp>
#include <Dialogs.hpp>
#include <ExtCtrls.hpp>
#include <ImgList.hpp>
#include <Menus.hpp>
#include <ToolWin.hpp>
#include "dxPSBaseGridLnk.hpp"
#include "dxPSCore.hpp"
#include "dxBkgnd.hpp"
#include "dxPrnDev.hpp"
#include "dxPrnPg.hpp"
#include "dxPSCompsProvider.hpp"
#include "dxPSEdgePatterns.hpp"
#include "dxPSEngn.hpp"
#include "dxPSFillPatterns.hpp"
#include "dxPSGlbl.hpp"
#include "dxPSUtl.hpp"
#include "dxWrap.hpp"
#include <DB.hpp>
#include "cxGraphics.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxLookAndFeels.hpp"
#include "cxDrawTextUtils.hpp"
#include "dxPScxEditorProducers.hpp"
#include "dxPScxExtEditorProducers.hpp"
#include "dxPScxPageControlProducer.hpp"
#include "dxPSPDFExport.hpp"
#include "dxPSPDFExportCore.hpp"
#include "dxPSPrVwStd.hpp"
#include "DemoBasicMain.h"
#include "dxmdaset.hpp"
#include "cxClasses.hpp"
#include "dxPSStdGrLnk.hpp"
#include "dxBarBuiltInMenu.hpp"
#include "dxCore.hpp"
#include "dxCoreClasses.hpp"
#include "dxHashUtils.hpp"
#include "dxPSdxSpreadSheetLnk.hpp"
#include "dxSpreadSheet.hpp"
#include "dxSpreadSheetClasses.hpp"
#include "dxSpreadSheetCore.hpp"
#include "dxSpreadSheetCoreHistory.hpp"
#include "dxSpreadSheetFormulas.hpp"
#include "dxSpreadSheetFunctions.hpp"
#include "dxSpreadSheetGraphics.hpp"
#include "dxSpreadSheetPrinting.hpp"
#include "dxSpreadSheetTypes.hpp"
//#include "dxSpreadSheetFormatCellsDialog.hpp"

//---------------------------------------------------------------------------
class TSpreadSheetRLForm : public TDemoBasicMainForm
{
__published:	// IDE-managed Components
	TSaveDialog* SaveDialog;
	TAction* actDeleteCells;
	TAction* actSave;
	TAction* actInsertCells;
	TAction* actCut;
	TAction* actCopy;
	TAction* actPaste;
	TAction* actFormatCells;
	TAction* actOpen;
	TAction* actSetPrintArea;
	TAction* actClearPrintArea;
	TMenuItem* miSaveSpreadSheet;
	TMenuItem* LoadData1;
	TMenuItem* MenuItem6;
	TMenuItem* PrintArea1;
	TMenuItem* SetPrintArea1;
	TMenuItem* ClearPrintArea1;
	TMenuItem* mnuEdit;
	TMenuItem* miCut;
	TMenuItem* miCopy;
	TMenuItem* miPaste;
	TMenuItem* N7;
	TMenuItem* miCells;
	TMenuItem* miFormat;
	TMenuItem* miDeletecells;
	TMenuItem* Insertcells1;
	TToolButton *ToolButton2;
	TToolButton *ToolButton3;
	TToolButton* ToolButton4;
	TToolButton* ToolButton5;
	TToolBar* ToolBar2;
	TPanel* pnCellsRect;
	TPanel* Panel2;
	TEdit* edtCellEdit;
	TdxSpreadSheet* dxSpreadSheet1;
	TdxSpreadSheetReportLnk* dxComponentPrinterLink1;
	TOpenDialog* OpenDialog;
	TToolButton* tbsOpen;
	void __fastcall actPreviewExecute(TObject* Sender);
	void __fastcall actPrintExecute(TObject* Sender);
	void __fastcall actSetPrintAreaExecute(TObject* Sender);
	void __fastcall actClearPrintAreaExecute(TObject* Sender);
	void __fastcall actPrintSetupExecute(TObject* Sender);
	void __fastcall FormCreate(TObject* Sender);
	void __fastcall actOpenExecute(TObject* Sender);
	void __fastcall actSaveExecute(TObject* Sender);
	void __fastcall actCutExecute(TObject* Sender);
	void __fastcall actCopyExecute(TObject* Sender);
	void __fastcall actPasteExecute(TObject* Sender);
	void __fastcall actExitExecute(TObject* Sender);
	void __fastcall actDeleteCellsExecute(TObject* Sender);
	void __fastcall actInsertCellsExecute(TObject* Sender);
	void __fastcall actFormatCellsExecute(TObject* Sender);
	void __fastcall dxSpreadSheet1ActiveCellChanging(TdxSpreadSheetTableView* Sender,
	  const TPoint ANewActiveCell, bool &ACanSelect);
    void __fastcall edtCellEditChange(TObject* Sender);
private:	// User declarations
  TdxSpreadSheetCell* GetCell(int ARow, int AColumn);
  TdxSpreadSheet* GetSpreadSheet();
public:		// User declarations
  __fastcall TSpreadSheetRLForm(TComponent* Owner);

  __property TdxSpreadSheet *SpreadSheet = {read = GetSpreadSheet};
};
//---------------------------------------------------------------------------
extern PACKAGE TSpreadSheetRLForm *SpreadSheetRLForm;
//---------------------------------------------------------------------------
#endif
