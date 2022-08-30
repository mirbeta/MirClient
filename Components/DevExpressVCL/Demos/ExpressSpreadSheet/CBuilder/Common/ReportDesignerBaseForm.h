//---------------------------------------------------------------------------

#ifndef ReportDesignerBaseFormH
#define ReportDesignerBaseFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "BaseForm.h"
#include "ReportPreviewUnit.h"
#include "cxCheckBox.hpp"
#include "cxClasses.hpp"
#include "cxContainer.hpp"
#include "cxControls.hpp"
#include "cxEdit.hpp"
#include "cxFilterControl.hpp"
#include "cxGraphics.hpp"
#include "cxGroupBox.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxLookAndFeels.hpp"
#include "cxSplitter.hpp"
#include "dxSpreadSheetCore.hpp"
#include "dxSpreadSheetReportDesigner.hpp"
#include <ComCtrls.hpp>
#include <Dialogs.hpp>
#include <ExtCtrls.hpp>
#include <dxmdaset.hpp>
#include <Menus.hpp>
//---------------------------------------------------------------------------
class TfrmReportDesignerBase : public TfmBaseForm
{
__published:	// IDE-managed Components
	TPanel *Panel1;
	TcxSplitter *cxSplitter2;
	TcxGroupBox *cxgFieldChooserSite;
	TcxGroupBox *cxgFilter;
	TcxCheckBox *cbxUseFilter;
	TcxFilterControl *Filter;
	TButton *btnApply;
	TButton *btnClear;
	TdxSpreadSheetReportDesigner *ReportDesigner;
	TMenuItem *miDesignView;
	TMenuItem *miPreview;
	TMenuItem *miSections;
	TMenuItem *N2;
	TMenuItem *miRemove;
	TMenuItem *miHeader;
	TMenuItem *miFooter;
	TMenuItem *miDetail;
	TMenuItem *N3;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall miDesignViewClick(TObject *Sender);
	void __fastcall miPreviewClick(TObject *Sender);
	void __fastcall miRemoveClick(TObject *Sender);
	void __fastcall OnSectionClick(TObject *Sender);
	void __fastcall ReportDesignerSelectionChanged(TObject *Sender);
	void __fastcall FilterChanged(TObject *Sender);
	void __fastcall FormDestroy(TObject *Sender);
	void __fastcall btnApplyClick(TObject *Sender);
	void __fastcall btnClearClick(TObject *Sender);
private:	// User declarations
	virtual TdxSpreadSheet* GetSpreadSheet();
public:		// User declarations
	UnicodeString FilterFileName;
	__fastcall TfrmReportDesignerBase(TComponent* Owner);
	void __fastcall TfrmReportDesignerBase::AfterConstruction();
	virtual void Initialize();
	void LoadDataset(TdxMemData *ADataSet, const UnicodeString AFileName);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmReportDesignerBase *frmReportDesignerBase;
//---------------------------------------------------------------------------
#endif

