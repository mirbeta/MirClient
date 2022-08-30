//---------------------------------------------------------------------------

#ifndef SimpleReportDemoMainH
#define SimpleReportDemoMainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "BaseForm.h"
#include "dxSpreadSheetReportDesigner.hpp"
#include "ReportPreviewUnit.h"
#include "cxClasses.hpp"
#include "cxLookAndFeels.hpp"
#include <ComCtrls.hpp>
#include <Dialogs.hpp>
#include <Menus.hpp>
#include "cxCheckBox.hpp"
#include "cxContainer.hpp"
#include "cxControls.hpp"
#include "cxEdit.hpp"
#include "cxGraphics.hpp"
#include "cxGroupBox.hpp"
#include "cxLabel.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxSplitter.hpp"
#include "cxTextEdit.hpp"
#include "dxSpreadSheetCore.hpp"
#include "dxSpreadSheetReportDesigner.hpp"
#include <ExtCtrls.hpp>
#include <DB.hpp>
#include <DBClient.hpp>
#include <dxmdaset.hpp>
#include "cxFilterControl.hpp"
#include "ReportDesignerBaseForm.h"
//---------------------------------------------------------------------------
class TfrmSimpleReport : public TfrmReportDesignerBase
{
__published:	// IDE-managed Components
	TDataSource *dsOrders;
	TdxMemData *mdsOrders;
private:	// User declarations
protected:
public:		// User declarations
	 __fastcall TfrmSimpleReport(TComponent* Owner);
	virtual void Initialize();
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmSimpleReport *frmSimpleReport;
//---------------------------------------------------------------------------
#endif
