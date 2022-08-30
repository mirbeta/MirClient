//---------------------------------------------------------------------------
#ifndef SchedulerRLMainH
#define SchedulerRLMainH
//---------------------------------------------------------------------------
#include "cxDemosBCB.inc"
#include "cxControls.hpp"
#include "cxEdit.hpp"
#include "cxGraphics.hpp"
#include "cxScheduler.hpp"
#include "cxSchedulerCustomControls.hpp"
#include "cxSchedulerCustomResourceView.hpp"
#include "cxSchedulerDateNavigator.hpp"
#include "cxSchedulerDayView.hpp"
#include "cxStyles.hpp"
#include <Classes.hpp>
#include <ComCtrls.hpp>
#include <Controls.hpp>
#include <Dialogs.hpp>
#include <ExtCtrls.hpp>
#include <Menus.hpp>
#include <StdCtrls.hpp>
#include "cxSchedulerStorage.hpp"
#include <ImgList.hpp>
#include <ToolWin.hpp>
#include "dxPSCore.hpp"
#include "dxPScxCommon.hpp"
#include "dxPScxSchedulerLnk.hpp"
#include "cxDrawTextUtils.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxLookAndFeels.hpp"
#include "cxSchedulerGanttView.hpp"
#include "cxSchedulerHolidays.hpp"
#include "cxSchedulerTimeGridView.hpp"
#include "cxSchedulerUtils.hpp"
#include "cxSchedulerWeekView.hpp"
#include "cxSchedulerYearView.hpp"
#include "dxBkgnd.hpp"
#include "dxPrnDev.hpp"
#include "dxPrnPg.hpp"
#include "dxPSCompsProvider.hpp"
#include "dxPScxEditorProducers.hpp"
#include "dxPScxExtEditorProducers.hpp"
#include "dxPScxPageControlProducer.hpp"
#include "dxPSEdgePatterns.hpp"
#include "dxPSEngn.hpp"
#include "dxPSFillPatterns.hpp"
#include "dxPSGlbl.hpp"
#include "dxPSPDFExport.hpp"
#include "dxPSPDFExportCore.hpp"
#include "dxPSPrVwStd.hpp"
#include "dxPSUtl.hpp"
#include "dxWrap.hpp"
#include "DemoBasicMain.h"
#include <ActnList.hpp>

#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxControls.hpp"
#include "cxEdit.hpp"
#include "cxGraphics.hpp"
#include "cxScheduler.hpp"
#include "cxSchedulerStorage.hpp"
#include "cxSchedulerOutlookExchange.hpp"
#include "cxSchedulerCustomControls.hpp"
#include "cxSchedulerCustomResourceView.hpp"
#include "cxSchedulerDateNavigator.hpp"
#include "cxSchedulerDayView.hpp"
#include "cxStyles.hpp"
#include <ComCtrls.hpp>
#include <Dialogs.hpp>
#include <ExtCtrls.hpp>
#include <Menus.hpp>
//---------------------------------------------------------------------------
class TSchedulerRLMainForm : public TDemoBasicMainForm
{
__published:	// IDE-managed Components
		TcxScheduler *Scheduler;
	TcxSchedulerStorage *Storage;

        void __fastcall FormCreate(TObject* Sender);
        void __fastcall tbnGenerateClick(TObject *Sender);
private:	// User declarations
public:		// User declarations
        TDateTime AnchorDate;
        __fastcall TSchedulerRLMainForm(TComponent* Owner);
        void GenerateRandomEvents(int ACount, bool ARandomResource = false);
protected:
    String GetRandomCaption();
    TDateTime GetRandomDate();
    TColor GetRandomLabelColor();
    Variant GetRandomResourceID();
    int GetRandomState();
};
//---------------------------------------------------------------------------
extern PACKAGE TSchedulerRLMainForm *SchedulerRLMainForm;
//---------------------------------------------------------------------------
#endif
