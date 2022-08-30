//---------------------------------------------------------------------------
#include "..\cxDemosBCB.inc"
#include "cxClasses.hpp"
#include "cxControls.hpp"
#include "cxEdit.hpp"
#include "cxGraphics.hpp"
#include "cxScheduler.hpp"
#include "cxSchedulerCustomControls.hpp"
#include "cxSchedulerCustomResourceView.hpp"
#include "cxSchedulerDateNavigator.hpp"
#include "cxSchedulerDayView.hpp"
#include "cxSchedulerStorage.hpp"
#include "cxStyles.hpp"
#include "DemoBasicMain.h"
#include <Buttons.hpp>
#include <Classes.hpp>
#include <ComCtrls.hpp>
#include <Controls.hpp>
#include <Dialogs.hpp>
#include <ExtCtrls.hpp>
#include <Menus.hpp>
#include <StdCtrls.hpp>
#include "cxSchedulerTimeGridView.hpp"
#include "cxSchedulerUtils.hpp"
#include "cxSchedulerWeekView.hpp"
#include "cxSchedulerYearView.hpp"
#include "cxSchedulerAgendaView.hpp"
#include <ImgList.hpp>
#include "cxSchedulerGanttView.hpp"
#include "cxSchedulerHolidays.hpp"
#include "cxButtons.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxLookAndFeels.hpp"

#ifndef UnboundDemoMainH
#define UnboundDemoMainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxControls.hpp"
#include "cxEdit.hpp"
#include "cxGraphics.hpp"
#include "cxScheduler.hpp"
#include "cxSchedulerCustomControls.hpp"
#include "cxSchedulerCustomResourceView.hpp"
#include "cxSchedulerDateNavigator.hpp"
#include "cxSchedulerDayView.hpp"
#include "cxStyles.hpp"
#include "DemoBasicMain.h"
#include <ComCtrls.hpp>
#include <Dialogs.hpp>
#include <ExtCtrls.hpp>
#include <Menus.hpp>
#include "cxSchedulerStorage.hpp"
#include <Buttons.hpp>
//---------------------------------------------------------------------------
class TUnboundDemoMainForm : public TDemoBasicMainForm
{
__published:	// IDE-managed Components
		TcxSchedulerStorage *SchedulerUnboundStorage;
		TcxButton *btnGenerate;
        TcxButton *btnDelete;
        TImageList *EventImages;
        TcxImageList *imgResources; 
        void __fastcall btnGenerate1Click(TObject *Sender);
        void __fastcall btnDelete1Click(TObject *Sender);
        void __fastcall SchedulerInitEventImages(
          TcxCustomScheduler *Sender, TcxSchedulerControlEvent *AEvent,
          TcxSchedulerEventImages *AImages);
        void __fastcall SchedulerUnboundStorageRemindersOpenEvent(
          TcxSchedulerReminders *Sender, TcxSchedulerControlEvent *AEvent);
        void __fastcall FormCreate(TObject *Sender);
private:	// User declarations
protected:
        void __fastcall OnNewEvent(TcxSchedulerEvent *AEvent, int AIndex);
public:		// User declarations
        __fastcall TUnboundDemoMainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TUnboundDemoMainForm *DemoBasicMainForm1;
//---------------------------------------------------------------------------
#endif
