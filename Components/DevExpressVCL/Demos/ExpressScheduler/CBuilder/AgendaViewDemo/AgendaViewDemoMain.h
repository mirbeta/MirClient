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
#include "cxSchedulerAgendaView.hpp"
#include "cxSchedulerHolidays.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxLookAndFeels.hpp"

#ifndef AgendaViewDemoMainH
#define AgendaViewDemoMainH
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
class TAgendaViewDemoMainForm : public TDemoBasicMainForm
{
	__published:	// IDE-managed Components
		TcxSchedulerStorage *SchedulerStorage;
		TMenuItem *AgendaOptions1;
		TMenuItem *DayHeaderOrientation2;
		TMenuItem *Horizontal1;
		TMenuItem *Vertical1;
		TMenuItem *DisplayMode1;
		TMenuItem *AllDays1;
		TMenuItem *SelectedDays1;
		TMenuItem *SelectedNonEmptyDays1;
		TMenuItem *Showlocation2;    
		TMenuItem *Showresources2;
		TMenuItem *Showtimeasclock2;
		void __fastcall Horizontal1Click(TObject *Sender);
	private:	// User declarations

	protected:

	public:		// User declarations
        __fastcall TAgendaViewDemoMainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TAgendaViewDemoMainForm *DemoBasicMainForm1;
//---------------------------------------------------------------------------
#endif
