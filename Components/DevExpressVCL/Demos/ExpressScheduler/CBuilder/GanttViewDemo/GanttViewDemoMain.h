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
#include "cxLookAndFeelPainters.hpp"
#include "cxLookAndFeels.hpp"

#ifndef GanttViewDemoMainH
#define GanttViewDemoMainH
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
class TGanttViewDemoMainForm : public TDemoBasicMainForm
{
	__published:	// IDE-managed Components
		TcxSchedulerStorage *SchedulerStorage;
		TMenuItem *askOptions1;
		TMenuItem *miHotTrack;
		TMenuItem *miShowAsProgress;
		TMenuItem *miShowTotalProgress;
		TMenuItem *miShowExpandButtons;
		TMenuItem *N5;    
		TMenuItem *N6;
		void __fastcall miHotTrackClick(TObject *Sender);
		void __fastcall miShowAsProgressClick(TObject *Sender);
		void __fastcall miShowTotalProgressClick(TObject *Sender);
		void __fastcall miShowExpandButtonsClick(TObject *Sender);
	void __fastcall SchedulerViewGanttGetMinorUnitDisplayText(TcxSchedulerTimeGridView *Sender,
          const TDateTime AStart, const TDateTime AFinish,
          TcxSchedulerTimeGridScaleTextType ATextType, String &AText);

	private:	// User declarations

	protected:

	public:		// User declarations
        __fastcall TGanttViewDemoMainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TGanttViewDemoMainForm *DemoBasicMainForm1;
//---------------------------------------------------------------------------
#endif
