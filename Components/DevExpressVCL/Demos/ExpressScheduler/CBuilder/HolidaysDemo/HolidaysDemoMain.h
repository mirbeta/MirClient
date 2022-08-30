//---------------------------------------------------------------------------
#include "..\cxDemosBCB.inc"
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
#include "cxSchedulerDialogs.hpp"
#include "cxLookAndFeelPainters.hpp"

#ifndef HolidaysDemoMainH
#define HolidaysDemoMainH
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
class THolidaysDemoMainForm : public TDemoBasicMainForm
{
__published:	// IDE-managed Components
        TcxSchedulerStorage *SchedulerUnboundStorage;
		TcxSchedulerHolidays *Holidays;
		TMenuItem *Holidays1;
		TMenuItem *miShowHolidaysHints;
		TMenuItem *miHighlightHolidays;
		TMenuItem *miHolidaySeparator1;
		TMenuItem *miHighlight;
		TMenuItem *miRed;
		TMenuItem *miYellow;
		TMenuItem *miGreen;
		TMenuItem *miBlue;
		TPopupMenu *PopupMenu;
		TMenuItem *Forall1;
		TMenuItem *OnllyFOXSPORTS11;
		TMenuItem *FOXFOOTYandFUEL1;
		TcxButton *btnGenerate;
		TcxButton *btnHolidaysEditor;
		TImageList *EventImages;
                TcxImageList *imgResources; 
		void __fastcall btnDeleteClick(TObject *Sender);
		void __fastcall SchedulerInitEventImages(
		  TcxCustomScheduler *Sender, TcxSchedulerControlEvent *AEvent,
		  TcxSchedulerEventImages *AImages);
		void __fastcall SchedulerUnboundStorageRemindersOpenEvent(
		  TcxSchedulerReminders *Sender, TcxSchedulerControlEvent *AEvent);
		void __fastcall FormCreate(TObject *Sender);
	void __fastcall btnGenerateClick(TObject *Sender);
	void __fastcall Forall1Click(TObject *Sender);
	void __fastcall OnllyFOXSPORTS11Click(TObject *Sender);
	void __fastcall FOXFOOTYandFUEL1Click(TObject *Sender);
	void __fastcall btnHolidaysEditorClick(TObject *Sender);
	void __fastcall FormDestroy(TObject *Sender);
	void __fastcall miColorClick(TObject *Sender);
	void __fastcall miHighlightClick(TObject *Sender);
	void __fastcall miShowHolidaysHintsClick(TObject *Sender);
	void __fastcall SchedulerShowDateHint(TObject *Sender, const TDateTime ADate,
          String &AHintText, bool &AAllow);
private:	// User declarations
	TcxBitmap *FBitmapArray[4];
protected:
        void __fastcall OnNewEvent(TcxSchedulerEvent *AEvent, int AIndex);
public:		// User declarations
        __fastcall THolidaysDemoMainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE THolidaysDemoMainForm *HolidaysDemoMainForm1;
//---------------------------------------------------------------------------
#endif
