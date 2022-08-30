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
#include "cxSchedulerAgendaView.hpp"
#include "cxSchedulerDBStorage.hpp"
#include "cxSchedulerStorage.hpp"
#include "cxStyles.hpp"
#include "DemoBasicMain.h"
#include <Classes.hpp>
#include <ComCtrls.hpp>
#include <Controls.hpp>
#include <DB.hpp>
#include <Dialogs.hpp>
#include <ExtCtrls.hpp>
#include <Menus.hpp>
#include <StdCtrls.hpp>
#include "cxSchedulerTimeGridView.hpp"
#include "cxSchedulerUtils.hpp"
#include "cxSchedulerWeekView.hpp"
#include "cxSchedulerYearView.hpp"
#include "cxClasses.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxLookAndFeels.hpp"
#include "cxSchedulerGanttView.hpp"
#include "cxSchedulerHolidays.hpp"
#include "dxmdaset.hpp"

#ifndef StylesMainUnitH
#define StylesMainUnitH
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
#include "cxSchedulerDBStorage.hpp"
#include "cxSchedulerStorage.hpp"
#include <DB.hpp>
//---------------------------------------------------------------------------
class TStylesMainForm : public TDemoBasicMainForm
{
__published:	// IDE-managed Components
    TMenuItem *CustomDraw1;
    TMenuItem *DateNavigator1;
    TMenuItem *Days1;
    TMenuItem *Daycaptions1;
    TMenuItem *Monthheaders1;
    TMenuItem *ViewDay1;
    TMenuItem *imeRuler1;
    TMenuItem *Container1;
    TMenuItem *Groupseparator1;
    TMenuItem *Content1;
    TMenuItem *Headers1;
    TMenuItem *Events1;
    TMenuItem *Contentselection1;
    TMenuItem *miSplitter;
    TMenuItem *N5;
    TMenuItem *miResourcesStyle;
    TcxStyleRepository *cxStyleRepository1;
    TcxStyle *stEvents;
    TcxStyle *stHeaders;
    TcxStyle *stContent;
    TcxStyle *stContentSelection;
    TcxStyle *stResources;
    TcxStyle *stGroupSeparator;
    TcxStyle *stContainer;
    TcxStyle *stBackground;
    TcxStyle *stDateContent;
    TcxStyle *stVertSplitter;
    TcxStyle *stTimeRuler;
    TDataSource *SchedulerDataSource;
    TcxSchedulerDBStorage *SchedulerDBStorage;
	TdxMemData *mdEvents;
	TAutoIncField *mdEventsID;
	TIntegerField *mdEventsParentID;
	TIntegerField *mdEventsType;
	TDateTimeField *mdEventsStart;
	TDateTimeField *mdEventsFinish;
	TIntegerField *mdEventsOptions;
	TStringField *mdEventsCaption;
	TIntegerField *mdEventsRecurrenceIndex;
	TBlobField *mdEventsRecurrenceInfo;
	TBlobField *mdEventsResourceID;
	TStringField *mdEventsLocation;
	TStringField *mdEventsMessage;
	TDateTimeField *mdEventsReminderDate;
	TIntegerField *mdEventsReminderMinutes;
	TIntegerField *mdEventsState;
	TIntegerField *mdEventsLabelColor;
	TDateTimeField *mdEventsActualStart;
	TDateTimeField *mdEventsActualFinish;
	TStringField *mdEventsSyncIDField;
    void __fastcall miStylesItemClick(TObject *Sender);
    void __fastcall miSplitterClick(TObject *Sender);
    void __fastcall FormCreate(TObject *Sender);
private:	// User declarations
public:		// User declarations
		__fastcall TStylesMainForm(TComponent* Owner);
	    virtual TcxLookAndFeelKind __fastcall GetDefaultLookAndFeelKind();
};
//---------------------------------------------------------------------------
extern PACKAGE TStylesMainForm *StylesMainForm;
//---------------------------------------------------------------------------
#endif
