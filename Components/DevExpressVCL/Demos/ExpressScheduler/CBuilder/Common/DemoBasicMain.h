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
#include "cxSchedulerAgendaView.hpp"
#include "cxStyles.hpp"
#include <Classes.hpp>
#include <ComCtrls.hpp>
#include <Controls.hpp>
#include <Dialogs.hpp>
#include <ExtCtrls.hpp>
#include <Menus.hpp>
#include <StdCtrls.hpp>
#include "cxSchedulerStorage.hpp"
#include "cxSchedulerTimeGridView.hpp"
#include "cxSchedulerUtils.hpp"
#include "cxSchedulerWeekView.hpp"
#include "cxSchedulerYearView.hpp"
#include "cxSchedulerGanttView.hpp"
#include "cxSchedulerHolidays.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxLookAndFeels.hpp"

#ifndef DemoBasicMainH
#define DemoBasicMainH
//---------------------------------------------------------------------------
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
extern int MaxRandomPeriod;

class TDemoBasicMainForm : public TForm
{
__published:	// IDE-managed Components
        TLabel *lbDescrip;
        TcxScheduler *Scheduler;
        TPanel *pnlControls;
        TMemo *Memo1;
        TStatusBar *StatusBar;
        TMainMenu *mmMain;
        TMenuItem *miFile;
        TMenuItem *Outlook1;
        TMenuItem *Exportto1;
        TMenuItem *Outlook2;
        TMenuItem *Separator2;
        TMenuItem *Excel1;
        TMenuItem *Excel2;
        TMenuItem *ext1;
        TMenuItem *Html1;
        TMenuItem *Xml1;
        TMenuItem *Separator1;
        TMenuItem *miExit;
        TMenuItem *miView;
        TMenuItem *miDay;
        TMenuItem *miWorkweek;
        TMenuItem *miWeek;
        TMenuItem *miMonth;
        TMenuItem *N3;
        TMenuItem *miGotoDate;
        TMenuItem *N2;
        TMenuItem *miViewDateNavigator;
        TMenuItem *miControlBox;
        TMenuItem *miViewposition;
        TMenuItem *miAtLeft;
        TMenuItem *miAtRight;
        TMenuItem *Resources1;
        TMenuItem *GroupBy1;
        TMenuItem *miGroupByNone;
        TMenuItem *miGroupByResources;
        TMenuItem *miGroupByDate;
        TMenuItem *Resourcelayout1;
        TMenuItem *miAbout;
        TTimer *Timer1;
        TSaveDialog *SaveDialog;
        TMenuItem *miTimeGrid1;
        TMenuItem *miYear1;
        TMenuItem *N4;
        TMenuItem *miResPerPage;
        TMenuItem *miWorktimeonly;
        TMenuItem *miAllDayContainer;
        TMenuItem *Alwaysshoweventtime1;
        TMenuItem *Displayminutesontimeruler1;
        TMenuItem *miSingleColumn;
        TMenuItem *miOneResPerPage;
        TMenuItem *miTwoResPerPage;
        TMenuItem *miThreeResPerPage;
        TMenuItem *miAllResPerPage;
        TMenuItem *miSeparator;
        TMenuItem *miTimeWorktimeonly;
        TMenuItem *Alldayeventsonly1;
        TMenuItem *miCompressweekends;
        TMenuItem *miWeekCompressWeekends;
        TMenuItem *Options1;
        TMenuItem *miTimeGrid;
        TMenuItem *miYear;
        TMenuItem *Outlooksynchronization1;
        TMenuItem *miIntersection;
        TMenuItem *miSharing;
        TMenuItem *miEventsOpt;
	TMenuItem *miWeekViewHideweekend;
	TMenuItem *miMonthViewHideweekend;
	TMenuItem *miSelectOnRightClick;
	TMenuItem *Alldayarea1;
	TMenuItem *miShowEvents;
	TMenuItem *Scrollbar1;
	TMenuItem *miAllDayScrollDefault;
	TMenuItem *miAllDayScrollNever;
	TMenuItem *miAllDayScrollAlways;
	TMenuItem *miDayHeaderArea;
	TMenuItem *Height1;
	TMenuItem *miAllDayAreaHeightDefault;
	TMenuItem *mimiAllDayAreaHeight3;
	TMenuItem *miAllDayAreaHeight5;
	TMenuItem *miAgenda;
	TMenuItem *Agenda1;
	TMenuItem *DayHeaderOrientation1;
	TMenuItem *mi_dhoHorizontal;
	TMenuItem *mi_dhoVertical;
	TMenuItem *DisplayEmptyDayMode1;
	TMenuItem *mi_dedUnlimited;
	TMenuItem *mi_dedSelected;
	TMenuItem *mi_dedHideEmptyDays;
	TMenuItem *ShowLocation1;
	TMenuItem *ShowResources1;
	TMenuItem *ShowTimeAsClock1;
	TMenuItem *miViewStyle;
	TMenuItem *miClassic;
	TMenuItem *miModern;
	TMenuItem *miDayHeaderModernDisplayMode;
	TMenuItem *miDHMDefault;
	TMenuItem *miDHMClassic;
	TMenuItem *miDHMDayAndDate;
	TcxLookAndFeelController *lfController;
	void __fastcall FileExitExecute(TObject* Sender);
	void __fastcall miViewDateNavigatorClick(TObject* Sender);
	void __fastcall ViewClick(TObject* Sender);
	void __fastcall ViewModeClick(TObject* Sender);
	void __fastcall ViewPositionClick(TObject* Sender);
	void __fastcall miAboutClick(TObject* Sender);
	void __fastcall miGotoDateClick(TObject* Sender);
	void __fastcall FormCreate(TObject* Sender);
	void __fastcall Timer1Timer(TObject* Sender);
	void __fastcall Resourcelayout1Click(TObject* Sender);
	void __fastcall miGroupByClick(TObject* Sender);
	void __fastcall ExportToClick(TObject* Sender);
	void __fastcall SyncClick(TObject *Sender);
	void __fastcall miResCountClick(TObject *Sender);
	void __fastcall miTimeWorktimeonlyClick(TObject *Sender);
	void __fastcall miDaySettingsClick(TObject *Sender);
	void __fastcall miWeekViewClick(TObject *Sender);
	void __fastcall miCompressweekendsClick(TObject *Sender);
	void __fastcall AlldayeventsonlyClick(TObject *Sender);
	void __fastcall miSharingClick(TObject *Sender);
	void __fastcall miIntersectionClick(TObject *Sender);
	void __fastcall miWeekCompressWeekendsClick(TObject *Sender);
	void __fastcall miSelectOnRightClickClick(TObject *Sender);
	void __fastcall miShowEventsClick(TObject *Sender);
	void __fastcall miAllDayScrollClick(TObject *Sender);
	void __fastcall miDayHeaderAreaClick(TObject *Sender);
	void __fastcall AllDayAreaHeightClick(TObject *Sender);
	void __fastcall miWeekViewHideweekendClick(TObject *Sender);
	void __fastcall mi_dhoHorizontalClick(TObject *Sender);
	void __fastcall mi_dedUnlimitedClick(TObject *Sender);
	void __fastcall AgendaSettingsClick(TObject *Sender);
	void __fastcall miViewStyleClick(TObject* Sender);
	void __fastcall miDayHeaderModernDisplayModeClick(TObject* Sender);
private:	// User declarations
public:		// User declarations
	TDateTime AnchorDate;
	__fastcall TDemoBasicMainForm(TComponent* Owner);
	void GenerateRandomEvents(int ACount, bool ARandomResource = false, TcxCustomSchedulerStorage *AStorage = NULL,
	TColor AColor = clDefault);
	virtual void __fastcall AddLookAndFeelMenu();
	virtual TcxLookAndFeelKind __fastcall GetDefaultLookAndFeelKind();
	virtual bool __fastcall IsNativeDefaultStyle();
	virtual void __fastcall SetDefaultLookAndFeel();
protected:
	String GetRandomCaption();
	TDateTime GetRandomDate();
	TColor GetRandomLabelColor();
	Variant GetRandomResourceID();
	int GetRandomState();
	virtual void __fastcall TDemoBasicMainForm::OnNewEvent(TcxSchedulerEvent *AEvent, int AIndex);
	virtual void __fastcall CheckSchedulerViewStyle();
	void  TDemoBasicMainForm::CreateEventObject(bool AAllDayEvent, bool ARandomResource, TcxCustomSchedulerStorage *AStorage,
	  TColor AColor);
};
//---------------------------------------------------------------------------
extern PACKAGE TDemoBasicMainForm *DemoBasicMainForm;
//---------------------------------------------------------------------------
#endif
