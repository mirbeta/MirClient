//---------------------------------------------------------------------------

#define HDC unsigned int

#ifndef CustomDrawDemoMainH
#define CustomDrawDemoMainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxClasses.hpp"
#include "cxControls.hpp"
#include "cxEdit.hpp"
#include "cxGraphics.hpp"
#include "cxScheduler.hpp"
#include "cxSchedulerStorage.hpp"
#include "cxSchedulerCustomControls.hpp"
#include "cxSchedulerCustomResourceView.hpp"
#include "cxSchedulerDateNavigator.hpp"
#include "cxSchedulerDayView.hpp"
#include "cxSchedulerAgendaView.hpp"
#include "cxStyles.hpp"
#include "DemoBasicMain.h"
#include <ComCtrls.hpp>
#include <Dialogs.hpp>
#include <ExtCtrls.hpp>
#include <Menus.hpp>
#include "cxFormats.hpp"
#include "dxOffice11.hpp"
#include "DemoBasicMain.h"
#include <ComCtrls.hpp>
#include <Dialogs.hpp>
#include <ExtCtrls.hpp>
#include <Menus.hpp>
#include "cxSchedulerStorage.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxLookAndFeels.hpp"
#include "cxSchedulerGanttView.hpp"
#include "cxSchedulerHolidays.hpp"
#include "cxSchedulerRecurrence.hpp"
#include "cxSchedulerTimeGridView.hpp"
#include "cxSchedulerUtils.hpp"
#include "cxSchedulerWeekView.hpp"
#include "cxSchedulerYearView.hpp"
//---------------------------------------------------------------------------
class TCustomDrawDemoMainForm : public TDemoBasicMainForm
{
__published:	// IDE-managed Components
        TcxSchedulerStorage *Storage;
        TMenuItem *CustomDraw1;
        TMenuItem *miEvents;
        TMenuItem *miTimeRuler;
        TMenuItem *miHeaders;
        TMenuItem *miContent;
        TMenuItem *DateNavigator1;
        TMenuItem *miMonthHeaders;
        TMenuItem *miDayCaptions;
        TMenuItem *miDays;
        TMenuItem *ViewDay1;
        TMenuItem *miContainer;
        TMenuItem *miResources;
        TMenuItem *miGroupSeparator;
        TMenuItem *miDNContent;
        TcxStyleRepository *cxStyleRepository1;
        TcxStyle *csBoldItalic;
        TcxStyle *csItalic;
        TcxStyle *csRed;
        void __fastcall FormCreate(TObject *Sender);
        void __fastcall FormDestroy(TObject *Sender);
        void __fastcall SchedulerDateNavigatorCustomDrawHeader(TObject *Sender, TcxCanvas *ACanvas,
          TcxSchedulerDateNavigatorMonthHeaderViewInfo *AViewInfo, bool &ADone);
        void __fastcall SchedulerDateNavigatorCustomDrawDayCaption(TObject *Sender, TcxCanvas *ACanvas,
          TcxSchedulerDateNavigatorDayCaptionViewInfo *AViewInfo, bool &ADone);
        void __fastcall SchedulerDateNavigatorCustomDrawDayNumber(TObject *Sender, TcxCanvas *ACanvas,
          TcxSchedulerDateNavigatorDayNumberViewInfo *AViewInfo, bool &ADone);
        void __fastcall SchedulerCustomDrawDayHeader(TObject *Sender, TcxCanvas *ACanvas,
          TcxSchedulerDayHeaderCellViewInfo *AViewInfo, bool &ADone);
        void __fastcall SchedulerViewDayCustomDrawRuler(TObject *Sender, TcxCanvas *ACanvas,
          TcxSchedulerTimeRulerCellViewInfo *AViewInfo, bool &ADone);
        void __fastcall SchedulerViewDayCustomDrawContainer(TObject *Sender, TcxCanvas *ACanvas,
          TcxSchedulerContainerCellViewInfo *AViewInfo, bool &ADone);
        void __fastcall SchedulerCustomDrawEvent(TObject *Sender, TcxCanvas *ACanvas,
          TcxSchedulerEventCellViewInfo *AViewInfo, bool &ADone);
        void __fastcall UpdateCustomDraw(TObject *Sender);
        void __fastcall SchedulerCustomDrawContent(TObject *Sender, TcxCanvas *ACanvas,
          TcxSchedulerContentCellViewInfo *AViewInfo, bool &ADone);
        void __fastcall SchedulerCustomDrawResourceHeader(TObject *Sender, TcxCanvas *ACanvas,
          TcxSchedulerHeaderCellViewInfo *AViewInfo, bool &ADone);
        void __fastcall SchedulerCustomDrawGroupSeparator(TObject *Sender, TcxCanvas *ACanvas,
          TcxSchedulerGroupSeparatorCellViewInfo *AViewInfo, bool &ADone);
        void __fastcall SchedulerDateNavigatorCustomDrawContent(TObject *Sender, TcxCanvas *ACanvas,
          TcxSchedulerDateNavigatorMonthContentViewInfo *AViewInfo, bool &ADone);
private:	// User declarations
public:		// User declarations
		__fastcall TCustomDrawDemoMainForm(TComponent* Owner);
	    virtual TcxLookAndFeelKind __fastcall GetDefaultLookAndFeelKind();
};
//---------------------------------------------------------------------------
extern PACKAGE TCustomDrawDemoMainForm *CustomDrawDemoMainForm;
//---------------------------------------------------------------------------
#endif
