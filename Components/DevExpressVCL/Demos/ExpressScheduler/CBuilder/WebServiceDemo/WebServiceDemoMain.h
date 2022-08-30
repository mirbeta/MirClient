//---------------------------------------------------------------------------

#ifndef WebServiceDemoMainH
#define WebServiceDemoMainH
//---------------------------------------------------------------------------

#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <Classes.hpp>
#include <Controls.hpp>
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
#include "cxImageList.hpp"
#include "cxSchedulerRecurrence.hpp"
#include "cxSchedulerRibbonStyleEventEditor.hpp"
#include "cxSchedulerTreeListBrowser.hpp"
#include "dxBarBuiltInMenu.hpp"
#include "cxSchedulerWebServiceStorage.hpp"
#include "cxContainer.hpp"
#include "cxCustomData.hpp"
#include "cxDateNavigator.hpp"
#include "cxInplaceContainer.hpp"
#include "cxSplitter.hpp"
#include "cxTextEdit.hpp"
#include "cxTL.hpp"
#include <ActnList.hpp>
#include "cxContainer.hpp"
#include "cxCustomData.hpp"
#include "cxDataControllerConditionalFormattingRulesManagerDialog.hpp"
#include "cxDateNavigator.hpp"
#include "cxInplaceContainer.hpp"
#include "cxTextEdit.hpp"
#include "cxTL.hpp"
#include "cxTLdxBarBuiltInMenu.hpp"
#include "cxSplitter.hpp"
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
#include <Generics.Collections.hpp>
#include <Rtti.hpp>
#include "cxSchedulerWebServiceStorageGoogleProvider.hpp"
#include "cxSchedulerWebServiceStorageOfficeProvider.hpp"
//---------------------------------------------------------------------------
class TWebServiceDemoMainForm : public TDemoBasicMainForm
{
__published:	// IDE-managed Components
	TActionList *alMain;
	TAction *aReloadEvents;
	TcxSchedulerWebServiceStorage *Storage;
	TPanel *Panel1;
	TcxDateNavigator *cxDateNavigator1;
	TcxTreeList *tlCalendars;
	TcxTreeListColumn *tcName;
	TcxTreeListColumn *tcId;
	TcxButton *btnAddAccount;
	TcxButton *cxButton1;
	TPopupMenu *pmAddAccount;
	TMenuItem *Refresh1;
	TMenuItem *N5;
	TcxSplitter *cxSplitter1;

	void __fastcall aReloadEventsExecute(TObject *Sender);
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall tlCalendarsNodeCheckChanged(TcxCustomTreeList *Sender, TcxTreeListNode *ANode,
          TcxCheckBoxState AState);
	void __fastcall FormDestroy(TObject *Sender);
	void __fastcall FormShow(TObject *Sender);
private:	// User declarations
	TObjectList *FAuthorizationAgents;

	void __fastcall AddAccount(int AIndex);
	void __fastcall AddAccount(TcxSchedulerWebServiceStorageCustomProviderClass AProviderClass);
	TcxTreeListNode* __fastcall AddAccount(TdxCustomAuthorizationAgentClass AClass, TcxSchedulerWebServiceStorageCustomProviderClass AProviderClass);
	void __fastcall AddAccountClick(TObject *Sender);
	void __fastcall PopulateAccountNode(TcxTreeListNode *ANode, TcxSchedulerWebServiceStorageCustomProviderClass AProviderClass);

	TdxCustomAuthorizationAgentClass __fastcall GetAuthorizationAgentClass(TcxSchedulerWebServiceStorageCustomProviderClass AProviderClass);

	void __fastcall AddResource(int AAuthIndex, int ACalendarIndex);
	void __fastcall RemoveResource(int AAuthIndex, UnicodeString ACalendarId);

	void __fastcall LoadData();
	void __fastcall SaveData();
protected:
public:		// User declarations
		__fastcall TWebServiceDemoMainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TWebServiceDemoMainForm *DemoBasicMainForm1;
//---------------------------------------------------------------------------
#endif
