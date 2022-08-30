//---------------------------------------------------------------------------

#ifndef RangeControlDemoMainH
#define RangeControlDemoMainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxClasses.hpp"
#include "cxControls.hpp"
#include "cxEdit.hpp"
#include "cxGraphics.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxLookAndFeels.hpp"
#include "cxScheduler.hpp"
#include "cxSchedulerAgendaView.hpp"
#include "cxSchedulerCustomControls.hpp"
#include "cxSchedulerCustomResourceView.hpp"
#include "cxSchedulerDateNavigator.hpp"
#include "cxSchedulerDayView.hpp"
#include "cxSchedulerGanttView.hpp"
#include "cxSchedulerHolidays.hpp"
#include "cxSchedulerRecurrence.hpp"
#include "cxSchedulerStorage.hpp"
#include "cxSchedulerTimeGridView.hpp"
#include "cxSchedulerUtils.hpp"
#include "cxSchedulerWeekView.hpp"
#include "cxSchedulerYearView.hpp"
#include "cxStyles.hpp"
#include "DemoBasicMain.h"
#include <ComCtrls.hpp>
#include <Dialogs.hpp>
#include <ExtCtrls.hpp>
#include <Menus.hpp>
#include "cxCheckBox.hpp"
#include "cxContainer.hpp"
#include "cxDropDownEdit.hpp"
#include "cxGroupBox.hpp"
#include "cxLabel.hpp"
#include "cxMaskEdit.hpp"
#include "cxSchedulerRangeControlClientProperties.hpp"
#include "cxSpinEdit.hpp"
#include "cxSplitter.hpp"
#include "cxTextEdit.hpp"
#include "dxRangeControl.hpp"
//---------------------------------------------------------------------------
class TRangeControlDemoMainForm : public TDemoBasicMainForm
{
__published:	// IDE-managed Components
	TcxGroupBox *cxGroupBox1;
	TcxCheckBox *cxCheckBox1;
	TcxCheckBox *cxCheckBox2;
	TcxCheckBox *cxCheckBox3;
	TcxLabel *cxLabel1;
	TcxLabel *cxLabel2;
	TcxComboBox *cxComboBox1;
	TcxSpinEdit *cxSpinEdit1;
	TcxLabel *cxLabel3;
	TcxSpinEdit *cxSpinEdit2;
	TcxSplitter *cxSplitter1;
	TdxRangeControl *dxRangeControl1;
	TcxSchedulerStorage *SchedulerStorage;
	void __fastcall dxRangeControl1ClientPropertiesAutoAdjustRangeControlSettings(TObject *Sender,
          TcxSchedulerRangeControlAutoAdjustingInfo &AInfo);
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall cxCheckBox1PropertiesEditValueChanged(TObject *Sender);
	void __fastcall cxCheckBox2PropertiesEditValueChanged(TObject *Sender);
	void __fastcall cxCheckBox3PropertiesEditValueChanged(TObject *Sender);
	void __fastcall cxComboBox1PropertiesEditValueChanged(TObject *Sender);
	void __fastcall cxSpinEdit1PropertiesEditValueChanged(TObject *Sender);
	void __fastcall cxSpinEdit2PropertiesEditValueChanged(TObject *Sender);
private:	// User declarations
public:		// User declarations
	__fastcall TRangeControlDemoMainForm(TComponent* Owner);
	TcxSchedulerRangeControlClientProperties* __fastcall RangeControlProperties();
};
//---------------------------------------------------------------------------
extern PACKAGE TRangeControlDemoMainForm *RangeControlDemoMainForm;
//---------------------------------------------------------------------------
#endif
