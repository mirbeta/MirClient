//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "RangeControlDemoMain.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxClasses"
#pragma link "cxControls"
#pragma link "cxEdit"
#pragma link "cxGraphics"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxLookAndFeels"
#pragma link "cxScheduler"
#pragma link "cxSchedulerAgendaView"
#pragma link "cxSchedulerCustomControls"
#pragma link "cxSchedulerCustomResourceView"
#pragma link "cxSchedulerDateNavigator"
#pragma link "cxSchedulerDayView"
#pragma link "cxSchedulerGanttView"
#pragma link "cxSchedulerHolidays"
#pragma link "cxSchedulerRecurrence"
#pragma link "cxSchedulerStorage"
#pragma link "cxSchedulerTimeGridView"
#pragma link "cxSchedulerUtils"
#pragma link "cxSchedulerWeekView"
#pragma link "cxSchedulerYearView"
#pragma link "cxStyles"
#pragma link "DemoBasicMain"
#pragma link "cxCheckBox"
#pragma link "cxContainer"
#pragma link "cxDropDownEdit"
#pragma link "cxGroupBox"
#pragma link "cxLabel"
#pragma link "cxMaskEdit"
#pragma link "cxSchedulerRangeControlClientProperties"
#pragma link "cxSpinEdit"
#pragma link "cxSplitter"
#pragma link "cxTextEdit"
#pragma link "dxRangeControl"
#pragma resource "*.dfm"
TRangeControlDemoMainForm *RangeControlDemoMainForm;
//---------------------------------------------------------------------------
__fastcall TRangeControlDemoMainForm::TRangeControlDemoMainForm(TComponent* Owner)
	: TDemoBasicMainForm(Owner)
{
}
//---------------------------------------------------------------------------
TcxSchedulerRangeControlClientProperties* __fastcall TRangeControlDemoMainForm::RangeControlProperties()
{
  TcxSchedulerRangeControlClientProperties* A = dynamic_cast<TcxSchedulerRangeControlClientProperties*>(dxRangeControl1->ClientProperties);
  return A;
}
//---------------------------------------------------------------------------
void __fastcall TRangeControlDemoMainForm::dxRangeControl1ClientPropertiesAutoAdjustRangeControlSettings(TObject *Sender,
          TcxSchedulerRangeControlAutoAdjustingInfo &AInfo)
{
//
}
//---------------------------------------------------------------------------

void __fastcall TRangeControlDemoMainForm::FormCreate(TObject *Sender)
{
  Randomize();
  dxRangeControl1->BeginUpdate();
  GenerateRandomEvents(1000, false, SchedulerStorage, EventLabelColors[0]);
  dxRangeControl1->VisibleRangeMinValue = Date() - 10;
  dxRangeControl1->VisibleRangeMaxValue = Date() + 10;
  dxRangeControl1->EndUpdate();
}
//---------------------------------------------------------------------------

void __fastcall TRangeControlDemoMainForm::cxCheckBox1PropertiesEditValueChanged(TObject *Sender)

{
  if (cxCheckBox1->Checked)
	RangeControlProperties()->AutoAdjustments = RangeControlProperties()->AutoAdjustments << aaClient;
  else
	RangeControlProperties()->AutoAdjustments = RangeControlProperties()->AutoAdjustments >> aaClient;
}
//---------------------------------------------------------------------------


void __fastcall TRangeControlDemoMainForm::cxCheckBox2PropertiesEditValueChanged(TObject *Sender)

{
  RangeControlProperties()->AutoFormatScaleCaptions = cxCheckBox2->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TRangeControlDemoMainForm::cxCheckBox3PropertiesEditValueChanged(TObject *Sender)

{
  if (cxCheckBox3->Checked)
	RangeControlProperties()->AutoAdjustments = RangeControlProperties()->AutoAdjustments << aaRangeControl;
  else
	RangeControlProperties()->AutoAdjustments = RangeControlProperties()->AutoAdjustments >> aaRangeControl;
}
//---------------------------------------------------------------------------

void __fastcall TRangeControlDemoMainForm::cxComboBox1PropertiesEditValueChanged(TObject *Sender)

{
  RangeControlProperties()->DataDisplayType = TcxSchedulerRangeControlDataDisplayType(cxComboBox1->ItemIndex);
}
//---------------------------------------------------------------------------


void __fastcall TRangeControlDemoMainForm::cxSpinEdit1PropertiesEditValueChanged(TObject *Sender)

{
  RangeControlProperties()->ThumbnailHeight = cxSpinEdit1->EditingValue;
}
//---------------------------------------------------------------------------

void __fastcall TRangeControlDemoMainForm::cxSpinEdit2PropertiesEditValueChanged(TObject *Sender)

{
  RangeControlProperties()->ScaleIntervalMinWidth = cxSpinEdit2->EditingValue;
}
//---------------------------------------------------------------------------


