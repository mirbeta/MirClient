//---------------------------------------------------------------------------
#include "..\cxDemosBCB.inc"

#include <vcl.h>
#pragma hdrstop

#include "HolidaysDemoMain.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxControls"
#pragma link "cxEdit"
#pragma link "cxGraphics"
#pragma link "cxScheduler"
#pragma link "cxSchedulerCustomControls"
#pragma link "cxSchedulerCustomResourceView"
#pragma link "cxSchedulerDateNavigator"
#pragma link "cxSchedulerDayView"
#pragma link "cxStyles"
#pragma link "DemoBasicMain"
#pragma link "cxSchedulerStorage"
#pragma link "cxControls"
#pragma link "cxEdit"
#pragma link "cxGraphics"
#pragma link "cxScheduler"
#pragma link "cxSchedulerDialogs"
#pragma link "cxSchedulerCustomControls"
#pragma link "cxSchedulerCustomResourceView"
#pragma link "cxSchedulerDateNavigator"
#pragma link "cxSchedulerDayView"
#pragma link "cxSchedulerStorage"
#pragma link "cxStyles"
#pragma link "DemoBasicMain"
#pragma link "cxControls"
#pragma link "cxEdit"
#pragma link "cxGraphics"
#pragma link "cxScheduler"
#pragma link "cxSchedulerCustomControls"
#pragma link "cxSchedulerCustomResourceView"
#pragma link "cxSchedulerDateNavigator"
#pragma link "cxSchedulerDayView"
#pragma link "cxSchedulerStorage"
#pragma link "cxStyles"
#pragma link "DemoBasicMain"
#pragma link "cxSchedulerTimeGridView"
#pragma link "cxSchedulerUtils"
#pragma link "cxSchedulerWeekView"
#pragma link "cxSchedulerYearView"
#pragma link "cxSchedulerAgendaView"
#pragma link "cxSchedulerGanttView"
#pragma link "cxSchedulerHolidays"
#pragma link "cxButtons"
#pragma link "cxLookAndFeelPainters"
#pragma resource "*.dfm"
THolidaysDemoMainForm *HolidaysDemoMainForm;
const
  int GenerateCount = 500;
  TColor Colors[] = {clRed, clYellow, clGreen, clBlue};

//---------------------------------------------------------------------------
TcxBitmap* cxCreateBitmap(TColor AColor)
{
  TcxBitmap *Result;
  TSmallPoint ADefaultSize;
  ADefaultSize.x = 13;
  ADefaultSize.y = 13;
  Result = new TcxBitmap;
  Result->Width = ADefaultSize.x;
  Result->Height = ADefaultSize.y;
  Result->cxCanvas->FillRect(Rect(0, 0, ADefaultSize.x, ADefaultSize.y), clBlack);
  Result->cxCanvas->FillRect(Rect(1, 1, ADefaultSize.x - 1, ADefaultSize.y - 1), AColor);
  return Result;
}
//---------------------------------------------------------------------------
__fastcall THolidaysDemoMainForm::THolidaysDemoMainForm(TComponent* Owner)
		: TDemoBasicMainForm(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall THolidaysDemoMainForm::btnDeleteClick(TObject *Sender)
{
  SchedulerUnboundStorage->Clear();
}
//---------------------------------------------------------------------------
void __fastcall THolidaysDemoMainForm::SchedulerInitEventImages(
	  TcxCustomScheduler *Sender, TcxSchedulerControlEvent *AEvent,
	  TcxSchedulerEventImages *AImages)
{
  Variant AValue = AEvent->GetCustomFieldValueByName("IconIndex");
  if ((VarIsNull(AValue) || VarIsEmpty(AValue)) || ((int)AValue == -1)) return;
  AImages->Add(AValue, False);
}
//---------------------------------------------------------------------------

void __fastcall THolidaysDemoMainForm::FormCreate(TObject *Sender)
{
  TDemoBasicMainForm::FormCreate(Sender);
  Scheduler->SelectDays(Date(), Date(), True);
  Holidays->RestoreFromIniFile("..\\..\\Data\\Holidays.ini");
  for (int i = 0; i < 4; i++)
	FBitmapArray[i] = cxCreateBitmap(Colors[i]);
  miRed->Bitmap = FBitmapArray[0];
  miYellow->Bitmap = FBitmapArray[1];
  miGreen->Bitmap = FBitmapArray[2];
  miBlue->Bitmap = FBitmapArray[3];
}
//---------------------------------------------------------------------------

void __fastcall THolidaysDemoMainForm::OnNewEvent(TcxSchedulerEvent *AEvent, int AIndex)
{
  AEvent->SetCustomFieldValueByName("IconIndex", AIndex);
  if ((random(2) == 1) || ((int)AEvent->Start >= (int)Now())) AEvent->Reminder = true;
}
//---------------------------------------------------------------------------

void __fastcall THolidaysDemoMainForm::SchedulerUnboundStorageRemindersOpenEvent(
  TcxSchedulerReminders *Sender, TcxSchedulerControlEvent *AEvent)
{
  Scheduler->EditEventUsingDialog(AEvent, true, false);
}

//---------------------------------------------------------------------------

void __fastcall THolidaysDemoMainForm::btnGenerateClick(TObject *Sender)
{
	TPoint APos;
	APos = btnGenerate->ClientToScreen(Point(0, 0));
	PopupMenu->Popup(APos.x, APos.y);
}
//---------------------------------------------------------------------------

void __fastcall THolidaysDemoMainForm::Forall1Click(TObject *Sender)
{
  int i;
  Variant AIDs[8];
  for (i = 0; i < SchedulerUnboundStorage->ResourceCount; i++)
	AIDs[i] = SchedulerUnboundStorage->ResourceIDs[i];
  SchedulerUnboundStorage->GenerateHolidayEvents(VarArrayOf(AIDs, 7));
}
//---------------------------------------------------------------------------

void __fastcall THolidaysDemoMainForm::OnllyFOXSPORTS11Click(TObject *Sender)
{
  Variant AID = "0";
  SchedulerUnboundStorage->GenerateHolidayEvents(AID);
}
//---------------------------------------------------------------------------

void __fastcall THolidaysDemoMainForm::FOXFOOTYandFUEL1Click(TObject *Sender)
{
  Variant AIDs[2] = {"3", "4"};
  SchedulerUnboundStorage->GenerateHolidayEvents(VarArrayOf(AIDs, 1));
}
//---------------------------------------------------------------------------

void __fastcall THolidaysDemoMainForm::btnHolidaysEditorClick(TObject *Sender)
{
  TcxSchedulerHolidays *AHolidays;
  AHolidays = Holidays;
  cxShowHolidaysEditor(AHolidays, Scheduler->LookAndFeel);
}
//---------------------------------------------------------------------------

void __fastcall THolidaysDemoMainForm::FormDestroy(TObject *Sender)
{
  int i;
  for (i = 0; i < 4; i++)
	FreeAndNil(&FBitmapArray[i]);
}
//---------------------------------------------------------------------------

void __fastcall THolidaysDemoMainForm::miColorClick(TObject *Sender)
{
  Scheduler->DateNavigator->HolidayColor = Colors[((TMenuItem *)Sender)->Tag];
}
//---------------------------------------------------------------------------

void __fastcall THolidaysDemoMainForm::miHighlightClick(TObject *Sender)
{
  Scheduler->DateNavigator->ShowDatesContainingHolidaysInColor = !Scheduler->DateNavigator->ShowDatesContainingHolidaysInColor;
  miHighlight->Checked = Scheduler->DateNavigator->ShowDatesContainingHolidaysInColor;
  miShowHolidaysHints->Enabled = miHighlight->Checked;
}
//---------------------------------------------------------------------------

void __fastcall THolidaysDemoMainForm::miShowHolidaysHintsClick(TObject *Sender)
{
  Scheduler->OptionsView->ShowHints = !Scheduler->OptionsView->ShowHints;
  miShowHolidaysHints->Checked = Scheduler->OptionsView->ShowHints;
}
//---------------------------------------------------------------------------

void __fastcall THolidaysDemoMainForm::SchedulerShowDateHint(TObject *Sender,
	  const TDateTime ADate, String &AHintText, bool &AAllow)
{
  if (AHintText == "")
	AHintText = ADate.DateTimeString();
  AAllow = true;
}
//---------------------------------------------------------------------------

