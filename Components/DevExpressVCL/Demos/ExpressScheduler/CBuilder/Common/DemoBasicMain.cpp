//---------------------------------------------------------------------------
#include "..\cxDemosBCB.inc"

#include <vcl.h>
#pragma hdrstop

#include "cxDemosBCB.inc"
#include "DemoBasicMain.h"
#include "SysUtils.hpp"
#include "cxSchedulerUtils.hpp"
#include "cxSchedulerDialogs.hpp"
#include "cxExportSchedulerLink.hpp"
#include "cxSchedulerOutlookExchange.hpp"

#include "AboutDemoForm.h"
#include "DemoUtils.h"

//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxControls"
#pragma link "cxClasses"
#pragma link "cxLookAndFeels"
#pragma link "cxGraphics"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxEdit"
#pragma link "cxScheduler"
#pragma link "cxSchedulerCustomControls"
#pragma link "cxSchedulerCustomResourceView"
#pragma link "cxSchedulerDateNavigator"
#pragma link "cxSchedulerDayView"
#pragma link "cxSchedulerStorage"
#pragma link "cxStyles"
#pragma link "cxScheduler"
#pragma link "cxSchedulerCustomControls"
#pragma link "cxSchedulerCustomResourceView"
#pragma link "cxSchedulerDateNavigator"
#pragma link "cxSchedulerOutlookExchange"
#pragma link "cxSchedulerDayView"
#pragma link "cxSchedulerAgendaView"
#pragma link "cxStyles"
#pragma link "cxSchedulerStorage"
#pragma link "cxSchedulerTimeGridView"
#pragma link "cxSchedulerUtils"
#pragma link "cxSchedulerWeekView"
#pragma link "cxSchedulerYearView"
#pragma link "cxSchedulerGanttView"
#pragma link "cxSchedulerHolidays"
#pragma link "cxSchedulerGanttView"
#pragma link "cxSchedulerHolidays"
#pragma resource "*.dfm"
int  MaxRandomPeriod = 60;

TDemoBasicMainForm *DemoBasicMainForm;

const
  int StartIndexes[22] =
   {0, 12, 30, 44, 58, 80, 90, 106, 126, 157, 174, 196,
    208, 220, 226, 234, 238, 242, 249, 260, 267, 272};

  int EventsCount[22] =
   {12, 18, 14, 14, 22, 10, 16, 20, 31, 17,
    22, 12, 12, 6, 8, 4, 4, 7, 11, 7, 5, 8};

  String SportEvents[280]  =
   {"Basketball Qualifying - Men",
    "Basketball Qualifying - Women",
    "Basketball First Group Phase - Men",
    "Basketball First Group Phase - Women",
    "Basketball Quarterfinals - Men",
    "Basketball Quarterfinals - Women",
    "Basketball Semifinals - Men",
    "Basketball Semifinals - Women",
    "Basketball Places 3/4 - Men",
    "Basketball Places 3/4 - Women",
    "Basketball Finals - Men",
    "Basketball Finals - Women",

    "Lamon Brewster (32-2) vs. Luan Krasniqi (28-1-1) (WBO heavyweight belt)",
    "(PPV) Antonio Tarver (23-3) vs. Roy Jones (49-3) (IBO light heavyweight belt)",
    "(Showtime) James Toney (68-4-2) vs. Dominick Guinn (25-2-1)",
    "Nicolay Valuev (41-0) vs. TBA",
    "Danilo Haussler (25-3) vs. TBA",
    "Cengiz Koc (22-1) vs. TBA",
    "(PPV) Diego Corrales (40-2) vs. Jose Luis Castillo (52-7-1)",
    "(WBC and WBO lightweight belts) (PPV) Carlos Hernandez vs. Bobby Pacquiao",
    "(PPV) Jorge Arce vs. Hussein Hussein",
    "Vince Philips (47-9-1) vs. Reynaldo Pelonia (35-21-3)",
    "Kili Madrid (6-0-1) vs. Donny Fosmire (10-7)",
    "Nelson Zepeda (1-0-1) vs. Kaleo Padilla (0-0)",
    "Justin Mercado (1-1) vs. Waldo Rojas (0-0)",
    "Illima Vicente (0-0) vs. Jenny Houts (0-0)",
    "Tomasz Adamek (29-0) vs. Thomas Ulrich (28-1) (WBC light heavyweight belt)",
    "(Showtime) Jeff Lacy (20-0) vs. Joe Calzaghe (39-0) (IBF, IBO and WBO super middleweight belts)",
    "Jermain Taylor vs. Bernard Hopkins",
    "Ronald ""Winky"" Wright vs. TBA",

    "Australian Open",
    "Pacific Life Open",
    "NASDAQ-100 Open",
    "Tennis Masters Monte Carlo",
    "Telecom Italia Masters Roma",
    "Tennis Masters Hamburg",
    "Roland Garros",
    "Wimbledon",
    "Tennis Masters Montreal",
    "Western and Southern Financial Group Masters",
    "US Open",
    "Tennis Masters Madrid",
    "BNP Paribas Masters",
    "Tennis Masters Cup",

    "Weightlifting 48 Kg - Women  - Final",
    "Weightlifting 56 Kg - Men  - Final",
    "Weightlifting 53 Kg - Women  - Final",
    "Weightlifting 62 Kg - Men  - Final",
    "Weightlifting 63 Kg - Women  - Final",
    "Weightlifting 69 Kg - Men  - Final",
    "Weightlifting 69 Kg - Women  - Final",
    "Weightlifting 77 Kg - Men  - Final",
    "Weightlifting 75 Kg - Women  - Final",
    "Weightlifting 75kg - Women  - Final",
    "Weightlifting 85 Kg - Men  - Final",
    "Weightlifting 94 Kg - Men  - Final",
    "Weightlifting 105 Kg - Men  - Final",
    "Weightlifting 105kg - Men  - Final",

    "Sabre - Men - 1st Round",
    "Sabre - Men - Round 2",
    "Sabre - Men - 3rd Round",
    "Sabre - Men - Quarter final",
    "Sabre - Men - Semi-finals",
    "Sabre - Men - Final",
    "Epee - Women - 1st Round",
    "Epee - Women - Round 2",
    "Epee - Women - 3rd Round",
    "Epee - Women - Quarter final",
    "Epee - Women - Semi-finals",
    "Epee - Women - Final",
    "Epee - Women/Team - 1st Round",
    "Epee - Women/Team - Quarter final",
    "Epee - Women/Team - Semi-finals",
    "Epee - Women/Team - Final",
    "Foil - Men - 1st Round",
    "Foil - Men - Round 2",
    "Foil - Men - 3rd Round",
    "Foil - Men - Quarter final",
    "Foil - Men - Semi-finals",
    "Foil - Men - Final",

    "Soccer 1st Round - *Men's Preliminaries - Men",
    "Soccer 1st Round - *Women's Preliminaries - Women",
    "Soccer Quarter final - *Men's Quarterfinal - Men",
    "Soccer Quarter final - *Women's Quarterfinal - Women",
    "Soccer Semi-finals - *Women's Semifinal - Men",
    "Soccer Semi-finals - *Women's Semifinal - Women",
    "Soccer places 3/4 - *Men's Bronze Medal Match - Men",
    "Soccer places 3/4 - *Women's Bronze Medal Match - Women",
    "Soccer Final - *Men's Gold Medal Match - Men",
    "Soccer Final - *Women's Gold Medal Match - Women",

    "Men - Qualifying",
    "Women - Qualifying",
    "Men/Team - Final",
    "Women/Team - Final",
    "Individual All-Around - Men - Final",
    "Individual All-Around - Women - Final",
    "Floor Exercise - Men - Final",
    "Vault - Women - Final",
    "Uneven Bars - Women - Final",
    "Pommel Horse - Men - Final",
    "Rings - Men - Final",
    "Vault - Men - Final",
    "Beam - Women - Final",
    "Parallel Bars - Men - Final",
    "Floor Exercise - Women - Final",
    "Horizontal Bar - Men - Final",

    "Slalom C1 - Men - Heats",
    "Slalom C1 - Men - Heats",
    "Slalom C1 - Men - Semi-finals",
    "Slalom C1 - Men - Final",
    "Slalom C2 - Men - Heats",
    "Slalom C2 - Men - Heats",
    "Slalom C2 - Men - Semi-finals",
    "Slalom C2 - Men - Final",
    "Flatwater C1 - 1000m - Men - Heats",
    "Flatwater C2 - 1000m - Men - Heats",
    "Flatwater C1 - 500m - Men - Heats",
    "Flatwater C2 - 500m - Men - Heats",
    "Flatwater C1 - 1000m - Men - Semi-finals",
    "Flatwater C2 - 1000m - Men - Semi-finals",
    "Flatwater C1 - 500m - Men - Semi-finals",
    "Flatwater C2 - 500m - Men - Semi-finals",
    "Flatwater C1 - 1000m - Men - Final",
    "Flatwater C2 - 1000m - Men - Final",
    "Flatwater C1 - 500m - Men - Final",
    "Flatwater C2 - 500m - Men - Final",

    "Slalom K1 - Women - Heats",
    "Slalom K1 - Women - Heats",
    "Slalom K1 - Women - Semi-finals",
    "Slalom K1 - Women - Final",
    "Slalom K2 - Men - Heats",
    "Slalom K2 - Men - Heats",
    "Slalom K2 - Men - Semi-finals",
    "Slalom K1 - Men - Final",
    "Flatwater K1 - 1000m - Men - Heats",
    "Flatwater K4 - 500m - Women - Heats",
    "Flatwater K2 - 1000m - Men - Heats",
    "Flatwater K4 - 1000m - Men - Heats",
    "Flatwater K1 - 500m - Men - Heats",
    "Flatwater K2 - 500m - Men - Heats",
    "Flatwater K2 - 500m - Women - Heats",
    "Flatwater K1 - 1000m - Men - Semi-finals",
    "Flatwater K4 - 500m - Women - Semi-finals",
    "Flatwater K2 - 1000m - Men - Semi-finals",
    "Flatwater K4 - 1000m - Men - Semi-finals",
    "Flatwater K1 - 500m - Men - Semi-finals",
    "Flatwater K1 - 500m - Women - Semi-finals",
    "Flatwater K2 - 500m - Men - Semi-finals",
    "Flatwater K2 - 500m - Women - Semi-finals",
    "Flatwater K1 - 1000m - Men - Final",
    "Flatwater K4 - 500m - Women - Final",
    "Flatwater K2 - 1000m - Men - Final",
    "Flatwater K4 - 1000m - Men - Final",
    "Flatwater K1 - 500m - Men - Final",
    "Flatwater K1 - 500m - Women - Final",
    "Flatwater K2 - 500m - Men - Final",
    "Flatwater K2 - 500m - Women - Final",

    "Greco-Roman 55kg - Men - Qualifying",
    "Greco-Roman 66kg - Men - Qualifying",
    "Greco-Roman 84kg - Men - Qualifying",
    "Greco-Roman 120kg - Men - Qualifying",
    "Greco-Roman 55kg - Men - Semi-finals",
    "Greco-Roman 66kg - Men - Semi-finals",
    "Greco-Roman 84kg - Men - Semi-finals",
    "Greco-Roman 120kg - Men - Semi-finals",
    "Greco-Roman 96kg - Men - Qualifying",
    "Greco-Roman 55kg - Men - Final",
    "Greco-Roman 66kg - Men - Final",
    "Greco-Roman 84kg - Men - Final",
    "Greco-Roman 120kg - Men - Final",
    "Greco-Roman 55kg - Men - Play Off",
    "Greco-Roman 66kg - Men - Play Off",
    "Greco-Roman 84kg - Men - Play Off",
    "Greco-Roman 120kg - Men - Play Off",

    "Individual Eventing Dressage - 1st Day",
    "Team Eventing Dressage - 1st Day",
    "Individual Eventing Dressage - 2nd Day",
    "Team Eventing Dressage - 2nd Day",
    "Individual Eventing Cross Country - Final",
    "Team Eventing Cross Country - Final",
    "Team Eventing Jumping - Final",
    "Individual Eventing Jumping - Qualifying",
    "Individual Eventing Jumping - Final",
    "Individual Dressage Grand Prix - 1st Day",
    "Team Dressage Grand Prix - 1st Day",
    "Individual Dressage Grand Prix - 2nd Day",
    "Team Dressage Grand Prix - 2nd Day",
    "Individual Jumping - Qualifying",
    "Individual Dressage Grand Prix Special - Final",
    "Team Jumping - Final",
    "Individual Jumping - Qualifying",
    "Individual Jumping - Qualifying",
    "Team Jumping - Final",
    "Individual Dressage Grand Prix Freestyle - Final",
    "Individual Jumping - Final",
    "Individual Jumping - Final",

    "Men's 470 - Race 01",
    "Women's 470 - Race 01",
    "Men's 470 - Race 02",
    "Women's 470 - Race 02",
    "Finn - Race 1",
    "Yngling - Race 1",
    "Finn - Race 2",
    "Yngling - Race 2",
    "Laser - Race 1",
    "Women's Mistral - Race 01",
    "Men's Mistral - Race 02",
    "49er - Race 1",

    "Men's 400m Individual Medley - Heat 1",
    "Men's 400m Individual Medley - Heat 2",
    "Women's 100m Butterfly - Heat 1",
    "Men's 400m Freestyle - Heat 1",
    "Women's 400m Individual Medley - Heat 1",
    "Women's 400m Individual Medley - Heat 2",
    "Men's 100m Breaststroke - Heat 1",
    "Men's 100m Breaststroke - Heat 2",
    "Women's 4 x 100m Freestyle Relay - Heat 1",
    "Women's 4 x 100m Freestyle Relay - Heat 2",
    "Women's 100m Butterfly Semifinal 1",
    "Women's 4 x 100m Freestyle Relay Final",

    "Women's Synchronised 3m Springboard Final",
    "Men's Synchronised 3m Springboard Final",
    "Women's Synchronised 10m Platform Final",
    "Men's Synchronised 10m Platform Final",
    "Women's 10m Platform Preliminary",
    "Men's 10m Platform Semifinal",

    "Men's Preliminaries - Pool A Match 1 - Spain - Korea",
    "Women's Preliminaries - Pool A Match 1 - China - Hungary",
    "Men's Classification 11-12 Match 31 - Slovenia - Egypt",
    "Women's Classification 9-10 Match 21 - Greece - Angola",
    "Men's Classification 9-10 Match 32 - Brazil - Iceland",
    "Women's Quarterfinal Match 22 - Ukraine - Spain",
    "Men's Semifinal Match 40 - Germany - Russia",
    "Women's Semifinal Match 27 - France - Korea",

    "Men - Qualifying",
    "Women - Qualifying",
    "Men/Team - Final",
    "Women/Team - Final",

    "Women's 100m Round 1 - Heat 1",
    "Men's 100m Round 1 - Heat 1",
    "Men's 100m Semifinal 1",
    "Women's 100m Final",

    "Men's 10m Air Pistol Qualification",
    "Men's 10m Air Pistol Final",
    "Women's 10m Air Pistol Pre-event Training",
    "Men's 10m Air Pistol Medal Ceremony",
    "Women's 10m Air Pistol Qualification",
    "Men's 50m Pistol Qualification",
    "Women's 25m Pistol Final",

    "Women's Individual 1/32 Eliminations",
    "Men's Individual 1/32 Eliminations",
    "Women's Individual 1/16 Eliminations",
    "Men's Individual 1/16 Eliminations",
    "Women's Individual 1/8 Eliminations",
    "Men's Individual 1/8 Eliminations",
    "Women's Individual Quarterfinal 1",
    "Men's Individual Quarterfinal 1",
    "Men's Individual Semifinal 1",
    "Men's Individual Bronze Medal Match",
    "Women's Team Gold Medal Match",

    "Men's Road Race",
    "Women's Road Race",
    "Women's Individual Time Trial",
    "Men's Individual Time Trial",
    "Women's Sprint 1/8 Finals",
    "Women's Individual Pursuit Final",
    "Men's Sprint 1/8 Finals",

    "Men's Preliminaries - Group B - EGY - AUS",
    "Women's Classification 7th-8th - KAZ - CAN",
    "Women's Quarterfinal 02 - ITA - HUN",
    "Women Bronze Medal Game",
    "Men's Semifinal 02",

    "Women's Preliminaries - Pool B Match 1 - CUB - GER",
    "Men's Preliminaries - Pool A Match 1 - SCG - POL",
    "Women's Quarterfinal 04 - JPN - CHN",
    "Men's Quarterfinal 03 - GRE - USA",
    "Women's Semifinal 02 - CUB - CHN",
    "Men's Semifinal 02 - USA - BRA",
    "Women's Gold Medal Match - RUS - CHN",
    "Men's Bronze Medal Match - RUS - USA"};


//---------------------------------------------------------------------------
__fastcall TDemoBasicMainForm::TDemoBasicMainForm(TComponent* Owner)
        : TForm(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TDemoBasicMainForm::FileExitExecute(TObject* Sender)
{
  Close();
}
//---------------------------------------------------------------------------


void __fastcall TDemoBasicMainForm::miViewDateNavigatorClick(TObject* Sender)
{
  Scheduler->DateNavigator->Visible = ((TMenuItem*)Sender)->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TDemoBasicMainForm::ViewClick(TObject* Sender)
{
 ((TMenuItem*)Sender)->Checked = !((TMenuItem*)Sender)->Checked;
  switch (((TMenuItem*)Sender)->Tag) {
    case 0:
      Scheduler->DateNavigator->Visible = ((TMenuItem*)Sender)->Checked;
      break;
    case 1:
      Scheduler->ControlBox->Visible = ((TMenuItem*)Sender)->Checked;
  }
}
//---------------------------------------------------------------------------

void __fastcall TDemoBasicMainForm::ViewModeClick(TObject* Sender)
{
  if (Scheduler->SelectedDays->Count > 0) {
  AnchorDate = Scheduler->SelectedDays->Items[0];}
  Scheduler->SelectDays(AnchorDate, AnchorDate,
   ((TMenuItem*)Sender)->Tag == 0 || ((TMenuItem*)Sender)->Tag == 1 ||
   ((TMenuItem*)Sender)->Tag == 4 || ((TMenuItem*)Sender)->Tag == 5);
  switch (((TMenuItem*)Sender)->Tag) {
    case 0:
      Scheduler->ViewDay->Active = True;
      break;
    case 1:
      Scheduler->SelectWorkDays(Date());
      break;
    case 2:
      Scheduler->ViewWeek->Active = True;
      break;
    case 3:
      Scheduler->GoToDate(Scheduler->SelectedDays->Items[0], vmMonth);
      break;
    case 4:
      Scheduler->ViewTimeGrid->Active = True;
      break;
    case 5:
      Scheduler->ViewYear->Active = True;
      break;
    case 6:
      Scheduler->ViewAgenda->Active = True;
      break;
  }
  ((TMenuItem*)Sender)->Checked = True;
}
//---------------------------------------------------------------------------

void __fastcall TDemoBasicMainForm::ViewPositionClick(TObject* Sender)
{
  ((TMenuItem*)Sender)->Checked = !((TMenuItem*)Sender)->Checked;
  switch (((TMenuItem*)Sender)->Tag) {
    case 0:
       Scheduler->OptionsView->ViewPosition = vpLeft;
       break;
    case 1:
       Scheduler->OptionsView->ViewPosition = vpRight;
  }
}
//---------------------------------------------------------------------------

void __fastcall TDemoBasicMainForm::miViewStyleClick(TObject* Sender)
{
  ((TMenuItem*)Sender)->Checked = true;
  CheckSchedulerViewStyle();
}
//---------------------------------------------------------------------------

void __fastcall TDemoBasicMainForm::miDayHeaderModernDisplayModeClick(TObject* Sender)
{
  ((TMenuItem*)Sender)->Checked = true;
  Scheduler->ViewDay->DayHeaderModernStyleDisplayMode = (TcxSchedulerDayHeaderModernStyleDisplayMode)(((TMenuItem*)Sender)->Tag);
}
//---------------------------------------------------------------------------

void __fastcall TDemoBasicMainForm::miAboutClick(TObject* Sender)
{
  ShowAboutDemoForm();
}
//---------------------------------------------------------------------------

void __fastcall TDemoBasicMainForm::miGotoDateClick(TObject* Sender)
{
  TDateTime ADate;
  TcxSchedulerViewMode AMode;

  ADate = Scheduler->SelectedDays->Items[0];
  if (cxShowGoToDateDialog(Scheduler, Scheduler->LookAndFeel, ADate, AMode))
    Scheduler->GoToDate(ADate, AMode);
}
//---------------------------------------------------------------------------

void __fastcall TDemoBasicMainForm::FormCreate(TObject* Sender)
{
  SetDefaultLookAndFeel();
  AddLookAndFeelMenu();
  AnchorDate = Date();
  if (Scheduler->Storage != NULL)
    Resources1->Visible = Scheduler->Storage->ResourceCount > 0;
  miSharing->Visible = Resources1->Visible;
  Scheduler->SelectDays(Date() - 1, Date() + 1, True);
  Height = 550;
  Width = 700;
  CheckSchedulerViewStyle();
  WindowState = wsMaximized;
}

void __fastcall TDemoBasicMainForm::AddLookAndFeelMenu()
{
  mmMain->Items->Insert(mmMain->Items->IndexOf(miAbout),
	CreateLookAndFeelMenuItems(mmMain->Items, lfController));
}

void __fastcall TDemoBasicMainForm::CheckSchedulerViewStyle()
{
  if (miModern->Checked)
    Scheduler->OptionsView->Style = svsModern;
  else
    Scheduler->OptionsView->Style = svsClassic;
}

TcxLookAndFeelKind __fastcall TDemoBasicMainForm::GetDefaultLookAndFeelKind()
{
  return(lfOffice11);
}

bool __fastcall TDemoBasicMainForm::IsNativeDefaultStyle()
{
  return(false);
}

void __fastcall TDemoBasicMainForm::SetDefaultLookAndFeel()
{
  lfController->NativeStyle = IsNativeDefaultStyle();
  lfController->Kind = GetDefaultLookAndFeelKind();
}

//---------------------------------------------------------------------------

int LastCount;

void __fastcall TDemoBasicMainForm::Timer1Timer(TObject* Sender)
{
  int ACount;

  if (Scheduler->Storage != NULL)
    ACount = Scheduler->Storage->EventCount;
  else ACount = 0;
  if (LastCount != ACount) {
    LastCount = ACount;
    StatusBar->SimpleText = "Data Storage contains " + IntToStr(ACount) + " events";
  }
}
//---------------------------------------------------------------------------

void __fastcall TDemoBasicMainForm::Resourcelayout1Click(TObject* Sender)
{
  if (Scheduler->Storage != NULL)
    cxShowResourcesLayoutEditor(Scheduler->Storage, Scheduler->LookAndFeel);
}
//---------------------------------------------------------------------------

void __fastcall TDemoBasicMainForm::miGroupByClick(TObject* Sender)
{
  ((TMenuItem*)Sender)->Checked = !((TMenuItem*)Sender)->Checked;
  switch (((TMenuItem*)Sender)->Tag)
  {
    case 0:
        Scheduler->OptionsView->GroupingKind = gkNone;
        break;
    case 1:
        Scheduler->OptionsView->GroupingKind = gkByResource;
        break;
    case 2:
        Scheduler->OptionsView->GroupingKind = gkByDate;
  }
}
//---------------------------------------------------------------------------

void __fastcall TDemoBasicMainForm::ExportToClick(TObject* Sender)
{
  switch (((TMenuItem*)Sender)->Tag) {
    case 0:
      cxSchedulerExportToOutlook(Scheduler->Storage, NULL, NULL, NULL);
      break;
    case 1:
    case 2:
    case 3:
    case 4:
    case 5:
      if (SaveDialog->Execute()) {
        switch (((TMenuItem*)Sender)->Tag) {
          case 1:
            cxExportSchedulerToExcel(SaveDialog->FileName, Scheduler, false, false,
              "Event # %d", (TDateTime)NullDate, (TDateTime)NullDate, "xls");
            break;
          case 2:
            cxExportSchedulerToText(SaveDialog->FileName, Scheduler, false, false,
              "Event # %d", (TDateTime)NullDate, (TDateTime)NullDate, "", "", "", "txt");
            break;
          case 3:
            cxExportSchedulerToHTML(SaveDialog->FileName, Scheduler, false, false,
              "Event # %d", (TDateTime)NullDate, (TDateTime)NullDate, "html");
            break;
          case 4:
            cxExportSchedulerToXML(SaveDialog->FileName, Scheduler, false, false,
              "Event # %d", (TDateTime)NullDate, (TDateTime)NullDate, "xml");
            break;
          case 5:
            cxExportSchedulerToXLSX(SaveDialog->FileName, Scheduler, false, false,
              "Event # %d", (TDateTime)NullDate, (TDateTime)NullDate, "xlsx");
            break;
		}
	  }
  }
}
//---------------------------------------------------------------------------

void __fastcall TDemoBasicMainForm::OnNewEvent(TcxSchedulerEvent *AEvent, int AIndex)
{
}
//---------------------------------------------------------------------------
void  TDemoBasicMainForm::CreateEventObject(bool AAllDayEvent, bool ARandomResource, TcxCustomSchedulerStorage *AStorage,
  TColor AColor)
{
  TcxSchedulerEvent *AEvent = AStorage->createEvent();
  TDateTime ADate = GetRandomDate();
  AEvent->Start = ADate;
  if (!AAllDayEvent)
  {
	AEvent->Finish = ADate + (double)((random(180) + 30) * MinuteToTime);
  }
  else
  {
	AEvent->Finish = ADate + 1;
	AEvent->AllDayEvent = True;
  };
  AEvent->State = GetRandomState();
  if (AColor == Graphics::clDefault)
	AEvent->LabelColor = EventLabelColors[random(11)];
  else
	AEvent->LabelColor = AColor;
  int AIndex = random(22);
  AEvent->Caption = SportEvents[StartIndexes[AIndex] + random(EventsCount[AIndex])];
  if (ARandomResource) {
	AEvent->ResourceID = GetRandomResourceID(); }
  OnNewEvent(AEvent, AIndex);
}

void TDemoBasicMainForm::GenerateRandomEvents(int ACount, bool ARandomResource, TcxCustomSchedulerStorage *AStorage,
  TColor AColor)
{
  TDateTime ADate;
  TcxSchedulerEvent *AEvent;
  if (AStorage == NULL) AStorage = Scheduler->Storage;
  if (AStorage == NULL) return;
  AStorage->BeginUpdate();
  try {
	Randomize();
	for (int I = 0; I < (int)(ACount/5 * 4); I++){
	  CreateEventObject(false, ARandomResource, AStorage, AColor);
	}
	for (int I = 0; I < (int)(ACount/5); I++){
	  CreateEventObject(true, ARandomResource, AStorage, AColor);
	}
  }
  __finally {
    AStorage->EndUpdate();
  }
}

//---------------------------------------------------------------------------

String TDemoBasicMainForm::GetRandomCaption()
{
  int AIndex = random(22);
  return SportEvents[StartIndexes[AIndex] + random(EventsCount[AIndex])];
}

//---------------------------------------------------------------------------

TDateTime TDemoBasicMainForm::GetRandomDate()
{
  TDateTime ADate = Date() + random(MaxRandomPeriod) - 30 + Scheduler->OptionsView->WorkStart;
  if ((int)(ADate) == (int)Date())
    ADate = ADate + (double)(random(10) * HourToTime);
  else
    ADate = ADate + (double)(random(24) * HourToTime);
  return ADate;
}

//---------------------------------------------------------------------------

TColor TDemoBasicMainForm::GetRandomLabelColor()
{
  return EventLabelColors[random(11)];
}

//---------------------------------------------------------------------------

Variant TDemoBasicMainForm::GetRandomResourceID()
{
  if ((Scheduler->Storage == NULL) || (Scheduler->Storage->ResourceCount == 0))
    return Null;
  else
    if (Scheduler->Storage->ResourceCount == 1)
      return Scheduler->Storage->ResourceIDs[0];
    else
      return Scheduler->Storage->ResourceIDs[random(Scheduler->Storage->ResourceCount)];
}

//---------------------------------------------------------------------------

int TDemoBasicMainForm::GetRandomState()
{
  return random(4);
}
//---------------------------------------------------------------------------

void __fastcall TDemoBasicMainForm::SyncClick(TObject *Sender)
{
  TcxCustomSchedulerStorageField *SyncIDField = Scheduler->Storage->GetFieldByName("SyncIDField");
  if (SyncIDField == NULL) return;
  TcxOutlookSynchronize *OutlookSync = new TcxOutlookSynchronize(Scheduler->Storage);
  OutlookSync->EntryIDField = SyncIDField;
  if (((TMenuItem*)Sender)->Tag == 0)
  {
    OutlookSync->SynchronizeWithStorage(false);
  }
  else
  {
    OutlookSync->SynchronizeWithOutlook(false);
  }
  delete OutlookSync;
}
//---------------------------------------------------------------------------

void __fastcall TDemoBasicMainForm::miResCountClick(TObject *Sender)
{
  Scheduler->OptionsView->ResourcesPerPage = ((TMenuItem*)Sender)->Tag;
  if (((TMenuItem*)Sender)->Tag == 0)
  {
    Scheduler->ResourceNavigator->Visibility = snvNever;
  }
  else
  {
    Scheduler->ResourceNavigator->Visibility = snvAlways;
  }
}
//---------------------------------------------------------------------------

void __fastcall TDemoBasicMainForm::miTimeWorktimeonlyClick(TObject *Sender)
{
  ((TMenuItem*)Sender)->Checked = !((TMenuItem*)Sender)->Checked;
  Scheduler->ViewTimeGrid->WorkTimeOnly = ((TMenuItem*)Sender)->Checked;
  Scheduler->ViewTimeGrid->WorkDaysOnly = ((TMenuItem*)Sender)->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TDemoBasicMainForm::miDaySettingsClick(TObject *Sender)
{
  ((TMenuItem*)Sender)->Checked = !((TMenuItem*)Sender)->Checked;
  bool AChecked = ((TMenuItem*)Sender)->Checked;
  switch (((TMenuItem*)Sender)->Tag) {
    case 0:
      Scheduler->ViewDay->HeaderContainer = AChecked;
      break;
    case 1:
     Scheduler->ViewDay->AlwaysShowEventTime = AChecked;
     break;
    case 2:
     Scheduler->ViewDay->WorkTimeOnly = AChecked;
     break;
    case 3:
     Scheduler->ViewDay->TimeRulerMinutes = AChecked;
     break;
  }
}
//---------------------------------------------------------------------------

void __fastcall TDemoBasicMainForm::miWeekViewClick(TObject *Sender)
{
  ((TMenuItem*)Sender)->Checked = !((TMenuItem*)Sender)->Checked;
  if (((TMenuItem*)Sender)->Tag == 0)
  {
    Scheduler->ViewWeek->CompressWeekEnd = ((TMenuItem*)Sender)->Checked;
  }
  else
  {
    if (((TMenuItem*)Sender)->Checked)
    {
      Scheduler->ViewWeek->DaysLayout = wdlOneColumn;
    }
    else
    {
      Scheduler->ViewWeek->DaysLayout = wdlTwoColumns;
    }
  }
//
}
//---------------------------------------------------------------------------

void __fastcall TDemoBasicMainForm::miCompressweekendsClick(TObject *Sender)
{
  ((TMenuItem*)Sender)->Checked = !((TMenuItem*)Sender)->Checked;
  Scheduler->ViewWeeks->CompressWeekEnd = ((TMenuItem*)Sender)->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TDemoBasicMainForm::AlldayeventsonlyClick(TObject *Sender)
{
  ((TMenuItem*)Sender)->Checked = !((TMenuItem*)Sender)->Checked;
  Scheduler->ViewYear->AllDayEventsOnly = ((TMenuItem*)Sender)->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TDemoBasicMainForm::miSharingClick(TObject *Sender)
{
  ((TMenuItem*)Sender)->Checked = !((TMenuItem*)Sender)->Checked;
  Scheduler->EventOperations->SharingBetweenResources = ((TMenuItem*)Sender)->Checked;
}                  

void __fastcall TDemoBasicMainForm::miIntersectionClick(TObject *Sender)
{
  ((TMenuItem*)Sender)->Checked = !((TMenuItem*)Sender)->Checked;
  Scheduler->EventOperations->Intersection = ((TMenuItem*)Sender)->Checked;
}

void __fastcall TDemoBasicMainForm::miWeekCompressWeekendsClick(
	  TObject *Sender)
{
  ((TMenuItem*)Sender)->Checked = !((TMenuItem*)Sender)->Checked;
  Scheduler->ViewWeek->CompressWeekEnd = ((TMenuItem*)Sender)->Checked;
}

void __fastcall TDemoBasicMainForm::miWeekViewHideweekendClick(
	  TObject *Sender)
{
	Scheduler->ViewWeek->HideWeekEnd = miWeekViewHideweekend->Checked;
	Scheduler->ViewWeeks->HideWeekEnd = miMonthViewHideweekend->Checked;
	miWeekViewHideweekend->Checked = Scheduler->ViewWeek->HideWeekEnd;
	miMonthViewHideweekend->Checked = Scheduler->ViewWeeks->HideWeekEnd;
}

void __fastcall TDemoBasicMainForm::mi_dedUnlimitedClick(TObject *Sender)
{
  ((TMenuItem*)Sender)->Checked = True;
  Scheduler->ViewAgenda->DisplayMode = (TcxSchedulerAgendaViewDisplayMode)(((TMenuItem*)Sender)->Tag);
}

void __fastcall TDemoBasicMainForm::mi_dhoHorizontalClick(TObject *Sender)
{
  ((TMenuItem*)Sender)->Checked = True;
  Scheduler->ViewAgenda->DayHeaderOrientation = (TcxSchedulerAgendaViewDayHeaderOrientation)(((TMenuItem*)Sender)->Tag);
}

void __fastcall TDemoBasicMainForm::AgendaSettingsClick(TObject *Sender)
{
  ((TMenuItem*)Sender)->Checked = !((TMenuItem*)Sender)->Checked;
  bool AChecked = ((TMenuItem*)Sender)->Checked;
  switch (((TMenuItem*)Sender)->Tag) {
    case 0:
      Scheduler->ViewAgenda->ShowLocations = AChecked;
      break;
    case 1:
      Scheduler->ViewAgenda->ShowResources = AChecked;
      break;
    case 2:
      Scheduler->ViewAgenda->ShowTimeAsClock = AChecked;
      break;
  }
}

void __fastcall TDemoBasicMainForm::miSelectOnRightClickClick(TObject *Sender)
{
  Scheduler->OptionsBehavior->SelectOnRightClick = !Scheduler->OptionsBehavior->SelectOnRightClick;
  miSelectOnRightClick->Checked = Scheduler->OptionsBehavior->SelectOnRightClick;
}

void __fastcall TDemoBasicMainForm::miShowEventsClick(TObject *Sender)
{
  Scheduler->ViewDay->ShowAllDayEventsInContentArea = !Scheduler->ViewDay->ShowAllDayEventsInContentArea;
  miShowEvents->Checked = !Scheduler->ViewDay->ShowAllDayEventsInContentArea;
}

void __fastcall TDemoBasicMainForm::miAllDayScrollClick(TObject *Sender)
{
  Scheduler->ViewDay->AllDayAreaScrollBar = TcxSchedulerAllDayAreaScrollBar(((TMenuItem*)Sender)->Tag);
  ((TMenuItem*)Sender)->Checked = True;
}

void __fastcall TDemoBasicMainForm::miDayHeaderAreaClick(TObject *Sender)
{
  Scheduler->ViewDay->DayHeaderArea = !Scheduler->ViewDay->DayHeaderArea;
  miDayHeaderArea->Checked = Scheduler->ViewDay->DayHeaderArea;
}

void __fastcall TDemoBasicMainForm::AllDayAreaHeightClick(TObject *Sender)
{
  cxSchedulerAllDayEventContainerMaxLineCount = ((TMenuItem*)Sender)->Tag;
  ((TMenuItem*)Sender)->Checked = True;
  Scheduler->CurrentView->Refresh();
}

//---------------------------------------------------------------------------

