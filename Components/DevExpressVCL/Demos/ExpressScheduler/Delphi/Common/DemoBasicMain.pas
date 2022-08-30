unit DemoBasicMain;

interface

{$I cxVer.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, cxControls, cxLookAndFeels, ActnList, ImgList, Menus, ComCtrls,
  StdCtrls, cxClasses, cxStyles, cxGraphics, cxEdit, cxScheduler,
  cxSchedulerCustomControls, cxSchedulerCustomResourceView, cxSchedulerStorage,
  cxSchedulerDayView, cxSchedulerDateNavigator, ExtCtrls, cxExportSchedulerLink,
  cxSchedulerWeekView,
{$IFDEF EXPRESSPRINTINGSYSTEM}
  dxPSCore, dxPScxSchedulerLnk,
{$ENDIF}
{$IFDEF EXPRESSBARS}
  cxSchedulerRibbonStyleEventEditor,
{$ENDIF}
{$IFDEF EXPRESSSKINS}
  dxSkinsdxRibbonPainter,
  dxSkinsdxBarPainter,
{$ENDIF}
  cxSchedulerTimeGridView, cxSchedulerUtils, cxSchedulerYearView,
  cxSchedulerHolidays, cxSchedulerGanttView, cxSchedulerAgendaView, cxLookAndFeelPainters;

type
  TDemoBasicMainForm = class(TForm)
    mmMain: TMainMenu;
    miAbout: TMenuItem;
    miFile: TMenuItem;
    miExit: TMenuItem;
    lbDescrip: TLabel;
    miView: TMenuItem;
    miViewDateNavigator: TMenuItem;
    miControlBox: TMenuItem;
    miViewposition: TMenuItem;
    miAtLeft: TMenuItem;
    miAtRight: TMenuItem;
    Scheduler: TcxScheduler;
    miDay1: TMenuItem;
    miWeek1: TMenuItem;
    miMonth1: TMenuItem;
    miWorkweek: TMenuItem;
    miGotoDate: TMenuItem;
    pnlControls: TPanel;
    Memo1: TMemo;
    StatusBar: TStatusBar;
    Timer1: TTimer;
    Resources1: TMenuItem;
    GroupBy1: TMenuItem;
    miGroupByNone: TMenuItem;
    miGroupByResources: TMenuItem;
    miGroupByDate: TMenuItem;
    Resourcelayout1: TMenuItem;
    Exportto1: TMenuItem;
    Outlook1: TMenuItem;
    Outlook2: TMenuItem;
    Excel1: TMenuItem;
    Html1: TMenuItem;
    Xml1: TMenuItem;
    Separator2: TMenuItem;
    ext1: TMenuItem;
    SaveDialog: TSaveDialog;
    Separator1: TMenuItem;
    miTimeGrid1: TMenuItem;
    miYear1: TMenuItem;
    N4: TMenuItem;
    miResPerPage: TMenuItem;
    miWorktimeonly: TMenuItem;
    miAllDayContainer: TMenuItem;
    Alwaysshoweventtime1: TMenuItem;
    Displayminutesontimeruler1: TMenuItem;
    miSingleColumn: TMenuItem;
    miOneResPerPage: TMenuItem;
    miTwoResPerPage: TMenuItem;
    miThreeResPerPage: TMenuItem;
    miAllResPerPage: TMenuItem;
    miSeparator: TMenuItem;
    miTimeWorktimeonly: TMenuItem;
    Alldayeventsonly1: TMenuItem;
    miCompressweekends: TMenuItem;
    miWeekCompressWeekends: TMenuItem;
    Options1: TMenuItem;
    miDay: TMenuItem;
    miWeek: TMenuItem;
    miMonth: TMenuItem;
    miTimeGrid: TMenuItem;
    miYear: TMenuItem;
    Outlooksynchronization1: TMenuItem;
    miEventsOpt: TMenuItem;
    miIntersection: TMenuItem;
    miSharing: TMenuItem;
    miWeekViewHideWeekend: TMenuItem;
    miMonthViewHideWeekEnd: TMenuItem;
    N1: TMenuItem;
    miSelectOnRightClick: TMenuItem;
    Alldayarea1: TMenuItem;
    miShowEvents: TMenuItem;
    Scrollbar1: TMenuItem;
    miAllDayScrollDefault: TMenuItem;
    miAllDayScrollNever: TMenuItem;
    miAllDayScrollAlways: TMenuItem;
    miDayHeaderArea: TMenuItem;
    Height1: TMenuItem;
    miAllDayAreaHeightDefault: TMenuItem;
    mimiAllDayAreaHeight3: TMenuItem;
    miAllDayAreaHeight5: TMenuItem;
    lfController: TcxLookAndFeelController;
    Excel2: TMenuItem;
    miAgenda: TMenuItem;
    Agenda1: TMenuItem;
    DayHeaderOrientation1: TMenuItem;
    mi_dhoHorizontal: TMenuItem;
    mi_dhoVertical: TMenuItem;
    DisplayEmptyDayMode1: TMenuItem;
    mi_dedUnlimited: TMenuItem;
    mi_dedSelected: TMenuItem;
    mi_dedHideEmptyDays: TMenuItem;
    ShowLocation1: TMenuItem;
    ShowResources1: TMenuItem;
    ShowTimeAsClock1: TMenuItem;
    miViewStyle: TMenuItem;
    miModern: TMenuItem;
    miClassic: TMenuItem;
    miDayHeaderModernDisplayMode: TMenuItem;
    miDHMDefault: TMenuItem;
    miDHMClassic: TMenuItem;
    miDHMDayAndDate: TMenuItem;
    procedure FileExitExecute(Sender: TObject);
    procedure miViewDateNavigatorClick(Sender: TObject);
    procedure ViewClick(Sender: TObject);
    procedure ViewModeClick(Sender: TObject);
    procedure ViewPositionClick(Sender: TObject);
    procedure miAboutClick(Sender: TObject);
    procedure miGotoDateClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Resourcelayout1Click(Sender: TObject);
    procedure miGroupByClick(Sender: TObject);
    procedure ExportToClick(Sender: TObject);
    procedure SyncClick(Sender: TObject);
    procedure miResCountClick(Sender: TObject);
    procedure miTimeWorktimeonlyClick(Sender: TObject);
    procedure miDaySettingsClick(Sender: TObject);
    procedure miWeekViewClick(Sender: TObject);
    procedure miCompressweekendsClick(Sender: TObject);
    procedure AlldayeventsonlyClick(Sender: TObject);
    procedure miSharingClick(Sender: TObject);
    procedure miIntersectionClick(Sender: TObject);
    procedure miWeekCompressWeekendsClick(Sender: TObject);
    procedure miWeekViewHideWeekendClick(Sender: TObject);
    procedure miSelectOnRightClickClick(Sender: TObject);
    procedure miShowEventsClick(Sender: TObject);
    procedure miAllDayScrollClick(Sender: TObject);
    procedure miDayHeaderAreaClick(Sender: TObject);
    procedure AllDayAreaHeightClick(Sender: TObject);
    procedure mi_dhoHorizontalClick(Sender: TObject);
    procedure mi_dedUnlimitedClick(Sender: TObject);
    procedure AgendaSettingsClick(Sender: TObject);
    procedure miViewStyleClick(Sender: TObject);
    procedure miDayHeaderModernDisplayModeClick(Sender: TObject);
  private
    procedure AddDialogsStyle;
    procedure AdjustStorage(var AStorage: TcxCustomSchedulerStorage);
  protected
    AnchorDate: TDateTime;
  {$IFDEF EXPRESSPRINTINGSYSTEM}
    ComponentPrinter: TdxComponentPrinter;
    ReportLink: TcxSchedulerReportLink;
    procedure OnPrintClick(Sender: TObject);
  {$ENDIF}
    procedure AddLookAndFeelMenu; virtual;
    procedure CheckSchedulerViewStyle;
    procedure CreateEventObject(AAllDayEvent, ARandomResource: Boolean; AStorage: TcxCustomSchedulerStorage;
      AColor: TColor = clDefault);
    procedure GenerateRandomEvents(ACount: Integer; ARandomResource: Boolean = False;
      AStorage: TcxCustomSchedulerStorage = nil; AColor: TColor = clDefault);
    function GetDefaultLookAndFeelKind: TcxLookAndFeelKind; virtual;
    function GetRandomCaption: string;
    function GetRandomDate: TDateTime;
    function GetRandomLabelColor: TColor;
    function GetRandomResourceID: Variant;
    function GetRandomState: Integer;
    function GetSyncIDField: TcxCustomSchedulerStorageField; virtual;
    function IsNativeDefaultStyle: Boolean; virtual;
    procedure SetDefaultLookAndFeel; virtual;
    procedure OnNewEvent(AEvent: TcxSchedulerEvent; AIndex: Integer); virtual;
  public
  {$IFDEF EXPRESSPRINTINGSYSTEM}
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  {$ENDIF}
  end;

var
  DemoBasicMainForm: TDemoBasicMainForm;

  StartIndexes: array[0..21] of Integer =
   (0, 12, 30, 44, 58, 80, 90, 106, 126, 157, 174, 196,
    208, 220, 226, 234, 238, 242, 249, 260, 267, 272);

  EventsCount: array[0..21] of integer =
   (12, 18, 14, 14, 22, 10, 16, 20, 31, 17,
    22, 12, 12, 6, 8, 4, 4, 7, 11, 7, 5, 8);

  SportEvents: array[0..279] of string =
   ('Basketball Qualifying - Men',
    'Basketball Qualifying - Women',
    'Basketball First Group Phase - Men',
    'Basketball First Group Phase - Women',
    'Basketball Quarterfinals - Men',
    'Basketball Quarterfinals - Women',
    'Basketball Semifinals - Men',
    'Basketball Semifinals - Women',
    'Basketball Places 3/4 - Men',
    'Basketball Places 3/4 - Women',
    'Basketball Finals - Men',
    'Basketball Finals - Women',

    'Lamon Brewster (32-2) vs. Luan Krasniqi (28-1-1) (WBO heavyweight belt)',
    '(PPV) Antonio Tarver (23-3) vs. Roy Jones (49-3) (IBO light heavyweight belt)',
    '(Showtime) James Toney (68-4-2) vs. Dominick Guinn (25-2-1)',
    'Nicolay Valuev (41-0) vs. TBA',
    'Danilo Haussler (25-3) vs. TBA',
    'Cengiz Koc (22-1) vs. TBA',
    '(PPV) Diego Corrales (40-2) vs. Jose Luis Castillo (52-7-1)',
    '(WBC and WBO lightweight belts) (PPV) Carlos Hernandez vs. Bobby Pacquiao',
    '(PPV) Jorge Arce vs. Hussein Hussein',
    'Vince Philips (47-9-1) vs. Reynaldo Pelonia (35-21-3)',
    'Kili Madrid (6-0-1) vs. Donny Fosmire (10-7)',
    'Nelson Zepeda (1-0-1) vs. Kaleo Padilla (0-0)',
    'Justin Mercado (1-1) vs. Waldo Rojas (0-0)',
    'Illima Vicente (0-0) vs. Jenny Houts (0-0)',
    'Tomasz Adamek (29-0) vs. Thomas Ulrich (28-1) (WBC light heavyweight belt)',
    '(Showtime) Jeff Lacy (20-0) vs. Joe Calzaghe (39-0) (IBF, IBO and WBO super middleweight belts)',
    'Jermain Taylor vs. Bernard Hopkins',
    'Ronald "Winky" Wright vs. TBA',

    'Australian Open',
    'Pacific Life Open',
    'NASDAQ-100 Open',
    'Tennis Masters Monte Carlo',
    'Telecom Italia Masters Roma',
    'Tennis Masters Hamburg',
    'Roland Garros',
    'Wimbledon',
    'Tennis Masters Montreal',
    'Western and Southern Financial Group Masters',
    'US Open',
    'Tennis Masters Madrid',
    'BNP Paribas Masters',
    'Tennis Masters Cup',

    'Weightlifting 48 Kg - Women  - Final',
    'Weightlifting 56 Kg - Men  - Final',
    'Weightlifting 53 Kg - Women  - Final',
    'Weightlifting 62 Kg - Men  - Final',
    'Weightlifting 63 Kg - Women  - Final',
    'Weightlifting 69 Kg - Men  - Final',
    'Weightlifting 69 Kg - Women  - Final',
    'Weightlifting 77 Kg - Men  - Final',
    'Weightlifting 75 Kg - Women  - Final',
    'Weightlifting 75kg - Women  - Final',
    'Weightlifting 85 Kg - Men  - Final',
    'Weightlifting 94 Kg - Men  - Final',
    'Weightlifting 105 Kg - Men  - Final',
    'Weightlifting 105kg - Men  - Final',

    'Sabre - Men - 1st Round',
    'Sabre - Men - Round 2',
    'Sabre - Men - 3rd Round',
    'Sabre - Men - Quarter final',
    'Sabre - Men - Semi-finals',
    'Sabre - Men - Final',
    'Epee - Women - 1st Round',
    'Epee - Women - Round 2',
    'Epee - Women - 3rd Round',
    'Epee - Women - Quarter final',
    'Epee - Women - Semi-finals',
    'Epee - Women - Final',
    'Epee - Women/Team - 1st Round',
    'Epee - Women/Team - Quarter final',
    'Epee - Women/Team - Semi-finals',
    'Epee - Women/Team - Final',
    'Foil - Men - 1st Round',
    'Foil - Men - Round 2',
    'Foil - Men - 3rd Round',
    'Foil - Men - Quarter final',
    'Foil - Men - Semi-finals',
    'Foil - Men - Final',

    'Soccer 1st Round - *Men''s Preliminaries - Men',
    'Soccer 1st Round - *Women''s Preliminaries - Women',
    'Soccer Quarter final - *Men''s Quarterfinal - Men',
    'Soccer Quarter final - *Women''s Quarterfinal - Women',
    'Soccer Semi-finals - *Women''s Semifinal - Men',
    'Soccer Semi-finals - *Women''s Semifinal - Women',
    'Soccer places 3/4 - *Men''s Bronze Medal Match - Men',
    'Soccer places 3/4 - *Women''s Bronze Medal Match - Women',
    'Soccer Final - *Men''s Gold Medal Match - Men',
    'Soccer Final - *Women''s Gold Medal Match - Women',

    'Men - Qualifying',
    'Women - Qualifying',
    'Men/Team - Final',
    'Women/Team - Final',
    'Individual All-Around - Men - Final',
    'Individual All-Around - Women - Final',
    'Floor Exercise - Men - Final',
    'Vault - Women - Final',
    'Uneven Bars - Women - Final',
    'Pommel Horse - Men - Final',
    'Rings - Men - Final',
    'Vault - Men - Final',
    'Beam - Women - Final',
    'Parallel Bars - Men - Final',
    'Floor Exercise - Women - Final',
    'Horizontal Bar - Men - Final',

    'Slalom C1 - Men - Heats',
    'Slalom C1 - Men - Heats',
    'Slalom C1 - Men - Semi-finals',
    'Slalom C1 - Men - Final',
    'Slalom C2 - Men - Heats',
    'Slalom C2 - Men - Heats',
    'Slalom C2 - Men - Semi-finals',
    'Slalom C2 - Men - Final',
    'Flatwater C1 - 1000m - Men - Heats',
    'Flatwater C2 - 1000m - Men - Heats',
    'Flatwater C1 - 500m - Men - Heats',
    'Flatwater C2 - 500m - Men - Heats',
    'Flatwater C1 - 1000m - Men - Semi-finals',
    'Flatwater C2 - 1000m - Men - Semi-finals',
    'Flatwater C1 - 500m - Men - Semi-finals',
    'Flatwater C2 - 500m - Men - Semi-finals',
    'Flatwater C1 - 1000m - Men - Final',
    'Flatwater C2 - 1000m - Men - Final',
    'Flatwater C1 - 500m - Men - Final',
    'Flatwater C2 - 500m - Men - Final',

    'Slalom K1 - Women - Heats',
    'Slalom K1 - Women - Heats',
    'Slalom K1 - Women - Semi-finals',
    'Slalom K1 - Women - Final',
    'Slalom K2 - Men - Heats',
    'Slalom K2 - Men - Heats',
    'Slalom K2 - Men - Semi-finals',
    'Slalom K1 - Men - Final',
    'Flatwater K1 - 1000m - Men - Heats',
    'Flatwater K4 - 500m - Women - Heats',
    'Flatwater K2 - 1000m - Men - Heats',
    'Flatwater K4 - 1000m - Men - Heats',
    'Flatwater K1 - 500m - Men - Heats',
    'Flatwater K2 - 500m - Men - Heats',
    'Flatwater K2 - 500m - Women - Heats',
    'Flatwater K1 - 1000m - Men - Semi-finals',
    'Flatwater K4 - 500m - Women - Semi-finals',
    'Flatwater K2 - 1000m - Men - Semi-finals',
    'Flatwater K4 - 1000m - Men - Semi-finals',
    'Flatwater K1 - 500m - Men - Semi-finals',
    'Flatwater K1 - 500m - Women - Semi-finals',
    'Flatwater K2 - 500m - Men - Semi-finals',
    'Flatwater K2 - 500m - Women - Semi-finals',
    'Flatwater K1 - 1000m - Men - Final',
    'Flatwater K4 - 500m - Women - Final',
    'Flatwater K2 - 1000m - Men - Final',
    'Flatwater K4 - 1000m - Men - Final',
    'Flatwater K1 - 500m - Men - Final',
    'Flatwater K1 - 500m - Women - Final',
    'Flatwater K2 - 500m - Men - Final',
    'Flatwater K2 - 500m - Women - Final',

    'Greco-Roman 55kg - Men - Qualifying',
    'Greco-Roman 66kg - Men - Qualifying',
    'Greco-Roman 84kg - Men - Qualifying',
    'Greco-Roman 120kg - Men - Qualifying',
    'Greco-Roman 55kg - Men - Semi-finals',
    'Greco-Roman 66kg - Men - Semi-finals',
    'Greco-Roman 84kg - Men - Semi-finals',
    'Greco-Roman 120kg - Men - Semi-finals',
    'Greco-Roman 96kg - Men - Qualifying',
    'Greco-Roman 55kg - Men - Final',
    'Greco-Roman 66kg - Men - Final',
    'Greco-Roman 84kg - Men - Final',
    'Greco-Roman 120kg - Men - Final',
    'Greco-Roman 55kg - Men - Play Off',
    'Greco-Roman 66kg - Men - Play Off',
    'Greco-Roman 84kg - Men - Play Off',
    'Greco-Roman 120kg - Men - Play Off',

    'Individual Eventing Dressage - 1st Day',
    'Team Eventing Dressage - 1st Day',
    'Individual Eventing Dressage - 2nd Day',
    'Team Eventing Dressage - 2nd Day',
    'Individual Eventing Cross Country - Final',
    'Team Eventing Cross Country - Final',
    'Team Eventing Jumping - Final',
    'Individual Eventing Jumping - Qualifying',
    'Individual Eventing Jumping - Final',
    'Individual Dressage Grand Prix - 1st Day',
    'Team Dressage Grand Prix - 1st Day',
    'Individual Dressage Grand Prix - 2nd Day',
    'Team Dressage Grand Prix - 2nd Day',
    'Individual Jumping - Qualifying',
    'Individual Dressage Grand Prix Special - Final',
    'Team Jumping - Final',
    'Individual Jumping - Qualifying',
    'Individual Jumping - Qualifying',
    'Team Jumping - Final',
    'Individual Dressage Grand Prix Freestyle - Final',
    'Individual Jumping - Final',
    'Individual Jumping - Final',

    'Men''s 470 - Race 01',
    'Women''s 470 - Race 01',
    'Men''s 470 - Race 02',
    'Women''s 470 - Race 02',
    'Finn - Race 1',
    'Yngling - Race 1',
    'Finn - Race 2',
    'Yngling - Race 2',
    'Laser - Race 1',
    'Women''s Mistral - Race 01',
    'Men''s Mistral - Race 02',
    '49er - Race 1',

    'Men''s 400m Individual Medley - Heat 1',
    'Men''s 400m Individual Medley - Heat 2',
    'Women''s 100m Butterfly - Heat 1',
    'Men''s 400m Freestyle - Heat 1',
    'Women''s 400m Individual Medley - Heat 1',
    'Women''s 400m Individual Medley - Heat 2',
    'Men''s 100m Breaststroke - Heat 1',
    'Men''s 100m Breaststroke - Heat 2',
    'Women''s 4 x 100m Freestyle Relay - Heat 1',
    'Women''s 4 x 100m Freestyle Relay - Heat 2',
    'Women''s 100m Butterfly Semifinal 1',
    'Women''s 4 x 100m Freestyle Relay Final',

    'Women''s Synchronised 3m Springboard Final',
    'Men''s Synchronised 3m Springboard Final',
    'Women''s Synchronised 10m Platform Final',
    'Men''s Synchronised 10m Platform Final',
    'Women''s 10m Platform Preliminary',
    'Men''s 10m Platform Semifinal',

    'Men''s Preliminaries - Pool A Match 1 - Spain - Korea',
    'Women''s Preliminaries - Pool A Match 1 - China - Hungary',
    'Men''s Classification 11-12 Match 31 - Slovenia - Egypt',
    'Women''s Classification 9-10 Match 21 - Greece - Angola',
    'Men''s Classification 9-10 Match 32 - Brazil - Iceland',
    'Women''s Quarterfinal Match 22 - Ukraine - Spain',
    'Men''s Semifinal Match 40 - Germany - Russia',
    'Women''s Semifinal Match 27 - France - Korea',

    'Men - Qualifying',
    'Women - Qualifying',
    'Men/Team - Final',
    'Women/Team - Final',

    'Women''s 100m Round 1 - Heat 1',
    'Men''s 100m Round 1 - Heat 1',
    'Men''s 100m Semifinal 1',
    'Women''s 100m Final',

    'Men''s 10m Air Pistol Qualification',
    'Men''s 10m Air Pistol Final',
    'Women''s 10m Air Pistol Pre-event Training',
    'Men''s 10m Air Pistol Medal Ceremony',
    'Women''s 10m Air Pistol Qualification',
    'Men''s 50m Pistol Qualification',
    'Women''s 25m Pistol Final',

    'Women''s Individual 1/32 Eliminations',
    'Men''s Individual 1/32 Eliminations',
    'Women''s Individual 1/16 Eliminations',
    'Men''s Individual 1/16 Eliminations',
    'Women''s Individual 1/8 Eliminations',
    'Men''s Individual 1/8 Eliminations',
    'Women''s Individual Quarterfinal 1',
    'Men''s Individual Quarterfinal 1',
    'Men''s Individual Semifinal 1',
    'Men''s Individual Bronze Medal Match',
    'Women''s Team Gold Medal Match',

    'Men''s Road Race',
    'Women''s Road Race',
    'Women''s Individual Time Trial',
    'Men''s Individual Time Trial',
    'Women''s Sprint 1/8 Finals',
    'Women''s Individual Pursuit Final',
    'Men''s Sprint 1/8 Finals',

    'Men''s Preliminaries - Group B - EGY - AUS',
    'Women''s Classification 7th-8th - KAZ - CAN',
    'Women''s Quarterfinal 02 - ITA - HUN',
    'Women Bronze Medal Game',
    'Men''s Semifinal 02',

    'Women''s Preliminaries - Pool B Match 1 - CUB - GER',
    'Men''s Preliminaries - Pool A Match 1 - SCG - POL',
    'Women''s Quarterfinal 04 - JPN - CHN',
    'Men''s Quarterfinal 03 - GRE - USA',
    'Women''s Semifinal 02 - CUB - CHN',
    'Men''s Semifinal 02 - USA - BRA',
    'Women''s Gold Medal Match - RUS - CHN',
    'Men''s Bronze Medal Match - RUS - USA');

  MaxRandomPeriod: Integer = 60;

implementation

uses
  Variants, DateUtils,cxSchedulerDialogs, AboutDemoForm, cxSchedulerOutlookExchange,
  DemoUtils, SkinDemoUtils;

{$R *.dfm}

{$IFDEF EXPRESSPRINTINGSYSTEM}

constructor TDemoBasicMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ComponentPrinter := TdxComponentPrinter.Create(Self);
  ReportLink := ComponentPrinter.AddLink(Scheduler) as TcxSchedulerReportLink;
  miFile.Insert(2, NewLine);
  miFile.Insert(3, NewItem('Print...', ShortCut(Byte('P'), [ssCtrl]), False,
    True, OnPrintClick, 0, 'miPrint'));
end;

destructor TDemoBasicMainForm.Destroy;
begin
  ComponentPrinter.Free;
  inherited Destroy;
end;

procedure TDemoBasicMainForm.OnPrintClick(Sender: TObject);
const
  Note = 'Note: Printing functionality for the ExpressScheduler is only available if you own a copy of' + #13#10 +
    'the ExpressPrinting System, which is sold separately.';
begin
  ShowMessage(Note);
  ReportLink.Preview();
end;

{$ENDIF}

procedure TDemoBasicMainForm.CheckSchedulerViewStyle;
begin
  Scheduler.OptionsView.Style := TcxSchedulerViewStyle(Integer(miModern.Checked));
end;

procedure TDemoBasicMainForm.CreateEventObject(AAllDayEvent, ARandomResource: Boolean;
  AStorage: TcxCustomSchedulerStorage; AColor: TColor = clDefault);
var
  AIndex: Integer;
  ADate: TDateTime;
  AEvent: TcxSchedulerEvent;
begin
  AEvent := AStorage.CreateEvent;
  ADate := GetRandomDate;
  AEvent.Start := ADate;
  if not AAllDayEvent then
    AEvent.Finish := ADate + (Random(180) + 30) * MinuteToTime
  else
  begin
    AEvent.Finish := ADate + 1;
    AEvent.AllDayEvent := True;
  end;
  AEvent.State := GetRandomState;
  if AColor = clDefault then
    AEvent.LabelColor := EventLabelColors[Random(11)]
  else
    AEvent.LabelColor := AColor;
  AIndex := Random(22);
  AEvent.Caption := SportEvents[StartIndexes[AIndex] + Random(EventsCount[AIndex])];
  if ARandomResource then
    AEvent.ResourceID := GetRandomResourceID;
  OnNewEvent(AEvent, AIndex);
end;

procedure TDemoBasicMainForm.GenerateRandomEvents(ACount: Integer; ARandomResource: Boolean = False;
  AStorage: TcxCustomSchedulerStorage = nil; AColor: TColor = clDefault);
var
  I: Integer;
begin
  AdjustStorage(AStorage);
  if AStorage = nil then Exit;
  AStorage.BeginUpdate;
  try
    Randomize;
    for I := 0 to ACount div 5 * 4 - 1 do
      CreateEventObject(False, ARandomResource, AStorage, AColor);
    for I := 0 to ACount div 5 - 1 do
      CreateEventObject(True, ARandomResource, AStorage, AColor);
  finally
    AStorage.EndUpdate;
  end;
end;

function TDemoBasicMainForm.GetRandomCaption: string;
var
  AIndex: Integer;
begin
  AIndex := Random(22);
  Result := SportEvents[StartIndexes[AIndex] + Random(EventsCount[AIndex])]
end;

function TDemoBasicMainForm.GetRandomDate: TDateTime;
begin
  Result := Date + Random(MaxRandomPeriod)*(Random(2) * 2 - 1) + Scheduler.OptionsView.WorkStart;
  if Trunc(Result) = Date then
    Result := Result + Random(10) * HourToTime
  else
    Result := Result + Random(24) * HourToTime;
end;

function TDemoBasicMainForm.GetRandomLabelColor: TColor;
begin
  Result := EventLabelColors[Random(11)];
end;

function TDemoBasicMainForm.GetRandomResourceID: Variant;
begin
  Result := Null;
  if (Scheduler.Storage = nil) or (Scheduler.Storage.ResourceCount = 0) then
    Exit
  else
    if Scheduler.Storage.ResourceCount = 1 then
      Result := Scheduler.Storage.ResourceIDs[0]
    else
      Result := Scheduler.Storage.ResourceIDs[Random(Scheduler.Storage.ResourceCount)];
end;

function TDemoBasicMainForm.GetRandomState: Integer;
begin
  Result := Random(4);
end;

function TDemoBasicMainForm.GetSyncIDField: TcxCustomSchedulerStorageField;
begin
  Result := Scheduler.Storage.GetFieldByName('SyncIDField');
end;

procedure TDemoBasicMainForm.OnNewEvent(
  AEvent: TcxSchedulerEvent; AIndex: Integer);
begin
end;

procedure TDemoBasicMainForm.AddDialogsStyle;
begin
{$IFDEF EXPRESSBARS}
   mmMain.Items.Insert(mmMain.Items.IndexOf(miAbout), CreateDialogsStylesMenuItems(mmMain.Items));
{$ENDIF}
end;

procedure TDemoBasicMainForm.AdjustStorage(var AStorage: TcxCustomSchedulerStorage);
begin
  if AStorage = nil then
    AStorage := Scheduler.Storage;
end;

procedure TDemoBasicMainForm.FileExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TDemoBasicMainForm.miViewDateNavigatorClick(Sender: TObject);
begin
  Scheduler.DateNavigator.Visible := miViewDateNavigator.Checked;
end;

procedure TDemoBasicMainForm.ViewClick(Sender: TObject);
begin
  with TMenuItem(Sender) do
  begin
    Checked := not Checked;
    case Tag of
      0: Scheduler.DateNavigator.Visible := Checked;
      1: Scheduler.ControlBox.Visible := Checked;
    end;
  end;
end;

procedure TDemoBasicMainForm.miViewStyleClick(Sender: TObject);
begin
  TMenuItem(Sender).Checked := not TMenuItem(Sender).Checked;
  CheckSchedulerViewStyle;
end;

procedure TDemoBasicMainForm.ViewModeClick(Sender: TObject);
begin
  if not Scheduler.ViewWeek.Active then
    AnchorDate := Scheduler.SelectedDays[0];
  Scheduler.SelectDays([AnchorDate], TMenuItem(Sender).Tag in [0, 1]);
  case TMenuItem(Sender).Tag of
    0:
      Scheduler.ViewDay.Active := True;
    1:
      Scheduler.SelectWorkDays(Date);
    2:
      Scheduler.ViewWeek.Active := True;
    3:
      Scheduler.GoToDate(Scheduler.SelectedDays[0], vmMonth);
    4:
      Scheduler.ViewTimeGrid.Active := True;
    5:
      Scheduler.ViewYear.Active := True;
    6:
      Scheduler.ViewAgenda.Active := True;
  end;
  TMenuItem(Sender).Checked := True;
end;

procedure TDemoBasicMainForm.ViewPositionClick(Sender: TObject);
begin
  with TMenuItem(Sender) do
  begin
    case Tag of
      0: Scheduler.OptionsView.ViewPosition := vpLeft;
      1: Scheduler.OptionsView.ViewPosition := vpRight;
    end;
    Checked := True;
  end;
end;

procedure TDemoBasicMainForm.miAboutClick(Sender: TObject);
begin
  ShowAboutDemoForm;
end;

procedure TDemoBasicMainForm.miGotoDateClick(Sender: TObject);
var
  ADate: TDateTime;
  AMode: TcxSchedulerViewMode;
begin
  ADate := Scheduler.SelectedDays[0];
  if cxShowGoToDateDialog(Scheduler, Scheduler.LookAndFeel, ADate, AMode) then
    Scheduler.GoToDate(ADate, AMode);
end;

procedure TDemoBasicMainForm.FormCreate(Sender: TObject);
begin
  SetDefaultLookAndFeel;
  AddLookAndFeelMenu;
  AddDialogsStyle;
  AnchorDate := Date;
  Resources1.Visible := Scheduler.Storage.ResourceCount > 0;
  miSharing.Visible := Resources1.Visible;
  Scheduler.SelectDays([Date - 1, Date, Date + 1], True);
  WindowState := wsMaximized;
  CheckSchedulerViewStyle;
end;

const
  LastCount: Integer = -1;

procedure TDemoBasicMainForm.Timer1Timer(Sender: TObject);
var
  ACount: Integer;
const
  BarCaption = 'Data Storage contains %d events';
begin
  if csDestroying in ComponentState then Exit;  
  if Scheduler.Storage <> nil then
    ACount := Scheduler.Storage.EventCount
  else
    ACount := 0;
  if LastCount <> ACount then
  begin
    LastCount := ACount;
    StatusBar.SimpleText := Format(BarCaption, [ACount]);
  end;
end;

procedure TDemoBasicMainForm.Resourcelayout1Click(Sender: TObject);
begin
  if Scheduler.Storage <> nil then
    cxShowResourcesLayoutEditor(Scheduler.Storage, Scheduler.LookAndFeel);
end;

procedure TDemoBasicMainForm.miGroupByClick(Sender: TObject);
begin
  TMenuItem(Sender).Checked := not TMenuItem(Sender).Checked;
  case TMenuItem(Sender).Tag of
    0:
      Scheduler.OptionsView.GroupingKind := gkNone;
    1:
      Scheduler.OptionsView.GroupingKind := gkByResource;
    2:
      Scheduler.OptionsView.GroupingKind := gkByDate;
  end;
end;

procedure TDemoBasicMainForm.ExportToClick(Sender: TObject);

  procedure ExportToFile(AType: Integer);
  begin
    if SaveDialog.Execute then
    begin
      case AType of
        1:
          cxExportSchedulerToExcel(SaveDialog.FileName, Scheduler);
        2:
          cxExportSchedulerToText(SaveDialog.FileName, Scheduler);
        3:
          cxExportSchedulerToHTML(SaveDialog.FileName, Scheduler);
        4:
          cxExportSchedulerToXML(SaveDialog.FileName, Scheduler);
        5:
          cxExportSchedulerToXLSX(SaveDialog.FileName, Scheduler);
      end;
    end;
  end;

begin
  case (Sender as TMenuItem).Tag of
    0: cxSchedulerExportToOutlook(Scheduler.Storage);
    1..5:
      ExportToFile((Sender as TMenuItem).Tag);
  end;
end;

procedure TDemoBasicMainForm.SyncClick(Sender: TObject);
begin
  if (Sender as TMenuItem).Tag = 0 then
    cxSchedulerSynchronizeStorageWithOutlook(Scheduler.Storage, GetSyncIDField)
  else
    cxSchedulerSynchronizeOutlookWithStorage(Scheduler.Storage, GetSyncIDField);
end;

procedure TDemoBasicMainForm.miResCountClick(Sender: TObject);
begin
  Scheduler.OptionsView.ResourcesPerPage := TMenuItem(Sender).Tag;
  if TMenuItem(Sender).Tag = 0 then
    Scheduler.ResourceNavigator.Visibility := snvNever
  else
    Scheduler.ResourceNavigator.Visibility := snvAlways
end;

procedure TDemoBasicMainForm.miTimeWorktimeonlyClick(Sender: TObject);
begin
  TMenuItem(Sender).Checked := not TMenuItem(Sender).Checked;
  Scheduler.ViewTimeGrid.WorkTimeOnly := TMenuItem(Sender).Checked;
  Scheduler.ViewTimeGrid.WorkDaysOnly := TMenuItem(Sender).Checked;
end;

procedure TDemoBasicMainForm.miDaySettingsClick(Sender: TObject);
var
  AChecked: Boolean;
begin
  TMenuItem(Sender).Checked := not TMenuItem(Sender).Checked;
  AChecked := TMenuItem(Sender).Checked;
  case TMenuItem(Sender).Tag of
    0:
     Scheduler.ViewDay.HeaderContainer := AChecked;
    1:
     Scheduler.ViewDay.AlwaysShowEventTime := AChecked;
    2:
     Scheduler.ViewDay.WorkTimeOnly := AChecked;
    3:
     Scheduler.ViewDay.TimeRulerMinutes := AChecked;
  end;
end;

procedure TDemoBasicMainForm.miWeekViewClick(Sender: TObject);
begin
  TMenuItem(Sender).Checked := not TMenuItem(Sender).Checked;
  if TMenuItem(Sender).Tag = 0 then
    Scheduler.ViewWeek.CompressWeekEnd := TMenuItem(Sender).Checked
  else
  begin
    if TMenuItem(Sender).Checked then
      Scheduler.ViewWeek.DaysLayout := wdlOneColumn
    else
      Scheduler.ViewWeek.DaysLayout := wdlTwoColumns;
  end;
end;

procedure TDemoBasicMainForm.miCompressweekendsClick(Sender: TObject);
begin
  TMenuItem(Sender).Checked := not TMenuItem(Sender).Checked;
  Scheduler.ViewWeeks.CompressWeekEnd := TMenuItem(Sender).Checked;
end;

procedure TDemoBasicMainForm.AlldayeventsonlyClick(Sender: TObject);
begin
  TMenuItem(Sender).Checked := not TMenuItem(Sender).Checked;
  Scheduler.ViewYear.AllDayEventsOnly := TMenuItem(Sender).Checked;
end;

procedure TDemoBasicMainForm.miSharingClick(Sender: TObject);
begin
  TMenuItem(Sender).Checked := not TMenuItem(Sender).Checked;
  Scheduler.EventOperations.SharingBetweenResources := TMenuItem(Sender).Checked;
end;

procedure TDemoBasicMainForm.miIntersectionClick(Sender: TObject);
begin
  TMenuItem(Sender).Checked := not TMenuItem(Sender).Checked;
  Scheduler.EventOperations.Intersection := TMenuItem(Sender).Checked;
end;

procedure TDemoBasicMainForm.miWeekCompressWeekendsClick(Sender: TObject);
begin
  TMenuItem(Sender).Checked := not TMenuItem(Sender).Checked;
  Scheduler.ViewWeek.CompressWeekEnd := TMenuItem(Sender).Checked;
end;

procedure TDemoBasicMainForm.miWeekViewHideWeekendClick(Sender: TObject);
begin
  Scheduler.ViewWeek.HideWeekEnd := miWeekViewHideWeekend.Checked;
  Scheduler.ViewWeeks.HideWeekEnd := miMonthViewHideWeekEnd.Checked;
  miWeekViewHideWeekend.Checked := Scheduler.ViewWeek.HideWeekEnd;
  miMonthViewHideWeekEnd.Checked := Scheduler.ViewWeeks.HideWeekEnd;
end;

procedure TDemoBasicMainForm.mi_dedUnlimitedClick(Sender: TObject);
const
  AMode: array[0..2] of TcxSchedulerAgendaViewDisplayMode = (avmAllDays, avmSelectedDays, avmSelectedNonEmptyDays);
begin
  TMenuItem(Sender).Checked := True;
  Scheduler.ViewAgenda.DisplayMode := AMode[TMenuItem(Sender).Tag];
end;

procedure TDemoBasicMainForm.mi_dhoHorizontalClick(Sender: TObject);
const
  AOrientation: array[Boolean] of TcxSchedulerAgendaViewDayHeaderOrientation = (dhoHorizontal, dhoVertical);
begin
  TMenuItem(Sender).Checked := not TMenuItem(Sender).Checked;
  Scheduler.ViewAgenda.DayHeaderOrientation := AOrientation[mi_dhoVertical.Checked];
end;

procedure TDemoBasicMainForm.miSelectOnRightClickClick(Sender: TObject);
begin
  Scheduler.OptionsBehavior.SelectOnRightClick := not Scheduler.OptionsBehavior.SelectOnRightClick;
  miSelectOnRightClick.Checked := Scheduler.OptionsBehavior.SelectOnRightClick;
end;

procedure TDemoBasicMainForm.miShowEventsClick(Sender: TObject);
begin
  Scheduler.ViewDay.ShowAllDayEventsInContentArea := not Scheduler.ViewDay.ShowAllDayEventsInContentArea;
  miShowEvents.Checked := not Scheduler.ViewDay.ShowAllDayEventsInContentArea;
end;

procedure TDemoBasicMainForm.miAllDayScrollClick(Sender: TObject);
begin
  Scheduler.ViewDay.AllDayAreaScrollBar := TcxSchedulerAllDayAreaScrollBar(TMenuItem(Sender).Tag);
  TMenuItem(Sender).Checked := True;
end;

procedure TDemoBasicMainForm.miDayHeaderAreaClick(Sender: TObject);
begin
  Scheduler.ViewDay.DayHeaderArea := not Scheduler.ViewDay.DayHeaderArea;
  miDayHeaderArea.Checked := Scheduler.ViewDay.DayHeaderArea;
end;

procedure TDemoBasicMainForm.miDayHeaderModernDisplayModeClick(Sender: TObject);
begin
  TMenuItem(Sender).Checked := True;
  Scheduler.ViewDay.DayHeaderModernStyleDisplayMode := TcxSchedulerDayHeaderModernStyleDisplayMode(TMenuItem(Sender).Tag);
end;

procedure TDemoBasicMainForm.AllDayAreaHeightClick(Sender: TObject);
begin
  cxSchedulerAllDayEventContainerMaxLineCount := TMenuItem(Sender).Tag;
  TMenuItem(Sender).Checked := True;
  Scheduler.CurrentView.Refresh;
end;

procedure TDemoBasicMainForm.AddLookAndFeelMenu;
begin
  SkinDemoUtils.CreateSkinsMenuItem(mmMain, mmMain.Items.IndexOf(miAbout));
end;

function TDemoBasicMainForm.GetDefaultLookAndFeelKind: TcxLookAndFeelKind;
begin
  Result := lfOffice11;
end;

function TDemoBasicMainForm.IsNativeDefaultStyle: Boolean;
begin
  Result := False;
end;

procedure TDemoBasicMainForm.SetDefaultLookAndFeel;
begin
  lfController.NativeStyle := IsNativeDefaultStyle;
  lfController.Kind := GetDefaultLookAndFeelKind;
end;

procedure TDemoBasicMainForm.AgendaSettingsClick(Sender: TObject);
var
  AChecked: Boolean;
begin
  TMenuItem(Sender).Checked := not TMenuItem(Sender).Checked;
  AChecked := TMenuItem(Sender).Checked;
  case TMenuItem(Sender).Tag of
    0:
     Scheduler.ViewAgenda.ShowLocations := AChecked;
    1:
     Scheduler.ViewAgenda.ShowResources := AChecked;
    2:
     Scheduler.ViewAgenda.ShowTimeAsClock := AChecked;
  end;
end;

end.

