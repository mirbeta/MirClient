unit StylesMainUnit;

interface

{$I cxVer.inc}

uses
  Windows, Messages, SysUtils, Variants, Classes,
  Graphics, Controls, Forms, Dialogs, DemoBasicMain, cxStyles, cxGraphics,
  cxEdit, cxScheduler, cxSchedulerCustomControls, cxSchedulerCustomResourceView,
  cxSchedulerDayView, cxSchedulerDateNavigator, cxLookAndFeels, Menus,
  StdCtrls, ExtCtrls, cxControls, cxSchedulerStorage, ComCtrls, Buttons,
  cxSchedulerDBStorage, DB, cxSchedulerWeekView,
  cxSchedulerTimeGridView, cxSchedulerUtils, cxSchedulerYearView, Grids,
  DBGrids, cxCustomData, cxFilter, cxData, cxDataStorage, cxDBData, dxmdaset,
  cxLookAndFeelPainters, cxSchedulerHolidays, cxSchedulerGanttView,
  cxClasses, cxSchedulerAgendaView;

type
  TStylesMainForm = class(TDemoBasicMainForm)
    CustomDraw1: TMenuItem;
    DateNavigator1: TMenuItem;
    Days1: TMenuItem;
    Daycaptions1: TMenuItem;
    Monthheaders1: TMenuItem;
    ViewDay1: TMenuItem;
    imeRuler1: TMenuItem;
    Container1: TMenuItem;
    Groupseparator1: TMenuItem;
    Content1: TMenuItem;
    Headers1: TMenuItem;
    Events1: TMenuItem;
    Contentselection1: TMenuItem;
    cxStyleRepository1: TcxStyleRepository;
    stEvents: TcxStyle;
    stHeaders: TcxStyle;
    stContent: TcxStyle;
    stContentSelection: TcxStyle;
    stResources: TcxStyle;
    stGroupSeparator: TcxStyle;
    stContainer: TcxStyle;
    stBackground: TcxStyle;
    stDateContent: TcxStyle;
    stVertSplitter: TcxStyle;
    stTimeRuler: TcxStyle;
    miSplitter: TMenuItem;
    N5: TMenuItem;
    miResourcesStyle: TMenuItem;
    SchedulerDataSource: TDataSource;
    SchedulerDBStorage: TcxSchedulerDBStorage;
    mdEvents: TdxMemData;
    mdEventsID: TAutoIncField;
    mdEventsParentID: TIntegerField;
    mdEventsType: TIntegerField;
    mdEventsStart: TDateTimeField;
    mdEventsFinish: TDateTimeField;
    mdEventsOptions: TIntegerField;
    mdEventsCaption: TStringField;
    mdEventsRecurrenceIndex: TIntegerField;
    mdEventsRecurrenceInfo: TBlobField;
    mdEventsResourceID: TBlobField;
    mdEventsLocation: TStringField;
    mdEventsMessage: TStringField;
    mdEventsReminderDate: TDateTimeField;
    mdEventsReminderMinutes: TIntegerField;
    mdEventsState: TIntegerField;
    mdEventsLabelColor: TIntegerField;
    mdEventsActualStart: TDateTimeField;
    mdEventsActualFinish: TDateTimeField;
    mdEventsSyncIDField: TStringField;
    procedure btnGenerateClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure miStylesItemClick(Sender: TObject);
    procedure miSplitterClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
  protected
    function GetDefaultLookAndFeelKind: TcxLookAndFeelKind; override;
  public
    { Public declarations }
  end;

var
  StylesMainForm: TStylesMainForm;

implementation

{$R *.dfm}

procedure TStylesMainForm.btnGenerateClick(Sender: TObject);
begin
//  GenerateRandomEvents(100, True);
end;

procedure TStylesMainForm.btnDeleteClick(Sender: TObject);
begin
//  SchedulerUnboundStorage.Clear;
end;

procedure TStylesMainForm.miStylesItemClick(Sender: TObject);

  function GetStyle(AAssignedStyle: TcxStyle): TcxStyle;
  begin
    if TMenuItem(Sender).Checked then
      Result := AAssignedStyle
    else
      Result := nil;
  end;

begin
  TMenuItem(Sender).Checked := not TMenuItem(Sender).Checked;
  case TMenuItem(Sender).Tag of
    0: // Events
      Scheduler.Styles.Event := GetStyle(stEvents);
    1: // Headers
      Scheduler.Styles.DayHeader := GetStyle(stHeaders);
    2: // Content
      Scheduler.Styles.Content := GetStyle(stContent);
    3: // Content selection
      Scheduler.Styles.Selection := GetStyle(stContentSelection);
    4: // Resources
      Scheduler.Styles.ResourceHeader := GetStyle(stResources);
    5: // Group separator
      Scheduler.Styles.GroupSeparator := GetStyle(stGroupSeparator);
    6: // DayView container
      Scheduler.ViewDay.Styles.HeaderContainer := GetStyle(stContainer);
    7: // DayView time ruler
      Scheduler.ViewDay.Styles.TimeRuler := GetStyle(stTimeRuler);
    8: // date navigator header
      Scheduler.DateNavigator.Styles.Header := GetStyle(stHeaders);
    9: // date navigator background
      Scheduler.DateNavigator.Styles.Background := GetStyle(stBackground);
    10: // date navigator content
      Scheduler.DateNavigator.Styles.Content := GetStyle(stDateContent);
  end;
 //
end;

procedure TStylesMainForm.miSplitterClick(Sender: TObject);
begin
  miSplitter.Checked := not miSplitter.Checked;
  if miSplitter.Checked then
  begin
    Scheduler.OptionsView.VertSplitterWidth := 50;
    Scheduler.Styles.VertSplitter := stVertSplitter;
  end
  else
  begin
    Scheduler.OptionsView.VertSplitterWidth := cxDefaultSplitterWidth;
    Scheduler.Styles.VertSplitter := nil;
  end
end;

procedure TStylesMainForm.FormCreate(Sender: TObject);
begin
  inherited;
  mdEvents.LoadFromBinaryFile('..\..\Data\cxSchedulerEvents.dat');
  mdEvents.Open;
  Scheduler.GoToDate(EncodeDate(2016, 11, 30));
end;

function TStylesMainForm.GetDefaultLookAndFeelKind: TcxLookAndFeelKind;
begin
  Result := lfUltraFlat;
end;

end.
