unit DBDemoMainUnit;

interface

{$I cxVer.inc}

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Windows, Messages, SysUtils, Variants, Classes,
  Graphics, Controls, Forms, Dialogs, DemoBasicMain, cxStyles, cxGraphics, cxEdit,
  cxScheduler, cxSchedulerCustomControls, cxSchedulerCustomResourceView,
  cxSchedulerDayView, cxSchedulerDateNavigator, DB, cxSchedulerStorage,
  cxSchedulerDBStorage, cxLookAndFeels, Menus, StdCtrls,
  ExtCtrls, cxControls, ComCtrls, cxSchedulerWeekView, Math, 
  cxSchedulerTimeGridView, cxSchedulerUtils, cxSchedulerYearView,
  cxLookAndFeelPainters, cxContainer, cxCheckBox, cxButtons,
  cxSchedulerHolidays, cxSchedulerGanttView, cxGroupBox, dxmdaset, cxSchedulerAgendaView;

type
  TDBDemoMainForm = class(TDemoBasicMainForm)
    SchedulerDataSource: TDataSource;
    SchedulerDBStorage: TcxSchedulerDBStorage;
    cxButton1: TcxButton;
    cxCheckBox1: TcxCheckBox;
    cxGroupBox1: TcxGroupBox;
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
    procedure FormCreate(Sender: TObject);
    procedure chDataModeClick(Sender: TObject);
    procedure cxButton1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FEventCount: Integer;
    FMaxID: Integer;
  public
    { Public declarations }
  end;

var
  DBDemoMainForm: TDBDemoMainForm;

implementation

{$R *.dfm}

procedure TDBDemoMainForm.FormCreate(Sender: TObject);
var
  ADate: TDateTime;
  I: Integer;
begin
  inherited;
  mdEvents.LoadFromBinaryFile('..\..\Data\cxSchedulerEvents.dat');
  mdEvents.Open;
  ADate := EncodeDate(2016, 11, 30);
  Scheduler.GotoDate(ADate);
  ADate := ADate + Scheduler.OptionsView.WorkStart;
  Scheduler.SelectTime(ADate, ADate, nil);
  FMaxID := 0;
  FEventCount := SchedulerDBStorage.EventCount;
  for I := 0 to FEventCount - 1 do
    FMaxID := Max(FMaxID, SchedulerDBStorage.Events[I].ID);
end;

procedure TDBDemoMainForm.chDataModeClick(Sender: TObject);
begin
  SchedulerDBStorage.SmartRefresh := (Sender as TcxCheckBox).Checked;
end;

procedure TDBDemoMainForm.cxButton1Click(Sender: TObject);
begin
  MaxRandomPeriod := 720;
  GenerateRandomEvents(5000);
  MaxRandomPeriod := 180;
end;

procedure TDBDemoMainForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
var
  I: Integer;
begin
  if (FEventCount < SchedulerDBStorage.EventCount) and
    (MessageDlg('Do you want to delete previously created events?', mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
  begin
    SchedulerDBStorage.BeginUpdate;
    try
      I := 0;
      while I < SchedulerDBStorage.EventCount do
      begin
        if SchedulerDBStorage.Events[I].ID > FMaxID then
          SchedulerDBStorage.Events[I].Delete
        else
          Inc(I);
      end;
    finally
      SchedulerDBStorage.EndUpdate;
    end;
  end;
end;

end.
