unit SchedulerRLMain;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, Menus, Graphics, StdCtrls, dxPSCore,
  dxPSGlbl, dxPSUtl, cxSchedulerStorage, cxControls, cxSchedulerCustomControls, cxScheduler,
  dxPScxCommon, {dxPScxSchedulerLnk,} ComCtrls, ShellApi,
  cxSchedulerUtils, ToolWin, Controls, ImgList, dxPScxSchedulerLnk,
  cxStyles, cxGraphics, cxEdit, cxSchedulerCustomResourceView,
  cxSchedulerDayView, cxSchedulerDateNavigator, cxSchedulerHolidays,
  cxSchedulerTimeGridView, cxSchedulerWeekView, cxSchedulerYearView,
  cxSchedulerGanttView, dxPSEngn, dxPrnPg, dxBkgnd, dxWrap, dxPrnDev,
  dxPSCompsProvider, dxPSFillPatterns, dxPSEdgePatterns, cxLookAndFeels,
  cxLookAndFeelPainters, dxPSPDFExportCore, dxPSPDFExport, cxDrawTextUtils,
  dxPSPrVwStd, dxPScxEditorProducers, dxPScxExtEditorProducers,
  dxPScxPageControlProducer, DemoBasicMain, ActnList;

type
  TSchedulerRLMainForm = class(TDemoBasicMainForm)
    Storage: TcxSchedulerStorage;
    Scheduler: TcxScheduler;
    dxComponentPrinterLink1: TcxSchedulerReportLink;
    tbtnEvents: TToolButton;
    actEvents: TAction;
    N1: TMenuItem;
    miEvents: TMenuItem;
    procedure tbnGenerateClick(Sender: TObject);
  private
    procedure GenerateRandomEvents(ACount: Integer; ARandomResource: Boolean = False);
    function GetRandomCaption: string;
    function GetRandomDate: TDateTime;
    function GetRandomState: Integer;
    function GetRandomResourceID: Variant;
    function GetRandomLabelColor: TColor;
  public
    { Public declarations }
  end;

var
  SchedulerRLMainForm: TSchedulerRLMainForm;

implementation


{$R *.DFM}

const
  Captions: array[0..22] of string =
   ('Men''s Track and Field',
    '500 meter Men''s Speed Skating',
    '100 Meter Individual Medley',
    'Women''s Basketball Finals',
    'Women''s Cycling 20mi',
    'Men''s Gymnastics Finals',
    'Women''s Hockey Finals',
    'Women''s High Dive Finals',
    'Karate Finals',
    'Kayak Level IV Finals',
    'Lacrosse Semi-finals',
    'Polo Finals',
    'Women''s Soccer Finals',
    'Surfing:  Freestyle Finals',
    'Swimming: 100 m Finals',
    'Track & Field:  Women''s Finals',
    'Archery Finals',
    'Men''s Hockey Finals',
    'Men''s Downhill Giant Solom',
    'Men''s Ski Jump Finals',
    'Women''s Downhill Ski Finals',
    'Men''s Relay',
    'Baseball Quarter-finals');

procedure TSchedulerRLMainForm.GenerateRandomEvents(ACount: Integer;
  ARandomResource: Boolean = False);
var
  I: Integer;
  ADate: TDateTime;
  AStorage: TcxCustomSchedulerStorage;
begin
  AStorage := Scheduler.Storage;
  if AStorage = nil then Exit;
  AStorage.BeginUpdate;
  try
    Randomize;
    for I := 0 to ACount div 5 * 4 - 1 do
      with AStorage.CreateEvent do
      begin
        ADate := GetRandomDate;
        Start := ADate;
        Finish := ADate + Random(180) * MinuteToTime;
        State := GetRandomState;
        LabelColor := EventLabelColors[Random(11)];
        Caption := GetRandomCaption;
        if ARandomResource then
          ResourceID := GetRandomResourceID;
      end;
    for I := 0 to ACount div 5 - 1 do
      with AStorage.CreateEvent do
      begin
        ADate := GetRandomDate;
        Start := ADate;
        Finish := ADate + 1;
        AllDayEvent := True;
        LabelColor := EventLabelColors[Random(11)];
        Caption := GetRandomCaption;
        if ARandomResource then
          ResourceID := GetRandomResourceID;
      end;
  finally
    AStorage.EndUpdate;
  end;
end;

function TSchedulerRLMainForm.GetRandomCaption: string;
begin
  Result := Captions[Random(Length(Captions))];
end;

function TSchedulerRLMainForm.GetRandomDate: TDateTime;
begin
  Result := Date + Random(60) - 30 + Scheduler.OptionsView.WorkStart;
  if Trunc(Result) = Date then
    Result := Result + Random(10) * HourToTime
  else
    Result := Result + Random(24) * HourToTime;
end;

function TSchedulerRLMainForm.GetRandomLabelColor: TColor;
begin
  Result := EventLabelColors[Random(11)];
end;

function TSchedulerRLMainForm.GetRandomResourceID: Variant;
begin
  if (Scheduler.Storage = nil) or (Scheduler.Storage.ResourceCount = 0) then
    Exit
  else
    if Scheduler.Storage.ResourceCount = 1 then
      Result := Scheduler.Storage.ResourceIDs[0]
    else
      Result := Scheduler.Storage.ResourceIDs[Random(Scheduler.Storage.ResourceCount)];
end;

function TSchedulerRLMainForm.GetRandomState: Integer;
begin
  Result := Random(4);
end;


procedure TSchedulerRLMainForm.tbnGenerateClick(Sender: TObject);
begin
  GenerateRandomEvents(100);
end;

end.
