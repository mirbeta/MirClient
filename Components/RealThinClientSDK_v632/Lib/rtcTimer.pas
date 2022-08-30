{
  @html(<b>)
  Timer
  @html(</b>)
  - Copyright 2004-2014 (c) RealThinClient.com (http://www.realthinclient.com)
  @html(<br><br>)

  Thread-safe cross-platform timer class implemented using 2 background threads
  (one for background thread events, one for main thread events).
  @html(<br>)

  This class is used internally by TRtcConnection and all its descendant classes
  to implement the timeout, reconnect and restart functionality.
}
unit rtcTimer;

{$INCLUDE rtcDefs.inc}

interface

uses
{$IFDEF WINDOWS}
  Windows,
  Messages,
{$ENDIF}

  Classes,
  SysUtils,

  memBinList,
  memObjList,

  rtcTypes,
  rtcSyncObjs,
  rtcThrPool,
  rtcLog;

var
  LOG_TIMER_EXCEPTIONS:boolean={$IFDEF RTC_DEBUG}True{$ELSE}False{$ENDIF};

type
  // @Abstract(Events used by RTC Timer)
  TRtcTimerEvent = procedure of object;

  { @Abstract(RTC Timer class)

    This class ensures a Thread-Safe Timer by
    using the RTC Window Handle Pool and RTC Thread Pool
    instead of the TTimer class implementation. }
  TRtcTimer = class(TObject)
  private
    FAutoDisable:boolean;
    FAutoDestroy:boolean;
    FEvent:TRtcTimerEvent;
    FThr:TRtcThread;
    FJob:TObject;

    FBackThread:boolean;

    FActive:boolean;
    FInterval:Cardinal;
    FLastTrigger:Cardinal;

  protected
    { For internal use only!!!
      Called by the framework to call the Event for this timer.
      If the timer is still active after executing the event, returns interval until next trigger.
      If the timer was disabled and/or destroyed, returns 0 (zero).
      @exclude }
    class procedure Timer(me:TObject);

    property LastTrigger:Cardinal read FLastTrigger write FLastTrigger;

  public
    // Create a Timer. To start the timer, use the @Link(Enable) method.
    constructor Create(Multi_Threaded:boolean); virtual;

    { @exclude }
    destructor Destroy; override;

    { Allways use Stop instead of Free or Destroy! }
    class procedure Stop(var me);

    { Disable the Timer }
    class procedure Disable(me:TObject);

    { Enable the Timer to trigger 'Event' every 'Wait' miliseconds.
      If AutoDisable is True, Timer will be automatically disabled after the event has been triggered.
      If AutoDestroy is True, Timer object will be automaticaly destroyed after the event has been triggered. }
    class procedure Enable(me:TObject; Wait:Cardinal; Event:TRtcTimerEvent; AutoDisable:boolean=False; AutoDestroy:boolean=False); overload;

    { Enable the Timer to post the 'Job' to Thread 'Thr' every 'Wait' miliseconds.
      If AutoDisable is True, Timer will be automatically disabled after the event has been triggered.
      If AutoDestroy is True, Timer object will be automaticaly destroyed after the event has been triggered. }
    class procedure Enable(me:TObject; Wait:Cardinal; Thr:TRtcThread; Job:TObject; AutoDisable:boolean=False; AutoDestroy:boolean=False); overload;

    { Reset elapsed time counter.
      This will make the Timer skip one upcoming event. }
    class procedure Reset(me:TObject);
    end;

implementation

var
  TimList:tBinList;
  TimCS:TRtcCritSec;
  _Outside:TRtcEvent;

procedure CloseTimerPool;
  begin
  TimCS.Acquire;
  try
    RtcFreeAndNil(TimList);
  finally
    TimCS.Release;
    end;
  end;

function rtcStoreTimer(const obj):boolean;
  begin
  Result:=False;
  if not assigned(TimCS) then Exit;

  TimCS.Acquire;
  try
    if not assigned(TimList) then
      TimList:=tBinList.Create(128);

    if assigned(TimList) then
      if TimList.search(RtcIntPtr(Obj))=0 then
        begin
        Result:=True;
        TimList.insert(RtcIntPtr(Obj), 1);
        end;
  finally
    TimCS.Release;
    end;
  end;

function rtcRemoveTimer(const obj):boolean;
  begin
  Result:=False;
  if not assigned(TimCS) then Exit;

  TimCS.Acquire;
  try
    if assigned(TimList) then
      if TimList.search(RtcIntPtr(Obj))>0 then
        begin
        TimList.remove(RtcIntPtr(Obj));
        Result:=True;
        end;
  finally
    TimCS.Release;
    end;
  end;

function rtcEnterTimer(ID:RtcIntPtr):boolean;
  begin
  Result:=False;
  if not assigned(TimCS) then Exit;

  TimCS.Acquire;
  try
    if assigned(TimList) then
      if TimList.search(ID)>0 then
        Result:=True;
  finally
    if not Result then TimCS.Release;
    end;
  end;

procedure rtcLeaveTimer;
  begin
  if not assigned(TimCS) then Exit;
  TimCS.Release;
  end;

const
  RTC_TIMER_PRECISION=50;

var
  AppStartTime:TDateTime;
  TimerThreads:integer;

type
  TRtcTimerThread=class(TThread)
  private
    FSyncMode: boolean;
    FFinished: boolean;

    procedure SetFinished(const Value: boolean);

  protected
    FTriggers:tObjList;
    FTimers:tBinList;
    FEvent:TRtcEvent;
    FCS:TRtcCritSec;

    Trig:tBinList;
    MyTime:Cardinal;
    ENext:Cardinal;

    procedure RemoveAllTriggers;
    procedure ExecuteTimer;

  public
    constructor Create(CreateSuspended:boolean);
    destructor Destroy; override;

    procedure Execute; override;

    procedure AddTrigger(Tim:TRtcTimer; TriggerTime:Cardinal);
    procedure RemoveTrigger(Tim:TRtcTimer);
    procedure ChangeTrigger(Tim:TRtcTimer; TriggerTime:Cardinal);

    property Finished:boolean read FFinished write SetFinished;
    property SyncMode:boolean read FSyncMode write FSyncMode;
    end;

function GetRunTime:Cardinal;
  begin
  Result:=Cardinal(trunc((Now-AppStartTime)*24*60*60*1000/RTC_TIMER_PRECISION));
  end;

{ TRtcTimerThread }

constructor TRtcTimerThread.Create(CreateSuspended: boolean);
  begin
  FFinished:=False;
  FreeOnTerminate:=True;

  FCS:=TRtcCritSec.Create;
  FTriggers:=tObjList.Create(64);
  FEvent:=TRtcEvent.Create(True,False);

  TimCS.Acquire;
  try
    Inc(TimerThreads);
    if TimerThreads=1 then
      _Outside.ResetEvent;
  finally
    TimCS.Release;
    end;

  inherited Create(CreateSuspended);
  end;

destructor TRtcTimerThread.Destroy;
  begin
  RemoveAllTriggers;

  RtcFreeAndNil(FTriggers);
  RtcFreeAndNil(FEvent);
  RtcFreeAndNil(FCS);

  TimCS.Acquire;
  try
    Dec(TimerThreads);
    if TimerThreads=0 then
      _Outside.SetEvent;
  finally
    TimCS.Release;
    end;

  inherited;
  end;

procedure TRtcTimerThread.ExecuteTimer;
  var
    Tmp,Tmp2:RtcIntPtr;
  begin
  repeat
    Tmp2:=Trig.search_min(Tmp);
    if Tmp2>0 then
      begin
      Trig.remove(Tmp2);
      if rtcEnterTimer(Tmp2) then
        TRtcTimer.Timer(TRtcTimer(Tmp2));
      end
    else
      Break;
    until FFinished;
  end;

procedure TRtcTimerThread.Execute;
  var
    NextTrigger:RtcIntPtr;
    Tmp:TObject;
    SleepTime:Cardinal;
  begin
  Trig:=nil;
  SleepTime:=0;

  repeat
    MyTime:=GetRunTime;

    FCS.Acquire;
    try
      NextTrigger:=FTriggers.search_min(Tmp);
      if NextTrigger<=0 then // Nobody waiting
        begin
        FEvent.ResetEvent;
        SleepTime:=WAIT_INFINITE;
        end
      else if NextTrigger<=MyTime then // Triggers ready for execution
        begin
        Trig:=tBinList(Tmp);
        FTriggers.remove(NextTrigger);
        end
      else
        begin
        FEvent.ResetEvent;
        SleepTime:=Cardinal(NextTrigger-MyTime) * RTC_TIMER_PRECISION;
        end;
    finally
      FCS.Release;
      end;

    if not FFinished then
      if assigned(Trig) then
        begin
        try
          if SyncMode then
            begin
            if assigned(rtcSyncProc) then
              rtcSyncProc(ExecuteTimer)
            else
              Synchronize(ExecuteTimer);
            end
          else
            ExecuteTimer;
        finally
          RtcFreeAndNil(Trig);
          end;
        end
      else
        FEvent.WaitFor(SleepTime);
    until FFinished;

  if assigned(Trig) then
    RtcFreeAndNil(Trig);
  end;

procedure TRtcTimerThread.SetFinished(const Value: boolean);
  begin
  FFinished := Value;
  if Value then
    begin
    RemoveAllTriggers;
    FEvent.SetEvent;
    end;
  end;

procedure TRtcTimerThread.RemoveAllTriggers;
  var
    Tmp2:RtcIntPtr;
    Tmp:TObject;
    bl:tBinList;
  begin
  FCS.Acquire;
  try
    repeat
      Tmp2:=FTriggers.search_min(Tmp);
      if Tmp2>0 then
        begin
        bl:=TBinList(Tmp);
        FTriggers.remove(Tmp2);
        bl.Free;
        end
      else
        Break;
      until False;
  finally
    FCS.Release;
    end;
  end;

procedure TRtcTimerThread.AddTrigger(Tim: TRtcTimer; TriggerTime: Cardinal);
  var
    Tmp:TObject;
    bl:tBinList;
  begin
  TriggerTime := GetRunTime + TriggerTime;

  FCS.Acquire;
  try
    if Tim.FActive then
      begin
      Tim.LastTrigger:=TriggerTime;

      Tmp:=FTriggers.search(TriggerTime);
      if assigned(Tmp) then
        begin
        bl:=TBinList(Tmp);
        bl.insert(RtcIntPtr(Tim),1);
        end
      else
        begin
        bl:=tBinList.Create(8);
        bl.insert(RtcIntPtr(Tim),1);
        FTriggers.insert(TriggerTime,bl);
        if FTriggers.search_min(Tmp)=TriggerTime then
          FEvent.SetEvent;
        end;
      end;
  finally
    FCS.Release;
    end;
  end;

procedure TRtcTimerThread.RemoveTrigger(Tim: TRtcTimer);
  var
    TriggerTime:Cardinal;
    Tmp:TObject;
    bl:tBinList;
  begin
  FCS.Acquire;
  try
    TriggerTime:=Tim.LastTrigger;
    if TriggerTime>0 then
      begin
      Tim.LastTrigger:=0;

      Tmp:=FTriggers.search(TriggerTime);
      if assigned(Tmp) then
        begin
        bl:=TBinList(Tmp);
        if bl.search(RtcIntPtr(Tim))>0 then
          begin
          bl.remove(RtcIntPtr(Tim));
          if bl.count=0 then
            begin
            FTriggers.remove(TriggerTime);
            bl.Free;
            end;
          end;
        end;
      end;
  finally
    FCS.Release;
    end;
  end;

procedure TRtcTimerThread.ChangeTrigger(Tim: TRtcTimer; TriggerTime: Cardinal);
  var
    OldTriggerTime:Cardinal;
    Tmp:TObject;
    bl:tBinList;
  begin
  TriggerTime := GetRunTime + TriggerTime;

  FCS.Acquire;
  try
    if Tim.FActive then
      begin
      OldTriggerTime:=Tim.LastTrigger;
      if (OldTriggerTime>0) and (OldTriggerTime<>TriggerTime) then
        begin
        Tim.LastTrigger:=TriggerTime;

        Tmp:=FTriggers.search(OldTriggerTime);
        if Assigned(Tmp) then
          begin
          bl:=TBinList(Tmp);
          if bl.search(RtcIntPtr(Tim))>0 then
            begin
            bl.remove(RtcIntPtr(Tim));
            if bl.count=0 then
              begin
              FTriggers.remove(OldTriggerTime);
              bl.Free;
              end;

            Tmp:=FTriggers.search(TriggerTime);
            if Assigned(Tmp) then
              begin
              bl:=TBinList(Tmp);
              bl.insert(RtcIntPtr(Tim),1);
              end
            else
              begin
              bl:=tBinList.Create(8);
              bl.insert(RtcIntPtr(Tim),1);
              FTriggers.insert(TriggerTime,bl);
              if FTriggers.search_min(Tmp)=TriggerTime then
                FEvent.SetEvent;
              end;
            end;
          end;
        end;
      end;
  finally
    FCS.Release;
    end;
  end;

var
  MainTimer:TRtcTimerThread=nil;
  BackTimer:TRtcTimerThread=nil;

constructor TRtcTimer.Create(Multi_Threaded:boolean);
  begin
  inherited Create;
  FBackThread:=Multi_Threaded;

  TimCS.Acquire;
  try
    if FBackThread then
      begin
      if not assigned(BackTimer) then
        BackTimer:=TRtcTimerThread.Create(False);
      end
    else
      begin
      if not assigned(MainTimer) then
        begin
        MainTimer:=TRtcTimerThread.Create(False);
        MainTimer.SyncMode:=True;
        end;
      end;
  finally
    TimCS.Release;
    end;

  FInterval := 0;
  FLastTrigger := 0;

  rtcStoreTimer(self);
  end;

destructor TRtcTimer.Destroy;
  begin
  try
    rtcRemoveTimer(self);
    if FActive then
      begin
      FActive:=False;
      if FBackThread then
        BackTimer.RemoveTrigger(self)
      else
        MainTimer.RemoveTrigger(self);
      end;
    inherited;
  except
    on E:Exception do
      begin
      if LOG_TIMER_EXCEPTIONS then
        Log('TRtcTimer.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

class procedure TRtcTimer.Stop(var me);
  begin
  if pointer(me)<>nil then
    if rtcRemoveTimer(me) then
      RtcFreeAndNil(me);
  end;

class procedure TRtcTimer.Timer(me:TObject);
  var
    FE:TRtcTimerEvent;
    TH:TRtcThread;
    JO:TObject;
    intimer:boolean;
    tim:TRtcTimer;
  begin
  {$IFDEF NEXTGEN} FE:=nil; {$ENDIF}
  intimer:=True;
  try
    tim:=TRtcTimer(me);
    if assigned(tim.FEvent) then
      begin
      if tim.FAutoDisable then
        tim.FActive:=False
      else if tim.FBackThread then
        BackTimer.AddTrigger(tim,tim.FInterval)
      else
        MainTimer.AddTrigger(tim,tim.FInterval);
      FE:=tim.FEvent;
      if tim.FAutoDestroy then
        RtcFreeAndNil(tim)
      else
        tim:=nil;

      rtcLeaveTimer;
      intimer:=False;

      FE;
      end
    else if assigned(tim.FThr) then
      begin
      if tim.FAutoDisable then
        tim.FActive:=False
      else if tim.FBackThread then
        BackTimer.AddTrigger(tim,tim.FInterval)
      else
        MainTimer.AddTrigger(tim,tim.FInterval);
      TH:=tim.FThr;
      JO:=tim.FJob;
      if tim.FAutoDestroy then
        RtcFreeAndNil(tim)
      else
        tim:=nil;

      rtcLeaveTimer;
      intimer:=False;

      TRtcThread.PostJob(TH, JO);
      end
    else // Disable ...
      begin
      tim.FActive:=False;
      if tim.FAutoDestroy then
        RtcFreeAndNil(tim)
      else
        tim:=nil;
      end;
  finally
    if intimer then rtcLeaveTimer;
    end;
  end;

class procedure TRtcTimer.Disable(me:TObject);
  begin
  if pointer(me)<>nil then
    if TRtcTimer(me).FActive then
      begin
      TRtcTimer(me).FActive:=False;
      if TRtcTimer(me).FBackThread then
        BackTimer.RemoveTrigger(TRtcTimer(me))
      else
        MainTimer.RemoveTrigger(TRtcTimer(me));
      end;
  end;

class procedure TRtcTimer.Reset(me:TObject);
  begin
  if pointer(me)<>nil then
    if TRtcTimer(me).FActive then
      if TRtcTimer(me).FBackThread then
        BackTimer.ChangeTrigger(TRtcTimer(me), TRtcTimer(me).FInterval)
      else
        MainTimer.ChangeTrigger(TRtcTimer(me), TRtcTimer(me).FInterval);
  end;

class procedure TRtcTimer.Enable(me:TObject; Wait: Cardinal; Event: TRtcTimerEvent; AutoDisable:boolean=False; AutoDestroy:boolean=False);
  begin
  if pointer(me)<>nil then
    with TRtcTimer(me) do
      begin
      FAutoDisable:=AutoDisable or AutoDestroy;
      FAutoDestroy:=AutoDestroy;
      FThr:=nil;
      FJob:=nil;
      FEvent:=Event;

      FInterval:=Wait div RTC_TIMER_PRECISION;
      if FInterval<1 then FInterval:=1;

      if FActive then
        begin
        if FBackThread then
          BackTimer.ChangeTrigger(TRtcTimer(me), FInterval)
        else
          MainTimer.ChangeTrigger(TRtcTimer(me), FInterval);
        end
      else
        begin
        FActive:=True;
        if FBackThread then
          BackTimer.AddTrigger(TRtcTimer(me), FInterval)
        else
          MainTimer.AddTrigger(TRtcTimer(me), FInterval);
        end;
      end;
  end;

class procedure TRtcTimer.Enable(me:TObject; Wait:Cardinal; Thr:TRtcThread; Job:TObject; AutoDisable:boolean=False; AutoDestroy:boolean=False);
  begin
  if pointer(me)<>nil then
    with TRtcTimer(me) do
      begin
      FAutoDisable:=AutoDisable or AutoDestroy;
      FAutoDestroy:=AutoDestroy;
      FEvent:=nil;
      FThr:=Thr;
      FJob:=Job;

      FInterval:=Wait div RTC_TIMER_PRECISION;
      if FInterval<1 then FInterval:=1;

      if FActive then
        begin
        if FBackThread then
          BackTimer.ChangeTrigger(TRtcTimer(me), FInterval)
        else
          MainTimer.ChangeTrigger(TRtcTimer(me), FInterval);
        end
      else
        begin
        FActive:=True;
        if FBackThread then
          BackTimer.AddTrigger(TRtcTimer(me), FInterval)
        else
          MainTimer.AddTrigger(TRtcTimer(me), FInterval);
        end;
      end;
  end;

procedure WaitForClose(_timeout:cardinal);
{$IFDEF WINDOWS}
  var
    Msg:TMsg;
    MyTime:cardinal;
  begin
  MyTime:=GetTickCount+_Timeout*1000;
  Sleep(10);
  while _Outside.WaitFor(0)<>wr_Signaled do
    begin
    while PeekMessage(Msg,0,0,0,PM_REMOVE) do
      begin
      if (Msg.message=WM_QUIT) then
        Exit
      else
        begin
        TranslateMessage( Msg );
        DispatchMessage( Msg );
        end;
      end;
    Sleep(1);
    if GetTickCount>=MyTime then Exit;
    end;

  { Wait 0.01 seconds to allow the thread to terminate. }

  for MyTime:=1 to 10 do
    begin
    while PeekMessage(Msg,0,0,0,PM_REMOVE) do
      begin
      if (Msg.message=WM_QUIT) then
        Exit
      else
        begin
        TranslateMessage( Msg );
        DispatchMessage( Msg );
        end;
      end;
    Sleep(1);
    end;

  Sleep(10);
  end;
{$ELSE}
  begin
  _Outside.WaitFor(_Timeout*1000);
  Sleep(10);
  end;
{$ENDIF}

initialization
{$IFDEF RTC_DEBUG} Log('rtcTimer Initializing ...','DEBUG');{$ENDIF}

TimerThreads:=0;
MainTimer:=nil;
BackTimer:=nil;
AppStartTime:=Now;

TimCS:=TRtcCritSec.Create;
_Outside:=TRtcEvent.Create(True,True);

{$IFDEF RTC_DEBUG} Log('rtcTimer Initialized.','DEBUG');{$ENDIF}
finalization
{$IFDEF RTC_DEBUG} Log('rtcTimer Finalizing ...','DEBUG');{$ENDIF}

CloseThreadPool;

if assigned(MainTimer) then
  begin
  MainTimer.Finished:=True;
  MainTimer:=nil;
  end;

if assigned(BackTimer) then
  begin
  BackTimer.Finished:=True;
  BackTimer:=nil;
  end;

WaitForClose(10);

CloseTimerPool;
RtcFreeAndNil(TimCS);
RtcFreeAndNil(_Outside);

{$IFDEF RTC_DEBUG} Log('rtcTimer Finalized.','DEBUG');{$ENDIF}
end.
