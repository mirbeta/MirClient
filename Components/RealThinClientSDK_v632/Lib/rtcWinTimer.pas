{
  @html(<b>)
  WinTimer
  @html(</b>)
  - Copyright 2004-2014 (c) RealThinClient.com (http://www.realthinclient.com)
  @html(<br><br>)

  Thread-safe Windows-specific Timer class, tightly coupled
  with the RTC Window Handle and Thread Pool mechanisms.
  @html(<br>)

  This class is used internally by TRtcConnection and all its descendant classes
  to implement the timeout, reconnect and restart functionality.
}
unit rtcWinTimer;

{$INCLUDE rtcDefs.inc}

{$IFNDEF WINDOWS}
{$MESSAGE ERROR "This unit is ONLY for Windows!"}
{$ENDIF}

interface

uses
  Classes,

{$IFDEF WINDOWS}
  Windows,
  Messages,
  rtcHWndPool,
{$ENDIF}

  SysUtils,
  
  rtcTypes,
  rtcSyncObjs,
  rtcThrPool,
  rtcLog;

var
  LOG_WINTIMER_EXCEPTIONS:boolean={$IFDEF RTC_DEBUG}True{$ELSE}False{$ENDIF};

type
  // @Abstract(Events used by RTC Timer)
  TRtcWinTimerEvent = procedure of object;

  { @Abstract(RTC Timer class)

    This class ensures a Thread-Safe Timer by
    using the RTC Window Handle Pool and RTC Thread Pool
    instead of the TTimer class implementation. }
  TRtcWinTimer = class(TObject)
  private
    FRunning:boolean;
    FAutoDisable:boolean;
    FAutoDestroy:boolean;
  {$IFDEF WINDOWS}
    FHandle:HWND;
  {$ENDIF}
    FEvent:TRtcWinTimerEvent;
    FThr:TRtcThread;
    FJob:TObject;

    FInterval:Cardinal;
    FNextTrigger:Cardinal;

    function MyTimerCheck:boolean;
    procedure MyTimerReset;
    procedure MyTimerDisable;
    procedure MyTimerEnable;

  protected
    { For internal use only!!!
      Called by the framework to call the Event for this timer.
      @exclude }
    class procedure Timer(me:TObject);

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
    class procedure Enable(me:TObject; Wait:Cardinal; Event:TRtcWinTimerEvent; AutoDisable:boolean=False; AutoDestroy:boolean=False); overload;

    { Enable the Timer to post the 'Job' to Thread 'Thr' every 'Wait' miliseconds.
      If AutoDisable is True, Timer will be automatically disabled after the event has been triggered.
      If AutoDestroy is True, Timer object will be automaticaly destroyed after the event has been triggered. }
    class procedure Enable(me:TObject; Wait:Cardinal; Thr:TRtcThread; Job:TObject; AutoDisable:boolean=False; AutoDestroy:boolean=False); overload;

    { Reset elapsed time counter.
      This will make the Timer skip one upcoming event. }
    class procedure Reset(me:TObject);
    end;

implementation

uses
  memBinList;

var
  TimerList:tBinList;
  CS:TRtcCritSec;

procedure CloseTimerPool;
  begin
  CS.Acquire;
  try
    RtcFreeAndNil(TimerList);
  finally
    CS.Release;
    end;
  end;

function rtcStoreTimer(var obj):boolean;
  begin
  Result:=False;
  if not assigned(CS) then Exit;

  CS.Acquire;
  try
    if not assigned(TimerList) then
      TimerList:=tBinList.Create(128);

    if assigned(TimerList) then
      if TimerList.search(RtcIntPtr(Obj))=0 then
        begin
        Result:=True;
        TimerList.insert(RtcIntPtr(Obj), 1);
        end;
  finally
    CS.Release;
    end;
  end;

function rtcRemoveTimer(var obj):boolean;
  begin
  Result:=False;
  if not assigned(CS) then Exit;

  CS.Acquire;
  try
    if assigned(TimerList) then
      if TimerList.search(RtcIntPtr(Obj))>0 then
        begin
        TimerList.remove(RtcIntPtr(Obj));
        Result:=True;
        end;
  finally
    CS.Release;
    end;
  end;

function rtcGetTimer(ID:RtcIntPtr):TRtcWinTimer;
  begin
  Result:=nil;
  if not assigned(CS) then Exit;

  CS.Acquire;
  try
    if assigned(TimerList) then
      if TimerList.search(ID)>0 then
        Result:=TRtcWinTimer(ID);
  finally
    if Result=nil then
      CS.Release;
    end;
  end;

function rtcEnterTimer(ID:RtcIntPtr):boolean;
  begin
  Result:=False;
  if not assigned(CS) then Exit;

  CS.Acquire;
  try
    if assigned(TimerList) then
      if TimerList.search(ID)>0 then
        Result:=True;
  finally
    if not Result then CS.Release;
    end;
  end;

procedure rtcLeaveTimer;
  begin
  if not assigned(CS) then Exit;
  CS.Release;
  end;

constructor TRtcWinTimer.Create(Multi_Threaded:boolean);
  begin
  inherited Create;
  FRunning:=False;
  FInterval := 0;
  FNextTrigger := 0;

{$IFDEF WINDOWS}
  FHandle := rtcGetHWND(Multi_Threaded);
{$ELSE}
  {$MESSAGE WARN 'TRtcWinTimer.Create -> Timer create not implemented.'}
{$ENDIF}
  rtcStoreTimer(self);
  end;

destructor TRtcWinTimer.Destroy;
  begin
  try
    rtcRemoveTimer(self);
    MyTimerDisable;
  {$IFDEF WINDOWS}
    // rtcReturnHWND(FHandle);
  {$ELSE}
    {$MESSAGE WARN 'TRtcWinTimer.Destroy -> Timer destroy not implemented.'}
  {$ENDIF}
    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcWinTimer.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

function TRtcWinTimer.MyTimerCheck: boolean;
  begin
{$IFDEF WINDOWS}
  Result:=FRunning and (GetTickCount>=FNextTrigger);
{$ELSE}
  Result:=True;
  {$MESSAGE WARN 'TRtcWinTimer.MyTimerCheck -> Check if timer has to trigger not implemented'}
{$ENDIF}
  end;

procedure TRtcWinTimer.MyTimerEnable;
  begin
  if not FRunning then
    begin
  {$IFDEF WINDOWS}
    FNextTrigger:=GetTickCount+FInterval;
    if SetTimer(FHandle, RtcIntPtr(self), FInterval, nil) = 0 then
      raise EOutOfResources.Create('No more timers available.');
  {$ELSE}
    {$MESSAGE WARN 'TRtcWinTimer.MyTimerEnable -> Reset counter and Enable Timer not implemented'}
    raise EOutOfResources.Create('Timers not available for this OS.');
  {$ENDIF}
    FRunning:=True;
    end;
  end;

procedure TRtcWinTimer.MyTimerReset;
  begin
{$IFDEF WINDOWS}
  FNextTrigger:=GetTickCount+FInterval;
{$ELSE}
  {$MESSAGE WARN 'TRtcWinTimer.MyTimerReset -> Reset Timer (restart counter) not implemented.'}
{$ENDIF}
  end;

procedure TRtcWinTimer.MyTimerDisable;
  begin
  if FRunning then
    begin
    FRunning:=False;
  {$IFDEF WINDOWS}
    KillTimer(FHandle, RtcIntPtr(self));
  {$ELSE}
    {$MESSAGE WARN 'TRtcWinTimer.MyTimerDisable -> Disable Timer not implemented.'}
  {$ENDIF}
    end;
  end;

class procedure TRtcWinTimer.Stop(var me);
  begin
  if pointer(me)=nil then Exit;

  if rtcRemoveTimer(me) then
    RtcFreeAndNil(me);
  end;

class procedure TRtcWinTimer.Timer(me:TObject);
  var
    FE:TRtcWinTimerEvent;
    TH:TRtcThread;
    JO:TObject;
    intimer:boolean;
  begin
  intimer:=True;
  try
    with TRtcWinTimer(me) do
      if MyTimerCheck then
        begin
        if assigned(FEvent) then
          begin
          if FAutoDisable then
            MyTimerDisable
          else
            MyTimerReset;
          FE:=FEvent;
          if FAutoDestroy then Free;

          rtcLeaveTimer;
          intimer:=False;

          FE;
          end
        else if assigned(FThr) then
          begin
          if FAutoDisable then
            MyTimerDisable
          else
            MyTimerReset;
          TH:=FThr;
          JO:=FJob;
          if FAutoDestroy then Free;

          rtcLeaveTimer;
          intimer:=False;

          TRtcThread.PostJob(TH, JO);
          end
        else // Disable ...
          begin
          if FAutoDestroy then
            Free
          else
            MyTimerDisable;
          end;
        end;
    finally
      if intimer then rtcLeaveTimer;
      end;
  end;

class procedure TRtcWinTimer.Disable(me:TObject);
  begin
  if rtcEnterTimer(RtcIntPtr(me)) then
    try
      TRtcWinTimer(me).MyTimerDisable;
    finally
      rtcLeaveTimer;
      end;
  end;

class procedure TRtcWinTimer.Reset(me:TObject);
  begin
  if rtcEnterTimer(RtcIntPtr(me)) then
    try
      TRtcWinTimer(me).MyTimerReset;
    finally
      rtcLeaveTimer;
      end;
  end;

class procedure TRtcWinTimer.Enable(me:TObject; Wait: Cardinal; Event: TRtcWinTimerEvent; AutoDisable:boolean=False; AutoDestroy:boolean=False);
  begin
  if rtcEnterTimer(RtcIntPtr(me)) then
    with TRtcWinTimer(me) do
      try
        FAutoDisable:=AutoDisable or AutoDestroy;
        FAutoDestroy:=AutoDestroy;
        FThr:=nil;
        FJob:=nil;
        FEvent:=Event;
        FInterval:=Wait;
        MyTimerEnable;
      finally
        rtcLeaveTimer;
        end;
  end;

class procedure TRtcWinTimer.Enable(me:TObject; Wait:Cardinal; Thr:TRtcThread; Job:TObject; AutoDisable:boolean=False; AutoDestroy:boolean=False);
  begin
  if rtcEnterTimer(RtcIntPtr(me)) then
    with TRtcWinTimer(me) do
      try
        FAutoDisable:=AutoDisable or AutoDestroy;
        FAutoDestroy:=AutoDestroy;
        FEvent:=nil;
        FThr:=Thr;
        FJob:=Job;
        FInterval:=Wait;
        MyTimerEnable;
      finally
        rtcLeaveTimer;
        end;
  end;

{$IFDEF WINDOWS}
function RtcTimerWindowProc(ahWnd   : HWND;
                            auMsg   : LongWord;
                            awParam : WPARAM;
                            alParam : LPARAM): Integer;
  var
    Obj    : TObject;
  begin
  if (awParam<>0) then
    Obj:=rtcGetTimer(awParam)
  else
    Obj:=nil;

  if Obj<>nil then
    begin
    try
      TRtcWinTimer.Timer(Obj);
    except
      on E:Exception do
        if LOG_WINTIMER_EXCEPTIONS then
          Log('WM_TIMER',E,'ERROR');
      end;
    Result:=0;
    end
  else
    Result := DefWindowProc(ahWnd, auMsg, awParam, alParam);
  
  end;
{$ENDIF}

initialization
{$IFDEF RTC_DEBUG} Log('rtcWinTimer Initializing ...','DEBUG');{$ENDIF}

CS:=TRtcCritSec.Create;

{$IFDEF WINDOWS}
  rtcRegisterHWNDProc(@RtcTimerWindowProc, WM_TIMER, WM_TIMER);
{$ELSE}
  {$MESSAGE WARN 'Timer unit initialization missing'}
{$ENDIF}

{$IFDEF RTC_DEBUG} Log('rtcWinTimer Initialized.','DEBUG');{$ENDIF}
finalization
{$IFDEF RTC_DEBUG} Log('rtcWinTimer Finalizing ...','DEBUG');{$ENDIF}

CloseThreadPool;

CloseTimerPool;

{$IFDEF WINDOWS}

{$ELSE}
  {$MESSAGE WARN 'Timer unit finalization missing'}
{$ENDIF}

RtcFreeAndNil(CS);

{$IFDEF RTC_DEBUG} Log('rtcWinTimer Finalized.','DEBUG');{$ENDIF}
end.
