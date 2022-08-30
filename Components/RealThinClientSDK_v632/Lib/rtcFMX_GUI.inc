uses
  Classes,
  SysUtils,

  rtcInfo,
  rtcSyncObjs,
  rtcThrPool;

type
  TSyncRtcTimer=class(TTimer)
  public
    procedure rtcSync(Proc:TRtcSyncEvent);
    procedure rtcExecTimer(Sender:TObject);
  end;
  TProcEv=record
    P:TRtcSyncEvent;
    E:TRtcEvent;
  end;

var
  MyTimer:TSyncRtcTimer;
  MyReady:boolean;
  MyList:array of TProcEV;
  CS:TRtcCritSec;

{ TSyncRtcTimer }

procedure TSyncRtcTimer.rtcExecTimer(Sender:TObject);
  var
    xproc:TRtcSyncEvent;
    xev:TRtcEvent;
    a, loc:integer;
    done:boolean;
    tim:Cardinal;
  begin
  xproc:=nil;
  if MyReady then
    begin
    Enabled:=False;
    try
      tim:=GetTickTime;
      done:=False;
      loc:=0;
      repeat
        CS.Acquire;
        try
          with MyList[loc]do
            begin
            xproc:=P;
            xev:=E;
            end;
          Inc(loc);
          if loc=length(MyList) then
            begin
            SetLength(MyList,0);
            done:=True;
            MyReady:=False;
            end
          else if (GetTickTime-Tim>1000) or (loc>100) then // working too long
            begin
            for a:=loc to length(MyList)-1 do
              MyList[a-loc]:=MyList[a];
            SetLength(MyList,length(MyList)-loc);
            done:=True;
            end;
        finally
          CS.Release;
          end;
        try
          xproc;
        except
          end;
        xev.SetEvent;
        until done;
    finally
      Enabled:=True;
      end;
    end;
  end;

procedure TSyncRtcTimer.rtcSync(Proc: TRtcSyncEvent);
  var
    EV:TRtcEvent;
  begin
  EV:=TRtcEvent.Create(True,False);
  CS.Acquire;
  try
    SetLength(MyList, length(MyList)+1);
    with MyList[length(MyList)-1] do
      begin
      P:=Proc;
      E:=EV;
      end;
    MyReady:=True;
  finally
    CS.Release;
    end;
  EV.WaitFor(WAIT_INFINITE);
  EV.Free;
  end;

procedure MySyncProc(Proc: TRtcSyncEvent);
  begin
  MyTimer.rtcSync(Proc);
  end;

procedure MySyncNone(Proc: TRtcSyncEvent);
  begin
  end;

procedure MySyncCheck(var done:boolean);
  begin
  done:=MyReady;
  if done then
    MyTimer.rtcExecTimer(MyTimer);
  end;

initialization
MyReady:=False;
CS:=TRtcCritSec.Create;
MyTimer:=TSyncRtcTimer.Create(nil);
MyTimer.Interval:=RTC_FMXTIMER_INTERVAL;
MyTimer.OnTimer:=MyTimer.rtcExecTimer;
MyTimer.Enabled:=True;

rtcSyncProc:=MySyncProc;
rtcSyncCheckProc:=MySyncCheck;
finalization

rtcSyncProc:=MySyncNone;
MyTimer.Enabled:=False;

while MyReady do
  MyTimer.rtcExecTimer(MyTimer);

rtcSyncCheckProc:=nil;
FreeAndNil(MyTimer);

CloseThreadPool;

FreeAndNil(CS);
