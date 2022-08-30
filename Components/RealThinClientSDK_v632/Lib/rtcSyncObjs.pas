{
  "SyncObjcs"
  - Copyright 2004-2014 (c) RealThinClient.com (http://www.realthinclient.com)
  @exclude
}
unit rtcSyncObjs;

interface

{$INCLUDE rtcDefs.inc}

uses
  SysUtils,
  SyncObjs,

  rtcTypes;

const
  WAIT_INFINITE = LongWord($FFFFFFFF);     { Infinite timeout }
  wr_Signaled=wrSignaled;
  wr_Timeout=wrTimeout;
  wr_Abandoned=wrAbandoned;
  wr_Error=wrError;

type
  TRtcWaitResult = TWaitResult;

  TRtcEvent=class(TEvent)
  public
    // If ManualReset=False, a successful call to WaitFor will automatically Reset the event
    // If InitialState=True, event will be Set at start. If InitialState=False, event will be Reset at start.
    constructor Create(ManualReset,InitialState:boolean);
    end;

  TRtcCritSec=class(TCriticalSection);

  TRtcRWSec=class
  private
    WriteSec:TRtcEvent;
    PassSec,ReadSec:TRtcCritSec;
    Cnt,Cnt3:longint;
  public
    constructor Create;
    destructor Destroy; override;

    procedure EnterRead;  // Normal reader, no hurry.
    procedure LeaveRead;

    procedure ForceWrite;  // Need to write as fast as possible, force readers to stop reading.
    procedure EnterWrite;  // Normal writer, no hurry.
    procedure LeaveWrite;

    procedure ForceRead;  // Need to read as fast as possible, ignore waiting writers.
    procedure DoneRead;  // Done reading.
  end;


implementation

{ TRtcEvent }

constructor TRtcEvent.Create(ManualReset, InitialState: boolean);
  begin
  inherited Create(nil,ManualReset,InitialState,'');
  end;

{ TRtcRWSec }

constructor TRtcRWSec.Create;
  begin
  inherited;
  Cnt:=0;Cnt3:=0;
  PassSec:=TRtcCritSec.Create;
  ReadSec:=TRtcCritSec.Create;
  WriteSec:=TRtcEvent.Create(False,True);  // Auto-reset
  end;

destructor TRtcRWSec.Destroy;
  begin
  RtcFreeAndNil(PassSec);
  RtcFreeAndNil(ReadSec);
  RtcFreeAndNil(WriteSec);
  inherited;
  end;

procedure TRtcRWSec.EnterRead;
  begin
  PassSec.Acquire;
  PassSec.Release;

  ReadSec.Acquire;
  try
    if (Cnt=0) and (Cnt3=0) then // There are no readers inside
      WriteSec.WaitFor(WAIT_INFINITE);  // Block all writers, this is the first reader.
    Inc(Cnt);
  finally
    ReadSec.Release;
    end;
  end;

procedure TRtcRWSec.ForceRead;
  var
    OK:boolean;
  begin
  OK:=False;
  ReadSec.Acquire;
  try
    if Cnt>0 then // There are normal readers inside, writers are blocked.
      begin
      Inc(Cnt3);
      OK:=True;
      end;
  finally
    ReadSec.Release;
    end;

  if not OK then
    begin
    PassSec.Acquire;
    PassSec.Release;

    ReadSec.Acquire;
    try
      if (Cnt=0) and (Cnt3=0) then // There are no readers inside
        WriteSec.WaitFor(WAIT_INFINITE);  // Block all writers
      Inc(Cnt3);
    finally
      ReadSec.Release;
      end;
    end;
  end;

procedure TRtcRWSec.LeaveRead;
  begin
  ReadSec.Acquire;
  try
    Dec(Cnt);
    if (Cnt=0) and (Cnt3=0) then
      WriteSec.SetEvent;  // Un-block writers
  finally
    ReadSec.Release;
    end;
  end;

procedure TRtcRWSec.DoneRead;
  begin
  ReadSec.Acquire;
  try
    Dec(Cnt3);
    if (Cnt=0) and (Cnt3=0) then
      WriteSec.SetEvent;  // Un-block writers
  finally
    ReadSec.Release;
    end;
  end;

procedure TRtcRWSec.EnterWrite;
  begin
  PassSec.Acquire;

  WriteSec.WaitFor(WAIT_INFINITE);
  end;

procedure TRtcRWSec.ForceWrite;
  begin
  PassSec.Acquire;

  WriteSec.WaitFor(WAIT_INFINITE);
  end;

procedure TRtcRWSec.LeaveWrite;
  begin
  WriteSec.SetEvent;

  PassSec.Release;
  end;

end.
