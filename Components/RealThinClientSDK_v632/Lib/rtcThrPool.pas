
{
  @html(<b>)
  Thread Pool
  @html(</b>)
  - Copyright 2004-2014 (c) RealThinClient.com (http://www.realthinclient.com)
  @html(<br><br>)

  Thread pooling mechanism used by all RTC connection components
  when component's @Link(TRtcConnection.MultiThreaded) property is set to True.
  @html(<br><br>)

  Unless you want to enhance the connection components or add your
  own connection providers, you will NEVER get in direct contact
  with this classes. They are being used internaly by most
  Connection Provider components to enable MultiThreaded execution.
  @html(<br><br>)

  The only thing you could get in contact with as a component user
  are the global Threading parameters @Link(RTC_THREAD_POOL_PLUS),
  @Link(RTC_THREAD_POOL_OVERSIZE) and @Link(RTC_THREAD_POOL_MAX).
  @html(<br><br>)

  Or, in case you need to post jobs to a connection component
  to enhance its functionality, with the @Link(TRtcJob) class.
}
unit rtcThrPool;

{$INCLUDE rtcDefs.inc}

interface

uses
{$IFDEF WINDOWS}
  Windows,
  Messages,
{$ENDIF}
{$IFDEF POSIX}
  Posix.Pthread,
{$ENDIF}

  SysUtils,
  Classes,

  rtcTypes,
  rtcInfo,

  memXObjList,
  memObjList,

  rtcLog,
  rtcSyncObjs;

var
  // Min. number of unused threads to keep active
  RTC_THREAD_POOL_PLUS:word=3;
  // Max. number of unused threads to keep active
  RTC_THREAD_POOL_OVERSIZE:word=2048;
  // Max. number of threads in our thread pool.
  RTC_THREAD_POOL_MAX:word=256;

  // Thread Priority
{$IFDEF WINDOWS}
  RTC_THREAD_PRIORITY:TThreadPriority=tpNormal;
{$ENDIF}

  // Log unhandled thread exceptions?
  LOG_THREAD_EXCEPTIONS:boolean={$IFDEF RTC_DEBUG}True{$ELSE}False{$ENDIF};

type
  { @Abstract(Exception to be raised when System Thread limit was reached and not a single thread could be created)
    @exclude }
  EThreadLimitReached = class(Exception);

  // Event for Synchronized calls
  TRtcSyncEvent = procedure of object;

  TRtcWorkerThread = class;
  TRtcThread = class;

  // @exclude
  TRtcBaseMessage=class(TObject);

  { @Abstract(RTC Job class)

    To be able to post jobs to a threaded connection component,
    you can derive your own classes from @Link(TRtcJob). By implementing
    the methods @Link(TRtcJob.Run) and @Link(TRtcObject.Kill), you can post
    any job with your user-defined data to the connection component's thread. }
  TRtcJob = class(TRtcObject)

    { This method will be called ONCE to run (execute) the job.
      It is the Run() method's responsibility to release the object
      when it has finished with its execution, or return TRUE if
      the Job object should be released by the worked thread after
      executing the Run method.

      If you post jobs to connection components,
      handle your expected exceptions properly.

      Exceptions caught by the Threading mechanism will
      not be passed any further. If exception gets raised and it
      returns to the Threading mechanism, the corresponding Thread
      object will be closed, all jobs will be Killed and th Thread
      will be released from memory, which will result in
      abortive disconnect.

      Return TRUE if you want the Job object to be destroyed. }
    function Run(Thr:TRtcThread):boolean; virtual; abstract;
    end;

  { @Abstract(Thread start/stop callback class) }
  TRtcThreadCallback = class(TObject)
  public
    { Called from inside each Thread, after it was started/created }
    procedure AfterThreadStart; virtual; abstract;
    { Called from inside each Thread, before it will be stopped/destroyed }
    procedure BeforeThreadStop; virtual; abstract;
    { Callled after all threads have been stopped.
      This is the method from which you should destroy the object by calling "Free" }
    procedure DestroyCallback; virtual; abstract;
    end;

  { Event type used by the "PostQuickJob" procedure }
  TRtcQuickJobEvent = procedure(Data:TRtcValue) of object;

  { @Abstract(Our threading class)

    We create threads ONLY using this class.
    This class implements all methods needed for synchronizing with the GUI,
    posting jobs, pausing, resuming and stopping the thread. }
  TRtcThread = class(TObject)
  private
    MsgList:TXObjList;
    FInfo:TRtcInfo;
    Working,
    Waiting:boolean;
    Active:boolean;

    procedure GetJob;

    procedure Idle;

  protected
    Thr:TRtcWorkerThread;
    Job:TObject;

    procedure Finalize;

    { Called by the Worker Thread to execute the "Job".
      For user-defined jobs (the ones not derived from TRtcJob),
      you need to override this method and call the inherited RunJob.
      Return TRUE if Thread has to be released. }
    function RunJob:boolean; virtual;

    { Called by the Worket Thread to kill the "Job".
      For user-defined jobs(the ones not derived from TRtcJob),
      you need to override this method and call the inherited Kill(Job). }
    procedure KillJob; virtual;

  public
    // Create a Thread. To give the thread something to do, you will have to post a job to it.
    constructor Create; virtual;
    { @exclude }
    destructor Destroy; override;

    // Call the 'Event' synchronized (for GUI access). May only be used from within the thread.
    procedure Sync(Event:TRtcSyncEvent);

    // Lock threads
    class function Lock(me:TObject):boolean;

    // Unlock threads
    class procedure UnLock;

    // add new job for the thread (thread-safe call)
    class function PostJob(me:TObject; _Job:TObject; HighPriority:boolean=False):boolean;

    // post event for the thread (thread-safe call)
    class function PostEvent(me:TObject; Event:TRtcQuickJobEvent; Data:TRtcValueObject=nil; AccessGUI:boolean=False):boolean;

    // Stop the thread (thread-safe call: will post a QUIT message to thread and destroy it from inside the thread)
    class procedure Stop(me:TObject);

    // Return TRUE if we are currently inside this thread
    function InsideThread:boolean;

    // Attach additional information to this Thread. May only be used from within the thread.
    property Info:TRtcInfo read FInfo;
    end;

  { Component used to implement Quick Jobs }
  {$IFDEF IDE_XE2up}
  [ComponentPlatformsAttribute(pidAll)]
  {$ENDIF}
  TRtcQuickJob = class(TRtc_Component)
  private
    FEvent:TRtcQuickJobEvent;
    FAccessGUI: boolean;
    FSerialized: boolean;
    FThr:TRtcThread;

  public

    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;

    { Post this Quick Job ("OnExecute" event) to the background Thread Pool
      (AccessGUI=False) or to the Main Thread (AccessGUI=True). "Data" can be NIL,
      or any TRtcValueObject which should be passed as parameter to the Event.
      Data object will be destroyed by this procedure, so you should ONLY
      pass objects here which you have created yourself, or use ".copyOf"
      to get a copy of an object which you did not create yourself. }
    procedure Post(Data:TRtcValueObject=nil);

    { Stop serializing thread }
    procedure Stop;

  published
    { Does the Event need access to the GUI? If TRUE, Event will be called from the Main Thread }
    property AccessGUI:boolean read FAccessGUI write FAccessGUI default False;

    { Serialized jobs use only 1 thread at a time. }
    property Serialized:boolean read FSerialized write FSerialized default False;

    { Event to be executed }
    property OnExecute:TRtcQuickJobEvent read FEvent write FEvent;
    end;

  { Internal Class -> DO NOT CREATE!
    @exclude }
  TRtcWorkerThread = class(TThread)
  private
    Work: TRtcThread;

  protected
    Run:TRtcEvent;
    FEvent:TRtcSyncEvent;
    FInsideMain:boolean;
    FFinished:boolean;

    procedure Execute; override;
    procedure Sync(Event:TRtcSyncEvent);

  public
    procedure MySyncEvent;

    function InsideThread:boolean;

    procedure PostQuit;
    procedure PostWork(Thr:TRtcThread);

    constructor Create(CreateSuspended:boolean);
    destructor Destroy; override;
    end;

type
  TRtcSyncProc = procedure(Proc:TRtcSyncEvent);
  TRtcSyncCheckProc = procedure(var done:boolean);

var
  rtcSyncProc : TRtcSyncProc = nil;
  rtcSyncCheckProc : TRtcSyncCheckProc = nil;

{ Check if there are Sync() calls waiting and execute one burst.
  This procedure may ONLY be called from the MAIN Thread!
  Returns TRUE if at least one Sync call was executed. }
function rtcSyncCheck:boolean;

procedure OpenThreadPool;
procedure CloseThreadPool;

{ Post a Quick Job to the Thread Pool (AccessGUI=False) or to the Main Thread (AccessGUI=True).
   @param(Event = Event to be called)
   @param(Data = any TRtcValueObject which should be passed as parameter to the Event.
                 Data object will be destroyed by this procedure, so you should ONLY
                 pass objects here which you have created yourself, or use ".copyOf"
                 to get a copy of an object which you did not create yourself )
   @param(AccessGUI = does the Event need access to the GUI? If TRUE, Event will be called from the Main Thread) }
procedure PostQuickJob(Event:TRtcQuickJobEvent; Data:TRtcValueObject=nil; AccessGUI:boolean=False; UseThread:TRtcThread=nil);

{ Add a new Thread Callback.

  Please note that you can NOT remove a callback and that you need
  to add all callbacks before a first thread was created, which is best
  done from your units "initialization" section. To avoid memory leaks on
  application close, you should also implement the "DestroyCallback" method. }
procedure AddThreadCallback(const Callback:TRtcThreadCallback);

{ Return TRUE if we are inside the Main Thread now }
function InsideMainThread:boolean;

implementation

function rtcSyncCheck:boolean;
  var
    res:boolean;
  begin
  if assigned(rtcSyncCheckProc) then
    begin
    rtcSyncCheckProc(Res);
    Result:=Res;
    end
  else
    Result:=False;
  end;

{ TRtcQuickJob }

constructor TRtcQuickJob.Create(AOwner:TComponent);
  begin
  inherited Create(AOwner);
  FThr:=TRtcThread.Create;
  end;

destructor TRtcQuickJob.Destroy;
  begin
  TRtcThread.Stop(FThr);
  inherited;
  end;

procedure TRtcQuickJob.Post(Data: TRtcValueObject);
  begin
  if FSerialized then
    begin
    if assigned(FThr) then
      PostQuickJob(FEvent,Data,AccessGUI,FThr)
    else
      FreeAndNil(Data);
    end
  else
    PostQuickJob(FEvent,Data,AccessGUI,nil);
  end;

procedure TRtcQuickJob.Stop;
  begin
  TRtcThread.Stop(FThr);
  end;

{ TRtcMyQuickJob }

type
  TRtcMyQuickJob=class(TRtcJob)
  private
    FData:TRtcValue;
    FSync:boolean;
    FEvent:TRtcQuickJobEvent;
    FTemp:boolean;
  public
    procedure Execute;

    function Run(Thr:TRtcThread):boolean; override;
    procedure Kill; override;
    end;

procedure TRtcMyQuickJob.Kill;
  begin
  RtcFreeAndNil(FData);
  {$IFNDEF NEXTGEN} Free; {$ENDIF}
  end;

function TRtcMyQuickJob.Run(Thr: TRtcThread): boolean;
  begin
  try
    if FSync then
      Thr.Sync(Execute)
    else
      Execute;
  finally
    Result:=True;
    if FTemp then
      TRtcThread.Stop(Thr);
    end;
  end;

procedure TRtcMyQuickJob.Execute;
  begin
  try
    if assigned(FEvent) then
      FEvent(FData);
  except
    // ignore all exceptions here
    end;
  RtcFreeAndNil(FData);
  end;

procedure PostQuickJob(Event:TRtcQuickJobEvent; Data:TRtcValueObject=nil; AccessGUI:boolean=False; UseThread:TRtcThread=nil);
  var
    Thr:TRtcThread;
    Job:TRtcMyQuickJob;
  begin
  if not assigned(Event) then
    raise Exception.Create('"Event" required to execute the job');

  Job:=TRtcMyQuickJob.Create;
  if (Data<>nil) and (Data is TRtcValue) then
    Job.FData:=TRtcValue(Data)
  else
    begin
    Job.FData:=TRtcValue.Create;
    Job.FData.asObject:=Data;
    end;
  Job.FSync:=AccessGUI;
  Job.FEvent:=Event;

  if UseThread=nil then
    begin
    Thr:=TRtcThread.Create;
    Job.FTemp:=True;
    end
  else
    begin
    Thr:=UseThread;
    Job.FTemp:=False;
    end;

  if not TRtcThread.PostJob(Thr,Job) then
    FreeAndNil(Job);
  end;

{ TRtcThreadQuickJob }

type
  TRtcThreadQuickJob=class(TRtcJob)
  private
    FData:TRtcValue;
    FSync:boolean;
    FEvent:TRtcQuickJobEvent;
  public
    procedure Execute;

    function Run(Thr:TRtcThread):boolean; override;
    procedure Kill; override;
    end;

procedure TRtcThreadQuickJob.Kill;
  begin
  RtcFreeAndNil(FData);
  {$IFNDEF NEXTGEN} Free; {$ENDIF}
  end;

function TRtcThreadQuickJob.Run(Thr: TRtcThread): boolean;
  begin
  try
    if FSync then
      Thr.Sync(Execute)
    else
      Execute;
  finally
    Result:=True;
    end;
  end;

procedure TRtcThreadQuickJob.Execute;
  begin
  try
    if assigned(FEvent) then
      FEvent(FData);
  except
    // ignore all exceptions here
    end;
  RtcFreeAndNil(FData);
  end;

var
  { @exclude }
  MainThrID:RtcThrID;

function InsideMainThread:boolean;
  begin
  Result:=GetMyThreadID=MainThrID;
  end;

var
  ThreadPool:TObjList; // all running threads (sorted for searching)
  FreePool:TXObjList; // threads not in use (not sorted -> add/remove last)

  ThrList:tObjList; // list of all thread objects (sorted for fast searching)
  WaitList:tXObjList; // list of all thread objects waiting for execution

  Message_Quit:TRtcBaseMessage;

  CSThread:TRtcCritSec;

  InsideCallback:integer=0;
  ThreadCallbacks:array of TRtcThreadCallback;
  ThreadCallbackCount:integer=0;
  HaveThreadCallbacks:boolean=False;

  OpenCnt:integer;
  CSOpen:TRtcEvent;
  Threads_Running:boolean;

{ Add a new Thread Callback }
procedure AddThreadCallback(const Callback:TRtcThreadCallback);
  begin
  CSThread.Acquire;
  try
    HaveThreadCallbacks:=True;
    Inc(ThreadCallbackCount);
    SetLength(ThreadCallbacks, ThreadCallbackCount);
    ThreadCallbacks[ThreadCallbackCount-1]:=Callback;
  finally
    CSThread.Release;
    end;
  end;

{ Remove all Thread Callbacks }
procedure RemoveThreadCallbacks;
  var
    a:integer;
  begin
  if HaveThreadCallbacks then
    begin
    for a:=0 to ThreadCallbackCount-1 do
      begin
      try
        ThreadCallbacks[a].DestroyCallback;
      except
        on E:Exception do
          if LOG_THREAD_EXCEPTIONS then
            Log('RemoteThreadCallbacks TRtcThreadCallback.DestroyCallback',E,'THREAD');
        end;
      ThreadCallbacks[a]:=nil;
      end;
    SetLength(ThreadCallbacks,0);
    ThreadCallbackCount:=0;

    HaveThreadCallbacks:=False;
    end;
  end;

procedure DoAfterThreadStart;
  var
    i:integer;
  begin
  if HaveThreadCallbacks then
    begin
    CSThread.Acquire;
    try
      if ThreadCallbackCount>0 then
        begin
        Inc(InsideCallback);
        for i:=0 to ThreadCallbackCount-1 do
          try
            ThreadCallbacks[i].AfterThreadStart;
          except
            on E:Exception do
              if LOG_THREAD_EXCEPTIONS then
                Log('DoAfterThreadStart TRtcThreadCallback.AfterThreadStart',E,'THREAD');
            end;
        end;
    finally
      CSThread.Release;
      end;
    end;
  end;

procedure DoBeforeThreadStop;
  var
    i:integer;
  begin
  if HaveThreadCallbacks then
    begin
    CSThread.Acquire;
    try
      if ThreadCallbackCount>0 then
        begin
        for i:=ThreadCallbackCount-1 downto 0 do
          try
            ThreadCallbacks[i].BeforeThreadStop;
          except
            on E:Exception do
              if LOG_THREAD_EXCEPTIONS then
                Log('DoBeforeThreadStop TRtcThreadCallback.BeforeThreadStop',E,'THREAD');
            end;
        Dec(InsideCallback);
        if InsideCallback=0 then
          RemoveThreadCallbacks;
        end;
    finally
      CSThread.Release;
      end;
    end;
  end;

{ Work pool }

function GetWork:TRtcThread; // get next waiting object (remove it from waiting list, add it to working list)
  begin
  Result:=nil;
  if WaitList.Count>0 then
    begin
    repeat
      Result:=TRtcThread(WaitList.First);
      WaitList.removeFirst; // remove from waiting list
      Result.Waiting:=False;

      Result.GetJob;
      until assigned(Result.Job) or (WaitList.Count=0);

    if assigned(Result.Job) then
      Result.Working:=True
    else
      Result:=nil;
    end;
  end;

{ Thread Pool }

procedure OpenThreadPool;
  var
    NWork:TRtcWorkerThread;
  begin
  Threads_Running:=True;

  CSThread.Acquire;
  try

    NWork:=nil;
    while (ThreadPool.Count<RTC_THREAD_POOL_MAX) and // thread limit not reached
          (FreePool.Count<RTC_THREAD_POOL_PLUS) do // free thread count under our 'minimum'
      begin
      try
        NWork:=TRtcWorkerThread.Create(False);
      except
        on E:Exception do
          begin
          if LOG_THREAD_EXCEPTIONS then
            Log('OpenThreadPool WorkerThread.Create',E,'THREAD');
          Break;
          end;
        end;
      ThreadPool.insert(RtcIntPtr(NWork),NWork);
      FreePool.addLast(NWork);
      end;
  finally
    CSThread.Release;
    end;
  end;

function GetThread:TRtcWorkerThread;
  var
    NWork:TRtcWorkerThread;
  begin
  Result:=nil;

  if FreePool.Count>0 then // threads available
    begin
    Result:=TRtcWorkerThread(FreePool.Last);
    FreePool.removeLast; // remove from free threads list
    end
  else if ThreadPool.Count<RTC_THREAD_POOL_MAX then // thread limit not reached
    begin
    try
      Result:=TRtcWorkerThread.Create(False);
    except
      on E:Exception do
        begin
        if LOG_THREAD_EXCEPTIONS then
          Log('GetThread WorkerThread.Create',E,'THREAD');
        if ThreadPool.Count=0 then
          raise EThreadLimitReached.Create(E.ClassName+':'+E.Message)
        else
          Result:=nil;
        end;
      end;

    ThreadPool.insert(RtcIntPtr(Result),Result);
    end;

  if assigned(Result) then
    begin
    NWork:=nil;
    while (ThreadPool.Count<RTC_THREAD_POOL_MAX) and // thread limit not reached
          (FreePool.Count<RTC_THREAD_POOL_PLUS) do // free thread count under our 'minimum'
      begin
      try
        NWork:=TRtcWorkerThread.Create(False);
      except
        on E:Exception do
          begin
          if LOG_THREAD_EXCEPTIONS then
            Log('GeetThread WorkerThread.Create',E,'THREAD');
          Break;
          end;
        end;
      ThreadPool.insert(RtcIntPtr(NWork),NWork);
      FreePool.addLast(NWork);
      end;
    end;
  end;

function ReturnThread(Thr:TRtcWorkerThread):boolean; // executed 1 object, returning for another
  var
    Work:TRtcThread;
  begin
  Work:=GetWork;
  if Work<>nil then // execution object waiting
    begin
    Thr.PostWork(Work);
    Result:=True;
    end
  else if FreePool.Count<RTC_THREAD_POOL_OVERSIZE then
    begin
    FreePool.AddLast(Thr);
    Result:=True;
    end
  else
    begin
    ThreadPool.remove(RtcIntPtr(Thr));
    Result:=False;
    end;
  end;

procedure ClosingThread(const Thr:TRtcWorkerThread);
  begin
  {$IFDEF RTC_DEBUG}Log('Closing Thread: '+IntToStr(GetMyThreadID),'DEBUG');{$ENDIF}
  CSThread.Acquire;
  try
    if ThreadPool.search(RtcIntPtr(Thr))=Thr then
      ThreadPool.remove(RtcIntPtr(Thr));
    Dec(OpenCnt);
    {$IFDEF RTC_DEBUG}Log('Thread Stop: '+IntToStr(GetMyThreadID),'DEBUG');{$ENDIF}
    if OpenCnt=0 then
      begin
      {$IFDEF RTC_DEBUG}Log('Last Thread Stopped.','DEBUG');{$ENDIF}
      CSOpen.SetEvent;
      end;
  finally
    CSThread.Release;
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

  while CSOpen.WaitFor(0)<>wr_Signaled do
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

  { Wait 0.01 seconds to allow all threads to terminate. }
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
  CSOpen.WaitFor(_Timeout*1000);
  end;
{$ENDIF}

procedure MySyncNone(Proc: TRtcSyncEvent);
  begin
  end;

procedure CloseThreadPool;
  var
    wrk:RtcIntPtr;
    i:TObject;
    Work:TRtcWorkerThread absolute i;
    clr:RtcIntPtr absolute i;
    havetowait:boolean;
    haveto_removecallbacks:boolean;
  begin
  if not Threads_Running then Exit;
  Threads_Running:=False;

  {$IFDEF RTC_DEBUG}Log('CloseThreadPool begin ...','DEBUG');{$ENDIF}

  rtcSyncProc:=MySyncNone;

  CSThread.Acquire;
  try
    haveto_removecallbacks:=InsideCallback=0;
    havetowait:=ThreadPool.Count>0;
    {$IFDEF RTC_DEBUG}Log('Threads Runnning: '+IntToStr(ThreadPool.Count),'DEBUG');{$ENDIF}
    while ThreadPool.Count>0 do
      begin
      {$IFDEF RTC_DEBUG}Log('Post ThreadQuit '+IntToStr(ThreadPool.Count),'DEBUG');{$ENDIF}
      wrk:=ThreadPool.search_min(i);
      ThreadPool.remove(wrk);
      Work.PostQuit;
      clr:=0;
      end;
  finally
    CSThread.Release;
    end;

  if havetowait then // Wait up to 10 seconds for all threads to close
    begin
    {$IFDEF RTC_DEBUG}Log('Waiting for all Threads to terminate ...','DEBUG');{$ENDIF}
    WaitForClose(10);
    {$IFDEF RTC_DEBUG}Log('Done waiting, all Threads terminated.','DEBUG');{$ENDIF}
    end;

  if haveto_removecallbacks then
    RemoveThreadCallbacks;

  {$IFDEF RTC_DEBUG}Log('CloseThreadPool end.','DEBUG');{$ENDIF}
  end;

{ TRtcThread }

constructor TRtcThread.Create;
  begin
  inherited;

  Working:=False;
  Active:=True;

  if not assigned(CSThread) then
    begin
    MsgList:=nil;
    FInfo:=nil;
    raise Exception.Create('Thread Pool already closed.');
    end;

  MsgList:=TXObjList.Create(32);
  FInfo:=TRtcInfo.Create;

  CSThread.Acquire;
  try
    ThrList.insert(RtcIntPtr(self),self);
  finally
    CSThread.Release;
    end;
  end;

procedure TRtcThread.Finalize;
  begin
  if ThrList.search(RtcIntPtr(self))<>self then
    raise EInvalidPointer.Create('TRtcThread.Finalize called more than once.');

  while MsgList.Count>0 do
    begin
    Job:=TObject(MsgList.First);
    MsgList.removeFirst;
    if Job<>Message_Quit then
      try
        KillJob;
      except
        on E:Exception do
          if LOG_AV_ERRORS then
            Log('TRtcThread.Destroy (MsgList.KillJob)',E,'ERROR');
        end;
    end;
  Job:=nil;

  RtcFreeAndNil(MsgList);
  RtcFreeAndNil(FInfo);

  Active:=False;

  ThrList.remove(RtcIntPtr(self));
  end;

destructor TRtcThread.Destroy;
  begin
  if Active then
    raise EInvalidOperation.Create('TRtcThread.Destroy called directly. Use TRtcThread.Stop() instead.');
  Active:=True;

  inherited;
  end;

class procedure TRtcThread.Stop(me:TObject);
  begin
  TRtcThread.PostJob(me,Message_Quit,True);
  end;

class function TRtcThread.Lock(me: TObject): boolean;
  begin
  Result:=False;
  if assigned(CSThread) then
    begin
    CSThread.Acquire;
    try
      if ThrList.search(RtcIntPtr(me))=me then
        Result:=True;
    finally
      if not Result then
        CSThread.Release;
      end;
    end;
  end;

class procedure TRtcThread.UnLock;
  begin
  CSThread.Release;
  end;

class function TRtcThread.PostEvent(me:TObject; Event:TRtcQuickJobEvent; Data:TRtcValueObject=nil; AccessGUI:boolean=False):boolean;
  var
    Job:TRtcThreadQuickJob;
  begin
  if not assigned(Event) then
    raise Exception.Create('"Event" required to execute the job');

  Job:=TRtcThreadQuickJob.Create;
  if (Data<>nil) and (Data is TRtcValue) then
    Job.FData:=TRtcValue(Data)
  else
    begin
    Job.FData:=TRtcValue.Create;
    Job.FData.asObject:=Data;
    end;
  Job.FSync:=AccessGUI;
  Job.FEvent:=Event;

  Result:=TRtcThread.PostJob(me,Job);
  if not Result then
    FreeAndNil(Job);
  end;

class function TRtcThread.PostJob(me:TObject; _Job:TObject; HighPriority:boolean=False):boolean;
  var
    MyThr:TRtcWorkerThread;
    MyWork:TRtcThread;
  begin
  if _Job=nil then
    begin
    Result:=True;
    Exit;
    end;
  Result:=False;
  if CSThread=nil then Exit;

  CSThread.Acquire;
  try
    if ThrList.search(RtcIntPtr(me))=me then
      with TRtcThread(me) do
        begin
        Result:=True;

        if HighPriority then
          MsgList.addFirst(_Job)
        else
          MsgList.addLast(_Job);

        if not (Working or Waiting) then // not working and not waiting
          begin
          Waiting:=True;
          WaitList.addLast(me); // add to waiting list

          myThr:=GetThread;
          if assigned(myThr) then
            begin
            MyWork:=GetWork;
            if assigned(MyWork) then
              myThr.PostWork(MyWork)
            else
              ReturnThread(myThr);
            end;
          end;
        end;
  finally
    CSThread.Release;
    end;
  end;

procedure TRtcThread.Idle;
  begin
  if Working then
    begin
    Working:=False;
    if MsgList.Count>0 then
      begin
      Waiting:=True;
      WaitList.addLast(self); // add to waiting list
      end;
    end;
  end;

procedure TRtcThread.Sync(Event: TRtcSyncEvent);
  begin
  if assigned(Thr) then
    TRtcWorkerThread(Thr).Sync(Event)
  else
    raise Exception.Create('No thread assigned.');
  end;

procedure TRtcThread.GetJob;
  begin
  if MsgList.Count>0 then
    begin
    Job:=TObject(MsgList.First);
    MsgList.removeFirst;
    end;
  end;

function TRtcThread.RunJob:boolean;
  begin
  Result:=False;
  if Job is TRtcJob then
    begin
    if TRtcJob(Job).Run(self) then TRtcJob(Job).Kill;
    end
  else
    raise Exception.Create('Error!! TRtcThread -> Unknown Job class: '+Job.ClassName);
  end;

procedure TRtcThread.KillJob;
  begin
  if Job<>nil then
    try
      if Job is TRtcJob then
        TRtcJob(Job).Kill;
    except
      on E:Exception do
        if LOG_AV_ERRORS then
          try
            Log('TRtcThread.KillJob ('+Job.ClassName+')',E,'ERROR');
          except
            Log('TRtcThread.KillJob (Unknown_class)',E,'ERROR');
            end;
      end;
  end;

function TRtcThread.InsideThread: boolean;
  begin
  if assigned(Thr) then
    Result:=Thr.InsideThread
  else
    Result:=False;
  end;

{ TRtcWorkerThread }

constructor TRtcWorkerThread.Create(CreateSuspended: boolean);
  begin
  FFinished:=False;
  CSThread.Acquire;
  try
    Inc(OpenCnt);
    if OpenCnt=1 then
      CSOpen.ResetEvent;
  finally
    CSThread.Release;
    end;
  FreeOnTerminate:=True;
  Run:=TRtcEvent.Create(False,False);

  inherited Create(CreateSuspended);

  {$IFDEF WINDOWS}
    Priority:=RTC_THREAD_PRIORITY;
  {$ENDIF}
  end;

destructor TRtcWorkerThread.Destroy;
  begin
  try
    ClosingThread(self);

    RtcFreeAndNil(Run);
    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcWorkerThread.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcWorkerThread.Execute;
  var
    ToFree:boolean;
    w:TRtcThread;
  begin
  {$IFDEF RTC_DEBUG}Log('Thread Start: '+IntToStr(GetMyThreadID),'DEBUG');{$ENDIF}

  try
    DoAfterThreadStart;
  except
    end;

  w:=nil;
  ToFree:=False;
  try
    while Run.WaitFor(WAIT_INFINITE)=wr_Signaled do
      begin
      if FFinished then
        Break
      else if assigned(Work) then
        begin
        try
          if Work.Job=Message_Quit then
            ToFree:=True
          else
            ToFree:=Work.RunJob;
        except
          on E:Exception do
            begin
            ToFree:=True;
            if LOG_THREAD_EXCEPTIONS then
              Log('TRtcWorkerThread Work.RunJob',E,'THREAD');
            // ignore exceptions (do not want to kill this thread)
            end;
          end;

        CSThread.Acquire;
        try
          if ToFree then
            begin
            try
              Work.Thr:=nil;
              Work.Job:=nil;
              Work.Active:=False;
              Work.Finalize; // if Work.RunJob returned TRUE, the thread object is asking to be released
              w:=Work;
            except
              on E:Exception do
                if LOG_THREAD_EXCEPTIONS then
                  Log('TRtcWorkerThread Work.Finalize',E,'THREAD');
              end;
            end
          else
            begin
            try
              Work.Thr:=nil;
              Work.Job:=nil;
              Work.Idle;
            except
              on E:Exception do
                if LOG_THREAD_EXCEPTIONS then
                  Log('TRtcWorkerThread Work.Idle',E,'THREAD');
              end;
            end;
          Work:=nil;

          ToFree:=not ReturnThread(self);
        finally
          CSThread.Release;
          end;

        if assigned(w) then
          try
            RtcFreeAndNil(w);
          except
            on E:Exception do
              if LOG_THREAD_EXCEPTIONS then
                Log('TRtcWorkerThread Work.Free',E,'THREAD');
            end;

        if ToFree then Break;
        end
      else
        begin
        FFinished:=True;
        Break;
        end;
      end;
  except
    on E:Exception do
      if LOG_THREAD_EXCEPTIONS then
        Log('TRtcWorkThread.Execute',E,'THREAD');
    end;

  try
    DoBeforeThreadStop;
  except
    end;
  end;

function TRtcWorkerThread.InsideThread:boolean;
  begin
  if FInsideMain then
    Result:=InsideMainThread
  else
    Result:=GetMyThreadID=RtcThrID(ThreadID);
  end;

procedure TRtcWorkerThread.MySyncEvent;
  begin
  FInsideMain:=True;
  try
    FEvent;
  finally
    FInsideMain:=False;
    end;
  end;

procedure TRtcWorkerThread.PostWork(Thr: TRtcThread);
  begin
  Work:=Thr;
  Work.Thr:=self;
  Run.SetEvent;
  end;

procedure TRtcWorkerThread.PostQuit;
  begin
  FFinished:=True;
  Run.SetEvent;
  end;

procedure TRtcWorkerThread.Sync(Event: TRtcSyncEvent);
  begin
  FEvent:=Event;
  if assigned(rtcSyncProc) then
    rtcSyncProc(MySyncEvent)
  else
    Synchronize(MySyncEvent);
  end;

initialization
{$IFDEF RTC_DEBUG} Log('rtcThrPool Initializing ...','DEBUG');{$ENDIF}

MainThrID:=GetMyThreadID;

Threads_Running:=True;
ThreadCallbackCount:=0;
SetLength(ThreadCallbacks,0);
InsideCallback:=0;

CSThread:=TRtcCritSec.Create;
OpenCnt:=0;
CSOpen:=TRtcEvent.Create(True,True);

ThreadPool:=tObjList.Create(128);
FreePool:=TXObjList.Create(128);

Message_Quit:=TRtcBaseMessage.Create;
ThrList:=tObjList.Create(128);
WaitList:=tXObjList.Create(128);

{$IFDEF RTC_DEBUG} Log('rtcThrPool Initialized.','DEBUG');{$ENDIF}
finalization
{$IFDEF RTC_DEBUG} Log('rtcThrPool Finalizing ...','DEBUG');{$ENDIF}

CloseThreadPool;

RtcFreeAndNil(CSOpen);
RtcFreeAndNil(CSThread);

{$IFDEF RTC_DEBUG}Log('Releasing Thread Pool','DEBUG');{$ENDIF}
RtcFreeAndNil(ThreadPool);
RtcFreeAndNil(FreePool);
{$IFDEF RTC_DEBUG}Log('Thread Pool released.','DEBUG');{$ENDIF}
{$IFDEF RTC_DEBUG}Log('Releasing Thread List','DEBUG');{$ENDIF}
RtcFreeAndNil(ThrList);
RtcFreeAndNil(WaitList);
RtcFreeAndNil(Message_Quit);
{$IFDEF RTC_DEBUG}Log('Thread List released','DEBUG');{$ENDIF}

{$IFDEF RTC_DEBUG} Log('rtcThrPool Finalized.','DEBUG');{$ENDIF}
end.
