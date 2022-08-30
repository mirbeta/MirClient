{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressCore Library                                      }
{                                                                    }
{           Copyright (c) 1998-2019 Developer Express Inc.           }
{           ALL RIGHTS RESERVED                                      }
{                                                                    }
{   The entire contents of this file is protected by U.S. and        }
{   International Copyright Laws. Unauthorized reproduction,         }
{   reverse-engineering, and distribution of all or any portion of   }
{   the code contained in this file is strictly prohibited and may   }
{   result in severe civil and criminal penalties and will be        }
{   prosecuted to the maximum extent possible under the law.         }
{                                                                    }
{   RESTRICTIONS                                                     }
{                                                                    }
{   THIS SOURCE CODE AND ALL RESULTING INTERMEDIATE FILES            }
{   (DCU, OBJ, DLL, ETC.) ARE CONFIDENTIAL AND PROPRIETARY TRADE     }
{   SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER IS    }
{   LICENSED TO DISTRIBUTE THE EXPRESSCORE LIBRARY AND ALL           }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY. }
{                                                                    }
{   THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED       }
{   FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE         }
{   COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE        }
{   AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT   }
{   AND PERMISSION FROM DEVELOPER EXPRESS INC.                       }
{                                                                    }
{   CONSULT THE END USER LICENSE AGREEMENT FOR INFORMATION ON        }
{   ADDITIONAL RESTRICTIONS.                                         }
{                                                                    }
{********************************************************************}

unit dxThreading;

{$I cxVer.inc}

interface

uses
  Types, Windows, Messages, SysUtils, Variants, Classes, SyncObjs, dxCoreClasses, Contnrs, ExtCtrls,
  Generics.Defaults, Generics.Collections;

const
  dxMaxUserWorkItems = 32;

type
  TWorkItemObjectProc = procedure(Sender: TObject; AContext: Pointer) of object;
  TWorkItemProc = procedure(AContext: Pointer);

  { TdxThreadPoolHelper }

  TdxThreadPoolHelper = class
  {$REGION 'internal types'}
  protected type
    TUserWorkItem = record
      ThreadPool: TdxThreadPoolHelper;
      Sender: TObject;
      Context: Pointer;
      ObjectProc: TWorkItemObjectProc;
      Proc: TWorkItemProc;
    end;
    TUserWorkItemArray = array[0..dxMaxUserWorkItems - 1] of TUserWorkItem;
  {$ENDREGION}
  private
    FRunningTaskCount: Integer;
    FException: Exception;
    FTaskIndex: Integer;
    FEvent: TSimpleEvent;
    FWorkItems: TUserWorkItemArray;
    procedure DoneWorkItems;
  protected
    procedure DoHandleException;
    procedure HandleException;
    procedure ResetException;
    procedure SetException;
  public
    constructor Create;
    destructor Destroy; override;
    procedure CreateEvent;
    procedure DestroyEvent;
    procedure Initialize(ANumThreads: Integer; ACreateEvent: Boolean = False);
    procedure QueueWorkItem(ASender: TObject; AObjectProc: TWorkItemObjectProc; AContext: Pointer); overload;
    procedure QueueWorkItem(ASender: TObject; AObjectProc: TWorkItemObjectProc; AContext: Pointer; AFlags: ULONG); overload;
    procedure QueueWorkItem(AProc: TWorkItemProc; AContext: Pointer); overload;
    procedure QueueWorkItem(AProc: TWorkItemProc; AContext: Pointer; AFlags: ULONG); overload;
    procedure WaitForThreads(ADestroyEvent: Boolean = False);
  end;

  { TdxMultithreadOperation}

  TdxMultithreadOperation = class
  private
    FLocked: Integer;
    FThreadCount: Integer;
    FThreadPoolHelper: TdxThreadPoolHelper;
  protected
    property ThreadCount: Integer read FThreadCount;
    property ThreadPoolHelper: TdxThreadPoolHelper read FThreadPoolHelper;
  public
    constructor Create(AThreadCount: Integer); overload; virtual;
    constructor Create; overload; virtual;
    destructor Destroy; override;
    function Lock: Boolean;
    procedure Unlock;
  end;

  { TdxMultithreadedSort }

  TdxMultithreadedSort = class(TdxMultithreadOperation)
  {$REGION 'internal types'}
  protected type
    TSortWorkItem = record
      List: PdxPointerList;
      StartIndex: Integer;
      EndIndex: Integer;
    end;
    PSortWorkItem = ^TSortWorkItem;
    TSortWorkItemArray = array[0..dxMaxUserWorkItems - 1] of TSortWorkItem;

    TMergeWorkItem = record
      List: PdxPointerList;
      Temp: PdxPointerList;
      StartIndex: Integer;
      MiddleIndex: Integer;
      EndIndex: Integer;
    end;
    PMergeWorkItem = ^TMergeWorkItem;
    TMergeWorkItemArray = array[0..dxMaxUserWorkItems - 1] of TMergeWorkItem;
  {$ENDREGION}
  private
    FCompareClassFunc: TCompareItems;
    FCompareFunc: TListSortCompare;
    FCount: Integer;
    FSortChunkCount: Integer;
    FSortChunks: TSortWorkItemArray;
    procedure CalculateChunkCount(AItemCount: Integer);
    procedure Initialize(ACount: Integer);
    procedure MergeChunks(AList: PdxPointerList);
    procedure ParallelMerge(Sender: TObject; AContext: Pointer);
    procedure ParallelSort(Sender: TObject; AContext: Pointer);
    procedure SortListChunks(AList: PdxPointerList);
  public
    constructor Create(AThreadCount: Integer); override;
    procedure Sort(AList: PdxPointerList; ACount: Integer; const ACompareFunc: TCompareItems); overload;
    procedure Sort(AList: TdxFastList; const ACompareFunc: TCompareItems); overload;
    procedure Sort(AList: PdxPointerList; ACount: Integer; const ACompareFunc: TListSortCompare); overload;
    procedure Sort(AList: TdxFastList; const ACompareFunc: TListSortCompare); overload;
  end;

  { TdxMultithreadedIterator }

  TdxCustomMultithreadedIterator = class(TdxMultithreadOperation)
  {$REGION 'internal types'}
  protected type
    TIterateChunkWorkItem = record
      Context: Pointer;
      StartIndex: Integer;
      EndIndex: Integer;
      Reverse: Boolean;
    end;
    PIterateChunkWorkItem = ^TIterateChunkWorkItem;
    TIterateChunkWorkItemArray = array[0..dxMaxUserWorkItems - 1] of TIterateChunkWorkItem;
  {$ENDREGION}
  private
    FCount: Integer;
    FStartIndex: Integer;
    FEndIndex: Integer;
    FFinishedIndex: Integer;
    FChunkCount: Integer;
    FChunks: TIterateChunkWorkItemArray;
    FReverse: Boolean;
  protected
    function GetChunkContext(AChunkIndex: Integer; AContext: Pointer): Pointer; virtual;
    function Initialize(AStartIndex, AEndIndex: Integer; AReverse: Boolean): Boolean;
    procedure InitializeChunksEnvironment; virtual;
    property ChunkCount: Integer read FChunkCount;
  end;

  { TdxMultithreadedIterator }

  TdxIterateItemFunc = function (AContext: Pointer; AIndex: Integer): Boolean of object;

  TdxMultithreadedIterator = class(TdxCustomMultithreadedIterator)
  private
    FItemFunc: TdxIterateItemFunc;
  protected
    procedure ProcessChunk(AContext: TObject; AWorkItem: Pointer);
    function ProcessChunks(AContext: Pointer; const AItemFunc: TdxIterateItemFunc): Integer;
  public
    function IterateItems(AStartIndex, AEndIndex: Integer; AContext: Pointer; const AItemFunc: TdxIterateItemFunc; AReverse: Boolean): Integer;
  end;

  { TdxMultithreadedClassMethodIterator }

  TdxIterateItemObjectFunc = function (AContext: Pointer; AIndex: Integer): Boolean of object;

  TdxMultithreadedClassMethodIterator = class(TdxCustomMultithreadedIterator)
  private
    FItemObjectFunc: TdxIterateItemObjectFunc;
  protected
    procedure ProcessChunk(AContext: TObject; AWorkItem: Pointer);
    function ProcessChunks(AContext: Pointer; const AItemObjectFunc: TdxIterateItemObjectFunc): Integer;
  public
    function IterateItems(AStartIndex, AEndIndex: Integer; AContext: Pointer;
      const AItemObjectFunc: TdxIterateItemObjectFunc; AReverse: Boolean): Integer;
  end;

  { TdxMultithreadedListIterator }

  TdxFastListItemFunc = function (AList: TdxFastList; AIndex: Integer): Boolean of object;
  TdxListItemFunc = function (AList: TList; AIndex: Integer): Boolean of object;
  TdxStringsListItemFunc = function (AStrings: TStrings; AIndex: Integer): Boolean of object;

  TdxMultithreadedListIterator = class(TdxMultithreadedClassMethodIterator)
  protected
    function DoIterate(ACount: Integer; AContext: Pointer; const AItemObjectFunc: TdxIterateItemObjectFunc; AReverse: Boolean): Integer;
  public
    function Iterate(AContext: Pointer; ACount: Integer; const AItemFunc: TdxIterateItemObjectFunc; AReverse: Boolean = False): Integer; overload;
    function Iterate(AList: TdxFastList; const AItemFunc: TdxFastListItemFunc; AReverse: Boolean = False): Integer; overload;
    function Iterate(AList: TList; const AItemFunc: TdxListItemFunc; AReverse: Boolean = False): Integer; overload;
    function Iterate(AStrings: TStrings; const AItemFunc: TdxStringsListItemFunc; AReverse: Boolean = False): Integer; overload;
  end;

{$REGION 'Tasks Dispatcher'}

  TdxThreadMethodCallMode = (tmcmAsync, tmcmSync, tmcmSyncPostponed);
  TdxTaskCancelCallback = function: Boolean of object;
  TdxTaskProc = reference to procedure (CheckCanceled: TdxTaskCancelCallback);

  TdxTaskDispatcher = class;

  { IdxTask }

  TdxTaskCompletedStatus = (Success, Fail, Cancelled);
  IdxTask = interface
    function Run(const ACancelStatus: TdxTaskCancelCallback): TdxTaskCompletedStatus;
    procedure OnComplete(AStatus: TdxTaskCompletedStatus);
  end;

  { IdxTaskEvent }

  IdxTaskEvent = interface
  ['{3CAF68AD-4959-429F-A6BB-19DC671BD3BB}']
    function Signal: Boolean;
    function WaitFor(ATimeOut: Cardinal): TWaitResult;
  end;

  { TdxTask }

  TdxTask = class(TcxIUnknownObject)
  private
    FCanceled: Integer;
    FEvent: IdxTaskEvent;
    FOwner: TdxTaskDispatcher;
    FStartWaitingAt: Cardinal;
    FThreadId: Cardinal;

    FOnComplete: TThreadMethod;
    FOnCompleteMode: TdxThreadMethodCallMode;

    function GetHandle: THandle;
  protected
    procedure Complete; virtual;
    procedure Execute; virtual; abstract;
    function GetCanceled: Boolean;
  public
    property Canceled: Boolean read GetCanceled;
    property Handle: THandle read GetHandle;
  end;

  { TdxTaskGroup }

  TdxTaskGroup = class(TcxIUnknownObject)
  strict private type
  {$REGION 'Internal Types'}
    TTaskWrapper = class(TdxTask)
    protected
      FTask: TdxTask;

      procedure Execute; override;
      procedure Complete; override;
    end;
  {$ENDREGION}
  strict private
    FActiveTasks: Integer;
    FEvent: TEvent;

    procedure CompleteHandler;
    function GetTask(Index: Integer): TdxTask;
    function GetTaskCount: Integer;
  protected
    FTasks: TObjectList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddTask(const ATask: TdxTask); overload;
    procedure AddTask(const AProc: TdxTaskProc); overload;
    procedure Initialize;
    procedure RunAndWait; virtual;

    property ActiveTasks: Integer read FActiveTasks;
    property TaskCount: Integer read GetTaskCount;
    property Tasks[Index: Integer]: TdxTask read GetTask;
  end;

  { TdxTaskEvent }

  TdxTaskEvent = class(TInterfacedObject, IdxTaskEvent)
  strict private
    FHandle: THandle;
  public
    constructor Create;
    destructor Destroy; override;
    function Signal: Boolean;
    function WaitFor(ATimeOut: Cardinal): TWaitResult;
  end;

  { TdxTaskDispatcher }

  TdxTaskDispatcher = class
  strict private const
    CpuUsageLow = 80;
    CpuUsageMonitorLogSize = 10;
    CpuUsageMonitorUpdateInterval = 1000;
    SuccessfulWaitResults = [wrSignaled, wrAbandoned];
  protected type
  {$REGION 'Protected Types'}

    TProcWrapper = class(TdxTask)
    strict private
      FProc: TdxTaskProc;
    protected
      procedure Execute; override;
    public
      constructor Create(AProc: TdxTaskProc);
    end;

    TTaskWrapper = class(TdxTask)
    strict private
      FCompletedStatus: TdxTaskCompletedStatus;
      FTask: IdxTask;

      procedure SyncComplete;
    protected
      procedure Complete; override;
      procedure Execute; override;
    public
      constructor Create(const ATask: IdxTask);
    end;

  {$ENDREGION}
  strict private
    FActiveTasks: TList;
    FLock: TRTLCriticalSection;
    FMaxActiveTasks: Integer;
    FTasks: TObjectList;
  {$IFDEF DELPHI101BERLIN}
    FCpuUsageLog: array [0..CpuUsageMonitorLogSize - 1] of Integer;
    FCpuUsageMonitor: TTimer;
    FPrevSystemTimes: TThread.TSystemTimes;
  {$ENDIF}

    procedure AsyncRun(ATask: TdxTask);
    procedure CancelAll;
    procedure CheckActiveTasks;
    procedure SetMaxActiveTasks(AValue: Integer);
  {$IFDEF DELPHI101BERLIN}
    function GetUseCpuUsageMonitor: Boolean;
    procedure SetUseCpuUsageMonitor(AValue: Boolean);
    procedure HandlerCpuUsageMonitor(Sender: TObject);
  {$ENDIF}
  protected
    class function ThreadProc(ATask: TdxTask): Integer; stdcall; static;
    procedure Start(ATask: TdxTask);
  public
    constructor Create;
    destructor Destroy; override;
    procedure BeforeDestruction; override;

    function Run(const ATask: IdxTask): THandle; overload;
    function Run(const AProc: TdxTaskProc): THandle; overload;
    function Run(const AProc: TdxTaskProc; ACompleteEvent: TThreadMethod; ACompleteEventCallMode: TdxThreadMethodCallMode): THandle; overload;
    function Run(ATask: TdxTask): THandle; overload;
    function Run(ATask: TdxTask; ACompleteEvent: TThreadMethod; ACompleteEventCallMode: TdxThreadMethodCallMode): THandle; overload;
    function RunInCurrentThread(ATask: TdxTask): THandle;

    function Cancel(ATaskHandle: THandle; AWaitFor: Boolean = False): Boolean; overload;
    function Cancel(ATaskHandle: THandle; AWaitTimeOut: Cardinal): TWaitResult; overload;
    function IsCurrentTaskCanceled: Boolean;

    function WaitFor(ATaskHandle: THandle): Boolean; overload;
    function WaitFor(ATaskHandle: THandle; AWaitTimeOut: Cardinal): TWaitResult; overload;

    property MaxActiveTasks: Integer read FMaxActiveTasks write SetMaxActiveTasks;
  {$IFDEF DELPHI101BERLIN}
    property UseCpuUsageMonitor: Boolean read GetUseCpuUsageMonitor write SetUseCpuUsageMonitor;
  {$ENDIF}
  end;
{$ENDREGION}

  { TdxUIThreadSyncService }

  TdxUIThreadSyncService = class sealed
  protected type
    TEnqueuedProcedure = record
      Owner: TObject;
      ProcedureRef: TProc;
    end;
  strict private class var
    FDelegates: TList<TEnqueuedProcedure>;
    FEmptyDelegate: TEnqueuedProcedure;
    FHandle: THandle;
    FLock: TRTLCriticalSection;
    FShutdown: Boolean;
    class constructor Initialize;
  {$HINTS OFF}
    class destructor Finalize;
  {$HINTS ON}
    class procedure ClearWindowsQueue;
    class procedure Invoke;
    class procedure WndProc(var Message: TMessage);
    class function IsShutdown: Boolean; static;
    class procedure DeleteAllListeners; static;
  public
    class procedure EnqueueInvokeInUIThread(AOwner: TObject; const AProcedure: TProc); static;
    class procedure Unsubscribe(AListener: TObject); static;
  end;

const
  dxEnableMultiThreading: Boolean = True;

var
  dxListIterator: TdxMultithreadedListIterator;
  dxSortHelper: TdxMultithreadedSort;
  dxTasksDispatcher: TdxTaskDispatcher;

function dxCanUseMultiThreading: Boolean;

implementation

uses
  dxCore, Math, Forms, dxMessages;

var
  FQueueUserWorkItemProc: function (func: TThreadStartRoutine; Context: Pointer; Flags: ULONG): BOOL; stdcall;
  FCanUseMultiThreading: Boolean;

procedure CallThreadMethod(AMethod: TThreadMethod; AMode: TdxThreadMethodCallMode);
begin
  if Assigned(AMethod) then
    case AMode of
      tmcmAsync:
        AMethod;
      tmcmSync:
        TThread.Synchronize(nil, AMethod);
      tmcmSyncPostponed:
        TThread.Queue(nil, AMethod);
    end;
end;

function WaitForSyncObject(AHandle: THandle; ATimeOut: Cardinal): TWaitResult;
const
  WaitTime = 100;
var
  AHandles: array[0..1] of THandle;
  AMsg: TMsg;
begin
  if GetCurrentThreadId = MainThreadID then
  begin
    Result := wrTimeout;
    AHandles[0] := AHandle;
    AHandles[1] := SyncEvent;
    while ATimeOut > 0 do
    begin
      case MsgWaitForMultipleObjects(2, AHandles, False, Min(WaitTime, ATimeOut), QS_SENDMESSAGE) of
        WAIT_FAILED:
          Exit(wrError);
        WAIT_OBJECT_0:
          Exit(wrSignaled);
        WAIT_ABANDONED:
          Exit(wrAbandoned);
        WAIT_OBJECT_0 + 1:
          CheckSynchronize;
        WAIT_OBJECT_0 + 2:
          PeekMessage(AMsg, 0, 0, 0, PM_NOREMOVE);
      end;
      if ATimeOut <> INFINITE then
        Dec(ATimeOut, Min(WaitTime, ATimeOut));
    end;
  end
  else
    case WaitForSingleObject(AHandle, ATimeOut) of
      WAIT_OBJECT_0:
        Result := wrSignaled;
      WAIT_ABANDONED:
        Result := wrAbandoned;
      WAIT_TIMEOUT:
        Result := wrTimeout;
    else
      Result := wrError;
    end;
end;

{$IFDEF CPUX64}
function InterlockedDecrement(var Addend: LongInt): LongInt;
asm
  .NOFRAME
  MOV     EAX,-1
  LOCK XADD [RCX].Integer, EAX
  DEC     EAX
end;

function InterlockedIncrement(var Addend: LongInt): LongInt;
asm
  MOV     EAX,1
  LOCK XADD [RCX].Integer, EAX
  INC     EAX
end;

function InterlockedCompareExchange(var Destination: Integer; Exchange: Integer; Comparand: Integer): Integer;
asm
  .NOFRAME
  MOV     EAX,R8d
  LOCK CMPXCHG [RCX].Integer, EDX
end;

function InterlockedExchange(var Target: Integer; Value: Integer): Integer;
asm
  .NOFRAME
  LOCK XCHG [RCX], EDX
  MOV     EAX, EDX
end;
{$ELSE}
function InterlockedDecrement(var Target: Integer): Integer;
asm
  // --> EAX Target
  // <-- EAX Result
  MOV     ECX, EAX
  MOV     EAX, -1
  LOCK XADD [ECX], EAX
  DEC     EAX
end;

function InterlockedIncrement(var Target: Integer): Integer;
asm
  // --> EAX Target
  // <-- EAX Result
  MOV     ECX, EAX
  MOV     EAX, -1
  LOCK XADD [ECX], EAX
  INC     EAX
end;

function InterlockedCompareExchange(var Target: Integer; Exchange: Integer; Comparand: Integer): Integer;
asm
  XCHG    EAX,ECX
  LOCK CMPXCHG [ECX],EDX
end;

function InterlockedExchange(var Target: Integer; Value: Integer): Integer;
asm
  MOV     ECX, EAX
  MOV     EAX, [ECX]
@@1:
  LOCK CMPXCHG [ECX],EDX
  JNZ     @@1
end;
{$ENDIF CPUX64}

function ThreadFunction(lpThreadParameter: Pointer): Integer; stdcall;
var
  ACount: Integer;
begin
  Result := 0;
  with TdxThreadPoolHelper.TUserWorkItem(lpThreadParameter^) do
  try
    try
      if Assigned(Proc) then
        Proc(Context)
      else
        ObjectProc(Sender, Context);
    except
      ThreadPool.SetException;
    end;
  finally
    ACount := InterlockedDecrement(ThreadPool.FRunningTaskCount);
    if ACount = 0 then
      ThreadPool.DoneWorkItems;
    if not SwitchToThread then
      SleepEx(0, False);
  end;
end;

{ TThreadPool }

constructor TdxThreadPoolHelper.Create;
begin
  IsMultiThread := True;
end;

destructor TdxThreadPoolHelper.Destroy;
begin
  DestroyEvent;
  inherited Destroy;
end;

procedure TdxThreadPoolHelper.CreateEvent;
begin
  if FEvent = nil then
    FEvent := TSimpleEvent.Create;
end;

procedure TdxThreadPoolHelper.DestroyEvent;
begin
  FreeAndNil(FEvent);
end;

procedure TdxThreadPoolHelper.Initialize(ANumThreads: Integer; ACreateEvent: Boolean = False);
begin
  if ACreateEvent then
    CreateEvent;
  if ANumThreads > dxMaxUserWorkItems then
    ANumThreads := dxMaxUserWorkItems;
  FRunningTaskCount := ANumThreads;
  FTaskIndex := 0;
end;

procedure TdxThreadPoolHelper.DoHandleException;
begin
  if FException is Exception then
    dxShowException(FException);
end;

procedure TdxThreadPoolHelper.DoneWorkItems;
begin
  FEvent.SetEvent;
end;

procedure TdxThreadPoolHelper.HandleException;
begin
  if Assigned(FException) then
  try
    // Don't show EAbort messages
    if not (FException is EAbort) then
      TThread.Synchronize(nil, DoHandleException);
  finally
    ResetException;
  end;
end;

procedure TdxThreadPoolHelper.QueueWorkItem(ASender: TObject; AObjectProc: TWorkItemObjectProc; AContext: Pointer; AFlags: ULONG);
begin
  if Assigned(AObjectProc) then
  begin
    FWorkItems[FTaskIndex].Context    := AContext;
    FWorkItems[FTaskIndex].Sender     := ASender;
    FWorkItems[FTaskIndex].ThreadPool := Self;
    FWorkItems[FTaskIndex].ObjectProc := AObjectProc;
    FWorkItems[FTaskIndex].Proc       := nil;
    if not FQueueUserWorkItemProc(ThreadFunction, @FWorkItems[FTaskIndex], AFlags) then
      RaiseLastOSError;
    Inc(FTaskIndex);
  end;
end;

procedure TdxThreadPoolHelper.QueueWorkItem(ASender: TObject; AObjectProc: TWorkItemObjectProc; AContext: Pointer);
begin
  QueueWorkItem(ASender, AObjectProc, AContext,
    WT_EXECUTEDEFAULT
//    WT_EXECUTELONGFUNCTION
    );
end;

procedure TdxThreadPoolHelper.QueueWorkItem(AProc: TWorkItemProc; AContext: Pointer);
begin
  QueueWorkItem(AProc, AContext,
    WT_EXECUTEDEFAULT
//    WT_EXECUTELONGFUNCTION
    );
end;

procedure TdxThreadPoolHelper.QueueWorkItem(AProc: TWorkItemProc; AContext: Pointer; AFlags: ULONG);
begin
  if Assigned(AProc) then
  begin
    FWorkItems[FTaskIndex].Context    := AContext;
    FWorkItems[FTaskIndex].Sender     := nil;
    FWorkItems[FTaskIndex].ThreadPool := Self;
    FWorkItems[FTaskIndex].ObjectProc := nil;
    FWorkItems[FTaskIndex].Proc       := AProc;
    if not FQueueUserWorkItemProc(ThreadFunction, @FWorkItems[FTaskIndex], AFlags) then
      RaiseLastOSError;
    Inc(FTaskIndex);
  end;
end;

procedure TdxThreadPoolHelper.ResetException;
begin
  FException := nil;
end;

procedure TdxThreadPoolHelper.SetException;
var
  APatternException: Exception;
begin
  APatternException := Exception(ExceptObject);
  FException := Exception.CreateHelp(APatternException.Message, APatternException.HelpContext);
end;

procedure TdxThreadPoolHelper.WaitForThreads(ADestroyEvent: Boolean = False);
begin
  FEvent.WaitFor(INFINITE);
  if ADestroyEvent then
    DestroyEvent
  else
    FEvent.ResetEvent;
end;

{ TdxMultithreadOperation }

constructor TdxMultithreadOperation.Create(AThreadCount: Integer);
begin
  inherited Create;
  FThreadCount := Min(AThreadCount, dxMaxUserWorkItems);
  FThreadPoolHelper := TdxThreadPoolHelper.Create;
end;

constructor TdxMultithreadOperation.Create;
begin
  Create(CPUCount);
end;

destructor TdxMultithreadOperation.Destroy;
begin
  FThreadPoolHelper.Free;
  inherited Destroy;
end;

function TdxMultithreadOperation.Lock: Boolean;
begin
  Result := InterlockedCompareExchange(FLocked, 1, 0) = 0;
end;

procedure TdxMultithreadOperation.Unlock;
begin
  InterlockedCompareExchange(FLocked, 0, 1);
end;

{ TdxMultithreadedSort }

procedure TdxMultithreadedSort.CalculateChunkCount(AItemCount: Integer);
begin
  if AItemCount < dxMaxUserWorkItems then
    FSortChunkCount := 1
  else
    case ThreadCount of
      1: FSortChunkCount := 1;
      2..3: FSortChunkCount := 8;
    else
      FSortChunkCount := 16;
    end;
end;

constructor TdxMultithreadedSort.Create(AThreadCount: Integer);
begin
  inherited Create(AThreadCount);
  ThreadPoolHelper.CreateEvent;
end;

procedure TdxMultithreadedSort.Initialize(ACount: Integer);
begin
  FCount := ACount;
  CalculateChunkCount(ACount);
end;

procedure TdxMultithreadedSort.Sort(AList: PdxPointerList; ACount: Integer; const ACompareFunc: TCompareItems);
begin
  if Lock then
  try
    FCompareFunc := nil;
    FCompareClassFunc := ACompareFunc;
    Initialize(ACount);
    SortListChunks(AList);
    MergeChunks(AList);
  finally
    Unlock;
  end
  else
    dxInternalQuickSortList(AList, ACount, ACompareFunc);
end;

procedure TdxMultithreadedSort.Sort(AList: TdxFastList; const ACompareFunc: TCompareItems);
begin
  Sort(AList.List, AList.Count, ACompareFunc);
end;

procedure TdxMultithreadedSort.Sort(AList: PdxPointerList; ACount: Integer;
  const ACompareFunc: TListSortCompare);
begin
  if Lock then
  try
    FCompareClassFunc := nil;
    FCompareFunc := ACompareFunc;
    Initialize(ACount);
    SortListChunks(AList);
    MergeChunks(AList);
  finally
    Unlock;
  end
  else
    dxInternalQuickSortList(AList, ACount, ACompareFunc);
end;

procedure TdxMultithreadedSort.Sort(AList: TdxFastList;
  const ACompareFunc: TListSortCompare);
begin
  Sort(AList.List, AList.Count, ACompareFunc);
end;

procedure TdxMultithreadedSort.SortListChunks(AList: PdxPointerList);
var
  I, AStartIndex, AEndIndex, AChunkSize, ARemainder: Integer;
begin
  AChunkSize := FCount div FSortChunkCount;
  ARemainder := FCount mod FSortChunkCount;
  AStartIndex := 0;
  ThreadPoolHelper.Initialize(FSortChunkCount);
  for I := 0 to FSortChunkCount - 1 do
  begin
    AEndIndex := AStartIndex + AChunkSize - 1;
    if ARemainder > 0 then
    begin
      Inc(AEndIndex);
      Dec(ARemainder);
    end;
    with FSortChunks[I] do
    begin
      List := AList;
      StartIndex := AStartIndex;
      EndIndex := AEndIndex;
    end;
    ThreadPoolHelper.QueueWorkItem(Self, ParallelSort, @FSortChunks[I]);
    AStartIndex := AEndIndex + 1;
  end;
  ThreadPoolHelper.WaitForThreads;
end;

procedure Merge(AList, ATemp: PdxPointerList;
  ALowBound, ADivider, AHiBound: Integer; const ACompareFunc: TCompareItems); overload;
var
  AIndexA, AIndexB, ATempSize, ACount: Integer;
  ADest, AItemA, AItemB, AHighA, AHighB: PPointer;
begin
  ADest := @AList[ALowBound];
  ATempSize := (AHiBound - ALowBound + 1) * SizeOf(Pointer);
  Move(ADest^, Pointer(@ATemp[ALowBound])^, ATempSize);
  AIndexA := ALowBound;
  AIndexB := ADivider + 1;
  AItemA := @ATemp[AIndexA];
  AHighA := @ATemp[ADivider];
  AItemB := AHighA;
  Inc(AItemB);
  AHighB := @ATemp[AHiBound];
  while (TdxNativeUInt(AItemA) <= TdxNativeUInt(AHighA)) and
    (TdxNativeUInt(AItemB) <= TdxNativeUInt(AHighB)) do
  begin
    if ACompareFunc(AItemA^, AItemB^) < 0 then
    begin
      ADest^ := AItemA^;
      Inc(AItemA);
      Inc(AIndexA);
    end
    else
    begin
      ADest^ := AItemB^;
      Inc(AItemB);
      Inc(AIndexB);
    end;
    Inc(ADest);
  end;
  if AIndexB > AHiBound then
  begin
    ACount := ADivider - AIndexA + 1;
    AItemA := @ATemp[AIndexA];
  end
  else
  begin
    ACount := AHiBound - AIndexB + 1;
    AItemA := @ATemp[AIndexB];
  end;
  if ACount > 0 then
    Move(AItemA^, ADest^, ACount * SizeOf(Pointer));
end;

procedure Merge(AList, ATemp: PdxPointerList;
  ALowBound, ADivider, AHiBound: Integer; const ACompareFunc: TListSortCompare); overload;
var
  AIndexA, AIndexB, ATempSize, ACount: Integer;
  ADest, AItemA, AItemB, AHighA, AHighB: PPointer;
begin
  ADest := @AList[ALowBound];
  ATempSize := (AHiBound - ALowBound + 1) * SizeOf(Pointer);
  Move(ADest^, Pointer(@ATemp[ALowBound])^, ATempSize);
  AIndexA := ALowBound;
  AIndexB := ADivider + 1;
  AItemA := @ATemp[AIndexA];
  AHighA := @ATemp[ADivider];
  AItemB := AHighA;
  Inc(AItemB);
  AHighB := @ATemp[AHiBound];
  while (TdxNativeUInt(AItemA) <= TdxNativeUInt(AHighA)) and
    (TdxNativeUInt(AItemB) <= TdxNativeUInt(AHighB)) do
  begin
    if ACompareFunc(AItemA^, AItemB^) < 0 then
    begin
      ADest^ := AItemA^;
      Inc(AItemA);
      Inc(AIndexA);
    end
    else
    begin
      ADest^ := AItemB^;
      Inc(AItemB);
      Inc(AIndexB);
    end;
    Inc(ADest);
  end;
  if AIndexB > AHiBound then
  begin
    ACount := ADivider - AIndexA + 1;
    AItemA := @ATemp[AIndexA];
  end
  else
  begin
    ACount := AHiBound - AIndexB + 1;
    AItemA := @ATemp[AIndexB];
  end;
  if ACount > 0 then
    Move(AItemA^, ADest^, ACount * SizeOf(Pointer));
end;

procedure TdxMultithreadedSort.MergeChunks(AList: PdxPointerList);
var
  I, AMergeItemCount: Integer;
  ATempList: PdxPointerList;
  AMergeItems: TMergeWorkItemArray;
begin
  AMergeItemCount := (FSortChunkCount) div 2;
  GetMem(ATempList, FCount * SizeOf(Pointer));
  while AMergeItemCount > 0 do
  begin
    ThreadPoolHelper.Initialize(AMergeItemCount);
    for I := 0 to AMergeItemCount - 1 do
    begin
      with AMergeItems[I] do
      begin
        List := AList;
        Temp := ATempList;
        StartIndex  := FSortChunks[I * 2].StartIndex;
        MiddleIndex := FSortChunks[I * 2].EndIndex;
        EndIndex    := FSortChunks[I * 2 + 1].EndIndex;
      end;
      ThreadPoolHelper.QueueWorkItem(Self, ParallelMerge, @AMergeItems[I]);
      FSortChunks[I].StartIndex := FSortChunks[I * 2].StartIndex;
      FSortChunks[I].EndIndex   := FSortChunks[I * 2 + 1].EndIndex;
    end;
    ThreadPoolHelper.WaitForThreads;
    AMergeItemCount := AMergeItemCount div 2;
  end;
  FreeMem(ATempList);
end;

procedure TdxMultithreadedSort.ParallelMerge(Sender: TObject; AContext: Pointer);
var
  AMergeWorkItem: PMergeWorkItem absolute AContext;
begin
  with AMergeWorkItem^ do
    if Assigned(FCompareFunc) then
      Merge(List, Temp, StartIndex, MiddleIndex, EndIndex, FCompareFunc)
    else
      Merge(List, Temp, StartIndex, MiddleIndex, EndIndex, FCompareClassFunc);
end;

procedure TdxMultithreadedSort.ParallelSort(Sender: TObject; AContext: Pointer);
var
  ASortWorkItem: PSortWorkItem absolute AContext;
begin
  with ASortWorkItem^ do
    if Assigned(FCompareFunc) then
      dxInternalQuickSortList(@List[StartIndex], (EndIndex - StartIndex) + 1, FCompareFunc)
    else
      dxInternalQuickSortList(@List[StartIndex], (EndIndex - StartIndex) + 1, FCompareClassFunc);
end;

{ TdxCustomMultithreadedIterator }

function TdxCustomMultithreadedIterator.GetChunkContext(AChunkIndex: Integer;
  AContext: Pointer): Pointer;
begin
  Result := AContext;
end;

function TdxCustomMultithreadedIterator.Initialize(AStartIndex, AEndIndex: Integer; AReverse: Boolean): Boolean;
begin
  FFinishedIndex := -1;
  FCount := AEndIndex - AStartIndex + 1;
  Result := FCount > 0;
  if not Result then
    Exit;
  FReverse := AReverse;
  FStartIndex := AStartIndex;
  FEndIndex := AEndIndex;
  if FCount < ThreadCount then
    FChunkCount := Min(FCount, dxMaxUserWorkItems)
  else
    FChunkCount := Min(ThreadCount, dxMaxUserWorkItems);
end;

procedure TdxCustomMultithreadedIterator.InitializeChunksEnvironment;
begin
end;

{ TdxMultithreadedIterator }

function TdxMultithreadedIterator.ProcessChunks(AContext: Pointer; const AItemFunc: TdxIterateItemFunc): Integer;
var
  I, AStartIndex, AEndIndex, AChunkSize, ARemainder: Integer;
begin
  FItemFunc := AItemFunc;
  AChunkSize := FCount div FChunkCount;
  ARemainder := FCount mod FChunkCount;
  AStartIndex := FStartIndex;
  InitializeChunksEnvironment;
  ThreadPoolHelper.Initialize(FChunkCount, True);
  try
    for I := 0 to FChunkCount - 1 do
    begin
      AEndIndex := AStartIndex + AChunkSize - 1;
      if ARemainder > 0 then
      begin
        Inc(AEndIndex);
        Dec(ARemainder);
      end;
      with FChunks[I] do
      begin
        Context := GetChunkContext(I, AContext);
        StartIndex := AStartIndex;
        EndIndex := AEndIndex;
        Reverse := FReverse;
      end;
      ThreadPoolHelper.QueueWorkItem(Self, ProcessChunk, @FChunks[I]);
      AStartIndex := AEndIndex + 1;
    end;
  finally
    ThreadPoolHelper.WaitForThreads(True);
  end;
  Result := FFinishedIndex;
end;

procedure TdxMultithreadedIterator.ProcessChunk(AContext: TObject; AWorkItem: Pointer);
var
  AChunkWorkItem: PIterateChunkWorkItem absolute AWorkItem;
  I: Integer;
begin
  with AChunkWorkItem^ do
    if Reverse then
      for I := EndIndex downto StartIndex do
      begin
        if FFinishedIndex >= 0 then
          Break
        else
          if FItemFunc(Context, I) then
          begin
            InterlockedExchange(FFinishedIndex, I);
            Break;
          end;
      end
    else
      for I := StartIndex to EndIndex do
      begin
        if FFinishedIndex >= 0 then
          Break
        else
          if FItemFunc(Context, I) then
          begin
            InterlockedExchange(FFinishedIndex, I);
            Break;
          end;
      end;
end;

function TdxMultithreadedIterator.IterateItems(AStartIndex, AEndIndex: Integer;
  AContext: Pointer; const AItemFunc: TdxIterateItemFunc; AReverse: Boolean): Integer;
begin
  if Initialize(AStartIndex, AEndIndex, AReverse) then
    ProcessChunks(AContext, AItemFunc);
  Result := FFinishedIndex;
end;

{ TdxMultithreadedClassMethodIterator }

function TdxMultithreadedClassMethodIterator.ProcessChunks(AContext: Pointer;
  const AItemObjectFunc: TdxIterateItemObjectFunc): Integer;
var
  I, AStartIndex, AEndIndex, AChunkSize, ARemainder: Integer;
begin
  FItemObjectFunc := AItemObjectFunc;
  AChunkSize := FCount div FChunkCount;
  ARemainder := FCount mod FChunkCount;
  AStartIndex := FStartIndex;
  InitializeChunksEnvironment;
  ThreadPoolHelper.Initialize(FChunkCount, True);
  try
    for I := 0 to FChunkCount - 1 do
    begin
      AEndIndex := AStartIndex + AChunkSize - 1;
      if ARemainder > 0 then
      begin
        Inc(AEndIndex);
        Dec(ARemainder);
      end;
      with FChunks[I] do
      begin
        Context := GetChunkContext(I, AContext);
        StartIndex := AStartIndex;
        EndIndex := AEndIndex;
        Reverse := FReverse;
      end;
      ThreadPoolHelper.QueueWorkItem(Self, ProcessChunk, @FChunks[I]);
      AStartIndex := AEndIndex + 1;
    end;
  finally
    ThreadPoolHelper.WaitForThreads(True);
  end;
  Result := FFinishedIndex;
end;

procedure TdxMultithreadedClassMethodIterator.ProcessChunk(AContext: TObject; AWorkItem: Pointer);
var
  AChunkWorkItem: PIterateChunkWorkItem absolute AWorkItem;
  I: Integer;
begin
  with AChunkWorkItem^ do
    if Reverse then
      for I := EndIndex downto StartIndex do
      begin
        if FFinishedIndex >= 0 then
          Break
        else
          if FItemObjectFunc(Context, I) then
          begin
            InterlockedExchange(FFinishedIndex, I);
            Break;
          end;
      end
    else
      for I := StartIndex to EndIndex do
      begin
        if FFinishedIndex >= 0 then
          Break
        else
          if FItemObjectFunc(Context, I) then
          begin
            InterlockedExchange(FFinishedIndex, I);
            Break;
          end;
      end;
end;

function TdxMultithreadedClassMethodIterator.IterateItems(AStartIndex, AEndIndex: Integer;
  AContext: Pointer; const AItemObjectFunc: TdxIterateItemObjectFunc; AReverse: Boolean): Integer;
begin
  if Initialize(AStartIndex, AEndIndex, AReverse) then
    ProcessChunks(AContext, AItemObjectFunc);
  Result := FFinishedIndex;
end;

{ TdxMultithreadedListIterator }

function TdxMultithreadedListIterator.Iterate(AContext: Pointer; ACount: Integer;
  const AItemFunc: TdxIterateItemObjectFunc; AReverse: Boolean = False): Integer;
begin
  Result := DoIterate(ACount, AContext, AItemFunc, AReverse);
end;

function TdxMultithreadedListIterator.Iterate(AList: TdxFastList;
  const AItemFunc: TdxFastListItemFunc; AReverse: Boolean = False): Integer;
begin
  Result := DoIterate(AList.Count, Pointer(AList), TdxIterateItemObjectFunc(AItemFunc), AReverse);
end;

function TdxMultithreadedListIterator.Iterate(AList: TList;
  const AItemFunc: TdxListItemFunc; AReverse: Boolean = False): Integer;
begin
  Result := DoIterate(AList.Count, Pointer(AList), TdxIterateItemObjectFunc(AItemFunc), AReverse);
end;

function TdxMultithreadedListIterator.Iterate(AStrings: TStrings;
  const AItemFunc: TdxStringsListItemFunc; AReverse: Boolean = False): Integer;
begin
  Result := DoIterate(AStrings.Count, Pointer(AStrings), TdxIterateItemFunc(AItemFunc), AReverse);
end;

function TdxMultithreadedListIterator.DoIterate(ACount: Integer;
  AContext: Pointer; const AItemObjectFunc: TdxIterateItemObjectFunc; AReverse: Boolean): Integer;
var
  I: Integer;
begin
  Result := -1;
  if ACount = 0 then
    Exit;
  if (ACount > 1) and dxCanUseMultiThreading and Lock then
    try
      Result := IterateItems(0, ACount - 1, AContext, AItemObjectFunc, AReverse);
    finally
      Unlock;
    end
  else
    if FReverse then
      for I := ACount - 1 downto 0 do
      begin
        if AItemObjectFunc(AContext, I) then
        begin
          Result := I;
          Exit;
        end
      end
    else
      for I := 0 to ACount - 1 do
      begin
        if AItemObjectFunc(AContext, I) then
        begin
          Result := I;
          Exit;
        end;
      end;
end;

{ TdxTask }

procedure TdxTask.Complete;
begin
  CallThreadMethod(FOnComplete, FOnCompleteMode);
end;

function TdxTask.GetCanceled: Boolean;
begin
  Result := FCanceled <> 0;
end;

function TdxTask.GetHandle: THandle;
begin
  Result := THandle(Self);
end;

{ TdxTaskGroup }

constructor TdxTaskGroup.Create;
begin
  FTasks := TObjectList.Create;
  FEvent := TEvent.Create(nil, True, False, '');
end;

destructor TdxTaskGroup.Destroy;
begin
  FreeAndNil(FTasks);
  FreeAndNil(FEvent);
  inherited Destroy;
end;

procedure TdxTaskGroup.AddTask(const AProc: TdxTaskProc);
begin
  AddTask(TdxTaskDispatcher.TProcWrapper.Create(AProc));
end;

procedure TdxTaskGroup.AddTask(const ATask: TdxTask);
begin
  FTasks.Add(ATask);
end;

procedure TdxTaskGroup.Initialize;
begin
  FTasks.Clear;
  FEvent.ResetEvent;
end;

procedure TdxTaskGroup.RunAndWait;
var
  ATask: TTaskWrapper;
  I: Integer;
begin
  if TaskCount > 1 then
  begin
    FActiveTasks := TaskCount;
    for I := 0 to TaskCount - 1 do
    begin
      ATask := TTaskWrapper.Create;
      ATask.FTask := Tasks[I];
      dxTasksDispatcher.Run(ATask, CompleteHandler, tmcmAsync);
    end;
    WaitForSyncObject(FEvent.Handle, INFINITE);
  end
  else
    if TaskCount > 0 then
      Tasks[0].Execute;
end;

procedure TdxTaskGroup.CompleteHandler;
begin
  if InterlockedDecrement(FActiveTasks) = 0 then
    FEvent.SetEvent;
end;

function TdxTaskGroup.GetTask(Index: Integer): TdxTask;
begin
  Result := TdxTask(FTasks.Items[Index])
end;

function TdxTaskGroup.GetTaskCount: Integer;
begin
  Result := FTasks.Count;
end;

{ TdxTaskGroup.TTaskWrapper }

procedure TdxTaskGroup.TTaskWrapper.Complete;
begin
  FTask.Complete;
  inherited;
end;

procedure TdxTaskGroup.TTaskWrapper.Execute;
begin
  inherited;
  FTask.Execute;
end;

{ TdxTaskEvent }

constructor TdxTaskEvent.Create;
begin
  FHandle := CreateEvent(nil, True, False, nil);
end;

destructor TdxTaskEvent.Destroy;
begin
  CloseHandle(FHandle);
  inherited Destroy;
end;

function TdxTaskEvent.Signal: Boolean;
begin
  Result := SetEvent(FHandle);
end;

function TdxTaskEvent.WaitFor(ATimeOut: Cardinal): TWaitResult;
begin
  Result := WaitForSyncObject(FHandle, ATimeOut);
end;

{ TdxTaskDispatcher }

constructor TdxTaskDispatcher.Create;
begin
  inherited Create;
  FTasks := TObjectList.Create;
  FActiveTasks := TList.Create;
  InitializeCriticalSectionAndSpinCount(FLock, 1024);
  FMaxActiveTasks := CPUCount + 2;
{$IFDEF DELPHI101BERLIN}
  FCpuUsageMonitor := TTimer.Create(nil);
  FCpuUsageMonitor.Interval := CpuUsageMonitorUpdateInterval;
  FCpuUsageMonitor.Enabled := False;
  FCpuUsageMonitor.OnTimer := HandlerCpuUsageMonitor;
{$ENDIF}
end;

destructor TdxTaskDispatcher.Destroy;
begin
{$IFDEF DELPHI101BERLIN}
  FreeAndNil(FCpuUsageMonitor);
{$ENDIF}
  FreeAndNil(FActiveTasks);
  FreeAndNil(FTasks);
  DeleteCriticalSection(FLock);
  inherited Destroy;
end;

function TdxTaskDispatcher.Run(const ATask: IdxTask): THandle;
begin
  Result := Run(TTaskWrapper.Create(ATask));
end;

function TdxTaskDispatcher.Run(const AProc: TdxTaskProc): THandle;
begin
  Result := Run(TProcWrapper.Create(AProc));
end;

function TdxTaskDispatcher.Run(const AProc: TdxTaskProc;
  ACompleteEvent: TThreadMethod; ACompleteEventCallMode: TdxThreadMethodCallMode): THandle;
begin
  Result := Run(TProcWrapper.Create(AProc), ACompleteEvent, ACompleteEventCallMode);
end;

function TdxTaskDispatcher.Run(ATask: TdxTask): THandle;
begin
  Result := Run(ATask, TThreadMethod(nil), tmcmAsync);
end;

function TdxTaskDispatcher.Run(ATask: TdxTask;
  ACompleteEvent: TThreadMethod; ACompleteEventCallMode: TdxThreadMethodCallMode): THandle;
begin
  EnterCriticalSection(FLock);
  try
    ATask.FOnComplete := ACompleteEvent;
    ATask.FOnCompleteMode := ACompleteEventCallMode;
    ATask.FStartWaitingAt := GetTickCount;
    Result := ATask.Handle;
    FTasks.Add(ATask);
  finally
    LeaveCriticalSection(FLock);
  end;
  CheckActiveTasks;
end;

function TdxTaskDispatcher.RunInCurrentThread(ATask: TdxTask): THandle;
begin
  Result := 0;
  try
    try
      ATask.Execute;
    finally
      ATask.Complete;
    end;
  finally
    ATask.Free;
  end;
end;

procedure TdxTaskDispatcher.BeforeDestruction;
begin
  inherited BeforeDestruction;
  FMaxActiveTasks := 0;
  CancelAll;
end;

function TdxTaskDispatcher.Cancel(ATaskHandle: THandle; AWaitFor: Boolean = False): Boolean;
begin
  Result := Cancel(ATaskHandle, IfThen(AWaitFor, INFINITE)) <> wrError;
end;

function TdxTaskDispatcher.Cancel(ATaskHandle: THandle; AWaitTimeOut: Cardinal): TWaitResult;
var
  AIndex: Integer;
  ATask: TdxTask;
  AWaitEvent: IdxTaskEvent;
begin
  AWaitEvent := nil;
  if ATaskHandle <> 0 then
  begin
    // Cancel pending item
    EnterCriticalSection(FLock);
    try
      AIndex := FTasks.IndexOf(TdxTask(ATaskHandle));
      if AIndex >= 0 then
      begin
        TdxTask(ATaskHandle).FCanceled := 1;
        TdxTask(ATaskHandle).Complete;
        FTasks.Delete(AIndex);
        Exit(wrSignaled);
      end;

      // Cancel active item
      for AIndex := 0 to FActiveTasks.Count - 1 do
      begin
        ATask := FActiveTasks.List[AIndex];
        if ATaskHandle = ATask.Handle then
        begin
          InterlockedExchange(ATask.FCanceled, 1);
          AWaitEvent := ATask.FEvent;
          Break;
        end;
      end;
    finally
      LeaveCriticalSection(FLock);
    end;
  end;

  if AWaitEvent <> nil then
    Result := AWaitEvent.WaitFor(AWaitTimeOut)
  else
    Result := wrAbandoned;
end;

function TdxTaskDispatcher.IsCurrentTaskCanceled: Boolean;
var
  ATask: TdxTask;
  AThreadId: Cardinal;
  I: Integer;
begin
  EnterCriticalSection(FLock);
  try
    Result := False;
    AThreadId := GetCurrentThreadId;
    for I := 0 to FActiveTasks.Count - 1 do
    begin
      ATask := TdxTask(FActiveTasks.List[I]);
      if ATask.FThreadId = AThreadId then
        Exit(ATask.Canceled)
    end;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

function TdxTaskDispatcher.WaitFor(ATaskHandle: THandle): Boolean;
begin
  Result := WaitFor(ATaskHandle, INFINITE) in SuccessfulWaitResults;
end;

function TdxTaskDispatcher.WaitFor(ATaskHandle: THandle; AWaitTimeOut: Cardinal): TWaitResult;
var
  AIndex: Integer;
  AWaitEvent: IdxTaskEvent;
  ATask: TdxTask;
begin
  AWaitEvent := nil;

  EnterCriticalSection(FLock);
  try
    // if task is pending - activate it now
    AIndex := FTasks.IndexOf(TdxTask(ATaskHandle));
    if AIndex >= 0 then
      Start(TdxTask(FTasks[AIndex]));

    // find task in active work item list
    for AIndex := 0 to FActiveTasks.Count - 1 do
    begin
      ATask := FActiveTasks.List[AIndex];
      if ATaskHandle = ATask.Handle then
      begin
        AWaitEvent := ATask.FEvent;
        Break;
      end;
    end;
  finally
    LeaveCriticalSection(FLock);
  end;

  if AWaitEvent <> nil then
    Result := AWaitEvent.WaitFor(AWaitTimeOut)
  else
    Result := wrAbandoned;
end;

class function TdxTaskDispatcher.ThreadProc(ATask: TdxTask): Integer;
begin
  ATask.FOwner.AsyncRun(ATask);
  Result := 0;
end;

procedure TdxTaskDispatcher.Start(ATask: TdxTask);
begin
  EnterCriticalSection(FLock);
  try
    ATask.FOwner := Self;
    ATask.FEvent := TdxTaskEvent.Create;
    FActiveTasks.Add(FTasks.Extract(ATask));
    if Assigned(FQueueUserWorkItemProc) then
    begin
      if not FQueueUserWorkItemProc(@ThreadProc, ATask, WT_EXECUTELONGFUNCTION) then
        RaiseLastOSError;
    end
    else
      RunInCurrentThread(ATask); // TODO: test it
  finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure TdxTaskDispatcher.AsyncRun(ATask: TdxTask);
begin
  try
    ATask.FThreadId := GetCurrentThreadId;
    try
      try
        ATask.Execute;
      finally
        ATask.Complete;
      end;
    except
  //    FException := ExceptObject;
  //    RunInMainThread(SyncHandleException);
    end;

    EnterCriticalSection(FLock);
    try
      FActiveTasks.Remove(ATask);
      CheckActiveTasks;
    finally
      LeaveCriticalSection(FLock);
    end;

    ATask.FEvent.Signal;
  finally
    ATask.Free;
  end;
end;

procedure TdxTaskDispatcher.CancelAll;
var
  ATaskHandle: THandle;
  I: Integer;
begin
  EnterCriticalSection(FLock);
  try
    for I := 0 to FActiveTasks.Count - 1 do
      Cancel(TdxTask(FActiveTasks.List[I]).Handle, False);
  finally
    LeaveCriticalSection(FLock);
  end;

  while FActiveTasks.Count > 0 do
  begin
    EnterCriticalSection(FLock);
    try
      if FActiveTasks.Count > 0 then
        ATaskHandle := TdxTask(FActiveTasks.First).Handle
      else
        ATaskHandle := 0;
    finally
      LeaveCriticalSection(FLock);
    end;
    Cancel(ATaskHandle, True);
  end;
end;

procedure TdxTaskDispatcher.CheckActiveTasks;
begin
  EnterCriticalSection(FLock);
  try
    if FActiveTasks.Count < FMaxActiveTasks then
    begin
      if FTasks.Count > 0 then
        Start(TdxTask(FTasks.First));
    end;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure TdxTaskDispatcher.SetMaxActiveTasks(AValue: Integer);
begin
  AValue := Max(AValue, 1);
  if FMaxActiveTasks <> AValue then
  begin
    FMaxActiveTasks := AValue;
    CheckActiveTasks;
  end;
end;

{$IFDEF DELPHI101BERLIN}
function TdxTaskDispatcher.GetUseCpuUsageMonitor: Boolean;
begin
  Result := FCpuUsageMonitor.Enabled;
end;

procedure TdxTaskDispatcher.SetUseCpuUsageMonitor(AValue: Boolean);
begin
  if GetCurrentThreadId <> MainThreadID then
    raise EInvalidOperation.Create(ClassName);
  FCpuUsageMonitor.Enabled := AValue;
  FCpuUsageMonitor.Tag := 0;
end;

procedure TdxTaskDispatcher.HandlerCpuUsageMonitor(Sender: TObject);
var
  AAverageCpuUsage: Integer;
  I: Integer;
begin
  for I := Low(FCpuUsageLog) to High(FCpuUsageLog) - 1 do
    FCpuUsageLog[I + 1] := FCpuUsageLog[I];
  FCpuUsageLog[0] := TThread.GetCPUUsage(FPrevSystemTimes);
  FCpuUsageMonitor.Tag := Min(FCpuUsageMonitor.Tag + 1, CpuUsageMonitorLogSize);

  if FCpuUsageMonitor.Tag >= CpuUsageMonitorLogSize then
  begin
    AAverageCpuUsage := 0;
    for I := Low(FCpuUsageLog) to High(FCpuUsageLog) do
      Inc(AAverageCpuUsage, FCpuUsageLog[I]);
    AAverageCpuUsage := AAverageCpuUsage div Length(FCpuUsageLog);

    if AAverageCpuUsage <= CpuUsageLow then
    begin
      EnterCriticalSection(FLock);
      try
        if FTasks.Count > 0 then
        begin
          Start(FTasks.First as TdxTask);
          FCpuUsageMonitor.Tag := 0;
        end;
      finally
        LeaveCriticalSection(FLock);
      end;
    end;
  end;
end;
{$ENDIF}

{ TdxTaskDispatcher.TdxSimpleTask }

constructor TdxTaskDispatcher.TProcWrapper.Create(AProc: TdxTaskProc);
begin
  inherited Create;
  FProc := AProc;
end;

procedure TdxTaskDispatcher.TProcWrapper.Execute;
begin
  if Assigned(FProc) then
    FProc(GetCanceled);
end;

{ TdxTaskDispatcher.TdxTaskWrapper }

constructor TdxTaskDispatcher.TTaskWrapper.Create(const ATask: IdxTask);
begin
  inherited Create;
  FTask := ATask;
end;

procedure TdxTaskDispatcher.TTaskWrapper.Complete;
begin
  CallThreadMethod(SyncComplete, tmcmSync);
end;

procedure TdxTaskDispatcher.TTaskWrapper.Execute;
begin
  FCompletedStatus := FTask.Run(GetCanceled);
end;

procedure TdxTaskDispatcher.TTaskWrapper.SyncComplete;
begin
  FTask.OnComplete(FCompletedStatus);
end;

{ TdxThreadSyncService }

class constructor TdxUIThreadSyncService.Initialize;
begin
  FHandle := AllocateHWnd(WndProc);
  FDelegates := TList<TEnqueuedProcedure>.Create;
  FDelegates.Capacity := 4096;
  InitializeCriticalSectionAndSpinCount(FLock, 1024);
  FEmptyDelegate.Owner := nil;
  FEmptyDelegate.ProcedureRef := nil;
end;

{$HINTS OFF}
class destructor TdxUIThreadSyncService.Finalize;
begin
  FShutdown := True;
  DeleteAllListeners;
  ClearWindowsQueue;
  DeallocateHWnd(FHandle);
  DeleteCriticalSection(FLock);
end;
{$HINTS ON}

class procedure TdxUIThreadSyncService.Invoke;
var
  AQueueItem: TEnqueuedProcedure;
begin
  while not IsShutDown and (FDelegates.Count > 0) do
  begin
    try
      EnterCriticalSection(FLock);
      try
        AQueueItem := FDelegates.First;
        FDelegates[0] := FEmptyDelegate;
        FDelegates.Delete(0);
      finally
        LeaveCriticalSection(FLock);
      end;
      try
        AQueueItem.ProcedureRef();
      except
        Application.HandleException(AQueueItem.Owner);
      end;
    finally
      Finalize(AQueueItem);
    end;
  end;
end;

class function TdxUIThreadSyncService.IsShutdown: Boolean;
begin
  Result := FShutDown or ((Application <> nil) and Application.Terminated);
end;

class procedure TdxUIThreadSyncService.Unsubscribe(AListener: TObject);
var
  I: Integer;
begin
  Assert(AListener <> nil, 'bad listener');
  if IsShutDown then
    Exit;
  EnterCriticalSection(FLock);
  try
    for I := FDelegates.Count - 1 downto 0 do
      if AListener = FDelegates[I].Owner then
      begin
        FDelegates[I] := FEmptyDelegate;
        FDelegates.Delete(I);
      end;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

class procedure TdxUIThreadSyncService.DeleteAllListeners;
var
  I: Integer;
begin
  for I := 0 to FDelegates.Count - 1 do
    FDelegates[I] := FEmptyDelegate;
  FDelegates.Free;
end;

class procedure TdxUIThreadSyncService.ClearWindowsQueue;
var
  Msg: TMsg;
begin
  while PeekMessage(Msg, 0, DXM_UITHREADINVOKE, DXM_UITHREADINVOKE, PM_REMOVE) do
    DispatchMessage(Msg);
end;

class procedure TdxUIThreadSyncService.EnqueueInvokeInUIThread(AOwner: TObject; const AProcedure: TProc);
var
  AQueueItem: TEnqueuedProcedure;
begin
  if IsShutDown then
    Exit;
  EnterCriticalSection(FLock);
  try
    AQueueItem.Owner := AOwner;
    AQueueItem.ProcedureRef := AProcedure;

    FDelegates.Add(AQueueItem);
    PostMessage(FHandle, DXM_UITHREADINVOKE, 0, 0);
  finally
    LeaveCriticalSection(FLock);
  end;
end;

class procedure TdxUIThreadSyncService.WndProc(var Message: TMessage);
begin
  if not IsShutDown and (Message.Msg = DXM_UITHREADINVOKE) then
  try
    Invoke;
  finally
    Message.Result := 1;
  end
  else
    Message.Result := DefWindowProc(FHandle, Message.Msg, Message.wParam, Message.lParam);
end;

function dxCanUseMultiThreading: Boolean;
begin
  Result := FCanUseMultiThreading and dxEnableMultiThreading;
end;

procedure InitThreading;
begin
  FQueueUserWorkItemProc := GetProcAddress(GetModuleHandle(kernel32), 'QueueUserWorkItem');
  FCanUseMultiThreading := Assigned(FQueueUserWorkItemProc) and (CPUCount > 1);
  if FCanUseMultiThreading then
  begin
    IsMultiThread := True;
    dxSortHelper := TdxMultithreadedSort.Create(CPUCount * 4);
    dxListIterator := TdxMultithreadedListIterator.Create(CPUCount);
  end;
  dxTasksDispatcher := TdxTaskDispatcher.Create;
end;

procedure DoneThreading;
begin
  FreeAndNil(dxTasksDispatcher);
  FreeAndNil(dxSortHelper);
  FreeAndNil(dxListIterator);
end;

initialization
  InitThreading;

finalization
  DoneThreading;

end.
