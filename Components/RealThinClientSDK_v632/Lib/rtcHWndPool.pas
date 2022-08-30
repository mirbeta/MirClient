{
  "Windows Handle Pool"
  - Copyright 2004-2014 (c) RealThinClient.com (http://www.realthinclient.com)
  @html(<br>)

  This unit implements a pool of Window Handles, associated with their threads.
  Since creating Windows Handles takes a considerable ammount of processing time
  and each Windows handle is a valuable resource, this unit allows using a limited
  number of Window Handles to work with any number of objects.

  This unit is ONLY for MS Windows.

  @exclude
}
unit rtcHWndPool;

{$INCLUDE rtcDefs.inc}

{$IFNDEF WINDOWS}
  {$MESSAGE WARN 'rtcHwndPool unit is ONLY for MS Windows.'}
{$ENDIF}

interface

uses
  Windows,
  Messages,

  SysUtils,
  Classes,

  rtcTypes,
  rtcLog,

  rtcSyncObjs;

const
  // Number of Window Handles for use in the main thread
  RTC_MAIN_WINDOW_HANDLES = 5;

var
  // Number of Window Handles for use in background threads
  RTC_THREAD_WINDOW_HANDLES: integer = 5;

type
  PRtcHWNDProc = ^TRtcHWNDProc;
  TRtcHWNDProc = function( ahWnd  : HWND;
                           auMsg   : LongWord;
                           awParam : WPARAM;
                           alParam : LPARAM ) : Integer;

var
  { It is recommended to use normal thread priority for the HWND thread.
    Do NOT change this value unless there is a very good reason for it. }
  RTC_HWND_THREAD_PRIORITY:TThreadPriority=tpNormal;

{ Register a procedure which should be used for processing all messages
  in range Msg_Low to Msg_High. If a message in this range is received,
  procedure "_proc" will ALWAYS be used to process it. }
procedure rtcRegisterHWNDProc(_proc:PRtcHWNDProc; Msg_Low, Msg_High:LongWord);

{ Get a single-threaded or a multi-threaded Windows handle.
  A single-threaded handle will execute code in the main thread.
  Multi-Threaded handle will execute code from a background thread. }
function rtcGetHWND(Multi_Threaded:boolean):HWND;

implementation

var
  // Class Name to be used when creating windows
  RTC_HWND_CLASS_NAME:PChar='';

type
  tHWndThread=class(TThread)
  public
    constructor Create(CreateSuspended:boolean);
    destructor Destroy; override;

    procedure Execute; override;
    end;

  TRtcHWNDProcData=record
    proc:TRtcHWNDProc;
    msg_lo,msg_hi:LongWord;
    end;

var
  MyHdl:array[0..RTC_MAIN_WINDOW_HANDLES-1] of HWND;
  MyHdl_MT:array of HWND;

  NextMyHdl,NextMyHdl_MT:integer;
  CSHWND:TRtcCritSec;
  thr:THwndThread;
  _Inside,_Outside:TRtcEvent;

  HWNDProcs:array of TRtcHWNDProcData;

procedure rtcRegisterHWNDProc(_proc:PRtcHWNDProc; Msg_Low, Msg_High:LongWord);
  begin
  CSHWND.Acquire;
  try
    SetLength(HWNDProcs,length(HWNDProcs)+1);
    with HWNDProcs[length(HWNDProcs)-1] do
      begin
      msg_lo:=Msg_Low;
      msg_hi:=Msg_High;
      @proc:=_proc;
      end;
  finally
    CSHWND.Release;
    end;
  end;

function RtcMainHWNDWindowProc(ahWnd   : HWND;
                               auMsg   : LongWord;
                               awParam : WPARAM;
                               alParam : LPARAM): Integer; stdcall;
  var
    a:integer;
  begin
  try
    for a:=0 to length(HWNDProcs)-1 do
      with HWNDProcs[a] do
        if (auMsg>=msg_lo) and (auMsg<=msg_hi) then
          begin
          Result:=proc(ahWnd,auMsg,awParam,alParam);
          Exit;
          end;
    Result := DefWindowProc(ahWnd, auMsg, awParam, alParam);
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('WndProc_MAIN(Wnd='+IntToStr(ahWnd)+', '+
                         'Msg='+IntToStr(auMsg)+', '+
                         'wParam='+IntToStr(awParam)+', '+
                         'lParam='+IntToStr(alParam)+')',E,'ERROR');
      Result:=0;
      end;
    end;
  end;

{ This global variable is used to store the windows class characteristic    }
{ and is needed to register the window class used by TWSocket               }
var
  XWindowClassRegistered:boolean=False;

  RtcMainHWNDWindowClass: TWndClass = (
        style         : 0;
        lpfnWndProc   : @RtcMainHWNDWindowProc;
        cbClsExtra    : 0;
        cbWndExtra    : SizeOf(Pointer);
        hInstance     : 0;
        hIcon         : 0;
        hCursor       : 0;
        hbrBackground : 0;
        lpszMenuName  : nil;
        lpszClassName : 'RtcMainHWNDWindowClass');

function HWND_RegisterClass:integer;
  var
    TempClass       : TWndClass;
    ClassRegistered : BOOL;
  begin
  Result:=0;
  if not XWindowClassRegistered then
    begin
    { Check if the window class is already registered                   }
    RtcMainHWNDWindowClass.hInstance := HInstance;
    ClassRegistered := GetClassInfo(HInstance,
                                    RtcMainHWNDWindowClass.lpszClassName,
                                    TempClass);
    if not ClassRegistered then
      begin
      { Not yet registered, do it right now                            }
      Result := Windows.RegisterClass(RtcMainHWNDWindowClass);
      if Result = 0 then Exit;
      end;
    RTC_HWND_CLASS_NAME:=RtcMainHWNDWindowClass.lpszClassName;
    XWindowClassRegistered:=True;
    end
  else
    Result:=1;
  end;

{ Unregister the window class use by the component. This is necessary to do }
{ so from a DLL when the DLL is unloaded (that is when DllEntryPoint is     }
{ called with dwReason equal to DLL_PROCESS_DETACH.                         }
procedure HWND_UnregisterClass;
  begin
  if XWindowClassRegistered then
    begin
    Windows.UnregisterClass(RtcMainHWNDWindowClass.lpszClassName, HInstance);
    XWindowClassRegistered:=False;
    end;
  end;

function RtcAllocateHWnd:HWND;
  begin
  Result := CreateWindowEx(WS_EX_TOOLWINDOW,
                           RTC_HWND_CLASS_NAME,
                           '',        { Window name   }
                           WS_POPUP,  { Window Style  }
                           0, 0,      { X, Y          }
                           0, 0,      { Width, Height }
                           0,         { hWndParent    }
                           0,         { hMenu         }
                           HInstance, { hInstance     }
                           nil);      { CreateParam   }
  SetWindowLong(Result,GWL_USERDATA, 0);
  end;

function RtcDeallocateHWnd(Wnd: HWND): boolean;
  begin
  Result := DestroyWindow(Wnd);
  end;

{ tHWndThread -> will be catching all multithreaded Window Handle messages }

constructor tHWndThread.Create(CreateSuspended: boolean);
  begin
  {$IFDEF RTC_DEBUG}
    Log('tHWndThread.Create','DEBUG');
  {$ENDIF}
  FreeOnTerminate:=True;
  inherited Create(CreateSuspended);
  Priority:=RTC_HWND_THREAD_PRIORITY;
  end;

destructor tHWndThread.Destroy;
  begin
  try
  {$IFDEF RTC_DEBUG}
    Log('tHWndThread.Destroy','DEBUG');
  {$ENDIF}
    inherited;
    _Outside.SetEvent;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('THWndThread.Destroy',E,'ERROR');
      _Outside.SetEvent;
      // raise;
      end;
    end;
  end;

procedure tHWndThread.Execute;
  var
    MsgRec:TMsg;
    a:integer;
  begin
  SetLength(MyHdl_MT,RTC_THREAD_WINDOW_HANDLES);
  for a:=0 to length(MyHdl_MT)-1 do
    MyHdl_MT[a]:=RtcAllocateHWnd;
  _Inside.SetEvent;
  while GetMessage(MsgRec,0,0,0) do
    begin
      begin
      TranslateMessage(MsgRec);
      DispatchMessage(MsgRec);
      end;
    end;
  end;

function rtcGetHWND(Multi_Threaded:boolean):HWND;
  begin
  CSHWND.Acquire;
  try
    if Multi_Threaded then
      begin
      if not assigned(thr) then
        begin
        thr:=THWndThread.Create(False);
        _Inside.WaitFor(INFINITE); // wait for the thread to start and create a thread window handle.
        end;
      if length(MyHdl_MT)>0 then
        begin
        Result:=MyHdl_MT[NextMyHdl_MT];
        Inc(NextMyHdl_MT);
        if NextMyHdl_MT>=length(MyHdl_MT) then
          NextMyHdl_MT:=0;
        end
      else
        begin
        Result:=0;
        raise Exception.Create('Windows Message Thread terminated.');
        end
      end
    else
      begin
      Result:=MyHdl[NextMyHdl];
      Inc(NextMyHdl);
      if NextMyHdl>=RTC_MAIN_WINDOW_HANDLES then
        NextMyHdl:=0;
      end;
  finally
    CSHWND.Release;
    end;
  end;

type
  TRtcHWNDPoolUnit=class
    public
    constructor Create;
    destructor Destroy; override;
    end;

var
  myHWNDPool:TRtcHWNDPoolUnit;

constructor TRtcHWNDPoolUnit.Create;
  var
    a:integer;
  begin
  inherited;
  
  thr:=nil;
  SetLength(MyHdl_MT,0);
  CSHWND:=TRtcCritSec.Create;

  _Inside:=TRtcEvent.Create(True,False);
  _Outside:=TRtcEvent.Create(True,False);

  HWND_RegisterClass;

  for a:=0 to RTC_MAIN_WINDOW_HANDLES-1 do
    MyHdl[a]:=RtcAllocateHWnd;
  end;

destructor TRtcHWNDPoolUnit.Destroy;
  var
    a:integer;

  procedure WaitForClose(_timeout:cardinal);
  {$IFDEF WINDOWS}
    var
      Msg:TMsg;
      MyTime:cardinal;
    begin
    MyTime:=GetTickCount+_Timeout*1000;
    while _Outside.WaitFor(10)<>wr_Signaled do
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
      if GetTickCount>=MyTime then Exit;
      end;

    { Wait 0.1 seconds to allow the thread to terminate. }

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
      Sleep(10);
      end;
    end;
  {$ELSE}
    begin
    _Outside.WaitFor(_Timeout*1000);
    end;
  {$ENDIF}
  begin
  try
    if assigned(thr) then
      begin
      // Stop background thread ...
      if (_Inside.WaitFor(0)=wr_Signaled) and
         (_Outside.WaitFor(0)<>wr_Signaled) then
        begin
        if length(MyHdl_MT)>0 then
          begin
          {$IFDEF RTC_DEBUG} Log('rtcHWndPool waiting on thread close ...','DEBUG');{$ENDIF}
          PostThreadMessage(RtcThrID(thr.ThreadID),WM_QUIT,0,0);
          WaitForClose(10); // wait up to 10 seconds for the thread to close
          {$IFDEF RTC_DEBUG} Log('rtcHWndPool thread closed.','DEBUG');{$ENDIF}
          end;
        end {$IFDEF RTC_DEBUG} else Log('rtcHWndPool thread was already closed.','DEBUG'){$ENDIF} ;
      // Allow the thread object to be destroyed
      end;

    for a:=0 to RTC_MAIN_WINDOW_HANDLES-1 do
      RtcDeallocateHWnd(MyHdl[a]);

    if length(MyHdl_MT)>0 then
      begin
      for a:=0 to length(MyHdl_MT)-1 do
        RtcDeallocateHWnd(MyHdl_MT[a]);
      SetLength(MyHdl_MT,0);
      end;

    HWND_UnregisterClass;

    SetLength(HWNDProcs,0);
    RtcFreeAndNil(_Inside);
    RtcFreeAndNil(_Outside);
    RtcFreeAndNil(CSHWND);
    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcHWNDPoolUnit.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

initialization
{$IFDEF RTC_DEBUG} Log('rtcHWndPool Initializing ...','DEBUG');{$ENDIF}

myHWNDPool:=TRtcHWNDPoolUnit.Create;

{$IFDEF RTC_DEBUG} Log('rtcHwndPool Initialized.','DEBUG');{$ENDIF}
finalization
{$IFDEF RTC_DEBUG} Log('rtcHWndPool Finalizing ...','DEBUG');{$ENDIF}

RtcFreeAndNil(myHWNDPool);

{$IFDEF RTC_DEBUG} Log('rtcHWndPool Finalized.','DEBUG');{$ENDIF}
end.
