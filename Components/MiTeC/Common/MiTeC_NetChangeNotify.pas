{*******************************************************}
{               MiTeC Common Routines                   }
{           Network/IP address change notifier          }
{                                                       }
{                                                       }
{         Copyright (c) 2016-2017 Michal Mutl           }
{                                                       }
{*******************************************************}

{$INCLUDE Compilers.inc}

unit MiTeC_NetChangeNotify;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils, System.Classes,
     {$ELSE}
     Windows, SysUtils, Classes,
     {$ENDIF}
     MiTeC_IpHlpAPI;

type
  TNetworkNotifyEvent = procedure(Sender: TObject; ARes: Cardinal) of object;

  TWatchNetworkChangeThread = class(TThread)
  private
    FNotifyProc: TNetworkNotifyEvent;
    FRes :Cardinal;
    procedure DoSync;
  protected
    procedure Execute; override;
  public
    constructor Create(ANotifyProc: TNetworkNotifyEvent);
  end;

  TNetworkChangeNotifier = class(TObject)
  private
    FOnChange: TNetworkNotifyEvent;
    FThread: TThread;
    procedure DoNotify(Sender: TObject; ARes: Cardinal);
    function GetThreadId: Cardinal;
  public
    constructor Create;
    destructor Destroy; override;
    property ThreadId: Cardinal read GetThreadId;
    property OnChange: TNetworkNotifyEvent read FOnChange write FOnChange;
  end;


implementation

{ TNetworkChangeNotifier }

constructor TNetworkChangeNotifier.Create;
begin
  inherited;
  FThread:=TWatchNetworkChangeThread.Create(DoNotify);
end;

destructor TNetworkChangeNotifier.Destroy;
begin
  if Assigned(FThread) then
    TerminateThread(FThread.Handle,0);
  inherited;
end;

procedure TNetworkChangeNotifier.DoNotify(Sender: TObject; ARes: Cardinal);
begin
  if Assigned(FOnChange) then
    FOnChange(Self,ARes);
  if not Assigned(FThread) then
    FThread:=TWatchNetworkChangeThread.Create(DoNotify);
end;

function TNetworkChangeNotifier.GetThreadId: Cardinal;
begin
  Result:=0;
  if Assigned(FThread) then
    Result:=FThread.ThreadID;
end;

{ TWatchNetworkChangeThread }

constructor TWatchNetworkChangeThread.Create(ANotifyProc: TNetworkNotifyEvent);
begin
  FNotifyProc:=ANotifyProc;
  inherited Create(False);
  FreeOnTerminate:=True;
end;

procedure TWatchNetworkChangeThread.DoSync;
begin
  FNotifyProc(Self,FRes);
end;

procedure TWatchNetworkChangeThread.Execute;
var
  h: THandle;
  ovlp: TOverlapped;
  r,size: Cardinal;
  pBuf: PAnsiChar;
begin
  size:=SizeOf(TIP_INTERFACE_INFO);
  pBuf:=AllocMem(size);
  ovlp.hEvent:=CreateEvent(nil,False,False,nil);
  try
    while not Terminated do begin
      r:=GetInterfaceInfo(PIP_INTERFACE_INFO(pBuf),size);
      while(r=ERROR_INSUFFICIENT_BUFFER) do begin
        size:=Size+SizeOf(TIP_INTERFACE_INFO);
        ReallocMem(pBuf,size);
        r:=GetInterfaceInfo(PIP_INTERFACE_INFO(pBuf),size);
      end;
      if(r=ERROR_SUCCESS) and (PIP_INTERFACE_INFO(pBuf).NumAdapters>0) then begin
        r:=NotifyAddrChange(@h,@ovlp);
        if (r=NO_ERROR) or (r=ERROR_IO_PENDING) then begin
          while True do begin
            if Terminated then
              Exit;
            case WaitForSingleObject(ovlp.hEvent,500) of
              WAIT_TIMEOUT: Continue;
              WAIT_OBJECT_0: Break;
              else Exit;
            end;
          end;
          Synchronize(DoSync);
        end;
      end else
        Sleep(1000);
    end
  finally
    try FreeMem(pBuf) except end;
    CloseHandle(ovlp.hEvent);
  end;
end;

end.
