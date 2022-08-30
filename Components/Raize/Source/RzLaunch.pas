{===============================================================================
  RzLaunch Unit

  Raize Components - Component Source Unit

  Copyright © 1995-2015 by Raize Software, Inc.  All Rights Reserved.


  Components
  ------------------------------------------------------------------------------
  TRzLauncher
    Used to launch another process from within your application.


  Modification History
  ------------------------------------------------------------------------------
  6.1.6  (15 Feb 2014)
    * Added new GetWindowHandle function to TRzLauncher. After launching or
      executing a process, the GetWindowHandle method can be called to retrieve
      the top-level window handle associated with the process that was started.
  ------------------------------------------------------------------------------
  3.0.13 (15 May 2005)
    * Fixed problem where setting ShowMode to smMinimized would not result in
      the launched/executed app being minimized.  Changed the mapping of
      smMinimized from sw_ShowMinimized to sw_ShowMinNoActive.
  ------------------------------------------------------------------------------
  3.0.10 (26 Dec 2003)
    * When the Execute method is used to start a process, the HInstance and
      HProcess properties are now set to the values of the process that was
      started.
    * As a result of the above change, the Execute method will no start a
      process if the component is currently monitoring another process that was
      started using the Launch method. 
  ------------------------------------------------------------------------------
  3.0.5  (24 Mar 2003)
    * When WaitUntilFinished is False, Launch method will not return until
      Running is set to True.
    * Added new Execute method that starts the specified process without
      monitoring for termination.
  ------------------------------------------------------------------------------
  3.0    (20 Dec 2002)
    * Added the Running property.
    * Event handlers are now generated within the context of the main
      application thread.
    * When WaitUntilFinished is True, the new WaitType property controls whether
      the application completely stops, while waiting for the launched process
      to finish (wtFullStop), or to continue processing messages while waiting
      for the launched process to finish (wtProcessMessages).
===============================================================================}

{$I RzComps.inc}

unit RzLaunch;

interface

uses
  {$IFDEF USE_CS}
  CodeSiteLogging,
  {$ENDIF}
  SysUtils,
  Windows,
  Messages,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  ExtCtrls,
  RzCommon;

type
  TShowMode = ( smNormal, smMaximized, smMinimized, smHide );

const
  ShowWindowModes: array[ TShowMode ] of Integer =
    ( sw_Normal, sw_ShowMaximized, sw_ShowMinNoActive, sw_Hide );

type
  TRzLauncher = class;

  TRzLaunchThread = class( TThread )
  private
    FLauncher: TRzLauncher;
  protected
    procedure Execute; override;
  public
    constructor Create( ALauncher: TRzLauncher );
  end;

  TRzLaunchErrorEvent = procedure( Sender: TObject; ErrorCode: DWord ) of object;

  TRzWaitType = ( wtFullStop, wtProcessMessages );

  TRzLauncher = class( TComponent )
  private
    FAboutInfo: TRzAboutInfo;
    FHInstance: THandle;
    FAction: string;
    FFileName: string;
    FParameters: string;
    FShowMode: TShowMode;
    FStartDir: string;
    FTimeout: Integer;
    FWaitType: TRzWaitType;
    FWaitUntilFinished: Boolean;
    FOnFinished: TNotifyEvent;
    FOnTimeout: TNotifyEvent;
    FOnError: TRzLaunchErrorEvent;
    FExitCode: DWord;
    FLastErrorCode: DWord;
    FHProcess: THandle;
    FRunning: Boolean;
    FBackgroundThread: TRzLaunchThread;
  protected
    procedure StartProcess;

    procedure Finished; dynamic;
    procedure DoTimeout; dynamic;
    procedure LaunchError; dynamic;

    procedure WaitForProcessAndProcessMsgs; virtual;
    procedure WaitForProcessFullStop; virtual;
    procedure WaitForProcessFromThread; virtual;
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

    procedure Launch; virtual;
    function Execute: DWord; virtual;

    function GetErrorMsg( ErrorCode: DWord ): string;
    function GetWindowHandle: HWnd;

    property HInstance: THandle
      read FHInstance;

    property ExitCode: DWord
      read FExitCode;

    property HProcess: THandle
      read FHProcess;

    property Running: Boolean
      read FRunning;
  published
    property About: TRzAboutInfo
      read FAboutInfo
      write FAboutInfo
      stored False;

    property Action: string
      read FAction
      write FAction;

    property FileName: string
      read FFileName
      write FFileName;

    property Parameters: string
      read FParameters
      write FParameters;

    property ShowMode: TShowMode
      read FShowMode
      write FShowMode
      default smNormal;

    property StartDir: string
      read FStartDir
      write FStartDir;

    property Timeout: Integer
      read FTimeout
      write FTimeout;

    property WaitType: TRzWaitType
      read FWaitType
      write FWaitType
      default wtFullStop;

    property WaitUntilFinished: Boolean
      read FWaitUntilFinished
      write FWaitUntilFinished
      default False;

    property OnFinished: TNotifyEvent
      read FOnFinished
      write FOnFinished;

    property OnError: TRzLaunchErrorEvent
      read FOnError
      write FOnError;

    property OnTimeout: TNotifyEvent
      read FOnTimeout
      write FOnTimeout;
  end;


{$IFNDEF VCL150_OR_HIGHER}
function GetProcessId( Process: THandle ): DWORD; stdcall;
{$EXTERNALSYM GetProcessId}
{$ENDIF}

implementation

uses
  {&RAS}
  ShellApi;


resourcestring
  sRzLaunchOutOfMemory            = 'Out of memory or executable file is corrupt';
  sRzLaunchFileNotFound           = 'File was not found';
  sRzLaunchPathNotFound           = 'Path was not found';
  sRzLaunchSharingViolation       = 'Sharing violation or netword error';
  sRzLaunchSeparateDataSeg        = 'A library required separate data segments for each task';
  sRzLaunchInsufficientMemory     = 'Insufficient memory to start application';
  sRzLaunchIncorrectWindowsVer    = 'Incorrect version of Windows';
  sRzLaunchInvalidEXE             = 'File is not a Windows application or there was an error in the .EXE image';
  sRzLaunchIncorrectOS            = 'Application was designed for a different operating system';
  sRzLaunchForMSDos4              = 'Application was designed for MS-DOS 4.0';
  sRzLaunchUnknownType            = 'Unknown executable file type';
  sRzLaunchLoadRealMode           = 'Cannot load a real-mode application';
  sRzLaunchNonReadOnlyDataSeg     = 'Cannot load a second instance of an executable file containing multiple, non-read-only data segments';
  sRzLaunchCompressedEXE          = 'Cannot load a compressed executable file';
  sRzLaunchInvalidDLL             = 'A dynamic-link library (DLL) file is invalid';
  sRzLaunchWin32                  = 'Application requires Windows 32-bit extensions';
  sRzLaunchNoAssociation          = 'No association for specified file type';



{============================}
{= TRzLaunchThread Methods ==}
{============================}

constructor TRzLaunchThread.Create( ALauncher: TRzLauncher );
begin
  inherited Create( False );
  FLauncher := ALauncher;
  FreeOnTerminate := True;
end;


procedure TRzLaunchThread.Execute;
begin
  if FLauncher <> nil then
    FLauncher.StartProcess;
end;


{&RT}
{=========================}
{== TRzLauncher Methods ==}
{=========================}

constructor TRzLauncher.Create( AOwner: TComponent );
begin
  inherited;
  FShowMode := smNormal;
  FHInstance := 0;
  FAction := 'Open';
  {&RCI}
  FHProcess := 0;
  FExitCode := 0;
  FTimeout := -1{INFINITE};  {INFINTIE was changed to a DWord in D4 }

  FRunning := False;
  FWaitType := wtFullStop;
  FWaitUntilFinished := False;
  {&RV}
end;


destructor TRzLauncher.Destroy;
begin
  if FRunning and not FWaitUntilFinished and ( FBackgroundThread <> nil ) and not FBackgroundThread.Terminated then
  begin
    FBackgroundThread.Terminate;
    Sleep( 200 ); // Give it some time to clean up
  end;

  inherited;
end;


procedure TRzLauncher.Finished;
begin
  if Assigned( FOnFinished ) then
    FOnFinished( Self );
end;


function TRzLauncher.GetErrorMsg( ErrorCode: DWord ): string;
begin
  case ErrorCode of
    0: Result := sRzLaunchOutOfMemory;
    2: Result := sRzLaunchFileNotFound;
    3: Result := sRzLaunchPathNotFound;
    5: Result := sRzLaunchSharingViolation;
    6: Result := sRzLaunchSeparateDataSeg;
    8: Result := sRzLaunchInsufficientMemory;
    10: Result := sRzLaunchIncorrectWindowsVer;
    11: Result := sRzLaunchInvalidEXE;
    12: Result := sRzLaunchIncorrectOS;
    13: Result := sRzLaunchForMSDos4;
    14: Result := sRzLaunchUnknownType;
    15: Result := sRzLaunchLoadRealMode;
    16: Result := sRzLaunchNonReadOnlyDataSeg;
    19: Result := sRzLaunchCompressedEXE;
    20: Result := sRzLaunchInvalidDLL;
    21: Result := sRzLaunchWin32;
    31: Result := sRzLaunchNoAssociation;
    else
      Result := 'Unknown Error';
  end;
end;


procedure TRzLauncher.LaunchError;
begin
  if Assigned( FOnError ) then
    FOnError( Self, FLastErrorCode );
end;


procedure TRzLauncher.DoTimeout;
begin
  if Assigned( FOnTimeout ) then
    FOnTimeout( Self );
end;


procedure TRzLauncher.WaitForProcessAndProcessMsgs;
begin
  repeat
    case MsgWaitForMultipleObjects( 1, FHProcess, False, Cardinal( FTimeout ),
                                    QS_POSTMESSAGE or QS_SENDMESSAGE or QS_ALLPOSTMESSAGE ) of
      WAIT_OBJECT_0:
      begin
        GetExitCodeProcess( FHProcess, FExitCode );
        Finished;
        Break;
      end;

      WAIT_OBJECT_0 + 1:
      begin
        Application.ProcessMessages;
      end;

      WAIT_TIMEOUT:
      begin
        DoTimeout;
        Break;
      end;
    end;

  until False;
end; {= TRzLauncher.WaitForProcessAndProcessMsgs =}


procedure TRzLauncher.WaitForProcessFullStop;
begin
  case WaitForSingleObject( FHProcess, Cardinal( FTimeout ) ) of
    WAIT_FAILED:
    begin
      FLastErrorCode := GetLastError;
      LaunchError;
    end;

    WAIT_OBJECT_0:
    begin
      GetExitCodeProcess( FHProcess, FExitCode );
      Finished;
    end;

    WAIT_TIMEOUT:
      DoTimeout;
  end; { case }
end; {= TRzLauncher.WaitForProcessFullStop =}


procedure TRzLauncher.WaitForProcessFromThread;
var
  Done: Boolean;
  TimeoutCount: Cardinal;
begin
  Done := False;
  TimeoutCount := 0;
  repeat
    case WaitForSingleObject( FHProcess, Cardinal( 100 ) ) of
      WAIT_FAILED:
      begin
        FLastErrorCode := GetLastError;
        FBackgroundThread.Synchronize( LaunchError );
        Done := True;
      end;

      WAIT_OBJECT_0:
      begin
        GetExitCodeProcess( FHProcess, FExitCode );
        FBackgroundThread.Synchronize( Finished );
        Done := True;
      end;

      WAIT_TIMEOUT:
      begin
        Inc( TimeoutCount, 100 );
        if TimeoutCount >= Cardinal( FTimeout ) then
        begin
          FBackgroundThread.Synchronize( DoTimeout );
          Done := True;
        end;
      end;
    end; { case }
  until Done or FBackgroundThread.Terminated;
end;


procedure TRzLauncher.StartProcess;
var
  ShellInfo: TShellExecuteInfo;
begin
  FLastErrorCode := 0;
  FHInstance := 0;
  FHProcess := 0;
  FExitCode := 0;

  FillChar( ShellInfo, SizeOf( TShellExecuteInfo ), 0 );
  ShellInfo.cbSize := SizeOf( TShellExecuteInfo );
  ShellInfo.fMask := SEE_MASK_NOCLOSEPROCESS or SEE_MASK_FLAG_NO_UI or SEE_MASK_FLAG_DDEWAIT;
  ShellInfo.Wnd := HWnd_Desktop;
  ShellInfo.lpVerb := PChar( FAction );
  ShellInfo.lpFile := PChar( FFileName );
  ShellInfo.lpParameters := PChar( FParameters );
  ShellInfo.lpDirectory := PChar( FStartDir );
  ShellInfo.nShow := ShowWindowModes[ FShowMode ];

  if ShellExecuteEx( @ShellInfo ) then
  begin
    FHInstance := ShellInfo.hInstApp;
    FHProcess := ShellInfo.hProcess;
    FRunning := True;

    try
      if FWaitUntilFinished then
      begin
        if FWaitType = wtFullStop then
          WaitForProcessFullStop
        else
          WaitForProcessAndProcessMsgs;
      end
      else
        WaitForProcessFromThread;
    finally
      CloseHandle( FHProcess );
      FRunning := False;
    end;
  end
  else
  begin
    FLastErrorCode := ShellInfo.hInstApp;
    if FWaitUntilFinished then
      LaunchError
    else
      FBackgroundThread.Synchronize( LaunchError );
  end;
end; {= TRzLauncher.StartProcess =}



procedure TRzLauncher.Launch;
begin
  if FRunning or ( FFileName = '' ) then
    Exit;

  if FWaitUntilFinished then
    StartProcess
  else
  begin
    FBackgroundThread := TRzLaunchThread.Create( Self );
    repeat
      Sleep( 10 );
    until FRunning or ( FLastErrorCode <> 0 );
  end;
end;


function TRzLauncher.Execute: DWord;
var
  ShellInfo: TShellExecuteInfo;
begin
  if FRunning or ( FFileName = '' ) then
  begin
    Result := 0;
    Exit;
  end;

  FHInstance := 0;
  FHProcess := 0;
  FExitCode := 0;

  FillChar( ShellInfo, SizeOf( TShellExecuteInfo ), 0 );
  ShellInfo.cbSize := SizeOf( TShellExecuteInfo );
  ShellInfo.fMask := SEE_MASK_NOCLOSEPROCESS or SEE_MASK_FLAG_NO_UI or SEE_MASK_FLAG_DDEWAIT;
  ShellInfo.Wnd := HWnd_Desktop;
  ShellInfo.lpVerb := PChar( FAction );
  ShellInfo.lpFile := PChar( FFileName );
  ShellInfo.lpParameters := PChar( FParameters );
  ShellInfo.lpDirectory := PChar( FStartDir );
  ShellInfo.nShow := ShowWindowModes[ FShowMode ];

  if ShellExecuteEx( @ShellInfo ) then
  begin
    FHInstance := ShellInfo.hInstApp;
    FHProcess := ShellInfo.hProcess;
    Result := 0;
  end
  else
    Result := ShellInfo.hInstApp;
end; {= TRzLauncher.Execute =}


{$IFNDEF VCL150_OR_HIGHER}
function GetProcessId; external kernel32 name 'GetProcessId';
{$ENDIF}


function TRzLauncher.GetWindowHandle: HWnd;
var
  H: HWnd;
  ProcessID, PID: DWord;
begin
  Result := 0;
  if FHProcess = 0 then
    Exit;

  H := GetTopWindow( 0 );
  ProcessID := GetProcessID( FHProcess );
  if ProcessID = 0 then
    Exit;

  while H <> 0 do
  begin
    if GetParent( H ) = 0 then
    begin
      GetWindowThreadProcessID( H, PID );
      if PID = ProcessID then
      begin
        Result := H;
        Exit;
      end;
    end;
    H := GetWindow( H, gw_HwndNext );
  end;
end;



{&RUIF}
end.
