{***************************************************************************}
{ TMacroRecorder component                                                  }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2005 - 2012                                        }
{            Email : info@tmssoftware.com                                   }
{            Web : http://www.tmssoftware.com                               }
{                                                                           }
{ The source code is given as is. The author is not responsible             }
{ for any possible damage done due to the use of this code.                 }
{ The component can be freely used in any application. The complete         }
{ source code remains property of the author and may not be distributed,    }
{ published, given or sold in any form as such. No parts of the source      }
{ code can be included in any other component or application without        }
{ written authorization of the author.                                      }
{***************************************************************************}

unit MacroRecorder;

interface

uses
  Windows, Messages, SysUtils, Classes, AppEvnts, Dialogs, Forms, Types;

const
  MAXMSG = 20000;

  MAJ_VER = 1; // Major version nr.
  MIN_VER = 2; // Minor version nr.
  REL_VER = 1; // Release nr.
  BLD_VER = 0; // Build nr.

  // version history
  // 1.1.0.0 : New : event OnRecordCancelled added
  //         : New : event OnPlaybackCancelled added
  // 1.2.0.0 : New : functions IsPlaying, IsRecording added
  //           New : procedure StopPlayback added
  // 1.2.1.0 : New : exposed public property ElapsedTime 

type
  PEventMsg = ^TEventMsg;
  TMsgBuff = array[0..MAXMSG] of TEventMsg;

  TcbPlaybackFinishedProc = procedure(AppData: Longint) of object;

  TcbRecordFinishedProc = procedure(Sender: TObject) of object;

  TRecordOption = (roMouseMove, roMouseRelative);
  TRecordOptions = set of TRecordOption;

  TPlaybackSpeed = (pbNormal, pbFast);
  TRecordingRange = (rrSystem, rrApplication);

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TMacroRecorder = class(TComponent)
  private
    FElapsedTime: longint;
    FOwner: TComponent;
    FPlayback: boolean;
    FFileName: string;
    FOnPlaybackFinished: TcbPlaybackFinishedProc;
    FPlaybackSpeed: TPlaybackSpeed;
    FApplicationEvents: TApplicationEvents;
    FApplicationOnActivate: TNotifyEvent;
    FApplicationOnDeActivate: TNotifyEvent;
    FRecordingRange: TRecordingRange;
    FOptions: TRecordOptions;
    FOnRecordFinished: TcbRecordFinishedProc;
    FOnRecordCancelled: TNotifyEvent;
    FOnPlaybackCancelled: TNotifyEvent;
    procedure SetFileName(value: string);
    procedure SetPlaybackSpeed(value: TPlaybackSpeed);
    procedure PlaybackFinished(AppData: Longint);
    procedure ApplicationOnActivate(Sender: TObject);
    procedure ApplicationOnDeActivate(Sender: TObject);
    procedure SetRecordingRange(value: TRecordingRange);
    procedure ApplicationEventsMessage(var Msg: tagMSG; var Handled: Boolean);
    function KeyToString(aCode: Cardinal): string;
    function StringToKey(S: string): Cardinal;
    function GetVersion: string;
    procedure SetVersion(const Value: string);
  protected
    function GetVersionNr: Integer; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function IsPlaying: boolean;
    function IsRecording: boolean;
    procedure RecordMacro;
    procedure StopRecording;
    procedure StopPlayback;
    procedure PlayMacro;
    procedure SaveMacro;
    procedure LoadMacro;
    property ElapsedTime: longint read FElapsedTime write FElapsedTime;
  published
    property FileName: string read FFileName write SetFileName;
    property PlaybackSpeed: TPlaybackSpeed read FPlaybackSpeed write SetPlaybackSpeed;
    property RecordingRange: TRecordingRange read FRecordingRange write SetRecordingRange;
    property OnPlaybackFinished: TcbPlaybackFinishedProc read FOnPlaybackFinished write FOnPlaybackFinished;
    property OnPlaybackCancelled: TNotifyEvent read FOnPlayBackCancelled write FOnPlayBackCancelled;
    property OnRecordFinished: TcbRecordFinishedProc read FOnRecordFinished write FOnRecordFinished;
    property OnRecordCancelled: TNotifyEvent read FOnRecordCancelled write FOnRecordCancelled;
    property Options: TRecordOptions read FOptions write FOptions;
    property Version: string read GetVersion write SetVersion;
  end;

function JournalProc(Code, wParam: Integer; var EventStrut: TEventMsg): Integer; stdcall;
function JournalPlaybackProc(Code: Integer; wParam: integer; var EventStrut: TEventMsg): integer; stdcall;

var
  JHook: THandle;
  PMsgBuff: ^TMsgBuff;
  StartTime: Longint;
  MsgCount: Longint;
  CurrentMsg: Longint;
  ReportDelayTime: Bool;
  SysModalOn: Bool;
  cbPlaybackFinishedProc: TcbPlaybackFinishedProc;
  cbAppData: Longint;
  DoMouseMove: Boolean;
  DoRelative: Boolean;
  FHookStarted: Boolean;
  gPlaybackSpeed: TPlaybackSpeed;
  gRecordingRange: TRecordingRange;
  gOutOfRange: Boolean;
  gFirstEvent: Boolean;
  RecorderInstance: TMacroRecorder;
  pt: TPoint;

implementation

{
  this is the JournalRecordProc
  The lParam parameter contains a pointer to a TEventMsg
  structure containing information on
  the message removed from the system message queue.
}

function JournalProc(Code, wParam: Integer; var EventStrut: TEventMsg): Integer; stdcall;
var
 Evnt: TEventMsg;
begin
  Result := CallNextHookEx(JHook, Code, wParam, Longint(@EventStrut));
  {the CallNextHookEX is not really needed for journal hook since it it not
  really in a hook chain, but it's standard for a Hook}
  if Code < 0 then Exit;

  {cancel operation if get HC_SYSMODALON}
  if Code = HC_SYSMODALON then
  begin
    SysModalOn := True;
    CallNextHookEx(JHook, Code, wParam, Longint(@EventStrut));
    Exit;
  end;

  if Code = HC_SYSMODALOFF then
  begin
    SysModalOn := False;
    CallNextHookEx(JHook, Code, wParam, Longint(@EventStrut));
    Exit;
  end;

  if Code = HC_ACTION then
  begin
    if SysModalOn then
      Exit;

    Evnt := EventStrut;

    if MsgCount > MAXMSG then
      Exit;

    if (EventStrut.message = WM_MOUSEMOVE) and DoMouseMove then
      Exit;

    if (EventStrut.message = WM_MOUSEMOVE) or
       (EventStrut.message = WM_LBUTTONDOWN) or
       (EventStrut.message = WM_LBUTTONDBLCLK) or
       (EventStrut.message = WM_LBUTTONUP) or
       (EventStrut.message = WM_RBUTTONDOWN) or
       (EventStrut.message = WM_RBUTTONUP) or
       (EventStrut.message = WM_RBUTTONDBLCLK) or
       (EventStrut.message = WM_MBUTTONDOWN) or
       (EventStrut.message = WM_MBUTTONUP) or
       (EventStrut.message = WM_MBUTTONDBLCLK) or
       (EventStrut.message = WM_MOUSEWHEEL) then
    begin
      if (DoRelative) then
      begin
        pt := Point(Evnt.paramL, Evnt.paramH);

        Windows.ScreenToClient(GetActiveWindow,pt);

        Evnt.paramL := pt.X;
        Evnt.paramH := pt.Y;
      end;
    end;

    if (EventStrut.message = WM_KEYDOWN) then
    begin
      if (eventstrut.paraml and $FF = VK_CANCEL) then
      begin
        RecorderInstance.StopRecording;
        Exit;
      end;
    end;

    if (gRecordingRange = rrApplication) and gOutOfRange then Exit;

    {record the message}
//    PMsgBuff^[MsgCount] := PEventMsg(Longint(@EventStrut))^;
    PMsgBuff^[MsgCount] := Evnt;

    {set the delta time of the message}
    //Dec(PMsgBuff^[MsgCount].Time, StartTime);


    if gFirstEvent then
    begin
      StartTime := PMsgBuff^[MsgCount].Time;
      PMsgBuff^[MsgCount].Time := 0;
      gFirstEvent := false;
    end
    else
      Dec(PMsgBuff^[MsgCount].Time, StartTime);

    Inc(MsgCount);
  end;
end;

procedure TMacroRecorder.ApplicationEventsMessage(var Msg: tagMSG; var Handled: Boolean);
begin
  {the journal hook is automaticly cancelled if the Task manager
  (Ctrl-Alt-Del) or the Ctrl-Esc keys are pressed, restart it
  when the WM_CANCELJOURNAL is sent to the parent window, Application}

  Handled := False;
  if (Msg.message = WM_CANCELJOURNAL) and FPlayback then
  begin
    FPlayback := false;
    if Assigned(FOnPlayBackCancelled) then
      FOnPlayBackCancelled(self);
  end;
  
  if (Msg.message = WM_CANCELJOURNAL) and FHookStarted then
  begin
    if Assigned(OnRecordCancelled) then
      OnRecordCancelled(Self);
    StopRecording;
    //JHook := SetWindowsHookEx(WH_JOURNALRECORD, @JournalProc, 0, 0);
  end;
end;

function JournalPlaybackProc(Code: Integer; wParam: integer; var EventStrut: TEventMsg): integer; stdcall;
var
  TimeToFire: Longint;
  Evnt: TEventMsg;
  pt: TPoint;
begin
  Result := 0;
  case Code of
    HC_SKIP:
      begin
     {get the next message}
        Inc(CurrentMsg);
        ReportDelayTime := True;
     {Is finished?}
        if CurrentMsg >= (MsgCount - 1) then
          if JHook <> 0 then
            if UnHookWindowsHookEx(JHook) = True then
            begin
              JHook := 0;
              FreeMem(PMsgBuff, Sizeof(TMsgBuff));
              PMsgBuff := nil;
           {callback to the application announcing we are finished}
              cbPlaybackFinishedProc(cbAppData);
            end;
        exit;
      end;

    HC_GETNEXT:
      begin
     {play the current message}

        Evnt := PMsgBuff^[CurrentMsg];

        if (Evnt.message = WM_MOUSEMOVE) or
           (Evnt.message = WM_LBUTTONDOWN) or
           (Evnt.message = WM_LBUTTONUP) or
           (Evnt.message = WM_LBUTTONDBLCLK) or
           (Evnt.message = WM_RBUTTONDOWN) or
           (Evnt.message = WM_RBUTTONUP) or
           (Evnt.message = WM_RBUTTONDBLCLK) or
           (Evnt.message = WM_MBUTTONDOWN) or
           (Evnt.message = WM_MBUTTONUP) or
           (Evnt.message = WM_MBUTTONDBLCLK) or
           (Evnt.message = WM_MOUSEWHEEL) then
        begin
          pt := Point(Evnt.paraml, Evnt.paramH);
          ClientToScreen(GetActiveWindow,pt);
          Evnt.paramL := pt.X;
          Evnt.paramH := pt.Y;
        end;

        PEventMsg(Longint(@EventStrut))^ := Evnt;

//        PEventMsg(Longint(@EventStrut))^ := PMsgBuff^[CurrentMsg];

        if gPlaybackSpeed = pbNormal then
          PEventMsg(Longint(@EventStrut))^.Time := longint(StartTime) + longint(PMsgBuff^[CurrentMsg].Time);

        RecorderInstance.ElapsedTime := longint(PEventMsg(Longint(@EventStrut))^.Time) - longint(StartTime);

     {if first time this message has played - report the delay time}
        if ReportDelayTime then
        begin
          ReportDelayTime := False;
          TimeToFire := PEventMsg(Longint(@EventStrut))^.Time - GetTickCount;
          if TimeToFire > 0 then Result := TimeToFire;
        end;
        Exit;
      end;

    HC_SYSMODALON:
      begin
     {something is wrong}
        SysModalOn := True;
        CallNextHookEx(JHook, Code, wParam, Longint(@EventStrut));
        exit;
      end;

    HC_SYSMODALOFF:
      begin
     {we have been hosed by the system - our hook has been pulled!}
        SysModalOn := False;
        JHook := 0;
        FreeMem(PMsgBuff, Sizeof(TMsgBuff));
        PMsgBuff := nil;
     {callback to the application announcing we are finished}
        cbPlaybackFinishedProc(cbAppData);
        CallNextHookEx(JHook, Code, wParam, Longint(@EventStrut));
        exit;
      end;

  end;
  if code < 0 then
    Result := CallNextHookEx(JHook, Code, wParam, Longint(@EventStrut));
end;

constructor TMacroRecorder.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if not (Owner is TForm) then
    raise exception.Create('Control parent must be a form!');

  FOwner := AOwner;

  FApplicationOnActivate := Application.OnActivate;
  Application.OnActivate := ApplicationOnActivate;

  FApplicationOnDeActivate := Application.OnDeActivate;
  Application.OnDeActivate := ApplicationOnDeActivate;

  cbPlaybackFinishedProc := PlaybackFinished;

  DoMouseMove := True;
  DoRelative := True;

  FPlaybackSpeed := pbNormal;
  gPlaybackSpeed := FPlaybackSpeed;

  FRecordingRange := rrSystem;
  gRecordingRange := FRecordingRange;
  gOutOfRange := false;

  FOptions := [roMouseMove, roMouseRelative];

  FApplicationEvents := TApplicationEvents.Create(self);
  FApplicationEvents.OnMessage := ApplicationEventsMessage;
end;

destructor TMacroRecorder.Destroy;
begin
  {unhook on app closes}
  if FHookStarted then
    UnhookWindowsHookEx(JHook);

  Application.OnActivate := FApplicationOnActivate;
  Application.OnDeActivate := FApplicationOnDeActivate;
  inherited;
end;

procedure TMacroRecorder.RecordMacro;
begin
  if FHookStarted then
  begin
    ShowMessage('Mouse is already being Journaled, can not restart');
    Exit;
  end;

  RecorderInstance := self;

  if FFileName = '' then raise exception.Create('Invalid FileName');

  if pMsgBuff <> nil then Exit;
  GetMem(PMsgBuff, Sizeof(TMsgBuff));
  if PMsgBuff = nil then exit;
  SysModalOn := False;
  MsgCount := 0;
  gFirstEvent := true;
  StartTime := GetTickCount;

  DoMouseMove := not (roMouseMove in Options);
  DoRelative := (roMouseRelative in Options);

  JHook := SetWindowsHookEx(WH_JOURNALRECORD, @JournalProc, hInstance, 0);
  {SetWindowsHookEx starts the Hook}
  if JHook > 0 then
  begin
    FHookStarted := True;
  end
  else
  begin
    FreeMem(PMsgBuff, Sizeof(TMsgBuff));
    PMsgBuff := nil;
    ShowMessage('No Journal Hook available');
  end;
end;

procedure TMacroRecorder.StopPlayback;
begin
  if FPlayback then
  begin
    FPlayback := false;
    UnhookWindowsHookEx(JHook);
    JHook := 0;
  end;
end;

procedure TMacroRecorder.StopRecording;
begin
  FHookStarted := False;
  if PMsgBuff = nil then
    Exit;

  UnhookWindowsHookEx(JHook);
  JHook := 0;

  SaveMacro;

  FreeMem(PMsgBuff, Sizeof(TMsgBuff));
  PMsgBuff := nil;

  if Assigned(OnRecordFinished) then
    OnRecordFinished(Self);
end;

function TMacroRecorder.IsPlaying: boolean;
begin
  Result := FPlayback;
end;

function TMacroRecorder.IsRecording: boolean;
begin
  Result := FHookStarted;
end;

procedure TMacroRecorder.PlayMacro;
begin
  if FFileName = '' then raise exception.Create('Invalid FileName');

  if PMsgBuff <> nil then
    Exit;

  GetMem(PMsgBuff, Sizeof(TMsgBuff));
  if PMsgBuff = nil then
    Exit;

  LoadMacro;

  CurrentMsg := 0;
  ReportDelayTime := True;
  SysModalOn := False;

  cbAppData := 0;
  StartTime := GetTickCount;

  FElapsedTime := 0;

  RecorderInstance := self;

  FPlayback := true;
  JHook := SetWindowsHookEx(WH_JOURNALPLAYBACK, @JournalPlayBackProc, hInstance, 0);

  if JHook = 0 then
  begin
    FreeMem(PMsgBuff, Sizeof(TMsgBuff));
    PMsgBuff := nil;
    FPlayback := false;
    Exit;
  end;
end;

procedure TMacroRecorder.PlaybackFinished(AppData: Longint);
begin
  StopPlayback;

  if Assigned(FOnPlaybackFinished) then
    FOnPlaybackFinished(AppData);
end;

procedure TMacroRecorder.LoadMacro;
var
  ST1, ST2: TStringList;
  i: integer;
begin
  //if PMsgBuff <> nil then exit;
  if not FileExists (FFileName) then 
    Exit;
  ST1 := TStringList.Create;
  ST2 := TStringList.Create;

  try
    ST2.LoadFromFile(FFileName);
    ST1.CommaText := ST2[0];
    MsgCount := strtoint(ST1.Values['MessageCount']);

    for i := 1 to ST2.Count - 1 do
    begin
      ST1.Clear;
      ST1.CommaText := ST2[i];
      PMsgBuff[i - 1].message := StrToInt(ST1.Values[ST1.Names[0]]);
      if (PMsgBuff[i - 1].message = WM_KEYDOWN) or (PMsgBuff[i - 1].message = WM_KEYUP) then
        PMsgBuff[i - 1].paramL := StringToKey(ST1.Values['ParamL'])
      else
        PMsgBuff[i - 1].paramL := StrToInt(ST1.Values['ParamL']);

      PMsgBuff[i - 1].paramH := StrToInt(ST1.Values['ParamH']);
      PMsgBuff[i - 1].time := StrToInt(ST1.Values['Time']);
      PMsgBuff[i - 1].hwnd := StrToInt(ST1.Values['hwnd']);
    end;
  finally

    ST1.Free;
    ST2.Free;
  end;
end;

function TMacroRecorder.KeyToString(aCode: Cardinal): string;
begin
  case aCode of
    7745: Result := 'a';
    12354: Result := 'b';
    11843: Result := 'c';
    8260: Result := 'd';
    4677: Result := 'e';
    8518: Result := 'f';
    8775: Result := 'g';
    9032: Result := 'h';
    5961: Result := 'i';
    9290: Result := 'j';
    9547: Result := 'k';
    9804: Result := 'l';
    12877: Result := 'm';
    12622: Result := 'n';
    6223: Result := 'o';
    6480: Result := 'p';
    4177: Result := 'q';
    4946: Result := 'r';
    8019: Result := 's';
    5204: Result := 't';
    5717: Result := 'u';
    12118: Result := 'v';
    4439: Result := 'w';
    11608: Result := 'x';
    5465: Result := 'y';
    11354: Result := 'z';
  else
    Result := IntToStr(aCode);
  end;
end;

function TMacroRecorder.StringToKey(S: string): Cardinal;
var
  c: Char;
begin
  if length(S) = 1 then
  begin
    c := S[1];
    case c of
      'a': Result := 7745;
      'b': Result := 12354;
      'c': Result := 11843;
      'd': Result := 8260;
      'e': Result := 4677;
      'f': Result := 8518;
      'g': Result := 8775;
      'h': Result := 9032;
      'i': Result := 5961;
      'j': Result := 9290;
      'k': Result := 9547;
      'l': Result := 9804;
      'm': Result := 12877;
      'n': Result := 12622;
      'o': Result := 6223;
      'p': Result := 6480;
      'q': Result := 4177;
      'r': Result := 4946;
      's': Result := 8019;
      't': Result := 5204;
      'u': Result := 5717;
      'v': Result := 12118;
      'w': Result := 4439;
      'x': Result := 11608;
      'y': Result := 5465;
      'z': Result := 11354;
    else
      Result := StrToInt(S);
    end;
  end
  else
  begin
    Result := StrToInt(S);
  end;
end;

procedure TMacroRecorder.SaveMacro;
var
  ST1, ST2: TStringList;
  i,p: integer;
  S: string;

begin
  if PMsgBuff = nil then
    Exit;

  if MsgCount > 0 then
  begin
    ST1 := TStringList.Create;
    ST2 := TStringList.Create;
    try
      ST1.Values['MessageCount'] := inttostr(MsgCount);
      ST2.Add(ST1.CommaText);
      S := '';
      for i := 0 to MsgCount do
      begin
        ST1.Clear;
        case PMsgBuff[i].message of
        WM_MOUSEMOVE: S := 'MOUSEMOVE';
        WM_LBUTTONDOWN: S := 'LBUTTONDOWN';
        WM_LBUTTONUP: S := 'LBUTTONUP';
        WM_LBUTTONDBLCLK: S := 'LBUTTONDBLCLK';
        WM_RBUTTONDOWN: S := 'RBUTTONDOWN';
        WM_RBUTTONUP: S := 'RBUTTONUP';
        WM_RBUTTONDBLCLK: S := 'RBUTTONDBLCLK';
        WM_MBUTTONDOWN: S := 'MBUTTONDOWN';
        WM_MBUTTONUP: S := 'MBUTTONUP';
        WM_MBUTTONDBLCLK: S := 'MBUTTONDBLCLK';
        WM_MOUSEWHEEL: S := 'MOUSEWHEEL';

        WM_KEYDOWN: S := 'KEYDOWN';
        WM_KEYUP: S := 'KEYUP';
        WM_CHAR: S := 'CHAR';
        WM_DEADCHAR: S := 'DEADCHAR';
        WM_SYSKEYDOWN: S := 'SYSKEYDOWN';
        WM_SYSKEYUP: S := 'SYSKEYUP';
        WM_SYSCHAR: S := 'SYSCHAR';
        WM_SYSDEADCHAR: S := 'SYSDEADCHAR';
        WM_KEYLAST: S := 'KEYLAST';
        else
          S := 'UnKnown';
        end;
 
        p := PMsgBuff[i].paramL;

        ST1.Values[S] := IntToStr(PMsgBuff[i].message);
        if (S = 'KEYDOWN') or (S = 'KEYUP') then
          ST1.Values['ParamL'] := KeyToString(p)
        else
          ST1.Values['ParamL'] := IntToStr(p);

        p := PMsgBuff[i].paramH;
 
        ST1.Values['ParamH'] := IntToStr(p);
        ST1.Values['Time'] := InttoStr(PMsgBuff[i].time);
        ST1.Values['hwnd'] := inttoStr(PMsgBuff[i].hwnd);
        ST2.Add(ST1.CommaText);
      end;
      ST2.SaveToFile(FFileName);
    finally  
      ST1.Free;
      ST2.Free;
    end;  
  end;
end;

procedure TMacroRecorder.SetFileName(value: string);
begin
  if FHookStarted then
    raise exception.Create('Can not modify file name while recording Macro.');
  FFileName := value
end;

procedure TMacroRecorder.SetPlaybackSpeed(value: TPlaybackSpeed);
begin
  FPlaybackSpeed := value;
  GPlaybackSpeed := FPlaybackSpeed;
end;

procedure TMacroRecorder.ApplicationOnActivate(Sender: TObject);
begin
  gOutOfRange := false;
  if Assigned(FApplicationOnActivate) then FApplicationOnActivate(Sender);
end;

procedure TMacroRecorder.ApplicationOnDeActivate(Sender: TObject);
begin
  if FRecordingRange = rrApplication then gOutOfRange := true;

  if Assigned(FApplicationOnDeActivate) then FApplicationOnDeActivate(Sender);
end;

procedure TMacroRecorder.SetRecordingRange(value: TRecordingRange);
begin
  if FHookStarted then raise exception.Create('Can not change property while recording macro.');
  FRecordingRange := value;
  gRecordingRange := FRecordingRange;
end;

function TMacroRecorder.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TMacroRecorder.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TMacroRecorder.SetVersion(const Value: string);
begin

end;

end.
