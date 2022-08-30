{********************************************************************}
{ TMSWHEEL component                                                 }
{ for Delphi & C++Builder                                            }
{                                                                    }
{ written by                                                         }
{    TMS Software                                                    }
{    copyright © 1997-2012                                           }
{    Email : info@tmssoftware.com                                    }
{    Web : http://www.tmssoftware.com                                }
{                                                                    }
{ The source code is given as is. The author is not responsible      }
{ for any possible damage done due to the use of this code.          }
{ The component can be freely used in any application. The source    }
{ code remains property of the author and may not be distributed     }
{ freely as such.                                                    }
{********************************************************************}

unit mswheel;

{$I TMSDEFS.INC}

interface

uses
  SysUtils, Windows, Messages, Classes, Forms, ShellAPI, Grids, Stdctrls, Controls, Types;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 5; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.

  // version history
  // v1.0.0.0 : First release

type
  TMouseWheelEvent = procedure(zdelta,xpos,ypos,ScrollLines:integer) of object;
  TOnIntelliEvent = procedure(zdelta,xpos,ypos,ScrollLines:integer) of object;

  EMSWheelError = class(Exception);

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TMSWheel = class(TComponent)
  private
    wheelscrl:integer;
    wheelmsg:DWORD;
    { Private declarations }
    function ScrollLines:integer;
    function GetVersion: string;
    procedure SetVersion(const Value: string);
  protected
    { Protected declarations }
    OldWndProc: TFarProc;
    NewWndProc: Pointer;
    FOnWheelEvent:TMouseWheelEvent;
    FOnIntelliScroll:TOnIntelliEvent;
    FOnIntelliZoom:TOnIntelliEvent;
    FOnIntelliPan:TOnIntelliEvent;
    FOnIntelliDataZoom:TOnIntelliEvent;
    FOnIntelliAutoScroll:TOnIntelliEvent;
    FAuto:boolean;
    procedure HookWndProc(var Msg: TMessage);
    function GetVersionNr: Integer; virtual;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  published
    { Published declarations }
    property Auto: boolean read FAuto write FAuto;
    property Version: string read GetVersion write SetVersion;
    property OnWheelEvent: TMouseWheelEvent read FOnWheelEvent write FOnWheelEvent;
    property OnIntelliScroll:TOnIntelliEvent read FOnIntelliScroll write FOnIntelliScroll;
    property OnIntelliZoom:TOnIntelliEvent read FOnIntelliZoom write FOnIntelliZoom;
    property OnIntelliDataZoom:TOnIntelliEvent read FOnIntelliDataZoom write FOnIntelliDataZoom;
  end;


implementation

uses
  Registry;

type
  {$IFDEF DELPHIXE_LVL}
  LInteger = LONG_PTR;
  LIntParam = LPARAM;
  {$ENDIF}
  {$IFNDEF DELPHIXE_LVL}
  LInteger = Integer;
  LIntParam = Integer;
  {$ENDIF}

const
  WM_MOUSEWHEEL  = $020A;
  SPI_GETWHEELSCROLLLINES = 104;

  MSH_MOUSEWHEEL = 'MSWHEEL_ROLLMSG';
  MOUSEZ_CLASSNAME  = 'MouseZ';           // wheel window class
  MOUSEZ_TITLE      = 'Magellan MSWHEEL'; // wheel window title
  MSH_WHEELSUPPORT  = 'MSH_WHEELSUPPORT_MSG'; // name of msg to send
                                              // to query for wheel support
  MSH_SCROLL_LINES = 'MSH_SCROLL_LINES_MSG';

  SPI_SETWHEELSCROLLLINES  = 105;

  MSH_WHEELMODULE_CLASS = MOUSEZ_CLASSNAME;
  MSH_WHEELMODULE_TITLE = MOUSEZ_TITLE;

function TMSWheel.ScrollLines:integer;
var
 mshwheel:thandle;
 scrollmsg:integer;
 i:integer;
 verinfo:tosversioninfo;

begin
 verinfo.dwOSVersionInfoSize:=sizeof(tosversioninfo);
 getversionex(verinfo);

 if (verinfo.dwPlatformId = VER_PLATFORM_WIN32_WINDOWS) or

    ((verinfo.dwPlatformId = VER_PLATFORM_WIN32_NT) AND
     (verinfo.dwMajorVersion < 4)) then
   begin
   ScrollLines:=0;
   mshwheel:=FindWindow(MSH_WHEELMODULE_CLASS,MSH_WHEELMODULE_TITLE);


   if (mshwheel>0) then
    begin
     scrollmsg:=RegisterWindowMessage(MSH_SCROLL_LINES);
     i:=SendMessage(mshwheel,scrollmsg,0,0);
     scrollLines:=i;
    end;
   end

  else {its win nt 4+}
   begin
    SystemParametersInfo(SPI_GETWHEELSCROLLLINES,0,@i,0);
    scrollLines:=i;
   end;

end;

{$WARNINGS OFF}
constructor tmswheel.Create(AOwner:TComponent);
var
  I, Instances: Integer;
begin
  inherited Create(AOwner);
  if not (Owner is TForm) then
    raise EMSWheelError.Create('Control parent must be a form!');
  Instances := 0;
  for I := 0 to Owner.ComponentCount - 1 do
    if (Owner.Components[I] is tmswheel) then
      Inc(Instances);
  if (Instances > 1) then
    raise EMSWheelError.Create('Only one instance of TMSWheel allowed on form');

  { Hook parent }

  OldWndProc := TFarProc(GetWindowLong((Owner as TForm).Handle, GWL_WNDPROC));
  {$IFDEF DELPHI7_LVL}
  NewWndProc := Classes.MakeObjectInstance(HookWndProc);
  {$ELSE}
  NewWndProc := MakeObjectInstance(HookWndProc);  
  {$ENDIF}
  SetWindowLong((Owner as TForm).Handle, GWL_WNDPROC, LInteger(NewWndProc));

  {do mousewheel registering here}
  wheelscrl:=self.scrolllines;
  wheelmsg:=RegisterWindowMessage(MSH_MOUSEWHEEL);
end;  { tmswheel.Create }


destructor tmswheel.Destroy;
begin
  { Unhook parent }
  if (Owner<>nil) and Assigned(OldWndProc) then
    SetWindowLong((Owner as TForm).Handle, GWL_WNDPROC, LInteger(OldWndProc));
  if Assigned(NewWndProc) then
  {$IFDEF DELPHI7_LVL}
    Classes.FreeObjectInstance(NewWndProc);
  {$ELSE}
    FreeObjectInstance(NewWndProc);  
  {$ENDIF}
  { Clean up }
  inherited Destroy;
end;  { tmswheel.Destroy }
{$WARNINGS ON}

procedure tmswheel.HookWndProc(var Msg: TMessage);
var
 i:integer;
 Control: TWinControl;
 Num: integer;

 function hi_int(i:integer):integer;
 begin
  i:=hiword(i);
  if i>32768 then i:=i-65536;
  hi_int:=i;
 end;

begin
  with Msg do
   begin
     if (msg=WM_MOUSEWHEEL) then {this is the NT case}
      begin
       if (assigned(OnWheelEvent)) then
         begin
           FOnWheelEvent(hi_int(wparam),loword(lparam),hiword(lparam),wheelscrl);

           if FAuto then
            begin
             Control := FindVCLWindow(Point(loword(lparam),hiword(lparam)));
             if (Control <> nil) then
             if (Control is TStringGrid) then
              begin
               i:=(control as TStringGrid).row;
               Num:=wparam div 120;
               i:=i+(-num*wheelscrl);
               if (i<(control as TStringGrid).fixedrows) then i:=(control as TStringGrid).fixedrows;
               if (i>(control as TStringGrid).rowcount-1) then i:=(control as TStringGrid).rowcount-1;
               (control as TStringGrid).row:=i;
              end;
            end
         end;


       if ((wparam and MK_CONTROL)>0) then
           if assigned(OnIntelliZoom) then
             FOnIntelliZoom(hi_int(wparam),loword(lparam),hiword(lparam),wheelscrl);

       if ((wparam and MK_SHIFT)>0) then
           if assigned(OnIntelliDataZoom) then
             FOnIntelliDataZoom(hi_int(wparam),loword(lparam),hiword(lparam),wheelscrl);

       if ((wparam and MK_SHIFT)=0) and ((wparam and MK_CONTROL)=0) then
        if assigned(OnIntelliScroll) then
          FOnIntelliScroll(hi_int(wparam),loword(lparam),hiword(lparam),wheelscrl);

      end;

     Result:=CallWindowProc(OldWndProc,(Owner as TForm).Handle, Msg, wParam, lParam);

     {forward wheel messages to focussed control}

     if (msg=wheelmsg) and (wheelmsg<>0) then
      begin
       if assigned((Owner as TForm).ActiveControl) then
        sendmessage((Owner as TForm).ActiveControl.handle,msg,WParam,lparam);

       {invoke handler here, but what about diff.actions ???}

       if (getkeystate(VK_CONTROL) and $8000=$8000) then
         begin
           if assigned(OnIntelliZoom) then
             FOnIntelliZoom(wparam,loword(lparam),hiword(lparam),wheelscrl);
         end
       else
       if (getkeystate(VK_SHIFT) and $8000=$8000) then
         begin
          if assigned(OnIntelliDataZoom) then
           FOnIntelliDataZoom(wparam,loword(lparam),hiword(lparam),wheelscrl);
         end
       else
        if assigned(OnIntelliScroll) then
          FOnIntelliScroll(wparam,loword(lparam),hiword(lparam),wheelscrl);

       if (assigned(OnWheelEvent)) then
        begin
          FOnWheelEvent(wparam,loword(lparam),hiword(lparam),wheelscrl);
          if FAuto then
           begin
            Control := FindVCLWindow(Point(loword(lparam),hiword(lparam)));
            if (Control <> nil) then
            begin
             Num:=wparam div 120;

             if (Control is TMemo) or (Control is TEdit) then
              begin
               SendMessage(Control.Handle,EM_LINESCROLL,0,-num*wheelscrl);
              end;
             if (Control is TCustomListBox) then
              begin
               i:=sendmessage(control.handle,lb_gettopindex,0,0);
               i:=i+(-num*wheelscrl);
               if (i<0) then i:=0;
               sendmessage(control.handle,lb_settopindex,i,0);
              end;
             if (Control is TStringGrid) then
              begin
               i:=(control as TStringGrid).row;
               i:=i+(-num*wheelscrl);
               if (i<(control as TStringGrid).fixedrows) then i:=(control as TStringGrid).fixedrows;
               if (i>(control as TStringGrid).rowcount-1) then i:=(control as TStringGrid).rowcount-1;
               (control as TStringGrid).row:=i;
              end;
            end;

           end;
        end;

      end;
   end;
end;  { tmswheel.HookWndProc }

function TMSWheel.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TMSWheel.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TMSWheel.SetVersion(const Value: string);
begin

end;

end.
