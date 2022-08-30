{********************************************************************}
{ TPROPSCROLLBAR component                                           }
{ for Delphi & C++Builder                                            }
{                                                                    }
{ written by                                                         }
{  TMS Software                                                      }
{  copyright © 1997-2011                                             }
{  Email : info@tmssoftware.com                                      }
{  Website : http://www.tmssoftware.com                              }
{                                                                    }
{ The source code is given as is. The author is not responsible      }
{ for any possible damage done due to the use of this code.          }
{ The component can be freely used in any application. The source    }
{ code remains property of the author and may not be distributed     }
{ freely as such.                                                    }
{********************************************************************}
unit propscrl;


{$I TMSDEFS.INC}


interface

uses
  Stdctrls,Windows, Classes, Messages, Controls, SysUtils, Graphics, Forms;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 4; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.

  // version history
  // v1.0.0.0 : First release

type
  TScrollHintMode = (shTrack, shFixedLeft, shFixedRight, shFixedBottom,shFixedTop);

  TScrollBarHintEvent = procedure (Sender:TObject; Pos:longint;var hintstr:string) of object;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TPropScrollbar = class(TScrollbar)
  private
    FAutoArrowDisable:boolean;
    FPageSize:integer;
    FDoScrollHint:boolean;
    FScrollHint:THintWindow;
    FScrollHintShow:boolean;
    FScrollHintPos:tpoint;
    FScrollHintPrefix:string;
    FScrollHintMode:TScrollHintMode;
    FScrollHintColor:tColor;
    FScrollBarHintEvent:TScrollBarHintEvent;
    procedure SetPageSize(pagesize:integer);
    { not yet fully implemented
    FIsFlat:boolean;
    procedure FlatSetScrollProp(index,newValue:integer;fRedraw:bool);
    procedure FlatSetScrollInfo(code:integer;var scrollinfo:tscrollinfo;fRedraw:bool);
    procedure FlatShowScrollBar(code:integer;show:bool);
    procedure FlatSetScrollPos(code,pos:integer);
    procedure FlatInit;
    procedure FlatDone;
    }
    procedure SetArrowState(pos:integer);
    procedure SetAutoArrowDisable(value:boolean);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
  public
    constructor Create(aOwner:tComponent); override;
    destructor Destroy; override;
  protected
    function GetVersionNr: Integer; virtual;
    procedure Scroll(ScrollCode: TScrollCode; var ScrollPos: Integer); override;
    procedure Loaded; override;
  published
    property AutoArrowDisable:boolean read FAutoArrowDisable write SetAutoArrowDisable;
    property PageSize:integer read FPageSize write SetPageSize;
    property ScrollHint:boolean read FDoScrollHint write FDoScrollHint;
    property ScrollHintPrefix:string read FScrollHintPrefix write FScrollHintPrefix;
    property ScrollHintMode:TScrollHintMode read FScrollHintMode write FScrollHintMode;
    property ScrollHintColor:tColor read FScrollHintColor write FScrollHintColor;
    property OnScrollBarHint:TScrollBarHintEvent read fScrollBarHintEvent write fScrollBarHintEvent;
    property Version: string read GetVersion write SetVersion;
  end;

implementation

const
  comctrl = 'comctl32.dll';

 {------------------------------------------------}
 {constant definitions for flat/encarta scrollbars}
 {------------------------------------------------}
  WSB_PROP_CYVSCROLL  = $0000001;
  WSB_PROP_CXHSCROLL  = $0000002;
  WSB_PROP_CYHSCROLL  = $0000004;
  WSB_PROP_CXVSCROLL  = $0000008;
  WSB_PROP_CXHTHUMB   = $0000010;
  WSB_PROP_CYVTHUMB   = $0000020;
  WSB_PROP_VBKGCOLOR  = $0000040;
  WSB_PROP_HBKGCOLOR  = $0000080;
  WSB_PROP_VSTYLE     = $0000100;
  WSB_PROP_HSTYLE     = $0000200;
  WSB_PROP_WINSTYLE   = $0000400;
  WSB_PROP_PALETTE    = $0000800;
  WSB_PROP_MASK       = $0000FFF;

  FSB_FLAT_MODE       =    2;
  FSB_ENCARTA_MODE    =    1;
  FSB_REGULAR_MODE    =    0;


constructor TPropScrollbar.Create(aOwner:tComponent);
begin
  inherited Create(aOwner);
  fScrollHint:=THintWindow.Create(self);
  fScrollHintShow:=false;
  fScrollHintMode:=shTrack;
  fScrollHintColor:=clYellow;
end;

destructor TPropScrollbar.Destroy;
begin
  FScrollHint.Free;
  inherited Destroy;
end;

procedure TPropScrollBar.Loaded;
begin
  inherited;
  SetArrowState(position);
end;

procedure TPropScrollbar.Scroll(ScrollCode: TScrollCode; var ScrollPos: Integer);
var
  s:string;
  r: TRect;
  pt,ctrl: TPoint;
begin
  if not Enabled then
  begin
    EnableScrollBar(self.Handle, SB_CTL, ESB_DISABLE_BOTH);
    Exit;
  end;
  
  if fDoScrollHint then
  begin
    if (scrollcode=scENDSCROLL) then
    begin
      FScrollHint.ReleaseHandle;
      FScrollHintShow := false;
    end;

    if (scrollcode = scTRACK) then
    begin
      FScrollHint.Color := FScrollHintColor;
      s := ScrollHintPrefix + ' ' + inttostr(scrollpos);

      if Assigned(OnScrollBarHint) then
        OnScrollBarHint(self,scrollpos,s);

      r := FScrollHint.CalcHintRect(100,s,nil);
      FScrollHint.Caption := s;

      ctrl.y := top;
      ctrl.x := left;
      ctrl:=(parent as TWinControl).clienttoscreen(ctrl);

      if not FScrollHintShow then
      begin
        getcursorpos(pt);
        case self.kind of
        sbHorizontal:
          case fScrollHintMode of
          shFixedLeft:pt.x:=pt.x-(r.right-r.left)-10;
          shFixedRight:pt.x:=pt.x+10;
          shFixedTop:pt.y:=ctrl.y-4-(r.bottom-r.top);
          shFixedBottom:pt.y:=ctrl.y+height+4;
          end;
        sbVertical:
          case fScrollHintMode of
          shFixedLeft:pt.x:=ctrl.x-(r.right-r.left)-4;
          shFixedRight:pt.x:=ctrl.x+width+4;
          shFixedTop:pt.y:=pt.y-10;
          shFixedBottom:pt.y:=pt.y+10;
          end;
        end;
        fScrollHintPos := pt;
      end
      else
        pt := fScrollHintPos;

      if FScrollHintMode = shTrack then
        GetCursorPos(pt);

      r.left:=r.left+pt.x+10;
      r.right:=r.right+pt.x+10;
      r.top:=R.top+pt.y;
      r.bottom:=r.bottom+pt.y;

      FScrollHint.ActivateHint(r,s);
      FScrollHintShow:=true;
    end;
  end;

  inherited;
  
  if (scrollcode=scENDSCROLL) then
    SetArrowState(scrollpos);
end;

procedure TPropScrollbar.SetArrowState(pos:integer);
begin
  if not Enabled then
  begin
    EnableScrollBar(self.Handle, SB_CTL, ESB_DISABLE_BOTH);
    Exit;
  end;

  if not FAutoArrowDisable then
    Exit;

  if (pos=min) then
    Enablescrollbar(self.Handle,SB_CTL,ESB_DISABLE_UP);

  if (pos+pagesize>=max) then
    EnableScrollbar(self.Handle,SB_CTL,ESB_DISABLE_DOWN);
end;


procedure TPropScrollBar.SetAutoArrowDisable(value:boolean);
begin
  FAutoArrowDisable := value;
  if fAutoArrowDisable then
    SetArrowState(self.position)
  else
    Enablescrollbar(self.handle,SB_CTL,ESB_ENABLE_BOTH);
end;

procedure TPropScrollbar.SetPageSize(pagesize:integer);
var
  scrollinfo:tscrollinfo;
begin
  fPageSize:=pagesize;

  scrollinfo.cbSize := sizeof(scrollinfo);
  scrollinfo.fMask := SIF_RANGE;
  getscrollinfo(Handle,SB_CTL,scrollinfo);

  {set vertical page size}
  scrollinfo.cbSize := sizeof(scrollinfo);
  scrollinfo.fMask := SIF_PAGE;
  scrollinfo.nPage := PageSize;

  setscrollinfo(Handle,SB_CTL,scrollinfo,true);
end;

function TPropScrollbar.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TPropScrollbar.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TPropScrollbar.SetVersion(const Value: string);
begin

end;

{
procedure TPropScrollBar.FlatInit;
var
 ComCtl32DLL: THandle;
 _InitializeFlatSB: function(wnd:hwnd):Bool stdcall;
begin
 ComCtl32DLL:=GetModuleHandle(comctrl);
 if (ComCtl32DLL>0) then
  begin
   @_InitializeFlatSB:=GetProcAddress(ComCtl32DLL,'InitializeFlatSB');
   if assigned(_InitializeFlatSB) then _InitializeFlatSB((parent as TWinControl).handle);
   fIsFlat:=assigned(_InitializeFlatSB);

   if fIsFlat then outputdebugstring('test');

   if fIsFlat then FlatSetScrollProp(WSB_PROP_HSTYLE,FSB_FLAT_MODE,true);
   if fIsFlat then FlatSetScrollProp(WSB_PROP_VSTYLE,FSB_FLAT_MODE,true);
   FlatShowScrollBar(SB_HORZ,true);
  end;

end;

procedure TPropScrollBar.FlatDone;
var
 ComCtl32DLL: THandle;
 _UninitializeFlatSB: function(wnd:hwnd):Bool stdcall;
begin
 fisFlat:=false;
 ComCtl32DLL:=GetModuleHandle(comctrl);
 if (ComCtl32DLL>0) then
  begin
   @_UninitializeFlatSB:=GetProcAddress(ComCtl32DLL,'UninitializeFlatSB');
   if assigned(_UninitializeFlatSB) then _UninitializeFlatSB(self.handle);
  end;
end;


procedure TPropScrollBar.FlatSetScrollPos(code,pos: integer);
var
 scrollinfo:tscrollinfo;
begin
 scrollinfo.cbSize :=sizeof(scrollinfo);
 scrollinfo.fMask :=SIF_POS;
 scrollinfo.nPos :=pos;
 self.FlatSetScrollInfo(code,scrollinfo,true);
end;

procedure TPropScrollBar.FlatSetScrollInfo(code: integer;var scrollinfo:tscrollinfo;fRedraw: bool);
var
 ComCtl32DLL: THandle;
 _FlatSB_SetScrollInfo:function(wnd:hwnd;code:integer;var scrollinfo:tscrollinfo;fRedraw:bool):integer; stdcall;

begin
 ComCtl32DLL:=GetModuleHandle(comctrl);
 if (ComCtl32DLL>0) then
  begin
   @_FlatSB_SetScrollInfo:=GetProcAddress(ComCtl32DLL,'FlatSB_SetScrollInfo');
   if assigned(_FlatSB_SetScrollInfo) then
     begin
      _FlatSB_SetScrollInfo(self.handle,code,scrollinfo,fRedraw);
     end;
  end;
end;

procedure TPropScrollBar.FlatSetScrollProp(index, newValue: integer;
  fRedraw: bool);
var
 ComCtl32DLL: THandle;
 _FlatSB_SetScrollProp:function(wnd:hwnd;Index,newValue:integer;fredraw:bool):bool stdcall;

begin
 if not fIsFlat then exit;
 ComCtl32DLL:=GetModuleHandle(comctrl);
 if (ComCtl32DLL>0) then
  begin
   @_FlatSB_SetScrollProp:=GetProcAddress(ComCtl32DLL,'FlatSB_SetScrollProp');
   if assigned(_FlatSB_SetScrollProp) then
     _FlatSB_SetScrollProp(self.handle,index,newValue,fRedraw);
  end;
end;

procedure TPropScrollBar.FlatShowScrollBar(code:integer;show:bool);
var
 ComCtl32DLL: THandle;
 _FlatSB_ShowScrollBar:function(wnd:hwnd;code:integer;show:bool):integer; stdcall;

begin
 if not fIsFlat then exit;

 ComCtl32DLL:=GetModuleHandle(comctrl);
 if (ComCtl32DLL>0) then
  begin
   @_FlatSB_ShowScrollBar:=GetProcAddress(ComCtl32DLL,'FlatSB_ShowScrollBar');
   if assigned(_FlatSB_ShowScrollBar) then
     _FlatSB_ShowScrollBar(self.handle,code,show);
  end;
end;

}


end.
