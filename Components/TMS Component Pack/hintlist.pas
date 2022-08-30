{***************************************************************************}
{ THINTLISTBOX component                                                    }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by                                                                }
{   TMS Software                                                            }
{   copyright © 1998-2012                                                   }
{   Email : info@tmssoftware.com                                            }
{   Web : http://www.tmssoftware.com                                        }
{ The source code is given as is. The author is not responsible             }
{ for any possible damage done due to the use of this code.                 }
{ The component can be freely used in any application. The complete         }
{ source code remains property of the author and may not be distributed,    }
{ published, given or sold in any form as such. No parts of the source      }
{ code can be included in any other component or application without        }
{ written authorization of the author.                                      }
{***************************************************************************}

{$I TMSDEFS.INC}
 (*
{$IFDEF VER80}
 {$DEFINE DELPHI1}
 {$DEFINE ISDELPHI}
{$ENDIF}

{$IFDEF VER90}
 {$DEFINE DELPHI2}
 {$DEFINE DELPHI2_LVL}
 {$DEFINE ISDELPHI}
{$ENDIF}

{$IFDEF VER93}
 {$DEFINE BCB1}
 {$DEFINE DELPHI2_LVL}
 {$DEFINE ISBCB}
{$ENDIF}

{$IFDEF VER100}
 {$DEFINE DELPHI3}
 {$DEFINE DELPHI3_LVL}
 {$DEFINE DELPHI3_4_ONLY}
 {$DEFINE ISDELPHI}
{$ENDIF}

{$IFDEF VER110}
 {$DEFINE BCB3}
 {$DEFINE DELPHI3_LVL}
 {$DEFINE ISBCB}
{$ENDIF}

{$IFDEF VER120}
 {$DEFINE DELPHI4}
 {$DEFINE DELPHI3_LVL}
 {$DEFINE DELPHI4_LVL}
 {$DEFINE DELPHI3_4_ONLY}
 {$DEFINE ISDELPHI}
{$ENDIF}

{$IFDEF VER125}
 {$DEFINE DELPHI4}
 {$DEFINE DELPHI3_LVL}
 {$DEFINE DELPHI4_LVL}
{$ENDIF}

{$IFDEF VER130}
 {$DEFINE DELPHI5}
 {$DEFINE DELPHI3_LVL}
 {$DEFINE DELPHI4_LVL}
 {$DEFINE DELPHI3_4_ONLY}
 {$DEFINE ISDELPHI}
{$ENDIF}

{$IFDEF VER110}
 {$ObjExportAll On}
{$ENDIF}

{$IFDEF VER125}
 {$ObjExportAll On}
{$ENDIF}
*)

unit hintlist;

interface

uses
  stdctrls,graphics,windows,messages,classes,forms,controls, sysUtils, Types;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 4; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.

  // version history
  // v1.0.0.0 : First release

type
  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
 THintList = class(TListbox)
  private
   FHintColor: TColor;
   FLastHintPos: TPoint;
   procedure CMHintShow(Var Msg: TMessage); Message CM_HINTSHOW;
   procedure ShowHint(var HintStr: string; var CanShow: Boolean; var HintInfo: THintInfo);
   function GetVersion: string;
   procedure SetVersion(const Value: string);
  protected
   function GetVersionNr: Integer; virtual;
   procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(aOwner:tComponent); override;
    destructor Destroy; override;
  published
   property HintColor: TColor read FHintColor write FHintColor;
   property Version: string read GetVersion write SetVersion;
 end;

implementation

procedure THintList.CMHintShow(var Msg: TMessage);
var
 CanShow: Boolean;
 hi: PHintInfo;
Begin
 CanShow := True;
 hi := PHintInfo(Msg.LParam);
 ShowHint(hi.HintStr, CanShow, hi^);
 Msg.Result := Ord(Not CanShow);
end;


constructor THintList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHintColor := clwhite;
  FLastHintPos := Point(-1,-1);
end;

destructor THintList.Destroy;
begin
  inherited Destroy;
end;

procedure THintList.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  idx:integer;
begin
  if (FLastHintPos.y>=0) then
  begin
   idx:=y div itemheight;
   if (idx<>FLastHintPos.y) then
    begin
       Application.CancelHint;
       FLastHintPos:=Point(-1,-1);
     end;
  end;
 inherited MouseMove(Shift, X, Y);
end;

procedure THintList.ShowHint(var HintStr: string; var CanShow: Boolean; var HintInfo: THintInfo);
var
 hintpos:tpoint;
 idx:integer;

begin

 if (Hintinfo.Hintcontrol = Self) then
   begin
     Hintinfo.HintColor := FHintColor;

    {search over which thing mouse is}
    with (hintinfo.HintControl as tlistbox) do
     begin
       idx:=hintinfo.cursorpos.y div itemheight;

       if (idx+topindex<items.count) then
        hintstr:=items[topindex+idx]
       else
        hintstr:='';

       hintpos.x:=0;
       hintpos.y:=idx*itemheight-2;

       flasthintpos.y:=idx;
       flasthintpos.x:=0;
     end;
    hintinfo.hintpos:=self.clienttoscreen(hintpos);
   end;
end;

function THintList.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function THintList.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure THintList.SetVersion(const Value: string);
begin

end;

end.
