{***********************************************************************}
{ TADVPROGRESS component                                                }
{ for Delphi & C++Builder                                               }
{                                                                       }
{ written by                                                            }
{   TMS Software                                                        }
{   copyright © 1998-2011                                               }
{   Email : info@tmssoftware.com                                        }
{   Website : http://www.tmssoftware.com                                }
{                                                                       }
{ The source code is given as is. The author is not responsible         }
{ for any possible damage done due to the use of this code.             }
{ The component can be freely used in any application. The source       }
{ code remains property of the author and may not be distributed        }
{ freely as such.                                                       }
{***********************************************************************}

unit AdvProgr;

{$I TMSDEFS.INC}


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, ComCtrls, commctrl;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 2; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.

type

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvProgress = class(TProgressBar)
  private
    FBarColor: TColor;
    FBkColor: TColor;
    { Private declarations }
    procedure SetProBarColor(avalue:tcolor);
    procedure SetProBkColor(avalue:tcolor);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    function GetVersionNr: Integer;
  protected
    { Protected declarations }
    procedure CreateParams(var Params:TCreateParams); override;
    procedure Loaded; override;
  public
    { Public declarations }
    constructor Create(aOwner:TComponent); override;
  published
    { Published declarations }
    property BarColor: TColor read FBarColor write SetProBarColor;
    property BkColor: TColor read FBkColor write SetProBkColor;
    property Version: string read GetVersion write SetVersion;
  end;

implementation

procedure TAdvProgress.CreateParams(var Params:TCreateParams);
begin
 inherited CreateParams(params);
end;

procedure TAdvProgress.Loaded;
begin
 inherited;
end;


procedure TAdvProgress.SetProBarColor(avalue:tcolor);
begin
  FBarColor := AValue;
  sendmessage(Handle,PBM_SETBARCOLOR,0,LParam(colortorgb(avalue)));
end;

procedure TAdvProgress.SetProBkColor(avalue:tcolor);
begin
  FBkColor := AValue;
  Sendmessage(Handle,PBM_SETBKCOLOR,0,LParam(colortorgb(avalue)));
end;

constructor TAdvProgress.Create(aOwner: TComponent);
begin
  inherited;
  fBarColor:=clHighLight;
  fBkColor:=clWindow;
end;

function TAdvProgress.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TAdvProgress.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TAdvProgress.SetVersion(const Value: string);
begin

end;

end.
