{************************************************************************}
{ TMASKEDEX component                                                    }
{ for Delphi & C++Builder                                                }
{                                                                        }
{ Copyright © 1998-2014                                                  }
{ TMS Software                                                           }
{ Email : info@tmssoftware.com                                           }
{ Web : http://www.tmssoftware.com                                       }
{                                                                        }
{ The source code is given as is. The author is not responsible          }
{ for any possible damage done due to the use of this code.              }
{ The component can be freely used in any application. The complete      }
{ source code remains property of the author and may not be distributed, }
{ published, given or sold in any form as such. No parts of the source   }
{ code can be included in any other component or application without     }
{ written authorization of the author.                                   }
{************************************************************************}
unit MaskEdEx;

{$I TMSDEFS.INC}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Mask;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 2; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.

  // version history
  // v1.0.0.0 : First release

type
  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TMaskEditEx = class(TMaskEdit)
  private
   FAutoTab:boolean;
   FEnterTab:boolean;
   FAlignment:TAlignment;
    { Private declarations }
   {$IFNDEF DELPHIXE_LVL}
   procedure SetAlignment(value:TAlignment);
   {$ENDIF}
   function GetVersion: string;
   procedure SetVersion(const Value: string);
  protected
   function GetVersionNr: Integer; virtual;
   procedure KeyUp(var Key: Word; Shift: TShiftState); override;
   procedure DoEnter; override;
   procedure CreateParams(var Params:TCreateParams); override;
   procedure KeyPress(var Key: Char); override;
    { Protected declarations }
  public
   constructor Create(AOwner: TComponent); override;
    { Public declarations }
  published
   property AutoTab:boolean read FAutoTab write FAutoTab default true;
   property EnterTab:boolean read FEnterTab write FEnterTab default true;
   {$IFNDEF DELPHIXE_LVL}
   property Alignement:TAlignment read FAlignment write SetAlignment default taLeftJustify;
   {$ENDIF}
   property Version: string read GetVersion write SetVersion;
    { Published declarations }
  end;

implementation

constructor TMaskEditEx.Create(AOwner: TComponent);
begin
 inherited Create(aOwner);
 FAutoTab:=true;
 FEnterTab:=true;
end;

{$IFNDEF DELPHIXE_LVL}
procedure TMaskEditEx.SetAlignment(value:tAlignment);
begin
 if FAlignment <> Value then
  begin
   FAlignment := Value;
   RecreateWnd;
  end;
end;
{$ENDIF}

procedure TMaskEditEx.CreateParams(var Params:TCreateParams);
begin
 inherited CreateParams(params);

 if (FAlignment = taRightJustify) then
  begin
   params.style:=params.style AND NOT (ES_LEFT);
   params.style:=params.style or (ES_RIGHT);
   params.style:=params.style or (ES_MULTILINE);
  end;
end;



procedure TMaskEditEx.KeyUp(var Key: Word; Shift: TShiftState);
begin
 inherited keyUp(key,shift);
 if (pos(' ',self.text)=0) and (self.selstart=length(self.text)) and (self.editmask<>'') then
  begin
   if AutoTab then postmessage(self.handle,wm_keydown,VK_TAB,0);
  end;
end;

procedure TMaskEditEx.DoEnter;
begin
  inherited DoEnter;
  self.selstart:=0;
  self.sellength:=1;
end;


procedure TMaskEditEx.KeyPress(var Key: Char);
begin
 if EnterTab and (key=#13) then postmessage(self.handle,wm_keydown,VK_TAB,0);
 inherited Keypress(key);
end;


function TMaskEditEx.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TMaskEditEx.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TMaskEditEx.SetVersion(const Value: string);
begin

end;


end.


