{***************************************************************************}
{ TEllipsLabel component                                                    }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by                                                                }
{  TMS Software                                                             }
{  copyright © 2001-2012                                                    }
{  Email : info@tmssoftware.com                                             }
{  Web : http://www.tmssoftware.com                                         }
{ The source code is given as is. The author is not responsible             }
{ for any possible damage done due to the use of this code.                 }
{ The component can be freely used in any application. The complete         }
{ source code remains property of the author and may not be distributed,    }
{ published, given or sold in any form as such. No parts of the source      }
{ code can be included in any other component or application without        }
{ written authorization of the author.                                      }
{***************************************************************************}

unit EllipsLabel;

{$I TMSDEFS.INC}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.

  // version history
  // v1.0.0.0 : First release

type

  TEllipsType = (etNone, etEndEllips, etPathEllips);

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TEllipsLabel = class(TLabel)
  private
    FEllipsType: TEllipsType;
    procedure SetEllipsType(const Value: TEllipsType);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    { Private declarations }
  protected
    { Protected declarations }
    function GetVersionNr: Integer; virtual;
    procedure Paint; override;
  public
    { Public declarations }
  published
    { Published declarations }
    property EllipsType: TEllipsType read FEllipsType write SetEllipsType;
    property Version: string read GetVersion write SetVersion;
  end;

implementation

const
  ALIGNSTYLE : array[TAlignment] of DWORD = (DT_LEFT, DT_RIGHT, DT_CENTER);
  WORDWRAPSTYLE : array[Boolean] of DWORD = (DT_SINGLELINE, DT_WORDBREAK);
  LAYOUTSTYLE : array[TTextLayout] of DWORD = (0,DT_VCENTER,DT_BOTTOM);
  ELLIPSSTYLE : array[TEllipsType] of DWORD = (0,DT_END_ELLIPSIS,DT_PATH_ELLIPSIS);
  ACCELSTYLE : array[Boolean] of DWORD = (DT_NOPREFIX,0);

{ TEllipsLabel }

procedure TEllipsLabel.Paint;
var
  R: TRect;
  DrawStyle: DWORD;

begin
  R := GetClientRect;

  if not Transparent then
  begin
    Canvas.Brush.Color := Color;
    Canvas.Pen.Color := Color;
    Canvas.Rectangle(R.Left,R.Top,R.Right,R.Bottom);
  end;

  Canvas.Brush.Style := bsClear;

  DrawStyle := ALIGNSTYLE[Alignment] or WORDWRAPSTYLE[WordWrap] or
    LAYOUTSTYLE[Layout] or ELLIPSSTYLE[FEllipsType] or ACCELSTYLE[ShowAccelChar];

  DrawStyle := DrawTextBiDiModeFlags(DrawStyle);

  Canvas.Font := Font;

  if not Enabled then
  begin
    OffsetRect(R, 1, 1);
    Canvas.Font.Color := clBtnHighlight;
    DrawTextEx(Canvas.Handle,PChar(Caption),Length(Caption),R, DrawStyle, nil);
    OffsetRect(R, -1, -1);
    Canvas.Font.Color := clBtnShadow;
    DrawTextEx(Canvas.Handle,PChar(Caption),Length(Caption),R, DrawStyle, nil);
  end
  else
    DrawTextEx(Canvas.Handle,PChar(Caption),Length(Caption),R, DrawStyle, nil);
end;

procedure TEllipsLabel.SetEllipsType(const Value: TEllipsType);
begin
  if FEllipsType <> Value then
  begin
    FEllipsType := Value;
    Invalidate;
  end;
end;

function TEllipsLabel.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TEllipsLabel.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TEllipsLabel.SetVersion(const Value: string);
begin

end;

end.
