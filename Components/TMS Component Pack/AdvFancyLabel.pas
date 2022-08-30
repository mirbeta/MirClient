{**************************************************************************}
{ TAdvFancyLabel component                                                 }
{ for Delphi & C++Builder                                                  }
{                                                                          }
{ written                                                                  }
{   TMS Software                                                           }
{   copyright © 2013                                                       }
{   Email : info@tmssoftware.com                                           }
{   Web : http://www.tmssoftware.com                                       }
{                                                                          }
{ The source code is given as is. The author is not responsible            }
{ for any possible damage done due to the use of this code.                }
{ The component can be freely used in any application. The complete        }
{ source code remains property of the author and may not be distributed,   }
{ published, given or sold in any form as such. No parts of the source     }
{ code can be included in any other component or application without       }
{ written authorization of the author.                                     }
{**************************************************************************}

unit AdvFancyLabel;

{$I TMSDEFS.INC}

interface

uses
  Windows, Controls, Classes, Math, Graphics, AdvGDIP, SysUtils, AdvCurve, Contnrs;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.

  // version history
  // v1.0.0.0 : first release

type
  TAdvFancyLabel = class;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvFancyLabel = class(TAdvCustomCurve)
  private
    FText: String;
    FFont: TFont;
    procedure SetText(const Value: String);
    procedure SetFont(const Value: TFont);
  protected
    procedure FontChanged(Sender: TObject);
  public
    function GetPaths: TObjectList; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure InitSample;
    function GetVersionNr: Integer; override;
  published
    property Text: String read FText write SetText;
    property Font: TFont read FFont write SetFont;
  end;

implementation

{ TAdvFancyLabel }

procedure TAdvFancyLabel.Assign(Source: TPersistent);
begin
  inherited;
  if (Source is TAdvFancyLabel) then
  begin
    FFont.Assign((Source as TAdvFancyLabel).Font);
    FText := (Source as TAdvFancyLabel).Text;
  end;
end;

constructor TAdvFancyLabel.Create(AOwner: TComponent);
begin
  inherited;
  FFont := TFont.Create;
  FFont.Name := 'Tahoma';
  FFont.Size := 28;
  FFont.OnChange := FontChanged;

  if (csDesigning in ComponentState) and not
    ((csReading in Owner.ComponentState) or (csLoading in Owner.ComponentState)) then
      InitSample;
end;

destructor TAdvFancyLabel.Destroy;
begin
  FFont.Free;
  inherited;
end;

procedure TAdvFancyLabel.FontChanged(Sender: TObject);
begin
  Changed;
end;

function TAdvFancyLabel.GetPaths: TObjectList;
var
  sl: TStringList;
  I: Integer;
  pth: TGPGraphicsPath;
  f: TGPFontFamily;
  sf: TGPStringFormat;
begin
  Result := inherited GetPaths;

  f := TGPFontFamily.Create(Font.Name);

  if (f.Status in [FontFamilyNotFound, FontStyleNotFound]) then
  begin
    f.Free;
    f := TGPFontFamily.Create('Arial');
  end;

  sf := TGPStringFormat.Create;

  sl := SplitText(Font, Text);
  for I := 0 to sl.Count - 1 do
  begin
    pth := TGPGraphicsPath.Create;
    pth.AddString(sl[I], Length(sl[I]), f, GetFontStyle(Font.Style), Font.Size * 1.30, MakePointF(0, 0), sf);
    Result.Add(pth);
  end;

  sl.Free;

  sf.Free;
  f.Free;
end;

function TAdvFancyLabel.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TAdvFancyLabel.InitSample;
begin
  BeginUpdate;
  FText := ClassName;
  Points.Clear;
  AddPoint(10, 10, Width - 10, 10);
  AddPoint(Width / 3, Height / 3 * 2, Width  - 10, (Height / 3 * 2));
  EndUpdate;
end;

procedure TAdvFancyLabel.SetFont(const Value: TFont);
begin
  if FFont <> Value then
  begin
    FFont.Assign(Value);
    Changed;
  end;
end;

procedure TAdvFancyLabel.SetText(const Value: String);
begin
  if FText <> Value then
  begin
    FText := Value;
    Changed;
  end;
end;

end.
