{***************************************************************************}
{ TDateLabel component                                                      }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by                                                                }
{   TMS Software                                                            }
{   Copyright © 1999-2012                                                   }
{   Email : info@tmssoftware.com                                            }
{   Web : http://www.tmssoftware.com                                        }
{                                                                           }
{ The source code is given as is. The author is not responsible             }
{ for any possible damage done due to the use of this code.                 }
{ The component can be freely used in any application. The complete         }
{ source code remains property of the author and may not be distributed,    }
{ published, given or sold in any form as such. No parts of the source      }
{ code can be included in any other component or application without        }
{ written authorization of the author.                                      }
{***************************************************************************}

unit datelbl;

{$I TMSDEFS.INC}
interface


uses
  StdCtrls, Messages, Classes, SysUtils, Windows, ExtCtrls;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 1; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.

  // version history
  // v1.0.0.0 : First release

type
  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TDateLabel = class(TLabel)
  private
    FDateTimeFormat:string;
    FTimer: TTimer;
    FRefreshInterval: Integer;
    FRefreshEnabled: Boolean;
  procedure SetFormat(const Value: string);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    procedure SetInterval(Const Value: Integer);
    procedure SetRefreshEnabled(Const Value: Boolean);
  protected
    function GetVersionNr: Integer; virtual;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; Override;
    procedure RefreshLabel(Sender: TObject);
  published
    property DateTimeFormat:string read FDateTimeFormat write SetFormat;
    property Version: string read GetVersion write SetVersion;
    property RefreshInterval: Integer Read FRefreshInterval Write SetInterval;
    property RefreshEnabled: Boolean Read FRefreshEnabled Write SetRefreshEnabled;
  end;


procedure Register;

implementation

constructor TDateLabel.Create(AOwner: TComponent);
begin
  inherited Create(aOwner);
  Caption := DateToStr(Now);
  FDateTimeFormat := 'd/m/yyyy';
  FRefreshInterval := 0;
  FRefreshEnabled := False;
  FTimer := TTimer.Create(Self);
  FTimer.OnTimer := Self.RefreshLabel;
end;

destructor TDateLabel.Destroy;
begin
  FTimer.Free;
  Inherited;
end;

procedure TDateLabel.SetInterval(Const Value: Integer);
begin
  FRefreshInterval := Value;
  FTimer.Interval := FRefreshInterval;
end;


procedure TDateLabel.SetRefreshEnabled(Const Value: Boolean);
begin
  FRefreshEnabled := Value;
  FTimer.Enabled := False;
  If FRefreshInterval > 0 Then
    FTimer.Enabled := FRefreshEnabled;
end;


procedure TDateLabel.RefreshLabel(Sender: TObject);
begin
  self.Caption := FormatDateTime(fdatetimeformat,now);
end;

procedure TDateLabel.Loaded;
begin
  inherited;
  if (csDesigning in ComponentState) then
    self.caption := FormatDateTime(FDateTimeFormat,now);
end;

procedure tdatelabel.SetFormat(const Value: string);
begin
  FDatetimeformat := value;
  if (csDesigning in ComponentState) then
    self.Caption := FormatDateTime(fdatetimeformat,now);
end;

function TDateLabel.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TDateLabel.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TDateLabel.SetVersion(const Value: string);
begin
  // read only property 
end;

procedure Register;
begin
  RegisterComponents('TMS', [TDateLabel]);
end;


end.

