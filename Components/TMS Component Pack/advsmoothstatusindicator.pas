{**************************************************************************}
{ TAdvSmoothStatusIndicator component                                      }
{ for Delphi & C++Builder                                                  }
{                                                                          }
{ written                                                                  }
{   TMS Software                                                           }
{   copyright © 2009                                                       }
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
unit AdvSmoothStatusIndicator;

interface

{$I TMSDEFS.INC}

uses
  Windows, SysUtils, Classes, Controls, Messages,
  GDIPFill, Graphics, Math, Dialogs,
  AdvGDIP
  ;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 1; // Release nr.
  BLD_VER = 0; // Build nr.

  // version history
  // v1.0.0.0 : first release
  // v1.0.1.0 : New : Added spacing property

type
  TAdvSmoothStatusIndicator = class;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvSmoothStatusIndicator = class(TGraphicControl)
  private
    FDesignTime: Boolean;
    FResize: Boolean;
    FCaption: String;
    FAppearance: TGDIPStatus;
    FAutoSz: Boolean;
    function GetVersion: String;
    procedure SetVersion(const Value: String);
    procedure SetCaption(const Value: String);
    procedure SetAppearance(const Value: TGDIPStatus);
    procedure SetAutoSz(const Value: Boolean);
  protected
    procedure Changed;
    procedure AppearanceChanged(Sender: TObject);
    function GetVersionNr: integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Resize; override;
    procedure Assign(Source: TPersistent); override;
    procedure Paint; override;
  published
    property Caption: String read FCaption write SetCaption;
    property Version: String read GetVersion write SetVersion;
    property Appearance: TGDIPStatus read FAppearance write SetAppearance;

    property AutoSize: Boolean read FAutoSz write SetAutoSz default false;
    property Align;
    property Anchors;
    property Constraints;
    property PopupMenu;
    property ParentShowHint;
    property ShowHint;


    property OnMouseUp;
    property OnMouseMove;
    property OnMouseDown;
    {$IFDEF DELPHI2006_LVL}
    property OnMouseActivate;
    property OnMouseEnter;
    property OnMouseLeave;
    {$ENDIF}
    property OnResize;
    property OnDblClick;
    property OnClick;
    property OnDragDrop;
    property OnDragOver;
    property OnStartDrag;
    property OnEndDrag;
    property Visible;
    property Hint;
  end;


implementation

{ TAdvSmoothStatusIndicator }

procedure TAdvSmoothStatusIndicator.AppearanceChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothStatusIndicator.Assign(Source: TPersistent);
begin
  if Source is TAdvSmoothStatusIndicator then
  begin
    FCaption := (Source as TAdvSmoothStatusIndicator).Caption;
    FAppearance.Assign((Source as TAdvSmoothStatusIndicator).Appearance);
    FAutoSz := (Source as TAdvSmoothStatusIndicator).AutoSize;
    Changed;
  end;
end;

procedure TAdvSmoothStatusIndicator.Changed;
var
  g: TGPGraphics;
begin
  if (Parent <> nil) and Assigned(Canvas) and FAutoSz then
  begin
    g := TGPGraphics.Create(Canvas.Handle);
    Appearance.CalculateSize(g, Caption);
    FResize := false;
    Width := Appearance.GetWidth;
    Height := Appearance.GetHeight;
    FResize := true;
    g.Free;
  end;
  Invalidate;
end;

constructor TAdvSmoothStatusIndicator.Create(AOwner: TComponent);
begin
  inherited;
  FResize := false;
  Width := 24;
  Height := 24;
  FResize := true;
  FCaption := '0';
  FAppearance := TGDIPStatus.Create;
  FAppearance.OnChange := AppearanceChanged;
  FAutoSz := false;
  FDesignTime := (csDesigning in ComponentState) and not
    ((csReading in Owner.ComponentState) or (csLoading in Owner.ComponentState));
  if FDesigntime then
  begin
    FAppearance.Fill.Color := clRed;
    FAppearance.Fill.GradientType := gtSolid;
    FAppearance.Fill.BorderColor := clGray;
    FAppearance.Font.Color := clWhite;
  end;
end;

destructor TAdvSmoothStatusIndicator.Destroy;
begin
  FAppearance.Free;
  inherited;
end;

function TAdvSmoothStatusIndicator.GetVersion: String;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TAdvSmoothStatusIndicator.GetVersionNr: integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TAdvSmoothStatusIndicator.Paint;
var
  g: TGPGraphics;
begin
  inherited;
  g := TGPGraphics.Create(Canvas.Handle);
  g.SetSmoothingMode(SmoothingModeAntiAlias);
  g.SetTextRenderingHint(TextRenderingHintAntiAlias);    
  Appearance.Draw(g, 0, 0, Width, Height, FAutoSz, Caption);
  g.Free;
end;

procedure TAdvSmoothStatusIndicator.Resize;
begin
  inherited;
  if FResize then
    Changed;
end;

procedure TAdvSmoothStatusIndicator.SetAppearance(
  const Value: TGDIPStatus);
begin
  if FAppearance <> value then
  begin
    FAppearance.Assign(Value);
    AppearanceChanged(Self);
  end;
end;

procedure TAdvSmoothStatusIndicator.SetAutoSz(const Value: Boolean);
begin
  if FAutoSz <> value then
  begin
    FAutoSz := Value;
    Changed;
  end;
end;

procedure TAdvSmoothStatusIndicator.SetCaption(const Value: String);
begin
  if FCaption <> value then
  begin
    FCaption := Value;
    Changed;
  end;
end;

procedure TAdvSmoothStatusIndicator.SetVersion(const Value: String);
begin

end;

end.
