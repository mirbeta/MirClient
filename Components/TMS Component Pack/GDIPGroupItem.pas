{ ************************************************************************* }
{ TGroupItem Class }
{ for Delphi & C++Builder }
{ }
{ written by TMS Software }
{ copyright ©  2010 }
{ Email : info@tmssoftware.com }
{ Web : http://www.tmssoftware.com }
{ }
{ The source code is given as is. The author is not responsible }
{ for any possible damage done due to the use of this code. }
{ The component can be freely used in any application. The complete }
{ source code remains property of the author and may not be distributed, }
{ published, given or sold in any form as such. No parts of the source }
{ code can be included in any other component or application without }
{ written authorization of the author. }
{ ************************************************************************* }

unit GDIPGroupItem;

interface

{$I TMSDEFS.INC}

uses
  Windows, Messages, Classes, Controls, Forms, Graphics,
  GDIPCustomItem, AdvGDIP, GDIPImageSectionItem, ExtCtrls, SysUtils,
  AdvStyleIF;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.

  // v0.9.0.0 : First Beta Release
  // v1.0.0.0 : First Release

type
  TGroupItem = class;

  TGroupItem = class(TImageSectionItem, ITMSStyle, ITMSTones)
  private
    FGroupColor: TColor;
    FGroupBorderColor: TColor;
    FHeaderHeight: integer;
    procedure SetGroupColor(const Value: TColor);
    procedure SetGroupBorderColor(const Value: TColor);
    procedure SetHeaderHeight(const Value: integer);
    { Private declarations }
  protected
    { Protected declarations }
    procedure SetComponentStyle(AStyle: TTMSStyle);
    procedure SetColorTones(ATones: TColorTones);
    function GetVersionNr: integer; override;
  public
    function CreateNewItem(AOwner: TComponent): TCustomItem; override;
    function GetClassType: TComponentClass; override;
    class function CustomClassName: String; override;
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure DrawInRect(g: TGPGraphics; ItemAppearance: TItemAppearance;
      R: TGPRectF); override;
    function FirstTab(Backwards: Boolean): Boolean; override;
    function IsSection: Boolean; override;
    procedure InitDesignTime; override;
  published
    { Published declarations }
    property GroupColor
      : TColor read FGroupColor write SetGroupColor default $C9C9C9;
    property GroupBorderColor: TColor read FGroupBorderColor write
      SetGroupBorderColor default $C9C9C9;
    property HeaderHeight
      : integer read FHeaderHeight write SetHeaderHeight default 30;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterPolyItem(TGroupItem);
end;

{ TGroupItem }

procedure TGroupItem.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TGroupItem then
  begin
    FGroupColor := (Source as TGroupItem).GroupColor;
    FGroupBorderColor := (Source as TGroupItem).GroupBorderColor;
    FHeaderHeight := (Source as TGroupItem).HeaderHeight;
    Changed;
  end;
end;

constructor TGroupItem.Create(AOwner: TComponent);
begin
  inherited;
  FGroupColor := $C9C9C9;
  FGroupBorderColor := $C9C9C9;
  FHeaderHeight := 30;
  if (csDesigning in ComponentState) and not (csLoading in ComponentState) then
  begin
    Height := 200;
    CaptionSize := 10;
    ControlMargin.Top := HeaderHeight + 4;
    ControlStretched := true;
  end;
end;

function TGroupItem.CreateNewItem(AOwner: TComponent): TCustomItem;
begin
  Result := TGroupItem.Create(AOwner);
end;

class function TGroupItem.CustomClassName: String;
begin
  Result := 'Group Item';
end;

destructor TGroupItem.Destroy;
begin
  inherited;
end;

procedure TGroupItem.DrawInRect(g: TGPGraphics;
  ItemAppearance: TItemAppearance; R: TGPRectF);
var
  b: TGPSolidBrush;
  p: TGPPen;
  ir: TGPRectF;
  ri: TGPRectF;
begin
  if Visible then
  begin
    ri := MakeRect(R.x, R.y, R.Width, HeaderHeight);
    DoItemStartDraw(Self, g, Self, R);
    DoInternalItemStartDraw(Self, g, Self, R);

    if GroupColor <> clNone then
    begin
      b := TGPSolidBrush.Create(MakeColor(255, GroupColor));
      p := TGPPen.Create(MakeColor(255, GroupBorderColor));
      g.FillRectangle(b, ri);
      g.DrawRectangle(p, ri);
      g.DrawRectangle(p, R);
      b.Free;
      p.Free;
    end;

    DrawLine(g, ri, ItemAppearance);
    ir := DrawText(g, ri, ItemAppearance);
    case Status.Position of
      spItemRectangle:
        DrawStatus(g, ri, ItemAppearance);
      spItemText:
        DrawStatus(g, ir, ItemAppearance);
    end;

    DrawImage(g, ri, ItemAppearance);

    DoItemEndDraw(Self, g, Self, R);
    DoInternalItemEndDraw(Self, g, Self, R);
  end;
end;

function TGroupItem.FirstTab(Backwards: Boolean): Boolean;
begin
  ProcessTab(Backwards);
  Result := True;
end;

function TGroupItem.GetClassType: TComponentClass;
begin
  Result := TGroupItem;
end;

function TGroupItem.GetVersionNr: integer;
begin
  Result := MakeLong(MakeWord(BLD_VER, REL_VER), MakeWord(MIN_VER, MAJ_VER));
end;

procedure TGroupItem.InitDesignTime;
begin
  inherited;
  Height := 200;
  CaptionSize := 10;
  ControlMargin.Top := HeaderHeight + 4;
  ControlStretched := true;
end;

function TGroupItem.IsSection: Boolean;
begin
  Result := False;
end;

procedure TGroupItem.SetColorTones(ATones: TColorTones);
begin
  GroupColor := ATones.Selected.BrushColor;
  GroupBorderColor := GroupColor;
  CaptionColor := ATones.Selected.TextColor;
end;

procedure TGroupItem.SetComponentStyle(AStyle: TTMSStyle);
begin
  CaptionColor := clNone;
  GroupColor := $C9C9C9;
  GroupBorderColor := GroupColor;
end;

procedure TGroupItem.SetGroupBorderColor(const Value: TColor);
begin
  if FGroupBorderColor <> Value then
  begin
    FGroupBorderColor := Value;
    Changed;
  end;
end;

procedure TGroupItem.SetGroupColor(const Value: TColor);
begin
  if FGroupColor <> Value then
  begin
    FGroupColor := Value;
    Changed;
  end;
end;

procedure TGroupItem.SetHeaderHeight(const Value: integer);
begin
  if FHeaderHeight <> Value then
  begin
    FHeaderHeight := Value;
    if Assigned(Control) then
      ControlMargin.Top := HeaderHeight + 4;
    Changed;
  end;
end;

end.
