{*************************************************************************}
{ TImageTextButtonSectionItem Class                                       }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright ©  2010                                             }
{           Email : info@tmssoftware.com                                  }
{           Web : http://www.tmssoftware.com                              }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The complete       }
{ source code remains property of the author and may not be distributed,  }
{ published, given or sold in any form as such. No parts of the source    }
{ code can be included in any other component or application without      }
{ written authorization of the author.                                    }
{*************************************************************************}

unit GDIPImageTextButtonSectionItem;

interface

{$I TMSDEFS.INC}

uses
  Windows, Controls, Graphics, Classes,
  GDIPCustomItem, GDIPImageTextButtonItem,
  AdvGDIP, AdvStyleIF;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.

  //v0.9.0.0 : First Beta Release
  //v1.0.0.0 : First Release

type
  TImageTextButtonSectionItem = class(TImageTextButtonItem, ITMSStyle)
  private
    FLineColor: TColor;
    FLineSize: Integer;
    FLineMargin: TMargins;
    FLineStyle: TPenStyle;
    FLineOpacity: Byte;
    FLineLocation: TItemLineLocations;
    FLineVisible: Boolean;
    procedure SetLineColor(const Value: TColor);
    procedure SetLineLocation(const Value: TItemLineLocations);
    procedure SetLineMargin(const Value: TMargins);
    procedure SetLineOpacity(const Value: Byte);
    procedure SetLineSize(const Value: Integer);
    procedure SetLineStyle(const Value: TPenStyle);
    procedure SetLineVisible(const Value: Boolean);
  protected
    procedure SetComponentStyle(AStyle: TTMSStyle);
    procedure DrawLine(g: TGPGraphics; ARect: TGPRectF; ItemAppearance: TItemAppearance); virtual;
    procedure LineMarginChanged(Sender: TObject);
    function GetVersionNr: integer; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CreateNewItem(AOwner: TComponent): TCustomItem; override;
    procedure Assign(Source: TPersistent); override;
    function GetClassType: TComponentClass; override;
    class function CustomClassName: String; override;
    procedure DrawInRect(g: TGPGraphics; ItemAppearance: TItemAppearance; R: TGPRectF); override;
    function GetItemInteraction(pX, pY: integer): TItemInteractionType; override;
    function FirstTab(Backwards: Boolean): Boolean; override;
    function IsSection: Boolean; override;
    procedure InitDesignTime; override;
  published
    property LineStyle: TPenStyle read FLineStyle write SetLineStyle default psDot;
    property LineColor: TColor read FLineColor write SetLineColor default clSilver;
    property LineLocation: TItemLineLocations read FLineLocation write SetLineLocation default [llBottom];
    property LineMargin: TMargins read FLineMargin write SetLineMargin;
    property LineSize: Integer read FLineSize write SetLineSize default 2;
    property LineOpacity: Byte read FLineOpacity write SetLineOpacity default 255;
    property LineVisible: Boolean read FLineVisible write SetLineVisible default true;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterPolyItem(TImageTextButtonSectionItem);
end;


{ TImageTextButtonSectionItem }

procedure TImageTextButtonSectionItem.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TImageTextButtonSectionItem then
  begin
    FLineStyle := (Source as TImageTextButtonSectionItem).LineStyle;
    FLineColor := (Source as TImageTextButtonSectionItem).LineColor;
    FLineLocation := (Source as TImageTextButtonSectionItem).LineLocation;
    FLineMargin.Assign((Source as TImageTextButtonSectionItem).LineMargin);
    FLineSize := (Source as TImageTextButtonSectionItem).LineSize;
    FLineOpacity := (Source as TImageTextButtonSectionItem).LineOpacity;
    FLineVisible := (Source as TImageTextButtonSectionItem).LineVisible;
    Changed;
  end;
end;

constructor TImageTextButtonSectionItem.Create;
begin
  inherited Create(AOwner);
  if (csDesigning in ComponentState) and not (csLoading in ComponentState) then
  begin
    CaptionSize := 12;
    Selectable := false;
  end;
  FLineStyle := psDot;
  FLineColor := clSilver;
  FLineLocation := [llBottom];
  FLineMargin := TMargins.Create(nil);
  FLineMargin.OnChange := LineMarginChanged;
  FLineMargin.Left := 1;
  FLineMargin.Top := 1;
  FLineMargin.Right := 1;
  FLineMargin.Bottom := 1;
  FLineSize := 2;
  FLineOpacity := 255;
  FLineVisible := true;
end;

function TImageTextButtonSectionItem.CreateNewItem(AOwner: TComponent): TCustomItem;
begin
  Result := TImageTextButtonSectionItem.Create(AOwner);
end;

class function TImageTextButtonSectionItem.CustomClassName: String;
begin
  Result := 'Section Text, Image and Button Item';
end;

destructor TImageTextButtonSectionItem.Destroy;
begin
  FLineMargin.Free;
  inherited;
end;

procedure TImageTextButtonSectionItem.DrawInRect(g: TGPGraphics;
  ItemAppearance: TItemAppearance; R: TGPRectF);
var
  ir, tr: TGPRectF;
begin
  if Visible then
  begin
    DoItemStartDraw(Self, g, Self, r);
    DoInternalItemStartDraw(Self, g, Self, r);

    case ButtonLocation of
      blRight: ir := MakeRect(r.X, r.Y, r.Width - ButtonWidth - 1, r.Height);
      blLeft: ir := MakeRect(r.X + ButtonWidth, r.Y, r.Width - ButtonWidth + 1, r.Height);
    end;

    DrawLine(g, r, ItemAppearance);

    tr := DrawText(g, ir, ItemAppearance);

    case Status.Position of
      spItemRectangle:  DrawStatus(g, MakeRect(ir.x, ir.y, ir.width, ir.height), ItemAppearance);
      spItemText: DrawStatus(g, tr, ItemAppearance);
    end;

    DrawImage(g, ir, ItemAppearance);
    DrawButton(g, r, ItemAppearance);

    if ButtonGlow and not ItemAppearance.IsMetroStyle then
      DrawButtonGlow(g, r, ItemAppearance);

    DoItemEndDraw(Self, g, Self, r);
    DoInternalItemEndDraw(Self, g, Self, r);
  end;
end;

procedure TImageTextButtonSectionItem.DrawLine(g: TGPGraphics;
  ARect: TGPRectF; ItemAppearance: TItemAppearance);
var
  p: TGPPen;
  r: TGPRectF;
  oldmode: integer;
begin
  if LineVisible then
  begin
    oldmode := g.GetSmoothingMode;
    g.SetSmoothingMode(SmoothingModeDefault);
    r := ARect;
    p := TGPPen.Create(MakeColor(LineOpacity, LineColor), LineSize);
    p.SetDashStyle(DashStyle(LineStyle));

    if llLeft in LineLocation then
      g.DrawLine(p, r.X + LineMargin.Left, r.Y + LineMargin.Top, r.X + LineMargin.Left, r.Y + r.Height - LineMargin.Bottom);
    if llTop in LineLocation then
      g.DrawLine(p, r.X + LineMargin.Left, r.Y + LineMargin.Top, r.x + r.Width - LineMargin.Right, r.Y + LineMargin.Top);
    if llRight in LineLocation then
      g.DrawLine(p, r.X + r.Width - LineMargin.Right, r.Y + LineMargin.Top, r.X + r.Width - LineMargin.Right, r.Y + r.Height - LineMargin.Bottom);
    if llBottom in LineLocation then
      g.DrawLine(p, r.X + LineMargin.Left, r.Y + r.Height - LineMargin.Bottom, r.x + r.Width - LineMargin.Right, r.Y + r.Height - LineMargin.Bottom);

    p.Free;
    g.SetSmoothingMode(oldmode);
  end;
end;

function TImageTextButtonSectionItem.FirstTab(Backwards: Boolean): Boolean;
begin
  ProcessTab(Backwards);
  Result := False;
end;

function TImageTextButtonSectionItem.GetClassType: TComponentClass;
begin
  Result := TImageTextButtonSectionItem;
end;

function TImageTextButtonSectionItem.GetItemInteraction(pX,
  pY: integer): TItemInteractionType;
begin
  Result := itNone;
  if IsButtonAtXY(pX, pY) and ButtonEnabled then
    result := itButton
  else if IsStatusAtXY(pX, pY) then
    result := itStatus;
end;

function TImageTextButtonSectionItem.GetVersionNr: integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TImageTextButtonSectionItem.InitDesignTime;
begin
  inherited;
  CaptionSize := 12;
  Selectable := false;
end;

function TImageTextButtonSectionItem.IsSection: Boolean;
begin
  Result := True;
end;

procedure TImageTextButtonSectionItem.LineMarginChanged(Sender: TObject);
begin
  Changed;
end;

procedure TImageTextButtonSectionItem.SetComponentStyle(AStyle: TTMSStyle);
begin
  ButtonGlow := not (AStyle = tsWindows8);
end;

procedure TImageTextButtonSectionItem.SetLineColor(const Value: TColor);
begin
  if FLineColor <> value then
  begin
    FLineColor := Value;
    Changed;
  end;
end;

procedure TImageTextButtonSectionItem.SetLineLocation(
  const Value: TItemLineLocations);
begin
  if FLineLocation <> Value then
  begin
    FLineLocation := Value;
    Changed;
  end;
end;

procedure TImageTextButtonSectionItem.SetLineMargin(const Value: TMargins);
begin
  if FLineMargin <> value then
  begin
    FLineMargin := Value;
    Changed;
  end;
end;

procedure TImageTextButtonSectionItem.SetLineOpacity(const Value: Byte);
begin
  if FLineOpacity <> value then
  begin
    FLineOpacity := Value;
    Changed;
  end;
end;

procedure TImageTextButtonSectionItem.SetLineSize(const Value: Integer);
begin
  if FLineSize <> value then
  begin
    FLineSize := Value;
    Changed;
  end;
end;

procedure TImageTextButtonSectionItem.SetLineStyle(const Value: TPenStyle);
begin
  if FLineStyle <> value then
  begin
    FLineStyle := Value;
    Changed;
  end;
end;

procedure TImageTextButtonSectionItem.SetLineVisible(const Value: Boolean);
begin
  if FLineVisible <> Value then
  begin
    FLineVisible := Value;
    Changed;
  end;
end;

end.
