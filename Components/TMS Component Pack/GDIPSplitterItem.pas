{*************************************************************************}
{ TSplitterItem Class                                                      }
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


unit GDIPSplitterItem;

interface

{$I TMSDEFS.INC}

uses
  Windows, Classes, Controls, Graphics, AdvGDIP,
  GDIPCustomItem;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.

  //v0.9.0.0 : First Beta Release
  //v1.0.0.0 : First Release

type
  TLineGradient = (lgLeftToRight, lgRightToLeft, lgCenter);

  TSplitterItem = class(TCustomItem)
  private
    FLineColor: TColor;
    FLineSize: Integer;
    FLineMargin: TMargins;
    FLineOpacity: Byte;
    FLineVisible: Boolean;
    FLineGradient: TLineGradient;
    FLineColorTo: TColor;
    FLineOpacityTo: Byte;
    procedure SetLineColor(const Value: TColor);
    procedure SetLineMargin(const Value: TMargins);
    procedure SetLineOpacity(const Value: Byte);
    procedure SetLineSize(const Value: Integer);
    procedure SetLineVisible(const Value: Boolean);
    procedure SetLineColorTo(const Value: TColor);
    procedure SetLineGradient(const Value: TLineGradient);
    procedure SetLineOpacityTo(const Value: Byte);
  protected
    procedure DrawLine(g: TGPGraphics; ARect: TGPRectF; ItemAppearance: TItemAppearance); virtual;
    procedure LineMarginChanged(Sender: TObject);
    function GetVersionNr: integer; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CreateNewItem(AOwner: TComponent): TCustomItem; override;
    procedure Assign(Source: TPersistent); override;
    function GetClassType: TComponentClass; override;
    procedure DrawInRect(g: TGPGraphics; ItemAppearance: TItemAppearance; R: TGPRectF); override;
    function GetItemInteraction(pX, pY: integer): TItemInteractionType; override;
    class function CustomClassName: String; override;
    function IsSection: boolean; override;
    function IsFocusable: Boolean; override;
    function FirstTab(Backwards: Boolean): Boolean; override;
    procedure InitDesignTime; override;
  published
    property LineGradient: TLineGradient read FLineGradient write SetLineGradient default lgLeftToRight;
    property LineColor: TColor read FLineColor write SetLineColor default clSilver;
    property LineColorTo: TColor read FLineColorTo write SetLineColorTo default clWhite;
    property LineMargin: TMargins read FLineMargin write SetLineMargin;
    property LineSize: Integer read FLineSize write SetLineSize default 1;
    property LineOpacity: Byte read FLineOpacity write SetLineOpacity default 255;
    property LineOpacityTo: Byte read FLineOpacityTo write SetLineOpacityTo default 255;
    property LineVisible: Boolean read FLineVisible write SetLineVisible default true;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterPolyItem(TSplitterItem);
end;

{ TSplitterItem }

procedure TSplitterItem.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TSplitterItem then
  begin
    FLineColorTo := (Source as TSplitterItem).LineColorTo;
    FLineColor := (Source as TSplitterItem).LineColor;
    FLineGradient := (Source as TSplitterItem).LineGradient;
    FLineMargin.Assign((Source as TSplitterItem).LineMargin);
    FLineSize := (Source as TSplitterItem).LineSize;
    FLineOpacity := (Source as TSplitterItem).LineOpacity;
    FLineOpacityTo := (Source as TSplitterItem).LineOpacityTo;
    FLineVisible := (Source as TSplitterItem).LineVisible;
    Changed;
  end;
end;

constructor TSplitterItem.Create;
begin
  inherited Create(AOwner);
  if (csDesigning in ComponentState) and not (csLoading in ComponentState) then
    Selectable := false;
  FLineColorTo := clWhite;
  FLineColor := clSilver;
  FLineGradient := lgLeftToRight;
  FLineMargin := TMargins.Create(nil);
  FLineMargin.OnChange := LineMarginChanged;
  FLineMargin.Left := 1;
  FLineMargin.Top := 1;
  FLineMargin.Right := 1;
  FLineMargin.Bottom := 1;
  FLineSize := 1;
  FLineOpacity := 255;
  FLineOpacityTo := 255;
  FLineVisible := true;
  if (csDesigning in ComponentState) and not (csLoading in ComponentState) then
    Height := 34;
end;

function TSplitterItem.CreateNewItem(AOwner: TComponent): TCustomItem;
begin
  Result := TSplitterItem.Create(AOwner);
end;

class function TSplitterItem.CustomClassName: String;
begin
  Result := 'Splitter Item';
end;

destructor TSplitterItem.Destroy;
begin
  FLineMargin.Free;
  inherited;
end;

procedure TSplitterItem.DrawInRect(g: TGPGraphics;
  ItemAppearance: TItemAppearance; R: TGPRectF);
begin
  if Visible then
  begin
    DoItemStartDraw(Self, g, Self, r);
    DoInternalItemStartDraw(Self, g, Self, r);

    DrawLine(g, r, ItemAppearance);
    case Status.Position of
      spItemRectangle:  DrawStatus(g, r, ItemAppearance);
      spItemText: DrawStatus(g, r, ItemAppearance);
    end;

    DoItemEndDraw(Self, g, Self, r);
    DoInternalItemEndDraw(Self, g, Self, r);
  end;
end;

procedure TSplitterItem.DrawLine(g: TGPGraphics;
  ARect: TGPRectF; ItemAppearance: TItemAppearance);
var
  r: TGPRectF;
  lr: TGPRectF;
  b: TGPBrush;
  pth: TGPGraphicsPath;
  colors: array[0..0] of TGPColor;
  cCount: integer;
  old: SmoothingMode;
begin
  if LineVisible then
  begin
    old := g.GetSmoothingMode;
    g.SetSmoothingMode(SmoothingModeDefault);
    r := ARect;
    lr := MakeRect(LineMargin.Left + r.X, r.Y + (R.Height - LineSize) / 2, r.Width - LineMargin.Left - LineMargin.Right, LineSize);
    if LineColorTo <> clNone then
    begin
      case LineGradient of
        lgLeftToRight:
        begin
          b := TGPLinearGradientBrush.Create(r, MakeColor(LineOpacity, LineColor), MakeColor(LineOpacityTo, LineColorTo), LinearGradientModeHorizontal);
          g.FillRectangle(b, lr);
          b.Free;
        end;
        lgRightToLeft:
        begin
          b := TGPLinearGradientBrush.Create(r, MakeColor(LineOpacityTo, LineColorTo), MakeColor(LineOpacity, LineColor), LinearGradientModeHorizontal);
          g.FillRectangle(b, lr);
          b.Free;
        end;
        lgCenter:
        begin
          pth := TGPGraphicsPath.Create;
          pth.AddRectangle(lr);
          b := TGPPathGradientBrush.Create(pth);
          (b as TGPPathGradientBrush).SetCenterPoint(MakePoint(lr.X + lr.Width / 2, lr.Y));
          (b as TGPPathGradientBrush).SetCenterColor(MakeColor(LineOpacity, LineColor));
          colors[0] := MakeColor(LineOpacityTo, LineColorTo);
          cCount := 1;
          (b as TGPPathGradientBrush).SetSurroundColors(@colors, cCount);
          g.FillRectangle(b, lr);
          b.Free;
          pth.Free;
        end;
      end;
    end
    else if LineColor <> clNone then
    begin
      b := TGPSolidBrush.Create(MakeColor(255, LineColor));
      g.FillRectangle(b, lr);
      b.Free;
    end;

    g.SetSmoothingMode(old);
  end;
end;

function TSplitterItem.FirstTab(Backwards: Boolean): Boolean;
begin
  ProcessTab(Backwards);
  Result := True;
end;

function TSplitterItem.GetClassType: TComponentClass;
begin
  Result := TSplitterItem;
end;

function TSplitterItem.GetItemInteraction(pX,
  pY: integer): TItemInteractionType;
begin
  Result := itNone;
  if IsStatusAtXY(pX, pY) then
    Result := itStatus;
end;

function TSplitterItem.GetVersionNr: integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TSplitterItem.InitDesignTime;
begin
  inherited;
  Selectable := false;
end;

function TSplitterItem.IsFocusable: Boolean;
begin
  Result := False;
end;

function TSplitterItem.IsSection: boolean;
begin
  Result := true;
end;

procedure TSplitterItem.LineMarginChanged(Sender: TObject);
begin
  Changed;
end;

procedure TSplitterItem.SetLineColor(const Value: TColor);
begin
  if FLineColor <> value then
  begin
    FLineColor := Value;
    Changed;
  end;
end;

procedure TSplitterItem.SetLineColorTo(const Value: TColor);
begin
  if FLineColorTo <> Value then
  begin
    FLineColorTo := Value;
    Changed;
  end;
end;

procedure TSplitterItem.SetLineGradient(const Value: TLineGradient);
begin
  if FLineGradient <> Value then
  begin
    FLineGradient := Value;
    Changed;
  end;
end;

procedure TSplitterItem.SetLineMargin(const Value: TMargins);
begin
  if FLineMargin <> value then
  begin
    FLineMargin.Assign(Value);
    Changed;
  end;
end;

procedure TSplitterItem.SetLineOpacity(const Value: Byte);
begin
  if FLineOpacity <> value then
  begin
    FLineOpacity := Value;
    Changed;
  end;
end;

procedure TSplitterItem.SetLineOpacityTo(const Value: Byte);
begin
  if FLineOpacityTo <> Value then
  begin
    FLineOpacityTo := Value;
    Changed;
  end;
end;

procedure TSplitterItem.SetLineSize(const Value: Integer);
begin
  if FLineSize <> value then
  begin
    FLineSize := Value;
    Changed;
  end;
end;

procedure TSplitterItem.SetLineVisible(const Value: Boolean);
begin
  if FLineVisible <> Value then
  begin
    FLineVisible := Value;
    Changed;
  end;
end;

end.
