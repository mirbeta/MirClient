{*************************************************************************}
{ TWedgeItem Class                                                        }
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


unit GDIPWedgeItem;

interface

{$I TMSDEFS.inc}

uses
  Windows, Classes, GDIPCustomItem, Graphics, GDIPImageTextItem, AdvGDIP, GDIPFill;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.

  //v0.9.0.0 : First Beta Release
  //v1.0.0.0 : First Release


type
  TWedgePosition = (wpLeft, wpTop, wpBottom, wpRight);

  TWedgePositions = set of TWedgePosition;

  TWedgeItem = class(TImageTextItem)
  private
    FWedgePositions: TWedgePositions;
    FWedgeSize: integer;
    FWedgeStates: TItemStates;
    procedure SetWedgePositions(const Value: TWedgePositions);
    procedure SetWedgeSize(const Value: integer);
    procedure SetWedgeStates(const Value: TItemStates);
  protected
    function GetVersionNr: integer; override;
    procedure DrawFocus(g: TGPGraphics; ARect: TGPRectF; ItemAppearance: TItemAppearance); override;
    function GetWedgeRect(ARect: TGPRectF): TGPRectF;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;
    function CreateNewItem(AOwner: TComponent): TCustomItem; override;
    function GetClassType: TComponentClass; override;
    class function CustomClassName: String; override;
    procedure DrawInRect(g: TGPGraphics; ItemAppearance: TItemAppearance; R: TGPRectF); override;
    function DrawText(g: TGPGraphics; ARect: TGPRectF; ItemAppearance: TItemAppearance; DoText: Boolean = true): TGPRectF; override;
    procedure DrawImage(g: TGPGraphics; ARect: TGPRectF; ItemAppearance: TItemAppearance); override;
  published
    property WedgePositions: TWedgePositions read FWedgePositions write SetWedgePositions default [wpRight];
    property WedgeSize: integer read FWedgeSize write SetWedgeSize default 6;
    property WedgeStates: TItemStates read FWedgeStates write SetWedgeStates default [isNormal, isHovered, isDown, isSelected];
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterPolyItem(TWedgeItem);
end;

{ TWedgeItem }

procedure TWedgeItem.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TWedgeItem then
  begin
    FWedgePositions := (Source as TWedgeItem).WedgePositions;
    FWedgeSize := (Source as TWedgeItem).WedgeSize;
    FWedgeStates := (Source as TWedgeItem).WedgeStates;
    Changed;
  end;
end;

constructor TWedgeItem.Create(AOwner: TComponent);
begin
  inherited;
  FWedgePositions := [wpRight];
  FWedgeSize := 6;
  FWedgeStates := [isNormal, isHovered, isDown, isSelected];
end;

function TWedgeItem.CreateNewItem(AOwner: TComponent): TCustomItem;
begin
  Result := TWedgeItem.Create(AOwner);
end;

class function TWedgeItem.CustomClassName: String;
begin
  Result := 'Normal Wedge Item';
end;

procedure TWedgeItem.DrawInRect(g: TGPGraphics; ItemAppearance: TItemAppearance;
  R: TGPRectF);
type
  TPointArray = array of TGPPointF;
var
  rgn: TGPRegion;
  f: TGDIPFill;
  s: integer;
  pth: TGPGraphicsPath;
  wdRight, wdLeft, wdTop, wdBottom: TPointArray;
  p: TGPPen;
  ir: TGPRectF;
begin
  if (WedgePositions <> []) and (self.State in WedgeStates) then
  begin
    ir := MakeRect(r.x, r.y, r.width + 1, r.height + 1);
    f := GetFill(ItemAppearance);
    p := TGPPen.Create(MakeColor(f.BorderOpacity, f.BorderColor), f.BorderWidth);
    rgn := TGPRegion.Create(ir);
    s := WedgeSize;
    pth := TGPGraphicsPath.Create;

    if wpRight in WedgePositions then
    begin
      SetLength(wdRight, 3);
      wdRight[0] := MakePoint(ir.X + ir.Width, ir.Y - s + (ir.Height - 1) / 2);
      wdRight[1] := MakePoint(ir.X + ir.Width - s, ir.Y + (ir.Height - 1) / 2);
      wdRight[2] := MakePoint(ir.X + ir.Width, ir.Y + s + (ir.Height - 1) / 2);
      pth.AddPolygon(PGPPointF(wdRight), 3);
      rgn.Exclude(pth);
    end;

    if wpLeft in WedgePositions then
    begin
      SetLength(wdLeft, 3);
      wdLeft[0] := MakePoint(ir.X, ir.Y - s + ir.Height / 2);
      wdLeft[1] := MakePoint(ir.X + s, ir.Y + ir.Height / 2);
      wdLeft[2] := MakePoint(ir.X, ir.Y + s + ir.Height / 2);
      pth.AddPolygon(PGPPointF(wdLeft), 3);
      rgn.Exclude(pth);
    end;

    if wpTop in WedgePositions then
    begin
      SetLength(wdTop, 3);
      wdTop[0] := MakePoint(ir.X - s + ir.Width / 2, ir.Y);
      wdTop[1] := MakePoint(ir.X + ir.Width / 2, ir.Y + s);
      wdTop[2] := MakePoint(ir.X + s + ir.Width / 2, ir.Y);
      pth.AddPolygon(PGPPointF(wdTop), 3);
      rgn.Exclude(pth);
    end;

    if wpBottom in WedgePositions then
    begin
      SetLength(wdBottom, 3);
      wdBottom[0] := MakePoint(ir.X - s + ir.Width / 2, ir.Y + ir.Height);
      wdBottom[1] := MakePoint(ir.X + ir.Width / 2, ir.Y + ir.Height - s);
      wdBottom[2] := MakePoint(ir.X + s + ir.Width / 2, ir.Y + ir.Height);
      pth.AddPolygon(PGPPointF(wdBottom), 3);
      rgn.Exclude(pth);
    end;

    g.SetClip(rgn);

    inherited;

    g.ResetClip;
    pth.Free;
    rgn.Free;

    if wpRight in WedgePositions then
    begin
      g.DrawLine(p, wdRight[0].X - 1, wdRight[0].Y, wdRight[1].X - 1, wdRight[1].Y);
      g.DrawLine(p, wdRight[1].X - 1, wdRight[1].Y, wdRight[2].X - 1, wdRight[2].Y);
    end;

    if wpLeft in WedgePositions then
    begin
      g.DrawLine(p, wdleft[0].X, wdleft[0].Y, wdleft[1].X, wdleft[1].Y);
      g.DrawLine(p, wdleft[1].X, wdleft[1].Y, wdleft[2].X, wdleft[2].Y);
    end;

    if wptop in WedgePositions then
    begin
      g.DrawLine(p, wdTop[0].X, wdTop[0].Y, wdTop[1].X, wdTop[1].Y);
      g.DrawLine(p, wdTop[1].X, wdTop[1].Y, wdTop[2].X, wdTop[2].Y);
    end;

    if wpBottom in WedgePositions then
    begin
      g.DrawLine(p, wdBottom[0].X, wdBottom[0].Y - 1, wdBottom[1].X, wdBottom[1].Y - 1);
      g.DrawLine(p, wdBottom[1].X, wdBottom[1].Y - 1, wdBottom[2].X, wdBottom[2].Y - 1);
    end;

    p.Free;

  end
  else
    inherited;
end;

procedure TWedgeItem.DrawFocus(g: TGPGraphics; ARect: TGPRectF;
  ItemAppearance: TItemAppearance);
begin
  inherited DrawFocus(g, GetWedgeRect(ARect), ItemAppearance);
end;

procedure TWedgeItem.DrawImage(g: TGPGraphics; ARect: TGPRectF;
  ItemAppearance: TItemAppearance);
begin
  inherited DrawImage(g, GetWedgeRect(ARect), ItemAppearance);
end;

function TWedgeItem.DrawText(g: TGPGraphics; ARect: TGPRectF;
  ItemAppearance: TItemAppearance; DoText: Boolean = true): TGPRectF;
begin
  Result := inherited DrawText(g, GetWedgeRect(ARect), ItemAppearance, DoText);
end;

function TWedgeItem.GetClassType: TComponentClass;
begin
  Result := TWedgeItem;
end;

function TWedgeItem.GetVersionNr: integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

function TWedgeItem.GetWedgeRect(ARect: TGPRectF): TGPRectF;
begin
  Result := ARect;
  if (self.State in WedgeStates) then
  begin
    if wpRight in WedgePositions then
    begin
      Result.Width := Result.Width - WedgeSize;
    end;

    if wpLeft in WedgePositions then
    begin
      Result.X := Result.X + WedgeSize;
      Result.Width := Result.Width - WedgeSize;
    end;

    if wptop in WedgePositions then
    begin
      Result.Y := Result.Y + WedgeSize;
      Result.Height := Result.Height - WedgeSize;
    end;

    if wpBottom in WedgePositions then
    begin
      Result.Height := Result.Height - WedgeSize;
    end;
  end;
end;

procedure TWedgeItem.SetWedgePositions(const Value: TWedgePositions);
begin
  if FWedgePositions <> Value then
  begin
    FWedgePositions := Value;
    Changed;
  end;
end;

procedure TWedgeItem.SetWedgeSize(const Value: integer);
begin
  if FWedgeSize <> Value then
  begin
    FWedgeSize := Value;
    Changed;
  end;
end;

procedure TWedgeItem.SetWedgeStates(const Value: TItemStates);
begin
  if FWedgeStates <> Value then
  begin
    FWedgeStates := Value;
    Changed;
  end;
end;

end.
