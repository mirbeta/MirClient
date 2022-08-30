unit AdvSmoothTileListHTMLVisualizer;

{$I TMSDEFS.INC}

interface

uses
  Forms, Classes, Controls, AdvGDIP, AdvSmoothTileList, AdvSmoothTileListImageVisualizer;

type
  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvSmoothTileListHTMLVisualizer = class(TAdvSmoothTileListImageVisualizer)
  private
    preva: string;
  public
    function XYToAnchor(Tile: TAdvSmoothTile; pX, pY: Integer; Focus: Boolean = False): string; override;
    function DoMouseDown(Tile: TAdvSmoothTile; Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean; override;
    function DoMouseMove(Tile: TAdvSmoothTile; Shift: TShiftState; X, Y: Integer): Boolean; override;
    function DoMouseUp(Tile: TAdvSmoothTile; Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean; override;
    function DrawText(g: TGPGraphics; R: TGPRectF; Tile: TAdvSmoothTile; Text: String): TGPRectF; override;
  end;

procedure Register;

implementation

uses
  Windows, Graphics, SysUtils, GDIPPictureContainer, ImgList, CommCtrl, ShellApi;

{$I GDIPHTMLEngine.pas}

procedure Register;
begin
  RegisterComponents('TMS SmoothControls', [TAdvSmoothTileListHTMLVisualizer]);
end;

{ TAdvSmoothTileListHTMLVisualizer }

function TAdvSmoothTileListHTMLVisualizer.DoMouseDown(Tile: TAdvSmoothTile;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean;
var
  a: string;
begin
  Result := inherited DoMouseDown(Tile, Button, Shift, X, Y);
  if Assigned(Tile) then
  begin
    a := XYToAnchor(Tile, X, Y);
    if a <> '' then
      Result := False;
  end;
end;

function TAdvSmoothTileListHTMLVisualizer.DoMouseMove(Tile: TAdvSmoothTile; Shift: TShiftState; X,
  Y: Integer): Boolean;
var
  a: string;
begin
  Result := inherited DoMouseMove(Tile, Shift, X, Y);
  if Assigned(Tile) then
  begin
    a := XYToAnchor(Tile, X, Y);
    if (a <> preva) then
    begin
      preva := a;
      if a <> '' then
        Screen.Cursor := crHandPoint
      else
        Screen.Cursor := crDefault;
    end
  end;
end;

function TAdvSmoothTileListHTMLVisualizer.DoMouseUp(Tile: TAdvSmoothTile; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer): Boolean;
var
  a: string;
begin
  Result := inherited DoMouseUp(Tile, Button, Shift, X, Y);
  if Assigned(Tile) then
  begin
    a := XYToAnchor(Tile, X, Y);
    if a <> '' then
    begin
      Result := False;
      if Assigned(Tile.TileList.OnTileAnchorClick) then
        Tile.TileList.OnTileAnchorClick(Self, Tile, Tile.TileState, a);
    end;
  end;
end;

function TAdvSmoothTileListHTMLVisualizer.DrawText(g: TGPGraphics; R: TGPRectF;
  Tile: TAdvSmoothTile; Text: String): TGPRectF;
var
  rt: TGPRectF;
  ct: TAdvSmoothTileContent;
  ft: TFont;
  x, y: Double;
  a, s, k: String;
  XSize, YSize: integer;
  l, m: Integer;
  hr: TRect;
  htmlr: TRect;
begin
  if Tile.CheckContentTile then
    ct := Tile.ContentMaximized
  else
    ct := Tile.Content;


  ft := TFont.Create;
  if Tile.Enabled then
  begin
    if Tile.CheckContentTile then
      ft.Assign(Tile.TileList.TileAppearance.LargeViewFont)
    else if (Tile.TileList.SelectedTile = Tile) then
      ft.Assign(Tile.TileList.TileAppearance.SmallViewFontSelected)
    else if (Tile.TileList.HoverTile = Tile) then
      ft.Assign(Tile.TileList.TileAppearance.SmallViewFontHover)
    else
      ft.Assign(Tile.TileList.TileAppearance.SmallViewFont)
  end
  else
    ft.Assign(Tile.TileList.TileAppearance.SmallViewFontDisabled);

  if Assigned(Tile.TileList.OnTileFont) then
    Tile.TileList.OnTileFont(Self, Tile, Tile.TileState, ft);

  rt := MakeRect(r.X + 2, r.Y + 2, r.Width - 4, r.Height - 4);


  if ct.TextPosition <> tpCustom then
    htmlr := Bounds(0, 0, Round(rt.Width), Round(rt.Height))
  else
    htmlr := Bounds(0, 0, Round(rt.Width - ct.TextLeft), Round(rt.Height - ct.TextTop));

  HTMLDrawGDIP(g, ft, Text, htmlr, Tile.TileList.ImageList, 0, 0, -1, -1, 0, False, True, False, False,
    False, False, True, 1.0, clBlue, clNone, clNone, clNone, a, s, k, XSize, YSize, l, m, hr, nil, Tile.TileList.PictureContainer, 2);


  if (ct.TextPosition <> tpCustom) and (ct.TextPosition <> tpHTMLAlign) then
    GetTextPosition(x, y, rt, XSize, YSize, ct.TextPosition)
  else
  begin
    x := ct.TextLeft;
    y := ct.TextTop;
  end;

  x := x + rt.X;
  y := y + rt.Y;

  if ct.TextPosition = tpHTMLAlign then
  begin
    xSize := Round(rt.Width);
    ySize := Round(rt.Height);
  end;

  HTMLDrawGDIP(g, ft, Text, Bounds(Round(x), Round(y), XSize, YSize), Tile.TileList.ImageList, 0, 0, -1, -1, 0, False, False, False, False,
    False, False, True, 1.0, clBlue, clNone, clNone, clNone, a, s, k, XSize, YSize, l, m, hr, nil, Tile.TileList.PictureContainer, 2);

  ft.Free;

  Result := MakeRect(x, y, XSize, YSize);
end;

function TAdvSmoothTileListHTMLVisualizer.XYToAnchor(Tile: TAdvSmoothTile; pX,
  pY: Integer; Focus: Boolean = False): string;
var
  rt: TGPRectF;
  ct: TAdvSmoothTileContent;
  ft: TFont;
  x, y: Double;
  a, s, k: String;
  XSize, YSize: integer;
  l, m: Integer;
  hr: TRect;
  bmp: TBitmap;
  g: TGPGraphics;
  R: TGPRectF;
  chkmove: Boolean;
  str: String;
  htmlr: TRect;
begin
  bmp := TBitmap.Create;
  g := TGPGraphics.Create(bmp.Canvas.handle);
  if Tile.CheckContentTile then
    ct := Tile.ContentMaximized
  else
    ct := Tile.Content;

  str := ct.Text;
  if Assigned(Tile.TileList.OnTileText) then
    Tile.TileList.OnTileText(Self, Tile, Tile.TileState, str);


  R := Tile.TileRectangle;
  chkmove := (Tile.TileList.Mode = tmEdit) and (Tile = Tile.TileList.MoveTile);

  if not chkmove then
    R.X := R.X + Tile.TileList.TilePos;

  if Tile.Enabled then
  begin
    if Tile.CheckContentTile then
      ft := Tile.TileList.TileAppearance.LargeViewFont
    else if (Tile.TileList.SelectedTile = Tile) then
      ft := Tile.TileList.TileAppearance.SmallViewFontSelected
    else if (Tile.TileList.HoverTile = Tile) then
      ft := Tile.TileList.TileAppearance.SmallViewFontHover
    else
      ft := Tile.TileList.TileAppearance.SmallViewFont
  end
  else
    ft := Tile.TileList.TileAppearance.SmallViewFontDisabled;

  rt := MakeRect(r.X + 2, r.Y + 2, r.Width - 4, r.Height - 4);


  if ct.TextPosition <> tpCustom then
    htmlr := Bounds(0, 0, Round(rt.Width), Round(rt.Height))
  else
    htmlr := Bounds(0, 0, Round(rt.Width - ct.TextLeft), Round(rt.Height - ct.TextTop));

  HTMLDrawGDIP(g, ft, str, htmlr, Tile.TileList.ImageList, 0, 0, -1, -1, 0, False, True, False, False,
    False, False, True, 1.0, clBlue, clNone, clNone, clNone, a, s, k, XSize, YSize, l, m, hr, nil, Tile.TileList.PictureContainer, 2);


  x := 0;
  y := 0;
  if ct.TextPosition <> tpCustom then
    GetTextPosition(x, y, rt, XSize, YSize, ct.TextPosition)
  else
  begin
    x := ct.TextLeft;
    y := ct.TextTop;
  end;

  x := x + rt.X;
  y := y + rt.Y;


  HTMLDrawGDIP(g, ft, str, Bounds(Round(x), Round(y), XSize, YSize), Tile.TileList.ImageList, pX, pY, -1, -1, 0, False, False, False, False,
    False, False, True, 1.0, clBlue, clNone, clNone, clNone, a, s, k, XSize, YSize, l, m, hr, nil, Tile.TileList.PictureContainer, 2);

  g.free;
  bmp.free;

  if Focus then
    Result := k
  else
    Result := a;
end;

end.
