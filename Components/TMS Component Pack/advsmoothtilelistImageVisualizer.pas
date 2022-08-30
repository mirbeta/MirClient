unit AdvSmoothTileListImageVisualizer;

{$I TMSDEFS.INC}

interface

uses
  Classes, Graphics, AdvGDIP, AdvSmoothTileList, GDIPFill;

type
  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvSmoothTileListImageVisualizer = class(TAdvSmoothTileListVisualizer)
  public
    procedure DrawImage(g: TGPGraphics; Tile: TAdvSmoothTile; R, RText: TGPRectF); virtual;
    function DrawText(g: TGPGraphics; R: TGPRectF; Tile: TAdvSmoothTile; Text: String): TGPRectF; override;
    function DrawTile(g: TGPGraphics; R: TGPRectF; Tile: TAdvSmoothTile): TGPRectF; override;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS SmoothControls', [TAdvSmoothTileListImageVisualizer]);
end;


{ TAdvSmoothImageVisualizer }

procedure TAdvSmoothTileListImageVisualizer.DrawImage(g: TGPGraphics;
  Tile: TAdvSmoothTile; R, RText: TGPRectF);
var
  rt, imgr: TGPRectF;
  pic: TAdvGDIPPicture;
  ct: TAdvSmoothTileContent;
  chkContent: Boolean;
  bmp: TBitmap;
  w, h: Double;
  tiler: TGPRectF;
begin
  chkcontent := Tile.CheckContentTile;
  if chkContent then
    ct := Tile.ContentMaximized
  else
    ct := Tile.Content;

  tiler := R;

  case ct.TextPosition of
    tpCustom: rt := tiler;
    tpBottomLeft, tpBottomCenter, tpBottomRight: rt := MakeRect(tiler.X, tiler.Y, tiler.Width, tiler.Height - rtext.Height - 4);
    tpTopLeft, tpTopCenter, tpTopRight: rt := MakeRect(tiler.X,tiler.Y + 4 + rtext.Height, tiler.Width, tiler.Height - rtext.Height - 4);
    tpCenterLeft: rt := MakeRect(tiler.X + rtext.Width + 4, tiler.Y, tiler.Width - rtext.Width - 4, tiler.Height);
    tpCenterCenter: rt := tiler;
    tpCenterRight: rt := MakeRect(tiler.X, tiler.Y, tiler.Width - rtext.Width - 4,tiler.Height);
  end;

  rt := MakeRect(rt.X + 2, rt.Y + 2, rt.Width - 4, rt.Height - 4);

  pic := TAdvGDIPPicture.Create;

  pic.Assign(ct.Image);

  if Assigned(Tile.TileList.PictureContainer) then
    if ct.ImageName <> '' then
      pic.Assign(Tile.TileList.PictureContainer.FindPicture(ct.ImageName));


  if Assigned(Tile.TileList.ImageList) then
  begin
    if (ct.ImageIndex >= 0) and (ct.ImageIndex <= Tile.TileList.ImageList.Count - 1) then
    begin
      bmp := TBitmap.Create;
      Tile.TileList.ImageList.GetBitmap(ct.ImageIndex, bmp);
      if not bmp.Empty then
        pic.Assign(bmp);
      bmp.Free;
    end;
  end;

  pic.GetImageSizes;
  if ct.ImageAspectRatio then
  begin
    if ct.ImageStretch then
      GetAspectSize(w, h, pic.Width, pic.Height, rt.Width, rt.Height, pmStretch)
    else
      GetAspectSize(w, h, pic.Width, pic.Height, rt.Width, rt.Height, pmNormal)
  end
  else
  begin
    w := rt.Width;
    h := rt.Height;
  end;

  imgr := MakeRect(rt.X + (rt.Width - w) / 2, rt.Y + (rt.Height - h) / 2, w, h);

  if (imgr.Width > 0) and (imgr.Height > 0) and not pic.Empty and not (pic.Width = 0) and not (pic.Height = 0) and RectanglesInterSect(imgr, tiler) then
    pic.GDIPDraw(g, imgr);

  pic.Free;
end;

function TAdvSmoothTileListImageVisualizer.DrawText(g: TGPGraphics; R: TGPRectF;
  Tile: TAdvSmoothTile; Text: String): TGPRectF;
begin
  Result := inherited DrawText(g, R, Tile, Text);
end;

function TAdvSmoothTileListImageVisualizer.DrawTile(g: TGPGraphics;
  R: TGPRectF; Tile: TAdvSmoothTile): TGPRectF;
begin
  Result := inherited DrawTile(g, R, Tile);
  DrawImage(g, Tile, R, Result);
end;

end.
