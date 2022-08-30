{*************************************************************************}
{ TImageSectionItem Class                                                 }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright ©  2010 - 2013                                      }
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

unit GDIPImageSectionItem;

interface

{$I TMSDEFS.INC}

uses
  Windows, Classes, Controls, Graphics,
  AdvGDIP, GDIPSectionItem, GDIPCustomItem, ImgList
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.

type
  TImageSectionItem = class(TSectionItem)
  private
    FImageHeight: integer;
    FImage: TAdvGDIPPicture;
    FSpacing: Integer;
    FImageWidth: integer;
    FImageVisible: Boolean;
    FImageName: String;
    FImageIndex: Integer;
    procedure SetImage(const Value: TAdvGDIPPicture);
    procedure SetImageHeight(const Value: integer);
    procedure SetImageVisible(const Value: Boolean);
    procedure SetImageWidth(const Value: integer);
    procedure SetSpacing(const Value: Integer);
    procedure SeInteger(const Value: Integer);
    procedure SetImageName(const Value: String);
  protected
    function DrawText(g: TGPGraphics; ARect: TGPRectF; ItemAppearance: TItemAppearance; DoText: Boolean = true): TGPRectF; override;
    procedure DrawImage(g: TGPGraphics; ARect: TGPRectF; ItemAppearance: TItemAppearance); virtual;
    function DrawImageFromImageList(g: TGPGraphics; ARect: TGPRectF; ItemAppearance: TItemAppearance; Image: integer): Boolean; virtual;
    function DrawImageFromContainer(g: TGPGraphics; ARect: TGPRectF; ItemAppearance: TItemAppearance; Image: String): Boolean; virtual;
    procedure ImageChanged(Sender: TObject);
    function GetVersionNr: integer; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CreateNewItem(AOwner: TComponent): TCustomItem; override;
    procedure Assign(Source: TPersistent); override;
    function GetClassType: TComponentClass; override;
    procedure DrawInRect(g: TGPGraphics; ItemAppearance: TItemAppearance; R: TGPRectF); override;
    class function CustomClassName: String; override;
    function GetItemInteraction(pX, pY: integer): TItemInteractionType; override;
    function IsImageVisible(ItemAppearance: TItemAppearance): Boolean;
    procedure InitDesignTime; override;
    function IsSection: boolean; override;
  published
    property Image: TAdvGDIPPicture read FImage write SetImage;
    property ImageHeight: integer read FImageHeight write SetImageHeight default 18;
    property ImageWidth: integer read FImageWidth write SetImageWidth default 18;
    property ImageVisible: Boolean read FImageVisible write SetImageVisible default true;
    property ImageIndex: Integer read FImageIndex write SeInteger default -1;
    property ImageName: String read FImageName write SetImageName;
    property Spacing: Integer read FSpacing write SetSpacing default 3;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterPolyItem(TImageSectionItem);
end;

{ TImageSectionItem }

procedure TImageSectionItem.Assign(Source: TPersistent);
begin
  inherited;
  if (Source is TImageSectionItem) then
  begin
    FImage.Assign((Source as TImageSectionItem).Image);
    FImageHeight := (Source as TImageSectionItem).ImageHeight;
    FImageWidth := (Source as TImageSectionItem).ImageWidth;
    FImageVisible := (Source as TImageSectionItem).ImageVisible;
    FSpacing := (Source as TImageSectionItem).Spacing;
    FImageName := (Source as TImageSectionItem).ImageName;
    FImageIndex := (Source as TImageSectionItem).ImageIndex;
    Changed;
  end;
end;

constructor TImageSectionItem.Create(AOwner: TComponent);
var
  FDesignTime: boolean;
begin
  inherited Create(AOwner);
  if (csDesigning in ComponentState) and not (csLoading in ComponentState) then
    Selectable := false;
  FImage := TAdvGDIPPicture.Create;
  FImage.OnChange := ImageChanged;
  FImageHeight := 18;
  FImageWidth := 18;
  FImageVisible := true;
  FSpacing := 3;
  FImageIndex := -1;
  FImageName := '';

  FDesignTime := false;
  if Assigned(Owner) then
  begin
    FDesignTime := (csDesigning in ComponentState) and not
      ((csReading in Owner.ComponentState) or (csLoading in Owner.ComponentState));
  end;

  if FDesignTime then
    LoadFromRes('placeholder', FImage);
end;

function TImageSectionItem.CreateNewItem(AOwner: TComponent): TCustomItem;
begin
  Result := TImageSectionItem.Create(AOwner);
end;

class function TImageSectionItem.CustomClassName: String;
begin
  Result := 'Section Image Item';
end;

destructor TImageSectionItem.Destroy;
begin
  FImage.Free;
  inherited;
end;

procedure TImageSectionItem.DrawInRect(g: TGPGraphics;
  ItemAppearance: TItemAppearance; R: TGPRectF);
var
  ir: TGPRectF;
begin
  if Visible then
  begin
    DoItemStartDraw(Self, g, Self, r);
    DoInternalItemStartDraw(Self, g, Self, r);

    DrawLine(g, r, ItemAppearance);
    ir := DrawText(g, r, ItemAppearance);
    case Status.Position of
      spItemRectangle:  DrawStatus(g, r, ItemAppearance);
      spItemText: DrawStatus(g, ir, ItemAppearance);
    end;

    DrawImage(g, r, ItemAppearance);

    DoItemEndDraw(Self, g, Self, r);
    DoInternalItemEndDraw(Self, g, Self, r);
  end;
end;

procedure TImageSectionItem.DrawImage(g: TGPGraphics;
  ARect: TGPRectF; ItemAppearance: TItemAppearance);
var
  imgr: TGPRectF;
begin
  if ImageVisible then
  begin
    imgr := MakeRect(ARect.X + Spacing, ARect.Y + (ARect.Height - ImageHeight) / 2, ImageWidth, ImageHeight);
    if Assigned(Image) and not Image.Empty then
    begin
      Image.GDIPDraw(g, imgr);
    end;
    DrawImageFromImageList(g, imgr, ItemAppearance, ImageIndex);
    DrawImageFromContainer(g, imgr, ItemAppearance, ImageName);
  end;
end;

function TImageSectionItem.DrawImageFromContainer(g: TGPGraphics;
  ARect: TGPRectF; ItemAppearance: TItemAppearance; Image: String): Boolean;
var
  pic: TAdvGDIPPicture;
begin
  Result := false;
  if Assigned(ItemAppearance.PictureContainer) then
  begin
    if Image <> '' then
    begin
      pic := ItemAppearance.PictureContainer.FindPicture(Image);
      if Assigned(pic) then
      begin
        pic.GDIPDraw(g, ARect);
        Result := true;
      end;
    end;
  end;
end;

function TImageSectionItem.DrawImageFromImageList(g: TGPGraphics;
  ARect: TGPRectF; ItemAppearance: TItemAppearance; Image: integer): Boolean;
var
  bmp: TBitmap;
  h: THandle;
  ca: TCanvas;
  gpbmp: TGPBitmap;
  gp: TGPGraphics;
begin
  Result := false;
  if Assigned(ItemAppearance.ImageList) then
  begin
    if (Image >= 0) and (Image <= ItemAppearance.ImageList.Count - 1) then
    begin
      bmp := TBitmap.Create;
      try
        ItemAppearance.ImageList.GetBitmap(Image, bmp);

        if not bmp.Empty then
        begin
          gpbmp := TGPBitmap.Create(bmp.Width, bmp.Height);
          gp := TGPGraphics.Create(gpbmp);
          h := gp.GetHDC;
          ca := TCanvas.Create;
          ca.Handle := h;
          ItemAppearance.ImageList.Draw(ca, 0, 0, Image);
          ca.Free;
          gp.ReleaseHDC(h);
          gp.Free;

          g.DrawImageRect(gpbmp, Arect);
          Result := true;
          gpbmp.Free;
        end;
      finally
        bmp.Free;
      end;
    end;
  end;
end;

function TImageSectionItem.DrawText(g: TGPGraphics;
  ARect: TGPRectF; ItemAppearance: TItemAppearance; DoText: Boolean = true): TGPRectF;
var
  textr, textoutr: TGPRectF;
  textx, texty: Double;
  txtlt: integer;
  ff: TGPFontFamily;
  fs: integer;
  sf: TGPStringFormat;
  b: TGPBrush;
  f: TGPFont;
  ft: TFont;
  caph: Double;
  r: TGPRectF;
  fst: TFontStyles;
begin
  r := ARect;
  if IsImageVisible(ItemAppearance) then
  begin
    r.X := r.X + ImageWidth + Spacing;
    r.Width := r.Width - ImageWidth - Spacing;
  end;
  Result := r;
  if (Caption = '') and (Description = '') then
    Exit;

  with ItemAppearance do
  begin
    ft := GetFont(ItemAppearance);
    ff := TGPFontFamily.Create(ft.Name);
    if (ff.Status in [FontFamilyNotFound, FontStyleNotFound]) then
    begin
      ff.Free;
      ff := TGPFontFamily.Create('Arial');
    end;

    sf := TGPStringFormat.Create;
    sf.SetHotkeyPrefix(HotkeyPrefixShow);

    caph := 0;
    if Caption <> '' then
    begin
      if CaptionFontStyle <> [] then
        fst := CaptionFontStyle
      else
        fst := ft.Style;

      fs := 0;
      if (fsBold in fst) then
        fs := fs + 1;
      if (fsItalic in fst) then
        fs := fs + 2;
      if (fsUnderline in fst) then
        fs := fs + 4;
      if (fsStrikeOut in fst) then
        fs := fs + 8;

      if CaptionSize <> -1 then
        f := TGPFont.Create(ff, CaptionSize, fs, UnitPoint)
      else
        f := TGPFont.Create(ff, ft.Size, fs, UnitPoint);

      if CaptionColor <> clNone then
        b := TGPSolidBrush.Create(MakeColor(CaptionOpacity, CaptionColor))
      else
        b := TGPSolidBrush.Create(MakeColor(CaptionOpacity, ft.Color));

      textr := MakeRect(r.X + CaptionMargin.Left, r.Y + CaptionMargin.Top, r.Width - CaptionMargin.Right - CaptionMargin.Left,
        r.Height - CaptionMargin.Bottom - CaptionMargin.Top);

      txtlt := length(Caption);

      sf.SetFormatFlags(0);
      sf.SetTrimming(StringTrimmingNone);

      if Description = '' then
      begin
        if not WordWrap and Ellipsis then
        begin
          sf.SetFormatFlags(StringFormatFlagsNoWrap);
          sf.SetTrimming(StringTrimmingEllipsisWord);
        end
        else if not WordWrap then
          sf.SetFormatFlags(StringFormatFlagsNoWrap);
      end
      else
      begin
        if Ellipsis then
        begin
          sf.SetFormatFlags(StringFormatFlagsNoWrap);
          sf.SetTrimming(StringTrimmingEllipsisWord);
        end;
      end;

      g.MeasureString(Caption, txtlt, f, textr, sf, textoutr);

      if Description <> '' then
        textr.Height := textoutr.Height + CaptionMargin.Bottom + CaptionMargin.Top;

      textx := 0;
      texty := 0;
      if CaptionLocation <> tlCustom then
        GetObjectLocation(textx, texty, textr, textoutr.Width, textoutr.Height, CaptionLocation)
      else
      begin
        textx := CaptionLeft;
        texty := CaptionTop;
      end;

      textx := textx + textr.X;
      texty := texty + textr.Y;

      if DoText then
      begin
        g.DrawString(Caption, txtlt, f, MakeRect(textx, texty, textr.Width - (textx - textr.X), textr.Height - (texty - textr.Y)),
          sf, b);
      end;

      b.Free;
      f.Free;

      caph := textr.Height;
    end;

    Result := MakeRect(textx, texty, textoutr.Width, textoutr.Height);

    if Description <> '' then
    begin
      if DescriptionFontStyle <> [] then
        fst := DescriptionFontStyle
      else
        fst := ft.Style;

      fs := 0;
      if (fsBold in fst) then
        fs := fs + 1;
      if (fsItalic in fst) then
        fs := fs + 2;
      if (fsUnderline in fst) then
        fs := fs + 4;
      if (fsStrikeOut in fst) then
        fs := fs + 8;

      if DescriptionSize <> -1 then
        f := TGPFont.Create(ff, DescriptionSize, fs, UnitPoint)
      else
        f := TGPFont.Create(ff, ft.Size, fs, UnitPoint);

      if DescriptionColor <> clNone then
        b := TGPSolidBrush.Create(MakeColor(DescriptionOpacity, DescriptionColor))
      else
        b := TGPSolidBrush.Create(MakeColor(DescriptionOpacity, ft.Color));

      if caph = 0 then
      begin
        textr := MakeRect(r.X + DescriptionMargin.Left, r.Y + DescriptionMargin.Top, r.Width - DescriptionMargin.Right - DescriptionMargin.Left,
          r.Height - DescriptionMargin.Bottom - DescriptionMargin.Top);
      end
      else
      begin
        textr := MakeRect(r.X + DescriptionMargin.Left, r.Y + CaptionMargin.Top + DescriptionMargin.Top + caph , r.Width - DescriptionMargin.Right - DescriptionMargin.Left,
          r.Height - DescriptionMargin.Bottom - DescriptionMargin.Top - caph - CaptionMargin.Top);
      end;

      txtlt := length(Description);

      sf.SetFormatFlags(0);
      sf.SetTrimming(StringTrimmingNone);

      if not WordWrap and Ellipsis then
      begin
        sf.SetFormatFlags(StringFormatFlagsNoWrap);
        sf.SetTrimming(StringTrimmingEllipsisWord);
      end
      else if not WordWrap then
        sf.SetFormatFlags(StringFormatFlagsNoWrap);

      g.MeasureString(Description, txtlt, f, textr, sf, textoutr);

      textx := 0;
      texty := 0;
      if DescriptionLocation <> tlCustom then
        GetObjectLocation(textx, texty, textr, textoutr.Width, textoutr.Height, DescriptionLocation)
      else
      begin
        textx := DescriptionLeft;
        texty := DescriptionTop;
      end;

      textx := textx + textr.X;
      texty := texty + textr.Y;

      if DoText then
      begin
        g.DrawString(Description, txtlt, f, MakeRect(textx, texty, textr.Width - (textx - textr.X), textr.Height - (texty - textr.Y)),
          sf, b);
      end;

      b.Free;
      f.Free;
    end;
    sf.Free;
    ff.Free;
  end;
end;

function TImageSectionItem.GetClassType: TComponentClass;
begin
  Result := TImageSectionItem;
end;

function TImageSectionItem.GetItemInteraction(pX,
  pY: integer): TItemInteractionType;
begin
  Result := itNone;
  if IsStatusAtXY(pX, pY) then
    Result := itStatus;
end;

function TImageSectionItem.GetVersionNr: integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TImageSectionItem.ImageChanged(Sender: TObject);
begin
  Changed;
end;

procedure TImageSectionItem.InitDesignTime;
begin
  inherited;
  LoadFromRes('placeholder', FImage);
end;

function TImageSectionItem.IsImageVisible(
  ItemAppearance: TItemAppearance): Boolean;
begin
  Result := (not Image.Empty);
  if Assigned(ItemAppearance.ImageList) then
    Result := Result or ((ImageIndex >=0) and (ImageIndex <= ItemAppearance.ImageList.Count - 1));

  if Assigned(ItemAppearance.PictureContainer) then
    Result := Result or (Assigned(ItemAppearance.PictureContainer.FindPicture(ImageName)) and (ImageName <> ''));

  Result := Result and ImageVisible;
end;

function TImageSectionItem.IsSection: boolean;
begin
  Result := true;
end;

procedure TImageSectionItem.SetImage(const Value: TAdvGDIPPicture);
begin
  if FImage <> value then
  begin
    FImage.Assign(Value);
    Changed;
  end;
end;

procedure TImageSectionItem.SetImageHeight(const Value: integer);
begin
  if FImageHeight <> Value then
  begin
    FImageHeight := Value;
    Changed;
  end;
end;

procedure TImageSectionItem.SeInteger(const Value: Integer);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    Changed;
  end;
end;

procedure TImageSectionItem.SetImageName(const Value: String);
begin
  if FImageName <> Value then
  begin
    FImageName := Value;
    Changed;
  end;
end;

procedure TImageSectionItem.SetImageVisible(const Value: Boolean);
begin
  if FImageVisible <> Value then
  begin
    FImageVisible := Value;
    Changed;
  end;
end;

procedure TImageSectionItem.SetImageWidth(const Value: integer);
begin
  if FImageWidth <> Value then
  begin
    FImageWidth := Value;
    Changed;
  end;
end;

procedure TImageSectionItem.SetSpacing(const Value: Integer);
begin
  if FSpacing <> Value then
  begin
    FSpacing := Value;
    Changed;
  end;
end;

end.
