{*************************************************************************}
{ TImageTextItem Class                                                    }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright ©  2010 - 2015                                      }
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

unit GDIPImageTextItem;

interface

{$I TMSDEFS.INC}

uses
  Windows, Graphics, Classes, GDIPCustomItem, GDIPTextItem, AdvGDIP,
  ActnList, ImgList
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.

  //v0.9.0.0 : First Beta Release
  //v1.0.0.0 : First Release

type
  TImageTextItem = class(TTextItem)
  private
    FImage: TAdvGDIPPicture;
    FImageHeight: integer;
    FImageWidth: integer;
    FImageVisible: Boolean;
    FSpacing: Integer;
    FImageName: String;
    FImageIndex: TImageIndex;
    procedure SetImage(const Value: TAdvGDIPPicture);
    procedure SetImageHeight(const Value: integer);
    procedure SetImageVisible(const Value: Boolean);
    procedure SetImageWidth(const Value: integer);
    procedure SetSpacing(const Value: Integer);
    procedure SetImageIndex(const Value: TImageIndex);
    procedure SetImageName(const Value: String);
  protected
    procedure ImageChanged(Sender: TObject);
    function DrawImageFromImageList(g: TGPGraphics; ARect: TGPRectF; ItemAppearance: TItemAppearance; Image: integer): Boolean; virtual;
    function DrawImageFromContainer(g: TGPGraphics; ARect: TGPRectF; ItemAppearance: TItemAppearance; Image: String): Boolean; virtual;
    function GetVersionNr: integer; override;
    procedure ActionSetImageIndex(Value: Integer); override;
    function ActionIsImageIndexLinked: Boolean; override;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function CreateNewItem(AOwner: TComponent): TCustomItem; override;
    function GetClassType: TComponentClass; override;
    class function CustomClassName: String; override;
    procedure DrawInRect(g: TGPGraphics; ItemAppearance: TItemAppearance; R: TGPRectF); override;
    function DrawText(g: TGPGraphics; ARect: TGPRectF; ItemAppearance: TItemAppearance; DoText: Boolean = true): TGPRectF; override;
    procedure DrawImage(g: TGPGraphics; ARect: TGPRectF; ItemAppearance: TItemAppearance); virtual;
    function GetItemInteraction(pX, pY: integer): TItemInteractionType; override;
    function IsImageVisible(ItemAppearance: TItemAppearance): Boolean;
    procedure InitDesignTime; override;
  published
    property Action;
    property Image: TAdvGDIPPicture read FImage write SetImage;
    property ImageHeight: integer read FImageHeight write SetImageHeight default 18;
    property ImageWidth: integer read FImageWidth write SetImageWidth default 18;
    property ImageVisible: Boolean read FImageVisible write SetImageVisible default true;
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex default -1;
    property ImageName: String read FImageName write SetImageName;
    property Spacing: Integer read FSpacing write SetSpacing default 3;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterPolyItem(TImageTextItem);
end;

{ TImageTextItem }

procedure TImageTextItem.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  inherited;
  if Sender is TCustomAction then
  with TCustomAction(Sender) do
  begin
    if not CheckDefaults or (Self.ImageIndex = -1) then
      Self.ImageIndex := ImageIndex;
  end;
end;

function TImageTextItem.ActionIsImageIndexLinked: Boolean;
begin
  Result := ImageIndex = (Action as TCustomAction).ImageIndex;
end;

procedure TImageTextItem.ActionSetImageIndex(Value: Integer);
begin
  ImageIndex := Value;
end;

procedure TImageTextItem.Assign(Source: TPersistent);
begin
  inherited;
  if (Source is TImageTextItem) then
  begin
    FImage.Assign((Source as TImageTextItem).Image);
    FImageHeight := (Source as TImageTextItem).ImageHeight;
    FImageWidth := (Source as TImageTextItem).ImageWidth;
    FImageVisible := (Source as TImageTextItem).ImageVisible;
    FSpacing := (Source as TImageTextItem).Spacing;
    FImageName := (Source as TImageTextItem).ImageName;
    FImageIndex := (Source as TImageTextItem).ImageIndex;
    Changed;
  end;
end;

constructor TImageTextItem.Create(AOwner: TComponent);
var
  FDesignTime: boolean;
begin
  inherited Create(AOwner);
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

function TImageTextItem.CreateNewItem(AOwner: TComponent): TCustomItem;
begin
  Result := TImageTextItem.Create(AOwner);
end;

class function TImageTextItem.CustomClassName: String;
begin
  Result := 'Normal Text and Image Item';
end;

destructor TImageTextItem.Destroy;
begin
  FImage.Free;
  inherited;
end;

procedure TImageTextItem.DrawInRect(g: TGPGraphics;
  ItemAppearance: TItemAppearance; R: TGPRectF);
begin
  if Visible then
  begin
    DoItemStartDraw(Self, g, Self, r);
    DoInternalItemStartDraw(Self, g, Self, r);

    inherited;
    DrawImage(g, r, ItemAppearance);

    DoItemEndDraw(Self, g, Self, r);
    DoInternalItemEndDraw(Self, g, Self, r);
  end;
end;

procedure TImageTextItem.DrawImage(g: TGPGraphics;
  ARect: TGPRectF; ItemAppearance: TItemAppearance);
var
  r: TGPRectF;
  imgr: TGPRectF;
begin
  if ImageVisible then
  begin
    r := ARect;
    imgr := MakeRect(r.X + Spacing, r.Y + (r.Height - ImageHeight) / 2, ImageWidth, ImageHeight);
    if Assigned(Image) and not Image.Empty then
    begin
      Image.GDIPDraw(g, imgr);
    end;
    DrawImageFromImageList(g, imgr, ItemAppearance, ImageIndex);
    DrawImageFromContainer(g, imgr, ItemAppearance, ImageName);
  end;
end;

function TImageTextItem.DrawImageFromContainer(g: TGPGraphics; ARect: TGPRectF;
  ItemAppearance: TItemAppearance; Image: String): Boolean;
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

function TImageTextItem.DrawImageFromImageList(g: TGPGraphics; ARect: TGPRectF;
  ItemAppearance: TItemAppearance; Image: Integer): Boolean;
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

function TImageTextItem.DrawText(g: TGPGraphics;
  ARect: TGPRectF; ItemAppearance: TItemAppearance; DoText: Boolean = true): TGPRectF;
var
  r: TGPRectF;
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
        f := TGPFont.Create(ff, Round(CaptionSize * DPIScale), fs , UnitPoint)
      else
        f := TGPFont.Create(ff, Round(ft.Size * DPIScale), fs , UnitPoint);

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
        g.DrawString(Caption, txtlt, f, MakeRect(textx, texty, textr.Width - (textx - textr.X), textr.Height - (texty - textr.Y)), sf, b);
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
        f := TGPFont.Create(ff, Round(ft.Size * DPIScale), fs, UnitPoint);

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

function TImageTextItem.GetClassType: TComponentClass;
begin
  Result := TImageTextItem;
end;

function TImageTextItem.GetItemInteraction(pX,
  pY: integer): TItemInteractionType;
begin
  result := inherited GetItemInteraction(pX, pY);
end;

function TImageTextItem.GetVersionNr: integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TImageTextItem.ImageChanged(Sender: TObject);
begin
  Changed;
end;

procedure TImageTextItem.InitDesignTime;
begin
  inherited;
  LoadFromRes('placeholder', FImage);
end;

function TImageTextItem.IsImageVisible(ItemAppearance: TItemAppearance): Boolean;
begin
  Result := (not Image.Empty);
  if Assigned(ItemAppearance.ImageList) then
    Result := Result or ((ImageIndex >=0) and (ImageIndex <= ItemAppearance.ImageList.Count - 1));

  if Assigned(ItemAppearance.PictureContainer) then
    Result := Result or (Assigned(ItemAppearance.PictureContainer.FindPicture(ImageName)) and (ImageName <> ''));

  Result := Result and ImageVisible;
end;

procedure TImageTextItem.SetImage(const Value: TAdvGDIPPicture);
begin
  if FImage <> value then
  begin
    FImage.Assign(Value);
    Changed;
  end;
end;

procedure TImageTextItem.SetImageHeight(const Value: integer);
begin
  if FImageHeight <> Value then
  begin
    FImageHeight := Value;
    Changed;
  end;
end;

procedure TImageTextItem.SetImageIndex(const Value: TImageIndex);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;

    if (csDesigning in ComponentState) and (FImageIndex >= 0) then
    begin
      if not FImage.Empty then
        FImage.Assign(nil);

      if (FImageName <> '') then
        FImageName := '';
    end;

    Changed;
  end;
end;

procedure TImageTextItem.SetImageName(const Value: String);
begin
  if FImageName <> Value then
  begin
    FImageName := Value;

    if (csDesigning in ComponentState) and (FImageName <> '') then
    begin
      if not FImage.Empty then
        FImage.Assign(nil);
      if FImageIndex >= 0 then
        FImageIndex := -1;
    end;

    Changed;
  end;
end;

procedure TImageTextItem.SetImageVisible(const Value: Boolean);
begin
  if FImageVisible <> Value then
  begin
    FImageVisible := Value;
    Changed;
  end;
end;

procedure TImageTextItem.SetImageWidth(const Value: integer);
begin
  if FImageWidth <> Value then
  begin
    FImageWidth := Value;
    Changed;
  end;
end;

procedure TImageTextItem.SetSpacing(const Value: Integer);
begin
  if FSpacing <> Value then
  begin
    FSpacing := Value;
    Changed;
  end;
end;

end.
