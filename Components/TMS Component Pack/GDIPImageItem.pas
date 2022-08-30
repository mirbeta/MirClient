{*************************************************************************}
{ TImageItem Class                                                        }
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

unit GDIPImageItem;

interface

{$I TMSDEFS.INC}

uses
  Classes, AdvGDIP, Controls, Windows, Graphics,
  GDIPCustomItem, GDIPImageTextItem
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 1; // Build nr.

  //v0.9.0.0 : First Beta Release
  //v1.0.0.0 : First Release
  //v1.0.0.1 : Fixed : Issue with Caption / Description floating point error

type
  TImageItemLayout = (ilLeftRight, ilRightLeft, ilTopBottom, ilBottomTop);

  TImageItemMode = (imStretch, imNormal);

  TImageItem = class(TImageTextItem)
  private
    FLayout: TImageItemLayout;
    FImageMargin: TMargins;
    FAspectRatio: Boolean;
    FImageMode: TImageItemMode;
    procedure SetLayout(const Value: TImageItemLayout);
    procedure SetImageMargin(const Value: TMargins);
    procedure SetAspectRatio(const Value: Boolean);
    procedure SetImageMode(const Value: TImageItemMode);
  protected
    function GetVersionNr: integer; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function DrawText(g: TGPGraphics; ARect: TGPRectF; ItemAppearance: TItemAppearance; DoText: Boolean = true): TGPRectF; override;
    procedure DrawImage(g: TGPGraphics; ARect: TGPRectF; ItemAppearance: TItemAppearance); override;
    //must override
    function CreateNewItem(AOwner: TComponent): TCustomItem; override;
    function GetClassType: TComponentClass; override;
    class function CustomClassName: String; override;
    procedure GetAspectSize(var w, h: Double; ow, oh, nw, nh: double);
    procedure InitDesignTime; override;
  published
    property ImageTextLayout: TImageItemLayout read FLayout write SetLayout default ilTopBottom;
    property ImageMargin: TMargins read FImageMargin write SetImageMargin;
    property AspectRatio: Boolean read FAspectRatio write SetAspectRatio default true; 
    property ImageMode: TImageItemMode read FImageMode write SetImageMode default imNormal;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterPolyItem(TImageItem);
end;

{ TImageItem }

constructor TImageItem.Create(AOwner: TComponent);
begin
  inherited;
  FLayout := ilTopBottom;
  FImageMargin := TMargins.Create(nil);
  FImageMode := imNormal;
  FAspectRatio := true;
  if (csDesigning in ComponentState) and not (csLoading in ComponentState) then
  begin
    Height := 80;
    ImageHeight := 50;
    ImageWidth := 50;
    CaptionLocation := tlCenterCenter;
    DescriptionLocation := tlTopCenter;
  end;
end;

function TImageItem.CreateNewItem(AOwner: TComponent): TCustomItem;
begin
  Result := TImageItem.Create(AOwner);
end;

class function TImageItem.CustomClassName: String;
begin
  Result := 'Normal Image Item';
end;

destructor TImageItem.Destroy;
begin
  FImageMargin.Free;
  inherited;
end;

procedure TImageItem.DrawImage(g: TGPGraphics; ARect: TGPRectF;
  ItemAppearance: TItemAppearance);
var
  imgr, r: TGPRectF;
  w, h: Double;
begin
  if ImageVisible then
  begin
    case ImageTextLayout of
      ilLeftRight: imgr := MakeRect(ARect.X + ImageMargin.Left, ARect.Y + (ARect.Height - ImageHeight) / 2, ImageWidth, ImageHeight);
      ilRightLeft: imgr := MakeRect(ARect.X + ARect.Width - ImageMargin.Left - ImageWidth, ARect.Y + (ARect.Height - ImageHeight) / 2, ImageWidth, ImageHeight);
      ilTopBottom: imgr := MakeRect(ARect.X + (ARect.Width - ImageWidth) / 2, ARect.Y + ImageMargin.Top, ImageWidth, ImageHeight);
      ilBottomTop: imgr := MakeRect(ARect.X + (ARect.Width - ImageWidth) / 2, ARect.Y + ARect.Height - ImageMargin.Bottom - ImageHeight , ImageWidth, ImageHeight);
    end;        

    if Assigned(Image) and not Image.Empty then
    begin
      Image.GetImageSizes;
      GetAspectSize(w, h, Image.Width, Image.Height, imgr.Width, imgr.Height);
      r := MakeRect(imgr.X + (imgr.Width - w) / 2, imgr.Y + (imgr.Height - h) / 2, w, h);
      Image.GDIPDraw(g, r);
    end;

    DrawImageFromImageList(g, imgr, ItemAppearance, ImageIndex);
    DrawImageFromContainer(g, imgr, ItemAppearance, ImageName);
  end;
end;

function TImageItem.DrawText(g: TGPGraphics; ARect: TGPRectF;
  ItemAppearance: TItemAppearance; DoText: Boolean = true): TGPRectF;
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
  imgr: TGPRectF;
  fst: TFontStyles;
begin
  r := ARect;
  if IsImageVisible(ItemAppearance) then
  begin
    case ImageTextLayout of
      ilLeftRight:
      begin
        imgr := MakeRect(r.X + ImageMargin.Left, r.Y + (r.Height - ImageHeight) / 2, ImageWidth, ImageHeight);
        r.X := r.X + imgr.Width;
        r.Width := r.Width - imgr.Width;

      end;
      ilRightLeft:
      begin
        imgr := MakeRect(r.X + r.Width - ImageMargin.Left - ImageWidth, r.Y + (r.Height - ImageHeight) / 2, ImageWidth, ImageHeight);
        r.Width := r.Width - imgr.Width;
      end;
      ilTopBottom:
      begin
        imgr := MakeRect(r.X + (r.Width - ImageWidth) / 2, r.Y + ImageMargin.Top , ImageWidth, ImageHeight);
        r.Y := r.Y + imgr.Height;
        r.Height := r.Height - imgr.Height;
      end;
      ilBottomTop:
      begin
        imgr := MakeRect(r.X + (ARect.Width - ImageWidth) / 2, r.Y + r.Height - ImageMargin.Bottom - ImageHeight , ImageWidth, ImageHeight);
        r.Height := r.Height - imgr.Height;
      end;
    end;
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

    sf := TGPStringFormat.Create;
    sf.SetHotkeyPrefix(HotkeyPrefixShow);

    caph := 0;
    textoutr := MakeRect(0, 0, 0, 0);
    textx := 0;
    texty := 0;
    if Caption <> '' then
    begin
      if CaptionSize <> -1 then
        f := TGPFont.Create(ff, Round(CaptionSize * DPIScale), fs, UnitPoint)
      else
        f := TGPFont.Create(ff, Round(ft.Size * DPIScale) , fs, UnitPoint);

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

procedure TImageItem.GetAspectSize(var w, h: Double; ow, oh, nw, nh: double);
begin
  if AspectRatio then
  begin
    if (ow > 0) and (oh > 0) and (nw > 0) and (nh > 0) then
    begin
      if (ow < nw) and (oh < nh) and (ImageMode = imNormal) then
      begin
        w := ow;
        h := oh;
      end
      else
      begin
        if ow / oh < nw / nh then
        begin
          h := nh;
          w := nh * ow / oh;
        end
        else
        begin
          w := nw;
          h := nw * oh / ow;
        end;
      end;
    end
    else
    begin
      w := 0;
      h := 0;
    end;
  end
  else
  begin
    w := nw;
    h := nh;
  end;
end;

function TImageItem.GetClassType: TComponentClass;
begin
  Result := TImageItem;
end;

function TImageItem.GetVersionNr: integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TImageItem.InitDesignTime;
begin
  inherited;
  Height := 80;
  ImageHeight := 50;
  ImageWidth := 50;
  CaptionLocation := tlCenterCenter;
  DescriptionLocation := tlTopCenter;
end;

procedure TImageItem.SetAspectRatio(const Value: Boolean);
begin
  if FAspectRatio <> Value then
  begin
    FAspectRatio := Value;
    Changed;
  end;
end;

procedure TImageItem.SetImageMargin(const Value: TMargins);
begin
  if FImageMargin <> Value then
  begin
    FImageMargin.Assign(Value);
    Changed;
  end;
end;

procedure TImageItem.SetImageMode(const Value: TImageItemMode);
begin
  if FImageMode <> Value then
  begin
    FImageMode := Value;
    Changed;
  end;
end;

procedure TImageItem.SetLayout(const Value: TImageItemLayout);
begin
  if FLayout <> Value then
  begin
    FLayout := Value;
    Changed;
  end;
end;

end.
