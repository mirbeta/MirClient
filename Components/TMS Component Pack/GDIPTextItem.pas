{*************************************************************************}
{ TTextItem Class                                                         }
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

unit GDIPTextItem;

interface

{$I TMSDEFS.INC}

uses
  Windows, Classes, Graphics, Controls, AdvGDIP, GDIPCustomItem,
  GDIPFill, AdvStyleIF
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.

  //v1.0.0.0 : First Release

type
  TTextItem = class(TCustomItem, ITMSStyle, ITMSTones)
  private
    FCaption: String;
    FCaptionMargin: TMargins;
    FCaptionColor: TColor;
    FCaptionTop: Integer;
    FCaptionLeft: Integer;
    FCaptionLocation: TItemLocation;
    FWordWrap: Boolean;
    FEllipsis: Boolean;
    FCaptionOpacity: Byte;
    FCaptionSize: Integer;
    FDescriptionSize: Integer;
    FDescriptionMargin: TMargins;
    FDescriptionOpacity: Byte;
    FDescription: String;
    FDescriptionColor: TColor;
    FDescriptionTop: Integer;
    FDescriptionLeft: Integer;
    FDescriptionLocation: TItemLocation;
    FCaptionFontStyle: TFontStyles;
    FDescriptionFontStyle: TFontStyles;
    procedure SetCaption(const Value: String);
    procedure SetCaptionMargin(const Value: TMargins);
    procedure SetCaptionColor(const Value: TColor);
    procedure SetCaptionLeft(const Value: Integer);
    procedure SetCaptionTop(const Value: Integer);
    procedure SetCaptionLocation(const Value: TItemLocation);
    procedure SetWordWrap(const Value: Boolean);
    procedure SetEllipsis(const Value: Boolean);
    procedure SetCaptionOpacity(const Value: Byte);
    procedure SetCaptionSize(const Value: Integer);
    procedure SetDescription(const Value: String);
    procedure SetDescriptionColor(const Value: TColor);
    procedure SetDescriptionMargin(const Value: TMargins);
    procedure SetDescriptionOpacity(const Value: Byte);
    procedure SetDescriptionSize(const Value: Integer);
    procedure SetDescriptionLeft(const Value: Integer);
    procedure SetDescriptionLocation(const Value: TItemLocation);
    procedure SetDescriptionTop(const Value: Integer);
    procedure SetCaptionFontStyle(const Value: TFontStyles);
    procedure SetDescriptionFontStyle(const Value: TFontStyles);
  protected
    procedure SetComponentStyle(AStyle: TTMSStyle);
    procedure SetColorTones(ATones: TColorTones);
    procedure CaptionMarginChanged(Sender: TObject);
    function DrawText(g: TGPGraphics; ARect: TGPRectF; ItemAppearance: TItemAppearance; DoText: Boolean = true): TGPRectF; virtual;
    function GetVersionNr: integer; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function CreateNewItem(AOwner: TComponent): TCustomItem; override;
    function GetClassType: TComponentClass; override;
    class function CustomClassName: String; override;
    function GetItemInteraction(pX, pY: integer): TItemInteractionType; override;
    procedure DrawInRect(g: TGPGraphics; ItemAppearance: TItemAppearance; R: TGPRectF); override;
    procedure UseCaption(ACaption: String); override;
  published
    property Caption: String read FCaption write SetCaption;
    property CaptionMargin: TMargins read FCaptionMargin write SetCaptionMargin;
    property CaptionColor: TColor read FCaptionColor write SetCaptionColor default clNone;
    property CaptionOpacity: Byte read FCaptionOpacity write SetCaptionOpacity default 255;
    property CaptionLeft: Integer read FCaptionLeft write SetCaptionLeft default 0;
    property CaptionTop: Integer read FCaptionTop write SetCaptionTop default 0;
    property CaptionLocation: TItemLocation read FCaptionLocation write SetCaptionLocation default tlCenterLeft;
    property CaptionSize: Integer read FCaptionSize write SetCaptionSize default -1;
    property CaptionFontStyle: TFontStyles read FCaptionFontStyle write SetCaptionFontStyle default [];
    property Description: String read FDescription write SetDescription;
    property DescriptionFontStyle: TFontStyles read FDescriptionFontStyle write SetDescriptionFontStyle default [];
    property DescriptionMargin: TMargins read FDescriptionMargin write SetDescriptionMargin;
    property DescriptionColor: TColor read FDescriptionColor write SetDescriptionColor default clGray;
    property DescriptionOpacity: Byte read FDescriptionOpacity write SetDescriptionOpacity default 255;
    property DescriptionSize: Integer read FDescriptionSize write SetDescriptionSize default -1;
    property DescriptionLocation: TItemLocation read FDescriptionLocation write SetDescriptionLocation default tlTopLeft;
    property DescriptionLeft: Integer read FDescriptionLeft write SetDescriptionLeft default 0;
    property DescriptionTop: Integer read FDescriptionTop write SetDescriptionTop default 0;
    property WordWrap: Boolean read FWordWrap write SetWordWrap default true;
    property Ellipsis: Boolean read FEllipsis write SetEllipsis default false;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterPolyItem(TTextItem);
end;

{ TTextItem }

procedure TTextItem.Assign(Source: TPersistent);
begin
  inherited;
  if (Source is TTextItem) then
  begin
    FCaption := (Source as TTextItem).Caption;
    FCaptionMargin.Assign((Source as TTextItem).CaptionMargin);
    FCaptionColor := (Source as TTextItem).CaptionColor;
    FCaptionOpacity := (Source as TTextItem).CaptionOpacity;
    FCaptionLeft := (Source as TTextItem).CaptionLeft;
    FCaptionTop := (Source as TTextItem).CaptionTop;
    FCaptionLocation := (Source as TTextItem).CaptionLocation;
    FCaptionSize := (Source as TTextItem).CaptionSize;
    FDescription := (Source as TTextItem).Description;
    FDescriptionMargin.Assign((Source as TTextItem).DescriptionMargin);
    FDescriptionColor := (Source as TTextItem).DescriptionColor;
    FDescriptionOpacity := (Source as TTextItem).DescriptionOpacity;
    FDescriptionLeft := (Source as TTextItem).DescriptionLeft;
    FDescriptionTop := (Source as TTextItem).DescriptionTop;
    FDescriptionLocation := (Source as TTextItem).DescriptionLocation;
    FDescriptionSize := (Source as TTextItem).DescriptionSize;
    FWordWrap := (Source as TTextItem).WordWrap;
    FEllipsis := (Source as TTextItem).Ellipsis;
    Changed;
  end;
end;

procedure TTextItem.CaptionMarginChanged(Sender: TObject);
begin
  Changed;
end;

constructor TTextItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCaptionSize := -1;
  FCaptionColor := clNone;
  FCaptionMargin := TMargins.Create(nil);
  FCaptionMargin.OnChange := CaptionMarginChanged;
  FCaptionLeft := 0;
  FCaptionTop := 0;
  FCaptionLocation := tlCenterLeft;
  FCaptionOpacity := 255;

  FDescription := '';
  FDescriptionColor := clGray;
  FDescriptionMargin := TMargins.Create(nil);
  FDescriptionMargin.Top := 0;
  FDescriptionMargin.OnChange := CaptionMarginChanged;
  FDescriptionLeft := 0;
  FDescriptionTop := 0;
  FDescriptionLocation := tlTopLeft;
  FDescriptionOpacity := 255;
  FDescriptionSize := -1;
  FWordWrap := true;
  FEllipsis := false;
end;

function TTextItem.CreateNewItem(AOwner: TComponent): TCustomItem;
begin
  Result := TTextItem.Create(AOwner);
end;

class function TTextItem.CustomClassName: String;
begin
  Result := 'Normal Text Item';
end;

destructor TTextItem.Destroy;
begin
  FCaptionMargin.Free;
  FDescriptionMargin.Free;
  inherited;
end;

procedure TTextItem.DrawInRect(g: TGPGraphics; ItemAppearance: TItemAppearance;
  R: TGPRectF);
var
  ir: TGPRectF;
  f: TGDIPFill;
begin
  if Visible then
  begin
    DoItemStartDraw(Self, g, Self, r);
    DoInternalItemStartDraw(Self, g, Self, r);

    f := GetFill(ItemAppearance);
    if Assigned(f) then
      f.Fill(g, r);

    if ItemAppearance.Focus and (ItemAppearance.FocusedItem = Index) then
      DrawFocus(g, r, ItemAppearance);

    ir := DrawText(g, r, ItemAppearance);
    case Status.Position of
      spItemRectangle:  DrawStatus(g, r, ItemAppearance);
      spItemText: DrawStatus(g, ir, ItemAppearance);
    end;

    DoItemEndDraw(Self, g, Self, r);
    DoInternalItemEndDraw(Self, g, Self, r);
  end;
end;

function TTextItem.DrawText(g: TGPGraphics;
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
        f := TGPFont.Create(ff, Round(ft.Size * DPIScale), fs, UnitPoint);

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
        g.SetTextRenderingHint(TextRenderingHintClearTypeGridFit);
        g.DrawString(Description, txtlt, f, MakeRect(textx, texty, textr.Width - (textx - textr.X), textr.Height - (texty - textr.Y)), sf, b);
      end;

      b.Free;
      f.Free;
    end;
    sf.Free;
    ff.Free;
  end;
end;

function TTextItem.GetClassType: TComponentClass;
begin
  Result := TTextItem;
end;

function TTextItem.GetItemInteraction(pX,
  pY: integer): TItemInteractionType;
begin
  Result := inherited GetItemInteraction(pX, pY);
end;

function TTextItem.GetVersionNr: integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TTextItem.SetCaption(const Value: String);
begin
  if FCaption <> value then
  begin
    FCaption := Value;
    Changed;
  end;
end;

procedure TTextItem.SetCaptionColor(const Value: TColor);
begin
  if FCaptionColor <> value then
  begin
    FCaptionColor := Value;
    Changed;
  end;
end;

procedure TTextItem.SetCaptionFontStyle(const Value: TFontStyles);
begin
  if FCaptionFontStyle <> Value then
  begin
    FCaptionFontStyle := Value;
    Changed;
  end;
end;

procedure TTextItem.SetCaptionLeft(const Value: Integer);
begin
  if FCaptionLeft <> value then
  begin
    FCaptionLeft := Value;
    Changed;
  end;
end;

procedure TTextItem.SetCaptionLocation(const Value: TItemLocation);
begin
  if FCaptionLocation <> value then
  begin
    FCaptionLocation := Value;
    Changed;
  end;
end;

procedure TTextItem.SetCaptionMargin(const Value: TMargins);
begin
  if FCaptionMargin <> value then
  begin
    FCaptionMargin.Assign(Value);
    Changed;
  end;
end;

procedure TTextItem.SetCaptionOpacity(const Value: Byte);
begin
  if FCaptionOpacity <> Value then
  begin
    FCaptionOpacity := Value;
    Changed;
  end;
end;

procedure TTextItem.SetCaptionSize(const Value: Integer);
begin
  if FCaptionSize <> value then
  begin
    FCaptionSize := Value;
    Changed;
  end;
end;

procedure TTextItem.SetCaptionTop(const Value: Integer);
begin
  if FCaptionTop <> value then
  begin
    FCaptionTop := Value;
    Changed;
  end;
end;

procedure TTextItem.SetColorTones(ATones: TColorTones);
begin
  CaptionColor := clNone;
  DescriptionColor := clNone;
end;

procedure TTextItem.SetComponentStyle(AStyle: TTMSStyle);
begin
  CaptionColor := clNone;
  DescriptionColor := clGray;
end;

procedure TTextItem.SetDescription(const Value: String);
begin
  if FDescription <> Value then
  begin
    FDescription := Value;
    Changed;
  end;
end;

procedure TTextItem.SetDescriptionColor(const Value: TColor);
begin
  if FDescriptionColor <> Value then
  begin
    FDescriptionColor := Value;
    Changed;
  end;
end;

procedure TTextItem.SetDescriptionFontStyle(const Value: TFontStyles);
begin
  if FDescriptionFontStyle <> Value then
  begin
    FDescriptionFontStyle := Value;
    Changed;
  end;
end;

procedure TTextItem.SetDescriptionLeft(const Value: Integer);
begin
  if FDescriptionLeft <> Value then
  begin
    FDescriptionLeft := Value;
    Changed;
  end;
end;

procedure TTextItem.SetDescriptionLocation(const Value: TItemLocation);
begin
  if FDescriptionLocation <> Value then
  begin
    FDescriptionLocation := Value;
    Changed;
  end;
end;

procedure TTextItem.SetDescriptionMargin(const Value: TMargins);
begin
  if FDescriptionMargin <> Value then
  begin
    FDescriptionMargin.Assign(Value);
    Changed;
  end;
end;

procedure TTextItem.SetDescriptionOpacity(const Value: Byte);
begin
  if FDescriptionOpacity <> Value then
  begin
    FDescriptionOpacity := Value;
    Changed;
  end;
end;

procedure TTextItem.SetDescriptionSize(const Value: Integer);
begin
  if FDescriptionSize <> Value then
  begin
    FDescriptionSize := Value;
    Changed;
  end;
end;

procedure TTextItem.SetDescriptionTop(const Value: Integer);
begin
  if FDescriptionTop <> Value then
  begin
    FDescriptionTop := Value;
    Changed;
  end;
end;

procedure TTextItem.SetEllipsis(const Value: Boolean);
begin
  if FEllipsis <> value then
  begin
    FEllipsis := Value;
    Changed;
  end;
end;

procedure TTextItem.SetWordWrap(const Value: Boolean);
begin
  if FWordWrap <> value then
  begin
    FWordWrap := Value;
    Changed;
  end;
end;

procedure TTextItem.UseCaption(ACaption: String);
begin
  Caption := ACaption;
end;

end.
