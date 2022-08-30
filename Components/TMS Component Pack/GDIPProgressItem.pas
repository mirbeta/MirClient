{*************************************************************************}
{ TProgressItem Base Class                                                }
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

unit GDIPProgressItem;

interface

uses
  Classes, GDIPCustomItem, AdvGDIP, GDIPFill, AdvStyleIF;

type
  TProgressItem = class(TCustomItem, ITMSStyle)
  private
    FAppearance: TGDIPProgress;
    FMax: integer;
    FMin: integer;
    FPosition: integer;
    FValueFormat: string;
    FShowValue: boolean;
    FMargin: integer;
    procedure SetAppearance(const Value: TGDIPProgress);
    procedure SetMax(const Value: integer);
    procedure SetMin(const Value: integer);
    procedure SetPosition(const Value: integer);
    procedure SetShowValue(const Value: boolean);
    procedure SetValueFormat(const Value: string);
    procedure SetMargin(const Value: integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function CreateNewItem(AOwner: TComponent): TCustomItem; override;
    function GetClassType: TComponentClass; override;
    class function CustomClassName: String; override;
    procedure DrawInRect(g: TGPGraphics; ItemAppearance: TItemAppearance; R: TGPRectF); override;
    procedure SetComponentStyle(AStyle: TTMSStyle);
    procedure SetColorTones(ATones: TColorTones);
    procedure SetStyle(AStyle: TTMSStyle; ASelected: Boolean);
  published
    property Appearance: TGDIPProgress read FAppearance write SetAppearance;
    property Min: integer read FMin write SetMin default 0;
    property Max: integer read FMax write SetMax default 100;
    property Margin: integer read FMargin write SetMargin default 2;
    property Position: integer read FPosition write SetPosition default 0;
    property ShowValue: boolean read FShowValue write SetShowValue default true;
    property ValueFormat: string read FValueFormat write SetValueFormat;
  end;


procedure Register;

implementation

uses
  SysUtils, Graphics, Windows;

{ TProgressItem }

procedure TProgressItem.Assign(Source: TPersistent);
begin
  inherited;
  if (Source is TProgressItem) then
  begin
    FAppearance.Assign((Source as TProgressItem).Appearance);
    FMin := (Source as TProgressItem).Min;
    FMax := (Source as TProgressItem).Max;
    FMargin := (Source as TProgressItem).Margin;
    FPosition := (Source as TProgressItem).Position;
    FShowValue := (Source as TProgressItem).ShowValue;
    FValueFormat := (Source as TProgressItem).ValueFormat;
  end;
end;

constructor TProgressItem.Create(AOwner: TComponent);
begin
  inherited;
  FShowValue := true;
  FValueFormat := '%d%%';
  FMargin := 2;
  Height := 40;
  FMax := 100;

  FAppearance := TGDIPProgress.Create;

  with FAppearance do
  begin
    BackGroundFill.Color := $00FFD2AF;
    BackGroundFill.ColorTo := $00FFD2AF;
    BackGroundFill.BorderColor := $00C0C0C0;

    ProgressFill.Color := $00EBFDFF;
    ProgressFill.ColorTo := $00ABEBFF;
    ProgressFill.ColorMirror := $0069D6FF;
    ProgressFill.ColorMirrorTo := $0096E4FF;
    ProgressFill.BorderColor := clSilver;

    ProgressFill.GradientMirrorType := gtVertical;
  end;
end;

function TProgressItem.CreateNewItem(AOwner: TComponent): TCustomItem;
begin
  Result := TProgressItem.Create(AOwner);
end;

class function TProgressItem.CustomClassName: String;
begin
  Result := 'Progressbar item';
end;

destructor TProgressItem.Destroy;
begin
  FAppearance.Free;
  inherited;
end;

procedure TProgressItem.DrawInRect(g: TGPGraphics;
  ItemAppearance: TItemAppearance; R: TGPRectF);
var
  f: TGDIPFill;
  dr,tr: TRect;
  valstr: string;
  h,tw,th: integer;
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

    dr := Rect(Trunc(r.X), Trunc(r.Y), Trunc(R.X + R.Width), Trunc(R.Y + R.Height));
    InflateRect(dr, -Margin, - Margin);
    h := dr.Bottom - dr.Top;

    if ShowValue then
      h := h div 2;

    tr := Rect(dr.Left, dr.Top, dr.Right, dr.Top + h);

    // draw progressbar
    FAppearance.Draw(g, tr ,FMin,FMax,FPosition, pbdHorizontal);

    if ShowValue then
    begin
      tr := Rect(dr.Left, dr.Top + h, dr.Right, dr.Top + 2*h);
      valstr := Format(FValueFormat, [FPosition]);
      // measure text value size
      th := g.DrawText(valstr, length(valstr), tr, GetFont(ItemAppearance), DT_CALCRECT);
      tw := tr.Right - tr.Left;
      // center text
      tr := Rect(dr.Left + (dr.Right - dr.Left - tw) div 2, dr.Top + (dr.Bottom - dr.Top - th + h) div 2, dr.Right, dr.Bottom);
      // draw text
      g.DrawText(valstr, length(valstr), tr, GetFont(ItemAppearance), 0);
    end;

    DoItemEndDraw(Self, g, Self, r);
    DoInternalItemEndDraw(Self, g, Self, r);
  end;
end;

function TProgressItem.GetClassType: TComponentClass;
begin
  Result := TProgressItem;
end;

procedure TProgressItem.SetAppearance(const Value: TGDIPProgress);
begin
  FAppearance.Assign(Value);
end;

procedure TProgressItem.SetColorTones(ATones: TColorTones);
begin
  Appearance.Overlays := False;
  Appearance.Shadows := False;
  Appearance.BackGroundFill.Color := ATones.Foreground.BrushColor;
  Appearance.BackGroundFill.ColorTo := ATones.Foreground.BrushColor;
  Appearance.BackGroundFill.ColorMirror := ATones.Foreground.BrushColor;
  Appearance.BackGroundFill.ColorMirrorTo := ATones.Foreground.BrushColor;
  Appearance.BackGroundFill.BorderColor := ATones.Foreground.BorderColor;

  Appearance.ProgressFill.Color := ATones.Selected.BrushColor;
  Appearance.ProgressFill.ColorTo := ATones.Selected.BrushColor;
  Appearance.ProgressFill.ColorMirror := ATones.Selected.BrushColor;
  Appearance.ProgressFill.ColorMirrorTo := ATones.Selected.BrushColor;
  Appearance.ProgressFill.BorderColor := ATones.Selected.BorderColor;
  Appearance.Font.Color := ATones.Selected.TextColor;
end;

procedure TProgressItem.SetComponentStyle(AStyle: TTMSStyle);
begin
  Appearance.Overlays := True;
  Appearance.Shadows := True;
  SetStyle(AStyle, false);
end;

procedure TProgressItem.SetMargin(const Value: integer);
begin
  if (FMargin <> Value) then
  begin
    FMargin := Value;
    Changed;
  end;
end;

procedure TProgressItem.SetMax(const Value: integer);
begin
  if (FMax <> Value) then
  begin
    FMax := Value;
    Changed;
  end;
end;

procedure TProgressItem.SetMin(const Value: integer);
begin
  if (FMin <> Value) then
  begin
    FMin := Value;
    Changed;
  end;
end;

procedure TProgressItem.SetPosition(const Value: integer);
begin
  if (FPosition <> Value) then
  begin
    FPosition := Value;
    Changed;
  end;
end;

procedure TProgressItem.SetShowValue(const Value: boolean);
begin
  if FShowValue <> Value then
  begin
    FShowValue := Value;
    Changed;
  end;
end;

procedure TProgressItem.SetStyle(AStyle: TTMSStyle; ASelected: Boolean);
begin
  with Appearance do
  begin
    case AStyle of
      tsOffice2003Blue:
      begin
        BackGroundFill.Color := $00FFD2AF;
        BackGroundFill.ColorTo := $00FFD2AF;
        BackGroundFill.BorderColor := $00C0C0C0;

        if not ASelected then
        begin
          ProgressFill.Color := $FCE1CB;
          ProgressFill.ColorTo := $E0A57D;
          ProgressFill.ColorMirror := clNone;
          ProgressFill.ColorMirrorTo := clNone;
          ProgressFill.BorderColor := $962D00;
          ProgressFill.GradientMirrorType := gtVertical;
        end
        else
        begin
          ProgressFill.Color := $94E6FB;
          ProgressFill.ColorTo := $1595EE;
          ProgressFill.ColorMirror := clNone;
          ProgressFill.ColorMirrorTo := clNone;
          ProgressFill.BorderColor := $962D00;
          ProgressFill.GradientMirrorType := gtVertical;
        end;
      end;
      tsOffice2003Silver:
      begin
        BackGroundFill.Color := $00E6D8D8;
        BackGroundFill.ColorTo := $00E6D8D8;
        BackGroundFill.BorderColor := $00C0C0C0;

        if not ASelected then
        begin
          ProgressFill.Color := $ECE2E1;
          ProgressFill.ColorTo := $B39698;
          ProgressFill.ColorMirror := clNone;
          ProgressFill.ColorMirrorTo := clNone;
          ProgressFill.BorderColor := $947C7C;
          ProgressFill.GradientMirrorType := gtVertical;
        end
        else
        begin
          ProgressFill.Color := $94E6FB;
          ProgressFill.ColorTo := $1595EE;
          ProgressFill.ColorMirror := clNone;
          ProgressFill.ColorMirrorTo := clNone;
          ProgressFill.BorderColor := $947C7C;
          ProgressFill.GradientMirrorType := gtVertical;
        end;
      end;
      tsOffice2003Olive:
      begin
        BackGroundFill.Color := $CFF0EA;
        BackGroundFill.ColorTo := $CFF0EA;
        BackGroundFill.BorderColor := $00C0C0C0;

        if not ASelected then
        begin
          ProgressFill.Color := $CFF0EA;
          ProgressFill.ColorTo := $8CC0B1;
          ProgressFill.ColorMirror := clNone;
          ProgressFill.ColorMirrorTo := clNone;
          ProgressFill.BorderColor := $588060;
          ProgressFill.GradientMirrorType := gtVertical;
        end
        else
        begin
          ProgressFill.Color := $94E6FB;
          ProgressFill.ColorTo := $1595EE;
          ProgressFill.ColorMirror := clNone;
          ProgressFill.ColorMirrorTo := clNone;
          ProgressFill.BorderColor := $588060;
          ProgressFill.GradientMirrorType := gtVertical;
        end;
      end;
      tsOffice2003Classic:
      begin
        BackGroundFill.Color := $00F2F2F2;
        BackGroundFill.ColorTo := $00F2F2F2;
        BackGroundFill.BorderColor := $00C0C0C0;

        if not ASelected then
        begin
          ProgressFill.Color := clWhite;
          ProgressFill.ColorTo := $C9D1D5;
          ProgressFill.ColorMirror := clNone;
          ProgressFill.ColorMirrorTo := clNone;
          ProgressFill.BorderColor := $808080;
          ProgressFill.GradientMirrorType := gtVertical;
        end
        else
        begin
          ProgressFill.Color := $B59285;
          ProgressFill.ColorTo := $B59285;
          ProgressFill.ColorMirror := clNone;
          ProgressFill.ColorMirrorTo := clNone;
          ProgressFill.BorderColor := $962D00;
          ProgressFill.GradientMirrorType := gtVertical;
        end;
      end;
      tsOffice2007Luna:
      begin
        BackGroundFill.Color := $00FFD2AF;
        BackGroundFill.ColorTo := $00FFD2AF;
        BackGroundFill.BorderColor := $00C0C0C0;

        if not ASelected then
        begin
          ProgressFill.Color := $FFEFE3;
          ProgressFill.ColorTo := $FFDDC4;
          ProgressFill.ColorMirror := $FFD1AD;
          ProgressFill.ColorMirrorTo := $FFDBC0;
          ProgressFill.BorderColor := $FFD1AD;
          ProgressFill.GradientMirrorType := gtVertical;
        end
        else
        begin
          ProgressFill.Color := $AAD9FF;
          ProgressFill.ColorTo := $6EBBFF;
          ProgressFill.ColorMirror := $42AEFE;
          ProgressFill.ColorMirrorTo := $7AE1FE;
          ProgressFill.BorderColor := $FFD1AD;//$42AEFE;
          ProgressFill.GradientMirrorType := gtVertical;
        end;
      end;
      tsOffice2007Obsidian:
      begin
        BackGroundFill.Color := $5C534C;
        BackGroundFill.ColorTo := $5C534C;
        BackGroundFill.BorderColor := $00C0C0C0;

        if not ASelected then
        begin
          ProgressFill.Color := $F9F8F8;
          ProgressFill.ColorTo := $E4E2DF;
          ProgressFill.ColorMirror := $D1CBC7;
          ProgressFill.ColorMirrorTo := $E2DEDB;
          ProgressFill.BorderColor := clBlack;//$D1CBC7;
          ProgressFill.GradientMirrorType := gtVertical;
        end
        else
        begin
          ProgressFill.Color := $AAD9FF;
          ProgressFill.ColorTo := $6EBBFF;
          ProgressFill.ColorMirror := $42AEFE;
          ProgressFill.ColorMirrorTo := $7AE1FE;
          ProgressFill.BorderColor := clBlack;//$42AEFE;
          ProgressFill.GradientMirrorType := gtVertical;
        end;
      end;
      tsWindowsXP:
      begin
        BackGroundFill.Color := $00B6B6B6;
        BackGroundFill.ColorTo := $00B6B6B6;
        BackGroundFill.BorderColor := $00C0C0C0;

        if not ASelected then
        begin
          ProgressFill.Color := clWhite;
          ProgressFill.ColorTo := clBtnFace;
          ProgressFill.ColorMirror := clNone;
          ProgressFill.ColorMirrorTo := clNone;
          ProgressFill.BorderColor := clBlack;
          ProgressFill.GradientMirrorType := gtVertical;
        end
        else
        begin
          ProgressFill.Color := clInActiveCaption;
          ProgressFill.ColorTo := clInActiveCaption;
          ProgressFill.ColorMirror := clNone;
          ProgressFill.ColorMirrorTo := clNone;
          ProgressFill.BorderColor := clBlack;
          ProgressFill.GradientMirrorType := gtVertical;
        end;
      end;
      tsWhidbey:
      begin
        BackGroundFill.Color := $F5F9FA;
        BackGroundFill.ColorTo := $F5F9FA;
        BackGroundFill.BorderColor := $00C0C0C0;

        if not ASelected then
        begin
          ProgressFill.Color := $F5F9FA;
          ProgressFill.ColorTo := $A8C0C0;
          ProgressFill.ColorMirror := clNone;
          ProgressFill.ColorMirrorTo := clNone;
          ProgressFill.BorderColor := $962D00;
          ProgressFill.GradientMirrorType := gtVertical;
        end
        else
        begin
          ProgressFill.Color := $94E6FB;
          ProgressFill.ColorTo := $1595EE;
          ProgressFill.ColorMirror := clNone;
          ProgressFill.ColorMirrorTo := clNone;
          ProgressFill.BorderColor := $962D00;
          ProgressFill.GradientMirrorType := gtVertical;
        end;
      end;
      tsCustom: ;
      tsOffice2007Silver:
      begin
        BackGroundFill.Color := $00CAC1BA;
        BackGroundFill.ColorTo := $00CAC1BA;
        BackGroundFill.BorderColor := $00C0C0C0;

        if not ASelected then
        begin
          ProgressFill.Color := $FAEEEB;
          ProgressFill.ColorTo := $E5DBD7;
          ProgressFill.ColorMirror := $E2D8D4;
          ProgressFill.ColorMirrorTo := $D1C7C5;
          ProgressFill.BorderColor := clBlack;//$E2D8D4;
          ProgressFill.GradientMirrorType := gtVertical;
        end
        else
        begin
          ProgressFill.Color := $AAD9FF;
          ProgressFill.ColorTo := $6EBBFF;
          ProgressFill.ColorMirror := $42AEFE;
          ProgressFill.ColorMirrorTo := $7AE1FE;
          ProgressFill.BorderColor := clBlack;//$42AEFE;
          ProgressFill.GradientMirrorType := gtVertical;
        end;
      end;
      tsWindowsVista:
      begin
        BackGroundFill.Color := $FDF8F1;
        BackGroundFill.ColorTo := $FDF8F1;
        BackGroundFill.BorderColor := $FDDE99;

        if not ASelected then
        begin
          ProgressFill.Color := $FDF8F1;
          ProgressFill.ColorTo := $FCEFD5;
          ProgressFill.ColorMirror := clNone;
          ProgressFill.ColorMirrorTo := clNone;
          ProgressFill.BorderColor := $FDDE99;
          ProgressFill.GradientMirrorType := gtVertical;
        end
        else
        begin
          ProgressFill.Color := $FEF9F0;
          ProgressFill.ColorTo := $FDF0D7;
          ProgressFill.ColorMirror := clNone;
          ProgressFill.ColorMirrorTo := clNone;
          ProgressFill.BorderColor := $FEDF9A;
          ProgressFill.GradientMirrorType := gtVertical;
        end;
      end;
      tsWindows7:
      begin
        BackGroundFill.Color := $FDF8F1;
        BackGroundFill.ColorTo := $FDF8F1;
        BackGroundFill.BorderColor := $FDDE99;

        if not ASelected then
        begin
          ProgressFill.Color := $FDFBFA;
          ProgressFill.ColorTo := $FDF3EB;
          ProgressFill.ColorMirror := clNone;
          ProgressFill.ColorMirrorTo := clNone;
          ProgressFill.BorderColor := $FBD6B8;
          ProgressFill.GradientMirrorType := gtVertical;
        end
        else
        begin
          ProgressFill.Color := $FCEBDC;
          ProgressFill.ColorTo := $FCDBC1;
          ProgressFill.ColorMirror := clNone;
          ProgressFill.ColorMirrorTo := clNone;
          ProgressFill.BorderColor := $CEA27D;
          ProgressFill.GradientMirrorType := gtVertical;
        end;
      end;
      tsTerminal:
      begin
        BackGroundFill.Color := clBtnFace;
        BackGroundFill.ColorTo := clBtnFace;
        BackGroundFill.BorderColor := clGray;

        if not ASelected then
        begin
          ProgressFill.Color := clSilver;
          ProgressFill.ColorTo := clSilver;
          ProgressFill.ColorMirror := clNone;
          ProgressFill.ColorMirrorTo := clNone;
          ProgressFill.BorderColor := clGray;

        end
        else
        begin
          ProgressFill.Color := clWhite;
          ProgressFill.ColorTo := clWhite;
          ProgressFill.ColorMirror := clNone;
          ProgressFill.ColorMirrorTo := clNone;
          ProgressFill.BorderColor := clGray;

        end;
      end;
      tsOffice2010Blue:
      begin
        BackGroundFill.Color := $FDF6EF;
        BackGroundFill.ColorTo := $F0DAC7;
        BackGroundFill.BorderColor := $C7B29F;

        ProgressFill.Color := $EDDBCD;
        ProgressFill.ColorTo := clNone;
        ProgressFill.ColorMirror := clNone;
        ProgressFill.ColorMirrorTo := clNone;
        ProgressFill.BorderColor := $5B391E;
        ProgressFill.GradientMirrorType := gtVertical;

      end;
      tsOffice2010Silver:
      begin
        BackGroundFill.Color := $FFFFFF;
        BackGroundFill.ColorTo := $EDE5E0;
        BackGroundFill.BorderColor := $D2CDC8;

        ProgressFill.Color := $EDE9E5;
        ProgressFill.ColorTo := clNone;
        ProgressFill.ColorMirror := clNone;
        ProgressFill.ColorMirrorTo := clNone;
        ProgressFill.BorderColor := $7C6D66;
        ProgressFill.GradientMirrorType := gtVertical;

      end;
      tsOffice2010Black:
      begin
        BackGroundFill.Color := $BFBFBF;
        BackGroundFill.ColorTo := $919191;
        BackGroundFill.BorderColor := $D7D7D6;

        ProgressFill.Color := $828282;
        ProgressFill.ColorTo := clNone;
        ProgressFill.ColorMirror := clNone;
        ProgressFill.ColorMirrorTo := clNone;
        ProgressFill.BorderColor := $6D6D6D;
        ProgressFill.GradientMirrorType := gtVertical;

      end;
    tsWindows8, tsWindows10:
      begin

        Appearance.Overlays := False;
        Appearance.Shadows := False;
        BackGroundFill.Color := $F7F6F5;
        BackGroundFill.ColorTo := $F7F6F5;
        BackGroundFill.BorderColor := $E4E3E2;

        ProgressFill.Color := $F7E0C9;
        ProgressFill.ColorTo := clNone;
        ProgressFill.ColorMirror := clNone;
        ProgressFill.ColorMirrorTo := clNone;
        ProgressFill.BorderColor := clNone; //$E4A262;
        ProgressFill.GradientMirrorType := gtVertical;
      end;
    tsOffice2013White:
      begin
        Appearance.Overlays := False;
        Appearance.Shadows := False;

        BackGroundFill.Color := clWhite;
        BackGroundFill.ColorTo := clWhite;
        BackGroundFill.BorderColor := $D4D4D4;

        ProgressFill.Color := $FCE2C8;
        ProgressFill.ColorTo := clNone;
        ProgressFill.ColorMirror := clNone;
        ProgressFill.ColorMirrorTo := clNone;
        ProgressFill.BorderColor := clNone; //$E59D56;
        ProgressFill.GradientMirrorType := gtVertical;
      end;
    tsOffice2013LightGray:
      begin
        Appearance.Overlays := False;
        Appearance.Shadows := False;

        BackGroundFill.Color := $F6F6F6;
        BackGroundFill.ColorTo := $F6F6F6;
        BackGroundFill.BorderColor := $C6C6C6;

        ProgressFill.Color := $FCE2C8;
        ProgressFill.ColorTo := clNone;
        ProgressFill.ColorMirror := clNone;
        ProgressFill.ColorMirrorTo := clNone;
        ProgressFill.BorderColor := clNone; //$E59D56;
        ProgressFill.GradientMirrorType := gtVertical;
      end;

    tsOffice2013Gray:
      begin
        Appearance.Overlays := False;
        Appearance.Shadows := False;

        BackGroundFill.Color := $E5E5E5;
        BackGroundFill.ColorTo := $E5E5E5;
        BackGroundFill.BorderColor := $ABABAB;

        ProgressFill.Color := $FCE2C8;
        ProgressFill.ColorTo := clNone;
        ProgressFill.ColorMirror := clNone;
        ProgressFill.ColorMirrorTo := clNone;
        ProgressFill.BorderColor := clNone; //$E59D56;
        ProgressFill.GradientMirrorType := gtVertical;
      end;
    tsOffice2016White:  //todo
      begin
        Appearance.Overlays := False;
        Appearance.Shadows := False;

        BackGroundFill.Color := clWhite;
        BackGroundFill.ColorTo := clWhite;
        BackGroundFill.BorderColor := $D4D4D4;

        ProgressFill.Color := $F2E1D5;
        ProgressFill.ColorTo := clNone;
        ProgressFill.ColorMirror := clNone;
        ProgressFill.ColorMirrorTo := clNone;
        ProgressFill.BorderColor := clNone; //$E59D56;
        ProgressFill.GradientMirrorType := gtVertical;
      end;
    tsOffice2016Gray:
      begin
        Appearance.Overlays := False;
        Appearance.Shadows := False;

        BackGroundFill.Color := $B2B2B2;
        BackGroundFill.ColorTo := $B2B2B2;
        BackGroundFill.BorderColor := $444444;

        ProgressFill.Color := $F2E1D5;
        ProgressFill.ColorTo := clNone;
        ProgressFill.ColorMirror := clNone;
        ProgressFill.ColorMirrorTo := clNone;
        ProgressFill.BorderColor := clNone; //$E59D56;
        ProgressFill.GradientMirrorType := gtVertical;
      end;

    tsOffice2016Black:
      begin
        Appearance.Overlays := False;
        Appearance.Shadows := False;

        BackGroundFill.Color := $363636;
        BackGroundFill.ColorTo := $363636;
        BackGroundFill.BorderColor := $444444;

        ProgressFill.Color := $6A6A6A;
        ProgressFill.ColorTo := clNone;
        ProgressFill.ColorMirror := clNone;
        ProgressFill.ColorMirrorTo := clNone;
        ProgressFill.BorderColor := clNone; //$E59D56;
        ProgressFill.GradientMirrorType := gtVertical;
      end;

    end;
  end;
end;

procedure TProgressItem.SetValueFormat(const Value: string);
begin
  if (FValueFormat <> Value) then
  begin
    FValueFormat := Value;
    Changed;
  end;
end;

procedure Register;
begin
  RegisterPolyItem(TProgressItem);
end;

end.
