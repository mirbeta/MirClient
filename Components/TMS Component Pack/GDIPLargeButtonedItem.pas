{*************************************************************************}
{ TLargeButtonedItem Class                                                }
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

unit GDIPLargeButtonedItem;

interface

{$I TMSDEFS.INC}

uses
  Forms, Windows, Classes, Controls, GDIPCustomItem, GDIPImageTextButtonItem, GDIPImageTextButtonSectionItem,
  AdvGDIP, GDIPFill, Graphics, ActnList, Menus, AdvStyleIF
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
  TButtonCaptionLocation = (bclBottom, bclTop);

  TLargeButtonedItem = class(TImageTextButtonSectionItem)
  private
    FButtonCaption: String;
    FButtonCaptionLocation: TButtonCaptionLocation;
    FButtonCaptionSize: Integer;
    FButtonCaptionColor: TColor;
    FButtonCaptionOpacity: Byte;
    procedure SetButtonCaption(const Value: String);
    procedure SetButtonCaptionLocation(const Value: TButtonCaptionLocation);
    procedure SetButtonCaptionSize(const Value: Integer);
    procedure SetButtonCaptionColor(const Value: TColor);
    procedure SetButtonCaptionOpacity(const Value: Byte);
  protected
    function GetVersionNr: integer; override;
    procedure ActionSetCaption(const Value: string); override;
    function ActionIsCaptionLinked: Boolean; override;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    function UseButtonCaptionForAction: Boolean; override;
  public
    function CreateNewItem(AOwner: TComponent): TCustomItem; override;
    function GetClassType: TComponentClass; override;
    class function CustomClassName: String; override;
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;
    procedure DrawButton(g: TGPGraphics; ARect: TGPRectF; ItemAppearance: TItemAppearance); override;
    procedure InitDesignTime; override;
    procedure DoCMDialogChar(var Message: TCMDialogChar); override;
  published
    property ButtonCaption: String read FButtonCaption write SetButtonCaption;
    property ButtonCaptionLocation: TButtonCaptionLocation read FButtonCaptionLocation write SetButtonCaptionLocation default bclBottom;
    property ButtonCaptionSize: Integer read FButtonCaptionSize write SetButtonCaptionSize default -1;
    property ButtonCaptionColor: TColor read FButtonCaptionColor write SetButtonCaptionColor default clNone;
    property ButtonCaptionOpacity: Byte read FButtonCaptionOpacity write SetButtonCaptionOpacity default 255;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterPolyItem(TLargeButtonedItem);
end;

{ TLargeButtonedItem }

procedure TLargeButtonedItem.ActionChange(Sender: TObject;
  CheckDefaults: Boolean);
begin
  inherited;
  if Sender is TCustomAction then
  with TCustomAction(Sender) do
  begin
    if not CheckDefaults or (Self.ButtonCaption = '') and UseButtonCaptionForAction then
      Self.ButtonCaption := Caption;
  end;
end;

function TLargeButtonedItem.ActionIsCaptionLinked: Boolean;
begin
  Result := AnsiSameCaption(ButtonCaption, (Action as TCustomAction).Caption);
end;

procedure TLargeButtonedItem.ActionSetCaption(const Value: string);
begin
  ButtonCaption := Value;
end;

procedure TLargeButtonedItem.Assign(Source: TPersistent);
begin
  inherited;
  if (Source is TLargeButtonedItem) then
  begin
    FButtonCaption := (Source as TLargeButtonedItem).ButtonCaption;
    FButtonCaptionLocation := (Source as TLargeButtonedItem).ButtonCaptionLocation;
    FButtonCaptionSize := (Source as  TLargeButtonedItem).ButtonCaptionSize;
    FButtonCaptionLocation := (Source as TLargeButtonedItem).ButtonCaptionLocation;
    Changed;
  end;
end;

constructor TLargeButtonedItem.Create(AOwner: TComponent);
begin
  inherited;
  if (csDesigning in AOwner.ComponentState) and not (csLoading in AOwner.ComponentState) then
  begin
    Height := 100;
    ButtonLocation := blLeft;
    ButtonWidth := 85;
    ButtonMargin.Left := 5;
    ButtonMargin.Top := 5;
    ButtonMargin.Bottom := 10;
    ButtonMargin.Right := 5;
    ButtonType := btButton;
  end;
  if (csDesigning in AOwner.ComponentState) and not (csLoading in AOwner.ComponentState) then
    FButtonCaption := 'Button Caption';

  FButtonCaptionColor := clNone;
  FButtonCaptionSize := -1;
  FButtonCaptionOpacity := 255;
end;

function TLargeButtonedItem.CreateNewItem(AOwner: TComponent): TCustomItem;
begin
  Result := TLargeButtonedItem.Create(AOwner);
end;

class function TLargeButtonedItem.CustomClassName: String;
begin
  Result := 'Normal Large Buttoned Item';
end;

procedure TLargeButtonedItem.DoCMDialogChar(var Message: TCMDialogChar);
begin
  inherited;
  with Message do
  begin
    if IsAccel(CharCode, ButtonCaption) then
    begin
      Down := True;
      Down := False;
      Result := 1;
    end;
  end;
end;

procedure TLargeButtonedItem.DrawButton(g: TGPGraphics;
  ARect: TGPRectF; ItemAppearance: TItemAppearance);
var
  f: TGDIPFill;
  p: TGPPen;
  r, expr: TGPRectF;
  ft: TFont;
  ff: TGPFontFamily;
  fs: integer;
  sf: TGPStringFormat;
  fft: TGPFont;
  b: TGPSolidBrush;
  textoutr: TGPRectF;
  textr: TGPRectF;
begin
  f := nil;
  if ButtonEnabled then
  begin
    case ButtonState of
      bsNormal: f := ItemAppearance.ButtonNormal;
      bsDown: f := ItemAppearance.ButtonDown;
      bsHovered: f := ItemAppearance.ButtonHover;
    end;

    if ButtonType = btPushButton then
    begin
      if ButtonState = bsHovered then
        f := ItemAppearance.ButtonHover
      else
      begin
        if Checked or (ButtonState = bsDown) then
          f := ItemAppearance.ButtonDown
        else
          f := ItemAppearance.ButtonNormal;
      end;
    end;
  end
  else
    f:= ItemAppearance.ButtonDisabled;

  case ButtonLocation of
    blRight: r := MakeRect(ARect.x + ARect.Width - ButtonWidth - ButtonMargin.Right, ARect.y + ButtonMargin.Top, ButtonWidth, ARect.Height - ButtonMargin.Bottom - ButtonMargin.Top);
    blLeft: r := MakeRect(ARect.x + ButtonMargin.Left, ARect.y + ButtonMargin.Top, ButtonWidth, ARect.Height - ButtonMargin.Bottom - ButtonMargin.Top);
  end;

  if Assigned(f) then
  begin
    f.BeginUpdate;
    f.Focus := ButtonFocused and ItemAppearance.Focus and (ItemAppearance.FocusedItem = Self.Index);
    f.EndUpdate;


    f.Fill(g, r);

    ft := GetButtonFont(ItemAppearance);
    ff := TGPFontFamily.Create(ft.Name);
    if (ff.Status in [FontFamilyNotFound, FontStyleNotFound]) then
    begin
      ff.Free;
      ff := TGPFontFamily.Create('Arial');
    end;

    fs := 0;
    if (fsBold in ft.Style) then
      fs := fs + 1;
    if (fsItalic in ft.Style) then
      fs := fs + 2;
    if (fsUnderline in ft.Style) then
      fs := fs + 4;
    if (fsStrikeOut in ft.Style) then
      fs := fs + 8;

    if ButtonCaptionSize <> -1 then
      fft := TGPFont.Create(ff, Round(ButtonCaptionSize * DPIScale), fs, UnitPoint)
    else
      fft := TGPFont.Create(ff, Round(ft.Size * DPIScale), fs, UnitPoint);

    sf := TGPStringFormat.Create;
    sf.SetAlignment(StringAlignmentCenter);
    sf.SetLineAlignment(StringAlignmentCenter);
    sf.SetHotkeyPrefix(HotkeyPrefixShow);

    if ButtonCaptionColor <> clNone then
      b := TGPSolidBrush.Create(MakeColor(ButtonCaptionOpacity, ButtonCaptionColor))
    else
      b := TGPSolidBrush.Create(MakeColor(ButtonCaptionOpacity, ft.Color));

    g.MeasureString(ButtonCaption, Length(ButtonCaption), fft, MakeRect(r.X + 2, r.Y + 2, r.Width - 4, r.Height - 8), sf, textoutr);

    case ButtonCaptionLocation of
      bclBottom:
      begin
        textr := MakeRect(r.X + 2, r.Y + r.Height - textoutr.Height - 5, r.Width - 4, textoutr.Height);
        r.Height := r.Height - textoutr.Height - 7;
      end;
      bclTop:
      begin
        textr := MakeRect(r.X + 2, r.Y + 5,  r.Width - 4, textoutr.Height + 2);
        r.Y := r.Y + textoutr.Height + 7;
        r.Height := r.Height - textoutr.Height - 7;
      end;
    end;

    g.DrawString(ButtonCaption, Length(ButtonCaption), fft, textr, sf, b);

    b.Free;
    sf.Free;
    ff.Free;
    fft.Free;

    case ButtonType of
      btButton, btPushButton:
      begin
        if Down then
        begin
          if Assigned(ButtonImageDown) then
          begin
            r := MakeRect(r.X + (r.Width -  ButtonImageDown.Width) / 2, r.Y + (r.Height - ButtonImageDown.Height) / 2,
              ButtonImageDown.Width, ButtonImageDown.Height);
            ButtonImageDown.GDIPDraw(g, r)
          end;
        end
        else
        begin
          if Assigned(ButtonImageUp) then
          begin
            r := MakeRect(r.X + (r.Width -  ButtonImageUp.Width) / 2, r.Y +  (r.Height - ButtonImageUp.Height) / 2,
              ButtonImageUp.Width, ButtonImageUp.Height);
            ButtonImageUp.GDIPDraw(g, r);
          end;
        end;
      end;
    end;

    case ButtonType of
      btExpander:
      begin
        p := TGPPen.Create(MakeColor(255, ButtonExpanderColor));
        expr := MakeRect(r.X + (r.Width - (r.Width / 1.5)) / 2, r.Y + (r.Height - (r.Height / 1.5)) / 2, r.Width / 1.5, r.Height / 1.5);
        if not Down then
        begin
          g.DrawLine(p, expr.X + (expr.Width / 2), expr.Y + (expr.Height / 4 * 2), expr.X + (expr.Width / 3), expr.Y + (expr.Height / 4));
          g.DrawLine(p, expr.X + (expr.Width / 2), expr.Y + (expr.Height / 4 * 2), expr.X + (expr.Width / 3 * 2), expr.Y + (expr.Height / 4));
          g.DrawLine(p, expr.X + (expr.Width / 2), expr.Y + (expr.Height / 4 * 3), expr.X + (expr.Width / 3), expr.Y + (expr.Height / 2));
          g.DrawLine(p, expr.X + (expr.Width / 2), expr.Y + (expr.Height / 4 * 3), expr.X + (expr.Width / 3 * 2), expr.Y + (expr.Height / 2));
        end
        else
        begin
          g.DrawLine(p, expr.X + (expr.Width / 2), expr.Y + (expr.Height / 4), expr.X + (expr.Width / 3), expr.Y + (expr.Height / 4 * 2));
          g.DrawLine(p, expr.X + (expr.Width / 2), expr.Y + (expr.Height / 4), expr.X + (expr.Width / 3 * 2), expr.Y + (expr.Height / 4 * 2));
          g.DrawLine(p, expr.X + (expr.Width / 2), expr.Y + (expr.Height / 2), expr.X + (expr.Width / 3), expr.Y + (expr.Height / 4 * 3));
          g.DrawLine(p, expr.X + (expr.Width / 2), expr.Y + (expr.Height / 2), expr.X + (expr.Width / 3 * 2), expr.Y + (expr.Height / 4 * 3));
        end;
        p.Free;
      end;
    end;
  end;
end;

function TLargeButtonedItem.GetClassType: TComponentClass;
begin
  Result := TLargeButtonedItem;
end;

function TLargeButtonedItem.GetVersionNr: integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TLargeButtonedItem.InitDesignTime;
begin
  inherited;
  Height := 100;
  ButtonLocation := blLeft;
  ButtonWidth := 85;
  ButtonMargin.Left := 5;
  ButtonMargin.Top := 5;
  ButtonMargin.Bottom := 10;
  ButtonMargin.Right := 5;
  ButtonType := btButton;
end;

procedure TLargeButtonedItem.SetButtonCaption(const Value: String);
begin
  if FButtonCaption <> Value then
  begin
    FButtonCaption := Value;
    Changed;
  end;
end;

procedure TLargeButtonedItem.SetButtonCaptionColor(const Value: TColor);
begin
  if FButtonCaptionColor <> Value then
  begin
    FButtonCaptionColor := Value;
    Changed;
  end;
end;

procedure TLargeButtonedItem.SetButtonCaptionLocation(
  const Value: TButtonCaptionLocation);
begin
  if FButtonCaptionLocation <> Value then
  begin
    FButtonCaptionLocation := Value;
    Changed;
  end;
end;

procedure TLargeButtonedItem.SetButtonCaptionOpacity(const Value: Byte);
begin
  if FButtonCaptionOpacity <> Value then
  begin
    FButtonCaptionOpacity := Value;
    Changed;
  end;
end;

procedure TLargeButtonedItem.SetButtonCaptionSize(const Value: Integer);
begin
  if FButtonCaptionSize <> Value then
  begin
    FButtonCaptionSize := Value;
    Changed;
  end;
end;

function TLargeButtonedItem.UseButtonCaptionForAction: Boolean;
begin
  Result := True;
end;

end.
