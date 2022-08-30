{*************************************************************************}
{ TButtonItem Class                                                       }
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

unit GDIPButtonItem;

interface

{$I TMSDEFS.INC}

uses
  Graphics, Forms, Windows, Classes, GDIPGraphicItem, Controls, GDIPCustomItem, AdvGDIP,
  ActnList, AdvStyleIF, Types
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
  TButtonItem = class(TGraphicItem)
  private
    FButtonHeight: integer;
    FButtonWidth: integer;
    FOnInternalItemButtonClick: TItemEvent;
    FOnItemButtonClick: TItemEvent;
    FDown: Boolean;
    FButtonCaption: String;
    FButtonCaptionColor: TColor;
    FButtonCaptionSize: Integer;
    procedure SetButtonHeight(const Value: integer);
    procedure SetButtonWidth(const Value: integer);
    procedure SetDown(const Value: Boolean);
    procedure SetButtonCaption(const Value: String);
    procedure SetButtonCaptionColor(const Value: TColor);
    procedure SetButtonCaptionSize(const Value: Integer);
  protected
    procedure DrawButtonText(g: TGPGraphics; ARect: TGPRectF; ItemAppearance: TItemAppearance); virtual;
    procedure DoItemButtonClick(Sender: TObject; Item: TCustomItem); virtual;
    procedure DoInternalItemButtonClick(Sender: TObject; Item: TCustomItem); virtual;
    function GetVersionNr: integer; override;
    procedure ActionSetCaption(const Value: string); override;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;
    function CreateNewItem(AOwner: TComponent): TCustomItem; override;
    function GetClassType: TComponentClass; override;
    class function CustomClassName: String; override;
    procedure DrawInRect(g: TGPGraphics; ItemAppearance: TItemAppearance; R: TGPRectF); override;
    function IsGraphicAtXY(pX, pY: integer): Boolean; override;
    procedure DoMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      pX, pY: Integer; Interaction: TItemInteraction; ItemAppearance: TItemAppearance); override;
    procedure DoKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState); override;
    property OnInternalItemButtonClick: TItemEvent read FOnInternalItemButtonClick write FOnInternalItemButtonClick;
    property Down: Boolean read FDown write SetDown;
    function DrawText(g: TGPGraphics; ARect: TGPRectF; ItemAppearance: TItemAppearance; DoText: Boolean = true): TGPRectF; override;
    procedure DrawImage(g: TGPGraphics; ARect: TGPRectF; ItemAppearance: TItemAppearance); override;
    class function Display: Boolean; override;
    procedure AssignEvents(Item: TCustomItem); override;
    procedure DoCMDialogChar(var Message: TCMDialogChar); override;
  published
    property ButtonWidth: integer read FButtonWidth write SetButtonWidth default 75;
    property ButtonHeight: integer read FButtonHeight write SetButtonHeight default 25;
    property ButtonCaption: String read FButtonCaption write SetButtonCaption;
    property ButtonCaptionSize: Integer read FButtonCaptionSize write SetButtonCaptionSize default -1;
    property ButtonCaptionColor: TColor read FButtonCaptionColor write SetButtonCaptionColor default clBlack;
    property OnItemButtonClick: TItemEvent read FOnItemButtonClick write FOnItemButtonClick;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterPolyItem(TButtonItem);
end;

{ TButtonItem }

procedure TButtonItem.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  inherited;
  if Sender is TCustomAction then
  with TCustomAction(Sender) do
  begin
    if not CheckDefaults or (Self.ButtonCaption = '') then
      Self.ButtonCaption := Caption;
  end;
end;

procedure TButtonItem.ActionSetCaption(const Value: string);
begin
  ButtonCaption := Value;
end;

procedure TButtonItem.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TButtonItem then
  begin
    FButtonHeight := (Source as TButtonItem).ButtonHeight;
    FButtonWidth := (Source as TButtonItem).ButtonWidth;
    FButtonCaption := (Source as TButtonItem).ButtonCaption;
    FButtonCaptionSize := (Source as TButtonItem).ButtonCaptionSize;
    FButtonCaptionColor := (Source as TButtonItem).ButtonCaptionColor;
    Changed;
  end;
end;

procedure TButtonItem.AssignEvents(Item: TCustomItem);
begin
  inherited;
  if Item is TButtonItem then
  begin
    OnItemButtonClick := (Item as TButtonItem).OnItemButtonClick;
  end;
end;

constructor TButtonItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FButtonHeight := 25;
  FButtonWidth := 75;
  if (csDesigning in AOwner.ComponentState) and not (csLoading in AOwner.ComponentState) then
    FButtonCaption := 'Button';
  FButtonCaptionSize := -1;
  FButtonCaptionColor := clBlack;
end;

function TButtonItem.CreateNewItem(AOwner: TComponent): TCustomItem;
begin
  Result := TButtonItem.Create(AOwner);
end;

class function TButtonItem.CustomClassName: String;
begin
  Result := 'Graphic Button Item';
end;

class function TButtonItem.Display: Boolean;
begin
  Result := true;
end;

procedure TButtonItem.DoCMDialogChar(var Message: TCMDialogChar);
begin
  inherited;
  with Message do
  begin
    if IsAccel(CharCode, ButtonCaption) then
    begin
      Down := True;
      Down := False;
      DoAction;
      Result := 1;
    end;
  end;
end;

procedure TButtonItem.DoInternalItemButtonClick(Sender: TObject;
  Item: TCustomItem);
begin
  if Assigned(OnInternalItemButtonClick) then
    OnInternalItemButtonClick(Sender, Item);
end;

procedure TButtonItem.DoItemButtonClick(Sender: TObject; Item: TCustomItem);
begin
  if Assigned(OnItemButtonClick) then
    OnItemButtonClick(Sender, Item);

  GraphicFocused := True;
  ItemFocused := False;
  if Assigned(OnInternalFocus) then
    OnInternalFocus(Sender, Item);
end;

procedure TButtonItem.DoKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if GraphicFocused and ((Key = VK_SPACE) or (Key = VK_RETURN)) then
  begin
    Down := true;
    Down := false;
    GraphicState := gsNormal;
    DoAction;
  end
  else
    inherited;
end;

procedure TButtonItem.DoMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; pX, pY: Integer; Interaction: TItemInteraction; ItemAppearance: TItemAppearance);
begin
  if Interaction.InteractionType = itGraphic then
  begin
    if Interaction.InteractionItem = Self then
    begin
      Down := true;
      Down := false;
      GraphicState := gsNormal;
      DoAction;
    end;
  end
  else
    inherited;
end;

procedure TButtonItem.DrawInRect(g: TGPGraphics; ItemAppearance: TItemAppearance; R: TGPRectF);
var
  ri: TRect;
begin
  inherited;
  case GraphicPosition of
    gpLeft: ri := Bounds(Round(r.x + ControlMargin.Left), Round(r.y + (r.height - ButtonHeight) / 2), ButtonWidth, ButtonHeight);
    gpRight: ri := Bounds(Round(r.x + r.Width - ButtonWidth - ControlMargin.Right), Round(r.y + (r.height - ButtonHeight) / 2), ButtonWidth, ButtonHeight);
  end;
  DrawGraphic(g, ri, ItemAppearance, gkButton, false);
  DrawButtonText(g, MakeRect(ri.Left, ri.Top, ri.Right - ri.Left, ri.Bottom - ri.Top), ItemAppearance);
end;

procedure TButtonItem.DrawButtonText(g: TGPGraphics; ARect: TGPRectF;
  ItemAppearance: TItemAppearance);
var
  ft: TFont;
  ff: TGPFontFamily;
  fs: integer;
  sf: TGPStringFormat;
  f: TGPFont;
  b: TGPSolidBrush;
  txtlt: integer;
  r: TGPRectF;
begin
  r := ARect;
  if ButtonCaption <> '' then
  begin
    with ItemAppearance do
    begin
      ft := GetFont(ItemAppearance);
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

      sf := TGPStringFormat.Create;
      sf.SetHotkeyPrefix(HotkeyPrefixShow);
      sf.SetAlignment(StringAlignmentCenter);
      sf.SetLineAlignment(StringAlignmentCenter);

      if ButtonCaptionSize <> -1 then
        f := TGPFont.Create(ff, Round(ButtonCaptionSize * DPIScale), fs, UnitPoint)
      else
        f := TGPFont.Create(ff, Round(ft.Size * DPIScale), fs, UnitPoint);

      if ButtonCaptionColor <> clNone then
        b := TGPSolidBrush.Create(MakeColor(255, ButtonCaptionColor))
      else
        b := TGPSolidBrush.Create(MakeColor(255, ft.Color));

      txtlt := length(ButtonCaption);

      g.DrawString(ButtonCaption, txtlt, f, r, sf, b);

      sf.Free;
      ff.Free;
      b.Free;
      f.Free;
    end;
  end;
end;

procedure TButtonItem.DrawImage(g: TGPGraphics; ARect: TGPRectF;
  ItemAppearance: TItemAppearance);
begin
  case GraphicPosition of
    gpLeft:
    begin
      ARect.X := ARect.X + ControlMargin.Left + ButtonWidth - 16;
      ARect.Width := Arect.Width - ControlMargin.Left - ButtonWidth + 16;
    end;
    gpRight:
    begin
      ARect.Width := ARect.Width - ButtonWidth - ControlMargin.Right + 16;
    end;
  end;

  inherited DrawImage(g, ARect, ItemAppearance);
end;

function TButtonItem.DrawText(g: TGPGraphics; ARect: TGPRectF;
  ItemAppearance: TItemAppearance; DoText: Boolean = true): TGPRectF;
begin
  case GraphicPosition of
    gpLeft:
    begin
      ARect.X := ARect.X + ControlMargin.Left + ButtonWidth;
      ARect.Width := Arect.Width - ControlMargin.Left - ButtonWidth;
    end;
    gpRight:
    begin
      ARect.Width := ARect.Width - ButtonWidth - ControlMargin.Right;
    end;
  end;

  Result := inherited DrawText(g, ARect, ItemAppearance, DoText);
end;

function TButtonItem.GetClassType: TComponentClass;
begin
  Result := TButtonItem;
end;

function TButtonItem.GetVersionNr: integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

function TButtonItem.IsGraphicAtXY(pX, pY: integer): Boolean;
begin
  Result := false;
  case GraphicPosition of
    gpLeft: Result := PtInRect(Bounds(Round(x + ControlMargin.Left), Round(y + (height - ButtonHeight) / 2), ButtonWidth, ButtonHeight), Point(pX, pY));
    gpRight: Result := PtInRect(Bounds(Round(x + Width - ButtonWidth - ControlMargin.Right), Round(y + (height - ButtonHeight) / 2), ButtonWidth, ButtonHeight), Point(pX, pY));
  end;
end;

procedure TButtonItem.SetButtonCaption(const Value: String);
begin
  if FButtonCaption <> Value then
  begin
    FButtonCaption := Value;
    Changed;
  end;
end;

procedure TButtonItem.SetButtonCaptionColor(const Value: TColor);
begin
  if FButtonCaptionColor <> Value then
  begin
    FButtonCaptionColor := Value;
    Changed;
  end;
end;

procedure TButtonItem.SetButtonCaptionSize(const Value: Integer);
begin
  if FButtonCaptionSize <> Value then
  begin
    FButtonCaptionSize := Value;
    Changed;
  end;
end;

procedure TButtonItem.SetButtonHeight(const Value: integer);
begin
  if FButtonHeight <> Value then
  begin
    FButtonHeight := Value;
    Changed;
  end;
end;

procedure TButtonItem.SetButtonWidth(const Value: integer);
begin
  if FButtonWidth <> Value then
  begin
    FButtonWidth := Value;
    Changed;
  end;
end;

procedure TButtonItem.SetDown(const Value: Boolean);
begin
  if FDown <> Value then
  begin
    FDown := Value;
    if FDown then
    begin
      DoInternalItemButtonClick(Self, Self);
      DoItemButtonClick(Self, Self);
    end;
    Changed;
  end;
end;

end.
