{*************************************************************************}
{ TImageTextButtonItemClass                                               }
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

unit GDIPImageTextButtonItem;

interface

{$I TMSDEFS.INC}

uses
  Windows, Forms, SysUtils, imgList, Graphics, Classes, Controls, Messages, AdvGDIP,
  GDIPCustomItem, GDIPImageTextItem, GDIPFill, ActnList, AdvStyleIF, Menus, Types
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
  TItemButtonType = (btButton, btExpander, btPushButton);

  TButtonState = (bsNormal, bsDown, bsHovered);

  TButtonLocation = (blRight, blLeft);

  TImageTextButtonItem = class(TImageTextItem, ITMSStyle)
  private
    FButtonFocused: Boolean;
    FButtonWidth: Integer;
    FDown: Boolean;
    FButtonImageUp: TAdvGDIPPicture;
    FButtonType: TItemButtonType;
    FButtonImageDown: TAdvGDIPPicture;
    FButtonExpanderColor: TColor;
    FButtonHeight: Integer;
    FButtonHint: String;
    FButtonLocation: TButtonLocation;
    FButtonEnabled: Boolean;
    FButtonMargin: TMargins;
    FButtonGlow: Boolean;
    FButtonState: TButtonState;
    FOnItemButtonHint: TItemHintEvent;
    FOnInternalButtonClick: TItemEvent;
    FOnButtonClick: TItemEvent;
    FOnInternalItemButtonHint: TItemHintEvent;
    FChecked: Boolean;
    FAutoCheck: Boolean;
    procedure SetButtonWidth(const Value: Integer);
    procedure SetButtonImageUp(const Value: TAdvGDIPPicture);
    procedure SetButtonType(const Value: TItemButtonType);
    procedure SetButtonImageDown(const Value: TAdvGDIPPicture);
    procedure SetButtonExpanderColor(const Value: TColor);
    procedure SetButtonLocation(const Value: TButtonLocation);
    procedure SetButtonEnabled(const Value: Boolean);
    procedure SetButtonMargin(const Value: TMargins);
    procedure SetButtonGlow(const Value: Boolean);
    procedure SetButtonHint(const Value: String);
    procedure SetDown(const Value: Boolean);
    procedure SetButtonState(const Value: TButtonState);
    procedure SetChecked(const Value: Boolean);
    procedure SetAutoCheck(const Value: Boolean);
  protected
    procedure ButtonImageChanged(Sender: TObject);
    procedure DrawButton(g: TGPGraphics; ARect: TGPRectF; ItemAppearance: TItemAppearance); virtual;
    procedure DrawButtonGlow(g: TGPGraphics; ARect: TGPRectF; ItemAppearance: TItemAppearance); virtual;
    procedure ButtonMarginChanged(Sender: TObject);
    procedure DoItemButtonHint(Sender: TObject; Item: TCustomItem; var ButtonHint: String); virtual;
    procedure DoItemButtonClick(Sender: TObject; Item: TCustomItem); virtual;
    procedure DoInternalItemButtonClick(Sender: TObject; Item: TCustomItem); virtual;
    procedure DoInternalItemButtonHint(Sender: TObject; Item: TCustomItem; var ButtonHint: String); virtual;
    function GetVersionNr: integer; override;
    function ActionIsCaptionLinked: Boolean; override;
    function ActionIsEnabledLinked: Boolean; override;
    procedure ActionSetCaption(const Value: string); override;
    procedure ActionSetEnabled(Value: Boolean); override;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    function GetButtonFont(AItemAppearance: TItemAppearance): TFont;
  public
    procedure SetComponentStyle(AStyle: TTMSStyle);
    function CreateNewItem(AOwner: TComponent): TCustomItem; override;
    function GetClassType: TComponentClass; override;
    class function CustomClassName: String; override;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function IsButtonAtXY(pX, pY: Integer): Boolean;
    function GetItemInteraction(pX, pY: integer): TItemInteractionType; override;
    procedure DrawInRect(g: TGPGraphics; ItemAppearance: TItemAppearance; R: TGPRectF); override;
    function DrawText(g: TGPGraphics; ARect: TGPRectF; ItemAppearance: TItemAppearance; DoText: Boolean = true): TGPRectF; override;
    procedure DrawImage(g: TGPGraphics; ARect: TGPRectF; ItemAppearance: TItemAppearance); override;
    property Down: Boolean read FDown write SetDown;
    procedure DoCMHintShow(var Message: TMessage; Interaction: TItemInteraction); override;
    procedure DoMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      pX, pY: Integer; Interaction: TItemInteraction; ItemAppearance: TItemAppearance); override;
    procedure DoMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      pX, pY: Integer; Interaction: TItemInteraction; ItemAppearance: TItemAppearance); override;
    procedure DoMouseMove(Sender: TObject; Shift: TShiftState;
      pX, pY: Integer; Interaction: TItemInteraction; ItemAppearance: TItemAppearance); override;
     procedure ClearItemState; override;
    procedure AssignEvents(Item: TCustomItem); override;
    property OnInternalItemButtonClick: TItemEvent read FOnInternalButtonClick write FOnInternalButtonClick;
    property OnInternalItemButtonHint: TItemHintEvent read FOnInternalItemButtonHint write FOnInternalItemButtonHint;
    function ProcessTab(Backwards: Boolean): Boolean; override;
    procedure ResetTab; override;
    procedure DoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); override;
    procedure DoKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState); override;
    property ButtonFocused: Boolean read FButtonFocused;
  published
    property Action;
    property ButtonState: TButtonState read FButtonState write SetButtonState;
    property ButtonHint: String read FButtonHint write SetButtonHint;
    property ButtonWidth: Integer read FButtonWidth write SetButtonWidth default 30;
    property ButtonImageUp: TAdvGDIPPicture read FButtonImageUp write SetButtonImageUp;
    property ButtonImageDown: TAdvGDIPPicture read FButtonImageDown write SetButtonImageDown;
    property ButtonType: TItemButtonType read FButtonType write SetButtonType;
    property ButtonExpanderColor: TColor read FButtonExpanderColor write SetButtonExpanderColor default clBlack;
    property ButtonLocation: TButtonLocation read FButtonLocation write SetButtonLocation;
    property ButtonMargin: TMargins read FButtonMargin write SetButtonMargin;
    property ButtonEnabled: Boolean read FButtonEnabled write SetButtonEnabled default true;
    property ButtonGlow: Boolean read FButtonGlow write SetButtonGlow default true;
    property Checked: Boolean read FChecked write SetChecked default False;
    property AutoCheck: Boolean read FAutoCheck write SetAutoCheck default True;
    property OnItemButtonHint: TItemHintEvent read FOnItemButtonHint write FOnItemButtonHint;
    property OnItemButtonClick: TItemEvent read FOnButtonClick write FOnButtonClick;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterPolyItem(TImageTextButtonItem);
end;

{ TImageTextButtonItem }

procedure TImageTextButtonItem.ActionChange(Sender: TObject;
  CheckDefaults: Boolean);
begin
  inherited;
  if Sender is TCustomAction then
  with TCustomAction(Sender) do
  begin
    if not CheckDefaults or (Self.Caption = '') then
    begin
      if not UseButtonCaptionForAction then
        Self.Caption := Caption;
    end;
    if not CheckDefaults or (Self.ButtonEnabled = True) then
      Self.ButtonEnabled := Enabled;
  end;
end;

function TImageTextButtonItem.ActionIsCaptionLinked: Boolean;
begin
  Result := AnsiSameCaption(Caption, (Action as TCustomAction).Caption);
end;

function TImageTextButtonItem.ActionIsEnabledLinked: Boolean;
begin
  Result := ButtonEnabled = (Action as TCustomAction).Enabled;
end;

procedure TImageTextButtonItem.ActionSetCaption(const Value: string);
begin
  Caption := Value;
end;

procedure TImageTextButtonItem.ActionSetEnabled(Value: Boolean);
begin
  ButtonEnabled := Value;
end;

procedure TImageTextButtonItem.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TImageTextButtonItem then
  begin
    FButtonWidth := (Source as TImageTextButtonItem).ButtonWidth;
    FButtonMargin.Assign((Source as TImageTextButtonItem).ButtonMargin);
    FButtonImageUp.Assign((Source as TImageTextButtonItem).ButtonImageUp);
    FButtonImageDown.Assign((Source as TImageTextButtonItem).ButtonImageDown);
    FButtonHint := (Source as TImageTextButtonItem).ButtonHint;
    FButtonType := (Source as TImageTextButtonItem).ButtonType;
    FButtonExpanderColor := (Source as TImageTextButtonItem).ButtonExpanderColor;
    FButtonLocation := (Source as TImageTextButtonItem).ButtonLocation;
    FChecked := (Source as TImageTextButtonItem).Checked;
    FAutoCheck := (Source as TImageTextButtonItem).AutoCheck;
    Changed;
  end;
end;

procedure TImageTextButtonItem.AssignEvents(Item: TCustomItem);
begin
  inherited;
  if Item is TImageTextButtonItem then
  begin
    OnItemButtonHint := (Item as TImageTextButtonItem).OnItemButtonHint;
    OnItemButtonClick := (Item as TImageTextButtonItem).OnItemButtonClick;
  end;
end;

procedure TImageTextButtonItem.ButtonImageChanged(Sender: TObject);
begin
  Changed;
end;

procedure TImageTextButtonItem.ButtonMarginChanged(Sender: TObject);
begin
  Changed;
end;

procedure TImageTextButtonItem.ClearItemState;
begin
  inherited;
  ButtonState := bsNormal;
end;

constructor TImageTextButtonItem.Create;
begin
  inherited Create(AOwner);
  FButtonWidth := 30;
  FButtonHeight := 30;
  FDown := false;
  FButtonImageUp := TAdvGDIPPicture.Create;
  FButtonImageUp.OnChange := ButtonImageChanged;
  FButtonImageDown := TAdvGDIPPicture.Create;
  FButtonImageDown.OnChange := ButtonImageChanged;
  FButtonType := btExpander;
  FButtonExpanderColor := clBlack;
  FButtonHint := 'Image Text Button Item Hint';
  FButtonLocation := blRight;
  FButtonMargin := TMargins.Create(nil);
  FButtonMargin.OnChange := ButtonMarginChanged;
  FButtonMargin.Top := 0;
  FButtonMargin.Right := 0;
  FButtonMargin.Bottom := 0;
  FButtonMargin.Left := 0;
  FButtonEnabled := true;
  FButtonGlow := true;
  FChecked := False;
  FAutoCheck := True;
end;

function TImageTextButtonItem.CreateNewItem(AOwner: TComponent): TCustomItem;
begin
  Result := TImageTextButtonItem.Create(AOwner);
end;

class function TImageTextButtonItem.CustomClassName: String;
begin
  Result := 'Normal Text, Image and Button Item';
end;

destructor TImageTextButtonItem.Destroy;
begin
  FButtonMargin.Free;
  FButtonImageDown.Free;
  FButtonImageUp.Free;
  inherited;
end;

procedure TImageTextButtonItem.DoCMHintShow(var Message: TMessage;
  Interaction: TItemInteraction);
var
  hnt: String;
begin
  if Interaction.InteractionItem = Self then
  begin
    case Interaction.InteractionType of
      itButton:
      begin
        hnt := ButtonHint;

        with TCMHintShow(Message).HintInfo^ do
        begin
          DoInternalItemButtonHint(Self, Self, hnt);
          DoItemButtonHint(Self, Self, hnt);
          HintStr := hnt;
          ReshowTimeout := 0;
        end;
      end;
    end;
  end;
  inherited;
end;

procedure TImageTextButtonItem.DoInternalItemButtonClick(Sender: TObject;
  Item: TCustomItem);
begin
  if Assigned(OnInternalItemButtonClick) then
    OnInternalItemButtonClick(Sender, Item);
end;

procedure TImageTextButtonItem.DoInternalItemButtonHint(Sender: TObject;
  Item: TCustomItem; var ButtonHint: String);
begin
  if Assigned(OnInternalItemButtonHint) then
    OnInternalItemButtonHint(Sender, Item, ButtonHint);
end;

procedure TImageTextButtonItem.DoItemButtonClick(Sender: TObject;
  Item: TCustomItem);
begin
  if Assigned(OnItemButtonClick) then
    OnItemButtonClick(Sender, Item);
end;

procedure TImageTextButtonItem.DoItemButtonHint(Sender: TObject;
  Item: TCustomItem; var ButtonHint: String);
begin
  if Assigned(OnItemButtonHint) then
    OnItemButtonHint(Sender, Item, ButtonHint);
end;

procedure TImageTextButtonItem.DoKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if FButtonFocused and ((Key = VK_SPACE) or (Key = VK_RETURN)) then
  begin
    ButtonState := bsDown;
  end
  else
    inherited;
end;

procedure TImageTextButtonItem.DoKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if FButtonFocused and ((Key = VK_SPACE) or (Key = VK_RETURN)) then
  begin
    Down := not Down;
    ButtonState := bsNormal;
    DoAction;
  end
  else
    inherited;
end;

procedure TImageTextButtonItem.DoMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; pX, pY: Integer;
  Interaction: TItemInteraction; ItemAppearance: TItemAppearance);
begin
  inherited;
  if Interaction.InteractionItem = Self then
  begin
    case Interaction.InteractionType of
      itButton: ButtonState := bsDown;
    end;
  end;
end;

procedure TImageTextButtonItem.DoMouseMove(Sender: TObject; Shift: TShiftState;
  pX, pY: Integer; Interaction: TItemInteraction; ItemAppearance: TItemAppearance);
begin
  inherited;
  if Interaction.InteractionItem = Self then
  begin
    case Interaction.InteractionType of
      itButton:
      begin
        if ButtonState <> bsHovered then
          Application.CancelHint;

        ClearItemState;

        ButtonState := bsHovered;
      end;
    end;
  end;
end;

procedure TImageTextButtonItem.DoMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; pX, pY: Integer; Interaction: TItemInteraction; ItemAppearance: TItemAppearance);
begin
  inherited;
  if Interaction.InteractionItem = Self then
  begin
    case Interaction.InteractionType of
      itButton:
      begin
        Down := not Down;
        ButtonState := bsNormal;
        DoAction;
      end;
    end;
  end;
end;

procedure TImageTextButtonItem.DrawInRect(g: TGPGraphics;
  ItemAppearance: TItemAppearance; R: TGPRectF);
var
  tr, ir: TGPRectF;
  f: TGDIPFill;
begin
  if Visible then
  begin
    DoItemStartDraw(Self, g, Self, r);
    DoInternalItemStartDraw(Self, g, Self, r);

    case ButtonLocation of
      blRight: ir := MakeRect(r.X, r.Y, r.Width - ButtonWidth - 1, r.Height);
      blLeft: ir := MakeRect(r.X + ButtonWidth, r.Y, r.Width - ButtonWidth + 1, r.Height);
    end;

    f := GetFill(ItemAppearance);
    if Assigned(f) then
      f.Fill(g, ir);

    if ItemAppearance.Focus and (ItemAppearance.FocusedItem = Index) then
      DrawFocus(g, ir, ItemAppearance);

    tr := DrawText(g, ir, ItemAppearance);
    case Status.Position of
      spItemRectangle:  DrawStatus(g, MakeRect(ir.x, ir.y, ir.width, ir.height), ItemAppearance);
      spItemText: DrawStatus(g, tr, ItemAppearance);
    end;
    DrawImage(g, ir, ItemAppearance);
    DrawButton(g, r, ItemAppearance);

    if ButtonGlow and not ItemAppearance.IsMetroStyle then
      DrawButtonGlow(g, r, ItemAppearance);

    DoItemEndDraw(Self, g, Self, r);
    DoInternalItemEndDraw(Self, g, Self, r);
  end;
end;

procedure TImageTextButtonItem.DrawButton(g: TGPGraphics;
  ARect: TGPRectF; ItemAppearance: TItemAppearance);
var
  f: TGDIPFill;
  p: TGPPen;
  r, expr: TGPRectF;
  pic: TAdvGDIPPicture;
  picpos: TFillPicturePosition;
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
    f := ItemAppearance.ButtonDisabled;


  case ButtonLocation of
    blRight: r := MakeRect(ARect.x + ARect.Width - ButtonWidth - ButtonMargin.Right, ARect.y + ButtonMargin.Top, ButtonWidth, ARect.Height - ButtonMargin.Bottom - ButtonMargin.Top);
    blLeft: r := MakeRect(ARect.x + ButtonMargin.Left, ARect.y + ButtonMargin.Top, ButtonWidth, ARect.Height - ButtonMargin.Bottom - ButtonMargin.Top);
  end;

  if Assigned(f) then
  begin
    f.BeginUpdate;
    pic := TAdvGDIPPicture.Create;
    pic.Assign(f.Picture);
    picpos := f.PicturePosition;
    f.PicturePosition := ppCenterCenter;
    case ButtonType of
      btButton, btPushButton:
      begin
        if Down then
          f.Picture := ButtonImageDown
        else
          f.Picture := ButtonImageUp;
      end;
    end;
    f.Focus := ButtonFocused and ItemAppearance.Focus and (ItemAppearance.FocusedItem = Self.Index);
    f.EndUpdate;
    f.Fill(g, r);
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
    f.BeginUpdate;
    f.Picture.Assign(pic);
    pic.Free;
    f.PicturePosition := picpos;
    f.EndUpdate;
  end;
end;

procedure TImageTextButtonItem.DrawButtonGlow(g: TGPGraphics;
  ARect: TGPRectF; ItemAppearance: TItemAppearance);
var
  b: TGPLinearGradientBrush;
  p: TGPPen;
  r, ro, rf: TGPRectF;
  pth: TGPGraphicsPath;
  f: TGDIPFill;
begin
  case ButtonLocation of
    blRight: r := MakeRect(ARect.x + ARect.Width - ButtonWidth - ButtonMargin.Right, ARect.y + ButtonMargin.Top, ButtonWidth, ARect.Height - ButtonMargin.Bottom - ButtonMargin.Top);
    blLeft: r := MakeRect(ARect.x + ButtonMargin.Left, ARect.y + ButtonMargin.Top, ButtonWidth, ARect.Height - ButtonMargin.Bottom - ButtonMargin.Top);
  end;

  rf := MakeRect(r.X + 1, r.Y + 1, r.Width - 2, r.Height - 2);

  f := GetFill(ItemAppearance);
  pth := CreateRoundRectangle(rf, f.Rounding, f.RoundingType, false);
  p := TGPPen.Create(MakeColor(200, clWhite));
  g.DrawPath(p, pth);
  p.free;

  pth.Reset;
  ro := MakeRect(rf.x + 5, rf.y, rf.Width - 10, 4);
  b := TGPLinearGradientBrush.Create(MakeRect(ro.X - 1, ro.Y - 1, ro.Width + 2, ro.Height + 2), MakeColor(175, clWhite), MakeColor(0, clWhite), LinearGradientModeVertical);
  pth.AddLine(rf.X, rf.Y, rf.X + 5, rf.Y);
  pth.AddLine(rf.X + 5, rf.Y, rf.X + 5, rf.Y + 4);
  pth.CloseFigure;
  g.FillPath(b, pth);
  pth.Reset;
  pth.AddLine(rf.X + rf.Width, rf.Y, rf.X + rf.Width - 5, rf.Y);
  pth.AddLine(rf.X + rf.Width - 5, rf.Y, rf.X + rf.Width - 5, rf.Y + 4);
  pth.CloseFigure;
  g.FillPath(b, pth);
  g.FillRectangle(b, ro);
  b.free;

  //
  ro := MakeRect(rf.X + 5, rf.Y + rf.Height - 7, rf.Width - 10, 7);
  b := TGPLinearGradientBrush.Create(MakeRect(ro.X - 1, ro.Y - 1, ro.Width + 2, ro.Height + 2), MakeColor(0, clWhite), MakeColor(220, clWhite), LinearGradientModeVertical);
  pth.Reset;
  pth.AddLine(rf.X, rf.Y + rf.Height, rf.X + 5, rf.Y + rf.Height);
  pth.AddLine(rf.X + 5, rf.Y  + rf.Height, rf.X + 5, rf.Y + rf.Height - 7);
  pth.CloseFigure;
  g.FillPath(b, pth);
  pth.Reset;
  pth.AddLine(rf.X + rf.Width, rf.Y + rf.Height, rf.X + rf.Width - 5, rf.Y  + rf.Height);
  pth.AddLine(rf.X + rf.Width - 5, rf.Y  + rf.Height - 5, rf.X + rf.Width - 5, rf.Y + rf.Height - 7);
  pth.CloseFigure;
  g.FillPath(b, pth);
  g.FillRectangle(b, ro);
  b.free;

  pth.Free;
end;

procedure TImageTextButtonItem.DrawImage(g: TGPGraphics;
  ARect: TGPRectF; ItemAppearance: TItemAppearance);
var
  r: TGPRectF;
  imgr: TGPRectF;
begin
  if ImageVisible then
  begin
    r := ARect;
    case ButtonLocation of
      blLeft:
      begin
        r.X := r.X + ButtonMargin.Left + ButtonMargin.Right;
        r.Width := r.Width - ButtonMargin.Left - ButtonMargin.Right;
      end;
    end;
    imgr := MakeRect(r.X + Spacing, r.Y + (r.Height - ImageHeight) / 2, ImageWidth, ImageHeight);

    if Assigned(Image) and not Image.Empty then
    begin
      Image.GDIPDraw(g, imgr);
    end;
    DrawImageFromImageList(g, imgr, ItemAppearance, ImageIndex);
    DrawImageFromContainer(g, imgr, ItemAppearance, ImageName);
  end;
end;

function TImageTextButtonItem.DrawText(g: TGPGraphics;
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
  case ButtonLocation of
    blRight: r.Width := r.Width - ButtonMargin.Right;
    blLeft:
    begin
      r.X := r.X + ButtonMargin.Left + ButtonMargin.Right;
      r.Width := r.Width - ButtonMargin.Left - ButtonMargin.Right;
    end;
  end;
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
        f := TGPFont.Create(ff, Round(DescriptionSize * DPIScale), fs, UnitPoint)
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

function TImageTextButtonItem.GetButtonFont(
  AItemAppearance: TItemAppearance): TFont;
begin
  Result := nil;
  if ButtonEnabled then
  begin
    case ButtonState of
      bsNormal: Result := AItemAppearance.NormalFont;
      bsDown: result := AItemAppearance.DownFont;
      bsHovered: result := AItemAppearance.HoverFont;
    end;
  end
  else
    Result := AItemAppearance.DisabledFont;
end;

function TImageTextButtonItem.GetClassType: TComponentClass;
begin
  Result := TImageTextButtonItem;
end;

function TImageTextButtonItem.GetItemInteraction(pX,
  pY: integer): TItemInteractionType;
begin
  Result := itNone;
  if IsButtonAtXY(pX, pY) and ButtonEnabled then
    result := itButton
  else if IsStatusAtXY(pX, pY) then
    result := itStatus
  else if IsItemAtXY(pX, pY) then
    result := itDefault;
end;

function TImageTextButtonItem.GetVersionNr: integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

function TImageTextButtonItem.IsButtonAtXY(pX, pY: Integer): Boolean;
var
  r: TGPRectF;
begin
  case ButtonLocation of
    blRight: r := MakeRect(x + Width - ButtonWidth - ButtonMargin.Right, y + ButtonMargin.Top, ButtonWidth, Height - ButtonMargin.Bottom - ButtonMargin.Top);
    blLeft: r := MakeRect(x + ButtonMargin.Left, y + ButtonMargin.Top, ButtonWidth, Height - ButtonMargin.Bottom - ButtonMargin.Top);
  end;

  Result := PtInGPRect(r, Point(pX, pY));
end;

function TImageTextButtonItem.ProcessTab(Backwards: Boolean): Boolean;
begin
  if IsSection then
  begin
    Result := False;
    if not FButtonFocused then
    begin
      FButtonFocused := True;
      Changed;
    end
    else
    begin
      Result := True;
      Exit;
    end;
  end
  else
  begin
    Result := False;
    if ButtonEnabled then
    begin
      if ItemFocused then
      begin
        if not Backwards then
        begin
          FButtonFocused := True;
          ItemFocused := False;
          Changed;
        end
        else
        begin
          Result := True;
          Exit;
        end;
      end
      else if FButtonFocused then
      begin
        if not Backwards then
        begin
          Result := True;
          Exit;
        end
        else
        begin
          FButtonFocused := False;
          ItemFocused := True;
          Changed;
        end;
      end;
    end
    else
    begin
      Result := True;
    end;
  end;
end;

procedure TImageTextButtonItem.ResetTab;
begin
  inherited;
  FButtonFocused := False;
end;

procedure TImageTextButtonItem.SetButtonImageUp(
  const Value: TAdvGDIPPicture);
begin
  if FButtonImageUp <> value then
  begin
    FButtonImageUp.Assign(Value);
    Changed;
  end;
end;

procedure TImageTextButtonItem.SetButtonLocation(
  const Value: TButtonLocation);
begin
  if FButtonLocation <> Value then
  begin
    FButtonLocation := Value;
    Changed;
  end;
end;

procedure TImageTextButtonItem.SetButtonMargin(const Value: TMargins);
begin
  if FButtonMargin <> Value then
  begin
    FButtonMargin.Assign(Value);
    Changed;
  end;
end;

procedure TImageTextButtonItem.SetButtonState(const Value: TButtonState);
begin
  if FButtonState <> Value then
  begin
    FButtonState := Value;
    RefreshObject;
  end;
end;

procedure TImageTextButtonItem.SetAutoCheck(const Value: Boolean);
begin
  if FAutoCheck <> Value then
  begin
    FAutoCheck := Value;
    Changed;
  end;
end;

procedure TImageTextButtonItem.SetButtonEnabled(const Value: Boolean);
begin
  if FButtonEnabled <> Value then
  begin
    FButtonEnabled := Value;
    Changed;
  end;
end;

procedure TImageTextButtonItem.SetButtonExpanderColor(const Value: TColor);
begin
  if FButtonExpanderColor <> Value then
  begin
    FButtonExpanderColor := Value;
    Changed;
  end;
end;

procedure TImageTextButtonItem.SetButtonGlow(const Value: Boolean);
begin
  if FButtonGlow <> Value then
  begin
    FButtonGlow := Value;
    Changed;
  end;
end;

procedure TImageTextButtonItem.SetButtonHint(const Value: String);
begin
  if FButtonHint <> Value then
  begin
    FButtonHint := Value;
    Changed;
  end;
end;

procedure TImageTextButtonItem.SetButtonImageDown(
  const Value: TAdvGDIPPicture);
begin
  if FButtonImageDown <> value then
  begin
    FButtonImageDown.Assign(Value);
    Changed;
  end;
end;

procedure TImageTextButtonItem.SetButtonType(const Value: TItemButtonType);
begin
  if FButtonType <> value then
  begin
    FButtonType := Value;
    Changed;
  end;
end;

procedure TImageTextButtonItem.SetButtonWidth(const Value: Integer);
begin
  if FButtonWidth <> value then
  begin
    FButtonWidth := Value;
    Changed;
  end;
end;

procedure TImageTextButtonItem.SetChecked(const Value: Boolean);
begin
  if FChecked <> Value then
  begin
    FChecked := Value;
    FDown := Value;
    RefreshObject;
  end;
end;

procedure TImageTextButtonItem.SetComponentStyle(AStyle: TTMSStyle);
begin
  ButtonGlow := not (AStyle = tsWindows8);
end;

procedure TImageTextButtonItem.SetDown(const Value: Boolean);
begin
  if FDown <> Value then
  begin
    FDown := Value;
    if AutoCheck and (ButtonType = btPushButton) then
      Checked := Value;
//    if FDown then
    begin
      DoItemButtonClick(Self, Self);
      DoInternalItemButtonClick(Self, Self);
    end;
    RefreshObject;
  end;
end;

end.
