{***************************************************************************}
{ TMS W7 Controls Pack                                                      }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2012 - 2014                                        }
{            Email : info@tmssoftware.com                                   }
{            Web : http://www.tmssoftware.com                               }
{                                                                           }
{ The source code is given as is. The author is not responsible             }
{ for any possible damage done due to the use of this code.                 }
{ The component can be freely used in any application. The complete         }
{ source code remains property of the author and may not be distributed,    }
{ published, given or sold in any form as such. No parts of the source      }
{ code can be included in any other component or application without        }
{ written authorization of the author.                                      }
{***************************************************************************}

unit W7Buttons;


interface
{$I TMSDEFS.INC}

uses
  Windows,  Classes, Controls, ImgList, Graphics, CommCtrl, W7Classes, W7Common,
  W7Graphics, ExtCtrls, Forms;

type
  TW7Borders = set of (wsbLeft, wsbTop, wsbRight, wsbBottom);
  TW7PageSelectorStyle = (pssClassic, pssNavy, pssCustom);
  TW7SpeedButtonStyle = (sbsStandart, sbsLight, sbsFlat);
  TW7ImageAlignment = (ialLeft, ialRight);

  TW7PageSelectorColors = record
    MouseOutTopStartColor: TColor;
    MouseOutTopEndColor: TColor;
    MouseOutBottomStartColor: TColor;
    MouseOutBottomEndColor: TColor;
    MouseInTopStartColor: TColor;
    MouseInTopEndColor: TColor;
    MouseInBottomStartColor: TColor;
    MouseInBottomEndColor: TColor;
    ActiveTopStartColor: TColor;
    ActiveTopEndColor: TColor;
    ActiveBottomStartColor: TColor;
    ActiveBottomEndColor: TColor;
    NotActiveInnerFocusFrameColor: TColor;
    NotActiveOuterFocusFrameColor: TColor;
    ActiveInnerFocusFrameColor: TColor;
    ActiveOuterFocusFrameColor: TColor;
    BorderColor: TColor;
    FontColor: TColor;
  end;

  TW7CustomToolButton = class(TW7TransparentControl)
  private
    FCaptionAlignment: TAlignment;
    FTransparent: boolean;
    FShowArrow: boolean;
    FSetFocusOnClick: boolean;
    FImages: TCustomImageList;
    FImageChangeLink: TChangeLink;
    FImageIndex: integer;
    FIconSize: TW7ButtonIconSize;
    FIconHeight: integer;
    FArrowType: TW7ArrowType;
    FArrowHeight: integer;
//    procedure SetTransparent(Value: boolean);
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure SetShowArrow(Value: boolean);
    procedure SetImages(Value: TCustomImageList);
    procedure SetImageIndex(Value: integer);
    procedure SetIconSize(Value: TW7ButtonIconSize);
    procedure SetArrowType(Value: TW7ArrowType);
    procedure SetCaptionAlignment(const Value: TAlignment);
  protected
    ReturnButtonPressed: boolean;
    procedure Paint; override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure DrawBorder;
    procedure DrawButtonElements;
    procedure ImageListChange(Sender: TObject);
    procedure UpdateImageList;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Caption;
    property CaptionAlignment: TAlignment read FCaptionAlignment write SetCaptionAlignment default taCenter;
    property ShowArrow: boolean read FShowArrow write SetShowArrow default False;
    property SetFocusOnClick: boolean read FSetFocusOnClick write FSetFocusOnClick default True;
    property Images: TCustomImageList read FImages write SetImages;
    property ImageIndex: integer read FImageIndex write SetImageIndex default -1;
    property IconSize: TW7ButtonIconSize read FIconSize write SetIconSize default is16px;
    property ArrowType: TW7ArrowType read FArrowType write SetArrowType default atDown;
  end;


  TW7CustomPageSelector = class (TW7Control)
  private
    FShowArrow: boolean;
    FSetFocusOnClick: boolean;
    FImages: TCustomImageList;
    FImageChangeLink: TChangeLink;
    FImageIndex: integer;
    FIconSize: TW7ButtonIconSize;
    FIconHeight: integer;
    FActive: boolean;
    FGroupIndex: integer;
    FPressButtonEffect: boolean;
    FHideArrowOnActive: boolean;
    FArrowType: TW7ArrowType;
    FArrowHeight: integer;
    FBorders: TW7Borders;
    FBorderColor: TColor;
    FHideFocusIfActive: boolean;
    FStyle: TW7PageSelectorStyle;
    FColors: TW7PageSelectorColors;
    FInternalColors: TW7PageSelectorColors;
    FActiveFontColor: TColor;
    procedure SetShowArrow(Value: boolean);
    procedure SetImages(Value: TCustomImageList);
    procedure SetImageIndex(Value: integer);
    procedure SetIconSize(Value: TW7ButtonIconSize);
    procedure SetActive(Value: boolean);
    procedure SetArrowType(Value: TW7ArrowType);
    procedure SetBorders(Value: TW7Borders);
    procedure SetBorderColor(Value: TColor);
    procedure SetStyle(Value: TW7PageSelectorStyle);
    procedure SetColors(Value: TW7PageSelectorColors);
  protected
    ReturnButtonPressed: boolean;
    procedure Paint; override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure DrawFocusedBorder;
    procedure DrawButtonElements;
    procedure ImageListChange(Sender: TObject);
    procedure UpdateImageList;
    procedure ActivateStyle(Style: TW7PageSelectorStyle);
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Caption;
    property ShowArrow: boolean read FShowArrow write SetShowArrow default False;
    property SetFocusOnClick: boolean read FSetFocusOnClick write FSetFocusOnClick default True;
    property Images: TCustomImageList read FImages write SetImages;
    property ImageIndex: integer read FImageIndex write SetImageIndex default -1;
    property IconSize: TW7ButtonIconSize read FIconSize write SetIconSize default is24px;
    property Active: boolean read FActive write SetActive default false;
    property GroupIndex: integer read FGroupIndex write FGroupIndex default 0;
    property PressButtonEffect: boolean read FPressButtonEffect write FPressButtonEffect default false;
    property HideArrowOnActive: boolean read FHideArrowOnActive write FHideArrowOnActive default true;
    property ArrowType: TW7ArrowType read FArrowType write SetArrowType default atRight;
    property Borders: TW7Borders read FBorders write SetBorders default [wsbLeft, wsbTop, wsbRight, wsbBottom];
    property BorderColor: TColor read FBorderColor write SetBorderColor nodefault;
    property HideFocusIfActive: boolean read FHideFocusIfActive write FHideFocusIfActive default True;
    property Style: TW7PageSelectorStyle read FStyle write SetStyle default pssClassic;
    property Colors: TW7PageSelectorColors read FColors write SetColors nodefault;
    property ActiveFontColor: TColor read FActiveFontColor write FActiveFontColor nodefault;
  published

  end;

  TW7CustomSpeedButton = class (TW7GraphicControl)
  private
    FEnabled: boolean;
    FFlat: boolean;
    FLight: boolean;
    FBackground: TBitmap;
    FMouseInBackground: TBitmap;
    FMouseDownBackground: TBitmap;
    FInternalTimer: TTimer;
    FMouseInOpacity: integer;
    FBackgroundsCreated: boolean;
    FMouseUpped: boolean;
    FFadeInInterval: integer;
    FFadeOutInterval: integer;
    FShowArrow: boolean;
    FImages: TCustomImageList;
    FImageChangeLink: TChangeLink;
    FImageIndex: integer;
    FIconSize: TW7ButtonIconSize;
    FIconHeight: integer;
    FArrowType: TW7ArrowType;
    FArrowHeight: integer;
    FPressButtonEffect: boolean;
    procedure SetFlat(Value: boolean);
    procedure SetLight(Value: boolean);
    procedure SetShowArrow(Value: boolean);
    procedure SetImages(Value: TCustomImageList);
    procedure SetImageIndex(Value: integer);
    procedure SetIconSize(Value: TW7ButtonIconSize);
    procedure SetArrowType(Value: TW7ArrowType);
  protected
    procedure SetEnabled(Value: Boolean); override;
    procedure TimerProc(Sender: TObject);
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure DrawMouseInSubLight;
    procedure DrawMouseDownSubLight;
    procedure DrawFlatMouseInSubLight;
    procedure DrawFlatMouseDownSubLight;
    procedure DrawMouseInBorder;
    procedure DrawMouseDownBorder;
    procedure CreateMouseInBody;
    procedure CreateFlatMouseInBody;
    procedure CreateFlatMouseDownBody;
    procedure CreateMouseDownBody;
    procedure CreateBackgrounds;
    procedure HideCorners;
    procedure DrawEnabledBorder;
    procedure DrawDisabledBorder;
    procedure DrawBorder;
    procedure DrawStandartBackground;
    procedure DrawLightBackground;
    procedure DrawFlatBackground;
    procedure DrawDisabledBackground;
    procedure DrawMouseInBody;
    procedure DrawBackground;
    procedure DrawButtonElements;
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure ImageListChange(Sender: TObject);
    procedure UpdateImageList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Caption;
    property Flat: boolean read FFlat write SetFlat;
    property Light: boolean read FLight write SetLight;
    property FadeInInterval: integer read FFadeInInterval write FFadeInInterval;
    property FadeOutInterval: integer read FFadeOutInterval write FFadeOutInterval;
    property ShowArrow: boolean read FShowArrow write SetShowArrow default False;
    property Images: TCustomImageList read FImages write SetImages;
    property ImageIndex: integer read FImageIndex write SetImageIndex default -1;
    property IconSize: TW7ButtonIconSize read FIconSize write SetIconSize;
    property ArrowType: TW7ArrowType read FArrowType write SetArrowType;
    property PressButtonEffect: boolean read FPressButtonEffect write FPressButtonEffect default true;
    property Font;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TW7ToolButton = class (TW7CustomToolButton)
  published
    property Caption;
    property CaptionAlignment;
    property Enabled;
    property ShowArrow;
    property SetFocusOnClick;
    property Images;
    property ImageIndex;
    property IconSize;
    property ArrowType;
    property Font;
    property Action;
    property Align;
    property Anchors;
    property Constraints;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    {$IFDEF DELPHI_UNICODE}
    property ParentDoubleBuffered;
    {$ENDIF}
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop default true;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    {$IFDEF DELPHI2006_LVL}
    property OnMouseActivate;
    {$ENDIF}
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;

  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TW7PageSelector = class (TW7CustomPageSelector)
  published
    property Caption;
    property ShowArrow;
    property SetFocusOnClick;
    property Images;
    property ImageIndex;
    property IconSize;

    property Align;
    property Anchors;
    property Active;
    property GroupIndex;
    property PressButtonEffect;
    property HideArrowOnActive;
    property ArrowType;
    property Borders;
    property BorderColor;
    property HideFocusIfActive;
    property Style;
    property Colors;
    property ActiveFontColor;
    property Font;
    property Constraints;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    {$IFDEF DELPHI_UNICODE}
    property ParentDoubleBuffered;
    {$ENDIF}
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    {$IFDEF DELPHI2006_LVL}
    property OnMouseActivate;
    {$ENDIF}
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TW7SpeedButton = class (TW7CustomSpeedButton)
  published
    property Caption;
    property Flat;
    property Light;
    property FadeInInterval;
    property FadeOutInterval;
    property ShowArrow;
    property Images;
    property ImageIndex;
    property IconSize;
    property ArrowType;
    property Font;
    property Action;
    property Align;
    property Anchors;
    property Constraints;
    property Enabled;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property PressButtonEffect;
    property OnClick;
    property OnDblClick;
    {$IFDEF DELPHI2006_LVL}
    property OnMouseActivate;
    {$ENDIF}
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
  end;

implementation

procedure TW7CustomToolButton.CMDialogChar(var Message: TCMDialogChar);
begin
  if Enabled and IsAccel(Message.CharCode, Caption) then
  begin
    Click;
    Message.Result := 1;
    Exit;
  end;

  inherited;
end;

constructor TW7CustomToolButton.Create(AOwner: TComponent);
begin
  inherited;
  Height := 24;
  Width := 100;
  FTransparent := False;
  FCaptionAlignment := taCenter;
  TabStop := True;
  ReturnButtonPressed := False;
  Font.Color := $005B391E;
  if GetWindowsVersion >= 6 then
  begin
    Font.Name := W7StandartFontName;
    Font.Size := W7StandartFontSize;
  end;
  FSetFocusOnClick := True;
  FShowArrow := False;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
  FIconSize := is16px;
  FIconHeight := 16;
  FArrowType := atDown;
  FArrowHeight := 5;
  ControlStyle := ControlStyle - [csOpaque];
  FImageIndex := -1;
end;

destructor TW7CustomToolButton.Destroy;
begin
  FImageChangeLink.Free;
  FImageChangeLink := nil;
  inherited;
end;

procedure TW7CustomToolButton.DrawBorder;
begin
  Canvas.Pen.Style := psSolid;
  Canvas.Brush.Style := bsClear;
  Canvas.Pen.Color := $00DBCABB;
  Canvas.RoundRect(0, 0, Width, Height, 4, 4);
  Canvas.Pen.Color := $00F9F4EF;
  Canvas.RoundRect(1, 1, Width - 1, Height - 1, 4, 4);
  Canvas.Pixels[1, Height - 1] := $00E5D4C8;
  Canvas.Pixels[0, Height - 2] := $00E5D4C8;
  Canvas.Pixels[1, Height - 2] := $00E5D4C8;
  Canvas.Pixels[Width - 2, Height - 1] := $00E5D4C8;
  Canvas.Pixels[Width - 1, Height - 2] := $00E5D4C8;
  Canvas.Pixels[Width - 2, Height - 2] := $00E5D4C8;
  Canvas.Pixels[0, 1] := $00EBE0D8;
  Canvas.Pixels[1, 1] := $00EBE0D8;
  Canvas.Pixels[1, 0] := $00EBE0D8;
  Canvas.Pixels[Width - 1, 1] := $00EBE0D8;
  Canvas.Pixels[Width - 2, 1] := $00EBE0D8;
  Canvas.Pixels[Width - 2, 0] := $00EBE0D8;
end;

procedure TW7CustomToolButton.DrawButtonElements;
var
  DestRect: TRect;
  Flags: Cardinal;
  DropDownDelta: integer;
  TextWidth: integer;
  TextHeight: integer;
  IconDelta: integer;
  PushedDelta: integer;
  Delta: integer;
  TotalWidth: integer;
  ShowIcon: boolean;
begin
  Canvas.Font.Assign(Font);
  if not Enabled then
    Canvas.Font.Color := clGray;

  Canvas.Brush.Style := bsClear;
  Flags := 0;
  Flags := Flags or DT_CENTER or DT_SINGLELINE or DT_VCENTER or DT_WORD_ELLIPSIS;
  ShowIcon := False;
  DropDownDelta := 0;
  if FShowArrow then
  begin
    case FArrowType of
      atUp, atDown: DropDownDelta := 14;
      atRight, atLeft: DropDownDelta := 10;
    end;
  end;

  if ReturnButtonPressed or LeftButtonPressed then
    PushedDelta := 1
  else
    PushedDelta := 0;

  IconDelta := 0;

  if Assigned(FImages) and (FImageIndex >= 0) and (FImageIndex < FImages.Count) then
  begin
    if Caption = '' then
      IconDelta := FIconHeight
    else
      IconDelta := FIconHeight + 5;
    ShowIcon := True;
  end;

  Delta := 3;

  Canvas.TextFlags := Flags;
  TextWidth := Canvas.TextWidth(Caption);
  TextHeight := Canvas.TextHeight(Caption);
  TotalWidth := TextWidth + DropDownDelta + IconDelta;

  if (TotalWidth) > (Width - Delta * 2) then
  begin
    DestRect := Rect(IconDelta + Delta + PushedDelta, Height div 2 - TextHeight div 2 + PushedDelta - 1, Width - 1 - Delta + PushedDelta - DropDownDelta, Height div 2 + TextHeight div 2 + PushedDelta);
  end
  else
  begin
    case CaptionAlignment of
    taLeftJustify: DestRect := Rect(2 + IconDelta + PushedDelta, Height div 2 - TextHeight div 2 + PushedDelta - 1, 2 + IconDelta + PushedDelta + TextWidth, Height div 2 + TextHeight div 2 + PushedDelta);
    taCenter: DestRect := Rect(Width div 2 - TotalWidth div 2 + IconDelta + PushedDelta, Height div 2 - TextHeight div 2 + PushedDelta - 1, Width div 2 - TotalWidth div 2 + IconDelta + PushedDelta + TextWidth, Height div 2 + TextHeight div 2 + PushedDelta);
    taRightJustify: DestRect := Rect(Width - IconDelta + PushedDelta - TotalWidth - 2, Height div 2 - TextHeight div 2 + PushedDelta - 1, Width - 2, Height div 2 + TextHeight div 2 + PushedDelta);
    end;
  end;

  DrawText(Canvas.Handle, PChar(Caption), Length(Caption), DestRect, Flags);

  if FShowArrow then
  begin
    if Enabled then
      DrawW7Arrow(Canvas, $005B391E, DestRect.Right + 5, Height div 2 - FArrowHeight div 2 + PushedDelta, FArrowType)
    else
      DrawW7Arrow(Canvas, clGray, DestRect.Right + 5, Height div 2 - FArrowHeight div 2 + PushedDelta, FArrowType);
  end;

  if ShowIcon then
    FImages.Draw(Canvas, DestRect.Left - IconDelta, Height div 2 - FIconHeight div 2 + PushedDelta, FImageIndex);
end;


procedure TW7CustomToolButton.Paint;
begin
  inherited;
  Canvas.Lock;
  if (not LeftButtonPressed) and MouseInClient then
  begin
    DrawBorder();
    if FTransparent then
    begin
      //TODO alphablend body draw function
    end
    else
    begin
      DrawGradient(Canvas, $00FEFBF8, $00FAF2ED, Rect(2, 2, Width - 2, Height div 2), True);
      DrawGradient(Canvas, $00F4E4D7, $00E8D2C1, Rect(2, Height div 2, Width - 2, Height - 2), True);
      Canvas.Pixels[2, Height - 3] := $00F0E3D7;
      Canvas.Pixels[Width - 3, Height - 3] := $00F0E3D7;
      Canvas.Pixels[2, 2] := $00FEFCFA;
      Canvas.Pixels[Width - 3, 2] := $00FEFCFA;
    end;
  end
  else if LeftButtonPressed or ReturnButtonPressed then
  begin
    Canvas.Pen.Style := psSolid;
    Canvas.Brush.Style := bsClear;
    Canvas.Pen.Color := $00DBCABB;
    Canvas.RoundRect(0, 0, Width, Height, 4, 4);
    DrawGradient(Canvas, $00F1E8DD, $00F1E4D8, Rect(1, 1, Width - 1, Height div 2), True);
    DrawGradient(Canvas, $00ECDBCF, $00ECDBCF, Rect(1, Height div 2, Width - 1, Height - 1), True);
    Canvas.Pixels[1, Height - 1] := $00E4D0C2;
    Canvas.Pixels[0, Height - 2] := $00E4D0C2;
    Canvas.Pixels[1, Height - 2] := $00DFCABA;
    Canvas.Pixels[Width - 2, Height - 1] := $00E4D0C2;
    Canvas.Pixels[Width - 1, Height - 2] := $00E4D0C2;
    Canvas.Pixels[Width - 2, Height - 2] := $00DFCABA;
    Canvas.Pixels[0, 1] := $00EBDED4;
    Canvas.Pixels[1, 1] := $00DFCFC1;
    Canvas.Pixels[1, 0] := $00EBDED4;
    Canvas.Pixels[Width - 1, 1] := $00EBDED4;
    Canvas.Pixels[Width - 2, 1] := $00DFCFC1;
    Canvas.Pixels[Width - 2, 0] := $00EBDED4;
    Canvas.Pen.Color := $00E4D4C9;
    Canvas.MoveTo(2, 1);
    Canvas.LineTo(Width - 3, 1);
    Canvas.Pen.Color := $00EADED4;
    Canvas.MoveTo(1, 2);
    Canvas.LineTo(Width - 2, 2);
    Canvas.Pen.Color := $00F1E8DD;
    Canvas.MoveTo(1, 3);
    Canvas.LineTo(Width - 2, 3);
  end
  else if Focused then
  begin
    DrawBorder();
  end;
  if csDesigning in ComponentState then
  begin
    Canvas.Brush.Style := bsSolid;
    Canvas.DrawFocusRect(Rect(0, 0, Width, Height));
  end;
  DrawButtonElements;
  Canvas.Unlock;
end;

{procedure TW7CustomToolButton.SetTransparent(Value: boolean);
begin
  Invalidate;
end;}

procedure TW7CustomToolButton.MouseEnter;
begin
  inherited;
end;

procedure TW7CustomToolButton.MouseLeave;
begin
  inherited;
end;

procedure TW7CustomToolButton.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;
  if not (csDestroying in ComponentState) and (AOperation = opRemove) then
  begin
    if (AComponent = FImages) then
    begin
      Images := nil;
      Invalidate;
    end;
  end;
end;

procedure TW7CustomToolButton.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if (Key = VK_RETURN) or (Key = VK_SPACE) then
    ReturnButtonPressed := True;
  if FSetFocusOnClick then
    SetFocus;
  Invalidate;
end;

procedure TW7CustomToolButton.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited;
  ReturnButtonPressed := False;
  Invalidate;
end;

procedure TW7CustomToolButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if FSetFocusOnClick then
    SetFocus;
  inherited;
end;

procedure TW7CustomToolButton.SetShowArrow(Value: boolean);
begin
  FShowArrow := Value;
  Invalidate;
end;

procedure TW7CustomToolButton.SetImages(Value: TCustomImageList);
begin
  if Value <> FImages then
  begin
    if Images <> nil then
      Images.UnRegisterChanges(FImageChangeLink);
    FImages := Value;
    if Images <> nil then
    begin
      Images.RegisterChanges(FImageChangeLink);
      Images.FreeNotification(Self);
    end;
    UpdateImageList;
  end;
end;

procedure TW7CustomToolButton.SetImageIndex(Value: integer);
begin
  FImageIndex := Value;
  UpdateImageList;
end;

procedure TW7CustomToolButton.SetIconSize(Value: TW7ButtonIconSize);
begin
  FIconSize := Value;
  case FIconSize of
    is16px: FIconHeight := 16;
    is24px: FIconHeight := 24;
    is32px: FIconHeight := 32;
    is48px: FIconHeight := 48;
  end;
  UpdateImageList;
end;

procedure TW7CustomToolButton.SetArrowType(Value: TW7ArrowType);
begin
  FArrowType := Value;
  case ArrowType of
    atDown: FArrowHeight := 5;
    atUp: FArrowHeight := 5;
    atLeft: FArrowHeight := 9;
    atRight: FArrowHeight := 9;
  end;
  Invalidate;
end;

procedure TW7CustomToolButton.SetCaptionAlignment(const Value: TAlignment);
begin
  if (FCaptionAlignment <> Value) then
  begin
    FCaptionAlignment := Value;
    Invalidate;
  end;
end;

procedure TW7CustomToolButton.ImageListChange(Sender: TObject);
begin
  if HandleAllocated then
    UpdateImageList;
end;

procedure TW7CustomToolButton.UpdateImageList;
begin
  Invalidate;
end;

/////////////////////////

constructor TW7CustomPageSelector.Create(AOwner: TComponent);
begin
  inherited;
  Height := 34;
  Width := 245;
  TabStop := True;
  ReturnButtonPressed := False;
  Font.Color := $00464646;
  Font.size := 12;
  FSetFocusOnClick := True;
  FShowArrow := False;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
  FIconSize := is24px;
  FIconHeight := 24;
  FGroupIndex := 0;
  FActive := False;
  FPressButtonEffect := False;
  FHideArrowOnActive := True;
  FArrowType := atRight;
  FArrowHeight := 9;
  FBorderColor := $00FBEFE4;
  FHideFocusIfActive := True;
  FStyle := pssClassic;
  ActivateStyle(FStyle);
  FImageIndex := -1;
end;

destructor TW7CustomPageSelector.Destroy;
begin
  FImageChangeLink.Free;
  FImageChangeLink := nil;
  inherited;
end;

procedure TW7CustomPageSelector.DrawFocusedBorder;
var
  X, Y, DX, DY: integer;
begin
  if FHideFocusIfActive and FActive then
    Exit;
  Canvas.Pen.Style := psSolid;
  Canvas.Brush.Style := bsClear;
  if FActive then
    Canvas.Pen.Color := FInternalColors.ActiveOuterFocusFrameColor
  else
    Canvas.Pen.Color := FInternalColors.NotActiveOuterFocusFrameColor;
  X := 0;
  Y := 0;
  DX := Width;
  DY := Height;
  if wsbLeft in FBorders then
    Inc(X);
  if wsbTop in FBorders then
    Inc(Y);
  if wsbRight in FBorders then
    Dec(DX);
  if wsbBottom in FBorders then
    Dec(DY);

  Canvas.Rectangle(X, Y, DX, DY);
  Inc(X);
  Inc(Y);
  Dec(DX);
  Dec(DY);
  if FActive then
    Canvas.Pen.Color := FInternalColors.ActiveInnerFocusFrameColor
  else
    Canvas.Pen.Color := FInternalColors.NotActiveInnerFocusFrameColor;
  Canvas.Rectangle(X, Y, DX, DY);
end;

procedure TW7CustomPageSelector.DrawButtonElements;
var
  DestRect: TRect;
  ArrowDelta: integer;
  Flags: Cardinal;
  IconDelta: integer;
  PushedDelta: integer;
  ShowIcon: boolean;

begin
  ArrowDelta := 0;
  Canvas.Font.Assign(Font);
  if FActive then
    Canvas.Font.Color := FActiveFontColor;
  Canvas.Brush.Style := bsClear;
  Flags := 0;
  Flags := Flags or DT_SINGLELINE or DT_VCENTER or DT_WORD_ELLIPSIS;
  ShowIcon := False;
  case FArrowType of
    atUp, atDown: ArrowDelta := 20;
    atRight, atLeft: ArrowDelta := 16;
  end;
  if (ReturnButtonPressed or LeftButtonPressed) and (FPressButtonEffect) then
    PushedDelta := 1
  else
    PushedDelta := 0;
  IconDelta := 0;


  if Assigned(FImages) and (FImageIndex >= 0) and (FImageIndex < FImages.Count) then
  begin
    IconDelta := FIconHeight + 10;
    ShowIcon := True;
  end;

  Canvas.TextFlags := Flags;
  DestRect := Rect(IconDelta + 6 + PushedDelta, 4 + PushedDelta, Width - 30 + PushedDelta, Height - 5 + PushedDelta);
  DrawText(Canvas.Handle, PChar(Caption), Length(Caption), DestRect, Flags);

  Canvas.Pen.Color := FInternalColors.BorderColor;
  if wsbLeft in FBorders then
  begin
    Canvas.MoveTo(0, 0);
    Canvas.LineTo(0, Height);
  end;
  if wsbTop in FBorders then
  begin
    Canvas.MoveTo(0, 0);
    Canvas.LineTo(Width, 0);
  end;
  if wsbRight in FBorders then
  begin
    Canvas.MoveTo(Width - 1, 0);
    Canvas.LineTo(Width - 1, Height);
  end;
  if wsbBottom in FBorders then
  begin
    Canvas.MoveTo(0, Height - 1);
    Canvas.LineTo(Width, Height - 1);
  end;

  if ShowIcon then
    FImages.Draw(Canvas, 10 + PushedDelta, Height div 2 - FIconHeight div 2 + PushedDelta, FImageIndex);
  if FActive and FHideArrowOnActive then
    Exit;
  if FShowArrow then
    DrawW7Arrow(Canvas, Canvas.Font.Color, Width - ArrowDelta + PushedDelta, Height div 2 - FArrowHeight div 2 + PushedDelta, FArrowType);
end;


procedure TW7CustomPageSelector.Paint;
var
  X, Y, DX, DY: integer;
begin
  inherited;
  Canvas.Lock;
  X := 0;
  Y := 0;
  DX := Width;
  DY := Height;
{  if wsbLeft in FBorders then
    Inc(X);
  if wsbTop in FBorders then
    Inc(Y);
  if wsbRight in FBorders then
    Dec(DX);
  if wsbBottom in FBorders then
    Dec(DY);}
  if FActive then
  begin
    DrawGradient(Canvas, FInternalColors.ActiveTopStartColor, FInternalColors.ActiveTopEndColor, Rect(X, Y, DX, DY div 2), True);
    DrawGradient(Canvas, FInternalColors.ActiveBottomStartColor, FInternalColors.ActiveBottomEndColor, Rect(X, DY div 2, DX, DY), True);
  end
  else
  begin
    if MouseInClient then
    begin
      DrawGradient(Canvas, FInternalColors.MouseInTopStartColor, FInternalColors.MouseInTopEndColor, Rect(X, Y, DX, DY div 2), True);
      DrawGradient(Canvas, FInternalColors.MouseInBottomStartColor, FInternalColors.MouseInBottomEndColor, Rect(X, DY div 2, DX, DY), True);
    end
    else if not MouseInClient then
    begin
      DrawGradient(Canvas, FInternalColors.MouseOutTopStartColor, FInternalColors.MouseOutTopEndColor, Rect(X, Y, DX, DY div 2), True);
      DrawGradient(Canvas, FInternalColors.MouseOutBottomStartColor, FInternalColors.MouseOutBottomEndColor, Rect(X, DY div 2, DX, DY), True);
    end;
  end;
  if Focused then
    DrawFocusedBorder();

  DrawButtonElements;
  Canvas.Unlock;
end;

procedure TW7CustomPageSelector.MouseEnter;
begin
  inherited;
end;

procedure TW7CustomPageSelector.MouseLeave;
begin
  inherited;
end;

procedure TW7CustomPageSelector.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if (Key = VK_RETURN) or (Key = VK_SPACE) then
    ReturnButtonPressed := True;
  Invalidate;
end;

procedure TW7CustomPageSelector.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited;
  ReturnButtonPressed := False;
  if (Key = VK_RETURN) or (Key = VK_SPACE) then
    Active := True;
  if FSetFocusOnClick then
    SetFocus;
  Invalidate;
end;

procedure TW7CustomPageSelector.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
end;

procedure TW7CustomPageSelector.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if Button = mbLeft then
    Active := True;
  if FSetFocusOnClick then
    SetFocus;
  inherited;
end;

procedure TW7CustomPageSelector.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;
  if not (csDestroying in ComponentState) and (AOperation = opRemove) then
  begin
    if (AComponent = FImages) then
    begin
      Images := nil;
      Invalidate;
    end;
  end;
end;

procedure TW7CustomPageSelector.SetShowArrow(Value: boolean);
begin
  FShowArrow := Value;
  Invalidate;
end;

procedure TW7CustomPageSelector.SetImages(Value: TCustomImageList);
begin
  if Value <> FImages then
  begin
    if Images <> nil then
      Images.UnRegisterChanges(FImageChangeLink);
    FImages := Value;
    if Images <> nil then
    begin
      Images.RegisterChanges(FImageChangeLink);
      Images.FreeNotification(Self);
    end;
    UpdateImageList;
  end;
end;

procedure TW7CustomPageSelector.SetImageIndex(Value: integer);
begin
  FImageIndex := Value;
  UpdateImageList;
end;

procedure TW7CustomPageSelector.SetIconSize(Value: TW7ButtonIconSize);
begin
  FIconSize := Value;
  case FIconSize of
    is16px: FIconHeight := 16;
    is24px: FIconHeight := 24;
    is32px: FIconHeight := 32;
    is48px: FIconHeight := 48;
  end;
  UpdateImageList;
end;

procedure TW7CustomPageSelector.SetActive(Value: boolean);
var
  Ind: integer;
begin
  FActive := Value;
  if FActive then
    for Ind := 0 to Parent.ControlCount - 1 do
    begin
      if (Parent.Controls[Ind] <> Self) and (Parent.Controls[Ind] is TW7CustomPageSelector) then
        if TW7CustomPageSelector(Parent.Controls[Ind]).GroupIndex = FGroupIndex then
          TW7CustomPageSelector(Parent.Controls[Ind]).Active := False;
    end;
  Invalidate;
end;

procedure TW7CustomPageSelector.SetArrowType(Value: TW7ArrowType);
begin
  FArrowType := Value;
  case ArrowType of
    atDown: FArrowHeight := 5;
    atUp: FArrowHeight := 5;
    atLeft: FArrowHeight := 9;
    atRight: FArrowHeight := 9;
  end;
  Invalidate;
end;

procedure TW7CustomPageSelector.SetBorders(Value: TW7Borders);
begin
  FBorders := Value;
  Invalidate;
end;

procedure TW7CustomPageSelector.SetBorderColor(Value: TColor);
begin
  FBorderColor := Value;
  Invalidate;
end;

procedure TW7CustomPageSelector.SetStyle(Value: TW7PageSelectorStyle);
begin
  FStyle := Value;
  if FStyle = pssCustom then
    FInternalColors := FColors
  else
    ActivateStyle(Value);
  Invalidate;
end;

procedure TW7CustomPageSelector.SetColors(Value: TW7PageSelectorColors);
begin
  FColors := Value;
  if FStyle = pssCustom then
  begin
    FInternalColors := Value;
    Invalidate;
  end;
end;

procedure TW7CustomPageSelector.ImageListChange(Sender: TObject);
begin
  if HandleAllocated then
    UpdateImageList;
end;

procedure TW7CustomPageSelector.UpdateImageList;
begin
  Invalidate;
end;

procedure TW7CustomPageSelector.ActivateStyle(Style: TW7PageSelectorStyle);
begin
  case Style of
    pssClassic:
    begin
      FInternalColors.MouseOutTopStartColor := $00F9E5D6;
      FInternalColors.MouseOutTopEndColor := $00F9DEC9;
      FInternalColors.MouseOutBottomStartColor := $00F5D1B5;
      FInternalColors.MouseOutBottomEndColor := $00F9DEC9;
      FInternalColors.MouseInTopStartColor := $00BAE9FE;
      FInternalColors.MouseInTopEndColor := $0092DDFF;
      FInternalColors.MouseInBottomStartColor := $003FC4FF;
      FInternalColors.MouseInBottomEndColor := $006ED1FF;
      FInternalColors.ActiveTopStartColor := $00D6EAFA;
      FInternalColors.ActiveTopEndColor := $0069C2F5;
      FInternalColors.ActiveBottomStartColor := $00009BEF;
      FInternalColors.ActiveBottomEndColor := $0000CCFD;
      FInternalColors.NotActiveInnerFocusFrameColor := $00F9F4EF;
      FInternalColors.NotActiveOuterFocusFrameColor := $00F5D1B5;
      FInternalColors.ActiveInnerFocusFrameColor := $00F9F4EF;
      FInternalColors.ActiveOuterFocusFrameColor := $0000B7FF;
      FInternalColors.BorderColor := $00FBEFE4;
      Font.Color := $005B391E;
      ActiveFontColor := $005B391E;
    end;
    pssNavy:
    begin
      FInternalColors.MouseOutTopStartColor := $009F6E4C;
      FInternalColors.MouseOutTopEndColor := $0083532F;
      FInternalColors.MouseOutBottomStartColor := $0071411B;
      FInternalColors.MouseOutBottomEndColor := $00985928;
      FInternalColors.MouseInTopStartColor := $00C88455;
      FInternalColors.MouseInTopEndColor := $00B86F39;
      FInternalColors.MouseInBottomStartColor := $00AF6328;
      FInternalColors.MouseInBottomEndColor := $00E79252;
      FInternalColors.ActiveTopStartColor := $00D6EAFA;
      FInternalColors.ActiveTopEndColor := $0069C2F5;
      FInternalColors.ActiveBottomStartColor := $00009BEF;
      FInternalColors.ActiveBottomEndColor := $0000CCFD;
      FInternalColors.NotActiveInnerFocusFrameColor := $00F5D1B5;
      FInternalColors.NotActiveOuterFocusFrameColor := $00B65B18;
      FInternalColors.ActiveInnerFocusFrameColor := $00F9F4EF;
      FInternalColors.ActiveOuterFocusFrameColor := $0000B7FF;
      FInternalColors.BorderColor := $0071411B;
      Font.Color := $00FAFAFA;
      ActiveFontColor := $001E1E1E;
    end;
    pssCustom: Exit;
  end;
  FColors := FInternalColors;
end;

constructor TW7CustomSpeedButton.Create(AOwner: TComponent);
begin
  inherited;
  Width := 34;
  Height := 27;
  FEnabled := True;
  FLight := False;
  FFlat := False;
  FMouseInOpacity := 0;
  FFadeOutInterval := 40;
  FFadeInInterval := 15;
  FBackground := TBitmap.Create;
  FMouseInBackground := TBitmap.Create;
  FMouseDownBackground := TBitmap.Create;
  FInternalTimer := TTimer.Create(Self);
  FInternalTimer.Enabled := False;
  FInternalTimer.OnTimer := TimerProc;
  FBackgroundsCreated := False;
  FMouseUpped := True;
  Font.Color := $005B391E;
  if GetWindowsVersion >= 6 then
  begin
    Font.Name := W7StandartFontName;
    Font.Size := W7StandartFontSize;
  end;
  FShowArrow := False;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
  FIconSize := is16px;
  FIconHeight := 16;
  FArrowType := atDown;
  FArrowHeight := 5;
  FPressButtonEffect := True;
  FImageIndex := -1;
end;

destructor TW7CustomSpeedButton.Destroy;
begin
  FImageChangeLink.Free;
  FBackground.Destroy;
  FMouseInBackground.Destroy;
  FMouseDownBackground.Destroy;
  inherited;
end;

procedure TW7CustomSpeedButton.DrawMouseInBorder;
begin
  with FMouseInBackground do
  begin
    Canvas.Pen.Color := $0000DBFF;
    Canvas.RoundRect(0, 0, Width, Height, 4, 4);
    Canvas.Pen.Color := $00FFFFFF;
    Canvas.RoundRect(1, 1, Width - 1, Height - 1, 4, 4);
    Canvas.Pixels[1, Height - 1] := $003ED5F7;
    Canvas.Pixels[1, Height - 2] := $00CFF6FF;           //
    Canvas.Pixels[0, Height - 2] := $003ED5F7;
    Canvas.Pixels[Width - 2, Height - 1] := $003ED5F7;
    Canvas.Pixels[Width - 2, Height - 2] := $00CFF6FF; //
    Canvas.Pixels[Width - 1, Height - 2] := $003ED5F7;
    Canvas.Pixels[0, 1] := $003ED5F7;
    Canvas.Pixels[1, 1] := $00BEECFF; //
    Canvas.Pixels[1, 0] := $003ED5F7;
    Canvas.Pixels[Width - 1, 1] := $003ED5F7;
    Canvas.Pixels[Width - 2, 1] := $00BEECFF; //
    Canvas.Pixels[Width - 2, 0] := $003ED5F7;
  end;
end;

procedure TW7CustomSpeedButton.DrawMouseDownBorder;
begin
  with FMouseDownBackground do
  begin
    Canvas.Pen.Color := $00289BC2;
    Canvas.RoundRect(0, 0, Width, Height, 4, 4);
    Canvas.Pixels[1, Height - 1] := $0048A9CA;
    Canvas.Pixels[1, Height - 2] := $005BC3D7;           //
    Canvas.Pixels[0, Height - 2] := $0048A9CA;
    Canvas.Pixels[Width - 2, Height - 1] := $0048A9CA;
    Canvas.Pixels[Width - 2, Height - 2] := $005BC3D7; //
    Canvas.Pixels[Width - 1, Height - 2] := $0048A9CA;
    Canvas.Pixels[0, 1] := $0052ABCB;
    Canvas.Pixels[1, 1] := $005EB3D7; //
    Canvas.Pixels[1, 0] := $0052ABCB;
    Canvas.Pixels[Width - 1, 1] := $0052ABCB;
    Canvas.Pixels[Width - 2, 1] := $005EB3D7; //
    Canvas.Pixels[Width - 2, 0] := $0052ABCB;
  end;
end;

procedure TW7CustomSpeedButton.DrawMouseInSubLight;
var
  Etalon: TBitmap;
  Ind, PosY: integer;
begin
  Etalon := TBitmap.Create;
  Etalon.PixelFormat := pf24Bit;
  Etalon.Width := 3;                                   // 0073D5FF  0069D8FF
  Etalon.Height := 9;
  DrawGradient(Etalon.Canvas, $0069CFFF, $00A4F4FF, Rect(0, 0, 1, Etalon.Height), True);
  DrawGradient(Etalon.Canvas, $0069CFFF, $00EAFFFF, Rect(1, 0, 2, Etalon.Height), True);
  DrawGradient(Etalon.Canvas, $0069CFFF, $00A4F4FF, Rect(2, 0, 3, Etalon.Height), True);
  for Ind := 0 to Etalon.Height - 1 do
  begin
    PosY := Height - 11 + Ind;
    DrawGradient(FMouseInBackground.Canvas, Etalon.Canvas.Pixels[0, Ind], Etalon.Canvas.Pixels[1, Ind], Rect(3, PosY, Width div 2, PosY + 1), False);
    DrawGradient(FMouseInBackground.Canvas, Etalon.Canvas.Pixels[1, Ind], Etalon.Canvas.Pixels[2, Ind], Rect(Width div 2, PosY, Width - 3, PosY + 1), False);
  end;
  Etalon.Destroy;
end;

procedure TW7CustomSpeedButton.DrawMouseDownSubLight;
var
  Etalon: TBitmap;
  Ind, PosY: integer;
begin
  Etalon := TBitmap.Create;
  Etalon.PixelFormat := pf24Bit;                                 //  68BFEA  63C7EB
  Etalon.Width := 3;
  Etalon.Height := 10;
  DrawGradient(Etalon.Canvas, $0058BBF3, $0080DFEA, Rect(0, 0, 1, Etalon.Height), True);
  DrawGradient(Etalon.Canvas, $0058BBF3, $00D0EBE9, Rect(1, 0, 2, Etalon.Height), True);
  DrawGradient(Etalon.Canvas, $0058BBF3, $0080DFEA, Rect(2, 0, 3, Etalon.Height), True);
  for Ind := 0 to Etalon.Height - 1 do
  begin
    PosY := Height - 11 + Ind;
    DrawGradient(FMouseDownBackground.Canvas, Etalon.Canvas.Pixels[0, Ind], Etalon.Canvas.Pixels[1, Ind], Rect(1, PosY, Width div 2, PosY + 1), False);
    DrawGradient(FMouseDownBackground.Canvas, Etalon.Canvas.Pixels[1, Ind], Etalon.Canvas.Pixels[2, Ind], Rect(Width div 2, PosY, Width - 1, PosY + 1), False);
  end;
  Etalon.Destroy;
end;

procedure TW7CustomSpeedButton.DrawFlatMouseInSubLight;
var
  Etalon: TBitmap;
  Ind, PosY: integer;
begin
  Etalon := TBitmap.Create;
  Etalon.PixelFormat := pf24Bit;
  Etalon.Width := 3;                                   // 0073D5FF  0069D8FF
  Etalon.Height := 9;
  DrawGradient(Etalon.Canvas, $0071D3FF, $00A4F4FF, Rect(0, 0, 1, Etalon.Height), True);
  DrawGradient(Etalon.Canvas, $0071D3FF, $00EAFFFF, Rect(1, 0, 2, Etalon.Height), True);
  DrawGradient(Etalon.Canvas, $0071D3FF, $00A4F4FF, Rect(2, 0, 3, Etalon.Height), True);
  for Ind := 0 to Etalon.Height - 1 do
  begin
    PosY := Height - 11 + Ind;
    DrawGradient(FMouseInBackground.Canvas, Etalon.Canvas.Pixels[0, Ind], Etalon.Canvas.Pixels[1, Ind], Rect(3, PosY, Width div 2, PosY + 1), False);
    DrawGradient(FMouseInBackground.Canvas, Etalon.Canvas.Pixels[1, Ind], Etalon.Canvas.Pixels[2, Ind], Rect(Width div 2, PosY, Width - 3, PosY + 1), False);
  end;
  Etalon.Destroy;
end;

procedure TW7CustomSpeedButton.DrawFlatMouseDownSubLight;
var
  Etalon: TBitmap;
  Ind, PosY: integer;
begin
  Etalon := TBitmap.Create;
  Etalon.PixelFormat := pf24Bit;                                 //  68BFEA  63C7EB
  Etalon.Width := 3;
  Etalon.Height := 10;
  DrawGradient(Etalon.Canvas, $0060BBEB, $0080DFEA, Rect(0, 0, 1, Etalon.Height), True);
  DrawGradient(Etalon.Canvas, $0060BBEB, $00D0EBE9, Rect(1, 0, 2, Etalon.Height), True);
  DrawGradient(Etalon.Canvas, $0060BBEB, $0080DFEA, Rect(2, 0, 3, Etalon.Height), True);
  for Ind := 0 to Etalon.Height - 1 do
  begin
    PosY := Height - 11 + Ind;
    DrawGradient(FMouseDownBackground.Canvas, Etalon.Canvas.Pixels[0, Ind], Etalon.Canvas.Pixels[1, Ind], Rect(1, PosY, Width div 2, PosY + 1), False);
    DrawGradient(FMouseDownBackground.Canvas, Etalon.Canvas.Pixels[1, Ind], Etalon.Canvas.Pixels[2, Ind], Rect(Width div 2, PosY, Width - 1, PosY + 1), False);
  end;
  Etalon.Destroy;
end;


procedure TW7CustomSpeedButton.CreateMouseInBody;
begin
  with FMouseInBackground do
  begin
    PixelFormat := pf24Bit;
    Width := Self.Width;
    Height := Self.Height;
    Canvas.Pen.Color := $0069CFFF;
    CAnvas.Brush.Color := $0069CFFF;
    Canvas.Brush.Style := bsSolid;
    Canvas.Rectangle(3, Height div 2, Width - 3, Height - 3);

    Canvas.Brush.Style := bsClear;
    Canvas.Pen.Style := psSolid;
    DrawMouseInBorder;

    DrawGradient(Canvas, $00DFF0FF, $00C2E2FE, Rect(3, 3, Width - 3, Height div 2), True);
    Canvas.Pen.Color := $00E9F4FF;
    Canvas.MoveTo(3, 2);
    Canvas.LineTo(Width - 3, 2);
    Canvas.Pen.Color := $00DBEEFF;
    Canvas.MoveTo(2, 3);
    Canvas.LineTo(2, Height div 2);
    Canvas.Pen.Color := $008DE3FF;
    Canvas.LineTo(2, Height - 3);

    Canvas.Pen.Color := $00DBEEFF;
    Canvas.MoveTo(Width - 3, 3);
    Canvas.LineTo(Width - 3, Height div 2);
    Canvas.Pen.Color := $008DE3FF;
    Canvas.LineTo(Width - 3, Height - 3);

    Canvas.Pixels[2, 2] := $00F0F8FF;
    Canvas.Pixels[Width - 3, 2] := $00F0F8FF;

    Canvas.Pixels[2, Height - 3] := $00ADF5FF;
    Canvas.Pixels[Width - 3, Height - 3] := $00ADF5FF;

    Canvas.Pixels[1, 1] := $00F0F8FF;
    Canvas.Pixels[Width - 2, 1] := $00F0F8FF;

    Canvas.Pixels[1, Height - 2] := $00ADF5FF;
    Canvas.Pixels[Width - 2, Height - 2] := $00ADF5FF;

    DrawMouseInSubLight;
  end;
//DFF0FF
end;

procedure TW7CustomSpeedButton.CreateFlatMouseInBody;
begin
  with FMouseInBackground do
  begin
    PixelFormat := pf24Bit;
    Width := Self.Width;
    Height := Self.Height;
    DrawMouseInBorder;
    Canvas.Brush.Style := bsClear;
    Canvas.Pen.Style := psSolid;
    Canvas.Pen.Color := $00CAEFFF;
    Canvas.MoveTo(2, 1);
    Canvas.LineTo(Width - 2, 1);
    Canvas.Pen.Color := $00CCEFFF;
    Canvas.MoveTo(1, 2);
    Canvas.LineTo(1, Height - 2);
    Canvas.Pen.Color := $00CCEFFF;
    Canvas.MoveTo(Width - 2, 2);
    Canvas.LineTo(Width - 2, Height - 2);
    DrawGradient(Canvas, $0067CEFF, $0071D3FF, Rect(3, 3, Width - 3, Height - 11), True);
    Canvas.Pen.Color := $0077DAFF;
    Canvas.MoveTo(3, 2);
    Canvas.LineTo(Width - 3, 2);
    Canvas.Pen.Color := $0073D5FF;
    Canvas.MoveTo(2, 3);
    Canvas.LineTo(2, Height - 2);
    Canvas.MoveTo(Width - 3, 3);
    Canvas.LineTo(Width - 3, Height - 2);
    Canvas.Pixels[2, 2] :=  $0085E5FF;
    Canvas.Pixels[Width - 3, 2] :=  $0085E5FF;
    DrawFlatMouseInSubLight;
  end;
end;

procedure TW7CustomSpeedButton.CreateFlatMouseDownBody;
begin
  with FMouseDownBackground do
  begin
    PixelFormat := pf24Bit;
    Width := Self.Width;
    Height := Self.Height;
    Canvas.Brush.Style := bsClear;
    Canvas.Pen.Style := psSolid;

    Canvas.Pen.Color := $005BA7D1;
    Canvas.MoveTo(2, 1);
    Canvas.LineTo(Width - 2, 1);

    DrawGradient(Canvas, $005DB5E7, $0060BBEB, Rect(1, 2, Width - 1, Height - 9), True);

    DrawFlatMouseDownSubLight;
    DrawMouseDownBorder;
  end;
end;

procedure TW7CustomSpeedButton.CreateMouseDownBody;
begin
  with FMouseDownBackground do
  begin
    PixelFormat := pf24Bit;
    Width := Self.Width;
    Height := Self.Height;
    Canvas.Brush.Style := bsClear;
    Canvas.Pen.Style := psSolid;

    Canvas.Pen.Color := $0085C1E3;
    Canvas.MoveTo(2, 1);
    Canvas.LineTo(Width - 2, 1);

    DrawGradient(Canvas, $008ECBEE, $0078C7F5, Rect(1, 2, Width - 1, Height div 2), True);
    DrawGradient(Canvas, $0056BBF5, $0060BBEB, Rect(1, Height div 2, Width - 1, Height - 9), True);

    DrawMouseDownSubLight;
    DrawMouseDownBorder;
  end;
end;


procedure TW7CustomSpeedButton.CreateBackgrounds;
begin
  FBackground.Width := Width;
  FBackground.Height := Height;
  with FBackground do
  begin
    PixelFormat := pf24Bit;
    Canvas.Pen.Color := $00FFFFFF;
    Canvas.Brush.Color := $00FFFFFF;
    Canvas.Rectangle(0, 0, Width, Height);
  end;
  if FFlat then
  begin
    CreateFlatMouseInBody;
    CreateFlatMouseDownBody;
  end
  else
  begin
    CreateMouseInBody;
    CreateMouseDownBody;
  end;

end;

procedure TW7CustomSpeedButton.DrawEnabledBorder;
begin
  Canvas.Pen.Style := psSolid;
  Canvas.Brush.Style := bsClear;
  Canvas.Pen.Color := $00AA9787;
  Canvas.RoundRect(0, 0, Width, Height, 4, 4);
  Canvas.Pen.Color := $00FEFAF6;
  Canvas.RoundRect(1, 1, Width - 1, Height - 1, 4, 4);
  Canvas.Pixels[1, Height - 1] := $00BBAA9B;
  Canvas.Pixels[1, Height - 2] := $00E4DED9;           //
  Canvas.Pixels[0, Height - 2] := $00BBAA9B;
  Canvas.Pixels[Width - 2, Height - 1] := $00BBAA9B;
  Canvas.Pixels[Width - 2, Height - 2] := $00E4DED9; //
  Canvas.Pixels[Width - 1, Height - 2] := $00BBAA9B;
  Canvas.Pixels[0, 1] := $00BBAA9B;
  Canvas.Pixels[1, 1] := $00BBAA9B; //
  Canvas.Pixels[1, 0] := $00BBAA9B;
  Canvas.Pixels[Width - 1, 1] := $00BBAA9B;
  Canvas.Pixels[Width - 2, 1] := $00BBAA9B; //
  Canvas.Pixels[Width - 2, 0] := $00BBAA9B;
end;

procedure TW7CustomSpeedButton.DrawDisabledBorder;
begin
  Canvas.Pen.Style := psSolid;
  Canvas.Brush.Style := bsClear;
  Canvas.Pen.Color := $00D1C2B4;
  Canvas.RoundRect(0, 0, Width, Height, 4, 4);

  Canvas.Pixels[1, Height - 1] := $00D9CABD;
  Canvas.Pixels[1, Height - 2] := $00E6D9CD;           //
  Canvas.Pixels[0, Height - 2] := $00D9CABD;
  Canvas.Pixels[Width - 2, Height - 1] := $00D9CABD;
  Canvas.Pixels[Width - 2, Height - 2] := $00E6D9CD; //
  Canvas.Pixels[Width - 1, Height - 2] := $00D9CABD;
  Canvas.Pixels[0, 1] := $00D9CABD;
  Canvas.Pixels[1, 1] := $00E6D9CD; //
  Canvas.Pixels[1, 0] := $00D9CABD;
  Canvas.Pixels[Width - 1, 1] := $00D9CABD;
  Canvas.Pixels[Width - 2, 1] := $00E6D9CD; //
  Canvas.Pixels[Width - 2, 0] := $00D9CABD;
end;

procedure TW7CustomSpeedButton.DrawBorder;
begin
  case FEnabled of
    True: DrawEnabledBorder;
    False: DrawDisabledBorder;
  end;
end;

procedure TW7CustomSpeedButton.HideCorners;
begin
  with FMouseInBackground do
  begin
    Canvas.Pixels[0, 0] := Self.Canvas.Pixels[0, 0];
    Canvas.Pixels[0, Height - 1] := Self.Canvas.Pixels[0, Height - 1];
    Canvas.Pixels[Width - 1, 0] := Self.Canvas.Pixels[Width - 1, 0];
    Canvas.Pixels[Width - 1, Height - 1] := Self.Canvas.Pixels[Width - 1, Height - 1];
  end;
  with FMouseDownBackground do
  begin
    Canvas.Pixels[0, 0] := Self.Canvas.Pixels[0, 0];
    Canvas.Pixels[0, Height - 1] := Self.Canvas.Pixels[0, Height - 1];
    Canvas.Pixels[Width - 1, 0] := Self.Canvas.Pixels[Width - 1, 0];
    Canvas.Pixels[Width - 1, Height - 1] := Self.Canvas.Pixels[Width - 1, Height - 1];
  end;
end;

procedure TW7CustomSpeedButton.DrawStandartBackground;
begin
  if (not LeftButtonPressed) and FMouseUpped then
  begin
    DrawAlphaGradient(Canvas, FBackground, 180, 70, Rect(1, 1, Width - 1, Height div 2), True);
    DrawAlphaGradient(Canvas, FBackground, 20, 10, Rect(1, Height div 2, Width - 1, Height - 1), True);
  end
  else
    Canvas.Draw(0, 0, FMouseDownBackground);
end;

procedure TW7CustomSpeedButton.DrawLightBackground;
begin
  if (not LeftButtonPressed) and FMouseUpped then
  begin
    DrawAlphaGradient(Canvas, FBackground, 230, 180, Rect(1, 1, Width - 1, Height div 2), True);
    DrawAlphaGradient(Canvas, FBackground, 125, 150, Rect(1, Height div 2, Width - 1, Height - 1), True);
  end
  else
    Canvas.Draw(0, 0, FMouseDownBackground);

end;

procedure TW7CustomSpeedButton.DrawFlatBackground;
begin
  if (not LeftButtonPressed) and FMouseUpped then
    AlphaBlendBitmap(FBackground, Canvas, Rect(1, 1, Width - 1, Height - 1), 20)
  else
    Canvas.Draw(0, 0, FMouseDownBackground);
end;

procedure TW7CustomSpeedButton.DrawDisabledBackground;
begin
  AlphaBlendBitmap(FBackground, Canvas, Rect(1, 1, Width - 1, Height - 1), 10);
end;

procedure TW7CustomSpeedButton.DrawBackground;
begin
  HideCorners;
  if not FEnabled then
    DrawDisabledBackground
  else if FFlat then
    DrawFlatBackground
  else if FLight then
    DrawLightBackground
  else
    DrawStandartBackground;
end;

procedure TW7CustomSpeedButton.DrawMouseInBody;
begin
  if ((FMouseInOpacity > 0) and (FMouseInOpacity < 255)) or (MouseInControl) then
    AlphaBlendBitmap(FMouseInBackground, Canvas, Rect(0, 0, Width, Height), FMouseInOpacity);
end;

procedure TW7CustomSpeedButton.DrawButtonElements;
var
  DestRect: TRect;
  Flags: Cardinal;
  DropDownDelta: integer;
  TextWidth: integer;
  TextHeight: integer;
  IconDelta: integer;
  PushedDelta: integer;
  Delta: integer;
  TotalWidth: integer;
  ShowIcon: boolean;
begin
  Canvas.Font.Assign(Font);

  if not Enabled then
    Canvas.Font.Color := clGray;

  Canvas.Brush.Style := bsClear;
  Flags := 0;
  Flags := Flags or DT_CENTER or DT_SINGLELINE or DT_VCENTER or DT_WORD_ELLIPSIS;
  ShowIcon := False;
  DropDownDelta := 0;
  if FShowArrow then
  begin
    case FArrowType of
      atUp, atDown: DropDownDelta := 14;
      atRight, atLeft: DropDownDelta := 10;
    end;
  end;

  if (LeftButtonPressed) and (FPressButtonEffect) then
    PushedDelta := 1
  else
    PushedDelta := 0;

  IconDelta := 0;
  if Assigned(FImages) and (FImageIndex >= 0) and (FImageIndex < FImages.Count) then
  begin
    IconDelta := FIconHeight + 5;
    ShowIcon := True;
  end;

  Delta := 3;

  Canvas.TextFlags := Flags;
  TextWidth := Canvas.TextWidth(Caption);
  TextHeight := Canvas.TextHeight(Caption);
  TotalWidth := TextWidth + DropDownDelta + IconDelta;

  if (TotalWidth) > (Width - Delta * 2) then
  begin
    DestRect := Rect(IconDelta + Delta + PushedDelta, Height div 2 - TextHeight div 2 + PushedDelta, Width - 1 - Delta + PushedDelta - DropDownDelta, Height div 2 + TextHeight div 2 + PushedDelta + 1);
  end
  else
  begin
    DestRect := Rect(Width div 2 - TotalWidth div 2 + IconDelta + PushedDelta, Height div 2 - TextHeight div 2 + PushedDelta, Width div 2 - TotalWidth div 2 + IconDelta + PushedDelta + TextWidth, Height div 2 + TextHeight div 2 + PushedDelta + 1);
  end;

  DrawText(Canvas.Handle, PChar(Caption), Length(Caption), DestRect, Flags);
  if FShowArrow then
    DrawW7Arrow(Canvas, $005B391E, DestRect.Right + 5, Height div 2 - FArrowHeight div 2 + PushedDelta + 1, FArrowType);

  if ShowIcon then
    FImages.Draw(Canvas, DestRect.Left - IconDelta, Height div 2 - FIconHeight div 2 + PushedDelta + 1, FImageIndex);
end;

procedure TW7CustomSpeedButton.Paint;
begin
  inherited;
  Canvas.Lock;
  if (not FBackgroundsCreated) or (Width <> FBackground.Width) or (Height <> FBackground.Height) then
  begin
    CreateBackgrounds;
    FBackgroundsCreated := True;
  end;
  if (not LeftButtonPressed) or (not MouseInControl) then
    DrawBorder;
  DrawBackground;
  if (not LeftButtonPressed) or (not MouseInControl) then
    DrawMouseInBody;


  DrawButtonElements;
  Canvas.Unlock;
end;

procedure TW7CustomSpeedButton.TimerProc(Sender: TObject);
begin
  if MouseInControl then
    Inc(FMouseInOpacity, 25)
  else
    Dec(FMouseInOpacity, 15);
  if (FMouseInOpacity <= 0) or (FMouseInOpacity >= 255) then
  begin
    FInternalTimer.Enabled := False;
    if FMouseInOpacity > 255 then
      FMouseInOpacity := 255;
    FMouseUpped := True;
  end;
  Invalidate;
end;

procedure TW7CustomSpeedButton.MouseEnter;
begin

  if FEnabled then
  begin
    if FadeInInterval > 0 then
    begin
      if FMouseInOpacity <= 0 then
        FMouseInOpacity := 1;
      FMouseUpped := True;
      FInternalTimer.Interval := FFadeInInterval;
      FInternalTimer.Enabled := True;
    end
    else
    begin
      FMouseUpped := True;
      FMouseInOpacity := 255;
      Invalidate;
    end;
  end;

  inherited;
end;

procedure TW7CustomSpeedButton.MouseLeave;
begin
  if FEnabled then
  begin
    if FadeOutInterval > 0 then
    begin
      if (FMouseInOpacity >= 255) or (not FMouseUpped) then
        FMouseInOpacity := 254;
      FInternalTimer.Interval := FFadeOutInterval;
      FInternalTimer.Enabled := True;
      FMouseUpped := True;
    end
    else
    begin
      FMouseInOpacity := 1;
      Invalidate;
      FMouseUpped := True;
    end;
  end;

  inherited;
end;

procedure TW7CustomSpeedButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  FMouseInOpacity := 0;
  FInternalTimer.Enabled := False;
  FMouseUpped := False;
end;

procedure TW7CustomSpeedButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  if FMouseInOpacity <= 0 then
    FMouseInOpacity := 1;
  if MouseInControl then
    FMouseUpped := False;
  FInternalTimer.Interval := FFadeInInterval;
  FInternalTimer.Enabled := True;
end;

procedure TW7CustomSpeedButton.SetEnabled(Value: Boolean);
begin
  inherited;
  FEnabled := Value;
  CreateBackgrounds;
end;

procedure TW7CustomSpeedButton.SetFlat(Value: boolean);
begin
  FFlat := Value;
  CreateBackgrounds;
  Invalidate;
end;

procedure TW7CustomSpeedButton.SetLight(Value: boolean);
begin
  FLight := Value;
  if FLight = True then
    FFlat := False;
  CreateBackgrounds;
  Invalidate;
end;

procedure TW7CustomSpeedButton.SetShowArrow(Value: boolean);
begin
  FShowArrow := Value;
  Invalidate;
end;

procedure TW7CustomSpeedButton.SetImages(Value: TCustomImageList);
begin
  if Value <> FImages then
  begin
    if Images <> nil then
      Images.UnRegisterChanges(FImageChangeLink);
    FImages := Value;
    if Images <> nil then
    begin
      Images.RegisterChanges(FImageChangeLink);
      Images.FreeNotification(Self);
    end;
    UpdateImageList;
  end;
end;

procedure TW7CustomSpeedButton.SetImageIndex(Value: integer);
begin
  FImageIndex := Value;
  UpdateImageList;
end;

procedure TW7CustomSpeedButton.SetIconSize(Value: TW7ButtonIconSize);
begin
  FIconSize := Value;
  case FIconSize of
    is16px: FIconHeight := 16;
    is24px: FIconHeight := 24;
    is32px: FIconHeight := 32;
    is48px: FIconHeight := 48;
  end;
  UpdateImageList;
end;

procedure TW7CustomSpeedButton.SetArrowType(Value: TW7ArrowType);
begin
  FArrowType := Value;
  case ArrowType of
    atDown: FArrowHeight := 5;
    atUp: FArrowHeight := 5;
    atLeft: FArrowHeight := 9;
    atRight: FArrowHeight := 9;
  end;
  Invalidate;
end;

procedure TW7CustomSpeedButton.ImageListChange(Sender: TObject);
begin
  UpdateImageList;
end;

procedure TW7CustomSpeedButton.UpdateImageList;
begin
  Invalidate;
end;

end.