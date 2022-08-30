{***************************************************************************}
{ TAdvGraphicCheckLabel component                                           }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2014                                               }
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
unit AdvGraphicCheckLabel;

{$I TMSDEFS.INC}

interface

{$R AdvGraphicCheckLabel.res}

uses
  Windows, Classes, Messages, Controls, Forms, Dialogs, SysUtils, AdvGDIP, Graphics;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 1; // Build nr.

  // version history
  // v1.0.0.0 : First release
  // v1.0.0.1 : Fixed : Issue with runtime enabling/disabling control

type

  TLabelSettings =  class(TPersistent)
  private
    FPicture: TAdvGdipPicture;
    FText: string;
    FFont: TFont;
    FHoverPicture: TAdvGdipPicture;
    FOnChange: TNotifyEvent;
    FPictureColor: TColor;
    FHoverFontColor: TColor;
    FHoverFontStyle: TFontStyles;
    procedure SetPicture(const Value: TAdvGdipPicture);
    procedure SetText(const Value: string);
    procedure SetFont(const Value: TFont);
    procedure SetHoverPicture(const Value: TAdvGdipPicture);
    procedure SetPictureColor(const Value: TColor);
  protected
    procedure FontChanged(Sender: TObject);
    procedure Changed;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property HoverFontStyle: TFontStyles read FHoverFontStyle write FHoverFontStyle;
    property HoverFontColor: TColor read FHoverFontColor write FHoverFontColor default clNone;
    property HoverPicture: TAdvGdipPicture read FHoverPicture write SetHoverPicture;
    property Font: TFont read FFont write SetFont;
    property Picture: TAdvGdipPicture read FPicture write SetPicture;
    property PictureColor: TColor read FPictureColor write SetPictureColor;
    property Text: string read FText write SetText;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TCheckKind = (ckCheck, ckLike, ckAvailable, ckWish, ckBookmark, ckFavorite, ckInCart, ckFollowing, ckCustom);

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvGraphicCheckLabel = class(TCustomControl)
  private
    FStateChecked: TLabelSettings;
    FStateUnChecked: TLabelSettings;
    FChecked: boolean;
    FHasMouse: boolean;
    FOldCursor: TCursor;
    FKind: TCheckKind;
    FDesignTime: boolean;
    procedure WMLButtonUp(var Message: TWMLButtonUp); message WM_LBUTTONUP;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure SetStateChecked(const Value: TLabelSettings);
    procedure SetStateUnChecked(const Value: TLabelSettings);
    procedure SetChecked(const Value: boolean);
    procedure SetKind(const Value: TCheckKind);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
  protected
    function GetActiveRect(Canvas: TCanvas; ASettings: TLabelSettings): TRect; virtual;
    function GetTextRect: TRect; virtual;
    procedure LoadPictureSet(AName, CaptionNormal, CaptionActive: string);
    procedure LoadPredefined;
    procedure SettingsChanged(Sender: TObject);
    procedure KeyPress(var Key: Char); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure DoDraw(Canvas: TCanvas; ASettings: TLabelSettings); virtual;
    procedure Paint; override;
    procedure DoEnter; override;
    procedure DoExit; override;
    function GetVersionNr: Integer; virtual;
    procedure Toggle; virtual;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Align;
    property Anchors;
    property Checked: boolean read FChecked write SetChecked default false;
    property Enabled;
    property Hint;

    property Kind: TCheckKind read FKind write SetKind default ckCheck;

    {$IFDEF DELPHIXE_LVL}
    property Margins;
    property Padding;
    {$ENDIF}
    property PopupMenu;

    property ShowHint;
    property StateChecked: TLabelSettings read FStateChecked write SetStateChecked;
    property StateUnChecked: TLabelSettings read FStateUnChecked write SetStateUnChecked;

    property TabOrder;
    property TabStop;
    property Version: string read GetVersion write SetVersion;

    property Visible;

    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnExit;
    property OnEndDrag;
    property OnEnter;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseDown;
    {$IFDEF DELPHIXE_LVL}
    property OnMouseEnter;
    property OnMouseLeave;
    {$ENDIF}
    property OnResize;
  end;


implementation

uses
  Math, ActiveX;

procedure DrawPictureWithColor(g: TGPGraphics; Pic: TAdvGdipPicture; PicClr: TGPColor; DR: TRect);
var
  Attr: TGPImageAttributes;
  ColorMatrix: TColorMatrix;
  Img: TGPImage;
  pstm: IStream;
  hGlobal: THandle;
  pcbWrite: Longint;
  ms: TMemoryStream;
  RF: TGPRectF;
  CR: TRect;
  s: Integer;
  rc, gc, bc: Double;
  hr: HResult;

begin
  CR := DR;

  //InflateRect(CR, -2, -2);

  s := Min(CR.Right - CR.Left, CR.Bottom - CR.Top);
  RF := MakeRect(CR.Left + (CR.Right - CR.Left - s) div 2, CR.Top + (CR.Bottom - CR.Top - s) div 2, s, s);

  if not Assigned(Pic) or Pic.Empty then
    Exit;

  ms := TMemoryStream.Create;
  pic.SaveToStream(ms);
  hGlobal := GlobalAlloc(GMEM_MOVEABLE, ms.Size);
  if (hGlobal = 0) then
  begin
    ms.Free;
    raise Exception.Create('Could not allocate memory for image');
  end;

  pstm := nil;
  pcbWrite := 0;
  // Create IStream* from global memory

  hr := CreateStreamOnHGlobal(hGlobal, TRUE, pstm);

  if hr = S_OK then
  begin
    pstm.Write(ms.Memory, ms.Size,@pcbWrite);

    if (ms.Size = pcbWrite) then
    begin
      Img := TGPImage.Create(pstm);

      rc := ADVGDIP.GetRed(PicClr) / 255;
      gc := ADVGDIP.GetGreen(PicClr)/ 255;
      bc := ADVGDIP.GetBlue(PicClr)/ 255;

      // transformed image color
      FillChar(ColorMatrix, Sizeof(ColorMatrix), 0);

      ColorMatrix[3,3] := 1; // <- original A

      ColorMatrix[4,0] := rc; // <- desired R
      ColorMatrix[4,1] := gc; // <- desired G
      ColorMatrix[4,2] := bc; // <- desired B

      Attr := TGPImageAttributes.Create;
      Attr.SetColorMatrix(ColorMatrix);

      RF.X := DR.Left;
      RF.Y := DR.Top;
      RF.Width := Img.Width;
      RF.Height := Img.Height;

      g.DrawImage(img, RF, 0, 0, integer(Img.Width), integer(Img.Height), UnitPixel, Attr);

      Img.Free;
      attr.Free;
    end;
    pstm := nil;
  end
  else
    GlobalFree(hGlobal);

  ms.Free;
end;


{ TLabelSettings }

procedure TLabelSettings.Assign(Source: TPersistent);
begin
  if (Source is TLabelSettings) then
  begin
    FText := (Source as TLabelSettings).Text;
    FPicture.Assign((Source as TLabelSettings).Picture);
    FPictureColor := (Source as TLabelSettings).PictureColor;
    FFont.Assign((Source as TLabelSettings).Font);
    FHoverFontStyle := (Source as TLabelSettings).HoverFontStyle;
    FHoverFontColor := (Source as TLabelSettings).HoverFontColor;
  end;
end;

procedure TLabelSettings.Changed;
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

constructor TLabelSettings.Create;
begin
  inherited;
  FText := '';
  FPicture := TAdvGdipPicture.Create;
  FPicture.OnChange := FontChanged;
  FFont := TFont.Create;
  FFont.OnChange := FontChanged;
  FHoverFontStyle := [];
  FHoverFontColor := clNone;
  FHoverPicture := TAdvGdipPicture.Create;
  FHoverPicture.OnChange := FontChanged;
  FPictureColor := clNone;
end;

destructor TLabelSettings.Destroy;
begin
  FFont.Free;
  FPicture.Free;
  FHoverPicture.Free;
  inherited;
end;

procedure TLabelSettings.FontChanged(Sender: TObject);
begin
  Changed;
end;

procedure TLabelSettings.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
  Changed;
end;

procedure TLabelSettings.SetHoverPicture(const Value: TAdvGdipPicture);
begin
  FHoverPicture.Assign(Value);
  Changed;
end;

procedure TLabelSettings.SetPicture(const Value: TAdvGdipPicture);
begin
  FPicture.Assign(Value);
  Changed;
end;

procedure TLabelSettings.SetPictureColor(const Value: TColor);
begin
  if (FPictureColor <> Value) then
  begin
    FPictureColor := Value;
    Changed;
  end;
end;

procedure TLabelSettings.SetText(const Value: string);
begin
  if (FText <> Value) then
  begin
    FText := Value;
    Changed;
  end;
end;

{ TAdvGraphicCheckLabel }

procedure TAdvGraphicCheckLabel.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
end;

procedure TAdvGraphicCheckLabel.CMMouseEnter(var Message: TMessage);
begin
  inherited;
end;

procedure TAdvGraphicCheckLabel.CMMouseLeave(var Message: TMessage);
begin
  inherited;

  if FHasMouse then
  begin
    FHasMouse := false;
    Invalidate;
    Cursor := FOldCursor;
  end;
end;

constructor TAdvGraphicCheckLabel.Create(AOwner: TComponent);
begin
  inherited;
  FStateChecked := TLabelSettings.Create;
  FStateChecked.OnChange := SettingsChanged;
  FStateUnChecked := TLabelSettings.Create;
  FStateUnChecked.OnChange := SettingsChanged;
  FHasMouse := false;
  FChecked := false;
  Height := 20;
  Width := 80;
  LoadPictureSet('CHECK', '','');

  FDesignTime := (csDesigning in ComponentState) and not
      ((csReading in Owner.ComponentState) or (csLoading in Owner.ComponentState));
end;

procedure TAdvGraphicCheckLabel.CreateParams(var Params: TCreateParams);
begin
  inherited;

end;

procedure TAdvGraphicCheckLabel.CreateWnd;
begin
  inherited;
  if FDesignTime then
    FStateUnChecked.Text := Name;
end;

destructor TAdvGraphicCheckLabel.Destroy;
begin
  FStateChecked.Free;
  FStateUnChecked.Free;
  inherited;
end;

procedure TAdvGraphicCheckLabel.DoDraw(Canvas: TCanvas; ASettings: TLabelSettings);
var
  R: TRect;
  pic: TAdvGdipPicture;
  y: integer;
  gp: TGPGraphics;
  col: TGPColor;
begin
  R := GetClientRect;

  pic := ASettings.Picture;
  if FHasMouse and Assigned(ASettings.HoverPicture) and not ASettings.HoverPicture.Empty then
    pic := ASettings.HoverPicture;

  if Assigned(pic) and not pic.Empty then
  begin
    y := (r.Bottom - r.Top - pic.Height) div 2;
    // do color transform here?

    if ASettings.PictureColor <> clNone then
    begin
      gp := TGPGraphics.Create(Canvas.Handle);
      col := MakeColor(0,ColorToRGB(ASettings.PictureColor));
      DrawPictureWithColor(gp,pic,col,Rect(0,y,pic.Width,y + pic.Height));
      gp.Free;
    end
    else
    begin
      Canvas.Draw(0,y,pic);
    end;

    R.Left := R.Left + pic.Width + 4;
  end;

  Canvas.Font.Assign(ASettings.Font);
  Canvas.Font.Color := ASettings.Font.Color;

  if FHasMouse then
  begin
    Canvas.Font.Style := ASettings.HoverFontStyle;
    if ASettings.HoverFontColor <> clNone then
      Canvas.Font.Color := ASettings.HoverFontColor;
  end;

  Canvas.Brush.Style := bsClear;

  DrawText(Canvas.Handle, PChar(ASettings.Text),Length(ASettings.Text), R, DT_LEFT or DT_VCENTER or DT_SINGLELINE);

  if GetFocus = Handle then
  begin
    DrawFocusRect(Canvas.Handle, R);
  end;
end;

procedure TAdvGraphicCheckLabel.DoEnter;
begin
  inherited;
  Invalidate;
end;

procedure TAdvGraphicCheckLabel.DoExit;
begin
  inherited;
  Invalidate;
end;

function TAdvGraphicCheckLabel.GetActiveRect(Canvas: TCanvas;
  ASettings: TLabelSettings): TRect;
var
  wp,h,hp,ht: integer;
  R: TRect;
begin
  Canvas.Font.Assign(ASettings.Font);

  R := ClientRect;
  h := R.Bottom - R.Top;
  hp := 0;
  wp := 0;

  if Assigned(ASettings.Picture) and not ASettings.Picture.Empty then
  begin
    hp := ASettings.Picture.Height;
    wp := ASettings.Picture.Width + 4;
  end;

  R.Bottom := 0;

  ht := DrawText(Canvas.Handle, PChar(ASettings.Text),Length(ASettings.Text), R, DT_LEFT or DT_VCENTER or DT_SINGLELINE or DT_CALCRECT);

  ht := Max(ht,hp);
  ht := (h - ht) div 2;

  R.Bottom := R.Top + h - ht;
  R.Top := R.Top + ht;
  R.Right := R.Right + wp;
  Result := R;
end;

function TAdvGraphicCheckLabel.GetTextRect: TRect;
begin
  if Checked then
    Result := GetActiveRect(Canvas, FStateChecked)
  else
    Result := GetActiveRect(Canvas, FStateUnChecked);
end;

function TAdvGraphicCheckLabel.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn))) + '.' + IntToStr(Lo(Hiword(vn))) + '.' + IntToStr(Hi(Loword(vn))) + '.' + IntToStr(Lo(Loword(vn)));
end;

function TAdvGraphicCheckLabel.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER, REL_VER), MakeWord(MIN_VER, MAJ_VER));
end;

procedure TAdvGraphicCheckLabel.KeyPress(var Key: Char);
begin
  inherited;
  if Enabled and (Key = #32) then
  begin
    Toggle;
    Click;
  end;
end;

procedure TAdvGraphicCheckLabel.Loaded;
begin
  inherited;
  if (FStateChecked.Text = '') then
    FStateChecked.Text := FStateUnChecked.Text;
end;

procedure TAdvGraphicCheckLabel.LoadPictureSet(AName: string; CaptionNormal, CaptionActive: string);
begin
  FStateUnChecked.Picture.LoadFromResourceName(Hinstance, 'GC_' + AName);
  FStateChecked.Picture.LoadFromResourceName(Hinstance, 'GC_' + AName+'_ACTIVE');
  if (CaptionNormal <> '') then
    FStateUnChecked.Text := CaptionNormal;
  if (CaptionActive <> '') then
    FStateChecked.Text := CaptionActive;
end;

procedure TAdvGraphicCheckLabel.LoadPredefined;
begin
  case Kind of
    ckCheck: LoadPictureSet('CHECK', '','');
    ckLike: LoadPictureSet('LIKE','Like','Unlike');
    ckAvailable: LoadPictureSet('AVAILABLE','Not available','Available');
    ckWish: LoadPictureSet('WISH','Add to wishlisht','In wishlist');
    ckBookmark: LoadPictureSet('BOOKMARK','Bookmark','Bookmarked');
    ckFavorite: LoadPictureSet('FAVORITE','Favorite','Favorited');
    ckInCart: LoadPictureSet('INCART','Add to cart','In cart');
    ckFollowing: LoadPictureSet('FOLLOWING','Follow','Following');
  end;
end;

procedure TAdvGraphicCheckLabel.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  R: TRect;
begin
  inherited;

  R := GetTextRect;

  if PtInRect(R, Point(X,Y))then
  begin
    if not FHasMouse then
    begin
      FHasMouse := true;
      Invalidate;
      FOldCursor := Cursor;
      Cursor := crHandPoint;
    end;
  end
  else
  begin
    if FHasMouse then
    begin
      FHasMouse := false;
      Invalidate;
      Cursor := FOldCursor;
    end;
  end;
end;

procedure TAdvGraphicCheckLabel.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
end;

procedure TAdvGraphicCheckLabel.Paint;
begin
  inherited;
  if Checked then
    DoDraw(Canvas, FStateChecked)
  else
    DoDraw(Canvas, FStateUnChecked);
end;

procedure TAdvGraphicCheckLabel.SetChecked(const Value: boolean);
begin
  if (FChecked <> Value) then
  begin
    FChecked := Value;
    Invalidate;
  end;
end;

procedure TAdvGraphicCheckLabel.SetKind(const Value: TCheckKind);
begin
  if (FKind <> Value) then
  begin
    FKind := Value;

    if not ((csLoading in ComponentState) or (csReading in ComponentState)) then
    begin
      LoadPredefined;
    end;
  end;
end;

procedure TAdvGraphicCheckLabel.SetStateChecked(const Value: TLabelSettings);
begin
  FStateChecked.Assign(Value);
end;

procedure TAdvGraphicCheckLabel.SetStateUnChecked(const Value: TLabelSettings);
begin
  FStateUnChecked.Assign(Value);
end;

procedure TAdvGraphicCheckLabel.SettingsChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TAdvGraphicCheckLabel.SetVersion(const Value: string);
begin
  // read only streamed property
end;

procedure TAdvGraphicCheckLabel.Toggle;
begin
  Checked := not Checked;
end;

procedure TAdvGraphicCheckLabel.WMLButtonUp(var Message: TWMLButtonUp);
var
  R: TRect;
begin
  R := GetTextRect;
  if Enabled and (PtInRect(R, Point(Message.XPos, Message.YPos))) then
    Toggle;
  inherited;
end;

end.
