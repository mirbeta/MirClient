{***************************************************************************}
{ TAeroLabel component                                                      }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2010 - 2011                                        }
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

unit AeroLabel;

{$I TMSDEFS.INC}

interface

uses
  Classes, Windows, Forms, Dialogs, Controls, Graphics, Messages, ExtCtrls, ImgList, Types
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.

  // version history
  // v1.0.0.0 : First release
  // v1.1.0.0 : New : GlowSize property added

type
  TImagePos = (ipLeft, ipRight, ipTop, ipBottom);

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAeroLabel = class(TCustomControl)
  private
    FOnMouseLeave: TNotifyEvent;
    FOnMouseEnter: TNotifyEvent;
    FIsAeroVista: Boolean;
    FImages: TCustomImageList;
    FImageIndex: TImageIndex;
    FImagePos: TImagePos;
    FSpacing: Integer;
    FGlowSize: Integer;
    procedure WMEraseBkGnd(var Message: TWMEraseBkGnd); message WM_ERASEBKGND;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    procedure SetImageIndex(const Value: TImageIndex);
    procedure SetImages(const Value: TCustomImageList);
    procedure SetImagePos(const Value: TImagePos);
    procedure SetSpacing(const Value: Integer);
    procedure SetGlowSize(const Value: Integer);
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure Paint; override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetVersionNr: Integer; virtual;
  published
    property Anchors;
    property BiDiMode;
    property Caption;
    property Constraints;
    property Enabled;
    property Font;
    property GlowSize: Integer read FGlowSize write SetGlowSize default 12;   // 0-15
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex default -1;
    property Images: TCustomImageList read FImages write SetImages;
    property ImagePos: TImagePos read FImagePos write SetImagePos default ipLeft;
    property ParentFont;
    property ParentShowHint;
    property ParentBiDiMode;
    property PopupMenu;
    property ShowHint;
    property Spacing: Integer read FSpacing write SetSpacing default 4;
    property Version: string read GetVersion write SetVersion;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  end;

implementation

uses
  ActiveX, Themes, AdvDWM, AdvTBXPVS, AdvGDIP, SysUtils, Math;

{$IFDEF DELPHIXE2_LVL}
function ThemeServices: TCustomStyleServices;
begin
  Result := StyleServices;
end;

function ThemeServicesThemesEnabled: boolean;
begin
  Result := StyleServices.Enabled;
end;
{$ENDIF}

{$IFNDEF DELPHIXE2_LVL}
function ThemeServicesThemesEnabled: boolean;
begin
  Result := ThemeServices.ThemesEnabled;
end;
{$ENDIF}


//------------------------------------------------------------------------------

procedure DrawGDIPImageFromImageList(gr: TGPGraphics; Canvas: TCanvas; P: TPoint; Images: TCustomImageList; ImageIndex: Integer; Enable: Boolean);
var
  Img: TGPImage;
  pstm: IStream;
  hGlobal: THandle;
  pcbWrite: Longint;
  ms: TMemoryStream;
  ImageAttributes: TGPImageAttributes;
  r, g, b: byte;
  GPBmp: TGPBitmap;
  Aclr: TGPColor;
  bmp: TBitmap;
  graphics: TGPGraphics;
  hr: HResult;
begin
  if not Assigned(Images) or (ImageIndex < 0) or (not Assigned(gr) and not Assigned(Canvas)) then
    Exit;

  graphics := gr;
  if not Assigned(graphics) then
  begin
    graphics := TGPGraphics.Create(Canvas.Handle);
    graphics.SetSmoothingMode(SmoothingModeAntiAlias);
  end;

  bmp := TBitmap.Create;
  try
    bmp.Width := Images.Width;
    bmp.Height := Images.Height;
    //bmp.Canvas.Brush.Color := clFuchsia;
    //bmp.Canvas.FillRect(Rect(0, 0, bmp.Width, bmp.Height));
    Images.Draw(bmp.Canvas, 0, 0, ImageIndex, Enable);

    ms := TMemoryStream.Create;
    bmp.SaveToStream(ms);
  finally
    bmp.Free;
  end;

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
    pstm.Write(ms.Memory, ms.Size, @pcbWrite);

    if (ms.Size = pcbWrite) then
    begin
      Img := TGPImage.Create(pstm);

      GPBmp := TGPBitmap.Create(pstm);
      GPBmp.GetPixel(0, Img.GetHeight - 1, AClr);
      GPBmp.Free;

      r := ADVGDIP.GetRed(AClr);
      g := ADVGDIP.GetGreen(AClr);
      b := ADVGDIP.GetBlue(AClr);

      ImageAttributes := TGPImageAttributes.Create;
      ImageAttributes.SetColorKey(MakeColor(r, g, b), MakeColor(r, g, b), ColorAdjustTypeDefault);
      graphics.DrawImage(Img, MakeRect(P.X, P.Y, Img.GetWidth, Img.Getheight),  // destination rectangle
        0, 0,        // upper-left corner of source rectangle
        Img.GetWidth,       // width of source rectangle
        Img.GetHeight,      // height of source rectangle
        UnitPixel,
        ImageAttributes);

      ImageAttributes.Free;
      Img.Free;
    end;
    pstm := nil;
  end
  else
    GlobalFree(hGlobal);

  ms.Free;

  if not Assigned(gr) then
    graphics.Free;
end;

//------------------------------------------------------------------------------

procedure DrawBlurredText(WinCtrl: TWinControl; Canvas: TCanvas; Text: string; R, TextR: TRect; Align: TAlignment; GlowSize: Integer; BackGround: TColor = clNone);
  procedure DoDrawThemeTextEx(DC: HDC; const Text: string; TextLen: Integer;
    var TextRect: TRect; HTheme: THandle; TextFlags: Cardinal);
  var
    Options: TDTTOpts;
  begin
    FillChar(Options, SizeOf(Options), 0);
    Options.dwSize := SizeOf(Options);
    Options.dwFlags := DTT_TEXTCOLOR or DTT_COMPOSITED or DTT_GLOWSIZE {or DTT_SHADOWTYPE};
    Options.iGlowSize := GlowSize;
    //Options.iTextShadowType := 2;
    //Options.crShadow := 3;
    //if TextFlags and DT_CALCRECT = DT_CALCRECT then
      //Options.dwFlags := Options.dwFlags or DTT_CALCRECT;
    Options.crText := ColorToRGB(Canvas.Font.Color);

    with ThemeServices.GetElementDetails(teEditTextNormal) do
      DrawThemeTextEx(HTheme{ThemeServices.Theme[teEdit]}, DC, Part, State,
        PWideChar(WideString(Text)), TextLen, TextFlags, @TextRect, Options);
  end;
var
  HTheme: THandle;
  dc, hdcPaint: HDC;
  dt: DTTOPTS;
  cx, cy: Integer;
  CR: TRect;
  dib: BITMAPINFO;
  hbm, hbmOld: HBITMAP;
  pr: Pointer;
  //lgFont: LOGFONT;
  hFontOld{, hFont}: LongWord;
  R2: TRect;
begin
  HTheme := OpenThemeData(WinCtrl.Handle,'CompositedWindow::Window');
  if (HTheme > 0) then
  begin
    dc := GetDC(WinCtrl.Handle);
    hdcPaint := CreateCompatibleDC(dc);
    if (hdcPaint > 0) then
    begin
      CR := WinCtrl.ClientRect;
      cx := R.Right - R.Left;
      cy := R.Bottom - R.Top;

      dib.bmiHeader.biSize            := sizeof(BITMAPINFOHEADER);
      dib.bmiHeader.biWidth           := cx;
      dib.bmiHeader.biHeight          := -cy;
      dib.bmiHeader.biPlanes          := 1;
      dib.bmiHeader.biBitCount        := 32;{BIT_COUNT};
      dib.bmiHeader.biCompression     := BI_RGB;

      pr := nil;
      hbm := CreateDIBSection(dc, dib, DIB_RGB_COLORS, pr, 0, 0);
      if (hbm > 0) then
      begin
        hbmOld := HBITMAP(SelectObject(hdcPaint, hbm));

        // Setup the theme drawing options.
        //dt := sizeof(DTTOPTS);
        dt.dwFlags := DTT_COMPOSITED or DTT_GLOWSIZE;
        dt.iGlowSize := 15;

        // Select a font.
        {hFontOld := 0;
        hFont := 0;
        if (SUCCEEDED(GetThemeSysFont(hTheme, TMT_CAPTIONFONT, lgFont))) then
        begin
          hFont := CreateFontIndirect(lgFont);
          hFontOld := LongWord(SelectObject(hdcPaint, hFont));
        end;
        }
        hFontOld := LongWord(SelectObject(hdcPaint, Canvas.Font.Handle));

        // Draw the title.
        if (BackGround <> clNone) then
        begin
          //DrawTransGradient(hdcPaint, R);
        end;

        if (Text <> '') then
        begin
          R2 := Rect(4, R.Top + 6, cx, cy);
          DoDrawThemeTextEx(hdcPaint, Text, Length(Text), TextR, HTheme, DT_EXPANDTABS or DT_LEFT or DT_VCENTER or DT_END_ELLIPSIS);
        end;

        // Blit text to the frame.
        BitBlt(dc, R.Left, R.Top, cx, cy, hdcPaint, 0, 0, SRCCOPY);

        SelectObject(hdcPaint, hbmOld);
        if (hFontOld > 0) then
        begin
          SelectObject(hdcPaint, hFontOld);
          //if (hFont > 0) then
            //DeleteObject(hFont);
          DeleteObject(hFontOld);
        end;

        DeleteObject(hbm);
      end;
      DeleteDC(hdcPaint);
      hdcPaint := 0;
    end;
    ReleaseDC(WinCtrl.Handle, dc);

    if (hdcPaint > 0) then
      DeleteDC(hdcPaint);
    DeleteObject(dc);

    CloseThemeData(hTheme);
  end;
end;

//------------------------------------------------------------------------------

function IsComCtl6: Boolean;
var
  i: Integer;
begin
  i := GetFileVersion('COMCTL32.DLL');
  i := (i shr 16) and $FF;

  Result := (i > 5);
end;

//------------------------------------------------------------------------------

function AeroIsEnabled: boolean;
var
  enabled: bool;
begin
  Result := False;
  //if (DWMlibrary = 0) then
  begin
    if (@DwmIsCompositionEnabled <> nil) then
    begin
      DwmIsCompositionEnabled(enabled);
      Result := enabled;
    end;
  end;
end;

function IsVista: boolean;
var
  hKernel32: HMODULE;
begin
  hKernel32 := GetModuleHandle('kernel32');
  if (hKernel32 > 0) then
  begin
    Result := GetProcAddress(hKernel32, 'GetLocaleInfoEx') <> nil;
  end
  else
    Result := false;
end;

//------------------------------------------------------------------------------

{ TAeroLabel }

constructor TAeroLabel.Create(AOwner: TComponent);
begin
{$IFNDEF DELPHI6_LVL}
  FIsAeroVista := False;
{$ELSE}
  FIsAeroVista := IsComCtl6 and IsVista and ThemeServicesThemesEnabled and AeroIsEnabled and not (csDesigning in ComponentState);
{$ENDIF}

{$IFNDEF TMS_DWM}
  //FIsAeroVista := False;
{$ENDIF}
  inherited Create(AOwner);

  if (csDesigning in ComponentState) then
    FIsAeroVista := False;

  FGlowSize := 12;
  FImageIndex := -1;
  FImagePos := ipLeft;
  FSpacing := 4;
  SetBounds(0, 0, 100, 32);
  ParentFont := True;

  // make sure to use a Truetype font
  Font.Name := 'Tahoma';

  ShowHint := False;
end;

//------------------------------------------------------------------------------

destructor TAeroLabel.Destroy;
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAeroLabel.Paint;
var
  R, TextBounds: TRect;
  TextSize, ImgSize, ImgPos: TPoint;
  w, h: Integer;
begin
  R := ClientRect;
  w := R.Right - R.Left;
  h := R.Bottom - R.Top;
  Canvas.Font.Assign(Self.Font);

  if Length(Caption) > 0 then
  begin
    TextBounds := Rect(R.Left, R.Top, w, h);
    if False and FIsAeroVista then
    begin
      //DrawBlurredText(Self, Self.Canvas, Caption, ClientRect, TextBounds, taLeftJustify, False, TextSize);
    end
    else
    begin
      DrawText(Canvas.Handle, PChar(Caption), Length(Caption), TextBounds,
        DT_CALCRECT or DT_LEFT or DT_SINGLELINE);
      TextSize := Point(TextBounds.Right - TextBounds.Left, TextBounds.Bottom -
        TextBounds.Top);
    end;
    TextBounds := Rect(R.Left, R.Top, w, h);
  end
  else
  begin
    TextBounds := Rect(0, 0, 0, 0);
    TextSize := Point(0,0);
  end;

  if Assigned(Images) and (ImageIndex >= 0) and (ImageIndex < Images.Count) then
  begin
    ImgSize := Point(Images.Width, Images.Height);
    if (Caption <> '') then
    begin
      case ImagePos of
        ipLeft:
        begin
          if FIsAeroVista then
            Inc(TextSize.X, Length(Caption) div 2);

          ImgPos.X := R.Left + (w - TextSize.X - Spacing - ImgSize.X) div 2;
          ImgPos.Y := R.Top;// + (h - ImgSize.Y) div 2;
          TextBounds.Left := ImgPos.X + ImgSize.X + Spacing;
          //TextBounds.Top := TextBounds.Top - 2 + (h - TextSize.Y) div 2;
          TextBounds.Top := TextBounds.Top + 5;
          ImgPos.Y := ImgPos.Y + 5;
        end;
        ipRight:
        begin
          if FIsAeroVista then
            Inc(TextSize.X, Length(Caption) div 2);

          ImgPos.X := R.Right - ImgSize.X - (w - TextSize.X - Spacing - ImgSize.X) div 2;
          ImgPos.Y := R.Top;// + (h - ImgSize.Y) div 2;
          TextBounds.Right := ImgPos.X - Spacing;
          TextBounds.Left := TextBounds.Right - TextSize.X;
          //TextBounds.Top := TextBounds.Top - 2 + (h - TextSize.Y) div 2;
          TextBounds.Top := TextBounds.Top + 5;
          ImgPos.Y := ImgPos.Y + 5;
        end;
        ipTop:
        begin
          ImgPos.X := R.Left + (w - ImgSize.X) div 2;
          ImgPos.Y := R.Top + (h - ImgSize.Y - Spacing - TextSize.Y) div 2;
          TextBounds.Top := ImgPos.Y + ImgSize.Y + Spacing;
          TextBounds.Left := TextBounds.Left - 4 + (w - TextSize.X) div 2;
        end;
        ipBottom:
        begin
          ImgPos.X := R.Left + (w - ImgSize.X) div 2;
          ImgPos.Y := R.Bottom - ImgSize.Y - (h - ImgSize.Y - Spacing - TextSize.Y) div 2;
          TextBounds.Bottom := ImgPos.Y - Spacing;
          TextBounds.Top := TextBounds.Bottom - TextSize.Y;
          TextBounds.Left := TextBounds.Left - 4 + (w - TextSize.X) div 2;
        end;
      end;
    end
    else
    begin
      ImgPos := Point(1, 1);
    end;
  end
  else
  begin
    ImgPos := Point(0, 0);
    ImgSize := Point(0, 0);
    TextBounds := Rect(R.Left + 7, R.Top + 6, w, h);
  end;

  if not FIsAeroVista then
  begin
    //inherited;

    if Assigned(Images) and (ImageIndex >= 0) and (ImageIndex < Images.Count) then
    begin
      Images.Draw(Canvas, ImgPos.X, ImgPos.Y, ImageIndex, True);
    end;

    Canvas.Brush.Style := bsClear;
    DrawText(Canvas.Handle,PChar(Caption),Length(Caption), TextBounds, DT_LEFT or DT_TOP or DT_SINGLELINE)
  end
  else
  begin
    DrawBlurredText(Self, Self.Canvas, Caption, ClientRect, TextBounds, taLeftJustify, GlowSize);
    DrawGDIPImageFromImageList(nil, Canvas, ImgPos, Images, ImageIndex, True);
  end;

end;

//------------------------------------------------------------------------------

procedure TAeroLabel.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAeroLabel.CMFontChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;  
end;

//------------------------------------------------------------------------------

procedure TAeroLabel.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if (csDesigning in ComponentState) then
    Exit;

  if Assigned(FOnMouseEnter) then
     FOnMouseEnter(Self);
end;

//------------------------------------------------------------------------------

procedure TAeroLabel.CMMouseLeave(var Message: TMessage);
begin
  inherited;

  if (csDesigning in ComponentState) then
    exit;

  if Assigned(FOnMouseLeave) then
     FOnMouseLeave(Self);
end;

//------------------------------------------------------------------------------

procedure TAeroLabel.CMTextChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAeroLabel.Loaded;
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAeroLabel.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAeroLabel.MouseMove(Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAeroLabel.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAeroLabel.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;
  if (AOperation = opRemove) and (AComponent = FImages) then
    Images := nil;
end;

//------------------------------------------------------------------------------

function TAeroLabel.GetVersionNr: integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

//------------------------------------------------------------------------------

function TAeroLabel.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

//------------------------------------------------------------------------------

procedure TAeroLabel.SetGlowSize(const Value: Integer);
begin
  if (FGlowSize <> Value) and (Value >= 0) and (Value <= 15) then
  begin
    FGlowSize := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAeroLabel.SetImageIndex(const Value: TImageIndex);
begin
  if (FImageIndex <> Value) then
  begin
    FImageIndex := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAeroLabel.SetImages(const Value: TCustomImageList);
begin
  if (FImages <> Value) then
  begin
    FImages := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAeroLabel.SetImagePos(const Value: TImagePos);
begin
  if (FImagePos <> Value) then
  begin
    FImagePos := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAeroLabel.SetSpacing(const Value: Integer);
begin
  if (FSpacing <> Value) then
  begin
    FSpacing := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAeroLabel.SetVersion(const Value: string);
begin

end;

//------------------------------------------------------------------------------

procedure TAeroLabel.WMEraseBkGnd(var Message: TWMEraseBkGnd);
begin
  Message.Result := 1;
  inherited;
end;

//------------------------------------------------------------------------------


end.
