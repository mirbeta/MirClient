unit DemoUtils;

{$I cxVer.inc}

interface

uses
  Types, Windows, Graphics, DB, cxDB;

type
  TCustomDrawingStyle = (cdsBkImage, cdsGradient, cdsDependOnData, cdsDefaultDrawing);
  TCustomDrawArea = (cdaCell, cdaColumnHeader, cdaFooterCell, cdaGroupCell, cdaIndicatorCell, cdaPartBackGround);
  TViewType = (vtMaster, vtDetail);
  TColorScheme = (csGrey, csGold, csBlue, csGreen);

  TBkImage = (bkiTile, bkiSky, bkiEgypt, bkiMyFace, bkiUserDefined);
  TBkImages = array [0..1, 0..5] of TBkImage;

  TColorSchemes = array [0..1, 0..5] of TColorScheme;
  TColorSchemeArr = array [0..3, 0..2] of TColor;
  TCustomDrawingStyleArr = array [0..1, 0..5] of TCustomDrawingStyle;
  TUserDefinedBitMaps = array [0..1, 0..5] of TBitmap;
  TFonts = array [0..1, 0..5] of TFont;
  TCustomDrawItem = class
    ViewType: TViewType;
    CustomDrawArea: TCustomDrawArea;
  end;

const
  clBlueDark = TColor($00C56A31);
  clBlueLight = TColor($00F7EAD9);
  clBlueBright = TColor($00FF953D);
  clBlueSky = TColor($00EBC4A4);

  clGold = TColor($0047D5FE);
  clGoldDark = TColor($0001BDF3);

  clGreyLight = TColor($00E2EFF1);
  clGreyDark = TColor($00B9D9DD);
  clYellowLight = TColor($00E1FFFF);

  clGreenBright = TColor($0082E887);
  clGreenLight = TColor($00C9F5CB);
  clGreenObscured = TColor($00ACF0AF);
  clGreenDark = TColor($0044DD4B);

  clSilverDark = TColor($00A6A6A6);

  ColorScheme : TColorSchemeArr  = ((clSilver, clWhite, clGray),(clGold, clGreyLight, clGoldDark),(clBlueDark, clBlueLight, clBlueDark),(clGreenDark, clGreenLight, clGreen));

procedure DrawGradient(Canvas: TCanvas; const ARect: TRect;
  FromColor, ToColor: TColor; AStepCount: Integer; IsVertical: Boolean = False);
procedure LoadImageFromRes(ABitmap: TBitMap; AResName: String);
procedure SetStringFieldValue(AField: TStringField; AValue: string);

implementation

uses
{$IFDEF CLR}
  WinUtils,
{$ENDIF}
  SysUtils, Classes, cxEditPaintUtils;

procedure DrawGradient(Canvas: TCanvas; const ARect: TRect;
  FromColor, ToColor: TColor; AStepCount: Integer; IsVertical: Boolean = False);
var
  SR: TRect;
  H, I: Integer;
  R, G, B: Byte;
  FromR, ToR, FromG, ToG, FromB, ToB: Byte;
begin
  FromR := GetRValue(FromColor);
  FromG := GetGValue(FromColor);
  FromB := GetBValue(FromColor);
  ToR := GetRValue(ToColor);
  ToG := GetGValue(ToColor);
  ToB := GetBValue(ToColor);
  SR := ARect;
  with ARect do
    if IsVertical then
      H := Bottom - Top
    else
      H := Right - Left;

  for I := 0 to AStepCount - 1 do
  begin
    if IsVertical then
      SR.Bottom := ARect.Top + MulDiv(I + 1, H, AStepCount)
    else
      SR.Right := ARect.Left + MulDiv(I + 1, H, AStepCount);
    with Canvas do
    begin
      R := FromR + MulDiv(I, ToR - FromR, AStepCount - 1);
      G := FromG + MulDiv(I, ToG - FromG, AStepCount - 1);
      B := FromB + MulDiv(I, ToB - FromB, AStepCount - 1);
    {$IFNDEF CLR}
      Brush.Color := RGB(R, G, B);
      FillRect(SR);
    {$ELSE}
      Windows.FillRect(Canvas.Handle, SR, GetSolidBrush(RGB(R, G, B)));
    {$ENDIF}
    end;
    if IsVertical then
      SR.Top := SR.Bottom
    else
      SR.Left := SR.Right;
  end;
end;

procedure LoadImageFromRes(ABitmap: TBitMap; AResName: String);
var
  Rs: TResourceStream;
  BitMap: TBitMap;
begin
  BitMap := TBitMap.Create;
  Rs := TResourceStream.Create(hInstance, AResName, RT_RCDATA);
  try
    BitMap.LoadFromStream(Rs);
    ABitMap.Assign(BitMap);
  finally
    BitMap.Free;
    Rs.Free;
  end;
end;

procedure SetStringFieldValue(AField: TStringField; AValue: string);
begin
{$IFDEF CLR}
  {$IFNDEF DELPHI9}
// Bug in Delphi8
  while AValue.Length < AField.Size do
    AValue := AValue + ' ';
  {$ENDIF}
{$ENDIF}
  SetFieldValue(AField, AValue);
end;

end.
