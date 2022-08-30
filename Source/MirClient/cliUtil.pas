unit cliUtil;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, PXL.Types,
  PXL.Canvas, PXL.Textures, uGameEngine, uCommon, WIL, StdCtrls, DIB, HUtil32, mmSystem;

const
  ALPHAVALUE = 150;

type
  TColorEffect = (ceNone, ceGrayScale, ceBright, ceBlack, ceWhite, ceRed, ceGreen, ceBlue, ceYellow, ceFuchsia, cePurple);

procedure MakeDark(ACanavas: TCustomCanvas; DarkLevel: integer); inline;
procedure DrawEffect(X, Y: Integer; ACanavas: TCustomCanvas; ATexture: TCustomLockableTexture; AColorEff: TColorEffect; ATransparent: Boolean); inline;

var
  DarkLevel: integer;

implementation

uses MShare, ClMain;

procedure MakeDark(ACanavas: TCustomCanvas; DarkLevel: integer);
begin
  if darklevel in [1 .. 30] then
    ACanavas.FillRectAlpha(ACanavas.ClipRect, 0, round((30-darklevel)*255/30));
end;

procedure DrawEffect(X, Y: Integer; ACanavas: TCustomCanvas; ATexture: TCustomLockableTexture; AColorEff: TColorEffect; ATransparent: Boolean);
begin
  if ATexture = nil then Exit;

  if not ATransparent then
  begin
    case AColorEff of
      ceNone: ACanavas.Draw(X, Y, ATexture, True);
      ceGrayScale: ACanavas.Draw(X, Y, ATexture, TBlendingEffect.beGrayscale);
      //ceBright: ACanavas.Draw(X, Y, ATexture);
      ceBright: ACanavas.Draw(X, Y, ATexture.ClientRect, ATexture, IntColorWhite, TBlendingEffect.fxBlend);
      ceBlack: ACanavas.DrawColor(X, Y, ATexture, clBlack, True);
      ceRed: ACanavas.DrawColor(X, Y, ATexture, clRed, True);
      ceGreen: ACanavas.DrawColor(X, Y, ATexture, GetRGB(222), True);
      ceBlue: ACanavas.DrawColor(X, Y, ATexture, clBlue, True);
      ceYellow: ACanavas.DrawColor(X, Y, ATexture, clYellow, True);
      ceFuchsia: ACanavas.DrawColor(X, Y, ATexture, clFuchsia, True);
      ceWhite: ACanavas.DrawColor(X, Y, ATexture, clWhite, True);
    end;
  end
  else
  begin
    case AColorEff of
      ceNone: ACanavas.DrawAlpha(X, Y, ATexture, ALPHAVALUE);
      ceGrayScale: ACanavas.DrawAlpha(X, Y, ATexture.ClientRect, ATexture, ALPHAVALUE, TBlendingEffect.beGrayscale);
//      ceBright: ACanavas.Draw(X, Y, ATexture.ClientRect, ATexture, cColor4(cRGB1(150, 150, 150, 255)), beBlend);
     // ceBright: ACanavas.Draw(X, Y, ATexture.ClientRect, ATexture, IntColorRGB(ALPHAVALUE, ALPHAVALUE, ALPHAVALUE, 200), TBlendingEffect.beBlend);
      ceBright: ACanavas.Draw(X, Y, ATexture.ClientRect, ATexture, $80B4B4B4, TBlendingEffect.beBright);  //20200729修复32位素材光亮问题
      ceBlack: ACanavas.DrawColorAlpha(X, Y, ATexture, clBlack, True, ALPHAVALUE);
      ceRed: ACanavas.DrawColorAlpha(X, Y, ATexture, clRed, True, ALPHAVALUE);
      ceGreen: ACanavas.DrawColorAlpha(X, Y, ATexture, GetRGB(222), True, ALPHAVALUE);
      ceBlue: ACanavas.DrawColorAlpha(X, Y, ATexture, clBlue, True, ALPHAVALUE);
      ceYellow: ACanavas.DrawColorAlpha(X, Y, ATexture, clYellow, True, ALPHAVALUE);
      ceFuchsia: ACanavas.DrawColorAlpha(X, Y, ATexture, clFuchsia, True, ALPHAVALUE);
      ceWhite: ACanavas.DrawColorAlpha(X, Y, ATexture, clWhite, True, ALPHAVALUE);
    end;
  end;
end;

end.



