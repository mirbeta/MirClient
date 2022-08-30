{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           Express Cross Platform Library controls                  }
{                                                                    }
{           Copyright (c) 2000-2019 Developer Express Inc.           }
{           ALL RIGHTS RESERVED                                      }
{                                                                    }
{   The entire contents of this file is protected by U.S. and        }
{   International Copyright Laws. Unauthorized reproduction,         }
{   reverse-engineering, and distribution of all or any portion of   }
{   the code contained in this file is strictly prohibited and may   }
{   result in severe civil and criminal penalties and will be        }
{   prosecuted to the maximum extent possible under the law.         }
{                                                                    }
{   RESTRICTIONS                                                     }
{                                                                    }
{   THIS SOURCE CODE AND ALL RESULTING INTERMEDIATE FILES            }
{   (DCU, OBJ, DLL, ETC.) ARE CONFIDENTIAL AND PROPRIETARY TRADE     }
{   SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER IS    }
{   LICENSED TO DISTRIBUTE THE EXPRESSCROSSPLATFORMLIBRARY AND ALL   }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM       }
{   ONLY.                                                            }
{                                                                    }
{   THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED       }
{   FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE         }
{   COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE        }
{   AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT   }
{   AND PERMISSION FROM DEVELOPER EXPRESS INC.                       }
{                                                                    }
{   CONSULT THE END USER LICENSE AGREEMENT FOR INFORMATION ON        }
{   ADDITIONAL RESTRICTIONS.                                         }
{                                                                    }
{********************************************************************}

unit dxOffice11;

{$I cxVer.inc}

interface

uses
  Types, SysUtils, TypInfo, Windows, Messages, Graphics, Controls, Forms, Classes, cxGraphics, cxGeometry;

type
  TOffice11Scheme = (schUnknown, schNormalColor, schHomestead, schMetallic);
  TOffice11SchemeColor = schNormalColor .. schMetallic;

var
  { Colors }

  // dock
  dxOffice11DockColor1: COLORREF;
  dxOffice11DockColor2: COLORREF;
  // bar: background
  dxOffice11ToolbarsColor1: COLORREF;
  dxOffice11ToolbarsColor2: COLORREF;
  // bar: marks
  dxOffice11BarMarkColors1: array[1..3] of COLORREF; // Normal, Selected, Pressed
  dxOffice11BarMarkColors2: array[1..3] of COLORREF; // Normal, Selected, Pressed
  dxOffice11BarBorderColors: array[1..2] of COLORREF; // fixed colors
  dxOffice11BarBorderMarkColors: array[1..2, 1..3] of COLORREF;
  // bar: fingers, separators, mark arrows
  dxOffice11BarFingerColor1: COLORREF;
  dxOffice11BarFingerColor2: COLORREF;
  dxOffice11BarSeparatorColor1: COLORREF;
  dxOffice11BarSeparatorColor2: COLORREF;
  dxOffice11BarMarkArrowColor1: COLORREF;
  dxOffice11BarMarkArrowColor2: COLORREF;
  // bar: floating toolbar
  dxOffice11BarFloatingBorderColor1: COLORREF;
  dxOffice11BarFloatingBorderColor2: COLORREF;
  dxOffice11BarFloatingBorderColor3: COLORREF;
  dxOffice11BarFloatingCaptionColor: COLORREF;
  dxOffice11BarFloatingCaptionTextColor1: COLORREF;
  dxOffice11BarFloatingCaptionTextColor2: COLORREF;
  dxOffice11BarFloatingCaptionTextColor3: COLORREF;
  // drop downs
  dxOffice11MenuColor: COLORREF;
  dxOffice11MenuDownedColor1: COLORREF;
  dxOffice11MenuDownedColor2: COLORREF;
  dxOffice11MenuIndentColor1: COLORREF;
  dxOffice11MenuIndentColor2: COLORREF;
  dxOffice11MenuNonRecentIndentColor1: COLORREF;
  dxOffice11MenuNonRecentIndentColor2: COLORREF;
  dxOffice11DropDownBorderColor1: COLORREF;
  dxOffice11DropDownBorderColor2: COLORREF;
  // selected, pressed, downed
  dxOffice11ToolbarSelectedColor: COLORREF;
  dxOffice11SelectedBorderColor: COLORREF;
  dxOffice11SelectedColor1: COLORREF;
  dxOffice11SelectedColor2: COLORREF;
  dxOffice11SelectedDownColor1: COLORREF;
  dxOffice11SelectedDownColor2: COLORREF;
  dxOffice11OwnerControlDownedColor: COLORREF;
  dxOffice11DownedColor: COLORREF;
  dxOffice11DownedSelectedColor: COLORREF;
  // detachable
  dxOffice11DetachableColor: COLORREF;
  dxOffice11DetachableSelectedColor: COLORREF;
  // text
  dxOffice11TextEnabledColor: COLORREF;
  dxOffice11TextDisabledColor: COLORREF;
  // static
  dxOffice11StaticBorderColor: COLORREF;
  // date
  dxOffice11DateHeaderColor: COLORREF;
  // inplace sub item
  dxOffice11InPlaceSubItemColor: COLORREF;
  dxOffice11InPlaceSubItemTextColor: COLORREF;
  // LF painters
  // control border color
  dxOffice11ControlBorderColor: COLORREF;
  dxOffice11GroupColor: COLORREF;
  // Outlook colors
  dxOffice11DayNavigatorSelectedColor: COLORREF;
  dxOffice11OutlookBorderColor: COLORREF;
  dxOffice11OutlookControlColor: COLORREF;
  // Office11 group row style colors
  dxOffice11GroupIndentColor: COLORREF;
  dxOffice11GroupRowTextColor: COLORREF;
  dxOffice11GroupRowSeparatorColor: COLORREF;
  // NavPane colors
  dxOffice11NavPaneBorder: COLORREF;
  dxOffice11NavPaneGroupCaptionColor1: COLORREF;
  dxOffice11NavPaneGroupCaptionColor2: COLORREF;
  dxOffice11NavPaneGroupCaptionHotColor1: COLORREF;
  dxOffice11NavPaneGroupCaptionHotColor2: COLORREF;
  dxOffice11NavPaneGroupCaptionPressedColor1: COLORREF;
  dxOffice11NavPaneGroupCaptionPressedColor2: COLORREF;
  dxOffice11NavPaneGroupCaptionPressedHotColor1: COLORREF;
  dxOffice11NavPaneGroupCaptionPressedHotColor2: COLORREF;
  dxOffice11NavPaneHeaderColor1: COLORREF;
  dxOffice11NavPaneHeaderColor2: COLORREF;
  dxOffice11NavPaneHeaderFontColor: COLORREF;
  dxOffice11NavPaneSplitterColor1: COLORREF;
  dxOffice11NavPaneSplitterColor2: COLORREF;
  // Expand button colors
  dxOffice11ExpandButtonColor1: COLORREF;
  dxOffice11ExpandButtonColor2: COLORREF;

  { Brushes }

  dxOffice11BarFingerBrush1: HBRUSH;
  dxOffice11BarFingerBrush2: HBRUSH;
  dxOffice11BarSeparatorBrush1: HBRUSH;
  dxOffice11BarSeparatorBrush2: HBRUSH;
  dxOffice11MenuBrush: HBRUSH;
  dxOffice11SelectedBorderBrush: HBRUSH;
  dxOffice11ToolbarSelectedBrush: HBRUSH;
  dxOffice11OwnerControlDownedBrush: HBRUSH;
  dxOffice11DownedBrush: HBRUSH;
  dxOffice11DownedSelectedBrush: HBRUSH;

  dxOffice11DropDownBorderBrush1: HBRUSH;
  dxOffice11DropDownBorderBrush2: HBRUSH;
  dxOffice11DetachableBrush: HBRUSH;
  dxOffice11DetachableSelectedBrush: HBRUSH;

  dxOffice11StaticBorderBrush: HBRUSH;
  dxOffice11InPlaceSubItemBrush: HBRUSH;

  { Bitmaps }

  dxOffice11SubMenuExpandBitmap: TBitmap;
  dxOffice11ExpandButtonBitmap1: TBitmap;
  dxOffice11ExpandButtonBitmap2: TBitmap;

function GetOffice11Scheme: TOffice11Scheme;

procedure CreateOffice11Colors;
procedure RefreshOffice11Colors;
procedure ReleaseOffice11Colors;

function IsXPStandardScheme: Boolean;
procedure Office11FillTubeGradientRect(DC: HDC; const ARect: TRect; AColor1, AColor2: TColor; AHorizontal: Boolean);
procedure Office11FrameSelectedRect(DC: HDC; const R: TRect);
procedure Office11DrawFingerElements(DC: HDC; ARect: TRect; AHorizontal: Boolean;
  ABrush1: HBRUSH = 0; ABrush2: HBRUSH = 0; AScaleFactor: TdxScaleFactor = nil);
procedure Office11DrawItemArrow(DC: HDC; R: TRect; ADownArrow: Boolean;
  Enabled, Selected, Flat: Boolean);
procedure Office11DrawLargeItemArrow(ADC: HDC; R: TRect; AArrowType: TcxArrowDirection; ASize: Integer;
  ASelected, AEnabled, AFlat: Boolean);
procedure Office11DrawSizeGrip(DC: HDC; ARect: TRect; AColor1: TColor = clDefault;
  AColor2: TColor = clDefault; AScaleFactor: TdxScaleFactor = nil);
function GetOffice11FingerElementSize(AScaleFactor: TdxScaleFactor = nil): Integer;

implementation

{$R dxOffice11.res}

uses
  dxThemeManager, dxUxTheme, dxThemeConsts, dxDPIAwareUtils;

var
  FCounter: Integer;
  FOffice11Scheme: TOffice11Scheme;
  FXPStandardScheme: Boolean;
  GradientPercent: Integer;

function GetOffice11Scheme: TOffice11Scheme;
begin
  Result := FOffice11Scheme;
end;

procedure PrepareSubMenuExpandBitmap(ABitmap: TBitmap);
var
  I, J: Integer;
  ATransparentColor, AColor1, AColor2, APixelColor: COLORREF;
  AMinValue, AMaxValue, AValue: Byte;
begin
  ATransparentColor := ABitmap.Canvas.Pixels[0, 0];
//  AColor := ColorToRGB(clWindow);
  AColor1 := dxOffice11ToolbarsColor1;
  AColor2 := dxOffice11ToolbarsColor2;
  if AColor1 = AColor2 then
  begin
    if AColor1 = 0 then
      AColor2 := $FFFFFF
    else
      AColor2 := 0;
  end;
  AMaxValue := 1;
  AMinValue := 255;
  for I := 0 to ABitmap.Width - 1 do
    for J := 0 to ABitmap.Height - 1 do
    begin
      APixelColor := ABitmap.Canvas.Pixels[I, J];
      if (APixelColor <> ATransparentColor) and (APixelColor <> 0{mark}) then
      begin
        AValue := GetRValue(APixelColor);
        if AValue > AMaxValue then
          AMaxValue := AValue;
        if AValue < AMinValue then
          AMinValue := AValue;
      end;
    end;
  for I := 0 to ABitmap.Width - 1 do
    for J := 0 to ABitmap.Height - 1 do
    begin
      APixelColor := ABitmap.Canvas.Pixels[I, J];
      if (APixelColor <> ATransparentColor) and (APixelColor <> 0{mark}) then
      begin
//        APixelColor := dxGetMiddleRGB(APixelColor, AColor, 50);
        AValue := GetRValue(APixelColor);
        APixelColor := dxGetMiddleRGB(AColor1, AColor2, MulDiv(AValue - AMinValue, 100, AMaxValue - AMinValue));
        ABitmap.Canvas.Pixels[I, J] := APixelColor;
      end;
    end;
end;

procedure CreateOffice11Brushes;
begin
  dxOffice11BarFingerBrush1 := CreateSolidBrush(dxOffice11BarFingerColor1);
  dxOffice11BarFingerBrush2 := CreateSolidBrush(dxOffice11BarFingerColor2);
  dxOffice11BarSeparatorBrush1 := CreateSolidBrush(dxOffice11BarSeparatorColor1);
  dxOffice11BarSeparatorBrush2 := CreateSolidBrush(dxOffice11BarSeparatorColor2);
  dxOffice11DropDownBorderBrush1 := CreateSolidBrush(dxOffice11DropDownBorderColor1);
  dxOffice11MenuBrush := CreateSolidBrush(dxOffice11MenuColor);
  dxOffice11DropDownBorderBrush2 := dxOffice11MenuBrush;
  dxOffice11SelectedBorderBrush := CreateSolidBrush(dxOffice11SelectedBorderColor);
  dxOffice11OwnerControlDownedBrush := CreateSolidBrush(dxOffice11OwnerControlDownedColor);
  dxOffice11DownedBrush := CreateSolidBrush(dxOffice11DownedColor);
  dxOffice11DownedSelectedBrush := CreateSolidBrush(dxOffice11DownedSelectedColor);
  dxOffice11DetachableBrush := CreateSolidBrush(dxOffice11DetachableColor);
  dxOffice11ToolbarSelectedBrush := CreateSolidBrush(dxOffice11ToolbarSelectedColor);
  dxOffice11DetachableSelectedBrush := dxOffice11ToolbarSelectedBrush;
  dxOffice11StaticBorderBrush := dxOffice11DropDownBorderBrush1;
  dxOffice11InPlaceSubItemBrush := CreateSolidBrush(dxOffice11InPlaceSubItemColor);
  // TODO
  dxOffice11SubMenuExpandBitmap := TBitmap.Create;
  dxOffice11SubMenuExpandBitmap.Handle := LoadBitmap(HInstance, 'DXSUBMENUEXPAND');
  PrepareSubMenuExpandBitmap(dxOffice11SubMenuExpandBitmap);
  dxOffice11ExpandButtonBitmap1 := TBitmap.Create;
  dxOffice11ExpandButtonBitmap1.Handle := LoadBitmap(HInstance, 'DXEXPANDBUTTON_MINUS');
  dxOffice11ExpandButtonBitmap2 := TBitmap.Create;
  dxOffice11ExpandButtonBitmap2.Handle := LoadBitmap(HInstance, 'DXEXPANDBUTTON_PLUS');
  dxOffice11ExpandButtonColor1 := RGB(158, 174, 191);
  dxOffice11ExpandButtonColor2 := RGB(46, 70, 95);
end;

procedure DestroyOffice11Brushes;
begin
  dxOffice11SubMenuExpandBitmap.Free;
  dxOffice11SubMenuExpandBitmap := nil;
  dxOffice11ExpandButtonBitmap2.Free;
  dxOffice11ExpandButtonBitmap2 := nil;
  dxOffice11ExpandButtonBitmap1.Free;
  dxOffice11ExpandButtonBitmap1 := nil;
  if dxOffice11BarFingerBrush1 <> 0 then DeleteObject(dxOffice11BarFingerBrush1);
  if dxOffice11BarFingerBrush2 <> 0 then DeleteObject(dxOffice11BarFingerBrush2);
  if dxOffice11BarSeparatorBrush1 <> 0 then DeleteObject(dxOffice11BarSeparatorBrush1);
  if dxOffice11BarSeparatorBrush2 <> 0 then DeleteObject(dxOffice11BarSeparatorBrush2);
  if dxOffice11DropDownBorderBrush1 <> 0 then DeleteObject(dxOffice11DropDownBorderBrush1);
  if dxOffice11MenuBrush <> 0 then DeleteObject(dxOffice11MenuBrush);
  if dxOffice11SelectedBorderBrush <> 0 then DeleteObject(dxOffice11SelectedBorderBrush);
  if dxOffice11OwnerControlDownedBrush <> 0 then DeleteObject(dxOffice11OwnerControlDownedBrush);
  if dxOffice11DownedBrush <> 0 then DeleteObject(dxOffice11DownedBrush);
  if dxOffice11DownedSelectedBrush <> 0 then DeleteObject(dxOffice11DownedSelectedBrush);
  if dxOffice11DetachableBrush <> 0 then DeleteObject(dxOffice11DetachableBrush);
  if dxOffice11ToolbarSelectedBrush <> 0 then DeleteObject(dxOffice11ToolbarSelectedBrush);
  if dxOffice11InPlaceSubItemBrush <> 0 then DeleteObject(dxOffice11InPlaceSubItemBrush);
end;

function dxGetOffice11Scheme: TOffice11Scheme;
const
  SZ_MAX_CHARS = 1024;
  SStandardThemeFileName = 'LUNA.MSSTYLES';
  SNormalColor = 'NORMALCOLOR';
  SHomestead = 'HOMESTEAD';
  SMetallic = 'METALLIC';
var
  PThemeFileName, PThemeColor, PThemeSize: array [0..SZ_MAX_CHARS] of WideChar;
  S: string;
begin
  Result := schUnknown;
  if AreVisualStylesAvailable([]) and
    Succeeded(GetCurrentThemeName(PThemeFileName, SZ_MAX_CHARS, PThemeColor, SZ_MAX_CHARS, PThemeSize, SZ_MAX_CHARS)) then
  begin
    S := UpperCase(ExtractFileName(PThemeFileName));
    // WindowsXP or above
    if (S = SStandardThemeFileName) or (Win32MajorVersion > 5) then
    begin
      S := UpperCase(PThemeColor);
      if S = SNormalColor then
        Result := schNormalColor
      else
        if S = SHomestead then
          Result := schHomestead
        else
          if S = SMetallic then
            Result := schMetallic;
    end;
  end;
end;

function GetdxOffice11NavPaneGroupCaptionColor(AStartColor: Boolean): TColor;

  function TransformColorComponent(AValue: Byte): Byte;
  begin
    if AStartColor then
      Result := AValue * 127 div 255 + 128
    else
      Result := AValue + (4 - AValue * 4 div 255);
  end;

begin
  Result := ColorToRGB(clBtnFace);
  Result := RGB(TransformColorComponent(GetRValue(Result)),
    TransformColorComponent(GetGValue(Result)), TransformColorComponent(GetBValue(Result)));
  Result := dxGetNearestColor(Result);
end;

procedure InitOffice11Colors;
const
  Office11Colors: array[TOffice11SchemeColor, 0..48] of COLORREF = (
    ($FEECDD, $E2A981, $F5BE9E, $F9DAC3, $913500, $F1A675, $76C1FF, $D3F8FF, $98DDFF, $5295FE,
     $9C613B, $E0A47B, $764127, $FFFFFF, $CB8C6A, $FFF9F1, $C9662A, $F9DBC4, $962D00, $F6F6F6,
     $C2EEFF, $800000, $CCF4FF, $91D0FF, $8ED3FF, $4E91FE, $FCE1CB, $6FC0FF, $3E80FE, $F0C7A9,
     $000000, $8D8D8D, $FBDABE, $94E6FB, $FADAC4, $962D00, $B96837, $962D00, $FBE5D3, $E4AE88,
     $DCFFFF, $5BC0F7, $94E6FB, $1595EE, $087FE8, $7CDAF7, $D68759, $933803, $FFFFFF),
    ($DEF7F4, $91C6B7, $A7D9D9, $E4F0F2, $6B7760, $8CC2B0, $76C1FF, $D3F8FF, $98DDFF, $5295FE,
     $588060, $8FC4B5, $335E51, $FFFFFF, $588060, $DEF7F4, $5E8674, $ADDEE1, $5E8D75, $EEF4F4,
     $C2EEFF, $385D3F, $CCF4FF, $91D0FF, $8ED3FF, $4E91FE, $B6E3D8, $6FC0FF, $3E80FE, $9FD4C5,
     $000000, $8D8D8D, $91BAAF, $94E6FB, $E4F1F2, $588060, $548973, $5E8D75, $DBF5F2, $97C9BC,
     $DCFFFF, $5BC0F7, $94E6FB, $1595EE, $087FE8, $7CDAF7, $82C0AF, $447A63, $FFFFFF),
    ($FAF4F3, $B59799, $E5D7D7, $F7F3F3, $927476, $C8B2B3, $76C1FF, $D3F8FF, $98DDFF, $5295FE,
     $947C7C, $B09193, $755454, $FFFFFF, $8F6D6E, $FFFFFF, $99797A, $E4DADB, $947C7C, $FFFAFD,
     $C2EEFF, $6F4B4B, $CCF4FF, $91D0FF, $8ED3FF, $4E91FE, $E7D3D6, $6FC0FF, $3E80FE, $D3C0C0,
     $000000, $8D8D8D, $EBE5E5, $94E6FB, $F7F3F3, $947C7C, $916F70, $947C7C, $F8F1F0, $BA9EA0,
     $DCFFFF, $5BC0F7, $94E6FB, $1595EE, $087FE8, $7CDAF7, $BFA7A8, $916F70, $FFFFFF)
  );
  Office11GradientPercents: array[TOffice11SchemeColor] of Integer = (41, 125, 40);
var
  I: Integer;
begin
  FOffice11Scheme := dxGetOffice11Scheme;
  FXPStandardScheme := AreVisualStylesAvailable([]) and (FOffice11Scheme <> schUnknown){luna};
  if FXPStandardScheme then
  begin
    // bk colors
    dxOffice11ToolbarsColor1 := Office11Colors[FOffice11Scheme, 0];
    dxOffice11ToolbarsColor2 := Office11Colors[FOffice11Scheme, 1];
    dxOffice11DockColor1 := Office11Colors[FOffice11Scheme, 2];
    dxOffice11DockColor2 := Office11Colors[FOffice11Scheme, 3];
    // bar: mark
    dxOffice11BarMarkColors2[1] := Office11Colors[FOffice11Scheme, 4];
    dxOffice11BarMarkColors1[1] := Office11Colors[FOffice11Scheme, 5];
    dxOffice11BarMarkColors2[2] := Office11Colors[FOffice11Scheme, 6];
    dxOffice11BarMarkColors1[2] := Office11Colors[FOffice11Scheme, 7];
    dxOffice11BarMarkColors2[3] := Office11Colors[FOffice11Scheme, 8];
    dxOffice11BarMarkColors1[3] := Office11Colors[FOffice11Scheme, 9];
    // bar: border
    dxOffice11BarBorderColors[1] := Office11Colors[FOffice11Scheme, 10];
    dxOffice11BarBorderColors[2] := Office11Colors[FOffice11Scheme, 11];
    // bar: border and mark (Normal, Selected, Downed)
    for I := 1 to 3 do
      dxOffice11BarBorderMarkColors[1, I] := dxGetMiddleRGB(dxOffice11BarMarkColors1[I], dxOffice11ToolbarsColor1, 90); // #6 ???
    for I := 1 to 3 do
      dxOffice11BarBorderMarkColors[2, I] := dxGetMiddleRGB(dxOffice11BarMarkColors2[I], dxOffice11BarBorderColors[2], 50); // #9
    // bar: finger
    dxOffice11BarFingerColor1 := Office11Colors[FOffice11Scheme, 12];
    dxOffice11BarFingerColor2 := Office11Colors[FOffice11Scheme, 13];
    // bar: separator
    dxOffice11BarSeparatorColor1 := Office11Colors[FOffice11Scheme, 14];
    dxOffice11BarSeparatorColor2 := Office11Colors[FOffice11Scheme, 15];
    // bar: mark arrow
    dxOffice11BarMarkArrowColor1 := ColorToRGB(clBlack);
    dxOffice11BarMarkArrowColor2 := ColorToRGB(clWhite);
    // bar: floating
    dxOffice11BarFloatingBorderColor1 := Office11Colors[FOffice11Scheme, 16];
    dxOffice11BarFloatingBorderColor2 := Office11Colors[FOffice11Scheme, 17];
    dxOffice11BarFloatingCaptionTextColor1 := ColorToRGB(clWhite);
    dxOffice11BarFloatingCaptionTextColor2 := ColorToRGB(clBlack);
    dxOffice11BarFloatingCaptionTextColor3 := ColorToRGB(clBlack);
    // drop downs, menus
    dxOffice11DropDownBorderColor1 := Office11Colors[FOffice11Scheme, 18];
    dxOffice11MenuColor := Office11Colors[FOffice11Scheme, 19];
    // selected
    dxOffice11ToolbarSelectedColor := Office11Colors[FOffice11Scheme, 20];
    dxOffice11SelectedBorderColor := Office11Colors[FOffice11Scheme, 21];
    dxOffice11SelectedColor1 := Office11Colors[FOffice11Scheme, 22];
    dxOffice11SelectedColor2 := Office11Colors[FOffice11Scheme, 23];
    dxOffice11SelectedDownColor1 := Office11Colors[FOffice11Scheme, 24];
    dxOffice11SelectedDownColor2 := Office11Colors[FOffice11Scheme, 25];
    dxOffice11OwnerControlDownedColor := Office11Colors[FOffice11Scheme, 26];
    dxOffice11DownedColor := Office11Colors[FOffice11Scheme, 27];
    dxOffice11DownedSelectedColor := Office11Colors[FOffice11Scheme, 28];
    // detachable
    dxOffice11DetachableColor := Office11Colors[FOffice11Scheme, 29];
    // text
    dxOffice11TextEnabledColor := Office11Colors[FOffice11Scheme, 30];
    dxOffice11TextDisabledColor := Office11Colors[FOffice11Scheme, 31];
    // Gradient Tube Percent
    GradientPercent := Office11GradientPercents[FOffice11Scheme];
    // LF
    dxOffice11GroupColor := Office11Colors[FOffice11Scheme, 32];
    // Outlook day navigator
    dxOffice11DayNavigatorSelectedColor := Office11Colors[FOffice11Scheme, 33];
    dxOffice11OutlookControlColor := Office11Colors[FOffice11Scheme, 34];
    dxOffice11OutlookBorderColor := Office11Colors[FOffice11Scheme, 35];
    // Office11 group row style colors
    dxOffice11GroupIndentColor := $C9EEFD;
    dxOffice11GroupRowSeparatorColor := dxOffice11BarBorderColors[2];
    dxOffice11GroupRowTextColor := Office11Colors[FOffice11Scheme, 36];
    // NavPane
    dxOffice11NavPaneBorder := Office11Colors[FOffice11Scheme, 37];
    dxOffice11NavPaneGroupCaptionColor1 := Office11Colors[FOffice11Scheme, 38];
    dxOffice11NavPaneGroupCaptionColor2 := Office11Colors[FOffice11Scheme, 39];
    dxOffice11NavPaneGroupCaptionHotColor1 := Office11Colors[FOffice11Scheme, 40];
    dxOffice11NavPaneGroupCaptionHotColor2 := Office11Colors[FOffice11Scheme, 41];
    dxOffice11NavPaneGroupCaptionPressedColor1 := Office11Colors[FOffice11Scheme, 42];
    dxOffice11NavPaneGroupCaptionPressedColor2 := Office11Colors[FOffice11Scheme, 43];
    dxOffice11NavPaneGroupCaptionPressedHotColor1 := Office11Colors[FOffice11Scheme, 44];
    dxOffice11NavPaneGroupCaptionPressedHotColor2 := Office11Colors[FOffice11Scheme, 45];
    dxOffice11NavPaneHeaderColor1 := Office11Colors[FOffice11Scheme, 46];
    dxOffice11NavPaneHeaderColor2 := Office11Colors[FOffice11Scheme, 47];
    dxOffice11NavPaneHeaderFontColor := Office11Colors[FOffice11Scheme, 48];
    dxOffice11NavPaneSplitterColor1 := dxOffice11NavPaneHeaderColor1;
    dxOffice11NavPaneSplitterColor2 := dxOffice11NavPaneHeaderColor2;
  end
  else
  begin
    // bk colors
    dxOffice11DockColor1 := ColorToRGB(clBtnFace);
    dxOffice11DockColor2 := dxGetMiddleRGB(clBtnFace, clWindow, 20);
    dxOffice11ToolbarsColor1 := dxGetMiddleRGB(clBtnFace, clWindow, 22);
    dxOffice11ToolbarsColor2 := dxGetMiddleRGB(clBtnFace, clWindow, 96);
    // bar: mark
    dxOffice11BarMarkColors2[1] := ColorToRGB(clBtnShadow);
    dxOffice11BarMarkColors1[1] := dxGetMiddleRGB(dxOffice11BarMarkColors2[1], clWindow, 74);
    // TODO
    dxOffice11BarMarkColors1[2] := dxGetNearestColor(GetLightColor(-15, 29, 100--15-29)); // Selected
  //  dxOffice11BarMarkColors1[2] := FlatToolbarsSelColor;
    dxOffice11BarMarkColors2[2] := dxOffice11BarMarkColors1[2];
    dxOffice11BarMarkColors1[3] := dxGetNearestColor(GetLightColor(42, 11, 100-42-11)); // Downed
    dxOffice11BarMarkColors2[3] := dxOffice11BarMarkColors1[3];
    // bar: border
    dxOffice11BarBorderColors[1] := dxGetMiddleRGB(clBtnFace, clWindow, 85);
    dxOffice11BarBorderColors[2] := ColorToRGB(clBtnFace);
    // bar: border and mark (Normal, Selected, Downed)
    for I := 1 to 3 do
      dxOffice11BarBorderMarkColors[1, I] := dxGetMiddleRGB(dxOffice11BarMarkColors2[I], clWindow, 50); // #6
    for I := 1 to 3 do
      dxOffice11BarBorderMarkColors[2, I] := dxGetMiddleRGB(dxOffice11BarMarkColors2[I], dxOffice11BarBorderColors[2], 50); // #9
    // bar: finger
    dxOffice11BarFingerColor1 := dxGetMiddleRGB(clBtnShadow, clWindow, 76);
    dxOffice11BarFingerColor2 := ColorToRGB(clWindow);
    // bar: separator
    dxOffice11BarSeparatorColor1 := dxGetMiddleRGB(clBtnShadow, clWindow, 70);
    dxOffice11BarSeparatorColor2 := ColorToRGB(clWhite);
    // bar: mark arrow
    dxOffice11BarMarkArrowColor1 := ColorToRGB(clBtnText);
    dxOffice11BarMarkArrowColor2 := ColorToRGB(clWindow);
    // bar: floating
    dxOffice11BarFloatingBorderColor1 := ColorToRGB(clBtnShadow);
    dxOffice11BarFloatingBorderColor2 := dxOffice11ToolbarsColor2;
    dxOffice11BarFloatingCaptionTextColor1 := ColorToRGB(clBtnHighlight);
    dxOffice11BarFloatingCaptionTextColor2 := ColorToRGB(clBtnText);
    dxOffice11BarFloatingCaptionTextColor3 := ColorToRGB(clBtnText);
    // drop down
    dxOffice11DropDownBorderColor1 := ColorToRGB(clBtnShadow); // ?
    dxOffice11MenuColor := ColorToRGB(clWindow); // ?
    // selected
    dxOffice11ToolbarSelectedColor := dxGetNearestColor(GetLightColor(-2, 30, 72)); // ?
    dxOffice11SelectedBorderColor := ColorToRGB(clHighlight);
    dxOffice11SelectedColor1 := dxOffice11ToolbarSelectedColor;
    dxOffice11SelectedColor2 := dxOffice11ToolbarSelectedColor;
    dxOffice11OwnerControlDownedColor := dxOffice11ToolbarsColor2;
    dxOffice11DownedColor := GetLightColor(41, 11, 48); // ?
    dxOffice11DownedSelectedColor := dxGetNearestColor(GetLightColor(14, 44, 40)); // ?
    dxOffice11SelectedDownColor1 := dxOffice11DownedSelectedColor;
    dxOffice11SelectedDownColor2 := dxOffice11DownedSelectedColor;
    // detachable
    dxOffice11DetachableColor := dxOffice11OwnerControlDownedColor;
    // text
    dxOffice11TextEnabledColor := ColorToRGB(clBtnText);
    dxOffice11TextDisabledColor := ColorToRGB(clGrayText);
    // Gradient Tube Percent
    GradientPercent := 75;
    // LF
    dxOffice11GroupColor := ColorToRGB(clBtnFace);
    // Outlook day navigator
    dxOffice11DayNavigatorSelectedColor := dxOffice11DownedColor;
    dxOffice11OutlookControlColor := dxOffice11DockColor1;
    dxOffice11OutlookBorderColor := dxOffice11DropDownBorderColor1;
    // Office11 group row style colors
    dxOffice11GroupIndentColor := ColorToRGB(clBtnFace);
    dxOffice11GroupRowTextColor := ColorToRGB(clBtnShadow);
    dxOffice11GroupRowSeparatorColor := ColorToRGB(clBtnShadow);
    // NavPane
    dxOffice11NavPaneBorder := dxGetNearestColor(ColorToRGB(clBtnShadow));
    dxOffice11NavPaneGroupCaptionColor1 := GetdxOffice11NavPaneGroupCaptionColor(True);
    dxOffice11NavPaneGroupCaptionColor2 := GetdxOffice11NavPaneGroupCaptionColor(False);
    dxOffice11NavPaneGroupCaptionHotColor1 := dxOffice11SelectedColor1;
    dxOffice11NavPaneGroupCaptionHotColor2 := dxOffice11SelectedColor2;
    dxOffice11NavPaneGroupCaptionPressedColor1 := dxOffice11DownedColor;
    dxOffice11NavPaneGroupCaptionPressedColor2 := dxOffice11NavPaneGroupCaptionPressedColor1;
    dxOffice11NavPaneGroupCaptionPressedHotColor1 := dxOffice11DownedSelectedColor;
    dxOffice11NavPaneGroupCaptionPressedHotColor2 := dxOffice11NavPaneGroupCaptionPressedHotColor1;
    dxOffice11NavPaneHeaderColor1 := dxOffice11NavPaneBorder;
    dxOffice11NavPaneHeaderColor2 := dxOffice11NavPaneBorder;
    dxOffice11NavPaneHeaderFontColor := ColorToRGB(clHighlightText);
    dxOffice11NavPaneSplitterColor1 := dxGetNearestColor(ColorToRGB(clBtnFace));
    dxOffice11NavPaneSplitterColor2 := dxOffice11NavPaneBorder;
  end;
  // bar: floating
  dxOffice11BarFloatingBorderColor3 := dxOffice11ToolbarsColor1;
  dxOffice11BarFloatingCaptionColor := dxOffice11BarFloatingBorderColor1;
  // drop downs, menus
  dxOffice11DropDownBorderColor2 := dxOffice11MenuColor;
  dxOffice11MenuDownedColor1 := dxOffice11ToolbarsColor1;
  dxOffice11MenuDownedColor2 := dxOffice11ToolbarsColor2;
  dxOffice11MenuIndentColor1 := dxOffice11ToolbarsColor1;
  dxOffice11MenuIndentColor2 := dxOffice11ToolbarsColor2;
  dxOffice11MenuNonRecentIndentColor1 := dxGetMiddleRGB(dxOffice11MenuIndentColor1, clBlack, 92);
  dxOffice11MenuNonRecentIndentColor2 := dxGetMiddleRGB(dxOffice11MenuIndentColor2, clBlack, 92);
  // selected
  dxOffice11DetachableSelectedColor := dxOffice11ToolbarSelectedColor;
  // static
  dxOffice11StaticBorderColor := dxOffice11DropDownBorderColor1;
  // date
  dxOffice11DateHeaderColor := dxOffice11DockColor1;
  // inplace sub item
  dxOffice11InPlaceSubItemColor := dxOffice11BarBorderMarkColors[2, 1];
  dxOffice11InPlaceSubItemTextColor := dxOffice11BarFloatingCaptionTextColor1;
  // LF painters
  dxOffice11ControlBorderColor := dxOffice11DropDownBorderColor1;
end;

procedure CreateOffice11Colors;
begin
  if FCounter = 0 then
  begin
    InitOffice11Colors;
    CreateOffice11Brushes;
  end;
  Inc(FCounter);
end;

procedure ReleaseOffice11Colors;
begin
  Dec(FCounter);
  if FCounter = 0 then
    DestroyOffice11Brushes;
end;

procedure RefreshOffice11Colors;
begin
  InitOffice11Colors;
  if FCounter <> 0 then
  begin
    DestroyOffice11Brushes;
    CreateOffice11Brushes;
  end;
end;

function IsXPStandardScheme: Boolean;
begin
  Result := FXPStandardScheme;
end;

procedure Office11FillTubeGradientRect(DC: HDC; const ARect: TRect; AColor1, AColor2: TColor; AHorizontal: Boolean);
begin
  FillTubeGradientRect(DC, ARect, AColor1, AColor2, GradientPercent, AHorizontal);
end;

procedure Office11FrameSelectedRect(DC: HDC; const R: TRect);
begin
  if IsHighContrastBlack or IsHighContrast2 then
    FrameRectByColor(DC, R, clHighlightText)
  else
    FrameRect(DC, R, dxOffice11SelectedBorderBrush);
end;

procedure Office11DrawFingerElements(DC: HDC; ARect: TRect; AHorizontal: Boolean;
  ABrush1: HBRUSH = 0; ABrush2: HBRUSH = 0; AScaleFactor: TdxScaleFactor = nil);
var
  R1, R2: TRect;
  W, ASize, AOffset: Integer;
begin
  if AScaleFactor = nil then
    AScaleFactor := dxSystemScaleFactor;

  ASize := GetOffice11FingerElementSize(AScaleFactor);
  AOffset := AScaleFactor.Apply(1);
  with ARect do
    R1 := Rect(Left, Top, Left + ASize, Top + ASize);
  if AHorizontal then
  begin
    W := ARect.Bottom - ARect.Top;
    W := W - (W div 4) * ASize;
    if W > AOffset then W := W div 2;
    OffsetRect(R1, 0, W);
  end
  else
  begin
    W := ARect.Right - ARect.Left;
    W := W - (W div 4) * ASize;
    if W > AOffset then W := W div 2;
    OffsetRect(R1, W, 0);
  end;
  if ABrush1 = 0 then
     ABrush1 := dxOffice11BarFingerBrush1;
  if ABrush2 = 0 then
     ABrush2 := dxOffice11BarFingerBrush2;
  repeat
    R2 := R1;
    InflateRect(R2, -AOffset, -AOffset);
    FillRect(DC, R2, ABrush2);
    OffsetRect(R2, -AOffset, -AOffset);
    FillRect(DC, R2, ABrush1);
    if AHorizontal then
    begin
      OffsetRect(R1, 0, ASize);
      if R1.Bottom > ARect.Bottom then Break;
    end
    else
    begin
      OffsetRect(R1, ASize, 0);
      if R1.Right > ARect.Right then Break;
    end;
  until False;
end;

procedure Office11DrawItemArrow(DC: HDC; R: TRect; ADownArrow: Boolean; Enabled, Selected, Flat: Boolean);
var
  AArrowType: TcxArrowDirection;
  Size: Integer;
begin
  if not ADownArrow then
  begin
    AArrowType := adRight;
    Size := R.Bottom - R.Top - 6
  end
  else  // atDown
  begin
    AArrowType := adDown;
    Size := R.Right - R.Left - 8;
  end;
  Size := (Size - 1) div 2 + Byte(Size mod 2 <> 0);
  if Size < 3 then Size := 3;
  Office11DrawLargeItemArrow(DC, R, AArrowType, Size, Selected, Enabled, Flat);
end;

procedure Office11DrawLargeItemArrow(ADC: HDC; R: TRect; AArrowType: TcxArrowDirection; ASize: Integer;
  ASelected, AEnabled, AFlat: Boolean);
var
  AColor: COLORREF;
  X, Y: Integer;
  APoints: array[1..3] of TPoint;
  APen: HPEN;
  ABrush: HBRUSH;

  procedure DrawEnabled;
  begin
    case AArrowType of
      adDown:
        begin
          X := (R.Left + R.Right - (2 * ASize - 1)) div 2;
          Y := (R.Top + R.Bottom - ASize) div 2;
          APoints[1] := Point(X, Y);
          APoints[2] := Point(X + 2 * ASize - 2, Y);
          APoints[3] := Point(X + ASize - 1, Y + ASize - 1);
        end;
      adLeft:
        begin
          X := (R.Left + R.Right + ASize) div 2;
          Y := (R.Top + R.Bottom - (2 * ASize - 1)) div 2;
          APoints[1] := Point(X, Y);
          APoints[2] := Point(X, Y + 2 * ASize - 2);
          APoints[3] := Point(X - ASize + 1, Y + ASize - 1);
        end;
    else //adRight, adTop
      begin
        X := (R.Left + R.Right - ASize) div 2;
        Y := (R.Top + R.Bottom - (2 * ASize - 1)) div 2;
        APoints[1] := Point(X, Y);
        APoints[2] := Point(X, Y + 2 * ASize - 2);
        APoints[3] := Point(X + ASize - 1, Y + ASize - 1);
      end
    end;

    APen := SelectObject(ADC, CreatePen(PS_SOLID, 1, AColor));
    ABrush := SelectObject(ADC, CreateSolidBrush(AColor));
    Polygon(ADC, APoints, 3);
    DeleteObject(SelectObject(ADC, ABrush));
    DeleteObject(SelectObject(ADC, APen));
  end;

begin
  if AEnabled then
  begin
    if ASelected and IsHighContrastWhite then
      AColor := clWhite
    else
      AColor := dxOffice11TextEnabledColor;
  end
  else
    AColor := dxOffice11TextDisabledColor;
  DrawEnabled;
end;

procedure Office11DrawSizeGrip(DC: HDC; ARect: TRect; AColor1: TColor = clDefault;
  AColor2: TColor = clDefault; AScaleFactor: TdxScaleFactor = nil);
var
  ABrush1, ABrush2: HBRUSH;
begin
  if AScaleFactor = nil then
    AScaleFactor := dxSystemScaleFactor;

  ABrush1 := 0;
  ABrush2 := 0;
  if AColor1 <> clDefault then
    ABrush1 := CreateSolidBrush(ColorToRGB(AColor1));
  if AColor2 <> clDefault then
    ABrush2 := CreateSolidBrush(ColorToRGB(AColor2));
  ARect := Rect(ARect.Right - 12, ARect.Bottom - AScaleFactor.Apply(3), ARect.Right, ARect.Bottom);
  Office11DrawFingerElements(DC, ARect, False, ABrush1, ABrush2, AScaleFactor); // 3
  Inc(ARect.Left, 4);
  OffsetRect(ARect, 0, -GetOffice11FingerElementSize(AScaleFactor));
  Office11DrawFingerElements(DC, ARect, False, ABrush1, ABrush2, AScaleFactor); // 2
  Inc(ARect.Left, 4);
  OffsetRect(ARect, 0, -GetOffice11FingerElementSize(AScaleFactor));
  Office11DrawFingerElements(DC, ARect, False, ABrush1, ABrush2, AScaleFactor); // 1
  if ABrush1 <> 0 then DeleteObject(ABrush1);
  if ABrush2 <> 0 then DeleteObject(ABrush2);
end;

function GetOffice11FingerElementSize(AScaleFactor: TdxScaleFactor = nil): Integer;
begin
  if AScaleFactor = nil then
    AScaleFactor := dxSystemScaleFactor;
  Result := AScaleFactor.Apply(4);
end;

end.
