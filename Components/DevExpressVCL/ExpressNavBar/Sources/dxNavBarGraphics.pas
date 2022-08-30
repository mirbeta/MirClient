{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressNavBar                                            }
{                                                                    }
{           Copyright (c) 2002-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSNAVBAR AND ALL ACCOMPANYING    }
{   VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY.              }
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

unit dxNavBarGraphics;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, Windows, Classes, Controls, CommCtrl, Graphics, ImgList,
  cxClasses, cxControls, cxGraphics, dxNavBar, dxNavBarStyles, dxNavBarCustomPainters;

type
  TXPScheme = (schUnknown, schNormalColor, schHomestead, schMetallic);
  TXPSchemeColor = schNormalColor .. schMetallic;

var
  // AdvExplorerBar bitmaps, colors and fonts
  dxAdvExplorerBarGroupCaptionCollapseSignBitmap: TBitmap;
  dxAdvExplorerBarGroupCaptionExpandSignBitmap: TBitmap;
//  dxAdvExplorerBarSpecialGroupCaptionCollapseSignBitmap: TBitmap;
//  dxAdvExplorerBarSpecialGroupCaptionExpandSignBitmap: TBitmap;

  dxAdvExplorerBarBackgroundColor1: COLORREF;
  dxAdvExplorerBarBackgroundColor2: COLORREF;
  dxAdvExplorerBarFontColor: COLORREF;
  dxAdvExplorerBarFontHotColor: COLORREF;
  dxAdvExplorerBarGroupBackgroundColor: COLORREF;
  dxAdvExplorerBarGroupCaptionColor1: COLORREF;
  dxAdvExplorerBarGroupCaptionColor2: COLORREF;
  dxAdvExplorerBarGroupCaptionSignColor: COLORREF;
  dxAdvExplorerBarGroupCaptionSignHotColor: COLORREF;
  dxAdvExplorerBarGroupFontColor: COLORREF;
  dxAdvExplorerBarGroupFontHotColor: COLORREF;
  dxAdvExplorerBarSpecialGroupCaptionColor1: COLORREF;
  dxAdvExplorerBarSpecialGroupCaptionColor2: COLORREF;
  dxAdvExplorerBarSpecialGroupCaptionSignColor: COLORREF;
  dxAdvExplorerBarSpecialGroupCaptionSignHotColor: COLORREF;
  dxAdvExplorerBarSpecialGroupFontColor: COLORREF;
  dxAdvExplorerBarSpecialGroupFontHotColor: COLORREF;

  // Office11 color and bitmaps
  dxOffice11BackgroundColor1: COLORREF;
  dxOffice11BackgroundColor2: COLORREF;
  dxOffice11GroupBackgroundColor1: COLORREF;
  dxOffice11GroupBackgroundColor2: COLORREF;
  dxOffice11GroupCaptionColor1: COLORREF;
  dxOffice11GroupCaptionColor2: COLORREF;
  dxOffice11GroupBorderColor: COLORREF;
  dxOffice11GroupFontColor: COLORREF;
  dxOffice11LinkFontColor: COLORREF;

  dxOffice11CaptionExpandSignBitmap: TBitmap;
  dxOffice11CaptionCollapseSignBitmap: TBitmap;

  // Office11 NavPane bitmaps
  dxOffice11NavPaneOverflowPanelBitmap: TBitmap;
  dxOffice11NavPaneSplitterBitmap: TBitmap;
  dxOffice11NavPaneArrowUpBitmap: TBitmap;
  dxOffice11NavPaneArrowDownBitmap: TBitmap;
  dxOffice11NavPaneDefaultLargeBitmap: TBitmap;
  dxOffice11NavPaneDefaultSmallBitmap: TBitmap;

  // XPExplorerBar bitmaps, colors and fonts
  dxXPExplorerBarSpecialGroupHeader: TBitmap;
  dxXPExplorerBarNormalGroupHeader: TBitmap;
  dxXPExplorerBarSpecialGroupCollapseHot: TBitmap;
  dxXPExplorerBarNormalGroupCollapseHot: TBitmap;
  dxXPExplorerBarSpecialGroupCollapse: TBitmap;
  dxXPExplorerBarNormalGroupCollapse: TBitmap;
  dxXPExplorerBarSpecialGroupExpandHot: TBitmap;
  dxXPExplorerBarNormalGroupExpandHot: TBitmap;
  dxXPExplorerBarSpecialGroupExpand: TBitmap;
  dxXPExplorerBarNormalGroupExpand: TBitmap;
  dxXPExplorerBarBackground: TBitmap;

  dxXPExplorerBarItemFont: TFont;
  dxXPExplorerBarItemFontHot: TFont;
  dxXPExplorerBarGroupHeaderFont: TFont;
  dxXPExplorerBarGroupHeaderFontHot: TFont;

  dxXPExplorerBarBackgroundColor1: TColor = clNone; // TODO ???
  dxXPExplorerBarBackgroundColor2: TColor = clNone; // TODO ???
  dxXPExplorerBarGroupBackgroundColor1: TColor = clNone; // TODO ???
  dxXPExplorerBarGroupBackgroundColor2: TColor = clNone; // TODO ???
  dxXPExplorerBarGroupBorderColor: TColor = clNone;

procedure CreateAdvExplorerBarColors;
procedure RefreshAdvExplorerBarColors;
procedure ReleaseAdvExplorerBarColors;

procedure CreateOffice11Colors;
procedure RefreshOffice11Colors;
procedure ReleaseOffice11Colors;

procedure CreateOffice11NavPaneColors;
procedure RefreshOffice11NavPaneColors;
procedure ReleaseOffice11NavPaneColors;

function CheckShellInstance: Boolean;
procedure CreateXPExplorerBarColors;
procedure RefreshXPExplorerBarColors;
procedure ReleaseXPExplorerBarColors;

function XPScheme: TXPScheme;

function LightColor(AColor: TColor): TColor;
function LightLightColor(AColor: TColor): TColor;
function DarkColor(AColor: TColor): TColor;
function DarkDarkColor(AColor: TColor): TColor;
function LightBorderColor(AColor: TColor): TColor;
function LightLightBorderColor(AColor: TColor): TColor;
function DarkBorderColor(AColor: TColor): TColor;
function DarkDarkBorderColor(AColor: TColor): TColor;
procedure GetSystemFont(AFont: TFont);

procedure dxNavBarDrawSelectedFrame(ACanvas: TCanvas; const ARect: TRect; ATopLeftOuterColor, ABottomRightOuterColor,
  ATopLeftInnerColor, ABottomRightInnerColor: TColor);


type
  ARGB64 = Int64;
  {$EXTERNALSYM ARGB64}

function IsGdiPlusAvailable: Boolean;

procedure GdipFillGradientRect(DC: HDC; const ARect: TRect; AColor1, AColor2: TColor;
  AAlphaBlend1, AAlphaBlend2: Byte; AGradientMode: TdxBarStyleGradientMode);

implementation

uses
  SysUtils, Math, cxGeometry, dxGDIPlusAPI, dxGDIPlusClasses, dxCoreGraphics,
  dxThemeConsts, dxUxTheme, dxThemeManager, dxOffice11;

const
  ShellLibraryName = 'shell32.dll';

type
  TdxSHGetShellStyleHInstance = function: LongWord; stdcall;
  PdxSHGetShellStyleHInstance = ^TdxSHGetShellStyleHInstance;

var
  FCounterAdvExplorerBar: Integer;
  FCounterOffice11: Integer;
  FCounterOffice11NavPane: Integer;
  FCounterXPExplorerBar: Integer;

  FShellLibrary: HMODULE;

  FSHGetShellStyleHInstancePointer: PdxSHGetShellStyleHInstance;
  FSHGetShellStyleHInstance: TdxSHGetShellStyleHInstance;
  FShellInstance: LongInt;

{ TdxStyleSheetParser }

type
  TdxStyleSheetParser = class
  private
    FStyles: TStringList;

    function TrimChars(AText: string): string;
    function LeftString(AText: string; APos: Integer): string;
    function RightString(AText: string; APos: Integer): string;
    function MiddleString(AText: string; AStartPos, AEndPos: Integer): string;
  protected
    function GetStyleText(AStyleName: string): string;
    function GetStyleAttributeText(AStyleText, AAttributeName: string): string;

    procedure ParseColorName(var AColor: TColor; AAttributeText: string);
    procedure ParseColor(var AColor: TColor; AAttributeText: string);

    procedure ParseFontWeight(AFont: TFont; AStyleText: string);
    procedure ParseFontDecoration(AFont: TFont; AStyleText: string);
    procedure ParseFontColor(AFont: TFont; AStyleText: string);
    procedure ParseFontSize(AFont: TFont; AStyleText: string);
  public
    constructor Create(AText: string);
    destructor Destroy; override;

    procedure ParseBackgroundColor(var AColor: TColor; AStyleName: string);
    procedure ParseBorderColor(var AColor: TColor; AStyleName: string);
    procedure ParseFont(AFont: TFont; AStyleName: string);
  end;

constructor TdxStyleSheetParser.Create(AText: string);
begin
  FStyles := TStringList.Create;
  FStyles.Text := Trim(AText);
end;

destructor TdxStyleSheetParser.Destroy;
begin
  FStyles.Free;
  inherited;
end;

procedure TdxStyleSheetParser.ParseBackgroundColor(var AColor: TColor; AStyleName: string);
var
  AStyleText, AStyleAttributeText: string;
begin
  AStyleText := StringReplace(GetStyleText(AStyleName), ' ', '', [rfReplaceAll]);
  if AStyleText <> '' then
  begin
    AStyleAttributeText := GetStyleAttributeText(AStyleText, 'BACKGROUND-COLOR:');
    ParseColor(AColor, StringReplace(AStyleAttributeText, ' ', '', [rfReplaceAll]));
  end
end;

procedure TdxStyleSheetParser.ParseBorderColor(var AColor: TColor; AStyleName: string);
var
  APos: Integer;
  AStyleText, AStyleAttributeText: string;
begin
  AStyleText := StringReplace(GetStyleText(AStyleName), ' ', '', [rfReplaceAll]);
  if AStyleText <> '' then
  begin
    AStyleAttributeText := GetStyleAttributeText(AStyleText, 'BORDER:');
    AStyleAttributeText := StringReplace(AStyleAttributeText, '''', '', [rfReplaceAll]);
    APos := Pos(' ', AStyleAttributeText);
    if APos > 0 then
      AStyleAttributeText := RightString(AStyleAttributeText, APos + 1);
    APos := Pos(' ', AStyleAttributeText);
    if APos > 0 then
      AStyleAttributeText := RightString(AStyleAttributeText, APos + 1);
    ParseColor(AColor, StringReplace(AStyleAttributeText, ' ', '', [rfReplaceAll]));
  end;
end;

procedure TdxStyleSheetParser.ParseFont(AFont: TFont; AStyleName: string);
var
  AStyleText: string;
begin
  AStyleText := StringReplace(GetStyleText(AStyleName), ' ', '', [rfReplaceAll]);
  if AStyleText <> '' then
  begin
    ParseFontSize(AFont, AStyleText);
    ParseFontWeight(AFont, AStyleText);
    ParseFontDecoration(AFont, AStyleText);
    ParseFontColor(AFont, AStyleText);
  end;
end;

procedure TdxStyleSheetParser.ParseFontWeight(AFont: TFont; AStyleText: string);
var
  AFontWeight: string;
begin
  AFontWeight := UpperCase(GetStyleAttributeText(AStyleText, 'FONT-WEIGHT:'));
  if AFontWeight = 'BOLD' then
    AFont.Style := AFont.Style + [fsBold]
  else if AFontWeight = 'NORMAL' then
    AFont.Style := AFont.Style - [fsBold];
end;

procedure TdxStyleSheetParser.ParseFontDecoration(AFont: TFont; AStyleText: string);
var
  AFontDecoration: string;
begin
  AFontDecoration := UpperCase(GetStyleAttributeText(AStyleText, 'TEXT-DECORATION:'));
  if AFontDecoration = 'UNDERLINE' then
    AFont.Style := AFont.Style + [fsUnderline]
  else if AFontDecoration = 'NONE' then
    AFont.Style := AFont.Style - [fsUnderline];
end;

procedure TdxStyleSheetParser.ParseFontColor(AFont: TFont; AStyleText: string);
var
  AFontColor: TColor;
  AFontColorString: string;
begin
  AFontColorString := GetStyleAttributeText(AStyleText, 'COLOR:');
  if AFontColorString <> '' then
  begin
    ParseColor(AFontColor, AFontColorString);
    if AFontColor > 0 then
      AFont.Color := AFontColor;
  end;
end;

procedure TdxStyleSheetParser.ParseFontSize(AFont: TFont; AStyleText: string);
var
  AFontSize: string;
begin
  AFontSize := TrimChars(GetStyleAttributeText(AStyleText, 'FONT-SIZE:'));
  if AFontSize <> '' then
    AFont.Size := StrToInt(AFontSize);
end;

function TdxStyleSheetParser.GetStyleText(AStyleName: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to FStyles.Count - 1 do
    if Pos(UpperCase(AStyleName), UpperCase(FStyles[I])) = 1 then
    begin
      Result := FStyles[I];
      break;
    end;
end;

function TdxStyleSheetParser.GetStyleAttributeText(AStyleText, AAttributeName: string): string;
var
  ABeginPos, AEndPos: Integer;
begin
  Result := '';
  ABeginPos := Pos(UpperCase(AAttributeName), UpperCase(AStyleText));
  if ABeginPos > 0 then
  begin
    ABeginPos := ABeginPos + Length(AAttributeName);
    Result := RightString(AStyleText, ABeginPos);
    AEndPos := Pos(';', Result);
    if AEndPos = 0 then
      AEndPos := Pos('}', Result);
    if AEndPos > 0 then
      Result := LeftString(Result, AEndPos);
  end;
end;

procedure TdxStyleSheetParser.ParseColorName(var AColor: TColor; AAttributeText: string);
begin
  cxColorByName(AAttributeText, AColor);
end;

procedure TdxStyleSheetParser.ParseColor(var AColor: TColor; AAttributeText: string);
var
  APos: Integer;
  R, G, B: Byte;
begin
  APos := Pos('#', AAttributeText);
  if APos > 0 then
  begin
    AAttributeText := RightString(AAttributeText, APos + 1);
    R := StrToInt('$' + MiddleString(AAttributeText, 1, 2));
    G := StrToInt('$' + MiddleString(AAttributeText, 3, 4));
    B := StrToInt('$' + MiddleString(AAttributeText, 5, 6));
    AColor := RGB(R, G, B);
  end
  else
    ParseColorName(AColor, AAttributeText);
end;

function TdxStyleSheetParser.TrimChars(AText: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(AText) do
    if (AText[I] >= '0') and (AText[I] <= '9') then
      Result := Result + AText[I];
end;

function TdxStyleSheetParser.LeftString(AText: string; APos: Integer): string;
begin
  Result := Copy(AText, 1, APos - 1);
end;

function TdxStyleSheetParser.RightString(AText: string; APos: Integer): string;
begin
  Result := Copy(AText, APos, Length(AText) - APos + 1);
end;

function TdxStyleSheetParser.MiddleString(AText: string; AStartPos, AEndPos: Integer): string;
begin
  Result := Copy(AText, AStartPos, AEndPos - AStartPos + 1);
end;

{ utilities }

procedure InitAdvExplorerBarColors;
const
  Colors: array[TXPSchemeColor, 0..11] of COLORREF = (
    ($E7A27B, $D67563, $C65D21, $FF8E42, $F7DFD6, $FFFFFF, $F7D3C6, $B54900, $CE5D29, $A53C00, $FFCDAC, $FF8E42),
    ($ADD9CC, $84BDA5, $2D6656, $1D9272, $ECF6F6, $ECFCFF, $B8E7E0, $408C77, $67A896, $1C674B, $BEEDDD, $B8E7E0),
    ($D4C8C4, $C8B3B1, $3D3D3F, $7C7C7E, $F5F1F0, $FFFFFF, $E0D7D6, $927777, $C7B6B4, $734431, $FEFEF9, $E6E6E6)
  );
var
  AColorScheme: TXPScheme;
begin
  AColorScheme := XPScheme;
  if AColorScheme <> schUnknown then
  begin
    dxAdvExplorerBarBackgroundColor1 := Colors[AColorScheme, 0];
    dxAdvExplorerBarBackgroundColor2 := Colors[AColorScheme, 1];
    dxAdvExplorerBarFontColor := Colors[AColorScheme, 2];
    dxAdvExplorerBarFontHotColor := Colors[AColorScheme, 3];
    dxAdvExplorerBarGroupBackgroundColor := Colors[AColorScheme, 4];
    dxAdvExplorerBarGroupCaptionColor1 := Colors[AColorScheme, 5];
    dxAdvExplorerBarGroupCaptionColor2 := Colors[AColorScheme, 6];
    dxAdvExplorerBarGroupFontColor := dxAdvExplorerBarFontColor;
    dxAdvExplorerBarGroupFontHotColor := dxAdvExplorerBarFontHotColor;
    dxAdvExplorerBarSpecialGroupCaptionColor1 := Colors[AColorScheme, 7];
    dxAdvExplorerBarSpecialGroupCaptionColor2 := Colors[AColorScheme, 8];
    dxAdvExplorerBarGroupCaptionSignColor := Colors[AColorScheme, 9];
    dxAdvExplorerBarGroupCaptionSignHotColor := dxAdvExplorerBarFontHotColor;
    dxAdvExplorerBarSpecialGroupCaptionSignColor := dxAdvExplorerBarGroupCaptionSignColor;//clWhite;
    dxAdvExplorerBarSpecialGroupCaptionSignHotColor := dxAdvExplorerBarGroupCaptionSignHotColor;//Colors[AColorScheme, 10];
    dxAdvExplorerBarSpecialGroupFontColor := clWhite;
    dxAdvExplorerBarSpecialGroupFontHotColor := Colors[AColorScheme, 11];
  end
  else
  begin
    dxAdvExplorerBarBackgroundColor1 := ColorToRGB(clInactiveCaption);
    dxAdvExplorerBarBackgroundColor2 := dxAdvExplorerBarBackgroundColor1;
    dxAdvExplorerBarFontColor := dxMakeAlphaColor(255, 10, 15, 2);
    dxAdvExplorerBarFontHotColor := LightColor(dxAdvExplorerBarFontColor);
    dxAdvExplorerBarGroupBackgroundColor := LightLightColor(clInactiveCaption);
    dxAdvExplorerBarGroupCaptionColor1 := ColorToRGB(clWindow);
    dxAdvExplorerBarGroupCaptionColor2 := LightLightColor(clHighlight);
    dxAdvExplorerBarGroupFontColor := dxAdvExplorerBarFontColor;
    dxAdvExplorerBarGroupFontHotColor := dxAdvExplorerBarFontHotColor;
    dxAdvExplorerBarSpecialGroupCaptionColor1 := ColorToRGB(clHighlight);
    dxAdvExplorerBarSpecialGroupCaptionColor2 := dxAdvExplorerBarSpecialGroupCaptionColor1;
    dxAdvExplorerBarGroupCaptionSignColor := dxAdvExplorerBarGroupFontColor;
    dxAdvExplorerBarGroupCaptionSignHotColor := dxAdvExplorerBarGroupFontHotColor;
    dxAdvExplorerBarSpecialGroupCaptionSignColor := dxAdvExplorerBarSpecialGroupCaptionColor2;
    dxAdvExplorerBarSpecialGroupCaptionSignHotColor := dxAdvExplorerBarSpecialGroupCaptionColor2;
    dxAdvExplorerBarSpecialGroupFontColor := ColorToRGB(clCaptionText);
    dxAdvExplorerBarSpecialGroupFontHotColor := dxAdvExplorerBarSpecialGroupFontColor;
  end;
end;

procedure CreateAdvExplorerBarBitmaps;
begin
  dxAdvExplorerBarGroupCaptionCollapseSignBitmap := TBitmap.Create;
  dxAdvExplorerBarGroupCaptionCollapseSignBitmap.Handle := LoadBitmap(HInstance, 'DXNAVBARADVEXPLORERBARGROUPCAPTIONCOLLAPSESIGN');
  dxAdvExplorerBarGroupCaptionExpandSignBitmap := TBitmap.Create;
  dxAdvExplorerBarGroupCaptionExpandSignBitmap.Handle := LoadBitmap(HInstance, 'DXNAVBARADVEXPLORERBARGROUPCAPTIONEXPANDSIGN');
//  dxAdvExplorerBarSpecialGroupCaptionCollapseSignBitmap := TBitmap.Create;
//  dxAdvExplorerBarSpecialGroupCaptionCollapseSignBitmap.Handle := LoadBitmap(HInstance, 'DXNAVBARADVEXPLORERBARSPECIALGROUPCAPTIONCOLLAPSESIGN');
//  dxAdvExplorerBarSpecialGroupCaptionExpandSignBitmap := TBitmap.Create;
//  dxAdvExplorerBarSpecialGroupCaptionExpandSignBitmap.Handle := LoadBitmap(HInstance, 'DXNAVBARADVEXPLORERBARSPECIALGROUPCAPTIONEXPANDSIGN');
end;

procedure DestroyAdvExplorerBarBitmaps;
begin
  dxAdvExplorerBarGroupCaptionCollapseSignBitmap.Free;
  dxAdvExplorerBarGroupCaptionCollapseSignBitmap := nil;
  dxAdvExplorerBarGroupCaptionExpandSignBitmap.Free;
  dxAdvExplorerBarGroupCaptionExpandSignBitmap := nil;
//  dxAdvExplorerBarSpecialGroupCaptionCollapseSignBitmap.Free;
//  dxAdvExplorerBarSpecialGroupCaptionCollapseSignBitmap := nil;
//  dxAdvExplorerBarSpecialGroupCaptionExpandSignBitmap.Free;
//  dxAdvExplorerBarSpecialGroupCaptionExpandSignBitmap := nil;
end;

procedure CreateAdvExplorerBarColors;
begin
  if FCounterAdvExplorerBar = 0 then
  begin
    InitAdvExplorerBarColors;
    CreateAdvExplorerBarBitmaps;
  end;
  Inc(FCounterAdvExplorerBar);
end;

procedure RefreshAdvExplorerBarColors;
begin
  InitAdvExplorerBarColors;
  if FCounterAdvExplorerBar > 0 then
  begin
    DestroyAdvExplorerBarBitmaps;
    CreateAdvExplorerBarBitmaps;
  end;
end;

procedure ReleaseAdvExplorerBarColors;
begin
  Dec(FCounterAdvExplorerBar);
  if FCounterAdvExplorerBar = 0 then
    DestroyAdvExplorerBarBitmaps;
end;

procedure InitOffice11Colors;
const
  Office11Colors: array[TXPSchemeColor, 0..8] of COLORREF = (
    ($FADAC4, $C97A4A, $FEECDD, $F1D0BA, $F9DBC4, $E08F65, $FFFFFF, $862D00, $B23D00),
    ($E7F2F3, $98C6BE, $FFFFFF, $E7F2F3, $AEDFD2, $80B0A1, $FFFFFF, $466B5A, $5E8674),
    ($F4EEEE, $C3B0B1, $FCFBFB, $F5F0F0, $DFD0D0, $BFA8A9, $FFFFFF, $795B5C, $99797A)
  );
var
  AOffice11Scheme: TXPScheme;
begin
  AOffice11Scheme := XPScheme;
  if AreVisualStylesAvailable and (AOffice11Scheme <> schUnknown){luna} then
  begin
    dxOffice11BackgroundColor1 := Office11Colors[AOffice11Scheme, 0];
    dxOffice11BackgroundColor2 := Office11Colors[AOffice11Scheme, 1];
    dxOffice11GroupBackgroundColor1 := Office11Colors[AOffice11Scheme, 2];
    dxOffice11GroupBackgroundColor2 := Office11Colors[AOffice11Scheme, 3];
    dxOffice11GroupCaptionColor1 := Office11Colors[AOffice11Scheme, 4];
    dxOffice11GroupCaptionColor2 := Office11Colors[AOffice11Scheme, 5];
    dxOffice11GroupBorderColor := Office11Colors[AOffice11Scheme, 6];
    dxOffice11GroupFontColor := Office11Colors[AOffice11Scheme, 7];
    dxOffice11LinkFontColor := Office11Colors[AOffice11Scheme, 8];
  end
  else
  begin
    dxOffice11BackgroundColor1 := dxGetMiddleRGB(clBtnFace, clWindow, 50);
    dxOffice11BackgroundColor2 := dxGetMiddleRGB(clBtnFace, clWindow, 100);
    dxOffice11GroupBackgroundColor1 := dxGetMiddleRGB(clBtnFace, clWindow, 15);
    dxOffice11GroupBackgroundColor2 := dxGetMiddleRGB(clBtnFace, clWindow, 20);
    dxOffice11GroupCaptionColor1 := dxGetMiddleRGB(clBtnFace, clWindow, 45);
    dxOffice11GroupCaptionColor2 := dxGetMiddleRGB(clBtnFace, clWindow, 70);
    dxOffice11GroupBorderColor := dxGetMiddleRGB(clBtnFace, clWindow, 5);
    dxOffice11GroupFontColor := clBlack;
    dxOffice11LinkFontColor := clBlue;
  end;
end;

procedure CreateOffice11Bitmaps;
begin
  dxOffice11CaptionExpandSignBitmap := TBitmap.Create;
  dxOffice11CaptionExpandSignBitmap.Handle := LoadBitmap(HInstance, 'DXNAVBAROFFICE11EXPANDSIGN');
  dxOffice11CaptionCollapseSignBitmap := TBitmap.Create;
  dxOffice11CaptionCollapseSignBitmap.Handle := LoadBitmap(HInstance, 'DXNAVBAROFFICE11COLLAPSESIGN');
end;

procedure DestroyOffice11Bitmaps;
begin
  FreeAndNil(dxOffice11CaptionExpandSignBitmap);
  FreeAndNil(dxOffice11CaptionCollapseSignBitmap);
end;

procedure CreateOffice11Colors;
begin
  if FCounterOffice11 = 0 then
  begin
    InitOffice11Colors;
    CreateOffice11Bitmaps;
  end;
  Inc(FCounterOffice11);
end;

procedure RefreshOffice11Colors;
begin
  InitOffice11Colors;
  if FCounterOffice11 <> 0 then
  begin
    DestroyOffice11Bitmaps;
    CreateOffice11Bitmaps;
  end;
end;

procedure ReleaseOffice11Colors;
begin
  Dec(FCounterOffice11);
  if FCounterOffice11 = 0 then
    DestroyOffice11Bitmaps;
end;

procedure CreateOffice11NavPaneBitmaps;
begin
  dxOffice11NavPaneOverflowPanelBitmap := TBitmap.Create;
  dxOffice11NavPaneOverflowPanelBitmap.Handle := LoadBitmap(HInstance, 'DXNAVBAROFFICE11NAVPANEOVERFLOW');
  dxOffice11NavPaneOverflowPanelBitmap.Transparent := True;
  dxOffice11NavPaneSplitterBitmap := TBitmap.Create;
  dxOffice11NavPaneSplitterBitmap.Handle := LoadBitmap(HInstance, 'DXNAVBAROFFICE11NAVPANESPLITTER');
  dxOffice11NavPaneSplitterBitmap.Transparent := True;
  dxOffice11NavPaneArrowUpBitmap := TBitmap.Create;
  dxOffice11NavPaneArrowUpBitmap.Handle := LoadBitmap(HInstance, 'DXNAVBAROFFICE11NAVPANEARROWUP');
  dxOffice11NavPaneArrowUpBitmap.Transparent := True;
  dxOffice11NavPaneArrowDownBitmap := TBitmap.Create;
  dxOffice11NavPaneArrowDownBitmap.Handle := LoadBitmap(HInstance, 'DXNAVBAROFFICE11NAVPANEARROWDOWN');
  dxOffice11NavPaneArrowDownBitmap.Transparent := True;
  dxOffice11NavPaneDefaultLargeBitmap := TBitmap.Create;
  dxOffice11NavPaneDefaultLargeBitmap.Handle := LoadBitmap(HInstance, 'DXNAVBAROFFICE11NAVPANEDEFAULTLARGE');
  dxOffice11NavPaneDefaultLargeBitmap.Transparent := True;
  dxOffice11NavPaneDefaultSmallBitmap := TBitmap.Create;
  dxOffice11NavPaneDefaultSmallBitmap.Handle := LoadBitmap(HInstance, 'DXNAVBAROFFICE11NAVPANEDEFAULTSMALL');
  dxOffice11NavPaneDefaultSmallBitmap.Transparent := True;
end;

procedure DestroyOffice11NavPaneBitmaps;
begin
  FreeAndNil(dxOffice11NavPaneArrowUpBitmap);
  FreeAndNil(dxOffice11NavPaneArrowDownBitmap);
  FreeAndNil(dxOffice11NavPaneOverflowPanelBitmap);
  FreeAndNil(dxOffice11NavPaneSplitterBitmap);
  FreeAndNil(dxOffice11NavPaneDefaultLargeBitmap);
  FreeAndNil(dxOffice11NavPaneDefaultSmallBitmap);
end;

procedure CreateOffice11NavPaneColors;
begin
  if FCounterOffice11NavPane = 0 then
    CreateOffice11NavPaneBitmaps;
  Inc(FCounterOffice11NavPane);
end;

procedure RefreshOffice11NavPaneColors;
begin
  if FCounterOffice11NavPane <> 0 then
  begin
    DestroyOffice11NavPaneBitmaps;
    CreateOffice11NavPAneBitmaps;
  end;
end;

procedure ReleaseOffice11NavPaneColors;
begin
  Dec(FCounterOffice11NavPane);
  if FCounterOffice11NavPane = 0 then
    DestroyOffice11NavPaneBitmaps;
end;

function GetResourceImage(AID: Byte): TBitmap;
var
  AResType, AResID: PChar;
  AResInfo: THandle;
begin
  Result := nil;
  if CheckShellInstance then
  begin
    AResType := RT_BITMAP;
    AResID := PChar(char(AID));
    AResInfo := FindResource(FShellInstance, AResID, AResType);
    if AResInfo > 0 then
    begin
      Result := TBitmap.Create;
      Result.LoadFromResourceID(FShellInstance, AID);
    end;
  end;
end;

function GetResourceText(AName: string): string;
var
  AResType, AResName: PChar;
  AStream: TResourceStream;
  AResInfo: THandle;
begin
  Result := '';
  if CheckShellInstance then
  begin
    AResType := PChar(23);
    AResName := PChar(AName);
    AResInfo := FindResource(FShellInstance, AResName, AResType);
    if AResInfo > 0 then
    begin
      AStream := TResourceStream.Create(FShellInstance, AResName, AResType);
      try
        SetString(Result, PAnsiChar(AStream.Memory), AStream.Size);
      finally
        AStream.Free;
      end;
    end;
  end;
end;

procedure CreateXPExplorerBarBitmaps;
begin
  dxXPExplorerBarSpecialGroupHeader := GetResourceImage(110);
  dxXPExplorerBarNormalGroupHeader := GetResourceImage(112);
  dxXPExplorerBarSpecialGroupCollapseHot := GetResourceImage(105);
  dxXPExplorerBarNormalGroupCollapseHot := GetResourceImage(101);
  dxXPExplorerBarSpecialGroupCollapse := GetResourceImage(104);
  dxXPExplorerBarNormalGroupCollapse := GetResourceImage(100);
  dxXPExplorerBarSpecialGroupExpandHot := GetResourceImage(107);
  dxXPExplorerBarNormalGroupExpandHot := GetResourceImage(103);
  dxXPExplorerBarSpecialGroupExpand := GetResourceImage(106);
  dxXPExplorerBarNormalGroupExpand := GetResourceImage(102);
  dxXPExplorerBarBackground := GetResourceImage(114);
end;

procedure CreateXPExplorerBarStyleSheetAttributes;
var
  AStyleSheetText: string;
  AParser: TdxStyleSheetParser;
begin
  AStyleSheetText := GetResourceText('CPWEBVW.CSS');
  AParser := TdxStyleSheetParser.Create(AStyleSheetText);
  try
    dxXPExplorerBarItemFont := TFont.Create;
    dxXPExplorerBarItemFontHot := TFont.Create;
    dxXPExplorerBarGroupHeaderFont := TFont.Create;
    dxXPExplorerBarGroupHeaderFontHot := TFont.Create;

    GetSystemFont(dxXPExplorerBarItemFont);
    GetSystemFont(dxXPExplorerBarItemFontHot);
    GetSystemFont(dxXPExplorerBarGroupHeaderFont);
    GetSystemFont(dxXPExplorerBarGroupHeaderFontHot);

    AParser.ParseFont(dxXPExplorerBarGroupHeaderFont, 'BODY ');
    AParser.ParseFont(dxXPExplorerBarGroupHeaderFontHot, 'BODY ');
    AParser.ParseFont(dxXPExplorerBarItemFont, 'BODY ');
    AParser.ParseFont(dxXPExplorerBarItemFontHot, 'BODY ');
    AParser.ParseFont(dxXPExplorerBarItemFont, 'A ');
    AParser.ParseFont(dxXPExplorerBarItemFontHot, 'A ');
    AParser.ParseFont(dxXPExplorerBarGroupHeaderFont, '.LEARNABOUTCAPTION ');
    AParser.ParseFont(dxXPExplorerBarGroupHeaderFontHot, '.LEARNABOUTCAPTION ');
    AParser.ParseFont(dxXPExplorerBarGroupHeaderFontHot, 'A:HOVER ');
    AParser.ParseFont(dxXPExplorerBarItemFontHot, 'A:HOVER ');

    AParser.ParseBorderColor(dxXPExplorerBarGroupBorderColor, '.LEARNABOUTBOX');
  finally
    AParser.Free;
  end;
end;

procedure DestroyXPExplorerBarBitmaps;
begin
  FreeAndNil(dxXPExplorerBarSpecialGroupHeader);
  FreeAndNil(dxXPExplorerBarNormalGroupHeader);
  FreeAndNil(dxXPExplorerBarSpecialGroupCollapseHot);
  FreeAndNil(dxXPExplorerBarNormalGroupCollapseHot);
  FreeAndNil(dxXPExplorerBarSpecialGroupCollapse);
  FreeAndNil(dxXPExplorerBarNormalGroupCollapse);
  FreeAndNil(dxXPExplorerBarSpecialGroupExpandHot);
  FreeAndNil(dxXPExplorerBarNormalGroupExpandHot);
  FreeAndNil(dxXPExplorerBarSpecialGroupExpand);
  FreeAndNil(dxXPExplorerBarNormalGroupExpand);
  FreeAndNil(dxXPExplorerBarBackground);
end;

procedure DestroyXPExplorerBarStyleSheetAttributes;
begin
  FreeAndNil(dxXPExplorerBarItemFont);
  FreeAndNil(dxXPExplorerBarItemFontHot);
  FreeAndNil(dxXPExplorerBarGroupHeaderFont);
  FreeAndNil(dxXPExplorerBarGroupHeaderFontHot);
  dxXPExplorerBarGroupBorderColor := clNone;
end;

procedure LoadShellInstance;
begin
  FShellLibrary := LoadLibrary(ShellLibraryName);
  if FShellLibrary <> 0 then
    FSHGetShellStyleHInstancePointer := GetProcAddress(FShellLibrary, 'SHGetShellStyleHInstance');
  if FSHGetShellStyleHInstancePointer <> nil then
    @FSHGetShellStyleHInstance := FSHGetShellStyleHInstancePointer;
end;

procedure UnloadShellInstance;
begin
  if FShellLibrary <> 0 then
    FreeLibrary(FShellLibrary);
end;

function CheckShellInstance: Boolean;
begin
  Result := FShellInstance > 0;
end;

procedure CreateXPExplorerBarColors;
begin
  if FCounterXPExplorerBar = 0 then
  begin
    if FSHGetShellStyleHInstancePointer <> nil then
    begin
      FShellInstance := FSHGetShellStyleHInstance;
    end;
    CreateXPExplorerBarBitmaps;
    CreateXPExplorerBarStyleSheetAttributes;
  end;
  Inc(FCounterXPExplorerBar);
end;

procedure RefreshXPExplorerBarColors;
begin
  if FCounterXPExplorerBar <> 0 then
  begin
    FShellInstance := 0;
    DestroyXPExplorerBarBitmaps;
    DestroyXPExplorerBarStyleSheetAttributes;
    if FSHGetShellStyleHInstancePointer <> nil then
    begin
      FShellInstance := FSHGetShellStyleHInstance;
    end;
    CreateXPExplorerBarBitmaps;
    CreateXPExplorerBarStyleSheetAttributes;
  end;
end;

procedure ReleaseXPExplorerBarColors;
begin
  Dec(FCounterXPExplorerBar);
  if FCounterXPExplorerBar = 0 then
  begin
    FShellInstance := 0;
    DestroyXPExplorerBarBitmaps;
    DestroyXPExplorerBarStyleSheetAttributes;
  end;
end;

function XPScheme: TXPScheme;
begin
  Result := TXPScheme(GetOffice11Scheme);
end;

function LightColor(AColor: TColor): TColor;
begin
  Result := dxGetLighterColor(AColor, 80);
end;

function LightLightColor(AColor: TColor): TColor;
begin
  Result := dxGetLighterColor(AColor, 40);
end;

function DarkColor(AColor: TColor): TColor;
begin
  Result := dxGetDarkerColor(AColor, 60);
end;

function DarkDarkColor(AColor: TColor): TColor;
begin
  Result := dxGetDarkerColor(AColor, 40);
end;

function LightBorderColor(AColor: TColor): TColor;
begin
  Result := dxGetLighterColor(AColor, 70);
end;

function LightLightBorderColor(AColor: TColor): TColor;
begin
  Result := dxGetLighterColor(AColor, 20);
end;

function DarkBorderColor(AColor: TColor): TColor;
begin
  Result := dxGetDarkerColor(AColor, 70);
end;

function DarkDarkBorderColor(AColor: TColor): TColor;
begin
  Result := dxGetDarkerColor(AColor, 20);
end;

procedure GetSystemFont(AFont: TFont);
var
  ANonClientMetrics: TNonClientMetrics;
begin
  if dxSystemInfo.GetParameter(SPI_GETNONCLIENTMETRICS, ANonClientMetrics) then
    AFont.Handle := Windows.CreateFontIndirect(ANonClientMetrics.lfMessageFont)
  else
    AFont.Handle := GetStockObject(DEFAULT_GUI_FONT);
end;

procedure dxNavBarDrawSelectedFrame(ACanvas: TCanvas; const ARect: TRect; ATopLeftOuterColor, ABottomRightOuterColor,
  ATopLeftInnerColor, ABottomRightInnerColor: TColor);

  function GetBorders(ATopLeftColor, ABottomRightColor: TColor): TcxBorders;
  begin
    Result := [];
    if ATopLeftColor <> clNone then
      Result := Result + [bLeft, bTop];
    if ABottomRightColor <> clNone then
      Result := Result + [bRight, bBottom];
  end;

begin
  with TcxCanvas.Create(ACanvas) do
  begin
    DrawComplexFrame(ARect, ATopLeftOuterColor, ABottomRightOuterColor,
      GetBorders(ATopLeftOuterColor, ABottomRightOuterColor));
    DrawComplexFrame(cxRectInflate(ARect, -1, -1), ATopLeftInnerColor, ABottomRightInnerColor,
      GetBorders(ATopLeftInnerColor, ABottomRightInnerColor));
    Free;
  end;
end;

function IsGdiPlusAvailable: Boolean;
begin
  Result := CheckGdiPlus;
end;

procedure GdipFillGradientRect(DC: HDC; const ARect: TRect; AColor1, AColor2: TColor;
  AAlphaBlend1, AAlphaBlend2: Byte; AGradientMode: TdxBarStyleGradientMode);
begin
  dxGpFillRectByGradient(DC, ARect, AColor1, AColor2,
    TdxGPLinearGradientMode(AGradientMode), AAlphaBlend1, AAlphaBlend2);
end;

initialization
  LoadShellInstance;

finalization
  UnloadShellInstance;

end.
