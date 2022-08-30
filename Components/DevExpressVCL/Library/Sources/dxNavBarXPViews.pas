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

unit dxNavBarXPViews;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Windows, Graphics, dxNavBar, dxNavBarCollns, dxNavBarStyles, dxNavBarGraphics,
  dxNavBarCustomPainters, dxNavBarBaseViews, dxNavBarExplorerViews, dxNavBarOfficeViews,
  dxThemeManager, dxCore, cxGeometry;

type
  TdxNavBarXP1GroupViewInfo = class(TdxNavBarExplorerBarGroupViewInfo)
  private
    function IsDefaultBgColor: Boolean;
    function IsDefaultCaptionColor: Boolean;
  public
    function BgBackColor: TColor; override;
    function BgBackColor2: TColor; override;
    function BgAlphaBlend: Byte; override;
    function BgAlphaBlend2: Byte; override;
    function BgGradientMode: TdxBarStyleGradientMode; override;
    function CaptionBackColor: TColor; override;
    function CaptionBackColor2: TColor; override;
    function CaptionAlphaBlend: Byte; override;
    function CaptionAlphaBlend2: Byte; override;
    function CaptionGradientMode: TdxBarStyleGradientMode; override;
  end;

  TdxNavBarXP1ViewInfo = class(TdxNavBarBaseViewInfo)
  public
    procedure AssignDefaultBackgroundStyle; override;
    procedure AssignDefaultGroupBackgroundStyle; override;
    procedure AssignDefaultGroupHeaderStyle; override;
    procedure AssignDefaultItemStyle; override;
    procedure AssignDefaultItemDisabledStyle; override;
  end;

  TdxNavBarXP1Painter = class(TdxNavBarFlatPainter)
  protected
    class function GetViewInfoClass: TdxNavBarViewInfoClass; override;
    class function GetGroupViewInfoClass: TdxNavBarGroupViewInfoClass; override;

    class function ScrollButtonsPainterClass: TdxNavBarCustomScrollButtonsPainterClass; override;
    class function SelectionPainterClass: TdxNavBarCustomSelectionPainterClass; override;
  public
    procedure DrawGroupCaptionButton(AGroupViewInfo: TdxNavBarGroupViewInfo); override;
  end;

  TdxNavBarXP2Painter = class(TdxNavBarOffice1Painter)
  protected
    class function BackgroundPainterClass: TdxNavBarCustomBackgroundPainterClass; override;
    class function ButtonPainterClass: TdxNavBarCustomButtonPainterClass; override;
    class function GroupBackgroundPainterClass: TdxNavBarCustomGroupBackgroundPainterClass; override;
    class function ScrollButtonsPainterClass: TdxNavBarCustomScrollButtonsPainterClass; override;
    class function SelectionPainterClass: TdxNavBarCustomSelectionPainterClass; override;
  end;

  TdxNavBarXPExplorerBarLinkViewInfo = class(TdxNavBarExplorerBarLinkViewInfo)
  public
    function Font: TFont; override;
    function FontColor: TColor; override;
  end;

  TdxNavBarXPExplorerBarGroupViewInfo = class(TdxNavBarExplorerBarGroupViewInfo)
  public
    function BorderColor: TColor; override;
    function BgBackColor: TColor; override;
    function BgBackColor2: TColor; override;
    function BgAlphaBlend: Byte; override;
    function BgAlphaBlend2: Byte; override;
    function BgGradientMode: TdxBarStyleGradientMode; override;
    function CaptionBackColor: TColor; override;
    function CaptionBackColor2: TColor; override;
    function CaptionAlphaBlend: Byte; override;
    function CaptionAlphaBlend2: Byte; override;
    function CaptionGradientMode: TdxBarStyleGradientMode; override;
    function CaptionFont: TFont; override;
    function CaptionFontColor: TColor; override;
  end;

  TdxNavBarXPExplorerBarViewInfo = class(TdxNavBarExplorerBarViewInfo)
  protected
    procedure CreateColors; override;
    procedure RefreshColors; override;
    procedure ReleaseColors; override;

    function GetGroupCaptionHeightAddon: Integer; override;
    function GetGroupCaptionSignSize: TSize; override;
  public
    function BgBackColor: TColor; override;
    function BgBackColor2: TColor; override;
    function BgAlphaBlend: Byte; override;
    function BgAlphaBlend2: Byte; override;
    function BgGradientMode: TdxBarStyleGradientMode; override;
  end;

  TdxNavBarXPExplorerBarPainter = class(TdxNavBarExplorerBarPainter)
  protected
    class function GetViewInfoClass: TdxNavBarViewInfoClass; override;
    class function GetGroupViewInfoClass: TdxNavBarGroupViewInfoClass; override;
    class function GetLinkViewInfoClass: TdxNavBarLinkViewInfoClass; override;

    class function BackgroundPainterClass: TdxNavBarCustomBackgroundPainterClass; override;
    class function ButtonPainterClass: TdxNavBarCustomButtonPainterClass; override;
    class function SignPainterClass: TdxNavBarCustomSignPainterClass; override;
  end;

  TdxNavBarXPExplorerBarButtonPainter = class(TdxNavBarCustomButtonPainter)
  protected
    class function GetButtonBitmap(AState: TdxNavBarObjectStates): TBitmap;
    class procedure InternalDrawButton(ACanvas: TCanvas; ARect: TRect; APicture: TPicture;
      AColor1, AColor2: TColor; AAlphaBlend1, AAlphaBlend2: Byte;
      AGradientMode: TdxBarStyleGradientMode; ABorderColor: TColor;
      AState: TdxNavBarObjectStates); override;
  end;

  TdxNavBarXPExplorerBarSignPainter = class(TdxNavBarExplorerBarSignPainter)
  protected
    class procedure CreateBitmap(var ADC: HDC; var ABitmap, AOldBitmap: HBITMAP; var AData: Pointer; AWidth, AHeight: Integer);
    class procedure deleteBitmap(var ADC: HDC; var ABitmap, AOldBitmap: HBITMAP; var AData: Pointer); // DeleteBitmap conflicts with C++ macro
    class procedure DrawMaskBitmap(DestX, DestY: Integer; DestHandle: THandle; Bitmap: TBitmap);

    class function GetSignBitmap(AState: TdxNavBarObjectStates): TBitmap;
    class procedure InternalDrawSign(ACanvas: TCanvas; ARect: TRect; AScaleFactor: TdxScaleFactor;
      AForeColor, ABackColor1, ABackColor2 : TColor; AState: TdxNavBarObjectStates); override;
  end;

  TdxNavBarXPPainter = class
  public
    class function DrawObject(ACanvas: TCanvas; ARect: TRect; AObject: TdxThemedObjectType;
      iPartId, iStateId: Integer): Boolean;
    class function DrawText(ACanvas: TCanvas; ARect: TRect;
      ACaption: string; ADrawTextFlag: Integer;
      AObject: TdxThemedObjectType; iPartId, iStateId: Integer): Boolean;
  end;

  TdxNavBarXPBackgroundPainter = class(TdxNavBarCustomBackgroundPainter)
  protected
    class procedure InternalDrawBackground(ACanvas: TCanvas; ARect: TRect; APicture: TPicture;
      AEraseBackground: Boolean; ABackgroundColor: TColor;
      AColor1, AColor2: TColor; AAlphaBlend1, AAlphaBlend2: Byte;
      AGradientMode: TdxBarStyleGradientMode); override;
  end;

  TdxNavBarXPGroupBackgroundPainter = class(TdxNavBarCustomGroupBackgroundPainter)
  protected
    class procedure InternalDrawBackground(ACanvas: TCanvas; ARect: TRect; APicture: TPicture;
      AColor1, AColor2: TColor; AAlphaBlend1, AAlphaBlend2: Byte;
      AGradientMode: TdxBarStyleGradientMode); override;
  end;

  TdxNavBarXPSelectionPainter = class(TdxNavBarBaseSelectionPainter)
  protected
    class procedure InternalDrawSelection(ACanvas: TCanvas; ARect: TRect;
      ABackColor: TColor; AState: TdxNavBarObjectStates); override;
  end;

  TdxNavBarXPScrollButtonsPainter = class(TdxNavBarBaseScrollButtonsPainter)
  protected
    class procedure InternalDrawBottomButton(ACanvas: TCanvas; ARect: TRect;
      AButtonPainterClass: TdxNavBarCustomButtonPainterClass; AColor1, AColor2: TColor;
      AAlphaBlend1, AAlphaBlend2: Byte; AGradientMode: TdxBarStyleGradientMode;
      ABorderColor: TColor; AState: TdxNavBarObjectStates); override;
    class procedure InternalDrawTopButton(ACanvas: TCanvas; ARect: TRect;
      AButtonPainterClass: TdxNavBarCustomButtonPainterClass; AColor1, AColor2: TColor;
      AAlphaBlend1, AAlphaBlend2: Byte; AGradientMode: TdxBarStyleGradientMode;
      ABorderColor: TColor; AState: TdxNavBarObjectStates); override;
  end;

  TdxNavBarXPButtonPainter = class(TdxNavBarOfficeButtonPainter)
  protected
    class procedure InternalDrawButton(ACanvas: TCanvas; ARect: TRect; APicture: TPicture;
      AColor1, AColor2: TColor; AAlphaBlend1, AAlphaBlend2: Byte;
      AGradientMode: TdxBarStyleGradientMode; ABorderColor: TColor;
      AState: TdxNavBarObjectStates); override;
  end;

  TdxNavBarXPExplorerBarBackgroundPainter = class(TdxNavBarCustomBackgroundPainter)
  protected
    class function GetBackgroundBitmap: TBitmap;
    class procedure InternalDrawBackground(ACanvas: TCanvas; ARect: TRect; APicture: TPicture;
      AEraseBackground: Boolean; ABackgroundColor: TColor;
      AColor1, AColor2: TColor; AAlphaBlend1, AAlphaBlend2: Byte;
      AGradientMode: TdxBarStyleGradientMode); override;
  end;

implementation

uses
  Types, CommCtrl, ImgList, Buttons,
  dxNavBarViewsFact, dxNavBarConsts,
  dxThemeConsts, dxUxTheme, cxGraphics, dxDPIAwareUtils;

{ TdxNavBarXP1GroupViewInfo }

function TdxNavBarXP1GroupViewInfo.BgAlphaBlend: Byte;
begin
  if IsDefaultBgColor then
    Result := 255
  else Result := inherited BgAlphaBlend;
end;

function TdxNavBarXP1GroupViewInfo.BgAlphaBlend2: Byte;
begin
  if IsDefaultBgColor then
    Result := 255
  else Result := inherited BgAlphaBlend2;
end;

function TdxNavBarXP1GroupViewInfo.BgBackColor: TColor;
begin
  if IsDefaultBgColor then
    Result := LightLightColor(clInactiveCaption)
  else Result := inherited BgBackColor;
end;

function TdxNavBarXP1GroupViewInfo.BgBackColor2: TColor;
begin
  if IsDefaultBgColor then
    Result := clInactiveCaption
  else Result := inherited BgBackColor2;
end;

function TdxNavBarXP1GroupViewInfo.BgGradientMode: TdxBarStyleGradientMode;
begin
  if IsDefaultBgColor then
    Result := gmHorizontal
  else Result := inherited BgGradientMode;
end;

function TdxNavBarXP1GroupViewInfo.CaptionAlphaBlend: Byte;
begin
  if IsDefaultCaptionColor then
    Result := 255
  else Result := inherited CaptionAlphaBlend;
end;

function TdxNavBarXP1GroupViewInfo.CaptionAlphaBlend2: Byte;
begin
  if IsDefaultCaptionColor then
    Result := 255
  else Result := inherited CaptionAlphaBlend2;
end;

function TdxNavBarXP1GroupViewInfo.CaptionBackColor: TColor;
begin
  if IsDefaultCaptionColor then
    Result := clActiveCaption
  else Result := inherited CaptionBackColor;
end;

function TdxNavBarXP1GroupViewInfo.CaptionBackColor2: TColor;
begin
  if IsDefaultCaptionColor then
  begin
    if Group = NavBar.HotTrackedGroup then
      Result := clActiveCaption
    else Result := DarkColor(clActiveCaption);
  end
  else Result := inherited CaptionBackColor2;
end;

function TdxNavBarXP1GroupViewInfo.CaptionGradientMode: TdxBarStyleGradientMode;
begin
  if IsDefaultCaptionColor then
    Result := gmHorizontal
  else Result := inherited CaptionGradientMode;
end;

function TdxNavBarXP1GroupViewInfo.IsDefaultBgColor: Boolean;
begin
  Result := (inherited BgBackColor = clNone) or (inherited BgBackColor2 = clNone);
end;

function TdxNavBarXP1GroupViewInfo.IsDefaultCaptionColor: Boolean;
begin
  Result := (inherited CaptionBackColor = clNone) or (inherited CaptionBackColor2 = clNone);
end;

{ TdxNavBarXP1ViewInfo }

procedure TdxNavBarXP1ViewInfo.AssignDefaultBackgroundStyle;
begin
  NavBar.DefaultStyles.Background.ResetValues;
  NavBar.DefaultStyles.Background.BackColor := clAppWorkSpace;
  NavBar.DefaultStyles.Background.BackColor2 := clAppWorkSpace;
end;

procedure TdxNavBarXP1ViewInfo.AssignDefaultGroupBackgroundStyle;
begin
  NavBar.DefaultStyles.GroupBackground.ResetValues;
  NavBar.DefaultStyles.GroupBackground.BackColor := clNone;
  NavBar.DefaultStyles.GroupBackground.BackColor2 := clNone;
end;

procedure TdxNavBarXP1ViewInfo.AssignDefaultGroupHeaderStyle;
begin
  NavBar.DefaultStyles.GroupHeader.ResetValues;
  NavBar.DefaultStyles.GroupHeader.BackColor := clNone;
  NavBar.DefaultStyles.GroupHeader.BackColor2 := clNone;
  NavBar.DefaultStyles.GroupHeader.Font.Color := clCaptionText;
  NavBar.DefaultStyles.GroupHeader.HAlignment := haCenter;
end;

procedure TdxNavBarXP1ViewInfo.AssignDefaultItemStyle;
begin
  NavBar.DefaultStyles.Item.ResetValues;
  NavBar.DefaultStyles.Item.Font.Color := clBtnText;
  NavBar.DefaultStyles.Item.HAlignment := haCenter;
end;

procedure TdxNavBarXP1ViewInfo.AssignDefaultItemDisabledStyle;
begin
  NavBar.DefaultStyles.ItemDisabled.Assign(NavBar.DefaultStyles.Item);
  NavBar.DefaultStyles.ItemDisabled.Font.Color := clBtnShadow;
end;

{ TdxNavBarXP1Painter }

procedure TdxNavBarXP1Painter.DrawGroupCaptionButton(AGroupViewInfo: TdxNavBarGroupViewInfo);
var
  Color1, Color2: TColor;
begin
  with AGroupViewInfo do
  begin
    Color1 := LightLightColor(CaptionBackColor);
    if NavBar.HotTrackedGroup = Group then
      Color2 := LightColor(CaptionBackColor)
    else Color2 := CaptionBackColor;
    ButtonPainterClass.DrawButton(Canvas, CaptionRect, CaptionImage,
      Color1, Color2, CaptionAlphaBlend, CaptionAlphaBlend2,
      CaptionGradientMode, CaptionBorderColor, State);
  end;
end;

class function TdxNavBarXP1Painter.GetViewInfoClass: TdxNavBarViewInfoClass;
begin
  Result := TdxNavBarXP1ViewInfo;
end;

class function TdxNavBarXP1Painter.GetGroupViewInfoClass: TdxNavBarGroupViewInfoClass;
begin
  Result := TdxNavBarXP1GroupViewInfo;
end;

class function TdxNavBarXP1Painter.ScrollButtonsPainterClass: TdxNavBarCustomScrollButtonsPainterClass;
begin
  Result := TdxNavBarXPScrollButtonsPainter;
end;

class function TdxNavBarXP1Painter.SelectionPainterClass: TdxNavBarCustomSelectionPainterClass;
begin
  Result := TdxNavBarXPSelectionPainter;
end;

{ TdxNavBarXP2Painter }

class function TdxNavBarXP2Painter.BackgroundPainterClass: TdxNavBarCustomBackgroundPainterClass;
begin
  Result := TdxNavBarXPBackgroundPainter;
end;

class function TdxNavBarXP2Painter.ButtonPainterClass: TdxNavBarCustomButtonPainterClass;
begin
  Result := TdxNavBarXPButtonPainter;
end;

class function TdxNavBarXP2Painter.GroupBackgroundPainterClass: TdxNavBarCustomGroupBackgroundPainterClass;
begin
  Result := TdxNavBarXPGroupBackgroundPainter;
end;

class function TdxNavBarXP2Painter.ScrollButtonsPainterClass: TdxNavBarCustomScrollButtonsPainterClass;
begin
  Result := TdxNavBarXPScrollButtonsPainter;
end;

class function TdxNavBarXP2Painter.SelectionPainterClass: TdxNavBarCustomSelectionPainterClass;
begin
  Result := TdxNavBarXPSelectionPainter;
end;

{ TdxNavBarXPExplorerBarLinkViewInfo }

function TdxNavBarXPExplorerBarLinkViewInfo.Font: TFont;
var
  AFont: TFont;
begin
  Result := inherited Font;
  if (OpenTheme(totExplorerBar) <> 0) and CheckShellInstance then
  begin
    if sHotTracked in State then
      AFont := dxXPExplorerBarItemFontHot
    else
      AFont := dxXPExplorerBarItemFont;

    if AFont <> nil then
    begin
      dxAssignFont(Result, AFont, ScaleFactor, dxSystemScaleFactor);
      if sDisabled in State then
        Result.Style := Result.Style - [fsUnderline];
    end;
  end;
end;

function TdxNavBarXPExplorerBarLinkViewInfo.FontColor: TColor;
begin
  Result := inherited FontColor;
  if (OpenTheme(totExplorerBar) <> 0) and CheckShellInstance then
  begin
    Result := Font.Color;
    if sDisabled in State then
      Result := LightColor(Result);
  end;
end;

{ TdxNavBarXPExplorerBarGroupViewInfo }

function TdxNavBarXPExplorerBarGroupViewInfo.BorderColor: TColor;
var
  APart: Integer;
  AColor: COLORREF;
begin
  APart := EBP_NORMALGROUPBACKGROUND;
  if dxXPExplorerBarGroupBorderColor <> clNone then
    Result := dxXPExplorerBarGroupBorderColor
  else if (OpenTheme(totExplorerBar) <> 0) and not Failed(GetThemeColor(OpenTheme(totExplorerBar), APart, 0, TMT_BORDERCOLOR, AColor)) then
    Result := AColor
  else
    Result := inherited BorderColor;
end;

function TdxNavBarXPExplorerBarGroupViewInfo.BgBackColor: TColor;
var
  APart: Integer;
  AColor: COLORREF;
begin
  APart := EBP_NORMALGROUPBACKGROUND;
  if (OpenTheme(totExplorerBar) <> 0) and not Failed(GetThemeColor(OpenTheme(totExplorerBar), APart, 0, TMT_FILLCOLOR, AColor)) then
    Result := AColor
  else if (OpenTheme(totExplorerBar) <> 0) and not Failed(GetThemeColor(OpenTheme(totExplorerBar), APart, 0, TMT_GRADIENTCOLOR1, AColor)) then
    Result := AColor
  else
    Result := inherited BgBackColor;
end;

function TdxNavBarXPExplorerBarGroupViewInfo.BgBackColor2: TColor;
var
  APart: Integer;
  AColor: COLORREF;
begin
  APart := EBP_NORMALGROUPBACKGROUND;
  if (OpenTheme(totExplorerBar) <> 0) and not Failed(GetThemeColor(OpenTheme(totExplorerBar), APart, 0, TMT_FILLCOLOR, AColor)) then
    Result := AColor
  else if (OpenTheme(totExplorerBar) <> 0) and not Failed(GetThemeColor(OpenTheme(totExplorerBar), APart, 0, TMT_GRADIENTCOLOR2, AColor)) then
    Result := AColor
  else
    Result := inherited BgBackColor2;
end;

function TdxNavBarXPExplorerBarGroupViewInfo.BgAlphaBlend: Byte;
begin
  if OpenTheme(totExplorerBar) <> 0 then
    Result := 255
  else
    Result := inherited BgAlphaBlend;
end;

function TdxNavBarXPExplorerBarGroupViewInfo.BgAlphaBlend2: Byte;
begin
  if OpenTheme(totExplorerBar) <> 0 then
    Result := 255
  else
    Result := inherited BgAlphaBlend2;
end;

function TdxNavBarXPExplorerBarGroupViewInfo.BgGradientMode: TdxBarStyleGradientMode;
begin
  if OpenTheme(totExplorerBar) <> 0 then
    Result := gmHorizontal
  else
    Result := inherited BgGradientMode;
end;

function TdxNavBarXPExplorerBarGroupViewInfo.CaptionBackColor: TColor;
var
  APart: Integer;
  AColor: COLORREF;
begin
  if sSpecial in State then
    APart := EBP_SPECIALGROUPHEAD
  else
    APart := EBP_NORMALGROUPHEAD;

  if (OpenTheme(totExplorerBar) <> 0) and not Failed(GetThemeColor(OpenTheme(totExplorerBar), APart, 0, TMT_FILLCOLOR, AColor)) then
    Result := AColor
  else if (OpenTheme(totExplorerBar) <> 0) and not Failed(GetThemeColor(OpenTheme(totExplorerBar), APart, 0, TMT_GRADIENTCOLOR1, AColor)) then
    Result := AColor
  else
    Result := inherited CaptionBackColor;
end;

function TdxNavBarXPExplorerBarGroupViewInfo.CaptionBackColor2: TColor;
var
  APart: Integer;
  AColor: COLORREF;
begin
  if sSpecial in State then
    APart := EBP_SPECIALGROUPHEAD
  else
    APart := EBP_NORMALGROUPHEAD;

  if (OpenTheme(totExplorerBar) <> 0) and not Failed(GetThemeColor(OpenTheme(totExplorerBar), APart, 0, TMT_FILLCOLOR, AColor)) then
    Result := AColor
  else if (OpenTheme(totExplorerBar) <> 0) and not Failed(GetThemeColor(OpenTheme(totExplorerBar), APart, 0, TMT_GRADIENTCOLOR2, AColor)) then
    Result := AColor
  else
    Result := inherited CaptionBackColor2;
end;

function TdxNavBarXPExplorerBarGroupViewInfo.CaptionAlphaBlend: Byte;
begin
  if OpenTheme(totExplorerBar) <> 0 then
    Result := 255
  else
    Result := inherited CaptionAlphaBlend;
end;

function TdxNavBarXPExplorerBarGroupViewInfo.CaptionAlphaBlend2: Byte;
begin
  if OpenTheme(totExplorerBar) <> 0 then
    Result := 255
  else
    Result := inherited CaptionAlphaBlend2;
end;

function TdxNavBarXPExplorerBarGroupViewInfo.CaptionGradientMode: TdxBarStyleGradientMode;
begin
  if OpenTheme(totExplorerBar) <> 0 then
    Result := gmHorizontal
  else
    Result := inherited CaptionGradientMode;
end;

function TdxNavBarXPExplorerBarGroupViewInfo.CaptionFont: TFont;
var
  AFont: TFont;
begin
  Result := inherited CaptionFont;
  if (OpenTheme(totExplorerBar) <> 0) and CheckShellInstance then
  begin
    if sHotTracked in State then
      AFont := dxXPExplorerBarGroupHeaderFontHot
    else
      AFont := dxXPExplorerBarGroupHeaderFont;

    if AFont <> nil then
    begin
      dxAssignFont(Result, AFont, ScaleFactor, dxSystemScaleFactor);
      Result.Style := Result.Style - [fsUnderline];
    end;
  end;
end;

function TdxNavBarXPExplorerBarGroupViewInfo.CaptionFontColor: TColor;
begin
  if (OpenTheme(totExplorerBar) <> 0) and CheckShellInstance then
  begin
    Result := CaptionFont.Color;
    if sSpecial in State then
      Result := clWhite;
  end
  else
    Result := inherited CaptionFontColor;
end;

{ TdxNavBarXPExplorerBarViewInfo }

function TdxNavBarXPExplorerBarViewInfo.BgBackColor: TColor;
var
  AColor: COLORREF;
begin
  if (OpenTheme(totExplorerBar) <> 0) and not Failed(GetThemeColor(OpenTheme(totExplorerBar), EBP_HEADERBACKGROUND, -1, TMT_GRADIENTCOLOR1, AColor)) then
    Result := AColor
  else
    Result := inherited BgBackColor;
end;

function TdxNavBarXPExplorerBarViewInfo.BgBackColor2: TColor;
var
  AColor: COLORREF;
begin
  if (OpenTheme(totExplorerBar) <> 0) and not Failed(GetThemeColor(OpenTheme(totExplorerBar), EBP_HEADERBACKGROUND, -1, TMT_GRADIENTCOLOR2, AColor)) then
    Result := AColor
  else
    Result := inherited BgBackColor2;
end;

function TdxNavBarXPExplorerBarViewInfo.BgAlphaBlend: Byte;
begin
  if OpenTheme(totExplorerBar) <> 0 then
    Result := 255
  else
    Result := inherited BgAlphaBlend;
end;

function TdxNavBarXPExplorerBarViewInfo.BgAlphaBlend2: Byte;
begin
  if OpenTheme(totExplorerBar) <> 0 then
    Result := 255
  else
    Result := inherited BgAlphaBlend2;
end;

function TdxNavBarXPExplorerBarViewInfo.BgGradientMode: TdxBarStyleGradientMode;
begin
  if OpenTheme(totExplorerBar) <> 0 then
    Result := gmVertical
  else
    Result := inherited BgGradientMode;
end;

procedure TdxNavBarXPExplorerBarViewInfo.CreateColors;
begin
  CreateXPExplorerBarColors;
end;

procedure TdxNavBarXPExplorerBarViewInfo.RefreshColors;
begin
  RefreshXPExplorerBarColors;
end;

procedure TdxNavBarXPExplorerBarViewInfo.ReleaseColors;
begin
  ReleaseXPExplorerBarColors;
end;

function TdxNavBarXPExplorerBarViewInfo.GetGroupCaptionHeightAddon: Integer;
begin
  if OpenTheme(totExplorerBar) = 0 then
    Result := inherited GetGroupCaptionHeightAddon
  else
    Result := ScaleFactor.Apply(12);
end;

function TdxNavBarXPExplorerBarViewInfo.GetGroupCaptionSignSize: TSize;
begin
  if OpenTheme(totExplorerBar) = 0 then
    Result := inherited GetGroupCaptionSignSize
  else
    Result := ScaleFactor.Apply(cxSize(22, 22));
end;

{ TdxNavBarXPExplorerBarPainter }

class function TdxNavBarXPExplorerBarPainter.GetViewInfoClass: TdxNavBarViewInfoClass;
begin
  Result := TdxNavBarXPExplorerBarViewInfo;
end;

class function TdxNavBarXPExplorerBarPainter.GetGroupViewInfoClass: TdxNavBarGroupViewInfoClass;
begin
  Result := TdxNavBarXPExplorerBarGroupViewInfo;
end;

class function TdxNavBarXPExplorerBarPainter.GetLinkViewInfoClass: TdxNavBarLinkViewInfoClass;
begin
  Result := TdxNavBarXPExplorerBarLinkViewInfo;
end;

class function TdxNavBarXPExplorerBarPainter.BackgroundPainterClass: TdxNavBarCustomBackgroundPainterClass;
begin
  Result := TdxNavBarXPExplorerBarBackgroundPainter;
end;

class function TdxNavBarXPExplorerBarPainter.ButtonPainterClass: TdxNavBarCustomButtonPainterClass;
begin
  Result := TdxNavBarXPExplorerBarButtonPainter;
end;

class function TdxNavBarXPExplorerBarPainter.SignPainterClass: TdxNavBarCustomSignPainterClass;
begin
  Result := TdxNavBarXPExplorerBarSignPainter;
end;

{ TdxNavBarXPExplorerBarButtonPainter }

class function TdxNavBarXPExplorerBarButtonPainter.GetButtonBitmap(AState: TdxNavBarObjectStates): TBitmap;
begin
  if (sSpecial in AState) and (dxXPExplorerBarSpecialGroupHeader <> nil) then
    Result := dxXPExplorerBarSpecialGroupHeader
  else Result := dxXPExplorerBarNormalGroupHeader;
end;

class procedure TdxNavBarXPExplorerBarButtonPainter.InternalDrawButton(
  ACanvas: TCanvas; ARect: TRect; APicture: TPicture; AColor1,
  AColor2: TColor; AAlphaBlend1, AAlphaBlend2: Byte;
  AGradientMode: TdxBarStyleGradientMode; ABorderColor: TColor;
  AState: TdxNavBarObjectStates);
var
  ABitmap: TBitmap;
  ATheme: TdxTheme;
begin
  ATheme := OpenTheme(totExplorerBar);
  ABitmap := GetButtonBitmap(AState);
  if (ATheme <> 0) and (ABitmap <> nil) then
  begin
    ABitmap.PixelFormat := pf32bit;
    ABitmap.Transparent := True;
    ABitmap.TransparentColor := clFuchsia;
    ACanvas.StretchDraw(ARect, ABitmap);
  end
  else
    inherited;
end;

{ TdxNavBarXPExplorerBarSignPainter }

class procedure TdxNavBarXPExplorerBarSignPainter.CreateBitmap(var ADC: HDC;
  var ABitmap, AOldBitmap: HBITMAP; var AData: Pointer; AWidth, AHeight: Integer);
var
  TempDC: HDC;
  BitInfo: TBitmapInfo;
begin
  TempDC := GetDC(0);
  ADC := CreateCompatibleDC(TempDC);
  with BitInfo.bmiHeader do
  begin
    biSize := SizeOf(TBitmapInfoHeader);
    biPlanes := 1;
    biBitCount := 32;
    biCompression := BI_RGB;
    biWidth := AWidth;
    biHeight := AHeight;
    biSizeImage := 0;
    biXPelsPerMeter := 0;
    biYPelsPerMeter := 0;
    biClrUsed := 0;
    biClrImportant := 0;
  end;
  ABitmap := CreateDIBSection(ADC, BitInfo, DIB_RGB_COLORS, AData, 0, 0);
  GDIFlush;
  AOldBitmap := SelectObject(ADC, ABitmap);
  ReleaseDC(0, TempDC);
end;

class procedure TdxNavBarXPExplorerBarSignPainter.DeleteBitmap(var ADC: HDC;
  var ABitmap, AOldBitmap: HBITMAP; var AData: Pointer);
begin
  GDIFlush;
  SelectObject(ADC, AOldBitmap);
  DeleteObject(ABitmap);
  DeleteDC(ADC);
  AData := nil;
end;

class procedure TdxNavBarXPExplorerBarSignPainter.DrawMaskBitmap(DestX, DestY: Integer;
  DestHandle: THandle; Bitmap: TBitmap);
var
  I, J: Integer;
  DC1, DC2: HDC;
  Bitmap1, Bitmap2: HBITMAP;
  OldBitmap1, OldBitmap2: HBITMAP;
  Data1, Data2: Pointer;
  ASourceData, ADestData: Pointer;
  sR, sG, sB, sA: Byte;
  dR, dG, dB: Byte;
  R, G, B: Word;
begin
  CreateBitmap(DC1, Bitmap1, OldBitmap1, Data1, Bitmap.Width, Bitmap.Height);
  CreateBitmap(DC2, Bitmap2, OldBitmap2, Data2, Bitmap.Width, Bitmap.Height);
  try
    BitBlt(DC1, 0, 0, Bitmap.Width, Bitmap.Height, Bitmap.Canvas.Handle, 0, 0, SRCCOPY);
    BitBlt(DC2, 0, 0, Bitmap.Width, Bitmap.Height, DestHandle, DestX, DestY, SRCCOPY);

    ASourceData := Pointer(TdxNativeInt(Data1) + ((Bitmap.Height - 1) * Bitmap.Width * 4));
    ADestData := Pointer(TdxNativeInt(Data2) + ((Bitmap.Height - 1) * Bitmap.Width * 4));
    for I := 0 to Bitmap.Height - 1 do
    begin
      for J := 0 to Bitmap.Width - 1 do
      begin
        sA := PByte(TdxNativeUInt(ASourceData) + 3)^;

        sG := PByte(ASourceData)^;
        dG := PByte(ADestData)^;
        G := (sG * sA) + (dG * (255 - sA));
        PByte(ADestData)^ := G div 256 + 1;

        sB := PByte(TdxNativeInt(ASourceData) + 1)^;
        dB := PByte(TdxNativeInt(ADestData) + 1)^;
        B := (sB * sA) + (dB * (255 - sA));
        PByte(TdxNativeInt(ADestData) + 1)^ := B div 256  + 1;

        sR := PByte(TdxNativeInt(ASourceData) + 2)^;
        dR := PByte(TdxNativeInt(ADestData) + 2)^;
        R := (sR * sA) + (dR * (255 - sA));
        PByte(TdxNativeInt(ADestData) + 2)^ := R div 256  + 1;

        PByte(TdxNativeInt(ADestData) + 3)^ := PByte(TdxNativeInt(ASourceData) + 3)^;

        ASourceData := Pointer(TdxNativeInt(ASourceData) + 4);
        ADestData := Pointer(TdxNativeInt(ADestData) + 4);
      end;
      ASourceData := Pointer(TdxNativeInt(ASourceData) - Bitmap.Width * 4 * 2);
      ADestData := Pointer(TdxNativeInt(ADestData) - Bitmap.Width * 4 * 2);
    end;

    BitBlt(DestHandle, DestX, DestY, Bitmap.Width, Bitmap.Height, DC2, 0, 0, SRCCOPY);
  finally
    DeleteBitmap(DC1, Bitmap1, OldBitmap1, Data1);
    DeleteBitmap(DC2, Bitmap2, OldBitmap2, Data2);
  end;
end;

class function TdxNavBarXPExplorerBarSignPainter.GetSignBitmap(AState: TdxNavBarObjectStates): TBitmap;
begin
  if (sExpanded in AState) then
  begin
    if sHotTracked in AState then
    begin
      if (sSpecial in AState) and (dxXPExplorerBarSpecialGroupCollapseHot <> nil) then
        Result := dxXPExplorerBarSpecialGroupCollapseHot
      else
        Result := dxXPExplorerBarNormalGroupCollapseHot
    end
    else
      if (sSpecial in AState) and (dxXPExplorerBarSpecialGroupCollapse <> nil) then
        Result := dxXPExplorerBarSpecialGroupCollapse
      else
        Result := dxXPExplorerBarNormalGroupCollapse;
  end
  else
    if sHotTracked in AState then
    begin
      if (sSpecial in AState) and (dxXPExplorerBarSpecialGroupExpandHot <> nil) then
        Result := dxXPExplorerBarSpecialGroupExpandHot
      else
        Result := dxXPExplorerBarNormalGroupExpandHot
    end
    else
      if (sSpecial in AState) and (dxXPExplorerBarSpecialGroupExpand <> nil) then
        Result := dxXPExplorerBarSpecialGroupExpand
      else
        Result := dxXPExplorerBarNormalGroupExpand;
end;

class procedure TdxNavBarXPExplorerBarSignPainter.InternalDrawSign(ACanvas: TCanvas;
  ARect: TRect; AScaleFactor: TdxScaleFactor; AForeColor, ABackColor1, ABackColor2: TColor; AState: TdxNavBarObjectStates);
var
  ABitmap: TBitmap;
  ATheme: TdxTheme;
begin
  ATheme := OpenTheme(totExplorerBar);
  ABitmap := GetSignBitmap(AState);
  if (ATheme <> 0) and (ABitmap <> nil) then
  begin
    if TdxNavBarXPExplorerBarButtonPainter.GetButtonBitmap(AState) = nil then
    begin
      ABitmap.PixelFormat := pf32bit;
      ABitmap.Transparent := True;
      ABitmap.TransparentColor := clFuchsia;
      ACanvas.StretchDraw(ARect, ABitmap);
    end
    else
      if AScaleFactor.Assigned then
      begin
        ABitmap := CloneBitmap(ABitmap, AScaleFactor.Apply(cxSize(ABitmap.Width, ABitmap.Height)));
        try
          DrawMaskBitmap(ARect.Left, ARect.Top, ACanvas.Handle, ABitmap);
        finally
          ABitmap.Free;
        end;
      end
      else
        DrawMaskBitmap(ARect.Left, ARect.Top, ACanvas.Handle, ABitmap);
  end
  else
    inherited;
end;

{ TdxNavBarXPPainter }

class function TdxNavBarXPPainter.DrawObject(ACanvas: TCanvas; ARect: TRect;
    AObject: TdxThemedObjectType; iPartId, iStateId: Integer): Boolean;
var
  ATheme: TdxTheme;
begin
  ATheme := OpenTheme(AObject);
  Result := ATheme <> 0;
  if Result then
    DrawThemeBackground(ATheme, ACanvas.Handle, iPartId, iStateId, @ARect);
end;

class function TdxNavBarXPPainter.DrawText(ACanvas: TCanvas; ARect: TRect;
    ACaption: string; ADrawTextFlag: Integer;
    AObject: TdxThemedObjectType; iPartId, iStateId: Integer): Boolean;
var
  ATheme: TdxTheme;
begin
  ATheme := OpenTheme(AObject);
  Result := ATheme <> 0;
  if Result then
    DrawThemeText(ATheme, ACanvas.Handle, iPartId, iStateId, ACaption, -1, ADrawTextFlag, 0, ARect);
end;

{ TdxNavBarXPBackgroundPainter }

class procedure TdxNavBarXPBackgroundPainter.InternalDrawBackground(ACanvas: TCanvas;
  ARect: TRect; APicture: TPicture; AEraseBackground: Boolean; ABackgroundColor: TColor;
  AColor1, AColor2: TColor; AAlphaBlend1, AAlphaBlend2: Byte; AGradientMode: TdxBarStyleGradientMode);
begin
  if not TdxNavBarXPPainter.DrawObject(ACanvas, ARect, totTab, TABP_BODY, 0) then
    inherited;
end;

{ TdxNavBarXPGroupBackgroundPainter }

class procedure TdxNavBarXPGroupBackgroundPainter.InternalDrawBackground(ACanvas: TCanvas;
  ARect: TRect; APicture: TPicture; AColor1, AColor2: TColor;
  AAlphaBlend1, AAlphaBlend2: Byte; AGradientMode: TdxBarStyleGradientMode);
begin
  TdxNavBarXPBackgroundPainter.DrawBackground(ACanvas, ARect, APicture, False, clNone,
    AColor1, AColor2, AAlphaBlend1, AAlphaBlend2, AGradientMode);
end;

{ TdxNavBarXPSelectionPainter }

class procedure TdxNavBarXPSelectionPainter.InternalDrawSelection(ACanvas: TCanvas;
  ARect: TRect; ABackColor: TColor; AState: TdxNavBarObjectStates);
var
  AStateID: Integer;
begin
  if [sPressed, sSelected] * AState <> [] then
    AStateID := TS_PRESSED
  else
    if (sHotTracked in AState) then
      AStateID := TS_HOT
    else
      AStateID := TS_NORMAL;
  if not TdxNavBarXPPainter.DrawObject(ACanvas, ARect, totToolBar, TP_BUTTON, AStateID) then
    inherited;
end;

{ TdxNavBarXPButtonPainter }

class procedure TdxNavBarXPButtonPainter.InternalDrawButton(ACanvas: TCanvas;
  ARect: TRect; APicture: TPicture; AColor1, AColor2: TColor; AAlphaBlend1,
  AAlphaBlend2: Byte; AGradientMode: TdxBarStyleGradientMode; ABorderColor: TColor;
  AState: TdxNavBarObjectStates);
var
  AStateID: Integer;
begin
  if sPressed in AState then
    AStateID := PBS_PRESSED
  else
    if sHotTracked in AState then
      AStateID := PBS_HOT
    else
      if sActive in AState then
        AStateID := PBS_DEFAULTED
      else
        AStateID := PBS_NORMAL;
  if not TdxNavBarXPPainter.DrawObject(ACanvas, ARect, totButton, BP_PUSHBUTTON, AStateID) then
    inherited;
end;

{ TdxNavBarXPScrollButtonsPainter }

class procedure TdxNavBarXPScrollButtonsPainter.InternalDrawBottomButton(
  ACanvas: TCanvas; ARect: TRect;
  AButtonPainterClass: TdxNavBarCustomButtonPainterClass; AColor1, AColor2: TColor;
  AAlphaBlend1, AAlphaBlend2: Byte; AGradientMode: TdxBarStyleGradientMode;
  ABorderColor: TColor; AState: TdxNavBarObjectStates);
var
  ATheme: TdxTheme;
  AStateID: Integer;
begin
  ATheme := OpenTheme(totScrollBar);
  if ATheme <> 0 then
  begin
    if sPressed in AState then
      AStateID := ABS_DOWNPRESSED
    else if sHotTracked in AState then
      AStateID := ABS_DOWNHOT
    else AStateID := ABS_DOWNNORMAL;
    TdxNavBarXPPainter.DrawObject(ACanvas, ARect, totScrollBar, SBP_ARROWBTN, AStateID);
    ExcludeClipRect(ACanvas.Handle, ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
  end
  else inherited;
end;

class procedure TdxNavBarXPScrollButtonsPainter.InternalDrawTopButton(
  ACanvas: TCanvas; ARect: TRect;
  AButtonPainterClass: TdxNavBarCustomButtonPainterClass; AColor1, AColor2: TColor;
  AAlphaBlend1, AAlphaBlend2: Byte; AGradientMode: TdxBarStyleGradientMode;
  ABorderColor: TColor; AState: TdxNavBarObjectStates);
var
  ATheme: TdxTheme;
  AStateID: Integer;
begin
  ATheme := OpenTheme(totScrollBar);
  if ATheme <> 0 then
  begin
    if sPressed in AState then
      AStateID := ABS_UPPRESSED
    else if sHotTracked in AState then
      AStateID := ABS_UPHOT
    else AStateID := ABS_UPNORMAL;
    TdxNavBarXPPainter.DrawObject(ACanvas, ARect, totScrollBar, SBP_ARROWBTN, AStateID);
    ExcludeClipRect(ACanvas.Handle, ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
  end
  else inherited;
end;

{ TdxNavBarXPExplorerBarBackgroundPainter }

class function TdxNavBarXPExplorerBarBackgroundPainter.GetBackgroundBitmap: TBitmap;
begin
  Result := dxXPExplorerBarBackground;
end;

class procedure TdxNavBarXPExplorerBarBackgroundPainter.InternalDrawBackground(ACanvas: TCanvas;
  ARect: TRect; APicture: TPicture; AEraseBackground: Boolean; ABackgroundColor: TColor;
  AColor1, AColor2: TColor; AAlphaBlend1, AAlphaBlend2: Byte;
  AGradientMode: TdxBarStyleGradientMode);
var
  ABitmap: TBitmap;
  ATheme: TdxTheme;
begin
  ATheme := OpenTheme(totExplorerBar);
  ABitmap := GetBackgroundBitmap;
  if (ATheme <> 0) and (ABitmap <> nil) then
  begin
    ABitmap.PixelFormat := pf32bit;
    ABitmap.Transparent := True;
    ABitmap.TransparentColor := clFuchsia;
    ACanvas.StretchDraw(ARect, ABitmap);
  end
  else inherited;
end;


initialization
  RegisterView(dxNavBarXP1View, 'XP1View', TdxNavBarXP1Painter);
  RegisterView(dxNavBarXP2View, 'XP2View', TdxNavBarXP2Painter);
  RegisterView(dxNavBarXPExplorerBarView, 'XPExplorerBarView', TdxNavBarXPExplorerBarPainter);

finalization
  UnRegisterView(dxNavBarXP1View);
  UnRegisterView(dxNavBarXP2View);
  UnRegisterView(dxNavBarXPExplorerBarView);

end.
