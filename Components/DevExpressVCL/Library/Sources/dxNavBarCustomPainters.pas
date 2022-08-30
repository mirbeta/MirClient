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

unit dxNavBarCustomPainters;

{$I cxVer.inc}

interface

uses
  Types, Windows, Graphics, Classes, ImgList, Math, dxNavBar, dxNavBarStyles, dxNavBarCollns, cxGeometry, dxCoreGraphics;

type
  { TdxNavBarCustomImagePainter }

  TdxNavBarCustomImagePainterClass = class of TdxNavBarCustomImagePainter;
  TdxNavBarCustomImagePainter = class
  public
    class function DrawImage(ACanvas: TCanvas; AImageList: TCustomImageList; AImageIndex: Integer;
      const ARect: TRect; AEnabled: Boolean = True; AColorPalette: IdxColorPalette = nil): Boolean;
    class function IsValidImage(AImageList: TCustomImageList;AImageIndex: Integer): Boolean;
  end;

  { TdxNavBarCustomSelectionPainter }

  TdxNavBarCustomSelectionPainterClass = class of TdxNavBarCustomSelectionPainter;
  TdxNavBarCustomSelectionPainter = class
  protected
    class procedure GetColors(AState: TdxNavBarObjectStates; ABackColor: TColor; out AFillColor,
      ATopLeftOuterColor, ABottomRightOuterColor, ATopLeftInnerColor, ABottomRightInnerColor: TColor); virtual;
    class procedure InternalDrawSelection(ACanvas: TCanvas; ARect: TRect;
      ABackColor: TColor; AState: TdxNavBarObjectStates); virtual;
    class function IsPressed(AState: TdxNavBarObjectStates): Boolean; virtual;
  public
    class procedure DrawSelection(ACanvas: TCanvas; ARect: TRect;
      ABackColor: TColor; AState: TdxNavBarObjectStates);
  end;

  TdxNavBarUltraFlatSelectionPainter = class(TdxNavBarCustomSelectionPainter)
  protected
    class procedure GetColors(AState: TdxNavBarObjectStates; ABackColor: TColor; out AFillColor,
      ATopLeftOuterColor, ABottomRightOuterColor, ATopLeftInnerColor, ABottomRightInnerColor: TColor); override;
    class function IsPressed(AState: TdxNavBarObjectStates): Boolean; override;
  end;

  TdxNavBarCustomBackgroundPainter = class
  private
    class procedure InternalDrawGradientBackground(ACanvas: TCanvas; ARect: TRect; APicture: TPicture;
      AEraseBackground: Boolean; ABackgroundColor: TColor;
      AColor1, AColor2: TColor; AAlphaBlend1, AAlphaBlend2: Byte;
      AGradientMode: TdxBarStyleGradientMode);
  protected
    class procedure DrawPicture(ACanvas: TCanvas; ARect: TRect; APicture: TPicture);
    class procedure FillGradientRect(DC: HDC; const ARect: TRect;
      AColor1, AColor2: TColor; AAlphaBlend1, AAlphaBlend2: Byte; AGradientMode: TdxBarStyleGradientMode);
    class procedure InternalDrawBackground(ACanvas: TCanvas; ARect: TRect; APicture: TPicture;
      AEraseBackground: Boolean; ABackgroundColor: TColor;
      AColor1, AColor2: TColor; AAlphaBlend1, AAlphaBlend2: Byte;
      AGradientMode: TdxBarStyleGradientMode); virtual;
  public
    class procedure DrawBackground(ACanvas: TCanvas; ARect: TRect; APicture: TPicture;
      AEraseBackground: Boolean; ABackgroundColor: TColor;
      AColor1, AColor2: TColor; AAlphaBlend1, AAlphaBlend2: Byte;
      AGradientMode: TdxBarStyleGradientMode);
  end;
  TdxNavBarCustomBackgroundPainterClass = class of TdxNavBarCustomBackgroundPainter;

  TdxNavBarCustomGroupBackgroundPainter = class
  protected
    class procedure InternalDrawBackground(ACanvas: TCanvas; ARect: TRect; APicture: TPicture;
      AColor1, AColor2: TColor; AAlphaBlend1, AAlphaBlend2: Byte;
      AGradientMode: TdxBarStyleGradientMode); virtual;
  public
    class procedure DrawBackground(ACanvas: TCanvas; ARect: TRect; APicture: TPicture;
      AColor1, AColor2: TColor; AAlphaBlend1, AAlphaBlend2: Byte;
      AGradientMode: TdxBarStyleGradientMode);
  end;
  TdxNavBarCustomGroupBackgroundPainterClass = class of TdxNavBarCustomGroupBackgroundPainter;

  TdxNavBarCustomGroupBorderPainter = class
  protected
    class procedure InternalDrawBorder(ACanvas: TCanvas; ARect: TRect; AColor: TColor; AWithCaption: Boolean); virtual;
  public
    class procedure DrawBorder(ACanvas: TCanvas; ARect: TRect; AColor: TColor; AWithCaption: Boolean);
  end;
  TdxNavBarCustomGroupBorderPainterClass = class of TdxNavBarCustomGroupBorderPainter;

  { TdxNavBarCustomSignPainter }

  TdxNavBarCustomSignPainterClass = class of TdxNavBarCustomSignPainter;
  TdxNavBarCustomSignPainter = class
  protected
    class procedure InternalDrawSign(ACanvas: TCanvas; ARect: TRect; AScaleFactor: TdxScaleFactor;
      AForeColor, ABackColor1, ABackColor2: TColor; AState: TdxNavBarObjectStates); virtual;
  public
    class procedure DrawSign(ACanvas: TCanvas; ARect: TRect; AScaleFactor: TdxScaleFactor;
      AForeColor, ABackColor1, ABackColor2: TColor; AState: TdxNavBarObjectStates);
  end;

  TdxNavBarCustomHintPainter = class
  protected
    class procedure InternalDrawHint(ACanvas: TCanvas; ARect: TRect; AHint: string;
      APicture: TPicture; AColor1, AColor2: TColor; AAlphaBlend1, AAlphaBlend2: Byte;
      AGradientMode: TdxBarStyleGradientMode; AFont: TFont); virtual;
  public
    class procedure DrawHint(ACanvas: TCanvas; ARect: TRect; AHint: string;
      APicture: TPicture; AColor1, AColor2: TColor; AAlphaBlend1, AAlphaBlend2: Byte;
      AGradientMode: TdxBarStyleGradientMode; AFont: TFont);
  end;
  TdxNavBarCustomHintPainterClass = class of TdxNavBarCustomHintPainter;

  TdxNavBarCustomButtonPainter = class
  protected
    class procedure InternalDrawButton(ACanvas: TCanvas; ARect: TRect; APicture: TPicture;
      AColor1, AColor2: TColor; AAlphaBlend1, AAlphaBlend2: Byte;
      AGradientMode: TdxBarStyleGradientMode; ABorderColor: TColor;
      AState: TdxNavBarObjectStates); virtual;
  public
    class procedure DrawButton(ACanvas: TCanvas; ARect: TRect; APicture: TPicture;
      AColor1, AColor2: TColor; AAlphaBlend1, AAlphaBlend2: Byte;
      AGradientMode: TdxBarStyleGradientMode; ABorderColor: TColor;
      AState: TdxNavBarObjectStates);
  end;
  TdxNavBarCustomButtonPainterClass = class of TdxNavBarCustomButtonPainter;

  TdxNavBarCustomScrollButtonsPainter = class
  protected
    class procedure InternalDrawBottomButton(ACanvas: TCanvas; ARect: TRect;
      AButtonPainterClass: TdxNavBarCustomButtonPainterClass; AColor1, AColor2: TColor;
      AAlphaBlend1, AAlphaBlend2: Byte; AGradientMode: TdxBarStyleGradientMode;
      ABorderColor: TColor; AState: TdxNavBarObjectStates); virtual;
    class procedure InternalDrawTopButton(ACanvas: TCanvas; ARect: TRect;
      AButtonPainterClass: TdxNavBarCustomButtonPainterClass; AColor1, AColor2: TColor;
      AAlphaBlend1, AAlphaBlend2: Byte; AGradientMode: TdxBarStyleGradientMode;
      ABorderColor: TColor; AState: TdxNavBarObjectStates); virtual;
  public
    class procedure DrawBottomButton(ACanvas: TCanvas; ARect: TRect;
      AButtonPainterClass: TdxNavBarCustomButtonPainterClass; AColor1, AColor2: TColor;
      AAlphaBlend1, AAlphaBlend2: Byte; AGradientMode: TdxBarStyleGradientMode;
      ABorderColor: TColor; AState: TdxNavBarObjectStates);
    class procedure DrawTopButton(ACanvas: TCanvas; ARect: TRect;
      AButtonPainterClass: TdxNavBarCustomButtonPainterClass; AColor1, AColor2: TColor;
      AAlphaBlend1, AAlphaBlend2: Byte; AGradientMode: TdxBarStyleGradientMode;
      ABorderColor: TColor; AState: TdxNavBarObjectStates);
  end;
  TdxNavBarCustomScrollButtonsPainterClass = class of TdxNavBarCustomScrollButtonsPainter;

  TdxNavBarCustomDropTargetLinkPainter = class
  protected
    class procedure InternalDrawTargetLink(ACanvas: TCanvas;
      pt1, pt2, pt3, pt4, pt5, pt6: TPoint; AColor: TColor); virtual;
  public
    class procedure DrawTargetLink(ACanvas: TCanvas;
      pt1, pt2, pt3, pt4, pt5, pt6: TPoint; AColor: TColor);
  end;
  TdxNavBarCustomDropTargetLinkPainterClass = class of TdxNavBarCustomDropTargetLinkPainter;

  TdxNavBarCustomDropTargetGroupPainter = class
  protected
    class procedure InternalDrawTargetGroup(ACanvas: TCanvas; ARect: TRect;
      AColor1, AColor2: TColor; AAlphaBlend1, AAlphaBlend2: Byte;
      AGradientMode: TdxBarStyleGradientMode); virtual;
  public
    class procedure DrawTargetGroup(ACanvas: TCanvas; ARect: TRect;
      AColor1, AColor2: TColor; AAlphaBlend1, AAlphaBlend2: Byte;
      AGradientMode: TdxBarStyleGradientMode);
  end;
  TdxNavBarCustomDropTargetGroupPainterClass = class of TdxNavBarCustomDropTargetGroupPainter;

  TdxNavBarViewInfoAccess = class(TdxNavBarViewInfo);
  TdxNavBarLinkViewInfoAccess = class(TdxNavBarLinkViewInfo);

  TdxNavBarElementPainter = class(TdxNavBarPainter)
  protected
    // Drawing
    procedure DoDrawDropTargetLinkSelection(AItemGroup: TdxNavBarGroupViewInfo; AItem1, AItem2: TdxNavBarCustomItemViewInfo); virtual;
    procedure DoDrawHint(ACanvas: TCanvas; const ARect: TRect); override;
    // Color Palettes
    function GetItemColorPalette(ALinkViewInfo: TdxNavBarLinkViewInfo): IdxColorPalette; virtual;
    // Painters
    class function BackgroundPainterClass: TdxNavBarCustomBackgroundPainterClass; virtual;
    class function ButtonPainterClass: TdxNavBarCustomButtonPainterClass; virtual;
    class function DropTargetGroupPainterClass: TdxNavBarCustomDropTargetGroupPainterClass; virtual;
    class function DropTargetLinkPainterClass: TdxNavBarCustomDropTargetLinkPainterClass; virtual;
    class function GroupBackgroundPainterClass: TdxNavBarCustomGroupBackgroundPainterClass; virtual;
    class function GroupBorderPainterClass: TdxNavBarCustomGroupBorderPainterClass; virtual;
    class function ImagePainterClass: TdxNavBarCustomImagePainterClass; virtual;
    class function ScrollButtonsPainterClass: TdxNavBarCustomScrollButtonsPainterClass; virtual;
    class function SelectionPainterClass: TdxNavBarCustomSelectionPainterClass; virtual;
    class function SignPainterClass: TdxNavBarCustomSignPainterClass; virtual;
  public
    // Drawing
    procedure DrawBackground; override;
    procedure DrawDropTargetGroupSelection; override;
    procedure DrawDropTargetLinkSelection; override;
    procedure DrawChildGroupSelection(AChildGroupViewInfo: TdxNavBarChildGroupViewInfo); override;
    procedure DrawGroupBackground(AGroupViewInfo: TdxNavBarGroupViewInfo); override;
    procedure DrawGroupBorder(AGroupViewInfo: TdxNavBarGroupViewInfo); override;
    procedure DrawGroupCaptionButton(AGroupViewInfo: TdxNavBarGroupViewInfo); override;
    procedure DrawGroupCaptionImage(AGroupViewInfo: TdxNavBarGroupViewInfo); override;
    procedure DrawGroupCaptionSign(AGroupViewInfo: TdxNavBarGroupViewInfo); override;
    procedure DrawGroupControl(ACanvas: TCanvas; ARect: TRect; AGroupViewInfo: TdxNavBarGroupViewInfo); override;
    procedure DrawItemImage(ALinkViewInfo: TdxNavBarLinkViewInfo); override;
    procedure DrawItemSelection(ALinkViewInfo: TdxNavBarLinkViewInfo); override;
    procedure DrawBottomScrollButton; override;
    procedure DrawTopScrollButton; override;
  end;

implementation

uses
  CommCtrl, cxGraphics, dxCore, dxOffice11, dxNavBarGraphics;

type
  TdxCustomNavBarAccess =class(TdxCustomNavBar);

{ TdxNavBarCustomImagePainter }

class function TdxNavBarCustomImagePainter.DrawImage(ACanvas: TCanvas; AImageList: TCustomImageList;
  AImageIndex: Integer; const ARect: TRect; AEnabled: Boolean; AColorPalette: IdxColorPalette): Boolean;
begin
  Result := IsValidImage(AImageList, AImageIndex);
  if Result then
    cxDrawImage(ACanvas.Handle, ARect, ARect, nil, AImageList,
      AImageIndex, EnabledImageDrawModeMap[AEnabled or DrawIconsAsEnabled],
      False, 0, clNone, True, AColorPalette);
end;

class function TdxNavBarCustomImagePainter.IsValidImage(AImageList: TCustomImageList; AImageIndex: Integer): Boolean;
begin
  Result := IsImageAssigned(AImageList, AImageIndex);
end;

{ TdxNavBarCustomSelectionPainter }

class procedure TdxNavBarCustomSelectionPainter.DrawSelection(ACanvas: TCanvas;
  ARect: TRect; ABackColor: TColor; AState: TdxNavBarObjectStates);
begin
  InternalDrawSelection(ACanvas, ARect, ABackColor, AState);
end;

class procedure TdxNavBarCustomSelectionPainter.GetColors(AState: TdxNavBarObjectStates; ABackColor: TColor;
  out AFillColor, ATopLeftOuterColor, ABottomRightOuterColor, ATopLeftInnerColor, ABottomRightInnerColor: TColor);
begin
  AFillColor := clNone;
  ATopLeftOuterColor := clNone;
  ABottomRightOuterColor := clNone;
  ATopLeftInnerColor := clNone;
  ABottomRightInnerColor := clNone;
end;

class procedure TdxNavBarCustomSelectionPainter.InternalDrawSelection(
  ACanvas: TCanvas; ARect: TRect; ABackColor: TColor; AState: TdxNavBarObjectStates);
var
  AFillColor, ATopLeftOuterColor, ABottomRightOuterColor,
  ATopLeftInnerColor, ABottomRightInnerColor: TColor;
begin
  if [sSelected, sPressed, sHotTracked] * AState <> [] then
  begin
    GetColors(AState, ABackColor, AFillColor, ATopLeftOuterColor, ABottomRightOuterColor,
      ATopLeftInnerColor, ABottomRightInnerColor);
    dxNavBarDrawSelectedFrame(ACanvas, ARect, ATopLeftOuterColor, ABottomRightOuterColor,
      ATopLeftInnerColor, ABottomRightInnerColor);
    if AFillColor <> clNone then
      FillRectByColor(ACanvas.Handle, cxRectInflate(ARect, -1, -1), AFillColor);
  end;
end;

class function TdxNavBarCustomSelectionPainter.IsPressed(AState: TdxNavBarObjectStates): Boolean;
begin
  Result := [sPressed, sSelected] * AState <> [];
end;

{ TdxNavBarUltraFlatSelectionPainter }

class procedure TdxNavBarUltraFlatSelectionPainter.GetColors(AState: TdxNavBarObjectStates; ABackColor: TColor;
  out AFillColor, ATopLeftOuterColor, ABottomRightOuterColor, ATopLeftInnerColor, ABottomRightInnerColor: TColor);
begin
  inherited;
  ATopLeftOuterColor := ColorToRGB(clHighlight);
  ABottomRightOuterColor := ColorToRGB(clHighlight);
  if IsPressed(AState) then
    AFillColor := dxGetNearestColor(GetLightColor(14, 44, 40))
  else
    AFillColor := dxGetNearestColor(GetLightColor(-2, 30, 72));
end;

class function TdxNavBarUltraFlatSelectionPainter.IsPressed(AState: TdxNavBarObjectStates): Boolean;
begin
  Result := (sPressed in AState) and not (sSelected in AState);
end;

{ TdxNavBarCustomBackgroundPainter }

class procedure TdxNavBarCustomBackgroundPainter.DrawBackground(ACanvas: TCanvas; ARect: TRect;
  APicture: TPicture; AEraseBackground: Boolean; ABackgroundColor: TColor;
  AColor1, AColor2: TColor; AAlphaBlend1, AAlphaBlend2: Byte; AGradientMode: TdxBarStyleGradientMode);
begin
  InternalDrawBackground(ACanvas, ARect, APicture, AEraseBackground, ABackgroundColor,
    AColor1, AColor2, AAlphaBlend1, AAlphaBlend2, AGradientMode);
end;

class procedure TdxNavBarCustomBackgroundPainter.DrawPicture(ACanvas: TCanvas; ARect: TRect;
  APicture: TPicture);
var
  dLeft, dTop, dWidth, dHeight: Integer;
  ABmp: TBitmap;
  ACreatedFlag: Boolean;
  AOnChange: TNotifyEvent;
begin
  ACreatedFlag := not(APicture.Graphic is TBitmap);
  if ACreatedFlag then
  begin
    ABmp := cxCreateBitmap(APicture.Graphic.Height, APicture.Graphic.Height);
    AOnChange := APicture.Graphic.OnChange;
    APicture.Graphic.OnChange := nil;
    ABmp.Canvas.Draw(0, 0, APicture.Graphic);
    APicture.Graphic.OnChange := AOnChange;
  end
  else
    ABmp := APicture.Bitmap;

  if ABmp <> nil then
  begin
    dTop := ARect.Top;
    while (dTop < ARect.Bottom) do begin
       dHeight := ABmp.Height;
       if (dTop + dHeight > ARect.Bottom) then
         dHeight := ARect.Bottom - dTop;
       dLeft := ARect.Left;
       while (dLeft < ARect.Right) do begin
         dWidth := ABmp.Width;
         if (dLeft + dWidth > ARect.Right) then
           dWidth := ARect.Right - dLeft;
         BitBlt(ACanvas.Handle, dLeft, dTop, dWidth, dHeight, ABmp.Canvas.Handle, 0, 0, SRCCOPY);
         Inc(dLeft, dWidth);
       end;
       Inc(dTop, dHeight);
    end;
    if ACreatedFlag then ABmp.Free;
  end;
end;

class procedure TdxNavBarCustomBackgroundPainter.FillGradientRect(DC: HDC; const ARect: TRect;
  AColor1, AColor2: TColor; AAlphaBlend1, AAlphaBlend2: Byte; AGradientMode: TdxBarStyleGradientMode);

  function CalcColorComponent(AFromColorComponent, AToColorComponent: Byte;
    AGradientSize: TSize; AGradientPos: TPoint): Integer;
  var
    AFromToColorSize: Integer;
  begin
    AFromToColorSize := AToColorComponent - AFromColorComponent;
    case AGradientMode of
      gmHorizontal:
        Result := AFromColorComponent +
          MulDiv(AGradientPos.X, AFromToColorSize, (AGradientSize.cx - 1));
      gmVertical:
        Result := AFromColorComponent +
          MulDiv(AGradientPos.Y, AFromToColorSize, (AGradientSize.cy - 1));
      gmForwardDiagonal:
        Result := AFromColorComponent +
          (MulDiv(AGradientPos.X, AFromToColorSize, (AGradientSize.cx - 1)) +
          MulDiv(AGradientPos.Y, AFromToColorSize, (AGradientSize.cy - 1))) div 2;
      gmBackwardDiagonal:
        Result := AFromColorComponent +
          (MulDiv(AGradientSize.cx - 1 - AGradientPos.X, AFromToColorSize, (AGradientSize.cx - 1)) +
          MulDiv(AGradientPos.Y, AFromToColorSize, (AGradientSize.cy - 1))) div 2;
    else
      Result := 0;
    end
  end;

  function CalcRGBColor(AFromColor, AToColor: TColor;
    AGradientSize: TSize; AGradientPos: TPoint): TColor;
  var
    R, G, B: Byte;
  begin
    R := CalcColorComponent(GetRValue(AFromColor), GetRValue(AToColor),
      AGradientSize, AGradientPos);
    G := CalcColorComponent(GetGValue(AFromColor), GetGValue(AToColor),
      AGradientSize, AGradientPos);
    B := CalcColorComponent(GetBValue(AFromColor), GetBValue(AToColor),
      AGradientSize, AGradientPos);
    Result := RGB(R, G, B);
  end;

  function GetGradientSize(ARect: TRect): TSize;
  const
    MaxGradientHeight = 256;
    MaxGradientWidth = 256;
  var
    ARectWidth, ARectHeight: Integer;
  begin
    ARectWidth := ARect.Right - ARect.Left;
    ARectHeight := ARect.Bottom - ARect.Top;
    if ARectWidth < MaxGradientWidth then
      Result.cx := ARectWidth
    else
      Result.cx := MaxGradientWidth;
    if ARectHeight < MaxGradientHeight then
      Result.cy := ARectHeight
    else
      Result.cy := MaxGradientHeight;
  end;

var
  AOneColorRect: TRect;
  I, J, ARectWidth, ARectHeight: Integer;
  AGradientSize: TSize;

begin
  AColor1 := ColorToRGB(AColor1);
  AColor2 := ColorToRGB(AColor2);
  if AColor1 = AColor2 then
  begin
    FillRectByColor(DC, ARect, AColor1);
    Exit;
  end;

  AOneColorRect := ARect;
  ARectWidth := ARect.Right - ARect.Left;
  ARectHeight := ARect.Bottom - ARect.Top;
  AGradientSize := GetGradientSize(ARect);

  for I := 0 to AGradientSize.cy - 1 do
  begin
    AOneColorRect.Bottom := ARect.Top + MulDiv(I + 1, ARectHeight, AGradientSize.cy);
    for J := 0 to AGradientSize.cx - 1 do
    begin
      AOneColorRect.Right := ARect.Left + MulDiv(J + 1, ARectWidth, AGradientSize.cx);
      if not IsRectEmpty(AOneColorRect) then
        FillRectByColor(DC, AOneColorRect, CalcRGBColor(AColor1, AColor2, AGradientSize, Point(J, I)));
      AOneColorRect.Left := AOneColorRect.Right;
      if AOneColorRect.Left >= ARect.Right then
        Break;
    end;
    AOneColorRect.Top := AOneColorRect.Bottom;
    if AOneColorRect.Top >= ARect.Bottom then
      Break;
    AOneColorRect.Left := ARect.Left;
  end;
end;

class procedure TdxNavBarCustomBackgroundPainter.InternalDrawBackground(ACanvas: TCanvas; ARect: TRect;
  APicture: TPicture; AEraseBackground: Boolean; ABackgroundColor: TColor;
  AColor1, AColor2: TColor; AAlphaBlend1, AAlphaBlend2: Byte; AGradientMode: TdxBarStyleGradientMode);
begin
  if (APicture <> nil) and (APicture.Graphic <> nil) and not APicture.Graphic.Empty then
    DrawPicture(ACanvas, ARect, APicture)
  else
    if AEraseBackground then
      FillRectByColor(ACanvas.Handle, ARect, ABackgroundColor);
  if (AColor1 = AColor2) and (AAlphaBlend1 = 255) and (AAlphaBlend2 = 255) then
    FillRectByColor(ACanvas.Handle, ARect, AColor1)
  else
    InternalDrawGradientBackground(ACanvas, ARect, APicture, AEraseBackground,
      ABackgroundColor, AColor1, AColor2, AAlphaBlend1, AAlphaBlend2, AGradientMode);
end;

class procedure TdxNavBarCustomBackgroundPainter.InternalDrawGradientBackground(ACanvas: TCanvas; ARect: TRect; APicture: TPicture;
  AEraseBackground: Boolean; ABackgroundColor: TColor;
  AColor1, AColor2: TColor; AAlphaBlend1, AAlphaBlend2: Byte;
  AGradientMode: TdxBarStyleGradientMode);
begin
  if IsGdiPlusAvailable then
    GdipFillGradientRect(ACanvas.Handle, ARect, AColor1, AColor2, AAlphaBlend1, AAlphaBlend2, AGradientMode)
  else
    if (AAlphaBlend1 <> 0) or (AAlphaBlend2 <> 0) then
      FillGradientRect(ACanvas.Handle, ARect, AColor1, AColor2, AAlphaBlend1, AAlphaBlend2, AGradientMode);
end;

{ TdxNavBarCustomGroupBackgroundPainter }

class procedure TdxNavBarCustomGroupBackgroundPainter.DrawBackground(ACanvas: TCanvas;
  ARect: TRect; APicture: TPicture; AColor1, AColor2: TColor; AAlphaBlend1, AAlphaBlend2: Byte;
  AGradientMode: TdxBarStyleGradientMode);
begin
  InternalDrawBackground(ACanvas, ARect, APicture, AColor1, AColor2,
    AAlphaBlend1, AAlphaBlend2, AGradientMode);
end;

class procedure TdxNavBarCustomGroupBackgroundPainter.InternalDrawBackground(ACanvas: TCanvas; ARect:
  TRect; APicture: TPicture; AColor1, AColor2: TColor; AAlphaBlend1, AAlphaBlend2: Byte;
  AGradientMode: TdxBarStyleGradientMode);
begin
  TdxNavBarCustomBackgroundPainter.DrawBackground(ACanvas, ARect, APicture, False, clNone,
    AColor1, AColor2, AAlphaBlend1, AAlphaBlend2, AGradientMode);
end;

{ TdxNavBarCustomGroupBorderPainter }

class procedure TdxNavBarCustomGroupBorderPainter.InternalDrawBorder(ACanvas: TCanvas;
  ARect: TRect; AColor: TColor; AWithCaption: Boolean);
begin
end;

class procedure TdxNavBarCustomGroupBorderPainter.DrawBorder(ACanvas: TCanvas;
  ARect: TRect; AColor: TColor; AWithCaption: Boolean);
begin
  InternalDrawBorder(ACanvas, ARect, AColor, AWithCaption);
end;

{ TdxNavBarCustomSignPainter }

class procedure TdxNavBarCustomSignPainter.DrawSign(
  ACanvas: TCanvas; ARect: TRect; AScaleFactor: TdxScaleFactor;
  AForeColor, ABackColor1, ABackColor2: TColor; AState: TdxNavBarObjectStates);
begin
  InternalDrawSign(ACanvas, ARect, AScaleFactor, AForeColor, ABackColor1, ABackColor2, AState);
end;

class procedure TdxNavBarCustomSignPainter.InternalDrawSign(
  ACanvas: TCanvas; ARect: TRect; AScaleFactor: TdxScaleFactor;
  AForeColor, ABackColor1, ABackColor2: TColor; AState: TdxNavBarObjectStates);
begin
  // do nothing
end;

{ TdxNavBarCustomHintPainter }

class procedure TdxNavBarCustomHintPainter.DrawHint(ACanvas: TCanvas; ARect: TRect;
  AHint: string; APicture: TPicture; AColor1, AColor2: TColor;
  AAlphaBlend1, AAlphaBlend2: Byte; AGradientMode: TdxBarStyleGradientMode;
  AFont: TFont);
begin
  InternalDrawHint(ACanvas, ARect, AHint, APicture, AColor1, AColor2,
    AAlphaBlend1, AAlphaBlend2, AGradientMode, AFont);
end;

class procedure TdxNavBarCustomHintPainter.InternalDrawHint(ACanvas: TCanvas;
  ARect: TRect; AHint: string; APicture: TPicture; AColor1,
  AColor2: TColor; AAlphaBlend1, AAlphaBlend2: Byte;
  AGradientMode: TdxBarStyleGradientMode; AFont: TFont);
begin
  if AHint <> '' then
  begin
    TdxNavBarCustomBackgroundPainter.DrawBackground(ACanvas, ARect, APicture, True, clInfoBk,
      AColor1, AColor2, AAlphaBlend1, AAlphaBlend2, AGradientMode);
    Windows.DrawEdge(ACanvas.Handle, ARect, BDR_RAISEDOUTER, BF_RECT);
    InflateRect(ARect, -1, -1);
    Inc(ARect.Left, dxNavBarHintWindowTextOffset.X);
    Inc(ARect.Top, dxNavBarHintWindowTextOffset.Y);
    ACanvas.Font := AFont;
    cxDrawText(ACanvas, AHint, ARect, DT_LEFT or DT_NOPREFIX or DT_WORDBREAK);
  end;
end;

{ TdxNavBarCustomButtonPainter }

class procedure TdxNavBarCustomButtonPainter.DrawButton(ACanvas: TCanvas;
  ARect: TRect; APicture: TPicture; AColor1, AColor2: TColor; AAlphaBlend1,
  AAlphaBlend2: Byte; AGradientMode: TdxBarStyleGradientMode;
  ABorderColor: TColor; AState: TdxNavBarObjectStates);
begin
  InternalDrawButton(ACanvas, ARect, APicture, AColor1, AColor2,
    AAlphaBlend1, AAlphaBlend2, AGradientMode, ABorderColor, AState);
end;

class procedure TdxNavBarCustomButtonPainter.InternalDrawButton(ACanvas: TCanvas;
  ARect: TRect; APicture: TPicture; AColor1, AColor2: TColor; AAlphaBlend1,
  AAlphaBlend2: Byte; AGradientMode: TdxBarStyleGradientMode;
  ABorderColor: TColor; AState: TdxNavBarObjectStates);
begin
  TdxNavBarCustomBackgroundPainter.DrawBackground(ACanvas, ARect, APicture, False, clBtnFace, AColor1, AColor2,
    AAlphaBlend1, AAlphaBlend2, AGradientMode);
end;

{ TdxNavBarCustomScrollButtonsPainter }

class procedure TdxNavBarCustomScrollButtonsPainter.DrawBottomButton(
  ACanvas: TCanvas; ARect: TRect;
  AButtonPainterClass: TdxNavBarCustomButtonPainterClass; AColor1, AColor2: TColor;
  AAlphaBlend1, AAlphaBlend2: Byte; AGradientMode: TdxBarStyleGradientMode;
  ABorderColor: TColor; AState: TdxNavBarObjectStates);
begin
  InternalDrawBottomButton(ACanvas, ARect, AButtonPainterClass, AColor1, AColor2,
    AAlphaBlend1, AAlphaBlend2, AGradientMode, ABorderColor, AState);
end;

class procedure TdxNavBarCustomScrollButtonsPainter.DrawTopButton(ACanvas: TCanvas;
  ARect: TRect; AButtonPainterClass: TdxNavBarCustomButtonPainterClass; AColor1,
  AColor2: TColor; AAlphaBlend1, AAlphaBlend2: Byte; AGradientMode: TdxBarStyleGradientMode;
  ABorderColor: TColor; AState: TdxNavBarObjectStates);
begin
  InternalDrawTopButton(ACanvas, ARect, AButtonPainterClass, AColor1, AColor2,
    AAlphaBlend1, AAlphaBlend2, AGradientMode, ABorderColor, AState);
end;

class procedure TdxNavBarCustomScrollButtonsPainter.InternalDrawBottomButton(
  ACanvas: TCanvas; ARect: TRect;
  AButtonPainterClass: TdxNavBarCustomButtonPainterClass; AColor1, AColor2: TColor;
  AAlphaBlend1, AAlphaBlend2: Byte; AGradientMode: TdxBarStyleGradientMode;
  ABorderColor: TColor; AState: TdxNavBarObjectStates);
begin

end;

class procedure TdxNavBarCustomScrollButtonsPainter.InternalDrawTopButton(
  ACanvas: TCanvas; ARect: TRect;
  AButtonPainterClass: TdxNavBarCustomButtonPainterClass; AColor1, AColor2: TColor;
  AAlphaBlend1, AAlphaBlend2: Byte; AGradientMode: TdxBarStyleGradientMode;
  ABorderColor: TColor; AState: TdxNavBarObjectStates);
begin

end;

{ TdxNavBarCustomDropTargetLinkPainter }

class procedure TdxNavBarCustomDropTargetLinkPainter.DrawTargetLink(
  ACanvas: TCanvas; pt1, pt2, pt3, pt4, pt5, pt6: TPoint; AColor: TColor);
begin
  InternalDrawTargetLink(ACanvas, pt1, pt2, pt3, pt4, pt5, pt6, AColor);
end;

class procedure TdxNavBarCustomDropTargetLinkPainter.InternalDrawTargetLink(
  ACanvas: TCanvas; pt1, pt2, pt3, pt4, pt5, pt6: TPoint; AColor: TColor);
begin
  ACanvas.Brush.Style := bsSolid;
  ACanvas.Brush.Color := AColor;
  ACanvas.Pen.Style := psSolid;
  ACanvas.Pen.Color := AColor;
  ACanvas.Polygon([pt1, pt2, pt3, pt1, pt4, pt5, pt6, pt4]);
end;

{ TdxNavBarCustomDropTargetGroupPainter }

class procedure TdxNavBarCustomDropTargetGroupPainter.DrawTargetGroup(ACanvas: TCanvas;
  ARect: TRect; AColor1, AColor2: TColor; AAlphaBlend1, AAlphaBlend2: Byte;
  AGradientMode: TdxBarStyleGradientMode);
begin
  InternalDrawTargetGroup(ACanvas, ARect, AColor1, AColor2,
    AAlphaBlend1, AAlphaBlend2, AGradientMode);
end;

class procedure TdxNavBarCustomDropTargetGroupPainter.InternalDrawTargetGroup(
  ACanvas: TCanvas; ARect: TRect; AColor1, AColor2: TColor; AAlphaBlend1,
  AAlphaBlend2: Byte; AGradientMode: TdxBarStyleGradientMode);
begin
  if IsGdiPlusAvailable then
    TdxNavBarCustomBackgroundPainter.DrawBackground(ACanvas, ARect, nil, False, clBlack,
      AColor1, AColor2, AAlphaBlend1, AAlphaBlend2, AGradientMode)
  else
    with TcxCanvas.Create(ACanvas) do
    begin
      DrawComplexFrame(ARect, AColor1, AColor1, cxBordersAll, 2);
      Free;
    end;
end;

{ TdxNavBarElementPainter }

procedure TdxNavBarElementPainter.DrawBackground;
begin
  with ViewInfo do
    BackgroundPainterClass.DrawBackground(Canvas, NavBar.ClientRect, BgImage, True, NavBar.Color,
      BgBackColor, BgBackColor2, BgAlphaBlend, BgAlphaBlend2,
      BgGradientMode);
end;

procedure TdxNavBarElementPainter.DrawDropTargetGroupSelection;
var
  AGroupViewInfo: TdxNavBarGroupViewInfo;
  AGroup: TdxNavBarGroup;
begin
  if not NavBar.EnableDragging or not NavBar.IsTargetValid then exit;
  AGroup := NavBar.TargetGroup;
  if AGroup <> nil then
    AGroupViewInfo := ViewInfo.GetGroupViewInfoByGroup(AGroup)
  else
  begin
    AGroupViewInfo := ViewInfo.GetGroupViewInfoAtCaptionPos(NavBar.TargetPoint);
    if AGroupViewInfo = nil then
      AGroupViewInfo := ViewInfo.GetGroupViewInfoAtItemsPos(NavBar.TargetPoint);
  end;
  if (AGroupViewInfo <> nil) and (AGroupViewInfo.Group <> NavBar.SourceGroup) then
    with ViewInfo do
    DropTargetGroupPainterClass.DrawTargetGroup(Canvas, AGroupViewInfo.CaptionRect,
      DragDropGroupTargetBackColor, DragDropGroupTargetBackColor2,
      DragDropGroupTargetAlphaBlend, DragDropGroupTargetAlphaBlend2,
      DragDropGroupTargetGradient);
end;

procedure TdxNavBarElementPainter.DrawDropTargetLinkSelection;
var
  Item1, Item2: TdxNavBarCustomItemViewInfo;
  ItemGroup: TdxNavBarGroupViewInfo;
  APosition: Integer;
begin
  if not NavBar.EnableDragging or ((NavBar.SourceLink = nil) and (NavBar.SourceItem = nil) and (NavBar.SourceGroup = nil)) or
    ([fAllowDropLink, fAllowDropGroup] * NavBar.DragDropFlags = []) then exit;
  APosition := NavBar.TargetPosition;
  ItemGroup := ViewInfo.GetGroupViewInfoAtPos(TdxCustomNavBarAccess(NavBar).GetMouseCursorClientPos);
  if ItemGroup = nil then
    Exit;
  if InRange(APosition, 1, ItemGroup.Infos.Count - 1) then
  begin
    Item1 := ItemGroup.Infos[APosition - 1];
    Item2 := ItemGroup.Infos[APosition];
  end
  else
    if (APosition = -1) or (ItemGroup.Infos.Count = 0) then
    begin
      Item1 := nil;
      Item2 := nil;
    end
    else
    if APosition = 0 then
    begin
      Item1 := nil;
      Item2 := ItemGroup.Infos[0];
    end
    else
    begin
      Item1 := ItemGroup.Infos[ItemGroup.Infos.Count - 1];
      Item2 := nil;
    end;


//  if (Item1 <> nil) and (NavBar.SourceItem = nil) and (Item1.Link = NavBar.SourceLink) then exit;
//  if (Item2 <> nil) and (NavBar.SourceItem = nil) and (Item2.Link = NavBar.SourceLink) then exit;
  if (Item1 <> nil) or (Item2 <> nil) then
    DoDrawDropTargetLinkSelection(ItemGroup, Item1, Item2);
end;

procedure TdxNavBarElementPainter.DrawChildGroupSelection(
  AChildGroupViewInfo: TdxNavBarChildGroupViewInfo);
begin
  with AChildGroupViewInfo do
    SelectionPainterClass.DrawSelection(Canvas, SelectionRect, BgBackColor, State);
end;

procedure TdxNavBarElementPainter.DrawGroupBackground(AGroupViewInfo: TdxNavBarGroupViewInfo);
begin
  with AGroupViewInfo do
    GroupBackgroundPainterClass.DrawBackground(Canvas, ItemsRect, BgImage,
      BgBackColor, BgBackColor2, BgAlphaBlend, BgAlphaBlend2, BgGradientMode);
end;

procedure TdxNavBarElementPainter.DrawGroupBorder(AGroupViewInfo: TdxNavBarGroupViewInfo);
begin
  if not TdxNavBarViewInfoAccess(ViewInfo).IsGroupActive(AGroupViewInfo.Group) then exit;
  with AGroupViewInfo do
    GroupBorderPainterClass.DrawBorder(Canvas, ItemsRect, BorderColor, IsCaptionVisible);
end;

procedure TdxNavBarElementPainter.DrawGroupCaptionButton(AGroupViewInfo: TdxNavBarGroupViewInfo);
begin
  with AGroupViewInfo do
    ButtonPainterClass.DrawButton(Canvas, CaptionRect, CaptionImage,
      CaptionBackColor, CaptionBackColor2, CaptionAlphaBlend, CaptionAlphaBlend2,
      CaptionGradientMode, CaptionBorderColor, State);
end;

procedure TdxNavBarElementPainter.DrawGroupCaptionImage(AGroupViewInfo: TdxNavBarGroupViewInfo);
begin
  with AGroupViewInfo do
    ImagePainterClass.DrawImage(Canvas, ImageList, ImageIndex, CaptionImageRect);
end;

procedure TdxNavBarElementPainter.DrawGroupCaptionSign(AGroupViewInfo: TdxNavBarGroupViewInfo);
begin
  if AGroupViewInfo.Group.ShowExpandButton then
    with AGroupViewInfo do
      SignPainterClass.DrawSign(Canvas, CaptionSignRect, ScaleFactor, CaptionSignColor, CaptionBackColor, CaptionBackColor2, State);
end;

procedure TdxNavBarElementPainter.DrawGroupControl(ACanvas: TCanvas; ARect: TRect; AGroupViewInfo: TdxNavBarGroupViewInfo);
begin
  with AGroupViewInfo do
    GroupBackgroundPainterClass.DrawBackground(ACanvas, ARect, ControlImage,
      ControlBackColor, ControlBackColor2, ControlAlphaBlend, ControlAlphaBlend2,
      ControlGradientMode);
end;

procedure TdxNavBarElementPainter.DrawItemImage(ALinkViewInfo: TdxNavBarLinkViewInfo);
begin
  with TdxNavBarLinkViewInfoAccess(ALinkViewInfo) do
  begin
    ImagePainterClass.DrawImage(Canvas, ImageList, ImageIndex,
      ImageRect, IsEnabled or UseDisabledImages, GetItemColorPalette(ALinkViewInfo));
    if IsRectEmpty(CaptionRect) and Link.IAccessibilityHelper.IsFocused then
      DrawSolidFocusRect(cxRectInflate(ImageRect, 1, 1), FontColor);
  end;
end;

procedure TdxNavBarElementPainter.DrawItemSelection(ALinkViewInfo: TdxNavBarLinkViewInfo);
begin
  with ALinkViewInfo do
    if Item.Enabled then
      SelectionPainterClass.DrawSelection(Canvas, SelectionRect, GroupViewInfo.BgBackColor, State);
end;

procedure TdxNavBarElementPainter.DrawBottomScrollButton;
begin
  with ViewInfo do
  begin
    ScrollButtonsPainterClass.DrawBottomButton(Canvas, BottomScrollButtonRect,
      ButtonPainterClass, BottomScrollButtonBackColor, BottomScrollButtonBackColor2,
      BottomScrollButtonAlphaBlend, BottomScrollButtonAlphaBlend2, BottomScrollButtonGradientMode,
      BorderColor, BottomScrollButtonState);
    with BottomScrollButtonRect do
      ExcludeClipRect(Canvas.Handle, Left, Top, Right, Bottom);
  end;
end;

procedure TdxNavBarElementPainter.DrawTopScrollButton;
begin
  with ViewInfo do
  begin
    ScrollButtonsPainterClass.DrawTopButton(Canvas, TopScrollButtonRect,
      ButtonPainterClass, TopScrollButtonBackColor, TopScrollButtonBackColor2,
      TopScrollButtonAlphaBlend, TopScrollButtonAlphaBlend2, TopScrollButtonGradientMode,
      BorderColor, TopScrollButtonState);
    with TopScrollButtonRect do
      ExcludeClipRect(Canvas.Handle, Left, Top, Right, Bottom);
  end;
end;

procedure TdxNavBarElementPainter.DoDrawDropTargetLinkSelection(AItemGroup: TdxNavBarGroupViewInfo;
  AItem1, AItem2: TdxNavBarCustomItemViewInfo);
var
  lpt1, lpt2, lapt1, lapt2, lapt3, rapt1, rapt2, rapt3: TPoint;
  AViewInfoAccess: TdxNavBarViewInfoAccess;
  AWidth: Integer;
begin
  if AItem1 <> nil then
  begin
    if not AItemGroup.IsViewAsIconView then
      lpt1 := cxRectLeftBottom(AItem1.Rect)
    else
      lpt1 := cxRectRightTop(AItem1.Rect);
    lpt2 := AItem1.Rect.BottomRight;
    AWidth := cxRectWidth(AItem1.Rect);
  end
  else
  begin
    lpt1 := AItem2.Rect.TopLeft;
    AWidth := cxRectWidth(AItem2.Rect);
    if not AItemGroup.IsViewAsIconView then
      lpt2 := cxRectRightTop(AItem2.Rect)
    else
      lpt2 := cxRectLeftBottom(AItem2.Rect);
  end;

  AViewInfoAccess := TdxNavBarViewInfoAccess(ViewInfo);
  if PtInRect(cxRectInflate(AItemGroup.ItemsRect, 1, 1), lpt1) and PtInRect(cxRectInflate(AItemGroup.ItemsRect, 1, 1), lpt2) then
  begin
    lapt1 := lpt1;
    lapt2 := lpt1;
    lapt3 := lpt1;
    rapt1 := lpt2;
    rapt2 := lpt2;
    rapt3 := lpt2;
    if not AItemGroup.IsViewAsIconView then
    begin
      lapt1.x := lpt1.x + AViewInfoAccess.GetDragArrowWidth;
      if AItem1 <> nil then
        lapt2.y := lpt1.y - AViewInfoAccess.GetDragArrowHeight div 2;
      if AItem2 <> nil then
        lapt3.y := lpt1.y + AViewInfoAccess.GetDragArrowHeight div 2;
      rapt1.x := lapt1.x + AWidth - 1 - 2 * AViewInfoAccess.GetDragArrowWidth;
      rapt2.x := lapt2.x + AWidth - 1;
      rapt2.y := lapt2.y;
      rapt3.x := lapt3.x + AWidth - 1;
      rapt3.y := lapt3.y;
    end;
    DropTargetLinkPainterClass.DrawTargetLink(Canvas, lapt1, lapt2, lapt3, rapt1, rapt2, rapt3,
      ViewInfo.DragDropItemTargetBackColor);
  end;
end;

procedure TdxNavBarElementPainter.DoDrawHint(ACanvas: TCanvas; const ARect: TRect);
begin
  with ViewInfo do
    TdxNavBarCustomHintPainter.DrawHint(ACanvas, ARect,
        ViewInfo.HintText, HintImage, HintBackColor, HintBackColor2,
        HintAlphaBlend, HintAlphaBlend2, HintGradientMode, HintFont);
end;

function TdxNavBarElementPainter.GetItemColorPalette(ALinkViewInfo: TdxNavBarLinkViewInfo): IdxColorPalette;
begin
  Result := nil;
end;

class function TdxNavBarElementPainter.BackgroundPainterClass: TdxNavBarCustomBackgroundPainterClass;
begin
  Result := TdxNavBarCustomBackgroundPainter;
end;

class function TdxNavBarElementPainter.ButtonPainterClass: TdxNavBarCustomButtonPainterClass;
begin
  Result := TdxNavBarCustomButtonPainter;
end;

class function TdxNavBarElementPainter.DropTargetGroupPainterClass: TdxNavBarCustomDropTargetGroupPainterClass;
begin
  Result := TdxNavBarCustomDropTargetGroupPainter;
end;

class function TdxNavBarElementPainter.DropTargetLinkPainterClass: TdxNavBarCustomDropTargetLinkPainterClass;
begin
  Result := TdxNavBarCustomDropTargetLinkPainter;
end;

class function TdxNavBarElementPainter.GroupBackgroundPainterClass: TdxNavBarCustomGroupBackgroundPainterClass;
begin
  Result := TdxNavBarCustomGroupBackgroundPainter;
end;

class function TdxNavBarElementPainter.GroupBorderPainterClass: TdxNavBarCustomGroupBorderPainterClass;
begin
  Result := TdxNavBarCustomGroupBorderPainter;
end;

class function TdxNavBarElementPainter.ImagePainterClass: TdxNavBarCustomImagePainterClass;
begin
  Result := TdxNavBarCustomImagePainter;
end;

class function TdxNavBarElementPainter.ScrollButtonsPainterClass: TdxNavBarCustomScrollButtonsPainterClass;
begin
  Result := TdxNavBarCustomScrollButtonsPainter;
end;

class function TdxNavBarElementPainter.SelectionPainterClass: TdxNavBarCustomSelectionPainterClass;
begin
  Result := TdxNavBarCustomSelectionPainter;
end;

class function TdxNavBarElementPainter.SignPainterClass: TdxNavBarCustomSignPainterClass;
begin
  Result := TdxNavBarCustomSignPainter;
end;

end.
