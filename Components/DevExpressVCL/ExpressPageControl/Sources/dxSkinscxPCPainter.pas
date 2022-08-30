{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressSkins Library                                     }
{                                                                    }
{           Copyright (c) 2006-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSSKINS AND ALL ACCOMPANYING     }
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

unit dxSkinscxPCPainter;

{$I cxVer.inc}

interface

uses
  Windows, SysUtils, Classes, Controls, Graphics,
  dxCore, dxCoreGraphics, cxGraphics, cxGeometry, cxControls, cxLookAndFeels, dxSkinsCore, cxLookAndFeelPainters,
  cxPCPainters, cxPC, dxSkinInfo, cxPCPaintersFactory;

type
  { TcxPCSkinPainter }

  TcxPCSkinIndents = (siFar, siNear, siSelectedDownGrow, siHorzGrow, siVertGrow,
    siDownGrow, siDownGrowBottomRight, siSelectedDownGrowBottomRight);

  TcxPCSkinPainter = class(TcxPCTabsPainter)
  strict private const
    ButtonStates: array[TcxPCNavigatorButtonState] of TdxSkinElementState = (esNormal, esPressed, esHot, esDisabled);
  strict private
    FFrameContentCache: TdxSkinElementCache;

    function GetHeaderButton: TdxSkinElement;
    function GetNeedDrawTabBitmapBackground: Boolean;
    function GetTabState(ATabViewInfo: TcxTabViewInfo): TdxSkinElementState;
    procedure InternalCorrectCustomTabRect(APosition: TcxTabPosition; var ARect: TRect);
    procedure InternalCorrectMainTabRect(APosition: TcxTabPosition; var ARect: TRect);
  protected
    function CalculateTabNormalWidth(ATabViewInfo: TcxTabViewInfo): Integer; override;
    procedure CorrectTabNormalWidth(var AValue: Integer); override;
    function GetButtonHorz: TdxSkinElement; virtual;
    function GetButtonVert: TdxSkinElement; virtual;
    function GetFrameContent: TdxSkinElement; virtual;
    function GetHeader: TdxSkinElement; virtual;
    function GetIndentByIndex(AType: TcxPCSkinIndents): Integer; virtual;
    function GetSkinPainterData(var AData: TdxSkinInfo): Boolean;
    function GetTabClipRgn(ACanvas: TcxCanvas; ATabVisibleIndex: Integer): TcxRegion; override;
    function GetTabCorrection(ATabVisibleIndex: Integer): TRect; override;
    procedure DoPaintPageFrame(ACanvas: TcxCanvas); override;
    procedure DrawNativeTabBackground(DC: HDC; ATab: TcxTabSheet); override;
    procedure FillTabPaneContent(ACanvas: TcxCanvas); override;
    function GetButtonDrawOffsets: TRect; override;
    function DoGetButtonWidth(Button: TcxPCNavigatorButton): Integer; override;
    function GetFreeSpaceColor: TColor; override;
    function GetLookAndFeelPainter: TcxCustomLookAndFeelPainter; override;
    function GetNativeButtonHeight: Integer; override;
    function GetNativeButtonsDistance(AButton1, AButton2: TcxPCNavigatorButton): Integer; override;
    function GetNativeContentOffset: TRect; override;
    function GetTabButtonGlyphOffset: TRect; override;
    function GetTabContentWOffset(ATabVisibleIndex: Integer): TcxPCWOffset; override;
    function GetTabFocusRect(const ATabBounds: TRect): TRect; override;
    function GetTabNormalContentOffset(ATabVisibleIndex: Integer): Integer; override;
    function GetTabsNormalDistance: TcxPCDistance; override;
    function GetTabsPosition: TcxPCTabsPosition; override;
    function GetTabTextNormalWidth(ATabVisibleIndex: Integer): Integer; override;
    function GetTextColor(ATabVisibleIndex: Integer): TColor; override;
    procedure InternalDrawText(ACanvas: TcxCanvas; const ACaption: string; ARect: TRect; ATabVisibleIndex: Integer); override;
    procedure InternalPaintFrame(ACanvas: TcxCanvas); override;
    function IsEnableHotTrack: Boolean; override;
    function IsNativePainting: Boolean; override;
    function IsSkinAvailable: Boolean;
    function IsTabBorderThick(ATabVisibleIndex: Integer): Boolean; override;
    function NeedDisabledTextShadow: Boolean; override;
    function NeedDoubleBuffer: Boolean; override;

    procedure DrawParentBackground(ACanvas: TcxCanvas; const R: TRect); virtual;
    procedure DrawTabText(ACanvas: TcxCanvas; const ARect: TRect; const AText: string;
      AEnabled: Boolean; AColor: TColor; ATabVisibleIndex: Integer); override;
    procedure PaintButton(ACanvas: TcxCanvas; const ARect: TRect;
      AState: TcxPCNavigatorButtonState; AType: TcxPCNavigatorButton); override;
    procedure PaintHeaderButton(ACanvas: TcxCanvas; AButtonInfo: TcxPCHeaderButtonViewInfo); override;
    procedure PaintNativeTabBackground(DC: HDC; ATabVisibleIndex: Integer; const ABounds: TRect); override;
    procedure PaintTabsRegion(ACanvas: TcxCanvas); override;
    procedure PrepareTabBitmapBackground(ABitmap: TcxBitmap; const ARect: TRect; ATabViewInfo: TcxTabViewInfo); override;

    // Color Palette
    function GetHeaderButtonGlyphPalette(AState: TcxPCNavigatorButtonState): IdxColorPalette; override;
    function GetTabButtonColorPalette(AState: TcxPCNavigatorButtonState): IdxColorPalette; override;
    function GetTabImageColorPalette: IdxColorPalette; override;

    property ButtonHorz: TdxSkinElement read GetButtonHorz;
    property ButtonVert: TdxSkinElement read GetButtonVert;
    property FrameContent: TdxSkinElement read GetFrameContent;
    property FrameContentCache: TdxSkinElementCache read FFrameContentCache;
    property Header: TdxSkinElement read GetHeader;
    property HeaderButton: TdxSkinElement read GetHeaderButton;
    property Indents[AType: TcxPCSkinIndents]: Integer read GetIndentByIndex;
    property NeedDrawTabBitmapBackground: Boolean read GetNeedDrawTabBitmapBackground;
  public
    constructor Create(AViewInfo: TcxCustomTabControlViewInfo); override;
    destructor Destroy; override;
    function CalculateTabNormalHeight: Integer; override;
    class function GetStyleID: TcxPCStyleID; override;
    class function GetStyleName: TCaption; override;
    class function HasLookAndFeel(ALookAndFeel: TcxLookAndFeel): Boolean; override;
  end;

implementation

uses
  Types, Math;

{ TcxPCSkinPainter }

constructor TcxPCSkinPainter.Create(AViewInfo: TcxCustomTabControlViewInfo);
begin
  inherited;
  FFrameContentCache := TdxSkinElementCache.Create;
end;

destructor TcxPCSkinPainter.Destroy;
begin
  FreeAndNil(FFrameContentCache);
  inherited Destroy;
end;

function TcxPCSkinPainter.CalculateTabNormalHeight: Integer;
begin
  if Header = nil then
    Result := inherited CalculateTabNormalHeight
  else
    with Header.ContentOffset do
      Result := Top + Bottom + inherited CalculateTabNormalHeight - 2;
end;

class function TcxPCSkinPainter.GetStyleID: TcxPCStyleID;
begin
  Result := cxPCSkinStyle;
end;

class function TcxPCSkinPainter.GetStyleName: TCaption;
begin
  Result := 'Skin';
end;

class function TcxPCSkinPainter.HasLookAndFeel(ALookAndFeel: TcxLookAndFeel): Boolean;
begin
  Result := ALookAndFeel.SkinPainter <> nil;
end;

function TcxPCSkinPainter.CalculateTabNormalWidth(ATabViewInfo: TcxTabViewInfo): Integer;
begin
  Result := inherited CalculateTabNormalWidth(ATabViewInfo);
  if ATabViewInfo.GetDefinedWidth > 0 then
    Inc(Result, GetTabNormalContentOffset(ATabViewInfo.VisibleIndex));
end;

procedure TcxPCSkinPainter.CorrectTabNormalWidth(var AValue: Integer);
begin
//do nothing
end;

procedure TcxPCSkinPainter.InternalCorrectCustomTabRect(APosition: TcxTabPosition; var ARect: TRect);
begin
  case APosition of
    tpTop:
      ARect.Bottom := Indents[siDownGrow] + 1;
    tpBottom:
      ARect.Top := -Indents[siDownGrowBottomRight] - 1;
    tpLeft:
      ARect.Right := Indents[siDownGrow] + 1;
    tpRight:
      ARect.Left := -Indents[siDownGrowBottomRight] - 1;
  end;
end;

procedure TcxPCSkinPainter.InternalCorrectMainTabRect(APosition: TcxTabPosition; var ARect: TRect);
begin
  case APosition of
    tpTop:
      ARect := Rect(-Indents[siHorzGrow], -Indents[siVertGrow],
        Indents[siHorzGrow], Indents[siSelectedDownGrow]);
    tpBottom:
      ARect := Rect(-Indents[siHorzGrow], -Indents[siSelectedDownGrowBottomRight],
        Indents[siHorzGrow], Indents[siVertGrow]);
    tpLeft:
      ARect := Rect(-Indents[siVertGrow], -Indents[siHorzGrow],
        Indents[siSelectedDownGrow], Indents[siHorzGrow]);
    tpRight:
      ARect := Rect(-Indents[siSelectedDownGrowBottomRight],
        -Indents[siHorzGrow], Indents[siVertGrow], Indents[siHorzGrow]);
  end;
end;

function TcxPCSkinPainter.GetTabCorrection(ATabVisibleIndex: Integer): TRect;
var
  ATabViewInfo: TcxTabViewInfo;
begin
  if IsSkinAvailable then
  begin
    Result := cxNullRect;
    ATabViewInfo := TabViewInfo[ATabVisibleIndex];
    if ATabViewInfo.IsMainTab then
      InternalCorrectMainTabRect(ATabViewInfo.PaintingPosition, Result)
    else
      InternalCorrectCustomTabRect(ATabViewInfo.PaintingPosition, Result);
  end
  else
    Result := inherited GetTabCorrection(ATabVisibleIndex);
end;

function TcxPCSkinPainter.GetTabClipRgn(ACanvas: TcxCanvas; ATabVisibleIndex: Integer): TcxRegion;
var
  ATabViewInfo: TcxTabViewInfo;
  ATabRect: TRect;
begin
  ATabViewInfo := TabViewInfo[ATabVisibleIndex];
  IntersectRect(ATabRect, ATabViewInfo.VisibleRect, ATabViewInfo.NormalRect);
  Result := TcxRegion.Create(cxRectTransform(ATabRect, GetTabCorrection(ATabVisibleIndex)));
end;

procedure TcxPCSkinPainter.FillTabPaneContent(ACanvas: TcxCanvas);
var
  AFrameContent: TdxSkinElement;
begin
  AFrameContent := FrameContent;
  if AFrameContent = nil then
    inherited
  else
    AFrameContent.Draw(ACanvas.Handle, GetPageFrameRect);
end;

function TcxPCSkinPainter.GetButtonDrawOffsets: TRect;
begin
  Result := cxEmptyRect;
end;

procedure TcxPCSkinPainter.DoPaintPageFrame(ACanvas: TcxCanvas);
begin
end;

procedure TcxPCSkinPainter.DrawNativeTabBackground(DC: HDC; ATab: TcxTabSheet);
var
  AFrameContent: TdxSkinElement;
begin
  AFrameContent := FrameContent;
  if AFrameContent <> nil then
    AFrameContent.Draw(DC, cxRectOffset(GetPageFrameRect, -ATab.Left, -ATab.Top))
  else
    inherited;
end;

function TcxPCSkinPainter.DoGetButtonWidth(Button: TcxPCNavigatorButton): Integer;
var
  AElement: TdxSkinElement;
begin
  AElement := ButtonHorz;
  if AElement <> nil then
    Result := dxSkinGetElementSize(AElement, ScaleFactor).cx
  else
    Result := 0;

  if Result = 0 then
    Result := inherited DoGetButtonWidth(Button);
end;

function TcxPCSkinPainter.GetFreeSpaceColor: TColor;
var
  ASkinInfo: TdxSkinInfo;
begin
  if GetSkinPainterData(ASkinInfo) and (ASkinInfo.ContentColor <> nil)  then
    Result := ASkinInfo.ContentColor.Value
  else
    Result := inherited GetFreeSpaceColor;
end;

function TcxPCSkinPainter.GetLookAndFeelPainter: TcxCustomLookAndFeelPainter;
begin
  if ViewInfo.GetLookAndFeel.SkinPainter <> nil then
    Result := ViewInfo.GetLookAndFeel.SkinPainter
  else
    Result := inherited GetLookAndFeelPainter;
end;

function TcxPCSkinPainter.GetNativeButtonHeight: Integer;
begin
  Result := ScaleFactor.Apply(17);
end;

function TcxPCSkinPainter.GetNativeButtonsDistance(AButton1, AButton2: TcxPCNavigatorButton): Integer;
begin
  Result := 0;
end;

function TcxPCSkinPainter.GetNativeContentOffset: TRect;
begin
  if FrameContent = nil then
    Result := cxNullRect
  else
    Result := cxRectTransform(FrameContent.ContentOffset.Rect, 1, 1, 1, 1);
end;

function TcxPCSkinPainter.GetTabButtonGlyphOffset: TRect;
begin
  Result := ScaleFactor.Apply(Rect(4, 4, 4, 4));
end;

function TcxPCSkinPainter.GetTabContentWOffset(ATabVisibleIndex: Integer): TcxPCWOffset;
begin
  if Header <> nil then
  begin
    Result.Left := ScaleFactor.Apply(Header.ContentOffset.Left);
    Result.Right := ScaleFactor.Apply(Header.ContentOffset.Right);
  end
  else
    Result := inherited GetTabContentWOffset(ATabVisibleIndex);
end;

function TcxPCSkinPainter.GetTabFocusRect(const ATabBounds: TRect): TRect;
begin
  if Header <> nil then
  begin
    Result := cxRectContent(ATabBounds, Header.ContentOffset.Rect);
    Dec(Result.Bottom, Indents[siSelectedDownGrow]);
    InflateRect(Result, 1, 1);
  end
  else
    Result := inherited GetTabFocusRect(ATabBounds);
end;

function TcxPCSkinPainter.GetHeaderButtonGlyphPalette(AState: TcxPCNavigatorButtonState): IdxColorPalette;
var
  AElement: TdxSkinElement;
begin
  AElement := HeaderButton;
  if AElement <> nil then
    Result := AElement.GetGlyphColorPalette(ButtonStates[AState])
  else
    Result := nil;
end;

function TcxPCSkinPainter.GetTabButtonColorPalette(AState: TcxPCNavigatorButtonState): IdxColorPalette;
begin
  if UseLookAndFeelTabButton then
    Result := inherited GetTabButtonColorPalette(AState)
  else
    Result := GetHeaderButtonGlyphPalette(AState);
end;

function TcxPCSkinPainter.GetTabImageColorPalette: IdxColorPalette;
var
  AElement: TdxSkinElement;
begin
  AElement := Header;
  if AElement <> nil then
    Result := AElement.GetGlyphColorPalette(esNormal)
  else
    Result := nil;
end;

function TcxPCSkinPainter.GetTabNormalContentOffset(ATabVisibleIndex: Integer): Integer;
begin
  if Header <> nil then
    Result := ScaleFactor.Apply(cxMarginsWidth(Header.ContentOffset.Rect))
  else
    Result := inherited GetTabNormalContentOffset(ATabVisibleIndex);
end;

function TcxPCSkinPainter.GetTabsNormalDistance: TcxPCDistance;
begin
  if ViewInfo.ActuallyRotate then
  begin
    Result.dw := Indents[siVertGrow];
    Result.dh := 0;
  end
  else
  begin
    Result.dw := 0;
    Result.dh := Indents[siVertGrow];
  end;
end;

function TcxPCSkinPainter.GetTabsPosition: TcxPCTabsPosition;
var
  AddX: Integer;
  NormalTabsRectCorrection: TRect;

  procedure DoHardCalculation;
  var
    AButtonsWidth: Integer;
    ATabsContainerOffset: TRect;
  begin
    AButtonsWidth := CalculateButtonsRegionWidth;
    if AButtonsWidth <> 0 then
      Inc(AButtonsWidth);
    ATabsContainerOffset := cxEmptyRect;
    if ViewInfo.IsTabsContainer then
      ATabsContainerOffset := GetTabsContainerOffsets;
    Inc(ATabsContainerOffset.Top, GetHeaderButtonHeightCorrection);
    NormalTabsRectCorrection := RotateRect(Rect(0, Indents[siVertGrow] + ATabsContainerOffset.Top, 0, 0), ViewInfo.TabPosition);
    Result.ExtendedTabsRect := cxRectContent(Result.ExtendedTabsRect, RotateRect(Rect(0, ATabsContainerOffset.Top, 0, 0), ViewInfo.TabPosition));
    with ViewInfo do
    begin
      if TabPosition in [tpTop, tpBottom] then
      begin
        if MultiLine or (NavigatorButtonCount = 0) then
        begin
          Result.ExtendedTabsRect.Left := ATabsContainerOffset.Left + Indents[siNear];
          Result.ExtendedTabsRect.Right := Width - ATabsContainerOffset.Left - Indents[siFar];
        end
        else
        begin
          if NavigatorPosition in [npLeftTop, npLeftBottom] then
          begin
            Result.ExtendedTabsRect.Left := AButtonsWidth;
            Result.ExtendedTabsRect.Right := Width - ATabsContainerOffset.Left - Indents[siFar];
          end
          else
          begin
            Result.ExtendedTabsRect.Left := ATabsContainerOffset.Left + Indents[siNear];
            Result.ExtendedTabsRect.Right := Width - AButtonsWidth;
          end;
        end;
        Result.NormalTabsRect.Left := Result.ExtendedTabsRect.Left + AddX;
        Result.NormalTabsRect.Right := Result.ExtendedTabsRect.Right - AddX;
        Result.NormalRowWidth := Result.NormalTabsRect.Right - Result.NormalTabsRect.Left;
      end
      else
      begin
        if MultiLine or (NavigatorButtonCount = 0) then
        begin
          Result.ExtendedTabsRect.Top := ATabsContainerOffset.Left + Indents[siNear];
          Result.ExtendedTabsRect.Bottom := Height - ATabsContainerOffset.Left - Indents[siFar];
        end
        else
        begin
          if NavigatorPosition in [npLeftTop, npRightTop] then
          begin
            Result.ExtendedTabsRect.Top := AButtonsWidth;
            Result.ExtendedTabsRect.Bottom := Height - ATabsContainerOffset.Left - Indents[siFar];
          end
          else
          begin
            Result.ExtendedTabsRect.Top := ATabsContainerOffset.Left + Indents[siNear];
            Result.ExtendedTabsRect.Bottom := Height - AButtonsWidth;
          end;
        end;
        Result.NormalTabsRect.Top := Result.ExtendedTabsRect.Top + AddX;
        Result.NormalTabsRect.Bottom := Result.ExtendedTabsRect.Bottom - AddX;
        Result.NormalRowWidth := Result.NormalTabsRect.Bottom - Result.NormalTabsRect.Top;
      end;
    end;
  end;

begin
  if IsSkinAvailable then
  begin
    AddX := Indents[siHorzGrow];
    with Result do
    begin
      ExtendedTabsRect := Rect(0, 0, ViewInfo.Width, ViewInfo.Height);
      NormalTabsRect := ExtendedTabsRect;
      DoHardCalculation;
      NormalTabsRect := cxRectContent(NormalTabsRect, NormalTabsRectCorrection);
      ExtendedTopOrLeftTabsRectBottomOrRightBorderOffset := 0;
      ExtendedBottomOrRightTabsRectTopOrLeftBorderOffset := 0;
      if ViewInfo.TabPosition in [tpTop, tpLeft] then
        ExtendedTopOrLeftTabsRectBottomOrRightBorderOffset := 1
      else
        ExtendedBottomOrRightTabsRectTopOrLeftBorderOffset := -1;
      MinDistanceBetweenTopOrLeftAndBottomOrRightExtendedTabsRects := 0;
    end;
    CalculateButtonsRegion;
  end
  else
    Result := inherited GetTabsPosition;
end;

function TcxPCSkinPainter.GetTabTextNormalWidth(ATabVisibleIndex: Integer): Integer;
begin
  Result := inherited GetTabTextNormalWidth(ATabVisibleIndex) + 2;
end;

function TcxPCSkinPainter.GetTextColor(ATabVisibleIndex: Integer): TColor;
var
  ASkinColor: TdxSkinColor;
  ASkinPainterInfo: TdxSkinInfo;
  ATabViewInfo: TcxTabViewInfo;
begin
  ATabViewInfo := ViewInfo.TabsViewInfo[ATabVisibleIndex];
  ASkinColor := nil;
  if GetSkinPainterData(ASkinPainterInfo) then
  begin
    if not ATabViewInfo.Enabled then
      ASkinColor := ASkinPainterInfo.TabTextColorDisabled
    else
      if ATabViewInfo.IsMainTab then
        ASkinColor := ASkinPainterInfo.TabTextColorActive
      else
        if ATabViewInfo.IsHotTrack then
          ASkinColor := ASkinPainterInfo.TabTextColorHot
        else
          ASkinColor := ASkinPainterInfo.TabTextColor;
  end;
  if ASkinColor = nil then
    Result := inherited GetTextColor(ATabVisibleIndex)
  else
    Result := ASkinColor.Value;
end;

procedure TcxPCSkinPainter.InternalDrawText(ACanvas: TcxCanvas;
  const ACaption: string; ARect: TRect; ATabVisibleIndex: Integer);
begin
  cxDrawText(ACanvas.Handle, ACaption, ARect, GetDTFlags(ATabVisibleIndex));
end;

procedure TcxPCSkinPainter.InternalPaintFrame(ACanvas: TcxCanvas);

  procedure ValidateFrameRect(var R: TRect; const AOffsets: TRect);
  begin
    if ViewInfo.IsTabsContainer then
    begin
      if ViewInfo.TabPosition in [tpTop, tpBottom] then
        InflateRect(R, Max(AOffsets.Left, AOffsets.Right), 0)
      else
        InflateRect(R, 0, Max(AOffsets.Top, AOffsets.Bottom));
    end;

    if ViewInfo.TabPosition = tpBottom then
      R.Top := Min(R.Top, R.Bottom - (AOffsets.Top + AOffsets.Bottom))
    else
      R.Bottom := Max(R.Bottom, R.Top + AOffsets.Top + AOffsets.Bottom);

    if ViewInfo.TabPosition = tpRight then
      R.Left := Min(R.Left, R.Right - (AOffsets.Left + AOffsets.Right))
    else
      R.Right := Max(R.Right, R.Left + AOffsets.Left + AOffsets.Right);
  end;

var
  ARect, ADrawRect: TRect;
begin
  if FrameContent <> nil then
  begin
    ARect := GetPageFrameRect;
    ADrawRect := GetDrawFrameRect;
    if FrameContent.IsAlphaUsed then
      DrawParentBackground(ACanvas, ViewInfo.ClientRect);

    cxPaintCanvas.BeginPaint(ACanvas.Canvas);
    try
      cxPaintCanvas.IntersectClipRect(ARect);
      ValidateFrameRect(ADrawRect, GetNativeContentOffset);
      if FrameContent.Image.GradientBeginColor = clNone then
        FrameContentCache.DrawEx(ACanvas.Handle, FrameContent, ADrawRect)
      else
        FrameContent.Draw(ACanvas.Handle, ADrawRect);
    finally
      cxPaintCanvas.EndPaint;
    end;
    if ACanvas.Handle = ViewInfo.Canvas.Handle then
      ViewInfo.Canvas.ExcludeClipRect(ARect);
  end
  else
    inherited InternalPaintFrame(ACanvas);
end;

function TcxPCSkinPainter.IsEnableHotTrack: Boolean;
begin
  Result := True;
end;

function TcxPCSkinPainter.IsNativePainting: Boolean;
begin
  Result := IsSkinAvailable or inherited IsNativePainting;
end;

function TcxPCSkinPainter.IsSkinAvailable: Boolean;
begin
  Result := ViewInfo.GetLookAndFeel.SkinPainter <> nil;
end;

function TcxPCSkinPainter.IsTabBorderThick(ATabVisibleIndex: Integer): Boolean;
begin
  Result := False;
end;

function TcxPCSkinPainter.NeedDisabledTextShadow: Boolean;
begin
  Result := False;
end;

function TcxPCSkinPainter.NeedDoubleBuffer: Boolean;
begin
  Result := False;
end;

procedure TcxPCSkinPainter.DrawParentBackground(ACanvas: TcxCanvas; const R: TRect);

  function HasParent: Boolean;
  var
    AControl: TControl;
  begin
    AControl := ViewInfo.IControl.GetControl;
    Result := (AControl <> nil) and (AControl.Parent <> nil);
  end;

begin
  if ViewInfo.CanDrawParentBackground then
    cxDrawTransparentControlBackground(ViewInfo.IControl.GetControl, ACanvas, R, False)
  else
    if not HasParent then
      FillFreeSpaceArea(ACanvas, R);
end;

procedure TcxPCSkinPainter.DrawTabText(ACanvas: TcxCanvas; const ARect: TRect; const AText: string;
  AEnabled: Boolean; AColor: TColor; ATabVisibleIndex: Integer);
begin
  InternalDrawText(ACanvas, AText, ARect, ATabVisibleIndex);
end;

procedure TcxPCSkinPainter.PaintButton(ACanvas: TcxCanvas;
  const ARect: TRect; AState: TcxPCNavigatorButtonState; AType: TcxPCNavigatorButton);
const
  ButtonKindVert: array[TcxPCNavigatorButton] of Integer = (2, 1, 3, 0);
  ButtonKindHorz: array[TcxPCNavigatorButton] of Integer = (1, 2, 3, 0);
var
  AElement: TdxSkinElement;
  AImageIndex: Integer;
begin
  if (ButtonHorz = nil) or (ButtonVert = nil) then
    inherited PaintButton(ACanvas, ARect, AState, AType)
  else
  begin
    if ViewInfo.TabPosition in [tpTop, tpBottom] then
    begin
      AElement := ButtonHorz;
      AImageIndex := ButtonKindHorz[AType];
    end
    else
    begin
      AElement := ButtonVert;
      AImageIndex := ButtonKindVert[AType];
    end;
    if AElement.IsAlphaUsed then
      DrawParentBackground(ACanvas, ARect);
    AElement.Draw(ACanvas.Handle, ARect, ScaleFactor, AImageIndex, ButtonStates[AState]);
  end;
end;

procedure TcxPCSkinPainter.PaintHeaderButton(ACanvas: TcxCanvas; AButtonInfo: TcxPCHeaderButtonViewInfo);
const
  ButtonStates: array[TcxPCNavigatorButtonState] of TdxSkinElementState = (esNormal, esPressed, esHot, esDisabled);
var
  AElement: TdxSkinElement;
  ARect: TRect;
begin
  AElement := HeaderButton;
  if AElement <> nil then
  begin
    ARect := AButtonInfo.Bounds;
    if AElement.IsAlphaUsed then
      DrawParentBackground(ACanvas, ARect);
    AElement.Draw(ACanvas.Handle, ARect, 0, ButtonStates[AButtonInfo.State]);
  end
  else
    inherited PaintHeaderButton(ACanvas, AButtonInfo);
end;

procedure TcxPCSkinPainter.PaintNativeTabBackground(DC: HDC; ATabVisibleIndex: Integer; const ABounds: TRect);
begin
  if Header <> nil then
    Header.Draw(DC, ABounds, 0, GetTabState(ViewInfo.TabsViewInfo[ATabVisibleIndex]))
  else
    inherited PaintNativeTabBackground(DC, ATabVisibleIndex, ABounds);
end;

procedure TcxPCSkinPainter.PaintTabsRegion(ACanvas: TcxCanvas);

  procedure PaintLine(ATabIndexInterval: TcxPCIndexInterval; AIsUpperLine, AIsLowerLine: Boolean);
  var
    I: Integer;
  begin
    for I := ATabIndexInterval.Left to ATabIndexInterval.Right do
      if I <> ViewInfo.MainTabVisibleIndex then
        PaintTab(ACanvas, I);
  end;

var
  J: Integer;
  ALineIndexBoundsA: TcxPCLineIndexBoundsArray;
  AFirstIndex, ALastIndex: Integer;
begin
  if IsSkinAvailable then
  begin
    ViewInfo.InitializeVisibleTabRange(AFirstIndex, ALastIndex);

    if (ViewInfo.MainTabVisibleIndex <> -1) and
      (AFirstIndex <= ViewInfo.MainTabVisibleIndex) and
      (ViewInfo.MainTabVisibleIndex <= ALastIndex) then
      PaintTab(ACanvas, ViewInfo.MainTabVisibleIndex);

    ViewInfo.InitializeLineBoundsArray(ALineIndexBoundsA);

    for J := ViewInfo.TopOrLeftPartRowCount - 1 downto 0 do
      PaintLine(ALineIndexBoundsA[J], J = 0, J = ViewInfo.TopOrLeftPartRowCount - 1);

    for J := ViewInfo.TopOrLeftPartRowCount to ViewInfo.RowCount - 1 do
      PaintLine(ALineIndexBoundsA[J], J = ViewInfo.RowCount - 1, J = ViewInfo.TopOrLeftPartRowCount);
  end
  else
    inherited PaintTabsRegion(ACanvas);
end;

procedure TcxPCSkinPainter.PrepareTabBitmapBackground(ABitmap: TcxBitmap;
  const ARect: TRect; ATabViewInfo: TcxTabViewInfo);
var
  R: TRect;
begin
  inherited PrepareTabBitmapBackground(ABitmap, ARect, ATabViewInfo);
  if (FrameContent <> nil) and NeedDrawTabBitmapBackground then
  begin
    R := GetPageFrameRect;
    OffsetRect(R, -ATabViewInfo.FullRect.Left, -ATabViewInfo.FullRect.Top);
    if ATabViewInfo.IsMainTab then
    begin
      case ATabViewInfo.PaintingPosition of
        tpBottom:
          OffsetRect(R, 0, 1);
        tpRight:
          OffsetRect(R, 1, 0);
      end;
    end;
    FrameContent.Draw(ABitmap.Canvas.Handle, R);
  end;
end;

function TcxPCSkinPainter.GetButtonHorz: TdxSkinElement;
var
  ASkinPainterInfo: TdxSkinInfo;
begin
  if GetSkinPainterData(ASkinPainterInfo) then
    Result := ASkinPainterInfo.PageControlButtonHorz
  else
    Result := nil;
end;

function TcxPCSkinPainter.GetButtonVert: TdxSkinElement;
var
  ASkinPainterInfo: TdxSkinInfo;
begin
  if GetSkinPainterData(ASkinPainterInfo) then
    Result := ASkinPainterInfo.PageControlButtonVert
  else
    Result := nil;
end;

function TcxPCSkinPainter.GetFrameContent: TdxSkinElement;
var
  ASkinPainterInfo: TdxSkinInfo;
begin
  Result := nil;
  if GetSkinPainterData(ASkinPainterInfo) then
    if ViewInfo.HideTabs then
    begin
      if ViewInfo.ShowFrame then
        Result := ASkinPainterInfo.GroupBoxElements[cxgpCenter]
      else
        Result := ASkinPainterInfo.GroupBoxClient;
    end
    else
      Result := ASkinPainterInfo.PageControlPane;
end;

function TcxPCSkinPainter.GetHeader: TdxSkinElement;
var
  ASkinPainterInfo: TdxSkinInfo;
begin
  Result := nil;
  if GetSkinPainterData(ASkinPainterInfo) then
    Result := ASkinPainterInfo.PageControlHeader;
end;

function TcxPCSkinPainter.GetHeaderButton: TdxSkinElement;
var
  ASkinPainterInfo: TdxSkinInfo;
begin
  Result := nil;
  if GetSkinPainterData(ASkinPainterInfo) then
    Result := ASkinPainterInfo.PageControlButton;
end;

function TcxPCSkinPainter.GetIndentByIndex(AType: TcxPCSkinIndents): Integer;
var
  ASkinPainterInfo: TdxSkinInfo;
begin
  Result := 0;
  if GetSkinPainterData(ASkinPainterInfo) then
    Result := ASkinPainterInfo.PageControlIndents[Integer(AType)];
end;

function TcxPCSkinPainter.GetNeedDrawTabBitmapBackground: Boolean;
begin
  Result := {(Indents[siDownGrow] > 1) and} (Header <> nil) and Header.IsAlphaUsed;
end;

function TcxPCSkinPainter.GetSkinPainterData(var AData: TdxSkinInfo): Boolean;
begin
  Result := (ViewInfo.GetLookAndFeel.SkinPainter <> nil) and ViewInfo.GetLookAndFeel.SkinPainter.GetPainterData(AData);
end;

function TcxPCSkinPainter.GetTabState(ATabViewInfo: TcxTabViewInfo): TdxSkinElementState;
begin
  if not ATabViewInfo.Enabled then
    Result := esDisabled
  else
    if ATabViewInfo.IsMainTab then
      Result := esPressed
    else
      if ATabViewInfo.IsHotTrack or ATabViewInfo.IsHighlighted then
        Result := esHot
      else
        Result := esNormal;
end;

initialization
  RegisterPCPainterClass(TcxPCSkinPainter);

finalization
  UnregisterPCPainterClass(TcxPCSkinPainter);

end.
