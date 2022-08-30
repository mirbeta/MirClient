{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressWizardControl                                     }
{                                                                    }
{           Copyright (c) 2012-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSWIZARDCONTROL AND ALL          }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY. }
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

unit dxWizardControlViewStyleAero;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, Windows, Classes, Graphics, cxControls, dxCore, dxCustomWizardControl,
  cxGraphics, cxDrawTextUtils, cxClasses, Controls, cxLookAndFeelPainters;

type
  { TdxWizardControlViewStyleAeroPainter }

  TdxWizardControlViewStyleAeroPainter = class(TdxWizardControlCustomPainter)
  protected
    function GetHeaderDescriptionTextColor: TColor; override;
    function GetHeaderTitleTextColor: TColor; override;
    function GetSeparatorSize: Integer; override;
    function GetTitleTextColor: TColor; override;
    procedure InitializeFonts; override;
  public
    procedure DrawBackground(ACanvas: TcxCanvas; const ABounds: TRect); override;
    procedure DrawCommandAreaBackground(ACanvas: TcxCanvas; const ABounds: TRect); override;
    procedure DrawSeparator(ACanvas: TcxCanvas; const R: TRect); override;
    procedure DrawTitleBackground(ACanvas: TcxCanvas; const R: TRect); override;
    // BackButton
    function GetBackButtonSize: TSize; virtual;
    procedure DrawBackButton(ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState); virtual;
  end;

  { TdxWizardControlViewStyleAeroBackButtonViewInfo }

  TdxWizardControlViewStyleAeroBackButtonViewInfo = class(TdxWizardControlButtonViewInfo)
  private
    function GetPainter: TdxWizardControlViewStyleAeroPainter;
  protected
    function CanDrawContent: Boolean; override;
    procedure DrawButtonBackground(ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState); override;
    procedure DrawContent(ACanvas: TcxCanvas); override;
  public
    function MeasureHeight: Integer; override;
    function MeasureWidth: Integer; override;
    //
    property Painter: TdxWizardControlViewStyleAeroPainter read GetPainter;
  end;

  { TdxWizardControlViewStyleAeroTitleTextOnGlassCache }

  TdxWizardControlViewStyleAeroTitleTextOnGlassCache = class
  private
    FCache: TcxBitmap32;
    FFontHeight: Integer;
    FFontName: TFontName;
    FFontStyle: TFontStyles;
    FText: string;
    FUseRightToLeftReading: Boolean;
  protected
    property Cache: TcxBitmap32 read FCache;
  public
    destructor Destroy; override;
    procedure Draw(DC: HDC; AFont: TFont; const ATextRect: TRect; const AText: string);
    procedure Flush;
  end;

  { TdxWizardControlViewStyleAeroTitleViewInfo }

  TdxWizardControlViewStyleAeroTitleViewInfo = class(TdxWizardControlCustomTitleViewInfo)
  private
    FCache: TdxWizardControlViewStyleAeroTitleTextOnGlassCache;
    FBackButtonViewInfo: TdxWizardControlViewStyleAeroBackButtonViewInfo;
    function GetGlyphAreaSize: TSize;
  protected
    FGlyphRect: TRect;
    FTextRect: TRect;
    procedure CalculateBackButton(var R: TRect); virtual;
    procedure CalculateGlyph(var R: TRect); virtual;
    procedure CalculateTextRect(var R: TRect); virtual;
    function CanDrawContent: Boolean; override;
    procedure DoRightToLeftConversion; override;
    procedure DrawContent(ACanvas: TcxCanvas); override;
    //
    property Cache: TdxWizardControlViewStyleAeroTitleTextOnGlassCache read FCache;
  public
    constructor Create(AWizardControl: TdxCustomWizardControl); override;
    destructor Destroy; override;
    procedure Calculate; override;
    function MeasureHeight: Integer; override;
    //
    property BackButtonViewInfo: TdxWizardControlViewStyleAeroBackButtonViewInfo read FBackButtonViewInfo;
    property GlyphAreaSize: TSize read GetGlyphAreaSize;
    property GlyphRect: TRect read FGlyphRect;
    property TextRect: TRect read FTextRect;
  end;

  { TdxWizardControlViewStyleAeroHeaderViewInfo }

  TdxWizardControlViewStyleAeroHeaderViewInfo = class(TdxWizardControlCustomHeaderViewInfo)
  protected
    function GetContentMargins: TRect; override;
    function GetDescriptionIsVisible: Boolean; override;
    function GetGlyphIsVisible: Boolean; override;
    function GetIndentBetweenElements: Integer; override;
  public
    function MeasureHeight: Integer; override;
  end;

  { TdxWizardControlViewStyleAeroNavigationBarViewInfo }

  TdxWizardControlViewStyleAeroCommandAreaViewInfo = class(TdxWizardControlCustomCommandAreaViewInfo)
  protected
    procedure CalculateButtons(var ABounds: TRect); override;
    function GetContentMargins: TRect; override;
    function GetIndentBetweenElementsInGroup: Integer; override;
    function GetIndentBetweenGroups: Integer; override;
  end;

  { TdxWizardControlViewStyleAeroWatermarkViewInfo }

  TdxWizardControlViewStyleAeroWatermarkViewInfo = class(TdxWizardControlWatermarkViewInfo)
  protected
    function GetIsImageVisible: Boolean; override;
  end;

  { TdxWizardControlViewStyleAeroViewInfo }

  TdxWizardControlViewStyleAeroViewInfo = class(TdxWizardControlViewInfo)
  private
    function GetTitleViewInfo: TdxWizardControlViewStyleAeroTitleViewInfo;
  protected
    function CreateCommandAreaViewInfo: TdxWizardControlCustomCommandAreaViewInfo; override;
    function CreateHeaderViewInfo: TdxWizardControlCustomHeaderViewInfo; override;
    function CreatePainter: TdxWizardControlCustomPainter; override;
    function CreateTitleViewInfo: TdxWizardControlCustomTitleViewInfo; override;
    function CreateWatermarkViewInfo: TdxWizardControlWatermarkViewInfo; override;
    function GetDefaultResizeAnimation: TdxWizardControlResizeAnimation; override;
    function GetDefaultTransitionEffect: TdxWizardControlTransitionEffect; override;
    function GetTransitionEffectAreaBounds: TRect; override;
  public
    procedure Calculate; override;
    procedure GetTabOrderList(AList: TdxWizardControlCellViewInfoList); override;
    //
    property TitleViewInfo: TdxWizardControlViewStyleAeroTitleViewInfo read GetTitleViewInfo;
  end;

implementation

uses
  cxGeometry, Math, dxGDIPlusClasses, cxDWMApi, SysUtils, dxUxTheme, dxDPIAwareUtils;

const
  dxViewStyleAeroGlowingSize = 10;
  dxViewStyleAeroIndentBetweenElementsInGroup = 7;
  dxViewStyleAeroIndentBetweenGroups = 19;
  dxViewStyleAeroTitleHeight = 30;

{ TdxWizardControlViewStyleAeroPainter }

procedure TdxWizardControlViewStyleAeroPainter.DrawBackButton(
  ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState);
begin
  LookAndFeelPainter.DrawScaledBackButton(ACanvas, R, AState, ScaleFactor);
end;

procedure TdxWizardControlViewStyleAeroPainter.DrawBackground(ACanvas: TcxCanvas; const ABounds: TRect);
begin
  ACanvas.FillRect(ABounds, cxGetActualColor(
    LookAndFeelPainter.DefaultEditorBackgroundColor(False), clWindow));
end;

procedure TdxWizardControlViewStyleAeroPainter.DrawCommandAreaBackground(ACanvas: TcxCanvas; const ABounds: TRect);
begin
  LookAndFeelPainter.DrawWindowContent(ACanvas, ABounds);
  DrawSeparator(ACanvas, cxRectSetHeight(ABounds, SeparatorSize));
end;

procedure TdxWizardControlViewStyleAeroPainter.DrawSeparator(ACanvas: TcxCanvas; const R: TRect);
begin
  if IsSkinUsed then
    LookAndFeelPainter.DrawLabelLine(ACanvas, R, clBtnFace, clWhite, False)
  else
    ACanvas.FillRect(R, cl3DLight);
end;

procedure TdxWizardControlViewStyleAeroPainter.DrawTitleBackground(ACanvas: TcxCanvas; const R: TRect);
begin
  LookAndFeelPainter.DrawWindowContent(ACanvas, R);
  DrawSeparator(ACanvas, cxRectSetBottom(R, R.Bottom, SeparatorSize))
end;

procedure TdxWizardControlViewStyleAeroPainter.InitializeFonts;
begin
  inherited InitializeFonts;
  HeaderTitleFont.Size := ScaleFactor.Apply(12, HeaderTitleFont.PixelsPerInch, dxDefaultDPI);
  if IsWinVistaOrLater then
    HeaderTitleFont.Name := 'Segoe UI';
end;

function TdxWizardControlViewStyleAeroPainter.GetBackButtonSize: TSize;
begin
  Result := LookAndFeelPainter.GetScaledBackButtonSize(ScaleFactor);
end;

function TdxWizardControlViewStyleAeroPainter.GetHeaderDescriptionTextColor: TColor;
begin
  if IsSkinUsed then
    Result := inherited GetHeaderDescriptionTextColor
  else
    Result := $404040;
end;

function TdxWizardControlViewStyleAeroPainter.GetHeaderTitleTextColor: TColor;
begin
  if IsSkinUsed then
    Result := inherited GetHeaderTitleTextColor
  else
    Result := $AB7013;
end;

function TdxWizardControlViewStyleAeroPainter.GetSeparatorSize: Integer;
begin
  if IsSkinUsed then
    Result := inherited GetSeparatorSize
  else
    Result := 1;
end;

function TdxWizardControlViewStyleAeroPainter.GetTitleTextColor: TColor;
begin
  Result := LookAndFeelPainter.GetWindowContentTextColor;
end;

{ TdxWizardControlViewStyleAeroWatermarkViewInfo }

function TdxWizardControlViewStyleAeroWatermarkViewInfo.GetIsImageVisible: Boolean;
begin
  Result := WatermarkVisibility = wcevAlwaysVisible
end;

{ TdxWizardControlViewStyleAeroViewInfo }

procedure TdxWizardControlViewStyleAeroViewInfo.Calculate;
begin
  inherited Calculate;
  FPageArea := cxRectContent(PageArea, ScaleFactor.Apply(Rect(28, 0, 19, 19)));
end;

function TdxWizardControlViewStyleAeroViewInfo.CreateCommandAreaViewInfo: TdxWizardControlCustomCommandAreaViewInfo;
begin
  Result := TdxWizardControlViewStyleAeroCommandAreaViewInfo.Create(WizardControl);
end;

function TdxWizardControlViewStyleAeroViewInfo.CreateHeaderViewInfo: TdxWizardControlCustomHeaderViewInfo;
begin
  Result := TdxWizardControlViewStyleAeroHeaderViewInfo.Create(WizardControl);
end;

function TdxWizardControlViewStyleAeroViewInfo.CreatePainter: TdxWizardControlCustomPainter;
begin
  Result := TdxWizardControlViewStyleAeroPainter.Create(WizardControl);
end;

function TdxWizardControlViewStyleAeroViewInfo.CreateTitleViewInfo: TdxWizardControlCustomTitleViewInfo;
begin
  Result := TdxWizardControlViewStyleAeroTitleViewInfo.Create(WizardControl);
end;

function TdxWizardControlViewStyleAeroViewInfo.CreateWatermarkViewInfo: TdxWizardControlWatermarkViewInfo;
begin
  Result := TdxWizardControlViewStyleAeroWatermarkViewInfo.Create(WizardControl);
end;

function TdxWizardControlViewStyleAeroViewInfo.GetDefaultResizeAnimation: TdxWizardControlResizeAnimation;
begin
  Result := wcraEnabled;
end;

function TdxWizardControlViewStyleAeroViewInfo.GetDefaultTransitionEffect: TdxWizardControlTransitionEffect;
begin
  Result := wcteFade;
end;

procedure TdxWizardControlViewStyleAeroViewInfo.GetTabOrderList(AList: TdxWizardControlCellViewInfoList);
var
  ACommandArea: TdxWizardControlViewStyleAeroCommandAreaViewInfo;

  procedure AddCustomButtons;
  var
    I: Integer;
  begin
    for I := ACommandArea.CustomButtonViewInfosCount - 1 downto 0 do
      AList.Add(ACommandArea.CustomButtonViewInfos[I]);
  end;

begin
  ACommandArea := TdxWizardControlViewStyleAeroCommandAreaViewInfo(CommandAreaViewInfo);
  if ACommandArea.HasCustomButtonsAlignedLeft then
    AddCustomButtons;
  AList.Add(ACommandArea.NextButtonViewInfo);
  AList.Add(ACommandArea.FinishButtonViewInfo);
  AList.Add(ACommandArea.CancelButtonViewInfo);
  AList.Add(ACommandArea.HelpButtonViewInfo);
  if ACommandArea.HasCustomButtonsAlignedRight then
    AddCustomButtons;
  AList.Add(TitleViewInfo.BackButtonViewInfo);
end;

function TdxWizardControlViewStyleAeroViewInfo.GetTitleViewInfo: TdxWizardControlViewStyleAeroTitleViewInfo;
begin
  Result := inherited TitleViewInfo as TdxWizardControlViewStyleAeroTitleViewInfo;
end;

function TdxWizardControlViewStyleAeroViewInfo.GetTransitionEffectAreaBounds: TRect;
begin
  Result := Bounds;
  Result.Top := TitleViewInfo.Bounds.Bottom;
  Result.Bottom := CommandAreaViewInfo.Bounds.Top;
end;

{ TdxWizardControlViewStyleAeroBackButtonViewInfo }

function TdxWizardControlViewStyleAeroBackButtonViewInfo.CanDrawContent: Boolean;
begin
  Result := True;
end;

procedure TdxWizardControlViewStyleAeroBackButtonViewInfo.DrawButtonBackground(
  ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState);
var
  ARect: TRect;
  AUseRightToLeftAlignment: Boolean;
begin
  ARect := R;
  AUseRightToLeftAlignment := WizardControl.UseRightToLeftAlignment;
  cxRightToLeftDependentDraw(ACanvas.Handle, ARect, AUseRightToLeftAlignment,
    procedure
    begin
      Painter.DrawBackButton(ACanvas, ARect, AState);
    end);
end;

procedure TdxWizardControlViewStyleAeroBackButtonViewInfo.DrawContent(ACanvas: TcxCanvas);
begin
  if not FadingHelper.DrawImage(ACanvas.Handle, Bounds) then
    DrawButtonBackground(ACanvas, Bounds, State);
end;

function TdxWizardControlViewStyleAeroBackButtonViewInfo.MeasureHeight: Integer;
begin
  Result := Painter.GetBackButtonSize.cy;
end;

function TdxWizardControlViewStyleAeroBackButtonViewInfo.MeasureWidth: Integer;
begin
  Result := Painter.GetBackButtonSize.cx;
end;

function TdxWizardControlViewStyleAeroBackButtonViewInfo.GetPainter: TdxWizardControlViewStyleAeroPainter;
begin
  Result := TdxWizardControlViewStyleAeroPainter(inherited Painter);
end;

{ TdxWizardControlViewStyleAeroCommandAreaViewInfo }

procedure TdxWizardControlViewStyleAeroCommandAreaViewInfo.CalculateButtons(var ABounds: TRect);
begin
  inherited CalculateButtons(ABounds);
  if HasCustomButtonsAlignedRight then
    CalculateCustomButtons(ABounds);
  if AlignButtonToRightSide(HelpButtonViewInfo, ABounds) then
    Dec(ABounds.Right, IndentBetweenGroups);
  if AlignButtonToRightSide(CancelButtonViewInfo, ABounds) then
    Dec(ABounds.Right, IndentBetweenElementsInGroup);
  if Buttons.Finish.AlwaysVisible then
  begin
    if AlignButtonToRightSide(FinishButtonViewInfo, ABounds) then
      Dec(ABounds.Right, IndentBetweenElementsInGroup);
    if AlignButtonToRightSide(NextButtonViewInfo, ABounds) then
      Dec(ABounds.Right, IndentBetweenGroups);
  end
  else
    if AlignButtonToRightSide(ActualNextButtonViewInfo, ABounds) then
      Dec(ABounds.Right, IndentBetweenGroups);
  if HasCustomButtonsAlignedLeft then
    CalculateCustomButtons(ABounds);
end;

function TdxWizardControlViewStyleAeroCommandAreaViewInfo.GetContentMargins: TRect;
begin
  Result := ScaleFactor.Apply(Rect(28, 14, 19, 14));
  Inc(Result.Top, Painter.SeparatorSize);
end;

function TdxWizardControlViewStyleAeroCommandAreaViewInfo.GetIndentBetweenElementsInGroup: Integer;
begin
  Result := ScaleFactor.Apply(dxViewStyleAeroIndentBetweenElementsInGroup);
end;

function TdxWizardControlViewStyleAeroCommandAreaViewInfo.GetIndentBetweenGroups: Integer;
begin
  Result := ScaleFactor.Apply(dxViewStyleAeroIndentBetweenGroups);
end;

{ TdxWizardControlViewStyleAeroHeaderViewInfo }

function TdxWizardControlViewStyleAeroHeaderViewInfo.MeasureHeight: Integer;
begin
  if GetIsVisible then
    Result := inherited MeasureHeight
  else
    Result := ContentMargins.Bottom;
end;

function TdxWizardControlViewStyleAeroHeaderViewInfo.GetContentMargins: TRect;
begin
  Result := ScaleFactor.Apply(Rect(28, 19, 19, 19));
end;

function TdxWizardControlViewStyleAeroHeaderViewInfo.GetDescriptionIsVisible: Boolean;
var
  ASettings: TdxWizardControlHeader;
begin
  if GetActualSettings(wchvDescriptionVisibility, ASettings) then
    Result := ASettings.DescriptionVisibility = wcevAlwaysVisible
  else
    Result := False;
end;

function TdxWizardControlViewStyleAeroHeaderViewInfo.GetGlyphIsVisible: Boolean;
var
  ASettings: TdxWizardControlHeader;
begin
  if GetActualSettings(wchvGlyphVisibility, ASettings) then
    Result := ASettings.GlyphVisibility = wcevAlwaysVisible
  else
    Result := False;
end;

function TdxWizardControlViewStyleAeroHeaderViewInfo.GetIndentBetweenElements: Integer;
begin
  Result := ScaleFactor.Apply(dxViewStyleAeroIndentBetweenElementsInGroup);
end;

{ TdxWizardControlViewStyleAeroTitleViewInfo }

constructor TdxWizardControlViewStyleAeroTitleViewInfo.Create(AWizardControl: TdxCustomWizardControl);
begin
  inherited Create(AWizardControl);
  FCache := TdxWizardControlViewStyleAeroTitleTextOnGlassCache.Create;
  FBackButtonViewInfo := TdxWizardControlViewStyleAeroBackButtonViewInfo.Create(
    WizardControl, WizardControl.Buttons.Back);
  CellList.Add(BackButtonViewInfo);
end;

procedure TdxWizardControlViewStyleAeroTitleViewInfo.Calculate;
var
  R: TRect;
begin
  R := Bounds;
  CalculateBackButton(R);
  CalculateGlyph(R);
  CalculateTextRect(R);
end;

procedure TdxWizardControlViewStyleAeroTitleViewInfo.CalculateBackButton(var R: TRect);
var
  ABackButtonBounds: TRect;
begin
  ABackButtonBounds := cxRectCenterVertically(R, BackButtonViewInfo.MeasureHeight);
  ABackButtonBounds := cxRectSetWidth(ABackButtonBounds, BackButtonViewInfo.MeasureWidth);
  if not IsPaintOnGlass then
    OffsetRect(ABackButtonBounds, ABackButtonBounds.Top - R.Top, 0);
  BackButtonViewInfo.Bounds := ABackButtonBounds;
  R.Left := BackButtonViewInfo.Bounds.Right + ScaleFactor.Apply(dxViewStyleAeroIndentBetweenElementsInGroup);
end;

procedure TdxWizardControlViewStyleAeroTitleViewInfo.CalculateGlyph(var R: TRect);
begin
  if not Title.Glyph.Empty then
  begin
    FGlyphRect := cxRectCenterVertically(R, GlyphAreaSize.cy);
    FGlyphRect := cxRectSetWidth(GlyphRect, GlyphAreaSize.cx);
    R.Left := GlyphRect.Right + ScaleFactor.Apply(dxViewStyleAeroIndentBetweenElementsInGroup);
  end
  else
    FGlyphRect := cxNullRect;
end;

procedure TdxWizardControlViewStyleAeroTitleViewInfo.CalculateTextRect(var R: TRect);
begin
  if IsPaintOnGlass then
  begin
    cxScreenCanvas.Font := Font;
    FTextRect := cxRectSetWidth(R, cxScreenCanvas.TextWidth(Title.Text) +
      2 * ScaleFactor.Apply(dxViewStyleAeroGlowingSize));
    cxScreenCanvas.Dormant;
  end
  else
    FTextRect := R;
end;

function TdxWizardControlViewStyleAeroTitleViewInfo.CanDrawContent: Boolean;
begin
  Result := True;
end;

destructor TdxWizardControlViewStyleAeroTitleViewInfo.Destroy;
begin
  FreeAndNil(FCache);
  inherited Destroy;
end;

procedure TdxWizardControlViewStyleAeroTitleViewInfo.DoRightToLeftConversion;
begin
  inherited DoRightToLeftConversion;
  if BackButtonViewInfo <> nil then
    BackButtonViewInfo.DoRightToLeftConversion;
  FGlyphRect := GetRightToLeftConvertedRect(GlyphRect);
  FTextRect := GetRightToLeftConvertedRect(TextRect);
end;

procedure TdxWizardControlViewStyleAeroTitleViewInfo.DrawContent(ACanvas: TcxCanvas);
var
  AFormat: Cardinal;
begin
  if not cxRectIsEmpty(GlyphRect) then
    Title.Glyph.StretchDraw(ACanvas.Handle, GlyphRect);

  if not cxRectIsEmpty(TextRect) and (Title.Text <> '') then
  begin
    if IsPaintOnGlass then
    begin
      Cache.FUseRightToLeftReading := WizardControl.UseRightToLeftReading;
      Cache.Draw(ACanvas.Handle, Font, TextRect, Title.Text)
    end
    else
    begin
      ACanvas.SaveState;
      try
        ACanvas.Font := Font;
        ACanvas.Font.Color := cxGetActualColor(Painter.TitleTextColor, clWindowText);
        AFormat := DT_VCENTER or DT_END_ELLIPSIS or DT_SINGLELINE;
        if WizardControl.UseRightToLeftAlignment then
          AFormat := AFormat or DT_RIGHT
        else
          AFormat := AFormat or DT_LEFT;
        if WizardControl.UseRightToLeftReading then
          AFormat := AFormat or DT_RTLREADING;
        cxDrawText(ACanvas, Title.Text, TextRect, AFormat);
      finally
        ACanvas.RestoreState;
      end;
    end;
  end;
end;

function TdxWizardControlViewStyleAeroTitleViewInfo.MeasureHeight: Integer;
begin
  Result := Max(cxTextHeight(Font), BackButtonViewInfo.MeasureHeight);
  Result := Max(Result, GlyphAreaSize.cy);
  Result := Max(Result + 2 * ScaleFactor.Apply(cxTextSpace), ScaleFactor.Apply(dxViewStyleAeroTitleHeight));
  if IsPaintOnGlass then
    Inc(Result, ScaleFactor.Apply(2))
  else
    Inc(Result, Painter.SeparatorSize);
end;

function TdxWizardControlViewStyleAeroTitleViewInfo.GetGlyphAreaSize: TSize;
begin
  Result := cxSize(dxGetSystemMetrics(SM_CXSMICON, ScaleFactor), dxGetSystemMetrics(SM_CYSMICON, ScaleFactor));
end;

{ TdxWizardControlViewStyleAeroTitleTextOnGlassCache }

destructor TdxWizardControlViewStyleAeroTitleTextOnGlassCache.Destroy;
begin
  FreeAndNil(FCache);
  inherited Destroy;
end;

procedure TdxWizardControlViewStyleAeroTitleTextOnGlassCache.Draw(
  DC: HDC; AFont: TFont; const ATextRect: TRect; const AText: string);

  function CheckFont(AFont: TFont): Boolean;
  begin
    Result := (AFont.Name = FFontName) and
      (AFont.Style = FFontStyle) and (AFont.Height = FFontHeight);
  end;

  function CheckSize(const R: TRect): Boolean;
  begin
    Result := cxSizeIsEqual(cxSize(R), cxSize(Cache.Width, Cache.Height));
  end;

var
  AFlags: Integer;
begin
  if (FCache = nil) or not (CheckFont(AFont) and CheckSize(ATextRect)) or (FText <> AText) then
  begin
    Flush;
    FText := AText;
    FFontHeight := AFont.Height;
    FFontStyle := AFont.Style;
    FFontName := AFont.Name;

    FCache := TcxBitmap32.CreateSize(ATextRect, True);
    FCache.Canvas.Lock;
    try
      AFlags := DT_CENTER or DT_SINGLELINE or DT_VCENTER;
      if FUseRightToLeftReading then
        AFlags := AFlags or DT_RTLREADING;
      dxDrawTextOnGlass(Cache.Canvas.Handle, AText, AFont, Cache.ClientRect, clWindowText, AFlags,
        dxViewStyleAeroGlowingSize, True);
    finally
      Cache.Canvas.Unlock;
    end;
  end;
  cxBitBlt(DC, Cache.Canvas.Handle, ATextRect, cxNullPoint, SRCCOPY);
end;

procedure TdxWizardControlViewStyleAeroTitleTextOnGlassCache.Flush;
begin
  FreeAndNil(FCache);
end;

end.
