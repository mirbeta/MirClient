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

unit dxWizardControlViewStyleWizard97;

{$I cxVer.inc}

interface

uses
  Types, Windows, Classes, Graphics, cxControls, dxCore, dxCustomWizardControl,
  cxGraphics, cxDrawTextUtils, cxClasses;

type
  { TdxWizardControlViewStyleWizard97Painter }

  TdxWizardControlViewStyleWizard97Painter = class(TdxWizardControlCustomPainter)
  protected
    procedure InitializeFonts; override;
  public
    procedure DrawHeaderBackground(ACanvas: TcxCanvas; const ABounds: TRect); override;
  end;

  { TdxWizardControlViewStyleWizard97HeaderViewInfo }

  TdxWizardControlViewStyleWizard97HeaderViewInfo = class(TdxWizardControlCustomHeaderViewInfo)
  protected
    function GetContentMargins: TRect; override;
    function GetDescriptionIsVisible: Boolean; override;
    function GetGlyphIsVisible: Boolean; override;
    function GetIndentBetweenElements: Integer; override;
  public
    function MeasureHeight: Integer; override;
  end;

  { TdxWizardControlViewStyleWizard97CommandAreaViewInfo }

  TdxWizardControlViewStyleWizard97CommandAreaViewInfo = class(TdxWizardControlCustomCommandAreaViewInfo)
  private
    FBackButtonViewInfo: TdxWizardControlButtonViewInfo;
  protected
    procedure CalculateButtons(var ABounds: TRect); override;
    procedure CalculateButtonsMaxMeasureHeight; override;
    procedure DoRightToLeftConversion; override;
    function GetContentMargins: TRect; override;
    function GetIndentBetweenElementsInGroup: Integer; override;
    function GetIndentBetweenGroups: Integer; override;
    //
    property BackButtonViewInfo: TdxWizardControlButtonViewInfo read FBackButtonViewInfo;
  public
    constructor Create(AWizardControl: TdxCustomWizardControl); override;
    function MeasureWidth: Integer; override;
  end;

  { TdxWizardControlViewStyleWizard97WatermarkViewInfo }

  TdxWizardControlViewStyleWizard97WatermarkViewInfo = class(TdxWizardControlWatermarkViewInfo)
  protected
    function GetIsImageVisible: Boolean; override;
  end;

  { TdxWizardControlViewStyleWizard97ViewInfo }

  TdxWizardControlViewStyleWizard97ViewInfo = class(TdxWizardControlViewInfo)
  protected
    function CreateCommandAreaViewInfo: TdxWizardControlCustomCommandAreaViewInfo; override;
    function CreateHeaderViewInfo: TdxWizardControlCustomHeaderViewInfo; override;
    function CreatePainter: TdxWizardControlCustomPainter; override;
    function CreateWatermarkViewInfo: TdxWizardControlWatermarkViewInfo; override;
    function GetDefaultResizeAnimation: TdxWizardControlResizeAnimation; override;
    function GetDefaultTransitionEffect: TdxWizardControlTransitionEffect; override;
  public
    procedure Calculate; override;
    procedure GetTabOrderList(AList: TdxWizardControlCellViewInfoList); override;
  end;

implementation

uses
  cxGeometry, Math, dxGDIPlusClasses;

const
  dxWizardViewStyleWizard97IndentBetweenElementsInGroup = 5;
  dxWizardViewStyleWizard97IndentBetweenGroups = 11;
  dxWizardViewStyleWizard97OffsetFromBorders: TRect = (Left: 11; Top: 11; Right: 11; Bottom: 11);

type
  TdxWizardControlCellViewInfoAccess = class(TdxWizardControlCellViewInfo);

{ TdxWizardControlViewStyleWizard97ViewInfo }

procedure TdxWizardControlViewStyleWizard97ViewInfo.Calculate;
begin
  inherited Calculate;
  FPageArea := cxRectContent(PageArea, ScaleFactor.Apply(dxWizardViewStyleWizard97OffsetFromBorders));
end;

function TdxWizardControlViewStyleWizard97ViewInfo.CreateCommandAreaViewInfo: TdxWizardControlCustomCommandAreaViewInfo;
begin
  Result := TdxWizardControlViewStyleWizard97CommandAreaViewInfo.Create(WizardControl);
end;

function TdxWizardControlViewStyleWizard97ViewInfo.CreateHeaderViewInfo: TdxWizardControlCustomHeaderViewInfo;
begin
  Result := TdxWizardControlViewStyleWizard97HeaderViewInfo.Create(WizardControl);
end;

function TdxWizardControlViewStyleWizard97ViewInfo.CreatePainter: TdxWizardControlCustomPainter;
begin
  Result := TdxWizardControlViewStyleWizard97Painter.Create(WizardControl);
end;

function TdxWizardControlViewStyleWizard97ViewInfo.CreateWatermarkViewInfo: TdxWizardControlWatermarkViewInfo;
begin
  Result := TdxWizardControlViewStyleWizard97WatermarkViewInfo.Create(WizardControl);
end;

function TdxWizardControlViewStyleWizard97ViewInfo.GetDefaultResizeAnimation: TdxWizardControlResizeAnimation;
begin
  Result := wcraDisabled;
end;

function TdxWizardControlViewStyleWizard97ViewInfo.GetDefaultTransitionEffect: TdxWizardControlTransitionEffect;
begin
  Result := wcteNone;
end;

procedure TdxWizardControlViewStyleWizard97ViewInfo.GetTabOrderList(AList: TdxWizardControlCellViewInfoList);
var
  ACommandArea: TdxWizardControlViewStyleWizard97CommandAreaViewInfo;

  procedure AddCustomButtons;
  var
    I: Integer;
  begin
    for I := ACommandArea.CustomButtonViewInfosCount - 1 downto 0 do
      AList.Add(ACommandArea.CustomButtonViewInfos[I]);
  end;

begin
  ACommandArea := TdxWizardControlViewStyleWizard97CommandAreaViewInfo(CommandAreaViewInfo);
  if ACommandArea.HasCustomButtonsAlignedLeft then
    AddCustomButtons;
  AList.Add(ACommandArea.BackButtonViewInfo);
  AList.Add(ACommandArea.NextButtonViewInfo);
  AList.Add(ACommandArea.FinishButtonViewInfo);
  AList.Add(ACommandArea.CancelButtonViewInfo);
  AList.Add(ACommandArea.HelpButtonViewInfo);
  if ACommandArea.HasCustomButtonsAlignedRight then
    AddCustomButtons;
end;

{ TdxWizardControlViewStyleWizard97Painter }

procedure TdxWizardControlViewStyleWizard97Painter.DrawHeaderBackground(ACanvas: TcxCanvas; const ABounds: TRect);
begin
  ACanvas.FillRect(ABounds, cxGetActualColor(
    LookAndFeelPainter.DefaultEditorBackgroundColor(False), clWindow));
  DrawSeparator(ACanvas, cxRectSetBottom(ABounds, ABounds.Bottom, SeparatorSize));
end;

procedure TdxWizardControlViewStyleWizard97Painter.InitializeFonts;
begin
  inherited InitializeFonts;
  HeaderTitleFont.Style := [fsBold];
end;

{ TdxWizardControlViewStyleWizard97CommandAreaViewInfo }

constructor TdxWizardControlViewStyleWizard97CommandAreaViewInfo.Create(AWizardControl: TdxCustomWizardControl);
begin
  inherited Create(AWizardControl);
  FBackButtonViewInfo := AddButtonCell(Buttons.Back, TdxWizardControlButtonViewInfo);
end;

procedure TdxWizardControlViewStyleWizard97CommandAreaViewInfo.CalculateButtons(var ABounds: TRect);
begin
  inherited CalculateButtons(ABounds);
  if HasCustomButtonsAlignedRight then
    CalculateCustomButtons(ABounds);
  if AlignButtonToRightSide(HelpButtonViewInfo, ABounds) then
    Dec(ABounds.Right, IndentBetweenGroups);
  if AlignButtonToRightSide(CancelButtonViewInfo, ABounds) then
    Dec(ABounds.Right, IndentBetweenGroups);
  if Buttons.Finish.AlwaysVisible then
  begin
    if AlignButtonToRightSide(FinishButtonViewInfo, ABounds) then
      Dec(ABounds.Right, IndentBetweenElementsInGroup);
    if AlignButtonToRightSide(NextButtonViewInfo, ABounds) then
      Dec(ABounds.Right, IndentBetweenElementsInGroup);
  end
  else
    if AlignButtonToRightSide(ActualNextButtonViewInfo, ABounds) then
      Dec(ABounds.Right, IndentBetweenElementsInGroup);
  if AlignButtonToRightSide(BackButtonViewInfo, ABounds) then
    Dec(ABounds.Right, IndentBetweenGroups);
  if HasCustomButtonsAlignedLeft then
    CalculateCustomButtons(ABounds);
end;

procedure TdxWizardControlViewStyleWizard97CommandAreaViewInfo.CalculateButtonsMaxMeasureHeight;
begin
  inherited CalculateButtonsMaxMeasureHeight;
  if BackButtonViewInfo <> nil then
    FButtonsMaxMeasureHeight := Max(FButtonsMaxMeasureHeight, BackButtonViewInfo.MeasureHeight);
end;

procedure TdxWizardControlViewStyleWizard97CommandAreaViewInfo.DoRightToLeftConversion;
begin
  inherited DoRightToLeftConversion;
  if BackButtonViewInfo <> nil then
    TdxWizardControlCellViewInfoAccess(BackButtonViewInfo).DoRightToLeftConversion;
end;

function TdxWizardControlViewStyleWizard97CommandAreaViewInfo.GetContentMargins: TRect;
begin
  Result := ScaleFactor.Apply(dxWizardViewStyleWizard97OffsetFromBorders);
  Inc(Result.Top, Painter.SeparatorSize);
end;

function TdxWizardControlViewStyleWizard97CommandAreaViewInfo.GetIndentBetweenElementsInGroup: Integer;
begin
  Result := ScaleFactor.Apply(dxWizardViewStyleWizard97IndentBetweenElementsInGroup);
end;

function TdxWizardControlViewStyleWizard97CommandAreaViewInfo.GetIndentBetweenGroups: Integer;
begin
  Result := ScaleFactor.Apply(dxWizardViewStyleWizard97IndentBetweenGroups);
end;

function TdxWizardControlViewStyleWizard97CommandAreaViewInfo.MeasureWidth: Integer;
begin
  Result := inherited MeasureWidth + MeasureButtonWidth(BackButtonViewInfo, IndentBetweenElementsInGroup);
end;

{ TdxWizardControlViewStyleWizard97WatermarkViewInfo }

function TdxWizardControlViewStyleWizard97WatermarkViewInfo.GetIsImageVisible: Boolean;
begin
  Result := WatermarkVisibility <> wcevAlwaysHidden;
end;

{ TdxWizardControlViewStyleWizard97HeaderViewInfo }

function TdxWizardControlViewStyleWizard97HeaderViewInfo.MeasureHeight: Integer;
begin
  if GetIsVisible then
    Result := inherited MeasureHeight
  else
    Result := 0;
end;

function TdxWizardControlViewStyleWizard97HeaderViewInfo.GetContentMargins: TRect;
begin
  Result := ScaleFactor.Apply(dxWizardViewStyleWizard97OffsetFromBorders);
  Inc(Result.Bottom, Painter.SeparatorSize);
end;

function TdxWizardControlViewStyleWizard97HeaderViewInfo.GetDescriptionIsVisible: Boolean;
var
  ASettings: TdxWizardControlHeader;
begin
  if GetActualSettings(wchvDescriptionVisibility, ASettings) then
    Result := ASettings.DescriptionVisibility <> wcevAlwaysHidden
  else
    Result := True;
end;

function TdxWizardControlViewStyleWizard97HeaderViewInfo.GetGlyphIsVisible: Boolean;
var
  ASettings: TdxWizardControlHeader;
begin
  if GetActualSettings(wchvGlyphVisibility, ASettings) then
    Result := ASettings.GlyphVisibility <> wcevAlwaysHidden
  else
    Result := True;
end;

function TdxWizardControlViewStyleWizard97HeaderViewInfo.GetIndentBetweenElements: Integer;
begin
  Result := ScaleFactor.Apply(dxWizardViewStyleWizard97IndentBetweenElementsInGroup);
end;

end.
