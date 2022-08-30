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

unit dxNavBarBaseViews;

{$I cxVer.inc}

interface

uses
  Windows, Classes, Types, Graphics, ImgList, dxTypeHelpers,
  dxNavBar, dxNavBarBase, dxNavBarCollns, dxNavBarStyles, dxNavBarCustomPainters;

type
  TdxNavBarBaseViewInfo = class(TdxNavBarViewInfo)
  private
    function GetRootGroup(AGroup: TdxNavBarGroup): TdxNavBarGroup;
  protected
    procedure CheckControlWindowRegion(AGroup: TdxNavBarGroupViewInfo); override;
    procedure CorrectBounds; override;
    procedure CorrectTopVisibleIndex(AOffsetY: Integer);
    function GetActiveGroupMinHeight: Integer; virtual;
  public
    destructor Destroy; override;
    procedure DoGroupActivate(AGroup: TdxNavBarGroup); override;
    procedure DoGroupDeactivate(AGroup: TdxNavBarGroup); override;
    function IsGroupActive(AGroup: TdxNavBarGroup): Boolean; override;
    procedure MakeLinkVisible(ALink: TdxNavBarItemLink; ATop: Boolean = True); override;
    procedure MakeGroupVisible(AGroup: TdxNavBarGroup;
      AExpandGroup: Boolean = True; ATop: Boolean = True); override;
  end;

  { TdxNavBarNavigationBarController }

  TdxNavBarNavigationBarController = class(TdxNavBarController)
  end;

  TdxNavBarBasePainter = class(TdxNavBarElementPainter)
  protected
    class function GetViewInfoClass: TdxNavBarViewInfoClass; override;
    function GetControllerClass: TdxNavBarControllerClass; override;

    class function ButtonPainterClass: TdxNavBarCustomButtonPainterClass; override;
    class function ScrollButtonsPainterClass: TdxNavBarCustomScrollButtonsPainterClass; override;
    class function SelectionPainterClass: TdxNavBarCustomSelectionPainterClass; override;

    function NeedActiveGroupScrollBar: Boolean; override;
  public
    class function GetCategories: TdxNavBarViewCategories; override;
  end;

  TdxNavBarBaseSelectionPainter = class(TdxNavBarCustomSelectionPainter)
  protected
    class procedure GetColors(AState: TdxNavBarObjectStates; ABackColor: TColor; out AFillColor,
      ATopLeftOuterColor, ABottomRightOuterColor, ATopLeftInnerColor, ABottomRightInnerColor: TColor); override;
  end;

  TdxNavBarBaseButtonPainter = class(TdxNavBarCustomButtonPainter)
  protected
    class procedure InternalDrawButton(ACanvas: TCanvas; ARect: TRect; APicture: TPicture;
      AColor1, AColor2: TColor; AAlphaBlend1, AAlphaBlend2: Byte;
      AGradientMode: TdxBarStyleGradientMode; ABorderColor: TColor;
      AState: TdxNavBarObjectStates); override;
  end;

  TdxNavBarBaseScrollButtonsPainter = class(TdxNavBarCustomScrollButtonsPainter)
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

  TdxNavBarFlatPainter = class(TdxNavBarBasePainter)
  protected
    class function ButtonPainterClass: TdxNavBarCustomButtonPainterClass; override;
  end;

  TdxNavBarFlatButtonPainter = class(TdxNavBarCustomButtonPainter)
  protected
    class procedure InternalDrawButton(ACanvas: TCanvas; ARect: TRect; APicture: TPicture;
      AColor1, AColor2: TColor; AAlphaBlend1, AAlphaBlend2: Byte;
      AGradientMode: TdxBarStyleGradientMode; ABorderColor: TColor;
      AState: TdxNavBarObjectStates); override;
  end;

implementation

uses
  Math, cxGraphics, dxOffice11, cxGeometry, cxContainer,
  dxNavBarViewsFact, dxNavBarGraphics, dxNavBarConsts;

type
  TdxNavBarGroupAccess = class(TdxNavBarGroup);
  TdxNavBarAccess = class(TdxNavBar);

{ TdxNavBarBaseViewInfo }

destructor TdxNavBarBaseViewInfo.Destroy;
var
  I: Integer;
  AGroup: TdxNavBarGroup;
  AControl: TdxNavBarGroupControl;
begin
  if not NavBar.IsDestroying then
    for I := 0 to NavBar.Groups.Count - 1 do
    begin
      AGroup := NavBar.Groups[I];
      AControl := AGroup.Control;
      if (AControl <> nil) and (AControl.Parent = NavBar) and AControl.HandleAllocated then
        SetWindowRgn(AGroup.Control.Handle, 0, False);
    end;
  inherited Destroy;
end;

procedure TdxNavBarBaseViewInfo.CheckControlWindowRegion(AGroup: TdxNavBarGroupViewInfo);
begin
  SetGroupControlWindowRegion(cxRectContent(ActiveGroupViewInfo.ItemsRect, GetGroupBorderOffsets), AGroup);
end;

procedure TdxNavBarBaseViewInfo.CorrectBounds;
var
  I, AActiveGroupViewInfoIndex: Integer;
  AOffsetY, AMinHeight: Integer;
begin
  if (GroupCount = 0) or (ActiveGroupViewInfo = nil) then Exit;

  AActiveGroupViewInfoIndex := IndexOfGroupViewInfo(ActiveGroupViewInfo);
  AOffsetY := ClientHeight - Groups[GroupCount - 1].Rect.Bottom;
  AMinHeight := GetActiveGroupMinHeight;
  if (ActiveGroupViewInfo.ItemsRect.Bottom + AOffsetY < ActiveGroupViewInfo.ItemsRect.Top + AMinHeight) then
    AOffsetY := ActiveGroupViewInfo.ItemsRect.Top + AMinHeight - ActiveGroupViewInfo.ItemsRect.Bottom;

  CorrectTopVisibleIndex(AOffsetY);

  ActiveGroupViewInfo.CorrectActiveGroupBounds(0, AOffsetY);
  for I := AActiveGroupViewInfoIndex + 1 to GroupCount - 1 do
    Groups[I].CorrectBounds(0, AOffsetY);
  CalculateScrollButtonsBounds;
end;

procedure TdxNavBarBaseViewInfo.CorrectTopVisibleIndex(AOffsetY: Integer);
var
  I: Integer;
  AItemsRectHeight: Integer;
  ATopLinkIndex: Integer;
begin
  if (ActiveGroupViewInfo <> nil) and (ActiveGroupViewInfo.Group.TopVisibleLinkIndex > 0) then
  begin
    AItemsRectHeight := ActiveGroupViewInfo.ItemsRect.Height + AOffsetY - GetGroupCaptionSeparatorWidth;
    ATopLinkIndex := Min(ActiveGroupViewInfo.Group.TopVisibleLinkIndex, ActiveGroupViewInfo.ItemCount - 1);
    for I := ATopLinkIndex - 1 downto 0 do
      if (ActiveGroupViewInfo.Items[ActiveGroupViewInfo.ItemCount - 1].Rect.Bottom - ActiveGroupViewInfo.Items[I].Rect.Top) <= AItemsRectHeight then
        ATopLinkIndex := I
      else
        Break;
     TdxNavBarGroupAccess(ActiveGroupViewInfo.Group).CorrectTopVisibleIndex(ATopLinkIndex);
  end;
end;

function TdxNavBarBaseViewInfo.GetActiveGroupMinHeight: Integer;
var
  ATopVisibleLinkViewInfo: TdxNavBarLinkViewInfo;
  AViewInfo: TdxNavBarGroupViewInfo;
begin
  AViewInfo := ActiveGroupViewInfo;
  if AViewInfo <> nil then
  begin
    if (AViewInfo.Control <> nil) and (AViewInfo.Control.GetMinHeight <> 0) then
      Result := AViewInfo.Control.GetMinHeight
    else
    begin
      Result := 2 * GetScrollButtonVerticalSize + 3 * GetScrollButtonVerticalEdge;
      if AViewInfo.ItemCount > AViewInfo.Group.TopVisibleLinkIndex then
      begin
        ATopVisibleLinkViewInfo := AViewInfo.Items[ActiveGroupViewInfo.Group.TopVisibleLinkIndex];
        Result := Max(Result, cxRectHeight(ATopVisibleLinkViewInfo.Rect) + GetGroupCaptionSeparatorWidth);
      end;
    end;
  end
  else
    Result := 0;
end;

procedure TdxNavBarBaseViewInfo.DoGroupActivate(AGroup: TdxNavBarGroup);
begin
  if NavBar.ActiveGroupIndex <> AGroup.Index then
  begin
    NavBar.ActiveGroup := AGroup;
    NavBar.DesignerModified;
  end;
end;

procedure TdxNavBarBaseViewInfo.DoGroupDeactivate(AGroup: TdxNavBarGroup);
begin
end;

function TdxNavBarBaseViewInfo.IsGroupActive(AGroup: TdxNavBarGroup): Boolean;
begin
  Result := AGroup = NavBar.ActiveGroup;
end;

procedure TdxNavBarBaseViewInfo.MakeLinkVisible(ALink: TdxNavBarItemLink; ATop: Boolean = True);
var
  AGroupViewInfo: TdxNavBarGroupViewInfo;
  ALinkViewInfo: TdxNavBarLinkViewInfo;
  ADisplacement: Integer;
begin
  if NavBar.ActiveGroup <> ALink.Group then
  begin
    NavBar.ActiveGroup := GetRootGroup(ALink.Group);
    ALink.Group.Expanded := True;
  end;
  AGroupViewInfo := GetGroupViewInfoByGroup(ALink.Group);
  if AGroupViewInfo <> nil then
  begin
    ALinkViewInfo := AGroupViewInfo.GetLinkViewInfoByLink(ALink);
    if ALinkViewInfo <> nil then
    begin
      if NavBar.OptionsBehavior.Common.AllowChildGroups then
      begin
        ADisplacement := 0;
        if ATop or (ALinkViewInfo.Rect.Top < ActiveGroupViewInfo.ItemsRect.Top) then
          ADisplacement := ALinkViewInfo.Rect.Top - ActiveGroupViewInfo.ItemsRect.Top
        else
          if ALinkViewInfo.Rect.Bottom > ActiveGroupViewInfo.ItemsRect.Bottom then
            ADisplacement := ALinkViewInfo.Rect.Bottom - ActiveGroupViewInfo.ItemsRect.Bottom;
        TdxNavBarAccess(NavBar).ActiveGroupScrollBar.Position := TdxNavBarAccess(NavBar).ActiveGroupScrollBar.Position + ADisplacement;
      end
      else
        if ATop or (ALinkViewInfo.Rect.Top <= AGroupViewInfo.ItemsRect.Top) or
          (cxRectHeight(ALinkViewInfo.Rect) > cxRectHeight(AGroupViewInfo.ItemsRect)) then
          ALink.Group.TopVisibleLinkIndex := AGroupViewInfo.IndexOfLinkViewInfo(ALinkViewInfo)
        else
          while (ALinkViewInfo.Rect.Bottom > AGroupViewInfo.ItemsRect.Bottom) do
          begin
            ALink.Group.TopVisibleLinkIndex := ALink.Group.TopVisibleLinkIndex + 1;
            CalculateBounds;
          end;
      NavBar.InvalidateAll(doRecreate);
    end;
  end;
end;

procedure TdxNavBarBaseViewInfo.MakeGroupVisible(AGroup: TdxNavBarGroup;
  AExpandGroup: Boolean = True; ATop: Boolean = True);
var
  AGroupViewInfo: TdxNavBarGroupViewInfo;
  ADisplacement: Integer;
begin
  NavBar.ActiveGroup := GetRootGroup(AGroup);
  if AGroup.Parent <> nil then
  begin
    AGroup.Parent.Expanded := True;
    AGroupViewInfo := GetGroupViewInfoByGroup(AGroup);
    if AGroupViewInfo <> nil then
    begin
      ADisplacement := 0;
      if ATop or (AGroupViewInfo.CaptionRect.Top < ActiveGroupViewInfo.ItemsRect.Top) then
        ADisplacement := AGroupViewInfo.CaptionRect.Top - ActiveGroupViewInfo.ItemsRect.Top
      else
        if AGroupViewInfo.CaptionRect.Bottom > ActiveGroupViewInfo.ItemsRect.Bottom then
          ADisplacement := AGroupViewInfo.captionRect.Bottom - ActiveGroupViewInfo.ItemsRect.Bottom;
      TdxNavBarAccess(NavBar).ActiveGroupScrollBar.Position := TdxNavBarAccess(NavBar).ActiveGroupScrollBar.Position + ADisplacement;
      NavBar.InvalidateAll(doRecreate);
    end;
  end;
end;

function TdxNavBarBaseViewInfo.GetRootGroup(AGroup: TdxNavBarGroup): TdxNavBarGroup;
begin
  Result := AGroup;
  while Result.Parent <> nil do
    Result := Result.Parent;
end;

{ TdxNavBarCustomPainter }

class function TdxNavBarBasePainter.GetCategories: TdxNavBarViewCategories;
begin
  Result := [nbvcSideBar];
end;

class function TdxNavBarBasePainter.GetViewInfoClass: TdxNavBarViewInfoClass;
begin
  Result := TdxNavBarBaseViewInfo;
end;

function TdxNavBarBasePainter.NeedActiveGroupScrollBar: Boolean;
begin
  Result := True;
end;

function TdxNavBarBasePainter.GetControllerClass: TdxNavBarControllerClass;
begin
  Result := TdxNavBarNavigationBarController;
end;

class function TdxNavBarBasePainter.ButtonPainterClass: TdxNavBarCustomButtonPainterClass;
begin
  Result := TdxNavBarBaseButtonPainter;
end;

class function TdxNavBarBasePainter.ScrollButtonsPainterClass: TdxNavBarCustomScrollButtonsPainterClass;
begin
  Result := TdxNavBarBaseScrollButtonsPainter;
end;

class function TdxNavBarBasePainter.SelectionPainterClass: TdxNavBarCustomSelectionPainterClass;
begin
  Result := TdxNavBarBaseSelectionPainter;
end;

{ TdxNavBarBaseSelectionPainter }

class procedure TdxNavBarBaseSelectionPainter.GetColors(AState: TdxNavBarObjectStates; ABackColor: TColor;
  out AFillColor, ATopLeftOuterColor, ABottomRightOuterColor, ATopLeftInnerColor, ABottomRightInnerColor: TColor);
begin
  inherited;
  if IsPressed(AState) then
  begin
    ATopLeftOuterColor := DarkDarkBorderColor(ABackColor);
    ABottomRightOuterColor := LightLightBorderColor(ABackColor);
  end
  else
  begin
    ATopLeftOuterColor := LightLightBorderColor(ABackColor);
    ABottomRightOuterColor := DarkDarkBorderColor(ABackColor);
  end;
end;

{ TdxNavBarBaseButtonPainter }

class procedure TdxNavBarBaseButtonPainter.InternalDrawButton(ACanvas: TCanvas; ARect: TRect;
  APicture: TPicture; AColor1, AColor2: TColor; AAlphaBlend1, AAlphaBlend2: Byte;
  AGradientMode: TdxBarStyleGradientMode; ABorderColor: TColor; AState: TdxNavBarObjectStates);
begin
  inherited;
  if sPressed in AState then
    dxNavBarDrawSelectedFrame(ACanvas, ARect, DarkDarkBorderColor(AColor1), DarkDarkBorderColor(AColor1),
      DarkBorderColor(AColor1), DarkBorderColor(AColor1))
  else
    dxNavBarDrawSelectedFrame(ACanvas, ARect, LightLightBorderColor(AColor1), DarkDarkBorderColor(AColor1),
      LightBorderColor(AColor1), DarkBorderColor(AColor1));
  if sActive in AState then
    FillRectByColor(ACanvas.Handle, cxRectSetBottom(ARect, ARect.Bottom, 1), clBlack);
end;

{ TdxNavBarBaseScrollButtonsPainter }

class procedure TdxNavBarBaseScrollButtonsPainter.InternalDrawBottomButton(ACanvas: TCanvas;
  ARect: TRect; AButtonPainterClass: TdxNavBarCustomButtonPainterClass;
  AColor1, AColor2: TColor; AAlphaBlend1, AAlphaBlend2: Byte;
  AGradientMode: TdxBarStyleGradientMode; ABorderColor: TColor; AState: TdxNavBarObjectStates);
var
  pt1, pt2, pt3: TPoint;
  AArrowHeight, AArrowWidth: Integer;
begin
  AButtonPainterClass.DrawButton(ACanvas, ARect, nil, AColor1, AColor2,
    AAlphaBlend1, AAlphaBlend2, AGradientMode, ABorderColor, AState);

  AArrowHeight := (ARect.Bottom - ARect.Top) div 4;
  AArrowWidth := 2 * AArrowHeight - 1;
  pt1.X := ARect.Left + (ARect.Right - ARect.Left) div 2 - 1;
  pt1.Y := ARect.Top + (ARect.Bottom - ARect.Top) div 2 + (AArrowHeight div 2) - 1;
  pt2.X := pt1.X - AArrowWidth div 2;
  pt2.Y := pt1.Y - AArrowHeight + 1;
  pt3.X := pt2.X + AArrowWidth - 1;
  pt3.Y := pt2.Y;
  if sDisabled in AState then
  begin
    ACanvas.Brush.Color := clGrayText;
    ACanvas.Pen.Color := clGrayText;
  end
  else
  begin
    ACanvas.Brush.Color := clBlack;
    ACanvas.Pen.Color := clBlack;
  end;
  ACanvas.Brush.Style := bsSolid;
  ACanvas.Pen.Style := psSolid;
  ACanvas.Polygon([pt1, pt2, pt3]);
end;

class procedure TdxNavBarBaseScrollButtonsPainter.InternalDrawTopButton(ACanvas: TCanvas;
  ARect: TRect; AButtonPainterClass: TdxNavBarCustomButtonPainterClass;
  AColor1, AColor2: TColor; AAlphaBlend1, AAlphaBlend2: Byte;
  AGradientMode: TdxBarStyleGradientMode; ABorderColor: TColor; AState: TdxNavBarObjectStates);
var
  pt1, pt2, pt3: TPoint;
  AArrowHeight, AArrowWidth: Integer;
begin
  AButtonPainterClass.DrawButton(ACanvas, ARect, nil, AColor1, AColor2,
    AAlphaBlend2, AAlphaBlend2, AGradientMode, ABorderColor, AState);

  AArrowHeight := (ARect.Bottom - ARect.Top) div 4;
  AArrowWidth := 2 * AArrowHeight - 1;
  pt1.X := ARect.Left + (ARect.Right - ARect.Left) div 2 - 1;
  pt1.Y := ARect.Top + (ARect.Bottom - ARect.Top) div 2 - (AArrowHeight div 2) - 1;
  pt2.X := pt1.X - AArrowWidth div 2;
  pt2.Y := pt1.Y + AArrowHeight - 1;
  pt3.X := pt2.X + AArrowWidth - 1;
  pt3.Y := pt2.Y;
  if sDisabled in AState then
  begin
    ACanvas.Brush.Color := clGrayText;
    ACanvas.Pen.Color := clGrayText;
  end
  else
  begin
    ACanvas.Brush.Color := clBlack;
    ACanvas.Pen.Color := clBlack;
  end;
  ACanvas.Brush.Style := bsSolid;
  ACanvas.Pen.Style := psSolid;
  ACanvas.Polygon([pt1, pt2, pt3]);
end;

{ TdxNavBarFlatPainter }

class function TdxNavBarFlatPainter.ButtonPainterClass: TdxNavBarCustomButtonPainterClass;
begin
  Result := TdxNavBarFlatButtonPainter;
end;

{ TdxNavBarFlatButtonPainter }

class procedure TdxNavBarFlatButtonPainter.InternalDrawButton(ACanvas: TCanvas; ARect: TRect;
  APicture: TPicture; AColor1, AColor2: TColor; AAlphaBlend1, AAlphaBlend2: Byte;
  AGradientMode: TdxBarStyleGradientMode; ABorderColor: TColor; AState: TdxNavBarObjectStates);
begin
  inherited;
  if sPressed in AState then
    dxNavBarDrawSelectedFrame(ACanvas, ARect, DarkDarkBorderColor(AColor1), LightLightBorderColor(AColor1),
      DarkBorderColor(AColor1), clNone)
  else
    if sHotTracked in AState then
      dxNavBarDrawSelectedFrame(ACanvas, ARect, LightLightBorderColor(AColor1), DarkDarkBorderColor(AColor1),
        clNone, DarkBorderColor(AColor1))
    else
      dxNavBarDrawSelectedFrame(ACanvas, ARect, LightLightBorderColor(AColor1), DarkBorderColor(AColor1),
        clNone, clNone);
  if sActive in AState then
    FillRectByColor(ACanvas.Handle, cxRectSetBottom(ARect, ARect.Bottom, 1), clBlack);
end;

initialization
  RegisterView(dxNavBarBaseView, 'BaseView', TdxNavBarBasePainter);
  RegisterView(dxNavBarFlatView, 'FlatView', TdxNavBarFlatPainter);

finalization
  UnRegisterView(dxNavBarBaseView);
  UnRegisterView(dxNavBarFlatView);

end.
