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

unit dxNavBarOfficeViews;

{$I cxVer.inc}

interface

uses
  Windows, Graphics, Types, dxNavBar, dxNavBarStyles,
  dxNavBarBaseViews, dxNavBarCollns, dxNavBarCustomPainters;

type
  TdxNavBarOffice1ViewInfo = class(TdxNavBarBaseViewInfo)
  protected
    function GetGroupCaptionHeightAddon: Integer; override;
    function GetGroupSeparatorWidth: Integer; override;
  public
    procedure AssignDefaultBackgroundStyle; override;
    procedure AssignDefaultGroupBackgroundStyle; override;
    procedure AssignDefaultGroupHeaderStyle; override;
    procedure AssignDefaultItemStyle; override;
    procedure AssignDefaultItemDisabledStyle; override;
  end;

  TdxNavBarOffice1Painter = class(TdxNavBarBasePainter)
  protected
    class function GetViewInfoClass: TdxNavBarViewInfoClass; override;

    class function ButtonPainterClass: TdxNavBarCustomButtonPainterClass; override;
  end;

  TdxNavBarOffice2Painter = class(TdxNavBarOffice1Painter)
  protected
    class function SelectionPainterClass: TdxNavBarCustomSelectionPainterClass; override;
  end;

  TdxNavBarOffice3LinkViewInfo = class(TdxNavBarLinkViewInfo)
  public
    function SelectionRect: TRect; override;
  end;

  TdxNavBarOffice3ChildGroupViewInfo = class(TdxNavBarChildGroupViewInfo)
  public
    function SelectionRect: TRect; override;
  end;

  TdxNavBarOffice3ViewInfo = class(TdxNavBarOffice1ViewInfo)
  protected
    function GetLinksLargeSeparatorWidth: Integer; override;
    function CanSelectLinkByRect: Boolean; override;
  public
    procedure AssignDefaultItemHotTrackedStyle; override;
    procedure AssignDefaultItemPressedStyle; override;
  end;

  TdxNavBarOffice3Painter = class(TdxNavBarOffice2Painter)
  protected
    class function GetChildGroupViewInfoClass: TdxNavBarChildGroupViewInfoClass; override;
    class function GetViewInfoClass: TdxNavBarViewInfoClass; override;
    class function GetLinkViewInfoClass: TdxNavBarLinkViewInfoClass; override;
  end;

  TdxNavBarOfficeButtonPainter = class(TdxNavBarCustomButtonPainter)
  protected
    class procedure InternalDrawButton(ACanvas: TCanvas; ARect: TRect; APicture: TPicture;
      AColor1, AColor2: TColor; AAlphaBlend1, AAlphaBlend2: Byte;
      AGradientMode: TdxBarStyleGradientMode; ABorderColor: TColor;
      AState: TdxNavBarObjectStates); override;
  end;

implementation

uses
  CommCtrl, ImgList, Buttons, cxGeometry, cxGraphics, dxOffice11,
  dxNavBarViewsFact, dxNavBarGraphics, dxNavBarConsts;

{ TdxNavBarOffice1ViewInfo }

procedure TdxNavBarOffice1ViewInfo.AssignDefaultBackgroundStyle;
begin
  NavBar.DefaultStyles.Background.ResetValues;
  NavBar.DefaultStyles.Background.BackColor := clBtnFace;
  NavBar.DefaultStyles.Background.BackColor2 := clBtnFace;
end;

procedure TdxNavBarOffice1ViewInfo.AssignDefaultGroupBackgroundStyle;
begin
  NavBar.DefaultStyles.GroupBackground.ResetValues;
  NavBar.DefaultStyles.GroupBackground.BackColor := clBtnFace;
  NavBar.DefaultStyles.GroupBackground.BackColor2 := clBtnFace;
end;

procedure TdxNavBarOffice1ViewInfo.AssignDefaultGroupHeaderStyle;
begin
  NavBar.DefaultStyles.GroupHeader.ResetValues;
  NavBar.DefaultStyles.GroupHeader.BackColor := clBtnFace;
  NavBar.DefaultStyles.GroupHeader.BackColor2 := clBtnFace;
  NavBar.DefaultStyles.GroupHeader.Font.Color := clBtnText;
  NavBar.DefaultStyles.GroupHeader.HAlignment := haCenter;
end;

procedure TdxNavBarOffice1ViewInfo.AssignDefaultItemStyle;
begin
  NavBar.DefaultStyles.Item.ResetValues;
  NavBar.DefaultStyles.Item.Font.Color := clBtnText;
  NavBar.DefaultStyles.Item.HAlignment := haCenter;
end;

procedure TdxNavBarOffice1ViewInfo.AssignDefaultItemDisabledStyle;
begin
  with NavBar.DefaultStyles do
  begin
    ItemDisabled.Assign(NavBar.DefaultStyles.Item);
    ItemDisabled.Font.Color := clBtnShadow;
  end;
end;

function TdxNavBarOffice1ViewInfo.GetGroupCaptionHeightAddon: Integer;
begin
  Result := ScaleFactor.Apply(8);
end;

function TdxNavBarOffice1ViewInfo.GetGroupSeparatorWidth: Integer;
begin
  Result := ScaleFactor.Apply(1);
end;

{ TdxNavBarOffice1Painter }

class function TdxNavBarOffice1Painter.GetViewInfoClass: TdxNavBarViewInfoClass;
begin
  Result := TdxNavBarOffice1ViewInfo;
end;

class function TdxNavBarOffice1Painter.ButtonPainterClass: TdxNavBarCustomButtonPainterClass;
begin
  Result := TdxNavBarOfficeButtonPainter;
end;

{ TdxNavBarOffice2Painter }

class function TdxNavBarOffice2Painter.SelectionPainterClass: TdxNavBarCustomSelectionPainterClass;
begin
  Result := TdxNavBarUltraFlatSelectionPainter;
end;

{ TdxNavBarOffice3LinkViewInfo }

function TdxNavBarOffice3LinkViewInfo.SelectionRect: TRect;
begin
  Result := Rect;
  InflateRect(Result, -1, -1);
end;

{ TdxNavBarOffice3ChildGroupViewInfo }

function TdxNavBarOffice3ChildGroupViewInfo.SelectionRect: TRect;
begin
  Result := CaptionRect;
  InflateRect(Result, -1, -1);
end;

{ TdxNavBarOffice3ViewInfo }

procedure TdxNavBarOffice3ViewInfo.AssignDefaultItemHotTrackedStyle;
begin
  inherited;
  if IsHighContrastWhite then
    NavBar.DefaultStyles.ItemHotTracked.Font.Color := clHighlightText;
end;

procedure TdxNavBarOffice3ViewInfo.AssignDefaultItemPressedStyle;
begin
  inherited;
  if IsHighContrastWhite then
    NavBar.DefaultStyles.ItemPressed.Font.Color := clHighlightText;
end;

function TdxNavBarOffice3ViewInfo.GetLinksLargeSeparatorWidth: Integer;
begin
  Result := ScaleFactor.Apply(16);
end;

function TdxNavBarOffice3ViewInfo.CanSelectLinkByRect: Boolean;
begin
  Result := True;
end;

{ TdxNavBarOffice3Painter }

class function TdxNavBarOffice3Painter.GetViewInfoClass: TdxNavBarViewInfoClass;
begin
  Result := TdxNavBarOffice3ViewInfo;
end;

class function TdxNavBarOffice3Painter.GetChildGroupViewInfoClass: TdxNavBarChildGroupViewInfoClass;
begin
  Result := TdxNavBarOffice3ChildGroupViewInfo;
end;

class function TdxNavBarOffice3Painter.GetLinkViewInfoClass: TdxNavBarLinkViewInfoClass;
begin
  Result := TdxNavBarOffice3LinkViewInfo;
end;

{ TdxNavBarOfficeButtonPainter }

class procedure TdxNavBarOfficeButtonPainter.InternalDrawButton(ACanvas: TCanvas; ARect: TRect;
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
        LightBorderColor(AColor1), DarkBorderColor(AColor1))
    else
      dxNavBarDrawSelectedFrame(ACanvas, ARect, LightLightBorderColor(AColor1), DarkDarkBorderColor(AColor1),
        clNone, clNone);
  if sActive in AState then
    FillRectByColor(ACanvas.Handle, cxRectSetBottom(ARect, ARect.Bottom, 1), clBlack);
end;

initialization
  RegisterView(dxNavBarOffice1View, 'Office1View', TdxNavBarOffice1Painter);
  RegisterView(dxNavBarOffice2View, 'Office2View', TdxNavBarOffice2Painter);
  RegisterView(dxNavBarOffice3View, 'Office3View', TdxNavBarOffice3Painter);

finalization
  UnRegisterView(dxNavBarOffice1View);
  UnRegisterView(dxNavBarOffice2View);
  UnRegisterView(dxNavBarOffice3View);

end.
