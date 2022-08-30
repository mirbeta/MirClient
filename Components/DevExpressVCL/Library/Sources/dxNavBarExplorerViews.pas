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

unit dxNavBarExplorerViews;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Windows, Graphics, Classes, Forms, Controls, SysUtils,
  dxCoreGraphics, dxNavBar, dxNavBarBase, dxNavBarCollns, dxNavBarStyles, dxNavBarCustomPainters,
  cxClasses, cxControls, cxGeometry;

const
  ebSplitter = 3;

type
  { ExplorerBarView }

  TdxNavBarExplorerBarLinkViewInfo = class(TdxNavBarLinkViewInfo)
  public
    function SeparatorColor: TColor; override;
  end;

  TdxNavBarExplorerBarGroupViewInfo = class(TdxNavBarGroupViewInfo)
  private
    FAlign: TcxTopBottom;
    FSplitterRect: TRect;
    FUseRestSpace: Boolean;
  protected
    procedure AdjustBoundsByAlignment(ARestSpaceValue: Integer);
    function GetSplitterSize: Integer; virtual;
  public
    procedure CalculateBounds(var X, Y: Integer); override;
    procedure CorrectBounds(dX, dY: Integer); override;
    property Align: TcxTopBottom read FAlign write FAlign;
    property SplitterRect: TRect read FSplitterRect;
  end;

  TdxNavBarExplorerBarViewInfo = class(TdxNavBarViewInfo)
  private
    procedure CheckJustExpandedGroup;
    function IsEnoughSpaceForScrollBar: Boolean;
  protected
    procedure CorrectScrollInfo; virtual;

    procedure CorrectBounds; override;
    procedure DoCalculateBounds(X, Y: Integer); override;
    procedure DoCreateGroupsInfo; override;

    function GetGroupItemsVisible(AGroup: TdxNavBarGroup): Boolean; virtual;
    function NeedCalculateScrollBarVisible: Boolean; virtual;

    function GetGroupBorderOffsets: TRect; override;
    function GetGroupCaptionHeightAddon: Integer; override;
    function GetGroupCaptionSignSize: TSize; override;
    function GetGroupSeparatorWidth: Integer; override;
    function GetGroupEdges: TPoint; override;

    function GetAbsoluteLinksImageEdges: TRect; virtual;
    function GetLinksImageEdges: TRect; override;

    function CanHasActiveGroup: Boolean; override;
    function CanHasSpecialGroup: Boolean; override;
    function CanHasImageInGroupCaption: Boolean; override;
    function CanHasSignInGroupCaption: Boolean; override;
    function CanHasGroupViewAsIconView: Boolean; override;
    function CanHasGroupWithNoCaption: Boolean; override;
    function CanHasVisibleItemsInGroup(AGroup: TdxNavBarGroup): Boolean; override;
    function HasClientAreaScrollbar: Boolean; override;
  public
    procedure AssignDefaultBackgroundStyle; override;
    procedure AssignDefaultGroupBackgroundStyle; override;
    procedure AssignDefaultGroupHeaderStyle; override;
    procedure AssignDefaultGroupHeaderActiveStyle; override;
    procedure AssignDefaultItemStyle; override;
    procedure AssignDefaultItemDisabledStyle; override;
    procedure AssignDefaultItemHotTrackedStyle; override;
    procedure AssignDefaultItemPressedStyle; override;

    procedure DoGroupActivate(AGroup: TdxNavBarGroup); override;
    procedure DoGroupDeactivate(AGroup: TdxNavBarGroup); override;
    function IsGroupActive(AGroup: TdxNavBarGroup): Boolean; override;
    procedure MakeLinkVisible(ALink: TdxNavBarItemLink; ATop: Boolean = True); override;
    procedure MakeGroupVisible(AGroup: TdxNavBarGroup;
      AExpandGroup: Boolean = True; ATop: Boolean = True); override;

    // Hit tests
    function GetGroupViewInfoIndexAtSplitterPos(const pt: TPoint): Integer; virtual;
    function GetGroupViewInfoAtSplitterPos(const pt: TPoint): TdxNavBarGroupViewInfo; virtual;
  end;

  TdxNavBarExplorerBarController = class(TdxNavBarController)
  private
    FStartDragPoint: TPoint;
    FStartDragHeight: Integer;

    function GetSizedGroup: TdxNavBarGroupViewInfo;
  strict protected
    function NeedResetPressedPartOnMouseUp: Boolean; virtual;
  protected
    function GetPartAtPos(const APoint: TPoint): TdxNavBarPart; override;

    procedure DoMouseDown(AButton: TMouseButton; AShift: TShiftState; const APoint: TPoint); override;
    procedure DoMouseMove(AShift: TShiftState; const APoint: TPoint); override;
    procedure DoMouseUp(AButton: TMouseButton; AShift: TShiftState; const APoint: TPoint); override;
    function GetCursor: HIcon; override;

    procedure DoSplitterStartDrag(const APoint: TPoint);
    procedure DoSplitterDrag(const APoint: TPoint);
    procedure DoSplitterEndDrag(const APoint: TPoint);
    procedure DoSplitterCancelDrag;
  public
    destructor Destroy; override;
  end;

  TdxNavBarExplorerBarPainter = class(TdxNavBarElementPainter)
  protected
    class function GetViewInfoClass: TdxNavBarViewInfoClass; override;
    class function GetGroupViewInfoClass: TdxNavBarGroupViewInfoClass; override;
    class function GetLinkViewInfoClass: TdxNavBarLinkViewInfoClass; override;
    function GetControllerClass: TdxNavBarControllerClass; override;

    class function GroupBorderPainterClass: TdxNavBarCustomGroupBorderPainterClass; override;
    class function SignPainterClass: TdxNavBarCustomSignPainterClass; override;
    function NeedScrollBar: Boolean; override;
  public
    procedure DrawGroup(AGroupViewInfo: TdxNavBarGroupViewInfo); override;
    procedure DrawGroupControlSplitter(AGroupViewInfo: TdxNavBarExplorerBarGroupViewInfo); virtual;

    class function GetCategories: TdxNavBarViewCategories; override;
  end;

  TdxNavBarExplorerBarBorderPainter = class(TdxNavBarCustomGroupBorderPainter)
  protected
    class procedure InternalDrawBorder(ACanvas: TCanvas; ARect: TRect; AColor: TColor; AWithCaption: Boolean); override;
  end;

  { TdxNavBarExplorerBarSignPainter }

  TdxNavBarExplorerBarSignPainter = class(TdxNavBarCustomSignPainter)
  protected
    class function CloneBitmap(ASource: TBitmap; const ATargetSize: TSize): TBitmap;
    class procedure DrawSignSelection(ACanvas: TCanvas; ARect: TRect; AForeColor,
      ABackColor1, ABackColor2: TColor; AState: TdxNavBarObjectStates); virtual;
    class procedure InternalDrawSign(ACanvas: TCanvas; ARect: TRect; AScaleFactor: TdxScaleFactor;
      AForeColor, ABackColor1, ABackColor2 : TColor; AState: TdxNavBarObjectStates); override;
  end;

  { UltraFlatExplorerBarView }

  TdxNavBarUltraFlatExplorerBarPainter = class(TdxNavBarExplorerBarPainter)
  protected
    class function SelectionPainterClass: TdxNavBarCustomSelectionPainterClass; override;
  end;

  { AdvExplorerBarView }

  TdxNavBarAdvExplorerBarLinkViewInfo = class(TdxNavBarExplorerBarLinkViewInfo); // #Ch to avoid BC

  TdxNavBarAdvExplorerBarGroupViewInfo = class(TdxNavBarExplorerBarGroupViewInfo)
  private
    function IsDefaultBgColor: Boolean;
    function IsDefaultCaptionColor: Boolean;
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
    function CaptionFontColor: TColor; override;
    function CaptionSignColor: TColor; override;
  end;

  TdxNavBarAdvExplorerBarViewInfo = class(TdxNavBarExplorerBarViewInfo)
  private
    function IsDefaultBgColor: Boolean;
  protected
    procedure CreateColors; override;
    procedure RefreshColors; override;
    procedure ReleaseColors; override;

    function GetGroupCaptionSignSize: TSize; override;
  public
    function BgBackColor: TColor; override;
    function BgBackColor2: TColor; override;
    function BgAlphaBlend: Byte; override;
    function BgAlphaBlend2: Byte; override;
    function BgGradientMode: TdxBarStyleGradientMode; override;

    procedure AssignDefaultBackgroundStyle; override;
    procedure AssignDefaultGroupBackgroundStyle; override;
    procedure AssignDefaultGroupHeaderStyle; override;
    procedure AssignDefaultGroupHeaderActiveStyle; override;
    procedure AssignDefaultItemStyle; override;
    procedure AssignDefaultItemPressedStyle; override;
    procedure AssignDefaultItemHotTrackedStyle; override;
    procedure AssignDefaultItemDisabledStyle; override;
  end;

  TdxNavBarAdvExplorerBarPainter = class(TdxNavBarExplorerBarPainter)
  protected
    class function GetViewInfoClass: TdxNavBarViewInfoClass; override;
    class function GetGroupViewInfoClass: TdxNavBarGroupViewInfoClass; override;
    class function GetLinkViewInfoClass: TdxNavBarLinkViewInfoClass; override;

    class function ButtonPainterClass: TdxNavBarCustomButtonPainterClass; override;
    class function SignPainterClass: TdxNavBarCustomSignPainterClass; override;
  end;

  TdxNavBarAdvExplorerButtonPainter = class(TdxNavBarCustomButtonPainter)
  protected
    class procedure InternalDrawButton(ACanvas: TCanvas; ARect: TRect; APicture: TPicture;
      AColor1, AColor2: TColor; AAlphaBlend1, AAlphaBlend2: Byte;
      AGradientMode: TdxBarStyleGradientMode; ABorderColor: TColor;
      AState: TdxNavBarObjectStates); override;
  end;

  TdxNavBarAdvExplorerBarSignPainter = class(TdxNavBarExplorerBarSignPainter)
  protected
    class procedure InternalDrawSign(ACanvas: TCanvas; ARect: TRect; AScaleFactor: TdxScaleFactor;
      AForeColor, ABackColor1, ABackColor2 : TColor; AState: TdxNavBarObjectStates); override;
    class function PrepareBitmap(ACanvas: TCanvas; ARect: TRect;
      ABitmap: TBitmap; AForeColor: TColor; AState: TdxNavBarObjectStates): TBitmap;
  end;

implementation

uses
  Types, CommCtrl, ImgList, Math, cxGraphics, dxOffice11, dxNavBarViewsFact, dxNavBarConsts, dxNavBarGraphics;

type
  TdxCustomNavBarBarAccess = class(TdxCustomNavBar);

function GetColor(ANormalColor, AHotColor: TColor;
  AState: TdxNavBarObjectStates): TColor;
begin
  if sDisabled in AState then
    Result := LightLightColor(ANormalColor)
  else
    if AState * [sHotTracked, sPressed, sSelected] <> [] then
      Result := AHotColor
    else
      Result := ANormalColor;
end;

function TdxNavBarExplorerBarLinkViewInfo.SeparatorColor: TColor;
begin
  Result := GroupViewInfo.BorderColor;
end;

{ TdxNavBarExplorerBarGroupViewInfo }

procedure TdxNavBarExplorerBarGroupViewInfo.CalculateBounds(var X, Y: Integer);
var
  ASpace, AShortage: Integer;
begin
  inherited;
  if Group.OptionsGroupControl.AllowControlResizing and Group.Expanded and
    Group.ShowControl and Group.UseControl and not FUseRestSpace then
  begin
    ASpace := GetSplitterSize;
    AShortage := ASpace - TdxNavBarExplorerBarViewInfo(ViewInfo).GetSpaceBetweenGroups;

    if AShortage > 0 then
    begin
      Inc(Y, AShortage);
      if Align = vaBottom then
        CorrectBounds(0, AShortage);
    end;
    if Align = vaTop then
      FSplitterRect := cxRect(FRect.Left, FRect.Bottom, FRect.Right, FRect.Bottom + ASpace)
    else
      FSplitterRect := cxRect(FRect.Left, FRect.Top - ASpace, FRect.Right, FRect.Top);
  end
  else
    FSplitterRect := cxNullRect;
end;

procedure TdxNavBarExplorerBarGroupViewInfo.CorrectBounds(dX, dY: Integer);
begin
  inherited;
  OffsetRect(FSplitterRect, dX, dY);
end;

procedure TdxNavBarExplorerBarGroupViewInfo.AdjustBoundsByAlignment(ARestSpaceValue: Integer);
begin
  if FUseRestSpace then
  begin
    Inc(FRect.Bottom, ARestSpaceValue);
    Inc(FItemsRect.Bottom, ARestSpaceValue);
  end
  else
    if Align = vaBottom then
      CorrectBounds(0, ARestSpaceValue);
end;

function TdxNavBarExplorerBarGroupViewInfo.GetSplitterSize: Integer;
begin
  Result := ScaleFactor.Apply(3);
end;

{ TdxNavBarExplorerBarViewInfo }

procedure TdxNavBarExplorerBarViewInfo.CorrectBounds;
begin
  CorrectScrollInfo;
end;

procedure TdxNavBarExplorerBarViewInfo.DoCalculateBounds(X, Y: Integer);
var
  I: Integer;
  ARestPlace: Integer;
begin
  inherited;
  if GroupCount > 0 then
  begin
    ARestPlace := ClientHeight - Groups[GroupCount - 1].Rect.Bottom - GetGroupEdges.Y;
    if ARestPlace > 0 then
      for I := 0 to GroupCount - 1 do
        TdxNavBarExplorerBarGroupViewInfo(Groups[I]).AdjustBoundsByAlignment(ARestPlace);
  end;
end;

procedure TdxNavBarExplorerBarViewInfo.DoCreateGroupsInfo;

  procedure InternalDoCreateGroupInfo(AIndex: Integer; AAlign: TcxTopBottom; AUseRestSpace: Boolean = False);
  var
    AGroup: TdxNavBarGroup;
  begin
    AGroup := NavBar.RootGroups[AIndex];
    AddGroup(Self, AGroup, True, GetGroupItemsVisible(AGroup));
    TdxNavBarExplorerBarGroupViewInfo(Groups[GroupCount - 1]).Align := AAlign;
    TdxNavBarExplorerBarGroupViewInfo(Groups[GroupCount - 1]).FUseRestSpace := AUseRestSpace;
  end;

  procedure InternalDoCreateGroupInfos(AGroupAlign: TcxTopBottom; AUsingRestSpaceGroupIndex: Integer);
  var
    I: Integer;
    AGroupInfoAlign: TcxTopBottom;
  begin
    if (AGroupAlign = vaTop) or (AUsingRestSpaceGroupIndex <> -1) and
      (NavBar.Groups[AUsingRestSpaceGroupIndex].Align = vaBottom) then
      AGroupInfoAlign := vaTop
    else
      AGroupInfoAlign := vaBottom;

    for I := 0 to NavBar.RootGroupCount - 1 do
      if NavBar.RootGroups[I].Visible and (NavBar.RootGroups[I].Align = AGroupAlign) then
        if AUsingRestSpaceGroupIndex = I then
        begin
          InternalDoCreateGroupInfo(I, AGroupInfoAlign, True);
          AGroupInfoAlign := vaBottom;
        end
        else
          InternalDoCreateGroupInfo(I, AGroupInfoAlign);
  end;

var
  I: Integer;
  AUsingRestSpaceGroupIndex: Integer;
begin
  AUsingRestSpaceGroupIndex := -1;
  for I := 0 to NavBar.RootGroupCount - 1 do
    if NavBar.RootGroups[I].Visible and NavBar.RootGroups[I].UseRestSpace and NavBar.RootGroups[I].Expanded then
    begin
      AUsingRestSpaceGroupIndex := I;
      Break;
    end;

  InternalDoCreateGroupInfos(vaTop, AUsingRestSpaceGroupIndex);
  InternalDoCreateGroupInfos(vaBottom, AUsingRestSpaceGroupIndex);
end;

function TdxNavBarExplorerBarViewInfo.GetGroupItemsVisible(AGroup: TdxNavBarGroup): Boolean;
begin
  Result := True;
end;

function TdxNavBarExplorerBarViewInfo.NeedCalculateScrollBarVisible: Boolean;
begin
  Result := GroupCount > 0;
end;

procedure TdxNavBarExplorerBarViewInfo.CorrectScrollInfo;
var
  I, AClientHeight, AHeaderHeight, AHeight: Integer;
  ADelta: Double;
  AOldVisible: Boolean;
  AMin, AMax, APageSize, ALeft: Integer;
  R: TRect;
begin
  AOldVisible := NavBar.ScrollBar.Visible;
  if NeedCalculateScrollBarVisible then
  begin
    AHeight := Groups[GroupCount - 1].Rect.Bottom + GetGroupEdges.Y;
    AHeaderHeight := cxRectHeight(HeaderRect);
    AClientHeight := ClientHeight - AHeaderHeight;
    if AHeight > AClientHeight then
    begin
      ADelta := (AClientHeight * NavBar.ScrollBar.Height) / AHeight;
      APageSize := Round(ADelta);
      AMin := 0;
      AMax := AHeight - AClientHeight + Round(ADelta);

      NavBar.ScrollBar.SmallChange := GetGroupCaptionSignSize.cx;
      if TdxCustomNavBarBarAccess(NavBar).IsScrollBarUseClientArea then
      begin
        if NavBar.UseRightToLeftScrollBar then
          ALeft := NavBar.Bounds.Left
        else
          ALeft := NavBar.Bounds.Right - NavBar.ScrollBar.Width;
        R := cxRectBounds(ALeft, HeaderRect.Bottom, NavBar.ScrollBar.Width, AClientHeight);
        CalculateScrollBarBoundsBySizeGrip(R);
        NavBar.ScrollBar.Bounds := R;
      end;
      NavBar.ScrollBar.SetScrollParams(AMin, AMax, NavBar.ScrollBar.Position, APageSize);
      CheckJustExpandedGroup;
      if IsEnoughSpaceForScrollBar then
        NavBar.ScrollBar.Visible := True;
    end
    else
    begin
      NavBar.ScrollBar.Position := 0;
      NavBar.ScrollBar.Visible := False;
    end;
  end
  else
    NavBar.ScrollBar.Visible := False;

  if AOldVisible <> NavBar.ScrollBar.Visible then
    InternalCalculateBounds
  else
    for I := 0 to GroupCount - 1 do
      Groups[I].CorrectBounds(0, -NavBar.ScrollPosition);
end;

function TdxNavBarExplorerBarViewInfo.GetGroupBorderOffsets: TRect;
begin
  Result := cxRect(1, 1, 1, 1);
end;

function TdxNavBarExplorerBarViewInfo.GetGroupCaptionHeightAddon: Integer;
begin
  Result := ScaleFactor.Apply(12);
end;

function TdxNavBarExplorerBarViewInfo.GetGroupCaptionSignSize: TSize;
begin
  Result := ScaleFactor.Apply(cxSize(18, 18));
end;

function TdxNavBarExplorerBarViewInfo.GetGroupSeparatorWidth: Integer;
begin
  Result := ScaleFactor.Apply(15);
end;

function TdxNavBarExplorerBarViewInfo.GetGroupEdges: TPoint;
begin
  Result := cxPoint(ScaleFactor.Apply(12), ScaleFactor.Apply(12));
end;

function TdxNavBarExplorerBarViewInfo.GetAbsoluteLinksImageEdges: TRect;
begin
  Result := cxRect(13, 4, 7, 4);
end;

function TdxNavBarExplorerBarViewInfo.GetLinksImageEdges: TRect;
begin
  with GetAbsoluteLinksImageEdges do
    Result := Rect(ScaleFactor.Apply(Left), Top, ScaleFactor.Apply(Right), Bottom);
end;

function TdxNavBarExplorerBarViewInfo.CanHasActiveGroup: Boolean;
begin
  Result := False;
end;

function TdxNavBarExplorerBarViewInfo.CanHasSpecialGroup: Boolean;
begin
  Result := True;
end;

function TdxNavBarExplorerBarViewInfo.CanHasImageInGroupCaption: Boolean;
begin
  Result := True;
end;

function TdxNavBarExplorerBarViewInfo.CanHasSignInGroupCaption: Boolean;
begin
  Result := True;
end;

function TdxNavBarExplorerBarViewInfo.CanHasGroupViewAsIconView: Boolean;
begin
  Result := False;
end;

function TdxNavBarExplorerBarViewInfo.CanHasGroupWithNoCaption: Boolean;
begin
  Result := True;
end;

function TdxNavBarExplorerBarViewInfo.CanHasVisibleItemsInGroup(AGroup: TdxNavBarGroup): Boolean;
begin
  Result := AGroup.Expanded;
end;

function TdxNavBarExplorerBarViewInfo.HasClientAreaScrollbar: Boolean;
begin
  Result := IsHeaderVisible and NavBar.ScrollBar.Visible;
end;

procedure TdxNavBarExplorerBarViewInfo.AssignDefaultBackgroundStyle;
begin
  NavBar.DefaultStyles.Background.ResetValues;
  NavBar.DefaultStyles.Background.BackColor := clWindow;
  NavBar.DefaultStyles.Background.BackColor2 := clWindow;
end;

procedure TdxNavBarExplorerBarViewInfo.AssignDefaultGroupBackgroundStyle;
begin
  NavBar.DefaultStyles.GroupBackground.ResetValues;
  NavBar.DefaultStyles.GroupBackground.BackColor := clWindow;
  NavBar.DefaultStyles.GroupBackground.BackColor2 := clWindow;
end;

procedure TdxNavBarExplorerBarViewInfo.AssignDefaultGroupHeaderStyle;
begin
  NavBar.DefaultStyles.GroupHeader.ResetValues;
  NavBar.DefaultStyles.GroupHeader.BackColor := clBtnFace;
  NavBar.DefaultStyles.GroupHeader.BackColor2 := clBtnFace;
  NavBar.DefaultStyles.GroupHeader.Font.Color := clBtnText;
  NavBar.DefaultStyles.GroupHeader.Font.Style := [fsBold];
end;

procedure TdxNavBarExplorerBarViewInfo.AssignDefaultGroupHeaderActiveStyle;
begin
  NavBar.DefaultStyles.GroupHeaderActive.Assign(NavBar.DefaultStyles.GroupHeader);
  NavBar.DefaultStyles.GroupHeaderActive.BackColor := clActiveCaption;
  NavBar.DefaultStyles.GroupHeaderActive.BackColor2 := clActiveCaption;
  NavBar.DefaultStyles.GroupHeaderActive.Font.Color := clCaptionText;
end;

procedure TdxNavBarExplorerBarViewInfo.AssignDefaultItemStyle;
begin
  NavBar.DefaultStyles.Item.ResetValues;
  NavBar.DefaultStyles.Item.Font.Color := clWindowText;
  NavBar.DefaultStyles.Item.HAlignment := haCenter;
end;

procedure TdxNavBarExplorerBarViewInfo.AssignDefaultItemDisabledStyle;
begin
  NavBar.DefaultStyles.ItemDisabled.Assign(NavBar.DefaultStyles.Item);
  NavBar.DefaultStyles.ItemDisabled.Font.Color := clGrayText;
end;

procedure TdxNavBarExplorerBarViewInfo.AssignDefaultItemHotTrackedStyle;
begin
  inherited AssignDefaultItemHotTrackedStyle;
  NavBar.DefaultStyles.ItemHotTracked.Font.Style := NavBar.DefaultStyles.ItemHotTracked.Font.Style + [fsUnderline];
end;

procedure TdxNavBarExplorerBarViewInfo.AssignDefaultItemPressedStyle;
begin
  inherited AssignDefaultItemPressedStyle;
  NavBar.DefaultStyles.ItemPressed.Font.Style := NavBar.DefaultStyles.ItemPressed.Font.Style + [fsUnderline];
end;

procedure TdxNavBarExplorerBarViewInfo.DoGroupActivate(AGroup: TdxNavBarGroup);
begin
  if AGroup.Expandable and not AGroup.Expanded then
  begin
    AGroup.Expanded := True;
    NavBar.DesignerModified;
  end;
end;

procedure TdxNavBarExplorerBarViewInfo.DoGroupDeactivate(AGroup: TdxNavBarGroup);
begin
  if AGroup.Expandable and AGroup.Expanded then
  begin
    AGroup.Expanded := False;
    NavBar.DesignerModified;
  end;
end;

function TdxNavBarExplorerBarViewInfo.IsGroupActive(AGroup: TdxNavBarGroup): Boolean;
begin
  Result := AGroup.Expanded;
end;

procedure TdxNavBarExplorerBarViewInfo.MakeLinkVisible(ALink: TdxNavBarItemLink; ATop: Boolean = True);
var
  AGroupViewInfo: TdxNavBarGroupViewInfo;
  ALinkViewInfo: TdxNavBarLinkViewInfo;
  ADisplacement: Integer;
begin
  if not ALink.Group.Expanded then
  begin
    ALink.Group.Expanded := True;
    CreateInfo;
    CalculateBounds;
  end;
  AGroupViewInfo := GetGroupViewInfoByGroup(ALink.Group);
  if AGroupViewInfo <> nil then
  begin
    ALinkViewInfo := AGroupViewInfo.GetLinkViewInfoByLink(ALink);
    if ALinkViewInfo <> nil then
    begin
      ADisplacement := 0;
      if ATop or (ALinkViewInfo.Rect.Top < 0) then
        ADisplacement := ALinkViewInfo.Rect.Top - 2
      else
        if ALinkViewInfo.Rect.Bottom > NavBar.ClientRect.Bottom then
          ADisplacement := ALinkViewInfo.Rect.Bottom - NavBar.ClientRect.Bottom;
      NavBar.ScrollBar.Position := NavBar.ScrollBar.Position + ADisplacement;
      NavBar.InvalidateAll(doRecreate);
    end;
  end;
end;

procedure TdxNavBarExplorerBarViewInfo.MakeGroupVisible(AGroup: TdxNavBarGroup;
  AExpandGroup: Boolean = True; ATop: Boolean = True);
var
  AGroupViewInfo: TdxNavBarGroupViewInfo;
  ADisplacement, AMakeVisibleBottomEdge: Integer;
begin
  if AExpandGroup and not AGroup.Expanded then
  begin
    AGroup.Expanded := True;
    NavBar.Groups.JustExpandedGroup := nil;
    CreateInfo;
    CalculateBounds;
  end;
  AGroupViewInfo := GetGroupViewInfoByGroup(AGroup);
  if AGroupViewInfo <> nil then
  begin
    ADisplacement := 0;
    if ATop or (AGroupViewInfo.CaptionRect.Top < 0) then
      ADisplacement := AGroupViewInfo.Rect.Top - 2
    else
    begin
      AMakeVisibleBottomEdge := AGroupViewInfo.CaptionRect.Bottom;
      if AMakeVisibleBottomEdge > NavBar.ClientRect.Bottom then
        ADisplacement := AMakeVisibleBottomEdge - NavBar.ClientRect.Bottom;
    end;
    NavBar.ScrollBar.Position := NavBar.ScrollBar.Position + ADisplacement;
    NavBar.InvalidateAll(doRecreate);
  end;
end;

function TdxNavBarExplorerBarViewInfo.GetGroupViewInfoIndexAtSplitterPos(const pt: TPoint): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to GroupCount - 1 do
    if ptInRect(TdxNavBarExplorerBarGroupViewInfo(Groups[I]).FSplitterRect, pt) then
    begin
      Result := I;
      Break;
    end;
end;

function TdxNavBarExplorerBarViewInfo.GetGroupViewInfoAtSplitterPos(const pt: TPoint): TdxNavBarGroupViewInfo;
var
  AIndex: Integer;
begin
  AIndex := GetGroupViewInfoIndexAtSplitterPos(pt);
  if AIndex > -1 then
    Result := Groups[AIndex]
  else
    Result := nil;
end;

procedure TdxNavBarExplorerBarViewInfo.CheckJustExpandedGroup;
var
  AGroupViewInfo: TdxNavBarGroupViewInfo;
  AMakeVisibleBottomEdge, ADisplacement: Integer;
begin
  if NavBar.Groups.JustExpandedGroup <> nil then
  begin
    AGroupViewInfo := GetGroupViewInfoByGroup(NavBar.Groups.JustExpandedGroup);
    if AGroupViewInfo <> nil then
      if (cxRectHeight(NavBar.ClientRect) - cxRectHeight(HeaderRect) < cxRectHeight(AGroupViewInfo.Rect)) or
        (AGroupViewInfo.Rect.Top - HeaderRect.Bottom - NavBar.ScrollBar.Position < GetSpaceBetweenGroups div 2) then
      begin
        ADisplacement := AGroupViewInfo.Rect.Top - HeaderRect.Bottom - GetSpaceBetweenGroups div 2;
        NavBar.ScrollBar.Position := ADisplacement;
      end
      else
      begin
        AMakeVisibleBottomEdge := AGroupViewInfo.Rect.Bottom - HeaderRect.Bottom + GetSpaceBetweenGroups div 2;
        if AMakeVisibleBottomEdge - NavBar.ScrollBar.Position > NavBar.ClientRect.Bottom then
        begin
          ADisplacement := AMakeVisibleBottomEdge - NavBar.ClientRect.Bottom;
          NavBar.ScrollBar.Position := ADisplacement;
        end;
      end;
    NavBar.Groups.JustExpandedGroup := nil;
  end;
end;

function TdxNavBarExplorerBarViewInfo.IsEnoughSpaceForScrollBar: Boolean;
begin
  Result := NavBar.ScrollBar.Width < NavBar.Width;
end;

{ TdxNavBarExplorerBarController }

destructor TdxNavBarExplorerBarController.Destroy;
begin
  DoSplitterCancelDrag;
  inherited;
end;

function TdxNavBarExplorerBarController.GetPartAtPos(const APoint: TPoint): TdxNavBarPart;
begin
  Result.MinorPartIndex := TdxNavBarExplorerBarViewInfo(ViewInfo).GetGroupViewInfoIndexAtSplitterPos(APoint);
  if Result.MinorPartIndex <> nbNone then
    Result.MajorPartIndex := ebSplitter
  else
    Result := inherited GetPartAtPos(APoint);
end;

function TdxNavBarExplorerBarController.NeedResetPressedPartOnMouseUp: Boolean;
begin
  Result := True;
end;

procedure TdxNavBarExplorerBarController.DoMouseDown(AButton: TMouseButton; AShift: TShiftState; const APoint: TPoint);
begin
  if (ssLeft in AShift) then
  begin
    PressedPart := GetPartAtPos(APoint);
    case PressedPart.MajorPartIndex of
      ebSplitter: DoSplitterStartDrag(APoint);
    else {nbNone}
      inherited;
    end;
  end
  else
    inherited;
end;

procedure TdxNavBarExplorerBarController.DoMouseMove(AShift: TShiftState; const APoint: TPoint);
begin
  if PressedPart.MajorPartIndex = ebSplitter then
    DoSplitterDrag(APoint)
  else
  begin
    HotPart := GetPartAtPos(APoint);
    inherited;
  end;
end;

procedure TdxNavBarExplorerBarController.DoMouseUp(AButton: TMouseButton; AShift: TShiftState; const APoint: TPoint);
begin
  HotPart := GetPartAtPos(APoint);
  case PressedPart.MajorPartIndex of
    ebSplitter: DoSplitterEndDrag(APoint);
  else
    inherited;
  end;
  if NeedResetPressedPartOnMouseUp then
    PressedPart := dxNavBarPart(nbNone);
end;

function TdxNavBarExplorerBarController.GetCursor: HIcon;
begin
  if GetPartAtPos(NavBar.ScreenToClient(GetMouseCursorPos)).MajorPartIndex = ebSplitter then
    Result := Screen.Cursors[crSizeNS]
  else
    Result := inherited GetCursor;
end;

procedure TdxNavBarExplorerBarController.DoSplitterStartDrag(const APoint: TPoint);
begin
  FStartDragHeight := GetSizedGroup.Control.OriginalHeight;
  FStartDragPoint := APoint;
  SetCaptureControl(NavBar);
end;

procedure TdxNavBarExplorerBarController.DoSplitterDrag(const APoint: TPoint);
var
  AdY, AdPosition: Integer;
begin
  AdY := APoint.Y - FStartDragPoint.Y;
  if TdxNavBarExplorerBarGroupViewInfo(GetSizedGroup).Align = vaBottom then
  begin
    AdY := -AdY;
    AdPosition := GetSizedGroup.Control.OriginalHeight -(FStartDragHeight + AdY);

    if FStartDragHeight + AdY > 0 then
    begin
      GetSizedGroup.Control.OriginalHeight := FStartDragHeight + AdY;
      if NavBar.ScrollBar.Visible {and (AdPosition < 0)} then
        NavBar.ScrollBar.Position := NavBar.ScrollBar.Position - AdPosition;
    end;
  end
  else
    GetSizedGroup.Control.OriginalHeight := FStartDragHeight + AdY;
  NavBar.Update;
end;

procedure TdxNavBarExplorerBarController.DoSplitterEndDrag(const APoint: TPoint);
begin
  DoSplitterCancelDrag;
end;

procedure TdxNavBarExplorerBarController.DoSplitterCancelDrag;
begin
  SetCaptureControl(nil);
end;

function TdxNavBarExplorerBarController.GetSizedGroup: TdxNavBarGroupViewInfo;
begin
  Result := ViewInfo.Groups[PressedPart.MinorPartIndex];
end;

{ TdxNavBarExplorerBarPainter }

procedure TdxNavBarExplorerBarPainter.DrawGroup(AGroupViewInfo: TdxNavBarGroupViewInfo);
begin
  inherited;
  if not IsRectEmpty(TdxNavBarExplorerBarGroupViewInfo(AGroupViewInfo).FSplitterRect) then
    DrawGroupControlSplitter(TdxNavBarExplorerBarGroupViewInfo(AGroupViewInfo));
end;

procedure TdxNavBarExplorerBarPainter.DrawGroupControlSplitter(AGroupViewInfo: TdxNavBarExplorerBarGroupViewInfo);
begin
// do nothing
end;

class function TdxNavBarExplorerBarPainter.GetCategories: TdxNavBarViewCategories;
begin
  Result := [nbvcExplorerBar];
end;

class function TdxNavBarExplorerBarPainter.GetViewInfoClass: TdxNavBarViewInfoClass;
begin
  Result := TdxNavBarExplorerBarViewInfo;
end;

class function TdxNavBarExplorerBarPainter.GetGroupViewInfoClass: TdxNavBarGroupViewInfoClass;
begin
  Result := TdxNavBarExplorerBarGroupViewInfo;
end;

class function TdxNavBarExplorerBarPainter.GetLinkViewInfoClass: TdxNavBarLinkViewInfoClass;
begin
  Result := TdxNavBarExplorerBarLinkViewInfo;
end;

function TdxNavBarExplorerBarPainter.GetControllerClass: TdxNavBarControllerClass;
begin
  Result := TdxNavBarExplorerBarController;
end;

class function TdxNavBarExplorerBarPainter.GroupBorderPainterClass: TdxNavBarCustomGroupBorderPainterClass;
begin
  Result := TdxNavBarExplorerBarBorderPainter;
end;

class function TdxNavBarExplorerBarPainter.SignPainterClass: TdxNavBarCustomSignPainterClass;
begin
  Result := TdxNavBarExplorerBarSignPainter;
end;

function TdxNavBarExplorerBarPainter.NeedScrollBar: Boolean;
begin
  Result := True;
end;

{ TdxNavBarUltraFlatExplorerBarPainter }

class function TdxNavBarUltraFlatExplorerBarPainter.SelectionPainterClass: TdxNavBarCustomSelectionPainterClass;
begin
  Result := TdxNavBarUltraFlatSelectionPainter;
end;

{ TdxNavBarAdvExplorerBarGroupViewInfo }

function TdxNavBarAdvExplorerBarGroupViewInfo.BorderColor: TColor;
begin
  if not IsDefaultCaptionColor then
    Result := inherited BorderColor
  else if BgBackColor <> clWhite then
    Result := clWhite
  else Result := LightLightColor(clHighlight);
end;

function TdxNavBarAdvExplorerBarGroupViewInfo.BgAlphaBlend: Byte;
begin
  if IsDefaultBgColor then
    Result := 255
  else Result := inherited BgAlphaBlend;
end;

function TdxNavBarAdvExplorerBarGroupViewInfo.BgAlphaBlend2: Byte;
begin
  if IsDefaultBgColor then
    Result := 255
  else Result := inherited BgAlphaBlend2;
end;

function TdxNavBarAdvExplorerBarGroupViewInfo.BgBackColor: TColor;
begin
  if IsDefaultBgColor then
    Result := dxAdvExplorerBarGroupBackgroundColor
  else
    Result := inherited BgBackColor;
end;

function TdxNavBarAdvExplorerBarGroupViewInfo.BgBackColor2: TColor;
begin
  if IsDefaultBgColor then
    Result := dxAdvExplorerBarGroupBackgroundColor
  else
    Result := inherited BgBackColor2;
end;

function TdxNavBarAdvExplorerBarGroupViewInfo.BgGradientMode: TdxBarStyleGradientMode;
begin
  if IsDefaultBgColor then
    Result := gmHorizontal
  else Result := inherited BgGradientMode;
end;

function TdxNavBarAdvExplorerBarGroupViewInfo.CaptionAlphaBlend: Byte;
begin
  if IsDefaultCaptionColor then
    Result := 255
  else Result := inherited CaptionAlphaBlend;
end;

function TdxNavBarAdvExplorerBarGroupViewInfo.CaptionAlphaBlend2: Byte;
begin
  if IsDefaultCaptionColor then
    Result := 255
  else Result := inherited CaptionAlphaBlend2;
end;

function TdxNavBarAdvExplorerBarGroupViewInfo.CaptionBackColor: TColor;
begin
  if not IsDefaultCaptionColor then
    Result := inherited CaptionBackColor
  else
    if sSpecial in State then
      Result := dxAdvExplorerBarSpecialGroupCaptionColor1
    else
      Result := dxAdvExplorerBarGroupCaptionColor1;
end;

function TdxNavBarAdvExplorerBarGroupViewInfo.CaptionBackColor2: TColor;
begin
  if not IsDefaultCaptionColor then
    Result := inherited CaptionBackColor2
  else
    if sSpecial in State then
      Result := dxAdvExplorerBarSpecialGroupCaptionColor2
    else
      Result := dxAdvExplorerBarGroupCaptionColor2;
end;

function TdxNavBarAdvExplorerBarGroupViewInfo.CaptionFontColor: TColor;
begin
  Result := CaptionFont.Color;
  if Result = clNone then
    if sSpecial in State then
      Result := GetColor(dxAdvExplorerBarSpecialGroupFontColor,
        dxAdvExplorerBarSpecialGroupFontHotColor, State)
    else
      Result := GetColor(dxAdvExplorerBarGroupFontColor,
        dxAdvExplorerBarGroupFontHotColor, State);
end;

function TdxNavBarAdvExplorerBarGroupViewInfo.CaptionSignColor: TColor;
begin
  if sSpecial in State then
  begin
    if not IsDefaultCaptionColor then
      Result := CaptionBackColor2
    else
      Result := GetColor(dxAdvExplorerBarSpecialGroupCaptionSignColor,
        dxAdvExplorerBarSpecialGroupCaptionSignHotColor, State)
  end
  else
  begin
    if CaptionFont.Color = clNone then
      Result := GetColor(dxAdvExplorerBarGroupCaptionSignColor,
        dxAdvExplorerBarGroupCaptionSignHotColor, State)
    else
      Result := CaptionFontColor;
  end;
end;

function TdxNavBarAdvExplorerBarGroupViewInfo.CaptionGradientMode: TdxBarStyleGradientMode;
begin
  if IsDefaultCaptionColor then
    Result := gmHorizontal
  else Result := inherited CaptionGradientMode;
end;

function TdxNavBarAdvExplorerBarGroupViewInfo.IsDefaultBgColor: Boolean;
begin
  Result := (inherited BgBackColor = clNone) or (inherited BgBackColor2 = clNone);
end;

function TdxNavBarAdvExplorerBarGroupViewInfo.IsDefaultCaptionColor: Boolean;
begin
  Result := (inherited CaptionBackColor = clNone) or (inherited CaptionBackColor2 = clNone);
end;

{ TdxNavAdvBarExplorerBarViewInfo }

function TdxNavBarAdvExplorerBarViewInfo.BgAlphaBlend: Byte;
begin
  if IsDefaultBgColor then
    Result := 255
  else Result := inherited BgAlphaBlend;
end;

function TdxNavBarAdvExplorerBarViewInfo.BgAlphaBlend2: Byte;
begin
  if IsDefaultBgColor then
    Result := 255
  else Result := inherited BgAlphaBlend2;
end;

function TdxNavBarAdvExplorerBarViewInfo.BgBackColor: TColor;
begin
  if IsDefaultBgColor then
    Result := dxAdvExplorerBarBackgroundColor1
  else
    Result := inherited BgBackColor;
end;

function TdxNavBarAdvExplorerBarViewInfo.BgBackColor2: TColor;
begin
  if IsDefaultBgColor then
    Result := dxAdvExplorerBarBackgroundColor2
  else
    Result := inherited BgBackColor2;
end;

function TdxNavBarAdvExplorerBarViewInfo.BgGradientMode: TdxBarStyleGradientMode;
begin
  if IsDefaultBgColor then
    Result := gmVertical//gmHorizontal
  else Result := inherited BgGradientMode;
end;

procedure TdxNavBarAdvExplorerBarViewInfo.AssignDefaultBackgroundStyle;
begin
  NavBar.DefaultStyles.Background.ResetValues;
  NavBar.DefaultStyles.Background.BackColor := clNone;
  NavBar.DefaultStyles.Background.BackColor2 := clNone;
end;

procedure TdxNavBarAdvExplorerBarViewInfo.AssignDefaultGroupBackgroundStyle;
begin
  NavBar.DefaultStyles.GroupBackground.ResetValues;
  NavBar.DefaultStyles.GroupBackground.BackColor := clNone;
  NavBar.DefaultStyles.GroupBackground.BackColor2 := clNone;
end;

procedure TdxNavBarAdvExplorerBarViewInfo.AssignDefaultGroupHeaderActiveStyle;
begin
  NavBar.DefaultStyles.GroupHeaderActive.Assign(NavBar.DefaultStyles.GroupHeader);
  NavBar.DefaultStyles.GroupHeaderActive.BackColor := clNone;
  NavBar.DefaultStyles.GroupHeaderActive.BackColor2 := clNone;
  NavBar.DefaultStyles.GroupHeaderActive.Font.Color := clNone;
end;

procedure TdxNavBarAdvExplorerBarViewInfo.AssignDefaultGroupHeaderStyle;
begin
  NavBar.DefaultStyles.GroupHeader.ResetValues;
  NavBar.DefaultStyles.GroupHeader.BackColor := clNone;
  NavBar.DefaultStyles.GroupHeader.BackColor2 := clNone;
  NavBar.DefaultStyles.GroupHeader.Font.Color := clNone;
  NavBar.DefaultStyles.GroupHeader.Font.Style := [fsBold];
end;

procedure TdxNavBarAdvExplorerBarViewInfo.AssignDefaultItemStyle;
begin
  NavBar.DefaultStyles.Item.ResetValues;
  NavBar.DefaultStyles.Item.Font.Color := dxAdvExplorerBarFontColor;
  NavBar.DefaultStyles.Item.HAlignment := haCenter;
end;

procedure TdxNavBarAdvExplorerBarViewInfo.AssignDefaultItemDisabledStyle;
begin
  NavBar.DefaultStyles.ItemDisabled.Assign(NavBar.DefaultStyles.Item);
  NavBar.DefaultStyles.ItemDisabled.Font.Color := LightLightColor(dxAdvExplorerBarFontColor);
end;

procedure TdxNavBarAdvExplorerBarViewInfo.AssignDefaultItemHotTrackedStyle;
begin
  inherited AssignDefaultItemHotTrackedStyle;
  NavBar.DefaultStyles.ItemHotTracked.Font.Color := dxAdvExplorerBarFontHotColor;
end;

procedure TdxNavBarAdvExplorerBarViewInfo.AssignDefaultItemPressedStyle;
begin
  inherited AssignDefaultItemPressedStyle;
  NavBar.DefaultStyles.ItemPressed.Font.Color := dxAdvExplorerBarFontHotColor;
end;

procedure TdxNavBarAdvExplorerBarViewInfo.CreateColors;
begin
  CreateAdvExplorerBarColors;
end;

procedure TdxNavBarAdvExplorerBarViewInfo.RefreshColors;
begin
  RefreshAdvExplorerBarColors;
end;

procedure TdxNavBarAdvExplorerBarViewInfo.ReleaseColors;
begin
  ReleaseAdvExplorerBarColors;
end;

function TdxNavBarAdvExplorerBarViewInfo.GetGroupCaptionSignSize: TSize;
begin
  Result := ScaleFactor.Apply(cxSize(22, 22));
end;

function TdxNavBarAdvExplorerBarViewInfo.IsDefaultBgColor: Boolean;
begin
  Result := (inherited BgBackColor = clNone) or (inherited BgBackColor2 = clNone);
end;

{ TdxNavBarAdvExplorerBarPainter }

class function TdxNavBarAdvExplorerBarPainter.GetViewInfoClass: TdxNavBarViewInfoClass;
begin
  Result := TdxNavBarAdvExplorerBarViewInfo;
end;

class function TdxNavBarAdvExplorerBarPainter.GetGroupViewInfoClass: TdxNavBarGroupViewInfoClass;
begin
  Result := TdxNavBarAdvExplorerBarGroupViewInfo;
end;

class function TdxNavBarAdvExplorerBarPainter.GetLinkViewInfoClass: TdxNavBarLinkViewInfoClass;
begin
  Result := TdxNavBarAdvExplorerBarLinkViewInfo;
end;

class function TdxNavBarAdvExplorerBarPainter.ButtonPainterClass: TdxNavBarCustomButtonPainterClass;
begin
  Result := TdxNavBarAdvExplorerButtonPainter;
end;

class function TdxNavBarAdvExplorerBarPainter.SignPainterClass: TdxNavBarCustomSignPainterClass;
begin
  Result := TdxNavBarAdvExplorerBarSignPainter;
end;

{ TdxNavBarExplorerBarBorderPainter }

class procedure TdxNavBarExplorerBarBorderPainter.InternalDrawBorder(ACanvas: TCanvas;
  ARect: TRect; AColor: TColor; AWithCaption: Boolean);
var
  ABorders: TcxBorders;
begin
  ABorders := cxBordersAll;
  if AWithCaption then
    Exclude(ABorders, bTop);
  with TcxCanvas.Create(ACanvas) do
  begin
    DrawComplexFrame(ARect, AColor, AColor, ABorders);
    Free;
  end;
end;

{ TdxNavBarAdvExplorerButtonPainter }

class procedure TdxNavBarAdvExplorerButtonPainter.InternalDrawButton(ACanvas: TCanvas; ARect: TRect;
    APicture: TPicture; AColor1, AColor2: TColor; AAlphaBlend1, AAlphaBlend2: Byte;
    AGradientMode: TdxBarStyleGradientMode; ABorderColor: TColor; AState: TdxNavBarObjectStates);
begin
  cxPaintCanvas.BeginPaint(ACanvas);
  try
    ExcludeClipRect(ACanvas.Handle, ARect.Left, ARect.Top, ARect.Left + 2, ARect.Top + 1);
    ExcludeClipRect(ACanvas.Handle, ARect.Left, ARect.Top, ARect.Left + 1, ARect.Top + 2);
    ExcludeClipRect(ACanvas.Handle, ARect.Right - 2, ARect.Top, ARect.Right, ARect.Top + 1);
    ExcludeClipRect(ACanvas.Handle, ARect.Right - 1, ARect.Top, ARect.Right, ARect.Top + 2);
    inherited;
  finally
    cxPaintCanvas.EndPaint;
  end;
end;

{ TdxNavBarExplorerBarSignPainter }

class function TdxNavBarExplorerBarSignPainter.CloneBitmap(ASource: TBitmap; const ATargetSize: TSize): TBitmap;
begin
  Result := TBitmap.Create;
  Result.SetSize(ATargetSize.cx, ATargetSize.cy);
  Result.Canvas.StretchDraw(cxGetImageClientRect(Result), ASource);
end;

class procedure TdxNavBarExplorerBarSignPainter.DrawSignSelection(ACanvas: TCanvas;
  ARect: TRect; AForeColor, ABackColor1, ABackColor2: TColor; AState: TdxNavBarObjectStates);
begin
  if sHotTracked in AState then
    DrawEdge(ACanvas.Handle, ARect, BDR_RAISEDINNER, BF_RECT);
end;

class procedure TdxNavBarExplorerBarSignPainter.InternalDrawSign(ACanvas: TCanvas;
  ARect: TRect; AScaleFactor: TdxScaleFactor; AForeColor, ABackColor1, ABackColor2: TColor;
  AState: TdxNavBarObjectStates);

  procedure BoldPolyline(const APoint1, APoint2, APoint3: TPoint; ADirection: Integer);
  begin
    ACanvas.Polyline([APoint2, APoint1, APoint3]);
    ACanvas.Polyline([cxPointOffset(APoint2, 1, 0), cxPointOffset(APoint1, 0, ADirection), cxPointOffset(APoint3, -1, 0)])
  end;

var
  ADirection: Integer;
  APoint1, APoint2, APoint3: TPoint;
  ASignSize: Integer;
begin
  DrawSignSelection(ACanvas, ARect, AForeColor, ABackColor1, ABackColor2, AState);

  ACanvas.Pen.Color := AForeColor;
  ACanvas.Pen.Style := psSolid;
  ACanvas.Pen.Width := 1;

  ADirection := IfThen(sExpanded in AState, 1, -1);
  ASignSize := AScaleFactor.Apply(4);

  APoint1 := cxRectCenter(ARect);
  APoint1 := Point(APoint1.X - 1, APoint1.Y + ADirection * -ASignSize);
  APoint2 := Point(APoint1.X - (2 * ASignSize - 1) div 2, APoint1.Y + ADirection * (ASignSize - 1));
  APoint3 := Point(APoint2.X + (2 * ASignSize - 1), APoint2.Y + ADirection);
  BoldPolyline(APoint1, APoint2, APoint3, ADirection);

  APoint1.Y := APoint1.Y + ADirection * ASignSize;
  APoint2.Y := APoint2.Y + ADirection * ASignSize;
  APoint3.Y := APoint3.Y + ADirection * ASignSize;
  BoldPolyline(APoint1, APoint2, APoint3, ADirection);
end;

{ TdxNavAdvBarExplorerBarSignPainter }

class procedure TdxNavBarAdvExplorerBarSignPainter.InternalDrawSign(
  ACanvas: TCanvas; ARect: TRect; AScaleFactor: TdxScaleFactor;
  AForeColor, ABackColor1, ABackColor2 : TColor; AState: TdxNavBarObjectStates);

  function GetSignBitmap: TBitmap;
  begin
    if sExpanded in AState then
      Result := dxAdvExplorerBarGroupCaptionCollapseSignBitmap
    else
      Result := dxAdvExplorerBarGroupCaptionExpandSignBitmap;
  end;

var
  ABitmap: TBitmap;
begin
  ABitmap := PrepareBitmap(ACanvas, ARect, GetSignBitmap, AForeColor, AState);
  try
    ACanvas.Draw(ARect.Left, ARect.Top, ABitmap);
  finally
    ABitmap.Free;
  end;
end;

class function TdxNavBarAdvExplorerBarSignPainter.PrepareBitmap(ACanvas: TCanvas;
  ARect: TRect; ABitmap: TBitmap; AForeColor: TColor; AState: TdxNavBarObjectStates): TBitmap;
var
  ABgColor: COLORREF;
  AMarksColor: COLORREF;
  APixelColor: COLORREF;
  AResultColors: TRGBColors;
  ATransparentColor: COLORREF;
  I, J: Integer;
begin
  Result := CloneBitmap(ABitmap, cxSize(ARect));
  GetBitmapBits(Result, AResultColors, True);
  ATransparentColor := dxRGBQuadToColor(AResultColors[0]);
  AMarksColor := clBlue;
  for I := 0 to Result.Width - 1 do
    for J := 0 to Result.Height - 1 do
    begin
      APixelColor := dxRGBQuadToColor(AResultColors[J * Result.Width + I]);
      if APixelColor <> ATransparentColor then
        if APixelColor = AMarksColor then
          AResultColors[J * Result.Width + I] := dxColorToRGBQuad(ColorToRGB(AForeColor))
        else
        begin
          ABgColor := ACanvas.Pixels[ARect.Left + I, ARect.Top + J];
          APixelColor := dxGetMiddleRGB(ABgColor, APixelColor, MulDiv((255 - GetRValue(APixelColor)), 100, 255));
          AResultColors[J * Result.Width + I] := dxColorToRGBQuad(APixelColor);
        end;
    end;

  SetBitmapBits(Result, AResultColors, True);
  Result.Transparent := True;
end;

initialization
  RegisterView(dxNavBarExplorerBarView, 'ExplorerBarView', TdxNavBarExplorerBarPainter);
  RegisterView(dxNavBarUltraFlatExplorerView, 'UltraFlatExplorerBarView', TdxNavBarUltraFlatExplorerBarPainter);
  RegisterView(dxNavBarAdvExplorerBarView, 'AdvExplorerBarView', TdxNavBarAdvExplorerBarPainter);

finalization
  UnRegisterView(dxNavBarExplorerBarView);
  UnRegisterView(dxNavBarUltraFlatExplorerView);
  UnRegisterView(dxNavBarAdvExplorerBarView);

end.
