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

unit dxSkinsdxNavBarAccordionViewPainter;

interface

{$I cxVer.inc}

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, Windows, Graphics, Classes, ImgList,
  dxCore, dxNavBar, dxNavBarBase, dxNavBarCollns, dxNavBarStyles, dxNavBarExplorerViews,
  dxNavBarGraphics, dxNavBarConsts, dxNavBarOffice11Views, dxNavBarOfficeViews,
  cxLookAndFeels, cxLookAndFeelPainters,cxGraphics, cxClasses, dxSkinInfo,
  dxNavBarSkinBasedViews, dxSkinsCore, dxNavBarCustomPainters;

type
  { TdxNavBarAccordionViewSkinPainterHelper }

  TdxNavBarAccordionViewSkinPainterHelper = class(TdxNavBarSkinBasedPainterHelper)
  private
    FLookAndFeel: TcxLookAndFeel;
  protected
    function GetSkinInfoClass: TdxSkinInfoClass; override;
    function GetSkinName: TdxSkinName; override;
    function GetSkinPainterData(var AData: TdxSkinInfo): Boolean; override;
    procedure LookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues);
    procedure PopulateSkinNames(AList: TStrings); override;
    procedure SetSkinName(AValue: TdxSkinName); override;
  public
    constructor Create(ASkinName: TdxSkinName); override;
    destructor Destroy; override;

    function NavBarBackground: TdxSkinElement; override;
    function NavBarDragDropItemTarget: TdxSkinElement; override;
    function NavBarDistanceBetweenRootGroups: Integer;
    function NavBarChildGroup: TdxSkinElement; virtual;
    function NavBarChildGroupExpandButton(AIsClose: Boolean): TdxSkinElement; override;
    function NavBarChildItemOffset: Integer;
    function NavBarGroupButtonCaption: TdxSkinElement; override;
    function NavBarGroupSigns (AIsClose: Boolean): TdxSkinElement; override;
    function NavBarItem: TdxSkinElement; override;
    function NavBarSeparator: TdxSkinElement; override;
    function NavPaneDoesGroupCaptionButtonRequireOffset: Boolean; override;
    function NavPanePopupControl: TdxSkinElement; override;
  end;

  { TdxNavBarAccordionViewInfo }

  TdxNavBarAccordionViewInfo = class(TdxNavBarExplorerBarViewInfo)
  protected
    function GetHelper: TdxNavBarAccordionViewSkinPainterHelper;
    function AllowExpandAnimation: Boolean; override;
    function CanSelectLinkByRect: Boolean; override;
    function GetItemCaptionOffsets: TRect; override;
    function GetChildItemOffset: Integer; override;
    function GetDefaultSmallImageWidth: Integer; override;
    function GetGroupBorderOffsets: TRect; override;
    function GetGroupCaptionHeightAddon: Integer; override;
    function GetGroupCaptionSignSize: TSize; override;
    function GetGroupEdges: TPoint; override;
    function GetGroupSeparatorWidth: Integer; override;
    function GetScrollContentForegroundColor: TColor; override;
  public
    procedure AssignDefaultGroupHeaderStyle; override;
    procedure AssignDefaultItemStyle; override;
    procedure AssignDefaultItemHotTrackedStyle; override;
    procedure AssignDefaultItemPressedStyle; override;
    procedure AssignDefaultChildGroupCaptionStyle; override;
    procedure CalculateSizeGripBounds; override;  // for internal use
  end;

  TdxNavBarAccordionGroupViewInfo = class(TdxNavBarSkinBasedExplorerBarGroupViewInfo)
  public
    function GetCaptionContentHeight: Integer; override;
  end;

  TdxNavBarAccordionChildGroupViewInfo = class(TdxNavBarChildGroupViewInfo)
  protected
    function GetExpandButtonOffset: TSize; override;
    function GetExpandButtonSize: TSize; override;
    function GetHelper: TdxNavBarAccordionViewSkinPainterHelper;
  public
    function SelectionRect: TRect; override;
  end;

  TdxNavBarAccordionLinkViewInfo = class(TdxNavBarSkinBasedExplorerBarLinkViewInfo)
  protected
    function GetHelper: TdxNavBarAccordionViewSkinPainterHelper;
  public
    function SelectionRect: TRect; override;
    function Style: TdxNavBarBaseStyle; override;
  end;

  { TdxNavBarAccordionViewPainter }

  TdxNavBarAccordionViewPainter = class(TdxNavBarSkinBasedExplorerBarPainter)
  private
    function GetLookAndFeel: TcxLookAndFeel;
    function GetSkinName: TdxSkinName;
    function GetSkinNameAssigned: Boolean;
    function IsSkinNameStored: Boolean;
    procedure SetLookAndFeel(AValue: TcxLookAndFeel);
    procedure SetSkinName(const AValue: TdxSkinName);
    procedure SetSkinNameAssigned(AValue: Boolean);
  protected
    class function GetChildGroupViewInfoClass: TdxNavBarChildGroupViewInfoClass; override;
    class function GetGroupViewInfoClass: TdxNavBarGroupViewInfoClass; override;
    class function GetLinkViewInfoClass: TdxNavBarLinkViewInfoClass; override;
    class function GetViewInfoClass: TdxNavBarViewInfoClass; override;
    class function GetSkinPainterHelperClass: TdxNavBarSkinBasedPainterHelperClass; override;
    class function SignPainterClass: TdxNavBarCustomSignPainterClass; override;
    procedure DoDrawSizeGrip(ACanvas: TcxCanvas; const ARect: TRect); override;
    procedure DrawGroupTopBorder(AGroupViewInfo: TdxNavBarGroupViewInfo);
    function GetMasterLookAndFeel: TcxLookAndFeel; override;
    procedure UpdateScrollBarLookAndFeel(ALookAndFeel: TcxLookAndFeel); override;
  public
    procedure Assign(Source: TPersistent); override;

    procedure DrawBackground; override;
    procedure DrawChildGroupExpandButton(AChildGroupViewInfo: TdxNavBarChildGroupViewInfo); override;
    procedure DrawChildGroupSelection(AChildGroupViewInfo: TdxNavBarChildGroupViewInfo); override;
    procedure DrawGroupBackground(AGroupViewInfo: TdxNavBarGroupViewInfo); override;
    procedure DrawGroupBorder(AGroupViewInfo: TdxNavBarGroupViewInfo); override;
    procedure DrawGroupControl(ACanvas: TCanvas; ARect: TRect;
      AGroupViewInfo: TdxNavBarGroupViewInfo); override;
    procedure DrawGroupControlSplitter(AGroupViewInfo: TdxNavBarExplorerBarGroupViewInfo); override;
    procedure DrawItemSelection(ALinkViewInfo: TdxNavBarLinkViewInfo); override;
    procedure DrawNavBarControl; override;
    procedure DrawSeparator(ALinkViewInfo: TdxNavBarLinkViewInfo); override;

    property LookAndFeel: TcxLookAndFeel read GetLookAndFeel write SetLookAndFeel;
  published
    property SkinName: TdxSkinName read GetSkinName write SetSkinName stored IsSkinNameStored;
    property SkinNameAssigned: Boolean read GetSkinNameAssigned write SetSkinNameAssigned default False;
  end;

implementation

uses
  dxNavBarViewsFact, Math, cxGeometry, SysUtils, dxSkinsStrs, dxDPIAwareUtils;

type
  TdxCustomNavBarAccess = class(TdxCustomNavBar);

procedure DrawElementPart(AElement: TdxSkinElement; ACanvas: TcxCanvas; ADrawRect, AClipRect: TRect);
begin
  if AElement = nil then Exit;
  ACanvas.SaveClipRegion;
  try
    ACanvas.SetClipRegion(TcxRegion.Create(AClipRect), roIntersect);
    AElement.Draw(ACanvas.Handle, ADrawRect);
  finally
    ACanvas.RestoreClipRegion;
  end;
end;

{ TdxNavBarAccordionViewSkinPainterHelper }

constructor TdxNavBarAccordionViewSkinPainterHelper.Create(ASkinName: TdxSkinName);
begin
  inherited Create(ASkinName);
  FLookAndFeel := TcxLookAndFeel.Create(nil);
  FLookAndFeel.NativeStyle := False;
  FLookAndFeel.OnChanged := LookAndFeelChanged;
end;

destructor TdxNavBarAccordionViewSkinPainterHelper.Destroy;
begin
  FreeAndNil(FLookAndFeel);
  inherited Destroy;
end;

function TdxNavBarAccordionViewSkinPainterHelper.NavBarBackground: TdxSkinElement;
var
  ASkinInfo: TdxSkinInfo;
begin
  Result := nil;
  if GetSkinPainterData(ASkinInfo) then
  begin
    Result := ASkinInfo.NavBarAccordionControlBackground;
    if Result = nil then
      Result := inherited NavBarBackground;
  end;
end;

function TdxNavBarAccordionViewSkinPainterHelper.NavBarDragDropItemTarget: TdxSkinElement;
begin
  Result := nil;
end;

function TdxNavBarAccordionViewSkinPainterHelper.NavBarDistanceBetweenRootGroups: Integer;
var
  ASkinInfo: TdxSkinInfo;
begin
  if GetSkinPainterData(ASkinInfo) and
    (ASkinInfo.NavBarAccordionControlDistanceBetweenRootGroups <> nil) then
    Result := ASkinInfo.NavBarAccordionControlDistanceBetweenRootGroups.Value
  else
    Result := -1;
end;

function TdxNavBarAccordionViewSkinPainterHelper.NavBarChildGroup: TdxSkinElement;
var
  ASkinInfo: TdxSkinInfo;
begin
  Result := nil;
  if GetSkinPainterData(ASkinInfo) then
    Result := ASkinInfo.NavBarAccordionControlGroup;
end;

function TdxNavBarAccordionViewSkinPainterHelper.NavBarChildGroupExpandButton(AIsClose: Boolean): TdxSkinElement;
var
  ASkinInfo: TdxSkinInfo;
begin
  Result := nil;
  if GetSkinPainterData(ASkinInfo) then
    if AIsClose then
      Result := ASkinInfo.NavBarAccordionControlGroupOpenButton
    else
      Result := ASkinInfo.NavBarAccordionControlGroupCloseButton;
end;

function TdxNavBarAccordionViewSkinPainterHelper.NavBarChildItemOffset: Integer;
var
  ASkinInfo: TdxSkinInfo;
begin
  if GetSkinPainterData(ASkinInfo) and
    (ASkinInfo.NavBarAccordionControlChildItemOffset <> nil) then
    Result := ASkinInfo.NavBarAccordionControlChildItemOffset.Value
  else
    Result := -1;
end;

function TdxNavBarAccordionViewSkinPainterHelper.NavBarGroupButtonCaption: TdxSkinElement;
var
  ASkinInfo: TdxSkinInfo;
begin
  Result := nil;
  if GetSkinPainterData(ASkinInfo) then
  begin
    Result := ASkinInfo.NavBarAccordionControlRootGroup;
    if Result = nil then
      Result := inherited NavBarGroupButtonCaption;
  end;
end;

function TdxNavBarAccordionViewSkinPainterHelper.NavBarGroupSigns(AIsClose: Boolean): TdxSkinElement;
var
  ASkinInfo: TdxSkinInfo;
begin
  Result := nil;
  if GetSkinPainterData(ASkinInfo) then
    if AIsClose then
      Result := ASkinInfo.NavBarAccordionControlRootGroupCloseButton
    else
      Result := ASkinInfo.NavBarAccordionControlRootGroupOpenButton;
end;

function TdxNavBarAccordionViewSkinPainterHelper.NavBarItem: TdxSkinElement;
var
  ASkinInfo: TdxSkinInfo;
begin
  Result := nil;
  if GetSkinPainterData(ASkinInfo) then
    Result := ASkinInfo.NavBarAccordionControlItem;
end;

function TdxNavBarAccordionViewSkinPainterHelper.NavBarSeparator: TdxSkinElement;
begin
  Result := nil;
end;

function TdxNavBarAccordionViewSkinPainterHelper.NavPaneDoesGroupCaptionButtonRequireOffset: Boolean;
var
  ABoolProperty: TdxSkinBooleanProperty;
  ASkinInfo: TdxSkinInfo;
begin
  if GetSkinPainterData(ASkinInfo) then
    ABoolProperty := ASkinInfo.NavPaneOffsetGroupBorders
  else
    ABoolProperty := nil;

  if ABoolProperty = nil then
    Result := inherited NavPaneDoesGroupCaptionButtonRequireOffset
  else
    Result := ABoolProperty.Value;
end;

function TdxNavBarAccordionViewSkinPainterHelper.NavPanePopupControl: TdxSkinElement;
var
  ASkinInfo: TdxSkinInfo;
begin
  if GetSkinPainterData(ASkinInfo) then
    Result := ASkinInfo.NavPaneFormBorder
  else
    Result := nil;
end;

procedure TdxNavBarAccordionViewSkinPainterHelper.PopulateSkinNames(AList: TStrings);
var
  APainter: TcxCustomLookAndFeelPainter;
  I: Integer;
begin
  for I := 0 to cxLookAndFeelPaintersManager.Count - 1 do
  begin
    APainter := cxLookAndFeelPaintersManager[I];
    if APainter.LookAndFeelStyle = lfsSkin then
      AList.Add(APainter.LookAndFeelName);
  end;
end;

function TdxNavBarAccordionViewSkinPainterHelper.GetSkinInfoClass: TdxSkinInfoClass;
begin
  Result := TdxSkinInfo;
end;

function TdxNavBarAccordionViewSkinPainterHelper.GetSkinName: TdxSkinName;
begin
  Result := FLookAndFeel.SkinName;
end;

function TdxNavBarAccordionViewSkinPainterHelper.GetSkinPainterData(var AData: TdxSkinInfo): Boolean;
begin
  Result := (FLookAndFeel.SkinPainter <> nil) and FLookAndFeel.SkinPainter.GetPainterData(AData);
end;

procedure TdxNavBarAccordionViewSkinPainterHelper.LookAndFeelChanged(
  Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues);
begin
  DoChanged;
end;

procedure TdxNavBarAccordionViewSkinPainterHelper.SetSkinName(AValue: TdxSkinName);
begin
  FLookAndFeel.SkinName := AValue;
end;

{ TdxNavBarAccordionViewInfo }

procedure TdxNavBarAccordionViewInfo.AssignDefaultChildGroupCaptionStyle;
var
  AElement: TdxSkinElement;
begin
  inherited AssignDefaultChildGroupCaptionStyle;
  AElement := GetHelper.NavBarChildGroup;
  if AElement <> nil then
    NavBar.DefaultStyles.ChildGroupCaption.Font.Color := AElement.TextColor;
end;

procedure TdxNavBarAccordionViewInfo.AssignDefaultGroupHeaderStyle;
var
  AElement: TdxSkinElement;
  AFontSize: Integer;
  AProperty: TdxSkinProperty;
begin
  inherited AssignDefaultGroupHeaderStyle;

  AElement := GetHelper.NavBarGroupButtonCaption;
  if AElement <> nil then
  begin
    NavBar.DefaultStyles.GroupHeader.Font.Color := AElement.TextColor;

    if AElement.GetPropertyByName(sdxFontSize, AProperty) then
      AFontSize := TdxSkinIntegerProperty(AProperty).Value
    else
      if AElement.GetPropertyByName(sdxCaptionFontDelta, AProperty) then
        AFontSize := 8 + TdxSkinIntegerProperty(AProperty).Value
      else
        AFontSize := -1;

    if AFontSize > 0 then
      NavBar.DefaultStyles.GroupHeader.Font.Height := -MulDiv(AFontSize, dxDefaultDPI, 72);

    if AElement.GetPropertyByName(sdxFontBold, AProperty) and (AProperty is TdxSkinBooleanProperty) and
      TdxSkinBooleanProperty(AProperty).Value then
      NavBar.DefaultStyles.GroupHeader.Font.Style := [fsBold]
    else
      NavBar.DefaultStyles.GroupHeader.Font.Style := [];
  end;
end;

procedure TdxNavBarAccordionViewInfo.AssignDefaultItemHotTrackedStyle;
var
  AElement: TdxSkinElement;
begin
  inherited AssignDefaultItemHotTrackedStyle;
  AElement := GetHelper.NavBarItem;
  if AElement <> nil then
    NavBar.DefaultStyles.ItemHotTracked.Font.Color := AElement.GetTextColor(cxbsHot);
  NavBar.DefaultStyles.ItemHotTracked.Font.Style := NavBar.DefaultStyles.ItemHotTracked.Font.Style - [fsUnderline];
end;

procedure TdxNavBarAccordionViewInfo.AssignDefaultItemPressedStyle;
var
  AElement: TdxSkinElement;
begin
  inherited AssignDefaultItemPressedStyle;
  AElement := GetHelper.NavBarItem;
  if AElement <> nil then
    NavBar.DefaultStyles.ItemPressed.Font.Color := AElement.GetTextColor(cxbsPressed);
  NavBar.DefaultStyles.ItemPressed.Font.Style := NavBar.DefaultStyles.ItemPressed.Font.Style - [fsUnderline];
end;

procedure TdxNavBarAccordionViewInfo.AssignDefaultItemStyle;
var
  AElement: TdxSkinElement;
begin
  inherited AssignDefaultItemStyle;
  AElement := GetHelper.NavBarItem;
  if AElement <> nil then
    NavBar.DefaultStyles.Item.Font.Color := AElement.TextColor;
end;

function TdxNavBarAccordionViewInfo.GetHelper: TdxNavBarAccordionViewSkinPainterHelper;
begin
  Result := TdxNavBarAccordionViewSkinPainterHelper(TdxNavBarAccordionViewPainter(Painter).FSkinBasedPainterHelper);
end;

function TdxNavBarAccordionViewInfo.AllowExpandAnimation: Boolean;
var
  ASkinInfo: TdxSkinInfo;
begin
  Result := GetHelper.GetSkinPainterData(ASkinInfo);
end;

function TdxNavBarAccordionViewInfo.CanSelectLinkByRect: Boolean;
begin
  Result := True;
end;

function TdxNavBarAccordionViewInfo.GetItemCaptionOffsets: TRect;
begin
  if GetHelper.NavBarItem <> nil then
    Result := GetHelper.NavBarItem.ContentOffset.Rect
  else
    Result := inherited GetItemCaptionOffsets;
end;

function TdxNavBarAccordionViewInfo.GetChildItemOffset: Integer;
begin
  if GetHelper.NavBarChildItemOffset > 0 then
    Result := GetHelper.NavBarChildItemOffset
  else
    Result := inherited GetChildItemOffset;
end;

function TdxNavBarAccordionViewInfo.GetDefaultSmallImageWidth: Integer;
begin
  Result := 0;
end;

function TdxNavBarAccordionViewInfo.GetGroupBorderOffsets: TRect;
begin
  if TdxNavBarAccordionViewPainter(Painter).IsSkinAvailable then
    Result := GetSkinElementOffsets(GetHelper.NavBarGroupClient, ScaleFactor)
  else
    Result := inherited GetGroupBorderOffsets;
end;

function TdxNavBarAccordionViewInfo.GetGroupCaptionHeightAddon: Integer;
const
  ExpandCollapseButtonIndent = 3;
begin
  if GetHelper.NavBarGroupButtonCaption <> nil then
    Result := ScaleFactor.Apply(cxMarginsHeight(GetHelper.NavBarGroupButtonCaption.ContentOffset.Rect) + ExpandCollapseButtonIndent)
  else
    Result := inherited GetGroupCaptionHeightAddon;
end;

function TdxNavBarAccordionViewInfo.GetGroupCaptionSignSize: TSize;
var
  AElement: TdxSkinElement;
begin
  AElement := GetHelper.NavBarGroupSigns(True);
  if AElement <> nil then
  begin
    Result := dxSkinGetElementSize(AElement, ScaleFactor);
    Inc(Result.cx, ScaleFactor.Apply(cxMarginsWidth(AElement.ContentOffset.Rect)));
    Inc(Result.cy, ScaleFactor.Apply(cxMarginsHeight(AElement.ContentOffset.Rect)));
  end
  else
    Result := inherited GetGroupCaptionSignSize;
end;

function TdxNavBarAccordionViewInfo.GetGroupEdges: TPoint;
begin
  Result := ScaleFactor.Apply(cxSimplePoint);
end;

function TdxNavBarAccordionViewInfo.GetGroupSeparatorWidth: Integer;
begin
  Result := GetHelper.NavBarDistanceBetweenRootGroups;
  if Result = -1 then
    Result := inherited GetGroupSeparatorWidth
  else
    Result := ScaleFactor.Apply(Result);
end;

function TdxNavBarAccordionViewInfo.GetScrollContentForegroundColor: TColor;
var
  AElement: TdxSkinElement;
begin
  AElement := GetHelper.NavBarItem;
  if AElement <> nil then
    Result := AElement.TextColor
  else
    Result := inherited GetScrollContentForegroundColor;
end;

procedure TdxNavBarAccordionViewInfo.CalculateSizeGripBounds;
var
  AOffsetSign: Integer;
begin
  inherited CalculateSizeGripBounds;
  if IsInternal and NavBar.ScrollBar.Visible and not TdxCustomNavBarAccess(NavBar).IsScrollBarUseClientArea then
  begin
    if NavBar.UseRightToLeftScrollBar then
      AOffsetSign := IfThen(GetExpandDirection = dirRight, 0, -1)
    else
      AOffsetSign := IfThen(GetExpandDirection = dirRight, 1, 0);
    FSizeGripRect := cxRectOffsetHorz(FSizeGripRect, AOffsetSign * NavBar.ScrollBar.Width);
  end;
end;

{ TdxNavBarAccordionGroupViewInfo }

function TdxNavBarAccordionGroupViewInfo.GetCaptionContentHeight: Integer;
begin
  Result := Max(inherited GetCaptionContentHeight, (ViewInfo as TdxNavBarAccordionViewInfo).GetGroupCaptionSignSize.cy);
end;

{ TdxNavBarAccordionChildGroupViewInfo }

function TdxNavBarAccordionChildGroupViewInfo.SelectionRect: TRect;
begin
  Result := CaptionRect;
end;

function TdxNavBarAccordionChildGroupViewInfo.GetExpandButtonOffset: TSize;
var
  AOffsetX: TdxSkinIntegerProperty;
begin
  Result := cxNullSize;
  if GetHelper.NavBarChildGroupExpandButton(True) <> nil then
  begin
    AOffsetX := GetHelper.NavBarChildGroupExpandButton(True).GetPropertyByName(sdxOffsetX) as TdxSkinIntegerProperty;
    if AOffsetX <> nil then
      Result.cx := AOffsetX.Value;
  end;
end;

function TdxNavBarAccordionChildGroupViewInfo.GetExpandButtonSize: TSize;
begin
  if GetHelper.NavBarChildGroupExpandButton(True) <> nil then
    Result := dxSkinGetElementSize(GetHelper.NavBarChildGroupExpandButton(True), ScaleFactor)
  else
    Result := inherited GetExpandButtonSize;
end;

function TdxNavBarAccordionChildGroupViewInfo.GetHelper: TdxNavBarAccordionViewSkinPainterHelper;
begin
  Result := TdxNavBarAccordionViewSkinPainterHelper(TdxNavBarAccordionViewPainter(Painter).FSkinBasedPainterHelper);
end;

{ TdxNavBarAccordionLinkViewInfo }

function TdxNavBarAccordionLinkViewInfo.SelectionRect: TRect;
begin
  Result := Rect;
end;

function TdxNavBarAccordionLinkViewInfo.Style: TdxNavBarBaseStyle;
begin
  if sDisabled in State then
    Result := NavBar.DefaultStyles.ItemDisabled
  else
    if  State * [sPressed, sSelected] <> [] then
      Result := NavBar.DefaultStyles.ItemPressed
    else
      Result := inherited Style;
end;

function TdxNavBarAccordionLinkViewInfo.GetHelper: TdxNavBarAccordionViewSkinPainterHelper;
begin
  Result := TdxNavBarAccordionViewSkinPainterHelper(TdxNavBarAccordionViewPainter(Painter).FSkinBasedPainterHelper);
end;

{ TdxNavBarAccordionViewPainter }

procedure TdxNavBarAccordionViewPainter.Assign(Source: TPersistent);
begin
  if Source is TdxNavBarAccordionViewPainter then
    LookAndFeel := TdxNavBarAccordionViewPainter(Source).LookAndFeel
  else
    inherited;
end;

procedure TdxNavBarAccordionViewPainter.DrawBackground;

  function IsAnimationPrepare: Boolean;
  begin
    Result := cxCanvas <> NavBar.Canvas;
  end;

var
  R: TRect;
begin
  if FSkinBasedPainterHelper.NavBarBackground <> nil then
  begin
    R := NavBar.ClientRect;
    if IsAnimationPrepare then
    begin
      R.Top := ViewInfo.Groups[0].Rect.Top;
      R.Bottom := ViewInfo.Groups[ViewInfo.GroupCount - 1].Rect.Bottom;
    end;
    cxCanvas.FillRect(R, FSkinBasedPainterHelper.NavBarBackground.Color)
  end
  else
    inherited;
end;

procedure TdxNavBarAccordionViewPainter.DrawChildGroupExpandButton(AChildGroupViewInfo: TdxNavBarChildGroupViewInfo);
var
  AState: TdxSkinElementState;
begin
  AState := NavBarObjectStateToSkinState(AChildGroupViewInfo.State);
  if not DrawSkinElement(FSkinBasedPainterHelper.NavBarChildGroupExpandButton(not (sExpanded in AChildGroupViewInfo.State)),
    Canvas, AChildGroupViewInfo.ExpandButtonRect, ScaleFactor, 0, AState)
  then
    inherited;
end;

procedure TdxNavBarAccordionViewPainter.DrawChildGroupSelection(AChildGroupViewInfo: TdxNavBarChildGroupViewInfo);
var
  AState: TdxSkinElementState;
begin
  AState := NavBarObjectStateToSkinState(AChildGroupViewInfo.State);
  if not DrawSkinElement(FSkinBasedPainterHelper.NavBarItem, Canvas, AChildGroupViewInfo.SelectionRect, ScaleFactor, 0, AState) then
    inherited;
end;

procedure TdxNavBarAccordionViewPainter.DrawGroupBackground(AGroupViewInfo: TdxNavBarGroupViewInfo);
begin
  if FSkinBasedPainterHelper.NavBarBackground <> nil then
  begin
    cxCanvas.SaveClipRegion;
    try
      cxCanvas.IntersectClipRect(AGroupViewInfo.Rect);
      cxCanvas.FillRect(AGroupViewInfo.Rect, FSkinBasedPainterHelper.NavBarBackground.Color);
    finally
      cxCanvas.RestoreClipRegion;
    end;
  end
  else
    inherited;
end;

procedure TdxNavBarAccordionViewPainter.DrawGroupBorder(AGroupViewInfo: TdxNavBarGroupViewInfo);
begin
  // do nothing
end;

procedure TdxNavBarAccordionViewPainter.DrawGroupTopBorder(
  AGroupViewInfo: TdxNavBarGroupViewInfo);
var
  AElement: TdxSkinElement;
  R: TRect;
begin
  AElement := FSkinBasedPainterHelper.NavBarGroupButtonCaption;
  if AElement <> nil then
  begin
    cxCanvas.SaveClipRegion;
    try
      R := AGroupViewInfo.ItemsRect;
      cxCanvas.SetClipRegion(TcxRegion.Create(cxRectSetHeight(R, 1)), roSet);
      R.Bottom := R.Top + 1;
      Dec(R.Top, AElement.Size.cy);
      AElement.Draw(cxCanvas.Handle, R);
    finally
      cxCanvas.RestoreClipRegion;
    end;
  end;
end;

procedure TdxNavBarAccordionViewPainter.DrawGroupControl(ACanvas: TCanvas;
  ARect: TRect; AGroupViewInfo: TdxNavBarGroupViewInfo);
var
  AcxCanvas: TcxCanvas;
begin
  if FSkinBasedPainterHelper.NavBarBackground <> nil then
  begin
    AcxCanvas := TcxCanvas.Create(ACanvas);
    AcxCanvas.SaveClipRegion;
    try
      AcxCanvas.SetClipRegion(TcxRegion.Create(ARect), roIntersect);
      AcxCanvas.FillRect(ARect, FSkinBasedPainterHelper.NavBarBackground.Color);
    finally
      AcxCanvas.RestoreClipRegion;
      AcxCanvas.Free;
    end;
  end
  else
    inherited;
end;

procedure TdxNavBarAccordionViewPainter.DrawGroupControlSplitter(AGroupViewInfo: TdxNavBarExplorerBarGroupViewInfo);
begin
//do nothing
end;

procedure TdxNavBarAccordionViewPainter.DrawItemSelection(ALinkViewInfo: TdxNavBarLinkViewInfo);
var
  AState: TdxSkinElementState;
begin
  AState := NavBarObjectStateToSkinState(ALinkViewInfo.State);
  if not DrawSkinElement(FSkinBasedPainterHelper.NavBarItem, Canvas, ALinkViewInfo.SelectionRect, ScaleFactor, 0, AState) then
    inherited;
end;

procedure TdxNavBarAccordionViewPainter.DrawNavBarControl;
begin
  inherited DrawNavBarControl;
  InternalDrawSizeGrip(cxCanvas);
end;

procedure TdxNavBarAccordionViewPainter.DrawSeparator(ALinkViewInfo: TdxNavBarLinkViewInfo);
var
  AClipRect, ADrawRect: TRect;
begin
  if (FSkinBasedPainterHelper.NavPaneHeader <> nil) and (FSkinBasedPainterHelper.NavPaneCaptionHeight <> nil) then
  begin
    AClipRect := ALinkViewInfo.SeparatorRect;
    ADrawRect := cxRectSetBottom(AClipRect, AClipRect.Bottom, FSkinBasedPainterHelper.NavPaneCaptionHeight.Value);
    DrawElementPart(FSkinBasedPainterHelper.NavPaneHeader, cxCanvas, ADrawRect, AClipRect);
  end
  else
    inherited;
end;

class function TdxNavBarAccordionViewPainter.GetChildGroupViewInfoClass: TdxNavBarChildGroupViewInfoClass;
begin
  Result := TdxNavBarAccordionChildGroupViewInfo;
end;

class function TdxNavBarAccordionViewPainter.GetGroupViewInfoClass: TdxNavBarGroupViewInfoClass;
begin
  Result := TdxNavBarAccordionGroupViewInfo;
end;

class function TdxNavBarAccordionViewPainter.GetLinkViewInfoClass: TdxNavBarLinkViewInfoClass;
begin
  Result := TdxNavBarAccordionLinkViewInfo;
end;

class function TdxNavBarAccordionViewPainter.GetViewInfoClass: TdxNavBarViewInfoClass;
begin
  Result := TdxNavBarAccordionViewInfo;
end;

class function TdxNavBarAccordionViewPainter.GetSkinPainterHelperClass: TdxNavBarSkinBasedPainterHelperClass;
begin
  Result := TdxNavBarAccordionViewSkinPainterHelper;
end;

class function TdxNavBarAccordionViewPainter.SignPainterClass: TdxNavBarCustomSignPainterClass;
begin
  Result := TdxNavBarExplorerBarSignPainter;
end;

procedure TdxNavBarAccordionViewPainter.DoDrawSizeGrip(ACanvas: TcxCanvas; const ARect: TRect);
begin
  if not DrawSkinElement(FSkinBasedPainterHelper.NavPaneSizeGrip, ACanvas.Canvas, ARect, ScaleFactor) then
    inherited;
end;

function TdxNavBarAccordionViewPainter.GetMasterLookAndFeel: TcxLookAndFeel;
begin
  Result := LookAndFeel;
end;

procedure TdxNavBarAccordionViewPainter.UpdateScrollBarLookAndFeel(ALookAndFeel: TcxLookAndFeel);
begin
  ALookAndFeel.AssignedValues := [];
  ALookAndFeel.MasterLookAndFeel := LookAndFeel;
end;

function TdxNavBarAccordionViewPainter.IsSkinNameStored: Boolean;
begin
  Result := SkinNameAssigned;
end;

procedure TdxNavBarAccordionViewPainter.SetLookAndFeel(AValue: TcxLookAndFeel);
begin
  LookAndFeel.Assign(AValue);
end;

procedure TdxNavBarAccordionViewPainter.SetSkinName(const AValue: TdxSkinName);
begin
  ColorSchemeName := AValue;
end;

function TdxNavBarAccordionViewPainter.GetLookAndFeel: TcxLookAndFeel;
begin
  Result := TdxNavBarAccordionViewSkinPainterHelper(FSkinBasedPainterHelper).FLookAndFeel;
end;

function TdxNavBarAccordionViewPainter.GetSkinName: TdxSkinName;
begin
  Result := ColorSchemeName;
end;

function TdxNavBarAccordionViewPainter.GetSkinNameAssigned: Boolean;
begin
  Result := lfvSkinName in LookAndFeel.AssignedValues;
end;

procedure TdxNavBarAccordionViewPainter.SetSkinNameAssigned(AValue: Boolean);
begin
  if AValue then
    LookAndFeel.AssignedValues := LookAndFeel.AssignedValues + [lfvSkinName]
  else
    LookAndFeel.AssignedValues := LookAndFeel.AssignedValues - [lfvSkinName];
end;

initialization
  dxNavBarViewsFactory.RegisterView(dxNavBarAccordionView, 'AccordionView', TdxNavBarAccordionViewPainter);

finalization
  UnRegisterView(dxNavBarAccordionView);

end.
