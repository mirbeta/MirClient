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

unit dxSkinsdxNavBarPainter;

{$I cxVer.Inc}

interface

uses
  Types, Windows, Graphics, Classes, ImgList,
  dxCore, dxNavBar, dxNavBarBase, dxNavBarCollns, dxNavBarStyles, dxNavBarExplorerViews,
  dxNavBarGraphics, dxNavBarConsts, dxNavBarOffice11Views, dxNavBarOfficeViews,
  cxLookAndFeels, cxLookAndFeelPainters,cxGraphics, cxClasses, dxSkinInfo,
  dxNavBarSkinBasedViews, dxSkinsCore, dxNavBarCustomPainters, dxCoreGraphics;

type

  { TdxNavBarSkinPainterHelper }

  TdxNavBarSkinPainterHelper = class(TdxNavBarSkinBasedPainterHelper)
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

    function NavBarDragDropItemTarget: TdxSkinElement; override;
    function NavBarSeparator: TdxSkinElement; override;
    function NavPaneDoesGroupCaptionButtonRequireOffset: Boolean; override;
    function NavPanePopupControl: TdxSkinElement; override;
  end;

  { TdxNavBarSkinNavPaneLinkViewInfo }

  TdxNavBarSkinNavPaneLinkViewInfo = class(TdxNavBarSkinBasedNavPaneLinkViewInfo)
  public
    function SelectionRect: TRect; override;
    function SeparatorRect: TRect; override;
  end;

  { TdxNavBarSkinNavPaneGroupViewInfo }

  TdxNavBarSkinNavPaneGroupViewInfo = class(TdxNavBarSkinBasedNavPaneGroupViewInfo);

  { TdxNavBarSkinOverflowPanelViewInfo }

  TdxNavBarSkinOverflowPanelViewInfo = class(TdxNavBarSkinBasedOverflowPanelViewInfo)
  private
    function GetMinHeight: Integer;
  protected
    function GetHeight: Integer; override;
    function GetImageWidthAddon: Integer; override;
    function GetSignWidth: Integer; override;
  end;

  { TdxNavBarSkinNavPaneViewInfo }

  TdxNavBarSkinNavPaneViewInfo = class(TdxNavBarSkinBasedNavPaneViewInfo)
  protected
    function GetGroupBorderOffsets: TRect; override;
    function GetGroupCaptionHeightAddon: Integer; override;
    function GetGroupCaptionImageOffsets: TRect; override;
    function GetGroupCaptionSignSize: TSize; override;
    function GetGroupHeaderTextIndent: Integer; override;
    function GetHeaderClientOffset: TRect; override;
    function GetHeaderHeightAddon: Integer; override;
    function GetHeaderSignIndents: TRect; override;
    function GetLinksImageEdges: TRect; override;
    function GetOverflowPanelViewInfoClass: TdxNavBarOverflowPanelViewInfoClass; override;
  public
    function BorderColor: TColor; override;
    function BorderWidth: Integer; override;
  end;

  { TdxNavBarSkinPopupControlViewInfo }

  TdxNavBarSkinPopupControlViewInfo = class(TdxNavBarSkinBasedPopupControlViewInfo)
  protected
    function GetBorderOffsets: TRect; override;
  end;

  { TdxNavBarSkinNavPanePainter }

  TdxNavBarSkinNavPanePainter = class(TdxNavBarSkinBasedNavPanePainter)
  private
    function GetLookAndFeel: TcxLookAndFeel;
    function GetOverflowPanelViewInfo: TdxNavBarSkinOverflowPanelViewInfo;
    function GetSkinName: TdxSkinName;
    function GetSkinNameAssigned: Boolean;
    function IsSkinNameStored: Boolean;
    procedure SetLookAndFeel(AValue: TcxLookAndFeel);
    procedure SetSkinName(const AValue: TdxSkinName);
    procedure SetSkinNameAssigned(AValue: Boolean);
  protected
    procedure DoDrawSizeGrip(ACanvas: TcxCanvas; const ARect: TRect); override;
    class function GetGroupViewInfoClass: TdxNavBarGroupViewInfoClass; override;
    class function GetLinkViewInfoClass: TdxNavBarLinkViewInfoClass; override;
    class function GetPopupControlViewInfoClass: TdxNavBarPopupControlViewInfoClass; override;
    class function GetSkinPainterHelperClass: TdxNavBarSkinBasedPainterHelperClass; override;
    class function GetViewInfoClass: TdxNavBarViewInfoClass; override;

    function GetItemColorPalette(ALinkViewInfo: TdxNavBarLinkViewInfo): IdxColorPalette; override;
    function GetLookAndFeelPainter: TcxCustomLookAndFeelPainter; override;
    function GetMasterLookAndFeel: TcxLookAndFeel; override;
    function GetSplitterType: Integer; override;
    procedure UpdateScrollBarLookAndFeel(ALookAndFeel: TcxLookAndFeel); override;
  public
    procedure Assign(Source: TPersistent); override;

    procedure DrawBackground; override;
    procedure DrawBorder; override;
    procedure DrawGroupBackground(AGroupViewInfo: TdxNavBarGroupViewInfo); override;
    procedure DrawGroupControl(ACanvas: TCanvas; ARect: TRect; AGroupViewInfo: TdxNavBarGroupViewInfo); override;
    procedure DrawSeparator(ALinkViewInfo: TdxNavBarLinkViewInfo); override;

    property LookAndFeel: TcxLookAndFeel read GetLookAndFeel write SetLookAndFeel;
    property OverflowPanelViewInfo: TdxNavBarSkinOverflowPanelViewInfo read GetOverflowPanelViewInfo;
  published
    property SkinName: TdxSkinName read GetSkinName write SetSkinName stored IsSkinNameStored;
    property SkinNameAssigned: Boolean read GetSkinNameAssigned write SetSkinNameAssigned default False;
  end;

  { TdxNavBarSkinExplorerBarViewInfo }

  TdxNavBarSkinExplorerBarViewInfo = class(TdxNavBarExplorerBarViewInfo)
  protected
    function GetHelper: TdxNavBarSkinPainterHelper;

    function GetGroupBorderOffsets: TRect; override;
    function GetGroupCaptionHeightAddon: Integer; override;
    function GetGroupCaptionSignSize: TSize; override;
    function GetGroupEdges: TPoint; override;
    function IsEmbedded: Boolean;
  public
    procedure AssignDefaultItemStyle; override;
  end;

  { TdxNavBarSkinExplorerBarPainter }

  TdxNavBarSkinExplorerBarPainter = class(TdxNavBarSkinBasedExplorerBarPainter)
  strict private
    FEmbedded: Boolean;

    function GetLookAndFeel: TcxLookAndFeel;
    function GetSkinName: TdxSkinName;
    function GetSkinNameAssigned: Boolean;
    function IsSkinNameStored: Boolean;
    procedure SetEmbedded(const Value: Boolean);
    procedure SetLookAndFeel(AValue: TcxLookAndFeel);
    procedure SetSkinName(const AValue: TdxSkinName);
    procedure SetSkinNameAssigned(AValue: Boolean);
  protected
    class function GetSkinPainterHelperClass: TdxNavBarSkinBasedPainterHelperClass; override;
    class function GetViewInfoClass: TdxNavBarViewInfoClass; override;
    class function SignPainterClass: TdxNavBarCustomSignPainterClass; override;

    procedure DrawGroupTopBorder(AGroupViewInfo: TdxNavBarGroupViewInfo);
    function GetItemColorPalette(ALinkViewInfo: TdxNavBarLinkViewInfo): IdxColorPalette; override;
    function GetLookAndFeelPainter: TcxCustomLookAndFeelPainter; override;
    function GetMasterLookAndFeel: TcxLookAndFeel; override;
    procedure UpdateScrollBarLookAndFeel(ALookAndFeel: TcxLookAndFeel); override;
  public
    procedure Assign(Source: TPersistent); override;

    procedure DrawBackground; override;
    procedure DrawGroupBackground(AGroupViewInfo: TdxNavBarGroupViewInfo); override;
    procedure DrawGroupBorder(AGroupViewInfo: TdxNavBarGroupViewInfo); override;
    procedure DrawGroupControl(ACanvas: TCanvas; ARect: TRect; AGroupViewInfo: TdxNavBarGroupViewInfo); override;
    procedure DrawGroupControlSplitter(AGroupViewInfo: TdxNavBarExplorerBarGroupViewInfo); override;
    procedure DrawSeparator(ALinkViewInfo: TdxNavBarLinkViewInfo); override;

    property Embedded: Boolean read FEmbedded write SetEmbedded;
    property LookAndFeel: TcxLookAndFeel read GetLookAndFeel write SetLookAndFeel;
  published
    property SkinName: TdxSkinName read GetSkinName write SetSkinName stored IsSkinNameStored;
    property SkinNameAssigned: Boolean read GetSkinNameAssigned write SetSkinNameAssigned default False;
  end;

implementation

uses
  dxNavBarViewsFact, Math, cxGeometry, SysUtils;

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

{ TdxNavBarSkinPainterHelper }

constructor TdxNavBarSkinPainterHelper.Create(ASkinName: TdxSkinName);
begin
  inherited Create(ASkinName);
  FLookAndFeel := TcxLookAndFeel.Create(nil);
  FLookAndFeel.NativeStyle := False;
  FLookAndFeel.OnChanged := LookAndFeelChanged;
end;

destructor TdxNavBarSkinPainterHelper.Destroy;
begin
  FreeAndNil(FLookAndFeel);
  inherited Destroy;
end;

function TdxNavBarSkinPainterHelper.NavBarDragDropItemTarget: TdxSkinElement;
begin
  Result := nil;
end;

function TdxNavBarSkinPainterHelper.NavBarSeparator: TdxSkinElement;
begin
  Result := nil;
end;

function TdxNavBarSkinPainterHelper.NavPaneDoesGroupCaptionButtonRequireOffset: Boolean;
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

function TdxNavBarSkinPainterHelper.NavPanePopupControl: TdxSkinElement;
var
  ASkinInfo: TdxSkinInfo;
begin
  if GetSkinPainterData(ASkinInfo) then
    Result := ASkinInfo.NavPaneFormBorder
  else
    Result := nil;
end;

procedure TdxNavBarSkinPainterHelper.PopulateSkinNames(AList: TStrings);
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

function TdxNavBarSkinPainterHelper.GetSkinInfoClass: TdxSkinInfoClass;
begin
  Result := TdxSkinInfo;
end;

function TdxNavBarSkinPainterHelper.GetSkinName: TdxSkinName;
begin
  Result := FLookAndFeel.SkinName;
end;

function TdxNavBarSkinPainterHelper.GetSkinPainterData(var AData: TdxSkinInfo): Boolean;
begin
  Result := (FLookAndFeel.SkinPainter <> nil) and FLookAndFeel.SkinPainter.GetPainterData(AData);
end;

procedure TdxNavBarSkinPainterHelper.LookAndFeelChanged(
  Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues);
begin
  DoChanged;
end;

procedure TdxNavBarSkinPainterHelper.SetSkinName(AValue: TdxSkinName);
begin
  FLookAndFeel.SkinName := AValue;
end;

{ TdxNavBarSkinNavPaneLinkViewInfo }

function TdxNavBarSkinNavPaneLinkViewInfo.SelectionRect: TRect;
begin
  Result := Rect;
  InflateRect(Result, -1, 0);
  if not TdxCustomNavBarAccess(NavBar).IsInternal then
    Result := cxRectContent(Result, GetSkinElementOffsets(GetSkinHelper.NavPaneGroupClient, ScaleFactor));
end;

function TdxNavBarSkinNavPaneLinkViewInfo.SeparatorRect: TRect;
var
  AOffsets: TRect;
begin
  Result := inherited SeparatorRect;
  if not TdxCustomNavBarAccess(NavBar).IsInternal then
  begin
    AOffsets := GetSkinElementOffsets(GetSkinHelper.NavPaneGroupClient, ScaleFactor);
    AOffsets.Top := 0;
    AOffsets.Bottom := 0;
    Result := cxRectContent(Result,  AOffsets);
  end;
end;

{ TdxNavBarSkinOverflowPanelViewInfo }

function TdxNavBarSkinOverflowPanelViewInfo.GetHeight: Integer;
begin
  Result := inherited GetHeight;
  if IsSkinAvailable and (Result > 0) then
    Result := Max(Result, GetMinHeight);
end;

function TdxNavBarSkinOverflowPanelViewInfo.GetImageWidthAddon: Integer;
var
  AElement: TdxSkinElement;
begin
  AElement := GetSkinHelper.NavPaneOverflowPanelItem;
  if AElement = nil then
    Result := inherited GetImageWidthAddon
  else
    Result := ScaleFactor.Apply(cxMarginsWidth(AElement.ContentOffset.Rect)) div 2;
end;

function TdxNavBarSkinOverflowPanelViewInfo.GetSignWidth: Integer;
begin
  if IsSkinAvailable then
    Result := GetItemSelectionWidth
  else
    Result := inherited GetSignWidth;
end;

function TdxNavBarSkinOverflowPanelViewInfo.GetMinHeight: Integer;
var
  AElement: TdxSkinElement;
begin
  Result := cxMarginsHeight(GetClientOffset) + GetImageHeight;
  AElement := GetSkinHelper.NavPaneOverflowPanelItem;
  if AElement <> nil then
    Inc(Result, ScaleFactor.Apply(cxMarginsHeight(AElement.ContentOffset.Rect)));
end;

{ TdxNavBarSkinNavPaneViewInfo }

function TdxNavBarSkinNavPaneViewInfo.BorderColor: TColor;
begin
  if IsSkinAvailable then
    Result := HeaderFontColor
  else
    Result := inherited BorderColor;
end;

function TdxNavBarSkinNavPaneViewInfo.BorderWidth: Integer;
begin
  if IsSkinAvailable then
    Result := 0
  else
    Result := inherited BorderWidth;
end;

function TdxNavBarSkinNavPaneViewInfo.GetGroupBorderOffsets: TRect;
var
  AOffsets: TRect;
begin
  Result := inherited GetGroupBorderOffsets;
  if not TdxCustomNavBarAccess(NavBar).IsInternal then
  begin
    if IsTopBorderNeeded then
      Result.Top := Result.Top + 1;
    if IsBottomBorderNeeded and not NavBar.IsNavigationClient then
      Result.Bottom := Result.Bottom + 1;
    AOffsets := GetSkinElementOffsets(GetSkinHelper.NavPaneGroupClient, ScaleFactor);
    Inc(Result.Left, AOffsets.Left);
    Inc(Result.Right, AOffsets.Right);
  end;
end;

function TdxNavBarSkinNavPaneViewInfo.GetGroupCaptionHeightAddon: Integer;
var
  AElement: TdxSkinElement;
begin
  AElement := GetSkinHelper.NavPaneGroupButtonCaption(False);
  if AElement = nil then
    Result := inherited GetGroupCaptionHeightAddon
  else
    Result := ScaleFactor.Apply(cxMarginsHeight(AElement.ContentOffset.Rect));
end;

function TdxNavBarSkinNavPaneViewInfo.GetGroupCaptionImageOffsets: TRect;
var
  AElement: TdxSkinElement;
begin
  AElement := GetSkinHelper.NavPaneGroupButtonCaption(False);
  if AElement = nil then
    Result := inherited GetGroupCaptionImageOffsets
  else
    Result := GetSkinElementOffsets(AElement, ScaleFactor);
end;

function TdxNavBarSkinNavPaneViewInfo.GetGroupCaptionSignSize: TSize;
var
  AElement: TdxSkinElement;
begin
  AElement := GetSkinHelper.NavePaneHeaderSign(False);
  if AElement <> nil then
    Result := dxSkinGetElementSize(AElement, ScaleFactor)
  else
    Result := inherited GetGroupCaptionSignSize;
end;

function TdxNavBarSkinNavPaneViewInfo.GetGroupHeaderTextIndent: Integer;
begin
  if IsSkinAvailable then
    Result := 0
  else
    Result := inherited GetGroupHeaderTextIndent;
end;

function TdxNavBarSkinNavPaneViewInfo.GetHeaderClientOffset: TRect;
var
  AElement: TdxSkinElement;
begin
  AElement := GetSkinHelper.NavPaneHeader;
  if AElement <> nil then
    Result := AElement.ContentOffset.Rect
  else
    Result := inherited GetHeaderClientOffset;
end;

function TdxNavBarSkinNavPaneViewInfo.GetHeaderHeightAddon: Integer;
begin
  if IsSkinAvailable then
    Result := GetHeaderClientOffset.Top + GetHeaderClientOffset.Bottom
  else
    Result := inherited GetHeaderHeightAddon;
end;

function TdxNavBarSkinNavPaneViewInfo.GetHeaderSignIndents: TRect;
begin
  Result := inherited GetHeaderSignIndents;
  if IsSkinAvailable then
    Result.Right := 0;
end;

function TdxNavBarSkinNavPaneViewInfo.GetLinksImageEdges: TRect;
begin
  Result := inherited GetLinksImageEdges;
  if not TdxCustomNavBarAccess(NavBar).IsInternal then
    Result.Left := Result.Left + GetSkinElementOffsets(GetSkinHelper.NavPaneGroupClient, ScaleFactor).Left;
end;

function TdxNavBarSkinNavPaneViewInfo.GetOverflowPanelViewInfoClass: TdxNavBarOverflowPanelViewInfoClass;
begin
  Result := TdxNavBarSkinOverflowPanelViewInfo;
end;

{ TdxNavBarSkinPopupControlViewInfo }

function TdxNavBarSkinPopupControlViewInfo.GetBorderOffsets: TRect;
var
  AElement: TdxSkinElement;
begin
  AElement := GetSkinHelper.NavPanePopupControl;
  if AElement <> nil then
    if not AElement.Image.Empty then
      Result := AElement.Image.Margins.Margin
    else
      with AElement.Borders do
        Result := cxRect(Left.Thin, Top.Thin, Right.Thin, Bottom.Thin)
  else
    Result := inherited GetBorderOffsets;
end;

{ TdxNavBarSkinNavPanePainter }

procedure TdxNavBarSkinNavPanePainter.Assign(Source: TPersistent);
begin
  if Source is TdxNavBarSkinNavPanePainter then
    LookAndFeel := TdxNavBarSkinNavPanePainter(Source).LookAndFeel
  else
    inherited;
end;

procedure TdxNavBarSkinNavPanePainter.DoDrawSizeGrip(ACanvas: TcxCanvas; const ARect: TRect);
begin
  if not DrawSkinElement(FSkinBasedPainterHelper.NavPaneSizeGrip, ACanvas.Canvas, ARect, ScaleFactor) then
    inherited;
end;

class function TdxNavBarSkinNavPanePainter.GetViewInfoClass: TdxNavBarViewInfoClass;
begin
  Result := TdxNavBarSkinNavPaneViewInfo;
end;

class function TdxNavBarSkinNavPanePainter.GetGroupViewInfoClass: TdxNavBarGroupViewInfoClass;
begin
  Result := TdxNavBarSkinNavPaneGroupViewInfo;
end;

class function TdxNavBarSkinNavPanePainter.GetLinkViewInfoClass: TdxNavBarLinkViewInfoClass;
begin
  Result := TdxNavBarSkinNavPaneLinkViewInfo;
end;

class function TdxNavBarSkinNavPanePainter.GetSkinPainterHelperClass: TdxNavBarSkinBasedPainterHelperClass;
begin
  Result := TdxNavBarSkinPainterHelper;
end;

class function TdxNavBarSkinNavPanePainter.GetPopupControlViewInfoClass: TdxNavBarPopupControlViewInfoClass;
begin
  Result := TdxNavBarSkinPopupControlViewInfo;
end;

function TdxNavBarSkinNavPanePainter.GetMasterLookAndFeel: TcxLookAndFeel;
begin
  Result := LookAndFeel;
end;

function TdxNavBarSkinNavPanePainter.GetSplitterType: Integer;
begin
  Result := 0;
end;

procedure TdxNavBarSkinNavPanePainter.UpdateScrollBarLookAndFeel(ALookAndFeel: TcxLookAndFeel);
begin
  ALookAndFeel.AssignedValues := [];
  ALookAndFeel.MasterLookAndFeel := LookAndFeel;
end;

function TdxNavBarSkinNavPanePainter.IsSkinNameStored: Boolean;
begin
  Result := SkinNameAssigned;
end;

function TdxNavBarSkinNavPanePainter.GetLookAndFeel: TcxLookAndFeel;
begin
  Result := TdxNavBarSkinPainterHelper(FSkinBasedPainterHelper).FLookAndFeel;
end;

function TdxNavBarSkinNavPanePainter.GetItemColorPalette(ALinkViewInfo: TdxNavBarLinkViewInfo): IdxColorPalette;
var
  AElement: TdxSkinElement;
begin
  AElement := FSkinBasedPainterHelper.NavPaneItem([sSelected, sPressed] * ALinkViewInfo.State <> []);
  if AElement <> nil then
    Result := AElement.GetGlyphColorPalette(NavBarObjectStateToSkinState(ALinkViewInfo.State))
  else
    Result := nil;
end;

function TdxNavBarSkinNavPanePainter.GetLookAndFeelPainter: TcxCustomLookAndFeelPainter;
begin
  Result := LookAndFeel.Painter;
end;

function TdxNavBarSkinNavPanePainter.GetOverflowPanelViewInfo: TdxNavBarSkinOverflowPanelViewInfo;
begin
  Result := TdxNavBarSkinOverflowPanelViewInfo(inherited OverflowPanelViewInfo);
end;

function TdxNavBarSkinNavPanePainter.GetSkinName: TdxSkinName;
begin
  Result := ColorSchemeName;
end;

function TdxNavBarSkinNavPanePainter.GetSkinNameAssigned: Boolean;
begin
  Result := lfvSkinName in LookAndFeel.AssignedValues;
end;

procedure TdxNavBarSkinNavPanePainter.SetLookAndFeel(AValue: TcxLookAndFeel);
begin
  LookAndFeel.Assign(AValue);
end;

procedure TdxNavBarSkinNavPanePainter.SetSkinName(const AValue: TdxSkinName);
begin
  ColorSchemeName := AValue;
end;

procedure TdxNavBarSkinNavPanePainter.SetSkinNameAssigned(AValue: Boolean);
begin
  if AValue then
    LookAndFeel.AssignedValues := LookAndFeel.AssignedValues + [lfvSkinName]
  else
    LookAndFeel.AssignedValues := LookAndFeel.AssignedValues - [lfvSkinName];
end;

procedure TdxNavBarSkinNavPanePainter.DrawBackground;
var
  AElement: TdxSkinElement;
begin
  AElement := FSkinBasedPainterHelper.NavBarBackground;
  if AElement = nil then
    inherited DrawBackground
  else
  begin
    Canvas.Brush.Color := AElement.Color;
    Canvas.FillRect(NavBar.ClientRect);
  end;
end;

procedure TdxNavBarSkinNavPanePainter.DrawBorder;

  procedure DrawTopBorder(const ARect: TRect);
  begin
    DrawElementPart(FSkinBasedPainterHelper.NavPaneHeader, cxCanvas, cxRectSetBottom(ARect, ARect.Top + 1), cxRectSetHeight(ARect, 1));
  end;

  procedure DrawBottomBorder(const ARect: TRect);
  begin
    DrawElementPart(FSkinBasedPainterHelper.NavPaneSplitter, cxCanvas, cxRectSetTop(ARect, ARect.Bottom - 1), cxRectSetBottom(ARect, ARect.Bottom, 1));
  end;

begin
  if not IsSkinAvailable then
    inherited DrawBorder
  else
    if not TdxCustomNavBarAccess(NavBar).IsInternal then
    begin
      if TdxNavBarSkinNavPaneViewInfo(ViewInfo).IsTopBorderNeeded then
        DrawTopBorder(NavBar.ClientRect);
      if TdxNavBarSkinNavPaneViewInfo(ViewInfo).IsBottomBorderNeeded and not NavBar.IsNavigationClient then
        DrawBottomBorder(NavBar.ClientRect);
    end
end;

procedure TdxNavBarSkinNavPanePainter.DrawGroupBackground(AGroupViewInfo: TdxNavBarGroupViewInfo);
begin
  if not DrawSkinElement(FSkinBasedPainterHelper.NavPaneGroupClient, Canvas, AGroupViewInfo.Rect,
    ScaleFactor, 0, esNormal, TdxCustomNavBarAccess(NavBar).IsInternal)
  then
    inherited DrawGroupBackground(AGroupViewInfo);
end;

procedure TdxNavBarSkinNavPanePainter.DrawGroupControl(ACanvas: TCanvas; ARect: TRect; AGroupViewInfo: TdxNavBarGroupViewInfo);
begin
  if not DrawSkinElement(FSkinBasedPainterHelper.NavPaneGroupClient, ACanvas, ARect,  ScaleFactor, 0, esNormal, True) then
    inherited DrawGroupControl(ACanvas, ARect, AGroupViewInfo);
end;

procedure TdxNavBarSkinNavPanePainter.DrawSeparator(ALinkViewInfo: TdxNavBarLinkViewInfo);
var
  AClipRect, ADrawRect: TRect;
begin
  if (FSkinBasedPainterHelper.NavPaneHeader <> nil) and
    (FSkinBasedPainterHelper.NavPaneCaptionHeight <> nil) then
  begin
    AClipRect := ALinkViewInfo.SeparatorRect;
    ADrawRect := cxRectSetBottom(AClipRect, AClipRect.Bottom, FSkinBasedPainterHelper.NavPaneCaptionHeight.Value);
    DrawElementPart(FSkinBasedPainterHelper.NavPaneHeader, cxCanvas, ADrawRect, AClipRect);
  end
  else
    inherited;
end;

{ TdxNavBarSkinExplorerBarViewInfo }

procedure TdxNavBarSkinExplorerBarViewInfo.AssignDefaultItemStyle;
var
  AElement: TdxSkinElement;
begin
  inherited AssignDefaultItemStyle;
  AElement := GetHelper.NavBarItem;
  if AElement <> nil then
    NavBar.DefaultStyles.Item.Font.Color := AElement.TextColor;
end;

function TdxNavBarSkinExplorerBarViewInfo.GetHelper: TdxNavBarSkinPainterHelper;
begin
  Result := TdxNavBarSkinPainterHelper(TdxNavBarSkinExplorerBarPainter(Painter).FSkinBasedPainterHelper);
end;

function TdxNavBarSkinExplorerBarViewInfo.GetGroupBorderOffsets: TRect;
begin
  if IsEmbedded then
    Result := cxNullRect
  else
    if TdxNavBarSkinExplorerBarPainter(Painter).IsSkinAvailable then
      Result := GetSkinElementOffsets(GetHelper.NavBarGroupClient, ScaleFactor)
    else
      Result := inherited GetGroupBorderOffsets;
end;

function TdxNavBarSkinExplorerBarViewInfo.GetGroupCaptionHeightAddon: Integer;
begin
  if GetHelper.NavBarGroupButtonCaption <> nil then
    Result := ScaleFactor.Apply(cxMarginsHeight(GetHelper.NavBarGroupButtonCaption.ContentOffset.Rect))
  else
    Result := inherited GetGroupCaptionHeightAddon;
end;

function TdxNavBarSkinExplorerBarViewInfo.GetGroupCaptionSignSize: TSize;
var
  AElement: TdxSkinElement;
begin
  AElement := GetHelper.NavBarGroupSigns(True);
  if AElement <> nil then
    Result := dxSkinGetElementSize(AElement, ScaleFactor)
  else
    Result := inherited GetGroupCaptionSignSize;
end;

function TdxNavBarSkinExplorerBarViewInfo.GetGroupEdges: TPoint;
begin
  if IsEmbedded then
    Result := ScaleFactor.Apply(cxSimplePoint)
  else
    Result := inherited GetGroupEdges;
end;

function TdxNavBarSkinExplorerBarViewInfo.IsEmbedded: Boolean;
begin
  Result := (Painter as TdxNavBarSkinExplorerBarPainter).Embedded;
end;

{ TdxNavBarSkinExplorerBarPainter }

procedure TdxNavBarSkinExplorerBarPainter.Assign(Source: TPersistent);
begin
  if Source is TdxNavBarSkinExplorerBarPainter then
    LookAndFeel := TdxNavBarSkinExplorerBarPainter(Source).LookAndFeel
  else
    inherited;
end;

procedure TdxNavBarSkinExplorerBarPainter.DrawBackground;
begin
  if not DrawSkinElement(FSkinBasedPainterHelper.NavBarBackground, Canvas, NavBar.ClientRect, ScaleFactor) then
    inherited;
end;

procedure TdxNavBarSkinExplorerBarPainter.DrawGroupBackground(AGroupViewInfo: TdxNavBarGroupViewInfo);
begin
  if not IsSkinAvailable then
    inherited;
end;

procedure TdxNavBarSkinExplorerBarPainter.DrawGroupBorder(
  AGroupViewInfo: TdxNavBarGroupViewInfo);
begin
  inherited DrawGroupBorder(AGroupViewInfo);
  if not AGroupViewInfo.IsCaptionVisible then
    DrawGroupTopBorder(AGroupViewInfo);
end;

procedure TdxNavBarSkinExplorerBarPainter.DrawGroupTopBorder(
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

procedure TdxNavBarSkinExplorerBarPainter.DrawGroupControl(
  ACanvas: TCanvas; ARect: TRect; AGroupViewInfo: TdxNavBarGroupViewInfo);
begin
  if not DrawSkinElement(FSkinBasedPainterHelper.NavBarGroupClient, ACanvas, ARect, ScaleFactor, 0, esNormal, True) then
    inherited;
end;

procedure TdxNavBarSkinExplorerBarPainter.DrawGroupControlSplitter(AGroupViewInfo: TdxNavBarExplorerBarGroupViewInfo);
begin
//do nothing
end;

procedure TdxNavBarSkinExplorerBarPainter.DrawSeparator(ALinkViewInfo: TdxNavBarLinkViewInfo);
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

class function TdxNavBarSkinExplorerBarPainter.GetViewInfoClass: TdxNavBarViewInfoClass;
begin
  Result := TdxNavBarSkinExplorerBarViewInfo;
end;

class function TdxNavBarSkinExplorerBarPainter.GetSkinPainterHelperClass: TdxNavBarSkinBasedPainterHelperClass;
begin
  Result := TdxNavBarSkinPainterHelper;
end;

class function TdxNavBarSkinExplorerBarPainter.SignPainterClass: TdxNavBarCustomSignPainterClass;
begin
  Result := TdxNavBarExplorerBarSignPainter;
end;

function TdxNavBarSkinExplorerBarPainter.GetMasterLookAndFeel: TcxLookAndFeel;
begin
  Result := LookAndFeel;
end;

procedure TdxNavBarSkinExplorerBarPainter.UpdateScrollBarLookAndFeel(ALookAndFeel: TcxLookAndFeel);
begin
  ALookAndFeel.AssignedValues := [];
  ALookAndFeel.MasterLookAndFeel := LookAndFeel;
end;

function TdxNavBarSkinExplorerBarPainter.IsSkinNameStored: Boolean;
begin
  Result := SkinNameAssigned;
end;

procedure TdxNavBarSkinExplorerBarPainter.SetEmbedded(const Value: Boolean);
begin
  if FEmbedded <> Value then
  begin
    FEmbedded := Value;
    NavBar.InvalidateAll(doRecalc);
  end;
end;

procedure TdxNavBarSkinExplorerBarPainter.SetLookAndFeel(AValue: TcxLookAndFeel);
begin
  LookAndFeel.Assign(AValue);
end;

procedure TdxNavBarSkinExplorerBarPainter.SetSkinName(const AValue: TdxSkinName);
begin
  ColorSchemeName := AValue;
end;

function TdxNavBarSkinExplorerBarPainter.GetLookAndFeel: TcxLookAndFeel;
begin
  Result := TdxNavBarSkinPainterHelper(FSkinBasedPainterHelper).FLookAndFeel;
end;

function TdxNavBarSkinExplorerBarPainter.GetItemColorPalette(ALinkViewInfo: TdxNavBarLinkViewInfo): IdxColorPalette;
begin
  if FSkinBasedPainterHelper.NavBarItem <> nil then
    Result := FSkinBasedPainterHelper.NavBarItem.GetGlyphColorPalette(NavBarObjectStateToSkinState(ALinkViewInfo.State))
  else
    Result := nil;
end;

function TdxNavBarSkinExplorerBarPainter.GetLookAndFeelPainter: TcxCustomLookAndFeelPainter;
begin
  Result := LookAndFeel.Painter;
end;

function TdxNavBarSkinExplorerBarPainter.GetSkinName: TdxSkinName;
begin
  Result := ColorSchemeName;
end;

function TdxNavBarSkinExplorerBarPainter.GetSkinNameAssigned: Boolean;
begin
  Result := lfvSkinName in LookAndFeel.AssignedValues;
end;

procedure TdxNavBarSkinExplorerBarPainter.SetSkinNameAssigned(AValue: Boolean);
begin
  if AValue then
    LookAndFeel.AssignedValues := LookAndFeel.AssignedValues + [lfvSkinName]
  else
    LookAndFeel.AssignedValues := LookAndFeel.AssignedValues - [lfvSkinName];
end;

initialization
  dxNavBarViewsFactory.RegisterView(dxNavBarSkinExplorerBarView, 'SkinExplorerBarView', TdxNavBarSkinExplorerBarPainter);
  dxNavBarViewsFactory.RegisterView(dxNavBarSkinNavigatorPaneView, 'SkinNavigationPaneView', TdxNavBarSkinNavPanePainter);
  RegisterClasses([TdxNavBarSkinNavPanePainter, TdxNavBarSkinExplorerBarPainter]);

finalization
  UnRegisterView(dxNavBarSkinExplorerBarView);
  UnRegisterView(dxNavBarSkinNavigatorPaneView);

end.

