{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressStatusBar                                         }
{                                                                    }
{           Copyright (c) 1998-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSSTATUSBAR AND ALL              }
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

unit dxRibbonStatusBar;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Graphics, ImgList, Forms, Types, ExtCtrls,
  dxThemeManager, dxCore, cxClasses, cxGraphics, cxControls, cxLookAndFeels, dxGDIPlusClasses, dxBar, dxStatusBar,
  dxRibbonSkins, dxRibbon, dxRibbonFormCaptionHelper, cxGeometry, dxCoreGraphics;

type
  TdxRibbonStatusBar = class;
  TdxStatusBarToolbarPanelStyle = class;

  { TdxRibbonStatusBarBarControlPainter }

  TdxRibbonStatusBarBarControlPainter = class(TdxBarSkinnedPainter)
  protected
    procedure DrawToolbarContentPart(ABarControl: TdxBarControl; ACanvas: TcxCanvas); override;
    procedure DrawToolbarNonContentPart(ABarControl: TdxBarControl; DC: HDC); override;
  public
    function BarBeginGroupSize: Integer; override;
    procedure BarDrawBeginGroup(ABarControl: TCustomdxBarControl; DC: HDC;
      ABeginGroupRect: TRect; AToolbarBrush: HBRUSH; AHorz: Boolean); override;
    function GetButtonBorderHeight(AScaleFactor: TdxScaleFactor): Integer; override;
    function GetEnabledTextColor(ABarItemControl: TdxBarItemControl; ASelected, AFlat: Boolean): TColor; override;
    procedure GetDisabledTextColors(ABarItemControl: TdxBarItemControl; ASelected, AFlat: Boolean; var AColor1, AColor2: TColor); override;
    function GetGlyphColorPalette(ABarItemControl: TdxBarItemControl; APaintType: TdxBarPaintType; AState: Integer): IdxColorPalette; override;
    function GetToolbarContentOffsets(ABar: TdxBar; ADockingStyle: TdxBarDockingStyle;
      AScaleFactor: TdxScaleFactor; AHasSizeGrip: Boolean): TRect; override;
    function IsInSubMenu(ABarItemControl: TdxBarItemControl): Boolean;
    function MarkButtonOffset: Integer; virtual;
    function MarkSizeX(ABarControl: TdxBarControl): Integer; override;

    function GetColorScheme(ABarControl: TCustomdxBarControl; out AColorScheme: TdxCustomRibbonSkin): Boolean; overload;
    function GetColorScheme(ABarItemControl: TdxBarItemControl; out AColorScheme: TdxCustomRibbonSkin): Boolean; overload;
    function GetIsPanelLowered(ABarControl: TCustomdxBarControl): Boolean;
    function GetStatusBar(ABarControl: TCustomdxBarControl): TdxRibbonStatusBar;
  end;

  { TdxDefaultRibbonStatusBarBarControlPainter }

  TdxDefaultRibbonStatusBarBarControlPainter = class(TdxBarFlatPainter)
  public
    function GetButtonBorderHeight(AScaleFactor: TdxScaleFactor): Integer; override;
    function GetToolbarContentOffsets(ABar: TdxBar; ADockingStyle: TdxBarDockingStyle;
      AScaleFactor: TdxScaleFactor; AHasSizeGrip: Boolean): TRect; override;
    function MarkButtonOffset: Integer; virtual;
    function MarkSizeX(ABarControl: TdxBarControl): Integer; override;
  end;

  { TdxRibbonStatusBarBarControlViewInfo }

  TdxRibbonStatusBarBarControlViewInfo = class(TdxRibbonQuickAccessToolbarBarControlViewInfo)
  protected
    function CanShowButtonGroups: Boolean; override;
  end;

  { TdxRibbonStatusBarBarControl }

  TdxRibbonStatusBarBarControl = class(TdxRibbonQuickAccessToolbarBarControl)
  private
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    function GetStatusBar: TdxRibbonStatusBar;
  protected
    procedure DoPaintItem(AControl: TdxBarItemControl; ACanvas: TcxCanvas; const AItemRect: TRect); override;
    function GetBehaviorOptions: TdxBarBehaviorOptions; override;
    function GetDefaultItemGlyph: TdxSmartGlyph; override;
    function GetItemControlDefaultViewLevel(AItemControl: TdxBarItemControl): TdxBarItemViewLevel; override;
    function GetMinHeight(AStyle: TdxBarDockingStyle): Integer; override;
    function GetRibbon: TdxCustomRibbon; override;
    function GetScaleFactor: TdxScaleFactor; override;
    function GetViewInfoClass: TCustomdxBarControlViewInfoClass; override;
    procedure InitCustomizationPopup(AItemLinks: TdxBarItemLinks); override;
    function IsValid: Boolean;
    function MarkExists: Boolean; override;
    procedure UpdateDoubleBuffered; override;

    property StatusBar: TdxRibbonStatusBar read GetStatusBar;
  public
    function IsOnGlass: Boolean; override;
  end;

  { TdxRibbonStatusBarBarControlDesignHelper }

  TdxRibbonStatusBarBarControlDesignHelper = class(TCustomdxBarControlDesignHelper)
  public
    class function GetForbiddenActions: TdxBarCustomizationActions; override;
  end;

  { TdxRibbonStatusBarDockControl }

  TdxRibbonStatusBarDockControl = class(TdxCustomRibbonDockControl)
  strict private
    FPainter: TdxBarPainter;
    FPanel: TdxStatusBarPanel;
    FStatusBar: TdxRibbonStatusBar;

    function GetBarControl: TdxRibbonStatusBarBarControl;
    function GetColorScheme: TdxCustomRibbonSkin;
    function GetRibbon: TdxCustomRibbon;
  protected
    procedure CalcLayout; override;
    procedure FreePainter; virtual;
    procedure UpdateDoubleBuffered; override;
    procedure FillBackground(DC: HDC; const ADestR, ASourceR: TRect; ABrush: HBRUSH; AColor: TColor); override;
    function GetDockedBarControlClass: TdxBarControlClass; override;
    function GetOwner: TPersistent; override;
    function GetPainter: TdxBarPainter; override;
    procedure ShowCustomizePopup; override;
    procedure VisibleChanged; override;

    property BarControl: TdxRibbonStatusBarBarControl read GetBarControl;
  public
    constructor Create(AOwner: TdxStatusBarToolbarPanelStyle); reintroduce; virtual;
    destructor Destroy; override;

    property ColorScheme: TdxCustomRibbonSkin read GetColorScheme;
    property Panel: TdxStatusBarPanel read FPanel;
    property Ribbon: TdxCustomRibbon read GetRibbon;
    property StatusBar: TdxRibbonStatusBar read FStatusBar;
  end;

  { TdxStatusBarToolbarPanelStyle }

  TdxStatusBarToolbarPanelStyle = class(TdxStatusBarPanelStyle)
  strict private
    FDockControlBounds: TRect;
  private
    FDockControl: TdxRibbonStatusBarDockControl;
    FLoadedToolbarName: string;
    FToolbar: TdxBar;
    function GetToolbar: TdxBar;
    procedure ReadToolbarName(AReader: TReader);
    procedure SetToolbar(Value: TdxBar);
    procedure UpdateToolbarValue;
    procedure WriteToolbarName(AWriter: TWriter);
  protected
    function CanDelete: Boolean; override;
    function CanSizing: Boolean; override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure DoRightToLeftConversion(const ABounds: TRect); override;
    procedure DrawContent(ACanvas: TcxCanvas; R: TRect; APainter: TdxStatusBarPainterClass); override;
    function GetMinWidth: Integer; override;
    class function GetVersion: Integer; override;
    procedure Loaded; override;
    procedure UpdateDockControl(const ABounds: TRect);
    procedure UpdateSubControlsVisibility; override;
    procedure UpdateToolbarPainter; virtual;

    function FindBarManager: TdxBarManager;
    procedure UpdateByRibbon(ARibbon: TdxCustomRibbon); virtual;
  public
    constructor Create(AOwner: TdxStatusBarPanel); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    property DockControl: TdxRibbonStatusBarDockControl read FDockControl;
  published
    property Toolbar: TdxBar read GetToolbar write SetToolbar stored False;
  end;

  { TdxStatusBarToolbarPanelViewInfo }

  TdxStatusBarToolbarPanelViewInfo = class(TdxStatusBarPanelViewInfo)
  private
    function GetPanelStyle: TdxStatusBarToolbarPanelStyle;
  public
    function CalculateAutoHeight: Integer; override;
    //
    property PanelStyle: TdxStatusBarToolbarPanelStyle read GetPanelStyle;
  end;

  { TdxRibbonStatusBarPainter }

  TdxRibbonStatusBarPainter = class(TdxStatusBarPainter)
  public
    // calc
    class function ContentExtents(AStatusBar: TdxCustomStatusBar): TRect; override;
    class function GripAreaSize(AStatusBar: TdxCustomStatusBar): TSize; override;
    class function SeparatorSize(AStatusBar: TdxCustomStatusBar): Integer; override;
    class function SizeGripMargins(AStatusBar: TdxCustomStatusBar): TRect; override;
    // draw
    class procedure DrawBorder(AStatusBar: TdxCustomStatusBar; ACanvas: TcxCanvas;
      var R: TRect); override;
    class procedure DrawPanelBorder(AStatusBar: TdxCustomStatusBar; ABevel: TdxStatusBarPanelBevel;
      ACanvas: TcxCanvas; var R: TRect); override;
    class procedure DrawPanelSeparator(AStatusBar: TdxCustomStatusBar; ACanvas: TcxCanvas;
      const R: TRect); override;
    class procedure DrawSizeGrip(AStatusBar: TdxCustomStatusBar; ACanvas: TcxCanvas;
      R: TRect); override;
    class function GetPanelBevel(APanel: TdxStatusBarPanel): TdxStatusBarPanelBevel; override;
    class function TopBorderSize: Integer; override;
  end;

  { TdxRibbonStatusBarViewInfo }

  TdxRibbonStatusBarViewInfo = class(TdxStatusBarViewInfo)
  private
    function GetStatusBar: TdxRibbonStatusBar;
  protected
    function CanUpdateDockControls: Boolean;
    function CreatePanelViewInfo(APanel: TdxStatusBarPanel): TdxStatusBarPanelViewInfo; override;
    function IsSizeGripInPanelArea: Boolean; override;
    procedure UpdateDockControls(const ABounds: TRect);
  public
    procedure Calculate(const ABounds: TRect); override;
    //
    property StatusBar: TdxRibbonStatusBar read GetStatusBar;
  end;

  { TdxRibbonStatusBar }

  TdxRibbonStatusBar = class(TdxCustomStatusBar, IdxRibbonFormNonClientPart, IdxRibbonFormStatusBar)
  private
    FColor: TColor;
    FCreating: Boolean;
    FDefaultBarPainter: TdxBarPainter;
    FRibbon: TdxCustomRibbon;

    procedure CheckAssignRibbon;
    procedure CheckRemoveToolbar(ABar: TdxBar);
    procedure SetColor(const Value: TColor);
    procedure SetRibbon(const Value: TdxCustomRibbon);
    procedure UpdateToolbarPainters;
    procedure UpdateToolbars;
  protected
    FNeedSizeGripSeparator: Boolean;
    FRaisedSideStates: array[Boolean] of Boolean;

    // IdxRibbonFormStatusBar
    function IdxRibbonFormStatusBar.GetActive = FormDrawGetActive;
    function IdxRibbonFormStatusBar.GetHeight = FormDrawGetHeight;
    function IdxRibbonFormStatusBar.GetIsRaised = FormDrawGetIsRaised;
    function FormDrawGetActive(AForm: TCustomForm): Boolean;
    function FormDrawGetHeight: Integer;
    function FormDrawGetIsRaised(ALeft: Boolean): Boolean;

    procedure AdjustTextColor(var AColor: TColor; Active: Boolean); override;
    procedure BoundsChanged; override;
    procedure Calculate; override;
    procedure CalculateFormSidesAndSizeGrip;
    procedure ColorSchemeChanged(Sender: TObject; const AEventArgs); virtual;
    function CreateViewInfo: TdxStatusBarViewInfo; override;
    function GetMinHeight: Integer; virtual;
    function GetPainterClass: TdxStatusBarPainterClass; override;
    function GetPaintStyle: TdxStatusBarPaintStyle; override;
    function IsRibbonValid: Boolean;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure PaintStyleChanged; override;
    function RecreateWndOnBiDiModeChanged: Boolean; override;
    procedure SetParent(AParent: TWinControl); override;
    procedure UpdatePanels; override;
    procedure VisibleChanged; override;

    property DefaultBarPainter: TdxBarPainter read FDefaultBarPainter;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CanAcceptPanelStyle(Value: TdxStatusBarPanelStyleClass): Boolean; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  published
    property Images;
    property Panels;
    property Ribbon: TdxCustomRibbon read FRibbon write SetRibbon;
    property SimplePanelStyle;
    property SizeGrip;
    property LookAndFeel;
    property OnHint;
    property BorderWidth;
    { TcxControl properties}
    property Anchors;
    property BiDiMode;
    property Color: TColor read FColor write SetColor default clDefault;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property Constraints;
    property ShowHint;
    property ParentBiDiMode;
    property ParentFont default False;
    property ParentShowHint;
    property PopupMenu;
    property Visible;
    property OnContextPopup;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

uses
  dxThemeConsts, dxUxTheme, dxOffice11, ActnList, StdActns, Math, dxBarStrs, cxDWMApi, dxBarSkinConsts;

resourcestring
  cxSToolbarPanelStyle = 'Toolbar Panel';

const
  GRIP_AREA_SIZE = 19;
  GRIP_SIZE = 12;

type
  TdxBarButtonControlAccess = class(TdxBarButtonControl);
  TdxCustomStatusBarAccess = class(TdxCustomStatusBar);
  TCustomdxBarControlAccess = class(TCustomdxBarControl);

{ TdxRibbonStatusBarBarControlPainter }

function TdxRibbonStatusBarBarControlPainter.BarBeginGroupSize: Integer;
begin
  Result := 2;
end;

procedure TdxRibbonStatusBarBarControlPainter.BarDrawBeginGroup(
  ABarControl: TCustomdxBarControl; DC: HDC; ABeginGroupRect: TRect; AToolbarBrush: HBRUSH; AHorz: Boolean);
var
  AColorScheme: TdxCustomRibbonSkin;
begin
  if GetColorScheme(ABarControl, AColorScheme) then
  begin
    Dec(ABeginGroupRect.Top, GetToolbarContentOffsets(nil, dsTop,
      TCustomdxBarControlAccess(ABarControl).ScaleFactor, False).Top);
    AColorScheme.DrawStatusBarToolbarSeparator(DC, ABeginGroupRect);
  end;
end;

procedure TdxRibbonStatusBarBarControlPainter.DrawToolbarContentPart(ABarControl: TdxBarControl; ACanvas: TcxCanvas);
var
  ABounds: TRect;
  AColorScheme: TdxCustomRibbonSkin;
  AStatusBar: TdxRibbonStatusBar;
begin
  if GetColorScheme(ABarControl, AColorScheme) then
  begin
    AStatusBar := GetStatusBar(ABarControl);

    ABounds := ABarControl.ClientRect;
    Dec(ABounds.Top, GetToolbarContentOffsets(ABarControl.Bar, dsTop, TCustomdxBarControlAccess(ABarControl).ScaleFactor, False).Top);
    ABounds.Bottom := ABounds.Top + AStatusBar.Height;

    AColorScheme.DrawStatusBarPanel(ACanvas.Handle,
      dxMapWindowRect(AStatusBar.Handle, ABarControl.Handle, AStatusBar.ClientBounds),
      ABounds, GetIsPanelLowered(ABarControl));
  end;
end;

procedure TdxRibbonStatusBarBarControlPainter.DrawToolbarNonContentPart(ABarControl: TdxBarControl; DC: HDC);
var
  AColorScheme: TdxCustomRibbonSkin;
  ASaveIndex: Integer;
  AStatusBar: TdxRibbonStatusBar;
begin
  if GetColorScheme(ABarControl, AColorScheme) then
  begin
    ASaveIndex := SaveDC(DC);
    try
      with ABarControl.ClientBounds do
        ExcludeClipRect(DC, Left, Top, Right, Bottom);
      AStatusBar := GetStatusBar(ABarControl);
      AColorScheme.DrawStatusBarPanel(DC, dxMapWindowRect(AStatusBar.Handle, ABarControl.Handle,
        AStatusBar.ClientBounds, False), cxRectBounds(0, 0, ABarControl.Width, AStatusBar.Height),
        GetIsPanelLowered(ABarControl));
    finally
      RestoreDC(DC, ASaveIndex);
    end;
  end;
end;

function TdxRibbonStatusBarBarControlPainter.GetButtonBorderHeight(AScaleFactor: TdxScaleFactor): Integer;
begin
  Result := 4;
  if IsCompositionEnabled then
    Dec(Result, 2);
end;

procedure TdxRibbonStatusBarBarControlPainter.GetDisabledTextColors(
  ABarItemControl: TdxBarItemControl; ASelected, AFlat: Boolean; var AColor1, AColor2: TColor);
var
  AColorScheme: TdxCustomRibbonSkin;
begin
  if not IsInSubMenu(ABarItemControl) and GetColorScheme(ABarItemControl, AColorScheme) then
  begin
    AColor1 := AColorScheme.GetPartColor(rspStatusBarText, DXBAR_DISABLED);
    AColor2 := AColor1;
  end
  else
    inherited GetDisabledTextColors(ABarItemControl, ASelected, AFlat, AColor1, AColor2);
end;

function TdxRibbonStatusBarBarControlPainter.GetEnabledTextColor(
  ABarItemControl: TdxBarItemControl; ASelected, AFlat: Boolean): TColor;
const
  States: array[Boolean, Boolean] of Integer = (
    (DXBAR_NORMAL, DXBAR_HOT), (DXBAR_CHECKED, DXBAR_HOTCHECK)
  );
var
  AColorScheme: TdxCustomRibbonSkin;
begin
  if not IsInSubMenu(ABarItemControl) and GetColorScheme(ABarItemControl, AColorScheme) then
  begin
    Result := AColorScheme.GetPartColor(rspStatusBarText,
      States[(ABarItemControl is TdxBarButtonControl) and TdxBarButtonControlAccess(ABarItemControl).Down, ASelected]);
  end
  else
    Result := inherited GetEnabledTextColor(ABarItemControl, ASelected, AFlat);
end;

function TdxRibbonStatusBarBarControlPainter.GetIsPanelLowered(ABarControl: TCustomdxBarControl): Boolean;
begin
  Result := TdxRibbonStatusBarDockControl(TdxBarControl(ABarControl).DockControl).Panel.Bevel <> dxpbRaised;
end;

function TdxRibbonStatusBarBarControlPainter.GetGlyphColorPalette(
  ABarItemControl: TdxBarItemControl; APaintType: TdxBarPaintType; AState: Integer): IdxColorPalette;
var
  AColorScheme: TdxCustomRibbonSkin;
begin
  if GetColorScheme(ABarItemControl, AColorScheme) then
  begin
    if APaintType = ptMenu then
      Result := AColorScheme.GetMenuColorPalette(AState)
    else
      Result := AColorScheme.GetStatusBarPanelColorPalette(AState);
  end
  else
    Result := nil;
end;

function TdxRibbonStatusBarBarControlPainter.GetToolbarContentOffsets(
  ABar: TdxBar; ADockingStyle: TdxBarDockingStyle; AScaleFactor: TdxScaleFactor; AHasSizeGrip: Boolean): TRect;
begin
  Result := cxRect(2, 2, 2, 1);
end;

function TdxRibbonStatusBarBarControlPainter.IsInSubMenu(ABarItemControl: TdxBarItemControl): Boolean;
begin
  Result := (ABarItemControl.Parent <> nil) and (ABarItemControl.Parent.Kind = bkSubMenu);
end;

function TdxRibbonStatusBarBarControlPainter.MarkButtonOffset: Integer;
begin
  Result := 0;
end;

function TdxRibbonStatusBarBarControlPainter.MarkSizeX(ABarControl: TdxBarControl): Integer;
begin
  Result := 0;
end;

function TdxRibbonStatusBarBarControlPainter.GetColorScheme(
  ABarControl: TCustomdxBarControl; out AColorScheme: TdxCustomRibbonSkin): Boolean;
begin
  if ABarControl is TdxBarSubMenuControl then
    Result := GetColorScheme(TdxBarSubMenuControl(ABarControl).ParentItemControl, AColorScheme)
  else
  begin
    Result := TdxBarControl(ABarControl).DockControl is TdxRibbonStatusBarDockControl;
    if Result then
      AColorScheme := TdxRibbonStatusBarDockControl(TdxBarControl(ABarControl).DockControl).Ribbon.ColorScheme;
  end;
end;

function TdxRibbonStatusBarBarControlPainter.GetColorScheme(
  ABarItemControl: TdxBarItemControl; out AColorScheme: TdxCustomRibbonSkin): Boolean;
begin
  Result := (ABarItemControl <> nil) and GetColorScheme(ABarItemControl.Parent, AColorScheme);
end;

function TdxRibbonStatusBarBarControlPainter.GetStatusBar(ABarControl: TCustomdxBarControl): TdxRibbonStatusBar;
begin
  Result := (TdxBarControl(ABarControl).DockControl as TdxRibbonStatusBarDockControl).StatusBar;
end;

{ TdxDefaultRibbonStatusBarBarControlPainter }

function TdxDefaultRibbonStatusBarBarControlPainter.GetButtonBorderHeight(AScaleFactor: TdxScaleFactor): Integer;
begin
  Result := 4;
  if IsCompositionEnabled then
    Dec(Result, 2);
end;

function TdxDefaultRibbonStatusBarBarControlPainter.GetToolbarContentOffsets(
  ABar: TdxBar; ADockingStyle: TdxBarDockingStyle; AScaleFactor: TdxScaleFactor; AHasSizeGrip: Boolean): TRect;
begin
  Result := cxRect(1, 1, 1, 3);
end;

function TdxDefaultRibbonStatusBarBarControlPainter.MarkButtonOffset: Integer;
begin
  Result := 0;
end;

function TdxDefaultRibbonStatusBarBarControlPainter.MarkSizeX(ABarControl: TdxBarControl): Integer;
begin
  Result := 0;
end;

{ TdxRibbonStatusBarBarControlViewInfo }

function TdxRibbonStatusBarBarControlViewInfo.CanShowButtonGroups: Boolean;
begin
  Result := True;
end;

{ TdxRibbonStatusBarBarControl }

procedure TdxRibbonStatusBarBarControl.DoPaintItem(
  AControl: TdxBarItemControl; ACanvas: TcxCanvas; const AItemRect: TRect);
begin
  if Ribbon = nil then
  begin
    AControl.Paint(ACanvas, AItemRect, GetPaintType);
    DrawSelectedItem(ACanvas.Handle, AControl, AItemRect);
  end
  else
    inherited DoPaintItem(AControl, ACanvas, AItemRect);
end;

function TdxRibbonStatusBarBarControl.GetBehaviorOptions: TdxBarBehaviorOptions;
begin
  Result := inherited GetBehaviorOptions;
  if (Ribbon = nil) or not ((csDesigning in ComponentState) or Bar.AllowQuickCustomizing) then
    Exclude(Result, bboCanShowPopupMenuOnMouseClick);
end;

function TdxRibbonStatusBarBarControl.GetDefaultItemGlyph: TdxSmartGlyph;
begin
  Result := nil;
end;

function TdxRibbonStatusBarBarControl.GetItemControlDefaultViewLevel(
  AItemControl: TdxBarItemControl): TdxBarItemViewLevel;
begin
  Result := IdxBarItemControlViewInfo(AItemControl.ViewInfo).GetViewLevelForButtonGroup;
end;

function TdxRibbonStatusBarBarControl.GetMinHeight(AStyle: TdxBarDockingStyle): Integer;
begin
  if (Ribbon <> nil) and IsValid then
    Result := Painter.GetButtonHeight(BarManager.ImageOptions.GlyphSize, TextSize, ScaleFactor)
  else
    Result := 18;
end;

function TdxRibbonStatusBarBarControl.GetRibbon: TdxCustomRibbon;
var
  ADockControl: TdxRibbonStatusBarDockControl;
begin
  ADockControl := TdxRibbonStatusBarDockControl(DockControl);
  if (ADockControl <> nil) and (ADockControl.StatusBar <> nil) then
    Result := ADockControl.StatusBar.Ribbon
  else
    Result := nil;
end;

function TdxRibbonStatusBarBarControl.GetViewInfoClass: TCustomdxBarControlViewInfoClass;
begin
  Result := TdxRibbonStatusBarBarControlViewInfo;
end;

procedure TdxRibbonStatusBarBarControl.InitCustomizationPopup(AItemLinks: TdxBarItemLinks);
var
  I: Integer;
  AItemLink: TdxBarItemLink;
  ASubItemButton: TdxRibbonQuickAccessPopupSubItemButton;
begin
  while AItemLinks.Count > 0 do
    AItemLinks.Items[0].Item.Free;
  for I := 0 to ItemLinks.AvailableItemCount - 1 do
  begin
    AItemLink := ItemLinks.AvailableItems[I];
    ASubItemButton := TdxRibbonQuickAccessPopupSubItemButton(AItemLinks.AddItem(TdxRibbonQuickAccessPopupSubItemButton).Item);
    ASubItemButton.Tag := TdxNativeInt(AItemLink);
    ASubItemButton.ButtonStyle := bsChecked;
    ASubItemButton.Down := AItemLink.Visible;
    ASubItemButton.Caption := AItemLink.Caption;
    BarDesignController.AddInternalItem(ASubItemButton);
  end;
end;

function TdxRibbonStatusBarBarControl.IsOnGlass: Boolean;
begin
  Result := False;
end;

function TdxRibbonStatusBarBarControl.IsValid: Boolean;
begin
  Result := HandleAllocated and (BarManager <> nil) and (BarManager.ComponentState * [csDestroying, csLoading] = []);
end;

function TdxRibbonStatusBarBarControl.MarkExists: Boolean;
begin
  Result := False;
end;

procedure TdxRibbonStatusBarBarControl.UpdateDoubleBuffered;
begin
  DoubleBuffered := True;
end;

procedure TdxRibbonStatusBarBarControl.CMFontChanged(var Message: TMessage);
begin
  inherited;
  if (StatusBar <> nil) and StatusBar.IsRibbonValid then
    StatusBar.Invalidate;
end;

function TdxRibbonStatusBarBarControl.GetScaleFactor: TdxScaleFactor;
begin
  Result := TdxRibbonStatusBarDockControl(DockControl).ScaleFactor;
end;

function TdxRibbonStatusBarBarControl.GetStatusBar: TdxRibbonStatusBar;
var
  ADockControl: TdxRibbonStatusBarDockControl;
begin
  ADockControl := TdxRibbonStatusBarDockControl(DockControl);
  if ADockControl <> nil then
    Result := ADockControl.StatusBar
  else
    Result := nil;
end;

{ TdxRibbonStatusBarBarControlDesignHelper }

class function TdxRibbonStatusBarBarControlDesignHelper.GetForbiddenActions: TdxBarCustomizationActions;
begin
  Result := inherited GetForbiddenActions - [caChangeViewLevels, caChangeButtonGroup];
end;

{ TdxRibbonStatusBarDockControl }

constructor TdxRibbonStatusBarDockControl.Create(AOwner: TdxStatusBarToolbarPanelStyle);
begin
  inherited Create(AOwner.StatusBarControl);
  FStatusBar := TdxRibbonStatusBar(AOwner.StatusBarControl);
  FPanel := AOwner.Owner;
  Parent := FStatusBar;
  AllowDocking := False;
  Align := dalNone;
  ControlStyle := ControlStyle + [csNoDesignVisible];
end;

destructor TdxRibbonStatusBarDockControl.Destroy;
begin
  FreePainter;
  inherited Destroy;
end;

procedure TdxRibbonStatusBarDockControl.CalcLayout;
begin
  inherited CalcLayout;
  StatusBar.Recalculate;
  StatusBar.Invalidate;
end;

procedure TdxRibbonStatusBarDockControl.UpdateDoubleBuffered;
begin
  DoubleBuffered := True;
end;

procedure TdxRibbonStatusBarDockControl.FillBackground(
  DC: HDC; const ADestR, ASourceR: TRect; ABrush: HBRUSH; AColor: TColor);
var
  R: TRect;
begin
  R := ClientRect;
  R.Bottom := StatusBar.Height;
  if ColorScheme = nil then
    FillRectByColor(DC, ClientRect, clBtnFace)
  else
    ColorScheme.DrawStatusBarPanel(DC, dxMapWindowRect(StatusBar.Handle, Handle, StatusBar.ClientBounds), R,
      Panel.Bevel <> dxpbRaised);
end;

procedure TdxRibbonStatusBarDockControl.FreePainter;
begin
  FreeAndNil(FPainter);
end;

function TdxRibbonStatusBarDockControl.GetDockedBarControlClass: TdxBarControlClass;
begin
  Result := TdxRibbonStatusBarBarControl;
end;

function TdxRibbonStatusBarDockControl.GetOwner: TPersistent;
begin
  Result := nil;
end;

function TdxRibbonStatusBarDockControl.GetPainter: TdxBarPainter;
begin
  if Ribbon <> nil then
  begin
    if FPainter = nil then
      FPainter := TdxRibbonStatusBarBarControlPainter.Create(TdxNativeUInt(Ribbon));
  end
  else
    FreePainter;

  if FPainter = nil then
    Result := StatusBar.DefaultBarPainter
  else
    Result := FPainter;
end;

procedure TdxRibbonStatusBarDockControl.ShowCustomizePopup;
var
  ABarControl: TdxRibbonStatusBarBarControl;
begin
  ABarControl := BarControl;
  if (ABarControl <> nil) and ABarControl.CanShowPopupMenuOnMouseClick(False) then
    ABarControl.ShowPopup(nil);
end;

procedure TdxRibbonStatusBarDockControl.VisibleChanged;
begin
  Changed;
end;

function TdxRibbonStatusBarDockControl.GetBarControl: TdxRibbonStatusBarBarControl;
begin
  if (RowCount > 0) and (Rows[0].ColCount > 0) then
    Result := TdxRibbonStatusBarBarControl(Rows[0].Cols[0].BarControl)
  else
    Result := nil;
end;

function TdxRibbonStatusBarDockControl.GetColorScheme: TdxCustomRibbonSkin;
begin
  if Ribbon <> nil then
    Result := Ribbon.ColorScheme
  else
    Result := nil;
end;

function TdxRibbonStatusBarDockControl.GetRibbon: TdxCustomRibbon;
begin
  if StatusBar.IsRibbonValid then
    Result := StatusBar.Ribbon
  else
    Result := nil;
end;

{ TdxStatusBarToolbarPanelStyle }

constructor TdxStatusBarToolbarPanelStyle.Create(AOwner: TdxStatusBarPanel);
begin
  inherited Create(AOwner);
  FDockControl := TdxRibbonStatusBarDockControl.Create(Self);
end;

destructor TdxStatusBarToolbarPanelStyle.Destroy;
begin
  Toolbar := nil;
  FreeAndNil(FDockControl);
  inherited Destroy;
end;

procedure TdxStatusBarToolbarPanelStyle.Assign(Source: TPersistent);

  function IsInheritanceUpdating: Boolean;
  begin
    Result := (Owner.Collection <> nil) and (csUpdating in StatusBarControl.ComponentState);
  end;

begin
  BeginUpdate;
  try
    inherited Assign(Source);
    if Source is TdxStatusBarToolbarPanelStyle then
    begin
      if (TdxStatusBarToolbarPanelStyle(Source).Toolbar <> nil) and IsInheritanceUpdating then
        ToolBar := FindBarManager.BarByComponentName(TdxStatusBarToolbarPanelStyle(Source).Toolbar.Name)
      else
        Toolbar := TdxStatusBarToolbarPanelStyle(Source).Toolbar;
    end;
  finally
    EndUpdate;
  end;
end;

function TdxStatusBarToolbarPanelStyle.CanDelete: Boolean;
begin
  Result := (Toolbar = nil) or not (csAncestor in Toolbar.ComponentState);
end;

function TdxStatusBarToolbarPanelStyle.CanSizing: Boolean;
begin
  Result := not GetPanelFixed;
end;

procedure TdxStatusBarToolbarPanelStyle.DefineProperties(Filer: TFiler);

  function NeedWriteToolbarName: Boolean;
  var
    AAncestorToolbar: TdxBar;
  begin
    if Filer.Ancestor <> nil then
    begin
      AAncestorToolbar := TdxStatusBarToolbarPanelStyle(Filer.Ancestor).ToolBar;
      Result := (AAncestorToolbar = nil) and (Toolbar <> nil) or
        (AAncestorToolbar <> nil) and (ToolBar = nil) or
        (AAncestorToolbar <> nil) and (AAncestorToolbar.Name <> Toolbar.Name);
    end
    else
      Result := ToolBar <> nil;
  end;

begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('ToolbarName', ReadToolbarName, WriteToolbarName, NeedWriteToolbarName);
end;

procedure TdxStatusBarToolbarPanelStyle.DoRightToLeftConversion(const ABounds: TRect);
begin
  inherited DoRightToLeftConversion(ABounds);
  DockControl.BoundsRect := TdxRightToLeftLayoutConverter.ConvertRect(FDockControlBounds, ABounds);
end;

procedure TdxStatusBarToolbarPanelStyle.DrawContent(ACanvas: TcxCanvas;
  R: TRect; APainter: TdxStatusBarPainterClass);
begin
end;

function TdxStatusBarToolbarPanelStyle.GetMinWidth: Integer;
var
  ABarControl: TdxRibbonStatusBarBarControl;
begin
  Result := 50;
  if Owner.Fixed and (Toolbar <> nil) and (Toolbar.Control is TdxRibbonStatusBarBarControl) then
  begin
    ABarControl := TdxRibbonStatusBarBarControl(Toolbar.Control);
    if ABarControl.IsValid then
      Result := ABarControl.GetSize(cxMaxRectSize).cx + 2;
  end;
end;

class function TdxStatusBarToolbarPanelStyle.GetVersion: Integer;
begin
  Result := 1;
end;

procedure TdxStatusBarToolbarPanelStyle.Loaded;
begin
  inherited Loaded;
  UpdateToolbarValue;
end;

procedure TdxStatusBarToolbarPanelStyle.UpdateDockControl(const ABounds: TRect);
begin
  if not StatusBarControl.UseRightToLeftAlignment then
    DockControl.BoundsRect := ABounds
  else
    FDockControlBounds := ABounds;
end;

procedure TdxStatusBarToolbarPanelStyle.UpdateSubControlsVisibility;
begin
  DockControl.Visible := Owner.Visible and not
    TdxCustomStatusBarAccess(Owner.StatusBarControl).SimplePanelStyle.Active;
end;

function TdxStatusBarToolbarPanelStyle.FindBarManager: TdxBarManager;
begin
  Result := nil;
  if (Owner.Collection <> nil) and (StatusBarControl.Owner is TCustomForm) then
    Result := GetBarManagerByForm(TCustomForm(StatusBarControl.Owner));
  if Result = nil then
    raise EdxException.Create(cxGetResourceString(@dxSBAR_CANTFINDBARMANAGERFORSTATUSBAR));
end;

procedure TdxStatusBarToolbarPanelStyle.UpdateByRibbon(ARibbon: TdxCustomRibbon);
begin
  DockControl.UpdateDock;
  DockControl.RepaintBarControls;
end;

function TdxStatusBarToolbarPanelStyle.GetToolbar: TdxBar;
begin
  if (FLoadedToolbarName <> '') and (Owner.Collection <> nil) and
    IsAncestorComponentDifferencesDetection(StatusBarControl) then
      Result := FindBarManager.BarByComponentName(FLoadedToolbarName)
  else
    Result := FToolbar;
end;

procedure TdxStatusBarToolbarPanelStyle.ReadToolbarName(AReader: TReader);
begin
  FLoadedToolbarName := AReader.ReadString;
end;

procedure TdxStatusBarToolbarPanelStyle.SetToolbar(Value: TdxBar);
begin
  if FToolBar <> Value then
  begin
    if (FToolbar <> nil) and not (csDestroying in FToolbar.ComponentState) then
    begin
      FToolbar.RemoveFreeNotification(StatusBarControl);
      RibbonUndockToolBar(FToolbar);
    end;
    FToolbar := Value;
    if FToolbar <> nil then
    begin
      FToolbar.FreeNotification(StatusBarControl);
      DockControl.BarManager := FToolbar.BarManager;
      RibbonDockToolBar(FToolbar, DockControl);
    end
    else
      DockControl.BarManager := nil;
    if not (csDestroying in StatusBarControl.ComponentState) then
    begin
      TCustomForm(StatusBarControl.Owner).Realign;
      TdxRibbonStatusBar(StatusBarControl).PaintStyleChanged;
      StatusBarControl.Invalidate;
    end;
  end;
end;

procedure TdxStatusBarToolbarPanelStyle.UpdateToolbarPainter;
begin
  DockControl.FreePainter;
end;

procedure TdxStatusBarToolbarPanelStyle.UpdateToolbarValue;
begin
  if FLoadedToolbarName <> '' then
  begin
    ToolBar := FindBarManager.BarByComponentName(FLoadedToolbarName);
    FLoadedToolbarName := '';
  end;
end;

procedure TdxStatusBarToolbarPanelStyle.WriteToolbarName(AWriter: TWriter);
begin
  if ToolBar <> nil then
    AWriter.WriteString(ToolBar.Name)
  else
    AWriter.WriteString('');
end;

{ TdxStatusBarToolbarPanelViewInfo }

function TdxStatusBarToolbarPanelViewInfo.CalculateAutoHeight: Integer;
var
  ABarControl: TdxRibbonStatusBarBarControl;
begin
  Result := 0;
  if PanelStyle <> nil then
  begin
    ABarControl := PanelStyle.DockControl.BarControl;
    if (ABarControl <> nil) and ABarControl.IsValid then
    begin
      Result := ABarControl.GetSize(Panel.Width).cy + cxMarginsHeight(
        ABarControl.Painter.GetToolbarContentOffsets(ABarControl.Bar, dsNone, ABarControl.ScaleFactor, False));
    end;
  end;
end;

function TdxStatusBarToolbarPanelViewInfo.GetPanelStyle: TdxStatusBarToolbarPanelStyle;
begin
  Result := Panel.PanelStyle as TdxStatusBarToolbarPanelStyle;
end;

{ TdxRibbonStatusBarPainter }

class function TdxRibbonStatusBarPainter.ContentExtents(AStatusBar: TdxCustomStatusBar): TRect;
begin
  Result := cxNullRect;
end;

class function TdxRibbonStatusBarPainter.GripAreaSize(AStatusBar: TdxCustomStatusBar): TSize;
begin
  Result := TdxCustomStatusBarAccess(AStatusBar).ScaleFactor.Apply(cxSize(GRIP_AREA_SIZE));
end;

class function TdxRibbonStatusBarPainter.SeparatorSize(AStatusBar: TdxCustomStatusBar): Integer;
begin
  with TdxRibbonStatusBar(AStatusBar) do
    if IsRibbonValid then
      Result := Ribbon.ColorScheme.GetStatusBarSeparatorSize
    else
      Result := 3;
end;

class function TdxRibbonStatusBarPainter.SizeGripMargins(AStatusBar: TdxCustomStatusBar): TRect;
begin
  Result := Rect(1, 1, 1, 1);
end;

class procedure TdxRibbonStatusBarPainter.DrawBorder(AStatusBar: TdxCustomStatusBar; ACanvas: TcxCanvas; var R: TRect);
begin
  if TdxRibbonStatusBar(AStatusBar).IsRibbonValid then
    TdxRibbonStatusBar(AStatusBar).Ribbon.ColorScheme.DrawStatusBar(ACanvas.Handle, R)
  else
  begin
    ACanvas.FrameRect(R, clBtnShadow);
    InflateRect(R, -1, -1);
    FillRectByColor(ACanvas.Handle, R, clBtnFace);
  end;
end;

class procedure TdxRibbonStatusBarPainter.DrawPanelBorder(AStatusBar: TdxCustomStatusBar;
  ABevel: TdxStatusBarPanelBevel; ACanvas: TcxCanvas; var R: TRect);
var
  ARibbonStatusBar: TdxRibbonStatusBar;
begin
  ARibbonStatusBar := TdxRibbonStatusBar(AStatusBar);
  if ARibbonStatusBar.IsRibbonValid then
    ARibbonStatusBar.Ribbon.ColorScheme.DrawStatusBarPanel(ACanvas.Handle, ARibbonStatusBar.ClientBounds, R,
      ABevel <> dxpbRaised)
  else
    inherited DrawPanelBorder(AStatusBar, ABevel, ACanvas, R)
end;

class procedure TdxRibbonStatusBarPainter.DrawPanelSeparator(
  AStatusBar: TdxCustomStatusBar; ACanvas: TcxCanvas; const R: TRect);
var
  ARibbonStatusBar: TdxRibbonStatusBar;
begin
  ARibbonStatusBar := TdxRibbonStatusBar(AStatusBar);
  if ARibbonStatusBar.IsRibbonValid then
    ARibbonStatusBar.Ribbon.ColorScheme.DrawStatusBarPanelSeparator(ACanvas.Handle, R)
  else
    FillRectByColor(ACanvas.Handle, R, clBtnShadow);
end;

class procedure TdxRibbonStatusBarPainter.DrawSizeGrip(
  AStatusBar: TdxCustomStatusBar; ACanvas: TcxCanvas; R: TRect);
var
  AGripRect, ASeparatorRect: TRect;
  ARibbonStatusBar: TdxRibbonStatusBar;
begin
  ARibbonStatusBar := TdxRibbonStatusBar(AStatusBar);
  if ARibbonStatusBar.IsRibbonValid then
  begin
    ACanvas.SaveClipRegion;
    try
      if ARibbonStatusBar.UseRightToLeftAlignment then
      begin
        AGripRect := cxRectInflate(R, 1, 1, -1, 1);
        if not ARibbonStatusBar.FNeedSizeGripSeparator then
        begin
          ASeparatorRect := AGripRect;
          ASeparatorRect.Left := ASeparatorRect.Right - 3;
          ACanvas.ExcludeClipRect(ASeparatorRect);
        end;
      end
      else
      begin
        AGripRect := cxRectInflate(R, -1, 1, 1, 1);
        if not ARibbonStatusBar.FNeedSizeGripSeparator then
          ACanvas.ExcludeClipRect(cxRectSetWidth(AGripRect, 3));
      end;
      ARibbonStatusBar.Ribbon.ColorScheme.UseRightToLeftAlignment := ARibbonStatusBar.UseRightToLeftAlignment;
      ARibbonStatusBar.Ribbon.ColorScheme.DrawStatusBarGripBackground(ACanvas.Handle, AGripRect);
      Dec(AGripRect.Bottom, ARibbonStatusBar.ScaleFactor.Apply(3));
      ARibbonStatusBar.Ribbon.ColorScheme.DrawStatusBarSizeGrip(ACanvas.Handle, AGripRect);
    finally
      ACanvas.RestoreClipRegion;
    end;
  end
  else
    inherited DrawSizeGrip(AStatusBar, ACanvas, R);
end;

class function TdxRibbonStatusBarPainter.GetPanelBevel(
  APanel: TdxStatusBarPanel): TdxStatusBarPanelBevel;
begin
  if APanel <> nil then
    Result := APanel.Bevel
  else
    Result := dxpbNone;
end;

class function TdxRibbonStatusBarPainter.TopBorderSize: Integer;
begin
  Result := 0;
end;

{ TdxRibbonStatusBarViewInfo }

procedure TdxRibbonStatusBarViewInfo.Calculate(const ABounds: TRect);
begin
  inherited Calculate(ABounds);
  UpdateDockControls(ABounds);
end;

function TdxRibbonStatusBarViewInfo.CanUpdateDockControls: Boolean;
var
  F: TCustomForm;
begin
  F := TCustomForm(StatusBar.Owner);
  Result := (F <> nil) and not ((F.WindowState = wsMaximized) and (fsCreating in F.FormState));
end;

function TdxRibbonStatusBarViewInfo.CreatePanelViewInfo(APanel: TdxStatusBarPanel): TdxStatusBarPanelViewInfo;
begin
  if APanel.PanelStyle is TdxStatusBarToolbarPanelStyle then
    Result := TdxStatusBarToolbarPanelViewInfo.Create(APanel)
  else
    Result := inherited CreatePanelViewInfo(APanel);
end;

function TdxRibbonStatusBarViewInfo.IsSizeGripInPanelArea: Boolean;
begin
  Result := True;
end;

procedure TdxRibbonStatusBarViewInfo.UpdateDockControls(const ABounds: TRect);
var
  ADockControl: TdxRibbonStatusBarDockControl;
  I: Integer;
  R: TRect;
begin
  if CanUpdateDockControls then
  begin
    for I := 0 to PanelCount - 1 do
    begin
      if Panels[I].PanelStyle is TdxStatusBarToolbarPanelStyle then
      begin
        ADockControl := TdxStatusBarToolbarPanelStyle(Panels[I].PanelStyle).DockControl;
        R := cxRectSetHeight(PanelViewInfo[I].Bounds, ADockControl.Height);
        if not StatusBar.IsRibbonValid then
          OffsetRect(R, 0, 1);
        if StatusBar.SizeGripAllocated then
          R.Right := Max(2, Min(R.Right, ABounds.Right - GRIP_AREA_SIZE));
        TdxStatusBarToolbarPanelStyle(Panels[I].PanelStyle).UpdateDockControl(R);
      end;
    end;
  end;
end;

function TdxRibbonStatusBarViewInfo.GetStatusBar: TdxRibbonStatusBar;
begin
  Result := TdxRibbonStatusBar(inherited StatusBar);
end;

{ TdxRibbonStatusBar }

constructor TdxRibbonStatusBar.Create(AOwner: TComponent);
begin
  FCreating := True;
  RibbonCheckCreateComponent(AOwner, ClassType);
  FCreating := False;
  inherited Create(AOwner);
  FDefaultBarPainter := TdxDefaultRibbonStatusBarBarControlPainter.Create(0);
  Font.Color := clDefault;
  Color := clDefault;
  Height := 23;
  CheckAssignRibbon;
end;

destructor TdxRibbonStatusBar.Destroy;
begin
  if FCreating then Exit;
  if IsRibbonValid then
    Ribbon.ColorSchemeHandlers.Remove(ColorSchemeChanged);
  FreeAndNil(FDefaultBarPainter);
  inherited Destroy;
end;

function TdxRibbonStatusBar.CanAcceptPanelStyle(Value: TdxStatusBarPanelStyleClass): Boolean;
begin
  Result := True;
end;

procedure TdxRibbonStatusBar.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  if not AutoSize then
    AHeight := Max(AHeight, GetMinHeight);
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  Recalculate;
end;

function TdxRibbonStatusBar.FormDrawGetActive(AForm: TCustomForm): Boolean;
begin
  Result := Visible and (Owner = AForm);
end;

function TdxRibbonStatusBar.FormDrawGetHeight: Integer;
begin
  Result := Height;
end;

function TdxRibbonStatusBar.FormDrawGetIsRaised(ALeft: Boolean): Boolean;
begin
  Result := FRaisedSideStates[ALeft];
end;

procedure TdxRibbonStatusBar.AdjustTextColor(var AColor: TColor; Active: Boolean);
begin
  if IsRibbonValid then
    AColor := Ribbon.ColorScheme.GetPartColor(rspStatusBarText, Ord(not Active))
  else
    AColor := Font.Color;
end;

procedure TdxRibbonStatusBar.BoundsChanged;
var
  AIntf: IdxRibbonFormNonClientDraw;
begin
  inherited BoundsChanged;
  if IsRibbonValid then
  begin
    if Supports(Ribbon, IdxRibbonFormNonClientDraw, AIntf) then
      AIntf.Changed(Self);
  end;
end;

procedure TdxRibbonStatusBar.Calculate;
begin
  inherited Calculate;
  CalculateFormSidesAndSizeGrip;
end;

procedure TdxRibbonStatusBar.CalculateFormSidesAndSizeGrip;
var
  I: Integer;
begin
  FNeedSizeGripSeparator := Panels.Count = 0;
  FRaisedSideStates[True] := False;
  for I := ViewInfo.PanelCount - 1 downto 0 do
  begin
    FRaisedSideStates[True] := ViewInfo.Panels[I].Bevel = dxpbRaised;
    Break;
  end;
  FRaisedSideStates[False] := SizeGripAllocated;
  for I := ViewInfo.PanelCount - 1 downto 0 do
  begin
    FNeedSizeGripSeparator := ViewInfo.Panels[I].Bevel <> dxpbRaised;
    Break;
  end;
end;

procedure TdxRibbonStatusBar.ColorSchemeChanged(Sender: TObject; const AEventArgs);
begin
  PaintStyleChanged;
end;

function TdxRibbonStatusBar.CreateViewInfo: TdxStatusBarViewInfo;
begin
  Result := TdxRibbonStatusBarViewInfo.Create(Self);
end;

function TdxRibbonStatusBar.GetMinHeight: Integer;
begin
  if HandleAllocated and ([csDestroying, csLoading] * ComponentState = []) then
    Result := CalculateAutoHeight
  else
    Result := 0;
end;

function TdxRibbonStatusBar.GetPainterClass: TdxStatusBarPainterClass;
begin
  Result := TdxRibbonStatusBarPainter
end;

function TdxRibbonStatusBar.GetPaintStyle: TdxStatusBarPaintStyle;
begin
  Result := stpsOffice11;
end;

function TdxRibbonStatusBar.IsRibbonValid: Boolean;
begin
  Result := (Ribbon <> nil) and not Ribbon.IsDestroying;
end;

procedure TdxRibbonStatusBar.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = Ribbon then
      Ribbon := nil
    else
      if AComponent is TdxBar then
        CheckRemoveToolbar(TdxBar(AComponent));
  end;
end;

procedure TdxRibbonStatusBar.PaintStyleChanged;
begin
  if IsRibbonValid and (Color = clDefault) then
    inherited Color := Ribbon.ColorScheme.GetPartColor(rfspRibbonForm);
  inherited PaintStyleChanged;
end;

procedure TdxRibbonStatusBar.SetParent(AParent: TWinControl);
begin
  if Assigned(AParent) then
  begin
    AParent := GetParentForm(AParent, not (csDesigning in ComponentState));
    if Assigned(AParent) and not (AParent is TCustomForm) then
      raise EdxException.CreateFmt(cxGetResourceString(@dxSBAR_RIBBONBADPARENT), [ClassName]);
  end;
  inherited SetParent(AParent);
  Top := 32000;
end;

procedure TdxRibbonStatusBar.VisibleChanged;
var
  AForm: TCustomForm;
begin
  inherited VisibleChanged;
  if IsRibbonValid then
  begin
    AForm := TCustomForm(Owner);
    if (AForm <> nil) and AForm.HandleAllocated and AForm.Visible then
    begin
      SetWindowPos(AForm.Handle, 0, 0, 0, AForm.Width, AForm.Height,
        SWP_NOZORDER or SWP_NOACTIVATE or SWP_NOMOVE or SWP_FRAMECHANGED);
    end;
  end;
end;

procedure TdxRibbonStatusBar.UpdatePanels;
begin
  Recalculate;
  UpdateToolbars;
  CheckAutoSize;
  Invalidate;
end;

procedure TdxRibbonStatusBar.CheckAssignRibbon;
begin
  if IsDesigning then
    Ribbon := FindRibbonForComponent(Self);
end;

procedure TdxRibbonStatusBar.CheckRemoveToolbar(ABar: TdxBar);
var
  I: Integer;
  APanelStyle: TdxStatusBarToolbarPanelStyle;
begin
  //if csDestroying in ComponentState then Exit;
  for I := 0 to Panels.Count - 1 do
    if Panels[I].PanelStyle is TdxStatusBarToolbarPanelStyle then
    begin
      APanelStyle := TdxStatusBarToolbarPanelStyle(Panels[I].PanelStyle);
      if APanelStyle.Toolbar = ABar then
        APanelStyle.Toolbar := nil;
    end;
end;

function TdxRibbonStatusBar.RecreateWndOnBiDiModeChanged: Boolean;
begin
  Result := False;
end;

procedure TdxRibbonStatusBar.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    if Value <> clDefault then
      inherited Color := Value;
    PaintStyleChanged;
  end;
end;

procedure TdxRibbonStatusBar.SetRibbon(const Value: TdxCustomRibbon);
var
  AIntf: IdxRibbonFormNonClientDraw;
begin
  if FRibbon <> Value then
  begin
    if (FRibbon <> nil) and not FRibbon.IsDestroying then
    begin
      FRibbon.RemoveFreeNotification(Self);
      FRibbon.ColorSchemeHandlers.Remove(ColorSchemeChanged);
      if Supports(FRibbon, IdxRibbonFormNonClientDraw, AIntf) then
        AIntf.Remove(Self);
    end;
    FRibbon := Value;
    if FRibbon <> nil then
    begin
      FRibbon.FreeNotification(Self);
      FRibbon.ColorSchemeHandlers.Add(ColorSchemeChanged);
      if Supports(FRibbon, IdxRibbonFormNonClientDraw, AIntf) then
        AIntf.Add(Self);
    end;
    UpdateToolbarPainters;
    PaintStyleChanged;
  end;
end;

procedure TdxRibbonStatusBar.UpdateToolbarPainters;
var
  I: Integer;
begin
  if [csDestroying] * ComponentState = [] then
  begin
    for I := 0 to Panels.Count - 1 do
      if Panels[I].PanelStyle is TdxStatusBarToolbarPanelStyle then
        TdxStatusBarToolbarPanelStyle(Panels[I].PanelStyle).UpdateToolbarPainter;
  end;
end;

procedure TdxRibbonStatusBar.UpdateToolbars;
var
  I: Integer;
begin
  if HandleAllocated and (([csDestroying] * ComponentState) = []) then
  begin
    for I := 0 to Panels.Count - 1 do
      if Panels[I].PanelStyle is TdxStatusBarToolbarPanelStyle then
        TdxStatusBarToolbarPanelStyle(Panels[I].PanelStyle).UpdateByRibbon(Ribbon);
  end;
end;

initialization
  RegisterClasses([TdxRibbonStatusBar, TdxStatusBarContainerControl]);
  GetRegisteredStatusBarPanelStyles.Register(TdxStatusBarToolbarPanelStyle, cxSToolbarPanelStyle);
  BarDesignController.RegisterBarControlDesignHelper(
    TdxRibbonStatusBarBarControl, TdxRibbonStatusBarBarControlDesignHelper);

finalization
  BarDesignController.UnregisterBarControlDesignHelper(
    TdxRibbonStatusBarBarControl, TdxRibbonStatusBarBarControlDesignHelper);
  GetRegisteredStatusBarPanelStyles.Unregister(TdxStatusBarToolbarPanelStyle);

end.
