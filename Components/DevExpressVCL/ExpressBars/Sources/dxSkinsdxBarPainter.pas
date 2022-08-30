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

unit dxSkinsdxBarPainter;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, Windows, SysUtils, Classes, Controls, Graphics, Messages, cxLookAndFeels,
  cxLookAndFeelPainters, dxBar, cxControls,
  dxSkinsCore, cxGraphics, cxGeometry, dxScreenTip, dxCore, dxCoreGraphics, dxSkinInfo;

type
  { TdxBarSkinPainter }

  TdxBarSkinPainter = class(TdxBarPainter, IdxFadingPainterHelper)
  private
    FSkinPainter: TcxCustomLookAndFeelPainter;

    function GetBar(AIsVertical: Boolean): TdxSkinElement;
    function GetBarCustomize(AIsVertical: Boolean): TdxSkinElement;
    function GetBarDisabledTextColor: TdxSkinColor;
    function GetBarDrag(AIsVertical: Boolean): TdxSkinElement;
    function GetBarSeparator(AIsVertical: Boolean): TdxSkinElement;
    function GetDock: TdxSkinElement;
    function GetDockControlWindowButton: TdxSkinElement;
    function GetDockControlWindowButtonGlyph: TdxSkinElement;
    function GetFloatingBar: TdxSkinElement;
    function GetItemSeparator: TdxSkinElement;
    function GetLinkBorderPainter: TdxSkinElement;
    function GetLinkSelected: TdxSkinElement;
    function GetMainMenu(AIsVertical: Boolean): TdxSkinElement;
    function GetMainMenuCustomize(AIsVertical: Boolean): TdxSkinElement;
    function GetMainMenuDrag: TdxSkinElement;
    function GetMainMenuLinkSelected: TdxSkinElement;
    function GetMDIButton(AButton: TdxBarMDIButton): TdxSkinElement;
    function GetPopupMenu: TdxSkinElement;
    function GetPopupMenuCheck: TdxSkinElement;
    function GetPopupMenuExpandButton: TdxSkinElement;
    function GetPopupMenuLinkSelected: TdxSkinElement;
    function GetPopupMenuSeparator: TdxSkinElement;
    function GetPopupMenuSideStrip: TdxSkinElement;
    function GetPopupMenuSideStripNonRecent: TdxSkinElement;
    function GetPopupMenuSplitButton: TdxSkinElement;
    function GetPopupMenuSplitButton2: TdxSkinElement;
    function GetStatusBar: TdxSkinElement;
    function GetScreenTipItem: TdxSkinColor;
    function GetScreenTipSeparator: TdxSkinElement;
    function GetScreenTipTitleItem: TdxSkinColor;
    function GetScreenTipWindow: TdxSkinElement;
    function GetSkinPainterData(var AData: TdxSkinInfo): Boolean;
    procedure SetupCacheControl(AElement: TdxSkinElement; ACacheCapacity: Integer = 0);

    function DrawSkinElement(AElement: TdxSkinElement; DC: HDC; const ARect: TRect; AScaleFactor: TdxScaleFactor;
      AImageIndex: Integer = 0; AState: TdxSkinElementState = esNormal; AIsRightToLeft: Boolean = False): Boolean;
    function DrawSkinElementContent(AElement: TdxSkinElement; DC: HDC; const ARect: TRect;
      AScaleFactor: TdxScaleFactor; AImageIndex: Integer = 0; AState: TdxSkinElementState = esNormal): Boolean;
    function DrawSkinElementBorders(AElement: TdxSkinElement; DC: HDC;
      const ARect: TRect; AImageIndex: Integer = 0; AState: TdxSkinElementState = esNormal): Boolean;
    procedure DrawSkinElementParentBackground(DC: HDC; const R: TRect;
      ABarControl: TCustomdxBarControl; AElement: TdxSkinElement);

    function GetSkinEditorsBackgroundColor(AState: TcxEditStateColorKind): TColor;
    function GetSkinEditorsTextColor(AState: TcxEditStateColorKind): TColor;
    function GetSkinElementSize(ASkinElement: TdxSkinElement; AScaleFactor: TdxScaleFactor): TSize;
    function GetSkinElementTextColor(ASkinElement: TdxSkinElement; AState: TcxButtonState): TColor;

    function GetBarElement(ABarControl: TCustomdxBarControl): TdxSkinElement; overload;
    function GetBarElement(ABarControl: TCustomdxBarControl; AVertical: Boolean): TdxSkinElement; overload;
    function GetBarMarkElement(AMainMenu, AVertical: Boolean): TdxSkinElement;
    function GetTextColorElement(ABarControl: TCustomdxBarControl): TdxSkinElement;

    function IsBarElementSkinned(ABarControl: TCustomdxBarControl): Boolean;

    procedure DrawArrowButtonElement(AElement: TdxSkinElement; ACanvas: TcxCanvas;
      const ARect: TRect; AImageIndex: Integer = 0; AState: TdxSkinElementState = esNormal);

    // Bar
    procedure DrawFloatingBarCaptionButton(DC: HDC; ARect: TRect; AContentType: Integer; AState: TdxBarMarkState);
    procedure InternalDrawDockedBarBackground(ABarControl: TdxBarControl; DC: HDC; R: TRect; AClientArea: Boolean);

    // ButtonLikeControl
    function ButtonLikeControlGetState(const ADrawParams: TdxBarButtonLikeControlDrawParams): TdxSkinElementState;
    function ButtonLikeDrawArrowFadingElement(const ADrawParams: TdxBarButtonLikeControlDrawParams; var R: TRect): Boolean;
    function ButtonLikeDrawFadingElement(ABarItemControl: TdxBarItemControl; DC: HDC; const R: TRect; AIsSplit: Boolean): Boolean;
    procedure ButtonLikeDrawGlyphBorder(ABarItemControl: TdxBarItemControl; DC: HDC; ABrush: HBRUSH; NeedBorder: Boolean;
      R: TRect; PaintType: TdxBarPaintType; IsGlyphEmpty, Selected, Down, DrawDowned, ADroppedDown, IsSplit, AAllowFading: Boolean);
  protected
    function GetLookAndFeelPainter: TcxCustomLookAndFeelPainter; override;
    // IdxFadingPainterHelper
    function BarMarkIsOpaque: Boolean;
    procedure DrawBarMarkState(ABarControl: TdxBarControl; DC: HDC; const R: TRect; AState: TdxBarMarkState);
    procedure DrawButtonBackground(const ADrawParams: TdxBarButtonLikeControlDrawParams);

    // Common
      // Attributes
    function GetDefaultEnabledTextColor(ABarItemControl: TdxBarItemControl; ASelected, AFlat: Boolean): TColor; override;
    procedure GetDisabledTextColors(ABarItemControl: TdxBarItemControl; ASelected, AFlat: Boolean; var AColor1, AColor2: TColor); override;
      // Conditions
    class function UseTextColorForItemArrow: Boolean; override;
      // Draw
    procedure DrawGlyphEmptyImage(ABarItemControl: TdxBarItemControl; DC: HDC;
      R: TRect; APaintType: TdxBarPaintType; ADown: Boolean); override;
    procedure DrawGlyphBorder(ABarItemControl: TdxBarItemControl; DC: HDC;
      ABrush: HBRUSH; NeedBorder: Boolean; R: TRect; PaintType: TdxBarPaintType;
      IsGlyphEmpty, Selected, Down, DrawDowned, ADroppedDown, IsSplit: Boolean); override;

    // Bar
      // Attributes
    function BarCaptionColor(ABarControl: TdxBarControl): COLORREF; override;

    class function NeedDoubleBuffer: Boolean; override;
      // Positions
    class procedure BarOffsetFloatingBarCaption(ABarControl: TdxBarControl; var X: Integer; var R: TRect); override;
      // Draw
    procedure BarDrawGrip(ABarControl: TdxBarControl; DC: HDC; R: TRect; AToolbarBrush: HBRUSH); override;
    procedure BarDrawMarkBackground(ABarControl: TdxBarControl; DC: HDC; ItemRect: TRect; AToolbarBrush: HBRUSH); override;

    // SubMenuControl
    procedure SubMenuControlDrawMarkSelection(ABarSubMenuControl: TdxBarSubMenuControl; ADC: HDC; const AMarkRect: TRect); override;


    // Hints
    function CreateHintViewInfo(ABarManager: TdxBarManager; AHintText: string;
      const AShortCut: string; AScreenTip: TdxScreenTip): TdxBarCustomHintViewInfo; override;

    property Bar[AIsVertical: Boolean]: TdxSkinElement read GetBar;
    property BarCustomize[AIsVertical: Boolean]: TdxSkinElement read GetBarCustomize;
    property BarDisabledTextColor: TdxSkinColor read GetBarDisabledTextColor;
    property BarDrag[AIsVertical: Boolean]: TdxSkinElement read GetBarDrag;
    property BarSeparator[AIsVertical: Boolean]: TdxSkinElement read GetBarSeparator;
    property Dock: TdxSkinElement read GetDock;
    property DockControlWindowButton: TdxSkinElement read GetDockControlWindowButton;
    property DockControlWindowButtonGlyph: TdxSkinElement read GetDockControlWindowButtonGlyph;
    property FloatingBar: TdxSkinElement read GetFloatingBar;
    property ItemSeparator: TdxSkinElement read GetItemSeparator;
    property LinkBorderPainter: TdxSkinElement read GetLinkBorderPainter;
    property LinkSelected: TdxSkinElement read GetLinkSelected;
    property MainMenu[AIsVertical: Boolean]: TdxSkinElement read GetMainMenu;
    property MainMenuCustomize[AIsVertical: Boolean]: TdxSkinElement read GetMainMenuCustomize;
    property MainMenuDrag: TdxSkinElement read GetMainMenuDrag;
    property MainMenuLinkSelected: TdxSkinElement read GetMainMenuLinkSelected;
    property MDIButton[Button: TdxBarMDIButton]: TdxSkinElement read GetMDIButton;
    property PopupMenu: TdxSkinElement read GetPopupMenu;
    property PopupMenuCheck: TdxSkinElement read GetPopupMenuCheck;
    property PopupMenuExpandButton: TdxSkinElement read GetPopupMenuExpandButton;
    property PopupMenuLinkSelected: TdxSkinElement read GetPopupMenuLinkSelected;
    property PopupMenuSeparator: TdxSkinElement read GetPopupMenuSeparator;
    property PopupMenuSideStrip: TdxSkinElement read GetPopupMenuSideStrip;
    property PopupMenuSideStripNonRecent: TdxSkinElement read GetPopupMenuSideStripNonRecent;
    property PopupMenuSplitButton: TdxSkinElement read GetPopupMenuSplitButton;
    property PopupMenuSplitButton2: TdxSkinElement read GetPopupMenuSplitButton2;
    property ScreenTipItem: TdxSkinColor read GetScreenTipItem;
    property ScreenTipSeparator: TdxSkinElement read GetScreenTipSeparator;
    property ScreenTipTitleItem: TdxSkinColor read GetScreenTipTitleItem;
    property ScreenTipWindow: TdxSkinElement read GetScreenTipWindow;
    property StatusBar: TdxSkinElement read GetStatusBar;
  public
    constructor Create(AData: TdxNativeUInt); override;
    // Common
      // Conditions
    class function IsFlatGlyphImage: Boolean; override;
    class function IsFlatItemText: Boolean; override;
    function IsCustomSelectedTextColorExists(ABarItemControl: TdxBarItemControl): Boolean; override;
      // Sizes
    class function GetPopupWindowBorderWidth: Integer; override;
      // Draw
    procedure DrawItemBackgroundInSubMenu(const ADrawParams: TdxBarButtonLikeControlDrawParams; R: TRect); override;
    function GetGlyphColorPalette(ABarItemControl: TdxBarItemControl; APaintType: TdxBarPaintType;
      ASelected, ADowned, ADrawDowned, ADroppedDown, AIsSplit: Boolean): IdxColorPalette; override;
    function GetState(ASelected, ADowned, ADrawDowned, ADroppedDown, AIsMenuItem: Boolean): TdxSkinElementState;

    // BarManager
    function GripperSize(ABarControl: TdxBarControl): Integer; override;
    class function BorderSizeX(AScaleFactor: TdxScaleFactor): Integer; override;
    class function BorderSizeY(AScaleFactor: TdxScaleFactor): Integer; override;
    function FingersSize(ABarControl: TdxBarControl): Integer; override;
    class function SubMenuBeginGroupIndent: Integer; override;

    // Bar
      //Conditions
    class function BarCaptionTransparent: Boolean; override;
      // Sizes
    function BarBeginGroupSize: Integer; override;
    procedure BarBorderPaintSizes(ABarControl: TdxBarControl; var R: TRect); override;
    procedure BarBorderSizes(ABar: TdxBar; AStyle: TdxBarDockingStyle; var R: TRect); override;
    function BarMarkItemRect(ABarControl: TdxBarControl): TRect; override;
    function BarMarkRect(ABarControl: TdxBarControl): TRect; override;
    function MarkSizeX(ABarControl: TdxBarControl): Integer; override;
    function StatusBarBorderOffsets: TRect; override;
    class function StatusBarTopBorderSize: Integer; override;
    function StatusBarGripSize(ABarManager: TdxBarManager; AScaleFactor: TdxScaleFactor): TSize; override;
      // Draw
    procedure BarCaptionFillBackground(ABarControl: TdxBarControl; DC: HDC; R: TRect; AToolbarBrush: HBRUSH); override;
    procedure BarDrawBeginGroup(ABarControl: TCustomdxBarControl; DC: HDC; ABeginGroupRect: TRect; AToolbarBrush: HBRUSH; AHorz: Boolean); override;
    procedure BarDrawCloseButton(ABarControl: TdxBarControl; DC: HDC; R: TRect); override;
    procedure BarDrawDockedBarBorder(ABarControl: TdxBarControl; DC: HDC; R: TRect; AToolbarBrush: HBRUSH); override;
    procedure BarDrawFloatingBarBorder(ABarControl: TdxBarControl; DC: HDC; R, CR: TRect; AToolbarBrush: HBRUSH); override;
    procedure BarDrawFloatingBarCaption(ABarControl: TdxBarControl; DC: HDC; R, CR: TRect;
      AToolbarBrush: HBRUSH; AScaleFactor: TdxScaleFactor); override;
    procedure BarDrawMDIButton(ABarControl: TdxBarControl; AButton: TdxBarMDIButton; AState: Integer; DC: HDC; R: TRect); override;
    procedure BarDrawStatusBarBorder(ABarControl: TdxBarControl; DC: HDC; R: TRect; AToolbarBrush: HBRUSH); override;
    function BarLinkedOwnerHasShadow(ABarControl: TCustomdxBarControl): Boolean; override;
    procedure BarMarkRectInvalidate(ABarControl: TdxBarControl); override;
    procedure StatusBarFillBackground(ABarControl: TdxBarControl; DC: HDC;
      ADestR, ASourceR, AWholeR: TRect; ABrush: HBRUSH; AColor: TColor); override;

    // Edit
    function EditGetDisabledBkColor(ABarItemControl: TdxBarCustomEditControl): Cardinal; override;
    function EditGetDisabledTextColor: Cardinal; override;
    function EditGetEnabledBkColor(ABarItemControl: TdxBarCustomEditControl): Cardinal; override;
    function EditGetEnabledTextColor: Cardinal; override;

    // DockControl
    procedure DockControlFillBackground(ADockControl: TdxDockControl; DC: HDC;
      ADestR, ASourceR, AWholeR: TRect; ABrush: HBRUSH; AColor: TColor); override;
    class function IsNativeBackground: Boolean; override;

    // CustomBar
      // Sizes
    class function BarDockedGetRowIndent: Integer; override;
    function ComboBoxArrowWidth(ABarControl: TCustomdxBarControl; cX: Integer): Integer; override;
      // Draw
    procedure BarDrawDockedBackground(ABarControl: TdxBarControl; DC: HDC;
      ADestR, ASourceR: TRect; ABrush: HBRUSH; AColor: TColor); override;
    procedure BarDrawFloatingBackground(ABarControl: TCustomdxBarControl; DC: HDC;
      ADestR, ASourceR: TRect; ABrush: HBRUSH; AColor: TColor); override;
    class procedure BarDrawOwnerLink(ABarControl: TCustomdxBarControl; DC: HDC); override;

    // DropDownListBox
    class function DropDownListBoxBorderSize: Integer; override;
    procedure DropDownListBoxDrawBorder(DC: HDC; AColor: TColor; ARect: TRect); override;

    // SubMenuControl
      // Conditions
    class function SubMenuControlHasBand: Boolean; override;
      // Sizes
    class function SubMenuControlArrowsOffset: Integer; override;
    function SubMenuControlBeginGroupRect(ABarSubMenuControl: TdxBarSubMenuControl;
      AControl: TdxBarItemControl; const AItemRect: TRect): TRect; override;
    function SubMenuControlBeginGroupSize: Integer; override;
    procedure SubMenuControlCalcDrawingConsts(ACanvas: TcxCanvas; ATextSize: Integer;
      AScaleFactor: TdxScaleFactor; out AMenuArrowWidth, AMarkSize: Integer); override;
    class function SubMenuControlDetachCaptionAreaSize(ABarSubMenuControl: TdxBarSubMenuControl): Integer; override;
    class procedure SubMenuControlOffsetDetachCaptionRect(ABarSubMenuControl: TdxBarSubMenuControl; var R: TRect); override;
      // Positions
    class function SubMenuControlGetItemTextIndent(const ADrawParams: TdxBarItemControlDrawParams): Integer; override;
      // Draw
    procedure SubMenuControlDrawBeginGroup(ABarSubMenuControl: TdxBarSubMenuControl;
      AControl: TdxBarItemControl; ACanvas: TcxCanvas; const ABeginGroupRect: TRect); override;
    procedure SubMenuControlDrawBorder(ABarSubMenuControl: TdxBarSubMenuControl;
      DC: HDC; R: TRect); override;
    procedure SubMenuControlDrawClientBorder(ABarSubMenuControl: TdxBarSubMenuControl;
      DC: HDC; const R: TRect; ABrush: HBRUSH); override;
    procedure SubMenuControlDrawBackground(ABarSubMenuControl: TdxBarSubMenuControl;
      ACanvas: TcxCanvas; ARect: TRect; ABrush: HBRUSH; AColor: TColor); override;
    procedure SubMenuControlDrawDetachCaption(ABarSubMenuControl: TdxBarSubMenuControl;
      DC: HDC; R: TRect); override;
    procedure SubMenuControlDrawMarkContent(ABarSubMenuControl: TdxBarSubMenuControl;
      DC: HDC; R: TRect; ASelected: Boolean); override;
    procedure SubMenuControlDrawSeparator(ACanvas: TcxCanvas; const ARect: TRect); override;
    procedure SubMenuControlDrawSeparatorBackground(ACanvas: TcxCanvas; const ARect: TRect);

    // Mark
      // Draw
    procedure BarDrawMark(ABarControl: TdxBarControl; DC: HDC; MarkR: TRect); override;
    procedure BarDrawMarkElements(ABarControl: TdxBarControl; DC: HDC; ItemRect: TRect); override;

    // QuickCustItem
        // Conditions
    class function IsQuickControlPopupOnRight: Boolean; override;

    // Button Control
      // Conditions
    function IsButtonControlArrowBackgroundOpaque(const ADrawParams: TdxBarButtonLikeControlDrawParams): Boolean; override;
    function IsButtonControlArrowDrawSelected(const ADrawParams: TdxBarButtonLikeControlDrawParams): Boolean; override;
    function IsDropDownRepaintNeeded: Boolean; override;
      // Sizes
    class procedure CorrectButtonControlDefaultHeight(var DefaultHeight: Integer); override;
    class procedure CorrectButtonControlDefaultWidth(var DefaultWidth: Integer); override;
      // Draw
    procedure DrawButtonControlArrowBackground(const ADrawParams: TdxBarButtonLikeControlDrawParams; var R1: TRect; ABrush: HBRUSH); override;

    // ColorCombo
      // Conditions
    function ColorComboHasCompleteFrame: Boolean; override;
      // Sizes
    function GetCustomColorButtonIndents(APaintType: TdxBarPaintType): TRect; override;
    function GetCustomColorButtonWidth(APaintType: TdxBarPaintType; const ARect: TRect): Integer; override;
      // Draw
    procedure ColorComboDrawCustomButton(const ADrawParams: TdxBarColorComboControlDrawParams; ARect: TRect); override;
    procedure ColorComboDrawCustomButtonAdjacentZone(const ADrawParams: TdxBarColorComboControlDrawParams; ARect: TRect); override;

    // EditControl
      // Conditions
    class function EditControlCaptionBackgroundIsOpaque(const ADrawParams: TdxBarEditLikeControlDrawParams): Boolean; override;
    class function EditControlCaptionRightIndentIsOpaque(const ADrawParams: TdxBarEditLikeControlDrawParams): Boolean; override;
      // Sizes
    class function EditControlBorderOffsets(APaintType: TdxBarPaintType): TRect; override;
      // Draw
    procedure EditControlDrawBackground(const ADrawParams: TdxBarEditLikeControlDrawParams); override;
    procedure EditControlDrawBorder(const ADrawParams: TdxBarEditLikeControlDrawParams; var ARect: TRect); override;
    procedure EditControlDrawSelectionFrame(const ADrawParams: TdxBarEditLikeControlDrawParams; const ARect: TRect); override;
      // Select EditControl indents
    class function EditControlCaptionSimpleIndent(const ADrawParams: TdxBarEditLikeControlDrawParams): Integer; override;

    // CustomCombo
    class procedure CustomComboDrawItem(ABarCustomCombo: TdxBarCustomCombo;
      ACanvas: TCanvas; AIndex: Integer; ARect: TRect; AState: TOwnerDrawState;
      AInteriorIsDrawing: Boolean); override;

    // ComboControl
      // Sizes
    class function ComboControlArrowOffset: Integer; override;
      // Draw
    procedure ComboControlDrawArrowButton(const ADrawParams: TdxBarEditLikeControlDrawParams; ARect: TRect; AInClientArea: Boolean); override;

    // DateNavigator
      // Conditions
    class function IsDateNavigatorFlat: Boolean; override;
      // Attributes
    function DateNavigatorHeaderColor: TColor; override;
      // Draw
    procedure DateNavigatorDrawButton(ABarItem: TdxBarItem; DC: HDC; R: TRect;
      const ACaption: string; APressed: Boolean; AScaleFactor: TdxScaleFactor); override;

    // InPlaceSubItemControl
      // Conditions
    function InPlaceSubItemControlIsArrowSelected(const ADrawParams: TdxBarInPlaceSubItemControlDrawParams): Boolean; override;
      // Draw
    procedure InPlaceSubItemControlDrawBackground(const ADrawParams: TdxBarInPlaceSubItemControlDrawParams; ARect: TRect); override;

    // SpinEditControl
      // Sizes
    function GetSpinEditButtonWidth(APaintType: TdxBarPaintType; const ARect: TRect): Integer; override;
    function GetSpinEditButtonIndents(APaintType: TdxBarPaintType): TRect; override;
      // Draw
    procedure SpinEditControlDrawButton(const ADrawParams: TdxBarSpinEditDrawParams; ARect: TRect; AButtonIndex: Integer); override;
    procedure SpinEditControlDrawButtonsAdjacentZone(const ADrawParams: TdxBarSpinEditDrawParams; const ARect: TRect); override;
      // Others
    procedure CalculateSpinEditParts(const ADrawParams: TdxBarSpinEditDrawParams;
      var AParts, AAreaParts: array of TRect); override;
    procedure SpinEditCorrectFrameRect(const ADrawParams: TdxBarItemControlDrawParams; var ARect: TRect); override;

    // ProgressControl
      // Sizes
    function ProgressControlBarHeight(ABarItemControl: TdxBarItemControl): Integer; override;
    class function ProgressControlIndent(const ADrawParams: TdxBarItemControlDrawParams): Integer; override;
      // Draw
    procedure ProgressControlDrawBackground(const ADrawParams: TdxBarItemControlDrawParams; var BarR: TRect); override;
    procedure ProgressControlFillContent(const ADrawParams: TdxBarItemControlDrawParams; const R: TRect; ABarBrush: HBRUSH); override;

    // StaticControl
      // Sizes
    function StaticControlGetBorderOffsets(AParent: TCustomdxBarControl; ABorderStyle: TdxBarStaticBorderStyle): TRect; override;
      // Draw
    procedure DrawStaticBackground(const ADrawParams: TdxBarStaticLikeControlDrawParams; ARect: TRect); override;
    procedure DrawStaticBorder(const ADrawParams: TdxBarStaticLikeControlDrawParams; var ARect: TRect); override;

    // Separator
      // Sizes
    function SubMenuGetSeparatorSize: Integer; override;
    procedure SeparatorControlGetTextColors(ABarItemControl: TdxBarItemControl;
      AEnabled, ASelected, AFlat: Boolean; var AColor1, AColor2: TColor); override;
      // Draw
    procedure DrawSeparatorGlyphAndCaption(const ADrawParams: TdxBarSeparatorControlDrawParams; const ARect: TRect); override;

    //Gallery
    procedure DropDownGalleryDrawBackground(DC: HDC; const R: TRect); override;
    procedure DropDownGalleryDrawBorder(ABarSubMenuControl: TdxBarSubMenuControl; DC: HDC; const R: TRect); override;
    procedure DropDownGalleryDrawBottomSizeGrip(DC: HDC; const R: TRect); override;
    procedure DropDownGalleryDrawBottomSizeGripEx(ABarSubMenuControl: TdxBarSubMenuControl; DC: HDC; const R: TRect); override;
    procedure DropDownGalleryDrawBottomSizingBand(DC: HDC; const R: TRect); override;
    procedure DropDownGalleryDrawBottomVerticalSizeGrip(DC: HDC; const R: TRect); override;
    procedure DropDownGalleryDrawFilterBand(DC: HDC; const R: TRect); override;
    procedure DropDownGalleryDrawGroupHeaderBackground(DC: HDC; const R: TRect); override;
    procedure DropDownGalleryDrawItem(DC: HDC; const R: TRect; AState: Integer; AScaleFactor: TdxScaleFactor); override;
    procedure DropDownGalleryDrawScrollBarBackgroundEx(ABarSubItemControl: TdxBarSubItemControl; DC: HDC;
      const R: TRect; AScaleFactor: TdxScaleFactor); override;
    procedure DropDownGalleryDrawSelectionFrame(ACanvas: TcxCanvas; const R: TRect; AState: Integer); override;
    procedure DropDownGalleryDrawTopSizeGrip(DC: HDC; const R: TRect); override;
    procedure DropDownGalleryDrawTopSizeGripEx(ABarSubMenuControl: TdxBarSubMenuControl; DC: HDC; const R: TRect); override;
    procedure DropDownGalleryDrawTopSizingBand(DC: HDC; const R: TRect); override;
    procedure DropDownGalleryDrawTopVerticalSizeGrip(DC: HDC; const R: TRect); override;
    function DropDownGalleryGetClientBorderSize: Integer; override;
    function DropDownGalleryGetContentOffsets(APart: Integer): TRect; override;
    function DropDownGalleryGetFilterBandSeparatorColor: TColor; override;
    function DropDownGalleryGetFilterBandTextColor(AState: Integer): TColor; override;
    function DropDownGalleryGetGroupHeaderTextColor: TColor; override;
    function DropDownGalleryGetNCBorderSize: Integer; override;
    procedure DropDownGalleryItemGetTextColors(ABarItemControl: TdxBarItemControl;
      AEnabled, ASelected, AFlat: Boolean; var AColor1, AColor2: TColor); override;
  end;

implementation

uses
  Math, dxOffice11, dxBarSkinConsts, dxSkinsStrs, dxFading, dxDPIAwareUtils;

type
  TdxBarAccess = class(TdxBar);
  TCustomdxBarControlAccess = class(TCustomdxBarControl);
  TdxBarControlAccess = class(TdxBarControl);
  TdxBarSubMenuControlAccess = class(TdxBarSubMenuControl);
  TdxBarItemControlAccess = class(TdxBarItemControl);
  TdxBarEditControlAccess = class(TdxBarEditControl);
  TdxBarDockControlAccess = class(TdxBarDockControl);
  TdxBarSkinnedPainterAccess = class(TdxBarSkinnedPainter);
  TdxBarSubItemControlAccess = class(TdxBarSubItemControl);

{TdxBarSkinPainter}

constructor TdxBarSkinPainter.Create(AData: TdxNativeUInt);
begin
  inherited Create(AData);
  FSkinPainter := TcxCustomLookAndFeelPainter(AData);
end;

class function TdxBarSkinPainter.IsFlatGlyphImage: Boolean;
begin
  Result := True;
end;

class function TdxBarSkinPainter.IsFlatItemText: Boolean;
begin
  Result := True;
end;

function TdxBarSkinPainter.IsCustomSelectedTextColorExists(ABarItemControl: TdxBarItemControl): Boolean;
var
  ASkinElement: TdxSkinElement;
begin
  Result := False;
  if IsBarElementSkinned(ABarItemControl.Parent) then
  begin
    ASkinElement := GetTextColorElement(ABarItemControl.Parent);
    Result :=
      (ASkinElement.TextColor <> GetSkinElementTextColor(ASkinElement, cxbsHot)) or
      (ASkinElement.TextColor <> GetSkinElementTextColor(ASkinElement, cxbsPressed));
  end;
end;

class function TdxBarSkinPainter.GetPopupWindowBorderWidth: Integer;
begin
  Result := 1;
end;

procedure TdxBarSkinPainter.DrawItemBackgroundInSubMenu(const ADrawParams: TdxBarButtonLikeControlDrawParams; R: TRect);
var
  ASkinElement: TdxSkinElement;
begin
  with ADrawParams do
  begin
    R := BarItemControl.ItemBounds;  // because QuickControlItem draw before
    DrawBackground(BarItemControl, Canvas.Handle, R, 0, False);
    if DrawSelected then
    begin
      if IsDropDown and SplitDropDown then
      begin
        ASkinElement := PopupMenuSplitButton;
        if Canvas.UseRightToLeftAlignment then
          Inc(R.Left, ArrowSize.cx)
        else
          Dec(R.Right, ArrowSize.cx);
      end
      else
        ASkinElement := PopupMenuLinkSelected;

      DrawSkinElement(ASkinElement, Canvas.Handle, R, dxGetScaleFactor(BarItemControl), 0,
        ButtonLikeControlGetState(ADrawParams), Canvas.UseRightToLeftAlignment);
    end;
  end;
end;

function TdxBarSkinPainter.GripperSize(ABarControl: TdxBarControl): Integer;
var
  ABarControlAccess: TdxBarControlAccess;
begin
  ABarControlAccess := TdxBarControlAccess(ABarControl);
  if ABarControlAccess.IsMainMenu and not ABarControlAccess.IsRealVertical then
    Result := GetSkinElementSize(MainMenuDrag, ABarControlAccess.ScaleFactor).cx
  else
    Result := GetSkinElementSize(BarDrag[ABarControlAccess.IsRealVertical], ABarControlAccess.ScaleFactor).cx;
end;

class function TdxBarSkinPainter.BorderSizeX(AScaleFactor: TdxScaleFactor): Integer;
begin
  Result := AScaleFactor.Apply(2);
end;

class function TdxBarSkinPainter.BorderSizeY(AScaleFactor: TdxScaleFactor): Integer;
begin
  Result := AScaleFactor.Apply(2);
end;

function TdxBarSkinPainter.FingersSize(ABarControl: TdxBarControl): Integer;
begin
  Result := GripperSize(ABarControl);
end;

class function TdxBarSkinPainter.SubMenuBeginGroupIndent: Integer;
begin
  Result := TdxBarFlatPainter.SubMenuBeginGroupIndent;
end;

class function TdxBarSkinPainter.BarCaptionTransparent: Boolean;
begin
  Result := True; // TdxBarXPPainter
end;

function TdxBarSkinPainter.BarBeginGroupSize: Integer;
begin
  Result := GetSkinElementSize(BarSeparator[False], dxDefaultScaleFactor).cx;
end;

procedure TdxBarSkinPainter.BarBorderPaintSizes(ABarControl: TdxBarControl; var R: TRect);
var
  ABarControlAccess: TdxBarControlAccess;
  ABarAccess: TdxBarAccess;
begin
  ABarControlAccess := TdxBarControlAccess(ABarControl);
  ABarAccess := TdxBarAccess(ABarControlAccess.Bar);
  BarBorderSizes(ABarAccess, ABarAccess.DockingStyle, R);
  if ABarControlAccess.CanMoving then
    if ABarControlAccess.Vertical then
      Inc(R.Top, FingersSize(ABarControlAccess))
    else
      if ABarControl.UseRightToLeftAlignment and ABarControlAccess.Horizontal then
        Inc(R.Right, FingersSize(ABarControlAccess))
      else
        Inc(R.Left, FingersSize(ABarControlAccess));
end;

procedure TdxBarSkinPainter.BarBorderSizes(ABar: TdxBar; AStyle: TdxBarDockingStyle; var R: TRect);
begin
  R := cxEmptyRect;
  if IsBarElementSkinned(ABar.Control) then
  begin
    R := GetBarElement(ABar.Control).ContentOffset.Rect;
    if ABar.Control.UseRightToLeftAlignment and not ABar.IsVertical then
      R := TdxRightToLeftLayoutConverter.ConvertOffsets(R);
  end;
end;

function TdxBarSkinPainter.BarMarkItemRect(ABarControl: TdxBarControl): TRect;
var
  ABarControlAccess: TdxBarControlAccess;
  ABarAccess: TdxBarAccess;
  ABorderOffsets: TRect;
begin
  ABarControlAccess := TdxBarControlAccess(ABarControl);
  ABarAccess := TdxBarAccess(ABarControlAccess.Bar);

  Result := inherited BarMarkItemRect(ABarControl);
  ABorderOffsets := cxEmptyRect;

  if (ABarControlAccess.DockingStyle <> dsNone) and
    not ABarAccess.IsStatusBar then
  begin
    BarBorderSizes(ABarAccess, ABarAccess.DockingStyle, ABorderOffsets);
    if ABarControl.IsRealVertical then
    begin
      OffsetRect(Result, 0, ABorderOffsets.Bottom);
      Result := cxRectInflate(Result, ABorderOffsets.Left, 0, ABorderOffsets.Right, 0);
    end
    else
    begin
      if ABarControl.UseRightToLeftAlignment then
        OffsetRect(Result, -ABorderOffsets.Right, 0)
      else
        OffsetRect(Result, ABorderOffsets.Right, 0);
      Result := cxRectInflate(Result, 0, ABorderOffsets.Top, 0, ABorderOffsets.Bottom);
    end;
  end;

  if ABarAccess.IsStatusBar then
  begin
    ABorderOffsets := StatusBarBorderOffsets;
    ABorderOffsets.Left := 0;
    if ABarControl.UseRightToLeftAlignment and ABarControlAccess.Horizontal then
      Result := TdxRightToLeftLayoutConverter.ConvertOffsets(Result);
    Result := cxRectContent(Result, cxRectInvert(ABorderOffsets));
  end;
end;

function TdxBarSkinPainter.BarMarkRect(ABarControl: TdxBarControl): TRect;
var
  ABarControlAccess: TdxBarControlAccess;
  ABarAccess: TdxBarAccess;
  ABorderOffsets: TRect;
begin
  ABarControlAccess := TdxBarControlAccess(ABarControl);
  ABarAccess := TdxBarAccess(ABarControlAccess.Bar);
  if ABarAccess.BorderStyle = bbsNone then
  begin
    Result := inherited BarMarkItemRect(ABarControl);
    if not ABarAccess.IsStatusBar then
    begin
      BarBorderSizes(ABarAccess, ABarAccess.DockingStyle, ABorderOffsets);
      if ABarControlAccess.Vertical then
        Inc(Result.Top, ABorderOffsets.Bottom)
      else
        if ABarControl.UseRightToLeftAlignment then
          Dec(Result.Right, ABorderOffsets.Left)
        else
          Inc(Result.Left, ABorderOffsets.Right);
    end;
  end
  else
    Result := BarMarkItemRect(ABarControl);
end;

function TdxBarSkinPainter.MarkSizeX(ABarControl: TdxBarControl): Integer;
var
  ABarControlAccess: TdxBarControlAccess;
begin
  ABarControlAccess := TdxBarControlAccess(ABarControl);
  Result := GetSkinElementSize(GetBarMarkElement(ABarControlAccess.IsMainMenu, False), ABarControlAccess.ScaleFactor).cx;
end;

function TdxBarSkinPainter.StatusBarBorderOffsets: TRect;
begin
  if StatusBar <> nil then
    Result := StatusBar.ContentOffset.Rect
  else
    Result := inherited StatusBarBorderOffsets;
end;

class function TdxBarSkinPainter.StatusBarTopBorderSize: Integer;
begin
  Result := 0;
end;

function TdxBarSkinPainter.StatusBarGripSize(ABarManager: TdxBarManager; AScaleFactor: TdxScaleFactor): TSize;
begin
  Result := FSkinPainter.ScaledSizeGripSize(AScaleFactor);
end;

procedure TdxBarSkinPainter.BarCaptionFillBackground(ABarControl: TdxBarControl; DC: HDC; R: TRect; AToolbarBrush: HBRUSH);
begin
  if FloatingBar <> nil then
    FillRectByColor(DC, R, FloatingBar.Color)
  else
    inherited;
end;

procedure TdxBarSkinPainter.BarDrawBeginGroup(ABarControl: TCustomdxBarControl;
  DC: HDC; ABeginGroupRect: TRect; AToolbarBrush: HBRUSH; AHorz: Boolean);
begin
  TdxBarControlAccess(ABarControl).FillBackground(DC, ABeginGroupRect, 0, clNone, True);
  DrawSkinElement(BarSeparator[AHorz], DC, ABeginGroupRect, dxGetScaleFactor(ABarControl));
end;

procedure TdxBarSkinPainter.BarDrawCloseButton(ABarControl: TdxBarControl; DC: HDC; R: TRect);
begin
  cxPaintCanvas.BeginPaint(DC);
  try
    cxPaintCanvas.SetClipRegion(TcxRegion.Create(R), roIntersect);
    BarCaptionFillBackground(ABarControl, cxPaintCanvas.Handle, R, 0);
    DrawFloatingBarCaptionButton(cxPaintCanvas.Handle, R, 2, TdxBarControlAccess(ABarControl).CloseButtonState);
  finally
    cxPaintCanvas.EndPaint;
  end;
end;

procedure TdxBarSkinPainter.BarDrawDockedBarBorder(ABarControl: TdxBarControl; DC: HDC; R: TRect; AToolbarBrush: HBRUSH);
var
  ASaveIndex: Integer;
begin
  ASaveIndex := SaveDC(DC);
  try
    cxExcludeClipRect(DC, TdxBarControlAccess(ABarControl).ClientBounds);
    InternalDrawDockedBarBackground(ABarControl, DC, R, False);
  finally
    RestoreDC(DC, ASaveIndex)
  end;
end;

procedure TdxBarSkinPainter.BarDrawFloatingBarBorder(
  ABarControl: TdxBarControl; DC: HDC; R, CR: TRect; AToolbarBrush: HBRUSH);
begin
  if FloatingBar <> nil then
  begin
    FrameRectByColor(DC, R, FloatingBar.Borders.Items[Low(TcxBorder)].Color);
    InflateRect(R, -1, -1);
    FillRectByColor(DC, R, FloatingBar.Color);
  end
  else
    inherited;
end;

procedure TdxBarSkinPainter.BarDrawFloatingBarCaption(ABarControl: TdxBarControl;
  DC: HDC; R, CR: TRect; AToolbarBrush: HBRUSH; AScaleFactor: TdxScaleFactor);
begin
  if FloatingBar <> nil then
    AToolbarBrush := CreateSolidBrush(FloatingBar.Color)
  else
    AToolbarBrush := 0;
  inherited;
  DeleteObject(AToolbarBrush);
end;

procedure TdxBarSkinPainter.BarDrawMDIButton(ABarControl: TdxBarControl; AButton: TdxBarMDIButton; AState: Integer; DC: HDC; R: TRect);

  function GetSkinElementState: TdxSkinElementState;
  begin
    case AState of
      DXBAR_HOT:
        Result := esHot;
      DXBAR_PRESSED:
        Result := esPressed;
      DXBAR_DISABLED:
        Result := esDisabled;
      else
        Result := esNormal;
    end;
  end;

var
  AElement: TdxSkinElement;
begin
  AElement := MDIButton[AButton];
  if (AElement <> nil) and AElement.IsAlphaUsed then
    TdxBarControlAccess(ABarControl).FillBackground(DC, R, 0, clNone, True);
  DrawSkinElement(AElement, DC, R, dxGetScaleFactor(ABarControl), 0, GetSkinElementState);
end;

procedure TdxBarSkinPainter.BarDrawStatusBarBorder(
  ABarControl: TdxBarControl; DC: HDC; R: TRect; AToolbarBrush: HBRUSH);
begin
  DrawSkinElementBorders(StatusBar, DC, R);
end;

function TdxBarSkinPainter.BarLinkedOwnerHasShadow(ABarControl: TCustomdxBarControl): Boolean;
begin
  Result := False;
end;

procedure TdxBarSkinPainter.BarMarkRectInvalidate(ABarControl: TdxBarControl);
begin
  inherited;
  if TdxBarControlAccess(ABarControl).DockingStyle <> dsNone then       // TdxOffice11Painter
    SendMessage(ABarControl.Handle, WM_NCPAINT, 0, 0);
end;

function TdxBarSkinPainter.EditGetDisabledBkColor(ABarItemControl: TdxBarCustomEditControl): Cardinal;
begin
  if GetSkinEditorsBackgroundColor(esckDisabled) = clDefault then
    Result := inherited EditGetDisabledBkColor(ABarItemControl)
  else
    Result := GetSkinEditorsBackgroundColor(esckDisabled);
end;

function TdxBarSkinPainter.EditGetDisabledTextColor: Cardinal;
begin
  if GetSkinEditorsTextColor(esckDisabled) = clDefault then
    Result := inherited EditGetDisabledTextColor
  else
    Result := GetSkinEditorsTextColor(esckDisabled);
end;

function TdxBarSkinPainter.EditGetEnabledBkColor(ABarItemControl: TdxBarCustomEditControl): Cardinal;
begin
  if GetSkinEditorsBackgroundColor(esckNormal) = clDefault then
    Result := inherited EditGetEnabledBkColor(ABarItemControl)
  else
    Result := GetSkinEditorsBackgroundColor(esckNormal);
end;

function TdxBarSkinPainter.EditGetEnabledTextColor: Cardinal;
begin
  if GetSkinEditorsTextColor(esckNormal) = clDefault then
    Result := inherited EditGetEnabledTextColor
  else
    Result := GetSkinEditorsTextColor(esckNormal);
end;

procedure TdxBarSkinPainter.StatusBarFillBackground(ABarControl: TdxBarControl;
  DC: HDC; ADestR, ASourceR, AWholeR: TRect; ABrush: HBRUSH; AColor: TColor);
begin
  cxPaintCanvas.BeginPaint(DC);
  try
    cxPaintCanvas.SetClipRegion(TcxRegion.Create(ADestR), roIntersect);
    OffsetRect(AWholeR, -(ASourceR.Left - ADestR.Left), -(ASourceR.Top - ADestR.Top));
    DrawSkinElementParentBackground(cxPaintCanvas.Handle, AWholeR, ABarControl, StatusBar);
    DrawSkinElement(StatusBar, cxPaintCanvas.Handle, AWholeR, dxGetScaleFactor(ABarControl));
  finally
    cxPaintCanvas.EndPaint;
  end;
end;

procedure TdxBarSkinPainter.DockControlFillBackground(ADockControl: TdxDockControl;
  DC: HDC; ADestR, ASourceR, AWholeR: TRect; ABrush: HBRUSH; AColor: TColor);

  procedure FillBackgroundTempBitmap(ABitmap: TBitmap);
  begin
    ABitmap.Width := cxRectWidth(AWholeR);
    ABitmap.Height := cxRectHeight(AWholeR);
    DrawSkinElement(Dock, ABitmap.Canvas.Handle, AWholeR, dxGetScaleFactor(ADockControl));
  end;

var
  ADockControlAccess: TdxBarDockControlAccess;
begin
  ADockControlAccess := TdxBarDockControlAccess(ADockControl);
  if ADockControlAccess.BackgroundTempBitmap.Empty then
    FillBackgroundTempBitmap(ADockControlAccess.BackgroundTempBitmap);
  cxBitBlt(DC, ADockControlAccess.BackgroundTempBitmap.Canvas.Handle, ADestR, ASourceR.TopLeft, SRCCOPY);
end;

class function TdxBarSkinPainter.IsNativeBackground: Boolean;
begin
  Result := True; // TdxBarXPPainter
end;

class function TdxBarSkinPainter.BarDockedGetRowIndent: Integer;
begin
  Result := 1
end;

function TdxBarSkinPainter.ComboBoxArrowWidth(ABarControl: TCustomdxBarControl; cX: Integer): Integer;
begin
  Result := cX + 11;
end;

procedure TdxBarSkinPainter.BarDrawDockedBackground(
  ABarControl: TdxBarControl; DC: HDC; ADestR, ASourceR: TRect; ABrush: HBRUSH; AColor: TColor);
var
  ABarControlAccess: TdxBarControlAccess;
  ASaveIndex: Integer;
  AWholeR: TRect;
begin
  ABarControlAccess := TdxBarControlAccess(ABarControl);
  if (ABarControlAccess.Bar.BorderStyle = bbsNone) or ABarControlAccess.IsBackgroundBitmap then
    inherited
  else
  begin
    ASaveIndex := SaveDC(DC);
    try
      IntersectClipRect(DC, ADestR.Left, ADestR.Top, ADestR.Right, ADestR.Bottom);
      AWholeR := cxRectBounds(ADestR.Left - ASourceR.Left, ADestR.Top - ASourceR.Top, ABarControl.Width, ABarControl.Height);
      InternalDrawDockedBarBackground(ABarControl, DC, AWholeR, not cxRectIsEqual(ASourceR, ADestR));
    finally
      RestoreDC(DC, ASaveIndex)
    end;
  end;
end;

procedure TdxBarSkinPainter.BarDrawFloatingBackground(
  ABarControl: TCustomdxBarControl; DC: HDC; ADestR, ASourceR: TRect;
  ABrush: HBRUSH; AColor: TColor);
begin
  if TdxBarControlAccess(ABarControl).IsBackgroundBitmap then
    inherited
  else
  begin
    cxPaintCanvas.BeginPaint(DC);
    try
      cxPaintCanvas.IntersectClipRect(ADestR);
      DrawSkinElement(FloatingBar, cxPaintCanvas.Handle, ABarControl.ClientRect, dxGetScaleFactor(ABarControl));
    finally
      cxPaintCanvas.EndPaint;
    end;
  end;
end;

class procedure TdxBarSkinPainter.BarDrawOwnerLink(ABarControl: TCustomdxBarControl; DC: HDC);
begin
  // do nothing
end;

procedure TdxBarSkinPainter.DropDownGalleryDrawBackground(DC: HDC; const R: TRect);
var
  ASkinInfo: TdxSkinInfo;
  ASkinElement: TdxSkinElement;
begin
  if GetSkinPainterData(ASkinInfo) then
    ASkinElement := ASkinInfo.RibbonGalleryBackground
  else
    ASkinElement := nil;

  if ASkinElement = nil then
    inherited DropDownGalleryDrawBackground(DC, R)
  else
    ASkinElement.Draw(DC, R);
end;

procedure TdxBarSkinPainter.DropDownGalleryDrawBorder(
  ABarSubMenuControl: TdxBarSubMenuControl; DC: HDC; const R: TRect);
var
  ASkinInfo: TdxSkinInfo;
  ASkinElement: TdxSkinElement;
  ARect: TRect;
  R1: TRect;
begin
  R1 := Rect(2, 2, 2, 2);
  cxPaintCanvas.BeginPaint(DC);
  try
    ARect := cxRectOffset(ABarSubMenuControl.ClientRect,
      TdxBarSubMenuControlAccess(ABarSubMenuControl).GetClientOffset.TopLeft);
    cxPaintCanvas.ExcludeClipRect(ARect);
    if GetSkinPainterData(ASkinInfo) then
      ASkinElement := ASkinInfo.PopupMenu
    else
      ASkinElement := nil;

    if ASkinElement = nil then
      inherited DropDownGalleryDrawBorder(ABarSubMenuControl, DC, R)
    else
      ASkinElement.Draw(DC, R);
    cxPaintCanvas.RestoreClipRegion;
  finally
    cxPaintCanvas.EndPaint;
  end;
end;

procedure TdxBarSkinPainter.DropDownGalleryDrawBottomSizeGrip(DC: HDC; const R: TRect);
var
  ASkinInfo: TdxSkinInfo;
  ASkinElement: TdxSkinElement;
begin
  if GetSkinPainterData(ASkinInfo) then
    ASkinElement := ASkinInfo.RibbonGallerySizeGrips
  else
    ASkinElement := nil;

  if ASkinElement = nil then
    inherited DropDownGalleryDrawBottomSizeGrip(DC, R)
  else
    ASkinElement.Draw(DC, DropDownGalleryGetSizeGripRect(R, coBottomRight), 1);
end;

procedure TdxBarSkinPainter.DropDownGalleryDrawBottomSizeGripEx(ABarSubMenuControl: TdxBarSubMenuControl; DC: HDC; const R: TRect);
const
  SizeGripCorner: array [Boolean] of TdxCorner = (coBottomRight, coBottomLeft);
var
  ASkinInfo: TdxSkinInfo;
  ASkinElement: TdxSkinElement;
begin
  if GetSkinPainterData(ASkinInfo) then
    ASkinElement := ASkinInfo.RibbonGallerySizeGrips
  else
    ASkinElement := nil;

  if ASkinElement = nil then
    inherited DropDownGalleryDrawBottomSizeGripEx(ABarSubMenuControl, DC, R)
  else
    DrawSkinElement(ASkinElement, DC, DropDownGalleryGetSizeGripRect(R, SizeGripCorner[ABarSubMenuControl.UseRightToLeftAlignment]),
      dxSystemScaleFactor, 1, esNormal, ABarSubMenuControl.UseRightToLeftAlignment);
end;

procedure TdxBarSkinPainter.DropDownGalleryDrawBottomSizingBand(DC: HDC; const R: TRect);
var
  ASkinInfo: TdxSkinInfo;
  ASkinElement: TdxSkinElement;
begin
  if GetSkinPainterData(ASkinInfo) then
    ASkinElement := ASkinInfo.RibbonGallerySizingPanel
  else
    ASkinElement := nil;

  if ASkinElement = nil then
    inherited DropDownGalleryDrawBottomSizingBand(DC, R)
  else
    ASkinElement.Draw(DC, R);
end;

procedure TdxBarSkinPainter.DropDownGalleryDrawBottomVerticalSizeGrip(DC: HDC; const R: TRect);
var
  ASkinInfo: TdxSkinInfo;
  ASkinElement: TdxSkinElement;
begin
  if GetSkinPainterData(ASkinInfo) then
    ASkinElement := ASkinInfo.RibbonGallerySizeGrips
  else
    ASkinElement := nil;

  if ASkinElement = nil then
    inherited DropDownGalleryDrawBottomVerticalSizeGrip(DC, R)
  else
    ASkinElement.Draw(DC, R);
end;

procedure TdxBarSkinPainter.DropDownGalleryDrawFilterBand(DC: HDC; const R: TRect);
begin
  DropDownGalleryDrawGroupHeaderBackground(DC, R);
end;

procedure TdxBarSkinPainter.DropDownGalleryDrawGroupHeaderBackground(DC: HDC; const R: TRect);
var
  ASkinInfo: TdxSkinInfo;
  ASkinElement: TdxSkinElement;
begin
  if GetSkinPainterData(ASkinInfo) then
    ASkinElement := ASkinInfo.RibbonGalleryGroupCaption
  else
    ASkinElement := nil;

  if ASkinElement = nil then
    inherited DropDownGalleryDrawGroupHeaderBackground(DC, R)
  else
    ASkinElement.Draw(DC, R);
end;

procedure TdxBarSkinPainter.DropDownGalleryDrawItem(DC: HDC; const R: TRect; AState: Integer; AScaleFactor: TdxScaleFactor);

  function GetSkinElementState(AState: Integer): TdxSkinElementState;
  const
    StateMap: array[0..8] of TdxSkinElementState = (
      esNormal, esDisabled, esHot, esActive, esPressed,
      esChecked, esChecked, esHotCheck, esActiveDisabled
    );
  begin
    if (Low(StateMap) <= AState) and (High(StateMap) >= AState) then
      Result := StateMap[AState]
    else
      Result := esNormal;
  end;

var
  ASkinInfo: TdxSkinInfo;
  ASkinElement: TdxSkinElement;
begin
  if GetSkinPainterData(ASkinInfo) then
    ASkinElement := ASkinInfo.RibbonSmallButton
  else
    ASkinElement := nil;

  if ASkinElement = nil then
    inherited DropDownGalleryDrawItem(DC, R, AState, AScaleFactor)
  else
  begin
    ASkinElement.UseCache := True;
    ASkinElement.Draw(DC, R, 0, dxSkinElementCheckState(ASkinElement, GetSkinElementState(AState)));
  end;
end;

procedure TdxBarSkinPainter.DropDownGalleryDrawScrollBarBackgroundEx(ABarSubItemControl: TdxBarSubItemControl; DC: HDC; const R: TRect; AScaleFactor: TdxScaleFactor);
var
  ASkinInfo: TdxSkinInfo;
  ASkinElement: TdxSkinElement;
  R1: TRect;
  AIsRightToLeft: Boolean;
begin
  if GetSkinPainterData(ASkinInfo) then
    ASkinElement := ASkinInfo.RibbonGalleryPane
  else
    ASkinElement := nil;

  if ASkinElement = nil then
    inherited DropDownGalleryDrawScrollBarBackgroundEx(ABarSubItemControl, DC, R, AScaleFactor)
  else
  begin
    R1 := R;
    Dec(R1.Left, Max(ASkinElement.Size.cx, ASkinElement.Image.Margins.Left));
    AIsRightToLeft := (ABarSubItemControl <> nil) and ABarSubItemControl.UseRightToLeftAlignment;
    if AIsRightToLeft then
      R1 := TdxRightToLeftLayoutConverter.ConvertRect(R1, R);
    DrawSkinElement(ASkinElement, DC, R1, dxSystemScaleFactor, 0, esNormal, AIsRightToLeft);
  end;
end;

procedure TdxBarSkinPainter.DropDownGalleryDrawSelectionFrame(ACanvas: TcxCanvas; const R: TRect; AState: Integer);

  function GetSkinElementState: TdxSkinElementState;
  begin
    case AState of
      DXBAR_HOTCHECK:
        Result := esHotCheck;
      DXBAR_CHECKED:
        Result := esChecked;
    else
      Result := esHot;
    end;
  end;

var
  ASkinInfo: TdxSkinInfo;
begin
  if GetSkinPainterData(ASkinInfo) and (ASkinInfo.GalleryItem <> nil) then
  begin
    ACanvas.SaveClipRegion;
    try
      ACanvas.ExcludeClipRect(cxRectInflate(R, -2));
      DrawSkinElement(ASkinInfo.GalleryItem, ACanvas.Handle, R, dxDefaultScaleFactor, 0,
        dxSkinElementCheckState(ASkinInfo.GalleryItem, GetSkinElementState));
    finally
      ACanvas.RestoreClipRegion;
    end;
  end
  else
    inherited DropDownGalleryDrawSelectionFrame(ACanvas, R, AState);
end;

procedure TdxBarSkinPainter.DropDownGalleryDrawTopSizeGrip(DC: HDC; const R: TRect);
var
  ASkinInfo: TdxSkinInfo;
  ASkinElement: TdxSkinElement;
begin
  if GetSkinPainterData(ASkinInfo) then
    ASkinElement := ASkinInfo.RibbonGallerySizeGrips
  else
    ASkinElement := nil;

  if ASkinElement = nil then
    inherited DropDownGalleryDrawTopSizeGrip(DC, R)
  else
    ASkinElement.Draw(DC, DropDownGalleryGetSizeGripRect(R, coTopRight), 2);
end;

procedure TdxBarSkinPainter.DropDownGalleryDrawTopSizeGripEx(ABarSubMenuControl: TdxBarSubMenuControl; DC: HDC; const R: TRect);
const
  SizeGripCorner: array [Boolean] of TdxCorner = (coTopRight, coTopLeft);
var
  ASkinInfo: TdxSkinInfo;
  ASkinElement: TdxSkinElement;
begin
  if GetSkinPainterData(ASkinInfo) then
    ASkinElement := ASkinInfo.RibbonGallerySizeGrips
  else
    ASkinElement := nil;

  if ASkinElement = nil then
    inherited DropDownGalleryDrawTopSizeGripEx(ABarSubMenuControl, DC, R)
  else
    DrawSkinElement(ASkinElement, DC, DropDownGalleryGetSizeGripRect(R, SizeGripCorner[ABarSubMenuControl.UseRightToLeftAlignment]),
      dxSystemScaleFactor, 2, esNormal, ABarSubMenuControl.UseRightToLeftAlignment);
end;

procedure TdxBarSkinPainter.DropDownGalleryDrawTopSizingBand(DC: HDC; const R: TRect);
var
  ASkinInfo: TdxSkinInfo;
  ASkinElement: TdxSkinElement;
begin
  if GetSkinPainterData(ASkinInfo) then
    ASkinElement := ASkinInfo.RibbonGallerySizingPanel
  else
    ASkinElement := nil;

  if ASkinElement = nil then
    inherited DropDownGalleryDrawTopSizingBand(DC, R)
  else
    ASkinElement.Draw(DC, R);
end;

procedure TdxBarSkinPainter.DropDownGalleryDrawTopVerticalSizeGrip(DC: HDC; const R: TRect);
var
  ASkinInfo: TdxSkinInfo;
  ASkinElement: TdxSkinElement;
begin
  if GetSkinPainterData(ASkinInfo) then
    ASkinElement := ASkinInfo.RibbonGallerySizeGrips
  else
    ASkinElement := nil;

  if ASkinElement = nil then
    inherited DropDownGalleryDrawTopVerticalSizeGrip(DC, R)
  else
    ASkinElement.Draw(DC, R);
end;

function TdxBarSkinPainter.DropDownGalleryGetClientBorderSize: Integer;
begin
  Result := 0;
end;

function TdxBarSkinPainter.DropDownGalleryGetContentOffsets(APart: Integer): TRect;
var
  ASkinInfo: TdxSkinInfo;
  ASkinElement: TdxSkinElement;
begin
   if GetSkinPainterData(ASkinInfo) then
    case APart of
      DXBAR_GALLERYFILTERBAND:
        ASkinElement := ASkinInfo.RibbonGallerySizingPanel;
    else
      ASkinElement := nil;
    end
  else
    ASkinElement := nil;

  if ASkinElement = nil then
    Result := inherited DropDownGalleryGetContentOffsets(APart)
  else
    Result := ASkinElement.ContentOffset.Rect;
end;

function TdxBarSkinPainter.DropDownGalleryGetFilterBandSeparatorColor: TColor;
var
  ASkinInfo: TdxSkinInfo;
begin
  if GetSkinPainterData(ASkinInfo) then
    Result := ASkinInfo.RibbonGalleryGroupCaption.TextColor
  else
    Result := inherited DropDownGalleryGetFilterBandSeparatorColor;
end;

function TdxBarSkinPainter.DropDownGalleryGetFilterBandTextColor(AState: Integer): TColor;
begin
  Result := DropDownGalleryGetFilterBandSeparatorColor;
end;

function TdxBarSkinPainter.DropDownGalleryGetGroupHeaderTextColor: TColor;
var
  ASkinInfo: TdxSkinInfo;
  ASkinElement: TdxSkinElement;
begin
  if GetSkinPainterData(ASkinInfo) then
    ASkinElement := ASkinInfo.RibbonGalleryGroupCaption
  else
    ASkinElement := nil;

  if ASkinElement = nil then
    Result := inherited DropDownGalleryGetGroupHeaderTextColor
  else
    Result := ASkinElement.TextColor;
end;

function TdxBarSkinPainter.DropDownGalleryGetNCBorderSize: Integer;
begin
  Result := 2;
end;

procedure TdxBarSkinPainter.DropDownGalleryItemGetTextColors(ABarItemControl: TdxBarItemControl;
  AEnabled, ASelected, AFlat: Boolean; var AColor1, AColor2: TColor);
var
  ADrawParams: TdxBarItemControlDrawParams;
  ASkinInfo: TdxSkinInfo;
begin
  if GetSkinPainterData(ASkinInfo) and not TdxBarSubItemControlAccess(ABarItemControl).GetCollapsed then
    if AEnabled then
    begin
      ADrawParams := TdxBarItemControlAccess(ABarItemControl).FDrawParams;
      if TdxBarSkinnedPainterAccess.GetPartState(ADrawParams, icpControl) <> DXBAR_NORMAL then
        AColor1 := GetSkinElementTextColor(ASkinInfo.RibbonSmallButton, cxbsHot)
      else
        AColor1 := clDefault;
      AColor1 := cxGetActualColor(AColor1, GetSkinElementTextColor(ASkinInfo.RibbonGalleryBackground, cxbsNormal));
      AColor2 := AColor1;
    end
    else
      GetDisabledTextColors(ABarItemControl, ASelected, AFlat, AColor1, AColor2)
  else
    inherited DropDownGalleryItemGetTextColors(ABarItemControl, AEnabled, ASelected, AFlat, AColor1, AColor2);
end;

class function TdxBarSkinPainter.DropDownListBoxBorderSize: Integer;
begin
  Result := 1;
end;

procedure TdxBarSkinPainter.DropDownListBoxDrawBorder(DC: HDC; AColor: TColor; ARect: TRect);
begin
  cxPaintCanvas.BeginPaint(DC);
  try
    cxPaintCanvas.FrameRect(ARect, FSkinPainter.GetContainerBorderColor(False));
  finally
    cxPaintCanvas.EndPaint;
  end;
end;

class function TdxBarSkinPainter.SubMenuControlHasBand: Boolean;
begin
  Result := True;
end;

class function TdxBarSkinPainter.SubMenuControlArrowsOffset: Integer;
begin
  Result := 1;  // TdxBarXPPainter
end;

function TdxBarSkinPainter.SubMenuControlBeginGroupRect(
  ABarSubMenuControl: TdxBarSubMenuControl; AControl: TdxBarItemControl;
  const AItemRect: TRect): TRect;
begin
  Result := AItemRect;
  Result.Bottom := Result.Top;
  Dec(Result.Top, TdxBarSubMenuControlAccess(ABarSubMenuControl).BeginGroupSize);  // TdxBarXPPainter
end;

function TdxBarSkinPainter.SubMenuControlBeginGroupSize: Integer;
begin
  Result := GetSkinElementSize(PopupMenuSeparator, dxDefaultScaleFactor).cy;
end;

procedure TdxBarSkinPainter.SubMenuControlCalcDrawingConsts(ACanvas: TcxCanvas;
  ATextSize: Integer; AScaleFactor: TdxScaleFactor; out AMenuArrowWidth, AMarkSize: Integer);
begin
  inherited;
  if PopupMenuExpandButton <> nil then
    AMarkSize := GetSkinElementSize(PopupMenuExpandButton, dxDefaultScaleFactor).cy;
end;

class function TdxBarSkinPainter.SubMenuControlDetachCaptionAreaSize(ABarSubMenuControl: TdxBarSubMenuControl): Integer;
begin
  Result := TdxBarSubMenuControlAccess(ABarSubMenuControl).DetachCaptionSize + 1;// TdxBarXPPainter
end;

class procedure TdxBarSkinPainter.SubMenuControlOffsetDetachCaptionRect(ABarSubMenuControl: TdxBarSubMenuControl; var R: TRect);
begin
  R := cxRectInflate(R, TdxBarSubMenuControlAccess(ABarSubMenuControl).ScaleFactor.Apply(-2)); // TdxBarXPPainter
end;

class function TdxBarSkinPainter.SubMenuControlGetItemTextIndent(const ADrawParams: TdxBarItemControlDrawParams): Integer;
begin
  Result := TdxBarFlatPainter.SubMenuControlGetItemTextIndent(ADrawParams);
end;

procedure TdxBarSkinPainter.SubMenuControlDrawBeginGroup(ABarSubMenuControl: TdxBarSubMenuControl;
  AControl: TdxBarItemControl; ACanvas: TcxCanvas; const ABeginGroupRect: TRect);
var
  ARect: TRect;
begin
  ARect := cxRect(ABeginGroupRect.Left + SubMenuBeginGroupIndent + TdxBarItemControlAccess(AControl).TextAreaOffset,
    ABeginGroupRect.Bottom - SubMenuGetSeparatorSize,
    ABeginGroupRect.Right,
    ABeginGroupRect.Bottom);
  if ABarSubMenuControl.UseRightToLeftAlignment then
    ARect := TdxRightToLeftLayoutConverter.ConvertRect(ARect, ABeginGroupRect);
  DrawBackground(AControl, ACanvas.Handle, ABeginGroupRect, 0, False);
  DrawSkinElement(PopupMenuSeparator, ACanvas.Handle, ARect, dxGetScaleFactor(ABarSubMenuControl), 0, esNormal,
    ABarSubMenuControl.UseRightToLeftAlignment);
end;

procedure TdxBarSkinPainter.SubMenuControlDrawBorder(
  ABarSubMenuControl: TdxBarSubMenuControl; DC: HDC; R: TRect);
var
  ABarSubMenuControlAccess: TdxBarSubMenuControlAccess;
begin
  ABarSubMenuControlAccess := TdxBarSubMenuControlAccess(ABarSubMenuControl);
  cxPaintCanvas.BeginPaint(DC);
  try
    cxPaintCanvas.ExcludeClipRect(cxRectContent(R, ABarSubMenuControlAccess.GetClientOffset));
    DrawSkinElement(PopupMenu, DC, R, dxGetScaleFactor(ABarSubMenuControl));
    SubMenuControlDrawDetachCaption(ABarSubMenuControl, DC, ABarSubMenuControlAccess.DetachCaptionRect);
  finally
    cxPaintCanvas.EndPaint;
  end;
end;

procedure TdxBarSkinPainter.SubMenuControlDrawClientBorder(
  ABarSubMenuControl: TdxBarSubMenuControl; DC: HDC; const R: TRect; ABrush: HBRUSH);
var
  ASaveIndex: Integer;
begin
  ASaveIndex := SaveDC(DC);
  try
    IntersectClipRect(DC, R.Left, R.Top, R.Right, R.Bottom);
    DrawSkinElement(PopupMenu, DC,
      cxRectInflate(R, TdxBarSubMenuControlAccess(ABarSubMenuControl).GetClientOffset),
      dxGetScaleFactor(ABarSubMenuControl));
  finally
    RestoreDC(DC, ASaveIndex);
  end;
end;

procedure TdxBarSkinPainter.SubMenuControlDrawBackground(ABarSubMenuControl: TdxBarSubMenuControl;
  ACanvas: TcxCanvas; ARect: TRect; ABrush: HBRUSH; AColor: TColor);
var
  ASubMenuControlAccess: TdxBarSubMenuControlAccess;
  ASideStripElement: TdxSkinElement;
  AContentRect, ABarRect, ABandRect, ASeparatorRect: TRect;
begin
  if not TdxBarSubMenuControlAccess(ABarSubMenuControl).GetBackgroundBitmap.Empty then
    inherited
  else
  begin
    ASubMenuControlAccess := TdxBarSubMenuControlAccess(ABarSubMenuControl);
    AContentRect := ASubMenuControlAccess.ContentRect;

    ABarRect := ASubMenuControlAccess.BarRect;

    if ABarSubMenuControl.UseRightToLeftAlignment then
    begin
      ABandRect := Rect(ABarRect.Left - ASubMenuControlAccess.BandSize - ASubMenuControlAccess.GetIndent2, AContentRect.Top,
        ABarRect.Left, AContentRect.Bottom);

      ASeparatorRect := Rect(ABandRect.Left - 2, AContentRect.Top,
        ABandRect.Left, AContentRect.Bottom);   //  dxBar.MenuSeparatorSize = 2

      ABandRect.Left := ABandRect.Left - cxRectWidth(ASeparatorRect);
    end
    else
    begin
      ABandRect := Rect(ABarRect.Right, AContentRect.Top,
        ABarRect.Right + ASubMenuControlAccess.BandSize + ASubMenuControlAccess.GetIndent2, AContentRect.Bottom);

      ASeparatorRect := Rect(ABandRect.Right, AContentRect.Top,
        ABandRect.Right + 2, AContentRect.Bottom);   //  dxBar.MenuSeparatorSize = 2

      ABandRect.Right := ABandRect.Right + cxRectWidth(ASeparatorRect);
    end;

    if ASubMenuControlAccess.NonRecent then
      ASideStripElement := PopupMenuSideStripNonRecent
    else
      ASideStripElement := PopupMenuSideStrip;

    DrawSkinElement(PopupMenu, ACanvas.Handle,
      cxRectInflate(ASubMenuControlAccess.ContentRect, SubMenuControlBorderSize, SubMenuControlBorderSize),
      dxGetScaleFactor(ABarSubMenuControl));
    DrawSkinElement(ASideStripElement, ACanvas.Handle, ABandRect, dxGetScaleFactor(ABarSubMenuControl),
      0, esNormal, ABarSubMenuControl.UseRightToLeftAlignment);
  end;
end;

procedure TdxBarSkinPainter.SubMenuControlDrawDetachCaption(ABarSubMenuControl: TdxBarSubMenuControl; DC: HDC; R: TRect);
begin
  DrawSkinElement(PopupMenuSeparator, DC, R, dxGetScaleFactor(ABarSubMenuControl));
end;

procedure TdxBarSkinPainter.SubMenuControlDrawMarkContent(
  ABarSubMenuControl: TdxBarSubMenuControl; DC: HDC; R: TRect; ASelected: Boolean);
begin
  DrawSkinElement(PopupMenuExpandButton, DC, R,dxGetScaleFactor(ABarSubMenuControl));
end;

procedure TdxBarSkinPainter.SubMenuControlDrawSeparator(ACanvas: TcxCanvas; const ARect: TRect);
begin
  if PopupMenuSeparator = nil then
    inherited SubMenuControlDrawSeparator(ACanvas, ARect)
  else
  begin
    if PopupMenuSeparator.IsAlphaUsed then
      SubMenuControlDrawSeparatorBackground(ACanvas, ARect);
    DrawSkinElement(PopupMenuSeparator, ACanvas.Handle, ARect, dxDefaultScaleFactor, 0, esNormal, ACanvas.UseRightToLeftAlignment);
  end;
end;

procedure TdxBarSkinPainter.SubMenuControlDrawSeparatorBackground(ACanvas: TcxCanvas; const ARect: TRect);
var
  R: TRect;
  APopupElement: TdxSkinElement;
begin
  APopupElement := PopupMenu;
  if APopupElement <> nil then
  begin
    ACanvas.SaveClipRegion;
    try
      ACanvas.IntersectClipRect(ARect);
      with APopupElement.Image.Margins.Margin do
        R := Rect(ARect.Left - Max(Left, APopupElement.Borders.Left.Thin),
          ARect.Top - Max(Top, APopupElement.Borders.Top.Thin),
          ARect.Right + Max(Right, APopupElement.Borders.Right.Thin),
          ARect.Bottom + Max(Bottom, APopupElement.Borders.Bottom.Thin));
      DrawSkinElementContent(APopupElement, ACanvas.Handle, R, dxDefaultScaleFactor);
    finally
      ACanvas.RestoreClipRegion;
    end;
  end;
end;

procedure TdxBarSkinPainter.BarDrawMark(ABarControl: TdxBarControl; DC: HDC; MarkR: TRect);
begin
  cxPaintCanvas.BeginPaint(DC);
  try
    cxPaintCanvas.SetClipRegion(TcxRegion.Create(MarkR), roIntersect);
    BarCaptionFillBackground(ABarControl, cxPaintCanvas.Handle, MarkR, 0);
    DrawFloatingBarCaptionButton(cxPaintCanvas.Handle, MarkR, 6, TdxBarControlAccess(ABarControl).MarkState);
  finally
    cxPaintCanvas.EndPaint;
  end;
end;

procedure TdxBarSkinPainter.BarDrawMarkElements(ABarControl: TdxBarControl;
  DC: HDC; ItemRect: TRect);
begin
  // todo
end;

class function TdxBarSkinPainter.IsQuickControlPopupOnRight: Boolean;
begin
  Result := True; // TdxBarXPPainter
end;

function TdxBarSkinPainter.IsButtonControlArrowBackgroundOpaque(const ADrawParams: TdxBarButtonLikeControlDrawParams): Boolean;
begin
  Result := False;
end;

function TdxBarSkinPainter.IsButtonControlArrowDrawSelected(const ADrawParams: TdxBarButtonLikeControlDrawParams): Boolean;
begin
  Result := False;
end;

function TdxBarSkinPainter.IsDropDownRepaintNeeded: Boolean;
begin
  Result := False;
end;

class procedure TdxBarSkinPainter.CorrectButtonControlDefaultHeight(var DefaultHeight: Integer);
begin
  Inc(DefaultHeight, 5);
end;

class procedure TdxBarSkinPainter.CorrectButtonControlDefaultWidth(var DefaultWidth: Integer);
begin
  Inc(DefaultWidth, 9);
end;

procedure TdxBarSkinPainter.DrawButtonControlArrowBackground(
  const ADrawParams: TdxBarButtonLikeControlDrawParams;
  var R1: TRect; ABrush: HBRUSH);
var
  ASkinElement: TdxSkinElement;
begin
  if not ButtonLikeDrawArrowFadingElement(ADrawParams, R1) then
  begin
    inherited DrawButtonControlArrowBackground(ADrawParams, R1, ABrush);
    if ((ADrawParams.PaintType = ptMenu) or not ADrawParams.DroppedDown) and
        (ADrawParams.DrawSelected or (ADrawParams.PaintType <> ptMenu) and
        ADrawParams.Downed)
    then
    begin
      if ADrawParams.PaintType = ptMenu then
        ASkinElement := PopupMenuSplitButton2
      else
        ASkinElement := LinkSelected;
      DrawArrowButtonElement(ASkinElement, ADrawParams.Canvas, R1, 0,
        ButtonLikeControlGetState(ADrawParams));
    end;
  end;
end;

function TdxBarSkinPainter.ColorComboHasCompleteFrame: Boolean;
begin
  Result := True;
end;

function TdxBarSkinPainter.GetCustomColorButtonIndents(APaintType: TdxBarPaintType): TRect;
begin
  Result := EditControlBorderOffsets(APaintType);
  Result.Left := 0;
end;

function TdxBarSkinPainter.GetCustomColorButtonWidth(APaintType: TdxBarPaintType; const ARect: TRect): Integer;
begin
  Result := 17;
end;

procedure TdxBarSkinPainter.ColorComboDrawCustomButton(const ADrawParams: TdxBarColorComboControlDrawParams; ARect: TRect);

  function GetState: TcxButtonState;
  begin
    with ADrawParams do
      if not Enabled then
        Result := cxbsDisabled
      else
        if IsPressed then
          Result := cxbsPressed
        else
          if DrawSelected then
            Result := cxbsHot
          else
            Result := cxbsNormal;
  end;

begin
  FSkinPainter.DrawScaledEditorButton(ADrawParams.Canvas, ARect, cxbkEllipsisBtn, GetState, ADrawParams.ScaleFactor);
  FSkinPainter.DrawScaledEditorButtonGlyph(ADrawParams.Canvas, ARect, cxbkEllipsisBtn, GetState, ADrawParams.ScaleFactor);
end;

procedure TdxBarSkinPainter.ColorComboDrawCustomButtonAdjacentZone(const ADrawParams: TdxBarColorComboControlDrawParams; ARect: TRect);
begin
  FillRectByColor(ADrawParams.Canvas.Handle, cxRectContent(ARect, GetCustomColorButtonIndents(ADrawParams.PaintType)), EditGetBkColor(ADrawParams));
end;

class function TdxBarSkinPainter.EditControlCaptionBackgroundIsOpaque(const ADrawParams: TdxBarEditLikeControlDrawParams): Boolean;
begin
  Result := False;
end;

class function TdxBarSkinPainter.EditControlCaptionRightIndentIsOpaque(const ADrawParams: TdxBarEditLikeControlDrawParams): Boolean;
begin
  Result := EditControlCaptionBackgroundIsOpaque(ADrawParams);
end;

class function TdxBarSkinPainter.EditControlBorderOffsets(APaintType: TdxBarPaintType): TRect;
begin
  if APaintType = ptMenu then
    Result := Rect(1, 2, 1, 2)
  else
    Result := dxBarFlatPainter.EditControlBorderOffsets(APaintType);
end;

procedure TdxBarSkinPainter.EditControlDrawBackground(const ADrawParams: TdxBarEditLikeControlDrawParams);
begin
  with ADrawParams do
  begin
    Canvas.SaveClipRegion;
    Canvas.ExcludeClipRect(EditControlGetContentRect(PaintType, TdxBarEditControlAccess(BarEditControl).GetEditRect));
    DrawBackground(BarEditControl, Canvas.Handle, BarEditControl.ItemBounds, 0, False);
    Canvas.RestoreClipRegion;
  end;
end;

procedure TdxBarSkinPainter.EditControlDrawBorder(const ADrawParams: TdxBarEditLikeControlDrawParams; var ARect: TRect);
begin
  with ADrawParams do
  begin
    ARect := cxRectContent(ARect, EditControlBorderOffsets(PaintType));                     // TODO cxProgress: TdxBarItemControlAccess(BarEditControl).CanSelect
    if not IsTransparent or (IsTransparent and DrawSelected and not (PaintType = ptMenu)) then
      FrameRectByColor(Canvas.Handle, cxRectInflate(ARect, 1, 1), FSkinPainter.GetContainerBorderColor(DrawSelected and (PaintType <> ptMenu)));
  end;
end;

procedure TdxBarSkinPainter.EditControlDrawSelectionFrame(const ADrawParams: TdxBarEditLikeControlDrawParams; const ARect: TRect);
const
  State: array [Boolean] of TdxSkinElementState = (esDisabled, esHot);
var
  AExcludedRect: TRect;
begin
  with ADrawParams do
  begin
    Canvas.SaveClipRegion;
    AExcludedRect := EditControlGetContentRect(PaintType, TdxBarEditControlAccess(BarEditControl).GetEditRect);
    if IsTransparent and DrawSelected and (PaintType = ptMenu) then
      InflateRect(AExcludedRect, 1, 1);
    Canvas.ExcludeClipRect(AExcludedRect);
    try
      DrawSkinElement(PopupMenuLinkSelected, Canvas.Handle, ARect, dxGetScaleFactor(BarItemControl), 0, State[Enabled]);
    finally
      Canvas.RestoreClipRegion;
    end;
  end;
end;

class function TdxBarSkinPainter.EditControlCaptionSimpleIndent(const ADrawParams: TdxBarEditLikeControlDrawParams): Integer;
begin
  Result := TdxBarOffice11Painter.EditControlCaptionSimpleIndent(ADrawParams);
end;

class procedure TdxBarSkinPainter.CustomComboDrawItem(ABarCustomCombo: TdxBarCustomCombo;
  ACanvas: TCanvas; AIndex: Integer; ARect: TRect; AState: TOwnerDrawState;
  AInteriorIsDrawing: Boolean);
begin
  TdxBarFlatPainter.CustomComboDrawItem(ABarCustomCombo, ACanvas, AIndex, ARect, AState, AInteriorIsDrawing);
end;

class function TdxBarSkinPainter.ComboControlArrowOffset: Integer;
begin
  Result := 0; // TdxBarXPPainter
end;

procedure TdxBarSkinPainter.ComboControlDrawArrowButton(const ADrawParams: TdxBarEditLikeControlDrawParams; ARect: TRect; AInClientArea: Boolean);

  function GetState: TcxButtonState;
  begin
    with ADrawParams do
      if not Enabled then
        Result := cxbsDisabled
      else
        if DroppedDown then
          Result := cxbsPressed
        else
          if DrawSelected then
            Result := cxbsHot
          else
            Result := cxbsNormal;
  end;

var
  AButtonPosition: TcxEditBtnPosition;
begin
  FillRectByColor(ADrawParams.Canvas.Handle, ARect, EditGetBkColor(ADrawParams));
  if ADrawParams.Canvas.UseRightToLeftAlignment then
    AButtonPosition := cxbpLeft
  else
    AButtonPosition := cxbpRight;
  FSkinPainter.DrawScaledEditorButton(ADrawParams.Canvas, ARect, cxbkComboBtn, GetState, ADrawParams.ScaleFactor, AButtonPosition);
  FSkinPainter.DrawScaledEditorButtonGlyph(ADrawParams.Canvas, ARect, cxbkComboBtn, GetState, ADrawParams.ScaleFactor, AButtonPosition);
end;

class function TdxBarSkinPainter.IsDateNavigatorFlat: Boolean;
begin
  Result := True;
end;

function TdxBarSkinPainter.DateNavigatorHeaderColor: TColor;
begin
  if FloatingBar <> nil then
    Result := FloatingBar.Color
  else
    Result := inherited DateNavigatorHeaderColor;
end;

procedure TdxBarSkinPainter.DateNavigatorDrawButton(ABarItem: TdxBarItem;
  DC: HDC; R: TRect; const ACaption: string; APressed: Boolean; AScaleFactor: TdxScaleFactor);
const
  State: array [Boolean] of TcxButtonState = (cxbsNormal, cxbsPressed);
begin
  cxPaintCanvas.BeginPaint(DC);
  try
    FillRect(cxPaintCanvas.Handle, R, GetSysColorBrush(COLOR_WINDOW));
    FSkinPainter.DrawScaledButton(cxPaintCanvas, R, ACaption, State[APressed], AScaleFactor);
  finally
    cxPaintCanvas.EndPaint;
  end;
end;

function TdxBarSkinPainter.InPlaceSubItemControlIsArrowSelected(const ADrawParams: TdxBarInPlaceSubItemControlDrawParams): Boolean;
begin
  Result := False;
end;

procedure TdxBarSkinPainter.InPlaceSubItemControlDrawBackground(const ADrawParams: TdxBarInPlaceSubItemControlDrawParams; ARect: TRect);
begin
  if ADrawParams.DrawSelected then
    DrawItemBackgroundInSubMenu(ADrawParams, ARect)
  else
  begin
    DrawBackground(ADrawParams.BarItemControl, ADrawParams.Canvas.Handle, ARect, 0, False);
    DrawSkinElementContent(PopupMenuSideStrip, ADrawParams.Canvas.Handle, ARect, dxGetScaleFactor(ADrawParams.BarItemControl));
  end;
end;

function TdxBarSkinPainter.GetSpinEditButtonWidth(APaintType: TdxBarPaintType; const ARect: TRect): Integer;
begin
  Result := 17;
end;

function TdxBarSkinPainter.GetSpinEditButtonIndents(APaintType: TdxBarPaintType): TRect;
begin
  Result := EditControlBorderOffsets(APaintType);
  Result.Left := 0;
end;

procedure TdxBarSkinPainter.SpinEditControlDrawButton(
  const ADrawParams: TdxBarSpinEditDrawParams; ARect: TRect; AButtonIndex: Integer);
const
  ButtonKind: array [Boolean] of TcxEditBtnKind = (cxbkSpinDownBtn, cxbkSpinUpBtn);
begin
  FSkinPainter.DrawScaledEditorButton(ADrawParams.Canvas, ARect,
    ButtonKind[AButtonIndex = secButtonUp], GetSpinEditButtonState(ADrawParams, AButtonIndex), ADrawParams.ScaleFactor);
  FSkinPainter.DrawScaledEditorButtonGlyph(ADrawParams.Canvas, ARect,
    ButtonKind[AButtonIndex = secButtonUp], GetSpinEditButtonState(ADrawParams, AButtonIndex), ADrawParams.ScaleFactor);
end;

procedure TdxBarSkinPainter.SpinEditControlDrawButtonsAdjacentZone(const ADrawParams: TdxBarSpinEditDrawParams; const ARect: TRect);
begin
  FillRectByColor(ADrawParams.Canvas.Handle, cxRectContent(ARect, GetSpinEditButtonIndents(ADrawParams.PaintType)), EditGetBkColor(ADrawParams));
end;

procedure TdxBarSkinPainter.CalculateSpinEditParts(const ADrawParams: TdxBarSpinEditDrawParams;
  var AParts, AAreaParts: array of TRect);
begin
  inherited;
  AParts[ecpEdit].Right := AParts[secButtonUp].Left;
end;

procedure TdxBarSkinPainter.SpinEditCorrectFrameRect(const ADrawParams: TdxBarItemControlDrawParams; var ARect: TRect);
begin
  // do nothing
end;

function TdxBarSkinPainter.ProgressControlBarHeight(ABarItemControl: TdxBarItemControl): Integer;
begin
  Result := 24;
end;

class function TdxBarSkinPainter.ProgressControlIndent(const ADrawParams: TdxBarItemControlDrawParams): Integer;
begin
  Result := TdxBarOffice11Painter.ProgressControlIndent(ADrawParams);
end;

procedure TdxBarSkinPainter.ProgressControlDrawBackground(const ADrawParams: TdxBarItemControlDrawParams; var BarR: TRect);
begin
  DrawBackground(ADrawParams.BarItemControl, ADrawParams.Canvas.Handle, BarR, 0, False);
  FSkinPainter.DrawProgressBarBorder(ADrawParams.Canvas, BarR, ADrawParams.PaintType = ptVert);
  BarR := cxRectContent(BarR, FSkinPainter.ProgressBarBorderSize(ADrawParams.PaintType = ptVert));
end;

procedure TdxBarSkinPainter.ProgressControlFillContent(const ADrawParams: TdxBarItemControlDrawParams; const R: TRect; ABarBrush: HBRUSH);
begin
  FSkinPainter.DrawProgressBarChunk(ADrawParams.Canvas, R, ADrawParams.PaintType = ptVert);
end;

function TdxBarSkinPainter.StaticControlGetBorderOffsets(AParent: TCustomdxBarControl; ABorderStyle: TdxBarStaticBorderStyle): TRect;
begin
  if (LinkBorderPainter <> nil) and TCustomdxBarControlAccess(AParent).IsStatusBar then
    Result := LinkBorderPainter.ContentOffset.Rect
  else
    Result := inherited StaticControlGetBorderOffsets(AParent, ABorderStyle);
end;

procedure TdxBarSkinPainter.DrawStaticBackground(const ADrawParams: TdxBarStaticLikeControlDrawParams; ARect: TRect);
begin
  inherited;
  with ADrawParams do
    if (LinkBorderPainter <> nil) and TCustomdxBarControlAccess(BarStaticControl.Parent).IsStatusBar then
      DrawSkinElementContent(LinkBorderPainter, Canvas.Handle, ARect, dxGetScaleFactor(BarItemControl));
end;

procedure TdxBarSkinPainter.DrawStaticBorder(const ADrawParams: TdxBarStaticLikeControlDrawParams; var ARect: TRect);
var
  AContentRect: TRect;
begin
  with ADrawParams do
    if not cxRectIsNull(BorderOffsets) and (LinkBorderPainter <> nil) and TCustomdxBarControlAccess(BarStaticControl.Parent).IsStatusBar then
    begin
      Canvas.SaveClipRegion;
      try
        AContentRect := cxRectContent(ARect, BorderOffsets);
        Canvas.ExcludeClipRect(AContentRect);
        DrawBackground(BarStaticControl, Canvas.Handle, ARect, 0, False);
        DrawSkinElement(LinkBorderPainter, Canvas.Handle, ARect, dxGetScaleFactor(BarItemControl));
        ARect := AContentRect;
      finally
        Canvas.RestoreClipRegion;
      end;
    end
    else
      inherited;
end;

function TdxBarSkinPainter.SubMenuGetSeparatorSize: Integer;
begin
  Result := GetSkinElementSize(PopupMenuSeparator, dxDefaultScaleFactor).cy;
end;

procedure TdxBarSkinPainter.SeparatorControlGetTextColors(
  ABarItemControl: TdxBarItemControl; AEnabled, ASelected, AFlat: Boolean;
  var AColor1, AColor2: TColor);
const
  StatesMap: array[Boolean] of TcxButtonState = (cxbsDisabled, cxbsNormal);
begin
  if ItemSeparator <> nil then
  begin
    AColor1 := ItemSeparator.GetTextColor(StatesMap[AEnabled]);
    AColor2 := AColor1;
  end
  else
    inherited SeparatorControlGetTextColors(ABarItemControl, AEnabled, ASelected, AFlat, AColor1, AColor2);
end;

procedure TdxBarSkinPainter.DrawSeparatorGlyphAndCaption(
  const ADrawParams: TdxBarSeparatorControlDrawParams; const ARect: TRect);
var
  ACaptionRect: TRect;
begin
  with ADrawParams do
  begin
    DrawBackground(BarStaticControl, Canvas.Handle, ARect, 0, False);
    DrawSkinElement(ItemSeparator, Canvas.Handle, ARect, dxGetScaleFactor(BarItemControl));
    ACaptionRect := ARect;
    ACaptionRect.Left := ACaptionRect.Left +  SeparatorControlGetIndents(ADrawParams, cpText).Left;
    if Canvas.UseRightToLeftAlignment then
      ACaptionRect := TdxRightToLeftLayoutConverter.ConvertRect(ACaptionRect, ARect);
    Dec(ACaptionRect.Bottom, SeparatorControlSeparatorSize);
    if not IsTop then
      Inc(ACaptionRect.Top, SeparatorControlSeparatorSize);
    DrawItemText(BarItemControl, Canvas.Handle, Caption, ACaptionRect,
      SystemAlignmentsHorz[Alignment], Enabled, False, PaintType = ptVert, True,
      False);
  end;
end;

function TdxBarSkinPainter.GetDefaultEnabledTextColor(
  ABarItemControl: TdxBarItemControl; ASelected, AFlat: Boolean): TColor;
var
  ATextColorElement: TdxSkinElement;
begin
  ATextColorElement := GetTextColorElement(ABarItemControl.Parent);
  if IsBarElementSkinned(ABarItemControl.Parent) and (ATextColorElement <> nil) then
  begin
    Result := clDefault;
    if ASelected then
    begin
      if TCustomdxBarControlAccess(ABarItemControl.Parent).IsMainMenu and ABarItemControl.IsDroppedDown then
        Result := GetSkinElementTextColor(ATextColorElement, cxbsPressed);
      if Result = clDefault then
        Result := GetSkinElementTextColor(ATextColorElement, cxbsHot);
    end;
    if Result = clDefault then
      Result := ATextColorElement.TextColor;
  end
  else
    Result := inherited GetDefaultEnabledTextColor(ABarItemControl, ASelected, AFlat);
end;

procedure TdxBarSkinPainter.GetDisabledTextColors(ABarItemControl: TdxBarItemControl;
  ASelected, AFlat: Boolean; var AColor1, AColor2: TColor);
begin
  if BarDisabledTextColor <> nil then
  begin
    AColor1 := BarDisabledTextColor.Value;
    AColor2 := AColor1;
  end
  else
    inherited;
end;

class function TdxBarSkinPainter.UseTextColorForItemArrow: Boolean;
begin
  Result := True;
end;

function TdxBarSkinPainter.GetLookAndFeelPainter: TcxCustomLookAndFeelPainter;
begin
  Result := FSkinPainter;
end;

function TdxBarSkinPainter.BarMarkIsOpaque: Boolean;
begin
  Result := True;
end;

procedure TdxBarSkinPainter.DrawBarMarkState(ABarControl: TdxBarControl;
  DC: HDC; const R: TRect; AState: TdxBarMarkState);
const
  StatesMap: array [Boolean] of TdxSkinElementState = (esNormal, esHot);
var
  ABarControlAccess: TdxBarControlAccess;
  AElement: TdxSkinElement;
begin
  ABarControlAccess := TdxBarControlAccess(ABarControl);
  AElement := GetBarMarkElement(ABarControlAccess.IsMainMenu, ABarControlAccess.Vertical);
  DrawSkinElementParentBackground(DC, R, ABarControl, AElement);
  DrawSkinElement(AElement, DC, R, dxGetScaleFactor(ABarControl), 0, StatesMap[AState <> msNone], ABarControl.UseRightToLeftAlignment);
end;

procedure TdxBarSkinPainter.DrawButtonBackground(const ADrawParams: TdxBarButtonLikeControlDrawParams);
var
  R: TRect;
begin
  with ADrawParams do
  begin
    R := BarItemControl.ItemBounds;
    if SplitDropDown and IsDropDown then
    begin
      if Canvas.UseRightToLeftAlignment then
        Inc(R.Left, cxRectWidth(TdxBarItemControlAccess(BarItemControl).FParts[bcpDropButton]))
      else
        Dec(R.Right, cxRectWidth(TdxBarItemControlAccess(BarItemControl).FParts[bcpDropButton]));
    end;
    ButtonLikeDrawGlyphBorder(BarItemControl, Canvas.Handle, 0, False, R, PaintType, not (cpIcon in ViewStructure),
      DrawSelected, Downed, DrawDowned, DroppedDown, SplitDropDown and IsDropDown, False);
    if SplitDropDown and IsDropDown then
      DrawSplitControlArrow(ADrawParams, BarItemControl.ItemBounds);
  end;
end;

procedure TdxBarSkinPainter.DrawGlyphEmptyImage(ABarItemControl: TdxBarItemControl;
  DC: HDC; R: TRect; APaintType: TdxBarPaintType; ADown: Boolean);
begin
  if (APaintType = ptMenu) and ADown and (PopupMenuCheck <> nil) then
    DrawSkinElement(PopupMenuCheck, DC,
      cxRectCenter(R, cxSizeMax(PopupMenuCheck.Size, PopupMenuCheck.MinSize.Size)),
      dxGetScaleFactor(ABarItemControl), 0, esNormal)
  else
    inherited;
end;

procedure TdxBarSkinPainter.DrawGlyphBorder(ABarItemControl: TdxBarItemControl;
  DC: HDC; ABrush: HBRUSH; NeedBorder: Boolean; R: TRect; PaintType: TdxBarPaintType;
  IsGlyphEmpty, Selected, Down, DrawDowned, ADroppedDown, IsSplit: Boolean);
begin
  ButtonLikeDrawGlyphBorder(ABarItemControl, DC, ABrush, NeedBorder, R,
    PaintType, IsGlyphEmpty, Selected, Down, DrawDowned, ADroppedDown, IsSplit, True);
end;

function TdxBarSkinPainter.GetGlyphColorPalette(
  ABarItemControl: TdxBarItemControl; APaintType: TdxBarPaintType;
  ASelected, ADowned, ADrawDowned, ADroppedDown, AIsSplit: Boolean): IdxColorPalette;
var
  AElement: TdxSkinElement;
begin
  if APaintType = ptMenu then
    AElement := PopupMenu
  else
    AElement := Bar[APaintType = ptVert];

  if AElement <> nil then
    Result := AElement.GetGlyphColorPalette(GetState(ASelected, ADowned,
      ADrawDowned, ADroppedDown, TdxBarItemControlAccess(ABarItemControl).IsMenuItem))
  else
    Result := nil;
end;

function TdxBarSkinPainter.GetState(ASelected, ADowned, ADrawDowned, ADroppedDown, AIsMenuItem: Boolean): TdxSkinElementState;
const
  DownStates: array[Boolean] of TdxSkinElementState = (esHotCheck, esCheckPressed);
  DrawDownStates: array[Boolean] of TdxSkinElementState = (esHot, esPressed);
begin
  if not ASelected then
    Result := esChecked
  else
    if ADowned then
      Result := DownStates[ADrawDowned]
    else
      Result := DrawDownStates[ADrawDowned and (AIsMenuItem or not ADroppedDown)];
end;

function TdxBarSkinPainter.BarCaptionColor(ABarControl: TdxBarControl): COLORREF;
begin
  if FloatingBar <> nil then
    Result := FloatingBar.TextColor
  else
    Result := inherited BarCaptionColor(ABarControl);
end;

class function TdxBarSkinPainter.NeedDoubleBuffer: Boolean;
begin
  Result := True;
end;

class procedure TdxBarSkinPainter.BarOffsetFloatingBarCaption(ABarControl: TdxBarControl; var X: Integer; var R: TRect);
begin
  if ABarControl.UseRightToLeftAlignment then
  begin
    Dec(X, 2);
    R.Left := TdxBarControlAccess(ABarControl).MarkNCRect.Right;//  TdxBarXPPainter
  end
  else
  begin
    Inc(X, 2);
    R.Right := TdxBarControlAccess(ABarControl).MarkNCRect.Left;//  TdxBarXPPainter
  end;
end;

procedure TdxBarSkinPainter.BarDrawGrip(ABarControl: TdxBarControl; DC: HDC; R: TRect;
  AToolbarBrush: HBRUSH);
const
  ACorner: array [Boolean] of TdxCorner = (coBottomRight, coBottomLeft);
begin
  cxPaintCanvas.BeginPaint(DC);
  try
    FSkinPainter.DrawSizeGrip(cxPaintCanvas, R, clNone, ACorner[ABarControl.UseRightToLeftAlignment]);
  finally
    cxPaintCanvas.EndPaint;
  end;
end;

procedure TdxBarSkinPainter.BarDrawMarkBackground(ABarControl: TdxBarControl;
  DC: HDC; ItemRect: TRect; AToolbarBrush: HBRUSH);
begin
  DrawBarMarkState(ABarControl, DC, ItemRect, TdxBarControlAccess(ABarControl).MarkState);
end;

procedure TdxBarSkinPainter.SubMenuControlDrawMarkSelection(
  ABarSubMenuControl: TdxBarSubMenuControl; ADC: HDC; const AMarkRect: TRect);
begin
  DrawSkinElement(PopupMenuLinkSelected, ADC, AMarkRect, dxGetScaleFactor(ABarSubMenuControl));
end;

function TdxBarSkinPainter.CreateHintViewInfo(ABarManager: TdxBarManager; AHintText: string; const AShortCut: string;
  AScreenTip: TdxScreenTip): TdxBarCustomHintViewInfo;
begin
  Result := dxBarCreateScreenTipViewInfo(ABarManager, AHintText, AShortCut, AScreenTip, Self);
end;

function TdxBarSkinPainter.GetBar(AIsVertical: Boolean): TdxSkinElement;
var
  ASkinPainterInfo: TdxSkinInfo;
begin
  Result := nil;
  if GetSkinPainterData(ASkinPainterInfo) then
    if AIsVertical then
      Result := ASkinPainterInfo.BarVertical
    else
      Result := ASkinPainterInfo.Bar;
end;

function TdxBarSkinPainter.GetBarCustomize(AIsVertical: Boolean): TdxSkinElement;
var
  ASkinPainterInfo: TdxSkinInfo;
begin
  Result := nil;
  if GetSkinPainterData(ASkinPainterInfo) then
    if AIsVertical then
      Result := ASkinPainterInfo.BarCustomizeVertical
    else
      Result := ASkinPainterInfo.BarCustomize;
end;

function TdxBarSkinPainter.GetBarDisabledTextColor: TdxSkinColor;
var
  ASkinPainterInfo: TdxSkinInfo;
begin
  Result := nil;
  if GetSkinPainterData(ASkinPainterInfo) then
    Result := ASkinPainterInfo.BarDisabledTextColor;
end;

function TdxBarSkinPainter.GetBarDrag(AIsVertical: Boolean): TdxSkinElement;
var
  ASkinPainterInfo: TdxSkinInfo;
begin
  Result := nil;
  if GetSkinPainterData(ASkinPainterInfo) then
    if AIsVertical then
      Result := ASkinPainterInfo.BarDragVertical
    else
      Result := ASkinPainterInfo.BarDrag;
end;

function TdxBarSkinPainter.GetBarSeparator(AIsVertical: Boolean): TdxSkinElement;
var
  ASkinPainterInfo: TdxSkinInfo;
begin
  Result := nil;
  if GetSkinPainterData(ASkinPainterInfo) then
    if AIsVertical then
      Result := ASkinPainterInfo.BarVerticalSeparator
    else
      Result := ASkinPainterInfo.BarSeparator;
end;

function TdxBarSkinPainter.GetDock: TdxSkinElement;
var
  ASkinPainterInfo: TdxSkinInfo;
begin
  Result := nil;
  if GetSkinPainterData(ASkinPainterInfo) then
    Result := ASkinPainterInfo.Dock;
  SetupCacheControl(Result, 1);
end;

function  TdxBarSkinPainter.GetDockControlWindowButton: TdxSkinElement;
var
  ASkinPainterInfo: TdxSkinInfo;
begin
  Result := nil;
  if GetSkinPainterData(ASkinPainterInfo) then
    Result := ASkinPainterInfo.DockControlWindowButton;
end;

function  TdxBarSkinPainter.GetDockControlWindowButtonGlyph: TdxSkinElement;
var
  ASkinPainterInfo: TdxSkinInfo;
begin
  Result := nil;
  if GetSkinPainterData(ASkinPainterInfo) then
    Result := ASkinPainterInfo.DockControlWindowButtonGlyphs;
end;

function TdxBarSkinPainter.GetFloatingBar: TdxSkinElement;
var
  ASkinPainterInfo: TdxSkinInfo;
begin
  Result := nil;
  if GetSkinPainterData(ASkinPainterInfo) then
    Result := ASkinPainterInfo.FloatingBar;
end;

function TdxBarSkinPainter.GetItemSeparator: TdxSkinElement;
var
  ASkinPainterInfo: TdxSkinInfo;
begin
  if GetSkinPainterData(ASkinPainterInfo) then
    Result := ASkinPainterInfo.ItemSeparator
  else
    Result := nil;
end;

function TdxBarSkinPainter.GetLinkBorderPainter: TdxSkinElement;
var
  ASkinPainterInfo: TdxSkinInfo;
begin
  Result := nil;
  if GetSkinPainterData(ASkinPainterInfo) then
    Result := ASkinPainterInfo.LinkBorderPainter;
end;

function TdxBarSkinPainter.GetLinkSelected: TdxSkinElement;
var
  ASkinPainterInfo: TdxSkinInfo;
begin
  Result := nil;
  if GetSkinPainterData(ASkinPainterInfo) then
    Result := ASkinPainterInfo.LinkSelected;
end;

function TdxBarSkinPainter.GetMainMenu(AIsVertical: Boolean): TdxSkinElement;
var
  ASkinPainterInfo: TdxSkinInfo;
begin
  if GetSkinPainterData(ASkinPainterInfo) then
  begin
    if AIsVertical then
      Result := ASkinPainterInfo.BarVertical
    else
      Result := ASkinPainterInfo.MainMenu;
  end
  else
    Result := nil;
end;

function TdxBarSkinPainter.GetMainMenuCustomize(AIsVertical: Boolean): TdxSkinElement;
var
  ASkinPainterInfo: TdxSkinInfo;
begin
  Result := nil;
  if GetSkinPainterData(ASkinPainterInfo) then
  begin
    if not AIsVertical then
      Result := ASkinPainterInfo.MainMenuCustomize;
    if Result = nil then
      Result := BarCustomize[AIsVertical];
  end;
end;

function TdxBarSkinPainter.GetMainMenuDrag: TdxSkinElement;
var
  ASkinPainterInfo: TdxSkinInfo;
begin
  if GetSkinPainterData(ASkinPainterInfo) then
    Result := ASkinPainterInfo.MainMenuDrag
  else
    Result := nil;
end;

function TdxBarSkinPainter.GetMainMenuLinkSelected: TdxSkinElement;
var
  ASkinPainterInfo: TdxSkinInfo;
begin
  Result := nil;
  if GetSkinPainterData(ASkinPainterInfo) then
    Result := ASkinPainterInfo.MainMenuLinkSelected;
end;

function TdxBarSkinPainter.GetMDIButton(AButton: TdxBarMDIButton): TdxSkinElement;
var
  ASkinPainterInfo: TdxSkinInfo;
begin
  if GetSkinPainterData(ASkinPainterInfo) then
  begin
    case AButton of
      mdibMinimize:
        Result := ASkinPainterInfo.BarMDIButtonMinimize;
      mdibRestore:
        Result := ASkinPainterInfo.BarMDIButtonRestore;
      else // mdibClose
        Result := ASkinPainterInfo.BarMDIButtonClose;
    end;
  end
  else
    Result := nil;
end;

function TdxBarSkinPainter.GetPopupMenu: TdxSkinElement;
var
  ASkinPainterInfo: TdxSkinInfo;
begin
  Result := nil;
  if GetSkinPainterData(ASkinPainterInfo) then
    Result := ASkinPainterInfo.PopupMenu;
end;

function TdxBarSkinPainter.GetPopupMenuCheck: TdxSkinElement;
var
  ASkinPainterInfo: TdxSkinInfo;
begin
  Result := nil;
  if GetSkinPainterData(ASkinPainterInfo) then
    Result := ASkinPainterInfo.PopupMenuCheck;
end;

function TdxBarSkinPainter.GetPopupMenuExpandButton: TdxSkinElement;
var
  ASkinPainterInfo: TdxSkinInfo;
begin
  Result := nil;
  if GetSkinPainterData(ASkinPainterInfo) then
    Result := ASkinPainterInfo.PopupMenuExpandButton;
end;

function TdxBarSkinPainter.GetPopupMenuLinkSelected: TdxSkinElement;
var
  ASkinPainterInfo: TdxSkinInfo;
begin
  Result := nil;
  if GetSkinPainterData(ASkinPainterInfo) then
    Result := ASkinPainterInfo.PopupMenuLinkSelected;
end;

function TdxBarSkinPainter.GetPopupMenuSeparator: TdxSkinElement;
var
  ASkinPainterInfo: TdxSkinInfo;
begin
  Result := nil;
  if GetSkinPainterData(ASkinPainterInfo) then
    Result := ASkinPainterInfo.PopupMenuSeparator;
end;

function TdxBarSkinPainter.GetPopupMenuSideStrip: TdxSkinElement;
var
  ASkinPainterInfo: TdxSkinInfo;
begin
  Result := nil;
  if GetSkinPainterData(ASkinPainterInfo) then
    Result := ASkinPainterInfo.PopupMenuSideStrip;
end;

function TdxBarSkinPainter.GetPopupMenuSideStripNonRecent: TdxSkinElement;
var
  ASkinPainterInfo: TdxSkinInfo;
begin
  Result := nil;
  if GetSkinPainterData(ASkinPainterInfo) then
    Result := ASkinPainterInfo.PopupMenuSideStripNonRecent;
end;

function TdxBarSkinPainter.GetPopupMenuSplitButton: TdxSkinElement;
var
  ASkinPainterInfo: TdxSkinInfo;
begin
  Result := nil;
  if GetSkinPainterData(ASkinPainterInfo) then
    Result := ASkinPainterInfo.PopupMenuSplitButton;
end;

function TdxBarSkinPainter.GetPopupMenuSplitButton2: TdxSkinElement;
var
  ASkinPainterInfo: TdxSkinInfo;
begin
  Result := nil;
  if GetSkinPainterData(ASkinPainterInfo) then
    Result := ASkinPainterInfo.PopupMenuSplitButton2;
end;

function TdxBarSkinPainter.GetStatusBar: TdxSkinElement;
var
  ASkinPainterInfo: TdxSkinInfo;
begin
  Result := nil;
  if GetSkinPainterData(ASkinPainterInfo) then
    Result := ASkinPainterInfo.FormStatusBar;
end;

function TdxBarSkinPainter.GetScreenTipItem: TdxSkinColor;
var
  ASkinPainterInfo: TdxSkinInfo;
begin
  Result := nil;
  if GetSkinPainterData(ASkinPainterInfo) then
    Result := ASkinPainterInfo.ScreenTipItem;
end;

function TdxBarSkinPainter.GetScreenTipSeparator: TdxSkinElement;
var
  ASkinPainterInfo: TdxSkinInfo;
begin
  Result := nil;
  if GetSkinPainterData(ASkinPainterInfo) then
    Result := ASkinPainterInfo.ScreenTipSeparator;
end;

function TdxBarSkinPainter.GetScreenTipTitleItem: TdxSkinColor;
var
  ASkinPainterInfo: TdxSkinInfo;
begin
  Result := nil;
  if GetSkinPainterData(ASkinPainterInfo) then
    Result := ASkinPainterInfo.ScreenTipTitleItem;
end;

function TdxBarSkinPainter.GetScreenTipWindow: TdxSkinElement;
var
  ASkinPainterInfo: TdxSkinInfo;
begin
  Result := nil;
  if GetSkinPainterData(ASkinPainterInfo) then
    Result := ASkinPainterInfo.ScreenTipWindow;
end;

function TdxBarSkinPainter.GetSkinPainterData(var AData: TdxSkinInfo): Boolean;
begin
  Result := (FSkinPainter <> nil) and FSkinPainter.GetPainterData(AData);
end;

procedure TdxBarSkinPainter.SetupCacheControl(AElement: TdxSkinElement; ACacheCapacity: Integer = 0);
begin
  if (AElement <> nil) and not AElement.UseCache then
  begin
    if ACacheCapacity > 0 then
      AElement.CacheCapacity := ACacheCapacity;
    AElement.UseCache := True;
  end;
end;

function TdxBarSkinPainter.DrawSkinElement(AElement: TdxSkinElement; DC: HDC; const ARect: TRect;
  AScaleFactor: TdxScaleFactor; AImageIndex: Integer = 0; AState: TdxSkinElementState = esNormal; AIsRightToLeft: Boolean = False): Boolean;
begin
  Result := AElement <> nil;
  if Result then
    if AIsRightToLeft then
      AElement.DrawRTL(DC, ARect, AScaleFactor, Min(AImageIndex, AElement.ImageCount), AState)
    else
      AElement.Draw(DC, ARect, AScaleFactor, Min(AImageIndex, AElement.ImageCount), AState);
end;

procedure TdxBarSkinPainter.DrawSkinElementParentBackground(DC: HDC;
  const R: TRect; ABarControl: TCustomdxBarControl; AElement: TdxSkinElement);
begin
  if (AElement = nil) or AElement.IsAlphaUsed then
    BarFillParentBackground(ABarControl, DC, R, R, 0, clNone);
end;

function TdxBarSkinPainter.DrawSkinElementContent(AElement: TdxSkinElement; DC: HDC; const ARect: TRect;
  AScaleFactor: TdxScaleFactor; AImageIndex: Integer = 0; AState: TdxSkinElementState = esNormal): Boolean;
begin
  Result := AElement <> nil;
  if Result then
  begin
    cxPaintCanvas.BeginPaint(DC);
    try
      cxPaintCanvas.IntersectClipRect(ARect);
      Result := DrawSkinElement(AElement, cxPaintCanvas.Handle,
        cxRectContent(ARect, cxRectInvert(AElement.ContentOffset.Rect)),
        AScaleFactor, AImageIndex, AState);
    finally
      cxPaintCanvas.EndPaint;
    end;
  end;
end;

function TdxBarSkinPainter.DrawSkinElementBorders(AElement: TdxSkinElement; DC: HDC;
  const ARect: TRect; AImageIndex: Integer = 0; AState: TdxSkinElementState = esNormal): Boolean;
begin
  cxPaintCanvas.BeginPaint(DC);
  try
    Result := AElement <> nil;
    if Result then
    begin
      cxPaintCanvas.ExcludeClipRect(cxRectContent(ARect, AElement.ContentOffset.Rect));
      Result := DrawSkinElement(AElement, cxPaintCanvas.Handle, ARect, dxDefaultScaleFactor, AImageIndex, AState);
    end;
  finally
    cxPaintCanvas.EndPaint;
  end;
end;

function TdxBarSkinPainter.GetSkinElementTextColor(ASkinElement: TdxSkinElement; AState: TcxButtonState): TColor;
begin
  Result := clDefault;
  if ASkinElement <> nil then
  begin
    Result := ASkinElement.GetTextColor(AState);
    if not cxColorIsValid(Result) then
      Result := clDefault;
  end
end;

function TdxBarSkinPainter.GetSkinEditorsBackgroundColor(AState: TcxEditStateColorKind): TColor;
begin
  if Assigned(FSkinPainter) then
    Result := FSkinPainter.DefaultEditorBackgroundColorEx(AState)
  else
    Result := clDefault;
end;

function TdxBarSkinPainter.GetSkinEditorsTextColor(AState: TcxEditStateColorKind): TColor;
begin
  if Assigned(FSkinPainter) then
    Result := FSkinPainter.DefaultEditorTextColorEx(AState)
  else
    Result := clDefault;
end;

function TdxBarSkinPainter.GetSkinElementSize(ASkinElement: TdxSkinElement; AScaleFactor: TdxScaleFactor): TSize;
begin
  Result := cxNullSize;
  if ASkinElement <> nil then
  begin
    if ASkinElement.MinSize.IsEmpty then
      Result := ASkinElement.Size
    else
      Result := ASkinElement.MinSize.Size;

    Result := AScaleFactor.Apply(Result);
  end;
end;

function TdxBarSkinPainter.GetBarElement(ABarControl: TCustomdxBarControl; AVertical: Boolean): TdxSkinElement;
var
  ACustomBarControlAccess: TCustomdxBarControlAccess;
begin
  ACustomBarControlAccess := TCustomdxBarControlAccess(ABarControl);
  if ACustomBarControlAccess.Kind = bkSubMenu then
    Result := PopupMenu
  else if ACustomBarControlAccess.IsMainMenu then
    Result := MainMenu[AVertical]
  else if ACustomBarControlAccess.IsStatusBar then
    Result := StatusBar
  else
    Result := Bar[AVertical];

  SetupCacheControl(Result, 32);
end;

function TdxBarSkinPainter.GetBarElement(ABarControl: TCustomdxBarControl): TdxSkinElement;
begin
  if ABarControl <> nil then
    Result := GetBarElement(ABarControl, TCustomdxBarControlAccess(ABarControl).IsRealVertical)
  else
    Result := nil;
end;

function TdxBarSkinPainter.GetBarMarkElement(AMainMenu, AVertical: Boolean): TdxSkinElement;
begin
  if AMainMenu then
    Result := MainMenuCustomize[AVertical]
  else
    Result := BarCustomize[AVertical];

  SetupCacheControl(Result);
end;

function TdxBarSkinPainter.GetTextColorElement(ABarControl: TCustomdxBarControl): TdxSkinElement;
begin
  Result := GetBarElement(ABarControl, False);
  if (Result = Bar[ABarControl.IsRealVertical]) and (ABarControl is TdxBarControl) and
     (TdxBarControlAccess(ABarControl).Bar.BorderStyle = bbsNone)
  then
    Result := Dock;
end;

function TdxBarSkinPainter.IsBarElementSkinned(ABarControl: TCustomdxBarControl): Boolean;
begin
  Result := GetBarElement(ABarControl) <> nil;
end;

procedure TdxBarSkinPainter.DrawArrowButtonElement(AElement: TdxSkinElement;
  ACanvas: TcxCanvas; const ARect: TRect; AImageIndex: Integer = 0;
  AState: TdxSkinElementState = esNormal);
var
  R: TRect;
begin
  if AElement <> nil then
  begin
    R := ARect;
    if cxRectWidth(ARect) < cxMarginsWidth(AElement.Image.Margins.Margin) then
      R.Left := ARect.Left + cxRectWidth(ARect) - cxMarginsWidth(AElement.Image.Margins.Margin);
    ACanvas.SaveClipRegion;
    try
      ACanvas.IntersectClipRect(ARect);
      DrawSkinElement(AElement, ACanvas.Handle, R, dxDefaultScaleFactor, 0, AState);
    finally
      ACanvas.RestoreClipRegion;
    end;
  end;
end;

procedure TdxBarSkinPainter.DrawFloatingBarCaptionButton(DC: HDC; ARect: TRect; AContentType: Integer; AState: TdxBarMarkState);
const
  MarkState2SkinState: array [TdxBarMarkState] of TdxSkinElementState = (esActive, esHot, esPressed);
begin
  DrawSkinElement(DockControlWindowButton, DC, ARect, dxDefaultScaleFactor, 0, MarkState2SkinState[AState]);
  DrawSkinElement(DockControlWindowButtonGlyph, DC, ARect, dxDefaultScaleFactor, AContentType);
end;

procedure TdxBarSkinPainter.InternalDrawDockedBarBackground(
  ABarControl: TdxBarControl; DC: HDC; R: TRect; AClientArea: Boolean);

  function GetBarDragRect(ABarControlAccess: TdxBarControlAccess; const R: TRect): TRect;
  begin
    Result := R;
    if ABarControlAccess.Vertical then
      Result.Bottom := Result.Top + GripperSize(ABarControlAccess)
    else
      if ABarControlAccess.UseRightToLeftAlignment then
        Result.Left := Result.Right - GripperSize(ABarControlAccess)
      else
        Result.Right := Result.Left + GripperSize(ABarControlAccess);
  end;

  procedure DrawBarElement(ABarControlAccess: TdxBarControlAccess; ACanvas: TcxCanvas; R: TRect);
  var
    AElement: TdxSkinElement;
  begin
    AElement := GetBarElement(ABarControl);
    DrawSkinElementParentBackground(ACanvas.Handle, R, ABarControl, AElement);
    AElement.Draw(ACanvas.Handle, R);
  end;

  procedure DrawMarkElementNCPart(ABarControlAccess: TdxBarControlAccess; ACanvas: TcxCanvas; const ARect: TRect);
  var
    ANCMarkRect: TRect;
  begin
    ANCMarkRect := cxRectOffset(BarMarkItemRect(ABarControlAccess), ABarControlAccess.NCOffset);
    ANCMarkRect := cxRectOffset(ANCMarkRect, cxPointInvert(ARect.TopLeft));
    BarDrawMarks(ABarControlAccess, ACanvas, ANCMarkRect, 0);
  end;

  procedure DrawBarDragElement(ABarControlAccess: TdxBarControlAccess; ACanvas: TcxCanvas; const R: TRect);
  var
    AElement: TdxSkinElement;
  begin
    if ABarControlAccess.IsMainMenu and not ABarControlAccess.Vertical then
      AElement := MainMenuDrag
    else
      AElement := BarDrag[ABarControlAccess.IsRealVertical];

    SetupCacheControl(AElement, 1);
    DrawSkinElementParentBackground(ACanvas.Handle, R, ABarControl, AElement);
    DrawSkinElement(AElement, ACanvas.Handle, R, dxDefaultScaleFactor, 0, esNormal, ABarControlAccess.UseRightToLeftAlignment);
  end;

  procedure FillBackgroundTempBitmap(ABarControlAccess: TdxBarControlAccess; const AWholeR: TRect);
  var
    ABitmap: TcxBitmap;
    ABitmapRect: TRect;
  begin
    ABitmap := ABarControlAccess.BackgroundTempBitmap;
    ABitmap.SetSize(AWholeR);
    ABitmapRect := cxRectOffset(AWholeR, cxPointInvert(AWholeR.TopLeft));

    DrawBarElement(ABarControlAccess, ABitmap.cxCanvas, ABitmapRect);
    if not AClientArea then
    begin
      if ABarControlAccess.MarkExists then
        DrawMarkElementNCPart(ABarControlAccess, ABitmap.cxCanvas, AWholeR);
      if ABarControlAccess.CanMoving then
        DrawBarDragElement(ABarControlAccess, ABitmap.cxCanvas, GetBarDragRect(ABarControlAccess, ABitmapRect));
    end;
  end;

var
  ABarControlAccess: TdxBarControlAccess;
begin
  ABarControlAccess := TdxBarControlAccess(ABarControl);
  FillBackgroundTempBitmap(ABarControlAccess, R);
  cxBitBlt(DC, ABarControlAccess.BackgroundTempBitmap.Canvas.Handle, R, cxNullPoint, SRCCOPY);
end;

function TdxBarSkinPainter.ButtonLikeControlGetState(
  const ADrawParams: TdxBarButtonLikeControlDrawParams): TdxSkinElementState;
const
  MenuStatesMap: array[Boolean] of TdxSkinElementState = (esDisabled, esHot);
  SelectedStatesMap: array[Boolean, Boolean] of TdxSkinElementState = ((esHot, esPressed), (esHotCheck, esCheckPressed));
begin
  with ADrawParams do
    if PaintType = ptMenu then
      Result := MenuStatesMap[Enabled]
    else
      if DrawSelected then
        Result := SelectedStatesMap[Downed, IsPressed]
      else
        Result := esChecked;
end;

function TdxBarSkinPainter.ButtonLikeDrawArrowFadingElement(
  const ADrawParams: TdxBarButtonLikeControlDrawParams; var R: TRect): Boolean;
var
  ABarItem: TdxBarItemControlAccess;
  AFadingElement: TdxFadingElement;
  R1: TRect;
begin
  ABarItem := TdxBarItemControlAccess(ADrawParams.BarItemControl);
  Result := dxFader.Find(ABarItem, AFadingElement);
  if Result then
  begin
    if IsFlatItemText and (ADrawParams.PaintType <> ptMenu) then
      Dec(R.Left);
    ADrawParams.Canvas.SaveClipRegion;
    try
      R1 := R;
      R1.Left := R1.Right - cxRectWidth(ADrawParams.BarItemControl.ItemBounds);
      ADrawParams.Canvas.SetClipRegion(TcxRegion.Create(R), roIntersect);
      Result := AFadingElement.DrawImage(ADrawParams.Canvas.Handle, R1);
    finally
      ADrawParams.Canvas.RestoreClipRegion;
    end;
  end;
end;

function TdxBarSkinPainter.ButtonLikeDrawFadingElement(
  ABarItemControl: TdxBarItemControl; DC: HDC; const R: TRect; AIsSplit: Boolean): Boolean;
var
  ABarItem: TdxBarItemControlAccess;
  AFadingElement: TdxFadingElement;
  R1: TRect;
begin
  ABarItem := TdxBarItemControlAccess(ABarItemControl);
  Result := dxFader.Find(ABarItem, AFadingElement);
  if Result then
  begin
    if not AIsSplit then
      Result := AFadingElement.DrawImage(DC, R)
    else
    begin
      cxPaintCanvas.BeginPaint(DC);
      try
        R1 := R;
        Inc(R1.Right, cxRectWidth(ABarItem.FParts[bcpDropButton]));
        cxPaintCanvas.SaveClipRegion;
        try
          cxPaintCanvas.SetClipRegion(TcxRegion.Create(R), roIntersect);
          Result := AFadingElement.DrawImage(cxPaintCanvas.Handle, R1);
        finally
          cxPaintCanvas.RestoreClipRegion;
        end;
      finally
        cxPaintCanvas.EndPaint;
      end;
    end;
  end;
end;

procedure TdxBarSkinPainter.ButtonLikeDrawGlyphBorder(ABarItemControl: TdxBarItemControl; DC: HDC; ABrush: HBRUSH;
  NeedBorder: Boolean; R: TRect; PaintType: TdxBarPaintType; IsGlyphEmpty, Selected, Down, DrawDowned, ADroppedDown,
  IsSplit, AAllowFading: Boolean);
var
  AIsMenuItem: Boolean;
  AState: TdxSkinElementState;
begin
  if PaintType = ptMenu then
  begin
    if not IsGlyphEmpty and Down then
      DrawSkinElement(PopupMenuCheck, DC, cxRectInflate(R, -2), dxGetScaleFactor(ABarItemControl), 1, esNormal);
  end
  else
  begin
    DrawBackground(ABarItemControl, DC, R, ABrush, False);
    if not (AAllowFading and ButtonLikeDrawFadingElement(ABarItemControl, DC, R, IsSplit)) then
    begin
      if Selected or Down then
      begin
        AIsMenuItem := TdxBarItemControlAccess(ABarItemControl).IsMenuItem;
        AState := GetState(Selected, Down, DrawDowned, ADroppedDown, AIsMenuItem);
        if AIsMenuItem then
          DrawSkinElement(MainMenuLinkSelected, DC, R, dxGetScaleFactor(ABarItemControl), 0, AState)
        else
          DrawSkinElement(LinkSelected, DC, R, dxGetScaleFactor(ABarItemControl), 0, AState, ABarItemControl.UseRightToLeftAlignment)
      end;
    end;
  end;
end;

initialization
  dxBarSkinPainterClass := TdxBarSkinPainter;

finalization
  dxBarSkinPainterClass := TdxBarStandardPainter;

end.
