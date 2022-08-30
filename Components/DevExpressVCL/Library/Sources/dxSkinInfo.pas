{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           Express Cross Platform Library classes                   }
{                                                                    }
{           Copyright (c) 2000-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSCROSSPLATFORMLIBRARY AND ALL   }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM       }
{   ONLY.                                                            }
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

unit dxSkinInfo;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  UITypes,
{$ENDIF}
  Types, Windows, Classes, Graphics, SysUtils, cxClasses, cxGraphics, cxLookAndFeels,
  cxGeometry, cxLookAndFeelPainters, dxSkinsCore, dxSkinsStrs, dxCoreClasses, dxCoreGraphics;

const
  dxSkinsSchedulerResourceColorsCount = 12;

type

  TdxSkinFormIcon = (sfiMenu, sfiHelp, sfiMinimize, sfiMaximize, sfiRestore, sfiClose);
  TdxSkinFormIcons = set of TdxSkinFormIcon;

  { TdxSkinScrollInfo }

  TdxSkinScrollInfo = class(TObject)
  private
    FElement: TdxSkinElement;
    FImageIndex: Integer;
  public
    constructor Create(AElement: TdxSkinElement; AImageIndex: Integer; APart: TcxScrollBarPart);
    function DrawScaled(DC: HDC; const R: TRect; AImageIndex: Integer; AState: TdxSkinElementState; AScaleFactor: TdxScaleFactor): Boolean; overload;
    function Draw(DC: HDC; const R: TRect; AImageIndex: Integer; AState: TdxSkinElementState): Boolean; overload;
    function DrawScaled(DC: HDC; const R: TRect; AState: TdxSkinElementState; AScaleFactor: TdxScaleFactor): Boolean; overload;
    function Draw(DC: HDC; const R: TRect; AState: TdxSkinElementState): Boolean; overload;

    property Element: TdxSkinElement read FElement;
    property ImageIndex: Integer read FImageIndex;
  end;

  { TdxSkinElementHelper }

  TdxSkinElementHelper = class
  public
    class function CalculateCaptionButtonSize(ACaptionHeight: Integer; AElement: TdxSkinElement): TSize;
    class function IsAlternateImageSetUsed(AElement: TdxSkinElement; AImageIndex: Integer; AState: TdxSkinElementState): Boolean;
  end;

  { TdxSkinInfo }

  TdxSkinInfo = class(TcxIUnknownObject, IdxSkinChangeListener, IdxSkinInfo)
  private
    FSkin: TdxSkin;
    procedure MarkObjectUsed(AObject: TdxSkinCustomObject);
    procedure SetSkin(ASkin: TdxSkin);
  protected
    GroupBars: TdxSkinControlGroup;
    GroupCommon: TdxSkinControlGroup;
    GroupDocking: TdxSkinControlGroup;
    GroupEditors: TdxSkinControlGroup;
    GroupForm: TdxSkinControlGroup;
    GroupGrid: TdxSkinControlGroup;
    GroupMapControl: TdxSkinControlGroup;
    GroupNavBar: TdxSkinControlGroup;
    GroupNavPane: TdxSkinControlGroup;
    GroupPDFViewer: TdxSkinControlGroup;
    GroupPrintingSystem: TdxSkinControlGroup;
    GroupRibbon: TdxSkinControlGroup;
    GroupRichEdit: TdxSkinControlGroup;
    GroupScheduler: TdxSkinControlGroup;
    GroupTabs: TdxSkinControlGroup;
    GroupTileControl: TdxSkinControlGroup;
    GroupVGrid: TdxSkinControlGroup;
    //
    CardViewSeparator: TdxSkinElement;
    CheckboxElement: TdxSkinElement;
    ClockElements: array[Boolean] of TdxSkinElement;
    EditButtonElements: array [Boolean] of TdxSkinElement;
    EditButtonGlyphs: array [TcxEditBtnKind] of TdxSkinElement;
    EditButtonMergeBorders: Boolean;
    EditButtonSearchGlyph: TdxSkinElement;
    GridFixedLine: TdxSkinElement;
    GridGroupByBox: TdxSkinElement;
    GridGroupRow: TdxSkinElement;
    GridGroupRowStyleOffice11ContentColor: TdxSkinColor;
    GridGroupRowStyleOffice11SeparatorColor: TdxSkinColor;
    GridGroupRowStyleOffice11TextColor: TdxSkinColor;
    GridLine: TdxSkinElement;
    GridWinExplorerViewGroup: TdxSkinElement;
    GridWinExplorerViewGroupCaptionLine: TdxSkinElement;
    GridWinExplorerViewGroupExpandButton: TdxSkinElement;
    GridWinExplorerViewRecord: TdxSkinElement;
    HighlightedItem: TdxSkinElement;
    IndicatorImages: TdxSkinElement;
    NavigatorButton: TdxSkinElement;
    NavigatorGlyphs: TdxSkinElement;
    NavigatorGlyphsVert: TdxSkinElement;
    NavigatorInfoPanelColor: TdxSkinColor;
    NavigatorInfoPanelTextColor: TdxSkinColor;
    RadioGroupButton: TdxSkinElement;
    RangeTrackBarThumbBoth: TdxSkinElement;
    RangeTrackBarThumbLeft: TdxSkinElement;
    RangeTrackBarThumbRight: TdxSkinElement;
    RatingIndicator: TdxSkinElement;
    Splitter: array[Boolean] of TdxSkinElement;
    TrackBarThumb: array[Boolean, TcxTrackBarTicksAlign] of TdxSkinElement;
    TrackBarTickColor: TdxSkinColor;
    TrackBarTrack: array[Boolean] of TdxSkinElement;
    VGridCategory: TdxSkinElement;
    VGridContentColor: TdxSkinColor;
    VGridLine: array[Boolean] of TdxSkinElement;
    VGridRowHeader: TdxSkinElement;
    // Colors
    CalcEditButtonTextColors: array[TcxCalcButtonKind] of TdxSkinColor;
    ContentEvenColor: TdxSkinColor;
    ContentOddColor: TdxSkinColor;
    ContentTextColor: TdxSkinColor;
    HeaderBackgroundColor: TdxSkinColor;
    HeaderBackgroundTextColor: TdxSkinColor;
    SelectionColor: TdxSkinColor;
    SelectionTextColor: TdxSkinColor;
    TreeListGridLineColor: TdxSkinColor;
    TreeListTreeLineColor: TdxSkinColor;
    // ExpandButton
    ExpandButton: TdxSkinElement;
    // Footer
    FooterCell: TdxSkinElement;
    FooterPanel: TdxSkinElement;
    // header
    Header: TdxSkinElement;
    HeaderSpecial: TdxSkinElement;
    SortGlyphs: TdxSkinElement;
    // filter
    FilterButtons: array[Boolean] of TdxSkinElement;
    FilterPanel: TdxSkinElement;
    SmartFilterButton: TdxSkinElement;

    function CreateBlankElement(AGroup: TdxSkinControlGroup; const AName: string): TdxSkinElement;
    function CreateBlankGroup(const AName: string): TdxSkinControlGroup;
    function GetAlphaColorValueByName(AElement: TdxSkinPersistent; const AName: string): TdxAlphaColor;
    function GetBoolPropertyByName(AElement: TdxSkinPersistent; const AName: string): TdxSkinBooleanProperty;
    function GetColorByName(AElement: TdxSkinPersistent; const AName: string): TdxSkinColor; overload;
    function GetColorByName(const AName: string): TdxSkinColor; overload;
    function GetColorValueByName(AElement: TdxSkinPersistent; const AName: string): TColor;
    function GetElementByName(AGroup: TdxSkinControlGroup; const AName: string; ACreateIfAbsent: Boolean = True): TdxSkinElement;
    function GetGroupByName(const AName: string): TdxSkinControlGroup;
    function GetIntegerPropertyByName(AElement: TdxSkinPersistent; const AName: string): TdxSkinIntegerProperty;
    function GetPropertyByName(AElement: TdxSkinPersistent; const AName: string): TdxSkinProperty;

    procedure FinalizeScrollBarElements;
    procedure InitializeAlertWindowElements;
    procedure InitializeBarElements;
    procedure InitializeBreadcrumbEditElements;
    procedure InitializeButtonElements;
    procedure InitializeCalcEditColors;
    procedure InitializeCalendarElements;
    procedure InitializeCheckboxElements;
    procedure InitializeClockElements;
    procedure InitializeColors;
    procedure InitializeDockControlElements;
    procedure InitializeEditButtonElements;
    procedure InitializeFilterElements;
    procedure InitializeFooterElements;
    procedure InitializeFormElements;
    procedure InitializeGalleryElements;
    procedure InitializeGaugeElements;
    procedure InitializeGridElements;
    procedure InitializeGroupBoxElements;
    procedure InitializeGroups;
    procedure InitializeHeaderElements;
    procedure InitializeIndicatorImages;
    procedure InitializeLayoutViewElements;
    procedure InitializeMapControlElements;
    procedure InitializeNavBarElements;
    procedure InitializeNavigatorElements;
    procedure InitializePageControlElements;
    procedure InitializePDFViewerElements;
    procedure InitializePrintingSystemElements;
    procedure InitializeProgressBarElements;
    procedure InitializeRadioGroupElements;
    procedure InitializeRangeControlElements;
    procedure InitializeRibbonColors;
    procedure InitializeRibbonElements;
    procedure InitializeRibbonProperties;
    procedure InitializeRichEditElements;
    procedure InitializeSchedulerElements;
    procedure InitializeScrollBarElements;
    procedure InitializeSizeGripElements;
    procedure InitializeSplitterElements;
    procedure InitializeTileControlElements;
    procedure InitializeToggleSwitchElements;
    procedure InitializeToolTipElements;
    procedure InitializeTrackBarElements;

    procedure FinalizeSkinInfo; virtual;
    procedure InitializeSkinInfo; virtual;

    procedure SetUseCache(AElement: TdxSkinElement; ACapacity: Integer = -1);

    // IdxSkinInfo
    function GetSkin: TdxSkin;
    // IdxSkinChangeListener
    procedure SkinChanged(Sender: TdxSkin); virtual;
  public
    // Button
    BackButton: TdxSkinElement;
    ButtonDisabled: TdxSkinColor;
    ButtonElements: TdxSkinElement;
    DropDownButtonLeft: TdxSkinElement;
    DropDownButtonRight: TdxSkinElement;
    // Colors
    ContainerBorderColor: TdxSkinColor;
    ContainerHighlightBorderColor: TdxSkinColor;
    ContentColor: TdxSkinColor;
    EditorBackgroundColors: array[TcxEditStateColorKind] of TdxSkinColor;
    EditorTextColors: array[TcxEditStateColorKind] of TdxSkinColor;
    HyperLinkTextColor: TdxSkinColor;
    InactiveColor: TdxSkinColor;
    InactiveTextColor: TdxSkinColor;
    SpreadSheetContentColor: TdxSkinColor;
    SpreadSheetContentTextColor: TdxSkinColor;
    SpreadSheetFrozenPaneSeparatorColor: TdxSkinColor;
    SpreadSheetGroupLineColor: TdxSkinColor;
    SpreadSheetSelectionColor: TdxSkinColor;
    GridLikeControlContentColor: TdxSkinColor;
    GridLikeControlContentEvenColor: TdxSkinColor;
    GridLikeControlContentOddColor: TdxSkinColor;
    GridLikeControlContentTextColor: TdxSkinColor;
    GridLikeControlBackgroundColor: TdxSkinColor;
    // ProgressBar
    ProgressBarElements: array[Boolean, Boolean] of TdxSkinElement;
    ProgressBarTextColors: array[Boolean] of TdxSkinColor;
    // ScrollBars
    ScrollBar_Elements: array[Boolean, TcxScrollBarPart] of TdxSkinScrollInfo;
    // Label
    LabelLine: array[Boolean] of TdxSkinElement;
    // Bevel
    BevelShapeColor1: TdxSkinColor;
    BevelShapeColor2: TdxSkinColor;
    // AlertWindow
    AlertWindow: TdxSkinElement;
    AlertWindowButton: TdxSkinElement;
    AlertWindowButtonGlyphs: TdxSkinElement;
    AlertWindowCaption: TdxSkinElement;
    AlertWindowCornerRadius: Integer;
    AlertWindowNavigationPanel: TdxSkinElement;
    AlertWindowNavigationPanelButton: TdxSkinElement;
    // BreadcrumbEdit
    BreadcrumbEditBackgroundColors: array[TdxBreadcrumbEditState] of TdxSkinColor;
    BreadcrumbEditBordersColors: array[TdxBreadcrumbEditState] of TdxSkinColor;
    BreadcrumbEditButton: TdxSkinElement;
    BreadcrumbEditButtonMergeBorders: Boolean;
    BreadcrumbEditButtonsAreaSeparator: TdxSkinElement;
    BreadcrumbEditDropDownButton: TdxSkinElement;
    BreadcrumbEditNodeButton: TdxSkinElement;
    BreadcrumbEditNodeSplitButtonLeft: TdxSkinElement;
    BreadcrumbEditNodeSplitButtonRight: TdxSkinElement;
    BreadcrumbEditProgressChunk: TdxSkinElement;
    BreadcrumbEditProgressChunkOverlay: TdxSkinElement;
    BreadcrumbEditProgressChunkPadding: TdxSkinRectProperty;
    // GroupBox
    GroupBoxCaptionElements: array[TcxGroupBoxCaptionPosition] of TdxSkinElement;
    GroupBoxCaptionTailSizes: array[TcxGroupBoxCaptionPosition] of TdxSkinIntegerProperty;
    GroupBoxCaptionTextPadding: array[TcxGroupBoxCaptionPosition] of TdxSkinIntegerProperty;
    GroupBoxClient: TdxSkinElement;
    GroupBoxElements: array[TcxGroupBoxCaptionPosition] of TdxSkinElement;
    GroupButton: TdxSkinElement;
    GroupButtonExpandGlyph: TdxSkinElement;

    // DockControl
    DockControlBorder: TdxSkinElement;
    DockControlCaption: TdxSkinElement;
    DockControlCaptionNonFocusedTextColor: TColor;
    DockControlHideBar: TdxSkinElement;
    DockControlHideBarLeft: TdxSkinElement;
    DockControlHideBarRight: TdxSkinElement;
    DockControlHideBarBottom: TdxSkinElement;
    DockControlHideBarButtons: TdxSkinElement;
    DockControlHideBarTextColor: array[Boolean] of TdxSkinColor;
    DockControlIndents: array[0..2] of Integer;
    DockControlTabHeader: TdxSkinElement;
    DockControlTabButtonHorz: TdxSkinElement;
    DockControlTabButtonVert: TdxSkinElement;
    DockControlTabHeaderBackground: TdxSkinElement;
    DockControlTabHeaderCloseButton: TdxSkinElement;
    DockControlTabHeaderLine: TdxSkinElement;
    DockControlTabTextColor: array[Boolean] of TdxSkinColor;
    DockControlWindowButton: TdxSkinElement;
    DockControlWindowButtonGlyphs: TdxSkinElement;
    DockSiteContentColor: TdxSkinColor;
    // Layout
    LayoutControlColor: TdxSkinColor;

    // LayoutView
    LayoutViewElementPadding: array[TcxLayoutElement] of TdxSkinRectProperty;
    LayoutViewElementSpacing: array[TcxLayoutElement] of TdxSkinRectProperty;
    LayoutViewItem: TdxSkinElement;
    LayoutViewRecordCaptionElements: array[TcxGroupBoxCaptionPosition] of TdxSkinElement;
    LayoutViewRecordCaptionTailSizes: array[TcxGroupBoxCaptionPosition] of TdxSkinIntegerProperty;
    LayoutViewRecordCaptionTextPadding: array[TcxGroupBoxCaptionPosition] of TdxSkinIntegerProperty;
    LayoutViewRecordElements: array[TcxGroupBoxCaptionPosition] of TdxSkinElement;
    LayoutViewRecordExpandButton: TdxSkinElement;

    // PageControl
    PageControlButton: TdxSkinElement;
    PageControlButtonHorz: TdxSkinElement;
    PageControlButtonVert: TdxSkinElement;
    PageControlCloseButton: TdxSkinElement;
    PageControlHeader: TdxSkinElement;
    PageControlHeaderButton: TdxSkinElement;
    PageControlIndents: array[0..7] of Integer;
    PageControlPane: TdxSkinElement;
    TabTextColor: TdxSkinColor;
    TabTextColorActive: TdxSkinColor;
    TabTextColorDisabled: TdxSkinColor;
    TabTextColorHot: TdxSkinColor;

    // NavBar
    NavBarBackgroundColor: TdxSkinElement;
    NavBarGroupButtons: array [Boolean] of TdxSkinElement;
    NavBarGroupClient: TdxSkinElement;
    NavBarGroupHeader: TdxSkinElement;
    NavBarItem: TdxSkinElement;

    // NavBar - Accordion Control
    NavBarAccordionControlBackground: TdxSkinElement;
    NavBarAccordionControlChildItemOffset: TdxSkinIntegerProperty;
    NavBarAccordionControlContentContainerPadding: TdxSkinRectProperty;
    NavBarAccordionControlDistanceBetweenRootGroups: TdxSkinIntegerProperty;
    NavBarAccordionControlGroup: TdxSkinElement;
    NavBarAccordionControlGroupCloseButton: TdxSkinElement;
    NavBarAccordionControlGroupOpenButton: TdxSkinElement;
    NavBarAccordionControlItem: TdxSkinElement;
    NavBarAccordionControlRootGroup: TdxSkinElement;
    NavBarAccordionControlRootGroupCloseButton: TdxSkinElement;
    NavBarAccordionControlRootGroupOpenButton: TdxSkinElement;
    NavBarAccordionControlSearchButton: TdxSkinElement;
    NavBarAccordionControlSearchButtonGlyphToTextIndent: TdxSkinIntegerProperty;

    // NavPane
    NavPaneCaptionFontSize: TdxSkinIntegerProperty;
    NavPaneCaptionHeight: TdxSkinIntegerProperty;
    NavPaneCollapseButton: TdxSkinElement;
    NavPaneCollapsedGroupClient: TdxSkinElement;
    NavPaneExpandButton: TdxSkinElement;
    NavPaneFormBorder: TdxSkinElement;
    NavPaneFormSizeGrip: TdxSkinElement;
    NavPaneGroupButton: array[Boolean] of TdxSkinElement;
    NavPaneGroupCaption: TdxSkinElement;
    NavPaneGroupClient: TdxSkinElement;
    NavPaneItem: TdxSkinElement;
    NavPaneOfficeNavigationBar: TdxSkinElement;
    NavPaneOfficeNavigationBarItem: TdxSkinElement;
    NavPaneOfficeNavigationBarItemFontDelta: Integer;
    NavPaneOfficeNavigationBarSkinningItem: TdxSkinElement;
    NavPaneOfficeNavigationBarSkinningItemFontDelta: Integer;
    NavPaneOffsetGroupBorders: TdxSkinBooleanProperty;
    NavPaneOverflowPanel: TdxSkinElement;
    NavPaneOverflowPanelExpandedItem: TdxSkinElement;
    NavPaneOverflowPanelItem: TdxSkinElement;
    NavPaneScrollButtons: array[Boolean] of TdxSkinElement;
    NavPaneSelectedItem: TdxSkinElement;
    NavPaneSplitter: TdxSkinElement;

    // Form
    FormCaptionFontDelta: Integer;
    FormCaptionFontIsBold: TdxSkinBooleanProperty;
    FormContent: TdxSkinElement;
    FormFrames: array[Boolean, TcxBorder] of TdxSkinElement;
    FormIcons: array[Boolean, TdxSkinFormIcon] of TdxSkinElement;
    FormInactiveColor: TdxSkinColor;
    FormStatusBar: TdxSkinElement;
    FormTextShadowColor: TdxSkinColor;
    SizeGrip: TdxSkinElement;

    // Scheduler
    SchedulerAllDayArea: array[Boolean] of TdxSkinElement;
    SchedulerAppointment: array[Boolean] of TdxSkinElement;
    SchedulerAppointmentBorder: TdxSkinColor;
    SchedulerAppointmentBorderSize: TdxSkinIntegerProperty;
    SchedulerAppointmentMask: TdxSkinElement;
    SchedulerAppointmentShadow: array[Boolean] of TdxSkinElement;
    SchedulerCurrentTimeIndicator: TdxSkinElement;
    SchedulerGroup: TdxSkinElement;
    SchedulerMilestone: TdxSkinElement;
    SchedulerLabelCircle: TdxSkinElement;
    SchedulerMoreButton: TdxSkinElement;
    SchedulerNavigationButtons: array[Boolean] of TdxSkinElement;
    SchedulerNavigationButtonsArrow: array[Boolean] of TdxSkinElement;
    SchedulerNavigatorColor: TdxSkinColor;
    SchedulerResourceColors: array[0..dxSkinsSchedulerResourceColorsCount - 1] of TdxSkinColor;
    SchedulerTimeGridCurrentTimeIndicator: TdxSkinElement;
    SchedulerTimeGridHeader: array[Boolean] of TdxSkinElement;
    SchedulerTimeLine: TdxSkinElement;
    SchedulerTimeRuler: TdxSkinElement;

    // Bars
    Bar: TdxSkinElement;
    BarCustomize: TdxSkinElement;
    BarCustomizeVertical: TdxSkinElement;
    BarDisabledTextColor: TdxSkinColor;
    BarDrag: TdxSkinElement;
    BarDragVertical: TdxSkinElement;
    BarMDIButtonClose: TdxSkinElement;
    BarMDIButtonMinimize: TdxSkinElement;
    BarMDIButtonRestore: TdxSkinElement;
    BarSeparator: TdxSkinElement;
    BarVertical: TdxSkinElement;
    BarVerticalSeparator: TdxSkinElement;
    Dock: TdxSkinElement;
    FloatingBar: TdxSkinElement;
    ItemSeparator: TdxSkinElement;
    LinkBorderPainter: TdxSkinElement;
    LinkSelected: TdxSkinElement;
    MainMenu: TdxSkinElement;
    MainMenuCustomize: TdxSkinElement;
    MainMenuDrag: TdxSkinElement;
    MainMenuLinkSelected: TdxSkinElement;
    MainMenuVertical: TdxSkinElement;
    PopupMenu: TdxSkinElement;
    PopupMenuCheck: TdxSkinElement;
    PopupMenuExpandButton: TdxSkinElement;
    PopupMenuLinkSelected: TdxSkinElement;
    PopupMenuSeparator: TdxSkinElement;
    PopupMenuSideStrip: TdxSkinElement;
    PopupMenuSideStripNonRecent: TdxSkinElement;
    PopupMenuSplitButton: TdxSkinElement;
    PopupMenuSplitButton2: TdxSkinElement;
    ScreenTipItem: TdxSkinColor;
    ScreenTipSeparator: TdxSkinElement;
    ScreenTipTitleItem: TdxSkinColor;
    ScreenTipWindow: TdxSkinElement;

    // Ribbon
    RadialMenuBackgroundColor: TdxSkinColor;
    RadialMenuBaseColor: TdxSkinColor;
    RibbonApplicationBackground: TdxSkinElement;
    RibbonApplicationButton: TdxSkinElement;
    RibbonApplicationButton2010: TdxSkinElement;
    RibbonApplicationFooterBackground: TdxSkinElement;
    RibbonApplicationHeaderBackground: TdxSkinElement;
    RibbonBackstageView: TdxSkinElement;
    RibbonBackstageViewBackButton: TdxSkinElement;
    RibbonBackstageViewImage: TdxSkinElement;
    RibbonBackstageViewMenu: TdxSkinElement;
    RibbonBackstageViewMenuButton: TdxSkinElement;
    RibbonBackstageViewMenuHeader: TdxSkinElement;
    RibbonBackstageViewMenuSeparator: TdxSkinElement;
    RibbonBackstageViewTab: TdxSkinElement;
    RibbonBackstageViewTabArrow: TdxSkinElement;
    RibbonButtonArrow: TdxSkinElement;
    RibbonButtonGroup: TdxSkinElement;
    RibbonButtonGroupButton: TdxSkinElement;
    RibbonButtonGroupSeparator: TdxSkinElement;
    RibbonButtonGroupSplitButtonLeft: TdxSkinElement;
    RibbonButtonGroupSplitButtonRight: TdxSkinElement;
    RibbonButtonText: array[Boolean] of TColor;
    RibbonCaptionFontDelta: TdxSkinIntegerProperty;
    RibbonCaptionText: array[Boolean] of TColor;
    RibbonCollapsedToolBarBackground: TdxSkinElement;
    RibbonCollapsedToolBarGlyphBackground: TdxSkinElement;
    RibbonContextualTabHeader: TdxSkinElement;
    RibbonContextualTabHeaderText: array[Boolean] of TColor;
    RibbonContextualTabHeaderTextHot: TColor;
    RibbonContextualTabLabel: TdxSkinElement;
    RibbonContextualTabLabelOnGlass: TdxSkinElement;
    RibbonContextualTabLabelOnGlassShadowColor: TdxSkinColor;
    RibbonContextualTabLabelShadowColor: TdxSkinColor;
    RibbonContextualTabPanel: TdxSkinElement;
    RibbonContextualTabSeparator: TdxSkinElement;
    RibbonDocumentNameTextColor: array[Boolean] of TColor;
    RibbonEditorBackground: TdxSkinColor;
    RibbonExtraPaneButton: TdxSkinElement;
    RibbonExtraPaneColor: TdxSkinColor;
    RibbonExtraPaneHeaderSeparator: TdxSkinColor;
    RibbonExtraPanePinButtonGlyph: TdxSkinElement;
    RibbonFormBottom: array[Boolean] of TdxSkinElement;
    RibbonFormButtonAutoHideModeShowUI: TdxSkinElement;
    RibbonFormButtonClose: TdxSkinElement;
    RibbonFormButtonDisplayOptions: TdxSkinElement;
    RibbonFormButtonHelp: TdxSkinElement;
    RibbonFormButtonMaximize: TdxSkinElement;
    RibbonFormButtonMinimize: TdxSkinElement;
    RibbonFormButtonRestore: TdxSkinElement;
    RibbonFormCaption: TdxSkinElement;
    RibbonFormCaptionRibbonHidden: TdxSkinElement;
    RibbonFormContent: TdxSkinElement;
    RibbonFormLeft: array[Boolean] of TdxSkinElement;
    RibbonFormRight: array[Boolean] of TdxSkinElement;
    RibbonGalleryBackground: TdxSkinElement;
    RibbonGalleryButtonDown: TdxSkinElement;
    RibbonGalleryButtonDropDown: TdxSkinElement;
    RibbonGalleryButtonUp: TdxSkinElement;
    RibbonGalleryGroupCaption: TdxSkinElement;
    RibbonGalleryPane: TdxSkinElement;
    RibbonGallerySizeGrips: TdxSkinElement;
    RibbonGallerySizingPanel: TdxSkinElement;
    RibbonGroupScroll: array[Boolean] of TdxSkinElement;
    RibbonHeaderBackground: TdxSkinElement;
    RibbonHeaderBackgroundOnGlass: TdxSkinElement;
    RibbonKeyTip: TdxSkinElement;
    RibbonLargeButton: TdxSkinElement;
    RibbonLargeSplitButtonBottom: TdxSkinElement;
    RibbonLargeSplitButtonTop: TdxSkinElement;
    RibbonMinimizeButtonGlyph: TdxSkinElement;
    RibbonQATCustomizeButtonOutsizeQAT: array[Boolean] of TdxSkinBooleanProperty;
    RibbonQATIndentBeforeCustomizeButton: array[Boolean] of TdxSkinIntegerProperty;
    RibbonQuickToolbar: array[Boolean] of TdxSkinElement;
    RibbonQuickToolbarBelow: TdxSkinElement;
    RibbonQuickToolbarButtonGlyph: TdxSkinElement;
    RibbonQuickToolbarDropDown: TdxSkinElement;
    RibbonQuickToolbarGlyph: TdxSkinElement;
    RibbonSmallButton: TdxSkinElement;
    RibbonSpaceBetweenTabGroups: TdxSkinIntegerProperty;
    RibbonSplitButtonLeft: TdxSkinElement;
    RibbonSplitButtonRight: TdxSkinElement;
    RibbonStatusBarBackground: TdxSkinElement;
    RibbonStatusBarButton: TdxSkinElement;
    RibbonStatusBarSeparator: TdxSkinElement;
    RibbonStatusBarTextSelected: TColor;
    RibbonTab: TdxSkinElement;
    RibbonTabAeroSupport: TdxSkinBooleanProperty;
    RibbonTabGroup: TdxSkinElement;
    RibbonTabGroupHeader: TdxSkinElement;
    RibbonTabGroupItemsSeparator: TdxSkinElement;
    RibbonTabHeaderDownGrowIndent: TdxSkinIntegerProperty;
    RibbonTabPanel: TdxSkinElement;
    RibbonTabPanelBottomIndent: TdxSkinIntegerProperty;
    RibbonTabPanelGroupButton: TdxSkinElement;
    RibbonTabSeparatorLine: TdxSkinElement;
    RibbonTabText: array[Boolean] of TColor;
    RibbonTabTextHot: TColor;
    RibbonUseRoundedWindowCorners: TdxSkinBooleanProperty;

    // RichEdit
    RichEditCornerPanel: TdxSkinElement;
    RichEditRulerBackgroundHorz: TdxSkinElement;
    RichEditRulerBackgroundVert: TdxSkinElement;
    RichEditRulerColumnResizer: TdxSkinElement;
    RichEditRulerDefaultTabColor: TdxSkinColor;
    RichEditRulerIndent: TdxSkinElement;
    RichEditRulerIndentBottom: TdxSkinElement;
    RichEditRulerRightMargin: TdxSkinElement;
    RichEditRulerSection: TdxSkinElement;
    RichEditRulerTab: TdxSkinElement;
    RichEditRulerTabTypeBackground: TdxSkinElement;
    RichEditRulerTextColor: TdxSkinColor;

    // Calendar
    CalendarDayTextColor: TdxSkinColor;
    CalendarHolidayTextColor: TdxSkinColor;
    CalendarInactiveDayTextColor: TdxSkinColor;
    CalendarNavigationButton: TdxSkinElement;
    CalendarSelectedDayColor: TdxSkinColor;
    CalendarSelectedDayTextColor: TdxSkinColor;
    CalendarSeparatorColor: TdxSkinColor;
    CalendarTodayFrameColor: TdxSkinColor;
    CalendarTodayTextColor: TdxSkinColor;

    // Printing System
    PrintingPageBorder: TdxSkinElement;
    PrintingPreviewBackground: TdxSkinElement;

    // Gallery
    GalleryBackground: TdxSkinElement;
    GalleryGroup: TdxSkinElement;
    GalleryItem: TdxSkinElement;
    GalleryItemGlyphFrame: TdxSkinElement;

    // Gauge
    GaugeBackground: TdxSkinElement;

    // ToggleSwitch
    ToggleSwitch: TdxSkinElement;
    ToggleSwitchTextMargin: TdxSkinIntegerProperty;
    ToggleSwitchThumb: TdxSkinElement;

    // Zoom
    ZoomInButton: TdxSkinElement;
    ZoomOutButton: TdxSkinElement;

    // Slider
    SliderArrow: array[TcxArrowDirection] of TdxSkinElement;

    // TileControl
    TileControlActionBar: TdxSkinElement;
    TileControlBackground: TdxSkinElement;
    TileControlItem: TdxSkinElement;
    TileControlItemCheck: TdxSkinElement;
    TileControlGroupCaption: TdxSkinElement;
    TileControlGroupCaptionFontDelta: Integer;
    TileControlSelectionFocusedColor: TColor;
    TileControlSelectionHotColor: TColor;
    TileControlTabHeader: TdxSkinElement;
    TileControlTabHeaderFontDelta: Integer;
    TileControlTitle: TdxSkinElement;
    TileControlTitleFontDelta: Integer;
    TileControlVirtualGroup: TdxSkinElement;

    // MapControl
    MapControlBackColor: TColor;
    MapControlCallout: TdxSkinElement;
    MapControlCalloutPointer: TPoint;
    MapControlCalloutPointerHeight: Integer;
    MapControlCalloutTextGlowColor: TdxAlphaColor;
    MapControlCustomElement: TdxSkinElement;
    MapControlCustomElementTextGlowColor: TdxAlphaColor;
    MapControlPanelBackColor: TdxAlphaColor;
    MapControlPanelHotTrackedTextColor: TdxAlphaColor;
    MapControlPanelPressedTextColor: TdxAlphaColor;
    MapControlPanelTextColor: TdxAlphaColor;
    MapControlPushpin: TdxSkinElement;
    MapControlPushpinTextOrigin: TPoint;
    MapControlPushpinTextGlowColor: TdxAlphaColor;
    MapControlSelectedRegionBackgroundColor: TdxAlphaColor;
    MapControlSelectedRegionBorderColor: TdxAlphaColor;
    MapControlShapeBorderColor: array [TcxButtonState] of TdxAlphaColor;
    MapControlShapeBorderWidth: array [TcxButtonState] of Integer;
    MapControlShapeColor: array [TcxButtonState] of TdxAlphaColor;

    // RangeControl
    RangeControlBackColor: TdxAlphaColor;
    RangeControlBorder: TdxSkinElement;
    RangeControlDefaultElementColor: TdxSkinColor;
    RangeControlElementBaseColor: TdxSkinColor;
    RangeControlElementFontSize: TdxSkinIntegerProperty;
    RangeControlElementForeColor: TdxSkinColor;
    RangeControlInnerBorderColor: TdxAlphaColor;
    RangeControlLabelColor: TdxSkinColor;
    RangeControlLeftThumb: TdxSkinElement;
    RangeControlOutOfRangeColorMask: TdxAlphaColor;
    RangeControlRangePreviewColor: TdxSkinColor;
    RangeControlRightThumb: TdxSkinElement;
    RangeControlRuleColor: TdxAlphaColor;
    RangeControlRulerHeader: TdxSkinElement;
    RangeControlScrollAreaColor: TdxSkinColor;
    RangeControlScrollAreaHeight: TdxSkinIntegerProperty;
    RangeControlSelectionBorderColor: TdxAlphaColor;
    RangeControlSelectionColor: TdxAlphaColor;
    RangeControlSizingGlyph: TdxSkinElement;
    RangeControlViewPortPreviewColor: TdxSkinColor;

    // PDF
    PDFViewerFindPanel: TdxSkinElement;
    PDFViewerSelectionColor: TdxSkinColor;
    PDFViewerNavigationPaneBackground: TdxSkinElement;
    PDFViewerNavigationPaneButton: TdxSkinElement;
    PDFViewerNavigationPaneButtonArrow: TdxSkinElement;
    PDFViewerNavigationPaneButtonMinimized: TdxSkinElement;
    PDFViewerNavigationPanePageBackground: TdxSkinElement;
    PDFViewerNavigationPanePageButton: TdxSkinElement;
    PDFViewerNavigationPanePageCaption: TdxSkinElement;
    PDFViewerNavigationPaneSelectedPageExpandValue: TdxSkinIntegerProperty;
    PDFViewerNavigationPaneSelectedPageOverlapValue: TdxSkinIntegerProperty;

    // Common
    LoadingBig: TdxSkinElement;

    constructor Create(ASkin: TdxSkin); overload; virtual;
    destructor Destroy; override;
    function GetIntegerPropertyValue(AObject: TdxSkinPersistent; const APropertyName: string; ADefaultValue: Integer = 0): Integer;

    property Skin: TdxSkin read FSkin write SetSkin;
  end;

  TdxSkinInfoClass = class of TdxSkinInfo;

implementation

uses
  Math, dxGDIPlusClasses, StrUtils, dxDPIAwareUtils;

type
  TdxSkinElementAccess = class(TdxSkinElement);

{ TdxSkinElementHelper }

class function TdxSkinElementHelper.CalculateCaptionButtonSize(ACaptionHeight: Integer; AElement: TdxSkinElement): TSize;

  function CalculateScaleFactor(AElement: TdxSkinElement): Single;
  var
    ASize: TSize;
  begin
    if AElement <> nil then
      ASize := AElement.MinSize.Size
    else
      ASize := cxNullSize;

    if ASize.cy > 0 then
      Result := ASize.cx / ASize.cy
    else
      Result := 1;
  end;

begin
  Result.cy := ACaptionHeight;
  Result.cx := Round(Result.cy * CalculateScaleFactor(AElement));
  if Assigned(AElement) and (AElement.Image.Stretch = smNoResize) then
  begin
    Dec(Result.cx, Integer(Odd(Result.cx + AElement.Image.Size.cx)));
    Dec(Result.cy, Integer(Odd(Result.cy + AElement.Image.Size.cy)));
  end;
end;

class function TdxSkinElementHelper.IsAlternateImageSetUsed(
  AElement: TdxSkinElement; AImageIndex: Integer; AState: TdxSkinElementState): Boolean;
var
  AAttributes: TdxSkinAlternateImageAttributes;
begin
  Result := TdxSkinElementAccess(AElement).CanUseAlternateImageSet(
    AImageIndex, AState, dxGpIsDoubleBufferedNeeded(cxScreenCanvas.Handle), AAttributes);
  cxScreenCanvas.Dormant;
end;

{ TdxSkinInfo }

constructor TdxSkinInfo.Create(ASkin: TdxSkin);
begin
  Create;
  Skin := ASkin;
end;

destructor TdxSkinInfo.Destroy;
var
  ASkin: TdxSkin;
begin
  ASkin := Skin;
  Skin := nil;
  FreeAndNil(ASkin);
  inherited Destroy;
end;

function TdxSkinInfo.CreateBlankElement(AGroup: TdxSkinControlGroup; const AName: string): TdxSkinElement;
var
  ABitmap: TcxBitmap32;
begin
  Result := AGroup.AddElement(AName);
  if Result <> nil then
  begin
    Result.Image.States := [esNormal];
    Result.Image.Stretch := smStretch;
    Result.State := [sosUnassigned, sosUnused];
    ABitmap := TcxBitmap32.Create;
    try
      ABitmap.Width := 32;
      ABitmap.Height := 32;
      with ABitmap.Canvas do
      begin
        Pen.Color := clRed;
        Pen.Width := 2;
        MoveTo(0, 0);
        LineTo(ABitmap.Width, ABitmap.Height);
        MoveTo(0, ABitmap.Height);
        LineTo(ABitmap.Width, 0);
      end;
      ABitmap.MakeOpaque;
      Result.Image.Texture.SetBitmap(ABitmap);
    finally
      ABitmap.Free;
    end;
  end;
end;

function TdxSkinInfo.CreateBlankGroup(const AName: string): TdxSkinControlGroup;
begin
  Result := Skin.AddGroup(AName);
  Result.State := [sosUnassigned, sosUnused];
end;

function TdxSkinInfo.GetPropertyByName(
  AElement: TdxSkinPersistent; const AName: string): TdxSkinProperty;
begin
  if Assigned(AElement) then
  begin
    Result := AElement.GetPropertyByName(AName);
    MarkObjectUsed(Result);
  end
  else
    Result := nil;
end;

function TdxSkinInfo.GetColorByName(AElement: TdxSkinPersistent; const AName: string): TdxSkinColor;
begin
  Result := GetPropertyByName(AElement, AName) as TdxSkinColor;
end;

function TdxSkinInfo.GetColorByName(const AName: string): TdxSkinColor;
begin
  if Skin <> nil then
    Result := Skin.GetColorByName(AName)
  else
    Result := nil;
end;

function TdxSkinInfo.GetColorValueByName(AElement: TdxSkinPersistent; const AName: string): TColor;
var
  AColor: TdxSkinColor;
begin
  AColor := GetColorByName(AElement, AName);
  if AColor <> nil then
    Result := AColor.Value
  else
    Result := clDefault;
end;

function TdxSkinInfo.GetAlphaColorValueByName(AElement: TdxSkinPersistent; const AName: string): TdxAlphaColor;
begin
  Result := dxColorToAlphaColor(GetColorValueByName(AElement, AName),
    GetIntegerPropertyValue(AElement, AName + sdxAlpha, MaxByte));
end;

function TdxSkinInfo.GetBoolPropertyByName(AElement: TdxSkinPersistent; const AName: string): TdxSkinBooleanProperty;
begin
  Result := GetPropertyByName(AElement, AName) as TdxSkinBooleanProperty;
end;

function TdxSkinInfo.GetIntegerPropertyByName(AElement: TdxSkinPersistent; const AName: string): TdxSkinIntegerProperty;
begin
  Result := GetPropertyByName(AElement, AName) as TdxSkinIntegerProperty;
end;

function TdxSkinInfo.GetIntegerPropertyValue(AObject: TdxSkinPersistent; const APropertyName: string; ADefaultValue: Integer = 0): Integer;
var
  AProperty: TdxSkinIntegerProperty;
begin
  Result := ADefaultValue;
  if AObject <> nil then
  begin
    AProperty := AObject.GetPropertyByName(APropertyName) as TdxSkinIntegerProperty;
    if AProperty <> nil then
    begin
      MarkObjectUsed(AProperty);
      Result := AProperty.Value;
    end;
  end;
end;

function TdxSkinInfo.GetGroupByName(const AName: string): TdxSkinControlGroup;
begin
  if Assigned(Skin) then
  begin
    Result := Skin.GetGroupByName(AName);
    if Result = nil then
      Result := CreateBlankGroup(AName);
    MarkObjectUsed(Result);
  end
  else
    Result := nil;
end;

function TdxSkinInfo.GetElementByName(AGroup: TdxSkinControlGroup;
  const AName: string; ACreateIfAbsent: Boolean = True): TdxSkinElement;
begin
  if Assigned(AGroup) then
  begin
    Result := AGroup.GetElementByName(AName);
    if (Result = nil) and ACreateIfAbsent then
      Result := CreateBlankElement(AGroup, AName);
    MarkObjectUsed(Result);
  end
  else
    Result := nil;
end;

procedure TdxSkinInfo.InitializeGroups;
begin
  GroupBars := GetGroupByName(sdxSkinGroupBars);
  GroupCommon := GetGroupByName(sdxSkinGroupCommon);
  GroupDocking := GetGroupByName(sdxSkinGroupDocking);
  GroupEditors := GetGroupByName(sdxSkinGroupEditors);
  GroupForm := GetGroupByName(sdxSkinGroupForm);
  GroupGrid := GetGroupByName(sdxSkinGroupGrid);
  GroupMapControl := GetGroupByName(sdxSkinGroupMapControl);
  GroupNavBar := GetGroupByName(sdxSkinGroupNavBar);
  GroupNavPane := GetGroupByName(sdxSkinGroupNavPane);
  GroupPDFViewer := GetGroupByName(sdxSkinGroupPDFViewer);
  GroupPrintingSystem := GetGroupByName(sdxSkinGroupPrintingSystem);
  GroupRibbon := GetGroupByName(sdxSkinGroupRibbon);
  GroupRichEdit := GetGroupByName(sdxSkinGroupRichEdit);
  GroupScheduler := GetGroupByName(sdxSkinGroupScheduler);
  GroupTabs := GetGroupByName(sdxSkinGroupTabs);
  GroupTileControl := GetGroupByName(sdxSkinGroupTileControl);
  GroupVGrid := GetGroupByName(sdxSkinGroupVGrid);
end;

procedure TdxSkinInfo.InitializeAlertWindowElements;
begin
  AlertWindow := GetElementByName(GroupCommon, sdxAlertWindow);
  AlertWindowButton := GetElementByName(GroupCommon, sdxAlertWindowButton);
  AlertWindowButtonGlyphs := GetElementByName(GroupCommon, sdxAlertWindowButtonGlyphs);
  AlertWindowCaption := GetElementByName(GroupCommon, sdxAlertWindowCaption);
  AlertWindowCornerRadius := GetIntegerPropertyValue(AlertWindow, sdxAlertWindowCornerRadius);
  AlertWindowNavigationPanel := GetElementByName(GroupCommon, sdxAlertWindowNavigationPanel);
  AlertWindowNavigationPanelButton := GetElementByName(GroupCommon, sdxAlertWindowNavigationPanelButton);
end;

procedure TdxSkinInfo.InitializeBarElements;
begin
  Bar := GetElementByName(GroupBars, sdxBarsBar);
  BarCustomize := GetElementByName(GroupBars, sdxBarsBarCustomize);
  BarCustomizeVertical := GetElementByName(GroupBars, sdxBarsBarCustomizeVertical);
  BarDrag := GetElementByName(GroupBars, sdxBarsBarFinger);
  BarDragVertical := GetElementByName(GroupBars, sdxBarsBarFingerVertical);
  BarMDIButtonClose := GetElementByName(GroupBars, sdxBarsMDIButtonClose);
  BarMDIButtonMinimize := GetElementByName(GroupBars, sdxBarsMDIButtonMinimize);
  BarMDIButtonRestore := GetElementByName(GroupBars, sdxBarsMDIButtonRestore);
  BarSeparator := GetElementByName(GroupBars, sdxBarsBarSeparator);
  BarVertical := GetElementByName(GroupBars, sdxBarsBarVertical);
  BarVerticalSeparator := GetElementByName(GroupBars, sdxBarsBarVerticalSeparator);
  Dock := GetElementByName(GroupBars, sdxBarsDock);
  FloatingBar := GetElementByName(GroupBars, sdxBarsFloatBar);
  ItemSeparator := GetElementByName(GroupBars, sdxBarsItemSeparator);
  LinkBorderPainter := GetElementByName(GroupBars, sdxBarsLinkStatic);
  LinkSelected := GetElementByName(GroupBars, sdxBarsLinkSelected);
  MainMenu := GetElementByName(GroupBars, sdxBarsMainMenu);
  MainMenuCustomize := GetElementByName(GroupBars, sdxBarsMainMenuCustomize, False);
  MainMenuDrag := GetElementByName(GroupBars, sdxBarsMainMenuDrag, False);
  MainMenuLinkSelected := GetElementByName(GroupBars, sdxBarsMainMenuLinkSelected);
  MainMenuVertical := GetElementByName(GroupBars, sdxBarsMainMenuVertical);
  PopupMenu := GetElementByName(GroupBars, sdxBarsPopupMenu);
  PopupMenuCheck := GetElementByName(GroupBars, sdxBarsPopupMenuCheck);
  PopupMenuExpandButton := GetElementByName(GroupBars, sdxBarsPopupMenuExpandButton);
  PopupMenuLinkSelected := GetElementByName(GroupBars, sdxBarsPopupMenuLinkSelected);
  PopupMenuSeparator := GetElementByName(GroupBars, sdxBarsPopupMenuSeparator);
  PopupMenuSideStrip := GetElementByName(GroupBars, sdxBarsPopupMenuSideStrip);
  PopupMenuSideStripNonRecent := GetElementByName(GroupBars, sdxBarsPopupMenuSideStripNonRecent);
  PopupMenuSplitButton := GetElementByName(GroupBars, sdxBarsPopupMenuDropDownButtonLabel);
  PopupMenuSplitButton2 := GetElementByName(GroupBars, sdxBarsPopupMenuDropDownButtonArrow);
end;

procedure TdxSkinInfo.InitializeBreadcrumbEditElements;
var
  AIndex: TdxBreadcrumbEditState;
  AProperty: TdxSkinBooleanProperty;
begin
  BreadcrumbEditButton := GetElementByName(GroupEditors, sdxBreadcrumbEditButton);
  BreadcrumbEditButtonsAreaSeparator := GetElementByName(GroupEditors, sdxBreadcrumbEditButtonsAreaSeparator);
  BreadcrumbEditDropDownButton := GetElementByName(GroupEditors, sdxBreadcrumbEditDropDownButton);
  BreadcrumbEditNodeButton := GetElementByName(GroupEditors, sdxBreadcrumbEditNodeButton);
  BreadcrumbEditNodeSplitButtonLeft := GetElementByName(GroupEditors, sdxBreadcrumbEditNodeSplitButtonLeft);
  BreadcrumbEditNodeSplitButtonRight := GetElementByName(GroupEditors, sdxBreadcrumbEditNodeSplitButtonRight);
  BreadcrumbEditProgressChunk := GetElementByName(GroupEditors, sdxBreadcrumbEditProgressChunk);
  BreadcrumbEditProgressChunkOverlay := GetElementByName(GroupEditors, sdxBreadcrumbEditProgressChunkOverlay);
  BreadcrumbEditProgressChunkPadding := GetPropertyByName(GroupEditors, sdxBreadcrumbEditProgressChunkPadding) as TdxSkinRectProperty;
  for AIndex := Low(TdxBreadcrumbEditState) to High(TdxBreadcrumbEditState) do
  begin
    BreadcrumbEditBackgroundColors[AIndex] := GetColorByName(GroupEditors, BreadcrumbEditBackgroundColorsMap[AIndex]);
    BreadcrumbEditBordersColors[AIndex] := GetColorByName(GroupEditors, BreadcrumbEditBordersColorsMap[AIndex]);
  end;
  AProperty := GetBoolPropertyByName(BreadcrumbEditButton, sdxEditorButtonMergeBorders);
  BreadcrumbEditButtonMergeBorders := Assigned(AProperty) and AProperty.Value;
end;

procedure TdxSkinInfo.InitializeButtonElements;
begin
  BackButton := GetElementByName(GroupCommon, sdxBackButton);
  ButtonElements := GetElementByName(GroupCommon, sdxButton);
  ExpandButton := GetElementByName(GroupGrid, sdxPlusMinus);
  if ButtonElements <> nil then
    ButtonDisabled := GetColorByName(ButtonElements, sdxSkinsButtonDisabledTextColor);
  DropDownButtonLeft := GetElementByName(GroupCommon, sdxDropDownButtonLeft);
  DropDownButtonRight := GetElementByName(GroupCommon, sdxDropDownButtonRight);
end;

procedure TdxSkinInfo.InitializeCalcEditColors;
var
  AType: TcxCalcButtonKind;
begin
  for AType := Low(TcxCalcButtonKind) to High(TcxCalcButtonKind) do
    CalcEditButtonTextColors[AType] := GetColorByName(GroupEditors, CalcEditTextColorsMap[AType]);
end;

procedure TdxSkinInfo.InitializeCalendarElements;
begin
  CalendarNavigationButton := GetElementByName(GroupEditors, sdxSkinsCalendarNavigationButton);
  CalendarDayTextColor := GetColorByName(GroupEditors, sdxSkinsCalendarDayTextColor);
  CalendarHolidayTextColor := GetColorByName(GroupEditors, sdxSkinsCalendarHolidayTextColor);
  CalendarInactiveDayTextColor := GetColorByName(GroupEditors, sdxSkinsCalendarInactiveDayTextColor);
  CalendarSelectedDayTextColor := GetColorByName(GroupEditors, sdxSkinsCalendarSelectedDayTextColor);
  CalendarSelectedDayColor := GetColorByName(GroupEditors, sdxSkinsCalendarSelectedDayColor);
  CalendarSeparatorColor := GetColorByName(GroupEditors, sdxSkinsCalendarSeparatorColor);
  CalendarTodayFrameColor := GetColorByName(GroupEditors, sdxSkinsCalendarTodayFrameColor);
  CalendarTodayTextColor := GetColorByName(GroupEditors, sdxSkinsCalendarTodayTextColor);
end;

procedure TdxSkinInfo.InitializeCheckboxElements;
begin
  CheckboxElement := GetElementByName(GroupEditors, sdxCheckbox);
end;

procedure TdxSkinInfo.InitializeClockElements;
begin
  ClockElements[False] := GetElementByName(GroupEditors, sdxClock);
  ClockElements[True] := GetElementByName(GroupEditors, sdxClockGlass);
end;

procedure TdxSkinInfo.InitializeColors;
var
  AKind: TcxEditStateColorKind;
begin
  BarDisabledTextColor := GetColorByName(GroupBars, sdxSkinsBarDisabledTextColor);
  DockControlTabTextColor[False] := GetColorByName(GroupDocking, sdxSkinsTabTextColor);
  DockControlTabTextColor[True] := GetColorByName(GroupDocking, sdxSkinsTabTextColorActive);
  SchedulerNavigatorColor := GetColorByName(GroupScheduler, sdxSkinsSchedulerNavigatorColor);

  TabTextColor := GetColorByName(PageControlHeader, sdxTextColorNormal);
  TabTextColorActive := GetColorByName(PageControlHeader, sdxTextColorSelected);
  TabTextColorDisabled := GetColorByName(PageControlHeader, sdxTextColorDisabled);
  TabTextColorHot := GetColorByName(PageControlHeader, sdxTextColorHot);

  HyperLinkTextColor := GetColorByName(GroupEditors, sdxSkinsEditorHyperLinkTextColor);
  for AKind := Low(TcxEditStateColorKind) to High(TcxEditStateColorKind) do
  begin
    EditorTextColors[AKind] := GetColorByName(GroupEditors, EditTextColorsMap[AKind]);
    EditorBackgroundColors[AKind] := GetColorByName(GroupEditors, EditBackgroundColorsMap[AKind]);
  end;

  ContainerBorderColor := GetColorByName(sdxSkinsContainerBorderColor);
  ContainerHighlightBorderColor := GetColorByName(sdxSkinsContainerHighlightBorderColor);
  ContentColor := GetColorByName(sdxSkinsContentColor);
  ContentEvenColor := GetColorByName(sdxSkinsContentEvenColor);
  ContentOddColor := GetColorByName(sdxSkinsContentOddColor);
  ContentTextColor := GetColorByName(sdxSkinsContentTextColor);
  DockControlHideBarTextColor[False] := GetColorByName(sdxSkinsDCHiddenBarTextColor);
  DockControlHideBarTextColor[True] := GetColorByName(sdxSkinsDCHiddenBarTextHotColor);
  HeaderBackgroundColor := GetColorByName(sdxSkinsHeaderBackgroundColor);
  HeaderBackgroundTextColor := GetColorByName(sdxSkinsHeaderBackgroundTextColor);
  InactiveColor := GetColorByName(sdxSkinsInactiveColor);
  InactiveTextColor := GetColorByName(sdxSkinsInactiveTextColor);
  LayoutControlColor := GetColorByName(sdxSkinsLayoutControlColor);
  SelectionColor := GetColorByName(sdxSkinsSelectionColor);
  SelectionTextColor := GetColorByName(sdxSkinsSelectionTextColor);

  TreeListGridLineColor := GetColorByName(sdxTreeListGridLineColor);
  TreeListTreeLineColor := GetColorByName(sdxTreeListTreeLineColor);

  SpreadSheetContentColor := GetColorByName(sdxSkinsSpreadSheetContentColor);
  SpreadSheetContentTextColor := GetColorByName(sdxSkinsSpreadSheetContentTextColor);
  SpreadSheetFrozenPaneSeparatorColor := GetColorByName(sdxSkinsSpreadSheetFrozenPaneSeparatorColor);
  SpreadSheetSelectionColor := GetColorByName(sdxSkinsSpreadSheetSelectionColor);
  SpreadSheetGroupLineColor := GetColorByName(sdxSkinsSpreadSheetGroupLineColor);

  GridLikeControlContentColor := GetColorByName(sdxSkinsGridLikeControlContentColor);
  GridLikeControlContentEvenColor := GetColorByName(sdxSkinsGridLikeControlContentEvenColor);
  GridLikeControlContentOddColor := GetColorByName(sdxSkinsGridLikeControlContentOddColor);
  GridLikeControlContentTextColor := GetColorByName(sdxSkinsGridLikeControlContentTextColor);
  GridLikeControlBackgroundColor := GetColorByName(sdxSkinsGridLikeControlBackgroundColor);
end;

procedure TdxSkinInfo.InitializeDockControlElements;
begin
  DockControlHideBarButtons := GetElementByName(GroupDocking, sdxDockCtrlTabHeaderAutoHideBar);
  DockControlTabButtonHorz := GetElementByName(GroupDocking, sdxDockCtrlTabButtonHorz);
  DockControlTabButtonVert := GetElementByName(GroupDocking, sdxDockCtrlTabButtonVert);
  DockControlTabHeader := GetElementByName(GroupDocking, sdxDockCtrlTabHeader);
  DockControlTabHeaderBackground := GetElementByName(GroupDocking, sdxDockCtrlTabHeaderBackground);
  DockControlTabHeaderCloseButton := GetElementByName(GroupDocking, sdxDockCtrlTabHeaderCloseButton);
  DockControlTabHeaderLine := GetElementByName(GroupDocking, sdxDockCtrlTabHeaderLine);
  DockControlWindowButton := GetElementByName(GroupDocking, sdxDockCtrlWindowButton);
  DockControlWindowButtonGlyphs := GetElementByName(GroupDocking, sdxDockCtrlWindowGlyphs);
  DockSiteContentColor := GetColorByName(GroupDocking, sdxDockSiteContentColor);

  DockControlHideBar := GetElementByName(GroupDocking, sdxDockCtrlAutoHideBar);
  DockControlHideBarLeft := GetElementByName(GroupDocking, sdxDockCtrlAutoHideBarLeft);
  DockControlHideBarRight := GetElementByName(GroupDocking, sdxDockCtrlAutoHideBarRight);
  DockControlHideBarBottom := GetElementByName(GroupDocking, sdxDockCtrlAutoHideBarBottom);

  DockControlCaption := GetElementByName(GroupDocking, sdxDockCtrlCaption);
  DockControlBorder := GetElementByName(GroupDocking, sdxDockCtrlBorder);
  DockControlCaptionNonFocusedTextColor := GetColorValueByName(DockControlCaption, sdxDockCtrlInactiveCaptionTextColor);

  FillChar(DockControlIndents, SizeOf(DockControlIndents), 0);
  if GroupDocking <> nil then
  begin
    DockControlIndents[0] := GetIntegerPropertyValue(GroupDocking, sdxDCActiveTabHeaderDownGrow);
    DockControlIndents[1] := GetIntegerPropertyValue(GroupDocking, sdxDCActiveTabHeaderHGrow);
    DockControlIndents[2] := GetIntegerPropertyValue(GroupDocking, sdxDCActiveTabHeaderUpGrow);
  end;
end;

procedure TdxSkinInfo.InitializeEditButtonElements;
const
  SliderArrowNames: array[TcxArrowDirection] of string = (
    sdxSliderArrowTop, sdxSliderArrowBottom, sdxSliderArrowLeft, sdxSliderArrowRight
  );
var
  AArrowDirection: TcxArrowDirection;
  AKind: TcxEditBtnKind;
  AProperty: TdxSkinBooleanProperty;
begin
  LoadingBig := GetElementByName(GroupCommon, sdxLoadingBig);

  LabelLine[False] := GetElementByName(GroupEditors, sdxLabelLine);
  LabelLine[True] := GetElementByName(GroupEditors, sdxLabelLineVert);
  BevelShapeColor1 := GetColorByName(GroupEditors, sdxBevelShapeColor1);
  BevelShapeColor2 := GetColorByName(GroupEditors, sdxBevelShapeColor2);
  ZoomInButton := GetElementByName(GroupEditors, sdxZoomInButton);
  ZoomOutButton := GetElementByName(GroupEditors, sdxZoomOutButton);

  EditButtonElements[False] := GetElementByName(GroupEditors, sdxEditorButton);
  EditButtonElements[True] := GetElementByName(GroupEditors, sdxCloseButton);
  for AKind := Low(TcxEditBtnKind) to High(TcxEditBtnKind) do
    EditButtonGlyphs[AKind] := GetElementByName(GroupEditors, EditButtonsMap[AKind]);
  EditButtonSearchGlyph := GetElementByName(GroupEditors, sdxSearchButtonGlyph);
  AProperty := GetBoolPropertyByName(EditButtonElements[False], sdxEditorButtonMergeBorders);
  EditButtonMergeBorders := Assigned(AProperty) and AProperty.Value;

  for AArrowDirection := Low(TcxArrowDirection) to High(TcxArrowDirection) do
    SliderArrow[AArrowDirection] := GetElementByName(GroupEditors, SliderArrowNames[AArrowDirection]);

  SetUseCache(EditButtonElements[False]);
end;

procedure TdxSkinInfo.InitializeFilterElements;
begin
  FilterButtons[False] := GetElementByName(GroupGrid, sdxFilterButton);
  FilterButtons[True] := GetElementByName(GroupGrid, sdxFilterButtonActive);
  SmartFilterButton := GetElementByName(GroupGrid, sdxSmartFilterButton);
  FilterPanel := GetElementByName(GroupGrid, sdxFilterPanel);
end;

procedure TdxSkinInfo.InitializeFooterElements;
begin
  FooterCell := GetElementByName(GroupGrid, sdxFooterCell);
  FooterPanel := GetElementByName(GroupGrid, sdxFooterPanel);
end;

procedure TdxSkinInfo.InitializeFormElements;

  procedure InitializeFormIcons;
  begin
    FillChar(FormIcons, SizeOf(FormIcons), 0);
    FormIcons[False, sfiClose] := GetElementByName(GroupForm, sdxSmallFormButtonClose);
    FormIcons[True, sfiClose] := GetElementByName(GroupForm, sdxFormButtonClose);
    FormIcons[True, sfiHelp] := GetElementByName(GroupForm, sdxFormButtonHelp);
    FormIcons[True, sfiMaximize] := GetElementByName(GroupForm, sdxFormButtonMaximize);
    FormIcons[True, sfiMinimize] := GetElementByName(GroupForm, sdxFormButtonMinimize);
    FormIcons[True, sfiRestore] := GetElementByName(GroupForm, sdxFormButtonRestore);
  end;

  procedure InitializeFormFrames;
  var
    ASide: TcxBorder;
    AStandard: Boolean;
  begin
    for AStandard := False to True do
      for ASide := Low(TcxBorder) to High(TcxBorder) do
        FormFrames[AStandard, ASide] := GetElementByName(GroupForm, FormFrameMap[AStandard, ASide]);
  end;

begin
  InitializeFormIcons;
  InitializeFormFrames;
  FormStatusBar := GetElementByName(GroupBars, sdxStatusBar);
  FormContent := GetElementByName(GroupForm, sdxFormContent);
  FormInactiveColor := GetColorByName(GroupForm, sdxTextColorInactive);
  FormTextShadowColor := GetColorByName(GroupForm, sdxTextShadowColor);
  FormCaptionFontDelta := Max(1, GetIntegerPropertyValue(FormFrames[True, bTop], sdxCaptionFontDelta));
  FormCaptionFontIsBold := GetBoolPropertyByName(FormFrames[True, bTop], sdxCaptionFontBold);
end;

procedure TdxSkinInfo.InitializeGroupBoxElements;
const
  GroupBoxNamesMap: array[TcxGroupBoxCaptionPosition] of string = (
    sdxGroupPanelTop, sdxGroupPanelBottom, sdxGroupPanelLeft,
    sdxGroupPanelRight, sdxGroupPanel
  );
  GroupBoxCaptionNamesMap: array[TcxGroupBoxCaptionPosition] of string = (
    sdxGroupPanelCaptionTop, sdxGroupPanelCaptionBottom,
    sdxGroupPanelCaptionLeft, sdxGroupPanelCaptionRight, ''
  );
var
  APosition: TcxGroupBoxCaptionPosition;
begin
  GroupBoxClient := GetElementByName(GroupCommon, sdxGroupPanelNoBorder);
  GroupButton := GetElementByName(GroupCommon, sdxGroupButton);
  GroupButtonExpandGlyph := GetElementByName(GroupCommon, sdxGroupButtonExpandGlyph);
  for APosition := Low(TcxGroupBoxCaptionPosition) to High(TcxGroupBoxCaptionPosition) do
  begin
    GroupBoxElements[APosition] := GetElementByName(GroupCommon, GroupBoxNamesMap[APosition]);
    GroupBoxCaptionElements[APosition] := GetElementByName(GroupCommon, GroupBoxCaptionNamesMap[APosition], APosition <> cxgpCenter);
    GroupBoxCaptionTailSizes[APosition] := GetIntegerPropertyByName(GroupBoxCaptionElements[APosition], sdxGroupPanelCaptionTailSize);
    GroupBoxCaptionTextPadding[APosition] := GetIntegerPropertyByName(GroupBoxCaptionElements[APosition], sdxGroupPanelCaptionTextPadding);
  end;
end;

procedure TdxSkinInfo.InitializeGalleryElements;
begin
  GalleryBackground := GetElementByName(GroupEditors, sdxGalleryBackground);
  GalleryGroup := GetElementByName(GroupEditors, sdxGalleryGroup);
  GalleryItem := GetElementByName(GroupEditors, sdxGalleryItem);
  GalleryItemGlyphFrame := GetElementByName(GroupEditors, sdxGalleryItemGlyphFrame);
end;

procedure TdxSkinInfo.InitializeGaugeElements;
begin
  GaugeBackground := GetElementByName(GroupCommon, sdxGaugeBackground);
end;

procedure TdxSkinInfo.InitializeGridElements;
var
  AGroupRow: TdxSkinElement;
begin
  GridFixedLine := GetElementByName(GroupGrid, sdxGridFixedLine);
  CardViewSeparator := GetElementByName(GroupGrid, sdxCardSeparator);
  GridGroupByBox := GetElementByName(GroupGrid, sdxGroupByBox);
  GridGroupRow := GetElementByName(GroupGrid, sdxGroupRow);
  GridLine := GetElementByName(GroupGrid, sdxGridLine);

  VGridContentColor := GetColorByName(GroupVGrid, sdxSkinsContentColor);
  VGridCategory := GetElementByName(GroupVGrid, sdxVGridCategory);
  VGridLine[False] := GetElementByName(GroupVGrid, sdxVGridLine);
  VGridLine[True] := GetElementByName(GroupVGrid, sdxVGridBandLine);
  VGridRowHeader := GetElementByName(GroupVGrid, sdxVGridRowHeader);

  AGroupRow := GetElementByName(GroupGrid, sdxGroupRow);
  GridGroupRowStyleOffice11ContentColor := GetColorByName(AGroupRow, sdxGridGroupRowStyleOffice11ContentColor);
  GridGroupRowStyleOffice11SeparatorColor := GetColorByName(AGroupRow, sdxGridGroupRowStyleOffice11SeparatorColor);
  GridGroupRowStyleOffice11TextColor := GetColorByName(AGroupRow, sdxGridGroupRowStyleOffice11TextColor);

  GridWinExplorerViewGroup := GetElementByName(GroupGrid, sdxGridWinExplorerViewGroup);
  GridWinExplorerViewGroupCaptionLine := GetElementByName(GroupGrid, sdxGridWinExplorerViewGroupCaptionLine);
  GridWinExplorerViewGroupExpandButton := GetElementByName(GroupGrid, sdxGridWinExplorerViewGroupExpandButton);
  GridWinExplorerViewRecord := GetElementByName(GroupGrid, sdxGridWinExplorerViewRecord);
end;

procedure TdxSkinInfo.InitializeHeaderElements;
begin
  Header := GetElementByName(GroupCommon, sdxHeader);
  HeaderSpecial := GetElementByName(GroupCommon, sdxHeaderSpecial);
  SortGlyphs := GetElementByName(GroupCommon, sdxSortGlyphs);
end;

procedure TdxSkinInfo.InitializeIndicatorImages;
begin
  IndicatorImages := GetElementByName(GroupGrid, sdxIndicatorImages);
  RatingIndicator := GetElementByName(GroupEditors, sdxRatingIndicator);
  HighlightedItem := GetElementByName(GroupCommon, sdxHighlightedItem);

  SetUseCache(HighlightedItem);
end;

procedure TdxSkinInfo.InitializeNavBarElements;
begin
  NavBarAccordionControlBackground := GetElementByName(GroupNavBar, sdxNavBarAccordionControlBackground);
  NavBarAccordionControlChildItemOffset := GetIntegerPropertyByName(GroupNavBar, sdxNavBarAccordionControlChildItemOffset);
  NavBarAccordionControlContentContainerPadding := GetPropertyByName(GroupNavBar, sdxNavBarAccordionControlContentContainerPadding) as TdxSkinRectProperty;
  NavBarAccordionControlDistanceBetweenRootGroups := GetIntegerPropertyByName(GroupNavBar, sdxNavBarAccordionControlDistanceBetweenRootGroups);
  NavBarAccordionControlGroup := GetElementByName(GroupNavBar, sdxNavBarAccordionControlGroup);
  NavBarAccordionControlGroupCloseButton := GetElementByName(GroupNavBar, sdxNavBarAccordionControlGroupCloseButton);
  NavBarAccordionControlGroupOpenButton := GetElementByName(GroupNavBar, sdxNavBarAccordionControlGroupOpenButton);
  NavBarAccordionControlItem := GetElementByName(GroupNavBar, sdxNavBarAccordionControlItem);
  NavBarAccordionControlRootGroup := GetElementByName(GroupNavBar, sdxNavBarAccordionControlRootGroup);
  NavBarAccordionControlRootGroupCloseButton := GetElementByName(GroupNavBar, sdxNavBarAccordionControlRootGroupCloseButton);
  NavBarAccordionControlRootGroupOpenButton := GetElementByName(GroupNavBar, sdxNavBarAccordionControlRootGroupOpenButton);
  NavBarAccordionControlSearchButton := GetElementByName(GroupNavBar, sdxNavBarAccordionControlSearchButton);
  NavBarAccordionControlSearchButtonGlyphToTextIndent := GetIntegerPropertyByName(
    NavBarAccordionControlSearchButton, sdxNavBarAccordionControlGlyphToTextIndent);

  NavBarBackgroundColor := GetElementByName(GroupNavBar, sdxNavBarBackground);
  NavBarGroupClient := GetElementByName(GroupNavBar, sdxNavBarGroupClient);
  NavBarItem := GetElementByName(GroupNavBar, sdxNavBarItem);
  NavBarGroupHeader := GetElementByName(GroupNavBar, sdxNavBarGroupHeader);
  NavBarGroupButtons[True] := GetElementByName(GroupNavBar, sdxNavBarGroupCloseButton);
  NavBarGroupButtons[False] := GetElementByName(GroupNavBar, sdxNavBarGroupOpenButton);

  NavPaneCollapseButton := GetElementByName(GroupNavPane, sdxNavPaneCollapseButton);
  NavPaneCollapsedGroupClient := GetElementByName(GroupNavPane, sdxNavPaneCollapsedGroupClient);
  NavPaneExpandButton := GetElementByName(GroupNavPane, sdxNavPaneExpandButton);
  NavPaneFormBorder := GetElementByName(GroupNavPane, sdxNavPaneFormBorder);
  NavPaneFormSizeGrip := GetElementByName(GroupNavPane, sdxNavPaneFormSizeGrip);
  NavPaneGroupButton[False] := GetElementByName(GroupNavPane, sdxNavPaneGroupButton);
  NavPaneGroupButton[True] := GetElementByName(GroupNavPane, sdxNavPaneGroupButtonSelected);
  NavPaneGroupCaption := GetElementByName(GroupNavPane, sdxNavPaneGroupCaption);
  NavPaneSplitter := GetElementByName(GroupNavPane, sdxNavPaneSplitter);
  NavPaneScrollButtons[False] := GetElementByName(GroupNavPane, sdxNavPaneScrollUpBtn);
  NavPaneScrollButtons[True] := GetElementByName(GroupNavPane, sdxNavPaneScrollDownBtn);
  NavPaneOverflowPanel := GetElementByName(GroupNavPane, sdxNavPaneOverflowPanel);
  NavPaneOverflowPanelItem := GetElementByName(GroupNavPane, sdxNavPaneOverflowPanelItem);
  NavPaneOverflowPanelExpandedItem := GetElementByName(GroupNavPane, sdxNavPaneOverflowPanelExpandItem);
  NavPaneGroupClient := GetElementByName(GroupNavPane, sdxNavPaneGroupClient);
  NavPaneItem := GetElementByName(GroupNavPane, sdxNavPaneItem);
  NavPaneSelectedItem := GetElementByName(GroupNavPane, sdxNavPaneItemSelected);
  NavPaneCaptionHeight := GetIntegerPropertyByName(NavPaneGroupCaption, sdxNavPaneCaptionHeight);
  NavPaneCaptionFontSize := GetIntegerPropertyByName(NavPaneGroupCaption, sdxFontSize);

  NavPaneOfficeNavigationBar := GetElementByName(GroupNavPane, sdxNavPaneOfficeNavigationBar);
  NavPaneOfficeNavigationBarItem := GetElementByName(GroupNavPane, sdxNavPaneOfficeNavigationBarItem);
  NavPaneOfficeNavigationBarItemFontDelta := GetIntegerPropertyValue(NavPaneOfficeNavigationBarItem, sdxCaptionFontDelta);
  NavPaneOfficeNavigationBarSkinningItem := GetElementByName(GroupNavPane, sdxNavPaneOfficeNavigationBarSkinningItem);
  NavPaneOfficeNavigationBarSkinningItemFontDelta := GetIntegerPropertyValue(NavPaneOfficeNavigationBarSkinningItem, sdxCaptionFontDelta);

  if Assigned(GroupNavPane) then
    NavPaneOffsetGroupBorders := GroupNavPane.GetPropertyByName(sdxNavPaneOffsetGroupBorders) as TdxSkinBooleanProperty
  else
    NavPaneOffsetGroupBorders := nil;
end;

procedure TdxSkinInfo.InitializeNavigatorElements;
begin
  NavigatorButton := GetElementByName(GroupEditors, sdxNavigatorButton);
  NavigatorGlyphs := GetElementByName(GroupEditors, sdxNavigatorGlyphs);
  NavigatorGlyphsVert := GetElementByName(GroupEditors, sdxNavigatorGlyphsVert);
  NavigatorInfoPanelColor := GetColorByName(GroupEditors, sdxNavigatorInfoPanelColor);
  NavigatorInfoPanelTextColor := GetColorByName(GroupEditors, sdxNavigatorInfoPanelTextColor);
end;

procedure TdxSkinInfo.InitializePageControlElements;
begin
  PageControlHeader := GetElementByName(GroupTabs, sdxPageControlHeader);
  PageControlButton := GetElementByName(GroupTabs, sdxPageControlButton);
  PageControlButtonHorz := GetElementByName(GroupTabs, sdxPageControlHorz);
  PageControlButtonVert := GetElementByName(GroupTabs, sdxPageControlVert);
  PageControlHeaderButton := GetElementByName(GroupTabs, sdxPageControlHeaderButton);
  PageControlCloseButton := GetElementByName(GroupTabs, sdxPageControlHeaderCloseButton);
  PageControlPane := GetElementByName(GroupTabs, sdxPageControlPane);
  FillChar(PageControlIndents, SizeOf(PageControlIndents), 0);
  if GroupTabs <> nil then
  begin
    PageControlIndents[0] := GetIntegerPropertyValue(GroupTabs, sdxRowIndentFar);
    PageControlIndents[1] := GetIntegerPropertyValue(GroupTabs, sdxRowIndentNear);
    PageControlIndents[2] := GetIntegerPropertyValue(GroupTabs, sdxSelectedHeaderDownGrow);
    PageControlIndents[3] := GetIntegerPropertyValue(GroupTabs, sdxSelectedHeaderHGrow);
    PageControlIndents[4] := GetIntegerPropertyValue(GroupTabs, sdxSelectedHeaderUpGrow);
    PageControlIndents[5] := GetIntegerPropertyValue(GroupTabs, sdxHeaderDownGrow);
    PageControlIndents[6] := GetIntegerPropertyValue(GroupTabs, sdxHeaderDownGrowBottomRight);
    PageControlIndents[7] := GetIntegerPropertyValue(GroupTabs, sdxSelectedHeaderDownGrowBottomRight);
  end;
end;

procedure TdxSkinInfo.InitializePDFViewerElements;
begin
  PDFViewerFindPanel := GetElementByName(GroupPDFViewer, sdxSkinsPDFViewerFindPanel);
  PDFViewerSelectionColor := GetColorByName(GroupPDFViewer, sdxSkinsPDFViewerSelectionColor);

  PDFViewerNavigationPaneBackground := GetElementByName(GroupPDFViewer, sdxSkinsPDFViewerNavigationPaneBackground);
  PDFViewerNavigationPaneButton := GetElementByName(GroupPDFViewer, sdxSkinsPDFViewerNavigationPaneButton);
  PDFViewerNavigationPaneButtonArrow := GetElementByName(GroupPDFViewer, sdxSkinsPDFViewerNavigationPaneButtonArrow);
  PDFViewerNavigationPaneButtonMinimized := GetElementByName(GroupPDFViewer, sdxSkinsPDFViewerNavigationPaneButtonMinimized);
  PDFViewerNavigationPanePageBackground := GetElementByName(GroupPDFViewer, sdxSkinsPDFViewerNavigationPanePageBackground);
  PDFViewerNavigationPanePageButton := GetElementByName(GroupPDFViewer, sdxSkinsPDFViewerNavigationPanePageButton);
  PDFViewerNavigationPanePageCaption := GetElementByName(GroupPDFViewer, sdxSkinsPDFViewerNavigationPanePageCaption);

  PDFViewerNavigationPaneSelectedPageExpandValue := GetIntegerPropertyByName(GroupPDFViewer, sdxSkinsPDFViewerNavigationPaneSelectedPageExpandValue);
  PDFViewerNavigationPaneSelectedPageOverlapValue := GetIntegerPropertyByName(GroupPDFViewer, sdxSkinsPDFViewerNavigationPaneSelectedPageOverlapValue);
end;

procedure TdxSkinInfo.InitializeProgressBarElements;
begin
  ProgressBarElements[False, False] := GetElementByName(GroupEditors, sdxProgressBorder);
  ProgressBarElements[False, True] := GetElementByName(GroupEditors, sdxProgressBorderVert);
  ProgressBarElements[True, False] := GetElementByName(GroupEditors, sdxProgressChunk);
  ProgressBarElements[True, True] := GetElementByName(GroupEditors, sdxProgressChunkVert);

  ProgressBarTextColors[False] := GetColorByName(GroupEditors, sdxSkinsProgressBarEmptyTextColor);
  ProgressBarTextColors[True] := GetColorByName(GroupEditors, sdxSkinsProgressBarFilledTextColor);
end;

procedure TdxSkinInfo.InitializeRadioGroupElements;
begin
  RadioGroupButton := GetElementByName(GroupEditors, sdxRadioGroup);
end;

procedure TdxSkinInfo.InitializeRangeControlElements;
begin
  RangeControlBorder := GetElementByName(GroupEditors, sdxRangeControlBorder);
  RangeControlLeftThumb := GetElementByName(GroupEditors, sdxRangeControlLeftThumb);
  RangeControlRightThumb := GetElementByName(GroupEditors, sdxRangeControlRightThumb);
  RangeControlRulerHeader := GetElementByName(GroupEditors, sdxRangeControlRulerHeader);
  RangeControlSizingGlyph := GetElementByName(GroupEditors, sdxRangeControlSizingGlyph);

  RangeControlDefaultElementColor := GetColorByName(RangeControlBorder, sdxRangeControlDefaultElementColor);
  RangeControlElementBaseColor := GetColorByName(RangeControlBorder, sdxRangeControlElementBaseColor);
  RangeControlElementFontSize := GetIntegerPropertyByName(RangeControlBorder, sdxRangeControlElementFontSize);
  RangeControlElementForeColor := GetColorByName(RangeControlBorder, sdxRangeControlElementForeColor);
  RangeControlLabelColor := GetColorByName(RangeControlBorder, sdxRangeControlLabelColor);
  RangeControlOutOfRangeColorMask := GetAlphaColorValueByName(RangeControlBorder, sdxRangeControlOutOfRangeColorMask);
  RangeControlRangePreviewColor := GetColorByName(RangeControlBorder, sdxRangeControlRangePreviewColor);
  RangeControlRuleColor := GetAlphaColorValueByName(RangeControlBorder, sdxRangeControlRuleColor);
  RangeControlScrollAreaColor := GetColorByName(RangeControlBorder, sdxRangeControlScrollAreaColor);
  RangeControlScrollAreaHeight := GetIntegerPropertyByName(RangeControlBorder, sdxRangeControlScrollAreaHeight);
  RangeControlSelectionBorderColor := GetAlphaColorValueByName(RangeControlBorder, sdxRangeControlSelectionBorderColor);
  RangeControlSelectionColor := GetAlphaColorValueByName(RangeControlBorder, sdxRangeControlSelectionColor);
  RangeControlViewPortPreviewColor := GetColorByName(RangeControlBorder, sdxRangeControlViewPortPreviewColor);
  RangeControlBackColor := GetAlphaColorValueByName(RangeControlBorder, sdxRangeControlBackColor);
  RangeControlInnerBorderColor := GetAlphaColorValueByName(RangeControlBorder, sdxRangeControlInnerBorderColor);
end;

procedure TdxSkinInfo.InitializeRibbonColors;

  function GetElementTextColor(AElement: TdxSkinElement): TColor;
  begin
    if AElement = nil then
      Result := clDefault
    else
      Result := AElement.TextColor;
  end;

begin
  RibbonEditorBackground := GetColorByName(GroupRibbon, sdxRibbonEditorBackground);
  RibbonExtraPaneColor := GetColorByName(GroupRibbon, sdxRibbonExtraPaneColor);
  RibbonExtraPaneHeaderSeparator := GetColorByName(GroupRibbon, sdxRibbonExtraPaneHeaderSeparator);

  RibbonCaptionText[False] := GetColorValueByName(RibbonFormCaption, sdxTextColorInactive);
  RibbonCaptionText[True] := GetElementTextColor(RibbonFormCaption);
  RibbonTabTextHot := GetColorValueByName(RibbonTab, sdxTextColorHot);
  RibbonTabText[True] := GetColorValueByName(RibbonTab, sdxTextColorSelected);
  RibbonTabText[False] := GetElementTextColor(RibbonTab);
  RibbonContextualTabHeaderText[True] :=  GetColorValueByName(RibbonContextualTabHeader, sdxTextColorSelected);
  RibbonContextualTabHeaderText[False] := GetElementTextColor(RibbonContextualTabHeader);
  RibbonContextualTabHeaderTextHot := GetColorValueByName(RibbonContextualTabHeader, sdxTextColorHot);
  RibbonDocumentNameTextColor[True] := GetColorValueByName(RibbonFormCaption, sdxRibbonDocumentNameTextColor);
  RibbonDocumentNameTextColor[False] := RibbonCaptionText[False];
  RibbonStatusBarTextSelected := GetColorValueByName(RibbonStatusBarButton, sdxTextColorSelected);

  RibbonButtonText[False] := GetElementTextColor(RibbonSmallButton);
  RibbonButtonText[True] := GetColorValueByName(GroupRibbon, sdxRibbonButtonDisabledText);
end;

procedure TdxSkinInfo.InitializeRichEditElements;
begin
  RichEditCornerPanel := GetElementByName(GroupRichEdit, sdxRichEditCornerPanel);
  RichEditRulerBackgroundHorz := GetElementByName(GroupRichEdit, sdxRichEditRulerBackgroundHorz);
  RichEditRulerBackgroundVert := GetElementByName(GroupRichEdit, sdxRichEditRulerBackgroundVert);
  RichEditRulerColumnResizer := GetElementByName(GroupRichEdit, sdxRichEditRulerColumnResizer);
  RichEditRulerDefaultTabColor := GetColorByName(GroupRichEdit, sdxRichEditRulerDefaultTabColor);
  RichEditRulerIndent := GetElementByName(GroupRichEdit, sdxRichEditRulerIndent);
  RichEditRulerIndentBottom := GetElementByName(GroupRichEdit, sdxRichEditRulerIndentBottom);
  RichEditRulerRightMargin := GetElementByName(GroupRichEdit, sdxRichEditRulerRightMargin);
  RichEditRulerSection := GetElementByName(GroupRichEdit, sdxRichEditRulerSection);
  RichEditRulerTab := GetElementByName(GroupRichEdit, sdxRichEditRulerTab);
  RichEditRulerTabTypeBackground := GetElementByName(GroupRichEdit, sdxRichEditRulerTabTypeBackground);
  RichEditRulerTextColor := GetColorByName(GroupRichEdit, sdxRichEditRulerTextColor);
end;

procedure TdxSkinInfo.InitializeRibbonElements;
begin
  RadialMenuBackgroundColor := GetColorByName(GroupRibbon, sdxSkinsRadialMenuBackgroundColor);
  RadialMenuBaseColor := GetColorByName(GroupRibbon, sdxSkinsRadialMenuBaseColor);

  RibbonApplicationBackground := GetElementByName(GroupRibbon, sdxRibbonAppMenuBackground);
  RibbonApplicationButton := GetElementByName(GroupRibbon, sdxRibbonApplicationButton);
  RibbonApplicationButton2010 := GetElementByName(GroupRibbon, sdxRibbonApplicationButton2010);
  RibbonApplicationFooterBackground := GetElementByName(GroupRibbon, sdxRibbonAppMenuFooterBackground);
  RibbonApplicationHeaderBackground := GetElementByName(GroupRibbon, sdxRibbonAppMenuHeaderBackground);
  RibbonExtraPanePinButtonGlyph := GetElementByName(GroupRibbon, sdxRibbonExtraPanePinButtonGlyph, False);
  RibbonExtraPaneButton := GetElementByName(GroupRibbon, sdxRibbonExtraPaneButton);

  RibbonBackstageView := GetElementByName(GroupRibbon, sdxRibbonBackstageViewBackground);
  RibbonBackstageViewBackButton := GetElementByName(GroupRibbon, sdxRibbonBackstageViewBackButton);
  RibbonBackstageViewImage := GetElementByName(GroupRibbon, sdxRibbonBackstageViewImage);
  RibbonBackstageViewMenu := GetElementByName(GroupRibbon, sdxRibbonBackstageViewMenuBackground);
  RibbonBackstageViewMenuHeader := GetElementByName(GroupRibbon, sdxRibbonBackstageViewMenuHeader);
  RibbonBackstageViewMenuButton := GetElementByName(GroupRibbon, sdxRibbonBackstageViewMenuButton);
  RibbonBackstageViewMenuSeparator := GetElementByName(GroupRibbon, sdxRibbonBackstageViewMenuSeparator);
  RibbonBackstageViewTab := GetElementByName(GroupRibbon, sdxRibbonBackstageViewTabHeader);
  RibbonBackstageViewTabArrow := GetElementByName(GroupRibbon, sdxRibbonBackstageViewTabHeaderArrow, False);

  RibbonCollapsedToolBarBackground := GetElementByName(GroupRibbon, sdxRibbonCollapsedToolBarBackground);
  RibbonCollapsedToolBarGlyphBackground := GetElementByName(GroupRibbon, sdxRibbonCollapsedToolBarGlyphBackground);

  RibbonFormButtonAutoHideModeShowUI := GetElementByName(GroupRibbon, sdxRibbonFormButtonAutoHideModeShowUI);
  RibbonFormButtonDisplayOptions := GetElementByName(GroupRibbon, sdxRibbonFormButtonDisplayOptions);
  RibbonFormButtonClose := GetElementByName(GroupRibbon, sdxFormButtonClose);
  RibbonFormButtonHelp := GetElementByName(GroupRibbon, sdxFormButtonHelp);
  RibbonFormButtonMaximize := GetElementByName(GroupRibbon, sdxFormButtonMaximize);
  RibbonFormButtonMinimize := GetElementByName(GroupRibbon, sdxFormButtonMinimize);
  RibbonFormButtonRestore := GetElementByName(GroupRibbon, sdxFormButtonRestore);

  RibbonFormCaption := GetElementByName(GroupRibbon, sdxRibbonFormCaption);
  RibbonFormCaptionRibbonHidden := GetElementByName(GroupRibbon, sdxRibbonFormCaptionRibbonHidden, False);
  RibbonFormContent := GetElementByName(GroupRibbon, sdxRibbonFormContent);
  RibbonFormBottom[False] := GetElementByName(GroupRibbon, sdxRibbonFormBottom);
  RibbonFormBottom[True] := GetElementByName(GroupRibbon, sdxRibbonDialogFrameBottom);
  RibbonFormLeft[False] := GetElementByName(GroupRibbon, sdxRibbonFormFrameLeft);
  RibbonFormLeft[True] := GetElementByName(GroupRibbon, sdxRibbonDialogFrameLeft);
  RibbonFormRight[False] := GetElementByName(GroupRibbon, sdxRibbonFormFrameRight);
  RibbonFormRight[True] := GetElementByName(GroupRibbon, sdxRibbonDialogFrameRight);
  RibbonUseRoundedWindowCorners := GetBoolPropertyByName(GroupRibbon, sdxRibbonUseRoundedWindowCorners);
  RibbonMinimizeButtonGlyph := GetElementByName(GroupRibbon, sdxRibbonMinimizeButtonGlyph);
  RibbonTab := GetElementByName(GroupRibbon, sdxRibbonTabHeaderPage);
  RibbonTabPanel := GetElementByName(GroupRibbon, sdxRibbonTabPanel);
  RibbonTabPanelBottomIndent := GetIntegerPropertyByName(RibbonTabPanel, sdxRibbonTabPanelBottomIndent);
  RibbonTabPanelGroupButton := GetElementByName(GroupRibbon, sdxRibbonTabPanelGroupButton);
  RibbonTabSeparatorLine := GetElementByName(GroupRibbon, sdxRibbonTabSeparatorLine);
  RibbonTabGroup := GetElementByName(GroupRibbon, sdxRibbonTabGroup);
  RibbonTabGroupHeader := GetElementByName(GroupRibbon, sdxRibbonTabGroupHeader);
  RibbonTabGroupItemsSeparator := GetElementByName(GroupRibbon, sdxRibbonTabGroupItemsSeparator);
  RibbonGalleryBackground := GetElementByName(GroupRibbon, sdxRibbonGalleryBackground);
  RibbonGalleryButtonDown := GetElementByName(GroupRibbon, sdxRibbonGalleryButtonDown);
  RibbonGalleryButtonDropDown := GetElementByName(GroupRibbon, sdxRibbonGalleryButtonDropDown);
  RibbonGalleryButtonUp := GetElementByName(GroupRibbon, sdxRibbonGalleryButtonUp);
  RibbonGalleryGroupCaption := GetElementByName(GroupRibbon, sdxRibbonGalleryGroupCaption);
  RibbonGalleryPane := GetElementByName(GroupRibbon, sdxRibbonGalleryPane);
  RibbonGallerySizingPanel := GetElementByName(GroupRibbon, sdxRibbonGallerySizingPanel);
  RibbonGallerySizeGrips := GetElementByName(GroupRibbon, sdxRibbonGallerySizeGrips);
  RibbonHeaderBackground := GetElementByName(GroupRibbon, sdxRibbonHeaderBackground);
  RibbonHeaderBackgroundOnGlass := GetElementByName(GroupRibbon, sdxRibbonHeaderBackgroundOnGlass);
  RibbonSmallButton := GetElementByName(GroupRibbon, sdxRibbonSmallButton);
  RibbonSplitButtonLeft := GetElementByName(GroupRibbon, sdxRibbonSplitButtonLeft);
  RibbonSplitButtonRight := GetElementByName(GroupRibbon, sdxRibbonSplitButtonRight);
  RibbonKeyTip := GetElementByName(GroupRibbon, sdxRibbonKeyTip);
  RibbonLargeButton := GetElementByName(GroupRibbon, sdxRibbonLargeButton);
  RibbonLargeSplitButtonTop := GetElementByName(GroupRibbon, sdxRibbonLargeSplitButtonTop);
  RibbonLargeSplitButtonBottom := GetElementByName(GroupRibbon, sdxRibbonLargeSplitButtonBottom);
  RibbonButtonArrow := GetElementByName(GroupRibbon, sdxRibbonButtonArrow);
  RibbonButtonGroup := GetElementByName(GroupRibbon, sdxRibbonButtonGroup);
  RibbonStatusBarBackground := GetElementByName(GroupRibbon, sdxRibbonStatusBarBackground);
  RibbonStatusBarButton := GetElementByName(GroupRibbon, sdxRibbonStatusBarButton);
  RibbonStatusBarSeparator := GetElementByName(GroupRibbon, sdxRibbonStatusBarSeparator);
  RibbonQuickToolbar[True] := GetElementByName(GroupRibbon, sdxRibbonQuickToolbarInCaption);
  RibbonQuickToolbar[False] := GetElementByName(GroupRibbon, sdxRibbonQuickToolbarAbove);
  RibbonQuickToolbarBelow := GetElementByName(GroupRibbon, sdxRibbonQuickToolbarBelow);
  RibbonQuickToolbarButtonGlyph := GetElementByName(GroupRibbon, sdxRibbonQuickToolbarButtonGlyph);
  RibbonQuickToolbarDropDown := GetElementByName(GroupRibbon, sdxRibbonQuickToolbarDropDown);
  RibbonQuickToolbarGlyph := GetElementByName(GroupRibbon, sdxRibbonQuickToolbarGlyph);
  RibbonButtonGroupButton := GetElementByName(GroupRibbon, sdxRibbonButtonGroupButton);
  RibbonButtonGroupSeparator := GetElementByName(GroupRibbon, sdxRibbonButtonGroupSeparator);
  RibbonButtonGroupSplitButtonLeft := GetElementByName(GroupRibbon, sdxRibbonButtonGroupSplitButtonLeft);
  RibbonButtonGroupSplitButtonRight := GetElementByName(GroupRibbon, sdxRibbonButtonGroupSplitButtonRight);
  RibbonGroupScroll[True] := GetElementByName(GroupRibbon, sdxRibbonTabGroupLeftScroll);
  RibbonGroupScroll[False] := GetElementByName(GroupRibbon, sdxRibbonTabGroupRightScroll);
  RibbonContextualTabLabel := GetElementByName(GroupRibbon, sdxRibbonContextualTabLabel);
  RibbonContextualTabLabelShadowColor := GetColorByName(RibbonContextualTabLabel, sdxTextShadowColor);
  RibbonContextualTabLabelOnGlass := GetElementByName(GroupRibbon, sdxRibbonContextualTabLabelOnGlass);
  RibbonContextualTabLabelOnGlassShadowColor := GetColorByName(RibbonContextualTabLabelOnGlass, sdxTextShadowColor);
  RibbonContextualTabSeparator := GetElementByName(GroupRibbon, sdxRibbonContextualTabSeparator);
  RibbonContextualTabHeader := GetElementByName(GroupRibbon, sdxRibbonContextualTabHeader);
  RibbonContextualTabPanel := GetElementByName(GroupRibbon, sdxRibbonContextualTabPanel);
  InitializeRibbonProperties;
  InitializeRibbonColors;

  SetUseCache(RibbonLargeButton);
  SetUseCache(RibbonSmallButton);
end;

procedure TdxSkinInfo.InitializeRibbonProperties;
var
  AIndex: Boolean;
begin
  for AIndex := False to True do
  begin
    RibbonQATCustomizeButtonOutsizeQAT[AIndex] :=
      GetBoolPropertyByName(RibbonQuickToolbar[AIndex], sdxRibbonQATCustomizeButtonOutsideQAT);
    RibbonQATIndentBeforeCustomizeButton[AIndex] :=
      GetIntegerPropertyByName(RibbonQuickToolbar[AIndex], sdxRibbonQATIndentBeforeCustomizeItem);
  end;
  RibbonCaptionFontDelta := GetIntegerPropertyByName(RibbonFormCaption, sdxCaptionFontDelta);
  RibbonSpaceBetweenTabGroups := GetIntegerPropertyByName(GroupRibbon, sdxRibbonSpaceBetweenTabGroups);
  RibbonTabHeaderDownGrowIndent := GetIntegerPropertyByName(GroupRibbon, sdxRibbonTabHeaderDownGrowIndent);
  RibbonTabAeroSupport := GetBoolPropertyByName(GroupRibbon, sdxRibbonTabAeroSupport);
end;

procedure TdxSkinInfo.InitializeScrollBarElements;

  procedure SetInfo(AHorz: Boolean; APart: TcxScrollBarPart;
    AElement: TdxSkinElement; AImageIndex: Integer = 0);
  begin
    FreeAndNil(ScrollBar_Elements[AHorz, APart]);
    if Skin <> nil then
      ScrollBar_Elements[AHorz, APart] :=
        TdxSkinScrollInfo.Create(AElement, AImageIndex, APart);
  end;

var
  AElement: TdxSkinElement;
begin
  // buttons
  AElement := GetElementByName(GroupCommon, sdxScrollButton);
  SetInfo(False, sbpLineUp, AElement);
  SetInfo(False, sbpLineDown, AElement, 1);
  SetInfo(True, sbpLineUp, AElement, 2);
  SetInfo(True, sbpLineDown, AElement, 3);
  // Thumbnail
  SetInfo(False, sbpThumbnail, GetElementByName(GroupCommon, sdxScrollThumbButtonVert));
  SetInfo(True, sbpThumbnail, GetElementByName(GroupCommon, sdxScrollThumbButtonHorz));
  // Page
  AElement := GetElementByName(GroupCommon, sdxScrollContentVert);
  SetInfo(False, sbpPageUp, AElement);
  SetInfo(False, sbpPageDown, AElement);
  AElement := GetElementByName(GroupCommon, sdxScrollContentHorz);
  SetInfo(True, sbpPageUp, AElement);
  SetInfo(True, sbpPageDown, AElement);
end;

procedure TdxSkinInfo.InitializeSchedulerElements;
var
  I: Integer;
begin
  for I := 0 to dxSkinsSchedulerResourceColorsCount - 1 do
    SchedulerResourceColors[I] := GetColorByName(GroupScheduler, Format(sdxSchedulerResourceColor, [I + 1]));
  SchedulerTimeGridHeader[False] := GetElementByName(GroupScheduler, sdxSchedulerTimeGridHeader);
  SchedulerTimeGridHeader[True] := GetElementByName(GroupScheduler, sdxSchedulerTimeGridHeaderSelected);
  SchedulerTimeLine := GetElementByName(GroupScheduler, sdxSchedulerTimeLine);
  SchedulerTimeRuler := GetElementByName(GroupScheduler, sdxSchedulerTimeRuler);
  SchedulerMoreButton := GetElementByName(GroupScheduler, sdxSchedulerMoreButton);
  SchedulerAppointment[False] := GetElementByName(GroupScheduler, sdxSchedulerAppointmentRight);
  SchedulerAppointment[True] := GetElementByName(GroupScheduler, sdxSchedulerAppointment);
  SchedulerAllDayArea[False] := GetElementByName(GroupScheduler, sdxSchedulerAllDayArea);
  SchedulerAllDayArea[True] := GetElementByName(GroupScheduler, sdxSchedulerAllDayAreaSelected);
  SchedulerCurrentTimeIndicator := GetElementByName(GroupScheduler, sdxSchedulerCurrentTimeIndicator);
  SchedulerAppointmentShadow[False] := GetElementByName(GroupScheduler, sdxSchedulerAppointmentBottomShadow);
  SchedulerAppointmentShadow[True] := GetElementByName(GroupScheduler, sdxSchedulerAppointmentRightShadow);
  SchedulerAppointmentBorderSize := GetIntegerPropertyByName(SchedulerAppointment[True], sdxSchedulerAppointmentBorderSize);
  SchedulerAppointmentMask := GetElementByName(GroupScheduler, sdxSchedulerAppointmentMask);
  SchedulerAppointmentBorder := GetColorByName(SchedulerAppointment[True], sdxSchedulerSeparatorColor);
  SchedulerGroup := GetElementByName(GroupScheduler, sdxSchedulerGroup);
  SchedulerMilestone := GetElementByName(GroupScheduler, sdxSchedulerMilestone);
  SchedulerNavigationButtons[False] := GetElementByName(GroupScheduler, sdxSchedulerNavButtonPrev);
  SchedulerNavigationButtons[True] := GetElementByName(GroupScheduler, sdxSchedulerNavButtonNext);
  SchedulerNavigationButtonsArrow[False] := GetElementByName(GroupScheduler, sdxSchedulerNavButtonPrevArrow);
  SchedulerNavigationButtonsArrow[True] := GetElementByName(GroupScheduler, sdxSchedulerNavButtonNextArrow);
  SchedulerTimeGridCurrentTimeIndicator := GetElementByName(GroupScheduler, sdxSchedulerTimeGridCurrentTimeIndicator);
  SchedulerLabelCircle := GetElementByName(GroupScheduler, sdxSchedulerLabelCircle);

  SetUseCache(SchedulerAppointment[False], 64);
  SetUseCache(SchedulerAppointment[True], 64);
  SetUseCache(SchedulerAllDayArea[False]);
  SetUseCache(SchedulerAllDayArea[True]);
  SetUseCache(SchedulerTimeRuler);
end;

procedure TdxSkinInfo.InitializeSizeGripElements;
begin
  SizeGrip := GetElementByName(GroupCommon, sdxSizeGrip);
end;

procedure TdxSkinInfo.InitializeSplitterElements;
begin
  Splitter[False] := GetElementByName(GroupCommon, sdxSplitterVert);
  Splitter[True] := GetElementByName(GroupCommon, sdxSplitterHorz);
end;

procedure TdxSkinInfo.InitializeTileControlElements;
begin
  TileControlActionBar := GetElementByName(GroupTileControl, sdxTileControlActionBar);
  TileControlBackground := GetElementByName(GroupTileControl, sdxTileControlBackground);
  TileControlItem := GetElementByName(GroupTileControl, sdxTileControlItem);
  TileControlItemCheck := GetElementByName(GroupTileControl, sdxTileControlItemCheck);
  TileControlGroupCaption := GetElementByName(GroupTileControl, sdxTileControlGroupCaption);
  TileControlGroupCaptionFontDelta := GetIntegerPropertyValue(TileControlGroupCaption, sdxCaptionFontDelta);
  TileControlVirtualGroup := GetElementByName(GroupTileControl, sdxTileControlVirtualGroup);
  TileControlTitle := GetElementByName(GroupTileControl, sdxTileControlTitle);
  TileControlTitleFontDelta := GetIntegerPropertyValue(TileControlTitle, sdxCaptionFontDelta);
  TileControlTabHeader := GetElementByName(GroupTileControl, sdxTileControlTabHeader);
  TileControlTabHeaderFontDelta := GetIntegerPropertyValue(TileControlTabHeader, sdxCaptionFontDelta);
  TileControlSelectionFocusedColor := GetColorValueByName(GroupTileControl, sdxTileControlSelectionFocusedColor);
  TileControlSelectionHotColor := GetColorValueByName(GroupTileControl, sdxTileControlSelectionHotColor);
end;

procedure TdxSkinInfo.InitializeToggleSwitchElements;
begin
  ToggleSwitch := GetElementByName(GroupEditors, sdxToggleSwitch);
  ToggleSwitchThumb := GetElementByName(GroupEditors, sdxToggleSwitchThumb);
  ToggleSwitchTextMargin := GetIntegerPropertyByName(ToggleSwitch, sdxToggleSwitchTextMargin);
end;

procedure TdxSkinInfo.InitializeToolTipElements;
begin
  ScreenTipWindow := GetElementByName(GroupBars, sdxScreenTipWindow);
  ScreenTipItem := GetColorByName(ScreenTipWindow, sdxScreenTipItem);
  ScreenTipSeparator := GetElementByName(GroupBars, sdxScreenTipSeparator);
  ScreenTipTitleItem := GetColorByName(ScreenTipWindow, sdxScreenTipTitleItem);
end;

procedure TdxSkinInfo.InitializeLayoutViewElements;
const
  LayoutViewRecordElementNameMap: array[TcxGroupBoxCaptionPosition] of string =
   (sdxLayoutViewRecordTop, sdxLayoutViewRecordBottom, sdxLayoutViewRecordLeft,
    sdxLayoutViewRecordRight, sdxLayoutViewRecord);
  LayoutViewRecordElementCaptionNameMap: array[TcxGroupBoxCaptionPosition] of string =
   (sdxLayoutViewRecordCaptionTop, sdxLayoutViewRecordCaptionBottom,
    sdxLayoutViewRecordCaptionLeft, sdxLayoutViewRecordCaptionRight, '');
var
  ACaptionPosition: TcxGroupBoxCaptionPosition;
  AElement: TcxLayoutElement;
begin
  LayoutViewItem := GetElementByName(GroupGrid, sdxLayoutViewItem);
  for AElement := Low(TcxLayoutElement) to High(TcxLayoutElement) do
  begin
    LayoutViewElementPadding[AElement] := GetPropertyByName(
      GroupGrid, LayoutViewElementPaddingMap[AElement]) as TdxSkinRectProperty;
    LayoutViewElementSpacing[AElement] := GetPropertyByName(
      GroupGrid, LayoutViewElementSpacingMap[AElement]) as TdxSkinRectProperty;
  end;

  LayoutViewRecordExpandButton := GetElementByName(GroupGrid, sdxLayoutViewRecordExpandButton);
  for ACaptionPosition := Low(ACaptionPosition) to High(ACaptionPosition) do
  begin
    LayoutViewRecordElements[ACaptionPosition] :=
      GetElementByName(GroupGrid, LayoutViewRecordElementNameMap[ACaptionPosition]);
    LayoutViewRecordCaptionElements[ACaptionPosition] := GetElementByName(
      GroupGrid, LayoutViewRecordElementCaptionNameMap[ACaptionPosition], ACaptionPosition <> cxgpCenter);
    LayoutViewRecordCaptionTailSizes[ACaptionPosition] := GetIntegerPropertyByName(
      LayoutViewRecordCaptionElements[ACaptionPosition], sdxGroupPanelCaptionTailSize);
    LayoutViewRecordCaptionTextPadding[ACaptionPosition] :=
      GetIntegerPropertyByName(GroupBoxCaptionElements[ACaptionPosition], sdxGroupPanelCaptionTextPadding);
  end;
end;

procedure TdxSkinInfo.InitializeMapControlElements;
const
  NamesMap: array[TcxButtonState] of string = (
    '', '', 'Highlighted', 'Selected', ''
  );
var
  AElement: TdxSkinElement;
  AState: TcxButtonState;
begin
  MapControlBackColor := GetColorValueByName(GroupMapControl, sdxMapControlBackColor);

  MapControlCustomElement := GetElementByName(GroupMapControl, sdxMapControlCustomElement);
  MapControlCustomElementTextGlowColor := GetAlphaColorValueByName(MapControlCustomElement, sdxTextGlowColor);

  MapControlCallout := GetElementByName(GroupMapControl, sdxMapControlCallout);
  MapControlCalloutTextGlowColor := GetAlphaColorValueByName(MapControlCallout, sdxTextGlowColor);
  MapControlCalloutPointerHeight := GetIntegerPropertyValue(MapControlCallout, sdxMapControlCalloutPointerHeight);
  MapControlCalloutPointer := cxPoint(
    GetIntegerPropertyValue(MapControlCallout, sdxMapControlCalloutPointerX),
    GetIntegerPropertyValue(MapControlCallout, sdxMapControlCalloutPointerY));

  MapControlPushpin := GetElementByName(GroupMapControl, sdxMapControlPushpin);
  MapControlPushpinTextOrigin := cxPoint(
    GetIntegerPropertyValue(MapControlPushpin, sdxMapControlPushpinTextOriginX),
    GetIntegerPropertyValue(MapControlPushpin, sdxMapControlPushpinTextOriginY));
  MapControlPushpinTextGlowColor := GetAlphaColorValueByName(MapControlPushpin, sdxTextGlowColor);

  AElement := GetElementByName(GroupMapControl, sdxMapControlSelectedRegion);
  MapControlSelectedRegionBackgroundColor := GetAlphaColorValueByName(AElement, 'BackColor');
  MapControlSelectedRegionBorderColor := GetAlphaColorValueByName(AElement, sdxMapControlBorderColor);

  MapControlPanelBackColor := GetAlphaColorValueByName(GroupMapControl, sdxMapControlPanelBackColor);
  MapControlPanelHotTrackedTextColor := GetAlphaColorValueByName(GroupMapControl, sdxMapControlPanelHotTrackedTextColor);
  MapControlPanelPressedTextColor := GetAlphaColorValueByName(GroupMapControl, sdxMapControlPanelPressedTextColor);
  MapControlPanelTextColor := GetAlphaColorValueByName(GroupMapControl, sdxMapControlPanelTextColor);

  AElement := GetElementByName(GroupMapControl, sdxMapControlShape);
  for AState := Low(TcxButtonState) to High(TcxButtonState) do
  begin
    MapControlShapeBorderColor[AState] := GetAlphaColorValueByName(AElement, NamesMap[AState] + sdxMapControlBorderColor);
    MapControlShapeBorderWidth[AState] := GetIntegerPropertyValue(AElement, NamesMap[AState] + sdxMapControlBorderWidth);
    MapControlShapeColor[AState] := GetAlphaColorValueByName(AElement, IfThen(NamesMap[AState] <> '', NamesMap[AState], 'Back') + sdxColor);
  end;
end;

procedure TdxSkinInfo.InitializePrintingSystemElements;
begin
  PrintingPageBorder := GetElementByName(GroupPrintingSystem, sdxPrintingSystemPageBorder);
  if PrintingPageBorder <> nil then
    PrintingPageBorder.UseCache := True;
  PrintingPreviewBackground := GetElementByName(GroupPrintingSystem, sdxPrintingSystemPreviewBackground);
  if PrintingPreviewBackground <> nil then
    PrintingPreviewBackground.UseCache := True;
end;

procedure TdxSkinInfo.InitializeTrackBarElements;
begin
  RangeTrackBarThumbLeft := GetElementByName(GroupEditors, sdxRangeTrackBarThumbLeft);
  RangeTrackBarThumbRight := GetElementByName(GroupEditors, sdxRangeTrackBarThumbRight);
  RangeTrackBarThumbBoth := GetElementByName(GroupEditors, sdxRangeTrackBarThumbBoth);

  TrackBarTickColor := GetColorByName(GroupEditors, sdxTrackBarTickColor);

  TrackBarTrack[True] := GetElementByName(GroupEditors, sdxTrackBarTrack);
  TrackBarTrack[False] := GetElementByName(GroupEditors, sdxTrackBarTrackVert);

  TrackBarThumb[True, tbtaDown] := GetElementByName(GroupEditors, sdxTrackBarThumb);
  TrackBarThumb[True, tbtaUp] := GetElementByName(GroupEditors, sdxTrackBarThumbUp);
  TrackBarThumb[True, tbtaBoth] := GetElementByName(GroupEditors, sdxTrackBarThumbBoth);

  TrackBarThumb[False, tbtaDown] := GetElementByName(GroupEditors, sdxTrackBarThumbVert);
  TrackBarThumb[False, tbtaUp] := GetElementByName(GroupEditors, sdxTrackBarThumbVertUp);
  TrackBarThumb[False, tbtaBoth] := GetElementByName(GroupEditors, sdxTrackBarThumbVertBoth);
end;

procedure TdxSkinInfo.InitializeSkinInfo;
begin
  InitializeGroups;
  InitializeAlertWindowElements;
  InitializeBarElements;
  InitializeFormElements;
  InitializeDockControlElements;
  InitializeBreadcrumbEditElements;
  InitializeButtonElements;
  InitializeFooterElements;
  InitializeCalcEditColors;
  InitializeCalendarElements;
  InitializeCheckboxElements;
  InitializeClockElements;
  InitializeEditButtonElements;
  InitializeGroupBoxElements;
  InitializeGridElements;
  InitializeGalleryElements;
  InitializeGaugeElements;
  InitializeIndicatorImages;
  InitializeNavBarElements;
  InitializeNavigatorElements;
  InitializeSchedulerElements;
  InitializeHeaderElements;
  InitializeFilterElements;
  InitializePageControlElements;
  InitializeProgressBarElements;
  InitializeRadioGroupElements;
  InitializeRangeControlElements;
  InitializeScrollBarElements;
  InitializeRibbonElements;
  InitializeRichEditElements;
  InitializeSizeGripElements;
  InitializeSplitterElements;
  InitializeTrackBarElements;
  InitializeToolTipElements;
  InitializeToggleSwitchElements;
  InitializeTileControlElements;
  InitializePrintingSystemElements;
  InitializePDFViewerElements;
  InitializeMapControlElements;
  InitializeLayoutViewElements;
  InitializeColors;
end;

procedure TdxSkinInfo.FinalizeScrollBarElements;
var
  AHorz: Boolean;
  APart: TcxScrollBarPart;
begin
  for AHorz := False to True do
    for APart := Low(TcxScrollBarPart) to High(TcxScrollBarPart) do
      FreeAndNil(ScrollBar_Elements[AHorz, APart]);
end;

procedure TdxSkinInfo.FinalizeSkinInfo;
begin
  FinalizeScrollBarElements;
end;

procedure TdxSkinInfo.SetUseCache(AElement: TdxSkinElement; ACapacity: Integer = -1);
begin
  if AElement <> nil then
  begin
    AElement.UseCache := True;
    if ACapacity > 0 then
      TdxSkinElementAccess(AElement).Cache.Capacity := ACapacity;
  end;
end;

function TdxSkinInfo.GetSkin: TdxSkin;
begin
  Result := Skin;
end;

procedure TdxSkinInfo.SkinChanged(Sender: TdxSkin);
begin
  FinalizeSkinInfo;
  InitializeSkinInfo;
end;

procedure TdxSkinInfo.MarkObjectUsed(AObject: TdxSkinCustomObject);
begin
  if Assigned(AObject) then
    AObject.State := AObject.State - [sosUnused];
end;

procedure TdxSkinInfo.SetSkin(ASkin: TdxSkin);
begin
  if ASkin <> Skin then
  begin
    if FSkin <> nil then
    begin
      FinalizeSkinInfo;
      FSkin.RemoveListener(Self);
      FSkin := nil;
    end;
    if ASkin <> nil then
    begin
      FSkin := ASkin;
      InitializeSkinInfo;
      FSkin.AddListener(Self);
    end;
  end;
end;

{ TdxSkinScrollInfo }

constructor TdxSkinScrollInfo.Create(AElement: TdxSkinElement;
  AImageIndex: Integer; APart: TcxScrollBarPart);
begin
  FElement := AElement;
  FImageIndex := AImageIndex;
end;

function TdxSkinScrollInfo.DrawScaled(DC: HDC; const R: TRect; AImageIndex: Integer; AState: TdxSkinElementState;
  AScaleFactor: TdxScaleFactor): Boolean;
begin
  Result := Element <> nil;
  if Result then
    Element.Draw(DC, R, AScaleFactor, AImageIndex, AState);
end;

function TdxSkinScrollInfo.Draw(DC: HDC; const R: TRect;
  AImageIndex: Integer; AState: TdxSkinElementState): Boolean;
begin
  Result := DrawScaled(DC, R, AImageIndex, AState, dxSystemScaleFactor);
end;

function TdxSkinScrollInfo.DrawScaled(DC: HDC; const R: TRect; AState: TdxSkinElementState;
  AScaleFactor: TdxScaleFactor): Boolean;
begin
  Result := DrawScaled(DC, R, ImageIndex, AState, AScaleFactor);
end;

function TdxSkinScrollInfo.Draw(DC: HDC; const R: TRect; AState: TdxSkinElementState): Boolean;
begin
  Result := DrawScaled(DC, R, ImageIndex, AState, dxSystemScaleFactor);
end;

end.
