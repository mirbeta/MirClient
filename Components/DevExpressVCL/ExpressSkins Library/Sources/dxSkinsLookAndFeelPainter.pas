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

unit dxSkinsLookAndFeelPainter;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, Windows, Classes, Graphics, SysUtils, ImgList,
  cxLookAndFeels, cxLookAndFeelPainters, dxCore, dxCoreGraphics, cxGraphics, cxGeometry, cxClasses, dxGdiPlusApi,
  dxSkinsCore, dxSkinInfo, dxSkinsStrs;

type

  { TdxSkinLookAndFeelPainterInfo }

  TdxSkinLookAndFeelPainterInfo = class(TdxSkinInfo);
  TdxSkinLookAndFeelPainterInfoClass = class of TdxSkinLookAndFeelPainterInfo;

  { TcxSkinLookAndFeelPainter }

  TdxSkinLookAndFeelPainterClass = class of TdxSkinLookAndFeelPainter;
  TdxSkinLookAndFeelPainter = class(TcxOffice11LookAndFeelPainter)
  strict private const
    CalendarElementStateToButtonState: array [TcxCalendarElementState] of TcxButtonState = (
      cxbsNormal, cxbsHot, cxbsPressed, cxbsPressed, cxbsPressed, cxbsPressed, cxbsDisabled
    );
    CalendarElementStateToSkinElementState: array [TcxCalendarElementState] of TdxSkinElementState = (
      esNormal, esHot, esPressed, esFocused, esNormal, esNormal, esDisabled
    );
    LayoutViewRecordState: array[TcxButtonState] of TdxSkinElementState = (
      esNormal, esNormal, esActive, esActive, esActiveDisabled
    );
  strict private
    FSkinInfo: TdxSkinLookAndFeelPainterInfo;
    FSkinResInstance: THandle;
    FSkinResName: string;

    procedure DrawSkinElementRotated(ACanvas: TcxCanvas; ASkinElement: TdxSkinElement; const ARect: TRect;
      AState: TdxSkinElementState; AAngle: TcxRotationAngle; AFlipVertically: Boolean = False; AScaleFactor: TdxScaleFactor = nil);
    function GetSkinElementBordersWidth(AElement: TdxSkinElement): TRect;
    function GetSkinElementColorPalette(AElement: TdxSkinElement; AState: TdxSkinElementState): IdxColorPalette; inline;
    function GetSkinElementMinSize(AElement: TdxSkinElement): TSize;
    function GetSkinDetails: TdxSkinDetails;
    function GetSkinInfo: TdxSkinLookAndFeelPainterInfo;
    function IsFontBold(AElement: TdxSkinElement): Boolean;
  protected
    function CreateLookAndFeelPainterDetails: TObject; override;
    function GetLookAndFeelPainterDetails: TObject; override;

    procedure DrawContent(ACanvas: TcxCanvas; const ABounds, ATextAreaBounds: TRect; AState: Integer;
      AAlignmentHorz: TAlignment; AAlignmentVert: TcxAlignmentVert; AMultiLine, AShowEndEllipsis: Boolean;
      const AText: string; AFont: TFont; ATextColor, ABkColor: TColor; AOnDrawBackground: TcxDrawBackgroundEvent = nil;
      AIsFooter: Boolean = False); override;
    procedure DrawGroupCaption(ACanvas: TcxCanvas; const ACaptionRect, ATextRect: TRect;
      AElement: TdxSkinElement; ATextPadding: TdxSkinIntegerProperty; AState: TdxSkinElementState); virtual;
    procedure DrawSchedulerNavigationButtonContent(ACanvas: TcxCanvas; const ARect: TRect;
      const AArrowRect: TRect; AIsNextButton: Boolean; AState: TcxButtonState; const AIsVertical: Boolean = True); override;

    function DrawRightToLeftDependentSkinElement(AElement: TdxSkinElement; ACanvas: TcxCanvas; const R: TRect;
      AScaleFactor: TdxScaleFactor; AState: TdxSkinElementState = esNormal; AImageIndex: Integer = 0): Boolean; inline;
    function DrawSkinElement(AElement: TdxSkinElement; ACanvas: TcxCanvas; const R: TRect;
      AState: TdxSkinElementState = esNormal; AImageIndex: Integer = 0): Boolean; overload; inline;
    function DrawSkinElement(AElement: TdxSkinElement; ACanvas: TcxCanvas; const R: TRect;
      AScaleFactor: TdxScaleFactor; AState: TdxSkinElementState = esNormal; AImageIndex: Integer = 0): Boolean; overload; inline;

    function GalleryStateToButtonState(const AState: TdxGalleryItemViewState): TcxButtonState;
    function GetSkinInfoClass: TdxSkinLookAndFeelPainterInfoClass; virtual;
    function IsColorPropertyAssigned(AColor: TdxSkinColor): Boolean; inline;
    function IsSkinElementColorAssigned(AElement: TdxSkinElement): Boolean; inline;
    function IsSkinElementTextColorAssigned(AElement: TdxSkinElement): Boolean; inline;
  public
    constructor Create(const ASkinResName: string; ASkinResInstance: THandle); virtual;
    destructor Destroy; override;
    function GetPainterData(var AData): Boolean; override;
    function GetPainterDetails(var ADetails): Boolean; override;
    function LookAndFeelName: string; override;
    function LookAndFeelStyle: TcxLookAndFeelStyle; override;
    // colors
    function DefaultChartHistogramPlotColor: TColor; override;
    function DefaultContentColor: TColor; override;
    function DefaultContentEvenColor: TColor; override;
    function DefaultContentOddColor: TColor; override;
    function DefaultContentTextColor: TColor; override;
    function DefaultControlColor: TColor; override;
    function DefaultControlTextColor: TColor; override;
    function DefaultEditorBackgroundColorEx(AKind: TcxEditStateColorKind): TColor; override;
    function DefaultEditorTextColorEx(AKind: TcxEditStateColorKind): TColor; override;
    function DefaultFilterBoxTextColor: TColor; override;
    function DefaultFixedSeparatorColor: TColor; override;
    function DefaultFooterColor: TColor; override;
    function DefaultFooterTextColor: TColor; override;
    function DefaultGridDetailsSiteColor: TColor; override;
    function DefaultGridLineColor: TColor; override;
    function DefaultGroupByBoxTextColor: TColor; override;
    function DefaultGroupColor: TColor; override;
    function DefaultGroupContentOffsets: TRect; override;
    function DefaultGroupTextColor: TColor; override;
    function DefaultHeaderBackgroundColor: TColor; override;
    function DefaultHeaderBackgroundTextColor: TColor; override;
    function DefaultHeaderColor: TColor; override;
    function DefaultHeaderTextColor: TColor; override;
    function DefaultHyperlinkTextColor: TColor; override;
    function DefaultInactiveColor: TColor; override;
    function DefaultInactiveTextColor: TColor; override;
    function DefaultPreviewTextColor: TColor; override;
    function DefaultRecordSeparatorColor: TColor; override;
    function DefaultSelectionColor: TColor; override;
    function DefaultSelectionTextColor: TColor; override;
    function DefaultSeparatorColor: TColor; override;
    function DefaultSizeGripAreaColor: TColor; override;
    function DefaultTabTextColor: TColor; override;
    function DefaultTimeGridMajorScaleTextColor: TColor; override;
    function DefaultTimeGridMinorScaleTextColor: TColor; override;

    function DefaultGridOptionsTreeViewCategoryColor(ASelected: Boolean): TColor; override;
    function DefaultGridOptionsTreeViewCategoryTextColor(ASelected: Boolean): TColor; override;

    function DefaultSchedulerBackgroundColor: TColor; override;
    function DefaultSchedulerBorderColor: TColor; override;
    function DefaultSchedulerContentColor(AResourceIndex: Integer): TColor; override;
    function DefaultSchedulerControlColor: TColor; override;
    function DefaultSchedulerDateNavigatorArrowColor(AIsHighlight: Boolean): TColor; override;
    function DefaultSchedulerDayHeaderColor: TColor; override;
    function DefaultSchedulerDayHeaderTextColor: TColor; override;
    function DefaultSchedulerEventColor(AIsAllDayEvent: Boolean): TColor; override;
    function DefaultSchedulerEventColorClassic(AIsAllDayEvent: Boolean): TColor; override;
    function DefaultSchedulerHeaderContainerTextColor(ASelected: Boolean): TColor; override;
    function DefaultSchedulerNavigatorColor: TColor; override;
    function DefaultSchedulerTimeRulerColor: TColor; override;
    function DefaultSchedulerTimeRulerTextColorClassic: TColor; override;
    function DefaultSchedulerViewContentColor: TColor; override;
    function DefaultSchedulerViewContentColorClassic: TColor; override;
    function DefaultSchedulerViewSelectedTextColor: TColor; override;
    function DefaultSchedulerViewTextColor: TColor; override;
    function DefaultSchedulerYearViewUnusedContentColor(AIsWorkTime: Boolean): TColor; override;

    function DefaultTreeListGridLineColor: TColor; override;
    function DefaultTreeListTreeLineColor: TColor; override;

    function DefaultLayoutViewCaptionTextColor(ACaptionPosition: TcxGroupBoxCaptionPosition; AState: TcxButtonState): TColor; override;
    function DefaultLayoutViewContentTextColor(AState: TcxButtonState): TColor; override;

    function DefaultDateNavigatorHeaderColor: TColor; override;
    function DefaultDateNavigatorHeaderTextColor(AIsHighlight: Boolean): TColor; override;
    function DefaultDateNavigatorHolydayTextColor: TColor; override;
    function DefaultDateNavigatorInactiveTextColor: TColor; override;
    function DefaultDateNavigatorSelectionColor: TColor; override;
    function DefaultDateNavigatorSelectionTextColor: TColor; override;
    function DefaultDateNavigatorSeparator1Color: TColor; override;
    function DefaultDateNavigatorSeparator2Color: TColor; override;
    function DefaultDateNavigatorTextColor: TColor; override;
    function DefaultDateNavigatorTodayFrameColor: TColor; override;
    function DefaultDateNavigatorTodayTextColor(ASelected: Boolean): TColor; override;

    function DefaultVGridBandLineColor: TColor; override;
    function DefaultVGridCategoryColor: TColor; override;
    function DefaultVGridCategoryTextColor: TColor; override;
    function DefaultVGridContentColor: TColor; override;
    function DefaultVGridContentEvenColor: TColor; override;
    function DefaultVGridContentOddColor: TColor; override;
    function DefaultVGridHeaderColor: TColor; override;
    function DefaultVGridHeaderTextColor: TColor; override;
    function DefaultVGridLineColor: TColor; override;

    // borders
    function SeparatorSize: Integer; override;
    procedure DrawBorder(ACanvas: TcxCanvas; R: TRect); override;
    procedure DrawContainerBorder(ACanvas: TcxCanvas; const R: TRect; AStyle: TcxContainerBorderStyle;
      AWidth: Integer; AColor: TColor; ABorders: TcxBorders); override;
    procedure DoDrawSeparator(ACanvas: TcxCanvas; R: TRect; AIsVertical: Boolean); override;

    // buttons
    function ButtonBorderSize(AState: TcxButtonState = cxbsNormal): Integer; override;
    function ButtonColorPalette(AState: TcxButtonState): IdxColorPalette; override;
    function ButtonDescriptionTextColor(AState: TcxButtonState; ADefaultColor: TColor = clDefault): TColor; override;
    function ButtonSymbolColor(AState: TcxButtonState; ADefaultColor: TColor = clDefault): TColor; override;
    procedure DrawScaledButton(ACanvas: TcxCanvas; R: TRect; const ACaption: string; AState: TcxButtonState;
      AScaleFactor: TdxScaleFactor; ADrawBorder: Boolean = True; AColor: TColor = clDefault;
      ATextColor: TColor = clDefault; AWordWrap: Boolean = False; AIsToolButton: Boolean = False;
      APart: TcxButtonPart = cxbpButton); override;
    procedure DrawScaledSearchEditButtonGlyph(ACanvas: TcxCanvas; R: TRect; AState: TcxButtonState; AScaleFactor: TdxScaleFactor); override;
    function GetScaledDropDownButtonRightPartSize(AScaleFactor: TdxScaleFactor): Integer; override;

    // expand button
    procedure DrawScaledExpandButton(ACanvas: TcxCanvas; const R: TRect;
      AExpanded: Boolean; AScaleFactor: TdxScaleFactor; AColor: TColor = clDefault;
      AState: TcxExpandButtonState = cebsNormal); override;
    procedure DrawScaledExpandButtonEx(ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState;
      AExpanded: Boolean; AScaleFactor: TdxScaleFactor; ARotationAngle: TcxRotationAngle = ra0); override;
    procedure DrawScaledSmallExpandButton(ACanvas: TcxCanvas; R: TRect; AExpanded: Boolean;
      ABorderColor: TColor; AScaleFactor: TdxScaleFactor; AColor: TColor = clDefault); override;
    function ScaledExpandButtonSize(AScaleFactor: TdxScaleFactor): Integer; override;

    // scroll bars
    function ScaledScrollBarMinimalThumbSize(AVertical: Boolean; AScaleFactor: TdxScaleFactor): Integer; override;
    procedure DrawScaledScrollBarBackground(ACanvas: TcxCanvas; const R: TRect; AHorizontal: Boolean;
      AScaleFactor: TdxScaleFactor); override;
    procedure DrawScaledScrollBarPart(ACanvas: TcxCanvas; AHorizontal: Boolean; R: TRect; APart: TcxScrollBarPart;
      AState: TcxButtonState; AScaleFactor: TdxScaleFactor); override;
    procedure DrawScaledScrollBarSplitter(ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState; AScaleFactor: TdxScaleFactor); override;

    // label line
    procedure DrawLabelLine(ACanvas: TcxCanvas; const R: TRect; AOuterColor, AInnerColor: TColor; AIsVertical: Boolean); override;
    function LabelLineHeight: Integer; override;

    // size grip
    function ScaledSizeGripSize(AScaleFactor: TdxScaleFactor): TSize; override;
    procedure DoDrawSizeGrip(ACanvas: TcxCanvas; const ARect: TRect; AScaleFactor: TdxScaleFactor); override;

    // Slider
    function ScaledSliderButtonSize(ADirection: TcxArrowDirection; AScaleFactor: TdxScaleFactor): TSize; override;
    procedure DrawScaledSliderButton(ACanvas: TcxCanvas; const ARect: TRect;
      ADirection: TcxArrowDirection; AState: TcxButtonState; AScaleFactor: TdxScaleFactor); override;

    // SmallCloseButton
    function DoGetSmallCloseButtonSize: TSize; override;
    function GetSmallButtonColorPalette(AState: TcxButtonState): IdxColorPalette; override;
    procedure DrawScaledSmallButton(ACanvas: TcxCanvas; R: TRect; AState: TcxButtonState; AScaleFactor: TdxScaleFactor); override;
    procedure DrawScaledSmallCloseButton(ACanvas: TcxCanvas; R: TRect; AState: TcxButtonState; AScaleFactor: TdxScaleFactor); override;

    // RadioGroup
    procedure DrawScaledRadioButton(ACanvas: TcxCanvas; X, Y: Integer; AButtonState: TcxButtonState;
      AChecked, AFocused: Boolean; ABrushColor: TColor; AScaleFactor: TdxScaleFactor; AIsDesigning: Boolean = False); override;
    function ScaledRadioButtonSize(AScaleFactor: TdxScaleFactor): TSize; override;

    // Checkbox
    function ScaledCheckButtonSize(AScaleFactor: TdxScaleFactor): TSize; override;
    procedure DrawScaledCheckButton(ACanvas: TcxCanvas; R: TRect;
      AState: TcxButtonState; ACheckState: TcxCheckBoxState; AScaleFactor: TdxScaleFactor); override;

    // ToggleSwitch
    procedure DrawScaledToggleSwitchState(ACanvas: TcxCanvas; ABounds: TRect;
      AState: TcxButtonState; AChecked: Boolean; AScaleFactor: TdxScaleFactor); override;
    procedure DrawScaledToggleSwitchThumb(ACanvas: TcxCanvas; ABounds: TRect;
      AState: TcxButtonState; AScaleFactor: TdxScaleFactor); override;
    function GetToggleSwitchColorPalette: IdxColorPalette; override;
    function GetToggleSwitchThumbPercentsWidth: Integer; override;
    function GetToggleSwitchTextColor: TColor; override;

    // Editors
    procedure DrawScaledEditorButton(ACanvas: TcxCanvas; const ARect: TRect; AButtonKind: TcxEditBtnKind;
      AState: TcxButtonState; AScaleFactor: TdxScaleFactor; APosition: TcxEditBtnPosition = cxbpRight); override;
    procedure DrawScaledEditorButtonGlyph(ACanvas: TcxCanvas; const ARect: TRect; AButtonKind: TcxEditBtnKind;
      AState: TcxButtonState; AScaleFactor: TdxScaleFactor; APosition: TcxEditBtnPosition = cxbpRight); override;
    procedure EditButtonAdjustRect(var R: TRect; APosition: TcxEditBtnPosition = cxbpRight); virtual;
    function EditButtonColorPalette(AState: TcxButtonState): IdxColorPalette; override;
    function EditButtonSize: TSize; override;
    function EditButtonTextColor: TColor; override;
    function EditButtonTextOffset: Integer; override;
    function GetContainerBorderColor(AIsHighlightBorder: Boolean): TColor; override;
    function GetContainerBorderWidth(ABorderStyle: TcxContainerBorderStyle): Integer; override;

    // Clock
    function ScaledClockSize(AScaleFactor: TdxScaleFactor): TSize; override;
    procedure DrawScaledClock(ACanvas: TcxCanvas; const ARect: TRect;
      ADateTime: TDateTime; ABackgroundColor: TColor; AScaleFactor: TdxScaleFactor); override;

    // ZoomButtons
    procedure DrawScaledZoomInButton(ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState; AScaleFactor: TdxScaleFactor); override;
    procedure DrawScaledZoomOutButton(ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState; AScaleFactor: TdxScaleFactor); override;
    function GetScaledZoomInButtonSize(AScaleFactor: TdxScaleFactor): TSize; override;
    function GetScaledZoomOutButtonSize(AScaleFactor: TdxScaleFactor): TSize; override;

    // Navigator
    procedure DrawNavigatorBorder(ACanvas: TcxCanvas; R: TRect; ASelected: Boolean); override;
    procedure DrawNavigatorScaledButton(ACanvas: TcxCanvas; R: TRect; AState: TcxButtonState;
      ABackgroundColor: TColor; AScaleFactor: TdxScaleFactor); override;
    procedure DrawNavigatorScaledButtonGlyph(ACanvas: TcxCanvas; AImageList: TCustomImageList; AImageIndex: TcxImageIndex;
      const AGlyphRect: TRect; AEnabled: Boolean; AUserGlyphs: Boolean; AScaleFactor: TdxScaleFactor); override;
    function NavigatorBorderOverlap: Boolean; override;
    function NavigatorButtonColorPalette(AEnabled: Boolean): IdxColorPalette; override;
    function NavigatorButtonPressedGlyphOffset: TPoint; override;
    function NavigatorButtonTextColor(AState: TcxButtonState): TColor; override;
    function NavigatorScaledButtonGlyphPadding(AScaleFactor: TdxScaleFactor): TRect; override;
    function NavigatorScaledButtonGlyphSize(AScaleFactor: TdxScaleFactor): TSize; override;
    function NavigatorInfoPanelColor: TColor; override;
    function NavigatorInfoPanelTextColor: TColor; override;

    // ProgressBar
    procedure DrawProgressBarBorder(ACanvas: TcxCanvas; ARect: TRect; AVertical: Boolean); override;
    procedure DrawProgressBarChunk(ACanvas: TcxCanvas; ARect: TRect; AVertical: Boolean); override;
    function ProgressBarBorderSize(AVertical: Boolean): TRect; override;
    function ProgressBarTextColorEx(AIsFilledArea: Boolean): TColor; override;

    // GroupBox
    procedure DrawGroupBoxBackground(ACanvas: TcxCanvas; ABounds: TRect; ARect: TRect); override;
    procedure DrawGroupBoxCaption(ACanvas: TcxCanvas; const ACaptionRect, ATextRect: TRect;
      ACaptionPosition: TcxGroupBoxCaptionPosition); override;
    procedure DrawGroupBoxContent(ACanvas: TcxCanvas; ABorderRect: TRect;
      ACaptionPosition: TcxGroupBoxCaptionPosition; ABorders: TcxBorders = cxBordersAll); override;
    procedure DrawGroupBoxScaledExpandButton(ACanvas: TcxCanvas; const R: TRect;
      AState: TcxButtonState; AExpanded: Boolean; AScaleFactor: TdxScaleFactor; ARotationAngle: TcxRotationAngle = ra0); override;
    procedure DrawGroupBoxScaledButton(ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState; AScaleFactor: TdxScaleFactor); override;
    procedure DrawGroupBoxScaledExpandGlyph(ACanvas: TcxCanvas; const R: TRect;
      AState: TcxButtonState; AExpanded: Boolean; AScaleFactor: TdxScaleFactor); override;
    function GroupBoxBorderSize(ACaption: Boolean;
      ACaptionPosition: TcxGroupBoxCaptionPosition): TRect; override;
    procedure DrawGroupBoxFrame(ACanvas: TcxCanvas; R: TRect; AEnabled: Boolean;
      ACaptionPosition: TcxGroupBoxCaptionPosition; ABorders: TcxBorders = cxBordersAll); override;
    procedure GroupBoxAdjustCaptionFont(ACaptionFont: TFont; ACaptionPosition: TcxGroupBoxCaptionPosition); override;
    function GroupBoxCaptionTailSize(ACaptionPosition: TcxGroupBoxCaptionPosition): Integer; override;
    function GroupBoxTextColor(AEnabled: Boolean; ACaptionPosition: TcxGroupBoxCaptionPosition): TColor; override;
    function IsGroupBoxCaptionTextDrawnOverBorder(ACaptionPosition: TcxGroupBoxCaptionPosition): Boolean; override;
    function IsGroupBoxTransparent(AIsCaption: Boolean; ACaptionPosition: TcxGroupBoxCaptionPosition): Boolean; override;

    // Header
    procedure DrawScaledHeader(ACanvas: TcxCanvas; const ABounds, ATextAreaBounds: TRect; ANeighbors: TcxNeighbors;
      ABorders: TcxBorders; AState: TcxButtonState; AAlignmentHorz: TAlignment; AAlignmentVert: TcxAlignmentVert;
      AMultiLine, AShowEndEllipsis: Boolean; const AText: string; AFont: TFont; ATextColor, ABkColor: TColor;
      AScaleFactor: TdxScaleFactor; AOnDrawBackground: TcxDrawBackgroundEvent = nil; AIsLast: Boolean = False; AIsGroup: Boolean = False); override;
    procedure DrawScaledHeaderEx(ACanvas: TcxCanvas; const ABounds, ATextAreaBounds: TRect; ANeighbors: TcxNeighbors;
      ABorders: TcxBorders; AState: TcxButtonState; AAlignmentHorz: TAlignment; AAlignmentVert: TcxAlignmentVert;
      AMultiLine, AShowEndEllipsis: Boolean; const AText: string; AFont: TFont; ATextColor, ABkColor: TColor;
      AScaleFactor: TdxScaleFactor; AOnDrawBackground: TcxDrawBackgroundEvent = nil); override;
    procedure DrawScaledHeaderControlSection(ACanvas: TcxCanvas; const ABounds, ATextAreaBounds: TRect; ANeighbors: TcxNeighbors;
      ABorders: TcxBorders; AState: TcxButtonState; AAlignmentHorz: TAlignment; AAlignmentVert: TcxAlignmentVert;
      AMultiLine, AShowEndEllipsis: Boolean; const AText: string; AFont: TFont; ATextColor, ABkColor: TColor; AScaleFactor: TdxScaleFactor); override;
    procedure DrawHeaderSeparator(ACanvas: TcxCanvas; const ABounds: TRect;
      AIndentSize: Integer; AColor: TColor; AViewParams: TcxViewParams); override;
    procedure DrawHeaderPressed(ACanvas: TcxCanvas; const ABounds: TRect); override;
    procedure DrawScaledSortingMark(ACanvas: TcxCanvas; const R: TRect; AAscendingSorting: Boolean; AScaleFactor: TdxScaleFactor); override;
    procedure DrawScaledSummarySortingMark(ACanvas: TcxCanvas; const R: TRect; AAscendingSorting: Boolean; AScaleFactor: TdxScaleFactor); override;
    procedure DrawScaledSummaryValueSortingMark(ACanvas: TcxCanvas; const R: TRect; AAscendingSorting: Boolean; AScaleFactor: TdxScaleFactor); override;
    function HeaderBorders(ANeighbors: TcxNeighbors): TcxBorders; override;
    function HeaderContentOffsets(AScaleFactor: TdxScaleFactor): TRect; override;
    function HeaderDrawCellsFirst: Boolean; override;
    function ScaledSummarySortingMarkSize(AScaleFactor: TdxScaleFactor): TPoint; override;
    function ScaledSummaryValueSortingMarkSize(AScaleFactor: TdxScaleFactor): TPoint; override;

    // OfficeNavigationBar
      //draw
    procedure OfficeNavigationBarDrawBackground(ACanvas: TcxCanvas; const ARect: TRect); override;
    procedure OfficeNavigationBarDrawScaledButtonItemBackground(ACanvas: TcxCanvas;
      const ARect: TRect; AState: TcxCalendarElementState; AScaleFactor: TdxScaleFactor); override;
    procedure OfficeNavigationBarDrawScaledItemBackground(ACanvas: TcxCanvas;
      const ARect: TRect; AState: TcxCalendarElementState; AScaleFactor: TdxScaleFactor); override;
      //sizes
    function OfficeNavigationBarButtonItemTextColor(AState: TcxCalendarElementState): TColor; override;
    function OfficeNavigationBarItemTextColor(AState: TcxCalendarElementState): TColor; override;
    function OfficeNavigationBarScaledButtonItemContentOffsets(AScaleFactor: TdxScaleFactor): TRect; override;
    function OfficeNavigationBarScaledButtonItemFontSize(AScaleFactor: TdxScaleFactor): Integer; override;
    function OfficeNavigationBarScaledContentOffsets(AScaleFactor: TdxScaleFactor): TRect; override;
    function OfficeNavigationBarScaledItemContentOffsets(AScaleFactor: TdxScaleFactor): TRect; override;
    function OfficeNavigationBarScaledItemFontSize(AScaleFactor: TdxScaleFactor): Integer; override;

    // PDFViewer
    function PDFViewerNavigationPaneButtonColorPalette(AState: TcxButtonState): IdxColorPalette; override;
    function PDFViewerNavigationPaneButtonContentOffsets(AScaleFactor: TdxScaleFactor): TRect; override;
    function PDFViewerNavigationPaneButtonOverlay(AScaleFactor: TdxScaleFactor): TPoint; override;
    function PDFViewerNavigationPaneButtonSize(AScaleFactor: TdxScaleFactor): TSize; override;
    function PDFViewerNavigationPaneContentOffsets(AScaleFactor: TdxScaleFactor): TRect; override;
    function PDFViewerNavigationPanePageCaptionContentOffsets(AScaleFactor: TdxScaleFactor): TRect; override;
    function PDFViewerNavigationPanePageCaptionTextColor: TColor; override;
    function PDFViewerNavigationPanePageContentOffsets(AScaleFactor: TdxScaleFactor): TRect; override;
    function PDFViewerNavigationPanePageToolbarContentOffsets(AScaleFactor: TdxScaleFactor): TRect; override;
    function PDFViewerSelectionColor: TColor; override;
    procedure PDFViewerDrawNavigationPaneBackground(ACanvas: TcxCanvas; const ARect: TRect; AScaleFactor: TdxScaleFactor); override;
    procedure PDFViewerDrawNavigationPaneButton(ACanvas: TcxCanvas; const ARect: TRect;
      AState: TcxButtonState; AScaleFactor: TdxScaleFactor; AMinimized, ASelected, AIsFirst: Boolean); override;
    procedure PDFViewerDrawNavigationPanePageBackground(ACanvas: TcxCanvas; const ARect: TRect); override;
    procedure PDFViewerDrawNavigationPanePageButton(ACanvas: TcxCanvas; const ARect: TRect;
      AState: TcxButtonState; AScaleFactor: TdxScaleFactor); override;
    procedure PDFViewerDrawNavigationPanePageCaptionBackground(ACanvas: TcxCanvas; const ARect: TRect); override;
    procedure PDFViewerDrawNavigationPanePageToolbarBackground(ACanvas: TcxCanvas; const ARect: TRect); override;
    procedure PDFViewerDrawFindPanelBackground(ACanvas: TcxCanvas; const R: TRect; ABorders: TcxBorders); override;
    procedure PDFViewerDrawPageThumbnailPreviewBackground(ACanvas: TcxCanvas; const ARect: TRect); override;

    // SpreadSheet
    procedure DrawSpreadSheetScaledGroupExpandButton(ACanvas: TcxCanvas;
      const R: TRect; AState: TcxButtonState; AScaleFactor: TdxScaleFactor); override;
    procedure DrawSpreadSheetScaledGroupExpandButtonGlyph(ACanvas: TcxCanvas; const R: TRect;
      AState: TcxButtonState; AExpanded: Boolean; AScaleFactor: TdxScaleFactor; ADefaultGlyphs: TCustomImageList = nil); override;
    procedure DrawSpreadSheetScaledHeader(ACanvas: TcxCanvas; const ABounds, ATextAreaBounds: TRect;
      ANeighbors: TcxNeighbors; ABorders: TcxBorders; AState: TcxButtonState; AAlignmentHorz: TAlignment;
      AAlignmentVert: TcxAlignmentVert; AMultiLine, AShowEndEllipsis: Boolean; const AText: string; AFont: TFont;
      ATextColor, ABkColor: TColor; AScaleFactor: TdxScaleFactor; AOnDrawBackground: TcxDrawBackgroundEvent = nil;
      AIsLast: Boolean = False; AIsGroup: Boolean = False); override;
    function SpreadSheetContentColor: TColor; override;
    function SpreadSheetContentTextColor: TColor; override;
    function SpreadSheetFrozenPaneSeparatorColor: TColor; override;
    function SpreadSheetScaledGroupExpandButtonContentOffsets(AScaleFactor: TdxScaleFactor): TRect; override;
    function SpreadSheetGroupExpandButtonTextColor(AState: TcxButtonState): TColor; override;
    function SpreadSheetGroupLineColor: TColor; override;
    function SpreadSheetSelectionColor: TColor; override;

    // SpreadSheetFormulaBar
    procedure DrawSpreadSheetFormulaBarScaledExpandButton(ACanvas: TcxCanvas;
      const R: TRect; AState: TcxButtonState; AExpanded: Boolean; AScaleFactor: TdxScaleFactor); override;
    procedure DrawSpreadSheetFormulaBarScaledSeparator(
      ACanvas: TcxCanvas; const R: TRect; AScaleFactor: TdxScaleFactor); override;

    // Grid
    procedure DrawFilterRowSeparator(ACanvas: TcxCanvas; const ARect: TRect; ABackgroundColor: TColor); override;
    procedure DrawGroupByBox(ACanvas: TcxCanvas; const ARect: TRect;
      ATransparent: Boolean; ABackgroundColor: TColor; const ABackgroundBitmap: TBitmap); override;
    function GridBordersOverlapSize: Integer; override;
    function GridDrawHeaderCellsFirst: Boolean; override;
    function GridGroupRowStyleOffice11ContentColor(AHasData: Boolean): TColor; override;
    function GridGroupRowStyleOffice11SeparatorColor: TColor; override;
    function GridGroupRowStyleOffice11TextColor: TColor; override;
    function PivotGridHeadersAreaColor: TColor; override;
    function PivotGridHeadersAreaTextColor: TColor; override;

    // Grid like common
    function GridLikeControlContentColor: TColor; override;
    function GridLikeControlContentEvenColor: TColor; override;
    function GridLikeControlContentOddColor: TColor; override;
    function GridLikeControlContentTextColor: TColor; override;
    function GridLikeControlBackgroundColor: TColor; override;

    // Layout View
    procedure LayoutViewDrawRecordCaption(ACanvas: TcxCanvas; const ABounds, ATextRect: TRect;
      APosition: TcxGroupBoxCaptionPosition; AState: TcxButtonState; AColor: TColor = clDefault;
      const ABitmap: TBitmap = nil); override;
    procedure LayoutViewDrawRecordContent(ACanvas: TcxCanvas; const ABounds: TRect;
      ACaptionPosition: TcxGroupBoxCaptionPosition; AState: TcxButtonState; ABorders: TcxBorders = cxBordersAll); override;
    procedure LayoutViewDrawScaledRecordExpandButton(ACanvas: TcxCanvas; const ABounds: TRect;
      AState: TcxButtonState; AExpanded: Boolean; AScaleFactor: TdxScaleFactor); override;
    procedure LayoutViewDrawItem(ACanvas: TcxCanvas; const ABounds: TRect; AState: TcxButtonState; ABorders: TcxBorders = []); override;
    function LayoutViewGetPadding(AElement: TcxLayoutElement): TRect; override;
    function LayoutViewGetSpacing(AElement: TcxLayoutElement): TRect; override;
    function LayoutViewRecordCaptionTailSize(ACaptionPosition: TcxGroupBoxCaptionPosition): Integer; override;
    function LayoutViewRecordCaptionTextBold: Boolean; override;

    //WinExplorer View
    procedure WinExplorerViewDrawGroup(ACanvas: TcxCanvas; const ABounds: TRect; AState: TcxButtonState;
      AColor: TColor = clDefault; const ABitmap: TBitmap = nil); override;
    procedure WinExplorerViewDrawGroupCaptionLine(ACanvas: TcxCanvas; const ABounds: TRect); override;
    procedure WinExplorerViewDrawRecord(ACanvas: TcxCanvas; const ABounds: TRect; AState: TcxButtonState;
      AColor: TColor = clDefault; const ABitmap: TBitmap = nil); override;
    procedure WinExplorerViewDrawScaledRecordExpandButton(ACanvas: TcxCanvas; const ABounds: TRect;
      AState: TcxButtonState; AExpanded: Boolean; AScaleFactor: TdxScaleFactor); override;
    function WinExplorerViewGroupCaptionLineHeight: Integer; override;
    function WinExplorerViewGroupTextBold: Boolean; override;
    function WinExplorerViewGroupTextColor(AState: TcxButtonState): TColor; override;
    function WinExplorerViewRecordTextColor(AState: TcxButtonState): TColor; override;

    // Footer
    procedure DrawFooterBorderEx(ACanvas: TcxCanvas; const R: TRect; ABorders: TcxBorders); override;
    procedure DrawFooterCellBorder(ACanvas: TcxCanvas; const R: TRect); override;
    procedure DrawFooterCellContent(ACanvas: TcxCanvas; const ABounds: TRect; AAlignmentHorz: TAlignment;
      AAlignmentVert: TcxAlignmentVert; AMultiLine: Boolean; const AText: string; AFont: TFont; ATextColor, ABkColor: TColor;
      AOnDrawBackground: TcxDrawBackgroundEvent = nil); override;
    procedure DrawFooterContent(ACanvas: TcxCanvas; const ARect: TRect; const AViewParams: TcxViewParams); override;
    function FooterCellBorderSize: Integer; override;
    function FooterDrawCellsFirst: Boolean; override;
    function FooterSeparatorColor: TColor; override;

    // Filter
    function ScaledFilterActivateButtonSize(AScaleFactor: TdxScaleFactor): TPoint; override;
    function ScaledFilterCloseButtonSize(AScaleFactor: TdxScaleFactor): TPoint; override;
    function ScaledFilterSmartTagSize(AScaleFactor: TdxScaleFactor): TSize; override;
    procedure DrawScaledFilterCloseButton(ACanvas: TcxCanvas; R: TRect;
      AState: TcxButtonState; AScaleFactor: TdxScaleFactor); override;
    procedure DrawScaledFilterDropDownButton(ACanvas: TcxCanvas; R: TRect;
      AState: TcxButtonState; AIsFilterActive: Boolean; AScaleFactor: TdxScaleFactor); override;
    procedure DrawFilterPanel(ACanvas: TcxCanvas; const ARect: TRect;
      ATransparent: Boolean; ABackgroundColor: TColor; const ABackgroundBitmap: TGraphic); override;
    procedure DrawScaledFilterSmartTag(ACanvas: TcxCanvas; R: TRect;
      AState: TcxFilterSmartTagState; AIsFilterActive: Boolean; AScaleFactor: TdxScaleFactor); override;
    function FilterControlMenuGetColorPalette: IdxColorPalette; override;

    // GaugeControl
    function GaugeControlBackgroundColor: TColor; override;
    procedure DrawGaugeControlBackground(ACanvas: TcxCanvas; const ARect: TRect; ATransparent: Boolean; ABackgroundColor: TColor); override;

    // Map
    function MapControlBackgroundColor: TColor; override;
    function MapControlPanelBackColor: TdxAlphaColor; override;
    function MapControlPanelHotTrackedTextColor: TdxAlphaColor; override;
    function MapControlPanelPressedTextColor: TdxAlphaColor; override;
    function MapControlPanelTextColor: TdxAlphaColor; override;
    function MapControlGetMapPushpinSize(AScaleFactor: TdxScaleFactor): TSize; override;
    function MapControlGetMapPushpinTextOrigin(AScaleFactor: TdxScaleFactor): TPoint; override;
    function MapControlMapCustomElementSelectionOffset(AScaleFactor: TdxScaleFactor): TRect; override;
    function MapControlMapCustomElementTextColor: TdxAlphaColor; override;
    function MapControlMapCustomElementTextGlowColor: TdxAlphaColor; override;
    function MapControlMapPushpinTextColor: TdxAlphaColor; override;
    function MapControlMapPushpinTextGlowColor: TdxAlphaColor; override;
    function MapControlSelectedRegionBackgroundColor: TdxAlphaColor; override;
    function MapControlSelectedRegionBorderColor: TdxAlphaColor; override;
    function MapControlShapeColor: TdxAlphaColor; override;
    function MapControlShapeBorderColor: TdxAlphaColor; override;
    function MapControlShapeBorderWidth(AScaleFactor: TdxScaleFactor): Integer; override;
    function MapControlShapeHighlightedColor: TdxAlphaColor; override;
    function MapControlShapeBorderHighlightedColor: TdxAlphaColor; override;
    function MapControlShapeBorderHighlightedWidth(AScaleFactor: TdxScaleFactor): Integer; override;
    function MapControlShapeSelectedColor: TdxAlphaColor; override;
    function MapControlShapeBorderSelectedColor: TdxAlphaColor; override;
    function MapControlShapeBorderSelectedWidth(AScaleFactor: TdxScaleFactor): Integer; override;
    procedure DrawMapCustomElementBackground(ACanvas: TcxCanvas; const ARect: TRect; AState: TdxMapControlElementState); override;
    procedure DrawMapPushpin(ACanvas: TcxCanvas; const ARect: TRect; AState: TdxMapControlElementState; AScaleFactor: TdxScaleFactor); override;

    // Panel
    procedure DrawPanelBorders(ACanvas: TcxCanvas; const ABorderRect: TRect); override;
    procedure DrawPanelContent(ACanvas: TcxCanvas; const ARect: TRect; ADrawBorders: Boolean); override;
    function PanelBorderSize: TRect; override;
    function PanelTextColor: TColor; override;

    // TrackBar
    procedure DrawTrackBarScaledTrack(ACanvas: TcxCanvas; const ARect, ASelection: TRect;
      AShowSelection, AEnabled, AHorizontal: Boolean; ATrackColor: TColor; AScaleFactor: TdxScaleFactor); override;
    procedure DrawTrackBarScaledThumb(ACanvas: TcxCanvas; const ARect: TRect; AState: TcxButtonState; AHorizontal: Boolean;
      ATicks: TcxTrackBarTicksAlign; AThumbColor: TColor; AScaleFactor: TdxScaleFactor); override;
    function TrackBarScaledThumbSize(AHorizontal: Boolean; AScaleFactor: TdxScaleFactor): TSize; override;
    function TrackBarScaledTrackSize(AScaleFactor: TdxScaleFactor): Integer; override;
    function TrackBarTicksColor(AText: Boolean): TColor; override;

    // RangeControl
    procedure DrawRangeControlScaledLeftThumb(ACanvas: TcxCanvas; const ARect: TRect;
      AColor: TColor; ABorderColor: TdxAlphaColor; AScaleFactor: TdxScaleFactor); override;
    procedure DrawRangeControlScaledRightThumb(ACanvas: TcxCanvas; const ARect: TRect;
      AColor: TColor; ABorderColor: TdxAlphaColor; AScaleFactor: TdxScaleFactor); override;
    procedure DrawRangeControlScaledRulerHeader(ACanvas: TcxCanvas; const ARect: TRect; AIsHot: Boolean;
      AColor: TdxAlphaColor; ABorderColor: TdxAlphaColor; AScaleFactor: TdxScaleFactor); override;
    procedure DrawRangeControlScaledSizingGlyph(ACanvas: TcxCanvas; const ARect: TRect;
      ABorderColor: TdxAlphaColor; AScaleFactor: TdxScaleFactor); override;
    function GetRangeControlBackColor: TColor; override;
    function GetRangeControlBorderColor: TColor; override;
    function GetRangeControlDefaultElementColor: TColor; override;
    function GetRangeControlElementForeColor: TColor; override;
    function GetRangeControlElementsBorderColor: TdxAlphaColor; override;
    function GetRangeControlLabelColor: TColor; override;
    function GetRangeControlOutOfRangeColor: TdxAlphaColor; override;
    function GetRangeControlRangePreviewColor: TColor; override;
    function GetRangeControlRulerColor: TdxAlphaColor; override;
    function GetRangeControlScaledScrollAreaHeight(AScaleFactor: TdxScaleFactor): Integer; override;
    function GetRangeControlScaledSizingGlyphSize(AScaleFactor: TdxScaleFactor): TSize; override;
    function GetRangeControlScrollAreaColor: TColor; override;
    function GetRangeControlSelectedRegionBackgroundColor: TdxAlphaColor; override;
    function GetRangeControlSelectedRegionBorderColor: TdxAlphaColor; override;
    function GetRangeControlScaledThumbSize(AScaleFactor: TdxScaleFactor): TSize; override;
    function GetRangeControlViewPortPreviewColor: TColor; override;

    // RangeTrackBar
    function GetRangeTrackBarLeftThumbSkinElement(AHorizontal: Boolean; ATicks: TcxTrackBarTicksAlign): TdxSkinElement;
    function GetRangeTrackBarRightThumbSkinElement(AHorizontal: Boolean; ATicks: TcxTrackBarTicksAlign): TdxSkinElement;
    function RangeTrackBarScaledLeftThumbSize(AHorizontal: Boolean; ATicks: TcxTrackBarTicksAlign; AScaleFactor: TdxScaleFactor): TSize; override;
    function RangeTrackBarScaledRightThumbSize(AHorizontal: Boolean; ATicks: TcxTrackBarTicksAlign; AScaleFactor: TdxScaleFactor): TSize; override;
    procedure DrawRangeTrackBarScaledThumbSkinElement(ACanvas: TcxCanvas; ASkinElement: TdxSkinElement; const ARect: TRect;
      AState: TcxButtonState; AHorizontal: Boolean; ATicks: TcxTrackBarTicksAlign; AScaleFactor: TdxScaleFactor);
    procedure DrawRangeTrackBarScaledLeftThumb(ACanvas: TcxCanvas; const ARect: TRect; AState: TcxButtonState;
      AHorizontal: Boolean; ATicks: TcxTrackBarTicksAlign; AThumbColor: TColor; AScaleFactor: TdxScaleFactor); override;
    procedure DrawRangeTrackBarScaledRightThumb(ACanvas: TcxCanvas; const ARect: TRect; AState: TcxButtonState;
      AHorizontal: Boolean; ATicks: TcxTrackBarTicksAlign; AThumbColor: TColor; AScaleFactor: TdxScaleFactor); override;
    // Splitter
    procedure DrawScaledSplitter(ACanvas: TcxCanvas; const ARect: TRect; AHighlighted, AClicked, AHorizontal: Boolean; AScaleFactor: TdxScaleFactor; AHasCloseMark: Boolean = False; AArrowDirection: TcxArrowDirection = adLeft); override;
    function GetScaledSplitterSize(AHorizontal: Boolean; AScaleFactor: TdxScaleFactor): TSize; override;

    // Hints
    function GetHintBorderColor: TColor; override;
    procedure DrawHintBackground(ACanvas: TcxCanvas; const ARect: TRect; AColor: TColor = clDefault); override;

    // ScreenTips
    function ScreenTipGetColorPalette: IdxColorPalette; override;
    function ScreenTipGetDescriptionTextColor: TColor; override;
    function ScreenTipGetTitleTextColor: TColor; override;
    function ScreenTipGetFooterLineSize: Integer; override;
    procedure ScreenTipDrawBackground(ACanvas: TcxCanvas; ARect: TRect); override;
    procedure ScreenTipDrawFooterLine(ACanvas: TcxCanvas; const ARect: TRect); override;

    // Indicator
    procedure DrawScaledIndicatorImage(ACanvas: TcxCanvas; const R: TRect; AKind: TcxIndicatorKind; AScaleFactor: TdxScaleFactor); override;
    procedure DrawScaledIndicatorItem(ACanvas: TcxCanvas; const R, AImageAreaBounds: TRect; AKind: TcxIndicatorKind;
      AColor: TColor; AScaleFactor: TdxScaleFactor; AOnDrawBackground: TcxDrawBackgroundEvent = nil;
      ANeighbors: TcxNeighbors = [nTop, nBottom]); override;
    procedure DrawScaledIndicatorItemEx(ACanvas: TcxCanvas; const R: TRect; AKind: TcxIndicatorKind;
      AColor: TColor; AScaleFactor: TdxScaleFactor; AOnDrawBackground: TcxDrawBackgroundEvent = nil); override;
    procedure DrawScaledIndicatorCustomizationMark(ACanvas: TcxCanvas; const R: TRect; AColor: TColor; AScaleFactor: TdxScaleFactor); override;
    function IndicatorDrawItemsFirst: Boolean; override;

    // ms outlook
    procedure DrawScaledMonthHeader(ACanvas: TcxCanvas; const ABounds: TRect; const AText: string;
      ANeighbors: TcxNeighbors; const AViewParams: TcxViewParams; AArrows: TcxArrowDirections; ASideWidth: Integer;
      AScaleFactor: TdxScaleFactor; AOnDrawBackground: TcxDrawBackgroundEvent = nil); override;

    // Scheduler
    procedure CalculateSchedulerNavigationButtonRects(AIsNextButton: Boolean; ACollapsed: Boolean;
      APrevButtonTextSize: TSize; ANextButtonTextSize: TSize; var ABounds: TRect;
      out ATextRect: TRect; out AArrowRect: TRect; AScaleFactor: TdxScaleFactor; const AIsVertical: Boolean = True); override;
    procedure DrawSchedulerDayHeader(ACanvas: TcxCanvas; const ABounds, ATextAreaBounds: TRect;
      ANeighbors: TcxNeighbors; ABorders: TcxBorders; AState: TcxButtonState;
      AAlignmentHorz: TAlignment; AAlignmentVert: TcxAlignmentVert; AMultiLine, AShowEndEllipsis: Boolean;
      const AText: string; AFont: TFont; ATextColor, ABkColor: TColor;
      AScaleFactor: TdxScaleFactor; AOnDrawBackground: TcxDrawBackgroundEvent = nil;
      AIsLast: Boolean = False; AIsGroup: Boolean = False); override;
    procedure DrawSchedulerEventProgress(ACanvas: TcxCanvas; const ABounds, AProgress: TRect;
      AViewParams: TcxViewParams; ATransparent: Boolean); override;
    procedure DrawSchedulerGroup(ACanvas: TcxCanvas; const R: TRect; AColor: TColor = clDefault); override;
    procedure DrawSchedulerScaledGroupSeparator(ACanvas: TcxCanvas; const ABounds: TRect;
      ABackgroundColor: TColor; AScaleFactor: TdxScaleFactor; AOnDrawBackground: TcxDrawBackgroundEvent); override;
    procedure DrawSchedulerMilestone(ACanvas: TcxCanvas; const R: TRect); override;
    procedure DrawSchedulerScaledNavigatorButton(ACanvas: TcxCanvas; R: TRect;
      AState: TcxButtonState; AScaleFactor: TdxScaleFactor); override;
    function SchedulerEventProgressOffsets: TRect; override;
    function SchedulerNavigationButtonTextColor(AIsNextButton: Boolean;
      AState: TcxButtonState; ADefaultColor: TColor = clDefault): TColor; override;
    procedure SchedulerNavigationButtonSizes(AIsNextButton: Boolean; var ABorders: TRect; var AArrowSize: TSize;
      var AHasTextArea: Boolean; AScaleFactor: TdxScaleFactor; const AIsVertical: Boolean = True); override;

    // Layout Control
    function LayoutControlEmptyAreaColor: TColor; override;
    function LayoutControlGetColorPaletteForGroupButton(AState: TcxButtonState): IdxColorPalette; override;
    function LayoutControlGetColorPaletteForItemCaption: IdxColorPalette; override;
    function LayoutControlGetColorPaletteForTabbedGroupCaption(AIsActive: Boolean): IdxColorPalette; override;
    procedure DrawLayoutControlBackground(ACanvas: TcxCanvas; const R: TRect); override;

    // ScrollBox
    procedure DrawScrollBoxBackground(ACanvas: TcxCanvas; const R: TRect; AColor: TColor); override;

    // Popup
    procedure DrawEditPopupWindowBorder(ACanvas: TcxCanvas; var R: TRect;
      ABorderStyle: TcxEditPopupBorderStyle; AClientEdge: Boolean); override;
    function GetEditPopupWindowBorderWidth(AStyle: TcxEditPopupBorderStyle): Integer; override;
    function GetEditPopupWindowClientEdgeWidth(AStyle: TcxEditPopupBorderStyle): Integer; override;

    // Window
    function GetWindowContentTextColor: TColor; override;
    procedure DrawWindowContent(ACanvas: TcxCanvas; const ARect: TRect); override;

    // Printing System
    function PrintPreviewBackgroundTextColor: TColor; override;
    function PrintPreviewPageBordersScaledWidth(AScaleFactor: TdxScaleFactor): TRect; override;
    procedure DrawPrintPreviewScaledBackground(ACanvas: TcxCanvas; const R: TRect; AScaleFactor: TdxScaleFactor); override;
    procedure DrawPrintPreviewPageScaledBackground(ACanvas: TcxCanvas;
      const ABorderRect, AContentRect: TRect; ASelected, ADrawContent: Boolean; AScaleFactor: TdxScaleFactor); override;

    // DateNavigator
    procedure DrawDateNavigatorCellSelection(ACanvas: TcxCanvas; const R: TRect; AColor: TColor); override;
    procedure DrawDateNavigatorDateHeader(ACanvas: TcxCanvas; var R: TRect); override;
    procedure DrawDateNavigatorTodayCellSelection(ACanvas: TcxCanvas; const R: TRect); override;

    // CalcEdit
    function CalcEditButtonTextColor(AButtonKind: TcxCalcButtonKind): TColor; override;

    // Customization Form
    function GetCustomizationFormListBackgroundColor: TColor; override;

    // BreadcrumbEdit
    function BreadcrumbEditBackgroundColor(AState: TdxBreadcrumbEditState): TColor; override;
    function BreadcrumbEditBordersColor(AState: TdxBreadcrumbEditState): TColor; virtual;
    function BreadcrumbEditButtonColorPalette(AState: TdxBreadcrumbEditButtonState): IdxColorPalette; override;
    function BreadcrumbEditIsFadingSupports: Boolean; override;
    function BreadcrumbEditNodeTextColor(AState: TdxBreadcrumbEditButtonState): TColor; override;
    function BreadcrumbEditScaledButtonAreaSeparatorSize(AScaleFactor: TdxScaleFactor): Integer; override;
    function BreadcrumbEditScaledButtonContentOffsets(AIsFirst, AIsLast: Boolean; AScaleFactor: TdxScaleFactor): TRect; override;
    function BreadcrumbEditScaledDropDownButtonWidth(AScaleFactor: TdxScaleFactor): Integer; override;
    function BreadcrumbEditScaledNodeTextOffsets(AScaleFactor: TdxScaleFactor): TRect; override;
    function BreadcrumbEditScaledProgressChunkOverlaySize(AScaleFactor: TdxScaleFactor): TSize; override;
    function BreadcrumbEditScaledProgressChunkPadding(AScaleFactor: TdxScaleFactor): TRect; override;
    procedure DrawBreadcrumbEditBorders(ACanvas: TcxCanvas; const ARect: TRect;
      ABorders: TcxBorders; AState: TdxBreadcrumbEditState); override;
    procedure DrawBreadcrumbEditScaledButton(ACanvas: TcxCanvas; const ARect: TRect;
      AState: TdxBreadcrumbEditButtonState; AIsFirst, AIsLast: Boolean; AScaleFactor: TdxScaleFactor); override;
    procedure DrawBreadcrumbEditScaledButtonCore(ACanvas: TcxCanvas; ARect: TRect; AButtonElement: TdxSkinElement;
      AState: TdxBreadcrumbEditButtonState; AIsFirst, AIsLast: Boolean; AScaleFactor: TdxScaleFactor);
    procedure DrawBreadcrumbEditScaledButtonAreaSeparator(ACanvas: TcxCanvas;
      const ARect: TRect; AState: TdxBreadcrumbEditState; AScaleFactor: TdxScaleFactor); override;
    procedure DrawBreadcrumbEditScaledDropDownButton(ACanvas: TcxCanvas;
      const ARect: TRect; AState: TdxBreadcrumbEditButtonState; AIsInEditor: Boolean; AScaleFactor: TdxScaleFactor); override;
    procedure DrawBreadcrumbEditScaledDropDownButtonGlyph(ACanvas: TcxCanvas;
      const ARect: TRect; AState: TdxBreadcrumbEditButtonState; AIsInEditor: Boolean; AScaleFactor: TdxScaleFactor); override;
    procedure DrawBreadcrumbEditScaledNode(ACanvas: TcxCanvas;
      const R: TRect; AState: TdxBreadcrumbEditButtonState; AHasDelimiter: Boolean; AScaleFactor: TdxScaleFactor); override;
    procedure DrawBreadcrumbEditScaledNodeDelimiter(ACanvas: TcxCanvas;
      const R: TRect; AState: TdxBreadcrumbEditButtonState; AScaleFactor: TdxScaleFactor); override;
    procedure DrawBreadcrumbEditScaledNodeMoreButtonGlyph(ACanvas: TcxCanvas;
      const R: TRect; AState: TdxBreadcrumbEditButtonState; AScaleFactor: TdxScaleFactor); override;
    procedure DrawBreadcrumbEditScaledNodeDelimiterGlyph(ACanvas: TcxCanvas;
      const R: TRect; AState: TdxBreadcrumbEditButtonState; AScaleFactor: TdxScaleFactor); override;
    procedure DrawBreadcrumbEditScaledProgressChunk(ACanvas: TcxCanvas; const R: TRect; AScaleFactor: TdxScaleFactor); override;
    procedure DrawBreadcrumbEditScaledProgressChunkOverlay(ACanvas: TcxCanvas; const R: TRect; AScaleFactor: TdxScaleFactor); override;

    // DropDownList
    function DropDownListBoxBordersSize: Integer; override;
    function DropDownListBoxItemTextColor(ASelected: Boolean): TColor; override;
    function DropDownListBoxScaledItemImageOffsets(AScaleFactor: TdxScaleFactor): TRect; override;
    function DropDownListBoxScaledItemTextOffsets(AScaleFactor: TdxScaleFactor): TRect; override;
    function DropDownListBoxScaledSeparatorSize(AScaleFactor: TdxScaleFactor): Integer; override;
    procedure DrawDropDownListBoxBackground(ACanvas: TcxCanvas; const ARect: TRect; AHasBorders: Boolean); override;
    procedure DrawDropDownListBoxScaledGutterBackground(ACanvas: TcxCanvas; const ARect: TRect; AScaleFactor: TdxScaleFactor); override;
    procedure DrawDropDownListBoxScaledSelection(ACanvas: TcxCanvas; const ARect, AGutterRect: TRect; AScaleFactor: TdxScaleFactor); override;
    procedure DrawDropDownListBoxScaledSeparator(ACanvas: TcxCanvas; const ARect, AGutterRect: TRect; AScaleFactor: TdxScaleFactor); override;

    // AlertWindow
    function AlertWindowScaledButtonContentOffsets(AKind: TdxAlertWindowButtonKind; AScaleFactor: TdxScaleFactor): TRect; override;
    function AlertWindowButtonElement(AKind: TdxAlertWindowButtonKind): TdxSkinElement;
    function AlertWindowScaledContentOffsets(AScaleFactor: TdxScaleFactor): TRect; override;
    function AlertWindowCornerRadius: Integer; override;
    function AlertWindowNavigationPanelTextColor: TColor; override;
    function AlertWindowTextColor: TColor; override;
    procedure DrawAlertWindowBackground(ACanvas: TcxCanvas; const ABounds: TRect; AScaleFactor: TdxScaleFactor = nil); override;
    procedure DrawAlertWindowScaledButton(ACanvas: TcxCanvas; const ABounds: TRect;
      AState: TcxButtonState; AKind: TdxAlertWindowButtonKind; AScaleFactor: TdxScaleFactor; ADown: Boolean); override;
    procedure DrawAlertWindowNavigationPanel(ACanvas: TcxCanvas; const ABounds: TRect); override;

    // Bevel
    function GetBevelMinimalShapeSize(AShape: TdxBevelShape): TSize; override;
    procedure GetBevelShapeColors(out AColor1, AColor2: TColor); override;

    // Gallery
    procedure DrawGalleryBackground(ACanvas: TcxCanvas; const ABounds: TRect); override;
    procedure DrawGalleryGroupHeader(ACanvas: TcxCanvas; const ABounds: TRect); override;
    procedure DrawGalleryItemImageFrame(ACanvas: TcxCanvas; const R: TRect); override;
    procedure DrawGalleryItemSelection(ACanvas: TcxCanvas; const R: TRect; AViewState: TdxGalleryItemViewState); override;
    function DrawGalleryItemSelectionFirst: Boolean; override;
    function GetGalleryGroupTextColor: TColor; override;
    function GetGalleryItemCaptionTextColor(const AState: TdxGalleryItemViewState): TColor; override;
    function GetGalleryItemColorPalette(const AState: TdxGalleryItemViewState): IdxColorPalette; override;
    function GetGalleryItemDescriptionTextColor(const AState: TdxGalleryItemViewState): TColor; override;
    function GetGalleryScaledGroupHeaderContentOffsets(AScaleFactor: TdxScaleFactor): TRect; override;

    // ColorGallery
    function GetColorGalleryGlyphFrameColor: TColor; override;

    // BackButton
    function GetScaledBackButtonSize(AScaleFactor: TdxScaleFactor): TSize; override;
    procedure DrawScaledBackButton(ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState; AScaleFactor: TdxScaleFactor); override;
    // Calendar
    procedure DrawCalendarDateCellSelection(ACanvas: TcxCanvas; const ARect: TRect; AStates: TcxCalendarElementStates); override;
    // ModernCalendar
    procedure DrawModernCalendarDateCellSelection(ACanvas: TcxCanvas; const ARect: TRect;
      AStates: TcxCalendarElementStates); override;
    procedure DrawModernCalendarDateHeaderSelection(ACanvas: TcxCanvas; const ARect: TRect;
      AStates: TcxCalendarElementStates); override;
    procedure DrawModernCalendarHeaderSelection(ACanvas: TcxCanvas; const ARect: TRect;
      AStates: TcxCalendarElementStates); override;
    function GetModernCalendarCellTextColor(AStates: TcxCalendarElementStates): TColor; override;
    function GetModernCalendarDateHeaderTextColor(AStates: TcxCalendarElementStates): TColor; override;
    function GetModernCalendarHeaderTextColor(AStates: TcxCalendarElementStates): TColor; override;
    function GetModernCalendarHeaderTextOffsets: TRect; override;
    procedure DrawScaledModernCalendarArrow(ACanvas: TcxCanvas; const ARect: TRect;
      ADirection: TcxArrowDirection; AState: TcxCalendarElementState; AScaleFactor: TdxScaleFactor); override;

    //RatingControl
    function GetRatingControlIndicatorColorPalette(AState: TdxRatingControlIndicatorState): IdxColorPalette; override;
    function GetRatingControlScaledIndicatorSize(AScaleFactor: TdxScaleFactor): TSize; override;
    procedure DrawRatingControlScaledIndicator(ACanvas: TcxCanvas; const ABounds: TRect;
      AState: TdxRatingControlIndicatorState; AScaleFactor: TdxScaleFactor); override;

    // WheelPicker
    procedure DrawWheelPickerItem(ACanvas: TcxCanvas; const ARect: TRect; AState: TcxButtonState); override;
    function GetWheelPickerColorPalette(AState: TcxButtonState): IdxColorPalette; override;

    // RichEditControl
    function RichEditControlHeaderFooterLineColor: TColor; override;
    function RichEditControlHeaderFooterMarkBackColor: TColor; override;
    function RichEditControlHeaderFooterMarkTextColor: TColor; override;
    function RichEditRulerDefaultTabColor: TColor; override;
    function RichEditRulerTextColor: TColor; override;

    // TokenEdit
    procedure DrawScaledTokenBackground(ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState; AScaleFactor: TdxScaleFactor); override;
    function GetTokenColorPalette(AState: TcxButtonState): IdxColorPalette; override;
    function GetTokenTextColor(AState: TcxButtonState): TColor; override;

    property SkinDetails: TdxSkinDetails read GetSkinDetails;
    property SkinInfo: TdxSkinLookAndFeelPainterInfo read GetSkinInfo;
  end;

implementation

uses
  Math, dxOffice11, dxGDIPlusClasses, dxDPIAwareUtils;

const
  BreadcrumbButtonStateToElementState: array[TdxBreadcrumbEditButtonState] of TdxSkinElementState = (
    esNormal, esFocused, esHot, esPressed, esDisabled
  );
  ButtonStateToSkinState: array[TcxButtonState] of TdxSkinElementState = (
    esActive, esNormal, esHot, esPressed, esDisabled
  );
  RatingControlIndicatorStateToSkinState: array[TdxRatingControlIndicatorState] of TdxSkinElementState = (
    esNormal, esActive, esHot
  );

procedure dxSkinElementMakeDisable(ABitmap: TcxBitmap32);
var
  AColor: TRGBQuad;
  AColors: TRGBColors;
  I, AGray: Integer;
begin
  ABitmap.GetBitmapColors(AColors);
  try
    for I := 0 to Length(AColors) - 1 do
    begin
      AColor := AColors[I];
      AGray := (2 * AColor.rgbReserved + AColor.rgbBlue + AColor.rgbGreen + AColor.rgbRed) div 5;
      AColor.rgbBlue := AGray;
      AColor.rgbGreen := AGray;
      AColor.rgbRed := AGray;
      AColors[I] := AColor;
    end;
  finally
    ABitmap.SetBitmapColors(AColors);
  end;
end;

{ TdxSkinLookAndFeelPainter }

constructor TdxSkinLookAndFeelPainter.Create(const ASkinResName: string; ASkinResInstance: THandle);
begin
  inherited Create;
  FSkinResName := ASkinResName;
  FSkinResInstance := ASkinResInstance;
end;

destructor TdxSkinLookAndFeelPainter.Destroy;
begin
  FreeAndNil(FSkinInfo);
  inherited Destroy;
end;

function TdxSkinLookAndFeelPainter.GetPainterData(var AData): Boolean;
begin
  TObject(AData) := SkinInfo;
  Result := True;
end;

function TdxSkinLookAndFeelPainter.GetPainterDetails(var ADetails): Boolean;
begin
  Result := SkinDetails <> nil;
  if Result then
    TObject(ADetails) := SkinDetails;
end;

function TdxSkinLookAndFeelPainter.LookAndFeelName: string;
begin
  Result := SkinInfo.Skin.Name;
end;

function TdxSkinLookAndFeelPainter.LookAndFeelStyle: TcxLookAndFeelStyle;
begin
  Result := lfsSkin;
end;

function TdxSkinLookAndFeelPainter.CreateLookAndFeelPainterDetails: TObject;
var
  ASkinDetails: TdxSkinDetails;
  AStream: TStream;
begin
  Result := nil;
  if FSkinResName <> '' then
  begin
    AStream := TResourceStream.Create(FSkinResInstance, FSkinResName, sdxResourceType);
    try
      ASkinDetails := TdxSkinDetails.Create;
      ASkinDetails.LoadFromStream(AStream);
      Result := ASkinDetails;
    finally
      AStream.Free;
    end;
  end;
end;

function TdxSkinLookAndFeelPainter.GetLookAndFeelPainterDetails: TObject;
begin
  if (FSkinInfo <> nil) and (FSkinInfo.Skin <> nil) then
    Result := FSkinInfo.Skin.Details
  else
    Result := inherited GetLookAndFeelPainterDetails;
end;

function TdxSkinLookAndFeelPainter.DefaultChartHistogramPlotColor: TColor;
begin
  Result := DefaultContentColor;
end;

function TdxSkinLookAndFeelPainter.DefaultContentColor: TColor;
begin
  if IsColorPropertyAssigned(SkinInfo.ContentColor) then
    Result := SkinInfo.ContentColor.Value
  else
    Result := inherited DefaultContentColor;
end;

function TdxSkinLookAndFeelPainter.DefaultContentEvenColor: TColor;
begin
  if IsColorPropertyAssigned(SkinInfo.ContentEvenColor) then
    Result := SkinInfo.ContentEvenColor.Value
  else
    Result := inherited DefaultContentEvenColor
end;

function TdxSkinLookAndFeelPainter.DefaultContentOddColor: TColor;
begin
  if IsColorPropertyAssigned(SkinInfo.ContentOddColor) then
    Result := SkinInfo.ContentOddColor.Value
  else
    Result := inherited DefaultContentOddColor;
end;

function TdxSkinLookAndFeelPainter.DefaultContentTextColor: TColor;
begin
  if IsColorPropertyAssigned(SkinInfo.ContentTextColor) then
    Result := SkinInfo.ContentTextColor.Value
  else
    Result := inherited DefaultContentTextColor;
end;

function TdxSkinLookAndFeelPainter.DefaultControlColor: TColor;
begin
  Result := DefaultContentColor;
end;

function TdxSkinLookAndFeelPainter.DefaultControlTextColor: TColor;
begin
  Result := DefaultContentTextColor;
end;

function TdxSkinLookAndFeelPainter.DefaultEditorBackgroundColorEx(AKind: TcxEditStateColorKind): TColor;
begin
  if IsColorPropertyAssigned(SkinInfo.EditorBackgroundColors[AKind]) then
    Result := SkinInfo.EditorBackgroundColors[AKind].Value
  else
    if AKind in [esckReadOnly, esckInactive] then
      Result := DefaultEditorBackgroundColorEx(esckNormal)
    else
      Result := inherited DefaultEditorBackgroundColorEx(AKind);
end;

function TdxSkinLookAndFeelPainter.DefaultEditorTextColorEx(AKind: TcxEditStateColorKind): TColor;
begin
  if IsColorPropertyAssigned(SkinInfo.EditorTextColors[AKind]) then
    Result := SkinInfo.EditorTextColors[AKind].Value
  else
    if AKind in [esckReadOnly, esckInactive] then
      Result := DefaultEditorTextColorEx(esckNormal)
    else
      Result := inherited DefaultEditorTextColorEx(AKind);
end;

function TdxSkinLookAndFeelPainter.DefaultFilterBoxTextColor: TColor;
begin
  if SkinInfo.FilterPanel = nil then
    Result := inherited DefaultFilterBoxTextColor
  else
    Result := SkinInfo.FilterPanel.TextColor;
end;

function TdxSkinLookAndFeelPainter.DefaultFixedSeparatorColor: TColor;
begin
  if SkinInfo.GridFixedLine <> nil then
    Result := SkinInfo.GridFixedLine.Color
  else
    Result := inherited DefaultFixedSeparatorColor;
end;

function TdxSkinLookAndFeelPainter.DefaultFooterColor: TColor;
begin
  if SkinInfo.FooterPanel = nil then
    Result := inherited DefaultFooterColor
  else
    Result := SkinInfo.FooterPanel.Color
end;

function TdxSkinLookAndFeelPainter.DefaultFooterTextColor: TColor;
begin
  if SkinInfo.FooterPanel = nil then
    Result := inherited DefaultFooterTextColor
  else
    Result := SkinInfo.FooterPanel.TextColor;
end;

function TdxSkinLookAndFeelPainter.DefaultGridDetailsSiteColor: TColor;
begin
  if IsColorPropertyAssigned(SkinInfo.ContentColor) then
    Result := SkinInfo.ContentColor.Value
  else
    Result := inherited DefaultGridDetailsSiteColor
end;

function TdxSkinLookAndFeelPainter.DefaultGridLineColor: TColor;
begin
  if SkinInfo.GridLine <> nil then
    Result := SkinInfo.GridLine.Color
  else
    Result := inherited DefaultGridLineColor
end;

function TdxSkinLookAndFeelPainter.DefaultGroupColor: TColor;
begin
  if SkinInfo.GridGroupRow <> nil then
    Result := SkinInfo.GridGroupRow.Color
  else
    Result := inherited DefaultGroupColor;
end;

function TdxSkinLookAndFeelPainter.DefaultGroupContentOffsets: TRect;
begin
  if SkinInfo.GridGroupRow <> nil then
    Result := SkinInfo.GridGroupRow.ContentOffset.Rect
  else
    Result := inherited DefaultGroupContentOffsets;
end;

function TdxSkinLookAndFeelPainter.DefaultGroupByBoxTextColor: TColor;
begin
  if SkinInfo.GridGroupByBox <> nil then
    Result := SkinInfo.GridGroupByBox.TextColor
  else
    Result := inherited DefaultGroupByBoxTextColor;
end;

function TdxSkinLookAndFeelPainter.DefaultGroupTextColor: TColor;
begin
  if SkinInfo.GridGroupRow = nil then
    Result := inherited DefaultGroupTextColor
  else
    Result := SkinInfo.GridGroupRow.TextColor;
end;

function TdxSkinLookAndFeelPainter.DefaultHeaderBackgroundColor: TColor;
begin
  if SkinInfo.HeaderBackgroundColor = nil then
    Result := inherited DefaultHeaderBackgroundColor
  else
    Result := SkinInfo.HeaderBackgroundColor.Value;
end;

function TdxSkinLookAndFeelPainter.DefaultHeaderBackgroundTextColor: TColor;
begin
  if SkinInfo.HeaderBackgroundTextColor = nil then
    Result := inherited DefaultHeaderBackgroundTextColor
  else
    Result := SkinInfo.HeaderBackgroundTextColor.Value;
end;

function TdxSkinLookAndFeelPainter.DefaultHeaderColor: TColor;
begin
  if SkinInfo.Header = nil then
    Result := inherited DefaultHeaderColor
  else
    Result := SkinInfo.Header.Color;
end;

function TdxSkinLookAndFeelPainter.DefaultHeaderTextColor: TColor;
begin
  if SkinInfo.Header = nil then
    Result := inherited DefaultHeaderTextColor
  else
    Result := SkinInfo.Header.TextColor;
end;

function TdxSkinLookAndFeelPainter.DefaultHyperlinkTextColor: TColor;
begin
  if IsColorPropertyAssigned(SkinInfo.HyperLinkTextColor) then
    Result := SkinInfo.HyperLinkTextColor.Value
  else
    Result := inherited DefaultHyperlinkTextColor;
end;

function TdxSkinLookAndFeelPainter.DefaultInactiveColor: TColor;
begin
  if IsColorPropertyAssigned(SkinInfo.InactiveColor) then
    Result := SkinInfo.InactiveColor.Value
  else
    Result := inherited DefaultInactiveColor;
end;

function TdxSkinLookAndFeelPainter.DefaultInactiveTextColor: TColor;
begin
  if IsColorPropertyAssigned(SkinInfo.InactiveTextColor) then
    Result := SkinInfo.InactiveTextColor.Value
  else
    Result := inherited DefaultInactiveTextColor;
end;

function TdxSkinLookAndFeelPainter.DefaultPreviewTextColor: TColor;
begin
  Result := DefaultHyperlinkTextColor;
end;

function TdxSkinLookAndFeelPainter.LayoutControlEmptyAreaColor: TColor;
begin
  if SkinInfo.LayoutControlColor <> nil then
    Result := SkinInfo.LayoutControlColor.Value
  else
    Result := inherited LayoutControlEmptyAreaColor;
end;

function TdxSkinLookAndFeelPainter.LayoutControlGetColorPaletteForGroupButton(AState: TcxButtonState): IdxColorPalette;
begin
  Result := ButtonColorPalette(AState);
end;

function TdxSkinLookAndFeelPainter.LayoutControlGetColorPaletteForItemCaption: IdxColorPalette;
begin
  Result := GetSkinElementColorPalette(SkinInfo.FormContent, esNormal);
end;

function TdxSkinLookAndFeelPainter.LayoutControlGetColorPaletteForTabbedGroupCaption(AIsActive: Boolean): IdxColorPalette;
const
  Map: array[Boolean] of TdxSkinElementState = (esNormal, esActive);
begin
  Result := GetSkinElementColorPalette(SkinInfo.PageControlHeader, Map[AIsActive]);
end;

procedure TdxSkinLookAndFeelPainter.DrawLayoutControlBackground(ACanvas: TcxCanvas; const R: TRect);
begin
  ACanvas.FillRect(R, LayoutControlEmptyAreaColor);
end;

procedure TdxSkinLookAndFeelPainter.DrawScrollBoxBackground(ACanvas: TcxCanvas; const R: TRect; AColor: TColor);
begin
  ACanvas.FillRect(R, DefaultContentColor);
end;

procedure TdxSkinLookAndFeelPainter.DrawEditPopupWindowBorder(
  ACanvas: TcxCanvas; var R: TRect; ABorderStyle: TcxEditPopupBorderStyle; AClientEdge: Boolean);
begin
  ACanvas.FrameRect(R, GetContainerBorderColor(False));
  InflateRect(R, -1, -1);
end;

function TdxSkinLookAndFeelPainter.GetEditPopupWindowBorderWidth(AStyle: TcxEditPopupBorderStyle): Integer;
begin
  Result := 1;
end;

function TdxSkinLookAndFeelPainter.GetEditPopupWindowClientEdgeWidth(AStyle: TcxEditPopupBorderStyle): Integer;
begin
  Result := 2;
end;

function TdxSkinLookAndFeelPainter.GetGalleryScaledGroupHeaderContentOffsets(AScaleFactor: TdxScaleFactor): TRect;
begin
  if SkinInfo.GalleryGroup <> nil then
    Result := AScaleFactor.Apply(SkinInfo.GalleryGroup.ContentOffset.Rect)
  else
    Result := inherited GetGalleryScaledGroupHeaderContentOffsets(AScaleFactor);
end;

function TdxSkinLookAndFeelPainter.GetGalleryGroupTextColor: TColor;
begin
  if SkinInfo.GalleryGroup <> nil then
    Result := SkinInfo.GalleryGroup.TextColor
  else
    Result := clDefault;

  if not cxColorIsValid(Result) then
  begin
    if SkinInfo.GalleryItem <> nil then
      Result := SkinInfo.GalleryItem.TextColor
    else
      Result := inherited GetGalleryGroupTextColor;
  end;
end;

function TdxSkinLookAndFeelPainter.GetGalleryItemCaptionTextColor(const AState: TdxGalleryItemViewState): TColor;
begin
  if SkinInfo.GalleryItem <> nil then
    Result := SkinInfo.GalleryItem.GetTextColor(GalleryStateToButtonState(AState))
  else
    Result := inherited GetGalleryItemCaptionTextColor(AState);
end;

function TdxSkinLookAndFeelPainter.GetGalleryItemColorPalette(const AState: TdxGalleryItemViewState): IdxColorPalette;
begin
  Result := GetSkinElementColorPalette(SkinInfo.GalleryItem, ButtonStateToSkinState[GalleryStateToButtonState(AState)]);
end;

function TdxSkinLookAndFeelPainter.GetGalleryItemDescriptionTextColor(const AState: TdxGalleryItemViewState): TColor;
begin
  if SkinInfo.GalleryItem <> nil then
    Result := SkinInfo.GalleryItem.GetTextColor(GalleryStateToButtonState(AState), sdxDescriptionTextColorPrefix)
  else
    Result := inherited GetGalleryItemDescriptionTextColor(AState);
end;

function TdxSkinLookAndFeelPainter.DefaultSelectionColor: TColor;
begin
  if SkinInfo.SelectionColor = nil then
    Result := inherited DefaultSelectionColor
  else
    Result := SkinInfo.SelectionColor.Value;
end;

function TdxSkinLookAndFeelPainter.DefaultSchedulerTimeRulerColor: TColor;
begin
  if IsSkinElementColorAssigned(SkinInfo.SchedulerTimeRuler) then
    Result := SkinInfo.SchedulerTimeRuler.Color
  else
    Result := inherited DefaultSchedulerTimeRulerColor;
end;

function TdxSkinLookAndFeelPainter.DefaultSchedulerTimeRulerTextColorClassic: TColor;
begin
  if IsSkinElementTextColorAssigned(SkinInfo.SchedulerTimeRuler) then
    Result := SkinInfo.SchedulerTimeRuler.TextColor
  else
    Result := inherited DefaultSchedulerTimeRulerTextColor;
end;

function TdxSkinLookAndFeelPainter.DefaultSelectionTextColor: TColor;
begin
  if SkinInfo.SelectionTextColor = nil then
    Result := inherited DefaultSelectionTextColor
  else
    Result := SkinInfo.SelectionTextColor.Value;
end;

function TdxSkinLookAndFeelPainter.DefaultSeparatorColor: TColor;
begin
  if SkinInfo.CardViewSeparator = nil then
    Result := inherited DefaultSeparatorColor
  else
    Result := SkinInfo.CardViewSeparator.Color;
end;

function TdxSkinLookAndFeelPainter.DefaultSchedulerBackgroundColor: TColor;
begin
  if SkinInfo.ContentColor = nil then
    Result := inherited DefaultSchedulerBackgroundColor
  else
    Result := SkinInfo.ContentColor.Value;
end;

function TdxSkinLookAndFeelPainter.DefaultSchedulerBorderColor: TColor;
begin
  if SkinInfo.ContainerBorderColor = nil then
    Result := inherited DefaultSchedulerBorderColor
  else
    Result := SkinInfo.ContainerBorderColor.Value;
end;

function TdxSkinLookAndFeelPainter.DefaultSchedulerContentColor(AResourceIndex: Integer): TColor;
var
  AColorProperty: TdxSkinColor;
begin
  AColorProperty := SkinInfo.SchedulerResourceColors[AResourceIndex mod dxSkinsSchedulerResourceColorsCount];
  if AColorProperty <> nil then
    Result := AColorProperty.Value
  else
    Result := inherited DefaultSchedulerContentColor(AResourceIndex);
end;

function TdxSkinLookAndFeelPainter.DefaultSchedulerControlColor: TColor;
begin
  if SkinInfo.ContentColor = nil then
    Result := inherited DefaultSchedulerControlColor
  else
    Result := SkinInfo.ContentColor.Value;
end;

function TdxSkinLookAndFeelPainter.DefaultSchedulerDateNavigatorArrowColor(AIsHighlight: Boolean): TColor;
begin
  Result := DefaultDateNavigatorHeaderTextColor(AIsHighlight);
end;

function TdxSkinLookAndFeelPainter.DefaultSchedulerDayHeaderColor: TColor;
begin
  Result := DefaultHeaderColor;
end;

function TdxSkinLookAndFeelPainter.DefaultSchedulerDayHeaderTextColor: TColor;
begin
  Result := DefaultHeaderTextColor;
end;

function TdxSkinLookAndFeelPainter.DefaultSchedulerEventColor(AIsAllDayEvent: Boolean): TColor;
begin
  if SkinInfo.SchedulerAppointment[True] <> nil then
    Result := clWindow
  else
    Result := inherited DefaultSchedulerEventColor(AIsAllDayEvent);
end;

function TdxSkinLookAndFeelPainter.DefaultSchedulerEventColorClassic(AIsAllDayEvent: Boolean): TColor;
begin
  if SkinInfo.SchedulerAppointment[True] <> nil then
    Result := clWindow
  else
    Result := inherited DefaultSchedulerEventColorClassic(AIsAllDayEvent);
end;

function TdxSkinLookAndFeelPainter.DefaultSchedulerHeaderContainerTextColor(ASelected: Boolean): TColor;
begin
  if SkinInfo.SchedulerAllDayArea[ASelected] <> nil then
    Result := SkinInfo.SchedulerAllDayArea[ASelected].TextColor
  else
    Result := inherited DefaultSchedulerHeaderContainerTextColor(ASelected);
end;

function TdxSkinLookAndFeelPainter.DefaultSchedulerNavigatorColor: TColor;
begin
  if SkinInfo.SchedulerNavigatorColor = nil then
    Result := inherited DefaultSchedulerNavigatorColor
  else
    Result := SkinInfo.SchedulerNavigatorColor.Value;
end;

function TdxSkinLookAndFeelPainter.DefaultSchedulerViewContentColor: TColor;
begin
  Result := DefaultSchedulerContentColor(0);
  if Result = clDefault then
    Result := inherited DefaultSchedulerViewContentColor;
end;

function TdxSkinLookAndFeelPainter.DefaultSchedulerViewContentColorClassic: TColor;
begin
  Result := DefaultSchedulerContentColor(0);
  if Result = clDefault then
    Result := inherited DefaultSchedulerViewContentColorClassic;
end;

function TdxSkinLookAndFeelPainter.DefaultSchedulerViewSelectedTextColor: TColor;
begin
  Result := DefaultSchedulerViewTextColor;
end;

function TdxSkinLookAndFeelPainter.DefaultSchedulerViewTextColor: TColor;
begin
  if SkinInfo.SchedulerAppointment[True] = nil then
    Result := inherited DefaultSchedulerViewTextColor
  else
    Result := SkinInfo.SchedulerAppointment[True].TextColor;
end;

function TdxSkinLookAndFeelPainter.DefaultSchedulerYearViewUnusedContentColor(AIsWorkTime: Boolean): TColor;
const
  IntensityMap: array[Boolean] of Integer = (80, 85);
begin
  Result := DefaultSchedulerContentColor(0);
  if cxColorIsValid(Result) then
    Result := dxGetDarkerColor(Result, IntensityMap[AIsWorkTime]);
end;

function TdxSkinLookAndFeelPainter.DefaultSizeGripAreaColor: TColor;
begin
  Result := clDefault;
  if SkinInfo.FormContent <> nil then
    Result := SkinInfo.FormContent.Color;
  if Result = clDefault then
    Result := inherited DefaultSizeGripAreaColor;
end;

function TdxSkinLookAndFeelPainter.DefaultTabTextColor: TColor;
begin
  if IsColorPropertyAssigned(SkinInfo.TabTextColor) then
    Result := SkinInfo.TabTextColor.Value
  else
    Result := inherited DefaultTabTextColor;
end;

function TdxSkinLookAndFeelPainter.DefaultTimeGridMajorScaleTextColor: TColor;
begin
  if IsSkinElementTextColorAssigned(SkinInfo.SchedulerTimeGridHeader[False]) then
    Result := SkinInfo.SchedulerTimeGridHeader[False].TextColor
  else
    Result := inherited DefaultTimeGridMajorScaleTextColor;
end;

function TdxSkinLookAndFeelPainter.DefaultTimeGridMinorScaleTextColor: TColor;
begin
  Result := DefaultTimeGridMajorScaleTextColor;
end;

function TdxSkinLookAndFeelPainter.DefaultGridOptionsTreeViewCategoryColor(ASelected: Boolean): TColor;
begin
  if ASelected then
    Result := DefaultSelectionColor
  else
    Result := DefaultGroupColor;
end;

function TdxSkinLookAndFeelPainter.DefaultGridOptionsTreeViewCategoryTextColor(ASelected: Boolean): TColor;
begin
  if ASelected then
    Result := DefaultSelectionTextColor
  else
    Result := DefaultGroupTextColor;
end;

function TdxSkinLookAndFeelPainter.DefaultTreeListGridLineColor: TColor;
begin
  if IsColorPropertyAssigned(SkinInfo.TreeListGridLineColor) then
    Result := SkinInfo.TreeListGridLineColor.Value
  else
    Result := inherited DefaultTreeListGridLineColor
end;

function TdxSkinLookAndFeelPainter.DefaultTreeListTreeLineColor: TColor;
begin
  if IsColorPropertyAssigned(SkinInfo.TreeListTreeLineColor) then
    Result := SkinInfo.TreeListTreeLineColor.Value
  else
    Result := inherited DefaultTreeListTreeLineColor
end;

function TdxSkinLookAndFeelPainter.DefaultLayoutViewCaptionTextColor(
  ACaptionPosition: TcxGroupBoxCaptionPosition; AState: TcxButtonState): TColor;
const
  PropertiesMap: array[TcxButtonState] of string = (
    '', '', sdxTextColorHot, sdxTextColorSelected, sdxTextColorInactive
  );
var
  AElement: TdxSkinElement;
  AProperty: TdxSkinProperty;
begin
  AElement := SkinInfo.LayoutViewRecordCaptionElements[cxgpTop];
  if AElement = nil then
    Result := clDefault
  else
    if AElement.GetPropertyByName(PropertiesMap[AState], AProperty) then
      Result := (AProperty as TdxSkinColor).Value
    else
      Result := AElement.TextColor;

  if Result = clDefault then
    Result := DefaultContentTextColor;
end;

function TdxSkinLookAndFeelPainter.LayoutViewRecordCaptionTextBold: Boolean;
begin
  Result := IsFontBold(SkinInfo.LayoutViewRecordCaptionElements[cxgpTop]);
end;

function TdxSkinLookAndFeelPainter.DefaultLayoutViewContentTextColor(AState: TcxButtonState): TColor;
begin
  Result := clDefault;
  if SkinInfo.LayoutViewItem <> nil then
    Result := SkinInfo.LayoutViewItem.GetTextColor(AState);
  if Result = clDefault then
    Result := DefaultContentTextColor;
end;

function TdxSkinLookAndFeelPainter.DefaultRecordSeparatorColor: TColor;
begin
  if SkinInfo.GridFixedLine = nil then
    Result := inherited DefaultRecordSeparatorColor
  else
    Result := SkinInfo.GridFixedLine.Color;
end;

function TdxSkinLookAndFeelPainter.DefaultDateNavigatorHeaderColor: TColor;
begin
  Result := DefaultHeaderColor;
end;

function TdxSkinLookAndFeelPainter.DefaultDateNavigatorHeaderTextColor(AIsHighlight: Boolean): TColor;
begin
  Result := DefaultHeaderTextColor;
end;

function TdxSkinLookAndFeelPainter.DefaultDateNavigatorTextColor: TColor;
begin
  if IsColorPropertyAssigned(SkinInfo.CalendarDayTextColor) then
    Result := SkinInfo.CalendarDayTextColor.Value
  else
    Result := inherited DefaultDateNavigatorTextColor;
end;

function TdxSkinLookAndFeelPainter.DefaultDateNavigatorHolydayTextColor: TColor;
begin
  if IsColorPropertyAssigned(SkinInfo.CalendarHolidayTextColor) then
    Result := SkinInfo.CalendarHolidayTextColor.Value
  else
    Result := inherited DefaultDateNavigatorHolydayTextColor;
end;

function TdxSkinLookAndFeelPainter.DefaultDateNavigatorInactiveTextColor: TColor;
begin
  if IsColorPropertyAssigned(SkinInfo.CalendarInactiveDayTextColor) then
    Result := SkinInfo.CalendarInactiveDayTextColor.Value
  else
    Result := inherited DefaultDateNavigatorInactiveTextColor;
end;

function TdxSkinLookAndFeelPainter.DefaultDateNavigatorSelectionColor: TColor;
begin
  if IsColorPropertyAssigned(SkinInfo.CalendarSelectedDayColor) then
    Result := SkinInfo.CalendarSelectedDayColor.Value
  else
    Result := inherited DefaultDateNavigatorSelectionColor;
end;

function TdxSkinLookAndFeelPainter.DefaultDateNavigatorSelectionTextColor: TColor;
begin
  if IsColorPropertyAssigned(SkinInfo.CalendarSelectedDayTextColor) then
    Result := SkinInfo.CalendarSelectedDayTextColor.Value
  else
    Result := inherited DefaultDateNavigatorSelectionTextColor;
end;

function TdxSkinLookAndFeelPainter.DefaultDateNavigatorSeparator1Color: TColor;
begin
  if IsColorPropertyAssigned(SkinInfo.CalendarSeparatorColor) then
    Result := SkinInfo.CalendarSeparatorColor.Value
  else
    Result := inherited DefaultDateNavigatorSeparator1Color;
end;

function TdxSkinLookAndFeelPainter.DefaultDateNavigatorSeparator2Color: TColor;
begin
  Result := clNone;
end;

function TdxSkinLookAndFeelPainter.DefaultDateNavigatorTodayFrameColor: TColor;
begin
  if IsColorPropertyAssigned(SkinInfo.CalendarTodayFrameColor) then
    Result := SkinInfo.CalendarTodayFrameColor.Value
  else
    Result := inherited DefaultDateNavigatorTodayFrameColor;
end;

function TdxSkinLookAndFeelPainter.DefaultDateNavigatorTodayTextColor(ASelected: Boolean): TColor;
begin
  if IsColorPropertyAssigned(SkinInfo.CalendarTodayTextColor) then
    Result := SkinInfo.CalendarTodayTextColor.Value
  else
    Result := inherited DefaultDateNavigatorTodayTextColor(ASelected);
end;

function TdxSkinLookAndFeelPainter.DefaultVGridBandLineColor: TColor;
begin
  if SkinInfo.VGridLine[True] = nil then
    Result := inherited DefaultVGridBandLineColor
  else
    Result := SkinInfo.VGridLine[True].Color;
end;

function TdxSkinLookAndFeelPainter.DefaultVGridCategoryColor: TColor;
begin
  if SkinInfo.VGridCategory <> nil then
    Result := SkinInfo.VGridCategory.Color
  else
    Result := inherited DefaultVGridCategoryColor;
end;

function TdxSkinLookAndFeelPainter.DefaultVGridCategoryTextColor: TColor;
begin
  if SkinInfo.VGridCategory <> nil then
    Result := SkinInfo.VGridCategory.TextColor
  else
    Result := inherited DefaultVGridCategoryTextColor;
end;

function TdxSkinLookAndFeelPainter.DefaultVGridContentColor: TColor;
begin
  if IsColorPropertyAssigned(SkinInfo.VGridContentColor) then
    Result := SkinInfo.VGridContentColor.Value
  else
    Result := inherited;
end;

function TdxSkinLookAndFeelPainter.DefaultVGridContentEvenColor: TColor;
begin
  if IsColorPropertyAssigned(SkinInfo.ContentEvenColor) then
    Result := SkinInfo.ContentEvenColor.Value
  else
    Result := DefaultVGridContentColor;
end;

function TdxSkinLookAndFeelPainter.DefaultVGridContentOddColor: TColor;
begin
  if IsColorPropertyAssigned(SkinInfo.ContentOddColor) then
    Result := SkinInfo.ContentOddColor.Value
  else
    Result := DefaultVGridContentColor;
end;

function TdxSkinLookAndFeelPainter.DefaultVGridHeaderColor: TColor;
begin
  if SkinInfo.VGridRowHeader <> nil then
    Result := SkinInfo.VGridRowHeader.Color
  else
    Result := inherited DefaultVGridHeaderColor;
end;

function TdxSkinLookAndFeelPainter.DefaultVGridHeaderTextColor: TColor;
begin
  if SkinInfo.VGridRowHeader <> nil then
    Result := SkinInfo.VGridRowHeader.TextColor
  else
    Result := inherited DefaultVGridHeaderTextColor;
end;

function TdxSkinLookAndFeelPainter.DefaultVGridLineColor: TColor;
begin
  if SkinInfo.VGridLine[False] <> nil then
    Result := SkinInfo.VGridLine[False].Color
  else
    Result := inherited DefaultVGridLineColor;
end;

function TdxSkinLookAndFeelPainter.SeparatorSize: Integer;
begin
  Result := LabelLineHeight;
end;

procedure TdxSkinLookAndFeelPainter.DrawScaledBackButton(
  ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState; AScaleFactor: TdxScaleFactor);
begin
  if not DrawSkinElement(SkinInfo.BackButton, ACanvas, R, AScaleFactor, ButtonStateToSkinState[AState]) then
    inherited DrawScaledBackButton(ACanvas, R, AState, AScaleFactor);
end;

procedure TdxSkinLookAndFeelPainter.DrawScaledModernCalendarArrow(ACanvas: TcxCanvas; const ARect: TRect;
  ADirection: TcxArrowDirection; AState: TcxCalendarElementState; AScaleFactor: TdxScaleFactor);
const
  ElementStateToSkinElementState: array [TcxCalendarElementState] of TdxSkinElementState =
    (esNormal, esHot, esPressed, esPressed, esNormal, esNormal, esDisabled);
  ImageIndex: array [adLeft..adRight] of Integer = (0, 1);
begin
  if not DrawSkinElement(SkinInfo.CalendarNavigationButton, ACanvas, ARect,
    AScaleFactor, ElementStateToSkinElementState[AState], ImageIndex[ADirection])
  then
    inherited DrawScaledModernCalendarArrow(ACanvas, ARect, ADirection, AState, AScaleFactor);
end;

procedure TdxSkinLookAndFeelPainter.DrawModernCalendarDateCellSelection(
  ACanvas: TcxCanvas; const ARect: TRect; AStates: TcxCalendarElementStates);

  function GetImageIndex(AStates: TcxCalendarElementStates): Integer;
  begin
    if cesMarked in AStates then
      Result := 1
    else
      Result := 0
  end;

begin
  if not DrawSkinElement(SkinInfo.HighlightedItem, ACanvas, ARect, esNormal, GetImageIndex(AStates)) then
    inherited
  else
    if cesFocused in AStates then
      ACanvas.DrawFocusRect(ARect);
end;

procedure TdxSkinLookAndFeelPainter.DrawModernCalendarDateHeaderSelection(
  ACanvas: TcxCanvas; const ARect: TRect; AStates: TcxCalendarElementStates);
begin
  DrawModernCalendarHeaderSelection(ACanvas, ARect, AStates);
end;

procedure TdxSkinLookAndFeelPainter.DrawModernCalendarHeaderSelection(ACanvas: TcxCanvas;
  const ARect: TRect; AStates: TcxCalendarElementStates);
var
  AIsHighlighted: Boolean;
begin
  AIsHighlighted := AStates * [cesHot, cesPressed] <> [];

  if AIsHighlighted and (SkinInfo.HighlightedItem <> nil) then
    DrawSkinElement(SkinInfo.HighlightedItem, ACanvas, ARect);
end;

function TdxSkinLookAndFeelPainter.GetModernCalendarCellTextColor(AStates: TcxCalendarElementStates): TColor;
begin
  if cesDisabled in AStates then
    Result := DefaultDateNavigatorInactiveTextColor
  else
    if (cesHot in AStates) or
      (AStates * [cesMarked, cesSelected] = [cesSelected]) then
      Result := DefaultDateNavigatorSelectionTextColor
    else
      Result := DefaultDateNavigatorTextColor;
end;

function TdxSkinLookAndFeelPainter.GetModernCalendarDateHeaderTextColor(AStates: TcxCalendarElementStates): TColor;
begin
  if cesHot in AStates then
    Result := DefaultDateNavigatorSelectionTextColor
  else
    Result := DefaultDateNavigatorTextColor;
end;

function TdxSkinLookAndFeelPainter.GetModernCalendarHeaderTextColor(AStates: TcxCalendarElementStates): TColor;
begin
  Result := GetModernCalendarDateHeaderTextColor(AStates);
end;

function TdxSkinLookAndFeelPainter.GetModernCalendarHeaderTextOffsets: TRect;
begin
  if SkinInfo.HighlightedItem <> nil then
    Result := cxRectTransform(SkinInfo.HighlightedItem.ContentOffset.Rect, cxRect(4, 2, 4, 2))
  else
    Result := inherited GetModernCalendarHeaderTextOffsets;
end;

procedure TdxSkinLookAndFeelPainter.DrawRatingControlScaledIndicator(ACanvas: TcxCanvas;
  const ABounds: TRect; AState: TdxRatingControlIndicatorState; AScaleFactor: TdxScaleFactor);
begin
  if not DrawSkinElement(SkinInfo.RatingIndicator, ACanvas, ABounds, AScaleFactor, RatingControlIndicatorStateToSkinState[AState]) then
    inherited DrawRatingControlScaledIndicator(ACanvas, ABounds, AState, AScaleFactor);
end;

procedure TdxSkinLookAndFeelPainter.DrawWheelPickerItem(ACanvas: TcxCanvas; const ARect: TRect; AState: TcxButtonState);

  function GetImageIndex: Integer;
  begin
    if AState = cxbsHot then
      Result := 1
    else
      Result := 0
  end;

begin
  if (AState in [cxbsHot, cxbsPressed]) and not DrawSkinElement(SkinInfo.HighlightedItem, ACanvas, ARect, esNormal, GetImageIndex) then
    inherited;
end;

function TdxSkinLookAndFeelPainter.GetWheelPickerColorPalette(AState: TcxButtonState): IdxColorPalette;
begin
  Result := GetSkinElementColorPalette(SkinInfo.HighlightedItem, ButtonStateToSkinState[AState]);
end;

function TdxSkinLookAndFeelPainter.RichEditControlHeaderFooterLineColor: TColor;
begin
  Result := DefaultControlColor;
end;

function TdxSkinLookAndFeelPainter.RichEditControlHeaderFooterMarkBackColor: TColor;
begin
  Result := DefaultControlColor;
end;

function TdxSkinLookAndFeelPainter.RichEditControlHeaderFooterMarkTextColor: TColor;
begin
  Result := DefaultControlTextColor;
end;

function TdxSkinLookAndFeelPainter.RichEditRulerDefaultTabColor: TColor;
begin
  if IsColorPropertyAssigned(SkinInfo.RichEditRulerDefaultTabColor) then
    Result := SkinInfo.RichEditRulerDefaultTabColor.Value
  else
    Result := inherited RichEditRulerDefaultTabColor;
end;

function TdxSkinLookAndFeelPainter.RichEditRulerTextColor: TColor;
begin
  if IsColorPropertyAssigned(SkinInfo.RichEditRulerTextColor) then
    Result := SkinInfo.RichEditRulerTextColor.Value
  else
    Result := inherited RichEditRulerTextColor;
end;

procedure TdxSkinLookAndFeelPainter.DrawScaledTokenBackground(
  ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState; AScaleFactor: TdxScaleFactor);
begin
  if SkinInfo.HighlightedItem <> nil then
  begin
    if AState in [cxbsHot, cxbsPressed] then
      SkinInfo.HighlightedItem.Draw(ACanvas.Handle, R, AScaleFactor, Ord(AState = cxbsPressed));
  end
  else
    inherited DrawScaledTokenBackground(ACanvas, R, AState, AScaleFactor);
end;

function TdxSkinLookAndFeelPainter.GetTokenColorPalette(AState: TcxButtonState): IdxColorPalette;
begin
  Result := GetSkinElementColorPalette(SkinInfo.HighlightedItem, ButtonStateToSkinState[AState]);
end;

function TdxSkinLookAndFeelPainter.GetTokenTextColor(AState: TcxButtonState): TColor;
begin
  Result := inherited GetTokenTextColor(AState);
  if (SkinInfo.HighlightedItem <> nil) and (AState in [cxbsHot, cxbsPressed]) then
    Result := cxGetActualColor(SkinInfo.HighlightedItem.TextColor, Result);
end;

function TdxSkinLookAndFeelPainter.GetRatingControlIndicatorColorPalette(AState: TdxRatingControlIndicatorState): IdxColorPalette;
begin
  Result := GetSkinElementColorPalette(SkinInfo.RatingIndicator, RatingControlIndicatorStateToSkinState[AState]);
end;

function TdxSkinLookAndFeelPainter.GetRatingControlScaledIndicatorSize(AScaleFactor: TdxScaleFactor): TSize;
begin
  if SkinInfo.RatingIndicator <> nil then
    Result := AScaleFactor.Apply(SkinInfo.RatingIndicator.Image.Size)
  else
    Result := inherited GetRatingControlScaledIndicatorSize(AScaleFactor);
end;

procedure TdxSkinLookAndFeelPainter.DrawBorder(ACanvas: TcxCanvas; R: TRect);
begin
  if SkinInfo.ContainerBorderColor <> nil then
    ACanvas.FrameRect(R, SkinInfo.ContainerBorderColor.Value)
  else
    inherited DrawBorder(ACanvas, R)
end;

procedure TdxSkinLookAndFeelPainter.DrawContainerBorder(ACanvas: TcxCanvas;
  const R: TRect; AStyle: TcxContainerBorderStyle; AWidth: Integer; AColor: TColor; ABorders: TcxBorders);
begin
  inherited DrawContainerBorder(ACanvas, R, cbsSingle, AWidth, AColor, ABorders);
end;

procedure TdxSkinLookAndFeelPainter.DoDrawSeparator(ACanvas: TcxCanvas; R: TRect; AIsVertical: Boolean);
begin
  DrawLabelLine(ACanvas, R, clDefault, clDefault, AIsVertical);
end;

function TdxSkinLookAndFeelPainter.ButtonBorderSize(AState: TcxButtonState = cxbsNormal): Integer;
begin
  if SkinInfo.ButtonElements <> nil then
    Result := 0
  else
    Result := inherited ButtonBorderSize(AState);
end;

function TdxSkinLookAndFeelPainter.ButtonColorPalette(AState: TcxButtonState): IdxColorPalette;
begin
  Result := GetSkinElementColorPalette(SkinInfo.ButtonElements, ButtonStateToSkinState[AState]);
end;

function TdxSkinLookAndFeelPainter.ButtonDescriptionTextColor(AState: TcxButtonState; ADefaultColor: TColor): TColor;
begin
  if SkinInfo.ButtonElements <> nil then
    Result := SkinInfo.ButtonElements.GetTextColor(AState, sdxDescriptionTextColorPrefix)
  else
    Result := clDefault;

  if not cxColorIsValid(Result) then
    Result := ADefaultColor;
end;

function TdxSkinLookAndFeelPainter.ButtonSymbolColor(AState: TcxButtonState; ADefaultColor: TColor = clDefault): TColor;
begin
  if SkinInfo.ButtonElements = nil then
    Result := inherited ButtonSymbolColor(AState, ADefaultColor)
  else
    if (AState = cxbsDisabled) and (SkinInfo.ButtonDisabled <> nil) then
      Result := SkinInfo.ButtonDisabled.Value
    else
      Result := SkinInfo.ButtonElements.GetTextColor(AState);
end;

procedure TdxSkinLookAndFeelPainter.DrawScaledButton(ACanvas: TcxCanvas; R: TRect; const ACaption: string;
  AState: TcxButtonState; AScaleFactor: TdxScaleFactor; ADrawBorder: Boolean = True; AColor: TColor = clDefault;
  ATextColor: TColor = clDefault; AWordWrap: Boolean = False; AIsToolButton: Boolean = False; APart: TcxButtonPart = cxbpButton);
var
  AElement: TdxSkinElement;
  AFlags: Integer;
begin
  case APart of
    cxbpDropDownLeftPart:
      AElement := SkinInfo.DropDownButtonLeft;
    cxbpDropDownRightPart:
      AElement := SkinInfo.DropDownButtonRight;
  else
    AElement := SkinInfo.ButtonElements;
  end;

  if AElement <> nil then
  begin
    if APart <> cxbpButton then
      DrawRightToLeftDependentSkinElement(AElement, ACanvas, R, AScaleFactor, ButtonStateToSkinState[AState])
    else
      AElement.Draw(ACanvas.Handle, R, AScaleFactor, 0, ButtonStateToSkinState[AState]);

    if ACaption <> '' then
    begin
      R := cxRectContent(R, AElement.ContentOffset.Rect);
      if Odd(cxRectHeight(R)) then
        Dec(R.Bottom);
      if cxRectHeight(R) < 18 then
        Dec(R.Top);
      if AState = cxbsPressed then
        OffsetRect(R, ScaledButtonTextShift(AScaleFactor), ScaledButtonTextShift(AScaleFactor));

      if ATextColor = clDefault then
        ATextColor := ButtonSymbolColor(AState);
      ACanvas.Font.Color := ATextColor;
      ACanvas.Brush.Style := bsClear;

      AFlags := cxAlignVCenter or cxShowPrefix or cxAlignHCenter;
      if AWordWrap then
        AFlags := AFlags or cxWordBreak
      else
        AFlags := AFlags or cxSingleLine;

      ACanvas.DrawText(ACaption, R, AFlags, AState <> cxbsDisabled);
      ACanvas.Brush.Style := bsSolid;
    end;
  end
  else
    inherited DrawScaledButton(ACanvas, R, ACaption, AState, AScaleFactor, ADrawBorder, AColor, ATextColor, AWordWrap);
end;

procedure TdxSkinLookAndFeelPainter.DrawScaledSearchEditButtonGlyph(
  ACanvas: TcxCanvas; R: TRect; AState: TcxButtonState; AScaleFactor: TdxScaleFactor);
begin
  if not DrawSkinElement(SkinInfo.EditButtonSearchGlyph, ACanvas, R, AScaleFactor, ButtonStateToSkinState[AState]) then
    inherited DrawScaledSearchEditButtonGlyph(ACanvas, R, AState, AScaleFactor);
end;

function TdxSkinLookAndFeelPainter.GetScaledDropDownButtonRightPartSize(AScaleFactor: TdxScaleFactor): Integer;
begin
  if (SkinInfo.DropDownButtonRight <> nil) and (SkinInfo.DropDownButtonRight.MinSize.Width > 0) then
    Result := AScaleFactor.Apply(SkinInfo.DropDownButtonRight.MinSize.Width)
  else
    Result := inherited GetScaledDropDownButtonRightPartSize(AScaleFactor);
end;

procedure TdxSkinLookAndFeelPainter.DrawScaledExpandButton(ACanvas: TcxCanvas; const R: TRect;
  AExpanded: Boolean; AScaleFactor: TdxScaleFactor; AColor: TColor = clDefault;
  AState: TcxExpandButtonState = cebsNormal);
const
  StateMap: array[TcxExpandButtonState] of TdxSkinElementState = (esNormal, esActive, esActiveDisabled);
begin
  if not DrawRightToLeftDependentSkinElement(SkinInfo.ExpandButton, ACanvas, R, AScaleFactor, StateMap[AState], Byte(AExpanded)) then
    inherited DrawScaledExpandButton(ACanvas, R, AExpanded, AScaleFactor, AColor);
end;

procedure TdxSkinLookAndFeelPainter.DrawScaledExpandButtonEx(ACanvas: TcxCanvas; const R:
  TRect; AState: TcxButtonState; AExpanded: Boolean; AScaleFactor: TdxScaleFactor; ARotationAngle: TcxRotationAngle = ra0);
begin
  DrawScaledExpandButton(ACanvas, R, AExpanded, AScaleFactor);
end;

procedure TdxSkinLookAndFeelPainter.DrawScaledSmallExpandButton(ACanvas: TcxCanvas;
  R: TRect; AExpanded: Boolean; ABorderColor: TColor; AScaleFactor: TdxScaleFactor; AColor: TColor = clDefault);
begin
  if not DrawRightToLeftDependentSkinElement(SkinInfo.ExpandButton, ACanvas, R, AScaleFactor, esNormal, Byte(AExpanded)) then
    inherited DrawScaledSmallExpandButton(ACanvas, R, AExpanded, ABorderColor, AScaleFactor, AColor);
end;

function TdxSkinLookAndFeelPainter.ScaledExpandButtonSize(AScaleFactor: TdxScaleFactor): Integer;
begin
  if SkinInfo.ExpandButton <> nil then
    Result := dxSkinGetElementSize(SkinInfo.ExpandButton, AScaleFactor).cy
  else
    Result := inherited ScaledExpandButtonSize(AScaleFactor);
end;

function TdxSkinLookAndFeelPainter.ScaledScrollBarMinimalThumbSize(AVertical: Boolean; AScaleFactor: TdxScaleFactor): Integer;
var
  AInfo: TdxSkinScrollInfo;
  ASize: TSize;
begin
  AInfo := SkinInfo.ScrollBar_Elements[not AVertical, sbpThumbnail];
  if (AInfo = nil) or (AInfo.Element = nil) then
    Result := inherited ScaledScrollBarMinimalThumbSize(AVertical, AScaleFactor)
  else
  begin
    ASize := GetSkinElementMinSize(AInfo.Element);
    Result := IfThen(AVertical, ASize.cy, ASize.cx);
    AScaleFactor.Apply(Result);
  end;
end;

procedure TdxSkinLookAndFeelPainter.DrawScaledScrollBarBackground(
  ACanvas: TcxCanvas; const R: TRect; AHorizontal: Boolean; AScaleFactor: TdxScaleFactor);
var
  AScrollInfoElement: TdxSkinScrollInfo;
begin
  AScrollInfoElement := SkinInfo.ScrollBar_Elements[AHorizontal, sbpPageUp];
  if (AScrollInfoElement = nil) or (AScrollInfoElement.Element = nil) then
    inherited DrawScaledScrollBarBackground(ACanvas, R, AHorizontal, AScaleFactor)
  else
  begin
    AScrollInfoElement.Element.UseCache := True;
    DrawRightToLeftDependentSkinElement(AScrollInfoElement.Element, ACanvas, R, AScaleFactor);
  end;
end;

procedure TdxSkinLookAndFeelPainter.DrawScaledScrollBarPart(ACanvas: TcxCanvas;
  AHorizontal: Boolean; R: TRect; APart: TcxScrollBarPart; AState: TcxButtonState; AScaleFactor: TdxScaleFactor);
var
  AScrollPartInfo: TdxSkinScrollInfo;
begin
  AScrollPartInfo := SkinInfo.ScrollBar_Elements[AHorizontal, APart];
  if (AScrollPartInfo = nil) or (AScrollPartInfo.Element = nil) then
    inherited DrawScaledScrollBarPart(ACanvas, AHorizontal, R, APart, AState, AScaleFactor)
  else
    if not ((APart in [sbpPageUp, sbpPageDown]) and (AState = cxbsNormal)) then
      DrawRightToLeftDependentSkinElement(AScrollPartInfo.Element, ACanvas, R, AScaleFactor,
        ButtonStateToSkinState[AState], AScrollPartInfo.ImageIndex);
end;

procedure TdxSkinLookAndFeelPainter.DrawScaledScrollBarSplitter(
  ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState; AScaleFactor: TdxScaleFactor);
const
  StateMap: array[Boolean] of TdxSkinElementState = (esNormal, esHot);
var
  AElement: TdxSkinElement;
begin
  AElement := SkinInfo.Splitter[False];
  if AElement <> nil then
    AElement.Glyph.Draw(ACanvas.Handle, R, AScaleFactor)
  else
    inherited DrawScaledScrollBarSplitter(ACanvas, R, AState, AScaleFactor);
end;

procedure TdxSkinLookAndFeelPainter.DrawLabelLine(ACanvas: TcxCanvas;
  const R: TRect; AOuterColor, AInnerColor: TColor; AIsVertical: Boolean);
begin
  if not DrawSkinElement(SkinInfo.LabelLine[AIsVertical], ACanvas, R) then
    inherited DrawLabelLine(ACanvas, R, AOuterColor, AInnerColor, AIsVertical)
end;

function TdxSkinLookAndFeelPainter.LabelLineHeight: Integer;
begin
  if SkinInfo.LabelLine[False] = nil then
    Result := inherited LabelLineHeight
  else
    Result := SkinInfo.LabelLine[False].MinSize.Height;
end;

function TdxSkinLookAndFeelPainter.ScaledSizeGripSize(AScaleFactor: TdxScaleFactor): TSize;
begin
  if SkinInfo.SizeGrip = nil then
    Result := inherited ScaledSizeGripSize(AScaleFactor)
  else
    Result := AScaleFactor.Apply(GetSkinElementMinSize(SkinInfo.SizeGrip));
end;

procedure TdxSkinLookAndFeelPainter.DoDrawSizeGrip(ACanvas: TcxCanvas; const ARect: TRect; AScaleFactor: TdxScaleFactor);
begin
  if not DrawSkinElement(SkinInfo.SizeGrip, ACanvas, ARect, AScaleFactor) then
    inherited;
end;

function TdxSkinLookAndFeelPainter.ScaledSliderButtonSize(ADirection: TcxArrowDirection; AScaleFactor: TdxScaleFactor): TSize;
begin
  if SkinInfo.SliderArrow[ADirection] = nil then
    Result := inherited ScaledSliderButtonSize(ADirection, AScaleFactor)
  else
    Result := AScaleFactor.Apply(SkinInfo.SliderArrow[ADirection].Size)
end;

procedure TdxSkinLookAndFeelPainter.DrawScaledSliderButton(ACanvas: TcxCanvas;
  const ARect: TRect; ADirection: TcxArrowDirection; AState: TcxButtonState; AScaleFactor: TdxScaleFactor);
begin
  if not DrawSkinElement(SkinInfo.SliderArrow[ADirection], ACanvas, ARect, AScaleFactor, ButtonStateToSkinState[AState]) then
    inherited DrawScaledSliderButton(ACanvas, ARect, ADirection, AState, AScaleFactor);
end;

function TdxSkinLookAndFeelPainter.DoGetSmallCloseButtonSize: TSize;
begin
  if SkinInfo.PageControlCloseButton <> nil then
    Result := SkinInfo.PageControlCloseButton.MinSize.Size
  else
    Result := inherited DoGetSmallCloseButtonSize;
end;

function TdxSkinLookAndFeelPainter.GetSmallButtonColorPalette(AState: TcxButtonState): IdxColorPalette;
begin
  Result := GetSkinElementColorPalette(SkinInfo.ButtonElements, ButtonStateToSkinState[AState]);
end;

procedure TdxSkinLookAndFeelPainter.DrawScaledSmallButton(
  ACanvas: TcxCanvas; R: TRect; AState: TcxButtonState; AScaleFactor: TdxScaleFactor);
begin
  if not DrawSkinElement(SkinInfo.PageControlHeaderButton, ACanvas, R, AScaleFactor, ButtonStateToSkinState[AState]) then
    inherited DrawScaledSmallButton(ACanvas, R, AState, AScaleFactor);
end;

procedure TdxSkinLookAndFeelPainter.DrawScaledSmallCloseButton(
  ACanvas: TcxCanvas; R: TRect; AState: TcxButtonState; AScaleFactor: TdxScaleFactor);
begin
  if not DrawSkinElement(SkinInfo.PageControlCloseButton, ACanvas, R, AScaleFactor, ButtonStateToSkinState[AState]) then
    inherited DrawScaledSmallCloseButton(ACanvas, R, AState, AScaleFactor);
end;

procedure TdxSkinLookAndFeelPainter.DrawScaledRadioButton(ACanvas: TcxCanvas; X, Y: Integer; AButtonState: TcxButtonState;
  AChecked, AFocused: Boolean; ABrushColor: TColor; AScaleFactor: TdxScaleFactor; AIsDesigning: Boolean = False);
var
  ADestRect: TRect;
begin
  if SkinInfo.RadioGroupButton <> nil then
  begin
    ADestRect := cxRectBounds(X, Y, ScaledRadioButtonSize(AScaleFactor));
    if ABrushColor <> clDefault then
      ACanvas.FillRect(ADestRect, ABrushColor);
    SkinInfo.RadioGroupButton.Draw(ACanvas.Handle, ADestRect, AScaleFactor, Byte(AChecked), ButtonStateToSkinState[AButtonState]);
  end
  else
    inherited DrawScaledRadioButton(ACanvas, X, Y, AButtonState, AChecked, AFocused, ABrushColor, AScaleFactor, AIsDesigning);
end;

function TdxSkinLookAndFeelPainter.ScaledRadioButtonSize(AScaleFactor: TdxScaleFactor): TSize;
begin
  with SkinInfo do
    if RadioGroupButton <> nil then
      Result := AScaleFactor.Apply(RadioGroupButton.Size)
    else
      Result := inherited ScaledRadioButtonSize(AScaleFactor);
end;

function TdxSkinLookAndFeelPainter.ScaledCheckButtonSize(AScaleFactor: TdxScaleFactor): TSize;
var
  ACheckboxElement: TdxSkinElement;
begin
  ACheckboxElement := SkinInfo.CheckboxElement;
  if ACheckboxElement <> nil then
    Result := AScaleFactor.Apply(ACheckboxElement.Size)
  else
    Result := inherited ScaledCheckButtonSize(AScaleFactor);
end;

function TdxSkinLookAndFeelPainter.CalcEditButtonTextColor(AButtonKind: TcxCalcButtonKind): TColor;
begin
  with SkinInfo do
    if IsColorPropertyAssigned(CalcEditButtonTextColors[AButtonKind]) then
      Result := CalcEditButtonTextColors[AButtonKind].Value
    else
      Result := inherited CalcEditButtonTextColor(AButtonKind);
end;

function TdxSkinLookAndFeelPainter.GetCustomizationFormListBackgroundColor: TColor;
begin
  with SkinInfo do
    if IsColorPropertyAssigned(ContentColor) then
      Result := ContentColor.Value
    else
      Result := inherited GetCustomizationFormListBackgroundColor;
end;

function TdxSkinLookAndFeelPainter.BreadcrumbEditBackgroundColor(AState: TdxBreadcrumbEditState): TColor;
begin
  if IsColorPropertyAssigned(SkinInfo.BreadcrumbEditBackgroundColors[AState]) then
    Result := SkinInfo.BreadcrumbEditBackgroundColors[AState].Value
  else
    if IsColorPropertyAssigned(SkinInfo.BreadcrumbEditBackgroundColors[dxbcsNormal]) then
      Result := SkinInfo.BreadcrumbEditBackgroundColors[dxbcsNormal].Value
    else
      Result := inherited BreadcrumbEditBackgroundColor(AState);
end;

function TdxSkinLookAndFeelPainter.BreadcrumbEditBordersColor(AState: TdxBreadcrumbEditState): TColor;
begin
  if IsColorPropertyAssigned(SkinInfo.BreadcrumbEditBordersColors[AState]) then
    Result := SkinInfo.BreadcrumbEditBordersColors[AState].Value
  else
    if IsColorPropertyAssigned(SkinInfo.BreadcrumbEditBordersColors[dxbcsNormal]) then
      Result := SkinInfo.BreadcrumbEditBordersColors[dxbcsNormal].Value
    else
      Result := clDefault;
end;

function TdxSkinLookAndFeelPainter.BreadcrumbEditScaledButtonAreaSeparatorSize(AScaleFactor: TdxScaleFactor): Integer;
begin
  if SkinInfo.BreadcrumbEditButtonsAreaSeparator <> nil then
    Result := AScaleFactor.Apply(SkinInfo.BreadcrumbEditButtonsAreaSeparator.MinSize.Width)
  else
    Result := inherited BreadcrumbEditScaledButtonAreaSeparatorSize(AScaleFactor);
end;

function TdxSkinLookAndFeelPainter.BreadcrumbEditButtonColorPalette(AState: TdxBreadcrumbEditButtonState): IdxColorPalette;
begin
  Result := GetSkinElementColorPalette(SkinInfo.BreadcrumbEditButton, BreadcrumbButtonStateToElementState[AState]);
end;

function TdxSkinLookAndFeelPainter.BreadcrumbEditScaledButtonContentOffsets(
  AIsFirst, AIsLast: Boolean; AScaleFactor: TdxScaleFactor): TRect;
begin
  if SkinInfo.BreadcrumbEditButton <> nil then
  begin
    Result := SkinInfo.BreadcrumbEditButton.ContentOffset.Rect;
    if SkinInfo.BreadcrumbEditButtonMergeBorders then
    begin
      if AIsFirst and (BreadcrumbEditScaledButtonAreaSeparatorSize(AScaleFactor) > 0) then
        Result.Left := 1;
      Result.Right := 1;
    end;
  end
  else
    Result := inherited BreadcrumbEditScaledButtonContentOffsets(AIsFirst, AIsLast, AScaleFactor);
end;

function TdxSkinLookAndFeelPainter.BreadcrumbEditScaledDropDownButtonWidth(AScaleFactor: TdxScaleFactor): Integer;
begin
  if SkinInfo.BreadcrumbEditDropDownButton <> nil then
    Result := AScaleFactor.Apply(SkinInfo.BreadcrumbEditDropDownButton.MinSize.Width)
  else
    Result := inherited BreadcrumbEditScaledDropDownButtonWidth(AScaleFactor);
end;

function TdxSkinLookAndFeelPainter.BreadcrumbEditIsFadingSupports: Boolean;
begin
  Result := True;
end;

function TdxSkinLookAndFeelPainter.BreadcrumbEditNodeTextColor(AState: TdxBreadcrumbEditButtonState): TColor;
const
  TextColorMap: array[TdxBreadcrumbEditButtonState] of string = ('',
    sdxTextColorSelected, sdxTextColorHot, sdxTextColorPressed, sdxTextColorDisabled
  );
var
  AProperty: TdxSkinProperty;
begin
  if SkinInfo.BreadcrumbEditNodeButton <> nil then
  begin
    Result := clDefault;
    if SkinInfo.BreadcrumbEditNodeButton.GetPropertyByName(TextColorMap[AState], AProperty) then
    begin
      if AProperty is TdxSkinColor then
        Result := TdxSkinColor(AProperty).Value;
    end;
    if Result = clDefault then
      Result := SkinInfo.BreadcrumbEditNodeButton.TextColor;
  end
  else
    Result := inherited BreadcrumbEditNodeTextColor(AState)
end;

function TdxSkinLookAndFeelPainter.BreadcrumbEditScaledNodeTextOffsets(AScaleFactor: TdxScaleFactor): TRect;
begin
  if SkinInfo.BreadcrumbEditNodeButton <> nil then
    Result := AScaleFactor.Apply(SkinInfo.BreadcrumbEditNodeButton.ContentOffset.Rect)
  else
    Result := inherited BreadcrumbEditScaledNodeTextOffsets(AScaleFactor);
end;

function TdxSkinLookAndFeelPainter.BreadcrumbEditScaledProgressChunkPadding(AScaleFactor: TdxScaleFactor): TRect;
begin
  if SkinInfo.BreadcrumbEditProgressChunkPadding <> nil then
    Result := AScaleFactor.Apply(SkinInfo.BreadcrumbEditProgressChunkPadding.Value.Rect)
  else
    Result := inherited BreadcrumbEditScaledProgressChunkPadding(AScaleFactor);
end;

function TdxSkinLookAndFeelPainter.BreadcrumbEditScaledProgressChunkOverlaySize(AScaleFactor: TdxScaleFactor): TSize;
begin
  if SkinInfo.BreadcrumbEditProgressChunkOverlay <> nil then
    Result := AScaleFactor.Apply(SkinInfo.BreadcrumbEditProgressChunkOverlay.MinSize.Size)
  else
    Result := inherited BreadcrumbEditScaledProgressChunkOverlaySize(AScaleFactor);
end;

procedure TdxSkinLookAndFeelPainter.DrawBreadcrumbEditBorders(
  ACanvas: TcxCanvas; const ARect: TRect; ABorders: TcxBorders; AState: TdxBreadcrumbEditState);
var
  AColor: TColor;
begin
  AColor := BreadcrumbEditBordersColor(AState);
  if cxColorIsValid(AColor) then
    ACanvas.FrameRect(ARect, AColor, 1, ABorders)
  else
    inherited DrawBreadcrumbEditBorders(ACanvas, ARect, ABorders, AState);
end;

procedure TdxSkinLookAndFeelPainter.DrawBreadcrumbEditScaledButton(ACanvas: TcxCanvas;
  const ARect: TRect; AState: TdxBreadcrumbEditButtonState; AIsFirst, AIsLast: Boolean; AScaleFactor: TdxScaleFactor);
begin
  if SkinInfo.BreadcrumbEditButton <> nil then
    DrawBreadcrumbEditScaledButtonCore(ACanvas, ARect, SkinInfo.BreadcrumbEditButton, AState, AIsFirst, AIsLast, AScaleFactor)
  else
    inherited DrawBreadcrumbEditScaledButton(ACanvas, ARect, AState, AIsFirst, AIsLast, AScaleFactor);
end;

procedure TdxSkinLookAndFeelPainter.DrawBreadcrumbEditScaledButtonCore(ACanvas: TcxCanvas;
  ARect: TRect; AButtonElement: TdxSkinElement; AState: TdxBreadcrumbEditButtonState;
  AIsFirst, AIsLast: Boolean; AScaleFactor: TdxScaleFactor);

  function ShouldMergeBorders: Boolean;
  var
    AProperty: TdxSkinProperty;
  begin
    if AButtonElement.GetPropertyByName(sdxEditorButtonMergeBorders, AProperty) then
      Result := (AProperty as TdxSkinBooleanProperty).Value
    else
      Result := False;
  end;

  procedure DoDraw;
  begin
    AButtonElement.UseCache := True;
    AButtonElement.Draw(ACanvas.Handle, ARect, AScaleFactor, 0, BreadcrumbButtonStateToElementState[AState]);
  end;

begin
  if ShouldMergeBorders then
  begin
    ACanvas.SaveClipRegion;
    try
      ACanvas.IntersectClipRect(ARect);

      ARect := cxRectInflate(ARect, -1);
      ARect := cxRectInflate(ARect, AButtonElement.ContentOffset.Rect);
      if not AIsFirst or (BreadcrumbEditScaledButtonAreaSeparatorSize(AScaleFactor) = 0) then
        Inc(ARect.Left, AButtonElement.ContentOffset.Left - 1);

      DoDraw;
    finally
      ACanvas.RestoreClipRegion;
    end;
  end
  else
    DoDraw;
end;

procedure TdxSkinLookAndFeelPainter.DrawBreadcrumbEditScaledButtonAreaSeparator(
  ACanvas: TcxCanvas; const ARect: TRect; AState: TdxBreadcrumbEditState; AScaleFactor: TdxScaleFactor);
begin
  if not DrawSkinElement(SkinInfo.BreadcrumbEditButtonsAreaSeparator, ACanvas, ARect, AScaleFactor) then
    inherited DrawBreadcrumbEditScaledButtonAreaSeparator(ACanvas, ARect, AState, AScaleFactor);
end;

procedure TdxSkinLookAndFeelPainter.DrawBreadcrumbEditScaledDropDownButton(ACanvas: TcxCanvas;
  const ARect: TRect; AState: TdxBreadcrumbEditButtonState; AIsInEditor: Boolean; AScaleFactor: TdxScaleFactor);
begin
  if SkinInfo.BreadcrumbEditDropDownButton <> nil then
    DrawBreadcrumbEditScaledButtonCore(ACanvas, ARect, SkinInfo.BreadcrumbEditDropDownButton, AState, True, False, AScaleFactor)
  else
    inherited DrawBreadcrumbEditScaledDropDownButton(ACanvas, ARect, AState, AIsInEditor, AScaleFactor);
end;

procedure TdxSkinLookAndFeelPainter.DrawBreadcrumbEditScaledDropDownButtonGlyph(ACanvas: TcxCanvas;
  const ARect: TRect; AState: TdxBreadcrumbEditButtonState; AIsInEditor: Boolean; AScaleFactor: TdxScaleFactor);
begin
  if SkinInfo.BreadcrumbEditDropDownButton = nil then
    inherited DrawBreadcrumbEditScaledDropDownButtonGlyph(ACanvas, ARect, AState, AIsInEditor, AScaleFactor);
end;

procedure TdxSkinLookAndFeelPainter.DrawBreadcrumbEditScaledNode(ACanvas: TcxCanvas;
  const R: TRect; AState: TdxBreadcrumbEditButtonState; AHasDelimiter: Boolean; AScaleFactor: TdxScaleFactor);
var
  AElement: TdxSkinElement;
begin
  if AHasDelimiter then
    AElement := SkinInfo.BreadcrumbEditNodeSplitButtonLeft
  else
    AElement := SkinInfo.BreadcrumbEditNodeButton;

  if not DrawSkinElement(AElement, ACanvas, R, AScaleFactor, BreadcrumbButtonStateToElementState[AState]) then
    inherited DrawBreadcrumbEditScaledNode(ACanvas, R, AState, AHasDelimiter, AScaleFactor);
end;

procedure TdxSkinLookAndFeelPainter.DrawBreadcrumbEditScaledNodeDelimiter(
  ACanvas: TcxCanvas; const R: TRect; AState: TdxBreadcrumbEditButtonState; AScaleFactor: TdxScaleFactor);
begin
  if not DrawSkinElement(SkinInfo.BreadcrumbEditNodeSplitButtonRight, ACanvas, R, AScaleFactor, BreadcrumbButtonStateToElementState[AState]) then
    inherited DrawBreadcrumbEditScaledNodeDelimiter(ACanvas, R, AState, AScaleFactor);
end;

procedure TdxSkinLookAndFeelPainter.DrawBreadcrumbEditScaledNodeMoreButtonGlyph(
  ACanvas: TcxCanvas; const R: TRect; AState: TdxBreadcrumbEditButtonState; AScaleFactor: TdxScaleFactor);
begin
  DrawCollapseArrow(ACanvas, R, BreadcrumbEditNodeTextColor(AState), AScaleFactor.Apply(2));
end;

procedure TdxSkinLookAndFeelPainter.DrawBreadcrumbEditScaledNodeDelimiterGlyph(
  ACanvas: TcxCanvas; const R: TRect; AState: TdxBreadcrumbEditButtonState; AScaleFactor: TdxScaleFactor);
var
  ADirection: TcxArrowDirection;
begin
  if AState = dxbcbsPressed then
    ADirection := adDown
  else
    if ACanvas.UseRightToLeftAlignment then
      ADirection := adLeft
    else
      ADirection := adRight;

  DrawArrow(ACanvas, cxRectInflate(R, -1, -1), ADirection, BreadcrumbEditNodeTextColor(AState));
end;

procedure TdxSkinLookAndFeelPainter.DrawBreadcrumbEditScaledProgressChunk(ACanvas: TcxCanvas; const R: TRect; AScaleFactor: TdxScaleFactor);
begin
  if not DrawSkinElement(SkinInfo.BreadcrumbEditProgressChunk, ACanvas, R, AScaleFactor) then
    inherited DrawBreadcrumbEditScaledProgressChunk(ACanvas, R, AScaleFactor);
end;

procedure TdxSkinLookAndFeelPainter.DrawBreadcrumbEditScaledProgressChunkOverlay(ACanvas: TcxCanvas; const R: TRect; AScaleFactor: TdxScaleFactor);
begin
  if not DrawSkinElement(SkinInfo.BreadcrumbEditProgressChunkOverlay, ACanvas, R, AScaleFactor) then
    inherited DrawBreadcrumbEditScaledProgressChunkOverlay(ACanvas, R, AScaleFactor);
end;

function TdxSkinLookAndFeelPainter.DropDownListBoxBordersSize: Integer;
begin
  Result := 2;
end;

function TdxSkinLookAndFeelPainter.DropDownListBoxScaledItemImageOffsets(AScaleFactor: TdxScaleFactor): TRect;
begin
  Result := DropDownListBoxScaledItemTextOffsets(AScaleFactor);
  if SkinInfo.PopupMenuSideStrip <> nil then
  begin
    Result.Left := Max(Result.Left, SkinInfo.PopupMenuSideStrip.ContentOffset.Left);
    Result.Right := Max(Result.Right, SkinInfo.PopupMenuSideStrip.ContentOffset.Right);
  end;
end;

function TdxSkinLookAndFeelPainter.DropDownListBoxItemTextColor(ASelected: Boolean): TColor;
const
  StatesMap: array[Boolean] of TcxButtonState = (cxbsNormal, cxbsHot);
begin
  if SkinInfo.PopupMenu = nil then
    Result := inherited DropDownListBoxItemTextColor(ASelected)
  else
    Result := SkinInfo.PopupMenu.GetTextColor(StatesMap[ASelected]);
end;

function TdxSkinLookAndFeelPainter.DropDownListBoxScaledItemTextOffsets(AScaleFactor: TdxScaleFactor): TRect;
begin
  Result := inherited DropDownListBoxScaledItemTextOffsets(AScaleFactor);
  if SkinInfo.PopupMenuLinkSelected <> nil then
  begin
    Result.Top := Max(Result.Top, SkinInfo.PopupMenuLinkSelected.ContentOffset.Top);
    Result.Bottom := Max(Result.Bottom, SkinInfo.PopupMenuLinkSelected.ContentOffset.Bottom);
  end;
end;

function TdxSkinLookAndFeelPainter.DropDownListBoxScaledSeparatorSize(AScaleFactor: TdxScaleFactor): Integer;
begin
  if SkinInfo.PopupMenuSeparator = nil then
    Result := inherited DropDownListBoxScaledSeparatorSize(AScaleFactor)
  else
    Result := SkinInfo.PopupMenuSeparator.MinSize.Height;
end;

procedure TdxSkinLookAndFeelPainter.DrawDropDownListBoxBackground(
  ACanvas: TcxCanvas; const ARect: TRect; AHasBorders: Boolean);
begin
  if SkinInfo.PopupMenu = nil then
    inherited DrawDropDownListBoxBackground(ACanvas, ARect, AHasBorders)
  else
    if AHasBorders then
      SkinInfo.PopupMenu.Draw(ACanvas.Handle, ARect)
    else
    begin
      ACanvas.SaveClipRegion;
      try
        ACanvas.IntersectClipRect(ARect);
        SkinInfo.PopupMenu.Draw(ACanvas.Handle, cxRectInflate(ARect, GetSkinElementBordersWidth(SkinInfo.PopupMenu)));
      finally
        ACanvas.RestoreClipRegion;
      end;
    end;
end;

procedure TdxSkinLookAndFeelPainter.DrawDropDownListBoxScaledGutterBackground(
  ACanvas: TcxCanvas; const ARect: TRect; AScaleFactor: TdxScaleFactor);
begin
  if not DrawRightToLeftDependentSkinElement(SkinInfo.PopupMenuSideStrip, ACanvas, ARect, AScaleFactor) then
    inherited DrawDropDownListBoxScaledGutterBackground(ACanvas, ARect, AScaleFactor);
end;

procedure TdxSkinLookAndFeelPainter.DrawDropDownListBoxScaledSelection(
  ACanvas: TcxCanvas; const ARect, AGutterRect: TRect; AScaleFactor: TdxScaleFactor);
begin
  if not DrawSkinElement(SkinInfo.PopupMenuLinkSelected, ACanvas, ARect, AScaleFactor) then
    inherited DrawDropDownListBoxScaledSelection(ACanvas, ARect, AGutterRect, AScaleFactor)
end;

procedure TdxSkinLookAndFeelPainter.DrawDropDownListBoxScaledSeparator(
  ACanvas: TcxCanvas; const ARect, AGutterRect: TRect; AScaleFactor: TdxScaleFactor);
begin
  if SkinInfo.PopupMenuSeparator = nil then
    inherited DrawDropDownListBoxScaledSeparator(ACanvas, ARect, AGutterRect, AScaleFactor)
  else
    SkinInfo.PopupMenuSeparator.Draw(ACanvas.Handle,
      cxRect(AGutterRect.Right + cxTextOffset, ARect.Top, ARect.Right, ARect.Bottom), AScaleFactor);
end;

function TdxSkinLookAndFeelPainter.AlertWindowScaledButtonContentOffsets(
  AKind: TdxAlertWindowButtonKind; AScaleFactor: TdxScaleFactor): TRect;
var
  AElement: TdxSkinElement;
begin
  AElement := AlertWindowButtonElement(AKind);
  if AElement <> nil then
    Result := AScaleFactor.Apply(AElement.ContentOffset.Rect)
  else
    Result := inherited AlertWindowScaledButtonContentOffsets(AKind, AScaleFactor);
end;

function TdxSkinLookAndFeelPainter.AlertWindowButtonElement(AKind: TdxAlertWindowButtonKind): TdxSkinElement;
begin
  if AKind in [awbkPrevious, awbkNext] then
    Result := SkinInfo.AlertWindowNavigationPanelButton
  else
    Result := SkinInfo.AlertWindowButton;
end;

function TdxSkinLookAndFeelPainter.AlertWindowScaledContentOffsets(AScaleFactor: TdxScaleFactor): TRect;
begin
  if SkinInfo.AlertWindow <> nil then
    Result := AScaleFactor.Apply(SkinInfo.AlertWindow.ContentOffset.Rect)
  else
    Result := inherited AlertWindowScaledContentOffsets(AScaleFactor);
end;

function TdxSkinLookAndFeelPainter.AlertWindowCornerRadius: Integer;
begin
  if SkinInfo.AlertWindow = nil then
    Result := inherited AlertWindowCornerRadius
  else
    if TdxSkinElementHelper.IsAlternateImageSetUsed(SkinInfo.AlertWindow, 0, esNormal) then
      Result := 0
    else
      Result := SkinInfo.AlertWindowCornerRadius;
end;

function TdxSkinLookAndFeelPainter.AlertWindowNavigationPanelTextColor: TColor;
begin
  if SkinInfo.AlertWindowNavigationPanel <> nil  then
    Result := SkinInfo.AlertWindowNavigationPanel.TextColor
  else
    Result := clDefault;

  if Result = clDefault then
    Result := inherited AlertWindowNavigationPanelTextColor;
end;

function TdxSkinLookAndFeelPainter.AlertWindowTextColor: TColor;
begin
  if SkinInfo.AlertWindow <> nil then
  begin
    Result := SkinInfo.AlertWindow.TextColor;
    if Result = clDefault then
      Result := DefaultContentTextColor;
  end
  else
    Result := inherited AlertWindowTextColor;
end;

procedure TdxSkinLookAndFeelPainter.DrawAlertWindowBackground(
  ACanvas: TcxCanvas; const ABounds: TRect; AScaleFactor: TdxScaleFactor = nil);
var
  ACaptionElement: TdxSkinElement;
  ACaptionSize: Integer;
  AWindowElement: TdxSkinElement;
begin
  AWindowElement := SkinInfo.AlertWindow;
  ACaptionElement := SkinInfo.AlertWindowCaption;
  if (AWindowElement <> nil) and (ACaptionElement <> nil) then
  begin
    if AScaleFactor = nil then
      AScaleFactor := dxSystemScaleFactor;
    AWindowElement.Draw(ACanvas.Handle, ABounds, AScaleFactor);
    ACaptionSize := ACaptionElement.MinSize.Height;
    if ACaptionSize = 0 then
      ACaptionSize := cxMarginsHeight(ACaptionElement.ContentOffset.Rect) + ACaptionElement.Glyph.Size.cy;
    ACaptionElement.Draw(ACanvas.Handle, cxRectSetHeight(ABounds, AScaleFactor.Apply(ACaptionSize)), AScaleFactor);
  end
  else
    inherited DrawAlertWindowBackground(ACanvas, ABounds, AScaleFactor);
end;

procedure TdxSkinLookAndFeelPainter.DrawAlertWindowScaledButton(ACanvas: TcxCanvas; const ABounds: TRect;
  AState: TcxButtonState; AKind: TdxAlertWindowButtonKind; AScaleFactor: TdxScaleFactor; ADown: Boolean);
const
  GlyphsMap: array[TdxAlertWindowButtonKind] of Integer = (0, 1, 3, 4, 5, -1);
  StatesMap: array[Boolean, TcxButtonState] of TdxSkinElementState = (
    (esNormal, esNormal, esHot, esPressed, esDisabled),
    (esChecked, esChecked, esHotCheck, esCheckPressed, esDisabled)
  );

  procedure DoDrawGlyph(AGlyphIndex: Integer);
  begin
    if AGlyphIndex >= 0 then
    begin
      if SkinInfo.AlertWindowButtonGlyphs <> nil then
        SkinInfo.AlertWindowButtonGlyphs.Draw(ACanvas.Handle, ABounds, AScaleFactor, AGlyphIndex, StatesMap[False, AState]);
    end;
  end;

var
  AElement: TdxSkinElement;
  AElementState: TdxSkinElementState;
begin
  AElement := AlertWindowButtonElement(AKind);
  if AElement = nil then
    inherited DrawAlertWindowScaledButton(ACanvas, ABounds, AState, AKind, AScaleFactor, ADown)
  else
  begin
    AElementState := StatesMap[ADown, AState];
    if AElementState in AElement.Image.States then
      AElement.Draw(ACanvas.Handle, ABounds, AScaleFactor, 0, AElementState);
    case AKind of
      awbkPin:
        DoDrawGlyph(1 + Ord(ADown));
      else
        DoDrawGlyph(GlyphsMap[AKind]);
    end;
  end;
end;

procedure TdxSkinLookAndFeelPainter.DrawAlertWindowNavigationPanel(ACanvas: TcxCanvas; const ABounds: TRect);
begin
  if not DrawSkinElement(SkinInfo.AlertWindowNavigationPanel, ACanvas, ABounds) then
    inherited DrawAlertWindowNavigationPanel(ACanvas, ABounds);
end;

function TdxSkinLookAndFeelPainter.GetScaledBackButtonSize(AScaleFactor: TdxScaleFactor): TSize;
begin
  if SkinInfo.BackButton = nil then
    Result := inherited GetScaledBackButtonSize(AScaleFactor)
  else
    Result := AScaleFactor.Apply(SkinInfo.BackButton.MinSize.Size);
end;

function TdxSkinLookAndFeelPainter.GetBevelMinimalShapeSize(AShape: TdxBevelShape): TSize;
var
  AColor1: TColor;
  AColor2: TColor;
  AShapeSize: Integer;
begin
  GetBevelShapeColors(AColor1, AColor2);
  AShapeSize := Integer(cxColorIsValid(AColor1)) + Integer(cxColorIsValid(AColor2));
  Result := cxSize(AShapeSize);
end;

procedure TdxSkinLookAndFeelPainter.GetBevelShapeColors(out AColor1, AColor2: TColor);
begin
  inherited GetBevelShapeColors(AColor1, AColor2);
  if (SkinInfo.BevelShapeColor1 <> nil) and (SkinInfo.BevelShapeColor1.Value <> clDefault) then
    AColor1 := SkinInfo.BevelShapeColor1.Value;
  if (SkinInfo.BevelShapeColor2 <> nil) and (SkinInfo.BevelShapeColor2.Value <> clDefault) then
    AColor2 := SkinInfo.BevelShapeColor2.Value;
end;

procedure TdxSkinLookAndFeelPainter.DrawCalendarDateCellSelection(
  ACanvas: TcxCanvas; const ARect: TRect; AStates: TcxCalendarElementStates);
begin
  if cesMarked in AStates then
    DrawDateNavigatorTodayCellSelection(ACanvas, ARect)
  else
    if cesSelected in AStates then
      DrawDateNavigatorCellSelection(ACanvas, ARect, DefaultDateNavigatorSelectionColor);
end;

procedure TdxSkinLookAndFeelPainter.DrawScaledCheckButton(
  ACanvas: TcxCanvas; R: TRect; AState: TcxButtonState; ACheckState: TcxCheckBoxState; AScaleFactor: TdxScaleFactor);
const
  ImageIndexMap: array[TcxCheckBoxState] of Integer = (0, 1, 2);
begin
  if not DrawSkinElement(SkinInfo.CheckboxElement, ACanvas, R, AScaleFactor, ButtonStateToSkinState[AState], ImageIndexMap[ACheckState]) then
    inherited DrawScaledCheckButton(ACanvas, R, AState, ACheckState, AScaleFactor);
end;

function TdxSkinLookAndFeelPainter.ScaledClockSize(AScaleFactor: TdxScaleFactor): TSize;
var
  AElement: TdxSkinElement;
begin
  AElement := SkinInfo.ClockElements[False];
  if AElement <> nil then
    Result := dxSkinGetElementSize(AElement, AScaleFactor)
  else
    Result := inherited ScaledClockSize(AScaleFactor);
end;

procedure TdxSkinLookAndFeelPainter.DrawScaledClock(ACanvas: TcxCanvas; const ARect: TRect;
  ADateTime: TDateTime; ABackgroundColor: TColor; AScaleFactor: TdxScaleFactor);
var
  ABitmap: TcxBitmap;
begin
  with SkinInfo do
    if (ClockElements[False] = nil) or (ClockElements[True] = nil) then
      inherited DrawScaledClock(ACanvas, ARect, ADateTime, ABackgroundColor, AScaleFactor)
    else
    begin
      ABitmap := TcxBitmap.CreateSize(ARect);
      try
        if ABackgroundColor = clNone then
          cxBitBlt(ABitmap.cxCanvas.Handle, ACanvas.Handle, ARect, cxNullPoint, SRCCOPY)
        else
          ABitmap.cxCanvas.FillRect(ARect, ABackgroundColor);

        ClockElements[False].Draw(ABitmap.Canvas.Handle, ARect, AScaleFactor);
        DrawScaledModernClockHands(ABitmap.cxCanvas, ARect, ADateTime, ClockElements[False].TextColor, AScaleFactor);
        ClockElements[True].Draw(ABitmap.Canvas.Handle, ARect, AScaleFactor);
        cxBitBlt(ACanvas.Handle, ABitmap.cxCanvas.Handle, ARect, cxNullPoint, SRCCOPY);
      finally
        ABitmap.Free;
      end;
    end;
end;

procedure TdxSkinLookAndFeelPainter.DrawScaledZoomInButton(
  ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState; AScaleFactor: TdxScaleFactor);
begin
  if not DrawSkinElement(SkinInfo.ZoomInButton, ACanvas, R, AScaleFactor, ButtonStateToSkinState[AState]) then
    inherited DrawScaledZoomInButton(ACanvas, R, AState, AScaleFactor)
end;

procedure TdxSkinLookAndFeelPainter.DrawScaledZoomOutButton(
  ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState; AScaleFactor: TdxScaleFactor);
begin
  if not DrawSkinElement(SkinInfo.ZoomOutButton, ACanvas, R, AScaleFactor, ButtonStateToSkinState[AState]) then
    inherited DrawScaledZoomOutButton(ACanvas, R, AState, AScaleFactor);
end;

function TdxSkinLookAndFeelPainter.GetScaledZoomInButtonSize(AScaleFactor: TdxScaleFactor): TSize;
begin
  if SkinInfo.ZoomInButton <> nil then
    Result := AScaleFactor.Apply(SkinInfo.ZoomInButton.Size)
  else
    Result := inherited GetScaledZoomInButtonSize(AScaleFactor);
end;

function TdxSkinLookAndFeelPainter.GetScaledZoomOutButtonSize(AScaleFactor: TdxScaleFactor): TSize;
begin
  if SkinInfo.ZoomOutButton <> nil then
    Result := AScaleFactor.Apply(SkinInfo.ZoomOutButton.Size)
  else
    Result := inherited GetScaledZoomOutButtonSize(AScaleFactor);
end;

procedure TdxSkinLookAndFeelPainter.DrawScaledEditorButton(
  ACanvas: TcxCanvas; const ARect: TRect; AButtonKind: TcxEditBtnKind;
  AState: TcxButtonState; AScaleFactor: TdxScaleFactor; APosition: TcxEditBtnPosition = cxbpRight);
var
  AElement: TdxSkinElement;
  R: TRect;
begin
  AElement := SkinInfo.EditButtonElements[AButtonKind = cxbkCloseBtn];
  if AElement <> nil then
  begin
    ACanvas.SaveClipRegion;
    try
      R := ARect;
      ACanvas.IntersectClipRect(ARect);
      EditButtonAdjustRect(R, APosition);
      AElement.UseCache := True;
      AElement.Draw(ACanvas.Handle, R, AScaleFactor, 0, ButtonStateToSkinState[AState]);
   finally
      ACanvas.RestoreClipRegion;
    end;
  end
  else
    inherited DrawScaledEditorButton(ACanvas, ARect, AButtonKind, AState, AScaleFactor, APosition)
end;

procedure TdxSkinLookAndFeelPainter.DrawScaledEditorButtonGlyph(ACanvas: TcxCanvas; const ARect: TRect;
  AButtonKind: TcxEditBtnKind; AState: TcxButtonState; AScaleFactor: TdxScaleFactor; APosition: TcxEditBtnPosition = cxbpRight);

  function GetActualTextColor: TColor;
  var
    AProperty: TdxSkinProperty;
  begin
    Result := SkinInfo.EditButtonElements[False].TextColor;
    if AState = cxbsDisabled then
      if SkinInfo.EditButtonElements[False].GetPropertyByName(sdxSkinsButtonDisabledTextColor, AProperty) then
      begin
        if TdxSkinColor(AProperty).Value <> clDefault then
          Result := TdxSkinColor(AProperty).Value;
      end;
  end;

  procedure DrawEllipsis(R: TRect; ASize: Integer);
  var
    AColor: TColor;
  begin
    R := cxRectCenter(R, 3 * ASize + 4, ASize);
    AColor := GetActualTextColor;
    ACanvas.FillRect(Rect(R.Left, R.Top, R.Left + ASize, R.Top + ASize), AColor);
    ACanvas.FillRect(Rect(R.Left + ASize + 2, R.Top, R.Left + ASize * 2 + 2, R.Top + ASize), AColor);
    ACanvas.FillRect(Rect(R.Left + ASize * 2 + 4, R.Top, R.Left + ASize * 3 + 4, R.Top + ASize), AColor);
  end;

  function GetGlyphRect(const R: TRect; AElement: TdxSkinElement): TRect;
  begin
    Result := cxRectContent(R, AElement.ContentOffset.Rect);
    if SkinInfo.EditButtonMergeBorders then
    begin
      if APosition = cxbpLeft then
        Dec(Result.Left, AElement.ContentOffset.Left - 1)
      else
        Inc(Result.Right, AElement.ContentOffset.Right - 1);
    end;
  end;

const
  EllipseSizeMap: array[Boolean] of Integer = (1, 2);
var
  AElement: TdxSkinElement;
  R: TRect;
begin
  AElement := SkinInfo.EditButtonElements[AButtonKind = cxbkCloseBtn];
  if AElement <> nil then
  begin
    ACanvas.SaveClipRegion;
    try
      R := ARect;
      ACanvas.IntersectClipRect(ARect);
      EditButtonAdjustRect(R, APosition);
      R := GetGlyphRect(R, AElement);
      if AButtonKind = cxbkEllipsisBtn then
        DrawEllipsis(R, AScaleFactor.Apply(EllipseSizeMap[cxRectWidth(R) >= AScaleFactor.Apply(12)]))
      else
        if SkinInfo.EditButtonGlyphs[AButtonKind] <> nil then
          SkinInfo.EditButtonGlyphs[AButtonKind].Glyph.Draw(ACanvas.Handle, R, AScaleFactor, 0, ButtonStateToSkinState[AState]);
   finally
      ACanvas.RestoreClipRegion;
    end;
  end
  else
    inherited DrawScaledEditorButton(ACanvas, ARect, AButtonKind, AState, AScaleFactor, APosition);
end;

function TdxSkinLookAndFeelPainter.EditButtonTextOffset: Integer;
begin
  Result := 1;
end;

procedure TdxSkinLookAndFeelPainter.EditButtonAdjustRect(var R: TRect; APosition: TcxEditBtnPosition);
begin
  if SkinInfo.EditButtonMergeBorders then
  begin
    InflateRect(R, 0, 1);
    if APosition = cxbpLeft then
      Dec(R.Left)
    else
      Inc(R.Right);
  end;
end;

function TdxSkinLookAndFeelPainter.EditButtonColorPalette(AState: TcxButtonState): IdxColorPalette;
begin
  Result := GetSkinElementColorPalette(SkinInfo.EditButtonElements[False], ButtonStateToSkinState[AState]);
end;

function TdxSkinLookAndFeelPainter.EditButtonSize: TSize;
begin
  if SkinInfo.EditButtonElements[False] <> nil then
    Result := dxSkinGetElementSize(SkinInfo.EditButtonElements[False], dxDefaultScaleFactor)
  else
    Result := inherited EditButtonSize;
end;

function TdxSkinLookAndFeelPainter.EditButtonTextColor: TColor;
begin
  with SkinInfo do
    if EditButtonElements[False] <> nil then
      Result := EditButtonElements[False].TextColor
    else
      Result := inherited EditButtonTextColor;
end;

function TdxSkinLookAndFeelPainter.GetContainerBorderColor(AIsHighlightBorder: Boolean): TColor;
var
  ASkinColor: TdxSkinColor;
begin
  with SkinInfo do
  begin
    if AIsHighlightBorder then
      ASkinColor := ContainerHighlightBorderColor
    else
      ASkinColor := ContainerBorderColor;

    if ASkinColor = nil then
      Result := inherited GetContainerBorderColor(AIsHighlightBorder)
    else
      Result := ASkinColor.Value;
  end;
end;

function TdxSkinLookAndFeelPainter.GetContainerBorderWidth(ABorderStyle: TcxContainerBorderStyle): Integer;
const
  BordersWidthMap: array [Boolean] of Integer = (1, cxContainerMaxBorderWidth);
begin
  if ABorderStyle = cbsNone then
    Result := inherited GetContainerBorderWidth(ABorderStyle)
  else
    Result := BordersWidthMap[ABorderStyle = cbsThick];
end;

procedure TdxSkinLookAndFeelPainter.DrawNavigatorBorder(ACanvas: TcxCanvas; R: TRect; ASelected: Boolean);
begin
  if SkinInfo.ContainerBorderColor <> nil then
  begin
    dxGPPaintCanvas.BeginPaint(ACanvas.Handle, R);
    try
      dxGPPaintCanvas.Rectangle(R, SkinInfo.ContainerBorderColor.Value, clNone, 1, psSolid, 255, 255);
    finally
      dxGPPaintCanvas.EndPaint;
    end;
  end
  else
    inherited DrawNavigatorBorder(ACanvas, R, ASelected);
end;

procedure TdxSkinLookAndFeelPainter.DrawNavigatorScaledButton(ACanvas: TcxCanvas; R: TRect; AState: TcxButtonState;
  ABackgroundColor: TColor; AScaleFactor: TdxScaleFactor);
begin
  if not DrawSkinElement(SkinInfo.NavigatorButton, ACanvas, R, AScaleFactor, ButtonStateToSkinState[AState]) then
    inherited DrawNavigatorScaledButton(ACanvas, R, AState, ABackgroundColor, AScaleFactor);
end;

procedure TdxSkinLookAndFeelPainter.DrawNavigatorScaledButtonGlyph(
  ACanvas: TcxCanvas; AImageList: TCustomImageList; AImageIndex: TcxImageIndex;
  const AGlyphRect: TRect; AEnabled, AUserGlyphs: Boolean; AScaleFactor: TdxScaleFactor);
const
  StateMap: array[Boolean] of TdxSkinElementState = (esDisabled, esNormal);
var
  ABitmap: TcxAlphaBitmap;
  AElement: TdxSkinElement;
  ARect: TRect;
begin
  if (SkinInfo.NavigatorGlyphs = nil) or (SkinInfo.NavigatorGlyphsVert = nil) or AUserGlyphs then
    inherited DrawNavigatorScaledButtonGlyph(ACanvas, AImageList, AImageIndex, AGlyphRect, AEnabled, AUserGlyphs, AScaleFactor)
  else
    if not IsRectEmpty(AGlyphRect) then
    begin
      AElement := SkinInfo.NavigatorGlyphs;
      if SkinInfo.NavigatorGlyphs.ImageCount <= AImageIndex then
      begin
        AElement := SkinInfo.NavigatorGlyphsVert;
        Dec(AImageIndex, SkinInfo.NavigatorGlyphs.ImageCount);
      end;

      ARect := cxRectCenter(AGlyphRect, AScaleFactor.Apply(AElement.Size));
      if esDisabled in AElement.States then
        AElement.Draw(ACanvas.Handle, ARect, AScaleFactor, AImageIndex, StateMap[AEnabled])
      else
      begin
        ABitmap := TcxAlphaBitmap.CreateSize(AElement.Size.cx, AElement.Size.cy, True);
        try
          AElement.Draw(ABitmap.Canvas.Handle, ABitmap.ClientRect, dxDefaultScaleFactor, AImageIndex);
          if not AEnabled then
            dxSkinElementMakeDisable(ABitmap);
          cxAlphaBlend(ACanvas.Handle, ABitmap, ARect, ABitmap.ClientRect);
        finally
          ABitmap.Free;
        end;
      end;
    end;
end;

function TdxSkinLookAndFeelPainter.NavigatorBorderOverlap: Boolean;
begin
  Result := False;
end;

function TdxSkinLookAndFeelPainter.NavigatorScaledButtonGlyphPadding(AScaleFactor: TdxScaleFactor): TRect;
begin
  if SkinInfo.NavigatorButton <> nil then
    Result := AScaleFactor.Apply(SkinInfo.NavigatorButton.ContentOffset.Rect)
  else
    Result := inherited NavigatorScaledButtonGlyphPadding(AScaleFactor);
end;

function TdxSkinLookAndFeelPainter.NavigatorScaledButtonGlyphSize(AScaleFactor: TdxScaleFactor): TSize;
begin
  if SkinInfo.NavigatorGlyphs <> nil then
    Result := AScaleFactor.Apply(SkinInfo.NavigatorGlyphs.Size)
  else
    Result := inherited NavigatorScaledButtonGlyphSize(AScaleFactor);
end;

function TdxSkinLookAndFeelPainter.NavigatorButtonColorPalette(AEnabled: Boolean): IdxColorPalette;
const
  StateMap: array[Boolean] of TdxSkinElementState = (esDisabled, esNormal);
begin
  if SkinInfo.NavigatorButton <> nil then
    Result := SkinInfo.NavigatorButton.GetGlyphColorPalette(StateMap[AEnabled])
  else
    Result := inherited NavigatorButtonColorPalette(AEnabled);
end;

function TdxSkinLookAndFeelPainter.NavigatorButtonPressedGlyphOffset: TPoint;
begin
  Result := cxNullPoint;
end;

function TdxSkinLookAndFeelPainter.NavigatorButtonTextColor(AState: TcxButtonState): TColor;
begin
  if SkinInfo.NavigatorButton <> nil then
    Result := SkinInfo.NavigatorButton.GetTextColor(AState)
  else
    Result := clDefault;

  if Result = clDefault then
    Result := inherited NavigatorButtonTextColor(AState);
end;

function TdxSkinLookAndFeelPainter.NavigatorInfoPanelColor: TColor;
begin
  if IsColorPropertyAssigned(SkinInfo.NavigatorInfoPanelColor) then
    Result := SkinInfo.NavigatorInfoPanelColor.Value
  else
    Result := inherited;
end;

function TdxSkinLookAndFeelPainter.NavigatorInfoPanelTextColor: TColor;
begin
  if IsColorPropertyAssigned(SkinInfo.NavigatorInfoPanelTextColor) then
    Result := SkinInfo.NavigatorInfoPanelTextColor.Value
  else
    Result := inherited;
end;

procedure TdxSkinLookAndFeelPainter.DrawProgressBarBorder(ACanvas: TcxCanvas; ARect: TRect; AVertical: Boolean);
begin
  if not DrawSkinElement(SkinInfo.ProgressBarElements[False, AVertical], ACanvas, ARect) then
    inherited DrawProgressBarBorder(ACanvas, ARect, AVertical);
end;

procedure TdxSkinLookAndFeelPainter.DrawProgressBarChunk(ACanvas: TcxCanvas; ARect: TRect; AVertical: Boolean);
begin
  if not DrawSkinElement(SkinInfo.ProgressBarElements[True, AVertical], ACanvas, ARect) then
    inherited DrawProgressBarChunk(ACanvas, ARect, AVertical);
end;

function TdxSkinLookAndFeelPainter.ProgressBarBorderSize(AVertical: Boolean): TRect;
begin
  if SkinInfo.ProgressBarElements[False, AVertical] <> nil then
    Result := SkinInfo.ProgressBarElements[False, AVertical].ContentOffset.Rect
  else
    Result := inherited ProgressBarBorderSize(AVertical);
end;

function TdxSkinLookAndFeelPainter.ProgressBarTextColorEx(AIsFilledArea: Boolean): TColor;
begin
  if IsColorPropertyAssigned(SkinInfo.ProgressBarTextColors[AIsFilledArea]) then
    Result := SkinInfo.ProgressBarTextColors[AIsFilledArea].Value
  else
    Result := inherited ProgressBarTextColorEx(AIsFilledArea);
end;

function TdxSkinLookAndFeelPainter.GroupBoxBorderSize(
  ACaption: Boolean; ACaptionPosition: TcxGroupBoxCaptionPosition): TRect;
var
  AGroupBoxInfo: TdxSkinElement;
begin
  if ACaption then
    AGroupBoxInfo := SkinInfo.GroupBoxCaptionElements[ACaptionPosition]
  else
    AGroupBoxInfo := SkinInfo.GroupBoxElements[ACaptionPosition];

  if AGroupBoxInfo <> nil then
    Result := AGroupBoxInfo.ContentOffset.Rect
  else
    Result := inherited GroupBoxBorderSize(ACaption, ACaptionPosition);
end;

procedure TdxSkinLookAndFeelPainter.DrawGroupBoxFrame(ACanvas: TcxCanvas;
  R: TRect; AEnabled: Boolean; ACaptionPosition: TcxGroupBoxCaptionPosition;
  ABorders: TcxBorders = cxBordersAll);
begin
  DrawGroupBoxContent(ACanvas, R, ACaptionPosition, ABorders);
end;

procedure TdxSkinLookAndFeelPainter.GroupBoxAdjustCaptionFont(ACaptionFont: TFont; ACaptionPosition: TcxGroupBoxCaptionPosition);
begin
  if IsFontBold(SkinInfo.GroupBoxCaptionElements[ACaptionPosition]) then
    ACaptionFont.Style := ACaptionFont.Style + [fsBold];
end;

function TdxSkinLookAndFeelPainter.GroupBoxCaptionTailSize(ACaptionPosition: TcxGroupBoxCaptionPosition): Integer;
begin
  if SkinInfo.GroupBoxCaptionTailSizes[ACaptionPosition] <> nil then
    Result := SkinInfo.GroupBoxCaptionTailSizes[ACaptionPosition].Value
  else
    Result := inherited GroupBoxCaptionTailSize(ACaptionPosition);
end;

function TdxSkinLookAndFeelPainter.GroupBoxTextColor(
  AEnabled: Boolean; ACaptionPosition: TcxGroupBoxCaptionPosition): TColor;
var
  AGroupBoxCaption: TdxSkinElement;
begin
  if ACaptionPosition = cxgpCenter then
    Result := PanelTextColor
  else
  begin
    AGroupBoxCaption := SkinInfo.GroupBoxCaptionElements[ACaptionPosition];
    if AGroupBoxCaption <> nil then
      Result := AGroupBoxCaption.TextColor
    else
      Result := inherited GroupBoxTextColor(AEnabled, ACaptionPosition)
  end;
end;

function TdxSkinLookAndFeelPainter.IsGroupBoxCaptionTextDrawnOverBorder(ACaptionPosition: TcxGroupBoxCaptionPosition): Boolean;
begin
  if SkinInfo.GroupBoxCaptionElements[ACaptionPosition] <> nil then
    Result := cxColorIsValid(SkinInfo.GroupBoxCaptionElements[ACaptionPosition].Color)
  else
    Result := inherited IsGroupBoxCaptionTextDrawnOverBorder(ACaptionPosition);
end;

function TdxSkinLookAndFeelPainter.IsGroupBoxTransparent(
  AIsCaption: Boolean; ACaptionPosition: TcxGroupBoxCaptionPosition): Boolean;
var
  AGroupBoxInfo: TdxSkinElement;
begin
  with SkinInfo do
    if AIsCaption then
      AGroupBoxInfo := GroupBoxCaptionElements[ACaptionPosition]
    else
      AGroupBoxInfo := GroupBoxElements[ACaptionPosition];

  if AGroupBoxInfo = nil then
    Result := inherited IsGroupBoxTransparent(AIsCaption, ACaptionPosition)
  else
    Result := AGroupBoxInfo.IsAlphaUsed;
end;

procedure TdxSkinLookAndFeelPainter.DrawGroupBoxCaption(ACanvas: TcxCanvas;
  const ACaptionRect, ATextRect: TRect; ACaptionPosition: TcxGroupBoxCaptionPosition);
begin
  if SkinInfo.GroupBoxCaptionElements[ACaptionPosition] = nil then
    inherited DrawGroupBoxCaption(ACanvas, ACaptionRect, ATextRect, ACaptionPosition)
  else
    DrawGroupCaption(ACanvas, ACaptionRect, ATextRect,
      SkinInfo.GroupBoxCaptionElements[ACaptionPosition],
      SkinInfo.GroupBoxCaptionTextPadding[ACaptionPosition], esNormal);
end;

procedure TdxSkinLookAndFeelPainter.DrawGroupBoxContent(ACanvas: TcxCanvas;
  ABorderRect: TRect; ACaptionPosition: TcxGroupBoxCaptionPosition; ABorders: TcxBorders = cxBordersAll);
var
  ARect: TRect;
begin
  with SkinInfo do
    if (GroupBoxElements[ACaptionPosition] = nil) or (GroupBoxClient = nil) then
      inherited DrawGroupBoxContent(ACanvas, ABorderRect, ACaptionPosition)
    else
      if not IsRectEmpty(ABorderRect) then
      begin
        ARect := ABorderRect;
        ACanvas.SaveClipRegion;
        try
          ACanvas.IntersectClipRect(ABorderRect);
          with GetSkinElementBordersWidth(GroupBoxElements[ACaptionPosition]) do
          begin
            if bLeft in ABorders then
              Inc(ARect.Left, Left);
            if bTop in ABorders then
              Inc(ARect.Top, Top);
            if bRight in ABorders then
              Dec(ARect.Right, Right);
            if bBottom in ABorders then
              Dec(ARect.Bottom, Bottom);
          end;
          GroupBoxClient.Draw(ACanvas.Handle, ARect);
          ACanvas.ExcludeClipRect(ARect);
          GroupBoxElements[ACaptionPosition].Draw(ACanvas.Handle, ABorderRect);
        finally
          ACanvas.RestoreClipRegion;
        end;
      end;
end;

procedure TdxSkinLookAndFeelPainter.DrawGroupBoxScaledExpandButton(ACanvas: TcxCanvas; const R: TRect;
  AState: TcxButtonState; AExpanded: Boolean; AScaleFactor: TdxScaleFactor; ARotationAngle: TcxRotationAngle = ra0);

  procedure DoDrawGroupBoxExpandButton(ACanvas: TcxCanvas; const R: TRect);
  begin
    if DrawSkinElement(SkinInfo.GroupButton, ACanvas, R, AScaleFactor, ButtonStateToSkinState[AState], Ord(not AExpanded)) then
      DrawSkinElement(SkinInfo.GroupButtonExpandGlyph, ACanvas, R, AScaleFactor, ButtonStateToSkinState[AState], Ord(not AExpanded))
    else
      inherited DrawGroupBoxScaledExpandButton(ACanvas, R, AState, AExpanded, AScaleFactor);
  end;

var
  ABitmap: TcxAlphaBitmap;
  ARect: TRect;
begin
  if ARotationAngle = ra0 then
    DoDrawGroupBoxExpandButton(ACanvas, R)
  else
  begin
    if ARotationAngle in [raPlus90, raMinus90] then
      ARect := cxRectRotate(R)
    else
      ARect := R;

    ABitmap := TcxAlphaBitmap.CreateSize(ARect);
    try
      ABitmap.Clear;
      DoDrawGroupBoxExpandButton(ABitmap.cxCanvas, ABitmap.ClientRect);
      ACanvas.RotateBitmap(ABitmap, ARotationAngle);
      cxDrawImage(ACanvas.Handle, R, R, ABitmap, nil, -1, idmNormal);
    finally
      ABitmap.Free;
    end;
  end;
end;

procedure TdxSkinLookAndFeelPainter.DrawGroupBoxScaledButton(
  ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState; AScaleFactor: TdxScaleFactor);
begin
  if not DrawSkinElement(SkinInfo.GroupButton, ACanvas, R, AScaleFactor, ButtonStateToSkinState[AState]) then
    inherited;
end;

procedure TdxSkinLookAndFeelPainter.DrawGroupBoxScaledExpandGlyph(ACanvas: TcxCanvas;
  const R: TRect; AState: TcxButtonState; AExpanded: Boolean; AScaleFactor: TdxScaleFactor);
begin
  if not DrawSkinElement(SkinInfo.GroupButtonExpandGlyph, ACanvas, R, AScaleFactor, ButtonStateToSkinState[AState], Ord(not AExpanded)) then
    inherited;
end;

procedure TdxSkinLookAndFeelPainter.DrawGroupBoxBackground(ACanvas: TcxCanvas; ABounds: TRect; ARect: TRect);
begin
  if not DrawSkinElement(SkinInfo.GroupBoxClient, ACanvas, ARect) then
    inherited DrawGroupBoxBackground(ACanvas, ABounds, ARect);
end;

procedure TdxSkinLookAndFeelPainter.DrawScaledHeader(ACanvas: TcxCanvas; const ABounds, ATextAreaBounds: TRect;
  ANeighbors: TcxNeighbors; ABorders: TcxBorders; AState: TcxButtonState;  AAlignmentHorz: TAlignment;
  AAlignmentVert: TcxAlignmentVert; AMultiLine, AShowEndEllipsis: Boolean; const AText: string; AFont: TFont;
  ATextColor, ABkColor: TColor; AScaleFactor: TdxScaleFactor; AOnDrawBackground: TcxDrawBackgroundEvent = nil;
  AIsLast: Boolean = False; AIsGroup: Boolean = False);

  function AdjustHeaderBounds(const ABounds: TRect; ABorders: TcxBorders): TRect;
  begin
    Result := cxRectExcludeBorders(ABounds, cxRect(1, 1, 1, 1), ABorders);
  end;

var
  AHeader: TdxSkinElement;
begin
  with SkinInfo do
    if AIsGroup then
      AHeader := HeaderSpecial
    else
      AHeader := Header;

  if AHeader <> nil then
  begin
    ACanvas.SaveClipRegion;
    try
      ACanvas.IntersectClipRect(ABounds);
      AHeader.UseCache := True;
      AHeader.Draw(ACanvas.Handle, AdjustHeaderBounds(ABounds, ABorders), AScaleFactor, 0, ButtonStateToSkinState[AState]);
      DrawContent(ACanvas, ScaledHeaderContentBounds(ABounds, ABorders, AScaleFactor), ATextAreaBounds, Integer(AState),
        AAlignmentHorz, AAlignmentVert, AMultiLine, AShowEndEllipsis, AText, AFont, ATextColor, ABkColor, AOnDrawBackground);
    finally
      ACanvas.RestoreClipRegion;
    end;
  end
  else
    inherited DrawScaledHeader(ACanvas, ABounds, ATextAreaBounds, ANeighbors, ABorders,
      AState, AAlignmentHorz, AAlignmentVert, AMultiLine, AShowEndEllipsis, AText,
      AFont, ATextColor, ABkColor, AScaleFactor, AOnDrawBackGround, AIsLast, AIsGroup)
end;

procedure TdxSkinLookAndFeelPainter.DrawScaledHeaderEx(ACanvas: TcxCanvas; const ABounds, ATextAreaBounds: TRect;
  ANeighbors: TcxNeighbors; ABorders: TcxBorders; AState: TcxButtonState;  AAlignmentHorz: TAlignment;
  AAlignmentVert: TcxAlignmentVert; AMultiLine, AShowEndEllipsis: Boolean; const AText: string; AFont: TFont;
  ATextColor, ABkColor: TColor; AScaleFactor: TdxScaleFactor; AOnDrawBackground: TcxDrawBackgroundEvent = nil);
begin
  DrawScaledHeader(ACanvas, ABounds, ATextAreaBounds, ANeighbors, ABorders, AState, AAlignmentHorz, AAlignmentVert,
    AMultiLine, AShowEndEllipsis, AText, AFont, ATextColor, ABkColor, AScaleFactor, AOnDrawBackground);
end;

procedure TdxSkinLookAndFeelPainter.DrawHeaderPressed(ACanvas: TcxCanvas; const ABounds: TRect);
begin
  // do nothing
end;

procedure TdxSkinLookAndFeelPainter.DrawScaledHeaderControlSection(ACanvas: TcxCanvas; const ABounds, ATextAreaBounds: TRect;
  ANeighbors: TcxNeighbors; ABorders: TcxBorders; AState: TcxButtonState; AAlignmentHorz: TAlignment;
  AAlignmentVert: TcxAlignmentVert; AMultiLine, AShowEndEllipsis: Boolean; const AText: string; AFont: TFont;
  ATextColor, ABkColor: TColor; AScaleFactor: TdxScaleFactor);
begin
  ABorders := [bLeft, bTop, bBottom, bRight];
  if nLeft in ANeighbors then
    Exclude(ABorders, bLeft);
  DrawScaledHeader(ACanvas, ABounds, ATextAreaBounds, ANeighbors, ABorders, AState, AAlignmentHorz,
    AAlignmentVert, AMultiLine, AShowEndEllipsis, AText, AFont, ATextColor, ABkColor, AScaleFactor);
end;

procedure TdxSkinLookAndFeelPainter.DrawHeaderSeparator(ACanvas: TcxCanvas;
  const ABounds: TRect; AIndentSize: Integer; AColor: TColor; AViewParams: TcxViewParams);
begin
  with SkinInfo do
    if HeaderBackgroundColor = nil then
      inherited DrawHeaderSeparator(ACanvas, ABounds, AIndentSize, AColor, AViewParams)
    else
      ACanvas.FillRect(cxRectInflate(ABounds, -AIndentSize, 0), HeaderBackgroundColor.Value);
end;

procedure TdxSkinLookAndFeelPainter.DrawScaledSortingMark(
  ACanvas: TcxCanvas; const R: TRect; AAscendingSorting: Boolean; AScaleFactor: TdxScaleFactor);
begin
  if not DrawSkinElement(SkinInfo.SortGlyphs, ACanvas, R, AScaleFactor, esNormal, Integer(AAscendingSorting)) then
    inherited DrawScaledSortingMark(ACanvas, R, AAscendingSorting, AScaleFactor);
end;

procedure TdxSkinLookAndFeelPainter.DrawScaledSummarySortingMark(
  ACanvas: TcxCanvas; const R: TRect; AAscendingSorting: Boolean; AScaleFactor: TdxScaleFactor);
begin
  if not DrawSkinElement(SkinInfo.SortGlyphs, ACanvas, R, AScaleFactor, esNormal, 2 + Integer(AAscendingSorting)) then
    inherited DrawScaledSummarySortingMark(ACanvas, R, AAscendingSorting, AScaleFactor);
end;

procedure TdxSkinLookAndFeelPainter.DrawScaledSummaryValueSortingMark(
  ACanvas: TcxCanvas; const R: TRect; AAscendingSorting: Boolean; AScaleFactor: TdxScaleFactor);
begin
  if not DrawSkinElement(SkinInfo.IndicatorImages, ACanvas, R, AScaleFactor, esNormal, 10) then
    inherited DrawScaledSummaryValueSortingMark(ACanvas, R, AAscendingSorting, AScaleFactor);
end;

function TdxSkinLookAndFeelPainter.HeaderBorders(ANeighbors: TcxNeighbors): TcxBorders;
begin
  Result := inherited HeaderBorders(ANeighbors);
  if nLeft in ANeighbors then
    Exclude(Result, bLeft);
  if nTop in ANeighbors then
    Exclude(Result, bTop);
end;

function TdxSkinLookAndFeelPainter.HeaderContentOffsets(AScaleFactor: TdxScaleFactor): TRect;
begin
  if SkinInfo.Header <> nil then
    Result := AScaleFactor.Apply(SkinInfo.Header.ContentOffset.Rect)
  else
    Result := inherited;
end;

function TdxSkinLookAndFeelPainter.HeaderDrawCellsFirst: Boolean;
begin
  Result := False;
end;

function TdxSkinLookAndFeelPainter.ScaledSummarySortingMarkSize(AScaleFactor: TdxScaleFactor): TPoint;
begin
  Result := ScaledSortingMarkSize(AScaleFactor);
end;

function TdxSkinLookAndFeelPainter.ScaledSummaryValueSortingMarkSize(AScaleFactor: TdxScaleFactor): TPoint;
begin
  Result := inherited ScaledSummaryValueSortingMarkSize(AScaleFactor);
  Result.Y := AScaleFactor.Apply(SkinInfo.IndicatorImages.Size.cy);
end;

procedure TdxSkinLookAndFeelPainter.OfficeNavigationBarDrawBackground(ACanvas: TcxCanvas; const ARect: TRect);
begin
  if not DrawSkinElement(SkinInfo.NavPaneOfficeNavigationBar, ACanvas, ARect) then
    inherited;
end;

procedure TdxSkinLookAndFeelPainter.OfficeNavigationBarDrawScaledButtonItemBackground(
  ACanvas: TcxCanvas; const ARect: TRect; AState: TcxCalendarElementState; AScaleFactor: TdxScaleFactor);
begin
  if not DrawSkinElement(SkinInfo.NavPaneOfficeNavigationBarSkinningItem, ACanvas,
    ARect, AScaleFactor, CalendarElementStateToSkinElementState[AState])
  then
    inherited;
end;

procedure TdxSkinLookAndFeelPainter.OfficeNavigationBarDrawScaledItemBackground(
  ACanvas: TcxCanvas; const ARect: TRect; AState: TcxCalendarElementState; AScaleFactor: TdxScaleFactor);
begin
  if not DrawSkinElement(SkinInfo.NavPaneOfficeNavigationBarItem, ACanvas, ARect, AScaleFactor) then
    inherited;
end;

function TdxSkinLookAndFeelPainter.OfficeNavigationBarScaledContentOffsets(AScaleFactor: TdxScaleFactor): TRect;
begin
  if (SkinInfo.NavPaneOfficeNavigationBar <> nil) and not cxRectIsNull(SkinInfo.NavPaneOfficeNavigationBar.ContentOffset.Rect) then
    Result := AScaleFactor.Apply(SkinInfo.NavPaneOfficeNavigationBar.ContentOffset.Rect)
  else
    Result := inherited OfficeNavigationBarScaledContentOffsets(AScaleFactor);
end;

function TdxSkinLookAndFeelPainter.OfficeNavigationBarScaledButtonItemContentOffsets(AScaleFactor: TdxScaleFactor): TRect;
begin
  if SkinInfo.NavPaneOfficeNavigationBarSkinningItem <> nil then
    Result := AScaleFactor.Apply(SkinInfo.NavPaneOfficeNavigationBarSkinningItem.ContentOffset.Rect)
  else
    Result := inherited OfficeNavigationBarScaledButtonItemContentOffsets(AScaleFactor);
end;

function TdxSkinLookAndFeelPainter.OfficeNavigationBarScaledButtonItemFontSize(AScaleFactor: TdxScaleFactor): Integer;
begin
  Result := AScaleFactor.Apply(8 + SkinInfo.NavPaneOfficeNavigationBarSkinningItemFontDelta);
end;

function TdxSkinLookAndFeelPainter.OfficeNavigationBarButtonItemTextColor(AState: TcxCalendarElementState): TColor;
begin
  if SkinInfo.NavPaneOfficeNavigationBarSkinningItem <> nil then
    Result := SkinInfo.NavPaneOfficeNavigationBarSkinningItem.GetTextColor(CalendarElementStateToButtonState[AState])
  else
    Result := inherited OfficeNavigationBarButtonItemTextColor(AState);
end;

function TdxSkinLookAndFeelPainter.OfficeNavigationBarScaledItemContentOffsets(AScaleFactor: TdxScaleFactor): TRect;
begin
  if SkinInfo.NavPaneOfficeNavigationBarItem <> nil then
    Result := AScaleFactor.Apply(SkinInfo.NavPaneOfficeNavigationBarItem.ContentOffset.Rect)
  else
    Result := inherited OfficeNavigationBarScaledItemContentOffsets(AScaleFactor);
end;

function TdxSkinLookAndFeelPainter.OfficeNavigationBarScaledItemFontSize(AScaleFactor: TdxScaleFactor): Integer;
begin
  Result := AScaleFactor.Apply(8 + SkinInfo.NavPaneOfficeNavigationBarItemFontDelta);
end;

function TdxSkinLookAndFeelPainter.OfficeNavigationBarItemTextColor(AState: TcxCalendarElementState): TColor;
begin
  if SkinInfo.NavPaneOfficeNavigationBarItem <> nil then
    Result := SkinInfo.NavPaneOfficeNavigationBarItem.GetTextColor(CalendarElementStateToButtonState[AState])
  else
    Result := inherited OfficeNavigationBarItemTextColor(AState);
end;

procedure TdxSkinLookAndFeelPainter.DrawFilterRowSeparator(
  ACanvas: TcxCanvas; const ARect: TRect; ABackgroundColor: TColor);
begin
  DrawScaledHeader(ACanvas, ARect, ARect, [nLeft, nTop, nRight, nBottom], [], cxbsNormal,
    taLeftJustify, vaTop, False, False, '', nil, clWindowText, ABackgroundColor, dxDefaultScaleFactor);
  ACanvas.FrameRect(ARect, DefaultFixedSeparatorColor, 1, [bTop, bRight, bBottom]);
end;

procedure TdxSkinLookAndFeelPainter.DrawGroupByBox(ACanvas: TcxCanvas; const ARect: TRect;
  ATransparent: Boolean; ABackgroundColor: TColor; const ABackgroundBitmap: TBitmap);
begin
  if not DrawSkinElement(SkinInfo.GridGroupByBox, ACanvas, ARect) then
    inherited DrawGroupByBox(ACanvas, ARect, ATransparent, ABackgroundColor, ABackgroundBitmap)
end;

function TdxSkinLookAndFeelPainter.PDFViewerNavigationPaneButtonColorPalette(AState: TcxButtonState): IdxColorPalette;
begin
  Result := GetSkinElementColorPalette(SkinInfo.PDFViewerNavigationPaneButton, ButtonStateToSkinState[AState]);
end;

function TdxSkinLookAndFeelPainter.PDFViewerNavigationPaneButtonContentOffsets(AScaleFactor: TdxScaleFactor): TRect;
begin
  if SkinInfo.PDFViewerNavigationPaneButton <> nil then
    Result := AScaleFactor.Apply(SkinInfo.PDFViewerNavigationPaneButton.ContentOffset.Rect)
  else
    Result := inherited PDFViewerNavigationPaneButtonContentOffsets(AScaleFactor);
end;

function TdxSkinLookAndFeelPainter.PDFViewerNavigationPaneButtonOverlay(AScaleFactor: TdxScaleFactor): TPoint;
begin
  Result := inherited PDFViewerNavigationPaneButtonOverlay(AScaleFactor);
  if SkinInfo.PDFViewerNavigationPaneSelectedPageOverlapValue <> nil then
    Result.X := SkinInfo.PDFViewerNavigationPaneSelectedPageOverlapValue.Value;
  if SkinInfo.PDFViewerNavigationPaneSelectedPageExpandValue <> nil then
    Result.Y := SkinInfo.PDFViewerNavigationPaneSelectedPageExpandValue.Value;
end;

function TdxSkinLookAndFeelPainter.PDFViewerNavigationPaneButtonSize(AScaleFactor: TdxScaleFactor): TSize;
begin
  if SkinInfo.PDFViewerNavigationPaneButton <> nil then
    Result := dxSkinGetElementSize(SkinInfo.PDFViewerNavigationPaneButton, AScaleFactor)
  else
    Result := inherited PDFViewerNavigationPaneButtonSize(AScaleFactor);
end;

function TdxSkinLookAndFeelPainter.PDFViewerNavigationPaneContentOffsets(AScaleFactor: TdxScaleFactor): TRect;
begin
  if SkinInfo.PDFViewerNavigationPaneBackground <> nil then
    Result := AScaleFactor.Apply(SkinInfo.PDFViewerNavigationPaneBackground.ContentOffset.Rect)
  else
    Result := inherited PDFViewerNavigationPaneContentOffsets(AScaleFactor);
end;

function TdxSkinLookAndFeelPainter.PDFViewerNavigationPanePageCaptionContentOffsets(AScaleFactor: TdxScaleFactor): TRect;
begin
  if SkinInfo.PDFViewerNavigationPanePageCaption <> nil then
    Result := AScaleFactor.Apply(SkinInfo.PDFViewerNavigationPanePageCaption.ContentOffset.Rect)
  else
    Result := inherited PDFViewerNavigationPanePageCaptionContentOffsets(AScaleFactor);
end;

function TdxSkinLookAndFeelPainter.PDFViewerNavigationPanePageCaptionTextColor: TColor;
begin
  if SkinInfo.PDFViewerNavigationPanePageCaption <> nil then
    Result := SkinInfo.PDFViewerNavigationPanePageCaption.TextColor
  else
    Result := inherited PDFViewerNavigationPanePageCaptionTextColor;
end;

function TdxSkinLookAndFeelPainter.PDFViewerNavigationPanePageContentOffsets(AScaleFactor: TdxScaleFactor): TRect;
begin
  if SkinInfo.PDFViewerNavigationPanePageBackground <> nil then
    Result := AScaleFactor.Apply(SkinInfo.PDFViewerNavigationPanePageBackground.ContentOffset.Rect)
  else
    Result := inherited PDFViewerNavigationPanePageContentOffsets(AScaleFactor);
end;

function TdxSkinLookAndFeelPainter.PDFViewerNavigationPanePageToolbarContentOffsets(AScaleFactor: TdxScaleFactor): TRect;
begin
  if SkinInfo.Bar <> nil then
    Result := AScaleFactor.Apply(SkinInfo.Bar.ContentOffset.Rect)
  else
    Result := inherited PDFViewerNavigationPanePageToolbarContentOffsets(AScaleFactor);
end;

function TdxSkinLookAndFeelPainter.PDFViewerSelectionColor: TColor;
begin
  if IsColorPropertyAssigned(SkinInfo.PDFViewerSelectionColor) then
    Result := SkinInfo.PDFViewerSelectionColor.Value
  else
    Result := inherited PDFViewerSelectionColor;
end;

procedure TdxSkinLookAndFeelPainter.PDFViewerDrawNavigationPaneBackground(ACanvas: TcxCanvas; const ARect: TRect;
  AScaleFactor: TdxScaleFactor);
begin
  if not DrawSkinElement(SkinInfo.PDFViewerNavigationPaneBackground, ACanvas, ARect, esActive) then
    inherited PDFViewerDrawNavigationPaneBackground(ACanvas, ARect, AScaleFactor);
end;

procedure TdxSkinLookAndFeelPainter.PDFViewerDrawNavigationPanePageBackground(ACanvas: TcxCanvas; const ARect: TRect);
begin
  if not DrawSkinElement(SkinInfo.PDFViewerNavigationPanePageBackground, ACanvas, ARect) then
    inherited PDFViewerDrawNavigationPanePageBackground(ACanvas, ARect);
end;

procedure TdxSkinLookAndFeelPainter.PDFViewerDrawNavigationPanePageButton(ACanvas: TcxCanvas; const ARect: TRect;
  AState: TcxButtonState; AScaleFactor: TdxScaleFactor);
begin
  if not DrawSkinElement(SkinInfo.PDFViewerNavigationPanePageButton, ACanvas, ARect, AScaleFactor,
    ButtonStateToSkinState[AState]) then
    inherited PDFViewerDrawNavigationPanePageButton(ACanvas, ARect, AState, AScaleFactor);
end;

procedure TdxSkinLookAndFeelPainter.PDFViewerDrawNavigationPanePageCaptionBackground(ACanvas: TcxCanvas;
  const ARect: TRect);
begin
  if not DrawSkinElement(SkinInfo.PDFViewerNavigationPanePageCaption, ACanvas, ARect) then
    inherited PDFViewerDrawNavigationPanePageCaptionBackground(ACanvas, ARect);
end;

procedure TdxSkinLookAndFeelPainter.PDFViewerDrawNavigationPanePageToolbarBackground(ACanvas: TcxCanvas;
  const ARect: TRect);
begin
  if not DrawSkinElement(SkinInfo.Dock, ACanvas, ARect) then
    inherited PDFViewerDrawNavigationPanePageToolbarBackground(ACanvas, ARect);
end;

procedure TdxSkinLookAndFeelPainter.PDFViewerDrawNavigationPaneButton(ACanvas: TcxCanvas; const ARect: TRect;
  AState: TcxButtonState; AScaleFactor: TdxScaleFactor; AMinimized, ASelected, AIsFirst: Boolean);
const
  StateMap: array[TcxButtonState] of TdxSkinElementState = (esNormal, esNormal, esHot, esDisabled, esDisabled);
var
  AElement: TdxSkinElement;
  AElementState: TdxSkinElementState;
  ASize: TSize;
  AButtonRect, AArrowRect: TRect;
begin
  if AMinimized then
    AElement := SkinInfo.PDFViewerNavigationPaneButtonMinimized
  else
    AElement := SkinInfo.PDFViewerNavigationPaneButton;

  if ASelected then
    if esDisabled in AElement.States then
    begin
      if AIsFirst then
        AElementState := esDisabled
      else
        AElementState := esPressed;
    end
    else
      AElementState := esHot
  else
    AElementState := StateMap[AState];

  AButtonRect := PDFViewerNavigationPaneButtonRect(ARect, AState, ASelected, AScaleFactor);

  DrawSkinElementRotated(ACanvas, AElement, AButtonRect, AElementState, raMinus90, True, AScaleFactor);

  if not AMinimized and ASelected and (SkinInfo.PDFViewerNavigationPaneButtonArrow <> nil) then
  begin
    ASize := dxSkinGetElementSize(SkinInfo.PDFViewerNavigationPaneButtonArrow, AScaleFactor);
    AArrowRect.Left := AButtonRect.Right - ASize.cx;
    AArrowRect.Top := cxRectCenter(AButtonRect).Y - ASize.cy div 2;
    AArrowRect := cxRectSetSize(AArrowRect, ASize);
    DrawSkinElement(SkinInfo.PDFViewerNavigationPaneButtonArrow, ACanvas, AArrowRect, AScaleFactor, AElementState);
  end;
end;

procedure TdxSkinLookAndFeelPainter.PDFViewerDrawFindPanelBackground(ACanvas: TcxCanvas; const R: TRect;
  ABorders: TcxBorders);
var
  AMargins: TRect;
begin
  if SkinInfo.PDFViewerFindPanel <> nil then
  begin
    AMargins := SkinInfo.PDFViewerFindPanel.Image.Margins.Margin;
    if bLeft in ABorders then
      AMargins.Left := 0;
    if bTop in ABorders then
      AMargins.Top := 0;
    if bRight in ABorders then
      AMargins.Right := 0;
    if bBottom in ABorders then
      AMargins.Bottom := 0;
    ACanvas.SaveClipRegion;
    try
      ACanvas.IntersectClipRect(R);
      SkinInfo.PDFViewerFindPanel.Draw(ACanvas.Handle, cxRectInflate(R, AMargins));
    finally
      ACanvas.RestoreClipRegion;
    end;
  end
  else
    inherited PDFViewerDrawFindPanelBackground(ACanvas, R, ABorders);
end;

procedure TdxSkinLookAndFeelPainter.PDFViewerDrawPageThumbnailPreviewBackground(ACanvas: TcxCanvas; const ARect: TRect);
begin
  DrawGroupBoxBackground(ACanvas, ARect, ARect);
end;

procedure TdxSkinLookAndFeelPainter.DrawSpreadSheetScaledGroupExpandButton(
  ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState; AScaleFactor: TdxScaleFactor);
begin
  if SkinInfo.EditButtonElements[False] <> nil then
    SkinInfo.EditButtonElements[False].Draw(ACanvas.Handle, R, AScaleFactor, 0, ButtonStateToSkinState[AState])
  else
    inherited DrawSpreadSheetScaledGroupExpandButton(ACanvas, R, AState, AScaleFactor);
end;

procedure TdxSkinLookAndFeelPainter.DrawSpreadSheetScaledGroupExpandButtonGlyph(
  ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState; AExpanded: Boolean;
  AScaleFactor: TdxScaleFactor; ADefaultGlyphs: TCustomImageList = nil);
begin
  if SkinInfo.NavigatorGlyphs <> nil then
    SkinInfo.NavigatorGlyphs.Draw(ACanvas.Handle,
      cxRectCenter(R, NavigatorScaledButtonGlyphSize(AScaleFactor)),
      AScaleFactor, IfThen(AExpanded, 8, 6))
  else
    inherited DrawSpreadSheetScaledGroupExpandButtonGlyph(ACanvas, R, AState, AExpanded, AScaleFactor, ADefaultGlyphs);
end;

procedure TdxSkinLookAndFeelPainter.DrawSpreadSheetScaledHeader(ACanvas: TcxCanvas; const ABounds, ATextAreaBounds: TRect;
  ANeighbors: TcxNeighbors; ABorders: TcxBorders; AState: TcxButtonState; AAlignmentHorz: TAlignment;
  AAlignmentVert: TcxAlignmentVert; AMultiLine, AShowEndEllipsis: Boolean; const AText: string; AFont: TFont;
  ATextColor, ABkColor: TColor; AScaleFactor: TdxScaleFactor; AOnDrawBackground: TcxDrawBackgroundEvent = nil;
  AIsLast: Boolean = False; AIsGroup: Boolean = False);
begin
  DrawScaledHeader(ACanvas, ABounds, ATextAreaBounds, ANeighbors, ABorders, AState, AAlignmentHorz, AAlignmentVert,
    AMultiLine, AShowEndEllipsis, AText, AFont, ATextColor, ABkColor, AScaleFactor, AOnDrawBackground, AIsLast, AIsGroup);
end;

function TdxSkinLookAndFeelPainter.SpreadSheetContentColor: TColor;
begin
  if IsColorPropertyAssigned(SkinInfo.SpreadSheetContentColor) then
    Result := SkinInfo.SpreadSheetContentColor.Value
  else
    Result := inherited SpreadSheetContentColor;
end;

function TdxSkinLookAndFeelPainter.SpreadSheetContentTextColor: TColor;
begin
  if IsColorPropertyAssigned(SkinInfo.SpreadSheetContentTextColor) then
    Result := SkinInfo.SpreadSheetContentTextColor.Value
  else
    Result := inherited SpreadSheetContentTextColor;
end;

function TdxSkinLookAndFeelPainter.SpreadSheetFrozenPaneSeparatorColor: TColor;
begin
  if IsColorPropertyAssigned(SkinInfo.SpreadSheetFrozenPaneSeparatorColor) then
    Result := SkinInfo.SpreadSheetFrozenPaneSeparatorColor.Value
  else
    Result := inherited SpreadSheetFrozenPaneSeparatorColor;
end;

function TdxSkinLookAndFeelPainter.SpreadSheetScaledGroupExpandButtonContentOffsets(AScaleFactor: TdxScaleFactor): TRect;
begin
  if SkinInfo.EditButtonElements[False] <> nil then
    Result := AScaleFactor.Apply(SkinInfo.EditButtonElements[False].ContentOffset.Rect)
  else
    Result := inherited SpreadSheetScaledGroupExpandButtonContentOffsets(AScaleFactor);
end;

function TdxSkinLookAndFeelPainter.SpreadSheetGroupExpandButtonTextColor(AState: TcxButtonState): TColor;
begin
  if SkinInfo.EditButtonElements[False] <> nil then
    Result := SkinInfo.EditButtonElements[False].GetTextColor(AState)
  else
    Result := clDefault;

  if Result = clDefault then
    Result := SpreadSheetGroupExpandButtonTextColor(AState);
end;

function TdxSkinLookAndFeelPainter.SpreadSheetGroupLineColor: TColor;
begin
  if IsColorPropertyAssigned(SkinInfo.SpreadSheetGroupLineColor) then
    Result := SkinInfo.SpreadSheetGroupLineColor.Value
  else
    Result := inherited SpreadSheetGroupLineColor;
end;

function TdxSkinLookAndFeelPainter.SpreadSheetSelectionColor: TColor;
begin
  if IsColorPropertyAssigned(SkinInfo.SpreadSheetSelectionColor) then
    Result := SkinInfo.SpreadSheetSelectionColor.Value
  else
    Result := inherited SpreadSheetSelectionColor;
end;

procedure TdxSkinLookAndFeelPainter.DrawSpreadSheetFormulaBarScaledExpandButton(ACanvas: TcxCanvas;
  const R: TRect; AState: TcxButtonState; AExpanded: Boolean; AScaleFactor: TdxScaleFactor);
var
  AElement: TdxSkinElement;
begin
  ACanvas.FillRect(R, DefaultEditorBackgroundColorEx(esckNormal));
  DrawScaledEditorButton(ACanvas, cxRectInflate(R, 1), cxbkEditorBtn, AState, AScaleFactor);

  AElement := SkinInfo.EditButtonGlyphs[cxbkComboBtn];
  if AElement <> nil then
  begin
    dxGPPaintCanvas.BeginPaint(ACanvas.Handle, R);
    try
      if AExpanded then
      begin
        dxGPPaintCanvas.SaveWorldTransform;
        dxGPPaintCanvas.FlipWorldTransform(False, True, R.Left, (R.Top + R.Bottom) / 2);
      end;
      AElement.DrawEx(dxGPPaintCanvas, R, AScaleFactor, 0, ButtonStateToSkinState[AState]);
      if AExpanded then
        dxGPPaintCanvas.RestoreWorldTransform;
    finally
      dxGPPaintCanvas.EndPaint;
    end;
  end;
end;

procedure TdxSkinLookAndFeelPainter.DrawSpreadSheetFormulaBarScaledSeparator(
  ACanvas: TcxCanvas; const R: TRect; AScaleFactor: TdxScaleFactor);
var
  AElement: TdxSkinElement;
begin
  AElement := SkinInfo.Splitter[False];
  if AElement <> nil then
  begin
    if AElement.Glyph.Empty then
      AElement.Draw(ACanvas.Handle, R, AScaleFactor)
    else
      AElement.Glyph.Draw(ACanvas.Handle, R, AScaleFactor);
  end
  else
    inherited;
end;

procedure TdxSkinLookAndFeelPainter.DrawGroupCaption(ACanvas: TcxCanvas;
  const ACaptionRect, ATextRect: TRect; AElement: TdxSkinElement;
  ATextPadding: TdxSkinIntegerProperty; AState: TdxSkinElementState);
var
  R: TRect;
begin
  AElement.Draw(ACanvas.Handle, ACaptionRect, 0, AState);
  if cxColorIsValid(AElement.Color) and not cxRectIsEmpty(ATextRect) then
  begin
    R := ATextRect;
    if ATextPadding <> nil then
      R := cxRectInflate(R, ATextPadding.Value);
    ACanvas.SaveClipRegion;
    try
      ACanvas.IntersectClipRect(ACaptionRect);
      ACanvas.FillRect(R, AElement.Color);
    finally
      ACanvas.RestoreClipRegion;
    end;
  end;
end;

function TdxSkinLookAndFeelPainter.GridBordersOverlapSize: Integer;
begin
  Result := BorderSize;
end;

function TdxSkinLookAndFeelPainter.GridDrawHeaderCellsFirst: Boolean;
begin
  with SkinInfo do
  begin
    if Header = nil then
      Result := inherited GridDrawHeaderCellsFirst
    else
      Result := not Header.IsAlphaUsed;
  end;
end;

function TdxSkinLookAndFeelPainter.GridGroupRowStyleOffice11ContentColor(AHasData: Boolean): TColor;
begin
  if SkinInfo.GridGroupRowStyleOffice11ContentColor = nil then
    Result := inherited GridGroupRowStyleOffice11ContentColor(AHasData)
  else
    Result := SkinInfo.GridGroupRowStyleOffice11ContentColor.Value;
end;

function TdxSkinLookAndFeelPainter.GridGroupRowStyleOffice11SeparatorColor: TColor;
begin
  if SkinInfo.GridGroupRowStyleOffice11SeparatorColor = nil then
    Result := inherited GridGroupRowStyleOffice11SeparatorColor
  else
    Result := SkinInfo.GridGroupRowStyleOffice11SeparatorColor.Value;
end;

function TdxSkinLookAndFeelPainter.GridGroupRowStyleOffice11TextColor: TColor;
begin
  if SkinInfo.GridGroupRowStyleOffice11TextColor = nil then
    Result := inherited GridGroupRowStyleOffice11TextColor
  else
    Result := SkinInfo.GridGroupRowStyleOffice11TextColor.Value;
end;

function TdxSkinLookAndFeelPainter.GridLikeControlContentColor: TColor;
begin
  if IsColorPropertyAssigned(SkinInfo.GridLikeControlContentColor) then
    Result := SkinInfo.GridLikeControlContentColor.Value
  else
    Result := inherited GridLikeControlContentColor;
end;

function TdxSkinLookAndFeelPainter.GridLikeControlContentEvenColor: TColor;
begin
  if IsColorPropertyAssigned(SkinInfo.GridLikeControlContentEvenColor) then
    Result := SkinInfo.GridLikeControlContentEvenColor.Value
  else
    Result := inherited GridLikeControlContentEvenColor;
end;

function TdxSkinLookAndFeelPainter.GridLikeControlContentOddColor: TColor;
begin
  if IsColorPropertyAssigned(SkinInfo.GridLikeControlContentOddColor) then
    Result := SkinInfo.GridLikeControlContentOddColor.Value
  else
    Result := inherited GridLikeControlContentOddColor;
end;

function TdxSkinLookAndFeelPainter.GridLikeControlContentTextColor: TColor;
begin
  if IsColorPropertyAssigned(SkinInfo.GridLikeControlContentTextColor) then
    Result := SkinInfo.GridLikeControlContentTextColor.Value
  else
    Result := inherited GridLikeControlContentTextColor;
end;

function TdxSkinLookAndFeelPainter.GridLikeControlBackgroundColor: TColor;
begin
  if IsColorPropertyAssigned(SkinInfo.GridLikeControlBackgroundColor) then
    Result := SkinInfo.GridLikeControlBackgroundColor.Value
  else
    Result := inherited GridLikeControlBackgroundColor;
end;

procedure TdxSkinLookAndFeelPainter.LayoutViewDrawRecordCaption(ACanvas: TcxCanvas; const ABounds, ATextRect: TRect;
  APosition: TcxGroupBoxCaptionPosition; AState: TcxButtonState; AColor: TColor = clDefault; const ABitmap: TBitmap = nil);
begin
  if SkinInfo.LayoutViewRecordCaptionElements[APosition] = nil then
    inherited LayoutViewDrawRecordCaption(ACanvas, ABounds, ATextRect, APosition, AState, AColor, ABitmap)
  else
    DrawGroupCaption(ACanvas, ABounds, ATextRect,
      SkinInfo.LayoutViewRecordCaptionElements[APosition],
      SkinInfo.LayoutViewRecordCaptionTextPadding[APosition],
      LayoutViewRecordState[AState]);
end;

procedure TdxSkinLookAndFeelPainter.LayoutViewDrawRecordContent(
  ACanvas: TcxCanvas; const ABounds: TRect; ACaptionPosition: TcxGroupBoxCaptionPosition;
  AState: TcxButtonState; ABorders: TcxBorders = cxBordersAll);
begin
  if not DrawSkinElement(SkinInfo.LayoutViewRecordElements[ACaptionPosition], ACanvas, ABounds, LayoutViewRecordState[AState]) then
    inherited LayoutViewDrawRecordContent(ACanvas, ABounds, ACaptionPosition, AState, ABorders);
end;

procedure TdxSkinLookAndFeelPainter.LayoutViewDrawScaledRecordExpandButton(
  ACanvas: TcxCanvas; const ABounds: TRect; AState: TcxButtonState; AExpanded: Boolean; AScaleFactor: TdxScaleFactor);
begin
  if not DrawSkinElement(SkinInfo.LayoutViewRecordExpandButton, ACanvas, ABounds, AScaleFactor, ButtonStateToSkinState[AState], Ord(not AExpanded)) then
    inherited LayoutViewDrawScaledRecordExpandButton(ACanvas, ABounds, AState, AExpanded, AScaleFactor);
end;

procedure TdxSkinLookAndFeelPainter.LayoutViewDrawItem(
  ACanvas: TcxCanvas; const ABounds: TRect; AState: TcxButtonState; ABorders: TcxBorders = []);
begin
  if SkinInfo.LayoutViewItem = nil then
    inherited LayoutViewDrawItem(ACanvas, ABounds, AState, ABorders)
  else
    if AState <> cxbsNormal then
      SkinInfo.LayoutViewItem.Draw(ACanvas.Handle, ABounds, 0, ButtonStateToSkinState[AState])
    else
      if ABorders <> [] then
      begin
        ACanvas.SaveClipRegion;
        try
          ACanvas.IntersectClipRect(ABounds);
          SkinInfo.LayoutViewItem.Draw(ACanvas.Handle,
            cxRectExcludeBorders(ABounds, SkinInfo.LayoutViewItem.Image.Margins.Margin, ABorders), 0,
            ButtonStateToSkinState[AState]);
        finally
          ACanvas.RestoreClipRegion;
        end;
      end;
end;

function TdxSkinLookAndFeelPainter.LayoutViewGetPadding(AElement: TcxLayoutElement): TRect;
begin
  if SkinInfo.LayoutViewElementPadding[AElement] = nil then
    Result := inherited LayoutViewGetPadding(AElement)
  else
    Result := SkinInfo.LayoutViewElementPadding[AElement].Value.Rect;
end;

function TdxSkinLookAndFeelPainter.LayoutViewGetSpacing(AElement: TcxLayoutElement): TRect;
begin
  if SkinInfo.LayoutViewElementSpacing[AElement] = nil then
    Result := inherited LayoutViewGetSpacing(AElement)
  else
    Result := SkinInfo.LayoutViewElementSpacing[AElement].Value.Rect;
end;

function TdxSkinLookAndFeelPainter.LayoutViewRecordCaptionTailSize(ACaptionPosition: TcxGroupBoxCaptionPosition): Integer;
var
  AProperty: TdxSkinIntegerProperty;
begin
  AProperty := SkinInfo.LayoutViewRecordCaptionTailSizes[ACaptionPosition];
  if AProperty = nil then
    Result := inherited LayoutViewRecordCaptionTailSize(ACaptionPosition)
  else
    Result := AProperty.Value;
end;

procedure TdxSkinLookAndFeelPainter.WinExplorerViewDrawGroup(ACanvas: TcxCanvas; const ABounds: TRect; AState: TcxButtonState;
  AColor: TColor = clDefault; const ABitmap: TBitmap = nil);
begin
  if not DrawSkinElement(SkinInfo.GridWinExplorerViewGroup, ACanvas, ABounds, ButtonStateToSkinState[AState]) then
    inherited WinExplorerViewDrawGroup(ACanvas, ABounds, AState);
end;

procedure TdxSkinLookAndFeelPainter.WinExplorerViewDrawGroupCaptionLine(ACanvas: TcxCanvas; const ABounds: TRect);
begin
  if not DrawSkinElement(SkinInfo.GridWinExplorerViewGroupCaptionLine, ACanvas, ABounds) then
    inherited WinExplorerViewDrawGroupCaptionLine(ACanvas, ABounds);
end;

procedure TdxSkinLookAndFeelPainter.WinExplorerViewDrawRecord(ACanvas: TcxCanvas; const ABounds: TRect; AState: TcxButtonState;
  AColor: TColor = clDefault; const ABitmap: TBitmap = nil);
begin
  if not DrawSkinElement(SkinInfo.GridWinExplorerViewRecord, ACanvas, ABounds, ButtonStateToSkinState[AState]) then
    inherited WinExplorerViewDrawRecord(ACanvas, ABounds, AState);
end;

procedure TdxSkinLookAndFeelPainter.WinExplorerViewDrawScaledRecordExpandButton(
  ACanvas: TcxCanvas; const ABounds: TRect; AState: TcxButtonState; AExpanded: Boolean; AScaleFactor: TdxScaleFactor);
begin
  if not DrawSkinElement(SkinInfo.GridWinExplorerViewGroupExpandButton, ACanvas, ABounds,
    AScaleFactor, ButtonStateToSkinState[AState], Ord(not AExpanded))
  then
    inherited WinExplorerViewDrawScaledRecordExpandButton(ACanvas, ABounds, AState, AExpanded, AScaleFactor);
end;

function TdxSkinLookAndFeelPainter.WinExplorerViewGroupCaptionLineHeight: Integer;
begin
  if SkinInfo.GridWinExplorerViewGroupCaptionLine = nil then
    Result := inherited WinExplorerViewGroupCaptionLineHeight
  else
    Result := SkinInfo.GridWinExplorerViewGroupCaptionLine.MinSize.Height;
end;

function TdxSkinLookAndFeelPainter.WinExplorerViewGroupTextColor(AState: TcxButtonState): TColor;
begin
  if SkinInfo.GridWinExplorerViewGroup <> nil then
  begin
    Result := SkinInfo.GridWinExplorerViewGroup.GetTextColor(AState);
    if Result = clDefault then
      Result := DefaultContentTextColor;
  end
  else
    Result := inherited WinExplorerViewGroupTextColor(AState);
end;

function TdxSkinLookAndFeelPainter.WinExplorerViewGroupTextBold: Boolean;
begin
  Result := IsFontBold(SkinInfo.GridWinExplorerViewGroup);
end;

function TdxSkinLookAndFeelPainter.WinExplorerViewRecordTextColor(AState: TcxButtonState): TColor;
begin
  if SkinInfo.GridWinExplorerViewRecord = nil then
    Result := inherited WinExplorerViewRecordTextColor(AState)
  else
  begin
    Result := SkinInfo.GridWinExplorerViewRecord.GetTextColor(AState);
    if Result = clDefault then
      Result := DefaultContentTextColor;
  end;
end;

function TdxSkinLookAndFeelPainter.PivotGridHeadersAreaColor: TColor;
begin
  if IsSkinElementColorAssigned(SkinInfo.GridGroupByBox) then
    Result := SkinInfo.GridGroupByBox.Color
  else
    Result := inherited PivotGridHeadersAreaColor;
end;

function TdxSkinLookAndFeelPainter.PivotGridHeadersAreaTextColor: TColor;
begin
  if IsSkinElementTextColorAssigned(SkinInfo.GridGroupByBox) then
    Result := SkinInfo.GridGroupByBox.TextColor
  else
    Result := inherited PivotGridHeadersAreaColor
end;

procedure TdxSkinLookAndFeelPainter.DrawFooterCellBorder(ACanvas: TcxCanvas; const R: TRect);
begin
  if SkinInfo.FooterCell = nil then
    inherited DrawFooterCellBorder(ACanvas, R);
end;

procedure TdxSkinLookAndFeelPainter.DrawFooterCellContent(ACanvas: TcxCanvas; const ABounds: TRect;
  AAlignmentHorz: TAlignment; AAlignmentVert: TcxAlignmentVert; AMultiLine: Boolean; const AText: string;
  AFont: TFont; ATextColor, ABkColor: TColor; AOnDrawBackground: TcxDrawBackgroundEvent = nil);
begin
  if SkinInfo.FooterCell <> nil then
    SkinInfo.FooterCell.Draw(ACanvas.Handle, ABounds);
  inherited DrawFooterCellContent(ACanvas, ABounds, AAlignmentHorz,
    AAlignmentVert, AMultiLine, AText, AFont, ATextColor, ABkColor, AOnDrawBackground);
end;

procedure TdxSkinLookAndFeelPainter.DrawFooterContent(
  ACanvas: TcxCanvas; const ARect: TRect; const AViewParams: TcxViewParams);
begin
  if (SkinInfo.FooterPanel = nil) or (AViewParams.Bitmap <> nil) and not AViewParams.Bitmap.Empty then
    inherited DrawFooterContent(ACanvas, ARect, AViewParams)
  else
  begin
    ACanvas.SaveClipRegion;
    try
      ACanvas.IntersectClipRect(ARect);
      SkinInfo.FooterPanel.Draw(ACanvas.Handle, cxRectInflate(ARect, FooterBorderSize));
    finally
      ACanvas.RestoreClipRegion;
    end;
  end;
end;

function TdxSkinLookAndFeelPainter.FooterCellBorderSize: Integer;
begin
  if SkinInfo.FooterCell = nil then
    Result := inherited FooterCellBorderSize
  else
    with SkinInfo.FooterCell.ContentOffset do
      Result := Max(Max(Left, Top), Max(Right, Bottom));
end;

function TdxSkinLookAndFeelPainter.FooterDrawCellsFirst: Boolean;
begin
  Result := False;
end;

function TdxSkinLookAndFeelPainter.FooterSeparatorColor: TColor;
begin
  Result := DefaultGridLineColor;
end;

procedure TdxSkinLookAndFeelPainter.DrawScaledFilterDropDownButton(
  ACanvas: TcxCanvas; R: TRect; AState: TcxButtonState; AIsFilterActive: Boolean; AScaleFactor: TdxScaleFactor);
begin
  if not DrawSkinElement(SkinInfo.FilterButtons[AIsFilterActive], ACanvas, R, AScaleFactor, ButtonStateToSkinState[AState]) then
    inherited DrawScaledFilterDropDownButton(ACanvas, R, AState, AIsFilterActive, AScaleFactor);
end;

procedure TdxSkinLookAndFeelPainter.DrawScaledFilterCloseButton(
  ACanvas: TcxCanvas; R: TRect; AState: TcxButtonState; AScaleFactor: TdxScaleFactor);
begin
  if not DrawSkinElement(SkinInfo.EditButtonElements[True], ACanvas, R, AScaleFactor, ButtonStateToSkinState[AState]) then
    inherited DrawScaledFilterCloseButton(ACanvas, R, AState, AScaleFactor);
end;

procedure TdxSkinLookAndFeelPainter.DrawFilterPanel(ACanvas: TcxCanvas; const ARect: TRect;
  ATransparent: Boolean; ABackgroundColor: TColor; const ABackgroundBitmap: TGraphic);
begin
  if not DrawSkinElement(SkinInfo.FilterPanel, ACanvas, ARect) then
    inherited DrawFilterPanel(ACanvas, ARect, ATransparent, ABackgroundColor, ABackgroundBitmap);
end;

procedure TdxSkinLookAndFeelPainter.DrawPanelBorders(ACanvas: TcxCanvas; const ABorderRect: TRect);
var
  AElement: TdxSkinElement;
begin
  AElement := SkinInfo.GroupBoxElements[cxgpCenter];
  if AElement <> nil then
  begin
    ACanvas.SaveClipRegion;
    try
      ACanvas.ExcludeClipRect(cxRectContent(ABorderRect, AElement.ContentOffset.Rect));
      AElement.Draw(ACanvas.Handle, ABorderRect);
    finally
      ACanvas.RestoreClipRegion;
    end;
  end
  else
    inherited DrawPanelBorders(ACanvas, ABorderRect)
end;

procedure TdxSkinLookAndFeelPainter.DrawPanelContent(ACanvas: TcxCanvas; const ARect: TRect; ADrawBorders: Boolean);
begin
  if SkinInfo.GroupBoxClient <> nil then
  begin
    SkinInfo.GroupBoxClient.Draw(ACanvas.Handle, ARect);
    if ADrawBorders then
      DrawPanelBorders(ACanvas, ARect);
  end
  else
    inherited DrawPanelContent(ACanvas, ARect, ADrawBorders)
end;

function TdxSkinLookAndFeelPainter.PanelBorderSize: TRect;
begin
  if SkinInfo.GroupBoxElements[cxgpCenter] <> nil then
    Result := SkinInfo.GroupBoxElements[cxgpCenter].ContentOffset.Rect
  else
    Result := inherited PanelBorderSize;
end;

function TdxSkinLookAndFeelPainter.PanelTextColor: TColor;
begin
  if SkinInfo.GroupBoxClient <> nil then
    Result := SkinInfo.GroupBoxClient.TextColor
  else
    Result := inherited PanelTextColor;
end;

// ToggleSwitch

procedure TdxSkinLookAndFeelPainter.DrawScaledToggleSwitchState(ACanvas: TcxCanvas;
  ABounds: TRect; AState: TcxButtonState; AChecked: Boolean; AScaleFactor: TdxScaleFactor);
const
  CheckedToSkinElementState: array[Boolean] of TdxSkinElementState = (esNormal, esActive);
begin
  if SkinInfo.ToggleSwitch <> nil then
  begin
    if AState = cxbsDisabled then
      SkinInfo.ToggleSwitch.Draw(ACanvas.Handle, ABounds, AScaleFactor, 1, esDisabled)
    else
      SkinInfo.ToggleSwitch.Draw(ACanvas.Handle, ABounds, AScaleFactor, 1, CheckedToSkinElementState[AChecked]);
  end
  else
    inherited DrawScaledToggleSwitchState(ACanvas, ABounds, AState, AChecked, AScaleFactor);
end;

procedure TdxSkinLookAndFeelPainter.DrawScaledToggleSwitchThumb(
  ACanvas: TcxCanvas; ABounds: TRect; AState: TcxButtonState; AScaleFactor: TdxScaleFactor);
begin
  if SkinInfo.ToggleSwitchThumb <> nil then
    SkinInfo.ToggleSwitchThumb.Draw(ACanvas.Handle, ABounds, AScaleFactor, 0, ButtonStateToSkinState[AState])
  else
    inherited DrawScaledToggleSwitchThumb(ACanvas, ABounds, AState, AScaleFactor);
end;

function TdxSkinLookAndFeelPainter.GetToggleSwitchColorPalette: IdxColorPalette;
begin
  Result := GetSkinElementColorPalette(SkinInfo.ToggleSwitch, esNormal);
end;

function TdxSkinLookAndFeelPainter.GetToggleSwitchThumbPercentsWidth: Integer;
begin
  if SkinInfo.ToggleSwitchThumb <> nil then
    Result := SkinInfo.ToggleSwitchThumb.MinSize.Width
  else
    Result := inherited GetToggleSwitchThumbPercentsWidth;
end;

function TdxSkinLookAndFeelPainter.GetToggleSwitchTextColor: TColor;
begin
  if SkinInfo.ToggleSwitch <> nil then
    Result := SkinInfo.ToggleSwitch.TextColor
  else
    Result := inherited GetToggleSwitchThumbPercentsWidth;
end;

function TdxSkinLookAndFeelPainter.ScaledFilterActivateButtonSize(AScaleFactor: TdxScaleFactor): TPoint;
begin
  if SkinInfo.CheckboxElement <> nil then
    Result := AScaleFactor.Apply(cxPoint(SkinInfo.CheckboxElement.Size))
  else
    Result := inherited ScaledFilterActivateButtonSize(AScaleFactor);
end;

function TdxSkinLookAndFeelPainter.ScaledFilterCloseButtonSize(AScaleFactor: TdxScaleFactor): TPoint;
begin
  if SkinInfo.EditButtonElements[True] <> nil then
    Result := AScaleFactor.Apply(cxPoint(SkinInfo.EditButtonElements[True].MinSize.Size))
  else
    Result := inherited ScaledFilterCloseButtonSize(AScaleFactor);
end;

function TdxSkinLookAndFeelPainter.ScaledFilterSmartTagSize(AScaleFactor: TdxScaleFactor): TSize;
begin
  if SkinInfo.SmartFilterButton <> nil then
    Result := AScaleFactor.Apply(SkinInfo.SmartFilterButton.MinSize.Size)
  else
    Result := inherited ScaledFilterSmartTagSize(AScaleFactor);
end;

procedure TdxSkinLookAndFeelPainter.DrawScaledFilterSmartTag(ACanvas: TcxCanvas;
  R: TRect; AState: TcxFilterSmartTagState; AIsFilterActive: Boolean; AScaleFactor: TdxScaleFactor);
const
  StatesMap: array[Boolean, TcxFilterSmartTagState] of TdxSkinElementState = (
    (esNormal, esHot, esPressed, esNormal),
    (esChecked, esHot, esPressed, esActive)
  );
begin
  if not DrawSkinElement(SkinInfo.SmartFilterButton, ACanvas, R, AScaleFactor, StatesMap[AIsFilterActive, AState]) then
    inherited DrawScaledFilterSmartTag(ACanvas, R, AState, AIsFilterActive, AScaleFactor)
end;

function TdxSkinLookAndFeelPainter.FilterControlMenuGetColorPalette: IdxColorPalette;
begin
  Result := GetSkinElementColorPalette(SkinInfo.PopupMenu, esNormal);
end;

function TdxSkinLookAndFeelPainter.GaugeControlBackgroundColor: TColor;
begin
  if IsSkinElementColorAssigned(SkinInfo.GaugeBackground) then
    Result := SkinInfo.GaugeBackground.Color
  else
    Result := inherited GaugeControlBackgroundColor;
end;

procedure TdxSkinLookAndFeelPainter.DrawGaugeControlBackground(ACanvas: TcxCanvas; const ARect: TRect;
  ATransparent: Boolean; ABackgroundColor: TColor);
begin
  inherited DrawBackground(ACanvas, ARect, ATransparent, GaugeControlBackgroundColor, nil);
end;

function TdxSkinLookAndFeelPainter.MapControlBackgroundColor: TColor;
begin
  Result := SkinInfo.MapControlBackColor;
end;

function TdxSkinLookAndFeelPainter.MapControlPanelBackColor: TdxAlphaColor;
begin
  Result := SkinInfo.MapControlPanelBackColor;
end;

function TdxSkinLookAndFeelPainter.MapControlPanelHotTrackedTextColor: TdxAlphaColor;
begin
  Result := SkinInfo.MapControlPanelHotTrackedTextColor;
end;

function TdxSkinLookAndFeelPainter.MapControlPanelPressedTextColor: TdxAlphaColor;
begin
  Result := SkinInfo.MapControlPanelPressedTextColor;
end;

function TdxSkinLookAndFeelPainter.MapControlPanelTextColor: TdxAlphaColor;
begin
  Result := SkinInfo.MapControlPanelTextColor;
end;

function TdxSkinLookAndFeelPainter.MapControlSelectedRegionBackgroundColor: TdxAlphaColor;
begin
  Result := SkinInfo.MapControlSelectedRegionBackgroundColor;
end;

function TdxSkinLookAndFeelPainter.MapControlSelectedRegionBorderColor: TdxAlphaColor;
begin
  Result := SkinInfo.MapControlSelectedRegionBorderColor;
end;

function TdxSkinLookAndFeelPainter.MapControlShapeBorderColor: TdxAlphaColor;
begin
  Result := SkinInfo.MapControlShapeBorderColor[cxbsNormal];
end;

function TdxSkinLookAndFeelPainter.MapControlShapeBorderHighlightedColor: TdxAlphaColor;
begin
  Result := SkinInfo.MapControlShapeBorderColor[cxbsHot];
end;

function TdxSkinLookAndFeelPainter.MapControlShapeBorderHighlightedWidth(AScaleFactor: TdxScaleFactor): Integer;
begin
  Result := AScaleFactor.Apply(SkinInfo.MapControlShapeBorderWidth[cxbsHot]);
end;

function TdxSkinLookAndFeelPainter.MapControlShapeBorderSelectedColor: TdxAlphaColor;
begin
  Result := SkinInfo.MapControlShapeBorderColor[cxbsPressed];
end;

function TdxSkinLookAndFeelPainter.MapControlShapeBorderSelectedWidth(AScaleFactor: TdxScaleFactor): Integer;
begin
  Result := AScaleFactor.Apply(SkinInfo.MapControlShapeBorderWidth[cxbsPressed]);
end;

function TdxSkinLookAndFeelPainter.MapControlShapeBorderWidth(AScaleFactor: TdxScaleFactor): Integer;
begin
  Result := AScaleFactor.Apply(SkinInfo.MapControlShapeBorderWidth[cxbsNormal]);
end;

function TdxSkinLookAndFeelPainter.MapControlShapeColor: TdxAlphaColor;
begin
  Result := SkinInfo.MapControlShapeColor[cxbsNormal];
end;

function TdxSkinLookAndFeelPainter.MapControlShapeHighlightedColor: TdxAlphaColor;
begin
  Result := SkinInfo.MapControlShapeColor[cxbsHot];
end;

function TdxSkinLookAndFeelPainter.MapControlShapeSelectedColor: TdxAlphaColor;
begin
  Result := SkinInfo.MapControlShapeColor[cxbsPressed];
end;

function TdxSkinLookAndFeelPainter.MapControlGetMapPushpinSize(AScaleFactor: TdxScaleFactor): TSize;
begin
  Result := AScaleFactor.Apply(SkinInfo.MapControlPushpin.Size);
end;

function TdxSkinLookAndFeelPainter.MapControlGetMapPushpinTextOrigin(AScaleFactor: TdxScaleFactor): TPoint;
begin
  Result := AScaleFactor.Apply(SkinInfo.MapControlPushpinTextOrigin);
end;

function TdxSkinLookAndFeelPainter.MapControlMapCustomElementSelectionOffset(AScaleFactor: TdxScaleFactor): TRect;
begin
  Result := AScaleFactor.Apply(SkinInfo.MapControlCustomElement.ContentOffset.Rect);
end;

function TdxSkinLookAndFeelPainter.MapControlMapCustomElementTextColor: TdxAlphaColor;
begin
  Result := dxColorToAlphaColor(SkinInfo.MapControlCustomElement.TextColor, 255);
end;

function TdxSkinLookAndFeelPainter.MapControlMapCustomElementTextGlowColor: TdxAlphaColor;
begin
  Result := SkinInfo.MapControlCustomElementTextGlowColor;
end;

function TdxSkinLookAndFeelPainter.MapControlMapPushpinTextColor: TdxAlphaColor;
begin
  Result := dxColorToAlphaColor(SkinInfo.MapControlPushpin.TextColor, 255);
end;

function TdxSkinLookAndFeelPainter.MapControlMapPushpinTextGlowColor: TdxAlphaColor;
begin
  Result := SkinInfo.MapControlPushpinTextGlowColor;
end;

procedure TdxSkinLookAndFeelPainter.DrawMapCustomElementBackground(ACanvas: TcxCanvas;
  const ARect: TRect; AState: TdxMapControlElementState);
const
  MapElementStateToSkinElementState: array [TdxMapControlElementState] of TdxSkinElementState =
    (esNormal, esHot, esPressed, esPressed, esDisabled);
begin
  if AState in [mcesHot, mcesPressed, mcesSelected] then
    SkinInfo.MapControlCustomElement.Draw(ACanvas.Handle, ARect, 0, MapElementStateToSkinElementState[AState]);
end;

procedure TdxSkinLookAndFeelPainter.DrawMapPushpin(ACanvas: TcxCanvas; const ARect: TRect;
  AState: TdxMapControlElementState; AScaleFactor: TdxScaleFactor);
begin
  SkinInfo.MapControlPushpin.Draw(ACanvas.Handle, ARect, AScaleFactor);
end;

procedure TdxSkinLookAndFeelPainter.DrawTrackBarScaledTrack(ACanvas: TcxCanvas; const ARect, ASelection: TRect;
  AShowSelection, AEnabled, AHorizontal: Boolean; ATrackColor: TColor; AScaleFactor: TdxScaleFactor);
begin
  if SkinInfo.TrackBarTrack[AHorizontal] = nil then
    inherited DrawTrackBarScaledTrack(ACanvas, ARect, ASelection, AShowSelection, AEnabled, AHorizontal, ATrackColor, AScaleFactor)
  else
  begin
    SkinInfo.TrackBarTrack[AHorizontal].Draw(ACanvas.Handle, ARect, AScaleFactor, 2 * Byte(not AEnabled));
    if AShowSelection then
    begin
      ACanvas.SaveClipRegion;
      try
        ACanvas.IntersectClipRect(ASelection);
        SkinInfo.TrackBarTrack[AHorizontal].Draw(ACanvas.Handle, ARect, AScaleFactor, 2 * Byte(not AEnabled) + 1);
      finally
        ACanvas.RestoreClipRegion;
      end;
    end;
  end;
end;

procedure TdxSkinLookAndFeelPainter.DrawTrackBarScaledThumb(ACanvas: TcxCanvas; const ARect: TRect; AState: TcxButtonState;
  AHorizontal: Boolean; ATicks: TcxTrackBarTicksAlign; AThumbColor: TColor; AScaleFactor: TdxScaleFactor);
begin
  if not DrawSkinElement(SkinInfo.TrackBarThumb[AHorizontal, ATicks], ACanvas, ARect, AScaleFactor, ButtonStateToSkinState[AState]) then
    inherited DrawTrackBarScaledThumb(ACanvas, ARect, AState, AHorizontal, ATicks, AThumbColor, AScaleFactor);
end;

function TdxSkinLookAndFeelPainter.TrackBarScaledThumbSize(AHorizontal: Boolean; AScaleFactor: TdxScaleFactor): TSize;
begin
  if SkinInfo.TrackBarThumb[AHorizontal, tbtaDown] <> nil then
    Result := AScaleFactor.Apply(SkinInfo.TrackBarThumb[AHorizontal, tbtaDown].Size)
  else
    Result := inherited TrackBarScaledThumbSize(AHorizontal, AScaleFactor);
end;

function TdxSkinLookAndFeelPainter.TrackBarTicksColor(AText: Boolean): TColor;
begin
  if SkinInfo.TrackBarTickColor <> nil then
    Result := SkinInfo.TrackBarTickColor.Value
  else
    Result := inherited TrackBarTicksColor(AText);
end;

function TdxSkinLookAndFeelPainter.TrackBarScaledTrackSize(AScaleFactor: TdxScaleFactor): Integer;
begin
  if SkinInfo.TrackBarTrack[True] <> nil then
    Result := AScaleFactor.Apply(SkinInfo.TrackBarTrack[True].Size.cy)
  else
    Result := inherited TrackBarScaledTrackSize(AScaleFactor);
end;

procedure TdxSkinLookAndFeelPainter.DrawRangeControlScaledLeftThumb(ACanvas: TcxCanvas;
  const ARect: TRect; AColor: TColor; ABorderColor: TdxAlphaColor; AScaleFactor: TdxScaleFactor);
begin
  if not DrawSkinElement(SkinInfo.RangeControlLeftThumb, ACanvas, ARect, AScaleFactor) then
    inherited;
end;

procedure TdxSkinLookAndFeelPainter.DrawRangeControlScaledRightThumb(
  ACanvas: TcxCanvas; const ARect: TRect; AColor: TColor; ABorderColor: TdxAlphaColor; AScaleFactor: TdxScaleFactor);
begin
  if not DrawSkinElement(SkinInfo.RangeControlRightThumb, ACanvas, ARect, AScaleFactor) then
    inherited
end;

procedure TdxSkinLookAndFeelPainter.DrawRangeControlScaledRulerHeader(ACanvas: TcxCanvas;
  const ARect: TRect; AIsHot: Boolean; AColor: TdxAlphaColor; ABorderColor: TdxAlphaColor; AScaleFactor: TdxScaleFactor);
const
  AState: array [Boolean] of TdxSkinElementState = (esNormal, esHot);
begin
  if not DrawSkinElement(SkinInfo.RangeControlRulerHeader, ACanvas, ARect, AScaleFactor, AState[AIsHot]) then
    inherited;
end;

procedure TdxSkinLookAndFeelPainter.DrawRangeControlScaledSizingGlyph(
  ACanvas: TcxCanvas; const ARect: TRect; ABorderColor: TdxAlphaColor; AScaleFactor: TdxScaleFactor);
begin
  if not DrawSkinElement(SkinInfo.RangeControlSizingGlyph, ACanvas, ARect, AScaleFactor) then
    inherited;
end;

function TdxSkinLookAndFeelPainter.GetRangeControlBackColor: TColor;
begin
  if SkinInfo.RangeControlBackColor <> TdxAlphaColors.Default then
    Result := dxAlphaColorToColor(SkinInfo.RangeControlBackColor)
  else
    Result := inherited GetRangeControlBackColor;
end;

function TdxSkinLookAndFeelPainter.GetRangeControlBorderColor: TColor;
begin
  if SkinInfo.RangeControlBorder <> nil then
    Result := SkinInfo.RangeControlBorder.Borders.Left.Color
  else
    Result := inherited GetRangeControlBorderColor;
end;

function TdxSkinLookAndFeelPainter.GetRangeControlDefaultElementColor: TColor;
begin
  if SkinInfo.RangeControlDefaultElementColor <> nil then
    Result := SkinInfo.RangeControlDefaultElementColor.Value
  else
    Result := inherited GetRangeControlDefaultElementColor;
end;

function TdxSkinLookAndFeelPainter.GetRangeControlElementForeColor: TColor;
begin
  if SkinInfo.RangeControlElementForeColor <> nil then
    Result := SkinInfo.RangeControlElementForeColor.Value
  else
    Result := inherited GetRangeControlElementForeColor;
end;

function TdxSkinLookAndFeelPainter.GetRangeControlElementsBorderColor: TdxAlphaColor;
begin
  if SkinInfo.RangeControlInnerBorderColor <> TdxAlphaColors.Default then
    Result := SkinInfo.RangeControlInnerBorderColor
  else
    Result := inherited GetRangeControlElementsBorderColor;
end;

function TdxSkinLookAndFeelPainter.GetRangeControlLabelColor: TColor;
begin
  if SkinInfo.RangeControlLabelColor <> nil then
    Result := SkinInfo.RangeControlLabelColor.Value
  else
    Result := inherited GetRangeControlLabelColor;
end;

function TdxSkinLookAndFeelPainter.GetRangeControlOutOfRangeColor: TdxAlphaColor;
begin
  if SkinInfo.RangeControlOutOfRangeColorMask <> TdxAlphaColors.Default then
    Result := SkinInfo.RangeControlOutOfRangeColorMask
  else
    Result := inherited GetRangeControlOutOfRangeColor;
end;

function TdxSkinLookAndFeelPainter.GetRangeControlRangePreviewColor: TColor;
begin
  if SkinInfo.RangeControlRangePreviewColor <> nil then
    Result := SkinInfo.RangeControlRangePreviewColor.Value
  else
    Result := inherited GetRangeControlRangePreviewColor;
end;

function TdxSkinLookAndFeelPainter.GetRangeControlRulerColor: TdxAlphaColor;
begin
  if SkinInfo.RangeControlRuleColor <> TdxAlphaColors.Default then
    Result := SkinInfo.RangeControlRuleColor
  else
    Result := inherited GetRangeControlRulerColor;
end;

function TdxSkinLookAndFeelPainter.GetRangeControlScrollAreaColor: TColor;
begin
  if SkinInfo.RangeControlScrollAreaColor <> nil then
    Result := SkinInfo.RangeControlScrollAreaColor.Value
  else
    Result := inherited GetRangeControlScrollAreaColor;
end;

function TdxSkinLookAndFeelPainter.GetRangeControlSelectedRegionBackgroundColor: TdxAlphaColor;
begin
  if SkinInfo.RangeControlSelectionColor <> TdxAlphaColors.Default then
    Result := SkinInfo.RangeControlSelectionColor
  else
    Result := inherited GetRangeControlSelectedRegionBackgroundColor;
end;

function TdxSkinLookAndFeelPainter.GetRangeControlSelectedRegionBorderColor: TdxAlphaColor;
begin
  if SkinInfo.RangeControlSelectionBorderColor <> TdxAlphaColors.Default then
    Result := SkinInfo.RangeControlSelectionBorderColor
  else
    Result := inherited GetRangeControlSelectedRegionBorderColor;
end;

function TdxSkinLookAndFeelPainter.GetRangeControlScaledScrollAreaHeight(AScaleFactor: TdxScaleFactor): Integer;
begin
  if SkinInfo.RangeControlScrollAreaHeight <> nil then
    Result := AScaleFactor.Apply(SkinInfo.RangeControlScrollAreaHeight.Value)
  else
    Result := inherited GetRangeControlScaledScrollAreaHeight(AScaleFactor);
end;

function TdxSkinLookAndFeelPainter.GetRangeControlScaledSizingGlyphSize(AScaleFactor: TdxScaleFactor): TSize;
begin
  if SkinInfo.RangeControlSizingGlyph <> nil then
    Result := AScaleFactor.Apply(SkinInfo.RangeControlSizingGlyph.MinSize.Size)
  else
    Result := inherited GetRangeControlScaledSizingGlyphSize(AScaleFactor);
end;

function TdxSkinLookAndFeelPainter.GetRangeControlScaledThumbSize(AScaleFactor: TdxScaleFactor): TSize;
var
  AElement: TdxSkinElement;
begin
  AElement := SkinInfo.RangeControlLeftThumb;
  if AElement <> nil then
    Result := AScaleFactor.Apply(GetSkinElementMinSize(AElement))
  else
    Result := inherited GetRangeControlScaledThumbSize(AScaleFactor);
end;

function TdxSkinLookAndFeelPainter.GetRangeControlViewPortPreviewColor: TColor;
begin
  if SkinInfo.RangeControlViewPortPreviewColor <> nil then
    Result := SkinInfo.RangeControlViewPortPreviewColor.Value
  else
    Result := inherited GetRangeControlViewPortPreviewColor;
end;

function TdxSkinLookAndFeelPainter.GetRangeTrackBarLeftThumbSkinElement(
  AHorizontal: Boolean; ATicks: TcxTrackBarTicksAlign): TdxSkinElement;
begin
  if ATicks = tbtaBoth then
    Result := SkinInfo.RangeTrackBarThumbBoth
  else
    Result := SkinInfo.RangeTrackBarThumbLeft;
end;

function TdxSkinLookAndFeelPainter.GetRangeTrackBarRightThumbSkinElement(
  AHorizontal: Boolean; ATicks: TcxTrackBarTicksAlign): TdxSkinElement;
begin
  if ATicks = tbtaBoth then
    Result := SkinInfo.RangeTrackBarThumbBoth
    else
      Result := SkinInfo.RangeTrackBarThumbRight;
end;

function TdxSkinLookAndFeelPainter.RangeTrackBarScaledLeftThumbSize(
  AHorizontal: Boolean; ATicks: TcxTrackBarTicksAlign; AScaleFactor: TdxScaleFactor): TSize;
var
  ASkinElement: TdxSkinElement;
begin
  ASkinElement := GetRangeTrackBarLeftThumbSkinElement(AHorizontal, ATicks);
  if ASkinElement <> nil then
    Result := AScaleFactor.Apply(ASkinElement.Size)
  else
    Result := inherited RangeTrackBarScaledLeftThumbSize(AHorizontal, ATicks, AScaleFactor);
end;

function TdxSkinLookAndFeelPainter.RangeTrackBarScaledRightThumbSize(
  AHorizontal: Boolean; ATicks: TcxTrackBarTicksAlign; AScaleFactor: TdxScaleFactor): TSize;
var
  ASkinElement: TdxSkinElement;
begin
  ASkinElement := GetRangeTrackBarRightThumbSkinElement(AHorizontal, ATicks);
  if ASkinElement <> nil then
    Result := AScaleFactor.Apply(ASkinElement.Size)
  else
    Result := inherited RangeTrackBarScaledRightThumbSize(AHorizontal, ATicks, AScaleFactor);
end;

procedure TdxSkinLookAndFeelPainter.DrawRangeTrackBarScaledThumbSkinElement(
  ACanvas: TcxCanvas; ASkinElement: TdxSkinElement; const ARect: TRect; AState: TcxButtonState;
  AHorizontal: Boolean; ATicks: TcxTrackBarTicksAlign; AScaleFactor: TdxScaleFactor);
var
  AAngle: TcxRotationAngle;
  AFlipVertically: Boolean;
begin
  if AHorizontal then
  begin
    AAngle := ra0;
    AFlipVertically := ATicks = tbtaUp;
  end
  else
  begin
    AFlipVertically := ATicks <> tbtaUp;
    if ATicks = tbtaUp then
      AAngle := raMinus90
    else
      AAngle := raPlus90;
  end;
  DrawSkinElementRotated(ACanvas, ASkinElement, ARect,
    ButtonStateToSkinState[AState], AAngle, AFlipVertically, AScaleFactor);
end;

procedure TdxSkinLookAndFeelPainter.DrawRangeTrackBarScaledLeftThumb(
  ACanvas: TcxCanvas; const ARect: TRect; AState: TcxButtonState; AHorizontal: Boolean;
  ATicks: TcxTrackBarTicksAlign; AThumbColor: TColor; AScaleFactor: TdxScaleFactor);
var
  ASkinElement: TdxSkinElement;
begin
  ASkinElement := GetRangeTrackBarLeftThumbSkinElement(AHorizontal, ATicks);
  if ASkinElement <> nil then
    DrawRangeTrackBarScaledThumbSkinElement(ACanvas, ASkinElement, ARect, AState, AHorizontal, ATicks, AScaleFactor)
  else
    inherited;
end;

procedure TdxSkinLookAndFeelPainter.DrawRangeTrackBarScaledRightThumb(
  ACanvas: TcxCanvas; const ARect: TRect; AState: TcxButtonState; AHorizontal: Boolean;
  ATicks: TcxTrackBarTicksAlign; AThumbColor: TColor; AScaleFactor: TdxScaleFactor);
var
  ASkinElement: TdxSkinElement;
begin
  ASkinElement := GetRangeTrackBarRightThumbSkinElement(AHorizontal, ATicks);
  if ASkinElement <> nil then
    DrawRangeTrackBarScaledThumbSkinElement(ACanvas, ASkinElement, ARect, AState, AHorizontal, ATicks, AScaleFactor)
  else
    inherited;
end;

procedure TdxSkinLookAndFeelPainter.DrawContent(ACanvas: TcxCanvas;
  const ABounds, ATextAreaBounds: TRect; AState: Integer; AAlignmentHorz: TAlignment;
  AAlignmentVert: TcxAlignmentVert; AMultiLine, AShowEndEllipsis: Boolean;
  const AText: string; AFont: TFont; ATextColor, ABkColor: TColor;
  AOnDrawBackground: TcxDrawBackgroundEvent = nil; AIsFooter: Boolean = False);
const
  AlignmentsHorz: array[TAlignment] of Integer =
    (cxAlignLeft, cxAlignRight, cxAlignHCenter);
  AlignmentsVert: array[TcxAlignmentVert] of Integer =
    (cxAlignTop, cxAlignBottom, cxAlignVCenter);
  MultiLines: array[Boolean] of Integer = (cxSingleLine, cxWordBreak);
  ShowEndEllipsis: array[Boolean] of Integer = (0, cxShowEndEllipsis);
begin
  if AText <> '' then
  begin
    ACanvas.Brush.Style := bsClear;
    ACanvas.Font := AFont;
    ACanvas.Font.Color := ATextColor;
    ACanvas.DrawText(AText, ATextAreaBounds, AlignmentsHorz[AAlignmentHorz] or
      AlignmentsVert[AAlignmentVert] or MultiLines[AMultiLine] or ShowEndEllipsis[AShowEndEllipsis]);
    ACanvas.Brush.Style := bsSolid;
  end;
end;

procedure TdxSkinLookAndFeelPainter.DrawScaledSplitter(ACanvas: TcxCanvas; const ARect: TRect;
  AHighlighted, AClicked, AHorizontal: Boolean; AScaleFactor: TdxScaleFactor; AHasCloseMark: Boolean = False; AArrowDirection: TcxArrowDirection = adLeft);
const
  StateMap: array[Boolean] of TdxSkinElementState = (esNormal, esHot);
  ImageNumber: array[Boolean, TcxArrowDirection] of Integer = ((0, 0, 0, 0), (1, 2, 1, 2));
begin
  if not DrawSkinElement(SkinInfo.Splitter[AHorizontal], ACanvas, ARect, AScaleFactor, StateMap[AHighlighted], ImageNumber[AHasCloseMark, AArrowDirection]) then
    inherited DrawScaledSplitter(ACanvas, ARect, AHighlighted, AClicked, AHorizontal, AScaleFactor, AHasCloseMark, AArrowDirection);
end;

function TdxSkinLookAndFeelPainter.GetScaledSplitterSize(AHorizontal: Boolean; AScaleFactor: TdxScaleFactor): TSize;
var
  AElement: TdxSkinElement;
begin
  AElement := SkinInfo.Splitter[AHorizontal];
  if AElement <> nil then
    Result := AScaleFactor.Apply(AElement.MinSize.Size)
  else
    Result := inherited GetScaledSplitterSize(AHorizontal, AScaleFactor);
end;

function TdxSkinLookAndFeelPainter.GetHintBorderColor: TColor;
begin
  if SkinInfo.ScreenTipWindow <> nil then
    Result := SkinInfo.ScreenTipWindow.Borders.Left.Color
  else
    Result := inherited GetHintBorderColor;
end;

procedure TdxSkinLookAndFeelPainter.DrawHintBackground(ACanvas: TcxCanvas; const ARect: TRect; AColor: TColor = clDefault);
begin
  if SkinInfo.ScreenTipWindow <> nil then
    SkinInfo.ScreenTipWindow.Draw(ACanvas.Handle, cxRectInflate(ARect, SkinInfo.ScreenTipWindow.ContentOffset.Rect))
  else
    inherited DrawHintBackground(ACanvas, ARect, AColor);
end;

function TdxSkinLookAndFeelPainter.ScreenTipGetColorPalette: IdxColorPalette;
begin
  Result := GetSkinElementColorPalette(SkinInfo.ScreenTipWindow, esNormal);
end;

function TdxSkinLookAndFeelPainter.ScreenTipGetDescriptionTextColor: TColor;
begin
  if SkinInfo.ScreenTipItem <> nil then
    Result := SkinInfo.ScreenTipItem.Value
  else
    Result := inherited ScreenTipGetDescriptionTextColor;
end;

function TdxSkinLookAndFeelPainter.ScreenTipGetTitleTextColor: TColor;
begin
  if SkinInfo.ScreenTipTitleItem <> nil then
    Result := SkinInfo.ScreenTipTitleItem.Value
  else
    Result := inherited ScreenTipGetTitleTextColor;
end;

function TdxSkinLookAndFeelPainter.ScreenTipGetFooterLineSize: Integer;
begin
  if SkinInfo.ScreenTipSeparator <> nil then
    Result := SkinInfo.ScreenTipSeparator.Size.cy
  else
    Result := inherited ScreenTipGetFooterLineSize;
end;

procedure TdxSkinLookAndFeelPainter.ScreenTipDrawBackground(ACanvas: TcxCanvas; ARect: TRect);
begin
  if not DrawSkinElement(SkinInfo.ScreenTipWindow, ACanvas, ARect) then
    inherited ScreenTipDrawBackground(ACanvas, ARect);
end;

procedure TdxSkinLookAndFeelPainter.ScreenTipDrawFooterLine(ACanvas: TcxCanvas; const ARect: TRect);
begin
  if not DrawSkinElement(SkinInfo.ScreenTipSeparator, ACanvas, ARect) then
    inherited ScreenTipDrawFooterLine(ACanvas, ARect);
end;

procedure TdxSkinLookAndFeelPainter.DrawScaledIndicatorCustomizationMark(
  ACanvas: TcxCanvas; const R: TRect; AColor: TColor; AScaleFactor: TdxScaleFactor);
begin
  if SkinInfo.IndicatorImages <> nil then
  begin
    SkinInfo.IndicatorImages.Draw(ACanvas.Handle,
      cxRectCenter(R, AScaleFactor.Apply(SkinInfo.IndicatorImages.Image.Size)), AScaleFactor, 2)
  end
  else
    inherited DrawIndicatorCustomizationMark(ACanvas, R, AColor);
end;

procedure TdxSkinLookAndFeelPainter.DrawScaledIndicatorImage(
  ACanvas: TcxCanvas; const R: TRect; AKind: TcxIndicatorKind; AScaleFactor: TdxScaleFactor);

  procedure DrawIndicator(ACanvas: TcxCanvas; const R: TRect; AIndex: Integer);
  begin
    DrawRightToLeftDependentSkinElement(SkinInfo.IndicatorImages, ACanvas, R, AScaleFactor, esNormal, AIndex);
  end;

  procedure DrawGlyph(const R: TRect; AIndex: Integer; AAlpha: Byte);
  var
    ABitmap: TcxBitmap32;
  begin
    if AIndex >= 0 then
    begin
      if AAlpha < MaxByte then
      begin
        ABitmap := TcxBitmap32.CreateSize(R, True);
        ABitmap.cxCanvas.UseRightToLeftAlignment := ACanvas.UseRightToLeftAlignment;
        try
          DrawIndicator(ABitmap.cxCanvas, ABitmap.ClientRect, AIndex);
          cxAlphaBlend(ACanvas.Handle, ABitmap, R, ABitmap.ClientRect, False, AAlpha);
        finally
          ABitmap.Free;
        end;
      end
      else
        DrawIndicator(ACanvas, R,  AIndex);
    end;
  end;

const
  IndicatorImagesMap: array[TcxIndicatorKind] of integer = (-1, 0, 1, 2, 0, 0, 8, 5);
begin
  if SkinInfo.IndicatorImages <> nil then
    DrawGlyph(cxRectCenter(R, AScaleFactor.Apply(SkinInfo.IndicatorImages.Image.Size)),
      IndicatorImagesMap[AKind], IfThen(AKind <> ikMultiDot, 255, 120))
  else
    inherited DrawScaledIndicatorImage(ACanvas, R, AKind, AScaleFactor)
end;

procedure TdxSkinLookAndFeelPainter.DrawScaledIndicatorItem(ACanvas: TcxCanvas; const R, AImageAreaBounds: TRect;
  AKind: TcxIndicatorKind; AColor: TColor; AScaleFactor: TdxScaleFactor; AOnDrawBackground: TcxDrawBackgroundEvent = nil;
  ANeighbors: TcxNeighbors = [nTop, nBottom]);
var
  ARect: TRect;
begin
  ARect := R;
  if nTop in ANeighbors  then
    ARect.Top := R.Top - HeaderBorderSize;
  inherited DrawScaledIndicatorItem(ACanvas, ARect, AImageAreaBounds, AKind, AColor, AScaleFactor, AOnDrawBackground, ANeighbors);
end;

procedure TdxSkinLookAndFeelPainter.DrawScaledIndicatorItemEx(ACanvas: TcxCanvas; const R: TRect;
  AKind: TcxIndicatorKind; AColor: TColor; AScaleFactor: TdxScaleFactor; AOnDrawBackground: TcxDrawBackgroundEvent = nil);
begin
  DrawScaledIndicatorItem(ACanvas, R, AKind, AColor, AScaleFactor, AOnDrawBackground);
end;

procedure TdxSkinLookAndFeelPainter.DrawScaledMonthHeader(ACanvas: TcxCanvas; const ABounds: TRect;
  const AText: string; ANeighbors: TcxNeighbors; const AViewParams: TcxViewParams; AArrows: TcxArrowDirections;
  ASideWidth: Integer; AScaleFactor: TdxScaleFactor; AOnDrawBackground: TcxDrawBackgroundEvent = nil);
begin
  DrawScaledHeader(ACanvas, ABounds, ABounds, ANeighbors, HeaderBorders(ANeighbors), cxbsNormal, taCenter,
    vaCenter, False, False, AText, AViewParams.Font, AViewParams.TextColor, AViewParams.Color, AScaleFactor, AOnDrawBackground);
  DrawMonthHeaderArrows(ACanvas, ABounds, AArrows, ASideWidth, DefaultDateNavigatorHeaderTextColor(False));
end;

procedure TdxSkinLookAndFeelPainter.DrawSchedulerDayHeader(ACanvas: TcxCanvas; const ABounds, ATextAreaBounds: TRect;
  ANeighbors: TcxNeighbors; ABorders: TcxBorders; AState: TcxButtonState;
  AAlignmentHorz: TAlignment; AAlignmentVert: TcxAlignmentVert; AMultiLine, AShowEndEllipsis: Boolean;
  const AText: string; AFont: TFont; ATextColor, ABkColor: TColor;
  AScaleFactor: TdxScaleFactor; AOnDrawBackground: TcxDrawBackgroundEvent = nil;
  AIsLast: Boolean = False; AIsGroup: Boolean = False);
begin
  DrawScaledHeader(ACanvas, ABounds, ATextAreaBounds, ANeighbors, ABorders, AState,
    AAlignmentHorz, AAlignmentVert, AMultiLine, AShowEndEllipsis, AText, AFont, ATextColor, ABkColor,
    AScaleFactor, AOnDrawBackground, AIsLast, AIsGroup);
end;

procedure TdxSkinLookAndFeelPainter.DrawSchedulerEventProgress(
  ACanvas: TcxCanvas; const ABounds, AProgress: TRect; AViewParams: TcxViewParams; ATransparent: Boolean);
var
  AProgressBar: TdxSkinElement;
  AProgressChunk: TdxSkinElement;
begin
  AProgressBar := SkinInfo.ProgressBarElements[False, False];
  AProgressChunk := SkinInfo.ProgressBarElements[True, False];
  if (AProgressBar = nil) or (AProgressChunk = nil) then
    inherited DrawSchedulerEventProgress(ACanvas, ABounds, AProgress, AViewParams, ATransparent)
  else
  begin
    AProgressBar.Draw(ACanvas.Handle, ABounds);
    AProgressChunk.Draw(ACanvas.Handle, AProgress);
  end;
end;

function TdxSkinLookAndFeelPainter.SchedulerEventProgressOffsets: TRect;
var
  AProgressBar: TdxSkinElement;
begin
  AProgressBar := SkinInfo.ProgressBarElements[False, False];
  if AProgressBar = nil then
    Result := inherited SchedulerEventProgressOffsets
  else
    Result := AProgressBar.ContentOffset.Rect
end;

function TdxSkinLookAndFeelPainter.SchedulerNavigationButtonTextColor(
  AIsNextButton: Boolean; AState: TcxButtonState; ADefaultColor: TColor = clDefault): TColor;
var
  AElement: TdxSkinElement;
begin
  AElement := SkinInfo.SchedulerNavigationButtons[AIsNextButton];
  if AElement = nil then
    Result := inherited SchedulerNavigationButtonTextColor(AIsNextButton, AState, ADefaultColor)
  else
  begin
    Result := AElement.GetTextColor(AState);
    if Result = clDefault then
      Result := ADefaultColor;
  end;
end;

procedure TdxSkinLookAndFeelPainter.CalculateSchedulerNavigationButtonRects(
  AIsNextButton: Boolean; ACollapsed: Boolean; APrevButtonTextSize: TSize; ANextButtonTextSize: TSize;
  var ABounds: TRect; out ATextRect: TRect; out AArrowRect: TRect; AScaleFactor: TdxScaleFactor; const AIsVertical: Boolean = True);
var
  AMinSize: TSize;
begin
  AMinSize := cxNullSize;
  if SkinInfo.SchedulerNavigationButtons[AIsNextButton] <> nil then
    AMinSize := SkinInfo.SchedulerNavigationButtons[AIsNextButton].MinSize.Size;
  if SkinInfo.SchedulerNavigationButtonsArrow[AIsNextButton] <> nil then
    AMinSize := cxSizeMax(AMinSize, SkinInfo.SchedulerNavigationButtonsArrow[AIsNextButton].MinSize.Size);

  if AIsVertical then
  begin
    if (AMinSize.cx > 0) and (cxRectWidth(ABounds) < AMinSize.cx) then
    begin
      if AIsNextButton then
        ABounds.Left := ABounds.Right - AMinSize.cx
      else
        ABounds.Right := ABounds.Left + AMinSize.cx
    end;
  end
  else
    if (AMinSize.cx > 0) and (cxRectHeight(ABounds) < AMinSize.cx) then
    begin
      if AIsNextButton then
        ABounds.Top := ABounds.Bottom - AMinSize.cx
      else
        ABounds.Bottom := ABounds.Top + AMinSize.cx
    end;

  inherited CalculateSchedulerNavigationButtonRects(AIsNextButton, ACollapsed,
    APrevButtonTextSize, ANextButtonTextSize, ABounds, ATextRect, AArrowRect, AScaleFactor, AIsVertical);
end;

procedure TdxSkinLookAndFeelPainter.DrawFooterBorderEx(ACanvas: TcxCanvas; const R: TRect; ABorders: TcxBorders);

  function GetContentMargins: TRect;
  begin
    Result := SkinInfo.FooterPanel.ContentOffset.Rect;
    Result.Top := FooterSeparatorSize;
  end;

begin
  if SkinInfo.FooterPanel <> nil then
  begin
    ACanvas.SaveClipRegion;
    try
      ACanvas.ExcludeClipRect(cxRectContent(R, GetContentMargins));
      SkinInfo.FooterPanel.Draw(ACanvas.Handle, cxRectExcludeBorders(R, GetContentMargins, ABorders));
    finally
      ACanvas.RestoreClipRegion;
    end;
  end
  else
    inherited DrawFooterBorder(ACanvas, R);
end;

procedure TdxSkinLookAndFeelPainter.DrawSchedulerNavigationButtonContent(ACanvas: TcxCanvas; const ARect: TRect;
  const AArrowRect: TRect; AIsNextButton: Boolean; AState: TcxButtonState; const AIsVertical: Boolean = True);
const
  AArrowOffset: array[Boolean] of Integer = (1, 0);
var
  R, AArrowNewRect: TRect;
  ABitmap: TcxBitmap;
begin
  if SkinInfo.SchedulerNavigationButtons[AIsNextButton] = nil then
    inherited DrawSchedulerNavigationButtonContent(ACanvas, ARect, AArrowRect, AIsNextButton, AState, AIsVertical)
  else
    if AIsVertical then
    begin
      R := ARect;
      if AIsNextButton then
        Inc(R.Right)
      else
        Dec(R.Left);
      ACanvas.SaveClipRegion;
      try
        ACanvas.IntersectClipRect(ARect);
        SkinInfo.SchedulerNavigationButtons[AIsNextButton].Draw(ACanvas.Handle, R, 0, ButtonStateToSkinState[AState]);
        DrawSkinElement(SkinInfo.SchedulerNavigationButtonsArrow[AIsNextButton], ACanvas, AArrowRect, ButtonStateToSkinState[AState]);
      finally
        ACanvas.RestoreClipRegion;
      end;
    end
    else
    begin
      R := ARect;
      if AIsNextButton then
        Inc(R.Bottom)
      else
        Dec(R.Top);
      ABitmap := TcxBitmap.CreateSize(R, pf32bit);
      try
        cxBitBlt(ABitmap.Canvas.Handle, ACanvas.Handle, ABitmap.ClientRect, R.TopLeft, SRCCOPY);
        ABitmap.Rotate(raPlus90);
        AArrowNewRect := cxRectSetOrigin(cxRectRotate(AArrowRect), cxPoint(AArrowRect.Top - ARect.Top, ARect.Right - AArrowRect.Right));
        AArrowNewRect := cxRectOffsetHorz(AArrowNewRect, AArrowOffset[AIsNextButton]);
        SkinInfo.SchedulerNavigationButtons[AIsNextButton].Draw(ABitmap.Canvas.Handle, ABitmap.ClientRect, 0, ButtonStateToSkinState[AState]);
        DrawSkinElement(SkinInfo.SchedulerNavigationButtonsArrow[AIsNextButton], ABitmap.cxCanvas, AArrowNewRect, ButtonStateToSkinState[AState]);
        ABitmap.Rotate(raMinus90);
        cxBitBlt(ACanvas.Handle, ABitmap.Canvas.Handle, R, cxNullPoint, SRCCOPY);
      finally
        ABitmap.Free;
      end;
    end;
end;

function TdxSkinLookAndFeelPainter.IsColorPropertyAssigned(AColor: TdxSkinColor): Boolean;
begin
  Result := Assigned(AColor) and cxColorIsValid(AColor.Value);
end;

function TdxSkinLookAndFeelPainter.IsSkinElementColorAssigned(AElement: TdxSkinElement): Boolean;
begin
  Result := Assigned(AElement) and cxColorIsValid(AElement.Color);
end;

function TdxSkinLookAndFeelPainter.IsSkinElementTextColorAssigned(AElement: TdxSkinElement): Boolean;
begin
  Result := Assigned(AElement) and cxColorIsValid(AElement.TextColor);
end;

function TdxSkinLookAndFeelPainter.DrawRightToLeftDependentSkinElement(AElement: TdxSkinElement; ACanvas: TcxCanvas;
  const R: TRect; AScaleFactor: TdxScaleFactor; AState: TdxSkinElementState = esNormal; AImageIndex: Integer = 0): Boolean;
begin
  Result := AElement <> nil;
  if Result then
    if ACanvas.UseRightToLeftAlignment then
      AElement.DrawRTL(ACanvas.Handle, R, AScaleFactor, AImageIndex, AState)
    else
      AElement.Draw(ACanvas.Handle, R, AScaleFactor, AImageIndex, AState);
end;

function TdxSkinLookAndFeelPainter.DrawSkinElement(AElement: TdxSkinElement;
  ACanvas: TcxCanvas; const R: TRect; AState: TdxSkinElementState = esNormal; AImageIndex: Integer = 0): Boolean;
begin
  Result := DrawSkinElement(AElement, ACanvas, R, dxSystemScaleFactor, AState, AImageIndex);
end;

function TdxSkinLookAndFeelPainter.DrawSkinElement(AElement: TdxSkinElement; ACanvas: TcxCanvas;
  const R: TRect; AScaleFactor: TdxScaleFactor; AState: TdxSkinElementState = esNormal; AImageIndex: Integer = 0): Boolean;
begin
  Result := AElement <> nil;
  if Result then
    AElement.Draw(ACanvas.Handle, R, AScaleFactor, AImageIndex, AState);
end;

function TdxSkinLookAndFeelPainter.GalleryStateToButtonState(const AState: TdxGalleryItemViewState): TcxButtonState;
begin
  if not AState.Enabled then
    Result := cxbsDisabled
  else if AState.Pressed then
    Result := cxbsPressed
  else if AState.Hover or AState.Focused then
    Result := cxbsHot
  else
    Result := cxbsNormal;
end;

function TdxSkinLookAndFeelPainter.GetSkinInfoClass: TdxSkinLookAndFeelPainterInfoClass;
begin
  Result := TdxSkinLookAndFeelPainterInfo;
end;

procedure TdxSkinLookAndFeelPainter.SchedulerNavigationButtonSizes(AIsNextButton: Boolean; var ABorders: TRect;
  var AArrowSize: TSize; var AHasTextArea: Boolean; AScaleFactor: TdxScaleFactor; const AIsVertical: Boolean = True);
var
  AElementArrow: TdxSkinElement;
  AElementButton: TdxSkinElement;
begin
  AElementArrow := SkinInfo.SchedulerNavigationButtonsArrow[AIsNextButton];
  AElementButton := SkinInfo.SchedulerNavigationButtons[AIsNextButton];
  if (AElementButton <> nil) and (AElementArrow <> nil) then
  begin
    ABorders := AElementButton.ContentOffset.Rect;
    if not AIsVertical then
      ABorders := cxRectRotate(ABorders);
    AArrowSize := AScaleFactor.Apply(AElementArrow.MinSize.Size);
    if not AIsVertical then
      AArrowSize := AScaleFactor.Apply(cxSize(AArrowSize.cy, AArrowSize.cx));
    AHasTextArea := not AElementButton.Image.Empty or cxColorIsValid(AElementButton.Color);
  end
  else
    inherited;
end;

procedure TdxSkinLookAndFeelPainter.DrawSchedulerGroup(ACanvas: TcxCanvas; const R: TRect; AColor: TColor = clDefault);
var
  AMask: TcxBitmap;
  ARegion: HRGN;
begin
  if SkinInfo.SchedulerGroup <> nil then
  begin
    SkinInfo.SchedulerGroup.Draw(ACanvas.Handle, R);
    if cxColorIsValid(AColor) and (AColor <> clWindow) then
    begin
      AMask := TcxBitmap.CreateSize(R);
      try
        SkinInfo.SchedulerGroup.Draw(AMask.Canvas.Handle, AMask.ClientRect, 1);
        AMask.Monochrome := True;
        ARegion := cxCreateRegionFromBitmap(AMask, clBlack);
        try
          OffsetRgn(ARegion, R.Left, R.Top);
          ACanvas.FillRegion(ARegion, AColor);
        finally
          DeleteObject(ARegion);
        end;
      finally
        AMask.Free;
      end;
    end;
  end
  else
    inherited DrawSchedulerGroup(ACanvas, R);
end;

procedure TdxSkinLookAndFeelPainter.DrawSchedulerScaledGroupSeparator(ACanvas: TcxCanvas;
  const ABounds: TRect; ABackgroundColor: TColor; AScaleFactor: TdxScaleFactor; AOnDrawBackground: TcxDrawBackgroundEvent);
begin
  ACanvas.FrameRect(ABounds, GetContainerBorderColor(False), 1, [bLeft, bRight]);
  ACanvas.FillRect(cxRectInflate(ABounds, -1, 0), DefaultContentColor);
end;

procedure TdxSkinLookAndFeelPainter.DrawSchedulerMilestone(ACanvas: TcxCanvas; const R: TRect);
var
  ASize: TSize;
begin
  if SkinInfo.SchedulerMilestone <> nil then
  begin
    ASize := cxSize(cxRectProportionalStretch(R, SkinInfo.SchedulerMilestone.Size));
    SkinInfo.SchedulerMilestone.Draw(ACanvas.Handle, cxRectCenter(R, ASize));
  end
  else
    inherited DrawSchedulerMilestone(ACanvas, R);
end;

procedure TdxSkinLookAndFeelPainter.DrawSchedulerScaledNavigatorButton(
  ACanvas: TcxCanvas; R: TRect; AState: TcxButtonState; AScaleFactor: TdxScaleFactor);
begin
  if not DrawSkinElement(SkinInfo.EditButtonElements[False], ACanvas, R, AScaleFactor, ButtonStateToSkinState[AState]) then
    inherited DrawSchedulerScaledNavigatorButton(ACanvas, R, AState, AScaleFactor);
end;

function TdxSkinLookAndFeelPainter.GetWindowContentTextColor: TColor;
begin
  Result := clDefault;
  if SkinInfo.FormContent <> nil then
    Result := SkinInfo.FormContent.TextColor;
  Result := cxGetActualColor(Result, inherited GetWindowContentTextColor);
end;

procedure TdxSkinLookAndFeelPainter.DrawWindowContent(ACanvas: TcxCanvas; const ARect: TRect);
begin
  if SkinInfo.FormContent <> nil then
    ACanvas.FillRect(ARect, SkinInfo.FormContent.Color)
  else
    inherited DrawWindowContent(ACanvas, ARect);
end;

function TdxSkinLookAndFeelPainter.PrintPreviewBackgroundTextColor: TColor;
begin
  Result := clDefault;
  if SkinInfo.PrintingPreviewBackground <> nil then
    Result := SkinInfo.PrintingPreviewBackground.TextColor;
  if not cxColorIsValid(Result) and (SkinInfo.MainMenu <> nil) then
    Result := SkinInfo.MainMenu.TextColor;
  if not cxColorIsValid(Result) then
    Result := inherited PrintPreviewBackgroundTextColor;
end;

function TdxSkinLookAndFeelPainter.PrintPreviewPageBordersScaledWidth(AScaleFactor: TdxScaleFactor): TRect;
begin
  if SkinInfo.PrintingPageBorder <> nil then
    Result := AScaleFactor.Apply(SkinInfo.PrintingPageBorder.ContentOffset.Rect)
  else
    Result := inherited PrintPreviewPageBordersScaledWidth(AScaleFactor);
end;

procedure TdxSkinLookAndFeelPainter.DrawPrintPreviewScaledBackground(ACanvas: TcxCanvas; const R: TRect;
  AScaleFactor: TdxScaleFactor);
begin
  if SkinInfo.PrintingPreviewBackground = nil then
    inherited DrawPrintPreviewBackground(ACanvas, R)
  else
    DrawSkinElement(SkinInfo.PrintingPreviewBackground, ACanvas, R, AScaleFactor);
end;

procedure TdxSkinLookAndFeelPainter.DrawPrintPreviewPageScaledBackground(
  ACanvas: TcxCanvas; const ABorderRect, AContentRect: TRect; ASelected, ADrawContent: Boolean;
  AScaleFactor: TdxScaleFactor);
const
  StateMap: array[Boolean] of TdxSkinElementState = (esNormal, esActive);
begin
  if SkinInfo.PrintingPageBorder =  nil then
    inherited DrawPrintPreviewPageBackground(ACanvas, ABorderRect, AContentRect, ASelected, ADrawContent)
  else
  begin
    ACanvas.SaveClipRegion;
    try
      if ADrawContent then
        ACanvas.FillRect(AContentRect, clWhite);
      ACanvas.ExcludeClipRect(AContentRect);
      DrawSkinElement(SkinInfo.PrintingPageBorder, ACanvas, ABorderRect, StateMap[ASelected], 0);
    finally
      ACanvas.RestoreClipRegion;
    end;
  end;
end;

procedure TdxSkinLookAndFeelPainter.DrawDateNavigatorCellSelection(
  ACanvas: TcxCanvas; const R: TRect; AColor: TColor);
begin
  if not DrawSkinElement(SkinInfo.HighlightedItem, ACanvas, R) then
    inherited DrawDateNavigatorCellSelection(ACanvas, R, AColor);
end;

procedure TdxSkinLookAndFeelPainter.DrawDateNavigatorDateHeader(ACanvas: TcxCanvas; var R: TRect);
begin
  InflateRect(R, 1, 0);
  DrawScaledHeader(ACanvas, R, cxEmptyRect, [], [], cxbsNormal,
    taCenter, vaCenter, False, False, '', nil, 0, 0, dxDefaultScaleFactor);
end;

procedure TdxSkinLookAndFeelPainter.DrawDateNavigatorTodayCellSelection(ACanvas: TcxCanvas; const R: TRect);
begin
  if not DrawSkinElement(SkinInfo.HighlightedItem, ACanvas, R, esNormal, 1) then
    inherited DrawDateNavigatorTodayCellSelection(ACanvas, R);
end;

function TdxSkinLookAndFeelPainter.IndicatorDrawItemsFirst: Boolean;
begin
  Result := True;
end;

function TdxSkinLookAndFeelPainter.GetColorGalleryGlyphFrameColor: TColor;
begin
  Result := GetContainerBorderColor(True);
end;

procedure TdxSkinLookAndFeelPainter.DrawGalleryBackground(ACanvas: TcxCanvas; const ABounds: TRect);
begin
  if not DrawSkinElement(SkinInfo.GalleryBackground, ACanvas, ABounds) then
    inherited DrawGalleryBackground(ACanvas, ABounds);
end;

procedure TdxSkinLookAndFeelPainter.DrawGalleryGroupHeader(ACanvas: TcxCanvas; const ABounds: TRect);
begin
  if not DrawSkinElement(SkinInfo.GalleryGroup, ACanvas, ABounds) then
    inherited DrawGalleryGroupHeader(ACanvas, ABounds);
end;

procedure TdxSkinLookAndFeelPainter.DrawGalleryItemImageFrame(ACanvas: TcxCanvas; const R: TRect);
begin
  if not DrawSkinElement(SkinInfo.GalleryItemGlyphFrame, ACanvas, R) then
    inherited DrawGalleryItemImageFrame(ACanvas, R);
end;

procedure TdxSkinLookAndFeelPainter.DrawGalleryItemSelection(
  ACanvas: TcxCanvas; const R: TRect; AViewState: TdxGalleryItemViewState);

  function GetElementState: TdxSkinElementState;
  const
    CheckedMap: array[Boolean] of TdxSkinElementState = (esNormal, esChecked);
    HoverMap: array[Boolean] of TdxSkinElementState = (esHot, esHotCheck);
  begin
    if not AViewState.Enabled then
      Result := esDisabled
    else
      if AViewState.Pressed then
        Result := esPressed
      else
        if AViewState.Hover or AViewState.Focused then
          Result := HoverMap[AViewState.Checked]
        else
          Result := CheckedMap[AViewState.Checked];
  end;

begin
  if not DrawSkinElement(SkinInfo.GalleryItem, ACanvas, R, GetElementState) then
    inherited DrawGalleryItemSelection(ACanvas, R, AViewState);
end;

function TdxSkinLookAndFeelPainter.DrawGalleryItemSelectionFirst: Boolean;
begin
  Result := True;
end;

procedure TdxSkinLookAndFeelPainter.DrawSkinElementRotated(ACanvas: TcxCanvas;
  ASkinElement: TdxSkinElement; const ARect: TRect; AState: TdxSkinElementState;
  AAngle: TcxRotationAngle; AFlipVertically: Boolean = False; AScaleFactor: TdxScaleFactor = nil);
const
  InvertAngle: array [TcxRotationAngle] of TcxRotationAngle =
    (ra0, raMinus90, raPlus90, ra180);
var
  ABitmap: TcxBitmap;
begin
  if AScaleFactor = nil then
    AScaleFactor := dxSystemScaleFactor;

  if (AAngle = ra0) and not AFlipVertically then
    DrawSkinElement(ASkinElement, ACanvas, ARect, AScaleFactor, AState)
  else
  begin
    ABitmap := TcxBitmap32.CreateSize(ARect);
    try
      ABitmap.cxCanvas.Brush := ACanvas.Brush;
      cxBitBlt(ABitmap.Canvas.Handle, ACanvas.Handle, ABitmap.ClientRect, ARect.TopLeft, SRCCOPY);
      ABitmap.Rotate(ra0, AFlipVertically);
      ABitmap.Rotate(InvertAngle[AAngle]);
      DrawSkinElement(ASkinElement, ABitmap.cxCanvas, ABitmap.ClientRect, AScaleFactor, AState);
      ABitmap.Rotate(AAngle, AFlipVertically);
      cxDrawBitmap(ACanvas.Handle, ABitmap, ARect, cxNullPoint);
    finally
      ABitmap.Free;
    end;
  end;
end;

function TdxSkinLookAndFeelPainter.GetSkinElementBordersWidth(AElement: TdxSkinElement): TRect;
begin
  Result := AElement.ContentOffset.Rect;
  Inc(Result.Bottom, AElement.Borders[bBottom].Thin);
  Inc(Result.Left, AElement.Borders[bLeft].Thin);
  Inc(Result.Right, AElement.Borders[bRight].Thin);
  Inc(Result.Top, AElement.Borders[bTop].Thin);
end;

function TdxSkinLookAndFeelPainter.GetSkinElementColorPalette(AElement: TdxSkinElement; AState: TdxSkinElementState): IdxColorPalette;
begin
  if AElement <> nil then
    Result := AElement.GetGlyphColorPalette(AState)
  else
    Result := nil;
end;

function TdxSkinLookAndFeelPainter.GetSkinElementMinSize(AElement: TdxSkinElement): TSize;
begin
  Result := cxSizeMax(AElement.MinSize.Size, AElement.Size);
end;

function TdxSkinLookAndFeelPainter.GetSkinDetails: TdxSkinDetails;
begin
  Result := GetLookAndFeelPainterDetails as TdxSkinDetails;
end;

function TdxSkinLookAndFeelPainter.GetSkinInfo: TdxSkinLookAndFeelPainterInfo;
begin
  if FSkinInfo = nil then
  begin
    FSkinInfo := GetSkinInfoClass.Create;
    if FSkinResName <> '' then
    begin
      FSkinInfo.Skin := TdxSkin.Create(FSkinResName, True, FSkinResInstance);
      FreeAndNil(FLookAndFeelPainterDetailsCache);
    end;
  end;
  Result := FSkinInfo;
end;

function TdxSkinLookAndFeelPainter.IsFontBold(AElement: TdxSkinElement): Boolean;
var
  AProperty: TdxSkinProperty;
begin
  Result := (AElement <> nil) and AElement.GetPropertyByName(sdxFontBold, AProperty) and
    (AProperty is TdxSkinBooleanProperty) and TdxSkinBooleanProperty(AProperty).Value;
end;

end.
