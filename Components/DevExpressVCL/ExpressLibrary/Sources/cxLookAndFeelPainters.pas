{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressCommonLibrary                                     }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSCOMMONLIBRARY AND ALL          }
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

unit cxLookAndFeelPainters;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, Windows, dxCore, dxUxTheme, dxThemeManager, Classes, Graphics, Generics.Collections, Generics.Defaults,
  Controls, ImgList,
  dxCoreGraphics, dxOffice11, cxClasses, cxGraphics, cxGeometry, dxGDIPlusClasses;

const
  cxContainerMaxBorderWidth = 2;
  cxTextOffset = 2;
  cxHeaderTextOffset = cxTextOffset;

  cxArrowLeftBasePointIndex = 0;
  cxArrowTopPointIndex = 1;
  cxArrowRightBasePointIndex = 2;

  cxStdThumbnailMinimalSize = 8;

  cxInplaceNavigatorDefaultOffset = 50;

  cxTouchElementMinSize = 27; // ~7mm on 96 dpi

type
  TcxCustomLookAndFeelPainter = class;
  TcxLookAndFeelStyle = (lfsFlat, lfsStandard, lfsUltraFlat, lfsNative, lfsOffice11, lfsSkin);
  TcxLookAndFeelStyles = set of TcxLookAndFeelStyle;

  TcxEditBtnPosition = (cxbpLeft, cxbpRight);
  TcxGroupBoxCaptionPosition = (cxgpTop, cxgpBottom, cxgpLeft, cxgpRight, cxgpCenter);
  TcxNeighbor = (nLeft, nTop, nRight, nBottom);
  TcxNeighbors = set of TcxNeighbor;
  TcxScrollBarPart = (sbpNone, sbpLineUp, sbpLineDown, sbpThumbnail, sbpPageUp, sbpPageDown);
  TcxTrackBarTicksAlign = (tbtaUp, tbtaDown, tbtaBoth);
  TcxLayoutElement = (leGroup, leGroupWithoutBorders, leTabbedGroup, leRootGroup, leRootGroupWithoutBorders, leItem);

  TdxBevelShape = (dxbsNone, dxbsBox, dxbsFrame, dxbsLineTop, dxbsLineBottom,
    dxbsLineLeft, dxbsLineRight, dxbsLineCenteredHorz, dxbsLineCenteredVert);
  TdxBevelStyle = (dxbsLowered, dxbsRaised);
  TdxBreadcrumbEditButtonState = (dxbcbsNormal, dxbcbsFocused, dxbcbsHot, dxbcbsPressed, dxbcbsDisabled);
  TdxBreadcrumbEditState = (dxbcsNormal, dxbcsFocused, dxbcsHot, dxbcsDisabled);
  TcxEditCheckState = (ecsNormal, ecsHot, ecsPressed, ecsDisabled);
  TcxButtonPart = (cxbpButton, cxbpDropDownLeftPart, cxbpDropDownRightPart);
  TcxButtonState = (cxbsDefault, cxbsNormal, cxbsHot, cxbsPressed, cxbsDisabled);
  TcxCheckBoxState = (cbsUnchecked, cbsChecked, cbsGrayed);
  TdxRatingControlIndicatorState = (rcisUnchecked, rcisChecked, rcisHover);

  TdxGalleryItemViewState = record
    Enabled: Boolean;
    Checked: Boolean;
    Hover: Boolean;
    Pressed: Boolean;
    Focused: Boolean;
  end;

  TcxEditBtnKind = (cxbkCloseBtn, cxbkComboBtn, cxbkEditorBtn, cxbkEllipsisBtn,
    cxbkSpinUpBtn, cxbkSpinDownBtn, cxbkSpinLeftBtn, cxbkSpinRightBtn);
  TcxEditStateColorKind = (esckNormal, esckDisabled, esckInactive, esckReadOnly);
  TcxCalcButtonKind = (cbBack, cbCancel, cbClear, cbMC, cbMR, cbMS, cbMP, cbNum0, cbNum1, cbNum2, cbNum3, cbNum4, cbNum5,
    cbNum6, cbNum7, cbNum8, cbNum9, cbSign, cbDecimal, cbDiv, cbMul, cbSub, cbAdd, cbSqrt, cbPercent, cbRev, cbEqual, cbNone);
  TcxIndicatorKind = (ikNone, ikArrow, ikEdit, ikInsert, ikMultiDot, ikMultiArrow, ikFilter, ikInplaceEdit);
  TdxAlertWindowButtonKind = (awbkClose, awbkPin, awbkDropdown, awbkPrevious, awbkNext, awbkCustom);
  TcxFilterSmartTagState = (fstsNormal, fstsHot, fstsPressed, fstsParentHot);
  TdxMapControlElementState = (mcesNormal, mcesHot, mcesPressed, mcesSelected, mcesDisabled);
  TdxMapControlElementStates = set of TdxMapControlElementState;
  TcxCalendarElementState = (cesNormal, cesHot, cesPressed, cesSelected, cesFocused, cesMarked, cesDisabled);
  TcxCalendarElementStates = set of TcxCalendarElementState;
  TcxExpandButtonState = (cebsNormal, cebsSelected, cebsInactive);
  TcxFilterButtonShowMode = (fbmButton, fbmSmartTag, fbmDefault);
  TcxShowFilterButtons = (sfbAlways, sfbWhenSelected, sfbDefault);

  TcxContainerBorderStyle = (cbsNone, cbsSingle, cbsThick, cbsFlat, cbs3D, cbsUltraFlat, cbsOffice11);
  TcxEditPopupBorderStyle = (epbsDefault, epbsSingle, epbsFrame3D, epbsFlat);
  TcxPopupBorderStyle = (pbsNone, pbsUltraFlat, pbsFlat, pbs3D);

  TcxDrawBackgroundEvent = function(ACanvas: TcxCanvas; const ABounds: TRect): Boolean of object;
  TdxDrawEvent = procedure(ACanvas: TcxCanvas; const ARect: TRect) of object;
  TdxDrawScaledRectEvent = procedure(ACanvas: TcxCanvas; const ARect: TRect; AScaleFactor: TdxScaleFactor) of object;

  IcxLookAndFeelPainterListener = interface
  ['{2284D497-2FBA-46CA-968B-9857A31F462B}']
    procedure PainterAdded(APainter: TcxCustomLookAndFeelPainter);
    procedure PainterRemoved(APainter: TcxCustomLookAndFeelPainter);
  end;

  { TcxCustomLookAndFeelPainter }

  TcxCustomLookAndFeelPainter = class(TPersistent)
  strict private
    FBackButton: TdxSmartImage;
    FCalendarButtonGlyph: TdxSmartImage;
    FClockFace: TdxSmartImage;
    FClockGlass: TdxSmartImage;
    FFixedGroupIndicator: TdxSmartImage;
    FMapPushpin: TdxSmartImage;
    FNavigationBarCustomizationButton: TdxSmartImage;
    FRatingControlIndicator: TdxSmartImage;
    FSearchButtonGlyph: TdxSmartImage;
    FSmartTagGlyph: TdxSmartImage;

    function GetBackButton: TdxSmartImage;
    function GetCalendarButtonGlyph: TdxSmartImage;
    function GetClockFace: TdxSmartImage;
    function GetClockGlass: TdxSmartImage;
    function GetFixedGroupIndicator: TdxSmartImage;
    function GetMapPushpin: TdxSmartImage;
    function GetNavigationBarCustomizationButton: TdxSmartImage;
    function GetRangeTrackBarThumbDrawRect(const R: TRect; ATicks: TcxTrackBarTicksAlign; AHorizontal: Boolean): TRect;
    function GetRatingControlIndicator: TdxSmartImage;
    function GetSearchButtonGlyph: TdxSmartImage;
    function GetSmartTagGlyph: TdxSmartImage;
  protected
    FLookAndFeelPainterDetailsCache: TObject;

    function CreateLookAndFeelPainterDetails: TObject; virtual;
    function GetLookAndFeelPainterDetails: TObject; virtual;

    function DefaultDateNavigatorHeaderHighlightTextColor: TColor; virtual;
    procedure DoDrawScaledScrollBarPart(ACanvas: TcxCanvas; AHorizontal: Boolean; R: TRect; APart: TcxScrollBarPart;
      AState: TcxButtonState; AScaleFactor: TdxScaleFactor); virtual;
    procedure DrawBackground(ACanvas: TcxCanvas; const ARect: TRect;
      ATransparent: Boolean; ABackgroundColor: TColor; const ABackgroundBitmap: TBitmap); virtual;
    procedure DrawButtonArrow(ACanvas: TcxCanvas; const R: TRect; AColor: TColor); virtual;
    procedure DrawContent(ACanvas: TcxCanvas; const ABounds, ATextAreaBounds: TRect; AState: Integer;
      AAlignmentHorz: TAlignment; AAlignmentVert: TcxAlignmentVert; AMultiLine, AShowEndEllipsis: Boolean;
      const AText: string; AFont: TFont; ATextColor, ABkColor: TColor;
      AOnDrawBackground: TcxDrawBackgroundEvent = nil; AIsFooter: Boolean = False); virtual;
    procedure DrawExpandButtonCross(ACanvas: TcxCanvas; const R: TRect;
      AExpanded: Boolean; AColor: TColor; AScaleFactor: TdxScaleFactor);
    procedure DrawMonthHeaderArrows(ACanvas: TcxCanvas; const ABounds: TRect;
      AArrows: TcxArrowDirections; ASideWidth: Integer; AColor: TColor);
    procedure DrawMonthHeaderLeftArrow(ACanvas: TcxCanvas; const ABounds: TRect; AColor: TColor);
    procedure DrawMonthHeaderRightArrow(ACanvas: TcxCanvas; const ABounds: TRect; AColor: TColor);
    procedure DrawRangeControlThumb(ACanvas: TcxCanvas; const ARect: TRect;
      AColor: TColor; ABorderColor: TdxAlphaColor; AScaleFactor: TdxScaleFactor);
    procedure DrawSortingArrow(ACanvas: TcxCanvas; const R: TRect;
      AColor1, AColor2: TColor; AAscendingSorting: Boolean; AScaleFactor: TdxScaleFactor);
    procedure DrawSummarySortingArrow(ACanvas: TcxCanvas; const R: TRect;
      AColor1, AColor2: TColor; AAscendingSorting: Boolean; AScaleFactor: TdxScaleFactor);
    procedure DrawSchedulerNavigationButtonContent(ACanvas: TcxCanvas; const ARect: TRect; const AArrowRect: TRect;
      AIsNextButton: Boolean; AState: TcxButtonState; const AIsVertical: Boolean = True); virtual;
    function FooterCellContentBounds(const ABounds: TRect): TRect; virtual;
    function FooterCellTextAreaBounds(const ABounds: TRect): TRect; virtual;
    function GetArrowDirection(AHorizontal: Boolean; APart: TcxScrollBarPart): TcxArrowDirection;
    function GetFilterSmartTagColor(AState: TcxFilterSmartTagState; AIsFilterActive: Boolean): TColor; virtual;
    function GetSeparatorBounds(const R: TRect; AWidth: Integer; AIsVertical: Boolean): TRect; virtual;

    procedure ReleaseImageResources; virtual;

    property BackButton: TdxSmartImage read GetBackButton;
    property CalendarButtonGlyph: TdxSmartImage read GetCalendarButtonGlyph;
    property ClockFace: TdxSmartImage read GetClockFace;
    property ClockGlass: TdxSmartImage read GetClockGlass;
    property FixedGroupIndicator: TdxSmartImage read GetFixedGroupIndicator;
    property MapPushpin: TdxSmartImage read GetMapPushpin;
    property NavigationBarCustomizationButton: TdxSmartImage read GetNavigationBarCustomizationButton;
    property RatingControlIndicator: TdxSmartImage read GetRatingControlIndicator;
    property SearchButtonGlyph: TdxSmartImage read GetSearchButtonGlyph;
    property SmartTagGlyph: TdxSmartImage read GetSmartTagGlyph;
  public
    destructor Destroy; override;

    function GetPainterData(var AData): Boolean; virtual;
    function GetPainterDetails(var ADetails): Boolean; virtual;
    function IsInternalPainter: Boolean; virtual;
    function LookAndFeelName: string; virtual;
    function LookAndFeelStyle: TcxLookAndFeelStyle; virtual;
    function NeedRedrawOnResize: Boolean; virtual;

    // colors
    function DefaultContentColor: TColor; virtual;
    function DefaultContentEvenColor: TColor; virtual;
    function DefaultContentOddColor: TColor; virtual;
    function DefaultContentTextColor: TColor; virtual;
    function DefaultControlColor: TColor; virtual;
    function DefaultControlTextColor: TColor; virtual;
    function DefaultEditorBackgroundColor(AIsDisabled: Boolean): TColor; virtual;
    function DefaultEditorBackgroundColorEx(AKind: TcxEditStateColorKind): TColor; virtual;
    function DefaultEditorTextColor(AIsDisabled: Boolean): TColor; virtual;
    function DefaultEditorTextColorEx(AKind: TcxEditStateColorKind): TColor; virtual;
    function DefaultFilterBoxColor: TColor; virtual;
    function DefaultFilterBoxTextColor: TColor; virtual;
    function DefaultFixedSeparatorColor: TColor; virtual;
    function DefaultFooterColor: TColor; virtual;
    function DefaultFooterTextColor: TColor; virtual;
    function DefaultGridDetailsSiteColor: TColor; virtual;
    function DefaultGridLineColor: TColor; virtual;
    function DefaultGroupByBoxColor: TColor; virtual;
    function DefaultGroupByBoxTextColor: TColor; virtual;
    function DefaultGroupColor: TColor; virtual;
    function DefaultGroupContentOffsets: TRect; virtual;
    function DefaultGroupTextColor: TColor; virtual;
    function DefaultHeaderBackgroundColor: TColor; virtual;
    function DefaultHeaderBackgroundTextColor: TColor; virtual;
    function DefaultHeaderColor: TColor; virtual;
    function DefaultHeaderTextColor: TColor; virtual;
    function DefaultHyperlinkTextColor: TColor; virtual;
    function DefaultInactiveColor: TColor; virtual;
    function DefaultInactiveTextColor: TColor; virtual;
    function DefaultPreviewTextColor: TColor; virtual;
    function DefaultRecordSeparatorColor: TColor; virtual;
    function DefaultSizeGripAreaColor: TColor; virtual;

    function DefaultTreeListGridLineColor: TColor; virtual;
    function DefaultTreeListTreeLineColor: TColor; virtual;

    function DefaultVGridBandLineColor: TColor; virtual;
    function DefaultVGridCategoryColor: TColor; virtual;
    function DefaultVGridCategoryTextColor: TColor; virtual;
    function DefaultVGridContentColor: TColor; virtual;
    function DefaultVGridContentEvenColor: TColor; virtual;
    function DefaultVGridContentOddColor: TColor; virtual;
    function DefaultVGridContentTextColor: TColor; virtual;
    function DefaultVGridHeaderColor: TColor; virtual;
    function DefaultVGridHeaderTextColor: TColor; virtual;
    function DefaultVGridLineColor: TColor; virtual;

    function DefaultDateNavigatorContentColor: TColor; virtual;
    function DefaultDateNavigatorHeaderColor: TColor; virtual;
    function DefaultDateNavigatorHeaderTextColor(AIsHighlight: Boolean): TColor; virtual;
    function DefaultDateNavigatorHolydayTextColor: TColor; virtual;
    function DefaultDateNavigatorInactiveTextColor: TColor; virtual;
    function DefaultDateNavigatorSelectionColor: TColor; virtual;
    function DefaultDateNavigatorSelectionTextColor: TColor; virtual;
    function DefaultDateNavigatorSeparator1Color: TColor; virtual;
    function DefaultDateNavigatorSeparator2Color: TColor; virtual;
    function DefaultDateNavigatorTextColor: TColor; virtual;
    function DefaultDateNavigatorTodayFrameColor: TColor; virtual;
    function DefaultDateNavigatorTodayTextColor: TColor; overload;
    function DefaultDateNavigatorTodayTextColor(ASelected: Boolean): TColor; overload; virtual;

    function DefaultSchedulerBackgroundColor: TColor; virtual;
    function DefaultSchedulerBorderColor: TColor; virtual;
    function DefaultSchedulerContentColor(AResourceIndex: Integer): TColor; virtual;
    function DefaultSchedulerControlColor: TColor; virtual;
    function DefaultSchedulerDayHeaderColor: TColor; virtual;
    function DefaultSchedulerDayHeaderBorderColor: TColor; virtual;
    function DefaultSchedulerDayHeaderTextColor: TColor; virtual;
    function DefaultSchedulerDateNavigatorArrowColor(AIsHighlight: Boolean): TColor; virtual;
    function DefaultSchedulerHeaderContainerAlternateBackgroundColor: TColor; virtual;
    function DefaultSchedulerHeaderContainerBackgroundColor(ASelected: Boolean): TColor; virtual;
    function DefaultSchedulerHeaderContainerTextColor(ASelected: Boolean): TColor; virtual;
    function DefaultSchedulerHeaderContainerBorderColor: TColor; virtual;
    function DefaultSchedulerEventColor(AIsAllDayEvent: Boolean): TColor; virtual;
    function DefaultSchedulerEventColorClassic(AIsAllDayEvent: Boolean): TColor; virtual;
    function DefaultSchedulerNavigatorColor: TColor; virtual;
    function DefaultSchedulerSelectedEventBorderColor: TColor; virtual;
    function DefaultSchedulerTextColor: TColor; virtual;
    function DefaultSchedulerTimeRulerBorderColor: TColor; virtual;
    function DefaultSchedulerTimeRulerBorderColorClassic: TColor; virtual;
    function DefaultSchedulerTimeRulerColor: TColor; virtual;
    function DefaultSchedulerTimeRulerColorClassic: TColor; virtual;
    function DefaultSchedulerTimeRulerTextColor: TColor; virtual;
    function DefaultSchedulerTimeRulerTextColorClassic: TColor; virtual;
    function DefaultSchedulerViewContentColor: TColor; virtual;
    function DefaultSchedulerViewContentColorClassic: TColor; virtual;
    function DefaultSchedulerViewSelectedTextColor: TColor; virtual;
    function DefaultSchedulerViewTextColor: TColor; virtual;
    function DefaultSchedulerYearViewUnusedContentColor(AIsWorkTime: Boolean): TColor; virtual;

    function DefaultSelectionColor: TColor; virtual;
    function DefaultSelectionTextColor: TColor; virtual;
    function DefaultSeparatorColor: TColor; virtual;
    function DefaultTabColor: TColor; virtual;
    function DefaultTabTextColor: TColor; virtual;
    function DefaultTabsBackgroundColor: TColor; virtual;
    function DefaultRootTabsBackgroundColor: TColor; virtual;

    function DefaultTimeGridMajorScaleColor: TColor; virtual;
    function DefaultTimeGridMajorScaleTextColor: TColor; virtual;
    function DefaultTimeGridMinorScaleColor: TColor; virtual;
    function DefaultTimeGridMinorScaleTextColor: TColor; virtual;
    function DefaultTimeGridSelectionBarColor: TColor; virtual;

    function DefaultChartDiagramValueBorderColor: TColor; virtual;
    function DefaultChartDiagramValueCaptionTextColor: TColor; virtual;
    function DefaultChartHistogramAxisColor: TColor; virtual;
    function DefaultChartHistogramGridLineColor: TColor; virtual;
    function DefaultChartHistogramPlotColor: TColor; virtual;
    function DefaultChartPieDiagramSeriesSiteBorderColor: TColor; virtual;
    function DefaultChartPieDiagramSeriesSiteCaptionColor: TColor; virtual;
    function DefaultChartPieDiagramSeriesSiteCaptionTextColor: TColor; virtual;
    function DefaultChartToolBoxDataLevelInfoBorderColor: TColor; virtual;
    function DefaultChartToolBoxItemSeparatorColor: TColor; virtual;

    // LayoutView
    function DefaultLayoutViewCaptionColor(AState: TcxButtonState): TColor; virtual;
    function DefaultLayoutViewCaptionTextColor(ACaptionPosition: TcxGroupBoxCaptionPosition; AState: TcxButtonState): TColor; virtual;
    function DefaultLayoutViewContentColor: TColor; virtual;
    function DefaultLayoutViewContentTextColor(AState: TcxButtonState): TColor; virtual;

    function DefaultGridOptionsTreeViewCategoryColor(ASelected: Boolean): TColor; virtual;
    function DefaultGridOptionsTreeViewCategoryTextColor(ASelected: Boolean): TColor; virtual;

    // Arrow
    procedure CalculateArrowPoints(R: TRect; var P: TcxArrowPoints; AArrowDirection: TcxArrowDirection; AProportional: Boolean; AArrowSize: Integer = 0);
    procedure DrawArrow(ACanvas: TcxCanvas; const R: TRect; AArrowDirection: TcxArrowDirection; AColor: TColor); overload; virtual;
    procedure DrawArrow(ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState; AArrowDirection: TcxArrowDirection; ADrawBorder: Boolean = True); overload;
    procedure DrawArrowBorder(ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState);
    procedure DrawCollapseArrow(ACanvas: TcxCanvas; R: TRect; AColor: TColor; ALineWidth: Integer = 1); virtual;
    procedure DrawScaledArrow(ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState;
      AArrowDirection: TcxArrowDirection; AScaleFactor: TdxScaleFactor; ADrawBorder: Boolean = True); overload; virtual;
    procedure DrawScaledArrowBorder(ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState; AScaleFactor: TdxScaleFactor); virtual;
    procedure DrawScaledScrollBarArrow(ACanvas: TcxCanvas; R: TRect; AState: TcxButtonState; AArrowDirection: TcxArrowDirection; AScaleFactor: TdxScaleFactor); virtual;
    procedure DrawScrollBarArrow(ACanvas: TcxCanvas; R: TRect; AState: TcxButtonState; AArrowDirection: TcxArrowDirection);

    // Border
    function BorderSize: Integer; virtual;
    function SeparatorSize: Integer; virtual;
    procedure DrawBorder(ACanvas: TcxCanvas; R: TRect); virtual;
    procedure DrawContainerBorder(ACanvas: TcxCanvas; const R: TRect; AStyle: TcxContainerBorderStyle;
      AWidth: Integer; AColor: TColor; ABorders: TcxBorders); virtual;
    procedure DoDrawSeparator(ACanvas: TcxCanvas; R: TRect; AIsVertical: Boolean); virtual;
    procedure DrawSeparator(ACanvas: TcxCanvas; const R: TRect; AIsVertical: Boolean);

    // Pin
    procedure DrawPin(ACanvas: TcxCanvas; const ABounds: TRect; AColor: TColor; APinned: Boolean);

    // Buttons
    function ButtonBorderSize(AState: TcxButtonState = cxbsNormal): Integer; virtual; // abstract;
    function ButtonColor(AState: TcxButtonState): TColor; virtual;
    function ButtonColorPalette(AState: TcxButtonState): IdxColorPalette; virtual;
    function ButtonDescriptionTextColor(AState: TcxButtonState; ADefaultColor: TColor = clDefault): TColor; virtual;
    function ButtonFocusRect(ACanvas: TcxCanvas; R: TRect): TRect;
    function ButtonSymbolColor(AState: TcxButtonState; ADefaultColor: TColor = clDefault): TColor; virtual;
    function ButtonSymbolState(AState: TcxButtonState): TcxButtonState; virtual;
    function ButtonTextOffset: Integer;
    function ButtonTextShift: Integer;
    function ScaledButtonFocusRect(ACanvas: TcxCanvas; R: TRect; AScaleFactor: TdxScaleFactor): TRect; virtual;
    function ScaledButtonTextOffset(AScaleFactor: TdxScaleFactor): Integer; virtual; // abstract;
    function ScaledButtonTextShift(AScaleFactor: TdxScaleFactor): Integer; virtual; // abstract;
    procedure DrawButton(ACanvas: TcxCanvas; R: TRect; const ACaption: string; AState: TcxButtonState;
      ADrawBorder: Boolean = True; AColor: TColor = clDefault; ATextColor: TColor = clDefault;
      AWordWrap: Boolean = False; AIsToolButton: Boolean = False; APart: TcxButtonPart = cxbpButton);
    procedure DrawButtonBorder(ACanvas: TcxCanvas; R: TRect; AState: TcxButtonState); virtual; // abstract;
    procedure DrawButtonCross(ACanvas: TcxCanvas; const R: TRect; AColor: TColor; AState: TcxButtonState); overload;
    procedure DrawButtonCross(ACanvas: TcxCanvas; const R: TRect; AColor: TColor; AState: TcxButtonState; ASize: Integer); overload;
    procedure DrawScaledButton(ACanvas: TcxCanvas; R: TRect; const ACaption: string; AState: TcxButtonState;
      AScaleFactor: TdxScaleFactor; ADrawBorder: Boolean = True; AColor: TColor = clDefault;
      ATextColor: TColor = clDefault; AWordWrap: Boolean = False; AIsToolButton: Boolean = False; APart: TcxButtonPart = cxbpButton); virtual;
    procedure DrawScaledButtonCross(ACanvas: TcxCanvas; const R: TRect; AColor: TColor; AState: TcxButtonState; AScaleFactor: TdxScaleFactor);
    procedure DrawScaledButtonCrossEx(ACanvas: TcxCanvas; const R: TRect; AColor: TColor; AState: TcxButtonState; ASize: Integer; AScaleFactor: TdxScaleFactor);
    procedure DrawScaledSearchEditButtonGlyph(ACanvas: TcxCanvas; R: TRect; AState: TcxButtonState; AScaleFactor: TdxScaleFactor); virtual;
    procedure DrawSearchEditButtonGlyph(ACanvas: TcxCanvas; R: TRect; AState: TcxButtonState);
    function GetDropDownButtonRightPartSize: Integer;
    function GetScaledDropDownButtonRightPartSize(AScaleFactor: TdxScaleFactor): Integer; virtual;
    function IsButtonHotTrack: Boolean; virtual; // abstract;
    function IsPointOverGroupExpandButton(const R: TRect; const P: TPoint): Boolean; virtual;
    function ScaledSearchButtonGlyphSize(AScaleFactor: TdxScaleFactor): TSize; virtual;

    // Expand Button
    procedure DrawExpandButton(ACanvas: TcxCanvas; const R: TRect; AExpanded: Boolean; AColor: TColor = clDefault);
    procedure DrawExpandButtonEx(ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState; AExpanded: Boolean; ARotationAngle: TcxRotationAngle = ra0);
    function DrawExpandButtonFirst: Boolean; virtual;
    procedure DrawExpandMark(ACanvas: TcxCanvas; const R: TRect; AColor: TColor; AExpanded: Boolean);
    procedure DrawGroupExpandButton(ACanvas: TcxCanvas; const R: TRect; AExpanded: Boolean; AState: TcxButtonState);
    procedure DrawScaledExpandButton(ACanvas: TcxCanvas; const R: TRect;
      AExpanded: Boolean; AScaleFactor: TdxScaleFactor; AColor: TColor = clDefault;
      AState: TcxExpandButtonState = cebsNormal); virtual; abstract;
    procedure DrawScaledExpandButtonEx(ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState;
      AExpanded: Boolean; AScaleFactor: TdxScaleFactor; ARotationAngle: TcxRotationAngle = ra0); overload; virtual;
    procedure DrawScaledExpandMark(ACanvas: TcxCanvas; const R: TRect;
      AColor: TColor; AExpanded: Boolean; AScaleFactor: TdxScaleFactor); virtual;
    procedure DrawScaledGroupExpandButton(ACanvas: TcxCanvas; const R: TRect;
      AExpanded: Boolean; AState: TcxButtonState; AScaleFactor: TdxScaleFactor); virtual;
    procedure DrawScaledSmallExpandButton(ACanvas: TcxCanvas; R: TRect; AExpanded: Boolean;
      ABorderColor: TColor; AScaleFactor: TdxScaleFactor; AColor: TColor = clDefault); virtual;
    procedure DrawSmallExpandButton(ACanvas: TcxCanvas; R: TRect; AExpanded: Boolean;
      ABorderColor: TColor; AColor: TColor = clDefault);
    function ExpandButtonAreaSize: Integer;
    function ExpandButtonSize: Integer;
    function GroupExpandButtonSize: Integer;
    function SmallExpandButtonSize: Integer;
    function ScaledExpandButtonAreaSize(AScaleFactor: TdxScaleFactor): Integer; virtual;
    function ScaledExpandButtonSize(AScaleFactor: TdxScaleFactor): Integer; virtual; abstract;
    function ScaledGroupExpandButtonSize(AScaleFactor: TdxScaleFactor): Integer; virtual;
    function ScaledSmallExpandButtonSize(AScaleFactor: TdxScaleFactor): Integer; virtual;

    // CommandLink
    function DefaultCommandLinkTextColor(AState: TcxButtonState; ADefaultColor: TColor = clDefault): TColor; virtual;
    procedure DrawCommandLinkBackground(ACanvas: TcxCanvas; R: TRect; AState: TcxButtonState; AColor: TColor = clDefault);
    procedure DrawCommandLinkGlyph(ACanvas: TcxCanvas; const AGlyphPos: TPoint; AState: TcxButtonState);
    procedure DrawScaledCommandLinkBackground(ACanvas: TcxCanvas; R: TRect; AState: TcxButtonState; AScaleFactor: TdxScaleFactor; AColor: TColor = clDefault); virtual;
    procedure DrawScaledCommandLinkGlyph(ACanvas: TcxCanvas; const AGlyphPos: TPoint; AState: TcxButtonState; AScaleFactor: TdxScaleFactor); virtual;
    function GetCommandLinkGlyphSize: TSize;
    function GetCommandLinkMargins: TRect;
    function GetScaledCommandLinkGlyphSize(AScaleFactor: TdxScaleFactor): TSize; virtual;
    function GetScaledCommandLinkMargins(AScaleFactor: TdxScaleFactor): TRect; virtual;

    // Header
    procedure DrawHeader(ACanvas: TcxCanvas; const ABounds, ATextAreaBounds: TRect; ANeighbors: TcxNeighbors;
      ABorders: TcxBorders; AState: TcxButtonState; AAlignmentHorz: TAlignment; AAlignmentVert: TcxAlignmentVert;
      AMultiLine, AShowEndEllipsis: Boolean; const AText: string; AFont: TFont; ATextColor, ABkColor: TColor;
      AOnDrawBackground: TcxDrawBackgroundEvent = nil; AIsLast: Boolean = False; AIsGroup: Boolean = False);
    procedure DrawHeaderEx(ACanvas: TcxCanvas; const ABounds, ATextAreaBounds: TRect; ANeighbors: TcxNeighbors;
      ABorders: TcxBorders; AState: TcxButtonState; AAlignmentHorz: TAlignment; AAlignmentVert: TcxAlignmentVert;
      AMultiLine, AShowEndEllipsis: Boolean; const AText: string; AFont: TFont; ATextColor, ABkColor: TColor;
      AOnDrawBackground: TcxDrawBackgroundEvent = nil);
    procedure DrawHeaderBorder(ACanvas: TcxCanvas; const R: TRect; ANeighbors: TcxNeighbors; ABorders: TcxBorders); virtual;
    procedure DrawHeaderPressed(ACanvas: TcxCanvas; const ABounds: TRect); virtual;
    procedure DrawHeaderControlSection(ACanvas: TcxCanvas; const ABounds, ATextAreaBounds: TRect; ANeighbors: TcxNeighbors;
      ABorders: TcxBorders; AState: TcxButtonState; AAlignmentHorz: TAlignment; AAlignmentVert: TcxAlignmentVert;
      AMultiLine, AShowEndEllipsis: Boolean; const AText: string; AFont: TFont; ATextColor, ABkColor: TColor);
    procedure DrawHeaderControlSectionBorder(ACanvas: TcxCanvas; const R: TRect; ABorders: TcxBorders; AState: TcxButtonState); virtual;
    procedure DrawHeaderControlSectionContent(ACanvas: TcxCanvas; const ABounds, ATextAreaBounds: TRect;
      AState: TcxButtonState; AAlignmentHorz: TAlignment; AAlignmentVert: TcxAlignmentVert; AMultiLine, AShowEndEllipsis: Boolean;
      const AText: string; AFont: TFont; ATextColor, ABkColor: TColor); virtual;
    procedure DrawHeaderControlSectionText(ACanvas: TcxCanvas; const ATextAreaBounds: TRect; AState: TcxButtonState;
      AAlignmentHorz: TAlignment; AAlignmentVert: TcxAlignmentVert; AMultiLine, AShowEndEllipsis: Boolean;
      const AText: string; AFont: TFont; ATextColor: TColor); virtual;
    procedure DrawHeaderSeparator(ACanvas: TcxCanvas; const ABounds: TRect;
      AIndentSize: Integer; AColor: TColor; AViewParams: TcxViewParams); virtual;
    procedure DrawScaledHeader(ACanvas: TcxCanvas; const ABounds, ATextAreaBounds: TRect; ANeighbors: TcxNeighbors;
      ABorders: TcxBorders; AState: TcxButtonState; AAlignmentHorz: TAlignment; AAlignmentVert: TcxAlignmentVert;
      AMultiLine, AShowEndEllipsis: Boolean; const AText: string; AFont: TFont; ATextColor, ABkColor: TColor;
      AScaleFactor: TdxScaleFactor; AOnDrawBackground: TcxDrawBackgroundEvent = nil; AIsLast: Boolean = False; AIsGroup: Boolean = False); virtual;
    procedure DrawScaledHeaderEx(ACanvas: TcxCanvas; const ABounds, ATextAreaBounds: TRect; ANeighbors: TcxNeighbors;
      ABorders: TcxBorders; AState: TcxButtonState; AAlignmentHorz: TAlignment; AAlignmentVert: TcxAlignmentVert;
      AMultiLine, AShowEndEllipsis: Boolean; const AText: string; AFont: TFont; ATextColor, ABkColor: TColor;
      AScaleFactor: TdxScaleFactor; AOnDrawBackground: TcxDrawBackgroundEvent = nil); overload; virtual;
    procedure DrawScaledHeaderControlSection(ACanvas: TcxCanvas; const ABounds, ATextAreaBounds: TRect; ANeighbors: TcxNeighbors;
      ABorders: TcxBorders; AState: TcxButtonState; AAlignmentHorz: TAlignment; AAlignmentVert: TcxAlignmentVert;
      AMultiLine, AShowEndEllipsis: Boolean; const AText: string; AFont: TFont; ATextColor, ABkColor: TColor; AScaleFactor: TdxScaleFactor); overload; virtual;
    function HeaderBorders(ANeighbors: TcxNeighbors): TcxBorders; virtual;
    function HeaderBorderSize: Integer; virtual;
    function HeaderBounds(const ABounds: TRect; ANeighbors: TcxNeighbors; ABorders: TcxBorders = cxBordersAll): TRect; virtual;
    function HeaderContentBounds(const ABounds: TRect; ABorders: TcxBorders): TRect;
    function HeaderContentOffsets(AScaleFactor: TdxScaleFactor): TRect; virtual;
    function HeaderControlSectionBorderSize(AState: TcxButtonState = cxbsNormal): Integer; virtual;
    function HeaderControlSectionContentBounds(const ABounds: TRect; AState: TcxButtonState): TRect; virtual;
    function HeaderControlSectionTextAreaBounds(ABounds: TRect; AState: TcxButtonState): TRect; virtual;
    function HeaderDrawCellsFirst: Boolean; virtual;
    function HeaderHeight(AFontHeight: Integer): Integer;
    function HeaderWidth(ACanvas: TcxCanvas; ABorders: TcxBorders; const AText: string; AFont: TFont): Integer;
    function IsHeaderHotTrack: Boolean; virtual;
    function ScaledHeaderContentBounds(const ABounds: TRect; ABorders: TcxBorders; AScaleFactor: TdxScaleFactor): TRect; virtual;
    function ScaledHeaderHeight(AFontHeight: Integer; AScaleFactor: TdxScaleFactor): Integer; virtual;
    function ScaledHeaderWidth(ACanvas: TcxCanvas; ABorders: TcxBorders; const AText: string; AFont: TFont; AScaleFactor: TdxScaleFactor): Integer; virtual;

    // Sorting Marks
    procedure DrawScaledSortingMark(ACanvas: TcxCanvas; const R: TRect; AAscendingSorting: Boolean; AScaleFactor: TdxScaleFactor); virtual; abstract;
    procedure DrawScaledSummarySortingMark(ACanvas: TcxCanvas; const R: TRect; AAscendingSorting: Boolean; AScaleFactor: TdxScaleFactor); virtual; abstract;
    procedure DrawScaledSummaryValueSortingMark(ACanvas: TcxCanvas; const R: TRect; AAscendingSorting: Boolean; AScaleFactor: TdxScaleFactor); virtual;
    procedure DrawSortingMark(ACanvas: TcxCanvas; const R: TRect; AAscendingSorting: Boolean);
    procedure DrawSummarySortingMark(ACanvas: TcxCanvas; const R: TRect; AAscendingSorting: Boolean);
    procedure DrawSummaryValueSortingMark(ACanvas: TcxCanvas; const R: TRect; AAscendingSorting: Boolean);
    function SortingMarkAreaSize: TPoint;
    function SortingMarkSize: TPoint;
    function SummarySortingMarkSize: TPoint;
    function SummaryValueSortingMarkSize: TPoint;
    function ScaledSortingMarkAreaSize(AScaleFactor: TdxScaleFactor): TPoint; virtual;
    function ScaledSortingMarkSize(AScaleFactor: TdxScaleFactor): TPoint; virtual; abstract;
    function ScaledSummarySortingMarkSize(AScaleFactor: TdxScaleFactor): TPoint; virtual; abstract;
    function ScaledSummaryValueSortingMarkSize(AScaleFactor: TdxScaleFactor): TPoint; overload; virtual;

    // Grid
    procedure DrawFilterRowSeparator(ACanvas: TcxCanvas; const ARect: TRect; ABackgroundColor: TColor); virtual;
    procedure DrawGroupByBox(ACanvas: TcxCanvas; const ARect: TRect;
      ATransparent: Boolean; ABackgroundColor: TColor; const ABackgroundBitmap: TBitmap); virtual;
    function GridBordersOverlapSize: Integer; virtual;
    function GridGroupRowStyleOffice11ContentColor(AHasData: Boolean): TColor; virtual;
    function GridGroupRowStyleOffice11SeparatorColor: TColor; virtual;
    function GridGroupRowStyleOffice11TextColor: TColor; virtual;
    function GridDrawHeaderCellsFirst: Boolean; virtual;
    function PivotGridHeadersAreaColor: TColor; virtual;
    function PivotGridHeadersAreaTextColor: TColor; virtual;
    // Grid like common
    function GridLikeControlBackgroundColor: TColor; virtual;
    function GridLikeControlContentColor: TColor; virtual;
    function GridLikeControlContentEvenColor: TColor; virtual;
    function GridLikeControlContentOddColor: TColor; virtual;
    function GridLikeControlContentTextColor: TColor; virtual;
    function GridLikeControlDefaultUseOddEvenStyle: Boolean; virtual;
    // Layout View
    procedure LayoutViewDrawItem(ACanvas: TcxCanvas; const ABounds: TRect;
      AState: TcxButtonState; ABorders: TcxBorders = []); virtual;
    procedure LayoutViewDrawRecordCaption(ACanvas: TcxCanvas; const ABounds, ATextRect: TRect;
      APosition: TcxGroupBoxCaptionPosition; AState: TcxButtonState; AColor: TColor = clDefault;
      const ABitmap: TBitmap = nil); virtual;
    procedure LayoutViewDrawRecordBorder(ACanvas: TcxCanvas; const ABounds: TRect;
      AState: TcxButtonState; ABorders: TcxBorders = []); virtual;
    procedure LayoutViewDrawRecordContent(ACanvas: TcxCanvas; const ABounds: TRect;
      ACaptionPosition: TcxGroupBoxCaptionPosition; AState: TcxButtonState; ABorders: TcxBorders = cxBordersAll); virtual;
    procedure LayoutViewDrawScaledRecordExpandButton(ACanvas: TcxCanvas;
      const ABounds: TRect; AState: TcxButtonState; AExpanded: Boolean; AScaleFactor: TdxScaleFactor); virtual;
    procedure LayoutViewDrawRecordExpandButton(ACanvas: TcxCanvas;
      const ABounds: TRect; AState: TcxButtonState; AExpanded: Boolean);
    function LayoutViewGetPadding(AElement: TcxLayoutElement): TRect; virtual;
    function LayoutViewGetSpacing(AElement: TcxLayoutElement): TRect; virtual;
    function LayoutViewRecordCaptionTailSize(ACaptionPosition: TcxGroupBoxCaptionPosition): Integer; virtual;
    function LayoutViewRecordCaptionTextBold: Boolean; virtual;

    //WinExplorer View
    function WinExplorerViewDefaultRecordColor(AState: TcxButtonState): TColor; virtual;
    procedure WinExplorerViewDrawGroup(ACanvas: TcxCanvas; const ABounds: TRect; AState: TcxButtonState;
      AColor: TColor = clDefault; const ABitmap: TBitmap = nil); virtual;
    procedure WinExplorerViewDrawGroupCaptionLine(ACanvas: TcxCanvas; const ABounds: TRect); virtual;
    procedure WinExplorerViewDrawRecord(ACanvas: TcxCanvas; const ABounds: TRect; AState: TcxButtonState;
      AColor: TColor = clDefault; const ABitmap: TBitmap = nil); virtual;
    procedure WinExplorerViewDrawRecordExpandButton(ACanvas: TcxCanvas; const ABounds: TRect; AState: TcxButtonState; AExpanded: Boolean);
    procedure WinExplorerViewDrawScaledRecordExpandButton(ACanvas: TcxCanvas; const ABounds: TRect; AState: TcxButtonState; AExpanded: Boolean; AScaleFactor: TdxScaleFactor); virtual;
    function WinExplorerViewExpandButtonSize: Integer;
    function WinExplorerViewGroupCaptionLineHeight: Integer; virtual;
    function WinExplorerViewGroupTextBold: Boolean; virtual;
    function WinExplorerViewGroupTextColor(AState: TcxButtonState): TColor; virtual;
    function WinExplorerViewRecordTextColor(AState: TcxButtonState): TColor; virtual;
    function WinExplorerViewScaledExpandButtonSize(AScaleFactor: TdxScaleFactor): Integer; overload; virtual;

    // Chart View
    function ChartToolBoxDataLevelInfoBorderSize: Integer; virtual;

    // Footer
    function FooterBorders: TcxBorders; virtual;
    function FooterBorderSize: Integer; virtual; // abstract;
    function FooterCellBorderSize: Integer; virtual; // abstract;
    function FooterCellOffset: Integer; virtual;// abstract;
    function FooterDrawCellsFirst: Boolean; virtual;
    function FooterSeparatorColor: TColor; virtual;
    function FooterSeparatorSize: Integer; virtual;
    procedure DrawFooterPanel(ACanvas: TcxCanvas; const ABounds: TRect; const AViewParams: TcxViewParams; ABorders: TcxBorders);
    procedure DrawFooterCell(ACanvas: TcxCanvas; const ABounds: TRect; AAlignmentHorz: TAlignment;
      AAlignmentVert: TcxAlignmentVert; AMultiLine: Boolean; const AText: string; AFont: TFont;
      ATextColor, ABkColor: TColor; AOnDrawBackground: TcxDrawBackgroundEvent = nil); virtual;
    procedure DrawFooterCellContent(ACanvas: TcxCanvas; const ABounds: TRect; AAlignmentHorz: TAlignment;
      AAlignmentVert: TcxAlignmentVert; AMultiLine: Boolean; const AText: string; AFont: TFont;
      ATextColor, ABkColor: TColor; AOnDrawBackground: TcxDrawBackgroundEvent = nil); virtual;
    procedure DrawFooterBorder(ACanvas: TcxCanvas; const R: TRect); virtual; // abstract;
    procedure DrawFooterBorderEx(ACanvas: TcxCanvas; const R: TRect; ABorders: TcxBorders); virtual;
    procedure DrawFooterCellBorder(ACanvas: TcxCanvas; const R: TRect); virtual; // abstract;
    procedure DrawFooterContent(ACanvas: TcxCanvas; const ARect: TRect; const AViewParams: TcxViewParams); virtual;
    procedure DrawFooterSeparator(ACanvas: TcxCanvas; const R: TRect); virtual;

    // Filter
    procedure DrawFilterActivateButton(ACanvas: TcxCanvas; R: TRect; AState: TcxButtonState; AChecked: Boolean);
    procedure DrawFilterCloseButton(ACanvas: TcxCanvas; R: TRect; AState: TcxButtonState);
    procedure DrawFilterDropDownButton(ACanvas: TcxCanvas; R: TRect; AState: TcxButtonState; AIsFilterActive: Boolean);
    procedure DrawFilterPanel(ACanvas: TcxCanvas; const ARect: TRect; ATransparent: Boolean; ABackgroundColor: TColor; const ABackgroundBitmap: TGraphic); virtual;
    procedure DrawFilterSmartTag(ACanvas: TcxCanvas; R: TRect; AState: TcxFilterSmartTagState; AIsFilterActive: Boolean);
    procedure DrawScaledFilterActivateButton(ACanvas: TcxCanvas; R: TRect; AState: TcxButtonState; AChecked: Boolean; AScaleFactor: TdxScaleFactor); overload; virtual;
    procedure DrawScaledFilterCloseButton(ACanvas: TcxCanvas; R: TRect; AState: TcxButtonState; AScaleFactor: TdxScaleFactor); virtual;
    procedure DrawScaledFilterDropDownButton(ACanvas: TcxCanvas; R: TRect; AState: TcxButtonState; AIsFilterActive: Boolean; AScaleFactor: TdxScaleFactor); virtual; abstract;
    procedure DrawScaledFilterSmartTag(ACanvas: TcxCanvas; R: TRect; AState: TcxFilterSmartTagState; AIsFilterActive: Boolean; AScaleFactor: TdxScaleFactor); overload; virtual;
    function FilterActivateButtonSize: TPoint;
    function FilterCloseButtonSize: TPoint;
    function FilterControlMenuGetColorPalette: IdxColorPalette; virtual;
    function FilterDropDownButtonSize: TPoint;
    function FilterSmartTagSize: TSize;
    function ScaledFilterActivateButtonSize(AScaleFactor: TdxScaleFactor): TPoint; virtual;
    function ScaledFilterCloseButtonSize(AScaleFactor: TdxScaleFactor): TPoint; virtual;
    function ScaledFilterDropDownButtonSize(AScaleFactor: TdxScaleFactor): TPoint; virtual;
    function ScaledFilterSmartTagSize(AScaleFactor: TdxScaleFactor): TSize; virtual;

    // Find Panel
    procedure DrawFindPanel(ACanvas: TcxCanvas; const ARect: TRect;
      ATransparent: Boolean; ABackgroundColor: TColor; const ABackgroundBitmap: TGraphic); virtual;
    procedure DrawFindPanelBorder(ACanvas: TcxCanvas; const R: TRect; ABorders: TcxBorders); virtual;

    // Window Content
    function GetWindowContentTextColor: TColor; virtual;
    procedure DrawWindowContent(ACanvas: TcxCanvas; const ARect: TRect); virtual;

    // Popup
    procedure DrawEditPopupWindowBorder(ACanvas: TcxCanvas; var R: TRect;
      ABorderStyle: TcxEditPopupBorderStyle; AClientEdge: Boolean); virtual;
    function GetEditPopupWindowBorderWidth(AStyle: TcxEditPopupBorderStyle): Integer; virtual;
    function GetEditPopupWindowClientEdgeWidth(AStyle: TcxEditPopupBorderStyle): Integer; virtual;
    function PopupBorderStyle: TcxPopupBorderStyle; virtual;
    // Hints
      // Attributes
    function GetHintBorderColor: TColor; virtual;
      // Draw
    procedure DrawHintBackground(ACanvas: TcxCanvas; const ARect: TRect; AColor: TColor = clDefault); virtual;

    // ScreenTips
    function ScreenTipGetColorPalette: IdxColorPalette; virtual;
    function ScreenTipGetDescriptionTextColor: TColor; virtual;
    function ScreenTipGetFooterLineSize: Integer; virtual;
    function ScreenTipGetTitleTextColor: TColor; virtual;
    procedure ScreenTipDrawBackground(ACanvas: TcxCanvas; ARect: TRect); virtual;
    procedure ScreenTipDrawFooterLine(ACanvas: TcxCanvas; const ARect: TRect); virtual;

    // Tabs
    procedure DrawTab(ACanvas: TcxCanvas; R: TRect; ABorders: TcxBorders;
      const AText: string; AState: TcxButtonState; AVertical: Boolean; AFont: TFont;
      ATextColor, ABkColor: TColor; AShowPrefix: Boolean = False); virtual;
    procedure DrawTabBorder(ACanvas: TcxCanvas; R: TRect; ABorder: TcxBorder; ABorders: TcxBorders; AVertical: Boolean); virtual; // abstract;
    procedure DrawTabsRoot(ACanvas: TcxCanvas; const R: TRect; ABorders: TcxBorders; AVertical: Boolean); virtual; // abstract;
    function IsDrawTabImplemented(AVertical: Boolean): Boolean; virtual;
    function IsTabHotTrack(AVertical: Boolean): Boolean; virtual;
    function TabBorderSize(AVertical: Boolean): Integer; virtual;

    // Indicator
    procedure DrawIndicatorCustomizationMark(ACanvas: TcxCanvas; const R: TRect; AColor: TColor);
    procedure DrawIndicatorImage(ACanvas: TcxCanvas; const R: TRect; AKind: TcxIndicatorKind);
    procedure DrawIndicatorItem(ACanvas: TcxCanvas; const R, AImageAreaBounds: TRect;
      AKind: TcxIndicatorKind; AColor: TColor; AOnDrawBackground: TcxDrawBackgroundEvent = nil); overload;
    procedure DrawIndicatorItem(ACanvas: TcxCanvas; const R: TRect;
      AKind: TcxIndicatorKind; AColor: TColor; AOnDrawBackground: TcxDrawBackgroundEvent = nil); overload;
    procedure DrawIndicatorItemEx(ACanvas: TcxCanvas; const R: TRect;
      AKind: TcxIndicatorKind; AColor: TColor; AOnDrawBackground: TcxDrawBackgroundEvent = nil);
    procedure DrawScaledIndicatorImage(ACanvas: TcxCanvas; const R: TRect; AKind: TcxIndicatorKind; AScaleFactor: TdxScaleFactor); virtual;
    procedure DrawScaledIndicatorItem(ACanvas: TcxCanvas; const R, AImageAreaBounds: TRect;
      AKind: TcxIndicatorKind; AColor: TColor; AScaleFactor: TdxScaleFactor; AOnDrawBackground: TcxDrawBackgroundEvent = nil;
      ANeighbors: TcxNeighbors = [nTop, nBottom]); overload; virtual;
    procedure DrawScaledIndicatorItem(ACanvas: TcxCanvas; const R: TRect; AKind: TcxIndicatorKind;
      AColor: TColor; AScaleFactor: TdxScaleFactor; AOnDrawBackground: TcxDrawBackgroundEvent = nil); overload; virtual;
    procedure DrawScaledIndicatorItemEx(ACanvas: TcxCanvas; const R: TRect; AKind: TcxIndicatorKind;
      AColor: TColor; AScaleFactor: TdxScaleFactor; AOnDrawBackground: TcxDrawBackgroundEvent = nil); virtual;
    procedure DrawScaledIndicatorCustomizationMark(ACanvas: TcxCanvas; const R: TRect; AColor: TColor; AScaleFactor: TdxScaleFactor); virtual;
    function IndicatorDrawItemsFirst: Boolean; virtual;

    // ScrollBars
    procedure DrawScrollBarBackground(ACanvas: TcxCanvas; const R: TRect; AHorizontal: Boolean);
    procedure DrawScrollBarPart(ACanvas: TcxCanvas; AHorizontal: Boolean; R: TRect; APart: TcxScrollBarPart; AState: TcxButtonState);
    procedure DrawScrollBarSplitter(ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState);
    procedure DrawScaledScrollBarBackground(ACanvas: TcxCanvas; const R: TRect; AHorizontal: Boolean; AScaleFactor: TdxScaleFactor); virtual;
    procedure DrawScaledScrollBarPart(ACanvas: TcxCanvas; AHorizontal: Boolean; R: TRect; APart: TcxScrollBarPart; AState: TcxButtonState; AScaleFactor: TdxScaleFactor); virtual;
    procedure DrawScaledScrollBarSplitter(ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState; AScaleFactor: TdxScaleFactor); virtual;
    function ScaledScrollBarMinimalThumbSize(AVertical: Boolean; AScaleFactor: TdxScaleFactor): Integer; virtual;
    function ScrollBarMinimalThumbSize(AVertical: Boolean): Integer;

    // PopupPanel
    function PopupPanelSize: Integer;
    function ScaledPopupPanelSize(AScaleFactor: TdxScaleFactor): Integer;
    procedure CalculatePopupPanelClientRect(var AClientRect, APanelRect, AGripRect, ACloseButtonRect: TRect;
      ACorner: TdxCorner; const ABorders, APanelBorders: TRect; APanelHeight: Integer = 0;
      AShowCloseButton: Boolean = True; AShowGripSize: Boolean = True);
    procedure CalculateScaledPopupPanelClientRect(var AClientRect, APanelRect, AGripRect, ACloseButtonRect: TRect;
      ACorner: TdxCorner; const ABorders, APanelBorders: TRect; AScaleFactor: TdxScaleFactor;
      APanelHeight: Integer = 0; AShowCloseButton: Boolean = True; AShowGripSize: Boolean = True); virtual;
    procedure DrawPopupNCPanel(AHandle: HWND; AMouseAboveCloseButton, ACloseButtonIsTracking: Boolean;
      ACorner: TdxCorner; ACloseButtonRect, AGripRect: TRect; ABorderColor: TColor);
    procedure DrawPopupPanelBand(ACanvas: TcxCanvas; const ABounds, AGripRect, ACloseButtonRect: TRect;
      AGripCorner: TdxCorner; ACloseButtonState: TcxButtonState; ABorders: TRect; ABorderColor: TColor;
      AShowGripSize: Boolean = True; AShowCloseButton: Boolean = True);
    procedure DrawScaledPopupPanelBand(ACanvas: TcxCanvas; const ABounds, AGripRect, ACloseButtonRect: TRect;
      AGripCorner: TdxCorner; ACloseButtonState: TcxButtonState; ABorders: TRect; ABorderColor: TColor; AScaleFactor: TdxScaleFactor;
      AShowGripSize: Boolean = True; AShowCloseButton: Boolean = True);
    procedure DrawScaledPopupNCPanel(AHandle: HWND; AMouseAboveCloseButton, ACloseButtonIsTracking: Boolean;
      ACorner: TdxCorner; ACloseButtonRect, AGripRect: TRect; ABorderColor: TColor; AScaleFactor: TdxScaleFactor); virtual;

    // Sizegrip
    procedure DoDrawSizeGrip(ACanvas: TcxCanvas; const ARect: TRect); overload;
    procedure DoDrawSizeGrip(ACanvas: TcxCanvas; const ARect: TRect; AScaleFactor: TdxScaleFactor); overload; virtual;
    procedure DrawSizeGrip(ACanvas: TcxCanvas; const ARect: TRect;
      ABackgroundColor: TColor = clDefault; ACorner: TdxCorner = coBottomRight); virtual;
    procedure DrawScaledSizeGrip(ACanvas: TcxCanvas; const ARect: TRect; AScaleFactor: TdxScaleFactor;
      ABackgroundColor: TColor = clDefault; ACorner: TdxCorner = coBottomRight); virtual;
    function ScaledSizeGripSize(AScaleFactor: TdxScaleFactor): TSize; virtual;
    function SizeGripSize: TSize;

    // Slider
    function ScaledSliderButtonSize(ADirection: TcxArrowDirection; AScaleFactor: TdxScaleFactor): TSize; virtual;
    function SliderButtonSize(ADirection: TcxArrowDirection): TSize;
    procedure DrawSliderButton(ACanvas: TcxCanvas; const ARect: TRect;
      ADirection: TcxArrowDirection; AState: TcxButtonState);
    procedure DrawScaledSliderButton(ACanvas: TcxCanvas; const ARect: TRect;
      ADirection: TcxArrowDirection; AState: TcxButtonState; AScaleFactor: TdxScaleFactor); virtual;

    // SmallButton / SmallCloseButton
    function DoGetSmallCloseButtonSize: TSize; virtual;
    procedure DrawSmallButton(ACanvas: TcxCanvas; R: TRect; AState: TcxButtonState);
    procedure DrawSmallCloseButton(ACanvas: TcxCanvas; R: TRect; AState: TcxButtonState);
    procedure DrawScaledSmallButton(ACanvas: TcxCanvas; R: TRect; AState: TcxButtonState; AScaleFactor: TdxScaleFactor); virtual;
    procedure DrawScaledSmallCloseButton(ACanvas: TcxCanvas; R: TRect; AState: TcxButtonState; AScaleFactor: TdxScaleFactor); virtual;
    function GetSmallButtonColorPalette(AState: TcxButtonState): IdxColorPalette; virtual;
    function SmallCloseButtonSize: TSize;
    function ScaledSmallCloseButtonSize(AScaleFactor: TdxScaleFactor): TSize; virtual;

    // Scheduler
    procedure CalculateSchedulerNavigationButtonRects(AIsNextButton: Boolean; ACollapsed: Boolean;
      APrevButtonTextSize: TSize; ANextButtonTextSize: TSize; var ABounds: TRect; out ATextRect: TRect;
      out AArrowRect: TRect; AScaleFactor: TdxScaleFactor; const AIsVertical: Boolean = True); overload; virtual;
    procedure CalculateSchedulerNavigationButtonRects(AIsNextButton: Boolean; ACollapsed: Boolean;
      APrevButtonTextSize: TSize; ANextButtonTextSize: TSize; var ABounds: TRect; out ATextRect: TRect;
      out AArrowRect: TRect; const AIsVertical: Boolean = True); overload; virtual;
    procedure DrawScaledMonthHeader(ACanvas: TcxCanvas; const ABounds: TRect; const AText: string; ANeighbors: TcxNeighbors;
      const AViewParams: TcxViewParams; AArrows: TcxArrowDirections; ASideWidth: Integer; AScaleFactor: TdxScaleFactor;
      AOnDrawBackground: TcxDrawBackgroundEvent = nil); virtual;
    procedure DrawMonthHeader(ACanvas: TcxCanvas; const ABounds: TRect; const AText: string; ANeighbors: TcxNeighbors;
      const AViewParams: TcxViewParams; AArrows: TcxArrowDirections; ASideWidth: Integer;
      AOnDrawBackground: TcxDrawBackgroundEvent = nil);
    procedure DrawSchedulerBorder(ACanvas: TcxCanvas; R: TRect); virtual;
    procedure DrawSchedulerDayHeader(ACanvas: TcxCanvas; const ABounds, ATextAreaBounds: TRect; ANeighbors: TcxNeighbors;
      ABorders: TcxBorders; AState: TcxButtonState; AAlignmentHorz: TAlignment; AAlignmentVert: TcxAlignmentVert;
      AMultiLine, AShowEndEllipsis: Boolean; const AText: string; AFont: TFont; ATextColor, ABkColor: TColor;
      AScaleFactor: TdxScaleFactor; AOnDrawBackground: TcxDrawBackgroundEvent = nil; AIsLast: Boolean = False; AIsGroup: Boolean = False); virtual;
    procedure DrawSchedulerEventProgress(ACanvas: TcxCanvas;
      const ABounds, AProgress: TRect; AViewParams: TcxViewParams; ATransparent: Boolean); virtual;
    procedure DrawSchedulerGroup(ACanvas: TcxCanvas; const R: TRect; AColor: TColor = clDefault); virtual;
    procedure DrawSchedulerScaledGroupSeparator(ACanvas: TcxCanvas; const ABounds: TRect;
      ABackgroundColor: TColor; AScaleFactor: TdxScaleFactor; AOnDrawBackground: TcxDrawBackgroundEvent = nil); virtual;
    procedure DrawSchedulerGroupSeparator(ACanvas: TcxCanvas; const ABounds: TRect;
      ABackgroundColor: TColor; AOnDrawBackground: TcxDrawBackgroundEvent = nil);
    procedure DrawSchedulerMilestone(ACanvas: TcxCanvas; const R: TRect); virtual;
    procedure DrawSchedulerNavigationButton(ACanvas: TcxCanvas;
      const ARect: TRect; AIsNextButton: Boolean; AState: TcxButtonState;
      const AText: string; const ATextRect: TRect; const AArrowRect: TRect; const AIsVertical: Boolean = True); virtual;
    procedure DrawSchedulerNavigationButtonArrow(ACanvas: TcxCanvas;
      const ARect: TRect; AIsNextButton: Boolean; AColor: TColor; const AForVertical: Boolean); virtual;
    procedure DrawSchedulerScaledNavigatorButton(ACanvas: TcxCanvas; R: TRect;
      AState: TcxButtonState; AScaleFactor: TdxScaleFactor); virtual;
    procedure DrawSchedulerNavigatorButton(ACanvas: TcxCanvas; R: TRect; AState: TcxButtonState);
    procedure DrawSchedulerSplitterBorder(ACanvas: TcxCanvas; R: TRect; const AViewParams: TcxViewParams; AIsHorizontal: Boolean); virtual;
    function SchedulerEventProgressOffsets: TRect; virtual;
    function SchedulerNavigationButtonTextColor(AIsNextButton: Boolean;
      AState: TcxButtonState; ADefaultColor: TColor = clDefault): TColor; virtual;
    procedure SchedulerNavigationButtonSizes(AIsNextButton: Boolean; var ABorders: TRect; var AArrowSize: TSize;
      var AHasTextArea: Boolean; AScaleFactor: TdxScaleFactor; const AIsVertical: Boolean = True); overload; virtual;
    procedure SchedulerNavigationButtonSizes(AIsNextButton: Boolean; var ABorders: TRect; var AArrowSize: TSize;
      var AHasTextArea: Boolean; const AIsVertical: Boolean = True); overload; virtual;

    // Editors
    procedure DrawEditorButton(ACanvas: TcxCanvas; const ARect: TRect; AButtonKind: TcxEditBtnKind;
      AState: TcxButtonState; APosition: TcxEditBtnPosition = cxbpRight);
    procedure DrawEditorButtonGlyph(ACanvas: TcxCanvas; const ARect: TRect;
      AButtonKind: TcxEditBtnKind; AState: TcxButtonState; APosition: TcxEditBtnPosition = cxbpRight);
    procedure DrawScaledEditorButton(ACanvas: TcxCanvas; const ARect: TRect; AButtonKind: TcxEditBtnKind;
      AState: TcxButtonState; AScaleFactor: TdxScaleFactor; APosition: TcxEditBtnPosition = cxbpRight); virtual;
    procedure DrawScaledEditorButtonGlyph(ACanvas: TcxCanvas; const ARect: TRect; AButtonKind: TcxEditBtnKind;
      AState: TcxButtonState; AScaleFactor: TdxScaleFactor; APosition: TcxEditBtnPosition = cxbpRight); virtual;
    function EditButtonColorPalette(AState: TcxButtonState): IdxColorPalette; virtual;
    function EditButtonSize: TSize; virtual;
    function EditButtonTextColor: TColor; virtual;
    function EditButtonTextOffset: Integer; virtual;
    function GetContainerBorderColor(AIsHighlightBorder: Boolean): TColor; virtual;
    function GetContainerBorderWidth(ABorderStyle: TcxContainerBorderStyle): Integer; virtual;

    // Clock
    function ClockSize: TSize;
    procedure DrawClock(ACanvas: TcxCanvas; const ARect: TRect; ADateTime: TDateTime; ABackgroundColor: TColor);
    procedure DrawScaledClock(ACanvas: TcxCanvas; const ARect: TRect; ADateTime: TDateTime; ABackgroundColor: TColor; AScaleFactor: TdxScaleFactor); overload; virtual;
    function ScaledClockSize(AScaleFactor: TdxScaleFactor): TSize; virtual;

    // ZoomButtons
    procedure DrawScaledZoomInButton(ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState; AScaleFactor: TdxScaleFactor); virtual;
    procedure DrawScaledZoomOutButton(ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState; AScaleFactor: TdxScaleFactor); virtual;
    procedure DrawZoomInButton(ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState);
    procedure DrawZoomOutButton(ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState);
    function GetScaledZoomInButtonSize(AScaleFactor: TdxScaleFactor): TSize; virtual;
    function GetScaledZoomOutButtonSize(AScaleFactor: TdxScaleFactor): TSize; virtual;
    function GetZoomInButtonSize: TSize;
    function GetZoomOutButtonSize: TSize;

    // BackButton
    function GetBackButtonSize: TSize;
    function GetScaledBackButtonSize(AScaleFactor: TdxScaleFactor): TSize; virtual;
    procedure DrawBackButton(ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState);
    procedure DrawScaledBackButton(ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState; AScaleFactor: TdxScaleFactor); virtual;

    // DateNavigator
    procedure DrawDateNavigatorCellSelection(ACanvas: TcxCanvas; const R: TRect; AColor: TColor); virtual;
    procedure DrawDateNavigatorDateHeader(ACanvas: TcxCanvas; var R: TRect); virtual;
    procedure DrawDateNavigatorTodayCellSelection(ACanvas: TcxCanvas; const R: TRect); virtual;

    // Navigator
    procedure DrawNavigatorBorder(ACanvas: TcxCanvas; R: TRect; ASelected: Boolean); virtual;
    procedure DrawNavigatorScaledButton(ACanvas: TcxCanvas; R: TRect; AState: TcxButtonState;
      ABackgroundColor: TColor; AScaleFactor: TdxScaleFactor); overload; virtual;
    procedure DrawNavigatorButton(ACanvas: TcxCanvas; R: TRect; AState: TcxButtonState; ABackgroundColor: TColor);
    procedure DrawNavigatorButtonGlyph(ACanvas: TcxCanvas; AImageList: TCustomImageList; AImageIndex: TcxImageIndex;
      const AGlyphRect: TRect; AEnabled: Boolean; AUserGlyphs: Boolean);
    procedure DrawNavigatorScaledButtonGlyph(ACanvas: TcxCanvas; AImageList: TCustomImageList; AImageIndex: TcxImageIndex;
      const AGlyphRect: TRect; AEnabled: Boolean; AUserGlyphs: Boolean; AScaleFactor: TdxScaleFactor); overload; virtual;
    function NavigatorBorderOverlap: Boolean; virtual;
    function NavigatorBorderSize: Integer; virtual;
    function NavigatorButtonColorPalette(AEnabled: Boolean): IdxColorPalette; virtual;
    function NavigatorButtonGlyphPadding: TRect;
    function NavigatorButtonGlyphSize: TSize;
    function NavigatorButtonMinSize: TSize;
    function NavigatorButtonPressedGlyphOffset: TPoint; virtual;
    function NavigatorButtonTextColor(AState: TcxButtonState): TColor; virtual;
    function NavigatorScaledButtonGlyphPadding(AScaleFactor: TdxScaleFactor): TRect; overload; virtual;
    function NavigatorScaledButtonGlyphSize(AScaleFactor: TdxScaleFactor): TSize; overload; virtual;
    function NavigatorScaledButtonMinSize(AScaleFactor: TdxScaleFactor): TSize; overload; virtual;
    function NavigatorInfoPanelColor: TColor; virtual;
    function NavigatorInfoPanelTextColor: TColor; virtual;

    // ProgressBar
    procedure DrawProgressBarBorder(ACanvas: TcxCanvas; ARect: TRect; AVertical: Boolean); virtual;
    procedure DrawProgressBarChunk(ACanvas: TcxCanvas; ARect: TRect; AVertical: Boolean); virtual;
    procedure DrawProgressBarText(ACanvas: TcxCanvas; AVertical, ASolid: Boolean;
      const AText: string; const ATextRect, AProgressBarRect, AProgressChunkRect: TRect;
      ATextColor: TColor = clDefault); virtual;
    function ProgressBarBorderSize(AVertical: Boolean): TRect; virtual;
    function ProgressBarTextColor: TColor; virtual;
    function ProgressBarTextColorEx(AIsFilledArea: Boolean): TColor; virtual;

    // GroupBox
    procedure DrawGroupBoxBackground(ACanvas: TcxCanvas; ABounds: TRect; ARect: TRect); virtual;
    procedure DrawGroupBoxCaption(ACanvas: TcxCanvas; const ACaptionRect, ATextRect: TRect;
      ACaptionPosition: TcxGroupBoxCaptionPosition); virtual;
    procedure DrawGroupBoxContent(ACanvas: TcxCanvas; ABorderRect: TRect;
      ACaptionPosition: TcxGroupBoxCaptionPosition; ABorders: TcxBorders = cxBordersAll); virtual;
    procedure DrawGroupBoxScaledExpandButton(ACanvas: TcxCanvas; const R: TRect;
      AState: TcxButtonState; AExpanded: Boolean; AScaleFactor: TdxScaleFactor; ARotationAngle: TcxRotationAngle = ra0); overload; virtual;
    procedure DrawGroupBoxExpandButton(ACanvas: TcxCanvas; const R: TRect;
      AState: TcxButtonState; AExpanded: Boolean; ARotationAngle: TcxRotationAngle = ra0);
    procedure DrawGroupBoxScaledButton(ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState; AScaleFactor: TdxScaleFactor); overload; virtual;
    procedure DrawGroupBoxButton(ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState);
    procedure DrawGroupBoxScaledExpandGlyph(ACanvas: TcxCanvas; const R: TRect;
      AState: TcxButtonState; AExpanded: Boolean; AScaleFactor: TdxScaleFactor); overload; virtual;
    procedure DrawGroupBoxExpandGlyph(ACanvas: TcxCanvas; const R: TRect;
      AState: TcxButtonState; AExpanded: Boolean);
    procedure DrawGroupBoxFrame(ACanvas: TcxCanvas; R: TRect; AEnabled: Boolean;
      ACaptionPosition: TcxGroupBoxCaptionPosition; ABorders: TcxBorders = cxBordersAll); virtual;
    procedure GroupBoxAdjustCaptionFont(ACaptionFont: TFont; ACaptionPosition: TcxGroupBoxCaptionPosition); virtual;
    function GroupBoxBorderSize(ACaption: Boolean;
      ACaptionPosition: TcxGroupBoxCaptionPosition): TRect; virtual;
    function GroupBoxCaptionTailSize(ACaptionPosition: TcxGroupBoxCaptionPosition): Integer; virtual;
    function GroupBoxTextColor(AEnabled: Boolean; ACaptionPosition: TcxGroupBoxCaptionPosition): TColor; virtual;
    function IsGroupBoxCaptionTextDrawnOverBorder(ACaptionPosition: TcxGroupBoxCaptionPosition): Boolean; virtual;
    function IsGroupBoxTransparent(AIsCaption: Boolean; ACaptionPosition: TcxGroupBoxCaptionPosition): Boolean; virtual;

    // CheckBox
    function CheckBorderSize: Integer; virtual;
    function CheckButtonAreaSize: TSize;
    function CheckButtonColor(AState: TcxButtonState; ACheckState: TcxCheckBoxState): TColor; virtual;
    function CheckButtonSize: TSize;
    function ScaledCheckButtonAreaSize(AScaleFactor: TdxScaleFactor): TSize; virtual;
    function ScaledCheckButtonSize(AScaleFactor: TdxScaleFactor): TSize; overload; virtual;
    procedure DrawCheck(ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState; AChecked: Boolean; AColor: TColor); overload;
    procedure DrawCheck(ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState; ACheckState: TcxCheckBoxState; AColor: TColor); overload;
    procedure DrawCheckBorder(ACanvas: TcxCanvas; R: TRect; AState: TcxButtonState); virtual; // abstract;
    procedure DrawCheckButton(ACanvas: TcxCanvas; R: TRect; AState: TcxButtonState; AChecked: Boolean); overload;
    procedure DrawCheckButton(ACanvas: TcxCanvas; R: TRect; AState: TcxButtonState; ACheckState: TcxCheckBoxState); overload;
    procedure DrawScaledCheck(ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState;
      AChecked: Boolean; AColor: TColor; AScaleFactor: TdxScaleFactor); overload; virtual;
    procedure DrawScaledCheck(ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState;
      ACheckState: TcxCheckBoxState; AColor: TColor; AScaleFactor: TdxScaleFactor); overload; virtual;
    procedure DrawScaledCheckButton(ACanvas: TcxCanvas; R: TRect; AState: TcxButtonState;
      AChecked: Boolean; AScaleFactor: TdxScaleFactor); overload; virtual;
    procedure DrawScaledCheckButton(ACanvas: TcxCanvas; R: TRect; AState: TcxButtonState;
      ACheckState: TcxCheckBoxState; AScaleFactor: TdxScaleFactor); overload; virtual;

    // ToggleSwitch
    procedure DrawScaledToggleSwitch(ACanvas: TcxCanvas; ABounds: TRect; AState: TcxButtonState; AThumbBounds: TRect; AScaleFactor: TdxScaleFactor); virtual;
    procedure DrawScaledToggleSwitchState(ACanvas: TcxCanvas; ABounds: TRect; AState: TcxButtonState; AChecked: Boolean; AScaleFactor: TdxScaleFactor); virtual;
    procedure DrawScaledToggleSwitchThumb(ACanvas: TcxCanvas; ABounds: TRect; AState: TcxButtonState; AScaleFactor: TdxScaleFactor); virtual;
    procedure DrawToggleSwitch(ACanvas: TcxCanvas; ABounds: TRect; AState: TcxButtonState; AThumbBounds: TRect);
    procedure DrawToggleSwitchState(ACanvas: TcxCanvas; ABounds: TRect; AState: TcxButtonState; AChecked: Boolean);
    procedure DrawToggleSwitchStateIndicator(ACanvas: TcxCanvas; ABounds: TRect; AText: string; AFont: TFont); virtual;
    procedure DrawToggleSwitchThumb(ACanvas: TcxCanvas; ABounds: TRect; AState: TcxButtonState);
    function GetToggleSwitchColorPalette: IdxColorPalette; virtual;
    function GetToggleSwitchTextColor: TColor; virtual;
    function GetToggleSwitchThumbPercentsWidth: Integer; virtual;
    function ToggleSwitchToggleColor(AChecked: Boolean): TColor; virtual;

    // RadioButton
    procedure DrawRadioButton(ACanvas: TcxCanvas; X, Y: Integer; AButtonState: TcxButtonState;
      AChecked, AFocused: Boolean; ABrushColor: TColor;  AIsDesigning: Boolean = False);
    procedure DrawScaledRadioButton(ACanvas: TcxCanvas; X, Y: Integer; AButtonState: TcxButtonState;
      AChecked, AFocused: Boolean; ABrushColor: TColor; AScaleFactor: TdxScaleFactor; AIsDesigning: Boolean = False); overload; virtual;
    function RadioButtonBodyColor(AState: TcxButtonState): TColor; virtual;
    function RadioButtonSize: TSize;
    function ScaledRadioButtonSize(AScaleFactor: TdxScaleFactor): TSize; overload; virtual;

    // Label
    procedure DrawLabelLine(ACanvas: TcxCanvas; const R: TRect; AOuterColor, AInnerColor: TColor; AIsVertical: Boolean); virtual;
    function LabelLineHeight: Integer; virtual;

    // GaugeControl
    function GaugeControlBackgroundColor: TColor; virtual;
    procedure DrawGaugeControlBackground(ACanvas: TcxCanvas; const ARect: TRect;
      ATransparent: Boolean; ABackgroundColor: TColor); virtual;

    // MapControl
    function MapControlBackgroundColor: TColor; virtual;
    function MapControlPanelBackColor: TdxAlphaColor; virtual;
    function MapControlPanelHotTrackedTextColor: TdxAlphaColor; virtual;
    function MapControlPanelPressedTextColor: TdxAlphaColor; virtual;
    function MapControlPanelTextColor: TdxAlphaColor; virtual;
    function MapControlGetMapPushpinSize(AScaleFactor: TdxScaleFactor): TSize; virtual;
    function MapControlGetMapPushpinTextOrigin(AScaleFactor: TdxScaleFactor): TPoint; virtual;
    function MapControlMapCustomElementSelectionOffset(AScaleFactor: TdxScaleFactor): TRect; virtual;
    function MapControlMapCustomElementTextColor: TdxAlphaColor; virtual;
    function MapControlMapCustomElementTextGlowColor: TdxAlphaColor; virtual;
    function MapControlMapPushpinTextColor: TdxAlphaColor; virtual;
    function MapControlMapPushpinTextGlowColor: TdxAlphaColor; virtual;
    function MapControlSelectedRegionBackgroundColor: TdxAlphaColor; virtual;
    function MapControlSelectedRegionBorderColor: TdxAlphaColor; virtual;
    function MapControlShapeColor: TdxAlphaColor; virtual;
    function MapControlShapeBorderColor: TdxAlphaColor; virtual;
    function MapControlShapeBorderWidth(AScaleFactor: TdxScaleFactor): Integer; virtual;
    function MapControlShapeHighlightedColor: TdxAlphaColor; virtual;
    function MapControlShapeBorderHighlightedColor: TdxAlphaColor; virtual;
    function MapControlShapeBorderHighlightedWidth(AScaleFactor: TdxScaleFactor): Integer; virtual;
    function MapControlShapeSelectedColor: TdxAlphaColor; virtual;
    function MapControlShapeBorderSelectedColor: TdxAlphaColor; virtual;
    function MapControlShapeBorderSelectedWidth(AScaleFactor: TdxScaleFactor): Integer; virtual;
    procedure DrawMapCustomElementBackground(ACanvas: TcxCanvas; const ARect: TRect; AState: TdxMapControlElementState); virtual;
    procedure DrawMapPushpin(ACanvas: TcxCanvas; const ARect: TRect; AState: TdxMapControlElementState; AScaleFactor: TdxScaleFactor); virtual;

    // OfficeNavigationBar
      //draw
    procedure OfficeNavigationBarDrawCustomizationButton(ACanvas: TcxCanvas;
      const ARect: TRect; AState: TcxCalendarElementState; AColor: TdxAlphaColor = dxacDefault);
    procedure OfficeNavigationBarDrawBackground(ACanvas: TcxCanvas; const ARect: TRect); virtual;
    procedure OfficeNavigationBarDrawImageSelection(ACanvas: TcxCanvas;
      const ARect: TRect; AState: TcxCalendarElementState);
    procedure OfficeNavigationBarButtonItemDrawBackground(ACanvas: TcxCanvas;
      const ARect: TRect; AState: TcxCalendarElementState);
    procedure OfficeNavigationBarItemDrawBackground(ACanvas: TcxCanvas;
      const ARect: TRect; AState: TcxCalendarElementState);
    procedure OfficeNavigationBarDrawScaledButtonItemBackground(ACanvas: TcxCanvas;
      const ARect: TRect; AState: TcxCalendarElementState; AScaleFactor: TdxScaleFactor); virtual;
    procedure OfficeNavigationBarDrawScaledCustomizationButton(ACanvas: TcxCanvas;
      const ARect: TRect; AState: TcxCalendarElementState; AScaleFactor: TdxScaleFactor; AColor: TdxAlphaColor = dxacDefault); virtual;
    procedure OfficeNavigationBarDrawScaledItemBackground(ACanvas: TcxCanvas;
      const ARect: TRect; AState: TcxCalendarElementState; AScaleFactor: TdxScaleFactor); virtual;
    procedure OfficeNavigationBarDrawScaledImageSelection(ACanvas: TcxCanvas;
      const ARect: TRect; AState: TcxCalendarElementState; AScaleFactor: TdxScaleFactor); virtual;
      //sizes
    function OfficeNavigationBarButtonItemContentOffsets: TRect;
    function OfficeNavigationBarButtonItemFontSize: Integer;
    function OfficeNavigationBarButtonItemTextColor(AState: TcxCalendarElementState): TColor; virtual;
    function OfficeNavigationBarContentOffsets: TRect;
    function OfficeNavigationBarCustomizationButtonSize: TSize;
    function OfficeNavigationBarItemContentOffsets: TRect;
    function OfficeNavigationBarItemFontSize: Integer;
    function OfficeNavigationBarItemTextColor(AState: TcxCalendarElementState): TColor; virtual;
    function OfficeNavigationBarScaledButtonItemContentOffsets(AScaleFactor: TdxScaleFactor): TRect; virtual;
    function OfficeNavigationBarScaledButtonItemFontSize(AScaleFactor: TdxScaleFactor): Integer; virtual;
    function OfficeNavigationBarScaledContentOffsets(AScaleFactor: TdxScaleFactor): TRect; virtual;
    function OfficeNavigationBarScaledCustomizationButtonSize(AScaleFactor: TdxScaleFactor): TSize; virtual;
    function OfficeNavigationBarScaledItemContentOffsets(AScaleFactor: TdxScaleFactor): TRect; virtual;
    function OfficeNavigationBarScaledItemFontSize(AScaleFactor: TdxScaleFactor): Integer; virtual;

    // PDFViewer
    function PDFViewerNavigationPaneButtonColorPalette(AState: TcxButtonState): IdxColorPalette; virtual;
    function PDFViewerNavigationPaneButtonContentOffsets(AScaleFactor: TdxScaleFactor): TRect; virtual;
    function PDFViewerNavigationPaneButtonOverlay(AScaleFactor: TdxScaleFactor): TPoint; virtual;
    function PDFViewerNavigationPaneButtonRect(const ARect: TRect; AState: TcxButtonState; ASelected: Boolean;
      AScaleFactor: TdxScaleFactor): TRect;
    function PDFViewerNavigationPaneButtonSize(AScaleFactor: TdxScaleFactor): TSize; virtual;
    function PDFViewerNavigationPaneContentOffsets(AScaleFactor: TdxScaleFactor): TRect; virtual;
    function PDFViewerNavigationPanePageCaptionContentOffsets(AScaleFactor: TdxScaleFactor): TRect; virtual;
    function PDFViewerNavigationPanePageCaptionTextColor: TColor; virtual;
    function PDFViewerNavigationPanePageContentOffsets(AScaleFactor: TdxScaleFactor): TRect; virtual;
    function PDFViewerNavigationPanePageToolbarContentOffsets(AScaleFactor: TdxScaleFactor): TRect; virtual;
    function PDFViewerSelectionColor: TColor; virtual;
    procedure PDFViewerDrawNavigationPaneBackground(ACanvas: TcxCanvas; const ARect: TRect; AScaleFactor: TdxScaleFactor); virtual;
    procedure PDFViewerDrawNavigationPaneButton(ACanvas: TcxCanvas; const ARect: TRect; AState: TcxButtonState;
      AScaleFactor: TdxScaleFactor; AMinimized, ASelected, AIsFirst: Boolean); virtual;
    procedure PDFViewerDrawNavigationPanePageBackground(ACanvas: TcxCanvas; const ARect: TRect); virtual;
    procedure PDFViewerDrawNavigationPanePageButton(ACanvas: TcxCanvas; const ARect: TRect;
      AState: TcxButtonState; AScaleFactor: TdxScaleFactor); virtual;
    procedure PDFViewerDrawNavigationPanePageCaptionBackground(ACanvas: TcxCanvas; const ARect: TRect); virtual;
    procedure PDFViewerDrawNavigationPanePageToolbarBackground(ACanvas: TcxCanvas; const ARect: TRect); virtual;
    procedure PDFViewerDrawFindPanelBackground(ACanvas: TcxCanvas; const R: TRect; ABorders: TcxBorders); virtual;
    procedure PDFViewerDrawPageThumbnailPreviewBackground(ACanvas: TcxCanvas; const ARect: TRect); virtual;

    // SpreadSheet
    procedure DrawSpreadSheetGroupExpandButton(ACanvas: TcxCanvas;
      const R: TRect; AState: TcxButtonState);
    procedure DrawSpreadSheetGroupExpandButtonGlyph(ACanvas: TcxCanvas; const R: TRect;
      AState: TcxButtonState; AExpanded: Boolean; ADefaultGlyphs: TCustomImageList = nil);
    procedure DrawSpreadSheetHeader(ACanvas: TcxCanvas; const ABounds, ATextAreaBounds: TRect; ANeighbors: TcxNeighbors;
      ABorders: TcxBorders; AState: TcxButtonState; AAlignmentHorz: TAlignment; AAlignmentVert: TcxAlignmentVert;
      AMultiLine, AShowEndEllipsis: Boolean; const AText: string; AFont: TFont; ATextColor, ABkColor: TColor;
      AOnDrawBackground: TcxDrawBackgroundEvent = nil; AIsLast: Boolean = False; AIsGroup: Boolean = False);
    procedure DrawSpreadSheetScaledHeader(ACanvas: TcxCanvas; const ABounds, ATextAreaBounds: TRect; ANeighbors: TcxNeighbors;
      ABorders: TcxBorders; AState: TcxButtonState; AAlignmentHorz: TAlignment; AAlignmentVert: TcxAlignmentVert;
      AMultiLine, AShowEndEllipsis: Boolean; const AText: string; AFont: TFont; ATextColor, ABkColor: TColor;
      AScaleFactor: TdxScaleFactor; AOnDrawBackground: TcxDrawBackgroundEvent = nil; AIsLast: Boolean = False; AIsGroup: Boolean = False); overload; virtual;
    procedure DrawSpreadSheetScaledGroupExpandButton(ACanvas: TcxCanvas;
      const R: TRect; AState: TcxButtonState; AScaleFactor: TdxScaleFactor); overload; virtual;
    procedure DrawSpreadSheetScaledGroupExpandButtonGlyph(ACanvas: TcxCanvas; const R: TRect;
      AState: TcxButtonState; AExpanded: Boolean; AScaleFactor: TdxScaleFactor; ADefaultGlyphs: TCustomImageList = nil); overload; virtual;
    function SpreadSheetContentColor: TColor; virtual;
    function SpreadSheetContentTextColor: TColor; virtual;
    function SpreadSheetFrozenPaneSeparatorColor: TColor; virtual;
    function SpreadSheetGroupExpandButtonContentOffsets: TRect;
    function SpreadSheetGroupExpandButtonGlyphSize: TSize;
    function SpreadSheetGroupExpandButtonTextColor(AState: TcxButtonState): TColor; virtual;
    function SpreadSheetGroupLevelMarkSize: TSize;
    function SpreadSheetGroupLineColor: TColor; virtual;
    function SpreadSheetScaledGroupExpandButtonContentOffsets(AScaleFactor: TdxScaleFactor): TRect; virtual;
    function SpreadSheetScaledGroupExpandButtonGlyphSize(AScaleFactor: TdxScaleFactor): TSize; overload; virtual;
    function SpreadSheetScaledGroupLevelMarkSize(AScaleFactor: TdxScaleFactor): TSize; overload; virtual;
    function SpreadSheetSelectionColor: TColor; virtual;

    // SpreadSheetFormulaBar
    procedure DrawSpreadSheetFormulaBarScaledExpandButton(ACanvas: TcxCanvas; const R: TRect;
      AState: TcxButtonState; AExpanded: Boolean; AScaleFactor: TdxScaleFactor); virtual;
    procedure DrawSpreadSheetFormulaBarScaledSeparator(ACanvas: TcxCanvas; const R: TRect; AScaleFactor: TdxScaleFactor); virtual;
    function SpreadSheetFormulaBarGetScaledSeparatorSize(AScaleFactor: TdxScaleFactor): Integer; virtual;

    // Panel
    procedure DrawPanelBackground(ACanvas: TcxCanvas; AControl: TWinControl; ABounds: TRect;
      AColorFrom: TColor = clDefault; AColorTo: TColor = clDefault); virtual;
    procedure DrawPanelBorders(ACanvas: TcxCanvas; const ABorderRect: TRect); virtual;
    procedure DrawPanelCaption(ACanvas: TcxCanvas; const ACaptionRect: TRect; ACaptionPosition: TcxGroupBoxCaptionPosition); virtual;
    procedure DrawPanelContent(ACanvas: TcxCanvas; const ARect: TRect; ADrawBorders: Boolean); virtual;
    function PanelBorderSize: TRect; virtual;
    function PanelTextColor: TColor; virtual;

    // TrackBar
    procedure CorrectThumbRect(ACanvas: TcxCanvas; var ARect: TRect; AHorizontal: Boolean; ATicks: TcxTrackBarTicksAlign); virtual;
    procedure DrawTrackBarScaledTrack(ACanvas: TcxCanvas; const ARect, ASelection: TRect;
      AShowSelection, AEnabled, AHorizontal: Boolean; ATrackColor: TColor; AScaleFactor: TdxScaleFactor); overload; virtual;
    procedure DrawTrackBarTrack(ACanvas: TcxCanvas; const ARect, ASelection: TRect;
      AShowSelection, AEnabled, AHorizontal: Boolean; ATrackColor: TColor);
    procedure DrawTrackBarTrackBounds(ACanvas: TcxCanvas; const ARect: TRect); virtual;
    procedure DrawTrackBarScaledThumb(ACanvas: TcxCanvas; const ARect: TRect; AState: TcxButtonState;
      AHorizontal: Boolean; ATicks: TcxTrackBarTicksAlign; AThumbColor: TColor; AScaleFactor: TdxScaleFactor); overload; virtual;
    procedure DrawTrackBarThumb(ACanvas: TcxCanvas; const ARect: TRect; AState: TcxButtonState;
      AHorizontal: Boolean; ATicks: TcxTrackBarTicksAlign; AThumbColor: TColor);
    procedure DrawTrackBarThumbBorderUpDown(ACanvas: TcxCanvas;
      const ALightPolyLine, AShadowPolyLine, ADarkPolyLine: TPoints); virtual;
    procedure DrawTrackBarThumbBorderBoth(ACanvas: TcxCanvas; const ARect: TRect); virtual;
    function TrackBarScaledThumbSize(AHorizontal: Boolean; AScaleFactor: TdxScaleFactor): TSize; overload; virtual;
    function TrackBarScaledTrackSize(AScaleFactor: TdxScaleFactor): Integer; overload; virtual;
    function TrackBarThumbSize(AHorizontal: Boolean): TSize;
    function TrackBarTicksColor(AText: Boolean): TColor; virtual;
    function TrackBarTrackSize: Integer;

    // RangeControl
    procedure DrawRangeControlScaledLeftThumb(ACanvas: TcxCanvas; const ARect: TRect;
      AColor: TColor; ABorderColor: TdxAlphaColor; AScaleFactor: TdxScaleFactor); overload; virtual;
    procedure DrawRangeControlLeftThumb(ACanvas: TcxCanvas; const ARect: TRect;
      AColor: TColor; ABorderColor: TdxAlphaColor);
    procedure DrawRangeControlScaledRightThumb(ACanvas: TcxCanvas; const ARect: TRect;
      AColor: TColor; ABorderColor: TdxAlphaColor; AScaleFactor: TdxScaleFactor); overload; virtual;
    procedure DrawRangeControlRightThumb(ACanvas: TcxCanvas; const ARect: TRect;
      AColor: TColor; ABorderColor: TdxAlphaColor);
    procedure DrawRangeControlScaledRulerHeader(ACanvas: TcxCanvas; const ARect: TRect; AIsHot: Boolean;
      AColor: TdxAlphaColor; ABorderColor: TdxAlphaColor; AScaleFactor: TdxScaleFactor); overload; virtual;
    procedure DrawRangeControlRulerHeader(ACanvas: TcxCanvas; const ARect: TRect; AIsHot: Boolean;
      AColor: TdxAlphaColor; ABorderColor: TdxAlphaColor);
    procedure DrawRangeControlScaledSizingGlyph(ACanvas: TcxCanvas; const ARect: TRect;
      ABorderColor: TdxAlphaColor; AScaleFactor: TdxScaleFactor); overload; virtual;
    procedure DrawRangeControlSizingGlyph(ACanvas: TcxCanvas; const ARect: TRect;
      ABorderColor: TdxAlphaColor);

    function GetRangeControlBackColor: TColor; virtual;
    function GetRangeControlBorderColor: TColor; virtual;
    function GetRangeControlDefaultElementColor: TColor; virtual;
    function GetRangeControlElementForeColor: TColor; virtual;
    function GetRangeControlElementsBorderColor: TdxAlphaColor; virtual;
    function GetRangeControlLabelColor: TColor; virtual;
    function GetRangeControlOutOfRangeColor: TdxAlphaColor; virtual;
    function GetRangeControlRangePreviewColor: TColor; virtual;
    function GetRangeControlRulerColor: TdxAlphaColor; virtual;
    function GetRangeControlScaledScrollAreaHeight(AScaleFactor: TdxScaleFactor): Integer; virtual;
    function GetRangeControlScaledSizingGlyphSize(AScaleFactor: TdxScaleFactor): TSize; overload; virtual;
    function GetRangeControlScaledThumbSize(AScaleFactor: TdxScaleFactor): TSize; overload; virtual;
    function GetRangeControlScrollAreaColor: TColor; virtual;
    function GetRangeControlScrollAreaHeight: Integer;
    function GetRangeControlSelectedRegionBackgroundColor: TdxAlphaColor; virtual;
    function GetRangeControlSelectedRegionBorderColor: TdxAlphaColor; virtual;
    function GetRangeControlSizingGlyphSize: TSize;
    function GetRangeControlThumbSize: TSize;
    function GetRangeControlViewPortPreviewColor: TColor; virtual;

    // RangeTrackBar
    function RangeTrackBarLeftThumbSize(AHorizontal: Boolean; ATicks: TcxTrackBarTicksAlign): TSize;
    function RangeTrackBarRightThumbSize(AHorizontal: Boolean; ATicks: TcxTrackBarTicksAlign): TSize;
    function RangeTrackBarScaledLeftThumbSize(AHorizontal: Boolean; ATicks: TcxTrackBarTicksAlign; AScaleFactor: TdxScaleFactor): TSize; virtual;
    function RangeTrackBarScaledRightThumbSize(AHorizontal: Boolean; ATicks: TcxTrackBarTicksAlign; AScaleFactor: TdxScaleFactor): TSize; virtual;
    procedure DrawRangeTrackBarScaledLeftThumb(ACanvas: TcxCanvas; const ARect: TRect; AState: TcxButtonState;
      AHorizontal: Boolean; ATicks: TcxTrackBarTicksAlign; AThumbColor: TColor; AScaleFactor: TdxScaleFactor); virtual;
    procedure DrawRangeTrackBarLeftThumb(ACanvas: TcxCanvas; const ARect: TRect; AState: TcxButtonState;
      AHorizontal: Boolean; ATicks: TcxTrackBarTicksAlign; AThumbColor: TColor);
    procedure DrawRangeTrackBarScaledRightThumb(ACanvas: TcxCanvas; const ARect: TRect; AState: TcxButtonState;
      AHorizontal: Boolean; ATicks: TcxTrackBarTicksAlign; AThumbColor: TColor; AScaleFactor: TdxScaleFactor); virtual;
    procedure DrawRangeTrackBarRightThumb(ACanvas: TcxCanvas; const ARect: TRect; AState: TcxButtonState;
      AHorizontal: Boolean; ATicks: TcxTrackBarTicksAlign; AThumbColor: TColor);

    // Splitter
    function GetSplitterInnerColor(AHighlighted: Boolean): TColor; virtual;
    function GetSplitterOuterColor(AHighlighted: Boolean): TColor; virtual;
    procedure DrawScaledSplitter(ACanvas: TcxCanvas; const ARect: TRect; AHighlighted, AClicked, AHorizontal: Boolean;
      AScaleFactor: TdxScaleFactor; AHasCloseMark: Boolean = False; AArrowDirection: TcxArrowDirection = adLeft); virtual;
    procedure DrawSplitter(ACanvas: TcxCanvas; const ARect: TRect; AHighlighted, AClicked, AHorizontal: Boolean);
    procedure DrawSplitterCloseMark(ACanvas: TcxCanvas; const ARect: TRect; AHighlighted, AClicked, AHorizontal: Boolean; AScaleFactor: TdxScaleFactor; AArrowDirection: TcxArrowDirection);
    function GetScaledSplitterSize(AHorizontal: Boolean; AScaleFactor: TdxScaleFactor): TSize; virtual;
    function GetSplitterSize(AHorizontal: Boolean): TSize;
    function HasSplitterInnerLine(AHorizontal: Boolean; AScaleFactor: TdxScaleFactor): Boolean;

    // LayoutControl
    function LayoutControlEmptyAreaColor: TColor; virtual;
    function LayoutControlGetColorPaletteForGroupButton(AState: TcxButtonState): IdxColorPalette; virtual;
    function LayoutControlGetColorPaletteForItemCaption: IdxColorPalette; virtual;
    function LayoutControlGetColorPaletteForTabbedGroupCaption(AIsActive: Boolean): IdxColorPalette; virtual;
    procedure DrawLayoutControlBackground(ACanvas: TcxCanvas; const R: TRect); virtual;

    // ScrollBox
    procedure DrawScrollBoxBackground(ACanvas: TcxCanvas; const R: TRect; AColor: TColor); virtual;

    // Printing System
    function PrintPreviewBackgroundTextColor: TColor; virtual;
    function PrintPreviewPageBordersWidth: TRect;
    function PrintPreviewPageBordersScaledWidth(AScaleFactor: TdxScaleFactor): TRect; virtual;
    procedure DrawPrintPreviewBackground(ACanvas: TcxCanvas; const R: TRect);
    procedure DrawPrintPreviewScaledBackground(ACanvas: TcxCanvas; const R: TRect; AScaleFactor: TdxScaleFactor); virtual;
    procedure DrawPrintPreviewPageBackground(ACanvas: TcxCanvas;
      const ABorderRect, AContentRect: TRect; ASelected, ADrawContent: Boolean);
    procedure DrawPrintPreviewPageScaledBackground(ACanvas: TcxCanvas;
      const ABorderRect, AContentRect: TRect; ASelected, ADrawContent: Boolean; AScaleFactor: TdxScaleFactor); virtual;

    // CalcEdit
    function CalcEditButtonTextColor(AButtonKind: TcxCalcButtonKind): TColor; virtual;

    // Customization Form
    function GetCustomizationFormListBackgroundColor: TColor; virtual;

    // Message Box
    procedure DrawMessageBox(ACanvas: TcxCanvas; const ABounds: TRect;
      const AMessage: string; AFont: TFont = nil; AColor: TColor = clNone); virtual;

    // BreadcrumbEdit
    function BreadcrumbEditBackgroundColor(AState: TdxBreadcrumbEditState): TColor; virtual;
    function BreadcrumbEditBordersSize: TRect; virtual;
    function BreadcrumbEditButtonAreaSeparatorSize: Integer;
    function BreadcrumbEditButtonColorPalette(AState: TdxBreadcrumbEditButtonState): IdxColorPalette; virtual;
    function BreadcrumbEditButtonContentOffsets(AIsFirst, AIsLast: Boolean): TRect;
    function BreadcrumbEditDropDownButtonWidth: Integer;
    function BreadcrumbEditIsFadingSupports: Boolean; virtual;
    function BreadcrumbEditNodeDelimiterSize: Integer;
    function BreadcrumbEditNodeTextColor(AState: TdxBreadcrumbEditButtonState): TColor; virtual;
    function BreadcrumbEditNodeTextOffsets: TRect;
    function BreadcrumbEditProgressChunkOverlaySize: TSize;
    function BreadcrumbEditProgressChunkPadding: TRect;
    function BreadcrumbEditScaledButtonAreaSeparatorSize(AScaleFactor: TdxScaleFactor): Integer; virtual;
    function BreadcrumbEditScaledButtonContentOffsets(AIsFirst, AIsLast: Boolean; AScaleFactor: TdxScaleFactor): TRect; virtual;
    function BreadcrumbEditScaledDropDownButtonWidth(AScaleFactor: TdxScaleFactor): Integer;  virtual;
    function BreadcrumbEditScaledNodeDelimiterSize(AScaleFactor: TdxScaleFactor): Integer; virtual;
    function BreadcrumbEditScaledNodeTextOffsets(AScaleFactor: TdxScaleFactor): TRect; virtual;
    function BreadcrumbEditScaledProgressChunkOverlaySize(AScaleFactor: TdxScaleFactor): TSize; virtual;
    function BreadcrumbEditScaledProgressChunkPadding(AScaleFactor: TdxScaleFactor): TRect; virtual;
    procedure DrawBreadcrumbEditBorders(ACanvas: TcxCanvas; const ARect: TRect; ABorders: TcxBorders; AState: TdxBreadcrumbEditState); virtual;
    procedure DrawBreadcrumbEditButton(ACanvas: TcxCanvas; const ARect: TRect; AState: TdxBreadcrumbEditButtonState; AIsFirst, AIsLast: Boolean);
    procedure DrawBreadcrumbEditButtonAreaSeparator( ACanvas: TcxCanvas; const ARect: TRect; AState: TdxBreadcrumbEditState);
    procedure DrawBreadcrumbEditDropDownButton(ACanvas: TcxCanvas; const ARect: TRect; AState: TdxBreadcrumbEditButtonState; AIsInEditor: Boolean);
    procedure DrawBreadcrumbEditDropDownButtonGlyph(ACanvas: TcxCanvas; const ARect: TRect; AState: TdxBreadcrumbEditButtonState; AIsInEditor: Boolean);
    procedure DrawBreadcrumbEditNode(ACanvas: TcxCanvas; const R: TRect; AState: TdxBreadcrumbEditButtonState; AHasDelimiter: Boolean);
    procedure DrawBreadcrumbEditNodeDelimiter(ACanvas: TcxCanvas; const R: TRect; AState: TdxBreadcrumbEditButtonState);
    procedure DrawBreadcrumbEditNodeDelimiterGlyph(ACanvas: TcxCanvas; const R: TRect; AState: TdxBreadcrumbEditButtonState);
    procedure DrawBreadcrumbEditNodeMoreButtonGlyph(ACanvas: TcxCanvas; const R: TRect; AState: TdxBreadcrumbEditButtonState);
    procedure DrawBreadcrumbEditProgressChunk(ACanvas: TcxCanvas; const R: TRect);
    procedure DrawBreadcrumbEditProgressChunkOverlay(ACanvas: TcxCanvas; const R: TRect);
    procedure DrawBreadcrumbEditScaledButton(ACanvas: TcxCanvas; const ARect: TRect;
      AState: TdxBreadcrumbEditButtonState; AIsFirst, AIsLast: Boolean; AScaleFactor: TdxScaleFactor); virtual;
    procedure DrawBreadcrumbEditScaledButtonAreaSeparator(ACanvas: TcxCanvas; const ARect: TRect;
      AState: TdxBreadcrumbEditState; AScaleFactor: TdxScaleFactor); virtual;
    procedure DrawBreadcrumbEditScaledDropDownButton(ACanvas: TcxCanvas; const ARect: TRect;
      AState: TdxBreadcrumbEditButtonState; AIsInEditor: Boolean; AScaleFactor: TdxScaleFactor); virtual;
    procedure DrawBreadcrumbEditScaledDropDownButtonGlyph(ACanvas: TcxCanvas; const ARect: TRect;
      AState: TdxBreadcrumbEditButtonState; AIsInEditor: Boolean; AScaleFactor: TdxScaleFactor); virtual;
    procedure DrawBreadcrumbEditScaledNode(ACanvas: TcxCanvas; const R: TRect;
      AState: TdxBreadcrumbEditButtonState; AHasDelimiter: Boolean; AScaleFactor: TdxScaleFactor); virtual;
    procedure DrawBreadcrumbEditScaledNodeDelimiter(ACanvas: TcxCanvas; const R: TRect;
      AState: TdxBreadcrumbEditButtonState; AScaleFactor: TdxScaleFactor); virtual;
    procedure DrawBreadcrumbEditScaledNodeDelimiterGlyph(ACanvas: TcxCanvas; const R: TRect;
      AState: TdxBreadcrumbEditButtonState; AScaleFactor: TdxScaleFactor); virtual;
    procedure DrawBreadcrumbEditScaledNodeMoreButtonGlyph(ACanvas: TcxCanvas; const R: TRect;
      AState: TdxBreadcrumbEditButtonState; AScaleFactor: TdxScaleFactor); virtual;
    procedure DrawBreadcrumbEditScaledProgressChunk(ACanvas: TcxCanvas; const R: TRect; AScaleFactor: TdxScaleFactor); virtual;
    procedure DrawBreadcrumbEditScaledProgressChunkOverlay(ACanvas: TcxCanvas; const R: TRect; AScaleFactor: TdxScaleFactor); virtual;

    // CustomMenu
    procedure DrawDropDownListBoxBackground(ACanvas: TcxCanvas; const ARect: TRect; AHasBorders: Boolean); virtual;
    procedure DrawDropDownListBoxGutterBackground(ACanvas: TcxCanvas; const ARect: TRect);
    procedure DrawDropDownListBoxScaledGutterBackground(ACanvas: TcxCanvas; const ARect: TRect; AScaleFactor: TdxScaleFactor); virtual;
    procedure DrawDropDownListBoxScaledSelection(ACanvas: TcxCanvas; const ARect, AGutterRect: TRect; AScaleFactor: TdxScaleFactor); virtual;
    procedure DrawDropDownListBoxScaledSeparator(ACanvas: TcxCanvas; const ARect, AGutterRect: TRect; AScaleFactor: TdxScaleFactor); virtual;
    procedure DrawDropDownListBoxSelection(ACanvas: TcxCanvas; const ARect, AGutterRect: TRect);
    procedure DrawDropDownListBoxSeparator(ACanvas: TcxCanvas; const ARect, AGutterRect: TRect);
    function DropDownListBoxBordersSize: Integer; virtual;
    function DropDownListBoxItemImageOffsets: TRect;
    function DropDownListBoxItemTextColor(ASelected: Boolean): TColor; virtual;
    function DropDownListBoxItemTextOffsets: TRect;
    function DropDownListBoxScaledItemImageOffsets(AScaleFactor: TdxScaleFactor): TRect; virtual;
    function DropDownListBoxScaledItemTextOffsets(AScaleFactor: TdxScaleFactor): TRect; virtual;
    function DropDownListBoxScaledSeparatorSize(AScaleFactor: TdxScaleFactor): Integer; virtual;
    function DropDownListBoxSeparatorSize: Integer;

    // AlertWindow
    function AlertWindowButtonContentOffsets(AKind: TdxAlertWindowButtonKind): TRect;
    function AlertWindowButtonGetColorPalette(AState: TcxButtonState): IdxColorPalette; virtual;
    function AlertWindowButtonGlyphSize(AKind: TdxAlertWindowButtonKind): TSize;
    function AlertWindowContentOffsets: TRect;
    function AlertWindowCornerRadius: Integer; virtual;
    function AlertWindowNavigationPanelTextColor: TColor; virtual;
    function AlertWindowScaledButtonContentOffsets(AKind: TdxAlertWindowButtonKind; AScaleFactor: TdxScaleFactor): TRect; virtual;
    function AlertWindowScaledButtonGlyphSize(AKind: TdxAlertWindowButtonKind; AScaleFactor: TdxScaleFactor): TSize; virtual;
    function AlertWindowScaledContentOffsets(AScaleFactor: TdxScaleFactor): TRect; virtual;
    function AlertWindowTextColor: TColor; virtual;
    procedure DrawAlertWindowBackground(ACanvas: TcxCanvas; const ABounds: TRect; AScaleFactor: TdxScaleFactor = nil); virtual;
    procedure DrawAlertWindowScaledButton(ACanvas: TcxCanvas; const ABounds: TRect;
      AState: TcxButtonState; AKind: TdxAlertWindowButtonKind; AScaleFactor: TdxScaleFactor; ADown: Boolean = False); virtual;
    procedure DrawAlertWindowButton(ACanvas: TcxCanvas; const ABounds: TRect;
      AState: TcxButtonState; AKind: TdxAlertWindowButtonKind; ADown: Boolean = False);
    procedure DrawAlertWindowNavigationPanel(ACanvas: TcxCanvas; const ABounds: TRect); virtual;

    // Gallery
    function GetGalleryGroupHeaderContentOffsets: TRect;
    function GetGalleryGroupTextColor: TColor; virtual;
    function GetGalleryItemCaptionTextColor(const AState: TdxGalleryItemViewState): TColor; virtual;
    function GetGalleryItemColorPalette(const AState: TdxGalleryItemViewState): IdxColorPalette; virtual;
    function GetGalleryItemDescriptionTextColor(const AState: TdxGalleryItemViewState): TColor; virtual;
    function GetGalleryItemImageFrameColor: TColor; virtual;
    function GetGalleryScaledGroupHeaderContentOffsets(AScaleFactor: TdxScaleFactor): TRect; virtual;
    procedure DrawGalleryBackground(ACanvas: TcxCanvas; const ABounds: TRect); virtual;
    procedure DrawGalleryGroupHeader(ACanvas: TcxCanvas; const ABounds: TRect); virtual;
    function DrawGalleryItemSelectionFirst: Boolean; virtual;
    procedure DrawGalleryItemSelection(ACanvas: TcxCanvas; const R: TRect; AViewState: TdxGalleryItemViewState); virtual;
    procedure DrawGalleryItemImageFrame(ACanvas: TcxCanvas; const R: TRect); virtual;

    // ColorGallery
    function GetColorGalleryGlyphFrameColor: TColor; virtual;
    procedure DrawColorGalleryItemSelection(ACanvas: TcxCanvas; const R: TRect; AViewState: TdxGalleryItemViewState); virtual;

    // Bevel
    procedure DrawBevelFrame(ACanvas: TcxCanvas; const R: TRect; AColor1, AColor2: TColor; ABoxStyle: Boolean); virtual;
    procedure DrawBevelLine(ACanvas: TcxCanvas; const R: TRect; AColor1, AColor2: TColor; AIsVertical: Boolean); virtual;
    procedure DrawBevelShape(ACanvas: TcxCanvas; const R: TRect; AShape: TdxBevelShape; AStyle: TdxBevelStyle); virtual;
    function GetBevelMinimalShapeSize(AShape: TdxBevelShape): TSize; virtual;
    procedure GetBevelShapeColors(out AColor1, AColor2: TColor); virtual;

    // Calendar
    procedure DrawCalendarDateCellSelection(ACanvas: TcxCanvas; const ARect: TRect; AStates: TcxCalendarElementStates); virtual;
    // ModernCalendar
    procedure DrawModernCalendarArrow(ACanvas: TcxCanvas; const ARect: TRect; ADirection: TcxArrowDirection; AState: TcxCalendarElementState); virtual;
    procedure DrawModernCalendarClock(ACanvas: TcxCanvas; const ARect: TRect;
      ADateTime: TDateTime; ABackgroundColor: TColor; AScaleFactor: TdxScaleFactor); virtual;
    procedure DrawModernCalendarDateCellSelection(ACanvas: TcxCanvas; const ARect: TRect; AStates: TcxCalendarElementStates); virtual;
    procedure DrawModernCalendarDateHeaderSelection(ACanvas: TcxCanvas; const ARect: TRect; AStates: TcxCalendarElementStates); virtual;
    procedure DrawModernCalendarHeaderSelection(ACanvas: TcxCanvas; const ARect: TRect; AStates: TcxCalendarElementStates); virtual;
    function GetModernCalendarCellTextColor(AStates: TcxCalendarElementStates): TColor; virtual;
    function GetModernCalendarDateHeaderTextColor(AStates: TcxCalendarElementStates): TColor; virtual;
    function GetModernCalendarHeaderTextColor(AStates: TcxCalendarElementStates): TColor; virtual;
    function GetModernCalendarHeaderTextOffsets: TRect; virtual;
    function GetModernCalendarMarkedCellBorderColor: TColor; virtual;
    function GetModernCalendarSelectedTextColor: TColor; virtual;
    procedure DrawModernClockHands(ACanvas: TcxCanvas; const ARect: TRect; ADateTime: TDateTime; AColor: TColor);
    procedure DrawScaledModernCalendarArrow(ACanvas: TcxCanvas; const ARect: TRect;
      ADirection: TcxArrowDirection; AState: TcxCalendarElementState; AScaleFactor: TdxScaleFactor); virtual;
    procedure DrawScaledModernClockHands(ACanvas: TcxCanvas; const ARect: TRect; ADateTime: TDateTime; AColor: TColor; AScaleFactor: TdxScaleFactor); virtual;

    // RatingControl
    procedure DrawRatingControlIndicator(ACanvas: TcxCanvas; const ABounds: TRect; AState: TdxRatingControlIndicatorState);
    procedure DrawRatingControlScaledIndicator(ACanvas: TcxCanvas; const ABounds: TRect;
      AState: TdxRatingControlIndicatorState; AScaleFactor: TdxScaleFactor); virtual;
    function GetRatingControlIndicatorColorPalette(AState: TdxRatingControlIndicatorState): IdxColorPalette; virtual;
    function GetRatingControlIndicatorSize: TSize;
    function GetRatingControlScaledIndicatorSize(AScaleFactor: TdxScaleFactor): TSize; virtual;

    //Fixed Group Indicator
    procedure DrawFixedGroupIndicator(ACanvas: TcxCanvas; const ABounds: TRect);
    procedure DrawScaledFixedGroupIndicator(ACanvas: TcxCanvas; const ABounds: TRect; AScaleFactor: TdxScaleFactor); virtual;
    function GetFixedGroupIndicatorSize: TSize;
    function GetScaledFixedGroupIndicatorSize(AScaleFactor: TdxScaleFactor): TSize; virtual;

    // WheelPicker
    function GetWheelPickerBorderItemColor(AState: TcxButtonState): TColor; virtual;
    function GetWheelPickerColorPalette(AState: TcxButtonState): IdxColorPalette; virtual;
    function GetWheelPickerFillItemColor(AState: TcxButtonState): TColor; virtual;
    procedure DrawWheelPickerItem(ACanvas: TcxCanvas; const ARect: TRect; AState: TcxButtonState); virtual;
    procedure DrawWheelPickerItemBackground(ACanvas: TcxCanvas; const ARect: TRect; AState: TcxButtonState); virtual;
    procedure DrawWheelPickerItemBorder(ACanvas: TcxCanvas; const ARect: TRect; AState: TcxButtonState); virtual;

    // RichEditControl
    function RichEditControlHeaderFooterLineColor: TColor; virtual;
    function RichEditControlHeaderFooterMarkBackColor: TColor; virtual;
    function RichEditControlHeaderFooterMarkTextColor: TColor; virtual;

    function RichEditRulerControlColor: TColor; virtual;
    function RichEditRulerActiveAreaColor: TColor; virtual;
    function RichEditRulerDefaultTabColor: TColor; virtual;
    function RichEditRulerInactiveAreaColor: TColor; virtual;
    function RichEditRulerTabTypeToggleBorderColor: TColor; virtual;
    function RichEditRulerTextColor: TColor; virtual;

	  // TokenEdit
    procedure DrawScaledTokenBackground(ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState; AScaleFactor: TdxScaleFactor); virtual;
    procedure DrawScaledTokenCloseGlyph(ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState; AScaleFactor: TdxScaleFactor); virtual;
    procedure DrawTokenBackground(ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState);
    procedure DrawTokenCloseGlyph(ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState);
    function GetScaledTokenCloseGlyphSize(AScaleFactor: TdxScaleFactor): TSize; virtual;
    function GetScaledTokenContentOffsets(AScaleFactor: TdxScaleFactor): TRect; virtual;
    function GetScaledTokenDefaultGlyphSize(AScaleFactor: TdxScaleFactor): TSize; virtual;
    function GetTokenCloseGlyphSize: TSize;
    function GetTokenColorPalette(AState: TcxButtonState): IdxColorPalette; virtual;
    function GetTokenContentOffsets: TRect;
    function GetTokenDefaultGlyphSize: TSize;
    function GetTokenTextColor(AState: TcxButtonState): TColor; virtual;
  end;

  TcxCustomLookAndFeelPainterClass = class of TcxCustomLookAndFeelPainter;

  { TcxStandardLookAndFeelPainter }

  TcxStandardLookAndFeelPainter = class(TcxCustomLookAndFeelPainter)
  protected
    procedure DoDrawScaledScrollBarPart(ACanvas: TcxCanvas; AHorizontal: Boolean; R: TRect; APart: TcxScrollBarPart;
      AState: TcxButtonState; AScaleFactor: TdxScaleFactor); override;
  public
    function LookAndFeelName: string; override;
    function LookAndFeelStyle: TcxLookAndFeelStyle; override;
    // border
    function BorderSize: Integer; override;
    procedure DrawBorder(ACanvas: TcxCanvas; R: TRect); override;
    // buttons
    function ButtonBorderSize(AState: TcxButtonState = cxbsNormal): Integer; override;
    function ScaledButtonTextOffset(AScaleFactor: TdxScaleFactor): Integer; override;
    function ScaledButtonTextShift(AScaleFactor: TdxScaleFactor): Integer; override;
    procedure DrawButtonBorder(ACanvas: TcxCanvas; R: TRect; AState: TcxButtonState); override;
    procedure DrawScaledGroupExpandButton(ACanvas: TcxCanvas; const R: TRect;
      AExpanded: Boolean; AState: TcxButtonState; AScaleFactor: TdxScaleFactor); override;
    function ScaledExpandButtonSize(AScaleFactor: TdxScaleFactor): Integer; override;
    procedure DrawScaledExpandButton(ACanvas: TcxCanvas; const R: TRect; AExpanded: Boolean;
      AScaleFactor: TdxScaleFactor; AColor: TColor = clDefault; AState: TcxExpandButtonState = cebsNormal); override;
    function IsButtonHotTrack: Boolean; override;
    // checkbox
    procedure DrawCheckBorder(ACanvas: TcxCanvas; R: TRect; AState: TcxButtonState); override;
    // colors
    function DefaultSchedulerTimeRulerBorderColor: TColor; override;
    function DefaultSchedulerTimeRulerTextColor: TColor; override;
    // header
    procedure DrawHeaderBorder(ACanvas: TcxCanvas; const R: TRect; ANeighbors: TcxNeighbors; ABorders: TcxBorders); override;
    procedure DrawHeaderControlSectionBorder(ACanvas: TcxCanvas; const R: TRect; ABorders: TcxBorders; AState: TcxButtonState); override;
    procedure DrawScaledSortingMark(ACanvas: TcxCanvas; const R: TRect; AAscendingSorting: Boolean; AScaleFactor: TdxScaleFactor); override;
    procedure DrawScaledSummarySortingMark(ACanvas: TcxCanvas; const R: TRect; AAscendingSorting: Boolean; AScaleFactor: TdxScaleFactor); override;
    function HeaderBorderSize: Integer; override;
    function HeaderControlSectionBorderSize(AState: TcxButtonState = cxbsNormal): Integer; override;
    function ScaledSortingMarkSize(AScaleFactor: TdxScaleFactor): TPoint; override;
    function ScaledSummarySortingMarkSize(AScaleFactor: TdxScaleFactor): TPoint; override;
    // footer
    function FooterBorderSize: Integer; override;
    function FooterCellBorderSize: Integer; override;
    function FooterCellOffset: Integer; override;
    procedure DrawFooterBorder(ACanvas: TcxCanvas; const R: TRect); override;
    procedure DrawFooterCellBorder(ACanvas: TcxCanvas; const R: TRect); override;
    // filter
    procedure DrawScaledFilterDropDownButton(ACanvas: TcxCanvas; R: TRect;
      AState: TcxButtonState; AIsFilterActive: Boolean; AScaleFactor: TdxScaleFactor); override;
    // tabs
    procedure DrawTabBorder(ACanvas: TcxCanvas; R: TRect; ABorder: TcxBorder; ABorders: TcxBorders; AVertical: Boolean); override;
    procedure DrawTabsRoot(ACanvas: TcxCanvas; const R: TRect; ABorders: TcxBorders; AVertical: Boolean); override;
    function TabBorderSize(AVertical: Boolean): Integer; override;
    // ms outlook
    function DefaultSchedulerViewContentColor: TColor; override;
    function DefaultSchedulerViewContentColorClassic: TColor; override;
    procedure DrawScaledMonthHeader(ACanvas: TcxCanvas; const ABounds: TRect; const AText: string; ANeighbors: TcxNeighbors;
      const AViewParams: TcxViewParams; AArrows: TcxArrowDirections; ASideWidth: Integer; AScaleFactor: TdxScaleFactor;
      AOnDrawBackground: TcxDrawBackgroundEvent = nil); override;
    procedure DrawSchedulerSplitterBorder(ACanvas: TcxCanvas; R: TRect; const AViewParams: TcxViewParams; AIsHorizontal: Boolean); override;
    // GroupBox
    procedure DrawGroupBoxFrame(ACanvas: TcxCanvas; R: TRect; AEnabled: Boolean;
      ACaptionPosition: TcxGroupBoxCaptionPosition; ABorders: TcxBorders = cxBordersAll); override;
    // TrackBar
    procedure DrawTrackBarTrackBounds(ACanvas: TcxCanvas; const ARect: TRect); override;
    procedure DrawTrackBarThumbBorderUpDown(ACanvas: TcxCanvas;
      const ALightPolyLine, AShadowPolyLine, ADarkPolyLine: TPoints); override;
    procedure DrawTrackBarThumbBorderBoth(ACanvas: TcxCanvas; const ARect: TRect); override;
    // BreadcrumbEdit
    procedure DrawBreadcrumbEditScaledButton(ACanvas: TcxCanvas; const ARect: TRect;
      AState: TdxBreadcrumbEditButtonState; AIsFirst, AIsLast: Boolean; AScaleFactor: TdxScaleFactor); override;
    // ToggleSwitch
    procedure DrawScaledToggleSwitchState(ACanvas: TcxCanvas; ABounds: TRect;
      AState: TcxButtonState; AChecked: Boolean; AScaleFactor: TdxScaleFactor); override;
    function ToggleSwitchToggleColor(AChecked: Boolean): TColor; override;
    // Find Panel
    procedure DrawFindPanelBorder(ACanvas: TcxCanvas; const R: TRect; ABorders: TcxBorders); override;
    // TokenEdit
    procedure DrawScaledTokenBackground(ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState; AScaleFactor: TdxScaleFactor); override;
  end;

  { TcxFlatLookAndFeelPainter }

  TcxFlatLookAndFeelPainter = class(TcxCustomLookAndFeelPainter)
  protected
    procedure DoDrawScaledScrollBarPart(ACanvas: TcxCanvas; AHorizontal: Boolean; R: TRect; APart: TcxScrollBarPart;
      AState: TcxButtonState; AScaleFactor: TdxScaleFactor); override;
  public
    function LookAndFeelName: string; override;
    function LookAndFeelStyle: TcxLookAndFeelStyle; override;
    // border
    function BorderSize: Integer; override;
    function SeparatorSize: Integer; override;
    procedure DrawBorder(ACanvas: TcxCanvas; R: TRect); override;
    // buttons
    function ButtonBorderSize(AState: TcxButtonState = cxbsNormal): Integer; override;
    function ScaledButtonTextOffset(AScaleFactor: TdxScaleFactor): Integer; override;
    function ScaledButtonTextShift(AScaleFactor: TdxScaleFactor): Integer; override;
    procedure DrawButtonBorder(ACanvas: TcxCanvas; R: TRect; AState: TcxButtonState); override;
    procedure DrawScaledExpandButton(ACanvas: TcxCanvas; const R: TRect; AExpanded: Boolean;
      AScaleFactor: TdxScaleFactor; AColor: TColor = clDefault; AState: TcxExpandButtonState = cebsNormal); override;
    function ScaledExpandButtonSize(AScaleFactor: TdxScaleFactor): Integer; override;
    function IsButtonHotTrack: Boolean; override;
    // checkbox
    procedure DrawCheckBorder(ACanvas: TcxCanvas; R: TRect; AState: TcxButtonState); override;
    // colors
    function DefaultSchedulerTimeRulerBorderColor: TColor; override;
    function DefaultSchedulerTimeRulerTextColor: TColor; override;
    // header
    procedure DrawHeaderBorder(ACanvas: TcxCanvas; const R: TRect; ANeighbors: TcxNeighbors; ABorders: TcxBorders); override;
    procedure DrawHeaderControlSectionBorder(ACanvas: TcxCanvas; const R: TRect; ABorders: TcxBorders; AState: TcxButtonState); override;
    procedure DrawScaledSortingMark(ACanvas: TcxCanvas; const R: TRect; AAscendingSorting: Boolean; AScaleFactor: TdxScaleFactor); override;
    procedure DrawScaledSummarySortingMark(ACanvas: TcxCanvas; const R: TRect; AAscendingSorting: Boolean; AScaleFactor: TdxScaleFactor); override;
    function HeaderBorderSize: Integer; override;
    function ScaledSortingMarkSize(AScaleFactor: TdxScaleFactor): TPoint; override;
    function ScaledSummarySortingMarkSize(AScaleFactor: TdxScaleFactor): TPoint; override;
    // footer
    function FooterBorderSize: Integer; override;
    function FooterCellBorderSize: Integer; override;
    function FooterCellOffset: Integer; override;
    procedure DrawFooterBorder(ACanvas: TcxCanvas; const R: TRect); override;
    procedure DrawFooterCellBorder(ACanvas: TcxCanvas; const R: TRect); override;
    // filter
    procedure DrawScaledFilterDropDownButton(ACanvas: TcxCanvas; R: TRect;
      AState: TcxButtonState; AIsFilterActive: Boolean; AScaleFactor: TdxScaleFactor); override;
    // tabs
    procedure DrawTabBorder(ACanvas: TcxCanvas; R: TRect; ABorder: TcxBorder; ABorders: TcxBorders; AVertical: Boolean); override;
    procedure DrawTabsRoot(ACanvas: TcxCanvas; const R: TRect; ABorders: TcxBorders; AVertical: Boolean); override;
    function TabBorderSize(AVertical: Boolean): Integer; override;
    // ms outlook
    procedure DrawSchedulerSplitterBorder(ACanvas: TcxCanvas; R: TRect; const AViewParams: TcxViewParams; AIsHorizontal: Boolean); override;
    // GroupBox
    procedure DrawGroupBoxFrame(ACanvas: TcxCanvas; R: TRect; AEnabled: Boolean;
      ACaptionPosition: TcxGroupBoxCaptionPosition; ABorders: TcxBorders = cxBordersAll); override;
    // Trackbar
    procedure DrawTrackBarTrackBounds(ACanvas: TcxCanvas; const ARect: TRect); override;
    procedure DrawTrackBarThumbBorderUpDown(ACanvas: TcxCanvas;
      const ALightPolyLine, AShadowPolyLine, ADarkPolyLine: TPoints); override;
    procedure DrawTrackBarThumbBorderBoth(ACanvas: TcxCanvas; const ARect: TRect); override;
    // DateNavigator
    procedure DrawDateNavigatorDateHeader(ACanvas: TcxCanvas; var R: TRect); override;
    // Splitter
    function GetSplitterOuterColor(AHighlighted: Boolean): TColor; override;
    // ToggleSwitch
    procedure DrawScaledToggleSwitchState(ACanvas: TcxCanvas; ABounds: TRect;
      AState: TcxButtonState; AChecked: Boolean; AScaleFactor: TdxScaleFactor); override;
    function ToggleSwitchToggleColor(AChecked: Boolean): TColor; override;
    // Find Panel
    procedure DrawFindPanelBorder(ACanvas: TcxCanvas; const R: TRect; ABorders: TcxBorders); override;
  end;

  { TcxUltraFlatLookAndFeelPainter }

  TcxUltraFlatLookAndFeelPainter = class(TcxCustomLookAndFeelPainter)
  protected
    // scrollbar
    procedure DoDrawScaledScrollBarPart(ACanvas: TcxCanvas; AHorizontal: Boolean; R: TRect; APart: TcxScrollBarPart;
      AState: TcxButtonState; AScaleFactor: TdxScaleFactor); override;
    // filter
    function FilterDropDownButtonBorderColor(AState: TcxButtonState): TColor; virtual;
    // Scheduler
    procedure DrawSchedulerNavigationButtonContent(ACanvas: TcxCanvas;
      const ARect: TRect; const AArrowRect: TRect; AIsNextButton: Boolean;
      AState: TcxButtonState; const AIsVertical: Boolean = True); override;
    // BreadcrumbEdit
    procedure DrawBreadcrumbEditCustomButton(ACanvas: TcxCanvas; const R: TRect;
      AState: TdxBreadcrumbEditButtonState; ABorders: TcxBorders); virtual;
    // tabs
    function TabBorderHighlightColor: TColor; virtual;
    function TabBorderDarkColor: TColor; virtual;
  public
    function LookAndFeelName: string; override;
    function LookAndFeelStyle: TcxLookAndFeelStyle; override;
    // default
    function DefaultSchedulerBorderColor: TColor; override;
    // border
    function BorderHighlightColor: TColor; virtual;
    function BorderSize: Integer; override;
    procedure DrawBorder(ACanvas: TcxCanvas; R: TRect); override;
    procedure DoDrawSeparator(ACanvas: TcxCanvas; R: TRect; AIsVertical: Boolean); override;
    // buttons
    function ButtonBorderSize(AState: TcxButtonState = cxbsNormal): Integer; override;
    function ButtonColor(AState: TcxButtonState): TColor; override;
    function ScaledButtonFocusRect(ACanvas: TcxCanvas; R: TRect; AScaleFactor: TdxScaleFactor): TRect; override;
    function ButtonSymbolColor(AState: TcxButtonState;
      ADefaultColor: TColor = clDefault): TColor; override;
    function ButtonSymbolState(AState: TcxButtonState): TcxButtonState; override;
    function ScaledButtonTextOffset(AScaleFactor: TdxScaleFactor): Integer; override;
    function ScaledButtonTextShift(AScaleFactor: TdxScaleFactor): Integer; override;
    procedure DrawButtonBorder(ACanvas: TcxCanvas; R: TRect; AState: TcxButtonState); override;
    procedure DrawScaledExpandButton(ACanvas: TcxCanvas; const R: TRect; AExpanded: Boolean;
      AScaleFactor: TdxScaleFactor; AColor: TColor = clDefault; AState: TcxExpandButtonState = cebsNormal); override;
    procedure DrawHeaderControlSectionBorder(ACanvas: TcxCanvas; const R: TRect;
      ABorders: TcxBorders; AState: TcxButtonState); override;
    procedure DrawHeaderControlSectionContent(ACanvas: TcxCanvas; const ABounds,
      ATextAreaBounds: TRect; AState: TcxButtonState; AAlignmentHorz: TAlignment;
      AAlignmentVert: TcxAlignmentVert; AMultiLine, AShowEndEllipsis: Boolean;
      const AText: string; AFont: TFont; ATextColor, ABkColor: TColor); override;
    function ScaledExpandButtonSize(AScaleFactor: TdxScaleFactor): Integer; override;
    function IsButtonHotTrack: Boolean; override;
    // checkbox
    procedure DrawCheckBorder(ACanvas: TcxCanvas; R: TRect; AState: TcxButtonState); override;
    // colors
    function DefaultSchedulerTimeRulerBorderColor: TColor; override;
    function DefaultSchedulerTimeRulerTextColor: TColor; override;
    // RadioButton
    function RadioButtonBodyColor(AState: TcxButtonState): TColor; override;
    // header
    procedure DrawHeaderBorder(ACanvas: TcxCanvas; const R: TRect; ANeighbors: TcxNeighbors; ABorders: TcxBorders); override;
    procedure DrawScaledHeaderEx(ACanvas: TcxCanvas; const ABounds, ATextAreaBounds: TRect; ANeighbors: TcxNeighbors;
      ABorders: TcxBorders; AState: TcxButtonState; AAlignmentHorz: TAlignment; AAlignmentVert: TcxAlignmentVert;
      AMultiLine, AShowEndEllipsis: Boolean; const AText: string; AFont: TFont; ATextColor, ABkColor: TColor;
      AScaleFactor: TdxScaleFactor; AOnDrawBackground: TcxDrawBackgroundEvent = nil); override;
    procedure DrawScaledSortingMark(ACanvas: TcxCanvas; const R: TRect; AAscendingSorting: Boolean; AScaleFactor: TdxScaleFactor); override;
    procedure DrawScaledSummarySortingMark(ACanvas: TcxCanvas; const R: TRect; AAscendingSorting: Boolean; AScaleFactor: TdxScaleFactor); override;
    function HeaderBorders(ANeighbors: TcxNeighbors): TcxBorders; override;
    function HeaderBorderSize: Integer; override;
    function ScaledSortingMarkSize(AScaleFactor: TdxScaleFactor): TPoint; override;
    function ScaledSummarySortingMarkSize(AScaleFactor: TdxScaleFactor): TPoint; override;
    // footer
    procedure DrawFooterBorder(ACanvas: TcxCanvas; const R: TRect); override;
    procedure DrawFooterBorderEx(ACanvas: TcxCanvas; const R: TRect; ABorders: TcxBorders); override;
    procedure DrawFooterCellBorder(ACanvas: TcxCanvas; const R: TRect); override;
    function FooterBorders: TcxBorders; override;
    function FooterBorderSize: Integer; override;
    function FooterCellBorderSize: Integer; override;
    function FooterCellOffset: Integer; override;
    // filter
    procedure DrawScaledFilterDropDownButton(ACanvas: TcxCanvas; R: TRect;
      AState: TcxButtonState; AIsFilterActive: Boolean; AScaleFactor: TdxScaleFactor); override;
    function ScaledFilterCloseButtonSize(AScaleFactor: TdxScaleFactor): TPoint; override;
    // tabs
    procedure DrawTabBorder(ACanvas: TcxCanvas; R: TRect; ABorder: TcxBorder; ABorders: TcxBorders; AVertical: Boolean); override;
    procedure DrawTabsRoot(ACanvas: TcxCanvas; const R: TRect; ABorders: TcxBorders; AVertical: Boolean); override;
    function TabBorderSize(AVertical: Boolean): Integer; override;
    // ms outlook
    procedure DrawSchedulerScaledNavigatorButton(ACanvas: TcxCanvas; R: TRect; AState: TcxButtonState; AScaleFactor: TdxScaleFactor); override;
    procedure DrawSchedulerSplitterBorder(ACanvas: TcxCanvas; R: TRect; const AViewParams: TcxViewParams; AIsHorizontal: Boolean); override;
    // GroupBox
    procedure DrawGroupBoxFrame(ACanvas: TcxCanvas; R: TRect; AEnabled: Boolean;
      ACaptionPosition: TcxGroupBoxCaptionPosition; ABorders: TcxBorders = cxBordersAll); override;
    // TrackBar
    procedure DrawTrackBarTrackBounds(ACanvas: TcxCanvas; const ARect: TRect); override;
    procedure DrawTrackBarThumbBorderUpDown(ACanvas: TcxCanvas;
      const ALightPolyLine, AShadowPolyLine, ADarkPolyLine: TPoints); override;
    procedure DrawTrackBarThumbBorderBoth(ACanvas: TcxCanvas; const ARect: TRect); override;
    // Printing System
    procedure DrawPrintPreviewScaledBackground(ACanvas: TcxCanvas; const R: TRect; AScaleFactor: TdxScaleFactor); override;
    // Splitter
    function GetSplitterOuterColor(AHighlighted: Boolean): TColor; override;
    function GetScaledSplitterSize(AHorizontal: Boolean; AScaleFactor: TdxScaleFactor): TSize; override;
    // navigator
    procedure DrawNavigatorScaledButton(ACanvas: TcxCanvas; R: TRect; AState: TcxButtonState;
      ABackgroundColor: TColor; AScaleFactor: TdxScaleFactor); override;
    procedure DrawNavigatorBorder(ACanvas: TcxCanvas; R: TRect; ASelected: Boolean); override;
    function NavigatorBorderOverlap: Boolean; override;
    function NavigatorBorderSize: Integer; override;
    // CustomMenu
    procedure DrawDropDownListBoxBackground(ACanvas: TcxCanvas; const ARect: TRect; AHasBorders: Boolean); override;
    procedure DrawDropDownListBoxScaledGutterBackground(ACanvas: TcxCanvas; const ARect: TRect; AScaleFactor: TdxScaleFactor); override;
    procedure DrawDropDownListBoxScaledSelection(ACanvas: TcxCanvas; const ARect, AGutterRect: TRect; AScaleFactor: TdxScaleFactor); override;
    procedure DrawDropDownListBoxScaledSeparator(ACanvas: TcxCanvas; const ARect, AGutterRect: TRect; AScaleFactor: TdxScaleFactor); override;
    function DropDownListBoxItemTextColor(ASelected: Boolean): TColor; override;
    // BreadcrumbEdit
    function BreadcrumbEditScaledButtonAreaSeparatorSize(AScaleFactor: TdxScaleFactor): Integer; override;
    function BreadcrumbEditScaledButtonContentOffsets(AIsFirst, AIsLast: Boolean; AScaleFactor: TdxScaleFactor): TRect; override;
    procedure DrawBreadcrumbEditBorders(ACanvas: TcxCanvas; const ARect: TRect;
      ABorders: TcxBorders; AState: TdxBreadcrumbEditState); override;
    procedure DrawBreadcrumbEditScaledButtonAreaSeparator(ACanvas: TcxCanvas;
      const ARect: TRect; AState: TdxBreadcrumbEditState; AScaleFactor: TdxScaleFactor); override;
    procedure DrawBreadcrumbEditScaledButton(ACanvas: TcxCanvas; const ARect: TRect;
      AState: TdxBreadcrumbEditButtonState; AIsFirst, AIsLast: Boolean; AScaleFactor: TdxScaleFactor); override;
    procedure DrawBreadcrumbEditScaledDropDownButton(ACanvas: TcxCanvas; const ARect: TRect;
      AState: TdxBreadcrumbEditButtonState; AIsInEditor: Boolean; AScaleFactor: TdxScaleFactor); override;

    // Bevel
    function GetBevelMinimalShapeSize(AShape: TdxBevelShape): TSize; override;
    procedure GetBevelShapeColors(out AColor1, AColor2: TColor); override;

    // Grid
    function GridBordersOverlapSize: Integer; override;

    // ToggleSwitch
    procedure DrawScaledToggleSwitchState(ACanvas: TcxCanvas; ABounds: TRect;
      AState: TcxButtonState; AChecked: Boolean; AScaleFactor: TdxScaleFactor); override;
    function ToggleSwitchToggleColor(AChecked: Boolean): TColor; override;
    procedure DrawScaledToggleSwitchThumb(ACanvas: TcxCanvas; ABounds: TRect; AState: TcxButtonState; AScaleFactor: TdxScaleFactor); override;

    // TokenEdit
    procedure DrawScaledTokenBackground(ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState; AScaleFactor: TdxScaleFactor); override;
  end;

  { TcxOffice11LookAndFeelPainter }

  TcxOffice11LookAndFeelPainter = class(TcxUltraFlatLookAndFeelPainter)
  protected
    function DefaultDateNavigatorHeaderHighlightTextColor: TColor; override;
    procedure DrawContent(ACanvas: TcxCanvas; const ABounds, ATextAreaBounds: TRect; AState: Integer;
      AAlignmentHorz: TAlignment; AAlignmentVert: TcxAlignmentVert; AMultiLine, AShowEndEllipsis: Boolean;
      const AText: string; AFont: TFont; ATextColor, ABkColor: TColor;
      AOnDrawBackground: TcxDrawBackgroundEvent = nil; AIsFooter: Boolean = False); override;
    function HeaderBottomColor: TColor; virtual;
    function HeaderDarkEdgeColor: TColor; virtual;
    function HeaderHighlightEdgeColor: TColor; virtual;
    function HeaderTopColor: TColor; virtual;
    // scrollbar
    procedure DoDrawScaledScrollBarPart(ACanvas: TcxCanvas; AHorizontal: Boolean; R: TRect; APart: TcxScrollBarPart;
      AState: TcxButtonState; AScaleFactor: TdxScaleFactor); override;
    // filter
    function FilterDropDownButtonBorderColor(AState: TcxButtonState): TColor; override;
    // tabs
    function TabBorderHighlightColor: TColor; override;
    function TabBorderDarkColor: TColor; override;
    // scheduler3
    procedure DrawSchedulerNavigationButtonContent(ACanvas: TcxCanvas;
      const ARect: TRect; const AArrowRect: TRect; AIsNextButton: Boolean;
      AState: TcxButtonState; const AIsVertical: Boolean = True); override;
    // BreadcrumbEdit
    procedure DrawBreadcrumbEditCustomButton(ACanvas: TcxCanvas; const R: TRect;
      AState: TdxBreadcrumbEditButtonState; ABorders: TcxBorders); override;
  public
    function LookAndFeelName: string; override;
    function LookAndFeelStyle: TcxLookAndFeelStyle; override;
    function NeedRedrawOnResize: Boolean; override;

    // colors
    function DefaultControlColor: TColor; override;
    function DefaultControlTextColor: TColor; override;
    function DefaultDateNavigatorHeaderColor: TColor; override;
    function DefaultDateNavigatorSelectionColor: TColor; override;
    function DefaultDateNavigatorSelectionTextColor: TColor; override;
    function DefaultFilterBoxColor: TColor; override;
    function DefaultFilterBoxTextColor: TColor; override;
    function DefaultFooterColor: TColor; override;
    function DefaultFooterTextColor: TColor; override;
    function DefaultGroupColor: TColor; override;
    function DefaultGroupByBoxColor: TColor; override;
    function DefaultGroupByBoxTextColor: TColor; override;
    function DefaultHeaderColor: TColor; override;
    function DefaultHeaderBackgroundColor: TColor; override;
    function DefaultSchedulerBorderColor: TColor; override;
    function DefaultSchedulerControlColor: TColor; override;
    function DefaultSchedulerDayHeaderColor: TColor; override;
    function DefaultTabColor: TColor; override;
    function DefaultTabsBackgroundColor: TColor; override;
    function DefaultTimeGridMinorScaleColor: TColor; override;
    function DefaultTimeGridSelectionBarColor: TColor; override;
    // border
    function SeparatorSize: Integer; override;
    function BorderHighlightColor: TColor; override;
    procedure DrawBorder(ACanvas: TcxCanvas; R: TRect); override;
    procedure DoDrawSeparator(ACanvas: TcxCanvas; R: TRect; AIsVertical: Boolean); override;
    // buttons
    function ButtonColor(AState: TcxButtonState): TColor; override;
    function ButtonSymbolColor(AState: TcxButtonState; ADefaultColor: TColor = clDefault): TColor; override;
    procedure DrawButtonBorder(ACanvas: TcxCanvas; R: TRect; AState: TcxButtonState); override;
    procedure DrawScaledExpandButton(ACanvas: TcxCanvas; const R: TRect; AExpanded: Boolean;
      AScaleFactor: TdxScaleFactor; AColor: TColor = clDefault; AState: TcxExpandButtonState = cebsNormal); override;
    function DrawExpandButtonFirst: Boolean; override;
    procedure DrawScaledSmallExpandButton(ACanvas: TcxCanvas; R: TRect; AExpanded: Boolean;
      ABorderColor: TColor; AScaleFactor: TdxScaleFactor; AColor: TColor = clDefault); override;
    function ScaledExpandButtonSize(AScaleFactor: TdxScaleFactor): Integer; override;
    function ScaledSmallExpandButtonSize(AScaleFactor: TdxScaleFactor): Integer; override;
    // checkbox
    function CheckButtonColor(AState: TcxButtonState; ACheckState: TcxCheckBoxState): TColor; override;
    // RadioButton
    function RadioButtonBodyColor(AState: TcxButtonState): TColor; override;
    // filter
    procedure DrawScaledFilterDropDownButton(ACanvas: TcxCanvas; R: TRect;
      AState: TcxButtonState; AIsFilterActive: Boolean; AScaleFactor: TdxScaleFactor); override;
    // header
    procedure DrawScaledHeader(ACanvas: TcxCanvas; const ABounds, ATextAreaBounds: TRect; ANeighbors: TcxNeighbors;
      ABorders: TcxBorders; AState: TcxButtonState; AAlignmentHorz: TAlignment; AAlignmentVert: TcxAlignmentVert;
      AMultiLine, AShowEndEllipsis: Boolean; const AText: string; AFont: TFont; ATextColor, ABkColor: TColor;
      AScaleFactor: TdxScaleFactor; AOnDrawBackground: TcxDrawBackgroundEvent = nil; AIsLast: Boolean = False; AInGroupByBox: Boolean = False); override;
    procedure DrawHeaderBorder(ACanvas: TcxCanvas; const R: TRect; ANeighbors: TcxNeighbors; ABorders: TcxBorders); override;
    procedure DrawScaledHeaderControlSection(ACanvas: TcxCanvas; const ABounds, ATextAreaBounds: TRect; ANeighbors: TcxNeighbors;
      ABorders: TcxBorders; AState: TcxButtonState; AAlignmentHorz: TAlignment; AAlignmentVert: TcxAlignmentVert; AMultiLine,
      AShowEndEllipsis: Boolean; const AText: string; AFont: TFont; ATextColor, ABkColor: TColor; AScaleFactor: TdxScaleFactor); override;
    procedure DrawScaledSortingMark(ACanvas: TcxCanvas; const R: TRect; AAscendingSorting: Boolean; AScaleFactor: TdxScaleFactor); override;
    procedure DrawScaledSummarySortingMark(ACanvas: TcxCanvas; const R: TRect; AAscendingSorting: Boolean; AScaleFactor: TdxScaleFactor); override;
    function HeaderBorders(ANeighbors: TcxNeighbors): TcxBorders; override;
    function HeaderBorderSize: Integer; override;
    function IsHeaderHotTrack: Boolean; override;
    // footer
    function FooterSeparatorColor: TColor; override;
    // Grid
    function GridBordersOverlapSize: Integer; override;
    procedure LayoutViewDrawRecordCaption(ACanvas: TcxCanvas; const ABounds, ATextRect: TRect;
      APosition: TcxGroupBoxCaptionPosition; AState: TcxButtonState; AColor: TColor = clDefault;
      const ABitmap: TBitmap = nil); override;
    // tabs
    // scrollbars
    function ScaledScrollBarMinimalThumbSize(AVertical: Boolean; AScaleFactor: TdxScaleFactor): Integer; override;
    // ms outlook
    procedure DrawScaledMonthHeader(ACanvas: TcxCanvas; const ABounds: TRect; const AText: string; ANeighbors: TcxNeighbors;
      const AViewParams: TcxViewParams; AArrows: TcxArrowDirections; ASideWidth: Integer; AScaleFactor: TdxScaleFactor;
      AOnDrawBackground: TcxDrawBackgroundEvent = nil); override;
    procedure DrawSchedulerScaledNavigatorButton(ACanvas: TcxCanvas; R: TRect;
      AState: TcxButtonState; AScaleFactor: TdxScaleFactor); override;
    procedure DrawSchedulerBorder(ACanvas: TcxCanvas; R: TRect); override;
    // SizeGrip
    procedure DoDrawSizeGrip(ACanvas: TcxCanvas; const ARect: TRect; AScaleFactor: TdxScaleFactor); override;
    // GroupBox
    procedure DrawGroupBoxFrame(ACanvas: TcxCanvas; R: TRect;
      AEnabled: Boolean; ACaptionPosition: TcxGroupBoxCaptionPosition;
      ABorders: TcxBorders = cxBordersAll); override;
    function GroupBoxTextColor(AEnabled: Boolean; ACaptionPosition: TcxGroupBoxCaptionPosition): TColor; override;
    // TrackBar
    procedure DrawTrackBarTrackBounds(ACanvas: TcxCanvas; const ARect: TRect); override;
    procedure DrawTrackBarThumbBorderUpDown(ACanvas: TcxCanvas;
      const ALightPolyLine, AShadowPolyLine, ADarkPolyLine: TPoints); override;
    procedure DrawTrackBarThumbBorderBoth(ACanvas: TcxCanvas; const ARect: TRect); override;
    // Panel
    procedure DrawPanelBackground(ACanvas: TcxCanvas; AControl: TWinControl; ABounds: TRect;
      AColorFrom: TColor = clDefault; AColorTo: TColor = clDefault); override;
    // Layout Control
    procedure DrawLayoutControlBackground(ACanvas: TcxCanvas; const R: TRect); override;
    // DateNavigator
    procedure DrawDateNavigatorDateHeader(ACanvas: TcxCanvas; var R: TRect); override;
    // Splitter
    function GetSplitterInnerColor(AHighlighted: Boolean): TColor; override;
    function GetSplitterOuterColor(AHighlighted: Boolean): TColor; override;
    // navigator
    function NavigatorBorderSize: Integer; override;
    // BreadcrumbEdit
    function BreadcrumbEditBackgroundColor(AState: TdxBreadcrumbEditState): TColor; override;
    procedure DrawBreadcrumbEditBorders(ACanvas: TcxCanvas; const ARect: TRect;
      ABorders: TcxBorders; AState: TdxBreadcrumbEditState); override;
    // CustomMenu
    procedure DrawDropDownListBoxBackground(ACanvas: TcxCanvas; const ARect: TRect; AHasBorders: Boolean); override;
    procedure DrawDropDownListBoxScaledGutterBackground(ACanvas: TcxCanvas; const ARect: TRect; AScaleFactor: TdxScaleFactor); override;
    procedure DrawDropDownListBoxScaledSelection(ACanvas: TcxCanvas; const ARect: TRect; const AGutterRect: TRect; AScaleFactor: TdxScaleFactor); override;
    procedure DrawDropDownListBoxScaledSeparator(ACanvas: TcxCanvas; const ARect: TRect; const AGutterRect: TRect; AScaleFactor: TdxScaleFactor); override;
    //AlertWindow
    procedure DrawAlertWindowBackground(ACanvas: TcxCanvas; const ABounds: TRect; AScaleFactor: TdxScaleFactor = nil); override;

    // Bevel
    function GetBevelMinimalShapeSize(AShape: TdxBevelShape): TSize; override;
    procedure GetBevelShapeColors(out AColor1, AColor2: TColor); override;

    // Clock
    function ScaledClockSize(AScaleFactor: TdxScaleFactor): TSize; override;
    procedure DrawScaledClock(ACanvas: TcxCanvas; const ARect: TRect;
      ADateTime: TDateTime; ABackgroundColor: TColor; AScaleFactor: TdxScaleFactor); override;
    // RichEditControl
    function RichEditControlHeaderFooterLineColor: TColor; override;
    function RichEditControlHeaderFooterMarkBackColor: TColor; override;
    function RichEditRulerInactiveAreaColor: TColor; override;
    function RichEditRulerTabTypeToggleBorderColor: TColor; override;
	  // TokenEdit
    procedure DrawScaledTokenBackground(ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState; AScaleFactor: TdxScaleFactor); override;
  end;

  { TcxWinXPLookAndFeelPainter }

  TcxWinXPLookAndFeelPainter = class(TcxStandardLookAndFeelPainter)
  private
    FZoomInButtonGlyph: TcxBitmap32;
    FZoomOutButtonGlyph: TcxBitmap32;

    procedure DrawNativeRangeTrackBarThumb(ACanvas: TcxCanvas; const ARect: TRect; AState: TcxButtonState;
      AHorizontal: Boolean; ATicks: TcxTrackBarTicksAlign; AThumbColor: TColor; AScaleFactor: TdxScaleFactor);
  protected
    procedure DoDrawScaledScrollBarPart(ACanvas: TcxCanvas; AHorizontal: Boolean; R: TRect; APart: TcxScrollBarPart;
      AState: TcxButtonState; AScaleFactor: TdxScaleFactor); override;
    procedure DrawBreadcrumbEditNodePart(ACanvas: TcxCanvas; const R: TRect;
      AState: TdxBreadcrumbEditButtonState; ABorders: TcxBorders; AScaleFactor: TdxScaleFactor);
    procedure DrawContent(ACanvas: TcxCanvas; ATheme: TdxTheme; APartId, AStateId: Integer;
      const ABounds, ATextAreaBounds: TRect; AAlignmentHorz: TAlignment;
      AAlignmentVert: TcxAlignmentVert; AMultiLine, AShowEndEllipsis, AShowPrefix: Boolean;
      const AText: string; AFont: TFont; ATextColor, ABkColor: TColor); reintroduce; virtual;
    procedure DrawSchedulerNavigationButtonContent(ACanvas: TcxCanvas;
      const ARect: TRect; const AArrowRect: TRect; AIsNextButton: Boolean;
      AState: TcxButtonState; const AIsVertical: Boolean = True); override;
    procedure DrawThemedTab(ACanvas: TcxCanvas; R: TRect; ABorders: TcxBorders;
      const AText: string; ATheme: TdxTheme; AState: Integer; AVertical: Boolean;
      AFont: TFont; ATextColor, ABkColor: TColor; AShowPrefix: Boolean = False);
    function NativeZoomButtonDraw(ACanvas: TcxCanvas; const R: TRect;
      AState: TcxButtonState; AScaleFactor: TdxScaleFactor; AGlyph: TBitmap): Boolean;
    function NativeZoomButtonGetMinSize(AGlyph: TBitmap; AScaleFactor: TdxScaleFactor; out ASize: TSize): Boolean;

    property ZoomInButtonGlyph: TcxBitmap32 read FZoomInButtonGlyph;
    property ZoomOutButtonGlyph: TcxBitmap32 read FZoomOutButtonGlyph;
  public
    destructor Destroy; override;
    procedure AfterConstruction; override;
    function LookAndFeelName: string; override;
    function LookAndFeelStyle: TcxLookAndFeelStyle; override;
    function NeedRedrawOnResize: Boolean; override;

    class procedure DrawScaledThemeBackground(ATheme: TdxTheme; DC: HDC; APartId, AStateId: Integer; const R: TRect;
      AScaleFactor: TdxScaleFactor; AForceScale: Boolean = False; AUseThemePartSize: Boolean = True);

    // colors
    function DefaultSchedulerBorderColor: TColor; override;
    // arrow
    procedure DrawScaledArrow(ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState; AArrowDirection: TcxArrowDirection; AScaleFactor: TdxScaleFactor; ADrawBorder: Boolean = True); override;
    procedure DrawScaledArrowBorder(ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState; AScaleFactor: TdxScaleFactor); override;
    // border
    function BorderSize: Integer; override;
    function SeparatorSize: Integer; override;
    procedure DrawBorder(ACanvas: TcxCanvas; R: TRect); override;
    procedure DoDrawSeparator(ACanvas: TcxCanvas; R: TRect; AIsVertical: Boolean); override;
    // buttons
    function ButtonBorderSize(AState: TcxButtonState = cxbsNormal): Integer; override;
    function ButtonColor(AState: TcxButtonState): TColor; override;
    function ScaledButtonFocusRect(ACanvas: TcxCanvas; R: TRect; AScaleFactor: TdxScaleFactor): TRect; override;
    function ButtonSymbolColor(AState: TcxButtonState;
      ADefaultColor: TColor = clDefault): TColor; override;
    function ScaledButtonTextOffset(AScaleFactor: TdxScaleFactor): Integer; override;
    function ScaledButtonTextShift(AScaleFactor: TdxScaleFactor): Integer; override;
    procedure DrawScaledButton(ACanvas: TcxCanvas; R: TRect; const ACaption: string; AState: TcxButtonState;
      AScaleFactor: TdxScaleFactor; ADrawBorder: Boolean = True; AColor: TColor = clDefault; ATextColor: TColor = clDefault;
      AWordWrap: Boolean = False; AIsToolButton: Boolean = False; APart: TcxButtonPart = cxbpButton); override;

    //CommandLink
    function DefaultCommandLinkTextColor(AState: TcxButtonState; ADefaultColor: TColor = clDefault): TColor; override;
    procedure DrawScaledCommandLinkBackground(ACanvas: TcxCanvas; R: TRect; AState: TcxButtonState;
      AScaleFactor: TdxScaleFactor; AColor: TColor = clDefault); override;
    procedure DrawScaledCommandLinkGlyph(ACanvas: TcxCanvas; const AGlyphPos: TPoint;
      AState: TcxButtonState; AScaleFactor: TdxScaleFactor); override;
    function GetScaledCommandLinkGlyphSize(AScaleFactor: TdxScaleFactor): TSize; override;
    function GetScaledCommandLinkMargins(AScaleFactor: TdxScaleFactor): TRect; override;

    procedure DrawScaledExpandButton(ACanvas: TcxCanvas; const R: TRect; AExpanded: Boolean;
      AScaleFactor: TdxScaleFactor; AColor: TColor = clDefault; AState: TcxExpandButtonState = cebsNormal); override;
    function DrawExpandButtonFirst: Boolean; override;
    procedure DrawScaledGroupExpandButton(ACanvas: TcxCanvas; const R: TRect;
      AExpanded: Boolean; AState: TcxButtonState; AScaleFactor: TdxScaleFactor); override;
    procedure DrawScaledSmallButton(ACanvas: TcxCanvas; R: TRect; AState: TcxButtonState; AScaleFactor: TdxScaleFactor); override;
    procedure DrawScaledSmallExpandButton(ACanvas: TcxCanvas; R: TRect;
      AExpanded: Boolean; ABorderColor: TColor; AScaleFactor: TdxScaleFactor; AColor: TColor = clDefault); override;
    function ScaledExpandButtonSize(AScaleFactor: TdxScaleFactor): Integer; override;
    function ScaledGroupExpandButtonSize(AScaleFactor: TdxScaleFactor): Integer; override;
    function ScaledSmallExpandButtonSize(AScaleFactor: TdxScaleFactor): Integer; override;
    function IsButtonHotTrack: Boolean; override;
    function IsPointOverGroupExpandButton(const R: TRect; const P: TPoint): Boolean; override;
    // checkbox
    function CheckBorderSize: Integer; override;
    function ScaledCheckButtonSize(AScaleFactor: TdxScaleFactor): TSize; override;
    procedure DrawScaledCheck(ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState;
      ACheckState: TcxCheckBoxState; AColor: TColor; AScaleFactor: TdxScaleFactor); override;
    procedure DrawCheckBorder(ACanvas: TcxCanvas; R: TRect; AState: TcxButtonState); override;
    // RadioGroup
    procedure DrawScaledRadioButton(ACanvas: TcxCanvas; X, Y: Integer; AButtonState: TcxButtonState;
      AChecked, AFocused: Boolean; ABrushColor: TColor; AScaleFactor: TdxScaleFactor; AIsDesigning: Boolean = False); override;
    function ScaledRadioButtonSize(AScaleFactor: TdxScaleFactor): TSize; override;
    // BackButton
    function GetScaledBackButtonSize(AScaleFactor: TdxScaleFactor): TSize; override;
    procedure DrawScaledBackButton(ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState; AScaleFactor: TdxScaleFactor); override;
    // ZoomButtons
    function GetScaledZoomInButtonSize(AScaleFactor: TdxScaleFactor): TSize; override;
    function GetScaledZoomOutButtonSize(AScaleFactor: TdxScaleFactor): TSize; override;
    procedure DrawScaledZoomInButton(ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState; AScaleFactor: TdxScaleFactor); override;
    procedure DrawScaledZoomOutButton(ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState; AScaleFactor: TdxScaleFactor); override;
    // header
    procedure DrawScaledHeader(ACanvas: TcxCanvas; const ABounds, ATextAreaBounds: TRect; ANeighbors: TcxNeighbors;
      ABorders: TcxBorders; AState: TcxButtonState; AAlignmentHorz: TAlignment; AAlignmentVert: TcxAlignmentVert;
      AMultiLine, AShowEndEllipsis: Boolean; const AText: string; AFont: TFont; ATextColor, ABkColor: TColor;
      AScaleFactor: TdxScaleFactor; AOnDrawBackground: TcxDrawBackgroundEvent = nil; AIsLast: Boolean = False;
      AInGroupByBox: Boolean = False); override;
    procedure DrawHeaderPressed(ACanvas: TcxCanvas; const ABounds: TRect); override;
    procedure DrawScaledHeaderControlSection(ACanvas: TcxCanvas; const ABounds, ATextAreaBounds: TRect; ANeighbors: TcxNeighbors;
      ABorders: TcxBorders; AState: TcxButtonState; AAlignmentHorz: TAlignment; AAlignmentVert: TcxAlignmentVert;
      AMultiLine, AShowEndEllipsis: Boolean; const AText: string; AFont: TFont; ATextColor, ABkColor: TColor; AScaleFactor: TdxScaleFactor); override;
    procedure DrawScaledSortingMark(ACanvas: TcxCanvas; const R: TRect; AAscendingSorting: Boolean; AScaleFactor: TdxScaleFactor); override;
    procedure DrawSpreadSheetScaledHeader(ACanvas: TcxCanvas; const ABounds, ATextAreaBounds: TRect; ANeighbors: TcxNeighbors;
      ABorders: TcxBorders; AState: TcxButtonState; AAlignmentHorz: TAlignment; AAlignmentVert: TcxAlignmentVert;
      AMultiLine, AShowEndEllipsis: Boolean; const AText: string; AFont: TFont; ATextColor, ABkColor: TColor; AScaleFactor: TdxScaleFactor;
      AOnDrawBackground: TcxDrawBackgroundEvent = nil; AIsLast: Boolean = False; AIsGroup: Boolean = False); override;
    procedure DrawScaledSummarySortingMark(ACanvas: TcxCanvas; const R: TRect; AAscendingSorting: Boolean; AScaleFactor: TdxScaleFactor); override;
    function HeaderBorderSize: Integer; override;
    function HeaderControlSectionBorderSize(AState: TcxButtonState = cxbsNormal): Integer; override;
    function HeaderControlSectionContentBounds(const ABounds: TRect; AState: TcxButtonState): TRect; override;
    function IsHeaderHotTrack: Boolean; override;
    function ScaledSortingMarkSize(AScaleFactor: TdxScaleFactor): TPoint; override;
    // filter row separator
    procedure DrawFilterRowSeparator(ACanvas: TcxCanvas; const ARect: TRect; ABackgroundColor: TColor); override;
    // footer
    procedure DrawFooterBorder(ACanvas: TcxCanvas; const R: TRect); override;
    procedure DrawFooterCell(ACanvas: TcxCanvas; const ABounds: TRect;
      AAlignmentHorz: TAlignment; AAlignmentVert: TcxAlignmentVert; AMultiLine: Boolean;
      const AText: string; AFont: TFont; ATextColor, ABkColor: TColor;
      AOnDrawBackground: TcxDrawBackgroundEvent = nil); override;
    function FooterBorderSize: Integer; override;
    function FooterCellBorderSize: Integer; override;
    function FooterCellOffset: Integer; override;
    function FooterDrawCellsFirst: Boolean; override;

    // filter
    procedure DrawScaledFilterCloseButton(ACanvas: TcxCanvas; R: TRect;
      AState: TcxButtonState; AScaleFactor: TdxScaleFactor); override;
    procedure DrawScaledFilterDropDownButton(ACanvas: TcxCanvas; R: TRect;
      AState: TcxButtonState; AIsFilterActive: Boolean; AScaleFactor: TdxScaleFactor); override;
    function ScaledFilterActivateButtonSize(AScaleFactor: TdxScaleFactor): TPoint; override;
    function ScaledFilterCloseButtonSize(AScaleFactor: TdxScaleFactor): TPoint; override;

    //LayoutView
    function DefaultLayoutViewContentTextColor(AState: TcxButtonState): TColor; override;
    //LayoutView
    function DefaultLayoutViewCaptionTextColor(ACaptionPosition: TcxGroupBoxCaptionPosition; AState: TcxButtonState): TColor; override;
    procedure LayoutViewDrawRecordCaption(ACanvas: TcxCanvas; const ABounds, ATextRect: TRect;
      APosition: TcxGroupBoxCaptionPosition; AState: TcxButtonState; AColor: TColor = clDefault;
      const ABitmap: TBitmap = nil); override;
    procedure LayoutViewDrawItem(ACanvas: TcxCanvas; const ABounds: TRect;
      AState: TcxButtonState; ABorders: TcxBorders = []); override;
    //WinExplorer View
    procedure WinExplorerViewDrawRecord(ACanvas: TcxCanvas; const ABounds: TRect; AState: TcxButtonState;
      AColor: TColor = clDefault; const ABitmap: TBitmap = nil); override;
    function WinExplorerViewRecordTextColor(AState: TcxButtonState): TColor; override;
    // tabs
    procedure DrawTab(ACanvas: TcxCanvas; R: TRect; ABorders: TcxBorders;
      const AText: string; AState: TcxButtonState; AVertical: Boolean; AFont: TFont;
      ATextColor, ABkColor: TColor; AShowPrefix: Boolean = False); override;
    procedure DrawTabBorder(ACanvas: TcxCanvas; R: TRect; ABorder: TcxBorder; ABorders: TcxBorders; AVertical: Boolean); override;
    procedure DrawTabsRoot(ACanvas: TcxCanvas; const R: TRect; ABorders: TcxBorders; AVertical: Boolean); override;
    function IsDrawTabImplemented(AVertical: Boolean): Boolean; override;
    function IsTabHotTrack(AVertical: Boolean): Boolean; override;
    function TabBorderSize(AVertical: Boolean): Integer; override;
    // Splitter
    procedure DrawScaledSplitter(ACanvas: TcxCanvas; const ARect: TRect; AHighlighted, AClicked, AHorizontal: Boolean;
      AScaleFactor: TdxScaleFactor; AHasCloseMark: Boolean = False; AArrowDirection: TcxArrowDirection = adLeft); override;
    // indicator
    procedure DrawScaledIndicatorItem(ACanvas: TcxCanvas; const R, AImageAreaBounds: TRect;
      AKind: TcxIndicatorKind; AColor: TColor; AScaleFactor: TdxScaleFactor; AOnDrawBackground: TcxDrawBackgroundEvent = nil;
      ANeighbors: TcxNeighbors = [nTop, nBottom]); override;
    // scrollbars
    function ScaledScrollBarMinimalThumbSize(AVertical: Boolean; AScaleFactor: TdxScaleFactor): Integer; override;
    procedure DrawScaledScrollBarSplitter(ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState; AScaleFactor: TdxScaleFactor); override;
    // ms outlook
    function DefaultSchedulerViewContentColor: TColor; override;
    function DefaultSchedulerViewContentColorClassic: TColor; override;
    procedure DrawScaledMonthHeader(ACanvas: TcxCanvas; const ABounds: TRect; const AText: string; ANeighbors: TcxNeighbors;
      const AViewParams: TcxViewParams; AArrows: TcxArrowDirections; ASideWidth: Integer; AScaleFactor: TdxScaleFactor;
      AOnDrawBackground: TcxDrawBackgroundEvent = nil); override;
    procedure DrawSchedulerEventProgress(ACanvas: TcxCanvas;
      const ABounds, AProgressChunk: TRect; AViewParams: TcxViewParams;
      ATransparent: Boolean); override;
    procedure DrawSchedulerScaledNavigatorButton(ACanvas: TcxCanvas; R: TRect; AState: TcxButtonState; AScaleFactor: TdxScaleFactor); override;
    procedure DrawSchedulerSplitterBorder(ACanvas: TcxCanvas; R: TRect;
      const AViewParams: TcxViewParams; AIsHorizontal: Boolean); override;
    function SchedulerEventProgressOffsets: TRect; override;
    // SizeGrip
    function ScaledSizeGripSize(AScaleFactor: TdxScaleFactor): TSize; override;
    procedure DoDrawSizeGrip(ACanvas: TcxCanvas; const ARect: TRect; AScaleFactor: TdxScaleFactor); override;
    // CloseButton
    function DoGetSmallCloseButtonSize: TSize; override;
    // GroupBox
    procedure DrawGroupBoxFrame(ACanvas: TcxCanvas; R: TRect; AEnabled: Boolean;
      ACaptionPosition: TcxGroupBoxCaptionPosition; ABorders: TcxBorders = cxBordersAll); override;
    function GroupBoxTextColor(AEnabled: Boolean; ACaptionPosition: TcxGroupBoxCaptionPosition): TColor; override;
    // Panel
    procedure DrawPanelBackground(ACanvas: TcxCanvas; AControl: TWinControl; ABounds: TRect;
      AColorFrom: TColor = clDefault; AColorTo: TColor = clDefault); override;
    // Popup
    procedure DrawEditPopupWindowBorder(ACanvas: TcxCanvas; var R: TRect;
      ABorderStyle: TcxEditPopupBorderStyle; AClientEdge: Boolean); override;
    function GetEditPopupWindowBorderWidth(AStyle: TcxEditPopupBorderStyle): Integer; override;
    function GetEditPopupWindowClientEdgeWidth(AStyle: TcxEditPopupBorderStyle): Integer; override;
    // Hints
      // Attributes
    function GetHintBorderColor: TColor; override;
      // Draw
    procedure DrawHintBackground(ACanvas: TcxCanvas; const ARect: TRect; AColor: TColor = clDefault); override;
    // ScreenTips
      // Attributes
    function ScreenTipGetDescriptionTextColor: TColor; override;
    function ScreenTipGetTitleTextColor: TColor; override;
    // TrackBar
    procedure CorrectThumbRect(ACanvas: TcxCanvas; var ARect: TRect; AHorizontal: Boolean; ATicks: TcxTrackBarTicksAlign); override;
    procedure DrawTrackBarScaledTrack(ACanvas: TcxCanvas; const ARect, ASelection: TRect;
      AShowSelection, AEnabled, AHorizontal: Boolean; ATrackColor: TColor; AScaleFactor: TdxScaleFactor); override;
    procedure DrawTrackBarScaledThumb(ACanvas: TcxCanvas; const ARect: TRect; AState: TcxButtonState;
      AHorizontal: Boolean; ATicks: TcxTrackBarTicksAlign; AThumbColor: TColor; AScaleFactor: TdxScaleFactor); override;
    // RangeTrackBar
    procedure DrawRangeTrackBarScaledLeftThumb(ACanvas: TcxCanvas; const ARect: TRect; AState: TcxButtonState;
      AHorizontal: Boolean; ATicks: TcxTrackBarTicksAlign; AThumbColor: TColor; AScaleFactor: TdxScaleFactor); override;
    procedure DrawRangeTrackBarScaledRightThumb(ACanvas: TcxCanvas; const ARect: TRect; AState: TcxButtonState;
      AHorizontal: Boolean; ATicks: TcxTrackBarTicksAlign; AThumbColor: TColor; AScaleFactor: TdxScaleFactor); override;
    // DateNavigator
    procedure DrawDateNavigatorDateHeader(ACanvas: TcxCanvas; var R: TRect); override;
    // navigator
    function NavigatorButtonPressedGlyphOffset: TPoint; override;
    function NavigatorScaledButtonMinSize(AScaleFactor: TdxScaleFactor): TSize; override;
    // DropDownListBox
    function DropDownListBoxItemTextColor(ASelected: Boolean): TColor; override;
    function DropDownListBoxScaledSeparatorSize(AScaleFactor: TdxScaleFactor): Integer; override;
    procedure DrawDropDownListBoxBackground(ACanvas: TcxCanvas; const ARect: TRect; AHasBorders: Boolean); override;
    procedure DrawDropDownListBoxScaledGutterBackground(ACanvas: TcxCanvas; const ARect: TRect; AScaleFactor: TdxScaleFactor); override;
    procedure DrawDropDownListBoxScaledSelection(ACanvas: TcxCanvas; const ARect: TRect; const AGutterRect: TRect; AScaleFactor: TdxScaleFactor); override;
    procedure DrawDropDownListBoxScaledSeparator(ACanvas: TcxCanvas; const ARect: TRect; const AGutterRect: TRect; AScaleFactor: TdxScaleFactor); override;
    // BreadcrumbEdit
    function BreadcrumbEditBackgroundColor(AState: TdxBreadcrumbEditState): TColor; override;
    function BreadcrumbEditBordersSize: TRect; override;
    function BreadcrumbEditScaledButtonContentOffsets(AIsFirst, AIsLast: Boolean; AScaleFactor: TdxScaleFactor): TRect; override;
    function BreadcrumbEditIsFadingSupports: Boolean; override;
    function BreadcrumbEditScaledProgressChunkOverlaySize(AScaleFactor: TdxScaleFactor): TSize; override;
    function BreadcrumbEditScaledProgressChunkPadding(AScaleFactor: TdxScaleFactor): TRect; override;
    procedure DrawBreadcrumbEditBorders(ACanvas: TcxCanvas;
      const ARect: TRect; ABorders: TcxBorders; AState: TdxBreadcrumbEditState); override;
    procedure DrawBreadcrumbEditScaledButtonAreaSeparator(ACanvas: TcxCanvas; const ARect: TRect;
      AState: TdxBreadcrumbEditState; AScaleFactor: TdxScaleFactor); override;
    procedure DrawBreadcrumbEditScaledButton(ACanvas: TcxCanvas; const ARect: TRect;
      AState: TdxBreadcrumbEditButtonState; AIsFirst, AIsLast: Boolean; AScaleFactor: TdxScaleFactor); override;
    procedure DrawBreadcrumbEditScaledDropDownButton(ACanvas: TcxCanvas; const ARect: TRect;
      AState: TdxBreadcrumbEditButtonState; AIsInEditor: Boolean; AScaleFactor: TdxScaleFactor); override;
    procedure DrawBreadcrumbEditScaledNode(ACanvas: TcxCanvas; const R: TRect;
      AState: TdxBreadcrumbEditButtonState; AHasDelimiter: Boolean; AScaleFactor: TdxScaleFactor); override;
    procedure DrawBreadcrumbEditScaledNodeDelimiter(ACanvas: TcxCanvas; const R: TRect;
      AState: TdxBreadcrumbEditButtonState; AScaleFactor: TdxScaleFactor); override;
    procedure DrawBreadcrumbEditScaledNodeDelimiterGlyph(ACanvas: TcxCanvas; const R: TRect;
      AState: TdxBreadcrumbEditButtonState; AScaleFactor: TdxScaleFactor); override;
    procedure DrawBreadcrumbEditScaledNodeMoreButtonGlyph(ACanvas: TcxCanvas; const R: TRect;
      AState: TdxBreadcrumbEditButtonState; AScaleFactor: TdxScaleFactor); override;
    procedure DrawBreadcrumbEditScaledProgressChunk(ACanvas: TcxCanvas; const R: TRect; AScaleFactor: TdxScaleFactor); override;
    procedure DrawBreadcrumbEditScaledProgressChunkOverlay(ACanvas: TcxCanvas; const R: TRect; AScaleFactor: TdxScaleFactor); override;
    //AlertWindow
    function AlertWindowCornerRadius: Integer; override;

    // Bevel
    procedure DrawBevelFrame(ACanvas: TcxCanvas; const R: TRect; AColor1, AColor2: TColor; ABoxStyle: Boolean); override;
    procedure DrawBevelLine(ACanvas: TcxCanvas; const R: TRect; AColor1, AColor2: TColor; AIsVertical: Boolean); override;
    function GetBevelMinimalShapeSize(AShape: TdxBevelShape): TSize; override;

    // Gallery
    function DrawGalleryItemSelectionFirst: Boolean; override;
    procedure DrawGalleryItemSelection(ACanvas: TcxCanvas; const R: TRect; AViewState: TdxGalleryItemViewState); override;
    //ToggleSwitch
    procedure DrawScaledToggleSwitchState(ACanvas: TcxCanvas; ABounds: TRect;
      AState: TcxButtonState; AChecked: Boolean; AScaleFactor: TdxScaleFactor); override;
    procedure DrawScaledToggleSwitchThumb(ACanvas: TcxCanvas; ABounds: TRect;
      AState: TcxButtonState; AScaleFactor: TdxScaleFactor); override;
    function ToggleSwitchToggleColor(AChecked: Boolean): TColor; override;
    // Clock
    function ScaledClockSize(AScaleFactor: TdxScaleFactor): TSize; override;
    procedure DrawScaledClock(ACanvas: TcxCanvas; const ARect: TRect;
      ADateTime: TDateTime; ABackgroundColor: TColor; AScaleFactor: TdxScaleFactor); override;
    // modern calendar
    procedure DrawModernCalendarDateCellSelection(ACanvas: TcxCanvas; const ARect: TRect; AStates: TcxCalendarElementStates); override;
    function GetModernCalendarHeaderTextColor(AStates: TcxCalendarElementStates): TColor; override;
    // Find Panel
    procedure DrawFindPanelBorder(ACanvas: TcxCanvas; const R: TRect; ABorders: TcxBorders); override;
    //WheelPicker
    procedure DrawWheelPickerItemBackground(ACanvas: TcxCanvas; const ARect: TRect; AState: TcxButtonState); override;
    procedure DrawWheelPickerItemBorder(ACanvas: TcxCanvas; const ARect: TRect; AState: TcxButtonState); override;

    // TokenEdit
    procedure DrawScaledTokenBackground(ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState; AScaleFactor: TdxScaleFactor); override;
  end;

  { TcxLookAndFeelPaintersManager }

  TcxLookAndFeelPaintersManager = class
  strict private
    FListeners: TInterfaceList;
    FPainters: TcxObjectList;

    function GetCount: Integer;
    function GetItem(AIndex: Integer): TcxCustomLookAndFeelPainter;
  protected
    procedure DoPainterAdded(APainter: TcxCustomLookAndFeelPainter);
    procedure DoPainterRemoved(APainter: TcxCustomLookAndFeelPainter);
    procedure DoRootLookAndFeelChanged;
    procedure ReleaseImageResources;
    procedure SortPainters;
    //
    property Listeners: TInterfaceList read FListeners;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure AddListener(const AListener: IcxLookAndFeelPainterListener);
    procedure RemoveListener(const AListener: IcxLookAndFeelPainterListener);

    function GetPainter(AStyle: TcxLookAndFeelStyle; out APainter: TcxCustomLookAndFeelPainter): Boolean; overload;
    function GetPainter(const AName: string; out APainter: TcxCustomLookAndFeelPainter): Boolean; overload;
    function GetPainter(AStyle: TcxLookAndFeelStyle): TcxCustomLookAndFeelPainter; overload;
    function GetPainter(const AName: string): TcxCustomLookAndFeelPainter; overload;

    function Register(APainter: TcxCustomLookAndFeelPainter): Boolean;
    function Unregister(const AName: string): Boolean;
    //
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TcxCustomLookAndFeelPainter read GetItem; default;
  end;

var
  cxEditStateColorKindMap: array[Boolean] of TcxEditStateColorKind = (esckDisabled, esckNormal);
  cxLookAndFeelPaintersManager: TcxLookAndFeelPaintersManager;

function cxDataRowFixingImages: TcxImageList;
function cxIndicatorImages: TImageList;

function BtnStateToXPBtnState(AState: TcxButtonState): Integer;

procedure PrepareRadioButtonImageList(AScaleFactor: TdxScaleFactor);
procedure UpdateScrollBarBitmaps;

procedure dxAdjustToTouchableSize(var AElementSizeDimension: Integer; AScaleFactor: TdxScaleFactor = nil); overload;
procedure dxAdjustToTouchableSize(var AElementSize: TSize; AScaleFactor: TdxScaleFactor = nil); overload;
function cxGetSchedulerGroupPolygon(const R: TRect): TPoints;
function cxGetSchedulerMilestonePolygon(const R: TRect): TPoints;
function cxTextRect(const R: TRect): TRect;
function dxElementSizeFitsForTouch(AElementSizeDimension: Integer): Boolean; overload;
function dxElementSizeFitsForTouch(AElementSizeDimension: Integer; AScaleFactor: TdxScaleFactor): Boolean; overload;
procedure dxRotateSizeGrip(ACanvas: TcxCanvas; const ARect: TRect;
  ABackgroundColor: TColor; ACorner: TdxCorner; AOnDrawSizeGrip: TdxDrawEvent); overload;
procedure dxRotateSizeGrip(ACanvas: TcxCanvas; const ARect: TRect; AScaleFactor: TdxScaleFactor;
  ABackgroundColor: TColor; ACorner: TdxCorner; AOnDrawSizeGrip: TdxDrawScaledRectEvent); overload;

implementation

{$R cxLookAndFeelPainters.res}

uses
  Messages,
  Forms, SysUtils, dxThemeConsts, cxControls, cxLookAndFeels, Math, cxDrawTextUtils, dxSkinsCore, cxLibraryStrs,
  dxDPIAwareUtils;

const
  FilterDropDownButtonWidth = 15;

  SortingMarkAreaWidth = 16;
  SortingTouchableMarkAreaWidth = 20;

  FilterActiveButtonWidth = 13;
  FilterActiveButtonHeight = 13;
  FilterCloseButtonWidth = 16;
  FilterCloseButtonHeight = 14;
  ZoomButtonWidth = 15;
  ZoomButtonHeight = 15;

  ActiveFilterButtonArrowColor = clBlue;
  cxContainerBorderWidthA1: array [TcxContainerBorderStyle] of Integer =
    (0, 1, 2, 2, 2, 1, 1);
  cxContainerBorderWidthA2: array [TcxLookAndFeelKind] of Integer =
    (2, 2, 1, 1);
  cxEditPopupClientEdgeWidthA: array[TcxEditPopupBorderStyle] of Integer =
    (0, 2, 2, 1);
  cxEditPopupWindowFrameWidthA: array[TcxEditPopupBorderStyle] of Integer =
    (0, 1, 4, 2);

  BreadcrumbButtonStateToButtonState: array[TdxBreadcrumbEditButtonState] of TcxButtonState =
    (cxbsNormal, cxbsNormal, cxbsHot, cxbsPressed, cxbsDisabled);

  TrackBarThumbThemeParts: array[TcxTrackBarTicksAlign, Boolean] of Byte = (
    (TKP_THUMBLEFT, TKP_THUMBTOP),
    (TKP_THUMBRIGHT, TKP_THUMBBOTTOM),
    (TKP_THUMBVERT, TKP_THUMB)
  );

type
  { TSystemPaletteChangedNotifier }

  TSystemPaletteChangedNotifier = class(TcxSystemPaletteChangedNotifier)
  protected
    procedure DoChanged; override;
  end;

  { TcxRadioButtonImageList }

  TcxRadioButtonImageList = class
  strict private
    FButtonMask: TBitmap;
    FList: TImageList;
    FRadioButtonCheckRect: TRect;
    FRadioButtonImageListIndexes: array of Integer;
    FRadioButtonPattern: array of array of Integer;
    FRadioButtonRect: TRect;
    FRadioButtonSize: TSize;
    FScaleFactor: TdxScaleFactor;

    procedure CalculateRadioButtonSize;
    function GetImageIndex(ALookAndFeelKind: TcxLookAndFeelKind;
      AButtonState: TcxButtonState; AChecked, AFocused, AIsDesigning: Boolean): Integer;
    function GetImageListIndexMapIndex(ALookAndFeelKind: TcxLookAndFeelKind;
      AButtonState: TcxButtonState; AChecked, AFocused, AIsDesigning: Boolean): Integer;
    procedure Initialize;
    procedure PrepareButtonMask;
    procedure PrepareRadioButtonPattern;
    procedure Reinitialize;

    procedure SetScaleFactor(AScaleFactor: TdxScaleFactor);
  public
    constructor Create(AScaleFactor: TdxScaleFactor);
    destructor Destroy; override;
    procedure Draw(ACanvas: TcxCanvas; X, Y: Integer; ABrushColor: TColor;
      ALookAndFeelKind: TcxLookAndFeelKind; AButtonState: TcxButtonState; AChecked, AFocused, AIsDesigning: Boolean);
    function GetSize: TSize;
    procedure Prepare;
    procedure Reset;

    property ScaleFactor: TdxScaleFactor read FScaleFactor write SetScaleFactor;
  end;

  { TcxRadioButtonImageListManager }

  TcxRadioButtonImageListManager = class
  strict private
    class var FCache: TDictionary<Integer, TcxRadioButtonImageList>;
  protected
    class procedure Finalize;
    class procedure Reset;
  public
    class function Get(AScaleFactor: TdxScaleFactor): TcxRadioButtonImageList;
  end;

var
  FCheckButtonSize: TSize;
  FDataRowFixingImages: TcxImageList;
  FIndicatorImages: TImageList;
  FSystemPaletteChangedNotifier: TSystemPaletteChangedNotifier;
  StdScrollBitmaps: array[Boolean] of TBitmap;

{ TcxUltraFlatLookAndFeelPainter }

function TcxUltraFlatLookAndFeelPainter.LookAndFeelName: string;
begin
  Result := 'UltraFlat';
end;

function TcxUltraFlatLookAndFeelPainter.LookAndFeelStyle: TcxLookAndFeelStyle;
begin
  Result := lfsUltraFlat;
end;

function TcxUltraFlatLookAndFeelPainter.DefaultSchedulerBorderColor: TColor;
begin
  Result := clBtnShadow;
end;

function TcxUltraFlatLookAndFeelPainter.BorderHighlightColor: TColor;
begin
  Result := clHighlight;
end;

function TcxUltraFlatLookAndFeelPainter.BorderSize: Integer;
begin
  Result := 1;
end;

procedure TcxUltraFlatLookAndFeelPainter.DrawBorder(ACanvas: TcxCanvas; R: TRect);
begin
  ACanvas.FrameRect(R, clBtnShadow);
end;

procedure TcxUltraFlatLookAndFeelPainter.DoDrawSeparator(ACanvas: TcxCanvas; R: TRect; AIsVertical: Boolean);
begin
  ACanvas.FillRect(R, clBtnShadow);
end;

function TcxUltraFlatLookAndFeelPainter.ButtonBorderSize(AState: TcxButtonState = cxbsNormal): Integer;
begin
  if AState = cxbsDefault then
    Result := 2
  else
    Result := 1;
end;

function TcxUltraFlatLookAndFeelPainter.ButtonColor(AState: TcxButtonState): TColor;
begin
  case AState of
    cxbsHot:
      Result := GetLightSelColor;
    cxbsPressed:
      Result := GetLightDownedSelColor;
  else
    Result := inherited ButtonColor(AState);
  end;
end;

function TcxUltraFlatLookAndFeelPainter.ScaledButtonFocusRect(ACanvas: TcxCanvas; R: TRect; AScaleFactor: TdxScaleFactor): TRect;
begin
  Result := R;
  InflateRect(Result, AScaleFactor.Apply(-3), AScaleFactor.Apply(-3));
  if IsRectEmpty(Result) then
    Result := R;
end;

function TcxUltraFlatLookAndFeelPainter.ButtonSymbolColor(
  AState: TcxButtonState; ADefaultColor: TColor = clDefault): TColor;
begin
  if AState = cxbsPressed then
    Result := clHighlightText
  else
    Result := inherited ButtonSymbolColor(AState, ADefaultColor);
end;

function TcxUltraFlatLookAndFeelPainter.ButtonSymbolState(AState: TcxButtonState): TcxButtonState;
begin
  Result := cxbsNormal;
end;

function TcxUltraFlatLookAndFeelPainter.ScaledButtonTextOffset(AScaleFactor: TdxScaleFactor): Integer;
begin
  Result := AScaleFactor.Apply(2);
end;

function TcxUltraFlatLookAndFeelPainter.ScaledButtonTextShift(AScaleFactor: TdxScaleFactor): Integer;
begin
  Result := 0;
end;

procedure TcxUltraFlatLookAndFeelPainter.DrawButtonBorder(ACanvas: TcxCanvas; R: TRect;
  AState: TcxButtonState);

  function GetBorderColor: TColor;
  begin
    case AState of
//      cxbsDefault:
//        Result := clNavy;
      cxbsDisabled:
        Result := clBtnShadow;
      cxbsNormal:
        Result := clBtnText;
    else
      Result := clHighlight;
    end;
  end;

begin
  if AState = cxbsDefault then
  begin
    ACanvas.FrameRect(R, clWindowFrame);
    InflateRect(R, -1, -1);
    ACanvas.FrameRect(R, clWindowFrame);
  end
  else
    ACanvas.FrameRect(R, GetBorderColor);
end;

procedure TcxUltraFlatLookAndFeelPainter.DrawScaledExpandButton(
  ACanvas: TcxCanvas; const R: TRect; AExpanded: Boolean;
  AScaleFactor: TdxScaleFactor; AColor: TColor = clDefault; AState: TcxExpandButtonState = cebsNormal);
var
  ARect: TRect;
begin
  ARect := R;
  ACanvas.FrameRect(ARect, clBtnShadow);
  InflateRect(ARect, -1, -1);
  ACanvas.Brush.Color := cxGetActualColor(AColor, clBtnFace);
  ACanvas.FillRect(ARect);
  DrawExpandButtonCross(ACanvas, ARect, AExpanded, clBtnText, AScaleFactor);
  ACanvas.ExcludeClipRect(R);
end;

procedure TcxUltraFlatLookAndFeelPainter.DrawHeaderControlSectionBorder(
  ACanvas: TcxCanvas; const R: TRect; ABorders: TcxBorders; AState: TcxButtonState);
begin
  if AState <> cxbsDisabled then
    ACanvas.DrawComplexFrame(R, clBlack, clBlack, ABorders)
  else
    ACanvas.DrawComplexFrame(R, clBtnShadow, clBtnShadow, ABorders);
end;

procedure TcxUltraFlatLookAndFeelPainter.DrawHeaderControlSectionContent(
  ACanvas: TcxCanvas; const ABounds, ATextAreaBounds: TRect;
  AState: TcxButtonState; AAlignmentHorz: TAlignment;
  AAlignmentVert: TcxAlignmentVert; AMultiLine, AShowEndEllipsis: Boolean;
  const AText: string; AFont: TFont; ATextColor, ABkColor: TColor);
begin
  if AState in [cxbsDefault, cxbsNormal] then
    ACanvas.SetBrushColor(ABkColor)
  else
    ACanvas.SetBrushColor(ButtonColor(AState));
  ACanvas.FillRect(ABounds);
  ACanvas.Font.Color := ATextColor;
  DrawHeaderControlSectionText(ACanvas, ATextAreaBounds, AState, AAlignmentHorz,
    AAlignmentVert, AMultiLine, AShowEndEllipsis, AText, AFont, ATextColor);
end;

function TcxUltraFlatLookAndFeelPainter.ScaledExpandButtonSize(AScaleFactor: TdxScaleFactor): Integer;
begin
  Result := AScaleFactor.Apply(11);
end;

function TcxUltraFlatLookAndFeelPainter.IsButtonHotTrack: Boolean;
begin
  Result := True;
end;

function TcxUltraFlatLookAndFeelPainter.DefaultSchedulerTimeRulerBorderColor: TColor;
begin
  Result := DefaultSchedulerTimeRulerBorderColorClassic;
end;

function TcxUltraFlatLookAndFeelPainter.DefaultSchedulerTimeRulerTextColor: TColor;
begin
  Result := DefaultSchedulerTimeRulerTextColorClassic;
end;

procedure TcxUltraFlatLookAndFeelPainter.DrawCheckBorder(ACanvas: TcxCanvas; R: TRect;
  AState: TcxButtonState);
begin
  ACanvas.FrameRect(R, clBtnText);
  InflateRect(R, -1, -1);
  ACanvas.FrameRect(R, CheckButtonColor(AState, cbsChecked));
end;

function TcxUltraFlatLookAndFeelPainter.RadioButtonBodyColor(AState: TcxButtonState): TColor;
begin
  case AState of
    cxbsHot:
      Result := GetLightSelColor;
    cxbsNormal:
      Result := clWindow{clBtnFace};
    cxbsPressed:
      Result := GetLightDownedSelColor;
    else
      Result := clBtnFace;
  end;
end;

procedure TcxUltraFlatLookAndFeelPainter.DrawHeaderBorder(ACanvas: TcxCanvas;
  const R: TRect; ANeighbors: TcxNeighbors; ABorders: TcxBorders);
begin
  ACanvas.Brush.Color := clBtnText;
  with R do
  begin
    if bLeft in ABorders then
      ACanvas.FillRect(Rect(Left, Top, Left + 1, Bottom));
    if bTop in ABorders then
      ACanvas.FillRect(Rect(Left, Top, Right, Top + 1));
    ACanvas.FillRect(Rect(Right - 1, Top, Right, Bottom));
    ACanvas.FillRect(Rect(Left, Bottom - 1, Right, Bottom));
  end;
end;

procedure TcxUltraFlatLookAndFeelPainter.DrawScaledHeaderEx(ACanvas: TcxCanvas;
  const ABounds, ATextAreaBounds: TRect; ANeighbors: TcxNeighbors; ABorders: TcxBorders;
  AState: TcxButtonState; AAlignmentHorz: TAlignment; AAlignmentVert: TcxAlignmentVert;
  AMultiLine, AShowEndEllipsis: Boolean; const AText: string; AFont: TFont;
  ATextColor, ABkColor: TColor; AScaleFactor: TdxScaleFactor; AOnDrawBackground: TcxDrawBackgroundEvent = nil);
var
  R: TRect;
begin
  R := ATextAreaBounds;
  InflateRect(R, -1, -1);
  DrawContent(ACanvas, ABounds, R, Integer(AState), AAlignmentHorz, AAlignmentVert,
    AMultiLine, AShowEndEllipsis, AText, AFont, ATextColor, ABkColor, AOnDrawBackground);
  R := ABounds;
  InflateRect(R, -1, -1);
  DrawHeaderBorder(ACanvas, R, ANeighbors, ABorders);
end;

procedure TcxUltraFlatLookAndFeelPainter.DrawScaledSortingMark(
  ACanvas: TcxCanvas; const R: TRect; AAscendingSorting: Boolean; AScaleFactor: TdxScaleFactor);
begin
  DrawSortingArrow(ACanvas, R, clBtnShadow, clBtnShadow, AAscendingSorting, AScaleFactor);
end;

procedure TcxUltraFlatLookAndFeelPainter.DrawScaledSummarySortingMark(
  ACanvas: TcxCanvas; const R: TRect; AAscendingSorting: Boolean; AScaleFactor: TdxScaleFactor);
begin
  DrawSummarySortingArrow(ACanvas, R, clBtnShadow, clBtnShadow, AAscendingSorting, AScaleFactor);
end;

function TcxUltraFlatLookAndFeelPainter.HeaderBorders(ANeighbors: TcxNeighbors): TcxBorders;
begin
  Result := inherited HeaderBorders(ANeighbors);
  if nLeft in ANeighbors then Exclude(Result, bLeft);
  if nTop in ANeighbors then Exclude(Result, bTop);
end;

function TcxUltraFlatLookAndFeelPainter.HeaderBorderSize: Integer;
begin
  Result := 1;
end;

function TcxUltraFlatLookAndFeelPainter.ScaledSortingMarkSize(AScaleFactor: TdxScaleFactor): TPoint;
begin
  Result := AScaleFactor.Apply(Point(7, 8));
end;

function TcxUltraFlatLookAndFeelPainter.ScaledSummarySortingMarkSize(AScaleFactor: TdxScaleFactor): TPoint;
begin
  Result := AScaleFactor.Apply(Point(7, 9));
end;

procedure TcxUltraFlatLookAndFeelPainter.DrawFooterBorder(ACanvas: TcxCanvas; const R: TRect);
begin
  DrawFooterBorderEx(ACanvas, R, [bRight, bBottom]);
end;

procedure TcxUltraFlatLookAndFeelPainter.DrawFooterBorderEx(ACanvas: TcxCanvas;
  const R: TRect; ABorders: TcxBorders);
begin
  ACanvas.FrameRect(R, FooterSeparatorColor, FooterSeparatorSize, ABorders);
end;

procedure TcxUltraFlatLookAndFeelPainter.DrawFooterCellBorder(ACanvas: TcxCanvas;
  const R: TRect);
begin
  ACanvas.FrameRect(R, FooterSeparatorColor);
end;

function TcxUltraFlatLookAndFeelPainter.FooterBorders: TcxBorders;
begin
  Result := [bRight, bBottom];
end;

function TcxUltraFlatLookAndFeelPainter.FooterBorderSize: Integer;
begin
  Result := 1;
end;

function TcxUltraFlatLookAndFeelPainter.FooterCellBorderSize: Integer;
begin
  Result := 1;
end;

function TcxUltraFlatLookAndFeelPainter.FooterCellOffset: Integer;
begin
  Result := 2;
end;

procedure TcxUltraFlatLookAndFeelPainter.DrawScaledFilterDropDownButton(ACanvas: TcxCanvas;
  R: TRect; AState: TcxButtonState; AIsFilterActive: Boolean; AScaleFactor: TdxScaleFactor);

  function GetArrowColor: TColor;
  begin
    if AIsFilterActive then
      Result := ActiveFilterButtonArrowColor
    else
      Result := ButtonSymbolColor(AState);
  end;

begin
  ACanvas.FrameRect(R, FilterDropDownButtonBorderColor(AState));
  InflateRect(R, -1, -1);
  ACanvas.Brush.Color := ButtonColor(AState);
  ACanvas.FillRect(R);
  DrawButtonArrow(ACanvas, R, GetArrowColor);
end;

function TcxUltraFlatLookAndFeelPainter.ScaledFilterCloseButtonSize(AScaleFactor: TdxScaleFactor): TPoint;
begin
  Result := inherited ScaledFilterCloseButtonSize(AScaleFactor);
  Inc(Result.Y);
end;

procedure TcxUltraFlatLookAndFeelPainter.DrawTabBorder(ACanvas: TcxCanvas; R: TRect;
  ABorder: TcxBorder; ABorders: TcxBorders; AVertical: Boolean);
begin
  if AVertical and (ABorder = bBottom) or not AVertical and (ABorder = bRight) then
  begin
    if not AVertical then
      Dec(R.Bottom, TabBorderSize(AVertical));
    ACanvas.Brush.Color := TabBorderDarkColor;
  end
  else
    ACanvas.Brush.Color := TabBorderHighlightColor;
  ACanvas.FillRect(R);
end;

procedure TcxUltraFlatLookAndFeelPainter.DrawTabsRoot(ACanvas: TcxCanvas;
  const R: TRect; ABorders: TcxBorders; AVertical: Boolean);
begin
  ACanvas.DrawComplexFrame(R, TabBorderHighlightColor, TabBorderHighlightColor, ABorders, TabBorderSize(AVertical));
end;

function TcxUltraFlatLookAndFeelPainter.TabBorderSize(AVertical: Boolean): Integer;
begin
  Result := 1;
end;

procedure TcxUltraFlatLookAndFeelPainter.DrawSchedulerNavigationButtonContent(
  ACanvas: TcxCanvas; const ARect: TRect; const AArrowRect: TRect; AIsNextButton: Boolean;
  AState: TcxButtonState; const AIsVertical: Boolean = True);
const
  Borders: array[Boolean, Boolean] of TcxBorders = (
    ([bLeft, bRight, bBottom], [bLeft, bTop, bRight]),
    ([bRight, bTop, bBottom], [bLeft, bTop, bBottom]) );
var
  AColor: TColor;
  R: TRect;
begin
  R := ARect;
  ACanvas.FillRect(R, clBtnFace);
  if AState = cxbsHot then
    AColor := TabBorderHighlightColor
  else
    AColor := TabBorderDarkColor;
  ACanvas.DrawComplexFrame(R, AColor, AColor, Borders[AIsVertical, AIsNextButton]);
  DrawSchedulerNavigationButtonArrow(ACanvas, AArrowRect, AIsNextButton, ButtonSymbolColor(AState), AIsVertical);
end;

function TcxUltraFlatLookAndFeelPainter.FilterDropDownButtonBorderColor(AState: TcxButtonState): TColor;
begin
  if AState = cxbsNormal then
    Result := clBtnShadow
  else
    Result := clHighlight;
end;

function TcxUltraFlatLookAndFeelPainter.TabBorderHighlightColor: TColor;
begin
  Result := clBtnShadow;
end;

function TcxUltraFlatLookAndFeelPainter.TabBorderDarkColor: TColor;
begin
  Result := clBtnText;
end;

procedure TcxUltraFlatLookAndFeelPainter.DrawSchedulerScaledNavigatorButton(
  ACanvas: TcxCanvas; R: TRect; AState: TcxButtonState; AScaleFactor: TdxScaleFactor);
begin
  DrawScaledButton(ACanvas, R, '', AState, AScaleFactor, AState in [cxbsHot, cxbsPressed]);
end;

procedure TcxUltraFlatLookAndFeelPainter.DrawSchedulerSplitterBorder(
  ACanvas: TcxCanvas; R: TRect; const AViewParams: TcxViewParams;
  AIsHorizontal: Boolean);
begin
  ACanvas.SetBrushColor(DefaultSchedulerBorderColor);
  if AIsHorizontal then
  begin
    ACanvas.FillRect(Rect(R.Left, R.Top, R.Right, R.Top + 1));
    ACanvas.FillRect(Rect(R.Left, R.Bottom - 1, R.Right, R.Bottom));
    InflateRect(R, 1, -1);
  end
  else
  begin
    ACanvas.FillRect(Rect(R.Left, R.Top, R.Left + 1, R.Bottom));
    ACanvas.FillRect(Rect(R.Right - 1, R.Top, R.Right, R.Bottom));
    InflateRect(R, -1, 1);
  end;
  ACanvas.FillRect(R, AViewParams);
end;

procedure TcxUltraFlatLookAndFeelPainter.DrawGroupBoxFrame(ACanvas: TcxCanvas;
  R: TRect; AEnabled: Boolean; ACaptionPosition: TcxGroupBoxCaptionPosition;
  ABorders: TcxBorders = cxBordersAll);
const
  FrameColorMap: array[Boolean] of TColor = (clBtnShadow, clHighlight);
begin
  ACanvas.FrameRect(R, FrameColorMap[AEnabled], 1, ABorders, True);
end;

procedure TcxUltraFlatLookAndFeelPainter.DrawTrackBarTrackBounds(ACanvas: TcxCanvas; const ARect: TRect);
begin
  ACanvas.FrameRect(ARect, clWindowText);
end;

procedure TcxUltraFlatLookAndFeelPainter.DrawTrackBarThumbBorderUpDown(ACanvas: TcxCanvas;
  const ALightPolyLine, AShadowPolyLine, ADarkPolyLine: TPoints);
begin
  ACanvas.Pen.Color := clWindowText;
  ACanvas.Polyline(ALightPolyLine);
  ACanvas.Polyline(ADarkPolyLine);
end;

procedure TcxUltraFlatLookAndFeelPainter.DrawTrackBarThumbBorderBoth(ACanvas: TcxCanvas; const ARect: TRect);
begin
  ACanvas.FrameRect(ARect, clWindowText);
end;

procedure TcxUltraFlatLookAndFeelPainter.DrawPrintPreviewScaledBackground(
  ACanvas: TcxCanvas; const R: TRect; AScaleFactor: TdxScaleFactor);

  function GetUltraFlatValue(Value: Byte): Byte;
  begin
    Result := Value + (255 - Value) div 3;
  end;

var
  AColor: TColor;
begin
  AColor := ColorToRGB(clBtnShadow);
  AColor := RGB(
    GetUltraFlatValue(GetRValue(AColor)),
    GetUltraFlatValue(GetGValue(AColor)),
    GetUltraFlatValue(GetBValue(AColor)));
  ACanvas.FillRect(R, AColor);
end;

function TcxUltraFlatLookAndFeelPainter.GetSplitterOuterColor(AHighlighted: Boolean): TColor;
begin
  if AHighlighted then
    Result := clHighlight
  else
    Result := inherited GetSplitterOuterColor(AHighlighted);
end;

function TcxUltraFlatLookAndFeelPainter.GetScaledSplitterSize(AHorizontal: Boolean; AScaleFactor: TdxScaleFactor): TSize;
begin
  Result := inherited GetScaledSplitterSize(AHorizontal, AScaleFactor);
  if AHorizontal then
    Result.cy := 1
  else
    Result.cx := 1;
end;

procedure TcxUltraFlatLookAndFeelPainter.DrawNavigatorScaledButton(ACanvas: TcxCanvas; R: TRect;
  AState: TcxButtonState; ABackgroundColor: TColor; AScaleFactor: TdxScaleFactor);
begin
  if AState in [cxbsDisabled, cxbsNormal] then
    ACanvas.FrameRect(R, ABackgroundColor)
  else
    ACanvas.FrameRect(R, BorderHighlightColor);
  ACanvas.Brush.Color := ButtonColor(AState);
  InflateRect(R, -1, -1);
  ACanvas.FillRect(R);
end;

procedure TcxUltraFlatLookAndFeelPainter.DrawNavigatorBorder(ACanvas: TcxCanvas;
  R: TRect; ASelected: Boolean);
var
  AColor: TColor;
begin
  if ASelected then
    AColor := BorderHighlightColor
  else
    AColor := clBtnShadow;
  ACanvas.FrameRect(R, AColor, 1, cxBordersAll, True);
end;

function TcxUltraFlatLookAndFeelPainter.NavigatorBorderOverlap: Boolean;
begin
  Result := True;
end;

function TcxUltraFlatLookAndFeelPainter.NavigatorBorderSize: Integer;
begin
  Result := BorderSize;
end;

procedure TcxUltraFlatLookAndFeelPainter.DrawDropDownListBoxBackground(
  ACanvas: TcxCanvas; const ARect: TRect; AHasBorders: Boolean);
var
  R: TRect;
begin
  R := ARect;
  if AHasBorders then
  begin
    ACanvas.FrameRect(R, clBtnShadow);
    R := cxRectInflate(R, -1, -1);
    ACanvas.FrameRect(R, clBtnHighlight);
    R := cxRectInflate(R, -1, -1);
  end;
  ACanvas.FillRect(R, clWindow);
end;

procedure TcxUltraFlatLookAndFeelPainter.DrawDropDownListBoxScaledSelection(
  ACanvas: TcxCanvas; const ARect, AGutterRect: TRect; AScaleFactor: TdxScaleFactor);
begin
  ACanvas.FillRect(ARect, GetLightColor(-2, 30, 72));
  ACanvas.FrameRect(ARect, clHighlight);
end;

procedure TcxUltraFlatLookAndFeelPainter.DrawDropDownListBoxScaledSeparator(
  ACanvas: TcxCanvas; const ARect, AGutterRect: TRect; AScaleFactor: TdxScaleFactor);
var
  R: TRect;
begin
  R := ARect;
  R.Left := AGutterRect.Right + 4;
  ACanvas.FillRect(cxRectCenterVertically(R, 1), clBtnShadow);
end;

procedure TcxUltraFlatLookAndFeelPainter.DrawDropDownListBoxScaledGutterBackground(
  ACanvas: TcxCanvas; const ARect: TRect; AScaleFactor: TdxScaleFactor);
begin
  ACanvas.FillRect(ARect, clBtnFace);
end;

function TcxUltraFlatLookAndFeelPainter.DropDownListBoxItemTextColor(ASelected: Boolean): TColor;
begin
  Result := clMenuText;
end;

function TcxUltraFlatLookAndFeelPainter.BreadcrumbEditScaledButtonAreaSeparatorSize(AScaleFactor: TdxScaleFactor): Integer;
begin
  Result := 1;
end;

function TcxUltraFlatLookAndFeelPainter.BreadcrumbEditScaledButtonContentOffsets(
  AIsFirst, AIsLast: Boolean; AScaleFactor: TdxScaleFactor): TRect;
begin
  Result := AScaleFactor.Apply(cxRect(3 - Ord(AIsFirst), 3, 3 - Ord(AIsLast), 3));
end;

procedure TcxUltraFlatLookAndFeelPainter.DrawBreadcrumbEditBorders(
  ACanvas: TcxCanvas; const ARect: TRect; ABorders: TcxBorders;
  AState: TdxBreadcrumbEditState);
const
  ColorMap: array[TdxBreadcrumbEditState] of TColor =
    (clBtnShadow, clHighlight, clHighlight, clBtnShadow);
begin
  ACanvas.FrameRect(ARect, ColorMap[AState], 1, ABorders);
end;

procedure TcxUltraFlatLookAndFeelPainter.DrawBreadcrumbEditScaledButtonAreaSeparator(
  ACanvas: TcxCanvas; const ARect: TRect; AState: TdxBreadcrumbEditState; AScaleFactor: TdxScaleFactor);
begin
  DrawBreadcrumbEditBorders(ACanvas, ARect, [bLeft], AState);
end;

procedure TcxUltraFlatLookAndFeelPainter.DrawBreadcrumbEditCustomButton(
  ACanvas: TcxCanvas; const R: TRect; AState: TdxBreadcrumbEditButtonState;
  ABorders: TcxBorders);
const
  BorderColorsMap: array[TdxBreadcrumbEditButtonState] of TColor =
    (clNone, clBtnShadow, clHighlight, clHighlight, clNone);
begin
  ACanvas.FillRect(R, ButtonColor(BreadcrumbButtonStateToButtonState[AState]));
  ACanvas.FrameRect(R, BorderColorsMap[AState], 1, ABorders);
end;

procedure TcxUltraFlatLookAndFeelPainter.DrawBreadcrumbEditScaledButton(
  ACanvas: TcxCanvas; const ARect: TRect; AState: TdxBreadcrumbEditButtonState;
  AIsFirst, AIsLast: Boolean; AScaleFactor: TdxScaleFactor);
var
  ABorders: TcxBorders;
begin
  if not (AState in [dxbcbsNormal, dxbcbsDisabled]) then
  begin
    ABorders := [bLeft, bRight];
    if AIsFirst then
      Exclude(ABorders, bLeft);
    if AIsLast then
      Exclude(ABorders, bRight);
    DrawBreadcrumbEditCustomButton(ACanvas, ARect, AState, ABorders);
  end;
end;

procedure TcxUltraFlatLookAndFeelPainter.DrawBreadcrumbEditScaledDropDownButton(
  ACanvas: TcxCanvas; const ARect: TRect; AState: TdxBreadcrumbEditButtonState;
  AIsInEditor: Boolean; AScaleFactor: TdxScaleFactor);
begin
  if AIsInEditor or not (AState in [dxbcbsNormal, dxbcbsDisabled]) then
    DrawBreadcrumbEditCustomButton(ACanvas, ARect, AState, [bLeft]);
end;

function TcxUltraFlatLookAndFeelPainter.GetBevelMinimalShapeSize(AShape: TdxBevelShape): TSize;
begin
  Result := cxSize(1, 1);
end;

procedure TcxUltraFlatLookAndFeelPainter.GetBevelShapeColors(out AColor1, AColor2: TColor);
begin
  AColor1 := clBtnShadow;
  AColor2 := clNone;
end;

function TcxUltraFlatLookAndFeelPainter.GridBordersOverlapSize: Integer;
begin
  Result := 1;
end;

procedure TcxUltraFlatLookAndFeelPainter.DrawScaledToggleSwitchState(
  ACanvas: TcxCanvas; ABounds: TRect; AState: TcxButtonState; AChecked: Boolean; AScaleFactor: TdxScaleFactor);
begin
  ACanvas.FillRect(ABounds, ToggleSwitchToggleColor(AChecked));
  if AChecked then
    ACanvas.Pen.Color := $000000
  else
    ACanvas.Pen.Color := $A0A0A0;

  ACanvas.Rectangle(ABounds);
end;

procedure TcxUltraFlatLookAndFeelPainter.DrawScaledToggleSwitchThumb(
  ACanvas: TcxCanvas; ABounds: TRect; AState: TcxButtonState; AScaleFactor: TdxScaleFactor);
begin
  inherited;
  ACanvas.FrameRect(cxRectInflate(ABounds, -2), $000000);
end;

procedure TcxUltraFlatLookAndFeelPainter.DrawScaledTokenBackground(
  ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState; AScaleFactor: TdxScaleFactor);
const
  ColorMap: array [TcxButtonState] of TColor = (clBtnShadow, clBtnShadow, clHighlight, clHighlight, clBtnShadow);
begin
  ACanvas.FrameRect(R, ColorMap[AState]);
  if not (AState in [cxbsNormal, cxbsDisabled]) then
    ACanvas.FillRect(cxRectInflate(R, -1), ButtonColor(AState));
end;

function TcxUltraFlatLookAndFeelPainter.ToggleSwitchToggleColor(AChecked: Boolean): TColor;
begin
  if AChecked then
    Result := $FF9933
  else
    Result := $C8C8C8;
end;

procedure TcxUltraFlatLookAndFeelPainter.DoDrawScaledScrollBarPart(ACanvas: TcxCanvas; AHorizontal: Boolean; R: TRect;
  APart: TcxScrollBarPart; AState: TcxButtonState; AScaleFactor: TdxScaleFactor);
begin
  if IsRectEmpty(R) or ((APart = sbpThumbnail) and (AState = cxbsDisabled)) then Exit;
  case APart of
    sbpThumbnail, sbpLineUp, sbpLineDown:
      begin
        DrawScaledButton(ACanvas, R, '', AState, AScaleFactor);
        if APart <> sbpThumbnail then
          DrawScaledScrollBarArrow(ACanvas, R, AState, GetArrowDirection(AHorizontal, APart), AScaleFactor);
      end;
    sbpPageUp, sbpPageDown:
      begin
        ACanvas.Brush.Bitmap := StdScrollBitmaps[AState = cxbsPressed];
        ACanvas.FillRect(R);
        ACanvas.Brush.Bitmap := nil;
        ACanvas.Brush.Style := bsSolid;
      end;
  end;
end;

{ TcxOffice11LookAndFeelPainter }

function TcxOffice11LookAndFeelPainter.LookAndFeelName: string;
begin
  Result := 'Office11';
end;

function TcxOffice11LookAndFeelPainter.LookAndFeelStyle: TcxLookAndFeelStyle;
begin
  Result := lfsOffice11;
end;

function TcxOffice11LookAndFeelPainter.NeedRedrawOnResize: Boolean;
begin
  Result := True;
end;

function TcxOffice11LookAndFeelPainter.DefaultControlColor: TColor;
begin
  Result := dxOffice11DockColor2;
end;

function TcxOffice11LookAndFeelPainter.DefaultControlTextColor: TColor;
begin
  Result := dxOffice11TextEnabledColor;
end;

function TcxOffice11LookAndFeelPainter.DefaultDateNavigatorHeaderColor: TColor;
begin
  Result := dxOffice11DockColor1;
end;

function TcxOffice11LookAndFeelPainter.DefaultDateNavigatorSelectionColor: TColor;
begin
  Result := dxOffice11DayNavigatorSelectedColor;
end;

function TcxOffice11LookAndFeelPainter.DefaultDateNavigatorSelectionTextColor: TColor;
begin
  Result := clWindowText;
end;

function TcxOffice11LookAndFeelPainter.DefaultFilterBoxColor: TColor;
begin
  Result := dxOffice11BarFloatingBorderColor1;
end;

function TcxOffice11LookAndFeelPainter.DefaultFilterBoxTextColor: TColor;
begin
  Result := dxOffice11BarFloatingCaptionTextColor1;
end;

function TcxOffice11LookAndFeelPainter.DefaultFooterColor: TColor;
begin
  Result := dxOffice11DockColor1;  //DefaultHeaderColor;
end;

function TcxOffice11LookAndFeelPainter.DefaultFooterTextColor: TColor;
begin
  Result := DefaultHeaderTextColor;
end;

function TcxOffice11LookAndFeelPainter.DefaultGroupColor: TColor;
begin
  Result := dxOffice11GroupColor;
end;

function TcxOffice11LookAndFeelPainter.DefaultGroupByBoxColor: TColor;
begin
  Result := dxOffice11InPlaceSubItemColor{dxOffice11DockColor2};
end;

function TcxOffice11LookAndFeelPainter.DefaultGroupByBoxTextColor: TColor;
begin
  Result := dxOffice11ToolbarsColor1{dxOffice11TextEnabledColor};
end;

function TcxOffice11LookAndFeelPainter.DefaultHeaderColor: TColor;
begin
  Result := HeaderTopColor;  //dxOffice11DockColor1;
end;

function TcxOffice11LookAndFeelPainter.DefaultHeaderBackgroundColor: TColor;
begin
  Result := DefaultGroupByBoxColor;
end;

function TcxOffice11LookAndFeelPainter.DefaultSchedulerBorderColor: TColor;
begin
  Result := dxOffice11OutlookBorderColor;
end;

function TcxOffice11LookAndFeelPainter.DefaultSchedulerControlColor: TColor;
begin
  Result := dxOffice11OutlookControlColor;
end;

function TcxOffice11LookAndFeelPainter.DefaultSchedulerDayHeaderColor: TColor;
begin
  Result := DefaultHeaderColor;
end;

function TcxOffice11LookAndFeelPainter.DefaultTabColor: TColor;
begin
  Result := dxOffice11DockColor2;
end;

function TcxOffice11LookAndFeelPainter.DefaultTabsBackgroundColor: TColor;
begin
  Result := DefaultTabColor;
end;

function TcxOffice11LookAndFeelPainter.DefaultTimeGridMinorScaleColor: TColor;
begin
  Result := dxOffice11DockColor1;
end;

function TcxOffice11LookAndFeelPainter.DefaultTimeGridSelectionBarColor: TColor;
begin
  Result := DefaultSchedulerControlColor;
end;

function TcxOffice11LookAndFeelPainter.SeparatorSize: Integer;
begin
  Result := 2;
end;

function TcxOffice11LookAndFeelPainter.BorderHighlightColor: TColor;
begin
  Result := dxOffice11SelectedBorderColor;
end;

procedure TcxOffice11LookAndFeelPainter.DrawBorder(ACanvas: TcxCanvas; R: TRect);
begin
  ACanvas.FrameRect(R, dxOffice11ControlBorderColor);
end;

procedure TcxOffice11LookAndFeelPainter.DoDrawSeparator(ACanvas: TcxCanvas; R: TRect; AIsVertical: Boolean);
begin
  Dec(R.Right);
  Dec(R.Bottom);
  ACanvas.FillRect(R, dxOffice11BarSeparatorColor1);
  OffsetRect(R, 1, 1);
  ACanvas.FillRect(R, dxOffice11BarSeparatorColor2);
end;

function TcxOffice11LookAndFeelPainter.GetBevelMinimalShapeSize(AShape: TdxBevelShape): TSize;
begin
  Result := cxSize(2);
end;

procedure TcxOffice11LookAndFeelPainter.GetBevelShapeColors(out AColor1, AColor2: TColor);
begin
  AColor1 := dxOffice11BarSeparatorColor1;
  AColor2 := dxOffice11BarSeparatorColor2;
end;

function TcxOffice11LookAndFeelPainter.ScaledClockSize(AScaleFactor: TdxScaleFactor): TSize;
begin
  Result := AScaleFactor.Apply(ClockFace.Size);
end;

procedure TcxOffice11LookAndFeelPainter.DrawScaledClock(ACanvas: TcxCanvas; const ARect: TRect;
  ADateTime: TDateTime; ABackgroundColor: TColor; AScaleFactor: TdxScaleFactor);
begin
  DrawModernCalendarClock(ACanvas, ARect, ADateTime, ABackgroundColor, AScaleFactor);
end;

function TcxOffice11LookAndFeelPainter.RichEditControlHeaderFooterLineColor: TColor;
begin
  Result := dxOffice11DockColor1;
end;

function TcxOffice11LookAndFeelPainter.RichEditControlHeaderFooterMarkBackColor: TColor;
begin
  Result := dxOffice11DockColor2;
end;

function TcxOffice11LookAndFeelPainter.RichEditRulerInactiveAreaColor: TColor;
begin
  Result := dxOffice11DockColor1;
end;

function TcxOffice11LookAndFeelPainter.RichEditRulerTabTypeToggleBorderColor: TColor;
begin
  Result := dxOffice11DropDownBorderColor1;
end;

procedure TcxOffice11LookAndFeelPainter.DrawScaledTokenBackground(
  ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState; AScaleFactor: TdxScaleFactor);

  procedure GetColors(out AColor1, AColor2: TColor);
  begin
    case AState of
      cxbsHot:
        begin
          AColor1 := dxOffice11SelectedColor1;
          AColor2 := dxOffice11SelectedColor2;
        end;
      cxbsPressed:
        begin
          AColor1 := dxOffice11DownedColor;
          AColor2 := dxOffice11SelectedColor1;
        end;
      else
      begin
        AColor1 := clNone;
        AColor2 := AColor1;
      end;
    end;
  end;

var
  AColor1, AColor2: TColor;
begin
  ACanvas.FrameRect(R, dxOffice11ControlBorderColor, 1);
  GetColors(AColor1, AColor2);
  if cxColorIsValid(AColor1) and cxColorIsValid(AColor2) then
    Office11FillTubeGradientRect(ACanvas.Handle, cxRectInflate(R, -1), AColor1, AColor2, False);
end;

function TcxOffice11LookAndFeelPainter.ButtonColor(AState: TcxButtonState): TColor;
begin
  case AState of
    cxbsHot:
      Result := dxOffice11SelectedColor1;
    cxbsPressed:
      Result := dxOffice11SelectedDownColor1;
    cxbsDisabled:
      Result := {dxOffice11ToolbarsColor2}clBtnFace;
  else
    Result := dxOffice11DockColor1{inherited ButtonColor(AState)};
  end;
end;

function TcxOffice11LookAndFeelPainter.ButtonSymbolColor(
  AState: TcxButtonState; ADefaultColor: TColor = clDefault): TColor;
begin
  if (AState = cxbsPressed) and not IsXPStandardScheme then
    Result := clHighlightText
  else
    if AState = cxbsDisabled then
      Result := dxOffice11TextDisabledColor
    else
      Result := dxOffice11TextEnabledColor;
end;

procedure TcxOffice11LookAndFeelPainter.DrawButtonBorder(ACanvas: TcxCanvas;
  R: TRect; AState: TcxButtonState);

  function GetBorderColor: TColor;
  begin
    case AState of
      cxbsNormal:
        Result := clBtnText;
      cxbsDisabled:
        Result := dxOffice11TextDisabledColor;
    else
      Result := dxOffice11SelectedBorderColor;
    end;
  end;

begin
  if AState = cxbsDefault then
    inherited
  else
    ACanvas.FrameRect(R, GetBorderColor);
end;

procedure TcxOffice11LookAndFeelPainter.DrawScaledExpandButton(
  ACanvas: TcxCanvas; const R: TRect; AExpanded: Boolean;
  AScaleFactor: TdxScaleFactor; AColor: TColor = clDefault; AState: TcxExpandButtonState = cebsNormal);
var
  AColor2: TColor;
  ABitmap: TBitmap;
begin
  if AExpanded then
    ABitmap := dxOffice11ExpandButtonBitmap1
  else
    ABitmap := dxOffice11ExpandButtonBitmap2;

  AColor2 := dxGetMiddleRGB(dxOffice11ExpandButtonColor1, ACanvas.Canvas.Pixels[R.Left, R.Top], 25);
  ABitmap.Canvas.Pixels[0, 0] := AColor2;
  AColor2 := dxGetMiddleRGB(dxOffice11ExpandButtonColor2, ACanvas.Canvas.Pixels[R.Right - 1, R.Bottom - 1], 25);
  ABitmap.Canvas.Pixels[10, 10] := AColor2;
  ABitmap.Canvas.Pixels[0, 10] := AColor2;
  ABitmap.Canvas.Pixels[10, 0] := AColor2;
  cxDrawImage(ACanvas, R, ABitmap, ifmProportionalStretch, nil, AScaleFactor);
end;

function TcxOffice11LookAndFeelPainter.DrawExpandButtonFirst: Boolean;
begin
  Result := False;
end;

procedure TcxOffice11LookAndFeelPainter.DrawScaledSmallExpandButton(ACanvas: TcxCanvas;
  R: TRect; AExpanded: Boolean; ABorderColor: TColor; AScaleFactor: TdxScaleFactor; AColor: TColor = clDefault);
begin
  DrawScaledExpandButton(ACanvas, R, AExpanded, AScaleFactor, AColor);
end;

function TcxOffice11LookAndFeelPainter.ScaledExpandButtonSize(AScaleFactor: TdxScaleFactor): Integer;
begin
  Result := AScaleFactor.Apply(11);
end;

function TcxOffice11LookAndFeelPainter.ScaledSmallExpandButtonSize(AScaleFactor: TdxScaleFactor): Integer;
begin
  Result := ScaledExpandButtonSize(AScaleFactor);
end;

function TcxOffice11LookAndFeelPainter.CheckButtonColor(AState: TcxButtonState; ACheckState: TcxCheckBoxState): TColor;
begin
  case AState of
    cxbsHot, cxbsPressed, cxbsDisabled:
      Result := ButtonColor(AState);
  else
    Result := inherited CheckButtonColor(AState, ACheckState);
  end;
end;

function TcxOffice11LookAndFeelPainter.RadioButtonBodyColor(AState: TcxButtonState): TColor;
begin
  if AState = cxbsNormal then
    Result := clWindow
  else
    Result := ButtonColor(AState)
end;

procedure TcxOffice11LookAndFeelPainter.DrawScaledFilterDropDownButton(
  ACanvas: TcxCanvas; R: TRect; AState: TcxButtonState; AIsFilterActive: Boolean; AScaleFactor: TdxScaleFactor);

  function GetArrowColor: TColor;
  begin
    if AIsFilterActive then
      Result := ActiveFilterButtonArrowColor
    else
      Result := ButtonSymbolColor(AState);
  end;

begin
//  inherited;
  ACanvas.FrameRect(R, FilterDropDownButtonBorderColor(AState));
  InflateRect(R, -1, -1);
  ACanvas.Brush.Color := ButtonColor(AState);
  ACanvas.FillRect(R);
  DrawButtonArrow(ACanvas, R, GetArrowColor);
end;

procedure TcxOffice11LookAndFeelPainter.DrawScaledHeader(ACanvas: TcxCanvas; const ABounds, ATextAreaBounds: TRect;
  ANeighbors: TcxNeighbors; ABorders: TcxBorders; AState: TcxButtonState; AAlignmentHorz: TAlignment;
  AAlignmentVert: TcxAlignmentVert; AMultiLine, AShowEndEllipsis: Boolean; const AText: string; AFont: TFont;
  ATextColor, ABkColor: TColor; AScaleFactor: TdxScaleFactor; AOnDrawBackground: TcxDrawBackgroundEvent = nil;
  AIsLast: Boolean = False; AInGroupByBox: Boolean = False);
begin
  inherited DrawScaledHeader(ACanvas, ABounds, ATextAreaBounds, ANeighbors, ABorders,
    AState, AAlignmentHorz, AAlignmentVert, AMultiLine, AShowEndEllipsis, AText,
    AFont, ATextColor, ABkColor, dxDefaultScaleFactor, TcxDrawBackgroundEvent(nil), AIsLast, False);
end;

procedure TcxOffice11LookAndFeelPainter.DrawHeaderBorder(ACanvas: TcxCanvas;
  const R: TRect; ANeighbors: TcxNeighbors; ABorders: TcxBorders);
var
  AColor1, AColor2: TColor;
  Y1, Y2: Integer;
begin
  AColor1 := HeaderTopColor;
  AColor2 := HeaderBottomColor;
  if bTop in ABorders then
  begin
    ACanvas.Brush.Color := AColor1;
    ACanvas.FillRect(Rect(R.Left, R.Top, R.Right, R.Top + 1));
  end;
  if bBottom in ABorders then
  begin
    ACanvas.Brush.Color := HeaderDarkEdgeColor;
    ACanvas.FillRect(Rect(R.Left, R.Bottom - 1, R.Right, R.Bottom));
  end;
  if bLeft in ABorders then
  begin
    ACanvas.Pixels[R.Left, R.Top + 1] := AColor1;
    if nLeft in ANeighbors then
    begin
      Y1 := R.Top + 2;
      Y2 := R.Bottom - 3;
      ACanvas.Pixels[R.Left, Y1] := AColor1;
      ACanvas.Pixels[R.Left, Y2] := dxGetMiddleRGB(AColor1, AColor2, 25);
      ACanvas.Pixels[R.Left, R.Bottom - 2] := AColor2;
    end
    else
    begin
      Y1 := R.Top + 1;
      Y2 := R.Bottom - 2;
      ACanvas.Pixels[R.Left, R.Bottom - 2] := AColor2;
    end;
    ACanvas.Brush.Color := HeaderHighlightEdgeColor;
    ACanvas.FillRect(Rect(R.Left, Y1, R.Left + 1, Y2));
  end;
  if bRight in ABorders then
  begin
    if nRight in ANeighbors then
    begin
      Y1 := R.Top + 2;
      Y2 := R.Bottom - 3;
      ACanvas.Pixels[R.Right - 1, R.Top + 1] := AColor1;
      ACanvas.Pixels[R.Right - 1, Y1] := AColor1;
      ACanvas.Pixels[R.Right - 1, Y2] := dxGetMiddleRGB(AColor1, AColor2, 25);
      ACanvas.Pixels[R.Right - 1, R.Bottom - 2] := AColor2;
    end
    else
    begin
      Y1 := R.Top + 1;
      ACanvas.Pixels[R.Right - 1, R.Top] := dxGetMiddleRGB(AColor1, HeaderDarkEdgeColor, 50);
      Y2 := R.Bottom - 2;
      ACanvas.Pixels[R.Right - 1, R.Bottom - 2] := dxGetMiddleRGB(AColor2, HeaderDarkEdgeColor, 50);
    end;
    ACanvas.Brush.Color := HeaderDarkEdgeColor;
    ACanvas.FillRect(Rect(R.Right - 1, Y1, R.Right, Y2));
  end;
end;

procedure TcxOffice11LookAndFeelPainter.DrawScaledHeaderControlSection(
  ACanvas: TcxCanvas; const ABounds, ATextAreaBounds: TRect;
  ANeighbors: TcxNeighbors; ABorders: TcxBorders; AState: TcxButtonState;
  AAlignmentHorz: TAlignment; AAlignmentVert: TcxAlignmentVert; AMultiLine,
  AShowEndEllipsis: Boolean; const AText: string; AFont: TFont; ATextColor,
  ABkColor: TColor; AScaleFactor: TdxScaleFactor);
begin
  DrawScaledHeader(ACanvas, ABounds, ATextAreaBounds, [nRight], ABorders, AState, AAlignmentHorz, AAlignmentVert, AMultiLine,
    AShowEndEllipsis, AText, AFont, ATextColor, ABkColor, AScaleFactor);
  if AState = cxbsPressed then
    DrawHeaderPressed(ACanvas, ABounds);
end;

procedure TcxOffice11LookAndFeelPainter.DrawScaledSortingMark(
  ACanvas: TcxCanvas; const R: TRect; AAscendingSorting: Boolean; AScaleFactor: TdxScaleFactor);
begin
  DrawSortingArrow(ACanvas, R, HeaderDarkEdgeColor, HeaderHighlightEdgeColor, AAscendingSorting, AScaleFactor);
end;

procedure TcxOffice11LookAndFeelPainter.DrawScaledSummarySortingMark(
  ACanvas: TcxCanvas; const R: TRect; AAscendingSorting: Boolean; AScaleFactor: TdxScaleFactor);
begin
  DrawSummarySortingArrow(ACanvas, R, HeaderDarkEdgeColor, HeaderHighlightEdgeColor, AAscendingSorting, AScaleFactor);
end;

function TcxOffice11LookAndFeelPainter.HeaderBorders(ANeighbors: TcxNeighbors): TcxBorders;
begin
  Result := cxBordersAll;
end;

function TcxOffice11LookAndFeelPainter.HeaderBorderSize: Integer;
begin
  Result := 1;
end;

function TcxOffice11LookAndFeelPainter.IsHeaderHotTrack: Boolean;
begin
  Result := True;
end;

function TcxOffice11LookAndFeelPainter.FooterSeparatorColor: TColor;
begin
  Result := HeaderDarkEdgeColor;
end;

function TcxOffice11LookAndFeelPainter.GridBordersOverlapSize: Integer;
begin
  Result := 0;
end;

procedure TcxOffice11LookAndFeelPainter.LayoutViewDrawRecordCaption(ACanvas: TcxCanvas; const ABounds, ATextRect: TRect;
  APosition: TcxGroupBoxCaptionPosition; AState: TcxButtonState; AColor: TColor = clDefault; const ABitmap: TBitmap = nil);
begin
  LayoutViewDrawRecordBorder(ACanvas, ABounds, AState, cxBordersAll);
  DrawContent(ACanvas, ABounds, cxEmptyRect, Integer(AState), taLeftJustify,
    vaTop, False, False, '', nil, clDefault, clDefault, nil, APosition = cxgpTop);
end;

function TcxOffice11LookAndFeelPainter.ScaledScrollBarMinimalThumbSize(AVertical: Boolean;
  AScaleFactor: TdxScaleFactor): Integer;
var
  APainter: TcxCustomLookAndFeelPainter;
begin
  if cxLookAndFeelPaintersManager.GetPainter(lfsNative, APainter) then
    Result := APainter.ScaledScrollBarMinimalThumbSize(AVertical, AScaleFactor)
  else
    Result := inherited ScaledScrollBarMinimalThumbSize(AVertical, AScaleFactor);
end;

procedure TcxOffice11LookAndFeelPainter.DoDrawSizeGrip(ACanvas: TcxCanvas; const ARect: TRect; AScaleFactor: TdxScaleFactor);
begin
  Office11DrawSizeGrip(ACanvas.Handle, ARect, clDefault, clDefault, AScaleFactor);
end;

procedure TcxOffice11LookAndFeelPainter.DrawScaledMonthHeader(ACanvas: TcxCanvas; const ABounds: TRect;
  const AText: string; ANeighbors: TcxNeighbors; const AViewParams: TcxViewParams; AArrows: TcxArrowDirections;
  ASideWidth: Integer; AScaleFactor: TdxScaleFactor; AOnDrawBackground: TcxDrawBackgroundEvent = nil);
var
  R: TRect;
begin
  R := ABounds;
  InflateRect(R, -1, -1);
  with ACanvas do
  begin
    if not Assigned(AOnDrawBackground) or not AOnDrawBackground(ACanvas, R) then
      FillRect(R, AViewParams);
    if AText <> '' then
    begin
      Brush.Style := bsClear;
      Font := AViewParams.Font;
      Font.Color := AViewParams.TextColor;
      DrawText(AText, R, cxAlignCenter or cxSingleLine);
      Brush.Style := bsSolid;
    end;
  end;
  DrawMonthHeaderArrows(ACanvas, ABounds, AArrows, ASideWidth, clWindowText);
end;

procedure TcxOffice11LookAndFeelPainter.DrawSchedulerNavigationButtonContent(
  ACanvas: TcxCanvas; const ARect: TRect; const AArrowRect: TRect;
  AIsNextButton: Boolean; AState: TcxButtonState; const AIsVertical: Boolean = True);
const
  Borders: array[Boolean, Boolean] of TcxBorders = (
    ([bLeft, bRight, bBottom], [bLeft, bTop, bRight]),
    ([bRight, bTop, bBottom], [bLeft, bTop, bBottom]) );
var
  ABackgroundColor: TColor;
begin
  case AState of
    cxbsPressed:
      ABackgroundColor := dxOffice11SelectedDownColor1;
    cxbsHot:
      ABackgroundColor := dxOffice11SelectedColor1;
    else
      ABackgroundColor := dxOffice11DockColor1;
  end;
  ACanvas.FillRect(ARect, ABackgroundColor);
  ACanvas.DrawComplexFrame(ARect, dxOffice11OutlookBorderColor,
    dxOffice11OutlookBorderColor, Borders[AIsVertical, AIsNextButton]);
  DrawSchedulerNavigationButtonArrow(ACanvas, AArrowRect, AIsNextButton,
    ButtonSymbolColor(AState), AIsVertical);
end;

procedure TcxOffice11LookAndFeelPainter.DrawSchedulerScaledNavigatorButton(
  ACanvas: TcxCanvas; R: TRect; AState: TcxButtonState; AScaleFactor: TdxScaleFactor);
var
  APainter: TcxCustomLookAndFeelPainter;
begin
  if cxLookAndFeelPaintersManager.GetPainter(lfsNative, APainter) then
    APainter.DrawSchedulerScaledNavigatorButton(ACanvas, R, AState, AScaleFactor)
  else
    inherited DrawSchedulerScaledNavigatorButton(ACanvas, R, AState, AScaleFactor);
end;

procedure TcxOffice11LookAndFeelPainter.DrawSchedulerBorder(
  ACanvas: TcxCanvas; R: TRect);
begin
  ACanvas.FrameRect(R, DefaultSchedulerBorderColor);
end;

function TcxOffice11LookAndFeelPainter.DefaultDateNavigatorHeaderHighlightTextColor: TColor;
begin
  Result := clHighlightText;
end;

procedure TcxOffice11LookAndFeelPainter.DrawContent(ACanvas: TcxCanvas;
  const ABounds, ATextAreaBounds: TRect; AState: Integer; AAlignmentHorz: TAlignment;
  AAlignmentVert: TcxAlignmentVert; AMultiLine, AShowEndEllipsis: Boolean;
  const AText: string; AFont: TFont; ATextColor, ABkColor: TColor;
  AOnDrawBackground: TcxDrawBackgroundEvent = nil; AIsFooter: Boolean = False);
const
  MultiLines: array[Boolean] of Integer = (cxSingleLine, cxWordBreak);
  ShowEndEllipsises: array[Boolean] of Integer = (0, cxShowEndEllipsis);
var
  R: TRect;
  AColor1, AColor2: TColor;
begin
  with ACanvas do
  begin
    R := ABounds;
    if not Assigned(AOnDrawBackground) or not AOnDrawBackground(ACanvas, ABounds) then
    begin
      if AState = Integer(cxbsHot) then
      begin
        AColor1 := dxOffice11ToolbarsColor1;
        AColor2 := dxOffice11ToolbarsColor1;
      end
      else
      begin
        AColor1 := HeaderTopColor;
        AColor2 := HeaderBottomColor;
      end;
      with ABounds do
      begin
        if AIsFooter then
          FillRectByColor(ACanvas.Handle, Rect(Left, Top, Right, Bottom), AColor1)
        else
        begin
          FillRectByColor(ACanvas.Handle, Rect(Left, Top, Right, Bottom - 3), AColor1);
          FillGradientRect(ACanvas.Handle, Rect(Left, Bottom - 4, Right, Bottom), AColor1, AColor2, False);
        end;
      end;
    end;
    if AText <> '' then
    begin
      Brush.Style := bsClear;
      Font := AFont;
      Font.Color := ATextColor;
      DrawText(AText, ATextAreaBounds, cxAlignmentsHorz[AAlignmentHorz] or
        cxAlignmentsVert[AAlignmentVert] or MultiLines[AMultiLine] or
        ShowEndEllipsises[AShowEndEllipsis]);
      Brush.Style := bsSolid;
    end;
    if AState = Integer(cxbsHot) then
      with ABounds do
        FillGradientRect(ACanvas.Handle, Rect(Left - 1, Bottom - 2, Right, Bottom + 1),
          dxOffice11SelectedColor1, dxOffice11SelectedColor2, False);
  end;
end;

function TcxOffice11LookAndFeelPainter.HeaderBottomColor: TColor;
begin
  Result := dxOffice11ToolbarsColor2;
end;

function TcxOffice11LookAndFeelPainter.HeaderDarkEdgeColor: TColor;
begin
  Result := dxOffice11BarSeparatorColor1;
end;

function TcxOffice11LookAndFeelPainter.HeaderHighlightEdgeColor: TColor;
begin
  Result := dxOffice11BarSeparatorColor2;
end;

function TcxOffice11LookAndFeelPainter.HeaderTopColor: TColor;
begin
  Result := dxGetMiddleRGB(dxOffice11ToolbarsColor1, dxOffice11ToolbarsColor2, 50);
end;

procedure TcxOffice11LookAndFeelPainter.DoDrawScaledScrollBarPart(ACanvas: TcxCanvas; AHorizontal: Boolean; R: TRect;
  APart: TcxScrollBarPart; AState: TcxButtonState; AScaleFactor: TdxScaleFactor);
var
  APainter: TcxCustomLookAndFeelPainter;
begin
  if cxLookAndFeelPaintersManager.GetPainter(lfsNative, APainter) then
    APainter.DoDrawScaledScrollBarPart(ACanvas, AHorizontal, R, APart, AState, AScaleFactor)
  else
    inherited DoDrawScaledScrollBarPart(ACanvas, AHorizontal, R, APart, AState, AScaleFactor);
end;

function TcxOffice11LookAndFeelPainter.FilterDropDownButtonBorderColor(AState: TcxButtonState): TColor;
begin
  if AState = cxbsNormal then
    Result := HeaderDarkEdgeColor
  else
    Result := dxOffice11SelectedBorderColor;
end;

function TcxOffice11LookAndFeelPainter.TabBorderHighlightColor: TColor;
begin
  Result := HeaderDarkEdgeColor;
end;

function TcxOffice11LookAndFeelPainter.TabBorderDarkColor: TColor;
begin
  Result := inherited TabBorderDarkColor;
end;

function cxDataRowFixingImages: TcxImageList;

  procedure AddImage(ASmartImage: TdxSmartImage; const AResourceName: string);
  begin
    ASmartImage.LoadFromResource(HInstance, AResourceName, 'PNG');
    if FDataRowFixingImages = nil then
      FDataRowFixingImages := TcxImageList.CreateSize(ASmartImage.Width, ASmartImage.Height);
    FDataRowFixingImages.AddMasked(ASmartImage, clDefault);
  end;

var
  ASmartImage: TdxSmartImage;
begin
  if FDataRowFixingImages = nil then
  begin
    ASmartImage := TdxSmartImage.Create;
    try
      AddImage(ASmartImage, 'CX_DATAROWFIXEDNONE');
      AddImage(ASmartImage, 'CX_DATAROWFIXEDONTOP');
      AddImage(ASmartImage, 'CX_DATAROWFIXEDONBOTTOM');
    finally
      ASmartImage.Free;
    end;
  end;
  Result := FDataRowFixingImages;
end;

function cxIndicatorImages: TImageList;

  procedure AddImage(const AImage: TBitmap; const AResourceName: string);
  begin
    AImage.LoadFromResourceName(HInstance, AResourceName);
    if FIndicatorImages = nil then
      FIndicatorImages := TImageList.CreateSize(AImage.Width, AImage.Height);
    FIndicatorImages.AddMasked(AImage, clWhite);
  end;

var
  B: TBitmap;
begin
  if FIndicatorImages = nil then
  begin
    B := TBitmap.Create;
    try
      AddImage(B, 'CX_ARROWBITMAP');
      AddImage(B, 'CX_EDITBITMAP');
      AddImage(B, 'CX_INSERTBITMAP');
      AddImage(B, 'CX_MULTIDOTBITMAP');
      AddImage(B, 'CX_MULTIARROWBITMAP');
      AddImage(B, 'CX_FILTERBITMAP');
      AddImage(B, 'CX_INPLACEEDITBITMAP');
      AddImage(B, 'CX_SORTBYSUMMARYVALUE');
    finally
      B.Free;
    end;
  end;
  Result := FIndicatorImages;
end;

function BtnStateToXPBtnState(AState: TcxButtonState): Integer;
const
  ButtonStates: array[TcxButtonState] of Integer =(PBS_DEFAULTED, PBS_NORMAL,  PBS_HOT, PBS_PRESSED, PBS_DISABLED);
begin
  Result := ButtonStates[AState];
end;

procedure TcxOffice11LookAndFeelPainter.DrawGroupBoxFrame(ACanvas: TcxCanvas;
  R: TRect; AEnabled: Boolean; ACaptionPosition: TcxGroupBoxCaptionPosition;
  ABorders: TcxBorders = cxBordersAll);
var
  APainter: TcxCustomLookAndFeelPainter;
begin
  if AreVisualStylesAvailable([totButton]) and
    cxLookAndFeelPaintersManager.GetPainter(lfsNative, APainter)
  then
    APainter.DrawGroupBoxFrame(ACanvas, R, AEnabled, ACaptionPosition, ABorders)
  else
    inherited DrawGroupBoxFrame(ACanvas, R, AEnabled, ACaptionPosition, ABorders);
end;

function TcxOffice11LookAndFeelPainter.GroupBoxTextColor(
  AEnabled: Boolean; ACaptionPosition: TcxGroupBoxCaptionPosition): TColor;
var
  APainter: TcxCustomLookAndFeelPainter;
begin
  if AreVisualStylesAvailable([totButton]) and
    cxLookAndFeelPaintersManager.GetPainter(lfsNative, APainter)
  then
    Result := APainter.GroupBoxTextColor(AEnabled, ACaptionPosition)
  else
    Result := inherited GroupBoxTextColor(AEnabled, ACaptionPosition);
end;

procedure TcxOffice11LookAndFeelPainter.DrawTrackBarTrackBounds(ACanvas: TcxCanvas; const ARect: TRect);
begin
  ACanvas.FrameRect(ARect, clBtnText);
end;

procedure TcxOffice11LookAndFeelPainter.DrawTrackBarThumbBorderUpDown(ACanvas: TcxCanvas;
  const ALightPolyLine, AShadowPolyLine, ADarkPolyLine: TPoints);
begin
  ACanvas.Pen.Color := clBtnText;
  ACanvas.Polyline(ALightPolyLine);
  ACanvas.Polyline(ADarkPolyLine);
end;

procedure TcxOffice11LookAndFeelPainter.DrawTrackBarThumbBorderBoth(ACanvas: TcxCanvas; const ARect: TRect);
begin
  ACanvas.FrameRect(ARect, clBtnText);
end;

procedure TcxOffice11LookAndFeelPainter.DrawPanelBackground(
  ACanvas: TcxCanvas; AControl: TWinControl; ABounds: TRect; AColorFrom,
  AColorTo: TColor);
begin
  if AColorFrom = clDefault then
    cxDrawTransparentControlBackground(AControl, ACanvas, ABounds)
  else
    if AColorTo = clDefault then
      ACanvas.FillRect(ABounds, AColorFrom)
    else
      with ABounds do
        FillGradientRect(ACanvas.Handle, ABounds, AColorFrom, AColorTo, False);
end;

procedure TcxOffice11LookAndFeelPainter.DrawLayoutControlBackground(
  ACanvas: TcxCanvas; const R: TRect);
begin
  FillGradientRect(ACanvas.Handle, R,
    dxOffice11ToolbarsColor1, dxOffice11ToolbarsColor2, False);
end;

procedure TcxOffice11LookAndFeelPainter.DrawDateNavigatorDateHeader(
  ACanvas: TcxCanvas; var R: TRect);
begin
  ACanvas.FillRect(R, DefaultDateNavigatorHeaderColor);
end;

function TcxOffice11LookAndFeelPainter.GetSplitterInnerColor(AHighlighted: Boolean): TColor;
begin
  Result := dxOffice11BarSeparatorColor2;
end;

function TcxOffice11LookAndFeelPainter.GetSplitterOuterColor(AHighlighted: Boolean): TColor;
begin
  if AHighlighted then
    Result := dxOffice11SelectedBorderColor
  else
    Result := dxOffice11BarSeparatorColor1;
end;

function TcxOffice11LookAndFeelPainter.NavigatorBorderSize: Integer;
begin
  Result := 1;
end;

function TcxOffice11LookAndFeelPainter.BreadcrumbEditBackgroundColor(
  AState: TdxBreadcrumbEditState): TColor;
begin
  Result := HeaderTopColor;
end;

procedure TcxOffice11LookAndFeelPainter.DrawBreadcrumbEditBorders(ACanvas: TcxCanvas;
  const ARect: TRect; ABorders: TcxBorders; AState: TdxBreadcrumbEditState);
begin
  ACanvas.FrameRect(ARect, dxOffice11ControlBorderColor, 1, ABorders);
end;

procedure TcxOffice11LookAndFeelPainter.DrawBreadcrumbEditCustomButton(
  ACanvas: TcxCanvas; const R: TRect; AState: TdxBreadcrumbEditButtonState;
  ABorders: TcxBorders);

  procedure GetColors(out AColor1, AColor2: TColor);
  begin
    case AState of
      dxbcbsFocused:
        begin
          AColor1 := dxOffice11SelectedColor1;
          AColor2 := dxOffice11SelectedColor1;
        end;

      dxbcbsHot:
        begin
          AColor1 := dxOffice11SelectedColor1;
          AColor2 := dxOffice11SelectedColor2;
        end;

      dxbcbsPressed:
        begin
          AColor1 := dxOffice11DownedColor;
          AColor2 := dxOffice11SelectedColor1;
        end;

      else
      begin
        AColor1 := HeaderTopColor;
        AColor2 := AColor1;
      end;
    end;
  end;

var
  AColor1, AColor2: TColor;
begin
  if not cxRectIsEmpty(R) then
  begin
    GetColors(AColor1, AColor2);
    Office11FillTubeGradientRect(ACanvas.Handle, R, AColor1, AColor2, False);
    ACanvas.FrameRect(R, dxOffice11DropDownBorderColor1, 1, ABorders);
  end;
end;

procedure TcxOffice11LookAndFeelPainter.DrawDropDownListBoxBackground(
  ACanvas: TcxCanvas; const ARect: TRect; AHasBorders: Boolean);
var
  R: TRect;
begin
  R := ARect;
  if AHasBorders then
  begin
    ACanvas.FrameRect(R, dxOffice11DropDownBorderColor1);
    R := cxRectInflate(R, -1, -1);
    ACanvas.FrameRect(R, dxOffice11DropDownBorderColor2);
    R := cxRectInflate(R, -1, -1);
  end;
  ACanvas.FillRect(R, dxOffice11MenuColor);
end;

procedure TcxOffice11LookAndFeelPainter.DrawDropDownListBoxScaledSelection(
  ACanvas: TcxCanvas; const ARect: TRect; const AGutterRect: TRect; AScaleFactor: TdxScaleFactor);
begin
  FillRectByColor(ACanvas.Handle, ARect, dxOffice11SelectedColor1);
  Office11FrameSelectedRect(ACanvas.Handle, ARect);
end;

procedure TcxOffice11LookAndFeelPainter.DrawDropDownListBoxScaledSeparator(
  ACanvas: TcxCanvas; const ARect: TRect; const AGutterRect: TRect; AScaleFactor: TdxScaleFactor);
var
  R: TRect;
begin
  R := ARect;
  R.Left := AGutterRect.Right + 8;
  FillRectByColor(ACanvas.Handle, cxRectCenterVertically(R, 1), dxOffice11BarSeparatorColor1);
end;

procedure TcxOffice11LookAndFeelPainter.DrawDropDownListBoxScaledGutterBackground(
  ACanvas: TcxCanvas; const ARect: TRect; AScaleFactor: TdxScaleFactor);
begin
  Office11FillTubeGradientRect(ACanvas.Handle, ARect, dxOffice11MenuIndentColor1, dxOffice11MenuIndentColor2, True);
end;

procedure TcxOffice11LookAndFeelPainter.DrawAlertWindowBackground(
  ACanvas: TcxCanvas; const ABounds: TRect; AScaleFactor: TdxScaleFactor = nil);
var
  R: TRect;
begin
  R := ABounds;
  ACanvas.FrameRect(R, dxOffice11ControlBorderColor);
  InflateRect(R, -1, -1);
  FillGradientRect(ACanvas.Handle, R, dxOffice11DockColor2, dxOffice11DockColor1, False);
end;


{ TcxWinXPLookAndFeelPainter }

destructor TcxWinXPLookAndFeelPainter.Destroy;
begin
  FreeAndNil(FZoomOutButtonGlyph);
  FreeAndNil(FZoomInButtonGlyph);
  inherited Destroy;
end;

procedure TcxWinXPLookAndFeelPainter.AfterConstruction;
begin
  inherited AfterConstruction;
  FZoomInButtonGlyph := TcxBitmap32.Create;
  FZoomInButtonGlyph.LoadFromResourceName(HInstance, 'CX_ZOOMINBUTTONGLYPH');
  FZoomOutButtonGlyph := TcxBitmap32.Create;
  FZoomOutButtonGlyph.LoadFromResourceName(HInstance, 'CX_ZOOMOUTBUTTONGLYPH');
end;

procedure TcxWinXPLookAndFeelPainter.DrawContent(ACanvas: TcxCanvas;
  ATheme: TdxTheme; APartId, AStateId: Integer; const ABounds, ATextAreaBounds: TRect;
  AAlignmentHorz: TAlignment; AAlignmentVert: TcxAlignmentVert;
  AMultiLine, AShowEndEllipsis, AShowPrefix: Boolean;
  const AText: string; AFont: TFont; ATextColor, ABkColor: TColor);
const
  MultiLines: array[Boolean] of Integer = (DT_SINGLELINE, DT_WORDBREAK);
  ShowEndEllipsises: array[Boolean] of Integer = (0, DT_END_ELLIPSIS);
  ShowPrefixes: array[Boolean] of Integer = (DT_NOPREFIX, 0);
var
  R: TRect;
begin
  if AText = '' then Exit;
  R := ATextAreaBounds;
  ACanvas.Font := AFont;
  ACanvas.Font.Color := ATextColor;
  if AMultiLine then
    ACanvas.AlignMultiLineTextRectVertically(R, AText, AAlignmentVert, True,
      AShowPrefix, True, False, AShowEndEllipsis);
  DrawThemeText(ATheme, ACanvas.Handle, APartId, AStateId, AText,
    -1, DT_EDITCONTROL or SystemAlignmentsHorz[AAlignmentHorz] or SystemAlignmentsVert[AAlignmentVert] or
    ShowEndEllipsises[AShowEndEllipsis] or ShowPrefixes[AShowPrefix] or
    MultiLines[AMultiLine], 0, R);
end;

procedure TcxWinXPLookAndFeelPainter.DrawSchedulerNavigationButtonContent(
  ACanvas: TcxCanvas; const ARect: TRect; const AArrowRect: TRect;
  AIsNextButton: Boolean; AState: TcxButtonState; const AIsVertical: Boolean = True);
const
  ARotationAngle: array[Boolean] of TcxRotationAngle = (raMinus90, raPlus90);
  AStates: array[TcxButtonState] of Integer =
    (TIS_NORMAL, TIS_NORMAL, TIS_HOT, TIS_SELECTED, TIS_DISABLED);
var
  ABitmap: TcxBitmap;
  ATheme: TdxTheme;
begin
  ATheme := OpenTheme(totTab);
  if ATheme = 0 then
    inherited DrawSchedulerNavigationButtonContent(ACanvas, ARect, AArrowRect,
      AIsNextButton, AState, AIsVertical)
  else
  begin
    ABitmap := TcxBitmap.CreateSize(ARect, pf32bit);
    try
      cxBitBlt(ABitmap.Canvas.Handle, ACanvas.Handle, ABitmap.ClientRect, ARect.TopLeft, SRCCOPY);
      if AIsVertical then
        ABitmap.Rotate(ARotationAngle[not AIsNextButton]);
      DrawThemeBackground(ATheme, ABitmap.Canvas.Handle, TABP_TABITEM, AStates[AState],
        Rect(0, 0, ABitmap.Width, ABitmap.Height));
      if AIsVertical then
        ABitmap.Rotate(ARotationAngle[AIsNextButton])
      else
        if not AIsNextButton then
          ABitmap.Rotate(ra180);
      cxBitBlt(ACanvas.Handle, ABitmap.Canvas.Handle, ARect, cxNullPoint, SRCCOPY);
      DrawSchedulerNavigationButtonArrow(ACanvas, AArrowRect, AIsNextButton,
        ButtonSymbolColor(AState), AIsVertical);
    finally
      ABitmap.Free;
    end;
  end;
end;

function TcxWinXPLookAndFeelPainter.LookAndFeelName: string;
begin
  Result := 'Native';
end;

function TcxWinXPLookAndFeelPainter.LookAndFeelStyle: TcxLookAndFeelStyle;
begin
  Result := lfsNative;
end;

function TcxWinXPLookAndFeelPainter.NeedRedrawOnResize: Boolean;
begin
  Result := AreVisualStylesAvailable;
end;

function cxGetThemeColor(AObjectType: TdxThemedObjectType; APartId, AStateId, APropId: Integer;
  out AColor: TColor): Boolean;
var
  ATheme: TdxTheme;
  AColorRef: TColorRef;
begin
  ATheme := OpenTheme(AObjectType);
  if ATheme <> 0 then
  begin
    AColorRef := clRed;
    Result := GetThemeColor(ATheme, APartId, AStateId, APropId, AColorRef) = S_OK;
    AColor := AColorRef;
  end
  else
    Result := False;
end;

function TcxWinXPLookAndFeelPainter.DefaultSchedulerBorderColor: TColor;
begin
  if not cxGetThemeColor(totComboBox, CP_DROPDOWNBUTTON, CBXS_NORMAL, TMT_BORDERCOLOR, Result) then
    Result := inherited DefaultSchedulerBorderColor;
end;

procedure TcxWinXPLookAndFeelPainter.DrawScaledArrow(ACanvas: TcxCanvas;
  const R: TRect; AState: TcxButtonState; AArrowDirection: TcxArrowDirection;
  AScaleFactor: TdxScaleFactor; ADrawBorder: Boolean = True);
var
  ATheme: TdxTheme;
const
  States: array[TcxArrowDirection, TcxButtonState] of Integer =
   ((ABS_UPNORMAL, ABS_UPNORMAL, ABS_UPHOT, ABS_UPPRESSED, ABS_UPDISABLED),
    (ABS_DOWNNORMAL, ABS_DOWNNORMAL, ABS_DOWNHOT, ABS_DOWNPRESSED, ABS_DOWNDISABLED),
    (ABS_LEFTNORMAL, ABS_LEFTNORMAL, ABS_LEFTHOT, ABS_LEFTPRESSED, ABS_LEFTDISABLED),
    (ABS_RIGHTNORMAL, ABS_RIGHTNORMAL, ABS_RIGHTHOT, ABS_RIGHTPRESSED, ABS_RIGHTDISABLED));

begin
  ATheme := OpenTheme(totScrollBar);
  if ATheme <> 0 then
    DrawThemeBackground(ATheme, ACanvas.Handle, SBP_ARROWBTN, States[AArrowDirection, AState], @R)
  else
    inherited
end;

procedure TcxWinXPLookAndFeelPainter.DrawScaledArrowBorder(ACanvas: TcxCanvas;
  const R: TRect; AState: TcxButtonState; AScaleFactor: TdxScaleFactor);
var
  ATheme: TdxTheme;
begin
  ATheme := OpenTheme(totButton);
  if ATheme <> 0 then
    DrawThemeBackground(ATheme, ACanvas.Handle, BP_PUSHBUTTON, BtnStateToXPBtnState(AState), @R)
  else
    inherited;
end;

function TcxWinXPLookAndFeelPainter.BorderSize: Integer;
var
  ATheme: TdxTheme;
begin
  ATheme := OpenTheme(totListView);
  if ATheme <> 0 then
    Result := 1 //!!! lcm corrected
  else
    Result := inherited BorderSize;
end;

function TcxWinXPLookAndFeelPainter.SeparatorSize: Integer;
begin
  Result := 6;
end;

procedure TcxWinXPLookAndFeelPainter.DrawBorder(ACanvas: TcxCanvas; R: TRect);
var
  ATheme: TdxTheme;
  ASavedDC: HDC;
begin
  ATheme := OpenTheme(totListView);
  if ATheme <> 0 then
  begin
    //DrawThemeEdge(ATheme, ACanvas.Handle, 0, 0, @R, EDGE_SUNKEN, BF_RECT, nil)
    ASavedDC := SaveDC(ACanvas.Handle);
    InflateRect(R, -1, -1);
    ACanvas.ExcludeClipRect(R);
    InflateRect(R, 1, 1);
    DrawThemeBackground(ATheme, ACanvas.Handle, LVP_EMPTYTEXT, 0, @R);
    RestoreDC(ACanvas.Handle, ASavedDC);
  end
  else
    inherited;
end;

procedure TcxWinXPLookAndFeelPainter.DoDrawSeparator(
  ACanvas: TcxCanvas; R: TRect; AIsVertical: Boolean);
const
  APartMap: array[Boolean] of Cardinal = (TP_SEPARATORVERT, TP_SEPARATOR);
var
  ATheme: TdxTheme;
begin
  ATheme := OpenTheme(totToolBar);
  if ATheme <> 0 then
    DrawThemeBackground(ATheme, ACanvas.Handle, APartMap[AIsVertical], TS_NORMAL, R)
  else
    inherited;
end;

function TcxWinXPLookAndFeelPainter.ButtonBorderSize(AState: TcxButtonState = cxbsNormal): Integer;
var
  ATheme: TdxTheme;
  R: TRect;
begin
  ATheme := OpenTheme(totButton);
  if ATheme <> 0 then
  begin
    R := Rect(0, 0, 100, 100);
    GetThemeBackgroundContentRect(ATheme, 0, BP_PUSHBUTTON, BtnStateToXPBtnState(AState), R, R);
    Result := R.Left;
  end
  else
    Result := inherited ButtonBorderSize;
end;

function TcxWinXPLookAndFeelPainter.ButtonColor(AState: TcxButtonState): TColor;
begin
  if not cxGetThemeColor(totButton, BP_PUSHBUTTON, BtnStateToXPBtnState(AState), TMT_COLOR, Result) then
    Result := inherited ButtonColor(AState);
end;

function TcxWinXPLookAndFeelPainter.ScaledButtonFocusRect(ACanvas: TcxCanvas; R: TRect; AScaleFactor: TdxScaleFactor): TRect;
var
  ATheme: TdxTheme;
begin
  ATheme := OpenTheme(totButton);
  if (ATheme <> 0) and (GetThemeBackgroundContentRect(ATheme, ACanvas.Canvas.Handle, BP_PUSHBUTTON, PBS_NORMAL, R, R) = S_OK) then
    Result := R
  else
    Result := inherited ScaledButtonFocusRect(ACanvas, R, AScaleFactor);
end;

function TcxWinXPLookAndFeelPainter.ButtonSymbolColor(
  AState: TcxButtonState; ADefaultColor: TColor = clDefault): TColor;
begin
  if not cxGetThemeColor(totButton, BP_PUSHBUTTON, BtnStateToXPBtnState(AState), TMT_TEXTCOLOR, Result) then
    Result := inherited ButtonSymbolColor(AState, ADefaultColor);
end;

function TcxWinXPLookAndFeelPainter.ScaledButtonTextOffset(AScaleFactor: TdxScaleFactor): Integer;
begin
  Result := AScaleFactor.Apply(cxTextOffset);
end;

function TcxWinXPLookAndFeelPainter.ScaledButtonTextShift(AScaleFactor: TdxScaleFactor): Integer;
begin
  Result := 0;
end;

procedure TcxWinXPLookAndFeelPainter.DrawScaledButton(ACanvas: TcxCanvas; R: TRect; const ACaption: string;
  AState: TcxButtonState; AScaleFactor: TdxScaleFactor; ADrawBorder: Boolean = True; AColor: TColor = clDefault;
  ATextColor: TColor = clDefault; AWordWrap: Boolean = False; AIsToolButton: Boolean = False; APart: TcxButtonPart = cxbpButton);
const
  ButtonObjectType: array[Boolean] of TdxThemedObjectType = (totButton, totToolBar);
var
  ATheme: TdxTheme;
begin
  ATheme := OpenTheme(ButtonObjectType[AIsToolButton]);
  if ATheme <> 0 then
  begin
    DrawThemeBackground(ATheme, ACanvas.Handle, BP_PUSHBUTTON,
      BtnStateToXPBtnState(AState), R);
    DrawThemeText(ATheme, ACanvas.Handle, BP_PUSHBUTTON, BtnStateToXPBtnState(AState),
      ACaption, -1, DT_CENTER or DT_VCENTER or DT_SINGLELINE, 0, R);
  end
  else
    inherited;
end;

function TcxWinXPLookAndFeelPainter.DefaultCommandLinkTextColor(
  AState: TcxButtonState; ADefaultColor: TColor): TColor;
var
  ATheme: TdxTheme;
  AColor: Cardinal;
begin
  ATheme := OpenTheme(totButton);
  if (ATheme <> 0) and IsWinVistaOrLater then
  begin
    GetThemeColor(ATheme, BP_COMMANDLINK, BtnStateToXPBtnState(AState), TMT_TEXTCOLOR, AColor);
    Result := TColor(AColor);
  end
  else
    Result := inherited DefaultCommandLinkTextColor(AState, ADefaultColor);
end;

procedure TcxWinXPLookAndFeelPainter.DrawScaledCommandLinkBackground(ACanvas: TcxCanvas; R: TRect;
  AState: TcxButtonState; AScaleFactor: TdxScaleFactor; AColor: TColor = clDefault);
var
  ATheme: TdxTheme;
begin
  ATheme := OpenTheme(totButton);
  if (ATheme <> 0) and IsWinVistaOrLater then
    DrawThemeBackground(ATheme, ACanvas.Handle, BP_COMMANDLINK, BtnStateToXPBtnState(AState), R, nil)
  else
    cxLookAndFeelPaintersManager.GetPainter(lfsFlat).DrawScaledButton(
      ACanvas, R, '', AState, AScaleFactor, (AState = cxbsHot) or (AState = cxbsPressed), AColor);
end;

procedure TcxWinXPLookAndFeelPainter.DrawScaledCommandLinkGlyph(ACanvas: TcxCanvas;
  const AGlyphPos: TPoint; AState: TcxButtonState; AScaleFactor: TdxScaleFactor);
var
  ARect: TRect;
  ATheme: TdxTheme;
begin
  ATheme := OpenTheme(totButton);
  if (ATheme <> 0) and IsWinVistaOrLater then
  begin
    ARect := cxRectBounds(AGlyphPos.X, AGlyphPos.Y, GetScaledCommandLinkGlyphSize(AScaleFactor));
    cxRightToLeftDependentDraw(ACanvas, ARect,
      procedure
      begin
        DrawThemeBackground(ATheme, ACanvas.Handle, BP_COMMANDLINKGLYPH, BtnStateToXPBtnState(AState), ARect);
      end);
  end
  else
    inherited DrawScaledCommandLinkGlyph(ACanvas, AGlyphPos, AState, AScaleFactor);
end;

function TcxWinXPLookAndFeelPainter.GetScaledCommandLinkGlyphSize(AScaleFactor: TdxScaleFactor): TSize;
var
  ATheme: TdxTheme;
begin
  ATheme := OpenTheme(totButton);
  if (ATheme <> 0) and IsWinVistaOrLater then
    GetThemePartSize(ATheme, 0, BP_COMMANDLINKGLYPH, 0, TS_TRUE, Result)
  else
    Result := inherited GetScaledCommandLinkGlyphSize(AScaleFactor);
end;

function TcxWinXPLookAndFeelPainter.GetScaledCommandLinkMargins(AScaleFactor: TdxScaleFactor): TRect;

  function ConverMarginsToRect(AMargins: TdxMargins): TRect;
  begin
    Result := Rect(AMargins.cxLeftWidth, AMargins.cyTopHeight, AMargins.cxRightWidth, AMargins.cyBottomHeight);
  end;

var
  ATheme: TdxTheme;
  AMargins: TdxMargins;
begin
  ATheme := OpenTheme(totButton);
  if (ATheme <> 0) and IsWinVistaOrLater then
  begin
    GetThemeMargins(ATheme, 0, BP_COMMANDLINK, 0, TMT_CONTENTMARGINS, nil, AMargins);
    Result := AScaleFactor.Apply(ConverMarginsToRect(AMargins));
  end
  else
    Result := inherited GetScaledCommandLinkMargins(AScaleFactor);
end;

procedure TcxWinXPLookAndFeelPainter.DrawScaledExpandButton(
  ACanvas: TcxCanvas; const R: TRect; AExpanded: Boolean;
  AScaleFactor: TdxScaleFactor; AColor: TColor = clDefault; AState: TcxExpandButtonState = cebsNormal);
const
  States: array[Boolean] of Integer = (GLPS_CLOSED, GLPS_OPENED);
var
  ATheme: TdxTheme;
begin
  ATheme := OpenTheme(totTreeview);
  if ATheme <> 0 then
  begin
    DrawScaledThemeBackground(ATheme, ACanvas.Handle, TVP_GLYPH, States[AExpanded], R, AScaleFactor);
    ACanvas.ExcludeClipRect(R);
  end
  else
    inherited;
end;

function TcxWinXPLookAndFeelPainter.DrawExpandButtonFirst: Boolean;
begin
  Result := False;
end;

procedure TcxWinXPLookAndFeelPainter.DrawScaledGroupExpandButton(ACanvas: TcxCanvas;
  const R: TRect; AExpanded: Boolean; AState: TcxButtonState; AScaleFactor: TdxScaleFactor);
const
  Buttons: array[Boolean] of Integer = (EBP_NORMALGROUPEXPAND, EBP_NORMALGROUPCOLLAPSE);
  States: array[cxbsNormal..cxbsPressed] of Integer = (EBNGE_NORMAL, EBNGE_HOT, EBNGE_PRESSED);
var
  ATheme: TdxTheme;
begin
  ATheme := OpenTheme(totExplorerBar);
  if ATheme <> 0 then
    DrawThemeBackground(ATheme, ACanvas.Handle, Buttons[AExpanded], States[AState], @R)
  else
    inherited;
end;

procedure TcxWinXPLookAndFeelPainter.DrawScaledSmallButton(
  ACanvas: TcxCanvas; R: TRect; AState: TcxButtonState; AScaleFactor: TdxScaleFactor);
begin
  DrawScaledButton(ACanvas, R, '', AState, AScaleFactor);
end;

procedure TcxWinXPLookAndFeelPainter.DrawScaledSmallExpandButton(ACanvas: TcxCanvas; R: TRect;
  AExpanded: Boolean; ABorderColor: TColor; AScaleFactor: TdxScaleFactor; AColor: TColor = clDefault);
begin
  if OpenTheme(totTreeview) <> 0 then
    DrawScaledExpandButton(ACanvas, R, AExpanded, AScaleFactor)
  else
    inherited DrawScaledSmallExpandButton(ACanvas, R, AExpanded, ABorderColor, AScaleFactor, AColor);
end;

function TcxWinXPLookAndFeelPainter.ScaledExpandButtonSize(AScaleFactor: TdxScaleFactor): Integer;
var
  ATheme: TdxTheme;
  ASize: TSize;
begin
  ATheme := OpenTheme(totTreeview);
  if ATheme <> 0 then
  begin
    GetThemePartSize(ATheme, 0, TVP_GLYPH, GLPS_OPENED, nil, TS_TRUE, @ASize);
    Result := AScaleFactor.Apply(ASize.cx, dxSystemScaleFactor);
  end
  else
    Result := inherited ScaledExpandButtonSize(AScaleFactor);
end;

function TcxWinXPLookAndFeelPainter.ScaledGroupExpandButtonSize(AScaleFactor: TdxScaleFactor): Integer;
var
  ATheme: TdxTheme;
  ASize: TSize;
begin
  ATheme := OpenTheme(totExplorerBar);
  if ATheme <> 0 then
  begin
    GetThemePartSize(ATheme, 0, EBP_NORMALGROUPEXPAND, EBNGE_NORMAL, nil, TS_TRUE, @ASize);
    Result := AScaleFactor.Apply(ASize.cx);
  end
  else
    Result := inherited ScaledGroupExpandButtonSize(AScaleFactor);
end;

function TcxWinXPLookAndFeelPainter.ScaledSmallExpandButtonSize(AScaleFactor: TdxScaleFactor): Integer;
begin
  if OpenTheme(totTreeview) <> 0 then
    Result := ScaledExpandButtonSize(AScaleFactor)
  else
    Result := inherited ScaledSmallExpandButtonSize(AScaleFactor);
end;

function TcxWinXPLookAndFeelPainter.IsButtonHotTrack: Boolean;
begin
  Result := True;
end;

function TcxWinXPLookAndFeelPainter.IsPointOverGroupExpandButton(const R: TRect;
  const P: TPoint): Boolean;
var
  ATheme: TdxTheme;
  ARegion: HRGN;
begin
  ATheme := OpenTheme(totExplorerBar);
  if ATheme <> 0 then
  begin
    GetThemeBackgroundRegion(ATheme, 0, EBP_NORMALGROUPEXPAND, EBNGE_HOT, @R, ARegion);
    Result := cxPtInRegion(ARegion, P);
    DeleteObject(ARegion);
  end
  else
    Result := inherited IsPointOverGroupExpandButton(R, P);
end;

function TcxWinXPLookAndFeelPainter.CheckBorderSize: Integer;
var
  ATheme: TdxTheme;
begin
  ATheme := OpenTheme(totButton);
  if ATheme <> 0 then
    Result := 0
  else
    Result := inherited CheckBorderSize;
end;

function TcxWinXPLookAndFeelPainter.ScaledCheckButtonSize(AScaleFactor: TdxScaleFactor): TSize;
var
  ATheme: TdxTheme;
begin
  ATheme := OpenTheme(totButton);
  if ATheme <> 0 then
  begin
    GetThemePartSize(ATheme, 0, BP_CHECKBOX, CBS_CHECKEDNORMAL, TS_TRUE, Result);
    Result := AScaleFactor.Apply(Result, dxSystemScaleFactor);
  end
  else
    Result := inherited ScaledCheckButtonSize(AScaleFactor);
end;

procedure TcxWinXPLookAndFeelPainter.DrawScaledCheck(ACanvas: TcxCanvas; const R: TRect;
  AState: TcxButtonState; ACheckState: TcxCheckBoxState; AColor: TColor; AScaleFactor: TdxScaleFactor);
const
  NativeCheckStateMap: array[TcxCheckBoxState, TcxButtonState] of Integer = (
    (CBS_UNCHECKEDNORMAL, CBS_UNCHECKEDNORMAL, CBS_UNCHECKEDHOT, CBS_UNCHECKEDPRESSED, CBS_UNCHECKEDDISABLED),
    (CBS_CHECKEDNORMAL, CBS_CHECKEDNORMAL, CBS_CHECKEDHOT, CBS_CHECKEDPRESSED, CBS_CHECKEDDISABLED),
    (CBS_MIXEDNORMAL, CBS_MIXEDNORMAL, CBS_MIXEDHOT, CBS_MIXEDPRESSED, CBS_MIXEDDISABLED)
  );
var
  ATheme: TdxTheme;
begin
  ATheme := OpenTheme(totButton);
  if ATheme <> 0 then
    DrawScaledThemeBackground(ATheme, ACanvas.Handle, BP_CHECKBOX, NativeCheckStateMap[ACheckState, AState], R, AScaleFactor)
  else
    inherited;
end;

procedure TcxWinXPLookAndFeelPainter.DrawCheckBorder(ACanvas: TcxCanvas; R: TRect;
  AState: TcxButtonState);
var
  ATheme: TdxTheme;
begin
  ATheme := OpenTheme(totButton);
  if ATheme = 0 then inherited;
end;

procedure TcxWinXPLookAndFeelPainter.DrawScaledRadioButton(ACanvas: TcxCanvas; X, Y: Integer; AButtonState: TcxButtonState;
  AChecked: Boolean; AFocused: Boolean; ABrushColor: TColor; AScaleFactor: TdxScaleFactor; AIsDesigning: Boolean = False);

  function GetNativeState: Integer;
  const
    ANativeStateMap: array [Boolean, TcxButtonState] of Integer = (
      (RBS_UNCHECKEDNORMAL, RBS_UNCHECKEDNORMAL, RBS_UNCHECKEDHOT,
      RBS_UNCHECKEDPRESSED, RBS_UNCHECKEDDISABLED),
      (RBS_CHECKEDNORMAL,RBS_CHECKEDNORMAL, RBS_CHECKEDHOT,
      RBS_CHECKEDPRESSED, RBS_CHECKEDDISABLED)
    );
  begin
    Result := ANativeStateMap[AChecked, AButtonState];
  end;

var
  ARadioButtonSize: TSize;
begin
  ARadioButtonSize := ScaledRadioButtonSize(AScaleFactor);
  DrawScaledThemeBackground(OpenTheme(totButton), ACanvas.Handle, BP_RADIOBUTTON,
    GetNativeState, Rect(X, Y, X + ARadioButtonSize.cx, Y + ARadioButtonSize.cy), AScaleFactor);
end;

function TcxWinXPLookAndFeelPainter.ScaledRadioButtonSize(AScaleFactor: TdxScaleFactor): TSize;
begin
  GetThemePartSize(OpenTheme(totButton), 0, BP_RADIOBUTTON, RBS_UNCHECKEDNORMAL,
    TS_TRUE, Result);
  Result := AScaleFactor.Apply(Result, dxSystemScaleFactor);
end;

procedure TcxWinXPLookAndFeelPainter.DrawScaledHeader(ACanvas: TcxCanvas; const ABounds, ATextAreaBounds: TRect;
  ANeighbors: TcxNeighbors; ABorders: TcxBorders; AState: TcxButtonState; AAlignmentHorz: TAlignment;
  AAlignmentVert: TcxAlignmentVert; AMultiLine, AShowEndEllipsis: Boolean; const AText: string; AFont: TFont;
  ATextColor, ABkColor: TColor; AScaleFactor: TdxScaleFactor; AOnDrawBackground: TcxDrawBackgroundEvent = nil;
  AIsLast: Boolean = False; AInGroupByBox: Boolean = False);
const
  States: array[TcxButtonState] of Integer = (HIS_NORMAL, HIS_NORMAL, HIS_HOT, HIS_PRESSED, HIS_NORMAL);

  function GetHeaderItem: Integer;
  begin
    if AIsLast and (States[AState] = HIS_NORMAL) and (GetOffice11Scheme in [schNormalColor, schHomestead]) then
      Result := HP_HEADERITEMRIGHT
    else
      Result := HP_HEADERITEM;
  end;

var
  ATheme: TdxTheme;
  R: TRect;
  AItem: Integer;
begin
  ATheme := OpenTheme(totHeader);
  if ATheme <> 0 then
  begin
    AItem := GetHeaderItem;
    if IsWin10OrLater then
      FillRectByColor(ACanvas.Handle, ABounds, clWindow);
    DrawThemeBackground(ATheme, ACanvas.Handle, AItem, States[AState], @ABounds);
    R := ATextAreaBounds;
    if AState = cxbsPressed then
      OffsetRect(R, 1, 1);
    DrawContent(ACanvas, ATheme, AItem, States[AState], ABounds, R,
      AAlignmentHorz, AAlignmentVert, AMultiLine, AShowEndEllipsis, False, AText,
      AFont, ATextColor, ABkColor);
  end
  else
    inherited;
end;

procedure TcxWinXPLookAndFeelPainter.DrawHeaderPressed(ACanvas: TcxCanvas;
  const ABounds: TRect);
begin
end;

procedure TcxWinXPLookAndFeelPainter.DrawScaledHeaderControlSection(
  ACanvas: TcxCanvas; const ABounds, ATextAreaBounds: TRect;
  ANeighbors: TcxNeighbors; ABorders: TcxBorders; AState: TcxButtonState;
  AAlignmentHorz: TAlignment; AAlignmentVert: TcxAlignmentVert; AMultiLine,
  AShowEndEllipsis: Boolean; const AText: string; AFont: TFont; ATextColor,
  ABkColor: TColor; AScaleFactor: TdxScaleFactor);
begin
  ACanvas.Brush.Color := clBtnFace;
  ACanvas.FillRect(ABounds);
  DrawScaledHeader(ACanvas, ABounds, ATextAreaBounds, [nRight], ABorders, AState, AAlignmentHorz,
    AAlignmentVert, AMultiLine, AShowEndEllipsis, AText, AFont, ATextColor, ABkColor, AScaleFactor);
end;

procedure TcxWinXPLookAndFeelPainter.DrawScaledSortingMark(
  ACanvas: TcxCanvas; const R: TRect; AAscendingSorting: Boolean; AScaleFactor: TdxScaleFactor);
begin
  DrawSortingArrow(ACanvas, R, clBtnShadow, clBtnShadow, AAscendingSorting, AScaleFactor);
end;

procedure TcxWinXPLookAndFeelPainter.DrawSpreadSheetScaledHeader(ACanvas: TcxCanvas; const ABounds, ATextAreaBounds: TRect;
  ANeighbors: TcxNeighbors; ABorders: TcxBorders; AState: TcxButtonState;
  AAlignmentHorz: TAlignment; AAlignmentVert: TcxAlignmentVert; AMultiLine, AShowEndEllipsis: Boolean;
  const AText: string; AFont: TFont; ATextColor, ABkColor: TColor; AScaleFactor: TdxScaleFactor;
  AOnDrawBackground: TcxDrawBackgroundEvent = nil; AIsLast: Boolean = False;
  AIsGroup: Boolean = False);
begin
  DrawScaledHeader(ACanvas, ABounds, ATextAreaBounds, ANeighbors, ABorders, AState, AAlignmentHorz,
    AAlignmentVert, AMultiLine, AShowEndEllipsis, AText, AFont, ATextColor, ABkColor,
    AScaleFactor, AOnDrawBackground, AIsLast, AIsGroup);
end;

procedure TcxWinXPLookAndFeelPainter.DrawScaledSummarySortingMark(ACanvas: TcxCanvas;
  const R: TRect; AAscendingSorting: Boolean; AScaleFactor: TdxScaleFactor);
begin
  DrawSummarySortingArrow(ACanvas, R, clBtnShadow, clBtnShadow, AAscendingSorting, AScaleFactor);
end;

function TcxWinXPLookAndFeelPainter.HeaderBorderSize: Integer;
var
  ATheme: TdxTheme;
  R: TRect;
begin
  ATheme := OpenTheme(totHeader);
  if ATheme <> 0 then
  begin
    R := Rect(0, 0, 100, 100);
    GetThemeBackgroundContentRect(ATheme, 0, HP_HEADERITEM, HIS_NORMAL, R, R);
    Result := Max((R.Left + R.Top) div 2, 1);
  end
  else
    Result := inherited HeaderBorderSize;
end;

function TcxWinXPLookAndFeelPainter.HeaderControlSectionBorderSize(AState: TcxButtonState = cxbsNormal): Integer;
begin
  Result := HeaderBorderSize;
end;

function TcxWinXPLookAndFeelPainter.HeaderControlSectionContentBounds(const ABounds: TRect; AState: TcxButtonState): TRect;
var
  ATheme: TdxTheme;
begin
  ATheme := OpenTheme(totHeader);
  if ATheme <> 0 then
    Result := cxRectInflate(ABounds, -HeaderBorderSize)
  else
    Result := inherited HeaderControlSectionContentBounds(ABounds, AState);
end;

function TcxWinXPLookAndFeelPainter.IsHeaderHotTrack: Boolean;
begin
  Result := True;
end;

function TcxWinXPLookAndFeelPainter.ScaledSortingMarkSize(AScaleFactor: TdxScaleFactor): TPoint;
begin
  Result := AScaleFactor.Apply(Point(7, 8));
end;

procedure TcxWinXPLookAndFeelPainter.DrawFilterRowSeparator(ACanvas: TcxCanvas; const ARect: TRect; ABackgroundColor: TColor);
begin
  inherited DrawFilterRowSeparator(ACanvas, ARect, ABackgroundColor);
  if IsWin10OrLater then
    ACanvas.FrameRect(ARect, DefaultGridLineColor, 1, [bTop, bRight, bBottom]);
end;

procedure TcxWinXPLookAndFeelPainter.DrawFooterBorder(ACanvas: TcxCanvas;
  const R: TRect);
var
  ATheme: TdxTheme;
begin
  ATheme := OpenTheme(totEdit);
  if ATheme <> 0 then
    DrawThemeEdge(ATheme, ACanvas.Handle, 0, 0, @R, BDR_RAISEDINNER, BF_RECT, nil)
  else
    inherited;
end;

procedure TcxWinXPLookAndFeelPainter.DrawFooterCell(ACanvas: TcxCanvas;
  const ABounds: TRect; AAlignmentHorz: TAlignment; AAlignmentVert: TcxAlignmentVert;
  AMultiLine: Boolean; const AText: string; AFont: TFont; ATextColor, ABkColor: TColor;
  AOnDrawBackground: TcxDrawBackgroundEvent = nil);
var
  ATheme: TdxTheme;
begin
  ATheme := OpenTheme(totTrackBar);
  if ATheme <> 0 then
  begin
    //DrawThemeParentBackground(0, ACanvas.Handle, @ABounds);
    DrawThemeBackground(ATheme, ACanvas.Handle, TKP_TRACK, TRS_NORMAL, @ABounds);
    DrawContent(ACanvas, ATheme, TKP_TRACK, TRS_NORMAL, ABounds, FooterCellTextAreaBounds(ABounds),
      AAlignmentHorz, AAlignmentVert, AMultiLine, False, False, AText, AFont,
      ATextColor, ABkColor);
  end
  else
    inherited;
end;

function TcxWinXPLookAndFeelPainter.FooterBorderSize: Integer;
var
  ATheme: TdxTheme;
begin
  ATheme := OpenTheme(totEdit);
  if ATheme <> 0 then
    Result := 1 //!!!
  else
    Result := inherited FooterBorderSize;
end;

function TcxWinXPLookAndFeelPainter.FooterCellBorderSize: Integer;
var
  ATheme: TdxTheme;
  R: TRect;
begin
  ATheme := OpenTheme(totTrackBar);
  if ATheme <> 0 then
  begin
    R := Rect(0, 0, 100, 100);
    GetThemeBackgroundContentRect(ATheme, 0, TKP_TRACK, TRS_NORMAL, R, R);
    Result := R.Left;
  end
  else
    Result := inherited FooterCellBorderSize;
end;

function TcxWinXPLookAndFeelPainter.FooterCellOffset: Integer;
var
  ATheme: TdxTheme;
begin
  ATheme := OpenTheme(totTrackBar);
  if ATheme <> 0 then
    Result := 1
  else
    Result := inherited FooterCellOffset;
end;

function TcxWinXPLookAndFeelPainter.FooterDrawCellsFirst: Boolean;
begin
  Result := False;
end;

procedure TcxWinXPLookAndFeelPainter.DrawScaledFilterCloseButton(
  ACanvas: TcxCanvas; R: TRect; AState: TcxButtonState; AScaleFactor: TdxScaleFactor);
const
  States: array[TcxButtonState] of Integer = (CBS_NORMAL, CBS_NORMAL, CBS_HOT, CBS_PUSHED, CBS_DISABLED);
var
  ATheme: TdxTheme;
begin
  ATheme := OpenTheme(totWindow);
  if ATheme <> 0 then
  begin
    DrawScaledThemeBackground(ATheme, ACanvas.Handle, WP_SMALLCLOSEBUTTON,
      States[AState], R, AScaleFactor, not AScaleFactor.Equals(dxDefaultScaleFactor));
  end
  else
    inherited;
end;

procedure TcxWinXPLookAndFeelPainter.DrawScaledFilterDropDownButton(
  ACanvas: TcxCanvas; R: TRect; AState: TcxButtonState; AIsFilterActive: Boolean; AScaleFactor: TdxScaleFactor);
const
  States: array[TcxButtonState] of Integer =(CBXS_NORMAL, CBXS_NORMAL, CBXS_HOT, CBXS_PRESSED, CBXS_DISABLED);
var
  ATheme: TdxTheme;
begin
  ATheme := OpenTheme(totComboBox);
  if ATheme <> 0 then
    DrawThemeBackground(ATheme, ACanvas.Handle, CP_DROPDOWNBUTTON, States[AState], @R)
  else
    inherited;
end;

function TcxWinXPLookAndFeelPainter.ScaledFilterActivateButtonSize(AScaleFactor: TdxScaleFactor): TPoint;
var
  ATheme: TdxTheme;
  ASize: TSize;
begin
  ATheme := OpenTheme(totButton);
  if ATheme <> 0 then
  begin
    GetThemePartSize(ATheme, 0, BP_CHECKBOX, CBS_CHECKEDNORMAL, nil, TS_TRUE, @ASize);
    ASize := AScaleFactor.Apply(ASize);
    Result.X := ASize.cx;
    Result.Y := ASize.cy;
  end
  else
    Result := inherited ScaledFilterActivateButtonSize(AScaleFactor);
end;

function TcxWinXPLookAndFeelPainter.ScaledFilterCloseButtonSize(AScaleFactor: TdxScaleFactor): TPoint;
var
  ATheme: TdxTheme;
  ASize: TSize;
  ADelta: Integer;
begin
  ATheme := OpenTheme(totWindow);
  if ATheme <> 0 then
  begin
    ASize := ScaledSmallCloseButtonSize(AScaleFactor);
    ADelta := AScaleFactor.Apply(4);
    Result := Point(ASize.cx + ADelta, ASize.cy + ADelta);
  end
  else
    Result := inherited ScaledFilterCloseButtonSize(AScaleFactor)
end;

function TcxWinXPLookAndFeelPainter.DefaultLayoutViewContentTextColor(AState: TcxButtonState): TColor;
var
  ATheme: TdxTheme;
begin
  ATheme := OpenTheme(totTab);
  if ATheme <> 0 then
    Result := clMenuText
  else
    Result := inherited DefaultLayoutViewContentTextColor(AState);
end;

function TcxWinXPLookAndFeelPainter.DefaultLayoutViewCaptionTextColor(
  ACaptionPosition: TcxGroupBoxCaptionPosition; AState: TcxButtonState): TColor;
var
  ATheme: TdxTheme;
begin
  ATheme := OpenTheme(totTab);
  if ATheme <> 0 then
    Result := DefaultTabTextColor
  else
    Result := inherited DefaultLayoutViewCaptionTextColor(ACaptionPosition, AState);
end;

procedure TcxWinXPLookAndFeelPainter.LayoutViewDrawRecordCaption(ACanvas: TcxCanvas; const ABounds, ATextRect: TRect;
  APosition: TcxGroupBoxCaptionPosition; AState: TcxButtonState; AColor: TColor = clDefault; const ABitmap: TBitmap = nil);
const
  States: array[TcxButtonState] of Integer =
    (TIS_NORMAL, TIS_NORMAL, TIS_HOT, TIS_SELECTED, TIS_FOCUSED);
var
  ATheme: TdxTheme;
begin
  LayoutViewDrawRecordBorder(ACanvas, ABounds, AState, cxBordersAll);
  ATheme := OpenTheme(totTab);
  if ATheme <> 0 then
    DrawThemedTab(ACanvas, ABounds, [], '', ATheme, States[AState], APosition <> cxgpTop,
      nil, clDefault, clDefault)
  else
    inherited DrawTab(ACanvas, ABounds, [], '', AState, APosition <> cxgpTop, nil, clDefault, clDefault);
end;

procedure TcxWinXPLookAndFeelPainter.LayoutViewDrawItem(ACanvas: TcxCanvas; const ABounds: TRect;
  AState: TcxButtonState; ABorders: TcxBorders = []);
var
  ATheme: TdxTheme;
  AThemeState: Integer;
begin
  ATheme := OpenTheme(totMenu);
  if (ATheme <> 0) and (AState in [cxbsHot, cxbsPressed, cxbsDisabled]) then
  begin
    if AState <> cxbsDisabled then
      AThemeState := 2
    else
      AThemeState := 4;
    DrawThemeBackground(ATheme, ACanvas.Handle, MENU_POPUPITEM, AThemeState, ABounds, nil);
  end
  else
    inherited;
end;

procedure TcxWinXPLookAndFeelPainter.WinExplorerViewDrawRecord(ACanvas: TcxCanvas; const ABounds: TRect; AState: TcxButtonState;
  AColor: TColor = clDefault; const ABitmap: TBitmap = nil);
var
  ATheme: TdxTheme;
  AThemeState: Integer;
begin
  ATheme := OpenTheme(totMenu);
  if (ATheme <> 0) and (AState in [cxbsHot, cxbsPressed, cxbsDisabled]) then
  begin
    if AState <> cxbsDisabled then
      AThemeState := 2
    else
      AThemeState := 4;
    DrawThemeBackground(ATheme, ACanvas.Handle, MENU_POPUPITEM, AThemeState, ABounds, nil);
  end
  else
    inherited WinExplorerViewDrawRecord(ACanvas, ABounds, AState);
end;

function TcxWinXPLookAndFeelPainter.WinExplorerViewRecordTextColor(AState: TcxButtonState): TColor;
var
  ATheme: TdxTheme;
begin
  ATheme := OpenTheme(totTab);
  if ATheme <> 0 then
    Result := DefaultTabTextColor
  else
    Result := inherited WinExplorerViewRecordTextColor(AState);
end;

procedure TcxWinXPLookAndFeelPainter.DrawThemedTab(ACanvas: TcxCanvas; R: TRect;
  ABorders: TcxBorders; const AText: string; ATheme: TdxTheme; AState: Integer;
  AVertical: Boolean; AFont: TFont; ATextColor, ABkColor: TColor; AShowPrefix: Boolean = False);
var
  AOriginalR, ADrawR: TRect;
  B: TBitmap;
  DC: HDC;

  procedure RotateRect(var R: TRect);
  var
    I: Integer;
  begin
    with R do
    begin
      I := Right;
      Right := Bottom;
      Bottom := I;
    end;
  end;

  procedure PrepareRects;
  begin
    ADrawR := R;
    if AVertical then
    begin
      if not (bTop in ABorders) then Dec(ADrawR.Left, 2);
      if not (bBottom in ABorders) then Inc(ADrawR.Right, 2);
    end
    else
    begin
      if not (bLeft in ABorders) then Dec(ADrawR.Left, 2);
      if not (bRight in ABorders) then Inc(ADrawR.Right, 2);
    end;
    if AState = TIS_SELECTED then
      Inc(ADrawR.Bottom)
    else
      Dec(R.Bottom);
  end;

begin
  if AVertical then
  begin
    AOriginalR := R;
    OffsetRect(R, -R.Left, -R.Top);
    RotateRect(R);
    B := TBitmap.Create;
    B.PixelFormat := pf32bit;
    B.HandleType := bmDDB;
    B.Width := R.Right;
    B.Height := R.Bottom;
    DC := B.Canvas.Handle;
  end
  else
  begin
    B := nil;
    DC := ACanvas.Handle;
  end;

  PrepareRects;
  DrawThemeBackground(ATheme, DC, TABP_TABITEM, AState, @ADrawR, @R);

  if AVertical then
  begin
    ACanvas.RotateBitmap(B, raPlus90, True);
    RotateRect(R);
    RotateRect(ADrawR);
    ACanvas.CopyRect(Bounds(AOriginalR.Left, AOriginalR.Top, R.Right, R.Bottom),
      B.Canvas, R);
    B.Free;
    OffsetRect(R, AOriginalR.Left, AOriginalR.Top);
    OffsetRect(ADrawR, AOriginalR.Left, AOriginalR.Top);
  end;

  if AState = TIS_SELECTED then
    if AVertical then
      Dec(ADrawR.Right, 2)
    else
      Dec(ADrawR.Bottom, 2);
  DrawContent(ACanvas, ATheme, TABP_TABITEM, AState, R, ADrawR,
    taCenter, vaCenter, False, False, AShowPrefix, AText, AFont, ATextColor, ABkColor);
end;

procedure TcxWinXPLookAndFeelPainter.DrawTab(ACanvas: TcxCanvas; R: TRect;
  ABorders: TcxBorders; const AText: string; AState: TcxButtonState; AVertical: Boolean;
  AFont: TFont; ATextColor, ABkColor: TColor; AShowPrefix: Boolean = False);
const
  States: array[TcxButtonState] of Integer =
    (TIS_NORMAL, TIS_NORMAL, TIS_HOT, TIS_SELECTED, TIS_DISABLED);
var
  ATheme: TdxTheme;
begin
  ATheme := OpenTheme(totTab);
  if ATheme <> 0 then
    DrawThemedTab(ACanvas, R, ABorders, AText, ATheme, States[AState], AVertical,
      AFont, ATextColor, ABkColor, AShowPrefix)
  else
    inherited;
end;

procedure TcxWinXPLookAndFeelPainter.DrawTabBorder(ACanvas: TcxCanvas; R: TRect;
  ABorder: TcxBorder; ABorders: TcxBorders; AVertical: Boolean);
var
  ATheme: TdxTheme;
begin
  ATheme := OpenTheme(totTab);
  if ATheme <> 0 then
  begin
    //
  end
  else
    inherited;
end;

procedure TcxWinXPLookAndFeelPainter.DrawTabsRoot(ACanvas: TcxCanvas;
  const R: TRect; ABorders: TcxBorders; AVertical: Boolean);
var
  ATheme: TdxTheme;
  AContentR, AFullR: TRect;
begin
  ATheme := OpenTheme(totTab);
  if ATheme <> 0 then
  begin
    //DrawThemeParentBackground(0, ACanvas.Handle, @R);
    GetThemeBackgroundContentRect(ATheme, 0, TABP_PANE, 0, R, AContentR);
    AFullR := R;
    if AVertical then
    begin
      Inc(AFullR.Bottom, (R.Bottom - AContentR.Bottom) - (AContentR.Top - R.Top));
      Inc(AFullR.Right, 10);
    end
    else
    begin
      Inc(AFullR.Right, (R.Right - AContentR.Right) - (AContentR.Left - R.Left));
      Inc(AFullR.Bottom, 10);
    end;
    DrawThemeBackground(ATheme, ACanvas.Handle, TABP_PANE, 0, @AFullR, @R);
  end
  else
    inherited;
end;

function TcxWinXPLookAndFeelPainter.IsDrawTabImplemented(AVertical: Boolean): Boolean;
var
  ATheme: TdxTheme;
begin
  ATheme := OpenTheme(totTab);
  if ATheme <> 0 then
    Result := True
  else
    Result := inherited IsDrawTabImplemented(AVertical);
end;

function TcxWinXPLookAndFeelPainter.IsTabHotTrack(AVertical: Boolean): Boolean;
var
  ATheme: TdxTheme;
begin
  ATheme := OpenTheme(totTab);
  if ATheme <> 0 then
    Result := True
  else
    Result := inherited IsTabHotTrack(AVertical);
end;

function TcxWinXPLookAndFeelPainter.TabBorderSize(AVertical: Boolean): Integer;
var
  ATheme: TdxTheme;
  //R: TRect;
begin
  ATheme := OpenTheme(totTab);
  if ATheme <> 0 then
  begin
    {R := Rect(0, 0, 100, 100);
    GetThemeBackgroundContentRect(ATheme, 0, TABP_TABITEM, TIS_NORMAL, @R, R);}
    Result := 1;//R.Left;
  end
  else
    Result := inherited TabBorderSize(AVertical);
end;

procedure TcxWinXPLookAndFeelPainter.DrawScaledSplitter(ACanvas: TcxCanvas; const ARect: TRect;
  AHighlighted, AClicked, AHorizontal: Boolean; AScaleFactor: TdxScaleFactor; AHasCloseMark: Boolean = False; AArrowDirection: TcxArrowDirection = adLeft);
begin
  if AHasCloseMark then
    DrawSplitterCloseMark(ACanvas, ARect, AHighlighted, AClicked, AHorizontal, AScaleFactor, AArrowDirection);
end;

procedure TcxWinXPLookAndFeelPainter.DrawScaledIndicatorItem(
  ACanvas: TcxCanvas; const R, AImageAreaBounds: TRect; AKind: TcxIndicatorKind; AColor: TColor;
  AScaleFactor: TdxScaleFactor; AOnDrawBackground: TcxDrawBackgroundEvent = nil; ANeighbors: TcxNeighbors = [nTop, nBottom]);
var
  APainter: TcxCustomLookAndFeelPainter;
begin
  if cxLookAndFeelPaintersManager.GetPainter(lfsFlat, APainter) then
    APainter.DrawScaledIndicatorItem(ACanvas, R, AImageAreaBounds, AKind, AColor, AScaleFactor, AOnDrawBackground, ANeighbors)
  else
    inherited DrawScaledIndicatorItem(ACanvas, R, AImageAreaBounds, AKind, AColor, AScaleFactor, AOnDrawBackground, ANeighbors);
end;

function TcxWinXPLookAndFeelPainter.ScaledScrollBarMinimalThumbSize(AVertical: Boolean;
  AScaleFactor: TdxScaleFactor): Integer;
const
  ThumbnailKind: array[Boolean] of Integer = (SBP_THUMBBTNHORZ, SBP_THUMBBTNVERT);

  function GetThumbSize(ATheme: TdxTheme; out ASize: TSize): Boolean;
  begin
    Result := (ATheme <> 0) and (GetThemePartSize(ATheme, 0,
      ThumbnailKind[AVertical], SCRBS_NORMAL, TS_MIN, ASize) = S_OK);
  end;

var
  ATheme: TdxTheme;
  AThumbSize: TSize;
  AMargins: TdxMargins;
begin
  ATheme := OpenTheme(totScrollBar);
  if GetThumbSize(ATheme, AThumbSize) then
  begin
    if IsWinVistaOrLater then
      GetThemeMargins(ATheme, 0, ThumbnailKind[AVertical], SCRBS_NORMAL, TMT_CONTENTMARGINS, nil, AMargins);
    if AVertical then
    begin
      Result := AThumbSize.cy;
      if IsWinVistaOrLater then
        Inc(Result, AMargins.cyTopHeight + AMargins.cyBottomHeight);
    end
    else
    begin
      Result := AThumbSize.cx;
      if IsWinVistaOrLater then
        Inc(Result, AMargins.cxLeftWidth + AMargins.cxRightWidth);
    end;
    AScaleFactor.Apply(Result);
  end
  else
    Result := inherited ScaledScrollBarMinimalThumbSize(AVertical, AScaleFactor);
end;

procedure TcxWinXPLookAndFeelPainter.DrawScaledScrollBarSplitter(
  ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState; AScaleFactor: TdxScaleFactor);
begin
  inherited DrawScaledSplitter(ACanvas, cxTextRect(R), AState = cxbsHot, AState = cxbsPressed, False, AScaleFactor);
end;

function TcxWinXPLookAndFeelPainter.DefaultSchedulerViewContentColor: TColor;
begin
  Result := $FFFFFF;
end;

function TcxWinXPLookAndFeelPainter.DefaultSchedulerViewContentColorClassic: TColor;
begin
  Result := $D5FFFF;
end;

procedure TcxWinXPLookAndFeelPainter.DrawScaledMonthHeader(ACanvas: TcxCanvas; const ABounds: TRect; const AText: string;
  ANeighbors: TcxNeighbors; const AViewParams: TcxViewParams; AArrows: TcxArrowDirections; ASideWidth: Integer;
  AScaleFactor: TdxScaleFactor; AOnDrawBackground: TcxDrawBackgroundEvent = nil);
begin
  DrawScaledHeader(ACanvas, ABounds, ABounds, ANeighbors, cxBordersAll, cxbsNormal, taCenter,
    vaCenter, False, False, AText, AViewParams.Font, AViewParams.TextColor,
    AViewParams.Color, AScaleFactor, AOnDrawBackground, not (nRight in ANeighbors));
  DrawMonthHeaderArrows(ACanvas, ABounds, AArrows, ASideWidth, clWindowText);
end;

procedure TcxWinXPLookAndFeelPainter.DrawSchedulerEventProgress(
  ACanvas: TcxCanvas; const ABounds, AProgressChunk: TRect;
  AViewParams: TcxViewParams; ATransparent: Boolean);
var
  ATheme: TdxTheme;
begin
  ATheme := OpenTheme(totProgress);
  if ATheme = 0 then
    inherited DrawSchedulerEventProgress(ACanvas, ABounds, AProgressChunk,
      AViewParams, ATransparent)
  else
  begin
    DrawThemeBackground(ATheme, ACanvas.Handle, PP_BAR, 0, ABounds);
    DrawThemeBackground(ATheme, ACanvas.Handle, PP_CHUNK, 0, AProgressChunk);
  end;
end;

function TcxWinXPLookAndFeelPainter.SchedulerEventProgressOffsets: TRect;
begin
  Result := Rect(3, 3, 3, 3);
end;

procedure TcxWinXPLookAndFeelPainter.DrawSchedulerScaledNavigatorButton(
  ACanvas: TcxCanvas; R: TRect; AState: TcxButtonState; AScaleFactor: TdxScaleFactor);
const
  ButtonStates: array[TcxButtonState] of Integer =
    (TS_HOT, TS_HOT, TS_CHECKED, TS_PRESSED, TS_DISABLED);
var
  ATheme: TdxTheme;
begin
  ATheme := OpenTheme(totToolBar);
  if ATheme <> 0 then
    DrawThemeBackground(ATheme, ACanvas.Handle, TP_BUTTON, ButtonStates[AState], @R)
  else
    inherited;
end;

procedure TcxWinXPLookAndFeelPainter.DrawSchedulerSplitterBorder(
  ACanvas: TcxCanvas; R: TRect; const AViewParams: TcxViewParams;
  AIsHorizontal: Boolean);
var
  AColor: TColor;
begin
  if (OpenTheme(totListView) <> 0) and
    cxGetThemeColor(totComboBox, CP_DROPDOWNBUTTON, CBXS_NORMAL, TMT_BORDERCOLOR, AColor) then
  begin
    ACanvas.SetBrushColor(AColor);
    if AIsHorizontal then
    begin
      ACanvas.FillRect(Rect(R.Left, R.Top, R.Right, R.Top + 1));
      ACanvas.FillRect(Rect(R.Left, R.Bottom - 1, R.Right, R.Bottom));
      InflateRect(R, 1, -1);
    end
    else
    begin
      ACanvas.FillRect(Rect(R.Left, R.Top, R.Left + 1, R.Bottom));
      ACanvas.FillRect(Rect(R.Right - 1, R.Top, R.Right, R.Bottom));
      InflateRect(R, -1, 1);
    end;
    ACanvas.FillRect(R, AViewParams);
  end
  else
    inherited DrawSchedulerSplitterBorder(ACanvas, R, AViewParams, AIsHorizontal);
end;

function TcxWinXPLookAndFeelPainter.ScaledSizeGripSize(AScaleFactor: TdxScaleFactor): TSize;
var
  ATheme: Cardinal;
begin
  ATheme := OpenTheme(totScrollBar);
  GetThemePartSize(ATheme, 0, SBP_SIZEBOX, SZB_RIGHTALIGN, TS_TRUE, Result);
  if IsWin8OrLater then
    Result := AScaleFactor.Apply(Result, dxSystemScaleFactor)
  else
    Result := AScaleFactor.Apply(Result);
end;

procedure TcxWinXPLookAndFeelPainter.DoDrawSizeGrip(ACanvas: TcxCanvas; const ARect: TRect; AScaleFactor: TdxScaleFactor);
var
  ATheme: Cardinal;
begin
  ATheme := OpenTheme(totScrollBar);
  if ATheme <> 0 then
    DrawThemeBackground(ATheme, ACanvas.Handle, SBP_SIZEBOX, SZB_RIGHTALIGN, ARect)
  else
    inherited;
end;

function TcxWinXPLookAndFeelPainter.DoGetSmallCloseButtonSize: TSize;
var
  ATheme: TdxTheme;
begin
  ATheme := OpenTheme(totWindow);
  if ATheme <> 0 then
  begin
    GetThemePartSize(ATheme, 0, WP_SMALLCLOSEBUTTON, CBXS_NORMAL, TS_TRUE, Result);
    if IsWin8OrLater and ((Result.cx < 10) or (Result.cy < 10)) then
      Result := Size(13, 13);
  end
  else
    Result := inherited DoGetSmallCloseButtonSize;
end;

procedure TcxWinXPLookAndFeelPainter.DrawGroupBoxFrame(ACanvas: TcxCanvas;
  R: TRect; AEnabled: Boolean; ACaptionPosition: TcxGroupBoxCaptionPosition;
  ABorders: TcxBorders = cxBordersAll);
const
  StateMap: array[Boolean] of Integer = (GBS_DISABLED, GBS_NORMAL);
var
  ATheme: TdxTheme;
begin
  ATheme := OpenTheme(totButton);
  if ATheme <> 0 then
    DrawThemeBackground(ATheme, ACanvas.Handle, BP_GROUPBOX, StateMap[AEnabled], R)
  else
    inherited DrawGroupBoxFrame(ACanvas, R, AEnabled, ACaptionPosition, ABorders);
end;

function TcxWinXPLookAndFeelPainter.GroupBoxTextColor(AEnabled: Boolean; ACaptionPosition: TcxGroupBoxCaptionPosition): TColor;
const
  GroupBoxStateMap: array[Boolean] of Integer = (GBS_DISABLED, GBS_NORMAL);
begin
  if not cxGetThemeColor(totButton, BP_GROUPBOX, GroupBoxStateMap[AEnabled], TMT_TEXTCOLOR, Result) then
    Result := inherited GroupBoxTextColor(AEnabled, ACaptionPosition);
end;

procedure TcxWinXPLookAndFeelPainter.DrawPanelBackground(ACanvas: TcxCanvas;
  AControl: TWinControl; ABounds: TRect; AColorFrom, AColorTo: TColor);
begin
  if AColorFrom = clDefault then
    cxDrawTransparentControlBackground(AControl, ACanvas, ABounds)
  else
    ACanvas.FillRect(ABounds, AColorFrom);
end;

procedure TcxWinXPLookAndFeelPainter.DrawEditPopupWindowBorder(ACanvas: TcxCanvas; var R: TRect;
  ABorderStyle: TcxEditPopupBorderStyle; AClientEdge: Boolean);
begin
  if IsWinVistaOrLater then
    DrawThemeBackground(OpenTheme(totListBox), ACanvas.Handle, LBCP_BORDER_NOSCROLL, LBPSN_HOT, R)
  else
    ACanvas.FrameRect(R, clBtnText);
  InflateRect(R, -1, -1);
end;

function TcxWinXPLookAndFeelPainter.GetEditPopupWindowBorderWidth(AStyle: TcxEditPopupBorderStyle): Integer;
begin
  Result := 1;
end;

function TcxWinXPLookAndFeelPainter.GetEditPopupWindowClientEdgeWidth(AStyle: TcxEditPopupBorderStyle): Integer;
begin
  Result := 2;
end;

function TcxWinXPLookAndFeelPainter.GetHintBorderColor: TColor;
begin
  if OpenTheme(totToolTip) <> 0 then
    Result := clWindowFrame
  else
    Result := inherited GetHintBorderColor;
end;

function TcxWinXPLookAndFeelPainter.GetScaledZoomInButtonSize(AScaleFactor: TdxScaleFactor): TSize;
begin
  if not NativeZoomButtonGetMinSize(ZoomInButtonGlyph, AScaleFactor, Result) then
    Result := inherited GetScaledZoomInButtonSize(AScaleFactor);
end;

function TcxWinXPLookAndFeelPainter.GetScaledZoomOutButtonSize(AScaleFactor: TdxScaleFactor): TSize;
begin
  if not NativeZoomButtonGetMinSize(ZoomOutButtonGlyph, AScaleFactor, Result) then
    Result := inherited GetScaledZoomOutButtonSize(AScaleFactor);
end;

procedure TcxWinXPLookAndFeelPainter.DrawScaledZoomInButton(
  ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState; AScaleFactor: TdxScaleFactor);
begin
  if not NativeZoomButtonDraw(ACanvas, R, AState, AScaleFactor, ZoomInButtonGlyph) then
    inherited DrawScaledZoomInButton(ACanvas, R, AState, AScaleFactor);
end;

procedure TcxWinXPLookAndFeelPainter.DrawScaledZoomOutButton(
  ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState; AScaleFactor: TdxScaleFactor);
begin
  if not NativeZoomButtonDraw(ACanvas, R, AState, AScaleFactor, ZoomOutButtonGlyph) then
    inherited DrawScaledZoomOutButton(ACanvas, R, AState, AScaleFactor);
end;

procedure TcxWinXPLookAndFeelPainter.DrawHintBackground(ACanvas: TcxCanvas; const ARect: TRect; AColor: TColor);
var
  AClientRect: TRect;
  ATheme: TdxTheme;
begin
  ATheme := OpenTheme(totToolTip);
  if ATheme <> 0 then
  begin
    AClientRect := cxRectInflate(ARect, 4, 4);
    DrawThemeBackground(ATheme, ACanvas.Handle, TTP_STANDARD, 1, AClientRect);
  end
  else
    inherited DrawHintBackground(ACanvas, ARect, AColor);
end;

function TcxWinXPLookAndFeelPainter.ScreenTipGetDescriptionTextColor: TColor;
var
  ATheme: TdxTheme;
  AColorRef: COLORREF;
begin
  ATheme := OpenTheme(totToolTip);
  if (ATheme <> 0) and
    not Failed(GetThemeColor(ATheme, TTP_STANDARD, 1, TMT_TEXTCOLOR, AColorRef)) then
      Result := AColorRef
  else
    Result := inherited ScreenTipGetDescriptionTextColor;
end;

function TcxWinXPLookAndFeelPainter.ScreenTipGetTitleTextColor: TColor;
begin
  Result := ScreenTipGetDescriptionTextColor;
end;

procedure TcxWinXPLookAndFeelPainter.CorrectThumbRect(ACanvas: TcxCanvas;
  var ARect: TRect; AHorizontal: Boolean; ATicks: TcxTrackBarTicksAlign);
var
  ATheme: TdxTheme;
  AThumbSize: TSize;
begin
  ATheme := OpenTheme(totTrackBar);
  if ATheme <> 0 then
  begin
    GetThemePartSize(ATheme, ACanvas.Handle,
      TrackBarThumbThemeParts[ATicks, AHorizontal], TUS_NORMAL, ARect, TS_DRAW, AThumbSize);
    ARect.Left := ARect.Left + (cxRectWidth(ARect) - AThumbSize.cx) div 2;
    ARect.Top := ARect.Top + (cxRectHeight(ARect) - AThumbSize.cy) div 2;
    ARect.Right := ARect.Left + AThumbSize.cx;
    ARect.Bottom := ARect.Top + AThumbSize.cy;
  end
  else
    inherited;
end;

procedure TcxWinXPLookAndFeelPainter.DrawTrackBarScaledTrack(ACanvas: TcxCanvas; const ARect, ASelection: TRect;
  AShowSelection, AEnabled, AHorizontal: Boolean; ATrackColor: TColor; AScaleFactor: TdxScaleFactor);
const
  ATrackType: array[Boolean] of Byte = (TKP_TRACKVERT, TKP_TRACK);
var
  ATheme: TdxTheme;
begin
  ATheme := OpenTheme(totTrackBar);
  if ATheme <> 0 then
    DrawThemeBackground(ATheme, ACanvas.Handle, ATrackType[AHorizontal], TUS_NORMAL, ARect)
  else
    inherited;
end;

procedure TcxWinXPLookAndFeelPainter.DrawTrackBarScaledThumb(ACanvas: TcxCanvas; const ARect: TRect;
  AState: TcxButtonState; AHorizontal: Boolean; ATicks: TcxTrackBarTicksAlign; AThumbColor: TColor;
  AScaleFactor: TdxScaleFactor);
const
  StateMap: array[TcxButtonState] of Byte = (
    TUS_HOT, TUS_NORMAL, TUS_HOT, TUS_PRESSED, TUS_DISABLED
  );
var
  ATheme: TdxTheme;
begin
  ATheme := OpenTheme(totTrackBar);
  if ATheme <> 0 then
    DrawScaledThemeBackground(ATheme, ACanvas.Handle,
      TrackBarThumbThemeParts[ATicks, AHorizontal], StateMap[AState], ARect, AScaleFactor)
  else
    inherited DrawTrackBarScaledThumb(ACanvas, ARect, AState, AHorizontal, ATicks, AThumbColor, AScaleFactor);
end;

procedure TcxWinXPLookAndFeelPainter.DrawRangeTrackBarScaledLeftThumb(ACanvas: TcxCanvas; const ARect: TRect;
  AState: TcxButtonState; AHorizontal: Boolean; ATicks: TcxTrackBarTicksAlign; AThumbColor: TColor;
  AScaleFactor: TdxScaleFactor);
begin
  DrawNativeRangeTrackBarThumb(ACanvas, ARect, AState, AHorizontal, ATicks, AThumbColor, AScaleFactor);
end;

procedure TcxWinXPLookAndFeelPainter.DrawRangeTrackBarScaledRightThumb(ACanvas: TcxCanvas;
  const ARect: TRect; AState: TcxButtonState; AHorizontal: Boolean; ATicks: TcxTrackBarTicksAlign;
  AThumbColor: TColor; AScaleFactor: TdxScaleFactor);
begin
  DrawNativeRangeTrackBarThumb(ACanvas, ARect, AState, AHorizontal, ATicks, AThumbColor, AScaleFactor);
end;

procedure TcxWinXPLookAndFeelPainter.DrawDateNavigatorDateHeader(ACanvas: TcxCanvas; var R: TRect);
begin
  DrawThemeBackground(OpenTheme(totHeader), ACanvas.Handle, HP_HEADERITEMLEFT, HIS_NORMAL, R);
end;

function TcxWinXPLookAndFeelPainter.NavigatorButtonPressedGlyphOffset: TPoint;
begin
  Result := cxNullPoint;
end;

function TcxWinXPLookAndFeelPainter.NativeZoomButtonDraw(ACanvas: TcxCanvas;
  const R: TRect; AState: TcxButtonState; AScaleFactor: TdxScaleFactor; AGlyph: TBitmap): Boolean;
var
  ATheme: TdxTheme;
begin
  ATheme := OpenTheme(totButton);
  Result := ATheme <> 0;
  if Result then
  begin
    DrawThemeBackground(ATheme, ACanvas.Handle, BP_PUSHBUTTON, BtnStateToXPBtnState(AState), R);
    cxDrawImage(ACanvas, R, AGlyph, nil, -1, ifmNormal, idmNormal, True, nil, AScaleFactor);
  end;
end;

function TcxWinXPLookAndFeelPainter.NativeZoomButtonGetMinSize(
  AGlyph: TBitmap; AScaleFactor: TdxScaleFactor; out ASize: TSize): Boolean;
var
  AMargins: TdxMargins;
  ATheme: TdxTheme;
begin
  ATheme := OpenTheme(totButton);
  Result := ATheme <> 0;
  if Result then
  begin
    GetThemeMargins(ATheme, 0, BP_PUSHBUTTON, 0, TMT_CONTENTMARGINS, nil, AMargins);
    ASize := cxSize(AGlyph.Width + AMargins.cxLeftWidth + AMargins.cxRightWidth,
      AGlyph.Height + AMargins.cyTopHeight + AMargins.cyBottomHeight);
    ASize := AScaleFactor.Apply(ASize);
  end;
end;

function TcxWinXPLookAndFeelPainter.NavigatorScaledButtonMinSize(AScaleFactor: TdxScaleFactor): TSize;
begin
  Result := AScaleFactor.Apply(Size(18, 19));
end;

function TcxWinXPLookAndFeelPainter.DropDownListBoxItemTextColor(ASelected: Boolean): TColor;
var
  ATheme: TdxTheme;
  AColorRef: COLORREF;
begin
  ATheme := OpenTheme(totMenu);
  if ATheme <> 0 then
  begin
    GetThemeColor(ATheme, MENU_POPUPITEM, MPI_NORMAL, TMT_TEXTCOLOR, AColorRef);
    Result := AColorRef;
  end
  else
    Result := inherited DropDownListBoxItemTextColor(ASelected);
end;

function TcxWinXPLookAndFeelPainter.DropDownListBoxScaledSeparatorSize(AScaleFactor: TdxScaleFactor): Integer;
var
  ASize: TSize;
  ATheme: TdxTheme;
begin
  ATheme := OpenTheme(totMenu);
  if ATheme <> 0 then
  begin
    GetThemePartSize(ATheme, 0, MENU_POPUPSEPARATOR, 0, nil, TS_TRUE, @ASize);
    Result := ASize.cy;
  end
  else
    Result := inherited DropDownListBoxScaledSeparatorSize(AScaleFactor);
end;

procedure TcxWinXPLookAndFeelPainter.DrawDropDownListBoxBackground(
  ACanvas: TcxCanvas; const ARect: TRect; AHasBorders: Boolean);

  function GetMenuEdgeColor(ATheme: TdxTheme; APropID: Integer; ADefaultColor: TColor): TColor;
  var
    AColor: COLORREF;
  begin
    if GetThemeColor(ATheme, MENU_POPUPBACKGROUND, 0, APropID, AColor) = S_OK then
      Result := AColor
    else
      Result := ADefaultColor;
  end;

var
  ATheme: TdxTheme;
  R: TRect;
begin
  if AreVisualStylesAvailable then
  begin
    R := ARect;
    ATheme := OpenTheme(totMenu);
    if AHasBorders then
    begin
      ACanvas.FrameRect(R, GetMenuEdgeColor(ATheme, TMT_EDGELIGHTCOLOR, clBtnShadow));
      R := cxRectInflate(R, -1, -1);
      ACanvas.FrameRect(R, GetMenuEdgeColor(ATheme, TMT_EDGEHIGHLIGHTCOLOR, clBtnHighlight));
      R := cxRectInflate(R, -1, -1);
    end;
    if ATheme <> 0 then
      DrawThemeBackground(ATheme, ACanvas.Handle, MENU_POPUPBACKGROUND, 0, R, nil)
    else
      ACanvas.FillRect(R, clWindow);
  end
  else
    inherited DrawDropDownListBoxBackground(ACanvas, ARect, AHasBorders);
end;

procedure TcxWinXPLookAndFeelPainter.DrawDropDownListBoxScaledGutterBackground(
  ACanvas: TcxCanvas; const ARect: TRect; AScaleFactor: TdxScaleFactor);
var
  ATheme: TdxTheme;
begin
  ATheme := OpenTheme(totMenu);
  if ATheme <> 0 then
    DrawThemeBackground(ATheme, ACanvas.Handle, MENU_POPUPGUTTER, 0, ARect, nil)
  else
    inherited DrawDropDownListBoxScaledGutterBackground(ACanvas, ARect, AScaleFactor);
end;

procedure TcxWinXPLookAndFeelPainter.DrawDropDownListBoxScaledSelection(
  ACanvas: TcxCanvas; const ARect, AGutterRect: TRect; AScaleFactor: TdxScaleFactor);
var
  ATheme: TdxTheme;
begin
  ATheme := OpenTheme(totMenu);
  if ATheme <> 0 then
    DrawThemeBackground(ATheme, ACanvas.Handle, MENU_POPUPITEM, MPI_HOT, ARect, nil)
  else
    inherited DrawDropDownListBoxScaledSelection(ACanvas, ARect, AGutterRect, AScaleFactor);
end;

procedure TcxWinXPLookAndFeelPainter.DrawDropDownListBoxScaledSeparator(
  ACanvas: TcxCanvas; const ARect, AGutterRect: TRect; AScaleFactor: TdxScaleFactor);
var
  ASize: TSize;
  ATheme: TdxTheme;
  R: TRect;
begin
  ATheme := OpenTheme(totMenu);
  if ATheme <> 0 then
  begin
    GetThemePartSize(ATheme, ACanvas.Handle, MENU_POPUPSEPARATOR, 0, nil, TS_TRUE, @ASize);
    R := cxRectCenterVertically(ARect, ASize.cy);
    R.Left := AGutterRect.Right + 2;
    DrawThemeBackground(ATheme, ACanvas.Handle, MENU_POPUPSEPARATOR, 0, R, nil);
  end
  else
    inherited DrawDropDownListBoxScaledSeparator(ACanvas, ARect, AGutterRect, AScaleFactor);
end;

function TcxWinXPLookAndFeelPainter.BreadcrumbEditBackgroundColor(
  AState: TdxBreadcrumbEditState): TColor;
begin
  Result := DefaultContentColor;
end;

function TcxWinXPLookAndFeelPainter.BreadcrumbEditIsFadingSupports: Boolean;
begin
  Result := True;
end;

function TcxWinXPLookAndFeelPainter.BreadcrumbEditScaledProgressChunkPadding(AScaleFactor: TdxScaleFactor): TRect;
begin
  Result := cxNullRect;
end;

function TcxWinXPLookAndFeelPainter.BreadcrumbEditScaledProgressChunkOverlaySize(AScaleFactor: TdxScaleFactor): TSize;
var
  ATheme: TdxTheme;
begin
  ATheme := OpenTheme(totProgress);
  if IsWinVistaOrLater and (ATheme <> 0) and (GetThemePartSize(ATheme, 0, PP_MOVEOVERLAY, 0, nil, TS_TRUE, @Result) = S_OK) then
    Result := AScaleFactor.Apply(Result, dxSystemScaleFactor)
  else
    Result := inherited BreadcrumbEditScaledProgressChunkOverlaySize(AScaleFactor);
end;

function TcxWinXPLookAndFeelPainter.BreadcrumbEditBordersSize: TRect;
begin
  Result := cxSimpleRect;
end;

function TcxWinXPLookAndFeelPainter.BreadcrumbEditScaledButtonContentOffsets(
  AIsFirst, AIsLast: Boolean; AScaleFactor: TdxScaleFactor): TRect;
var
  ATheme: TdxTheme;
  AMargins: TdxMargins;
begin
  ATheme := OpenTheme(totButton);
  if ATheme <> 0 then
  begin
    GetThemeMargins(ATheme, 0, BP_PUSHBUTTON, 0, TMT_CONTENTMARGINS, nil, AMargins);
    Result := cxRect(AMargins.cxLeftWidth, AMargins.cyTopHeight, AMargins.cxRightWidth, AMargins.cyBottomHeight);
    Result := AScaleFactor.Apply(Result, dxSystemScaleFactor);
  end
  else
    Result := inherited BreadcrumbEditScaledButtonContentOffsets(AIsFirst, AIsLast, AScaleFactor);
end;

procedure TcxWinXPLookAndFeelPainter.DrawBreadcrumbEditBorders(
  ACanvas: TcxCanvas; const ARect: TRect; ABorders: TcxBorders; AState: TdxBreadcrumbEditState);
const
  PartsMap: array[Boolean] of Integer = (EP_EDITTEXT, EP_EDITBORDER_NOSCROLL);
  StatesMap: array[TdxBreadcrumbEditState] of Integer =
    (ETS_NORMAL, ETS_FOCUSED, ETS_HOT, ETS_DISABLED);

  function ExcludeBorders(const R: TRect; const ASizingMargins: TdxMargins): TRect;
  begin
    Result := R;
    if not (bLeft in ABorders) then
      Dec(Result.Left, ASizingMargins.cxLeftWidth);
    if not (bTop in ABorders) then
      Dec(Result.Top, ASizingMargins.cyTopHeight);
    if not (bRight in ABorders) then
      Inc(Result.Right, ASizingMargins.cxRightWidth);
    if not (bBottom in ABorders) then
      Inc(Result.Bottom, ASizingMargins.cyBottomHeight);
  end;

  procedure DoDrawBorders(ATheme: TdxTheme; const R: TRect);
  begin
    DrawThemeBackground(ATheme, ACanvas.Handle, PartsMap[IsWinVistaOrLater], StatesMap[AState], R);
  end;

var
  AMargins: TdxMargins;
  ATheme: TdxTheme;
begin
  ATheme := OpenTheme(totEdit);
  if ATheme = 0 then
    inherited DrawBreadcrumbEditBorders(ACanvas, ARect, ABorders, AState)
  else
    if ABorders <> cxBordersAll then
    begin
      ACanvas.SaveClipRegion;
      try
        ACanvas.IntersectClipRect(ARect);
        GetThemeMargins(ATheme, 0, PartsMap[IsWinVistaOrLater], 0, TMT_SIZINGMARGINS, nil, AMargins);
        DoDrawBorders(ATheme, ExcludeBorders(ARect, AMargins));
      finally
        ACanvas.RestoreClipRegion;
      end;
    end
    else
      DoDrawBorders(ATheme, ARect);
end;

procedure TcxWinXPLookAndFeelPainter.DrawBreadcrumbEditScaledButtonAreaSeparator(
  ACanvas: TcxCanvas; const ARect: TRect; AState: TdxBreadcrumbEditState; AScaleFactor: TdxScaleFactor);
begin
end;

procedure TcxWinXPLookAndFeelPainter.DrawBreadcrumbEditScaledButton(
  ACanvas: TcxCanvas; const ARect: TRect; AState: TdxBreadcrumbEditButtonState;
  AIsFirst, AIsLast: Boolean; AScaleFactor: TdxScaleFactor);

  function GetContentMargins: TRect;
  begin
    Result := BreadcrumbEditScaledButtonContentOffsets(AIsFirst, AIsLast, AScaleFactor);
    Result.Left := 1;
    Result.Right := 1;
  end;

const
  StateMap: array[TdxBreadcrumbEditButtonState] of Integer = (-1, PBS_NORMAL, PBS_HOT, PBS_PRESSED, -1);
var
  ATheme: TdxTheme;
begin
  ATheme := OpenTheme(totButton);
  if ATheme = 0 then
    inherited DrawBreadcrumbEditScaledButton(ACanvas, ARect, AState, AIsFirst, AIsLast, AScaleFactor)
  else
    if StateMap[AState] >= 0 then
    begin
      ACanvas.SaveClipRegion;
      try
        ACanvas.IntersectClipRect(ARect);
        DrawThemeBackground(ATheme, ACanvas.Handle, BP_PUSHBUTTON, StateMap[AState], cxRectInflate(ARect, GetContentMargins));
      finally
        ACanvas.RestoreClipRegion;
      end;
    end;
end;

procedure TcxWinXPLookAndFeelPainter.DrawBreadcrumbEditScaledDropDownButton(
  ACanvas: TcxCanvas; const ARect: TRect; AState: TdxBreadcrumbEditButtonState;
  AIsInEditor: Boolean; AScaleFactor: TdxScaleFactor);
begin
  DrawBreadcrumbEditScaledButton(ACanvas, ARect, AState, True, True, AScaleFactor);
end;

procedure TcxWinXPLookAndFeelPainter.DrawBreadcrumbEditScaledNode(ACanvas: TcxCanvas; const R: TRect;
  AState: TdxBreadcrumbEditButtonState; AHasDelimiter: Boolean; AScaleFactor: TdxScaleFactor);
const
  BordersMap: array[Boolean] of TcxBorders = ([bLeft, bRight], [bLeft]);
begin
  if OpenTheme(totButton) = 0 then
    inherited DrawBreadcrumbEditScaledNode(ACanvas, R, AState, AHasDelimiter, AScaleFactor)
  else
    DrawBreadcrumbEditNodePart(ACanvas, R, AState, BordersMap[AHasDelimiter], AScaleFactor);
end;

procedure TcxWinXPLookAndFeelPainter.DrawBreadcrumbEditScaledNodeDelimiter(
  ACanvas: TcxCanvas; const R: TRect; AState: TdxBreadcrumbEditButtonState; AScaleFactor: TdxScaleFactor);
begin
  if OpenTheme(totButton) = 0 then
    inherited DrawBreadcrumbEditScaledNodeDelimiter(ACanvas, R, AState, AScaleFactor)
  else
    DrawBreadcrumbEditNodePart(ACanvas, R, AState, [bLeft, bRight], AScaleFactor);
end;

class procedure TcxWinXPLookAndFeelPainter.DrawScaledThemeBackground(ATheme: TdxTheme; DC: HDC; APartId, AStateId: Integer;
  const R: TRect; AScaleFactor: TdxScaleFactor; AForceScale: Boolean = False; AUseThemePartSize: Boolean = True);
var
  ABitmap: TcxBitmap32;
  APartSize: TSize;
begin
  if AForceScale or not AScaleFactor.Equals(dxSystemScaleFactor) then
  begin
    if AUseThemePartSize then
      GetThemePartSize(ATheme, 0, APartId, AStateId, TS_TRUE, APartSize)
    else
      APartSize := dxSystemScaleFactor.Apply(cxRectSize(R), AScaleFactor);

    ABitmap := TcxBitmap32.CreateSize(APartSize.cx, APartSize.cy, True);
    try
      DrawThemeBackground(ATheme, ABitmap.Canvas.Handle, APartId, AStateId, ABitmap.ClientRect);
      ABitmap.RecoverTransparency(ABitmap.Canvas.Pixels[0, ABitmap.Height]);
      cxAlphaBlend(DC, ABitmap, R, ABitmap.ClientRect, True);
    finally
      ABitmap.Free;
    end;
  end
  else
    DrawThemeBackground(ATheme, DC, APartId, AStateId, R);
end;

procedure TcxWinXPLookAndFeelPainter.DoDrawScaledScrollBarPart(ACanvas: TcxCanvas; AHorizontal: Boolean; R: TRect;
  APart: TcxScrollBarPart; AState: TcxButtonState; AScaleFactor: TdxScaleFactor);
const
  ThumbnailKind: array[Boolean] of Integer = (SBP_THUMBBTNVERT, SBP_THUMBBTNHORZ);
  ThumbnailGripperKind: array[Boolean] of Integer = (SBP_GRIPPERVERT, SBP_GRIPPERHORZ);
  ThumbnailPage: array[Boolean, Boolean] of Integer =
    ((SBP_UPPERTRACKVERT, SBP_LOWERTRACKVERT), (SBP_UPPERTRACKHORZ, SBP_LOWERTRACKHORZ));

  function GetThumbnailState: Integer;
  begin
    case AState of
      cxbsHot:
        Result := SCRBS_HOT;
      cxbsPressed:
        Result := SCRBS_PRESSED;
      cxbsDisabled:
        Result := SCRBS_DISABLED;
      else
        Result := SCRBS_NORMAL;
    end;
  end;

  function CanDrawThumbnailGripper(ATheme: TdxTheme; AElement: Integer): Boolean;
  var
    ASize: TSize;
  begin
    Result := True;
    if GetThemePartSize(ATheme, 0, AElement, SCRBS_NORMAL, TS_TRUE, ASize) = S_OK then
      if AHorizontal then
        Result := AScaleFactor.Apply(ASize.cy) < cxRectWidth(R)
      else
        Result := AScaleFactor.Apply(ASize.cx) < cxRectHeight(R);
  end;

var
  ATheme: TdxTheme;
begin
  if IsRectEmpty(R) then
    Exit;

  ATheme := OpenTheme(totScrollBar);
  if ATheme = TC_NONE then
    inherited DoDrawScaledScrollBarPart(ACanvas, AHorizontal, R, APart, AState, AScaleFactor)
  else
    case APart of
      sbpLineUp, sbpLineDown:
        DrawScaledArrow(ACanvas, R, AState, GetArrowDirection(AHorizontal, APart), AScaleFactor);

      sbpPageUp, sbpPageDown:
        DrawThemeBackground(ATheme, ACanvas.Handle,
          ThumbnailPage[AHorizontal, APart = sbpPageUp], GetThumbnailState, @R);

      sbpThumbnail:
        begin
          DrawThemeBackground(ATheme, ACanvas.Handle, ThumbnailKind[AHorizontal], GetThumbnailState, @R);
          if CanDrawThumbnailGripper(ATheme, ThumbnailKind[AHorizontal]) then
            DrawThemeBackground(ATheme, ACanvas.Handle, ThumbnailGripperKind[AHorizontal], GetThumbnailState, @R);
        end;
    end;
end;

procedure TcxWinXPLookAndFeelPainter.DrawBreadcrumbEditNodePart(
  ACanvas: TcxCanvas; const R: TRect; AState: TdxBreadcrumbEditButtonState;
  ABorders: TcxBorders; AScaleFactor: TdxScaleFactor);
const
  StateMap: array[TdxBreadcrumbEditButtonState] of Integer =
    (-1, PBS_NORMAL, PBS_HOT, PBS_PRESSED, -1);
var
  AContentExtends: TRect;
begin
  if (StateMap[AState] >= 0) and not cxRectIsEmpty(R) then
  begin
    ACanvas.SaveClipRegion;
    try
      ACanvas.IntersectClipRect(R);
      AContentExtends := BreadcrumbEditScaledButtonContentOffsets(True, True, AScaleFactor);
      if bLeft in ABorders then
        AContentExtends.Left := 1;
      if bBottom in ABorders then
        AContentExtends.Bottom := 1;
      if bRight in ABorders then
        AContentExtends.Right := 1;
      if bTop in ABorders then
        AContentExtends.Top := 1;
      DrawThemeBackground(OpenTheme(totButton), ACanvas.Handle,
        BP_PUSHBUTTON, StateMap[AState], cxRectInflate(R, AContentExtends));
    finally
      ACanvas.RestoreClipRegion;
    end;
  end;
end;

procedure TcxWinXPLookAndFeelPainter.DrawBreadcrumbEditScaledNodeDelimiterGlyph(
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

procedure TcxWinXPLookAndFeelPainter.DrawBreadcrumbEditScaledNodeMoreButtonGlyph(
  ACanvas: TcxCanvas; const R: TRect; AState: TdxBreadcrumbEditButtonState; AScaleFactor: TdxScaleFactor);
begin
  DrawCollapseArrow(ACanvas, R, BreadcrumbEditNodeTextColor(AState), AScaleFactor.Apply(2));
end;

procedure TcxWinXPLookAndFeelPainter.DrawBreadcrumbEditScaledProgressChunk(
  ACanvas: TcxCanvas; const R: TRect; AScaleFactor: TdxScaleFactor);
var
  ATheme: TdxTheme;
begin
  ATheme := OpenTheme(totProgress);
  if ATheme <> 0 then
    DrawThemeBackground(ATheme, ACanvas.Handle, PP_CHUNK, 1, R)
  else
    inherited DrawBreadcrumbEditScaledProgressChunk(ACanvas, R, AScaleFactor);
end;

procedure TcxWinXPLookAndFeelPainter.DrawBreadcrumbEditScaledProgressChunkOverlay(
  ACanvas: TcxCanvas; const R: TRect; AScaleFactor: TdxScaleFactor);
var
  ATheme: TdxTheme;
begin
  ATheme := OpenTheme(totProgress);
  if (ATheme <> 0) and IsWinVistaOrLater then
    DrawThemeBackground(ATheme, ACanvas.Handle, PP_MOVEOVERLAY, 0, R)
  else
    inherited DrawBreadcrumbEditScaledProgressChunkOverlay(ACanvas, R, AScaleFactor);
end;

function TcxWinXPLookAndFeelPainter.AlertWindowCornerRadius: Integer;
begin
  Result := 5;
end;

function TcxWinXPLookAndFeelPainter.GetScaledBackButtonSize(AScaleFactor: TdxScaleFactor): TSize;
var
  ATheme: TdxTheme;
begin
  ATheme := OpenTheme(totNavigation);
  if (ATheme = 0) or (GetThemePartSize(ATheme, 0, NAV_BACKBUTTON, 0, nil, TS_TRUE, @Result) <> S_OK) then
    Result := inherited GetScaledBackButtonSize(AScaleFactor)
  else
    Result := AScaleFactor.Apply(Result, dxSystemScaleFactor);
end;

procedure TcxWinXPLookAndFeelPainter.DrawScaledBackButton(
  ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState; AScaleFactor: TdxScaleFactor);
const
  StatesMap: array[TcxButtonState] of Integer = (
    NAV_BB_NORMAL, NAV_BB_NORMAL, NAV_BB_HOT, NAV_BB_PRESSED, NAV_BB_DISABLED
  );
var
  ATheme: TdxTheme;
begin
  ATheme := OpenTheme(totNavigation);
  if ATheme <> 0 then
    DrawScaledThemeBackground(ATheme, ACanvas.Handle, NAV_BACKBUTTON, StatesMap[AState], R, AScaleFactor)
  else
    inherited DrawBackButton(ACanvas, R, AState);
end;

procedure TcxWinXPLookAndFeelPainter.DrawBevelFrame(
  ACanvas: TcxCanvas; const R: TRect; AColor1, AColor2: TColor; ABoxStyle: Boolean);
var
  ATheme: TdxTheme;
begin
  ATheme := OpenTheme(totButton);
  if ATheme <> 0 then
    DrawThemeBackground(ATheme, ACanvas.Handle, BP_GROUPBOX, GBS_NORMAL, R)
  else
    inherited DrawBevelFrame(ACanvas, R, AColor1, AColor2, ABoxStyle);
end;

procedure TcxWinXPLookAndFeelPainter.DrawBevelLine(
  ACanvas: TcxCanvas; const R: TRect; AColor1, AColor2: TColor; AIsVertical: Boolean);
const
  PartMap: array[Boolean] of Cardinal = (TP_SEPARATORVERT, TP_SEPARATOR);
var
  ATheme: TdxTheme;
begin
  ATheme := OpenTheme(totToolBar);
  if ATheme <> 0 then
    DrawThemeBackground(ATheme, ACanvas.Handle, PartMap[AIsVertical], TS_NORMAL, R)
  else
    inherited DrawBevelLine(ACanvas, R, AColor1, AColor2, AIsVertical);
end;

function TcxWinXPLookAndFeelPainter.GetBevelMinimalShapeSize(AShape: TdxBevelShape): TSize;
begin
  Result := cxSize(6, 6);
end;

function TcxWinXPLookAndFeelPainter.DrawGalleryItemSelectionFirst: Boolean;
begin
  Result := (OpenTheme(totExplorerListView) <> 0) or inherited DrawGalleryItemSelectionFirst;
end;

procedure TcxWinXPLookAndFeelPainter.DrawGalleryItemSelection(
  ACanvas: TcxCanvas; const R: TRect; AViewState: TdxGalleryItemViewState);

  function GetState: Integer;
  begin
    if AViewState.Pressed then
      Result := LIS_SELECTED
    else
      if AViewState.Checked then
      begin
        if (AViewState.Hover or AViewState.Focused) and IsWinVistaOrLater then
          Result := LIS_HOTSELECTED
        else
          Result := LIS_SELECTED
      end
      else
        if AViewState.Focused then
          Result := LIS_SELECTEDNOTFOCUS
        else
          if AViewState.Hover then
            Result := LIS_HOT
          else
            Result := LIS_NORMAL;
  end;

var
  ATheme: TdxTheme;
  AState: Integer;
begin
  ATheme := OpenTheme(totExplorerListView);
  if ATheme <> 0 then
  begin
    AState := GetState;
    if AState <> LIS_NORMAL then
      DrawThemeBackground(ATheme, ACanvas.Handle, LVP_LISTITEM, AState, R)
  end
  else
    inherited DrawGalleryItemSelection(ACanvas, R, AViewState);
end;

procedure TcxWinXPLookAndFeelPainter.DrawScaledToggleSwitchState(ACanvas: TcxCanvas;
  ABounds: TRect; AState: TcxButtonState; AChecked: Boolean; AScaleFactor: TdxScaleFactor);
begin
  dxGPPaintCanvas.BeginPaint(ACanvas.Handle, ABounds);
  try
    dxGPPaintCanvas.Rectangle(ABounds, $707070, ToggleSwitchToggleColor(AChecked), 1, psSolid, 255, 255);
  finally
    dxGPPaintCanvas.EndPaint;
  end;
end;

procedure TcxWinXPLookAndFeelPainter.DrawScaledToggleSwitchThumb(
  ACanvas: TcxCanvas; ABounds: TRect; AState: TcxButtonState; AScaleFactor: TdxScaleFactor);
begin
  DrawScaledButton(ACanvas, cxRectInflate(ABounds, -1), '', AState, AScaleFactor);
end;

function TcxWinXPLookAndFeelPainter.ToggleSwitchToggleColor(AChecked: Boolean): TColor;
begin
  if AChecked then
    Result := $A0A0A0
  else
    Result := $C8C8C8;
end;

function TcxWinXPLookAndFeelPainter.ScaledClockSize(AScaleFactor: TdxScaleFactor): TSize;
begin
  Result := AScaleFactor.Apply(ClockFace.Size);
end;

procedure TcxWinXPLookAndFeelPainter.DrawScaledClock(ACanvas: TcxCanvas;
  const ARect: TRect; ADateTime: TDateTime; ABackgroundColor: TColor; AScaleFactor: TdxScaleFactor);
begin
  DrawModernCalendarClock(ACanvas, ARect, ADateTime, ABackgroundColor, AScaleFactor);
end;

procedure TcxWinXPLookAndFeelPainter.DrawModernCalendarDateCellSelection(
  ACanvas: TcxCanvas; const ARect: TRect; AStates: TcxCalendarElementStates);

  function GetThemePartState(AStates: TcxCalendarElementStates): Integer;
  begin
    if cesSelected in AStates then
      if cesHot in AStates then
        Result := LIS_HOTSELECTED
      else
        if cesFocused in AStates then
          Result := LIS_SELECTED
        else
          Result := LIS_SELECTEDNOTFOCUS
    else
      if cesHot in AStates then
        Result := LIS_HOT
      else
        Result := LIS_NORMAL;
  end;

var
  ATheme: TdxTheme;
begin
  ATheme := OpenTheme(totExplorerListView);
  if ATheme <> 0 then
  begin
    if AStates * [cesHot, cesSelected] <> [] then
      DrawThemeBackground(ATheme, ACanvas.Handle, LVP_LISTITEM, GetThemePartState(AStates), ARect, nil);

    if cesMarked in AStates then
      ACanvas.FrameRect(ARect, GetModernCalendarMarkedCellBorderColor);

    if cesFocused in AStates then
      ACanvas.DrawFocusRect(cxRectInflate(ARect, -1, -1));
  end
  else
    inherited;
end;

function TcxWinXPLookAndFeelPainter.GetModernCalendarHeaderTextColor(AStates: TcxCalendarElementStates): TColor;
var
  ATheme: TdxTheme;
begin
  ATheme := OpenTheme(totTextStyle);
  if Failed(GetThemeColor(ATheme, TEXT_HYPERLINKTEXT, TS_HYPERLINK_NORMAL, TMT_TEXTCOLOR, COLORREF(Result))) then
    Result := inherited GetModernCalendarHeaderTextColor(AStates);
end;

procedure TcxWinXPLookAndFeelPainter.DrawFindPanelBorder(ACanvas: TcxCanvas; const R: TRect; ABorders: TcxBorders);
begin
end;

procedure TcxWinXPLookAndFeelPainter.DrawWheelPickerItemBackground(ACanvas: TcxCanvas; const ARect: TRect;
  AState: TcxButtonState);

  function GetThemePartState: Integer;
  begin
    if AState = cxbsPressed then
      Result := LIS_SELECTED
    else
      Result := LIS_SELECTEDNOTFOCUS;
  end;

var
  ATheme: TdxTheme;
begin
  ATheme := OpenTheme(totExplorerListView);
  if (ATheme <> 0) and (AState in [cxbsPressed, cxbsHot]) then
    DrawThemeBackground(ATheme, ACanvas.Handle, LVP_LISTITEM, GetThemePartState, ARect, nil)
  else
    inherited;
end;

procedure TcxWinXPLookAndFeelPainter.DrawWheelPickerItemBorder(ACanvas: TcxCanvas; const ARect: TRect; AState: TcxButtonState);
begin
  if (OpenTheme(totExplorerListView) = 0) or not (AState in [cxbsPressed, cxbsHot]) then
    inherited;
end;

procedure TcxWinXPLookAndFeelPainter.DrawScaledTokenBackground(
  ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState; AScaleFactor: TdxScaleFactor);
const
  ButtonStateMap: array [TcxButtonState] of Integer = (-1, -1, PBS_HOT, PBS_PRESSED, -1);

  function MarginsToRect(const AMargins: TdxMargins): TRect;
  begin
    Result := cxRect(AMargins.cxLeftWidth, AMargins.cyTopHeight, AMargins.cxRightWidth, AMargins.cyBottomHeight);
  end;

var
  AMargins: TdxMargins;
  ATheme: TdxTheme;
begin
  ATheme := OpenTheme(totButton);
  if ATheme = 0 then
    inherited DrawScaledTokenBackground(ACanvas, R, AState, AScaleFactor)
  else
    if (ButtonStateMap[AState] >= 0) and not cxRectIsEmpty(R) then
    begin
      ACanvas.SaveClipRegion;
      try
        ATheme := OpenTheme(totButton);
        GetThemeMargins(ATheme, 0, BP_PUSHBUTTON, 0, TMT_CONTENTMARGINS, nil, AMargins);
        DrawThemeBackground(ATheme, ACanvas.Handle, BP_PUSHBUTTON, ButtonStateMap[AState], cxRectInflate(R, MarginsToRect(AMargins)));
      finally
        ACanvas.RestoreClipRegion;
      end;
    end;
end;

procedure TcxWinXPLookAndFeelPainter.DrawNativeRangeTrackBarThumb(
  ACanvas: TcxCanvas; const ARect: TRect; AState: TcxButtonState; AHorizontal: Boolean;
  ATicks: TcxTrackBarTicksAlign; AThumbColor: TColor; AScaleFactor: TdxScaleFactor);
var
  R: TRect;
begin
  R := ARect;
  if IsWinVistaOrLater then
  begin
    if AHorizontal then
      Inc(R.Right)
    else
      Inc(R.Bottom);
  end;
  DrawTrackBarScaledThumb(ACanvas, R, AState, AHorizontal, tbtaBoth, AThumbColor, AScaleFactor);
end;


{ TcxLookAndFeelPaintersManager }

function cxLookAndFeelPaintersManagerSortProc(AItem1, AItem2: TcxCustomLookAndFeelPainter): Integer;
begin
  Result := AnsiCompareStr(AItem1.LookAndFeelName, AItem2.LookAndFeelName);
end;


constructor TcxLookAndFeelPaintersManager.Create;
begin
  inherited Create;
  FPainters := TcxObjectList.Create;
  FListeners := TInterfaceList.Create;
end;

destructor TcxLookAndFeelPaintersManager.Destroy;
begin
  FreeAndNil(FListeners);
  FreeAndNil(FPainters);
  inherited Destroy;
end;

procedure TcxLookAndFeelPaintersManager.AddListener(const AListener: IcxLookAndFeelPainterListener);
begin
  if Listeners.IndexOf(AListener) < 0 then
    Listeners.Add(AListener);
end;

procedure TcxLookAndFeelPaintersManager.DoPainterAdded(APainter: TcxCustomLookAndFeelPainter);
var
  I: Integer;
begin
  DoRootLookAndFeelChanged;
  for I := Listeners.Count - 1 downto 0 do
    IcxLookAndFeelPainterListener(Listeners[I]).PainterAdded(APainter);
end;

procedure TcxLookAndFeelPaintersManager.DoPainterRemoved(APainter: TcxCustomLookAndFeelPainter);
var
  I: Integer;
begin
  DoRootLookAndFeelChanged;
  for I := Listeners.Count - 1 downto 0 do
    IcxLookAndFeelPainterListener(Listeners[I]).PainterRemoved(APainter);
end;

procedure TcxLookAndFeelPaintersManager.DoRootLookAndFeelChanged;
begin
  if RootLookAndFeel <> nil then
    RootLookAndFeel.Refresh;
end;

function TcxLookAndFeelPaintersManager.GetPainter(
  AStyle: TcxLookAndFeelStyle; out APainter: TcxCustomLookAndFeelPainter): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Count - 1 do
    if Items[I].LookAndFeelStyle = AStyle then
    begin
      APainter := Items[I];
      Result := True;
      Break;
    end;
end;

function TcxLookAndFeelPaintersManager.GetPainter(
  const AName: string; out APainter: TcxCustomLookAndFeelPainter): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Count - 1 do
    if SameText(Items[I].LookAndFeelName, AName) then
    begin
      APainter := Items[I];
      Result := True;
      Break;
    end;
end;

function TcxLookAndFeelPaintersManager.GetPainter(AStyle: TcxLookAndFeelStyle): TcxCustomLookAndFeelPainter;
begin
  if not GetPainter(AStyle, Result) then
    Result := nil;
end;

function TcxLookAndFeelPaintersManager.GetPainter(const AName: string): TcxCustomLookAndFeelPainter;
begin
  if not GetPainter(AName, Result) then
    Result := nil;
end;

function TcxLookAndFeelPaintersManager.Register(APainter: TcxCustomLookAndFeelPainter): Boolean;
begin
  Result := GetPainter(APainter.LookAndFeelName) = nil;
  if Result then
  begin
    FPainters.Add(APainter);
    SortPainters;
    DoPainterAdded(APainter);
  end
  else
    APainter.Free;
end;

procedure TcxLookAndFeelPaintersManager.RemoveListener(const AListener: IcxLookAndFeelPainterListener);
begin
  Listeners.Remove(AListener);
end;

procedure TcxLookAndFeelPaintersManager.ReleaseImageResources;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].ReleaseImageResources;
end;

procedure TcxLookAndFeelPaintersManager.SortPainters;
begin
  FPainters.Sort(TListSortCompare(@cxLookAndFeelPaintersManagerSortProc));
end;

function TcxLookAndFeelPaintersManager.Unregister(const AName: string): Boolean;
var
  APainter: TcxCustomLookAndFeelPainter;
begin
  APainter := GetPainter(AName);
  Result := APainter <> nil;
  if Result then
  try
    FPainters.Remove(APainter);
    DoPainterRemoved(APainter);
  finally
    APainter.Free;
  end;
end;

function TcxLookAndFeelPaintersManager.GetCount: Integer;
begin
  Result := FPainters.Count;
end;

function TcxLookAndFeelPaintersManager.GetItem(AIndex: Integer): TcxCustomLookAndFeelPainter;
begin
  Result := TcxCustomLookAndFeelPainter(FPainters[AIndex]);
end;

{ TcxFlatLookAndFeelPainter }

function TcxFlatLookAndFeelPainter.LookAndFeelName: string;
begin
  Result := 'Flat';
end;

function TcxFlatLookAndFeelPainter.LookAndFeelStyle: TcxLookAndFeelStyle;
begin
  Result := lfsFlat;
end;

function TcxFlatLookAndFeelPainter.BorderSize: Integer;
begin
  Result := 1;
end;

function TcxFlatLookAndFeelPainter.SeparatorSize: Integer;
begin
  Result := 2;
end;

procedure TcxFlatLookAndFeelPainter.DrawBorder(ACanvas: TcxCanvas; R: TRect);
begin
  ACanvas.DrawEdge(R, True, True);
end;

function TcxFlatLookAndFeelPainter.ButtonBorderSize(AState: TcxButtonState = cxbsNormal): Integer;
begin
  if AState = cxbsDefault then
    Result := 2
  else
    Result := 1;
end;

function TcxFlatLookAndFeelPainter.ScaledButtonTextOffset(AScaleFactor: TdxScaleFactor): Integer;
begin
  Result := AScaleFactor.Apply(1);
end;

function TcxFlatLookAndFeelPainter.ScaledButtonTextShift(AScaleFactor: TdxScaleFactor): Integer;
begin
  Result := AScaleFactor.Apply(1);
end;

procedure TcxFlatLookAndFeelPainter.DrawButtonBorder(ACanvas: TcxCanvas; R: TRect;
  AState: TcxButtonState);
begin
  if AState = cxbsPressed then
    ACanvas.DrawComplexFrame(R, clBtnShadow, clBtnHighlight)
  else
  begin
    if AState = cxbsDefault then
    begin
      ACanvas.FrameRect(R, clBlack);
      InflateRect(R, -1, -1);
    end;
    ACanvas.DrawComplexFrame(R, clBtnHighlight, clBtnShadow)
  end;
end;

procedure TcxFlatLookAndFeelPainter.DrawScaledExpandButton(
  ACanvas: TcxCanvas; const R: TRect; AExpanded: Boolean; AScaleFactor: TdxScaleFactor;
  AColor: TColor = clDefault; AState: TcxExpandButtonState = cebsNormal);
var
  ARect: TRect;
begin
  ARect := R;
  DrawScaledButton(ACanvas, ARect, '', cxbsNormal, AScaleFactor, True, AColor);
  InflateRect(ARect, -1, -1);
  DrawExpandButtonCross(ACanvas, ARect, AExpanded, clBtnText, AScaleFactor);
  ACanvas.ExcludeClipRect(R);
end;

function TcxFlatLookAndFeelPainter.ScaledExpandButtonSize(AScaleFactor: TdxScaleFactor): Integer;
begin
  Result := AScaleFactor.Apply(11);
end;

function TcxFlatLookAndFeelPainter.IsButtonHotTrack: Boolean;
begin
  Result := False;
end;

function TcxFlatLookAndFeelPainter.DefaultSchedulerTimeRulerBorderColor: TColor;
begin
  Result := DefaultSchedulerTimeRulerBorderColorClassic;
end;

function TcxFlatLookAndFeelPainter.DefaultSchedulerTimeRulerTextColor: TColor;
begin
  Result := DefaultSchedulerTimeRulerBorderColorClassic;
end;

procedure TcxFlatLookAndFeelPainter.DrawCheckBorder(ACanvas: TcxCanvas; R: TRect; AState: TcxButtonState);
begin
  ACanvas.DrawEdge(R, True, True);
  InflateRect(R, -1, -1);
  ACanvas.FrameRect(R, CheckButtonColor(AState, cbsChecked));
end;

procedure TcxFlatLookAndFeelPainter.DrawHeaderBorder(ACanvas: TcxCanvas;
  const R: TRect; ANeighbors: TcxNeighbors; ABorders: TcxBorders);
var
  I: TcxNeighbor;
begin
  with ACanvas do
  begin
    DrawEdge(R, False, False);
    for I := Low(I) to High(I) do
      if I in ANeighbors then
        case I of
          nLeft:
            begin
              Pixels[R.Left, R.Top + 1] := clBtnFace;  //!!!
              Pixels[R.Left, R.Bottom - 2] := clBtnFace;  //!!!
            end;
          nRight:
            begin
              Pixels[R.Right - 1, R.Top] := clBtnHighlight;
              Pixels[R.Right - 1, R.Top + 1] := clBtnFace;  //!!!
              Pixels[R.Right - 1, R.Bottom - 2] := clBtnFace;  //!!!
            end;
          nTop:;
          nBottom:;
        end;
  end;
end;

procedure TcxFlatLookAndFeelPainter.DrawHeaderControlSectionBorder(
  ACanvas: TcxCanvas; const R: TRect; ABorders: TcxBorders; AState: TcxButtonState);
begin
  if AState <> cxbsPressed then
    ACanvas.DrawComplexFrame(R, clBtnHighlight, clBtnShadow, ABorders)
  else
    ACanvas.DrawComplexFrame(R, clBtnShadow, clBtnHighlight, ABorders);
end;

procedure TcxFlatLookAndFeelPainter.DrawScaledSortingMark(
  ACanvas: TcxCanvas; const R: TRect; AAscendingSorting: Boolean; AScaleFactor: TdxScaleFactor);
begin
  DrawSortingArrow(ACanvas, R, clBtnShadow, clBtnHighlight, AAscendingSorting, AScaleFactor);
end;

procedure TcxFlatLookAndFeelPainter.DrawScaledSummarySortingMark(ACanvas: TcxCanvas;
  const R: TRect; AAscendingSorting: Boolean; AScaleFactor: TdxScaleFactor);
begin
  DrawSummarySortingArrow(ACanvas, R, clBtnShadow, clBtnHighlight, AAscendingSorting, AScaleFactor);
end;

function TcxFlatLookAndFeelPainter.HeaderBorderSize: Integer;
begin
  Result := 1;
end;

function TcxFlatLookAndFeelPainter.ScaledSortingMarkSize(AScaleFactor: TdxScaleFactor): TPoint;
begin
  Result := AScaleFactor.Apply(Point(8, 7));
end;

function TcxFlatLookAndFeelPainter.ScaledSummarySortingMarkSize(AScaleFactor: TdxScaleFactor): TPoint;
begin
  Result := AScaleFactor.Apply(Point(8, 8));
end;

function TcxFlatLookAndFeelPainter.FooterBorderSize: Integer;
begin
  Result := 1;
end;

function TcxFlatLookAndFeelPainter.FooterCellBorderSize: Integer;
begin
  Result := 1;
end;

function TcxFlatLookAndFeelPainter.FooterCellOffset: Integer;
begin
  Result := 1;
end;

procedure TcxFlatLookAndFeelPainter.DrawFooterBorder(ACanvas: TcxCanvas;
  const R: TRect);
begin
  ACanvas.DrawEdge(R, False, False);
end;

procedure TcxFlatLookAndFeelPainter.DrawFooterCellBorder(ACanvas: TcxCanvas; const R: TRect);
begin
  ACanvas.DrawEdge(R, True, True);
end;

procedure TcxFlatLookAndFeelPainter.DrawScaledFilterDropDownButton(ACanvas: TcxCanvas;
  R: TRect; AState: TcxButtonState; AIsFilterActive: Boolean; AScaleFactor: TdxScaleFactor);

  function GetArrowColor: TColor;
  begin
    if AIsFilterActive then
      Result := ActiveFilterButtonArrowColor
    else
      Result := clBtnText;
  end;

begin
  if AState <> cxbsPressed then
    ACanvas.DrawEdge(R, False, False)
  else
    ACanvas.DrawEdge(R, True, True);
  InflateRect(R, -1, -1);
  ACanvas.Brush.Color := clBtnFace;
  ACanvas.FillRect(R);
  DrawButtonArrow(ACanvas, R, GetArrowColor);
end;

procedure TcxFlatLookAndFeelPainter.DrawTabBorder(ACanvas: TcxCanvas; R: TRect;
  ABorder: TcxBorder; ABorders: TcxBorders; AVertical: Boolean);
const
  Colors: array[Boolean] of TColor = (clBtnShadow, clBtnHighlight);

  procedure ProcessVerticalTabBorder;
  begin
    case ABorder of
       bLeft:
         begin
           Inc(R.Top);
           if bTop in ABorders then Inc(R.Top);
           Dec(R.Bottom);
           if bBottom in ABorders then Dec(R.Bottom);
         end;
       bTop, bBottom:
         if bLeft in ABorders then Inc(R.Left);
    end;
    if ABorder = bLeft then
    begin
      ACanvas.Pixels[R.Left + 1, R.Top - 1] := Colors[True];
      ACanvas.Pixels[R.Left + 1, R.Bottom] := Colors[True];
    end;
    ACanvas.Brush.Color := Colors[ABorder <> bBottom];
  end;

  procedure ProcessHorizontalTabBorder;
  begin
    case ABorder of
       bTop:
         begin
           Inc(R.Left);
           Dec(R.Right);
         end;
       bLeft, bRight:
         begin
           if bTop in ABorders then Inc(R.Top, 2);
           if bBottom in ABorders then Dec(R.Bottom);
         end;
    end;
    if ABorder = bTop then
    begin
      ACanvas.Pixels[R.Left - 1, R.Top + 1] := Colors[True];
      ACanvas.Pixels[R.Right, R.Top + 1] := Colors[True];
    end;
    ACanvas.Brush.Color := Colors[ABorder <> bRight];
  end;

begin
  if AVertical then
    ProcessVerticalTabBorder
  else
    ProcessHorizontalTabBorder;
  ACanvas.FillRect(R);
end;

procedure TcxFlatLookAndFeelPainter.DrawTabsRoot(ACanvas: TcxCanvas; const R: TRect;
  ABorders: TcxBorders; AVertical: Boolean);
begin
  ACanvas.DrawEdge(R, False, False, ABorders);
end;

function TcxFlatLookAndFeelPainter.TabBorderSize(AVertical: Boolean): Integer;
begin
  Result := 1;
end;

procedure TcxFlatLookAndFeelPainter.DrawSchedulerSplitterBorder(
  ACanvas: TcxCanvas; R: TRect; const AViewParams: TcxViewParams;
  AIsHorizontal: Boolean);
begin
  if AIsHorizontal then
  begin
    ACanvas.SetBrushColor(clBtnHighlight);
    ACanvas.FillRect(Rect(R.Left, R.Top, R.Right, R.Top + 1));
    ACanvas.SetBrushColor(clBtnShadow);
    ACanvas.FillRect(Rect(R.Left, R.Bottom - 1, R.Right, R.Bottom));
    InflateRect(R, 1, -1);
  end
  else
  begin
    ACanvas.SetBrushColor(clBtnHighlight);
    ACanvas.FillRect(Rect(R.Left, R.Top, R.Left + 1, R.Bottom));
    ACanvas.SetBrushColor(clBtnShadow);
    ACanvas.FillRect(Rect(R.Right - 1, R.Top, R.Right, R.Bottom));
    InflateRect(R, -1, 1);
  end;
  ACanvas.FillRect(R, AViewParams);
end;

procedure TcxFlatLookAndFeelPainter.DrawGroupBoxFrame(ACanvas: TcxCanvas;
  R: TRect; AEnabled: Boolean; ACaptionPosition: TcxGroupBoxCaptionPosition;
  ABorders: TcxBorders = cxBordersAll);
begin
  ACanvas.FrameRect(R, clBtnShadow, 1, ABorders, True);
  InflateRect(R, -1, -1);
  ACanvas.FrameRect(R, clBtnHighlight, 1, ABorders, True);
end;

procedure TcxFlatLookAndFeelPainter.DrawTrackBarTrackBounds(ACanvas: TcxCanvas; const ARect: TRect);
var
  R: TRect;
begin
  R := ARect;
  ACanvas.DrawComplexFrame(R, clBtnShadow, clBtnHighlight);
  InflateRect(R, -1, -1);
  ACanvas.DrawComplexFrame(R, clBtnFace, clBtnFace);
end;

procedure TcxFlatLookAndFeelPainter.DrawTrackBarThumbBorderUpDown(ACanvas: TcxCanvas;
  const ALightPolyLine, AShadowPolyLine, ADarkPolyLine: TPoints);
begin
  ACanvas.Pen.Color := clBtnHighlight;
  ACanvas.Polyline(ALightPolyLine);
  ACanvas.Pen.Color := clBtnShadow;
  ACanvas.Polyline(ADarkPolyLine);
end;

procedure TcxFlatLookAndFeelPainter.DrawTrackBarThumbBorderBoth(ACanvas: TcxCanvas; const ARect: TRect);
begin
  ACanvas.DrawComplexFrame(ARect, clBtnHighlight, clBtnShadow)
end;

procedure TcxFlatLookAndFeelPainter.DrawDateNavigatorDateHeader(
  ACanvas: TcxCanvas; var R: TRect);
begin
  ACanvas.FillRect(R, DefaultDateNavigatorHeaderColor);
  ACanvas.FrameRect(R, clBtnFace, 1, [bBottom]);
  Dec(R.Bottom);
end;

function TcxFlatLookAndFeelPainter.GetSplitterOuterColor(AHighlighted: Boolean): TColor;
begin
  Result := inherited GetSplitterOuterColor(AHighlighted);
  if AHighlighted then
    Result := cl3DDkShadow;
end;

procedure TcxFlatLookAndFeelPainter.DrawScaledToggleSwitchState(ACanvas: TcxCanvas; ABounds: TRect;
  AState: TcxButtonState; AChecked: Boolean; AScaleFactor: TdxScaleFactor);
begin
  ACanvas.FillRect(ABounds, ToggleSwitchToggleColor(AChecked));
  ACanvas.Pen.Color := $A0A0A0;
  ACanvas.MoveTo(ABounds.Left, ABounds.Bottom);
  ACanvas.LineTo(ABounds.Left, ABounds.Top);
  ACanvas.LineTo(ABounds.Right, ABounds.Top);
  ACanvas.Pen.Color := $FFFFFF;
  ACanvas.LineTo(ABounds.Right, ABounds.Bottom);
  ACanvas.LineTo(ABounds.Left, ABounds.Bottom);
end;

function TcxFlatLookAndFeelPainter.ToggleSwitchToggleColor(AChecked: Boolean): TColor;
begin
  if AChecked then
    Result := $A0A0A0
  else
    Result := $C8C8C8;
end;

procedure TcxFlatLookAndFeelPainter.DrawFindPanelBorder(ACanvas: TcxCanvas; const R: TRect; ABorders: TcxBorders);
begin
end;

procedure TcxFlatLookAndFeelPainter.DoDrawScaledScrollBarPart(ACanvas: TcxCanvas; AHorizontal: Boolean; R: TRect;
  APart: TcxScrollBarPart; AState: TcxButtonState; AScaleFactor: TdxScaleFactor);
begin
  if IsRectEmpty(R) or ((APart = sbpThumbnail) and (AState = cxbsDisabled)) then Exit;
  if AState = cxbsHot then
    AState := cxbsNormal;
  case APart of
    sbpThumbnail, sbpLineUp, sbpLineDown:
      begin
        DrawScaledButton(ACanvas, R, '', AState, AScaleFactor);
        InflateRect(R, -ButtonBorderSize, -ButtonBorderSize);
        if APart <> sbpThumbnail then
          DrawScaledScrollBarArrow(ACanvas, R, AState, GetArrowDirection(AHorizontal, APart), AScaleFactor);
      end;
    sbpPageUp, sbpPageDown:
      begin
        ACanvas.Brush.Bitmap := StdScrollBitmaps[AState = cxbsPressed];
        ACanvas.FillRect(R);
        ACanvas.Brush.Bitmap := nil;
        ACanvas.Brush.Style := bsSolid;
      end;
  end;
end;

procedure PrepareRadioButtonImageList(AScaleFactor: TdxScaleFactor);
begin
  TcxRadioButtonImageListManager.Get(AScaleFactor);
end;

{ TcxRadioButtonImageListManager }

class procedure TcxRadioButtonImageListManager.Finalize;
begin
  FreeAndNil(FCache);
end;

class procedure TcxRadioButtonImageListManager.Reset;
var
  AList: TcxRadioButtonImageList;
begin
  if FCache <> nil then
  begin
    for AList in FCache.Values do
      AList.Reset;
  end;
end;

class function TcxRadioButtonImageListManager.Get(AScaleFactor: TdxScaleFactor): TcxRadioButtonImageList;
begin
  if FCache = nil then
    FCache := TObjectDictionary<Integer, TcxRadioButtonImageList>.Create([doOwnsValues]);
  if not FCache.TryGetValue(AScaleFactor.TargetDPI, Result) then
  begin
    Result := TcxRadioButtonImageList.Create(AScaleFactor);
    FCache.Add(AScaleFactor.TargetDPI, Result);
  end;
end;

procedure CalculateCheckButtonSize;
var
  AButtonsBitmap: HBITMAP;
  B: Windows.TBitmap;
begin
  AButtonsBitmap := LoadBitmap(0, PChar(OBM_CHECKBOXES));
  try
    dxGetBitmapData(AButtonsBitmap, B);
    FCheckButtonSize := Size(B.bmWidth div 4, B.bmHeight div 3);
  finally
    DeleteObject(AButtonsBitmap);
  end;
end;


{ TcxStandardLookAndFeelPainter }

function TcxStandardLookAndFeelPainter.LookAndFeelName: string;
begin
  Result := 'Standard';
end;

function TcxStandardLookAndFeelPainter.LookAndFeelStyle: TcxLookAndFeelStyle;
begin
  Result := lfsStandard;
end;

function TcxStandardLookAndFeelPainter.BorderSize: Integer;
begin
  Result := 2;
end;

procedure TcxStandardLookAndFeelPainter.DrawBorder(ACanvas: TcxCanvas; R: TRect);
begin
  ACanvas.DrawComplexFrame(R, clBtnShadow, clBtnHighlight);
  InflateRect(R, -1, -1);
  ACanvas.DrawComplexFrame(R, cl3DDkShadow, cl3DLight);
end;

function TcxStandardLookAndFeelPainter.ButtonBorderSize(AState: TcxButtonState = cxbsNormal): Integer;
begin
  if AState = cxbsDefault then
    Result := 3
  else
    Result := 2;
end;

function TcxStandardLookAndFeelPainter.ScaledButtonTextOffset(AScaleFactor: TdxScaleFactor): Integer;
begin
  Result := AScaleFactor.Apply(1);
end;

function TcxStandardLookAndFeelPainter.ScaledButtonTextShift(AScaleFactor: TdxScaleFactor): Integer;
begin
  Result := AScaleFactor.Apply(1);
end;

procedure TcxStandardLookAndFeelPainter.DrawButtonBorder(ACanvas: TcxCanvas; R: TRect;
  AState: TcxButtonState);
begin
  if AState = cxbsPressed then
  begin
    ACanvas.FrameRect(R, clBlack);
    InflateRect(R, -1, -1);
    ACanvas.DrawComplexFrame(R, clBtnShadow, clBtnShadow);
  end
  else
  begin
    if AState = cxbsDefault then
    begin
      ACanvas.FrameRect(R, clBlack);
      InflateRect(R, -1, -1);
    end;
    ACanvas.DrawComplexFrame(R, clBtnHighlight, cl3DDkShadow);
    InflateRect(R, -1, -1);
    ACanvas.DrawComplexFrame(R, cl3DLight, clBtnShadow);
  end;
end;

procedure TcxStandardLookAndFeelPainter.DrawScaledGroupExpandButton(ACanvas: TcxCanvas;
  const R: TRect; AExpanded: Boolean; AState: TcxButtonState; AScaleFactor: TdxScaleFactor);
var
  APainter: TcxCustomLookAndFeelPainter;
begin
  if cxLookAndFeelPaintersManager.GetPainter(lfsFlat, APainter) then
    APainter.DrawScaledGroupExpandButton(ACanvas, R, AExpanded, AState, AScaleFactor)
  else
    inherited DrawScaledGroupExpandButton(ACanvas, R, AExpanded, AState, AScaleFactor);
end;

function TcxStandardLookAndFeelPainter.ScaledExpandButtonSize(AScaleFactor: TdxScaleFactor): Integer;
begin
  Result := AScaleFactor.Apply(12);
end;

procedure TcxStandardLookAndFeelPainter.DrawScaledExpandButton(
  ACanvas: TcxCanvas; const R: TRect; AExpanded: Boolean; AScaleFactor: TdxScaleFactor;
  AColor: TColor = clDefault; AState: TcxExpandButtonState = cebsNormal);
var
  ARect: TRect;

  procedure DrawButton;
  begin
    ACanvas.DrawEdge(ARect, False, False, [bLeft, bTop]);
    ACanvas.DrawEdge(ARect, False, True, [bRight, bBottom]);
    InflateRect(ARect, -1, -1);
    ACanvas.DrawEdge(ARect, False, False, [bRight, bBottom]);
    Dec(ARect.Right);
    Dec(ARect.Bottom);
    ACanvas.Brush.Color := cxGetActualColor(AColor, clBtnFace);
    ACanvas.FillRect(ARect);
  end;

begin
  ARect := R;
  DrawButton;
  DrawExpandButtonCross(ACanvas, ARect, AExpanded, clBtnText, AScaleFactor);
  ACanvas.ExcludeClipRect(R);
end;

function TcxStandardLookAndFeelPainter.IsButtonHotTrack: Boolean;
begin
  Result := False;
end;

function TcxStandardLookAndFeelPainter.DefaultSchedulerTimeRulerBorderColor: TColor;
begin
  Result := DefaultSchedulerTimeRulerBorderColorClassic;
end;

function TcxStandardLookAndFeelPainter.DefaultSchedulerTimeRulerTextColor: TColor;
begin
  Result := DefaultSchedulerTimeRulerBorderColorClassic;
end;

procedure TcxStandardLookAndFeelPainter.DrawCheckBorder(ACanvas: TcxCanvas; R: TRect;
  AState: TcxButtonState);
begin
  ACanvas.DrawEdge(R, True, False{True});
  InflateRect(R, -1, -1);
  ACanvas.DrawEdge(R, True, True{False});
end;

procedure TcxStandardLookAndFeelPainter.DrawHeaderBorder(ACanvas: TcxCanvas;
  const R: TRect; ANeighbors: TcxNeighbors; ABorders: TcxBorders);
var
  I: TcxNeighbor;
begin
  with ACanvas do
  begin
    DrawEdge(R, False, False);
    for I := Low(I) to High(I) do
      if I in ANeighbors then
        case I of
          nLeft:
            begin
              Pixels[R.Left, R.Top + 1] := clBtnFace;  //!!!
              Pixels[R.Left, R.Bottom - 2] := clBtnFace;  //!!!
            end;
          nRight:
            begin
              Pixels[R.Right - 1, R.Top] := clBtnHighlight;
              Pixels[R.Right - 1, R.Top + 1] := clBtnFace;  //!!!
              Pixels[R.Right - 1, R.Bottom - 2] := clBtnFace;  //!!!
            end;
          nTop:;
          nBottom:;
        end;
  end;
end;

procedure TcxStandardLookAndFeelPainter.DrawHeaderControlSectionBorder(
  ACanvas: TcxCanvas; const R: TRect;
  ABorders: TcxBorders; AState: TcxButtonState);
var
  ARect: TRect;
begin
  ARect := R;
  if AState <> cxbsPressed then
  begin
    ACanvas.DrawComplexFrame(ARect, clBtnHighlight,
      cl3DDkShadow, ABorders);
    InflateRect(ARect, -1, -1);
    ACanvas.DrawComplexFrame(ARect, cl3DLight, clBtnShadow, ABorders);
  end
  else
    ACanvas.DrawComplexFrame(ARect, clBtnShadow, clBtnShadow, ABorders);
end;

procedure TcxStandardLookAndFeelPainter.DrawScaledSortingMark(
  ACanvas: TcxCanvas; const R: TRect; AAscendingSorting: Boolean; AScaleFactor: TdxScaleFactor);
begin
  DrawSortingArrow(ACanvas, R, clBtnShadow, clBtnHighlight, AAscendingSorting, AScaleFactor);
end;

procedure TcxStandardLookAndFeelPainter.DrawScaledSummarySortingMark(
  ACanvas: TcxCanvas; const R: TRect; AAscendingSorting: Boolean; AScaleFactor: TdxScaleFactor);
begin
  DrawSummarySortingArrow(ACanvas, R, clBtnShadow, clBtnHighlight, AAscendingSorting, AScaleFactor);
end;

function TcxStandardLookAndFeelPainter.HeaderBorderSize: Integer;
begin
  Result := 1;
end;

function TcxStandardLookAndFeelPainter.HeaderControlSectionBorderSize(
  AState: TcxButtonState = cxbsNormal): Integer;
begin
  if AState = cxbsPressed then
    Result := 1
  else
    Result := 2;
end;

function TcxStandardLookAndFeelPainter.ScaledSortingMarkSize(AScaleFactor: TdxScaleFactor): TPoint;
begin
  Result := AScaleFactor.Apply(Point(8, 7));
end;

function TcxStandardLookAndFeelPainter.ScaledSummarySortingMarkSize(AScaleFactor: TdxScaleFactor): TPoint;
begin
  Result := AScaleFactor.Apply(Point(8, 8));
end;

function TcxStandardLookAndFeelPainter.FooterBorderSize: Integer;
begin
  Result := 1;
end;

function TcxStandardLookAndFeelPainter.FooterCellBorderSize: Integer;
begin
  Result := 1;
end;

function TcxStandardLookAndFeelPainter.FooterCellOffset: Integer;
begin
  Result := 1;
end;

procedure TcxStandardLookAndFeelPainter.DrawFooterBorder(ACanvas: TcxCanvas;
  const R: TRect);
begin
  ACanvas.DrawEdge(R, False, False);
end;

procedure TcxStandardLookAndFeelPainter.DrawFooterCellBorder(ACanvas: TcxCanvas; const R: TRect);
begin
  ACanvas.DrawEdge(R, True, True);
end;

procedure TcxStandardLookAndFeelPainter.DrawScaledFilterDropDownButton(ACanvas: TcxCanvas;
  R: TRect; AState: TcxButtonState; AIsFilterActive: Boolean; AScaleFactor: TdxScaleFactor);

  function GetArrowColor: TColor;
  begin
    if AIsFilterActive then
      Result := ActiveFilterButtonArrowColor
    else
      Result := clBtnText;
  end;

begin
  if AState <> cxbsPressed then
    ACanvas.DrawEdge(R, False, False)
  else
    ACanvas.DrawEdge(R, True, True);
  InflateRect(R, -1, -1);
  ACanvas.Brush.Color := clBtnFace;
  ACanvas.FillRect(R);
  DrawButtonArrow(ACanvas, R, GetArrowColor);
end;

procedure TcxStandardLookAndFeelPainter.DrawTabBorder(ACanvas: TcxCanvas; R: TRect;
  ABorder: TcxBorder; ABorders: TcxBorders; AVertical: Boolean);
const
  Colors: array[Boolean] of TColor = (clBtnShadow, clBtnHighlight);

  procedure ProcessVerticalTabBorder;
  begin
    case ABorder of
       bLeft:
         begin
           Inc(R.Top);
           if bTop in ABorders then Inc(R.Top);
           Dec(R.Bottom);
           if bBottom in ABorders then Dec(R.Bottom);
         end;
       bTop, bBottom:
         if bLeft in ABorders then Inc(R.Left);
    end;
    if ABorder = bLeft then
    begin
      ACanvas.Pixels[R.Left + 1, R.Top - 1] := Colors[True];
      ACanvas.Pixels[R.Left + 1, R.Bottom] := Colors[True];
    end;
    ACanvas.Brush.Color := Colors[ABorder <> bBottom];
  end;

  procedure ProcessHorizontalTabBorder;
  begin
    case ABorder of
       bTop:
         begin
           Inc(R.Left);
           Dec(R.Right);
         end;
       bLeft, bRight:
         begin
           if bTop in ABorders then Inc(R.Top, 2);
           if bBottom in ABorders then Dec(R.Bottom);
         end;
    end;
    if ABorder = bTop then
    begin
      ACanvas.Pixels[R.Left - 1, R.Top + 1] := Colors[True];
      ACanvas.Pixels[R.Right, R.Top + 1] := Colors[True];
    end;
    ACanvas.Brush.Color := Colors[ABorder <> bRight];
  end;

begin
  if AVertical then
    ProcessVerticalTabBorder
  else
    ProcessHorizontalTabBorder;
  ACanvas.FillRect(R);
end;

procedure TcxStandardLookAndFeelPainter.DrawTabsRoot(ACanvas: TcxCanvas; const R: TRect;
  ABorders: TcxBorders; AVertical: Boolean);
begin
  ACanvas.DrawEdge(R, False, False, ABorders);
end;

function TcxStandardLookAndFeelPainter.TabBorderSize(AVertical: Boolean): Integer;
begin
  Result := 1;
end;

procedure TcxStandardLookAndFeelPainter.DrawScaledMonthHeader(ACanvas: TcxCanvas;
  const ABounds: TRect; const AText: string; ANeighbors: TcxNeighbors;
  const AViewParams: TcxViewParams; AArrows: TcxArrowDirections;
  ASideWidth: Integer; AScaleFactor: TdxScaleFactor; AOnDrawBackground: TcxDrawBackgroundEvent = nil);
const
  Borders: array[Boolean, Boolean] of TcxBorders =
  ((cxBordersAll, [bTop, bBottom, bLeft]), ([bTop, bBottom, bRight], [bTop, bBottom]));
var
  ABorders: TcxBorders;
  R: TRect;
begin
  ABorders := Borders[nLeft in ANeighbors, nRight in ANeighbors];
  ACanvas.FrameRect(ABounds, clBlack, 1, ABorders);
  R := ScaledHeaderContentBounds(ABounds, ABorders, AScaleFactor);
  DrawScaledHeader(ACanvas, R, R, ANeighbors, cxBordersAll, cxbsNormal, taCenter,
    vaCenter, False, False, AText, AViewParams.Font, AViewParams.TextColor,
    AViewParams.Color, AScaleFactor, AOnDrawBackground);
  DrawMonthHeaderArrows(ACanvas, ABounds, AArrows, ASideWidth, clWindowText);
end;

function TcxStandardLookAndFeelPainter.DefaultSchedulerViewContentColor: TColor;
begin
  Result := DefaultSchedulerViewContentColorClassic;
end;

function TcxStandardLookAndFeelPainter.DefaultSchedulerViewContentColorClassic: TColor;
begin
  Result := clWindow;
end;

procedure TcxStandardLookAndFeelPainter.DrawSchedulerSplitterBorder(
  ACanvas: TcxCanvas; R: TRect; const AViewParams: TcxViewParams;
  AIsHorizontal: Boolean);

  procedure DrawHorzBorders(const R: TRect; ATopColor, ABottomColor: TColor);
  begin
    ACanvas.SetBrushColor(ATopColor);
    ACanvas.FillRect(Rect(R.Left, R.Top, R.Right, R.Top + 1));
    ACanvas.SetBrushColor(ABottomColor);
    ACanvas.FillRect(Rect(R.Left, R.Bottom - 1, R.Right, R.Bottom));
  end;

  procedure DrawVertBorders(const R: TRect; ALeftColor, ARightColor: TColor);
  begin
    ACanvas.SetBrushColor(ALeftColor);
    ACanvas.FillRect(Rect(R.Left, R.Top, R.Left + 1, R.Bottom));
    ACanvas.SetBrushColor(ARightColor);
    ACanvas.FillRect(Rect(R.Right - 1, R.Top, R.Right, R.Bottom));
  end;

begin
  if AIsHorizontal then
  begin
    DrawHorzBorders(R, clBtnFace, cl3DDkShadow);
    InflateRect(R, 1, -1);
    DrawHorzBorders(R, clBtnHighlight, clBtnShadow);
    InflateRect(R, 1, -1);
  end
  else
  begin
    DrawVertBorders(R, clBtnFace, cl3DDkShadow);
    InflateRect(R, -1, 1);
    DrawVertBorders(R, clBtnHighlight, clBtnShadow);
    InflateRect(R, -1, 1);
  end;
  ACanvas.FillRect(R, AViewParams);
end;

procedure TcxStandardLookAndFeelPainter.DrawGroupBoxFrame(ACanvas: TcxCanvas;
  R: TRect; AEnabled: Boolean; ACaptionPosition: TcxGroupBoxCaptionPosition;
  ABorders: TcxBorders = cxBordersAll);
begin
  Dec(R.Right);
  Dec(R.Bottom);
  ACanvas.FrameRect(R, clBtnShadow, 1, ABorders, True);
  OffsetRect(R, 1, 1);
  ACanvas.FrameRect(R, clBtnHighlight, 1, ABorders, True);
end;

procedure TcxStandardLookAndFeelPainter.DrawTrackBarTrackBounds(ACanvas: TcxCanvas; const ARect: TRect);
var
  R: TRect;
begin
  R := ARect;
  ACanvas.DrawComplexFrame(R, clBtnShadow, clBtnHighlight);
  InflateRect(R, -1, -1);
  ACanvas.DrawComplexFrame(R, cl3DDkShadow, cl3DLight);
end;

procedure TcxStandardLookAndFeelPainter.DrawTrackBarThumbBorderUpDown(ACanvas: TcxCanvas;
  const ALightPolyLine, AShadowPolyLine, ADarkPolyLine: TPoints);
begin
  ACanvas.Pen.Color := clBtnHighlight;
  ACanvas.Polyline(ALightPolyLine);
  ACanvas.Pen.Color := clBtnShadow;
  ACanvas.Polyline(AShadowPolyLine);
  ACanvas.Pen.Color := cl3DDkShadow;
  ACanvas.Polyline(ADarkPolyLine);
end;

procedure TcxStandardLookAndFeelPainter.DrawTrackBarThumbBorderBoth(ACanvas: TcxCanvas; const ARect: TRect);
var
  R: TRect;
begin
  R := ARect;
  ACanvas.DrawComplexFrame(R, clBtnHighlight, cl3DDkShadow);
  InflateRect(R, -1, -1);
  ACanvas.DrawComplexFrame(R, cl3DLight, clBtnShadow);
end;

procedure TcxStandardLookAndFeelPainter.DrawBreadcrumbEditScaledButton(
  ACanvas: TcxCanvas; const ARect: TRect; AState: TdxBreadcrumbEditButtonState;
  AIsFirst, AIsLast: Boolean; AScaleFactor: TdxScaleFactor);
begin
  if not cxRectIsEmpty(ARect) then
  begin
    if AState = dxbcbsPressed then
      ACanvas.DrawComplexFrame(ARect, clBtnShadow, clBtnHighlight)
    else
      if AState in [dxbcbsFocused, dxbcbsHot] then
        ACanvas.DrawComplexFrame(ARect, clBtnHighlight, clBtnShadow);
  end;
end;

procedure TcxStandardLookAndFeelPainter.DrawScaledToggleSwitchState(
  ACanvas: TcxCanvas; ABounds: TRect; AState: TcxButtonState; AChecked: Boolean; AScaleFactor: TdxScaleFactor);
begin
  ACanvas.FillRect(ABounds, ToggleSwitchToggleColor(AChecked));
  DrawContainerBorder(ACanvas, ABounds, cbs3D, 2, $696969, [bLeft, bTop, bRight, bBottom]);
end;

function TcxStandardLookAndFeelPainter.ToggleSwitchToggleColor(AChecked: Boolean): TColor;
begin
  if AChecked then
    Result := $219921
  else
    Result := $E3E3E3;
end;

procedure TcxStandardLookAndFeelPainter.DrawFindPanelBorder(ACanvas: TcxCanvas; const R: TRect; ABorders: TcxBorders);
begin
end;

procedure TcxStandardLookAndFeelPainter.DrawScaledTokenBackground(
  ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState; AScaleFactor: TdxScaleFactor);
begin
  ACanvas.DrawComplexFrame(R, clBtnShadow, clBtnHighlight);
  case AState of
    cxbsHot:
      ACanvas.DrawComplexFrame(cxRectInflate(R, -1), clBtnHighlight, clBtnShadow);
    cxbsPressed:
      ACanvas.DrawComplexFrame(cxRectInflate(R, -1), clBtnShadow, clBtnHighlight);
  end;
end;

procedure TcxStandardLookAndFeelPainter.DoDrawScaledScrollBarPart(ACanvas: TcxCanvas; AHorizontal: Boolean; R: TRect;
  APart: TcxScrollBarPart; AState: TcxButtonState; AScaleFactor: TdxScaleFactor);

  function GetArrowState: Integer;
  const
    States: array[Boolean, Boolean] of Integer = ((DFCS_SCROLLUP, DFCS_SCROLLDOWN),
     (DFCS_SCROLLLEFT, DFCS_SCROLLRIGHT));
  begin
    Result := States[AHorizontal, APart = sbpLineDown];
    if AState = cxbsDisabled then Result := Result or DFCS_INACTIVE
    else if AState = cxbsPressed then Result := Result or DFCS_FLAT;
  end;

  procedure DrawScrollBarButtonBorder(R: TRect);
  begin
    if (AState <> cxbsPressed) or (APart = sbpThumbnail) then
    begin
      ACanvas.DrawComplexFrame(R, clBtnFace, cl3DDkShadow);
      InflateRect(R, -1, -1);
      ACanvas.DrawComplexFrame(R, clBtnHighlight, clBtnShadow);
    end
    else
    begin
      ACanvas.DrawComplexFrame(R, clBtnShadow, clBtnShadow);
      InflateRect(R, -1, -1);
      ACanvas.DrawComplexFrame(R, clBtnFace, clBtnFace);
    end;
  end;

begin
  if IsRectEmpty(R) or ((APart = sbpThumbnail) and (AState = cxbsDisabled)) then Exit;
  if AState = cxbsHot then AState := cxbsNormal;
  case APart of
    sbpThumbnail, sbpLineUp, sbpLineDown:
      begin
        // for compatibility with standard painting
        if APart <> sbpThumbnail then
        begin
          if AState = cxbsPressed then OffsetRect(R, 1, 1);
          DrawFrameControl(ACanvas.Canvas.Handle, R, DFC_SCROLL, GetArrowState);
          if AState = cxbsPressed then OffsetRect(R, -1, -1);
          DrawScrollBarButtonBorder(R);
        end
        else
        begin
          DrawScrollBarButtonBorder(R);
          InflateRect(R, -ButtonBorderSize, -ButtonBorderSize);
          ACanvas.Brush.Color := clBtnFace;
          ACanvas.FillRect(R);
        end;
      end;
    sbpPageUp, sbpPageDown:
      begin
        ACanvas.Brush.Bitmap := StdScrollBitmaps[AState = cxbsPressed];
        ACanvas.FillRect(R);
        ACanvas.Brush.Bitmap := nil;
        ACanvas.Brush.Style := bsSolid;
      end;
  end;
end;


{ TcxCustomLookAndFeelPainter }

destructor TcxCustomLookAndFeelPainter.Destroy;
begin
  ReleaseImageResources;
  inherited Destroy;
end;

function TcxCustomLookAndFeelPainter.CreateLookAndFeelPainterDetails: TObject;

  procedure LoadIcon(ADetails: TdxSkinDetails; AIcon: TdxSkinIconSize);
  const
    SuffixMap: array[TdxSkinIconSize] of string = ('16', '48');
  var
    AResName: string;
  begin
    AResName := 'CX_LOOKANDFEELSTYLEICON_' + UpperCase(LookAndFeelName) + SuffixMap[AIcon];
    if FindResource(HInstance, PChar(AResName), 'PNG') <> 0 then
      ADetails.Icons[AIcon].LoadFromResource(HInstance, AResName, 'PNG')
    else
      ADetails.ResetIcon(AIcon);
  end;

var
  ADetails: TdxSkinDetails;
begin
  ADetails := TdxSkinDetails.Create;
  ADetails.Name := LookAndFeelName;
  ADetails.DisplayName := LookAndFeelName;
  ADetails.GroupName := cxGetResourceString(@scxBuiltInLookAndFeelStyles);
  LoadIcon(ADetails, sis16);
  LoadIcon(ADetails, sis48);
  Result := ADetails;
end;

function TcxCustomLookAndFeelPainter.GetLookAndFeelPainterDetails: TObject;
begin
  if FLookAndFeelPainterDetailsCache = nil then
    FLookAndFeelPainterDetailsCache := CreateLookAndFeelPainterDetails;
  Result := FLookAndFeelPainterDetailsCache;
end;

function TcxCustomLookAndFeelPainter.DefaultDateNavigatorHeaderHighlightTextColor: TColor;
begin
  if IsWinVistaOrLater then
    Result := clHotLight
  else
    Result := clHighlightText;
end;

procedure TcxCustomLookAndFeelPainter.DoDrawScaledScrollBarPart(ACanvas: TcxCanvas; AHorizontal: Boolean; R: TRect;
  APart: TcxScrollBarPart; AState: TcxButtonState; AScaleFactor: TdxScaleFactor);
begin
end;

procedure TcxCustomLookAndFeelPainter.DrawBackground(ACanvas: TcxCanvas; const ARect: TRect;
  ATransparent: Boolean; ABackgroundColor: TColor; const ABackgroundBitmap: TBitmap);
begin
  if not ATransparent then
    ACanvas.FillRect(ARect, ABackgroundColor)
  else
    if ABackgroundBitmap <> nil then
      ACanvas.FillRect(ARect, ABackgroundBitmap);
end;

procedure TcxCustomLookAndFeelPainter.DrawButtonArrow(ACanvas: TcxCanvas; const R: TRect; AColor: TColor);
var
  P: array[0..2] of TPoint;

  procedure CalculatePoints;
  var
    ASize: TPoint;

    function _GetSize: TPoint;
    begin
      Result.X := (R.Right - R.Left) div 2;
      if not Odd(Result.X) then Inc(Result.X);
      Result.Y := Result.X div 2 + 1;
    end;

  begin
    with R do
    begin
      ASize := _GetSize;
      P[0] := Point((Left + Right - ASize.X) div 2, MulDiv(Top + Bottom - ASize.Y, 1, 2));
      P[1] := Point(P[0].X + ASize.X - 1, P[0].Y);
      P[2] := Point(P[0].X + ASize.X div 2, P[0].Y + ASize.Y - 1);
    end;
  end;

begin
  CalculatePoints;
  ACanvas.Brush.Color := AColor;
  ACanvas.Pen.Color := AColor;
  ACanvas.Polygon(P);
end;

procedure TcxCustomLookAndFeelPainter.DrawContent(ACanvas: TcxCanvas;
  const ABounds, ATextAreaBounds: TRect; AState: Integer; AAlignmentHorz: TAlignment;
  AAlignmentVert: TcxAlignmentVert; AMultiLine, AShowEndEllipsis: Boolean;
  const AText: string; AFont: TFont; ATextColor, ABkColor: TColor;
  AOnDrawBackground: TcxDrawBackgroundEvent = nil; AIsFooter: Boolean = False);
const
  MultiLines: array[Boolean] of Integer = (cxSingleLine, cxWordBreak);
  ShowEndEllipsises: array[Boolean] of Integer = (0, cxShowEndEllipsis);
begin
  with ACanvas do
  begin
    if not Assigned(AOnDrawBackground) or not AOnDrawBackground(ACanvas, ABounds) then
    begin
      SetBrushColor(ABkColor);
      FillRect(ABounds);
    end;
    if AText <> '' then
    begin
      Brush.Style := bsClear;
      Font := AFont;
      Font.Color := ATextColor;
      DrawText(AText, ATextAreaBounds, cxAlignmentsHorz[AAlignmentHorz] or
        cxAlignmentsVert[AAlignmentVert] or MultiLines[AMultiLine] or
        ShowEndEllipsises[AShowEndEllipsis]);
      Brush.Style := bsSolid;
    end;
  end;
end;

procedure TcxCustomLookAndFeelPainter.DrawExpandButtonCross(ACanvas: TcxCanvas;
  const R: TRect; AExpanded: Boolean; AColor: TColor; AScaleFactor: TdxScaleFactor);
var
  ASize, X, Y: Integer;
begin
  ASize := R.Right - R.Left - AScaleFactor.Apply(2) * 2;
  X := GetRangeCenter(R.Left, R.Right);
  Y := GetRangeCenter(R.Top, R.Bottom);

  ACanvas.Brush.Color := AColor;
  ACanvas.FillRect(Rect(X - ASize div 2, Y, X + ASize div 2 + 1, Y + 1));
  if not AExpanded then
    ACanvas.FillRect(Rect(X, Y - ASize div 2, X + 1, Y + ASize div 2 + 1));
end;

procedure TcxCustomLookAndFeelPainter.DrawIndicatorCustomizationMark(
  ACanvas: TcxCanvas; const R: TRect; AColor: TColor);
begin
  DrawScaledIndicatorCustomizationMark(ACanvas, R, AColor, dxDefaultScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DrawIndicatorImage(ACanvas: TcxCanvas; const R: TRect; AKind: TcxIndicatorKind);
begin
  DrawScaledIndicatorImage(ACanvas, R, AKind, dxSystemScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DrawScaledIndicatorImage(
  ACanvas: TcxCanvas; const R: TRect; AKind: TcxIndicatorKind; AScaleFactor: TdxScaleFactor);
var
  ADestRect: TRect;
begin
  if AKind <> ikNone then
  begin
    ADestRect := R;
    cxRightToLeftDependentDraw(ACanvas.Handle, ADestRect, ACanvas.UseRightToLeftAlignment and (AKind in [ikArrow, ikMultiArrow]),
      procedure
      begin
        cxDrawImage(ACanvas, ADestRect, nil, cxIndicatorImages, Ord(AKind) - 1, ifmNormal, idmNormal, False, nil, AScaleFactor);
      end);
  end;
end;

procedure TcxCustomLookAndFeelPainter.DrawMonthHeaderLeftArrow(ACanvas: TcxCanvas; const ABounds: TRect; AColor: TColor);
var
  R: TRect;
begin
  R := ABounds;
  R.Right := R.Left + cxRectHeight(R);
  DrawArrow(ACanvas, R, adLeft, AColor);
end;

procedure TcxCustomLookAndFeelPainter.DrawMonthHeaderRightArrow(ACanvas: TcxCanvas; const ABounds: TRect; AColor: TColor);
var
  R: TRect;
begin
  R := ABounds;
  R.Left := R.Right - cxRectHeight(R);
  DrawArrow(ACanvas, R, adRight, AColor);
end;

procedure TcxCustomLookAndFeelPainter.DrawMonthHeaderArrows(ACanvas: TcxCanvas; const ABounds: TRect;
  AArrows: TcxArrowDirections; ASideWidth: Integer; AColor: TColor);
begin
  if adLeft in AArrows then
    DrawMonthHeaderLeftArrow(ACanvas, ABounds, AColor);
  if adRight in AArrows then
    DrawMonthHeaderRightArrow(ACanvas, ABounds, AColor);
end;

procedure TcxCustomLookAndFeelPainter.DrawSortingArrow(ACanvas: TcxCanvas; const R: TRect;
  AColor1, AColor2: TColor; AAscendingSorting: Boolean; AScaleFactor: TdxScaleFactor);
var
  Sign, AWidth, AHeight, X, Y, ALeftSide, ARightSide: Integer;
begin
  Sign := 2 * Byte(AAscendingSorting) - 1;
  with ScaledSortingMarkSize(AScaleFactor) do
  begin
    AWidth := X;
    AHeight := Y;
  end;

  X := (R.Left + R.Right) div 2;
  if not Odd(AWidth) then Dec(X);
  if AAscendingSorting then
    Y := (R.Top + R.Bottom - AHeight) div 2
  else
    Y := (R.Top + R.Bottom + AHeight) div 2 - 1;

  ALeftSide := AWidth div 2;
  if not Odd(AWidth) then Dec(ALeftSide);
  ARightSide := AWidth div 2;

  ACanvas.Pen.Color := AColor2;
  ACanvas.MoveTo(X + ARightSide, Y + Sign * (AHeight - 2));
  ACanvas.LineTo(X + ARightSide - ALeftSide, Y);
  ACanvas.LineTo(X + ARightSide, Y + Sign * (AHeight - 1));
  if not AAscendingSorting then
    ACanvas.Pen.Color := AColor1;
  ACanvas.LineTo(X - ALeftSide, Y + Sign * (AHeight - 1));
  if AAscendingSorting then
    ACanvas.Pen.Color := AColor1;
  ACanvas.LineTo(X, Y);
  ACanvas.LineTo(X - ALeftSide, Y + Sign * (AHeight - Ord(Odd(AWidth))));
end;

procedure TcxCustomLookAndFeelPainter.DrawSummarySortingArrow(ACanvas: TcxCanvas;
  const R: TRect; AColor1, AColor2: TColor; AAscendingSorting: Boolean; AScaleFactor: TdxScaleFactor);
var
  Sign, AWidth, AHeight, X, Y, ALeftSide, ARightSide: Integer;
begin
  Sign := 2 * Byte(AAscendingSorting) - 1;
  with ScaledSummarySortingMarkSize(AScaleFactor) do
  begin
    AWidth := X;
    AHeight := Y;
  end;

  X := (R.Left + R.Right) div 2;
  if not Odd(AWidth) then Dec(X);
  ALeftSide := AWidth div 2;
  if not Odd(AWidth) then Dec(ALeftSide);
  ARightSide := AWidth div 2;

  Y := R.Top;
  ACanvas.Pen.Color := AColor1;
  ACanvas.MoveTo(X - ALeftSide, Y);
  ACanvas.LineTo(X + ARightSide + 1, Y);
  if AColor1 <> AColor2 then
  begin
    Inc(Y);
    ACanvas.Pen.Color := AColor2;
    ACanvas.MoveTo(X - ALeftSide, Y);
    ACanvas.LineTo(X + ARightSide + 1, Y);
  end;

  Dec(AHeight, 2);
  if AAscendingSorting then
    Y := (R.Top + R.Bottom - AHeight) div 2 + 2
  else
    Y := (R.Top + R.Bottom + AHeight) div 2 + 1;

  ACanvas.Pen.Color := AColor2;
  ACanvas.MoveTo(X + ARightSide, Y + Sign * (AHeight - 2));
  ACanvas.LineTo(X + ARightSide - ALeftSide, Y);
  ACanvas.LineTo(X + ARightSide, Y + Sign * (AHeight - 1));
  if not AAscendingSorting then
    ACanvas.Pen.Color := AColor1;
  ACanvas.LineTo(X - ALeftSide, Y + Sign * (AHeight - 1));
  if AAscendingSorting then
    ACanvas.Pen.Color := AColor1;
  ACanvas.LineTo(X, Y);
  ACanvas.LineTo(X - ALeftSide, Y + Sign * (AHeight - Ord(Odd(AWidth))));
end;

function TcxCustomLookAndFeelPainter.FooterCellContentBounds(const ABounds: TRect): TRect;
begin
  Result := ABounds;
  InflateRect(Result, -FooterCellBorderSize, -FooterCellBorderSize);
end;

function TcxCustomLookAndFeelPainter.FooterCellTextAreaBounds(const ABounds: TRect): TRect;
begin
  Result := FooterCellContentBounds(ABounds);
  InflateRect(Result, -cxTextOffset, -cxTextOffset);
end;

function TcxCustomLookAndFeelPainter.GetArrowDirection(AHorizontal: Boolean;
  APart: TcxScrollBarPart): TcxArrowDirection;
const
  ArrowKind: array[Boolean, Boolean] of TcxArrowDirection = ((adUp, adDown), (adLeft, adRight));
begin
  Result := ArrowKind[AHorizontal, APart <> sbpLineUp];
end;

function TcxCustomLookAndFeelPainter.GetFilterSmartTagColor(
  AState: TcxFilterSmartTagState; AIsFilterActive: Boolean): TColor;
begin
  case AState of
    fstsNormal, fstsParentHot:
      if AIsFilterActive then
        Result := $FF2600
      else
        Result := $606060;

    fstsHot:
      Result := $4848484;
    fstsPressed:
      Result := $303030;
  else
    Result := $606060;
  end;
end;

function TcxCustomLookAndFeelPainter.GetSeparatorBounds(
  const R: TRect; AWidth: Integer; AIsVertical: Boolean): TRect;
var
  P: TPoint;
begin
  P := cxRectCenter(R);
  Result := R;
  if AIsVertical then
  begin
    Result.Left := P.X - AWidth div 2;
    Result.Right := Result.Left + AWidth;
  end
  else
  begin
    Result.Top := P.Y - AWidth div 2;
    Result.Bottom := Result.Top + AWidth;
  end;
end;

procedure TcxCustomLookAndFeelPainter.ReleaseImageResources;
begin
  FreeAndNil(FLookAndFeelPainterDetailsCache);
  FreeAndNil(FSmartTagGlyph);
  FreeAndNil(FClockFace);
  FreeAndNil(FClockGlass);
  FreeAndNil(FNavigationBarCustomizationButton);
  FreeAndNil(FBackButton);
  FreeAndNil(FMapPushpin);
  FreeAndNil(FRatingControlIndicator);
  FreeAndNil(FFixedGroupIndicator);
  FreeAndNil(FSearchButtonGlyph);
  FreeAndNil(FCalendarButtonGlyph);
end;

function TcxCustomLookAndFeelPainter.GetPainterData(var AData): Boolean;
begin
  Result := False;
end;

function TcxCustomLookAndFeelPainter.GetPainterDetails(var ADetails): Boolean;
begin
  Result := GetLookAndFeelPainterDetails <> nil;
  if Result then
    TObject(ADetails) := GetLookAndFeelPainterDetails;
end;

function TcxCustomLookAndFeelPainter.IsInternalPainter: Boolean;
begin
  Result := False;
end;

function TcxCustomLookAndFeelPainter.LookAndFeelName: string;
begin
  Result := '';
end;

function TcxCustomLookAndFeelPainter.LookAndFeelStyle: TcxLookAndFeelStyle;
begin
  Result := lfsStandard;
end;

function TcxCustomLookAndFeelPainter.NeedRedrawOnResize: Boolean;
begin
  Result := False;
end;

function TcxCustomLookAndFeelPainter.DefaultContentColor: TColor;
begin
  Result := clWindow;
end;

function TcxCustomLookAndFeelPainter.DefaultContentOddColor: TColor;
begin
  Result := DefaultContentColor;
end;

function TcxCustomLookAndFeelPainter.DefaultContentEvenColor: TColor;
begin
  Result := DefaultContentColor;
end;

function TcxCustomLookAndFeelPainter.DefaultContentTextColor: TColor;
begin
  Result := clWindowText;
end;

function TcxCustomLookAndFeelPainter.DefaultControlColor: TColor;
begin
  Result := clBtnFace;
end;

function TcxCustomLookAndFeelPainter.DefaultControlTextColor: TColor;
begin
  Result := clBtnText;
end;

function TcxCustomLookAndFeelPainter.DefaultSchedulerBackgroundColor: TColor;
begin
  Result := clWindow;
end;

function TcxCustomLookAndFeelPainter.DefaultSchedulerTextColor: TColor;
begin
  Result := clWindowText;
end;

function TcxCustomLookAndFeelPainter.DefaultDateNavigatorTextColor: TColor;
begin
   Result := clWindowText;
end;

function TcxCustomLookAndFeelPainter.DefaultDateNavigatorHolydayTextColor: TColor;
begin
  Result := clRed;
end;

function TcxCustomLookAndFeelPainter.DefaultDateNavigatorInactiveTextColor: TColor;
begin
  Result := clGrayText;
end;

function TcxCustomLookAndFeelPainter.DefaultDateNavigatorSeparator1Color: TColor;
begin
  Result := clBtnShadow;
end;

function TcxCustomLookAndFeelPainter.DefaultDateNavigatorSeparator2Color: TColor;
begin
  Result := clWindow;
end;

function TcxCustomLookAndFeelPainter.DefaultDateNavigatorTodayFrameColor: TColor;
begin
  Result := clMaroon;
end;

function TcxCustomLookAndFeelPainter.DefaultDateNavigatorTodayTextColor: TColor;
begin
  Result := DefaultDateNavigatorTodayTextColor(False);
end;

function TcxCustomLookAndFeelPainter.DefaultDateNavigatorTodayTextColor(ASelected: Boolean): TColor;
begin
  if ASelected then
    Result := clDefault
  else
    Result := clNavy;
end;

function TcxCustomLookAndFeelPainter.DefaultDateNavigatorContentColor: TColor;
begin
  Result := DefaultEditorBackgroundColor(False);
  if Result = clDefault then
    Result := DefaultContentColor;
end;

function TcxCustomLookAndFeelPainter.DefaultDateNavigatorHeaderColor: TColor;
begin
  Result := clBtnFace;
end;

function TcxCustomLookAndFeelPainter.DefaultDateNavigatorHeaderTextColor(
  AIsHighlight: Boolean): TColor;
begin
  if AIsHighlight then
    Result := DefaultDateNavigatorHeaderHighlightTextColor
  else
    Result := DefaultHeaderTextColor;
end;

function TcxCustomLookAndFeelPainter.DefaultDateNavigatorSelectionColor: TColor;
begin
  Result := clHighlight;
end;

function TcxCustomLookAndFeelPainter.DefaultDateNavigatorSelectionTextColor: TColor;
begin
  Result := clHighlightText;
end;

function TcxCustomLookAndFeelPainter.DefaultEditorBackgroundColor(AIsDisabled: Boolean): TColor;
begin
  Result := DefaultEditorBackgroundColorEx(cxEditStateColorKindMap[not AIsDisabled]);
end;

function TcxCustomLookAndFeelPainter.DefaultEditorTextColor(AIsDisabled: Boolean): TColor;
begin
  Result := DefaultEditorTextColorEx(cxEditStateColorKindMap[not AIsDisabled]);
end;

function TcxCustomLookAndFeelPainter.DefaultEditorBackgroundColorEx(AKind: TcxEditStateColorKind): TColor;
begin
  Result := clDefault;
end;

function TcxCustomLookAndFeelPainter.DefaultEditorTextColorEx(AKind: TcxEditStateColorKind): TColor;
begin
  Result := clDefault;
end;

function TcxCustomLookAndFeelPainter.DefaultFilterBoxColor: TColor;
begin
  Result := clBtnShadow;
end;

function TcxCustomLookAndFeelPainter.DefaultFilterBoxTextColor: TColor;
begin
  Result := clBtnHighlight;
end;

function TcxCustomLookAndFeelPainter.DefaultFixedSeparatorColor: TColor;
begin
  Result := clBtnText;
end;

function TcxCustomLookAndFeelPainter.DefaultFooterColor: TColor;
begin
  Result := clBtnFace;
end;

function TcxCustomLookAndFeelPainter.DefaultFooterTextColor: TColor;
begin
  Result := clBtnText;
end;

function TcxCustomLookAndFeelPainter.DefaultGridDetailsSiteColor: TColor;
begin
  Result := clBtnFace;
end;

function TcxCustomLookAndFeelPainter.DefaultGridLineColor: TColor;
begin
  Result := clBtnShadow;
end;

function TcxCustomLookAndFeelPainter.DefaultGroupColor: TColor;
begin
  Result := clBtnFace;
end;

function TcxCustomLookAndFeelPainter.DefaultGroupContentOffsets: TRect;
begin
  Result := cxNullRect;
end;

function TcxCustomLookAndFeelPainter.DefaultGroupTextColor: TColor;
begin
  Result := clBtnText;
end;

function TcxCustomLookAndFeelPainter.DefaultGroupByBoxColor: TColor;
begin
  Result := clBtnShadow;
end;

function TcxCustomLookAndFeelPainter.DefaultGroupByBoxTextColor: TColor;
begin
  Result := clBtnFace;
end;

function TcxCustomLookAndFeelPainter.DefaultHeaderColor: TColor;
begin
  Result := clBtnFace;
end;

function TcxCustomLookAndFeelPainter.DefaultHeaderTextColor: TColor;
begin
  Result := clBtnText;
end;

function TcxCustomLookAndFeelPainter.DefaultHeaderBackgroundColor: TColor;
begin
  Result := clBtnShadow;
end;

function TcxCustomLookAndFeelPainter.DefaultHeaderBackgroundTextColor: TColor;
begin
  Result := clBtnFace;
end;

function TcxCustomLookAndFeelPainter.DefaultHyperlinkTextColor: TColor;
begin
  Result := clBlue;
end;

function TcxCustomLookAndFeelPainter.DefaultInactiveColor: TColor;
begin
  Result := clBtnFace;
end;

function TcxCustomLookAndFeelPainter.DefaultInactiveTextColor: TColor;
begin
  Result := clBtnText;
end;

function TcxCustomLookAndFeelPainter.DefaultPreviewTextColor: TColor;
begin
  Result := clBlue;
end;

function TcxCustomLookAndFeelPainter.DefaultRecordSeparatorColor: TColor;
begin
  Result := DefaultGridLineColor;
end;

function TcxCustomLookAndFeelPainter.DefaultSizeGripAreaColor: TColor;
begin
  Result := clBtnFace;
end;

function TcxCustomLookAndFeelPainter.DefaultTreeListGridLineColor: TColor;
begin
  Result := DefaultGridLineColor;
end;

function TcxCustomLookAndFeelPainter.DefaultTreeListTreeLineColor: TColor;
begin
  Result := DefaultGridLineColor;
end;

function TcxCustomLookAndFeelPainter.DefaultVGridCategoryColor: TColor;
begin
  Result := DefaultHeaderColor;
end;

function TcxCustomLookAndFeelPainter.DefaultVGridCategoryTextColor: TColor;
begin
  Result := clDefault;
end;

function TcxCustomLookAndFeelPainter.DefaultVGridContentColor: TColor;
begin
  Result := GridLikeControlContentColor;
end;

function TcxCustomLookAndFeelPainter.DefaultVGridContentEvenColor: TColor;
begin
  Result := GridLikeControlContentEvenColor;
end;

function TcxCustomLookAndFeelPainter.DefaultVGridContentOddColor: TColor;
begin
  Result := GridLikeControlContentOddColor;
end;

function TcxCustomLookAndFeelPainter.DefaultVGridContentTextColor: TColor;
begin
  Result := DefaultContentTextColor;
end;

function TcxCustomLookAndFeelPainter.DefaultVGridHeaderColor: TColor;
begin
  Result := DefaultHeaderColor;
end;

function TcxCustomLookAndFeelPainter.DefaultVGridHeaderTextColor: TColor;
begin
  Result := DefaultHeaderTextColor;
end;

function TcxCustomLookAndFeelPainter.DefaultVGridLineColor: TColor;
begin
  Result := clBlack; //todo
end;

function TcxCustomLookAndFeelPainter.DefaultVGridBandLineColor: TColor;
begin
  Result := DefaultVGridLineColor;
end;

function TcxCustomLookAndFeelPainter.DefaultSchedulerBorderColor: TColor;
begin
  Result := $9D9DA1;
end;

function TcxCustomLookAndFeelPainter.DefaultSchedulerContentColor(
  AResourceIndex: Integer): TColor;
begin
  Result := clDefault;
end;

function TcxCustomLookAndFeelPainter.DefaultSchedulerControlColor: TColor;
begin
  Result := clBtnFace;
end;

function TcxCustomLookAndFeelPainter.DefaultSchedulerDayHeaderColor: TColor;
begin
  Result := $F0F0F0;
end;

function TcxCustomLookAndFeelPainter.DefaultSchedulerDayHeaderBorderColor: TColor;
begin
  Result := $E1E1E1;
end;

function TcxCustomLookAndFeelPainter.DefaultSchedulerDayHeaderTextColor: TColor;
begin
  Result := $444444;
end;

function TcxCustomLookAndFeelPainter.DefaultSchedulerDateNavigatorArrowColor(AIsHighlight: Boolean): TColor;
begin
  if AIsHighlight then
    Result := DefaultDateNavigatorHeaderHighlightTextColor
  else
    Result := $5F5F5F;
end;

function TcxCustomLookAndFeelPainter.DefaultSchedulerTimeRulerBorderColor: TColor;
begin
  Result := DefaultSchedulerHeaderContainerBorderColor;
end;

function TcxCustomLookAndFeelPainter.DefaultSchedulerTimeRulerBorderColorClassic: TColor;
begin
  Result := clBtnShadow;
end;

function TcxCustomLookAndFeelPainter.DefaultSchedulerTimeRulerColor: TColor;
begin
  Result := DefaultSchedulerHeaderContainerBackgroundColor(False);
end;

function TcxCustomLookAndFeelPainter.DefaultSchedulerTimeRulerColorClassic: TColor;
begin
  Result := DefaultHeaderColor;
end;

function TcxCustomLookAndFeelPainter.DefaultSchedulerTimeRulerTextColor: TColor;
begin
  Result := $666666;
end;

function TcxCustomLookAndFeelPainter.DefaultSchedulerTimeRulerTextColorClassic: TColor;
begin
  Result := DefaultHeaderTextColor;
end;

function TcxCustomLookAndFeelPainter.DefaultSchedulerViewContentColor: TColor;
begin
  Result := $FFFFFF;
end;

function TcxCustomLookAndFeelPainter.DefaultSchedulerViewContentColorClassic: TColor;
begin
  Result := $D5FFFF;
end;

function TcxCustomLookAndFeelPainter.DefaultSchedulerEventColor(AIsAllDayEvent: Boolean): TColor;
const
  Map: array[Boolean] of TColor = ($D7D7D7, $D7D7D7);
begin
  Result := Map[AIsAllDayEvent];
end;

function TcxCustomLookAndFeelPainter.DefaultSchedulerEventColorClassic(AIsAllDayEvent: Boolean): TColor;
const
  Map: array[Boolean] of TColor = (clWindow, clWhite);
begin
  Result := Map[AIsAllDayEvent];
end;

function TcxCustomLookAndFeelPainter.DefaultSchedulerHeaderContainerAlternateBackgroundColor: TColor;
begin
  Result := dxOffice11SelectedDownColor1;
end;

function TcxCustomLookAndFeelPainter.DefaultSchedulerHeaderContainerBackgroundColor(ASelected: Boolean): TColor;
begin
  if ASelected then
    Result := DefaultSelectionColor
  else
    Result := $FFFFFF;
end;

function TcxCustomLookAndFeelPainter.DefaultSchedulerHeaderContainerTextColor(ASelected: Boolean): TColor;
begin
  if ASelected then
    Result := DefaultSelectionTextColor
  else
    Result := DefaultSchedulerDayHeaderTextColor;
end;

function TcxCustomLookAndFeelPainter.DefaultSchedulerHeaderContainerBorderColor: TColor;
begin
  Result := $E1E1E1;
end;

function TcxCustomLookAndFeelPainter.DefaultSchedulerNavigatorColor: TColor;
begin
  Result := clBtnFace;
end;

function TcxCustomLookAndFeelPainter.DefaultSchedulerSelectedEventBorderColor: TColor;
begin
  Result := clBlack;
end;

function TcxCustomLookAndFeelPainter.DefaultSchedulerViewSelectedTextColor: TColor;
begin
  Result := DefaultSelectionTextColor;
end;

function TcxCustomLookAndFeelPainter.DefaultSchedulerViewTextColor: TColor;
begin
  Result := clWindowText;
end;

function TcxCustomLookAndFeelPainter.DefaultSchedulerYearViewUnusedContentColor(
  AIsWorkTime: Boolean): TColor;
begin
  Result := clDefault;
end;

function TcxCustomLookAndFeelPainter.DefaultSelectionColor: TColor;
begin
  Result := clHighlight;
end;

function TcxCustomLookAndFeelPainter.DefaultSelectionTextColor: TColor;
begin
  Result := clHighlightText;
end;

function TcxCustomLookAndFeelPainter.DefaultSeparatorColor: TColor;
begin
  Result := clBtnFace;
end;

function TcxCustomLookAndFeelPainter.DefaultTabColor: TColor;
begin
  Result := clBtnFace;
end;

function TcxCustomLookAndFeelPainter.DefaultTabTextColor: TColor;
begin
  Result := clBtnText;
end;

function TcxCustomLookAndFeelPainter.DefaultTabsBackgroundColor: TColor;
begin
  Result := clWindow;
end;

function TcxCustomLookAndFeelPainter.DefaultRootTabsBackgroundColor: TColor;
begin
  Result := clBtnFace;
end;

function TcxCustomLookAndFeelPainter.DefaultTimeGridMajorScaleColor: TColor;
begin
  Result := clWhite;
end;

function TcxCustomLookAndFeelPainter.DefaultTimeGridMajorScaleTextColor: TColor;
begin
  Result := clBlack;
end;

function TcxCustomLookAndFeelPainter.DefaultTimeGridMinorScaleColor: TColor;
begin
  Result := clBtnFace;
end;

function TcxCustomLookAndFeelPainter.DefaultTimeGridMinorScaleTextColor: TColor;
begin
  Result := clBtnText;
end;

function TcxCustomLookAndFeelPainter.DefaultTimeGridSelectionBarColor: TColor;
begin
  Result := clWhite;
end;

function TcxCustomLookAndFeelPainter.DefaultChartDiagramValueBorderColor: TColor;
begin
  Result := clBlack;
end;

function TcxCustomLookAndFeelPainter.DefaultChartDiagramValueCaptionTextColor: TColor;
begin
  Result := clBlack;
end;

function TcxCustomLookAndFeelPainter.DefaultChartHistogramAxisColor: TColor;
begin
  Result := clBlack;
end;

function TcxCustomLookAndFeelPainter.DefaultChartHistogramGridLineColor: TColor;
begin
  Result := clcxLightGray;
end;

function TcxCustomLookAndFeelPainter.DefaultChartHistogramPlotColor: TColor;
begin
  Result := clWhite;
end;

function TcxCustomLookAndFeelPainter.DefaultChartPieDiagramSeriesSiteBorderColor: TColor;
begin
  Result := clBtnShadow;
end;

function TcxCustomLookAndFeelPainter.DefaultChartPieDiagramSeriesSiteCaptionColor: TColor;
begin
  Result := clSkyBlue;
end;

function TcxCustomLookAndFeelPainter.DefaultChartPieDiagramSeriesSiteCaptionTextColor: TColor;
begin
  Result := clNavy;
end;

function TcxCustomLookAndFeelPainter.DefaultChartToolBoxDataLevelInfoBorderColor: TColor;
begin
  Result := clBtnShadow;
end;

function TcxCustomLookAndFeelPainter.DefaultChartToolBoxItemSeparatorColor: TColor;
begin
  Result := clBtnShadow;
end;

function TcxCustomLookAndFeelPainter.DefaultLayoutViewCaptionColor(AState: TcxButtonState): TColor;
begin
  case AState of
    cxbsHot:
      Result := DefaultSelectionColor;
    cxbsDisabled:
      Result := clInactiveCaption;
  else
    Result := DefaultGroupColor;
  end;
end;

function TcxCustomLookAndFeelPainter.DefaultLayoutViewCaptionTextColor(ACaptionPosition: TcxGroupBoxCaptionPosition; AState: TcxButtonState): TColor;
begin
  Result := DefaultLayoutViewContentTextColor(AState);
end;

function TcxCustomLookAndFeelPainter.DefaultLayoutViewContentColor: TColor;
begin
  Result := DefaultContentColor;
end;

function TcxCustomLookAndFeelPainter.DefaultLayoutViewContentTextColor(AState: TcxButtonState): TColor;
begin
  case AState of
    cxbsPressed:
      Result := DefaultSelectionTextColor;
    cxbsDisabled:
      Result := clInactiveCaptionText;
  else
    Result := DefaultContentTextColor;
  end;
end;

function TcxCustomLookAndFeelPainter.DefaultGridOptionsTreeViewCategoryColor(ASelected: Boolean): TColor;
begin
  if ASelected then
    Result := clHighlight
  else
    Result := clBtnShadow;
end;

function TcxCustomLookAndFeelPainter.DefaultGridOptionsTreeViewCategoryTextColor(ASelected: Boolean): TColor;
begin
  if ASelected then
    Result := clHighlightText
  else
    Result := clBtnText;
end;

function TcxCustomLookAndFeelPainter.GetColorGalleryGlyphFrameColor: TColor;
begin
  Result := $E2E4E6;
end;

procedure TcxCustomLookAndFeelPainter.DrawColorGalleryItemSelection(ACanvas: TcxCanvas; const R: TRect; AViewState: TdxGalleryItemViewState);
const
  AColor1 = $3694F2;
  AColor2 = $94E2FF;
  AColor3 = $1048Ef;
  AColor4 = $0022Ef;
begin
  if not (AViewState.Checked or AViewState.Hover or AViewState.Pressed or AViewState.Focused) then
    Exit;

  if AViewState.Checked then
    ACanvas.FrameRect(R, AColor3)
  else
    if AViewState.Pressed then
      ACanvas.FrameRect(R, AColor4)
    else
      if AViewState.Hover or AViewState.Focused then
         ACanvas.FrameRect(R, AColor1);

  ACanvas.FrameRect(cxRectInflate(R, -1, -1), AColor2);
end;

function TcxCustomLookAndFeelPainter.GetGalleryScaledGroupHeaderContentOffsets(AScaleFactor: TdxScaleFactor): TRect;
begin
  Result := AScaleFactor.Apply(cxRect(7, 2, 7, 2));
end;

function TcxCustomLookAndFeelPainter.GetGalleryGroupHeaderContentOffsets: TRect;
begin
  Result := GetGalleryScaledGroupHeaderContentOffsets(dxDefaultScaleFactor);
end;

function TcxCustomLookAndFeelPainter.GetGalleryGroupTextColor: TColor;
begin
  Result := clDefault;
end;

function TcxCustomLookAndFeelPainter.GetGalleryItemImageFrameColor: TColor;
begin
  Result := clBtnShadow;
end;

function TcxCustomLookAndFeelPainter.GetGalleryItemCaptionTextColor(const AState: TdxGalleryItemViewState): TColor;
begin
  Result := clWindowText;
end;

function TcxCustomLookAndFeelPainter.GetGalleryItemColorPalette(const AState: TdxGalleryItemViewState): IdxColorPalette;
begin
  Result := nil;
end;

function TcxCustomLookAndFeelPainter.GetGalleryItemDescriptionTextColor(const AState: TdxGalleryItemViewState): TColor;
begin
  Result := clGrayText;
end;

procedure TcxCustomLookAndFeelPainter.DrawGalleryBackground(ACanvas: TcxCanvas; const ABounds: TRect);
const
  dxGalleryBackgroundColor: TColor = clWhite;
begin
  ACanvas.FillRect(ABounds, dxGalleryBackgroundColor);
end;

procedure TcxCustomLookAndFeelPainter.DrawGalleryGroupHeader(ACanvas: TcxCanvas; const ABounds: TRect);
begin
  DrawScaledHeader(ACanvas, ABounds, cxNullRect, [], cxBordersAll, cxbsNormal,
    taLeftJustify, vaCenter, False, True, '', nil, clNone, DefaultHeaderColor, dxDefaultScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DrawGalleryItemSelection(
  ACanvas: TcxCanvas; const R: TRect; AViewState: TdxGalleryItemViewState);
const
  dxGalleryBasicBackgroundColor: TColor = $582801;
  dxGalleryFocusRectColor: TColor = $CEA27D;
  dxGalleryMouseHoverFrameRectColor: TColor = $FBD6B8;

  function GetAlphaValue: Byte;
  begin
    Result := 0;
    if AViewState.Checked then
    begin
      if AViewState.Pressed then
        Result := $80
      else
        if AViewState.Hover then
          Result := $60
        else
          Result := $10
    end
    else
    begin
      if AViewState.Pressed then
        Result := $40
      else
        if AViewState.Hover then
          Result := $20
    end;
  end;

  function GetFrameColor: TColor;
  begin
    Result := clDefault;
    if AViewState.Focused then
      Result := dxGalleryFocusRectColor
    else
      if AViewState.Checked then
      begin
        if AViewState.Pressed then
          Result := dxGetColorTint(dxGalleryFocusRectColor, -20)
        else
          if AViewState.Hover then
            Result := dxGetColorTint(dxGalleryFocusRectColor, 10)
          else
            Result := dxGetColorTint(dxGalleryFocusRectColor, 80);
      end
      else
      begin
        if AViewState.Pressed then
          Result := dxGetColorTint(dxGalleryFocusRectColor, 20)
        else
          if AViewState.Hover then
            Result := dxGalleryMouseHoverFrameRectColor;
      end;
  end;

var
  AAlphaValue: Byte;
begin
  AAlphaValue := GetAlphaValue;
  if AAlphaValue <> 0 then
    dxGpFillRect(ACanvas.Handle, R, dxGalleryBasicBackgroundColor, AAlphaValue);
  if GetFrameColor <> clDefault then
    ACanvas.FrameRect(R, GetFrameColor, 2);
end;

function TcxCustomLookAndFeelPainter.DrawGalleryItemSelectionFirst: Boolean;
begin
  Result := False;
end;

procedure TcxCustomLookAndFeelPainter.DrawGalleryItemImageFrame(ACanvas: TcxCanvas; const R: TRect);
begin
  ACanvas.FrameRect(R, GetGalleryItemImageFrameColor);
end;

function TcxCustomLookAndFeelPainter.GetBackButton: TdxSmartImage;
begin
  if FBackButton = nil then
  begin
    FBackButton := TdxSmartImage.Create;
    FBackButton.LoadFromResource(HInstance, 'CX_BACKBUTTON', 'PNG');
  end;
  Result := FBackButton;
end;

function TcxCustomLookAndFeelPainter.GetCalendarButtonGlyph: TdxSmartImage;
begin
  if FCalendarButtonGlyph = nil then
  begin
    FCalendarButtonGlyph := TdxSmartImage.Create;
    FCalendarButtonGlyph.LoadFromResource(HInstance, 'CX_CALENDARBUTTON', 'SVG');
  end;
  Result := FCalendarButtonGlyph;
end;

function TcxCustomLookAndFeelPainter.GetClockFace: TdxSmartImage;
begin
  if FClockFace = nil then
  begin
    FClockFace := TdxSmartImage.Create;
    FClockFace.LoadFromResource(HInstance, 'CX_CLOCKFACE', 'PNG');
  end;
  Result := FClockFace;
end;

function TcxCustomLookAndFeelPainter.GetClockGlass: TdxSmartImage;
begin
  if FClockGlass = nil then
  begin
    FClockGlass := TdxSmartImage.Create;
    FClockGlass.LoadFromResource(HInstance, 'CX_CLOCKGLASS', 'PNG');
  end;
  Result := FClockGlass;
end;

function TcxCustomLookAndFeelPainter.GetFixedGroupIndicator: TdxSmartImage;
begin
  if FFixedGroupIndicator = nil then
  begin
    FFixedGroupIndicator := TdxSmartImage.Create;
    FFixedGroupIndicator.LoadFromResource(HInstance, 'CX_FIXEDGROUPINDICATOR', 'PNG');
  end;
  Result := FFixedGroupIndicator;
end;

function TcxCustomLookAndFeelPainter.GetMapPushpin: TdxSmartImage;
begin
  if FMapPushpin = nil then
  begin
    FMapPushpin := TdxSmartImage.Create;
    FMapPushpin.LoadFromResource(HInstance, 'DX_MAPPUSHPIN', 'PNG');
  end;
  Result := FMapPushpin;
end;

function TcxCustomLookAndFeelPainter.GetNavigationBarCustomizationButton: TdxSmartImage;
begin
  if FNavigationBarCustomizationButton = nil then
  begin
    FNavigationBarCustomizationButton := TdxSmartImage.Create;
    FNavigationBarCustomizationButton.LoadFromResource(HInstance, 'DX_NAVIGATIONBARCUSTOMIZATIONBUTTON', 'PNG');
  end;
  Result := FNavigationBarCustomizationButton;
end;

function TcxCustomLookAndFeelPainter.GetRangeTrackBarThumbDrawRect(
  const R: TRect; ATicks: TcxTrackBarTicksAlign; AHorizontal: Boolean): TRect;
begin
  Result := R;
  if ATicks in [tbtaUp, tbtaDown] then
    if AHorizontal then
      Dec(Result.Right)
    else
      Dec(Result.Bottom);
end;

function TcxCustomLookAndFeelPainter.GetRatingControlIndicator: TdxSmartImage;
begin
  if FRatingControlIndicator = nil then
  begin
    FRatingControlIndicator := TdxSmartImage.Create;
    FRatingControlIndicator.LoadFromResource(HInstance, 'CX_RATINGCONTROLINDICATOR', 'PNG');
  end;
  Result := FRatingControlIndicator;
end;

function TcxCustomLookAndFeelPainter.GetSearchButtonGlyph: TdxSmartImage;
begin
  if FSearchButtonGlyph = nil then
  begin
    FSearchButtonGlyph := TdxSmartImage.Create;
    FSearchButtonGlyph.LoadFromResource(HInstance, 'DX_SEARCHBUTTONGLYPH', 'PNG');
  end;
  Result := FSearchButtonGlyph;
end;

function TcxCustomLookAndFeelPainter.GetBackButtonSize: TSize;
begin
  Result := GetScaledBackButtonSize(dxSystemScaleFactor);
end;

function TcxCustomLookAndFeelPainter.GetScaledBackButtonSize(AScaleFactor: TdxScaleFactor): TSize;
begin
  Result := AScaleFactor.Apply(cxSize(BackButton.Width, BackButton.Height div 4));
end;

procedure TcxCustomLookAndFeelPainter.DrawBackButton(ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState);
begin
  DrawScaledBackButton(ACanvas, R, AState, dxSystemScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DrawScaledBackButton(
  ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState; AScaleFactor: TdxScaleFactor);
const
  OffsetMap: array[TcxButtonState] of Integer = (0, 0, 1, 2, 3);
var
  ASourceRect: TRect;
begin
  ASourceRect := cxRectSetHeight(BackButton.ClientRect, GetScaledBackButtonSize(dxDefaultScaleFactor).cy);
  ASourceRect := cxRectOffset(ASourceRect, 0, OffsetMap[AState] * GetScaledBackButtonSize(dxDefaultScaleFactor).cy);
  BackButton.StretchDraw(ACanvas.Handle, R, ASourceRect);
end;

procedure TcxCustomLookAndFeelPainter.DrawBevelFrame(
  ACanvas: TcxCanvas; const R: TRect; AColor1, AColor2: TColor; ABoxStyle: Boolean);
var
  ABorderWidth: Integer;
  ARect: TRect;
begin
  ABorderWidth := 1;{AScaleFactor.Apply(1);}
  if AColor2 = clNone then
    ACanvas.FrameRect(R, AColor1, ABorderWidth)
  else
    if ABoxStyle then
      ACanvas.DrawComplexFrame(R, AColor1, AColor2, cxBordersAll, ABorderWidth)
    else
    begin
      ARect := cxRectInflate(R, -ABorderWidth, -ABorderWidth, 0, 0);
      ACanvas.FrameRect(ARect, AColor2, ABorderWidth);
      ACanvas.FrameRect(cxRectOffset(ARect, -ABorderWidth, -ABorderWidth), AColor1, ABorderWidth);
    end;
end;

procedure TcxCustomLookAndFeelPainter.DrawBevelLine(ACanvas: TcxCanvas;
  const R: TRect; AColor1, AColor2: TColor; AIsVertical: Boolean);
const
  BordersMap: array[Boolean] of TcxBorders = ([bTop, bBottom], [bLeft, bRight]);
begin
  if AColor2 <> clNone then
    ACanvas.DrawComplexFrame(R, AColor1, AColor2, BordersMap[AIsVertical])
  else
    ACanvas.FillRect(R, AColor1);
end;

procedure TcxCustomLookAndFeelPainter.DrawBevelShape(
  ACanvas: TcxCanvas; const R: TRect; AShape: TdxBevelShape; AStyle: TdxBevelStyle);

  procedure PrepareShapeColors(out AColor1, AColor2: TColor);
  begin
    GetBevelShapeColors(AColor1, AColor2);
    if AStyle = dxbsRaised then
      cxExchangeColors(AColor1, AColor2);
    if AColor1 = clNone then
      cxExchangeColors(AColor1, AColor2);
  end;

var
  AColor1, AColor2: TColor;
  AMinSize: TSize;
begin
  AMinSize := GetBevelMinimalShapeSize(AShape);
  PrepareShapeColors(AColor1, AColor2);
  case AShape of
    dxbsLineTop:
      DrawBevelLine(ACanvas, cxRectSetHeight(R, AMinSize.cy), AColor1, AColor2, False);
    dxbsLineBottom:
      DrawBevelLine(ACanvas, cxRectSetBottom(R, R.Bottom, AMinSize.cy), AColor1, AColor2, False);
    dxbsLineLeft:
      DrawBevelLine(ACanvas, cxRectSetWidth(R, AMinSize.cx), AColor1, AColor2, True);
    dxbsLineRight:
      DrawBevelLine(ACanvas, cxRectSetRight(R, R.Right, AMinSize.cx), AColor1, AColor2, True);
    dxbsLineCenteredHorz:
      DrawBevelLine(ACanvas, cxRectCenterHorizontally(R, AMinSize.cx), AColor1, AColor2, True);
    dxbsLineCenteredVert:
      DrawBevelLine(ACanvas, cxRectCenterVertically(R, AMinSize.cy), AColor1, AColor2, False);
    dxbsBox, dxbsFrame:
      DrawBevelFrame(ACanvas, R, AColor1, AColor2, AShape = dxbsBox);
  end;
end;

function TcxCustomLookAndFeelPainter.GetBevelMinimalShapeSize(AShape: TdxBevelShape): TSize;
begin
  Result := cxSize(2);
end;

procedure TcxCustomLookAndFeelPainter.GetBevelShapeColors(out AColor1, AColor2: TColor);
begin
  AColor1 := clBtnShadow;
  AColor2 := clBtnHighlight;
end;

procedure TcxCustomLookAndFeelPainter.DrawCalendarDateCellSelection(
  ACanvas: TcxCanvas; const ARect: TRect; AStates: TcxCalendarElementStates);
begin
  if cesMarked in AStates then
    DrawDateNavigatorTodayCellSelection(ACanvas, ARect);
end;

procedure TcxCustomLookAndFeelPainter.DrawModernCalendarArrow(ACanvas: TcxCanvas;
  const ARect: TRect; ADirection: TcxArrowDirection; AState: TcxCalendarElementState);
begin
  DrawScaledModernCalendarArrow(ACanvas, ARect, ADirection, AState, dxSystemScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DrawScaledModernCalendarArrow(ACanvas: TcxCanvas;
  const ARect: TRect; ADirection: TcxArrowDirection; AState: TcxCalendarElementState; AScaleFactor: TdxScaleFactor);
var
  P: TcxArrowPoints;
  R: TRect;
  AColor: TColor;
begin
  R := ARect;
  Dec(R.Bottom);
  CalculateArrowPoints(R, P, ADirection, False);
  if AState in [cesHot, cesPressed] then
    AColor := GetModernCalendarSelectedTextColor
  else
    if AState = cesDisabled then
      AColor := clGrayText
    else
      AColor := clBtnText;
  ACanvas.Brush.Color := AColor;
  ACanvas.Pen.Color := AColor;
  ACanvas.Polygon(P);
end;

procedure TcxCustomLookAndFeelPainter.DrawModernCalendarClock(ACanvas: TcxCanvas;
  const ARect: TRect; ADateTime: TDateTime; ABackgroundColor: TColor; AScaleFactor: TdxScaleFactor);
var
  ABitmap: TcxBitmap;
begin
  ABitmap := TcxBitmap.CreateSize(ARect);
  try
    if ABackgroundColor = clNone then
      cxBitBlt(ABitmap.cxCanvas.Handle, ACanvas.Handle, ARect, cxNullPoint, SRCCOPY)
    else
      ABitmap.cxCanvas.FillRect(ARect, ABackgroundColor);

    ABitmap.cxCanvas.StretchDraw(ARect, ClockFace);
    DrawScaledModernClockHands(ABitmap.cxCanvas, ARect, ADateTime, $464646, AScaleFactor);
    ABitmap.cxCanvas.StretchDraw(ARect, ClockGlass);
    cxBitBlt(ACanvas.Handle, ABitmap.cxCanvas.Handle, ARect, cxNullPoint, SRCCOPY)
  finally
    ABitmap.Free;
  end;
end;

procedure TcxCustomLookAndFeelPainter.DrawModernCalendarDateCellSelection(
  ACanvas: TcxCanvas; const ARect: TRect; AStates: TcxCalendarElementStates);

  procedure GetColors(out ABorderColor, ABackroundColor: TColor);
  begin
    if cesSelected in AStates then
      if cesHot in AStates then
      begin
        ABorderColor := $E8A766;
        ABackroundColor := $FFE8D1;
      end
      else
        if cesFocused in AStates then
        begin
          ABorderColor := $DAA026;
          ABackroundColor := $F6E8CB;
        end
        else
        begin
          ABackroundColor := $F7F7F7;
          ABorderColor := $DEDEDE;
        end
    else
      if cesHot in AStates then
      begin
        ABorderColor := $E7C070;
        ABackroundColor := $FBF3E5;
      end
      else
      begin
        ABorderColor := clNone;
        ABackroundColor := clNone;
      end;
  end;

var
  ABounds: TRect;
  ABorderColor, ABackroundColor: TColor;
begin
  ABounds := ARect;
  if AStates * [cesHot, cesSelected] <> [] then
  begin
    GetColors(ABorderColor, ABackroundColor);
    if ABorderColor <> clNone then
      ACanvas.FrameRect(ABounds, ABorderColor);
    if ABackroundColor <> clNone then
      ACanvas.FillRect(cxRectInflate(ABounds, -1, -1), ABackroundColor);
  end;

  if cesMarked in AStates then
    ACanvas.FrameRect(ARect, GetModernCalendarMarkedCellBorderColor);

  if cesFocused in AStates then
    ACanvas.DrawFocusRect(cxRectInflate(ARect, -1, -1));
end;

procedure TcxCustomLookAndFeelPainter.DrawModernCalendarDateHeaderSelection(
  ACanvas: TcxCanvas; const ARect: TRect; AStates: TcxCalendarElementStates);
begin
end;

procedure TcxCustomLookAndFeelPainter.DrawModernCalendarHeaderSelection(
  ACanvas: TcxCanvas; const ARect: TRect; AStates: TcxCalendarElementStates);
begin
  if AStates * [cesHot, cesPressed] <> [] then
    ACanvas.Font.Style := ACanvas.Font.Style + [fsUnderline];
end;

function TcxCustomLookAndFeelPainter.GetModernCalendarCellTextColor(AStates: TcxCalendarElementStates): TColor;
begin
  if cesDisabled in AStates then
    Result := clGrayText
  else
    if (cesHot in AStates) or
      (AStates * [cesMarked, cesSelected] = [cesMarked]) then
      Result := GetModernCalendarSelectedTextColor
    else
      Result := clBtnText;
end;

function TcxCustomLookAndFeelPainter.GetModernCalendarDateHeaderTextColor(
  AStates: TcxCalendarElementStates): TColor;
var
  AIsHighlighted: Boolean;
begin
  AIsHighlighted := AStates = [cesHot];
  if AIsHighlighted then
    Result := GetModernCalendarSelectedTextColor
  else
    Result := DefaultDateNavigatorHeaderTextColor(False);
end;

function TcxCustomLookAndFeelPainter.GetModernCalendarHeaderTextColor(AStates: TcxCalendarElementStates): TColor;
begin
  Result := clHotLight;
end;

function TcxCustomLookAndFeelPainter.GetModernCalendarHeaderTextOffsets: TRect;
begin
  Result := cxNullRect;
end;

function TcxCustomLookAndFeelPainter.GetModernCalendarMarkedCellBorderColor: TColor;
begin
  Result := clHotLight;
end;

function TcxCustomLookAndFeelPainter.GetModernCalendarSelectedTextColor: TColor;
begin
  Result := clHotLight;
end;

procedure TcxCustomLookAndFeelPainter.DrawModernClockHands(
  ACanvas: TcxCanvas; const ARect: TRect; ADateTime: TDateTime; AColor: TColor);
begin
  DrawScaledModernClockHands(ACanvas, ARect, ADateTime, AColor, dxDefaultScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DrawScaledModernClockHands(ACanvas: TcxCanvas;
  const ARect: TRect; ADateTime: TDateTime; AColor: TColor; AScaleFactor: TdxScaleFactor);

    procedure DrawHand(AGPGraphics: TdxGPGraphics; const R: TRect;
      AAngle: Single; AStartOffset, AEndOffset: Single; AWidth: Single; AHandColor: TColor);
    var
      ACenter, ARadius: TdxPointF;
    begin
      ACenter := cxRectCenter(dxRectF(R));
      ARadius.X := cxRectWidth(R) / 2;
      ARadius.Y := cxRectHeight(R) / 2;
      AGPGRaphics.Line(
        ACenter.X - ARadius.X * AStartOffset * Cos(AAngle), ACenter.Y - ARadius.Y * AStartOffset * Sin(AAngle),
        ACenter.X + ARadius.X * AEndOffset * Cos(AAngle), ACenter.Y + ARadius.Y * AEndOffset * Sin(AAngle),
        dxColorToAlphaColor(AHandColor), AWidth * AScaleFactor.Numerator / AScaleFactor.Denominator);
    end;

var
  AAngle: Single;
  AHour, AMin, AMSec, ASec: Word;
  AGPGraphics: TdxGPGraphics;
begin
  DecodeTime(ADateTime, AHour, AMin, ASec, AMSec);
  AGPGraphics := dxGpBeginPaint(ACanvas.Handle, ARect);
  try
    AGPGraphics.SmoothingMode := smAntiAlias;

    AAngle := Pi * 2 * ((AHour mod 12) * 60 * 60 + AMin * 60 + ASec - 3 * 60 * 60) / 12 / 60 / 60;
    DrawHand(AGPGraphics, ARect, AAngle, 0.05, 0.55, 1.8, AColor);

    AAngle := Pi * 2 * (AMin * 60 + ASec - 15 * 60) / 60 / 60;
    DrawHand(AGPGraphics, ARect, AAngle, 0.05, 0.7, 1.8, AColor);

    AAngle := Pi * 2 * (ASec - 15) / 60;
    DrawHand(AGPGraphics, ARect, AAngle, 0.1, 0.745, 1, AColor);
  finally
    dxGpEndPaint(AGPGRaphics);
  end;
end;

procedure TcxCustomLookAndFeelPainter.DrawRatingControlScaledIndicator(
  ACanvas: TcxCanvas; const ABounds: TRect; AState: TdxRatingControlIndicatorState; AScaleFactor: TdxScaleFactor);
const
  OffsetMap: array[TdxRatingControlIndicatorState] of Integer = (0, 2, 1);
var
  ASourceRect: TRect;
  AHeight: Integer;
begin
  AHeight := RatingControlIndicator.Height div 3;
  ASourceRect := cxRectSetHeight(RatingControlIndicator.ClientRect, AHeight);
  ASourceRect := cxRectOffset(ASourceRect, 0, OffsetMap[AState] * AHeight);
  RatingControlIndicator.StretchDraw(ACanvas.Handle, ABounds, ASourceRect);
end;

procedure TcxCustomLookAndFeelPainter.DrawRatingControlIndicator(
  ACanvas: TcxCanvas; const ABounds: TRect; AState: TdxRatingControlIndicatorState);
begin
  DrawRatingControlScaledIndicator(ACanvas, ABounds, AState, dxSystemScaleFactor);
end;

function TcxCustomLookAndFeelPainter.GetRatingControlIndicatorSize: TSize;
begin
  Result := GetRatingControlScaledIndicatorSize(dxSystemScaleFactor);
end;

function TcxCustomLookAndFeelPainter.GetRatingControlIndicatorColorPalette(
  AState: TdxRatingControlIndicatorState): IdxColorPalette;
begin
  Result := nil;
end;

function TcxCustomLookAndFeelPainter.GetRatingControlScaledIndicatorSize(AScaleFactor: TdxScaleFactor): TSize;
begin
  Result := AScaleFactor.Apply(cxSize(RatingControlIndicator.Width, RatingControlIndicator.Height div 3));
end;

procedure TcxCustomLookAndFeelPainter.DrawFixedGroupIndicator(ACanvas: TcxCanvas; const ABounds: TRect);
begin
  DrawScaledFixedGroupIndicator(ACanvas, ABounds, dxSystemScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DrawScaledFixedGroupIndicator(
  ACanvas: TcxCanvas; const ABounds: TRect; AScaleFactor: TdxScaleFactor);
begin
  FixedGroupIndicator.StretchDraw(ACanvas.Handle, ABounds);
end;

function TcxCustomLookAndFeelPainter.GetFixedGroupIndicatorSize: TSize;
begin
  Result := GetScaledFixedGroupIndicatorSize(dxSystemScaleFactor);
end;

function TcxCustomLookAndFeelPainter.GetScaledFixedGroupIndicatorSize(AScaleFactor: TdxScaleFactor): TSize;
begin
  Result := AScaleFactor.Apply(FixedGroupIndicator.Size)
end;

function TcxCustomLookAndFeelPainter.GetWheelPickerBorderItemColor(AState: TcxButtonState): TColor;
begin
  case AState of
    cxbsHot:
      Result := RGB(217, 217, 217);
    cxbsPressed:
      Result := RGB(188, 201, 231);
  else
    Result := clDefault;
  end;
end;

function TcxCustomLookAndFeelPainter.GetWheelPickerColorPalette(AState: TcxButtonState): IdxColorPalette;
begin
  Result := nil;
end;

function TcxCustomLookAndFeelPainter.GetWheelPickerFillItemColor(AState: TcxButtonState): TColor;
begin
  case AState of
    cxbsHot:
      Result := RGB(239, 240, 242);
    cxbsPressed:
      Result := RGB(226, 234, 253);
  else
    Result := clDefault;
  end;
end;

procedure TcxCustomLookAndFeelPainter.DrawWheelPickerItem(ACanvas: TcxCanvas; const ARect: TRect; AState: TcxButtonState);
begin
  DrawWheelPickerItemBackground(ACanvas, ARect, AState);
  DrawWheelPickerItemBorder(ACanvas, ARect, AState);
end;

procedure TcxCustomLookAndFeelPainter.DrawWheelPickerItemBackground(ACanvas: TcxCanvas; const ARect: TRect; AState: TcxButtonState);
begin
  ACanvas.FillRect(ARect, GetWheelPickerFillItemColor(AState));
end;

procedure TcxCustomLookAndFeelPainter.DrawWheelPickerItemBorder(ACanvas: TcxCanvas; const ARect: TRect; AState: TcxButtonState);
begin
  ACanvas.FrameRect(ARect, GetWheelPickerBorderItemColor(AState));
end;

function TcxCustomLookAndFeelPainter.RichEditControlHeaderFooterLineColor: TColor;
begin
  Result := clBtnShadow;
end;

function TcxCustomLookAndFeelPainter.RichEditControlHeaderFooterMarkBackColor: TColor;
begin
  Result := DefaultControlColor;
end;

function TcxCustomLookAndFeelPainter.RichEditControlHeaderFooterMarkTextColor: TColor;
begin
  Result := DefaultControlTextColor;
end;

function TcxCustomLookAndFeelPainter.RichEditRulerControlColor: TColor;
begin
  Result := DefaultControlColor;
end;

function TcxCustomLookAndFeelPainter.RichEditRulerActiveAreaColor: TColor;
begin
  Result := DefaultContentColor;
end;

function TcxCustomLookAndFeelPainter.RichEditRulerInactiveAreaColor: TColor;
begin
  Result := clBtnShadow;
end;

function TcxCustomLookAndFeelPainter.RichEditRulerDefaultTabColor: TColor;
begin
  Result := clBtnShadow;
end;

function TcxCustomLookAndFeelPainter.RichEditRulerTabTypeToggleBorderColor: TColor;
begin
  Result := cl3DDkShadow;
end;

function TcxCustomLookAndFeelPainter.RichEditRulerTextColor: TColor;
begin
  Result := DefaultControlTextColor;
end;

procedure TcxCustomLookAndFeelPainter.DrawTokenBackground(ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState);
begin
  DrawScaledTokenBackground(ACanvas, R, AState, dxSystemScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DrawScaledTokenBackground(
  ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState; AScaleFactor: TdxScaleFactor);
begin
  ACanvas.SaveClipRegion;
  try
    ACanvas.FrameRect(R, clBtnShadow);
    if not (AState in [cxbsNormal, cxbsDisabled]) then
    begin
      ACanvas.IntersectClipRect(cxRectInflate(R, -1));
      DrawScaledButton(ACanvas, R, '', AState, AScaleFactor);
    end;
  finally
    ACanvas.RestoreClipRegion;
  end;
end;

procedure TcxCustomLookAndFeelPainter.DrawScaledTokenCloseGlyph(
  ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState; AScaleFactor: TdxScaleFactor);
begin
  DrawScaledButtonCrossEx(ACanvas, R, GetTokenTextColor(AState), AState, 7, AScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DrawTokenCloseGlyph(ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState);
begin
  DrawScaledTokenCloseGlyph(ACanvas, R, AState, dxSystemScaleFactor);
end;

function TcxCustomLookAndFeelPainter.GetTokenCloseGlyphSize: TSize;
begin
  Result := GetScaledTokenCloseGlyphSize(dxSystemScaleFactor);
end;

function TcxCustomLookAndFeelPainter.GetScaledTokenCloseGlyphSize(AScaleFactor: TdxScaleFactor): TSize;
begin
  Result := AScaleFactor.Apply(cxSize(12, 11));
  dxAdjustToTouchableSize(Result.cx);
end;

function TcxCustomLookAndFeelPainter.GetTokenColorPalette(AState: TcxButtonState): IdxColorPalette;
begin
  Result := nil;
end;

function TcxCustomLookAndFeelPainter.GetScaledTokenContentOffsets(AScaleFactor: TdxScaleFactor): TRect;
begin
  Result := AScaleFactor.Apply(cxRect(3, 3, 3, 3));
end;

function TcxCustomLookAndFeelPainter.GetTokenContentOffsets: TRect;
begin
  Result := GetScaledTokenContentOffsets(dxSystemScaleFactor);
end;

function TcxCustomLookAndFeelPainter.GetScaledTokenDefaultGlyphSize(AScaleFactor: TdxScaleFactor): TSize;
begin
  Result := AScaleFactor.Apply(cxSize(12, 12));
end;

function TcxCustomLookAndFeelPainter.GetTokenDefaultGlyphSize: TSize;
begin
  Result := GetScaledTokenDefaultGlyphSize(dxSystemScaleFactor);
end;

function TcxCustomLookAndFeelPainter.GetTokenTextColor(AState: TcxButtonState): TColor;
begin
  Result := ButtonSymbolColor(AState);
end;

procedure TcxCustomLookAndFeelPainter.CalculateArrowPoints(R: TRect;
  var P: TcxArrowPoints; AArrowDirection: TcxArrowDirection; AProportional: Boolean; AArrowSize: Integer = 0);

  function _GetSize: TSize;
  begin
    if AArrowDirection in [adUp, adDown] then
    begin
      if AArrowSize = 0 then
        AArrowSize := (R.Right - R.Left - 1) div 4 + 1;
      Result.cy := AArrowSize;
      Result.cx := AArrowSize * 2 - 1;
    end
    else
    begin
      if AArrowSize = 0 then
        AArrowSize := (R.Bottom - R.Top - 1) div 4 + 1;
      Result.cx := AArrowSize;
      Result.cy := AArrowSize * 2 - 1;
    end
  end;

var
  ASize: TSize;
  ADelta: Integer;
begin
  with R do
  begin
    if AProportional then
    begin
      ADelta := (Right - Left) - (Bottom - Top);
      if ADelta > 0 then
        InflateRect(R, -ADelta div 2, 0)
      else
        InflateRect(R, 0, ADelta div 2);
    end;
    ASize := _GetSize;
    case AArrowDirection of
      adUp:
        begin
          P[cxArrowTopPointIndex] := Point((Left + Right - 1) div 2, MulDiv(Top + Bottom - ASize.cy, 1, 2){ - 1});
          P[cxArrowLeftBasePointIndex] := Point((Left + Right - ASize.cx) div 2, P[cxArrowTopPointIndex].Y + ASize.cy - 1);
          P[cxArrowRightBasePointIndex] := Point(P[cxArrowLeftBasePointIndex].X + ASize.cx - 1, P[cxArrowLeftBasePointIndex].Y);
        end;
      adDown:
        begin
          P[cxArrowRightBasePointIndex] := Point((Left + Right - ASize.cx) div 2, MulDiv(Top + Bottom - ASize.cy, 1, 2));
          P[cxArrowLeftBasePointIndex] := Point(P[cxArrowRightBasePointIndex].X + ASize.cx - 1, P[cxArrowRightBasePointIndex].Y);
          P[cxArrowTopPointIndex] := Point((Left + Right - 1) div 2, P[cxArrowRightBasePointIndex].Y + ASize.cy - 1);
        end;
      adLeft:
        begin
          P[cxArrowTopPointIndex] := Point((Left + Right - ASize.cx) div 2, (Top + Bottom (**)) div 2);
          P[cxArrowRightBasePointIndex] := Point(P[cxArrowTopPointIndex].X + ASize.cx - 1, MulDiv(Top + Bottom - ASize.cy, 1, 2));
          P[cxArrowLeftBasePointIndex] := Point(P[cxArrowRightBasePointIndex].X, P[cxArrowRightBasePointIndex].Y + ASize.cy - 1);
        end;
      adRight:
        begin
          P[cxArrowLeftBasePointIndex] := Point((Left + Right - ASize.cx) div 2, MulDiv(Top + Bottom - ASize.cy, 1, 2));
          P[cxArrowTopPointIndex] := Point(P[cxArrowLeftBasePointIndex].X + ASize.cx - 1, (Top + Bottom (**)) div 2);
          P[cxArrowRightBasePointIndex] := Point(P[cxArrowLeftBasePointIndex].X, P[cxArrowLeftBasePointIndex].Y + ASize.cy - 1);
        end;
    end;
  end;
end;

procedure TcxCustomLookAndFeelPainter.DrawArrow(ACanvas: TcxCanvas;
  const R: TRect; AArrowDirection: TcxArrowDirection; AColor: TColor);
var
  P: TcxArrowPoints;
begin
  CalculateArrowPoints(R, P, AArrowDirection, True);
  ACanvas.Brush.Style := bsSolid;
  ACanvas.SetBrushColor(AColor);
  ACanvas.Pen.Color := AColor;
  ACanvas.Polygon(P);
end;

procedure TcxCustomLookAndFeelPainter.DrawScaledArrow(ACanvas: TcxCanvas;
  const R: TRect; AState: TcxButtonState; AArrowDirection: TcxArrowDirection;
  AScaleFactor: TdxScaleFactor; ADrawBorder: Boolean = True);
var
  P: TcxArrowPoints;
begin
  CalculateArrowPoints(R, P, AArrowDirection, False);
  with ACanvas do
  begin
    Brush.Style := bsSolid;
    if ADrawBorder then
      DrawScaledArrowBorder(ACanvas, R, AState, AScaleFactor)
    else
    begin
      SetBrushColor(ButtonColor(AState));
      FillRect(R);
    end;
    SetBrushColor(ButtonSymbolColor(AState));
    Pen.Color := Brush.Color;
    Polygon(P);
  end;
end;

procedure TcxCustomLookAndFeelPainter.DrawArrow(ACanvas: TcxCanvas; const R: TRect;
  AState: TcxButtonState; AArrowDirection: TcxArrowDirection; ADrawBorder: Boolean = True);
begin
  DrawScaledArrow(ACanvas, R, AState, AArrowDirection, dxSystemScaleFactor, ADrawBorder);
end;

procedure TcxCustomLookAndFeelPainter.DrawScaledArrowBorder(ACanvas: TcxCanvas;
  const R: TRect; AState: TcxButtonState; AScaleFactor: TdxScaleFactor);
begin
  ACanvas.Brush.Color := ButtonColor(AState);
  ACanvas.Brush.Style := bsSolid;
  ACanvas.FillRect(R);
  DrawButtonBorder(ACanvas, R, AState);
end;

procedure TcxCustomLookAndFeelPainter.DrawArrowBorder(ACanvas: TcxCanvas;
  const R: TRect; AState: TcxButtonState);
begin
  DrawScaledArrowBorder(ACanvas, R, AState, dxSystemScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DrawCollapseArrow(
  ACanvas: TcxCanvas; R: TRect; AColor: TColor; ALineWidth: Integer = 1);

  function CalculateArrowSize: Integer;
  begin
    Result := (Min(cxRectWidth(R), cxRectHeight(R)) - 1) div 4;
  end;

  procedure PrepareRect(var R: TRect);
  var
    ADelta: Integer;
  begin
    ADelta := cxRectWidth(R) - cxRectHeight(R);
    if ADelta > 0 then
      InflateRect(R, -ADelta div 2, 0)
    else
      InflateRect(R, 0, ADelta div 2);
  end;

  procedure DoDrawArrow(X, Y, ASize: Integer);
  var
    I: Integer;
    R: TRect;
  begin
    R := cxRect(X, Y, X + ALineWidth, Y + 1);
    for I := 0 to ASize - 1 do
    begin
      ACanvas.FillRect(cxRectOffset(R, I, -I));
      ACanvas.FillRect(cxRectOffset(R, I,  I));
    end;
  end;

var
  ASize: Integer;
begin
  ASize := CalculateArrowSize;
  ACanvas.Brush.Color := AColor;
  R := cxRectCenterHorizontally(R, 3 * ALineWidth + ASize - 1);
  DoDrawArrow(R.Left, cxRectCenter(R).Y, ASize);
  DoDrawArrow(R.Left + 2 * ALineWidth, cxRectCenter(R).Y, ASize);
end;

procedure TcxCustomLookAndFeelPainter.DrawScaledScrollBarArrow(ACanvas: TcxCanvas; R: TRect; AState: TcxButtonState;
  AArrowDirection: TcxArrowDirection; AScaleFactor: TdxScaleFactor);
var
  P: TcxArrowPoints;

  procedure OffsetPoints(ADelta: Integer);
  var
    I: Integer;
  begin
    for I := 0 to 2 do
    begin
      Inc(P[I].X, ADelta);
      Inc(P[I].Y, ADelta);
    end;
  end;

begin
  if AState = cxbsPressed then
    OffsetRect(R, ScaledButtonTextShift(AScaleFactor), ScaledButtonTextShift(AScaleFactor));
  CalculateArrowPoints(R, P, AArrowDirection, True);
  with ACanvas do
  begin
    Brush.Style := bsSolid;
    if AState <> cxbsDisabled then
      Brush.Color := ButtonSymbolColor(AState)
    else
    begin
      OffsetPoints(AScaleFactor.Apply(1));
      Brush.Color := clBtnHighlight;
      Pen.Color := Brush.Color;
      Polygon(P);
      OffsetPoints(AScaleFactor.Apply(-1));
      Brush.Color := clBtnShadow;
    end;
    Pen.Color := Brush.Color;
    Polygon(P);
  end;
end;

procedure TcxCustomLookAndFeelPainter.DrawScrollBarArrow(ACanvas: TcxCanvas;
  R: TRect; AState: TcxButtonState; AArrowDirection: TcxArrowDirection);
begin
  DrawScaledScrollBarArrow(ACanvas, R, AState, AArrowDirection, dxSystemScaleFactor);
end;

function TcxCustomLookAndFeelPainter.BorderSize: Integer;
begin
  Result := 0;
end;

function TcxCustomLookAndFeelPainter.SeparatorSize: Integer;
begin
  Result := BorderSize;
end;

procedure TcxCustomLookAndFeelPainter.DrawBorder(ACanvas: TcxCanvas; R: TRect);
begin
end;

procedure TcxCustomLookAndFeelPainter.DrawContainerBorder(ACanvas: TcxCanvas; const R: TRect; AStyle: TcxContainerBorderStyle;
  AWidth: Integer; AColor: TColor; ABorders: TcxBorders);
begin
  case AStyle of
    cbsSingle, cbsThick:
      ACanvas.FrameRect(R, AColor, AWidth);
    cbsFlat:
      begin
        ACanvas.DrawEdge(R, True, True, ABorders);
        ACanvas.FrameRect(cxRectInflate(R, -1, -1), clBtnFace);
      end;
    cbs3D:
      begin
        ACanvas.DrawEdge(R, True, True, ABorders);
        ACanvas.DrawComplexFrame(cxRectInflate(R, -1, -1), cl3DDkShadow, cl3DLight, ABorders);
      end;
  end;
end;

procedure TcxCustomLookAndFeelPainter.DoDrawSeparator(ACanvas: TcxCanvas; R: TRect; AIsVertical: Boolean);
begin
  Dec(R.Right);
  Dec(R.Bottom);
  ACanvas.FillRect(R, clBtnShadow);
  OffsetRect(R, 1, 1);
  ACanvas.FillRect(R, clBtnHighlight);
end;

procedure TcxCustomLookAndFeelPainter.DrawSeparator(ACanvas: TcxCanvas; const R: TRect; AIsVertical: Boolean);
begin
  DoDrawSeparator(ACanvas, GetSeparatorBounds(R, SeparatorSize, AIsVertical), AIsVertical);
end;

function TcxCustomLookAndFeelPainter.ButtonBorderSize(AState: TcxButtonState = cxbsNormal): Integer;
begin
  Result := 0;
end;

function TcxCustomLookAndFeelPainter.ButtonColor(AState: TcxButtonState): TColor;
begin
  Result := clBtnFace;
end;

function TcxCustomLookAndFeelPainter.ButtonColorPalette(AState: TcxButtonState): IdxColorPalette;
begin
  Result := nil;
end;

function TcxCustomLookAndFeelPainter.ButtonDescriptionTextColor(AState: TcxButtonState; ADefaultColor: TColor): TColor;
begin
  Result := ADefaultColor;
end;

function TcxCustomLookAndFeelPainter.ScaledButtonFocusRect(ACanvas: TcxCanvas; R: TRect; AScaleFactor: TdxScaleFactor): TRect;
begin
  Result := R;
  InflateRect(Result, AScaleFactor.Apply(-4), AScaleFactor.Apply(-4));
  if IsRectEmpty(Result) then
    Result := R;
end;

function TcxCustomLookAndFeelPainter.ButtonFocusRect(ACanvas: TcxCanvas; R: TRect): TRect;
begin
  Result := ScaledButtonFocusRect(ACanvas, R, dxSystemScaleFactor);
end;

function TcxCustomLookAndFeelPainter.ScaledButtonTextOffset(AScaleFactor: TdxScaleFactor): Integer;
begin
  Result := 0;
end;

function TcxCustomLookAndFeelPainter.ButtonTextOffset: Integer;
begin
  Result := ScaledButtonTextOffset(dxSystemScaleFactor);
end;

function TcxCustomLookAndFeelPainter.ButtonTextShift: Integer;
begin
  Result := 0;
end;

function TcxCustomLookAndFeelPainter.ScaledButtonTextShift(AScaleFactor: TdxScaleFactor): Integer;
begin
  Result := ScaledButtonTextShift(dxSystemScaleFactor);
end;

function TcxCustomLookAndFeelPainter.ButtonSymbolColor(
  AState: TcxButtonState; ADefaultColor: TColor = clDefault): TColor;
begin
  if AState = cxbsDisabled then
    Result := clBtnShadow
  else
    Result := cxGetActualColor(ADefaultColor, clBtnText);
end;

function TcxCustomLookAndFeelPainter.ButtonSymbolState(AState: TcxButtonState): TcxButtonState;
begin
  Result := AState;
end;

procedure TcxCustomLookAndFeelPainter.DrawScaledButton(ACanvas: TcxCanvas; R: TRect; const ACaption: string;
  AState: TcxButtonState; AScaleFactor: TdxScaleFactor; ADrawBorder: Boolean = True; AColor: TColor = clDefault;
  ATextColor: TColor = clDefault; AWordWrap: Boolean = False; AIsToolButton: Boolean = False; APart: TcxButtonPart = cxbpButton);
var
  AFlags: Integer;
begin
  with ACanvas do
  begin
    if ADrawBorder then
    begin
      DrawButtonBorder(ACanvas, R, AState);
      InflateRect(R, -ButtonBorderSize(AState), -ButtonBorderSize(AState));
    end;
    if AColor = clDefault then
      Brush.Color := ButtonColor(AState)
    else
      Brush.Color := AColor;
    FillRect(R);
    if ATextColor = clDefault then
      Font.Color := ButtonSymbolColor(AState)
    else
      Font.Color := ATextColor;
    Brush.Style := bsClear;
    with R do // for compatible with standard buttons
    begin
      Dec(Bottom, Ord(Odd(Bottom - Top)));
      if (Bottom - Top) < 18 then Dec(Top);
    end;
    if AState = cxbsPressed then
      OffsetRect(R, ScaledButtonTextShift(AScaleFactor), ScaledButtonTextShift(AScaleFactor));
    if Length(ACaption) > 0 then
    begin
      AFlags := cxAlignVCenter or cxShowPrefix or cxAlignHCenter;
      if AWordWrap then
        AFlags := AFlags or cxWordBreak
      else
        AFlags := AFlags or cxSingleLine;
      DrawText(ACaption, R, AFlags, AState <> cxbsDisabled);
    end;
    Brush.Style := bsSolid;
  end;
end;

procedure TcxCustomLookAndFeelPainter.DrawButton(ACanvas: TcxCanvas; R: TRect; const ACaption: string;
  AState: TcxButtonState; ADrawBorder: Boolean = True; AColor: TColor = clDefault; ATextColor: TColor = clDefault;
  AWordWrap: Boolean = False; AIsToolButton: Boolean = False; APart: TcxButtonPart = cxbpButton);
begin
  DrawScaledButton(ACanvas, R, ACaption, AState, dxSystemScaleFactor,
    ADrawBorder, AColor, ATextColor, AWordWrap, AIsToolButton, APart);
end;

function TcxCustomLookAndFeelPainter.GetDropDownButtonRightPartSize: Integer;
begin
  Result := GetScaledDropDownButtonRightPartSize(dxSystemScaleFactor);
end;

function TcxCustomLookAndFeelPainter.GetScaledDropDownButtonRightPartSize(AScaleFactor: TdxScaleFactor): Integer;
begin
  Result := AScaleFactor.Apply(15);
end;

function TcxCustomLookAndFeelPainter.DefaultCommandLinkTextColor(AState: TcxButtonState; ADefaultColor: TColor): TColor;
begin
  Result := ButtonSymbolColor(AState, ADefaultColor);
end;

procedure TcxCustomLookAndFeelPainter.DrawScaledCommandLinkBackground(ACanvas: TcxCanvas; R: TRect; AState: TcxButtonState;
  AScaleFactor: TdxScaleFactor; AColor: TColor = clDefault);
begin
  DrawScaledButton(ACanvas, R, '', AState, AScaleFactor, True, AColor);
end;


procedure TcxCustomLookAndFeelPainter.DrawCommandLinkBackground(
  ACanvas: TcxCanvas; R: TRect; AState: TcxButtonState; AColor: TColor = clDefault);
begin
  DrawScaledButton(ACanvas, R, '', AState, dxSystemScaleFactor, True, AColor);
end;

procedure TcxCustomLookAndFeelPainter.DrawScaledCommandLinkGlyph(ACanvas: TcxCanvas;
  const AGlyphPos: TPoint; AState: TcxButtonState; AScaleFactor: TdxScaleFactor);
var
  ADestRect: TRect;
  ASrcRect: TRect;
  ACommandLinkGlyphs: TdxGPImage;
begin
  ACommandLinkGlyphs := TdxGPImage.Create;
  try
    ACommandLinkGlyphs.LoadFromResource(HInstance, 'CX_COMMANDLINKGLYPH', 'PNG');
    ASrcRect := cxRect(GetScaledCommandLinkGlyphSize(dxDefaultScaleFactor));
    ADestRect := cxRectSetOrigin(AScaleFactor.Apply(ASrcRect), AGlyphPos);
    if AState = cxbsHot then
      ASrcRect := cxRectSetOrigin(ASrcRect, Point(0, GetScaledCommandLinkGlyphSize(dxDefaultScaleFactor).cy));
    cxRightToLeftDependentDraw(ACanvas, ADestRect,
      procedure
      begin
        ACommandLinkGlyphs.StretchDraw(ACanvas.Handle, ADestRect, ASrcRect);
      end);
  finally
    ACommandLinkGlyphs.Free;
  end;
end;

procedure TcxCustomLookAndFeelPainter.DrawCommandLinkGlyph(
  ACanvas: TcxCanvas; const AGlyphPos: TPoint; AState: TcxButtonState);
begin
  DrawScaledCommandLinkGlyph(ACanvas, AGlyphPos, AState, dxSystemScaleFactor);
end;

function TcxCustomLookAndFeelPainter.GetScaledCommandLinkGlyphSize(AScaleFactor: TdxScaleFactor): TSize;
begin
  Result := AScaleFactor.Apply(Size(20, 20));
end;

function TcxCustomLookAndFeelPainter.GetCommandLinkGlyphSize: TSize;
begin
  Result := GetScaledCommandLinkGlyphSize(dxSystemScaleFactor);
end;

function TcxCustomLookAndFeelPainter.GetScaledCommandLinkMargins(AScaleFactor: TdxScaleFactor): TRect;
begin
  Result := AScaleFactor.Apply(Rect(7, 10, 7, 10));
end;

function TcxCustomLookAndFeelPainter.GetCommandLinkMargins: TRect;
begin
  Result := GetScaledCommandLinkMargins(dxSystemScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DrawScaledButtonCross(ACanvas: TcxCanvas;
  const R: TRect; AColor: TColor; AState: TcxButtonState; AScaleFactor: TdxScaleFactor);
var
  ASize: Integer;
begin
  ASize := ScaledFilterCloseButtonSize(AScaleFactor).X div 2;
  if not Odd(ASize) then
    Dec(ASize);
  DrawScaledButtonCrossEx(ACanvas, R, AColor, AState, ASize, dxDefaultScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DrawButtonCross(
  ACanvas: TcxCanvas; const R: TRect; AColor: TColor; AState: TcxButtonState);
begin
  DrawScaledButtonCross(ACanvas, R, AColor, AState, dxSystemScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DrawScaledButtonCrossEx(ACanvas: TcxCanvas;
  const R: TRect; AColor: TColor; AState: TcxButtonState; ASize: Integer; AScaleFactor: TdxScaleFactor);

  procedure DrawOneMark(ADelta: Integer);
  var
    P: TPoint;
  begin
    P := Point((R.Left + R.Right - ASize) div 2 + ADelta, (R.Top + R.Bottom - ASize) div 2);
    if AState = cxbsPressed then
    begin
      Inc(P.X);
      Inc(P.Y);
    end;
    ACanvas.MoveTo(P.X, P.Y);
    ACanvas.LineTo(P.X + ASize, P.Y + ASize);
    ACanvas.MoveTo(P.X, P.Y + ASize - 1);
    ACanvas.LineTo(P.X + ASize, P.Y - 1);
  end;

var
  I: Integer;
begin
  ACanvas.Pen.Color := AColor;
  ASize := AScaleFactor.Apply(ASize);
  for I := 0 to 1 do
    DrawOneMark(I);
end;

procedure TcxCustomLookAndFeelPainter.DrawButtonCross(ACanvas: TcxCanvas;
  const R: TRect; AColor: TColor; AState: TcxButtonState; ASize: Integer);
begin
  DrawScaledButtonCrossEx(ACanvas, R, AColor, AState, ASize, dxSystemScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DrawButtonBorder(ACanvas: TcxCanvas;
  R: TRect; AState: TcxButtonState);
begin
end;

procedure TcxCustomLookAndFeelPainter.DrawScaledSearchEditButtonGlyph(
  ACanvas: TcxCanvas; R: TRect; AState: TcxButtonState; AScaleFactor: TdxScaleFactor);
var
  AGlyphRect: TRect;
begin
  AGlyphRect := SearchButtonGlyph.ClientRect;
  AGlyphRect.Bottom := cxRectHeight(AGlyphRect) div 4;
  SearchButtonGlyph.StretchDraw(ACanvas.Handle,
    cxRectCenter(R, ScaledSearchButtonGlyphSize(AScaleFactor)),
    cxRectOffset(AGlyphRect, 0, cxRectHeight(AGlyphRect) * (Ord(AState) - 1)));
end;

procedure TcxCustomLookAndFeelPainter.DrawSearchEditButtonGlyph(ACanvas: TcxCanvas; R: TRect; AState: TcxButtonState);
begin
  DrawScaledSearchEditButtonGlyph(ACanvas, R, AState, dxSystemScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DrawExpandButton(
  ACanvas: TcxCanvas; const R: TRect; AExpanded: Boolean; AColor: TColor = clDefault);
begin
  DrawScaledExpandButton(ACanvas, R, AExpanded, dxSystemScaleFactor, AColor);
end;

procedure TcxCustomLookAndFeelPainter.DrawScaledExpandButtonEx(ACanvas: TcxCanvas; const R: TRect;
  AState: TcxButtonState; AExpanded: Boolean; AScaleFactor: TdxScaleFactor; ARotationAngle: TcxRotationAngle = ra0);
var
  ABitmap: TcxBitmap;
  ARect: TRect;
begin
  DrawScaledButton(ACanvas, R, '', AState, AScaleFactor);
  if ARotationAngle in [raMinus90, raPlus90] then
    ARect := cxRectRotate(R)
  else
    ARect := R;

  ABitmap := TcxBitmap.CreateSize(ARect);
  try
    ABitmap.cxCanvas.FillRect(ABitmap.ClientRect, ButtonColor(AState));
    DrawScaledExpandMark(ABitmap.cxCanvas, ABitmap.ClientRect, ButtonSymbolColor(AState), AExpanded, AScaleFactor);
    ACanvas.RotateBitmap(ABitmap, ARotationAngle);
    cxDrawImage(ACanvas.Handle, R, R, ABitmap, nil, -1, idmNormal);
  finally
    ABitmap.Free;
  end;
end;

procedure TcxCustomLookAndFeelPainter.DrawExpandButtonEx(ACanvas: TcxCanvas; const R: TRect;
  AState: TcxButtonState; AExpanded: Boolean; ARotationAngle: TcxRotationAngle = ra0);
begin
  DrawScaledExpandButtonEx(ACanvas, R, AState, AExpanded, dxSystemScaleFactor, ARotationAngle);
end;

function TcxCustomLookAndFeelPainter.DrawExpandButtonFirst: Boolean;
begin
  Result := True;
end;

procedure TcxCustomLookAndFeelPainter.DrawScaledGroupExpandButton(ACanvas: TcxCanvas;
  const R: TRect; AExpanded: Boolean; AState: TcxButtonState; AScaleFactor: TdxScaleFactor);
begin
  DrawScaledButton(ACanvas, R, '', AState, AScaleFactor);
  DrawScaledExpandMark(ACanvas, R, ButtonSymbolColor(AState), AExpanded, AScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DrawGroupExpandButton(
  ACanvas: TcxCanvas; const R: TRect; AExpanded: Boolean; AState: TcxButtonState);
begin
  DrawScaledGroupExpandButton(ACanvas, R, AExpanded, AState, dxSystemScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DrawSmallExpandButton(ACanvas: TcxCanvas;
  R: TRect; AExpanded: Boolean; ABorderColor: TColor; AColor: TColor = clDefault);
begin
  DrawScaledSmallExpandButton(ACanvas, R, AExpanded, ABorderColor, dxSystemScaleFactor, AColor);
end;

procedure TcxCustomLookAndFeelPainter.DrawScaledSmallExpandButton(ACanvas: TcxCanvas; R: TRect;
  AExpanded: Boolean; ABorderColor: TColor; AScaleFactor: TdxScaleFactor; AColor: TColor = clDefault);
begin
  ACanvas.Brush.Color := ABorderColor;
  ACanvas.FrameRect(R);
  if AColor <> clDefault then
  begin
    InflateRect(R, -1, -1);
    ACanvas.Brush.Color := AColor;
    ACanvas.FillRect(R);
    InflateRect(R, 1, 1);
  end;
  DrawExpandButtonCross(ACanvas, R, AExpanded, clBtnText, AScaleFactor);
end;

function TcxCustomLookAndFeelPainter.ScaledExpandButtonAreaSize(AScaleFactor: TdxScaleFactor): Integer;
begin
  Result := ScaledExpandButtonSize(AScaleFactor);
  dxAdjustToTouchableSize(Result);
end;

function TcxCustomLookAndFeelPainter.ExpandButtonAreaSize: Integer;
begin
  Result := ScaledExpandButtonAreaSize(dxSystemScaleFactor);
end;

function TcxCustomLookAndFeelPainter.ExpandButtonSize: Integer;
begin
  Result := ScaledExpandButtonSize(dxSystemScaleFactor);
end;

function TcxCustomLookAndFeelPainter.ScaledGroupExpandButtonSize(AScaleFactor: TdxScaleFactor): Integer;
begin
  Result := AScaleFactor.Apply(15);
end;

function TcxCustomLookAndFeelPainter.GroupExpandButtonSize: Integer;
begin
  Result := ScaledGroupExpandButtonSize(dxSystemScaleFactor);
end;

function TcxCustomLookAndFeelPainter.IsButtonHotTrack: Boolean;
begin
  Result := False;
end;

function TcxCustomLookAndFeelPainter.IsPointOverGroupExpandButton(const R: TRect; const P: TPoint): Boolean;
begin
  Result := PtInRect(R, P);
end;

function TcxCustomLookAndFeelPainter.ScaledSearchButtonGlyphSize(AScaleFactor: TdxScaleFactor): TSize;
begin
  Result.cx := SearchButtonGlyph.Width;
  Result.cy := SearchButtonGlyph.Height div 4;
  Result := AScaleFactor.Apply(Result);
end;

function TcxCustomLookAndFeelPainter.ScaledSmallExpandButtonSize(AScaleFactor: TdxScaleFactor): Integer;
begin
  Result := AScaleFactor.Apply(9);
end;

function TcxCustomLookAndFeelPainter.SmallExpandButtonSize: Integer;
begin
  Result := ScaledSmallExpandButtonSize(dxSystemScaleFactor);
end;

function TcxCustomLookAndFeelPainter.ScaledCheckButtonAreaSize(AScaleFactor: TdxScaleFactor): TSize;
begin
  Result := ScaledCheckButtonSize(AScaleFactor);
  dxAdjustToTouchableSize(Result);
end;

function TcxCustomLookAndFeelPainter.CheckButtonAreaSize: TSize;
begin
  Result := ScaledCheckButtonAreaSize(dxSystemScaleFactor);
end;

function TcxCustomLookAndFeelPainter.CheckBorderSize: Integer;
begin
  Result := 2;
end;

function TcxCustomLookAndFeelPainter.CheckButtonColor(AState: TcxButtonState; ACheckState: TcxCheckBoxState): TColor;
const
  Colors1: array[TcxCheckBoxState] of TColor = (clWindow, clWindow, clBtnFace);
  Colors2: array[TcxButtonState] of TColor = (clWindow, clWindow, clWindow, clBtnFace, clBtnFace);
begin
  if AState = cxbsNormal then
    Result := Colors1[ACheckState]
  else
    Result := Colors2[AState];
end;

function TcxCustomLookAndFeelPainter.ScaledCheckButtonSize(AScaleFactor: TdxScaleFactor): TSize;
begin
  Result := AScaleFactor.Apply(FCheckButtonSize, dxSystemScaleFactor);
end;

function TcxCustomLookAndFeelPainter.CheckButtonSize: TSize;
begin
  Result := ScaledCheckButtonSize(dxSystemScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DrawScaledCheck(ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState;
  ACheckState: TcxCheckBoxState; AColor: TColor; AScaleFactor: TdxScaleFactor);

  function GetCheckTemplate(ASize: TSize): TPoints;
  var
    I: Integer;
    ATemplate, AFix: TPoints;
  begin
    SetLength(ATemplate, 3);
    SetLength(AFix, 3);
    SetLength(Result, 3);
    ATemplate[0] := Point(0, 0);
    ATemplate[1] := Point(500, 500);
    ATemplate[2] := Point(1500, -499);
    for I := Low(ATemplate) to High(ATemplate) do
    begin
      Result[I].X := MulDiv(ATemplate[I].X, ASize.cx, 1000) - (Trunc(ASize.cx * 0.15) + 1);
      Result[I].Y := MulDiv(ATemplate[I].Y, ASize.cx, 1000) + ASize.cy div 3;
    end;
  end;

  procedure DoDrawCheck;
  var
    I: Integer;
    ARect: TRect;
    APoints: TPoints;
    APrevClipRgn: TcxRegion;
  begin
    ARect := R;
    InflateRect(ARect, -1, -1);
    APrevClipRgn := ACanvas.GetClipRegion;
    try
      ACanvas.SetClipRegion(TcxRegion.Create(ARect), roIntersect);
      APoints := GetCheckTemplate(cxRectSize(ARect));
      for I := Low(APoints) to High(APoints) do
      begin
        APoints[I].X := APoints[I].X + ARect.Left;
        APoints[I].Y := APoints[I].Y + ARect.Top;
      end;

      for I := 0 downto -2 do
      begin
        ACanvas.MoveTo(APoints[0].X, APoints[0].Y + I);
        ACanvas.LineTo(APoints[1].X, APoints[1].Y + I);
        ACanvas.LineTo(APoints[2].X, APoints[2].Y + I);
      end;
    finally
      ACanvas.SetClipRegion(APrevClipRgn, roSet);
    end;
  end;

begin
  if ACanvas.Brush.Style = bsSolid then
  begin
    ACanvas.Brush.Color := CheckButtonColor(AState, ACheckState);
    ACanvas.FillRect(R);
  end;
  if ACheckState in [cbsChecked, cbsGrayed] then
  begin
    ACanvas.Pen.Color := AColor;
    DoDrawCheck;
  end;
end;

procedure TcxCustomLookAndFeelPainter.DrawCheck(ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState;
  ACheckState: TcxCheckBoxState; AColor: TColor);
begin
  DrawScaledCheck(ACanvas, R, AState, ACheckState, AColor, dxSystemScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DrawScaledCheck(ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState;
  AChecked: Boolean; AColor: TColor; AScaleFactor: TdxScaleFactor);
const
  CheckStatesMap: array[Boolean] of TcxCheckBoxState = (cbsUnchecked, cbsChecked);
begin
  DrawScaledCheck(ACanvas, R, AState, CheckStatesMap[AChecked], AColor, AScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DrawCheck(ACanvas: TcxCanvas; const R: TRect;
  AState: TcxButtonState; AChecked: Boolean; AColor: TColor);
begin
  DrawScaledCheck(ACanvas, R, AState, AChecked, AColor, dxSystemScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DrawCheckBorder(ACanvas: TcxCanvas;
  R: TRect; AState: TcxButtonState);
begin
end;

procedure TcxCustomLookAndFeelPainter.DrawScaledCheckButton(ACanvas: TcxCanvas;
  R: TRect; AState: TcxButtonState; ACheckState: TcxCheckBoxState; AScaleFactor: TdxScaleFactor);
const
  ColorMap: array[Boolean] of TColor = (clBtnText, clGrayText);
begin
  DrawCheckBorder(ACanvas, R, AState);
  InflateRect(R, -CheckBorderSize, -CheckBorderSize);
  DrawScaledCheck(ACanvas, R, AState, ACheckState,
    ColorMap[(AState = cxbsDisabled) or (ACheckState = cbsGrayed)], AScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DrawCheckButton(ACanvas: TcxCanvas; R: TRect; AState: TcxButtonState;
  ACheckState: TcxCheckBoxState);
begin
  DrawScaledCheckButton(ACanvas, R, AState, ACheckState, dxSystemScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DrawScaledCheckButton(ACanvas: TcxCanvas; R: TRect; AState: TcxButtonState;
  AChecked: Boolean; AScaleFactor: TdxScaleFactor);
const
  CheckStates: array[Boolean] of TcxCheckBoxState = (cbsUnchecked, cbsChecked);
begin
  DrawScaledCheckButton(ACanvas, R, AState, CheckStates[AChecked], AScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DrawCheckButton(ACanvas: TcxCanvas;
  R: TRect; AState: TcxButtonState; AChecked: Boolean);
begin
  DrawScaledCheckButton(ACanvas, R, AState, AChecked, dxSystemScaleFactor);
end;

function TcxCustomLookAndFeelPainter.ToggleSwitchToggleColor(AChecked: Boolean): TColor;
begin
  Result := 0;
end;

procedure TcxCustomLookAndFeelPainter.DrawScaledToggleSwitch(
  ACanvas: TcxCanvas; ABounds: TRect; AState: TcxButtonState; AThumbBounds: TRect; AScaleFactor: TdxScaleFactor);

  procedure InternalDrawPart(const AExcludeRect: TRect; AChecked: Boolean);
  begin
    ACanvas.SaveState;
    try
      ACanvas.ExcludeClipRect(AExcludeRect);
      DrawScaledToggleSwitchState(ACanvas, ABounds, AState, AChecked, AScaleFactor);
    finally
      ACanvas.RestoreState;
    end;
  end;

var
  ACenterThumb: Integer;
  AExcludeRect: TRect;
begin
  if ABounds.Left = AThumbBounds.Left then
    DrawScaledToggleSwitchState(ACanvas, ABounds, AState, ACanvas.UseRightToLeftAlignment, AScaleFactor)
  else
    if ABounds.Right = AThumbBounds.Right then
      DrawScaledToggleSwitchState(ACanvas, ABounds, AState, not ACanvas.UseRightToLeftAlignment, AScaleFactor)
    else
    begin
      ACenterThumb := AThumbBounds.Left + cxRectWidth(AThumbBounds) div 2;
      AExcludeRect := ABounds;
      AExcludeRect.Left := ACenterThumb;
      InternalDrawPart(AExcludeRect, not ACanvas.UseRightToLeftAlignment);
      AExcludeRect := ABounds;
      AExcludeRect.Right := ACenterThumb;
      InternalDrawPart(AExcludeRect,  ACanvas.UseRightToLeftAlignment);
    end;
end;

procedure TcxCustomLookAndFeelPainter.DrawToggleSwitch(
  ACanvas: TcxCanvas; ABounds: TRect; AState: TcxButtonState; AThumbBounds: TRect);
begin
  DrawScaledToggleSwitch(ACanvas, ABounds, AState, AThumbBounds, dxSystemScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DrawToggleSwitchState(
  ACanvas: TcxCanvas; ABounds: TRect; AState: TcxButtonState; AChecked: Boolean);
begin
  DrawScaledToggleSwitchState(ACanvas, ABounds, AState, AChecked, dxSystemScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DrawScaledToggleSwitchState(ACanvas: TcxCanvas;
  ABounds: TRect; AState: TcxButtonState; AChecked: Boolean; AScaleFactor: TdxScaleFactor);
begin
  // do nothing
end;

procedure TcxCustomLookAndFeelPainter.DrawToggleSwitchStateIndicator(
  ACanvas: TcxCanvas; ABounds: TRect; AText: string; AFont: TFont);
begin
  ACanvas.SaveState;
  try
    ACanvas.Font := AFont;
    cxDrawText(ACanvas, AText, ABounds, DT_SINGLELINE or DT_CENTER or DT_VCENTER, GetToggleSwitchTextColor);
  finally
    ACanvas.RestoreState;
  end;
end;

procedure TcxCustomLookAndFeelPainter.DrawScaledToggleSwitchThumb(
  ACanvas: TcxCanvas; ABounds: TRect; AState: TcxButtonState; AScaleFactor: TdxScaleFactor);
begin
  DrawScaledButton(ACanvas, cxRectInflate(ABounds, -2), '', AState, AScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DrawToggleSwitchThumb(ACanvas: TcxCanvas; ABounds: TRect; AState: TcxButtonState);
begin
  DrawScaledToggleSwitchThumb(ACanvas, ABounds, AState, dxSystemScaleFactor);
end;

function TcxCustomLookAndFeelPainter.GetToggleSwitchColorPalette: IdxColorPalette;
begin
  Result := nil;
end;

function TcxCustomLookAndFeelPainter.GetToggleSwitchTextColor: TColor;
begin
  Result := DefaultContentTextColor;
end;

function TcxCustomLookAndFeelPainter.GetToggleSwitchThumbPercentsWidth: Integer;
begin
  Result := 50;
end;

procedure TcxCustomLookAndFeelPainter.DrawScaledRadioButton(ACanvas: TcxCanvas; X, Y: Integer; AButtonState: TcxButtonState;
  AChecked, AFocused: Boolean; ABrushColor: TColor; AScaleFactor: TdxScaleFactor; AIsDesigning: Boolean = False);
begin
  TcxRadioButtonImageListManager.Get(AScaleFactor).Draw(
    ACanvas, X, Y, ABrushColor, cxLookAndFeelKindMap[LookAndFeelStyle], AButtonState, AChecked, AFocused, AIsDesigning);
end;

procedure TcxCustomLookAndFeelPainter.DrawRadioButton(ACanvas: TcxCanvas;
  X, Y: Integer; AButtonState: TcxButtonState; AChecked, AFocused: Boolean;
  ABrushColor: TColor; AIsDesigning: Boolean = False);
begin
  DrawScaledRadioButton(ACanvas, X, Y, AButtonState, AChecked, AFocused, ABrushColor, dxSystemScaleFactor, AIsDesigning);
end;

function TcxCustomLookAndFeelPainter.RadioButtonBodyColor(AState: TcxButtonState): TColor;
begin
  case AState of
    cxbsDisabled, cxbsPressed:
      Result := clBtnFace;
    else
      Result := clWindow;
  end;
end;

function TcxCustomLookAndFeelPainter.ScaledRadioButtonSize(AScaleFactor: TdxScaleFactor): TSize;
begin
  Result := TcxRadioButtonImageListManager.Get(AScaleFactor).GetSize;
end;

function TcxCustomLookAndFeelPainter.RadioButtonSize: TSize;
begin
  Result := ScaledRadioButtonSize(dxSystemScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DrawLabelLine(ACanvas: TcxCanvas;
  const R: TRect; AOuterColor, AInnerColor: TColor; AIsVertical: Boolean);
const
  BordersMap: array[Boolean] of TcxBorders = ([bTop, bBottom], [bLeft, bRight]);
begin
  AOuterColor := cxGetActualColor(AOuterColor, clBtnShadow);
  AInnerColor := cxGetActualColor(AInnerColor, clBtnHighlight);
  ACanvas.DrawComplexFrame(R, AOuterColor, AInnerColor, BordersMap[AIsVertical]);
end;

function TcxCustomLookAndFeelPainter.LabelLineHeight: Integer;
begin
  Result := 2;
end;

function TcxCustomLookAndFeelPainter.GaugeControlBackgroundColor: TColor;
begin
  Result := clWindow;
end;

procedure TcxCustomLookAndFeelPainter.DrawGaugeControlBackground(ACanvas: TcxCanvas; const ARect: TRect;
  ATransparent: Boolean; ABackgroundColor: TColor);
begin
  DrawBackground(ACanvas, ARect, ATransparent, ABackgroundColor, nil);
end;

function TcxCustomLookAndFeelPainter.MapControlBackgroundColor: TColor;
begin
  Result := $958B5F;
end;

function TcxCustomLookAndFeelPainter.MapControlPanelBackColor: TdxAlphaColor;
begin
  Result := $C8000000;
end;

function TcxCustomLookAndFeelPainter.MapControlPanelHotTrackedTextColor: TdxAlphaColor;
begin
  Result := $FFFFFFFF;
end;

function TcxCustomLookAndFeelPainter.MapControlPanelPressedTextColor: TdxAlphaColor;
begin
  Result := $FF7E8383;
end;

function TcxCustomLookAndFeelPainter.MapControlPanelTextColor: TdxAlphaColor;
begin
  Result := $FFADDFFF;
end;

function TcxCustomLookAndFeelPainter.MapControlGetMapPushpinSize(AScaleFactor: TdxScaleFactor): TSize;
begin
  Result := AScaleFactor.Apply(MapPushpin.Size);
end;

function TcxCustomLookAndFeelPainter.MapControlGetMapPushpinTextOrigin(AScaleFactor: TdxScaleFactor): TPoint;
begin
  Result := AScaleFactor.Apply(Point(17, 17));
end;

function TcxCustomLookAndFeelPainter.MapControlMapCustomElementSelectionOffset(AScaleFactor: TdxScaleFactor): TRect;
begin
  Result :=AScaleFactor.Apply(cxRect(14, 14, 14, 14));
end;

function TcxCustomLookAndFeelPainter.MapControlMapCustomElementTextColor: TdxAlphaColor;
begin
  Result := $FF000000;
end;

function TcxCustomLookAndFeelPainter.MapControlMapCustomElementTextGlowColor: TdxAlphaColor;
begin
  Result := $FFFFFFFF;
end;

function TcxCustomLookAndFeelPainter.MapControlMapPushpinTextColor: TdxAlphaColor;
begin
  Result := $FF000000;
end;

function TcxCustomLookAndFeelPainter.MapControlMapPushpinTextGlowColor: TdxAlphaColor;
begin
  Result := $FFFFFFFF;
end;

function TcxCustomLookAndFeelPainter.MapControlShapeColor: TdxAlphaColor;
begin
  Result := $FFD7D7D7;
end;

function TcxCustomLookAndFeelPainter.MapControlSelectedRegionBackgroundColor: TdxAlphaColor;
begin
  Result := $800072AE;
end;

function TcxCustomLookAndFeelPainter.MapControlSelectedRegionBorderColor: TdxAlphaColor;
begin
  Result := $FF3399FF;
end;

function TcxCustomLookAndFeelPainter.MapControlShapeBorderColor: TdxAlphaColor;
begin
  Result := $FFFFFFFF;
end;

function TcxCustomLookAndFeelPainter.MapControlShapeBorderWidth(AScaleFactor: TdxScaleFactor): Integer;
begin
  Result := AScaleFactor.Apply(1);
end;

function TcxCustomLookAndFeelPainter.MapControlShapeHighlightedColor: TdxAlphaColor;
begin
  Result := $FFD7D7D7;
end;

function TcxCustomLookAndFeelPainter.MapControlShapeBorderHighlightedColor: TdxAlphaColor;
begin
  Result := $FFFFFFFF;
end;

function TcxCustomLookAndFeelPainter.MapControlShapeBorderHighlightedWidth(AScaleFactor: TdxScaleFactor): Integer;
begin
  Result := AScaleFactor.Apply(1);
end;

function TcxCustomLookAndFeelPainter.MapControlShapeSelectedColor: TdxAlphaColor;
begin
  Result := $FFD7D7D7;
end;

function TcxCustomLookAndFeelPainter.MapControlShapeBorderSelectedColor: TdxAlphaColor;
begin
  Result := $FFFFFFFF;
end;

function TcxCustomLookAndFeelPainter.MapControlShapeBorderSelectedWidth(AScaleFactor: TdxScaleFactor): Integer;
begin
  Result := AScaleFactor.Apply(1);
end;

procedure TcxCustomLookAndFeelPainter.DrawMapCustomElementBackground(ACanvas: TcxCanvas;
  const ARect: TRect; AState: TdxMapControlElementState);

  function GetBorderColor: TdxAlphaColor;
  begin
    case AState of
      mcesHot, mcesPressed:
        Result := $5A000000;
      mcesSelected:
        Result := $80000000
    else // mcesNormal
      Result := 0
    end;
  end;

  function GetBackgroundColor: TdxAlphaColor;
  begin
    case AState of
      mcesHot, mcesPressed:
        Result := $5AFFFFFF;
      mcesSelected:
        Result := $80FFFFFF
    else // mcesNormal
      Result := 0
    end;
  end;

  function GetBorderWidth: Integer;
  begin
    Result := 1;
  end;

var
  AGpGrahpics: TdxGPGraphics;
begin
  AGpGrahpics := dxGpBeginPaint(ACanvas.Handle, ARect);
  try
    AGpGrahpics.SmoothingMode := smAntiAlias;
    AGpGrahpics.RoundRect(cxRectInflate(ARect, -GetBorderWidth, -GetBorderWidth), GetBorderColor, GetBackgroundColor, 4, 4, GetBorderWidth);
  finally
    dxGpEndPaint(AGpGrahpics);
  end;
end;

procedure TcxCustomLookAndFeelPainter.DrawMapPushpin(ACanvas: TcxCanvas; const ARect: TRect;
  AState: TdxMapControlElementState; AScaleFactor: TdxScaleFactor);
begin
  MapPushpin.StretchDraw(ACanvas.Handle, ARect);
end;

procedure TcxCustomLookAndFeelPainter.OfficeNavigationBarDrawCustomizationButton(
  ACanvas: TcxCanvas; const ARect: TRect; AState: TcxCalendarElementState; AColor: TdxAlphaColor);
begin
  OfficeNavigationBarDrawScaledCustomizationButton(ACanvas, ARect, AState, dxSystemScaleFactor, AColor);
end;

procedure TcxCustomLookAndFeelPainter.OfficeNavigationBarDrawBackground(ACanvas: TcxCanvas; const ARect: TRect);
begin
  ACanvas.FillRect(ARect, $E4E4E4);
end;

procedure TcxCustomLookAndFeelPainter.OfficeNavigationBarDrawImageSelection(
  ACanvas: TcxCanvas; const ARect: TRect; AState: TcxCalendarElementState);
begin
  OfficeNavigationBarDrawScaledImageSelection(ACanvas, ARect, AState, dxSystemScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.OfficeNavigationBarButtonItemDrawBackground(
  ACanvas: TcxCanvas; const ARect: TRect; AState: TcxCalendarElementState);
begin
  OfficeNavigationBarDrawScaledButtonItemBackground(ACanvas, ARect, AState, dxSystemScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.OfficeNavigationBarDrawScaledButtonItemBackground(
  ACanvas: TcxCanvas; const ARect: TRect; AState: TcxCalendarElementState; AScaleFactor: TdxScaleFactor);
begin
  if AState = cesHot then
    DrawScaledButton(ACanvas, ARect, '', cxbsHot, AScaleFactor)
  else
    DrawScaledButton(ACanvas, ARect, '', cxbsNormal, AScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.OfficeNavigationBarItemDrawBackground(
  ACanvas: TcxCanvas; const ARect: TRect; AState: TcxCalendarElementState);
begin
  OfficeNavigationBarDrawScaledItemBackground(ACanvas, ARect, AState, dxSystemScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.OfficeNavigationBarDrawScaledCustomizationButton(ACanvas: TcxCanvas;
  const ARect: TRect; AState: TcxCalendarElementState; AScaleFactor: TdxScaleFactor; AColor: TdxAlphaColor = dxacDefault);
var
  AImage: TdxGPImage;
begin
  if AColor <> dxacDefault then
  begin
    AImage := NavigationBarCustomizationButton.Clone;
    try
      AImage.ChangeColor(dxAlphaColorToColor(AColor));
      AImage.StretchDraw(ACanvas.Handle, ARect, dxGetAlpha(AColor));
    finally
      AImage.Free;
    end;
  end
  else
    NavigationBarCustomizationButton.StretchDraw(ACanvas.Handle, ARect);
end;

procedure TcxCustomLookAndFeelPainter.OfficeNavigationBarDrawScaledItemBackground(
  ACanvas: TcxCanvas; const ARect: TRect; AState: TcxCalendarElementState; AScaleFactor: TdxScaleFactor);
begin
  // do nothing
end;

procedure TcxCustomLookAndFeelPainter.OfficeNavigationBarDrawScaledImageSelection(
  ACanvas: TcxCanvas; const ARect: TRect; AState: TcxCalendarElementState; AScaleFactor: TdxScaleFactor);
begin
  case AState of
    cesPressed:
      ACanvas.FillRect(ARect, $E0C092);
    cesHot, cesSelected:
      ACanvas.FillRect(ARect, $F7E6CD);
  end;
end;

function TcxCustomLookAndFeelPainter.OfficeNavigationBarContentOffsets: TRect;
begin
  Result := OfficeNavigationBarScaledContentOffsets(dxSystemScaleFactor);
end;

function TcxCustomLookAndFeelPainter.OfficeNavigationBarCustomizationButtonSize: TSize;
begin
  Result := OfficeNavigationBarScaledCustomizationButtonSize(dxSystemScaleFactor);
end;

function TcxCustomLookAndFeelPainter.OfficeNavigationBarButtonItemContentOffsets: TRect;
begin
  Result := OfficeNavigationBarScaledButtonItemContentOffsets(dxSystemScaleFactor);
end;

function TcxCustomLookAndFeelPainter.OfficeNavigationBarButtonItemFontSize: Integer;
begin
  Result := OfficeNavigationBarScaledButtonItemFontSize(dxSystemScaleFactor);
end;

function TcxCustomLookAndFeelPainter.OfficeNavigationBarButtonItemTextColor(AState: TcxCalendarElementState): TColor;
begin
  case AState of
    cesNormal:
      Result := $565656;
    cesHot:
      Result := $C67200;
    cesPressed:
      Result := $C67200;
    cesSelected:
      Result := $C67200;
  else
    Result := $565656;
  end;
end;

function TcxCustomLookAndFeelPainter.OfficeNavigationBarItemContentOffsets: TRect;
begin
  Result := OfficeNavigationBarScaledItemContentOffsets(dxSystemScaleFactor);
end;

function TcxCustomLookAndFeelPainter.OfficeNavigationBarItemTextColor(AState: TcxCalendarElementState): TColor;
begin
  case AState of
    cesNormal:
      Result := $565656;
    cesHot:
      Result := $C67200;
    cesPressed:
      Result := $C67200;
    cesSelected:
      Result := $C67200;
  else
    Result := $565656;
  end;
end;

function TcxCustomLookAndFeelPainter.OfficeNavigationBarItemFontSize: Integer;
begin
  Result := OfficeNavigationBarScaledItemFontSize(dxSystemScaleFactor);
end;

function TcxCustomLookAndFeelPainter.OfficeNavigationBarScaledButtonItemContentOffsets(AScaleFactor: TdxScaleFactor): TRect;
begin
  Result := AScaleFactor.Apply(cxRect(4, 4, 4, 4));
end;

function TcxCustomLookAndFeelPainter.OfficeNavigationBarScaledButtonItemFontSize(AScaleFactor: TdxScaleFactor): Integer;
begin
  Result := AScaleFactor.Apply(9);
end;

function TcxCustomLookAndFeelPainter.OfficeNavigationBarScaledContentOffsets(AScaleFactor: TdxScaleFactor): TRect;
begin
  Result := AScaleFactor.Apply(cxRect(2, 2, 2, 2));
end;

function TcxCustomLookAndFeelPainter.OfficeNavigationBarScaledCustomizationButtonSize(AScaleFactor: TdxScaleFactor): TSize;
begin
  Result := AScaleFactor.Apply(NavigationBarCustomizationButton.Size);
end;

function TcxCustomLookAndFeelPainter.OfficeNavigationBarScaledItemContentOffsets(AScaleFactor: TdxScaleFactor): TRect;
begin
  Result := AScaleFactor.Apply(cxRect(2, 2, 2, 2));
end;

function TcxCustomLookAndFeelPainter.OfficeNavigationBarScaledItemFontSize(AScaleFactor: TdxScaleFactor): Integer;
begin
  Result := AScaleFactor.Apply(19);
end;

function TcxCustomLookAndFeelPainter.PDFViewerNavigationPaneButtonColorPalette(AState: TcxButtonState): IdxColorPalette;
begin
  Result := nil;
end;

function TcxCustomLookAndFeelPainter.PDFViewerNavigationPaneButtonContentOffsets(AScaleFactor: TdxScaleFactor): TRect;
begin
  Result := AScaleFactor.Apply(cxRect(2, 2, 2, 2));
end;

function TcxCustomLookAndFeelPainter.PDFViewerNavigationPaneButtonOverlay(AScaleFactor: TdxScaleFactor): TPoint;
begin
  Result.X := PDFViewerNavigationPaneContentOffsets(AScaleFactor).Left;
  Result.Y := PDFViewerNavigationPaneContentOffsets(AScaleFactor).Top + BorderSize;
end;

function TcxCustomLookAndFeelPainter.PDFViewerNavigationPaneButtonRect(const ARect: TRect; AState: TcxButtonState;
  ASelected: Boolean; AScaleFactor: TdxScaleFactor): TRect;
var
  ADelta: TPoint;
begin
  Result := ARect;
  if ASelected or (AState = cxbsPressed) then
  begin
    ADelta := PDFViewerNavigationPaneButtonOverlay(AScaleFactor);
    Result := cxRectInflate(Result, ADelta.X, ADelta.Y);
  end;
end;

function TcxCustomLookAndFeelPainter.PDFViewerNavigationPaneButtonSize(AScaleFactor: TdxScaleFactor): TSize;
begin
  Result := AScaleFactor.Apply(cxSize(42, 42));
end;

function TcxCustomLookAndFeelPainter.PDFViewerNavigationPaneContentOffsets(AScaleFactor: TdxScaleFactor): TRect;
begin
  Result := AScaleFactor.Apply(cxRect(4, 4, 4, 4));
end;

function TcxCustomLookAndFeelPainter.PDFViewerNavigationPanePageCaptionContentOffsets(AScaleFactor: TdxScaleFactor): TRect;
begin
  Result := PDFViewerNavigationPanePageContentOffsets(AScaleFactor);
end;

function TcxCustomLookAndFeelPainter.PDFViewerNavigationPanePageCaptionTextColor: TColor;
begin
  Result := DefaultContentTextColor;
end;

function TcxCustomLookAndFeelPainter.PDFViewerNavigationPanePageContentOffsets(AScaleFactor: TdxScaleFactor): TRect;
begin
  Result := PDFViewerNavigationPaneContentOffsets(AScaleFactor);
end;

function TcxCustomLookAndFeelPainter.PDFViewerNavigationPanePageToolbarContentOffsets(AScaleFactor: TdxScaleFactor): TRect;
begin
  Result := PDFViewerNavigationPaneContentOffsets(AScaleFactor);
end;

function TcxCustomLookAndFeelPainter.PDFViewerSelectionColor: TColor;
begin
  Result := DefaultSelectionColor;
end;

procedure TcxCustomLookAndFeelPainter.PDFViewerDrawNavigationPaneBackground(ACanvas: TcxCanvas; const ARect: TRect;
  AScaleFactor: TdxScaleFactor);
begin
  PDFViewerDrawFindPanelBackground(ACanvas, ARect, [bRight]);
end;

procedure TcxCustomLookAndFeelPainter.PDFViewerDrawNavigationPaneButton(ACanvas: TcxCanvas; const ARect: TRect;
  AState: TcxButtonState; AScaleFactor: TdxScaleFactor; AMinimized, ASelected, AIsFirst: Boolean);
var
  AButtonRect: TRect;
begin
  AButtonRect := PDFViewerNavigationPaneButtonRect(ARect, cxbsPressed, ASelected, AScaleFactor);

  if AMinimized or not ASelected then
  begin
    PDFViewerDrawFindPanelBackground(ACanvas, AButtonRect, [bRight]);
    InflateRect(AButtonRect, -ButtonBorderSize, -ButtonBorderSize);
    ACanvas.Brush.Color := ButtonColor(AState);
    ACanvas.FillRect(AButtonRect);
  end
  else
  begin
    DrawBorder(ACanvas, AButtonRect);
    ACanvas.FillRect(cxRectContent(AButtonRect, cxRect(0, BorderSize, 0, BorderSize)), DefaultControlColor);
  end;
end;

procedure TcxCustomLookAndFeelPainter.PDFViewerDrawNavigationPanePageBackground(ACanvas: TcxCanvas; const ARect: TRect);
begin
  PDFViewerDrawFindPanelBackground(ACanvas, ARect, [bRight]);
end;

procedure TcxCustomLookAndFeelPainter.PDFViewerDrawNavigationPanePageButton(ACanvas: TcxCanvas; const ARect: TRect;
  AState: TcxButtonState; AScaleFactor: TdxScaleFactor);
begin
  if AState in [cxbsHot, cxbsPressed] then
    ACanvas.FillRect(ARect, clBtnShadow);
end;

procedure TcxCustomLookAndFeelPainter.PDFViewerDrawNavigationPanePageCaptionBackground(ACanvas: TcxCanvas;
  const ARect: TRect);
begin
  PDFViewerDrawFindPanelBackground(ACanvas, ARect, [bRight]);
end;

procedure TcxCustomLookAndFeelPainter.PDFViewerDrawNavigationPanePageToolbarBackground(ACanvas: TcxCanvas;
  const ARect: TRect);
begin
  PDFViewerDrawFindPanelBackground(ACanvas, ARect, []);
end;

procedure TcxCustomLookAndFeelPainter.PDFViewerDrawFindPanelBackground(ACanvas: TcxCanvas; const R: TRect;
  ABorders: TcxBorders);
var
  ARect: TRect;
begin
  DrawBorder(ACanvas, R);
  ARect := R;
  if bLeft in ABorders then
    Inc(ARect.Left, BorderSize);
  if bBottom in ABorders then
    Dec(ARect.Bottom, BorderSize);
  if bTop in ABorders then
    Inc(ARect.Top, BorderSize);
  if bRight in ABorders then
    Dec(ARect.Right, BorderSize);
  ACanvas.FillRect(ARect, DefaultControlColor);
end;

procedure TcxCustomLookAndFeelPainter.PDFViewerDrawPageThumbnailPreviewBackground(ACanvas: TcxCanvas; const ARect: TRect);
begin
  DrawPrintPreviewBackground(ACanvas, ARect);
end;

procedure TcxCustomLookAndFeelPainter.DrawSpreadSheetScaledGroupExpandButton(
  ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState; AScaleFactor: TdxScaleFactor);
begin
  DrawScaledButton(ACanvas, R, '', AState, AScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DrawSpreadSheetGroupExpandButton(ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState);
begin
  DrawSpreadSheetScaledGroupExpandButton(ACanvas, R, AState, dxSystemScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DrawSpreadSheetGroupExpandButtonGlyph(ACanvas: TcxCanvas;
  const R: TRect; AState: TcxButtonState; AExpanded: Boolean; ADefaultGlyphs: TCustomImageList = nil);
begin
  DrawSpreadSheetScaledGroupExpandButtonGlyph(ACanvas, R, AState, AExpanded, dxSystemScaleFactor, ADefaultGlyphs);
end;

procedure TcxCustomLookAndFeelPainter.DrawSpreadSheetScaledGroupExpandButtonGlyph(ACanvas: TcxCanvas; const R: TRect;
  AState: TcxButtonState; AExpanded: Boolean; AScaleFactor: TdxScaleFactor; ADefaultGlyphs: TCustomImageList = nil);
begin
  DrawNavigatorScaledButtonGlyph(ACanvas, ADefaultGlyphs, IfThen(AExpanded, 8, 6),
    cxRectCenter(R, NavigatorScaledButtonGlyphSize(AScaleFactor)), True, False, AScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DrawSpreadSheetScaledHeader(
  ACanvas: TcxCanvas; const ABounds, ATextAreaBounds: TRect; ANeighbors: TcxNeighbors;
  ABorders: TcxBorders; AState: TcxButtonState; AAlignmentHorz: TAlignment; AAlignmentVert: TcxAlignmentVert;
  AMultiLine, AShowEndEllipsis: Boolean; const AText: string; AFont: TFont; ATextColor, ABkColor: TColor;
  AScaleFactor: TdxScaleFactor; AOnDrawBackground: TcxDrawBackgroundEvent = nil; AIsLast: Boolean = False; AIsGroup: Boolean = False);
begin
  DrawHeaderBorder(ACanvas, ABounds, ANeighbors, ABorders);
  if AState = cxbsHot then
    ABkColor := dxGetMiddleRGB(ABkColor, clHighlight, 70);
  DrawContent(ACanvas, ScaledHeaderContentBounds(ABounds, ABorders, AScaleFactor), ATextAreaBounds, Integer(AState),
    AAlignmentHorz, AAlignmentVert, AMultiLine, AShowEndEllipsis, AText, AFont, ATextColor, ABkColor, AOnDrawBackground);
end;

procedure TcxCustomLookAndFeelPainter.DrawSpreadSheetHeader(ACanvas: TcxCanvas; const ABounds, ATextAreaBounds: TRect;
  ANeighbors: TcxNeighbors; ABorders: TcxBorders; AState: TcxButtonState; AAlignmentHorz: TAlignment;
  AAlignmentVert: TcxAlignmentVert; AMultiLine, AShowEndEllipsis: Boolean; const AText: string; AFont: TFont;
  ATextColor, ABkColor: TColor; AOnDrawBackground: TcxDrawBackgroundEvent = nil; AIsLast: Boolean = False;
  AIsGroup: Boolean = False);
begin
  DrawSpreadSheetScaledHeader(ACanvas, ABounds, ATextAreaBounds, ANeighbors, ABorders, AState, AAlignmentHorz,
    AAlignmentVert, AMultiLine, AShowEndEllipsis, AText, AFont, ATextColor, ABkColor, dxSystemScaleFactor,
    AOnDrawBackground, AIsLast, AIsGroup);
end;

function TcxCustomLookAndFeelPainter.SpreadSheetContentColor: TColor;
begin
  Result := clWindow;
end;

function TcxCustomLookAndFeelPainter.SpreadSheetContentTextColor: TColor;
begin
  Result := clWindowText;
end;

function TcxCustomLookAndFeelPainter.SpreadSheetFrozenPaneSeparatorColor: TColor;
begin
  Result := clBlack;
end;

function TcxCustomLookAndFeelPainter.SpreadSheetScaledGroupExpandButtonContentOffsets(AScaleFactor: TdxScaleFactor): TRect;
begin
  Result := AScaleFactor.Apply(cxRect(3, 3, 3, 3));
end;

function TcxCustomLookAndFeelPainter.SpreadSheetGroupExpandButtonContentOffsets: TRect;
begin
  Result := SpreadSheetScaledGroupExpandButtonContentOffsets(dxSystemScaleFactor);
end;

function TcxCustomLookAndFeelPainter.SpreadSheetScaledGroupExpandButtonGlyphSize(AScaleFactor: TdxScaleFactor): TSize;
begin
  Result := NavigatorScaledButtonGlyphSize(AScaleFactor);
end;

function TcxCustomLookAndFeelPainter.SpreadSheetGroupExpandButtonGlyphSize: TSize;
begin
  Result := SpreadSheetScaledGroupExpandButtonGlyphSize(dxSystemScaleFactor);
end;

function TcxCustomLookAndFeelPainter.SpreadSheetGroupExpandButtonTextColor(AState: TcxButtonState): TColor;
begin
  Result := ButtonSymbolColor(AState);
end;

function TcxCustomLookAndFeelPainter.SpreadSheetScaledGroupLevelMarkSize(AScaleFactor: TdxScaleFactor): TSize;
begin
  Result := AScaleFactor.Apply(Size(2, 2));
end;

function TcxCustomLookAndFeelPainter.SpreadSheetGroupLevelMarkSize: TSize;
begin
  Result := SpreadSheetScaledGroupLevelMarkSize(dxSystemScaleFactor);
end;

function TcxCustomLookAndFeelPainter.SpreadSheetGroupLineColor: TColor;
begin
  Result := clGray;
end;

function TcxCustomLookAndFeelPainter.SpreadSheetSelectionColor: TColor;
begin
  Result := DefaultSelectionColor;
end;

procedure TcxCustomLookAndFeelPainter.DrawSpreadSheetFormulaBarScaledSeparator(
  ACanvas: TcxCanvas; const R: TRect; AScaleFactor: TdxScaleFactor);
begin
  DrawScaledSplitter(ACanvas, R, False, False, False, AScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DrawSpreadSheetFormulaBarScaledExpandButton(
  ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState; AExpanded: Boolean; AScaleFactor: TdxScaleFactor);
begin
  if AExpanded then
    DrawScrollBarPart(ACanvas, False, R, sbpLineUp, AState)
  else
    DrawScrollBarPart(ACanvas, False, R, sbpLineDown, AState);
end;

function TcxCustomLookAndFeelPainter.SpreadSheetFormulaBarGetScaledSeparatorSize(AScaleFactor: TdxScaleFactor): Integer;
begin
  Result := GetScaledSplitterSize(False, AScaleFactor).cx;
end;

procedure TcxCustomLookAndFeelPainter.DrawScaledHeader(ACanvas: TcxCanvas; const ABounds, ATextAreaBounds: TRect;
  ANeighbors: TcxNeighbors; ABorders: TcxBorders; AState: TcxButtonState; AAlignmentHorz: TAlignment;
  AAlignmentVert: TcxAlignmentVert; AMultiLine, AShowEndEllipsis: Boolean; const AText: string; AFont: TFont;
  ATextColor, ABkColor: TColor; AScaleFactor: TdxScaleFactor; AOnDrawBackground: TcxDrawBackgroundEvent = nil;
  AIsLast: Boolean = False; AIsGroup: Boolean = False);
begin
  DrawHeaderBorder(ACanvas, ABounds, ANeighbors, ABorders);
  DrawContent(ACanvas, ScaledHeaderContentBounds(ABounds, ABorders, AScaleFactor), ATextAreaBounds, Integer(AState),
    AAlignmentHorz, AAlignmentVert, AMultiLine, AShowEndEllipsis, AText,
    AFont, ATextColor, ABkColor, AOnDrawBackground);
end;

procedure TcxCustomLookAndFeelPainter.DrawHeader(ACanvas: TcxCanvas;
  const ABounds, ATextAreaBounds: TRect; ANeighbors: TcxNeighbors; ABorders: TcxBorders;
  AState: TcxButtonState; AAlignmentHorz: TAlignment; AAlignmentVert: TcxAlignmentVert;
  AMultiLine, AShowEndEllipsis: Boolean; const AText: string; AFont: TFont;
  ATextColor, ABkColor: TColor; AOnDrawBackground: TcxDrawBackgroundEvent = nil;
  AIsLast: Boolean = False; AIsGroup: Boolean = False);
begin
  DrawScaledHeader(ACanvas, ABounds, ATextAreaBounds, ANeighbors, ABorders, AState, AAlignmentHorz, AAlignmentVert,
    AMultiLine, AShowEndEllipsis, AText, AFont, ATextColor, ABkColor, dxDefaultScaleFactor, AOnDrawBackground, AIsLast, AIsGroup);
end;

procedure TcxCustomLookAndFeelPainter.DrawScaledHeaderEx(ACanvas: TcxCanvas;
  const ABounds, ATextAreaBounds: TRect; ANeighbors: TcxNeighbors; ABorders: TcxBorders; AState: TcxButtonState;
  AAlignmentHorz: TAlignment; AAlignmentVert: TcxAlignmentVert; AMultiLine, AShowEndEllipsis: Boolean; const AText: string;
  AFont: TFont; ATextColor, ABkColor: TColor; AScaleFactor: TdxScaleFactor; AOnDrawBackground: TcxDrawBackgroundEvent = nil);
begin
  DrawScaledHeader(ACanvas, ABounds, ATextAreaBounds, ANeighbors, ABorders, AState, AAlignmentHorz,
    AAlignmentVert, AMultiLine, AShowEndEllipsis, AText, AFont, ATextColor, ABkColor, AScaleFactor,
    AOnDrawBackground);
end;

procedure TcxCustomLookAndFeelPainter.DrawHeaderEx(ACanvas: TcxCanvas;
  const ABounds, ATextAreaBounds: TRect; ANeighbors: TcxNeighbors; ABorders: TcxBorders;
  AState: TcxButtonState; AAlignmentHorz: TAlignment; AAlignmentVert: TcxAlignmentVert;
  AMultiLine, AShowEndEllipsis: Boolean; const AText: string; AFont: TFont;
  ATextColor, ABkColor: TColor; AOnDrawBackground: TcxDrawBackgroundEvent = nil);
begin
  DrawScaledHeaderEx(ACanvas, ABounds, ATextAreaBounds, ANeighbors, ABorders, AState, AAlignmentHorz,
    AAlignmentVert, AMultiLine, AShowEndEllipsis, AText, AFont, ATextColor, ABkColor, dxDefaultScaleFactor,
    AOnDrawBackground);
end;

procedure TcxCustomLookAndFeelPainter.DrawHeaderBorder(ACanvas: TcxCanvas;
  const R: TRect; ANeighbors: TcxNeighbors; ABorders: TcxBorders);
begin
end;

procedure TcxCustomLookAndFeelPainter.DrawHeaderPressed(ACanvas: TcxCanvas;
  const ABounds: TRect);
begin
  ACanvas.InvertRect(ABounds);
end;

procedure TcxCustomLookAndFeelPainter.DrawScaledHeaderControlSection(ACanvas: TcxCanvas; const ABounds, ATextAreaBounds: TRect;
  ANeighbors: TcxNeighbors; ABorders: TcxBorders; AState: TcxButtonState; AAlignmentHorz: TAlignment;
  AAlignmentVert: TcxAlignmentVert; AMultiLine, AShowEndEllipsis: Boolean; const AText: string; AFont: TFont;
  ATextColor, ABkColor: TColor; AScaleFactor: TdxScaleFactor);
begin
  DrawHeaderControlSectionBorder(ACanvas, ABounds, ABorders, AState);
  DrawHeaderControlSectionContent(ACanvas,
    HeaderControlSectionContentBounds(ABounds, AState), ATextAreaBounds, AState,
    AAlignmentHorz, AAlignmentVert, AMultiLine, AShowEndEllipsis, AText,
    AFont, ATextColor, ABkColor);
end;

procedure TcxCustomLookAndFeelPainter.DrawHeaderControlSection(ACanvas: TcxCanvas;
  const ABounds, ATextAreaBounds: TRect; ANeighbors: TcxNeighbors; ABorders: TcxBorders; AState: TcxButtonState;
  AAlignmentHorz: TAlignment; AAlignmentVert: TcxAlignmentVert; AMultiLine, AShowEndEllipsis: Boolean;
  const AText: string; AFont: TFont; ATextColor, ABkColor: TColor);
begin
  DrawScaledHeaderControlSection(ACanvas, ABounds, ATextAreaBounds, ANeighbors, ABorders, AState, AAlignmentHorz,
    AAlignmentVert, AMultiLine, AShowEndEllipsis, AText, AFont, ATextColor, ABkColor, dxSystemScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DrawHeaderControlSectionBorder(
  ACanvas: TcxCanvas; const R: TRect; ABorders: TcxBorders; AState: TcxButtonState);
begin
end;

procedure TcxCustomLookAndFeelPainter.DrawHeaderControlSectionContent(
  ACanvas: TcxCanvas; const ABounds, ATextAreaBounds: TRect;
  AState: TcxButtonState; AAlignmentHorz: TAlignment;
  AAlignmentVert: TcxAlignmentVert; AMultiLine, AShowEndEllipsis: Boolean;
  const AText: string; AFont: TFont; ATextColor, ABkColor: TColor);
begin
  ACanvas.SetBrushColor(ABkColor);
  ACanvas.FillRect(ABounds);
  DrawHeaderControlSectionText(ACanvas,
    HeaderControlSectionTextAreaBounds(ATextAreaBounds, AState), AState, AAlignmentHorz,
    AAlignmentVert, AMultiLine, AShowEndEllipsis, AText, AFont, ATextColor);
end;

procedure TcxCustomLookAndFeelPainter.DrawHeaderControlSectionText(
  ACanvas: TcxCanvas; const ATextAreaBounds: TRect; AState: TcxButtonState;
  AAlignmentHorz: TAlignment; AAlignmentVert: TcxAlignmentVert; AMultiLine,
  AShowEndEllipsis: Boolean; const AText: string; AFont: TFont; ATextColor: TColor);

  procedure DoDrawText(ATextRect: TRect; ATextColor: TColor);
  const
    MultiLines: array[Boolean] of Integer = (cxSingleLine, cxWordBreak);
    ShowEndEllipsises: array[Boolean] of Integer = (0, cxShowEndEllipsis);
  begin
    with ACanvas do
    begin
      if AText <> '' then
      begin
        Font := AFont;
        Font.Color := ATextColor;
        DrawText(AText, ATextRect, cxAlignmentsHorz[AAlignmentHorz] or
          cxAlignmentsVert[AAlignmentVert] or MultiLines[AMultiLine] or
          ShowEndEllipsises[AShowEndEllipsis]);
      end;
    end;
  end;
var
  R: TRect;
  AColor: TColor;
begin
  R := ATextAreaBounds;
  ACanvas.Brush.Style := bsClear;
  AColor := ATextColor;
  if AState = cxbsDisabled then
  begin
    OffsetRect(R, 1, 1);
    DoDrawText(R, clBtnHighlight);
    OffsetRect(R, -1, -1);
    AColor := clBtnShadow;
  end;
  DoDrawText(R, AColor);
  ACanvas.Brush.Style := bsSolid;
end;

procedure TcxCustomLookAndFeelPainter.DrawHeaderSeparator(ACanvas: TcxCanvas;
  const ABounds: TRect; AIndentSize: Integer; AColor: TColor; AViewParams: TcxViewParams);
begin
  ACanvas.FillRect(cxRectSetWidth(ABounds, AIndentSize), AViewParams);
  ACanvas.Brush.Color := AColor;
  ACanvas.FillRect(cxRectInflate(ABounds, -AIndentSize, 0));
  ACanvas.FillRect(cxRectSetLeft(ABounds, ABounds.Right - AIndentSize), AViewParams);
end;

procedure TcxCustomLookAndFeelPainter.DrawSortingMark(ACanvas: TcxCanvas; const R: TRect; AAscendingSorting: Boolean);
begin
  DrawScaledSortingMark(ACanvas, R, AAscendingSorting, dxSystemScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DrawSummarySortingMark(
  ACanvas: TcxCanvas; const R: TRect; AAscendingSorting: Boolean);
begin
end;

procedure TcxCustomLookAndFeelPainter.DrawSummaryValueSortingMark(
  ACanvas: TcxCanvas; const R: TRect; AAscendingSorting: Boolean);
begin
  DrawScaledSummaryValueSortingMark(ACanvas, R, AAscendingSorting, dxSystemScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DrawScaledSummaryValueSortingMark(
  ACanvas: TcxCanvas; const R: TRect; AAscendingSorting: Boolean; AScaleFactor: TdxScaleFactor);
begin
  cxDrawImage(ACanvas, R, nil, cxIndicatorImages, Integer(High(TcxIndicatorKind)), ifmNormal, idmNormal, False, nil, AScaleFactor);
end;

function TcxCustomLookAndFeelPainter.HeaderBorders(ANeighbors: TcxNeighbors): TcxBorders;
begin
  Result := cxBordersAll;
end;

function TcxCustomLookAndFeelPainter.HeaderBorderSize: Integer;
begin
  Result := 0;
end;

function TcxCustomLookAndFeelPainter.HeaderBounds(const ABounds: TRect;
  ANeighbors: TcxNeighbors; ABorders: TcxBorders = cxBordersAll): TRect;
begin
  Result := ABounds;
  ABorders := ABorders - HeaderBorders(ANeighbors);
  if bLeft in ABorders then
    Dec(Result.Left, HeaderBorderSize);
  if bTop in ABorders then
    Dec(Result.Top, HeaderBorderSize);
  if bRight in ABorders then
    Inc(Result.Right, HeaderBorderSize);
  if bBottom in ABorders then
    Inc(Result.Bottom, HeaderBorderSize);
end;

function TcxCustomLookAndFeelPainter.HeaderContentBounds(const ABounds: TRect; ABorders: TcxBorders): TRect;
begin
  Result := ScaledHeaderContentBounds(ABounds, ABorders, dxDefaultScaleFactor);
end;

function TcxCustomLookAndFeelPainter.HeaderContentOffsets(AScaleFactor: TdxScaleFactor): TRect;
begin
  Result := cxRect(cxHeaderTextOffset, cxHeaderTextOffset, cxHeaderTextOffset, cxHeaderTextOffset);
  Result := AScaleFactor.Apply(Result);
end;

function TcxCustomLookAndFeelPainter.ScaledHeaderContentBounds(const ABounds: TRect; ABorders: TcxBorders; AScaleFactor: TdxScaleFactor): TRect;
begin
  Result := ABounds;
  if bLeft in ABorders then
    Inc(Result.Left, HeaderBorderSize);
  if bTop in ABorders then
    Inc(Result.Top, HeaderBorderSize);
  if bRight in ABorders then
    Dec(Result.Right, HeaderBorderSize);
  if bBottom in ABorders then
    Dec(Result.Bottom, HeaderBorderSize);
end;

function TcxCustomLookAndFeelPainter.HeaderDrawCellsFirst: Boolean;
begin
  Result := True;
end;

function TcxCustomLookAndFeelPainter.ScaledHeaderHeight(AFontHeight: Integer; AScaleFactor: TdxScaleFactor): Integer;
begin
  Result := AFontHeight + 2 * HeaderBorderSize + cxMarginsHeight(HeaderContentOffsets(AScaleFactor));
  dxAdjustToTouchableSize(Result, AScaleFactor);
end;

function TcxCustomLookAndFeelPainter.HeaderHeight(AFontHeight: Integer): Integer;
begin
  Result := ScaledHeaderHeight(AFontHeight, dxDefaultScaleFactor);
end;

function TcxCustomLookAndFeelPainter.HeaderControlSectionBorderSize(AState: TcxButtonState = cxbsNormal): Integer;
begin
  Result := 1;
end;

function TcxCustomLookAndFeelPainter.HeaderControlSectionTextAreaBounds(ABounds: TRect; AState: TcxButtonState): TRect;
begin
  Result := ABounds;
  if AState = cxbsPressed then
    OffsetRect(Result, 1, 1);
end;

function TcxCustomLookAndFeelPainter.HeaderControlSectionContentBounds(const ABounds: TRect; AState: TcxButtonState): TRect;
begin
  Result := cxRectInflate(ABounds, -HeaderControlSectionBorderSize(AState));
end;

function TcxCustomLookAndFeelPainter.HeaderWidth(ACanvas: TcxCanvas; ABorders: TcxBorders; const AText: string; AFont: TFont): Integer;
begin
  Result := ScaledHeaderWidth(ACanvas, ABorders, AText, AFont, dxSystemScaleFactor);
end;

function TcxCustomLookAndFeelPainter.ScaledHeaderWidth(ACanvas: TcxCanvas;
  ABorders: TcxBorders; const AText: string; AFont: TFont; AScaleFactor: TdxScaleFactor): Integer;
begin
  ACanvas.Font := AFont;
  Result := ACanvas.TextWidth(AText) + cxMarginsWidth(HeaderContentOffsets(AScaleFactor));
  if bLeft in ABorders then
    Inc(Result, HeaderBorderSize);
  if bRight in ABorders then
    Dec(Result, HeaderBorderSize);
end;

function TcxCustomLookAndFeelPainter.IsHeaderHotTrack: Boolean;
begin
  Result := False;
end;

function TcxCustomLookAndFeelPainter.SortingMarkAreaSize: TPoint;
begin
  Result := ScaledSortingMarkAreaSize(dxSystemScaleFactor);
end;

function TcxCustomLookAndFeelPainter.ScaledSortingMarkAreaSize(AScaleFactor: TdxScaleFactor): TPoint;
begin
  if cxIsTouchModeEnabled then
    Result := Point(AScaleFactor.Apply(SortingTouchableMarkAreaWidth), ScaledSortingMarkSize(AScaleFactor).Y)
  else
    Result := Point(AScaleFactor.Apply(SortingMarkAreaWidth), ScaledSortingMarkSize(AScaleFactor).Y);
end;

function TcxCustomLookAndFeelPainter.SortingMarkSize: TPoint;
begin
  Result := ScaledSortingMarkSize(dxSystemScaleFactor);
end;

function TcxCustomLookAndFeelPainter.SummarySortingMarkSize: TPoint;
begin
  Result := ScaledSummarySortingMarkSize(dxSystemScaleFactor);
end;

function TcxCustomLookAndFeelPainter.SummaryValueSortingMarkSize: TPoint;
begin
  Result := ScaledSummaryValueSortingMarkSize(dxSystemScaleFactor);
end;

function TcxCustomLookAndFeelPainter.ScaledSummaryValueSortingMarkSize(AScaleFactor: TdxScaleFactor): TPoint;
begin
  Result := cxPoint(dxGetImageSize(cxIndicatorImages, AScaleFactor));
end;

procedure TcxCustomLookAndFeelPainter.DrawFooterCell(ACanvas: TcxCanvas;
  const ABounds: TRect; AAlignmentHorz: TAlignment; AAlignmentVert: TcxAlignmentVert;
  AMultiLine: Boolean; const AText: string; AFont: TFont; ATextColor, ABkColor: TColor;
  AOnDrawBackground: TcxDrawBackgroundEvent = nil);
begin
  DrawFooterCellBorder(ACanvas, ABounds);
  DrawFooterCellContent(ACanvas, ABounds, AAlignmentHorz, AAlignmentVert,
    AMultiLine, AText, AFont, ATextColor, ABkColor, AOnDrawBackground);
end;

procedure TcxCustomLookAndFeelPainter.DrawFooterCellContent(ACanvas: TcxCanvas;
  const ABounds: TRect; AAlignmentHorz: TAlignment; AAlignmentVert: TcxAlignmentVert;
  AMultiLine: Boolean; const AText: string; AFont: TFont; ATextColor, ABkColor: TColor;
  AOnDrawBackground: TcxDrawBackgroundEvent = nil);
begin
  DrawContent(ACanvas, FooterCellContentBounds(ABounds), FooterCellTextAreaBounds(ABounds), 0,
    AAlignmentHorz, AAlignmentVert, AMultiLine, False, AText, AFont, ATextColor, ABkColor,
    AOnDrawBackground, True);
end;

procedure TcxCustomLookAndFeelPainter.DrawFilterRowSeparator(
  ACanvas: TcxCanvas; const ARect: TRect; ABackgroundColor: TColor);
begin
  DrawScaledHeader(ACanvas, ARect, ARect, [], cxBordersAll, cxbsNormal, taLeftJustify,
    vaTop, False, False, '', nil, clWindowText, ABackgroundColor, dxDefaultScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DrawGroupByBox(ACanvas: TcxCanvas;
  const ARect: TRect; ATransparent: Boolean; ABackgroundColor: TColor; const ABackgroundBitmap: TBitmap);
begin
  DrawBackground(ACanvas, ARect, ATransparent, ABackgroundColor, ABackgroundBitmap);
end;

function TcxCustomLookAndFeelPainter.GridGroupRowStyleOffice11ContentColor(AHasData: Boolean): TColor;
begin
  if AHasData then
    Result := clWindow
  else
    Result := dxOffice11GroupIndentColor;
end;

function TcxCustomLookAndFeelPainter.GridBordersOverlapSize: Integer;
begin
  Result := 0;
end;

function TcxCustomLookAndFeelPainter.GridGroupRowStyleOffice11SeparatorColor: TColor;
begin
  Result := dxOffice11GroupRowSeparatorColor;
end;

function TcxCustomLookAndFeelPainter.GridGroupRowStyleOffice11TextColor: TColor;
begin
  Result := dxOffice11GroupRowTextColor;
end;

function TcxCustomLookAndFeelPainter.GridDrawHeaderCellsFirst: Boolean;
begin
  Result := HeaderDrawCellsFirst;
end;

function TcxCustomLookAndFeelPainter.GridLikeControlBackgroundColor: TColor;
begin
  Result := DefaultContentColor;
end;

function TcxCustomLookAndFeelPainter.GridLikeControlContentColor: TColor;
begin
  Result := DefaultContentColor;
end;

function TcxCustomLookAndFeelPainter.GridLikeControlContentEvenColor: TColor;
begin
  Result := DefaultContentEvenColor;
end;

function TcxCustomLookAndFeelPainter.GridLikeControlContentOddColor: TColor;
begin
  Result := DefaultContentOddColor;
end;

function TcxCustomLookAndFeelPainter.GridLikeControlContentTextColor: TColor;
begin
  Result := DefaultContentTextColor;
end;

function TcxCustomLookAndFeelPainter.GridLikeControlDefaultUseOddEvenStyle: Boolean;
begin
  Result := (GridLikeControlContentColor <> GridLikeControlContentOddColor) or
    (GridLikeControlContentOddColor <> GridLikeControlContentEvenColor);
end;

function TcxCustomLookAndFeelPainter.PivotGridHeadersAreaColor: TColor;
begin
  Result := DefaultHeaderBackgroundColor;
end;

function TcxCustomLookAndFeelPainter.PivotGridHeadersAreaTextColor: TColor;
begin
  Result := DefaultHeaderBackgroundTextColor;
end;

function TcxCustomLookAndFeelPainter.LayoutViewGetPadding(AElement: TcxLayoutElement): TRect;
begin
  Result := cxNullRect;
end;

function TcxCustomLookAndFeelPainter.LayoutViewGetSpacing(AElement: TcxLayoutElement): TRect;
begin
  Result := cxNullRect;
end;

procedure TcxCustomLookAndFeelPainter.LayoutViewDrawItem(ACanvas: TcxCanvas;
  const ABounds: TRect; AState: TcxButtonState; ABorders: TcxBorders = []);
var
  AColor: TColor;
begin
  case AState of
    cxbsHot:
      begin
        ACanvas.FrameRect(ABounds, clBtnShadow, 1, cxBordersAll, True);
        AColor := clGradientInactiveCaption;
      end;
    cxbsPressed:
      AColor := DefaultSelectionColor;
    cxbsDisabled:
      AColor := clInactiveCaption;
  else
    AColor := DefaultGroupColor;
  end;
  ACanvas.FillRect(ABounds, AColor);
end;

procedure TcxCustomLookAndFeelPainter.LayoutViewDrawRecordCaption(ACanvas: TcxCanvas; const ABounds, ATextRect: TRect;
  APosition: TcxGroupBoxCaptionPosition; AState: TcxButtonState; AColor: TColor = clDefault; const ABitmap: TBitmap = nil);
begin
  LayoutViewDrawRecordBorder(ACanvas, ABounds, AState, cxBordersAll);
  if (AColor = clDefault) and (ABitmap = nil) then
    AColor := DefaultLayoutViewCaptionColor(AState);
  DrawBackground(ACanvas, ABounds, ABitmap <> nil, AColor, ABitmap);
end;

procedure TcxCustomLookAndFeelPainter.LayoutViewDrawRecordBorder(
  ACanvas: TcxCanvas; const ABounds: TRect; AState: TcxButtonState; ABorders: TcxBorders = []);
var
  ABorderColor: TColor;
begin
  if AState = cxbsHot then
    ABorderColor := DefaultSelectionColor
  else
    ABorderColor := clBtnShadow;
  ACanvas.FrameRect(ABounds, ABorderColor, 1, ABorders, True);
end;

procedure TcxCustomLookAndFeelPainter.LayoutViewDrawRecordContent(
  ACanvas: TcxCanvas; const ABounds: TRect;
  ACaptionPosition: TcxGroupBoxCaptionPosition; AState: TcxButtonState;
  ABorders: TcxBorders = cxBordersAll);
begin
  LayoutViewDrawRecordBorder(ACanvas, ABounds, AState, ABorders);
  ACanvas.FillRect(ABounds, DefaultGroupColor);
end;

procedure TcxCustomLookAndFeelPainter.LayoutViewDrawScaledRecordExpandButton(ACanvas: TcxCanvas;
  const ABounds: TRect; AState: TcxButtonState; AExpanded: Boolean; AScaleFactor: TdxScaleFactor);
begin
  DrawScaledGroupExpandButton(ACanvas, ABounds, AExpanded, AState, AScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.LayoutViewDrawRecordExpandButton(
  ACanvas: TcxCanvas; const ABounds: TRect; AState: TcxButtonState; AExpanded: Boolean);
begin
  LayoutViewDrawScaledRecordExpandButton(ACanvas, ABounds, AState, AExpanded, dxSystemScaleFactor);
end;

function TcxCustomLookAndFeelPainter.LayoutViewRecordCaptionTailSize(
  ACaptionPosition: TcxGroupBoxCaptionPosition): Integer;
begin
  Result := 0;
end;

function TcxCustomLookAndFeelPainter.LayoutViewRecordCaptionTextBold: Boolean;
begin
  Result := False;
end;

function TcxCustomLookAndFeelPainter.WinExplorerViewDefaultRecordColor(AState: TcxButtonState): TColor;
begin
  case AState of
    cxbsHot:
      begin
        Result := clGradientInactiveCaption;
      end;
    cxbsPressed:
        Result := DefaultSelectionColor;
    cxbsDisabled:
      Result := clInactiveCaption;
  else
    Result := DefaultContentColor;
  end;
end;

procedure TcxCustomLookAndFeelPainter.WinExplorerViewDrawGroup(ACanvas: TcxCanvas; const ABounds: TRect; AState: TcxButtonState;
  AColor: TColor = clDefault; const ABitmap: TBitmap = nil);
begin
  WinExplorerViewDrawRecord(ACanvas, ABounds, AState, AColor, ABitmap);
end;

procedure TcxCustomLookAndFeelPainter.WinExplorerViewDrawGroupCaptionLine(ACanvas: TcxCanvas; const ABounds: TRect);
begin
  DrawSeparator(ACanvas, ABounds, False);
end;

procedure TcxCustomLookAndFeelPainter.WinExplorerViewDrawRecord(ACanvas: TcxCanvas; const ABounds: TRect;
  AState: TcxButtonState; AColor: TColor = clDefault; const ABitmap: TBitmap = nil);
begin
  if AState in [cxbsHot, cxbsPressed, cxbsDisabled] then
    ACanvas.FrameRect(ABounds, clBtnShadow, 1, cxBordersAll, True);
  if (AColor = clDefault) and (ABitmap = nil) then
    AColor := WinExplorerViewDefaultRecordColor(AState);
  DrawBackground(ACanvas, ABounds, ABitmap <> nil, AColor, ABitmap);
end;

procedure TcxCustomLookAndFeelPainter.WinExplorerViewDrawRecordExpandButton(
  ACanvas: TcxCanvas; const ABounds: TRect; AState: TcxButtonState; AExpanded: Boolean);
begin
  WinExplorerViewDrawScaledRecordExpandButton(ACanvas, ABounds, AState, AExpanded, dxSystemScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.WinExplorerViewDrawScaledRecordExpandButton(ACanvas: TcxCanvas;
  const ABounds: TRect; AState: TcxButtonState; AExpanded: Boolean; AScaleFactor: TdxScaleFactor);
begin
  DrawScaledGroupExpandButton(ACanvas, ABounds, AExpanded, AState, AScaleFactor);
end;

function TcxCustomLookAndFeelPainter.WinExplorerViewScaledExpandButtonSize(AScaleFactor: TdxScaleFactor): Integer;
begin
  Result := ScaledGroupExpandButtonSize(AScaleFactor);
end;

function TcxCustomLookAndFeelPainter.WinExplorerViewExpandButtonSize: Integer;
begin
  Result := WinExplorerViewScaledExpandButtonSize(dxSystemScaleFactor);
end;

function TcxCustomLookAndFeelPainter.WinExplorerViewGroupCaptionLineHeight: Integer;
begin
  Result := SeparatorSize;
end;

function TcxCustomLookAndFeelPainter.WinExplorerViewGroupTextColor(AState: TcxButtonState): TColor;
begin
  Result := WinExplorerViewRecordTextColor(AState);
end;

function TcxCustomLookAndFeelPainter.WinExplorerViewGroupTextBold: Boolean;
begin
  Result := False;
end;

function TcxCustomLookAndFeelPainter.WinExplorerViewRecordTextColor(AState: TcxButtonState): TColor;
begin
  case AState of
    cxbsPressed:
      Result := DefaultSelectionTextColor;
    cxbsDisabled:
      Result := DefaultInactiveTextColor;
  else
    Result := DefaultContentTextColor;
  end;
end;

function TcxCustomLookAndFeelPainter.FooterBorders: TcxBorders;
begin
  Result := cxBordersAll;
end;

function TcxCustomLookAndFeelPainter.FooterBorderSize: Integer;
begin
  Result := 0;
end;

function TcxCustomLookAndFeelPainter.FooterCellBorderSize: Integer;
begin
  Result := 0;
end;

function TcxCustomLookAndFeelPainter.FooterCellOffset: Integer;
begin
  Result := 0;
end;

function TcxCustomLookAndFeelPainter.FooterDrawCellsFirst: Boolean;
begin
  Result := True;
end;

function TcxCustomLookAndFeelPainter.FooterSeparatorColor: TColor;
begin
  Result := clBtnShadow;
end;

function TcxCustomLookAndFeelPainter.FooterSeparatorSize: Integer;
begin
  Result := 1;
end;

procedure TcxCustomLookAndFeelPainter.DrawFooterPanel(ACanvas: TcxCanvas;
  const ABounds: TRect; const AViewParams: TcxViewParams; ABorders: TcxBorders);
begin
  ACanvas.SaveClipRegion;
  try
    ACanvas.IntersectClipRect(ABounds);
    DrawFooterContent(ACanvas, ABounds, AViewParams);
    DrawFooterBorderEx(ACanvas, ABounds, ABorders);
  finally
    ACanvas.RestoreClipRegion;
  end;
end;

procedure TcxCustomLookAndFeelPainter.DrawFooterBorder(ACanvas: TcxCanvas;
  const R: TRect);
begin
end;

procedure TcxCustomLookAndFeelPainter.DrawFooterBorderEx(ACanvas: TcxCanvas;
  const R: TRect; ABorders: TcxBorders);
var
  ABounds: TRect;
begin
  DrawFooterSeparator(ACanvas, cxRectSetHeight(R, FooterSeparatorSize));
  ABounds := R;
  Inc(ABounds.Top, FooterSeparatorSize);
  DrawFooterBorder(ACanvas, ABounds);
end;

procedure TcxCustomLookAndFeelPainter.DrawFooterCellBorder(ACanvas: TcxCanvas; const R: TRect);
begin
end;

procedure TcxCustomLookAndFeelPainter.DrawFooterContent(ACanvas: TcxCanvas;
  const ARect: TRect; const AViewParams: TcxViewParams);
begin
  if IsRectEmpty(ARect) then Exit;
  ACanvas.FillRect(ARect, AViewParams);
end;

procedure TcxCustomLookAndFeelPainter.DrawFooterSeparator(ACanvas: TcxCanvas;
  const R: TRect);
begin
  ACanvas.Brush.Color := FooterSeparatorColor;
  ACanvas.FillRect(R);
end;

procedure TcxCustomLookAndFeelPainter.DrawFilterActivateButton(
  ACanvas: TcxCanvas; R: TRect; AState: TcxButtonState; AChecked: Boolean);
begin
  DrawScaledFilterActivateButton(ACanvas, R, AState, AChecked, dxSystemScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DrawScaledFilterActivateButton(
  ACanvas: TcxCanvas; R: TRect; AState: TcxButtonState; AChecked: Boolean; AScaleFactor: TdxScaleFactor);
begin
  DrawScaledCheckButton(ACanvas, R, AState, AChecked, AScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DrawFilterCloseButton(ACanvas: TcxCanvas; R: TRect; AState: TcxButtonState);
begin
  DrawScaledFilterCloseButton(ACanvas, R, AState, dxSystemScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DrawScaledFilterCloseButton(
  ACanvas: TcxCanvas; R: TRect; AState: TcxButtonState; AScaleFactor: TdxScaleFactor);
begin
  DrawScaledSmallButton(ACanvas, R, AState, AScaleFactor);
  DrawScaledButtonCross(ACanvas, R, ButtonSymbolColor(AState), ButtonSymbolState(AState), AScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DrawFilterDropDownButton(
  ACanvas: TcxCanvas; R: TRect; AState: TcxButtonState; AIsFilterActive: Boolean);
begin
  DrawScaledFilterDropDownButton(ACanvas, R, AState, AIsFilterActive, dxSystemScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DrawFilterPanel(ACanvas: TcxCanvas;
  const ARect: TRect; ATransparent: Boolean; ABackgroundColor: TColor;
  const ABackgroundBitmap: TGraphic);
var
  ABitmap: TBitmap;
begin
  ABitmap := cxGetAsBitmap(ABackgroundBitmap);
  try
    DrawBackground(ACanvas, ARect, ATransparent, ABackgroundColor, ABitmap);
  finally
    ABitmap.Free;
  end;
end;

procedure TcxCustomLookAndFeelPainter.DrawFilterSmartTag(ACanvas: TcxCanvas;
  R: TRect; AState: TcxFilterSmartTagState; AIsFilterActive: Boolean);
begin
  DrawScaledFilterSmartTag(ACanvas, R, AState, AIsFilterActive, dxSystemScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DrawScaledFilterSmartTag(ACanvas: TcxCanvas;
  R: TRect; AState: TcxFilterSmartTagState; AIsFilterActive: Boolean; AScaleFactor: TdxScaleFactor);
begin
  SmartTagGlyph.StretchDraw(ACanvas.Handle, R, MaxByte, TdxSimpleColorPalette.Create(
    TdxAlphaColors.Empty, dxColorToAlphaColor(GetFilterSmartTagColor(AState, AIsFilterActive))));
end;

function TcxCustomLookAndFeelPainter.ScaledFilterActivateButtonSize(AScaleFactor: TdxScaleFactor): TPoint;
begin
  Result := AScaleFactor.Apply(Point(FilterActiveButtonWidth, FilterActiveButtonHeight));
end;

function TcxCustomLookAndFeelPainter.FilterActivateButtonSize: TPoint;
begin
  Result := ScaledFilterActivateButtonSize(dxSystemScaleFactor);
end;

function TcxCustomLookAndFeelPainter.FilterCloseButtonSize: TPoint;
begin
  Result := ScaledFilterCloseButtonSize(dxSystemScaleFactor);
end;

function TcxCustomLookAndFeelPainter.ScaledFilterCloseButtonSize(AScaleFactor: TdxScaleFactor): TPoint;
begin
  Result := AScaleFactor.Apply(Point(FilterCloseButtonWidth, FilterCloseButtonHeight));
end;

function TcxCustomLookAndFeelPainter.FilterControlMenuGetColorPalette: IdxColorPalette;
begin
  Result := nil;
end;

function TcxCustomLookAndFeelPainter.FilterDropDownButtonSize: TPoint;
begin
  Result := ScaledFilterDropDownButtonSize(dxSystemScaleFactor);
end;

function TcxCustomLookAndFeelPainter.ScaledFilterDropDownButtonSize(AScaleFactor: TdxScaleFactor): TPoint;
begin
  Result := AScaleFactor.Apply(Point(FilterDropDownButtonWidth, 0));
  dxAdjustToTouchableSize(Result.X);
end;

function TcxCustomLookAndFeelPainter.FilterSmartTagSize: TSize;
begin
  Result := ScaledFilterSmartTagSize(dxSystemScaleFactor)
end;

function TcxCustomLookAndFeelPainter.ScaledFilterSmartTagSize(AScaleFactor: TdxScaleFactor): TSize;
begin
  Result := AScaleFactor.Apply(cxSize(10, 10));
end;

procedure TcxCustomLookAndFeelPainter.DrawFindPanel(ACanvas: TcxCanvas; const ARect: TRect; ATransparent: Boolean; ABackgroundColor: TColor; const ABackgroundBitmap: TGraphic);
begin
  DrawFilterPanel(ACanvas, ARect, ATransparent, ABackgroundColor, ABackgroundBitmap);
end;

procedure TcxCustomLookAndFeelPainter.DrawFindPanelBorder(ACanvas: TcxCanvas; const R: TRect; ABorders: TcxBorders);
begin
  DrawFooterBorderEx(ACanvas, R, ABorders);
end;

procedure TcxCustomLookAndFeelPainter.DrawEditPopupWindowBorder(ACanvas: TcxCanvas; var R: TRect;
  ABorderStyle: TcxEditPopupBorderStyle; AClientEdge: Boolean);
begin
  case ABorderStyle of
    epbsSingle:
      ACanvas.FrameRect(R, clBtnText);
    epbsFrame3D, epbsFlat:
      begin
        ACanvas.DrawEdge(R, False, True);
        InflateRect(R, -1, -1);
        ACanvas.DrawEdge(R, False, False);
        if ABorderStyle = epbsFrame3D then
        begin
          InflateRect(R, -1, -1);
          if AClientEdge then
            ACanvas.FrameRect(R, clInactiveBorder)
          else
            ACanvas.FrameRect(R, clBtnFace);
          InflateRect(R, -1, -1);
          if AClientEdge then
            ACanvas.FrameRect(R, clBtnFace)
          else
            ACanvas.DrawEdge(R, True, True);
        end;
      end;
  end;
  InflateRect(R, -1, -1);
end;

procedure TcxCustomLookAndFeelPainter.DrawWindowContent(ACanvas: TcxCanvas;
  const ARect: TRect);
begin
  ACanvas.FillRect(ARect, clBtnFace);
end;

procedure TcxCustomLookAndFeelPainter.DrawScaledZoomInButton(
  ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState; AScaleFactor: TdxScaleFactor);
const
  PlusSignIndent = 3;
begin
  DrawScaledZoomOutButton(ACanvas, R, AState, AScaleFactor);
  ACanvas.MoveTo((R.Left + R.Right) div 2, R.Top + AScaleFactor.Apply(PlusSignIndent));
  ACanvas.LineTo((R.Left + R.Right) div 2, R.Bottom - AScaleFactor.Apply(PlusSignIndent));
end;

procedure TcxCustomLookAndFeelPainter.DrawScaledZoomOutButton(
  ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState; AScaleFactor: TdxScaleFactor);
const
  MinusSignIndent = 3;
var
  ARect: TRect;
begin
  ARect := R;
  DrawButtonBorder(ACanvas, ARect, AState);
  InflateRect(ARect, -1, -1);
  ACanvas.Brush.Color := ButtonColor(AState);
  ACanvas.FillRect(ARect);
  if AState <> cxbsDisabled then
    ACanvas.Pen.Color := ButtonSymbolColor(AState)
  else
    ACanvas.Pen.Color := clBtnShadow;

  ACanvas.MoveTo(R.Left + AScaleFactor.Apply(MinusSignIndent), (R.Top + R.Bottom) div 2);
  ACanvas.LineTo(R.Right - AScaleFactor.Apply(MinusSignIndent), (R.Top + R.Bottom) div 2);
end;

function TcxCustomLookAndFeelPainter.GetEditPopupWindowBorderWidth(AStyle: TcxEditPopupBorderStyle): Integer;
begin
  Result := cxEditPopupWindowFrameWidthA[AStyle];
end;

function TcxCustomLookAndFeelPainter.GetEditPopupWindowClientEdgeWidth(AStyle: TcxEditPopupBorderStyle): Integer;
begin
  Result := cxEditPopupClientEdgeWidthA[AStyle]
end;

function TcxCustomLookAndFeelPainter.PopupBorderStyle: TcxPopupBorderStyle;
begin
  Result := pbsUltraFlat;
end;

function TcxCustomLookAndFeelPainter.GetHintBorderColor: TColor;
begin
  Result := clDefault;
end;

procedure TcxCustomLookAndFeelPainter.DrawHintBackground(ACanvas: TcxCanvas; const ARect: TRect; AColor: TColor);
begin
  ACanvas.FillRect(ARect, cxGetActualColor(AColor, clInfoBk));
end;

function TcxCustomLookAndFeelPainter.ScreenTipGetColorPalette: IdxColorPalette;
begin
  Result := nil;
end;

function TcxCustomLookAndFeelPainter.ScreenTipGetDescriptionTextColor: TColor;
begin
  Result := clDefault;
end;

function TcxCustomLookAndFeelPainter.ScreenTipGetTitleTextColor: TColor;
begin
  Result := clDefault;
end;

function TcxCustomLookAndFeelPainter.ScreenTipGetFooterLineSize: Integer;
begin
  Result := 3;
end;

procedure TcxCustomLookAndFeelPainter.ScreenTipDrawBackground(ACanvas: TcxCanvas; ARect: TRect);

  procedure ScreenTipDrawBorder(ACanvas: TcxCanvas; ARect: TRect);
  begin
    ACanvas.Pen.Color := clWindowFrame;
    ACanvas.Brush.Style := bsClear;
    ACanvas.Canvas.RoundRect(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom, 2, 2);
    ACanvas.Brush.Style := bsSolid;
  end;

begin
  DrawHintBackground(ACanvas, cxRectInflate(ARect, -1, -1));
  ScreenTipDrawBorder(ACanvas, ARect);
end;

procedure TcxCustomLookAndFeelPainter.ScreenTipDrawFooterLine(ACanvas: TcxCanvas; const ARect: TRect);
begin
  ACanvas.DrawComplexFrame(ARect, clWindowFrame, clWindowFrame, [bTop]);
  ACanvas.DrawComplexFrame(cxRectOffset(ARect, 0, 1), clWhite, clWhite, [bTop]);
end;

procedure TcxCustomLookAndFeelPainter.DrawTab(ACanvas: TcxCanvas; R: TRect;
  ABorders: TcxBorders; const AText: string; AState: TcxButtonState;
  AVertical: Boolean; AFont: TFont; ATextColor, ABkColor: TColor; AShowPrefix: Boolean = False);
begin
end;

procedure TcxCustomLookAndFeelPainter.DrawTabBorder(ACanvas: TcxCanvas;
  R: TRect; ABorder: TcxBorder; ABorders: TcxBorders; AVertical: Boolean);
begin
end;

procedure TcxCustomLookAndFeelPainter.DrawTabsRoot(ACanvas: TcxCanvas;
  const R: TRect; ABorders: TcxBorders; AVertical: Boolean);
begin
end;

function TcxCustomLookAndFeelPainter.IsDrawTabImplemented(AVertical: Boolean): Boolean;
begin
  Result := False;
end;

function TcxCustomLookAndFeelPainter.IsTabHotTrack(AVertical: Boolean): Boolean;
begin
  Result := False;
end;

function TcxCustomLookAndFeelPainter.TabBorderSize(AVertical: Boolean): Integer;
begin
  Result := 0;
end;

procedure TcxCustomLookAndFeelPainter.DrawScaledIndicatorItem(ACanvas: TcxCanvas; const R: TRect;
  AKind: TcxIndicatorKind; AColor: TColor; AScaleFactor: TdxScaleFactor; AOnDrawBackground: TcxDrawBackgroundEvent = nil);
begin
  DrawScaledIndicatorItem(ACanvas, R, R, AKind, AColor, AScaleFactor, AOnDrawBackground);
end;

procedure TcxCustomLookAndFeelPainter.DrawIndicatorItem(ACanvas: TcxCanvas;
  const R: TRect; AKind: TcxIndicatorKind; AColor: TColor; AOnDrawBackground: TcxDrawBackgroundEvent = nil);
begin
  DrawScaledIndicatorItem(ACanvas, R, AKind, AColor, dxSystemScaleFactor, AOnDrawBackground);
end;

procedure TcxCustomLookAndFeelPainter.DrawIndicatorItem(ACanvas: TcxCanvas; const R, AImageAreaBounds: TRect;
  AKind: TcxIndicatorKind; AColor: TColor; AOnDrawBackground: TcxDrawBackgroundEvent = nil);
begin
  DrawScaledIndicatorItem(ACanvas, R, AImageAreaBounds, AKind, AColor, dxSystemScaleFactor, AOnDrawBackground);
end;

procedure TcxCustomLookAndFeelPainter.DrawScaledIndicatorItem(ACanvas: TcxCanvas; const R, AImageAreaBounds: TRect;
  AKind: TcxIndicatorKind; AColor: TColor; AScaleFactor: TdxScaleFactor; AOnDrawBackground: TcxDrawBackgroundEvent = nil;
  ANeighbors: TcxNeighbors = [nTop, nBottom]);
begin
  DrawScaledHeader(ACanvas, R, R, [], HeaderBorders(ANeighbors), cxbsNormal,
    taLeftJustify, vaTop, False, False, '', nil, clNone, AColor, AScaleFactor, AOnDrawBackground);
  DrawScaledIndicatorImage(ACanvas, AImageAreaBounds, AKind, AScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DrawIndicatorItemEx(
  ACanvas: TcxCanvas; const R: TRect; AKind: TcxIndicatorKind;
  AColor: TColor; AOnDrawBackground: TcxDrawBackgroundEvent = nil);
begin
  DrawScaledIndicatorItemEx(ACanvas, R, AKind, AColor, dxSystemScaleFactor, AOnDrawBackground);
end;

procedure TcxCustomLookAndFeelPainter.DrawScaledIndicatorItemEx(ACanvas: TcxCanvas; const R: TRect;
  AKind: TcxIndicatorKind; AColor: TColor; AScaleFactor: TdxScaleFactor; AOnDrawBackground: TcxDrawBackgroundEvent = nil);
begin
  DrawScaledHeaderEx(ACanvas, R, R, [], cxBordersAll, cxbsNormal,
    taLeftJustify, vaTop, False, False, '', nil, clNone, AColor, AScaleFactor, AOnDrawBackground);
  DrawScaledIndicatorImage(ACanvas, R, AKind, AScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DrawScaledIndicatorCustomizationMark(
  ACanvas: TcxCanvas; const R: TRect; AColor: TColor; AScaleFactor: TdxScaleFactor);
const
  LineCount = 5;
  LineHeight = 1;
  LineOffset = 3;

  procedure DrawLine(X, Y, ALineWidth: Integer; AChecked: Boolean);
  var
    ASize: Integer;
  begin
    ASize := AScaleFactor.Apply(LineHeight);
    if AChecked then
      ACanvas.FillRect(cxRectBounds(X, Y, ASize, ASize), AColor);
    ACanvas.FillRect(cxRectBounds(X + ASize + AScaleFactor.Apply(1), Y, ALineWidth - ASize - AScaleFactor.Apply(1), ASize));
  end;

var
  ALineWidth: Integer;
  X, Y, I: Integer;
begin
  ALineWidth := cxRectWidth(R) - 2 * AScaleFactor.Apply(LineOffset);
  X := (R.Left + AScaleFactor.Apply(LineOffset));
  Y := (R.Top + R.Bottom - ((AScaleFactor.Apply(LineHeight) + 1) * LineCount - 1)) div 2;

  ACanvas.Pen.Color := AColor;
  for I := 0 to LineCount - 1 do
  begin
    DrawLine(X, Y, ALineWidth, not Odd(I));
    Inc(Y, AScaleFactor.Apply(2));
  end;
end;

function TcxCustomLookAndFeelPainter.IndicatorDrawItemsFirst: Boolean;
begin
  Result := True;
end;

function TcxCustomLookAndFeelPainter.ScaledScrollBarMinimalThumbSize(
  AVertical: Boolean; AScaleFactor: TdxScaleFactor): Integer;
begin
  Result := AScaleFactor.Apply(cxStdThumbnailMinimalSize);
end;

function TcxCustomLookAndFeelPainter.ScrollBarMinimalThumbSize(AVertical: Boolean): Integer;
begin
  Result := ScaledScrollBarMinimalThumbSize(AVertical, dxSystemScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DrawScaledScrollBarBackground(
  ACanvas: TcxCanvas; const R: TRect; AHorizontal: Boolean; AScaleFactor: TdxScaleFactor);
begin
  DrawScaledScrollBarPart(ACanvas, AHorizontal, R, sbpPageUp, cxbsNormal, AScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DrawScrollBarBackground(
  ACanvas: TcxCanvas; const R: TRect; AHorizontal: Boolean);
begin
  DrawScaledScrollBarBackground(ACanvas, R, AHorizontal, dxSystemScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DrawScaledScrollBarPart(ACanvas: TcxCanvas; AHorizontal: Boolean;
  R: TRect; APart: TcxScrollBarPart; AState: TcxButtonState; AScaleFactor: TdxScaleFactor);
var
  ARealPart: TcxScrollBarPart;
begin
  ARealPart := APart;
  if AHorizontal and ACanvas.UseRightToLeftAlignment then
    case APart of
      sbpLineUp: ARealPart := sbpLineDown;
      sbpLineDown: ARealPart := sbpLineUp;
    end;
  DoDrawScaledScrollBarPart(ACanvas, AHorizontal, R, ARealPart, AState, AScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DrawScrollBarPart(ACanvas: TcxCanvas;
  AHorizontal: Boolean; R: TRect; APart: TcxScrollBarPart; AState: TcxButtonState);
begin
  DrawScaledScrollBarPart(ACanvas, AHorizontal, R, APart, AState, dxSystemScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DrawScrollBarSplitter(ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState);
begin
  DrawScaledScrollBarSplitter(ACanvas, R, AState, dxSystemScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DrawScaledScrollBarSplitter(
  ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState; AScaleFactor: TdxScaleFactor);
begin
  DrawNavigatorScaledButton(ACanvas, R, AState, clDefault, AScaleFactor);
  DrawNavigatorBorder(ACanvas, R, False);
  DrawScaledSplitter(ACanvas, cxTextRect(R), AState = cxbsHot, AState = cxbsPressed, False, AScaleFactor);
end;

function TcxCustomLookAndFeelPainter.ScaledPopupPanelSize(AScaleFactor: TdxScaleFactor): Integer;
begin
  Result := 2 + dxGetSystemMetrics(SM_CYHSCROLL, AScaleFactor);
  Inc(Result);
  Result := Max(Max(Result, ScaledSizeGripSize(AScaleFactor).cy + 2 * 2), ScaledSmallCloseButtonSize(AScaleFactor).cy + 2 * 3);
end;

function TcxCustomLookAndFeelPainter.PopupPanelSize: Integer;
begin
  Result := ScaledPopupPanelSize(dxSystemScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.CalculatePopupPanelClientRect(
  var AClientRect, APanelRect, AGripRect, ACloseButtonRect: TRect;
  ACorner: TdxCorner; const ABorders, APanelBorders: TRect;
  APanelHeight: Integer = 0; AShowCloseButton: Boolean = True; AShowGripSize: Boolean = True);
begin
  CalculateScaledPopupPanelClientRect(AClientRect, APanelRect, AGripRect, ACloseButtonRect, ACorner, ABorders,
    APanelBorders, dxSystemScaleFactor, APanelHeight, AShowCloseButton, AShowGripSize);
end;

procedure TcxCustomLookAndFeelPainter.CalculateScaledPopupPanelClientRect(
  var AClientRect, APanelRect, AGripRect, ACloseButtonRect: TRect;
  ACorner: TdxCorner; const ABorders, APanelBorders: TRect; AScaleFactor: TdxScaleFactor;
  APanelHeight: Integer = 0; AShowCloseButton: Boolean = True; AShowGripSize: Boolean = True);

  procedure CalculateClientRect;
  begin
    AClientRect := cxRectContent(AClientRect, ABorders);
    APanelRect := AClientRect;
    if ACorner in [coBottomLeft, coBottomRight] then
    begin
      APanelRect.Top := APanelRect.Bottom - APanelHeight;
      AClientRect.Bottom := APanelRect.Top;
    end
    else
    begin
      APanelRect.Bottom := APanelRect.Top + APanelHeight;
      AClientRect.Top := APanelRect.Bottom;
    end;
  end;

  procedure CalculateSizeGripRect;
  const
    AOffset = 2;
  var
    AGripSize: TSize;
  begin
    if AShowGripSize then
    begin
      AGripRect := APanelRect;
      AGripSize := ScaledSizeGripSize(AScaleFactor);
      with AGripRect do
      begin
        if ACorner in [coTopRight, coBottomRight] then
        begin
          Right := Right - AOffset;
          Left := Right - AGripSize.cx;
        end
        else
        begin
          Left := Left + AOffset;
          Right := Left + AGripSize.cx;
        end;
        if ACorner in [coBottomLeft, coBottomRight] then
        begin
          Bottom := Bottom - AOffset;
          Top := Bottom - AGripSize.cy;
        end
        else
        begin
          Top := Top + AOffset;
          Bottom := Top + AGripSize.cy;
        end;
      end;
    end
    else
      AGripRect := cxNullRect;
  end;

  procedure CalculateCloseButtonRect;
  begin
    if AShowCloseButton then
    begin
      ACloseButtonRect := cxRectContent(APanelRect, APanelBorders);
        if ACorner in [coTopRight, coBottomRight] then
          ACloseButtonRect.Right := ACloseButtonRect.Left + cxRectHeight(ACloseButtonRect)
        else
          ACloseButtonRect.Left := ACloseButtonRect.Right - cxRectHeight(ACloseButtonRect);
      ACloseButtonRect := cxRectCenter(ACloseButtonRect, ScaledSmallCloseButtonSize(AScaleFactor));
    end
    else
      ACloseButtonRect := cxNullRect;
  end;

begin
  if APanelHeight = 0 then
    APanelHeight := ScaledPopupPanelSize(AScaleFactor);
  CalculateClientRect;
  CalculateSizeGripRect;
  CalculateCloseButtonRect;
end;

procedure TcxCustomLookAndFeelPainter.DrawPopupNCPanel(AHandle: HWND;
  AMouseAboveCloseButton, ACloseButtonIsTracking: Boolean; ACorner: TdxCorner;
  ACloseButtonRect, AGripRect: TRect; ABorderColor: TColor);
begin
  DrawScaledPopupNCPanel(AHandle, AMouseAboveCloseButton, ACloseButtonIsTracking,
    ACorner, ACloseButtonRect, AGripRect, ABorderColor, dxSystemScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DrawScaledPopupNCPanel(
  AHandle: HWND; AMouseAboveCloseButton, ACloseButtonIsTracking: Boolean;
  ACorner: TdxCorner; ACloseButtonRect, AGripRect: TRect; ABorderColor: TColor; AScaleFactor: TdxScaleFactor);
var
  AWindowRect, AWindowBounds, ABandBounds: TRect;
  ABorderSize: Integer;
  ADC: HDC;

  procedure CalculateBounds;
  var
    AClientRect, AClientBounds: TRect;
  begin
    AWindowRect := cxGetWindowRect(AHandle);
    AClientRect := cxGetClientRect(AHandle);

    AWindowBounds := AWindowRect;
    OffsetRect(AWindowBounds, -AWindowRect.Left, -AWindowRect.Top);

    AClientBounds := AClientRect;
    MapWindowPoints(AHandle, 0, AClientBounds, 2);
    OffsetRect(AClientBounds, -AWindowRect.Left, -AWindowRect.Top);

    ABorderSize := AClientBounds.Left;

    ABandBounds := cxRectInflate(AWindowBounds, -ABorderSize, -ABorderSize);
    if ACorner in [coBottomLeft, coBottomRight] then
      ABandBounds.Top := AClientBounds.Bottom
    else
      ABandBounds.Bottom := AClientBounds.Top;

    OffsetRect(ACloseButtonRect, -AWindowRect.Left, -AWindowRect.Top);
    OffsetRect(AGripRect, -AWindowRect.Left, -AWindowRect.Top);
  end;

  function GetBorders: TRect;
  begin
    Result := cxEmptyRect;
    if ACorner in [coBottomLeft, coBottomRight] then
      Result.Top := 1
    else
      Result.Bottom := 1;
  end;

  function GetButtonState: TcxButtonState;
  begin
    if AMouseAboveCloseButton and ACloseButtonIsTracking then
      Result := cxbsPressed
    else
      if AMouseAboveCloseButton or ACloseButtonIsTracking then
        Result := cxbsHot
      else
        Result := cxbsNormal;
  end;

begin
  ADC := GetWindowDC(AHandle);
  cxPaintCanvas.BeginPaint(ADC);
  try
    CalculateBounds;
    cxPaintCanvas.FrameRect(AWindowBounds, ABorderColor, ABorderSize);
    DrawScaledPopupPanelBand(cxPaintCanvas, ABandBounds, AGripRect, ACloseButtonRect,
      ACorner, GetButtonState, GetBorders, ABorderColor, AScaleFactor);
  finally
    cxPaintCanvas.EndPaint;
    ReleaseDC(AHandle, ADC);
  end;
end;

procedure TcxCustomLookAndFeelPainter.DrawPopupPanelBand(
  ACanvas: TcxCanvas; const ABounds, AGripRect, ACloseButtonRect: TRect; AGripCorner: TdxCorner;
  ACloseButtonState: TcxButtonState; ABorders: TRect; ABorderColor: TColor; AShowGripSize: Boolean; AShowCloseButton: Boolean);
begin
  DrawScaledPopupPanelBand(ACanvas, ABounds, AGripRect, ACloseButtonRect, AGripCorner,
    ACloseButtonState, ABorders, ABorderColor, dxSystemScaleFactor, AShowGripSize, AShowCloseButton);
end;

procedure TcxCustomLookAndFeelPainter.DrawScaledPopupPanelBand(
  ACanvas: TcxCanvas; const ABounds, AGripRect, ACloseButtonRect: TRect; AGripCorner: TdxCorner;
  ACloseButtonState: TcxButtonState; ABorders: TRect; ABorderColor: TColor; AScaleFactor: TdxScaleFactor;
  AShowGripSize: Boolean = True; AShowCloseButton: Boolean = True);
var
  ABitmap: TcxBitmap;
begin
  ACanvas.SaveClipRegion;
  ACanvas.ExcludeClipRect(cxRectContent(ABounds, ABorders));
  ACanvas.FillRect(ABounds, ABorderColor);
  ACanvas.RestoreClipRegion;

  if AShowGripSize then
  begin
    ABitmap := TcxBitmap.CreateSize(AGripRect);
    try
      DrawWindowContent(ABitmap.cxCanvas, ABitmap.ClientRect);
      DrawScaledSizeGrip(ABitmap.cxCanvas, ABitmap.ClientRect, AScaleFactor, clNone, AGripCorner);
      cxDrawBitmap(ACanvas.Handle, ABitmap, AGripRect, cxNullPoint);
    finally
      ABitmap.Free;
    end;
    ACanvas.ExcludeClipRect(AGripRect);
  end;

  if AShowCloseButton then
  begin
    DrawWindowContent(ACanvas, ACloseButtonRect);
    DrawScaledSmallCloseButton(ACanvas, ACloseButtonRect, ACloseButtonState, AScaleFactor);
    ACanvas.ExcludeClipRect(ACloseButtonRect);
  end;

  DrawWindowContent(ACanvas, cxRectContent(ABounds, ABorders));
  ACanvas.ExcludeClipRect(ABounds);
end;

function TcxCustomLookAndFeelPainter.ScaledSizeGripSize(AScaleFactor: TdxScaleFactor): TSize;
begin
  Result := Size(dxGetSystemMetrics(SM_CXVSCROLL, AScaleFactor), dxGetSystemMetrics(SM_CYHSCROLL, AScaleFactor));
end;

function TcxCustomLookAndFeelPainter.SizeGripSize: TSize;
begin
  Result := ScaledSizeGripSize(dxSystemScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DoDrawSizeGrip(ACanvas: TcxCanvas; const ARect: TRect);
begin
  DoDrawSizeGrip(ACanvas, ARect, dxSystemScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DoDrawSizeGrip(ACanvas: TcxCanvas; const ARect: TRect; AScaleFactor: TdxScaleFactor);
begin
  DrawFrameControl(ACanvas.Handle, ARect, DFC_SCROLL, DFCS_SCROLLSIZEGRIP);
end;

procedure TcxCustomLookAndFeelPainter.DrawSizeGrip(ACanvas: TcxCanvas;
  const ARect: TRect; ABackgroundColor: TColor; ACorner: TdxCorner);
begin
  DrawScaledSizeGrip(ACanvas, ARect, dxSystemScaleFactor, ABackgroundColor, ACorner);
end;

procedure TcxCustomLookAndFeelPainter.DrawScaledSizeGrip(ACanvas: TcxCanvas; const ARect: TRect;
  AScaleFactor: TdxScaleFactor; ABackgroundColor: TColor = clDefault; ACorner: TdxCorner = coBottomRight);
begin
  dxRotateSizeGrip(ACanvas, ARect, AScaleFactor, ABackgroundColor, ACorner, DoDrawSizeGrip);
end;

function TcxCustomLookAndFeelPainter.SliderButtonSize(ADirection: TcxArrowDirection): TSize;
begin
  Result := ScaledSliderButtonSize(ADirection, dxSystemScaleFactor);
end;

function TcxCustomLookAndFeelPainter.ScaledSliderButtonSize(ADirection: TcxArrowDirection; AScaleFactor: TdxScaleFactor): TSize;
begin
  Result := ScaledSizeGripSize(AScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DrawScaledSliderButton(ACanvas: TcxCanvas; const ARect: TRect;
  ADirection: TcxArrowDirection; AState: TcxButtonState; AScaleFactor: TdxScaleFactor);
const
  APart: array[Boolean] of TcxScrollBarPart = (sbpLineUp, sbpLineDown);
begin
  DrawScaledScrollBarPart(ACanvas, ADirection in [adLeft, adRight], ARect, APart[ADirection in [adRight, adDown]], AState, AScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DrawSliderButton(ACanvas: TcxCanvas;
  const ARect: TRect; ADirection: TcxArrowDirection; AState: TcxButtonState);
begin
  DrawScaledSliderButton(ACanvas, ARect, ADirection, AState, dxSystemScaleFactor);
end;

function TcxCustomLookAndFeelPainter.GetSmallButtonColorPalette(AState: TcxButtonState): IdxColorPalette;
begin
  Result := nil;
end;

function TcxCustomLookAndFeelPainter.GetSmartTagGlyph: TdxSmartImage;
begin
  if FSmartTagGlyph = nil then
  begin
    FSmartTagGlyph := TdxSmartImage.Create;
    FSmartTagGlyph.LoadFromResource(HInstance, 'CX_SMARTTAG', 'SVG');
  end;
  Result := FSmartTagGlyph;
end;

function TcxCustomLookAndFeelPainter.ScaledSmallCloseButtonSize(AScaleFactor: TdxScaleFactor): TSize;
begin
  Result := AScaleFactor.Apply(DoGetSmallCloseButtonSize);
  dxAdjustToTouchableSize(Result);
end;

function TcxCustomLookAndFeelPainter.SmallCloseButtonSize: TSize;
begin
  Result := ScaledSmallCloseButtonSize(dxSystemScaleFactor);
end;

function TcxCustomLookAndFeelPainter.DoGetSmallCloseButtonSize: TSize;
begin
  Result := cxSize(FilterCloseButtonHeight, FilterCloseButtonHeight + 1);
end;

procedure TcxCustomLookAndFeelPainter.DrawScaledSmallButton(
  ACanvas: TcxCanvas; R: TRect; AState: TcxButtonState; AScaleFactor: TdxScaleFactor);
begin
  DrawButtonBorder(ACanvas, R, AState);
  InflateRect(R, -ButtonBorderSize, -ButtonBorderSize);
  ACanvas.Brush.Color := ButtonColor(AState);
  ACanvas.FillRect(R);
end;

procedure TcxCustomLookAndFeelPainter.DrawSmallButton(ACanvas: TcxCanvas; R: TRect; AState: TcxButtonState);
begin
  DrawScaledSmallButton(ACanvas, R, AState, dxSystemScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DrawScaledSmallCloseButton(
  ACanvas: TcxCanvas; R: TRect; AState: TcxButtonState; AScaleFactor: TdxScaleFactor);
begin
  DrawScaledFilterCloseButton(ACanvas, R, AState, AScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DrawSmallCloseButton(ACanvas: TcxCanvas; R: TRect; AState: TcxButtonState);
begin
  DrawScaledSmallCloseButton(ACanvas, R, AState, dxSystemScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DrawScaledMonthHeader(ACanvas: TcxCanvas; const ABounds: TRect;
  const AText: string; ANeighbors: TcxNeighbors; const AViewParams: TcxViewParams; AArrows: TcxArrowDirections;
  ASideWidth: Integer; AScaleFactor: TdxScaleFactor; AOnDrawBackground: TcxDrawBackgroundEvent = nil);
begin
  DrawScaledHeader(ACanvas, ABounds, ABounds, ANeighbors, HeaderBorders(ANeighbors),
    cxbsNormal, taCenter, vaCenter, False, False, AText, AViewParams.Font,
    AViewParams.TextColor, AViewParams.Color, AScaleFactor, AOnDrawBackground);
  DrawMonthHeaderArrows(ACanvas, ABounds, AArrows, ASideWidth, clWindowText);
end;

procedure TcxCustomLookAndFeelPainter.DrawMonthHeader(ACanvas: TcxCanvas;
  const ABounds: TRect; const AText: string; ANeighbors: TcxNeighbors;
  const AViewParams: TcxViewParams; AArrows: TcxArrowDirections;
  ASideWidth: Integer; AOnDrawBackground: TcxDrawBackgroundEvent = nil);
begin
  DrawScaledMonthHeader(ACanvas, ABounds, AText, ANeighbors, AViewParams, AArrows, ASideWidth, dxSystemScaleFactor, AOnDrawBackground);
end;

procedure TcxCustomLookAndFeelPainter.CalculateSchedulerNavigationButtonRects(AIsNextButton: Boolean;
  ACollapsed: Boolean; APrevButtonTextSize: TSize; ANextButtonTextSize: TSize; var ABounds: TRect;
  out ATextRect: TRect; out AArrowRect: TRect; AScaleFactor: TdxScaleFactor; const AIsVertical: Boolean = True);
const
  cxTextOffset = 5;
var
  AArrowSize: TSize;
  AArrowZoneLength: Integer;
  ABorders: TRect;
  AContent: TRect;
  ADelta: Integer;
  AHasTextArea: Boolean;
  R: TRect;
begin
  SchedulerNavigationButtonSizes(AIsNextButton, ABorders, AArrowSize, AHasTextArea, AScaleFactor, AIsVertical);
  if AIsVertical then
  begin
    AArrowZoneLength := 3 * AArrowSize.cy;
    if ACollapsed or not AHasTextArea then
    begin
      ATextRect := cxNullRect;
      ABounds.Top := (cxMarginsHeight(ABounds) - cxMarginsHeight(ABorders) - AArrowZoneLength) div 2;
      ABounds.Bottom := ABounds.Top + AArrowZoneLength + cxMarginsHeight(ABorders);
      AArrowRect := cxRectCenter(ABounds, AArrowSize);
    end
    else
    begin
      AContent := cxRectContent(ABounds, ABorders);
      AArrowRect := cxRectCenter(cxRectSetHeight(AContent, AArrowZoneLength), AArrowSize);
      ATextRect := AContent;
      Inc(ATextRect.Top, AArrowZoneLength + AScaleFactor.Apply(cxTextOffset));
      Dec(ATextRect.Bottom, 2 * AArrowSize.cy);
      ADelta := Max(ANextButtonTextSize.cx, APrevButtonTextSize.cx) - cxRectHeight(ATextRect);
      if ADelta > 0 then
      begin
        OffsetRect(AArrowRect, 0, -ADelta);
        InflateRect(ATextRect, 0, ADelta);
        InflateRect(ABounds, 0, ADelta);
      end;
    end;
  end
  else
  begin
    AArrowZoneLength := 3 * AArrowSize.cx;
    if ACollapsed or not AHasTextArea then
    begin
      ATextRect := cxNullRect;
      ABounds.Left := (cxMarginsWidth(ABounds) - cxMarginsWidth(ABorders) - AArrowZoneLength) div 2;
      ABounds.Right := ABounds.Left + AArrowZoneLength + cxMarginsWidth(ABorders);
      AArrowRect := cxRectCenter(ABounds, AArrowSize);
    end
    else
    begin
      AContent := cxRectContent(ABounds, ABorders);
      R := cxRectSetWidth(AContent, AArrowZoneLength);
      AArrowRect := cxRectCenter(cxRectOffsetHorz(R, AContent.Right - R.Right), AArrowSize);
      ATextRect := AContent;
      Inc(ATextRect.Left, 2 * AArrowSize.cx);
      Dec(ATextRect.Right, AArrowZoneLength + AScaleFactor.Apply(cxTextOffset));
      ADelta := Max(ANextButtonTextSize.cx, APrevButtonTextSize.cx) - cxRectWidth(ATextRect);
      if ADelta > 0 then
      begin
        OffsetRect(AArrowRect, ADelta, 0);
        InflateRect(ATextRect, ADelta, 0);
        InflateRect(ABounds, ADelta, 0);
      end;
    end;
  end
end;

procedure TcxCustomLookAndFeelPainter.CalculateSchedulerNavigationButtonRects(AIsNextButton, ACollapsed: Boolean;
  APrevButtonTextSize, ANextButtonTextSize: TSize; var ABounds: TRect; out ATextRect, AArrowRect: TRect;
  const AIsVertical: Boolean = True);
begin
  CalculateSchedulerNavigationButtonRects(AIsNextButton, ACollapsed, APrevButtonTextSize, ANextButtonTextSize,
    ABounds, ATextRect, AArrowRect, dxSystemScaleFactor, AIsVertical);
end;

procedure TcxCustomLookAndFeelPainter.DrawSchedulerBorder(ACanvas: TcxCanvas; R: TRect);
begin
  DrawBorder(ACanvas, R);
end;

procedure TcxCustomLookAndFeelPainter.DrawSchedulerDayHeader(ACanvas: TcxCanvas; const ABounds, ATextAreaBounds: TRect;
  ANeighbors: TcxNeighbors; ABorders: TcxBorders; AState: TcxButtonState;
  AAlignmentHorz: TAlignment; AAlignmentVert: TcxAlignmentVert; AMultiLine, AShowEndEllipsis: Boolean;
  const AText: string; AFont: TFont; ATextColor, ABkColor: TColor;
  AScaleFactor: TdxScaleFactor; AOnDrawBackground: TcxDrawBackgroundEvent = nil;
  AIsLast: Boolean = False; AIsGroup: Boolean = False);
begin
  AOnDrawBackground(ACanvas, ABounds);
end;

procedure TcxCustomLookAndFeelPainter.DrawSchedulerEventProgress(
  ACanvas: TcxCanvas; const ABounds, AProgress: TRect; AViewParams: TcxViewParams;
  ATransparent: Boolean);
begin
  if ATransparent then
    ACanvas.FrameRect(ABounds, clGray)
  else
    ACanvas.Rectangle(ABounds, AViewParams, cxBordersAll, clGray);
  ACanvas.FillRect(AProgress, clNavy);
end;

procedure TcxCustomLookAndFeelPainter.DrawSchedulerGroup(
  ACanvas: TcxCanvas; const R: TRect; AColor: TColor = clDefault);
var
  APoints: TPoints;
begin
  APoints := cxGetSchedulerGroupPolygon(R);
  try
    ACanvas.Pen.Color := clBlack;
    ACanvas.SetBrushColor(AColor);
    ACanvas.Polygon(APoints);
  finally
    SetLength(APoints, 0);
  end;
end;

procedure TcxCustomLookAndFeelPainter.DrawSchedulerGroupSeparator(
  ACanvas: TcxCanvas; const ABounds: TRect; ABackgroundColor: TColor; AOnDrawBackground: TcxDrawBackgroundEvent);
begin
  DrawSchedulerScaledGroupSeparator(ACanvas, ABounds, ABackgroundColor, dxSystemScaleFactor, AOnDrawBackground);
end;

procedure TcxCustomLookAndFeelPainter.DrawSchedulerScaledGroupSeparator(ACanvas: TcxCanvas; const ABounds: TRect;
  ABackgroundColor: TColor; AScaleFactor: TdxScaleFactor; AOnDrawBackground: TcxDrawBackgroundEvent = nil);
begin
  DrawScaledHeader(ACanvas, ABounds, ABounds, [nRight, nLeft], cxBordersAll, cxbsNormal,
    taLeftJustify, vaCenter, False, False, '', nil, clNone, ABackgroundColor, AScaleFactor, AOnDrawBackground);
end;

procedure TcxCustomLookAndFeelPainter.DrawSchedulerMilestone(ACanvas: TcxCanvas; const R: TRect);
var
  APoints: TPoints;
begin
  APoints := cxGetSchedulerMilestonePolygon(R);
  try
    ACanvas.Pen.Color := clBlack;
    ACanvas.SetBrushColor(clGray);
    ACanvas.Polygon(APoints);
  finally
    SetLength(APoints, 0);
  end;
end;

procedure TcxCustomLookAndFeelPainter.DrawSchedulerNavigationButton(
  ACanvas: TcxCanvas; const ARect: TRect; AIsNextButton: Boolean;
  AState: TcxButtonState; const AText: string; const ATextRect: TRect;
  const AArrowRect: TRect; const AIsVertical: Boolean = True);
const
  AAngle: array[Boolean] of TcxRotationAngle = (ra0, raPlus90);
begin
  DrawSchedulerNavigationButtonContent(ACanvas, ARect, AArrowRect, AIsNextButton, AState, AIsVertical);
  if not IsRectEmpty(ATextRect) and (SchedulerNavigationButtonTextColor(AIsNextButton, AState, clDefault) <> clNone) then
    cxDrawText(ACanvas, AText, ATextRect, DT_CENTER or DT_VCENTER or DT_SINGLELINE,
      SchedulerNavigationButtonTextColor(AIsNextButton, AState, ACanvas.Font.Color), AAngle[AIsVertical]);
end;

procedure TcxCustomLookAndFeelPainter.DrawSchedulerNavigationButtonArrow(ACanvas: TcxCanvas; const ARect: TRect;
  AIsNextButton: Boolean; AColor: TColor; const AForVertical: Boolean);
const
  ASigns: array[Boolean] of Integer = (-1, 1);
var
  ACount: Integer;
  I: Integer;
  R1, R2: TRect;
begin
  ACanvas.Brush.Color := AColor;
  if AForVertical then
  begin
    ACount := (ARect.Bottom - ARect.Top) div 2 + Integer(Odd(ARect.Bottom - ARect.Top));
    with ARect do
      if AIsNextButton then
      begin
        R1 := Rect(Left, Top, Left + 2, Top + 1);
        R2 := Rect(Left, Bottom - 1, Left + 2, Bottom);
      end
      else
      begin
        R1 := Rect(Right - 2, Top, Right, Top + 1);
        R2 := Rect(Right - 2, Bottom - 1, Right, Bottom);
      end;

    for I := 0 to ACount - 1 do
    begin
      ACanvas.FillRect(R1);
      OffsetRect(R1, ASigns[AIsNextButton], 1);
      ACanvas.FillRect(R2);
      OffsetRect(R2, ASigns[AIsNextButton], -1);
    end;

    if AIsNextButton then
      ACanvas.FillRect(cxRectSetLeft(ARect, ARect.Right - 1, 1))
    else
      ACanvas.FillRect(cxRectSetRight(ARect, ARect.Left + 1, 1));
  end
  else
  begin
    ACount := (ARect.Right - ARect.Left) div 2 + Integer(Odd(ARect.Right - ARect.Left));
    with ARect do
      if AIsNextButton then
      begin
        R1 := Rect(Left, Top, Left + 1, Top + 2);
        R2 := Rect(Right - 1, Top, Right, Top + 2);
      end
      else
      begin
        R1 := Rect(Left, Bottom - 2, Left + 1, Bottom);
        R2 := Rect(Right - 1, Bottom - 2, Right, Bottom);
      end;

    for I := 0 to ACount - 1 do
    begin
      ACanvas.FillRect(R1);
      OffsetRect(R1, 1, ASigns[AIsNextButton]);
      ACanvas.FillRect(R2);
      OffsetRect(R2, -1, ASigns[AIsNextButton]);
    end;

    if AIsNextButton then
      ACanvas.FillRect(cxRectSetBottom(ARect, ARect.Bottom, 1))
    else
      ACanvas.FillRect(cxRectSetTop(ARect, ARect.Top, 1));
  end;
end;

procedure TcxCustomLookAndFeelPainter.DrawSchedulerNavigationButtonContent(
  ACanvas: TcxCanvas; const ARect: TRect; const AArrowRect: TRect;
  AIsNextButton: Boolean; AState: TcxButtonState; const AIsVertical: Boolean = True);
const
  Borders: array[Boolean, Boolean] of TcxBorders = (
    ([bLeft, bRight, bBottom], [bLeft, bTop, bRight]),
    ([bRight, bTop, bBottom], [bLeft, bTop, bBottom]) );
  BottomRightColors: array[Boolean] of TColor =
    (clBtnFace, clBtnHighlight);
  TopLeftColors: array[Boolean] of TColor =
    (clBtnHighlight, clBtnShadow);
var
  R: TRect;
begin
  R := ARect;
  ACanvas.FillRect(R, clBtnFace);
  ACanvas.DrawComplexFrame(R, clBtnShadow, clBtnShadow, Borders[AIsVertical, AIsNextButton]);
  if AIsVertical then
  begin
    InflateRect(R, 0, -1);
    if AIsNextButton then
      Inc(R.Left)
    else
      Dec(R.Right);
  end
  else
  begin
    InflateRect(R, -1, 0);
    if AIsNextButton then
      Inc(R.Top)
    else
      Dec(R.Bottom);
  end;
  ACanvas.DrawComplexFrame(R, TopLeftColors[AState = cxbsPressed],
    BottomRightColors[AState = cxbsPressed], Borders[AIsVertical, AIsNextButton]);
  DrawSchedulerNavigationButtonArrow(ACanvas, AArrowRect, AIsNextButton,
    ButtonSymbolColor(AState), AIsVertical);
end;

procedure TcxCustomLookAndFeelPainter.DrawSchedulerNavigatorButton(
  ACanvas: TcxCanvas; R: TRect; AState: TcxButtonState);
begin
  DrawSchedulerScaledNavigatorButton(ACanvas, R, AState, dxSystemScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DrawSchedulerScaledNavigatorButton(
  ACanvas: TcxCanvas; R: TRect; AState: TcxButtonState; AScaleFactor: TdxScaleFactor);
begin
  DrawScaledButton(ACanvas, R, '', AState, AScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DrawSchedulerSplitterBorder(
  ACanvas: TcxCanvas; R: TRect; const AViewParams: TcxViewParams; AIsHorizontal: Boolean);
begin
end;

function TcxCustomLookAndFeelPainter.SchedulerNavigationButtonTextColor(
  AIsNextButton: Boolean; AState: TcxButtonState; ADefaultColor: TColor = clDefault): TColor;
begin
  Result := ButtonSymbolColor(AState, ADefaultColor);
end;

procedure TcxCustomLookAndFeelPainter.SchedulerNavigationButtonSizes(
  AIsNextButton: Boolean; var ABorders: TRect; var AArrowSize: TSize;
  var AHasTextArea: Boolean; AScaleFactor: TdxScaleFactor; const AIsVertical: Boolean = True);
const
  AButtonBorders: array[Boolean, Boolean] of TRect = (
    ((Left: 6; Top: 0; Right: 6; Bottom: 2), (Left: 6; Top: 2; Right: 6; Bottom: 0)),
    ((Left: 0; Top: 6; Right: 2; Bottom: 6), (Left: 2; Top: 6; Right: 0; Bottom: 6)) );
begin
  ABorders := AButtonBorders[AIsVertical, AIsNextButton];
  AHasTextArea := True;
  AArrowSize.cx := AScaleFactor.Apply(7);
  AArrowSize.cy := AScaleFactor.Apply(7);
end;

procedure TcxCustomLookAndFeelPainter.SchedulerNavigationButtonSizes(AIsNextButton: Boolean; var ABorders: TRect;
  var AArrowSize: TSize; var AHasTextArea: Boolean; const AIsVertical: Boolean = True);
begin
  SchedulerNavigationButtonSizes(AIsNextButton, ABorders, AArrowSize, AHasTextArea, dxSystemScaleFactor, AIsVertical);
end;

function TcxCustomLookAndFeelPainter.SchedulerEventProgressOffsets: TRect;
begin
  Result := Rect(2, 2, 2, 2);
end;

function TcxCustomLookAndFeelPainter.ChartToolBoxDataLevelInfoBorderSize: Integer;
begin
  Result := 1;
end;

function TcxCustomLookAndFeelPainter.ScaledClockSize(AScaleFactor: TdxScaleFactor): TSize;
begin
  Result := AScaleFactor.Apply(cxSize(130, 130));
end;

function TcxCustomLookAndFeelPainter.ClockSize: TSize;
begin
  Result := ScaledClockSize(dxSystemScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DrawClock(ACanvas: TcxCanvas;
  const ARect: TRect; ADateTime: TDateTime; ABackgroundColor: TColor);
begin
  DrawScaledClock(ACanvas, ARect, ADateTime, ABackgroundColor, dxSystemScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DrawScaledClock(ACanvas: TcxCanvas; const ARect: TRect;
  ADateTime: TDateTime; ABackgroundColor: TColor; AScaleFactor: TdxScaleFactor);

  procedure DrawDot(X, Y: Integer; AHourDot: Boolean);
  var
    R: TRect;
  begin
    if AHourDot then
    begin
      R := cxRectCenter(cxRectBounds(X, Y, 0, 0), cxSize(AScaleFactor.Apply(2)));
      ACanvas.FillRect(R, clAqua);
      ACanvas.ExcludeClipRect(R);
      R := cxRectOffset(R, 1, 1);
      ACanvas.FillRect(R, clBtnText);
      ACanvas.ExcludeClipRect(R);
    end;
  end;

  procedure DrawHand(ACenter: TPoint; AAngle, L1X, L1Y, L2X, L2Y, L3: Extended);
  begin
    with ACanvas.Canvas do
    begin
      Brush.Color := clTeal;
      BeginPath(Handle);
      Pixels[Round(ACenter.X + L1X * Cos(AAngle)), Round(ACenter.Y + L1Y * Sin(AAngle))] := clTeal;
      Pen.Color := clTeal;
      MoveTo(Round(ACenter.X + L1X * Cos(AAngle)), Round(ACenter.Y + L1Y * Sin(AAngle)));
      LineTo(Round(ACenter.X + L3 / 2 * cos(AAngle + Pi / 2)), Round(ACenter.Y + L3 / 2 * sin(AAngle + Pi / 2)));
      LineTo(Round(ACenter.X + L2X * cos(AAngle + Pi)), Round(ACenter.Y + L2Y * sin(AAngle + Pi)));
      LineTo(Round(ACenter.X + L3 / 2 * cos(AAngle + Pi * 3 / 2)), Round(ACenter.Y + L3 / 2 * sin(AAngle + Pi * 3 / 2)));
      LineTo(Round(ACenter.X + L1X * cos(AAngle)), Round(ACenter.Y + L1Y * sin(AAngle)));
      EndPath(Handle);
      FillPath(Handle);
    end;
  end;

  procedure DrawHands;
  var
    AAngle: Extended;
    ACenter: TPoint;
    AHandRadiusX, AHandRadiusY: Extended;
    AHour, AMin, AMSec, ASec: Word;
  begin
    DecodeTime(ADateTime, AHour, AMin, ASec, AMSec);
    ACenter.X := (ARect.Right + ARect.Left) div 2;
    ACenter.Y := (ARect.Bottom + ARect.Top) div 2;
    AHandRadiusX := (ARect.Right - ARect.Left) / 2 - 2;
    AHandRadiusY := (ARect.Bottom - ARect.Top) / 2 - 2;
    with ACanvas.Canvas do
    begin
      AAngle := Pi * 2 * ((AHour mod 12) * 60 * 60 + AMin * 60 + ASec - 3 * 60 * 60) / 12 / 60 / 60;
      DrawHand(ACenter, AAngle, AHandRadiusX * 0.75, AHandRadiusY * 0.75, AHandRadiusX * 0.15, AHandRadiusY * 0.15, 9);

      AAngle := Pi * 2 * (AMin * 60 + ASec - 15 * 60) / 60 / 60;
      DrawHand(ACenter, AAngle, AHandRadiusX * 0.85, AHandRadiusY * 0.85, AHandRadiusX * 0.2, AHandRadiusY * 0.2, 7);

      Pen.Color := clRed;
      MoveTo(ACenter.X, ACenter.Y);
      AAngle := Pi * 2 * (ASec - 15) / 60;
      LineTo(Round(ACenter.X + AHandRadiusX * 0.9 * cos(AAngle)), Round(ACenter.Y + AHandRadiusY * 0.9 * sin(AAngle)));
    end;
  end;

var
  AAngle: Extended;
  ACenter: TPoint;
  I: Integer;
  RX, RY: Extended;
begin
  ACenter.X := (ARect.Right + ARect.Left) div 2;
  ACenter.Y := (ARect.Bottom + ARect.Top) div 2;
  RX := (ARect.Right - ARect.Left) / 2 - 2;
  RY := (ARect.Bottom - ARect.Top) / 2 - 2;
  for I := 0 to 59 do
  begin
    AAngle := 2 * Pi * I / 60;
    DrawDot(Round(ACenter.X + RX * cos(AAngle)), Round(ACenter.Y + RY * sin(AAngle)), I mod 5 = 0);
  end;
  ACanvas.FillRect(ARect, ABackgroundColor);
  DrawHands;
end;

procedure TcxCustomLookAndFeelPainter.DrawScaledEditorButton(ACanvas: TcxCanvas; const ARect: TRect;
  AButtonKind: TcxEditBtnKind; AState: TcxButtonState; AScaleFactor: TdxScaleFactor; APosition: TcxEditBtnPosition);
begin
  // do nothing
end;

procedure TcxCustomLookAndFeelPainter.DrawScaledEditorButtonGlyph(ACanvas: TcxCanvas; const ARect: TRect;
  AButtonKind: TcxEditBtnKind; AState: TcxButtonState; AScaleFactor: TdxScaleFactor; APosition: TcxEditBtnPosition);
begin
  // do nothing
end;

procedure TcxCustomLookAndFeelPainter.DrawEditorButton(
  ACanvas: TcxCanvas; const ARect: TRect; AButtonKind: TcxEditBtnKind;
  AState: TcxButtonState; APosition: TcxEditBtnPosition = cxbpRight);
begin
  DrawScaledEditorButton(ACanvas, ARect, AButtonKind, AState, dxSystemScaleFactor, APosition);
end;

procedure TcxCustomLookAndFeelPainter.DrawEditorButtonGlyph(
  ACanvas: TcxCanvas; const ARect: TRect; AButtonKind: TcxEditBtnKind;
  AState: TcxButtonState; APosition: TcxEditBtnPosition = cxbpRight);
begin
  DrawScaledEditorButtonGlyph(ACanvas, ARect, AButtonKind, AState, dxSystemScaleFactor, APosition);
end;

function TcxCustomLookAndFeelPainter.EditButtonTextOffset: Integer;
begin
  Result := 0;
end;

function TcxCustomLookAndFeelPainter.EditButtonColorPalette(AState: TcxButtonState): IdxColorPalette;
begin
  Result := nil;
end;

function TcxCustomLookAndFeelPainter.EditButtonSize: TSize;
begin
  Result := cxNullSize;
end;

function TcxCustomLookAndFeelPainter.EditButtonTextColor: TColor;
begin
  Result := clDefault;
end;

function TcxCustomLookAndFeelPainter.GetContainerBorderColor(AIsHighlightBorder: Boolean): TColor;
begin
  Result := clBtnText;
end;

function TcxCustomLookAndFeelPainter.GetContainerBorderWidth(ABorderStyle: TcxContainerBorderStyle): Integer;
begin
  Result := cxContainerBorderWidthA1[ABorderStyle];
end;

procedure TcxCustomLookAndFeelPainter.DrawDateNavigatorCellSelection(
  ACanvas: TcxCanvas; const R: TRect; AColor: TColor);
begin
  ACanvas.FillRect(R, AColor);
end;

procedure TcxCustomLookAndFeelPainter.DrawDateNavigatorDateHeader(
  ACanvas: TcxCanvas; var R: TRect);
begin
  ACanvas.FillRect(R, DefaultDateNavigatorHeaderColor);
  ACanvas.DrawEdge(R, False, False, cxBordersAll);
  InflateRect(R, -1, -1);
end;

procedure TcxCustomLookAndFeelPainter.DrawDateNavigatorTodayCellSelection(
  ACanvas: TcxCanvas; const R: TRect);
begin
  ACanvas.FrameRect(R, DefaultDateNavigatorTodayFrameColor);
end;

procedure TcxCustomLookAndFeelPainter.DrawNavigatorBorder(ACanvas: TcxCanvas;
  R: TRect; ASelected: Boolean);
begin
end;

procedure TcxCustomLookAndFeelPainter.DrawNavigatorScaledButton(ACanvas: TcxCanvas; R: TRect; AState: TcxButtonState;
  ABackgroundColor: TColor; AScaleFactor: TdxScaleFactor);
begin
  DrawScaledButton(ACanvas, R, '', AState, AScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DrawNavigatorButton(ACanvas: TcxCanvas;
  R: TRect; AState: TcxButtonState; ABackgroundColor: TColor);
begin
  DrawScaledButton(ACanvas, R, '', AState, dxSystemScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DrawNavigatorScaledButtonGlyph(ACanvas: TcxCanvas;
  AImageList: TCustomImageList; AImageIndex: TcxImageIndex; const AGlyphRect: TRect; AEnabled, AUserGlyphs: Boolean;
  AScaleFactor: TdxScaleFactor);
begin
  cxDrawImage(ACanvas.Handle, AGlyphRect, AGlyphRect, nil, AImageList, AImageIndex,
    EnabledImageDrawModeMap[AEnabled], False, 0, clDefault, True, NavigatorButtonColorPalette(AEnabled));
end;

procedure TcxCustomLookAndFeelPainter.DrawNavigatorButtonGlyph(ACanvas: TcxCanvas; AImageList: TCustomImageList;
  AImageIndex: TcxImageIndex; const AGlyphRect: TRect; AEnabled: Boolean; AUserGlyphs: Boolean);
begin
  DrawNavigatorScaledButtonGlyph(ACanvas, AImageList, AImageIndex, AGlyphRect, AEnabled, AUserGlyphs, dxSystemScaleFactor);
end;

function TcxCustomLookAndFeelPainter.NavigatorBorderOverlap: Boolean;
begin
  Result := False;
end;

function TcxCustomLookAndFeelPainter.NavigatorBorderSize: Integer;
begin
  Result := 0;
end;

function TcxCustomLookAndFeelPainter.NavigatorButtonColorPalette(AEnabled: Boolean): IdxColorPalette;
begin
  Result := nil;
end;

function TcxCustomLookAndFeelPainter.NavigatorScaledButtonGlyphPadding(AScaleFactor: TdxScaleFactor): TRect;
begin
  Result := AScaleFactor.Apply(Rect(3, 4, 3, 4));
end;

function TcxCustomLookAndFeelPainter.NavigatorButtonGlyphPadding: TRect;
begin
  Result := NavigatorScaledButtonGlyphPadding(dxSystemScaleFactor);
end;

function TcxCustomLookAndFeelPainter.NavigatorScaledButtonGlyphSize(AScaleFactor: TdxScaleFactor): TSize;
begin
  Result := AScaleFactor.Apply(cxSize(11, 11));
end;

function TcxCustomLookAndFeelPainter.NavigatorButtonGlyphSize: TSize;
begin
  Result := NavigatorScaledButtonGlyphSize(dxSystemScaleFactor);
end;

function TcxCustomLookAndFeelPainter.NavigatorButtonPressedGlyphOffset: TPoint;
begin
  Result := cxSimplePoint;
end;

function TcxCustomLookAndFeelPainter.NavigatorButtonTextColor(AState: TcxButtonState): TColor;
begin
  Result := clBtnText;
end;

function TcxCustomLookAndFeelPainter.NavigatorScaledButtonMinSize(AScaleFactor: TdxScaleFactor): TSize;
begin
  Result := AScaleFactor.Apply(cxSize(17, 19));
end;

function TcxCustomLookAndFeelPainter.NavigatorInfoPanelColor: TColor;
begin
  Result := DefaultGridDetailsSiteColor;
end;

function TcxCustomLookAndFeelPainter.NavigatorInfoPanelTextColor: TColor;
begin
  Result := DefaultContentTextColor;
end;

function TcxCustomLookAndFeelPainter.NavigatorButtonMinSize: TSize;
begin
  Result := NavigatorScaledButtonMinSize(dxSystemScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DrawProgressBarChunk(
  ACanvas: TcxCanvas; ARect: TRect; AVertical: Boolean);
begin
end;

procedure TcxCustomLookAndFeelPainter.DrawProgressBarText(
  ACanvas: TcxCanvas; AVertical, ASolid: Boolean; const AText: string;
  const ATextRect, AProgressBarRect, AProgressChunkRect: TRect; ATextColor: TColor = clDefault);

  procedure InternalDrawText(DC: HDC; ATextRect: TRect);
  begin
    if AVertical then
    begin
      OffsetRect(ATextRect, 2, 2);
      cxRotatedTextOut(DC, ATextRect, AText, ACanvas.Font, taLeft, taTop, False, True, True, vtdTopToBottom);
    end
    else
      cxTextOut(DC, AText, ATextRect, CXTO_DEFAULT_FORMAT or IfThen(ACanvas.UseRightToLeftAlignment, CXTO_RTLREADING));
  end;

  procedure DrawInvertedText(ACanvas: TcxCanvas; const ATextRect: TRect);
  var
    ACopyMode: Integer;
    ATextBmp: TcxBitmap;
  begin
    ATextBmp := TcxBitmap.CreateSize(ATextRect);
    try
      ATextBmp.Canvas.Font := ACanvas.Font;
      dxSetFontAsNonAntialiased(ATextBmp.Canvas.Font);
      ATextBmp.Canvas.Font.Color := clWhite;
      ATextBmp.Canvas.Brush.Color := clBlack;
      ATextBmp.Canvas.FillRect(ATextBmp.ClientRect);
      InternalDrawText(ATextBmp.Canvas.Handle, ATextBmp.ClientRect);

      ACopyMode := ACanvas.CopyMode;
      try
        ACanvas.CopyMode := cmSrcInvert;
        ACanvas.Draw(ATextRect.Left, ATextRect.Top, ATextBmp);
      finally
        ACanvas.CopyMode := ACopyMode;
      end;
    finally
      ATextBmp.Free;
    end;
  end;

var
  ARect: TRect;
  ATextBmp: TcxBitmap;
begin
  ACanvas.SaveState;
  try
    if (ATextColor = clDefault) or not ASolid then
      ATextColor := ProgressBarTextColor;
    ACanvas.Font.Color := ATextColor;

    if ASolid then
      InternalDrawText(ACanvas.Handle, ATextRect)
    else
      if ProgressBarTextColorEx(True) = clDefault then
        DrawInvertedText(ACanvas, ATextRect)
      else
        if cxRectIntersect(ARect, ATextRect, AProgressChunkRect) then
        begin
          ATextBmp := TcxBitmap.CreateSize(ARect);
          try
            cxBitBlt(ATextBmp.Canvas.Handle, ACanvas.Handle, ATextBmp.ClientRect, ARect.TopLeft, SRCCOPY);
            ACanvas.Font.Color := ProgressBarTextColorEx(False);
            InternalDrawText(ACanvas.Handle, ATextRect);
            cxBitBlt(ACanvas.Handle, ATextBmp.Canvas.Handle, ARect, cxNullPoint, SRCCOPY);
            ACanvas.IntersectClipRect(ARect);
            ACanvas.Font.Color := ProgressBarTextColorEx(True);
            InternalDrawText(ACanvas.Handle, ATextRect);
          finally
            ATextBmp.Free;
          end;
        end
        else
          InternalDrawText(ACanvas.Handle, ATextRect);
  finally
    ACanvas.RestoreState;
  end;
end;

procedure TcxCustomLookAndFeelPainter.DrawProgressBarBorder(
  ACanvas: TcxCanvas; ARect: TRect; AVertical: Boolean);
begin
end;

function TcxCustomLookAndFeelPainter.ProgressBarBorderSize(AVertical: Boolean): TRect;
begin
  Result := cxEmptyRect;
end;

function TcxCustomLookAndFeelPainter.ProgressBarTextColor: TColor;
begin
  Result := ProgressBarTextColorEx(False);
end;

function TcxCustomLookAndFeelPainter.ProgressBarTextColorEx(AIsFilledArea: Boolean): TColor;
begin
  if AIsFilledArea then
    Result := clDefault
  else
    Result := clWindowText;
end;

procedure TcxCustomLookAndFeelPainter.DrawGroupBoxBackground(ACanvas: TcxCanvas;
  ABounds: TRect; ARect: TRect);
begin
end;

procedure TcxCustomLookAndFeelPainter.DrawGroupBoxCaption(ACanvas: TcxCanvas;
  const ACaptionRect, ATextRect: TRect; ACaptionPosition: TcxGroupBoxCaptionPosition);
begin
end;

procedure TcxCustomLookAndFeelPainter.DrawGroupBoxContent(ACanvas: TcxCanvas;
  ABorderRect: TRect; ACaptionPosition: TcxGroupBoxCaptionPosition;
  ABorders: TcxBorders = cxBordersAll);
begin
end;

procedure TcxCustomLookAndFeelPainter.DrawGroupBoxExpandButton(ACanvas: TcxCanvas;
  const R: TRect; AState: TcxButtonState; AExpanded: Boolean; ARotationAngle: TcxRotationAngle = ra0);
begin
  DrawGroupBoxScaledExpandButton(ACanvas, R, AState, AExpanded, dxSystemScaleFactor, ARotationAngle);
end;

procedure TcxCustomLookAndFeelPainter.DrawGroupBoxScaledExpandButton(ACanvas: TcxCanvas; const R: TRect;
  AState: TcxButtonState; AExpanded: Boolean; AScaleFactor: TdxScaleFactor; ARotationAngle: TcxRotationAngle = ra0);
begin
  DrawScaledExpandButtonEx(ACanvas, R, AState, AExpanded, AScaleFactor, ARotationAngle);
end;

procedure TcxCustomLookAndFeelPainter.DrawGroupBoxButton(
  ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState);
begin
  DrawGroupBoxScaledButton(ACanvas, R, AState, dxSystemScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DrawGroupBoxScaledButton(
  ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState; AScaleFactor: TdxScaleFactor);
begin
  DrawScaledButton(ACanvas, R, '', AState, AScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DrawGroupBoxExpandGlyph(
  ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState; AExpanded: Boolean);
begin
  DrawGroupBoxScaledExpandGlyph(ACanvas, R, AState, AExpanded, dxSystemScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DrawGroupBoxScaledExpandGlyph(ACanvas: TcxCanvas;
  const R: TRect; AState: TcxButtonState; AExpanded: Boolean; AScaleFactor: TdxScaleFactor);
begin
  DrawScaledExpandMark(ACanvas, R, ButtonSymbolColor(AState), AExpanded, AScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DrawGroupBoxFrame(ACanvas: TcxCanvas;
  R: TRect; AEnabled: Boolean; ACaptionPosition: TcxGroupBoxCaptionPosition;
  ABorders: TcxBorders = cxBordersAll);
begin
end;

procedure TcxCustomLookAndFeelPainter.GroupBoxAdjustCaptionFont(
  ACaptionFont: TFont; ACaptionPosition: TcxGroupBoxCaptionPosition);
begin
  // do nothing
end;

function TcxCustomLookAndFeelPainter.GroupBoxBorderSize(
  ACaption: Boolean; ACaptionPosition: TcxGroupBoxCaptionPosition): TRect;
begin
  Result := cxEmptyRect;
end;

function TcxCustomLookAndFeelPainter.GroupBoxCaptionTailSize(ACaptionPosition: TcxGroupBoxCaptionPosition): Integer;
begin
  Result := 0;
end;

function TcxCustomLookAndFeelPainter.GroupBoxTextColor(
  AEnabled: Boolean; ACaptionPosition: TcxGroupBoxCaptionPosition): TColor;
begin
  if AEnabled then
    Result := clWindowText
  else
    Result := clBtnShadow;
end;

function TcxCustomLookAndFeelPainter.IsGroupBoxCaptionTextDrawnOverBorder(
  ACaptionPosition: TcxGroupBoxCaptionPosition): Boolean;
begin
  Result := True;
end;

function TcxCustomLookAndFeelPainter.IsGroupBoxTransparent(AIsCaption: Boolean;
  ACaptionPosition: TcxGroupBoxCaptionPosition): Boolean;
begin
  Result := False;
end;

procedure TcxCustomLookAndFeelPainter.DrawPanelBorders(ACanvas: TcxCanvas;
  const ABorderRect: TRect);
begin
end;

procedure TcxCustomLookAndFeelPainter.DrawPanelCaption(ACanvas: TcxCanvas;
  const ACaptionRect: TRect; ACaptionPosition: TcxGroupBoxCaptionPosition);
begin
  DrawGroupBoxCaption(ACanvas, ACaptionRect, cxNullRect, ACaptionPosition);
end;

procedure TcxCustomLookAndFeelPainter.DrawPanelBackground(
  ACanvas: TcxCanvas; AControl: TWinControl; ABounds: TRect;
  AColorFrom: TColor = clDefault; AColorTo: TColor = clDefault);
begin
  DrawGroupBoxBackground(ACanvas, ABounds, ABounds);
end;

procedure TcxCustomLookAndFeelPainter.DrawPanelContent(
  ACanvas: TcxCanvas; const ARect: TRect; ADrawBorders: Boolean);
begin
  ACanvas.FillRect(ARect, clBtnFace);
end;

function TcxCustomLookAndFeelPainter.PanelBorderSize: TRect;
begin
  Result := cxEmptyRect;
end;

function TcxCustomLookAndFeelPainter.PanelTextColor: TColor;
begin
  Result := GroupBoxTextColor(True, cxgpTop);
end;

procedure TcxCustomLookAndFeelPainter.CorrectThumbRect(ACanvas: TcxCanvas;
  var ARect: TRect; AHorizontal: Boolean; ATicks: TcxTrackBarTicksAlign);
begin
  ARect := cxRectContent(ARect, Rect(0, 0, -1, -1));
end;

procedure TcxCustomLookAndFeelPainter.DrawTrackBarTrack(ACanvas: TcxCanvas;
  const ARect, ASelection: TRect; AShowSelection, AEnabled, AHorizontal: Boolean; ATrackColor: TColor);
begin
  DrawTrackBarScaledTrack(ACanvas, ARect, ASelection, AShowSelection, AEnabled, AHorizontal, ATrackColor, dxSystemScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DrawTrackBarScaledTrack(ACanvas: TcxCanvas; const ARect, ASelection: TRect;
  AShowSelection, AEnabled, AHorizontal: Boolean; ATrackColor: TColor; AScaleFactor: TdxScaleFactor);
begin
  ACanvas.FillRect(ARect, ATrackColor);
  DrawTrackBarTrackBounds(ACanvas, ARect);
end;

procedure TcxCustomLookAndFeelPainter.DrawTrackBarTrackBounds(ACanvas: TcxCanvas; const ARect: TRect);
begin
  // do nothing
end;

procedure TcxCustomLookAndFeelPainter.DrawTrackBarThumb(ACanvas: TcxCanvas;
  const ARect: TRect; AState: TcxButtonState; AHorizontal: Boolean;
  ATicks: TcxTrackBarTicksAlign; AThumbColor: TColor);
begin
  DrawTrackBarScaledThumb(ACanvas, ARect, AState, AHorizontal, ATicks, AThumbColor, dxSystemScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DrawTrackBarScaledThumb(ACanvas: TcxCanvas; const ARect: TRect; AState: TcxButtonState;
  AHorizontal: Boolean; ATicks: TcxTrackBarTicksAlign; AThumbColor: TColor; AScaleFactor: TdxScaleFactor);

  procedure GetBottomRightThumbShape(const AThumbRect: TRect; AHorizontal: Boolean;
    var ALightPolyLine, AShadowPolyLine, ADarkPolyLine, APolygon: TPoints);
  var
    AThumbSize: Integer;
    AMiddle: TPoint;
  begin
    SetLength(ALightPolyLine, 4);
    SetLength(AShadowPolyLine, 3);
    SetLength(ADarkPolyLine, 3);
    SetLength(APolygon, 5);

    if AHorizontal then // Bottom
    begin
      AThumbSize := cxRectWidth(AThumbRect);
      AMiddle.X := AThumbRect.Left + (AThumbSize div 2);
      AMiddle.Y := AThumbRect.Bottom - (AThumbSize div 2);

      ALightPolyLine[0] := Point(AThumbRect.Right, AThumbRect.Top);
      ALightPolyLine[1] := Point(AThumbRect.Left, AThumbRect.Top);
      ALightPolyLine[2] := Point(AThumbRect.Left, AMiddle.Y);
      ALightPolyLine[3] := Point(AMiddle.X, AThumbRect.Bottom);

      ADarkPolyLine[0] := Point(AMiddle.X, AThumbRect.Bottom);
      ADarkPolyLine[1] := Point(AThumbRect.Right, AMiddle.Y);
      ADarkPolyLine[2] := Point(AThumbRect.Right, AThumbRect.Top);

      AShadowPolyLine[0] := Point(AMiddle.X, AThumbRect.Bottom - 1);
      AShadowPolyLine[1] := Point(AThumbRect.Right - 1, AMiddle.Y);
      AShadowPolyLine[2] := Point(AThumbRect.Right - 1, AThumbRect.Top);

      APolygon[0] := Point(AThumbRect.Right - 1, AThumbRect.Top + 1);
      APolygon[1] := Point(AThumbRect.Left + 1, AThumbRect.Top + 1);
      APolygon[2] := Point(AThumbRect.Left + 1, AMiddle.Y);
      APolygon[3] := Point(AMiddle.X, AThumbRect.Bottom - 1);
      APolygon[4] := Point(AThumbRect.Right - 1, AMiddle.Y);
    end
    else // Right
    begin
      AThumbSize := cxRectHeight(AThumbRect);
      AMiddle.X := AThumbRect.Right - (AThumbSize div 2);
      AMiddle.Y := AThumbRect.Top + (AThumbSize div 2);

      ALightPolyLine[0] := Point(AThumbRect.Left, AThumbRect.Bottom);
      ALightPolyLine[1] := Point(AThumbRect.Left, AThumbRect.Top);
      ALightPolyLine[2] := Point(AMiddle.X, AThumbRect.Top);
      ALightPolyLine[3] := Point(AThumbRect.Right, AMiddle.Y);

      ADarkPolyLine[0] := Point(AThumbRect.Right, AMiddle.Y);
      ADarkPolyLine[1] := Point(AMiddle.X, AThumbRect.Bottom);
      ADarkPolyLine[2] := Point(AThumbRect.Left, AThumbRect.Bottom);

      AShadowPolyLine[0] := Point(AThumbRect.Right - 1, AMiddle.Y);
      AShadowPolyLine[1] := Point(AMiddle.X, AThumbRect.Bottom - 1);
      AShadowPolyLine[2] := Point(AThumbRect.Left, AThumbRect.Bottom - 1);

      APolygon[0] := Point(AThumbRect.Left + 1, AThumbRect.Bottom - 1);
      APolygon[1] := Point(AThumbRect.Left + 1, AThumbRect.Top + 1);
      APolygon[2] := Point(AMiddle.X, AThumbRect.Top + 1);
      APolygon[3] := Point(AThumbRect.Right - 1, AMiddle.Y);
      APolygon[4] := Point(AMiddle.X, AThumbRect.Bottom - 1);
    end;
  end;

  procedure GetTopLeftThumbShape(const AThumbRect: TRect;
    AHorizontal: Boolean;
    var ALightPolyLine, AShadowPolyLine, ADarkPolyLine, APolygon: TPoints);
  var
    AThumbSize: Integer;
    AMiddle: TPoint;
  begin
    SetLength(ALightPolyLine, 3);
    SetLength(AShadowPolyLine, 4);
    SetLength(ADarkPolyLine, 4);
    SetLength(APolygon, 5);

    if AHorizontal then // Top
    begin
      AThumbSize := cxRectWidth(AThumbRect);
      AMiddle.X :=  AThumbRect.Left + (AThumbSize div 2);
      AMiddle.Y := AThumbRect.Top + (AThumbSize div 2);

      ALightPolyLine[0] := Point(AThumbRect.Left, AThumbRect.Bottom - 1);
      ALightPolyLine[1] := Point(AThumbRect.Left, AMiddle.Y - 1);
      ALightPolyLine[2] := Point(AMiddle.X, AThumbRect.Top - 1);

      AShadowPolyLine[0] := Point(AMiddle.X, AThumbRect.Top);
      AShadowPolyLine[1] := Point(AThumbRect.Right - 1, AMiddle.Y - 1);
      AShadowPolyLine[2] := Point(AThumbRect.Right - 1, AThumbRect.Bottom - 2);
      AShadowPolyLine[3] := Point(AThumbRect.Left, AThumbRect.Bottom - 2);

      ADarkPolyLine[0] := Point(AMiddle.X, AThumbRect.Top - 1);
      ADarkPolyLine[1] := Point(AThumbRect.Right, AMiddle.Y - 1);
      ADarkPolyLine[2] := Point(AThumbRect.Right, AThumbRect.Bottom - 1);
      ADarkPolyLine[3] := Point(AThumbRect.Left, AThumbRect.Bottom - 1);

      APolygon[0] := Point(AThumbRect.Right - 1, AThumbRect.Bottom - 2);
      APolygon[1] := Point(AThumbRect.Left + 1, AThumbRect.Bottom - 2);
      APolygon[2] := Point(AThumbRect.Left + 1, AMiddle.Y - 1);
      APolygon[3] := Point(AMiddle.X, AThumbRect.Top);
      APolygon[4] := Point(AThumbRect.Right - 1, AMiddle.Y - 1);
    end
    else // Left
    begin
      AThumbSize := cxRectHeight(AThumbRect);
      AMiddle.X := AThumbRect.Left + (AThumbSize div 2);
      AMiddle.Y := AThumbRect.Top + (AThumbSize div 2);

      ALightPolyLine[0] := Point(AThumbRect.Right - 1, AThumbRect.Top);
      ALightPolyLine[1] := Point(AMiddle.X - 1, AThumbRect.Top);
      ALightPolyLine[2] := Point(AThumbRect.Left - 1, AMiddle.Y);

      ADarkPolyLine[0] := Point(AThumbRect.Left - 1, AMiddle.Y);
      ADarkPolyLine[1] := Point(AMiddle.X - 1, AThumbRect.Bottom);
      ADarkPolyLine[2] := Point(AThumbRect.Right - 1, AThumbRect.Bottom);
      ADarkPolyLine[3] := Point(AThumbRect.Right - 1, AThumbRect.Top);

      AShadowPolyLine[0] := Point(AThumbRect.Left, AMiddle.Y);
      AShadowPolyLine[1] := Point(AMiddle.X - 1, AThumbRect.Bottom - 1);
      AShadowPolyLine[2] := Point(AThumbRect.Right - 2, AThumbRect.Bottom - 1);
      AShadowPolyLine[3] := Point(AThumbRect.Right - 2, AThumbRect.Top);

      APolygon[0] := Point(AThumbRect.Right - 2, AThumbRect.Bottom - 1);
      APolygon[1] := Point(AThumbRect.Right - 2, AThumbRect.Top + 1);
      APolygon[2] := Point(AMiddle.X - 1, AThumbRect.Top + 1);
      APolygon[3] := Point(AThumbRect.Left, AMiddle.Y);
      APolygon[4] := Point(AMiddle.X - 1, AThumbRect.Bottom - 1);
    end;
  end;

var
  ALightPolyLine, AShadowPolyLine, ADarkPolyLine, APolygon: TPoints;
begin
  ACanvas.Pen.Color := AThumbColor;
  ACanvas.Brush.Color := AThumbColor;
  case ATicks of
    tbtaUp:
      begin
        GetTopLeftThumbShape(ARect, AHorizontal, ALightPolyLine, AShadowPolyLine, ADarkPolyLine, APolygon);
        ACanvas.Polygon(APolygon);
        DrawTrackBarThumbBorderUpDown(ACanvas, ALightPolyLine, AShadowPolyLine, ADarkPolyLine);
      end;
    tbtaDown:
      begin
        GetBottomRightThumbShape(ARect, AHorizontal, ALightPolyLine, AShadowPolyLine, ADarkPolyLine, APolygon);
        ACanvas.Polygon(APolygon);
        DrawTrackBarThumbBorderUpDown(ACanvas, ALightPolyLine, AShadowPolyLine, ADarkPolyLine);
      end;
    tbtaBoth:
    begin
      ACanvas.Canvas.FillRect(ARect);
      DrawTrackBarThumbBorderBoth(ACanvas, ARect);
    end;
  end;
end;

procedure TcxCustomLookAndFeelPainter.DrawTrackBarThumbBorderUpDown(ACanvas: TcxCanvas;
  const ALightPolyLine, AShadowPolyLine, ADarkPolyLine: TPoints);
begin
//
end;

procedure TcxCustomLookAndFeelPainter.DrawTrackBarThumbBorderBoth(ACanvas: TcxCanvas; const ARect: TRect);
begin

end;

function TcxCustomLookAndFeelPainter.TrackBarScaledThumbSize(AHorizontal: Boolean; AScaleFactor: TdxScaleFactor): TSize;
begin
  Result := cxNullSize;
end;

function TcxCustomLookAndFeelPainter.TrackBarThumbSize(AHorizontal: Boolean): TSize;
begin
  Result := TrackBarScaledThumbSize(AHorizontal, dxSystemScaleFactor);
end;

function TcxCustomLookAndFeelPainter.TrackBarTicksColor(AText: Boolean): TColor;
begin
  Result := clDefault;
end;

function TcxCustomLookAndFeelPainter.TrackBarTrackSize: Integer;
begin
  Result := TrackBarScaledTrackSize(dxSystemScaleFactor)
end;

function TcxCustomLookAndFeelPainter.TrackBarScaledTrackSize(AScaleFactor: TdxScaleFactor): Integer;
begin
  Result := 0;
end;

procedure TcxCustomLookAndFeelPainter.DrawRangeControlLeftThumb(
  ACanvas: TcxCanvas; const ARect: TRect; AColor: TColor; ABorderColor: TdxAlphaColor);
begin
  DrawRangeControlScaledLeftThumb(ACanvas, ARect, AColor, ABorderColor, dxSystemScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DrawRangeControlScaledLeftThumb(ACanvas: TcxCanvas;
  const ARect: TRect; AColor: TColor; ABorderColor: TdxAlphaColor; AScaleFactor: TdxScaleFactor);
begin
  DrawRangeControlThumb(ACanvas, ARect, AColor, ABorderColor, AScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DrawRangeControlRightThumb(
  ACanvas: TcxCanvas; const ARect: TRect; AColor: TColor; ABorderColor: TdxAlphaColor);
begin
  DrawRangeControlScaledRightThumb(ACanvas, ARect, AColor, ABorderColor, dxSystemScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DrawRangeControlScaledRightThumb(
  ACanvas: TcxCanvas; const ARect: TRect; AColor: TColor; ABorderColor: TdxAlphaColor; AScaleFactor: TdxScaleFactor);
begin
  DrawRangeControlThumb(ACanvas, ARect, AColor, ABorderColor, AScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DrawRangeControlRulerHeader(ACanvas: TcxCanvas;
  const ARect: TRect; AIsHot: Boolean; AColor, ABorderColor: TdxAlphaColor);
begin
  DrawRangeControlScaledRulerHeader(ACanvas, ARect, AIsHot, AColor, ABorderColor, dxSystemScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DrawRangeControlScaledRulerHeader(ACanvas: TcxCanvas;
  const ARect: TRect; AIsHot: Boolean; AColor, ABorderColor: TdxAlphaColor; AScaleFactor: TdxScaleFactor);
const
  BackColor = $FFFFFFFF;
  BackHotColor = $FFE2EAFD;
var
  APoint1: TPoint;
  APoint2: TPoint;
begin
  if AColor = dxacDefault then
  begin
    if AIsHot then
      AColor := BackHotColor
    else
      AColor := BackColor;
  end;

  dxGPPaintCanvas.BeginPaint(ACanvas.Handle, ARect);
  try
    dxGPPaintCanvas.FillRectangle(ARect, AColor);
    APoint1 := ARect.TopLeft;
    APoint2 := Point(ARect.Left, ARect.Bottom - 1);
    dxGPPaintCanvas.Line(APoint1.X, APoint1.Y, APoint2.X, APoint2.Y, ABorderColor);
    APoint1 := APoint2;
    APoint2 := Point(ARect.Right - 1, APoint1.Y);
    dxGPPaintCanvas.Line(APoint1.X, APoint1.Y, APoint2.X, APoint2.Y, ABorderColor);
  finally
    dxGPPaintCanvas.EndPaint;
  end;
end;

procedure TcxCustomLookAndFeelPainter.DrawRangeControlSizingGlyph(
  ACanvas: TcxCanvas; const ARect: TRect; ABorderColor: TdxAlphaColor);
begin
  DrawRangeControlScaledSizingGlyph(ACanvas, ARect, ABorderColor, dxSystemScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DrawRangeControlScaledSizingGlyph(
  ACanvas: TcxCanvas; const ARect: TRect; ABorderColor: TdxAlphaColor; AScaleFactor: TdxScaleFactor);
var
  R: TRect;
  APoint1: TPoint;
  APoint2: TPoint;
begin
  dxGPPaintCanvas.BeginPaint(ACanvas.Handle, ARect);
  try
    R := cxRectCenter(ARect, Size(AScaleFactor.Apply(1), cxRectHeight(ARect) - AScaleFactor.Apply(4)));
    APoint1 := R.TopLeft;
    APoint2 := Point(R.Left, R.Bottom - 1);
    dxGPPaintCanvas.Line(APoint1.X, APoint1.Y, APoint2.X, APoint2.Y, ABorderColor, AScaleFactor.ApplyF(1));
  finally
    dxGPPaintCanvas.EndPaint;
  end;
end;

function TcxCustomLookAndFeelPainter.GetRangeControlBackColor: TColor;
begin
  Result := clWindow;
end;

function TcxCustomLookAndFeelPainter.GetRangeControlBorderColor: TColor;
begin
  Result := $AAA09D;
end;

function TcxCustomLookAndFeelPainter.GetRangeControlDefaultElementColor: TColor;
begin
  Result := $A69A9A;
end;

function TcxCustomLookAndFeelPainter.GetRangeControlElementForeColor: TColor;
begin
  Result := $E1DEDD;
end;

function TcxCustomLookAndFeelPainter.GetRangeControlElementsBorderColor: TdxAlphaColor;
begin
  Result := $FFB6B8BF;
end;

function TcxCustomLookAndFeelPainter.GetRangeControlLabelColor: TColor;
begin
  Result := $BAAEAE;
end;

function TcxCustomLookAndFeelPainter.GetRangeControlOutOfRangeColor: TdxAlphaColor;
begin
  Result := $1E9DA0AA;
end;

function TcxCustomLookAndFeelPainter.GetRangeControlRangePreviewColor: TColor;
begin
  Result := clWindow;
end;

function TcxCustomLookAndFeelPainter.GetRangeControlRulerColor: TdxAlphaColor;
begin
  Result := $14000000;
end;

function TcxCustomLookAndFeelPainter.GetRangeControlScrollAreaColor: TColor;
begin
  Result := $E1DEDD;
end;

function TcxCustomLookAndFeelPainter.GetRangeControlSelectedRegionBackgroundColor: TdxAlphaColor;
begin
  Result := $64A8C3F1;
end;

function TcxCustomLookAndFeelPainter.GetRangeControlSelectedRegionBorderColor: TdxAlphaColor;
begin
  Result :=  $644571BA;
end;

function TcxCustomLookAndFeelPainter.GetRangeControlViewPortPreviewColor: TColor;
begin
  Result := $F2F1F0;
end;

function TcxCustomLookAndFeelPainter.GetRangeControlScrollAreaHeight: Integer;
begin
  Result := GetRangeControlScaledScrollAreaHeight(dxSystemScaleFactor);
end;

function TcxCustomLookAndFeelPainter.GetRangeControlScaledScrollAreaHeight(AScaleFactor: TdxScaleFactor): Integer;
begin
  Result := AScaleFactor.Apply(11);
end;

function TcxCustomLookAndFeelPainter.GetRangeControlSizingGlyphSize: TSize;
begin
  Result := GetRangeControlScaledSizingGlyphSize(dxSystemScaleFactor);
end;

function TcxCustomLookAndFeelPainter.GetRangeControlScaledSizingGlyphSize(AScaleFactor: TdxScaleFactor): TSize;
begin
  Result := AScaleFactor.Apply(Size(5, 9));
end;

function TcxCustomLookAndFeelPainter.GetRangeControlThumbSize: TSize;
begin
  Result := GetRangeControlScaledThumbSize(dxSystemScaleFactor)
end;

function TcxCustomLookAndFeelPainter.GetRangeControlScaledThumbSize(AScaleFactor: TdxScaleFactor): TSize;
begin
  Result := AScaleFactor.Apply(Size(11, 19));
end;

function TcxCustomLookAndFeelPainter.RangeTrackBarScaledLeftThumbSize(
  AHorizontal: Boolean; ATicks: TcxTrackBarTicksAlign; AScaleFactor: TdxScaleFactor): TSize;
begin
  Result := cxNullSize;
end;

function TcxCustomLookAndFeelPainter.RangeTrackBarLeftThumbSize(AHorizontal: Boolean; ATicks: TcxTrackBarTicksAlign): TSize;
begin
  Result := RangeTrackBarScaledLeftThumbSize(AHorizontal, ATicks, dxSystemScaleFactor);
end;

function TcxCustomLookAndFeelPainter.RangeTrackBarRightThumbSize(AHorizontal: Boolean; ATicks: TcxTrackBarTicksAlign): TSize;
begin
  Result := RangeTrackBarScaledRightThumbSize(AHorizontal, ATicks, dxSystemScaleFactor);
end;

function TcxCustomLookAndFeelPainter.RangeTrackBarScaledRightThumbSize(
  AHorizontal: Boolean; ATicks: TcxTrackBarTicksAlign; AScaleFactor: TdxScaleFactor): TSize;
begin
  Result := cxNullSize;
end;

procedure TcxCustomLookAndFeelPainter.DrawRangeTrackBarScaledLeftThumb(ACanvas: TcxCanvas;
  const ARect: TRect; AState: TcxButtonState; AHorizontal: Boolean;
  ATicks: TcxTrackBarTicksAlign; AThumbColor: TColor; AScaleFactor: TdxScaleFactor);

  procedure GetBottomRightThumbShape(const AThumbRect: TRect;
    AHorizontal: Boolean;
    var ALightPolyLine, AShadowPolyLine, ADarkPolyLine, APolygon: TPoints);
  var
    AThumbSize: Integer;
    AMiddle: Integer;
  begin
    if AHorizontal then // Bottom
    begin
      AThumbSize := cxRectWidth(AThumbRect);
      AMiddle := AThumbRect.Bottom - AThumbSize;

      SetLength(ALightPolyLine, 4);
      SetLength(AShadowPolyLine, 3);
      SetLength(ADarkPolyLine, 2);
      SetLength(APolygon, 4);

      ALightPolyLine[0] := Point(AThumbRect.Right, AThumbRect.Top);
      ALightPolyLine[1] := Point(AThumbRect.Left, AThumbRect.Top);
      ALightPolyLine[2] := Point(AThumbRect.Left, AMiddle);
      ALightPolyLine[3] := Point(AThumbRect.Right, AThumbRect.Bottom);

      ADarkPolyLine[0] := Point(AThumbRect.Right, AThumbRect.Bottom);
      ADarkPolyLine[1] := Point(AThumbRect.Right, AThumbRect.Top);

      AShadowPolyLine[0] := Point(AThumbRect.Right - 1, AThumbRect.Bottom - 1);
      AShadowPolyLine[1] := Point(AThumbRect.Right - 1, AMiddle);
      AShadowPolyLine[2] := Point(AThumbRect.Right - 1, AThumbRect.Top);

      APolygon[0] := Point(AThumbRect.Right - 1, AThumbRect.Top + 1);
      APolygon[1] := Point(AThumbRect.Left + 1, AThumbRect.Top + 1);
      APolygon[2] := Point(AThumbRect.Left + 1, AMiddle);
      APolygon[3] := Point(AThumbRect.Right - 1, AThumbRect.Bottom - 1);
    end
    else // Right
    begin
      AThumbSize := cxRectHeight(AThumbRect);
      AMiddle := AThumbRect.Right - AThumbSize;

      SetLength(ALightPolyLine, 4);
      SetLength(AShadowPolyLine, 2);
      SetLength(ADarkPolyLine, 2);
      SetLength(APolygon, 4);

      ALightPolyLine[0] := Point(AThumbRect.Left, AThumbRect.Bottom);
      ALightPolyLine[1] := Point(AThumbRect.Left, AThumbRect.Top);
      ALightPolyLine[2] := Point(AMiddle, AThumbRect.Top);
      ALightPolyLine[3] := Point(AThumbRect.Right, AThumbRect.Bottom);

      ADarkPolyLine[0] := Point(AThumbRect.Right, AThumbRect.Bottom);
      ADarkPolyLine[1] := Point(AThumbRect.Left, AThumbRect.Bottom);

      AShadowPolyLine[0] := Point(AThumbRect.Right - 1, AThumbRect.Bottom - 1);
      AShadowPolyLine[1] := Point(AThumbRect.Left, AThumbRect.Bottom - 1);

      APolygon[0] := Point(AThumbRect.Left + 1, AThumbRect.Bottom - 1);
      APolygon[1] := Point(AThumbRect.Left + 1, AThumbRect.Top + 1);
      APolygon[2] := Point(AMiddle, AThumbRect.Top + 1);
      APolygon[3] := Point(AThumbRect.Right - 1, AThumbRect.Bottom - 1);
    end;
  end;

  procedure GetTopLeftThumbShape(const AThumbRect: TRect;
    AHorizontal: Boolean; var ALightPolyLine, AShadowPolyLine, ADarkPolyLine, APolygon: TPoints);
  var
    AThumbSize: Integer;
    AMiddle: Integer;
  begin
    if AHorizontal then // Top
    begin
      AThumbSize := cxRectWidth(AThumbRect);
      AMiddle := AThumbRect.Top + AThumbSize;

      SetLength(ALightPolyLine, 3);
      SetLength(AShadowPolyLine, 3);
      SetLength(ADarkPolyLine, 3);
      SetLength(APolygon, 4);

      ALightPolyLine[0] := Point(AThumbRect.Left, AThumbRect.Bottom - 1);
      ALightPolyLine[1] := Point(AThumbRect.Left, AMiddle - 1);
      ALightPolyLine[2] := Point(AThumbRect.Right, AThumbRect.Top - 1);

      AShadowPolyLine[0] := Point(AThumbRect.Right - 1, AThumbRect.Top);
      AShadowPolyLine[1] := Point(AThumbRect.Right - 1, AThumbRect.Bottom - 2);
      AShadowPolyLine[2] := Point(AThumbRect.Left, AThumbRect.Bottom - 2);

      ADarkPolyLine[0] := Point(AThumbRect.Right, AThumbRect.Top - 1);
      ADarkPolyLine[1] := Point(AThumbRect.Right, AThumbRect.Bottom - 1);
      ADarkPolyLine[2] := Point(AThumbRect.Left, AThumbRect.Bottom - 1);

      APolygon[0] := Point(AThumbRect.Right - 1, AThumbRect.Bottom - 2);
      APolygon[1] := Point(AThumbRect.Left + 1, AThumbRect.Bottom - 2);
      APolygon[2] := Point(AThumbRect.Left + 1, AMiddle - 1);
      APolygon[3] := Point(AThumbRect.Right - 1, AThumbRect.Top);
    end
    else // Left
    begin
      AThumbSize := cxRectHeight(AThumbRect);
      AMiddle := AThumbRect.Left + AThumbSize;

      SetLength(ALightPolyLine, 3);
      SetLength(AShadowPolyLine, 3);
      SetLength(ADarkPolyLine, 3);
      SetLength(APolygon, 4);

      ALightPolyLine[0] := Point(AThumbRect.Right - 1, AThumbRect.Top);
      ALightPolyLine[1] := Point(AMiddle - 1, AThumbRect.Top);
      ALightPolyLine[2] := Point(AThumbRect.Left - 1, AThumbRect.Bottom);

      ADarkPolyLine[0] := Point(AThumbRect.Left - 1, AThumbRect.Bottom);
      ADarkPolyLine[1] := Point(AThumbRect.Right - 1, AThumbRect.Bottom);
      ADarkPolyLine[2] := Point(AThumbRect.Right - 1, AThumbRect.Top);

      AShadowPolyLine[0] := Point(AThumbRect.Left, AThumbRect.Bottom - 1);
      AShadowPolyLine[1] := Point(AThumbRect.Right - 2, AThumbRect.Bottom - 1);
      AShadowPolyLine[2] := Point(AThumbRect.Right - 2, AThumbRect.Top);

      APolygon[0] := Point(AThumbRect.Right - 2, AThumbRect.Bottom - 1);
      APolygon[1] := Point(AThumbRect.Right - 2, AThumbRect.Top + 1);
      APolygon[2] := Point(AMiddle - 1, AThumbRect.Top + 1);
      APolygon[3] := Point(AThumbRect.Left, AThumbRect.Bottom - 1);
    end;
  end;

var
  ALightPolyLine, AShadowPolyLine, ADarkPolyLine, APolygon: TPoints;
  R: TRect;
begin
  ACanvas.Pen.Color := AThumbColor;
  ACanvas.Brush.Color := AThumbColor;
  R := GetRangeTrackBarThumbDrawRect(ARect, ATicks, AHorizontal);
  case ATicks of
    tbtaUp:
      begin
        GetTopLeftThumbShape(R, AHorizontal, ALightPolyLine, AShadowPolyLine, ADarkPolyLine, APolygon);
        ACanvas.Polygon(APolygon);
        DrawTrackBarThumbBorderUpDown(ACanvas, ALightPolyLine, AShadowPolyLine, ADarkPolyLine);
      end;
    tbtaDown:
      begin
        GetBottomRightThumbShape(R, AHorizontal, ALightPolyLine, AShadowPolyLine, ADarkPolyLine, APolygon);
        ACanvas.Polygon(APolygon);
        DrawTrackBarThumbBorderUpDown(ACanvas, ALightPolyLine, AShadowPolyLine, ADarkPolyLine);
      end;
    tbtaBoth:
    begin
      ACanvas.Canvas.FillRect(R);
      DrawTrackBarThumbBorderBoth(ACanvas, R);
    end;
  end;
end;

procedure TcxCustomLookAndFeelPainter.DrawRangeTrackBarLeftThumb(ACanvas: TcxCanvas; const ARect: TRect;
  AState: TcxButtonState; AHorizontal: Boolean; ATicks: TcxTrackBarTicksAlign; AThumbColor: TColor);
begin
  DrawRangeTrackBarScaledLeftThumb(ACanvas, ARect, AState, AHorizontal, ATicks, AThumbColor, dxSystemScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DrawRangeTrackBarRightThumb(ACanvas: TcxCanvas; const ARect: TRect;
  AState: TcxButtonState; AHorizontal: Boolean; ATicks: TcxTrackBarTicksAlign; AThumbColor: TColor);
begin
  DrawRangeTrackBarScaledRightThumb(ACanvas, ARect, AState, AHorizontal, ATicks, AThumbColor, dxSystemScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DrawRangeTrackBarScaledRightThumb(ACanvas: TcxCanvas;
  const ARect: TRect; AState: TcxButtonState; AHorizontal: Boolean;
  ATicks: TcxTrackBarTicksAlign; AThumbColor: TColor; AScaleFactor: TdxScaleFactor);

  procedure GetBottomRightThumbShape(const AThumbRect: TRect;
    AHorizontal: Boolean;
    var ALightPolyLine, AShadowPolyLine, ADarkPolyLine, APolygon: TPoints);
  var
    AThumbSize: Integer;
    AMiddle: Integer;
  begin
    if AHorizontal then // Bottom
    begin
      AThumbSize := cxRectWidth(AThumbRect);
      AMiddle := AThumbRect.Bottom - AThumbSize;

      SetLength(ALightPolyLine, 3);
      SetLength(AShadowPolyLine, 3);
      SetLength(ADarkPolyLine, 3);
      SetLength(APolygon, 4);

      ALightPolyLine[0] := Point(AThumbRect.Right, AThumbRect.Top);
      ALightPolyLine[1] := Point(AThumbRect.Left, AThumbRect.Top);
      ALightPolyLine[2] := Point(AThumbRect.Left, AThumbRect.Bottom);

      ADarkPolyLine[0] := Point(AThumbRect.Left, AThumbRect.Bottom);
      ADarkPolyLine[1] := Point(AThumbRect.Right, AMiddle);
      ADarkPolyLine[2] := Point(AThumbRect.Right, AThumbRect.Top);

      AShadowPolyLine[0] := Point(AThumbRect.Left, AThumbRect.Bottom - 1);
      AShadowPolyLine[1] := Point(AThumbRect.Right - 1, AMiddle);
      AShadowPolyLine[2] := Point(AThumbRect.Right - 1, AThumbRect.Top);

      APolygon[0] := Point(AThumbRect.Right - 1, AThumbRect.Top + 1);
      APolygon[1] := Point(AThumbRect.Left + 1, AThumbRect.Top + 1);
      APolygon[2] := Point(AThumbRect.Left + 1, AThumbRect.Bottom - 1);
      APolygon[3] := Point(AThumbRect.Right - 1, AMiddle);
    end
    else // Right
    begin
      AThumbSize := cxRectHeight(AThumbRect);
      AMiddle := AThumbRect.Right - AThumbSize;

      SetLength(ALightPolyLine, 3);
      SetLength(AShadowPolyLine, 3);
      SetLength(ADarkPolyLine, 3);
      SetLength(APolygon, 4);

      ALightPolyLine[0] := Point(AThumbRect.Left, AThumbRect.Bottom);
      ALightPolyLine[1] := Point(AThumbRect.Left, AThumbRect.Top);
      ALightPolyLine[2] := Point(AThumbRect.Right, AThumbRect.Top);

      ADarkPolyLine[0] := Point(AThumbRect.Right, AThumbRect.Top);
      ADarkPolyLine[1] := Point(AMiddle, AThumbRect.Bottom);
      ADarkPolyLine[2] := Point(AThumbRect.Left, AThumbRect.Bottom);

      AShadowPolyLine[0] := Point(AThumbRect.Right - 1, AThumbRect.Top);
      AShadowPolyLine[1] := Point(AMiddle, AThumbRect.Bottom - 1);
      AShadowPolyLine[2] := Point(AThumbRect.Left, AThumbRect.Bottom - 1);

      APolygon[0] := Point(AThumbRect.Left + 1, AThumbRect.Bottom - 1);
      APolygon[1] := Point(AThumbRect.Left + 1, AThumbRect.Top + 1);
      APolygon[2] := Point(AThumbRect.Right - 1, AThumbRect.Top + 1);
      APolygon[3] := Point(AMiddle, AThumbRect.Bottom - 1);
    end;
  end;

  procedure GetTopLeftThumbShape(const AThumbRect: TRect;
    AHorizontal: Boolean;
    var ALightPolyLine, AShadowPolyLine, ADarkPolyLine, APolygon: TPoints);
  var
    AThumbSize: Integer;
    AMiddle: Integer;
  begin
    if AHorizontal then // Top
    begin
      AThumbSize := cxRectWidth(AThumbRect);
      AMiddle := AThumbRect.Top + AThumbSize;

      SetLength(ALightPolyLine, 2);
      SetLength(AShadowPolyLine, 4);
      SetLength(ADarkPolyLine, 4);
      SetLength(APolygon, 4);

      ALightPolyLine[0] := Point(AThumbRect.Left, AThumbRect.Bottom - 1);
      ALightPolyLine[1] := Point(AThumbRect.Left, AThumbRect.Top - 1);

      AShadowPolyLine[0] := Point(AThumbRect.Left, AThumbRect.Top);
      AShadowPolyLine[1] := Point(AThumbRect.Right - 1, AMiddle - 1);
      AShadowPolyLine[2] := Point(AThumbRect.Right - 1, AThumbRect.Bottom - 2);
      AShadowPolyLine[3] := Point(AThumbRect.Left, AThumbRect.Bottom - 2);

      ADarkPolyLine[0] := Point(AThumbRect.Left, AThumbRect.Top - 1);
      ADarkPolyLine[1] := Point(AThumbRect.Right, AMiddle - 1);
      ADarkPolyLine[2] := Point(AThumbRect.Right, AThumbRect.Bottom - 1);
      ADarkPolyLine[3] := Point(AThumbRect.Left, AThumbRect.Bottom - 1);

      APolygon[0] := Point(AThumbRect.Right - 1, AThumbRect.Bottom - 2);
      APolygon[1] := Point(AThumbRect.Left + 1, AThumbRect.Bottom - 2);
      APolygon[2] := Point(AThumbRect.Left + 1, AThumbRect.Top);
      APolygon[3] := Point(AThumbRect.Right - 1, AMiddle - 1);
    end
    else // Left
    begin
      AThumbSize := cxRectHeight(AThumbRect);
      AMiddle := AThumbRect.Left + AThumbSize;

      SetLength(ALightPolyLine, 2);
      SetLength(AShadowPolyLine, 4);
      SetLength(ADarkPolyLine, 4);
      SetLength(APolygon, 4);

      ALightPolyLine[0] := Point(AThumbRect.Right - 1, AThumbRect.Top);
      ALightPolyLine[1] := Point(AThumbRect.Left - 1, AThumbRect.Top);

      ADarkPolyLine[0] := Point(AThumbRect.Left - 1, AThumbRect.Top);
      ADarkPolyLine[1] := Point(AMiddle - 1, AThumbRect.Bottom);
      ADarkPolyLine[2] := Point(AThumbRect.Right - 1, AThumbRect.Bottom);
      ADarkPolyLine[3] := Point(AThumbRect.Right - 1, AThumbRect.Top);

      AShadowPolyLine[0] := Point(AThumbRect.Left, AThumbRect.Top);
      AShadowPolyLine[1] := Point(AMiddle - 1, AThumbRect.Bottom - 1);
      AShadowPolyLine[2] := Point(AThumbRect.Right - 2, AThumbRect.Bottom - 1);
      AShadowPolyLine[3] := Point(AThumbRect.Right - 2, AThumbRect.Top);

      APolygon[0] := Point(AThumbRect.Right - 2, AThumbRect.Bottom - 1);
      APolygon[1] := Point(AThumbRect.Right - 2, AThumbRect.Top + 1);
      APolygon[2] := Point(AThumbRect.Left, AThumbRect.Top + 1);
      APolygon[3] := Point(AMiddle - 1, AThumbRect.Bottom - 1);
    end;
  end;

var
  ALightPolyLine, AShadowPolyLine, ADarkPolyLine, APolygon: TPoints;
  R: TRect;
begin
  ACanvas.Pen.Color := AThumbColor;
  ACanvas.Brush.Color := AThumbColor;
  R := GetRangeTrackBarThumbDrawRect(ARect, ATicks, AHorizontal);
  case ATicks of
    tbtaUp:
      begin
        GetTopLeftThumbShape(R, AHorizontal, ALightPolyLine, AShadowPolyLine, ADarkPolyLine, APolygon);
        ACanvas.Polygon(APolygon);
        DrawTrackBarThumbBorderUpDown(ACanvas, ALightPolyLine, AShadowPolyLine, ADarkPolyLine);
      end;
    tbtaDown:
      begin
        GetBottomRightThumbShape(R, AHorizontal, ALightPolyLine, AShadowPolyLine, ADarkPolyLine, APolygon);
        ACanvas.Polygon(APolygon);
        DrawTrackBarThumbBorderUpDown(ACanvas, ALightPolyLine, AShadowPolyLine, ADarkPolyLine);
      end;
    tbtaBoth:
    begin
      ACanvas.Canvas.FillRect(R);
      DrawTrackBarThumbBorderBoth(ACanvas, R);
    end;
  end;
end;

function TcxCustomLookAndFeelPainter.GetSplitterInnerColor(AHighlighted: Boolean): TColor;
begin
  Result := clWhite;
end;

function TcxCustomLookAndFeelPainter.GetSplitterOuterColor(AHighlighted: Boolean): TColor;
begin
  Result := clBtnShadow;
end;

procedure TcxCustomLookAndFeelPainter.DrawScaledSplitter(ACanvas: TcxCanvas; const ARect: TRect;
  AHighlighted, AClicked, AHorizontal: Boolean; AScaleFactor: TdxScaleFactor; AHasCloseMark: Boolean = False; AArrowDirection: TcxArrowDirection = adLeft);

  procedure InternalDrawSplitter(AColor: TColor; const R: TRect; AHasInnerLine: Boolean);

    procedure SetFirst(var P1, P2: TPoint);
    begin
      P1 := R.TopLeft;
      P2 := P1;
      if AHorizontal then
        P2.X := (R.Right + R.Left) div 2 - 5
      else
        P2.Y := (R.Top + R.Bottom) div 2 - 5;
    end;

    procedure SetNext(var P1, P2: TPoint);
    begin
      if AHorizontal then
      begin
        P1.X := P2.X + 1;
        P2.X := P1.X + 2;
      end
      else
      begin
        P1.Y := P2.Y + 1;
        P2.Y := P1.Y + 2;
      end;
    end;

    procedure SetLast(var P1, P2: TPoint);
    begin
      SetNext(P1, P2);
      if AHorizontal then
        P2.X := R.Right
      else
        P2.Y := R.Bottom;
    end;

  var
    I: Integer;
    P1, P2: TPoint;
  begin
    ACanvas.Pen.Color := AColor;
    SetFirst(P1, P2);
    ACanvas.Polyline([P1, P2]);
    for I := 0 to 2 do
    begin
      SetNext(P1, P2);
      if not AHasCloseMark then
        ACanvas.Polyline([P1, P2]);
    end;
    SetLast(P1, P2);
    if AHasCloseMark then
      P1 := Point(P1.X + Ord(AHorizontal) + 2 * Ord(AHorizontal and AHasInnerLine),
        P1.Y + Ord(not AHorizontal) + 2 * Ord(not AHorizontal and AHasInnerLine));
    ACanvas.Polyline([P1, P2]);
  end;

var
  R: TRect;
  AHasInnerLine: Boolean;
begin
  AHasInnerLine := HasSplitterInnerLine(AHorizontal, AScaleFactor);
  R := cxRectCenter(ARect, IfThen(AHorizontal, cxRectWidth(ARect), 1), IfThen(AHorizontal, 1, cxRectHeight(ARect)));
  R := Rect(R.Left, R.Top, R.Right - IfThen(AHorizontal and AHasInnerLine, 1, 0), R.Bottom - IfThen(not AHorizontal and AHasInnerLine, 1, 0));
  InternalDrawSplitter(GetSplitterOuterColor(AHighlighted), R, AHasInnerLine);
  if AHasInnerLine then
  begin
    R := cxRectOffset(R, 1, 1);
    InternalDrawSplitter(GetSplitterInnerColor(AHighlighted), R, AHasInnerLine);
  end;

  if AHasCloseMark then
    DrawSplitterCloseMark(ACanvas, ARect, AHighlighted, AClicked, AHorizontal, AScaleFactor, AArrowDirection);
end;

procedure TcxCustomLookAndFeelPainter.DrawSplitter(ACanvas: TcxCanvas;
  const ARect: TRect; AHighlighted: Boolean; AClicked: Boolean; AHorizontal: Boolean);
begin
  DrawScaledSplitter(ACanvas, ARect, AHighlighted, AClicked, AHorizontal, dxSystemScaleFactor)
end;

procedure TcxCustomLookAndFeelPainter.DrawSplitterCloseMark(ACanvas: TcxCanvas; const ARect: TRect;
  AHighlighted, AClicked, AHorizontal: Boolean; AScaleFactor: TdxScaleFactor; AArrowDirection: TcxArrowDirection);

  procedure DrawCanvasLine(const AFromPoint, AToPoint: TPoint);
  begin
    ACanvas.MoveTo(AFromPoint.x, AFromPoint.y);
    ACanvas.LineTo(AToPoint.x, AToPoint.y);
  end;

var
  I: Integer;
  ACenter: TPoint;
begin
  ACanvas.Pen.Color := GetSplitterOuterColor(False);
  ACenter := cxRectCenter(ARect);
  ACenter.X := ACenter.X +
    Ord(AHorizontal and HasSplitterInnerLine(AHorizontal, AScaleFactor) and (((ARect.Right - ARect.Left) mod 2) <> 0));

  ACenter.Y := ACenter.Y + Ord(not AHorizontal and HasSplitterInnerLine(AHorizontal, AScaleFactor) and (((ARect.Bottom - ARect.Top) mod 2) <> 0));
  case AArrowDirection of
    adUp:
      for I := 0 to AScaleFactor.Apply(3) do
      begin
        DrawCanvasLine(Point(ACenter.X - I, ACenter.Y - AScaleFactor.Apply(3) + I),
          Point(ACenter.X - I, ACenter.Y - AScaleFactor.Apply(5) + I));
        DrawCanvasLine(Point(ACenter.X + I, ACenter.Y - AScaleFactor.Apply(3) + I),
          Point(ACenter.X + I, ACenter.Y - AScaleFactor.Apply(5) + I));
      end;
    adDown:
      for I := 0 to AScaleFactor.Apply(3) do
      begin
        DrawCanvasLine(Point(ACenter.X - I, ACenter.Y + AScaleFactor.Apply(2) - I ),
          Point(ACenter.X - I, ACenter.Y + AScaleFactor.Apply(4) - I));
        DrawCanvasLine(Point(ACenter.X + I, ACenter.Y + AScaleFactor.Apply(2) - I),
          Point(ACenter.X + I, ACenter.Y + AScaleFactor.Apply(4) - I));
      end;
    adLeft:
      for I := 0 to AScaleFactor.Apply(3) do
      begin
        DrawCanvasLine(Point(ACenter.X - AScaleFactor.Apply(3) + I, ACenter.Y - I),
          Point(ACenter.X - AScaleFactor.Apply(5) + I, ACenter.Y - I));
        DrawCanvasLine(Point(ACenter.X - AScaleFactor.Apply(3) + I, ACenter.Y + I),
          Point(ACenter.X - AScaleFactor.Apply(5) + I, ACenter.Y + I));
      end;
  else {adRight}
    for I := 0 to AScaleFactor.Apply(3) do
    begin
      DrawCanvasLine(Point(ACenter.X + AScaleFactor.Apply(2) - I, ACenter.Y - I),
        Point(ACenter.X + AScaleFactor.Apply(4) - I, ACenter.Y - I));
      DrawCanvasLine(Point(ACenter.X + AScaleFactor.Apply(2) - I, ACenter.Y + I),
        Point(ACenter.X + AScaleFactor.Apply(4) - I, ACenter.Y + I));
    end;
  end;
end;

function TcxCustomLookAndFeelPainter.GetScaledSplitterSize(AHorizontal: Boolean; AScaleFactor: TdxScaleFactor): TSize;
begin
  if AHorizontal then
    Result := Size(17, 2)
  else
    Result := Size(2, 17);

  Result := AScaleFactor.Apply(Result);
end;

function TcxCustomLookAndFeelPainter.GetSplitterSize(AHorizontal: Boolean): TSize;
begin
  Result := GetScaledSplitterSize(AHorizontal, dxSystemScaleFactor);
end;

function TcxCustomLookAndFeelPainter.HasSplitterInnerLine(AHorizontal: Boolean; AScaleFactor: TdxScaleFactor): Boolean;
var
  ASplitterSize: TSize;
begin
  ASplitterSize := GetScaledSplitterSize(AHorizontal, AScaleFactor);
  Result := (AHorizontal and (ASplitterSize.cy > 1)) or (not AHorizontal and (ASplitterSize.cx > 1));
end;

function TcxCustomLookAndFeelPainter.GetWindowContentTextColor: TColor;
begin
  Result := clBtnText;
end;

function TcxCustomLookAndFeelPainter.GetScaledZoomInButtonSize(AScaleFactor: TdxScaleFactor): TSize;
begin
  Result := AScaleFactor.Apply(Size(ZoomButtonWidth, ZoomButtonHeight));
end;

function TcxCustomLookAndFeelPainter.GetScaledZoomOutButtonSize(AScaleFactor: TdxScaleFactor): TSize;
begin
  Result := AScaleFactor.Apply(Size(ZoomButtonWidth, ZoomButtonHeight));
end;

function TcxCustomLookAndFeelPainter.GetZoomInButtonSize: TSize;
begin
  Result := GetScaledZoomInButtonSize(dxSystemScaleFactor)
end;

function TcxCustomLookAndFeelPainter.GetZoomOutButtonSize: TSize;
begin
  Result := GetScaledZoomOutButtonSize(dxSystemScaleFactor)
end;

procedure TcxCustomLookAndFeelPainter.DrawZoomInButton(ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState);
begin
  DrawScaledZoomInButton(ACanvas, R, AState, dxSystemScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DrawZoomOutButton(ACanvas: TcxCanvas; const R: TRect; AState: TcxButtonState);
begin
  DrawScaledZoomOutButton(ACanvas, R, AState, dxSystemScaleFactor);
end;

function TcxCustomLookAndFeelPainter.LayoutControlEmptyAreaColor: TColor;
begin
  Result := clBtnFace;
end;

function TcxCustomLookAndFeelPainter.LayoutControlGetColorPaletteForGroupButton(AState: TcxButtonState): IdxColorPalette;
begin
  Result := nil;
end;

function TcxCustomLookAndFeelPainter.LayoutControlGetColorPaletteForItemCaption: IdxColorPalette;
begin
  Result := nil;
end;

function TcxCustomLookAndFeelPainter.LayoutControlGetColorPaletteForTabbedGroupCaption(AIsActive: Boolean): IdxColorPalette;
begin
  Result := nil;
end;

procedure TcxCustomLookAndFeelPainter.DrawLayoutControlBackground(ACanvas: TcxCanvas; const R: TRect);
begin
  ACanvas.FillRect(R, LayoutControlEmptyAreaColor);
end;

procedure TcxCustomLookAndFeelPainter.DrawScrollBoxBackground(ACanvas: TcxCanvas; const R: TRect; AColor: TColor);
begin
  ACanvas.FillRect(R, AColor);
end;

function TcxCustomLookAndFeelPainter.PrintPreviewBackgroundTextColor: TColor;
begin
  Result := clWindowText;
end;

function TcxCustomLookAndFeelPainter.PrintPreviewPageBordersWidth: TRect;
begin
  Result := PrintPreviewPageBordersScaledWidth(dxSystemScaleFactor);
end;

function TcxCustomLookAndFeelPainter.PrintPreviewPageBordersScaledWidth(AScaleFactor: TdxScaleFactor): TRect;
begin
  Result := Rect(2, 2, 4, 4);
end;

procedure TcxCustomLookAndFeelPainter.DrawPrintPreviewBackground(
  ACanvas: TcxCanvas; const R: TRect);
begin
  DrawPrintPreviewScaledBackground(ACanvas, R, dxSystemScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DrawPrintPreviewScaledBackground(ACanvas: TcxCanvas; const R: TRect;
  AScaleFactor: TdxScaleFactor);
begin
  ACanvas.FillRect(R, clBtnShadow);
end;

procedure TcxCustomLookAndFeelPainter.DrawPrintPreviewPageBackground(
  ACanvas: TcxCanvas; const ABorderRect, AContentRect: TRect;
  ASelected, ADrawContent: Boolean);
begin
  DrawPrintPreviewPageScaledBackground(ACanvas, ABorderRect, AContentRect, ASelected, ADrawContent, dxSystemScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DrawPrintPreviewPageScaledBackground(ACanvas: TcxCanvas;
  const ABorderRect, AContentRect: TRect; ASelected, ADrawContent: Boolean; AScaleFactor: TdxScaleFactor);
const
  FrameColorMap: array[Boolean] of TColor = (clWindowText, clHighlight);
var
  R: TRect;
begin
  ACanvas.SaveClipRegion;
  try
    R := cxRectInflate(ABorderRect, cxRect(0, 0, -2, -2));
    if ADrawContent then
      ACanvas.FillRect(AContentRect, clWindow);
    if not ASelected then
      InflateRect(R, -1, -1);
    ACanvas.ExcludeClipRect(AContentRect);
    ACanvas.FillRect(R, FrameColorMap[ASelected]);
    ACanvas.ExcludeClipRect(R);
    ACanvas.FillRect(cxRectOffset(R, 2, 2), clWindowText);
  finally
    ACanvas.RestoreClipRegion;
  end;
end;

function TcxCustomLookAndFeelPainter.CalcEditButtonTextColor(
  AButtonKind: TcxCalcButtonKind): TColor;
const
  BtnColors : array [TcxCalcButtonKind] of TColor = (
    clMaroon, clMaroon, clMaroon, clRed, clRed, clRed, clRed, clBlue, clBlue,
    clBlue, clBlue, clBlue, clBlue, clBlue, clBlue, clBlue, clBlue, clBlue,
    clBlue, clRed, clRed, clRed, clRed, clNavy, clNavy, clNavy, clRed, clBlue);
begin
  Result := BtnColors[AButtonKind];
end;

function TcxCustomLookAndFeelPainter.GetCustomizationFormListBackgroundColor: TColor;
begin
  Result := clBtnFace;
end;

procedure TcxCustomLookAndFeelPainter.DrawMessageBox(ACanvas: TcxCanvas;
  const ABounds: TRect; const AMessage: string; AFont: TFont = nil; AColor: TColor = clNone);
begin
  ACanvas.FrameRect(ABounds, DefaultGridLineColor, 1, cxBordersAll, True);
  if AColor = clNone then
    AColor := DefaultGroupColor;
  ACanvas.FillRect(ABounds, AColor);
  if AFont <> nil then
    ACanvas.Font := AFont;
  ACanvas.Brush.Style := bsClear;
  ACanvas.DrawTexT(AMessage, ABounds, cxAlignCenter or cxSingleLine);
  ACanvas.Brush.Style := bsSolid;
end;

function TcxCustomLookAndFeelPainter.BreadcrumbEditBackgroundColor(
  AState: TdxBreadcrumbEditState): TColor;
begin
  Result := clBtnFace;
end;

function TcxCustomLookAndFeelPainter.BreadcrumbEditBordersSize: TRect;
begin
  Result := cxSimpleRect;
end;

function TcxCustomLookAndFeelPainter.BreadcrumbEditScaledButtonAreaSeparatorSize(AScaleFactor: TdxScaleFactor): Integer;
begin
  Result := 2;
end;

function TcxCustomLookAndFeelPainter.BreadcrumbEditButtonAreaSeparatorSize: Integer;
begin
  Result := BreadcrumbEditScaledButtonAreaSeparatorSize(dxSystemScaleFactor);
end;

function TcxCustomLookAndFeelPainter.BreadcrumbEditButtonContentOffsets(AIsFirst, AIsLast: Boolean): TRect;
begin
  Result := BreadcrumbEditScaledButtonContentOffsets(AIsFirst, AIsLast, dxSystemScaleFactor);
end;

function TcxCustomLookAndFeelPainter.BreadcrumbEditButtonColorPalette(AState: TdxBreadcrumbEditButtonState): IdxColorPalette;
begin
  Result := nil;
end;

function TcxCustomLookAndFeelPainter.BreadcrumbEditScaledButtonContentOffsets(
  AIsFirst, AIsLast: Boolean; AScaleFactor: TdxScaleFactor): TRect;
begin
  Result := AScaleFactor.Apply(cxRect(3, 3, 3, 3));
end;

function TcxCustomLookAndFeelPainter.BreadcrumbEditScaledDropDownButtonWidth(AScaleFactor: TdxScaleFactor): Integer;
begin
  Result := dxGetSystemMetrics(SM_CXHSCROLL, AScaleFactor);
end;

function TcxCustomLookAndFeelPainter.BreadcrumbEditDropDownButtonWidth: Integer;
begin
  Result := BreadcrumbEditScaledDropDownButtonWidth(dxSystemScaleFactor);
end;

function TcxCustomLookAndFeelPainter.BreadcrumbEditIsFadingSupports: Boolean;
begin
  Result := False;
end;

function TcxCustomLookAndFeelPainter.BreadcrumbEditScaledNodeDelimiterSize(AScaleFactor: TdxScaleFactor): Integer;
begin
  Result := AScaleFactor.Apply(16);
end;

function TcxCustomLookAndFeelPainter.BreadcrumbEditNodeDelimiterSize: Integer;
begin
  Result := BreadcrumbEditScaledNodeDelimiterSize(dxSystemScaleFactor);
end;

function TcxCustomLookAndFeelPainter.BreadcrumbEditNodeTextColor(
  AState: TdxBreadcrumbEditButtonState): TColor;
begin
  if AState = dxbcbsDisabled then
    Result := ButtonSymbolColor(cxbsDisabled)
  else
    Result := ButtonSymbolColor(cxbsNormal);
end;

function TcxCustomLookAndFeelPainter.BreadcrumbEditScaledNodeTextOffsets(AScaleFactor: TdxScaleFactor): TRect;
begin
  Result := AScaleFactor.Apply(cxRect(4, 4, 4, 4));
end;

function TcxCustomLookAndFeelPainter.BreadcrumbEditNodeTextOffsets: TRect;
begin
  Result := BreadcrumbEditScaledNodeTextOffsets(dxSystemScaleFactor);
end;

function TcxCustomLookAndFeelPainter.BreadcrumbEditScaledProgressChunkPadding(AScaleFactor: TdxScaleFactor): TRect;
begin
  Result := cxSimpleRect;
end;

function TcxCustomLookAndFeelPainter.BreadcrumbEditProgressChunkPadding: TRect;
begin
  Result := BreadcrumbEditScaledProgressChunkPadding(dxSystemScaleFactor);
end;

function TcxCustomLookAndFeelPainter.BreadcrumbEditScaledProgressChunkOverlaySize(AScaleFactor: TdxScaleFactor): TSize;
begin
  Result := cxNullSize;
end;

function TcxCustomLookAndFeelPainter.BreadcrumbEditProgressChunkOverlaySize: TSize;
begin
  Result := BreadcrumbEditScaledProgressChunkOverlaySize(dxSystemScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DrawBreadcrumbEditBorders(ACanvas: TcxCanvas;
  const ARect: TRect; ABorders: TcxBorders; AState: TdxBreadcrumbEditState);
begin
  ACanvas.DrawComplexFrame(ARect, clBtnShadow, clBtnHighlight, ABorders);
end;

procedure TcxCustomLookAndFeelPainter.DrawBreadcrumbEditScaledButtonAreaSeparator(
  ACanvas: TcxCanvas; const ARect: TRect; AState: TdxBreadcrumbEditState; AScaleFactor: TdxScaleFactor);
begin
  // do nothing
end;

procedure TcxCustomLookAndFeelPainter.DrawBreadcrumbEditButtonAreaSeparator(
  ACanvas: TcxCanvas; const ARect: TRect; AState: TdxBreadcrumbEditState);
begin
  DrawBreadcrumbEditScaledButtonAreaSeparator(ACanvas, ARect, AState, dxSystemScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DrawBreadcrumbEditButton(
  ACanvas: TcxCanvas; const ARect: TRect; AState: TdxBreadcrumbEditButtonState; AIsFirst, AIsLast: Boolean);
begin
  DrawBreadcrumbEditScaledButton(ACanvas, ARect, AState, AIsFirst, AIsLast, dxSystemScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DrawBreadcrumbEditScaledButton(ACanvas: TcxCanvas; const ARect: TRect;
  AState: TdxBreadcrumbEditButtonState; AIsFirst, AIsLast: Boolean; AScaleFactor: TdxScaleFactor);
begin
  if not (AState in [dxbcbsNormal, dxbcbsDisabled]) then
    DrawScaledButton(ACanvas, ARect, '', BreadcrumbButtonStateToButtonState[AState], AScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DrawBreadcrumbEditScaledDropDownButton(ACanvas: TcxCanvas;
  const ARect: TRect; AState: TdxBreadcrumbEditButtonState; AIsInEditor: Boolean; AScaleFactor: TdxScaleFactor);
begin
  if AIsInEditor then
    DrawScaledButton(ACanvas, ARect, '', BreadcrumbButtonStateToButtonState[AState], AScaleFactor)
  else
    DrawBreadcrumbEditScaledButton(ACanvas, ARect, AState, True, True, AScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DrawBreadcrumbEditDropDownButton(
  ACanvas: TcxCanvas; const ARect: TRect; AState: TdxBreadcrumbEditButtonState; AIsInEditor: Boolean);
begin
  DrawBreadcrumbEditScaledDropDownButton(ACanvas, ARect, AState, AIsInEditor, dxSystemScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DrawBreadcrumbEditScaledDropDownButtonGlyph(ACanvas: TcxCanvas;
  const ARect: TRect; AState: TdxBreadcrumbEditButtonState; AIsInEditor: Boolean; AScaleFactor: TdxScaleFactor);
begin
  DrawButtonArrow(ACanvas, cxRectInflate(ARect, -AScaleFactor.Apply(cxTextOffset)),
    ButtonSymbolColor(BreadcrumbButtonStateToButtonState[AState]));
end;

procedure TcxCustomLookAndFeelPainter.DrawBreadcrumbEditDropDownButtonGlyph(
  ACanvas: TcxCanvas; const ARect: TRect; AState: TdxBreadcrumbEditButtonState; AIsInEditor: Boolean);
begin
  DrawBreadcrumbEditScaledDropDownButtonGlyph(ACanvas, ARect, AState, AIsInEditor, dxSystemScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DrawBreadcrumbEditScaledNode(ACanvas: TcxCanvas; const R: TRect;
  AState: TdxBreadcrumbEditButtonState; AHasDelimiter: Boolean; AScaleFactor: TdxScaleFactor);
begin
  DrawBreadcrumbEditScaledButton(ACanvas, R, AState, False, AHasDelimiter, AScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DrawBreadcrumbEditNode(ACanvas: TcxCanvas;
  const R: TRect; AState: TdxBreadcrumbEditButtonState; AHasDelimiter: Boolean);
begin
  DrawBreadcrumbEditScaledNode(ACanvas, R, AState, AHasDelimiter, dxSystemScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DrawBreadcrumbEditScaledNodeDelimiter(
  ACanvas: TcxCanvas; const R: TRect; AState: TdxBreadcrumbEditButtonState; AScaleFactor: TdxScaleFactor);
begin
  DrawBreadcrumbEditScaledButton(ACanvas, R, AState, False, False, AScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DrawBreadcrumbEditNodeDelimiter(
  ACanvas: TcxCanvas; const R: TRect; AState: TdxBreadcrumbEditButtonState);
begin
  DrawBreadcrumbEditScaledNodeDelimiter(ACanvas, R, AState, dxSystemScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DrawBreadcrumbEditScaledNodeDelimiterGlyph(
  ACanvas: TcxCanvas; const R: TRect; AState: TdxBreadcrumbEditButtonState; AScaleFactor: TdxScaleFactor);
begin
  DrawArrow(ACanvas, R, adDown, BreadcrumbEditNodeTextColor(AState));
end;

procedure TcxCustomLookAndFeelPainter.DrawBreadcrumbEditNodeDelimiterGlyph(
  ACanvas: TcxCanvas; const R: TRect; AState: TdxBreadcrumbEditButtonState);
begin
  DrawBreadcrumbEditScaledNodeDelimiterGlyph(ACanvas, R, AState, dxSystemScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DrawBreadcrumbEditNodeMoreButtonGlyph(
  ACanvas: TcxCanvas; const R: TRect; AState: TdxBreadcrumbEditButtonState);
begin
  DrawBreadcrumbEditScaledNodeMoreButtonGlyph(ACanvas, R, AState, dxSystemScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DrawBreadcrumbEditScaledNodeMoreButtonGlyph(
  ACanvas: TcxCanvas; const R: TRect; AState: TdxBreadcrumbEditButtonState; AScaleFactor: TdxScaleFactor);
begin
  DrawBreadcrumbEditScaledNodeDelimiterGlyph(ACanvas, R, AState, AScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DrawBreadcrumbEditScaledProgressChunk(
  ACanvas: TcxCanvas; const R: TRect; AScaleFactor: TdxScaleFactor);
begin
  ACanvas.FillRect(R, $AAA8A4);
end;

procedure TcxCustomLookAndFeelPainter.DrawBreadcrumbEditProgressChunk(ACanvas: TcxCanvas; const R: TRect);
begin
  DrawBreadcrumbEditScaledProgressChunk(ACanvas, R, dxSystemScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DrawBreadcrumbEditScaledProgressChunkOverlay(
  ACanvas: TcxCanvas; const R: TRect; AScaleFactor: TdxScaleFactor);
begin
  // do nothing
end;

procedure TcxCustomLookAndFeelPainter.DrawBreadcrumbEditProgressChunkOverlay(ACanvas: TcxCanvas; const R: TRect);
begin
  DrawBreadcrumbEditScaledProgressChunkOverlay(ACanvas, R, dxSystemScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DrawDropDownListBoxBackground(
  ACanvas: TcxCanvas; const ARect: TRect; AHasBorders: Boolean);
var
  R: TRect;
begin
  R := ARect;
  if AHasBorders then
  begin
    ACanvas.DrawComplexFrame(R, cl3DLight, cl3DDkShadow);
    R := cxRectInflate(R, -1, -1);
    ACanvas.DrawComplexFrame(R, clBtnHighlight, clBtnShadow);
    R := cxRectInflate(R, -1, -1);
  end;
  ACanvas.FillRect(R, clMenu);
end;

procedure TcxCustomLookAndFeelPainter.DrawDropDownListBoxScaledGutterBackground(
  ACanvas: TcxCanvas; const ARect: TRect; AScaleFactor: TdxScaleFactor);
begin
  // do nothing
end;

procedure TcxCustomLookAndFeelPainter.DrawDropDownListBoxGutterBackground(ACanvas: TcxCanvas; const ARect: TRect);
begin
  DrawDropDownListBoxScaledGutterBackground(ACanvas, ARect, dxSystemScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DrawDropDownListBoxScaledSelection(
  ACanvas: TcxCanvas; const ARect, AGutterRect: TRect; AScaleFactor: TdxScaleFactor);
begin
  ACanvas.FillRect(ARect, clMenuHighlight);
end;

procedure TcxCustomLookAndFeelPainter.DrawDropDownListBoxSelection(ACanvas: TcxCanvas; const ARect, AGutterRect: TRect);
begin
  DrawDropDownListBoxScaledSelection(ACanvas, ARect, AGutterRect, dxSystemScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DrawDropDownListBoxSeparator(ACanvas: TcxCanvas; const ARect, AGutterRect: TRect);
begin
  DrawDropDownListBoxScaledSeparator(ACanvas, ARect, AGutterRect, dxSystemScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DrawDropDownListBoxScaledSeparator(
  ACanvas: TcxCanvas; const ARect, AGutterRect: TRect; AScaleFactor: TdxScaleFactor);
begin
  ACanvas.DrawComplexFrame(cxRectCenterVertically(ARect, 2), clBtnShadow, clBtnHighlight, [bTop, bBottom]);
end;

function TcxCustomLookAndFeelPainter.DropDownListBoxItemImageOffsets: TRect;
begin
  Result := DropDownListBoxScaledItemImageOffsets(dxSystemScaleFactor);
end;

function TcxCustomLookAndFeelPainter.DropDownListBoxScaledItemImageOffsets(AScaleFactor: TdxScaleFactor): TRect;
begin
  Result := AScaleFactor.Apply(cxRect(5, 3, 5, 3));
end;

function TcxCustomLookAndFeelPainter.DropDownListBoxItemTextColor(ASelected: Boolean): TColor;
begin
  if ASelected then
    Result := clHighlightText
  else
    Result := clMenuText;
end;

function TcxCustomLookAndFeelPainter.DropDownListBoxScaledItemTextOffsets(AScaleFactor: TdxScaleFactor): TRect;
begin
  Result := AScaleFactor.Apply(cxRect(5, cxTextOffset, 5, cxTextOffset));
end;

function TcxCustomLookAndFeelPainter.DropDownListBoxItemTextOffsets: TRect;
begin
  Result := DropDownListBoxScaledItemTextOffsets(dxSystemScaleFactor);
end;

function TcxCustomLookAndFeelPainter.DropDownListBoxScaledSeparatorSize(AScaleFactor: TdxScaleFactor): Integer;
begin
  Result := AScaleFactor.Apply(8);
end;

function TcxCustomLookAndFeelPainter.DropDownListBoxSeparatorSize: Integer;
begin
  Result := DropDownListBoxScaledSeparatorSize(dxSystemScaleFactor);
end;

function TcxCustomLookAndFeelPainter.DropDownListBoxBordersSize: Integer;
begin
  Result := 3;
end;

function TcxCustomLookAndFeelPainter.AlertWindowButtonContentOffsets(AKind: TdxAlertWindowButtonKind): TRect;
begin
  Result := AlertWindowScaledButtonContentOffsets(AKind, dxSystemScaleFactor);
end;

function TcxCustomLookAndFeelPainter.AlertWindowScaledButtonContentOffsets(
  AKind: TdxAlertWindowButtonKind; AScaleFactor: TdxScaleFactor): TRect;
begin
  Result := AScaleFactor.Apply(cxRect(3, 3, 3, 3));
end;

function TcxCustomLookAndFeelPainter.AlertWindowButtonGlyphSize(AKind: TdxAlertWindowButtonKind): TSize;
begin
  Result := AlertWindowScaledButtonGlyphSize(AKind, dxSystemScaleFactor)
end;

function TcxCustomLookAndFeelPainter.AlertWindowButtonGetColorPalette(AState: TcxButtonState): IdxColorPalette;
begin
  Result := ButtonColorPalette(AState);
end;

function TcxCustomLookAndFeelPainter.AlertWindowScaledButtonGlyphSize(
  AKind: TdxAlertWindowButtonKind; AScaleFactor: TdxScaleFactor): TSize;
begin
  if AKind <> awbkCustom then
    Result := AScaleFactor.Apply(cxSize(10, 10))
  else
    Result := cxNullSize;
end;

function TcxCustomLookAndFeelPainter.AlertWindowScaledContentOffsets(AScaleFactor: TdxScaleFactor): TRect;
begin
  Result := AScaleFactor.Apply(cxRect(4, 4, 4, 4));
end;

function TcxCustomLookAndFeelPainter.AlertWindowContentOffsets: TRect;
begin
  Result := AlertWindowScaledContentOffsets(dxSystemScaleFactor);
end;

function TcxCustomLookAndFeelPainter.AlertWindowCornerRadius: Integer;
begin
  Result := 0;
end;

function TcxCustomLookAndFeelPainter.AlertWindowNavigationPanelTextColor: TColor;
begin
  Result := AlertWindowTextColor;
end;

function TcxCustomLookAndFeelPainter.AlertWindowTextColor: TColor;
begin
  Result := clBtnText;
end;

procedure TcxCustomLookAndFeelPainter.DrawAlertWindowBackground(
  ACanvas: TcxCanvas; const ABounds: TRect; AScaleFactor: TdxScaleFactor = nil);
begin
  ACanvas.FillRect(ABounds, clBtnFace);
  ACanvas.Canvas.RoundRect(ABounds.Left, ABounds.Top, ABounds.Right, ABounds.Bottom,
    AlertWindowCornerRadius, AlertWindowCornerRadius);
end;

procedure TcxCustomLookAndFeelPainter.DrawAlertWindowButton(ACanvas: TcxCanvas; const ABounds: TRect;
  AState: TcxButtonState; AKind: TdxAlertWindowButtonKind; ADown: Boolean = False);
begin
  DrawAlertWindowScaledButton(ACanvas, ABounds, AState, AKind, dxSystemScaleFactor, ADown);
end;

procedure TcxCustomLookAndFeelPainter.DrawAlertWindowScaledButton(ACanvas: TcxCanvas; const ABounds: TRect;
  AState: TcxButtonState; AKind: TdxAlertWindowButtonKind; AScaleFactor: TdxScaleFactor; ADown: Boolean = False);
var
  R: TRect;
begin
  if ADown then
    AState := cxbsPressed;
  DrawScaledButton(ACanvas, ABounds, '', AState, AScaleFactor);
  R := cxRectContent(ABounds, AlertWindowScaledButtonContentOffsets(AKind, AScaleFactor));
  case AKind of
    awbkClose:
      DrawScaledButtonCross(ACanvas, R, ButtonSymbolColor(AState), AState, AScaleFactor);
    awbkPin:
      DrawPin(ACanvas, R, ButtonSymbolColor(AState), ADown);
    awbkDropdown:
      DrawArrow(ACanvas, R, adDown, ButtonSymbolColor(AState));
    awbkPrevious:
      DrawArrow(ACanvas, R, adLeft, ButtonSymbolColor(AState));
    awbkNext:
      DrawArrow(ACanvas, R, adRight, ButtonSymbolColor(AState));
  end;
end;

procedure TcxCustomLookAndFeelPainter.DrawAlertWindowNavigationPanel(ACanvas: TcxCanvas; const ABounds: TRect);
begin
  ACanvas.FrameRect(ABounds, clBtnShadow, 1, cxBordersAll);
end;

procedure TcxCustomLookAndFeelPainter.DrawPin(ACanvas: TcxCanvas; const ABounds: TRect; AColor: TColor; APinned: Boolean);

  function GetElementSize(AElementIndex: Integer): TSize;
  const
    SizeMap: array[0..2, 0..1] of Single = ((0.5, 0.5), (0.9, 0.1), (0.1, 0.5));
  begin
    Result.cx := 2 * Trunc((SizeMap[AElementIndex, 0] * cxRectWidth(ABounds)) / 2) + 1;
    Result.cy := Round(SizeMap[AElementIndex, 1] * cxRectHeight(ABounds));
  end;

var
  R: TRect;
  S: TSize;
  I: Integer;
begin
  R := Rect(ABounds.Right - 1, 0, 0, ABounds.Top + 1);
  for I := 0 to 2 do
  begin
    S := GetElementSize(I);
    if APinned then
      R := cxRectCenterHorizontally(cxRectSetTop(ABounds, R.Bottom - 1, S.cy), S.cx)
    else
      R := cxRectCenterVertically(cxRectSetRight(ABounds, R.Left + 1, S.cy), S.cx);
    if I = 0 then
    begin
      ACanvas.FrameRect(R, AColor);
      if APinned then
        ACanvas.FillRect(cxRectSetRight(R, R.Right, cxRectWidth(R) div 2), AColor)
      else
        ACanvas.FillRect(cxRectSetBottom(R, R.Bottom, cxRectHeight(R) div 2), AColor);
    end
    else
      ACanvas.FillRect(R, AColor);
  end;
end;

procedure TcxCustomLookAndFeelPainter.DrawScaledExpandMark(ACanvas: TcxCanvas;
  const R: TRect; AColor: TColor; AExpanded: Boolean; AScaleFactor: TdxScaleFactor);
var
  ASize, X, MainY, I: Integer;

  procedure DrawOneMark(Y: Integer);
  var
    ASign, ADelta: Integer;
  begin
    if AExpanded then Inc(Y, ASize);
    ASign := 2 * Ord(AExpanded) - 1;
    ADelta := Ord(Odd(I - MainY));
    if not AExpanded then
      ADelta := Ord(not Boolean(ADelta));
    ACanvas.MoveTo(X + ADelta, Y - ASign * ADelta);
    ACanvas.LineTo(X + ASize, Y - ASign * ASize);
    ACanvas.LineTo(X + 2 * ASize + 1 - ADelta, Y + ASign * (1 - ADelta));
  end;

begin
  ASize := AScaleFactor.Apply(3);
  ACanvas.Pen.Color := AColor;
  with R do
  begin
    X := (Left + Right - (2 * ASize + 1)) div 2;
    MainY := (Top + Bottom - 2 * (ASize + 1)) div 2;
    for I := MainY to MainY + 4 - 1 do
      DrawOneMark(I + Ord(I >= MainY + 2) * (ASize - 1));
  end;
end;

procedure TcxCustomLookAndFeelPainter.DrawExpandMark(ACanvas: TcxCanvas; const R: TRect; AColor: TColor; AExpanded: Boolean);
begin
  DrawScaledExpandMark(ACanvas, R, AColor, AExpanded, dxSystemScaleFactor);
end;

procedure TcxCustomLookAndFeelPainter.DrawRangeControlThumb(ACanvas: TcxCanvas;
  const ARect: TRect; AColor: TColor; ABorderColor: TdxAlphaColor; AScaleFactor: TdxScaleFactor);
const
  ThumbBackColor = $FFFFFF;
var
  R: TRect;
  APoint1: TPoint;
  APoint2: TPoint;
begin
  if AColor = clDefault then
    AColor := ThumbBackColor;
  dxGPPaintCanvas.BeginPaint(ACanvas.Handle, ARect);
  try
    dxGPPaintCanvas.Rectangle(ARect, ABorderColor, dxColorToAlphaColor(AColor));
    R := cxRectCenter(ARect, Size(AScaleFactor.Apply(2) * 2 + 1, cxRectHeight(ARect) - AScaleFactor.Apply(8)));

    APoint1 := R.TopLeft;
    APoint2 := Point(R.Left, R.Bottom - 1);
    dxGPPaintCanvas.Line(APoint1.X, APoint1.Y, APoint2.X, APoint2.Y, ABorderColor, AScaleFactor.ApplyF(1));

    APoint1 := cxPointOffset(APoint1, AScaleFactor.Apply(2), 0);
    APoint2 := cxPointOffset(APoint2, AScaleFactor.Apply(2), 0);
    dxGPPaintCanvas.Line(APoint1.X, APoint1.Y, APoint2.X, APoint2.Y, ABorderColor, AScaleFactor.ApplyF(1));

    APoint1 := cxPointOffset(APoint1, AScaleFactor.Apply(2), 0);
    APoint2 := cxPointOffset(APoint2, AScaleFactor.Apply(2), 0);
    dxGPPaintCanvas.Line(APoint1.X, APoint1.Y, APoint2.X, APoint2.Y, ABorderColor, AScaleFactor.ApplyF(1));
  finally
    dxGPPaintCanvas.EndPaint;
  end;
end;


procedure CreateStdScrollBarBitmaps;
begin
  StdScrollBitmaps[False] := TBitmap.Create;
  with StdScrollBitmaps[False] do
  begin
    Width := 8;
    Height := 8;
  end;
  StdScrollBitmaps[True] := TBitmap.Create;
  with StdScrollBitmaps[True] do
  begin
    Width := 8;
    Height := 8;
  end;
  UpdateScrollBarBitmaps;
end;

procedure UpdateScrollBarBitmaps;
var
  X, Y: Integer;
begin
  if StdScrollBitmaps[False] = nil then
    CreateStdScrollBarBitmaps;
  for X := 0 to 7 do
    for Y := 0 to 7 do
      if (Y mod 2) = (X mod 2) then
      begin
        StdScrollBitmaps[False].Canvas.Pixels[X, Y] := clBtnFace;
        StdScrollBitmaps[True].Canvas.Pixels[X, Y] := clBlack;
      end
      else
      begin
        StdScrollBitmaps[False].Canvas.Pixels[X, Y] := clBtnHighlight;
        StdScrollBitmaps[True].Canvas.Pixels[X, Y] := cl3DDkShadow;
      end;
end;


{ TSystemPaletteChangedNotifier }

procedure TSystemPaletteChangedNotifier.DoChanged;
begin
  TcxRadioButtonImageListManager.Reset;
end;

procedure dxRotateSizeGrip(ACanvas: TcxCanvas; const ARect: TRect;
  ABackgroundColor: TColor; ACorner: TdxCorner; AOnDrawSizeGrip: TdxDrawEvent);
const
  ARotationMap: array[Boolean] of TcxRotationAngle = (ra0, ra180);
var
  ABitmap: TcxBitmap;
begin
  ABitmap := TcxBitmap.CreateSize(ARect);
  try
    ABitmap.cxCanvas.Brush := ACanvas.Brush;
    if ABackgroundColor = clNone then
      cxBitBlt(ABitmap.Canvas.Handle, ACanvas.Handle, ABitmap.ClientRect, ARect.TopLeft, SRCCOPY)
    else
      ABitmap.cxCanvas.FillRect(ABitmap.ClientRect, ABackgroundColor);
    ABitmap.Rotate(ARotationMap[(ACorner in [coTopLeft, coBottomLeft])], (ACorner in [coTopRight, coBottomLeft]));
    if Assigned(AOnDrawSizeGrip) then
      AOnDrawSizeGrip(ABitmap.cxCanvas, ABitmap.ClientRect);
    ABitmap.Rotate(ARotationMap[ACorner in [coTopLeft, coBottomLeft]], ACorner in [coTopRight, coBottomLeft]);
    cxDrawBitmap(ACanvas.Handle, ABitmap, ARect, cxNullPoint);
  finally
    ABitmap.Free;
  end;
end;

procedure dxRotateSizeGrip(ACanvas: TcxCanvas; const ARect: TRect; AScaleFactor: TdxScaleFactor;
  ABackgroundColor: TColor; ACorner: TdxCorner; AOnDrawSizeGrip: TdxDrawScaledRectEvent);
const
  ARotationMap: array[Boolean] of TcxRotationAngle = (ra0, ra180);
var
  ABitmap: TcxBitmap;
begin
  ABitmap := TcxBitmap.CreateSize(ARect);
  try
    ABitmap.cxCanvas.Brush := ACanvas.Brush;
    if ABackgroundColor = clNone then
      cxBitBlt(ABitmap.Canvas.Handle, ACanvas.Handle, ABitmap.ClientRect, ARect.TopLeft, SRCCOPY)
    else
      ABitmap.cxCanvas.FillRect(ABitmap.ClientRect, ABackgroundColor);
    ABitmap.Rotate(ARotationMap[(ACorner in [coTopLeft, coBottomLeft])], (ACorner in [coTopRight, coBottomLeft]));
    if Assigned(AOnDrawSizeGrip) then
      AOnDrawSizeGrip(ABitmap.cxCanvas, ABitmap.ClientRect, AScaleFactor);
    ABitmap.Rotate(ARotationMap[ACorner in [coTopLeft, coBottomLeft]], ACorner in [coTopRight, coBottomLeft]);
    cxDrawBitmap(ACanvas.Handle, ABitmap, ARect, cxNullPoint);
  finally
    ABitmap.Free;
  end;
end;


procedure dxAdjustToTouchableSize(var AElementSizeDimension: Integer; AScaleFactor: TdxScaleFactor = nil);
begin
  if AScaleFactor = nil then
    AScaleFactor := dxSystemScaleFactor;
  if cxIsTouchModeEnabled and (AElementSizeDimension > 0) then
    AElementSizeDimension := Max(AElementSizeDimension, AScaleFactor.Apply(cxTouchElementMinSize));
end;

procedure dxAdjustToTouchableSize(var AElementSize: TSize; AScaleFactor: TdxScaleFactor = nil);
begin
  dxAdjustToTouchableSize(AElementSize.cx, AScaleFactor);
  dxAdjustToTouchableSize(AElementSize.cy, AScaleFactor);
end;

function dxElementSizeFitsForTouch(AElementSizeDimension: Integer): Boolean;
begin
  Result := dxElementSizeFitsForTouch(AElementSizeDimension, dxSystemScaleFactor)
end;

function dxElementSizeFitsForTouch(AElementSizeDimension: Integer; AScaleFactor: TdxScaleFactor): Boolean;
begin
  Result := AElementSizeDimension >= AScaleFactor.Apply(cxTouchElementMinSize);
end;

function cxTextRect(const R: TRect): TRect;
begin
  Result := R;
  InflateRect(Result, -cxTextOffset, -cxTextOffset);
end;

function cxGetSchedulerGroupPolygon(const R: TRect): TPoints;
var
  ARect: TRect;
  ADelta: Integer;
begin
  SetLength(Result, 6);
  ARect := cxRectInflate(R, 0, -cxTextOffset);
  Dec(ARect.Right);
  Result[0] := ARect.TopLeft;
  Result[1] := cxPoint(ARect.Right, ARect.Top);
  Result[2] := ARect.BottomRight;
  ADelta := cxRectHeight(ARect) div 2;
  Result[3] := cxPoint(ARect.Right - ADelta, ARect.Bottom - ADelta);
  Result[4] := cxPoint(ARect.Left + ADelta, ARect.Bottom - ADelta);
  Result[5] := cxPoint(ARect.Left, ARect.Bottom);
end;

function cxGetSchedulerMilestonePolygon(const R: TRect): TPoints;
var
  ARect: TRect;
  ASize: Integer;
  AMin: Integer;
begin
  SetLength(Result, 4);
  AMin := Min(cxRectWidth(R), cxRectHeight(R));
  ARect := cxRectCenter(R, AMin, AMin);
  ASize := cxRectWidth(ARect);
  ASize := ASize - Integer(Odd(ASize));

  Result[0].X := ARect.Left;
  Result[0].Y := ARect.Top + ASize div 2;

  Result[1].X := ARect.Left + ASize div 2;
  Result[1].Y := ARect.Top;

  Result[2].X := ARect.Left + ASize;
  Result[2].Y := Result[0].Y;

  Result[3].X := Result[1].X;
  Result[3].Y := ARect.Top + ASize;
end;

const
//  FilledRadioButtonListSize = 48;
  cxPixelColorMask = $FFFFFF;

procedure InternalRoundRect(ACanvas: TCanvas; const R: TRect);
begin
  ACanvas.RoundRect(R.Left, R.Top, R.Right, R.Bottom, cxRectWidth(R) - 1, cxRectHeight(R) - 1);
end;

constructor TcxRadioButtonImageList.Create(AScaleFactor: TdxScaleFactor);
begin
  inherited Create;
  FScaleFactor := TdxScaleFactor.Create;
  FScaleFactor.Assign(AScaleFactor);
  FList := TImageList.Create(nil);
  Initialize;
end;

destructor TcxRadioButtonImageList.Destroy;
begin
  FreeAndNil(FList);
  FreeAndNil(FButtonMask);
  FreeAndNil(FScaleFactor);
  inherited Destroy;
end;

procedure TcxRadioButtonImageList.Draw(ACanvas: TcxCanvas; X, Y: Integer; ABrushColor: TColor;
  ALookAndFeelKind: TcxLookAndFeelKind; AButtonState: TcxButtonState; AChecked, AFocused, AIsDesigning: Boolean);

  procedure DrawGlyph(ACanvas: TcxCanvas; AImageList: TCustomImageList; AImageIndex: TcxImageIndex;
    const AGlyphRect: TRect; ABrushColor: TColor; AEnabled: Boolean);
  var
    ABitmap: TcxBitmap;
  begin
    ABitmap := TcxBitmap.CreateSize(AImageList.Width, AImageList.Height);
    try
      ABitmap.cxCanvas.FillRect(ABitmap.ClientRect, ABrushColor);
      AImageList.Draw(ABitmap.Canvas, 0, 0, AImageIndex, AEnabled);
      cxDrawImage(ACanvas.Handle, AGlyphRect, AGlyphRect, ABitmap, nil, -1, idmNormal, True, 0, clDefault, False)
    finally
      ABitmap.Free;
    end;
  end;

var
  ARect: TRect;
  AImageIndex: Integer;
begin
  AImageIndex := GetImageIndex(ALookAndFeelKind, AButtonState, AChecked, AFocused, AIsDesigning);
  ARect := cxRectBounds(X, Y, GetSize);
  if ABrushColor = clDefault then
    cxDrawImage(ACanvas.Handle, ARect, ARect, nil, FList, AImageIndex, idmNormal, True, 0, clDefault, False)
  else
    DrawGlyph(ACanvas, FList, AImageIndex, ARect, ABrushColor, True);
end;

function TcxRadioButtonImageList.GetSize: TSize;
begin
  Result := Size(FList.Width, FList.Height);
end;

procedure TcxRadioButtonImageList.Prepare;
var
  AColorMap: array[0..6] of TColor;

  function GetRadioButtonBodyColor(ALookAndFeelKind: TcxLookAndFeelKind; AButtonState: TcxButtonState): TColor;
  var
    APainter: TcxCustomLookAndFeelPainter;
  begin
    if cxLookAndFeelPaintersManager.GetPainter(cxLookAndFeelStyleMap[ALookAndFeelKind], APainter) then
      Result := APainter.RadioButtonBodyColor(AButtonState)
    else
      Result := clWindow;
  end;

  procedure PrepareColorMap(ALookAndFeelKind: TcxLookAndFeelKind;
    AButtonState: TcxButtonState; AChecked, AFocused, AIsDesigning: Boolean);
  begin
    AColorMap[0] := clBlack;
    AColorMap[5] := GetRadioButtonBodyColor(ALookAndFeelKind, AButtonState);
    case ALookAndFeelKind of
      lfStandard:
        begin
          AColorMap[1] := clBtnShadow;
          AColorMap[2] := clBtnHighlight;
          AColorMap[3] := cl3DDkShadow;
          AColorMap[4] := cl3DLight;
        end;
      lfFlat:
        begin
          AColorMap[1] := clBtnShadow;
          AColorMap[2] := clBtnHighlight;
          AColorMap[3] := cl3DLight;
          AColorMap[4] := cl3DLight;
        end;
      lfUltraFlat:
        begin
          if AFocused or (AButtonState in [cxbsHot, cxbsPressed]) or (AIsDesigning and
              (AButtonState <> cxbsDisabled)) then
            AColorMap[1] := clHighlight
          else
            AColorMap[1] := clBtnShadow;
          AColorMap[2] := AColorMap[1];
          AColorMap[3] := AColorMap[5];
          AColorMap[4] := AColorMap[5];
        end;
      lfOffice11: // TODO: to method
        begin
          if AButtonState = cxbsDisabled then
            AColorMap[1] := dxOffice11TextDisabledColor
          else
            if AFocused or (AButtonState in [cxbsHot, cxbsPressed]) or AIsDesigning then
              AColorMap[1] := dxOffice11SelectedBorderColor
            else
              AColorMap[1] := clBtnText;//dxOffice11BarFloatingBorderColor1; //clBtnShadow;
          AColorMap[2] := AColorMap[1];
          AColorMap[3] := AColorMap[5];
          AColorMap[4] := AColorMap[5];
        end;
    end;

    if not AChecked then
      AColorMap[6] := AColorMap[5]
    else
      if AButtonState <> cxbsDisabled then
        AColorMap[6] := clWindowText
      else
        if ALookAndFeelKind = lfOffice11 then
          AColorMap[6] := dxOffice11TextDisabledColor
        else
          AColorMap[6] :=  clBtnShadow;
  end;

var
  ABitmap: TcxBitmap32;
  AButtonState: TcxButtonState;
  AChecked, AFocused, AIsDesigning: Boolean;
  AImageListIndexMapIndex, I, J: Integer;
  ALookAndFeelKind: TcxLookAndFeelKind;
  AColors: TRGBColors;
begin
  if FList.Count > 0 then exit;
  ABitmap := TcxBitmap32.CreateSize(FRadioButtonSize.cx, FRadioButtonSize.cy);
  try
    SetLength(AColors, ABitmap.Width * ABitmap.Height);

    for AFocused := False to True do
      for AIsDesigning := False to True do
        for ALookAndFeelKind := Low(TcxLookAndFeelKind) to High(TcxLookAndFeelKind) do
          for AChecked := False to True do
            for AButtonState := Succ(Low(TcxButtonState)) to High(TcxButtonState) do
            begin
              PrepareColorMap(ALookAndFeelKind, AButtonState, AChecked, AFocused, AIsDesigning);

              for I := 0 to FRadioButtonSize.cy - 1 do
                for J := 0 to FRadioButtonSize.cx - 1 do
                  AColors[I * ABitmap.Width + J] := dxColorToRGBQuad(AColorMap[FRadioButtonPattern[I, J]]);

              SetBitmapBits(ABitmap, AColors, True);
              FList.Add(ABitmap, FButtonMask);
              AImageListIndexMapIndex := GetImageListIndexMapIndex(
                ALookAndFeelKind, AButtonState, AChecked, AFocused, AIsDesigning);
              if AImageListIndexMapIndex + 1 > Length(FRadioButtonImageListIndexes) then
                SetLength(FRadioButtonImageListIndexes, AImageListIndexMapIndex + 1);
              FRadioButtonImageListIndexes[AImageListIndexMapIndex] := FList.Count - 1;
            end;
  finally
    ABitmap.Free;
  end;
end;

procedure TcxRadioButtonImageList.Reset;
begin
  FList.Clear;
  Prepare;
end;

procedure TcxRadioButtonImageList.CalculateRadioButtonSize;
var
  B: Windows.TBitmap;
  HB: HBITMAP;
  ABitmap: TBitmap;
  I, J: Integer;
  FRadioButtonMaskSize: TSize;
begin
  HB := LoadBitmap(0, PChar(OBM_CHECKBOXES));
  try
    dxGetBitmapData(HB, B);
  finally
    DeleteObject(HB);
  end;
  FRadioButtonMaskSize.cx := ScaleFactor.Apply(B.bmWidth div 4, dxSystemScaleFactor);
  FRadioButtonMaskSize.cy := ScaleFactor.Apply(B.bmHeight div 3, dxSystemScaleFactor);
  ABitmap := cxCreateBitmap(FRadioButtonMaskSize);
  try
    ABitmap.Canvas.Brush.Color := 0;
    ABitmap.Canvas.FillRect(Rect(0, 0, FRadioButtonMaskSize.cx, FRadioButtonMaskSize.cy));
    DrawFrameControl(ABitmap.Canvas.Handle, Rect(0, 0, FRadioButtonMaskSize.cx,
      FRadioButtonMaskSize.cy), DFC_BUTTON, DFCS_BUTTONRADIOMASK + DFCS_FLAT);
    FRadioButtonSize.cX := 0;
    FRadioButtonSize.cY := 0;
    FRadioButtonRect.Left := -1;
    FRadioButtonRect.Top := -1;
    for J := 0 to FRadioButtonMaskSize.cx - 1 do
      for I := 0 to FRadioButtonMaskSize.cy - 1 do
        if ABitmap.Canvas.Pixels[J, I] = 0 then
        begin
          if FRadioButtonRect.Left = -1 then
            FRadioButtonRect.Left := J;
          Inc(FRadioButtonSize.cX);
          Break;
        end;
    for I := 0 to FRadioButtonMaskSize.cy - 1 do
      for J := 0 to FRadioButtonMaskSize.cx - 1 do
        if ABitmap.Canvas.Pixels[J, I] = 0 then
        begin
          if FRadioButtonRect.Top = -1 then
            FRadioButtonRect.Top := I;
          Inc(FRadioButtonSize.cY);
          Break;
        end;
    FRadioButtonRect.Right := FRadioButtonRect.Left + FRadioButtonSize.cx;
    FRadioButtonRect.Bottom := FRadioButtonRect.Top + FRadioButtonSize.cy;
    ABitmap.Canvas.Brush.Color := 0;
    ABitmap.Canvas.FillRect(Rect(0, 0, FRadioButtonMaskSize.cx, FRadioButtonMaskSize.cy));
    DrawFrameControl(ABitmap.Canvas.Handle, Rect(0, 0, FRadioButtonMaskSize.cx,
      FRadioButtonMaskSize.cy), DFC_BUTTON, DFCS_BUTTONRADIOIMAGE + DFCS_FLAT + DFCS_CHECKED);
    I := FRadioButtonRect.Top + (FRadioButtonSize.cy div 2) - 2;
    J := FRadioButtonRect.Left + (FRadioButtonSize.cx div 2) - 1;
    while ABitmap.Canvas.Pixels[J, I] = ColorToRGB(clWindowText) do
      Dec(I);
    Inc(I);
    FRadioButtonCheckRect.Top := I;
    repeat
      Inc(I);
    until ABitmap.Canvas.Pixels[J, I] <> ColorToRGB(clWindowText);
    FRadioButtonCheckRect.Bottom := I;

    I := FRadioButtonRect.Top + (FRadioButtonSize.cy div 2) - 1;
    J := FRadioButtonRect.Left + (FRadioButtonSize.cx div 2) - 2;
    while ABitmap.Canvas.Pixels[J, I] = ColorToRGB(clWindowText) do
      Dec(J);
    Inc(J);
    FRadioButtonCheckRect.Left := J;
    repeat
      Inc(J);
    until ABitmap.Canvas.Pixels[J, I] <> ColorToRGB(clWindowText);
    FRadioButtonCheckRect.Right := J;
  finally
    ABitmap.Free;
  end;
end;

function TcxRadioButtonImageList.GetImageIndex(ALookAndFeelKind: TcxLookAndFeelKind;
  AButtonState: TcxButtonState; AChecked, AFocused, AIsDesigning: Boolean): Integer;
begin
  Result := FRadioButtonImageListIndexes[GetImageListIndexMapIndex(
    ALookAndFeelKind, AButtonState, AChecked, AFocused, AIsDesigning)];
end;

function TcxRadioButtonImageList.GetImageListIndexMapIndex(ALookAndFeelKind: TcxLookAndFeelKind;
  AButtonState: TcxButtonState; AChecked, AFocused, AIsDesigning: Boolean): Integer;
var
  AButtonStateIndex: Integer;
  ALookAndFeelKindCount: Integer;
begin
  AButtonStateIndex := Integer(AButtonState) - 1;
  ALookAndFeelKindCount := Integer(High(TcxLookAndFeelKind)) - Integer(Low(TcxLookAndFeelKind)) + 1;

  Result := (Integer(AFocused) * 2 * ALookAndFeelKindCount + Integer(AIsDesigning) * ALookAndFeelKindCount +
    Integer(ALookAndFeelKind)) * 8 + Integer(AChecked) * 4 + AButtonStateIndex;
end;

procedure TcxRadioButtonImageList.Initialize;
begin
  CalculateRadioButtonSize;
  FList.Width := FRadioButtonSize.cx;
  FList.Height := FRadioButtonSize.cy;
  FList.Masked := True;
  FList.ImageType := itImage;

  PrepareButtonMask;
  PrepareRadioButtonPattern;
  Prepare;
end;

procedure TcxRadioButtonImageList.PrepareButtonMask;
var
  R: TRect;
begin
  if FButtonMask = nil then
    FButtonMask := TBitmap.Create;

  FButtonMask.Monochrome := True;
  FButtonMask.Width := FRadioButtonSize.cx;
  FButtonMask.Height := FRadioButtonSize.cy;

  with FButtonMask.Canvas do
  begin
    Brush.Color := clWhite;
    R := Rect(0, 0, FRadioButtonSize.cx, FRadioButtonSize.cy);
    FillRect(R);
    Brush.Color := clBlack;
    Pen.Color := clBlack;
    InternalRoundRect(FButtonMask.Canvas, R);
  end;
end;

procedure TcxRadioButtonImageList.PrepareRadioButtonPattern;

  procedure PrepareOuterCircle;
  var
    ABitmap: TBitmap;
    I, J: Integer;
  begin
    ABitmap := cxCreateBitmap(FRadioButtonSize, pf32bit);
    try
      ABitmap.Canvas.Brush.Color := clWhite;
      ABitmap.Canvas.FillRect(cxRect(cxNullPoint, cxPoint(FRadioButtonSize)));
      ABitmap.Canvas.Pen.Color := clBlack;
      InternalRoundRect(ABitmap.Canvas, cxRect(cxNullPoint, cxPoint(FRadioButtonSize)));
      SetLength(FRadioButtonPattern, FRadioButtonSize.cy, FRadioButtonSize.cx);
      for I := 0 to FRadioButtonSize.cy - 1 do
      begin
        for J := 0 to FRadioButtonSize.cx - 1 do
        begin
          if ABitmap.Canvas.Pixels[J, I] and cxPixelColorMask <> 0 then
            FRadioButtonPattern[I, J] := 0
          else
            if (FRadioButtonSize.cy - 1) * (FRadioButtonSize.cx - 1 - J) < I * (FRadioButtonSize.cx - 1) then
              FRadioButtonPattern[I, J] := 2
            else
              FRadioButtonPattern[I, J] := 1;
        end;
      end;
    finally
      ABitmap.Free;
    end;
  end;

  procedure PrepareInnerCircle;

    procedure FillPoint(I, J: Integer);
    var
      ASign: Integer;
    begin
      ASign := (FRadioButtonSize.cy - 1) * (FRadioButtonSize.cx - 1 - J) - I * (FRadioButtonSize.cx - 1);
      if ASign = 0 then
        if J <= FRadioButtonSize.cx div 2 - 1 then
          FRadioButtonPattern[I, J] := 3
        else
          FRadioButtonPattern[I, J] := 4
      else
        if ASign < 0 then
          FRadioButtonPattern[I, J] := 4
        else
          FRadioButtonPattern[I, J] := 3;
    end;

  var
    I, I1, J, J1: Integer;
    AFirstColumn, ALastColumn, AFirstRow, ALastRow: Integer;
  begin
    AFirstRow := 1;
    ALastRow := FRadioButtonSize.cy - 2;
    J1 := FRadioButtonSize.cx div 2 - 1;

    for I := AFirstRow to ALastRow do
    begin
      J := J1;
      while FRadioButtonPattern[I, J] = 0 do
      begin
        FRadioButtonPattern[I, J] := 5;
        Dec(J);
      end;
      J := J1 + 1;
      while FRadioButtonPattern[I, J] = 0 do
      begin
        FRadioButtonPattern[I, J] := 5;
        Inc(J);
      end;
    end;

    for I := AFirstRow to ALastRow do
    begin
      J := J1;
      while not(FRadioButtonPattern[I, J] in [1, 2]) do
      begin
        if (I = AFirstRow) or (I = ALastRow) then
          FillPoint(I, J);
        Dec(J);
      end;
      Inc(J);
      FillPoint(I, J);
      J := J1 + 1;
      while not(FRadioButtonPattern[I, J] in [1, 2]) do
      begin
        if (I = AFirstRow) or (I = ALastRow) then
          FillPoint(I, J);
        Inc(J);
      end;
      Dec(J);
      FillPoint(I, J);
    end;

    AFirstColumn := 1;
    ALastColumn := FRadioButtonSize.cx - 2;
    I1 := FRadioButtonSize.cy div 2 - 1;
    for J := AFirstColumn to ALastColumn do
    begin
      I := I1;
      while not(FRadioButtonPattern[I, J] in [1, 2]) do
      begin
        if (J = AFirstColumn) or (J = ALastColumn) then
          FillPoint(I, J);
        Dec(I);
      end;
      Inc(I);
      FillPoint(I, J);
      I := I1 + 1;
      while not(FRadioButtonPattern[I, J] in [1, 2]) do
      begin
        if (J = AFirstColumn) or (J = ALastColumn) then
          FillPoint(I, J);
        Inc(I);
      end;
      Dec(I);
      FillPoint(I, J);
    end;
  end;

  procedure PrepareCheck;
  var
    ABitmap: TBitmap;
    I, J: Integer;
    R: TRect;
  begin
    ABitmap := cxCreateBitmap(FRadioButtonSize, pf32Bit);
    try
      ABitmap.Canvas.Brush.Color := clWhite;
      ABitmap.Canvas.FillRect(cxRect(cxNullPoint, cxPoint(FRadioButtonSize)));
      ABitmap.Canvas.Pen.Color := clBlack;
      ABitmap.Canvas.Brush.Color := clBlack;
      R := cxRectOffset(FRadioButtonCheckRect, cxPointInvert(FRadioButtonRect.TopLeft));
      InternalRoundRect(ABitmap.Canvas, R);
      for I := 0 to FRadioButtonSize.cy - 1 do
      begin
        for J := 0 to FRadioButtonSize.cx - 1 do
        begin
          if ABitmap.Canvas.Pixels[J, I] and cxPixelColorMask = 0 then
            FRadioButtonPattern[I, J] := 6;
        end;
      end;
    finally
      ABitmap.Free;
    end;
  end;

begin
  PrepareOuterCircle;
  PrepareInnerCircle;
  PrepareCheck;
end;

procedure TcxRadioButtonImageList.Reinitialize;
begin
  FList.Clear;
  Initialize;
end;

procedure TcxRadioButtonImageList.SetScaleFactor(AScaleFactor: TdxScaleFactor);
begin
  if (ScaleFactor.Denominator <> AScaleFactor.Denominator) or (ScaleFactor.Numerator <> AScaleFactor.Numerator) then
  begin
    FScaleFactor.Assign(AScaleFactor);
    Reinitialize;
  end;
end;

procedure DestroyStdScrollBarBitmaps;
begin
  FreeAndNil(StdScrollBitmaps[False]);
  FreeAndNil(StdScrollBitmaps[True]);
end;


procedure RegisterStandardLookAndFeelPainters;
begin
  cxLookAndFeelPaintersManager.Register(TcxFlatLookAndFeelPainter.Create);
  cxLookAndFeelPaintersManager.Register(TcxOffice11LookAndFeelPainter.Create);
  cxLookAndFeelPaintersManager.Register(TcxStandardLookAndFeelPainter.Create);
  cxLookAndFeelPaintersManager.Register(TcxUltraFlatLookAndFeelPainter.Create);
  cxLookAndFeelPaintersManager.Register(TcxWinXPLookAndFeelPainter.Create);
end;

procedure RegisterAssistants;
begin
  CreateStdScrollBarBitmaps;
  CalculateCheckButtonSize;
  RegisterStandardLookAndFeelPainters;
  if IsLibrary then
    RefreshOffice11Colors;
end;

procedure UnregisterAssistants;
begin
  cxLookAndFeelPaintersManager.ReleaseImageResources;
  TcxRadioButtonImageListManager.Finalize;
  DestroyStdScrollBarBitmaps;
  FreeAndNil(FIndicatorImages);
  FreeAndNil(FDataRowFixingImages);
end;

initialization
  cxLookAndFeelPaintersManager := TcxLookAndFeelPaintersManager.Create;
  FSystemPaletteChangedNotifier := TSystemPaletteChangedNotifier.Create(True);
  dxUnitsLoader.AddUnit(@RegisterAssistants, @UnregisterAssistants);

finalization
  dxUnitsLoader.RemoveUnit(@UnregisterAssistants);
  FreeAndNil(FSystemPaletteChangedNotifier);
  FreeAndNil(cxLookAndFeelPaintersManager);

end.
