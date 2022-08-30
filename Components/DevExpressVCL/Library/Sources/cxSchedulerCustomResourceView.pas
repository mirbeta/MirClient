{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressScheduler                                         }
{                                                                    }
{           Copyright (c) 2003-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSSCHEDULER AND ALL ACCOMPANYING }
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

unit cxSchedulerCustomResourceView;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Windows, Graphics, SysUtils, StdCtrls, Classes, Forms, Controls, Contnrs,
  DateUtils, Variants, Math, ImgList, Types,
  dxCoreClasses, cxClasses, cxGraphics,
  cxGeometry, cxControls, cxStyles, cxEdit, cxContainer, dxGDIPlusClasses,
  cxSchedulerStorage, cxSchedulerUtils, cxSchedulerCustomControls, cxLookAndFeelPainters,
  cxDateUtils, cxEditUtils, cxSchedulerStrs, cxLookAndFeels;

const
  // default values
  cxTimeLineWidth: Integer = 7;
  cxEventImagesOffset = 1;
  cxEventBorderWidth = 1;

  cxHitDelta: Integer = 4;

  // HitTest constants
  htcButton           = $3;
  htcContainer        = $4;
  htcTimeRuler        = $5;
  htcTimeZoneLabel    = $6;
  htcContent          = $7;
  htcDayHeader        = $8;
  htcGroupSeparator   = $9;
  htcResourceHeader   = $10;
  htcNavigationButton = $16;

type
  TcxSchedulerCustomViewInfoItem = class;
  TcxSchedulerCustomResourceViewViewInfo = class;
  TcxSchedulerCustomResourceViewHitTest = class;
  TcxSchedulerBackgroundCellViewInfo = class;
  TcxSchedulerContentCellViewInfo = class;
  TcxSchedulerDayHeaderCellViewInfo = class;
  TcxSchedulerEventCellViewInfo = class;
  TcxSchedulerExternalPainter = class;
  TcxSchedulerGroupSeparatorCellViewInfo = class;
  TcxSchedulerHeaderCellViewInfo = class;
  TcxSchedulerMoreEventsButtonViewInfo = class;
  TcxSchedulerResourceViewInfo = class;
  TcxCustomResourceViewAdapter = class;
  TcxSchedulerEventLayoutBuilder = class;
  TcxSchedulerEventImages = class;
  TcxSchedulerContentNavigationButtonViewInfo = class;
  TcxSchedulerCachedImage = class;

  TcxSchedulerContentNavigationButtonKind = (nbkPrevious, nbkNext);
  TcxSchedulerColumnPositionInResource = (cprIndefinite, cprFirst, cprLast, cprSingle);
  TcxSchedulerDayHeaderModernStyleDisplayMode = (hdmDefault, hdmClassic, hdmDayAndDate);

  // custom draw event types definition

  TcxSchedulerCustomDrawBackgroundEvent = procedure(Sender: TObject; ACanvas: TcxCanvas;
    AViewInfo: TcxSchedulerBackgroundCellViewInfo; var ADone: Boolean) of object;

  TcxSchedulerCustomDrawButtonEvent = procedure(Sender: TObject; ACanvas: TcxCanvas;
    AViewInfo: TcxSchedulerMoreEventsButtonViewInfo; var ADone: Boolean) of object;

  TcxSchedulerCustomDrawDayHeaderEvent = procedure(Sender: TObject; ACanvas: TcxCanvas;
    AViewInfo: TcxSchedulerDayHeaderCellViewInfo; var ADone: Boolean) of object;

  TcxSchedulerCustomDrawNavigationButtonEvent = procedure(Sender: TObject; ACanvas: TcxCanvas;
    AViewInfo: TcxSchedulerContentNavigationButtonViewInfo; var ADone: Boolean) of object;

  TcxSchedulerCustomDrawResourceHeaderEvent = procedure(Sender: TObject; ACanvas: TcxCanvas;
    AViewInfo: TcxSchedulerHeaderCellViewInfo; var ADone: Boolean) of object;

  TcxSchedulerCustomDrawContentEvent = procedure(Sender: TObject; ACanvas: TcxCanvas;
    AViewInfo: TcxSchedulerContentCellViewInfo; var ADone: Boolean) of object;

  TcxSchedulerCustomDrawEventEvent = procedure(Sender: TObject; ACanvas: TcxCanvas;
    AViewInfo: TcxSchedulerEventCellViewInfo; var ADone: Boolean) of object;

  TcxSchedulerCustomDrawGroupSeparatorEvent = procedure(Sender: TObject; ACanvas: TcxCanvas;
    AViewInfo: TcxSchedulerGroupSeparatorCellViewInfo; var ADone: Boolean) of object;

  { IcxSchedulerCommonViewItemsPainter }

  IcxSchedulerCommonViewItemsPainter = interface
  ['{0702AB17-C2F1-479D-B809-C3B972F8A334}']
    procedure DoCustomDrawBackground(
      AViewInfo: TcxSchedulerBackgroundCellViewInfo; var ADone: Boolean);
    procedure DoCustomDrawButton(
      AViewInfo: TcxSchedulerMoreEventsButtonViewInfo; var ADone: Boolean);
    procedure DoCustomDrawContent(
      AViewInfo: TcxSchedulerContentCellViewInfo; var ADone: Boolean);
    procedure DoCustomDrawDayHeader(
      AViewInfo: TcxSchedulerDayHeaderCellViewInfo; var ADone: Boolean);
    procedure DoCustomDrawEvent(
      AViewInfo: TcxSchedulerEventCellViewInfo; var ADone: Boolean);
    procedure DoCustomDrawGroupSeparator(
      AViewInfo: TcxSchedulerGroupSeparatorCellViewInfo; var ADone: Boolean);
    procedure DoCustomDrawNavigationButton(
      AViewInfo: TcxSchedulerContentNavigationButtonViewInfo; var ADone: Boolean);
    procedure DoCustomDrawResourceHeader(
      AViewInfo: TcxSchedulerHeaderCellViewInfo; var ADone: Boolean);
    //cached paint
    function HasCustomDrawGroupSeparator: Boolean;
    function HasCustomDrawResourceHeader: Boolean;
  end;

  { IcxSchedulerEventImages }

  IcxSchedulerEventImages = interface
  ['{4C5A8F8B-5356-4D2B-9972-507A7D60954A}']
    procedure DoInitEventImages(AEvent: TcxSchedulerControlEvent;
      AImages: TcxSchedulerEventImages);
    function GetImages: TCustomImageList;
    function SupportEventImages: Boolean;
  end;

  { IcxSchedulerViewTimeScaleStep }

  IcxSchedulerViewTimeScaleStep = interface
  ['{801DD3CE-A6E5-4246-913E-1441EFB25B0A}']
    function GetTimeScaleStep: Integer;
    procedure SetTimeScaleStep(const AValue: Integer);
  end;

  { TcxSchedulerViewInfoCellList }

  TcxSchedulerCustomDrawItemProc = procedure(
    AItem: TcxSchedulerCustomViewInfoItem; var ADone: Boolean) of object;

  TcxSchedulerViewInfoCellList = class(TList)
  private
    function GetItem(AIndex: Integer): TcxSchedulerCustomViewInfoItem;
  protected
    procedure DoRightToLeftConversion(const AClientBounds: TRect); virtual;
  public
    function CalculateHitTest(AHitTest: TcxSchedulerCustomResourceViewHitTest): Boolean;
    procedure Clear; override;
    procedure Draw(ACanvas: TcxCanvas; ADrawItemProc: TcxSchedulerCustomDrawItemProc);

    property Items[Index: Integer]: TcxSchedulerCustomViewInfoItem read GetItem; default;
  end;

  { TcxSchedulerCustomViewInfoItem }

  TcxSchedulerCustomViewInfoItem = class
  private
    FCache: TcxSchedulerCachedImage;
    FUseRightToLeftAlignment: Boolean;
    FScaleFactor: TdxScaleFactor;

    function GetBitmap: TBitmap;
    function GetColor: TColor;
    function GetDateTimeHelper: TcxSchedulerDateTimeHelperClass;
    function GetHeight: Integer;
    function GetPainterHelper: TcxSchedulerPainterHelperClass;
    function GetScaleFactor: TdxScaleFactor;
    function GetTextColor: TColor;
    function GetWidth: Integer;
    procedure SetBitmap(AValue: TBitmap);
    procedure SetColor(AValue: TColor);
    procedure SetTextColor(AValue: TColor);
  protected
    FBackgroundDrawing: Boolean;
    FBorders: TcxBorders;
    FBounds: TRect;
    FCanvas: TcxCanvas;
    FClipRect: TRect;
    FClipRgn: TcxRegion;
    FClipRef: Integer;
    FDateTime: TDateTime;
    FDayBorderColor: TColor;
    FDisplayText: string;
    FExternalPainter: TcxSchedulerExternalPainter;
    FHasClipping: Boolean;
    FPainter: TcxCustomLookAndFeelPainter;
    FSavedClipRgn: TcxRegion;
    FTransparent: Boolean;
    FViewParams: TcxViewParams;
    FVisible: Boolean;
    FVisibleRect: TRect;

    procedure CalculateCellBounds(const ABounds, AVisibleRect: TRect); virtual;
    function CalculateClipRect(const ABounds, AVisibleRect: TRect): TRect; virtual;
    procedure ClippingCreate(AHasClipping: Boolean); virtual;
    procedure ClippingRestore;
    procedure DoDraw; virtual; abstract;
    procedure DoRightToLeftConversion(const AClientBounds: TRect); virtual;
    procedure DoRightToLeftConversionWithChecking(var ARect: TRect; const AClientBounds: TRect);
    function DrawBackground(ACanvas: TcxCanvas; const ABounds: TRect): Boolean; virtual;
    procedure DrawFrame(const ARect: TRect; ABorders: TcxBorders = []; ABorderColor: TColor = clDefault; ABorderSize: Integer = 1);
    procedure DrawRect(const ARect: TRect; ABorders: TcxBorders = [];  ABorderColor: TColor = clDefault; ABorderSize: Integer = 1);
    procedure DrawText(const ARect: TRect; const AText: string;
      AFlags: Integer; AFont: TFont = nil; AColor: TColor = clDefault);
    function GetActiveBounds: TRect; virtual;
    function GetActualRectRTL(const ARect, AClientBounds: TRect; AOffsetIfRTL: Integer = 0): TRect;
    function GetBoundsForHitTest: TRect; virtual;
    function GetFont: TFont; virtual;
    function GetHitTest(AHitTest: TcxSchedulerCustomResourceViewHitTest; var ABreak: Boolean): Boolean; virtual;
    procedure InitHitTest(AHitTest: TcxSchedulerCustomResourceViewHitTest); virtual;
    function IsDrawBySkin: Boolean;
    procedure TuneClipping(var AClipRgn: TcxRegion); virtual;
    // custom draw support
    procedure AfterCustomDraw(ACanvas: TcxCanvas); virtual;
    procedure BeforeCustomDraw(ACanvas: TcxCanvas); virtual;
    procedure UpdateCachedImage(const AViewParams: TcxViewParams); virtual;


    property Cache: TcxSchedulerCachedImage read FCache write FCache;

    // class item settings
    property ActiveBounds: TRect read GetActiveBounds;
    property DateTimeHelper: TcxSchedulerDateTimeHelperClass read GetDateTimeHelper;
    property DisplayText: string read FDisplayText write FDisplayText;
    property PainterHelper: TcxSchedulerPainterHelperClass read GetPainterHelper;
    property ExternalPainter: TcxSchedulerExternalPainter read FExternalPainter;
    property VisibleRect: TRect read FVisibleRect write FVisibleRect;
  public
    constructor Create(APainter: TcxCustomLookAndFeelPainter; const ABounds, AVisibleRect: TRect;
      const AViewParams: TcxViewParams; AScaleFactor: TdxScaleFactor; AUseRightToLeftAlignment: Boolean); virtual;
    destructor Destroy; override;
    procedure Draw(ACanvas: TcxCanvas); virtual;

    property Bitmap: TBitmap read GetBitmap write SetBitmap;
    property Borders: TcxBorders read FBorders write FBorders;
    property Bounds: TRect read FBounds;
    property Canvas: TcxCanvas read FCanvas;
    property ClipRect: TRect read FClipRect;
    property Color: TColor read GetColor write SetColor;
    property Font: TFont read GetFont;
    property Height: Integer read GetHeight;
    property Painter: TcxCustomLookAndFeelPainter read FPainter;
    property ScaleFactor: TdxScaleFactor read GetScaleFactor;
    property TextColor: TColor read GetTextColor write SetTextColor;
    property Transparent: Boolean read FTransparent write FTransparent;
    property UseRightToLeftAlignment: Boolean read FUseRightToLeftAlignment;
    property ViewParams: TcxViewParams read FViewParams;
    property Visible: Boolean read FVisible;
    property Width: Integer read GetWidth;
  end;

  { TcxSchedulerBackgroundCellViewInfo }

  TcxSchedulerBackgroundCellViewInfo = class(TcxSchedulerCustomViewInfoItem)
  protected
    procedure DoDraw; override;
  end;

  TcxSchedulerCustomViewInfoItemClass = class of TcxSchedulerCustomViewInfoItem;

  { TcxSchedulerCustomResourceViewInfoItem }

  TcxSchedulerCustomResourceViewInfoItem = class(TcxSchedulerCustomViewInfoItem)
  private
    FIsResourceAssigned: Boolean;
    FTimeFinish: TDateTime;
    FResource: TcxSchedulerResourceViewInfo;
  protected
    procedure InitHitTest(AHitTest: TcxSchedulerCustomResourceViewHitTest); override;

    property DateTime: TDateTime read FDateTime write FDateTime;
    property IsResourceAssigned: Boolean read FIsResourceAssigned;
    property Resource: TcxSchedulerResourceViewInfo read FResource write FResource;
    property TimeFinish: TDateTime read FTimeFinish;
  end;

  { TcxSchedulerCustomHeaderCellViewInfo }

  TcxSchedulerCustomHeaderCellViewInfo = class(TcxSchedulerCustomResourceViewInfoItem)
  protected
    DisplayBounds: TRect;
    FAlignHorz: TAlignment;
    FAlignVert: TcxAlignmentVert;
    FAutoHeight: Boolean;
    FButtonState: TcxButtonState;
    FDrawRotatedBackground: Boolean;
    FImageIndex: Integer;
    FImagePosition: TcxSchedulerHeaderImagePosition;
    FImages: TCustomImageList;
    FImageRectAssigned: Boolean;
    FImageRect: TRect;
    FIsResourceHeader: Boolean;
    FMultiLine: Boolean;
    FNeighbors: TcxNeighbors;
    FRotateBitmap: TcxBitmap;
    FRotateHeader: Boolean;
    FRotateText: Boolean;
    FSelected: Boolean;
    FSelectionBitmap: TcxBitmap;
    FSelectionColor: TColor;
    FSelectionRect: TRect;
    FSelectionTextColor: TColor;
    FShowEndEllipsis: Boolean;
    FTextRect: TRect;
    procedure CalculateImageLayout; virtual;
    procedure CheckNeighbor(APrevCell: TcxSchedulerCustomHeaderCellViewInfo);
    function CheckSelection: Boolean; virtual;
    procedure CheckTextColorWhenSelected; virtual;
    procedure CreateRotatedBitmap;
    procedure DoDraw; override;
    procedure DoRightToLeftConversion(const AClientBounds: TRect); override;
    procedure DrawCaption(ACanvas: TcxCanvas = nil); virtual;
    procedure DrawHorizontalHeader; virtual;
    function DrawRotateBackground(ACanvas: TcxCanvas; const ABounds: TRect): Boolean; virtual;
    procedure DrawSelection; virtual;
    procedure DrawVerticalHeader; virtual;
    function GetDefaultTextRect: TRect; virtual;
    function GetImagePositionRTL: TcxSchedulerHeaderImagePosition;
    function GetTextOutcxFlags: Integer;
    function GetTextOutFlags: Integer;
    function HasImage: Boolean;
    procedure InitHitTest(AHitTest: TcxSchedulerCustomResourceViewHitTest); override;
    procedure TextOut(ACanvas: TcxCanvas; const AText: string; R: TRect);
    procedure ValidateSelection;

    property AlignHorz: TAlignment read FAlignHorz write FAlignHorz;
    property AlignVert: TcxAlignmentVert read FAlignVert write FAlignVert;
    property AutoHeight: Boolean read FAutoHeight;
    property DateTime: TDateTime read FDateTime write FDateTime;
    property ImageIndex: Integer read FImageIndex;
    property ImagePosition: TcxSchedulerHeaderImagePosition read FImagePosition;
    property ImageRect: TRect read FImageRect;
    property Images: TCustomImageList read FImages;
    property IsResourceHeader: Boolean read FIsResourceHeader;
    property MultiLine: Boolean read FMultiLine write FMultiLine;
    property Neighbors: TcxNeighbors read FNeighbors write FNeighbors;
    property RotateHeader: Boolean read FRotateHeader write FRotateHeader;
    property RotateText: Boolean read FRotateText write FRotateText;
    property Selected: Boolean read FSelected write FSelected;
    property SelectionColor: TColor read FSelectionColor write FSelectionColor;
    property SelectionTextColor: TColor read FSelectionTextColor write FSelectionTextColor;
    property ShowEndEllipsis: Boolean read FShowEndEllipsis write FShowEndEllipsis;
  public
    constructor Create(APainter: TcxCustomLookAndFeelPainter; const ABounds, AVisibleRect: TRect;
      const AViewParams: TcxViewParams; AScaleFactor: TdxScaleFactor; AUseRightToLeftAlignment: Boolean); override;
    destructor Destroy; override;
    procedure Calculate(const AText: string); virtual;

    property TextRect: TRect read FTextRect;
    property Bounds;
    property DisplayText;
    property Painter;
  end;

  { TcxSchedulerHeaderCellViewInfo }

  TcxSchedulerHeaderCellViewInfo = class(TcxSchedulerCustomHeaderCellViewInfo)
  public
    procedure CalculateImageLayout; override;

    property AlignHorz;
    property AlignVert;
    property AutoHeight;
    property DateTime;
    property ImageIndex;
    property ImagePosition;
    property ImageRect;
    property Images;
    property IsResourceHeader;
    property MultiLine;
    property Neighbors;
    property Resource;
    property RotateHeader;
    property RotateText;
    property Selected;
    property SelectionColor;
    property SelectionTextColor;
    property ShowEndEllipsis;
  end;

  { TcxSchedulerDayHeaderCellViewInfo }

  TcxSchedulerDayHeaderCellViewInfo = class(TcxSchedulerHeaderCellViewInfo)
  private
    FCompressed: Boolean;
  protected
    FActualModernStyleDisplayMode: TcxSchedulerDayHeaderModernStyleDisplayMode;
    function CheckSelection: Boolean; override;
    function DrawBackground(ACanvas: TcxCanvas; const ABounds: TRect): Boolean; override;
    procedure DrawHorizontalHeader; override;
    function GetDefaultTextRectForModernStyle: TRect;
  public
    function ConvertDateToDisplayText(AType: Integer = 0): Integer; virtual;

    property ActualModernStyleDisplayMode: TcxSchedulerDayHeaderModernStyleDisplayMode read FActualModernStyleDisplayMode;
    property Compressed: Boolean read FCompressed write FCompressed;
  end;

  TcxSchedulerDayHeaderCellViewInfoClass = class of TcxSchedulerDayHeaderCellViewInfo;

  { TcxSchedulerDayHeaderCellModernViewInfo }

  TcxSchedulerDayHeaderCellModernViewInfo = class(TcxSchedulerDayHeaderCellViewInfo)
  protected
    procedure CheckTextColorWhenSelected; override;
    function DrawBackground(ACanvas: TcxCanvas; const ABounds: TRect): Boolean; override;
    procedure DrawCaption(ACanvas: TcxCanvas = nil); override;
    procedure DrawHorizontalHeader; override;
    function GetDayText(AType: Integer): string;
    function GetDefaultTextRect: TRect; override;
  public
    function ConvertDateToDisplayText(AType: Integer = 0): Integer; override;

    property TextRect;
  end;

  { TcxSchedulerWeekDayHeaderCellViewInfo }

  TcxSchedulerWeekDayHeaderCellViewInfo = class(TcxSchedulerDayHeaderCellViewInfo)
  protected
    function GetDayText(AType: Integer): string; virtual;
    procedure InitHitTest(AHitTest: TcxSchedulerCustomResourceViewHitTest); override;
  public
    function ConvertDateToDisplayText(AType: Integer = 0): Integer; override;
  end;

  { TcxSchedulerWeekDayHeaderCellModernViewInfo }

  TcxSchedulerWeekDayHeaderCellModernViewInfo = class(TcxSchedulerWeekDayHeaderCellViewInfo)
  protected
    function DrawBackground(ACanvas: TcxCanvas; const ABounds: TRect): Boolean; override;
    procedure DrawHorizontalHeader; override;
    function GetDayText(AType: Integer): string; override;
    function GetDefaultTextRect: TRect; override;
  end;

  { TcxSchedulerContainerCellViewInfo }

  TcxSchedulerContainerCellViewInfo = class(TcxSchedulerCustomResourceViewInfoItem)
  private
    FModernStyleCaptionDisplayMode: TcxSchedulerDayHeaderModernStyleDisplayMode;
    FHeaderEventsAreaTop: Integer;
    FHeaderEvent: TcxSchedulerEventCellViewInfo;
  protected
    FFirstEventTop: Integer;
    FLineCount: Integer;
    FSelected: Boolean;

    procedure DoDraw; override;
    function GetBorderColor: TColor; virtual;
    procedure InitHitTest(AHitTest: TcxSchedulerCustomResourceViewHitTest); override;
    function NeedDrawCaption: Boolean; virtual;

    property HeaderEventsAreaTop: Integer read FHeaderEventsAreaTop;
  public
    procedure Calculate(ADate: TDateTime; ASelected: Boolean;
      ACaptionDisplayMode: TcxSchedulerDayHeaderModernStyleDisplayMode; AHeadersEventsAreaTop: Integer); virtual;

    property DateTime;
    property HeaderEvent: TcxSchedulerEventCellViewInfo read FHeaderEvent write FHeaderEvent;
    property LineCount: Integer read FLineCount write FLineCount;
    property ModernStyleCaptionDisplayMode: TcxSchedulerDayHeaderModernStyleDisplayMode read FModernStyleCaptionDisplayMode;
    property Resource;
    property Selected: Boolean read FSelected write FSelected;
  end;

  TcxSchedulerContainerCellViewInfoClass = class of TcxSchedulerContainerCellViewInfo;

  { TcxSchedulerContainerModernCellViewInfo }

  TcxSchedulerContainerModernCellViewInfo = class(TcxSchedulerContainerCellViewInfo)
  strict private
    FTextRect: TRect;
    function GetIsToday: Boolean;
  protected
    procedure DoDraw; override;
    procedure DoRightToLeftConversion(const AClientBounds: TRect); override;
    procedure DrawCaption;
    function GetBorderColor: TColor; override;
    function NeedDrawCaption: Boolean; override;
  public
    procedure Calculate(ADate: TDateTime; ASelected: Boolean;
      ACaptionDisplayMode: TcxSchedulerDayHeaderModernStyleDisplayMode; AHeadersEventsAreaTop: Integer); override;

    property DisplayText;
    property HeaderEventsAreaTop;
    property IsToday: Boolean read GetIsToday;
  end;

  { TcxSchedulerTimeRulerCellViewInfo }

  TcxSchedulerTimeRulerCellViewInfo = class(TcxSchedulerCustomViewInfoItem)
  private
    FCanDrawCurrentTime: Boolean;
    FLastVisibleHour: Boolean;
    function GetBoundsRect(AType: Boolean): TRect;
    function GetDisplayText(AType: Boolean): string;
    function GetHasAdditionalTimeZone: Boolean;
  protected
    FAdditionalTimeZone: Integer;
    FAdditionalTimeZoneBiasDelta: Integer;
    FBounds: array[Boolean] of TRect;
    FDisplayTexts: array[Boolean] of string;
    FHour: Integer;
    FLargeFont: TFont;
    FLineCount: Integer;
    FShowMinutes: Boolean;
    FTimeZone: Integer;
    procedure CalculateDisplayInfo;
    procedure DoDraw; override;
    procedure DoRightToLeftConversion(const AClientBounds: TRect); override;
    procedure DrawTimeZone(const ABounds: TRect; const AText: string; AIsCurrent: Boolean); virtual;
    procedure DrawTimeZoneLabel(const ABounds: TRect; const AText: string; ABorders: TcxBorders); virtual;
    procedure DrawCurrentTime(const AStart: TDateTime; ABounds: TRect);
    function GetAdditionalTime: TDateTime;
    class function GetBaseWidth(ALineCount: Integer; AFont1, AFont2: TFont;
      AScaleFactor: TdxScaleFactor; AShowMinutes: Boolean): Integer; virtual;
    function GetBorderColor: TColor; virtual;
    function GetCanDrawCurrentTime(ASelectedDays: TcxSchedulerDateList): Boolean; virtual;
    function GetLabelZoneBorders(const AIsCurrentTimeZone: Boolean): TcxBorders; virtual;
    function GetLabelZoneTextRect(const ABounds: TRect): TRect; virtual;
    function GetRealRightBorder: TcxBorders;
    function GetTimeDisplayText(const ATime: TDateTime): string; virtual;
    procedure InitHitTest(AHitTest: TcxSchedulerCustomResourceViewHitTest); override;

    //
    procedure AfterCustomDraw(ACanvas: TcxCanvas); override;

    property CanDrawCurrentTime: Boolean read FCanDrawCurrentTime;
  public
    class function CalculateWidth(ATimeZoneCount, ALineCount: Integer; AFont1, AFont2: TFont;
      AScaleFactor: TdxScaleFactor = nil; AShowMinutes: Boolean = False): Integer;
    procedure Calculate(const ALabel1, ALabel2: string; ATimeZone, AAdditionalTimeZone: Integer;
      AAdditionalTimeZoneBiasDelta: Integer; ASelectedDays: TcxSchedulerDateList = nil); overload; virtual;
    procedure Calculate(AHour, ALineCount: Integer; ATimeZone, AAdditionalTimeZone: Integer;
      ALargeFont: TFont; AAdditionalTimeZoneBiasDelta: Integer; ASelectedDays: TcxSchedulerDateList = nil); overload; virtual;
    procedure SetBottom(AValue: Integer);

    property AdditionalTimeZone: Integer read FAdditionalTimeZone;
    property Bounds[ACurrentTimeZone: Boolean]: TRect read GetBoundsRect;
    property DisplayTexts[ACurrentTimeZone: Boolean]: string read GetDisplayText;
    property HasAdditionalTimeZone: Boolean read GetHasAdditionalTimeZone;
    property Hour: Integer read FHour;
    property LargeFont: TFont read FLargeFont;
    property LastVisibleHour: Boolean read FLastVisibleHour write FLastVisibleHour;
    property LineCount: Integer read FLineCount;
    property ShowMinutes: Boolean read FShowMinutes write FShowMinutes;
    property TimeZone: Integer read FTimeZone;
  end;

  TcxSchedulerTimeRulerCellViewInfoClass = class of TcxSchedulerTimeRulerCellViewInfo;

  { TcxSchedulerTimeRulerModernCellViewInfo }

  TcxSchedulerTimeRulerModernCellViewInfo = class(TcxSchedulerTimeRulerCellViewInfo)
  protected
    procedure DrawBottomLine(const ABounds: TRect); virtual;
    procedure DrawTimeZone(const ABounds: TRect; const AText: string; AIsCurrent: Boolean); override;
    procedure DrawTimeZoneLabel(const ABounds: TRect; const AText: string; ABorders: TcxBorders); override;
    class function GetBaseWidth(ALineCount: Integer; AFont1, AFont2: TFont;
      AScaleFactor: TdxScaleFactor; AShowMinutes: Boolean): Integer; override;
    function GetBorderColor: TColor; override;
    function GetBottomLineIndent: Integer; inline;
    function GetCanDrawCurrentTime(ASelectedDays: TcxSchedulerDateList): Boolean; override;
    function GetLabelZoneBorders(const AIsCurrentTimeZone: Boolean): TcxBorders; override;
    function GetLabelZoneTextRect(const ABounds: TRect): TRect; override;
    function GetTimeDisplayText(const ATime: TDateTime): string; override;
  end;

  { TcxSchedulerContentCellViewInfo }

  TcxSchedulerContentCellViewInfo = class(TcxSchedulerCustomResourceViewInfoItem)
  protected
    FBorderColor: TColor;
    FDontPrint: Boolean;
    FFirstEventTop: Integer;
    FLineCount: Integer;
    FSelected: Boolean;
    FSelectionColor: TColor;
    FSelectionTextColor: TColor;
    FShowTimeLine: Boolean;
    FTimeEvent: TcxSchedulerEventCellViewInfo;
    FTimeLineBorders: TcxBorders;
    FTimeLineParams: TcxViewParams;
    procedure DoDraw; override;
    procedure DoDrawWithTimeEventStateConsidering;
    procedure DoRightToLeftConversion(const AClientBounds: TRect); override;

    function GetDefaultBorderColor: TColor; virtual;
    procedure InitHitTest(AHitTest: TcxSchedulerCustomResourceViewHitTest); override;
    procedure SetContentSelectionParameters(ASelected: Boolean; const ASelectionParams: TcxViewParams);
    procedure SpecialDrawRect(const ARect: TRect; ABorders: TcxBorders; AContentColor, ABorderColor: TColor);

    property SelectionColor: TColor read FSelectionColor write FSelectionColor;
    property SelectionTextColor: TColor read FSelectionTextColor write FSelectionTextColor;
  public
    constructor Create(APainter: TcxCustomLookAndFeelPainter; const ABounds, AVisibleRect: TRect;
      const AViewParams: TcxViewParams; AScaleFactor: TdxScaleFactor; AUseRightToLeftAlignment: Boolean); override;
    procedure SetTime(const ATimeStart, ATimeFinish: TDateTime);

    property BorderColor: TColor read FBorderColor write FBorderColor;
    property DontPrint: Boolean read FDontPrint write FDontPrint;
    property LineCount: Integer read FLineCount write FLineCount;
    property Selected: Boolean read FSelected;
    property ShowTimeLine: Boolean read FShowTimeLine write FShowTimeLine;
    property TimeEvent: TcxSchedulerEventCellViewInfo read FTimeEvent write FTimeEvent;
    property TimeLineBorders: TcxBorders read FTimeLineBorders write FTimeLineBorders;
    property TimeLineParams: TcxViewParams read FTimeLineParams write FTimeLineParams;
    property TimeStart: TDateTime read FDateTime;
    property TimeFinish;
    property Resource;
  end;

  TcxSchedulerContentCellViewInfoClass = class of TcxSchedulerContentCellViewInfo;

 { TcxSchedulerBackgroundSlotCellViewInfo }

  TcxSchedulerBackgroundSlotCellViewInfo = class(TcxSchedulerContentCellViewInfo)
  protected
    procedure DoDraw; override;
  public
    property DisplayText;
  end;

  { TcxSchedulerWeekDayModernContentCellViewInfo }

  TcxSchedulerWeekDayContentCellModernViewInfo = class(TcxSchedulerContentCellViewInfo)
  protected
    procedure DoDraw; override;
    function GetDefaultBorderColor: TColor; override;
  end;

  { TcxSchedulerMonthDayContentCellCustomViewInfo }

  TcxSchedulerMonthDayContentCellCustomViewInfo = class(TcxSchedulerContentCellViewInfo)
  private
    function GetIsToday: Boolean;
    procedure SetSmallTextFont(AFont: TFont);
  protected
    FTextRect: TRect;
    FSmallFont: Boolean;
    FSmallFontCreated: Boolean;
    FSmallTextFont: TFont;
    procedure DoDraw; override;
    procedure DoDrawBackground; virtual; abstract;
    procedure DoRightToLeftConversion(const AClientBounds: TRect); override;
    procedure DrawDisplayText;
    function GetDisplayText: string; virtual;
    function GetTextFlags: Cardinal; virtual; abstract;
    function GetTextLeft: Integer; virtual;
    function GetTextLeftForSmallFont: Integer; virtual;
    function GetTextLeftIndent: Integer; virtual;
    function GetTextRightIndent: Integer; virtual;
    property IsToday: Boolean read GetIsToday;
    function SaveFontStyle(AFont: TFont): TFontStyles; virtual;
  public
    destructor Destroy; override;
    procedure SetContentState(AsDayOfMonth: Boolean; ASelected: Boolean;
      ATextHeight: Integer; const ASelectionParams: TcxViewParams);
    function UpdateSelection(ASelected: Boolean): Boolean;

    property DisplayText;
    property SmallFont: Boolean read FSmallFont;
    property SmallTextFont: TFont read FSmallTextFont write SetSmallTextFont;
    property SelectionColor;
    property SelectionTextColor;
  end;

  { TcxSchedulerMonthDayContentCellViewInfo }

  TcxSchedulerMonthDayContentCellViewInfo = class(TcxSchedulerMonthDayContentCellCustomViewInfo)
  protected
    procedure DoDrawBackground; override;
    function GetTextFlags: Cardinal; override;
    function GetTextRightIndent: Integer; override;
  end;

  { TcxSchedulerMonthDayModernContentCellViewInfo }

  TcxSchedulerMonthDayContentCellModernViewInfo = class(TcxSchedulerMonthDayContentCellCustomViewInfo)
  protected
    procedure DoDraw; override;
    procedure DoDrawBackground; override;
    function GetDefaultBorderColor: TColor; override;
    function GetDisplayText: string; override;
    function GetTextFlags: Cardinal; override;
    function GetTextLeft: Integer; override;
    function GetTextLeftForSmallFont: Integer; override;
    function SaveFontStyle(AFont: TFont): TFontStyles; override;
  end;

  TcxSchedulerEventImagesLayout = (eilAuto, eilHorizontal, eilVertical);

  { TcxSchedulerEventViewData }

  TcxSchedulerEventViewData = class
  public
    AlwaysShowTime: Boolean;
    AutoHeight: Boolean;
    BorderColor: TColor;
    Bounds: TRect;
    Canvas: TcxCanvas;
    Caption: string;
    ContentFinish: TDateTime;
    ContentStart: TDateTime;
    CurrentDate: TDateTime;
    DrawAsProgress: Boolean;
    DrawShadows: Boolean;
    EditProperties: TcxCustomEditProperties;
    EditStyle: TcxCustomEditStyle;
    Event: TcxSchedulerControlEvent;
    ExternalPainter: TcxSchedulerExternalPainter;
    GetEventHint: function(AEvent: TcxSchedulerControlEvent): string of object;
    ImagesLayout: TcxSchedulerEventImagesLayout;
    IsGroup: Boolean;
    IsMilestone: Boolean;
    LineHeight: Integer;
    MaxHeight: Integer;
    Painter: TcxCustomLookAndFeelPainter;
    Resource: TcxSchedulerResourceViewInfo;
    ShowAllDayEventsInContentArea: Boolean;
    ShowFinishTime: Boolean;
    ShowStartTime: Boolean;
    ShowTimeAsClock: Boolean;
    ShowTimeLine: Boolean;
    StartLine: Integer;
    TaskComplete: Integer;
    TaskStatus: TcxSchedulerEventTaskStatus;
    TimeLineParams: TcxViewParams;
    ViewContentFinish: TDateTime;
    ViewContentStart: TDateTime;
    ViewParams: TcxViewParams;
    VisibleRect: TRect;
  end;

  { TcxSchedulerEventCellViewInfo }

  TcxSchedulerEventCellViewInfo = class(TcxSchedulerCustomViewInfoItem)
  private
    FActualBorderColorIsNone: Boolean;
    FCanResize: Boolean;
    FBorderSize: Integer;
    FEventFinish: TDateTime;
    FEventStart: TDateTime;
    FIsHeaderEvent: Boolean;
    FSelectionBorderRect: TRect;
    FSelectionBorderSize: Integer;
    FSizingHandlerRect1: TRect;
    FSizingHandlerRect2: TRect;
    function CalculateMessageHeight(const R: TRect): Integer;
    function GetBorderColor: TColor;
    function GetCaption: string;
    function GetCaptionBounds(const R: TRect): TRect;
    function GetContentFinish: TDateTime;
    function GetContentStart: TDateTime;
    function GetEvent: TcxSchedulerControlEvent;
    function GetHint: string;
    function GetIsHeaderEvent: Boolean;
    function GetMessage: string;
    function GetResourceInfo: TcxSchedulerResourceViewInfo;
    function GetResourceItem: TcxSchedulerStorageResourceItem;
    function GetSelected: Boolean;
    function GetShowingState(AIndex: Integer): Boolean;
    procedure SetBorderColor(AValue: TColor);
  protected
    FCalculationCounter: Integer;
    FCaptionAndLocationCanPlaceOneLineOnly: Boolean;
    FCaptionIsBold: Boolean;
    FCaptionRect: TRect;
    FDetailCaptionVertOffset: Integer;
    FEditViewInfo: TcxCustomEditViewInfo;
    FEndContinueArrowCaption: string;
    FEndContinueArrowCaptionRect: TRect;
    FEndContinueArrowRect: TRect;
    FEventTimeRect: TRect;
    FFinishRect: TRect;
    FFinishText: string;
    FHidden: Boolean;
    FImages: TcxSchedulerEventImages;
    FIsDetailCaption: Boolean;
    FIsDetailInfo: Boolean;
    FMessageRect: TRect;
    FHint: string;
    FHintAssigned: Boolean;
    FHintNeeded: Boolean;
    FHintNeededCalculated: Boolean;
    FLocation: string;
    FLocationRect: TRect;
    FSeparatorColor: TColor;
    FShowMessage: Boolean;
    FStartContinueArrowCaption: string;
    FStartContinueArrowCaptionRect: TRect;
    FStartContinueArrowRect: TRect;
    FStartRect: TRect;
    FStartText: string;
    FTimeLineRect: TRect;
    ViewData: TcxSchedulerEventViewData;
    procedure AfterDraw; virtual;
    procedure AssignEditStyle(AEditStyle: TcxCustomEditStyle);
    procedure BeforeDraw; virtual;
    procedure BeforeCustomDraw(ACanvas: TcxCanvas); override;
    procedure Calculate;
    procedure CalculateBorderAttributes; virtual;
    procedure CalculateBorders; virtual;
    procedure CalculateCaptions; virtual;
    procedure CalculateDetailInfo; virtual;
    procedure CalculateNeedHint;
    procedure CalculateEditViewInfo;
    procedure CalculateActualEditRect(AEditViewData: TcxCustomEditViewData; var AMessageRect: TRect); virtual;
    procedure InitializeEditViewInfo(var AMessageRect: TRect); virtual;
    procedure CalculateShowTimeAsClock; virtual;
    procedure CalculateSelectionBorderParams; virtual;
    procedure CalculateSizingHandlers; virtual;
    function CanBeExcludedSelectionLeftBorder: Boolean; virtual;
    function CanBeExcludedSelectionRightBorder: Boolean; virtual;
    function CanMoveLeftBorder: Boolean; virtual;
    function CanMoveRightBorder: Boolean; virtual;
    procedure DoRightToLeftConversion(const AClientBounds: TRect); override;
    function EnableModernSelection: Boolean; virtual;
    function GetDetailCaptionFlagValue: Boolean; virtual;
    function GetDetailInfoFlagValue: Boolean; virtual;
    function NeedBottomSizingHandler: Boolean; virtual;
    function NeedLeftSizingHandler: Boolean; virtual;
    function NeedRightSizingHandler: Boolean; virtual;
    function NeedTopSizingHandler: Boolean; virtual;
    procedure TuneClipping(var AClipRgn: TcxRegion); override;

    //items layout
    function CalculateAutoLayoutImagesAutoHeight(AVisibleImages: TList): Integer; virtual;
    procedure CalculateHeaderEventContinueArrows(ACompressing: Integer); virtual;
    function CalculateHorizontalImagesAutoHeight: Integer; virtual;
    procedure CalculateLocationAutoLayout; virtual;
    procedure CalculateMessageAutoLayout(const R: TRect; var AHeight: Integer);
    function CalculateVerticalImagesAutoHeight: Integer; virtual;
    function CanBeHeaderEventContinueArrows: Boolean; virtual;
    function CheckAutoLayoutImagesSingleLineCaption(const ACaptionWidth, AAvailableWidth: Integer): Boolean; virtual;
    procedure CheckItemsAfterAutoLayoutImagesCalculation(AFactuallyImagesRowCount, AImagesWidth: Integer); virtual;
    procedure CheckItemsBeforeAutoLayoutImagesCalculation; virtual;
    function GetAvailableBounds: TRect; virtual;
    function GetHeaderEventPossibleCaptionRect: TRect;
    function GetHorizontalImagesMessageAutoHeight(AMessageTop: Integer): Integer;
    function GetNonDetailEventPossibleCaptionRect: TRect; virtual;
    function GetMessageRectLeft: Integer; virtual;
    procedure InitializeHeaderEventContinueArrowsData;
    function NeedBorderAroundOfTimeLineRect: Boolean; virtual;
    function NeedCorrectCaptionBottomWhenMessageAutoLayoutCalculating: Boolean; virtual;
    function NeedHeaderEventStartContinueArrow: Boolean; virtual;
    function NeedHeaderEventFinishContinueArrow: Boolean; virtual;

    function CalculateClipRect(const ABounds, AVisibleRect: TRect): TRect; override;
    procedure CalculateItemsLayout; virtual;
    procedure CalculateDetailViewEventLayout;
    procedure CalculateHeaderEventNeededCaptionWidth(var AFullWidth, ACaptionOnlyWidth: Integer); virtual;
    procedure CalculateHeaderEventLayout;
    procedure CalculateLocationRect; virtual;
    procedure CalculateNonDetailEventLayout;
    procedure CalculateEventTimeAsClockLayout(const ABounds: TRect;
      const ACaptionWidth, AImagesWidth: Integer; var ALeft: Integer); virtual;
    procedure CalculateEventTimeAsTextLayout(const ABounds: TRect;
      const ACaptionWidth, AImagesWidth: Integer; var ALeft: Integer); virtual;
    function CalculateNonDetailEventImages(const ACaptionWidth: Integer;
      out AImagesWidth: Integer): TRect; virtual;
    function CalculateSingleLineImages(const ABounds: TRect; ARightLimit: Integer = 0): Integer;
    procedure CalculateTimeLineLayout; virtual;
    function CanShowHeaderEventClock(const ARect: TRect): Boolean; virtual;
    function CanShowHeaderEventTimeText(const ARect: TRect): Boolean; virtual;
    function GetActiveBounds: TRect; override;
    function GetBoundsWithoutTimeLineRect: TRect; virtual;
    function GetContinueArrowCaptionDateFormat: string; virtual;
    function GetHeaderEventCaptionLeft(const ARect: TRect; AContentWidth: Integer): Integer; virtual;
    function GetHeaderImagesPossibleRect(const ACaptionRect: TRect; ATextWidth: Integer): TRect; virtual;
    function GetHeaderEventStartClockOriginLeft: Integer; virtual;
    function GetHeaderEventFinishClockOriginRight: Integer; virtual;
    function GetLabelBasedBorderColor: TColor;
    function GetNonDetailEventSingleLineCaptionWidth: Integer; virtual;
    function GetOffsetSelectionBorderFromTimeLineRect: Integer; virtual;
    function GetSelectedBorderColor: TColor; virtual;
    function GetSelectionBoundsExtends: TRect; virtual;
    function GetStartTextJustify: Cardinal; virtual;
    function GetFinishTextJustify: Cardinal; virtual;
    function GetTimeLineBorderColor: TColor; virtual;
    function GetTimeLineHeight: Integer; virtual;
    function GetTimeLineRectBaseBounds: TRect; virtual;
    function GetTimeLineRectRight: Integer; virtual;
    function GetTimeLineRectTop: Integer; virtual;
    function GetTimeTextMaxWidth: Integer;
    function SetItemRect(AShowItem: Boolean; const ABounds: TRect;
      AVertOffset: Integer; var APos: Integer): TRect;

    //time visibility
    procedure CalculateEventTimeVisibility; virtual;
    procedure CalculateDetailEventTimeVisibility; virtual;
    procedure CalculateItemsLayoutForMeasureHeight; virtual;
    procedure CalculateNonDetailEventTimeVisibility; virtual;
    procedure CalculateVisibility; virtual;
    function CanAutoHideStandardImages: Boolean; virtual;
    function CanShowHint: Boolean; virtual;

    procedure DoDraw; override;
    procedure DoDrawCaption; virtual;
    procedure DoDrawEndContinueArrow;
    procedure DoDrawStartContinueArrow;
    procedure DrawEventBorderAroundOfTimeLineRect;
    procedure DrawCaption;
    procedure DrawContent; virtual;
    procedure DrawContinueArrowsAndCaptions; virtual;
    procedure DrawImages;
    procedure DrawLocation; virtual;
    procedure DrawMessage; virtual;
    procedure DrawMessageSeparator; virtual;
    procedure DrawSelection; virtual;
    procedure DrawSizingHandler(const R: TRect; var ARgn: TcxRegion);
    procedure DrawState; overload; virtual;
    procedure DrawTime; virtual;
    function GetCaptionAutoHeight(const R: TRect): Integer;
    function GetCaptionFlags: Cardinal; virtual;
    function GetCaptionFontStyle: TFontStyles; virtual;
    function GetDrawCaptionFlags: Cardinal; virtual;
    function GetEditingRect: TRect; virtual;
    function GetEditStyleColor: TColor; virtual;
    function GetForceShowClockInHeaderEvent: Boolean; virtual;
    function GetHitTest(AHitTest: TcxSchedulerCustomResourceViewHitTest; var ABreak: Boolean): Boolean; override;
    function GetImagesBounds: TRect;
    function GetImagesHorizontalOffset: Integer; virtual;
    function GetImagesVerticalOffset(AImageHeight: Integer; AIsAbsolute: Boolean): Integer; virtual;
    function GetLocationAutoHeight(const R: TRect): Integer;
    function GetMessageRect(const ACaptionRect: TRect; AHasImages: Boolean): TRect;
    function GetMessageRectOffset: Integer; virtual;
    function GetMessageSeparatorLeftIndent: Integer;
    function GetViewStyle: TcxSchedulerViewStyle; virtual;
    function HasLeftBorder: Boolean;
    function HasRightBorder: Boolean;
    function HasReminder: Boolean; virtual;
    procedure Initialize; virtual;
    procedure MeasureCaptionExtent(var R: TRect); virtual;
    function MeasureAutoLayoutImagesLocationHeight(const R: TRect): Integer; virtual;
    //HitTest
    function HintTriggerArea: TRect; virtual;
    procedure InitHitTest(AHitTest: TcxSchedulerCustomResourceViewHitTest); override;
    procedure InitHitTestForHorzEvent(AHitTest: TcxSchedulerCustomResourceViewHitTest);

    procedure InitStandardEventImages;
    function IsHorzSizing: Boolean; virtual;
    function IsNeedDrawTime: Boolean; virtual;
    function IsTimeLineVisible: Boolean; virtual;
    procedure ShiftRect(var R: TRect; const X, Y: Integer);
    function TruncTime(const ATime: TDateTime): TDateTime;

    property ActualBorderColorIsNone: Boolean read FActualBorderColorIsNone;
    property BorderSize: Integer read FBorderSize;
    property CanResize: Boolean read FCanResize;
    property EventFinish: TDateTime read FEventFinish;
    property EventStart: TDateTime read FEventStart;
    property IsHeaderEvent: Boolean read FIsHeaderEvent;
    property IsDetailCaption: Boolean read FIsDetailCaption;
    property LocationRect: TRect read FLocationRect;
    property SelectionBorderRect: TRect read FSelectionBorderRect;
    property SelectionBorderSize: Integer read FSelectionBorderSize;
    property SizingHandlerRect1: TRect read FSizingHandlerRect1;
    property SizingHandlerRect2: TRect read FSizingHandlerRect2;
  public
    constructor Create(AViewData: TcxSchedulerEventViewData; ACanResize: Boolean;
      AScaleFactor: TdxScaleFactor; AUseRightToLeftAlignment: Boolean); reintroduce; virtual;
    destructor Destroy; override;

    procedure DrawState(ACanvas: TcxCanvas; const ARect: TRect; ABorders: TcxBorders; ABorderColor: TColor); overload;
    function MeasureHeight(ACanvas: TcxCanvas): Integer; virtual;
    procedure MoveTo(X, Y: Integer); virtual;

    property BorderColor: TColor read GetBorderColor write SetBorderColor;
    property Caption: string read GetCaption;
    property CaptionRect: TRect read FCaptionRect;
    property ClipRect: TRect read FClipRect;
    property ClipRgn: TcxRegion read FClipRgn write FClipRgn;
    property ContentFinish: TDateTime read GetContentFinish;
    property ContentStart: TDateTime read GetContentStart;
    property EditingRect: TRect read GetEditingRect;
    property EditViewInfo: TcxCustomEditViewInfo read FEditViewInfo;
    property Event: TcxSchedulerControlEvent read GetEvent;
    property EventTimeRect: TRect read FEventTimeRect;
    property EventViewData: TcxSchedulerEventViewData read ViewData;
    property FinishRect: TRect read FFinishRect;
    property FinishText: string read FFinishText;
    property Hidden: Boolean read FHidden write FHidden;
    property Hint: string read GetHint;
    property Images: TcxSchedulerEventImages read FImages;
    property IsDetailInfo: Boolean read FIsDetailInfo;
    property Message: string read GetMessage;
    property MessageRect: TRect read FMessageRect;
    property ResourceInfo: TcxSchedulerResourceViewInfo read GetResourceInfo;
    property ResourceItem: TcxSchedulerStorageResourceItem read GetResourceItem;
    property Selected: Boolean read GetSelected;
    property SeparatorColor: TColor read FSeparatorColor write FSeparatorColor;
    property ShowFinishTime: Boolean index 0 read GetShowingState;
    property ShowMessage: Boolean read FShowMessage;
    property ShowStartTime: Boolean index 1 read GetShowingState;
    property ShowTimeAsClock: Boolean index 2 read GetShowingState;
    property ShowTimeLine: Boolean index 3 read GetShowingState;
    property StartRect: TRect read FStartRect;
    property StartText: string read FStartText;
    property TimeLineRect: TRect read FTimeLineRect write FTimeLineRect;
    property ViewStyle: TcxSchedulerViewStyle read GetViewStyle;
    property Visible: Boolean read FVisible;
  end;

  TcxSchedulerEventCellViewInfoClass = class of TcxSchedulerEventCellViewInfo;

  { TcxSchedulerEventCellViewInfoList }

  TcxSchedulerEventCellViewInfoList = class(TcxSchedulerViewInfoCellList)
  private
    function GetItem(Index: Integer): TcxSchedulerEventCellViewInfo;
  public
    procedure RemoveEvent(AEvent: TcxSchedulerEvent);
    property Items[Index: Integer]: TcxSchedulerEventCellViewInfo read GetItem; default;
  end;

  { TcxSchedulerMoreEventsButtonViewInfo }

  TcxSchedulerMoreEventsButtonClick = procedure (Sender: TcxSchedulerMoreEventsButtonViewInfo) of object;

  TcxSchedulerMoreEventsButtonViewInfoClass = class of TcxSchedulerMoreEventsButtonViewInfo;

  TcxSchedulerMoreEventsButtonViewInfo = class(TcxSchedulerCustomResourceViewInfoItem)
  protected
    FEvent: TcxSchedulerEvent;
    FIsDown: Boolean;
    FOnClick: TcxSchedulerMoreEventsButtonClick;
    procedure DoDraw; override;
    procedure InitHitTest(AHitTest: TcxSchedulerCustomResourceViewHitTest); override;
    property OnClick: TcxSchedulerMoreEventsButtonClick read FOnClick write FOnClick;
  public
    procedure Click; virtual;

    property Event: TcxSchedulerEvent read FEvent write FEvent;
    property IsDown: Boolean read FIsDown;
    property DateTime;
    property Resource;
  end;

  { TcxSchedulerMoreEventsModernButtonViewInfo }

  TcxSchedulerMoreEventsModernButtonViewInfo = class(TcxSchedulerMoreEventsButtonViewInfo)
  private
    FHitTest: TcxSchedulerCustomResourceViewHitTest;
  protected
    procedure DoDraw; override;
    procedure InitHitTest(AHitTest: TcxSchedulerCustomResourceViewHitTest); override;
  end;


  { TcxSchedulerContentNavigationButtonViewInfo }

  TcxSchedulerContentNavigationButtonClick = procedure (Sender: TcxSchedulerContentNavigationButtonViewInfo) of object;

  TcxSchedulerContentNavigationButtonViewInfo = class(TcxSchedulerCustomResourceViewInfoItem)
  private
    FArrowRect: TRect;
    FCaption: string;
    FHitTest: TcxSchedulerCustomResourceViewHitTest;
    FInterval: TDateTime;
    FIsVertical: Boolean;
    FKind: TcxSchedulerContentNavigationButtonKind;
    FOnClick: TcxSchedulerContentNavigationButtonClick;
    FTextRect: TRect;
    function GetEnabled: Boolean;
  protected
    procedure DoDraw; override;
    procedure DoRightToLeftConversion(const AClientBounds: TRect); override;
    procedure InitHitTest(AHitTest: TcxSchedulerCustomResourceViewHitTest); override;
    procedure SetIsVertical(AValue: Boolean);

    property Enabled: Boolean read GetEnabled;
    property OnClick: TcxSchedulerContentNavigationButtonClick read FOnClick write FOnClick;
  public
    constructor Create(APainter: TcxCustomLookAndFeelPainter; const ABounds, AVisibleRect: TRect;
      const AViewParams: TcxViewParams; AScaleFactor: TdxScaleFactor; AUseRightToLeftAlignment: Boolean); override;
    procedure Click; virtual;
    property ArrowRect: TRect read FArrowRect;
    property Caption: string read FCaption write FCaption;
    property Interval: TDateTime read FInterval write FInterval;
    property Kind: TcxSchedulerContentNavigationButtonKind read FKind write FKind;
    property TextRect: TRect read FTextRect;
  end;

  { TcxSchedulerGroupSeparatorCellViewInfo }

  TcxSchedulerGroupSeparatorCellViewInfo = class(TcxSchedulerCustomViewInfoItem)
  private
    FDrawRotatedBackground: Boolean;
    FRotated: Boolean;
    function DrawRotateBackground(ACanvas: TcxCanvas; const ABounds: TRect): Boolean;
  protected
    FRotateBitmap: TcxBitmap;
    procedure DoDraw; override;
    procedure InitHitTest(AHitTest: TcxSchedulerCustomResourceViewHitTest); override;
  public
    destructor Destroy; override;
    property Rotated: Boolean read FRotated write FRotated;
  end;

  { TcxSchedulerResourceViewInfo }

  TcxSchedulerResourceViewInfo = class
  protected
    FCaption: string;
    FColor: TColor;
    FImageIndex: Integer;
    FImagePosition: TcxSchedulerHeaderImagePosition;
    FImages: TCustomImageList;
    FResourceID: Variant;
    FResourceItem: TcxSchedulerStorageResourceItem;
    FTextRect: TRect;
    FVisibleEventCount: Integer;
  public
    constructor Create(AResource: TcxSchedulerStorageResourceItem); virtual;

    property Caption: string read FCaption;
    property Color: TColor read FColor;
    property ImageIndex: Integer read FImageIndex;
    property ImagePosition: TcxSchedulerHeaderImagePosition read FImagePosition;
    property Images: TCustomImageList read FImages;
    property ResourceID: Variant read FResourceID;
    property ResourceItem: TcxSchedulerStorageResourceItem read FResourceItem;
    property TextRect: TRect read FTextRect write FTextRect;
    property VisibleEventCount: Integer read FVisibleEventCount write FVisibleEventCount;
  end;

  { TcxSchedulerExternalPainter }

  TcxSchedulerExternalPainter = class(TInterfacedObject, IcxSchedulerCommonViewItemsPainter)
  private
    FCommonPainter: IcxSchedulerCommonViewItemsPainter;
    FPainter: TcxCustomLookAndFeelPainter;
    function GetPainterHelper: TcxSchedulerPainterHelperClass;
  protected
    procedure DrawCustomCurrentTime(ACanvas: TcxCanvas; AColor: TColor; AStart: TDateTime; const ABounds: TRect; AUseRightToLeftAlignment: Boolean); virtual;

    property PainterHelper: TcxSchedulerPainterHelperClass read GetPainterHelper;
  public
    // IcxSchedulerCommonViewItemsPainter
    procedure DoCustomDrawBackground(AViewInfo: TcxSchedulerBackgroundCellViewInfo; var ADone: Boolean); virtual;
    procedure DoCustomDrawButton(AViewInfo: TcxSchedulerMoreEventsButtonViewInfo; var ADone: Boolean); virtual;
    procedure DoCustomDrawContent(AViewInfo: TcxSchedulerContentCellViewInfo; var ADone: Boolean); virtual;
    procedure DoCustomDrawDayHeader(AViewInfo: TcxSchedulerDayHeaderCellViewInfo; var ADone: Boolean); virtual;
    procedure DoCustomDrawEvent(AViewInfo: TcxSchedulerEventCellViewInfo; var ADone: Boolean); virtual;
    procedure DoCustomDrawGroupSeparator(AViewInfo: TcxSchedulerGroupSeparatorCellViewInfo; var ADone: Boolean); virtual;
    procedure DoCustomDrawNavigationButton(AViewInfo: TcxSchedulerContentNavigationButtonViewInfo; var ADone: Boolean); virtual;
    procedure DoCustomDrawResourceHeader(AViewInfo: TcxSchedulerHeaderCellViewInfo; var ADone: Boolean); virtual;
    function HasCustomDrawGroupSeparator: Boolean;
    function HasCustomDrawResourceHeader: Boolean;
    //
    procedure DrawAllDayArea(ACanvas: TcxCanvas; const ARect: TRect; ABorderColor: TColor;
      ABorders: TcxBorders; AViewParams: TcxViewParams; ASelected, ATransparent: Boolean); virtual;
    procedure DrawCellFocus(AViewInfo: TcxSchedulerCustomViewInfoItem; AColor: TColor); virtual;
    procedure DrawCurrentTime(ACanvas: TcxCanvas; AColor: TColor; AStart: TDateTime; ABounds: TRect); virtual;
    procedure DrawCurrentTimeRTL(ACanvas: TcxCanvas; AColor: TColor; AStart: TDateTime; ABounds: TRect); virtual;
    function DrawCurrentTimeFirst: Boolean; virtual;
    procedure DrawEvent(AViewInfo: TcxSchedulerEventCellViewInfo); virtual;
    procedure DrawEventAsGroup(AViewInfo: TcxSchedulerEventCellViewInfo); virtual;
    procedure DrawEventAsMilestone(AViewInfo: TcxSchedulerEventCellViewInfo); virtual;
    procedure DrawEventAsProgress(AViewInfo: TcxSchedulerEventCellViewInfo); virtual;
    procedure DrawEventAsProgressText(AViewInfo: TcxSchedulerEventCellViewInfo;
      AContent: TRect; AProgressRect: TRect; const AText: string); virtual;
    procedure DrawEventLabel(AViewInfo: TcxSchedulerEventCellViewInfo; const R: TRect; AColor: TColor); virtual;
    procedure DrawTimeGridCurrentTime(ACanvas: TcxCanvas; AColor: TColor; const ATimeLineRect: TRect); virtual;
    procedure DrawTimeGridHeader(ACanvas: TcxCanvas; ABorderColor: TColor;
      AViewInfo: TcxSchedulerCustomViewInfoItem; ABorders: TcxBorders; ASelected: Boolean); virtual;
    function DrawTimeGridTimeScaleTicks: Boolean; virtual;
    procedure DrawTimeLine(ACanvas: TcxCanvas; const ARect: TRect; AViewParams: TcxViewParams; ABorders: TcxBorders; ABorderColor: TColor); virtual;
    procedure DrawTimeRulerBackground(ACanvas: TcxCanvas; const ARect: TRect;
      ABorders: TcxBorders; AViewParams: TcxViewParams; ATransparent: Boolean); overload; virtual;
    procedure DrawTimeRulerBackground(ACanvas: TcxCanvas; const ARect: TRect;
      ABorders: TcxBorders; ABorderBolor: TColor; AViewParams: TcxViewParams; ATransparent: Boolean); overload; virtual;
    procedure DrawShadow(ACanvas: TcxCanvas; const ABounds, AVisibleBounds: TRect; AScaleFactor: TdxScaleFactor = nil); virtual;
    function DrawShadowFirst: Boolean; virtual;

    function GetColorizedColor(ASource: TColor; AEvent: TcxSchedulerEventCellViewInfo): TColor;
    function GetEventBorderColor(AViewInfo: TcxSchedulerEventCellViewInfo): TColor; virtual;
    function GetEventBorderSize(AViewInfo: TcxSchedulerEventCellViewInfo): Integer; virtual;
    function GetEventLabelSize(AScaleFactor: TdxScaleFactor = nil): TSize; virtual;
    function GetEventSelectionBorderColor(AViewInfo: TcxSchedulerEventCellViewInfo): TColor; virtual;
    function GetEventSelectionBorderSize(AViewInfo: TcxSchedulerEventCellViewInfo): Integer; virtual;
    function GetEventSelectionExtends: TRect; overload; virtual;
    function GetEventSelectionExtends(AViewInfo: TcxSchedulerEventCellViewInfo): TRect; overload; virtual;
    function MoreButtonSize(ASize: TSize; AScaleFactor: TdxScaleFactor = nil): TSize; virtual;
    function NeedDrawSelection: Boolean; virtual;

    property Painter: TcxCustomLookAndFeelPainter read FPainter write FPainter;
    property CommonPainter: IcxSchedulerCommonViewItemsPainter read FCommonPainter write FCommonPainter;
  end;

  TcxSchedulerExternalPainterClass = class of TcxSchedulerExternalPainter;

  { TcxSchedulerCustomResourceView }

  TcxSchedulerCustomResourceView = class(TcxSchedulerCustomView)
  private
    FAdapter: TcxCustomResourceViewAdapter;
    FExternalPainter: TcxSchedulerExternalPainter;
    FGroupingKind: TcxSchedulerGroupingKind;
    FEventImagesLayout: TcxSchedulerEventImagesLayout;
    FShowEndTime: Boolean;
    FShowTimeAsClock: Boolean;
    FOnCustomDrawBackground: TcxSchedulerCustomDrawBackgroundEvent;
    FOnCustomDrawButton: TcxSchedulerCustomDrawButtonEvent;
    FOnCustomDrawContent: TcxSchedulerCustomDrawContentEvent;
    FOnCustomDrawDayHeader: TcxSchedulerCustomDrawDayHeaderEvent;
    FOnCustomDrawEvent: TcxSchedulerCustomDrawEventEvent;
    FOnCustomDrawGroupSeparator: TcxSchedulerCustomDrawGroupSeparatorEvent;
    FOnCustomDrawNavigationButton: TcxSchedulerCustomDrawNavigationButtonEvent;
    FOnCustomDrawResourceHeader: TcxSchedulerCustomDrawResourceHeaderEvent;
    procedure CheckSelectOnRightClick;
    function GetViewInfo: TcxSchedulerCustomResourceViewViewInfo;
    function IsRelevantSelection(out ATime: TDateTime; out AResource: TcxSchedulerStorageResourceItem): Boolean;
    procedure SetEventImagesLayout(const AValue: TcxSchedulerEventImagesLayout);
    procedure SetShowEndTime(const AValue: Boolean);
    procedure SetShowTimeAsClock(const AValue: Boolean);
  protected
    procedure CalculateViewInfo; override;
    procedure CheckGroupingKind(AStyle: TcxSchedulerGroupingKind;
      var ActuallyStyle: TcxSchedulerGroupingKind); virtual;
    procedure CheckResourceNavigatorKind; override;
    procedure ClearCachedData; override;
    function CreateController: TcxSchedulerSubControlController; override;
    function CreateHitTest: TcxSchedulerSubControlHitTest; override;
    function CreateViewAdapter: TcxCustomResourceViewAdapter; virtual;
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;
    procedure DoCustomDrawBackground(
      AViewInfo: TcxSchedulerBackgroundCellViewInfo; var ADone: Boolean); virtual;
    procedure DoCustomDrawButton(
      AViewInfo: TcxSchedulerMoreEventsButtonViewInfo; var ADone: Boolean); virtual;
    procedure DoCustomDrawContent(
      AViewInfo: TcxSchedulerContentCellViewInfo; var ADone: Boolean); virtual;
    procedure DoCustomDrawDayHeader(
      AViewInfo: TcxSchedulerDayHeaderCellViewInfo; var ADone: Boolean); virtual;
    procedure DoCustomDrawEvent(
      AViewInfo: TcxSchedulerEventCellViewInfo; var ADone: Boolean); virtual;
    procedure DoCustomDrawGroupSeparator(
      AViewInfo: TcxSchedulerGroupSeparatorCellViewInfo; var ADone: Boolean); virtual;
    procedure DoCustomDrawNavigationButton(
      AViewInfo: TcxSchedulerContentNavigationButtonViewInfo; var ADone: Boolean); virtual;
    procedure DoCustomDrawResourceHeader(
      AViewInfo: TcxSchedulerHeaderCellViewInfo; var ADone: Boolean); virtual;
    procedure DoLayoutChanged; override;
    procedure DoMouseDown(AButton: TMouseButton; AShift: TShiftState; X, Y: Integer); override;
    function DoShowPopupMenu(X, Y: Integer): Boolean; override;
    function FindEventViewInfo(AEvent: TcxSchedulerEvent; const ADate: TDateTime;
      AResource: TcxSchedulerStorageResourceItem; var AViewInfo: TcxSchedulerEventCellViewInfo): Boolean;
    function GetCommonViewItemsPainter: IcxSchedulerCommonViewItemsPainter; virtual;
    function GetCompressWeekEnd: Boolean; virtual;
    function GetEditRectForEvent(AEvent: TcxSchedulerControlEvent; const ADate: TDateTime;
      AResource: TcxSchedulerStorageResourceItem): TRect; override;
    function GetEventHintText(AEvent: TcxSchedulerControlEvent): string; override;
    function GetEventVisibility(AEvent: TcxSchedulerControlEvent): Boolean; override;
    function GetGroupingKind: TcxSchedulerGroupingKind; override;
    function HasCustomDrawGroupSeparator: Boolean;
    function HasCustomDrawResourceHeader: Boolean;
    procedure InitScrollBarsParameters; override;
    function IsShowResources: Boolean; override;
    procedure LookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues); override;
    procedure Scroll(AScrollBarKind: TScrollBarKind;
      AScrollCode: TScrollCode; var AScrollPos: Integer); override;
    procedure SetGroupingKind(AValue: TcxSchedulerGroupingKind); virtual;

    property Adapter: TcxCustomResourceViewAdapter read FAdapter write FAdapter;
    property EventImagesLayout: TcxSchedulerEventImagesLayout read FEventImagesLayout write SetEventImagesLayout;
    property ExternalPainter: TcxSchedulerExternalPainter read FExternalPainter;
    property ShowEndTime: Boolean read FShowEndTime write SetShowEndTime default True;
    property ShowTimeAsClock: Boolean read FShowTimeAsClock write SetShowTimeAsClock default False;
    property ViewInfo: TcxSchedulerCustomResourceViewViewInfo read GetViewInfo;

    property OnCustomDrawBackground: TcxSchedulerCustomDrawBackgroundEvent read FOnCustomDrawBackground write FOnCustomDrawBackground;
    property OnCustomDrawButton: TcxSchedulerCustomDrawButtonEvent read FOnCustomDrawButton write FOnCustomDrawButton;
    property OnCustomDrawContent: TcxSchedulerCustomDrawContentEvent read FOnCustomDrawContent write FOnCustomDrawContent;
    property OnCustomDrawDayHeader: TcxSchedulerCustomDrawDayHeaderEvent read FOnCustomDrawDayHeader write FOnCustomDrawDayHeader;
    property OnCustomDrawEvent: TcxSchedulerCustomDrawEventEvent read FOnCustomDrawEvent write FOnCustomDrawEvent;
    property OnCustomDrawGroupSeparator: TcxSchedulerCustomDrawGroupSeparatorEvent read FOnCustomDrawGroupSeparator write FOnCustomDrawGroupSeparator;
    property OnCustomDrawNavigationButton: TcxSchedulerCustomDrawNavigationButtonEvent read FOnCustomDrawNavigationButton write FOnCustomDrawNavigationButton;
    property OnCustomDrawResourceHeader: TcxSchedulerCustomDrawResourceHeaderEvent read FOnCustomDrawResourceHeader write FOnCustomDrawResourceHeader;
  public
    constructor Create(AOwner: TcxCustomScheduler); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure InitializePainter; virtual;
    procedure FinilizePainter; virtual;

    property GroupingKind: TcxSchedulerGroupingKind read FGroupingKind write SetGroupingKind default gkDefault;
  end;

  { TcxCustomResourceViewAdapter }

  TcxCustomResourceViewAdapter = class(TcxIUnknownObject)
  private
    FView: TcxSchedulerCustomResourceView;

    function GetCells: TcxObjectList;
    function GetContentLineHeight: Integer;
    function GetPainterHelper: TcxSchedulerPainterHelperClass;
    function GetResourceNavigator: TcxSchedulerResourceNavigator;
    function GetResources: TcxSchedulerStorageResourceItems;
    function GetScheduler: TcxCustomScheduler;
    function GetViewInfo: TcxSchedulerCustomResourceViewViewInfo;
  protected
    function CanCalculate: Boolean; virtual;
    function GetActualStart: TDateTime; virtual;
    function GetActualFinish: TDateTime; virtual;
    function GetCompressWeekends: Boolean; virtual;
    function GetDontPrintFreeTime: Boolean; virtual;
    function GetDontPrintWeekEnds: Boolean; virtual;
    function GetHideSelection: Boolean; virtual;
    function GetIsPrinting: Boolean; virtual;
    function GetPageBounds: TRect; virtual;
    function GetPagesPerWeek: Byte; virtual;
    function GetPrimaryPage: Boolean; virtual;
    function GetPrintExactlyOneMonth: Boolean; virtual;
    function GetPrintRange(Index: Integer): TDateTime; virtual;
    function GetStylesAdapter: IcxSchedulerStylesAdapter; virtual;
    function GetView: TcxSchedulerCustomResourceView; virtual;

    procedure AfterCalculatePage; virtual;
    procedure BeforeCalculatePage; virtual;
    procedure DoInitialize(var ASelectedDays: TcxSchedulerDateList;
       var AEvents: TcxSchedulerCachedEventList); virtual;
    function GetPageHeaderText: string; virtual;
    procedure Store; virtual;
    procedure Restore; virtual;

    property Scheduler: TcxCustomScheduler read GetScheduler;
    property PainterHelper: TcxSchedulerPainterHelperClass read GetPainterHelper;
    property ResourceNavigator: TcxSchedulerResourceNavigator read GetResourceNavigator;
    property ViewInfo: TcxSchedulerCustomResourceViewViewInfo read GetViewInfo;
  public
    constructor Create(AView: TcxSchedulerCustomResourceView); virtual;

    procedure Calculate; overload;

    procedure GetPageResources(AResources: TcxObjectList); virtual;
    procedure GetPageDays(APageIndex: Integer; ADays: TcxSchedulerDateList);
    procedure Invalidate; virtual;

    property ActualStart: TDateTime read GetActualStart;
    property ActualFinish: TDateTime read GetActualFinish;
    property Cells: TcxObjectList read GetCells;
    property ContentLineHeight: Integer read GetContentLineHeight;
    property DontPrintFreeTime: Boolean read GetDontPrintFreeTime;
    property DontPrintWeekEnd: Boolean read GetDontPrintWeekEnds;
    property IsPrinting: Boolean read GetIsPrinting;
    property PagesPerWeek: Byte read GetPagesPerWeek;
    property PrintExactlyOneMonth: Boolean read GetPrintExactlyOneMonth;
    property PrintFrom: TDateTime index 0 read GetPrintRange;
    property PrintTo: TDateTime index 1 read GetPrintRange;
    property Resources: TcxSchedulerStorageResourceItems read GetResources;
    property StylesAdapter: IcxSchedulerStylesAdapter read GetStylesAdapter;
    property View: TcxSchedulerCustomResourceView read GetView write FView;
  end;

  TcxCustomResourceViewAdapterClass = class of TcxCustomResourceViewAdapter;

  { TcxSchedulerEventImages }

  TcxSchedulerEventImageType = (eitReminder, eitRecurrence, eitCustomOccurrence, eitCustom);

  { TcxSchedulerEventImageItem }

  TcxSchedulerEventImageItem = class(TCollectionItem)
  private
    FAutoHide: Boolean;
    FImageType: TcxSchedulerEventImageType;
    FImageIndex: Integer;
    FImages: TCustomImageList;
    FVisible: Boolean;
    function GetHeight: Integer;
    function GetWidth: Integer;
  protected
    FBounds: TRect;
    FHidden: Boolean;

    procedure DoRightToLeftConversion(const AClientBounds: TRect);
  public
    constructor Create(Collection: TCollection); override;

    property AutoHide: Boolean read FAutoHide write FAutoHide;
    property Bounds: TRect read FBounds;
    property Height: Integer read GetHeight;
    property ImageIndex: Integer read FImageIndex;
    property ImageType: TcxSchedulerEventImageType read FImageType;
    property Images: TCustomImageList read FImages;
    property Visible: Boolean read FVisible write FVisible;
    property Width: Integer read GetWidth;
  end;

  { TcxSchedulerEventImages }

  TcxSchedulerEventImages = class(TCollection)
  private
    FEventImageGap: Integer;
    FForceVisibleHeight: Integer;
    FForceVisibleWidth: Integer;
    FImages: TCustomImageList;
    FLayout: TcxSchedulerEventImagesLayout;
    FItemHeight: Integer;
    FItemWidth: Integer;
    FTotalVisibleHeight: Integer;
    FTotalVisibleWidth: Integer;
    FTotalVisibleImageCount: Integer;
    FVisibleImageCount: Integer;
    function GetImageItem(AIndex: Integer): TcxSchedulerEventImageItem;
  protected
    function Calculate(const R: TRect): Integer;
    procedure CalculateForCols(AVisibleImages: TList; const ATopLeft: TPoint; AColCount: Integer);
    function CalculateImages(const R: TRect): Integer;
    function CalculateSingleColumnImages(const R: TRect): Integer;
    function CalculateSingleLineImages(const R: TRect; ARightLimit: Integer = 0): Integer;
    procedure DoRightToLeftConversion(const AClientBounds: TRect);
    function Offset(ADeltaX, ADeltaY: Integer): Integer;
    function CreateVisibleList: TList;
    procedure Init(AImages: TCustomImageList);
    procedure SetItemBounds(AItem: TcxSchedulerEventImageItem; ALeft, ATop: Integer);

    property ItemHeight: Integer read FItemHeight;
    property ItemWidth: Integer read FItemWidth;
    property ForceVisibleHeight: Integer read FForceVisibleHeight;
    property ForceVisibleWidth: Integer read FForceVisibleWidth;
    property TotalVisibleHeight: Integer read FTotalVisibleHeight;
    property TotalVisibleWidth: Integer read FTotalVisibleWidth;
    property TotalVisibleImageCount: Integer read FTotalVisibleImageCount;
    property VisibleImageCount: Integer read FVisibleImageCount;
  public
    constructor Create(ALayout: TcxSchedulerEventImagesLayout; AScaleFactor: TdxScaleFactor);
    function Add(AImageIndex: Integer; AutoHide: Boolean = True): TcxSchedulerEventImageItem;
    function AddStandardImage(AImageType: TcxSchedulerEventImageType;
      AutoHide: Boolean = True): TcxSchedulerEventImageItem;

    property Items[Index: Integer]: TcxSchedulerEventImageItem read GetImageItem; default;
    property Layout: TcxSchedulerEventImagesLayout read FLayout write FLayout;
  end;

  { TcxSchedulerCachedImage }

  TcxSchedulerCachedImage = class
  private
    FImage: TcxBitmap;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Invalidate;
    function IsValid(AItem: TcxSchedulerCustomViewInfoItem): Boolean; virtual;
    procedure Update(AItem: TcxSchedulerCustomViewInfoItem); virtual;

    property Image: TcxBitmap read FImage;
  end;

  { TcxSchedulerResourceHeaderCachedImage }

  TcxSchedulerResourceHeaderCachedImage = class(TcxSchedulerCachedImage)
  private
    FDisplayText: string;
    FDateTime: TDateTime;
    FResourceItem: TcxSchedulerStorageResourceItem;
  public
    function IsValid(AItem: TcxSchedulerCustomViewInfoItem): Boolean; override;
    procedure Update(AItem: TcxSchedulerCustomViewInfoItem); override;
  end;

  { TcxSchedulerResourceHeaderCachedImageList }

  TcxSchedulerResourceHeaderCachedImageList = class(TcxObjectList)
  public
    function Add(AItem: TcxSchedulerHeaderCellViewInfo): TcxSchedulerResourceHeaderCachedImage;
    function GetCacheForItem(AItem: TcxSchedulerHeaderCellViewInfo): TcxSchedulerResourceHeaderCachedImage;
  end;

  { TcxSchedulerImageCacheManager }

  TcxSchedulerImageCacheManager = class
  private
    FResourceHeaders:  TcxSchedulerResourceHeaderCachedImageList;
    FSeparator: TcxSchedulerCachedImage;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Invalidate; virtual;

    property ResourceHeaders: TcxSchedulerResourceHeaderCachedImageList read FResourceHeaders;
    property Separator: TcxSchedulerCachedImage read FSeparator;
  end;

  { TcxSchedulerCustomResourceViewViewInfo }

  TcxSchedulerCustomResourceViewViewInfo = class(TcxSchedulerCustomViewViewInfo, IcxMouseTrackingCaller)
  private
    FButtonBounds: TRect;
    FButtonTextRect: TRect;
    FButtonArrowRect: TRect;
    FContentNavigationIntervals: TObjectList;
    FContentNavigationWithoutResources: Boolean;
    FEventWithoutResourceCount: Integer;
    FImageCacheManager: TcxSchedulerImageCacheManager;
    FHotContentNavigationButton: TcxSchedulerContentNavigationButtonViewInfo;
    FHotMoreEventsButton: TcxSchedulerMoreEventsButtonViewInfo;
    procedure AddContentNavigationIntervalItem(AResourceIndex: Variant);
    procedure ClearContentNavigationIntervals;
    function GetClones: TcxSchedulerEventList;
    function GetCorrectColumnHeight(AColumnRect: TRect): Integer;
    function GetDay(AIndex: Integer): TDateTime;
    function GetDayCount: Integer;
    function GetGroupByDate: Boolean;
    function GetGroupByResource: Boolean;
    function GetHasSeparator: Boolean;
    function GetHasVisibleBounds: Boolean;
    function GetHiddenSelection: Boolean;
    function GetMinResourceHeight: Integer;
    function GetMoreEventsButtonCount(AResourceIndex: Integer): Integer;
    function GetResource(AIndex: Integer): TcxSchedulerResourceViewInfo;
    function GetResourceCaption(AIndex: Integer): string;
    function GetResourceCount: Integer;
    function GetResourceHeaders: TcxSchedulerResourceHeaders;
    function GetPrintRange(Index: Integer): TDateTime;
    function GetSeparatorWidth: Integer;
    function GetView: TcxSchedulerCustomResourceView;
    function IsDrawContentNavigationButtons: Boolean;
    procedure SetHitContentNavigationButton(AButton: TcxSchedulerContentNavigationButtonViewInfo;
      AHitTest: TcxSchedulerCustomResourceViewHitTest);
    procedure SetHitMoreEventsButton(AButton: TcxSchedulerMoreEventsButtonViewInfo;
      AHitTest: TcxSchedulerCustomResourceViewHitTest);
    procedure SetHotContentNavigationButton(AButton: TcxSchedulerContentNavigationButtonViewInfo);
    procedure SetHotMoreEventsButton(AButton: TcxSchedulerMoreEventsButtonViewInfo);
  protected
    FAdapter: TcxCustomResourceViewAdapter;
    FButtons: TcxSchedulerViewInfoCellList;
    FCanSelected: Boolean;
    FCells: TcxObjectList;
    FContentCells: TcxSchedulerViewInfoCellList;
    FContentFontHeight: Integer;
    FContentLineHeight: Integer;
    FDayBorderColor: TColor;
    FDayHeaderCells: TcxSchedulerViewInfoCellList;
    FDayHeaderHeight: Integer;
    FEventCells: TcxSchedulerEventCellViewInfoList;
    FEventImages: IcxSchedulerEventImages;
    FGroupSeparatorCells: TcxSchedulerViewInfoCellList;
    FGroupSeparatorParams: TcxViewParams;
    FGroupingKind: TcxSchedulerGroupingKind;
    FHasVisibleBounds: Boolean;
    FHeaderContainerCells: TcxSchedulerViewInfoCellList;
    FHideClones: Boolean;
    FHideSelection: Boolean;
    FHideSource: Boolean;
    FNavigationButtons: TcxSchedulerViewInfoCellList;
    FResources: TcxObjectList;
    FResourceBounds: array of TRect;
    FResourceHeaderCells: TcxSchedulerViewInfoCellList;
    FResourceHeaderHeight: Integer;
    FResourceImages: TCustomImageList;
    FSelectionParams: TcxViewParams;
    FSelStart: TDateTime;
    FSelFinish: TDateTime;
    FSelResource: TcxSchedulerResourceViewInfo;
    FStylesAdapter: IcxSchedulerStylesAdapter;
    FTimeLineParams: TcxViewParams;
    // for page splitting
    FPageBounds: TRect;
    FPagesPerWeek: Byte;
    FPrimaryPage: Boolean;
    FPrintWeekEnds: Boolean;
    FUseResourceImages: Boolean;
    ScreenCanvas: TcxCanvas;

    // IcxMouseTrackingCaller
    procedure MouseLeave; virtual;

    function AddBackgroundSlot(const ABounds: TRect; ABorders: TcxBorders;
      const AText: string = ''): TcxSchedulerBackgroundSlotCellViewInfo; virtual;
    function AddButton(ABounds: TRect; const ADateTime: TDateTime;
      AIsDown: Boolean; AEvent: TcxSchedulerEvent): TcxSchedulerMoreEventsButtonViewInfo; virtual;
    function AddContentCell(const ARect: TRect; const AStart, AFinish: TDateTime;
      AResourceIndex: Integer): TcxSchedulerContentCellViewInfo; virtual;
    procedure AddContentNavigationButton(const AColumnRect: TRect; AResourceIndex: Integer;
      AColumnPositionInResource: TcxSchedulerColumnPositionInResource);
    function AddDayHeader(const ADate: TDateTime; const ABounds: TRect; AResourceIndex: Integer;
      ADisplayMode: TcxSchedulerDayHeaderModernStyleDisplayMode): TcxSchedulerDayHeaderCellViewInfo; overload; virtual;
    function AddDayHeader(const ADate: TDateTime; ATop, ALeft, ARight: Integer; AResourceIndex: Integer;
      ADisplayMode: TcxSchedulerDayHeaderModernStyleDisplayMode): TcxSchedulerDayHeaderCellViewInfo; overload;
    function AddEventCell(
      AViewData: TcxSchedulerEventViewData; AImmediateCalculate: Boolean = True): TcxSchedulerEventCellViewInfo; virtual;
    function AddGroupHorzSeparator(APos: Integer): TcxSchedulerGroupSeparatorCellViewInfo; virtual;
    function AddGroupSeparator(const ABounds: TRect): TcxSchedulerGroupSeparatorCellViewInfo; virtual;
    function AddGroupVertSeparator(APos: Integer): TcxSchedulerGroupSeparatorCellViewInfo; virtual;
    procedure AddResourceBounds(AResourceIndex: Integer; const ABounds: TRect);
    function AddResourceHeader(const AIndex: Integer;
      const ABounds: TRect): TcxSchedulerHeaderCellViewInfo; virtual;
    procedure AfterCalculate; override;
    function AreThereEventsInVisibleInterval(AResourceIndex: Integer): Boolean; virtual;
    procedure AssignResourceID(ACell: TcxSchedulerCustomResourceViewInfoItem; AIndex: Integer);
    procedure CalculateContentNavigationButtons; virtual;
    procedure CalculateMetrics; virtual;
    procedure CalculateNavigationButtonParams(AColumnRect: TRect; AKind: TcxSchedulerContentNavigationButtonKind;
      out AButtonWidth: Integer);
    procedure CalculateResourceHeadersAutoHeight(AWidth: Integer); virtual;
    function CalculateResourceHeaderWidth: Integer; virtual;
    function CanCalculateNavigationButtons: Boolean; virtual;

    function CanCacheGroupSeparator(AItem: TcxSchedulerGroupSeparatorCellViewInfo): Boolean; virtual;
    function CanCacheResourceHeader(AItem: TcxSchedulerHeaderCellViewInfo): Boolean; virtual;

    procedure CheckResourceNavigatorKind; override;

    procedure Clear; override;
    procedure ClearResourceBounds;
    function ContentCellClass: TcxSchedulerContentCellViewInfoClass; virtual;
    procedure CreateCellInstance(AClass: TcxSchedulerCustomViewInfoItemClass;
      const ABounds: TRect; const AViewParams: TcxViewParams; AUseRightToLeftAlignment: Boolean; var Instance); overload;
    procedure CreateCellInstance(AClass: TcxSchedulerCustomViewInfoItemClass;
      const ABounds, AVisibleBounds: TRect; const AViewParams: TcxViewParams; AUseRightToLeftAlignment: Boolean; var Instance); overload;
    function CreateEventViewData(AEvent: TcxSchedulerControlEvent; const ABounds: TRect;
      const AStart, AFinish: TDateTime; AResource: TcxSchedulerResourceViewInfo): TcxSchedulerEventViewData; overload; virtual;
    function CreateEventViewData(AEvent: TcxSchedulerControlEvent; const ABounds: TRect;
      const AStart, AFinish: TDateTime; AResource: TcxSchedulerResourceViewInfo;
      const ACurrentDate: TDateTime): TcxSchedulerEventViewData; overload; virtual;
    function CreateEventCellViewInfo(AViewData: TcxSchedulerEventViewData; ACanResize: Boolean): TcxSchedulerEventCellViewInfo; virtual;
    function CreateImageCacheManager: TcxSchedulerImageCacheManager; virtual;
    function DayHeaderClass: TcxSchedulerDayHeaderCellViewInfoClass; virtual;
    procedure DoCalculate; override;
    procedure DoContentNavigationButtonClick(Sender: TcxSchedulerContentNavigationButtonViewInfo); virtual;
    procedure DoMoreEventsButtonClick(Sender: TcxSchedulerMoreEventsButtonViewInfo); virtual;
    procedure DoRightToLeftConversion(const AClientBounds: TRect); virtual;
    procedure DrawNavigationButtons(ACanvas: TcxCanvas; ADrawItemProc: TcxSchedulerCustomDrawItemProc);
    procedure ExtractResources; virtual;
    function EventCellClass: TcxSchedulerEventCellViewInfoClass; virtual;
    function GetBounds: TRect; override;
    function GetContentNavigationInterval(AContentNavigationButton: TcxSchedulerContentNavigationButtonViewInfo): TDateTime;
    function GetContentParams(const ATime: TDateTime;
      AResource: TcxSchedulerResourceViewInfo): TcxViewParams; virtual;
    function GetDayHeaderHeight: Integer; virtual;
    function GetEventForResourceCount(AResourceIndex: Integer; out AResourceID: Variant): Integer; virtual;
    function GetEventWithoutResourceCount: Integer; virtual;
    function GetGroupingKind: TcxSchedulerGroupingKind; virtual;
    function GetMoreEventButtonClass: TcxSchedulerMoreEventsButtonViewInfoClass; virtual;
    function GetResourceHeaderWidth: Integer;
    function GetResourceImagesSize: TSize; virtual;
    function GetResourcesContentWidth: Integer; virtual;
    function GetScaleUnit: TDateTime; virtual;
    function GetSelectionParams(const AParams: TcxViewParams): TcxViewParams;
    function GetSeparatorCount: Integer; virtual;
    function GetShowEventsWithoutResource: Boolean; virtual;
    function GetTimeLineParams: TcxViewParams; virtual;
    function GetActualFont(AStyle: TcxStyle): TFont;
    function GetFontHeight(AStyle: TcxStyle): Integer; overload;
    function GetFontHeight(const AParams: TcxViewParams): Integer; overload;
    function GetPageHeaderText: string; virtual;
    function GetResourceItemByIndex(AIndex: Integer): TcxSchedulerStorageResourceItem;
    function GetResourceViewInfoByItem(AItem: TcxSchedulerStorageResourceItem;
      var ResourceViewInfoIndex: Integer): TcxSchedulerResourceViewInfo;
    function GetStartDate(Index: Integer): TDateTime; virtual;
    function HasClone(AEvent: TcxSchedulerEvent): Boolean;
    function HasStorage: Boolean;
    function HasVisibleEvents: Boolean; virtual;
    procedure HideCloneEventsOnDragDrop; virtual;
    procedure HideSourceEventsOnDragDrop; virtual;
    procedure HotTrackMoreEventsButton(AHitTest: TcxSchedulerCustomResourceViewHitTest);
    procedure HotTrackNavigationButtons(AHitTest: TcxSchedulerCustomResourceViewHitTest);
    function IsNavigationButtonsVertical: Boolean; virtual;
    function IsTimeSelected(ATime: TDateTime; AResource: TObject): Boolean; virtual;
    function IsValidNavigationButtonsPlace(const AResourceRect: TRect): Boolean;
    procedure MakeTimeVisible(const ATime: TDateTime); virtual;
    function NavigationButtonOffset(AKind: TcxSchedulerContentNavigationButtonKind;
      AResourceIndex: Integer): Integer; virtual;
    function NeedClearResources(AGroupingKind: TcxSchedulerGroupingKind): Boolean; virtual;
    procedure OnContentNavigationButtonClick(Sender: TcxSchedulerContentNavigationButtonViewInfo); virtual;
    procedure OnMoreEventsButtonClick(Sender: TcxSchedulerMoreEventsButtonViewInfo);
    procedure ProcessDateToDisplayText(ArrangeByType: Boolean = False); virtual;
    procedure ProcessCheckBorders(AList: TcxSchedulerViewInfoCellList;
      AHasCommonArea: Boolean; ANeighbors: TcxNeighbors = [];
      AExcludeBorders: TcxBorders = []; AAddBorders: TcxBorders = []);
    procedure ReturnVisibleInterval(var AStart, AEnd: TDateTime); virtual;
    function SetAdapter(Adapter: TcxCustomResourceViewAdapter): TcxCustomResourceViewAdapter;
    procedure SetContentNavigationButtonsIntervals; virtual;
    procedure SetResourceHasVisibleEvent(AEvent: TcxSchedulerControlEvent);
    procedure SetResourceTextRect(AResource: TcxSchedulerStorageResourceItem; const ARect: TRect);
    function UseRightToLeftAlignment: Boolean; override;

    property HotContentNavigationButton: TcxSchedulerContentNavigationButtonViewInfo
      read FHotContentNavigationButton write SetHotContentNavigationButton;
    property HotMoreEventsButton: TcxSchedulerMoreEventsButtonViewInfo read FHotMoreEventsButton write SetHotMoreEventsButton;
    property ImageCacheManager: TcxSchedulerImageCacheManager read FImageCacheManager;
    property StartDates[AnIndex: Integer]: TDateTime read GetStartDate;
    property View: TcxSchedulerCustomResourceView read GetView;
  public
    constructor Create(AOwner: TcxSchedulerSubControl); override;
    destructor Destroy; override;
    procedure Calculate; override;
    procedure CalculateHitTest(AHitTest: TcxSchedulerCustomResourceViewHitTest); virtual;
    procedure InitScrollBarsParameters; virtual;
    procedure ScrollHorizontal(AScrollCode: TScrollCode; var AScrollPos: Integer); virtual;
    procedure ScrollVertical(AScrollCode: TScrollCode; var AScrollPos: Integer); virtual;
    procedure SetEventsVisibility(AShowSources, AShowClones: Boolean; AForceRepaint: Boolean = False); override;

    property Adapter: TcxCustomResourceViewAdapter read FAdapter;
    property Buttons: TcxSchedulerViewInfoCellList read FButtons;
    property CanSelected: Boolean read FCanSelected;
    property Clones: TcxSchedulerEventList read GetClones;
    property ContentCells: TcxSchedulerViewInfoCellList read FContentCells;
    property ContentLineHeight: Integer read FContentLineHeight;
    property DayCount: Integer read GetDayCount;
    property DayHeaderCells: TcxSchedulerViewInfoCellList read FDayHeaderCells;
    property DayHeaderHeight: Integer read GetDayHeaderHeight;
    property Days[Index: Integer]: TDateTime read GetDay;
    property EventCells: TcxSchedulerEventCellViewInfoList read FEventCells;
    property ShowEventsWithoutResource: Boolean read GetShowEventsWithoutResource;
    property GroupByDate: Boolean read GetGroupByDate;
    property GroupByResource: Boolean read GetGroupByResource;
    property GroupSeparatorCells: TcxSchedulerViewInfoCellList read FGroupSeparatorCells;
    property GroupSeparatorParams: TcxViewParams read FGroupSeparatorParams;
    property GroupingKind: TcxSchedulerGroupingKind read FGroupingKind;
    property HasSeparator: Boolean read GetHasSeparator;
    property HasVisibleBounds: Boolean read FHasVisibleBounds;
    property HeaderContainerCells: TcxSchedulerViewInfoCellList read FHeaderContainerCells;
    property HideSelection: Boolean read FHideSelection;
    property NavigationButtons: TcxSchedulerViewInfoCellList read FNavigationButtons;
    property PagesPerWeek: Byte read FPagesPerWeek;
    property PrimaryPage: Boolean read FPrimaryPage;
    property PrintFrom: TDateTime index 0 read GetPrintRange;
    property PrintTo: TDateTime index 1 read GetPrintRange;
    property PrintWeekEnds: Boolean read FPrintWeekEnds;
    property Resources[AIndex: Integer]: TcxSchedulerResourceViewInfo read GetResource;
    property ResourceCaptions[AIndex: Integer]: string read GetResourceCaption;
    property ResourceCount: Integer read GetResourceCount;
    property ResourceHeaders: TcxSchedulerResourceHeaders read GetResourceHeaders;
    property ResourceHeaderCells: TcxSchedulerViewInfoCellList read FResourceHeaderCells;
    property ResourceHeaderHeight: Integer read FResourceHeaderHeight;
    property ResourceImages: TCustomImageList read FResourceImages;
    property SelFinish: TDateTime read FSelFinish;
    property SelResource: TcxSchedulerResourceViewInfo read FSelResource;
    property SelStart: TDateTime read FSelStart;
    property SeparatorWidth: Integer read GetSeparatorWidth;
    property StylesAdapter: IcxSchedulerStylesAdapter read FStylesAdapter;
    property HiddenSelection: Boolean read GetHiddenSelection;
    property UseResourceImages: Boolean read FUseResourceImages;
  end;

  { TcxSchedulerCustomResourceViewNavigation }

  TcxSchedulerCustomResourceViewNavigation = class(TcxSchedulerViewNavigation)
  private
    function GetIsFirstResource: Boolean;
    function GetIsLastResource: Boolean;
    function GetGroupingKind: TcxSchedulerGroupingKind;
    function GetResourceObject: TcxSchedulerResourceViewInfo;
    function GetViewInfo: TcxSchedulerCustomResourceViewViewInfo;
  protected
    function GetNextResource(AGoToForward: Boolean): TcxSchedulerStorageResourceItem;
    function GetResourceItem: TcxSchedulerStorageResourceItem; override;
    function GetResourceFromViewInfo(AInfo: TcxSchedulerResourceViewInfo): TcxSchedulerStorageResourceItem;
    function RoundTime(const ADateTime: TDateTime): TDateTime;
  public
    procedure ValidateSelection(var ASelStart, ASelFinish: TDateTime;
      var AResource: TcxSchedulerStorageResourceItem); override;
    property IsFirstResource: Boolean read GetIsFirstResource;
    property IsLastResource: Boolean read GetIsLastResource;
    property GroupingKind: TcxSchedulerGroupingKind read GetGroupingKind;
    property ResourceItem: TcxSchedulerStorageResourceItem read GetResourceItem;
    property ResourceObject: TcxSchedulerResourceViewInfo read GetResourceObject;
    property ViewInfo: TcxSchedulerCustomResourceViewViewInfo read GetViewInfo;
  end;

  { TcxSchedulerCustomResourceViewController }

  TcxSchedulerCustomResourceViewController = class(TcxSchedulerViewController)
  private
    FCalculatedHintBounds: Boolean;
    FHintText: string;
    FNavigationButtonClicked: Boolean;
    function GetHintController: TcxSchedulerHintController;
    function GetHitTest: TcxSchedulerCustomResourceViewHitTest;
    function GetView: TcxSchedulerCustomResourceView;
  protected
    function CreateNavigation: TcxSchedulerViewNavigation; override;
    function GetCursor(X, Y: Integer): TCursor; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseLeave; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure NavigationButtonDownCustomProcessing; virtual;
    // hint
    function GetDayHeaderDate: TDateTime; virtual;
    procedure InternalHideHint;
    procedure InternalShowHint(P: TPoint; const AHintText: string);
    function NeedShowDayHeaderHint: Boolean; virtual;
    function ShowHint: Boolean; virtual;
    procedure ShowDayHeaderHint; virtual;

    property HintController: TcxSchedulerHintController read GetHintController;
    property HitTest: TcxSchedulerCustomResourceViewHitTest read GetHitTest;
    property NavigationButtonClicked: Boolean read FNavigationButtonClicked write FNavigationButtonClicked;
    property View: TcxSchedulerCustomResourceView read GetView;
  end;

  { TcxSchedulerCustomResourceViewHitTest }

  TcxSchedulerCustomResourceViewHitTest = class(TcxSchedulerViewHitTest)
  private
    function GetContentCell: TcxSchedulerContentCellViewInfo;
    function GetHitContentNavigationButton: TcxSchedulerContentNavigationButtonViewInfo;
    function GetHitMoreEventsButton: TcxSchedulerMoreEventsButtonViewInfo;
    function GetHeaderCell: TcxSchedulerHeaderCellViewInfo;
    function GetView: TcxSchedulerCustomResourceView;
  protected
    FButton: TcxSchedulerMoreEventsButtonViewInfo;
    FContentNavigationButton: TcxSchedulerContentNavigationButtonViewInfo;
    FDragKind: TcxEventDragKind;
    FEventCell: TcxSchedulerEventCellViewInfo;
    FHitObject: TObject;
    FHitObjectBounds: TRect;
    FTimeZone: Integer;
    function CanMoveEvent(AEvent: TcxSchedulerControlEvent): Boolean; virtual;
    function CanResizeEvent(AEvent: TcxSchedulerControlEvent): Boolean; virtual;
    procedure Clear; override;
    procedure DoCalculate; override;
    function GetHitEvent: TcxSchedulerControlEvent; override;
    procedure SetHitTime(AItemFlag: Integer; const ATime: TDateTime); virtual;
    procedure SetResource(AResource: TcxSchedulerResourceViewInfo);
    procedure ValidateDragKind; virtual;

    property View: TcxSchedulerCustomResourceView read GetView;
    property HitAtTimeZoneLabel: Boolean index htcTimeZoneLabel read GetBitState;
    property HitContentNavigationButton: TcxSchedulerContentNavigationButtonViewInfo read GetHitContentNavigationButton;
    property HitMoreEventsButton: TcxSchedulerMoreEventsButtonViewInfo read GetHitMoreEventsButton;
    property TimeZone: Integer read FTimeZone;
  public
    function GetDragKind: TcxEventDragKind; override;
    procedure SetDragKind(AValue: TcxEventDragKind);

    property Button: TcxSchedulerMoreEventsButtonViewInfo read FButton;
    property ContentCell: TcxSchedulerContentCellViewInfo read GetContentCell;
    property ContentNavigationButton: TcxSchedulerContentNavigationButtonViewInfo read FContentNavigationButton;
    property EventCell: TcxSchedulerEventCellViewInfo read FEventCell;
    property HeaderCell: TcxSchedulerHeaderCellViewInfo read GetHeaderCell;
    property HitAtButton: Boolean index htcButton read GetBitState;
    property HitAtContent: Boolean index htcContent read GetBitState;
    property HitAtContentNavigationButton: Boolean index htcNavigationButton read GetBitState;
    property HitAtDayHeader: Boolean index htcDayHeader read GetBitState;
    property HitAtGroupSeparator: Boolean index htcGroupSeparator read GetBitState;
    property HitAtResourceHeader: Boolean index htcResourceHeader read GetBitState;
  end;

 { TcxSchedulerCustomViewPainter }

  TcxSchedulerCustomViewPainter = class(TcxSchedulerSubControlPainter)
  private
    function GetView: TcxSchedulerCustomResourceView;
  protected
    FPainter: IcxSchedulerCommonViewItemsPainter;
    procedure DrawBackgroundCell(AItem: TcxSchedulerBackgroundCellViewInfo); virtual;
    procedure DrawButtonCell(AItem: TcxSchedulerCustomViewInfoItem; var ADone: Boolean); virtual;
    procedure DrawContentCell(AItem: TcxSchedulerCustomViewInfoItem; var ADone: Boolean); virtual;
    procedure DrawEventCell(AItem: TcxSchedulerCustomViewInfoItem; var ADone: Boolean); virtual;
    procedure DrawGroupSeparatorCell(AItem: TcxSchedulerCustomViewInfoItem; var ADone: Boolean); virtual;
    procedure DrawHeaderCell(AItem: TcxSchedulerCustomViewInfoItem; var ADone: Boolean); virtual;
    procedure DrawResourceHeaderCell(AItem: TcxSchedulerCustomViewInfoItem; var ADone: Boolean); virtual;
  public
    constructor Create(AOwner: TcxSchedulerSubControl); override;
    procedure InitializePainter; override;
    procedure Paint; override;

    property Painter: IcxSchedulerCommonViewItemsPainter read FPainter;
    property View: TcxSchedulerCustomResourceView read GetView;
  end;

  { TcxSchedulerEventPlace }

  TcxSchedulerEventPlace = class
  public
    ColFinish: Integer;
    ColStart: Integer;
    Resource: TObject;
    Data: TObject;
    Event: TcxSchedulerEvent;
    LineFinish: Integer;
    LineStart: Integer;
    procedure AlignPosition(APlace: TcxSchedulerEventPlace);
    function IntersectHorz(APlace: TcxSchedulerEventPlace): Boolean; inline;
    function IntersectVert(APlace: TcxSchedulerEventPlace): Boolean; inline;
    procedure LineStartToEvent;
    procedure ResetPosition;
  end;

  { TcxSchedulerEventLayoutBuilder }

  TcxSchedulerEventLayoutBuilderGetEventPlaceProc = function(Sender: TcxSchedulerEventLayoutBuilder;
    AEvent: TcxSchedulerControlEvent; out AStartCol, AFinishCol: Integer; out ALineCount: Integer): Boolean of object;

  TcxSchedulerEventLayoutBuilder = class
  strict private
    FEventPlaces: TcxObjectList;

    function GetEventPlace(AIndex: Integer): TcxSchedulerEventPlace;
    function GetEventPlaceCount: Integer;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function AddEventPlace(AEvent: TcxSchedulerEvent; AStartCol, AFinishCol: Integer;
      ALineCount: Integer = 1; AResource: TObject = nil): TcxSchedulerEventPlace;
    procedure Clear;
    procedure Calculate(const ASchedulerViewStyle: TcxSchedulerViewStyle = svsClassic); virtual;
    procedure CalculateEx(AEventsList: TcxSchedulerFilteredEventList;
      APlaceInfoProc: TcxSchedulerEventLayoutBuilderGetEventPlaceProc);

    property EventPlaceCount: Integer read GetEventPlaceCount;
    property EventPlaces[Index: Integer]: TcxSchedulerEventPlace read GetEventPlace;
    property EventPlacesList: TcxObjectList read FEventPlaces;
  end;

var
  ExternalPainterClass: TcxSchedulerExternalPainterClass = TcxSchedulerExternalPainter;

function MakeTextOutcxFlags(AlignHorz: TAlignment;
  AlignVert: TcxAlignmentVert; AMultiline: Boolean = False): Integer;

implementation

uses
  cxDrawTextUtils, cxLibraryConsts, cxVariants, cxSchedulerDateNavigator, dxCore, cxFormats, dxDPIAwareUtils, dxOffice11,
  dxCoreGraphics, dxTypeHelpers;

type
  TcxCustomEditStyleAccess = class(TcxCustomEditStyle);
  TcxCustomSchedulerAccess = class(TcxCustomScheduler);
  TcxDateNavigatorAccess = class(TcxSchedulerDateNavigator);
  TcxSchedulerControlEventAccess = class(TcxSchedulerControlEvent);
  TcxSchedulerExternalPainterAccess = class(TcxSchedulerExternalPainter);
  TcxSchedulerSplitterAccess = class(TcxSchedulerSplitter);
  TcxSchedulerSplitterControllerAccess = class(TcxSchedulerSplitterController);

const
  ContentNavigationButtonWidth = 24;
  ContentNavigationButtonHeight = 172;
  ContentNavigationButtonReducedHeight = 32;
  MultiLines: array[Boolean] of Integer = (0, cxWordBreak or cxNoFullWidthCharBreak);
  cxDrawTextFlags: array[Boolean] of Cardinal = (cxAlignLeft or cxAlignVCenter or cxSingleLine,
    cxAlignLeft or cxAlignTop or cxWordBreak or cxDontBreakChars or cxNoFullWidthCharBreak);

function MakeTextOutcxFlags(AlignHorz: TAlignment; AlignVert: TcxAlignmentVert; AMultiline: Boolean = False): Integer;
begin
  Result := cxAlignmentsHorz[AlignHorz] or cxDontBreakChars or
    cxAlignmentsVert[AlignVert] or MultiLines[AMultiline];
end;

function MakeRect(ALeft, ATop: Integer; ASize: TSize): TRect;
begin
  Result.Left := ALeft;
  Result.Top := ATop;
  Result.Right := ALeft + ASize.cx;
  Result.Bottom := ATop + ASize.cy;
end;

function GetTrueMultilineTextHeight(ACanvas: TcxCanvas; const S: string; ABounds: TRect; ATextFlags: Cardinal): Integer;
var
  ARowCount: Integer;
  ATextParams: TcxTextParams;
  ATextRows: TcxTextRows;
begin
  ABounds.Bottom := 30000;
  ATextParams := cxCalcTextParams(ACanvas.Canvas, ATextFlags);
  cxMakeTextRows(ACanvas.Canvas, PChar(S), Length(S), ABounds, ATextParams, ATextRows, ARowCount);
  Result := ATextParams.RowHeight * Max(ARowCount, 1);
  cxResetTextRows(ATextRows);
end;

{ TcxSchedulerViewInfoCellList }

function TcxSchedulerViewInfoCellList.CalculateHitTest(AHitTest: TcxSchedulerCustomResourceViewHitTest): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := Count - 1 downto 0  do
    if Items[I].GetHitTest(AHitTest, Result) and Result then Exit;
end;

procedure TcxSchedulerViewInfoCellList.Clear;
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    Items[I].Free;
  inherited Clear;
end;

procedure TcxSchedulerViewInfoCellList.DoRightToLeftConversion(const AClientBounds: TRect);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].DoRightToLeftConversion(AClientBounds);
end;

procedure TcxSchedulerViewInfoCellList.Draw(ACanvas: TcxCanvas; ADrawItemProc: TcxSchedulerCustomDrawItemProc);
var
  I: Integer;
  ADone: Boolean;
  AItem: TcxSchedulerCustomViewInfoItem;
  ASavedFont: TFont;
begin
  for I := 0 to Count - 1 do
  begin
    AItem := Items[I];
    if AItem.FVisible and not cxRectIsEmpty(AItem.ActiveBounds) and ACanvas.RectVisible(AItem.ActiveBounds) then
    begin
      ASavedFont := AItem.FViewParams.Font;
      AItem.BeforeCustomDraw(ACanvas);
      ADone := False;
      AItem.FCanvas := ACanvas;
      if AItem.FHasClipping then
      begin
        AItem.ClippingCreate(AItem.FHasClipping);
        ADrawItemProc(AItem, ADone);
        AItem.ClippingRestore;
      end
      else
        ADrawItemProc(AItem, ADone);
      AItem.AfterCustomDraw(ACanvas);
      if not ADone then
        AItem.Draw(ACanvas);
      AItem.FViewParams.Font := ASavedFont;
    end;
  end;

end;

function TcxSchedulerViewInfoCellList.GetItem(AIndex: Integer): TcxSchedulerCustomViewInfoItem;
begin
  Result := List[AIndex];
end;

{ TcxSchedulerCustomViewInfoItem }

constructor TcxSchedulerCustomViewInfoItem.Create(APainter: TcxCustomLookAndFeelPainter; const ABounds, AVisibleRect: TRect;
  const AViewParams: TcxViewParams; AScaleFactor: TdxScaleFactor; AUseRightToLeftAlignment: Boolean);
begin
  FPainter := APainter;
  FDateTime := NullDate;
  FViewParams := AViewParams;
  FVisibleRect := AVisibleRect;
  FScaleFactor := AScaleFactor;
  FUseRightToLeftAlignment := AUseRightToLeftAlignment;
  CalculateCellBounds(ABounds, AVisibleRect);
end;

destructor TcxSchedulerCustomViewInfoItem.Destroy;
begin
  FClipRgn.Free;
  inherited Destroy;
end;

procedure TcxSchedulerCustomViewInfoItem.Draw(ACanvas: TcxCanvas);
var
  ATransparent: Boolean;
  APrevCanvas: TcxCanvas;
  APrevParams: TcxViewParams;
begin
  if not FVisible then Exit;
  ATransparent := Transparent;
  APrevCanvas := FCanvas;
  ACanvas.GetParams(APrevParams);
  if Cache <> nil then
  begin
    if not Cache.IsValid(Self) then
    begin
      Cache.Update(Self);
      UpdateCachedImage(APrevParams);
    end;
    ACanvas.Draw(Bounds.Left, Bounds.Top, Cache.Image);
  end
  else
  begin
    FCanvas := ACanvas;
    DoDraw;
  end;
  ACanvas.SetParams(APrevParams);
  FCanvas := APrevCanvas;
  Transparent := ATransparent;
end;

procedure TcxSchedulerCustomViewInfoItem.CalculateCellBounds(const ABounds, AVisibleRect: TRect);
begin
  FBounds := ABounds;
  FClipRect := CalculateClipRect(ABounds, AVisibleRect);
  FVisible := not cxRectIsEmpty(FClipRect);
  FHasClipping := FVisible and not cxRectIsEqual(FClipRect, ABounds);
end;

function TcxSchedulerCustomViewInfoItem.CalculateClipRect(const ABounds, AVisibleRect: TRect): TRect;
begin
  if not cxRectIntersect(Result, ABounds, AVisibleRect) then
    Result := cxNullRect;
end;

procedure TcxSchedulerCustomViewInfoItem.ClippingCreate(AHasClipping: Boolean);
begin
  Inc(FClipRef);
  if not AHasClipping or (FClipRef > 1) then Exit;
  if FClipRgn = nil then
    FClipRgn := TcxRegion.Create(FClipRect);
  TuneClipping(FClipRgn);
  FSavedClipRgn := Canvas.GetClipRegion;
  Canvas.SetClipRegion(FClipRgn, roIntersect, False);
end;

procedure TcxSchedulerCustomViewInfoItem.ClippingRestore;
begin
  Dec(FClipRef);
  if (FClipRef = 0) and (FSavedClipRgn <> nil) then
  begin
    Canvas.SetClipRegion(FSavedClipRgn, roSet, True);
    FSavedClipRgn := nil;
  end;
end;

procedure TcxSchedulerCustomViewInfoItem.DoRightToLeftConversion(const AClientBounds: TRect);
begin
  FBounds := TdxRightToLeftLayoutConverter.ConvertRect(FBounds, AClientBounds);
  FClipRect := TdxRightToLeftLayoutConverter.ConvertRect(FClipRect, AClientBounds);
  FBorders := TdxRightToLeftLayoutConverter.ConvertBorders(FBorders);
end;

procedure TcxSchedulerCustomViewInfoItem.DoRightToLeftConversionWithChecking(var ARect: TRect; const AClientBounds: TRect);
begin
  if not cxRectIsNull(ARect) then
    ARect := TdxRightToLeftLayoutConverter.ConvertRect(ARect, AClientBounds);
end;

function TcxSchedulerCustomViewInfoItem.DrawBackground(ACanvas: TcxCanvas; const ABounds: TRect): Boolean;
begin
  Result := Transparent or Assigned(Bitmap) and not Bitmap.Empty;
  if Result and not Transparent then
    ACanvas.FillRect(ABounds, Bitmap);
end;

procedure TcxSchedulerCustomViewInfoItem.DrawFrame(const ARect: TRect; ABorders: TcxBorders = [];
  ABorderColor: TColor = clDefault; ABorderSize: Integer = 1);
begin
  Canvas.FrameRect(ARect, ABorderColor, ABorderSize, ABorders);
end;

procedure TcxSchedulerCustomViewInfoItem.DrawRect(const ARect: TRect; ABorders: TcxBorders = [];
  ABorderColor: TColor = clDefault; ABorderSize: Integer = 1);
begin
  if Transparent then
    Canvas.FrameRect(ARect, ABorderColor, ABorderSize, ABorders)
  else
    Canvas.Rectangle(ARect, FViewParams, ABorders, ABorderColor, ABorderSize);
end;

procedure TcxSchedulerCustomViewInfoItem.DrawText(const ARect: TRect; const AText: string; AFlags: Integer;
  AFont: TFont = nil; AColor: TColor = clDefault);
begin
  if AFont = nil then
    Canvas.Font := FViewParams.Font
  else
    Canvas.Font := AFont;
  if AColor <> clDefault then
    Canvas.Font.Color := AColor;
  Canvas.Brush.Style := bsClear;
  Canvas.DrawTexT(AText, ARect, AFlags);
end;

function TcxSchedulerCustomViewInfoItem.GetActiveBounds: TRect;
begin
  Result := Bounds;
end;

function TcxSchedulerCustomViewInfoItem.GetActualRectRTL(const ARect, AClientBounds: TRect; AOffsetIfRTL: Integer = 0): TRect;
begin
  Result := ARect;
  if UseRightToLeftAlignment then
  begin
    Result := TdxRightToLeftLayoutConverter.ConvertRect(Result, AClientBounds);
    if AOffsetIfRTL <> 0 then
      OffsetRect(Result, AOffsetIfRTL, 0);
  end;
end;

function TcxSchedulerCustomViewInfoItem.GetBoundsForHitTest: TRect;
begin
  Result := FClipRect;
end;

function TcxSchedulerCustomViewInfoItem.GetFont: TFont;
begin
  Result := FViewParams.Font;
end;

function TcxSchedulerCustomViewInfoItem.GetHitTest(AHitTest: TcxSchedulerCustomResourceViewHitTest;
  var ABreak: Boolean): Boolean;
begin
  Result := FVisible and PtInRect(GetBoundsForHitTest, AHitTest.HitPoint);
  ABreak := Result;
  if Result then
    InitHitTest(AHitTest);
end;

procedure TcxSchedulerCustomViewInfoItem.InitHitTest(AHitTest: TcxSchedulerCustomResourceViewHitTest);
begin
  AHitTest.FHitObject := Self;
end;

function TcxSchedulerCustomViewInfoItem.IsDrawBySkin: Boolean;
begin
  Result := ExternalPainter.Painter.LookAndFeelStyle = lfsSkin;
end;

procedure TcxSchedulerCustomViewInfoItem.TuneClipping(var AClipRgn: TcxRegion);
begin
//
end;

// custom draw support
procedure TcxSchedulerCustomViewInfoItem.AfterCustomDraw(ACanvas: TcxCanvas);
begin
  ACanvas.GetParams(FViewParams);
end;

procedure TcxSchedulerCustomViewInfoItem.BeforeCustomDraw(ACanvas: TcxCanvas);
begin
  ACanvas.SetParams(FViewParams);
end;

procedure TcxSchedulerCustomViewInfoItem.UpdateCachedImage(const AViewParams: TcxViewParams);
var
  P: TPoint;
begin
  if Transparent then
    BitBlt(Cache.Image.Canvas.Handle, 0, 0, Width, Height, Canvas.Handle, Bounds.Left, Bounds.Top, srcCopy);
  FCanvas := Cache.Image.cxCanvas;
  Canvas.SetParams(AViewParams);
  P := Canvas.WindowOrg;
  Canvas.WindowOrg := Bounds.TopLeft;
  DoDraw;
  Canvas.WindowOrg := P;
end;

function TcxSchedulerCustomViewInfoItem.GetBitmap: TBitmap;
begin
  Result := FViewParams.Bitmap;
end;

function TcxSchedulerCustomViewInfoItem.GetColor: TColor;
begin
  Result := FViewParams.Color;
end;

function TcxSchedulerCustomViewInfoItem.GetDateTimeHelper: TcxSchedulerDateTimeHelperClass;
begin
  Result := cxSchedulerUtils.DateTimeHelper;
end;

function TcxSchedulerCustomViewInfoItem.GetHeight: Integer;
begin
  Result := Bounds.Bottom - Bounds.Top;
end;

function TcxSchedulerCustomViewInfoItem.GetPainterHelper: TcxSchedulerPainterHelperClass;
begin
  Result := TcxSchedulerHelpersFactory.PainterHelperClass;
end;

function TcxSchedulerCustomViewInfoItem.GetScaleFactor: TdxScaleFactor;
begin
  Result := FScaleFactor;
end;

function TcxSchedulerCustomViewInfoItem.GetTextColor: TColor;
begin
  Result := FViewParams.TextColor;
end;

function TcxSchedulerCustomViewInfoItem.GetWidth: Integer;
begin
  Result := Bounds.Right - Bounds.Left;
end;

procedure TcxSchedulerCustomViewInfoItem.SetBitmap(AValue: TBitmap);
begin
  FViewParams.Bitmap := AValue;
end;

procedure TcxSchedulerCustomViewInfoItem.SetColor(AValue: TColor);
begin
  FViewParams.Color := AValue;
end;

procedure TcxSchedulerCustomViewInfoItem.SetTextColor(AValue: TColor);
begin
  FViewParams.TextColor := AValue;
end;

{ TcxSchedulerBackgroundCellViewInfo }

procedure TcxSchedulerBackgroundCellViewInfo.DoDraw;
begin
  DrawRect(Bounds, Borders, clBtnShadow);
end;

{ TcxSchedulerCustomResourceViewInfoItem }

procedure TcxSchedulerCustomResourceViewInfoItem.InitHitTest(AHitTest: TcxSchedulerCustomResourceViewHitTest);
begin
  if IsResourceAssigned then
  begin
    AHitTest.SetResource(FResource);
    AHitTest.SetBitState(htcResource, True);
  end;
end;

{ TcxSchedulerCustomHeaderCellViewInfo }

constructor TcxSchedulerCustomHeaderCellViewInfo.Create(APainter: TcxCustomLookAndFeelPainter; const ABounds, AVisibleRect: TRect;
  const AViewParams: TcxViewParams; AScaleFactor: TdxScaleFactor; AUseRightToLeftAlignment: Boolean);
begin
  inherited Create(APainter, ABounds, AVisibleRect, AViewParams, AScaleFactor, AUseRightToLeftAlignment);
  FImageIndex := -1;
  FButtonState := cxbsDefault;
  FAlignHorz := taCenter;
  FAlignVert := vaCenter;
  FBorders := cxBordersAll;
  FTextRect := cxRectInflate(FBounds, -ScaleFactor.Apply(cxTextOffset), 0);
end;

destructor TcxSchedulerCustomHeaderCellViewInfo.Destroy;
begin
  FreeAndNil(FSelectionBitmap);
  FreeAndNil(FRotateBitmap);
  inherited Destroy;
end;

procedure TcxSchedulerCustomHeaderCellViewInfo.Calculate(const AText: string);
begin
  FDisplayText := AText;
end;

procedure TcxSchedulerCustomHeaderCellViewInfo.CalculateImageLayout;
const
  Alignments: array[TcxSchedulerHeaderImagePosition] of TAlignment = (taLeftJustify, taCenter, taRightJustify, taCenter);
var
  ATopLeft: TPoint;
  ATextSize, AImageSize: TSize;
  ABounds: TRect;
  ATextOffset: Integer;
begin
  FImageRectAssigned := HasImage;
  ATextOffset := ScaleFactor.Apply(cxTextOffset);
  if not FImageRectAssigned then
  begin
    FTextRect := GetDefaultTextRect;
    if not HasImage and RotateHeader then
    begin
      if RotateText then
        FTextRect := MakeRect(0, 0, cxSize(Height, Width))
      else
        FTextRect := MakeRect(0, 0, cxSize(Width, Height));
      InflateRect(FTextRect, -(ATextOffset + ScaleFactor.Apply(1)), -(ATextOffset + ScaleFactor.Apply(1)));
    end
    else
      if UseRightToLeftAlignment then
        FTextRect := TdxRightToLeftLayoutConverter.ConvertRect(FTextRect, Bounds);
    DisplayBounds := FTextRect;
    Exit;
  end;
  FTextRect := cxTextRect(PainterHelper.ExcludeBorders(Bounds, Borders));
  FAlignHorz := Alignments[GetImagePositionRTL];
  FAlignVert := vaTop;
  MultiLine := AutoHeight;
  // calculate text and image extents
  AImageSize := dxGetImageSize(nil, Images, ImageIndex, ScaleFactor);
  if RotateHeader and RotateText then
  begin
    AImageSize := cxSize(AImageSize.cy, AImageSize.cx);
    ABounds := MakeRect(0, 0, cxSize(Height - ScaleFactor.Apply(2), Width - ScaleFactor.Apply(2)));
    FTextRect := cxTextRect(ABounds);
  end
  else
  begin
    ABounds := Bounds;
    if RotateHeader then
    begin
      OffsetRect(ABounds, -ABounds.Left, -ABounds.Top);
      FTextRect := cxTextRect(ABounds);
    end;
  end;
  DisplayBounds := cxRectInflate(ABounds, -ScaleFactor.Apply(1), -ScaleFactor.Apply(1));
  if AutoHeight and (ImagePosition in [ipLeft, ipRight]) then
    Dec(FTextRect.Right, AImageSize.cx + ATextOffset * 2);

  if Length(DisplayText) > 0 then
  begin
//    Canvas.Font.Assign(Font);
//    Canvas.TextExtent(DisplayText, FTextRect, GetTextOutcxFlags);
    FTextRect.Right := FTextRect.Left + cxRectWidth(Resource.FTextRect);
    FTextRect.Bottom := FTextRect.Top + cxRectHeight(Resource.FTextRect);
  end
  else
    FTextRect := cxNullRect;
  ATextSize := cxRectSize(FTextRect);
  ATopLeft := cxPointOffset(cxRectCenter(ABounds), ScaleFactor.Apply(1), -ScaleFactor.Apply(1) + Byte(RotateHeader));
  if ImagePosition in [ipLeft, ipRight] then
  begin
    Dec(ATopLeft.X, (AImageSize.cx + ATextSize.cx + ATextOffset) div 2);
    FTextRect := MakeRect(ATopLeft.X, ATopLeft.y - ATextSize.cy div 2, ATextSize);
    FImageRect := MakeRect(ATopLeft.X, ATopLeft.y - AImageSize.cy div 2, AImageSize);
  end
  else
  begin
    Dec(ATopLeft.Y, (AImageSize.cy + ATextSize.cy + ATextOffset) div 2);
    FTextRect := MakeRect(ATopLeft.x - ATextSize.cx div 2, ATopLeft.y, ATextSize);
    FImageRect := MakeRect(ATopLeft.x - AImageSize.cx div 2, ATopLeft.y, AImageSize);
  end;
  // calculate text bounds
  if Length(DisplayText) > 0 then
  begin
    case GetImagePositionRTL of
      ipLeft:
        OffsetRect(FTextRect, AImageSize.cx + ATextOffset, 0);
      ipTop:
        OffsetRect(FTextRect, 0, AImageSize.cy + ATextOffset);
      ipRight:
        OffsetRect(FImageRect, ATextSize.cx + ATextOffset, 0);
      ipBottom:
        OffsetRect(FImageRect, 0, ATextSize.cy + ATextOffset);
    end;
  end;
end;

procedure TcxSchedulerCustomHeaderCellViewInfo.CheckNeighbor(APrevCell: TcxSchedulerCustomHeaderCellViewInfo);
begin
  if (APrevCell = nil) or (APrevCell.RotateHeader <> RotateHeader)  then Exit;
  if (RotateHeader and (APrevCell.Bounds.Bottom <> Bounds.Top)) or
    (not RotateHeader and (APrevCell.Bounds.Right <> Bounds.Left)) then
    Exclude(APrevCell.FBorders, bRight);
end;

function TcxSchedulerCustomHeaderCellViewInfo.CheckSelection: Boolean;
begin
  Result := False;
end;

procedure TcxSchedulerCustomHeaderCellViewInfo.CheckTextColorWhenSelected;
begin
  if Selected then
    TextColor := FSelectionTextColor;
end;

procedure TcxSchedulerCustomHeaderCellViewInfo.CreateRotatedBitmap;
begin
  FRotateBitmap := TcxBitmap.CreateSize(Bounds, pf32bit);
  FRotateBitmap.Canvas.Lock;
  if Transparent then
    cxBitBlt(FRotateBitmap.Canvas.Handle, Canvas.Handle, FRotateBitmap.ClientRect, Bounds.TopLeft, SRCCOPY);
  FRotateBitmap.cxCanvas.WindowOrg := Bounds.TopLeft;
end;

procedure TcxSchedulerCustomHeaderCellViewInfo.DoDraw;
begin
  ValidateSelection;
  CalculateImageLayout;
  FBackgroundDrawing := False;
  if RotateText or RotateHeader then
    DrawVerticalHeader
  else
    DrawHorizontalHeader;
end;

procedure TcxSchedulerCustomHeaderCellViewInfo.DoRightToLeftConversion(const AClientBounds: TRect);
begin
  inherited DoRightToLeftConversion(AClientBounds);
  FNeighbors := TdxRightToLeftLayoutConverter.ConvertNeighbors(FNeighbors);
  ChangeBiDiModeAlignment(FAlignHorz);
end;

procedure TcxSchedulerCustomHeaderCellViewInfo.DrawCaption(ACanvas: TcxCanvas = nil);
var
  ABitmap: TcxBitmap;
  AImageBounds: TRect;
begin
  if ACanvas = nil then
    ACanvas := Canvas;
  ACanvas.SaveClipRegion;
  try
    ACanvas.Brush.Style := bsClear;
    ACanvas.IntersectClipRect(DisplayBounds);
    ACanvas.Font := Font;
    ACanvas.Font.Color := TextColor;
    ACanvas.DrawTexT(DisplayText, TextRect, GetTextOutcxFlags);
    if FImageRectAssigned and HasImage then
    begin
      if RotateText then
      begin
        AImageBounds := cxRect(0, 0, FImageRect.Height, FImageRect.Width);
        ABitmap := TcxBitmap.CreateSize(AImageBounds, pf32bit);
        try
          ABitmap.Rotate(raMinus90);
          cxBitBlt(ABitmap.Canvas.Handle, ACanvas.Handle, ABitmap.ClientRect, FImageRect.TopLeft, SRCCOPY);
          ABitmap.Rotate(raPlus90);
          cxDrawImage(ABitmap.cxCanvas, AImageBounds, nil, Images, ImageIndex, ifmNormal, idmNormal, True, nil, ScaleFactor);
          ABitmap.Rotate(raMinus90);
          ACanvas.Draw(FImageRect.Left, FImageRect.Top, ABitmap);
        finally
          ABitmap.Free;
        end;
      end
      else
        cxDrawImage(ACanvas, FImageRect, nil, Images, ImageIndex, ifmNormal, idmNormal, True, nil, ScaleFactor);
    end;
  finally
    ACanvas.RestoreClipRegion;
  end;
end;

procedure TcxSchedulerCustomHeaderCellViewInfo.DrawHorizontalHeader;
begin
  if not Transparent then
    Painter.DrawScaledHeader(Canvas, Bounds, TextRect, Neighbors, Borders,
      FButtonState, AlignHorz, AlignVert, MultiLine, ShowEndEllipsis, '',
      Font, TextColor, Color, ScaleFactor, DrawBackground, not (nRight in Neighbors));
  if not FBackgroundDrawing and Selected then
    DrawSelection;
  if DisplayText <> '' then
    DrawCaption;
end;

function TcxSchedulerCustomHeaderCellViewInfo.DrawRotateBackground(ACanvas: TcxCanvas; const ABounds: TRect): Boolean;
begin
  Result := FDrawRotatedBackground;
end;

procedure TcxSchedulerCustomHeaderCellViewInfo.DrawSelection;
begin
  if FSelectionBitmap = nil then
    FSelectionBitmap := TcxBitmap.CreateSize(Bounds, pf32bit)
  else
    FSelectionBitmap.SetSize(Width, Height);
  cxBitBlt(FSelectionBitmap.Canvas.Handle, Canvas.Handle, FSelectionBitmap.ClientRect, Bounds.TopLeft, SRCCOPY);
  cxAlphaBlend(FSelectionBitmap, cxRectOffset(FSelectionRect, -Bounds.Left, -Bounds.Top), ColorToRgb(SelectionColor));
  cxBitBlt(Canvas.Handle, FSelectionBitmap.Canvas.Handle, Bounds, cxNullPoint, SRCCOPY);
end;

procedure TcxSchedulerCustomHeaderCellViewInfo.DrawVerticalHeader;
const
  AAngles: array[Boolean] of TcxRotationAngle = (raPlus90, raMinus90);
var
  R: TRect;
begin
  if FRotateBitmap = nil then
  begin
    CreateRotatedBitmap;
    FDrawRotatedBackground := DrawBackground(FRotateBitmap.cxCanvas, Bounds);

    if not Transparent then
    begin
      FRotateBitmap.Rotate(AAngles[UseRightToLeftAlignment], True);
      R := FRotateBitmap.ClientRect;
      Painter.DrawScaledHeader(FRotateBitmap.cxCanvas, R,
        cxRectInflate(R, -ScaleFactor.Apply(cxTextOffset), -ScaleFactor.Apply(cxTextOffset)),
        Neighbors, Borders, cxbsNormal, taCenter, vaCenter, False, False, '',
        Font, TextColor, Color, ScaleFactor, DrawRotateBackground);
      FRotateBitmap.Rotate(AAngles[UseRightToLeftAlignment], True);
    end;

    if RotateText then
    begin
      FRotateBitmap.Rotate(raMinus90);
      DrawCaption(FRotateBitmap.cxCanvas);
      FRotateBitmap.Rotate(raPlus90);
    end
    else
      DrawCaption(FRotateBitmap.cxCanvas);
  end;
  Canvas.Draw(Bounds.Left, Bounds.Top, FRotateBitmap);
end;

function TcxSchedulerCustomHeaderCellViewInfo.GetDefaultTextRect: TRect;
begin
  Result := cxRectInflate(PainterHelper.ExcludeBorders(Bounds, Borders), -ScaleFactor.Apply(cxTextOffset), 0);
end;

function TcxSchedulerCustomHeaderCellViewInfo.GetImagePositionRTL: TcxSchedulerHeaderImagePosition;
begin
  Result := ImagePosition;
  if UseRightToLeftAlignment then
    case Result of
      ipLeft:  Result := ipRight;
      ipRight: Result := ipLeft;
    end;
end;

function TcxSchedulerCustomHeaderCellViewInfo.GetTextOutcxFlags: Integer;
begin
  Result := MakeTextOutcxFlags(AlignHorz, AlignVert, AutoHeight);
end;

function TcxSchedulerCustomHeaderCellViewInfo.GetTextOutFlags: Integer;
const
  Horz: array[Boolean, TAlignment] of Integer = ((CXTO_LEFT, CXTO_RIGHT, CXTO_CENTER_HORIZONTALLY), (CXTO_RIGHT, CXTO_LEFT, CXTO_CENTER_HORIZONTALLY));
  Vert: array[TcxAlignmentVert] of Integer = (CXTO_TOP, CXTO_BOTTOM, CXTO_CENTER_VERTICALLY);
begin
  if AutoHeight then
    Result := CXTO_PREVENT_LEFT_EXCEED or CXTO_PREVENT_TOP_EXCEED or Horz[UseRightToLeftAlignment, AlignHorz] or Vert[AlignVert] or CXTO_WORDBREAK
  else
    Result := CXTO_PREVENT_LEFT_EXCEED or CXTO_PREVENT_TOP_EXCEED or Horz[UseRightToLeftAlignment, AlignHorz] or Vert[AlignVert] + CXTO_SINGLELINE;
end;

function TcxSchedulerCustomHeaderCellViewInfo.HasImage: Boolean;
begin
  Result := ImageIndex >= 0
end;

procedure TcxSchedulerCustomHeaderCellViewInfo.InitHitTest(AHitTest: TcxSchedulerCustomResourceViewHitTest);
begin
  inherited InitHitTest(AHitTest);
  if IsResourceAssigned then
    AHitTest.SetBitState(htcResourceHeader, True);
  if DateTime <> NullDate then
    AHitTest.SetHitTime(htcDayHeader, DateTime);
  AHitTest.FHitObject := Self;
  AHitTest.FHitObjectBounds := Bounds;
end;

procedure TcxSchedulerCustomHeaderCellViewInfo.TextOut(ACanvas: TcxCanvas; const AText: string; R: TRect);
begin
  if Length(AText) = 0 then Exit;
  if not FImageRectAssigned then
    R := cxRectInflate(R, -ScaleFactor.Apply(1), -ScaleFactor.Apply(cxTextOffset + 1));

  ACanvas.Font := FViewParams.Font;
  ACanvas.Font.Color := FViewParams.TextColor;
  cxTextOut(ACanvas.Handle, AText, R, GetTextOutFlags);
end;

procedure TcxSchedulerCustomHeaderCellViewInfo.ValidateSelection;
begin
  if CheckSelection and (Date = dxDateOf(FDateTime)) then
    FButtonState := cxbsHot;
  if Selected then
  begin
    CheckTextColorWhenSelected;
    FSelectionRect := PainterHelper.ExcludeBorders(Bounds, Borders);
    if FButtonState = cxbsHot then
    begin
      FSelectionRect.Left := Max(FSelectionRect.Left, FSelectionRect.Right -
        PainterHelper.TextWidth(Font, DisplayText) - 2 * ScaleFactor.Apply(cxTextOffset));
    end;
  end;
end;

{ TcxSchedulerHeaderCellViewInfo }

procedure TcxSchedulerHeaderCellViewInfo.CalculateImageLayout;
begin
  inherited CalculateImageLayout;
end;

{ TcxSchedulerDayHeaderCellViewInfo }

function TcxSchedulerDayHeaderCellViewInfo.ConvertDateToDisplayText(AType: Integer = 0): Integer;
var
  AOffset: Integer;
begin
  AOffset := ScaleFactor.Apply(cxTextOffset);
  Result := AType - 1;
  repeat
    Inc(Result);
    if cxRectWidth(FTextRect) <= 0 then
      Break;

    FDisplayText := DateTimeHelper.DayToStr(FDateTime, Result, Compressed);
  until (Result = DateTimeHelper.DayToStrFormatCount - 1) or
    (cxTextWidth(Font, DisplayText) <= cxRectWidth(FTextRect) - AOffset);
end;

function TcxSchedulerDayHeaderCellViewInfo.CheckSelection: Boolean;
begin
  Result := True;
end;

function TcxSchedulerDayHeaderCellViewInfo.DrawBackground(ACanvas: TcxCanvas; const ABounds: TRect): Boolean;
var
  R: TRect;
begin
  FBackgroundDrawing := True;
  Result := inherited DrawBackground(ACanvas, ABounds);
  if Result then Exit;
  R := PainterHelper.ExcludeBorders(Bounds, Borders);
  if FButtonState = cxbsHot then
    PainterHelper.DrawGradientRect(ACanvas, Color, R);
  if Selected then
    ACanvas.FillRect(FSelectionRect, SelectionColor);
  Result := (FButtonState = cxbsHot) or Selected;
end;

procedure TcxSchedulerDayHeaderCellViewInfo.DrawHorizontalHeader;
begin
  if Selected and not (FButtonState = cxbsHot) then
  begin
    Color := FSelectionColor;
    TextColor := FSelectionTextColor;
  end;
  inherited DrawHorizontalHeader;
end;

function TcxSchedulerDayHeaderCellViewInfo.GetDefaultTextRectForModernStyle: TRect;
var
  AMinLeft: Integer;
begin
  Result := inherited GetDefaultTextRect;
  AMinLeft := Result.Left;
  Result.Left := Bounds.Left + ScaleFactor.Apply(cxTimeLineWidth);
  if cxRectIsEmpty(Result) then
    Result.Left := AMinLeft;
  Inc(Result.Top, ScaleFactor.Apply(cxTextOffset));
end;

{ TcxSchedulerDayHeaderCellModernViewInfo }

function TcxSchedulerDayHeaderCellModernViewInfo.ConvertDateToDisplayText(AType: Integer = 0): Integer;
var
  AOffsets: Integer;
  ADay, ADate: string;
begin
  if ActualModernStyleDisplayMode = hdmClassic then
  begin
    AlignHorz := inherited AlignHorz;
    Result := inherited ConvertDateToDisplayText(AType);
    Exit;
  end;
  AlignHorz := taLeftJustify;
  AOffsets := ScaleFactor.Apply((cxTimeLineWidth * 3) div 2 + cxTextOffset);
  case ActualModernStyleDisplayMode of
    hdmDefault:
      begin
        Result := AType - 1;
        repeat
          Inc(Result);
          if cxRectWidth(FTextRect) <= 0 then
            Break;
          FDisplayText := AnsiUpperCase(GetDayText(Result));
        until (Result = 3) or
          (cxTextWidth(Font, DisplayText) <= cxRectWidth(FTextRect) - AOffsets);
      end;
  else
    begin
      Result := AType - 1;
      repeat
        Inc(Result);
        if cxRectWidth(FTextRect) <= 0 then
          Break;
        ADate := AnsiUpperCase(DateTimeHelper.DayToStr(FDateTime, Result, Compressed));
        ADay := '';
        if Pos(AnsiUpperCase('DDD'), AnsiUpperCase(DateTimeHelper.GetDayMonthFormat(Result, False))) = 0 then
          ADay := AnsiUpperCase(GetDayText(Min(Result, 3))) + ', ';
        FDisplayText := Format('%s%s', [ADay, ADate]);
      until (Result = DateTimeHelper.DayToStrFormatCount - 1) or
        (cxTextWidth(Font, DisplayText) <= cxRectWidth(FTextRect) - AOffsets);
    end;
  end;
end;

procedure TcxSchedulerDayHeaderCellModernViewInfo.CheckTextColorWhenSelected;
begin
  //
end;

function TcxSchedulerDayHeaderCellModernViewInfo.DrawBackground(ACanvas: TcxCanvas; const ABounds: TRect): Boolean;
begin
  if ActualModernStyleDisplayMode = hdmClassic then
  begin
    Result := inherited DrawBackground(ACanvas, ABounds);
    Exit;
  end;
  Result := Transparent or Assigned(Bitmap) and not Bitmap.Empty;
  if Result and not Transparent then
    ACanvas.FillRect(ABounds, Bitmap);
  ACanvas.FrameRect(ABounds, ExternalPainter.Painter.DefaultSchedulerDayHeaderBorderColor, 1, [bBottom]);
  if not Result then
  begin
    if FButtonState = cxbsHot then
    begin
      ACanvas.FillRect(ABounds, ExternalPainter.Painter.DefaultSchedulerHeaderContainerAlternateBackgroundColor);
      FBackgroundDrawing := True;
    end
    else
      ACanvas.FillRect(PainterHelper.ExcludeBorders(ABounds, [bBottom]), Color);
    Result := True;
  end;
end;

procedure TcxSchedulerDayHeaderCellModernViewInfo.DrawCaption(ACanvas: TcxCanvas = nil);
var
  AStyles: TFontStyles;
begin
  if ActualModernStyleDisplayMode = hdmClassic then
  begin
    inherited DrawCaption(ACanvas);
    Exit;
  end;
  AStyles := Font.Style;
  try
    if (FButtonState = cxbsHot) then
      Font.Style := Font.Style + [fsBold];
    DrawText(TextRect, DisplayText, GetTextOutcxFlags, Font, TextColor);
  finally
    Font.Style := AStyles;
  end;
end;

procedure TcxSchedulerDayHeaderCellModernViewInfo.DrawHorizontalHeader;
begin
  if IsResourceHeader or (ActualModernStyleDisplayMode = hdmClassic) then
  begin
    inherited DrawHorizontalHeader;
    Exit;
  end;
  if not Transparent then
    Painter.DrawSchedulerDayHeader(Canvas, Bounds, TextRect, Neighbors, Borders,
        FButtonState, AlignHorz, AlignVert, MultiLine, ShowEndEllipsis, '',
        Font, TextColor, Color, ScaleFactor, DrawBackground, not (nRight in Neighbors));
  DrawCaption;
end;

function TcxSchedulerDayHeaderCellModernViewInfo.GetDayText(AType: Integer): string;
var
  ADay: Integer;
begin
  ADay := DayOfWeek(FDateTime);
  case AType of
    0:
      Result := dxFormatSettings.LongDayNames[ADay];
    1:
      Result := dxFormatSettings.ShortDayNames[ADay];
  else
    Result := AnsiUpperCase(dxFormatSettings.LongDayNames[ADay][1]);
  end;
end;

function TcxSchedulerDayHeaderCellModernViewInfo.GetDefaultTextRect: TRect;
begin
  Result := GetDefaultTextRectForModernStyle;
end;

{ TcxSchedulerWeekDayHeaderCellViewInfo }

function TcxSchedulerWeekDayHeaderCellViewInfo.ConvertDateToDisplayText(AType: Integer = 0): Integer;
begin
  Result := AType - 1;
  repeat
    Inc(Result);
    FDisplayText := GetDayText(Result);
  until (Result = 3) or (cxTextWidth(Font, DisplayText) <= cxRectWidth(FTextRect));
  if (dxDateOf(FDateTime) + 1) > 7 then
    Result := 0;
end;

procedure TcxSchedulerWeekDayHeaderCellViewInfo.InitHitTest(AHitTest: TcxSchedulerCustomResourceViewHitTest);
begin
  inherited InitHitTest(AHitTest);
  AHitTest.SetBitState(htcTime, False);
end;

function TcxSchedulerWeekDayHeaderCellViewInfo.GetDayText(AType: Integer): string;
var
  ADay: Integer;
begin
  ADay := Trunc(FDateTime) + 1;
  if ADay > 7 then
  begin
    case AType of
      0:
        Result := dxFormatSettings.LongDayNames[7]+ '/' + dxFormatSettings.LongDayNames[1];
      1:
        Result := dxFormatSettings.ShortDayNames[7]+ '/' + dxFormatSettings.ShortDayNames[1];
    else
      Result := AnsiUpperCase(dxFormatSettings.LongDayNames[7][1]+ '/' + dxFormatSettings.LongDayNames[1][1]);
    end;
  end
  else
    case AType of
      0:
        Result := dxFormatSettings.LongDayNames[ADay];
      1:
        Result := dxFormatSettings.ShortDayNames[ADay];
    else
      Result := AnsiUpperCase(dxFormatSettings.LongDayNames[ADay][1]);
    end;
end;

{ TcxSchedulerWeekDayHeaderCellModernViewInfo }

function TcxSchedulerWeekDayHeaderCellModernViewInfo.DrawBackground(ACanvas: TcxCanvas; const ABounds: TRect): Boolean;
begin
  Result := inherited DrawBackground(ACanvas, ABounds);
  if not Result then
    ACanvas.FillRect(PainterHelper.ExcludeBorders(ABounds, [bBottom]), Color);
  ACanvas.FrameRect(ABounds, ExternalPainter.Painter.DefaultSchedulerDayHeaderBorderColor, 1, [bBottom]);
  Result := True;
end;

procedure TcxSchedulerWeekDayHeaderCellModernViewInfo.DrawHorizontalHeader;
begin
  if not Transparent then
    Painter.DrawSchedulerDayHeader(Canvas, Bounds, TextRect, Neighbors, Borders,
        FButtonState, AlignHorz, AlignVert, MultiLine, ShowEndEllipsis, '',
        Font, TextColor, Color, ScaleFactor, DrawBackground, not (nRight in Neighbors));
  if not FBackgroundDrawing and Selected then
    DrawSelection;
  DrawCaption;
end;

function TcxSchedulerWeekDayHeaderCellModernViewInfo.GetDayText(AType: Integer): string;
begin
  AlignHorz := taLeftJustify;
  Result := AnsiUpperCase(inherited GetDayText(AType));
end;

function TcxSchedulerWeekDayHeaderCellModernViewInfo.GetDefaultTextRect: TRect;
begin
  Result := GetDefaultTextRectForModernStyle;
end;

{ TcxSchedulerContainerCellViewInfo }

procedure TcxSchedulerContainerCellViewInfo.Calculate(ADate: TDateTime; ASelected: Boolean;
  ACaptionDisplayMode: TcxSchedulerDayHeaderModernStyleDisplayMode; AHeadersEventsAreaTop: Integer);
begin
  FDateTime := ADate;
  FSelected := ASelected;
  FModernStyleCaptionDisplayMode := ACaptionDisplayMode;
  FHeaderEventsAreaTop := AHeadersEventsAreaTop;
  FHeaderEvent := nil;
end;

procedure TcxSchedulerContainerCellViewInfo.DoDraw;
begin
  FExternalPainter.DrawAllDayArea(Canvas, Bounds, GetBorderColor, Borders, ViewParams, Selected, Transparent);
end;

function TcxSchedulerContainerCellViewInfo.GetBorderColor: TColor;
begin
  Result := FDayBorderColor;
end;

procedure TcxSchedulerContainerCellViewInfo.InitHitTest(AHitTest: TcxSchedulerCustomResourceViewHitTest);
begin
  inherited InitHitTest(AHitTest);
  AHitTest.FHitObject := Self;
  AHitTest.SetHitTime(htcContainer, DateTime);
end;

function TcxSchedulerContainerCellViewInfo.NeedDrawCaption: Boolean;
begin
  Result := False;
end;

{ TcxSchedulerContainerModernCellViewInfo }

procedure TcxSchedulerContainerModernCellViewInfo.Calculate(ADate: TDateTime; ASelected: Boolean;
  ACaptionDisplayMode: TcxSchedulerDayHeaderModernStyleDisplayMode; AHeadersEventsAreaTop: Integer);
var
  AOffset: Integer;
begin
  inherited Calculate(ADate, ASelected, ACaptionDisplayMode, AHeadersEventsAreaTop);
  if NeedDrawCaption then
  begin
    DisplayText := IntToStr(DayOf(DateTime));
    AOffset := ScaleFactor.Apply(cxTextOffset);
    FTextRect := Bounds;
    FTextRect.Top := HeaderEventsAreaTop + AOffset;
    Dec(FTextRect.Right, AOffset);
    Inc(FTextRect.Left, ScaleFactor.Apply(cxTimeLineWidth));
    if cxRectIsEmpty(FTextRect) then
      FTextRect.Left := AOffset;
  end;
end;

procedure TcxSchedulerContainerModernCellViewInfo.DoDraw;
var
  ABounds: TRect;
  AContentColor: TColor;
  AViewParams: TcxViewParams;
begin
  if (HeaderEvent <> nil) and
     ((Resource = nil) or VarEquals(Resource.ResourceID, HeaderEvent.Event.ResourceID)) and
     (HeaderEvent.Event.State > 0) and not Selected then
  begin
    FExternalPainter.DrawAllDayArea(Canvas, cxRectSetHeight(Bounds, FFirstEventTop - Bounds.Top),
      GetBorderColor, Borders - [bBottom], ViewParams, Selected, Transparent);

    AContentColor := ExternalPainter.GetColorizedColor(ViewParams.Color, HeaderEvent);

    ABounds := Bounds;
    ABounds.Top := FFirstEventTop;

    if HeaderEvent.Event.State = 1 then
    begin
      FExternalPainter.DrawAllDayArea(Canvas, ABounds,
        GetBorderColor, [], ViewParams, Selected, Transparent);
      cxFillRectWithCustomBrush(Canvas.Canvas, StateBrushes[9], ABounds,
        AContentColor, ExternalPainter.GetColorizedColor(GetBorderColor, HeaderEvent), ViewParams.Bitmap <> nil);
      Canvas.FrameRect(ABounds, GetBorderColor, 1, Borders);
    end
    else
    begin
      AViewParams := FViewParams;
      AViewParams.Color := AContentColor;
      FExternalPainter.DrawAllDayArea(Canvas, ABounds, GetBorderColor, Borders, AViewParams, Selected, Transparent);
      if IsDrawBySkin then
        Canvas.Rectangle(PainterHelper.ExcludeBorders(ABounds, [bRight, bBottom]), AViewParams, [],  GetBorderColor, 1);
    end;
  end
  else
    inherited DoDraw;

  if NeedDrawCaption then
    DrawCaption;
  if FExternalPainter.Painter.LookAndFeelStyle <> lfsSkin then
    Canvas.FillRect(cxRectSetBottom(Bounds, Bounds.Bottom, 2), GetBorderColor);
end;

procedure TcxSchedulerContainerModernCellViewInfo.DoRightToLeftConversion(const AClientBounds: TRect);
begin
  inherited DoRightToLeftConversion(AClientBounds);
  FTextRect := TdxRightToLeftLayoutConverter.ConvertRect(FTextRect, AClientBounds);
end;

procedure TcxSchedulerContainerModernCellViewInfo.DrawCaption;
const
  AFlags: array[Boolean] of Integer = (cxAlignTop or cxAlignLeft, cxAlignTop or cxAlignRight);
var
  AStyle: TFontStyles;
begin
  AStyle := Font.Style;
  if IsToday then
    Font.Style := Font.Style + [fsBold];
  DrawText(FTextRect, DisplayText, AFlags[UseRightToLeftAlignment], Font, TextColor);
  Font.Style := AStyle;
end;

function TcxSchedulerContainerModernCellViewInfo.GetBorderColor: TColor;
begin
  Result := ExternalPainter.Painter.DefaultSchedulerHeaderContainerBorderColor;
end;

function TcxSchedulerContainerModernCellViewInfo.GetIsToday: Boolean;
begin
  Result := Date = FDateTime;
end;

function TcxSchedulerContainerModernCellViewInfo.NeedDrawCaption: Boolean;
begin
  Result := ModernStyleCaptionDisplayMode = hdmDefault;
end;

{ TcxSchedulerTimeRulerCellViewInfo }

procedure TcxSchedulerTimeRulerCellViewInfo.Calculate(const ALabel1, ALabel2: string; ATimeZone,
  AAdditionalTimeZone: Integer; AAdditionalTimeZoneBiasDelta: Integer; ASelectedDays: TcxSchedulerDateList = nil);
begin
  Calculate(-1, 1, ATimeZone, AAdditionalTimeZone, nil, AAdditionalTimeZoneBiasDelta, ASelectedDays);
  FDisplayTexts[True] := ALabel1;
  FDisplayTexts[False] := ALabel2;
end;

procedure TcxSchedulerTimeRulerCellViewInfo.Calculate(AHour, ALineCount: Integer;
  ATimeZone, AAdditionalTimeZone: Integer; ALargeFont: TFont;
  AAdditionalTimeZoneBiasDelta: Integer; ASelectedDays: TcxSchedulerDateList = nil);
begin
  FHour := AHour;
  FLineCount := ALineCount;
  FAdditionalTimeZone := AAdditionalTimeZone;
  FAdditionalTimeZoneBiasDelta := AAdditionalTimeZoneBiasDelta;
  FTimeZone := ATimeZone;
  FLargeFont := ALargeFont;
  if FVisible then
    CalculateDisplayInfo;
  FCanDrawCurrentTime := GetCanDrawCurrentTime(ASelectedDays);
end;

procedure TcxSchedulerTimeRulerCellViewInfo.SetBottom(AValue: Integer);
begin
  FClipRect.Bottom := AValue;
end;

class function TcxSchedulerTimeRulerCellViewInfo.CalculateWidth(ATimeZoneCount, ALineCount: Integer;
  AFont1, AFont2: TFont; AScaleFactor: TdxScaleFactor = nil; AShowMinutes: Boolean = False): Integer;
var
  AInternalScaleFactor: TdxScaleFactor;
begin
  if AScaleFactor = nil then
    AInternalScaleFactor := dxSystemScaleFactor
  else
    AInternalScaleFactor := AScaleFactor;
  Result := GetBaseWidth(ALineCount, AFont1, AFont2, AScaleFactor, AShowMinutes);
  Inc(Result, AInternalScaleFactor.Apply(cxTextOffset) shl 2);
  if ATimeZoneCount > 1 then
    Result := Result shl 1;
end;

procedure TcxSchedulerTimeRulerCellViewInfo.CalculateDisplayInfo;
begin
  FBounds[True] := inherited Bounds;
  FDateTime := Hour * HourToTime;
  if Hour >= 0 then
    FDisplayTexts[True] := GetTimeDisplayText(FDateTime);
  FBounds[False] := inherited Bounds;
  if HasAdditionalTimeZone then
  begin
    FBounds[True].Left := cxRectCenter(inherited Bounds).X;
    FBounds[False].Right := FBounds[True].Left;
    if Hour >= 0 then
      FDisplayTexts[False] := GetTimeDisplayText(GetAdditionalTime);
  end;
end;

procedure TcxSchedulerTimeRulerCellViewInfo.DoDraw;
var
  ACurrentTimeZone: Boolean;
begin
  ClippingCreate(FHasClipping);
  for ACurrentTimeZone := not HasAdditionalTimeZone to True do
  begin
    if Hour >= 0 then
      DrawTimeZone(Bounds[ACurrentTimeZone], DisplayTexts[ACurrentTimeZone], ACurrentTimeZone)
    else
      DrawTimeZoneLabel(Bounds[ACurrentTimeZone], DisplayTexts[ACurrentTimeZone], GetLabelZoneBorders(ACurrentTimeZone));
  end;
  ClippingRestore;
end;

procedure TcxSchedulerTimeRulerCellViewInfo.DoRightToLeftConversion(const AClientBounds: TRect);
begin
  inherited DoRightToLeftConversion(AClientBounds);
  FBounds[True] := TdxRightToLeftLayoutConverter.ConvertRect(FBounds[True], AClientBounds);
  FBounds[False] := TdxRightToLeftLayoutConverter.ConvertRect(FBounds[False], AClientBounds);
  FVisibleRect := TdxRightToLeftLayoutConverter.ConvertRect(FVisibleRect, AClientBounds);
end;

procedure TcxSchedulerTimeRulerCellViewInfo.DrawTimeZoneLabel(
  const ABounds: TRect; const AText: string; ABorders: TcxBorders);
var
  R: TRect;
begin
  R := ABounds;
  cxRightToLeftDependentDraw(Canvas.Handle, ABounds, Canvas.UseRightToLeftAlignment,
    procedure
    begin
      FExternalPainter.DrawTimeRulerBackground(Canvas, R, ABorders, GetBorderColor, ViewParams, Transparent);
    end);
  DrawText(GetActualRectRTL(GetLabelZoneTextRect(ABounds), ABounds), AText, cxAlignBottom or cxAlignLeft, Font, TextColor);
end;

procedure TcxSchedulerTimeRulerCellViewInfo.DrawTimeZone(const ABounds: TRect; const AText: string; AIsCurrent: Boolean);
var
  S: string;
  APos, J, LH: Integer;
  R, R1, TextR: TRect;
const
  cxRightCenterAlign: array[Boolean] of Integer = (cxAlignVCenter or cxAlignRight or cxDontClip, cxAlignVCenter or cxAlignLeft or cxDontClip);
  cxRightTopAlign: array[Boolean] of Integer = (cxAlignTop or cxAlignRight or cxDontClip, cxAlignTop or cxAlignLeft or cxDontClip);
begin
  LH := cxRectHeight(inherited Bounds) div LineCount;
  R := ABounds;
  cxRightToLeftDependentDraw(Canvas.Handle, ABounds, Canvas.UseRightToLeftAlignment,
    procedure
    begin
      FExternalPainter.DrawTimeRulerBackground(Canvas, R, [bRight], GetBorderColor, ViewParams, Transparent);
    end);
  R := cxRectSetHeight(ABounds, LH);
  R.Left := (R.Left + R.Right) div 2 + ScaleFactor.Apply(cxTextOffset) shl 1;
  Dec(R.Right, ScaleFactor.Apply(6));
  for J := 0 to LineCount - 2 do
  begin
    Canvas.FrameRect(GetActualRectRTL(R, ABounds), GetBorderColor, ScaleFactor.Apply(1), [bBottom]);
    OffsetRect(R, 0, LH);
  end;
  Canvas.Brush.Style := bsClear;
  TextR := cxRectInflate(PainterHelper.ExcludeBorders(ABounds, GetRealRightBorder + [bBottom]),
    -ScaleFactor.Apply(5), -ScaleFactor.Apply(cxTextOffset));
  if CanDrawCurrentTime and AIsCurrent and FExternalPainter.DrawCurrentTimeFirst then
    DrawCurrentTime(FDateTime, ABounds);
  if LineCount > 1 then
  begin
    J := Length(AText);
    APos := Pos(' ', AText);
    if APos = 0 then
      APos := J - ScaleFactor.Apply(3);
    DrawText(GetActualRectRTL(Rect(TextR.Left, TextR.Top, R.Left - ScaleFactor.Apply(cxTextOffset), TextR.Bottom), ABounds, 1),
      Copy(AText, 1, APos - 1), cxRightTopAlign[UseRightToLeftAlignment], LargeFont, TextColor);
    R1 := Rect(R.Left, TextR.Top, TextR.Right, TextR.Top + LH - ScaleFactor.Apply(3));
    DrawText(GetActualRectRTL(R1, ABounds), Copy(AText, APos + 1, J - APos), cxAlignCenter, Font, TextColor);
    if ShowMinutes and AIsCurrent then
      for J := 1 to LineCount - 1 do
      begin
        OffsetRect(R1, 0, LH);
        S := IntToStr(MulDiv(60, J, LineCount));
        if Length(S) < 2 then
          S := '0' + S;
        DrawText(GetActualRectRTL(R1, ABounds), S, cxAlignCenter, Font, TextColor);
      end;
  end
  else
    DrawText(GetActualRectRTL(TextR, ABounds, 1), AText, cxRightCenterAlign[UseRightToLeftAlignment], Font, TextColor);
  if CanDrawCurrentTime and AIsCurrent and not FExternalPainter.DrawCurrentTimeFirst then
    DrawCurrentTime(FDateTime, ABounds);
  R.Left := ABounds.Left + ScaleFactor.Apply(5);
  if (Hour <> 23) and not LastVisibleHour then
    DrawFrame(GetActualRectRTL(R, ABounds), [bBottom], clBtnShadow)
  else
    DrawFrame(ABounds, [bBottom], clBtnShadow);
end;

procedure TcxSchedulerTimeRulerCellViewInfo.DrawCurrentTime(const AStart: TDateTime; ABounds: TRect);
var
  ADelta: Double;
begin
  with DateTimeHelper do
    ADelta := TimeZoneBias(CurrentTimeZone) - TimeZoneBias(TimeZone);
  FExternalPainter.DrawCustomCurrentTime(Canvas, Color, AStart - ADelta, ABounds, UseRightToLeftAlignment);
  Canvas.Brush.Style := bsClear;
end;

function TcxSchedulerTimeRulerCellViewInfo.GetAdditionalTime: TDateTime;
begin
  Result := DateTimeHelper.ConvertToAnotherTimeZone(
    (Hour + FAdditionalTimeZoneBiasDelta / MinsPerHour) * HourToTime + Date, TimeZone, AdditionalTimeZone);
end;

class function TcxSchedulerTimeRulerCellViewInfo.GetBaseWidth(ALineCount: Integer;
  AFont1, AFont2: TFont; AScaleFactor: TdxScaleFactor; AShowMinutes: Boolean): Integer;
begin
  if ALineCount = 1 then
    Result := TcxSchedulerPainterHelper.TextWidth(AFont1, ' 24:PM')
  else
    Result := TcxSchedulerPainterHelper.TextWidth(AFont1, ' 24') +
      cxTextWidth(AFont2, 'pm') + AScaleFactor.Apply(cxTextOffset);
end;

function TcxSchedulerTimeRulerCellViewInfo.GetBorderColor: TColor;
begin
  Result := ExternalPainter.Painter.DefaultSchedulerTimeRulerBorderColorClassic;
end;

function TcxSchedulerTimeRulerCellViewInfo.GetCanDrawCurrentTime(ASelectedDays: TcxSchedulerDateList): Boolean;
begin
  Result := True;
end;

function TcxSchedulerTimeRulerCellViewInfo.GetLabelZoneBorders(const AIsCurrentTimeZone: Boolean): TcxBorders;
begin
  Result := [bBottom];
  if AIsCurrentTimeZone then
    Result := Result + [bRight];
end;

function TcxSchedulerTimeRulerCellViewInfo.GetLabelZoneTextRect(const ABounds: TRect): TRect;
begin
  Result := cxTextRect(ABounds);
end;

function TcxSchedulerTimeRulerCellViewInfo.GetRealRightBorder: TcxBorders;
const
  AResult: array[Boolean] of TcxBorders = ([bRight], [bLeft]);
begin
  Result := AResult[UseRightToLeftAlignment];
end;

function TcxSchedulerTimeRulerCellViewInfo.GetTimeDisplayText(const ATime: TDateTime): string;
var
  APos: Integer;
begin
  Result := DateTimeHelper.HourToStr(ATime);
  if LineCount > 1 then
  begin
    APos := Pos(dxFormatSettings.TimeSeparator, Result);
    if APos = 0 then
      APos := Pos(' ', Result);
    if APos = 0 then
      APos := Length(Result) - 2;
    Result := Copy(Result, 1, APos - 1) + ' ' + AnsiLowerCase(Copy(Result, APos + 1, 2));
  end;
end;

procedure TcxSchedulerTimeRulerCellViewInfo.InitHitTest(AHitTest: TcxSchedulerCustomResourceViewHitTest);
var
  LH: Integer;
  ACurrentTimeZone: Boolean;
  ATime, ATimePerLine: TDateTime;
begin
  inherited InitHitTest(AHitTest);
  if Hour >= 0 then
  begin
    LH := cxRectHeight(inherited Bounds) div FLineCount;
    ATimePerLine := 60 / FLineCount * MinuteToTime;
    ATime := (AHitTest.HitY - inherited Bounds.Top) div LH * ATimePerLine + FHour * HourToTime;
    AHitTest.SetHitTime(htcTimeRuler, ATime);
  end
  else
    AHitTest.SetBitState(htcTimeZoneLabel, True);
  for ACurrentTimeZone := not HasAdditionalTimeZone to True do
    if PtInRect(Bounds[ACurrentTimeZone], AHitTest.HitPoint) then
    begin
      if ACurrentTimeZone then
        AHitTest.FTimeZone := TimeZone
      else
        AHitTest.FTimeZone := AdditionalTimeZone;
    end;
end;

procedure TcxSchedulerTimeRulerCellViewInfo.AfterCustomDraw(ACanvas: TcxCanvas);
begin
  FViewParams.TextColor := ACanvas.Font.Color;
  FViewParams.Color := ACanvas.Brush.Color;
end;

function TcxSchedulerTimeRulerCellViewInfo.GetBoundsRect(AType: Boolean): TRect;
begin
  Result := FBounds[AType];
end;

function TcxSchedulerTimeRulerCellViewInfo.GetDisplayText(AType: Boolean): string;
begin
  Result := FDisplayTexts[AType];
end;

function TcxSchedulerTimeRulerCellViewInfo.GetHasAdditionalTimeZone: Boolean;
begin
  Result := AdditionalTimeZone >= 0;
end;

{ TcxSchedulerTimeRulerModernCellViewInfo }

procedure TcxSchedulerTimeRulerModernCellViewInfo.DrawBottomLine(const ABounds: TRect);
var
  R: TRect;
begin
  R := ABounds;
  if UseRightToLeftAlignment then
    Dec(R.Right, GetBottomLineIndent)
  else
    Inc(R.Left, GetBottomLineIndent);
  R.Top := R.Bottom - 1;
  Canvas.FillRect(R, GetBorderColor);
end;

procedure TcxSchedulerTimeRulerModernCellViewInfo.DrawTimeZone(const ABounds: TRect; const AText: string; AIsCurrent: Boolean);

  function InternalRightIndent: Integer;
  begin
    Result := GetBottomLineIndent;
    if not Is24HourTimeFormat then
      Result := cxTextWidth(Font, TcxSchedulerDateTimeHelper.GetMeridiem(True, True)) + ScaleFactor.Apply(cxTextOffset);
    if ShowMinutes then
      Result := Max(Result, (ABounds.Right - ABounds.Left) div 4);
  end;

const
  AAlignOnVCenter: array[Boolean] of Integer = (cxAlignVCenter or cxDontClip, cxAlignTop or cxDontClip);
  AAlignMinutes: array[Boolean] of Integer = (cxAlignTop or cxAlignRight, cxAlignTop or cxAlignLeft);
  AAlignHorz: array[Boolean] of Integer = (cxAlignRight, cxAlignLeft);
var
  R, R1: TRect;
  APos, LH, J, AHalf: Integer;
  st: string;
begin
  LH := cxRectHeight(ABounds) div LineCount;
  R := ABounds;
  cxRightToLeftDependentDraw(Canvas.Handle, ABounds, Canvas.UseRightToLeftAlignment,
    procedure
    begin
      FExternalPainter.DrawTimeRulerBackground(Canvas, R, [bRight], GetBorderColor, ViewParams, Transparent);
    end);
  if ShowMinutes then
  begin
    R := cxRectSetHeight(ABounds, LH);
    AHalf := (R.Left + R.Right) div 2;
    R.Left := R.Right - (R.Right - R.Left) div 4;
    for J := 0 to LineCount - 1 do
    begin
      if J < LineCount - 1 then
        Canvas.FrameRect(GetActualRectRTL(R, ABounds), GetBorderColor, ScaleFactor.Apply(1), [bBottom]);
      if Is24HourTimeFormat or not ((J = 0) and (Pos(' ', AText) > 0)) then
      begin
        R1 := R;
        R1.Left := AHalf;
        Dec(R1.Right, ScaleFactor.Apply(cxTextOffset));
        st := IntToStr(MulDiv(60, J, LineCount));
        if Length(st) < 2 then
          st := '0' + st;
        DrawText(GetActualRectRTL(R1, ABounds, 1), st, AAlignMinutes[UseRightToLeftAlignment], Font, TextColor);
      end;
      OffsetRect(R, 0, LH);
    end;
  end;
  DrawBottomLine(ABounds);
  if CanDrawCurrentTime and AIsCurrent and FExternalPainter.DrawCurrentTimeFirst then
    DrawCurrentTime(FDateTime, ABounds);
  R := cxRectInflate(PainterHelper.ExcludeBorders(ABounds, GetRealRightBorder + [bBottom]),
    -GetBottomLineIndent, -ScaleFactor.Apply(cxTextOffset));
  R.Right := ABounds.Right - InternalRightIndent;
  if Is24HourTimeFormat then
    DrawText(GetActualRectRTL(R, ABounds, 1), AText, AAlignOnVCenter[LineCount > 1] or cxAlignHCenter, LargeFont, TextColor)
  else
  begin
    st := AText;
    APos := Pos(' ', AText);
    if APos > 0 then
    begin
      R1 := ABounds;
      Dec(R1.Right, ScaleFactor.Apply(cxTextOffset));
      st := Copy(AText, APos + 1, Length(AText) - APos);
      DrawText(GetActualRectRTL(R1, ABounds, 1), st, AAlignOnVCenter[True] or AAlignHorz[UseRightToLeftAlignment], Font, TextColor);
      st := Copy(AText, 1, APos - 1);
    end;
    DrawText(GetActualRectRTL(R, ABounds, 1), st, AAlignOnVCenter[LineCount > 1] or cxAlignHCenter, LargeFont, TextColor);
  end;
  if CanDrawCurrentTime and AIsCurrent and not FExternalPainter.DrawCurrentTimeFirst then
    DrawCurrentTime(FDateTime, ABounds);
end;

procedure TcxSchedulerTimeRulerModernCellViewInfo.DrawTimeZoneLabel(const ABounds: TRect; const AText: string; ABorders: TcxBorders);
begin
  inherited DrawTimeZoneLabel(ABounds, AText, ABorders);
  DrawBottomLine(ABounds);
end;

class function TcxSchedulerTimeRulerModernCellViewInfo.GetBaseWidth(ALineCount: Integer;
  AFont1, AFont2: TFont; AScaleFactor: TdxScaleFactor; AShowMinutes: Boolean): Integer;
begin
  if not Is24HourTimeFormat or AShowMinutes then
    Result := TcxSchedulerPainterHelper.TextWidth(AFont2, ' 24') + cxTextWidth(AFont1, 'PM') + AScaleFactor.Apply(cxTextOffset)
  else
    Result := TcxSchedulerPainterHelper.TextWidth(AFont2, ' 24 ');
end;

function TcxSchedulerTimeRulerModernCellViewInfo.GetBorderColor: TColor;
begin
  Result := ExternalPainter.Painter.DefaultSchedulerTimeRulerBorderColor;
end;

function TcxSchedulerTimeRulerModernCellViewInfo.GetBottomLineIndent: Integer;
begin
  Result := ScaleFactor.Apply(7);
end;

function TcxSchedulerTimeRulerModernCellViewInfo.GetCanDrawCurrentTime(ASelectedDays: TcxSchedulerDateList): Boolean;
var
  I: Integer;
  ADate: TDate;
begin
  Result := False;
  ADate := Date;
  if ASelectedDays <> nil then
    for I := 0 to ASelectedDays.Count - 1 do
    begin
      Result := Abs(ADate - ASelectedDays[I]) < 0.99;
      if Result then
        Break;
    end;
end;

function TcxSchedulerTimeRulerModernCellViewInfo.GetLabelZoneBorders(const AIsCurrentTimeZone: Boolean): TcxBorders;
begin
  if AIsCurrentTimeZone then
    Result := [bRight]
  else
    Result := [];
end;

function TcxSchedulerTimeRulerModernCellViewInfo.GetLabelZoneTextRect(const ABounds: TRect): TRect;
begin
  Result := cxRectInflate(ABounds, -2 * cxTextOffset, -cxTextOffset);
end;

function TcxSchedulerTimeRulerModernCellViewInfo.GetTimeDisplayText(const ATime: TDateTime): string;
var
  AIsPM: Boolean;
  H, M, S: Word;
begin
  DecodeTime(ATime, H, M, S, S);
  Result := '';
  if Is24HourTimeFormat then
    Result := FormatFloat('00', H)
  else
  begin
    AIsPM := H >= 12;
    if AIsPM then Dec(H, 12);
    if H = 0 then
      Result := '12 ' + TcxSchedulerDateTimeHelper.GetMeridiem(AIsPM, True)
    else
    begin
      Result := FormatFloat('0', H);
      if InRange(Bounds[True].Top, FVisibleRect.Top, FVisibleRect.Top + cxRectHeight(Bounds[True]) - 1) then
        Result := Result + ' ' + TcxSchedulerDateTimeHelper.GetMeridiem(AIsPM, True);
    end;
  end;
end;

{ TcxSchedulerContentCellViewInfo }

constructor TcxSchedulerContentCellViewInfo.Create(APainter: TcxCustomLookAndFeelPainter; const ABounds, AVisibleRect: TRect;
  const AViewParams: TcxViewParams; AScaleFactor: TdxScaleFactor; AUseRightToLeftAlignment: Boolean);
begin
  inherited Create(APainter, ABounds, AVisibleRect, AViewParams, AScaleFactor, AUseRightToLeftAlignment);
  FTimeEvent := nil;
  FTimeLineBorders := [bLeft, bRight];
  FBorderColor := clDefault;
end;

procedure TcxSchedulerContentCellViewInfo.SetTime(const ATimeStart, ATimeFinish: TDateTime);
begin
  FDateTime := ATimeStart;
  FTimeFinish := ATimeFinish;
end;

procedure TcxSchedulerContentCellViewInfo.DoDraw;
var
  R: TRect;
begin
  if BorderColor = clDefault then
    BorderColor := GetDefaultBorderColor;
  if FShowTimeLine then
  begin
    R := Bounds;
    Inc(R.Left, ScaleFactor.Apply(cxTimeLineWidth));
    DrawRect(GetActualRectRTL(R, Bounds), Borders, BorderColor);
    R := cxRectSetRight(R, R.Left, ScaleFactor.Apply(cxTimeLineWidth));
    if FTimeEvent = nil then
      FExternalPainter.DrawTimeLine(Canvas, GetActualRectRTL(R, Bounds), FTimeLineParams, FTimeLineBorders, FTimeLineParams.TextColor)
    else
      FTimeEvent.DrawState(Canvas, GetActualRectRTL(R, Bounds), FTimeLineBorders, FTimeLineParams.TextColor);
  end
  else
    DrawRect(Bounds, Borders, BorderColor);
end;

procedure TcxSchedulerContentCellViewInfo.DoDrawWithTimeEventStateConsidering;
var
  AContentColor, ABorderColor1: TColor;
  ABounds: TRect;
begin
  AContentColor := ViewParams.Color;
  if (TimeEvent <> nil) and (TimeEvent.Event.State > 0) and not Selected then
  begin
    ABounds := Bounds;
    ABounds.Top := FFirstEventTop;
    SpecialDrawRect(cxRectSetHeight(Bounds, ABounds.Top - Bounds.Top), Borders - [bBottom], AContentColor, BorderColor);

    AContentColor := ExternalPainter.GetColorizedColor(AContentColor, TimeEvent);
    ABorderColor1 := ExternalPainter.GetColorizedColor(ExternalPainter.Painter.DefaultSchedulerHeaderContainerBorderColor, TimeEvent);
    SpecialDrawRect(ABounds, Borders - [bTop], AContentColor, BorderColor);
    if TimeEvent.Event.State = 1 then
      cxFillRectWithCustomBrush(Canvas.Canvas, StateBrushes[9], PainterHelper.ExcludeBorders(ABounds, Borders),
        AContentColor, ABorderColor1, ViewParams.Bitmap <> nil);
  end
  else
    SpecialDrawRect(Bounds, Borders, AContentColor, BorderColor);
end;

procedure TcxSchedulerContentCellViewInfo.DoRightToLeftConversion(const AClientBounds: TRect);
begin
  inherited DoRightToLeftConversion(AClientBounds);
  FTimeLineBorders := TdxRightToLeftLayoutConverter.ConvertBorders(FTimeLineBorders);
end;

function TcxSchedulerContentCellViewInfo.GetDefaultBorderColor: TColor;
begin
  Result := FDayBorderColor;
end;

procedure TcxSchedulerContentCellViewInfo.InitHitTest(AHitTest: TcxSchedulerCustomResourceViewHitTest);
begin
  inherited InitHitTest(AHitTest);
  AHitTest.SetHitTime(htcContent, FDateTime);
  AHitTest.FHitObject := Self;
  AHitTest.FHitObjectBounds := Bounds;
end;

procedure TcxSchedulerContentCellViewInfo.SetContentSelectionParameters(ASelected: Boolean; const ASelectionParams: TcxViewParams);
begin
  FSelectionColor := ASelectionParams.Color;
  FSelectionTextColor := ASelectionParams.TextColor;
  FSelected := ASelected;
end;

procedure TcxSchedulerContentCellViewInfo.SpecialDrawRect(const ARect: TRect; ABorders: TcxBorders; AContentColor, ABorderColor: TColor);
var
  AViewParams: TcxViewParams;
begin
  if Transparent or (AContentColor = clNone) then
    Canvas.FrameRect(ARect, ABorderColor, 1, ABorders)
  else
  begin
    AViewParams := FViewParams;
    AViewParams.Color := AContentColor;
    Canvas.Rectangle(ARect, AViewParams, ABorders, ABorderColor, 1);
  end;
end;

{ TcxSchedulerBackgroundSlotCellViewInfo }

procedure TcxSchedulerBackgroundSlotCellViewInfo.DoDraw;
begin
  inherited DoDraw;
end;

{ TcxSchedulerWeekDayContentCellModernViewInfo }

procedure TcxSchedulerWeekDayContentCellModernViewInfo.DoDraw;
begin
  if BorderColor = clDefault then
    BorderColor := GetDefaultBorderColor;
  if Selected then
  begin
    Canvas.FrameRect(Bounds, BorderColor, 1, Borders);
    Canvas.FillRect(PainterHelper.ExcludeBorders(Bounds, Borders), SelectionColor);
  end
  else
    DoDrawWithTimeEventStateConsidering;
end;

function TcxSchedulerWeekDayContentCellModernViewInfo.GetDefaultBorderColor: TColor;
begin
  Result := ExternalPainter.Painter.DefaultSchedulerHeaderContainerBorderColor;
end;

{ TcxSchedulerMonthDayContentCellCustomViewInfo }

destructor TcxSchedulerMonthDayContentCellCustomViewInfo.Destroy;
begin
  if FSmallFontCreated then
    FreeAndNil(FSmallTextFont);
  inherited Destroy;
end;

procedure TcxSchedulerMonthDayContentCellCustomViewInfo.SetContentState(AsDayOfMonth: Boolean; ASelected: Boolean;
  ATextHeight: Integer; const ASelectionParams: TcxViewParams);
begin
  SetContentSelectionParameters(ASelected, ASelectionParams);
  FTextRect := PainterHelper.ExcludeBorders(Bounds, Borders);
  if AsDayOfMonth then
    FDisplayText := IntToStr(DayOf(TimeStart))
  else
    FDisplayText := GetDisplayText;
  FTextRect.Bottom := Min(FTextRect.Bottom, FTextRect.Top + ATextHeight);
  if not SmallFont then
    FTextRect.Left := GetTextLeft
  else
  begin
    if FSmallTextFont = nil then
    begin
      SmallTextFont := TFont.Create;
      FSmallFontCreated := True;
      FSmallTextFont.Assign(Font);
      FSmallTextFont.Size := Round(Font.Size * 2 / 3);
    end;
    FTextRect.Left := GetTextLeftForSmallFont;
  end;
end;

function TcxSchedulerMonthDayContentCellCustomViewInfo.GetTextLeft: Integer;
begin
  Result := Max(FTextRect.Left,
    FTextRect.Right - PainterHelper.TextWidth(Font, DisplayText)) - 2 * ScaleFactor.Apply(cxTextOffset);
end;

function TcxSchedulerMonthDayContentCellCustomViewInfo.GetTextLeftForSmallFont: Integer;
begin
  Result := Max(FTextRect.Left,
    FTextRect.Right - PainterHelper.TextWidth(FSmallTextFont, DisplayText)) - ScaleFactor.Apply(cxTextOffset);
end;

procedure TcxSchedulerMonthDayContentCellCustomViewInfo.DoDraw;
begin
  inherited DoDraw;
  DrawDisplayText;
end;

procedure TcxSchedulerMonthDayContentCellCustomViewInfo.DoRightToLeftConversion(const AClientBounds: TRect);
begin
  inherited DoRightToLeftConversion(AClientBounds);
  FTextRect := TdxRightToLeftLayoutConverter.ConvertRect(FTextRect, AClientBounds);
end;

procedure TcxSchedulerMonthDayContentCellCustomViewInfo.DrawDisplayText;

  procedure InternalCalculateTextIndents(var ALeftIndent, ARightIndent: Integer);
  begin
    if not UseRightToLeftAlignment then
    begin
      ALeftIndent := GetTextLeftIndent;
      ARightIndent := GetTextRightIndent;
    end
    else
    begin
      ALeftIndent := GetTextRightIndent;
      ARightIndent := GetTextLeftIndent;
    end;
  end;

var
  AColor: Integer;
  AStyle: TFontStyles;
  R: TRect;
  ALeftIndent, ARightIndent: Integer;
begin
  if DisplayText <> '' then
  begin
    DoDrawBackground;
    if SmallFont then
      Canvas.Font := SmallTextFont
    else
      Canvas.Font := Font;
    if Selected then
      AColor := SelectionTextColor
    else
      AColor := TextColor;
    AStyle := SaveFontStyle(Canvas.Font);
    Canvas.Brush.Style := bsClear;
    R := Bounds;
    Inc(R.Left, cxTextOffset);
    Canvas.SaveClipRegion;
    try
      Canvas.IntersectClipRect(R);
      InternalCalculateTextIndents(ALeftIndent, ARightIndent);
      cxTextOut(Canvas.Handle, DisplayText, FTextRect, GetTextFlags, nil, 0, ALeftIndent, ARightIndent, AColor);
    finally
      Canvas.RestoreClipRegion;
    end;
    Canvas.Font.Style := AStyle;
  end;
end;

function TcxSchedulerMonthDayContentCellCustomViewInfo.GetDisplayText: string;
begin
  Result := dxFormatSettings.LongMonthNames[MonthOf(TimeStart)] + ', ' + IntToStr(DayOf(TimeStart));
  if cxTextWidth(Font, Result) >= (cxRectWidth(FTextRect) div 2) then
    Result := dxFormatSettings.ShortMonthNames[MonthOf(TimeStart)] + ' ' + IntToStr(DayOf(TimeStart));
end;

function TcxSchedulerMonthDayContentCellCustomViewInfo.GetIsToday: Boolean;
begin
  Result := Date = TimeStart;
end;

procedure TcxSchedulerMonthDayContentCellCustomViewInfo.SetSmallTextFont(AFont: TFont);
begin
  if FSmallFontCreated then
     FreeAndNil(FSmallTextFont);
  FSmallTextFont := AFont;
end;

function TcxSchedulerMonthDayContentCellCustomViewInfo.GetTextLeftIndent: Integer;
begin
  Result := 0;
end;

function TcxSchedulerMonthDayContentCellCustomViewInfo.GetTextRightIndent: Integer;
begin
  Result := 0;
end;

function TcxSchedulerMonthDayContentCellCustomViewInfo.SaveFontStyle(AFont: TFont): TFontStyles;
begin
  Result := AFont.Style;
end;

function TcxSchedulerMonthDayContentCellCustomViewInfo.UpdateSelection(ASelected: Boolean): Boolean;
begin
  Result := FSelected <> ASelected;
  if Result then
    FSelected := ASelected;
end;

{ TcxSchedulerMonthDayContentCellViewInfo }

function TcxSchedulerMonthDayContentCellViewInfo.GetTextFlags: Cardinal;
begin
  Result := CXTO_PREVENT_LEFT_EXCEED or CXTO_PREVENT_TOP_EXCEED or CXTO_CENTER_VERTICALLY or CXTO_RIGHT;
end;

procedure TcxSchedulerMonthDayContentCellViewInfo.DoDrawBackground;
var
  R: TRect;
begin
  R := PainterHelper.ExcludeBorders(Bounds, Borders);
  R.Bottom := FTextRect.Bottom;
  if not Transparent and IsToday then
    PainterHelper.DrawGradientRect(Canvas, Color, R);
  if Selected then
  begin
    Inc(R.Top);
    InflateRect(R, -ScaleFactor.Apply(1), 0);
    Canvas.SetBrushColor(SelectionColor);
    if not IsToday then
      Canvas.FillRect(R)
    else
    begin
      R := FTextRect;
      R.Left := Max(Bounds.Left, (R.Left - 2 * ScaleFactor.Apply(cxTextOffset)));
      Inc(R.Bottom);
      InflateRect(R, -ScaleFactor.Apply(1), -ScaleFactor.Apply(1));
      Canvas.FillRect(R);
    end;
  end;
end;

function TcxSchedulerMonthDayContentCellViewInfo.GetTextRightIndent: Integer;
begin
  Result := ScaleFactor.Apply(cxTextOffset + 1);
end;

{ TcxSchedulerMonthDayContentCellModernViewInfo }

procedure TcxSchedulerMonthDayContentCellModernViewInfo.DoDraw;
begin
  if BorderColor = clDefault then
    BorderColor := GetDefaultBorderColor;
  DoDrawWithTimeEventStateConsidering;
  DrawDisplayText;
end;

procedure TcxSchedulerMonthDayContentCellModernViewInfo.DoDrawBackground;
var
  R, ATodayRect: TRect;
begin
  R := PainterHelper.ExcludeBorders(Bounds, Borders);
  if IsToday then
  begin
    ATodayRect := R;
    ATodayRect.Bottom := FTextRect.Bottom;
    Canvas.FillRect(ATodayRect, ExternalPainter.Painter.DefaultSchedulerHeaderContainerAlternateBackgroundColor);
    R.Top := ATodayRect.Bottom;
  end;
  if Selected then
    Canvas.FillRect(R, SelectionColor);
end;

function TcxSchedulerMonthDayContentCellModernViewInfo.GetDefaultBorderColor: TColor;
begin
  Result := ExternalPainter.Painter.DefaultSchedulerHeaderContainerBorderColor;
end;

function TcxSchedulerMonthDayContentCellModernViewInfo.GetDisplayText: string;
begin
  Result := FormatDateTime('d mmm', TimeStart);
  if cxTextWidth(Font, Result) >= (cxRectWidth(FTextRect) div 2) then
    Result := IntToStr(DayOf(TimeStart));
end;

function TcxSchedulerMonthDayContentCellModernViewInfo.GetTextFlags: Cardinal;
const
  AHorizontalAlignment: array[Boolean] of Word = (CXTO_LEFT, CXTO_RIGHT);
begin
  Result := CXTO_PREVENT_LEFT_EXCEED or CXTO_PREVENT_TOP_EXCEED or CXTO_CENTER_VERTICALLY or
    AHorizontalAlignment[UseRightToLeftAlignment];
end;

function TcxSchedulerMonthDayContentCellModernViewInfo.GetTextLeft: Integer;
begin
  Result := Bounds.Left + ScaleFactor.Apply(cxTimeLineWidth);
end;

function TcxSchedulerMonthDayContentCellModernViewInfo.GetTextLeftForSmallFont: Integer;
begin
  Result := FTextRect.Left + ScaleFactor.Apply(cxTextOffset);
end;

function TcxSchedulerMonthDayContentCellModernViewInfo.SaveFontStyle(AFont: TFont): TFontStyles;
begin
  Result := inherited SaveFontStyle(AFont);
  if IsToday  or (DayOf(TimeStart) = 1) then
    AFont.Style := AFont.Style + [fsBold];
end;

{ TcxSchedulerEventCellViewInfo }

constructor TcxSchedulerEventCellViewInfo.Create(AViewData: TcxSchedulerEventViewData; ACanResize: Boolean;
  AScaleFactor: TdxScaleFactor; AUseRightToLeftAlignment: Boolean);
begin
  ViewData := AViewData;
  FExternalPainter := AViewData.ExternalPainter;
  inherited Create(AViewData.Painter, AViewData.Bounds, AViewData.VisibleRect, AViewData.ViewParams, AScaleFactor, AUseRightToLeftAlignment);
  FSeparatorColor := clBtnShadow;
  FCanResize := ACanResize;
  FCanvas := AViewData.Canvas;
  FImages := TcxSchedulerEventImages.Create(AViewData.ImagesLayout, AScaleFactor);
  Initialize;
end;

destructor TcxSchedulerEventCellViewInfo.Destroy;
begin
  ViewData.Free;
  FImages.Free;
  FEditViewInfo.Free;
  inherited Destroy;
end;

procedure TcxSchedulerEventCellViewInfo.DrawSelection;
var
  R: TRect;
  ARgn: TcxRegion;
  ABorders: TcxBorders;
begin
  Canvas.SaveClipRegion;
  try
    R := FSelectionBorderRect;
    ARgn := TcxRegion.Create(R);
    try
      ARgn.Combine(ClipRect, roIntersect);
      Canvas.SetClipRegion(ARgn, roSet, False);
      DrawSizingHandler(FSizingHandlerRect1, ARgn);
      DrawSizingHandler(FSizingHandlerRect2, ARgn);
      Canvas.SetClipRegion(ARgn, roSet, False);
      ABorders := Borders;
      Canvas.FrameRect(R, GetSelectedBorderColor, SelectionBorderSize, ABorders);
    finally
      ARgn.Free;
    end;
  finally
    Canvas.RestoreClipRegion;
  end;
end;

procedure TcxSchedulerEventCellViewInfo.DrawSizingHandler(const R: TRect; var ARgn: TcxRegion);
begin
  if cxRectIsNull(R) then
    Exit;
  Canvas.FrameRect(R, GetSelectedBorderColor, 1);
  ARgn.Combine(R, roSubtract);
end;

procedure TcxSchedulerEventCellViewInfo.DrawState;
const
  ALeftBorders: array[Boolean] of TcxBorders = ([bLeft], [bRight]);
begin
  if NeedBorderAroundOfTimeLineRect then
    DrawEventBorderAroundOfTimeLineRect;
  if HasLeftBorder and not ActualBorderColorIsNone then
    DrawFrame(Bounds, ALeftBorders[UseRightToLeftAlignment], BorderColor, BorderSize);
  DrawState(Canvas, FTimeLineRect, [], GetTimeLineBorderColor);
end;

procedure TcxSchedulerEventCellViewInfo.DrawState(ACanvas: TcxCanvas;
  const ARect: TRect; ABorders: TcxBorders; ABorderColor: TColor);
begin
  PainterHelper.DrawState(ACanvas, ARect, Event.State, ABorders, ABorderColor, ViewStyle);
end;

function TcxSchedulerEventCellViewInfo.MeasureHeight(ACanvas: TcxCanvas): Integer;
var
  L: TList;
begin
  Inc(FCalculationCounter);
  try
    Canvas.Font := Font;
    FIsHeaderEvent := GetIsHeaderEvent;
    CalculateVisibility;
    CalculateItemsLayoutForMeasureHeight;
    L := Images.CreateVisibleList;
    try
      case Images.Layout of
        eilHorizontal:
          Result := CalculateHorizontalImagesAutoHeight;
        eilVertical:
          Result := CalculateVerticalImagesAutoHeight;
        else //eilAuto:;
          Result := CalculateAutoLayoutImagesAutoHeight(L);
      end;
    finally
      L.Free;
    end;
    Result := Max(ViewData.LineHeight, Result);
  finally
    Dec(FCalculationCounter);
  end;
end;

procedure TcxSchedulerEventCellViewInfo.MoveTo(X, Y: Integer);
var
  I: Integer;
begin
  ShiftRect(ViewData.Bounds, X, Y);
  CalculateCellBounds(ViewData.Bounds, ViewData.VisibleRect);
  if not Visible or Hidden then Exit;
  ShiftRect(FCaptionRect, X, Y);
  ShiftRect(FFinishRect, X, Y);
  ShiftRect(FMessageRect, X, Y);
  ShiftRect(FStartRect, X, Y);
  ShiftRect(FEventTimeRect, X, Y);
  ShiftRect(FTimeLineRect, X, Y);
  ShiftRect(FSelectionBorderRect, X, Y);
  ShiftRect(FSizingHandlerRect1, X, Y);
  ShiftRect(FSizingHandlerRect2, X, Y);
  ShiftRect(FStartContinueArrowRect, X, Y);
  ShiftRect(FStartContinueArrowCaptionRect, X, Y);
  ShiftRect(FEndContinueArrowRect, X, Y);
  ShiftRect(FEndContinueArrowCaptionRect, X, Y);
  ShiftRect(FLocationRect, X, Y);
  if FShowMessage then
    CalculateEditViewInfo;
  for I := 0 to Images.Count - 1 do
    ShiftRect(Images[I].FBounds, X, Y);
end;

procedure TcxSchedulerEventCellViewInfo.AfterDraw;
begin
end;

procedure TcxSchedulerEventCellViewInfo.AssignEditStyle(
  AEditStyle: TcxCustomEditStyle);
var
  AStyle: TcxCustomEditStyleAccess;
begin
  AStyle := TcxCustomEditStyleAccess(AEditStyle);
  AStyle.FAssignedValues := AStyle.FAssignedValues -
    [svFont] + [svColor, svButtonTransparency];
  AStyle.StyleData.Font := Font;
  AStyle.StyleData.Color := GetEditStyleColor;
  AStyle.StyleData.FontColor := TextColor;
  AStyle.ButtonTransparency :=  ebtHideInactive;
  AStyle.Changed;
end;

procedure TcxSchedulerEventCellViewInfo.BeforeCustomDraw(ACanvas: TcxCanvas);
begin
  FViewParams := ViewData.ViewParams;
  inherited BeforeCustomDraw(ACanvas);
end;

procedure TcxSchedulerEventCellViewInfo.BeforeDraw;
begin
  // do nothing
end;

procedure TcxSchedulerEventCellViewInfo.Calculate;
begin
  Inc(FCalculationCounter);
  try
    Canvas.Font := Font;
    CalculateBorderAttributes;
    CalculateItemsLayout;
    if Selected then
      CalculateSelectionBorderParams;
  finally
    Dec(FCalculationCounter);
  end;
end;

procedure TcxSchedulerEventCellViewInfo.CalculateBorderAttributes;
begin
  FBorderSize := ExternalPainter.GetEventBorderSize(Self);
end;

procedure TcxSchedulerEventCellViewInfo.CalculateBorders;
begin
end;

procedure TcxSchedulerEventCellViewInfo.CalculateCaptions;
begin
  FStartText := DateTimeHelper.TimeToStr(EventStart);
  FFinishText := DateTimeHelper.TimeToStr(EventFinish);
end;

function TcxSchedulerEventCellViewInfo.CalculateClipRect(const ABounds, AVisibleRect: TRect): TRect;
begin
  Result := inherited CalculateClipRect(cxRectInflate(ABounds, GetSelectionBoundsExtends), AVisibleRect);
end;

procedure TcxSchedulerEventCellViewInfo.CalculateDetailInfo;
begin
  FIsDetailInfo := GetDetailInfoFlagValue;
  FIsDetailCaption := GetDetailCaptionFlagValue;
  FShowMessage := FIsDetailInfo and (Length(Message) > 0);
  FDetailCaptionVertOffset := ScaleFactor.Apply(cxTextOffset + 1);
end;

procedure TcxSchedulerEventCellViewInfo.CalculateNeedHint;
var
  R: TRect;
begin
  if FHintNeededCalculated then Exit;
  FHintNeeded := CanShowHint;
  if not FHintNeeded then
  begin
    R := FCaptionRect;
    MeasureCaptionExtent(R);
    FHintNeeded := (FCaptionRect.Right < R.Right) or (FCaptionRect.Bottom < R.Bottom);
  end;
  FHintNeededCalculated := True;
end;

procedure TcxSchedulerEventCellViewInfo.CalculateEditViewInfo;
begin
  if not ViewData.AutoHeight then
  begin
    if not FCaptionAndLocationCanPlaceOneLineOnly and (LocationRect.Bottom > LocationRect.Top) then
    begin
      FMessageRect := GetMessageRect(LocationRect, Images.VisibleImageCount > 0);
      FCaptionRect.Bottom := FLocationRect.Top - 1;
      FLocationRect.Bottom := Min(FMessageRect.Top, Bounds.Bottom - ScaleFactor.Apply(cxTextOffset));
    end
    else
    begin
      FMessageRect := GetMessageRect(CaptionRect, Images.VisibleImageCount > 0);
      FCaptionRect.Bottom := Min(FMessageRect.Top, Bounds.Bottom - ScaleFactor.Apply(cxTextOffset));
    end;
  end;
  if cxRectIsEmpty(FMessageRect) then
    FShowMessage := False;
  if FShowMessage then
    InitializeEditViewInfo(FMessageRect);
end;

procedure TcxSchedulerEventCellViewInfo.CalculateActualEditRect(AEditViewData: TcxCustomEditViewData; var AMessageRect: TRect);
begin
// do nothing
end;

procedure TcxSchedulerEventCellViewInfo.InitializeEditViewInfo(var AMessageRect: TRect);
var
  AProperties: TcxCustomEditProperties;
  AEditViewData: TcxCustomEditViewData;
begin
  if FEditViewInfo = nil then
    FEditViewInfo := TcxCustomEditViewInfo(ViewData.EditProperties.GetViewInfoClass.Create);
  AProperties := ViewData.EditProperties;
  AEditViewData := AProperties.CreateViewData(ViewData.EditStyle, True);
  try
    AEditViewData.ScaleFactor.Assign(ScaleFactor);
    AEditViewData.UseRightToLeftAlignment := UseRightToLeftAlignment;
    Include(AEditViewData.PaintOptions, epoAutoHeight);
    AssignEditStyle(ViewData.EditStyle);
    AEditViewData.ContentOffset := cxSimpleRect;
    CalculateActualEditRect(AEditViewData, AMessageRect);
    AEditViewData.EditValueToDrawValue(Message, FEditViewInfo);
    AEditViewData.CalculateEx(Canvas, AMessageRect, cxInvalidPoint, cxmbNone, [], FEditViewInfo, False);
  finally
    AEditViewData.Free;
  end;
end;

procedure TcxSchedulerEventCellViewInfo.CalculateShowTimeAsClock;
begin
  ViewData.ShowTimeAsClock := IsHeaderEvent or (ViewData.ShowTimeAsClock and not IsDetailInfo);
end;

procedure TcxSchedulerEventCellViewInfo.CalculateSelectionBorderParams;
begin
  if not EnableModernSelection then
    Exit;
  FSelectionBorderSize := Max(ExternalPainter.GetEventSelectionBorderSize(Self), ScaleFactor.Apply(2));
  FSelectionBorderRect := cxRectInflate(Bounds, ExternalPainter.GetEventSelectionExtends(Self));
  if not IsDrawBySkin and not cxRectIsNull(TimeLineRect) then
    FSelectionBorderRect.Left := TimeLineRect.Left - GetOffsetSelectionBorderFromTimeLineRect;
  if CanBeExcludedSelectionLeftBorder and not HasLeftBorder then
    FSelectionBorderRect.Left := Bounds.Left;
  if CanBeExcludedSelectionRightBorder and not HasRightBorder then
    FSelectionBorderRect.Right := Bounds.Right;
  CalculateSizingHandlers;
end;

procedure TcxSchedulerEventCellViewInfo.CalculateSizingHandlers;
var
  R: TRect;
  M: Integer;
  ASize: Integer;
begin
  if not(Selected and FCanResize) or Event.IsClone then
    Exit;
  ASize := ScaleFactor.Apply(3) + 2;
  R := cxRect(cxSize(ASize, ASize));
  if not IsHorzSizing then
  begin
    M := (SelectionBorderRect.Left + FSelectionBorderRect.Right) div 2;
    if NeedTopSizingHandler then
      FSizingHandlerRect1 := cxRectSetOrigin(R, cxPoint(M - ASize div 2, FSelectionBorderRect.Top - 1));
    if NeedBottomSizingHandler then
      FSizingHandlerRect2 := cxRectSetOrigin(R, cxPoint(M - ASize div 2, FSelectionBorderRect.Bottom + 1 - ASize));
  end
  else
  if HasLeftBorder or HasRightBorder then
  begin
    M := (FSelectionBorderRect.Top + FSelectionBorderRect.Bottom) div 2;
    if NeedLeftSizingHandler then
      FSizingHandlerRect1 := cxRectSetOrigin(R, cxPoint(FSelectionBorderRect.Left - 1, M - ASize div 2));
    if NeedRightSizingHandler then
      FSizingHandlerRect2 := cxRectSetOrigin(R, cxPoint(FSelectionBorderRect.Right + 1 - ASize, M - ASize div 2));
  end
end;

function TcxSchedulerEventCellViewInfo.CanBeExcludedSelectionLeftBorder: Boolean;
begin
  Result := False;
end;

function TcxSchedulerEventCellViewInfo.CanBeExcludedSelectionRightBorder: Boolean;
begin
  Result := False;
end;

function TcxSchedulerEventCellViewInfo.CanMoveLeftBorder: Boolean;
begin
  Result := HasLeftBorder;
end;

function TcxSchedulerEventCellViewInfo.CanMoveRightBorder: Boolean;
begin
  Result := HasRightBorder;
end;

function TcxSchedulerEventCellViewInfo.CalculateAutoLayoutImagesAutoHeight(
  AVisibleImages: TList): Integer;

  function GetEventTextsHeight(const AWidth: Integer): Integer;
  var
    R: TRect;
  begin
    R := cxRect(0, 0, AWidth, MaxInt);
    Result := GetCaptionAutoHeight(R) + 2 * ScaleFactor.Apply(cxTextOffset) + MeasureAutoLayoutImagesLocationHeight(R);
    if ShowMessage then
    begin
      Inc(R.Right, ScaleFactor.Apply(3));
      Inc(Result, CalculateMessageHeight(R) + ScaleFactor.Apply(1));
    end;
  end;

  function GetColCountForRowCount(ARowCount: Integer): Integer;
  begin
    Result := Images.TotalVisibleImageCount div ARowCount;
    if (Images.TotalVisibleImageCount mod ARowCount) > 0 then
      Inc(Result);
    Result := Max(1, Result);
  end;

var
  I, ANewColCount, ACaptionWidth, ACaptionOnlyWidth, ASpaceWidth, AImagesHeight,
  AImagesWidth, AImagesColCount, AImagesRowCount,
  ATextsHeight: Integer;
begin
  ASpaceWidth := cxRectWidth(GetAvailableBounds) - 2 * ScaleFactor.Apply(cxTextOffset);
  CalculateHeaderEventNeededCaptionWidth(ACaptionWidth, ACaptionOnlyWidth);
  if (Images.TotalVisibleImageCount < 2) or
   (ASpaceWidth >= Images.TotalVisibleWidth + ScaleFactor.Apply(cxEventImagesGap) + ACaptionWidth) then
  begin
    Result := CalculateHorizontalImagesAutoHeight;
    Exit;
  end;
  CheckItemsBeforeAutoLayoutImagesCalculation;
  AImagesWidth := 0;
  AImagesRowCount := 1;
  AImagesColCount := 0;
  for I := 1 to Images.TotalVisibleImageCount do
  begin
    AImagesRowCount := I;
    AImagesHeight := Min(Images.TotalVisibleImageCount, (AImagesRowCount + ScaleFactor.Apply(1))) *
      (Images.ItemHeight + ScaleFactor.Apply(cxEventImagesGap)) + ScaleFactor.Apply(4);
    ANewColCount := GetColCountForRowCount(AImagesRowCount);
    if ANewColCount <> AImagesColCount then
    begin
      AImagesColCount := ANewColCount;
      AImagesWidth := AImagesColCount * (Images.ItemWidth + ScaleFactor.Apply(cxEventImagesGap)) + ScaleFactor.Apply(1);
      if ASpaceWidth <= AImagesWidth then Continue;
      ATextsHeight := GetEventTextsHeight(ASpaceWidth - AImagesWidth);
      if (ATextsHeight < AImagesHeight) and CheckAutoLayoutImagesSingleLineCaption(ACaptionOnlyWidth, ASpaceWidth - AImagesWidth) then
        Break;
    end
    else Continue;
  end;
  Result := AImagesRowCount * (Images.ItemHeight + ScaleFactor.Apply(cxEventImagesGap)) + ScaleFactor.Apply(1);
  Images.CalculateForCols(AVisibleImages,
    cxPointOffset(GetAvailableBounds.TopLeft, ScaleFactor.Apply(cxEventImagesOffset) + cxEventBorderWidth,
    ScaleFactor.Apply(cxEventImagesOffset) + cxEventBorderWidth), AImagesColCount);
  CheckItemsAfterAutoLayoutImagesCalculation(AImagesRowCount, AImagesWidth);
  CalculateMessageAutoLayout(CaptionRect, Result);
end;

procedure TcxSchedulerEventCellViewInfo.CalculateHeaderEventContinueArrows(ACompressing: Integer);
var
  AOffset: Integer;
begin
  InitializeHeaderEventContinueArrowsData;
  if (ACompressing = 2) or not CanBeHeaderEventContinueArrows then
    Exit;
  AOffset := ScaleFactor.Apply(cxTextOffset);
  if NeedHeaderEventStartContinueArrow then
  begin
    FStartContinueArrowRect := GetBoundsWithoutTimeLineRect;
    Inc(FStartContinueArrowRect.Left, AOffset);
    FStartContinueArrowRect.Right := FStartContinueArrowRect.Left + ScaleFactor.Apply(TcxSchedulerPainterHelper.GetStartContinueArrowSize.cx);
    if ACompressing = 0 then
    begin
      FStartContinueArrowCaption := cxGetResourceString(@scxContinueFrom) + ' ' +
        FormatDateTime(GetContinueArrowCaptionDateFormat, EventStart);
      FStartContinueArrowCaptionRect := FStartContinueArrowRect;
      FStartContinueArrowCaptionRect.Left := FStartContinueArrowRect.Right + AOffset;
      FStartContinueArrowCaptionRect.Right := FStartContinueArrowCaptionRect.Left +
        cxTextWidth(Canvas.Font, FStartContinueArrowCaption);
    end;
  end;
  if NeedHeaderEventFinishContinueArrow then
  begin
    FEndContinueArrowRect := GetTimeLineRectBaseBounds;
    Dec(FEndContinueArrowRect.Right, AOffset);
    FEndContinueArrowRect.Left := FEndContinueArrowRect.Right - ScaleFactor.Apply(TcxSchedulerPainterHelper.GetStartContinueArrowSize.cx);
    if ACompressing = 0 then
    begin
      FEndContinueArrowCaption := cxGetResourceString(@scxContinueTo) + ' ' +
        FormatDateTime(GetContinueArrowCaptionDateFormat, IfThen(TimeOf(EventFinish) > 0, EventFinish, EventFinish - 1));
      FEndContinueArrowCaptionRect := FEndContinueArrowRect;
      FEndContinueArrowCaptionRect.Right := FEndContinueArrowRect.Left - AOffset;
      FEndContinueArrowCaptionRect.Left := FEndContinueArrowCaptionRect.Right -
        cxTextWidth(Canvas.Font, FEndContinueArrowCaption);
    end;
  end;
end;

function TcxSchedulerEventCellViewInfo.CalculateHorizontalImagesAutoHeight: Integer;
var
  ARightLimit, ACaptionWidth, ACaptionHeight, ASpaceWidth,
  AMessageTop, AImagesWidth: Integer;
  AImagesRect, R: TRect;
begin
  Result := Images.ItemHeight + ScaleFactor.Apply(cxTextOffset);
  ASpaceWidth := cxRectWidth(Bounds) - 2 * ScaleFactor.Apply(cxTextOffset);
  ACaptionWidth := cxTextWidth(ViewParams.Font, Caption);
  AImagesRect := cxRectInflate(Bounds, -ScaleFactor.Apply(cxTextOffset), -ScaleFactor.Apply(cxTextOffset));
  if Images.ForceVisibleWidth + ScaleFactor.Apply(cxEventImagesGap) >= ASpaceWidth then
  begin
    FCaptionRect := cxNullRect;
    Images.CalculateSingleLineImages(AImagesRect);
    AMessageTop := Bounds.Top + ScaleFactor.Apply(cxTextOffset + cxEventImagesGap) + Result;
  end
  else
  begin
    ARightLimit := 0;
    if not (Images.TotalVisibleWidth + ScaleFactor.Apply(cxEventImagesGap) + ACaptionWidth <= ASpaceWidth) then
    begin
      R := FCaptionRect;
      R.Left := Min(AImagesRect.Left + Images.TotalVisibleWidth, R.Right - 1);
      Canvas.TextExtent(Caption, R, cxWordBreak or cxDontBreakChars);
      if R.Right > FCaptionRect.Right then
      begin
        ACaptionWidth := cxRectWidth(R);
        Dec(AImagesRect.Right, ACaptionWidth);
        ARightLimit := Bounds.Right - ScaleFactor.Apply(cxTextOffset) - cxRectWidth(R);
      end;
    end;
    AImagesWidth := Images.CalculateSingleLineImages(AImagesRect, ARightLimit);
    if AImagesWidth > 0 then Inc(AImagesWidth);
    Inc(FCaptionRect.Left, AImagesWidth);
    ACaptionHeight := GetCaptionAutoHeight(CaptionRect);
    FCaptionRect.Bottom := FCaptionRect.Top + ACaptionHeight;
    Result := Max(Result, ACaptionHeight + 2 * ScaleFactor.Apply(cxTextOffset));
    AMessageTop := FCaptionRect.Top + Result;
  end;
  Result := Result + GetHorizontalImagesMessageAutoHeight(AMessageTop);
end;

procedure TcxSchedulerEventCellViewInfo.CalculateLocationAutoLayout;
begin
  CalculateLocationRect;
end;

procedure TcxSchedulerEventCellViewInfo.CalculateMessageAutoLayout(const R: TRect; var AHeight: Integer);
var
  ACaptionHeight, AMessageHeight, AMessageTop: Integer;
begin
  if not cxRectIsEmpty(R) then
  begin
    ACaptionHeight := GetCaptionAutoHeight(CaptionRect);
    if NeedCorrectCaptionBottomWhenMessageAutoLayoutCalculating then
    begin
      FCaptionRect.Bottom := CaptionRect.Top + ACaptionHeight;
      Inc(ACaptionHeight, 2 * ScaleFactor.Apply(cxTextOffset));
    end;
    CalculateLocationAutoLayout;
    if FCaptionAndLocationCanPlaceOneLineOnly then
      ACaptionHeight := Max(ACaptionHeight, cxRectHeight(FLocationRect))
    else
      ACaptionHeight := Max(cxRectHeight(CaptionRect), ACaptionHeight + cxRectHeight(FLocationRect));
    AHeight := Max(AHeight, ACaptionHeight);
    if FShowMessage then
    begin
      AMessageTop := FCaptionRect.Top +  ACaptionHeight - ScaleFactor.Apply(1);
      FMessageRect := cxRect(R.Left - ScaleFactor.Apply(2), AMessageTop, Bounds.Right - ScaleFactor.Apply(1), 0);
      AMessageHeight := CalculateMessageHeight(FMessageRect);
      FMessageRect.Bottom := FMessageRect.Top + AMessageHeight;
      AHeight := Max(AHeight, ACaptionHeight + AMessageHeight);
      CalculateEditViewInfo;
    end;
  end
  else FShowMessage := False;
end;

function TcxSchedulerEventCellViewInfo.CalculateVerticalImagesAutoHeight: Integer;
var
  AImagesRect: TRect;
begin
  Result := Images.ItemHeight + ScaleFactor.Apply(cxTextOffset);
  if Images.TotalVisibleHeight > 0 then
  begin
    AImagesRect := cxRectInflate(Bounds, -ScaleFactor.Apply(cxTextOffset), -ScaleFactor.Apply(cxTextOffset));
    AImagesRect.Bottom := AImagesRect.Top + Images.TotalVisibleHeight;
    if (cxRectWidth(AImagesRect) > Images.ItemWidth + ScaleFactor.Apply(cxEventImagesGap)) or
       (Images.ForceVisibleHeight > 0) then
    begin
      Inc(FCaptionRect.Left, Images.ItemWidth + ScaleFactor.Apply(cxTextOffset));
      Result := Images.TotalVisibleHeight + ScaleFactor.Apply(cxEventImagesGap);
    end;
    Images.CalculateSingleColumnImages(AImagesRect);
  end;
  CalculateMessageAutoLayout(CaptionRect, Result);
end;

procedure TcxSchedulerEventCellViewInfo.CalculateItemsLayout;
begin
  if ViewData.ShowTimeLine then
    FBounds.Left := ViewData.Bounds.Left + ScaleFactor.Apply(cxTimeLineWidth);
  if IsDetailInfo then
    CalculateDetailViewEventLayout
  else
    CalculateHeaderEventLayout;
  if ViewData.ShowTimeLine then
    CalculateTimeLineLayout;
end;

procedure TcxSchedulerEventCellViewInfo.CalculateDetailViewEventLayout;
var
  AImagesWidth: Integer;
begin
  FCaptionRect := cxRectInflate(Bounds, -ScaleFactor.Apply(cxTextOffset), -FDetailCaptionVertOffset);
  AImagesWidth := Images.Calculate(GetImagesBounds);
  Images.Offset(GetImagesHorizontalOffset, GetImagesVerticalOffset(Images.ItemHeight, False));
  Inc(FCaptionRect.Left, AImagesWidth + ScaleFactor.Apply(cxEventImagesGap));
  CalculateLocationRect;
  if FShowMessage then
    CalculateEditViewInfo;
end;

procedure TcxSchedulerEventCellViewInfo.CalculateHeaderEventNeededCaptionWidth(var AFullWidth, ACaptionOnlyWidth: Integer);
begin
  AFullWidth := cxTextWidth(Canvas.Font, ViewData.Caption);
  ACaptionOnlyWidth := cxTextWidth(Canvas.Font, Event.Caption);
end;

procedure TcxSchedulerEventCellViewInfo.CalculateHeaderEventLayout;
var
  ACaptionRect: TRect;
  ALeft, AWidth, AImagesWidth, AFullCaptionWidth, ACaptionOnlyWidth, AVertOffset, ACompressing: Integer;
  ACanShowClock, ACanShowTimeText: Boolean;
  R: TRect;
  ATextOffset: Integer;

  procedure CalculateCaption;
  var
    R: TRect;
  begin
    InflateRect(ACaptionRect, -ScaleFactor.Apply(cxTextOffset), 0);
    R := GetHeaderImagesPossibleRect(ACaptionRect, AFullCaptionWidth);
    AImagesWidth := Images.CalculateSingleLineImages(R);
    if AImagesWidth <> 0 then Inc(AImagesWidth, ScaleFactor.Apply(cxTextOffset));
    AWidth := AImagesWidth + AFullCaptionWidth;
  end;

begin
  ATextOffset := ScaleFactor.Apply(cxTextOffset);
  for ACompressing := 0 to 2 do
  begin
    CalculateHeaderEventContinueArrows(ACompressing);
    FCaptionRect := GetHeaderEventPossibleCaptionRect;
    ACaptionRect := FCaptionRect;
    CalculateHeaderEventNeededCaptionWidth(AFullCaptionWidth, ACaptionOnlyWidth);
    if (ACaptionOnlyWidth > cxRectWidth(ACaptionRect)) and (ACompressing < 2) then
      Continue;
    R := ScaleFactor.Apply(cxRect(0, 0, EventImages.Width, EventImages.Height));
    AVertOffset := GetImagesVerticalOffset(R.Bottom, True);
    ACanShowClock := CanShowHeaderEventClock(ACaptionRect);
    ACanShowTimeText := not ACanShowClock and CanShowHeaderEventTimeText(ACaptionRect);
    if ShowStartTime and (ACanShowClock or ACanShowTimeText) then
    begin
      if ACanShowClock  then
        FStartRect := cxRectOffset(R, GetHeaderEventStartClockOriginLeft +
          ScaleFactor.Apply(cxEventImagesOffset) + cxEventBorderWidth, AVertOffset)
      else
      begin
        FStartRect := Bounds;
        FStartRect.Left := GetHeaderEventStartClockOriginLeft + ATextOffset + cxEventBorderWidth;
        FStartRect.Right := FStartRect.Left + GetTimeTextMaxWidth +  ATextOffset;
      end;
      ACaptionRect.Left := FStartRect.Right;
    end;
    if ShowFinishTime and (ACanShowClock or ACanShowTimeText) then
    begin
      if ACanShowClock then
        FFinishRect := cxRectOffset(R, GetHeaderEventFinishClockOriginRight - R.Right -
          ScaleFactor.Apply(cxEventImagesOffset) - cxEventBorderWidth, AVertOffset)
      else
      begin
        FFinishRect := Bounds;
        FFinishRect.Right := GetHeaderEventFinishClockOriginRight - ATextOffset - cxEventBorderWidth;
        FFinishRect.Left := FFinishRect.Right - GetTimeTextMaxWidth - ATextOffset;
      end;
      ACaptionRect.Right := FFinishRect.Left;
    end;
    CalculateCaption;
    if (ShowStartTime or ShowFinishTime) and
      (AFullCaptionWidth + Images.TotalVisibleWidth > cxRectWidth(ACaptionRect)) and
      not GetForceShowClockInHeaderEvent then
    begin
      FStartRect := cxEmptyRect;
      FFinishRect := cxEmptyRect;
      ViewData.ShowStartTime := False;
      ViewData.ShowFinishTime := False;
      ACaptionRect := FCaptionRect;
      CalculateCaption;
    end;
    ALeft := GetHeaderEventCaptionLeft(ACaptionRect, AWidth);
    Images.Offset(ALeft, GetImagesVerticalOffset(Images.ItemHeight, False));
    ACaptionRect.Left := ALeft + AImagesWidth;
    if (ACaptionOnlyWidth <= cxRectWidth(ACaptionRect)) or
       ((EventStart >= ContentStart) and (EventFinish <= ContentFinish)) then
      Break;
  end;
  FCaptionRect := ACaptionRect;
  CalculateLocationRect;
end;

function TcxSchedulerEventCellViewInfo.GetNonDetailEventSingleLineCaptionWidth: Integer;
begin
  Result := cxTextWidth(ViewParams.Font, ViewData.Caption);
end;

procedure TcxSchedulerEventCellViewInfo.CalculateNonDetailEventLayout;
var
  R: TRect;
  ALeft, ACaptionWidth, AImagesWidth, AOffset: Integer;
begin
  FCaptionRect := GetNonDetailEventPossibleCaptionRect;
  ALeft := CaptionRect.Left;
  ACaptionWidth := GetNonDetailEventSingleLineCaptionWidth;
  R := CalculateNonDetailEventImages(ACaptionWidth, AImagesWidth);
  if ShowFinishTime or ShowStartTime then
  begin
    if ShowTimeAsClock then
      CalculateEventTimeAsClockLayout(R, ACaptionWidth, AImagesWidth, ALeft)
    else
      CalculateEventTimeAsTextLayout(R, ACaptionWidth, AImagesWidth, ALeft);
  end;
  AOffset := ALeft - FCaptionRect.Left;
  if IsTimeLineVisible then
    Inc(AOffset, ScaleFactor.Apply(cxTimeLineWidth + cxTextOffset));
  AImagesWidth := Images.Offset(AOffset, GetImagesVerticalOffset(Images.ItemHeight, False));
  if AImagesWidth > 0 then Inc(AImagesWidth);
  Inc(ALeft, AImagesWidth + ScaleFactor.Apply(cxEventImagesOffset));
  FCaptionRect.Left := ALeft;
  CalculateLocationRect;
end;

procedure TcxSchedulerEventCellViewInfo.CalculateEventTimeAsClockLayout(
  const ABounds: TRect; const ACaptionWidth, AImagesWidth: Integer; var ALeft: Integer);
var
  R: TRect;
  AVertOffset: Integer;
begin
  R := cxRectBounds(0, 0, TcxSchedulerPainterHelper.IconsWidth(ScaleFactor),
    TcxSchedulerPainterHelper.IconsHeight(ScaleFactor));
  AVertOffset := GetImagesVerticalOffset(R.Bottom, True);
  FStartRect := SetItemRect(ShowStartTime, R, AVertOffset, ALeft);
  FFinishRect := SetItemRect(ShowFinishTime, R, AVertOffset, ALeft);
  Inc(ALeft);
end;

function TcxSchedulerEventCellViewInfo.GetTimeTextMaxWidth: Integer;
begin
  Result := Max(
    Canvas.TextWidth('00:00' + dxFormatSettings.TimeAMString),
    Canvas.TextWidth('00:00' + dxFormatSettings.TimePMString));
end;

procedure TcxSchedulerEventCellViewInfo.CalculateEventTimeAsTextLayout(
  const ABounds: TRect; const ACaptionWidth, AImagesWidth: Integer; var ALeft: Integer);
var
  ASpaceWidth, AWidth: Integer;
  R: TRect;
begin
  ASpaceWidth := cxRectWidth(ABounds) - AImagesWidth;
  with Canvas.TextExtent('00:00') do
    R := cxRectBounds(ABounds.Left, 0, CX, CY);
  if not Is24HourTimeFormat then
    R := cxRectSetWidth(R, GetTimeTextMaxWidth);
  AWidth := cxRectWidth(R);
  if ASpaceWidth >= AWidth then
  begin
    FStartRect := cxRect(ALeft, FCaptionRect.Top, ALeft + AWidth, FCaptionRect.Bottom);
    Inc(AWidth, Canvas.TextWidth('0') div 2);
    Inc(ALeft, AWidth);
    Dec(ASpaceWidth, AWidth);
    if ShowFinishTime and (ASpaceWidth >= AWidth) then
    begin
      FFinishRect := cxRectOffset(FStartRect, AWidth, 0);
      Inc(ALeft, AWidth);
    end;
  end;
end;

function TcxSchedulerEventCellViewInfo.CalculateNonDetailEventImages(
  const ACaptionWidth: Integer; out AImagesWidth: Integer): TRect;
begin
  Result := GetImagesBounds;
  AImagesWidth := CalculateSingleLineImages(Result);
end;

function TcxSchedulerEventCellViewInfo.CalculateSingleLineImages(
  const ABounds: TRect; ARightLimit: Integer = 0): Integer;
begin
  Result := Images.CalculateSingleLineImages(ABounds, ARightLimit);
end;

procedure TcxSchedulerEventCellViewInfo.CalculateTimeLineLayout;
var
  ATop, AHeight, AWidth: Integer;
begin
  if not IsTimeLineVisible then
    Exit;
  AWidth := ScaleFactor.Apply(cxTimeLineWidth);
  ATop := GetTimeLineRectTop;
  AHeight := GetTimeLineHeight;
  FTimeLineRect := cxRectSetRight(GetTimeLineRectBaseBounds, GetTimeLineRectRight, AWidth);
  FTimeLineRect.Top := ATop;
  FTimeLineRect.Bottom := ATop + AHeight;
  Dec(AHeight, 2);
  FEventTimeRect := cxRectBounds(FTimeLineRect.Left, ATop + 1, AWidth, AHeight);
end;

function TcxSchedulerEventCellViewInfo.CanBeHeaderEventContinueArrows: Boolean;
begin
  Result := False;
end;

function TcxSchedulerEventCellViewInfo.CheckAutoLayoutImagesSingleLineCaption(
  const ACaptionWidth, AAvailableWidth: Integer): Boolean;
begin
  Result := True;
end;

procedure TcxSchedulerEventCellViewInfo.CheckItemsAfterAutoLayoutImagesCalculation(
  AFactuallyImagesRowCount, AImagesWidth: Integer);
begin
  Inc(FCaptionRect.Left,  AImagesWidth);
end;

procedure TcxSchedulerEventCellViewInfo.CheckItemsBeforeAutoLayoutImagesCalculation;
begin
//
end;

function TcxSchedulerEventCellViewInfo.CanShowHeaderEventClock(const ARect: TRect): Boolean;
begin
  Result := (cxRectWidth(ARect) >= EventImages.Width + ScaleFactor.Apply(cxEventImagesGap)) and ViewData.ShowTimeAsClock;
end;

function TcxSchedulerEventCellViewInfo.CanShowHeaderEventTimeText(const ARect: TRect): Boolean;
var
  AWidth, L, AOffset: Integer;
begin
  Result := ShowStartTime or ShowFinishTime;
  if Result then
  begin
    AWidth := 0;
    L := GetTimeTextMaxWidth;
    AOffset := ScaleFactor.Apply(cxTextOffset);
    if ShowStartTime then
      AWidth := L + AOffset * 2;
    if ShowFinishTime then
      Inc(AWidth, L + AOffset * 2);
    if Length(Caption) > 0 then
      Inc(AWidth, 2 * cxTextWidth(Canvas.Font, Caption[1]));
    Result := cxRectWidth(ARect) > AWidth;
  end;
end;

procedure TcxSchedulerEventCellViewInfo.DoRightToLeftConversion(const AClientBounds: TRect);
var
  ALeft: Integer;
begin
  inherited DoRightToLeftConversion(AClientBounds);
  DoRightToLeftConversionWithChecking(FCaptionRect, AClientBounds);
  DoRightToLeftConversionWithChecking(FLocationRect, AClientBounds);
  ALeft := MessageRect.Left;
  DoRightToLeftConversionWithChecking(FMessageRect, AClientBounds);
  if (FMessageRect.Left - ALeft <> 0) and (FEditViewInfo <> nil) then
    FEditViewInfo.Left := FEditViewInfo.Left + FMessageRect.Left - ALeft;
  DoRightToLeftConversionWithChecking(FSelectionBorderRect, AClientBounds);
  DoRightToLeftConversionWithChecking(FSizingHandlerRect1, AClientBounds);
  DoRightToLeftConversionWithChecking(FSizingHandlerRect2, AClientBounds);
  DoRightToLeftConversionWithChecking(FEndContinueArrowCaptionRect, AClientBounds);
  DoRightToLeftConversionWithChecking(FEndContinueArrowRect, AClientBounds);
  DoRightToLeftConversionWithChecking(FFinishRect, AClientBounds);
  DoRightToLeftConversionWithChecking(FStartContinueArrowCaptionRect, AClientBounds);
  DoRightToLeftConversionWithChecking(FStartContinueArrowRect, AClientBounds);
  DoRightToLeftConversionWithChecking(FStartRect, AClientBounds);
  DoRightToLeftConversionWithChecking(FTimeLineRect, AClientBounds);
  DoRightToLeftConversionWithChecking(FEventTimeRect, AClientBounds);
  DoRightToLeftConversionWithChecking(ViewData.VisibleRect, AClientBounds);
  Images.DoRightToLeftConversion(AClientBounds);
end;

function TcxSchedulerEventCellViewInfo.EnableModernSelection: Boolean;
begin
  Result := False;
end;

function TcxSchedulerEventCellViewInfo.GetActiveBounds: TRect;
begin
  Result := inherited GetActiveBounds;
  if not cxRectIsNull(TimeLineRect) then
    if UseRightToLeftAlignment then
      Result.Right := TimeLineRect.Right
    else
      Result.Left := TimeLineRect.Left;
end;

function TcxSchedulerEventCellViewInfo.GetBoundsWithoutTimeLineRect: TRect;
begin
  Result := Bounds;
end;

function TcxSchedulerEventCellViewInfo.GetContinueArrowCaptionDateFormat: string;
begin
  Result := 'd mmm';
end;

function TcxSchedulerEventCellViewInfo.GetAvailableBounds: TRect;
begin
  Result := Bounds;
  Result.Left := Max(Result.Left, ViewData.VisibleRect.Left);
end;

function TcxSchedulerEventCellViewInfo.GetHeaderEventPossibleCaptionRect: TRect;
var
  AOffset: Integer;
  ABounds: TRect;
begin
  ABounds := GetBoundsWithoutTimeLineRect;
  if cxRectIsEmpty(FStartContinueArrowRect) and cxRectIsEmpty(FEndContinueArrowRect) then
    Result := cxRectInflate(ABounds, -ScaleFactor.Apply(cxTextOffset), -ScaleFactor.Apply(1))
  else
  begin
    Result := cxRectInflate(ABounds, 0, -ScaleFactor.Apply(1));
    AOffset := ScaleFactor.Apply(15);

    if not cxRectIsEmpty(FStartContinueArrowCaptionRect) then
      Result.Left := FStartContinueArrowCaptionRect.Right + AOffset
    else
      if not cxRectIsEmpty(FStartContinueArrowRect) then
        Result.Left := FStartContinueArrowRect.Right + AOffset
      else
        Result.Left := ABounds.Left + AOffset;

    if not cxRectIsEmpty(FEndContinueArrowCaptionRect) then
      Result.Right := FEndContinueArrowCaptionRect.Left - AOffset
    else
      if not cxRectIsEmpty(FEndContinueArrowRect) then
        Result.Right := FEndContinueArrowRect.Left - AOffset
      else
        Result.Right := ABounds.Right - AOffset;
  end;
end;

function TcxSchedulerEventCellViewInfo.GetHorizontalImagesMessageAutoHeight(AMessageTop: Integer): Integer;
var
  AMessageHeight: Integer;
begin
  Result := 0;
  if FShowMessage then
  begin
    FMessageRect := cxRect(GetMessageRectLeft, AMessageTop - ScaleFactor.Apply(1), Bounds.Right - ScaleFactor.Apply(1), 0);
    AMessageHeight := CalculateMessageHeight(FMessageRect);
    FMessageRect.Bottom := FMessageRect.Top + AMessageHeight;
    Inc(Result, AMessageHeight + ScaleFactor.Apply(1));
    CalculateEditViewInfo;
  end;
end;

function TcxSchedulerEventCellViewInfo.GetHeaderEventCaptionLeft(
  const ARect: TRect; AContentWidth: Integer): Integer;
begin
  with ARect do
    Result := Max(Left + ((Right - Left - AContentWidth) div 2), ARect.Left);
end;

function TcxSchedulerEventCellViewInfo.GetHeaderImagesPossibleRect(const ACaptionRect: TRect; ATextWidth: Integer): TRect;
begin
  Result := cxRectSetLeft(cxRectInflate(ACaptionRect, -ScaleFactor.Apply(1), -ScaleFactor.Apply(1)), 0,
    cxRectWidth(ACaptionRect) - (ATextWidth + ScaleFactor.Apply(cxTextOffset)));
end;

function TcxSchedulerEventCellViewInfo.GetHeaderEventStartClockOriginLeft: Integer;
begin
  Result := Bounds.Left;
end;

function TcxSchedulerEventCellViewInfo.GetHeaderEventFinishClockOriginRight: Integer;
begin
  Result := Bounds.Right;
end;

function TcxSchedulerEventCellViewInfo.GetLabelBasedBorderColor: TColor;
var
  ALabelColor: TColor;
begin
  ALabelColor := Event.LabelColor;
  if ALabelColor = clDefault then
    ALabelColor := Painter.DefaultSchedulerEventColor(False);
  Result := PainterHelper.GetTimeLineBorderColor(ALabelColor);
end;

function TcxSchedulerEventCellViewInfo.GetNonDetailEventPossibleCaptionRect: TRect;
begin
  Result := cxRectInflate(ClipRect, -ScaleFactor.Apply(cxTextOffset), -ScaleFactor.Apply(1));
end;

function TcxSchedulerEventCellViewInfo.GetMessageRectLeft: Integer;
begin
  Result := Bounds.Left + ScaleFactor.Apply(cxEventImagesGap);
end;

function TcxSchedulerEventCellViewInfo.GetOffsetSelectionBorderFromTimeLineRect: Integer;
begin
  Result := 1;
end;

function TcxSchedulerEventCellViewInfo.GetSelectedBorderColor: TColor;
begin
  Result := ExternalPainter.GetEventSelectionBorderColor(Self);
end;

function TcxSchedulerEventCellViewInfo.GetSelectionBoundsExtends: TRect;
begin
  Result := cxNullRect;
  if Selected then
    Result := ExternalPainter.GetEventSelectionExtends(Self);
end;

function TcxSchedulerEventCellViewInfo.GetTimeLineHeight: Integer;
begin
  Result := cxRectHeight(FBounds);
  if not ActualBorderColorIsNone then
    Dec(Result, 2 * BorderSize);
end;

function TcxSchedulerEventCellViewInfo.GetTimeLineRectBaseBounds: TRect;
begin
  Result := Bounds;
end;

function TcxSchedulerEventCellViewInfo.GetTimeLineRectRight: Integer;
begin
  Result := FBounds.Left + BorderSize;
end;

function TcxSchedulerEventCellViewInfo.GetTimeLineRectTop: Integer;
begin
  Result := FBounds.Top;
  if not ActualBorderColorIsNone then
    Inc(Result, BorderSize);
end;

procedure TcxSchedulerEventCellViewInfo.InitializeHeaderEventContinueArrowsData;
begin
  FStartContinueArrowRect := cxNullRect;
  FEndContinueArrowRect := cxNullRect;
  FStartContinueArrowCaption := '';
  FEndContinueArrowCaption := '';
  FStartContinueArrowCaptionRect := cxNullRect;
  FEndContinueArrowCaptionRect := cxNullRect;
end;

function TcxSchedulerEventCellViewInfo.NeedBorderAroundOfTimeLineRect: Boolean;
begin
  Result := False;
end;

function TcxSchedulerEventCellViewInfo.NeedCorrectCaptionBottomWhenMessageAutoLayoutCalculating: Boolean;
begin
  Result := True;
end;

function TcxSchedulerEventCellViewInfo.NeedHeaderEventStartContinueArrow: Boolean;
begin
  Result := False;
end;

function TcxSchedulerEventCellViewInfo.NeedHeaderEventFinishContinueArrow: Boolean;
begin
  Result := False;
end;

function TcxSchedulerEventCellViewInfo.SetItemRect(AShowItem: Boolean;
  const ABounds: TRect; AVertOffset: Integer; var APos: Integer): TRect;
begin
  Result := cxNullRect;
  if AShowItem then
  begin
    if ((APos + cxRectWidth(ABounds)) <= cxRectCenter(FBounds).X) then
    begin
      Result := cxRectOffset(ABounds, APos, AVertOffset);
      APos := Result.Right + ScaleFactor.Apply(cxTextOffset);
    end;
  end
end;

procedure TcxSchedulerEventCellViewInfo.CalculateEventTimeVisibility;
begin
  if IsDetailInfo then
    CalculateDetailEventTimeVisibility
  else
    CalculateNonDetailEventTimeVisibility;
end;

procedure TcxSchedulerEventCellViewInfo.CalculateDetailEventTimeVisibility;
begin
  ViewData.ShowFinishTime := (IsDetailCaption and ViewData.AlwaysShowTime) or
    ((ViewData.ContentFinish > EventFinish) and (ViewData.ContentStart <> EventFinish)) or
    (ViewData.ContentStart < EventStart);
  ViewData.ShowStartTime := ViewData.ShowFinishTime;
end;

procedure TcxSchedulerEventCellViewInfo.CalculateItemsLayoutForMeasureHeight;
begin
  FCaptionRect := cxRectInflate(GetAvailableBounds, -ScaleFactor.Apply(cxTextOffset + 1), -FDetailCaptionVertOffset);
end;

procedure TcxSchedulerEventCellViewInfo.CalculateLocationRect;
begin
  FLocationRect := cxNullRect;
end;

procedure TcxSchedulerEventCellViewInfo.CalculateNonDetailEventTimeVisibility;
begin
  if Event.AllDayEvent then
  begin
    ViewData.ShowFinishTime := False;
    ViewData.ShowStartTime := False;
  end
  else
  begin
    ViewData.ShowFinishTime := (ViewData.ContentFinish >= EventFinish) and
      (ViewData.ShowFinishTime or (Event.Duration >= 1));
    ViewData.ShowStartTime := (ContentStart <= EventStart);
  end;
end;

procedure TcxSchedulerEventCellViewInfo.CalculateVisibility;
begin
  if not ViewData.ShowTimeLine then
    FVisible := FVisible and (FClipRect.Bottom >= FBounds.Bottom);
  CalculateDetailInfo;
  CalculateShowTimeAsClock;
  CalculateEventTimeVisibility;
  CalculateBorders;
end;

function TcxSchedulerEventCellViewInfo.CanAutoHideStandardImages: Boolean;
begin
  Result := False;
end;

function TcxSchedulerEventCellViewInfo.CanShowHint: Boolean;
begin
  Result := (ViewStyle = svsModern) or
    (not Event.AllDayEvent and not IsDetailInfo and (cxRectIsEmpty(FinishRect) or cxRectIsEmpty(StartRect)));
end;

procedure TcxSchedulerEventCellViewInfo.DoDraw;
begin
  if FHidden then Exit;
  BeforeDraw;
  ClippingCreate(True);
  DrawContent;
  ClippingRestore;
  AfterDraw;
end;

procedure TcxSchedulerEventCellViewInfo.DoDrawCaption;
begin
  Canvas.DrawText(Caption, FCaptionRect, GetDrawCaptionFlags);
end;

procedure TcxSchedulerEventCellViewInfo.DoDrawEndContinueArrow;
begin
  if not UseRightToLeftAlignment then
    TcxSchedulerPainterHelper.DrawEndContinueArrow(Canvas, FEndContinueArrowRect, Bounds, ScaleFactor)
  else
    TcxSchedulerPainterHelper.DrawStartContinueArrow(Canvas, FEndContinueArrowRect, Bounds, ScaleFactor);
end;

procedure TcxSchedulerEventCellViewInfo.DoDrawStartContinueArrow;
begin
  if not UseRightToLeftAlignment then
    TcxSchedulerPainterHelper.DrawStartContinueArrow(Canvas, FStartContinueArrowRect, Bounds, ScaleFactor)
  else
    TcxSchedulerPainterHelper.DrawEndContinueArrow(Canvas, FStartContinueArrowRect, Bounds, ScaleFactor);
end;

procedure TcxSchedulerEventCellViewInfo.DrawEventBorderAroundOfTimeLineRect;
const
  ABorders: array[Boolean, Boolean] of TcxBorders =(([bTop, bBottom], [bLeft, bTop, bBottom]), ([bTop, bBottom], [bTop, bRight, bBottom]));
  ARightBordersRTL: array[Boolean] of TcxBorders =([bRight], [bLeft]);
var
  ABounds: TRect;
begin
  if ActualBorderColorIsNone then
    Exit;
  ABounds := FTimeLineRect;
  if UseRightToLeftAlignment then
    Inc(ABounds.Right)
  else
    Dec(ABounds.Left, BorderSize);
  Dec(ABounds.Top, BorderSize);
  Inc(ABounds.Bottom, BorderSize);
  DrawFrame(ABounds, ABorders[UseRightToLeftAlignment, HasLeftBorder], BorderColor, BorderSize);
  if ABounds.Bottom < Bounds.Bottom then
  begin
    ABounds.Top := ABounds.Bottom - BorderSize;
    ABounds.Bottom := Bounds.Bottom;
    if UseRightToLeftAlignment then
      ABounds.Right := ABounds.Left + BorderSize
    else
      ABounds.Left := ABounds.Right - BorderSize;
    DrawFrame(ABounds, ARightBordersRTL[UseRightToLeftAlignment], BorderColor, BorderSize);
  end;
end;

procedure TcxSchedulerEventCellViewInfo.DrawCaption;
var
  ASavedStyle: TFontStyles;
begin
  if cxRectIsEmpty(FCaptionRect) or ((Caption = '') and (Event.Location = '')) then Exit;
  Canvas.Brush.Style := bsClear;
  ASavedStyle := Canvas.Font.Style;
  Canvas.SaveClipRegion;
  try
    Canvas.Font.Style := GetCaptionFontStyle;
    Canvas.IntersectClipRect(FCaptionRect);
    DoDrawCaption;
  finally
    Canvas.RestoreClipRegion ;
  end;
  Canvas.Font.Style := ASavedStyle;
  Canvas.Brush.Style := bsSolid;
end;

procedure TcxSchedulerEventCellViewInfo.DrawContent;
begin
  if ViewData.IsMilestone then
    ExternalPainter.DrawEventAsMilestone(Self)
  else
    if ViewData.IsGroup then
      ExternalPainter.DrawEventAsGroup(Self)
    else
      if ViewData.DrawAsProgress then
        ExternalPainter.DrawEventAsProgress(Self)
      else
      begin
        ExternalPainter.DrawEvent(Self);
        if not Event.IsEditing then
          DrawImages;
        if IsNeedDrawTime then
          DrawTime;
        DrawContinueArrowsAndCaptions;
        DrawCaption;
        DrawLocation;
        if FShowMessage then
          DrawMessage;
        if IsTimeLineVisible then
          DrawState;
        if Selected then
          DrawSelection;
      end;
end;

procedure TcxSchedulerEventCellViewInfo.DrawContinueArrowsAndCaptions;
begin
  if not cxRectIsEmpty(FStartContinueArrowRect) then
    DoDrawStartContinueArrow;
  if FStartContinueArrowCaption <> '' then
    cxTextOut(Canvas.Canvas, FStartContinueArrowCaption, FStartContinueArrowCaptionRect, GetCaptionFlags,
      0, 0, Canvas.Font, clNone, clNone, 0, 0, 0, ViewParams.TextColor);
  if not cxRectIsEmpty(FEndContinueArrowRect) then
    DoDrawEndContinueArrow;
  if FEndContinueArrowCaption <> '' then
    cxTextOut(Canvas.Canvas, FEndContinueArrowCaption, FEndContinueArrowCaptionRect, GetCaptionFlags,
      0, 0, Canvas.Font, clNone, clNone, 0, 0, 0, ViewParams.TextColor);
end;

procedure TcxSchedulerEventCellViewInfo.DrawImages;
var
  I: Integer;
  ASaveRgn: TcxRegion;
begin
  if Images.Count > 0 then
  begin
    ASaveRgn := Canvas.GetClipRegion;
    Canvas.IntersectClipRect(cxRectInflate(Bounds, -1, -1));
    for I := 0 to Images.Count - 1 do
      if Images.Items[I].Visible and not FHidden then
        PainterHelper.DrawTransparentImage(Canvas, Images.Items[I].Images, Images.Items[I].Bounds,
          Images.Items[I].ImageIndex, ScaleFactor);
    Canvas.SetClipRegion(ASaveRgn, roSet);
  end;
end;

procedure TcxSchedulerEventCellViewInfo.DrawLocation;
begin
  if (FLocation <> '') and (cxRectWidth(LocationRect) > 0) then
    cxTextOut(Canvas.Canvas, FLocation, FLocationRect, GetCaptionFlags,
      0, 0, Canvas.Font, clNone, clNone, 0, 0, 0, ViewParams.TextColor);
end;

procedure TcxSchedulerEventCellViewInfo.DrawMessage;
begin
  FEditViewInfo.Transparent := True;
  FEditViewInfo.TextColor := TextColor;
  FEditViewInfo.PaintEx(Canvas);
  DrawMessageSeparator;
end;

procedure TcxSchedulerEventCellViewInfo.DrawMessageSeparator;
var
  R: TRect;
begin
  if not UseRightToLeftAlignment then
    R := cxRectInflate(MessageRect, -GetMessageSeparatorLeftIndent, 1, -2, 0)
  else
    R := cxRectInflate(MessageRect, -2, 1, -GetMessageSeparatorLeftIndent, 0);
  Canvas.FrameRect(R, SeparatorColor, 1, [bTop]);
end;

procedure TcxSchedulerEventCellViewInfo.DrawTime;
begin
  if ShowTimeAsClock then
  begin
    if ShowStartTime then
      PainterHelper.DrawClock(Canvas, FStartRect, Event.Start, FViewParams, ScaleFactor);
    if ShowFinishTime then
      PainterHelper.DrawClock(Canvas, FFinishRect, Event.Finish, FViewParams, ScaleFactor);
  end
  else
  begin
    DrawText(FStartRect, FStartText, GetStartTextJustify);
    DrawText(FFinishRect, FFinishText, GetFinishTextJustify);
  end;
end;

function TcxSchedulerEventCellViewInfo.NeedBottomSizingHandler: Boolean;
begin
  Result := EventFinish <= ContentFinish;
end;

function TcxSchedulerEventCellViewInfo.NeedLeftSizingHandler: Boolean;
begin
  Result := HasLeftBorder and (EventStart >= Int(ContentStart));
end;

function TcxSchedulerEventCellViewInfo.NeedRightSizingHandler: Boolean;
var
  AEventFinish, AContentFinish: TDate;
begin
  AEventFinish := Int(EventFinish);
  AContentFinish := Int(ContentFinish);
  Result := HasRightBorder and ((AEventFinish = AContentFinish) or (AEventFinish + 1 = AContentFinish)) ;
end;

function TcxSchedulerEventCellViewInfo.NeedTopSizingHandler: Boolean;
begin
  Result := EventStart >= ContentStart;
end;

procedure TcxSchedulerEventCellViewInfo.TuneClipping(var AClipRgn: TcxRegion);
begin
  if not cxRectIsNull(FSizingHandlerRect1) then
    AClipRgn.Combine(FSizingHandlerRect1, roSubtract);
  if not cxRectIsNull(FSizingHandlerRect2) then
    AClipRgn.Combine(FSizingHandlerRect2, roSubtract);
end;

function TcxSchedulerEventCellViewInfo.GetCaptionAutoHeight(const R: TRect): Integer;
var
  AStyle: TFontStyles;
begin
  AStyle := Canvas.Font.Style;
  if FCaptionIsBold then
    Canvas.Font.Style := Canvas.Font.Style + [fsBold];
  if (Caption <> '') and not cxRectIsEmpty(CaptionRect) then
    Result := GetTrueMultilineTextHeight(Canvas, Caption, R, CXTO_CALCROWCOUNT or GetCaptionFlags)
  else
    Result := Canvas.FontHeight(Font);
  Canvas.Font.Style := AStyle;
end;

function TcxSchedulerEventCellViewInfo.GetCaptionFlags: Cardinal;
begin
  Result := CXTO_LEFT or CXTO_TOP or CXTO_WORDBREAK or CXTO_EDITCONTROL;
end;

function TcxSchedulerEventCellViewInfo.GetCaptionFontStyle: TFontStyles;
begin
  Result := Canvas.Font.Style;
end;

function TcxSchedulerEventCellViewInfo.GetDrawCaptionFlags: Cardinal;
const
  AFlags: array[Boolean, Boolean] of Cardinal = (
    (cxAlignLeft or cxAlignVCenter or cxSingleLine, cxAlignLeft or cxAlignTop or cxWordBreak or cxDontBreakChars or cxNoFullWidthCharBreak),
    (cxAlignRight or cxAlignVCenter or cxSingleLine, cxAlignRight or cxAlignTop or cxWordBreak or cxDontBreakChars or cxNoFullWidthCharBreak)
    );
begin
  Result := AFlags[UseRightToLeftAlignment, IsDetailCaption];
end;

function TcxSchedulerEventCellViewInfo.GetEditingRect: TRect;
begin
  cxRectIntersect(Result, PainterHelper.ExcludeBorders(Bounds, Borders), FClipRect);
  with Result do
  begin
    if IsDetailCaption then
    begin
      Inc(Left, ScaleFactor.Apply(cxTextOffset) - Byte(HasLeftBorder));
      Dec(Right);
    end
    else
      Inc(Left);
  end;
end;

function TcxSchedulerEventCellViewInfo.GetEditStyleColor: TColor;
begin
  Result := Color;
end;

function TcxSchedulerEventCellViewInfo.GetForceShowClockInHeaderEvent: Boolean;
begin
  Result := True;
end;

function TcxSchedulerEventCellViewInfo.GetHitTest(
  AHitTest: TcxSchedulerCustomResourceViewHitTest; var ABreak: Boolean): Boolean;
begin
  Result := inherited GetHitTest(AHitTest, ABreak);
  Result := Result and not Hidden and Event.Enabled;
  if Result and not Hidden and ShowTimeLine and Selected then
    ABreak := not PtInRect(CaptionRect, AHitTest.HitPoint) and not
      PtInRect(MessageRect, AHitTest.HitPoint)
  else
    ABreak := False;
end;

function TcxSchedulerEventCellViewInfo.GetImagesBounds: TRect;
begin
  Result := cxRect(Bounds.Left + ScaleFactor.Apply(cxEventImagesOffset) + cxEventBorderWidth - Byte(ViewData.ShowTimeLine),
    Bounds.Top + cxEventBorderWidth, (Bounds.Left + Bounds.Right - ScaleFactor.Apply(cxTextOffset)) div 2,
    Bounds.Bottom - (ScaleFactor.Apply(cxEventImagesOffset) + cxEventBorderWidth));
end;

function TcxSchedulerEventCellViewInfo.GetImagesHorizontalOffset: Integer;
begin
  Result := 0;
end;

function TcxSchedulerEventCellViewInfo.GetImagesVerticalOffset(AImageHeight: Integer; AIsAbsolute: Boolean): Integer;
begin
  Result := ScaleFactor.Apply(cxEventImagesOffset);
  if not FIsDetailCaption then
    Result := Max(Result, (cxRectHeight(Bounds) - 2 * cxEventBorderWidth - AImageHeight) div 2);
  if AIsAbsolute then
    Inc(Result, Bounds.Top + cxEventBorderWidth);
end;

function TcxSchedulerEventCellViewInfo.GetLocationAutoHeight(const R: TRect): Integer;
begin
  Result := 0;
  if (FLocation <> '') and (R.Right - R.Left > 0) then
    Result := GetTrueMultilineTextHeight(Canvas, FLocation, R, CXTO_CALCROWCOUNT or GetCaptionFlags)
end;

function TcxSchedulerEventCellViewInfo.GetMessageRect(const ACaptionRect: TRect;
  AHasImages: Boolean): TRect;
var
  ACaptionBottom: Integer;
  R: TRect;
begin
  if not cxRectIsEmpty(ACaptionRect) then
  begin
    R := GetCaptionBounds(ACaptionRect);
    ACaptionBottom := R.Bottom + ScaleFactor.Apply(cxTextOffset); //bottom text offset
  end
  else
    if Caption = '' then
      ACaptionBottom := Bounds.Top + ScaleFactor.Apply(cxTextOffset)
    else
    begin
      Result := cxNullRect;
      Exit;
    end;
  if AHasImages then
    ACaptionBottom := Max(ACaptionBottom, Bounds.Top + Images.ItemHeight + GetMessageRectOffset);
  Inc(ACaptionBottom); //horz line
  Result.Top := ACaptionBottom;
  Result.Bottom := Bounds.Bottom;
  Result.Right := Bounds.Right - ScaleFactor.Apply(1);
  if Images.Layout = eilHorizontal then
    Result.Left := Bounds.Left + cxEventBorderWidth
  else
    Result.Left := R.Left - ScaleFactor.Apply(cxTextOffset);
end;

function TcxSchedulerEventCellViewInfo.GetMessageRectOffset: Integer;
begin
  Result := 0;
end;

function TcxSchedulerEventCellViewInfo.GetMessageSeparatorLeftIndent: Integer;
begin
  Result := 1 + Byte(not ShowTimeLine);
end;

function TcxSchedulerEventCellViewInfo.GetViewStyle: TcxSchedulerViewStyle;
begin
  Result := svsClassic;
end;

function TcxSchedulerEventCellViewInfo.GetStartTextJustify: Cardinal;
begin
  Result := cxAlignRight or cxAlignVCenter or cxSingleLine;
end;

function TcxSchedulerEventCellViewInfo.GetFinishTextJustify: Cardinal;
begin
  Result := cxAlignRight or cxAlignVCenter or cxSingleLine;
end;

function TcxSchedulerEventCellViewInfo.GetTimeLineBorderColor: TColor;
begin
  if Event.State = 3 then
    Result := cxSchedulerOutOfOfficeStateColor
  else
    Result := GetLabelBasedBorderColor;
end;

function TcxSchedulerEventCellViewInfo.HasLeftBorder: Boolean;
const
  ALeftBorderRTL: array[Boolean] of TcxBorder = (bLeft, bRight);
begin
  Result := ALeftBorderRTL[UseRightToLeftAlignment and (FCalculationCounter = 0)] in Borders;
end;

function TcxSchedulerEventCellViewInfo.HasRightBorder: Boolean;
const
  ARightBorderRTL: array[Boolean] of TcxBorder = (bRight, bLeft);
begin
  Result := ARightBorderRTL[UseRightToLeftAlignment and (FCalculationCounter = 0)] in Borders;
end;

function TcxSchedulerEventCellViewInfo.HasReminder: Boolean;
begin
  with Event do
  begin
    Result := Storage.Reminders.Active and Storage.IsReminderAvailable and Reminder;
    if Result and (ResourceItem <> nil) and Storage.IsReminderByResourceAvailable then
      Result := HasReminderForResourceID(ResourceItem.ResourceID);
  end;
end;

procedure TcxSchedulerEventCellViewInfo.Initialize;
begin
  FBorders := cxBordersAll;
  FEventStart := TruncTime(Event.Start);
  FEventFinish := TruncTime(Event.Finish);
  FIsHeaderEvent := GetIsHeaderEvent;
  FStartRect := cxNullRect;
  FFinishRect := cxNullRect;
  FTimeLineRect := cxNullRect;
  FLocationRect := cxNullRect;
  FSelectionBorderRect := cxNullRect;
  FSizingHandlerRect1 := cxNullRect;
  FSizingHandlerRect2 := cxNullRect;
  ViewData.ShowTimeLine := IsTimeLineVisible;
  InitializeHeaderEventContinueArrowsData;
  InitStandardEventImages;
  FActualBorderColorIsNone := (ViewData.BorderColor = clNone) and not IsDrawBySkin;
  CalculateVisibility;
  CalculateCaptions;
end;

function TcxSchedulerEventCellViewInfo.HintTriggerArea: TRect;
begin
  Result := FClipRect;
end;

procedure TcxSchedulerEventCellViewInfo.InitHitTest(
  AHitTest: TcxSchedulerCustomResourceViewHitTest);
begin
  AHitTest.FEventCell := Self;
  AHitTest.FEventBounds := FClipRect;
  AHitTest.FDragKind := edkMoveEvent;
  if PtInRect(HintTriggerArea, AHitTest.HitPoint) then
  begin
    CalculateNeedHint;
    AHitTest.FNeedShowHint := FHintNeeded;
  end
  else
  begin
    AHitTest.FNeedShowHint := False;
    TcxCustomSchedulerAccess(AHitTest.Scheduler).EventHitTestController.HideEventHint;
  end;
end;

procedure TcxSchedulerEventCellViewInfo.InitHitTestForHorzEvent(
  AHitTest: TcxSchedulerCustomResourceViewHitTest);
var
  AHitAtLeft, AHitAtRight: Boolean;
begin
  AHitAtLeft := AHitTest.HitX - Bounds.Left <= cxHitDelta;
  AHitAtRight := Bounds.Right - AHitTest.HitX <= cxHitDelta;
  if CanMoveLeftBorder and ((not UseRightToLeftAlignment and AHitAtLeft) or (UseRightToLeftAlignment and AHitAtRight)) then
    AHitTest.FDragKind := edkResizeStart
  else
    if CanMoveRightBorder and ((not UseRightToLeftAlignment and AHitAtRight) or (UseRightToLeftAlignment and AHitAtLeft)) then
      AHitTest.FDragKind := edkResizeEnd;
end;

procedure TcxSchedulerEventCellViewInfo.InitStandardEventImages;
var
  AutoHide: Boolean;
begin
  AutoHide := CanAutoHideStandardImages;
  with Event do
  begin
    if HasReminder then
      Images.AddStandardImage(eitReminder, AutoHide);
    case EventType of
      etOccurrence: Images.AddStandardImage(eitRecurrence, AutoHide);
      etCustom: Images.AddStandardImage(eitCustomOccurrence, AutoHide);
    end;
  end;
end;

function TcxSchedulerEventCellViewInfo.IsHorzSizing: Boolean;
begin
  Result := True;
end;

function TcxSchedulerEventCellViewInfo.IsNeedDrawTime: Boolean;
begin
  Result := ViewData.ShowFinishTime or ViewData.ShowStartTime;
end;

function TcxSchedulerEventCellViewInfo.IsTimeLineVisible: Boolean;
begin
  Result := False;
end;

procedure TcxSchedulerEventCellViewInfo.MeasureCaptionExtent(var R: TRect);
begin
  Canvas.TextExtent(Hint, R, cxDrawTextFlags[IsDetailCaption]);
end;

function TcxSchedulerEventCellViewInfo.MeasureAutoLayoutImagesLocationHeight(const R: TRect): Integer;
begin
  Result := 0;
end;

procedure TcxSchedulerEventCellViewInfo.ShiftRect(var R: TRect; const X, Y: Integer);
begin
  if not cxRectIsEmpty(R) then OffsetRect(R, X, Y);
end;

function TcxSchedulerEventCellViewInfo.TruncTime(const ATime: TDateTime): TDateTime;
begin
  Result := DateTimeHelper.RoundTime(ATime);
end;

function TcxSchedulerEventCellViewInfo.GetContentFinish: TDateTime;
begin
  Result := ViewData.ContentFinish;
end;

function TcxSchedulerEventCellViewInfo.CalculateMessageHeight(const R: TRect): Integer;

  function CreateEditViewData: TcxCustomEditViewData;
  begin
    Result := ViewData.EditProperties.CreateViewData(ViewData.EditStyle, True);
    Result.ScaleFactor.Assign(ScaleFactor);
  end;

var
  AEditViewData: TcxCustomEditViewData;
  ASize: TcxEditSizeProperties;
begin
  AEditViewData := CreateEditViewData;
  try
    AssignEditStyle(ViewData.EditStyle);
    AEditViewData.ContentOffset := cxSimpleRect;
    ASize.MaxLineCount := 0;
    ASize.Height := -ScaleFactor.Apply(1);
    ASize.Width := cxRectWidth(R);
    Result := AEditViewData.GetEditSize(Canvas, Message, ASize).cy;
  finally
    AEditViewData.Free;
  end;
end;

function TcxSchedulerEventCellViewInfo.GetBorderColor: TColor;
begin
  Result := ViewData.BorderColor;
end;

function TcxSchedulerEventCellViewInfo.GetCaption: string;
begin
  Result := ViewData.Caption;
end;

function TcxSchedulerEventCellViewInfo.GetCaptionBounds(const R: TRect): TRect;
begin
  Result := R;
  if LocationRect.Top > CaptionRect.Top then
    Canvas.TextExtent(FLocation, Result, cxDrawTextFlags[IsDetailCaption])
  else
    if Length(ViewData.Caption) > 0 then
      Canvas.TextExtent(ViewData.Caption, Result, cxDrawTextFlags[IsDetailCaption])
    else
      Canvas.TextExtent('Wg', Result, cxDrawTextFlags[IsDetailCaption]);
end;

function TcxSchedulerEventCellViewInfo.GetContentStart: TDateTime;
begin
  Result := ViewData.ContentStart;
end;

function TcxSchedulerEventCellViewInfo.GetDetailCaptionFlagValue: Boolean;
begin
  Result := FIsDetailInfo and not IsHeaderEvent;
end;

function TcxSchedulerEventCellViewInfo.GetDetailInfoFlagValue: Boolean;
begin
  Result := ViewData.AutoHeight or (not IsHeaderEvent and
    ((dxTimeOf(ContentFinish) <> 0) or (dxTimeOf(ContentStart) <> 0)));
end;

function TcxSchedulerEventCellViewInfo.GetEvent: TcxSchedulerControlEvent;
begin
  Result := ViewData.Event;
end;

function TcxSchedulerEventCellViewInfo.GetHint: string;
begin
  if not FHintAssigned then
    FHint := ViewData.GetEventHint(Event);
  FHintAssigned := True;
  Result := FHint;
end;

function TcxSchedulerEventCellViewInfo.GetIsHeaderEvent: Boolean;
begin
  Result := Event.AllDayEvent or ((Event.Finish - Event.Start) >= 1);
end;

function TcxSchedulerEventCellViewInfo.GetMessage: string;
begin
  Result := Event.Message;
end;

function TcxSchedulerEventCellViewInfo.GetResourceInfo: TcxSchedulerResourceViewInfo;
begin
  Result := ViewData.Resource;
end;

function TcxSchedulerEventCellViewInfo.GetResourceItem: TcxSchedulerStorageResourceItem;
begin
  Result := nil;
  if ResourceInfo <> nil then
    Result := ResourceInfo.ResourceItem;
end;

function TcxSchedulerEventCellViewInfo.GetSelected: Boolean;
begin
  Result := Event.Selected;
end;

function TcxSchedulerEventCellViewInfo.GetShowingState(
  AIndex: Integer): Boolean;
begin
  case AIndex of
    0: Result := ViewData.ShowFinishTime;
    1: Result := ViewData.ShowStartTime;
    2: Result := ViewData.ShowTimeAsClock;
    3: Result := ViewData.ShowTimeLine;
  else
    Result := False;
  end;
end;

procedure TcxSchedulerEventCellViewInfo.SetBorderColor(AValue: TColor);
begin
  ViewData.BorderColor := AValue;
end;

{ TcxSchedulerEventCellViewInfoList }

procedure TcxSchedulerEventCellViewInfoList.RemoveEvent(
  AEvent: TcxSchedulerEvent);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if Items[I].Event = AEvent then
    begin
      Items[I].Free;
      Delete(I);
      Break;
    end;
end;

function TcxSchedulerEventCellViewInfoList.GetItem(Index: Integer): TcxSchedulerEventCellViewInfo;
begin
  Result := TcxSchedulerEventCellViewInfo(inherited Items[Index]);
end;

{ TcxSchedulerMoreEventsButtonViewInfo }

procedure TcxSchedulerMoreEventsButtonViewInfo.Click;
begin
  if Assigned(FOnClick) then
    FOnClick(Self);
end;

procedure TcxSchedulerMoreEventsButtonViewInfo.DoDraw;
begin
  PainterHelper.DrawTransparentImage(Canvas, MoreEventButtonGlyphs, Bounds, Byte(not IsDown), ScaleFactor);
end;

procedure TcxSchedulerMoreEventsButtonViewInfo.InitHitTest(
  AHitTest: TcxSchedulerCustomResourceViewHitTest);
begin
  inherited InitHitTest(AHitTest);
  AHitTest.SetBitState(htcButton, True);
  AHitTest.FButton := Self;
  if Int(DateTime) > 0 then
    AHitTest.SetHitTime(htcTime, Int(DateTime));
end;

{ TcxSchedulerMoreEventsModernButtonViewInfo }

procedure TcxSchedulerMoreEventsModernButtonViewInfo.DoDraw;
begin
  TcxSchedulerPainterHelper.DrawMoreEventButton(Canvas, Bounds, ScaleFactor, Byte(IsDown) +
    2 * Byte((FHitTest <> nil) and (FHitTest.HitMoreEventsButton = Self)));
end;

procedure TcxSchedulerMoreEventsModernButtonViewInfo.InitHitTest(AHitTest: TcxSchedulerCustomResourceViewHitTest);
begin
  FHitTest := AHitTest;
  inherited InitHitTest(AHitTest);
end;

{ TcxSchedulerContentNavigationButtonViewInfo }

constructor TcxSchedulerContentNavigationButtonViewInfo.Create(APainter: TcxCustomLookAndFeelPainter; const ABounds, AVisibleRect: TRect;
  const AViewParams: TcxViewParams; AScaleFactor: TdxScaleFactor; AUseRightToLeftAlignment: Boolean);
begin
  inherited Create(APainter, ABounds, AVisibleRect, AViewParams, AScaleFactor, AUseRightToLeftAlignment);
  FIsVertical := True;
end;

procedure TcxSchedulerContentNavigationButtonViewInfo.Click;
begin
  if Enabled and Assigned(FOnClick) then
    FOnClick(Self);
end;

procedure TcxSchedulerContentNavigationButtonViewInfo.DoDraw;

   function GetButtonKindRTL: TcxSchedulerContentNavigationButtonKind;
   const
     AKindRTL: array[Boolean] of TcxSchedulerContentNavigationButtonKind = (nbkNext, nbkPrevious);
   begin
     Result := Kind;
     if FIsVertical and UseRightToLeftAlignment then
       Result := AKindRTL[Result = nbkNext];
   end;

var
  AState: TcxButtonState;
begin
  if Enabled then
  begin
    if (FHitTest <> nil) and (FHitTest.FContentNavigationButton = Self) then
      AState := cxbsHot
    else
      AState := cxbsNormal;
  end
  else
    AState := cxbsDisabled;
  Painter.DrawSchedulerNavigationButton(Canvas, Bounds, GetButtonKindRTL = nbkNext, AState, Caption, TextRect, ArrowRect, FIsVertical);
end;

procedure TcxSchedulerContentNavigationButtonViewInfo.DoRightToLeftConversion(const AClientBounds: TRect);
begin
  inherited DoRightToLeftConversion(AClientBounds);
  FArrowRect := TdxRightToLeftLayoutConverter.ConvertRect(FArrowRect, AClientBounds);
  FTextRect := TdxRightToLeftLayoutConverter.ConvertRect(FTextRect, AClientBounds);
end;

procedure TcxSchedulerContentNavigationButtonViewInfo.InitHitTest(
  AHitTest: TcxSchedulerCustomResourceViewHitTest);
begin
  FHitTest := AHitTest;
  inherited InitHitTest(AHitTest);
  AHitTest.SetBitState(htcNavigationButton, True);
  AHitTest.FContentNavigationButton := Self;
end;

function TcxSchedulerContentNavigationButtonViewInfo.GetEnabled: Boolean;
begin
  Result := Abs(Interval) < MaxDateTime;
end;

procedure TcxSchedulerContentNavigationButtonViewInfo.SetIsVertical(AValue: Boolean);
begin
  FIsVertical := AValue;
end;

{ TcxSchedulerGroupSeparatorCellViewInfo }

destructor TcxSchedulerGroupSeparatorCellViewInfo.Destroy;
begin
  FreeAndNil(FRotateBitmap);
  inherited Destroy;
end;

procedure TcxSchedulerGroupSeparatorCellViewInfo.DoDraw;

  procedure CreateRotatedBitmap;
  begin
    FRotateBitmap := TcxBitmap.CreateSize(Bounds, pf32bit);
    FRotateBitmap.Canvas.Lock;
    FRotateBitmap.cxCanvas.WindowOrg := Bounds.TopLeft;
  end;

begin
  if Rotated then
  begin
    if FRotateBitmap = nil then
    begin
      CreateRotatedBitmap;
      FDrawRotatedBackground := DrawBackground(FRotateBitmap.cxCanvas, Bounds);

      FRotateBitmap.Rotate(raPlus90, True);
      Painter.DrawSchedulerScaledGroupSeparator(FRotateBitmap.cxCanvas,
        FRotateBitmap.ClientRect, Color, ScaleFactor, DrawRotateBackground);
      FRotateBitmap.Rotate(raPlus90, True);
    end;
    Canvas.Draw(Bounds.Left, Bounds.Top, FRotateBitmap);
  end
  else
    Painter.DrawSchedulerScaledGroupSeparator(Canvas, Bounds, Color, ScaleFactor, DrawBackground);
end;

procedure TcxSchedulerGroupSeparatorCellViewInfo.InitHitTest(
  AHitTest: TcxSchedulerCustomResourceViewHitTest);
begin
  AHitTest.SetBitState(htcGroupSeparator, True);
end;

function TcxSchedulerGroupSeparatorCellViewInfo.DrawRotateBackground(
  ACanvas: TcxCanvas; const ABounds: TRect): Boolean;
begin
  Result := FDrawRotatedBackground;
end;

{ TcxSchedulerResourceViewInfo }

constructor TcxSchedulerResourceViewInfo.Create(
  AResource: TcxSchedulerStorageResourceItem);
begin
  FResourceItem := AResource;
  FColor := AResource.BackgroundColor;
  FResourceID := AResource.ResourceID;
  FCaption := AResource.Resources.GetResourceName(FResourceItem);
  FImages := AResource.Resources.Images;
  FImageIndex := AResource.ActualImageIndex;
end;

{ TcxSchedulerExternalPainter }

procedure TcxSchedulerExternalPainter.DoCustomDrawBackground(
  AViewInfo: TcxSchedulerBackgroundCellViewInfo; var ADone: Boolean);
begin
  if FCommonPainter <> nil then
    FCommonPainter.DoCustomDrawBackground(AViewInfo, ADone);
end;

procedure TcxSchedulerExternalPainter.DoCustomDrawButton(
  AViewInfo: TcxSchedulerMoreEventsButtonViewInfo; var ADone: Boolean);
begin
  if FCommonPainter <> nil then
    FCommonPainter.DoCustomDrawButton(AViewInfo, ADone);
end;

procedure TcxSchedulerExternalPainter.DoCustomDrawContent(
  AViewInfo: TcxSchedulerContentCellViewInfo; var ADone: Boolean);
begin
  if FCommonPainter <> nil then
    FCommonPainter.DoCustomDrawContent(AViewInfo, ADone);
end;

procedure TcxSchedulerExternalPainter.DoCustomDrawDayHeader(
  AViewInfo: TcxSchedulerDayHeaderCellViewInfo; var ADone: Boolean);
begin
  if FCommonPainter <> nil then
    FCommonPainter.DoCustomDrawDayHeader(AViewInfo, ADone);
end;

procedure TcxSchedulerExternalPainter.DoCustomDrawEvent(
  AViewInfo: TcxSchedulerEventCellViewInfo; var ADone: Boolean);
begin
  if FCommonPainter <> nil then
    FCommonPainter.DoCustomDrawEvent(AViewInfo, ADone);
end;

procedure TcxSchedulerExternalPainter.DoCustomDrawGroupSeparator(
  AViewInfo: TcxSchedulerGroupSeparatorCellViewInfo; var ADone: Boolean);
begin
  if FCommonPainter <> nil then
    FCommonPainter.DoCustomDrawGroupSeparator(AViewInfo, ADone);
end;

procedure TcxSchedulerExternalPainter.DoCustomDrawNavigationButton(
  AViewInfo: TcxSchedulerContentNavigationButtonViewInfo; var ADone: Boolean);
begin
  if FCommonPainter <> nil then
    FCommonPainter.DoCustomDrawNavigationButton(AViewInfo, ADone);
end;

procedure TcxSchedulerExternalPainter.DoCustomDrawResourceHeader(
  AViewInfo: TcxSchedulerHeaderCellViewInfo; var ADone: Boolean);
begin
  if FCommonPainter <> nil then
    FCommonPainter.DoCustomDrawResourceHeader(AViewInfo, ADone);
end;

function TcxSchedulerExternalPainter.HasCustomDrawGroupSeparator: Boolean;
begin
  Result := (FCommonPainter <> nil) and
    FCommonPainter.HasCustomDrawGroupSeparator;
end;

function TcxSchedulerExternalPainter.HasCustomDrawResourceHeader: Boolean;
begin
  Result := (FCommonPainter <> nil) and
    FCommonPainter.HasCustomDrawResourceHeader;
end;

procedure TcxSchedulerExternalPainter.DrawAllDayArea(ACanvas: TcxCanvas;
  const ARect: TRect; ABorderColor: TColor; ABorders: TcxBorders;
  AViewParams: TcxViewParams; ASelected: Boolean; ATransparent: Boolean);
begin
  if ATransparent then
    ACanvas.FrameRect(ARect, ABorderColor, 1, ABorders)
  else
    ACanvas.Rectangle(ARect, AViewParams, ABorders,  ABorderColor, 1);
end;

procedure TcxSchedulerExternalPainter.DrawCellFocus(AViewInfo: TcxSchedulerCustomViewInfoItem; AColor: TColor);
var
  R: TRect;
  APenWidth: Integer;
  ASavedWidth: Integer;
  ASavedColor: TColor;
  ASavedPenStyle: TPenStyle;
  ASavedBrushStyle: TBrushStyle;
begin
  APenWidth := -AViewInfo.ScaleFactor.Apply(1);
  R := AViewInfo.Bounds;
  Dec(R.Bottom);

  ASavedWidth := AViewInfo.Canvas.Pen.Width;
  ASavedColor := AViewInfo.Canvas.Pen.Color;
  ASavedPenStyle := AViewInfo.Canvas.Pen.Style;
  ASavedBrushStyle := AViewInfo.Canvas.Brush.Style;
  try
    AViewInfo.Canvas.Pen.Width := APenWidth;
    AViewInfo.Canvas.Pen.Color := AColor;
    AViewInfo.Canvas.Pen.Style := psDot;
    AViewInfo.Canvas.Brush.Style := bsClear;
    AViewInfo.Canvas.Rectangle(R);
  finally
    AViewInfo.Canvas.Pen.Width := ASavedWidth;
    AViewInfo.Canvas.Pen.Color := ASavedColor;
    AViewInfo.Canvas.Pen.Style := ASavedPenStyle;
    AViewInfo.Canvas.Brush.Style := ASavedBrushStyle;
  end;
end;

procedure TcxSchedulerExternalPainter.DrawCustomCurrentTime(ACanvas: TcxCanvas; AColor: TColor; AStart: TDateTime; const ABounds: TRect;
  AUseRightToLeftAlignment: Boolean);
var
  Y: Integer;
  ANow: TDateTime;
  R: TRect;
begin
  ANow := dxTimeOf(Now) - dxTimeOf(AStart);
  if (ANow < 0) or (ANow >= HourToTime) then Exit;
  Y := Round(ABounds.Top + (ANow * cxRectHeight(ABounds)) / HourToTime);
  Dec(Y, 3);
  R := ABounds;
  Dec(R.Right);
  if Y < ABounds.Top then
    Y := ABounds.Top;
  if AUseRightToLeftAlignment then
    R := TdxRightToLeftLayoutConverter.ConvertRect(R, ABounds);
  PainterHelper.DrawGradientRect(ACanvas, AColor, cxRectSetTop(R, Y - 6, 6));
end;

procedure TcxSchedulerExternalPainter.DrawCurrentTime(ACanvas: TcxCanvas;
  AColor: TColor; AStart: TDateTime; ABounds: TRect);
begin
  DrawCustomCurrentTime(ACanvas, AColor, AStart, ABounds, False);
end;

procedure TcxSchedulerExternalPainter.DrawCurrentTimeRTL(ACanvas: TcxCanvas; AColor: TColor; AStart: TDateTime; ABounds: TRect);
begin
  DrawCustomCurrentTime(ACanvas, AColor, AStart, ABounds, True);
end;

function TcxSchedulerExternalPainter.NeedDrawSelection: Boolean;
begin
  Result := True;
end;

function TcxSchedulerExternalPainter.DrawCurrentTimeFirst: Boolean;
begin
  Result := True;
end;

procedure TcxSchedulerExternalPainter.DrawEvent(AViewInfo: TcxSchedulerEventCellViewInfo);
begin
  AViewInfo.DrawRect(AViewInfo.Bounds, AViewInfo.Borders, AViewInfo.ViewData.BorderColor);
end;

procedure TcxSchedulerExternalPainter.DrawEventAsGroup(AViewInfo: TcxSchedulerEventCellViewInfo);
begin
  Painter.DrawSchedulerGroup(AViewInfo.Canvas, AViewInfo.Bounds, AViewInfo.Color);
end;

procedure TcxSchedulerExternalPainter.DrawEventAsMilestone(AViewInfo: TcxSchedulerEventCellViewInfo);
begin
  Painter.DrawSchedulerMilestone(AViewInfo.Canvas, AViewInfo.Bounds);
end;

procedure TcxSchedulerExternalPainter.DrawEventAsProgress(
  AViewInfo: TcxSchedulerEventCellViewInfo);
var
  AContent: TRect;
  AProgressRect: TRect;
begin
  with AViewInfo do
  begin
    AContent := cxRectContent(Bounds, Painter.SchedulerEventProgressOffsets);
    AProgressRect := AContent;
    if not AViewInfo.UseRightToLeftAlignment then
      AProgressRect.Right := AProgressRect.Left + Trunc(ViewData.TaskComplete / 100 * cxRectWidth(AProgressRect))
    else
      AProgressRect.Left := AProgressRect.Right - Trunc(ViewData.TaskComplete / 100 * cxRectWidth(AProgressRect));
    Painter.DrawSchedulerEventProgress(Canvas, Bounds, AProgressRect, ViewParams,
      Transparent);
    Canvas.Brush.Style := bsClear;
    Canvas.Font.Color := Painter.ProgressBarTextColor;
    DrawEventAsProgressText(AViewInfo, AContent, AProgressRect,
      IntToStr(ViewData.TaskComplete) + '%');
  end;
end;

procedure TcxSchedulerExternalPainter.DrawEventAsProgressText(
  AViewInfo: TcxSchedulerEventCellViewInfo; AContent: TRect; AProgressRect: TRect;
  const AText: string);
begin
  Painter.DrawProgressBarText(AViewInfo.Canvas, False, False, AText,
    cxRectCenter(AContent, AViewInfo.Canvas.TextExtent(AText)),
    AContent, AProgressRect);
end;

procedure TcxSchedulerExternalPainter.DrawEventLabel(AViewInfo: TcxSchedulerEventCellViewInfo; const R: TRect; AColor: TColor);
var
  AWidth, AHeight: Integer;
  ABorderColor: TColor;
  ARegion: TcxRegion;
begin
    AViewInfo.Canvas.SaveClipRegion;
    try
      AWidth := cxRectWidth(R);
      AHeight := cxRectHeight(R);
      ABorderColor := RGB(128, 128, 128);

      ARegion := TcxRegion.CreateRoundCorners(cxRectInflate(R, 1), AWidth, AHeight);
      AViewInfo.Canvas.SetClipRegion(ARegion, roIntersect);
      dxGpFillRect(AViewInfo.Canvas.Handle, R, ABorderColor);

      ARegion := TcxRegion.CreateRoundCorners(cxRectInflate(R, 1), AWidth + 2, AHeight + 2);
      AViewInfo.Canvas.SetClipRegion(ARegion, roIntersect);
      dxGpFillRect(AViewInfo.Canvas.Handle, R, ABorderColor);

      ARegion := TcxRegion.CreateRoundCorners(R, AWidth, AHeight);
      AViewInfo.Canvas.SetClipRegion(ARegion, roIntersect);
      dxGpFillRect(AViewInfo.Canvas.Handle, R, AColor);
    finally
      AViewInfo.Canvas.RestoreClipRegion;
    end;
end;

procedure TcxSchedulerExternalPainter.DrawTimeGridCurrentTime(ACanvas: TcxCanvas;
  AColor: TColor; const ATimeLineRect: TRect);
begin
  ACanvas.FillRect(ATimeLineRect, AColor);
end;

procedure TcxSchedulerExternalPainter.DrawTimeGridHeader(ACanvas: TcxCanvas;
  ABorderColor: TColor; AViewInfo: TcxSchedulerCustomViewInfoItem;
  ABorders: TcxBorders; ASelected: Boolean);
begin
  with AViewInfo do
    DrawRect(Bounds, Borders, ABorderColor);
end;

function TcxSchedulerExternalPainter.DrawTimeGridTimeScaleTicks: Boolean;
begin
  Result := True;
end;

procedure TcxSchedulerExternalPainter.DrawTimeLine(ACanvas: TcxCanvas;
  const ARect: TRect; AViewParams: TcxViewParams; ABorders: TcxBorders; ABorderColor: TColor);
begin
  ACanvas.Rectangle(ARect, AViewParams, ABorders, ABorderColor);
end;

procedure TcxSchedulerExternalPainter.DrawTimeRulerBackground(ACanvas: TcxCanvas; const ARect: TRect;
  ABorders: TcxBorders; AViewParams: TcxViewParams; ATransparent: Boolean);
begin
  DrawTimeRulerBackground(ACanvas, ARect, ABorders, clBtnShadow, AViewParams, ATransparent);
end;

procedure TcxSchedulerExternalPainter.DrawTimeRulerBackground(ACanvas: TcxCanvas;
  const ARect: TRect; ABorders: TcxBorders; ABorderBolor: TColor; AViewParams: TcxViewParams; ATransparent: Boolean);
begin
  if ATransparent then
    ACanvas.FrameRect(ARect, ABorderBolor, 1, ABorders)
  else
    ACanvas.Rectangle(ARect, AViewParams, ABorders,  ABorderBolor, 1);
end;

procedure TcxSchedulerExternalPainter.DrawShadow(
  ACanvas: TcxCanvas; const ABounds, AVisibleBounds: TRect; AScaleFactor: TdxScaleFactor = nil);
begin
  if AScaleFactor = nil then
    AScaleFactor := dxSystemScaleFactor;
  PainterHelper.DrawShadow(ACanvas, ABounds, AVisibleBounds, AScaleFactor);
end;

function TcxSchedulerExternalPainter.DrawShadowFirst: Boolean;
begin
  Result := False;
end;

function TcxSchedulerExternalPainter.GetColorizedColor(ASource: TColor; AEvent: TcxSchedulerEventCellViewInfo): TColor;
var
  AEventHSL: TdxHSL;
  ASourceHSL: TdxHSL;
begin
  AEventHSL := TdxColorSpaceConverter.ColorToHSL(AEvent.GetTimeLineBorderColor);
  ASourceHSL := TdxColorSpaceConverter.ColorToHSL(ASource);
  if (AEventHSL.H = 0) and (AEventHSL.S = 0) and (ASourceHSL.H = 0) and (ASourceHSL.S = 0) then
    ASourceHSL.L := Min(ASourceHSL.L, 0.97)
  else
  begin
    ASourceHSL.H := AEventHSL.H;
    ASourceHSL.S := Max(ASourceHSL.S, Min(AEventHSL.S, 0.6));
    if ASourceHSL.S > 0 then
      ASourceHSL.L := Min(ASourceHSL.L, 0.97);
  end;
  Result := TdxColorSpaceConverter.HSLToColor(ASourceHSL);
end;

function TcxSchedulerExternalPainter.GetEventBorderColor(AViewInfo: TcxSchedulerEventCellViewInfo): TColor;
begin
  Result := AViewInfo.BorderColor;
end;

function TcxSchedulerExternalPainter.GetEventBorderSize(AViewInfo: TcxSchedulerEventCellViewInfo): Integer;
begin
  Result := cxEventBorderWidth;
end;

function TcxSchedulerExternalPainter.GetEventLabelSize(AScaleFactor: TdxScaleFactor = nil): TSize;
begin
  if AScaleFactor = nil then
    Result := dxSystemScaleFactor.Apply(cxSize(14, 14))
  else
    Result := AScaleFactor.Apply(cxSize(14, 14));
end;

function TcxSchedulerExternalPainter.GetEventSelectionBorderColor(AViewInfo: TcxSchedulerEventCellViewInfo): TColor;
begin
  Result := Painter.DefaultSchedulerSelectedEventBorderColor;
end;

function TcxSchedulerExternalPainter.GetEventSelectionBorderSize(AViewInfo: TcxSchedulerEventCellViewInfo): Integer;
begin
  Result := 0;
  if (AViewInfo <> nil) and (AViewInfo.ViewStyle = svsModern) then
    Result := AViewInfo.ScaleFactor.Apply(2);
end;

function TcxSchedulerExternalPainter.GetEventSelectionExtends: TRect;
begin
  Result := cxNullRect;
end;

function TcxSchedulerExternalPainter.GetEventSelectionExtends(AViewInfo: TcxSchedulerEventCellViewInfo): TRect;
var
  ABorderSize: Integer;
begin
  ABorderSize := GetEventSelectionBorderSize(AViewInfo);
  if (AViewInfo <> nil) and (AViewInfo.ViewStyle = svsModern) then
    Result := cxRect(0, ABorderSize - 1, 0, ABorderSize - 1)
  else
    Result := cxRect(ABorderSize, ABorderSize, ABorderSize, ABorderSize);
end;

function TcxSchedulerExternalPainter.GetPainterHelper: TcxSchedulerPainterHelperClass;
begin
  Result := SchedulerHelpersFactory.PainterHelperClass;
end;

function TcxSchedulerExternalPainter.MoreButtonSize(ASize: TSize; AScaleFactor: TdxScaleFactor = nil): TSize;
begin
  if AScaleFactor = nil then
    Result := dxSystemScaleFactor.Apply(ASize)
  else
    Result := AScaleFactor.Apply(ASize);
end;

{ TcxSchedulerCustomResourceView }

constructor TcxSchedulerCustomResourceView.Create(
  AOwner: TcxCustomScheduler);
begin
  inherited Create(AOwner);
  FShowEndTime := True;
  InitializePainter;
end;

destructor TcxSchedulerCustomResourceView.Destroy;
begin
  FinilizePainter;
  inherited Destroy;
end;

procedure TcxSchedulerCustomResourceView.Assign(Source: TPersistent);
begin
  if Source is TcxSchedulerCustomResourceView then
  begin
    ShowEndTime := TcxSchedulerCustomResourceView(Source).ShowEndTime;
    ShowTimeAsClock := TcxSchedulerCustomResourceView(Source).ShowTimeAsClock;
    FGroupingKind := TcxSchedulerCustomResourceView(Source).GroupingKind;
  end;
  inherited Assign(Source);
end;

procedure TcxSchedulerCustomResourceView.InitializePainter;
begin
  if FExternalPainter = nil then
    FExternalPainter := ExternalPainterClass.Create;
end;

procedure TcxSchedulerCustomResourceView.FinilizePainter;
begin
  FExternalPainter := nil;
end;

procedure TcxSchedulerCustomResourceView.CalculateViewInfo;
begin
  Adapter.Calculate;
end;

procedure TcxSchedulerCustomResourceView.CheckGroupingKind(
  AStyle: TcxSchedulerGroupingKind; var ActuallyStyle: TcxSchedulerGroupingKind);
begin
  ActuallyStyle := AStyle;
end;

procedure TcxSchedulerCustomResourceView.CheckResourceNavigatorKind;
begin
  ViewInfo.CheckResourceNavigatorKind;
end;

procedure TcxSchedulerCustomResourceView.ClearCachedData;
begin
  ViewInfo.ImageCacheManager.Invalidate;
end;

function TcxSchedulerCustomResourceView.CreateController: TcxSchedulerSubControlController;
begin
  Result := TcxSchedulerCustomResourceViewController.Create(Self);
end;

function TcxSchedulerCustomResourceView.CreateHitTest: TcxSchedulerSubControlHitTest;
begin
  Result := TcxSchedulerCustomResourceViewHitTest.Create(Self);
end;

function TcxSchedulerCustomResourceView.CreateViewAdapter: TcxCustomResourceViewAdapter;
begin
  Result := TcxCustomResourceViewAdapter.Create(Self);
end;

procedure TcxSchedulerCustomResourceView.CreateSubClasses;
begin
  inherited CreateSubClasses;
  FAdapter := CreateViewAdapter;
  ViewInfo.FAdapter := FAdapter;
end;

procedure TcxSchedulerCustomResourceView.DestroySubClasses;
var
  Adapter: TObject;
begin
  Adapter := ViewInfo.FAdapter;
  inherited DestroySubClasses;
  Adapter.Free;
end;

procedure TcxSchedulerCustomResourceView.DoCustomDrawBackground(
  AViewInfo: TcxSchedulerBackgroundCellViewInfo; var ADone: Boolean);
begin
  if Assigned(FOnCustomDrawBackground) then
    FOnCustomDrawBackground(Self, Canvas, AViewInfo, ADone);
end;

procedure TcxSchedulerCustomResourceView.DoCustomDrawButton(
  AViewInfo: TcxSchedulerMoreEventsButtonViewInfo; var ADone: Boolean);
begin
  if Assigned(FOnCustomDrawButton) then
    FOnCustomDrawButton(Self, Canvas, AViewInfo, ADone);
end;

procedure TcxSchedulerCustomResourceView.DoCustomDrawContent(
  AViewInfo: TcxSchedulerContentCellViewInfo; var ADone: Boolean);
begin
  if Assigned(FOnCustomDrawContent) then
    FOnCustomDrawContent(Self, Canvas, AViewInfo, ADone);
end;

procedure TcxSchedulerCustomResourceView.DoCustomDrawDayHeader(
  AViewInfo: TcxSchedulerDayHeaderCellViewInfo; var ADone: Boolean);
begin
  if Assigned(FOnCustomDrawDayHeader) then
    FOnCustomDrawDayHeader(Self, Canvas, AViewInfo, ADone);
end;

procedure TcxSchedulerCustomResourceView.DoCustomDrawEvent(
  AViewInfo: TcxSchedulerEventCellViewInfo; var ADone: Boolean);
begin
  if Assigned(FOnCustomDrawEvent) then
    FOnCustomDrawEvent(Self, Canvas, AViewInfo, ADone);
end;

procedure TcxSchedulerCustomResourceView.DoCustomDrawGroupSeparator(
  AViewInfo: TcxSchedulerGroupSeparatorCellViewInfo; var ADone: Boolean);
begin
  if Assigned(FOnCustomDrawGroupSeparator) then
    FOnCustomDrawGroupSeparator(Self, Canvas, AViewInfo, ADone);
end;

procedure TcxSchedulerCustomResourceView.DoCustomDrawNavigationButton(
  AViewInfo: TcxSchedulerContentNavigationButtonViewInfo; var ADone: Boolean);
begin
  if Assigned(FOnCustomDrawNavigationButton) then
    FOnCustomDrawNavigationButton(Self, Canvas, AViewInfo, ADone);
end;

procedure TcxSchedulerCustomResourceView.DoCustomDrawResourceHeader(
  AViewInfo: TcxSchedulerHeaderCellViewInfo; var ADone: Boolean);
begin
  if Assigned(FOnCustomDrawResourceHeader) then
    FOnCustomDrawResourceHeader(Self, Canvas, AViewInfo, ADone);
end;

procedure TcxSchedulerCustomResourceView.DoLayoutChanged;
begin
  inherited DoLayoutChanged;
  if Active then
  begin
    if (ViewInfo.GroupingKind = gkNone) and (Scheduler.SelResource <> nil) then
      with Controller.Navigation do
        ReplaceSelParams(SelStart, SelFinish, nil)
    else
      if (ViewInfo.GroupingKind <> gkNone) and (Scheduler.SelResource = nil) then
        with Controller.Navigation do
          ReplaceSelParams(SelStart, SelFinish, VisibleResources[0]);
  end;
end;

procedure TcxSchedulerCustomResourceView.DoMouseDown(AButton: TMouseButton; AShift: TShiftState; X, Y: Integer);
begin
  inherited DoMouseDown(AButton, AShift, X, Y);
  if AButton = mbRight then
    CheckSelectOnRightClick;
end;

function TcxSchedulerCustomResourceView.DoShowPopupMenu(X, Y: Integer): Boolean;
begin
  if HitTest.HitAtEvent then
    Result := not Controller.IsEditing and Scheduler.EventPopupMenu.Popup(X, Y)
  else
    Result := Scheduler.ContentPopupMenu.Popup(X, Y)
end;

function TcxSchedulerCustomResourceView.FindEventViewInfo(AEvent: TcxSchedulerEvent;
  const ADate: TDateTime; AResource: TcxSchedulerStorageResourceItem;
  var AViewInfo: TcxSchedulerEventCellViewInfo): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to ViewInfo.EventCells.Count - 1 do
  begin
    AViewInfo := ViewInfo.EventCells[I];
    if AViewInfo.Event = AEvent then
    begin
      Result := True;
      with AViewInfo do
      begin
        if (AResource = nil) or (ResourceItem = AResource) then
          if (ADate = NullDate) or ((dxDateOf(ContentStart) = ADate) or (IsHeaderEvent and
            (ADate >= dxDateOf(ContentStart)) and (ADate < dxDateOf(ContentFinish)))) then Break;
      end;
    end;
  end;
end;

function TcxSchedulerCustomResourceView.GetCommonViewItemsPainter: IcxSchedulerCommonViewItemsPainter;
var
  ACustomPainter: IcxSchedulerCommonViewItemsPainter;
begin
  if not Supports(TObject(Scheduler), IcxSchedulerCommonViewItemsPainter, ACustomPainter) then
    ACustomPainter := nil;
  InitializePainter;
  ExternalPainter.Painter := LookAndFeelPainter;
  ExternalPainter.CommonPainter := ACustomPainter;
  Result := ExternalPainter;
end;

function TcxSchedulerCustomResourceView.GetCompressWeekEnd: Boolean;
begin
  Result := False;
end;

function TcxSchedulerCustomResourceView.GetEditRectForEvent(
  AEvent: TcxSchedulerControlEvent; const ADate: TDateTime;
  AResource: TcxSchedulerStorageResourceItem): TRect;
var
  AViewInfo: TcxSchedulerEventCellViewInfo;
begin
  if FindEventViewInfo(AEvent, ADate, AResource, AViewInfo) then
    Result := AViewInfo.EditingRect
  else
    Result := inherited GetEditRectForEvent(AEvent, ADate, AResource);
end;

procedure TcxSchedulerCustomResourceView.InitScrollBarsParameters;
begin
  ViewInfo.InitScrollBarsParameters;
end;

function TcxSchedulerCustomResourceView.GetEventHintText(AEvent: TcxSchedulerControlEvent): string;
var
  AViewInfo: TcxSchedulerEventCellViewInfo;
begin
  if FindEventViewInfo(AEvent, NullDate, nil, AViewInfo) then
    Result := AViewInfo.Hint
  else
    Result := inherited GetEventHintText(AEvent);
end;

function TcxSchedulerCustomResourceView.GetEventVisibility(
  AEvent: TcxSchedulerControlEvent): Boolean;
var
  AViewInfo: TcxSchedulerEventCellViewInfo;
begin
  Result := FindEventViewInfo(AEvent, NullDate, nil, AViewInfo)
end;

function TcxSchedulerCustomResourceView.GetGroupingKind: TcxSchedulerGroupingKind;
begin
  Result := GroupingKind;
  if Result = gkDefault then
    Result := OptionsView.GroupingKind;
end;

function TcxSchedulerCustomResourceView.HasCustomDrawGroupSeparator: Boolean;
begin
  Result := Assigned(FOnCustomDrawGroupSeparator) or
    ExternalPainter.HasCustomDrawGroupSeparator;
end;
function TcxSchedulerCustomResourceView.HasCustomDrawResourceHeader: Boolean;
begin
  Result := Assigned(FOnCustomDrawResourceHeader) or
    ExternalPainter.HasCustomDrawResourceHeader;
end;

function TcxSchedulerCustomResourceView.IsShowResources: Boolean;
var
  AKind: TcxSchedulerGroupingKind;
begin
  Result := inherited IsShowResources;
  if Result then
  begin
    AKind := GroupingKind;
    if AKind = gkDefault then
      AKind := OptionsView.GroupingKind;
    Result := AKind <> gkNone;
  end;
end;

procedure TcxSchedulerCustomResourceView.LookAndFeelChanged(
  Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues);
begin
  ViewInfo.ImageCacheManager.Invalidate;
end;

procedure TcxSchedulerCustomResourceView.Scroll(AScrollBarKind: TScrollBarKind;
  AScrollCode: TScrollCode; var AScrollPos: Integer);
begin
  if AScrollBarKind = sbHorizontal then
    ViewInfo.ScrollHorizontal(AScrollCode, AScrollPos)
  else
    ViewInfo.ScrollVertical(AScrollCode, AScrollPos);
  inherited Scroll(AScrollBarKind, AScrollCode, AScrollPos);
end;

procedure TcxSchedulerCustomResourceView.SetGroupingKind(
  AValue: TcxSchedulerGroupingKind);
begin
  CheckGroupingKind(AValue, AValue);
  if (FGroupingKind <> AValue) then
  begin
    FGroupingKind := AValue;
    LayoutChanged;
  end;
end;

procedure TcxSchedulerCustomResourceView.CheckSelectOnRightClick;
var
  ATime: TDateTime;
  AResource: TcxSchedulerStorageResourceItem;
begin
  if Scheduler.OptionsBehavior.SelectOnRightClick and
    not IsRelevantSelection(ATime, AResource) and HitTest.HitAtTime then
  begin
    if AResource <> nil then
      Scheduler.SelectTime(ATime, ATime, AResource)
    else
      Scheduler.SelectTime(ATime, ATime, Scheduler.SelResource)
  end;
end;

function TcxSchedulerCustomResourceView.GetViewInfo: TcxSchedulerCustomResourceViewViewInfo;
begin
  Result := TcxSchedulerCustomResourceViewViewInfo(inherited ViewInfo);
end;

function TcxSchedulerCustomResourceView.IsRelevantSelection(out ATime: TDateTime; out AResource: TcxSchedulerStorageResourceItem): Boolean;
begin
  ATime := HitTest.Time;
  if ATime < 1 then
    ATime := ATime + dxDateOf(Scheduler.SelStart);
  AResource := HitTest.Resource;
  Result := ((ATime >= Scheduler.SelStart) and (ATime < Scheduler.SelFinish)) and
    ((Scheduler.SelResource = AResource) or (AResource = nil));
end;

procedure TcxSchedulerCustomResourceView.SetEventImagesLayout(
  const AValue: TcxSchedulerEventImagesLayout);
begin
  if FEventImagesLayout <> AValue then
  begin
    FEventImagesLayout := AValue;
    LayoutChanged;
  end;
end;

procedure TcxSchedulerCustomResourceView.SetShowEndTime(
  const AValue: Boolean);
begin
  if FShowEndTime <> AValue then
  begin
    FShowEndTime := AValue;
    LayoutChanged;
  end;
end;

procedure TcxSchedulerCustomResourceView.SetShowTimeAsClock(
  const AValue: Boolean);
begin
  if FShowTimeAsClock <> AValue then
  begin
    FShowTimeAsClock := AValue;
    LayoutChanged;
  end;
end;

{ TcxCustomResourceViewAdapter }

constructor TcxCustomResourceViewAdapter.Create(
  AView: TcxSchedulerCustomResourceView);
begin
  FView := AView;
end;

procedure TcxCustomResourceViewAdapter.Calculate;
var
  FPrevAdapter: TcxCustomResourceViewAdapter;
begin
  FPrevAdapter := FView.ViewInfo.SetAdapter(Self);
  FPrevAdapter.Store;
  try
    BeforeCalculatePage;
    FView.ViewInfo.Calculate;
    AfterCalculatePage;
  finally
    FView.ViewInfo.SetAdapter(FPrevAdapter);
    if (FPrevAdapter <> Self) and FView.Active then
    begin
      FPrevAdapter.Restore;
      FView.ViewInfo.Calculate;
    end;
    FView.ViewInfo.FStylesAdapter := nil;
  end;
end;

procedure TcxCustomResourceViewAdapter.GetPageResources(
  AResources: TcxObjectList);
var
  ACount, I: Integer;
  AResourceViewInfo: TcxSchedulerResourceViewInfo;
begin
  if Resources = nil then Exit;
  Scheduler.ValidateFirstVisibleResourceIndex;
  ACount := Resources.VisibleResourceCount;
  with Scheduler.OptionsView do
  begin
    if (ResourcesPerPage > 0) and (ACount > ResourcesPerPage) then
      ACount := ResourcesPerPage;
  end;
  for I := 0 to ACount - 1 do
  begin
    AResourceViewInfo := TcxSchedulerResourceViewInfo.Create(
      Resources.VisibleResources[Scheduler.FirstVisibleResourceIndex + I]);
    AResources.Add(AResourceViewInfo);
  end;
end;

procedure TcxCustomResourceViewAdapter.GetPageDays(
  APageIndex: Integer; ADays: TcxSchedulerDateList);
begin
end;

procedure TcxCustomResourceViewAdapter.Invalidate;
begin
end;

function TcxCustomResourceViewAdapter.CanCalculate: Boolean;
begin
  Result := View.Active;
end;

function TcxCustomResourceViewAdapter.GetActualStart: TDateTime;
begin
  Result := PrintFrom;
end;

function TcxCustomResourceViewAdapter.GetActualFinish: TDateTime;
begin
  Result := PrintTo;
end;

function TcxCustomResourceViewAdapter.GetCompressWeekends: Boolean;
begin
  Result := View.GetCompressWeekEnd;
end;

function TcxCustomResourceViewAdapter.GetDontPrintFreeTime: Boolean;
begin
  Result := False;
end;

function TcxCustomResourceViewAdapter.GetDontPrintWeekEnds: Boolean;
begin
  Result := False;
end;

function TcxCustomResourceViewAdapter.GetHideSelection: Boolean;
begin
  Result := View.OptionsView.HideSelection and not View.Scheduler.IsFocused;
end;

function TcxCustomResourceViewAdapter.GetIsPrinting: Boolean;
begin
  Result := False;
end;

function TcxCustomResourceViewAdapter.GetPageBounds: TRect;
begin
  Result := View.ClientRect;
end;

function TcxCustomResourceViewAdapter.GetPagesPerWeek: Byte;
begin
  Result := 1;
end;

function TcxCustomResourceViewAdapter.GetPrimaryPage: Boolean;
begin
  Result := True;
end;

function TcxCustomResourceViewAdapter.GetPrintExactlyOneMonth: Boolean;
begin
  Result := False;
end;

function TcxCustomResourceViewAdapter.GetPrintRange(
  Index: Integer): TDateTime;
begin
  Result := NullDate;
end;

function TcxCustomResourceViewAdapter.GetStylesAdapter: IcxSchedulerStylesAdapter;
begin
  Result := FView.Styles;
end;

function TcxCustomResourceViewAdapter.GetView: TcxSchedulerCustomResourceView;
begin
  Result := FView;
end;

procedure TcxCustomResourceViewAdapter.AfterCalculatePage;
begin
end;

procedure TcxCustomResourceViewAdapter.BeforeCalculatePage;
begin
end;

procedure TcxCustomResourceViewAdapter.DoInitialize(
  var ASelectedDays: TcxSchedulerDateList; var AEvents: TcxSchedulerCachedEventList);
begin
  ASelectedDays := View.Scheduler.SelectedDays;
  AEvents := ViewInfo.GetSchedulerEventsList;
end;

function TcxCustomResourceViewAdapter.GetPageHeaderText: string;
begin
  Result := ViewInfo.GetPageHeaderText;
end;

procedure TcxCustomResourceViewAdapter.Store;
begin
end;

procedure TcxCustomResourceViewAdapter.Restore;
begin
end;

function TcxCustomResourceViewAdapter.GetCells: TcxObjectList;
begin
  Result := ViewInfo.FCells;
end;

function TcxCustomResourceViewAdapter.GetContentLineHeight: Integer;
var
  AEventHeight, AIconHeight: Integer;
  AImages: TCustomImageList;
begin
  AEventHeight := View.OptionsView.EventHeight;
  AImages := View.Scheduler.EventImages;
  Result := ViewInfo.GetFontHeight(StylesAdapter.GetEventParams(nil)) + 3 * ViewInfo.ScaleFactor.Apply(cxTextOffset);
  if AEventHeight = 0 then
  begin
    if AImages <> nil then
      AIconHeight := Max(ViewInfo.PainterHelper.IconsHeight(ViewInfo.ScaleFactor), AImages.Height)
    else
      AIconHeight := ViewInfo.PainterHelper.IconsHeight(ViewInfo.ScaleFactor);
    Result := Max(AIconHeight + ViewInfo.ScaleFactor.Apply(cxTextOffset) + 3 * ViewInfo.ScaleFactor.Apply(cxEventImagesOffset), Result);
  end
  else
    Result := Max(AEventHeight, Result);
end;

function TcxCustomResourceViewAdapter.GetPainterHelper: TcxSchedulerPainterHelperClass;
begin
  Result := TcxSchedulerHelpersFactory.PainterHelperClass;
end;

function TcxCustomResourceViewAdapter.GetResourceNavigator: TcxSchedulerResourceNavigator;
begin
  Result := View.Scheduler.ResourceNavigator;
end;

function TcxCustomResourceViewAdapter.GetResources: TcxSchedulerStorageResourceItems;
begin
  Result := View.Resources;
end;

function TcxCustomResourceViewAdapter.GetScheduler: TcxCustomScheduler;
begin
  Result := View.Scheduler;
end;

function TcxCustomResourceViewAdapter.GetViewInfo: TcxSchedulerCustomResourceViewViewInfo;
begin
  Result := View.ViewInfo;
end;

{ TcxSchedulerEventImageItem }

constructor TcxSchedulerEventImageItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FAutoHide := True;
  FImageIndex := -1;
  FVisible := True;
end;

procedure TcxSchedulerEventImageItem.DoRightToLeftConversion(const AClientBounds: TRect);
begin
  FBounds := TdxRightToLeftLayoutConverter.ConvertRect(FBounds, AClientBounds);
end;

function TcxSchedulerEventImageItem.GetHeight: Integer;
begin
  Result := Images.Height;
end;

function TcxSchedulerEventImageItem.GetWidth: Integer;
begin
  Result := Images.Width;
end;

{ TcxSchedulerEventImages }

constructor TcxSchedulerEventImages.Create(ALayout: TcxSchedulerEventImagesLayout; AScaleFactor: TdxScaleFactor);
begin
  inherited Create(TcxSchedulerEventImageItem);
  FLayout := ALayout;
  FItemHeight := AScaleFactor.Apply(EventImages.Height);
  FItemWidth := AScaleFactor.Apply(EventImages.Width);
  FEventImageGap := AScaleFactor.Apply(cxEventImagesGap);
end;

function TcxSchedulerEventImages.Add(AImageIndex: Integer;
  AutoHide: Boolean = True): TcxSchedulerEventImageItem;
begin
  if (FImages = nil) or ((AImageIndex < 0) or (AImageIndex >= FImages.Count)) then
  begin
    Result := nil;
    Exit;
  end;
  Result := TcxSchedulerEventImageItem(inherited Add);
  Result.FImageIndex := AImageIndex;
  Result.AutoHide := AutoHide;
  Result.FImageType := eitCustom;
  Result.FImages := FImages;
end;

function TcxSchedulerEventImages.AddStandardImage(
  AImageType: TcxSchedulerEventImageType;
  AutoHide: Boolean = True): TcxSchedulerEventImageItem;
const
  Indexes: array[TcxSchedulerEventImageType] of TcxImageIndex = (4, 2, 3, -1);
begin
  Result := TcxSchedulerEventImageItem(inherited Add);
  Result.AutoHide := AutoHide;
  Result.FImageType := AImageType;
  Result.FImages := EventImages;
  Result.FImageIndex := Indexes[AImageType];
end;

function TcxSchedulerEventImages.Calculate(const R: TRect): Integer;
begin
  case FLayout of
    eilAuto: Result := CalculateImages(R);
    eilHorizontal: Result := CalculateSingleLineImages(R);
  else
    Result := CalculateSingleColumnImages(R);
  end;
end;

procedure TcxSchedulerEventImages.CalculateForCols(AVisibleImages: TList;
  const ATopLeft: TPoint; AColCount: Integer);
var
  ARow, ACol, AIndex: Integer;
begin
  AIndex := 0;
  ACol := 0;
  ARow := 0;
  while AIndex < TotalVisibleImageCount do
  begin
    SetItemBounds(TcxSchedulerEventImageItem(AVisibleImages[AIndex]),
      ATopLeft.X + ACol * (ItemWidth + FEventImageGap),
      ATopLeft.Y + ARow * (ItemHeight + FEventImageGap));
    if ACol = AColCount - 1 then
    begin
      ACol := 0;
      Inc(ARow);
    end
    else Inc(ACol);
    Inc(AIndex);
  end;
end;

function TcxSchedulerEventImages.CalculateImages(const R: TRect): Integer;
var
  I, AWidth, AHeight, ACount, ARows, AColIndex,
  ARowIndex: Integer;
  L: TList;
  RS: TSize;

  procedure CheckAutoHideItems;
  var
    I: Integer;
    AItem: TcxSchedulerEventImageItem;
  begin
    if ACount >= L.Count then Exit;
    for I := L.Count - 1 downto 0 do
    begin
      AItem := TcxSchedulerEventImageItem(L[I]);
      if AItem.AutoHide then
      begin
        AItem.FHidden := True;
        L.Delete(I);
        if ACount >= L.Count then break;
      end;
    end;
  end;

begin
  AHeight := ItemHeight + FEventImageGap;
  RS := cxRectSize(R);
  ARows := Max(1, RS.cy div AHeight);
  if ARows = 1 then
  begin
    Result := CalculateSingleLineImages(R);
    Exit;
  end;
  Result := 0;
  L := CreateVisibleList;
  try
    if L.Count = 0 then Exit;
    AWidth := ItemWidth + FEventImageGap;
    ACount := Max(1, RS.cx div AWidth) * ARows;
    CheckAutoHideItems;
    AColIndex := 0;
    ARowIndex := 0;
    for I := 0 to L.Count - 1 do
    begin
      SetItemBounds(TcxSchedulerEventImageItem(L[I]), R.Left + AColIndex * AWidth, R.Top + ARowIndex * AHeight);
      Result := Max(Result, (AColIndex + 1) * AWidth);
      Inc(ARowIndex);
      if ARowIndex = ARows then
      begin
        Inc(AColIndex);
        ARowIndex := 0;
      end;
    end;
    FVisibleImageCount := L.Count;
  finally
    L.Free;
  end;
end;

function TcxSchedulerEventImages.CalculateSingleLineImages(const R: TRect; ARightLimit: Integer = 0): Integer;
var
  I, ALeft, AImageWidth, ATotalWidth, ARectWidth: Integer;
  L: TList;
  AItem: TcxSchedulerEventImageItem;

  procedure CheckAutoHideItems;
  var
    I: Integer;
    AItem: TcxSchedulerEventImageItem;
  begin
    if ARectWidth >= ATotalWidth then Exit;
    if ARightLimit > 0 then
      ARectWidth := Min(ARightLimit, Max(ARectWidth, ForceVisibleWidth));
    for I := L.Count - 1 downto 0 do
    begin
      AItem := TcxSchedulerEventImageItem(L.List[I]);
      if AItem.AutoHide then
      begin
        AItem.FHidden := True;
        Dec(ATotalWidth, AImageWidth);
        L.Delete(I);
        if ARectWidth >= ATotalWidth then break;
      end;
    end;
  end;

begin
  Result := 0;
  L := CreateVisibleList;
  try
    if L.Count = 0 then Exit;
    ATotalWidth := TotalVisibleWidth;
    ARectWidth := R.Right - R.Left;
    AImageWidth := ItemWidth + FEventImageGap;
    CheckAutoHideItems;
    ALeft := R.Left;
    for I := 0 to L.Count - 1 do
    begin
      AItem := TcxSchedulerEventImageItem(L.List[I]);
      SetItemBounds(AItem, ALeft, R.Top);
      Inc(ALeft, AImageWidth);
      Inc(Result, AImageWidth);
    end;
    FVisibleImageCount := L.Count;
  finally
    L.Free;
  end;
end;

function TcxSchedulerEventImages.CalculateSingleColumnImages(const R: TRect): Integer;
var
  I, ATop, AWidth, AHeight, ATotalHeight: Integer;
  RS: TSize;
  L: TList;

  procedure CheckAutoHideItems;
  var
    I: Integer;
    AItem: TcxSchedulerEventImageItem;
  begin
    if RS.cy + FEventImageGap >= ATotalHeight then Exit;
    for I := L.Count - 1 downto 0 do
    begin
      AItem := TcxSchedulerEventImageItem(L.List[I]);
      if AItem.AutoHide then
      begin
        AItem.FHidden := True;
        Dec(ATotalHeight, AHeight);
        L.Delete(I);
        if RS.cy + FEventImageGap >= ATotalHeight then break;
      end;
    end;
  end;

  function CanAutoHideAllItems: Boolean;
  var
    I: Integer;
  begin
    Result := True;
    for I := 0 to L.Count - 1 do
      if not TcxSchedulerEventImageItem(L.List[I]).AutoHide then
      begin
        Result := False;
        break;
      end;
  end;

begin
  Result := 0;
  L := CreateVisibleList;
  try
    if L.Count = 0 then Exit;
    ATotalHeight := TotalVisibleHeight;
    RS := cxRectSize(R);
    AWidth := ItemWidth + FEventImageGap;
    if (RS.cx < AWidth) and CanAutoHideAllItems then Exit;
    AHeight := ItemHeight + FEventImageGap;
    CheckAutoHideItems;
    if L.Count = 0 then Exit;
    Result := AWidth;
    ATop := R.Top;
    for I := 0 to L.Count - 1 do
    begin
      SetItemBounds(TcxSchedulerEventImageItem(L.List[I]), R.Left, ATop);
      Inc(ATop, AHeight);
    end;
    FVisibleImageCount := L.Count;
  finally
    L.Free;
  end;
end;

function TcxSchedulerEventImages.CreateVisibleList: TList;
var
  I: Integer;
  AItem: TcxSchedulerEventImageItem;
begin
  FVisibleImageCount := 0;
  FForceVisibleHeight := 0;
  FForceVisibleWidth := 0;
  FTotalVisibleImageCount := 0;
  Result := TList.Create;
  for I := 0 to Count - 1 do
  begin
    AItem := Items[I];
    if AItem.Visible then
    begin
      Inc(FTotalVisibleImageCount);
      if not AItem.AutoHide then
      begin
        Inc(FForceVisibleHeight, ItemHeight + FEventImageGap);
        Inc(FForceVisibleWidth, ItemWidth + FEventImageGap);
      end;
      AItem.FHidden := False;
      Result.Add(AItem);
    end
    else AItem.FHidden := True;
  end;
  FTotalVisibleWidth := (ItemWidth + FEventImageGap) * Result.Count;
  FTotalVisibleHeight := (ItemHeight + FEventImageGap) * Result.Count;
end;

procedure TcxSchedulerEventImages.DoRightToLeftConversion(const AClientBounds: TRect);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].DoRightToLeftConversion(AClientBounds);
end;

procedure TcxSchedulerEventImages.Init(AImages: TCustomImageList);
begin
  FVisibleImageCount := 0;
  FImages := AImages;
  if (FImages <> nil) then
  begin
    if FImages.Height > FItemHeight then FItemHeight := FImages.Height;
    if FImages.Width > FItemWidth then FItemWidth := FImages.Width;
  end;
end;

function TcxSchedulerEventImages.Offset(ADeltaX, ADeltaY: Integer): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
    with Items[I] do
    begin
      if not FHidden then
      begin
        FBounds := cxRectOffset(FBounds, ADeltaX, ADeltaY);
        Inc(Result, ItemWidth + FEventImageGap);
      end;
    end;
end;

procedure TcxSchedulerEventImages.SetItemBounds(AItem: TcxSchedulerEventImageItem; ALeft, ATop: Integer);
begin
  Inc(ALeft, Max(0, (ItemWidth - AItem.Width) div 2));
  Inc(ATop, Max(0, (ItemHeight - AItem.Height) div 2));
  AItem.FBounds := cxRectBounds(ALeft, ATop, AItem.Width, AItem.Height);
end;

function TcxSchedulerEventImages.GetImageItem(AIndex: Integer): TcxSchedulerEventImageItem;
begin
  Result := TcxSchedulerEventImageItem(inherited Items[AIndex]);
end;

{ TcxSchedulerCachedImage }

constructor TcxSchedulerCachedImage.Create;
begin
  inherited;
  FImage := TcxBitmap.CreateSize(0, 0, pf32bit);
end;

destructor TcxSchedulerCachedImage.Destroy;
begin
  FreeAndNil(FImage);
  inherited Destroy;
end;

procedure TcxSchedulerCachedImage.Invalidate;
begin
  Image.SetSize(0, 0);
end;

function TcxSchedulerCachedImage.IsValid(AItem: TcxSchedulerCustomViewInfoItem): Boolean;
begin
  Result := (Image.Width = AItem.Width) and (Image.Height = AItem.Height);
end;

procedure TcxSchedulerCachedImage.Update(AItem: TcxSchedulerCustomViewInfoItem);
begin
  if not cxRectIsEmpty(AItem.Bounds) then
  begin
    Image.SetSize(AItem.Width, AItem.Height);
    Image.cxCanvas.Canvas.Lock;
  end;
end;

{ TcxSchedulerResourceHeaderCachedImage }

function TcxSchedulerResourceHeaderCachedImage.IsValid(AItem: TcxSchedulerCustomViewInfoItem): Boolean;
begin
  Result := inherited IsValid(AItem);
  if Result then
  begin
    Result := (TcxSchedulerHeaderCellViewInfo(AItem).DisplayText = FDisplayText) and
      (TcxSchedulerHeaderCellViewInfo(AItem).DateTime = FDateTime);
  end;
end;

procedure TcxSchedulerResourceHeaderCachedImage.Update(AItem: TcxSchedulerCustomViewInfoItem);
begin
  inherited Update(AItem);
  FDisplayText := TcxSchedulerHeaderCellViewInfo(AItem).DisplayText;
  FDateTime := TcxSchedulerHeaderCellViewInfo(AItem).DateTime;
end;

{ TcxSchedulerResourceHeaderCachedImageList }

function TcxSchedulerResourceHeaderCachedImageList.Add(AItem: TcxSchedulerHeaderCellViewInfo): TcxSchedulerResourceHeaderCachedImage;
begin
  Result := TcxSchedulerResourceHeaderCachedImage.Create;
  if AItem.Resource <> nil then
    Result.FResourceItem := AItem.Resource.ResourceItem;
  inherited Add(Result);
end;

function TcxSchedulerResourceHeaderCachedImageList.GetCacheForItem(
  AItem: TcxSchedulerHeaderCellViewInfo): TcxSchedulerResourceHeaderCachedImage;

  function GetResourceItem(AItem: TcxSchedulerHeaderCellViewInfo): TcxSchedulerStorageResourceItem;
  begin
    if AItem.Resource = nil then
      Result := nil
    else
      Result := AItem.Resource.ResourceItem;
  end;

var
  I: Integer;
  ACachedItem: TcxSchedulerResourceHeaderCachedImage;
begin
  Result := nil;
  for I := 0 to Count - 1 do
  begin
    ACachedItem := TcxSchedulerResourceHeaderCachedImage(Items[I]);
    if ACachedItem.FResourceItem = GetResourceItem(AItem) then
    begin
      Result := ACachedItem;
      Break;
    end;
  end;
  if Result = nil then
    Result := Add(AItem);
end;

{ TcxSchedulerImageCacheManager }

constructor TcxSchedulerImageCacheManager.Create;
begin
  FResourceHeaders := TcxSchedulerResourceHeaderCachedImageList.Create;
  FSeparator := TcxSchedulerCachedImage.Create;
end;

destructor TcxSchedulerImageCacheManager.Destroy;
begin
  FResourceHeaders.Free;
  FSeparator.Free;
  inherited Destroy;
end;

procedure TcxSchedulerImageCacheManager.Invalidate;
begin
  ResourceHeaders.Clear;
  Separator.Invalidate;
end;

{ TcxSchedulerCustomResourceViewViewInfo }

constructor TcxSchedulerCustomResourceViewViewInfo.Create(AOwner: TcxSchedulerSubControl);
begin
  inherited Create(AOwner);
  FCells := TcxObjectList.Create;
  FResources := TcxObjectList.Create;
  FGroupSeparatorCells := TcxSchedulerViewInfoCellList.Create;
  FButtons := TcxSchedulerViewInfoCellList.Create;
  FNavigationButtons := TcxSchedulerViewInfoCellList.Create;
  FContentCells := TcxSchedulerViewInfoCellList.Create;
  FEventCells := TcxSchedulerEventCellViewInfoList.Create;
  FDayHeaderCells := TcxSchedulerViewInfoCellList.Create;
  FResourceHeaderCells := TcxSchedulerViewInfoCellList.Create;
  FHeaderContainerCells := TcxSchedulerViewInfoCellList.Create;
  FCells.Add(FEventCells);
  FCells.Add(FGroupSeparatorCells);
  FCells.Add(FDayHeaderCells);
  FCells.Add(FResourceHeaderCells);
  FCells.Add(FContentCells);
  FCells.Add(FHeaderContainerCells);
  // cache
  FImageCacheManager := CreateImageCacheManager;
end;

destructor TcxSchedulerCustomResourceViewViewInfo.Destroy;
begin
  HotContentNavigationButton := nil;
  FreeAndNil(FImageCacheManager);
  FEventImages := nil;
  FResources.Free;
  FButtons.Free;
  FNavigationButtons.Free;
  FCells.Free;
  FreeAndNil(FContentNavigationIntervals);
  inherited Destroy;
end;

procedure TcxSchedulerCustomResourceViewViewInfo.Calculate;
begin
  if ScreenCanvas = nil then
    ScreenCanvas := TcxScreenCanvas.Create;
  try
    Supports(TObject(View.Scheduler), IcxSchedulerEventImages, FEventImages);
    FPrimaryPage := Adapter.GetPrimaryPage;
    Adapter.DoInitialize(FSelectedDays, FEvents);
    FHasVisibleBounds := GetHasVisibleBounds;
    FPrintWeekEnds := not Adapter.DontPrintWeekEnd;
    FPagesPerWeek := Adapter.PagesPerWeek;
    FPageBounds := Adapter.GetPageBounds;
    FCanSelected := not Adapter.GetHideSelection;
    inherited Calculate;
  finally
    FreeAndNil(ScreenCanvas);
  end;
end;

procedure TcxSchedulerCustomResourceViewViewInfo.CalculateHitTest(
  AHitTest: TcxSchedulerCustomResourceViewHitTest);
begin
  FEventCells.CalculateHitTest(AHitTest);
  if not FNavigationButtons.CalculateHitTest(AHitTest) then
    if not FDayHeaderCells.CalculateHitTest(AHitTest) then
      if not FResourceHeaderCells.CalculateHitTest(AHitTest) then
        if not FHeaderContainerCells.CalculateHitTest(AHitTest) then
          if not FButtons.CalculateHitTest(AHitTest) then
            if not FContentCells.CalculateHitTest(AHitTest) then
              FGroupSeparatorCells.CalculateHitTest(AHitTest);
  if AHitTest.HitAtButton and AHitTest.HitAtEvent then
  begin
    AHitTest.FHitObject := nil;
    AHitTest.FEventCell := nil;
  end;
  HotTrackNavigationButtons(AHitTest);
  HotTrackMoreEventsButton(AHitTest);
end;

procedure TcxSchedulerCustomResourceViewViewInfo.InitScrollBarsParameters;
begin
  CheckResourceNavigator;
end;

procedure TcxSchedulerCustomResourceViewViewInfo.ScrollHorizontal(
  AScrollCode: TScrollCode; var AScrollPos: Integer);
begin
end;

procedure TcxSchedulerCustomResourceViewViewInfo.ScrollVertical(
  AScrollCode: TScrollCode; var AScrollPos: Integer);
begin
end;

procedure TcxSchedulerCustomResourceViewViewInfo.SetEventsVisibility(
  AShowSources, AShowClones: Boolean; AForceRepaint: Boolean = False);
var
  ARepaint: Boolean;
begin
  ARepaint := AForceRepaint or (FHideSource <> not AShowSources) or
    (FHideClones <> not AShowClones);
  FHideSource := not AShowSources;
  FHideClones := not AShowClones;
  if ARepaint then View.Refresh;
end;

procedure TcxSchedulerCustomResourceViewViewInfo.MouseLeave;
begin
  if View.HitTest is TcxSchedulerCustomResourceViewHitTest then
  begin
    SetHitContentNavigationButton(nil, TcxSchedulerCustomResourceViewHitTest(View.HitTest));
    SetHitMoreEventsButton(nil, TcxSchedulerCustomResourceViewHitTest(View.HitTest));
  end;
end;

function TcxSchedulerCustomResourceViewViewInfo.AddButton(
  ABounds: TRect; const ADateTime: TDateTime;
   AIsDown: Boolean; AEvent: TcxSchedulerEvent): TcxSchedulerMoreEventsButtonViewInfo;
var
  AMoreButtonSize: TSize;
begin
  ABounds := cxRectInflate(ABounds, -ScaleFactor.Apply(cxTextOffset), -ScaleFactor.Apply(cxTextOffset));
  AMoreButtonSize := View.ExternalPainter.MoreButtonSize(
    Size(PainterHelper.MoreButtonWidth(ViewStyle), PainterHelper.MoreButtonHeight), ScaleFactor);

  ABounds.Left := ABounds.Right - AMoreButtonSize.cx;
  if AIsDown then
    ABounds.Top := ABounds.Bottom - AMoreButtonSize.cy
  else
    ABounds.Bottom := ABounds.Top + AMoreButtonSize.cy;
  if dxDateOf(ADateTime) = 0 then
    CreateCellInstance(GetMoreEventButtonClass, ABounds,
      Styles.GetBackgroundParams, UseRightToLeftAlignment, Result)
  else
    CreateCellInstance(GetMoreEventButtonClass, ABounds,
      GetContentParams(ADateTime, nil), UseRightToLeftAlignment, Result);
  Result.FIsDown := AIsDown;
  Result.FDateTime := ADateTime;
  Result.FOnClick := OnMoreEventsButtonClick;
  Result.Event := AEvent;
  FButtons.Add(Result);
end;

function TcxSchedulerCustomResourceViewViewInfo.AddBackgroundSlot(const ABounds: TRect;
  ABorders: TcxBorders; const AText: string = ''): TcxSchedulerBackgroundSlotCellViewInfo;
begin
  CreateCellInstance(TcxSchedulerBackgroundSlotCellViewInfo, ABounds,
    Styles.GetBackgroundParams, UseRightToLeftAlignment, Result);
  Result.FBorders := ABorders;
  AssignResourceID(Result, -1);
  ContentCells.Add(Result);
end;

function TcxSchedulerCustomResourceViewViewInfo.AddContentCell(const ARect: TRect;
  const AStart, AFinish: TDateTime; AResourceIndex: Integer): TcxSchedulerContentCellViewInfo;
begin
  CreateCellInstance(ContentCellClass, ARect,
    GetContentParams(AStart, Resources[AResourceIndex]), UseRightToLeftAlignment, Result);
  Result.SetTime(AStart, AFinish);
  AssignResourceID(Result, AResourceIndex);
  ContentCells.Add(Result);
end;

procedure TcxSchedulerCustomResourceViewViewInfo.AddContentNavigationButton(
  const AColumnRect: TRect; AResourceIndex: Integer;
  AColumnPositionInResource: TcxSchedulerColumnPositionInResource);

  procedure AddAppointedButton(AKind: TcxSchedulerContentNavigationButtonKind);
  const
    AButtonTexts: array[TcxSchedulerContentNavigationButtonKind] of pointer =
      (@scxPrevAppointment, @scxNextAppointment);
  var
    AButton: TcxSchedulerContentNavigationButtonViewInfo;
    AWidth, AButtonOffset: Integer;
  begin
    CalculateNavigationButtonParams(AColumnRect, AKind, AWidth);

    AButtonOffset := NavigationButtonOffset(AKind, AResourceIndex);
    if IsNavigationButtonsVertical then
      OffsetRect(FButtonBounds, AButtonOffset, 0)
    else
      OffsetRect(FButtonBounds, 0, AButtonOffset);
    CreateCellInstance(TcxSchedulerContentNavigationButtonViewInfo, FButtonBounds,
      Styles.GetBackgroundParams, UseRightToLeftAlignment, AButton);
    AButton.SetIsVertical(IsNavigationButtonsVertical);
    AButton.FOnClick := OnContentNavigationButtonClick;
    AssignResourceID(AButton, AResourceIndex);
    AButton.FKind := AKind;
    if IsNavigationButtonsVertical then
    begin
      OffsetRect(FButtonTextRect, AButtonOffset, 0);
      OffsetRect(FButtonArrowRect, AButtonOffset, 0);
    end
    else
    begin
      OffsetRect(FButtonTextRect, 0, AButtonOffset);
      OffsetRect(FButtonArrowRect, 0, AButtonOffset);
    end;
    AButton.FTextRect := FButtonTextRect;
    AButton.FArrowRect := FButtonArrowRect;
    AButton.Caption := cxGetResourceString(AButtonTexts[AKind]);
    FNavigationButtons.Add(AButton);
    AddContentNavigationIntervalItem(AResourceIndex);
  end;

var
  AColumnHeight: Integer;
begin
  if Adapter.IsPrinting then Exit;

  AColumnHeight := GetCorrectColumnHeight(AColumnRect);
  if Scheduler.OptionsView.ShowNavigationButtons and
    not AreThereEventsInVisibleInterval(AResourceIndex) and
    (AColumnPositionInResource <> cprIndefinite) and
    (AColumnHeight > ScaleFactor.Apply(ContentNavigationButtonReducedHeight)) then
  begin
    case AColumnPositionInResource of
      cprFirst: AddAppointedButton(nbkPrevious);
      cprLast: AddAppointedButton(nbkNext);
      cprSingle:
        begin
          AddAppointedButton(nbkPrevious);
          AddAppointedButton(nbkNext);
        end;
    end;
  end;
end;

function TcxSchedulerCustomResourceViewViewInfo.AddDayHeader(
  const ADate: TDateTime; const ABounds: TRect; AResourceIndex: Integer;
  ADisplayMode: TcxSchedulerDayHeaderModernStyleDisplayMode): TcxSchedulerDayHeaderCellViewInfo;
begin
  CreateCellInstance(DayHeaderClass, ABounds, StylesAdapter.GetDayHeaderParams(ADate), UseRightToLeftAlignment, Result);
  Result.DateTime := ADate;
  Result.FActualModernStyleDisplayMode := ADisplayMode;
  AssignResourceID(Result, AResourceIndex);
  FDayHeaderCells.Add(Result);
end;

function TcxSchedulerCustomResourceViewViewInfo.AddDayHeader(
  const ADate: TDateTime; ATop, ALeft, ARight: Integer; AResourceIndex: Integer;
  ADisplayMode: TcxSchedulerDayHeaderModernStyleDisplayMode): TcxSchedulerDayHeaderCellViewInfo;
begin
  Result := AddDayHeader(ADate, cxRect(ALeft, ATop, ARight, ATop + FDayHeaderHeight), AResourceIndex, ADisplayMode);
end;

function TcxSchedulerCustomResourceViewViewInfo.AddEventCell(
  AViewData: TcxSchedulerEventViewData;
  AImmediateCalculate: Boolean = True): TcxSchedulerEventCellViewInfo;
begin
  Result := CreateEventCellViewInfo(AViewData,
    Scheduler.EventOperations.Sizing and TcxSchedulerControlEventAccess(AViewData.Event).CanResize);
  if (FEventImages <> nil) and FEventImages.SupportEventImages then
  begin
    Result.Images.Init(FEventImages.GetImages);
    FEventImages.DoInitEventImages(AViewData.Event, Result.Images);
  end;
  if AImmediateCalculate then
    Result.Calculate;
  EventCells.Add(Result);
  SetResourceHasVisibleEvent(AViewData.Event);
end;

function TcxSchedulerCustomResourceViewViewInfo.AddGroupHorzSeparator(
  APos: Integer): TcxSchedulerGroupSeparatorCellViewInfo;
begin
  Result := AddGroupSeparator(cxRectSetTop(Bounds, APos, SeparatorWidth));
  if Result = nil then Exit;
  Result.Rotated := True;
  if CanCacheGroupSeparator(Result) then
    Result.Cache := ImageCacheManager.Separator;
end;

function TcxSchedulerCustomResourceViewViewInfo.AddGroupSeparator(
  const ABounds: TRect): TcxSchedulerGroupSeparatorCellViewInfo;
begin
  Result := nil;
  if not HasSeparator then Exit;
  CreateCellInstance(TcxSchedulerGroupSeparatorCellViewInfo, ABounds,
    FGroupSeparatorParams, UseRightToLeftAlignment, Result);
  GroupSeparatorCells.Add(Result);
end;

function TcxSchedulerCustomResourceViewViewInfo.AddGroupVertSeparator(
  APos: Integer): TcxSchedulerGroupSeparatorCellViewInfo;
begin
  Result := AddGroupSeparator(cxRectSetLeft(Bounds, APos, SeparatorWidth));
end;

procedure TcxSchedulerCustomResourceViewViewInfo.AddResourceBounds(
  AResourceIndex: Integer; const ABounds: TRect);
begin
  if Length(FResourceBounds) <> ResourceCount then
    SetLength(FResourceBounds, ResourceCount);
  FResourceBounds[AResourceIndex] := ABounds;
end;

function TcxSchedulerCustomResourceViewViewInfo.AddResourceHeader(
  const AIndex: Integer; const ABounds: TRect): TcxSchedulerHeaderCellViewInfo;
begin
  CreateCellInstance(TcxSchedulerDayHeaderCellViewInfo, ABounds,
    StylesAdapter.GetResourceHeaderParams(GetResourceItemByIndex(AIndex)), UseRightToLeftAlignment, Result);
  Result.FIsResourceHeader := AIndex >= 0;
  if Result.IsResourceHeader then
  begin
    Result.FDisplayText := ResourceCaptions[AIndex];
    AssignResourceID(Result, AIndex);
    with Resources[AIndex] do
    begin
      Result.FAutoHeight := ResourceHeaders.MultilineCaptions;
      Result.FImageIndex := ImageIndex;
      Result.FImagePosition := ImagePosition;
      Result.FImages := Images;
    end;
  end;
  ResourceHeaderCells.Add(Result);
  if CanCacheResourceHeader(Result) then
    Result.Cache := ImageCacheManager.ResourceHeaders.GetCacheForItem(Result);
end;

procedure TcxSchedulerCustomResourceViewViewInfo.AfterCalculate;
begin
  inherited AfterCalculate;
  if FHideClones then
    HideCloneEventsOnDragDrop;
  if FHideSource then
    HideSourceEventsOnDragDrop;
  if Scheduler.OptionsView.ShowNavigationButtons then
    SetContentNavigationButtonsIntervals;
  if UseRightToLeftAlignment then
    DoRightToLeftConversion(Bounds);
end;

function TcxSchedulerCustomResourceViewViewInfo.AreThereEventsInVisibleInterval(
  AResourceIndex: Integer): Boolean;
var
  AResourceID: Variant;
begin
  Result := (((GetGroupingKind = gkNone) or (AResourceIndex < 0))
    and (HasVisibleEvents or (GetMoreEventsButtonCount(-1) <> 0))) or
    ((GetGroupingKind <> gkNone) and (AResourceIndex >= 0) and
    ((GetEventForResourceCount(AResourceIndex, AResourceID) <> 0) or
    (GetEventWithoutResourceCount <> 0)));
end;

procedure TcxSchedulerCustomResourceViewViewInfo.AssignResourceID(
  ACell: TcxSchedulerCustomResourceViewInfoItem; AIndex: Integer);
begin
  if AIndex >= 0 then
    ACell.Resource := Resources[AIndex];
  ACell.FIsResourceAssigned := AIndex >= 0;
end;

procedure TcxSchedulerCustomResourceViewViewInfo.CalculateContentNavigationButtons;
var
  AResourceIndex: Integer;
begin
  if IsDrawContentNavigationButtons then
  begin
    if ResourceCount > 0 then
      for AResourceIndex := 0 to ResourceCount - 1 do
        AddContentNavigationButton(FResourceBounds[AResourceIndex], AResourceIndex,
          cprSingle)
    else
      AddContentNavigationButton(Bounds, -1, cprSingle);
  end;
end;

procedure TcxSchedulerCustomResourceViewViewInfo.CalculateMetrics;
begin
  FHideSelection := View.OptionsView.HideSelection and not View.Scheduler.IsFocused;
  FGroupSeparatorParams := Styles.GetGroupSeparatorParams;
  FTimeLineParams := GetTimeLineParams;
  FSelectionParams := Styles.GetSelectionParams;
  FContentLineHeight := Adapter.GetContentLineHeight;
  FContentFontHeight := cxTextHeight(StylesAdapter.GetContentParams(Now, nil).Font) + 2 * ScaleFactor.Apply(cxTextOffset);
  FDayHeaderHeight := LookAndFeelPainter.ScaledHeaderHeight(GetFontHeight(StylesAdapter.GetDayHeaderStyle), ScaleFactor);
  FResourceHeaderHeight := LookAndFeelPainter.ScaledHeaderHeight(
    GetFontHeight(StylesAdapter.GetResourceHeaderStyle), ScaleFactor);
  FUseResourceImages := HasStorage and View.Scheduler.Storage.Resources.AreImagesUsed;
  if HasStorage then
    FResourceImages := View.Scheduler.Storage.Resources.Images
  else
    FResourceImages := nil;
  if HasStorage then
  begin
    if ResourceHeaders.Height <> 0 then
      FResourceHeaderHeight := ResourceHeaders.Height;
    CalculateResourceHeadersAutoHeight(GetResourceHeaderWidth - 3 * ScaleFactor.Apply(cxTextOffset));
    if ResourceHeaders.Height <> 0 then
      FResourceHeaderHeight := ResourceHeaders.Height;
  end;
end;

procedure TcxSchedulerCustomResourceViewViewInfo.CalculateNavigationButtonParams(AColumnRect: TRect;
  AKind: TcxSchedulerContentNavigationButtonKind; out AButtonWidth: Integer);
var
  AHeight, AMiddle: Integer;
  AIsCollapsed: Boolean;
begin
  if IsNavigationButtonsVertical then
    AIsCollapsed := Round(ScaleFactor.ApplyF(ContentNavigationButtonHeight * 1.5)) >= GetCorrectColumnHeight(AColumnRect)
  else
    AIsCollapsed := Round(ScaleFactor.ApplyF(ContentNavigationButtonHeight * 1.5)) >= GetCorrectColumnHeight(cxRectRotate(AColumnRect));
  if AIsCollapsed then
    AHeight := ScaleFactor.Apply(ContentNavigationButtonReducedHeight)
  else
    AHeight := ScaleFactor.Apply(ContentNavigationButtonHeight);

  if IsNavigationButtonsVertical then
  begin
    AMiddle := (AColumnRect.Top + AColumnRect.Bottom) div 2;
    case AKind of
      nbkPrevious: FButtonBounds := Rect(AColumnRect.Left, AMiddle - AHeight div 2,
        AColumnRect.Left + ScaleFactor.Apply(ContentNavigationButtonWidth), AMiddle + AHeight div 2);
      nbkNext: FButtonBounds := Rect(AColumnRect.Right - ScaleFactor.Apply(ContentNavigationButtonWidth),
        AMiddle - AHeight div 2, AColumnRect.Right, AMiddle + AHeight div 2);
    else
      FButtonBounds := cxNullRect;
    end;
  end
  else
  begin
    AMiddle := (AColumnRect.Left + AColumnRect.Right) div 2;
    case AKind of
      nbkPrevious: FButtonBounds := Rect(AMiddle - AHeight div 2, AColumnRect.Top,
        AMiddle + AHeight div 2, AColumnRect.Top + ScaleFactor.Apply(ContentNavigationButtonWidth));
      nbkNext: FButtonBounds := Rect(AMiddle - AHeight div 2, AColumnRect.Bottom -
        ScaleFactor.Apply(ContentNavigationButtonWidth), AMiddle + AHeight div 2, AColumnRect.Bottom);
    else
      FButtonBounds := cxNullRect;
    end;
  end;

  LookAndFeelPainter.CalculateSchedulerNavigationButtonRects(
    AKind = nbkNext, AIsCollapsed, cxTextExtent(Styles.GetBackgroundParams.Font,
     cxGetResourceString(@scxPrevAppointment)),
     cxTextExtent(Styles.GetBackgroundParams.Font,
     cxGetResourceString(@scxNextAppointment)), FButtonBounds, FButtonTextRect,
    FButtonArrowRect, ScaleFactor, IsNavigationButtonsVertical);

  if IsNavigationButtonsVertical then
    AButtonWidth := FButtonBounds.Right - FButtonBounds.Left
  else
    AButtonWidth := FButtonBounds.Bottom - FButtonBounds.Top;
end;

procedure TcxSchedulerCustomResourceViewViewInfo.CalculateResourceHeadersAutoHeight(AWidth: Integer);

  function CalculateResourceHeight(AResource: TcxSchedulerStorageResourceItem; AWidth: Integer): Integer;
  var
    R: TRect;
    AFont: TFont;
    AText: string;
    AHasImage: Boolean;
  begin
    AFont := Styles.GetResourceHeaderParams(AResource).Font;
    AHasImage := UseResourceImages and (AResource.ActualImageIndex >= 0);
    if AHasImage and (ResourceHeaders.ImagePosition in [ipLeft, ipRight]) then
      AWidth := Max(0, AWidth - (GetResourceImagesSize.cx + ScaleFactor.Apply(cxTextOffset * 2)));
    R := Rect(0, 0, Max(AWidth, 2), 0);
    AText := AResource.Resources.GetResourceName(AResource);
    ScreenCanvas.Font.Assign(AFont);
    ScreenCanvas.TextExtent(AText, R,
      MakeTextOutcxFlags(taLeftJustify, vaCenter, ResourceHeaders.MultilineCaptions));
    Result := R.Bottom - R.Top + 3 * ScaleFactor.Apply(cxTextOffset);
    SetResourceTextRect(AResource, R);
    if AHasImage then
    begin
      if (ResourceHeaders.ImagePosition in [ipTop, ipBottom]) then
        Inc(Result, GetResourceImagesSize.cy + 4 * ScaleFactor.Apply(cxTextOffset))
      else
        Result := Max(Result, GetResourceImagesSize.cy + 3 * ScaleFactor.Apply(cxTextOffset));
    end;
  end;

var
  I: Integer;
begin
  AWidth := Max(0, AWidth);
  for I := 0 to Adapter.Resources.VisibleResourceCount - 1 do
    FResourceHeaderHeight := Max(FResourceHeaderHeight, CalculateResourceHeight(
      Adapter.Resources.VisibleResources[I], AWidth));
end;

function TcxSchedulerCustomResourceViewViewInfo.CalculateResourceHeaderWidth: Integer;
var
  I: Integer;
  AWidth: Integer;

  function CalculateWidth(AResource: TcxSchedulerStorageResourceItem): Integer;
  var
    R: TRect;
    AText: string;
    AHasImage: Boolean;
    AViewParams: TcxViewParams;
  begin
    AViewParams := StylesAdapter.GetResourceHeaderParams(AResource);
    Result := 0;
    AHasImage := AResource.ActualImageIndex >= 0;
    AText := AResource.Resources.GetResourceName(AResource);
    if Length(AText) > 0 then
    begin
      R := Rect(0, 0, FResourceHeaderHeight, AWidth);
      if AHasImage and (ResourceHeaders.ImagePosition in [ipTop, ipBottom]) then
        Dec(R.Bottom, GetResourceImagesSize.cy + 3 * ScaleFactor.Apply(cxTextOffset));
      ScreenCanvas.Font.Assign(AViewParams.Font);
      ScreenCanvas.TextExtent(AText, R, MakeTextOutcxFlags(taLeftJustify, vaCenter, ResourceHeaders.MultilineCaptions));
      Result := R.Right - R.Left + 3 * ScaleFactor.Apply(cxTextOffset);
      SetResourceTextRect(AResource, R);
      if AHasImage and (ResourceHeaders.ImagePosition in [ipTop, ipBottom]) then
        Result := Max(Result, ResourceImages.Width + 4 * ScaleFactor.Apply(cxTextOffset));
    end;
    if AHasImage and (ResourceHeaders.ImagePosition in [ipLeft, ipRight]) then
      Inc(Result, ResourceImages.Width + 4 * ScaleFactor.Apply(cxTextOffset));
  end;

begin
  AWidth := GetResourceHeaderWidth - 3 * ScaleFactor.Apply(cxTextOffset);
  Result := FResourceHeaderHeight;
  for I := 0 to Adapter.Resources.VisibleResourceCount - 1 do
    Result := Max(Result, CalculateWidth(Adapter.Resources.VisibleResources[I]));
end;

function TcxSchedulerCustomResourceViewViewInfo.CanCalculateNavigationButtons: Boolean;
begin
  Result := Scheduler.OptionsView.ShowNavigationButtons and not Adapter.IsPrinting;
end;

function TcxSchedulerCustomResourceViewViewInfo.CanCacheGroupSeparator(
  AItem: TcxSchedulerGroupSeparatorCellViewInfo): Boolean;
begin
  Result := AItem.Rotated and (AItem.Bitmap = nil) and not View.HasCustomDrawGroupSeparator;
end;

function TcxSchedulerCustomResourceViewViewInfo.CanCacheResourceHeader(
  AItem: TcxSchedulerHeaderCellViewInfo): Boolean;
begin
  Result := (AItem.Bitmap = nil) and not View.HasCustomDrawResourceHeader;
end;

procedure TcxSchedulerCustomResourceViewViewInfo.CheckResourceNavigatorKind;
begin
  if ResourceCount = 0 then
    ExtractResources;
  FGroupingKind := GetGroupingKind;
  inherited CheckResourceNavigatorKind;
end;

procedure TcxSchedulerCustomResourceViewViewInfo.Clear;
begin
  FResources.Clear;
  GroupSeparatorCells.Clear;
  Buttons.Clear;
  NavigationButtons.Clear;
  ContentCells.Clear;
  EventCells.Clear;
  DayHeaderCells.Clear;
  ResourceHeaderCells.Clear;
  HeaderContainerCells.Clear;
  ClearResourceBounds;
  ClearContentNavigationIntervals;
end;

procedure TcxSchedulerCustomResourceViewViewInfo.ClearResourceBounds;
var
  I: Integer;
begin
  for I := 0 to Length(FResourceBounds) - 1 do
    FResourceBounds[I] := cxNullRect;
end;

function TcxSchedulerCustomResourceViewViewInfo.ContentCellClass: TcxSchedulerContentCellViewInfoClass;
begin
  Result := TcxSchedulerContentCellViewInfo;
end;

procedure TcxSchedulerCustomResourceViewViewInfo.CreateCellInstance(
  AClass: TcxSchedulerCustomViewInfoItemClass; const ABounds: TRect;
  const AViewParams: TcxViewParams; AUseRightToLeftAlignment: Boolean; var Instance);
begin
  CreateCellInstance(AClass, ABounds, Bounds, AViewParams, AUseRightToLeftAlignment, Instance);
end;

procedure TcxSchedulerCustomResourceViewViewInfo.CreateCellInstance(
  AClass: TcxSchedulerCustomViewInfoItemClass; const ABounds,
  AVisibleBounds: TRect; const AViewParams: TcxViewParams; AUseRightToLeftAlignment: Boolean; var Instance);
begin
  Pointer(Instance) := AClass.Create(LookAndFeelPainter, ABounds, AVisibleBounds, AViewParams, ScaleFactor, AUseRightToLeftAlignment);
  TcxSchedulerCustomViewInfoItem(Instance).FDayBorderColor := FDayBorderColor;
  TcxSchedulerCustomViewInfoItem(Instance).FExternalPainter := View.ExternalPainter;
end;

function TcxSchedulerCustomResourceViewViewInfo.CreateEventViewData(
  AEvent: TcxSchedulerControlEvent; const ABounds: TRect;
  const AStart, AFinish: TDateTime; AResource: TcxSchedulerResourceViewInfo): TcxSchedulerEventViewData;
var
  ALocation: string;
begin
  Result := TcxSchedulerEventViewData.Create;
  Result.Bounds := ABounds;
  Result.VisibleRect := Bounds;
  Result.Canvas := View.Canvas;
  Result.ContentStart := DateTimeHelper.RoundTime(AStart);
  Result.ExternalPainter := View.ExternalPainter;
  Result.GetEventHint := GetEventHint;
  Result.BorderColor := View.OptionsView.EventBorderColor;
  Result.ContentFinish := DateTimeHelper.RoundTime(AFinish);
  Result.Event := AEvent;
  Result.ShowTimeAsClock := View.ShowTimeAsClock;
  Result.TimeLineParams := FTimeLineParams;
  Result.ShowFinishTime := View.ShowEndTime;
  Result.Painter := LookAndFeelPainter;
  Result.ViewParams := StylesAdapter.GetEventParams(AEvent);
  Result.EditProperties := View.GetEditProperties(AEvent);
  Result.EditStyle := View.GetEditStyle(AEvent);
  Result.Resource := AResource;
  if (AEvent <> nil) and AEvent.Shared and (AResource <> nil) then
    AEvent.LockResource(AResource.ResourceID);
  Result.Caption := DoGetEventDisplayText(AEvent);
  AEvent.UnlockResource;
  Result.ImagesLayout := View.EventImagesLayout;
  Result.DrawAsProgress := False;
  Result.TaskComplete := AEvent.TaskComplete;
  Result.TaskStatus := AEvent.TaskStatus;
  if ViewStyle = svsClassic then
  begin
    ALocation := Trim(AEvent.Location);
    if ALocation <> '' then
      Result.Caption := Result.Caption + ' (' + ALocation + ')';
  end;
  Result.LineHeight := FContentLineHeight;
end;

function TcxSchedulerCustomResourceViewViewInfo.CreateEventViewData(
  AEvent: TcxSchedulerControlEvent; const ABounds: TRect;
  const AStart, AFinish: TDateTime; AResource: TcxSchedulerResourceViewInfo;
  const ACurrentDate: TDateTime): TcxSchedulerEventViewData;
begin
  Result := CreateEventViewData(AEvent, ABounds, AStart, AFinish, AResource);
  Result.CurrentDate := ACurrentDate;
end;

function TcxSchedulerCustomResourceViewViewInfo.CreateEventCellViewInfo(
  AViewData: TcxSchedulerEventViewData; ACanResize: Boolean): TcxSchedulerEventCellViewInfo;
begin
  Result := EventCellClass.Create(AViewData, ACanResize, ScaleFactor, UseRightToLeftAlignment);
end;

function TcxSchedulerCustomResourceViewViewInfo.CreateImageCacheManager: TcxSchedulerImageCacheManager;
begin
  Result := TcxSchedulerImageCacheManager.Create;
end;

function TcxSchedulerCustomResourceViewViewInfo.DayHeaderClass: TcxSchedulerDayHeaderCellViewInfoClass;
const
  AClass: array[Boolean] of TcxSchedulerDayHeaderCellViewInfoClass =
    (TcxSchedulerDayHeaderCellViewInfo, TcxSchedulerDayHeaderCellModernViewInfo);
begin
  Result := AClass[ViewStyle = svsModern];
end;

procedure TcxSchedulerCustomResourceViewViewInfo.DoCalculate;
var
  Temp: Integer;
begin
  FDayBorderColor := View.OptionsView.DayBorderColor;
  if FDayBorderColor = clDefault then
    FDayBorderColor := clWindowText;
  FEventWithoutResourceCount := 0;
  ExtractResources;
  FGroupingKind := GetGroupingKind;
  if NeedClearResources(FGroupingKind) then
    FResources.Clear;
  if SelectedDays.Count = 0 then
    SelectedDays.Add(Date);
  with View.Controller.Navigation do
    Self.FSelResource := GetResourceViewInfoByItem(SelResource, Temp);
  if (FSelResource = nil) and (ResourceCount > 0) then
    FSelResource := Resources[0];
  CalculateMetrics;
  View.Controller.Navigation.CheckSelection;
  with View.Controller.Navigation do
  begin
    Self.FSelStart := DateTimeHelper.RoundTime(Min(SelStart, SelFinish));
    Self.FSelFinish := DateTimeHelper.RoundTime(Max(SelStart, SelFinish));
  end;
end;

procedure TcxSchedulerCustomResourceViewViewInfo.DoContentNavigationButtonClick(
  Sender: TcxSchedulerContentNavigationButtonViewInfo);
begin
  TcxCustomSchedulerAccess(Scheduler).PeriodChanged;
  TcxDateNavigatorAccess(Scheduler.DateNavigator).MakeSelectionVisible;
end;

procedure TcxSchedulerCustomResourceViewViewInfo.DoMoreEventsButtonClick(
  Sender: TcxSchedulerMoreEventsButtonViewInfo);
begin
end;

procedure TcxSchedulerCustomResourceViewViewInfo.DoRightToLeftConversion(const AClientBounds: TRect);
var
  I: Integer;
begin
  for I := 0 to FCells.Count - 1 do
    if FCells.Items[I] is TcxSchedulerViewInfoCellList then
      TcxSchedulerViewInfoCellList(FCells.Items[I]).DoRightToLeftConversion(AClientBounds);
  NavigationButtons.DoRightToLeftConversion(AClientBounds);
  Buttons.DoRightToLeftConversion(AClientBounds);
end;

procedure TcxSchedulerCustomResourceViewViewInfo.DrawNavigationButtons(ACanvas: TcxCanvas; ADrawItemProc: TcxSchedulerCustomDrawItemProc);
var
  AHorzSplitterMovingRect, AVertSplitterMovingRect, AActualBounds, R: TRect;
  AIsHorzSplitterMoving, AIsVertSplitterMoving: Boolean;
  I: Integer;
begin
  NavigationButtons.Draw(ACanvas, ADrawItemProc);

  AHorzSplitterMovingRect := TcxSchedulerSplitterControllerAccess(TcxSchedulerSplitterAccess(TcxCustomSchedulerAccess(Scheduler).HorzSplitter).Controller).PrevInvertRect;
  AVertSplitterMovingRect := TcxSchedulerSplitterControllerAccess(TcxSchedulerSplitterAccess(TcxCustomSchedulerAccess(Scheduler).VertSplitter).Controller).PrevInvertRect;
  AIsHorzSplitterMoving := not cxRectIsEmpty(AHorzSplitterMovingRect);
  AIsVertSplitterMoving := not cxRectIsEmpty(AVertSplitterMovingRect);
  if AIsHorzSplitterMoving or AIsVertSplitterMoving then
    for I := 0 to NavigationButtons.Count - 1 do
      if ACanvas.RectVisible(NavigationButtons[I].ActiveBounds) then
      begin
        AActualBounds := cxRectOffset(NavigationButtons[I].ActiveBounds, View.Bounds.TopLeft);
        if (AIsHorzSplitterMoving and IntersectRect(R, AHorzSplitterMovingRect, AActualBounds)) or
           (AIsVertSplitterMoving and IntersectRect(R, AVertSplitterMovingRect, AActualBounds)) then
          InvertRect(ACanvas.Handle, cxRectOffset(R, -View.Bounds.Left, -View.Bounds.Top));
      end;
end;

procedure TcxSchedulerCustomResourceViewViewInfo.ExtractResources;
var
  I: Integer;
begin
  Adapter.GetPageResources(FResources);
  for I := 0 to FResources.Count - 1 do
    Resources[I].FImagePosition := ResourceHeaders.ImagePosition;
end;

function TcxSchedulerCustomResourceViewViewInfo.GetBounds: TRect;
begin
  Result := FPageBounds;
end;

function TcxSchedulerCustomResourceViewViewInfo.GetContentNavigationInterval(
  AContentNavigationButton: TcxSchedulerContentNavigationButtonViewInfo): TDateTime;

  function GetContentNavigationInfo(AIndex: Integer): TcxSchedulerContentNavigationInfo;
  begin
    Result := TcxSchedulerContentNavigationInfo(FContentNavigationIntervals[AIndex]);
  end;

  function GetInterval(AIndex: Integer): TDateTime;
  begin
    if AContentNavigationButton.Kind = nbkPrevious then
      Result := - GetContentNavigationInfo(AIndex).FIntervalBefore
    else
      Result := GetContentNavigationInfo(AIndex).FIntervalAfter;
  end;

var
  I: Integer;
begin
  Result := MaxDateTime;
  if AContentNavigationButton.Resource = nil then
    Result := GetInterval(0)
  else
    for I := 0 to FContentNavigationIntervals.Count - 1 do
      if VarEqualsSoft(GetContentNavigationInfo(I).FResourceID,
        AContentNavigationButton.Resource.ResourceID) then
      begin
        Result := GetInterval(I);
        Break;
      end;
end;

function TcxSchedulerCustomResourceViewViewInfo.GetContentParams(
  const ATime: TDateTime; AResource: TcxSchedulerResourceViewInfo): TcxViewParams;
begin
  if AResource = nil then
    Result := StylesAdapter.GetContentParams(ATime, nil)
  else
    Result := StylesAdapter.GetContentParams(ATime, AResource.ResourceItem)
end;

function TcxSchedulerCustomResourceViewViewInfo.GetDayHeaderHeight: Integer;
begin
  Result := FDayHeaderHeight;
end;

function TcxSchedulerCustomResourceViewViewInfo.EventCellClass: TcxSchedulerEventCellViewInfoClass;
begin
  Result := TcxSchedulerEventCellViewInfo;
end;

function TcxSchedulerCustomResourceViewViewInfo.GetEventForResourceCount(
  AResourceIndex: Integer; out AResourceID: Variant): Integer;
begin
  Result := Resources[AResourceIndex].VisibleEventCount +
    GetMoreEventsButtonCount(AResourceIndex);
  AResourceID := Resources[AResourceIndex].ResourceID;
end;

function TcxSchedulerCustomResourceViewViewInfo.GetEventWithoutResourceCount: Integer;
begin
  Result := FEventWithoutResourceCount;
end;

function TcxSchedulerCustomResourceViewViewInfo.GetGroupingKind: TcxSchedulerGroupingKind;
begin
  Result := View.GroupingKind;
  if Result = gkDefault then
    Result := View.OptionsView.GroupingKind;
  if ResourceCount = 0 then
    Result := gkNone
  else
    if Result = gkDefault then
      Result := gkByResource;
end;

function TcxSchedulerCustomResourceViewViewInfo.GetMoreEventButtonClass: TcxSchedulerMoreEventsButtonViewInfoClass;
const
  AResult: array[Boolean] of TcxSchedulerMoreEventsButtonViewInfoClass =
    (TcxSchedulerMoreEventsButtonViewInfo, TcxSchedulerMoreEventsModernButtonViewInfo);
begin
  Result := AResult[ViewStyle = svsModern];
end;

function TcxSchedulerCustomResourceViewViewInfo.GetResourceHeaderWidth: Integer;
begin
  Result := GetResourcesContentWidth;
  if ResourceCount > 0 then
    Result := Max(0, (Result - SeparatorWidth * GetSeparatorCount) div ResourceCount);
end;

function TcxSchedulerCustomResourceViewViewInfo.GetResourceImagesSize: TSize;
begin
  if (ResourceImages = nil) or (ResourceImages.Count = 0) then
    Result := TSize(cxNullSize)
  else
    Result := dxGetImageSize(nil, ResourceImages, 0, ScaleFactor);
end;

function TcxSchedulerCustomResourceViewViewInfo.GetResourcesContentWidth: Integer;
begin
  Result := FBounds.Bottom - FBounds.Top - FDayHeaderHeight;
end;

function TcxSchedulerCustomResourceViewViewInfo.GetScaleUnit: TDateTime;
begin
  Result := 0;
end;

function TcxSchedulerCustomResourceViewViewInfo.GetShowEventsWithoutResource: Boolean;
begin
  Result := View.OptionsView.ShowEventsWithoutResource or (FGroupingKind = gkNone);
end;

function TcxSchedulerCustomResourceViewViewInfo.GetTimeLineParams: TcxViewParams;
begin
  Result := StylesAdapter.GetContentParams(NullDate, nil)
end;

function TcxSchedulerCustomResourceViewViewInfo.GetSelectionParams(
  const AParams: TcxViewParams): TcxViewParams;
begin
  Result := AParams;
  if View.Scheduler.Focused or not HideSelection then
  begin
    Result := FSelectionParams;
    Result.Bitmap := nil;
  end;
end;

function TcxSchedulerCustomResourceViewViewInfo.GetSeparatorCount: Integer;
begin
  Result := ResourceCount - 1;
end;

function TcxSchedulerCustomResourceViewViewInfo.GetActualFont(AStyle: TcxStyle): TFont;
begin
  if (AStyle = nil) or not (cxStyles.svFont in AStyle.AssignedValues) then
    Result := DefaultFont
  else
    Result := AStyle.Font;
end;

function TcxSchedulerCustomResourceViewViewInfo.GetFontHeight(AStyle: TcxStyle): Integer;
begin
  Result := PainterHelper.TextHeight(GetActualFont(AStyle));
end;

function TcxSchedulerCustomResourceViewViewInfo.GetFontHeight(
  const AParams: TcxViewParams): Integer;
begin
  Result := PainterHelper.TextHeight(AParams.Font);
end;

function TcxSchedulerCustomResourceViewViewInfo.GetPageHeaderText: string;
begin
  Result := FormatDateTime(dxFormatSettings.LongDateFormat, Days[0]);
  if Days[0] <> Days[DayCount - 1] then
    Result := Result + ' - ' + FormatDateTime(dxFormatSettings.LongDateFormat, Days[DayCount - 1]);
end;

function TcxSchedulerCustomResourceViewViewInfo.GetResourceItemByIndex(
  AIndex: Integer): TcxSchedulerStorageResourceItem;
begin
  Result := nil;
  if AIndex >= 0 then
    Result := Resources[AIndex].ResourceItem;
end;

function TcxSchedulerCustomResourceViewViewInfo.GetResourceViewInfoByItem(
  AItem: TcxSchedulerStorageResourceItem;
  var ResourceViewInfoIndex: Integer): TcxSchedulerResourceViewInfo;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to ResourceCount - 1 do
    if Resources[I].ResourceItem = AItem then
    begin
      Result := Resources[I];
      ResourceViewInfoIndex := I;
    end;
end;

function TcxSchedulerCustomResourceViewViewInfo.GetStartDate(
  Index: Integer): TDateTime;
begin
  Result := SelectedDays[0];
end;

function TcxSchedulerCustomResourceViewViewInfo.HasClone(AEvent: TcxSchedulerEvent): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Events.Clones.Count - 1 do
  begin
    Result := TcxSchedulerControlEvent(Events.Clones[I]).Source = AEvent;
    if Result then
      Break;
  end;
end;

function TcxSchedulerCustomResourceViewViewInfo.HasStorage: Boolean;
begin
  Result := View.Scheduler.Storage <> nil;
end;

function TcxSchedulerCustomResourceViewViewInfo.HasVisibleEvents: Boolean;
begin
  Result := EventCells.Count <> 0;
end;

procedure TcxSchedulerCustomResourceViewViewInfo.ProcessCheckBorders(
  AList: TcxSchedulerViewInfoCellList; AHasCommonArea: Boolean;
  ANeighbors: TcxNeighbors = []; AExcludeBorders: TcxBorders = [];
  AAddBorders: TcxBorders = []);
var
  I: Integer;
  DrawCommonBorders: Boolean;
  ACell, APrevCell: TcxSchedulerHeaderCellViewInfo;
begin
  DrawCommonBorders :=
    LookAndFeelPainter.HeaderBorders([nLeft..nBottom]) = cxBordersAll;
  APrevCell := nil;
  for I := 0 to AList.Count - 1 do
  begin
    ACell := TcxSchedulerHeaderCellViewInfo(AList[I]);
    ACell.FNeighbors := [nLeft, nRight];
    if (I = 0) and not ACell.RotateHeader then
    begin
      Exclude(ACell.FNeighbors, nLeft);
      if AHasCommonArea then
        Include(ACell.FNeighbors, nBottom);
    end;
    ACell.FNeighbors := ACell.FNeighbors + ANeighbors;
    ACell.FBorders := LookAndFeelPainter.HeaderBorders(ACell.FNeighbors);
    if not DrawCommonBorders then
    begin
      ACell.CheckNeighbor(APrevCell);
      ACell.FBorders := ACell.FBorders - AExcludeBorders + AAddBorders;
    end;
    if I = (AList.Count - 1) then
    begin
      Exclude(ACell.FBorders, bRight);
      Exclude(ACell.FNeighbors, nRight);
    end;
    APrevCell := ACell;
  end;
end;

procedure TcxSchedulerCustomResourceViewViewInfo.ReturnVisibleInterval(
  var AStart, AEnd: TDateTime);

   function IsDiscreteSelection: Boolean;
   var
     I: Integer;
   begin
     Result := False;
     I := 0;
     while not Result and (I < SelectedDays.Count - 1) do
     begin
       Result := SelectedDays[I] + 1 <> SelectedDays[I + 1];
       Inc(I);
     end;
   end;

begin
  AStart := StartDates[0];
  if IsDiscreteSelection then
    AEnd := AStart
  else
    AEnd := AStart + SelectedDays.Count - 1;
end;

function TcxSchedulerCustomResourceViewViewInfo.SetAdapter(
  Adapter: TcxCustomResourceViewAdapter): TcxCustomResourceViewAdapter;
begin
  Result := FAdapter;
  FAdapter := Adapter;
  FStylesAdapter := FAdapter.GetStylesAdapter;
end;

procedure TcxSchedulerCustomResourceViewViewInfo.SetResourceHasVisibleEvent(
  AEvent: TcxSchedulerControlEvent);
var
  AResourceIndex: Integer;
  AResourceFound: Boolean;
begin
  AResourceFound := False;
  for AResourceIndex := 0 to ResourceCount - 1 do
    if AEvent.IsSharedWithResource(Resources[AResourceIndex].ResourceID) then
    begin
      Resources[AResourceIndex].VisibleEventCount :=
        Resources[AResourceIndex].VisibleEventCount + 1;
      AResourceFound := True;
    end;
  if not AResourceFound then
    Inc(FEventWithoutResourceCount);
end;

procedure TcxSchedulerCustomResourceViewViewInfo.SetResourceTextRect(
  AResource: TcxSchedulerStorageResourceItem; const ARect: TRect);
var
  I: Integer;
begin
  for I := 0 to ResourceCount - 1 do
    if Resources[I].ResourceItem = AResource then
    begin
      Resources[I].FTextRect := ARect;
      Break;
    end;
end;

function TcxSchedulerCustomResourceViewViewInfo.UseRightToLeftAlignment: Boolean;
begin
  Result := inherited UseRightToLeftAlignment and not Adapter.IsPrinting;
end;

procedure TcxSchedulerCustomResourceViewViewInfo.ProcessDateToDisplayText(
  ArrangeByType: Boolean = False);
var
  I: Integer;
  AType: Integer;
begin
  AType := 0;
  for I := 0 to FDayHeaderCells.Count - 1 do
    AType := Max(TcxSchedulerDayHeaderCellViewInfo(
      FDayHeaderCells[I]).ConvertDateToDisplayText, AType);
  if ArrangeByType and (AType > 0) then
  begin
    for I := 0 to FDayHeaderCells.Count - 1 do
      TcxSchedulerDayHeaderCellViewInfo(
         FDayHeaderCells[I]).ConvertDateToDisplayText(AType);
  end;
end;

procedure TcxSchedulerCustomResourceViewViewInfo.HideCloneEventsOnDragDrop;
var
  I: Integer;
  AEventCell: TcxSchedulerEventCellViewInfo;
begin
  for I := 0 to EventCells.Count - 1 do
  begin
    AEventCell := EventCells[I];
    if Events.Clones.IndexOf(AEventCell.Event) <> -1 then
      AEventCell.Hidden := True;
  end;
end;

procedure TcxSchedulerCustomResourceViewViewInfo.HideSourceEventsOnDragDrop;
var
  I: Integer;
  AEventCell: TcxSchedulerEventCellViewInfo;
begin
  for I := 0 to EventCells.Count - 1 do
  begin
    AEventCell := EventCells[I];
    AEventCell.Hidden := HasClone(AEventCell.Event);
  end;
end;

procedure TcxSchedulerCustomResourceViewViewInfo.HotTrackMoreEventsButton(AHitTest: TcxSchedulerCustomResourceViewHitTest);
var
  AMoreEventsButton: TcxSchedulerMoreEventsButtonViewInfo;
begin
  if Scheduler.OptionsBehavior.HotTrack then
    AMoreEventsButton := AHitTest.HitMoreEventsButton
  else
    AMoreEventsButton := nil;
  SetHitMoreEventsButton(AMoreEventsButton, AHitTest);
end;

procedure TcxSchedulerCustomResourceViewViewInfo.HotTrackNavigationButtons(
  AHitTest: TcxSchedulerCustomResourceViewHitTest);
var
  ANavigationButton: TcxSchedulerContentNavigationButtonViewInfo;
begin
  if not CanCalculateNavigationButtons then Exit;
  if Scheduler.OptionsBehavior.HotTrack then
    ANavigationButton := AHitTest.HitContentNavigationButton
  else
    ANavigationButton := nil;
  SetHitContentNavigationButton(ANavigationButton, AHitTest);
end;

function TcxSchedulerCustomResourceViewViewInfo.IsNavigationButtonsVertical: Boolean;
begin
  Result := True;
end;

function TcxSchedulerCustomResourceViewViewInfo.IsTimeSelected(
  ATime: TDateTime; AResource: TObject): Boolean;
begin
  ATime := DateTimeHelper.RoundTime(ATime);
  Result := not HideSelection and ((AResource = FSelResource) and
    (ATime >= SelStart) and (ATime <= SelFinish));
  Result := Result and CanSelected;
end;

function TcxSchedulerCustomResourceViewViewInfo.IsValidNavigationButtonsPlace(
  const AResourceRect: TRect): Boolean;
var
  AButtonWidth1, AButtonWidth2: Integer;
begin
  CalculateNavigationButtonParams(AResourceRect, nbkPrevious, AButtonWidth1);
  CalculateNavigationButtonParams(AResourceRect, nbkNext, AButtonWidth2);
  Result := AButtonWidth1 + AButtonWidth2 <= AResourceRect.Right - AResourceRect.Left;
end;

procedure TcxSchedulerCustomResourceViewViewInfo.MakeTimeVisible(
  const ATime: TDateTime);
begin
end;

function TcxSchedulerCustomResourceViewViewInfo.NavigationButtonOffset(
  AKind: TcxSchedulerContentNavigationButtonKind;
  AResourceIndex: Integer): Integer;
begin
  Result := 0;
end;

function TcxSchedulerCustomResourceViewViewInfo.NeedClearResources(AGroupingKind: TcxSchedulerGroupingKind): Boolean;
begin
  Result := FGroupingKind = gkNone;
end;

procedure TcxSchedulerCustomResourceViewViewInfo.OnContentNavigationButtonClick(
  Sender: TcxSchedulerContentNavigationButtonViewInfo);
var
  AResource: TcxSchedulerStorageResourceItem;
begin
  if Sender.Resource <> nil then
    AResource := Sender.Resource.ResourceItem
  else
    AResource := nil;
  if not DoSchedulerNavigationButtonClick(Sender.Interval, AResource) then
    DoContentNavigationButtonClick(Sender);
end;

procedure TcxSchedulerCustomResourceViewViewInfo.OnMoreEventsButtonClick(
  Sender: TcxSchedulerMoreEventsButtonViewInfo);
begin
  if not DoSchedulerMoreEventsButtonClick then
    DoMoreEventsButtonClick(Sender);
end;

procedure TcxSchedulerCustomResourceViewViewInfo.AddContentNavigationIntervalItem(
  AResourceIndex: Variant);
var
  AResourceID: Variant;
  I: Integer;
  AnAlreadyExists: Boolean;
begin
  if AResourceIndex > -1 then
    AResourceID := Resources[AResourceIndex].ResourceID
  else
  begin
    AResourceID := 0;
    FContentNavigationWithoutResources := True;
  end;
  AnAlreadyExists := False;
  I := 0;
  while not AnAlreadyExists and (I < FContentNavigationIntervals.Count) do
  begin
    AnAlreadyExists := VarEqualsSoft(TcxSchedulerContentNavigationInfo(
      FContentNavigationIntervals[I]).FResourceID, AResourceID);
    Inc(I);
  end;
  if not AnAlreadyExists then
    FContentNavigationIntervals.Add(TcxSchedulerContentNavigationInfo.Create(AResourceID));
end;

procedure TcxSchedulerCustomResourceViewViewInfo.ClearContentNavigationIntervals;
begin
  if FContentNavigationIntervals = nil then
    FContentNavigationIntervals := TObjectList.Create
  else
    FContentNavigationIntervals.Clear;
  FContentNavigationWithoutResources := False;
end;

function TcxSchedulerCustomResourceViewViewInfo.GetClones: TcxSchedulerEventList;
begin
  Result := View.EventList.Clones;
end;

function TcxSchedulerCustomResourceViewViewInfo.GetCorrectColumnHeight(
  AColumnRect: TRect): Integer;
begin
  Result := Min(AColumnRect.Bottom - AColumnRect.Top, GetMinResourceHeight);
end;

function TcxSchedulerCustomResourceViewViewInfo.GetDay(AIndex: Integer): TDateTime;
begin
  Result := Integer(SelectedDays.List[AIndex]);
end;

function TcxSchedulerCustomResourceViewViewInfo.GetDayCount: Integer;
begin
  Result := SelectedDays.Count;
end;

function TcxSchedulerCustomResourceViewViewInfo.GetGroupByDate: Boolean;
begin
  Result := GroupingKind = gkByDate;
end;

function TcxSchedulerCustomResourceViewViewInfo.GetGroupByResource: Boolean;
begin
  Result := GroupingKind = gkByResource;
end;

function TcxSchedulerCustomResourceViewViewInfo.GetHasSeparator: Boolean;
begin
  Result := (SeparatorWidth > 0) and (GroupingKind <> gkNone);
end;

function TcxSchedulerCustomResourceViewViewInfo.GetHasVisibleBounds: Boolean;
begin
  Result := (PrintFrom <> NullDate) and (PrintTo <> NullDate)
end;

function TcxSchedulerCustomResourceViewViewInfo.GetHiddenSelection: Boolean;
begin
  Result := View.Controller.IsEditing or (Events.Selection.Count > 0);
end;

function TcxSchedulerCustomResourceViewViewInfo.GetMinResourceHeight: Integer;
var
  I, AResourceHeight: Integer;
begin
  Result := MaxInt;
  if Length(FResourceBounds) = ResourceCount then
    for I := 0 to ResourceCount - 1 do
    begin
      AResourceHeight := FResourceBounds[I].Bottom - FResourceBounds[I].Top;
      if (AResourceHeight <> 0) and (Result > AResourceHeight) then
        Result := AResourceHeight;
    end;
end;

function TcxSchedulerCustomResourceViewViewInfo.GetMoreEventsButtonCount(
  AResourceIndex: Integer): Integer;

  function IsButtonWithoutResources(
    AButton: TcxSchedulerMoreEventsButtonViewInfo): Boolean;
  var
    I: Integer;
  begin
    Result := True;
    I := 0;
    while (I < ResourceCount) and Result do
    begin
      if AButton.Event.IsSharedWithResource(Resources[I].ResourceID) then
        Result := False;
      Inc(I);
    end;
  end;

var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Buttons.Count - 1 do
    if (Buttons[I] is TcxSchedulerMoreEventsButtonViewInfo) and
      ((AResourceIndex = -1) or
      TcxSchedulerMoreEventsButtonViewInfo(Buttons[I]).Event.IsSharedWithResource(
      Resources[AResourceIndex].ResourceID) or
      IsButtonWithoutResources(TcxSchedulerMoreEventsButtonViewInfo(Buttons[I])))
      then
      Inc(Result);
end;

function TcxSchedulerCustomResourceViewViewInfo.GetResource(
  AIndex: Integer): TcxSchedulerResourceViewInfo;
begin
  if AIndex < 0 then
    Result := nil
  else
    Result := TcxSchedulerResourceViewInfo(FResources[AIndex]);
end;

function TcxSchedulerCustomResourceViewViewInfo.GetResourceCaption(
  AIndex: Integer): string;
begin
  Result := Resources[AIndex].Caption;
end;

function TcxSchedulerCustomResourceViewViewInfo.GetResourceCount: Integer;
begin
  Result := FResources.Count;
end;

function TcxSchedulerCustomResourceViewViewInfo.GetResourceHeaders: TcxSchedulerResourceHeaders;
begin
  Result := View.OptionsView.ResourceHeaders;
end;

function TcxSchedulerCustomResourceViewViewInfo.GetPrintRange(
  Index: Integer): TDateTime;
begin
  Result := Adapter.GetPrintRange(Index)
end;

function TcxSchedulerCustomResourceViewViewInfo.GetSeparatorWidth: Integer;
begin
  Result := View.OptionsView.GroupSeparatorWidth;
end;

function TcxSchedulerCustomResourceViewViewInfo.GetView: TcxSchedulerCustomResourceView;
begin
  Result := TcxSchedulerCustomResourceView(Owner);
end;

function TcxSchedulerCustomResourceViewViewInfo.IsDrawContentNavigationButtons: Boolean;
var
  I: Integer;
begin
  Result := CanCalculateNavigationButtons;
  if Result then
  begin
    I := 0;
    if ResourceCount > 0 then
      while (I < ResourceCount) and Result do
      begin
        Result := IsValidNavigationButtonsPlace(FResourceBounds[I]);
        Inc(I);
      end
    else
      Result := IsValidNavigationButtonsPlace(Bounds);
  end;
end;

procedure TcxSchedulerCustomResourceViewViewInfo.SetContentNavigationButtonsIntervals;

   procedure FillContentNavigationIntervals;
   var
     ACalculator: TcxSchedulerContentNavigationCalculator;
     AStart, AFinish: TDateTime;
   begin
     ReturnVisibleInterval(AStart, AFinish);
     ACalculator := TcxSchedulerContentNavigationCalculator.Create;
     try
       ACalculator.FindNavigationIntervals(Scheduler.Storage,
         FContentNavigationIntervals, AStart, AFinish,
         FContentNavigationWithoutResources, ShowEventsWithoutResource, GetScaleUnit);
     finally
       ACalculator.Free;
     end;
   end;

var
  I: Integer;
  AContentNavigationButton: TcxSchedulerContentNavigationButtonViewInfo;
begin
  FillContentNavigationIntervals;
  for I := 0 to FNavigationButtons.Count - 1 do
  begin
    AContentNavigationButton := TcxSchedulerContentNavigationButtonViewInfo(FNavigationButtons[I]);
    AContentNavigationButton.Interval := GetContentNavigationInterval(AContentNavigationButton);
  end;
end;

procedure TcxSchedulerCustomResourceViewViewInfo.SetHitMoreEventsButton(AButton: TcxSchedulerMoreEventsButtonViewInfo;
  AHitTest: TcxSchedulerCustomResourceViewHitTest);
begin
  HotMoreEventsButton := AButton;
end;

procedure TcxSchedulerCustomResourceViewViewInfo.SetHitContentNavigationButton(
  AButton: TcxSchedulerContentNavigationButtonViewInfo;
  AHitTest: TcxSchedulerCustomResourceViewHitTest);
begin
  HotContentNavigationButton := AButton;
end;

procedure TcxSchedulerCustomResourceViewViewInfo.SetHotContentNavigationButton(
  AButton: TcxSchedulerContentNavigationButtonViewInfo);
begin
  if FHotContentNavigationButton <> AButton then
  begin
    if FHotContentNavigationButton <> nil then
    begin
      EndMouseTracking(Self);
      View.RepaintRect(FHotContentNavigationButton.Bounds);
    end;
    FHotContentNavigationButton := AButton;
    if FHotContentNavigationButton <> nil then
    begin
      View.RepaintRect(FHotContentNavigationButton.Bounds);
      BeginMouseTracking(Scheduler,
        cxRectOffset(FHotContentNavigationButton.Bounds, View.Left, View.Top),
        Self);
    end;
  end;
end;

procedure TcxSchedulerCustomResourceViewViewInfo.SetHotMoreEventsButton(AButton: TcxSchedulerMoreEventsButtonViewInfo);
begin
  if FHotMoreEventsButton <> AButton then
  begin
    if FHotMoreEventsButton <> nil then
    begin
      EndMouseTracking(Self);
      View.RepaintRect(FHotMoreEventsButton.Bounds);
    end;
    FHotMoreEventsButton := AButton;
    if FHotMoreEventsButton <> nil then
    begin
      View.RepaintRect(FHotMoreEventsButton.Bounds);
      BeginMouseTracking(Scheduler, cxRectOffset(FHotMoreEventsButton.Bounds, View.Left, View.Top), Self);
    end;
  end;
end;

{ TcxSchedulerCustomResourceViewNavigation }

procedure TcxSchedulerCustomResourceViewNavigation.ValidateSelection(
  var ASelStart, ASelFinish: TDateTime;
  var AResource: TcxSchedulerStorageResourceItem);
begin
  AResource := ResourceItem;
end;

function TcxSchedulerCustomResourceViewNavigation.GetNextResource(
  AGoToForward: Boolean): TcxSchedulerStorageResourceItem;
var
  AIndex: Integer;
begin
  Result := ResourceItem;
  AIndex := ViewInfo.FResources.IndexOf(ResourceObject) + Byte(AGoToForward) * 2 - 1;
  if (AIndex < 0) or (AIndex >= ViewInfo.FResources.Count) then Exit;
  Result := ViewInfo.GetResourceItemByIndex(AIndex);
end;

function TcxSchedulerCustomResourceViewNavigation.GetResourceItem: TcxSchedulerStorageResourceItem;
begin
  Result := nil;
  if ResourceObject <> nil then
    Result := ResourceObject.ResourceItem;
end;

function TcxSchedulerCustomResourceViewNavigation.GetResourceFromViewInfo(
  AInfo: TcxSchedulerResourceViewInfo): TcxSchedulerStorageResourceItem;
begin
  Result := nil;
  if AInfo <> nil then
    Result := AInfo.ResourceItem;
end;

function TcxSchedulerCustomResourceViewNavigation.RoundTime(
  const ADateTime: TDateTime): TDateTime;
begin
  Result := DateTimeHelper.RoundTime(ADateTime);
end;

function TcxSchedulerCustomResourceViewNavigation.GetIsFirstResource: Boolean;
begin
  Result := ViewInfo.FResources.IndexOf(ResourceObject) <= 0;
end;

function TcxSchedulerCustomResourceViewNavigation.GetIsLastResource: Boolean;
begin
  Result := ViewInfo.FResources.IndexOf(ResourceObject) =
    (ViewInfo.FResources.Count - 1);
end;

function TcxSchedulerCustomResourceViewNavigation.GetGroupingKind: TcxSchedulerGroupingKind;
begin
  Result := ViewInfo.FGroupingKind;
end;

function TcxSchedulerCustomResourceViewNavigation.GetResourceObject: TcxSchedulerResourceViewInfo;
begin
  Result := ViewInfo.FSelResource;
end;

function TcxSchedulerCustomResourceViewNavigation.GetViewInfo: TcxSchedulerCustomResourceViewViewInfo;
begin
  Result := TcxSchedulerCustomResourceView(View).ViewInfo;
end;

{ TcxSchedulerCustomResourceViewController }

function TcxSchedulerCustomResourceViewController.CreateNavigation: TcxSchedulerViewNavigation;
begin
  Result := TcxSchedulerCustomResourceViewNavigation.Create(View);
end;

function TcxSchedulerCustomResourceViewController.GetCursor(X, Y: Integer): TCursor;
const
  Cursors: array[Boolean] of TCursor = (crSchedulerVertResize, crSchedulerHorzResize);
begin
  with HitTest do
  begin
    HitPoint := Point(X, Y);
    case GetDragKind of
      edkEventDragRect:
        Result := crSchedulerSplitAll;
      edkResizeStart, edkResizeEnd:
        Result := Cursors[EventCell.IsHorzSizing];
    else
      Result := inherited GetCursor(X, Y);
    end;
  end;
end;

procedure TcxSchedulerCustomResourceViewController.MouseDown(
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  NavigationButtonClicked := HitTest.HitAtContentNavigationButton;
  if NavigationButtonClicked then
    NavigationButtonDownCustomProcessing;
  if HitTest.HitAtButton then
    Exclude(Shift, ssDouble);
  inherited MouseDown(Button, Shift, X, Y);
  if HitTest.HitAtButton then
    HitTest.Button.Click;
  InternalHideHint;
end;

procedure TcxSchedulerCustomResourceViewController.MouseLeave;
begin
  inherited MouseLeave;
  InternalHideHint;
  NavigationButtonClicked := False;
end;

procedure TcxSchedulerCustomResourceViewController.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if not NavigationButtonClicked then
    inherited MouseMove(Shift, X, Y);
  ShowHint;
end;

procedure TcxSchedulerCustomResourceViewController.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if NavigationButtonClicked and HitTest.HitAtContentNavigationButton and (Button = mbLeft) then
    HitTest.ContentNavigationButton.Click;
  InternalHideHint;
  NavigationButtonClicked := False;
end;

procedure TcxSchedulerCustomResourceViewController.NavigationButtonDownCustomProcessing;
begin
//
end;

function TcxSchedulerCustomResourceViewController.GetDayHeaderDate: TDateTime;
begin
  Result := HitTest.Time;
end;

procedure TcxSchedulerCustomResourceViewController.InternalHideHint;
begin
  HintController.Hide;
  FCalculatedHintBounds := False;
end;

procedure TcxSchedulerCustomResourceViewController.InternalShowHint(P: TPoint; const AHintText: string);
const
  AFlags: array[Boolean] of Integer = (cxAlignRight, cxAlignLeft);
var
  R: TRect;
begin
  if (not FCalculatedHintBounds) then
  begin
    FHintText := AHintText;
    R := HintController.CalcHintRect(cxscMinHintWidth, AHintText, AFlags[View.UseRightToLeftAlignment]);
    OffsetRect(R, P.X, P.Y + cxGetCursorSize.cy);
    if View.UseRightToLeftAlignment then
      OffsetRect(R, -cxRectWidth(R), 0);
    HintController.Activate(R, AHintText, True, False);
    FCalculatedHintBounds := True;
  end;
end;

function TcxSchedulerCustomResourceViewController.NeedShowDayHeaderHint: Boolean;
var
  AGroupingKind: TcxSchedulerGroupingKind;
  AGroupByDate: Boolean;
begin
  AGroupingKind := View.GetGroupingKind;
  AGroupByDate := AGroupingKind in [gkByDate, gkDefault];
  Result := Scheduler.OptionsView.ShowHints and HitTest.HitAtDayHeader and
    (HitTest.HitAtTime) and ((AGroupByDate and
    not HitTest.HitAtResourceHeader) or not AGroupByDate);
end;

function TcxSchedulerCustomResourceViewController.ShowHint: Boolean;
begin
  Result := NeedShowDayHeaderHint;
  if Result then
    ShowDayHeaderHint
  else
  begin
    Result := HitTest.NeedShowHint;
    if not Result then
      InternalHideHint;
  end;
end;

procedure TcxSchedulerCustomResourceViewController.ShowDayHeaderHint;
var
  AHintText: string;
  ADate: TDateTime;
begin
  AHintText := '';
  ADate := GetDayHeaderDate;
  if TcxCustomSchedulerAccess(Scheduler).DoShowDateHint(ADate, AHintText) then
  begin
    if AnsiCompareText(AHintText, FHintText) <> 0 then
      InternalHideHint;
    InternalShowHint(GetMouseCursorPos, AHintText);
  end
  else
    InternalHideHint;
end;

function TcxSchedulerCustomResourceViewController.GetHintController: TcxSchedulerHintController;
begin
  Result := TcxCustomSchedulerAccess(Scheduler).HintController;
end;

function TcxSchedulerCustomResourceViewController.GetHitTest: TcxSchedulerCustomResourceViewHitTest;
begin
  Result := TcxSchedulerCustomResourceViewHitTest(inherited HitTest);
end;

function TcxSchedulerCustomResourceViewController.GetView: TcxSchedulerCustomResourceView;
begin
  Result := TcxSchedulerCustomResourceView(inherited View);
end;

{ TcxSchedulerCustomResourceViewHitTest }

function TcxSchedulerCustomResourceViewHitTest.GetDragKind: TcxEventDragKind;
begin
  ValidateDragKind;
  Result := FDragKind;
end;

procedure TcxSchedulerCustomResourceViewHitTest.SetDragKind(
  AValue: TcxEventDragKind);
begin
  FDragKind := AValue;
  ValidateDragKind;
end;

function TcxSchedulerCustomResourceViewHitTest.CanMoveEvent(
  AEvent: TcxSchedulerControlEvent): Boolean;
begin
  Result := Scheduler.EventOperations.Moving and TcxSchedulerControlEventAccess(AEvent).CanMove;
end;

function TcxSchedulerCustomResourceViewHitTest.CanResizeEvent(
  AEvent: TcxSchedulerControlEvent): Boolean;
begin
  Result := Scheduler.EventOperations.Sizing and TcxSchedulerControlEventAccess(AEvent).CanResize;
end;

procedure TcxSchedulerCustomResourceViewHitTest.Clear;
begin
  FContentNavigationButton := nil;
  FDragKind := edkNone;
  FEventCell := nil;
  FResource := nil;
  FHitObject := nil;
  inherited Clear;
end;

procedure TcxSchedulerCustomResourceViewHitTest.DoCalculate;
begin
  inherited DoCalculate;
  if HitAtControl then
    View.ViewInfo.CalculateHitTest(Self);
end;

function TcxSchedulerCustomResourceViewHitTest.GetHitEvent: TcxSchedulerControlEvent;
begin
  Result := inherited GetHitEvent;
  if FEventCell <> nil then
    Result := FEventCell.Event;
end;

procedure TcxSchedulerCustomResourceViewHitTest.SetHitTime(
  AItemFlag: Integer; const ATime: TDateTime);
begin
  SetBitState(AItemFlag, True);
  SetBitState(htcTime, True);
  FTime := ATime;
end;

procedure TcxSchedulerCustomResourceViewHitTest.SetResource(
  AResource: TcxSchedulerResourceViewInfo);
begin
  if AResource = nil then
    FResource := nil
  else
    FResource := AResource.ResourceItem;
end;

procedure TcxSchedulerCustomResourceViewHitTest.ValidateDragKind;
begin
  if (Event = nil) or ((FDragKind in [edkEventDragRect, edkMoveEvent]) and not CanMoveEvent(Event)) or
     ((FDragKind in [edkResizeStart, edkResizeEnd]) and not CanResizeEvent(Event)) then
    FDragKind := edkNone;
end;

function TcxSchedulerCustomResourceViewHitTest.GetContentCell: TcxSchedulerContentCellViewInfo;
begin
  if HitAtContent then
    Result := TcxSchedulerContentCellViewInfo(FHitObject)
  else
    Result := nil;
end;

function TcxSchedulerCustomResourceViewHitTest.GetHitContentNavigationButton: TcxSchedulerContentNavigationButtonViewInfo;
begin
  if GetBitState(htcNavigationButton) then
    Result := FContentNavigationButton
  else
    Result := nil;
end;

function TcxSchedulerCustomResourceViewHitTest.GetHitMoreEventsButton: TcxSchedulerMoreEventsButtonViewInfo;
begin
  if HitAtButton then
    Result := FButton
  else
    Result := nil;
end;

function TcxSchedulerCustomResourceViewHitTest.GetHeaderCell: TcxSchedulerHeaderCellViewInfo;
begin
  if HitAtDayHeader or HitAtResourceHeader then
    Result := TcxSchedulerHeaderCellViewInfo(FHitObject)
  else
    Result := nil;
end;

function TcxSchedulerCustomResourceViewHitTest.GetView: TcxSchedulerCustomResourceView;
begin
  Result := TcxSchedulerCustomResourceView(inherited Owner);
end;

{ TcxSchedulerCustomViewPainter }

constructor TcxSchedulerCustomViewPainter.Create(
  AOwner: TcxSchedulerSubControl);
begin
  inherited Create(AOwner);
  FPainter := View.GetCommonViewItemsPainter;
end;

procedure TcxSchedulerCustomViewPainter.InitializePainter;
begin
  FPainter := View.GetCommonViewItemsPainter;
end;

procedure TcxSchedulerCustomViewPainter.Paint;
begin
end;

procedure TcxSchedulerCustomViewPainter.DrawBackgroundCell(
  AItem: TcxSchedulerBackgroundCellViewInfo);
var
  ADone: Boolean;
begin
  if AItem = nil then Exit;
  ADone := False;
  AItem.BeforeCustomDraw(Canvas);
  if Painter <> nil then
    Painter.DoCustomDrawBackground(AItem, ADone);
  if not ADone then
    View.DoCustomDrawBackground(AItem, ADone);
  AItem.AfterCustomDraw(Canvas);
  if not ADone then
    AItem.Draw(Canvas);
end;

procedure TcxSchedulerCustomViewPainter.DrawButtonCell(
  AItem: TcxSchedulerCustomViewInfoItem; var ADone: Boolean);
begin
  if AItem is TcxSchedulerMoreEventsButtonViewInfo then
  begin
    if Painter <> nil then
      Painter.DoCustomDrawButton(TcxSchedulerMoreEventsButtonViewInfo(AItem), ADone);
    if not ADone then
      View.DoCustomDrawButton(TcxSchedulerMoreEventsButtonViewInfo(AItem), ADone);
  end
  else
  begin
    if Painter <> nil then
      Painter.DoCustomDrawNavigationButton(TcxSchedulerContentNavigationButtonViewInfo(AItem), ADone);
    if not ADone then
      View.DoCustomDrawNavigationButton(TcxSchedulerContentNavigationButtonViewInfo(AItem), ADone);
  end;
end;

procedure TcxSchedulerCustomViewPainter.DrawContentCell(
  AItem: TcxSchedulerCustomViewInfoItem; var ADone: Boolean);
begin
  if Painter <> nil then
    Painter.DoCustomDrawContent(TcxSchedulerContentCellViewInfo(AItem), ADone);
  if not ADone then
    View.DoCustomDrawContent(TcxSchedulerContentCellViewInfo(AItem), ADone);
end;

procedure TcxSchedulerCustomViewPainter.DrawEventCell(
  AItem: TcxSchedulerCustomViewInfoItem; var ADone: Boolean);
begin
  if TcxSchedulerEventCellViewInfo(AItem).Hidden or not TcxSchedulerEventCellViewInfo(AItem).Visible then
    Exit;
  if Painter <> nil then
    Painter.DoCustomDrawEvent(TcxSchedulerEventCellViewInfo(AItem), ADone);
  if not ADone then
    View.DoCustomDrawEvent(TcxSchedulerEventCellViewInfo(AItem), ADone);
end;

procedure TcxSchedulerCustomViewPainter.DrawHeaderCell(
  AItem: TcxSchedulerCustomViewInfoItem; var ADone: Boolean);
begin
  if Painter <> nil then
    Painter.DoCustomDrawDayHeader(TcxSchedulerDayHeaderCellViewInfo(AItem), ADone);
  if not ADone then
    View.DoCustomDrawDayHeader(TcxSchedulerDayHeaderCellViewInfo(AItem), ADone);
end;

procedure TcxSchedulerCustomViewPainter.DrawGroupSeparatorCell(
  AItem: TcxSchedulerCustomViewInfoItem; var ADone: Boolean);
begin
  if Painter <> nil then
    Painter.DoCustomDrawGroupSeparator(TcxSchedulerGroupSeparatorCellViewInfo(AItem), ADone);
  if not ADone then
    View.DoCustomDrawGroupSeparator(TcxSchedulerGroupSeparatorCellViewInfo(AItem), ADone);
end;

procedure TcxSchedulerCustomViewPainter.DrawResourceHeaderCell(
  AItem: TcxSchedulerCustomViewInfoItem; var ADone: Boolean);
begin
  if Painter <> nil then
    Painter.DoCustomDrawResourceHeader(TcxSchedulerHeaderCellViewInfo(AItem), ADone);
  if not ADone then
    View.DoCustomDrawResourceHeader(TcxSchedulerHeaderCellViewInfo(AItem), ADone);
end;

function TcxSchedulerCustomViewPainter.GetView: TcxSchedulerCustomResourceView;
begin
  Result := TcxSchedulerCustomResourceView(Owner);
end;

{ TcxSchedulerEventPlace }

procedure TcxSchedulerEventPlace.AlignPosition(APlace: TcxSchedulerEventPlace);
begin
  LineFinish := APlace.LineFinish + 1 + (LineFinish - LineStart);
  LineStart := APlace.LineFinish + 1;
end;

function TcxSchedulerEventPlace.IntersectHorz(APlace: TcxSchedulerEventPlace): Boolean;
begin
  Result := (APlace.Resource = Resource) and (APlace.ColFinish >= ColStart) and (APlace.ColStart <= ColFinish);
end;

function TcxSchedulerEventPlace.IntersectVert(APlace: TcxSchedulerEventPlace): Boolean;
begin
  Result := (APlace.Resource = Resource) and (APlace.LineFinish >= LineStart) and (APlace.LineStart <= LineFinish);
end;

procedure TcxSchedulerEventPlace.LineStartToEvent;
begin
  TcxSchedulerControlEventAccess(Event).LineStart := LineStart;
end;

procedure TcxSchedulerEventPlace.ResetPosition;
begin
  LineFinish := LineFinish - LineStart;
  LineStart := 0;
end;

function cxCompareEventPlaces(AEventPlace1, AEventPlace2: TcxSchedulerEventPlace): Integer;
var
  AAllDay1, AAllDay2: Boolean;
  AEvent1, AEvent2: TcxSchedulerEvent;
begin
  Result := TdxNativeInt(AEventPlace1.Resource) - TdxNativeInt(AEventPlace2.Resource);
  if Result <> 0 then Exit;
  AEvent1 := AEventPlace1.Event;
  AEvent2 := AEventPlace2.Event;
  AAllDay1 := AEvent1.IsAllDayOrLonger;
  AAllDay2 := AEvent2.IsAllDayOrLonger;
  Result := Byte(AAllDay2) - Byte(AAllDay1);
  if Result <> 0 then Exit;

  if AEventPlace1.ColStart < AEventPlace2.ColStart then
    Result := -1
  else
    if AEventPlace1.ColStart > AEventPlace2.ColStart then
      Result := 1
    else
      if AEventPlace1.ColFinish > AEventPlace2.ColFinish then
        Result := -1
      else
        if AEventPlace1.ColFinish < AEventPlace2.ColFinish then
          Result := 1
        else
          if AEvent1.Start < AEvent2.Start then
            Result := -1
          else
            if AEvent1.Start > AEvent2.Start then
              Result := 1
            else
              if AEvent1.Finish > AEvent2.Finish then
                Result := -1
              else
                if AEvent1.Finish < AEvent2.Finish then
                  Result := 1
                else
                  Result := AEvent1.Index - AEvent2.Index;
end;

function cxCompareEventModernPlaces(AEventPlace1, AEventPlace2: TcxSchedulerEventPlace): Integer;
var
  AAllDay1, AAllDay2: Boolean;
  AEvent1, AEvent2: TcxSchedulerEvent;
begin
  Result := TdxNativeInt(AEventPlace1.Resource) - TdxNativeInt(AEventPlace2.Resource);
  if Result <> 0 then Exit;
  AEvent1 := AEventPlace1.Event;
  AEvent2 := AEventPlace2.Event;
  AAllDay1 := AEvent1.IsAllDayOrLonger;
  AAllDay2 := AEvent2.IsAllDayOrLonger;
  Result := Byte(AAllDay2) - Byte(AAllDay1);
  if Result <> 0 then Exit;

  if AEventPlace1.ColStart < AEventPlace2.ColStart then
    Result := -1
  else
    if AEventPlace1.ColStart > AEventPlace2.ColStart then
      Result := 1
    else
      if AEventPlace1.ColFinish > AEventPlace2.ColFinish then
        Result := -1
      else
        if AEventPlace1.ColFinish < AEventPlace2.ColFinish then
          Result := 1
        else
        begin
          if AAllDay1 then
          begin
            if AEvent1.Finish > AEvent2.Finish then
              Result := -1
            else
              if AEvent1.Finish < AEvent2.Finish then
                Result := 1
              else
                if AEvent1.Start < AEvent2.Start then
                  Result := -1
                else
                  if AEvent1.Start > AEvent2.Start then
                    Result := 1;
          end
          else
          begin
            if AEvent1.Start < AEvent2.Start then
              Result := -1
            else
              if AEvent1.Start > AEvent2.Start then
                Result := 1
              else
                if AEvent1.Finish > AEvent2.Finish then
                  Result := -1
                else
                  if AEvent1.Finish < AEvent2.Finish then
                    Result := 1;
          end;
          if Result = 0 then
          begin
            Result := AEvent2.State - AEvent1.State;
            if Result = 0 then
              Result := AEvent1.Index - AEvent2.Index;
          end;
        end;
end;

{ TcxSchedulerEventLayoutBuilder }

constructor TcxSchedulerEventLayoutBuilder.Create;
begin
  FEventPlaces := TcxObjectList.Create;
end;

destructor TcxSchedulerEventLayoutBuilder.Destroy;
begin
  FreeAndNil(FEventPlaces);
  inherited Destroy;
end;

function TcxSchedulerEventLayoutBuilder.AddEventPlace(AEvent: TcxSchedulerEvent;
  AStartCol, AFinishCol: Integer; ALineCount: Integer = 1; AResource: TObject = nil): TcxSchedulerEventPlace;
begin
  Result := TcxSchedulerEventPlace.Create;
  Result.ColStart := AStartCol;
  Result.ColFinish := AFinishCol;
  Result.Event := AEvent;
  Result.LineFinish := ALineCount - 1;
  Result.Resource := AResource;
  FEventPlaces.Add(Result);
end;

procedure TcxSchedulerEventLayoutBuilder.Clear;
begin
  FEventPlaces.Clear;
end;

procedure TcxSchedulerEventLayoutBuilder.Calculate(const ASchedulerViewStyle: TcxSchedulerViewStyle = svsClassic);
var
  ACurPlace: TcxSchedulerEventPlace;
  APlace: TcxSchedulerEventPlace;
  I, J, K: Integer;
begin
  if ASchedulerViewStyle = svsClassic then
    FEventPlaces.Sort(TListSortCompare(@cxCompareEventPlaces))
  else
    FEventPlaces.Sort(TListSortCompare(@cxCompareEventModernPlaces));

  for I := 1 to EventPlaceCount - 1 do
  begin
    J := I;
    K := I;
    ACurPlace := TcxSchedulerEventPlace(FEventPlaces.List[I]);
    repeat
      Dec(J);
      if ACurPlace.IntersectHorz(TcxSchedulerEventPlace(FEventPlaces.List[J])) then
        K := J;
    until J = 0;
    ACurPlace.ResetPosition;
    J := K;
    while J < I do
    begin
      APlace := TcxSchedulerEventPlace(FEventPlaces.List[J]);
      if ACurPlace.IntersectHorz(APlace) and ACurPlace.IntersectVert(APlace) then
      begin
        ACurPlace.AlignPosition(APlace);
        J := K;
      end
      else
        Inc(J);
    end;
  end;
end;

procedure TcxSchedulerEventLayoutBuilder.CalculateEx(AEventsList: TcxSchedulerFilteredEventList;
  APlaceInfoProc: TcxSchedulerEventLayoutBuilderGetEventPlaceProc);
var
  AEvent: TcxSchedulerControlEvent;
  I, AStartCol, AFinishCol, ALineCount: Integer;
begin
  Clear;
  for I := 0 to AEventsList.Count - 1 do
  begin
    AEvent := AEventsList[I];
    ALineCount := 1;
    if APlaceInfoProc(Self, AEventsList[I], AStartCol, AFinishCol, ALineCount) then
      AddEventPlace(AEvent, AStartCol, AFinishCol, ALineCount);
  end;
  Calculate;
end;

function TcxSchedulerEventLayoutBuilder.GetEventPlace(AIndex: Integer): TcxSchedulerEventPlace;
begin
  Result := TcxSchedulerEventPlace(FEventPlaces.List[AIndex]);
end;

function TcxSchedulerEventLayoutBuilder.GetEventPlaceCount: Integer;
begin
  Result := FEventPlaces.Count;
end;

end.
