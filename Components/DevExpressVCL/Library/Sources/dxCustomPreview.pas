{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressOfficeCore Library classes                        }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSOFFICECORE LIBRARY AND ALL     }
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

unit dxCustomPreview;

interface

{$I cxVer.inc}

uses
  Types, Classes, Controls, Messages, Windows, Graphics, Forms, StdCtrls, SysUtils, IniFiles,
  dxCore, cxGraphics, cxGeometry, cxControls, cxLookAndFeels, cxClasses, dxTouch, Generics.Defaults, Generics.Collections;

const
  dxPreviewCachedPagesCount: Integer = 16;
  dxPreviewDragHintOffset = 5;
  dxPreviewMarginSelectDelta = 3;
  dxPreviewMinUsefulSize: TPoint = (X: 500; Y: 500);
  dxPreviewMinZoomFactor = 5;
  dxPreviewScrollStep = 30;
  dxPreviewZoomStep = 10;

type
  TdxPreviewOptionBehavior = (pobAllowDragMargins, pobHotTrack, pobKeyNavigation,
    pobStoreInRegistry, pobThumbTracking, pobAutoSwapMargins, pobNonCenterizePages);
  TdxPreviewOptionsBehavior = set of TdxPreviewOptionBehavior;

  TdxPreviewOptionHint = (pohShowForMargins, pohShowOnDrag, pohShowOnScroll);
  TdxPreviewOptionsHint = set of TdxPreviewOptionHint;

  TdxPreviewOptionStore = (posZoom);
  TdxPreviewOptionsStore = set of TdxPreviewOptionStore;

  TdxPreviewOptionView = (povAutoHideScrollBars, povDefaultDrawPageBackground, povMargins, povPageSelection);
  TdxPreviewOptionsView = set of TdxPreviewOptionView;

  TdxPreviewOptionZoom = (pozZoomOnClick, pozZoomOnMouseRoll);
  TdxPreviewOptionsZoom = set of TdxPreviewOptionZoom;

  TdxPreviewDragStage = (pdsAfter, pdsBefore, pdsDrag);
  TdxPreviewMeasurementUnits = (pmuDefault, pmuInches, pmuMillimeters, pmuCentimeters, pmuPoints, pmuPicas);
  TdxPreviewPaperOrientation = (ppoPortrait, ppoLandscape);
  TdxPreviewScrollDirection = (psdLeft, psdUp, psdRight, psdDown);
  TdxPreviewZoomMode = (pzmNone, pzmPageWidth, pzmPages);

const
  dxPreviewDefaultBehaviorOptions = [pobAllowDragMargins, pobKeyNavigation, pobThumbTracking, pobAutoSwapMargins];
  dxPreviewDefaultOptionsView = [povAutoHideScrollBars, povDefaultDrawPageBackground, povMargins, povPageSelection];
  dxPreviewDefaultOptionsHint = [pohShowForMargins, pohShowOnDrag, pohShowOnScroll];
  dxPreviewDefaultOptionsZoom = [pozZoomOnClick];

type
  TCMHintHide = TWMNoParams;

  TdxCustomPreview = class;
  TdxPreviewPageList = class;
  TdxPreviewPageMargins = class;

  TdxPreviewPageClass = class of TdxPreviewPage;

  TdxPreviewPageSizeOptions = class;
  TdxPReviewPageSizeOptionsClass = class of TdxPreviewPageSizeOptions;

  { TdxPreviewPageSizeOptions }

  TdxPreviewPageSizeOptions = class(TPersistent)
  strict private
    FAssigned: Boolean;
    FFooter: Integer;
    FHeader: Integer;
    FMargins: TRect;
    FMarginsMinValues: TRect;
    FMaster: TdxPreviewPageSizeOptions;
    FMinUsefulSize: TPoint;
    FOrientation: TdxPreviewPaperOrientation;
    FSize: TPoint;
    FPreview: TdxCustomPreview;

    FOnChange: TNotifyEvent;

    function GetActualSize: TPoint;
    function GetFooter: Integer;
    function GetHeader: Integer;
    function GetMargins: TRect;
    function GetMarginsMinValues: TRect;
    function GetMinUsefulSize: TPoint;
    function GetOrientation: TdxPreviewPaperOrientation;
    function GetSize: TPoint;
    procedure SetAssigned(AValue: Boolean);
    procedure SetFooter(Value: Integer);
    procedure SetHeader(Value: Integer);
    procedure SetMargins(const Value: TRect);
    procedure SetMarginsMinValues(const Value: TRect);
    procedure SetMinUsefulSize(const Value: TPoint);
    procedure SetOrientation(AValue: TdxPreviewPaperOrientation);
    procedure SetSize(const Value: TPoint);
  protected
    function AllowAutoSwapMargins: Boolean; virtual;
    function GetActualSizeInPixels: TPoint; virtual;
    function GetDefaultMinUsefulSize: TPoint; virtual;

    procedure Changed;
    procedure RotateMargins;

    property Preview: TdxCustomPreview read FPreview;
  public
    constructor Create(APreview: TdxCustomPreview); overload;
    constructor Create(APreview: TdxCustomPreview; const ADefaultSize: TPoint); overload;
    constructor Create(APreview: TdxCustomPreview; const AMaster: TdxPreviewPageSizeOptions); overload;
    procedure Assign(Source: TPersistent); override;
    //
    property ActualSize: TPoint read GetActualSize;
    property ActualSizeInPixels: TPoint read GetActualSizeInPixels;
    //
    property Assigned: Boolean read FAssigned write SetAssigned default False;
    property Footer: Integer read GetFooter write SetFooter;
    property Header: Integer read GetHeader write SetHeader;
    property Margins: TRect read GetMargins write SetMargins;
    property MarginsMinValues: TRect read GetMarginsMinValues write SetMarginsMinValues;
    property Master: TdxPreviewPageSizeOptions read FMaster;
    property MinUsefulSize: TPoint read GetMinUsefulSize write SetMinUsefulSize;
    property Orientation: TdxPreviewPaperOrientation read GetOrientation write SetOrientation default ppoPortrait;
    property Size: TPoint read GetSize write SetSize;
    //
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  { TdxPreviewPage }

  TdxPreviewPage = class(TObject)
  strict private
    FBounds: TRect;
    FCell: TPoint;
    FPageSize: TdxPreviewPageSizeOptions;
    FPreview: TdxCustomPreview;
    FTag: TdxNativeInt;

    function GetIndex: Integer;
    function GetLargerPartVisible: Boolean;
    function GetPartVisible: Boolean;
    function GetSelected: Boolean;
    function GetSiteBounds: TRect;
    function GetZoomed: Boolean;
    procedure SetSelected(Value: Boolean);
  protected
    function GetPageSizeOptionsClass: TdxPreviewPageSizeOptionsClass; virtual;
    function GetVisible: Boolean; virtual;
  public
    constructor Create(APreview: TdxCustomPreview); virtual;
    destructor Destroy; override;
    procedure Draw(ACanvas: TcxCanvas); virtual;
    function GetCursor: TCursor; virtual;
    function HasPoint(const Pt: TPoint): Boolean; overload;
    function HasPoint(X, Y: Integer): Boolean; overload;
    procedure Invalidate;
    procedure MakeVisible;
    //
    property Bounds: TRect read FBounds write FBounds;
    property Cell: TPoint read FCell write FCell;
    property Index: Integer read GetIndex;
    property LargerPartVisible: Boolean read GetLargerPartVisible;
    property PageSize: TdxPreviewPageSizeOptions read FPageSize;
    property PartVisible: Boolean read GetPartVisible;
    property Selected: Boolean read GetSelected write SetSelected;
    property SiteBounds: TRect read GetSiteBounds;
    property Tag: TdxNativeInt read FTag write FTag;
    property Preview: TdxCustomPreview read FPreview;
    property Visible: Boolean read GetVisible;
    property Zoomed: Boolean read GetZoomed;
  end;

  { TdxPreviewPagesRow }

  TdxPreviewPagesRow = class
  public
    Bounds: TRect;
    FinishIndex: Integer;
    StartIndex: Integer;
  end;

  { TdxPreviewPagesRowList }

  TdxPreviewPagesRowList = class(TObjectList<TdxPreviewPagesRow>)
  public
    function Add(StartIndex, FinishIndex: Integer; const Bounds: TRect): TdxPreviewPagesRow;
    procedure OffsetBounds(X, Y: Integer);
  end;

  { TdxPreviewPageList }

  TdxPreviewPageList = class(TList<TdxPreviewPage>)
  protected
    procedure CenterPagesInRow(ARow: TdxPreviewPagesRow; AMaxWidth: Integer);
  public
    procedure GetPartVisiblePageRanges(AStartIndex, AEndIndex: PInteger ); overload;
    procedure GetPartVisiblePageRanges(out AStartIndex, AEndIndex: Integer); overload;
    procedure GetVisiblePageRanges(AStartIndex, AEndIndex: PInteger ); overload;
    procedure GetVisiblePageRanges(out AStartIndex, AEndIndex: Integer); overload;
    procedure GetVisibleSelectedPageIndex(out AIndex: Integer);
    procedure Invalidate;

    procedure OffsetBounds(X, Y: Integer); overload;
    procedure OffsetBounds(X, Y, AStartIndex, AFinishIndex: Integer); overload;

    function PageIndexFromPoint(const Pt: TPoint): Integer; overload;
    function PageIndexFromPoint(const Pt: TSmallPoint): Integer; overload;
    function PageIndexFromPoint(X, Y: Integer): Integer; overload;

    function GetBounds: TRect; overload;
    function GetBounds(AStartIndex, AFinishIndex: Integer): TRect; overload;

    property Bounds: TRect read GetBounds;
  end;

  { TdxPreviewPageContentCache }

  TdxPreviewPageContentCache = class(TcxBitmap)
  strict private
    FDirty: Boolean;
    FPage: TdxPreviewPage;

    procedure SetPage(AValue: TdxPreviewPage);
  public
    procedure CheckSize;
    //
    property Dirty: Boolean read FDirty write FDirty;
    property Page: TdxPreviewPage read FPage write SetPage;
  end;

  { TdxPreviewPageContentCachePool }

  TdxPreviewPageContentCachePool = class
  strict private
    FCapacity: Integer;
    FList: TcxObjectList;

    function Find(APage: TdxPreviewPage; out AItem: TdxPreviewPageContentCache): Boolean;
    function FindInvisible(APage: TdxPreviewPage; out AItem: TdxPreviewPageContentCache): Boolean;
    function GetCount: Integer;
    function GetItem(Index: Integer): TdxPreviewPageContentCache;
  protected
    procedure Clear;
  public
    constructor Create(ACapacity: Integer); virtual;
    destructor Destroy; override;
    function Add(APage: TdxPreviewPage): TdxPreviewPageContentCache;
    procedure Invalidate(APage: TdxPreviewPage);
    procedure InvalidateAll;
    procedure Remove(APage: TdxPreviewPage);
    //
    property Capacity: Integer read FCapacity;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TdxPreviewPageContentCache read GetItem; default;
  end;

  TdxPreviewHitTest = (phtNoWhere, phtPage, phtMarginLeft, phtMarginTop,
    phtMarginRight, phtMarginBottom, phtMarginHeader, phtMarginFooter);
  TdxPreviewHitTests = set of TdxPreviewHitTest;

  TdxPreviewPageMarginClass = class of TdxPreviewPageMargin;

  { TdxPreviewPageMargin }

  TdxPreviewPageMargin = class(TPersistent)
  strict private
    FCaption: string;
    FDraggingPos: Integer;
    FEnabled: Boolean;
    FIsCaptionAssigned: Boolean;
    FMargins: TdxPreviewPageMargins;
    FMaxValue: Integer;
    FMinValue: Integer;
    FScreenBitmap: HBITMAP;
    FValue: Integer;
    FVisible: Boolean;

    function GetActualMaxValue: Integer;
    function GetActualMinValue: Integer;
    function GetActualPageSize: TdxPreviewPageSizeOptions;
    function GetActualValue: Integer;
    function GetBounds: TRect;
    function GetCaption: string;
    function GetDisplayText: string;
    function GetDraggingValue: Integer;
    function GetIsDragging: Boolean;
    function GetIsDraggingPosAssigned: Boolean;
    function GetMaxPos: Integer;
    function GetMinPos: Integer;
    function GetPageBounds: TRect;
    function GetViewer: TdxCustomPreview;
    function GetSelectableBounds: TRect;
    function GetVisibleValue: Integer;
    function IsCaptionStored: Boolean;
    function IsValueStored: Boolean;
    procedure SetCaption(const Value: string);
    procedure SetDraggingPos(Value: Integer);
    procedure SetEnabled(Value: Boolean);
    procedure SetMaxValue(AValue: Integer);
    procedure SetMinValue(AValue: Integer);
    procedure SetValue(AValue: Integer);
    procedure SetVisible(Value: Boolean);

    procedure ReadIsCaptionAssigned(AReader: TReader);
    procedure WriteIsCaptionAssigned(AWriter: TWriter);
  protected
    procedure DefineProperties(Filer: TFiler); override;

    function GetIsForceRecalculatePageCount: Boolean; virtual;
    function GetIsForward: Boolean; virtual;
    function GetIsVertical: Boolean; virtual;
    function GetSelectDelta: Integer; virtual;
    function DoGetActualMaxValue: Integer; virtual;
    function DoGetActualMinValue: Integer; virtual;
    function DoGetMaxMinValue: Integer; virtual;
    function DoPosFromValue(AValue: Integer): Integer; virtual;
    function DoValueFromPos(APos: Integer): Integer; virtual;

    procedure Changed(HardRefresh: Boolean); virtual;
    procedure DoAssign(Source: TdxPreviewPageMargin); virtual;
    procedure DoDragAfter;
    procedure DoDragBefore;
    procedure DoRestoreDefaults; virtual;

    procedure Draw(DC: HDC); virtual;
    procedure Invalidate; virtual;
    procedure Invert(DC: HDC); virtual;

    function CheckValue(Value: Integer): Integer;
    function PosFromValue(AValue: Integer): Integer;
    function ValueFromPos(APos: Integer): Integer;

    property ActualMaxValue: Integer read GetActualMaxValue;
    property ActualMinValue: Integer read GetActualMinValue;
    property ActualPageSize: TdxPreviewPageSizeOptions read GetActualPageSize;
    property ActualValue: Integer read GetActualValue;
    property Bounds: TRect read GetBounds;
    property DraggingPos: Integer read FDraggingPos write SetDraggingPos;
    property IsForceRecalculatePageCount: Boolean read GetIsForceRecalculatePageCount;
    property MaxPos: Integer read GetMaxPos;
    property MaxValue: Integer read FMaxValue write SetMaxValue; // HIMETRIC
    property MinPos: Integer read GetMinPos;
    property PageBounds: TRect read GetPageBounds;
    property SelectableBounds: TRect read GetSelectableBounds;
    property SelectDelta: Integer read GetSelectDelta;
    property Preview: TdxCustomPreview read GetViewer;
  public
    constructor Create(AMargins: TdxPreviewPageMargins); virtual;
    procedure Assign(Source: TPersistent); override;
    procedure RestoreDefaults;

    procedure BeginUpdate;
    procedure EndUpdate;

    function DefaultCaption: string; virtual;
    function DefaultValue: Integer; virtual;
    function GetHitTest: TdxPreviewHitTest; virtual;
    function GetCursor: TCursor; virtual;

    function HasPoint(const Pt: TPoint): Boolean; overload;
    function HasPoint(const Pt: TSmallPoint): Boolean; overload;
    function HasPoint(X, Y: Integer): Boolean; overload;

    property DisplayText: string read GetDisplayText;
    property DraggingValue: Integer read GetDraggingValue;
    property IsDragging: Boolean read GetIsDragging;
    property IsDraggingPosAssigned: Boolean read GetIsDraggingPosAssigned;
    property IsForward: Boolean read GetIsForward;
    property IsVertical: Boolean read GetIsVertical;
    property Margins: TdxPreviewPageMargins read FMargins;
    property VisibleValue: Integer read GetVisibleValue; // pixels
  published
    property Caption: string read GetCaption write SetCaption stored IsCaptionStored;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property MinValue: Integer read FMinValue write SetMinValue default 0;   // HIMETRIC
    property Value: Integer read FValue write SetValue stored IsValueStored; // HIMETRIC
    property Visible: Boolean read FVisible write SetVisible default True;
  end;

  { TdxPreviewPageMarginBottom }

  TdxPreviewPageMarginBottom = class(TdxPreviewPageMargin)
  protected
    function GetIsForceRecalculatePageCount: Boolean; override;
    function GetIsForward: Boolean; override;
    function GetIsVertical: Boolean; override;
    function DoGetActualMaxValue: Integer; override;
    function DoGetActualMinValue: Integer; override;
    function DoGetMaxMinValue: Integer; override;
    function DoPosFromValue(AValue: Integer): Integer; override;
    function DoValueFromPos(APos: Integer): Integer; override;
  public
    function DefaultCaption: string; override;
    function DefaultValue: Integer; override;
    function GetHitTest: TdxPreviewHitTest; override;
    function GetCursor: TCursor; override;
  end;

  { TdxPreviewPageMarginFooter }

  TdxPreviewPageMarginFooter = class(TdxPreviewPageMargin)
  protected
    function GetIsForward: Boolean; override;
    function GetIsVertical: Boolean; override;
    function DoGetActualMaxValue: Integer; override;
    function DoGetMaxMinValue: Integer; override;
    function DoPosFromValue(AValue: Integer): Integer; override;
    function DoValueFromPos(APos: Integer): Integer; override;
  public
    function DefaultCaption: string; override;
    function DefaultValue: Integer; override;
    function GetHitTest: TdxPreviewHitTest; override;
    function GetCursor: TCursor; override;
  end;

  { TdxPreviewPageMarginHeader }

  TdxPreviewPageMarginHeader = class(TdxPreviewPageMargin)
  protected
    function GetIsForward: Boolean; override;
    function GetIsVertical: Boolean; override;
    function DoGetActualMaxValue: Integer; override;
    function DoGetMaxMinValue: Integer; override;
    function DoPosFromValue(AValue: Integer): Integer; override;
    function DoValueFromPos(APos: Integer): Integer; override;
  public
    function DefaultCaption: string; override;
    function DefaultValue: Integer; override;
    function GetHitTest: TdxPreviewHitTest; override;
    function GetCursor: TCursor; override;
  end;

  { TdxPreviewPageMarginLeft }

  TdxPreviewPageMarginLeft = class(TdxPreviewPageMargin)
  protected
    function GetIsForceRecalculatePageCount: Boolean; override;
    function GetIsForward: Boolean; override;
    function GetIsVertical: Boolean; override;
    function DoGetActualMaxValue: Integer; override;
    function DoGetMaxMinValue: Integer; override;
    function DoPosFromValue(AValue: Integer): Integer; override;
    function DoValueFromPos(APos: Integer): Integer; override;
  public
    function DefaultCaption: string; override;
    function DefaultValue: Integer; override;
    function GetHitTest: TdxPreviewHitTest; override;
    function GetCursor: TCursor; override;
  end;

  { TdxPreviewPageMarginRight }

  TdxPreviewPageMarginRight = class(TdxPreviewPageMargin)
  protected
    function GetIsForceRecalculatePageCount: Boolean; override;
    function GetIsForward: Boolean; override;
    function GetIsVertical: Boolean; override;
    function DoGetActualMaxValue: Integer; override;
    function DoGetMaxMinValue: Integer; override;
    function DoPosFromValue(AValue: Integer): Integer; override;
    function DoValueFromPos(APos: Integer): Integer; override;
  public
    function DefaultCaption: string; override;
    function DefaultValue: Integer; override;
    function GetHitTest: TdxPreviewHitTest; override;
    function GetCursor: TCursor; override;
  end;

  { TdxPreviewPageMarginTop }

  TdxPreviewPageMarginTop = class(TdxPreviewPageMargin)
  protected
    function GetIsForceRecalculatePageCount: Boolean; override;
    function GetIsForward: Boolean; override;
    function GetIsVertical: Boolean; override;
    function DoGetActualMaxValue: Integer; override;
    function DoGetActualMinValue: Integer; override;
    function DoGetMaxMinValue: Integer; override;
    function DoPosFromValue(AValue: Integer): Integer; override;
    function DoValueFromPos(APos: Integer): Integer; override;
  public
    function DefaultCaption: string; override;
    function DefaultValue: Integer; override;
    function GetHitTest: TdxPreviewHitTest; override;
    function GetCursor: TCursor; override;
  end;

  { TdxPreviewPageMargins }

  TdxPreviewPageMarginsClass = class of TdxPreviewPageMargins;

  TdxPreviewPageMargins = class(TPersistent)
  strict private
    FMargins: TList;
    FUpdateCount: Integer;
    FPreview: TdxCustomPreview;

    function GetCount: Integer;
    function GetMargin(Index: Integer): TdxPreviewPageMargin;
    function GetMarginBottom: TdxPreviewPageMarginBottom;
    function GetMarginByCaption(const Caption: string): TdxPreviewPageMargin;
    function GetMarginByClass(Index: TdxPreviewPageMarginClass): TdxPreviewPageMargin;
    function GetMarginFooter: TdxPreviewPageMarginFooter;
    function GetMarginHeader: TdxPreviewPageMarginHeader;
    function GetMarginLeft: TdxPreviewPageMarginLeft;
    function GetMarginRight: TdxPreviewPageMarginRight;
    function GetMarginTop: TdxPreviewPageMarginTop;
    procedure SetMargin(Index: Integer; Value: TdxPreviewPageMargin);
    procedure SetMarginByClass(Index: TdxPreviewPageMarginClass; Value: TdxPreviewPageMargin);
    procedure SetMarginBottom(Value: TdxPreviewPageMarginBottom);
    procedure SetMarginFooter(Value: TdxPreviewPageMarginFooter);
    procedure SetMarginHeader(Value: TdxPreviewPageMarginHeader);
    procedure SetMarginLeft(Value: TdxPreviewPageMarginLeft);
    procedure SetMarginRight(Value: TdxPreviewPageMarginRight);
    procedure SetMarginTop(Value: TdxPreviewPageMarginTop);
  protected
    procedure DoAssign(Source: TdxPreviewPageMargins); virtual;
    procedure DoRestoreDefaults; virtual;

    function AddMargin(AClass: TdxPreviewPageMarginClass): TdxPreviewPageMargin; virtual;
    procedure AddMargins; virtual;
    procedure FreeAndNilMargins;
    procedure Update(AMargin: TdxPreviewPageMargin); virtual;
  public
    constructor Create(APreview: TdxCustomPreview); virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure RestoreDefaults;

    procedure BeginUpdate;
    procedure EndUpdate;

    property Count: Integer read GetCount;
    property Margins[Index: Integer]: TdxPreviewPageMargin read GetMargin write SetMargin; default;
    property MarginsByCaption[const Caption: string]: TdxPreviewPageMargin read GetMarginByCaption;
    property MarginsByClass[Index: TdxPreviewPageMarginClass]: TdxPreviewPageMargin read GetMarginByClass write SetMarginByClass;
    property Preview: TdxCustomPreview read FPreview;
  published
    property Bottom: TdxPreviewPageMarginBottom read GetMarginBottom write SetMarginBottom;
    property Footer: TdxPreviewPageMarginFooter read GetMarginFooter write SetMarginFooter;
    property Header: TdxPreviewPageMarginHeader read GetMarginHeader write SetMarginHeader;
    property Left: TdxPreviewPageMarginLeft read GetMarginLeft write SetMarginLeft;
    property Right: TdxPreviewPageMarginRight read GetMarginRight write SetMarginRight;
    property Top: TdxPreviewPageMarginTop read GetMarginTop write SetMarginTop;
  end;

  TdxCanShowMarginHintEvent = procedure(Sender: TObject; var ACanShowHint: Boolean) of object;
  TdxDrawPageContentEvent = procedure(Sender: TObject; ACanvas: TCanvas; ARect: TRect; APageIndex: Integer) of object;
  TdxGetPageNumberHintEvent = procedure(Sender: TObject; AStartPage, AEndPage: Integer; var AHintString: string) of object;
  TdxMarginEvent = procedure(Sender: TObject; AMargin: TdxPreviewPageMargin) of object;
  TdxPageBackgroundDrawEvent = procedure(Sender: TObject; ACanvas: TCanvas; const ARect: TRect; APageIndex: Integer) of object;
  TdxPreviewGetDefaultMeasurementUnitsEvent = procedure (Sender: TObject; var AMeasurementUnits: TdxPreviewMeasurementUnits) of object;
  TdxPreviewPageEvent = procedure(Sender: TObject; APageIndex: Integer) of object;
  TdxSelectingPageEvent = procedure(Sender: TObject; APagePage: Integer; var ACanSelect: Boolean) of object;

  TdxPreviewStates = (psZooming, psScrolling);
  TdxPreviewState = set of TdxPreviewStates;

  { TdxCustomPreview }

  TdxCustomPreview = class(TcxScrollingControl)
  private
    FAbsoluteIndentLeft: Integer;
    FAbsoluteIndentRight: Integer;
    FBeforeDragPos: Integer;
    FDefaultMeasurementUnits: TdxPreviewMeasurementUnits;
    FDraggingMargin: TdxPreviewPageMargin;
    FDragOffset: Integer;
    FHintHideLongTimer: TcxTimer;
    FHintHideShortTimer: TcxTimer;
    FHintShowTimer: TcxTimer;
    FHintWindow: TCustomControl;
    FHintWindowPageNumber: TCustomControl;
    FIndent: Integer;
    FIsMarginDropping: Boolean;
    FIsNoPagesTextAssigned: Boolean;
    FLastMousePos: TPoint;

    FMarginColor: TColor;
    FMarginPen: HPEN;
    FMargins: TdxPreviewPageMargins;
    FMaxZoomFactor: Integer;
    FMeasurementUnits: TdxPreviewMeasurementUnits;
    FMinZoomFactor: Integer;
    FNoPagesText: string;

    FInternalOptionsBehavior: TdxPreviewOptionsBehavior;
    FOptionsHint: TdxPreviewOptionsHint;
    FOptionsStore: TdxPreviewOptionsStore;
    FInternalOptionsView: TdxPreviewOptionsView;
    FInternalOptionsZoom: TdxPreviewOptionsZoom;

    FPages: TdxPreviewPageList;
    FPagesContentCache: TdxPreviewPageContentCachePool;
    FPageSize: TdxPreviewPageSizeOptions;
    FPagesRows: TdxPreviewPagesRowList;
    FPageXCount: Integer;
    FPageYCount: Integer;
    FPrevZoomFactor: Integer;
    FRegistryPath: string;
    FState: TdxPreviewState;

    FUnzoomedFactor: Integer;
    FUnzoomedMode: TdxPreviewZoomMode;
    FUpdateCount: Integer;
    FZoomed: Boolean;
    FZoomFactor: Integer;
    FZoomMode: TdxPreviewZoomMode;
    FZoomStep: Integer;

    FOnAfterDragMargin: TdxMarginEvent;
    FOnBeforeDragMargin: TdxMarginEvent;
    FOnCalcPageCount: TNotifyEvent;
    FOnCanShowMarginHint: TdxCanShowMarginHintEvent;
    FOnChangePageCount: TNotifyEvent;
    FOnDragMargin: TdxMarginEvent;
    FOnDrawPageBackground: TdxPageBackgroundDrawEvent;
    FOnDrawPageContent: TdxDrawPageContentEvent;
    FOnGetPageNumberHint: TdxGetPageNumberHintEvent;
    FOnMarginChanged: TdxMarginEvent;
    FOnPostDrawPageContent: TdxDrawPageContentEvent;
    FOnSelectedPageChanged: TdxPreviewPageEvent;
    FOnSelectedPageChanging: TdxPreviewPageEvent;
    FOnSelectingPage: TdxSelectingPageEvent;
    FOnZoomFactorChanged: TNotifyEvent;
    FOnZoomModeChanged: TNotifyEvent;

    function GetAbsoluteIndent: Integer;
    function GetActualMeasurementUnits: TdxPreviewMeasurementUnits;
    function GetActualZoomFactor: Integer;
    function GetClientHeight: Integer;
    function GetClientWidth: Integer;
    function GetContentHeight: Integer;
    function GetContentWidth: Integer;
    function GetIndent: Integer;
    function GetNoPagesText: string;
    function GetPage(Index: Integer): TdxPreviewPage;
    function GetPageBorders: TRect;
    function GetPageCount: Integer;
    function GetSelPage: TdxPreviewPage;
    function GetSelPageSizeOptions: TdxPreviewPageSizeOptions;
    function IsNoPagesTextStored: Boolean;
    procedure SetMarginColor(Value: TColor);
    procedure SetMargins(Value: TdxPreviewPageMargins);
    procedure SetMaxZoomFactor(Value: Integer);
    procedure SetMinZoomFactor(Value: Integer);
    procedure SetNoPagesText(const Value: string);
    procedure SetOnCalcPageCount(Value: TNotifyEvent);
    procedure SetOptionsHint(Value:  TdxPreviewOptionsHint);
    procedure SetOptionsStore(Value: TdxPreviewOptionsStore);

    procedure SetInternalOptionsBehavior(Value: TdxPreviewOptionsBehavior);
    procedure SetInternalOptionsView(Value: TdxPreviewOptionsView);
    procedure SetInternalOptionsZoom(Value: TdxPreviewOptionsZoom);

    procedure SetPageCount(Value: Integer);
    procedure SetPageSize(AValue: TdxPreviewPageSizeOptions);
    procedure SetPageXCount(Value: Integer);
    procedure SetPageYCount(Value: Integer);

    procedure SetZoomed(Value: Boolean);
    procedure SetZoomFactor(Value: Integer);
    procedure SetZoomMode(Value: TdxPreviewZoomMode);
    procedure SetZoomStep(Value: Integer);

    procedure PageParametersChanged(Sender: TObject);
    procedure ResyncSelPageIndex;

    function IsScrolling: Boolean;
    function CanAnyScrolling: Boolean;
    function CanHorzScrolling: Boolean;
    function CanPageScrolling(ADirection: TdxPreviewScrollDirection): Boolean;
    function CanVertScrolling: Boolean;

    function SelectPage(APageIndex: Integer): Boolean;
    procedure ChangeSelPageIndex(ADirection: Integer);

    function IsEnabledBehaviorOption(AOption: TdxPreviewOptionBehavior): Boolean;
    function IsEnabledViewOption(AOption: TdxPreviewOptionView): Boolean;

    procedure CalculateAbsoluteIndents;
    procedure CalculateIndent;
    procedure CalculateLayout(AType: TdxChangeType);
    procedure CalculateZoomFactor;

    function CanChangeMargins: Boolean;
    procedure CancelDragMargin;
    procedure ClearLastMousePos;
    procedure FreeMarginPen;
    procedure RecreateMarginPen;
    procedure StopMarginDragging;

    procedure ActivateHint(AMargin: TdxPreviewPageMargin);
    procedure CancelHintHide;
    procedure CancelHintShow;
    procedure CreateHint;
    procedure DestroyHint;
    procedure ResetHintShowTimer(X, Y: Integer);
    procedure StartHintShow;

    procedure DestroyPageNumberHint;
    procedure UpdatePageNumberHint;

    function IsDesigning: Boolean;
    function IsParentFormActive: Boolean;

    procedure HintHideLongTimerHandler(Sender: TObject);
    procedure HintHideShortTimerHandler(Sender: TObject);
    procedure HintShowTimerHandler(Sender: TObject);

    procedure ReadIsNoPagesTextAssigned(AReader: TReader);
    procedure WriteIsNoPagesTextAssigned(AWriter: TWriter);

    procedure CMCancelMode(var Message: TCMCancelMode); Message CM_CANCELMODE;
    procedure CMHintShow(var Message: TCMHintShow); Message CM_HINTSHOW;
    procedure CMSysColorChange(var Message: TMessage); Message CM_SYSCOLORCHANGE;
    procedure WMCaptureChanged(var Message: TMessage); Message WM_CAPTURECHANGED;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); Message WM_GETDLGCODE;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); Message WM_ERASEBKGND;
    procedure WMKillFocus(var Message: TWMKillFocus); Message WM_KILLFOCUS;
    procedure WMRButtonUp(var Message: TWMRButtonUp); Message WM_RBUTTONUP;
    procedure WMLButtonUp(var Message: TWMLButtonUp); message WM_LBUTTONUP;
    procedure WMMouseActivate(var Message: TWMMouseActivate); Message WM_MOUSEACTIVATE;
    procedure WMMouseWheel(var Message: TWMMouseWheel); Message WM_MOUSEWHEEL;
    procedure WMNCDestroy(var Message: TWMNCDestroy); Message WM_NCDESTROY;
    procedure WMNCHitTest(var Message: TWMNCHitTest); Message WM_NCHITTEST;
    procedure WMSetCursor(var Message: TWMSetCursor); Message WM_SETCURSOR;
  protected
    FSelPageIndex: Integer;

    function GetContentSize: TSize; override;
    function GetDesignHitTest(X, Y: Integer; Shift: TShiftState): Boolean; override;

    procedure Calculate(AType: TdxChangeType); override;
    procedure LayoutChanged(AType: TdxChangeType = ctHard); override;

    procedure CreateParams(var Params: TCreateParams); override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure DblClick; override;

    function GetActualPageSizeOptions(APage: TdxPreviewPage): TdxPreviewPageSizeOptions;
    function GetDefaultPanOptions: Integer; override;
    function GetMainScrollBarsClass: TcxControlCustomScrollBarsClass; override;

    function IsDefaultGesture(AGestureID: Integer): Boolean; override;
    function IsDoubleBufferedNeeded: Boolean; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure DoPaint; override;

    function AllowTouchScrollUIMode: Boolean; override;
    function GetScrollStep: Integer; override;
    procedure Scroll(AScrollBarKind: TScrollBarKind; AScrollCode: TScrollCode; var AScrollPos: Integer); override;
    procedure ScrollHorizontal(AScrollCode: TScrollCode; AScrollPos: Integer);
    procedure ScrollVertical(AScrollCode: TScrollCode; AScrollPos: Integer);

    function ProcessMouseWheelMessage(AWheelDelta: Integer): Boolean;
    procedure WndProc(var Message: TMessage); override;

    function CreateMargins: TdxPreviewPageMargins; virtual;
    function GetCursor: TCursor; virtual;
    function GetMarginsClass: TdxPreviewPageMarginsClass; virtual;
    procedure CalculateZoomFactorForPageWidthPreviewZoomMode(AFirstPageIndex, ALastPageIndex: Integer); virtual;
    procedure CalculateZoomFactorForPagesPreviewZoomMode(AFirstPageIndex, ALastPageIndex: Integer); virtual;
    procedure ResyncMargins; virtual;

    function CreatePage: TdxPreviewPage; virtual;
    function GetPageClass: TdxPreviewPageClass; virtual;
    function GetPageSizeOptionsClass: TdxPreviewPageSizeOptionsClass; virtual;
    // Default values
    function GetDefaultCachedPagesCount: Integer; virtual;
    function GetDefaultHeight: Integer; virtual;
    function GetDefaultMaxZoomFactor: Integer; virtual;
    function GetDefaultMinZoomFactor: Integer; virtual;
    function GetDefaultWidth: Integer; virtual;
    function GetDefaultZoomFactor: Integer; virtual;
    function GetDefaultZoomStep: Integer; virtual;
    function GetZoomInCursor: TCursor; virtual;
    function GetZoomOutCursor: TCursor; virtual;
    // OptionsBehavior flags
    function AllowDragMargins: Boolean; virtual;
    function AllowHotTrack: Boolean; virtual;
    function AllowKeyNavigation: Boolean; virtual;
    function AllowMakeVisibleSelectedPageWhenScrolling: Boolean; virtual;
    function AllowThumbTracking: Boolean; virtual;
    function NonCenterizePages: Boolean; virtual;
    function NonVerticalCenterizePages: Boolean; virtual;
    function NeedStoreInRegistry: Boolean; virtual;
    // OptionsStore flags
    function NeedStoreZoomOptions: Boolean; virtual;
    // OptionsView flags
    function CanShowMargins: Boolean; virtual;
    function AllowAutoHideScrollBars: Boolean; virtual;
    // scrolling
    function AllowHybridScrollbarMode: Boolean; override;
    function AllowPageSelection: Boolean; virtual;
    function IsDefaultDrawPageBackground: Boolean; virtual;
    // OptionsZoom flags
    function AllowZoomOnClick: Boolean; virtual;
    function AllowZoomOnMouseRoll: Boolean; virtual;

    function GetAbsoluteIndentLeft: Integer; virtual;
    function GetAbsoluteIndentRight: Integer; virtual;
    procedure CalculatePageNumberHintText(AStartPage, AEndPage: Integer; var AText: string); virtual;
    procedure CalculatePagesLayout;

    procedure OffsetContent(const ADelta: TPoint);
    procedure ProcessClickBySelectedPage(Shift: TShiftState; X, Y: Integer); virtual;
    procedure ProcessLeftClickByPage(Shift: TShiftState; X, Y: Integer); virtual;
    procedure SetSelPageIndex(Value: Integer); virtual;
    procedure UpdateSelectedPageIndex; virtual;
    procedure UpdateScrollPositions; virtual;

    procedure DesignerModified;
    procedure CheckMargins; virtual;
    // Page navigation
    procedure InternalGoToFirstPage; virtual;
    procedure InternalGoToLastPage; virtual;
    procedure InternalGoToNextPage; virtual;
    procedure InternalGoToPrevPage; virtual;
    procedure ScrollPage(ADirection: TdxPreviewScrollDirection);

    function CanDrawBackground: Boolean;
    function CanSelectPage(APageIndex: Integer): Boolean; dynamic;
    function CanShowMarginHint: Boolean; dynamic;
    function CreateBackgroundRegion(APage: TdxPreviewPage): TcxRegion; virtual;
    procedure DoAfterDragMargin(AMargin: TdxPreviewPageMargin); dynamic;
    procedure DoBeforeDragMargin(AMargin: TdxPreviewPageMargin); dynamic;
    procedure DoCalcPageCount; dynamic;
    procedure DoChangePageCount; dynamic;
    procedure DoDragMargin(AMargin: TdxPreviewPageMargin); dynamic;
    procedure DoDrawPageContent(ACanvas: TcxCanvas; const R: TRect; APageIndex: Integer); dynamic;
    procedure DoGetPageNumberHintText(out AText: string); dynamic;
    procedure DoMarginChanged(AMargin: TdxPreviewPageMargin); dynamic;
    procedure DoZoomFactorChanged; dynamic;
    procedure DoZoomIn; dynamic;
    procedure DoZoomModeChanged; dynamic;
    procedure DoZoomOut; dynamic;
    procedure DoSelectedPageChanging; dynamic;
    procedure DoSelectedPageChanged; dynamic;

    procedure DrawContent(ACanvas: TcxCanvas; const R: TRect); virtual;
    procedure DrawMargins(DC: HDC);
    procedure DrawNoPages(ACanvas: TcxCanvas); virtual;
    procedure DrawPage(ACanvas: TcxCanvas; APage: TdxPreviewPage; ASelected: Boolean);
    procedure DrawPageBackground(ACanvas: TcxCanvas; APage: TdxPreviewPage; ASelected: Boolean); virtual;
    procedure DrawPages(ACanvas: TcxCanvas);
    procedure DrawViewerBackground(ACanvas: TcxCanvas; const R: TRect); virtual;
    procedure InvalidateMargins;
    // For CustomDraw
    function PageGetContentBounds(const APageBounds: TRect): TRect;
    function PageGetFooterBounds(const APageBounds: TRect): TRect;
    function PageGetHeaderBounds(const APageBounds: TRect): TRect;
    procedure SelectFirstPage; virtual;
    procedure SelectLastPage; virtual;
    procedure SelectNextPage; virtual;
    procedure SelectPrevPage; virtual;

    procedure LoadFromIniFile(AIniFile: TCustomIniFile; const ASectionName: string); virtual;
    procedure LoadFromRegistry(const ARegistryPath: string); virtual;
    procedure SaveToIniFile(AIniFile: TCustomIniFile; const ASectionName: string); virtual;
    procedure SaveToRegistry(const ARegistryPath: string); virtual;

    function DefaultNoPagesText: string; virtual;

    function IsUpdateLocked: Boolean;

    procedure InvalidatePage(APageIndex: Integer);
    procedure InvalidatePages;
    procedure InvalidatePagesContent;
    procedure InvalidatePagesFooter;
    procedure InvalidatePagesHeader;

    function GetHitInfoAt(const Pt: TPoint): TdxPreviewHitTests; overload;
    function GetHitInfoAt(const Pt: TSmallPoint): TdxPreviewHitTests; overload;
    function GetHitInfoAt(X, Y: Integer): TdxPreviewHitTests; overload;
    function GetPagesArea: TRect; virtual;
    procedure FullRefresh; virtual;
    procedure HideAllHints;
    procedure MakeVisible(APageIndex: Integer); virtual;
    procedure SetPageXYCount(XCount, YCount: Integer);

    function MarginFromPoint(const Pt: TPoint): TdxPreviewPageMargin; overload;
    function MarginFromPoint(const Pt: TSmallPoint): TdxPreviewPageMargin; overload;
    function MarginFromPoint(X, Y: Integer): TdxPreviewPageMargin; overload;
    function MarginValueToString(Value: Integer): string;

    function IndexOfPage(APage: TdxPreviewPage): Integer;
    function PageSizeToString: string;

    property AbsoluteIndent: Integer read GetAbsoluteIndent;
    property AbsoluteIndentLeft: Integer read FAbsoluteIndentLeft;
    property AbsoluteIndentRight: Integer read FAbsoluteIndentRight;
    property ActualZoomFactor: Integer read GetActualZoomFactor;
    property ContentHeight: Integer read GetContentHeight;
    property ContentWidth: Integer read GetContentWidth;
    property Margins: TdxPreviewPageMargins read FMargins write SetMargins;
    property PagesContentCache: TdxPreviewPageContentCachePool read FPagesContentCache;
    property PagesRows: TdxPreviewPagesRowList read FPagesRows;
    property State: TdxPreviewState read FState;
    property PrevZoomFactor: Integer read FPrevZoomFactor;
    property ActualMeasurementUnits: TdxPreviewMeasurementUnits read GetActualMeasurementUnits;
    property DefaultMeasurementUnits: TdxPreviewMeasurementUnits read FDefaultMeasurementUnits write FDefaultMeasurementUnits default pmuDefault;
    property MeasurementUnits: TdxPreviewMeasurementUnits read FMeasurementUnits write FMeasurementUnits default pmuDefault;

    property ClientHeight: Integer read GetClientHeight;
    property ClientWidth: Integer read GetClientWidth;
    property DraggingMargin: TdxPreviewPageMargin read FDraggingMargin;
    property Indent: Integer read GetIndent;

    property MarginColor: TColor read FMarginColor write SetMarginColor default clWindowText;
    property MaxZoomFactor: Integer read FMaxZoomFactor write SetMaxZoomFactor default 500;
    property MinZoomFactor: Integer read FMinZoomFactor write SetMinZoomFactor default 10;
    property NoPagesText: string read GetNoPagesText write SetNoPagesText stored IsNoPagesTextStored;

    property OptionsHint: TdxPreviewOptionsHint read FOptionsHint write SetOptionsHint default dxPreviewDefaultOptionsHint;
    property OptionsStore: TdxPreviewOptionsStore read FOptionsStore write SetOptionsStore default [posZoom];

    property InternalOptionsBehavior: TdxPreviewOptionsBehavior read FInternalOptionsBehavior write
      SetInternalOptionsBehavior default dxPreviewDefaultBehaviorOptions;
    property InternalOptionsView: TdxPreviewOptionsView read FInternalOptionsView write
      SetInternalOptionsView default dxPreviewDefaultOptionsView;
    property InternalOptionsZoom: TdxPreviewOptionsZoom read FInternalOptionsZoom write
      SetInternalOptionsZoom default dxPreviewDefaultOptionsZoom;

    property PageBorders: TRect read GetPageBorders;
    property PageList: TdxPreviewPageList read FPages;
    property Pages[Index: Integer]: TdxPreviewPage read GetPage;
    property PageSize: TdxPreviewPageSizeOptions read FPageSize write SetPageSize;
    property PageXCount: Integer read FPageXCount write SetPageXCount default 1;
    property PageYCount: Integer read FPageYCount write SetPageYCount default 1;
    property RegistryPath: string read FRegistryPath write FRegistryPath;

    property SelPage: TdxPreviewPage read GetSelPage;
    property SelPageIndex: Integer read FSelPageIndex write SetSelPageIndex;
    property SelPageSizeOptions: TdxPreviewPageSizeOptions read GetSelPageSizeOptions;

    property Zoomed: Boolean read FZoomed write SetZoomed;
    property ZoomFactor: Integer read FZoomFactor write SetZoomFactor stored True default 100;
    property ZoomMode: TdxPreviewZoomMode read FZoomMode write SetZoomMode default pzmNone; //Pages;
    property ZoomStep: Integer read FZoomStep write SetZoomStep default dxPreviewZoomStep;

    property OnAfterDragMargin: TdxMarginEvent read FOnAfterDragMargin write FOnAfterDragMargin;
    property OnBeforeDragMargin: TdxMarginEvent read FOnBeforeDragMargin write FOnBeforeDragMargin;
    property OnCalcPageCount: TNotifyEvent read FOnCalcPageCount write SetOnCalcPageCount;
    property OnDrawPageBackground: TdxPageBackgroundDrawEvent read FOnDrawPageBackground write FOnDrawPageBackground;
    property OnDragMargin: TdxMarginEvent read FOnDragMargin write FOnDragMargin;
    property OnDrawPageContent: TdxDrawPageContentEvent read FOnDrawPageContent write FOnDrawPageContent;
    property OnGetPageNumberHint: TdxGetPageNumberHintEvent read FOnGetPageNumberHint write FOnGetPageNumberHint;
    property OnCanShowMarginHint: TdxCanShowMarginHintEvent read FOnCanShowMarginHint write FOnCanShowMarginHint;
    property OnChangePageCount: TNotifyEvent read FOnChangePageCount write FOnChangePageCount;
    property OnMarginChanged: TdxMarginEvent read FOnMarginChanged write FOnMarginChanged;
    property OnPostDrawPageContent: TdxDrawPageContentEvent read FOnPostDrawPageContent write FOnPostDrawPageContent;
    property OnSelectedPageChanged: TdxPreviewPageEvent read FOnSelectedPageChanged write FOnSelectedPageChanged;
    property OnSelectedPageChanging: TdxPreviewPageEvent read FOnSelectedPageChanging write FOnSelectedPageChanging;
    property OnSelectingPage: TdxSelectingPageEvent read FOnSelectingPage write FOnSelectingPage;
    property OnZoomFactorChanged: TNotifyEvent read FOnZoomFactorChanged write FOnZoomFactorChanged;
    property OnZoomModeChanged: TNotifyEvent read FOnZoomModeChanged write FOnZoomModeChanged;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure BeginUpdate;
    procedure CancelUpdate;
    procedure EndUpdate;
    procedure GoToFirstPage;
    procedure GoToLastPage;
    procedure GoToNextPage;
    procedure GoToPrevPage;
    procedure ZoomIn;
    procedure ZoomOut;

    property PageCount: Integer read GetPageCount write SetPageCount;
  end;

const
  phtMargins: TdxPreviewHitTests = [phtMarginLeft..phtMarginFooter];
  phtMarginsHorz: TdxPreviewHitTests = [phtMarginTop, phtMarginBottom, phtMarginHeader];
  phtMarginsVert: TdxPreviewHitTests = [phtMarginLeft, phtMarginRight];

procedure dxDrawDefaultPagePreview(const ACanvas: TCanvas; const AContentBounds: TRect;
  ACenterHorizontally, ACenterVertically: Boolean; AScaleFactor: TdxScaleFactor = nil);
implementation

{$R dxCustomPreview.res}

uses
  Themes, Math, Registry, dxCustomPreviewStrs, dxDPIAwareUtils, dxPrintUtils, dxMeasurementUnits, dxTypeHelpers;

type
  TFloat = Extended;

const
  { strings used when saving(loading) properties to(from) registry }
  sdxOptionsBehavior = 'OptionsBehavior';            // Don't localize
  sdxOptionsHint = 'OptionHint';                     // Don't localize
  sdxOptionsView = 'OptionView';                     // Don't localize
  sdxOptionsZoom = 'OptionZoom';                     // Don't localize
  sdxZoomFactor = 'ZoomFactor';                      // Don't localize
  sdxZoomStep = 'ZoomStep';                          // Don't localize
  sdxZoomMode = 'ZoomMode';                          // Don't localize
  sdxPageXCount = 'PageXCount';                      // Don't localize
  sdxPageYCount = 'PageYCount';                      // Don't localize
  sdxMarginColor = 'MarginColor';                    // Don't localize
  sdxMeasurementUnits = 'MeasurementUnits';          // Don't localize
  sdxOrientation = 'Orientation';                    // Don't localize

  NullDraggingPos = MinInt;
  dxShowHintTimerID = 1;
  dxHideHintTimerID = 2;

  dxPreviewHideHintShortTime = 500;
  dxPreviewHideHintLongTime = 10000;
  dxPreviewShowHintTime = 500;
  dxPreviewAbsoluteIndent = 4;

const
  IDC_DXPREVIEW_MARGINSMOVEHORZ = 'IDC_DXPREVIEW_MARGINSMOVEHORZ';
  IDC_DXPREVIEW_MARGINSMOVEVERT = 'IDC_DXPREVIEW_MARGINSMOVEVERT';
  IDC_DXPREVIEW_ZOOMIN = 'IDC_DXPREVIEW_ZOOMIN';
  IDC_DXPREVIEW_ZOOMOUT = 'IDC_DXPREVIEW_ZOOMOUT';

type
  { TdxPreviewHintWindow }

  TdxPreviewHintWindow = class(TCustomControl)
  private
    FirstPos: TPoint;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Paint; override;

    function GetDragHintOffset: Integer; virtual;

    procedure DrawBorder(DC: HDC; R: TRect);
    procedure DrawText(DC: HDC; const R: TRect);

    property DragHintOffset: Integer read GetDragHintOffset;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ActivateHint(P: TPoint; const AHint: string; Margin: TdxPreviewPageMargin);
  end;

var
  FLongShowHintTime: DWORD;

  crdxPreviewHorzResize: TCursor;
  crdxPreviewVertResize: TCursor;
  crdxPreviewZoomIn: TCursor;
  crdxPreviewZoomOut: TCursor;

procedure dxDrawDefaultPagePreview(const ACanvas: TCanvas; const AContentBounds: TRect;
  ACenterHorizontally, ACenterVertically: Boolean; AScaleFactor: TdxScaleFactor = nil);
var
  ALineBounds: TRect;
  ALineColor: TdxHSL;
  ALineCountX: Integer;
  ALineCountY: Integer;
  ALineStepX: Integer;
  ALineStepY: Integer;
  ARect: TRect;
  I: Integer;
begin
  ARect := AContentBounds;
  ALineStepX := 15;
  ALineStepY := 7;
  if AScaleFactor <> nil then
  begin
    ALineStepX := AScaleFactor.Apply(ALineStepX);
    ALineStepY := AScaleFactor.Apply(ALineStepY);
  end;
  ALineCountX := (ARect.Right - ARect.Left - 1) div ALineStepX;
  ALineCountY := (ARect.Bottom - ARect.Top - 1) div ALineStepY;
  ARect.Right := ARect.Left + ALineCountX * ALineStepX;
  ARect.Bottom := ARect.Top + ALineCountY * ALineStepY;

  if ACenterHorizontally then
    OffsetRect(ARect, (AContentBounds.Width - ARect.Width) div 2, 0);
  if ACenterVertically then
    OffsetRect(ARect, 0, (AContentBounds.Height - ARect.Height) div 2);

  ALineColor := TdxColorSpaceConverter.ColorToHSL(ColorToRGB(clWindow));
  ALineColor.L := IfThen(ALineColor.L > 0.5, ALineColor.L - 0.1, ALineColor.L + 0.1);
  ACanvas.Brush.Color := TdxColorSpaceConverter.HSLToColor(ALineColor);

  for I := 0 to ALineCountX do
  begin
    ALineBounds := Rect(ARect.Left + I * ALineStepX, ARect.Top, ARect.Left + I * ALineStepX + 1, ARect.Bottom);
    if RectVisible(ACanvas.Handle, ALineBounds) then
      ACanvas.FillRect(ALineBounds);
  end;

  for I := 0 to ALineCountY do
  begin
    ALineBounds := Rect(ARect.Left, ARect.Top + I * ALineStepY, ARect.Right + 1, ARect.Top + I * ALineStepY + 1);
    if RectVisible(ACanvas.Handle, ALineBounds) then
      ACanvas.FillRect(ALineBounds);
  end;
end;

function DropAmpersand(const Source: string): string;
begin
  Result := StringReplace(Source, '&', '', [rfReplaceAll]);
end;

function GetDefaultMeasurementUnits: TdxPreviewMeasurementUnits;
const
  Map: array[TdxMeasurementUnits] of TdxPreviewMeasurementUnits = (pmuDefault, pmuInches, pmuMillimeters);
begin
  Result := Map[dxGetDefaultMeasurementUnits];
end;

function LoMetricToAnother(AUnits: TdxPreviewMeasurementUnits; Value: Integer): TFloat;
begin
  if AUnits = pmuDefault then
    AUnits := GetDefaultMeasurementUnits;

  case AUnits of
    pmuInches:
      Result := Value / 254;
    pmuMillimeters:
      Result := Value / 10;
    pmuCentimeters:
      Result := Value / 100;
    pmuPoints:
      Result := Value * 72 / 254;
  else { pmuPicas}
    Result := Value * 6 / 254;
  end;
end;

function MinMax(const AValue, AMinValue, AMaxValue: Integer): Integer;
begin
  if AMaxValue >= AMinValue then
    Result := Min(AMaxValue, Max(AValue, AMinValue))
  else
    Result := AMaxValue;
end;

function MaxMin(const AValue, AMinValue, AMaxValue: Integer): Integer;
begin
  Result := Max(AMinValue, Min(AValue, AMaxValue));
end;

{ TdxPreviewHintWindow }

constructor TdxPreviewHintWindow.Create(AOwner: TComponent);
var
  AMetrics: TNonClientMetrics;
begin
  inherited Create(AOwner);
  if dxSystemInfo.GetParameter(SPI_GETNONCLIENTMETRICS, AMetrics) then
    Canvas.Font.Handle := CreateFontIndirect(AMetrics.lfStatusFont)
  else
    Canvas.Font.Size := 8;
end;

procedure TdxPreviewHintWindow.ActivateHint(
  P: TPoint; const AHint: string; Margin: TdxPreviewPageMargin);
var
  R: TRect;
  AWidth, AHeight: Integer;
begin
  Application.CancelHint;
  Caption := AHint;

  R := Rect(0, 0, Screen.Width, 0);
  Windows.DrawText(Canvas.Handle, PChar(AHint), Length(AHint), R,
    DT_CALCRECT or DT_LEFT or DT_NOPREFIX);
  Inc(R.Right, 2 * (1 + 2));
  Inc(R.Bottom, 2 * (1 + 2));
  AWidth := R.Right;
  AHeight := R.Bottom;
  if IsWindowVisible(Handle) then
  begin
    if AWidth < Width then AWidth := Width;
    if AHeight < Height then AHeight := Height;
  end
  else
    FirstPos := P;

  if Assigned(Margin) then
  begin
    if Margin.IsForward then
      if Margin.IsVertical then
        OffsetRect(R, -(DragHintOffset + AWidth), DragHintOffset)
      else
        OffsetRect(R, DragHintOffset, -(DragHintOffset + AHeight))
    else
      OffsetRect(R, DragHintOffset, DragHintOffset);
  end
  else {scroll bar hint}
    OffsetRect(R, -(GetSystemMetrics(SM_CXVSCROLL) + AWidth), 0);

  OffsetRect(R, FirstPos.X, FirstPos.Y);

  with R do
  begin
    if Right > Screen.Width then
      OffsetRect(R, Screen.Width - Right, 0);
    if Bottom > Screen.Height then
      OffsetRect(R, 0, Screen.Height - Bottom);
    if Left < 0 then
      OffsetRect(R, -Left, 0);
    if Top < 0 then
      OffsetRect(R, 0, -Top);
  end;

  if IsWindowVisible(Handle) then
  begin
    if (Width <> AWidth) or (Height <> AHeight) then
      ShowWindow(Handle, SW_HIDE)
    else
    begin
      InvalidateRect(Handle, nil, False);
      UpdateWindow(Handle);
    end;
  end;

  if not IsWindowVisible(Handle) then
    SetWindowPos(Handle, HWND_TOPMOST, R.Left, R.Top, AWidth, AHeight,
      SWP_SHOWWINDOW or SWP_NOACTIVATE);
end;

procedure TdxPreviewHintWindow.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result := 1;
end;

procedure TdxPreviewHintWindow.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := WS_POPUP or WS_DISABLED;
    WindowClass.Style := WindowClass.Style or CS_SAVEBITS;
    if IsWinXPOrLater then
      WindowClass.Style := WindowClass.Style or CS_DROPSHADOW;
    ExStyle := WS_EX_TOOLWINDOW;
  end;
end;

procedure TdxPreviewHintWindow.Paint;
var
  R: TRect;
begin
  R := ClientRect;
  DrawBorder(Canvas.Handle, R);
  InflateRect(R, -1, -1);
  DrawText(Canvas.Handle, R);
end;

function TdxPreviewHintWindow.GetDragHintOffset: Integer;
const
  dxDragHintOffset = 5;
begin
  Result := dxDragHintOffset;
end;

procedure TdxPreviewHintWindow.DrawBorder(DC: HDC; R: TRect);
begin
  if cxIsVCLThemesEnabled then
  begin
    dxDrawThemeEdge(DC, cxStyleServices.GetElementDetails(twWindowRoot), R, BDR_RAISEDOUTER, BF_RECT);
    Exit;
  end;
  DrawEdge(DC, R, BDR_RAISEDOUTER, BF_RECT);
end;

procedure TdxPreviewHintWindow.DrawText(DC: HDC; const R: TRect);
begin
  SetTextColor(DC, GetSysColor(COLOR_INFOTEXT));
  SetBkColor(DC, GetSysColor(COLOR_INFOBK));
  ExtTextOut(DC, R.Left + 2, R.Top + 2, ETO_OPAQUE, @R, PChar(Caption), Length(Caption), nil);
end;

{ TdxPreviewPageSizeOptions }

constructor TdxPreviewPageSizeOptions.Create(APreview: TdxCustomPreview);
begin
  inherited Create;
  FAssigned := True;
  FMinUsefulSize := GetDefaultMinUsefulSize;
  FPreview := APreview;
end;

constructor TdxPreviewPageSizeOptions.Create(APreview: TdxCustomPreview; const AMaster: TdxPreviewPageSizeOptions);
begin
  Create(APreview);
  FMaster := AMaster;
  FAssigned := False;
end;

constructor TdxPreviewPageSizeOptions.Create(APreview: TdxCustomPreview; const ADefaultSize: TPoint);
begin
  Create(APreview);
  FSize := ADefaultSize;
end;

procedure TdxPreviewPageSizeOptions.Assign(Source: TPersistent);
begin
  if Source is TdxPreviewPageSizeOptions then
  begin
    Size := TdxPreviewPageSizeOptions(Source).Size;
    Margins := TdxPreviewPageSizeOptions(Source).Margins;
    Orientation := TdxPreviewPageSizeOptions(Source).Orientation;
    MinUsefulSize := TdxPreviewPageSizeOptions(Source).MinUsefulSize;
    Assigned := TdxPreviewPageSizeOptions(Source).Assigned;
  end;
end;

function TdxPreviewPageSizeOptions.AllowAutoSwapMargins: Boolean;
begin
  Result := pobAutoSwapMargins in FPreview.InternalOptionsBehavior;
end;

function TdxPreviewPageSizeOptions.GetActualSizeInPixels: TPoint;
begin
  Result := LoMetricToPixels(ActualSize);
end;

function TdxPreviewPageSizeOptions.GetDefaultMinUsefulSize: TPoint;
begin
  Result := dxPreviewMinUsefulSize;
end;

procedure TdxPreviewPageSizeOptions.Changed;
begin
  dxCallNotify(OnChange, Self);
end;

function TdxPreviewPageSizeOptions.GetActualSize: TPoint;
begin
  Result := Size;
  if Orientation = ppoLandscape then
    Result := cxPoint(Result.y, Result.x)
end;

function TdxPreviewPageSizeOptions.GetFooter: Integer;
begin
  if Assigned then
    Result := FFooter
  else
    Result := Master.Footer;
end;

function TdxPreviewPageSizeOptions.GetHeader: Integer;
begin
  if Assigned then
    Result := FHeader
  else
    Result := Master.Header;
end;

function TdxPreviewPageSizeOptions.GetMargins: TRect;
begin
  if Assigned then
    Result := FMargins
  else
    Result := Master.Margins;
end;

function TdxPreviewPageSizeOptions.GetMarginsMinValues: TRect;
begin
  if Assigned then
    Result := FMarginsMinValues
  else
    Result := Master.MarginsMinValues;
end;

function TdxPreviewPageSizeOptions.GetMinUsefulSize: TPoint;
begin
  if Assigned then
    Result := FMinUsefulSize
  else
    Result := Master.MinUsefulSize;
end;

function TdxPreviewPageSizeOptions.GetOrientation: TdxPreviewPaperOrientation;
begin
  if Assigned then
    Result := FOrientation
  else
    Result := Master.Orientation;
end;

function TdxPreviewPageSizeOptions.GetSize: TPoint;
begin
  if Assigned then
    Result := FSize
  else
    Result := Master.Size;
end;

procedure TdxPreviewPageSizeOptions.SetAssigned(AValue: Boolean);
begin
  AValue := AValue or (Master = nil);
  if AValue <> FAssigned then
  begin
    FAssigned := AValue;
    if Assigned and (Master <> nil) then
      Assign(Master);
    Changed;
  end;
end;

procedure TdxPreviewPageSizeOptions.SetFooter(Value: Integer);
begin
  Value := Max(0, Value);
  if Value <> FFooter then
  begin
    Assigned := True;
    FFooter := Value;
    Changed;
  end;
end;

procedure TdxPreviewPageSizeOptions.SetHeader(Value: Integer);
begin
  Value := Max(0, Value);
  if Value <> FHeader then
  begin
    Assigned := True;
    FHeader := Value;
    Changed;
  end;
end;

procedure TdxPreviewPageSizeOptions.SetMargins(const Value: TRect);
begin
  if not cxRectIsEqual(Value, FMargins) then
  begin
    Assigned := True;
    FMargins := Value;
    Changed;
  end;
end;

procedure TdxPreviewPageSizeOptions.SetMarginsMinValues(const Value: TRect);
begin
  if not cxRectIsEqual(Value, FMarginsMinValues) then
  begin
    Assigned := True;
    FMarginsMinValues := Value;
    Changed;
  end;
end;

procedure TdxPreviewPageSizeOptions.SetMinUsefulSize(const Value: TPoint);
begin
  FMinUsefulSize.X := MinMax(Value.X, 0, ActualSize.x - Margins.Left - Margins.Right);
  FMinUsefulSize.Y := MinMax(Value.Y, 0, ActualSize.y - Margins.Top - Margins.Bottom);
end;

procedure TdxPreviewPageSizeOptions.SetOrientation(AValue: TdxPreviewPaperOrientation);
begin
  if AValue <> FOrientation then
  begin
    Assigned := True;
    FOrientation := AValue;
    if AllowAutoSwapMargins then
      RotateMargins;
    Changed;
  end;
end;

procedure TdxPreviewPageSizeOptions.SetSize(const Value: TPoint);
begin
  if not cxPointIsEqual(FSize, Value) then
  begin
    Assigned := True;
    FSize := cxPoint(Max(Value.X, FMinUsefulSize.X), Max(Value.Y, FMinUsefulSize.Y));
    Changed;
  end;
end;

procedure TdxPreviewPageSizeOptions.RotateMargins;
var
  ATemp1: Integer;
  ATemp2: Integer;
begin
  FPreview.BeginUpdate;
  try
    if Orientation = ppoLandscape then
    begin
      ATemp1 := Margins.Left;
      FMargins.Left := Margins.Bottom;
      ATemp2 := Margins.Top;
      FMargins.Top := ATemp1;
      ATemp1 := Margins.Right;
      FMargins.Right := ATemp2;
      FMargins.Bottom := ATemp1;
    end
    else
    begin
      ATemp1 := Margins.Bottom;
      FMargins.Bottom := Margins.Left;
      ATemp2 := Margins.Right;
      FMargins.Right := ATemp1;
      ATemp1 := Margins.Top;
      FMargins.Top := ATemp2;
      FMargins.Left := ATemp1;
    end;
  finally
    FPreview.EndUpdate;
  end;
end;

{ TdxPreviewPage }

constructor TdxPreviewPage.Create(APreview: TdxCustomPreview);
begin
  inherited Create;
  Assert(APreview <> nil);
  FPreview := APreview;
  FPageSize := GetPageSizeOptionsClass.Create(APreview, APreview.PageSize);
  FPageSize.OnChange := APreview.PageParametersChanged;
end;

destructor TdxPreviewPage.Destroy;
begin
  Preview.PagesContentCache.Remove(Self);
  FreeAndNil(FPageSize);
  inherited Destroy;
end;

procedure TdxPreviewPage.Draw(ACanvas: TcxCanvas);
var
  ACache: TdxPreviewPageContentCache;
begin
  ACache := Preview.PagesContentCache.Add(Self);
  if ACache.Dirty then
  begin
    ACache.Canvas.Lock;
    try
      ACache.Dirty := False;
      ACache.cxCanvas.WindowOrg := Bounds.TopLeft;
      Preview.DrawPageBackground(ACache.cxCanvas, Self, Selected);
      ACache.cxCanvas.WindowOrg := cxNullPoint;
      Preview.DoDrawPageContent(ACache.cxCanvas, ACache.ClientRect, Index);
    finally
      ACache.Canvas.Unlock;
    end;
  end;
  cxBitBlt(ACanvas.Handle, ACache.Canvas.Handle, Bounds, cxNullPoint, SRCCOPY);
end;

function TdxPreviewPage.GetCursor: TCursor;
begin
  if Zoomed then
    Result := crdxPreviewZoomOut
  else
    Result := crdxPreviewZoomIn;
end;

function TdxPreviewPage.HasPoint(const Pt: TPoint): Boolean;
var
  R: TRect;
begin
  Result := IntersectRect(R, SiteBounds, Preview.ClientRect) and PtInRect(R, Pt);
end;

function TdxPreviewPage.HasPoint(X, Y: Integer): Boolean;
begin
  Result := HasPoint(Point(X, Y));
end;

procedure TdxPreviewPage.Invalidate;
begin
  Preview.PagesContentCache.Invalidate(Self);
  Preview.InvalidateRect(SiteBounds, False);
end;

procedure TdxPreviewPage.MakeVisible;
begin
  Preview.MakeVisible(Index);
end;

function TdxPreviewPage.GetPageSizeOptionsClass: TdxPreviewPageSizeOptionsClass;
begin
  Result := TdxPreviewPageSizeOptions;
end;

function TdxPreviewPage.GetVisible: Boolean;
var
  R: TRect;
begin
  Result := IntersectRect(R, Bounds, Preview.ClientRect) and EqualRect(R, Bounds);
end;

function TdxPreviewPage.GetIndex: Integer;
begin
  Result := Preview.IndexOfPage(Self);
end;

function TdxPreviewPage.GetLargerPartVisible: Boolean;
begin
  Result := PartVisible and (Bounds.Bottom >= (Preview.ClientRect.Bottom div 2));
end;

function TdxPreviewPage.GetPartVisible: Boolean;
var
  R: TRect;
begin
  Result := IntersectRect(R, Bounds, Preview.ClientRect);
end;

function TdxPreviewPage.GetSelected: Boolean;
begin
  Result := Preview.SelPageIndex = Index;
end;

function TdxPreviewPage.GetSiteBounds: TRect;
begin
  Result := cxRectInflate(Bounds, Preview.PageBorders);
end;

function TdxPreviewPage.GetZoomed: Boolean;
begin
  Result := Selected and Preview.Zoomed;
end;

procedure TdxPreviewPage.SetSelected(Value: Boolean);
begin
  Preview.SelPageIndex := Index;
end;

{ TdxPreviewPagesRowList }

function TdxPreviewPagesRowList.Add(StartIndex, FinishIndex: Integer; const Bounds: TRect): TdxPreviewPagesRow;
begin
  Result := TdxPreviewPagesRow.Create;
  Result.FinishIndex := FinishIndex;
  Result.StartIndex := StartIndex;
  Result.Bounds := Bounds;
  inherited Add(Result);
end;

procedure TdxPreviewPagesRowList.OffsetBounds(X, Y: Integer);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    OffsetRect(Items[I].Bounds, X, Y);
end;

{ TdxPreviewPageList }

procedure TdxPreviewPageList.GetPartVisiblePageRanges(AStartIndex, AEndIndex: PInteger);
var
  StartIndex, EndIndex: Integer;
begin
  GetPartVisiblePageRanges(StartIndex, EndIndex);

  if AStartIndex <> nil then
    AStartIndex^ := StartIndex;

  if AEndIndex <> nil then
    AEndIndex^ := EndIndex;
end;

procedure TdxPreviewPageList.GetPartVisiblePageRanges(out AStartIndex, AEndIndex: Integer);
begin
  AEndIndex := -1;
  AStartIndex := -1;
  if Count > 0 then
  begin
    AStartIndex := 0;
    while (AStartIndex < Count) and not Items[AStartIndex].PartVisible do
      Inc(AStartIndex);

    if AStartIndex = Count then
    begin
      AStartIndex := -1;
      AEndIndex := -1;
    end
    else
    begin
      AEndIndex := AStartIndex;
      while (AEndIndex < Count) and Items[AEndIndex].PartVisible do
        Inc(AEndIndex);
      Dec(AEndIndex);
    end;
  end;
end;

procedure TdxPreviewPageList.GetVisiblePageRanges(AStartIndex, AEndIndex: PInteger);
var
  StartIndex, EndIndex: Integer;
begin
  GetVisiblePageRanges(StartIndex, EndIndex);

  if AStartIndex <> nil then
    AStartIndex^ := StartIndex;

  if AEndIndex <> nil then
    AEndIndex^ := EndIndex;
end;

procedure TdxPreviewPageList.GetVisiblePageRanges(out AStartIndex, AEndIndex: Integer);
begin
  if Count <> 0 then
  begin
    AStartIndex := 0;
    while (AStartIndex < Count) and not Items[AStartIndex].Visible do
      Inc(AStartIndex);

    if AStartIndex = Count then
    begin
      AStartIndex := -1;
      AEndIndex := -1;
    end
    else
    begin
      AEndIndex := AStartIndex;
      while (AEndIndex < Count) and Items[AEndIndex].Visible do
        Inc(AEndIndex);
      Dec(AEndIndex);
    end;
  end
  else
  begin
    AStartIndex := -1;
    AEndIndex := -1;
  end;
end;

procedure TdxPreviewPageList.GetVisibleSelectedPageIndex(out AIndex: Integer);
begin
  AIndex := -1;
  if Count > 0 then
  begin
    AIndex := 0;
    while (AIndex < Count) and not Items[AIndex].LargerPartVisible do
      Inc(AIndex);
  end;
end;

procedure TdxPreviewPageList.Invalidate;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].Invalidate;
end;

procedure TdxPreviewPageList.OffsetBounds(X, Y: Integer);
begin
  OffsetBounds(X, Y, 0, Count - 1);
end;

procedure TdxPreviewPageList.OffsetBounds(X, Y, AStartIndex, AFinishIndex: Integer);
var
  I: Integer;
begin
  for I := AStartIndex to AFinishIndex do
    Items[I].Bounds := cxRectOffset(Items[I].Bounds, X, Y);
end;

function TdxPreviewPageList.PageIndexFromPoint(const Pt: TPoint): Integer;
begin
  for Result := 0 to Count - 1 do
    if Items[Result].HasPoint(Pt) then
      Exit;

  Result := -1;
end;

function TdxPreviewPageList.PageIndexFromPoint(const Pt: TSmallPoint): Integer;
begin
  Result := PageIndexFromPoint(SmallPointToPoint(Pt));
end;

function TdxPreviewPageList.PageIndexFromPoint(X, Y: Integer): Integer;
begin
  Result := PageIndexFromPoint(Point(X, Y));
end;

function TdxPreviewPageList.GetBounds: TRect;
begin
  Result := GetBounds(0, Count - 1);
end;

function TdxPreviewPageList.GetBounds(AStartIndex, AFinishIndex: Integer): TRect;
var
  I: Integer;
begin
  Result := cxNullRect;
  if AStartIndex <= AFinishIndex then
  begin
    Result := Items[AStartIndex].Bounds;
    for I := AStartIndex + 1 to AFinishIndex do
      Result := cxRectUnion(Result, Items[I].Bounds);
  end;
end;

procedure TdxPreviewPageList.CenterPagesInRow(ARow: TdxPreviewPagesRow; AMaxWidth: Integer);
var
  ADelta: Integer;
  APage: TdxPreviewPage;
  I: Integer;
begin
  ADelta := Max((AMaxWidth - cxRectWidth(ARow.Bounds)) div 2, 0);
  for I := ARow.StartIndex to ARow.FinishIndex do
  begin
    APage := Items[I];
    APage.Bounds := cxRectOffset(APage.Bounds, ADelta, (cxRectHeight(ARow.Bounds) - cxRectHeight(APage.Bounds)) div 2);
  end
end;

{ TdxPreviewPageMargin }

constructor TdxPreviewPageMargin.Create(AMargins: TdxPreviewPageMargins);
begin
  inherited Create;
  FMargins := AMargins;
  FDraggingPos := NullDraggingPos;
  FEnabled := True;
  FMaxValue := -1;
  FVisible := True;
end;

procedure TdxPreviewPageMargin.Assign(Source: TPersistent);
begin
  if Source is TdxPreviewPageMargin then
  begin
    BeginUpdate;
    try
      DoAssign(TdxPreviewPageMargin(Source));
    finally
      EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;

procedure TdxPreviewPageMargin.RestoreDefaults;
begin
  BeginUpdate;
  try
    DoRestoreDefaults;
  finally
    EndUpdate;
  end;
end;

procedure TdxPreviewPageMargin.BeginUpdate;
begin
  if Margins <> nil then
    Margins.BeginUpdate;
end;

procedure TdxPreviewPageMargin.EndUpdate;
begin
  if Margins <> nil then
    Margins.EndUpdate;
end;

function TdxPreviewPageMargin.DefaultCaption: string;
begin
  Result := '';
end;

function TdxPreviewPageMargin.DefaultValue: Integer;
begin
  Result := 0;
end;

function TdxPreviewPageMargin.GetHitTest: TdxPreviewHitTest;
begin
  Result := phtPage;
end;

function TdxPreviewPageMargin.GetCursor: TCursor;
begin
  Result := crDefault;
end;

function TdxPreviewPageMargin.HasPoint(const Pt: TPoint): Boolean;
begin
  Result := Visible and Enabled and PtInRect(SelectableBounds, Pt);
end;

function TdxPreviewPageMargin.HasPoint(const Pt: TSmallPoint): Boolean;
begin
  Result := HasPoint(SmallPointToPoint(Pt));
end;

function TdxPreviewPageMargin.HasPoint(X, Y: Integer): Boolean;
begin
  Result := HasPoint(Point(X, Y));
end;

procedure TdxPreviewPageMargin.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('IsCaptionAssigned', ReadIsCaptionAssigned, WriteIsCaptionAssigned,
    FIsCaptionAssigned and (FCaption = ''));
end;

function TdxPreviewPageMargin.GetIsForceRecalculatePageCount: Boolean;
begin
  Result := False;
end;

function TdxPreviewPageMargin.GetIsForward: Boolean;
begin
  Result := False;
end;

function TdxPreviewPageMargin.GetIsVertical: Boolean;
begin
  Result := False;
end;

function TdxPreviewPageMargin.GetSelectDelta: Integer;
begin
  Result := Preview.ScaleFactor.Apply(dxPreviewMarginSelectDelta);
end;

function TdxPreviewPageMargin.DoGetActualMaxValue: Integer;
begin
  Result := MaxValue;
end;

function TdxPreviewPageMargin.DoGetActualMinValue: Integer;
begin
  Result := MinValue;
end;

function TdxPreviewPageMargin.DoGetMaxMinValue: Integer;
begin
  Result := 0;
end;

function TdxPreviewPageMargin.DoPosFromValue(AValue: Integer): Integer;
begin
  Result := AValue;
end;

function TdxPreviewPageMargin.DoValueFromPos(APos: Integer): Integer;
begin
  Result := APos;
end;

procedure TdxPreviewPageMargin.Changed(HardRefresh: Boolean);
begin
  if Assigned(Margins) then
  begin
    if HardRefresh then
      Margins.Update(nil)
    else
      Margins.Update(Self)
  end;
end;

procedure TdxPreviewPageMargin.DoAssign(Source: TdxPreviewPageMargin);
begin
  Enabled := Source.Enabled;
  MaxValue := Source.MaxValue;
  MinValue := Source.MinValue;
  Value := Source.Value;
  Visible := Source.Visible;
end;

procedure TdxPreviewPageMargin.DoDragAfter;
begin
  if Preview <> nil then
  begin
    Preview.DestroyHint;
    DeleteObject(FScreenBitmap);
    Invert(Preview.Canvas.Handle);
    Preview.DoAfterDragMargin(Self);
  end;
end;

procedure TdxPreviewPageMargin.DoDragBefore;
var
  DC: HDC;
begin
  if Preview <> nil then
  begin
    Preview.DoBeforeDragMargin(Self);

    DC := GetDC(Preview.Handle);
    try
      Invert(DC);
      with PageBounds do
        if IsVertical then
          FScreenBitmap := CreateCompatibleBitmap(DC, 1, Bottom - Top)
        else
          FScreenBitmap := CreateCompatibleBitmap(DC, Right - Left, 1);
    finally
      ReleaseDC(Preview.Handle, DC);
    end;

    with Preview do
      if pohShowOnDrag in OptionsHint then
        CreateHint
      else
        DestroyHint;
  end;
end;

procedure TdxPreviewPageMargin.DoRestoreDefaults;
begin
  Enabled := True;
  MaxValue := -1;
  MinValue := 0;
  Value := DefaultValue;
  Visible := True;
  FIsCaptionAssigned := False;
end;

procedure TdxPreviewPageMargin.Draw(DC: HDC);
var
  R: TRect;
  Pen: HPEN;
begin
  if Preview <> nil then
  begin
    R := Bounds;
    Pen := SelectObject(DC, Preview.FMarginPen);
    SetBkMode(DC, Windows.TRANSPARENT);
    try
      if IsVertical then
      begin
        MoveToEx(DC, R.Left, R.Top, nil);
        LineTo(DC, R.Left, R.Bottom);
      end
      else
      begin
        MoveToEx(DC, R.Left, R.Top, nil);
        LineTo(DC, R.Right, R.Top);
      end;
    finally
      SetBkMode(DC, Windows.OPAQUE);
      SelectObject(DC, Pen);
    end;
  end;
end;

procedure TdxPreviewPageMargin.Invalidate;
var
  R: TRect;
begin
  if (Preview <> nil) and Preview.HandleAllocated then
  begin
    R := Bounds;
    if IsVertical then
      Inc(R.Right)
    else
      Inc(R.Bottom);

    InvalidateRect(Preview.Handle, @R, False);
  end;
end;

procedure TdxPreviewPageMargin.Invert(DC: HDC);
begin
  with Bounds do
    BitBlt(DC, Left, Top, Right - Left + Ord(IsVertical),
      Bottom - Top + Ord(not IsVertical), 0, 0, 0, DSTINVERT);
end;

function TdxPreviewPageMargin.CheckValue(Value: Integer): Integer;
begin
  Result := MinMax(Value, ActualMinValue, ActualMaxValue);
end;

function TdxPreviewPageMargin.PosFromValue(AValue: Integer): Integer;
begin
  Result := LoMetricToPixels(AValue);
  if Preview <> nil then
    Result := MulDiv(Result, Preview.ActualZoomFactor, 100);
  Result := DoPosFromValue(Result);
end;

function TdxPreviewPageMargin.ValueFromPos(APos: Integer): Integer;
begin
  if APos = MinPos then
  begin
    if IsForward then
      Result := ActualMinValue
    else
      Result := ActualMaxValue;
  end
  else
    if APos = MaxPos then
    begin
      if IsForward then
        Result := ActualMaxValue
      else
        Result := ActualMinValue;
    end
    else
      if APos = PosFromValue(FValue) then
        Result := FValue
      else
      begin
        Result := DoValueFromPos(APos);
        if Preview <> nil then
          Result := PixelsToLoMetric(MulDiv(Result, 100, Preview.ActualZoomFactor));
        CheckValue(Result);
      end;
end;

function TdxPreviewPageMargin.GetActualMaxValue: Integer;
begin
  if (Preview <> nil) and Preview.CanChangeMargins then
  begin
    Result := DoGetActualMaxValue;
    if (FMaxValue <> -1) and (Result > FMaxValue) then
      Result := FMaxValue;
  end
  else
    Result := FMaxValue;
end;

function TdxPreviewPageMargin.GetActualMinValue: Integer;
begin
  if (Preview <> nil) and Preview.CanChangeMargins then
    Result := Max(MinValue, DoGetActualMinValue)
  else
    Result := MinValue;
end;

function TdxPreviewPageMargin.GetActualPageSize: TdxPreviewPageSizeOptions;
begin
  Result := Preview.SelPageSizeOptions;
end;

function TdxPreviewPageMargin.GetActualValue: Integer;
begin
  Result := LoMetricToPixels(Value);
  if Assigned(Preview) then
    Result := MulDiv(Result, Preview.ActualZoomFactor, 100);
end;

function TdxPreviewPageMargin.GetBounds: TRect;
var
  AValue: Integer;
begin
  if IsDragging and IsDraggingPosAssigned then
    AValue := DraggingPos
  else
    AValue := PosFromValue(Value);

  Result := PageBounds;
  if IsVertical then
    Result := Rect(AValue, PageBounds.Top, AValue, PageBounds.Bottom)
  else
    Result := Rect(PageBounds.Left, AValue, PageBounds.Right, AValue);
end;

function TdxPreviewPageMargin.GetCaption: string;
begin
  if FIsCaptionAssigned then
    Result := FCaption
  else
    Result := DefaultCaption;
end;

function TdxPreviewPageMargin.GetDisplayText: string;
begin
  Result := Caption;
  if Preview <> nil then
  begin
    Result := Result + ': ';
    if IsDragging then
      Result := Result + Preview.MarginValueToString(DraggingValue)
    else
      Result := Result + Preview.MarginValueToString(Value);
  end;
end;

function TdxPreviewPageMargin.GetDraggingValue: Integer;
begin
  if IsDraggingPosAssigned then
    Result := ValueFromPos(FDraggingPos)
  else
    Result := -1;
end;

function TdxPreviewPageMargin.GetIsDragging: Boolean;
begin
  Result := (Preview <> nil) and (Preview.DraggingMargin = Self);
end;

function TdxPreviewPageMargin.GetIsDraggingPosAssigned: Boolean;
begin
  Result := FDraggingPos <> NullDraggingPos;
end;

function TdxPreviewPageMargin.GetMaxPos: Integer;
begin
  if IsForward then
    Result := PosFromValue(ActualMaxValue)
  else
    Result := PosFromValue(ActualMinValue);
end;

function TdxPreviewPageMargin.GetMinPos: Integer;
begin
  if IsForward then
    Result := PosFromValue(ActualMinValue)
  else
    Result := PosFromValue(ActualMaxValue);
end;

function TdxPreviewPageMargin.GetPageBounds: TRect;
begin
  if Preview = nil then
    Result := cxNullRect
  else
    with Preview do
      if SelPageIndex > -1 then
        Result := Pages[SelPageIndex].Bounds
      else
      begin
        if PageCount > 0 then
          Result := Pages[0].Bounds
        else
          Result := cxNullRect
      end;
end;

function TdxPreviewPageMargin.GetViewer: TdxCustomPreview;
begin
  if Margins = nil then
    Result := nil
  else
    Result := Margins.Preview;
end;

function TdxPreviewPageMargin.GetSelectableBounds: TRect;
begin
  Result := Bounds;
  if IsVertical then
    InflateRect(Result, SelectDelta, 0)
  else
    InflateRect(Result, 0, SelectDelta);
end;

function TdxPreviewPageMargin.GetVisibleValue: Integer;
begin
  Result := LoMetricToPixels(Value);
  if Preview <> nil then
    Result := MulDiv(Result, Preview.ActualZoomFactor, 100);
end;

function TdxPreviewPageMargin.IsCaptionStored: Boolean;
begin
  Result := FIsCaptionAssigned and (FCaption <> DefaultCaption);
end;

function TdxPreviewPageMargin.IsValueStored: Boolean;
begin
  Result := Value <> DefaultValue;
end;

procedure TdxPreviewPageMargin.SetCaption(const Value: string);
begin
  if Caption <> Value then
  begin
    FCaption := Value;
    FIsCaptionAssigned := True;
  end;
end;

procedure TdxPreviewPageMargin.SetDraggingPos(Value: Integer);

  procedure SaveScreenImage(DC, BitmapDC: HDC);
  begin
    if IsDraggingPosAssigned then
    begin
      with PageBounds do
        if IsVertical then
          BitBlt(BitmapDC, 0, 0, 1, Bottom - Top, DC, FDraggingPos, Top, SRCCOPY)
        else
          BitBlt(BitmapDC, 0, 0, Right - Left, 1, DC, Left, FDraggingPos, SRCCOPY);
    end;
  end;

  procedure RestoreScreenImage(DC, BitmapDC: HDC);
  begin
    if IsDraggingPosAssigned then
    begin
      with PageBounds do
        if IsVertical then
          BitBlt(DC, FDraggingPos, Top, 1, Bottom - Top, BitmapDC, 0, 0, SRCCOPY)
        else
          BitBlt(DC, Left, FDraggingPos, Right - Left, 1, BitmapDC, 0, 0, SRCCOPY);
    end;
  end;

var
  DC, BitmapDC: HDC;
begin
  if Preview <> nil then
  begin
    if Value <> NullDraggingPos then
      Value := MinMax(Value, MinPos, MaxPos);

    if FDraggingPos <> Value then
    begin
      DC := GetDC(Preview.Handle);
      try
        BitmapDC := CreateCompatibleDC(DC);
        try
          FScreenBitmap := SelectObject(BitmapDC, FScreenBitmap);
          RestoreScreenImage(DC, BitmapDC);
          FDraggingPos := Value;
          SaveScreenImage(DC, BitmapDC);
          if IsDraggingPosAssigned then
          begin
            Preview.ActivateHint(Self);
            Draw(DC);
          end;
          FScreenBitmap := SelectObject(BitmapDC, FScreenBitmap);
        finally
          DeleteDC(BitmapDC);
        end;
      finally
        ReleaseDC(Preview.Handle, DC);
      end;

      if IsDraggingPosAssigned then
        Preview.DoDragMargin(Self);
    end;
  end;
end;

procedure TdxPreviewPageMargin.SetMaxValue(AValue: Integer);
begin
  AValue := Max(-1, AValue);
  if FMaxValue <> AValue then
  begin
    FMaxValue := AValue;
    if MaxValue <> -1 then
    begin
      Value := Min(Value, MaxValue);
      MinValue := Min(MinValue, MaxValue);
    end;
  end;
end;

procedure TdxPreviewPageMargin.SetMinValue(AValue: Integer);
begin
  AValue := Max(AValue, 0);
  if FMinValue <> AValue then
  begin
    if (Preview <> nil) and Preview.CanChangeMargins then
      AValue := Min(DoGetMaxMinValue, AValue);

    FMinValue := AValue;
    if (MaxValue <> -1) and (MinValue > MaxValue) then
      MaxValue := MinValue;
    Value := Max(MinValue, Value);
  end;
end;

procedure TdxPreviewPageMargin.SetValue(AValue: Integer);
begin
  AValue := CheckValue(AValue);
  if FValue <> AValue then
  begin
    FValue := AValue;
    if (Preview <> nil) and Preview.CanChangeMargins then
    begin
      Preview.DoMarginChanged(Self);
      Changed(True);
    end;
  end;
end;

procedure TdxPreviewPageMargin.SetEnabled(Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    Changed(False);
  end;
end;

procedure TdxPreviewPageMargin.SetVisible(Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    Changed(False);
  end;
end;

procedure TdxPreviewPageMargin.ReadIsCaptionAssigned(AReader: TReader);
begin
  FIsCaptionAssigned := AReader.ReadBoolean;
end;

procedure TdxPreviewPageMargin.WriteIsCaptionAssigned(AWriter: TWriter);
begin
  AWriter.WriteBoolean(FIsCaptionAssigned);
end;

{ TdxPreviewPageMarginBottom }

function TdxPreviewPageMarginBottom.DefaultCaption: string;
begin
  Result := cxGetResourceString(@sdxPreviewBottomMargin);
end;

function TdxPreviewPageMarginBottom.DefaultValue: Integer;
begin
  Result := 254;
end;

function TdxPreviewPageMarginBottom.GetHitTest: TdxPreviewHitTest;
begin
  Result := phtMarginBottom;
end;

function TdxPreviewPageMarginBottom.GetCursor: TCursor;
begin
  Result := crdxPreviewVertResize;
end;

function TdxPreviewPageMarginBottom.GetIsForceRecalculatePageCount: Boolean;
begin
  Result := True;
end;

function TdxPreviewPageMarginBottom.GetIsForward: Boolean;
begin
  Result := False;
end;

function TdxPreviewPageMarginBottom.GetIsVertical: Boolean;
begin
  Result := False;
end;

function TdxPreviewPageMarginBottom.DoGetActualMaxValue: Integer;
begin
  Result := ActualPageSize.ActualSize.y - (ActualPageSize.MinUsefulSize.Y +
    MaxIntValue([Margins.Top.Value, Margins.Top.MinValue, Margins.Header.MinValue]));
end;

function TdxPreviewPageMarginBottom.DoGetActualMinValue: Integer;
begin
  Result := Min(Margins.Footer.Value, ActualMaxValue);
end;

function TdxPreviewPageMarginBottom.DoGetMaxMinValue: Integer;
begin
  Result := ActualPageSize.ActualSize.y - (Margins.Top.Value + ActualPageSize.MinUsefulSize.Y);
end;

function TdxPreviewPageMarginBottom.DoPosFromValue(AValue: Integer): Integer;
begin
  Result := PageBounds.Bottom - 1 - AValue;
end;

function TdxPreviewPageMarginBottom.DoValueFromPos(APos: Integer): Integer;
begin
  Result := PageBounds.Bottom - 1 - APos;
end;

{ TdxPreviewPageMarginFooter }

function TdxPreviewPageMarginFooter.DefaultCaption: string;
begin
  Result := cxGetResourceString(@sdxPreviewFooterMargin);
end;

function TdxPreviewPageMarginFooter.DefaultValue: Integer;
begin
  Result := 127;
end;

function TdxPreviewPageMarginFooter.GetHitTest: TdxPreviewHitTest;
begin
  Result := phtMarginFooter;
end;

function TdxPreviewPageMarginFooter.GetCursor: TCursor;
begin
  Result := crdxPreviewVertResize;
end;

function TdxPreviewPageMarginFooter.GetIsForward: Boolean;
begin
  Result := False;
end;

function TdxPreviewPageMarginFooter.GetIsVertical: Boolean;
begin
  Result := False;
end;

function TdxPreviewPageMarginFooter.DoGetActualMaxValue: Integer;
begin
  Result := ActualPageSize.ActualSize.Y - (Margins.Top.Value + ActualPageSize.MinUsefulSize.Y);
end;

function TdxPreviewPageMarginFooter.DoGetMaxMinValue: Integer;
begin
  Result := ActualPageSize.ActualSize.Y - (Margins.Top.Value + ActualPageSize.MinUsefulSize.Y);
end;

function TdxPreviewPageMarginFooter.DoPosFromValue(AValue: Integer): Integer;
begin
  Result := PageBounds.Bottom - 1 - AValue;
end;

function TdxPreviewPageMarginFooter.DoValueFromPos(APos: Integer): Integer;
begin
  Result := PageBounds.Bottom - 1 - APos;
end;

{ TdxPreviewPageMarginHeader }

function TdxPreviewPageMarginHeader.DefaultCaption: string;
begin
  Result := cxGetResourceString(@sdxPreviewHeaderMargin);
end;

function TdxPreviewPageMarginHeader.DefaultValue: Integer;
begin
  Result := 127;
end;

function TdxPreviewPageMarginHeader.GetHitTest: TdxPreviewHitTest;
begin
  Result := phtMarginHeader;
end;

function TdxPreviewPageMarginHeader.GetCursor: TCursor;
begin
  Result := crdxPreviewVertResize;
end;

function TdxPreviewPageMarginHeader.GetIsForward: Boolean;
begin
  Result := True;
end;

function TdxPreviewPageMarginHeader.GetIsVertical: Boolean;
begin
  Result := False;
end;

function TdxPreviewPageMarginHeader.DoGetActualMaxValue: Integer;
begin
  Result := ActualPageSize.ActualSize.Y - (ActualPageSize.MinUsefulSize.Y + Margins.Bottom.Value);
end;

function TdxPreviewPageMarginHeader.DoGetMaxMinValue: Integer;
begin
  Result := ActualPageSize.ActualSize.Y - (Margins.Bottom.Value + ActualPageSize.MinUsefulSize.Y);
end;

function TdxPreviewPageMarginHeader.DoPosFromValue(AValue: Integer): Integer;
begin
  Result := PageBounds.Top + AValue;
end;

function TdxPreviewPageMarginHeader.DoValueFromPos(APos: Integer): Integer;
begin
  Result := APos - PageBounds.Top;
end;

{ TdxPreviewPageMarginLeft }

function TdxPreviewPageMarginLeft.DefaultCaption: string;
begin
  Result := cxGetResourceString(@sdxPreviewLeftMargin);
end;

function TdxPreviewPageMarginLeft.DefaultValue: Integer;
begin
  Result := 254;
end;

function TdxPreviewPageMarginLeft.GetHitTest: TdxPreviewHitTest;
begin
  Result := phtMarginLeft;
end;

function TdxPreviewPageMarginLeft.GetCursor: TCursor;
begin
  Result := crdxPreviewHorzResize;
end;

function TdxPreviewPageMarginLeft.GetIsForceRecalculatePageCount: Boolean;
begin
  Result := True;
end;

function TdxPreviewPageMarginLeft.GetIsForward: Boolean;
begin
  Result := True;
end;

function TdxPreviewPageMarginLeft.GetIsVertical: Boolean;
begin
  Result := True;
end;

function TdxPreviewPageMarginLeft.DoGetActualMaxValue: Integer;
begin
  Result := Max(ActualMinValue, ActualPageSize.ActualSize.X - (ActualPageSize.MinUsefulSize.X + Margins.Right.Value));
end;

function TdxPreviewPageMarginLeft.DoGetMaxMinValue: Integer;
begin
  Result := ActualPageSize.ActualSize.X - (Margins.Right.Value + ActualPageSize.MinUsefulSize.X);
end;

function TdxPreviewPageMarginLeft.DoPosFromValue(AValue: Integer): Integer;
begin
  Result := PageBounds.Left + AValue;
end;

function TdxPreviewPageMarginLeft.DoValueFromPos(APos: Integer): Integer;
begin
  Result := APos - PageBounds.Left;
end;

{ TdxPreviewPageMarginRight }

function TdxPreviewPageMarginRight.DefaultCaption: string;
begin
  Result := cxGetResourceString(@sdxPreviewRightMargin);
end;

function TdxPreviewPageMarginRight.DefaultValue: Integer;
begin
  Result := 254;
end;

function TdxPreviewPageMarginRight.GetHitTest: TdxPreviewHitTest;
begin
  Result := phtMarginRight;
end;

function TdxPreviewPageMarginRight.GetCursor: TCursor;
begin
  Result := crdxPreviewHorzResize;
end;

function TdxPreviewPageMarginRight.GetIsForceRecalculatePageCount: Boolean;
begin
  Result := True;
end;

function TdxPreviewPageMarginRight.GetIsForward: Boolean;
begin
  Result := False;
end;

function TdxPreviewPageMarginRight.GetIsVertical: Boolean;
begin
  Result := True;
end;

function TdxPreviewPageMarginRight.DoGetActualMaxValue: Integer;
begin
  Result := Max(ActualMinValue, ActualPageSize.ActualSize.X - (Margins.Left.Value + ActualPageSize.MinUsefulSize.X));
end;

function TdxPreviewPageMarginRight.DoGetMaxMinValue: Integer;
begin
  Result := ActualPageSize.ActualSize.X - (Margins.Left.Value + ActualPageSize.MinUsefulSize.X);
end;

function TdxPreviewPageMarginRight.DoPosFromValue(AValue: Integer): Integer;
begin
  Result := PageBounds.Right - 1 - AValue;
end;

function TdxPreviewPageMarginRight.DoValueFromPos(APos: Integer): Integer;
begin
  Result := PageBounds.Right - 1 - APos;
end;

{ TdxPreviewPageMarginTop }

function TdxPreviewPageMarginTop.DefaultCaption: string;
begin
  Result := cxGetResourceString(@sdxPreviewTopMargin);
end;

function TdxPreviewPageMarginTop.DefaultValue: Integer;
begin
  Result := 254;
end;

function TdxPreviewPageMarginTop.GetHitTest: TdxPreviewHitTest;
begin
  Result := phtMarginLeft;
end;

function TdxPreviewPageMarginTop.GetCursor: TCursor;
begin
  Result := crdxPreviewVertResize;
end;

function TdxPreviewPageMarginTop.GetIsForceRecalculatePageCount: Boolean;
begin
  Result := True;
end;

function TdxPreviewPageMarginTop.GetIsForward: Boolean;
begin
  Result := True;
end;

function TdxPreviewPageMarginTop.GetIsVertical: Boolean;
begin
  Result := False;
end;

function TdxPreviewPageMarginTop.DoGetActualMaxValue: Integer;
begin
  Result := ActualPageSize.ActualSize.Y - (ActualPageSize.MinUsefulSize.Y +
    MaxIntValue([Margins.Bottom.Value, Margins.Bottom.MinValue, Margins.Footer.MinValue]));
end;

function TdxPreviewPageMarginTop.DoGetActualMinValue: Integer;
begin
  Result := Min(Margins.Header.Value, ActualMaxValue);
end;

function TdxPreviewPageMarginTop.DoGetMaxMinValue: Integer;
begin
  Result := ActualPageSize.ActualSize.Y - (Margins.Bottom.Value + ActualPageSize.MinUsefulSize.Y);
end;

function TdxPreviewPageMarginTop.DoPosFromValue(AValue: Integer): Integer;
begin
  Result := PageBounds.Top + AValue;
end;

function TdxPreviewPageMarginTop.DoValueFromPos(APos: Integer): Integer;
begin
  Result := APos - PageBounds.Top;
end;

{ TdxPreviewPageMargins }

constructor TdxPreviewPageMargins.Create(APreview: TdxCustomPreview);
begin
  inherited Create;
  FPreview := APreview;
  FMargins := TList.Create;
  AddMargins;
end;

destructor TdxPreviewPageMargins.Destroy;
begin
  FreeAndNilMargins;
  inherited Destroy;
end;

procedure TdxPreviewPageMargins.Assign(Source: TPersistent);
begin
  if Source is TdxPreviewPageMargins then
  begin
    BeginUpdate;
    try
      DoAssign(TdxPreviewPageMargins(Source));
    finally
      EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;

procedure TdxPreviewPageMargins.RestoreDefaults;
begin
  BeginUpdate;
  try
    DoRestoreDefaults;
  finally
    EndUpdate;
  end;
end;

procedure TdxPreviewPageMargins.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TdxPreviewPageMargins.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then
    Update(nil);
end;

procedure TdxPreviewPageMargins.DoAssign(Source: TdxPreviewPageMargins);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Margins[I].Assign(Source[I]);
end;

procedure TdxPreviewPageMargins.DoRestoreDefaults;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Margins[I].RestoreDefaults;
end;

function TdxPreviewPageMargins.AddMargin(AClass: TdxPreviewPageMarginClass): TdxPreviewPageMargin;
begin
  Result := AClass.Create(Self);
  FMargins.Add(Result);
end;

procedure TdxPreviewPageMargins.AddMargins;
begin
  AddMargin(TdxPreviewPageMarginLeft);
  AddMargin(TdxPreviewPageMarginTop);
  AddMargin(TdxPreviewPageMarginRight);
  AddMargin(TdxPreviewPageMarginBottom);
  AddMargin(TdxPreviewPageMarginHeader);
  AddMargin(TdxPreviewPageMarginFooter);
end;

procedure TdxPreviewPageMargins.FreeAndNilMargins;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Margins[I].Free;
  FreeAndNil(FMargins);
end;

procedure TdxPreviewPageMargins.Update(AMargin: TdxPreviewPageMargin);
begin
  if Assigned(Preview) and not Preview.IsUpdateLocked then
  begin
    Preview.SelPageSizeOptions.Margins := Rect(Left.Value, Top.Value, Right.Value, Bottom.Value);
    Preview.SelPageSizeOptions.Footer := Footer.Value;
    Preview.SelPageSizeOptions.Header := Header.Value;
    Preview.FullRefresh;
  end;
end;

function TdxPreviewPageMargins.GetCount: Integer;
begin
  Result := FMargins.Count;
end;

function TdxPreviewPageMargins.GetMargin(Index: Integer): TdxPreviewPageMargin;
begin
  Result := TdxPreviewPageMargin(FMargins[Index]);
end;

function TdxPreviewPageMargins.GetMarginByCaption(const Caption: string): TdxPreviewPageMargin;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Result := Margins[I];
    if AnsiSameText(Result.Caption, Caption) then
      Exit;
  end;
  Result := nil;
end;

function TdxPreviewPageMargins.GetMarginByClass(Index: TdxPreviewPageMarginClass): TdxPreviewPageMargin;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Result := Margins[I];
    if Result.ClassType = Index then
      Exit;
  end;
  Result := nil;
end;

function TdxPreviewPageMargins.GetMarginBottom: TdxPreviewPageMarginBottom;
begin
  Result := MarginsByClass[TdxPreviewPageMarginBottom] as TdxPreviewPageMarginBottom;
end;

function TdxPreviewPageMargins.GetMarginFooter: TdxPreviewPageMarginFooter;
begin
  Result := MarginsByClass[TdxPreviewPageMarginFooter] as TdxPreviewPageMarginFooter;
end;

function TdxPreviewPageMargins.GetMarginHeader: TdxPreviewPageMarginHeader;
begin
  Result := MarginsByClass[TdxPreviewPageMarginHeader] as TdxPreviewPageMarginHeader;
end;

function TdxPreviewPageMargins.GetMarginLeft: TdxPreviewPageMarginLeft;
begin
  Result := MarginsByClass[TdxPreviewPageMarginLeft] as TdxPreviewPageMarginLeft;
end;

function TdxPreviewPageMargins.GetMarginRight: TdxPreviewPageMarginRight;
begin
  Result := MarginsByClass[TdxPreviewPageMarginRight] as TdxPreviewPageMarginRight;
end;

function TdxPreviewPageMargins.GetMarginTop: TdxPreviewPageMarginTop;
begin
  Result := MarginsByClass[TdxPreviewPageMarginTop] as TdxPreviewPageMarginTop;
end;

procedure TdxPreviewPageMargins.SetMargin(Index: Integer; Value: TdxPreviewPageMargin);
begin
  Margins[Index].Assign(Value);
end;

procedure TdxPreviewPageMargins.SetMarginByClass(Index: TdxPreviewPageMarginClass;
  Value: TdxPreviewPageMargin);
var
  Margin: TdxPreviewPageMargin;
begin
  Margin := MarginsByClass[Index];
  if Margin <> nil then
    Margin.Assign(Value);
end;

procedure TdxPreviewPageMargins.SetMarginBottom(Value: TdxPreviewPageMarginBottom);
begin
  MarginsByClass[TdxPreviewPageMarginBottom] := Value;
end;

procedure TdxPreviewPageMargins.SetMarginFooter(Value: TdxPreviewPageMarginFooter);
begin
  MarginsByClass[TdxPreviewPageMarginFooter] := Value;
end;

procedure TdxPreviewPageMargins.SetMarginHeader(Value: TdxPreviewPageMarginHeader);
begin
  MarginsByClass[TdxPreviewPageMarginHeader] := Value;
end;

procedure TdxPreviewPageMargins.SetMarginLeft(Value: TdxPreviewPageMarginLeft);
begin
  MarginsByClass[TdxPreviewPageMarginLeft] := Value;
end;

procedure TdxPreviewPageMargins.SetMarginRight(Value: TdxPreviewPageMarginRight);
begin
  MarginsByClass[TdxPreviewPageMarginRight] := Value;
end;

procedure TdxPreviewPageMargins.SetMarginTop(Value: TdxPreviewPageMarginTop);
begin
  MarginsByClass[TdxPreviewPageMarginTop] := Value;
end;

{ TdxCustomPreview }

constructor TdxCustomPreview.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csAcceptsControls];
  ParentColor := False;
  Brush.Color := Color;

  FPagesContentCache := TdxPreviewPageContentCachePool.Create(GetDefaultCachedPagesCount);
  FHintHideLongTimer := cxCreateTimer(HintHideLongTimerHandler, dxPreviewHideHintLongTime);
  FHintHideShortTimer := cxCreateTimer(HintHideShortTimerHandler, dxPreviewHideHintShortTime);
  FHintShowTimer := cxCreateTimer(HintShowTimerHandler, dxPreviewShowHintTime);

  FOptionsHint := dxPreviewDefaultOptionsHint;
  FOptionsStore := [posZoom];

  FInternalOptionsBehavior := dxPreviewDefaultBehaviorOptions;
  FInternalOptionsView := dxPreviewDefaultOptionsView;
  FInternalOptionsZoom := dxPreviewDefaultOptionsZoom;

  MarginColor := clWindowText;

  ClearLastMousePos;
  FMargins := CreateMargins;

  FMaxZoomFactor := GetDefaultMaxZoomFactor;
  FMinZoomFactor := GetDefaultMinZoomFactor;

  FPagesRows := TdxPreviewPagesRowList.Create;
  FPageSize := GetPageSizeOptionsClass.Create(Self, cxPoint(2100, 2970));
  FPageSize.OnChange := PageParametersChanged;

  FPages := TdxPreviewPageList.Create;
  FPageXCount := 1;
  FPageYCount := 1;
  FSelPageIndex := -1;

  FUnzoomedFactor := 50;
  FZoomFactor := GetDefaultZoomFactor;
  FPrevZoomFactor := FZoomFactor;
  FZoomMode := pzmNone;
  FZoomStep := GetDefaultZoomStep;
  FZoomed := True;
  Height := GetDefaultHeight;
  Width := GetDefaultWidth;

  if IsDesigning then
  begin
    PageCount := 1;
    SelPageIndex := 0;
  end;

  BorderStyle := cxcbsDefault;
end;

destructor TdxCustomPreview.Destroy;
begin
  if not IsDesigning and NeedStoreInRegistry then
  begin
    if RegistryPath <> '' then
      SaveToRegistry(RegistryPath);
  end;
  DestroyPageNumberHint;
  DestroyHint;
  FreeMarginPen;
  FreeAndNil(FPageSize);

  PageCount := 0;
  FreeAndNil(FHintHideLongTimer);
  FreeAndNil(FHintHideShortTimer);
  FreeAndNil(FHintShowTimer);
  FreeAndNil(FPagesContentCache);
  FreeAndNil(FPagesRows);
  FreeAndNil(FPages);
  FreeAndNil(FMargins);
  inherited Destroy;
end;

procedure TdxCustomPreview.LoadFromIniFile(AIniFile: TCustomIniFile; const ASectionName: string);
begin
  OptionsHint := TdxPreviewOptionsHint(Byte(AIniFile.ReadInteger(ASectionName, sdxOptionsHint, Byte(OptionsHint))));
  InternalOptionsView := TdxPreviewOptionsView(Byte(AIniFile.ReadInteger(ASectionName, sdxOptionsView, Byte(InternalOptionsView))));
  InternalOptionsZoom := TdxPreviewOptionsZoom(Byte(AIniFile.ReadInteger(ASectionName, sdxOptionsZoom, Byte(InternalOptionsZoom))));

  ZoomStep := AIniFile.ReadInteger(ASectionName, sdxZoomStep, ZoomStep);
  MarginColor := AIniFile.ReadInteger(ASectionName, sdxMarginColor, MarginColor);
  MeasurementUnits := TdxPreviewMeasurementUnits(AIniFile.ReadInteger(ASectionName, sdxMeasurementUnits, Ord(MeasurementUnits)));
  PageSize.Orientation := TdxPreviewPaperOrientation(AIniFile.ReadInteger(ASectionName, sdxOrientation, Ord(PageSize.Orientation)));

  if NeedStoreZoomOptions then
  begin
    ZoomFactor := AIniFile.ReadInteger(ASectionName, sdxZoomFactor, ZoomFactor);
    ZoomMode := TdxPreviewZoomMode(AIniFile.ReadInteger(ASectionName, sdxZoomMode, Ord(ZoomMode)));
    if ZoomMode = pzmPages then
    begin
      PageXCount := AIniFile.ReadInteger(ASectionName, sdxPageXCount, PageXCount);
      PageYCount := AIniFile.ReadInteger(ASectionName, sdxPageYCount, PageYCount);
    end;
  end;
end;

procedure TdxCustomPreview.LoadFromRegistry(const ARegistryPath: string);
var
  ARegIniFile: TRegistryIniFile;
begin
  ARegIniFile := TRegistryIniFile.Create('');
  try
    LoadFromIniFile(ARegIniFile, ARegistryPath);
  finally
    ARegIniFile.Free;
  end;
end;

procedure TdxCustomPreview.SaveToIniFile(
  AIniFile: TCustomIniFile; const ASectionName: string);
begin
  AIniFile.WriteInteger(ASectionName, sdxMeasurementUnits, Integer(MeasurementUnits));
  AIniFile.WriteInteger(ASectionName, sdxOptionsHint, Integer(Byte(OptionsHint)));
  AIniFile.WriteInteger(ASectionName, sdxOptionsView, Integer(Byte(InternalOptionsView)));
  AIniFile.WriteInteger(ASectionName, sdxOptionsZoom, Integer(Byte(InternalOptionsZoom)));
  AIniFile.WriteInteger(ASectionName, sdxOrientation, Integer(FPageSize.Orientation));
  AIniFile.WriteInteger(ASectionName, sdxMarginColor, Integer(MarginColor));
  AIniFile.WriteInteger(ASectionName, sdxZoomStep, ZoomStep);

  if NeedStoreZoomOptions then
  begin
    AIniFile.WriteInteger(ASectionName, sdxZoomFactor, ZoomFactor);
    AIniFile.WriteInteger(ASectionName, sdxZoomMode, Integer(ZoomMode));
    if ZoomMode = pzmPages then
    begin
      AIniFile.WriteInteger(ASectionName, sdxPageXCount, PageXCount);
      AIniFile.WriteInteger(ASectionName, sdxPageYCount, PageYCount);
    end;
  end;
end;

procedure TdxCustomPreview.SaveToRegistry(const ARegistryPath: string);
var
  ARegIniFile: TRegistryIniFile;
begin
  ARegIniFile := TRegistryIniFile.Create('');
  try
    SaveToIniFile(ARegIniFile, ARegistryPath);
  finally
    ARegIniFile.Free;
  end;
end;

function TdxCustomPreview.DefaultNoPagesText: string;
begin
  Result := cxGetResourceString(@sdxPreviewNoPages);
end;

procedure TdxCustomPreview.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TdxCustomPreview.CancelUpdate;
begin
  if FUpdateCount <> 0 then
    Dec(FUpdateCount);
end;

procedure TdxCustomPreview.EndUpdate;
begin
  if FUpdateCount <> 0 then
  begin
    Dec(FUpdateCount);
    LayoutChanged;
  end;
end;

procedure TdxCustomPreview.GoToFirstPage;
begin
  InternalGoToFirstPage;
end;

procedure TdxCustomPreview.GoToLastPage;
begin
  InternalGoToLastPage;
end;

procedure TdxCustomPreview.GoToNextPage;
begin
  InternalGoToNextPage;
end;

procedure TdxCustomPreview.GoToPrevPage;
begin
  InternalGoToPrevPage;
end;

function TdxCustomPreview.IsUpdateLocked: Boolean;
begin
  Result := (FUpdateCount <> 0) or (csLoading in ComponentState);
end;

procedure TdxCustomPreview.InvalidatePage(APageIndex: Integer);
begin
  if (APageIndex > -1) and (APageIndex < PageCount) then
    Pages[APageIndex].Invalidate;
end;

procedure TdxCustomPreview.InvalidatePages;
begin
  FPages.Invalidate;
end;

procedure TdxCustomPreview.InvalidatePagesContent;
begin
  InvalidatePages;
end;

procedure TdxCustomPreview.InvalidatePagesFooter;
begin
  InvalidatePages;
end;

procedure TdxCustomPreview.InvalidatePagesHeader;
begin
  InvalidatePages;
end;

function TdxCustomPreview.GetHitInfoAt(const Pt: TPoint): TdxPreviewHitTests;
var
  Margin: TdxPreviewPageMargin;
begin
  if PtInRect(ClientRect, Pt) then
  begin
    if FPages.PageIndexFromPoint(Pt) < 0 then
      Result := [phtNoWhere]
    else
    begin
      Result := [phtPage];
      Margin := MarginFromPoint(Pt);
      if Margin <> nil then
        Result := Result + [Margin.GetHitTest];
    end;
  end
  else
    Result := [];
end;

function TdxCustomPreview.GetHitInfoAt(const Pt: TSmallPoint): TdxPreviewHitTests;
begin
  Result := GetHitInfoAt(SmallPointToPoint(Pt));
end;

function TdxCustomPreview.GetHitInfoAt(X, Y: Integer): TdxPreviewHitTests;
begin
  Result := GetHitInfoAt(Point(X, Y));
end;

function TdxCustomPreview.GetPagesArea: TRect;
begin
  Result := cxRectContent(ClientBounds, Rect(AbsoluteIndentLeft, AbsoluteIndentLeft, AbsoluteIndentRight, AbsoluteIndentRight));
end;

procedure TdxCustomPreview.FullRefresh;
begin
  PagesContentCache.InvalidateAll;
  DoCalcPageCount;
  CalculateAbsoluteIndents;
  Invalidate;
end;

procedure TdxCustomPreview.HideAllHints;
begin
  CancelHintShow;
  CancelHintHide;
end;

procedure TdxCustomPreview.MakeVisible(APageIndex: Integer);

  function GetDeltaX(const APagesArea, R: TRect): Integer;
  begin
    Result := 0;
    if R.Right > APagesArea.Right then
      Result := R.Right - APagesArea.Right;
    if R.Left - Result < APagesArea.Left then
      Result := R.Left - APagesArea.Left;
  end;

  function GetDeltaY(const APagesArea, R: TRect): Integer;
  begin
    Result := 0;
    if R.Bottom > APagesArea.Bottom then
      Result := R.Bottom - APagesArea.Bottom;
    if R.Top - Result < APagesArea.Top then
      Result := R.Top - APagesArea.Top;
  end;

begin
  if HandleAllocated and (APageIndex > -1) and (APageIndex < PageCount) then
  begin
    CalculatePagesLayout;
    LeftPos := LeftPos + GetDeltaX(GetPagesArea, Pages[APageIndex].SiteBounds);
    TopPos := TopPos + GetDeltaY(GetPagesArea, Pages[APageIndex].SiteBounds);
    CalculatePagesLayout;
  end;
end;

procedure TdxCustomPreview.SetPageXYCount(XCount, YCount: Integer);
begin
  BeginUpdate;
  try
    PageXCount := XCount;
    PageYCount := YCount;
  finally
    EndUpdate;
  end;
end;

procedure TdxCustomPreview.ZoomIn;
begin
  DoZoomIn;
end;

procedure TdxCustomPreview.ZoomOut;
begin
  DoZoomOut;
end;

function TdxCustomPreview.MarginFromPoint(const Pt: TPoint): TdxPreviewPageMargin;
var
  I: Integer;
begin
  if SelPageIndex <> -1 then
  begin
    for I := Margins.Count - 1 downto 0 do
    begin
      Result := Margins[I];
      if Result.HasPoint(Pt) then
        Exit;
    end;
  end;
  Result := nil;
end;

function TdxCustomPreview.MarginFromPoint(const Pt: TSmallPoint): TdxPreviewPageMargin;
begin
  Result := MarginFromPoint(SmallPointToPoint(Pt));
end;

function TdxCustomPreview.MarginFromPoint(X, Y: Integer): TdxPreviewPageMargin;
begin
  Result := MarginFromPoint(Point(X, Y));
end;

function TdxCustomPreview.MarginValueToString(Value: Integer): string;
var
  ADisplayValue: TFloat;
  AMask: string;
begin
  ADisplayValue := LoMetricToAnother(ActualMeasurementUnits, Value);
  case ActualMeasurementUnits of
    pmuInches:
      Result := cxGetResourceString(@sdxPreviewUnitsInches);
    pmuMillimeters:
      Result := cxGetResourceString(@sdxPreviewUnitsMillimeters);
    pmuCentimeters:
      Result := cxGetResourceString(@sdxPreviewUnitsCentimeters);
    pmuPoints:
      Result := cxGetResourceString(@sdxPreviewUnitsPoints);
    pmuPicas:
      Result := cxGetResourceString(@sdxPreviewUnitsPicas);
  end;
  AMask := '########0.#';
  if ActualMeasurementUnits in [pmuInches, pmuCentimeters, pmuPicas] then
    AMask := AMask + '#';
  Result := FormatFloat(AMask, ADisplayValue) + ' ' + Result;
end;

function TdxCustomPreview.IndexOfPage(APage: TdxPreviewPage): Integer;
begin
  Result := FPages.IndexOf(APage);
end;

function TdxCustomPreview.PageSizeToString: string;
var
  APageHeight: Double;
  APageWidth: Double;
  AMask: string;
begin
  case ActualMeasurementUnits of
    pmuInches:
      Result := cxGetResourceString(@sdxPreviewUnitsInches);
    pmuMillimeters:
      Result := cxGetResourceString(@sdxPreviewUnitsMillimeters);
    pmuCentimeters:
      Result := cxGetResourceString(@sdxPreviewUnitsCentimeters);
    pmuPoints:
      Result := cxGetResourceString(@sdxPreviewUnitsPoints);
    pmuPicas:
      Result := cxGetResourceString(@sdxPreviewUnitsPicas);
  end;

  AMask := '########0.#';
  if ActualMeasurementUnits in [pmuInches, pmuCentimeters, pmuPicas] then
    AMask := AMask + '#';

  APageWidth := LoMetricToAnother(ActualMeasurementUnits, SelPageSizeOptions.Size.X);
  APageHeight := LoMetricToAnother(ActualMeasurementUnits, SelPageSizeOptions.Size.Y);

  Result := FormatFloat(AMask, APageWidth) + ' ' + Result + ' x ' + FormatFloat(AMask, APageHeight) + ' ' + Result;
end;

function TdxCustomPreview.PageGetContentBounds(const APageBounds: TRect): TRect;
begin
  Result := Rect(
    APageBounds.Left + Margins.Left.VisibleValue,
    APageBounds.Top + Margins.Header.VisibleValue,
    APageBounds.Right - Margins.Right.VisibleValue,
    APageBounds.Bottom - Margins.Top.VisibleValue);
end;

function TdxCustomPreview.PageGetFooterBounds(const APageBounds: TRect): TRect;
begin
  Result := Rect(
    APageBounds.Left + Margins.Left.VisibleValue,
    APageBounds.Bottom - Margins.Footer.VisibleValue,
    APageBounds.Right - Margins.Right.VisibleValue,
    APageBounds.Bottom - Margins.Bottom.VisibleValue);
end;

function TdxCustomPreview.PageGetHeaderBounds(const APageBounds: TRect): TRect;
begin
  Result := Rect(
    APageBounds.Left + Margins.Left.VisibleValue,
    APageBounds.Top + Margins.Header.VisibleValue,
    APageBounds.Right - Margins.Right.VisibleValue,
    APageBounds.Top + Margins.Top.VisibleValue);
end;

procedure TdxCustomPreview.SelectFirstPage;
var
  I: Integer;
begin
  I := 0;
  repeat
    if SelectPage(I) then
      Break;
    Inc(I);
  until SelPageIndex + I = PageCount + 1;
end;

procedure TdxCustomPreview.SelectLastPage;
var
  I: Integer;
begin
  I := 1;
  repeat
    if SelectPage(PageCount - I) then
      Break;
    Inc(I);
  until I = -1;
end;

procedure TdxCustomPreview.SelectNextPage;
begin
  if SelPageIndex < PageCount then
    ChangeSelPageIndex(1);
end;

procedure TdxCustomPreview.SelectPrevPage;
begin
  if SelPageIndex > 0 then
    ChangeSelPageIndex(-1);
end;

function TdxCustomPreview.GetContentSize: TSize;
begin
  Result := cxSize(ContentWidth, ContentHeight);
end;

function TdxCustomPreview.GetDesignHitTest(X, Y: Integer; Shift: TShiftState): Boolean;
begin
  Result := (DraggingMargin <> nil) or (GetHitInfoAt(cxPoint(X, Y)) * phtMargins <> []);
end;

procedure TdxCustomPreview.Calculate(AType: TdxChangeType);
begin
  CalculateLayout(AType);
end;

procedure TdxCustomPreview.LayoutChanged(AType: TdxChangeType = ctHard);
begin
  if not IsUpdateLocked then
    inherited LayoutChanged(AType);
end;

procedure TdxCustomPreview.CreateParams(var Params: TCreateParams);
const
  CS_ON = CS_OWNDC;
  CS_OFF = CS_HREDRAW or CS_VREDRAW;
begin
  inherited CreateParams(Params);
  with Params do
    WindowClass.Style := (WindowClass.Style or CS_ON) and not CS_OFF;
end;

procedure TdxCustomPreview.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('IsNoPagesTextAssigned', ReadIsNoPagesTextAssigned,
    WriteIsNoPagesTextAssigned, FIsNoPagesTextAssigned and (FNoPagesText = ''));
end;

procedure TdxCustomPreview.DblClick;
begin
  if not IsScrollBarsArea(ScreenToClient(GetMouseCursorPos)) then
    inherited;
end;

function TdxCustomPreview.GetActualPageSizeOptions(APage: TdxPreviewPage): TdxPreviewPageSizeOptions;
begin
  Result := APage.PageSize;
  while not Result.Assigned do
    Result := Result.Master;
end;

function TdxCustomPreview.GetDefaultPanOptions: Integer;
begin
  Result := inherited GetDefaultPanOptions and not GC_PAN_WITH_GUTTER;
end;

function TdxCustomPreview.GetMainScrollBarsClass: TcxControlCustomScrollBarsClass;
begin
  if IsPopupScrollbars then
    Result := inherited GetMainScrollBarsClass
  else
    Result := TcxControlScrollBars;
end;

procedure TdxCustomPreview.KeyDown(var Key: Word; Shift: TShiftState);

  procedure CheckAndScrollPage(ADirection: TdxPreviewScrollDirection);
  begin
    if CanPageScrolling(ADirection) then
    begin
      ScrollPage(ADirection);
      ScrollPage(ADirection);
    end;
  end;

var
  ARow: TdxPreviewPagesRow;
begin
  case Key of
    VK_RETURN:
      if (SelPageIndex > -1) and (DraggingMargin = nil) then
      begin
        HideAllHints;
        Zoomed := not Zoomed;
      end;

    VK_ADD:
      if (SelPageIndex > -1) and (DraggingMargin = nil) then
      begin
        HideAllHints;
        if GetKeyState(VK_CONTROL) < 0 then
          ZoomIn
        else
          if not Zoomed then
            Zoomed := True;
      end;

    VK_SUBTRACT:
      if (SelPageIndex > -1) and (DraggingMargin = nil) then
      begin
        HideAllHints;
        if GetKeyState(VK_CONTROL) < 0 then
          ZoomOut
        else
          if Zoomed then
            Zoomed := False;
      end;

    VK_MENU:
      if DraggingMargin = nil then
        HideAllHints;

    VK_APPS:
      if (PopupMenu <> nil) or IsDesigning then
        HideAllHints;

    VK_ESCAPE:
      if (GetCapture = Handle) and not IsScrollBarsCapture then
      begin
        ReleaseCapture;
        Key := 0;
      end;

    VK_PRIOR:
      if not (ssCtrl in Shift) then
        SelectPrevPage
      else
        CheckAndScrollPage(psdUp);

    VK_NEXT:
      if not (ssCtrl in Shift) then
        SelectNextPage
      else
        CheckAndScrollPage(psdDown);

    VK_END:
      SelectLastPage;

    VK_HOME:
      SelectFirstPage;

    VK_LEFT:
      if ssCtrl in Shift then
        ScrollPage(psdLeft)
      else
        if SelPage.Cell.X > 0 then
          SelectPrevPage;

    VK_UP:
      if ssAlt in Shift then
        ZoomIn
      else
        if ssCtrl in Shift then
          ScrollPage(psdUp)
        else
          if SelPage.Cell.Y > 0 then
          begin
            ARow := PagesRows[SelPage.Cell.Y - 1];
            SelPageIndex := Min(ARow.StartIndex + SelPage.Cell.X, ARow.FinishIndex);
          end;

    VK_RIGHT:
      if ssCtrl in Shift then
        ScrollPage(psdRight)
      else
        if (SelPageIndex + 1 < PageCount) and (Pages[SelPageIndex + 1].Cell.Y = SelPage.Cell.Y) then
          SelectNextPage;

    VK_DOWN:
      if ssAlt in Shift then
        ZoomOut
      else
        if ssCtrl in Shift then
          ScrollPage(psdDown)
        else
          if (SelPage <> nil) and (SelPage.Cell.Y + 1 < PagesRows.Count) then
          begin
            ARow := PagesRows[SelPage.Cell.Y + 1];
            SelPageIndex := Min(ARow.StartIndex + SelPage.Cell.X, ARow.FinishIndex);
          end;
  end;
  inherited KeyDown(Key, Shift);
end;

procedure TdxCustomPreview.Loaded;
begin
  inherited Loaded;
  DoCalcPageCount;
  if not IsDesigning and NeedStoreInRegistry and (RegistryPath <> '') then
    LoadFromRegistry(RegistryPath);
end;

procedure TdxCustomPreview.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if not IsScrollBarsArea(Point(X, Y)) then
    if Button <> mbLeft then
    begin
      if DraggingMargin <> nil then
        ReleaseCapture;
    end
    else
      ProcessLeftClickByPage(Shift, X, Y);

  if (Button = mbMiddle) and (DraggingMargin = nil) and CanAnyScrolling then
    DoScrolling
  else
    inherited MouseDown(Button, Shift, X, Y);
end;

procedure TdxCustomPreview.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Pos: Integer;
  APageIndex: Integer;
begin
  inherited MouseMove(Shift, X, Y);
  if (FLastMousePos.X <> X) or (FLastMousePos.Y <> Y) then
  begin
    FLastMousePos := Point(X, Y);
    if FDraggingMargin = nil then
    begin
      ResetHintShowTimer(X, Y);
      if AllowHotTrack and IsParentFormActive then
      begin
        APageIndex := FPages.PageIndexFromPoint(X, Y);
        if APageIndex <> -1 then
          SelPageIndex := APageIndex;
      end;
    end
    else
    begin
      Pos := IfThen(DraggingMargin.IsVertical, X, Y);
      DraggingMargin.DraggingPos := Pos - FDragOffset;
    end;
  end;
end;

procedure TdxCustomPreview.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Margin: TdxPreviewPageMargin;
  Pos: Integer;
begin
  inherited MouseUp(Button, Shift, X, Y);
  if (Button = mbLeft) and Assigned(DraggingMargin) then
  begin
    Margin := DraggingMargin;
    with Margin do
    begin
      if IsVertical then
        Pos := X
      else
        Pos := Y;

      if FBeforeDragPos <> Pos then
        Value := ValueFromPos(Pos - FDragOffset);
    end;
    StopMarginDragging;
  end;
end;

procedure TdxCustomPreview.DoPaint;
begin
  inherited DoPaint;
  DrawContent(Canvas, ClientRect);
end;

function TdxCustomPreview.AllowTouchScrollUIMode: Boolean;
begin
  Result := not IsDesigning;
end;

function TdxCustomPreview.GetScrollStep: Integer;
begin
  Result := dxPreviewScrollStep;
end;

procedure TdxCustomPreview.Scroll(AScrollBarKind: TScrollBarKind; AScrollCode: TScrollCode; var AScrollPos: Integer);
begin
  Include(FState, psScrolling);
  try
    inherited Scroll(AScrollBarKind, AScrollCode, AScrollPos);

    case AScrollBarKind of
      sbHorizontal:
        ScrollHorizontal(AScrollCode, AScrollPos);
      sbVertical:
        ScrollVertical(AScrollCode, AScrollPos);
    end;

  finally
    Exclude(FState, psScrolling);
  end;
  UpdateScrollBars;
  Update;
end;

procedure TdxCustomPreview.ScrollHorizontal(AScrollCode: TScrollCode; AScrollPos: Integer);
begin
  case AScrollCode of
    scTrack:
      if AllowThumbTracking then
        LeftPos := AScrollPos;
    scPosition:
      if not AllowThumbTracking then
        LeftPos := AScrollPos;
  end;
end;

procedure TdxCustomPreview.ScrollVertical(AScrollCode: TScrollCode; AScrollPos: Integer);
begin
  case AScrollCode of
    scEndScroll:
      DestroyPageNumberHint;
    scPosition:
      if not AllowThumbTracking then
        TopPos := AScrollPos;
    scTrack:
      begin
        FHintShowTimer.Enabled := False;
        if AllowThumbTracking then
          TopPos := AScrollPos;
        if pohShowOnScroll in OptionsHint then
          UpdatePageNumberHint;
      end;
  end;
  UpdateSelectedPageIndex;
end;

function TdxCustomPreview.ProcessMouseWheelMessage(AWheelDelta: Integer): Boolean;
begin
  HideAllHints;
  Result := PageCount <> 0;
  if Result then
  begin
    if AllowZoomOnMouseRoll or (GetKeyState(VK_CONTROL) < 0) then
    begin
      if AWheelDelta > 0 then
        ZoomIn
      else
        ZoomOut;
    end
    else
      Result := False;
  end;
end;

procedure TdxCustomPreview.WndProc(var Message: TMessage);

  function CanDispatchMessage(var AMouse: TWMMouse): Boolean;
  begin
    Result := not IsControlMouseMsg(AMouse) and (not AllowZoomOnClick or (FPages.PageIndexFromPoint(AMouse.Pos) = -1));
  end;

  function HandleWindowMessage(var Message: TMessage): Boolean;
  begin
    Result := (Message.Msg = WM_LBUTTONDOWN) or (Message.Msg = WM_LBUTTONDBLCLK);
    if Result then
    begin
      Result := not (IsDesigning or Dragging) and
        (DragMode = dmAutomatic) and (DragKind = dkDrag);

      if Result and CanDispatchMessage(TWMMouse(Message)) then
      begin
        ControlState := ControlState + [csLButtonDown];
        Dispatch(Message);
      end;
    end;
  end;

begin
  if not HandleWindowMessage(Message) then
    inherited WndProc(Message);
end;

function TdxCustomPreview.CreateMargins: TdxPreviewPageMargins;
begin
  Result := GetMarginsClass.Create(Self);
end;

function TdxCustomPreview.GetCursor: TCursor;
begin
  if Zoomed then
    Result := GetZoomOutCursor
  else
    Result := GetZoomInCursor;
end;

function TdxCustomPreview.GetMarginsClass: TdxPreviewPageMarginsClass;
begin
  Result := TdxPreviewPageMargins;
end;

procedure TdxCustomPreview.CalculateZoomFactorForPageWidthPreviewZoomMode(AFirstPageIndex, ALastPageIndex: Integer);

  function GetMaxPageSize(AFirstIndex, ALastIndex: Integer): Integer;
  var
    I: Integer;
  begin
    Result := 0;
    for I := AFirstIndex to ALastIndex do
      Result := Max(Result, Pages[I].PageSize.ActualSizeInPixels.X);
  end;

begin
  ZoomFactor := MulDiv(100, cxRectWidth(GetPagesArea), GetMaxPageSize(AFirstPageIndex, ALastPageIndex));
end;

procedure TdxCustomPreview.CalculateZoomFactorForPagesPreviewZoomMode(AFirstPageIndex, ALastPageIndex: Integer);

  function CalcZoomFactorY(const R: TRect; AIndex, ACount: Integer): Integer;
  var
    AZoom: Integer;
    I: Integer;
  begin
    Result := MaxZoomFactor;
    AIndex := Pages[AIndex].Cell.Y;
    for I := AIndex to AIndex + Min(PagesRows.Count - AIndex, ACount) - 1 do
    begin
      AZoom := Trunc(ZoomFactor * cxRectHeight(R) / (PagesRows[I].Bounds.Bottom - PagesRows[AIndex].Bounds.Top));
      if AZoom < MinZoomFactor then
        Break;
      Result := Min(Result, AZoom);
    end;
  end;

  function CalcZoomFactorX(AStartIndex, AFinishIndex, APagesCount: Integer): Integer;
  begin
    Result := 0;
    AStartIndex := FPages[AStartIndex].Cell.Y;
    AFinishIndex := FPages[AFinishIndex].Cell.Y;
    while AStartIndex <= AFinishIndex do
    begin
      Result := Max(Result, cxRectWidth(PagesRows[AStartIndex].Bounds));
      Inc(AStartIndex);
    end;

    Result := Max(Trunc(ZoomFactor * cxRectWidth(GetPagesArea) / Result), MinZoomFactor);
  end;

begin
  ZoomFactor := Min(
    CalcZoomFactorX(AFirstPageIndex, ALastPageIndex, PageXCount),
    CalcZoomFactorY(GetPagesArea, AFirstPageIndex, PageYCount));
end;

procedure TdxCustomPreview.ResyncMargins;
begin
  BeginUpdate;
  try
    Margins.Bottom.MinValue := SelPageSizeOptions.MarginsMinValues.Bottom;
    Margins.Left.MinValue := SelPageSizeOptions.MarginsMinValues.Left;
    Margins.Right.MinValue := SelPageSizeOptions.MarginsMinValues.Right;
    Margins.Top.MinValue := SelPageSizeOptions.MarginsMinValues.Top;

    Margins.Bottom.Value := SelPageSizeOptions.Margins.Bottom;
    Margins.Footer.Value := SelPageSizeOptions.Footer;
    Margins.Header.Value := SelPageSizeOptions.Header;
    Margins.Left.Value := SelPageSizeOptions.Margins.Left;
    Margins.Right.Value := SelPageSizeOptions.Margins.Right;
    Margins.Top.Value := SelPageSizeOptions.Margins.Top;
  finally
    EndUpdate;
  end;
end;

function TdxCustomPreview.CreatePage: TdxPreviewPage;
begin
  Result := GetPageClass.Create(Self);
  FPages.Add(Result);
end;

function TdxCustomPreview.GetPageClass: TdxPreviewPageClass;
begin
  Result := TdxPreviewPage;
end;

function TdxCustomPreview.GetPageSizeOptionsClass: TdxPreviewPageSizeOptionsClass;
begin
  Result := TdxPReviewPageSizeOptions;
end;

function TdxCustomPreview.GetDefaultCachedPagesCount: Integer;
begin
  Result := dxPreviewCachedPagesCount;
end;

function TdxCustomPreview.GetDefaultHeight: Integer;
begin
  Result := 460;
end;

function TdxCustomPreview.GetDefaultMaxZoomFactor: Integer;
begin
  Result := 500;
end;

function TdxCustomPreview.GetDefaultMinZoomFactor: Integer;
begin
  Result := dxPreviewMinZoomFactor;
end;

function TdxCustomPreview.GetDefaultWidth: Integer;
begin
  Result := 320;
end;

function TdxCustomPreview.GetDefaultZoomFactor: Integer;
begin
  Result := 100;
end;

function TdxCustomPreview.GetDefaultZoomStep: Integer;
begin
  Result := dxPreviewZoomStep;
end;

function TdxCustomPreview.GetZoomInCursor: TCursor;
begin
  Result := crdxPreviewZoomIn;
end;

function TdxCustomPreview.GetZoomOutCursor: TCursor;
begin
  Result := crdxPreviewZoomOut;
end;

function TdxCustomPreview.AllowDragMargins: Boolean;
begin
  Result := IsEnabledBehaviorOption(pobAllowDragMargins);
end;

function TdxCustomPreview.AllowHotTrack: Boolean;
begin
  Result := IsEnabledBehaviorOption(pobHotTrack);
end;

function TdxCustomPreview.AllowKeyNavigation: Boolean;
begin
  Result := IsEnabledBehaviorOption(pobKeyNavigation);
end;

function TdxCustomPreview.AllowMakeVisibleSelectedPageWhenScrolling: Boolean;
begin
  Result := True;
end;

function TdxCustomPreview.AllowThumbTracking: Boolean;
begin
  Result := IsEnabledBehaviorOption(pobThumbTracking);
end;

function TdxCustomPreview.NonCenterizePages: Boolean;
begin
  Result := IsEnabledBehaviorOption(pobNonCenterizePages);
end;

function TdxCustomPreview.NonVerticalCenterizePages: Boolean;
begin
  Result := False;
end;

function TdxCustomPreview.NeedStoreInRegistry: Boolean;
begin
  Result := IsEnabledBehaviorOption(pobStoreInRegistry);
end;

function TdxCustomPreview.NeedStoreZoomOptions: Boolean;
begin
  Result := posZoom in OptionsStore;
end;

function TdxCustomPreview.CanShowMargins: Boolean;
begin
  Result := IsEnabledViewOption(povMargins);
end;

function TdxCustomPreview.AllowAutoHideScrollBars: Boolean;
begin
  Result := IsEnabledViewOption(povAutoHideScrollBars);
end;

function TdxCustomPreview.AllowPageSelection: Boolean;
begin
  Result := IsEnabledViewOption(povPageSelection);
end;

function TdxCustomPreview.IsDefaultDrawPageBackground: Boolean;
begin
  Result := IsEnabledViewOption(povDefaultDrawPageBackground);
end;

function TdxCustomPreview.AllowZoomOnClick: Boolean;
begin
  Result := pozZoomOnClick in InternalOptionsZoom;
end;

function TdxCustomPreview.AllowZoomOnMouseRoll: Boolean;
begin
  Result := pozZoomOnMouseRoll in InternalOptionsZoom;
end;

function TdxCustomPreview.GetAbsoluteIndentLeft: Integer;
begin
  Result := ScaleFactor.Apply(dxPreviewAbsoluteIndent) + Max(PageBorders.Left, PageBorders.Top);
end;

function TdxCustomPreview.GetAbsoluteIndentRight: Integer;
begin
  Result := ScaleFactor.Apply(dxPreviewAbsoluteIndent) + Max(PageBorders.Right, PageBorders.Bottom);
end;

procedure TdxCustomPreview.CalculatePageNumberHintText(AStartPage, AEndPage: Integer; var AText: string);
begin
  AText := AText + IntToStr(AStartPage + 1);
  if AStartPage <> AEndPage then
    AText := AText + ' - ' + IntToStr(AEndPage + 1);
end;

procedure TdxCustomPreview.CalculatePagesLayout;
var
  AColIndex: Integer;
  AContentRect: TRect;
  ADelta: Integer;
  AFirstPageInRow: Integer;
  APage: TdxPreviewPage;
  APageSize: TPoint;
  ARect: TRect;
  I: Integer;
begin
  PagesRows.Clear;
  AContentRect := GetPagesArea;
  ARect := cxRectSetHeight(AContentRect, 0);
  AFirstPageInRow := 0;
  AColIndex := 0;

  for I := 0 to PageCount - 1 do
  begin
    APage := Pages[I];
    APageSize := cxPointScale(APage.PageSize.ActualSizeInPixels, ActualZoomFactor, 100);
    if AFirstPageInRow <> I then
      if (ZoomMode = pzmPages) and (AColIndex = PageXCount) or
        (ZoomMode <> pzmPages) and (ARect.Left + APageSize.X > ARect.Right) then
      begin
        PagesRows.Add(AFirstPageInRow, I - 1, cxRect(AContentRect.Left, ARect.Top, ARect.Left - Indent, ARect.Bottom));
        ARect.Top := ARect.Bottom + Indent;
        ARect.Left := AContentRect.Left;
        ARect.Bottom := ARect.Top;
        AFirstPageInRow := I;
        AColIndex := 0;
      end;

    APage.Cell := Point(AColIndex, PagesRows.Count);
    APage.Bounds := cxRectSetSize(ARect, APageSize.X, APageSize.Y);
    ARect.Bottom := Max(ARect.Bottom, APage.Bounds.Bottom);
    ARect.Left := APage.Bounds.Right + Indent;
    Inc(AColIndex);
  end;
  PagesRows.Add(AFirstPageInRow, PageCount - 1, cxRect(AContentRect.Left, ARect.Top, ARect.Left - Indent, ARect.Bottom));

  if not NonCenterizePages then
  begin
    for I := 0 to PagesRows.Count - 1 do
      FPages.CenterPagesInRow(PagesRows[I], cxRectWidth(AContentRect));

    if not NonVerticalCenterizePages then
    begin
      ADelta := (cxRectHeight(AContentRect) - cxRectHeight(FPages.Bounds)) div 2;
      if ADelta > 0 then
      begin
        FPages.OffsetBounds(0, ADelta);
        FPagesRows.OffsetBounds(0, ADelta);
      end;
    end;
  end;
  OffsetContent(cxPoint(-LeftPos, -TopPos));
end;

procedure TdxCustomPreview.OffsetContent(const ADelta: TPoint);
begin
  FPages.OffsetBounds(ADelta.X, ADelta.Y);
  FPagesRows.OffsetBounds(ADelta.X, ADelta.Y);
end;

procedure TdxCustomPreview.ProcessClickBySelectedPage(Shift: TShiftState; X, Y: Integer);

  function CanDragMargin(AMargin: TdxPreviewPageMargin): Boolean;
  begin
    Result := AllowDragMargins and Assigned(AMargin) and AMargin.Enabled and not (ssDouble in Shift);
  end;

  procedure StartMarginDragging(AMargin: TdxPreviewPageMargin);
  var
    R: TRect;
  begin
    AMargin.DoDragBefore;
    R := AMargin.Bounds;
    FDraggingMargin := AMargin;
    if AMargin.IsVertical then
    begin
      FBeforeDragPos := X;
      FDragOffset := X - R.Left;
    end
    else
    begin
      FBeforeDragPos := Y;
      FDragOffset := Y - R.Top;
    end;
    AMargin.DraggingPos := FBeforeDragPos - FDragOffset;
  end;

var
  AMargin: TdxPreviewPageMargin;
begin
  AMargin := MarginFromPoint(X, Y);
  if CanDragMargin(AMargin) then
    StartMarginDragging(AMargin);
  if (AMargin = nil) and AllowZoomOnClick then
    Zoomed := not Zoomed;
end;

procedure TdxCustomPreview.ProcessLeftClickByPage(Shift: TShiftState; X, Y: Integer);
var
  AOldPageIndex, APageIndex: Integer;
begin
  APageIndex := FPages.PageIndexFromPoint(X, Y);
  if APageIndex <> -1 then
  begin
    if SelPageIndex = APageIndex then
      ProcessClickBySelectedPage(Shift, X, Y)
    else
    begin
      AOldPageIndex := SelPageIndex;
      SelPageIndex := APageIndex;
      if AOldPageIndex <> SelPageIndex then
        ResetHintShowTimer(X, Y);
    end;
  end;
end;

procedure TdxCustomPreview.SetSelPageIndex(Value: Integer);
begin
  Value := MinMax(Value, -1, PageCount - 1);

  if FSelPageIndex <> Value then
  begin
    if (Value = -1) or CanSelectPage(Value) then
    begin
      DoSelectedPageChanging;
      if FSelPageIndex < PageCount then
      begin
        InvalidatePage(FSelPageIndex);
        InvalidateMargins;
      end;
      FSelPageIndex := Value;
      if FSelPageIndex <> -1 then
      begin
        if not IsScrolling or IsScrolling and AllowMakeVisibleSelectedPageWhenScrolling then
        begin
          ResyncMargins;
          MakeVisible(FSelPageIndex);
        end;
        InvalidatePage(FSelPageIndex);
        InvalidateMargins;
      end;
      DoSelectedPageChanged;
    end;
  end;
end;

procedure TdxCustomPreview.UpdateSelectedPageIndex;
begin
// do nothing
end;

procedure TdxCustomPreview.UpdateScrollPositions;
begin
// do nothing
end;

procedure TdxCustomPreview.CheckMargins;
var
  I: Integer;
begin
  for I := 0 to Margins.Count - 1  do
    with Margins[I] do
      Value := Value;
end;

procedure TdxCustomPreview.InternalGoToFirstPage;
begin
  SelectFirstPage;
end;

procedure TdxCustomPreview.InternalGoToLastPage;
begin
  SelectLastPage;
end;

procedure TdxCustomPreview.InternalGoToNextPage;
begin
  SelectNextPage;
end;

procedure TdxCustomPreview.InternalGoToPrevPage;
begin
  SelectPrevPage;
end;

procedure TdxCustomPreview.ScrollPage(ADirection: TdxPreviewScrollDirection);
const
  BarMap: array[TdxPreviewScrollDirection] of TScrollBarKind = (sbHorizontal, sbVertical, sbHorizontal, sbVertical);
  ScrollCodeMap: array[TdxPreviewScrollDirection] of TScrollCode = (scLineUp, scLineUp, scLineDown, scLineDown);
var
  AScrollPos: Integer;
begin
  if CanPageScrolling(ADirection) then
    Scroll(BarMap[ADirection], ScrollCodeMap[ADirection], AScrollPos);
end;

function TdxCustomPreview.CreateBackgroundRegion(APage: TdxPreviewPage): TcxRegion;
var
  AMargins: TRect;
  APageSize: TdxPreviewPageSizeOptions;
begin
  APageSize := GetActualPageSizeOptions(APage);

  AMargins := cxRect(APageSize.Margins.Left, APageSize.Header, APageSize.Margins.Right, APageSize.Margins.Bottom);
  AMargins.BottomRight := LoMetricToPixels(AMargins.BottomRight);
  AMargins.TopLeft := LoMetricToPixels(AMargins.TopLeft);
  AMargins := cxRectScale(AMargins, ActualZoomFactor, 100);

  Result := TcxRegion.Create(cxRectContent(APage.Bounds, AMargins));
end;

function TdxCustomPreview.CanSelectPage(APageIndex: Integer): Boolean;
begin
  Result := True;
  if Assigned(OnSelectingPage) then
    OnSelectingPage(Self, APageIndex, Result);
end;

function TdxCustomPreview.CanShowMarginHint: Boolean;
begin
  Result := True;
  if Assigned(OnCanShowMarginHint) then
    OnCanShowMarginHint(Self, Result);
end;

procedure TdxCustomPreview.DoAfterDragMargin(AMargin: TdxPreviewPageMargin);
begin
  if Assigned(OnAfterDragMargin) then
    OnAfterDragMargin(Self, AMargin);
end;

procedure TdxCustomPreview.DoBeforeDragMargin(AMargin: TdxPreviewPageMargin);
begin
  if Assigned(OnBeforeDragMargin) then
    OnBeforeDragMargin(Self, AMargin);
end;

procedure TdxCustomPreview.DoCalcPageCount;
begin
  if FUpdateCount > 0 then Exit;
  if Assigned(OnCalcPageCount) then
    OnCalcPageCount(Self);
end;

procedure TdxCustomPreview.DoChangePageCount;
begin
  if Assigned(OnChangePageCount) then
    OnChangePageCount(Self);
end;

procedure TdxCustomPreview.DoDragMargin(AMargin: TdxPreviewPageMargin);
begin
  if Assigned(OnDragMargin) then
    OnDragMargin(Self, AMargin);
end;

procedure TdxCustomPreview.DoDrawPageContent(
  ACanvas: TcxCanvas; const R: TRect; APageIndex: Integer);
begin
  if Assigned(OnDrawPageContent) then
    OnDrawPageContent(Self, ACanvas.Canvas, R, APageIndex);
end;

procedure TdxCustomPreview.DoGetPageNumberHintText(out AText: string);
var
  StartPage, EndPage: Integer;
begin
  AText := DropAmpersand(cxGetResourceString(@sdxPreviewPage)) + ':  ';
  FPages.GetPartVisiblePageRanges(StartPage, EndPage);
  CalculatePageNumberHintText(StartPage, EndPage, AText);
  if Assigned(OnGetPageNumberHint) then
    OnGetPageNumberHint(Self, StartPage, EndPage, AText);
end;

procedure TdxCustomPreview.DoMarginChanged(AMargin: TdxPreviewPageMargin);
begin
  if Assigned(OnMarginChanged) then
    OnMarginChanged(Self, AMargin);
  if (AMargin <> nil) and AMargin.IsForceRecalculatePageCount then
    DoCalcPageCount;
end;

procedure TdxCustomPreview.DoZoomFactorChanged;
begin
  if Assigned(OnZoomFactorChanged) then
    OnZoomFactorChanged(Self);
end;

procedure TdxCustomPreview.DoZoomIn;
begin
  ZoomFactor := ZoomFactor + ZoomStep;
end;

procedure TdxCustomPreview.DoZoomModeChanged;
begin
  if Assigned(OnZoomModeChanged) then
    OnZoomModeChanged(Self);
end;

procedure TdxCustomPreview.DoZoomOut;
begin
  ZoomFactor := ZoomFactor - ZoomStep;
end;

procedure TdxCustomPreview.DoSelectedPageChanging;
begin
  if Assigned(OnSelectedPageChanging) then
    OnSelectedPageChanging(Self, FSelPageIndex);
end;

procedure TdxCustomPreview.DoSelectedPageChanged;
begin
  if Assigned(OnSelectedPageChanged) then
    OnSelectedPageChanged(Self, SelPageIndex);
end;

procedure TdxCustomPreview.DrawContent(ACanvas: TcxCanvas; const R: TRect);
begin
  DrawViewerBackground(ACanvas, R);
  ACanvas.Brush.Color := clNone; // reset brush
  if PageCount = 0 then
    DrawNoPages(ACanvas)
  else
    DrawPages(ACanvas);
end;

procedure TdxCustomPreview.DrawMargins(DC: HDC);
var
  I: Integer;
  Margin: TdxPreviewPageMargin;
begin
  for I := 0 to Margins.Count - 1 do
  begin
    Margin := Margins[I];
    if Margin.Visible then
      Margin.Draw(DC);
  end;
end;

procedure TdxCustomPreview.DrawPageBackground(ACanvas: TcxCanvas; APage: TdxPreviewPage; ASelected: Boolean);
begin
  if Assigned(OnDrawPageBackground) then
    OnDrawPageBackground(Self, ACanvas.Canvas, APage.Bounds, APage.Index);

  ASelected := AllowPageSelection and ASelected;
  LookAndFeelPainter.DrawPrintPreviewPageScaledBackground(ACanvas,
    APage.SiteBounds, APage.Bounds, ASelected, CanDrawBackground, ScaleFactor);
end;

procedure TdxCustomPreview.InvalidateMargins;
var
  I: Integer;
begin
  if CanShowMargins then
  begin
    for I := 0 to Margins.Count - 1 do
      Margins[I].Invalidate;
  end;
end;

function TdxCustomPreview.GetAbsoluteIndent: Integer;
begin
  Result := AbsoluteIndentLeft + AbsoluteIndentRight;
end;

function TdxCustomPreview.GetActualZoomFactor: Integer;
begin
  Result := ScaleFactor.Apply(ZoomFactor);
end;

function TdxCustomPreview.GetActualMeasurementUnits: TdxPreviewMeasurementUnits;
begin
  if MeasurementUnits <> pmuDefault then
    Result := MeasurementUnits
  else
    if DefaultMeasurementUnits <> pmuDefault then
      Result := DefaultMeasurementUnits
    else
      Result := GetDefaultMeasurementUnits;
end;

function TdxCustomPreview.GetClientHeight: Integer;
begin
  Result := cxRectHeight(ClientBounds);
end;

function TdxCustomPreview.GetClientWidth: Integer;
begin
  Result := cxRectWidth(ClientBounds);
end;

function TdxCustomPreview.GetContentHeight: Integer;
begin
  Result := AbsoluteIndent;
  if PageCount > 0 then
    Inc(Result, cxRectHeight(FPages.Bounds));
end;

function TdxCustomPreview.GetContentWidth: Integer;
begin
  if (PageCount = 0) or (ZoomMode = pzmPageWidth) then
    Result := ClientWidth
  else
    Result := AbsoluteIndent + cxRectWidth(FPages.Bounds);
end;

function TdxCustomPreview.GetIndent: Integer;
begin
  if ZoomFactor <= 100 then
    Result := AbsoluteIndent
  else
    Result := FIndent;
end;

function TdxCustomPreview.GetNoPagesText: string;
begin
  if FIsNoPagesTextAssigned then
    Result := FNoPagesText
  else
    Result := DefaultNoPagesText;
end;

procedure TdxCustomPreview.SetOptionsHint(Value: TdxPreviewOptionsHint);
var
  Changes: TdxPreviewOptionsHint;
begin
  Changes := FOptionsHint + Value - FOptionsHint * Value;
  if Changes <> [] then
    FOptionsHint := Value;
end;

procedure TdxCustomPreview.SetOptionsStore(Value: TdxPreviewOptionsStore);
var
  Changes: TdxPreviewOptionsStore;
begin
  Changes := FOptionsStore + Value - FOptionsStore * Value;
  if Changes <> [] then
    FOptionsStore := Value;
end;

procedure TdxCustomPreview.SetInternalOptionsBehavior(Value: TdxPreviewOptionsBehavior);
var
  Changes: TdxPreviewOptionsBehavior;
  I: Integer;
begin
  Changes := FInternalOptionsBehavior + Value - FInternalOptionsBehavior * Value;
  if Changes <> [] then
  begin
    FInternalOptionsBehavior := Value;
    if pobAllowDragMargins in Changes then
    begin
      for I := 0 to Margins.Count - 1 do
        Margins[I].Enabled := pobAllowDragMargins in Value;
    end;
  end;
end;

procedure TdxCustomPreview.SetInternalOptionsView(Value: TdxPreviewOptionsView);
var
  Changes: TdxPreviewOptionsView;
  I: Integer;
begin
  Changes := FInternalOptionsView + Value - FInternalOptionsView * Value;
  if Changes <> [] then
  begin
    FInternalOptionsView := Value;

    if HandleAllocated then
    begin
      if [povDefaultDrawPageBackground, povPageSelection] * Changes <> [] then
        InvalidatePages
      else
        if povAutoHideScrollBars in Changes then
          RecreateWnd;
    end;

    if povMargins in Changes then
    begin
      for I := 0 to Margins.Count - 1 do
        Margins[I].Visible := povMargins in Value;
    end;
  end;
end;

procedure TdxCustomPreview.SetInternalOptionsZoom(Value: TdxPreviewOptionsZoom);
var
  Changes: TdxPreviewOptionsZoom;
begin
  Changes := FInternalOptionsZoom + Value - FInternalOptionsZoom * Value;
  if Changes <> [] then
    FInternalOptionsZoom := Value;
end;

procedure TdxCustomPreview.SetMarginColor(Value: TColor);
begin
  if FMarginColor <> Value then
  begin
    FMarginColor := Value;
    RecreateMarginPen;
    if HandleAllocated then
      InvalidateMargins;
  end;
end;

procedure TdxCustomPreview.FreeMarginPen;
begin
  if FMarginPen <> 0 then
  begin
    DeleteObject(FMarginPen);
    FMarginPen := 0;
  end;
end;

procedure TdxCustomPreview.RecreateMarginPen;
begin
  FreeMarginPen;
  FMarginPen := CreatePen(PS_DOT, 1, ColorToRGB(MarginColor));
end;

procedure TdxCustomPreview.StopMarginDragging;
begin
  CancelDragMargin;
  DestroyPageNumberHint;
end;

function TdxCustomPreview.IsNoPagesTextStored: Boolean;
begin
  Result := FIsNoPagesTextAssigned and (FNoPagesText <> DefaultNoPagesText);
end;

function TdxCustomPreview.GetPage(index: Integer): TdxPreviewPage;
begin
  Result := FPages[index];
end;

function TdxCustomPreview.GetPageCount: Integer;
begin
  Result := FPages.Count;
end;

function TdxCustomPreview.GetPageBorders: TRect;
begin
  Result := LookAndFeelPainter.PrintPreviewPageBordersScaledWidth(ScaleFactor);
end;

function TdxCustomPreview.GetSelPageSizeOptions: TdxPreviewPageSizeOptions;
begin
  if SelPageIndex >= 0 then
    Result := GetActualPageSizeOptions(SelPage)
  else
    Result := PageSize;
end;

function TdxCustomPreview.GetSelPage: TdxPreviewPage;
begin
  if SelPageIndex >= 0 then
    Result := Pages[SelPageIndex]
  else
    Result := nil;
end;

procedure TdxCustomPreview.SetNoPagesText(const Value: string);
begin
  if NoPagesText <> Value then
  begin
    FNoPagesText := Value;
    FIsNoPagesTextAssigned := True;
    if PageCount = 0 then
      Invalidate;
  end;
end;

procedure TdxCustomPreview.SetMargins(Value: TdxPreviewPageMargins);
begin
  FMargins.Assign(Value);
  if FUpdateCount = 0 then
    Invalidate;
end;

procedure TdxCustomPreview.SetMaxZoomFactor(Value: Integer);
begin
  Value := Max(FMinZoomFactor, Value);
  if FMaxZoomFactor <> Value then
  begin
    FMaxZoomFactor := Value;
    ZoomFactor := Min(FMaxZoomFactor, ZoomFactor);
  end;
end;

procedure TdxCustomPreview.SetMinZoomFactor(Value: Integer);
begin
  Value := MinMax(Value, GetDefaultMinZoomFactor, FMaxZoomFactor);
  if FMinZoomFactor <> Value then
  begin
    FMinZoomFactor := Value;
    ZoomFactor := Max(ZoomFactor, FMinZoomFactor);
  end;
end;

procedure TdxCustomPreview.SetOnCalcPageCount(Value: TNotifyEvent);
begin
  if @FOnCalcPageCount <> @Value then
  begin
    FOnCalcPageCount := Value;
    if Assigned(FOnCalcPageCount) and ([csReading, csLoading] * ComponentState = []) then
    begin
      DoCalcPageCount;
      if HandleAllocated then
        Invalidate;
    end;
  end;
end;

procedure TdxCustomPreview.PageParametersChanged(Sender: TObject);
begin
  if ZoomMode <> pzmNone then
    FZoomFactor := 100;
  ResyncMargins;
  CheckMargins;
  LayoutChanged;
end;

procedure TdxCustomPreview.SetPageCount(Value: Integer);
var
  I: Integer;
begin
  Value := Max(Value, 0);
  if PageCount <> Value then
  begin
    if Value < PageCount then
    begin
      for I := Value to FPages.Count - 1 do
        FPages[I].Free;
      FPages.Count := Value;
    end
    else
    begin
      for I := PageCount to Value - 1 do
        CreatePage;
    end;
    // check SelPageIndex
    if not (csDestroying in ComponentState) then
    begin
      ResyncSelPageIndex;
      LayoutChanged;
    end;
  end;
end;

procedure TdxCustomPreview.SetPageSize(AValue: TdxPreviewPageSizeOptions);
begin
  FPageSize.Assign(AValue);
end;

procedure TdxCustomPreview.ResyncSelPageIndex;
var
  Value: Integer;
begin
  Value := SelPageIndex;
  while (Value > -1) and not CanSelectPage(Value) do
    Dec(Value);
  if Value = -1 then
  begin
    Value := 0;
    while (Value < PageCount) and not CanSelectPage(Value) do
      Inc(Value);
  end;
  if Value = PageCount then
    Value := -1;
  SelPageIndex := Value;
end;

procedure TdxCustomPreview.SetPageXCount(Value: Integer);
begin
  Value := Max(1, Value);
  if FPageXCount <> Value then
  begin
    FPageXCount := Value;
    FZoomMode := pzmPages;
    LayoutChanged;
  end;
end;

procedure TdxCustomPreview.SetPageYCount(Value: Integer);
begin
  Value := Max(1, Value);
  if FPageYCount <> Value then
  begin
    FPageYCount := Value;
    FZoomMode := pzmPages;
    LayoutChanged;
  end;
end;

procedure TdxCustomPreview.SetZoomed(Value: Boolean);
begin
  if not FZoomed and Value then
  begin
    FZoomed := True;
    FUnzoomedFactor := ZoomFactor;
    FUnzoomedMode := ZoomMode;
    ZoomFactor := 100;
  end
  else
    if FZoomed then
    begin
      if ZoomFactor = 100 then
      begin
        FZoomed := False;
        ZoomMode := FUnzoomedMode;
        ZoomFactor := FUnzoomedFactor;
      end
      else
        ZoomFactor := 100;
    end;

  MakeVisible(SelPageIndex);
  UpdateWindow(Handle);
end;

procedure TdxCustomPreview.SetZoomStep(Value: Integer);
begin
  FZoomStep := Max(Value, 1);
end;

procedure TdxCustomPreview.SetZoomFactor(Value: Integer);
begin
  Value := MinMax(Value, FMinZoomFactor, FMaxZoomFactor);
  if FZoomFactor <> Value then
  begin
    BeginUpdate;
    try
      FPrevZoomFactor := FZoomFactor;
      FZoomFactor := Value;
      if [psZooming] * State = [] then
      begin
        FZoomMode := pzmNone;
        FZoomed := FZoomFactor >= 100;
      end;
      LayoutChanged;
      UpdateScrollPositions;
    finally
      EndUpdate;
    end;
    DoZoomFactorChanged;
  end;
end;

procedure TdxCustomPreview.SetZoomMode(Value: TdxPreviewZoomMode);
begin
  if FZoomMode <> Value then
  begin
    FZoomMode := Value;
    LayoutChanged;
    DoZoomModeChanged;
  end;
end;

function TdxCustomPreview.CanDrawBackground: Boolean;
begin
  Result := not Assigned(OnDrawPageBackground) and IsDefaultDrawPageBackground;
end;

procedure TdxCustomPreview.CreateHint;
begin
  if FHintWindow = nil then
    FHintWindow := TdxPreviewHintWindow.Create(nil);
end;

procedure TdxCustomPreview.DestroyHint;
begin
  FreeAndNil(FHintWindow);
end;

procedure TdxCustomPreview.ActivateHint(AMargin: TdxPreviewPageMargin);
var
  Pt: TPoint;
begin
  if FHintWindow = nil then Exit;
  Pt := GetMouseCursorClientPos;
  with AMargin do
  begin
    if IsVertical then
      if IsDragging then
        Pt.X := DraggingPos
      else
        Pt.X := Bounds.Left
    else
      if IsDragging then
        Pt.Y := DraggingPos
      else
        Pt.Y := Bounds.Top;
    Windows.ClientToScreen(Handle, Pt);
  end;
  TdxPreviewHintWindow(FHintWindow).ActivateHint(Pt, AMargin.DisplayText, AMargin);
end;

function TdxCustomPreview.AllowHybridScrollbarMode: Boolean;
begin
  Result := False;
end;

procedure TdxCustomPreview.CancelHintHide;
begin
  FHintHideShortTimer.Enabled := False;
end;

procedure TdxCustomPreview.CancelHintShow;
begin
  FHintShowTimer.Enabled := False;
  DestroyHint;
end;

procedure TdxCustomPreview.StartHintShow;
begin
  FHintShowTimer.Enabled := True;
end;

procedure TdxCustomPreview.ClearLastMousePos;
begin
  FLastMousePos := Point(MaxInt, MaxInt);
end;

procedure TdxCustomPreview.CancelDragMargin;
begin
  if DraggingMargin = nil then Exit;
  DraggingMargin.DraggingPos := NullDraggingPos;
  DraggingMargin.DoDragAfter;
  FDraggingMargin := nil;
  ClearLastMousePos;
end;

procedure TdxCustomPreview.DestroyPageNumberHint;
begin
  FreeAndNil(FHintWindowPageNumber);
end;

procedure TdxCustomPreview.UpdatePageNumberHint;
var
  HintText: string;
begin
  if FHintWindowPageNumber = nil then
    FHintWindowPageNumber := TdxPreviewHintWindow.Create(nil);
  DoGetPageNumberHintText(HintText);
  TdxPreviewHintWindow(FHintWindowPageNumber).ActivateHint(GetMouseCursorPos, HintText, nil);
end;

function TdxCustomPreview.CanChangeMargins: Boolean;
begin
  Result := [csReading, csLoading] * ComponentState = [];
end;

function TdxCustomPreview.SelectPage(APageIndex: Integer): Boolean;
begin
  Result := CanSelectPage(APageIndex);
  if Result then
    SelPageIndex := APageIndex;
end;

procedure TdxCustomPreview.ChangeSelPageIndex(ADirection: Integer);
var
  I: Integer;
begin
  I := 1;
  repeat
    if SelectPage(SelPageIndex + ADirection * I) then
      Break;
    Inc(I);
  until (SelPageIndex + ADirection * I = PageCount + 1) or (SelPageIndex - ADirection * I = -2);
end;

function TdxCustomPreview.IsEnabledBehaviorOption(AOption: TdxPreviewOptionBehavior): Boolean;
begin
  Result := AOption in InternalOptionsBehavior;
end;

function TdxCustomPreview.IsEnabledViewOption(AOption: TdxPreviewOptionView): Boolean;
begin
  Result := AOption in InternalOptionsView;
end;

procedure TdxCustomPreview.CalculateAbsoluteIndents;
begin
  FAbsoluteIndentLeft := GetAbsoluteIndentLeft;
  FAbsoluteIndentRight := GetAbsoluteIndentRight;
end;

procedure TdxCustomPreview.CalculateIndent;
begin
  FIndent := AbsoluteIndent * ZoomFactor div 100;
end;

procedure TdxCustomPreview.CalculateLayout(AType: TdxChangeType);
begin
  if AType <> ctLight then
  begin
    CalculateZoomFactor;
    CalculateAbsoluteIndents;
    CalculateIndent;
  end;
  CalculatePagesLayout;
end;

procedure TdxCustomPreview.CalculateZoomFactor;
var
  AFirstPageIndex: Integer;
  ALastPageIndex: Integer;
  APageBounds: TRect;
begin
  if (PageCount = 0) or not HandleAllocated or (psZooming in State) then
    Exit;

  Include(FState, psZooming);
  try
    FPages.GetPartVisiblePageRanges(AFirstPageIndex, ALastPageIndex);

    if AFirstPageIndex >= 0 then
    begin
      APageBounds := Pages[AFirstPageIndex].Bounds;
      case ZoomMode of
        pzmPageWidth:
          CalculateZoomFactorForPageWidthPreviewZoomMode(AFirstPageIndex, ALastPageIndex);
        pzmPages:
          CalculateZoomFactorForPagesPreviewZoomMode(AFirstPageIndex, ALastPageIndex);
      end;
    end;
  finally
    Exclude(FState, psZooming);
  end;
end;

procedure TdxCustomPreview.DrawNoPages(ACanvas: TcxCanvas);
var
  R: TRect;
begin
  R := ClientRect;
  ACanvas.Font.Assign(Font);
  ACanvas.Font.Color := LookAndFeelPainter.PrintPreviewBackgroundTextColor;
  ACanvas.Brush.Style := bsClear;
  cxDrawText(ACanvas.Handle, NoPagesText, R, DT_CALCRECT or DT_CENTER or DT_WORDBREAK or DT_TOP);
  R := cxRectCenter(ClientRect, cxRectWidth(R), cxRectHeight(R));
  cxDrawText(ACanvas.Handle, NoPagesText, R, DT_WORDBREAK);
end;

procedure TdxCustomPreview.DrawPage(ACanvas: TcxCanvas; APage: TdxPreviewPage; ASelected: Boolean);
begin
  if ACanvas.RectVisible(APage.Bounds) then
  begin
    DrawPageBackground(ACanvas, APage, ASelected);
    APage.Draw(ACanvas);
    if CanShowMargins and ASelected then
      DrawMargins(ACanvas.Handle);
  end;
end;

procedure TdxCustomPreview.DrawPages(ACanvas: TcxCanvas);
var
  I: Integer;
begin
  if not IsUpdateLocked then
    for I := 0 to PageCount - 1 do
      DrawPage(ACanvas, Pages[I], I = SelPageIndex);
end;

procedure TdxCustomPreview.DrawViewerBackground(ACanvas: TcxCanvas; const R: TRect);
begin
  if Transparent then
    cxDrawTransparentControlBackground(Self, ACanvas, R)
  else
    LookAndFeelPainter.DrawPrintPreviewScaledBackground(ACanvas, R, ScaleFactor);
end;

function TdxCustomPreview.IsScrolling: Boolean;
begin
  Result := psScrolling in State;
end;

function TdxCustomPreview.CanAnyScrolling: Boolean;
begin
  Result := CanHorzScrolling or CanVertScrolling;
end;

function TdxCustomPreview.CanHorzScrolling: Boolean;
begin
  Result := (ZoomMode <> pzmPageWidth) and (ClientWidth < ContentWidth);
end;

function TdxCustomPreview.CanVertScrolling: Boolean;
begin
  Result := ClientHeight < ContentHeight;
end;

function TdxCustomPreview.CanPageScrolling(ADirection: TdxPreviewScrollDirection): Boolean;
begin
  Result := ((ZoomMode <> pzmPages) or (PageCount <> 1)) and
    ((ADirection in [psdLeft, psdRight]) and CanHorzScrolling) or
    ((ADirection in [psdUp, psdDown]) and CanVertScrolling);
end;

function TdxCustomPreview.IsDefaultGesture(AGestureID: Integer): Boolean;
begin
  Result := inherited IsDefaultGesture(AGestureID) or (AGestureID = GID_ZOOM);
end;

procedure TdxCustomPreview.ResetHintShowTimer(X, Y: Integer);
begin
  CancelHintShow;
  if not IsDesigning and IsParentFormActive and (pohShowForMargins in OptionsHint) and
    (GetHitInfoAt(X, Y) * phtMargins <> []) and CanShowMarginHint
  then
    StartHintShow;
end;

function TdxCustomPreview.IsDesigning: Boolean;
begin
  Result := csDesigning in ComponentState;
end;

function TdxCustomPreview.IsDoubleBufferedNeeded: Boolean;
begin
  Result := True;
end;

function TdxCustomPreview.IsParentFormActive: Boolean;
var
  Form: TCustomForm;
begin
  Form := GetParentForm(Self);
  Result := (Form <> nil) and Form.Active;
end;

procedure TdxCustomPreview.HintHideLongTimerHandler(Sender: TObject);
begin
end;

procedure TdxCustomPreview.HintHideShortTimerHandler(Sender: TObject);
begin
  if not IsParentFormActive or (Time - FLongShowHintTime > dxPreviewHideHintLongTime) then
  begin
    DestroyHint;
    //FHintHideTimer.Enabled := False;
  end;
end;

procedure TdxCustomPreview.HintShowTimerHandler(Sender: TObject);
var
  Margin: TdxPreviewPageMargin;
begin
  FHintShowTimer.Enabled := False;
  Margin := MarginFromPoint(GetMouseCursorClientPos);
  if (Margin <> nil) and (not Margin.IsDragging or (pohShowOnDrag in OptionsHint)) then
  begin
    CreateHint;
    ActivateHint(Margin);
    FLongShowHintTime := GetTickCount;
    FHintHideShortTimer.Enabled := True;
  end;
end;

procedure TdxCustomPreview.ReadIsNoPagesTextAssigned(AReader: TReader);
begin
  FIsNoPagesTextAssigned := AReader.ReadBoolean;
end;

procedure TdxCustomPreview.WriteIsNoPagesTextAssigned(AWriter: TWriter);
begin
  AWriter.WriteBoolean(FIsNoPagesTextAssigned);
end;

procedure TdxCustomPreview.WMCaptureChanged(var Message: TMessage);
begin
  if not FIsMarginDropping then
    StopMarginDragging;
  inherited;
end;

procedure TdxCustomPreview.WMGetDlgCode(var Message: TWMGetDlgCode);
const
  AllKeys: array[Boolean] of LongInt = (0, DLGC_WANTALLKEYS);
  ArrowKeys: array[Boolean] of LongInt = (0, DLGC_WANTARROWS);
begin
  inherited;
  Message.Result := Message.Result or
    AllKeys[DraggingMargin <> nil] or ArrowKeys[AllowKeyNavigation];
end;

procedure TdxCustomPreview.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result := 1;
end;

procedure TdxCustomPreview.WMKillFocus(var Message: TWMKillFocus);
begin
  ClearLastMousePos;
  HideAllHints;
  inherited;
end;

procedure TdxCustomPreview.WMRButtonUp(var Message: TWMRButtonUp);
begin
  HideAllHints;
  inherited;
end;

procedure TdxCustomPreview.WMLButtonUp(var Message: TWMLButtonUp);
begin
  FIsMarginDropping := True;
  inherited;
  FIsMarginDropping := False;
end;

procedure TdxCustomPreview.WMMouseActivate(var Message: TWMMouseActivate);
var
  Control: TWinControl;
  Pt: TPoint;
begin
  inherited;
  if not IsDesigning then
  begin
    Control := FindControl(GetFocus);
    if (Control = nil) or (GetParentForm(Control) <> GetParentForm(Self)) then
    begin
      Pt := GetMouseCursorClientPos;
      if (FPages.PageIndexFromPoint(Pt) > -1) {and not Assigned(MarginFromPoint(APt)) } then
        Message.Result := MA_ACTIVATEANDEAT;
    end;
    if CanFocus then
      SetFocus;
  end;
end;

procedure TdxCustomPreview.WMMouseWheel(var Message: TWMMouseWheel);
begin
  if DraggingMargin <> nil then
    Perform(WM_LBUTTONUP, Message.Keys, TMessage(Message).lParam);

  if not ProcessMouseWheelMessage(Message.WheelDelta) then
    inherited;
end;

procedure TdxCustomPreview.WMNCDestroy(var Message: TWMNCDestroy);
begin
  if not (csDestroying in ComponentState) then
    HideAllHints;
  inherited;
end;

procedure TdxCustomPreview.WMNCHitTest(var Message: TWMNCHitTest);
begin
  DefaultHandler(Message);
end;

procedure TdxCustomPreview.WMSetCursor(var Message: TWMSetCursor);
var
  Cursor: HCURSOR;
  Margin: TdxPreviewPageMargin;
  Pt: TPoint;
  HitInfo: TdxPreviewHitTests;
begin
  Cursor := 0;
  Pt := GetMouseCursorClientPos;
  if (Message.HitTest = HTCLIENT) and not IsScrollBarsArea(Pt) then
  begin
    Margin := DraggingMargin;
    if (Margin = nil) and IsParentFormActive then
    begin
      HitInfo := GetHitInfoAt(Pt);
      if phtPage in HitInfo then
        if phtMargins * HitInfo <> [] then
        begin
          Margin := MarginFromPoint(Pt);
          if Margin.Enabled then
            Cursor := Screen.Cursors[Margin.GetCursor];
        end
        else
          if not IsDesigning and AllowZoomOnClick and (FPages.PageIndexFromPoint(Pt) = SelPageIndex) then
            Cursor := Screen.Cursors[GetCursor];
    end;
  end;

  if Cursor <> 0 then
    SetCursor(Cursor)
  else
    inherited;
end;

procedure TdxCustomPreview.CMCancelMode(var Message: TCMCancelMode);
begin
  HideAllHints;
  inherited;
end;

procedure TdxCustomPreview.CMHintShow(var Message: TCMHintShow);
begin
  inherited;
  Message.Result := Integer((DraggingMargin <> nil) and (FPages.PageIndexFromPoint(Message.HintInfo^.CursorPos) <> -1));
end;

procedure TdxCustomPreview.CMSysColorChange(var Message: TMessage);
begin
  inherited;
  if MarginColor and $80000000 = $80000000 then
  begin
    RecreateMarginPen;
    InvalidateMargins;
  end;
end;

procedure TdxCustomPreview.DesignerModified;
var
  Designer: IDesignerNotify;
begin
  if IsDesigning then
  begin
    Designer := GetParentForm(Self).Designer;
    if Designer <> nil then
      Designer.Modified;
  end;
end;

function DefineCursor(const AResName: PChar): TCursor;
var
  Handle: HCURSOR;
begin
  Result := crDefault;
  Handle := LoadCursor(hInstance, AResName);
  if Handle > 0 then
  begin
    for Result := 100 to High(TCursor) do
      if Screen.Cursors[Result] = Screen.Cursors[crDefault] then
      begin
        Screen.Cursors[Result] := Handle;
        Exit;
      end;
    DestroyCursor(Handle);
    raise EOutOfResources.Create(cxGetResourceString(@sdxPreviewOutOfResources));
  end;
end;

procedure DefineCursors;
begin
  crdxPreviewHorzResize := DefineCursor(IDC_DXPREVIEW_MARGINSMOVEHORZ);
  crdxPreviewVertResize := DefineCursor(IDC_DXPREVIEW_MARGINSMOVEVERT);
  crdxPreviewZoomIn := DefineCursor(IDC_DXPREVIEW_ZOOMIN);
  crdxPreviewZoomOut := DefineCursor(IDC_DXPREVIEW_ZOOMOUT);
end;

{ TdxPreviewPageContentCache }

procedure TdxPreviewPageContentCache.CheckSize;
begin
  if (cxRectWidth(Page.Bounds) <> Width) or (cxRectHeight(Page.Bounds) <> Height) then
  begin
    SetSize(Page.Bounds);
    Dirty := True;
  end;
end;

procedure TdxPreviewPageContentCache.SetPage(AValue: TdxPreviewPage);
begin
  if FPage <> AValue then
  begin
    FPage := AValue;
    Dirty := True;
  end;
end;

{ TdxPreviewPageContentCachePool }

constructor TdxPreviewPageContentCachePool.Create(ACapacity: Integer);
begin
  inherited Create;
  FCapacity := ACapacity;
  FList := TcxObjectList.Create;
  FList.Capacity := Capacity;
end;

destructor TdxPreviewPageContentCachePool.Destroy;
begin
  FreeAndNil(FList);
  inherited Destroy;
end;

function TdxPreviewPageContentCachePool.Add(APage: TdxPreviewPage): TdxPreviewPageContentCache;
begin
  if not (Find(APage, Result) or Find(nil, Result)) then
  begin
    if Count < Capacity then
    begin
      Result := TdxPreviewPageContentCache.Create;
      FList.Add(Result);
    end
    else
      if not FindInvisible(nil, Result) then
        Result := Items[0];
  end;
  Result.Page := APage;
  Result.CheckSize;
end;

procedure TdxPreviewPageContentCachePool.Invalidate(APage: TdxPreviewPage);
var
  AItem: TdxPreviewPageContentCache;
begin
  if Find(APage, AItem) then
    AItem.Dirty := True;
end;

procedure TdxPreviewPageContentCachePool.InvalidateAll;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].Dirty := True;
end;

procedure TdxPreviewPageContentCachePool.Remove(APage: TdxPreviewPage);
var
  AItem: TdxPreviewPageContentCache;
begin
  if Find(APage, AItem) then
    AItem.Page := nil;
end;

procedure TdxPreviewPageContentCachePool.Clear;
begin
  FList.Clear;
end;

function TdxPreviewPageContentCachePool.Find(APage: TdxPreviewPage; out AItem: TdxPreviewPageContentCache): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Count - 1 do
  begin
    Result := APage = Items[I].Page;
    if Result then
    begin
      AItem := Items[I];
      Break;
    end;
  end;
end;

function TdxPreviewPageContentCachePool.FindInvisible(APage: TdxPreviewPage;
  out AItem: TdxPreviewPageContentCache): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Count - 1 do
  begin
    AItem := Items[I];
    Result := (AItem.Page <> nil) and not AItem.Page.PartVisible;
    if Result then
    begin
      AItem := Items[I];
      Break;
    end;
  end;
end;

function TdxPreviewPageContentCachePool.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TdxPreviewPageContentCachePool.GetItem(Index: Integer): TdxPreviewPageContentCache;
begin
  Result := TdxPreviewPageContentCache(FList[Index]);
end;

initialization
  DefineCursors;

end.
