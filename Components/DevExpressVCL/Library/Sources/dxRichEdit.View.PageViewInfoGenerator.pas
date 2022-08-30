{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressRichEditControl                                   }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSRICHEDITCONTROL AND ALL        }
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

unit dxRichEdit.View.PageViewInfoGenerator;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface


uses
  Types, SysUtils, Classes, Generics.Defaults, Generics.Collections, dxTypeHelpers, dxCoreClasses,

  dxGenerics,
  dxRichEdit.Utils.Types,
  dxRichEdit.View.ViewInfo,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Boxes,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentLayout.UnitConverter,
  dxRichEdit.Types;

type
  TdxPageGeneratorLayoutManager = class;
  TdxPageViewInfoGeneratorStateBase = class;
  TdxPageViewInfoGeneratorRunningHeight = class;
  TdxPageViewInfoGeneratorFirstPageOffset = class;
  TdxPageViewInfoGeneratorBase = class;
  TdxPageViewInfoRow = class;
  TdxOwnedRowInfoList = class;

  TdxPageViewInfoGeneratorState = (InvisibleEmptyRow, InvisibleRow, PartialVisibleEmptyRow,
    PartialVisibleRow, VisibleEmptyRow, VisibleRow, VisiblePagesGenerationComplete);

  TdxStateProcessPageResult = (Success, TryAgain);

  TdxProcessPageResult = (VisiblePagesGenerationComplete, VisiblePagesGenerationIncomplete);

  TdxPageVisibility = (Entire, Partial, Invisible);

  { TdxPageViewInfoRow }

  TdxPageViewInfoRow = class(TdxPageViewInfoCollection)
  strict private
    FBounds: TRect;
  protected
    function FindByBound(X: Integer; out AFoundIndex: Integer): Boolean;
  public
    constructor Create(AOwner: TdxOwnedRowInfoList);
    destructor Destroy; override;
    function GetPageAtPoint(const APoint: TPoint; AStrictSearch: Boolean): TdxPageViewInfo;
    function IntersectsWithHorizontalLine(ALineY: Integer): Boolean;

    property Bounds: TRect read FBounds write FBounds;
  end;
  TdxOwnedRowInfoList = class(TdxObjectList<TdxPageViewInfoRow>);

  { PageViewInfoRowCollection }

  TdxPageViewInfoRowCollection = class(TdxList<TdxPageViewInfoRow>)
  protected
    function FindByBound(Y: Integer; out AFoundIndex: Integer): Boolean;
  public
    function GetRowAtPoint(const APoint: TPoint; AStrictSearch: Boolean): TdxPageViewInfoRow;

    function First: TdxPageViewInfoRow;
    function Last: TdxPageViewInfoRow;
  end;

  { TdxPageGeneratorLayoutManager }

  TdxPageGeneratorLayoutManager = class(TcxIUnknownObject)
  strict private
    FScaleFactor: Single;
    FHorizontalPageGap: Integer;
    FVerticalPageGap: Integer;
    FViewPortBounds: TRect;
  strict protected
    FViewInfo: TObject;
    FView: TObject;
  protected
    function CreateEmptyClone: TdxPageGeneratorLayoutManager; virtual; abstract;
    procedure CopyFrom(AManager: TdxPageGeneratorLayoutManager);
    function CalculateVisibleHeightFactor(APageViewInfo: TdxPageViewInfo; const AViewPort: TRect): Single; virtual;
    function CalculateVisibleWidthFactor(APageViewInfo: TdxPageViewInfo; const AViewPort: TRect): Single; virtual;
    procedure OffsetRowVertically(ARow: TdxPageViewInfoRow; AOffset: Integer);
    function CreateInitialPageViewInfoRowBounds(Y: Integer): TRect; virtual;
    function CalculateFirstPageLeftOffset(ATotalPagesWidth: Integer): Integer; virtual;
    procedure AlignPages(ARow: TdxPageViewInfoRow); virtual;
    procedure AlignPagesVertically(ARow: TdxPageViewInfoRow); virtual;
    procedure AlignPagesHorizontally(ARow: TdxPageViewInfoRow; ARowLeftOffset: Integer); virtual;
    function GetHorizontalPageGap: Integer; virtual;
    function GetVerticalPageGap: Integer; virtual;
    procedure SetHorizontalPageGap(Value: Integer); virtual;
    procedure SetVerticalPageGap(Value: Integer); virtual;
    procedure UpdatePagesClientBounds(ARow: TdxPageViewInfoRow); virtual;

    procedure OnLayoutUnitChanging(AUnitConverter: TdxDocumentLayoutUnitConverter); virtual;
    procedure OnLayoutUnitChanged(AUnitConverter: TdxDocumentLayoutUnitConverter); virtual;
  public
    constructor Create(AView: TObject; AViewInfo: TObject); virtual;
    // IdxCloneable
    function Clone: TdxPageGeneratorLayoutManager;

    function CalculatePageVisibleLogicalWidth(APageViewInfo: TdxPageViewInfo): Integer; overload; virtual;
    function CalculatePageVisibleLogicalWidth(APageViewInfo: TdxPageViewInfo; const AViewPort: TRect): Integer; overload; virtual;
    function CalculatePageVisibleLogicalHeight(APageViewInfo: TdxPageViewInfo): Integer; overload; virtual;
    function CalculatePageVisibleLogicalHeight(APageViewInfo: TdxPageViewInfo; const AViewPort: TRect): Integer; overload; virtual;
    function CalculatePagesTotalLogicalHeight(ARow: TdxPageViewInfoRow): Int64; virtual;
    function CalculatePagesTotalVisibleLogicalHeight(ARow: TdxPageViewInfoRow): Int64; overload; virtual;
    function CalculatePagesTotalVisibleLogicalHeight(ARow: TdxPageViewInfoRow; const AViewPort: TRect): Int64; overload; virtual;
    function CalculatePagesTotalLogicalHeightAbove(ARow: TdxPageViewInfoRow; ALogicalY: Integer): Int64; virtual;
    function CalculatePagesTotalLogicalHeightBelow(ARow: TdxPageViewInfoRow; ALogicalY: Integer): Int64; virtual;
    function CalculatePageLogicalWidth(APage: TdxPage): Integer; virtual;
    function CalculatePageLogicalTotalWidth(APage: TdxPage): Integer; virtual;
    function CalculatePagePhysicalWidth(APage: TdxPage): Integer; virtual;
    function CalculatePageLogicalHeight(APage: TdxPage): Integer; virtual;
    function CalculatePagePhysicalHeight(APage: TdxPage): Integer; virtual;
    function CalculatePageLogicalSize(APage: TdxPage): TSize; virtual;
    function CalculatePagePhysicalSize(APage: TdxPage): TSize; virtual;
    function CanFitPageToPageRow(APage: TdxPage; ARow: TdxPageViewInfoRow): Boolean; virtual;

    function OffsetRectangle(const ARect: TRect; ADeltaX, ADeltaY: Integer): TRect;
    procedure UpdatePageClientBounds(APageViewInfo: TdxPageViewInfo); virtual;

    property ScaleFactor: Single read FScaleFactor write FScaleFactor;
    property ViewPortBounds: TRect read FViewPortBounds write FViewPortBounds;
    property HorizontalPageGap: Integer read GetHorizontalPageGap write SetHorizontalPageGap;
    property VerticalPageGap: Integer read GetVerticalPageGap write SetVerticalPageGap;
  end;

  { TdxPageViewInfoGenerator }

  TdxPageViewInfoGenerator = class(TdxPageGeneratorLayoutManager)
  strict private
    FTotalWidth: Int64;
    FVisibleWidth: Int64;
    FLeftInvisibleWidth: Int64;
    FMaxPageWidth: Int64;
    FRunningHeightAnchor: TdxRunningHeightFirstPageAnchor;
    FFirstPageOffsetAnchor: TdxFirstPageOffsetFirstPageAnchor;
    FRunningHeightGenerator: TdxPageViewInfoGeneratorRunningHeight;
    FFirstPageOffsetGenerator: TdxPageViewInfoGeneratorFirstPageOffset;
    FActiveGenerator: TdxPageViewInfoGeneratorBase;
    function GetVisibleHeight: Int64;
    function GetTotalHeight: Int64;
    function GetTopInvisibleHeight: Int64;
    procedure SetTopInvisibleHeight(const Value: Int64);
  protected
    procedure UpdateRunningHeightAnchor; virtual;
    procedure UpdateFirstPageOffsetAnchor; virtual;
    function LookupFirstVisiblePage(ARow: TdxPageViewInfoRow): TdxPageViewInfo; virtual;
  public
    constructor Create(AView: TObject; AViewInfo: TObject); override;
    destructor Destroy; override;
    procedure CalculateMaxPageWidth; virtual;
    procedure CalculateWidthParameters; virtual;

    function GetVerticalScrollBarLargeChange: Integer; virtual;

    function PreProcessPage(APage: TdxPage; APageIndex: Integer): TdxProcessPageResult; virtual;
    function ProcessPage(APage: TdxPage; APageIndex: Integer): TdxProcessPageResult; virtual;
    procedure ResetAnchors;
    procedure Reset(AStrategy: TdxPageGenerationStrategyType); virtual;
    function GetPageRowAtPoint(const APoint: TPoint): TdxPageViewInfoRow; overload; virtual;
    function GetPageRowAtPoint(const APoint: TPoint; AStrictSearch: Boolean): TdxPageViewInfoRow; overload; virtual;

    procedure OnLayoutUnitChanging(AUnitConverter: TdxDocumentLayoutUnitConverter); override;
    procedure OnLayoutUnitChanged(AUnitConverter: TdxDocumentLayoutUnitConverter); override;

    property TotalWidth: Int64 read FTotalWidth write FTotalWidth;
    property VisibleWidth: Int64 read FVisibleWidth write FVisibleWidth;
    property MaxPageWidth: Int64 read FMaxPageWidth write FMaxPageWidth;
    property LeftInvisibleWidth: Int64 read FLeftInvisibleWidth write FLeftInvisibleWidth;
    property VisibleHeight: Int64 read GetVisibleHeight;
    property TotalHeight: Int64 read GetTotalHeight;
    property TopInvisibleHeight: Int64 read GetTopInvisibleHeight write SetTopInvisibleHeight;
    property ActiveGenerator: TdxPageViewInfoGeneratorBase read FActiveGenerator;
    property RunningHeightAnchor: TdxRunningHeightFirstPageAnchor read FRunningHeightAnchor;
    property FirstPageOffsetAnchor: TdxFirstPageOffsetFirstPageAnchor read FFirstPageOffsetAnchor;
  end;

  { TdxPageViewInfoGeneratorBase }

  TdxPageViewInfoGeneratorBase = class
  strict private
    FPages: TdxPageViewInfoCollection;
    FPageRows: TdxPageViewInfoRowCollection;
    FCurrentPageRow: TdxPageViewInfoRow;
    FTotalHeight: Int64;
    FVisibleHeight: Int64;
    FState: TdxPageViewInfoGeneratorStateBase;
    FFirstPageAnchor: TdxFirstPageAnchor;
    FLayoutManager: TdxPageGeneratorLayoutManager;
    FOnRowFinished: TdxEventHandler;
    function GetRowOwner: TdxOwnedRowInfoList;
    procedure SetCurrentPageRow(Value: TdxPageViewInfoRow);
  protected
    FInvisible: Boolean;
    procedure RaiseRowFinished;
    procedure OnLayoutUnitChanging; virtual;
    procedure OnLayoutUnitChanged; virtual;
    procedure AddPageRow(ARow: TdxPageViewInfoRow);
    function CreateInvisibleEmptyRowState: TdxPageViewInfoGeneratorStateBase; virtual; abstract;
    function CreateInvisibleRowState: TdxPageViewInfoGeneratorStateBase; virtual; abstract;
    function CreatePartialVisibleEmptyRowState: TdxPageViewInfoGeneratorStateBase; virtual; abstract;
    function CreatePartialVisibleRowState: TdxPageViewInfoGeneratorStateBase; virtual; abstract;
    function CreateState(AStateType: TdxPageViewInfoGeneratorState): TdxPageViewInfoGeneratorStateBase;

    property RowOwner: TdxOwnedRowInfoList read GetRowOwner;
  public
    constructor Create(ALayoutManager: TdxPageGeneratorLayoutManager; AFirstPageAnchor: TdxFirstPageAnchor; APages: TdxPageViewInfoCollection);
    destructor Destroy; override;

    function ChangeState(AStateType: TdxPageViewInfoGeneratorState; AFreePrevState: Boolean = True): TdxPageViewInfoGeneratorStateBase; virtual;
    function GetPageRowAtPoint(const APoint: TPoint; AStrictSearch: Boolean): TdxPageViewInfoRow; virtual;
    function PreProcessPage(APage: TdxPage; APageIndex: Integer): TdxProcessPageResult; virtual;
    function ProcessPage(APage: TdxPage; APageIndex: Integer): TdxProcessPageResult; virtual;

    property CurrentPageRow: TdxPageViewInfoRow read FCurrentPageRow write SetCurrentPageRow;
    property FirstPageAnchor: TdxFirstPageAnchor read FFirstPageAnchor;
    property LayoutManager: TdxPageGeneratorLayoutManager read FLayoutManager;
    property PageRows: TdxPageViewInfoRowCollection read FPageRows;
    property Pages: TdxPageViewInfoCollection read FPages;
    property State: TdxPageViewInfoGeneratorStateBase read FState;
    property TotalHeight: Int64 read FTotalHeight write FTotalHeight;
    property VisibleHeight: Int64 read FVisibleHeight write FVisibleHeight;
    property OnRowFinished: TdxEventHandler read FOnRowFinished write FOnRowFinished;
  end;

  { TdxPageViewInfoGeneratorRunningHeight }

  TdxPageViewInfoGeneratorRunningHeight = class(TdxPageViewInfoGeneratorBase)
  strict private
    FRunningHeight: Int64;
    function GetFirstPageAnchor: TdxRunningHeightFirstPageAnchor;
  protected
    function CreateInvisibleEmptyRowState: TdxPageViewInfoGeneratorStateBase; override;
    function CreateInvisibleRowState: TdxPageViewInfoGeneratorStateBase; override;
    function CreatePartialVisibleEmptyRowState: TdxPageViewInfoGeneratorStateBase; override;
    function CreatePartialVisibleRowState: TdxPageViewInfoGeneratorStateBase; override;
  public
    property RunningHeight: Int64 read FRunningHeight write FRunningHeight;
    property FirstPageAnchor: TdxRunningHeightFirstPageAnchor read GetFirstPageAnchor;
  end;

  { TdxPageViewInfoGeneratorFirstPageOffset }

  TdxPageViewInfoGeneratorFirstPageOffset = class(TdxPageViewInfoGeneratorBase)
  strict private
    function GetFirstPageAnchor: TdxFirstPageOffsetFirstPageAnchor;
  protected
    function CreateInvisibleEmptyRowState: TdxPageViewInfoGeneratorStateBase; override;
    function CreateInvisibleRowState: TdxPageViewInfoGeneratorStateBase; override;
    function CreatePartialVisibleEmptyRowState: TdxPageViewInfoGeneratorStateBase; override;
    function CreatePartialVisibleRowState: TdxPageViewInfoGeneratorStateBase; override;
  public
    property FirstPageAnchor: TdxFirstPageOffsetFirstPageAnchor read GetFirstPageAnchor;
  end;

  { TdxPageViewInfoGeneratorStateBase }

  TdxPageViewInfoGeneratorStateBase = class
  strict private
    FGenerator: TdxPageViewInfoGeneratorBase;
    FLayoutManager: TdxPageGeneratorLayoutManager;
    function GetRowOwner: TdxOwnedRowInfoList;
    function GetViewPortBounds: TRect;
    function GetScaleFactor: Single;
    function GetTotalHeight: Int64;
    function GetFirstPageAnchor: TdxFirstPageAnchor;
    function GetVisibleHeight: Int64;
    procedure SetTotalHeight(const Value: Int64);
    procedure SetVisibleHeight(const Value: Int64);
  protected
    function ChangeState(AStateType: TdxPageViewInfoGeneratorState; AFreePrevState: Boolean = True): TdxPageViewInfoGeneratorStateBase; virtual;
    procedure AddPageToOutput(APage: TdxPageViewInfo); virtual;
    function GetCurrentRow: TdxPageViewInfoRow; virtual; abstract;
    function GetType: TdxPageViewInfoGeneratorState; virtual; abstract;

    property RowOwner: TdxOwnedRowInfoList read GetRowOwner;
  public
    constructor Create(AGenerator: TdxPageViewInfoGeneratorBase); virtual;
    function PreProcessPage(APage: TdxPage; APageIndex: Integer): TdxProcessPageResult; virtual;
    function ProcessPage(APage: TdxPage; APageIndex: Integer): TdxStateProcessPageResult; virtual; abstract;

    property CurrentRow: TdxPageViewInfoRow read GetCurrentRow;
    property FirstPageAnchor: TdxFirstPageAnchor read GetFirstPageAnchor;
    property Generator: TdxPageViewInfoGeneratorBase read FGenerator;
    property LayoutManager: TdxPageGeneratorLayoutManager read FLayoutManager;
    property TotalHeight: Int64 read GetTotalHeight write SetTotalHeight;
    property &Type: TdxPageViewInfoGeneratorState read GetType;
    property ViewPortBounds: TRect read GetViewPortBounds;
    property VisibleHeight: Int64 read GetVisibleHeight write SetVisibleHeight;
    property ScaleFactor: Single read GetScaleFactor;
  end;

  { TdxAddPageGeneratorStateBase }

  TdxAddPageGeneratorStateBase = class(TdxPageViewInfoGeneratorStateBase)
  protected
    procedure AddPage(APage: TdxPage; AIndex: Integer); virtual;
    procedure RecalculateCurrentRowPagesLayout(APage: TdxPage); virtual; abstract;
    function CanAddPage(APage: TdxPage): Boolean; virtual;
    function GetCurrentRow: TdxPageViewInfoRow; override;
    procedure UpdateVisibleHeight(APageViewInfo: TdxPageViewInfo); virtual;
    procedure UpdateVisibleHeightCore(AVisiblePageHeight: Integer); virtual;
    procedure UpdateHeights(APage: TdxPage; APageViewInfo: TdxPageViewInfo); virtual;
    function CalculatePageBounds(APage: TdxPage): TRect; virtual;
    function CalculatePageVisibility(APage: TdxPage; APageIndex: Integer): TdxPageVisibility; virtual;
    procedure FinishCurrentRow; virtual;
    procedure AlignPagesVertically; virtual;
  end;

  { TdxStateInvisibleRowBase }

  TdxStateInvisibleRowBase = class(TdxAddPageGeneratorStateBase)
  strict private
    FCurrentRow: TdxPageViewInfoRow;
  protected
    function CalculatePageVisibility(APage: TdxPage; APageIndex: Integer): TdxPageVisibility; override;
    function GetCurrentRow: TdxPageViewInfoRow; override;
    procedure AddPageToOutput(APage: TdxPageViewInfo); override;
    procedure UpdateVisibleHeight(APageViewInfo: TdxPageViewInfo); override;
    procedure CopyPagesTo(ANewState: TdxPageViewInfoGeneratorStateBase);
    procedure HideStateCurrentRow(ANewState: TdxPageViewInfoGeneratorStateBase); virtual;
    procedure FinishCurrentRow; override;
    procedure RecalculateCurrentRowPagesLayout(APage: TdxPage); override;
    function IsPageFullyVisible(APage: TdxPage; APageIndex: Integer): Boolean; virtual; abstract;
    function IsPageFullyInvisible(APage: TdxPage; APageIndex: Integer): Boolean; virtual; abstract;
  public
    constructor Create(AGenerator: TdxPageViewInfoGeneratorBase); override;
  end;

  { TdxStateInvisibleEmptyRow }

  TdxStateInvisibleEmptyRow = class(TdxStateInvisibleRowBase)
  protected
    function CalculatePageVisibility(APage: TdxPage; APageIndex: Integer): TdxPageVisibility; override;
    function GetType: TdxPageViewInfoGeneratorState; override;
  public
    function ProcessPage(APage: TdxPage; APageIndex: Integer): TdxStateProcessPageResult; override;
  end;

  { TdxStateInvisibleRow }

  TdxStateInvisibleRow = class(TdxStateInvisibleRowBase)
  protected
    function GetType: TdxPageViewInfoGeneratorState; override;
    procedure RollbackStateToTheStartOfRow; virtual; abstract;
    function SwitchToPartialVisibleRowState(APage: TdxPage; APageIndex: Integer): TdxStateProcessPageResult; virtual;
  public
    function ProcessPage(APage: TdxPage; APageIndex: Integer): TdxStateProcessPageResult; override;
  end;

  { TdxStateInvisibleEmptyRowRunningHeight }

  TdxStateInvisibleEmptyRowRunningHeight = class(TdxStateInvisibleEmptyRow)
  strict private
    function GetGenerator: TdxPageViewInfoGeneratorRunningHeight;
  protected
    procedure UpdateHeights(APage: TdxPage; APageViewInfo: TdxPageViewInfo); override;
    function IsPageFullyVisible(APage: TdxPage; APageIndex: Integer): Boolean; override;
    function IsPageFullyInvisible(APage: TdxPage; APageIndex: Integer): Boolean; override;
  public
    property Generator: TdxPageViewInfoGeneratorRunningHeight read GetGenerator;
  end;

  { TdxStateInvisibleRowRunningHeight }

  TdxStateInvisibleRowRunningHeight = class(TdxStateInvisibleRow)
  strict private
    function GetGenerator: TdxPageViewInfoGeneratorRunningHeight;
  protected
    procedure UpdateHeights(APage: TdxPage; APageViewInfo: TdxPageViewInfo); override;
    function IsPageFullyVisible(APage: TdxPage; APageIndex: Integer): Boolean; override;
    function IsPageFullyInvisible(APage: TdxPage; APageIndex: Integer): Boolean; override;
    procedure RollbackStateToTheStartOfRow; override;
  public
    property Generator: TdxPageViewInfoGeneratorRunningHeight read GetGenerator;
  end;

  { TdxStateInvisibleEmptyRowFirstPageOffset }

  TdxStateInvisibleEmptyRowFirstPageOffset = class(TdxStateInvisibleEmptyRow)
  strict private
    function GetGenerator: TdxPageViewInfoGeneratorFirstPageOffset;
  protected
    function IsPageFullyVisible(APage: TdxPage; APageIndex: Integer): Boolean; override;
    function IsPageFullyInvisible(APage: TdxPage; APageIndex: Integer): Boolean; override;
  public
    property Generator: TdxPageViewInfoGeneratorFirstPageOffset read GetGenerator;
  end;

  { TdxStateInvisibleRowFirstPageOffset }

  TdxStateInvisibleRowFirstPageOffset = class(TdxStateInvisibleRow)
  strict private
    function GetGenerator: TdxPageViewInfoGeneratorFirstPageOffset;
  protected
    function IsPageFullyVisible(APage: TdxPage; APageIndex: Integer): Boolean; override;
    function IsPageFullyInvisible(APage: TdxPage; APageIndex: Integer): Boolean; override;
    procedure RollbackStateToTheStartOfRow; override;
    function SwitchToPartialVisibleRowState(APage: TdxPage; APageIndex: Integer): TdxStateProcessPageResult; override;
    procedure HideStateCurrentRow(ANewState: TdxPageViewInfoGeneratorStateBase); override;
  public
    property Generator: TdxPageViewInfoGeneratorFirstPageOffset read GetGenerator;
  end;

  { TdxStatePartialVisibleEmptyRow }

  TdxStatePartialVisibleEmptyRow = class(TdxAddPageGeneratorStateBase)
  protected
    function CalculateVerticalRowOffset: Integer; virtual; abstract;
    function GetType: TdxPageViewInfoGeneratorState; override;
    procedure RecalculateCurrentRowPagesLayout(APage: TdxPage); override;
    procedure UpdateHeights(APage: TdxPage; APageViewInfo: TdxPageViewInfo); override;
  public
    function ProcessPage(APage: TdxPage; APageIndex: Integer): TdxStateProcessPageResult; override;
  end;

  { TdxStatePartialVisibleRow }

  TdxStatePartialVisibleRow = class(TdxAddPageGeneratorStateBase)
  protected
    function GetType: TdxPageViewInfoGeneratorState; override;
    procedure UpdateHeights(APage: TdxPage; APageViewInfo: TdxPageViewInfo); override;
    procedure UpdateVisibleHeight(APageViewInfo: TdxPageViewInfo); override;
  public
    function PreProcessPage(APage: TdxPage; APageIndex: Integer): TdxProcessPageResult; override;
    function ProcessPage(APage: TdxPage; APageIndex: Integer): TdxStateProcessPageResult; override;
  end;

  { TdxStatePartialVisibleEmptyRowRunningHeight }

  TdxStatePartialVisibleEmptyRowRunningHeight = class(TdxStatePartialVisibleEmptyRow)
  strict private
    function GetGenerator: TdxPageViewInfoGeneratorRunningHeight;
  protected
    function CalculateVerticalRowOffset: Integer; override;
  public
    property Generator: TdxPageViewInfoGeneratorRunningHeight read GetGenerator;
  end;

  { TdxStatePartialVisibleRowRunningHeight }

  TdxStatePartialVisibleRowRunningHeight = class(TdxStatePartialVisibleRow)
  strict private
    function GetGenerator: TdxPageViewInfoGeneratorRunningHeight;
  protected
    procedure RecalculateCurrentRowPagesLayout(APage: TdxPage); override;
    function CalculatePartialRowOffset: Integer; virtual;
    function CheckRowOffset(AOffset: Integer; ATotalInvisibleRowHeight: Int64): Integer; virtual;
  public
    property Generator: TdxPageViewInfoGeneratorRunningHeight read GetGenerator;
  end;

  { TdxStatePartialVisibleEmptyRowFirstPageOffset }

  TdxStatePartialVisibleEmptyRowFirstPageOffset = class(TdxStatePartialVisibleEmptyRow)
  strict private
    function GetGenerator: TdxPageViewInfoGeneratorFirstPageOffset;
  protected
    function CalculateVerticalRowOffset: Integer; override;
  public
    property Generator: TdxPageViewInfoGeneratorFirstPageOffset read GetGenerator;
  end;

  { TdxStatePartialVisibleRowFirstPageOffset }

  TdxStatePartialVisibleRowFirstPageOffset = class(TdxStatePartialVisibleRow)
  strict private
    function GetGenerator: TdxPageViewInfoGeneratorFirstPageOffset;
  protected
    procedure RecalculateCurrentRowPagesLayout(APage: TdxPage); override;
    procedure UpdateHeights(APage: TdxPage; APageViewInfo: TdxPageViewInfo); override;
  public
    property Generator: TdxPageViewInfoGeneratorFirstPageOffset read GetGenerator;
  end;

  { TdxStateVisibleEmptyRow }

  TdxStateVisibleEmptyRow = class(TdxAddPageGeneratorStateBase)
  protected
    function GetType: TdxPageViewInfoGeneratorState; override;
    procedure RemoveLastRow;
    procedure RecalculateCurrentRowPagesLayout(APage: TdxPage); override;
    procedure UpdateHeights(APage: TdxPage; APageViewInfo: TdxPageViewInfo); override;
  public
    function ProcessPage(APage: TdxPage; APageIndex: Integer): TdxStateProcessPageResult; override;
  end;

  { TdxStateVisibleRow }

  TdxStateVisibleRow = class(TdxAddPageGeneratorStateBase)
  strict private
    FInitialVisibleHeight: Int64;
  protected
    function GetType: TdxPageViewInfoGeneratorState; override;
    procedure UpdateVisibleHeight(APageViewInfo: TdxPageViewInfo); override;
    procedure RecalculateCurrentRowPagesLayout(APage: TdxPage); override;
  public
    function PreProcessPage(APage: TdxPage; APageIndex: Integer): TdxProcessPageResult; override;
    function ProcessPage(APage: TdxPage; APageIndex: Integer): TdxStateProcessPageResult; override;

    property InitialVisibleHeight: Int64 read FInitialVisibleHeight write FInitialVisibleHeight;
  end;

  { TdxStateVisiblePagesGenerationComplete }

  TdxStateVisiblePagesGenerationComplete = class(TdxPageViewInfoGeneratorStateBase)
  protected
    function GetCurrentRow: TdxPageViewInfoRow; override;
    function GetType: TdxPageViewInfoGeneratorState; override;
  public
    function ProcessPage(APage: TdxPage; APageIndex: Integer): TdxStateProcessPageResult; override;
  end;

  { TdxInvisiblePageRowsGenerator }

  TdxInvisiblePageRowsGenerator = class
  strict private
    FPages: TdxPageCollection;
    FGenerator: TdxPageViewInfoGeneratorBase;
    FFirstPageIndex: Integer;
    FFirstInvalidPageIndex: Integer;
    FStep: Integer;
    FCurrentPageIndex: Integer;
    FResult: TdxPageViewInfoRow;
    FLayoutManager: TdxPageGeneratorLayoutManager;
    FIsFinished: Boolean;
    FPagesProcessed: Integer;
    FRunningHeightFirstPageAnchor: TdxRunningHeightFirstPageAnchor;
    FPageViewInfoCollection: TdxPageViewInfoCollection;
  private
    procedure SetFirstInvalidPageIndex(const Value: Integer);
    procedure SetFirstPageIndex(const Value: Integer);
  protected
    procedure Reset; virtual;
    procedure OnRowFinished(Sender: TObject; E: TdxEventArgs); virtual;

    property Pages: TdxPageCollection read FPages;
    property Step: Integer read FStep;
    property CurrentPageIndex: Integer read FCurrentPageIndex;
    property &Result: TdxPageViewInfoRow read FResult;
    property IsFinished: Boolean read FIsFinished;
    property PagesProcessed: Integer read FPagesProcessed;
  public
    constructor Create(APages: TdxPageCollection; ASourceLayoutManager: TdxPageGeneratorLayoutManager);
    destructor Destroy; override;
    function GenerateNextRow: TdxPageViewInfoRow; virtual;

    property FirstPageIndex: Integer read FFirstPageIndex write SetFirstPageIndex;
    property FirstInvalidPageIndex: Integer read FFirstInvalidPageIndex write SetFirstInvalidPageIndex;
    property Generator: TdxPageViewInfoGeneratorBase read FGenerator;
    property LayoutManager: TdxPageGeneratorLayoutManager read FLayoutManager;
  end;

implementation

uses
  Contnrs, Math, dxCore,

  dxRichEdit.Utils.Exceptions,
  dxRichEdit.View.Core,
  dxRichEdit.DocumentModel.Section;

type
  TdxPageGeneratorLayoutManagerHelper = class helper for TdxPageGeneratorLayoutManager
  strict private
    function GetView: TdxRichEditView;
    function GetViewInfo: TdxRichEditViewInfo;
  public
    property View: TdxRichEditView read GetView;
    property ViewInfo: TdxRichEditViewInfo read GetViewInfo;
  end;

function TdxPageGeneratorLayoutManagerHelper.GetView: TdxRichEditView;
begin
  Result := TdxRichEditView(FView);
end;

function TdxPageGeneratorLayoutManagerHelper.GetViewInfo: TdxRichEditViewInfo;
begin
  Result := TdxRichEditViewInfo(FViewInfo);
end;

{ TdxPageViewInfoRow }

function TdxPageViewInfoRow.IntersectsWithHorizontalLine(ALineY: Integer): Boolean;
begin
  Result := (FBounds.Top < ALineY) and (FBounds.Bottom >= ALineY);
end;

function TdxPageViewInfoRow.GetPageAtPoint(const APoint: TPoint; AStrictSearch: Boolean): TdxPageViewInfo;
var
  AIndex: Integer;
begin
  if Count = 0 then
    Exit(nil);

  if FindByBound(APoint.X, AIndex) then
    Exit(Items[AIndex]);

  if AStrictSearch then
    Exit(nil);

  if AIndex = Count then
    if APoint.X < First.Bounds.Left then
      Exit(First)
    else
      Exit(Last)
  else
    Result := Items[AIndex];
end;


constructor TdxPageViewInfoRow.Create(AOwner: TdxOwnedRowInfoList);
begin
  inherited Create;
  AOwner.Add(Self);
end;

destructor TdxPageViewInfoRow.Destroy;
begin
  inherited Destroy;
end;

function TdxPageViewInfoRow.FindByBound(X: Integer; out AFoundIndex: Integer): Boolean;
var
  L, H, AMedian: Integer;
  ABounds: TRect;
begin
  if Count = 0 then
  begin
    AFoundIndex := 0;
    Exit(False);
  end;
  Result := False;
  L := 0;
  H := Count - 1;
  while L <= H do
  begin
    AMedian := L + (H - L) shr 1;
    ABounds := Items[AMedian].Bounds;
    if X >= ABounds.Right then
      L := AMedian + 1
    else
    begin
      H := AMedian - 1;
      if X >= ABounds.Left then
        Result := True;
    end;
  end;
  AFoundIndex := L;
end;

{ TdxPageViewInfoRowCollection }

function TdxPageViewInfoRowCollection.GetRowAtPoint(const APoint: TPoint; AStrictSearch: Boolean): TdxPageViewInfoRow;
var
  AIndex: Integer;
begin
  if Count = 0 then
    Exit(nil);

  if FindByBound(APoint.Y, AIndex) then
    Exit(Items[AIndex]);

  if AStrictSearch then
    Exit(nil);

  if AIndex = Count then
    if APoint.Y < First.Bounds.Top then
      Exit(First)
    else
      Exit(Last)
  else
    Result := Items[AIndex];
end;

function TdxPageViewInfoRowCollection.First: TdxPageViewInfoRow;
begin
  if Count > 0  then
    Result := inherited First
  else
    Result := nil;
end;

function TdxPageViewInfoRowCollection.Last: TdxPageViewInfoRow;
begin
  if Count > 0  then
    Result := inherited Last
  else
    Result := nil;
end;

function TdxPageViewInfoRowCollection.FindByBound(Y: Integer; out AFoundIndex: Integer): Boolean;
var
  L, H, AMedian: Integer;
  ABounds: TRect;
begin
  if Count = 0 then
  begin
    AFoundIndex := 0;
    Exit(False);
  end;
  Result := False;
  L := 0;
  H := Count - 1;
  while L <= H do
  begin
    AMedian := L + (H - L) shr 1;
    ABounds := Items[AMedian].Bounds;
    if Y >= ABounds.Bottom then
      L := AMedian + 1
    else
    begin
      H := AMedian - 1;
      if Y >= ABounds.Top then
        Result := True;
    end;
  end;
  AFoundIndex := L;
end;

{ TdxPageGeneratorLayoutManager }


constructor TdxPageGeneratorLayoutManager.Create(AView: TObject; AViewInfo: TObject);
var
  AUnitConverter: TdxDocumentLayoutUnitConverter;
  APageGap: Integer;
begin
  inherited Create;
  FView := AView;
  FViewInfo := AViewInfo;
  AUnitConverter := View.DocumentModel.LayoutUnitConverter;
  APageGap := AUnitConverter.DocumentsToLayoutUnits(60);
  HorizontalPageGap := APageGap;
  VerticalPageGap := APageGap;
end;

function TdxPageGeneratorLayoutManager.Clone: TdxPageGeneratorLayoutManager;
begin
  if Self = nil then
    Exit(nil);
  Result := CreateEmptyClone;
  Result.CopyFrom(Self);
end;

procedure TdxPageGeneratorLayoutManager.CopyFrom(AManager: TdxPageGeneratorLayoutManager);
begin
  FScaleFactor := AManager.ScaleFactor;
  FViewPortBounds := AManager.ViewPortBounds;
  FHorizontalPageGap := AManager.HorizontalPageGap;
  FVerticalPageGap := AManager.VerticalPageGap;
end;

function TdxPageGeneratorLayoutManager.CalculatePageLogicalWidth(APage: TdxPage): Integer;
begin
  Result := APage.Bounds.Width + HorizontalPageGap;
end;

function TdxPageGeneratorLayoutManager.CalculatePageLogicalTotalWidth(APage: TdxPage): Integer;
begin
  Result := CalculatePageLogicalWidth(APage);
end;

function TdxPageGeneratorLayoutManager.CalculatePagePhysicalWidth(APage: TdxPage): Integer;
begin
  Result := Ceil(CalculatePageLogicalWidth(APage) * ScaleFactor);
end;

function TdxPageGeneratorLayoutManager.CalculatePageLogicalHeight(APage: TdxPage): Integer;
begin
  Result := APage.Bounds.Height + VerticalPageGap;
end;

function TdxPageGeneratorLayoutManager.CalculatePagePhysicalHeight(APage: TdxPage): Integer;
begin
  Result := Ceil(CalculatePageLogicalHeight(APage) * ScaleFactor);
end;

function TdxPageGeneratorLayoutManager.CalculatePageLogicalSize(APage: TdxPage): TSize;
var
  APageBounds: TRect;
begin
  APageBounds := APage.Bounds;
  Result.Init(APageBounds.Width + HorizontalPageGap, APageBounds.Height + VerticalPageGap);
end;

function TdxPageGeneratorLayoutManager.CalculatePagePhysicalSize(APage: TdxPage): TSize;
var
  APageBounds: TRect;
begin
  APageBounds := APage.Bounds;
  Result.Init(Ceil((APageBounds.Width + HorizontalPageGap) * ScaleFactor),
    Ceil((APageBounds.Height + VerticalPageGap) * ScaleFactor));
end;

function TdxPageGeneratorLayoutManager.CanFitPageToPageRow(APage: TdxPage; ARow: TdxPageViewInfoRow): Boolean;
var
  APageSize: TSize;
begin
  APageSize := CalculatePagePhysicalSize(APage);
  Result := (ARow.Bounds.Width + APageSize.Width) <= ViewPortBounds.Width;
end;

function TdxPageGeneratorLayoutManager.CalculateVisibleHeightFactor(APageViewInfo: TdxPageViewInfo; const AViewPort: TRect): Single;
var
  APageViewInfoTotalHeight: Integer;
  APageViewInfoVisibleBounds: TRect;
begin
  APageViewInfoVisibleBounds.InitSize(0, AViewPort.Top, MaxInt, AViewPort.Height);
  APageViewInfoVisibleBounds.Intersect(APageViewInfo.Bounds);
  APageViewInfoTotalHeight := APageViewInfo.Bounds.Height;
  if APageViewInfoTotalHeight <= 0 then
    Exit(0);
  Result := APageViewInfoVisibleBounds.Height / APageViewInfoTotalHeight;
end;

function TdxPageGeneratorLayoutManager.CalculateVisibleWidthFactor(APageViewInfo: TdxPageViewInfo; const AViewPort: TRect): Single;
var
  APageViewInfoTotalWidth: Integer;
  APageViewInfoVisibleBounds: TRect;
begin
  APageViewInfoVisibleBounds.InitSize(0, AViewPort.Top, AViewPort.Width, MaxInt);
  APageViewInfoVisibleBounds.Intersect(APageViewInfo.Bounds);
  APageViewInfoTotalWidth := APageViewInfo.Bounds.Width;
  if APageViewInfoTotalWidth <= 0 then
    Exit(0);
  Result := APageViewInfoVisibleBounds.Width / APageViewInfoTotalWidth;
end;

function TdxPageGeneratorLayoutManager.CalculatePageVisibleLogicalWidth(APageViewInfo: TdxPageViewInfo): Integer;
begin
  Result := CalculatePageVisibleLogicalWidth(APageViewInfo, ViewPortBounds);
end;

function TdxPageGeneratorLayoutManager.CalculatePageVisibleLogicalWidth(APageViewInfo: TdxPageViewInfo; const AViewPort: TRect): Integer;
var
  AVisibleWidthFactor, AResultF: Single;
begin
  AVisibleWidthFactor := CalculateVisibleWidthFactor(APageViewInfo, AViewPort);
  if AVisibleWidthFactor <= 0 then
    Exit(0);
  AResultF := AVisibleWidthFactor * CalculatePageLogicalWidth(APageViewInfo.Page);
  Result := Round(AResultF);
end;

function TdxPageGeneratorLayoutManager.CalculatePageVisibleLogicalHeight(APageViewInfo: TdxPageViewInfo): Integer;
begin
  Result := CalculatePageVisibleLogicalHeight(APageViewInfo, ViewPortBounds);
end;

function TdxPageGeneratorLayoutManager.CalculatePageVisibleLogicalHeight(APageViewInfo: TdxPageViewInfo; const AViewPort: TRect): Integer;
var
  AVisibleHeightFactor, AResultF: Single;
begin
  AVisibleHeightFactor := CalculateVisibleHeightFactor(APageViewInfo, AViewPort);
  if AVisibleHeightFactor <= 0 then
    Exit(0);
  AResultF := AVisibleHeightFactor * CalculatePageLogicalHeight(APageViewInfo.Page);
  Result := Round(AResultF);
end;

function TdxPageGeneratorLayoutManager.CalculatePagesTotalLogicalHeight(ARow: TdxPageViewInfoRow): Int64;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to ARow.Count - 1 do
    Inc(Result, CalculatePageLogicalHeight(ARow[I].Page));
end;

function TdxPageGeneratorLayoutManager.CalculatePagesTotalVisibleLogicalHeight(ARow: TdxPageViewInfoRow): Int64;
begin
  Result := CalculatePagesTotalVisibleLogicalHeight(ARow, ViewPortBounds);
end;

function TdxPageGeneratorLayoutManager.CalculatePagesTotalVisibleLogicalHeight(ARow: TdxPageViewInfoRow; const AViewPort: TRect): Int64;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to ARow.Count - 1 do
    Inc(Result, CalculatePageVisibleLogicalHeight(ARow[I], AViewPort));
end;

function TdxPageGeneratorLayoutManager.CalculatePagesTotalLogicalHeightAbove(ARow: TdxPageViewInfoRow; ALogicalY: Integer): Int64;
var
  AViewPort: TRect;
begin
  AViewPort.InitSize(0, MinInt div 2, 0, ALogicalY - (MinInt div 2));
  Result := CalculatePagesTotalVisibleLogicalHeight(ARow, AViewPort);
end;

function TdxPageGeneratorLayoutManager.CalculatePagesTotalLogicalHeightBelow(ARow: TdxPageViewInfoRow; ALogicalY: Integer): Int64;
var
  AViewPort: TRect;
begin
  AViewPort.InitSize(0, ALogicalY, 0, MaxInt div 2);
  Result := CalculatePagesTotalVisibleLogicalHeight(ARow, AViewPort);
end;

procedure TdxPageGeneratorLayoutManager.UpdatePageClientBounds(APageViewInfo: TdxPageViewInfo);
var
  AVerticalGapPhysicalSize: Integer;
  AHorizontalGapPhysicalSize: Integer;
  AClientBounds: TRect;
begin
  AClientBounds := APageViewInfo.Bounds;
  AHorizontalGapPhysicalSize := Floor(HorizontalPageGap * ScaleFactor);
  AVerticalGapPhysicalSize := Floor(VerticalPageGap * ScaleFactor);
  AClientBounds.Width := AClientBounds.Width - AHorizontalGapPhysicalSize;
  AClientBounds.Height := AClientBounds.Height - AVerticalGapPhysicalSize;
  AClientBounds.Offset(AHorizontalGapPhysicalSize div 2, AVerticalGapPhysicalSize div 2);
  APageViewInfo.ClientBounds := AClientBounds;
end;

function TdxPageGeneratorLayoutManager.OffsetRectangle(const ARect: TRect; ADeltaX, ADeltaY: Integer): TRect;
begin
  Result := ARect;
  Result.Offset(ADeltaX, ADeltaY);
end;

procedure TdxPageGeneratorLayoutManager.OffsetRowVertically(ARow: TdxPageViewInfoRow; AOffset: Integer);
var
  APageViewInfo: TdxPageViewInfo;
  I: Integer;
begin
  for I := 0 to ARow.Count - 1 do
  begin
    APageViewInfo := ARow[I];
    APageViewInfo.Bounds := OffsetRectangle(APageViewInfo.Bounds, 0, AOffset);
    UpdatePageClientBounds(APageViewInfo);
  end;
  ARow.Bounds := OffsetRectangle(ARow.Bounds, 0, AOffset);
end;

function TdxPageGeneratorLayoutManager.CreateInitialPageViewInfoRowBounds(Y: Integer): TRect;
begin
  Result.InitSize(ViewPortBounds.Width div 2, Y, 0, 0);
end;

function TdxPageGeneratorLayoutManager.GetHorizontalPageGap: Integer;
begin
  Result := FHorizontalPageGap;
end;

function TdxPageGeneratorLayoutManager.GetVerticalPageGap: Integer;
begin
  Result := FVerticalPageGap;
end;

function TdxPageGeneratorLayoutManager.CalculateFirstPageLeftOffset(ATotalPagesWidth: Integer): Integer;
begin
  Result := (ViewPortBounds.Width - ATotalPagesWidth) div 2;
end;

procedure TdxPageGeneratorLayoutManager.AlignPages(ARow: TdxPageViewInfoRow);
var
  ATotalPagesWidth, AOffset: Integer;
begin
  ATotalPagesWidth := ARow.Last.Bounds.Right - ARow.First.Bounds.Left;
  AOffset := Max(0, CalculateFirstPageLeftOffset(ATotalPagesWidth));
  AlignPagesHorizontally(ARow, AOffset);
  AlignPagesVertically(ARow);
  UpdatePagesClientBounds(ARow);
  ARow.Bounds := TRect.CreateSize(AOffset, ARow.Bounds.Top, ATotalPagesWidth, ARow.Bounds.Height);
end;

procedure TdxPageGeneratorLayoutManager.AlignPagesVertically(ARow: TdxPageViewInfoRow);
var
  R: TRect;
  APageViewInfo: TdxPageViewInfo;
  I, ARowHeight: Integer;
begin
  ARowHeight := ARow.Bounds.Height;
  for I := 0 to ARow.Count - 1 do
  begin
    APageViewInfo := ARow[I];
    R := APageViewInfo.Bounds;
    APageViewInfo.Bounds := TRect.CreateSize(R.Left, ARow.Bounds.Top + (ARowHeight - R.Height) div 2, R.Width, R.Height);
  end;
end;

procedure TdxPageGeneratorLayoutManager.AlignPagesHorizontally(ARow: TdxPageViewInfoRow; ARowLeftOffset: Integer);
var
  R: TRect;
  APageViewInfo: TdxPageViewInfo;
  I, X: Integer;
begin
  X := ARowLeftOffset;
  for I := 0 to ARow.Count - 1 do
  begin
    APageViewInfo := ARow[I];
    R := APageViewInfo.Bounds;
    APageViewInfo.Bounds := TRect.CreateSize(X, R.Top, R.Width, R.Height);
    Inc(X, R.Width);
  end;
end;

procedure TdxPageGeneratorLayoutManager.UpdatePagesClientBounds(ARow: TdxPageViewInfoRow);
var
  I: Integer;
begin
  for I := 0 to ARow.Count - 1 do
    UpdatePageClientBounds(ARow[I]);
end;

procedure TdxPageGeneratorLayoutManager.SetHorizontalPageGap(Value: Integer);
begin
  FHorizontalPageGap := Value;
end;

procedure TdxPageGeneratorLayoutManager.SetVerticalPageGap(Value: Integer);
begin
  FVerticalPageGap := Value;
end;

procedure TdxPageGeneratorLayoutManager.OnLayoutUnitChanging(AUnitConverter: TdxDocumentLayoutUnitConverter);
begin
end;

procedure TdxPageGeneratorLayoutManager.OnLayoutUnitChanged(AUnitConverter: TdxDocumentLayoutUnitConverter);
begin
end;

{ TdxPageViewInfoGenerator }

constructor TdxPageViewInfoGenerator.Create(AView: TObject; AViewInfo: TObject);
begin
  inherited Create(AView, AViewInfo);
  FRunningHeightAnchor := TdxRunningHeightFirstPageAnchor.Create;
  FFirstPageOffsetAnchor := TdxFirstPageOffsetFirstPageAnchor.Create;
  Reset(TdxPageGenerationStrategyType.RunningHeight);
end;

destructor TdxPageViewInfoGenerator.Destroy;
begin
  FreeAndNil(FFirstPageOffsetGenerator);
  FreeAndNil(FRunningHeightGenerator);
  FreeAndNil(FFirstPageOffsetAnchor);
  FreeAndNil(FRunningHeightAnchor);
  inherited Destroy;
end;

function TdxPageViewInfoGenerator.PreProcessPage(APage: TdxPage; APageIndex: Integer): TdxProcessPageResult;
begin
  Result := FActiveGenerator.PreProcessPage(APage, APageIndex);
end;

function TdxPageViewInfoGenerator.ProcessPage(APage: TdxPage; APageIndex: Integer): TdxProcessPageResult;
begin
  Result := FActiveGenerator.ProcessPage(APage, APageIndex);
  if Result = TdxProcessPageResult.VisiblePagesGenerationIncomplete then
  begin
    if FActiveGenerator = FFirstPageOffsetGenerator then
      UpdateRunningHeightAnchor();
  end;
end;

procedure TdxPageViewInfoGenerator.ResetAnchors;
begin
  FRunningHeightAnchor.TopInvisibleHeight := 0;
  FFirstPageOffsetAnchor.PageIndex := 0;
  FFirstPageOffsetAnchor.VerticalOffset := 0;
end;

procedure TdxPageViewInfoGenerator.Reset(AStrategy: TdxPageGenerationStrategyType);
var
  ANewRunningHeightGenerator: TdxPageViewInfoGeneratorRunningHeight;
  ANewFirstPageOffsetGenerator: TdxPageViewInfoGeneratorFirstPageOffset;
begin
  ViewPortBounds := View.Bounds;
  ScaleFactor := View.ScaleFactor;
  if AStrategy = TdxPageGenerationStrategyType.RunningHeight then
  begin
    ANewRunningHeightGenerator := TdxPageViewInfoGeneratorRunningHeight.Create(Self, FRunningHeightAnchor, View.PageViewInfos);
    if FActiveGenerator <> FRunningHeightGenerator then
      UpdateRunningHeightAnchor;
    FreeAndNil(FRunningHeightGenerator);
    FRunningHeightGenerator := ANewRunningHeightGenerator;
    FActiveGenerator := FRunningHeightGenerator;
  end
  else
  begin
    ANewFirstPageOffsetGenerator := TdxPageViewInfoGeneratorFirstPageOffset.Create(Self, FFirstPageOffsetAnchor, View.PageViewInfos);
    if FActiveGenerator <> FFirstPageOffsetGenerator then
      UpdateFirstPageOffsetAnchor;
    FreeAndNil(FFirstPageOffsetGenerator);
    FFirstPageOffsetGenerator := ANewFirstPageOffsetGenerator;
    FActiveGenerator := FFirstPageOffsetGenerator;
  end;
end;

function TdxPageViewInfoGenerator.GetPageRowAtPoint(const APoint: TPoint): TdxPageViewInfoRow;
begin
  Result := GetPageRowAtPoint(APoint, True);
end;

function TdxPageViewInfoGenerator.GetPageRowAtPoint(const APoint: TPoint; AStrictSearch: Boolean): TdxPageViewInfoRow;
begin
  Result := ActiveGenerator.GetPageRowAtPoint(APoint, AStrictSearch);
end;

procedure TdxPageViewInfoGenerator.OnLayoutUnitChanging(AUnitConverter: TdxDocumentLayoutUnitConverter);
begin
  inherited OnLayoutUnitChanging(AUnitConverter);
  HorizontalPageGap := AUnitConverter.LayoutUnitsToTwips(HorizontalPageGap);
  VerticalPageGap := AUnitConverter.LayoutUnitsToTwips(VerticalPageGap);
  LeftInvisibleWidth := AUnitConverter.LayoutUnitsToTwips(LeftInvisibleWidth);
  FRunningHeightAnchor.TopInvisibleHeight := AUnitConverter.LayoutUnitsToTwips(FRunningHeightAnchor.TopInvisibleHeight);
  FFirstPageOffsetAnchor.VerticalOffset := AUnitConverter.LayoutUnitsToTwips(FFirstPageOffsetAnchor.VerticalOffset);
end;

procedure TdxPageViewInfoGenerator.OnLayoutUnitChanged(AUnitConverter: TdxDocumentLayoutUnitConverter);
begin
  inherited OnLayoutUnitChanged(AUnitConverter);
  HorizontalPageGap := AUnitConverter.TwipsToLayoutUnits(HorizontalPageGap);
  VerticalPageGap := AUnitConverter.TwipsToLayoutUnits(VerticalPageGap);
  LeftInvisibleWidth := AUnitConverter.TwipsToLayoutUnits(LeftInvisibleWidth);
  FRunningHeightAnchor.TopInvisibleHeight := AUnitConverter.TwipsToLayoutUnits(FRunningHeightAnchor.TopInvisibleHeight);
  FFirstPageOffsetAnchor.VerticalOffset := AUnitConverter.TwipsToLayoutUnits(FFirstPageOffsetAnchor.VerticalOffset);
end;

procedure TdxPageViewInfoGenerator.CalculateWidthParameters;
var
  ATotalWidthValue, AVisibleWidthValue: Int64;
  I, ACount: Integer;
  ARow: TdxPageViewInfoRow;
  APage: TdxPageViewInfo;
begin
  ATotalWidthValue := MinInt64;
  AVisibleWidthValue := MinInt64;

  ACount := ActiveGenerator.PageRows.Count;
  for I := 0 to ACount - 1 do
  begin
    ARow := ActiveGenerator.PageRows[I];
    if ARow.Count = 1 then
    begin
      APage := ARow[0];
      ATotalWidthValue := Max(ATotalWidthValue, CalculatePageLogicalWidth(APage.Page));
      AVisibleWidthValue := Max(AVisibleWidthValue, CalculatePageVisibleLogicalWidth(APage));
    end;
  end;

  if (ATotalWidthValue <> MinInt64) and (AVisibleWidthValue <> MinInt64) then
  begin
    TotalWidth := ATotalWidthValue;
    VisibleWidth := AVisibleWidthValue;
  end
  else
  begin
    TotalWidth := 100;
    VisibleWidth := 100;
  end;
end;

function TdxPageViewInfoGenerator.GetVerticalScrollBarLargeChange: Integer;
begin
  Result := VisibleHeight;
end;

procedure TdxPageViewInfoGenerator.CalculateMaxPageWidth;
var
  ASections: TdxSectionCollection;
  I, ACount: TdxSectionIndex;
  APage: TdxPage;
begin
  ASections := View.DocumentModel.Sections;
  ACount := ASections.Count;
  FMaxPageWidth := MinInt64;
  APage := TdxPage.Create;
  try
    for I := 0 to ACount - 1 do
    begin
      APage.Bounds := View.FormattingController.PageController.CalculatePageBounds(ASections[I]);
      FMaxPageWidth := Max(FMaxPageWidth, CalculatePageLogicalWidth(APage));
    end;
  finally
    APage.Free;
  end;
end;

procedure TdxPageViewInfoGenerator.UpdateRunningHeightAnchor;
var
  AGenerator: TdxPageViewInfoGeneratorBase;
  AFirstRow: TdxPageViewInfoRow;
  AFirstPage: TdxPageViewInfo;
  AResult: Int64;
  APages: TdxPageCollection;
  I, ALastIndex: Integer;
begin
  AGenerator := FFirstPageOffsetGenerator;
  AFirstRow := AGenerator.PageRows.First;
  if AFirstRow = nil then
    Exit;
  AFirstPage := AFirstRow.First;
  if AFirstPage = nil then
    Exit;

  AResult := CalculatePagesTotalLogicalHeightAbove(AFirstRow, 0);
  APages := View.FormattingController.PageController.Pages;
  ALastIndex := APages.IndexOf(AFirstPage.Page);
  for I := 0 to ALastIndex - 1 do
    Inc(AResult, CalculatePageLogicalHeight(APages[I]));

  FRunningHeightAnchor.TopInvisibleHeight := AResult;
end;

procedure TdxPageViewInfoGenerator.UpdateFirstPageOffsetAnchor;
var
  AGenerator: TdxPageViewInfoGeneratorBase;
  AFirstRow: TdxPageViewInfoRow;
  APageViewInfo: TdxPageViewInfo;
  AViewPort: TRect;
begin
  AGenerator := FRunningHeightGenerator;
  AFirstRow := AGenerator.PageRows.First;
  if AFirstRow = nil then
    Exit;
  APageViewInfo := LookupFirstVisiblePage(AFirstRow);
  if APageViewInfo = nil then
    Exit;

  AViewPort := ViewPortBounds;
  AViewPort.Height := MaxInt div 2;
  FFirstPageOffsetAnchor.PageIndex := View.FormattingController.PageController.Pages.IndexOf(APageViewInfo.Page);
  FFirstPageOffsetAnchor.VerticalOffset := CalculatePageLogicalHeight(APageViewInfo.Page) - CalculatePageVisibleLogicalHeight(APageViewInfo, AViewPort);
end;

function TdxPageViewInfoGenerator.LookupFirstVisiblePage(ARow: TdxPageViewInfoRow): TdxPageViewInfo;
var
  I: Integer;
begin
  for I := 0 to ARow.Count - 1 do
    if CalculatePageVisibleLogicalHeight(ARow[I]) > 0 then
      Exit(ARow[I]);
  Result := nil;
end;

function TdxPageViewInfoGenerator.GetVisibleHeight: Int64;
begin
  Result := FActiveGenerator.VisibleHeight;
end;

function TdxPageViewInfoGenerator.GetTotalHeight: Int64;
begin
  Result := FActiveGenerator.TotalHeight;
end;

function TdxPageViewInfoGenerator.GetTopInvisibleHeight: Int64;
begin
  Result := FRunningHeightAnchor.TopInvisibleHeight
end;

procedure TdxPageViewInfoGenerator.SetTopInvisibleHeight(const Value: Int64);
begin
  FRunningHeightAnchor.TopInvisibleHeight := Value;
end;

{ TdxPageViewInfoGeneratorBase }

constructor TdxPageViewInfoGeneratorBase.Create(ALayoutManager: TdxPageGeneratorLayoutManager;
  AFirstPageAnchor: TdxFirstPageAnchor; APages: TdxPageViewInfoCollection);
begin
  inherited Create;
  Assert(ALayoutManager <> nil, 'layoutManager = nil');
  Assert(AFirstPageAnchor <> nil, 'firstPageAnchor = nil');
  Assert(APages <> nil, 'pages = nil');
  FLayoutManager := ALayoutManager;
  FFirstPageAnchor := AFirstPageAnchor;
  FPages := APages;
  FPageRows := TdxPageViewInfoRowCollection.Create;
  CurrentPageRow := TdxPageViewInfoRow.Create(RowOwner);
  CurrentPageRow.Bounds := ALayoutManager.CreateInitialPageViewInfoRowBounds(0);
  FPageRows.Add(CurrentPageRow);
  ChangeState(TdxPageViewInfoGeneratorState.InvisibleEmptyRow);
end;

destructor TdxPageViewInfoGeneratorBase.Destroy;
begin
  FreeAndNil(FState);
  FreeAndNil(FPageRows);
  inherited Destroy;
end;

procedure TdxPageViewInfoGeneratorBase.RaiseRowFinished;
begin
  if not OnRowFinished.Empty then
    OnRowFinished.Invoke(Self, nil);
end;

procedure TdxPageViewInfoGeneratorBase.OnLayoutUnitChanging;
begin
end;

procedure TdxPageViewInfoGeneratorBase.OnLayoutUnitChanged;
begin
end;

function TdxPageViewInfoGeneratorBase.PreProcessPage(APage: TdxPage; APageIndex: Integer): TdxProcessPageResult;
begin
  Result := State.PreProcessPage(APage, APageIndex);
end;

function TdxPageViewInfoGeneratorBase.ProcessPage(APage: TdxPage; APageIndex: Integer): TdxProcessPageResult;
begin
  while True do
    if State.ProcessPage(APage, APageIndex) = TdxStateProcessPageResult.Success then
      Break;

  if State.&Type <> TdxPageViewInfoGeneratorState.VisiblePagesGenerationComplete then
    Result := TdxProcessPageResult.VisiblePagesGenerationIncomplete
  else
    Result := TdxProcessPageResult.VisiblePagesGenerationComplete;
end;

procedure TdxPageViewInfoGeneratorBase.AddPageRow(ARow: TdxPageViewInfoRow);
begin
  PageRows.Add(ARow);
end;

function TdxPageViewInfoGeneratorBase.ChangeState(AStateType: TdxPageViewInfoGeneratorState; AFreePrevState: Boolean = True): TdxPageViewInfoGeneratorStateBase;
begin
  if AFreePrevState then
    FreeAndNil(FState);
  FState := CreateState(AStateType);
  Result := FState;
end;

function TdxPageViewInfoGeneratorBase.CreateState(AStateType: TdxPageViewInfoGeneratorState): TdxPageViewInfoGeneratorStateBase;
begin
  case AStateType of
    TdxPageViewInfoGeneratorState.InvisibleEmptyRow:
      Result := CreateInvisibleEmptyRowState;
    TdxPageViewInfoGeneratorState.InvisibleRow:
      Result := CreateInvisibleRowState;
    TdxPageViewInfoGeneratorState.PartialVisibleEmptyRow:
      Result := CreatePartialVisibleEmptyRowState;
    TdxPageViewInfoGeneratorState.PartialVisibleRow:
      Result := CreatePartialVisibleRowState;
    TdxPageViewInfoGeneratorState.VisibleEmptyRow:
      Result := TdxStateVisibleEmptyRow.Create(Self);
    TdxPageViewInfoGeneratorState.VisibleRow:
      Result := TdxStateVisibleRow.Create(Self);
  else
      Result := TdxStateVisiblePagesGenerationComplete.Create(Self);
  end;
end;

function TdxPageViewInfoGeneratorBase.GetPageRowAtPoint(const APoint: TPoint; AStrictSearch: Boolean): TdxPageViewInfoRow;
begin
  Result := PageRows.GetRowAtPoint(APoint, AStrictSearch);
end;

function TdxPageViewInfoGeneratorBase.GetRowOwner: TdxOwnedRowInfoList;
begin
  Result := LayoutManager.ViewInfo.OwnedRows;
end;

procedure TdxPageViewInfoGeneratorBase.SetCurrentPageRow(Value: TdxPageViewInfoRow);
begin
  FCurrentPageRow := Value;
end;

{ TdxPageViewInfoGeneratorRunningHeight }

function TdxPageViewInfoGeneratorRunningHeight.CreateInvisibleEmptyRowState: TdxPageViewInfoGeneratorStateBase;
begin
  Result := TdxStateInvisibleEmptyRowRunningHeight.Create(Self);
end;

function TdxPageViewInfoGeneratorRunningHeight.CreateInvisibleRowState: TdxPageViewInfoGeneratorStateBase;
begin
  Result := TdxStateInvisibleRowRunningHeight.Create(Self);
end;

function TdxPageViewInfoGeneratorRunningHeight.CreatePartialVisibleEmptyRowState: TdxPageViewInfoGeneratorStateBase;
begin
  Result := TdxStatePartialVisibleEmptyRowRunningHeight.Create(Self);
end;

function TdxPageViewInfoGeneratorRunningHeight.CreatePartialVisibleRowState: TdxPageViewInfoGeneratorStateBase;
begin
  Result := TdxStatePartialVisibleRowRunningHeight.Create(Self);
end;

function TdxPageViewInfoGeneratorRunningHeight.GetFirstPageAnchor: TdxRunningHeightFirstPageAnchor;
begin
  Result := TdxRunningHeightFirstPageAnchor(inherited FirstPageAnchor);
end;

{ TdxPageViewInfoGeneratorFirstPageOffset }

function TdxPageViewInfoGeneratorFirstPageOffset.CreateInvisibleEmptyRowState: TdxPageViewInfoGeneratorStateBase;
begin
  Result := TdxStateInvisibleEmptyRowFirstPageOffset.Create(Self);
end;

function TdxPageViewInfoGeneratorFirstPageOffset.CreateInvisibleRowState: TdxPageViewInfoGeneratorStateBase;
begin
  Result := TdxStateInvisibleRowFirstPageOffset.Create(Self);
end;

function TdxPageViewInfoGeneratorFirstPageOffset.CreatePartialVisibleEmptyRowState: TdxPageViewInfoGeneratorStateBase;
begin
  Result := TdxStatePartialVisibleEmptyRowFirstPageOffset.Create(Self);
end;

function TdxPageViewInfoGeneratorFirstPageOffset.CreatePartialVisibleRowState: TdxPageViewInfoGeneratorStateBase;
begin
  Result := TdxStatePartialVisibleRowFirstPageOffset.Create(Self);
end;

function TdxPageViewInfoGeneratorFirstPageOffset.GetFirstPageAnchor: TdxFirstPageOffsetFirstPageAnchor;
begin
  Result := TdxFirstPageOffsetFirstPageAnchor(inherited FirstPageAnchor);
end;

{ TdxPageViewInfoGeneratorStateBase }

constructor TdxPageViewInfoGeneratorStateBase.Create(AGenerator: TdxPageViewInfoGeneratorBase);
begin
  inherited Create;
  Assert(AGenerator <> nil, 'generator = nil');
  FGenerator := AGenerator;
  FLayoutManager := AGenerator.LayoutManager;
end;

function TdxPageViewInfoGeneratorStateBase.GetRowOwner: TdxOwnedRowInfoList;
begin
  Result := LayoutManager.ViewInfo.OwnedRows;
end;

function TdxPageViewInfoGeneratorStateBase.GetViewPortBounds: TRect;
begin
  Result := LayoutManager.ViewPortBounds;
end;

function TdxPageViewInfoGeneratorStateBase.GetScaleFactor: System.Single;
begin
  Result := LayoutManager.ScaleFactor;
end;

function TdxPageViewInfoGeneratorStateBase.GetTotalHeight: Int64;
begin
  Result := Generator.TotalHeight;
end;

function TdxPageViewInfoGeneratorStateBase.GetFirstPageAnchor: TdxFirstPageAnchor;
begin
  Result := Generator.FirstPageAnchor;
end;

function TdxPageViewInfoGeneratorStateBase.GetVisibleHeight: Int64;
begin
  Result := Generator.VisibleHeight;
end;

procedure TdxPageViewInfoGeneratorStateBase.SetTotalHeight(const Value: Int64);
begin
  Generator.TotalHeight := Value;
end;

procedure TdxPageViewInfoGeneratorStateBase.SetVisibleHeight(const Value: Int64);
begin
  Generator.VisibleHeight := Value;
end;

function TdxPageViewInfoGeneratorStateBase.PreProcessPage(APage: TdxPage; APageIndex: Integer): TdxProcessPageResult;
begin
  Result := TdxProcessPageResult.VisiblePagesGenerationIncomplete;
end;

function TdxPageViewInfoGeneratorStateBase.ChangeState(AStateType: TdxPageViewInfoGeneratorState; AFreePrevState: Boolean = True): TdxPageViewInfoGeneratorStateBase;
begin
  Result := Generator.ChangeState(AStateType, AFreePrevState);
end;

procedure TdxPageViewInfoGeneratorStateBase.AddPageToOutput(APage: TdxPageViewInfo);
begin
  Generator.Pages.Add(APage);
end;

{ TdxAddPageGeneratorStateBase }

function TdxAddPageGeneratorStateBase.GetCurrentRow: TdxPageViewInfoRow;
begin
  Result := Generator.CurrentPageRow;
end;

procedure TdxAddPageGeneratorStateBase.AddPage(APage: TdxPage; AIndex: Integer);
var
  APageViewInfo: TdxPageViewInfo;
  ARow: TdxPageViewInfoRow;
begin
  ARow := CurrentRow;
  APageViewInfo := TdxPageViewInfo.Create(APage, LayoutManager.ViewInfo.OwnedPageViewInfos);
  APageViewInfo.Index := AIndex;
  APageViewInfo.Bounds := TRect.CreateSize(ARow.Bounds.Right, ARow.Bounds.Top, LayoutManager.CalculatePagePhysicalSize(APage));
  ARow.Add(APageViewInfo);
  RecalculateCurrentRowPagesLayout(APage);
  LayoutManager.UpdatePageClientBounds(APageViewInfo);
  ARow.Bounds := TRect.CreateSize(ARow.Bounds.Location, ARow.Bounds.Width + APageViewInfo.Bounds.Width,
    Max(ARow.Bounds.Height, APageViewInfo.Bounds.Height));
  UpdateHeights(APage, APageViewInfo);
  AddPageToOutput(APageViewInfo);
  Assert(ARow = CurrentRow);
end;

function TdxAddPageGeneratorStateBase.CanAddPage(APage: TdxPage): Boolean;
begin
  Result := LayoutManager.CanFitPageToPageRow(APage, CurrentRow);
end;

procedure TdxAddPageGeneratorStateBase.UpdateVisibleHeight(APageViewInfo: TdxPageViewInfo);
var
  APageVisibleLogicalHeight: Integer;
begin
  APageVisibleLogicalHeight := LayoutManager.CalculatePageVisibleLogicalHeight(APageViewInfo);
  if APageVisibleLogicalHeight <= 0 then
    APageVisibleLogicalHeight := 0;
  UpdateVisibleHeightCore(APageVisibleLogicalHeight);
end;

procedure TdxAddPageGeneratorStateBase.UpdateVisibleHeightCore(AVisiblePageHeight: Integer);
begin
  VisibleHeight := VisibleHeight + AVisiblePageHeight;
end;

procedure TdxAddPageGeneratorStateBase.UpdateHeights(APage: TdxPage; APageViewInfo: TdxPageViewInfo);
begin
  UpdateVisibleHeight(APageViewInfo);
  TotalHeight := TotalHeight + LayoutManager.CalculatePageLogicalHeight(APage);
end;

function TdxAddPageGeneratorStateBase.CalculatePageBounds(APage: TdxPage): TRect;
var
  APageSize: TSize;
  ARowBounds: TRect;
begin
  ARowBounds := CurrentRow.Bounds;
  APageSize := LayoutManager.CalculatePagePhysicalSize(APage);
  Result.InitSize(ARowBounds.Right, ARowBounds.Top, APageSize);
end;

function TdxAddPageGeneratorStateBase.CalculatePageVisibility(APage: TdxPage; APageIndex: Integer): TdxPageVisibility;
var
  APageViewInfoVisibleBounds, AExtendedViewPortBounds, APageBounds: TRect;
begin
  APageBounds := CalculatePageBounds(APage);
  AExtendedViewPortBounds.InitSize(0, ViewPortBounds.Top, MaxInt, ViewPortBounds.Height);
  APageViewInfoVisibleBounds := AExtendedViewPortBounds;
  APageViewInfoVisibleBounds.Intersect(APageBounds);
  if APageBounds.IsEqual(APageViewInfoVisibleBounds) then
    Exit(TdxPageVisibility.Entire)
  else
    if (APageViewInfoVisibleBounds.Width > 0) and (APageViewInfoVisibleBounds.Height > 0) then
      Exit(TdxPageVisibility.Partial)
    else
      Exit(TdxPageVisibility.Invisible);
end;

procedure TdxAddPageGeneratorStateBase.FinishCurrentRow;
var
  ANewRow: TdxPageViewInfoRow;
begin
  Generator.RaiseRowFinished;
  ANewRow := TdxPageViewInfoRow.Create(RowOwner);
  ANewRow.Bounds := LayoutManager.CreateInitialPageViewInfoRowBounds(CurrentRow.Bounds.Bottom);
  Generator.AddPageRow(ANewRow);
  Generator.CurrentPageRow := ANewRow;
end;

procedure TdxAddPageGeneratorStateBase.AlignPagesVertically;
begin
  LayoutManager.AlignPages(CurrentRow);
end;

{ TdxStateInvisibleRowBase }

constructor TdxStateInvisibleRowBase.Create(AGenerator: TdxPageViewInfoGeneratorBase);
begin
  inherited Create(AGenerator);
  FCurrentRow := TdxPageViewInfoRow.Create(RowOwner);
  FCurrentRow.Bounds := LayoutManager.CreateInitialPageViewInfoRowBounds(0);
end;

function TdxStateInvisibleRowBase.GetCurrentRow: TdxPageViewInfoRow;
begin
  Result := FCurrentRow;
end;

function TdxStateInvisibleRowBase.CalculatePageVisibility(APage: TdxPage; APageIndex: Integer): TdxPageVisibility;
begin
  if IsPageFullyInvisible(APage, APageIndex) then
    Result := TdxPageVisibility.Invisible
  else
    Result := TdxPageVisibility.Partial;
end;

procedure TdxStateInvisibleRowBase.AddPageToOutput(APage: TdxPageViewInfo);
begin
  //do nothing
end;

procedure TdxStateInvisibleRowBase.UpdateVisibleHeight(APageViewInfo: TdxPageViewInfo);
begin
  //do nothing
end;

procedure TdxStateInvisibleRowBase.CopyPagesTo(ANewState: TdxPageViewInfoGeneratorStateBase);
var
  I: Integer;
  ARow, ACurrentRow: TdxPageViewInfoRow;
begin
  ARow := ANewState.CurrentRow;
  ACurrentRow := CurrentRow;
  ARow.AddRange(ACurrentRow);
  ARow.Bounds := ACurrentRow.Bounds;
  for I := 0 to ACurrentRow.Count - 1 do
    ANewState.AddPageToOutput(ACurrentRow[I]);
  ACurrentRow.Clear;
end;

procedure TdxStateInvisibleRowBase.HideStateCurrentRow(ANewState: TdxPageViewInfoGeneratorStateBase);
var
  ARow: TdxPageViewInfoRow;
begin
  ARow := ANewState.CurrentRow;
  LayoutManager.OffsetRowVertically(ARow, -ARow.Bounds.Height);
end;

procedure TdxStateInvisibleRowBase.FinishCurrentRow;
begin
  FCurrentRow := TdxPageViewInfoRow.Create(RowOwner);
  FCurrentRow.Bounds := LayoutManager.CreateInitialPageViewInfoRowBounds(0);
end;

procedure TdxStateInvisibleRowBase.RecalculateCurrentRowPagesLayout(APage: TdxPage);
begin
  //do nothing
end;

{ TdxStateInvisibleEmptyRow }

function TdxStateInvisibleEmptyRow.CalculatePageVisibility(APage: TdxPage;
  APageIndex: Integer): TdxPageVisibility;
begin
  if IsPageFullyVisible(APage, APageIndex) then
    Result := TdxPageVisibility.Entire
  else
    Result := inherited CalculatePageVisibility(APage, APageIndex);
end;

function TdxStateInvisibleEmptyRow.GetType: TdxPageViewInfoGeneratorState;
begin
  Result := TdxPageViewInfoGeneratorState.InvisibleEmptyRow;
end;

function TdxStateInvisibleEmptyRow.ProcessPage(APage: TdxPage;
  APageIndex: Integer): TdxStateProcessPageResult;
var
  ANewState, APrevState: TdxPageViewInfoGeneratorStateBase;
begin
  case CalculatePageVisibility(APage, APageIndex) of
    TdxPageVisibility.Invisible:
      begin
        AddPage(APage, APageIndex);
        APrevState := Generator.State;
        try
          ANewState := ChangeState(TdxPageViewInfoGeneratorState.InvisibleRow, False);
          CopyPagesTo(ANewState);
        finally
          APrevState.Free;
        end;
        Result := TdxStateProcessPageResult.Success;
      end;
    TdxPageVisibility.Partial:
      begin
        ChangeState(TdxPageViewInfoGeneratorState.PartialVisibleEmptyRow);
        Result := TdxStateProcessPageResult.TryAgain;
      end;
  else
      ChangeState(TdxPageViewInfoGeneratorState.VisibleEmptyRow);
      Result := TdxStateProcessPageResult.TryAgain;
  end;
end;

{ TdxStateInvisibleRow }

function TdxStateInvisibleRow.GetType: TdxPageViewInfoGeneratorState;
begin
  Result := TdxPageViewInfoGeneratorState.InvisibleRow;
end;

function TdxStateInvisibleRow.ProcessPage(APage: TdxPage; APageIndex: Integer): TdxStateProcessPageResult;
begin
  if IsPageFullyVisible(APage, APageIndex) then
  begin
    if CanAddPage(APage) then
      SwitchToPartialVisibleRowState(APage, APageIndex)
    else
    begin
      FinishCurrentRow;
      ChangeState(TdxPageViewInfoGeneratorState.VisibleEmptyRow);
    end;
    Exit(TdxStateProcessPageResult.TryAgain);
  end;
  if not CanAddPage(APage) then
  begin
    FinishCurrentRow;
    ChangeState(TdxPageViewInfoGeneratorState.InvisibleEmptyRow);
    Exit(TdxStateProcessPageResult.TryAgain);
  end;
  case CalculatePageVisibility(APage, APageIndex) of
    TdxPageVisibility.Invisible:
      begin
        AddPage(APage, APageIndex);
        Result := TdxStateProcessPageResult.Success;
      end;
    TdxPageVisibility.Partial:
      Result := SwitchToPartialVisibleRowState(APage, APageIndex);
    else
     TdxRichEditExceptions.ThrowInternalException;
     Result := TdxStateProcessPageResult.Success;
  end;
end;

function TdxStateInvisibleRow.SwitchToPartialVisibleRowState(APage: TdxPage; APageIndex: Integer): TdxStateProcessPageResult;
var
  AState, APrevState: TdxPageViewInfoGeneratorStateBase;
begin
  AlignPagesVertically;
  APrevState := Generator.State;
  try
    AState := ChangeState(TdxPageViewInfoGeneratorState.PartialVisibleRow, False);
    RollbackStateToTheStartOfRow;
    CopyPagesTo(AState);
    HideStateCurrentRow(AState);
  finally
    APrevState.Free;
  end;
  Result := TdxStateProcessPageResult.TryAgain;
end;

{ TdxStateInvisibleEmptyRowRunningHeight }

function TdxStateInvisibleEmptyRowRunningHeight.IsPageFullyInvisible(APage: TdxPage; APageIndex: Integer): Boolean;
var
  ARunningHeight: Int64;
begin
  ARunningHeight := Generator.RunningHeight + LayoutManager.CalculatePageLogicalHeight(APage);
  Result := ARunningHeight <= Generator.FirstPageAnchor.TopInvisibleHeight;
end;

function TdxStateInvisibleEmptyRowRunningHeight.IsPageFullyVisible(APage: TdxPage; APageIndex: Integer): Boolean;
begin
  Result := Generator.RunningHeight = Generator.FirstPageAnchor.TopInvisibleHeight;
end;

procedure TdxStateInvisibleEmptyRowRunningHeight.UpdateHeights(APage: TdxPage; APageViewInfo: TdxPageViewInfo);
begin
  inherited UpdateHeights(APage, APageViewInfo);
  Generator.RunningHeight := Generator.RunningHeight + LayoutManager.CalculatePageLogicalHeight(APage);
end;

function TdxStateInvisibleEmptyRowRunningHeight.GetGenerator: TdxPageViewInfoGeneratorRunningHeight;
begin
  Result := TdxPageViewInfoGeneratorRunningHeight(inherited Generator);
end;

{ TdxStateInvisibleRowRunningHeight }

function TdxStateInvisibleRowRunningHeight.IsPageFullyInvisible(APage: TdxPage;
  APageIndex: Integer): Boolean;
var
  ARunningHeight: Int64;
begin
  ARunningHeight := Generator.RunningHeight + LayoutManager.CalculatePageLogicalHeight(APage);
  Result := ARunningHeight <= Generator.FirstPageAnchor.TopInvisibleHeight;
end;

function TdxStateInvisibleRowRunningHeight.IsPageFullyVisible(APage: TdxPage;
  APageIndex: Integer): Boolean;
begin
  Result := Generator.RunningHeight = Generator.FirstPageAnchor.TopInvisibleHeight;
end;

procedure TdxStateInvisibleRowRunningHeight.RollbackStateToTheStartOfRow;
begin
  Generator.RunningHeight := Generator.RunningHeight - LayoutManager.CalculatePagesTotalLogicalHeight(CurrentRow);
end;

procedure TdxStateInvisibleRowRunningHeight.UpdateHeights(APage: TdxPage; APageViewInfo: TdxPageViewInfo);
begin
  inherited UpdateHeights(APage, APageViewInfo);
  Generator.RunningHeight := Generator.RunningHeight + LayoutManager.CalculatePageLogicalHeight(APage);
end;

function TdxStateInvisibleRowRunningHeight.GetGenerator: TdxPageViewInfoGeneratorRunningHeight;
begin
  Result := TdxPageViewInfoGeneratorRunningHeight(inherited Generator);
end;

{ TdxStateInvisibleEmptyRowFirstPageOffset }

function TdxStateInvisibleEmptyRowFirstPageOffset.GetGenerator: TdxPageViewInfoGeneratorFirstPageOffset;
begin
  Result := TdxPageViewInfoGeneratorFirstPageOffset(inherited Generator);
end;

function TdxStateInvisibleEmptyRowFirstPageOffset.IsPageFullyInvisible(APage: TdxPage; APageIndex: Integer): Boolean;
var
  AAnchor: TdxFirstPageOffsetFirstPageAnchor;
begin
  AAnchor := Generator.FirstPageAnchor;
  Result := APageIndex < AAnchor.PageIndex;
end;

function TdxStateInvisibleEmptyRowFirstPageOffset.IsPageFullyVisible(APage: TdxPage; APageIndex: Integer): Boolean;
begin
  Result := False;
end;

{ TdxStateInvisibleRowFirstPageOffset }

procedure TdxStateInvisibleRowFirstPageOffset.HideStateCurrentRow(
  ANewState: TdxPageViewInfoGeneratorStateBase);
var
  ASectionLine, ALastPageOffset: Integer;
  ARow: TdxPageViewInfoRow;
begin
  ALastPageOffset := Ceil(Generator.FirstPageAnchor.VerticalOffset * ScaleFactor);
  ARow := ANewState.CurrentRow;
  ASectionLine := ARow.Last.Bounds.Top + ALastPageOffset;
  LayoutManager.OffsetRowVertically(ARow, - ARow.Bounds.Top - ASectionLine);
  VisibleHeight := LayoutManager.CalculatePagesTotalVisibleLogicalHeight(ARow);
end;

function TdxStateInvisibleRowFirstPageOffset.IsPageFullyInvisible(APage: TdxPage; APageIndex: Integer): Boolean;
var
  AAnchor: TdxFirstPageOffsetFirstPageAnchor;
begin
  AAnchor := Generator.FirstPageAnchor;
  Result := APageIndex < AAnchor.PageIndex;
end;

function TdxStateInvisibleRowFirstPageOffset.IsPageFullyVisible(APage: TdxPage;
  APageIndex: Integer): Boolean;
begin
  Result := False;
end;

procedure TdxStateInvisibleRowFirstPageOffset.RollbackStateToTheStartOfRow;
begin
//do nothing
end;

function TdxStateInvisibleRowFirstPageOffset.SwitchToPartialVisibleRowState(APage: TdxPage; APageIndex: Integer): TdxStateProcessPageResult;
begin
  AddPage(APage, APageIndex);
  inherited SwitchToPartialVisibleRowState(APage, APageIndex);
  Result := TdxStateProcessPageResult.Success;
end;

function TdxStateInvisibleRowFirstPageOffset.GetGenerator: TdxPageViewInfoGeneratorFirstPageOffset;
begin
  Result := TdxPageViewInfoGeneratorFirstPageOffset(inherited Generator);
end;

{ TdxStatePartialVisibleEmptyRow }

function TdxStatePartialVisibleEmptyRow.ProcessPage(APage: TdxPage; APageIndex: Integer): TdxStateProcessPageResult;
begin
  Assert(VisibleHeight = 0);
  AddPage(APage, APageIndex);
  ChangeState(TdxPageViewInfoGeneratorState.PartialVisibleRow);
  Result := TdxStateProcessPageResult.Success;
end;

procedure TdxStatePartialVisibleEmptyRow.RecalculateCurrentRowPagesLayout(APage: TdxPage);
var
  ARow: TdxPageViewInfoRow;
  APageViewInfo: TdxPageViewInfo;
  AInvisiblePageAreaHeight: Integer;
begin
  ARow := CurrentRow;
  Assert(ARow.Count = 1);
  AInvisiblePageAreaHeight := CalculateVerticalRowOffset;
  ARow.Bounds := LayoutManager.OffsetRectangle(ARow.Bounds, 0, -AInvisiblePageAreaHeight);
  APageViewInfo := ARow[0];
  APageViewInfo.Bounds := LayoutManager.OffsetRectangle(APageViewInfo.Bounds, 0, -AInvisiblePageAreaHeight);
  LayoutManager.UpdatePageClientBounds(APageViewInfo);
end;

procedure TdxStatePartialVisibleEmptyRow.UpdateHeights(APage: TdxPage; APageViewInfo: TdxPageViewInfo);
begin
  AlignPagesVertically;
  inherited UpdateHeights(APage, APageViewInfo);
end;

function TdxStatePartialVisibleEmptyRow.GetType: TdxPageViewInfoGeneratorState;
begin
  Result := TdxPageViewInfoGeneratorState.PartialVisibleEmptyRow;
end;

{ TdxStatePartialVisibleRow }

function TdxStatePartialVisibleRow.PreProcessPage(APage: TdxPage; APageIndex: Integer): TdxProcessPageResult;
begin
  if (CurrentRow.Bounds.Bottom >= ViewPortBounds.Bottom) and not CanAddPage(APage) then
    Result := TdxProcessPageResult.VisiblePagesGenerationComplete
  else
    Result := TdxProcessPageResult.VisiblePagesGenerationIncomplete;
end;

function TdxStatePartialVisibleRow.ProcessPage(APage: TdxPage; APageIndex: Integer): TdxStateProcessPageResult;
begin
  if not CanAddPage(APage) then
  begin
    FinishCurrentRow;
    ChangeState(TdxPageViewInfoGeneratorState.VisibleEmptyRow);
    Result := TdxStateProcessPageResult.TryAgain;
  end
  else
  begin
    AddPage(APage, APageIndex);
    Result := TdxStateProcessPageResult.Success;
  end;
end;

procedure TdxStatePartialVisibleRow.UpdateHeights(APage: TdxPage; APageViewInfo: TdxPageViewInfo);
begin
  AlignPagesVertically;
  inherited UpdateHeights(APage, APageViewInfo);
end;

procedure TdxStatePartialVisibleRow.UpdateVisibleHeight(APageViewInfo: TdxPageViewInfo);
begin
  VisibleHeight := LayoutManager.CalculatePagesTotalVisibleLogicalHeight(CurrentRow);
end;

function TdxStatePartialVisibleRow.GetType: TdxPageViewInfoGeneratorState;
begin
  Result := TdxPageViewInfoGeneratorState.PartialVisibleRow;
end;

{ TdxStatePartialVisibleEmptyRowRunningHeight }

function TdxStatePartialVisibleEmptyRowRunningHeight.CalculateVerticalRowOffset: Integer;
begin
  Result := Round((Generator.FirstPageAnchor.TopInvisibleHeight - Generator.RunningHeight) * ScaleFactor);
end;

function TdxStatePartialVisibleEmptyRowRunningHeight.GetGenerator: TdxPageViewInfoGeneratorRunningHeight;
begin
  Result := TdxPageViewInfoGeneratorRunningHeight(inherited Generator);
end;

{ TdxStatePartialVisibleRowRunningHeight }

function TdxStatePartialVisibleRowRunningHeight.CalculatePartialRowOffset: Integer;
var
  ALow, AHi, AMedian, ACompareResult: Integer;
  ATotalInvisibleRowHeight: Int64;
begin
  ATotalInvisibleRowHeight := Ceil((Generator.FirstPageAnchor.TopInvisibleHeight - Generator.RunningHeight) * ScaleFactor);
  ALow := 1;
  AHi := CurrentRow.Bounds.Height;
  while ALow <= AHi do
  begin
    AMedian := ALow + (AHi - ALow) shr 1;
    ACompareResult := CheckRowOffset(AMedian, ATotalInvisibleRowHeight);
    if ACompareResult = 0 then
      Exit(AMedian);
    if ACompareResult < 0 then
      ALow := AMedian + 1
    else
      AHi := AMedian - 1;
  end;
  Result := not ALow;
end;

function TdxStatePartialVisibleRowRunningHeight.CheckRowOffset(AOffset: Integer; ATotalInvisibleRowHeight: Int64): Integer;
var
  I, ADelta, ATotal: Integer;
  APageBounds, AInvisibleBounds: TRect;
  ARow: TdxPageViewInfoRow;
begin
  ARow := CurrentRow;
  AInvisibleBounds := ARow.Bounds;
  AInvisibleBounds.Height := AOffset;
  ATotal := 0;
  for I := 0 to ARow.Count - 1 do
  begin
    APageBounds := ARow[I].Bounds;
    ADelta := Min(AInvisibleBounds.Bottom, APageBounds.Bottom) - APageBounds.Top;
    if ADelta > 0 then
      Inc(ATotal, ADelta);
  end;
  if ATotal < ATotalInvisibleRowHeight then
    Exit(-1);
  if ATotal > ATotalInvisibleRowHeight then
    Exit(1);
  Result := 0;
end;

procedure TdxStatePartialVisibleRowRunningHeight.RecalculateCurrentRowPagesLayout(APage: TdxPage);
var
  APhysicalPageHeight, AOffset: Integer;
  APageViewInfo: TdxPageViewInfo;
  ARow: TdxPageViewInfoRow;
begin
  ARow := CurrentRow;
  APhysicalPageHeight := LayoutManager.CalculatePagePhysicalHeight(APage);
  APageViewInfo := ARow.Last;
  APageViewInfo.Bounds := LayoutManager.OffsetRectangle(APageViewInfo.Bounds, 0, (ARow.Bounds.Height - APhysicalPageHeight) div 2);
  if APageViewInfo.Bounds.Top < ARow.Bounds.Top then
    ARow.Bounds := TRect.CreateSize(ARow.Bounds.Left, APageViewInfo.Bounds.Top, ARow.Bounds.Width, APageViewInfo.Bounds.Height);
  AOffset := CalculatePartialRowOffset;
  Assert(AOffset <> not (ARow.Bounds.Height + 1));
  if AOffset < 0 then
    AOffset := not AOffset;
  LayoutManager.OffsetRowVertically(ARow, - ARow.Bounds.Top - AOffset);
end;

function TdxStatePartialVisibleRowRunningHeight.GetGenerator: TdxPageViewInfoGeneratorRunningHeight;
begin
  Result := TdxPageViewInfoGeneratorRunningHeight(inherited Generator);
end;

{ TdxStatePartialVisibleEmptyRowFirstPageOffset }

function TdxStatePartialVisibleEmptyRowFirstPageOffset.CalculateVerticalRowOffset: Integer;
begin
  Result := Ceil(Generator.FirstPageAnchor.VerticalOffset * ScaleFactor);
end;

function TdxStatePartialVisibleEmptyRowFirstPageOffset.GetGenerator: TdxPageViewInfoGeneratorFirstPageOffset;
begin
  Result := TdxPageViewInfoGeneratorFirstPageOffset(inherited Generator);
end;

{ TdxStatePartialVisibleRowFirstPageOffset }

procedure TdxStatePartialVisibleRowFirstPageOffset.RecalculateCurrentRowPagesLayout(APage: TdxPage);
begin
 //do nothing
end;

procedure TdxStatePartialVisibleRowFirstPageOffset.UpdateHeights(APage: TdxPage; APageViewInfo: TdxPageViewInfo);
begin
  AlignPagesVertically;
  inherited UpdateHeights(APage, APageViewInfo);
end;

function TdxStatePartialVisibleRowFirstPageOffset.GetGenerator: TdxPageViewInfoGeneratorFirstPageOffset;
begin
  Result := TdxPageViewInfoGeneratorFirstPageOffset(inherited Generator);
end;

{ TdxStateVisibleEmptyRow }

function TdxStateVisibleEmptyRow.GetType: TdxPageViewInfoGeneratorState;
begin
  Result := TdxPageViewInfoGeneratorState.VisibleEmptyRow;
end;

function TdxStateVisibleEmptyRow.ProcessPage(APage: TdxPage; APageIndex: Integer): TdxStateProcessPageResult;
var
  AState: TdxStateVisibleRow;
  AInitialVisibleHeight: Int64;
begin
  if CalculatePageVisibility(APage, APageIndex) = TdxPageVisibility.Invisible then
  begin
    RemoveLastRow;
    ChangeState(TdxPageViewInfoGeneratorState.VisiblePagesGenerationComplete);
    Result := TdxStateProcessPageResult.TryAgain;
  end
  else
  begin
    AInitialVisibleHeight := VisibleHeight;
    AddPage(APage, APageIndex);
    AState := TdxStateVisibleRow(ChangeState(TdxPageViewInfoGeneratorState.VisibleRow));
    AState.InitialVisibleHeight := AInitialVisibleHeight;
    Result := TdxStateProcessPageResult.Success;
  end;
end;

procedure TdxStateVisibleEmptyRow.RecalculateCurrentRowPagesLayout(APage: TdxPage);
begin
//do nothing
end;

procedure TdxStateVisibleEmptyRow.RemoveLastRow;
var
  ARows: TdxPageViewInfoRowCollection;
begin
  ARows := Generator.PageRows;
  ARows.Delete(ARows.Count - 1);
end;

procedure TdxStateVisibleEmptyRow.UpdateHeights(APage: TdxPage; APageViewInfo: TdxPageViewInfo);
begin
  AlignPagesVertically;
  inherited UpdateHeights(APage, APageViewInfo);
end;

{ TdxStateVisibleRow }

function TdxStateVisibleRow.PreProcessPage(APage: TdxPage; APageIndex: Integer): TdxProcessPageResult;
begin
  if (CurrentRow.Bounds.Bottom >= ViewPortBounds.Bottom) and not CanAddPage(APage) then
    Result := TdxProcessPageResult.VisiblePagesGenerationComplete
  else
    Result := TdxProcessPageResult.VisiblePagesGenerationIncomplete;
end;

function TdxStateVisibleRow.ProcessPage(APage: TdxPage; APageIndex: Integer): TdxStateProcessPageResult;
begin
  if not CanAddPage(APage) then
  begin
    FinishCurrentRow;
    ChangeState(TdxPageViewInfoGeneratorState.VisibleEmptyRow);
    Exit(TdxStateProcessPageResult.TryAgain);
  end;
  case CalculatePageVisibility(APage, APageIndex) of
    TdxPageVisibility.Partial,
    TdxPageVisibility.Entire:
      begin
        AddPage(APage, APageIndex);
        Exit(TdxStateProcessPageResult.Success);
      end;
  else
    Assert(False, 'Error!');
    Result := TdxStateProcessPageResult.Success;
  end;
end;

procedure TdxStateVisibleRow.RecalculateCurrentRowPagesLayout(APage: TdxPage);
begin
//do nothing
end;

procedure TdxStateVisibleRow.UpdateVisibleHeight(APageViewInfo: TdxPageViewInfo);
begin
  AlignPagesVertically;
  VisibleHeight := InitialVisibleHeight + LayoutManager.CalculatePagesTotalVisibleLogicalHeight(CurrentRow);
end;

function TdxStateVisibleRow.GetType: TdxPageViewInfoGeneratorState;
begin
  Result := TdxPageViewInfoGeneratorState.VisibleRow;
end;

{ TdxStateVisiblePagesGenerationComplete }

function TdxStateVisiblePagesGenerationComplete.GetCurrentRow: TdxPageViewInfoRow;
begin
  Result := nil;
end;

function TdxStateVisiblePagesGenerationComplete.GetType: TdxPageViewInfoGeneratorState;
begin
  Result := TdxPageViewInfoGeneratorState.VisiblePagesGenerationComplete;
end;

function TdxStateVisiblePagesGenerationComplete.ProcessPage(APage: TdxPage; APageIndex: Integer): TdxStateProcessPageResult;
begin
  TotalHeight := TotalHeight + LayoutManager.CalculatePageLogicalHeight(APage);
  Result := TdxStateProcessPageResult.Success;
end;

{ TdxInvisiblePageRowsGenerator }

constructor TdxInvisiblePageRowsGenerator.Create(APages: TdxPageCollection; ASourceLayoutManager: TdxPageGeneratorLayoutManager);
var
  AViewPortBounds: TRect;
begin
  Assert(APages <> nil);
  Assert(ASourceLayoutManager <> nil);
  FLayoutManager := ASourceLayoutManager.Clone;
  AViewPortBounds := LayoutManager.ViewPortBounds;
  AViewPortBounds.Height := MaxInt div 2;
  LayoutManager.ViewPortBounds := AViewPortBounds;
  FPages := APages;
  FRunningHeightFirstPageAnchor := TdxRunningHeightFirstPageAnchor.Create;
  FPageViewInfoCollection := TdxPageViewInfoCollection.Create;
  FGenerator := TdxPageViewInfoGeneratorRunningHeight.Create(LayoutManager, FRunningHeightFirstPageAnchor, FPageViewInfoCollection);
  Reset;
end;

destructor TdxInvisiblePageRowsGenerator.Destroy;
begin
  FreeAndNil(FRunningHeightFirstPageAnchor);
  FreeAndNil(FPageViewInfoCollection);
  FreeAndNil(FGenerator);
  FreeAndNil(FLayoutManager);
  inherited Destroy;
end;

function TdxInvisiblePageRowsGenerator.GenerateNextRow: TdxPageViewInfoRow;
begin
  if FIsFinished then
    Exit(nil);

  FResult := nil;
  FGenerator.OnRowFinished.Add(OnRowFinished);
  try
    while FCurrentPageIndex <> FFirstInvalidPageIndex do
    begin
      Generator.ProcessPage(Pages[CurrentPageIndex], FCurrentPageIndex);
      Inc(FPagesProcessed);
      if FResult <> nil then
      begin
        Inc(FCurrentPageIndex, Step);
        Exit(FResult);
      end;
      Inc(FCurrentPageIndex, Step);
    end;
  finally
    FGenerator.OnRowFinished.Remove(OnRowFinished);
  end;
  FIsFinished := True;
  if FPagesProcessed <= 0 then
    Exit(nil);
  FResult := Generator.PageRows.Last;
  Exit(FResult);
end;

procedure TdxInvisiblePageRowsGenerator.OnRowFinished(Sender: TObject;
  E: TdxEventArgs);
begin
  FResult := Generator.PageRows.Last;
end;

procedure TdxInvisiblePageRowsGenerator.Reset;
begin
  if FFirstPageIndex <= FFirstInvalidPageIndex then
    FStep := 1
  else
    FStep := -1;
  FCurrentPageIndex := FFirstPageIndex;
  FPagesProcessed := 0;
  FIsFinished := False;
  FResult := nil;
end;

procedure TdxInvisiblePageRowsGenerator.SetFirstInvalidPageIndex(
  const Value: Integer);
begin
  if FFirstInvalidPageIndex <> Value then
  begin
    FFirstInvalidPageIndex := Value;
    Reset;
  end;
end;

procedure TdxInvisiblePageRowsGenerator.SetFirstPageIndex(const Value: Integer);
begin
  if FFirstPageIndex <> Value then
  begin
    FFirstPageIndex := Value;
    Reset;
  end;
end;

end.
