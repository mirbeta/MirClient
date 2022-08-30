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

unit dxRichEdit.View.Simple;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface


uses
  SysUtils, Types, Generics.Defaults, Generics.Collections, Controls, SyncObjs, Classes, Graphics, ActiveX,
  cxGraphics, dxCoreClasses, cxControls, dxCoreGraphics,
  dxRichEdit.Types,
  dxRichEdit.Utils.Types,
  dxRichEdit.Utils.BatchUpdateHelper,
  dxRichEdit.Utils.DataObject,
  dxRichEdit.Utils.BackgroundThreadUIUpdater,
  dxRichEdit.Utils.PredefinedFontSizeCollection,
  dxRichEdit.Utils.Graphics,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.Section,
  dxRichEdit.DocumentModel.SectionFormatting,
  dxRichEdit.DocumentModel.Boxes,
  dxRichEdit.DocumentModel.Selection,
  dxRichEdit.DocumentModel.Tables,
  dxRichEdit.DocumentModel.TableCalculator,
  dxRichEdit.DocumentModel.FloatingObjectFormatting,
  dxRichEdit.DocumentLayout,
  dxRichEdit.DocumentLayout.Position,
  dxRichEdit.LayoutEngine.Formatter,
  dxRichEdit.LayoutEngine.DocumentFormatter,
  dxRichEdit.Control.HitTest,
  dxRichEdit.Control.HotZones,
  dxRichEdit.View.Core,
  dxRichEdit.View.ViewInfo,
  dxRichEdit.View.PageViewInfoGenerator,
  dxRichEdit.Platform.Win.Painter, cxGeometry;

type
  TdxSimpleView = class;
  TdxSimpleViewPageController = class;
  TdxSimpleViewDocumentFormattingController = class;

  { TdxSimpleViewColumnsBoundsCalculator }

  TdxSimpleViewColumnsBoundsCalculator = class(TdxColumnsBoundsCalculator)
  public
    procedure PopulateColumnsBounds(const AResult: TdxRectList; const ABounds: TRect;
      const AColumnInfoCollection: TdxColumnInfoCollection); override;
    procedure PopulateEqualWidthColumnsBounds(const AResult: TdxRectList; const ABounds: TRect;
      AColumnCount, ASpaceBetweenColumns: Integer); override;
  end;

  { TdxSimpleViewColumnController }

  TdxSimpleViewColumnController = class(TdxColumnController)
  protected
    function GetShouldZeroSpacingBeforeWhenMoveRowToNextColumn: Boolean; override;
  public
    function CalculateColumnBoundsCore(AColumnIndex: Integer): TRect; override;
    function CreateColumnBoundsCalculator: TdxColumnsBoundsCalculator; override;
  end;

  { TdxSimplePageBoundsCalculator }

  TdxSimplePageBoundsCalculator = class(TdxPageBoundsCalculator)
  private
    FController: TdxSimpleViewPageController;
  protected
    function CalculatePageBounds(ASection: TdxSection): TRect; override;
  public
    constructor Create(AController: TdxSimpleViewPageController);
    function CalculatePageClientBoundsCore(APageWidth, APageHeight, AMarginLeft, AMarginTop, AMarginRight, AMarginBottom: Integer): TRect; override;
  end;

  { TdxSimpleViewCurrentHorizontalPositionController }

  TdxSimpleViewCurrentHorizontalPositionController = class(TdxCurrentHorizontalPositionController)
  protected
    function CanFitBoxToCurrentRow(const ABoxSize: TSize): Boolean; override;
    function GetMaxBoxWidth: Integer; override;
  end;

  { TdxSimpleViewBoxHitTestCalculator }

  TdxSimpleViewBoxHitTestCalculator = class(TdxBoxHitTestCalculator)
  public
    procedure ProcessColumnCollection(ACollection: TdxColumnCollection); override;
  end;

  { TdxSimpleViewPageController }

  TdxSimpleViewPageController = class(TdxNonPrintViewPageControllerBase)
  strict private
    FPageSize: TSize;
    FMinPageWidth: Integer;
  private
    function GetVirtualPageHeight: Integer;
    function GetVirtualPageWidth: Integer;
    procedure SetVirtualPageHeight(const Value: Integer);
    procedure SetVirtualPageWidth(const Value: Integer);
  protected
    function GetMaxWidth(AAreas: TdxPageAreaCollection): Integer; overload; virtual;
    function GetMaxWidth(ARows: TdxRowCollection): Integer; overload; virtual;
    procedure ResetPageSize;

    property PageSize: TSize read FPageSize;
  public
    constructor Create(ADocumentLayout: TdxDocumentLayout);
    function CreatePageBoundsCalculator: TdxPageBoundsCalculator; override;
    function CreateHitTestCalculator(const ARequest: TdxRichEditHitTestRequest;
      AResult: TdxRichEditHitTestResult): TdxBoxHitTestCalculator; override;
    procedure FinalizePagePrimaryFormatting(APage: TdxPage; ASkipAddingFloatingObjects: Boolean); override;

    property MinPageWidth: Integer read FMinPageWidth write FMinPageWidth;
    property VirtualPageHeight: Integer read GetVirtualPageHeight write SetVirtualPageHeight;
    property VirtualPageWidth: Integer read GetVirtualPageWidth write SetVirtualPageWidth;
  end;

  { TdxSimpleViewTableViewInfoManager }

  TdxSimpleViewTableViewInfoManager = class(TdxTableViewInfoManager)
  protected
    function GetSplitAnchorHorizontalCellBorders(ASplitAnchor: TdxTableCellVerticalAnchor): TdxHorizontalCellBordersInfoList; override;
  public
    procedure FixColumnOverflow; override;
  end;

  { TdxSimpleViewTablesController }

  TdxSimpleViewTablesController = class(TdxTablesController)
  protected
    function CreateTableViewInfoManager(AParentTableViewInfoManager: TdxTableViewInfoManager;
      APageController: TdxPageController; ARowsController: TdxRowsController): TdxTableViewInfoManager; override;
    function GetSimpleView: Boolean; override;
  public
    function CanFitRowToColumn(ALastTextRowBottom: Integer; AColumn: TdxColumn): TdxCanFitCurrentRowToColumnResult; override;
  end;

  { TdxSimpleViewRowsControllerBase }

  TdxSimpleViewRowsControllerBase = class abstract(TdxRowsController)
  public
    function CreateTableGridCalculator(ADocumentModel: TdxDocumentModel; AWidthsCalculator: TdxTableWidthsCalculator; AMaxTableWidth: Integer): TdxTableGridCalculator; override;
    function CreateTablesController: TdxTablesController; override;
    function GetEffectiveLineNumberingRestartTypeBase(ASection: TdxSection): TdxLineNumberingRestart;
    function GetEffectiveLineNumberingRestartType(ASection: TdxSection): TdxLineNumberingRestart; override;
    function CreateFloatingObjectSizeAndPositionController: TdxFloatingObjectSizeAndPositionController; override;
  end;

  { TdxSimpleViewFloatingObjectSizeAndPositionController }

  TdxSimpleViewFloatingObjectSizeAndPositionController = class(TdxFloatingObjectSizeAndPositionController)
  protected
    function ValidateRotatedShapeHorizontalPosition(const AShapeBounds: TRect; AProperties: TdxFloatingObjectProperties): TRect; override;
    function ValidateRotatedShapeVerticalPosition(const AShapeBounds: TRect; AProperties: TdxFloatingObjectProperties): TRect; override;
  end;

  { TdxSimpleViewRowsController }

  TdxSimpleViewRowsController = class(TdxSimpleViewRowsControllerBase)
  strict private
    FController: TdxSimpleViewDocumentFormattingController;
  strict protected
    function CreateCurrentHorizontalPosition: TdxCurrentHorizontalPositionController; overload; override;
    function CreateCurrentHorizontalPosition(APosition: Integer): TdxCurrentHorizontalPositionController; overload; override;
  public
    constructor Create(AController: TdxSimpleViewDocumentFormattingController; APieceTable: TdxPieceTable;
      AColumnController: TdxColumnController; AMatchHorizontalTableIndentsToTextEdge: Boolean);
    property Controller: TdxSimpleViewDocumentFormattingController read FController;
  end;

  { TdxSimpleViewPageViewInfoGenerator }

  TdxSimpleViewPageViewInfoGenerator = class(TdxPageViewInfoGenerator)
  strict private
    function GetView: TdxSimpleView;
  protected
    function GetHorizontalPageGap: Integer; override;
    function GetVerticalPageGap: Integer; override;
    procedure SetHorizontalPageGap(Value: Integer); override;
    procedure SetVerticalPageGap(Value: Integer); override;
    function CreateEmptyClone: TdxPageGeneratorLayoutManager; override;
  public
    procedure Reset(AStrategy: TdxPageGenerationStrategyType); override;
    function ProcessPage(APage: TdxPage; APageIndex: Integer): TdxProcessPageResult; override;
    function CalculatePageLogicalTotalWidth(APage: TdxPage): Integer; override;
    function CanFitPageToPageRow(APage: TdxPage; ARow: TdxPageViewInfoRow): Boolean; override;
    procedure CalculateWidthParameters; override;
    function GetVerticalScrollBarLargeChange: Integer; override;

    property View: TdxSimpleView read GetView;
  end;

  { TdxSimpleViewDocumentFormattingController }

  TdxSimpleViewDocumentFormattingController = class(TdxDocumentFormattingController)
  strict private
    FView: TdxSimpleView;
  public
    constructor Create(AView: TdxSimpleView; ADocumentLayout: TdxDocumentLayout; APieceTable: TdxPieceTable);
    function CreatePageController: TdxPageController; override;
    function CreateColumnController: TdxColumnController; override;
    function CreateRowController: TdxRowsController; override;

    property View: TdxSimpleView read FView;
  end;

  { TdxSimpleViewPainter }

  TdxSimpleViewPainter = class(TdxRichEditViewPainter)
  protected
    procedure DrawEmptyPages(AGraphics: TdxGraphics); override;
    procedure DrawEmptyPage(AGraphics: TdxGraphics; APage: TdxPageViewInfo); override;
    procedure DrawEmptyComment(AGraphics: TdxGraphics; AComment: TdxCommentViewInfo); override;
    procedure DrawEmptyExtensionComment(AGraphics: TdxGraphics; AComment: TdxCommentViewInfo); override;
    function GetPageBounds(APage: TdxPageViewInfo): TRect; override;
    function GetNewClientBounds(const AClipBounds: TRect; const AOldClipBounds: TdxRectF): TdxRectF; override;
  end;

  { TdxSimpleViewBackgroundPainter }

  TdxSimpleViewBackgroundPainter = class(TdxRichEditViewBackgroundPainter)
  strict private
    function GetView: TdxSimpleView;
  protected
    function GetActualPageBackColor: TdxAlphaColor; override;
  public
    procedure Draw(AGraphics: TdxGraphics; const ABounds: TRect); override;

    property View: TdxSimpleView read GetView;
  end;

  { TdxSimpleView }

  TdxSimpleView = class(TdxRichEditView)
  strict private const
    DefaultPadding: TRect = (Left: 15; Top: 4; Right: 4; Bottom: 0);
  strict private
    FHidePartiallyVisibleRow: Boolean;
    FWordWrap: Boolean;
    FInternalWordWrap: Boolean;
    procedure SetInternalWordWrap(Value: Boolean);
    procedure SetWordWrap(Value: Boolean);
  protected
    procedure ControlResizeDelegate;
    function GetActualPadding: TRect; override;
    function CreatePadding: TdxRichEditControlPadding; override;
    function GetDefaultHitTestPageAccuracy: TdxHitTestAccuracy; override;
    function GetMatchHorizontalTableIndentsToTextEdge: Boolean; override;
    function GetShowHorizontalRulerByDefault: Boolean; override;
    function GetShowVerticalRulerByDefault: Boolean; override;
    function GetType: TdxRichEditViewType; override;
    function ShouldUpdatePageWidth: Boolean; virtual;
    procedure OnAutoSizeModeChanged; override;
    procedure UpdatePageWidthCore(AController: TdxDocumentFormattingController); virtual;
    procedure UpdatePageWidth; virtual;

    property InternalWordWrap: Boolean read FInternalWordWrap write SetInternalWordWrap;
    property WordWrap: Boolean read FWordWrap write SetWordWrap default True;
  public
    constructor Create(const AControl: IdxRichEditControl); override;
    procedure Assign(Source: TPersistent); override;

    function CalcBestSize(AFixedWidth: Boolean): TSize; override;
    function CalculatePageContentClipBounds(APage: TdxPageViewInfo): TRect; override;
    function CreateDocumentFormattingController: TdxDocumentFormattingController; override;
    function CreatePageViewInfoGenerator: TdxPageViewInfoGenerator; override;
    function CreatePainter: TdxRichEditViewPainter; override;
    function CreateBackgroundPainter: TdxRichEditViewBackgroundPainter; override;
    procedure EnsureCaretVisibleOnResize; override;
    procedure OnResizeCore; override;
    procedure OnZoomFactorChangedCore; override;
    procedure PerformZoomFactorChanged; override;
    function CreateDocumentLayoutExporter(APainter: TdxPainter; AAdapter: TdxGraphicsDocumentLayoutExporterAdapter;
      APageViewInfo: TdxPageViewInfo; const ABounds: TRect): TdxDocumentLayoutExporter; override;
    procedure Visit(const AVisitor: IdxRichEditViewVisitor); override;
    function GetPageViewInfoRowFromPoint(const APoint: TPoint; AStrictSearch: Boolean): TdxPageViewInfoRow; override;
    function PerformStrictPageViewInfoHitTest(APageViewInfo: TdxPageViewInfo; const APt: TPoint): Boolean; override;
    procedure UpdateHorizontalScrollBar; override;
    procedure OnResize(const ABounds: TRect; AEnsureCaretVisibleOnResize, AEnsureOptimalHorizontalScrollbarPosition: Boolean); override;

    property HidePartiallyVisibleRow: Boolean read FHidePartiallyVisibleRow write FHidePartiallyVisibleRow;
  published
    property AllowDisplayLineNumbers;
    property BackColor;
    property Padding;
  end;

implementation

uses
  Math, dxCore, dxTypeHelpers, dxThreading, cxLookAndFeels, cxLookAndFeelPainters,

  dxRichEdit.Platform.Win.Control,
  dxRichEdit.DocumentModel.Boxes.Simple,
  dxRichEdit.DocumentModel.UnitConverter,
  dxRichEdit.DocumentLayout.UnitConverter;

{ TdxSimpleViewColumnsBoundsCalculator }

procedure TdxSimpleViewColumnsBoundsCalculator.PopulateColumnsBounds(const AResult: TdxRectList; const ABounds: TRect; const AColumnInfoCollection: TdxColumnInfoCollection);
begin
  AResult.Add(ABounds);
end;

procedure TdxSimpleViewColumnsBoundsCalculator.PopulateEqualWidthColumnsBounds(const AResult: TdxRectList;
  const ABounds: TRect; AColumnCount, ASpaceBetweenColumns: Integer);
begin
  AResult.Add(ABounds);
end;

{ TdxSimpleViewFloatingObjectSizeAndPositionController }

function TdxSimpleViewFloatingObjectSizeAndPositionController.ValidateRotatedShapeHorizontalPosition(const AShapeBounds: TRect; AProperties: TdxFloatingObjectProperties): TRect;
var
  APlacementInfo: TdxFloatingObjectTargetPlacementInfo;
begin
  Result := AShapeBounds;
  APlacementInfo := CalculatePlacementInfo(AProperties);
  Result.X := Max(APlacementInfo.OriginX, AShapeBounds.X);
end;

function TdxSimpleViewFloatingObjectSizeAndPositionController.ValidateRotatedShapeVerticalPosition(const AShapeBounds: TRect; AProperties: TdxFloatingObjectProperties): TRect;
var
  APlacementInfo: TdxFloatingObjectTargetPlacementInfo;
begin
  Result := AShapeBounds;
  APlacementInfo := CalculatePlacementInfo(AProperties);
  Result.Y := Max(APlacementInfo.OriginY, AShapeBounds.Y);
end;

{ TdxSimpleViewRowsController }

constructor TdxSimpleViewRowsController.Create(AController: TdxSimpleViewDocumentFormattingController; APieceTable: TdxPieceTable;
  AColumnController: TdxColumnController; AMatchHorizontalTableIndentsToTextEdge: Boolean);
begin
  inherited Create(APieceTable, AColumnController, AMatchHorizontalTableIndentsToTextEdge);
  FController := AController;
end;

function TdxSimpleViewRowsController.CreateCurrentHorizontalPosition: TdxCurrentHorizontalPositionController;
begin
  Result := TdxSimpleViewCurrentHorizontalPositionController.Create(Self);
end;

function TdxSimpleViewRowsController.CreateCurrentHorizontalPosition(APosition: Integer): TdxCurrentHorizontalPositionController;
begin
  Result := TdxSimpleViewCurrentHorizontalPositionController.Create(Self, APosition);
end;

{ TdxSimpleViewPageViewInfoGenerator }

function TdxSimpleViewPageViewInfoGenerator.GetHorizontalPageGap: Integer;
begin
  Result := 0;
end;

function TdxSimpleViewPageViewInfoGenerator.GetVerticalPageGap: Integer;
begin
  Result := 0;
end;

function TdxSimpleViewPageViewInfoGenerator.GetVerticalScrollBarLargeChange: Integer;
begin
  Result := Max(VisibleHeight, ViewPortBounds.Height);
end;

function TdxSimpleViewPageViewInfoGenerator.GetView: TdxSimpleView;
begin
  Result := TdxSimpleView(FView);
end;

procedure TdxSimpleViewPageViewInfoGenerator.SetHorizontalPageGap(Value: Integer);
begin
end;

procedure TdxSimpleViewPageViewInfoGenerator.SetVerticalPageGap(Value: Integer);
begin
end;

function TdxSimpleViewPageViewInfoGenerator.CanFitPageToPageRow(APage: TdxPage; ARow: TdxPageViewInfoRow): Boolean;
begin
  Result := False;
end;

function TdxSimpleViewPageViewInfoGenerator.CreateEmptyClone: TdxPageGeneratorLayoutManager;
begin
  Result := TdxSimpleViewPageViewInfoGenerator.Create(FView, FViewInfo);
end;

procedure TdxSimpleViewPageViewInfoGenerator.Reset(AStrategy: TdxPageGenerationStrategyType);
begin
  TotalWidth := MinInt64;
  VisibleWidth := MinInt64;
  inherited Reset(AStrategy);
end;

procedure TdxSimpleViewPageViewInfoGenerator.CalculateWidthParameters;
var
  AActualTotalWidth: Int64;
begin
  if TotalWidth = MinInt64 then
    TotalWidth := 100;
  if VisibleWidth = MinInt64 then
    VisibleWidth := 100;
  AActualTotalWidth := TotalWidth;
  inherited CalculateWidthParameters;
  TotalWidth := AActualTotalWidth;
end;

function TdxSimpleViewPageViewInfoGenerator.ProcessPage(APage: TdxPage; APageIndex: Integer): TdxProcessPageResult;
begin
  Result := inherited ProcessPage(APage, APageIndex);
  TotalWidth := Max(TotalWidth, CalculatePageLogicalTotalWidth(APage));
end;

function TdxSimpleViewPageViewInfoGenerator.CalculatePageLogicalTotalWidth(APage: TdxPage): Integer;
var
  AMaxWidth: Integer;
  AMaxWidthCalculator: TdxMaxWidthCalculator;
begin
  Result := inherited CalculatePageLogicalTotalWidth(APage);
  if View.InternalWordWrap then
    Exit;
  AMaxWidthCalculator := TdxMaxWidthCalculator.Create;
  try
    AMaxWidth := AMaxWidthCalculator.GetMaxWidth(APage);
  finally
    AMaxWidthCalculator.Free;
  end;
  Result := Max(Result, AMaxWidth);
end;

{ TdxSimpleViewDocumentFormattingController }

constructor TdxSimpleViewDocumentFormattingController.Create(AView: TdxSimpleView;
  ADocumentLayout: TdxDocumentLayout; APieceTable: TdxPieceTable);
begin
  inherited Create(ADocumentLayout, APieceTable, nil, nil);
  FView := AView;
end;

function TdxSimpleViewDocumentFormattingController.CreateColumnController: TdxColumnController;
begin
  Result := TdxSimpleViewColumnController.Create(PageAreaController);
end;

function TdxSimpleViewDocumentFormattingController.CreatePageController: TdxPageController;
begin
  Result := TdxSimpleViewPageController.Create(DocumentLayout);
end;

function TdxSimpleViewDocumentFormattingController.CreateRowController: TdxRowsController;
begin
  Result := TdxSimpleViewRowsController.Create(Self, PieceTable, ColumnController,
    DocumentModel.LayoutOptions.SimpleView.MatchHorizontalTableIndentsToTextEdge);
end;

{ TdxSimpleViewColumnController }

function TdxSimpleViewColumnController.CalculateColumnBoundsCore(AColumnIndex: Integer): TRect;
begin
  Result := ColumnsBounds[0];
end;

function TdxSimpleViewColumnController.CreateColumnBoundsCalculator: TdxColumnsBoundsCalculator;
begin
  Result := TdxSimpleViewColumnsBoundsCalculator.Create(PageAreaController.PageController.DocumentLayout.DocumentModel.ToDocumentLayoutUnitConverter);
end;

function TdxSimpleViewColumnController.GetShouldZeroSpacingBeforeWhenMoveRowToNextColumn: Boolean;
begin
  Result := False;
end;

{ TdxSimpleViewPageController }

constructor TdxSimpleViewPageController.Create(ADocumentLayout: TdxDocumentLayout);
begin
  inherited Create(ADocumentLayout);
  ResetPageSize;
end;

function TdxSimpleViewPageController.CreatePageBoundsCalculator: TdxPageBoundsCalculator;
begin
  Result := TdxSimplePageBoundsCalculator.Create(Self);
end;

function TdxSimpleViewPageController.CreateHitTestCalculator(const ARequest: TdxRichEditHitTestRequest;
  AResult: TdxRichEditHitTestResult): TdxBoxHitTestCalculator;
begin
  Result := TdxSimpleViewBoxHitTestCalculator.Create(ARequest, AResult);
end;

procedure TdxSimpleViewPageController.FinalizePagePrimaryFormatting(APage: TdxPage;
  ASkipAddingFloatingObjects: Boolean);
var
  ALastPageArea: TdxPageArea;
  AColumn: TdxColumn;
  APageBounds, APageAreaBounds, AColumnBounds: TRect;
  AHeight: Integer;
begin
  ALastPageArea := APage.Areas.Last;
  AColumn := ALastPageArea.Columns.First;
  APageBounds := APage.Bounds;
  APageAreaBounds := ALastPageArea.Bounds;
  AColumnBounds := AColumn.Bounds;

  AHeight := GetColumnBottom(AColumn);
  APageBounds.Height := AHeight;
  APageAreaBounds.Height := Max(APageAreaBounds.Height, AHeight);
  AColumnBounds.Height := Max(AColumnBounds.Height, AHeight);

  APage.Bounds := APageBounds;
  APage.ClientBounds := TRect.CreateSize(0, 0, APageBounds.Width, APageBounds.Height);
  ALastPageArea.Bounds := APageAreaBounds;
  AColumn.Bounds := AColumnBounds;
  inherited FinalizePagePrimaryFormatting(APage, ASkipAddingFloatingObjects);
end;

function TdxSimpleViewPageController.GetMaxWidth(AAreas: TdxPageAreaCollection): Integer;
var
  I: Integer;
  ARows: TdxRowCollection;
begin
  Result := MinInt;
  for I := 0 to AAreas.Count - 1 do
  begin
    ARows := AAreas[I].Columns.First.Rows;
    Result := Max(Result, GetMaxWidth(ARows));
  end;
end;

function TdxSimpleViewPageController.GetMaxWidth(ARows: TdxRowCollection): Integer;
var
  I: Integer;
  ALastBox: TdxBox;
begin
  Result := MinInt;
  for I := 0 to ARows.Count - 1 do
  begin
    ALastBox := ARows[I].Boxes.Last;
    if ALastBox <> nil then
      Result := Max(Result, ALastBox.Bounds.Right);
  end;
end;

function TdxSimpleViewPageController.GetVirtualPageHeight: Integer;
begin
  Result := FPageSize.cy;
end;

function TdxSimpleViewPageController.GetVirtualPageWidth: Integer;
begin
  Result := FPageSize.cx;
end;

procedure TdxSimpleViewPageController.SetVirtualPageHeight(const Value: Integer);
begin
  FPageSize.cy := Value;
end;

procedure TdxSimpleViewPageController.SetVirtualPageWidth(const Value: Integer);
begin
  FPageSize.cx := Value;
end;

procedure TdxSimpleViewPageController.ResetPageSize;
begin
  VirtualPageHeight := DocumentLayout.UnitConverter.DocumentsToLayoutUnits(4800);
  VirtualPageWidth := DocumentLayout.UnitConverter.DocumentsToLayoutUnits(900);
  FMinPageWidth := VirtualPageWidth;
end;

{ TdxSimpleViewTableViewInfoManager }

function TdxSimpleViewTableViewInfoManager.GetSplitAnchorHorizontalCellBorders(ASplitAnchor: TdxTableCellVerticalAnchor): TdxHorizontalCellBordersInfoList;
begin
  Result := nil;
end;

procedure TdxSimpleViewTableViewInfoManager.FixColumnOverflow;
begin
end;

{ TdxSimpleViewTablesController }

function TdxSimpleViewTablesController.CreateTableViewInfoManager(AParentTableViewInfoManager: TdxTableViewInfoManager;
  APageController: TdxPageController; ARowsController: TdxRowsController): TdxTableViewInfoManager;
begin
  Result := TdxSimpleViewTableViewInfoManager.Create(AParentTableViewInfoManager, APageController, ARowsController);
end;

function TdxSimpleViewTablesController.GetSimpleView: Boolean;
begin
  Result := True;
end;

function TdxSimpleViewTablesController.CanFitRowToColumn(ALastTextRowBottom: Integer; AColumn: TdxColumn): TdxCanFitCurrentRowToColumnResult;
var
  ARowBounds: TRect;
  AFloatingObjects: TdxFloatingObjectBoxList;
begin
  if RowsController.TablesController.IsInsideTable then
    Exit(TdxCanFitCurrentRowToColumnResult.RowFitted);

  ARowBounds := RowsController.CurrentRow.Bounds;
  ARowBounds.Height := MaxInt - ARowBounds.Y;
  AFloatingObjects := RowsController.FloatingObjectsLayout.GetAllObjectsInRectangle(ARowBounds);
  try
    if AFloatingObjects.Count > 0 then
      Exit(TdxCanFitCurrentRowToColumnResult.RowFitted);
  finally
    AFloatingObjects.Free;
  end;
  Result := inherited CanFitRowToColumn(ALastTextRowBottom, AColumn);
end;

{ TdxSimpleViewRowsControllerBase }

function TdxSimpleViewRowsControllerBase.CreateTablesController: TdxTablesController;
begin
  Result := TdxSimpleViewTablesController.Create(ColumnController.PageAreaController.PageController, Self);
end;

function TdxSimpleViewRowsControllerBase.CreateTableGridCalculator(ADocumentModel: TdxDocumentModel; AWidthsCalculator: TdxTableWidthsCalculator; AMaxTableWidth: Integer): TdxTableGridCalculator;
begin
  Result := TdxTableGridCalculator.Create(AWidthsCalculator, AMaxTableWidth,
    DocumentModel.LayoutOptions.SimpleView.AllowTablesToExtendIntoMargins, True);
end;

function TdxSimpleViewRowsControllerBase.GetEffectiveLineNumberingRestartTypeBase(ASection: TdxSection): TdxLineNumberingRestart;
begin
  Result := inherited GetEffectiveLineNumberingRestartType(ASection);
end;

function TdxSimpleViewRowsControllerBase.GetEffectiveLineNumberingRestartType(ASection: TdxSection): TdxLineNumberingRestart;
begin
  Result := inherited GetEffectiveLineNumberingRestartType(ASection);
  if Result = TdxLineNumberingRestart.NewPage then
    Exit(TdxLineNumberingRestart.NewSection);
end;

function TdxSimpleViewRowsControllerBase.CreateFloatingObjectSizeAndPositionController: TdxFloatingObjectSizeAndPositionController;
begin
  Result := TdxSimpleViewFloatingObjectSizeAndPositionController.Create(Self);
end;

{ TdxSimplePageBoundsCalculator }

constructor TdxSimplePageBoundsCalculator.Create(AController: TdxSimpleViewPageController);
begin
  inherited Create(AController.DocumentLayout.DocumentModel.ToDocumentLayoutUnitConverter);
  FController := AController;
end;

function TdxSimplePageBoundsCalculator.CalculatePageBounds(ASection: TdxSection): TRect;
begin
  Result := CalculatePageClientBounds(ASection);
end;

function TdxSimplePageBoundsCalculator.CalculatePageClientBoundsCore(APageWidth, APageHeight, AMarginLeft, AMarginTop,
  AMarginRight, AMarginBottom: Integer): TRect;
begin
  Result.InitSize(cxNullPoint, FController.PageSize.cx, FController.PageSize.cy);
end;

{ TdxSimpleViewCurrentHorizontalPositionController }

function TdxSimpleViewCurrentHorizontalPositionController.CanFitBoxToCurrentRow(const ABoxSize: TSize): Boolean;
begin
  if TdxSimpleViewRowsController(RowsController).Controller.View.InternalWordWrap then
    Result := inherited CanFitBoxToCurrentRow(ABoxSize)
  else
    Result := True;
end;


function TdxSimpleViewCurrentHorizontalPositionController.GetMaxBoxWidth: Integer;
begin
  if (TdxSimpleViewRowsController(RowsController)).Controller.View.InternalWordWrap then
    Result := inherited GetMaxBoxWidth
  else
    Result := MaxInt div 2;
end;

{ TdxSimpleViewBoxHitTestCalculator }

procedure TdxSimpleViewBoxHitTestCalculator.ProcessColumnCollection(ACollection: TdxColumnCollection);
var
  AStrictHitTest: Boolean;
begin
  AStrictHitTest := (HitTestRequest.Accuracy and ExactColumn) <> 0;
  FastHitTestAssumingArrangedVertically(ACollection, AStrictHitTest);
end;

{ TdxSimpleViewPainter }

procedure TdxSimpleViewPainter.DrawEmptyComment(AGraphics: TdxGraphics; AComment: TdxCommentViewInfo);
begin
end;

procedure TdxSimpleViewPainter.DrawEmptyExtensionComment(AGraphics: TdxGraphics;
  AComment: TdxCommentViewInfo);
begin
end;

procedure TdxSimpleViewPainter.DrawEmptyPage(AGraphics: TdxGraphics; APage: TdxPageViewInfo);
begin
end;

procedure TdxSimpleViewPainter.DrawEmptyPages(AGraphics: TdxGraphics);
begin
end;

function TdxSimpleViewPainter.GetNewClientBounds(const AClipBounds: TRect; const AOldClipBounds: TdxRectF): TdxRectF;
begin
  Result := AOldClipBounds;
end;

function TdxSimpleViewPainter.GetPageBounds(APage: TdxPageViewInfo): TRect;
begin
  Result := APage.Page.Bounds;
  Result.Width := MaxInt div 2;
end;

{ TdxSimpleViewBackgroundPainter }

function TdxSimpleViewBackgroundPainter.GetView: TdxSimpleView;
begin
  Result := TdxSimpleView(inherited View);
end;

procedure TdxSimpleViewBackgroundPainter.Draw(AGraphics: TdxGraphics; const ABounds: TRect);
begin
  AGraphics.FillRectangle(ABounds, GetActualPageBackColor);
end;

function TdxSimpleViewBackgroundPainter.GetActualPageBackColor: TdxAlphaColor;
var
  APageBackColor: TdxAlphaColor;
begin
  APageBackColor := View.DocumentModel.DocumentProperties.PageBackColor;
  if TdxAlphaColors.IsTransparentOrEmpty(APageBackColor) then
    Result := View.ActualBackColor
  else
    Result := APageBackColor;
end;

{ TdxSimpleView }

constructor TdxSimpleView.Create(const AControl: IdxRichEditControl);
begin
  inherited Create(AControl);
  FWordWrap := True;
  FInternalWordWrap := True;
end;

procedure TdxSimpleView.Assign(Source: TPersistent);
begin
  if Source is TdxSimpleView then
  begin
    Padding := TdxSimpleView(Source).Padding;
    WordWrap := TdxSimpleView(Source).WordWrap;
  end;
end;

function TdxSimpleView.CalcBestSize(AFixedWidth: Boolean): TSize;
var
  AOldInternalWordWrap: Boolean;
  AOldTopInvisibleHeight: Int64;
  AMaxWidthCalculator: TdxMaxWidthCalculator;
  I, ACount, AWidth, AHeight: Integer;
begin
  Control.BeginUpdate;
  try
    AOldInternalWordWrap := InternalWordWrap;
    AOldTopInvisibleHeight := PageViewInfoGenerator.TopInvisibleHeight;
    if not AFixedWidth then
      InternalWordWrap := False;
    BeginDocumentRendering;

    AMaxWidthCalculator := TdxMaxWidthCalculator.Create;
    try
      AWidth := AMaxWidthCalculator.GetMaxWidth(DocumentLayout.Pages[0]);
      AHeight := DocumentLayout.Pages[0].Bounds.Height;
      ACount := DocumentLayout.Pages.Count;
      for I := 1 to ACount - 1 do
      begin
        AWidth := Max(AMaxWidthCalculator.GetMaxWidth(DocumentLayout.Pages[I]), AWidth);
        Inc(AHeight, DocumentLayout.Pages[I].Bounds.Height);
      end;

      EndDocumentRendering;
      PageViewInfoGenerator.Reset(TdxPageGenerationStrategyType.RunningHeight);
      PageViewInfoGenerator.TopInvisibleHeight := AOldTopInvisibleHeight;
      InternalWordWrap := AOldInternalWordWrap;
    finally
      AMaxWidthCalculator.Free;
    end;
  finally
    Control.EndUpdate;
  end;
  Result := DocumentLayout.UnitConverter.LayoutUnitsToPixels(TSize.Create(AWidth, AHeight),
    DocumentModel.DpiX, DocumentModel.DpiY);
end;

function TdxSimpleView.CalculatePageContentClipBounds(APage: TdxPageViewInfo): TRect;
begin
  Result := inherited CalculatePageContentClipBounds(APage);
  Result.Left := Min(Result.Left, 0);
  Result.Width := MaxInt div 2;
  Result.Height := MaxInt div 2;
end;

function TdxSimpleView.CreateDocumentFormattingController: TdxDocumentFormattingController;
begin
  Result := TdxSimpleViewDocumentFormattingController.Create(Self, DocumentLayout, DocumentModel.MainPieceTable);
  UpdatePageWidthCore(Result);
end;

function TdxSimpleView.CreatePageViewInfoGenerator: TdxPageViewInfoGenerator;
begin
  Result := TdxSimpleViewPageViewInfoGenerator.Create(Self, ViewInfo);
end;

function TdxSimpleView.CreatePainter: TdxRichEditViewPainter;
begin
  Result := TdxSimpleViewPainter.Create(Self);
end;

function TdxSimpleView.CreateBackgroundPainter: TdxRichEditViewBackgroundPainter;
begin
  Result := TdxSimpleViewBackgroundPainter.Create(Self);
end;

function TdxSimpleView.GetActualPadding: TRect;
var
  AHorizontalRuler: IdxRulerControl;
  AOffset: Integer;
begin
  Result := Padding.Value;
  AHorizontalRuler := Control.InnerControl.HorizontalRuler;
  if (AHorizontalRuler <> nil) and AHorizontalRuler.IsVisible then
  begin
    AOffset := AHorizontalRuler.GetRulerSizeInPixels;
    if (Control.InnerControl.VerticalRuler <> nil) and Control.InnerControl.VerticalRuler.IsVisible then
      Result.Left := Max(Result.Left - AOffset, AOffset div 3)
    else
      Result.Left := Max(Result.Left, 4 * AOffset div 3);
  end;
  Result := Control.ScaleFactor.Apply(Result);
end;


function TdxSimpleView.GetPageViewInfoRowFromPoint(const APoint: TPoint; AStrictSearch: Boolean): TdxPageViewInfoRow;
begin
  Result := inherited GetPageViewInfoRowFromPoint(APoint, False);
end;

function TdxSimpleView.GetShowHorizontalRulerByDefault: Boolean;
begin
  Result := False;
end;

function TdxSimpleView.GetShowVerticalRulerByDefault: Boolean;
begin
  Result := False;
end;

function TdxSimpleView.GetType: TdxRichEditViewType;
begin
  Result := TdxRichEditViewType.Simple;
end;

procedure TdxSimpleView.OnResize(const ABounds: TRect; AEnsureCaretVisibleOnResize, AEnsureOptimalHorizontalScrollbarPosition: Boolean);
begin
  inherited OnResize(ABounds, AEnsureCaretVisibleOnResize, AEnsureOptimalHorizontalScrollbarPosition);
  if AEnsureCaretVisibleOnResize then
    EnsureCaretVisible;
end;

procedure TdxSimpleView.OnResizeCore;
begin
  inherited OnResizeCore;
  if ShouldUpdatePageWidth then
    UpdatePageWidth;
end;

procedure TdxSimpleView.OnZoomFactorChangedCore;
begin
  inherited OnZoomFactorChangedCore;
  UpdatePageWidth;
end;

function TdxSimpleView.PerformStrictPageViewInfoHitTest(APageViewInfo: TdxPageViewInfo; const APt: TPoint): Boolean;
var
  ABounds: TRect;
begin
  ABounds := APageViewInfo.ClientBounds;
  Result := (ABounds.Left <= APt.X) and (APt.X <= ABounds.Right);
end;

procedure TdxSimpleView.PerformZoomFactorChanged;
begin
  inherited PerformZoomFactorChanged;
  EnsureCaretVisible;
end;

function TdxSimpleView.CreateDocumentLayoutExporter(APainter: TdxPainter; AAdapter: TdxGraphicsDocumentLayoutExporterAdapter;
  APageViewInfo: TdxPageViewInfo; const ABounds: TRect): TdxDocumentLayoutExporter;
var
  AExporter: TdxScreenOptimizedGraphicsDocumentLayoutExporter;
begin
  if AllowDisplayLineNumbers then
    AExporter := TdxScreenOptimizedGraphicsDocumentLayoutExporter.Create(DocumentModel, APainter, AAdapter, ABounds, TextColors, ScaleFactor)
  else
    AExporter := TdxScreenOptimizedGraphicsDocumentLayoutExporterNoLineNumbers.Create(DocumentModel, APainter, AAdapter, ABounds, TextColors, ScaleFactor);
  AExporter.VisibleBounds := CalculateVisiblePageBounds(ABounds, APageViewInfo);
  AExporter.HidePartiallyVisibleRow := HidePartiallyVisibleRow;
  Result := AExporter;
end;

function TdxSimpleView.GetDefaultHitTestPageAccuracy: TdxHitTestAccuracy;
begin
  Result := NearestPage;
end;

function TdxSimpleView.CreatePadding: TdxRichEditControlPadding;
begin
  Result := TdxRichEditControlPadding.Create(Self, DefaultPadding);
end;

function TdxSimpleView.GetMatchHorizontalTableIndentsToTextEdge: Boolean;
begin
  Result := Control.InnerControl.Options.Layout.SimpleView.MatchHorizontalTableIndentsToTextEdge;
end;

procedure TdxSimpleView.EnsureCaretVisibleOnResize;
begin
  if (Bounds.Height > 0) and (Bounds.Width > 0) then
    EnsureCaretVisible;
end;

function TdxSimpleView.ShouldUpdatePageWidth: Boolean;
var
  APageController: TdxSimpleViewPageController;
begin
  APageController := FormattingController.PageController as TdxSimpleViewPageController;
  Result := APageController.VirtualPageWidth <> Math.Max(DocumentLayout.UnitConverter.DocumentsToLayoutUnits(4), Trunc(Bounds.Width / ScaleFactor));
end;

procedure TdxSimpleView.OnAutoSizeModeChanged;
const
  ChangeActions = [
    TdxDocumentModelChangeAction.ResetAllPrimaryLayout,
    TdxDocumentModelChangeAction.ResetSelectionLayout,
    TdxDocumentModelChangeAction.ResetSecondaryLayout,
    TdxDocumentModelChangeAction.ForceResetHorizontalRuler,
    TdxDocumentModelChangeAction.ForceResetVerticalRuler];
var
  APieceTable: TdxPieceTable;
begin
  DocumentModel.BeginUpdate;
  try
    FormattingController.Reset(False);

    APieceTable := DocumentModel.ActivePieceTable;
    APieceTable.ApplyChangesCore(ChangeActions, 0, MaxInt);
  finally
    DocumentModel.EndUpdate;
  end;
end;

procedure TdxSimpleView.UpdateHorizontalScrollBar;
var
  APrevVisibility: Boolean;
begin
  APrevVisibility := HorizontalScrollController.IsScrollPossible;
  inherited UpdateHorizontalScrollBar;
  if APrevVisibility <> HorizontalScrollController.IsScrollPossible then
    TdxUIThreadSyncService.EnqueueInvokeInUIThread(Self, ControlResizeDelegate);
end;

procedure TdxSimpleView.ControlResizeDelegate;
begin
  Control.OnResizeCore;
end;

procedure TdxSimpleView.UpdatePageWidth;
const
  ChangeActions = [
    TdxDocumentModelChangeAction.ResetAllPrimaryLayout,
    TdxDocumentModelChangeAction.ResetSelectionLayout,
    TdxDocumentModelChangeAction.ResetSecondaryLayout,
    TdxDocumentModelChangeAction.ForceResetHorizontalRuler,
    TdxDocumentModelChangeAction.ForceResetVerticalRuler];
var
  APieceTable: TdxPieceTable;
begin
  DocumentModel.BeginUpdate;
  try
    UpdatePageWidthCore(FormattingController);
    APieceTable := DocumentModel.ActivePieceTable;
    APieceTable.ApplyChangesCore(ChangeActions, 0, MaxInt);
  finally
    DocumentModel.EndUpdate;
  end;
end;

procedure TdxSimpleView.UpdatePageWidthCore(AController: TdxDocumentFormattingController);
var
  APageController: TdxSimpleViewPageController;
begin
  APageController := AController.PageController as TdxSimpleViewPageController;
  APageController.ResetPageSize;
  APageController.VirtualPageWidth := Math.Max(DocumentLayout.UnitConverter.DocumentsToLayoutUnits(4), Trunc(Bounds.Width / ScaleFactor));
  AController.Reset(False);
end;

procedure TdxSimpleView.Visit(const AVisitor: IdxRichEditViewVisitor);
begin
  AVisitor.Visit(Self);
end;

procedure TdxSimpleView.SetInternalWordWrap(Value: Boolean);
const
  ChangeActions =
    [TdxDocumentModelChangeAction.ResetAllPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSelectionLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ForceResetHorizontalRuler,
     TdxDocumentModelChangeAction.ForceResetVerticalRuler,
     TdxDocumentModelChangeAction.Redraw];
begin
  DocumentModel.BeginUpdate;
  try
    FInternalWordWrap := Value;
    DocumentModel.ActivePieceTable.ApplyChangesCore(ChangeActions, 0, MaxInt);
  finally
    DocumentModel.EndUpdate;
  end;
end;

procedure TdxSimpleView.SetWordWrap(Value: Boolean);
begin
  FWordWrap := Value;
  InternalWordWrap := Value;
end;

end.
