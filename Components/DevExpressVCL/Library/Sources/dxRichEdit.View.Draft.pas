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

unit dxRichEdit.View.Draft;

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
  dxRichEdit.Utils.PredefinedFontSizeCollection,
  dxRichEdit.Utils.Graphics,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.Section,
  dxRichEdit.DocumentModel.SectionFormatting,
  dxRichEdit.DocumentModel.Selection,
  dxRichEdit.DocumentModel.TableCalculator,
  dxRichEdit.DocumentModel.Boxes,
  dxRichEdit.DocumentLayout,
  dxRichEdit.DocumentLayout.Position,
  dxRichEdit.LayoutEngine.Formatter,
  dxRichEdit.LayoutEngine.DocumentFormatter,
  dxRichEdit.Control.HitTest,
  dxRichEdit.Control.HotZones,
  dxRichEdit.View.Core,
  dxRichEdit.View.ViewInfo,
  dxRichEdit.View.PageViewInfoGenerator,
  dxRichEdit.View.Simple,
  dxRichEdit.Platform.Win.Painter, cxGeometry;

type
  TdxDraftView = class;

  { TdxDraftViewColumnsBoundsCalculator }

  TdxDraftViewColumnsBoundsCalculator = class(TdxColumnsBoundsCalculator)
  public
    procedure PopulateColumnsBounds(const AResult: TdxRectList; const ABounds: TRect;
      const AColumnInfoCollection: TdxColumnInfoCollection); override;
    procedure PopulateEqualWidthColumnsBoundsCore(const AResult: TdxRectList;
      const AColumnRects: TArray<TRect>; ASpaceBetweenColumns: Integer); override;
  end;

  { TdxDraftViewColumnController }

  TdxDraftViewColumnController = class(TdxColumnController)
  protected
    function GetShouldZeroSpacingBeforeWhenMoveRowToNextColumn: Boolean; override;
  public
    function CalculateColumnBoundsCore(AColumnIndex: Integer): TRect; override;
    function CreateColumnBoundsCalculator: TdxColumnsBoundsCalculator; override;
  end;

  { TdxDraftPageBoundsCalculator }

  TdxDraftPageBoundsCalculator = class(TdxPageBoundsCalculator)
  protected
    function CalculatePageBounds(ASection: TdxSection): TRect; override;
  public
    function CalculatePageClientBoundsCore(APageWidth, APageHeight, AMarginLeft, AMarginTop, AMarginRight, AMarginBottom: Integer): TRect; override;
  end;

  { TdxDraftViewPageController }

  TdxDraftViewPageController = class(TdxNonPrintViewPageControllerBase)
  public
    procedure FinalizePagePrimaryFormatting(APage: TdxPage; ADocumentEnded: Boolean); override;
    function CreatePageBoundsCalculator: TdxPageBoundsCalculator; override;
    function CreateHitTestCalculator(const ARequest: TdxRichEditHitTestRequest;
      AResult: TdxRichEditHitTestResult): TdxBoxHitTestCalculator; override;
  end;

  { TdxDraftViewRowsController }

  TdxDraftViewRowsController = class(TdxSimpleViewRowsControllerBase)
  public
    function CreateTableGridCalculator(ADocumentModel: TdxDocumentModel; AWidthsCalculator: TdxTableWidthsCalculator;
      AMaxTableWidth: Integer): TdxTableGridCalculator; override;
    function GetEffectiveLineNumberingRestartType(ASection: TdxSection): TdxLineNumberingRestart; override;
  end;

  { TdxDraftViewBoxHitTestCalculator }

  TdxDraftViewBoxHitTestCalculator = class(TdxBoxHitTestCalculator)
  public
    procedure ProcessColumnCollection(ACollection: TdxColumnCollection); override;
  end;

  { TdxDraftViewDocumentFormattingController }

  TdxDraftViewDocumentFormattingController = class(TdxDocumentFormattingController)
  protected
    function CreatePageController: TdxPageController; override;
    function CreateColumnController: TdxColumnController; override;
    function CreateRowController: TdxRowsController; override;
  public
    constructor Create(ADocumentLayout: TdxDocumentLayout; APieceTable: TdxPieceTable);
  end;

  { TdxDraftViewPageViewInfoGenerator }

  TdxDraftViewPageViewInfoGenerator = class(TdxPageViewInfoGenerator)
  protected
    function GetHorizontalPageGap: Integer; override;
    procedure SetHorizontalPageGap(AValue: Integer); override;
    function GetVerticalPageGap: Integer; override;
    procedure SetVerticalPageGap(AValue: Integer); override;
    function CreateEmptyClone: TdxPageGeneratorLayoutManager; override;
    function CreateInitialPageViewInfoRowBounds(Y: Integer): TRect; override;
    function CalculateFirstPageLeftOffset(ATotalPagesWidth: Integer): Integer; override;
  public
    function CanFitPageToPageRow(APage: TdxPage; ARow: TdxPageViewInfoRow): Boolean; override;
  end;

  { TdxDraftViewPainter }

  TdxDraftViewPainter = class(TdxRichEditViewPainter)
  protected
    procedure DrawEmptyPages(AGraphics: TdxGraphics); override;
    procedure DrawEmptyPage(AGraphics: TdxGraphics; APage: TdxPageViewInfo); override;
    procedure DrawEmptyComment(AGraphics: TdxGraphics; AComment: TdxCommentViewInfo); override;
    procedure DrawEmptyExtensionComment(AGraphics: TdxGraphics; AComment: TdxCommentViewInfo); override;
  end;

  { TdxDraftViewBackgroundPainter }

  TdxDraftViewBackgroundPainter = class(TdxRichEditViewBackgroundPainter)
  strict private
    function GetView: TdxDraftView;
  protected
    function GetActualPageBackColor: TdxAlphaColor; override;
  public
    procedure Draw(AGraphics: TdxGraphics; const ABounds: TRect); override;

    property View: TdxDraftView read GetView;
  end;

  { TdxDraftView }

  TdxDraftView = class(TdxPageBasedRichEditView)
  strict private const
    DefaultPadding: TRect = (Left: 15; Top: 4; Right: 0; Bottom: 0);
  protected
    function CreatePadding: TdxRichEditControlPadding; override;
    function GetType: TdxRichEditViewType; override;
    function GetShowHorizontalRulerByDefault: Boolean; override;
    function GetShowVerticalRulerByDefault: Boolean; override;
    function GetMatchHorizontalTableIndentsToTextEdge: Boolean; override;
    function GetActualPadding: TRect; override;
    function GetDefaultHitTestPageAccuracy: TdxHitTestAccuracy; override;
    function CreateDocumentFormattingController: TdxDocumentFormattingController; override;
    function CreatePageViewInfoGenerator: TdxPageViewInfoGenerator; override;
    function GetPageViewInfoRowFromPoint(const APoint: TPoint; AStrictSearch: Boolean): TdxPageViewInfoRow; override;
    function PerformStrictPageViewInfoHitTest(APageViewInfo: TdxPageViewInfo; const APt: TPoint): Boolean; override;
  public
    function CalculatePageContentClipBounds(APage: TdxPageViewInfo): TRect; override;
    function CreatePainter: TdxRichEditViewPainter; override;
    function CreateBackgroundPainter: TdxRichEditViewBackgroundPainter; override;
    function CreateDocumentLayoutExporter(APainter: TdxPainter; AAdapter: TdxGraphicsDocumentLayoutExporterAdapter;
      APageViewInfo: TdxPageViewInfo; const ABounds: TRect): TdxDocumentLayoutExporter; override;
    procedure Visit(const AVisitor: IdxRichEditViewVisitor); override;
  published
    property AllowDisplayLineNumbers;
    property BackColor;
    property Padding;
  end;

implementation

uses
  Math, dxCore, dxTypeHelpers, cxLookAndFeels, cxLookAndFeelPainters,

  dxRichEdit.DocumentModel.DocumentProperties,
  dxRichEdit.Platform.Win.Control;

{ TdxDraftViewColumnsBoundsCalculator }

procedure TdxDraftViewColumnsBoundsCalculator.PopulateColumnsBounds(const AResult: TdxRectList;
  const ABounds: TRect; const AColumnInfoCollection: TdxColumnInfoCollection);
var
  AColumnBounds: TRect;
begin
  AColumnBounds := ABounds;
  AColumnBounds.Width := UnitConverter.ToLayoutUnits(AColumnInfoCollection[0].Width);
  AResult.Add(AColumnBounds);
end;

procedure TdxDraftViewColumnsBoundsCalculator.PopulateEqualWidthColumnsBoundsCore(const AResult: TdxRectList;
  const AColumnRects: TArray<TRect>; ASpaceBetweenColumns: Integer);
begin
  AResult.Add(AColumnRects[0]);
end;

{ TdxDraftViewColumnController }

function TdxDraftViewColumnController.GetShouldZeroSpacingBeforeWhenMoveRowToNextColumn: Boolean;
begin
  Result := False;
end;

function TdxDraftViewColumnController.CalculateColumnBoundsCore(AColumnIndex: Integer): TRect;
begin
  Result := ColumnsBounds[0];
end;

function TdxDraftViewColumnController.CreateColumnBoundsCalculator: TdxColumnsBoundsCalculator;
begin
  Result := TdxDraftViewColumnsBoundsCalculator.Create(PageAreaController.PageController.DocumentLayout.DocumentModel.ToDocumentLayoutUnitConverter);
end;

{ TdxDraftPageBoundsCalculator }

function TdxDraftPageBoundsCalculator.CalculatePageBounds(ASection: TdxSection): TRect;
begin
  Result := CalculatePageClientBounds(ASection);
end;

function TdxDraftPageBoundsCalculator.CalculatePageClientBoundsCore(APageWidth: Integer; APageHeight: Integer;
  AMarginLeft: Integer; AMarginTop: Integer; AMarginRight: Integer; AMarginBottom: Integer): TRect;
var
  AWidth, AHeight, ALeft, ATop, ARight, ABottom: Integer;
begin
  AWidth := UnitConverter.ToLayoutUnits(APageWidth);
  AHeight := UnitConverter.ToLayoutUnits(APageHeight);
  ALeft := UnitConverter.ToLayoutUnits(AMarginLeft);
  ATop := UnitConverter.ToLayoutUnits(AMarginTop);
  ARight := UnitConverter.ToLayoutUnits(AMarginRight);
  ABottom := UnitConverter.ToLayoutUnits(AMarginBottom);

  Result.Init(0, 0, AWidth - ARight - ALeft, AHeight - ABottom - ATop);
end;

{ TdxDraftViewPageController }

procedure TdxDraftViewPageController.FinalizePagePrimaryFormatting(APage: TdxPage; ADocumentEnded: Boolean);
var
  ALastPageArea: TdxPageArea;
  AColumns: TdxColumnCollection;
  ACount, AHeight, I: Integer;
  ANewPageBounds, ANewPageClientBounds, ANewAreaBounds: TRect;
begin
  ALastPageArea := APage.Areas.Last;
  AColumns := ALastPageArea.Columns;
  ACount := AColumns.Count;
  AHeight := 0;
  for I := 0 to ACount - 1 do
    AHeight := Max(AHeight, GetColumnBottom(AColumns[I]));

  ANewPageBounds := APage.Bounds;
  ANewPageBounds.Height := AHeight;
  APage.Bounds := ANewPageBounds;
  ANewPageClientBounds := APage.ClientBounds;
  ANewPageClientBounds.Height := AHeight;
  APage.ClientBounds := ANewPageClientBounds;
  ANewAreaBounds := ALastPageArea.Bounds;
  ANewAreaBounds.Height := Max(ANewAreaBounds.Height, AHeight);
  ALastPageArea.Bounds := ANewAreaBounds;

  inherited FinalizePagePrimaryFormatting(APage, ADocumentEnded);
end;

function TdxDraftViewPageController.CreatePageBoundsCalculator: TdxPageBoundsCalculator;
begin
  Result := TdxDraftPageBoundsCalculator.Create(DocumentLayout.DocumentModel.ToDocumentLayoutUnitConverter);
end;

function TdxDraftViewPageController.CreateHitTestCalculator(const ARequest: TdxRichEditHitTestRequest;
  AResult: TdxRichEditHitTestResult): TdxBoxHitTestCalculator;
begin
  Result := TdxDraftViewBoxHitTestCalculator.Create(ARequest, AResult);
end;

{ TdxDraftViewRowsController }

function TdxDraftViewRowsController.GetEffectiveLineNumberingRestartType(ASection: TdxSection): TdxLineNumberingRestart;
begin
  Result := GetEffectiveLineNumberingRestartTypeBase(ASection);
end;

function TdxDraftViewRowsController.CreateTableGridCalculator(ADocumentModel: TdxDocumentModel;
  AWidthsCalculator: TdxTableWidthsCalculator; AMaxTableWidth: Integer): TdxTableGridCalculator;
begin
  Result := TdxTableGridCalculator.Create(AWidthsCalculator, AMaxTableWidth, DocumentModel.LayoutOptions.DraftView.AllowTablesToExtendIntoMargins, False);
end;

{ TdxDraftViewBoxHitTestCalculator }

procedure TdxDraftViewBoxHitTestCalculator.ProcessColumnCollection(ACollection: TdxColumnCollection);
var
  AStrictHitTest: Boolean;
begin
  AStrictHitTest := (HitTestRequest.Accuracy and ExactColumn) <> 0;
  FastHitTestAssumingArrangedVertically(ACollection, AStrictHitTest);
end;

{ TdxDraftViewDocumentFormattingController }

constructor TdxDraftViewDocumentFormattingController.Create(ADocumentLayout: TdxDocumentLayout; APieceTable: TdxPieceTable);
begin
  inherited Create(ADocumentLayout, APieceTable, nil, nil);
end;

function TdxDraftViewDocumentFormattingController.CreatePageController: TdxPageController;
begin
  Result := TdxDraftViewPageController.Create(DocumentLayout);
end;

function TdxDraftViewDocumentFormattingController.CreateColumnController: TdxColumnController;
begin
  Result := TdxDraftViewColumnController.Create(PageAreaController);
end;

function TdxDraftViewDocumentFormattingController.CreateRowController: TdxRowsController;
begin
  Result := TdxDraftViewRowsController.Create(PieceTable, ColumnController, DocumentModel.LayoutOptions.DraftView.MatchHorizontalTableIndentsToTextEdge);
end;

{ TdxDraftViewPageViewInfoGenerator }

function TdxDraftViewPageViewInfoGenerator.GetHorizontalPageGap: Integer;
begin
  Result := 0;
end;

procedure TdxDraftViewPageViewInfoGenerator.SetHorizontalPageGap(AValue: Integer);
begin
end;

function TdxDraftViewPageViewInfoGenerator.GetVerticalPageGap: Integer;
begin
  Result := 0;
end;

procedure TdxDraftViewPageViewInfoGenerator.SetVerticalPageGap(AValue: Integer);
begin
end;

function TdxDraftViewPageViewInfoGenerator.CanFitPageToPageRow(APage: TdxPage; ARow: TdxPageViewInfoRow): Boolean;
begin
  Result := False;
end;

function TdxDraftViewPageViewInfoGenerator.CreateEmptyClone: TdxPageGeneratorLayoutManager;
begin
  Result := TdxDraftViewPageViewInfoGenerator.Create(FView, FViewInfo);
end;

function TdxDraftViewPageViewInfoGenerator.CreateInitialPageViewInfoRowBounds(Y: Integer): TRect;
begin
  Result.Init(0 {ViewPortBounds.Left}, Y, 0, 0);
end;

function TdxDraftViewPageViewInfoGenerator.CalculateFirstPageLeftOffset(ATotalPagesWidth: Integer): Integer;
begin
  Result := 0; {ViewPortBounds.Left}
end;

{ TdxDraftViewPainter }

procedure TdxDraftViewPainter.DrawEmptyComment(AGraphics: TdxGraphics;
  AComment: TdxCommentViewInfo);
begin
end;

procedure TdxDraftViewPainter.DrawEmptyExtensionComment(AGraphics: TdxGraphics;
  AComment: TdxCommentViewInfo);
begin
end;

procedure TdxDraftViewPainter.DrawEmptyPage(AGraphics: TdxGraphics;
  APage: TdxPageViewInfo);
begin
end;

procedure TdxDraftViewPainter.DrawEmptyPages(AGraphics: TdxGraphics);
begin
end;

{ TdxDraftViewBackgroundPainter }

procedure TdxDraftViewBackgroundPainter.Draw(AGraphics: TdxGraphics;
  const ABounds: TRect);
begin
  AGraphics.FillRectangle(ABounds, GetActualPageBackColor);
end;

function TdxDraftViewBackgroundPainter.GetActualPageBackColor: TdxAlphaColor;
var
  APageBackColor: TdxAlphaColor;
  ADocumentProperties: TdxDocumentProperties;
begin
  ADocumentProperties := View.DocumentModel.DocumentProperties;
  APageBackColor := ADocumentProperties.PageBackColor;
  if ADocumentProperties.DisplayBackgroundShape and not TdxAlphaColors.IsTransparentOrEmpty(APageBackColor) then
    Result := APageBackColor
  else
    Result := View.ActualBackColor;
end;

function TdxDraftViewBackgroundPainter.GetView: TdxDraftView;
begin
  Result := TdxDraftView(inherited View);
end;

{ TdxDraftView }

function TdxDraftView.CalculatePageContentClipBounds(APage: TdxPageViewInfo): TRect;
begin
  Result := inherited CalculatePageContentClipBounds(APage);
  Result.X := Min(Result.X, 0);
  Result.Width := MaxInt div 2;
  Result.Height := MaxInt div 2;
end;

function TdxDraftView.CreatePadding: TdxRichEditControlPadding;
begin
  Result := TdxRichEditControlPadding.Create(Self, DefaultPadding);
end;

function TdxDraftView.GetType: TdxRichEditViewType;
begin
  Result := TdxRichEditViewType.Draft;
end;

function TdxDraftView.GetShowHorizontalRulerByDefault: Boolean;
begin
  Result := True;
end;

function TdxDraftView.GetShowVerticalRulerByDefault: Boolean;
begin
  Result := False;
end;

function TdxDraftView.GetMatchHorizontalTableIndentsToTextEdge: Boolean;
begin
  Result := Control.InnerControl.Options.Layout.DraftView.MatchHorizontalTableIndentsToTextEdge;
end;

function TdxDraftView.GetActualPadding: TRect;
var
  AHorizontalRuler: IdxRulerControl;
  AOffset: Integer;
begin
  Result := inherited GetActualPadding;
  AHorizontalRuler := Control.InnerControl.HorizontalRuler;
  if (AHorizontalRuler <> nil) and AHorizontalRuler.IsVisible then
  begin
    AOffset := AHorizontalRuler.GetRulerSizeInPixels;
    if (Control.InnerControl.VerticalRuler <> nil) and Control.InnerControl.VerticalRuler.IsVisible then
      Result.Left := Max(Result.Left - AOffset, AOffset div 3)
    else
      Result.Left := Max(Result.Left, 4 * AOffset div 3);
  end;
end;

function TdxDraftView.GetDefaultHitTestPageAccuracy: TdxHitTestAccuracy;
begin
  Result := NearestPage;
end;

function TdxDraftView.CreateBackgroundPainter: TdxRichEditViewBackgroundPainter;
begin
  Result := TdxDraftViewBackgroundPainter.Create(Self);
end;

function TdxDraftView.CreateDocumentFormattingController: TdxDocumentFormattingController;
begin
  Result := TdxDraftViewDocumentFormattingController.Create(DocumentLayout, DocumentModel.MainPieceTable);
end;

function TdxDraftView.CreatePageViewInfoGenerator: TdxPageViewInfoGenerator;
begin
  Result := TdxDraftViewPageViewInfoGenerator.Create(Self, ViewInfo);
end;

function TdxDraftView.CreatePainter: TdxRichEditViewPainter;
begin
  Result := TdxDraftViewPainter.Create(Self);
end;

function TdxDraftView.CreateDocumentLayoutExporter(APainter: TdxPainter; AAdapter: TdxGraphicsDocumentLayoutExporterAdapter;
  APageViewInfo: TdxPageViewInfo; const ABounds: TRect): TdxDocumentLayoutExporter;
var
  AExporter: TdxScreenOptimizedGraphicsDocumentLayoutExporter;
begin
  if AllowDisplayLineNumbers then
    AExporter := TdxScreenOptimizedGraphicsDocumentLayoutExporter.Create(DocumentModel, APainter, AAdapter, ABounds, TextColors, ScaleFactor)
  else
    AExporter := TdxScreenOptimizedGraphicsDocumentLayoutExporterNoLineNumbers.Create(DocumentModel, APainter, AAdapter, ABounds, TextColors, ScaleFactor);

  AExporter.VisibleBounds := CalculateVisiblePageBounds(ABounds, APageViewInfo);
  Result := AExporter;
end;

procedure TdxDraftView.Visit(const AVisitor: IdxRichEditViewVisitor);
begin
  AVisitor.Visit(Self);
end;

function TdxDraftView.GetPageViewInfoRowFromPoint(const APoint: TPoint; AStrictSearch: Boolean): TdxPageViewInfoRow;
begin
  Result := inherited GetPageViewInfoRowFromPoint(APoint, False);
end;

function TdxDraftView.PerformStrictPageViewInfoHitTest(APageViewInfo: TdxPageViewInfo; const APt: TPoint): Boolean;
var
  ABounds: TRect;
begin
  ABounds := APageViewInfo.ClientBounds;
  Result := (ABounds.Left <= APt.X) and (APt.X <= ABounds.Right);
end;

end.
