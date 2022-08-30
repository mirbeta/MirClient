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

unit dxRichEdit.View.PrintLayout;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Windows,
{$IFDEF DELPHIXE2}
  System.UITypes,
{$ENDIF}
  SysUtils, Types, Generics.Defaults, Generics.Collections, Controls, SyncObjs, Classes, Graphics, ActiveX,
  cxGraphics, dxCoreClasses, cxControls, dxCoreGraphics, dxSkinsCore, dxSkinInfo,
  dxGDIPlusAPI, dxGDIPlusClasses,

  dxRichEdit.Types,
  dxGenerics,
  dxRichEdit.Utils.Types,
  dxRichEdit.Utils.Graphics,
  dxRichEdit.Utils.BatchUpdateHelper,
  dxRichEdit.Utils.DataObject,
  dxRichEdit.Utils.PredefinedFontSizeCollection,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.Selection,
  dxRichEdit.DocumentModel.Boxes.Simple,
  dxRichEdit.DocumentModel.Boxes,
  dxRichEdit.DocumentLayout,
  dxRichEdit.DocumentLayout.Position,
  dxRichEdit.LayoutEngine.Formatter,
  dxRichEdit.LayoutEngine.DocumentFormatter,
  dxRichEdit.DocumentModel.VisibleTextFilter.Core,
  dxRichEdit.Control.HitTest,
  dxRichEdit.Control.HotZones,
  dxRichEdit.View.Core,
  dxRichEdit.View.ViewInfo,
  dxRichEdit.View.PageViewInfoGenerator,
  dxRichEdit.Platform.Win.Painter,
  dxRichEdit.Platform.Win.Control,
  dxRichEdit.DocumentLayout.UnitConverter,
  dxRichEdit.DocumentModel.DocumentProperties,
  dxRichEdit.DocumentModel.Section,
  dxRichEdit.DocumentModel.SectionFormatting;

type
  TdxPrintLayoutView = class;

  { IdxHeaderFooterDecoratorPainterOwner }

  IdxHeaderFooterDecoratorPainterOwner = interface
  ['{3FB25038-005C-4A70-9BC1-4A8A9D571B9A}']
    function GetLeftHeaderFooterLineOffset: Integer;
    function GetRightHeaderFooterLineOffset: Integer;
    function GetHeaderFooterLineColor: TdxAlphaColor;
    function GetHeaderFooterMarkBackColor: TdxAlphaColor;
    function GetHeaderFooterMarkTextColor: TdxAlphaColor;

    property LeftHeaderFooterLineOffset: Integer read GetLeftHeaderFooterLineOffset;
    property RightHeaderFooterLineOffset: Integer read GetRightHeaderFooterLineOffset;
    property HeaderFooterLineColor: TdxAlphaColor read GetHeaderFooterLineColor;
    property HeaderFooterMarkBackColor: TdxAlphaColor read GetHeaderFooterMarkBackColor;
    property HeaderFooterMarkTextColor: TdxAlphaColor read GetHeaderFooterMarkTextColor;
  end;

  { TdxPrintLayoutViewPageViewInfoGenerator }

  TdxPrintLayoutViewPageViewInfoGenerator = class(TdxPageViewInfoGenerator)
  private
    FPageHorizontalAlignment: TAlignment;
    FMaxHorizontalPageCount: Integer;
  protected
    function CreateEmptyClone: TdxPageGeneratorLayoutManager; override;
    function CalculateFirstPageLeftOffset(ATotalPagesWidth: Integer): Integer; override;
  public
    constructor Create(AView: TdxRichEditView; AViewInfo: TObject); reintroduce;
    function CanFitPageToPageRow(APage: TdxPage; ARow: TdxPageViewInfoRow): Boolean; override;

    property PageHorizontalAlignment: TAlignment read FPageHorizontalAlignment write FPageHorizontalAlignment;
    property MaxHorizontalPageCount: Integer read FMaxHorizontalPageCount write FMaxHorizontalPageCount;
  end;

  { TdxPrintLayoutViewBoxHitTestCalculator }

  TdxPrintLayoutViewBoxHitTestCalculator = class(TdxBoxHitTestCalculator)
  protected
    function ProcessSpecificPageArea(AArea: TdxPageArea; AAreaKind: TdxPageAreaHitTestManager.TPageAreaKind; ACollection: TdxPageAreaCollection): Boolean;
  public
    constructor Create(const ARequest: TdxRichEditHitTestRequest; AResult: TdxRichEditHitTestResult);
    procedure ProcessPageAreaCollection(ACollection: TdxPageAreaCollection); override;
  end;

  { TdxPrintLayoutViewDocumentFormattingController }

  TdxPrintLayoutViewDocumentFormattingController = class(TdxDocumentFormattingController)
  protected
    function CreatePageController: TdxPageController; override;
    function CreateColumnController: TdxColumnController; override;
    function CreatePageAreaController: TdxPageAreaController; override;
    function CreateRowController: TdxRowsController; override;
  public
    constructor Create(ADocumentLayout: TdxDocumentLayout; APieceTable: TdxPieceTable);
  end;

  { TdxHeaderFooterPageController }

  TdxHeaderFooterPageController = class(TdxPageController)
  strict private
    FPages: TdxPageCollection;
    FPieceTable: TdxPieceTable;
    FPageNumberSource: TdxPage;
    FSectionIndex: TdxSectionIndex;
  protected
    function GetPages: TdxPageCollection; override;
    function GetPieceTable: TdxPieceTable; override;
    function GetCurrentSection: TdxSection; override;
    function GetCurrentSectionIndex: TdxSectionIndex; override;
    procedure SetCurrentSectionIndex(const AValue: TdxSectionIndex); override;
    function GetPageNumberSource: TdxPage; virtual;
    procedure SetPageNumberSource(const AValue: TdxPage); virtual;
  public
    constructor Create(ADocumentLayout: TdxDocumentLayout; AFloatingObjectsLayout: TdxFloatingObjectsLayout; AParagraphFramesLayout: TdxParagraphFramesLayout; APieceTable: TdxPieceTable);
    destructor Destroy; override;
    function CreatePageBoundsCalculator: TdxPageBoundsCalculator; override;
    function CreateHitTestCalculator(const ARequest: TdxRichEditHitTestRequest;
      AResult: TdxRichEditHitTestResult): TdxBoxHitTestCalculator; override;
    procedure ClearFloatingObjectsLayout; override;
    procedure ClearParagraphFramesLayout; override;
    procedure SetFixedSectionIndex(ASectionIndex: TdxSectionIndex);
    procedure Reset(AKeepFloatingObjects: Boolean); override;
    function GetNextPage(AKeepFloatingObjects: Boolean): TdxPage; override;

    property PageNumberSource: TdxPage read GetPageNumberSource write SetPageNumberSource;
  end;

  { TdxHeaderFooterPageBoundsCalculator }

  TdxHeaderFooterPageBoundsCalculator = class(TdxPageBoundsCalculator)
  public
    constructor Create(AController: TdxHeaderFooterPageController);
  end;

  { TdxHeaderFooterColumnsBoundsCalculator }

  TdxHeaderFooterColumnsBoundsCalculator = class(TdxColumnsBoundsCalculator)
  public
    procedure PopulateColumnsBounds(const AResult: TdxRectList; const ABounds: TRect;
      const AColumnInfoCollection: TdxColumnInfoCollection); override;
    procedure PopulateEqualWidthColumnsBounds(const AResult: TdxRectList;
      const ABounds: TRect; AColumnCount, ASpaceBetweenColumns: Integer); override;
  end;

  { TdxHeaderFooterColumnController }

  TdxHeaderFooterColumnController = class(TdxColumnController)
  protected
    function GetPageLastRunIndex: TdxRunIndex; override;
  public
    function CalculateColumnBoundsCore(AColumnIndex: Integer): TRect; override;
    function CreateColumnBoundsCalculator: TdxColumnsBoundsCalculator; override;
  end;

  { TdxHeaderFooterPageAreaController }

  TdxHeaderFooterPageAreaController = class(TdxPageAreaController)
  strict private
    FAreas: TdxPageAreaCollection;
  protected
    function GetAreas: TdxPageAreaCollection; override;
    function GetCurrentAreaBounds: TRect; override;
  public
    constructor Create(APageController: TdxPageController);
    destructor Destroy; override;
    function CreateDefaultState(ACurrentAreaIndex: Integer): TdxPageAreaControllerState; override;
    function CompleteCurrentAreaFormatting: TdxCompleteFormattingResult; override;
    function GetNextPageArea(AKeepFloatingObjects: Boolean): TdxPageArea; override;
    procedure Reset(ASection: TdxSection); override;
    procedure BeginSectionFormatting(ASection: TdxSection; ACurrentColumnsCount: Integer); override;
    procedure RestartFormattingFromTheStartOfSection(ASection: TdxSection; ACurrentAreaIndex: Integer); override;
    procedure RestartFormattingFromTheMiddleOfSection(ASection: TdxSection; ACurrentAreaIndex: Integer); override;
    procedure ClearInvalidatedContent(const APos: TdxFormatterPosition); override;
    procedure SwitchToState(AState: TdxPageAreaControllerState); override;
  end;

  { TdxHeaderFooterRowsController }

  TdxHeaderFooterRowsController = class(TdxRowsController)
  public
    function CanRestartDueFloatingObject: Boolean; override;
  end;

  { TdxHeaderFooterDocumentFormattingController }

  TdxHeaderFooterDocumentFormattingController = class(TdxDocumentFormattingController)
  protected
    function CreatePageController: TdxPageController; override;
    function CreatePageAreaController: TdxPageAreaController; override;
    function CreateColumnController: TdxColumnController; override;
    function CreateRowController: TdxRowsController; override;
  public
    constructor Create(ADocumentLayout: TdxDocumentLayout; AFloatingObjectsLayout: TdxFloatingObjectsLayout;
      AParagraphFramesLayout: TdxParagraphFramesLayout; APieceTable: TdxPieceTable; AArea: TdxPageArea;
      ASectionIndex: TdxSectionIndex; APageNumberSource: TdxPage);
    procedure NotifyDocumentFormattingComplete; override;
  end;

  { TdxSimplePieceTablePrimaryFormatter }

  TdxSimplePieceTablePrimaryFormatter = class
  strict private
    FPieceTable: TdxPieceTable;
    FRowsController: TdxRowsController;
    FPage: TdxPage;
    function GetDocumentModel: TdxDocumentModel;
  public
    constructor Create(APieceTable: TdxPieceTable; AMeasurer: TdxBoxMeasurer; ARowsController: TdxRowsController;
      const AVisibleTextFilter: TdxVisibleTextFilterBase; APage: TdxPage);
    function Format(AMaxHeight: Integer; AController: TdxDocumentFormattingController): Integer; virtual;

    property PieceTable: TdxPieceTable read FPieceTable;
    property DocumentModel: TdxDocumentModel read GetDocumentModel;
    property Page: TdxPage read FPage;
  end;

  TShouldMoveBoxVertically = function (ABox: TdxFloatingObjectBox): Boolean of object;

  { TdxPageHeaderFooterFormatterBase }

  TdxPageHeaderFooterFormatterBase<TModelObject: TdxContentTypeBase; TLayoutObject: TdxHeaderFooterPageAreaBase> = class abstract
  strict private
    FDocumentLayout: TdxDocumentLayout;
    FFloatingObjectsLayout: TdxFloatingObjectsLayout;
    FParagraphFramesLayout: TdxParagraphFramesLayout;
    FPage: TdxPage;
    FSectionIndex: TdxSectionIndex;
    FSection: TdxSection;
  strict protected
    function CalculateDeltaAfterAdjustPageClientBounds(const AOldPageClientBounds, ANewPageClientBounds, AColumnBounds: TRect): Integer; virtual; abstract;
    function GetExistingArea(APage: TdxPage): TLayoutObject; virtual; abstract;
    procedure MoveParagraphFramesVertically(APieceTable: TdxPieceTable; ADeltaY: Integer); virtual;
    procedure MoveFloatingObjectsVertically(APieceTable: TdxPieceTable; ADeltaY: Integer; ACheckMoveFunc: TShouldMoveBoxVertically); virtual;
    function ShouldMoveFloatingObjectVertically(ABox: TdxFloatingObjectBox): Boolean; virtual;
    function ShouldMoveFloatingObjectVerticallyAfterAdjustPageClientBounds(ABox: TdxFloatingObjectBox): Boolean; virtual; abstract;
    function FormatArea(AContentType: TModelObject; AArea: TLayoutObject; const AAvailableAreaBounds: TRect): Integer; virtual;
    function GetPageAreaBottom(AArea: TdxPageArea; AActualAreaBottom: Integer): Integer;
    function FindRow(AColumn: TdxColumn; Y: Integer): TdxRow; overload;
    function FindTable(ATables: TdxTableViewInfoCollection; Y: Integer): TdxTableViewInfo;
    function FindRow(ATable: TdxTableViewInfo; Y: Integer): TdxRow; overload;
    function FindRow(ARows: TdxRowCollection; Y: Integer): TdxRow; overload;
    function GetActualModelObject(AFirstPageOfSection: Boolean): TModelObject; virtual; abstract;
    function CreatePageArea(APieceTable: TModelObject): TLayoutObject; virtual; abstract;
    procedure AppendPageAreaToPage(AArea: TLayoutObject); virtual; abstract;
    function CalculateAvailableAreaBounds: TRect; virtual; abstract;
    function CalculateFinalAreaBounds(AArea: TLayoutObject; AActualAreaBottom: Integer): TRect; virtual; abstract;
    function CalculateFinalAdjustedPageClientBounds(AArea: TLayoutObject): TRect; virtual; abstract;
    function AdjustNewAreaBounds(APieceTable: TdxPieceTable; const ANewAreaBounds: TRect): TRect; virtual; abstract;
    procedure MoveFloatingObjectsAfterAdjustPageClientBounds(APieceTable: TdxPieceTable; ADeltaY: Integer); virtual;

    property FloatingObjectsLayout: TdxFloatingObjectsLayout read FFloatingObjectsLayout;
    property ParagraphFramesLayout: TdxParagraphFramesLayout read FParagraphFramesLayout;
  public
    constructor Create(ADocumentLayout: TdxDocumentLayout; AFloatingObjectsLayout: TdxFloatingObjectsLayout; AParagraphFramesLayout: TdxParagraphFramesLayout; APage: TdxPage; ASectionIndex: TdxSectionIndex);
    procedure Format(AFirstPageOfSection: Boolean); virtual;
    procedure ApplyExistingAreaBounds; virtual;

    property DocumentLayout: TdxDocumentLayout read FDocumentLayout;
    property Page: TdxPage read FPage;
    property SectionIndex: TdxSectionIndex read FSectionIndex;
    property Section: TdxSection read FSection;
  end;

  { TdxPageHeaderFormatter }

  TdxPageHeaderFormatter = class(TdxPageHeaderFooterFormatterBase<TdxSectionHeader, TdxHeaderPageArea>)
  protected
    function CalculateDeltaAfterAdjustPageClientBounds(const AOldPageClientBounds, ANewPageClientBounds, AColumnBounds: TRect): Integer; override;
    function GetExistingArea(APage: TdxPage): TdxHeaderPageArea; override;
    function CreatePageArea(APieceTable: TdxSectionHeader): TdxHeaderPageArea; override;
    procedure AppendPageAreaToPage(AArea: TdxHeaderPageArea); override;
    function CalculateAvailableAreaBounds: TRect; override;
    function CalculateFinalAreaBounds(AArea: TdxHeaderPageArea; AActualAreaBottom: Integer): TRect; override;
    function CalculateFinalAdjustedPageClientBounds(AArea: TdxHeaderPageArea): TRect; override;
    function AdjustNewAreaBounds(APieceTable: TdxPieceTable; const ANewAreaBounds: TRect): TRect; override;
    function ShouldMoveFloatingObjectVerticallyAfterAdjustPageClientBounds(ABox: TdxFloatingObjectBox): Boolean; override;
  public
    function GetActualModelObject(AFirstPageOfSection: Boolean): TdxSectionHeader; override;
  end;

  { TdxPageFooterFormatter }

  TdxPageFooterFormatter = class(TdxPageHeaderFooterFormatterBase<TdxSectionFooter, TdxFooterPageArea>)
  protected
    function CalculateDeltaAfterAdjustPageClientBounds(const AOldPageClientBounds, ANewPageClientBounds, AColumnBounds: TRect): Integer; override;
    function GetExistingArea(APage: TdxPage): TdxFooterPageArea; override;
    function CreatePageArea(APieceTable: TdxSectionFooter): TdxFooterPageArea; override;
    procedure AppendPageAreaToPage(AArea: TdxFooterPageArea); override;
    function CalculateAvailableAreaBounds: TRect; override;
    function CalculateFinalAreaBounds(AArea: TdxFooterPageArea; AActualAreaBottom: Integer): TRect; override;
    procedure MoveFloatingObjectsVertically(APieceTable: TdxPieceTable; ADeltaY: Integer; ACheckMoveFunc: TShouldMoveBoxVertically); override;
    function AdjustNewAreaBounds(APieceTable: TdxPieceTable; const ANewAreaBounds: TRect): TRect; override;
    function CalculateFloatingObjectsBottom(APieceTable: TdxPieceTable; AInitialBottom: Integer): Integer;
    function CalculateFloatingObjectsBottomWithDelta(APieceTable: TdxPieceTable; AInitialBottom: Integer; ADelta: Integer): Integer; overload;
    function CalculateFloatingObjectsBottomWithDelta(APieceTable: TdxPieceTable; ADelta: Integer): Integer; overload;
    function CalculateFinalAdjustedPageClientBounds(AArea: TdxFooterPageArea): TRect; override;
    procedure MoveFloatingObjectsAfterAdjustPageClientBounds(APieceTable: TdxPieceTable; ADeltaY: Integer); override;
    function ShouldMoveFloatingObjectVerticallyAfterAdjustPageClientBounds(ABox: TdxFloatingObjectBox): Boolean; override;
  public
    function GetActualModelObject(AFirstPageOfSection: Boolean): TdxSectionFooter; override;
  end;

  { TdxPrintLayoutViewPageController }

  TdxPrintLayoutViewPageController = class(TdxPageController)
  protected
    procedure FormatHeader(APage: TdxPage; AFirstPageOfSection: Boolean); override;
    procedure FormatFooter(APage: TdxPage; AFirstPageOfSection: Boolean); override;
  public
    constructor Create(ADocumentLayout: TdxDocumentLayout);
    procedure ApplyExistingHeaderAreaBounds(APage: TdxPage); override;
    procedure ApplyExistingFooterAreaBounds(APage: TdxPage); override;
    function CreatePageBoundsCalculator: TdxPageBoundsCalculator; override;
    function CreateHitTestCalculator(const ARequest: TdxRichEditHitTestRequest; AResult: TdxRichEditHitTestResult): TdxBoxHitTestCalculator; override;
  end;

  { TdxPrintLayoutViewColumnController }

  TdxPrintLayoutViewColumnController = class(TdxColumnController)
  public
    function CalculateColumnBoundsCore(AColumnIndex: Integer): TRect; override;
  end;

  { TdxPrintLayoutViewPageAreaController }

  TdxPrintLayoutViewPageAreaController = class(TdxPageAreaController)
  protected
    procedure SwitchToDefaultState; virtual;
  public
    function CreateDefaultState(ACurrentAreaIndex: Integer): TdxPageAreaControllerState; override;
    procedure Reset(ASection: TdxSection); override;
    procedure RestartFormattingFromTheStartOfSection(ASection: TdxSection; ACurrentAreaIndex: Integer); override;
    procedure RestartFormattingFromTheMiddleOfSection(ASection: TdxSection; ACurrentAreaIndex: Integer); override;
  end;

  { TdxPrintLayoutViewDefaultPageAreaControllerState }

  TdxPrintLayoutViewDefaultPageAreaControllerState = class(TdxDefaultPageAreaControllerState);

  { TdxFinishContinuousSectionStartPageAreaControllerState }

  TdxFinishContinuousSectionStartPageAreaControllerState = class(TdxPageAreaControllerState)
  private
    FAreaBounds: TRect;
    FGetNextPageAreaCount: Integer;
  public
    constructor Create(AOwner: TdxPageAreaController; const AAreaBounds: TRect);
    procedure ApplySectionStart(ASection: TdxSection; ACurrentColumnsCount: Integer); override;
    function CreateCurrentAreaBoundsCore: TRect; override;
    function CompleteCurrentAreaFormatting: TdxCompleteFormattingResult; override;
    function GetNextPageArea(AKeepFloatingObjects: Boolean): TdxPageArea; override;
  end;

  { TdxHeaderFooterDecoratorPainter }

  TdxHeaderFooterDecoratorPainter = class(TInterfacedObject, IdxDecoratorPainter)
  public const
    HeaderFooterLineThinkness = 2;
  strict private
    FCurrentPageInfo: TdxPageViewInfo;
    FHeaderFooterLinePen: TdxGPPen;
    FHeaderFooterMarkFont: TFont;
    FRoundedRectangleRadius: Integer;
    FView: TdxPrintLayoutView;
    FOwner: IdxHeaderFooterDecoratorPainterOwner;
    function GetDocumentModel: TdxDocumentModel;
    function GetRichEditControl: IdxRichEditControl;
  protected
    FControl: TdxVCLControl;
    function GetView: TdxPrintLayoutView; virtual;
    function GetOwner: IdxHeaderFooterDecoratorPainterOwner; virtual;
    procedure DrawPageDecorators(APage: TdxPage; APainter: TdxPainter); virtual;
    procedure DrawHeaderFrame(APage: TdxPage; AArea: TdxPageArea; APainter: TdxPainter); virtual;
    procedure DrawFooterFrame(APage: TdxPage; AArea: TdxPageArea; APainter: TdxPainter); virtual;
    function DrawHeaderFooterLine(APainter: TdxPainter; const AInitialLineBounds: TRect): TRect; virtual;
    function GetCaption(AHeaderFooter: TdxSectionHeaderFooterBase; APage: TdxPage): string;
    function MeasureString(const ACaption: string; APainter: TdxGdiPlusPainter): TSize; virtual;
    function CalculateHeaderCaptionBaseBounds(const ACaption: string; const AHeaderLineBounds: TRect; AGdiPlusPainter: TdxGdiPlusPainter): TRect; virtual;
    function CalculateHeaderLinkCaptionBaseBounds(const ACaption: string; const AHeaderLineBounds: TRect; AGdiPlusPainter: TdxGdiPlusPainter): TRect; virtual;
    function CalculateFooterCaptionBaseBounds(const ACaption: string; const AHeaderLineBounds: TRect; AGdiPlusPainter: TdxGdiPlusPainter): TRect; virtual;
    function CalculateFooterLinkCaptionBaseBounds(const ACaption: string; const AFooterLineBounds: TRect; AGdiPlusPainter: TdxGdiPlusPainter): TRect; virtual;
    procedure DrawHeaderCaptionBackground(const ABounds: TRect; AGdiPlusPainter: TdxGdiPlusPainter); virtual;
    procedure DrawFooterCaptionBackground(const ABounds: TRect; AGdiPlusPainter: TdxGdiPlusPainter); virtual;
    function CreateHeaderCaptionBackgroundPath(const ABounds: TRect): TdxGPPath; virtual;
    function CreateFooterCaptionBackgroundPath(const ABounds: TRect): TdxGPPath; virtual;
    procedure DrawHeaderFooterCaptionBackground(APath: TdxGPPath; AGdiPlusPainter: TdxGdiPlusPainter); virtual;
    procedure DrawCaptionText(const ACaption: string; ABounds: TRect; AGdiPlusPainter: TdxGdiPlusPainter; AOffset: Integer); virtual;
    function CreateHeaderFooterLinePen: TdxGPPen; virtual;
    function CreateHeaderFooterCaptionFont: TFont; virtual;

    property View: TdxPrintLayoutView read GetView;
    property Owner: IdxHeaderFooterDecoratorPainterOwner read GetOwner;
    property DocumentModel: TdxDocumentModel read GetDocumentModel;
    property Control: TdxVCLControl read FControl;
    property RichEditControl: IdxRichEditControl read GetRichEditControl;
    property CurrentPageInfo: TdxPageViewInfo read FCurrentPageInfo;
  public
    constructor Create(AView: TdxPrintLayoutView; const AOwner: IdxHeaderFooterDecoratorPainterOwner);
    destructor Destroy; override;
    procedure DrawDecorators(APainter: TdxPainter; AViewInfos: TdxPageViewInfoCollection);
  end;

  { TdxPrintLayoutViewPainter }

  TdxPrintLayoutViewPainter = class abstract(TdxRichEditViewPainter, IdxHeaderFooterDecoratorPainterOwner)
  strict private
    function GetUnitConverter: TdxDocumentLayoutUnitConverter;
  protected
    function GetLeftHeaderFooterLineOffset: Integer; virtual;
    function GetRightHeaderFooterLineOffset: Integer; virtual;
    function GetHeaderFooterLineColor: TdxAlphaColor; virtual;
    function GetHeaderFooterMarkBackColor: TdxAlphaColor; virtual;
    function GetHeaderFooterMarkTextColor: TdxAlphaColor; virtual;
    procedure DrawEmptyPage(AGraphics: TdxGraphics{ACache: TdxGraphicsCache}; APage: TdxPageViewInfo); override;
    procedure DrawEmptyComment(AGraphics: TdxGraphics; AComment: TdxCommentViewInfo); override;
    procedure DrawEmptyExtensionComment(AGraphics: TdxGraphics; AComment: TdxCommentViewInfo); override;
    procedure DrawRoundedRectangle(AGraphics: TdxGraphics; const ARect: TRect; ARadius: Integer; AFillColor: TdxAlphaColor; ALineColor: TdxAlphaColor);
    function GetActualPageBackColor: TdxAlphaColor; virtual;
    function GetActualCommentBackColor: TdxAlphaColor; virtual;

    property UnitConverter: TdxDocumentLayoutUnitConverter read GetUnitConverter;
  public
    constructor Create(AView: TdxPrintLayoutView);

    property LeftHeaderFooterLineOffset: Integer read GetLeftHeaderFooterLineOffset;
    property RightHeaderFooterLineOffset: Integer read GetRightHeaderFooterLineOffset;
    property HeaderFooterLineColor: TdxAlphaColor read GetHeaderFooterLineColor;
    property HeaderFooterMarkBackColor: TdxAlphaColor read GetHeaderFooterMarkBackColor;
    property HeaderFooterMarkTextColor: TdxAlphaColor read GetHeaderFooterMarkTextColor;
  end;

  { TdxPrintLayoutViewSkinPainter }

  TdxPrintLayoutViewSkinPainter = class(TdxPrintLayoutViewPainter)
  strict private
    FSkinInfo: TdxSkinInfo;
  protected
    function GetLeftHeaderFooterLineOffset: Integer; override;
    function GetRightHeaderFooterLineOffset: Integer; override;
    procedure DrawEmptyPages(AGraphics: TdxGraphics); override;
    procedure DrawEmptyPage(AGraphics: TdxGraphics; APage: TdxPageViewInfo); override;
    function GetActualPageBackColor: TdxAlphaColor; reintroduce; virtual;
    function CalculatePageViewInfoPixelBounds(APageViewInfo: TdxPageViewInfo): TRect; virtual;
    property SkinInfo: TdxSkinInfo read FSkinInfo;
  public
    constructor Create(AView: TdxPrintLayoutView);
    class function GetSkinEdges(AElement: TdxSkinElement): TRect; static;
  end;

  { TdxPrintLayoutViewBackgroundPainter }

  TdxPrintLayoutViewBackgroundPainter = class(TdxRichEditViewBackgroundPainter);

  { TdxPrintLayoutViewSkinBackgroundPainter }

  TdxPrintLayoutViewSkinBackgroundPainter = class(TdxRichEditViewSkinBackgroundPainter);

  { TdxHeaderFooterSelectionLayout }

  TdxHeaderFooterSelectionLayout = class(TdxSelectionLayout)
  public
    function CreateDocumentLayoutPosition(ALogPosition: TdxDocumentLogPosition): TdxDocumentLayoutPosition; override;
  end;

  { TdxPrintLayoutView }

  TdxPrintLayoutView = class(TdxPageBasedRichEditView)
  private
    FTextBoxPageIndex: Integer;
    function GetPageHorizontalAlignment: TAlignment;
    procedure SetPageHorizontalAlignment(const AValue: TAlignment);
    function GetMaxHorizontalPageCount: Integer;
    procedure SetMaxHorizontalPageCount(const AValue: Integer);
  protected
    function GetDefaultAllowDisplayLineNumbers: Boolean; override;
    function GetType: TdxRichEditViewType; override;
    function GetShowHorizontalRulerByDefault: Boolean; override;
    function GetShowVerticalRulerByDefault: Boolean; override;
    function GetMatchHorizontalTableIndentsToTextEdge: Boolean; override;
    function GetFixedLeftTextBorderOffset: Integer; override;
    function CreateDocumentFormattingController: TdxDocumentFormattingController; override;
    function CreatePageViewInfoGenerator: TdxPageViewInfoGenerator; override;
    function ExpandClipBoundsToPaddings(const AClipBounds: TRect): TRect; override;
  public
    constructor Create(const AControl: IdxRichEditControl); override;
    function CreateDocumentLayoutExporter(APainter: TdxPainter; AAdapter: TdxGraphicsDocumentLayoutExporterAdapter;
      APageViewInfo: TdxPageViewInfo; const ABounds: TRect): TdxDocumentLayoutExporter; override;
    function CreatePainter: TdxRichEditViewPainter; override;
    function CreateBackgroundPainter: TdxRichEditViewBackgroundPainter; override;
    procedure OnActivePieceTableChanged; override;
    procedure Visit(const AVisitor: IdxRichEditViewVisitor); override;
  published
    property AllowDisplayLineNumbers;
    property BackColor;
    property PageHorizontalAlignment: TAlignment read GetPageHorizontalAlignment write SetPageHorizontalAlignment default taCenter;
    property MaxHorizontalPageCount: Integer read GetMaxHorizontalPageCount write SetMaxHorizontalPageCount default 0;
    property ZoomFactor;
  end;

implementation

uses
  Math, Contnrs, dxCore, dxTypeHelpers, cxGeometry, cxLookAndFeelPainters,

  dxRichEdit.DocumentModel.Boxes.Core,
  dxRichEdit.DocumentModel.FloatingObjectFormatting,
  dxRichEdit.DocumentModel.FloatingObjectRange,
  dxRichEdit.Strs;

type
  TdxPrintLayoutViewPageViewInfoGeneratorHelper = class helper for TdxPrintLayoutViewPageViewInfoGenerator
  strict private
    function GetView: TdxRichEditView;
    function GetViewInfo: TdxRichEditViewInfo;
  public
    property View: TdxRichEditView read GetView;
    property ViewInfo: TdxRichEditViewInfo read GetViewInfo;
  end;

function TdxPrintLayoutViewPageViewInfoGeneratorHelper.GetView: TdxRichEditView;
begin
  Result := TdxRichEditView(FView);
end;

function TdxPrintLayoutViewPageViewInfoGeneratorHelper.GetViewInfo: TdxRichEditViewInfo;
begin
  Result := TdxRichEditViewInfo(FViewInfo);
end;

{ TdxPrintLayoutViewPageViewInfoGenerator }

constructor TdxPrintLayoutViewPageViewInfoGenerator.Create(AView: TdxRichEditView; AViewInfo: TObject);
begin
  inherited Create(AView, AViewInfo);
  FPageHorizontalAlignment := TAlignment.taCenter;
end;

function TdxPrintLayoutViewPageViewInfoGenerator.CreateEmptyClone: TdxPageGeneratorLayoutManager;
begin
  Result := TdxPrintLayoutViewPageViewInfoGenerator.Create(View, ViewInfo);
end;

function TdxPrintLayoutViewPageViewInfoGenerator.CalculateFirstPageLeftOffset(ATotalPagesWidth: Integer): Integer;
begin
  case PageHorizontalAlignment of
    TAlignment.taLeftJustify:
      Result := 0;
    TAlignment.taRightJustify:
      Result := ViewPortBounds.Width - ATotalPagesWidth;
    else
      Result := (ViewPortBounds.Width - ATotalPagesWidth) div 2;
  end;
end;

function TdxPrintLayoutViewPageViewInfoGenerator.CanFitPageToPageRow(APage: TdxPage; ARow: TdxPageViewInfoRow): Boolean;
begin
  if MaxHorizontalPageCount <= 0 then
    Result := inherited CanFitPageToPageRow(APage, ARow)
  else
  begin
    if ARow.Count >= MaxHorizontalPageCount then
      Result := False
    else
      Result := inherited CanFitPageToPageRow(APage, ARow);
  end;
end;

{ TdxPrintLayoutViewBoxHitTestCalculator }

constructor TdxPrintLayoutViewBoxHitTestCalculator.Create(const ARequest: TdxRichEditHitTestRequest; AResult: TdxRichEditHitTestResult);
begin
  inherited Create(ARequest, AResult);
end;

procedure TdxPrintLayoutViewBoxHitTestCalculator.ProcessPageAreaCollection(ACollection: TdxPageAreaCollection);
begin
  if HitTestRequest.SearchAnyPieceTable then
  begin
    if ProcessSpecificPageArea(HitTestResult.Page.Header, TdxPageAreaHitTestManager.TPageAreaKind.Header, ACollection) then
      Exit;
    if ProcessSpecificPageArea(HitTestResult.Page.Footer, TdxPageAreaHitTestManager.TPageAreaKind.Footer, ACollection) then
      Exit;
    inherited ProcessPageAreaCollection(ACollection);
  end
  else
  begin
    if (HitTestResult.Page.Header <> nil) and (HitTestRequest.PieceTable = HitTestResult.Page.Header.PieceTable) then
    begin
      ProcessSpecificPageArea(HitTestResult.Page.Header, TdxPageAreaHitTestManager.TPageAreaKind.Header, ACollection);
      Exit;
    end;
    if (HitTestResult.Page.Footer <> nil) and (HitTestRequest.PieceTable = HitTestResult.Page.Footer.PieceTable) then
    begin
      ProcessSpecificPageArea(HitTestResult.Page.Footer, TdxPageAreaHitTestManager.TPageAreaKind.Footer, ACollection);
      Exit;
    end;
    if HitTestRequest.PieceTable = HitTestRequest.PieceTable.DocumentModel.MainPieceTable then
    begin
      inherited ProcessPageAreaCollection(ACollection);
      Exit;
    end;
    ACollection.RegisterFailedItemHitTest(Self);
  end;
end;

function TdxPrintLayoutViewBoxHitTestCalculator.ProcessSpecificPageArea(AArea: TdxPageArea; AAreaKind: TdxPageAreaHitTestManager.TPageAreaKind; ACollection: TdxPageAreaCollection): Boolean;
var
  AManager: TdxPageAreaHitTestManager;
begin
  if AArea = nil then
    Exit(False);

  AManager := TdxPageAreaHitTestManager(AArea.CreateHitTestManager(Self));
  try
    AManager.AreaKind := AAreaKind;
    AManager.CalcHitTest(False);
  finally
    AManager.Free;
  end;
  if (HitTestResult.Accuracy and ExactPageArea) <> 0 then
  begin
    ACollection.RegisterSuccessfullItemHitTest(Self, AArea);
    Exit(True);
  end
  else
  begin
    if (HitTestRequest.Accuracy and ExactPageArea) <> 0 then
    begin
      ACollection.RegisterFailedItemHitTest(Self);
      Exit(False);
    end;
    HitTestResult.IncreaseDetailsLevel(TdxDocumentLayoutDetailsLevel.PageArea);
    Exit(True);
  end;
end;

{ TdxPrintLayoutViewDocumentFormattingController }

constructor TdxPrintLayoutViewDocumentFormattingController.Create(ADocumentLayout: TdxDocumentLayout; APieceTable: TdxPieceTable);
begin
  inherited Create(ADocumentLayout, APieceTable, nil, nil);
end;

function TdxPrintLayoutViewDocumentFormattingController.CreatePageController: TdxPageController;
begin
  Result := TdxPrintLayoutViewPageController.Create(DocumentLayout);
end;

function TdxPrintLayoutViewDocumentFormattingController.CreateColumnController: TdxColumnController;
begin
  Result := TdxPrintLayoutViewColumnController.Create(PageAreaController);
end;

function TdxPrintLayoutViewDocumentFormattingController.CreatePageAreaController: TdxPageAreaController;
begin
  Result := TdxPrintLayoutViewPageAreaController.Create(PageController);
end;

function TdxPrintLayoutViewDocumentFormattingController.CreateRowController: TdxRowsController;
begin
  Result := TdxRowsController.Create(PieceTable, ColumnController,
    DocumentModel.LayoutOptions.PrintLayoutView.MatchHorizontalTableIndentsToTextEdge);
end;

{ TdxHeaderFooterPageController }

constructor TdxHeaderFooterPageController.Create(ADocumentLayout: TdxDocumentLayout; AFloatingObjectsLayout: TdxFloatingObjectsLayout; AParagraphFramesLayout: TdxParagraphFramesLayout; APieceTable: TdxPieceTable);
begin
  inherited Create(ADocumentLayout, AFloatingObjectsLayout, AParagraphFramesLayout);
  FSectionIndex := 0;
  Assert(APieceTable <> nil);
  FPages := TdxPageCollection.Create;
  FPieceTable := APieceTable;
  SetPageLastRunIndex(MaxInt - 1);
end;

destructor TdxHeaderFooterPageController.Destroy;
begin
  FPages.Free;
  inherited Destroy;
end;

function TdxHeaderFooterPageController.GetPages: TdxPageCollection;
begin
  Result := FPages;
end;

function TdxHeaderFooterPageController.GetPieceTable: TdxPieceTable;
begin
  Result := FPieceTable;
end;

function TdxHeaderFooterPageController.GetCurrentSection: TdxSection;
begin
  Result := DocumentModel.Sections[CurrentSectionIndex];
end;

function TdxHeaderFooterPageController.GetCurrentSectionIndex: TdxSectionIndex;
begin
  Result := FSectionIndex;
end;

procedure TdxHeaderFooterPageController.SetCurrentSectionIndex(const AValue: TdxSectionIndex);
begin
end;

function TdxHeaderFooterPageController.GetPageNumberSource: TdxPage;
begin
  Result := FPageNumberSource;
end;

procedure TdxHeaderFooterPageController.SetPageNumberSource(const AValue: TdxPage);
begin
  FPageNumberSource := AValue;
end;

procedure TdxHeaderFooterPageController.SetFixedSectionIndex(ASectionIndex: TdxSectionIndex);
begin
  FSectionIndex := ASectionIndex;
end;

procedure TdxHeaderFooterPageController.Reset(AKeepFloatingObjects: Boolean);
begin
  inherited Reset(AKeepFloatingObjects);
  Pages.Add(TdxPage.Create(FPageNumberSource));
end;

function TdxHeaderFooterPageController.GetNextPage(AKeepFloatingObjects: Boolean): TdxPage;
begin
  Result := Pages[0];
end;

function TdxHeaderFooterPageController.CreatePageBoundsCalculator: TdxPageBoundsCalculator;
begin
  Result := TdxHeaderFooterPageBoundsCalculator.Create(Self);
end;

function TdxHeaderFooterPageController.CreateHitTestCalculator(const ARequest: TdxRichEditHitTestRequest; AResult: TdxRichEditHitTestResult): TdxBoxHitTestCalculator;
begin
  Result := TdxPrintLayoutViewBoxHitTestCalculator.Create(ARequest, AResult);
end;

procedure TdxHeaderFooterPageController.ClearFloatingObjectsLayout;
begin
end;

procedure TdxHeaderFooterPageController.ClearParagraphFramesLayout;
begin
end;

{ TdxHeaderFooterPageBoundsCalculator }

constructor TdxHeaderFooterPageBoundsCalculator.Create(AController: TdxHeaderFooterPageController);
begin
  inherited Create(AController.DocumentLayout.DocumentModel.ToDocumentLayoutUnitConverter);
  Assert(AController <> nil);
end;

{ TdxHeaderFooterColumnsBoundsCalculator }

procedure TdxHeaderFooterColumnsBoundsCalculator.PopulateColumnsBounds(const AResult: TdxRectList;
  const ABounds: TRect; const AColumnInfoCollection: TdxColumnInfoCollection);
begin
  AResult.Add(ABounds);
end;

procedure TdxHeaderFooterColumnsBoundsCalculator.PopulateEqualWidthColumnsBounds(const AResult: TdxRectList;
  const ABounds: TRect; AColumnCount, ASpaceBetweenColumns: Integer);
begin
  AResult.Add(ABounds);
end;

{ TdxHeaderFooterColumnController }

function TdxHeaderFooterColumnController.GetPageLastRunIndex: TdxRunIndex;
begin
  Result := MaxInt;
end;

function TdxHeaderFooterColumnController.CalculateColumnBoundsCore(AColumnIndex: Integer): TRect;
begin
  Result := ColumnsBounds[0];
end;

function TdxHeaderFooterColumnController.CreateColumnBoundsCalculator: TdxColumnsBoundsCalculator;
begin
  Result := TdxHeaderFooterColumnsBoundsCalculator.Create(
    PageAreaController.PageController.DocumentLayout.DocumentModel.ToDocumentLayoutUnitConverter);
end;

{ TdxHeaderFooterPageAreaController }

constructor TdxHeaderFooterPageAreaController.Create(APageController: TdxPageController);
begin
  inherited Create(APageController);
  FAreas := TdxPageAreaCollection.Create;
  FAreas.Add(TdxPageArea.Create(PageController.PieceTable.ContentType, PageController.DocumentModel.Sections.First));
end;

destructor TdxHeaderFooterPageAreaController.Destroy;
begin
  FAreas.Free;
  inherited Destroy;
end;

function TdxHeaderFooterPageAreaController.GetAreas: TdxPageAreaCollection;
begin
  Result := FAreas;
end;

function TdxHeaderFooterPageAreaController.GetCurrentAreaBounds: TRect;
begin
  Result := Areas[0].Bounds;
end;

function TdxHeaderFooterPageAreaController.CompleteCurrentAreaFormatting: TdxCompleteFormattingResult;
begin
  Result := TdxCompleteFormattingResult.Success;
end;

function TdxHeaderFooterPageAreaController.GetNextPageArea(AKeepFloatingObjects: Boolean): TdxPageArea;
begin
  Result := Areas[0];
end;

procedure TdxHeaderFooterPageAreaController.Reset(ASection: TdxSection);
begin
end;

procedure TdxHeaderFooterPageAreaController.BeginSectionFormatting(ASection: TdxSection; ACurrentColumnsCount: Integer);
begin
end;

procedure TdxHeaderFooterPageAreaController.RestartFormattingFromTheStartOfSection(ASection: TdxSection; ACurrentAreaIndex: Integer);
begin
end;

procedure TdxHeaderFooterPageAreaController.RestartFormattingFromTheMiddleOfSection(ASection: TdxSection; ACurrentAreaIndex: Integer);
begin
end;

procedure TdxHeaderFooterPageAreaController.ClearInvalidatedContent(const APos: TdxFormatterPosition);
begin
end;

function TdxHeaderFooterPageAreaController.CreateDefaultState(ACurrentAreaIndex: Integer): TdxPageAreaControllerState;
begin
  Result := nil;
end;

procedure TdxHeaderFooterPageAreaController.SwitchToState(AState: TdxPageAreaControllerState);
begin
end;

{ TdxHeaderFooterRowsController }

function TdxHeaderFooterRowsController.CanRestartDueFloatingObject: Boolean;
begin
  Result := False;
end;

{ TdxHeaderFooterDocumentFormattingController }

constructor TdxHeaderFooterDocumentFormattingController.Create(ADocumentLayout: TdxDocumentLayout;
  AFloatingObjectsLayout: TdxFloatingObjectsLayout; AParagraphFramesLayout: TdxParagraphFramesLayout;
  APieceTable: TdxPieceTable; AArea: TdxPageArea; ASectionIndex: TdxSectionIndex; APageNumberSource: TdxPage);
var
  AController: TdxHeaderFooterPageController;
begin
  inherited Create(ADocumentLayout, APieceTable, AFloatingObjectsLayout, AParagraphFramesLayout);
  Assert(AArea <> nil);
  PageAreaController.Areas.Extract(AArea);
  PageAreaController.Areas.Clear;
  PageAreaController.Areas.Add(AArea);
  AController := TdxHeaderFooterPageController(PageController);
  AController.PageNumberSource := APageNumberSource;
  AController.SetFixedSectionIndex(ASectionIndex);
  Reset(False);
end;

function TdxHeaderFooterDocumentFormattingController.CreatePageController: TdxPageController;
begin
  Result := TdxHeaderFooterPageController.Create(DocumentLayout, FloatingObjectsLayout, ParagraphFramesLayout, PieceTable);
end;

function TdxHeaderFooterDocumentFormattingController.CreatePageAreaController: TdxPageAreaController;
begin
  Result := TdxHeaderFooterPageAreaController.Create(PageController);
end;

function TdxHeaderFooterDocumentFormattingController.CreateColumnController: TdxColumnController;
begin
  Result := TdxHeaderFooterColumnController.Create(PageAreaController);
end;

function TdxHeaderFooterDocumentFormattingController.CreateRowController: TdxRowsController;
begin
  Result := TdxHeaderFooterRowsController.Create(PieceTable, ColumnController,
    DocumentModel.LayoutOptions.PrintLayoutView.MatchHorizontalTableIndentsToTextEdge);
end;

procedure TdxHeaderFooterDocumentFormattingController.NotifyDocumentFormattingComplete;
begin
end;

{ TdxSimplePieceTablePrimaryFormatter }

constructor TdxSimplePieceTablePrimaryFormatter.Create(APieceTable: TdxPieceTable; AMeasurer: TdxBoxMeasurer;
  ARowsController: TdxRowsController; const AVisibleTextFilter: TdxVisibleTextFilterBase; APage: TdxPage);
begin
  inherited Create;
  Assert(APieceTable <> nil);
  Assert(AMeasurer <> nil);
  Assert(ARowsController <> nil);
  Assert(APage <> nil);
  FPieceTable := APieceTable;

  FRowsController := ARowsController;

  FPage := APage;
end;

function TdxSimplePieceTablePrimaryFormatter.GetDocumentModel: TdxDocumentModel;
begin
  Result := FPieceTable.DocumentModel;
end;

function TdxSimplePieceTablePrimaryFormatter.Format(AMaxHeight: Integer; AController: TdxDocumentFormattingController): Integer;
var
  ADocumentFormatter: TdxDocumentFormatter;
  AResult: TdxFormattingProcessResult;
  AArea: TdxPageArea;
begin
  ADocumentFormatter := TdxDocumentFormatter.Create(AController);
  try
    if FRowsController.FrameParagraphIndex <> -1 then
      ADocumentFormatter.ParagraphIndex := FRowsController.FrameParagraphIndex;
    ADocumentFormatter.ParagraphFormatter.PageNumberSource := Page.PageNumberSource;
    ADocumentFormatter.ParagraphFormatter.MaxHeight := AMaxHeight;
    repeat
      AResult := ADocumentFormatter.FormatNextRow;
      if ADocumentFormatter.ParagraphFormatter.ForceFormattingComplete then
        Break;
    until not (AResult.FormattingProcess <> TdxFormattingProcess.Finish);
    if FRowsController.FrameParagraphIndex <> -1 then
      FRowsController.FrameParagraphIndex := ADocumentFormatter.ParagraphIndex;
    AArea := AController.PageController.Pages.Last.Areas.Last;
    if AArea <> nil then
      if AArea.Columns.Last.Rows.Count = 0 then
        AArea.Columns.Delete(AArea.Columns.Count - 1);
    if FRowsController.CurrentRow.Boxes.Count = 0 then
      Exit(FRowsController.CurrentRow.Bounds.Top)
    else
      Exit(FRowsController.CurrentRow.Bounds.Bottom);
  finally
    ADocumentFormatter.Free;
  end;
end;

{ TdxPageHeaderFooterFormatterBase }

constructor TdxPageHeaderFooterFormatterBase<TModelObject, TLayoutObject>.Create(ADocumentLayout: TdxDocumentLayout;
  AFloatingObjectsLayout: TdxFloatingObjectsLayout; AParagraphFramesLayout: TdxParagraphFramesLayout; APage: TdxPage;
  ASectionIndex: TdxSectionIndex);
begin
  inherited Create;
  Assert(ADocumentLayout <> nil);
  Assert(APage <> nil);
  Assert(AFloatingObjectsLayout <> nil);
  Assert(AParagraphFramesLayout <> nil);

  FDocumentLayout := ADocumentLayout;
  FFloatingObjectsLayout := AFloatingObjectsLayout;
  FParagraphFramesLayout := AParagraphFramesLayout;
  FPage := APage;
  FSectionIndex := ASectionIndex;
  FSection := TdxDocumentModel(ADocumentLayout.DocumentModel).Sections[ASectionIndex];
end;

procedure TdxPageHeaderFooterFormatterBase<TModelObject, TLayoutObject>.Format(AFirstPageOfSection: Boolean);
var
  APieceTable: TModelObject;
  AArea: TLayoutObject;
  AAvailableAreaBounds, AInitialAreaBounds, ANewAreaBounds, AOldPageClientBounds, ANewPageClientBounds: TRect;
  AActualAreaBottom, ADeltaY: Integer;
begin
  APieceTable := GetActualModelObject(AFirstPageOfSection);
  if APieceTable = nil then
    Exit;
  AArea := CreatePageArea(APieceTable);
  AppendPageAreaToPage(AArea);

  AAvailableAreaBounds := CalculateAvailableAreaBounds;
  AInitialAreaBounds := AAvailableAreaBounds;
  AInitialAreaBounds.Height := MaxInt div 4;
  AArea.Bounds := AInitialAreaBounds;
  AActualAreaBottom := FormatArea(APieceTable, AArea, AInitialAreaBounds);
  AArea.Bounds := AAvailableAreaBounds;
  ANewAreaBounds := CalculateFinalAreaBounds(AArea, AActualAreaBottom);
  ADeltaY := ANewAreaBounds.Y - AArea.Bounds.Y;
  AArea.MoveVertically(ADeltaY);
  MoveParagraphFramesVertically(TdxPieceTable(APieceTable.PieceTable), ADeltaY);
  MoveFloatingObjectsVertically(TdxPieceTable(APieceTable.PieceTable), ADeltaY, ShouldMoveFloatingObjectVertically);

  AArea.ContentBounds := ANewAreaBounds;
  ANewAreaBounds := AdjustNewAreaBounds(TdxPieceTable(APieceTable.PieceTable), ANewAreaBounds);

  AArea.Bounds := ANewAreaBounds;
  AArea.Columns.First.Bounds := ANewAreaBounds;
  AOldPageClientBounds := FPage.ClientBounds;
  FPage.ClientBounds := CalculateFinalAdjustedPageClientBounds(AArea);
  ANewPageClientBounds := FPage.ClientBounds;
  ADeltaY := CalculateDeltaAfterAdjustPageClientBounds(AOldPageClientBounds, ANewPageClientBounds, AArea.Columns.First.Bounds);
  if ADeltaY <> 0 then
    MoveFloatingObjectsAfterAdjustPageClientBounds(TdxPieceTable(APieceTable.PieceTable), ADeltaY);
end;

procedure TdxPageHeaderFooterFormatterBase<TModelObject, TLayoutObject>.MoveFloatingObjectsAfterAdjustPageClientBounds(
  APieceTable: TdxPieceTable; ADeltaY: Integer);
var
  AObjects: TdxFloatingObjectBoxList;
  ACount, I, AHalfOfDeltaY: Integer;
  ABox: TdxFloatingObjectBox;
begin
  AObjects := FloatingObjectsLayout.GetFloatingObjects(APieceTable);
  try
    ACount := AObjects.Count;
    for I := 0 to ACount - 1 do
    begin
      ABox := AObjects[I];
      if ShouldMoveFloatingObjectVerticallyAfterAdjustPageClientBounds(ABox) then
      begin
        if ABox.GetFloatingObjectRun.FloatingObjectProperties.VerticalPositionAlignment = TdxFloatingObjectVerticalPositionAlignment.Center then
        begin
          AHalfOfDeltaY := ADeltaY div 2;
          ABox.MoveVerticallyCore(AHalfOfDeltaY);
        end
        else
          ABox.MoveVerticallyCore(ADeltaY);
      end;
    end;
  finally
    AObjects.Free;
  end;
end;

procedure TdxPageHeaderFooterFormatterBase<TModelObject, TLayoutObject>.ApplyExistingAreaBounds;
var
  AExistingArea: TLayoutObject;
begin
  AExistingArea := GetExistingArea(FPage);
  if AExistingArea <> nil then
    FPage.ClientBounds := CalculateFinalAdjustedPageClientBounds(AExistingArea);
end;

procedure TdxPageHeaderFooterFormatterBase<TModelObject, TLayoutObject>.MoveParagraphFramesVertically(
  APieceTable: TdxPieceTable; ADeltaY: Integer);
var
  I: Integer;
  ABox: TdxParagraphFrameBox;
begin
  for I := 0 to FParagraphFramesLayout.Items.Count - 1 do
  begin
    ABox := FParagraphFramesLayout.Items[I];
    if ABox.PieceTable.ContentType.IsFooter then
      ABox.MoveVertically(ADeltaY);
  end;
end;

procedure TdxPageHeaderFooterFormatterBase<TModelObject, TLayoutObject>.MoveFloatingObjectsVertically(
  APieceTable: TdxPieceTable; ADeltaY: Integer; ACheckMoveFunc: TShouldMoveBoxVertically);
var
  AObjects: TdxFloatingObjectBoxList;
  ACount, I: Integer;
  ABox: TdxFloatingObjectBox;
begin
  AObjects := FFloatingObjectsLayout.GetFloatingObjects(APieceTable);
  try
    ACount := AObjects.Count;
    for I := 0 to ACount - 1 do
    begin
      ABox := AObjects[I];
      if ACheckMoveFunc(ABox) then
        ABox.MoveVertically(ADeltaY);
    end;
  finally
    AObjects.Free;
  end;
end;

function TdxPageHeaderFooterFormatterBase<TModelObject, TLayoutObject>.ShouldMoveFloatingObjectVertically(ABox: TdxFloatingObjectBox): Boolean;
var
  ARun: TdxFloatingObjectAnchorRun;
  AVerticalPositionType: TdxFloatingObjectVerticalPositionType;
begin
  ARun := ABox.GetFloatingObjectRun;
  AVerticalPositionType := ARun.FloatingObjectProperties.VerticalPositionType;
  Result := AVerticalPositionType in
    [TdxFloatingObjectVerticalPositionType.Line, TdxFloatingObjectVerticalPositionType.Margin, TdxFloatingObjectVerticalPositionType.Paragraph];
end;

function TdxPageHeaderFooterFormatterBase<TModelObject, TLayoutObject>.FormatArea(AContentType: TModelObject; AArea: TLayoutObject; const AAvailableAreaBounds: TRect): Integer;
var
  AMeasurer: TdxBoxMeasurer;
  AOldPieceTable, APieceTable: TdxPieceTable;
  AController: TdxHeaderFooterDocumentFormattingController;
  AFakePage: TdxPage;
  AFormatter: TdxSimplePieceTablePrimaryFormatter;
begin
  AMeasurer := DocumentLayout.Measurer;
  AOldPieceTable := TdxPieceTable(AMeasurer.PieceTable);
  try
    APieceTable := TdxPieceTable(AContentType.PieceTable);
    AMeasurer.PieceTable := APieceTable;
    AController := TdxHeaderFooterDocumentFormattingController.Create(DocumentLayout, FFloatingObjectsLayout, FParagraphFramesLayout, APieceTable, AArea, SectionIndex, FPage);
    try
      AFakePage := AController.PageController.Pages.Last;
      AFakePage.Bounds := FPage.Bounds;
      AFakePage.ClientBounds := FPage.ClientBounds;

      AFormatter := TdxSimplePieceTablePrimaryFormatter.Create(APieceTable, AMeasurer,
        AController.RowsController, nil, Page);
      try
        Exit(AFormatter.Format(AAvailableAreaBounds.Bottom, AController));
      finally
        AFormatter.Free;
      end;
    finally
      AController.PageAreaController.Areas.Extract(AArea);
      AController.Free;
    end;
  finally
    AMeasurer.PieceTable := AOldPieceTable;
  end;
end;

function TdxPageHeaderFooterFormatterBase<TModelObject, TLayoutObject>.GetPageAreaBottom(AArea: TdxPageArea; AActualAreaBottom: Integer): Integer;
var
  AInitialBottom: Integer;
  AColumn: TdxColumn;
  ARow: TdxRow;
begin
  AInitialBottom := AArea.Bounds.Bottom;
  if AActualAreaBottom > AInitialBottom then
  begin
    AColumn := AArea.Columns.First;
    if AColumn <> nil then
    begin
      ARow := FindRow(AColumn, AInitialBottom);
      if ARow <> nil then
        Exit(ARow.Bounds.Bottom);
    end;
    Exit(AInitialBottom);
  end
  else
    Exit(AActualAreaBottom);
end;

function TdxPageHeaderFooterFormatterBase<TModelObject, TLayoutObject>.FindRow(AColumn: TdxColumn; Y: Integer): TdxRow;
var
  ATables: TdxTableViewInfoCollection;
  ATable: TdxTableViewInfo;
begin
  ATables := AColumn.InnerTables;
  if (ATables <> nil) and (ATables.Count > 0) then
  begin
    ATable := FindTable(ATables, Y);
    if ATable <> nil then
      Exit(FindRow(ATable, Y));
  end;
  Result := FindRow(AColumn.Rows, Y);
end;

function TdxPageHeaderFooterFormatterBase<TModelObject, TLayoutObject>.FindTable(
  ATables: TdxTableViewInfoCollection; Y: Integer): TdxTableViewInfo;
var
  ATable: TdxTableViewInfo;
  ATableBottom: Integer;
  I: Integer;
begin
  for I := 0 to ATables.Count - 1 do
  begin
    ATable := ATables[I];
    ATableBottom := ATable.GetActualBottomPosition;
    if (ATable.Anchors.First.VerticalPosition <= Y) and (ATableBottom >= Y) then
      Exit(ATable);
  end;
  Result := nil;
end;

function TdxPageHeaderFooterFormatterBase<TModelObject, TLayoutObject>.FindRow(ATable: TdxTableViewInfo; Y: Integer): TdxRow;
var
  ATableRowIndex: Integer;
  ARowViewInfo: TdxTableRowViewInfoBase;
  ACellViewInfo: TdxTableCellViewInfo;
  ATableCellRows: TdxRowCollection;
  ARow: TdxRow;
  AComparable: TdxTableRowAnchorComparable;
  I: Integer;
begin
  AComparable := TdxTableRowAnchorComparable.Create(Y, ATable.Table.Rows.Last);
  try
    if not TdxAlgorithms1<TdxTableRowViewInfoBase>.BinarySearch(ATable.Rows, AComparable, ATableRowIndex) then
      Exit(nil);
  finally
    AComparable.Free;
  end;

  ARowViewInfo := ATable.Rows[ATableRowIndex];
  for I := 0 to ARowViewInfo.Cells.Count - 1 do
  begin
    ACellViewInfo := ARowViewInfo.Cells[I];
    ATableCellRows := ACellViewInfo.GetRows(ATable.Column);
    try
      ARow := FindRow(ATableCellRows, Y);
      if ARow <> nil then
        Exit(ARow);
    finally
      ATableCellRows.Free;
    end;
  end;
  Result := nil;
end;

function TdxPageHeaderFooterFormatterBase<TModelObject, TLayoutObject>.FindRow(ARows: TdxRowCollection; Y: Integer): TdxRow;
var
  ARowIndex: Integer;
  AComparable: TdxBoxAndPointYComparable;
begin
  AComparable := TdxBoxAndPointYComparable.Create(TPoint.Create(0, Y));
  try
    if TdxAlgorithms1<TdxBoxBase>.BinarySearch(ARows, AComparable, ARowIndex) then
      Result := ARows[ARowIndex]
    else
      Result := nil;
  finally
    AComparable.Free;
  end;
end;

{ TdxPageHeaderFormatter }

function TdxPageHeaderFormatter.GetActualModelObject(AFirstPageOfSection: Boolean): TdxSectionHeader;
begin
  Result := Section.Headers.CalculateActualObject(AFirstPageOfSection, Page.IsEven);
end;

function TdxPageHeaderFormatter.CalculateDeltaAfterAdjustPageClientBounds(
  const AOldPageClientBounds, ANewPageClientBounds, AColumnBounds: TRect): Integer;
begin
  Result := ANewPageClientBounds.Top - AColumnBounds.Top;
end;

function TdxPageHeaderFormatter.GetExistingArea(APage: TdxPage): TdxHeaderPageArea;
begin
  Result := APage.Header;
end;

function TdxPageHeaderFormatter.CreatePageArea(APieceTable: TdxSectionHeader): TdxHeaderPageArea;
begin
  Result := TdxHeaderPageArea.Create(APieceTable, Section);
end;

procedure TdxPageHeaderFormatter.AppendPageAreaToPage(AArea: TdxHeaderPageArea);
begin
  Page.Header := AArea;
end;

function TdxPageHeaderFormatter.CalculateAvailableAreaBounds: TRect;
var
  APageClientBounds, ABounds: TRect;
  AMaxBottom, AMaxHeight: Integer;
begin
  APageClientBounds := Page.ClientBounds;
  ABounds := APageClientBounds;
  ABounds.Y := DocumentLayout.DocumentModel.ToDocumentLayoutUnitConverter.ToLayoutUnits(Section.Margins.HeaderOffset);
  AMaxBottom := APageClientBounds.Y + APageClientBounds.Height div 3;
  AMaxHeight := AMaxBottom - ABounds.Y;
  ABounds.Height := AMaxHeight;
  Result := ABounds;
end;

function TdxPageHeaderFormatter.CalculateFinalAreaBounds(AArea: TdxHeaderPageArea; AActualAreaBottom: Integer): TRect;
var
  AAreaBottom: Integer;
begin
  Result := AArea.Bounds;
  AAreaBottom := GetPageAreaBottom(AArea, AActualAreaBottom);
  Result.Height := Math.Max(0, AAreaBottom - AArea.Bounds.Y);
end;

function TdxPageHeaderFormatter.CalculateFinalAdjustedPageClientBounds(AArea: TdxHeaderPageArea): TRect;
var
  ADeltaClientTop: Integer;
begin
  if Section.Margins.Top < 0 then
    Exit(Page.ClientBounds);
  Result := Page.ClientBounds;
  ADeltaClientTop := Math.Max(0, AArea.Bounds.Bottom - Result.Top);
  Result.Y := Result.Y + ADeltaClientTop;
  Result.Height := Result.Height - ADeltaClientTop;
end;

function TdxPageHeaderFormatter.AdjustNewAreaBounds(APieceTable: TdxPieceTable; const ANewAreaBounds: TRect): TRect;
begin
  Result := ANewAreaBounds;
end;

function TdxPageHeaderFormatter.ShouldMoveFloatingObjectVerticallyAfterAdjustPageClientBounds(ABox: TdxFloatingObjectBox): Boolean;
var
  ARun: TdxFloatingObjectAnchorRun;
  AVerticalPositionType: TdxFloatingObjectVerticalPositionType;
  AVerticalPositionAlignment: TdxFloatingObjectVerticalPositionAlignment;
begin
  ARun := ABox.GetFloatingObjectRun;
  AVerticalPositionType := ARun.FloatingObjectProperties.VerticalPositionType;
  AVerticalPositionAlignment := ARun.FloatingObjectProperties.VerticalPositionAlignment;
  Result :=
    ((AVerticalPositionType = TdxFloatingObjectVerticalPositionType.Margin) and
     (AVerticalPositionAlignment <> TdxFloatingObjectVerticalPositionAlignment.Bottom) and
     (AVerticalPositionAlignment <> TdxFloatingObjectVerticalPositionAlignment.Outside)) or
    ((AVerticalPositionType = TdxFloatingObjectVerticalPositionType.TopMargin) and
     (AVerticalPositionAlignment <> TdxFloatingObjectVerticalPositionAlignment.Bottom) and
     (AVerticalPositionAlignment <> TdxFloatingObjectVerticalPositionAlignment.Inside)) or
    ((AVerticalPositionType = TdxFloatingObjectVerticalPositionType.InsideMargin) and
     (AVerticalPositionAlignment <> TdxFloatingObjectVerticalPositionAlignment.Bottom) and
     (AVerticalPositionAlignment <> TdxFloatingObjectVerticalPositionAlignment.Inside));
end;

{ TdxPageFooterFormatter }

function TdxPageFooterFormatter.GetActualModelObject(AFirstPageOfSection: Boolean): TdxSectionFooter;
begin
  Result := Section.Footers.CalculateActualObject(AFirstPageOfSection, Page.IsEven);
end;

function TdxPageFooterFormatter.CalculateDeltaAfterAdjustPageClientBounds(
  const AOldPageClientBounds, ANewPageClientBounds, AColumnBounds: TRect): Integer;
begin
  Result := ANewPageClientBounds.Bottom - AOldPageClientBounds.Bottom;
end;

procedure TdxPageFooterFormatter.MoveFloatingObjectsAfterAdjustPageClientBounds(APieceTable: TdxPieceTable; ADeltaY: Integer);
begin
  inherited MoveFloatingObjectsAfterAdjustPageClientBounds(APieceTable, ADeltaY);
  if Page.Header <> nil then
    inherited MoveFloatingObjectsAfterAdjustPageClientBounds(TdxPieceTable(Page.Header.Header.PieceTable), ADeltaY);
end;

function TdxPageFooterFormatter.ShouldMoveFloatingObjectVerticallyAfterAdjustPageClientBounds(ABox: TdxFloatingObjectBox): Boolean;
var
  ARun: TdxFloatingObjectAnchorRun;
  AVerticalPositionType: TdxFloatingObjectVerticalPositionType;
  AVerticalPositionAlignment: TdxFloatingObjectVerticalPositionAlignment;
begin
  ARun := ABox.GetFloatingObjectRun;
  AVerticalPositionType := ARun.FloatingObjectProperties.VerticalPositionType;
  AVerticalPositionAlignment := ARun.FloatingObjectProperties.VerticalPositionAlignment;
  Result :=
  ((AVerticalPositionType = TdxFloatingObjectVerticalPositionType.Margin) and (AVerticalPositionAlignment = TdxFloatingObjectVerticalPositionAlignment.Bottom)) or
  ((AVerticalPositionType = TdxFloatingObjectVerticalPositionType.Margin) and (AVerticalPositionAlignment = TdxFloatingObjectVerticalPositionAlignment.Center)) or
  ((AVerticalPositionType = TdxFloatingObjectVerticalPositionType.Margin) and (AVerticalPositionAlignment = TdxFloatingObjectVerticalPositionAlignment.Outside)) or
  ((AVerticalPositionType = TdxFloatingObjectVerticalPositionType.BottomMargin) and
   (AVerticalPositionAlignment <> TdxFloatingObjectVerticalPositionAlignment.Bottom) and
   (AVerticalPositionAlignment <> TdxFloatingObjectVerticalPositionAlignment.Outside)) or
  ((AVerticalPositionType = TdxFloatingObjectVerticalPositionType.OutsideMargin) and
   (AVerticalPositionAlignment <> TdxFloatingObjectVerticalPositionAlignment.Bottom) and
   (AVerticalPositionAlignment <> TdxFloatingObjectVerticalPositionAlignment.Outside));
end;

function TdxPageFooterFormatter.GetExistingArea(APage: TdxPage): TdxFooterPageArea;
begin
  Result := APage.Footer;
end;

function TdxPageFooterFormatter.CreatePageArea(APieceTable: TdxSectionFooter): TdxFooterPageArea;
begin
  Result := TdxFooterPageArea.Create(APieceTable, Section);
end;

procedure TdxPageFooterFormatter.AppendPageAreaToPage(AArea: TdxFooterPageArea);
begin
  Page.Footer := AArea;
end;

function TdxPageFooterFormatter.CalculateAvailableAreaBounds: TRect;
var
  APageClientBounds, ABounds: TRect;
  AMaxHeight: Integer;
begin
  APageClientBounds := Page.ClientBounds;
  ABounds := APageClientBounds;
  ABounds.Y := APageClientBounds.Bottom - APageClientBounds.Height div 3;
  AMaxHeight := (Page.Bounds.Bottom - DocumentLayout.DocumentModel.ToDocumentLayoutUnitConverter.ToLayoutUnits(Section.Margins.FooterOffset)) - ABounds.Y;
  ABounds.Height := AMaxHeight;
  Result := ABounds;
end;

function TdxPageFooterFormatter.CalculateFinalAreaBounds(AArea: TdxFooterPageArea; AActualAreaBottom: Integer): TRect;
var
  AInitialBottom, AAreaBottom: Integer;
begin
  Result := AArea.Bounds;
  AInitialBottom := Result.Bottom;

  AAreaBottom := GetPageAreaBottom(AArea, AActualAreaBottom);
  Result.Height := AAreaBottom - Result.Y;
  Result.Y := AInitialBottom - Result.Height;
end;

procedure TdxPageFooterFormatter.MoveFloatingObjectsVertically(APieceTable: TdxPieceTable; ADeltaY: Integer;
  ACheckMoveFunc: TShouldMoveBoxVertically);
var
  AFloatingObjectsBottom, APageBottom: Integer;
begin
  AFloatingObjectsBottom := CalculateFloatingObjectsBottomWithDelta(APieceTable, ADeltaY);
  APageBottom := Page.Bounds.Bottom;
  if AFloatingObjectsBottom > APageBottom then
    Dec(ADeltaY, AFloatingObjectsBottom - APageBottom);
  inherited MoveFloatingObjectsVertically(APieceTable, ADeltaY, ACheckMoveFunc);
end;

function TdxPageFooterFormatter.AdjustNewAreaBounds(APieceTable: TdxPieceTable; const ANewAreaBounds: TRect): TRect;
var
  AFloatingObjectsBottom, ABottom: Integer;
begin
  AFloatingObjectsBottom := CalculateFloatingObjectsBottom(APieceTable, ANewAreaBounds.Top);
  ABottom := Math.Min(Page.Bounds.Bottom, Math.Max(ANewAreaBounds.Bottom, AFloatingObjectsBottom));
  ANewAreaBounds.Height := ABottom - ANewAreaBounds.Top;
  Result := ANewAreaBounds;
end;

function TdxPageFooterFormatter.CalculateFloatingObjectsBottom(APieceTable: TdxPieceTable; AInitialBottom: Integer): Integer;
begin
  Result := CalculateFloatingObjectsBottomWithDelta(APieceTable, AInitialBottom, 0);
end;

function TdxPageFooterFormatter.CalculateFloatingObjectsBottomWithDelta(APieceTable: TdxPieceTable; AInitialBottom: Integer; ADelta: Integer): Integer;
var
  ACount, I: Integer;
  AFloatingObjects: TdxFloatingObjectBoxList;
begin
  Result := AInitialBottom;
  AFloatingObjects := FloatingObjectsLayout.GetFloatingObjects(APieceTable);
  try
    ACount := AFloatingObjects.Count;
    for I := 0 to ACount - 1 do
      if ShouldMoveFloatingObjectVertically(AFloatingObjects[I]) then
        Result := Math.Max(Result, AFloatingObjects[I].Bounds.Bottom + ADelta);
  finally
    AFloatingObjects.Free;
  end;
end;

function TdxPageFooterFormatter.CalculateFloatingObjectsBottomWithDelta(APieceTable: TdxPieceTable; ADelta: Integer): Integer;
begin
  Result := CalculateFloatingObjectsBottomWithDelta(APieceTable, MinInt, ADelta);
end;

function TdxPageFooterFormatter.CalculateFinalAdjustedPageClientBounds(AArea: TdxFooterPageArea): TRect;
begin
  if Section.Margins.Bottom < 0 then
    Exit(Page.ClientBounds);
  Result := Page.ClientBounds;
  Result.Height := Math.Min(AArea.Bounds.Top, Result.Bottom) - Result.Top;
end;

{ TdxPrintLayoutViewPageController }

constructor TdxPrintLayoutViewPageController.Create(ADocumentLayout: TdxDocumentLayout);
begin
  inherited Create(ADocumentLayout, nil, nil);
end;

function TdxPrintLayoutViewPageController.CreatePageBoundsCalculator: TdxPageBoundsCalculator;
begin
  Result := TdxPageBoundsCalculator.Create(DocumentLayout.DocumentModel.ToDocumentLayoutUnitConverter);
end;

procedure TdxPrintLayoutViewPageController.FormatHeader(APage: TdxPage; AFirstPageOfSection: Boolean);
var
  AFormatter: TdxPageHeaderFormatter;
begin
  AFormatter := TdxPageHeaderFormatter.Create(DocumentLayout, FloatingObjectsLayout, ParagraphFramesLayout, APage, CurrentSectionIndex);
  try
    AFormatter.Format(AFirstPageOfSection);
  finally
    AFormatter.Free;
  end;
end;

procedure TdxPrintLayoutViewPageController.FormatFooter(APage: TdxPage; AFirstPageOfSection: Boolean);
var
  AFormatter: TdxPageFooterFormatter;
begin
  AFormatter := TdxPageFooterFormatter.Create(DocumentLayout, FloatingObjectsLayout, ParagraphFramesLayout, APage, CurrentSectionIndex);
  try
    AFormatter.Format(AFirstPageOfSection);
  finally
    AFormatter.Free;
  end;
end;

procedure TdxPrintLayoutViewPageController.ApplyExistingHeaderAreaBounds(APage: TdxPage);
var
  AFormatter: TdxPageHeaderFormatter;
begin
  AFormatter := TdxPageHeaderFormatter.Create(DocumentLayout, FloatingObjectsLayout, ParagraphFramesLayout, APage, CurrentSectionIndex);
  try
    AFormatter.ApplyExistingAreaBounds;
  finally
    AFormatter.Free;
  end;
end;

procedure TdxPrintLayoutViewPageController.ApplyExistingFooterAreaBounds(APage: TdxPage);
var
  AFormatter: TdxPageFooterFormatter;
begin
  AFormatter := TdxPageFooterFormatter.Create(DocumentLayout, FloatingObjectsLayout, ParagraphFramesLayout, APage, CurrentSectionIndex);
  try
    AFormatter.ApplyExistingAreaBounds;
  finally
    AFormatter.Free;
  end;
end;

function TdxPrintLayoutViewPageController.CreateHitTestCalculator(const ARequest: TdxRichEditHitTestRequest; AResult: TdxRichEditHitTestResult): TdxBoxHitTestCalculator;
begin
  Result := TdxPrintLayoutViewBoxHitTestCalculator.Create(ARequest, AResult);
end;

{ TdxPrintLayoutViewColumnController }

function TdxPrintLayoutViewColumnController.CalculateColumnBoundsCore(AColumnIndex: Integer): TRect;
begin
  Result := ColumnsBounds[AColumnIndex];
end;

{ TdxPrintLayoutViewPageAreaController }

procedure TdxPrintLayoutViewPageAreaController.Reset(ASection: TdxSection);
begin
  SwitchToDefaultState;
  inherited Reset(ASection);
end;

procedure TdxPrintLayoutViewPageAreaController.RestartFormattingFromTheStartOfSection(ASection: TdxSection; ACurrentAreaIndex: Integer);
begin
  SwitchToDefaultState;
  inherited RestartFormattingFromTheStartOfSection(ASection, ACurrentAreaIndex);
end;

procedure TdxPrintLayoutViewPageAreaController.RestartFormattingFromTheMiddleOfSection(ASection: TdxSection; ACurrentAreaIndex: Integer);
begin
  SwitchToDefaultState;
  inherited RestartFormattingFromTheMiddleOfSection(ASection, ACurrentAreaIndex);
end;

procedure TdxPrintLayoutViewPageAreaController.SwitchToDefaultState;
var
  ANextState: TdxPageAreaControllerState;
begin
  ANextState := CreateDefaultState(0);
  SwitchToState(ANextState);
end;

function TdxPrintLayoutViewPageAreaController.CreateDefaultState(ACurrentAreaIndex: Integer): TdxPageAreaControllerState;
begin
  Result := TdxPrintLayoutViewDefaultPageAreaControllerState.Create(Self, ACurrentAreaIndex);
end;

{ TdxFinishContinuousSectionStartPageAreaControllerState }

constructor TdxFinishContinuousSectionStartPageAreaControllerState.Create(AOwner: TdxPageAreaController; const AAreaBounds: TRect);
begin
  inherited Create(AOwner);
  FAreaBounds := AAreaBounds;
  CreateCurrentAreaBounds;
end;

function TdxFinishContinuousSectionStartPageAreaControllerState.CompleteCurrentAreaFormatting: TdxCompleteFormattingResult;
begin
  if Owner.PageController.Pages.Last = nil then
    Result := Owner.PageController.CompleteCurrentPageFormatting
  else
    Result := TdxCompleteFormattingResult.Success;
end;

function TdxFinishContinuousSectionStartPageAreaControllerState.GetNextPageArea(AKeepFloatingObjects: Boolean): TdxPageArea;
var
  ANewPageArea: TdxPageArea;
begin
  if Owner.PageController.Pages.Last = nil then
    Owner.PageController.GetNextPage(AKeepFloatingObjects);
  Assert(FGetNextPageAreaCount = 0);
  Inc(FGetNextPageAreaCount);

  CreateCurrentAreaBounds;
  ANewPageArea := GetNextPageAreaCore;
  Areas.Add(ANewPageArea);
  Result := ANewPageArea;
end;

procedure TdxFinishContinuousSectionStartPageAreaControllerState.ApplySectionStart(ASection: TdxSection;
  ACurrentColumnsCount: Integer);
var
  ANextState: TdxPageAreaControllerState;
begin
  if FGetNextPageAreaCount = 0 then
    Exit;
  ANextState := Owner.CreateDefaultState(Areas.Count - 1);
  Owner.SwitchToState(ANextState);
  ANextState.CreateCurrentAreaBounds;
end;

function TdxFinishContinuousSectionStartPageAreaControllerState.CreateCurrentAreaBoundsCore: TRect;
begin
  Result := FAreaBounds;
end;

{ TdxHeaderFooterDecoratorPainter }

constructor TdxHeaderFooterDecoratorPainter.Create(AView: TdxPrintLayoutView; const AOwner: IdxHeaderFooterDecoratorPainterOwner);
begin
  inherited Create;
  FRoundedRectangleRadius := 6;
  FView := AView;
  FOwner := AOwner;
  FControl := TdxVCLControl(AView.Control.Control);
  FHeaderFooterLinePen := CreateHeaderFooterLinePen;
  FHeaderFooterMarkFont := CreateHeaderFooterCaptionFont;
end;

destructor TdxHeaderFooterDecoratorPainter.Destroy;
begin
  FHeaderFooterMarkFont.Free;
  FHeaderFooterLinePen.Free;
  inherited Destroy;
end;

function TdxHeaderFooterDecoratorPainter.GetView: TdxPrintLayoutView;
begin
  Result := FView;
end;

function TdxHeaderFooterDecoratorPainter.GetOwner: IdxHeaderFooterDecoratorPainterOwner;
begin
  Result := FOwner;
end;

function TdxHeaderFooterDecoratorPainter.GetDocumentModel: TdxDocumentModel;
begin
  Result := RichEditControl.DocumentModel;
end;

function TdxHeaderFooterDecoratorPainter.GetRichEditControl: IdxRichEditControl;
begin
  Result := FView.Control;
end;

procedure TdxHeaderFooterDecoratorPainter.DrawDecorators(APainter: TdxPainter; AViewInfos: TdxPageViewInfoCollection);
var
  ACount, I: Integer;
begin
  ACount := AViewInfos.Count;
  for I := 0 to ACount - 1 do
  begin
    FCurrentPageInfo := AViewInfos[I];
    DrawPageDecorators(AViewInfos[I].Page, APainter);
    FCurrentPageInfo := nil;
  end;
end;

procedure TdxHeaderFooterDecoratorPainter.DrawPageDecorators(APage: TdxPage; APainter: TdxPainter);
begin
  if DocumentModel.ActivePieceTable.IsHeaderFooter then
  begin
    if APage.Header <> nil then
      DrawHeaderFrame(APage, APage.Header, APainter);
    if APage.Footer <> nil then
      DrawFooterFrame(APage, APage.Footer, APainter);
  end;
end;

procedure TdxHeaderFooterDecoratorPainter.DrawHeaderFrame(APage: TdxPage; AArea: TdxPageArea; APainter: TdxPainter);
var
  ABounds, ABaseBounds: TRect;
  AGdiPlusPainter: TdxGdiPlusPainter;
  AHeaderFooter: TdxSectionHeaderFooterBase;
  ACaption: string;
begin
  ABounds := APage.Bounds;
  ABounds.Y := AArea.Bounds.Bottom;
  ABounds := DrawHeaderFooterLine(APainter, ABounds);

  AGdiPlusPainter := TdxGdiPlusPainter(APainter);

  AHeaderFooter := TdxSectionHeaderFooterBase(AArea.PieceTable.ContentType);
  ACaption := GetCaption(AHeaderFooter, APage);

  ABaseBounds := CalculateHeaderCaptionBaseBounds(ACaption, ABounds, AGdiPlusPainter);
  DrawHeaderCaptionBackground(ABaseBounds, AGdiPlusPainter);
  DrawCaptionText(ACaption, ABaseBounds, AGdiPlusPainter, FRoundedRectangleRadius div 2);

  if AArea.Section.Headers.IsLinkedToPrevious(AHeaderFooter.&Type) then
  begin
    ACaption := cxGetResourceString(@sdxRichEditCaption_SameAsPrevious);
    ABaseBounds := CalculateHeaderLinkCaptionBaseBounds(ACaption, ABounds, AGdiPlusPainter);
    DrawHeaderCaptionBackground(ABaseBounds, AGdiPlusPainter);
    DrawCaptionText(ACaption, ABaseBounds, AGdiPlusPainter, FRoundedRectangleRadius div 2);
  end;
end;

procedure TdxHeaderFooterDecoratorPainter.DrawFooterFrame(APage: TdxPage; AArea: TdxPageArea; APainter: TdxPainter);
var
  ABounds, ABaseBounds: TRect;
  AGdiPlusPainter: TdxGdiPlusPainter;
  AHeaderFooter: TdxSectionHeaderFooterBase;
  ACaption: string;
begin
  ABounds := APage.Bounds;
  ABounds.Y := AArea.Bounds.Top;
  ABounds := DrawHeaderFooterLine(APainter, ABounds);

  AGdiPlusPainter := TdxGdiPlusPainter(APainter);

  AHeaderFooter := TdxSectionHeaderFooterBase(AArea.PieceTable.ContentType);
  ACaption := GetCaption(AHeaderFooter, APage);

  ABaseBounds := CalculateFooterCaptionBaseBounds(ACaption, ABounds, AGdiPlusPainter);
  DrawFooterCaptionBackground(ABaseBounds, AGdiPlusPainter);
  DrawCaptionText(ACaption, ABaseBounds, AGdiPlusPainter, -FRoundedRectangleRadius div 2);

  if AArea.Section.Footers.IsLinkedToPrevious(AHeaderFooter.&Type) then
  begin
    ACaption := cxGetResourceString(@sdxRichEditCaption_SameAsPrevious);
    ABaseBounds := CalculateFooterLinkCaptionBaseBounds(ACaption, ABounds, AGdiPlusPainter);
    DrawFooterCaptionBackground(ABaseBounds, AGdiPlusPainter);
    DrawCaptionText(ACaption, ABaseBounds, AGdiPlusPainter, -FRoundedRectangleRadius div 2);
  end;
end;

function TdxHeaderFooterDecoratorPainter.DrawHeaderFooterLine(APainter: TdxPainter; const AInitialLineBounds: TRect): TRect;
begin
  Result := RichEditControl.GetPixelPhysicalBounds(CurrentPageInfo, AInitialLineBounds);
  Result.Height := 1;
  Result.X := Result.X + Owner.LeftHeaderFooterLineOffset;
  Result.Width := Result.Width - Owner.LeftHeaderFooterLineOffset + Owner.RightHeaderFooterLineOffset;
  APainter.DrawLine(FHeaderFooterLinePen, Result.Left, Result.Top, Result.Right, Result.Top);
end;

function TdxHeaderFooterDecoratorPainter.GetCaption(AHeaderFooter: TdxSectionHeaderFooterBase; APage: TdxPage): string;
var
  ASectionIndex: TdxSectionIndex;
begin
  Result := AHeaderFooter.Caption;
  if DocumentModel.Sections.Count > 1 then
  begin
    ASectionIndex := DocumentModel.Sections.IndexOf(APage.Areas.First.Section);
    Result := Format('%s -Section %d-', [Result, ASectionIndex + 1]);
  end;
end;

function TdxHeaderFooterDecoratorPainter.MeasureString(const ACaption: string; APainter: TdxGdiPlusPainter): TSize;
begin
  Result := cxTextSize(FHeaderFooterMarkFont, ACaption);
end;

function TdxHeaderFooterDecoratorPainter.CalculateHeaderCaptionBaseBounds(const ACaption: string;
  const AHeaderLineBounds: TRect; AGdiPlusPainter: TdxGdiPlusPainter): TRect;
begin
  Result := AHeaderLineBounds;
  Result.Size := MeasureString(ACaption, AGdiPlusPainter);
  Result.X := Result.X + FRoundedRectangleRadius;
end;

function TdxHeaderFooterDecoratorPainter.CalculateHeaderLinkCaptionBaseBounds(const ACaption: string;
  const AHeaderLineBounds: TRect; AGdiPlusPainter: TdxGdiPlusPainter): TRect;
var
  ASize: TSize;
begin
  Result := AHeaderLineBounds;
  ASize := MeasureString(ACaption, AGdiPlusPainter);
  Result.X := Result.Right - ASize.Width - FRoundedRectangleRadius;
  Result.Size := ASize;
end;

function TdxHeaderFooterDecoratorPainter.CalculateFooterCaptionBaseBounds(const ACaption: string;
  const AHeaderLineBounds: TRect; AGdiPlusPainter: TdxGdiPlusPainter): TRect;
begin
  Result := AHeaderLineBounds;
  Result.Size := MeasureString(ACaption, AGdiPlusPainter);
  Result.X := Result.X + FRoundedRectangleRadius;
  Result.Y := Result.Y - Result.Size.Height;
end;

function TdxHeaderFooterDecoratorPainter.CalculateFooterLinkCaptionBaseBounds(const ACaption: string;
  const AFooterLineBounds: TRect; AGdiPlusPainter: TdxGdiPlusPainter): TRect;
var
  ASize: TSize;
begin
  Result := AFooterLineBounds;
  ASize := MeasureString(ACaption, AGdiPlusPainter);
  Result.X := Result.Right - ASize.Width - FRoundedRectangleRadius;
  Result.Size := ASize;
  Result.Y := Result.Y - Result.Size.Height;
end;

procedure TdxHeaderFooterDecoratorPainter.DrawHeaderCaptionBackground(const ABounds: TRect; AGdiPlusPainter: TdxGdiPlusPainter);
var
  APath: TdxGPPath;
begin
  APath := CreateHeaderCaptionBackgroundPath(ABounds);
  try
    DrawHeaderFooterCaptionBackground(APath, AGdiPlusPainter);
  finally
    APath.Free;
  end;
end;

procedure TdxHeaderFooterDecoratorPainter.DrawFooterCaptionBackground(const ABounds: TRect; AGdiPlusPainter: TdxGdiPlusPainter);
var
  APath: TdxGPPath;
begin
  APath := CreateFooterCaptionBackgroundPath(ABounds);
  try
    DrawHeaderFooterCaptionBackground(APath, AGdiPlusPainter);
  finally
    APath.Free;
  end;
end;

function TdxHeaderFooterDecoratorPainter.CreateHeaderCaptionBackgroundPath(const ABounds: TRect): TdxGPPath;
begin
  Result := TdxGPPath.Create;
  Result.AddLine(ABounds.X - FRoundedRectangleRadius, ABounds.Y, ABounds.X - FRoundedRectangleRadius, ABounds.Y + ABounds.Height);
  Result.AddArc(ABounds.X - FRoundedRectangleRadius, ABounds.Y + ABounds.Height - FRoundedRectangleRadius, 2 * FRoundedRectangleRadius, 2 * FRoundedRectangleRadius, 180, -90);
  Result.AddLine(ABounds.X + FRoundedRectangleRadius, ABounds.Y + ABounds.Height + FRoundedRectangleRadius, ABounds.Right - FRoundedRectangleRadius, ABounds.Y + ABounds.Height + FRoundedRectangleRadius);
  Result.AddArc(ABounds.Right - FRoundedRectangleRadius, ABounds.Y + ABounds.Height - FRoundedRectangleRadius, 2 * FRoundedRectangleRadius, 2 * FRoundedRectangleRadius, 90, -90);
  Result.AddLine(ABounds.Right + FRoundedRectangleRadius, ABounds.Y + ABounds.Height - FRoundedRectangleRadius, ABounds.Right + FRoundedRectangleRadius, ABounds.Y);
  Result.AddLine(ABounds.Right + FRoundedRectangleRadius, ABounds.Y, ABounds.X - FRoundedRectangleRadius, ABounds.Y);
end;

function TdxHeaderFooterDecoratorPainter.CreateFooterCaptionBackgroundPath(const ABounds: TRect): TdxGPPath;
begin
  Result := TdxGPPath.Create;
  Result.AddLine(ABounds.X - FRoundedRectangleRadius, ABounds.Y, ABounds.X - FRoundedRectangleRadius, ABounds.Bottom);
  Result.AddLine(ABounds.X - FRoundedRectangleRadius, ABounds.Bottom, ABounds.Right + FRoundedRectangleRadius, ABounds.Bottom);
  Result.AddLine(ABounds.Right + FRoundedRectangleRadius, ABounds.Bottom, ABounds.Right + FRoundedRectangleRadius, ABounds.Y);
  Result.AddArc(ABounds.Right - FRoundedRectangleRadius, ABounds.Y - FRoundedRectangleRadius, 2 * FRoundedRectangleRadius, 2 * FRoundedRectangleRadius, 0, -90);
  Result.AddLine(ABounds.Right - FRoundedRectangleRadius, ABounds.Y - FRoundedRectangleRadius, ABounds.X + FRoundedRectangleRadius, ABounds.Y - FRoundedRectangleRadius);
  Result.AddArc(ABounds.X - FRoundedRectangleRadius, ABounds.Y - FRoundedRectangleRadius, 2 * FRoundedRectangleRadius, 2 * FRoundedRectangleRadius, -90, -90);
end;

procedure TdxHeaderFooterDecoratorPainter.DrawHeaderFooterCaptionBackground(APath: TdxGPPath; AGdiPlusPainter: TdxGdiPlusPainter);
var
  AGraphics: TdxGraphics;
  AOldSmoothingMode: TdxGPSmoothingMode;
begin
  AGraphics := AGdiPlusPainter.Graphics;
  AOldSmoothingMode := AGraphics.SmoothingMode;
  try
    AGraphics.SmoothingMode := TdxGPSmoothingMode.smHighQuality;
    AGraphics.Path(APath, Owner.HeaderFooterMarkTextColor, Owner.HeaderFooterMarkBackColor);
  finally
    AGraphics.SmoothingMode := AOldSmoothingMode;
  end;
end;

procedure TdxHeaderFooterDecoratorPainter.DrawCaptionText(const ACaption: string; ABounds: TRect; AGdiPlusPainter: TdxGdiPlusPainter; AOffset: Integer);
var
  AGraphics: TdxGraphics;
  AHdc: HDC;
  AOldFont: HFONT;
  AOldMode: Integer;
  AOldColor: TColorRef;
begin
  ABounds.Y := ABounds.Y + AOffset;
  AGraphics := AGdiPlusPainter.Graphics;
  AHdc := AGraphics.GetHDC;
  try
    AOldFont := SelectObject(AHdc, FHeaderFooterMarkFont.Handle);
    AOldMode := SetBkMode(AHdc, TRANSPARENT);
    AOldColor := SetTextColor(AHdc, TdxAlphaColors.ToColor(Owner.HeaderFooterMarkTextColor));
    ExtTextOut(AHdc, ABounds.Left, ABounds.Top, 0, @ABounds, PChar(ACaption), Length(ACaption), nil);
    SetTextColor(AHdc, AOldColor);
    SetBkMode(AHdc, AOldMode);
    SelectObject(AHdc, AOldFont);
  finally
    AGraphics.ReleaseHDC(AHdc);
  end;
end;

function TdxHeaderFooterDecoratorPainter.CreateHeaderFooterLinePen: TdxGPPen;
var
  ABrush: TdxGPBrush;
begin
  Result := TdxGPPen.Create;
  Result.Style := TdxGPPenStyle.gppsDash;
  Result.Width := HeaderFooterLineThinkness;
  ABrush := TdxGPBrush.Create;
  try
    ABrush.Color := Owner.HeaderFooterLineColor;
    Result.Brush := ABrush;
  finally
    FreeAndNil(ABrush);
  end;
end;

function TdxHeaderFooterDecoratorPainter.CreateHeaderFooterCaptionFont: TFont;
begin
  Result := TFont.Create;
  Result.Name := 'Calibri';
  Result.Size := 10;
end;

{ TdxPrintLayoutViewPainter }

constructor TdxPrintLayoutViewPainter.Create(AView: TdxPrintLayoutView);
begin
  inherited Create(AView);
  AddDecoratorPainter(TdxHeaderFooterDecoratorPainter.Create(AView, Self));
end;

function TdxPrintLayoutViewPainter.GetUnitConverter: TdxDocumentLayoutUnitConverter;
begin
  Result := View.DocumentModel.LayoutUnitConverter;
end;

function TdxPrintLayoutViewPainter.GetLeftHeaderFooterLineOffset: Integer;
begin
  Result := 2;
end;

function TdxPrintLayoutViewPainter.GetRightHeaderFooterLineOffset: Integer;
begin
  Result := 2;
end;

function TdxPrintLayoutViewPainter.GetHeaderFooterLineColor: TdxAlphaColor;
begin
  Result := TdxAlphaColors.FromColor(LookAndFeel.Painter.RichEditControlHeaderFooterLineColor);
end;

function TdxPrintLayoutViewPainter.GetHeaderFooterMarkBackColor: TdxAlphaColor;
begin
  Result := TdxAlphaColors.FromColor(LookAndFeel.Painter.RichEditControlHeaderFooterMarkBackColor);
end;

function TdxPrintLayoutViewPainter.GetHeaderFooterMarkTextColor: TdxAlphaColor;
begin
  Result := TdxAlphaColors.FromColor(LookAndFeel.Painter.RichEditControlHeaderFooterMarkTextColor);
end;

procedure TdxPrintLayoutViewPainter.DrawEmptyPage(AGraphics: TdxGraphics{ACache: TdxGraphicsCache}; APage: TdxPageViewInfo);
begin
  AGraphics.FillRectangle(APage.ClientBounds, GetActualPageBackColor);
  AGraphics.Rectangle(APage.ClientBounds, TdxAlphaColors.Black, TdxAlphaColors.Empty);
end;

procedure TdxPrintLayoutViewPainter.DrawEmptyComment(AGraphics: TdxGraphics; AComment: TdxCommentViewInfo);
begin
  NotImplemented;
end;

procedure TdxPrintLayoutViewPainter.DrawEmptyExtensionComment(AGraphics: TdxGraphics; AComment: TdxCommentViewInfo);
begin
  NotImplemented;
end;

procedure TdxPrintLayoutViewPainter.DrawRoundedRectangle(AGraphics: TdxGraphics; const ARect: TRect; ARadius: Integer; AFillColor: TdxAlphaColor; ALineColor: TdxAlphaColor);
begin
  NotImplemented;
end;

function TdxPrintLayoutViewPainter.GetActualPageBackColor: TdxAlphaColor;
var
  ADocumentProperties: TdxDocumentProperties;
  APageBackColor: TdxAlphaColor;
begin
  ADocumentProperties := DocumentModel.DocumentProperties;
  APageBackColor := ADocumentProperties.PageBackColor;
  if ADocumentProperties.DisplayBackgroundShape and not TdxAlphaColors.IsEmpty(APageBackColor) then
    Exit(APageBackColor);

  Result := View.ActualBackColor;
end;

function TdxPrintLayoutViewPainter.GetActualCommentBackColor: TdxAlphaColor;
var
  ADocumentProperties: TdxDocumentProperties;
  ACommentBackColor: TdxAlphaColor;
begin
  ADocumentProperties := DocumentModel.DocumentProperties;
  ACommentBackColor := ADocumentProperties.PageBackColor;
  if ADocumentProperties.DisplayBackgroundShape and not TdxAlphaColors.IsEmpty(ACommentBackColor) then
    Exit(ACommentBackColor);

  Result := View.ActualBackColor;
end;

{ TdxPrintLayoutViewSkinPainter }

constructor TdxPrintLayoutViewSkinPainter.Create(AView: TdxPrintLayoutView);
begin
  inherited Create(AView);
  LookAndFeel.Painter.GetPainterData(FSkinInfo);
end;

function TdxPrintLayoutViewSkinPainter.GetLeftHeaderFooterLineOffset: Integer;
begin
  Result := Max(2, SkinInfo.PrintingPageBorder.ContentOffset.Left);
end;

function TdxPrintLayoutViewSkinPainter.GetRightHeaderFooterLineOffset: Integer;
begin
  Result := Max(2, SkinInfo.PrintingPageBorder.ContentOffset.Right);
end;

procedure TdxPrintLayoutViewSkinPainter.DrawEmptyPages(AGraphics: TdxGraphics);
var
  AState: TdxGraphics.TClipState;
  ATransform: TdxGPMatrix;
  AOldPageScale: Single;
  AOldPageUnit: TdxGraphicsUnit;
begin
  ATransform := AGraphics.Transform;
  try
    AState := AGraphics.GetClipState;
    AOldPageScale := AGraphics.PageScale;
    AOldPageUnit := AGraphics.PageUnit;
    try
      AGraphics.PageUnit := TdxGraphicsUnit.guPixel;
      AGraphics.PageScale := 1.0;
      AGraphics.ResetTransform;
      inherited DrawEmptyPages(AGraphics);
    finally
      if AGraphics.PageScale <> AOldPageScale then
        AGraphics.PageScale := AOldPageScale;
      if AGraphics.PageUnit <> AOldPageUnit then
        AGraphics.PageUnit := AOldPageUnit;
      AGraphics.Transform := ATransform;
      AGraphics.RestoreClipState(AState);
    end;
  finally
    ATransform.Free;
  end;
end;

procedure TdxPrintLayoutViewSkinPainter.DrawEmptyPage(AGraphics: TdxGraphics; APage: TdxPageViewInfo);
var
  AOffsets, AContentBounds, APageBounds: TRect;
begin
  AOffsets := GetSkinEdges(SkinInfo.PrintingPageBorder);
  AContentBounds := CalculatePageViewInfoPixelBounds(APage);
  APageBounds := AContentBounds;
  if View.Control.UseSkinMargins then
  begin
    APageBounds.Inflate(AOffsets.Left, AOffsets.Top, AOffsets.Right, AOffsets.Bottom);
    APageBounds.Inflate(-2, -2);
  end;
  SkinInfo.PrintingPageBorder.DrawEx(AGraphics, APageBounds);
  AGraphics.FillRectangle(AContentBounds, GetActualPageBackColor);
end;

function TdxPrintLayoutViewSkinPainter.GetActualPageBackColor: TdxAlphaColor;
var
  ADocumentProperties: TdxDocumentProperties;
  APageBackColor: TdxAlphaColor;
begin
  ADocumentProperties := DocumentModel.DocumentProperties;
  APageBackColor := ADocumentProperties.PageBackColor;
  if ADocumentProperties.DisplayBackgroundShape and (APageBackColor <> TdxAlphaColors.Empty) then
    Exit(APageBackColor);

  Result := View.ActualBackColor;
end;

function TdxPrintLayoutViewSkinPainter.CalculatePageViewInfoPixelBounds(APageViewInfo: TdxPageViewInfo): TRect;
var
  AUnitConverter: TdxDocumentLayoutUnitConverter;
  AControl: IdxRichEditControl;
begin
  AControl := Control;
  AUnitConverter := AControl.DocumentModel.LayoutUnitConverter;
  Result := APageViewInfo.ClientBounds;
  Result.Offset(AControl.ViewBounds.X - View.HorizontalScrollController.GetPhysicalLeftInvisibleWidth, AControl.ViewBounds.Y);
  Result.X := AUnitConverter.LayoutUnitsToPixels(Result.X);
  Result.Width := AUnitConverter.LayoutUnitsToPixels(Result.Width);
  Result.Y := AUnitConverter.LayoutUnitsToPixels(Result.Y);
  Result.Height := AUnitConverter.LayoutUnitsToPixels(Result.Height);
end;

class function TdxPrintLayoutViewSkinPainter.GetSkinEdges(AElement: TdxSkinElement): TRect;
begin
  if AElement = nil then
    Result.Empty
  else
    Result := AElement.Image.Margins.Margin;
end;

{ TdxHeaderFooterSelectionLayout }

function TdxHeaderFooterSelectionLayout.CreateDocumentLayoutPosition(ALogPosition: TdxDocumentLogPosition): TdxDocumentLayoutPosition;
begin
  Result := TdxHeaderFooterDocumentLayoutPosition.Create(DocumentLayout,
    TdxDocumentModel(DocumentLayout.DocumentModel).Selection.PieceTable, ALogPosition, PreferredPageIndex);
end;

{ TdxPrintLayoutView }

constructor TdxPrintLayoutView.Create(const AControl: IdxRichEditControl);
begin
  inherited Create(AControl);
  FTextBoxPageIndex := -1;
end;

function TdxPrintLayoutView.GetType: TdxRichEditViewType;
begin
  Result := TdxRichEditViewType.PrintLayout;
end;

function TdxPrintLayoutView.GetShowHorizontalRulerByDefault: Boolean;
begin
  Result := True;
end;

function TdxPrintLayoutView.GetShowVerticalRulerByDefault: Boolean;
begin
  Result := True;
end;

function TdxPrintLayoutView.GetMatchHorizontalTableIndentsToTextEdge: Boolean;
begin
  Result := Control.InnerControl.Options.Layout.PrintLayoutView.MatchHorizontalTableIndentsToTextEdge;
end;

function TdxPrintLayoutView.GetFixedLeftTextBorderOffset: Integer;
var
  AHorizontalRuler, AVerticalRuler: IdxRulerControl;
  AOffset, ALogicalOffset: Integer;
begin
  AHorizontalRuler := Control.InnerControl.HorizontalRuler;
  AVerticalRuler := Control.InnerControl.VerticalRuler;
  AOffset := 0;
  if AHorizontalRuler <> nil then
    AOffset := Math.Max(AOffset, AHorizontalRuler.GetRulerSizeInPixels);
  if AVerticalRuler <> nil then
    AOffset := Math.Max(AOffset, AVerticalRuler.GetRulerSizeInPixels);
  if (AVerticalRuler <> nil) and (not AVerticalRuler.IsVisible) then
    AOffset := AOffset * 2;
  ALogicalOffset := DocumentLayout.UnitConverter.PixelsToLayoutUnits(AOffset, DocumentModel.DpiY);
  Result := Round(ALogicalOffset / ScaleFactor);
end;

function TdxPrintLayoutView.GetDefaultAllowDisplayLineNumbers: Boolean;
begin
  Result := True;
end;

function TdxPrintLayoutView.GetPageHorizontalAlignment: TAlignment;
var
  AGenerator: TdxPrintLayoutViewPageViewInfoGenerator;
begin
  AGenerator := TdxPrintLayoutViewPageViewInfoGenerator(PageViewInfoGenerator);
  Result := AGenerator.PageHorizontalAlignment;
end;

procedure TdxPrintLayoutView.SetPageHorizontalAlignment(const AValue: TAlignment);
var
  AGenerator: TdxPrintLayoutViewPageViewInfoGenerator;
begin
  if PageHorizontalAlignment = AValue then
    Exit;
  AGenerator := TdxPrintLayoutViewPageViewInfoGenerator(PageViewInfoGenerator);
  AGenerator.PageHorizontalAlignment := AValue;
  PerformZoomFactorChanged;
end;

function TdxPrintLayoutView.GetMaxHorizontalPageCount: Integer;
var
  AGenerator: TdxPrintLayoutViewPageViewInfoGenerator;
begin
  AGenerator := TdxPrintLayoutViewPageViewInfoGenerator(PageViewInfoGenerator);
  Result := AGenerator.MaxHorizontalPageCount;
end;

procedure TdxPrintLayoutView.SetMaxHorizontalPageCount(const AValue: Integer);
var
  AGenerator: TdxPrintLayoutViewPageViewInfoGenerator;
begin
  if MaxHorizontalPageCount = AValue then
    Exit;
  AGenerator := TdxPrintLayoutViewPageViewInfoGenerator(PageViewInfoGenerator);
  AGenerator.MaxHorizontalPageCount := AValue;
  PerformZoomFactorChanged;
end;

function TdxPrintLayoutView.CreateDocumentFormattingController: TdxDocumentFormattingController;
begin
  Result := TdxPrintLayoutViewDocumentFormattingController.Create(DocumentLayout, DocumentModel.MainPieceTable);
end;

function TdxPrintLayoutView.CreatePageViewInfoGenerator: TdxPageViewInfoGenerator;
begin
  Result := TdxPrintLayoutViewPageViewInfoGenerator.Create(Self, ViewInfo);
end;

function TdxPrintLayoutView.CreateDocumentLayoutExporter(APainter: TdxPainter; AAdapter: TdxGraphicsDocumentLayoutExporterAdapter;
  APageViewInfo: TdxPageViewInfo; const ABounds: TRect): TdxDocumentLayoutExporter;
begin
  if AllowDisplayLineNumbers then
    Result := TdxScreenOptimizedGraphicsDocumentLayoutExporter.Create(DocumentModel, APainter, AAdapter, ABounds, TextColors, ScaleFactor)
  else
    Result := TdxScreenOptimizedGraphicsDocumentLayoutExporterNoLineNumbers.Create(DocumentModel, APainter, AAdapter, ABounds, TextColors, ScaleFactor);
end;

function TdxPrintLayoutView.CreatePainter: TdxRichEditViewPainter;
begin
  if LookAndFeel.ActiveStyle = lfsSkin then
    Result := TdxPrintLayoutViewSkinPainter.Create(Self)
  else
    Result := TdxPrintLayoutViewPainter.Create(Self);
end;

function TdxPrintLayoutView.CreateBackgroundPainter: TdxRichEditViewBackgroundPainter;
begin
  if LookAndFeel.ActiveStyle = lfsSkin then
    Result := TdxPrintLayoutViewSkinBackgroundPainter.Create(Self)
  else
    Result := TdxPrintLayoutViewBackgroundPainter.Create(Self);
end;

procedure TdxPrintLayoutView.OnActivePieceTableChanged;
var
  APreferredPageIndex: Integer;
begin
  if DocumentModel.ActivePieceTable.IsHeaderFooter then
  begin
    APreferredPageIndex := CurrentPageIndex;
    if APreferredPageIndex < 0 then
      if FTextBoxPageIndex > 0 then
        APreferredPageIndex := FTextBoxPageIndex
      else
        APreferredPageIndex := FormattingController.PageController.Pages.Count - 1;
    Assert(APreferredPageIndex >= 0);
    SelectionLayout := TdxHeaderFooterSelectionLayout.Create(Self, APreferredPageIndex);
    CaretPosition := TdxHeaderFooterCaretPosition.Create(Self, APreferredPageIndex);
  end
  else
    inherited OnActivePieceTableChanged;
  if DocumentModel.ActivePieceTable.IsTextBox then
    FTextBoxPageIndex := CurrentPageIndex
  else
    FTextBoxPageIndex := -1;
end;

procedure TdxPrintLayoutView.Visit(const AVisitor: IdxRichEditViewVisitor);
begin
  AVisitor.Visit(Self);
end;

function TdxPrintLayoutView.ExpandClipBoundsToPaddings(const AClipBounds: TRect): TRect;
begin
  Result := AClipBounds;
end;

end.
